#!/usr/bin/env python2.7

"""A script used to run provers against problem files,
collect results into a sqlite database and perform some
operations on those results.
"""


import json
import os
import re
import sqlite3
import sys
import time
import argparse
import resource
import logging
import ConfigParser
import atexit

from subprocess import Popen, PIPE

MAIN_SECTION = 'benchmark' # section for configuring the script
CONFIG_FILE = "./benchs.ini"
DB_FILE = "./benchs.db"
TIMEOUT = 120
LOG_LEVEL = 'info'
MEMORY = 1024 * 1024

log = logging.getLogger("benchmark")
log.addHandler(logging.StreamHandler())
log.setLevel(logging.DEBUG)

# available commands
commands = set(())
def command(fun):
    """Makes the command available to the user"""
    commands.add(fun.__name__)
    return fun

levels = {
    'debug': logging.DEBUG,
    'info': logging.INFO,
    'error': logging.ERROR,
}



class Run(object):
    """Contains everything needed for running benchmarks."""

    def __init__(self, config):
        """Initializes with the given db file name. It opens a sqlite
        connection to the given file.
        [profile] is the benchmark profile to use.
        """
        self.config = config
        self.db_name = config.get(MAIN_SECTION, 'db_file')
        log.debug('use DB %s', self.db_name)
        self.profile = config.get(MAIN_SECTION, 'profile')
        self.provers = config.get(MAIN_SECTION, 'provers') \
            if config.has_option(MAIN_SECTION, 'provers')
            else config.get(self.profile, 'provers', vars=)
        self.timeout = config.getint(self.profile, 'timeout') \
            if config.has_option(self.profile, 'timeout') \
            else config.getint(MAIN_SECTION, 'timeout')
        # connect to the Database
        self.conn = sqlite3.connect(self.db_name)
        try:
            # create table
            create_query =  """create table results (id integer primary key asc, filename varchar2(200),
                prover varchar2(50), result varchar2(50), time double, output text,
                constraint foobar unique (filename, prover) on conflict replace
                );"""
            self.conn.execute(create_query)
            atexit.register(lambda : db.close())
        except sqlite3.OperationalError as e:
            pass

    def save(self, filename, prover, result, time, output):
        """Save this individual result in the table. If an entry with same
        (filename,prover) already exists, it will be deleted. """ 
        try:
            output = output.decode('utf8')
            self.conn.execute("""insert into results (filename, prover, result, time, output)
                values (?, ?, ?, ?, ?);""",
                [filename.strip(), prover.strip(), result.strip(), time, output])
            self.conn.commit()
        except sqlite3.Error as e:
            log.error("sqlite error while saving: %s", e)

    def solve_with(self, filename, prover, verbose=False):
        """Run the prover on the given file. Returns (result, time, output)."""
        if prover not in self.config.sections():
            log.error('prover not known: %s', prover)
            sys.exit(1) # unknown prover

        cmd = self.config.get(prover, 'cmd')
        unsat = self.config.get(prover, 'unsat')
        sat = self.config.get(prover, 'sat')

        # limit memory (address space)
        memory = self.config.getint(MAIN_SECTION, 'memory')
        log.debug('memory limit is %d', memory)
        resource.setrlimit(resource.RLIMIT_AS, (memory * 1024, memory * 1024))

        log.debug('time limit is %d', self.timeout)

        print "solve %-30s with prover %-20s... " % (filename, prover),
        sys.stdout.flush()
      
        # run prover and wait for result
        cmd = cmd.format(time=self.timeout, file=filename)
        start = time.time()
        p = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
        out, _ = p.communicate()
        stop = time.time()

        result = "failure"
        if re.search(unsat, out, re.IGNORECASE):
            result = "unsat"
        elif re.search(sat, out, re.IGNORECASE):
            result = "sat"

        print "%-10s in %-6.3f" % (result, stop - start)
        log.debug("  (prover cmd line was: %s)", cmd)
        # now return result
        return (result, stop - start, out)

    def raw_solve(self, provers, filenames):
        """Run the provers on the files"""
        # TODO use the self.cores argument to run those in parallel
        rows = []
        for filename in filenames:
            results = []
            print '-' * 70
            for prover_name in provers:
                result, t, out = self.solve_with(filename, prover_name)
                self.save(filename, prover_name, result, t, out)
                results.append( [prover_name, result] )
            # check all results are similar
            bools = set(r for _, r in results)
            if 0 in bools and any(bools):
                # conflict
                print "for problem %s, provers do not agree" % filename
                print ', '.join("%s: %d" % (name, result) for name, result in results)
            # append results to [rows]
            rows.extend(results)
        return rows

    @command
    def solve_using(self, prover, *filenames):
        """Run the prover on the given files"""
        return self.raw_solve([prover], filenames)

    @command
    def solve(self, *filenames):
        """Run all provers against the given files"""
        provers = self.config.get(self.profile, 'provers')
        provers = provers.strip().split(',')
        return self.raw_solve(provers, filenames)

    def print_rows(self, rows):
        """Print the rows on stdout"""
        rows = list(rows)
        # aggregate rows by filename
        files = set(f for f, _, _, _ in rows)
        aggregated = dict((f, []) for f in files)
        for filename, prover, result, time in rows:
            aggregated[filename].append( [prover, result, time] )
        # print results for each filename
        for filename, items in aggregated.iteritems():
            print '-' * 70
            # print results for this file
            for prover, result, time in items:
                print "%-40s with %-20s: %-10s in %-6.3f" % \
                    (filename, prover, result, time)
            # warning for files for which there are different results
            if len(set(r for _, r, _ in items)) > 1:
                print ">>> provers disagree on file", filename

    @command
    def dump(self):
        """Print all the results currently in the database"""
        try:
            query = """select filename, prover, result, time from results
                order by filename, prover asc;"""
            rows = self.conn.execute(query)
            # print rows
            self.print_rows(rows)
        except sqlite3.Error as e:
            print e

    @command
    def results(self, *filenames):
        "print results for given filenames"
        query = """select filename, prover, result, time from results
            where filename in (%s) order by filename, prover asc;
            """ % ','.join('"'+f.strip()+'"' for f in filenames)
        rows = self.conn.execute(query)
        self.print_rows(rows)

    @command
    def clear(self):
        "clear database of results"
        self.conn.execute("""delete from results;""")

    @command
    def clear_files(self, *filenames):
        "remove results relative to the given files"
        self.conn.execute("""delete from results where filename in (%s); """ % \
            ','.join('"'+f+'"' for f in filenames))

    @command
    def clear_provers(self, *provers):
        "remove results relative to the given provers"
        self.conn.executemany("""delete from results where prover like ?; """,
            [(prover,) for prover in provers])

    @command
    def normalize(self):
        "remove outliers, rounds times to TIMEOUT"
        query = """update results set time={time} where time > {time};""".format(
            time = TIMEOUT)
        self.conn.execute(query)

    @command
    def disagree(self, display=True):
        "finds the files on which provers give a different result"
        query = """select filename, prover, result, time from results r where
            (select count(distinct result) from results r2 where r2.filename=r.filename) > 1 ;"""
        rows = list(self.conn.execute(query))
        if display:
            self.print_rows(rows)
        return rows

    @command
    def json_dump(self):
        "dumps the table in json format"
        query = """select filename, prover, result, time from results;"""
        rows = self.conn.execute(query)
        # print as a big json array
        obj = [ { 'filename': filename
                , 'prover': prover
                , 'result': result
                , 'time': time
                } for filename, prover, result, time in rows]
        json.dump(obj, sys.stdout, indent=2)

    @command
    def json_load(self, filename):
        "load results from the json file ('-' means stdin)"
        f = open(filename) if filename != '-' else sys.stdin
        try:
            l = json.load(f)
        finally:
            f.close()
        assert isinstance(l, list)
        for row in l:
            assert isinstance(row, dict)
            filename = row['filename']
            prover = row['prover']
            result = row['result']
            time = float(row['time'])
            self.save(filename, prover, result, time)

    @command
    def stats(self):
        "print some statics about the database of results"
        s = { 'filenames': set([]), 'provers': set([]) }
        rows = self.conn.execute("""select filename, prover, result, time from results;""")
        for filename, prover, result, t in rows:
            # basic stats
            s['filenames'].add(filename)
            s['provers'].add(prover)
            if prover not in s:
                s[prover] = { 'solved': 0, 'failed': 0, 'time': 0 }
            # stats per prover
            if result in ("sat", "unsat"):
                s[prover]['solved'] += 1
                s[prover]['time'] += t
            else:
                s[prover]['failed'] += 1
        # print stats
        print "database contains %d files and %d provers" % (len(s['filenames']), len(s['provers']))
        for prover in s['provers']:
            avg = s[prover]['time'] / s[prover]['solved'] if s[prover]['solved'] > 0 else 0
            total = s[prover]['solved'] + s[prover]['failed']
            ratio = float(s[prover]['solved']) / (s[prover]['solved'] + s[prover]['failed'])
            print "prover %-20s: solved %5d / %-5d | failed %-5d | ratio %.2f" \
                "| average solving time %-6.3f | total time %-6.3f" %\
                (prover, s[prover]['solved'], total, s[prover]['failed'], ratio, avg, s[prover]['time'])

    @command
    def list_provers(self):
        "prints the list of known provers"
        for p in provers.keys():
            print '  ', p

def list_commands():
    "list available commands"
    l = []
    for cmd in sorted(commands):
        fun = getattr(Run, cmd)
        l.append("  %-15s %s\n" % (cmd, getattr(fun, '__doc__', '')))
    return l

def arg_parser():
    """Argument parser"""
    parser = argparse.ArgumentParser(
        description="A script to run provers against files, "\
        "and store results in a database",
        epilog="".join(list_commands()),
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("command", help="the command to run")
    parser.add_argument("files", nargs="*", help="files to run the provers on")
    parser.add_argument("--level", "-l", dest='level', type=str, default='info', help='logging level')
    parser.add_argument("--config", "-c", dest="config", default=CONFIG_FILE, help="config file")
    parser.add_argument("--profile", dest="profile", default=None, help="profile to use")
    parser.add_argument("--provers", dest="provers", default=None, help="provers")
    parser.add_argument("-j", dest="cores", type=int, default=1, help="number of cores used")
    parser.add_argument("--timeout", "-t", dest="timeout", type=int, default=None, help="timeout (in seconds)")
    parser.add_argument("-m", dest="memory", type=int, default=MEMORY, help="memory limit (in kbytes)")
    parser.add_argument("--db", dest="db", default=DB_FILE, help="db to use")
    return parser

def parse_args(args):
    """Parse the CLI arguments."""
    parser = arg_parser()
    args = parser.parse_args(args=args)
    return args

if __name__ == "__main__":
    if len(sys.argv) == 1:
        parser = arg_parser()
        parser.print_help()
        sys.exit(0)
    # parse arguments
    args = parse_args(sys.argv[1:])
    log.setLevel(levels.get(args.level, logging.INFO))
    # parse config (my_config contains default values)
    log.debug('parse config file %s', args.config)
    my_config = {
        'memory': str(args.memory),
        'db': args.db,
        'cores': args.cores
    }
    if args.profile:
        my_config['profile'] = args.profile
    if args.timeout:
        my_config['timeout'] = str(args.timeout)
    if args.provers:
        my_config['provers'] = args.provers
    config = ConfigParser.ConfigParser(my_config)
    config.read(args.config)
    # do actions
    run = Run(config)
    fun = getattr(run, args.command)
    fun(* args.files)

# vim:foldnestmax=2:shiftwidth=4
