#!/usr/bin/env python2

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

from subprocess import Popen, PIPE

DB_FILE = "provers_output.db"
TIMEOUT = 30
MEMORY = 512000

# available commands
commands = set(())
def command(fun):
    "decorator for commands available to the user"
    commands.add(fun.__name__)
    return fun


# known provers
provers = {
    'spass': "SPASS -TPTP -TimeLimit={time} -Memory=512000000 {file}",
    'zipperposition': "./prover3.sh {file} -timeout {time}"
}
provers_unsat = {
    'spass': "proof found",
    'zipperposition': "SZS Status Theorem"
}
provers_sat = {
    'spass': "completion found",
    'zipperposition': "SZS Status CounterSatisfiable"
}


class Run(object):
    """Contains everything needed for running benchmarks."""

    def __init__(self, db_name):
        """Initializes with the given db file name. It opens a
        sqlite connection to the given file.
        """
        self.db_name = db_name
        self.conn = sqlite3.connect(db_name, isolation_level = None)
        try:
            # create table
            create_query =  """create table results (id integer primary key asc, filename varchar2(200),
                prover varchar2(50), result varchar2(50), time double,
                constraint foobar unique (filename, prover) on conflict replace
                );"""
            self.conn.execute(create_query)
        except sqlite3.OperationalError as e:
            pass

    def save(self, filename, prover, result, time):
        """save this result in the table. If an entry with
        same (filename,prover) already exists, it will be
        deleted.
        """ 
        try:
            self.conn.execute("""insert into results (filename, prover, result, time)
                values (?, ?, ?, ?);""", [filename.strip(), prover.strip(), result.strip(), time])
        except sqlite3.Error as e:
            print "sqlite error while saving", e

    def solve_with(self, filename, prover, verbose=True):
        "Solve the file with the prover."
        if prover not in provers:
            print "unknown prover", prover
            sys.exit(1)
        assert prover in provers_unsat and prover in provers_sat
        pstring = provers[prover]
        punsat = provers_unsat[prover]
        psat = provers_sat[prover]

        print "solve %-30s with prover %-20s... " % (filename, prover),
        sys.stdout.flush()
      
        cmd = pstring.format(time=TIMEOUT, file=filename)
        start = time.time()
        p = Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
        out, _ = p.communicate()
        stop = time.time()

        result = "failure"
        if re.search(punsat, out, re.IGNORECASE):
            result = "unsat"
        elif re.search(psat, out, re.IGNORECASE):
            result = "sat"

        print "%-10s in %-6.3f" % (result, stop - start)
        return (result, stop - start)

    @command
    def solve_using(self, prover, *filenames):
        "solve the files using the given prover"
        rows = []
        for filename in filenames:
            result, t = self.solve_with(filename, prover)
            self.save(filename, prover, result, t)
            rows.append( (filename, prover, result, t) )
        return rows

    @command
    def solve(self, *filenames):
        "run all provers against the given files"
        for filename in filenames:
            results = []
            print "-" * 70
            for prover_name in provers.keys():
                result, t = self.solve_with(filename, prover_name)
                self.save(filename, prover_name, result, t)
                results.append( [prover_name, result] )
            # check all results are similar
            bools = set(r for _, r in results)
            if 0 in bools and any(bools):
                # conflict
                print "for problem %s, provers do not agree" % filename
                print ', '.join("%s: %d" % (name, result) for name, result in results)

    def print_rows(self, rows):
        "print the rows on stdout"
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
        "print all the results currently in the database"
        try:
            query = """select filename, prover, result, time from results
                order by filename, prover asc;"""
            rows = self.conn.execute(query)
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
    def clear_provers(self, provers):
        "remove results relative to the given provers"
        self.conn.executemany("""delete from results where prover like "?"; """,
            [(prover, ) for prover in provers])

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
            print "prover %-20s: solved %-5d failed %-5d average solving time %-6.3f total time %-6.3f" %\
                (prover, s[prover]['solved'], s[prover]['failed'], avg, s[prover]['time'])

    @command
    def list_provers(self):
        "prints the list of known provers"
        for p in provers.keys():
            print '  ', p

    @command
    def help(self):
        "print this help"
        print "usage: run.py cmd [arg...]"
        print "available commands:"
        for cmd in sorted(commands):
            fun = getattr(self, cmd)
            print "  %-15s %s" % (cmd, getattr(fun, '__doc__', ''))


if __name__ == "__main__":
    run = Run(db_name = DB_FILE)
    if len(sys.argv) == 1 or sys.argv[1] not in commands:
        run.help()
        sys.exit(1)
    cmd = sys.argv[1]
    fun = getattr(run, cmd)
    fun(* sys.argv[2:])

# vim:foldnestmax=2:shiftwidth=4
