#!/usr/bin/env python3
import subprocess as sp
import sys
import re
from multiprocessing import cpu_count

STATUS_REGEX = re.compile(r'\% SZS status (\w+)')

def print_locked(msg, stdout_lock):
  assert stdout_lock is not None

  stdout_lock.acquire()
  try:
    print('{0}'.format(msg), flush=True)
  except:
    import traceback
    print("% Print lock error:{0}".format(traceback.format_exc()))
  finally:
    stdout_lock.release()

THM_STATUSES = ["Theorem", "Unsatisfiable", "ContradictoryAxioms"]

class Configuration:
  class RunResult:
    THEOREM = 1
    GAVE_UP = 2
    TIMEOUT = 3
    CANCELLED = 4
    UNKNOWN_ERROR = 5


  def __init__(self, conf_path, preferred_time):
    import os
    cwd = os.path.dirname(os.path.realpath(__file__))
    conf_path = cwd + "/" + conf_path
    self._conf_path = conf_path
    self._preferred_time = preferred_time
    
    if not os.path.isfile(conf_path):
      raise ValueError("{0} cannot be found".format(conf_path))
    if not (os.access(conf_path, os.X_OK)):
      raise ValueError("{0} is not executable".format(conf_path))


  def conf_path(self):
    return self._conf_path

  def preferred_timeout(self):
    return self._preferred_time
  
  def compute_timeout(self, ratio):
    lower_cutoff, upper_cutoff = 0.25,2.5
    from math import ceil

    if ratio < lower_cutoff:
      self._actual_timeout =  int(ceil(lower_cutoff * self._preferred_time))
    elif ratio > upper_cutoff:
      self._actual_timeout =  int(ceil(upper_cutoff * self._preferred_time))
    else:
      self._actual_timeout =  int(ceil(ratio * self._preferred_time))
    
    return self._actual_timeout

  def actual_timeout(self):
    if hasattr(self, '_actual_timeout'):
      return self._actual_timeout
    else:
      raise AttributeError(" actual timeout must be computed first") 

  def _do_run(self, prob_path, total_timeout, tmp_dir, stdout_lock, extra_args):
    try:
      from math import ceil
      import os
      cwd = os.path.dirname(os.path.realpath(__file__))
      conf_path = os.path.join(cwd, self.conf_path())

      timeout = self.actual_timeout()

      print_locked("% {0} running for {1}s".format(conf_path, timeout), stdout_lock)
      proc_res = sp.run([conf_path, prob_path, str(timeout)] + list(extra_args), stdout=sp.PIPE, stderr=sp.PIPE, timeout=timeout)
      res_out = proc_res.stdout.decode(encoding='ascii', errors='ignore')
      matches = STATUS_REGEX.findall(res_out)
      if any(matches):
        match = matches[0]
        if match.strip() in THM_STATUSES:
          # print_locked("{0}:{1}".format(conf_path, res_out))          
          return (Configuration.RunResult.THEOREM, res_out, self.conf_path())
        elif match.strip() == "ResourceOut":
          #print_locked("% {0}:{1}".format(conf_path, "TO"), stdout_lock)          
          return (Configuration.RunResult.TIMEOUT, None, self.conf_path())
        else:
          #print_locked("% {0}:{1}".format(conf_path, match.strip()), stdout_lock)          
          return (Configuration.RunResult.GAVE_UP, None, self.conf_path())
      
      print_locked("% {0} could not parse the output".format(conf_path), stdout_lock)
      err_msg = "\n".join(map(lambda x: "% " + x, res_out.split('\n')))
      print_locked(err_msg, stdout_lock)
      return (Configuration.RunResult.UNKNOWN_ERROR, None, self.conf_path())
      
    
    except Exception as e:
      # timeouts and things...
      #err_msg = "\n".join(map(lambda x: "% " + x, str(e).split('\n')))
      #print_locked("% Error: {0}".format(err_msg.replace("\n","\n% ")), stdout_lock)
      # import traceback
      # print("% Fatal error: {0}".format(traceback.format_exc().replace("\n", "\n%")))
      return (Configuration.RunResult.UNKNOWN_ERROR, None, self.conf_path())

  def run(self, prob_path, total_timeout, tmp_dir, stdout_lock, extra_args):
    return self._do_run(prob_path, total_timeout, tmp_dir, stdout_lock, extra_args)
      

all_confs = [
  Configuration("fo/fo6_bce.sh",30),
  Configuration("fo/fo3_bce.sh",30),
  Configuration("fo/fo1_av.sh",30),
  Configuration("fo/fo7.sh",25),
  Configuration("fo/fo13.sh",20),
  Configuration("fo/fo5.sh",20),
  Configuration("fo/fo4.sh",20),
  Configuration("fo/fo1_lcnf.sh",20),
  Configuration("fo/fo17_bce.sh",20),
  Configuration("fo/fo8.sh",20),
  Configuration("fo/fo2.sh",15),
  Configuration("fo/fo14.sh",20),
  Configuration("fo/fo20.sh",20),
  Configuration("fo/fo19.sh",20),
  Configuration("fo/fo15.sh",20),
  Configuration("fo/fo9.sh",15),
  Configuration("fo/fo6_hlbe.sh",15),
  Configuration("fo/fo10.sh",15),
  Configuration("fo/fo1_bce.sh",15),
  Configuration("fo/fo12.sh",15),
  Configuration("fo/fo17_hlbe.sh",15),
  Configuration("fo/fo6_lcnf.sh",15)
]


class Runner(object):
  def __init__(self, prob, timeout, tmp_dir, extra_args):
    self._prob = prob
    self._timeout = timeout
    self._tmp_dir = tmp_dir
    self._extra_args = extra_args
  
  def __call__(self, arg):
    conf, stdout_lock, flags = arg
    assert stdout_lock is not None
    try:
      if flags['done']:
        return (Configuration.RunResult.CANCELLED, None, conf.conf_path())
      else:
        return conf.run(self._prob, self._timeout, self._tmp_dir, stdout_lock, self._extra_args)
    except:
      # import traceback
      # print("% Fatal error:{0}".format(traceback.format_exc()))
      return (Configuration.RunResult.UNKNOWN_ERROR, None, conf.conf_path())



def run_parallel(confs, prob, timeout, tmp_dir, use_all_cpus, extra_args):
  try:
    import multiprocessing as m
    import os
    m.set_start_method("spawn")

    manager = m.Manager()
    stdout_lock = manager.Lock()
    flags = manager.dict({'done': False})
    runner = Runner(prob, timeout, tmp_dir, extra_args)

    n_cpus = len(os.sched_getaffinity(0)) - (0 if use_all_cpus else 1)
    
    pool = m.Pool(n_cpus)
    
    total_wc_time = sum([c.preferred_timeout() for c in confs])
    ratio = float(timeout) / float(total_wc_time)
    total_computed_time = sum([c.compute_timeout(ratio) for c in confs])
    print("% Total configuration time : {0}".format(total_wc_time), flush=True)
    print("% Estimated wc time : {0}".format(total_computed_time), flush=True)
    print("% Estimated cpu time ({1} cpus) : {0}".format(total_computed_time/n_cpus, n_cpus), flush=True)

    results = pool.imap_unordered(runner, [(c, stdout_lock, flags) for c in confs])

    for (status,proof,conf_id) in results:
      
      # print_locked("% {0} says {1}".format(conf_id, status), stdout_lock)
      if status == Configuration.RunResult.THEOREM:
        assert proof is not None

        print_locked("% Solved by {0}.\n{1}\n".format(conf_id,proof), stdout_lock)
        flags['done'] = True

        break
    
    print_locked("% Terminating...", stdout_lock)
    pool.terminate()
    pool.join()
  except:
    print("Unknown error in runner body.")
    # import traceback
    # print("% Fatal error:{0}".format(traceback.format_exc()))
        

def main():
  import os
  try:
    prob_path = sys.argv[1]
    timeout = int(sys.argv[2])
    tmp_dir   = None
    use_all_cpus = True if sys.argv[3].lower() == "true" else False
    cpu_cnt = len(os.sched_getaffinity(0))
    timeout = timeout*(cpu_cnt-1) if not use_all_cpus else timeout*(cpu_cnt)

    run_parallel(all_confs, prob_path, timeout, tmp_dir, use_all_cpus, sys.argv[5:])
  except IndexError:
    print("Usage: python3 {0} <prob_path> <timeout> <use_all_cpus:true/false> [extra arguments for the runscripts]".format(sys.argv[0]))
  except Exception as e:
    print("Uncaught exception {0}".format(e))
  finally:
    print("% Runner terminated.")
 
if __name__ == "__main__":
  main()


