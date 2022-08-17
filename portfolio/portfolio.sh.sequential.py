#!/usr/bin/env python3
import subprocess as sp
import sys
import re
from multiprocessing import cpu_count

STATUS_REGEX = re.compile(r'\% SZS status (\w+)')

def print_locked(msg):
  print('{0}'.format(msg), flush=True)

THM_STATUSES = ["Theorem", "Unsatisfiable", "ContradictoryAxioms"]

class Configuration:
  class RunResult:
    THEOREM = 1
    GAVE_UP = 2
    TIMEOUT = 3
    CANCELLED = 4
    UNKNOWN_ERROR = 5


  def __init__(self, conf_path, preferred_time):
    self._conf_path = conf_path
    self._preferred_time = preferred_time
    
    import os
    if not os.path.isfile(conf_path):
      raise ValueError("{0} cannot be found".format(conf_path))
    if not (os.access(conf_path, os.X_OK)):
      raise ValueError("{0} is not executable".format(conf_path))


  def conf_path(self):
    return self._conf_path

  def preferred_timeout(self):
    return self._preferred_time
  
  def compute_timeout(self, ratio):
    lower_cutoff, upper_cutoff = 0.4,2
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

  def _do_run(self, prob_path, total_timeout, tmp_dir, extra_args):
    try:
      from math import ceil
      import os
      cwd = os.path.dirname(os.path.realpath(__file__))
      conf_path = os.path.join(cwd, self.conf_path())

      timeout = self.actual_timeout()

      print_locked("% {0} running for {1}s".format(conf_path, timeout))
      proc_res = sp.run([conf_path, prob_path, str(timeout), tmp_dir] + list(extra_args), stdout=sp.PIPE, stderr=sp.PIPE, timeout=timeout)
      res_out = proc_res.stdout.decode(encoding='ascii', errors='ignore')
      matches = STATUS_REGEX.findall(res_out)
      if any(matches):
        match = matches[0]
        if match.strip() in THM_STATUSES:
          return (Configuration.RunResult.THEOREM, res_out, self.conf_path())
        elif match.strip() == "ResourceOut":
          return (Configuration.RunResult.TIMEOUT, None, self.conf_path())
        else:
          return (Configuration.RunResult.GAVE_UP, None, self.conf_path())
      
      print_locked("% {0} could not parse the output".format(conf_path))
      err_msg = "\n".join(map(lambda x: "% " + x, res_out.split('\n')))
      print_locked(err_msg)
      return (Configuration.RunResult.UNKNOWN_ERROR, None, self.conf_path())
      
    
    except Exception as e:
      # timeouts and things...
      #err_msg = "\n".join(map(lambda x: "% " + x, str(e).split('\n')))
      #print_locked("% Error: {0}".format(err_msg.replace("\n","\n% ")), stdout_lock)
      # import traceback
      # print("% Fatal error: {0}".format(traceback.format_exc().replace("\n", "\n%")))
      return (Configuration.RunResult.UNKNOWN_ERROR, None, self.conf_path())

  def run(self, prob_path, total_timeout, tmp_dir, extra_args):
    return self._do_run(prob_path, total_timeout, tmp_dir, extra_args)
      

all_confs = [
  Configuration('sh/sh5_sh1.sh', 8),
  Configuration('sh/sh8_shallow_sine.sh', 8),
  Configuration('sh/sh10_new_c.s3.sh', 6),
  Configuration('sh/sh10_c_ic.sh', 6),
  Configuration('sh/sh8_b.comb.sh', 5),
  Configuration('sh/sh5_add_var_l_av.sh', 5),
  Configuration('sh/sh10_e_lift.sh', 5),
  Configuration('sh/sh5_shallow_sine.sh', 2),
  Configuration('sh/sh5_e_short1.sh', 3),
  Configuration('sh/sh5_32.sh', 3),
  Configuration('sh/sh5_sh4.sh', 3),
  Configuration('sh/sh5_lifting2.sh', 5),
  Configuration('sh/sh5_noforms.sh', 2),
  Configuration('sh/sh8_old_zip1.sh', 5),
  Configuration('sh/sh5_sh.eqenc.sh', 1)
]


class Runner(object):
  def __init__(self, prob, timeout, tmp_dir, extra_args):
    self._prob = prob
    self._timeout = timeout
    self._tmp_dir = tmp_dir
    self._extra_args = extra_args
  
  def __call__(self, conf):
    try:
      return conf.run(self._prob, self._timeout, self._tmp_dir, self._extra_args)
    except:
      import traceback
      print("% Fatal error:{0}".format(traceback.format_exc()))
      return (Configuration.RunResult.UNKNOWN_ERROR, None, conf.conf_path())



def run_sequential(confs, prob, timeout, tmp_dir, extra_args):
  try:
    runner = Runner(prob, timeout, tmp_dir, extra_args)
    
    total_wc_time = sum([c.preferred_timeout() for c in confs])
    ratio = float(timeout) / float(total_wc_time)
    total_computed_time = sum([c.compute_timeout(ratio) for c in confs])
    print("% Total configuration time : {0}".format(total_wc_time), flush=True)
    print("% Estimated wc time : {0}".format(total_computed_time), flush=True)
    print("% Estimated cpu time ({1} cpus) : {0}".format(total_computed_time, 1), flush=True)

    for c in confs:
      (status,proof,conf_id) = runner(c)
      if status == Configuration.RunResult.THEOREM:
        assert proof is not None

        print_locked("% Solved by {0}.\n{1}\n".format(conf_id,proof))
        break

    print_locked("% Terminating...")
  except:
    print("Unknown error in runner body.")
    # import traceback
    # print("% Fatal error:{0}".format(traceback.format_exc()))
        

def main():
  import os
  try:
    prob_path = sys.argv[1]
    timeout = int(sys.argv[2])
    tmp_dir   = sys.argv[3]

    run_sequential(all_confs, prob_path, timeout, tmp_dir, sys.argv[4:])
  except IndexError:
    print("Usage: python3 {0} <prob_path> <timeout> <tmp_dir> [extra arguments for the runscripts]".format(sys.argv[0]))
  except Exception as e:
    print("Uncaught exception {0}".format(e))
  finally:
    print("% Runner terminated.")
 
if __name__ == "__main__":
  main()


