import subprocess as sp
import sys
import re

time_left = None

STATUS_REGEX = re.compile(r'\% SZS status (\w+)')


class Configuration:
  import Enum
  class RunResult(Enum):
    THEOREM = 1
    GAVE_UP = 2
    TIMEOUT = 3


  def __init__(self, conf_path, ratio):
    self._conf_path = conf_path
    self._ratio = ratio
    self._if_timeout = None
    self._if_gave_up = None

  def conf_path(self):
    self._conf_path

  def set_if_timeout(self, conf):
    self._if_timeout = conf

  def set_if_gave_up(self, conf):
    self._if_gave_up = conf

  def if_timeout(self):
    return self._if_timeout
  
  def if_gave_up(self):
    return self._if_gave_up

  def _do_run(self, prob_path):
    try:
      from math import floor
      timeout = int(floor(self._ratio*time_left)) \
                if self.if_timeout() is not None \
                else time_left
      proc_res = sp.run([self.conf_path(), prob_path, str(timeout)], 
                        stdout=sp.PIPE, stderr=sp.DEVNULL, timeout=timeout)
      res_out = proc_res.stdout.decode('utf-8')
      matches = STATUS_REGEX.findall(res_out)
      if any(matches):
        match = matches[0]
        if match.strip() == "Theorem":
          print(res_out)
          return RunResult.THEOREM
        else:
          return RunResult.GAVE_UP
      return RunResult.TIMEOUT
    except Exception:
      # timeouts and things...
      return RunResult.TIMEOUT

  def run(self, prob_path):
    from timeit import default_timer as timer
    start = timer()
    res   = self._do_run(prob_path)
    end   = timer()

    from math import ceil
    time_elapsed = int(ceil(end-start))
    time_left -= time_elapsed
    if res == RunResult.THEOREM:
      return res
    elif res == RunResult.TIMEOUT:
      if self._if_timeout is not None:
        return self._if_timeout.run(prob_path)
    elif res == RunResult.GAVE_UP:
      if self._if_gave_up is not None:
        return self._if_gave_up.run(prob_path)

    return RunResult.TIMEOUT 
    

COMPETITIVE = Configuration('competitive.sh', 0.25)
PRAGMATIC = Configuration('pragmatic.sh', 0.75)
COMPETITIVE_STRONG = Configuration('competitive.strong.sh', 1)
FULL = Configuration('full.sh', 1)

def setup_confs():
  COMPETITIVE.set_if_timeout(PRAGMATIC)
  COMPETITIVE.set_if_gave_up(COMPETITIVE_STRONG)

  PRAGMATIC.set_if_gave_up(COMPETITIVE_STRONG)

  COMPETITIVE_STRONG.set_if_gave_up(FULL)

def main():  
  try:
    prob_path = sys.argv[1]
    time_left = int(sys.argv[2])

    setup_confs()
    COMPETITIVE.run(prob_path)
  except IndexError:
    print("Usage: python {0} <conf_1> <conf_2> <timeout> <problem>".format(sys.argv[0]))
  except Exception as e:
    print(e)
 
if __name__ == "__main__":
  main()