import subprocess as sp
import sys
import re

STATUS_REGEX = re.compile(r'\% SZS status (\w+)')
time_left    = None

class Configuration:
  class RunResult:
    THEOREM = 1
    GAVE_UP = 2
    TIMEOUT = 3


  def __init__(self, conf_path, ratio):
    self._conf_path = conf_path
    self._ratio = ratio
    self._if_timeout = None
    self._if_gave_up = None

  def conf_path(self):
    return self._conf_path

  def set_if_timeout(self, conf):
    self._if_timeout = conf

  def set_if_gave_up(self, conf):
    self._if_gave_up = conf

  def if_timeout(self):
    return self._if_timeout
  
  def if_gave_up(self):
    return self._if_gave_up

  def _do_run(self, prob_path):
    global time_left
    try:
      from math import floor
      import os
      cwd = os.path.dirname(os.path.realpath(__file__))
      conf_path = os.path.join(cwd, self.conf_path())

      timeout = int(floor(self._ratio*time_left)) \
                if self.if_timeout() is not None \
                else time_left
      print("% Running {0} for {1}".format(conf_path, timeout))

      # gnuchess = subprocess.Popen('gnuchess', stdin = subprocess.PIPE, stdout = subprocess.PIPE, stderr = subprocess.PIPE)

      # # Python 3 strings are Unicode and must be encoded before writing to a pipe (and decoded after reading)
      # gnuchess.stdin.write('e4\n'.encode())

      # while True:   
      # L = gnuchess.stdout.readline().decode()
      # L = L[0:-1]
      # print(L)
      # if L.startswith('My move is'):
      #     movimiento = L.split()[-1]
      #     break

      # print(movimiento)

      # gnuchess.stdin.write('exit\n'.encode())

      # gnuchess.terminate()

      proc_res = sp.Popen([conf_path, prob_path, str(timeout)], 
                          stdout=sp.PIPE, stderr=sp.PIPE)
      res_out = proc_res.stdout.read().decode('utf-8')
      matches = STATUS_REGEX.findall(res_out)
      if any(matches):
        match = matches[0]
        if match.strip() == "Theorem":
          print(res_out)
          return Configuration.RunResult.THEOREM
        elif match.strip() == "ResourceOut":
          return Configuration.RunResult.TIMEOUT
        else:
          return Configuration.RunResult.GAVE_UP
      return Configuration.RunResult.TIMEOUT
    except Exception as e:
      # timeouts and things...
      err_msg = "\n".join(map(lambda x: "% " + x, str(e).split('\n')))
      print("% Error:\n {0}".format(err_msg))
      return Configuration.RunResult.TIMEOUT

  def run(self, prob_path):
    from timeit import default_timer as timer
    start = timer()
    res   = self._do_run(prob_path)
    end   = timer()

    from math import ceil
    time_elapsed = int(ceil(end-start))
    global time_left
    time_left -= time_elapsed
    if res == Configuration.RunResult.THEOREM:
      return res
    elif res == Configuration.RunResult.TIMEOUT:
      if self._if_timeout is not None:
        return self._if_timeout.run(prob_path)
    elif res == Configuration.RunResult.GAVE_UP:
      if self._if_gave_up is not None:
        return self._if_gave_up.run(prob_path)
    return Configuration.RunResult.TIMEOUT 
    

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
    global time_left
    time_left = int(sys.argv[2])

    setup_confs()
    COMPETITIVE.run(prob_path)
  except IndexError:
    print("Usage: python3 {0} <prob_path> <timeout>".format(sys.argv[0]))
  except Exception as e:
    print(e)
 
if __name__ == "__main__":
  main()
