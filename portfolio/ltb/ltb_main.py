VERSIONS_TO_TRY = [("^7", 0.4), ("^5", 0.2), ("^4", 0.2), ("_7", 0.2)]

from batch import Batch, BatchParseError
from runner import ProverRunner
import concurrent as cc
import concurrent.futures
import re

VERSION_REGEX = re.compile(r"[+_^]\d+\.p")


def run_ltb(batch, prover, out_folder):
  to_per_prob = batch.overall_timeout() / len(batch.problems())


  from os import path as p
  import multiprocessing

  m = multiprocessing.Manager()
  stdout_lock = m.Lock()

  import os
  n_cpus = len(os.sched_getaffinity(0))
  with cc.futures.ProcessPoolExecutor(max_workers=n_cpus-1) as pool:
    results = []
    
    for (p_path, p_out) in batch.problems():
      paths_w_to = [(p.join(batch.base_dir(), p_path.replace('*', vers)),
                    to_per_prob * to_per_vers) for (vers, to_per_vers) in VERSIONS_TO_TRY]
      out_path = p.join(out_folder, p_out)
      results.append(pool.submit(prover.run, p_path, paths_w_to, out_path, stdout_lock))
      
    
    for fut_res in cc.futures.as_completed(results, timeout=batch.overall_timeout()):
      ans,id = fut_res.result(timeout=to_per_prob)
      with open('/tmp/ltb_messages', 'a') as log:
        log.write("Ans {0} for {1}\n".format(ans,id))
      
      # from os.path import join
      # out_path = join(out_folder, path_to_out[path])
      # print('% SZS status {0} for {1}'.format(status, path))
      # if proof_text is not None:
      #   with open(out_path, 'w') as out_f:
      #     out_f.write(proof_text)
      # print('% SZS status End for {0}'.format(VERSION_REGEX.sub("", path)))
    

def main():
  import os, sys
  if len(sys.argv) < 3:
    print("usage: python3 {0} <batch conf file> <out directory>".format(sys.argv[0]))
  else:
    try:
      batch = Batch(sys.argv[1])
      cwd = os.path.dirname(os.path.realpath(__file__))
      prover = ProverRunner(os.path.join(cwd, "run_ltb.sh"), sys.argv[3])
      run_ltb(batch, prover, sys.argv[2])
    except BatchParseError as err:
      print("Error parsing {0}: {1})".format(sys.argv[1], err))

if __name__ == "__main__":
  main()
