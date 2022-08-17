# if intialized with the script that gets three arguments --
# problem, timeout and output path
# it can run the given script and process its output

import re

THM_STATUSES = ["Theorem", "Unsatisfiable", "ContradictoryAxioms"]

SZS_STATUS_PATTERN = re.compile(r'%?\s*SZS status (.*) for')
def parse_proof_output(prover_output, stdout_lock):
  match = SZS_STATUS_PATTERN.search(prover_output)
  if match is not None:
    status = match.group(1)
    proof = prover_output[match.span()[0]:]
  else:
    status = "GaveUp"
    proof = None
  return (status, proof)

def print_locked(l, msg):
    l.acquire()
    try:
        print('{0}'.format(msg))
    finally:
        l.release()

class ProverRunner:
  def __init__(self, prover_bin, tmp_path):
    self._bin = prover_bin
    self._tmp_path = tmp_path

  def run(self, id, problems_w_to, out_path, stdout_lock):
    import subprocess as sp

    for problem,timeout in problems_w_to:
      args = [self._bin, problem, str(int(timeout)), self._tmp_path]
      # print("% timeout {0}".format(int(timeout)))
      
      try:
        print_locked(stdout_lock, "% SZS status Started for {0}"\
                                  "\n% Command: {1}.".format(problem, " ".join(args)))
        proc_res = sp.run(args, stdout=sp.PIPE, stderr=sp.DEVNULL, timeout=timeout)

        from os.path import exists

        if not exists(problem):
          raise LookupError("File not found")

        res_out = proc_res.stdout.decode('utf-8')
        # res_err = proc_res.stderr.decode('utf-8')
        
        status,proof = parse_proof_output(res_out, stdout_lock)
        print_locked(stdout_lock, 
          "% SZS status {1} for {0}"
          "\n% SZS status Ended for {0}".format(problem, status))

        if status.strip() in THM_STATUSES:
          with open(out_path, 'w') as out_f:
            assert proof is not None
            out_f.write(proof)
          return True,id
      
      except sp.TimeoutExpired:
        pass # execute the next problem
      except Exception as e:
        err = str(e).replace('\n', '\n%')
        print("% Problem with {0}:\n% >{1}".format(problem, err))
    
    # signal that the problem was not solved
    return False,id
