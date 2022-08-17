
class BatchParseError(Exception):
   pass

class Batch:
  def __init__(self, batch_path):
    import os.path as p

    self._path = batch_path
    self._base_dir,_ = p.split(batch_path)
    found_time, found_problems, = False, False
    with open(batch_path, 'r') as batch_f :
      for line in batch_f:
        if line.strip().startswith("division.category"):
          self._name = line.split()[1]
        
        if line.strip().startswith("limit.time.overall.wc"):
          self._timeout = int(line.split()[1])
          found_time = True
        
        if line.strip().startswith("% SZS start BatchProblems"):
          line = next(batch_f)
          problems = []
          found_problems = True
          while not line.strip().startswith("% SZS end BatchProblems"):
            (prob_path, prob_out_name) = line.split()
            problems.append((prob_path, prob_out_name))
            line = next(batch_f)
          self._problems = problems
        
    if not found_time:
      raise BatchParseError("overall timeout not found in batch description")
    if not found_problems:
      raise BatchParseError("problem list not found in batch description")

  
  def overall_timeout(self):
    import os
    n_cpus = len(os.sched_getaffinity(0))
    return (n_cpus-1)*self._timeout

  
  def problems(self):
    return self._problems


  def base_dir(self):
    return self._base_dir
  
          
