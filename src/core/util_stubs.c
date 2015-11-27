/* C Stubs */

#include "caml/mlvalues.h"
#include <caml/memory.h>

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/resource.h>

CAMLprim value logtk_set_memory_limit(value megabytes)
{
  struct rlimit r = { 0 };
  rlim_t limit = Int_val(megabytes) * 1024 * 1024; /* in bytes */
  int err;

  CAMLparam0();

  r.rlim_cur = limit;
  r.rlim_max = limit;

  err = setrlimit(RLIMIT_AS, &r);
  if (err != 0)
  {
    fprintf(stderr, "could not set the memory limit to %ld bytes: %s\n",
            (long)limit, strerror(errno));
    exit(1);
  }

  CAMLreturn (Val_unit);
}

CAMLprim value logtk_set_time_limit(value t)
{
  struct rlimit r = { 0 };
  rlim_t limit = Int_val(t); /* in seconds */
  int err;

  CAMLparam0();

  r.rlim_cur = limit;
  r.rlim_max = limit;

  err = setrlimit(RLIMIT_CPU, &r);
  if (err != 0)
  {
    fprintf(stderr, "could not set the time limit to %ds: %s\n",
            (int)limit, strerror(errno));
    exit(1);
  }

  CAMLreturn (Val_unit);
}
