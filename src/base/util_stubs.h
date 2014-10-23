/* C Stubs */

#include "caml/mlvalues.h"

/** Limit memory usage to the given amount of megabytes, using setrlimit.
 *  Type:  int -> unit */
CAMLprim value logtk_set_memory_limit(CAMLprim megabytes);
