/* C Stubs */

#include "caml/mlvalues.h"

/** Limit memory usage to the given amount of megabytes, using setrlimit.
 *  Type:  int -> unit */
CAMLprim value logtk_set_memory_limit(CAMLprim megabytes);

/** Limit CPU time usage to the given amount of seconds, using setrlimit.
 *  Type:  int -> unit */
CAMLprim value logtk_set_time_limit(CAMLprim megabytes);
