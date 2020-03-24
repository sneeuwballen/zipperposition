
include('list.ax').

% L != [] ==> L = butlast L @ [last L]

tff(the, conjecture, ![L:list]:
    (L != nil => L = append(butlast(L), cons(last(L),nil)))).
