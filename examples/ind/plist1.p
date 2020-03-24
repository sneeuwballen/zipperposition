
include('plist.ax').

% L != [] ==> L = butlast L @ [last L]

tff(the, conjecture, ![A:$tType,L:list(A)]:
    (L != nil(A) => L = append(A,butlast(A,L), cons(A,last(A,L),nil(A))))).
