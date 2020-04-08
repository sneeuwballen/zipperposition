
include('list.ax').

% L = butlast L @ [last L]     is false if L=[]

tff(the, conjecture, ![L:list]: L = append(butlast(L), cons(last(L),nil))).

