
include('list.ax').

% append L nil = L

tff(the, conjecture, ![L:list]: append(L,nil) = L).
