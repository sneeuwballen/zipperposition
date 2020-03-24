include('list.ax').

% append associative

tff(the, conjecture, ![L1:list, L2:list, L3:list]:
    append(append(L1,L2),L3) = append(L1, append(L2,L3))).

