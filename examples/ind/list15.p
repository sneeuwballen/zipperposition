
% len (l1 @ l2) = len(l1) + len(l2)

include('list.ax').
include('nat.ax').

tff(the, conjecture,
    ![L1:list, L2:list]:
        ( length(append(L1, L2)) = plus(length(L1), length(L2)) )
    ).
