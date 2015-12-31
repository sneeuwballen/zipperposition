tff(nat_ind, type, nat : $tType, inductive(s, z)).

tff(ty_s, type, s:nat > nat).
tff(ty_z, type, z:nat).
tff(ty_plus, type, plus : (nat*nat) > nat).


tff(the, conjecture, ![X:nat]: (X != s(X))).
