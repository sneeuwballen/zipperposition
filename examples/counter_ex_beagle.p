
% problem exposed in paper on beagle

tff(ty_p, type, p : ($int * $i) > $o).
tff(g, conjecture, 
    (
      (![I:$int, J:$int, X:$i]:
        ( $lesseq(I,J)
        | p($sum(I,1), X)
        | p($sum(J,2), X)))
    &
      (![I:$int, J:$int, X:$i]:
        ( $lesseq(I,J)
        | ~ p($sum(I,3), X)
        | ~ p($sum(J,4), X)))

    ) => $false).
