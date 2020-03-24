% Comments :
%------------------------------------------------------------------------------
tff(test,conjecture,(
    ! [X : $int, Y : $int] : (
        $greater(27, $sum($product(11, X), $product(13, Y))) |
        $greater($sum($product(11, X), $product(13, Y)), 45) |
        $greater(-10, $sum($product(7, X), $product(-9, Y))) |
        $greater($sum($product(7, X), $product(-9, Y)), 4)
    ))).
%------------------------------------------------------------------------------
