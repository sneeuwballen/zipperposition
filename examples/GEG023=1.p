%------------------------------------------------------------------------------
% File     : GEG023=1 : TPTP v6.1.0. Bugfixed v5.2.0.
% Domain   : Arithmetic
% Problem  : Estimate distance between cities (three steps)
% Version  : Especial.
% English  :

% Refs     : [Wal10] Waldmann (2010), Email to Geoff Sutcliffe
% Source   : [Wal10]
% Names    :

% Status   : Theorem
% Rating   : 0.62 v6.1.0, 0.67 v6.0.0, 0.71 v5.5.0, 0.67 v5.4.0, 0.75 v5.3.0, 0.80 v5.2.0
% Syntax   : Number of formulae    :   10 (   8 unit;   9 type)
%            Number of atoms       :   27 (  14 equality)
%            Maximal formula depth :   16 (   4 average)
%            Number of connectives :   15 (   0   ~;   0   |;  14   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    2 (   1   >;   1   *;   0   +;   0  <<)
%            Number of predicates  :   13 (  11 propositional; 0-2 arity)
%            Number of functors    :   22 (  20 constant; 0-2 arity)
%            Number of variables   :    6 (   0 sgn;   6   !;   0   ?)
%            Maximal term depth    :    3 (   2 average)
%            Arithmetic symbols    :   15 (   2 pred;    0 func;   13 numbers)
% SPC      : TF0_THM_EQU_ARI

% Comments :
% Bugfixes : v5.2.0 - Changed $plus to $sum.
%------------------------------------------------------------------------------
tff(city_type,type,(
    city: $tType )).

tff(d_type,type,(
    d: ( city * city ) > $int )).

tff(kiel_type,type,(
    kiel: city )).

tff(hamburg_type,type,(
    hamburg: city )).

tff(berlin_type,type,(
    berlin: city )).

tff(cologne_type,type,(
    cologne: city )).

tff(frankfurt_type,type,(
    frankfurt: city )).

tff(saarbruecken_type,type,(
    saarbruecken: city )).

tff(munich_type,type,(
    munich: city )).

tff(city_distance_3,conjecture,
    ( ( ! [X: city,Y: city] : d(X,Y) = d(Y,X)
      & ! [X: city,Y: city,Z: city] : $lesseq(d(X,Z),$sum(d(X,Y),d(Y,Z)))
      & ! [X: city] : d(X,X) = 0
      & d(berlin,munich) = 510
      & d(berlin,cologne) = 480
      & d(berlin,frankfurt) = 420
      & d(saarbruecken,frankfurt) = 160
      & d(saarbruecken,cologne) = 190
      & d(hamburg,cologne) = 360
      & d(hamburg,frankfurt) = 390
      & d(cologne,frankfurt) = 150
      & d(hamburg,kiel) = 90
      & d(hamburg,berlin) = 250
      & d(munich,frankfurt) = 300
      & d(munich,saarbruecken) = 360 )
   => $lesseq(d(kiel,saarbruecken),640) )).

%------------------------------------------------------------------------------
