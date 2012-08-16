%------------------------------------------------------------------------------
% File     : PUZ093^5 : TPTP v5.2.0. Released v4.0.0.
% Domain   : Puzzles
% Problem  : TPS problem from BASIC-HO-EQ-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_1207 [Bro09]

% Status   : Theorem
% Rating   : 0.40 v5.2.0, 0.20 v4.1.0, 0.00 v4.0.1, 0.33 v4.0.0
% Syntax   : Number of formulae    :    1 (   0 unit;   0 type;   0 defn)
%            Number of atoms       :  160 (  32 equality; 128 variable)
%            Maximal formula depth :   27 (  27 average)
%            Number of connectives :  107 (  12   ~;   5   |;  19   &;  64   @)
%                                         (   0 <=>;   7  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    1 (   1   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    1 (   0   :)
%            Number of variables   :   13 (   0 sgn;   1   !;  12   ?;   0   ^)
%                                         (  13   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_THM_EQU

% Comments : This problem is from the TPS library. Copyright (c) 2009 The TPS
%            project in the Department of Mathematical Sciences at Carnegie
%            Mellon University. Distributed under the Creative Commons copyleft
%            license: http://creativecommons.org/licenses/by-sa/3.0/
%          : THF0 syntax
%------------------------------------------------------------------------------
thf(cSIXFRIENDS_PTH,conjecture,(
    ? [Xa: $i,Xaa: $i,Xb: $i,Xbb: $i,Xc: $i,Xcc: $i,Xd: $i,Xdd: $i,Xe: $i,Xee: $i,Xh: $i,Xhh: $i] :
    ! [P: $i > $i] :
      ( ( ( ( ( ( P @ Xa )
              = ( P @ Xaa ) )
            & ( ( P @ Xb )
              = ( P @ Xbb ) )
            & ( ( P @ Xe )
              = ( P @ Xhh ) ) )
         => ( ( P @ Xc )
            = ( P @ Xdd ) ) )
        & ( ( ( ( P @ Xa )
              = ( P @ Xaa ) )
            & ( ( P @ Xh )
              = ( P @ Xhh ) )
            & ( ( P @ Xb )
              = ( P @ Xcc ) ) )
         => ( ( P @ Xd )
           != ( P @ Xee ) ) )
        & ( ( ( ( P @ Xc )
              = ( P @ Xcc ) )
            & ( ( P @ Xcc )
              = ( P @ Xd ) )
            & ( ( P @ Xd )
              = ( P @ Xdd ) )
            & ( ( P @ Xa )
             != ( P @ Xbb ) ) )
         => ( ( P @ Xe )
           != ( P @ Xhh ) ) )
        & ( ( ( ( P @ Xa )
              = ( P @ Xaa ) )
            & ( ( P @ Xd )
              = ( P @ Xdd ) )
            & ( ( P @ Xb )
             != ( P @ Xcc ) ) )
         => ( ( P @ Xe )
            = ( P @ Xhh ) ) )
        & ( ( ( ( P @ Xe )
              = ( P @ Xee ) )
            & ( ( P @ Xh )
              = ( P @ Xhh ) )
            & ( ( P @ Xc )
              = ( P @ Xdd ) ) )
         => ( ( P @ Xa )
           != ( P @ Xbb ) ) )
        & ( ( ( ( P @ Xb )
              = ( P @ Xbb ) )
            & ( ( P @ Xbb )
              = ( P @ Xc ) )
            & ( ( P @ Xc )
              = ( P @ Xcc ) )
            & ( ( P @ Xe )
             != ( P @ Xhh ) ) )
         => ( ( P @ Xd )
            = ( P @ Xee ) ) ) )
     => ( ( ( P @ Xa )
         != ( P @ Xaa ) )
        | ( ( P @ Xb )
         != ( P @ Xbb ) )
        | ( ( P @ Xc )
         != ( P @ Xcc ) )
        | ( ( P @ Xd )
         != ( P @ Xdd ) )
        | ( ( P @ Xe )
         != ( P @ Xee ) )
        | ( ( P @ Xh )
         != ( P @ Xhh ) ) ) ) )).

%------------------------------------------------------------------------------
