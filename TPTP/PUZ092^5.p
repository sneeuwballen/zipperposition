%------------------------------------------------------------------------------
% File     : PUZ092^5 : TPTP v5.2.0. Released v4.0.0.
% Domain   : Puzzles
% Problem  : TPS problem from BASIC-HO-EQ-THMS
% Version  : Especial.
% English  :

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_1179 [Bro09]

% Status   : Theorem
% Rating   : 0.40 v5.1.0, 0.60 v5.0.0, 0.40 v4.1.0, 0.67 v4.0.0
% Syntax   : Number of formulae    :    1 (   0 unit;   0 type;   0 defn)
%            Number of atoms       :   96 (  32 equality;  64 variable)
%            Maximal formula depth :   25 (  25 average)
%            Number of connectives :   43 (  12   ~;   5   |;  19   &;   0   @)
%                                         (   0 <=>;   7  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    1 (   0   :)
%            Number of variables   :   12 (   0 sgn;  12   !;   0   ?;   0   ^)
%                                         (  12   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_THM_EQU

% Comments : This problem is from the TPS library. Copyright (c) 2009 The TPS
%            project in the Department of Mathematical Sciences at Carnegie
%            Mellon University. Distributed under the Creative Commons copyleft
%            license: http://creativecommons.org/licenses/by-sa/3.0/
%          : THF0 syntax
%------------------------------------------------------------------------------
thf(cSIXFRIENDS_EASIER,conjecture,(
    ! [ACRES: $i,MRSACRES: $i,BARRY: $i,MRSBARRY: $i,COLE: $i,MRSCOLE: $i,DIX: $i,MRSDIX: $i,EDEN: $i,MRSEDEN: $i,HALL: $i,MRSHALL: $i] :
      ( ( ( ( ( ACRES = MRSACRES )
            & ( BARRY = MRSBARRY )
            & ( EDEN = MRSHALL ) )
         => ( COLE = MRSDIX ) )
        & ( ( ( ACRES = MRSACRES )
            & ( HALL = MRSHALL )
            & ( BARRY = MRSCOLE ) )
         => ( DIX != MRSEDEN ) )
        & ( ( ( COLE = MRSCOLE )
            & ( DIX = MRSDIX )
            & ( DIX = COLE )
            & ( ACRES != MRSBARRY ) )
         => ( EDEN != MRSHALL ) )
        & ( ( ( ACRES = MRSACRES )
            & ( DIX = MRSDIX )
            & ( BARRY != MRSCOLE ) )
         => ( EDEN = MRSHALL ) )
        & ( ( ( EDEN = MRSEDEN )
            & ( HALL = MRSHALL )
            & ( COLE = MRSDIX ) )
         => ( ACRES != MRSBARRY ) )
        & ( ( ( BARRY = MRSBARRY )
            & ( COLE = MRSCOLE )
            & ( COLE = BARRY )
            & ( EDEN != MRSHALL ) )
         => ( DIX = MRSEDEN ) ) )
     => ( ( ACRES != MRSACRES )
        | ( BARRY != MRSBARRY )
        | ( COLE != MRSCOLE )
        | ( DIX != MRSDIX )
        | ( EDEN != MRSEDEN )
        | ( HALL != MRSHALL ) ) ) )).

%------------------------------------------------------------------------------
