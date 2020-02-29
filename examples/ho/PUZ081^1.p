%------------------------------------------------------------------------------
% File     : PUZ081^1 : TPTP v6.2.0. Released v3.6.0.
% Domain   : Puzzles
% Problem  : 1 of http://philosophy.hku.hk/think/logic/knight.php
% Version  : [Ben08] axioms : Especial.
% English  : A very special island is inhabited only by knights and knaves.
%            Knights always tell the truth, and knaves always lie. You meet two
%            inhabitants: Zoey and Mel. Zoey tells you that Mel is a knave. Mel
%            says, `Neither Zoey nor I are knaves'. Who is a knight and who is
%            a knave?

% Refs     : [Ben08] Benzmueller (2008), Email to G. Sutcliffe
% Source   : [Ben08]
% Names    : Knights+Knaves_1b [Ben08]

% Status   : Theorem
% Rating   : 0.29 v6.1.0, 0.43 v5.5.0, 0.33 v5.4.0, 0.40 v5.3.0, 0.60 v5.0.0, 0.40 v4.1.0, 0.33 v4.0.1, 0.67 v4.0.0, 0.33 v3.7.0
% Syntax   : Number of formulae    :   14 (   5 unit;   7 type;   0 defn)
%            Number of atoms       :   71 (   4 equality;  17 variable)
%            Maximal formula depth :    8 (   4 average)
%            Number of connectives :   47 (   2   ~;   2   |;   4   &;  32   @)
%                                         (   0 <=>;   5  =>;   0  <=;   2 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    4 (   4   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   10 (   7   :)
%            Number of variables   :    7 (   0 sgn;   5   !;   2   ?;   0   ^)
%                                         (   7   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU

% Comments : See http://philosophy.hku.hk/think/logic/knight.php
%------------------------------------------------------------------------------
%----Type declarations
thf(islander,type,(
    islander: $i )).

thf(knight,type,(
    knight: $i )).

thf(knave,type,(
    knave: $i )).

thf(says,type,(
    says: $i > $o > $o )).

thf(zoey,type,(
    zoey: $i )).

thf(mel,type,(
    mel: $i )).

thf(is_a,type,(
    is_a: $i > $i > $o )).

%----A very special island is inhabited only by knights and knaves.
thf(kk_6_1,axiom,(
    ! [X: $i] :
      ( ( is_a @ X @ islander )
     => ( ( is_a @ X @ knight )
        | ( is_a @ X @ knave ) ) ) )).

%----Knights always tell the truth
thf(kk_6_2,axiom,(
    ! [X: $i] :
      ( ( is_a @ X @ knight )
     => ! [A: $o] :
          ( ( says @ X @ A )
         => A ) ) )).

%----Knaves always lie
thf(kk_6_3,axiom,(
    ! [X: $i] :
      ( ( is_a @ X @ knave )
     => ! [A: $o] :
          ( ( says @ X @ A )
         => ~ ( A ) ) ) )).

%----Zoey and Mel are islanders
thf(kk_6_4,axiom,
    ( ( is_a @ zoey @ islander )
    & ( is_a @ mel @ islander ) )).

%----Zoey says Mel is a knave
thf(kk_6_5,axiom,
    ( says @ zoey @ ( is_a @ mel @ knave ) )).

%----Mel says 'Neither Zoey nor I are knaves'
thf(kk_6_6,axiom,
    ( says @ mel
    @ ~ ( ( is_a @ zoey @ knave )
        | ( is_a @ mel @ knave ) ) )).

%----Who is a knight and who is a knave?
thf(query,conjecture,(
    ? [Y: $i,Z: $i] :
      ( ( ( Y = knight )
      <~> ( Y = knave ) )
      & ( ( Z = knight )
      <~> ( Z = knave ) )
      & ( is_a @ mel @ Y )
      & ( is_a @ zoey @ Z ) ) )).

%------------------------------------------------------------------------------
