%------------------------------------------------------------------------------
% File     : PUZ082^1 : TPTP v5.2.0. Released v3.6.0.
% Domain   : Puzzles
% Problem  : Peter the liar
% Version  : Especial.
% English  : Peter says that everything he says is false. Show that not
%            everything Peter says is false.

% Refs     : [Ben08] Benzmueller (2008), Email to G. Sutcliffe
% Source   : [Ben08]
% Names    :

% Status   : Theorem
% Rating   : 0.50 v5.2.0, 0.25 v4.1.0, 0.33 v3.7.0
% Syntax   : Number of formulae    :    4 (   1 unit;   2 type;   0 defn)
%            Number of atoms       :   14 (   0 equality;   4 variable)
%            Maximal formula depth :    6 (   4 average)
%            Number of connectives :   11 (   3   ~;   0   |;   0   &;   6   @)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    2 (   2   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    5 (   2   :)
%            Number of variables   :    2 (   0 sgn;   2   !;   0   ?;   0   ^)
%                                         (   2   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_THM_NEQ

% Comments : THF0 syntax
%------------------------------------------------------------------------------
%----Signature
thf(peter,type,(
    peter: $i )).

thf(says,type,(
    says: $i > $o > $o )).

%----Axioms
thf(ax1,axiom,
    ( says @ peter
    @ ! [X: $o] :
        ( ( says @ peter @ X )
       => ~ ( X ) ) )).

thf(thm,conjecture,(
    ~ ( ! [X: $o] :
          ( ( says @ peter @ X )
         => ~ ( X ) ) ) )).

%------------------------------------------------------------------------------
