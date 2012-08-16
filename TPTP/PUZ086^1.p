%------------------------------------------------------------------------------
% File     : PUZ086^1 : TPTP v5.2.0. Released v4.0.0.
% Domain   : Logic Calculi (Espistemic logic)
% Problem  : The friends puzzle - they both know
% Version  : [Ben09] axioms.
% English  : (i) Peter is a friend of John, so if Peter knows that John knows
%            something then John knows that Peter knows the same thing. 
%            (ii) Peter is married, so if Peter’s wife knows something, then
%            Peter knows the same thing. John and Peter have an appointment,
%            let us consider the following situation: (a) Peter knows the time
%            of their appointment. (b) Peter also knows that John knows the
%            place of their appointment. Moreover, (c) Peter’s wife knows that
%            if Peter knows the time of their appointment, then John knows
%            that too (since John and Peter are friends). Finally, (d) Peter
%            knows that if John knows the place and the time of their
%            appointment, then John knows that he has an appointment. From
%            this situation we want to prove (e) that each of the two friends
%            knows that the other one knows that he has an appointment.

% Refs     : [Gol92] Goldblatt (1992), Logics of Time and Computation
%          : [Bal98] Baldoni (1998), Normal Multimodal Logics: Automatic De
%          : [Ben09] Benzmueller (2009), Email to Geoff Sutcliffe
% Source   : [Ben09]
% Names    : mmex3.p [Ben09]

% Status   : Theorem
% Rating   : 0.40 v5.2.0, 0.60 v5.1.0, 0.80 v5.0.0, 0.40 v4.1.0, 0.33 v4.0.1, 0.67 v4.0.0
% Syntax   : Number of formulae    :   82 (   1 unit;  38 type;  31 defn)
%            Number of atoms       :  463 (  36 equality; 134 variable)
%            Maximal formula depth :   11 (   6 average)
%            Number of connectives :  190 (   4   ~;   4   |;   8   &; 166   @)
%                                         (   0 <=>;   8  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :  182 ( 182   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   43 (  38   :)
%            Number of variables   :   86 (   3 sgn;  29   !;   6   ?;  51   ^)
%                                         (  86   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_THM_EQU

% Comments : THF0 syntax
%------------------------------------------------------------------------------
%----Include embedding of quantified multimodal logic in simple type theory
include('Axioms/LCL013^0.ax').
%------------------------------------------------------------------------------
thf(peter,type,(
    peter: $i > $i > $o )).

thf(john,type,(
    john: $i > $i > $o )).

thf(wife,type,(
    wife: ( $i > $i > $o ) > $i > $i > $o )).

thf(refl_peter,axiom,
    ( mreflexive @ peter )).

thf(refl_john,axiom,
    ( mreflexive @ john )).

thf(refl_wife_peter,axiom,
    ( mreflexive @ ( wife @ peter ) )).

thf(trans_peter,axiom,
    ( mtransitive @ peter )).

thf(trans_john,axiom,
    ( mtransitive @ john )).

thf(trans_wife_peter,axiom,
    ( mtransitive @ ( wife @ peter ) )).

thf(ax_i,axiom,
    ( mvalid
    @ ( mforall_prop
      @ ^ [A: $i > $o] :
          ( mimplies @ ( mbox @ peter @ ( mbox @ john @ A ) ) @ ( mbox @ john @ ( mbox @ peter @ A ) ) ) ) )).

thf(ax_ii,axiom,
    ( mvalid
    @ ( mforall_prop
      @ ^ [A: $i > $o] :
          ( mimplies @ ( mbox @ ( wife @ peter ) @ A ) @ ( mbox @ peter @ A ) ) ) )).

thf(time,type,(
    time: $i > $o )).

thf(place,type,(
    place: $i > $o )).

thf(appointment,type,(
    appointment: $i > $o )).

thf(ax_a,axiom,
    ( mvalid @ ( mbox @ peter @ time ) )).

thf(ax_b,axiom,
    ( mvalid @ ( mbox @ peter @ ( mbox @ john @ place ) ) )).

thf(ax_c,axiom,
    ( mvalid @ ( mbox @ ( wife @ peter ) @ ( mimplies @ ( mbox @ peter @ time ) @ ( mbox @ john @ time ) ) ) )).

thf(ax_d,axiom,
    ( mvalid @ ( mbox @ peter @ ( mbox @ john @ ( mimplies @ ( mand @ place @ time ) @ appointment ) ) ) )).

thf(conj,conjecture,
    ( mvalid @ ( mand @ ( mbox @ peter @ ( mbox @ john @ appointment ) ) @ ( mbox @ john @ ( mbox @ peter @ appointment ) ) ) )).

%------------------------------------------------------------------------------
