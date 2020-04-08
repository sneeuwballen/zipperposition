%------------------------------------------------------------------------------
% File     : PUZ085^1 : TPTP v6.2.0. Released v4.0.0.
% Domain   : Logic Calculi (Espistemic logic)
% Problem  : The friends puzzle - transitivity for Peter's wife
% Version  : [Ben09] axioms.
% English  : (i) Peter is a friend of John, so if Peter knows that John knows
%            something then John knows that Peter knows the same thing. 
%            (ii) Peter is married, so if Peter's wife knows something, then
%            Peter knows the same thing. John and Peter have an appointment,
%            let us consider the following situation: (a) Peter knows the time
%            of their appointment. (b) Peter also knows that John knows the
%            place of their appointment. Moreover, (c) Peter's wife knows that
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
% Names    : mmex2.p [Ben09]

% Status   : Theorem
% Rating   : 0.14 v5.5.0, 0.17 v5.4.0, 0.20 v5.3.0, 0.40 v5.2.0, 0.20 v5.1.0, 0.40 v5.0.0, 0.20 v4.1.0, 0.33 v4.0.0
% Syntax   : Number of formulae    :   73 (   1 unit;  35 type;  31 defn)
%            Number of atoms       :  405 (  36 equality; 132 variable)
%            Maximal formula depth :   11 (   6 average)
%            Number of connectives :  144 (   4   ~;   4   |;   8   &; 120   @)
%                                         (   0 <=>;   8  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :  178 ( 178   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   40 (  35   :)
%            Number of variables   :   85 (   3 sgn;  29   !;   6   ?;  50   ^)
%                                         (  85   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU

% Comments : 
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

thf(conj,conjecture,
    ( mvalid
    @ ( mforall_prop
      @ ^ [A: $i > $o] :
          ( mimplies @ ( mbox @ ( wife @ peter ) @ A ) @ ( mbox @ ( wife @ peter ) @ ( mbox @ ( wife @ peter ) @ A ) ) ) ) )).

%------------------------------------------------------------------------------
