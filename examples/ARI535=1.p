%------------------------------------------------------------------------------
% File     : ARI535=1 : TPTP v6.1.0. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: Stickel's arithmetic challenge
% Version  : Especial.
% English  : 

% Refs     : [Sti10] Stickel (2010), Email to G. Sutcliffe
% Source   : [Sti10]
% Names    :

% Status   : Theorem
% Rating   : 0.40 v6.1.0, 0.44 v6.0.0, 0.50 v5.4.0, 0.38 v5.3.0, 0.43 v5.2.0, 0.60 v5.1.0, 0.75 v5.0.0
% Syntax   : Number of formulae    :    2 (   0 unit;   1 type)
%            Number of atoms       :    6 (   0 equality)
%            Maximal formula depth :    5 (   4 average)
%            Number of connectives :    1 (   0   ~;   0   |;   0   &)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    3 (   1   >;   2   *;   0   +;   0  <<)
%            Number of predicates  :    4 (   3 propositional; 0-3 arity)
%            Number of functors    :    3 (   1 constant; 0-2 arity)
%            Number of variables   :    2 (   0 sgn;   0   !;   2   ?)
%            Maximal term depth    :    2 (   1 average)
%            Arithmetic symbols    :    4 (   1 pred;    2 func;    1 numbers)
% SPC      : TF0_THM_NEQ_ARI

% Comments : 
%------------------------------------------------------------------------------
tff(p_type,type,
    p: ( $int * $int * $int ) > $o ).

tff(a,conjecture,(
    ? [X: $int,Y: $int] :
      ( p(2,Y,$sum(2,Y))
     => p(X,2,$product(X,2)) ) )).

%------------------------------------------------------------------------------
