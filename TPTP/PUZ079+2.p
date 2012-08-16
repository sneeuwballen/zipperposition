%------------------------------------------------------------------------------
% File     : PUZ079+2 : TPTP v5.2.0. Released v3.5.0.
% Domain   : Puzzles (Sudoku)
% Problem  : Another Sudoku
% Version  : [Kos08] axioms : Especial.
% English  : 1  6 4  9
%               5 7
%              4 9 5
%            47     53
%              3   1
%            91     68
%              7 5 2
%               9 3
%            2  1 6  4

% Refs     : [Kos08] Kossey (2008), Email to G. Sutcliffe
% Source   : [Kos08]
% Names    :

% Status   : Satisfiable
% Rating   : 0.00 v4.1.0, 0.50 v4.0.1, 0.33 v3.5.0
% Syntax   : Number of formulae    : 10558 (  28 unit)
%            Number of atoms       : 23356 (   0 equality)
%            Maximal formula depth :    9 (   3 average)
%            Number of connectives : 23004 (10206   ~;2592   |;   0   &)
%                                         (   0 <=>;10206  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    1 (   0 propositional; 3-3 arity)
%            Number of functors    :    9 (   9 constant; 0-0 arity)
%            Number of variables   :    0 (   0 sgn;   0   !;   0   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_SAT_EPR

% Comments :
%------------------------------------------------------------------------------
include('Axioms/PUZ006+0.ax').
%------------------------------------------------------------------------------
fof(ax353,axiom,(
    p(n1,n1,n1) )).

fof(ax354,axiom,(
    p(n1,n4,n6) )).

fof(ax355,axiom,(
    p(n1,n6,n4) )).

fof(ax356,axiom,(
    p(n1,n9,n9) )).

fof(ax357,axiom,(
    p(n2,n4,n5) )).

fof(ax358,axiom,(
    p(n2,n6,n7) )).

fof(ax359,axiom,(
    p(n3,n3,n4) )).

fof(ax360,axiom,(
    p(n3,n5,n9) )).

fof(ax369,axiom,(
    p(n3,n7,n5) )).

fof(ax361,axiom,(
    p(n4,n1,n4) )).

fof(ax362,axiom,(
    p(n4,n2,n7) )).

fof(ax363,axiom,(
    p(n4,n8,n5) )).

fof(ax364,axiom,(
    p(n4,n9,n3) )).

fof(ax365,axiom,(
    p(n5,n3,n3) )).

fof(ax366,axiom,(
    p(n5,n7,n1) )).

fof(ax367,axiom,(
    p(n6,n1,n9) )).

fof(ax368,axiom,(
    p(n6,n2,n1) )).

fof(ax369_1,axiom,(
    p(n6,n8,n6) )).

fof(ax369_2,axiom,(
    p(n6,n9,n8) )).

fof(ax369_3,axiom,(
    p(n7,n3,n7) )).

fof(ax369_4,axiom,(
    p(n7,n5,n5) )).

fof(ax369_5,axiom,(
    p(n7,n7,n2) )).

fof(ax369_6,axiom,(
    p(n8,n4,n9) )).

fof(ax369_7,axiom,(
    p(n8,n6,n3) )).

fof(ax369_8,axiom,(
    p(n9,n1,n2) )).

fof(ax369_9,axiom,(
    p(n9,n4,n1) )).

fof(ax369_10,axiom,(
    p(n9,n6,n6) )).

fof(ax369_11,axiom,(
    p(n9,n9,n4) )).

%------------------------------------------------------------------------------
