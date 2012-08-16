%------------------------------------------------------------------------------
% File     : PUZ071+1 : TPTP v5.2.0. Released v3.2.0.
% Domain   : Puzzles (Sudoku)
% Problem  : Thursday's Sudoku
% Version  : [Hil06] axioms : Especial.
% English  : 45  8
%                   71
%             2
%              16   3
%            5   4 2
%
%            2     8
%               1 3

% Refs     : [Hil06] Hillenbrand (2006), Email to G. Sutcliffe
% Source   : [Hil06]
% Names    :

% Status   : Satisfiable
% Rating   : 0.25 v5.2.0, 0.00 v4.0.1, 0.50 v3.7.0, 0.00 v3.5.0, 0.33 v3.4.0, 0.60 v3.3.0, 0.00 v3.2.0
% Syntax   : Number of formulae    :  370 (  18 unit)
%            Number of atoms       : 3942 (3941 equality)
%            Maximal formula depth :   37 (  11 average)
%            Number of connectives : 4580 (1008   ~;2592   |; 980   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    2 (   0 propositional; 2-81 arity)
%            Number of functors    :   10 (   9 constant; 0-2 arity)
%            Number of variables   :    0 (   0 sgn;   0   !;   0   ?)
%            Maximal term depth    :    2 (   2 average)
% SPC      : FOF_SAT_EPR

% Comments :
%------------------------------------------------------------------------------
include('Axioms/PUZ005+0.ax').
%------------------------------------------------------------------------------
fof(ax353,axiom,(
    ssA(n1,n1) = n4 )).

fof(ax354,axiom,(
    ssA(n1,n2) = n5 )).

fof(ax355,axiom,(
    ssA(n1,n5) = n8 )).

fof(ax356,axiom,(
    ssA(n2,n8) = n7 )).

fof(ax357,axiom,(
    ssA(n2,n9) = n1 )).

fof(ax358,axiom,(
    ssA(n3,n2) = n2 )).

fof(ax359,axiom,(
    ssA(n4,n3) = n1 )).

fof(ax360,axiom,(
    ssA(n4,n4) = n6 )).

fof(ax361,axiom,(
    ssA(n4,n8) = n3 )).

fof(ax362,axiom,(
    ssA(n5,n1) = n5 )).

fof(ax363,axiom,(
    ssA(n5,n5) = n4 )).

fof(ax364,axiom,(
    ssA(n5,n7) = n2 )).

fof(ax365,axiom,(
    ssA(n7,n1) = n2 )).

fof(ax366,axiom,(
    ssA(n7,n7) = n8 )).

fof(ax367,axiom,(
    ssA(n8,n4) = n1 )).

fof(ax368,axiom,(
    ssA(n8,n6) = n3 )).

fof(ax369,axiom,(
    ssA(n9,n4) = n7 )).

%------------------------------------------------------------------------------
