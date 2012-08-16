%------------------------------------------------------------------------------
% File     : PUZ080+2 : TPTP v5.2.0. Released v3.5.0.
% Domain   : Puzzles (Sudoku)
% Problem  : Another Sudoku
% Version  : [Kos08] axioms : Especial.
% English  :    3 1
%             7  9  6
%            9       4
%            4  8 6  2
%               4 7
%                1
%              67 21
%            3 1   9 7
%              8   3

% Refs     : [Kos08] Kossey (2008), Email to G. Sutcliffe
% Source   : [Kos08]
% Names    :

% Status   : Satisfiable
% Rating   : 0.25 v5.2.0, 0.33 v5.0.0, 0.00 v4.1.0, 0.50 v4.0.1, 0.33 v3.5.0
% Syntax   : Number of formulae    : 10554 (  24 unit)
%            Number of atoms       : 23352 (   0 equality)
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
fof(ax143,axiom,(
    p(n1,n4,n3) )).

fof(ax161,axiom,(
    p(n1,n6,n1) )).

fof(ax227,axiom,(
    p(n2,n2,n7) )).

fof(ax259,axiom,(
    p(n2,n5,n9) )).

fof(ax286,axiom,(
    p(n2,n8,n6) )).

fof(ax319,axiom,(
    p(n3,n1,n9) )).

fof(ax394,axiom,(
    p(n3,n9,n4) )).

fof(ax414,axiom,(
    p(n4,n1,n4) )).

fof(ax448,axiom,(
    p(n4,n4,n8) )).

fof(ax466,axiom,(
    p(n4,n6,n6) )).

fof(ax492,axiom,(
    p(n4,n9,n2) )).

fof(ax544,axiom,(
    p(n5,n4,n4) )).

fof(ax567,axiom,(
    p(n5,n6,n7) )).

fof(ax651,axiom,(
    p(n6,n5,n1) )).

fof(ax736,axiom,(
    p(n7,n3,n6) )).

fof(ax747,axiom,(
    p(n7,n4,n7) )).

fof(ax762,axiom,(
    p(n7,n6,n2) )).

fof(ax771,axiom,(
    p(n7,n7,n1) )).

fof(ax813,axiom,(
    p(n8,n1,n3) )).

fof(ax831,axiom,(
    p(n8,n3,n1) )).

fof(ax879,axiom,(
    p(n8,n7,n9) )).

fof(ax897,axiom,(
    p(n8,n9,n7) )).

fof(ax938,axiom,(
    p(n9,n3,n8) )).

fof(ax973,axiom,(
    p(n9,n7,n3) )).

%------------------------------------------------------------------------------
