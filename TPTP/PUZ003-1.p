%--------------------------------------------------------------------------
% File     : PUZ003-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : The Barber Puzzle
% Version  : Especial.
% English  : There is a barbers' club that obeys the following three
%            conditions:
%            (1) If any member A has shaved any other member B - whether
%                himself or another - then all members have shaved A,
%                though not necessarily at the same time.
%            (2) Four of the members are named Guido, Lorenzo, Petrucio,
%                and Cesare.
%            (3) Guido has shaved Cesare.
%            Prove Petrucio has shaved Lorenzo

% Refs     :
% Source   : [ANL]
% Names    : barber.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :    8 (   0 non-Horn;   6 unit;   8 RR)
%            Number of atoms       :   13 (   0 equality)
%            Maximal clause size   :    4 (   2 average)
%            Number of predicates  :    2 (   0 propositional; 1-2 arity)
%            Number of functors    :    5 (   5 constant; 0-0 arity)
%            Number of variables   :    4 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments :
%--------------------------------------------------------------------------
cnf(one_shaved_then_all_shaved,axiom,
    ( ~ member(X)
    | ~ member(Y)
    | ~ shaved(X,Y)
    | shaved(members,X) )).

cnf(all_shaved_then_one_shaved,axiom,
    ( ~ shaved(members,X)
    | ~ member(Y)
    | shaved(Y,X) )).

cnf(guido,hypothesis,
    ( member(guido) )).

cnf(lorenzo,hypothesis,
    ( member(lorenzo) )).

cnf(petruchio,hypothesis,
    ( member(petruchio) )).

cnf(cesare,hypothesis,
    ( member(cesare) )).

cnf(guido_has_shaved_cesare,hypothesis,
    ( shaved(guido,cesare) )).

cnf(prove_petruchio_has_shaved_lorenzo,negated_conjecture,
    ( ~ shaved(petruchio,lorenzo) )).

%--------------------------------------------------------------------------
