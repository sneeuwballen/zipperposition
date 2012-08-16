%--------------------------------------------------------------------------
% File     : PUZ011-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : An ocean that borders on an African and an Asian country
% Version  : Especial.
% English  : There is a database of assertions about various countries and
%            oceans and their relationships. Find which ocean borders
%            on African and Asian countries.

% Refs     : [Pla82] Plaisted (1982), A Simplified Problem Reduction Format
% Source   : [Pla82]
% Names    : Problem 5.6 [Pla82]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   27 (   0 non-Horn;  26 unit;  27 RR)
%            Number of atoms       :   31 (   0 equality)
%            Maximal clause size   :    5 (   1 average)
%            Number of predicates  :    5 (   0 propositional; 1-2 arity)
%            Number of functors    :   14 (  14 constant; 0-0 arity)
%            Number of variables   :    3 (   0 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments :
%--------------------------------------------------------------------------
cnf(atlantic,hypothesis,
    ( ocean(atlantic) )).

cnf(indian,hypothesis,
    ( ocean(indian) )).

cnf(atlantic_brazil,hypothesis,
    ( borders(atlantic,brazil) )).

cnf(atlantic_uruguay,hypothesis,
    ( borders(atlantic,uruguay) )).

cnf(atlantic_venesuela,hypothesis,
    ( borders(atlantic,venesuela) )).

cnf(atlantic_zaire,hypothesis,
    ( borders(atlantic,zaire) )).

cnf(atlantic_nigeria,hypothesis,
    ( borders(atlantic,nigeria) )).

cnf(atlantic_angola,hypothesis,
    ( borders(atlantic,angola) )).

cnf(indian_india,hypothesis,
    ( borders(indian,india) )).

cnf(indian_pakistan,hypothesis,
    ( borders(indian,pakistan) )).

cnf(indian_iran,hypothesis,
    ( borders(indian,iran) )).

cnf(indian_somalia,hypothesis,
    ( borders(indian,somalia) )).

cnf(indian_kenya,hypothesis,
    ( borders(indian,kenya) )).

cnf(indian_tanzania,hypothesis,
    ( borders(indian,tanzania) )).

cnf(brazil,hypothesis,
    ( south_american(brazil) )).

cnf(uruguay,hypothesis,
    ( south_american(uruguay) )).

cnf(venezuela,hypothesis,
    ( south_american(venesuela) )).

cnf(zaire,hypothesis,
    ( african(zaire) )).

cnf(nigeria,hypothesis,
    ( african(nigeria) )).

cnf(angola,hypothesis,
    ( african(angola) )).

cnf(somalia,hypothesis,
    ( african(somalia) )).

cnf(kenya,hypothesis,
    ( african(kenya) )).

cnf(tanzania,hypothesis,
    ( african(tanzania) )).

cnf(india,hypothesis,
    ( asian(india) )).

cnf(pakistan,hypothesis,
    ( asian(pakistan) )).

cnf(iran,hypothesis,
    ( asian(iran) )).

cnf(prove_there_is_a_common_ocean,negated_conjecture,
    ( ~ ocean(Ocean)
    | ~ borders(Ocean,African)
    | ~ african(African)
    | ~ borders(Ocean,Asian)
    | ~ asian(Asian) )).

%--------------------------------------------------------------------------
