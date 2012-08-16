%--------------------------------------------------------------------------
% File     : PUZ032-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Knights and Knaves #26
% Version  : Especial.
% English  : On a certain island the inhabitants are partitioned into those
%            who always tell the truth and those who always lie. I landed on
%            the island and met three inhabitants A, B, and C. I asked A,
%            'Are you a truthteller or a liar?' He mumbled something which I
%            couldn't make out. I asked B what A had said. B replied, 'A
%            said he was a liar'. C then volunteered, 'Don't believe B, he's
%            lying!' Prove C is a truthteller.

% Refs     : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
%          : [LO85]  Lusk & Overbeek (1985), Non-Horn Problems
% Source   : [LO85]
% Names    : Problem 26 [Smu78]
%          : Truthtellers and the Liars [LO85]
%          : tandl.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.2.1, 0.25 v2.1.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   10 (   2 non-Horn;   4 unit;   9 RR)
%            Number of atoms       :   20 (   0 equality)
%            Maximal clause size   :    3 (   2 average)
%            Number of predicates  :    1 (   0 propositional; 1-1 arity)
%            Number of functors    :    7 (   4 constant; 0-2 arity)
%            Number of variables   :   10 (   0 singleton)
%            Maximal term depth    :    4 (   2 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments :
%--------------------------------------------------------------------------
%----Include axioms for truthtellers and liars
include('Axioms/PUZ002-0.ax').
%--------------------------------------------------------------------------
cnf(a_mumbles,hypothesis,
    ( a_truth(says(a,mumble)) )).

cnf(b_says_a_says_hes_a_liar,hypothesis,
    ( a_truth(says(b,says(a,liar(a)))) )).

cnf(c_says_b_is_a_liar,hypothesis,
    ( a_truth(says(c,liar(b))) )).

cnf(prove_c_is_a_truthteller,negated_conjecture,
    ( ~ a_truth(truthteller(c)) )).

%--------------------------------------------------------------------------
