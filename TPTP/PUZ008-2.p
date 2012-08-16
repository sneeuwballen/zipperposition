%--------------------------------------------------------------------------
% File     : PUZ008-2 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : Missionaries and Cannibals
% Version  : Especial.
%            Theorem formulation : Procedural.
% English  : There are 3 missionaries, 3 cannibals, and a boat on the west
%            bank of a river. All wish to cross, but the boat holds
%            at most 2 people. If the cannibals ever outnumber the
%            missionaries on either bank of the river the outnumbered
%            missionaries will be eaten. Can they all safely cross the
%            river?  If so, how? (The boat cannot cross empty.)

% Refs     :
% Source   : [ANL]
% Names    : mission.ver2.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v3.3.0, 0.33 v3.2.0, 0.00 v3.1.0, 0.11 v2.7.0, 0.00 v2.0.0
% Syntax   : Number of clauses     :   32 (   0 non-Horn;   2 unit;  32 RR)
%            Number of atoms       :   62 (   0 equality)
%            Maximal clause size   :    2 (   2 average)
%            Number of predicates  :    1 (   0 propositional; 3-3 arity)
%            Number of functors    :   10 (   6 constant; 0-2 arity)
%            Number of variables   :    0 (   0 singleton)
%            Maximal term depth    :    3 (   2 average)
% SPC      : CNF_UNS_EPR

% Comments :
%--------------------------------------------------------------------------
%----Moving cannibals only, west to east
cnf(bccc_mmm_to_cc_bmmmc,axiom,
    ( ~ banks(west(m(n0),c(n3)),east(m(n3),c(n0)),boatonwest)
    | banks(west(m(n0),c(n2)),east(m(n3),c(n1)),boatoneast) )).

cnf(bmmmcc_c_to_mmmc_cc,axiom,
    ( ~ banks(west(m(n3),c(n2)),east(m(n0),c(n1)),boatonwest)
    | banks(west(m(n3),c(n1)),east(m(n0),c(n2)),boatoneast) )).

cnf(bcc_mmmc_to_c_mmmcc,axiom,
    ( ~ banks(west(m(n0),c(n2)),east(m(n3),c(n1)),boatonwest)
    | banks(west(m(n0),c(n1)),east(m(n3),c(n2)),boatoneast) )).

cnf(bmmmc_cc_to_mmm_ccc,axiom,
    ( ~ banks(west(m(n3),c(n1)),east(m(n0),c(n2)),boatonwest)
    | banks(west(m(n3),c(n0)),east(m(n0),c(n3)),boatoneast) )).

cnf(bc_mmmcc_to_x_bmmmccc,axiom,
    ( ~ banks(west(m(n0),c(n1)),east(m(n3),c(n2)),boatonwest)
    | banks(west(m(n0),c(n0)),east(m(n3),c(n3)),boatoneast) )).

cnf(bmmmccc_x_to_mmmc_bcc,axiom,
    ( ~ banks(west(m(n3),c(n3)),east(m(n0),c(n0)),boatonwest)
    | banks(west(m(n3),c(n1)),east(m(n0),c(n2)),boatoneast) )).

cnf(bccc_mmm_to_c_bmmmcc,axiom,
    ( ~ banks(west(m(n0),c(n3)),east(m(n3),c(n0)),boatonwest)
    | banks(west(m(n0),c(n1)),east(m(n3),c(n2)),boatoneast) )).

cnf(bmmmcc_c_to_mmm_bccc,axiom,
    ( ~ banks(west(m(n3),c(n2)),east(m(n0),c(n1)),boatonwest)
    | banks(west(m(n3),c(n0)),east(m(n0),c(n3)),boatoneast) )).

cnf(bcc_mmmc_to_x_bmmmccc,axiom,
    ( ~ banks(west(m(n0),c(n2)),east(m(n3),c(n1)),boatonwest)
    | banks(west(m(n0),c(n0)),east(m(n3),c(n3)),boatoneast) )).

%----Moving cannibals only, east to west
cnf(cc_bmmmc_to_bccc_mmm,axiom,
    ( ~ banks(west(m(n0),c(n2)),east(m(n3),c(n1)),boatoneast)
    | banks(west(m(n0),c(n3)),east(m(n3),c(n0)),boatonwest) )).

cnf(c_bmmmcc_to_bcc_mmmc,axiom,
    ( ~ banks(west(m(n0),c(n1)),east(m(n3),c(n2)),boatoneast)
    | banks(west(m(n0),c(n2)),east(m(n3),c(n1)),boatonwest) )).

cnf(mmm_bccc_to_bmmmc_cc,axiom,
    ( ~ banks(west(m(n3),c(n0)),east(m(n0),c(n3)),boatoneast)
    | banks(west(m(n3),c(n1)),east(m(n0),c(n2)),boatonwest) )).

cnf(mmmcc_bc_to_bmmmccc_x,axiom,
    ( ~ banks(west(m(n3),c(n2)),east(m(n0),c(n1)),boatoneast)
    | banks(west(m(n3),c(n3)),east(m(n0),c(n0)),boatonwest) )).

cnf(mmmc_bcc_to_bmmmcc_c,axiom,
    ( ~ banks(west(m(n3),c(n1)),east(m(n0),c(n2)),boatoneast)
    | banks(west(m(n3),c(n2)),east(m(n0),c(n1)),boatonwest) )).

cnf(c_bmmmcc_to_bccc_mmm,axiom,
    ( ~ banks(west(m(n0),c(n1)),east(m(n3),c(n2)),boatoneast)
    | banks(west(m(n0),c(n3)),east(m(n3),c(n0)),boatonwest) )).

cnf(mmmc_bcc_to_bmmmccc_x,axiom,
    ( ~ banks(west(m(n3),c(n1)),east(m(n0),c(n2)),boatoneast)
    | banks(west(m(n3),c(n3)),east(m(n0),c(n0)),boatonwest) )).

cnf(mmm_bccc_to_bmmmcc_c,axiom,
    ( ~ banks(west(m(n3),c(n0)),east(m(n0),c(n3)),boatoneast)
    | banks(west(m(n3),c(n2)),east(m(n0),c(n1)),boatonwest) )).

%----Moving missionaries only, west to east
cnf(bmmmcc_c_to_mmcc_bmc,axiom,
    ( ~ banks(west(m(n3),c(n2)),east(m(n0),c(n1)),boatonwest)
    | banks(west(m(n2),c(n2)),east(m(n1),c(n1)),boatoneast) )).

cnf(bmc_mmcc_to_c_bmmmcc,axiom,
    ( ~ banks(west(m(n1),c(n1)),east(m(n2),c(n2)),boatonwest)
    | banks(west(m(n0),c(n1)),east(m(n3),c(n2)),boatoneast) )).

cnf(bmmmc_cc_to_mc_bmmcc,axiom,
    ( ~ banks(west(m(n3),c(n1)),east(m(n0),c(n2)),boatonwest)
    | banks(west(m(n1),c(n1)),east(m(n2),c(n2)),boatoneast) )).

cnf(bmmcc_mc_to_cc_bmmmc,axiom,
    ( ~ banks(west(m(n2),c(n2)),east(m(n1),c(n1)),boatonwest)
    | banks(west(m(n0),c(n2)),east(m(n3),c(n1)),boatoneast) )).

%----Moving missionaries only, east to west
cnf(c_bmmmcc_to_bmc_mmcc,axiom,
    ( ~ banks(west(m(n0),c(n1)),east(m(n3),c(n2)),boatoneast)
    | banks(west(m(n1),c(n1)),east(m(n2),c(n2)),boatonwest) )).

cnf(mmcc_bmc_to_bmmmcc_c,axiom,
    ( ~ banks(west(m(n2),c(n2)),east(m(n1),c(n1)),boatoneast)
    | banks(west(m(n3),c(n2)),east(m(n0),c(n1)),boatonwest) )).

cnf(cc_bmmmc_to_bmmcc_mc,axiom,
    ( ~ banks(west(m(n0),c(n2)),east(m(n3),c(n1)),boatoneast)
    | banks(west(m(n2),c(n2)),east(m(n1),c(n1)),boatonwest) )).

cnf(mc_bmmcc_to_bmmmc_cc,axiom,
    ( ~ banks(west(m(n1),c(n1)),east(m(n2),c(n2)),boatoneast)
    | banks(west(m(n3),c(n1)),east(m(n0),c(n2)),boatonwest) )).

%----Moving a missionary and a cannibal, west to east
cnf(bmmmccc_x_to_mmcc_bmc,axiom,
    ( ~ banks(west(m(n3),c(n3)),east(m(n0),c(n0)),boatonwest)
    | banks(west(m(n2),c(n2)),east(m(n1),c(n1)),boatoneast) )).

cnf(bmmcc_mc_to_mc_bmmcc,axiom,
    ( ~ banks(west(m(n2),c(n2)),east(m(n1),c(n1)),boatonwest)
    | banks(west(m(n1),c(n1)),east(m(n2),c(n2)),boatoneast) )).

cnf(bmc_mmcc_to_x_bmmmccc,axiom,
    ( ~ banks(west(m(n1),c(n1)),east(m(n2),c(n2)),boatonwest)
    | banks(west(m(n0),c(n0)),east(m(n3),c(n3)),boatoneast) )).

%----Moving a missionary and a cannibal, east to west
cnf(mc_bmmcc_to_bmmcc_mc,axiom,
    ( ~ banks(west(m(n1),c(n1)),east(m(n2),c(n2)),boatoneast)
    | banks(west(m(n2),c(n2)),east(m(n1),c(n1)),boatonwest) )).

cnf(mmcc_bmc_to_bmmmccc_x,axiom,
    ( ~ banks(west(m(n2),c(n2)),east(m(n1),c(n1)),boatoneast)
    | banks(west(m(n3),c(n3)),east(m(n0),c(n0)),boatonwest) )).

%----The problem to be solved
cnf(starting_configuration,hypothesis,
    ( banks(west(m(n3),c(n3)),east(m(n0),c(n0)),boatonwest) )).

cnf(prove_can_get_across,negated_conjecture,
    ( ~ banks(west(m(n0),c(n0)),east(m(n3),c(n3)),boatoneast) )).

%--------------------------------------------------------------------------
