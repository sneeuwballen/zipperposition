%--------------------------------------------------------------------------
% File     : PUZ004-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : The Letters Puzzle
% Version  : Especial.
% English  : (1) All the dated letters in this room are written on blue paper.
%            (2) None of them are in black ink except those that are written
%                in the third person.
%            (3) I have not filed any of them that I can read.
%            (4) None of them that are written on one sheet are undated.
%            (5) All of them that are not crossed are in black ink.  :
%            (6) All of them written by Brown begin with "Dear Sir" :
%            (7) All of them written on blue paper are filed.        :
%            (8) None of them written on more than one sheet are crossed.
%            (9) None of them that begin with "Dear Sir" are written
%                in third person.
%            Prove that letters by Brown cannot be read.

% Refs     : [Car86] Carroll (1986), Lewis Carroll's Symbolic Logic
% Source   : [ANL]
% Names    : letters.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   12 (   1 non-Horn;   2 unit;  12 RR)
%            Number of atoms       :   22 (   0 equality)
%            Maximal clause size   :    2 (   2 average)
%            Number of predicates  :   10 (  10 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton)
%            Maximal term depth    :    0 (   0 average)
% SPC      : CNF_UNS_PRP

% Comments :
%--------------------------------------------------------------------------
cnf(dated_on_blue_paper,axiom,
    ( ~ dated
    | on_blue_paper )).

cnf(third_person_in_black_ink,axiom,
    ( ~ in_third_person
    | in_black_ink )).

cnf(black_ink_in_third_person,axiom,
    ( in_third_person
    | ~ in_black_ink )).

cnf(not_filed_if_read,axiom,
    ( ~ can_be_read
    | ~ filed )).

cnf(dated_if_on_one_sheet,axiom,
    ( ~ on_one_sheet
    | dated )).

cnf(not_crossed_in_black_ink,axiom,
    ( crossed
    | in_black_ink )).

cnf(brown_starts_with_sir,axiom,
    ( ~ by_brown
    | begins_with_dear_sir )).

cnf(filed_if_on_blue_paper,axiom,
    ( ~ on_blue_paper
    | filed )).

cnf(on_one_sheet_if_crossed,axiom,
    ( on_one_sheet
    | ~ crossed )).

cnf(no_dear_sirs_in_third_person,axiom,
    ( ~ begins_with_dear_sir
    | ~ in_third_person )).

cnf(letter_by_brown,hypothesis,
    ( by_brown )).

cnf(prove_it_cannot_be_read,negated_conjecture,
    ( can_be_read )).

%--------------------------------------------------------------------------
