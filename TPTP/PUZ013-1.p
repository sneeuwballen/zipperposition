%--------------------------------------------------------------------------
% File     : PUZ013-1 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : The School Boys : Prove some monitors are awake
% Version  : Especial.
% English  : "All the boys, in a certain school, sit together in one large
%            room every evening. They are of no less than five
%            nationalities - English, Scotch, Welsh, Irish, and German.
%            One of the Monitors (who is a great reader of Wilkie Collins'
%            novels) is very observant and takes MS. notes of almost
%            everything that happens, with the view of being a good
%            sensational witness, in case any conspiracy to commit
%            a murder should be afoot. The following are some of his
%            notes :
%            (1) Whenever some of the English boys are singing "Rule,
%            Britannia," and some not, some of the Monitors are wide awake.
%            (2) Whenever some of the Scotch are dancing reels, and some of
%            the Irish fighting, some of the Welsh are eating toasted cheese.
%            (3) Whenever all the Germans are playing chess, some of the
%            Eleven are not oiling their bats.
%            (4) Whenever some of the Monitors are asleep, and some not,
%            some of the Irish are fighting.
%            (5) Whenever some of the Germans are playing chess, and none
%            of the Scotch are dancing reels, some of the Welsh are not
%            eating toasted cheese.
%            (6) Whenever some of the Scotch are not dancing reels, and
%            some of the Irish are not fighting, some of the Germans are
%            playing chess.
%            (7) Whenever some of the Monitors are awake, and some of the
%            Welsh are eating toasted cheese, none of the Scotch are
%            dancing reels.
%            (8) Whenever some of the Germans are not playing chess, and
%            some of the Welsh are not eating toasted cheese, none of the
%            Irish are fighting.
%            (9) Whenever all of the English are singing "Rule, Britannia,"
%            and some of the Scotch are not dancing reels, none of the
%            Germans are playing chess.
%            (10) Whenever some of the English are singing "Rule, Britannia",
%            and some of the Monitors are asleep, some of the Irish are not
%            fighting.
%            (11) Whenever some of the Monitors are awake, and some of the
%            Eleven are not oiling their bats, some of the Scotch are
%            dancing reels.
%            (12) Whenever some of the English are singing "Rule,
%            Britannia," and some of the Scotch are not dancing reels,
%            Here the MS. breaks off suddenly. The problem is to complete
%            the sentence, if possible.

% Refs     : [Car86] Carroll (1986), Lewis Carroll's Symbolic Logic
% Source   : [ANL]
% Names    : boys.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v2.0.0
% Syntax   : Number of clauses     :   20 (   8 non-Horn;   3 unit;  20 RR)
%            Number of atoms       :   49 (   0 equality)
%            Maximal clause size   :    4 (   2 average)
%            Number of predicates  :   13 (  13 propositional; 0-0 arity)
%            Number of functors    :    0 (   0 constant; --- arity)
%            Number of variables   :    0 (   0 singleton)
%            Maximal term depth    :    0 (   0 average)
% SPC      : CNF_UNS_PRP

% Comments :
%--------------------------------------------------------------------------
cnf(c1,axiom,
    ( ~ some_english_sing
    | ~ some_english_sing_not
    | some_monitors_are_awake )).

cnf(c2,axiom,
    ( ~ some_scotch_dance
    | ~ some_irish_fight
    | some_welsh_eat )).

cnf(c3,axiom,
    ( ~ some_germans_play
    | some_germans_play_not
    | some_of_the_eleven_are_not_oiling )).

cnf(c4,axiom,
    ( ~ some_monitors_are_awake
    | ~ some_monitors_are_not_awake
    | some_irish_fight )).

cnf(c5,axiom,
    ( ~ some_germans_play
    | some_scotch_dance
    | some_welsh_eat_not )).

cnf(c6,axiom,
    ( ~ some_scotch_dance_not
    | ~ some_irish_fight_not
    | some_germans_play )).

cnf(c7,axiom,
    ( ~ some_monitors_are_awake
    | ~ some_welsh_eat
    | ~ some_scotch_dance )).

cnf(c8,axiom,
    ( ~ some_germans_play_not
    | ~ some_welsh_eat_not
    | ~ some_irish_fight )).

cnf(c9,axiom,
    ( ~ some_english_sing
    | some_english_sing_not
    | ~ some_scotch_dance_not
    | ~ some_germans_play )).

cnf(c10,axiom,
    ( ~ some_english_sing
    | ~ some_monitors_are_not_awake
    | some_irish_fight_not )).

cnf(c11,axiom,
    ( ~ some_monitors_are_awake
    | ~ some_of_the_eleven_are_not_oiling
    | some_scotch_dance )).

cnf(c12,axiom,
    ( some_english_sing_not
    | some_english_sing )).

cnf(c13,axiom,
    ( some_monitors_are_not_awake
    | some_monitors_are_awake )).

cnf(c14,axiom,
    ( some_scotch_dance
    | some_scotch_dance_not )).

cnf(c15,axiom,
    ( some_irish_fight
    | some_irish_fight_not )).

cnf(c16,axiom,
    ( some_welsh_eat
    | some_welsh_eat_not )).

cnf(c17,axiom,
    ( some_germans_play
    | some_germans_play_not )).

cnf(c18,axiom,
    ( some_english_sing )).

cnf(c19,axiom,
    ( some_scotch_dance_not )).

cnf(prove_some_monitors_are_awake,negated_conjecture,
    ( ~ some_monitors_are_awake )).

%--------------------------------------------------------------------------
