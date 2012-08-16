%--------------------------------------------------------------------------
% File     : PUZ019-1 : TPTP v5.2.0. Bugfixed v5.1.0.
% Domain   : Puzzles
% Problem  : The Jobs Puzzles
% Version  : Especial.
% English  : There are four people: Roberta, Thelma, Steve, and Pete. Among
%            them they hold eight different jobs. Each holds exactly two jobs.
%            The jobs are: chef, guard, nurse, telephone operator, police
%            officer (either gender), teacher, actor, and boxer. The job of
%            a nurse is held by a male. The husband of the chef is the
%            telephone operator. Roberta is not a boxer. Pete has no
%            education past the ninth grade. Roberta, the chef, and the
%            police officer went golfing together. Question : Who holds
%            which job?

% Refs     : [WO+92] Wos et al. (1992), Automated Reasoning: Introduction a
% Source   : [ANL]
% Names    : jobs.ver1.in [ANL]

% Status   : Unsatisfiable
% Rating   : 0.00 v5.1.0
% Syntax   : Number of clauses     :   64 (   4 non-Horn;  44 unit;  59 RR)
%            Number of atoms       :  105 (   0 equality)
%            Maximal clause size   :    8 (   2 average)
%            Number of predicates  :    7 (   0 propositional; 1-2 arity)
%            Number of functors    :   12 (  12 constant; 0-0 arity)
%            Number of variables   :   40 (  10 singleton)
%            Maximal term depth    :    1 (   1 average)
% SPC      : CNF_UNS_EPR

% Comments :
% Bugfixes : v1.1.3 - Changed peteacher to pete in clauses roberta_not_pete
%            and pete_is_male.
%          : v5.1.0 - Adding missing axiom thelma_not_steve
%--------------------------------------------------------------------------
cnf(reflexivity_for_equal_people,axiom,
    ( equal_people(X,X) )).

cnf(reflexivity_for_equal_jobs,axiom,
    ( equal_jobs(X,X) )).

cnf(symmetry_of_equal_people,axiom,
    ( ~ equal_people(X,Y)
    | equal_people(Y,X) )).

cnf(symmetry_of_equal_jobs,axiom,
    ( ~ equal_jobs(X,Y)
    | equal_jobs(Y,X) )).

cnf(roberta_not_thelma,axiom,
    ( ~ equal_people(roberta,thelma) )).

cnf(roberta_not_pete,axiom,
    ( ~ equal_people(roberta,pete) )).

cnf(roberta_not_steve,axiom,
    ( ~ equal_people(roberta,steve) )).

cnf(pete_not_thelma,axiom,
    ( ~ equal_people(pete,thelma) )).

cnf(pete_not_steve,axiom,
    ( ~ equal_people(pete,steve) )).

cnf(thelma_not_steve,axiom,
    ( ~ equal_people(thelma,steve) )).

cnf(chef_not_guard,axiom,
    ( ~ equal_jobs(chef,guard) )).

cnf(chef_not_nurse,axiom,
    ( ~ equal_jobs(chef,nurse) )).

cnf(chef_not_operator,axiom,
    ( ~ equal_jobs(chef,operator) )).

cnf(chef_not_police,axiom,
    ( ~ equal_jobs(chef,police) )).

cnf(chef_not_actor,axiom,
    ( ~ equal_jobs(chef,actor) )).

cnf(chef_not_boxer,axiom,
    ( ~ equal_jobs(chef,boxer) )).

cnf(chef_not_teacher,axiom,
    ( ~ equal_jobs(chef,teacher) )).

cnf(guard_not_nurse,axiom,
    ( ~ equal_jobs(guard,nurse) )).

cnf(guard_not_operator,axiom,
    ( ~ equal_jobs(guard,operator) )).

cnf(guard_not_police,axiom,
    ( ~ equal_jobs(guard,police) )).

cnf(guard_not_actor,axiom,
    ( ~ equal_jobs(guard,actor) )).

cnf(guard_not_boxer,axiom,
    ( ~ equal_jobs(guard,boxer) )).

cnf(guard_not_teacher,axiom,
    ( ~ equal_jobs(guard,teacher) )).

cnf(nurse_not_operator,axiom,
    ( ~ equal_jobs(nurse,operator) )).

cnf(nurse_not_police,axiom,
    ( ~ equal_jobs(nurse,police) )).

cnf(nurse_not_actor,axiom,
    ( ~ equal_jobs(nurse,actor) )).

cnf(nurse_not_boxer,axiom,
    ( ~ equal_jobs(nurse,boxer) )).

cnf(nurse_not_teacher,axiom,
    ( ~ equal_jobs(nurse,teacher) )).

cnf(operator_not_police,axiom,
    ( ~ equal_jobs(operator,police) )).

cnf(operator_not_actor,axiom,
    ( ~ equal_jobs(operator,actor) )).

cnf(operator_not_boxer,axiom,
    ( ~ equal_jobs(operator,boxer) )).

cnf(operator_not_teacher,axiom,
    ( ~ equal_jobs(operator,teacher) )).

cnf(police_not_actor,axiom,
    ( ~ equal_jobs(police,actor) )).

cnf(police_not_boxer,axiom,
    ( ~ equal_jobs(police,boxer) )).

cnf(police_not_teacher,axiom,
    ( ~ equal_jobs(police,teacher) )).

cnf(actor_not_boxer,axiom,
    ( ~ equal_jobs(actor,boxer) )).

cnf(actor_not_teacher,axiom,
    ( ~ equal_jobs(actor,teacher) )).

cnf(boxer_not_teacher,axiom,
    ( ~ equal_jobs(boxer,teacher) )).

cnf(nurse_is_male,axiom,
    ( ~ has_job(X,nurse)
    | male(X) )).

cnf(actor_is_male,axiom,
    ( ~ has_job(X,actor)
    | male(X) )).

cnf(chef_is_female,axiom,
    ( ~ has_job(X,chef)
    | female(X) )).

cnf(nurse_is_educated,axiom,
    ( ~ has_job(X,nurse)
    | educated(X) )).

cnf(teacher_is_educated,axiom,
    ( ~ has_job(X,teacher)
    | educated(X) )).

cnf(police_is_educated,axiom,
    ( ~ has_job(X,police)
    | educated(X) )).

cnf(chef_is_not_also_police,axiom,
    ( ~ has_job(X,chef)
    | ~ has_job(X,police) )).

cnf(males_are_not_female,axiom,
    ( ~ male(X)
    | ~ female(X) )).

cnf(everyone_male_or_female,axiom,
    ( male(X)
    | female(X) )).

cnf(husband_is_male,axiom,
    ( ~ husband(X,Y)
    | male(Y) )).

cnf(wife_is_female,axiom,
    ( ~ husband(X,Y)
    | female(X) )).

cnf(husband_of_chef_is_operator1,hypothesis,
    ( ~ has_job(X,chef)
    | ~ has_job(Y,operator)
    | husband(X,Y) )).

cnf(husband_of_chef_is_operator2,hypothesis,
    ( ~ has_job(X,chef)
    | has_job(Y,operator)
    | ~ husband(X,Y) )).

cnf(each_job_held_once,hypothesis,
    ( ~ has_job(X,Z)
    | ~ has_job(Y,Z)
    | equal_people(X,Y) )).

cnf(each_has_maximum_of_two_jobs,hypothesis,
    ( ~ has_job(Z,U)
    | ~ has_job(Z,X)
    | ~ has_job(Z,Y)
    | equal_jobs(U,X)
    | equal_jobs(U,Y)
    | equal_jobs(X,Y) )).

cnf(every_job_is_used,hypothesis,
    ( has_job(roberta,X)
    | has_job(thelma,X)
    | has_job(pete,X)
    | has_job(steve,X) )).

cnf(everyone_works,hypothesis,
    ( has_job(X,chef)
    | has_job(X,guard)
    | has_job(X,nurse)
    | has_job(X,operator)
    | has_job(X,police)
    | has_job(X,teacher)
    | has_job(X,actor)
    | has_job(X,boxer) )).

cnf(pete_is_not_educated,hypothesis,
    ( ~ educated(pete) )).

cnf(roberta_is_not_chef,hypothesis,
    ( ~ has_job(roberta,chef) )).

cnf(roberta_is_not_boxer,hypothesis,
    ( ~ has_job(roberta,boxer) )).

cnf(roberta_is_not_police,hypothesis,
    ( ~ has_job(roberta,police) )).

cnf(steve_is_male,hypothesis,
    ( male(steve) )).

cnf(pete_is_male,hypothesis,
    ( male(pete) )).

cnf(roberta_is_female,hypothesis,
    ( female(roberta) )).

cnf(thelma_is_female,hypothesis,
    ( female(thelma) )).

cnf(find_who_has_each_job,negated_conjecture,
    ( ~ has_job(X1,chef)
    | ~ has_job(X2,guard)
    | ~ has_job(X3,nurse)
    | ~ has_job(X4,operator)
    | ~ has_job(X5,police)
    | ~ has_job(X6,teacher)
    | ~ has_job(X7,actor)
    | ~ has_job(X8,boxer) )).

%--------------------------------------------------------------------------
