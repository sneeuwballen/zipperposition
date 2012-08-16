%------------------------------------------------------------------------------
% File     : PUZ134_1 : TPTP v5.2.0. Released v5.1.0.
% Domain   : Puzzles
% Problem  : The Knowheyan Job Puzzle - Jobs
% Version  : Especial.
% English  : Five Knowheyans, A, B, C, D, and E, work in Metropolisas Airfoil
%            Technician, Communications Consultant, Space Planner, Lunar Energy
%            Engineer, and Synthetic Food Nutrionist. No two of them are the
%            same age. The Knowheyan interpreter explains about the jobs and
%            ages of these five inhabitants, as follows:
%            1. The Communications Consultant is not younger than any of the
%               other four.
%            2. D is not as old as A and not as young as B, who is not as old
%               as the Lunar Energy Engineer, but not as young as C.
%            3. The Airfoil Technician is not younger than the Space Planner,
%               who is not younger than the Synthetic Food Nutrionist.
%            4. C is not the yougest of the five.
%            What is the job of each of the five Knowheyans?

% Refs     : [WS+06] Willis et al. (2006), The World's Biggest Book of Brai
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.00 v5.2.0, 0.67 v5.1.0
% Syntax   : Number of formulae    :   52 (  37 unit;  16 type)
%            Number of atoms       :   81 (  46 equality)
%            Maximal formula depth :    6 (   3 average)
%            Number of connectives :   52 (  27   ~;   9   |;   8   &)
%                                         (   1 <=>;   7  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    4 (   3   >;   1   *;   0   +;   0  <<)
%            Number of predicates  :   20 (  18 propositional; 0-2 arity)
%            Number of functors    :   12 (  10 constant; 0-1 arity)
%            Number of variables   :   21 (   0 sgn;  19   !;   2   ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : TFF_THM_EQU_NAR

% Comments :
%------------------------------------------------------------------------------
tff(knowheyan_type,type,(
    knowheyan: $tType )).

tff(job_type,type,(
    job: $tType )).

tff(age_type,type,(
    age: $tType )).

tff(age_of_type,type,(
    age_of: knowheyan > age )).

tff(job_of_type,type,(
    job_of: knowheyan > job )).

tff(greater_type,type,(
    greater: ( age * age ) > $o )).

tff(a_knowheyan,type,(
    a: knowheyan )).

tff(b_knowheyan,type,(
    b: knowheyan )).

tff(c_knowheyan,type,(
    c: knowheyan )).

tff(d_knowheyan,type,(
    d: knowheyan )).

tff(e_knowheyan,type,(
    e: knowheyan )).

tff(a_not_b,axiom,(
    a != b )).

tff(a_not_c,axiom,(
    a != c )).

tff(a_not_d,axiom,(
    a != d )).

tff(a_not_e,axiom,(
    a != e )).

tff(b_not_c,axiom,(
    b != c )).

tff(b_not_d,axiom,(
    b != d )).

tff(b_not_e,axiom,(
    b != e )).

tff(c_not_d,axiom,(
    c != d )).

tff(c_not_e,axiom,(
    c != e )).

tff(d_not_e,axiom,(
    d != e )).

tff(airfoil_technician_job,type,(
    airfoil_technician: job )).

tff(communications_consultant_job,type,(
    communications_consultant: job )).

tff(space_planner_job,type,(
    space_planner: job )).

tff(lunar_energy_engineer_job,type,(
    lunar_energy_engineer: job )).

tff(synthetic_food_nutitionist_job,type,(
    synthetic_food_nutitionist: job )).

tff(airfoil_technician_not_communications_consultant,axiom,(
    airfoil_technician != communications_consultant )).

tff(airfoil_technician_not_space_planner,axiom,(
    airfoil_technician != space_planner )).

tff(airfoil_technician_not_lunar_energy_engineer,axiom,(
    airfoil_technician != lunar_energy_engineer )).

tff(airfoil_technician_not_synthetic_food_nutitionist,axiom,(
    airfoil_technician != synthetic_food_nutitionist )).

tff(communications_consultant_not_space_planner,axiom,(
    communications_consultant != space_planner )).

tff(communications_consultant_not_lunar_energy_engineer,axiom,(
    communications_consultant != lunar_energy_engineer )).

tff(communications_consultant_not_synthetic_food_nutitionist,axiom,(
    communications_consultant != synthetic_food_nutitionist )).

tff(space_planner_not_lunar_energy_engineer,axiom,(
    space_planner != lunar_energy_engineer )).

tff(space_planner_not_synthetic_food_nutitionist,axiom,(
    space_planner != synthetic_food_nutitionist )).

tff(lunar_energy_engineer_not_synthetic_food_nutitionist,axiom,(
    lunar_energy_engineer != synthetic_food_nutitionist )).

tff(only_knowheyans,axiom,(
    ! [X: knowheyan] :
      ( X = a
      | X = b
      | X = c
      | X = d
      | X = e ) )).

tff(only_jobs,axiom,(
    ! [X: job] :
      ( X = airfoil_technician
      | X = communications_consultant
      | X = space_planner
      | X = lunar_energy_engineer
      | X = synthetic_food_nutitionist ) )).

tff(unique_ages,axiom,(
    ! [X: knowheyan,Y: knowheyan] :
      ( X != Y
     => age_of(X) != age_of(Y) ) )).

tff(unique_jobs,axiom,(
    ! [X: knowheyan,Y: knowheyan] :
      ( X != Y
     => job_of(X) != job_of(Y) ) )).

tff(age_transitive,axiom,(
    ! [X: age,Y: age,Z: age] :
      ( ( greater(X,Y)
        & greater(Y,Z) )
     => greater(X,Z) ) )).

tff(age_irreflexive,axiom,(
    ! [X: age,Y: age] :
      ~ ( greater(X,Y)
        & greater(Y,X) ) )).

tff(age_greater,axiom,(
    ! [X: age,Y: age] :
      ( X != Y
    <=> ( greater(X,Y)
        | greater(Y,X) ) ) )).

tff(communications_consultant_not_younger,axiom,(
    ! [X: knowheyan] :
      ( job_of(X) = communications_consultant
     => ~ ? [Y: knowheyan] : greater(age_of(Y),age_of(X)) ) )).

tff(age_info1,axiom,(
    greater(age_of(a),age_of(d)) )).

tff(age_info2,axiom,(
    greater(age_of(d),age_of(b)) )).

tff(age_info3,axiom,(
    ! [X: knowheyan] :
      ( job_of(X) = lunar_energy_engineer
     => greater(age_of(X),age_of(b)) ) )).

tff(age_info4,axiom,(
    greater(age_of(b),age_of(c)) )).

tff(age_job1,axiom,(
    ! [X: knowheyan,Y: knowheyan] :
      ( ( job_of(X) = airfoil_technician
        & job_of(Y) = space_planner )
     => greater(age_of(X),age_of(Y)) ) )).

tff(age_job2,axiom,(
    ! [X: knowheyan,Y: knowheyan] :
      ( ( job_of(X) = space_planner
        & job_of(Y) = synthetic_food_nutitionist )
     => greater(age_of(X),age_of(Y)) ) )).

tff(c_not_youngest,axiom,(
    ? [X: knowheyan] : greater(age_of(c),age_of(X)) )).

tff(known_jobs,conjecture,
    ( job_of(a) = communications_consultant
    & job_of(d) = lunar_energy_engineer
    & job_of(b) = airfoil_technician
    & job_of(c) = space_planner
    & job_of(e) = synthetic_food_nutitionist )).

%------------------------------------------------------------------------------
