%------------------------------------------------------------------------------
% File     : PUZ131+1 : TPTP v5.2.0. Released v4.1.0.
% Domain   : Puzzles
% Problem  : Victor teaches Michael
% Version  : Especial.
% English  : Every student is enrolled in at least one course. Every professor
%            teaches at least one course. Every course has at least one student
%            enrolled. Every course has at least one professor teaching. The
%            coordinator of a course teaches the course. If a student is
%            enroled in a course then the student is taught by every professor
%            who teaches the course. Michael is enrolled in CSC410. Victor is
%            the coordinator of CSC410. Therefore, Michael is taught by Victor.

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.11 v5.2.0, 0.05 v5.0.0, 0.04 v4.1.0
% Syntax   : Number of formulae    :   16 (   9 unit)
%            Number of atoms       :   31 (   1 equality)
%            Maximal formula depth :    8 (   3 average)
%            Number of connectives :   15 (   0   ~;   0   |;   5   &)
%                                         (   0 <=>;  10  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    7 (   0 propositional; 1-2 arity)
%            Number of functors    :    4 (   3 constant; 0-1 arity)
%            Number of variables   :   16 (   0 sgn;   9   !;   7   ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments :
%------------------------------------------------------------------------------
fof(student_type,axiom,(
    ? [A] : student(A) )).

fof(professor_type,axiom,(
    ? [A] : professor(A) )).

fof(course_type,axiom,(
    ? [A] : course(A) )).

fof(michael_type,axiom,(
    student(michael) )).

fof(victor_type,axiom,(
    professor(victor) )).

fof(csc410_type,axiom,(
    course(csc410) )).

fof(coordinator_of_type,axiom,(
    ! [A] :
      ( course(A)
     => professor(coordinatorof(A)) ) )).

fof(student_enrolled_axiom,axiom,(
    ! [X] :
      ( student(X)
     => ? [Y] :
          ( course(Y)
          & enrolled(X,Y) ) ) )).

fof(professor_teaches,axiom,(
    ! [X] :
      ( professor(X)
     => ? [Y] :
          ( course(Y)
          & teaches(X,Y) ) ) )).

fof(course_enrolled,axiom,(
    ! [X] :
      ( course(X)
     => ? [Y] :
          ( student(Y)
          & enrolled(Y,X) ) ) )).

fof(course_teaches,axiom,(
    ! [X] :
      ( course(X)
     => ? [Y] :
          ( professor(Y)
          & teaches(Y,X) ) ) )).

fof(coordinator_teaches,axiom,(
    ! [X] :
      ( course(X)
     => teaches(coordinatorof(X),X) ) )).

fof(student_enrolled_taught,axiom,(
    ! [X,Y] :
      ( ( student(X)
        & course(Y) )
     => ( enrolled(X,Y)
       => ! [Z] :
            ( professor(Z)
           => ( teaches(Z,Y)
             => taughtby(X,Z) ) ) ) ) )).

fof(michael_enrolled_csc410_axiom,axiom,(
    enrolled(michael,csc410) )).

fof(victor_coordinator_csc410_axiom,axiom,(
    coordinatorof(csc410) = victor )).

fof(teaching_conjecture,conjecture,(
    taughtby(michael,victor) )).

%------------------------------------------------------------------------------
