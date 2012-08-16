%--------------------------------------------------------------------------
% File     : PUZ034-1.004 : TPTP v5.2.0. Released v1.0.0.
% Domain   : Puzzles
% Problem  : N queens problem
% Version  : [SETHEO] axioms : Biased.
% English  : The problem is to place 4 queens on an 4x4 chess board,
%            so that no queen can attack another.

% Refs     :
% Source   : [SETHEO]
% Names    : q1-2.lop (Size 8) [SETHEO]
%          : q1-9.lop (Size 9) [SETHEO]
%          : q1-10.lop (Size 10) [SETHEO]

% Status   : Unsatisfiable
% Rating   : 0.80 v5.2.0, 0.70 v5.1.0, 0.64 v5.0.0, 0.79 v4.1.0, 0.62 v4.0.1, 0.60 v4.0.0, 0.71 v3.5.0, 0.86 v3.4.0, 0.75 v3.3.0, 0.67 v3.2.0, 0.33 v3.1.0, 0.67 v2.7.0, 0.75 v2.6.0, 0.67 v2.5.0, 0.40 v2.4.0, 0.60 v2.3.0, 0.67 v2.2.1, 1.00 v2.1.0
% Syntax   : Number of clauses     :   18 (   2 non-Horn;   9 unit;  12 RR)
%            Number of atoms       :   39 (   0 equality)
%            Maximal clause size   :    7 (   2 average)
%            Number of predicates  :    9 (   0 propositional; 2-3 arity)
%            Number of functors    :    4 (   2 constant; 0-2 arity)
%            Number of variables   :   45 (   6 singleton)
%            Maximal term depth    :    5 (   1 average)
% SPC      : CNF_UNS_RFO_NEQ_NHN

% Comments : Biased towards SETHEO.
%          : tptp2X: -f tptp -s4 PUZ034-1.g
%--------------------------------------------------------------------------
cnf(make_list_of_numbers1,axiom,
    ( range(Low,High,cons(Low,RestOfNumbers))
    | ~ less(Low,High)
    | ~ sum(Low,s(n0),NewLow)
    | ~ range(NewLow,High,RestOfNumbers) )).

cnf(make_list_of_numbers2,axiom,
    ( range(Same,Same,cons(Same,empty_list)) )).

cnf(less1,axiom,
    ( less(n0,s(X)) )).

cnf(less2,axiom,
    ( less(s(X),s(Y))
    | ~ less(X,Y) )).

cnf(add_0,axiom,
    ( sum(X,n0,X) )).

cnf(add,axiom,
    ( ~ sum(X,Y,Z)
    | sum(X,s(Y),s(Z)) )).

cnf(select1,axiom,
    ( select(Head,cons(Head,Tail),Tail) )).

cnf(select2,axiom,
    ( select(Element,cons(Head,Tail),cons(Head,NewTail))
    | ~ select(Element,Tail,NewTail) )).

cnf(same_definition1,axiom,
    ( ~ same(s(X),n0) )).

cnf(same_definition2,axiom,
    ( ~ same(n0,s(X)) )).

cnf(same_definition3,axiom,
    ( ~ same(s(X),s(Y))
    | same(X,Y) )).

cnf(attack,axiom,
    ( diagonal_attack(Queen,s(n0),PlacedQueens)
    | ~ attack(Queen,PlacedQueens) )).

cnf(check_diagonals1,axiom,
    ( ~ diagonal_attack(Queen,QueenNumber,cons(PlacedQueen,OtherPlacedQueens))
    | ~ sum(Diagonal1,QueenNumber,PlacedQueen)
    | same(Diagonal1,Queen)
    | ~ sum(PlacedQueen,QueenNumber,Diagonal2)
    | same(Diagonal2,Queen)
    | ~ sum(QueenNumber,s(n0),NextQueenNumber)
    | diagonal_attack(Queen,NextQueenNumber,OtherPlacedQueens) )).

cnf(check_diagonals2,axiom,
    ( ~ diagonal_attack(Queen,LastQueen,empty_list) )).

cnf(place_a_queen1,axiom,
    ( do_queens(UnplacedQueens,SafeQueens,Placement)
    | ~ select(AQueen,UnplacedQueens,RestOfUnplacedQueens)
    | attack(AQueen,SafeQueens)
    | ~ do_queens(RestOfUnplacedQueens,cons(AQueen,SafeQueens),Placement) )).

cnf(place_a_queen2,axiom,
    ( do_queens(empty_list,Placement,Placement) )).

cnf(set_up_queens,axiom,
    ( queens(NumberOfQueens,Placement)
    | ~ sum(NumberOfQueens,s(n0),Low)
    | ~ sum(NumberOfQueens,NumberOfQueens,High)
    | ~ range(Low,High,Positions)
    | ~ do_queens(Positions,empty_list,Placement) )).

cnf(place_queens,negated_conjecture,
    ( ~ queens(s(s(s(s(n0)))),Placement) )).

%--------------------------------------------------------------------------
