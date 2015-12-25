% inspired from Raphaël's dedukti code

val true : bool.
val ite : /\ A bool -> A -> A -> A.

and true false --> false.
and false true --> false.
and true true --> true.
and false false --> false.

or false true --> true.
or true false --> true.
or false false --> false.
or true true --> true.

not true --> false.
not false --> true.

imply A B --> or (not A) B.

equiv true true --> true.
equiv false false --> true.
equiv true false --> false.
equiv false true --> false.
% equiv(A, B) --> and(imply(A,B),imply().    % kills KBO

xor A B --> not (equiv A B).

ite _ true A B --> A.
ite _ false A B --> B.

% vim:syntax=prolog
