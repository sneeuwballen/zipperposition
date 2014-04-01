% calculus of explicit substitutions

% we use "at" as a connective for application.
% bound variables are unary De Bruijn indices, "db1" and "lift".

% explicit substitutions use the symbols "dot", "subst", "id" and "compose".
% "dot" is used to build environments, as lists accessed by DB index.
% - a[s] is subst(a,s)
% - a . b is dot(a,b). It is a list that maps db1 to a, and db2...n to b
% - s o t is compose(s,t)

at (lambda A) B --> subst A (dot B id).  % beta-reduction
lambda (at (subst A lift) db1) --> A.   % eta-reduction

% sigma reduction, ie computation of substitutions:

subst (at A B) S --> at (subst A S) (subst B S).  % map subst over At
subst db1 (dot A S) --> A.  % replace var
subst A id --> A.  % id subst
subst (lambda A) S --> lambda (subst A (dot db1 (compose S lift))).  % lift

subst (subst A S) T --> subst A (compose S T).  % (a[s])[t] -> a[s o t]

compose S id --> S.
compose id S --> S.  % id is neutral element of composition
compose lift (dot A S) --> S.  % lift all variables, so first item of list is unreachable
compose (compose A B) C --> compose A (compose B C).  % assoc
compose (dot A S) T --> dot (subst A T) (compose S T).  % compose maps over lists

dot db1 lift --> id.
dot (subst db1 S) (compose lift S) --> S.

% vim:syntax=prolog

