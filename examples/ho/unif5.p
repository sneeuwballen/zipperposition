
% expect: UNSAT

% type declarations
% constant type declarations
thf(1, type, g: ($i > $i) > $i ).
thf(2, type, f: $i > $i ).
thf(3, type, p: $i > $o ).
% sequents

% formula: ∃x (p(x(g(f:i>i))) ⊃ p(f(g(x))))
thf(4, conjecture, (?[X : $i > $i] : (((p @ (X @ (g @ f))) => (p @ (f @ (g @ X)))))) ).
