% `--enum-bool-funs` must re-enumerate the Boolean functions per file.
% The enumerated symbols and their defining clauses live in one env, so a
% cache hit in a second file leaves the axiom instances unconstrained.
thf(ax, axiom, ! [F: $o > $o] : ((F @ $true) = $false)).
thf(goal, conjecture, $false).
