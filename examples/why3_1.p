tff(bool, type, bool: $tType).

tff(true, type, true: bool).

tff(false, type, false: bool).

tff(match_bool, type, match_bool: !>[A : $tType]: ((bool * A * A) > A)).

tff(match_bool_True, axiom, ![A : $tType]: ![Z:A, Z1:A]: (match_bool(A, true,
  Z, Z1) = Z)).

tff(match_bool_False, axiom, ![A : $tType]: ![Z:A, Z1:A]:
  (match_bool(A, false, Z, Z1) = Z1)).

tff(true_False, axiom, ~ (true = false)).

tff(bool_inversion, axiom, ![U:bool]: ((U = true) | (U = false))).

tff(tuple0, type, tuple0: $tType).

tff(tuple01, type, tuple01: tuple0).

tff(tuple0_inversion, axiom, ![U:tuple0]: (U = tuple01)).

tff(qtmark, type, qtmark: $tType).

tff(compatOrderMult, axiom, ![X:$int, Y:$int, Z:$int]: ($lesseq(X,Y)
  => ($lesseq(0,Z) => $lesseq($product(X,Z),$product(Y,Z))))).

tff(list, type, list: $tType > $tType).

tff(nil, type, nil: !>[A : $tType]: list(A)).

tff(cons, type, cons: !>[A : $tType]: ((A * list(A)) > list(A))).

tff(match_list, type, match_list: !>[A : $tType, A1 : $tType]: ((list(A) *
  A1 * A1) > A1)).

tff(match_list_Nil, axiom, ![A : $tType, A1 : $tType]: ![Z:A1, Z1:A1]:
  (match_list(A, A1, nil(A), Z, Z1) = Z)).

tff(match_list_Cons, axiom, ![A : $tType, A1 : $tType]: ![Z:A1, Z1:A1, U:A,
  U1:list(A)]: (match_list(A, A1, cons(A, U, U1), Z, Z1) = Z1)).

tff(nil_Cons, axiom, ![A : $tType]: ![V:A, V1:list(A)]: ~
  (nil(A) = cons(A, V, V1))).

tff(cons_proj_1, type, cons_proj_1: !>[A : $tType]: (list(A) > A)).

tff(cons_proj_1_def, axiom, ![A : $tType]: ![U:A, U1:list(A)]:
  (cons_proj_1(A, cons(A, U, U1)) = U)).

tff(cons_proj_2, type, cons_proj_2: !>[A : $tType]: (list(A) > list(A))).

tff(cons_proj_2_def, axiom, ![A : $tType]: ![U:A, U1:list(A)]:
  (cons_proj_2(A, cons(A, U, U1)) = U1)).

tff(list_inversion, axiom, ![A : $tType]: ![U:list(A)]: ((U = nil(A))
  | (U = cons(A, cons_proj_1(A, U), cons_proj_2(A, U))))).

tff(length, type, length: !>[A : $tType]: (list(A) > $int)).

tff(length_def, axiom, ![A : $tType]: ((length(A, nil(A)) = 0) & ![X:A, X1:
  list(A)]: (length(A, cons(A, X, X1)) = $sum(1,length(A, X1))))).

tff(length_nonnegative, axiom, ![A : $tType]: ![L:list(A)]:
  $lesseq(0,length(A, L))).

tff(length_nil, axiom, ![A : $tType]: ![L:list(A)]: ((length(A, L) = 0)
  <=> (L = nil(A)))).

tff(infix_plpl, type, infix_plpl: !>[A : $tType]: ((list(A) * list(A)) >
  list(A))).

tff(infix_plpl_def, axiom, ![A : $tType]: ![L2:list(A)]:
  ((infix_plpl(A, nil(A), L2) = L2) & ![X:A, X1:list(A)]:
  (infix_plpl(A, cons(A, X, X1), L2) = cons(A, X, infix_plpl(A, X1, L2))))).

tff(append_assoc, axiom, ![A : $tType]: ![L1:list(A), L2:list(A), L3:
  list(A)]: (infix_plpl(A, L1, infix_plpl(A, L2,
  L3)) = infix_plpl(A, infix_plpl(A, L1, L2), L3))).

tff(append_l_nil, axiom, ![A : $tType]: ![L:list(A)]: (infix_plpl(A, L,
  nil(A)) = L)).

tff(append_length, axiom, ![A : $tType]: ![L1:list(A), L2:list(A)]:
  (length(A, infix_plpl(A, L1, L2)) = $sum(length(A, L1),length(A, L2)))).

tff(mem, type, mem: !>[A : $tType]: ((A * list(A)) > $o)).

tff(mem_def, axiom, ![A : $tType]: ![X:A]: (~ mem(A, X, nil(A)) & ![X1:A, X2:
  list(A)]: (mem(A, X, cons(A, X1, X2)) <=> ((X = X1) | mem(A, X, X2))))).

tff(mem_append, axiom, ![A : $tType]: ![X:A, L1:list(A), L2:list(A)]:
  (mem(A, X, infix_plpl(A, L1, L2)) <=> (mem(A, X, L1) | mem(A, X, L2)))).

tff(mem_decomp, axiom, ![A : $tType]: ![X:A, L:list(A)]: (mem(A, X, L)
  => ?[L1:list(A), L2:list(A)]: (L = infix_plpl(A, L1, cons(A, X, L2))))).

tff(elt, type, elt: $tType).

tff(tree, type, tree: $tType).

tff(empty, type, empty: tree).

tff(node, type, node: (tree * elt * tree) > tree).

tff(match_tree, type, match_tree: !>[A : $tType]: ((tree * A * A) > A)).

tff(match_tree_Empty, axiom, ![A : $tType]: ![Z:A, Z1:A]:
  (match_tree(A, empty, Z, Z1) = Z)).

tff(match_tree_Node, axiom, ![A : $tType]: ![Z:A, Z1:A, U:tree, U1:elt, U2:
  tree]: (match_tree(A, node(U, U1, U2), Z, Z1) = Z1)).

tff(empty_Node, axiom, ![V:tree, V1:elt, V2:tree]: ~ (empty = node(V, V1,
  V2))).

tff(node_proj_1, type, node_proj_1: tree > tree).

tff(node_proj_1_def, axiom, ![U:tree, U1:elt, U2:tree]: (node_proj_1(node(U,
  U1, U2)) = U)).

tff(node_proj_2, type, node_proj_2: tree > elt).

tff(node_proj_2_def, axiom, ![U:tree, U1:elt, U2:tree]: (node_proj_2(node(U,
  U1, U2)) = U1)).

tff(node_proj_3, type, node_proj_3: tree > tree).

tff(node_proj_3_def, axiom, ![U:tree, U1:elt, U2:tree]: (node_proj_3(node(U,
  U1, U2)) = U2)).

tff(tree_inversion, axiom, ![U:tree]: ((U = empty)
  | (U = node(node_proj_1(U), node_proj_2(U), node_proj_3(U))))).

tff(elements, type, elements: tree > list(elt)).

tff(elements_def, axiom, ((elements(empty) = nil(elt)) & ![X:tree, X1:elt,
  X2:tree]: (elements(node(X, X1, X2)) = infix_plpl(elt, elements(X),
  cons(elt, X1, elements(X2)))))).

tff(enum, type, enum: $tType).

tff(done, type, done: enum).

tff(next, type, next: (elt * tree * enum) > enum).

tff(match_enum, type, match_enum: !>[A : $tType]: ((enum * A * A) > A)).

tff(match_enum_Done, axiom, ![A : $tType]: ![Z:A, Z1:A]: (match_enum(A, done,
  Z, Z1) = Z)).

tff(match_enum_Next, axiom, ![A : $tType]: ![Z:A, Z1:A, U:elt, U1:tree, U2:
  enum]: (match_enum(A, next(U, U1, U2), Z, Z1) = Z1)).

tff(done_Next, axiom, ![V:elt, V1:tree, V2:enum]: ~ (done = next(V, V1,
  V2))).

tff(next_proj_1, type, next_proj_1: enum > elt).

tff(next_proj_1_def, axiom, ![U:elt, U1:tree, U2:enum]: (next_proj_1(next(U,
  U1, U2)) = U)).

tff(next_proj_2, type, next_proj_2: enum > tree).

tff(next_proj_2_def, axiom, ![U:elt, U1:tree, U2:enum]: (next_proj_2(next(U,
  U1, U2)) = U1)).

tff(next_proj_3, type, next_proj_3: enum > enum).

tff(next_proj_3_def, axiom, ![U:elt, U1:tree, U2:enum]: (next_proj_3(next(U,
  U1, U2)) = U2)).

tff(enum_inversion, axiom, ![U:enum]: ((U = done) | (U = next(next_proj_1(U),
  next_proj_2(U), next_proj_3(U))))).

tff(enum_elements, type, enum_elements: enum > list(elt)).

tff(enum_elements_def, axiom, ((enum_elements(done) = nil(elt)) & ![X:elt,
  X1:tree, X2:enum]: (enum_elements(next(X, X1, X2)) = cons(elt, X,
  infix_plpl(elt, elements(X1), enum_elements(X2)))))).

tff(wP_parameter_eq_enum, conjecture, ![X:elt, X1:tree, X2:enum]: ![X3:elt,
  X4:tree, X5:enum]: ![Result:bool]: (((Result = true) <=> (X3 = X))
  => ((Result = true) => ![O:enum]:
  ((enum_elements(O) = infix_plpl(elt, elements(X1), enum_elements(X2)))
  => ![O1:enum]: ((enum_elements(O1) = infix_plpl(elt, elements(X4),
  enum_elements(X5))) => ![Result1:bool]: (((Result1 = true)
  <=> (enum_elements(O1) = enum_elements(O))) => ((Result1 = true)
  <=> (enum_elements(next(X3, X4, X5)) = enum_elements(next(X, X1,
  X2)))))))))).
