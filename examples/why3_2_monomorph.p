fof(witness_sort, axiom, ![A]: (sort(A, witness(A)) = witness(A))).

fof(true_sort, axiom, (sort(bool1, true1) = true1)).

fof(false_sort, axiom, (sort(bool1, false1) = false1)).

fof(match_bool_sort, axiom, ![A]: ![X, X1, X2]: (sort(A, match_bool1(A, X,
  X1, X2)) = match_bool1(A, X, X1, X2))).

fof(match_bool_True, axiom, ![A]: ![Z, Z1]: (match_bool1(A, true1, Z,
  Z1) = sort(A, Z))).

fof(match_bool_False, axiom, ![A]: ![Z, Z1]: (match_bool1(A, false1, Z,
  Z1) = sort(A, Z1))).

fof(true_False, axiom, ~ (true1 = false1)).

fof(bool_inversion, axiom, ![U]: ((sort(bool1, U) = true1) | (sort(bool1,
  U) = false1))).

fof(tuple0_sort, axiom, (sort(tuple02, tuple03) = tuple03)).

fof(tuple0_inversion, axiom, ![U]: (sort(tuple02, U) = tuple03)).

fof(red_sort, axiom, (sort(color1, red1) = red1)).

fof(black_sort, axiom, (sort(color1, black1) = black1)).

fof(match_color_sort, axiom, ![A]: ![X, X1, X2]: (sort(A, match_color1(A, X,
  X1, X2)) = match_color1(A, X, X1, X2))).

fof(match_color_Red, axiom, ![A]: ![Z, Z1]: (match_color1(A, red1, Z,
  Z1) = sort(A, Z))).

fof(match_color_Black, axiom, ![A]: ![Z, Z1]: (match_color1(A, black1, Z,
  Z1) = sort(A, Z1))).

fof(red_Black, axiom, ~ (red1 = black1)).

fof(color_inversion, axiom, ![U]: ((sort(color1, U) = red1) | (sort(color1,
  U) = black1))).

fof(leaf_sort, axiom, (sort(tree1, leaf1) = leaf1)).

fof(node_sort, axiom, ![X, X1, X2, X3, X4]: (sort(tree1, node1(X, X1, X2, X3,
  X4)) = node1(X, X1, X2, X3, X4))).

fof(match_tree_sort, axiom, ![A]: ![X, X1, X2]: (sort(A, match_tree1(A, X,
  X1, X2)) = match_tree1(A, X, X1, X2))).

fof(match_tree_Leaf, axiom, ![A]: ![Z, Z1]: (match_tree1(A, leaf1, Z,
  Z1) = sort(A, Z))).

fof(match_tree_Node, axiom, ![A]: ![Z, Z1, U, U1, U2, U3, U4]:
  (match_tree1(A, node1(U, U1, U2, U3, U4), Z, Z1) = sort(A, Z1))).

fof(leaf_Node, axiom, ![V, V1, V2, V3, V4]: ~ (leaf1 = node1(V, V1, V2, V3,
  V4))).

fof(node_proj_1_sort, axiom, ![X]: (sort(color1,
  node_proj_11(X)) = node_proj_11(X))).

fof(node_proj_1_def, axiom, ![U, U1, U2, U3, U4]: (node_proj_11(node1(U, U1,
  U2, U3, U4)) = sort(color1, U))).

fof(node_proj_2_sort, axiom, ![X]: (sort(tree1,
  node_proj_21(X)) = node_proj_21(X))).

fof(node_proj_2_def, axiom, ![U, U1, U2, U3, U4]: (node_proj_21(node1(U, U1,
  U2, U3, U4)) = U1)).

fof(node_proj_3_sort, axiom, ![X]: (sort(int,
  node_proj_31(X)) = node_proj_31(X))).

fof(node_proj_3_def, axiom, ![U, U1, U2, U3, U4]: (node_proj_31(node1(U, U1,
  U2, U3, U4)) = U2)).

fof(node_proj_4_sort, axiom, ![X]: (sort(int,
  node_proj_41(X)) = node_proj_41(X))).

fof(node_proj_4_def, axiom, ![U, U1, U2, U3, U4]: (node_proj_41(node1(U, U1,
  U2, U3, U4)) = U3)).

fof(node_proj_5_sort, axiom, ![X]: (sort(tree1,
  node_proj_51(X)) = node_proj_51(X))).

fof(node_proj_5_def, axiom, ![U, U1, U2, U3, U4]: (node_proj_51(node1(U, U1,
  U2, U3, U4)) = U4)).

fof(tree_inversion, axiom, ![U]: ((U = leaf1) | (U = node1(node_proj_11(U),
  node_proj_21(U), node_proj_31(U), node_proj_41(U), node_proj_51(U))))).

fof(memt_def, axiom, ![K, V]: (~ memt1(leaf1, K, V) & ![X, X1, X2, X3, X4]:
  (memt1(node1(X, X1, X2, X3, X4), K, V) <=> (((K = X2) & (V = X3))
  | (memt1(X1, K, V) | memt1(X4, K, V)))))).

fof(memt_color, conjecture, ![L, R, K, Kqt, V, Vqt, C, Cqt]: ((sort(tree1,
  L) = L) => ((sort(tree1, R) = R) => ((sort(int, K) = K) => ((sort(int,
  Kqt) = Kqt) => ((sort(int, V) = V) => ((sort(int, Vqt) = Vqt)
  => ((sort(color1, C) = C) => ((sort(color1, Cqt) = Cqt) => (memt1(node1(C,
  L, K, V, R), Kqt, Vqt) => memt1(node1(Cqt, L, K, V, R), Kqt,
  Vqt))))))))))).
