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

tff(color, type, color: $tType).

tff(red, type, red: color).

tff(black, type, black: color).

tff(match_color, type, match_color: !>[A : $tType]: ((color * A * A) > A)).

tff(match_color_Red, axiom, ![A : $tType]: ![Z:A, Z1:A]: (match_color(A, red,
  Z, Z1) = Z)).

tff(match_color_Black, axiom, ![A : $tType]: ![Z:A, Z1:A]:
  (match_color(A, black, Z, Z1) = Z1)).

tff(red_Black, axiom, ~ (red = black)).

tff(color_inversion, axiom, ![U:color]: ((U = red) | (U = black))).

tff(tree, type, tree: $tType).

tff(leaf, type, leaf: tree).

tff(node, type, node: (color * tree * $int * $int * tree) > tree).

tff(match_tree, type, match_tree: !>[A : $tType]: ((tree * A * A) > A)).

tff(match_tree_Leaf, axiom, ![A : $tType]: ![Z:A, Z1:A]: (match_tree(A, leaf,
  Z, Z1) = Z)).

tff(match_tree_Node, axiom, ![A : $tType]: ![Z:A, Z1:A, U:color, U1:tree, U2:
  $int, U3:$int, U4:tree]: (match_tree(A, node(U, U1, U2, U3, U4), Z,
  Z1) = Z1)).

tff(leaf_Node, axiom, ![V:color, V1:tree, V2:$int, V3:$int, V4:tree]: ~
  (leaf = node(V, V1, V2, V3, V4))).

tff(node_proj_1, type, node_proj_1: tree > color).

tff(node_proj_1_def, axiom, ![U:color, U1:tree, U2:$int, U3:$int, U4:tree]:
  (node_proj_1(node(U, U1, U2, U3, U4)) = U)).

tff(node_proj_2, type, node_proj_2: tree > tree).

tff(node_proj_2_def, axiom, ![U:color, U1:tree, U2:$int, U3:$int, U4:tree]:
  (node_proj_2(node(U, U1, U2, U3, U4)) = U1)).

tff(node_proj_3, type, node_proj_3: tree > $int).

tff(node_proj_3_def, axiom, ![U:color, U1:tree, U2:$int, U3:$int, U4:tree]:
  (node_proj_3(node(U, U1, U2, U3, U4)) = U2)).

tff(node_proj_4, type, node_proj_4: tree > $int).

tff(node_proj_4_def, axiom, ![U:color, U1:tree, U2:$int, U3:$int, U4:tree]:
  (node_proj_4(node(U, U1, U2, U3, U4)) = U3)).

tff(node_proj_5, type, node_proj_5: tree > tree).

tff(node_proj_5_def, axiom, ![U:color, U1:tree, U2:$int, U3:$int, U4:tree]:
  (node_proj_5(node(U, U1, U2, U3, U4)) = U4)).

tff(tree_inversion, axiom, ![U:tree]: ((U = leaf) | (U = node(node_proj_1(U),
  node_proj_2(U), node_proj_3(U), node_proj_4(U), node_proj_5(U))))).

tff(memt, type, memt: (tree * $int * $int) > $o).

tff(memt_def, axiom, ![K:$int, V:$int]: (~ memt(leaf, K, V) & ![X:color, X1:
  tree, X2:$int, X3:$int, X4:tree]: (memt(node(X, X1, X2, X3, X4), K, V)
  <=> (((K = X2) & (V = X3)) | (memt(X1, K, V) | memt(X4, K, V)))))).

tff(memt_color, conjecture, ![L:tree, R:tree, K:$int, Kqt:$int, V:$int, Vqt:
  $int, C:color, Cqt:color]: (memt(node(C, L, K, V, R), Kqt, Vqt)
  => memt(node(Cqt, L, K, V, R), Kqt, Vqt))).
