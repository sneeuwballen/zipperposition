
% expect: sat

tff(zf_stmt_0, type, term : $tType).
tff(zf_stmt_1, type, app_encode_fun : ($tType * $tType) > $tType).
tff(zf_stmt_2, type, app : !> [Alpha:$tType,Beta:$tType]: (app_encode_fun(Alpha,Beta) * Alpha) > Beta).
tff(zf_stmt_3, type, f : app_encode_fun($i,app_encode_fun($i,$i))).
tff(zf_stmt_4, axiom,
  (app($i,$i,app($i,$_,f,X),Y) =
   app($i,$i,app($i,$_,f,Y),X))).
  
