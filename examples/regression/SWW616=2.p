%------------------------------------------------------------------------------
% File     : SWW616=2 : TPTP v6.4.0. Released v6.1.0.
% Domain   : Software Verification
% Problem  : Maximum subarray-T-WP parameter maximum subarray rec
% Version  : Especial : Let and conditional terms encoded away.
% English  :

% Refs     : [Fil14] Filliatre (2014), Email to Geoff Sutcliffe
%          : [BF+]   Bobot et al. (URL), Toccata: Certified Programs and Cert
% Source   : [Fil14]
% Names    : maximum_subarray-T-WP_parameter_maximum_subarray_rec [Fil14]

% Status   : Theorem
% Rating   : 0.00 v6.4.0, 0.33 v6.3.0, 0.14 v6.2.0, 0.38 v6.1.0
% Syntax   : Number of formulae    :  108 (  32 unit;  44 type)
%            Number of atoms       :  150 (  53 equality)
%            Maximal formula depth :   26 (   4 average)
%            Number of connectives :   95 (   9   ~;   1   |;  34   &)
%                                         (   3 <=>;  48  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :   66 (  30   >;  36   *;   0   +;   0  <<)
%            Number of predicates  :   53 (  47 propositional; 0-3 arity)
%            Number of functors    :   38 (   7 constant; 0-5 arity)
%            Number of variables   :  170 (   0 sgn; 170   !;   0   ?)
%                                         ( 170   :;   0  !>;   0  ?*)
%            Maximal term depth    :    6 (   2 average)
%            Arithmetic symbols    :   89 (   2 prd;   4 fun;   3 num;  80 var)
% SPC      : TF0_THM_EQU_ARI

% Comments :
%------------------------------------------------------------------------------
tff(uni,type,(
    uni: $tType )).

tff(ty,type,(
    ty: $tType )).

tff(sort,type,(
    sort: ( ty * uni ) > $o )).

tff(witness,type,(
    witness: ty > uni )).

tff(witness_sort,axiom,(
    ! [A: ty] : sort(A,witness(A)) )).

tff(int,type,(
    int: ty )).

tff(real,type,(
    real: ty )).

tff(bool,type,(
    bool: $tType )).

tff(bool1,type,(
    bool1: ty )).

tff(true,type,(
    true: bool )).

tff(false,type,(
    false: bool )).

tff(match_bool,type,(
    match_bool: ( ty * bool * uni * uni ) > uni )).

tff(match_bool_sort,axiom,(
    ! [A: ty,X: bool,X1: uni,X2: uni] : sort(A,match_bool(A,X,X1,X2)) )).

tff(match_bool_True,axiom,(
    ! [A: ty,Z: uni,Z1: uni] :
      ( sort(A,Z)
     => match_bool(A,true,Z,Z1) = Z ) )).

tff(match_bool_False,axiom,(
    ! [A: ty,Z: uni,Z1: uni] :
      ( sort(A,Z1)
     => match_bool(A,false,Z,Z1) = Z1 ) )).

tff(true_False,axiom,(
    true != false )).

tff(bool_inversion,axiom,(
    ! [U: bool] :
      ( U = true
      | U = false ) )).

tff(tuple0,type,(
    tuple0: $tType )).

tff(tuple01,type,(
    tuple01: ty )).

tff(tuple02,type,(
    tuple02: tuple0 )).

tff(tuple0_inversion,axiom,(
    ! [U: tuple0] : U = tuple02 )).

tff(qtmark,type,(
    qtmark: ty )).

tff(compatOrderMult,axiom,(
    ! [X: $int,Y: $int,Z: $int] :
      ( $lesseq(X,Y)
     => ( $lesseq(0,Z)
       => $lesseq($product(X,Z),$product(Y,Z)) ) ) )).

tff(ref,type,(
    ref: ty > ty )).

tff(mk_ref,type,(
    mk_ref: ( ty * uni ) > uni )).

tff(mk_ref_sort,axiom,(
    ! [A: ty,X: uni] : sort(ref(A),mk_ref(A,X)) )).

tff(contents,type,(
    contents: ( ty * uni ) > uni )).

tff(contents_sort,axiom,(
    ! [A: ty,X: uni] : sort(A,contents(A,X)) )).

tff(contents_def,axiom,(
    ! [A: ty,U: uni] :
      ( sort(A,U)
     => contents(A,mk_ref(A,U)) = U ) )).

tff(ref_inversion,axiom,(
    ! [A: ty,U: uni] :
      ( sort(ref(A),U)
     => U = mk_ref(A,contents(A,U)) ) )).

tff(abs,type,(
    abs: $int > $int )).

tff(abs_def,axiom,(
    ! [X: $int] :
      ( ( $lesseq(0,X)
       => abs(X) = X )
      & ( ~ $lesseq(0,X)
       => abs(X) = $uminus(X) ) ) )).

tff(abs_le,axiom,(
    ! [X: $int,Y: $int] :
      ( $lesseq(abs(X),Y)
    <=> ( $lesseq($uminus(Y),X)
        & $lesseq(X,Y) ) ) )).

tff(abs_pos,axiom,(
    ! [X: $int] : $lesseq(0,abs(X)) )).

tff(div,type,(
    div: ( $int * $int ) > $int )).

tff(mod,type,(
    mod: ( $int * $int ) > $int )).

tff(div_mod,axiom,(
    ! [X: $int,Y: $int] :
      ( Y != 0
     => X = $sum($product(Y,div(X,Y)),mod(X,Y)) ) )).

tff(div_bound,axiom,(
    ! [X: $int,Y: $int] :
      ( ( $lesseq(0,X)
        & $less(0,Y) )
     => ( $lesseq(0,div(X,Y))
        & $lesseq(div(X,Y),X) ) ) )).

tff(mod_bound,axiom,(
    ! [X: $int,Y: $int] :
      ( Y != 0
     => ( $less($uminus(abs(Y)),mod(X,Y))
        & $less(mod(X,Y),abs(Y)) ) ) )).

tff(div_sign_pos,axiom,(
    ! [X: $int,Y: $int] :
      ( ( $lesseq(0,X)
        & $less(0,Y) )
     => $lesseq(0,div(X,Y)) ) )).

tff(div_sign_neg,axiom,(
    ! [X: $int,Y: $int] :
      ( ( $lesseq(X,0)
        & $less(0,Y) )
     => $lesseq(div(X,Y),0) ) )).

tff(mod_sign_pos,axiom,(
    ! [X: $int,Y: $int] :
      ( ( $lesseq(0,X)
        & Y != 0 )
     => $lesseq(0,mod(X,Y)) ) )).

tff(mod_sign_neg,axiom,(
    ! [X: $int,Y: $int] :
      ( ( $lesseq(X,0)
        & Y != 0 )
     => $lesseq(mod(X,Y),0) ) )).

tff(rounds_toward_zero,axiom,(
    ! [X: $int,Y: $int] :
      ( Y != 0
     => $lesseq(abs($product(div(X,Y),Y)),abs(X)) ) )).

tff(div_1,axiom,(
    ! [X: $int] : div(X,1) = X )).

tff(mod_1,axiom,(
    ! [X: $int] : mod(X,1) = 0 )).

tff(div_inf,axiom,(
    ! [X: $int,Y: $int] :
      ( ( $lesseq(0,X)
        & $less(X,Y) )
     => div(X,Y) = 0 ) )).

tff(mod_inf,axiom,(
    ! [X: $int,Y: $int] :
      ( ( $lesseq(0,X)
        & $less(X,Y) )
     => mod(X,Y) = X ) )).

tff(div_mult,axiom,(
    ! [X: $int,Y: $int,Z: $int] :
      ( ( $less(0,X)
        & $lesseq(0,Y)
        & $lesseq(0,Z) )
     => div($sum($product(X,Y),Z),X) = $sum(Y,div(Z,X)) ) )).

tff(mod_mult,axiom,(
    ! [X: $int,Y: $int,Z: $int] :
      ( ( $less(0,X)
        & $lesseq(0,Y)
        & $lesseq(0,Z) )
     => mod($sum($product(X,Y),Z),X) = mod(Z,X) ) )).

tff(map,type,(
    map: ( ty * ty ) > ty )).

tff(get,type,(
    get: ( ty * ty * uni * uni ) > uni )).

tff(get_sort,axiom,(
    ! [A: ty,B: ty,X: uni,X1: uni] : sort(B,get(B,A,X,X1)) )).

tff(set,type,(
    set: ( ty * ty * uni * uni * uni ) > uni )).

tff(set_sort,axiom,(
    ! [A: ty,B: ty,X: uni,X1: uni,X2: uni] : sort(map(A,B),set(B,A,X,X1,X2)) )).

tff(select_eq,axiom,(
    ! [A: ty,B: ty,M: uni,A1: uni,A2: uni,B1: uni] :
      ( sort(B,B1)
     => ( A1 = A2
       => get(B,A,set(B,A,M,A1,B1),A2) = B1 ) ) )).

tff(select_neq,axiom,(
    ! [A: ty,B: ty,M: uni,A1: uni,A2: uni] :
      ( sort(A,A1)
     => ( sort(A,A2)
       => ! [B1: uni] :
            ( A1 != A2
           => get(B,A,set(B,A,M,A1,B1),A2) = get(B,A,M,A2) ) ) ) )).

tff(const,type,(
    const: ( ty * ty * uni ) > uni )).

tff(const_sort,axiom,(
    ! [A: ty,B: ty,X: uni] : sort(map(A,B),const(B,A,X)) )).

tff(const1,axiom,(
    ! [A: ty,B: ty,B1: uni,A1: uni] :
      ( sort(B,B1)
     => get(B,A,const(B,A,B1),A1) = B1 ) )).

tff(array,type,(
    array: ty > ty )).

tff(mk_array,type,(
    mk_array: ( ty * $int * uni ) > uni )).

tff(mk_array_sort,axiom,(
    ! [A: ty,X: $int,X1: uni] : sort(array(A),mk_array(A,X,X1)) )).

tff(length,type,(
    length: ( ty * uni ) > $int )).

tff(length_def,axiom,(
    ! [A: ty,U: $int,U1: uni] : length(A,mk_array(A,U,U1)) = U )).

tff(elts,type,(
    elts: ( ty * uni ) > uni )).

tff(elts_sort,axiom,(
    ! [A: ty,X: uni] : sort(map(int,A),elts(A,X)) )).

tff(elts_def,axiom,(
    ! [A: ty,U: $int,U1: uni] :
      ( sort(map(int,A),U1)
     => elts(A,mk_array(A,U,U1)) = U1 ) )).

tff(array_inversion,axiom,(
    ! [A: ty,U: uni] : U = mk_array(A,length(A,U),elts(A,U)) )).

tff(get1,type,(
    get1: ( ty * uni * $int ) > uni )).

tff(get_sort1,axiom,(
    ! [A: ty,X: uni,X1: $int] : sort(A,get1(A,X,X1)) )).

tff(t2tb,type,(
    t2tb: $int > uni )).

tff(t2tb_sort,axiom,(
    ! [X: $int] : sort(int,t2tb(X)) )).

tff(tb2t,type,(
    tb2t: uni > $int )).

tff(bridgeL,axiom,(
    ! [I: $int] : tb2t(t2tb(I)) = I )).

tff(bridgeR,axiom,(
    ! [J: uni] : t2tb(tb2t(J)) = J )).

tff(get_def,axiom,(
    ! [A: ty,A1: uni,I: $int] : get1(A,A1,I) = get(A,int,elts(A,A1),t2tb(I)) )).

tff(set1,type,(
    set1: ( ty * uni * $int * uni ) > uni )).

tff(set_sort1,axiom,(
    ! [A: ty,X: uni,X1: $int,X2: uni] : sort(array(A),set1(A,X,X1,X2)) )).

tff(set_def,axiom,(
    ! [A: ty,A1: uni,I: $int,V: uni] : set1(A,A1,I,V) = mk_array(A,length(A,A1),set(A,int,elts(A,A1),t2tb(I),V)) )).

tff(make,type,(
    make: ( ty * $int * uni ) > uni )).

tff(make_sort,axiom,(
    ! [A: ty,X: $int,X1: uni] : sort(array(A),make(A,X,X1)) )).

tff(make_def,axiom,(
    ! [A: ty,N: $int,V: uni] : make(A,N,V) = mk_array(A,N,const(A,int,V)) )).

tff(map_int_int,type,(
    map_int_int: $tType )).

tff(sum,type,(
    sum: ( map_int_int * $int * $int ) > $int )).

tff(sum_def_empty,axiom,(
    ! [C: map_int_int,I: $int,J: $int] :
      ( $lesseq(J,I)
     => sum(C,I,J) = 0 ) )).

tff(t2tb1,type,(
    t2tb1: map_int_int > uni )).

tff(t2tb_sort1,axiom,(
    ! [X: map_int_int] : sort(map(int,int),t2tb1(X)) )).

tff(tb2t1,type,(
    tb2t1: uni > map_int_int )).

tff(bridgeL1,axiom,(
    ! [I: map_int_int] : tb2t1(t2tb1(I)) = I )).

tff(bridgeR1,axiom,(
    ! [J: uni] : t2tb1(tb2t1(J)) = J )).

tff(sum_def_non_empty,axiom,(
    ! [C: map_int_int,I: $int,J: $int] :
      ( $less(I,J)
     => sum(C,I,J) = $sum(tb2t(get(int,int,t2tb1(C),t2tb(I))),sum(C,$sum(I,1),J)) ) )).

tff(sum_right_extension,axiom,(
    ! [C: map_int_int,I: $int,J: $int] :
      ( $less(I,J)
     => sum(C,I,J) = $sum(sum(C,I,$difference(J,1)),tb2t(get(int,int,t2tb1(C),t2tb($difference(J,1))))) ) )).

tff(sum_transitivity,axiom,(
    ! [C: map_int_int,I: $int,K: $int,J: $int] :
      ( ( $lesseq(I,K)
        & $lesseq(K,J) )
     => sum(C,I,J) = $sum(sum(C,I,K),sum(C,K,J)) ) )).

tff(sum_eq,axiom,(
    ! [C1: map_int_int,C2: map_int_int,I: $int,J: $int] :
      ( ! [K: $int] :
          ( ( $lesseq(I,K)
            & $less(K,J) )
         => tb2t(get(int,int,t2tb1(C1),t2tb(K))) = tb2t(get(int,int,t2tb1(C2),t2tb(K))) )
     => sum(C1,I,J) = sum(C2,I,J) ) )).

tff(array_int,type,(
    array_int: $tType )).

tff(sum1,type,(
    sum1: ( array_int * $int * $int ) > $int )).

tff(t2tb2,type,(
    t2tb2: array_int > uni )).

tff(t2tb_sort2,axiom,(
    ! [X: array_int] : sort(array(int),t2tb2(X)) )).

tff(tb2t2,type,(
    tb2t2: uni > array_int )).

tff(bridgeL2,axiom,(
    ! [I: array_int] : tb2t2(t2tb2(I)) = I )).

tff(bridgeR2,axiom,(
    ! [J: uni] : t2tb2(tb2t2(J)) = J )).

tff(sum_def,axiom,(
    ! [A: array_int,L: $int,H: $int] : sum1(A,L,H) = sum(tb2t1(elts(int,t2tb2(A))),L,H) )).

tff(maxsublo,type,(
    maxsublo: ( array_int * $int * $int ) > $o )).

tff(maxsublo_def,axiom,(
    ! [A: array_int,Maxlo: $int,S: $int] :
      ( maxsublo(A,Maxlo,S)
    <=> ! [L: $int,H: $int] :
          ( ( $lesseq(0,L)
            & $less(L,Maxlo) )
         => ( ( $lesseq(L,H)
              & $lesseq(H,length(int,t2tb2(A))) )
           => $lesseq(sum1(A,L,H),S) ) ) ) )).

tff(maxsub,type,(
    maxsub: ( array_int * $int ) > $o )).

tff(maxsub_def,axiom,(
    ! [A: array_int,S: $int] :
      ( maxsub(A,S)
    <=> ! [L: $int,H: $int] :
          ( ( $lesseq(0,L)
            & $lesseq(L,H)
            & $lesseq(H,length(int,t2tb2(A))) )
         => $lesseq(sum1(A,L,H),S) ) ) )).

tff(wP_parameter_maximum_subarray_rec,conjecture,(
    ! [A: $int,A1: map_int_int,L: $int,H: $int] :
      ( ( $lesseq(0,A)
        & $lesseq(0,L)
        & $lesseq(L,H)
        & $lesseq(H,A) )
     => ( H != L
       => ! [Lo: $int] :
            ( Lo = $sum(L,div($difference(H,L),2))
           => ! [Hi: $int] :
                ( Hi = $sum(L,div($difference(H,L),2))
               => ( $lesseq(L,$difference($sum(L,div($difference(H,L),2)),1))
                 => ! [S: $int,Ms: $int,Lo1: $int] :
                      ( ( $lesseq(L,Lo1)
                        & $lesseq(Lo1,$sum(L,div($difference(H,L),2)))
                        & $sum(L,div($difference(H,L),2)) = Hi
                        & Ms = sum(A1,Lo1,Hi)
                        & ! [Lqt: $int] :
                            ( ( $less($difference(L,1),Lqt)
                              & $lesseq(Lqt,$sum(L,div($difference(H,L),2))) )
                           => $lesseq(sum(A1,Lqt,$sum(L,div($difference(H,L),2))),Ms) )
                        & S = sum(A1,$sum($difference(L,1),1),$sum(L,div($difference(H,L),2))) )
                     => ( ! [Lqt: $int] :
                            ( ( $lesseq(L,Lqt)
                              & $lesseq(Lqt,$sum(L,div($difference(H,L),2))) )
                           => $lesseq(sum(A1,Lqt,$sum(L,div($difference(H,L),2))),sum(A1,Lo1,$sum(L,div($difference(H,L),2)))) )
                       => ! [S1: $int] :
                            ( S1 = Ms
                           => ( $lesseq($sum(L,div($difference(H,L),2)),$difference(H,1))
                             => ! [Lqt: $int,Hqt: $int] :
                                  ( ( $lesseq(L,Lqt)
                                    & $lesseq(Lqt,$sum(L,div($difference(H,L),2)))
                                    & $lesseq($sum(L,div($difference(H,L),2)),Hqt)
                                    & $lesseq(Hqt,$sum(L,div($difference(H,L),2))) )
                                 => $lesseq(sum(A1,Lqt,Hqt),Ms) ) ) ) ) ) ) ) ) ) ) )).

%------------------------------------------------------------------------------
