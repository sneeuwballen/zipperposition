%------------------------------------------------------------------------------
% File     : SEU684^1 : TPTP v6.2.0. Released v3.7.0.
% Domain   : Set Theory
% Problem  : Functions - Extensionality and Beta Reduction
% Version  : Especial.
% English  : (! A:i.! B:i.! f:i.func A B f -> (! x:i.in x A ->
%            in (kpair x (ap A B f x)) f))

% Refs     : [Bro08] Brown (2008), Email to G. Sutcliffe
% Source   : [Bro08]
% Names    : ZFC186g [Bro08]

% Status   : Theorem
% Rating   : 0.86 v5.5.0, 1.00 v5.4.0, 0.80 v5.2.0, 1.00 v3.7.0
% Syntax   : Number of formulae    :  446 ( 202 unit; 237 type; 208 defn)
%            Number of atoms       : 3120 ( 304 equality;1307 variable)
%            Maximal formula depth :  214 (   6 average)
%            Number of connectives : 2046 (  47   ~;   7   |;  36   &;1444   @)
%                                         (  14 <=>; 498  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :  126 ( 126   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :  242 ( 237   :)
%            Number of variables   :  667 (   2 sgn; 553   !;  36   ?;  78   ^)
%                                         ( 667   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU

% Comments : http://mathgate.info/detsetitem.php?id=241
%          : 
%------------------------------------------------------------------------------
thf(in_type,type,(
    in: $i > $i > $o )).

thf(exu_type,type,(
    exu: ( $i > $o ) > $o )).

thf(exu,definition,
    ( exu
    = ( ^ [Xphi: $i > $o] :
        ? [Xx: $i] :
          ( ( Xphi @ Xx )
          & ! [Xy: $i] :
              ( ( Xphi @ Xy )
             => ( Xx = Xy ) ) ) ) )).

thf(setextAx_type,type,(
    setextAx: $o )).

thf(setextAx,definition,
    ( setextAx
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
            <=> ( in @ Xx @ B ) )
         => ( A = B ) ) ) )).

thf(emptyset_type,type,(
    emptyset: $i )).

thf(emptysetAx_type,type,(
    emptysetAx: $o )).

thf(emptysetAx,definition,
    ( emptysetAx
    = ( ! [Xx: $i] :
          ~ ( in @ Xx @ emptyset ) ) )).

thf(setadjoin_type,type,(
    setadjoin: $i > $i > $i )).

thf(setadjoinAx_type,type,(
    setadjoinAx: $o )).

thf(setadjoinAx,definition,
    ( setadjoinAx
    = ( ! [Xx: $i,A: $i,Xy: $i] :
          ( ( in @ Xy @ ( setadjoin @ Xx @ A ) )
        <=> ( ( Xy = Xx )
            | ( in @ Xy @ A ) ) ) ) )).

thf(powerset_type,type,(
    powerset: $i > $i )).

thf(powersetAx_type,type,(
    powersetAx: $o )).

thf(powersetAx,definition,
    ( powersetAx
    = ( ! [A: $i,B: $i] :
          ( ( in @ B @ ( powerset @ A ) )
        <=> ! [Xx: $i] :
              ( ( in @ Xx @ B )
             => ( in @ Xx @ A ) ) ) ) )).

thf(setunion_type,type,(
    setunion: $i > $i )).

thf(setunionAx_type,type,(
    setunionAx: $o )).

thf(setunionAx,definition,
    ( setunionAx
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ ( setunion @ A ) )
        <=> ? [B: $i] :
              ( ( in @ Xx @ B )
              & ( in @ B @ A ) ) ) ) )).

thf(omega_type,type,(
    omega: $i )).

thf(omega0Ax_type,type,(
    omega0Ax: $o )).

thf(omega0Ax,definition,
    ( omega0Ax
    = ( in @ emptyset @ omega ) )).

thf(omegaSAx_type,type,(
    omegaSAx: $o )).

thf(omegaSAx,definition,
    ( omegaSAx
    = ( ! [Xx: $i] :
          ( ( in @ Xx @ omega )
         => ( in @ ( setadjoin @ Xx @ Xx ) @ omega ) ) ) )).

thf(omegaIndAx_type,type,(
    omegaIndAx: $o )).

thf(omegaIndAx,definition,
    ( omegaIndAx
    = ( ! [A: $i] :
          ( ( ( in @ emptyset @ A )
            & ! [Xx: $i] :
                ( ( ( in @ Xx @ omega )
                  & ( in @ Xx @ A ) )
               => ( in @ ( setadjoin @ Xx @ Xx ) @ A ) ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ omega )
             => ( in @ Xx @ A ) ) ) ) )).

thf(replAx_type,type,(
    replAx: $o )).

thf(replAx,definition,
    ( replAx
    = ( ! [Xphi: $i > $i > $o,A: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( exu
                @ ^ [Xy: $i] :
                    ( Xphi @ Xx @ Xy ) ) )
         => ? [B: $i] :
            ! [Xx: $i] :
              ( ( in @ Xx @ B )
            <=> ? [Xy: $i] :
                  ( ( in @ Xy @ A )
                  & ( Xphi @ Xy @ Xx ) ) ) ) ) )).

thf(foundationAx_type,type,(
    foundationAx: $o )).

thf(foundationAx,definition,
    ( foundationAx
    = ( ! [A: $i] :
          ( ? [Xx: $i] :
              ( in @ Xx @ A )
         => ? [B: $i] :
              ( ( in @ B @ A )
              & ~ ( ? [Xx: $i] :
                      ( ( in @ Xx @ B )
                      & ( in @ Xx @ A ) ) ) ) ) ) )).

thf(wellorderingAx_type,type,(
    wellorderingAx: $o )).

thf(wellorderingAx,definition,
    ( wellorderingAx
    = ( ! [A: $i] :
        ? [B: $i] :
          ( ! [C: $i] :
              ( ( in @ C @ B )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ C )
                 => ( in @ Xx @ A ) ) )
          & ! [Xx: $i,Xy: $i] :
              ( ( ( in @ Xx @ A )
                & ( in @ Xy @ A ) )
             => ( ! [C: $i] :
                    ( ( in @ C @ B )
                   => ( ( in @ Xx @ C )
                    <=> ( in @ Xy @ C ) ) )
               => ( Xx = Xy ) ) )
          & ! [C: $i,D: $i] :
              ( ( ( in @ C @ B )
                & ( in @ D @ B ) )
             => ( ! [Xx: $i] :
                    ( ( in @ Xx @ C )
                   => ( in @ Xx @ D ) )
                | ! [Xx: $i] :
                    ( ( in @ Xx @ D )
                   => ( in @ Xx @ C ) ) ) )
          & ! [C: $i] :
              ( ( ! [Xx: $i] :
                    ( ( in @ Xx @ C )
                   => ( in @ Xx @ A ) )
                & ? [Xx: $i] :
                    ( in @ Xx @ C ) )
             => ? [D: $i,Xx: $i] :
                  ( ( in @ D @ B )
                  & ( in @ Xx @ C )
                  & ~ ( ? [Xy: $i] :
                          ( ( in @ Xy @ D )
                          & ( in @ Xy @ C ) ) )
                  & ! [E: $i] :
                      ( ( in @ E @ B )
                     => ( ! [Xy: $i] :
                            ( ( in @ Xy @ E )
                           => ( in @ Xy @ D ) )
                        | ( in @ Xx @ E ) ) ) ) ) ) ) )).

thf(descr_type,type,(
    descr: ( $i > $o ) > $i )).

thf(descrp_type,type,(
    descrp: $o )).

thf(descrp,definition,
    ( descrp
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ( Xphi
            @ ( descr
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) ) ) ) ) )).

thf(dsetconstr_type,type,(
    dsetconstr: $i > ( $i > $o ) > $i )).

thf(dsetconstrI_type,type,(
    dsetconstrI: $o )).

thf(dsetconstrI,definition,
    ( dsetconstrI
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( Xphi @ Xx )
           => ( in @ Xx
              @ ( dsetconstr @ A
                @ ^ [Xy: $i] :
                    ( Xphi @ Xy ) ) ) ) ) ) )).

thf(dsetconstrEL_type,type,(
    dsetconstrEL: $o )).

thf(dsetconstrEL,definition,
    ( dsetconstrEL
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx
            @ ( dsetconstr @ A
              @ ^ [Xy: $i] :
                  ( Xphi @ Xy ) ) )
         => ( in @ Xx @ A ) ) ) )).

thf(dsetconstrER_type,type,(
    dsetconstrER: $o )).

thf(dsetconstrER,definition,
    ( dsetconstrER
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx
            @ ( dsetconstr @ A
              @ ^ [Xy: $i] :
                  ( Xphi @ Xy ) ) )
         => ( Xphi @ Xx ) ) ) )).

thf(exuE1_type,type,(
    exuE1: $o )).

thf(exuE1,definition,
    ( exuE1
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ? [Xx: $i] :
              ( ( Xphi @ Xx )
              & ! [Xy: $i] :
                  ( ( Xphi @ Xy )
                 => ( Xx = Xy ) ) ) ) ) )).

thf(prop2set_type,type,(
    prop2set: $o > $i )).

thf(prop2setE_type,type,(
    prop2setE: $o )).

thf(prop2setE,definition,
    ( prop2setE
    = ( ! [Xphi: $o,Xx: $i] :
          ( ( in @ Xx @ ( prop2set @ Xphi ) )
         => Xphi ) ) )).

thf(emptysetE_type,type,(
    emptysetE: $o )).

thf(emptysetE,definition,
    ( emptysetE
    = ( ! [Xx: $i] :
          ( ( in @ Xx @ emptyset )
         => ! [Xphi: $o] : Xphi ) ) )).

thf(emptysetimpfalse_type,type,(
    emptysetimpfalse: $o )).

thf(emptysetimpfalse,definition,
    ( emptysetimpfalse
    = ( ! [Xx: $i] :
          ( ( in @ Xx @ emptyset )
         => $false ) ) )).

thf(notinemptyset_type,type,(
    notinemptyset: $o )).

thf(notinemptyset,definition,
    ( notinemptyset
    = ( ! [Xx: $i] :
          ~ ( in @ Xx @ emptyset ) ) )).

thf(exuE3e_type,type,(
    exuE3e: $o )).

thf(exuE3e,definition,
    ( exuE3e
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ? [Xx: $i] :
              ( Xphi @ Xx ) ) ) )).

thf(setext_type,type,(
    setext: $o )).

thf(setext,definition,
    ( setext
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ Xx @ B ) )
         => ( ! [Xx: $i] :
                ( ( in @ Xx @ B )
               => ( in @ Xx @ A ) )
           => ( A = B ) ) ) ) )).

thf(emptyI_type,type,(
    emptyI: $o )).

thf(emptyI,definition,
    ( emptyI
    = ( ! [A: $i] :
          ( ! [Xx: $i] :
              ~ ( in @ Xx @ A )
         => ( A = emptyset ) ) ) )).

thf(noeltsimpempty_type,type,(
    noeltsimpempty: $o )).

thf(noeltsimpempty,definition,
    ( noeltsimpempty
    = ( ! [A: $i] :
          ( ! [Xx: $i] :
              ~ ( in @ Xx @ A )
         => ( A = emptyset ) ) ) )).

thf(setbeta_type,type,(
    setbeta: $o )).

thf(setbeta,definition,
    ( setbeta
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( in @ Xx
              @ ( dsetconstr @ A
                @ ^ [Xy: $i] :
                    ( Xphi @ Xy ) ) )
          <=> ( Xphi @ Xx ) ) ) ) )).

thf(nonempty_type,type,(
    nonempty: $i > $o )).

thf(nonempty,definition,
    ( nonempty
    = ( ^ [Xx: $i] : ( Xx != emptyset ) ) )).

thf(nonemptyE1_type,type,(
    nonemptyE1: $o )).

thf(nonemptyE1,definition,
    ( nonemptyE1
    = ( ! [A: $i] :
          ( ( nonempty @ A )
         => ? [Xx: $i] :
              ( in @ Xx @ A ) ) ) )).

thf(nonemptyI_type,type,(
    nonemptyI: $o )).

thf(nonemptyI,definition,
    ( nonemptyI
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( Xphi @ Xx )
           => ( nonempty
              @ ( dsetconstr @ A
                @ ^ [Xy: $i] :
                    ( Xphi @ Xy ) ) ) ) ) ) )).

thf(nonemptyI1_type,type,(
    nonemptyI1: $o )).

thf(nonemptyI1,definition,
    ( nonemptyI1
    = ( ! [A: $i] :
          ( ? [Xx: $i] :
              ( in @ Xx @ A )
         => ( nonempty @ A ) ) ) )).

thf(setadjoinIL_type,type,(
    setadjoinIL: $o )).

thf(setadjoinIL,definition,
    ( setadjoinIL
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xx @ ( setadjoin @ Xx @ Xy ) ) ) )).

thf(emptyinunitempty_type,type,(
    emptyinunitempty: $o )).

thf(emptyinunitempty,definition,
    ( emptyinunitempty
    = ( in @ emptyset @ ( setadjoin @ emptyset @ emptyset ) ) )).

thf(setadjoinIR_type,type,(
    setadjoinIR: $o )).

thf(setadjoinIR,definition,
    ( setadjoinIR
    = ( ! [Xx: $i,A: $i,Xy: $i] :
          ( ( in @ Xy @ A )
         => ( in @ Xy @ ( setadjoin @ Xx @ A ) ) ) ) )).

thf(setadjoinE_type,type,(
    setadjoinE: $o )).

thf(setadjoinE,definition,
    ( setadjoinE
    = ( ! [Xx: $i,A: $i,Xy: $i] :
          ( ( in @ Xy @ ( setadjoin @ Xx @ A ) )
         => ! [Xphi: $o] :
              ( ( ( Xy = Xx )
               => Xphi )
             => ( ( ( in @ Xy @ A )
                 => Xphi )
               => Xphi ) ) ) ) )).

thf(setadjoinOr_type,type,(
    setadjoinOr: $o )).

thf(setadjoinOr,definition,
    ( setadjoinOr
    = ( ! [Xx: $i,A: $i,Xy: $i] :
          ( ( in @ Xy @ ( setadjoin @ Xx @ A ) )
         => ( ( Xy = Xx )
            | ( in @ Xy @ A ) ) ) ) )).

thf(setoftrueEq_type,type,(
    setoftrueEq: $o )).

thf(setoftrueEq,definition,
    ( setoftrueEq
    = ( ! [A: $i] :
          ( ( dsetconstr @ A
            @ ^ [Xx: $i] : $true )
          = A ) ) )).

thf(powersetI_type,type,(
    powersetI: $o )).

thf(powersetI,definition,
    ( powersetI
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ B )
             => ( in @ Xx @ A ) )
         => ( in @ B @ ( powerset @ A ) ) ) ) )).

thf(emptyinPowerset_type,type,(
    emptyinPowerset: $o )).

thf(emptyinPowerset,definition,
    ( emptyinPowerset
    = ( ! [A: $i] :
          ( in @ emptyset @ ( powerset @ A ) ) ) )).

thf(emptyInPowerset_type,type,(
    emptyInPowerset: $o )).

thf(emptyInPowerset,definition,
    ( emptyInPowerset
    = ( ! [A: $i] :
          ( in @ emptyset @ ( powerset @ A ) ) ) )).

thf(powersetE_type,type,(
    powersetE: $o )).

thf(powersetE,definition,
    ( powersetE
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ B @ ( powerset @ A ) )
         => ( ( in @ Xx @ B )
           => ( in @ Xx @ A ) ) ) ) )).

thf(setunionI_type,type,(
    setunionI: $o )).

thf(setunionI,definition,
    ( setunionI
    = ( ! [A: $i,Xx: $i,B: $i] :
          ( ( in @ Xx @ B )
         => ( ( in @ B @ A )
           => ( in @ Xx @ ( setunion @ A ) ) ) ) ) )).

thf(setunionE_type,type,(
    setunionE: $o )).

thf(setunionE,definition,
    ( setunionE
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ ( setunion @ A ) )
         => ! [Xphi: $o] :
              ( ! [B: $i] :
                  ( ( in @ Xx @ B )
                 => ( ( in @ B @ A )
                   => Xphi ) )
             => Xphi ) ) ) )).

thf(subPowSU_type,type,(
    subPowSU: $o )).

thf(subPowSU,definition,
    ( subPowSU
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( in @ Xx @ ( powerset @ ( setunion @ A ) ) ) ) ) )).

thf(exuE2_type,type,(
    exuE2: $o )).

thf(exuE2,definition,
    ( exuE2
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ? [Xx: $i] :
            ! [Xy: $i] :
              ( ( Xphi @ Xy )
            <=> ( Xy = Xx ) ) ) ) )).

thf(nonemptyImpWitness_type,type,(
    nonemptyImpWitness: $o )).

thf(nonemptyImpWitness,definition,
    ( nonemptyImpWitness
    = ( ! [A: $i] :
          ( ( nonempty @ A )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & $true ) ) ) )).

thf(uniqinunit_type,type,(
    uniqinunit: $o )).

thf(uniqinunit,definition,
    ( uniqinunit
    = ( ! [Xx: $i,Xy: $i] :
          ( ( in @ Xx @ ( setadjoin @ Xy @ emptyset ) )
         => ( Xx = Xy ) ) ) )).

thf(notinsingleton_type,type,(
    notinsingleton: $o )).

thf(notinsingleton,definition,
    ( notinsingleton
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx != Xy )
         => ~ ( in @ Xy @ ( setadjoin @ Xx @ emptyset ) ) ) ) )).

thf(eqinunit_type,type,(
    eqinunit: $o )).

thf(eqinunit,definition,
    ( eqinunit
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx = Xy )
         => ( in @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(singletonsswitch_type,type,(
    singletonsswitch: $o )).

thf(singletonsswitch,definition,
    ( singletonsswitch
    = ( ! [Xx: $i,Xy: $i] :
          ( ( in @ Xx @ ( setadjoin @ Xy @ emptyset ) )
         => ( in @ Xy @ ( setadjoin @ Xx @ emptyset ) ) ) ) )).

thf(upairsetE_type,type,(
    upairsetE: $o )).

thf(upairsetE,definition,
    ( upairsetE
    = ( ! [Xx: $i,Xy: $i,Xz: $i] :
          ( ( in @ Xz @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) )
         => ( ( Xz = Xx )
            | ( Xz = Xy ) ) ) ) )).

thf(upairsetIL_type,type,(
    upairsetIL: $o )).

thf(upairsetIL,definition,
    ( upairsetIL
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xx @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(upairsetIR_type,type,(
    upairsetIR: $o )).

thf(upairsetIR,definition,
    ( upairsetIR
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xy @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(emptyE1_type,type,(
    emptyE1: $o )).

thf(emptyE1,definition,
    ( emptyE1
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ( Xphi @ Xx ) )
         => ( ( ( dsetconstr @ A
                @ ^ [Xx: $i] :
                    ( Xphi @ Xx ) )
              = emptyset )
           => $false ) ) ) )).

thf(vacuousDall_type,type,(
    vacuousDall: $o )).

thf(vacuousDall,definition,
    ( vacuousDall
    = ( ! [Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ emptyset )
         => ( Xphi @ Xx ) ) ) )).

thf(quantDeMorgan1_type,type,(
    quantDeMorgan1: $o )).

thf(quantDeMorgan1,definition,
    ( quantDeMorgan1
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ~ ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( Xphi @ Xx ) ) )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ~ ( Xphi @ Xx ) ) ) ) )).

thf(quantDeMorgan2_type,type,(
    quantDeMorgan2: $o )).

thf(quantDeMorgan2,definition,
    ( quantDeMorgan2
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ~ ( Xphi @ Xx ) )
         => ~ ( ? [Xx: $i] :
                  ( ( in @ Xx @ A )
                  & ( Xphi @ Xx ) ) ) ) ) )).

thf(quantDeMorgan3_type,type,(
    quantDeMorgan3: $o )).

thf(quantDeMorgan3,definition,
    ( quantDeMorgan3
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ~ ( ? [Xx: $i] :
                  ( ( in @ Xx @ A )
                  & ( Xphi @ Xx ) ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ~ ( Xphi @ Xx ) ) ) ) )).

thf(quantDeMorgan4_type,type,(
    quantDeMorgan4: $o )).

thf(quantDeMorgan4,definition,
    ( quantDeMorgan4
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ~ ( Xphi @ Xx ) )
         => ~ ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( Xphi @ Xx ) ) ) ) ) )).

thf(prop2setI_type,type,(
    prop2setI: $o )).

thf(prop2setI,definition,
    ( prop2setI
    = ( ! [Xphi: $o] :
          ( Xphi
         => ( in @ emptyset @ ( prop2set @ Xphi ) ) ) ) )).

thf(set2prop_type,type,(
    set2prop: $i > $o )).

thf(prop2set2propI_type,type,(
    prop2set2propI: $o )).

thf(prop2set2propI,definition,
    ( prop2set2propI
    = ( ! [Xphi: $o] :
          ( Xphi
         => ( set2prop @ ( prop2set @ Xphi ) ) ) ) )).

thf(notdexE_type,type,(
    notdexE: $o )).

thf(notdexE,definition,
    ( notdexE
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ~ ( ? [Xx: $i] :
                  ( ( in @ Xx @ A )
                  & ( Xphi @ Xx ) ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ~ ( Xphi @ Xx ) ) ) ) )).

thf(notdallE_type,type,(
    notdallE: $o )).

thf(notdallE,definition,
    ( notdallE
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ~ ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ( Xphi @ Xx ) ) )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ~ ( Xphi @ Xx ) ) ) ) )).

thf(exuI1_type,type,(
    exuI1: $o )).

thf(exuI1,definition,
    ( exuI1
    = ( ! [Xphi: $i > $o] :
          ( ? [Xx: $i] :
              ( ( Xphi @ Xx )
              & ! [Xy: $i] :
                  ( ( Xphi @ Xy )
                 => ( Xx = Xy ) ) )
         => ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) ) ) ) )).

thf(exuI3_type,type,(
    exuI3: $o )).

thf(exuI3,definition,
    ( exuI3
    = ( ! [Xphi: $i > $o] :
          ( ? [Xx: $i] :
              ( Xphi @ Xx )
         => ( ! [Xx: $i,Xy: $i] :
                ( ( Xphi @ Xx )
               => ( ( Xphi @ Xy )
                 => ( Xx = Xy ) ) )
           => ( exu
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) ) ) ) ) )).

thf(exuI2_type,type,(
    exuI2: $o )).

thf(exuI2,definition,
    ( exuI2
    = ( ! [Xphi: $i > $o] :
          ( ? [Xx: $i] :
            ! [Xy: $i] :
              ( ( Xphi @ Xy )
            <=> ( Xy = Xx ) )
         => ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) ) ) ) )).

thf(inCongP_type,type,(
    inCongP: $o )).

thf(inCongP,definition,
    ( inCongP
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ! [Xx: $i,Xy: $i] :
              ( ( Xx = Xy )
             => ( ( in @ Xx @ A )
               => ( in @ Xy @ B ) ) ) ) ) )).

thf(in__Cong_type,type,(
    in__Cong: $o )).

thf(in__Cong,definition,
    ( in__Cong
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ! [Xx: $i,Xy: $i] :
              ( ( Xx = Xy )
             => ( ( in @ Xx @ A )
              <=> ( in @ Xy @ B ) ) ) ) ) )).

thf(exuE3u_type,type,(
    exuE3u: $o )).

thf(exuE3u,definition,
    ( exuE3u
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ! [Xx: $i,Xy: $i] :
              ( ( Xphi @ Xx )
             => ( ( Xphi @ Xy )
               => ( Xx = Xy ) ) ) ) ) )).

thf(exu__Cong_type,type,(
    exu__Cong: $o )).

thf(exu__Cong,definition,
    ( exu__Cong
    = ( ! [Xphi: $i > $o,Xpsi: $i > $o] :
          ( ! [Xx: $i,Xy: $i] :
              ( ( Xx = Xy )
             => ( ( Xphi @ Xx )
              <=> ( Xpsi @ Xy ) ) )
         => ( ( exu
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) )
          <=> ( exu
              @ ^ [Xx: $i] :
                  ( Xpsi @ Xx ) ) ) ) ) )).

thf(emptyset__Cong_type,type,(
    emptyset__Cong: $o )).

thf(emptyset__Cong,definition,
    ( emptyset__Cong
    = ( emptyset = emptyset ) )).

thf(setadjoin__Cong_type,type,(
    setadjoin__Cong: $o )).

thf(setadjoin__Cong,definition,
    ( setadjoin__Cong
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx = Xy )
         => ! [Xz: $i,Xu: $i] :
              ( ( Xz = Xu )
             => ( ( setadjoin @ Xx @ Xz )
                = ( setadjoin @ Xy @ Xu ) ) ) ) ) )).

thf(powerset__Cong_type,type,(
    powerset__Cong: $o )).

thf(powerset__Cong,definition,
    ( powerset__Cong
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ( ( powerset @ A )
            = ( powerset @ B ) ) ) ) )).

thf(setunion__Cong_type,type,(
    setunion__Cong: $o )).

thf(setunion__Cong,definition,
    ( setunion__Cong
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ( ( setunion @ A )
            = ( setunion @ B ) ) ) ) )).

thf(omega__Cong_type,type,(
    omega__Cong: $o )).

thf(omega__Cong,definition,
    ( omega__Cong
    = ( omega = omega ) )).

thf(exuEu_type,type,(
    exuEu: $o )).

thf(exuEu,definition,
    ( exuEu
    = ( ! [Xphi: $i > $o] :
          ( ( exu
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ! [Xx: $i,Xy: $i] :
              ( ( Xphi @ Xx )
             => ( ( Xphi @ Xy )
               => ( Xx = Xy ) ) ) ) ) )).

thf(descr__Cong_type,type,(
    descr__Cong: $o )).

thf(descr__Cong,definition,
    ( descr__Cong
    = ( ! [Xphi: $i > $o,Xpsi: $i > $o] :
          ( ! [Xx: $i,Xy: $i] :
              ( ( Xx = Xy )
             => ( ( Xphi @ Xx )
              <=> ( Xpsi @ Xy ) ) )
         => ( ( exu
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) )
           => ( ( exu
                @ ^ [Xx: $i] :
                    ( Xpsi @ Xx ) )
             => ( ( descr
                  @ ^ [Xx: $i] :
                      ( Xphi @ Xx ) )
                = ( descr
                  @ ^ [Xx: $i] :
                      ( Xpsi @ Xx ) ) ) ) ) ) ) )).

thf(dsetconstr__Cong_type,type,(
    dsetconstr__Cong: $o )).

thf(dsetconstr__Cong,definition,
    ( dsetconstr__Cong
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ! [Xphi: $i > $o,Xpsi: $i > $o] :
              ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ B )
                     => ( ( Xx = Xy )
                       => ( ( Xphi @ Xx )
                        <=> ( Xpsi @ Xy ) ) ) ) )
             => ( ( dsetconstr @ A
                  @ ^ [Xx: $i] :
                      ( Xphi @ Xx ) )
                = ( dsetconstr @ B
                  @ ^ [Xx: $i] :
                      ( Xpsi @ Xx ) ) ) ) ) ) )).

thf(subset_type,type,(
    subset: $i > $i > $o )).

thf(disjoint_type,type,(
    disjoint: $i > $i > $o )).

thf(setsmeet_type,type,(
    setsmeet: $i > $i > $o )).

thf(subsetI1_type,type,(
    subsetI1: $o )).

thf(subsetI1,definition,
    ( subsetI1
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ Xx @ B ) )
         => ( subset @ A @ B ) ) ) )).

thf(eqimpsubset2_type,type,(
    eqimpsubset2: $o )).

thf(eqimpsubset2,definition,
    ( eqimpsubset2
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ( subset @ B @ A ) ) ) )).

thf(eqimpsubset1_type,type,(
    eqimpsubset1: $o )).

thf(eqimpsubset1,definition,
    ( eqimpsubset1
    = ( ! [A: $i,B: $i] :
          ( ( A = B )
         => ( subset @ A @ B ) ) ) )).

thf(subsetI2_type,type,(
    subsetI2: $o )).

thf(subsetI2,definition,
    ( subsetI2
    = ( ! [A: $i,B: $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ Xx @ B ) )
         => ( subset @ A @ B ) ) ) )).

thf(emptysetsubset_type,type,(
    emptysetsubset: $o )).

thf(emptysetsubset,definition,
    ( emptysetsubset
    = ( ! [A: $i] :
          ( subset @ emptyset @ A ) ) )).

thf(subsetE_type,type,(
    subsetE: $o )).

thf(subsetE,definition,
    ( subsetE
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( subset @ A @ B )
         => ( ( in @ Xx @ A )
           => ( in @ Xx @ B ) ) ) ) )).

thf(subsetE2_type,type,(
    subsetE2: $o )).

thf(subsetE2,definition,
    ( subsetE2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( subset @ A @ B )
         => ( ~ ( in @ Xx @ B )
           => ~ ( in @ Xx @ A ) ) ) ) )).

thf(notsubsetI_type,type,(
    notsubsetI: $o )).

thf(notsubsetI,definition,
    ( notsubsetI
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ~ ( subset @ A @ B ) ) ) ) )).

thf(notequalI1_type,type,(
    notequalI1: $o )).

thf(notequalI1,definition,
    ( notequalI1
    = ( ! [A: $i,B: $i] :
          ( ~ ( subset @ A @ B )
         => ( A != B ) ) ) )).

thf(notequalI2_type,type,(
    notequalI2: $o )).

thf(notequalI2,definition,
    ( notequalI2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ( A != B ) ) ) ) )).

thf(subsetRefl_type,type,(
    subsetRefl: $o )).

thf(subsetRefl,definition,
    ( subsetRefl
    = ( ! [A: $i] :
          ( subset @ A @ A ) ) )).

thf(subsetTrans_type,type,(
    subsetTrans: $o )).

thf(subsetTrans,definition,
    ( subsetTrans
    = ( ! [A: $i,B: $i,C: $i] :
          ( ( subset @ A @ B )
         => ( ( subset @ B @ C )
           => ( subset @ A @ C ) ) ) ) )).

thf(setadjoinSub_type,type,(
    setadjoinSub: $o )).

thf(setadjoinSub,definition,
    ( setadjoinSub
    = ( ! [Xx: $i,A: $i] :
          ( subset @ A @ ( setadjoin @ Xx @ A ) ) ) )).

thf(setadjoinSub2_type,type,(
    setadjoinSub2: $o )).

thf(setadjoinSub2,definition,
    ( setadjoinSub2
    = ( ! [A: $i,Xx: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( subset @ A @ ( setadjoin @ Xx @ B ) ) ) ) )).

thf(subset2powerset_type,type,(
    subset2powerset: $o )).

thf(subset2powerset,definition,
    ( subset2powerset
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( in @ A @ ( powerset @ B ) ) ) ) )).

thf(setextsub_type,type,(
    setextsub: $o )).

thf(setextsub,definition,
    ( setextsub
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( ( subset @ B @ A )
           => ( A = B ) ) ) ) )).

thf(subsetemptysetimpeq_type,type,(
    subsetemptysetimpeq: $o )).

thf(subsetemptysetimpeq,definition,
    ( subsetemptysetimpeq
    = ( ! [A: $i] :
          ( ( subset @ A @ emptyset )
         => ( A = emptyset ) ) ) )).

thf(powersetI1_type,type,(
    powersetI1: $o )).

thf(powersetI1,definition,
    ( powersetI1
    = ( ! [A: $i,B: $i] :
          ( ( subset @ B @ A )
         => ( in @ B @ ( powerset @ A ) ) ) ) )).

thf(powersetE1_type,type,(
    powersetE1: $o )).

thf(powersetE1,definition,
    ( powersetE1
    = ( ! [A: $i,B: $i] :
          ( ( in @ B @ ( powerset @ A ) )
         => ( subset @ B @ A ) ) ) )).

thf(inPowerset_type,type,(
    inPowerset: $o )).

thf(inPowerset,definition,
    ( inPowerset
    = ( ! [A: $i] :
          ( in @ A @ ( powerset @ A ) ) ) )).

thf(powersetsubset_type,type,(
    powersetsubset: $o )).

thf(powersetsubset,definition,
    ( powersetsubset
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( subset @ ( powerset @ A ) @ ( powerset @ B ) ) ) ) )).

thf(sepInPowerset_type,type,(
    sepInPowerset: $o )).

thf(sepInPowerset,definition,
    ( sepInPowerset
    = ( ! [A: $i,Xphi: $i > $o] :
          ( in
          @ ( dsetconstr @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
          @ ( powerset @ A ) ) ) )).

thf(sepSubset_type,type,(
    sepSubset: $o )).

thf(sepSubset,definition,
    ( sepSubset
    = ( ! [A: $i,Xphi: $i > $o] :
          ( subset
          @ ( dsetconstr @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
          @ A ) ) )).

thf(binunion_type,type,(
    binunion: $i > $i > $i )).

thf(binunionIL_type,type,(
    binunionIL: $o )).

thf(binunionIL,definition,
    ( binunionIL
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( in @ Xx @ ( binunion @ A @ B ) ) ) ) )).

thf(upairset2IR_type,type,(
    upairset2IR: $o )).

thf(upairset2IR,definition,
    ( upairset2IR
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xy @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(binunionIR_type,type,(
    binunionIR: $o )).

thf(binunionIR,definition,
    ( binunionIR
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ B )
         => ( in @ Xx @ ( binunion @ A @ B ) ) ) ) )).

thf(binunionEcases_type,type,(
    binunionEcases: $o )).

thf(binunionEcases,definition,
    ( binunionEcases
    = ( ! [A: $i,B: $i,Xx: $i,Xphi: $o] :
          ( ( in @ Xx @ ( binunion @ A @ B ) )
         => ( ( ( in @ Xx @ A )
             => Xphi )
           => ( ( ( in @ Xx @ B )
               => Xphi )
             => Xphi ) ) ) ) )).

thf(binunionE_type,type,(
    binunionE: $o )).

thf(binunionE,definition,
    ( binunionE
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( binunion @ A @ B ) )
         => ( ( in @ Xx @ A )
            | ( in @ Xx @ B ) ) ) ) )).

thf(binunionLsub_type,type,(
    binunionLsub: $o )).

thf(binunionLsub,definition,
    ( binunionLsub
    = ( ! [A: $i,B: $i] :
          ( subset @ A @ ( binunion @ A @ B ) ) ) )).

thf(binunionRsub_type,type,(
    binunionRsub: $o )).

thf(binunionRsub,definition,
    ( binunionRsub
    = ( ! [A: $i,B: $i] :
          ( subset @ B @ ( binunion @ A @ B ) ) ) )).

thf(binintersect_type,type,(
    binintersect: $i > $i > $i )).

thf(binintersectI_type,type,(
    binintersectI: $o )).

thf(binintersectI,definition,
    ( binintersectI
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( in @ Xx @ B )
           => ( in @ Xx @ ( binintersect @ A @ B ) ) ) ) ) )).

thf(binintersectSubset5_type,type,(
    binintersectSubset5: $o )).

thf(binintersectSubset5,definition,
    ( binintersectSubset5
    = ( ! [A: $i,B: $i,C: $i] :
          ( ( subset @ C @ A )
         => ( ( subset @ C @ B )
           => ( subset @ C @ ( binintersect @ A @ B ) ) ) ) ) )).

thf(binintersectEL_type,type,(
    binintersectEL: $o )).

thf(binintersectEL,definition,
    ( binintersectEL
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( binintersect @ A @ B ) )
         => ( in @ Xx @ A ) ) ) )).

thf(binintersectLsub_type,type,(
    binintersectLsub: $o )).

thf(binintersectLsub,definition,
    ( binintersectLsub
    = ( ! [A: $i,B: $i] :
          ( subset @ ( binintersect @ A @ B ) @ A ) ) )).

thf(binintersectSubset2_type,type,(
    binintersectSubset2: $o )).

thf(binintersectSubset2,definition,
    ( binintersectSubset2
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( ( binintersect @ A @ B )
            = A ) ) ) )).

thf(binintersectSubset3_type,type,(
    binintersectSubset3: $o )).

thf(binintersectSubset3,definition,
    ( binintersectSubset3
    = ( ! [A: $i,B: $i] :
          ( ( ( binintersect @ A @ B )
            = B )
         => ( subset @ B @ A ) ) ) )).

thf(binintersectER_type,type,(
    binintersectER: $o )).

thf(binintersectER,definition,
    ( binintersectER
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( binintersect @ A @ B ) )
         => ( in @ Xx @ B ) ) ) )).

thf(disjointsetsI1_type,type,(
    disjointsetsI1: $o )).

thf(disjointsetsI1,definition,
    ( disjointsetsI1
    = ( ! [A: $i,B: $i] :
          ( ~ ( ? [Xx: $i] :
                  ( ( in @ Xx @ A )
                  & ( in @ Xx @ B ) ) )
         => ( ( binintersect @ A @ B )
            = emptyset ) ) ) )).

thf(binintersectRsub_type,type,(
    binintersectRsub: $o )).

thf(binintersectRsub,definition,
    ( binintersectRsub
    = ( ! [A: $i,B: $i] :
          ( subset @ ( binintersect @ A @ B ) @ B ) ) )).

thf(binintersectSubset4_type,type,(
    binintersectSubset4: $o )).

thf(binintersectSubset4,definition,
    ( binintersectSubset4
    = ( ! [A: $i,B: $i] :
          ( ( subset @ B @ A )
         => ( ( binintersect @ A @ B )
            = B ) ) ) )).

thf(binintersectSubset1_type,type,(
    binintersectSubset1: $o )).

thf(binintersectSubset1,definition,
    ( binintersectSubset1
    = ( ! [A: $i,B: $i] :
          ( ( ( binintersect @ A @ B )
            = A )
         => ( subset @ A @ B ) ) ) )).

thf(bs114d_type,type,(
    bs114d: $o )).

thf(bs114d,definition,
    ( bs114d
    = ( ! [A: $i,B: $i,C: $i] :
          ( ( binintersect @ A @ ( binunion @ B @ C ) )
          = ( binunion @ ( binintersect @ A @ B ) @ ( binintersect @ A @ C ) ) ) ) )).

thf(regular_type,type,(
    regular: $i > $o )).

thf(setminus_type,type,(
    setminus: $i > $i > $i )).

thf(setminusI_type,type,(
    setminusI: $o )).

thf(setminusI,definition,
    ( setminusI
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ( in @ Xx @ ( setminus @ A @ B ) ) ) ) ) )).

thf(setminusEL_type,type,(
    setminusEL: $o )).

thf(setminusEL,definition,
    ( setminusEL
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( setminus @ A @ B ) )
         => ( in @ Xx @ A ) ) ) )).

thf(setminusER_type,type,(
    setminusER: $o )).

thf(setminusER,definition,
    ( setminusER
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( setminus @ A @ B ) )
         => ~ ( in @ Xx @ B ) ) ) )).

thf(setminusSubset2_type,type,(
    setminusSubset2: $o )).

thf(setminusSubset2,definition,
    ( setminusSubset2
    = ( ! [A: $i,B: $i] :
          ( ( subset @ A @ B )
         => ( ( setminus @ A @ B )
            = emptyset ) ) ) )).

thf(setminusERneg_type,type,(
    setminusERneg: $o )).

thf(setminusERneg,definition,
    ( setminusERneg
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ ( setminus @ A @ B ) )
         => ( ( in @ Xx @ A )
           => ( in @ Xx @ B ) ) ) ) )).

thf(setminusELneg_type,type,(
    setminusELneg: $o )).

thf(setminusELneg,definition,
    ( setminusELneg
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ ( setminus @ A @ B ) )
         => ( ~ ( in @ Xx @ B )
           => ~ ( in @ Xx @ A ) ) ) ) )).

thf(setminusILneg_type,type,(
    setminusILneg: $o )).

thf(setminusILneg,definition,
    ( setminusILneg
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ A )
         => ~ ( in @ Xx @ ( setminus @ A @ B ) ) ) ) )).

thf(setminusIRneg_type,type,(
    setminusIRneg: $o )).

thf(setminusIRneg,definition,
    ( setminusIRneg
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ B )
         => ~ ( in @ Xx @ ( setminus @ A @ B ) ) ) ) )).

thf(setminusLsub_type,type,(
    setminusLsub: $o )).

thf(setminusLsub,definition,
    ( setminusLsub
    = ( ! [A: $i,B: $i] :
          ( subset @ ( setminus @ A @ B ) @ A ) ) )).

thf(setminusSubset1_type,type,(
    setminusSubset1: $o )).

thf(setminusSubset1,definition,
    ( setminusSubset1
    = ( ! [A: $i,B: $i] :
          ( ( ( setminus @ A @ B )
            = emptyset )
         => ( subset @ A @ B ) ) ) )).

thf(symdiff_type,type,(
    symdiff: $i > $i > $i )).

thf(symdiffE_type,type,(
    symdiffE: $o )).

thf(symdiffE,definition,
    ( symdiffE
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ ( symdiff @ A @ B ) )
         => ! [Xphi: $o] :
              ( ( ( in @ Xx @ A )
               => ( ~ ( in @ Xx @ B )
                 => Xphi ) )
             => ( ( ~ ( in @ Xx @ A )
                 => ( ( in @ Xx @ B )
                   => Xphi ) )
               => Xphi ) ) ) ) )).

thf(symdiffI1_type,type,(
    symdiffI1: $o )).

thf(symdiffI1,definition,
    ( symdiffI1
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ( in @ Xx @ ( symdiff @ A @ B ) ) ) ) ) )).

thf(symdiffI2_type,type,(
    symdiffI2: $o )).

thf(symdiffI2,definition,
    ( symdiffI2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ A )
         => ( ( in @ Xx @ B )
           => ( in @ Xx @ ( symdiff @ A @ B ) ) ) ) ) )).

thf(symdiffIneg1_type,type,(
    symdiffIneg1: $o )).

thf(symdiffIneg1,definition,
    ( symdiffIneg1
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( in @ Xx @ B )
           => ~ ( in @ Xx @ ( symdiff @ A @ B ) ) ) ) ) )).

thf(symdiffIneg2_type,type,(
    symdiffIneg2: $o )).

thf(symdiffIneg2,definition,
    ( symdiffIneg2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ~ ( in @ Xx @ A )
         => ( ~ ( in @ Xx @ B )
           => ~ ( in @ Xx @ ( symdiff @ A @ B ) ) ) ) ) )).

thf(iskpair_type,type,(
    iskpair: $i > $o )).

thf(secondinupair_type,type,(
    secondinupair: $o )).

thf(secondinupair,definition,
    ( secondinupair
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xy @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) ) ) )).

thf(setukpairIL_type,type,(
    setukpairIL: $o )).

thf(setukpairIL,definition,
    ( setukpairIL
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xx @ ( setunion @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) ) ) ) )).

thf(setukpairIR_type,type,(
    setukpairIR: $o )).

thf(setukpairIR,definition,
    ( setukpairIR
    = ( ! [Xx: $i,Xy: $i] :
          ( in @ Xy @ ( setunion @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) ) ) ) )).

thf(kpairiskpair_type,type,(
    kpairiskpair: $o )).

thf(kpairiskpair,definition,
    ( kpairiskpair
    = ( ! [Xx: $i,Xy: $i] :
          ( iskpair @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) ) ) )).

thf(kpair_type,type,(
    kpair: $i > $i > $i )).

thf(kpairp_type,type,(
    kpairp: $o )).

thf(kpairp,definition,
    ( kpairp
    = ( ! [Xx: $i,Xy: $i] :
          ( iskpair @ ( kpair @ Xx @ Xy ) ) ) )).

thf(cartprod_type,type,(
    cartprod: $i > $i > $i )).

thf(singletonsubset_type,type,(
    singletonsubset: $o )).

thf(singletonsubset,definition,
    ( singletonsubset
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( subset @ ( setadjoin @ Xx @ emptyset ) @ A ) ) ) )).

thf(singletoninpowerset_type,type,(
    singletoninpowerset: $o )).

thf(singletoninpowerset,definition,
    ( singletoninpowerset
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( in @ ( setadjoin @ Xx @ emptyset ) @ ( powerset @ A ) ) ) ) )).

thf(singletoninpowunion_type,type,(
    singletoninpowunion: $o )).

thf(singletoninpowunion,definition,
    ( singletoninpowunion
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( in @ ( setadjoin @ Xx @ emptyset ) @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) )).

thf(upairset2E_type,type,(
    upairset2E: $o )).

thf(upairset2E,definition,
    ( upairset2E
    = ( ! [Xx: $i,Xy: $i,Xz: $i] :
          ( ( in @ Xz @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) )
         => ( ( Xz = Xx )
            | ( Xz = Xy ) ) ) ) )).

thf(upairsubunion_type,type,(
    upairsubunion: $o )).

thf(upairsubunion,definition,
    ( upairsubunion
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( subset @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ ( binunion @ A @ B ) ) ) ) ) )).

thf(upairinpowunion_type,type,(
    upairinpowunion: $o )).

thf(upairinpowunion,definition,
    ( upairinpowunion
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( in @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) ) )).

thf(ubforcartprodlem1_type,type,(
    ubforcartprodlem1: $o )).

thf(ubforcartprodlem1,definition,
    ( ubforcartprodlem1
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( subset @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) ) )).

thf(ubforcartprodlem2_type,type,(
    ubforcartprodlem2: $o )).

thf(ubforcartprodlem2,definition,
    ( ubforcartprodlem2
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( in @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) @ ( powerset @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) ) ) )).

thf(ubforcartprodlem3_type,type,(
    ubforcartprodlem3: $o )).

thf(ubforcartprodlem3,definition,
    ( ubforcartprodlem3
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( in @ ( kpair @ Xx @ Xy ) @ ( powerset @ ( powerset @ ( binunion @ A @ B ) ) ) ) ) ) ) )).

thf(cartprodpairin_type,type,(
    cartprodpairin: $o )).

thf(cartprodpairin,definition,
    ( cartprodpairin
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( in @ ( kpair @ Xx @ Xy ) @ ( cartprod @ A @ B ) ) ) ) ) )).

thf(cartprodmempair1_type,type,(
    cartprodmempair1: $o )).

thf(cartprodmempair1,definition,
    ( cartprodmempair1
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ? [Xy: $i] :
                  ( ( in @ Xy @ B )
                  & ( Xu
                    = ( kpair @ Xx @ Xy ) ) ) ) ) ) )).

thf(cartprodmempair_type,type,(
    cartprodmempair: $o )).

thf(cartprodmempair,definition,
    ( cartprodmempair
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ( iskpair @ Xu ) ) ) )).

thf(setunionE2_type,type,(
    setunionE2: $o )).

thf(setunionE2,definition,
    ( setunionE2
    = ( ! [A: $i,Xx: $i] :
          ( ( in @ Xx @ ( setunion @ A ) )
         => ? [X: $i] :
              ( ( in @ X @ A )
              & ( in @ Xx @ X ) ) ) ) )).

thf(setunionsingleton1_type,type,(
    setunionsingleton1: $o )).

thf(setunionsingleton1,definition,
    ( setunionsingleton1
    = ( ! [A: $i] :
          ( subset @ ( setunion @ ( setadjoin @ A @ emptyset ) ) @ A ) ) )).

thf(setunionsingleton2_type,type,(
    setunionsingleton2: $o )).

thf(setunionsingleton2,definition,
    ( setunionsingleton2
    = ( ! [A: $i] :
          ( subset @ A @ ( setunion @ ( setadjoin @ A @ emptyset ) ) ) ) )).

thf(setunionsingleton_type,type,(
    setunionsingleton: $o )).

thf(setunionsingleton,definition,
    ( setunionsingleton
    = ( ! [Xx: $i] :
          ( ( setunion @ ( setadjoin @ Xx @ emptyset ) )
          = Xx ) ) )).

thf(singleton_type,type,(
    singleton: $i > $o )).

thf(singleton,definition,
    ( singleton
    = ( ^ [A: $i] :
        ? [Xx: $i] :
          ( ( in @ Xx @ A )
          & ( A
            = ( setadjoin @ Xx @ emptyset ) ) ) ) )).

thf(singletonprop_type,type,(
    singletonprop: $o )).

thf(singletonprop,definition,
    ( singletonprop
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( Xphi @ Xx )
                   => ( ( Xphi @ Xy )
                     => ( Xx = Xy ) ) ) ) )
         => ( ? [Xx: $i] :
                ( ( in @ Xx @ A )
                & ( Xphi @ Xx ) )
           => ( singleton
              @ ( dsetconstr @ A
                @ ^ [Xx: $i] :
                    ( Xphi @ Xx ) ) ) ) ) ) )).

thf(ex1_type,type,(
    ex1: $i > ( $i > $o ) > $o )).

thf(ex1,definition,
    ( ex1
    = ( ^ [A: $i,Xphi: $i > $o] :
          ( singleton
          @ ( dsetconstr @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) ) ) ) )).

thf(ex1E1_type,type,(
    ex1E1: $o )).

thf(ex1E1,definition,
    ( ex1E1
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ( ex1 @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ? [Xx: $i] :
              ( ( in @ Xx @ A )
              & ( Xphi @ Xx ) ) ) ) )).

thf(ex1I_type,type,(
    ex1I: $o )).

thf(ex1I,definition,
    ( ex1I
    = ( ! [A: $i,Xphi: $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ( ( Xphi @ Xx )
           => ( ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( Xphi @ Xy )
                   => ( Xy = Xx ) ) )
             => ( ex1 @ A
                @ ^ [Xy: $i] :
                    ( Xphi @ Xy ) ) ) ) ) ) )).

thf(ex1I2_type,type,(
    ex1I2: $o )).

thf(ex1I2,definition,
    ( ex1I2
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( Xphi @ Xx )
                   => ( ( Xphi @ Xy )
                     => ( Xx = Xy ) ) ) ) )
         => ( ? [Xx: $i] :
                ( ( in @ Xx @ A )
                & ( Xphi @ Xx ) )
           => ( ex1 @ A
              @ ^ [Xx: $i] :
                  ( Xphi @ Xx ) ) ) ) ) )).

thf(singletonsuniq_type,type,(
    singletonsuniq: $o )).

thf(singletonsuniq,definition,
    ( singletonsuniq
    = ( ! [Xx: $i,Xy: $i] :
          ( ( ( setadjoin @ Xx @ emptyset )
            = ( setadjoin @ Xy @ emptyset ) )
         => ( Xx = Xy ) ) ) )).

thf(atmost1p_type,type,(
    atmost1p: $i > $o )).

thf(atleast2p_type,type,(
    atleast2p: $i > $o )).

thf(atmost2p_type,type,(
    atmost2p: $i > $o )).

thf(upairsetp_type,type,(
    upairsetp: $i > $o )).

thf(setukpairinjL1_type,type,(
    setukpairinjL1: $o )).

thf(setukpairinjL1,definition,
    ( setukpairinjL1
    = ( ! [Xx: $i,Xy: $i,Xz: $i] :
          ( ( in @ ( setadjoin @ Xz @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) ) )
         => ( Xx = Xz ) ) ) )).

thf(kfstsingleton_type,type,(
    kfstsingleton: $o )).

thf(kfstsingleton,definition,
    ( kfstsingleton
    = ( ! [Xu: $i] :
          ( ( iskpair @ Xu )
         => ( singleton
            @ ( dsetconstr @ ( setunion @ Xu )
              @ ^ [Xx: $i] :
                  ( in @ ( setadjoin @ Xx @ emptyset ) @ Xu ) ) ) ) ) )).

thf(theprop_type,type,(
    theprop: $o )).

thf(theprop,definition,
    ( theprop
    = ( ! [X: $i] :
          ( ( singleton @ X )
         => ( in @ ( setunion @ X ) @ X ) ) ) )).

thf(kfst_type,type,(
    kfst: $i > $i )).

thf(kfstpairEq_type,type,(
    kfstpairEq: $o )).

thf(kfstpairEq,definition,
    ( kfstpairEq
    = ( ! [Xx: $i,Xy: $i] :
          ( ( kfst @ ( kpair @ Xx @ Xy ) )
          = Xx ) ) )).

thf(cartprodfstin_type,type,(
    cartprodfstin: $o )).

thf(cartprodfstin,definition,
    ( cartprodfstin
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ( in @ ( kfst @ Xu ) @ A ) ) ) )).

thf(setukpairinjL2_type,type,(
    setukpairinjL2: $o )).

thf(setukpairinjL2,definition,
    ( setukpairinjL2
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) )
            = ( setadjoin @ ( setadjoin @ Xz @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xz @ ( setadjoin @ Xu @ emptyset ) ) @ emptyset ) ) )
         => ( Xx = Xz ) ) ) )).

thf(setukpairinjL_type,type,(
    setukpairinjL: $o )).

thf(setukpairinjL,definition,
    ( setukpairinjL
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( kpair @ Xx @ Xy )
            = ( kpair @ Xz @ Xu ) )
         => ( Xx = Xz ) ) ) )).

thf(setukpairinjR11_type,type,(
    setukpairinjR11: $o )).

thf(setukpairinjR11,definition,
    ( setukpairinjR11
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx = Xy )
         => ( ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) )
            = ( setadjoin @ Xx @ emptyset ) ) ) ) )).

thf(setukpairinjR12_type,type,(
    setukpairinjR12: $o )).

thf(setukpairinjR12,definition,
    ( setukpairinjR12
    = ( ! [Xx: $i,Xy: $i] :
          ( ( Xx = Xy )
         => ( ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) )
            = ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ emptyset ) ) ) ) )).

thf(setukpairinjR1_type,type,(
    setukpairinjR1: $o )).

thf(setukpairinjR1,definition,
    ( setukpairinjR1
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) )
            = ( setadjoin @ ( setadjoin @ Xz @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xz @ ( setadjoin @ Xu @ emptyset ) ) @ emptyset ) ) )
         => ( ( Xz = Xu )
           => ( Xy = Xu ) ) ) ) )).

thf(upairequniteq_type,type,(
    upairequniteq: $o )).

thf(upairequniteq,definition,
    ( upairequniteq
    = ( ! [Xx: $i,Xy: $i,Xz: $i] :
          ( ( ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) )
            = ( setadjoin @ Xz @ emptyset ) )
         => ( Xx = Xy ) ) ) )).

thf(setukpairinjR2_type,type,(
    setukpairinjR2: $o )).

thf(setukpairinjR2,definition,
    ( setukpairinjR2
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( setadjoin @ ( setadjoin @ Xx @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xx @ ( setadjoin @ Xy @ emptyset ) ) @ emptyset ) )
            = ( setadjoin @ ( setadjoin @ Xz @ emptyset ) @ ( setadjoin @ ( setadjoin @ Xz @ ( setadjoin @ Xu @ emptyset ) ) @ emptyset ) ) )
         => ( Xy = Xu ) ) ) )).

thf(setukpairinjR_type,type,(
    setukpairinjR: $o )).

thf(setukpairinjR,definition,
    ( setukpairinjR
    = ( ! [Xx: $i,Xy: $i,Xz: $i,Xu: $i] :
          ( ( ( kpair @ Xx @ Xy )
            = ( kpair @ Xz @ Xu ) )
         => ( Xy = Xu ) ) ) )).

thf(ksndsingleton_type,type,(
    ksndsingleton: $o )).

thf(ksndsingleton,definition,
    ( ksndsingleton
    = ( ! [Xu: $i] :
          ( ( iskpair @ Xu )
         => ( singleton
            @ ( dsetconstr @ ( setunion @ Xu )
              @ ^ [Xx: $i] :
                  ( Xu
                  = ( kpair @ ( kfst @ Xu ) @ Xx ) ) ) ) ) ) )).

thf(ksnd_type,type,(
    ksnd: $i > $i )).

thf(ksndpairEq_type,type,(
    ksndpairEq: $o )).

thf(ksndpairEq,definition,
    ( ksndpairEq
    = ( ! [Xx: $i,Xy: $i] :
          ( ( ksnd @ ( kpair @ Xx @ Xy ) )
          = Xy ) ) )).

thf(kpairsurjEq_type,type,(
    kpairsurjEq: $o )).

thf(kpairsurjEq,definition,
    ( kpairsurjEq
    = ( ! [Xu: $i] :
          ( ( iskpair @ Xu )
         => ( ( kpair @ ( kfst @ Xu ) @ ( ksnd @ Xu ) )
            = Xu ) ) ) )).

thf(cartprodsndin_type,type,(
    cartprodsndin: $o )).

thf(cartprodsndin,definition,
    ( cartprodsndin
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ( in @ ( ksnd @ Xu ) @ B ) ) ) )).

thf(cartprodpairmemEL_type,type,(
    cartprodpairmemEL: $o )).

thf(cartprodpairmemEL,definition,
    ( cartprodpairmemEL
    = ( ! [A: $i,B: $i,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy ) @ ( cartprod @ A @ B ) )
         => ( in @ Xx @ A ) ) ) )).

thf(cartprodpairmemER_type,type,(
    cartprodpairmemER: $o )).

thf(cartprodpairmemER,definition,
    ( cartprodpairmemER
    = ( ! [A: $i,B: $i,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy ) @ ( cartprod @ A @ B ) )
         => ( in @ Xy @ B ) ) ) )).

thf(cartprodmempaircEq_type,type,(
    cartprodmempaircEq: $o )).

thf(cartprodmempaircEq,definition,
    ( cartprodmempaircEq
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( kpair @ Xx @ Xy )
                = ( kpair @ Xx @ Xy ) ) ) ) ) )).

thf(cartprodfstpairEq_type,type,(
    cartprodfstpairEq: $o )).

thf(cartprodfstpairEq,definition,
    ( cartprodfstpairEq
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( kfst @ ( kpair @ Xx @ Xy ) )
                = Xx ) ) ) ) )).

thf(cartprodsndpairEq_type,type,(
    cartprodsndpairEq: $o )).

thf(cartprodsndpairEq,definition,
    ( cartprodsndpairEq
    = ( ! [A: $i,B: $i,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( ksnd @ ( kpair @ Xx @ Xy ) )
                = Xy ) ) ) ) )).

thf(cartprodpairsurjEq_type,type,(
    cartprodpairsurjEq: $o )).

thf(cartprodpairsurjEq,definition,
    ( cartprodpairsurjEq
    = ( ! [A: $i,B: $i,Xu: $i] :
          ( ( in @ Xu @ ( cartprod @ A @ B ) )
         => ( ( kpair @ ( kfst @ Xu ) @ ( ksnd @ Xu ) )
            = Xu ) ) ) )).

thf(breln_type,type,(
    breln: $i > $i > $i > $o )).

thf(breln,definition,
    ( breln
    = ( ^ [A: $i,B: $i,C: $i] :
          ( subset @ C @ ( cartprod @ A @ B ) ) ) )).

thf(dpsetconstr_type,type,(
    dpsetconstr: $i > $i > ( $i > $i > $o ) > $i )).

thf(dpsetconstrI_type,type,(
    dpsetconstrI: $o )).

thf(dpsetconstrI,definition,
    ( dpsetconstrI
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( Xphi @ Xx @ Xy )
               => ( in @ ( kpair @ Xx @ Xy )
                  @ ( dpsetconstr @ A @ B
                    @ ^ [Xz: $i,Xu: $i] :
                        ( Xphi @ Xz @ Xu ) ) ) ) ) ) ) )).

thf(dpsetconstrSub_type,type,(
    dpsetconstrSub: $o )).

thf(dpsetconstrSub,definition,
    ( dpsetconstrSub
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o] :
          ( subset
          @ ( dpsetconstr @ A @ B
            @ ^ [Xx: $i,Xy: $i] :
                ( Xphi @ Xx @ Xy ) )
          @ ( cartprod @ A @ B ) ) ) )).

thf(setOfPairsIsBReln_type,type,(
    setOfPairsIsBReln: $o )).

thf(setOfPairsIsBReln,definition,
    ( setOfPairsIsBReln
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o] :
          ( breln @ A @ B
          @ ( dpsetconstr @ A @ B
            @ ^ [Xx: $i,Xy: $i] :
                ( Xphi @ Xx @ Xy ) ) ) ) )).

thf(dpsetconstrERa_type,type,(
    dpsetconstrERa: $o )).

thf(dpsetconstrERa,definition,
    ( dpsetconstrERa
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i] :
          ( ( in @ Xx @ A )
         => ! [Xy: $i] :
              ( ( in @ Xy @ B )
             => ( ( in @ ( kpair @ Xx @ Xy )
                  @ ( dpsetconstr @ A @ B
                    @ ^ [Xz: $i,Xu: $i] :
                        ( Xphi @ Xz @ Xu ) ) )
               => ( Xphi @ Xx @ Xy ) ) ) ) ) )).

thf(dpsetconstrEL1_type,type,(
    dpsetconstrEL1: $o )).

thf(dpsetconstrEL1,definition,
    ( dpsetconstrEL1
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy )
            @ ( dpsetconstr @ A @ B
              @ ^ [Xz: $i,Xu: $i] :
                  ( Xphi @ Xz @ Xu ) ) )
         => ( in @ Xx @ A ) ) ) )).

thf(dpsetconstrEL2_type,type,(
    dpsetconstrEL2: $o )).

thf(dpsetconstrEL2,definition,
    ( dpsetconstrEL2
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy )
            @ ( dpsetconstr @ A @ B
              @ ^ [Xz: $i,Xu: $i] :
                  ( Xphi @ Xz @ Xu ) ) )
         => ( in @ Xy @ B ) ) ) )).

thf(dpsetconstrER_type,type,(
    dpsetconstrER: $o )).

thf(dpsetconstrER,definition,
    ( dpsetconstrER
    = ( ! [A: $i,B: $i,Xphi: $i > $i > $o,Xx: $i,Xy: $i] :
          ( ( in @ ( kpair @ Xx @ Xy )
            @ ( dpsetconstr @ A @ B
              @ ^ [Xz: $i,Xu: $i] :
                  ( Xphi @ Xz @ Xu ) ) )
         => ( Xphi @ Xx @ Xy ) ) ) )).

thf(func_type,type,(
    func: $i > $i > $i > $o )).

thf(func,definition,
    ( func
    = ( ^ [A: $i,B: $i,R: $i] :
          ( ( breln @ A @ B @ R )
          & ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( ex1 @ B
                @ ^ [Xy: $i] :
                    ( in @ ( kpair @ Xx @ Xy ) @ R ) ) ) ) ) )).

thf(funcSet_type,type,(
    funcSet: $i > $i > $i )).

thf(funcImageSingleton_type,type,(
    funcImageSingleton: $o )).

thf(funcImageSingleton,definition,
    ( funcImageSingleton
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( singleton
                @ ( dsetconstr @ B
                  @ ^ [Xy: $i] :
                      ( in @ ( kpair @ Xx @ Xy ) @ Xf ) ) ) ) ) ) )).

thf(apProp_type,type,(
    apProp: $o )).

thf(apProp,definition,
    ( apProp
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in
                @ ( setunion
                  @ ( dsetconstr @ B
                    @ ^ [Xy: $i] :
                        ( in @ ( kpair @ Xx @ Xy ) @ Xf ) ) )
                @ B ) ) ) ) )).

thf(ap_type,type,(
    ap: $i > $i > $i > $i > $i )).

thf(ap,definition,
    ( ap
    = ( ^ [A: $i,B: $i,Xf: $i,Xx: $i] :
          ( setunion
          @ ( dsetconstr @ B
            @ ^ [Xy: $i] :
                ( in @ ( kpair @ Xx @ Xy ) @ Xf ) ) ) ) )).

thf(app_type,type,(
    app: $o )).

thf(app,definition,
    ( app
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( ap @ A @ B @ Xf @ Xx ) @ B ) ) ) ) )).

thf(infuncsetfunc_type,type,(
    infuncsetfunc: $o )).

thf(infuncsetfunc,definition,
    ( infuncsetfunc
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ( func @ A @ B @ Xf ) ) ) )).

thf(ap2p_type,type,(
    ap2p: $o )).

thf(ap2p,definition,
    ( ap2p
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( in @ Xf @ ( funcSet @ A @ B ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( ap @ A @ B @ Xf @ Xx ) @ B ) ) ) ) )).

thf(funcinfuncset_type,type,(
    funcinfuncset: $o )).

thf(funcinfuncset,definition,
    ( funcinfuncset
    = ( ! [A: $i,B: $i,Xf: $i] :
          ( ( func @ A @ B @ Xf )
         => ( in @ Xf @ ( funcSet @ A @ B ) ) ) ) )).

thf(lamProp_type,type,(
    lamProp: $o )).

thf(lamProp,definition,
    ( lamProp
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ( func @ A @ B
            @ ( dpsetconstr @ A @ B
              @ ^ [Xx: $i,Xy: $i] :
                  ( ( Xf @ Xx )
                  = Xy ) ) ) ) ) )).

thf(lam_type,type,(
    lam: $i > $i > ( $i > $i ) > $i )).

thf(lam,definition,
    ( lam
    = ( ^ [A: $i,B: $i,Xf: $i > $i] :
          ( dpsetconstr @ A @ B
          @ ^ [Xx: $i,Xy: $i] :
              ( ( Xf @ Xx )
              = Xy ) ) ) )).

thf(lamp_type,type,(
    lamp: $o )).

thf(lamp,definition,
    ( lamp
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ( func @ A @ B
            @ ( lam @ A @ B
              @ ^ [Xx: $i] :
                  ( Xf @ Xx ) ) ) ) ) )).

thf(lam2p_type,type,(
    lam2p: $o )).

thf(lam2p,definition,
    ( lam2p
    = ( ! [A: $i,B: $i,Xf: $i > $i] :
          ( ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ( in @ ( Xf @ Xx ) @ B ) )
         => ( in
            @ ( lam @ A @ B
              @ ^ [Xx: $i] :
                  ( Xf @ Xx ) )
            @ ( funcSet @ A @ B ) ) ) ) )).

thf(brelnall1_type,type,(
    brelnall1: $o )).

thf(brelnall1,definition,
    ( brelnall1
    = ( ! [A: $i,B: $i,R: $i] :
          ( ( breln @ A @ B @ R )
         => ! [Xphi: $i > $o] :
              ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ B )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                       => ( Xphi @ ( kpair @ Xx @ Xy ) ) ) ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ R )
                 => ( Xphi @ Xx ) ) ) ) ) )).

thf(brelnall2_type,type,(
    brelnall2: $o )).

thf(brelnall2,definition,
    ( brelnall2
    = ( ! [A: $i,B: $i,R: $i] :
          ( ( breln @ A @ B @ R )
         => ! [Xphi: $i > $o] :
              ( ! [Xx: $i] :
                  ( ( in @ Xx @ A )
                 => ! [Xy: $i] :
                      ( ( in @ Xy @ B )
                     => ( ( in @ ( kpair @ Xx @ Xy ) @ R )
                       => ( Xphi @ ( kpair @ Xx @ Xy ) ) ) ) )
             => ! [Xx: $i] :
                  ( ( in @ Xx @ R )
                 => ( Xphi @ Xx ) ) ) ) ) )).

thf(ex1E2_type,type,(
    ex1E2: $o )).

thf(ex1E2,definition,
    ( ex1E2
    = ( ! [A: $i,Xphi: $i > $o] :
          ( ( ex1 @ A
            @ ^ [Xx: $i] :
                ( Xphi @ Xx ) )
         => ! [Xx: $i] :
              ( ( in @ Xx @ A )
             => ! [Xy: $i] :
                  ( ( in @ Xy @ A )
                 => ( ( Xphi @ Xx )
                   => ( ( Xphi @ Xy )
                     => ( Xx = Xy ) ) ) ) ) ) ) )).

thf(funcGraphProp1,conjecture,
    ( setextAx
   => ( emptysetAx
     => ( setadjoinAx
       => ( powersetAx
         => ( setunionAx
           => ( omega0Ax
             => ( omegaSAx
               => ( omegaIndAx
                 => ( replAx
                   => ( foundationAx
                     => ( wellorderingAx
                       => ( descrp
                         => ( dsetconstrI
                           => ( dsetconstrEL
                             => ( dsetconstrER
                               => ( exuE1
                                 => ( prop2setE
                                   => ( emptysetE
                                     => ( emptysetimpfalse
                                       => ( notinemptyset
                                         => ( exuE3e
                                           => ( setext
                                             => ( emptyI
                                               => ( noeltsimpempty
                                                 => ( setbeta
                                                   => ( nonemptyE1
                                                     => ( nonemptyI
                                                       => ( nonemptyI1
                                                         => ( setadjoinIL
                                                           => ( emptyinunitempty
                                                             => ( setadjoinIR
                                                               => ( setadjoinE
                                                                 => ( setadjoinOr
                                                                   => ( setoftrueEq
                                                                     => ( powersetI
                                                                       => ( emptyinPowerset
                                                                         => ( emptyInPowerset
                                                                           => ( powersetE
                                                                             => ( setunionI
                                                                               => ( setunionE
                                                                                 => ( subPowSU
                                                                                   => ( exuE2
                                                                                     => ( nonemptyImpWitness
                                                                                       => ( uniqinunit
                                                                                         => ( notinsingleton
                                                                                           => ( eqinunit
                                                                                             => ( singletonsswitch
                                                                                               => ( upairsetE
                                                                                                 => ( upairsetIL
                                                                                                   => ( upairsetIR
                                                                                                     => ( emptyE1
                                                                                                       => ( vacuousDall
                                                                                                         => ( quantDeMorgan1
                                                                                                           => ( quantDeMorgan2
                                                                                                             => ( quantDeMorgan3
                                                                                                               => ( quantDeMorgan4
                                                                                                                 => ( prop2setI
                                                                                                                   => ( prop2set2propI
                                                                                                                     => ( notdexE
                                                                                                                       => ( notdallE
                                                                                                                         => ( exuI1
                                                                                                                           => ( exuI3
                                                                                                                             => ( exuI2
                                                                                                                               => ( inCongP
                                                                                                                                 => ( in__Cong
                                                                                                                                   => ( exuE3u
                                                                                                                                     => ( exu__Cong
                                                                                                                                       => ( emptyset__Cong
                                                                                                                                         => ( setadjoin__Cong
                                                                                                                                           => ( powerset__Cong
                                                                                                                                             => ( setunion__Cong
                                                                                                                                               => ( omega__Cong
                                                                                                                                                 => ( exuEu
                                                                                                                                                   => ( descr__Cong
                                                                                                                                                     => ( dsetconstr__Cong
                                                                                                                                                       => ( subsetI1
                                                                                                                                                         => ( eqimpsubset2
                                                                                                                                                           => ( eqimpsubset1
                                                                                                                                                             => ( subsetI2
                                                                                                                                                               => ( emptysetsubset
                                                                                                                                                                 => ( subsetE
                                                                                                                                                                   => ( subsetE2
                                                                                                                                                                     => ( notsubsetI
                                                                                                                                                                       => ( notequalI1
                                                                                                                                                                         => ( notequalI2
                                                                                                                                                                           => ( subsetRefl
                                                                                                                                                                             => ( subsetTrans
                                                                                                                                                                               => ( setadjoinSub
                                                                                                                                                                                 => ( setadjoinSub2
                                                                                                                                                                                   => ( subset2powerset
                                                                                                                                                                                     => ( setextsub
                                                                                                                                                                                       => ( subsetemptysetimpeq
                                                                                                                                                                                         => ( powersetI1
                                                                                                                                                                                           => ( powersetE1
                                                                                                                                                                                             => ( inPowerset
                                                                                                                                                                                               => ( powersetsubset
                                                                                                                                                                                                 => ( sepInPowerset
                                                                                                                                                                                                   => ( sepSubset
                                                                                                                                                                                                     => ( binunionIL
                                                                                                                                                                                                       => ( upairset2IR
                                                                                                                                                                                                         => ( binunionIR
                                                                                                                                                                                                           => ( binunionEcases
                                                                                                                                                                                                             => ( binunionE
                                                                                                                                                                                                               => ( binunionLsub
                                                                                                                                                                                                                 => ( binunionRsub
                                                                                                                                                                                                                   => ( binintersectI
                                                                                                                                                                                                                     => ( binintersectSubset5
                                                                                                                                                                                                                       => ( binintersectEL
                                                                                                                                                                                                                         => ( binintersectLsub
                                                                                                                                                                                                                           => ( binintersectSubset2
                                                                                                                                                                                                                             => ( binintersectSubset3
                                                                                                                                                                                                                               => ( binintersectER
                                                                                                                                                                                                                                 => ( disjointsetsI1
                                                                                                                                                                                                                                   => ( binintersectRsub
                                                                                                                                                                                                                                     => ( binintersectSubset4
                                                                                                                                                                                                                                       => ( binintersectSubset1
                                                                                                                                                                                                                                         => ( bs114d
                                                                                                                                                                                                                                           => ( setminusI
                                                                                                                                                                                                                                             => ( setminusEL
                                                                                                                                                                                                                                               => ( setminusER
                                                                                                                                                                                                                                                 => ( setminusSubset2
                                                                                                                                                                                                                                                   => ( setminusERneg
                                                                                                                                                                                                                                                     => ( setminusELneg
                                                                                                                                                                                                                                                       => ( setminusILneg
                                                                                                                                                                                                                                                         => ( setminusIRneg
                                                                                                                                                                                                                                                           => ( setminusLsub
                                                                                                                                                                                                                                                             => ( setminusSubset1
                                                                                                                                                                                                                                                               => ( symdiffE
                                                                                                                                                                                                                                                                 => ( symdiffI1
                                                                                                                                                                                                                                                                   => ( symdiffI2
                                                                                                                                                                                                                                                                     => ( symdiffIneg1
                                                                                                                                                                                                                                                                       => ( symdiffIneg2
                                                                                                                                                                                                                                                                         => ( secondinupair
                                                                                                                                                                                                                                                                           => ( setukpairIL
                                                                                                                                                                                                                                                                             => ( setukpairIR
                                                                                                                                                                                                                                                                               => ( kpairiskpair
                                                                                                                                                                                                                                                                                 => ( kpairp
                                                                                                                                                                                                                                                                                   => ( singletonsubset
                                                                                                                                                                                                                                                                                     => ( singletoninpowerset
                                                                                                                                                                                                                                                                                       => ( singletoninpowunion
                                                                                                                                                                                                                                                                                         => ( upairset2E
                                                                                                                                                                                                                                                                                           => ( upairsubunion
                                                                                                                                                                                                                                                                                             => ( upairinpowunion
                                                                                                                                                                                                                                                                                               => ( ubforcartprodlem1
                                                                                                                                                                                                                                                                                                 => ( ubforcartprodlem2
                                                                                                                                                                                                                                                                                                   => ( ubforcartprodlem3
                                                                                                                                                                                                                                                                                                     => ( cartprodpairin
                                                                                                                                                                                                                                                                                                       => ( cartprodmempair1
                                                                                                                                                                                                                                                                                                         => ( cartprodmempair
                                                                                                                                                                                                                                                                                                           => ( setunionE2
                                                                                                                                                                                                                                                                                                             => ( setunionsingleton1
                                                                                                                                                                                                                                                                                                               => ( setunionsingleton2
                                                                                                                                                                                                                                                                                                                 => ( setunionsingleton
                                                                                                                                                                                                                                                                                                                   => ( singletonprop
                                                                                                                                                                                                                                                                                                                     => ( ex1E1
                                                                                                                                                                                                                                                                                                                       => ( ex1I
                                                                                                                                                                                                                                                                                                                         => ( ex1I2
                                                                                                                                                                                                                                                                                                                           => ( singletonsuniq
                                                                                                                                                                                                                                                                                                                             => ( setukpairinjL1
                                                                                                                                                                                                                                                                                                                               => ( kfstsingleton
                                                                                                                                                                                                                                                                                                                                 => ( theprop
                                                                                                                                                                                                                                                                                                                                   => ( kfstpairEq
                                                                                                                                                                                                                                                                                                                                     => ( cartprodfstin
                                                                                                                                                                                                                                                                                                                                       => ( setukpairinjL2
                                                                                                                                                                                                                                                                                                                                         => ( setukpairinjL
                                                                                                                                                                                                                                                                                                                                           => ( setukpairinjR11
                                                                                                                                                                                                                                                                                                                                             => ( setukpairinjR12
                                                                                                                                                                                                                                                                                                                                               => ( setukpairinjR1
                                                                                                                                                                                                                                                                                                                                                 => ( upairequniteq
                                                                                                                                                                                                                                                                                                                                                   => ( setukpairinjR2
                                                                                                                                                                                                                                                                                                                                                     => ( setukpairinjR
                                                                                                                                                                                                                                                                                                                                                       => ( ksndsingleton
                                                                                                                                                                                                                                                                                                                                                         => ( ksndpairEq
                                                                                                                                                                                                                                                                                                                                                           => ( kpairsurjEq
                                                                                                                                                                                                                                                                                                                                                             => ( cartprodsndin
                                                                                                                                                                                                                                                                                                                                                               => ( cartprodpairmemEL
                                                                                                                                                                                                                                                                                                                                                                 => ( cartprodpairmemER
                                                                                                                                                                                                                                                                                                                                                                   => ( cartprodmempaircEq
                                                                                                                                                                                                                                                                                                                                                                     => ( cartprodfstpairEq
                                                                                                                                                                                                                                                                                                                                                                       => ( cartprodsndpairEq
                                                                                                                                                                                                                                                                                                                                                                         => ( cartprodpairsurjEq
                                                                                                                                                                                                                                                                                                                                                                           => ( dpsetconstrI
                                                                                                                                                                                                                                                                                                                                                                             => ( dpsetconstrSub
                                                                                                                                                                                                                                                                                                                                                                               => ( setOfPairsIsBReln
                                                                                                                                                                                                                                                                                                                                                                                 => ( dpsetconstrERa
                                                                                                                                                                                                                                                                                                                                                                                   => ( dpsetconstrEL1
                                                                                                                                                                                                                                                                                                                                                                                     => ( dpsetconstrEL2
                                                                                                                                                                                                                                                                                                                                                                                       => ( dpsetconstrER
                                                                                                                                                                                                                                                                                                                                                                                         => ( funcImageSingleton
                                                                                                                                                                                                                                                                                                                                                                                           => ( apProp
                                                                                                                                                                                                                                                                                                                                                                                             => ( app
                                                                                                                                                                                                                                                                                                                                                                                               => ( infuncsetfunc
                                                                                                                                                                                                                                                                                                                                                                                                 => ( ap2p
                                                                                                                                                                                                                                                                                                                                                                                                   => ( funcinfuncset
                                                                                                                                                                                                                                                                                                                                                                                                     => ( lamProp
                                                                                                                                                                                                                                                                                                                                                                                                       => ( lamp
                                                                                                                                                                                                                                                                                                                                                                                                         => ( lam2p
                                                                                                                                                                                                                                                                                                                                                                                                           => ( brelnall1
                                                                                                                                                                                                                                                                                                                                                                                                             => ( brelnall2
                                                                                                                                                                                                                                                                                                                                                                                                               => ( ex1E2
                                                                                                                                                                                                                                                                                                                                                                                                                 => ! [A: $i,B: $i,Xf: $i] :
                                                                                                                                                                                                                                                                                                                                                                                                                      ( ( func @ A @ B @ Xf )
                                                                                                                                                                                                                                                                                                                                                                                                                     => ! [Xx: $i] :
                                                                                                                                                                                                                                                                                                                                                                                                                          ( ( in @ Xx @ A )
                                                                                                                                                                                                                                                                                                                                                                                                                         => ( in @ ( kpair @ Xx @ ( ap @ A @ B @ Xf @ Xx ) ) @ Xf ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )).

%------------------------------------------------------------------------------
