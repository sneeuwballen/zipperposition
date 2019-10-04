%------------------------------------------------------------------------------
% File     : CSR132^1 : TPTP v6.4.0. Released v4.1.0.
% Domain   : Commonsense Reasoning
% Problem  : In 2009, what's the feeling for Bill and Anna?
% Version  : Especial.
% English  : In the context of year 2009: Do there exist relations ?R and ?Q 
%            so that ?R holds between a person ?Y and Bill and ?Q between ?Y 
%            and Anna.

% Refs     : [Ben10] Benzmueller (2010), Email to Geoff Sutcliffe
% Source   : [Ben10]
% Names    : ef_rv_4.tq_SUMO_local [Ben10]

% Status   : Theorem
% Rating   : 0.43 v6.4.0, 0.50 v6.3.0, 0.60 v6.2.0, 0.71 v6.1.0, 0.86 v6.0.0, 0.71 v5.5.0, 0.67 v5.4.0, 0.60 v5.2.0, 0.80 v5.1.0, 1.00 v5.0.0, 0.80 v4.1.0
% Syntax   : Number of formulae    :   22 (   0 unit;  12 type;   0 defn)
%            Number of atoms       :   73 (   2 equality;  10 variable)
%            Maximal formula depth :   12 (   4 average)
%            Number of connectives :   59 (   0   ~;   0   |;   3   &;  56   @)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :   11 (  11   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   17 (  12   :;   0   =)
%            Number of variables   :   11 (   4 sgn;   0   !;   7   ?;   4   ^)
%                                         (  11   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU_NAR

% Comments : This is a simple test problem for reasoning in/about SUMO.
%            Initally the problem has been hand generated in KIF syntax in
%            SigmaKEE and then automatically translated by Benzmueller's
%            KIF2TH0 translator into THF syntax.
%          : The translation has been applied in two modes: local and SInE.
%            The local mode only translates the local assumptions and the
%            query. The SInE mode additionally translates the SInE-extract
%            of the loaded knowledge base (usually SUMO).
%          : The examples are selected to illustrate the benefits of
%            higher-order reasoning in ontology reasoning.
%------------------------------------------------------------------------------
%----The extracted signature
thf(numbers,type,(
    num: $tType )).

thf(holdsDuring_THFTYPE_IiooI,type,(
    holdsDuring_THFTYPE_IiooI: $i > $o > $o )).

thf(lAnna_THFTYPE_i,type,(
    lAnna_THFTYPE_i: $i )).

thf(lBen_THFTYPE_i,type,(
    lBen_THFTYPE_i: $i )).

thf(lBill_THFTYPE_i,type,(
    lBill_THFTYPE_i: $i )).

thf(lBob_THFTYPE_i,type,(
    lBob_THFTYPE_i: $i )).

thf(lMary_THFTYPE_i,type,(
    lMary_THFTYPE_i: $i )).

thf(lSue_THFTYPE_i,type,(
    lSue_THFTYPE_i: $i )).

thf(lYearFn_THFTYPE_IiiI,type,(
    lYearFn_THFTYPE_IiiI: $i > $i )).

thf(likes_THFTYPE_IiioI,type,(
    likes_THFTYPE_IiioI: $i > $i > $o )).

thf(n2009_THFTYPE_i,type,(
    n2009_THFTYPE_i: $i )).

thf(parent_THFTYPE_IiioI,type,(
    parent_THFTYPE_IiioI: $i > $i > $o )).

%----The translated axioms
thf(ax,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i ) @ ( parent_THFTYPE_IiioI @ lMary_THFTYPE_i @ lAnna_THFTYPE_i ) )).

thf(ax_001,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i )
    @ ? [X: $i,Y: $i] :
        ( ~ @ ( parent_THFTYPE_IiioI @ X @ Y ) ) )).

thf(ax_002,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i ) @ ( parent_THFTYPE_IiioI @ lSue_THFTYPE_i @ lBen_THFTYPE_i ) )).

thf(ax_003,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i ) @ ( likes_THFTYPE_IiioI @ lSue_THFTYPE_i @ lBill_THFTYPE_i ) )).

thf(ax_004,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i ) @ ( likes_THFTYPE_IiioI @ lBob_THFTYPE_i @ lBill_THFTYPE_i ) )).

thf(ax_005,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i )
    @ ? [X: $i,Y: $i] :
        ( ~ @ ( likes_THFTYPE_IiioI @ X @ Y ) ) )).

thf(ax_006,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i ) @ ( likes_THFTYPE_IiioI @ lMary_THFTYPE_i @ lBill_THFTYPE_i ) )).

thf(ax_007,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i ) @ ( parent_THFTYPE_IiioI @ lSue_THFTYPE_i @ lAnna_THFTYPE_i ) )).

thf(ax_008,axiom,
    ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i ) @ ( parent_THFTYPE_IiioI @ lMary_THFTYPE_i @ lBen_THFTYPE_i ) )).

%----The translated conjecture
thf(con,conjecture,(
    ? [Q: $i > $i > $o,R: $i > $i > $o,Y: $i] :
      ( holdsDuring_THFTYPE_IiooI @ ( lYearFn_THFTYPE_IiiI @ n2009_THFTYPE_i )
      @ ( ( R @ Y @ lBill_THFTYPE_i )
        & ( Q @ Y @ lAnna_THFTYPE_i )
        & ( ~
          @ ( R
            = ( ^ [Z: $i,W: $i] : $true ) ) )
        & ( ~
          @ ( Q
            = ( ^ [Z: $i,W: $i] : $true ) ) ) ) ) )).

%------------------------------------------------------------------------------
