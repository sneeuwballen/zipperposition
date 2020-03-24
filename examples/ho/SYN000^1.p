%------------------------------------------------------------------------------
% File     : SYN000^1 : TPTP v6.2.0. Released v3.7.0.
% Domain   : Syntactic
% Problem  : Basic TPTP 
% Version  : Biased.
% English  : Basic TPTP  that you can't survive without parsing.

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.57 v6.1.0, 0.29 v6.0.0, 0.43 v5.5.0, 0.50 v5.4.0, 0.60 v5.1.0, 0.20 v4.1.0, 0.00 v3.7.0
% Syntax   : Number of formulae    :   42 (  18 unit;  27 type;   0 defn)
%            Number of atoms       :  161 (   4 equality;  32 variable)
%            Maximal formula depth :   11 (   4 average)
%            Number of connectives :   96 (   9   ~;  10   |;   3   &;  68   @)
%                                         (   1 <=>;   3  =>;   1  <=;   1 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :   26 (  26   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   33 (  27   :)
%            Number of variables   :   18 (   1 sgn;   6   !;   8   ?;   4   ^)
%                                         (  18   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU

% Comments :
%------------------------------------------------------------------------------
%----Propositional
thf(p0_type,type,(
    p0: $o )).

thf(q0_type,type,(
    q0: $o )).

thf(r0_type,type,(
    r0: $o )).

thf(s0_type,type,(
    s0: $o )).

thf(propositional,axiom,
    ( ( p0
      & ~ ( q0 ) )
   => ( r0
      | ~ ( s0 ) ) )).

%----First-order
thf(a_type,type,(
    a: $i )).

thf(b_type,type,(
    b: $i )).

thf(h_type,type,(
    h: $i )).

thf(f_type,type,(
    f: $i > $i )).

thf(g_type,type,(
    g: $i > $i > $i > $i )).

thf(p_type,type,(
    p: $i > $o )).

thf(q_type,type,(
    q: $i > $i > $o )).

thf(r_type,type,(
    r: $i > $i > $i > $o )).

thf(s_type,type,(
    s: $i > $o )).

thf(first_order,axiom,(
    ! [X: $i] :
      ( ( ( p @ X )
        | ~ ( q @ X @ a ) )
     => ? [Y: $i,Z: $i] :
          ( ( r @ X @ ( f @ Y ) @ ( g @ X @ ( f @ Y ) @ Z ) )
          & ~ ( s @ ( f @ ( f @ ( f @ b ) ) ) ) ) ) )).

%----Equality
thf(equality,axiom,(
    ? [Y: $i] :
    ! [X: $i,Z: $i] :
      ( ( ( f @ Y )
        = ( g @ X @ ( f @ Y ) @ Z ) )
      | ( ( f @ ( f @ ( f @ b ) ) )
       != a )
      | ( X
        = ( f @ Y ) ) ) )).

%----True and false
thf(true_false,axiom,
    ( $true
    | $false )).

thf(quoted_proposition_type,type,(
    'A proposition': $o )).

thf(quoted_predicate_type,type,(
    'A predicate': $i > $o )).

thf(quoted_constant_type,type,(
    'A constant': $i )).

thf(quoted_function_type,type,(
    'A function': $i > $i )).

thf(quoted_escape_type,type,(
    'A \'quoted \\ escape\'': $i )).

%----Quoted symbols
thf(single_quoted,axiom,
    ( 'A proposition'
    | ( 'A predicate' @ a )
    | ( p @ 'A constant' )
    | ( p @ ( 'A function' @ a ) )
    | ( p @ 'A \'quoted \\ escape\'' ) )).

%----Connectives - seen |, &, =>, ~ already
thf(useful_connectives,axiom,(
    ! [X: $i] :
      ( ( ( p @ X )
       <= ~ ( q @ X @ a ) )
    <=> ? [Y: $i,Z: $i] :
          ( ( r @ X @ ( f @ Y ) @ ( g @ X @ ( f @ Y ) @ Z ) )
        <~> ~ ( s @ ( f @ ( f @ ( f @ b ) ) ) ) ) ) )).

%----Lambda terms
thf(l1_type,type,(
    l1: $i > ( $i > $o ) > $o )).

thf(l2_type,type,(
    l2: ( $i > ( $i > $i ) > $i ) > $o )).

thf(lambda_defn,axiom,
    ( l1
    = ( ^ [C: $i,P: $i > $o] :
          ( P @ C ) ) )).

thf(lambda_use,axiom,
    ( l2
    @ ^ [C: $i,F: $i > $i] :
        ( F @ C ) )).

%----New types
thf(new_type,type,(
    new: $tType )).

thf(newc_type,type,(
    newc: new )).

thf(newf_type,type,(
    newf: new > $i > new )).

thf(newp_type,type,(
    newp: new > $i > $o )).

thf(new_axiom,axiom,(
    ! [X: new] :
      ( newp @ ( newf @ newc @ a ) @ a ) )).

%----Annotated formula names
thf(123,axiom,(
    ! [X: $i] :
      ( ( ( p @ X )
        | ~ ( q @ X @ a ) )
     => ? [Y: $i,Z: $i] :
          ( ( r @ X @ ( f @ Y ) @ ( g @ X @ ( f @ Y ) @ Z ) )
          & ~ ( s @ ( f @ ( f @ ( f @ b ) ) ) ) ) ) )).

%----Roles
thf(role_hypothesis,hypothesis,
    ( p @ h )).

thf(role_conjecture,conjecture,(
    ? [X: $i] :
      ( p @ X ) )).

%----Include directive
include('Axioms/SYN000^0.ax').

%----Comments
/* This
   is a block
   comment.
*/

%------------------------------------------------------------------------------
