%------------------------------------------------------------------------------
% File     : ANA088^1 : TPTP v7.0.0. Released v7.0.0.
% Domain   : Analysis
% Problem  : REAL_INF_BOUNDS
% Version  : Especial.
% English  :

% Refs     : [Kal16] Kalisyk (2016), Email to Geoff Sutcliffe
% Source   : [Kal16]
% Names    : REAL_INF_BOUNDS_.p [Kal16]

% Status   : Theorem
% Rating   : ? v7.0.0
% Syntax   : Number of formulae    :    9 (   0 unit;   5 type;   0 defn)
%            Number of atoms       :   70 (   4 equality;  37 variable)
%            Maximal formula depth :   11 (   6 average)
%            Number of connectives :   61 (   3   ~;   0   |;   6   &;  44   @)
%                                         (   0 <=>;   8  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :   11 (  11   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    7 (   5   :;   0   =;   0  @=)
%                                         (   0  !!;   0  ??;   0 @@+;   0 @@-)
%            Number of variables   :   18 (   0 sgn;  14   !;   2   ?;   0   ^)
%                                         (  18   :;   2  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH1_THM_EQU_NAR

% Comments : Exported from core HOL Light.
%------------------------------------------------------------------------------
thf('thf_type_type/realax/real',type,(
    'type/realax/real': $tType )).

thf('thf_const_const/sets/inf',type,(
    'const/sets/inf': ( 'type/realax/real' > $o ) > 'type/realax/real' )).

thf('thf_const_const/sets/IN',type,(
    'const/sets/IN': 
      !>[A: $tType] :
        ( A > ( A > $o ) > $o ) )).

thf('thf_const_const/sets/EMPTY',type,(
    'const/sets/EMPTY': 
      !>[A: $tType] :
        ( A > $o ) )).

thf('thf_const_const/realax/real_le',type,(
    'const/realax/real_le': 'type/realax/real' > 'type/realax/real' > $o )).

thf('thm/realax/REAL_LE_TRANS_',axiom,(
    ! [A: 'type/realax/real',A0: 'type/realax/real',A1: 'type/realax/real'] :
      ( ( ( 'const/realax/real_le' @ A @ A0 )
        & ( 'const/realax/real_le' @ A0 @ A1 ) )
     => ( 'const/realax/real_le' @ A @ A1 ) ) )).

thf('thm/sets/MEMBER_NOT_EMPTY_',axiom,(
    ! [A: $tType,A0: A > $o] :
      ( ( ? [A1: A] :
            ( 'const/sets/IN' @ A @ A1 @ A0 ) )
      = ( A0
       != ( 'const/sets/EMPTY' @ A ) ) ) )).

thf('thm/sets/INF_',axiom,(
    ! [A: 'type/realax/real' > $o] :
      ( ( ( A
         != ( 'const/sets/EMPTY' @ 'type/realax/real' ) )
        & ? [A0: 'type/realax/real'] :
          ! [A1: 'type/realax/real'] :
            ( ( 'const/sets/IN' @ 'type/realax/real' @ A1 @ A )
           => ( 'const/realax/real_le' @ A0 @ A1 ) ) )
     => ( ! [A0: 'type/realax/real'] :
            ( ( 'const/sets/IN' @ 'type/realax/real' @ A0 @ A )
           => ( 'const/realax/real_le' @ ( 'const/sets/inf' @ A ) @ A0 ) )
        & ! [A0: 'type/realax/real'] :
            ( ! [A1: 'type/realax/real'] :
                ( ( 'const/sets/IN' @ 'type/realax/real' @ A1 @ A )
               => ( 'const/realax/real_le' @ A0 @ A1 ) )
           => ( 'const/realax/real_le' @ A0 @ ( 'const/sets/inf' @ A ) ) ) ) ) )).

thf('thm/sets/REAL_INF_BOUNDS_',conjecture,(
    ! [A: 'type/realax/real' > $o,A0: 'type/realax/real',A1: 'type/realax/real'] :
      ( ( ( A
         != ( 'const/sets/EMPTY' @ 'type/realax/real' ) )
        & ! [A2: 'type/realax/real'] :
            ( ( 'const/sets/IN' @ 'type/realax/real' @ A2 @ A )
           => ( ( 'const/realax/real_le' @ A0 @ A2 )
              & ( 'const/realax/real_le' @ A2 @ A1 ) ) ) )
     => ( ( 'const/realax/real_le' @ A0 @ ( 'const/sets/inf' @ A ) )
        & ( 'const/realax/real_le' @ ( 'const/sets/inf' @ A ) @ A1 ) ) ) )).

%------------------------------------------------------------------------------
