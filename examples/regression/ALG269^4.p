%------------------------------------------------------------------------------
% File     : ALG269^4 : TPTP v6.4.0. Bugfixed v5.2.0.
% Domain   : Algebra
% Problem  : HOAS induction
% Version  : [Bro09] axioms : Reduced > Especial.
%            Theorem formulation : Modified.
% English  :

% Refs     : [DHK95] Dowek et al. (1995), Higher-order Unification via Expl
%          : [Zha08] Zhang (2008), Using LEO-II to Prove Properties of an E
%          : [Ben09] Benzmueller (2009), Email to Geoff Sutcliffe
%          : [Bro09] Brown (2009), M-Set Models
% Source   : [Ben09]
% Names    : hoasinduction_no_psi_cond_lthm [Ben09]

% Status   : CounterSatisfiable
% Rating   : 0.67 v5.4.0, 1.00 v5.2.0
% Syntax   : Number of formulae    :  238 (   1 unit; 124 type; 113 defn)
%            Number of atoms       : 2372 ( 161 equality; 723 variable)
%            Maximal formula depth :   39 (   8 average)
%            Number of connectives : 1942 (   6   ~;   0   |;   4   &; 916   @)
%                                         (   2 <=>;1014  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :  128 ( 128   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :  126 ( 124   :;   0   =)
%            Number of variables   :  327 (   3 sgn; 283   !;   5   ?;  39   ^)
%                                         ( 327   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_CSA_EQU_NAR

% Comments : 
% Bugfixes : v5.2.0 - Bugfixes in ALG003^0.ax
%------------------------------------------------------------------------------
%----Include Untyped Lambda Sigma defs
include('Axioms/ALG003^0.ax').
%------------------------------------------------------------------------------
thf(thm,conjecture,(
    hoasinduction_no_psi_cond_lthm )).

%------------------------------------------------------------------------------
