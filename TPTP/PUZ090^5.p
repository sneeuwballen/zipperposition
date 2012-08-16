%------------------------------------------------------------------------------
% File     : PUZ090^5 : TPTP v5.2.0. Released v4.0.0.
% Domain   : Puzzles
% Problem  : TPS problem THM210
% Version  : Especial.
% English  : Lewis Carroll's problem of the winds and windows; from the Ninth 
%            Paper on Logic, November 1892.

% Refs     : [Bro09] Brown (2009), Email to Geoff Sutcliffe
% Source   : [Bro09]
% Names    : tps_0336 [Bro09]
%          : THM210 [TPS]

% Status   : Theorem
% Rating   : 0.25 v5.2.0, 0.00 v4.0.0
% Syntax   : Number of formulae    :   14 (  13 unit;  13 type;   0 defn)
%            Number of atoms       :   44 (   0 equality;   0 variable)
%            Maximal formula depth :   14 (   3 average)
%            Number of connectives :   37 (   7   ~;   0   |;  17   &;   0   @)
%                                         (   0 <=>;  13  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&;   0  !!;   0  ??)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   15 (  13   :)
%            Number of variables   :    0 (   0 sgn;   0   !;   0   ?;   0   ^)
%                                         (   0   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : THF_THM_NEQ

% Comments : This problem is from the TPS library. Copyright (c) 2009 The TPS
%            project in the Department of Mathematical Sciences at Carnegie
%            Mellon University. Distributed under the Creative Commons copyleft
%            license: http://creativecommons.org/licenses/by-sa/3.0/
%          : THF0 syntax
%------------------------------------------------------------------------------
thf(cOPEN_WINDOW,type,(
    cOPEN_WINDOW: $o )).

thf(cEAST,type,(
    cEAST: $o )).

thf(cRHEUMATIC,type,(
    cRHEUMATIC: $o )).

thf(cFOGGY,type,(
    cFOGGY: $o )).

thf(cOPEN_DOOR,type,(
    cOPEN_DOOR: $o )).

thf(cFLUTE,type,(
    cFLUTE: $o )).

thf(cSUNSHINE,type,(
    cSUNSHINE: $o )).

thf(cFIRE,type,(
    cFIRE: $o )).

thf(cGUSTY,type,(
    cGUSTY: $o )).

thf(cHEADACHE,type,(
    cHEADACHE: $o )).

thf(cSMOKES,type,(
    cSMOKES: $o )).

thf(cCOLD,type,(
    cCOLD: $o )).

thf(cSMOKE,type,(
    cSMOKE: $o )).

thf(cTHM210,conjecture,
    ( ( ( cEAST
       => cSUNSHINE )
      & ( ( cCOLD
          & cFOGGY )
       => cFLUTE )
      & ( ( cFIRE
          & cSMOKE )
       => cOPEN_DOOR )
      & ( ( cCOLD
          & cRHEUMATIC )
       => cFIRE )
      & ( ( cEAST
          & cGUSTY )
       => cSMOKES )
      & ( cOPEN_DOOR
       => ~ ( cHEADACHE ) )
      & ( cFOGGY
       => ~ ( cOPEN_WINDOW ) )
      & ( ( ~ ( cGUSTY )
          & cFIRE
          & ~ ( cOPEN_DOOR ) )
       => ~ ( cRHEUMATIC ) )
      & ( cSUNSHINE
       => cFOGGY )
      & ( cFLUTE
       => ~ ( cOPEN_DOOR ) )
      & ( ( cFOGGY
          & cEAST )
       => cRHEUMATIC ) )
   => ( cEAST
     => ~ ( cOPEN_WINDOW ) ) )).

%------------------------------------------------------------------------------
