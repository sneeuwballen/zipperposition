%------------------------------------------------------------------------------
% File     : PUZ132+1 : TPTP v5.2.0. Released v4.1.0.
% Domain   : Puzzles
% Problem  : Crime in beautiful Washington
% Version  : Especial.
% English  : A capital is a city. USA is a country. Every city has crime.
%            Washington is the capital of the USA. Every country has a
%            beautiful capital. Therefore, Washington is beautiful but has
%            crime.

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.04 v5.2.0, 0.00 v4.1.0
% Syntax   : Number of formulae    :   11 (   6 unit)
%            Number of atoms       :   16 (   1 equality)
%            Maximal formula depth :    3 (   2 average)
%            Number of connectives :    5 (   0   ~;   0   |;   1   &)
%                                         (   0 <=>;   4  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    6 (   0 propositional; 1-2 arity)
%            Number of functors    :    3 (   2 constant; 0-1 arity)
%            Number of variables   :    7 (   0 sgn;   4   !;   3   ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments :
%------------------------------------------------------------------------------
fof(capital_type,axiom,(
    ? [A] : capital(A) )).

fof(city_type,axiom,(
    ? [A] : city(A) )).

fof(capital_city_type,axiom,(
    ! [A] :
      ( capital(A)
     => city(A) ) )).

fof(country_type,axiom,(
    ? [A] : country(A) )).

fof(washington_type,axiom,(
    capital(washington) )).

fof(usa_type,axiom,(
    country(usa) )).

fof(country_capital_type,axiom,(
    ! [A] :
      ( country(A)
     => capital(capital_city(A)) ) )).

fof(crime_axiom,axiom,(
    ! [X] :
      ( city(X)
     => has_crime(X) ) )).

fof(usa_capital_axiom,axiom,(
    capital_city(usa) = washington )).

fof(beautiful_capital_axiom,axiom,(
    ! [X] :
      ( country(X)
     => beautiful(capital_city(X)) ) )).

fof(washington_conjecture,conjecture,
    ( beautiful(washington)
    & has_crime(washington) )).

%------------------------------------------------------------------------------
