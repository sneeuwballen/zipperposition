%--------------------------------------------------------------------------
% File     : PUZ005+1 : TPTP v5.2.0. Released v2.2.0.
% Domain   : Puzzles
% Problem  : The Lion and the Unicorn
% Version  : [OS95] axioms.
% English  :

% Refs     : [Wei94] Weidenbach (1994), Email to G. Sutcliffe
%          : [Wei93] Weidenbach (1993), Extending the Resolution Method wit
%          : [Smu78] Smullyan (1978), What is the Name of This Book? The Ri
%          : [OS85]  Ohlbach & Schmidt-Schauss M. (1985), The Lion and the
% Source   : [Wei94]
% Names    :

% Status   : Theorem
% Rating   : 0.17 v5.2.0, 0.07 v5.0.0, 0.05 v4.1.0, 0.06 v4.0.1, 0.05 v3.7.0, 0.00 v2.2.1
% Syntax   : Number of formulae    :   46 (   7 unit)
%            Number of atoms       :  110 (   0 equality)
%            Maximal formula depth :    8 (   4 average)
%            Number of connectives :   84 (  20   ~;   0   |;  10   &)
%                                         (   0 <=>;  54  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :   11 (   0 propositional; 1-3 arity)
%            Number of functors    :   10 (   9 constant; 0-1 arity)
%            Number of variables   :   47 (   0 sgn;  46   !;   1   ?)
%            Maximal term depth    :    2 (   1 average)
% SPC      : FOF_THM_RFO_NEQ

% Comments : This problem can be easily solved by sorted theorem provers which
%            are able to represent the one place predicates as sorts, but is
%            very hard for standard first-order provers. It can be further
%            complicated by iterating the 'yesterday' nesting in the theorem,
%            e.g.  the next iteration would be: -(exists x (day(x) &
%            lies(a_lion,x,yesterday(yesterday(x))) & lies(a_unicorn,x,
%            yesterday(yesterday(x))))).
%--------------------------------------------------------------------------
fof(monday,axiom,
    ( monday(a_monday) )).

fof(tuesday,axiom,
    ( tuesday(a_tuesday) )).

fof(wednesday,axiom,
    ( wednesday(a_wednesday) )).

fof(thursday,axiom,
    ( thursday(a_thursday) )).

fof(friday,axiom,
    ( friday(a_friday) )).

fof(saturday,axiom,
    ( saturday(a_saturday) )).

fof(sunday,axiom,
    ( sunday(a_sunday) )).

fof(monday_is_a_day,axiom,
    ( ! [X] :
        ( monday(X)
       => day(X) ) )).

fof(tuesday_is_a_day,axiom,
    ( ! [X] :
        ( tuesday(X)
       => day(X) ) )).

fof(wednesday_is_a_day,axiom,
    ( ! [X] :
        ( wednesday(X)
       => day(X) ) )).

fof(thursday_is_a_day,axiom,
    ( ! [X] :
        ( thursday(X)
       => day(X) ) )).

fof(friday_is_a_day,axiom,
    ( ! [X] :
        ( friday(X)
       => day(X) ) )).

fof(saturday_is_a_day,axiom,
    ( ! [X] :
        ( saturday(X)
       => day(X) ) )).

fof(sunday_is_a_day,axiom,
    ( ! [X] :
        ( sunday(X)
       => day(X) ) )).

fof(monday_follows_sunday,axiom,
    ( ! [X] :
        ( monday(X)
       => sunday(yesterday(X)) ) )).

fof(tuesday_follows_monday,axiom,
    ( ! [X] :
        ( tuesday(X)
       => monday(yesterday(X)) ) )).

fof(wednesday_follows_tuesday,axiom,
    ( ! [X] :
        ( wednesday(X)
       => tuesday(yesterday(X)) ) )).

fof(thursday_follows_wednesday,axiom,
    ( ! [X] :
        ( thursday(X)
       => wednesday(yesterday(X)) ) )).

fof(friday_follows_thursday,axiom,
    ( ! [X] :
        ( friday(X)
       => thursday(yesterday(X)) ) )).

fof(saturday_follows_friday,axiom,
    ( ! [X] :
        ( saturday(X)
       => friday(yesterday(X)) ) )).

fof(sunday_follows_saturday,axiom,
    ( ! [X] :
        ( sunday(X)
       => saturday(yesterday(X)) ) )).

fof(lion_lies_monday,axiom,
    ( ! [X] :
        ( monday(X)
       => lion_lies(X) ) )).

fof(lion_lies_tuesday,axiom,
    ( ! [X] :
        ( tuesday(X)
       => lion_lies(X) ) )).

fof(lion_lies_wednesday,axiom,
    ( ! [X] :
        ( wednesday(X)
       => lion_lies(X) ) )).

fof(lion_does_not_lie_thursday,axiom,
    ( ! [X] :
        ( thursday(X)
       => ~ lion_lies(X) ) )).

fof(lion_does_not_lie_friday,axiom,
    ( ! [X] :
        ( friday(X)
       => ~ lion_lies(X) ) )).

fof(lion_does_not_lie_saturday,axiom,
    ( ! [X] :
        ( saturday(X)
       => ~ lion_lies(X) ) )).

fof(lion_does_not_lie_sunday,axiom,
    ( ! [X] :
        ( sunday(X)
       => ~ lion_lies(X) ) )).

fof(unicorn_does_not_lie_monday,axiom,
    ( ! [X] :
        ( monday(X)
       => ~ unicorn_lies(X) ) )).

fof(unicorn_does_not_lie_tuesday,axiom,
    ( ! [X] :
        ( tuesday(X)
       => ~ unicorn_lies(X) ) )).

fof(unicorn_does_not_lie_wednesday,axiom,
    ( ! [X] :
        ( wednesday(X)
       => ~ unicorn_lies(X) ) )).

fof(unicorn_lies_thursday,axiom,
    ( ! [X] :
        ( thursday(X)
       => unicorn_lies(X) ) )).

fof(unicorn_lies_friday,axiom,
    ( ! [X] :
        ( friday(X)
       => unicorn_lies(X) ) )).

fof(unicorn_lies_saturday,axiom,
    ( ! [X] :
        ( saturday(X)
       => unicorn_lies(X) ) )).

fof(unicorn_does_not_lie_sunday,axiom,
    ( ! [X] :
        ( sunday(X)
       => ~ unicorn_lies(X) ) )).

fof(lion_lies_on_a_day,axiom,
    ( ! [X] :
        ( lion_lies(X)
       => day(X) ) )).

fof(unicorn_lies_on_a_day,axiom,
    ( ! [X] :
        ( unicorn_lies(X)
       => day(X) ) )).

fof(lion_lies_on_this_day,axiom,
    ( ! [X] :
        ( day(X)
       => ! [Y] :
            ( day(Y)
           => ( ( lion_lies(X)
                & lies_on_one_of(a_lion,X,Y) )
             => ~ lion_lies(Y) ) ) ) )).

fof(lion_lies_on_other_day,axiom,
    ( ! [X] :
        ( day(X)
       => ! [Y] :
            ( day(Y)
           => ( ( ~ lion_lies(X)
                & lies_on_one_of(a_lion,X,Y) )
             => lion_lies(Y) ) ) ) )).

fof(lion_lies_on_neither,axiom,
    ( ! [X] :
        ( day(X)
       => ! [Y] :
            ( day(Y)
           => ( ( ~ lion_lies(X)
                & ~ lies_on_one_of(a_lion,X,Y) )
             => ~ lion_lies(Y) ) ) ) )).

fof(lion_lies_on_both,axiom,
    ( ! [X] :
        ( day(X)
       => ! [Y] :
            ( day(Y)
           => ( ( lion_lies(X)
                & ~ lies_on_one_of(a_lion,X,Y) )
             => lion_lies(Y) ) ) ) )).

fof(unicorn_lies_on_this_day,axiom,
    ( ! [X] :
        ( day(X)
       => ! [Y] :
            ( day(Y)
           => ( ( unicorn_lies(X)
                & lies_on_one_of(a_unicorn,X,Y) )
             => ~ unicorn_lies(Y) ) ) ) )).

fof(unicorn_lies_on_other_day,axiom,
    ( ! [X] :
        ( day(X)
       => ! [Y] :
            ( day(Y)
           => ( ( ~ unicorn_lies(X)
                & lies_on_one_of(a_unicorn,X,Y) )
             => unicorn_lies(Y) ) ) ) )).

fof(unicorn_lies_on_neither,axiom,
    ( ! [X] :
        ( day(X)
       => ! [Y] :
            ( day(Y)
           => ( ( ~ unicorn_lies(X)
                & ~ lies_on_one_of(a_unicorn,X,Y) )
             => ~ unicorn_lies(Y) ) ) ) )).

fof(unicorn_lies_on_both,axiom,
    ( ! [X] :
        ( day(X)
       => ! [Y] :
            ( day(Y)
           => ( ( unicorn_lies(X)
                & ~ lies_on_one_of(a_unicorn,X,Y) )
             => unicorn_lies(Y) ) ) ) )).

fof(prove_there_are_close_lying_days,conjecture,
    ( ? [X] :
        ( day(X)
        & lies_on_one_of(a_lion,X,yesterday(X))
        & lies_on_one_of(a_unicorn,X,yesterday(X)) ) )).

%--------------------------------------------------------------------------
