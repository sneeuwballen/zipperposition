%------------------------------------------------------------------------------
% File     : PUZ075+1 : TPTP v5.2.0. Released v3.5.0.
% Domain   : Puzzles
% Problem  : Are two cells adjacent in the Wumpus world
% Version  : Especial.
% English  :

% Refs     : [Hin07] Hinrichs (2007), Email to Geoff Sutcliffe
% Source   : [Hin07]
% Names    : adj-pos [Hin07]

% Status   : Theorem
% Rating   : 0.27 v5.2.0, 0.00 v5.0.0, 0.25 v4.1.0, 0.35 v4.0.0, 0.38 v3.7.0, 0.14 v3.5.0
% Syntax   : Number of formulae    :    3 (   1 unit)
%            Number of atoms       :  365 ( 360 equality)
%            Maximal formula depth :  184 (  63 average)
%            Number of connectives :  362 (   0   ~; 180   |; 180   &)
%                                         (   2 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    3 (   0 propositional; 2-2 arity)
%            Number of functors    :  100 ( 100 constant; 0-0 arity)
%            Number of variables   :    4 (   0 sgn;   4   !;   0   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_EPR

% Comments :
%------------------------------------------------------------------------------
fof(tlhfof24987,axiom,(
    ! [TLH24985,TLH24986] :
      ( edge(TLH24986,TLH24985)
    <=> ( ( TLH24986 = cell1
          & TLH24985 = cell2 )
        | ( TLH24986 = cell2
          & TLH24985 = cell3 )
        | ( TLH24986 = cell3
          & TLH24985 = cell4 )
        | ( TLH24986 = cell4
          & TLH24985 = cell5 )
        | ( TLH24986 = cell5
          & TLH24985 = cell6 )
        | ( TLH24986 = cell6
          & TLH24985 = cell7 )
        | ( TLH24986 = cell7
          & TLH24985 = cell8 )
        | ( TLH24986 = cell8
          & TLH24985 = cell9 )
        | ( TLH24986 = cell9
          & TLH24985 = cell10 )
        | ( TLH24986 = cell11
          & TLH24985 = cell12 )
        | ( TLH24986 = cell12
          & TLH24985 = cell13 )
        | ( TLH24986 = cell13
          & TLH24985 = cell14 )
        | ( TLH24986 = cell14
          & TLH24985 = cell15 )
        | ( TLH24986 = cell15
          & TLH24985 = cell16 )
        | ( TLH24986 = cell16
          & TLH24985 = cell17 )
        | ( TLH24986 = cell17
          & TLH24985 = cell18 )
        | ( TLH24986 = cell18
          & TLH24985 = cell19 )
        | ( TLH24986 = cell19
          & TLH24985 = cell20 )
        | ( TLH24986 = cell21
          & TLH24985 = cell22 )
        | ( TLH24986 = cell22
          & TLH24985 = cell23 )
        | ( TLH24986 = cell23
          & TLH24985 = cell24 )
        | ( TLH24986 = cell24
          & TLH24985 = cell25 )
        | ( TLH24986 = cell25
          & TLH24985 = cell26 )
        | ( TLH24986 = cell26
          & TLH24985 = cell27 )
        | ( TLH24986 = cell27
          & TLH24985 = cell28 )
        | ( TLH24986 = cell28
          & TLH24985 = cell29 )
        | ( TLH24986 = cell29
          & TLH24985 = cell30 )
        | ( TLH24986 = cell31
          & TLH24985 = cell32 )
        | ( TLH24986 = cell32
          & TLH24985 = cell33 )
        | ( TLH24986 = cell33
          & TLH24985 = cell34 )
        | ( TLH24986 = cell34
          & TLH24985 = cell35 )
        | ( TLH24986 = cell35
          & TLH24985 = cell36 )
        | ( TLH24986 = cell36
          & TLH24985 = cell37 )
        | ( TLH24986 = cell37
          & TLH24985 = cell38 )
        | ( TLH24986 = cell38
          & TLH24985 = cell39 )
        | ( TLH24986 = cell39
          & TLH24985 = cell40 )
        | ( TLH24986 = cell41
          & TLH24985 = cell42 )
        | ( TLH24986 = cell42
          & TLH24985 = cell43 )
        | ( TLH24986 = cell43
          & TLH24985 = cell44 )
        | ( TLH24986 = cell44
          & TLH24985 = cell45 )
        | ( TLH24986 = cell45
          & TLH24985 = cell46 )
        | ( TLH24986 = cell46
          & TLH24985 = cell47 )
        | ( TLH24986 = cell47
          & TLH24985 = cell48 )
        | ( TLH24986 = cell48
          & TLH24985 = cell49 )
        | ( TLH24986 = cell49
          & TLH24985 = cell50 )
        | ( TLH24986 = cell51
          & TLH24985 = cell52 )
        | ( TLH24986 = cell52
          & TLH24985 = cell53 )
        | ( TLH24986 = cell53
          & TLH24985 = cell54 )
        | ( TLH24986 = cell54
          & TLH24985 = cell55 )
        | ( TLH24986 = cell55
          & TLH24985 = cell56 )
        | ( TLH24986 = cell56
          & TLH24985 = cell57 )
        | ( TLH24986 = cell57
          & TLH24985 = cell58 )
        | ( TLH24986 = cell58
          & TLH24985 = cell59 )
        | ( TLH24986 = cell59
          & TLH24985 = cell60 )
        | ( TLH24986 = cell61
          & TLH24985 = cell62 )
        | ( TLH24986 = cell62
          & TLH24985 = cell63 )
        | ( TLH24986 = cell63
          & TLH24985 = cell64 )
        | ( TLH24986 = cell64
          & TLH24985 = cell65 )
        | ( TLH24986 = cell65
          & TLH24985 = cell66 )
        | ( TLH24986 = cell66
          & TLH24985 = cell67 )
        | ( TLH24986 = cell67
          & TLH24985 = cell68 )
        | ( TLH24986 = cell68
          & TLH24985 = cell69 )
        | ( TLH24986 = cell69
          & TLH24985 = cell70 )
        | ( TLH24986 = cell71
          & TLH24985 = cell72 )
        | ( TLH24986 = cell72
          & TLH24985 = cell73 )
        | ( TLH24986 = cell73
          & TLH24985 = cell74 )
        | ( TLH24986 = cell74
          & TLH24985 = cell75 )
        | ( TLH24986 = cell75
          & TLH24985 = cell76 )
        | ( TLH24986 = cell76
          & TLH24985 = cell77 )
        | ( TLH24986 = cell77
          & TLH24985 = cell78 )
        | ( TLH24986 = cell78
          & TLH24985 = cell79 )
        | ( TLH24986 = cell79
          & TLH24985 = cell80 )
        | ( TLH24986 = cell81
          & TLH24985 = cell82 )
        | ( TLH24986 = cell82
          & TLH24985 = cell83 )
        | ( TLH24986 = cell83
          & TLH24985 = cell84 )
        | ( TLH24986 = cell84
          & TLH24985 = cell85 )
        | ( TLH24986 = cell85
          & TLH24985 = cell86 )
        | ( TLH24986 = cell86
          & TLH24985 = cell87 )
        | ( TLH24986 = cell87
          & TLH24985 = cell88 )
        | ( TLH24986 = cell88
          & TLH24985 = cell89 )
        | ( TLH24986 = cell89
          & TLH24985 = cell90 )
        | ( TLH24986 = cell91
          & TLH24985 = cell92 )
        | ( TLH24986 = cell92
          & TLH24985 = cell93 )
        | ( TLH24986 = cell93
          & TLH24985 = cell94 )
        | ( TLH24986 = cell94
          & TLH24985 = cell95 )
        | ( TLH24986 = cell95
          & TLH24985 = cell96 )
        | ( TLH24986 = cell96
          & TLH24985 = cell97 )
        | ( TLH24986 = cell97
          & TLH24985 = cell98 )
        | ( TLH24986 = cell98
          & TLH24985 = cell99 )
        | ( TLH24986 = cell99
          & TLH24985 = cell100 )
        | ( TLH24986 = cell1
          & TLH24985 = cell11 )
        | ( TLH24986 = cell2
          & TLH24985 = cell12 )
        | ( TLH24986 = cell3
          & TLH24985 = cell13 )
        | ( TLH24986 = cell4
          & TLH24985 = cell14 )
        | ( TLH24986 = cell5
          & TLH24985 = cell15 )
        | ( TLH24986 = cell6
          & TLH24985 = cell16 )
        | ( TLH24986 = cell7
          & TLH24985 = cell17 )
        | ( TLH24986 = cell8
          & TLH24985 = cell18 )
        | ( TLH24986 = cell9
          & TLH24985 = cell19 )
        | ( TLH24986 = cell10
          & TLH24985 = cell20 )
        | ( TLH24986 = cell11
          & TLH24985 = cell21 )
        | ( TLH24986 = cell12
          & TLH24985 = cell22 )
        | ( TLH24986 = cell13
          & TLH24985 = cell23 )
        | ( TLH24986 = cell14
          & TLH24985 = cell24 )
        | ( TLH24986 = cell15
          & TLH24985 = cell25 )
        | ( TLH24986 = cell16
          & TLH24985 = cell26 )
        | ( TLH24986 = cell17
          & TLH24985 = cell27 )
        | ( TLH24986 = cell18
          & TLH24985 = cell28 )
        | ( TLH24986 = cell19
          & TLH24985 = cell29 )
        | ( TLH24986 = cell20
          & TLH24985 = cell30 )
        | ( TLH24986 = cell21
          & TLH24985 = cell31 )
        | ( TLH24986 = cell22
          & TLH24985 = cell32 )
        | ( TLH24986 = cell23
          & TLH24985 = cell33 )
        | ( TLH24986 = cell24
          & TLH24985 = cell34 )
        | ( TLH24986 = cell25
          & TLH24985 = cell35 )
        | ( TLH24986 = cell26
          & TLH24985 = cell36 )
        | ( TLH24986 = cell27
          & TLH24985 = cell37 )
        | ( TLH24986 = cell28
          & TLH24985 = cell38 )
        | ( TLH24986 = cell29
          & TLH24985 = cell39 )
        | ( TLH24986 = cell30
          & TLH24985 = cell40 )
        | ( TLH24986 = cell31
          & TLH24985 = cell41 )
        | ( TLH24986 = cell32
          & TLH24985 = cell42 )
        | ( TLH24986 = cell33
          & TLH24985 = cell43 )
        | ( TLH24986 = cell34
          & TLH24985 = cell44 )
        | ( TLH24986 = cell35
          & TLH24985 = cell45 )
        | ( TLH24986 = cell36
          & TLH24985 = cell46 )
        | ( TLH24986 = cell37
          & TLH24985 = cell47 )
        | ( TLH24986 = cell38
          & TLH24985 = cell48 )
        | ( TLH24986 = cell39
          & TLH24985 = cell49 )
        | ( TLH24986 = cell40
          & TLH24985 = cell50 )
        | ( TLH24986 = cell41
          & TLH24985 = cell51 )
        | ( TLH24986 = cell42
          & TLH24985 = cell52 )
        | ( TLH24986 = cell43
          & TLH24985 = cell53 )
        | ( TLH24986 = cell44
          & TLH24985 = cell54 )
        | ( TLH24986 = cell45
          & TLH24985 = cell55 )
        | ( TLH24986 = cell46
          & TLH24985 = cell56 )
        | ( TLH24986 = cell47
          & TLH24985 = cell57 )
        | ( TLH24986 = cell48
          & TLH24985 = cell58 )
        | ( TLH24986 = cell49
          & TLH24985 = cell59 )
        | ( TLH24986 = cell50
          & TLH24985 = cell60 )
        | ( TLH24986 = cell51
          & TLH24985 = cell61 )
        | ( TLH24986 = cell52
          & TLH24985 = cell62 )
        | ( TLH24986 = cell53
          & TLH24985 = cell63 )
        | ( TLH24986 = cell54
          & TLH24985 = cell64 )
        | ( TLH24986 = cell55
          & TLH24985 = cell65 )
        | ( TLH24986 = cell56
          & TLH24985 = cell66 )
        | ( TLH24986 = cell57
          & TLH24985 = cell67 )
        | ( TLH24986 = cell58
          & TLH24985 = cell68 )
        | ( TLH24986 = cell59
          & TLH24985 = cell69 )
        | ( TLH24986 = cell60
          & TLH24985 = cell70 )
        | ( TLH24986 = cell61
          & TLH24985 = cell71 )
        | ( TLH24986 = cell62
          & TLH24985 = cell72 )
        | ( TLH24986 = cell63
          & TLH24985 = cell73 )
        | ( TLH24986 = cell64
          & TLH24985 = cell74 )
        | ( TLH24986 = cell65
          & TLH24985 = cell75 )
        | ( TLH24986 = cell66
          & TLH24985 = cell76 )
        | ( TLH24986 = cell67
          & TLH24985 = cell77 )
        | ( TLH24986 = cell68
          & TLH24985 = cell78 )
        | ( TLH24986 = cell69
          & TLH24985 = cell79 )
        | ( TLH24986 = cell70
          & TLH24985 = cell80 )
        | ( TLH24986 = cell71
          & TLH24985 = cell81 )
        | ( TLH24986 = cell72
          & TLH24985 = cell82 )
        | ( TLH24986 = cell73
          & TLH24985 = cell83 )
        | ( TLH24986 = cell74
          & TLH24985 = cell84 )
        | ( TLH24986 = cell75
          & TLH24985 = cell85 )
        | ( TLH24986 = cell76
          & TLH24985 = cell86 )
        | ( TLH24986 = cell77
          & TLH24985 = cell87 )
        | ( TLH24986 = cell78
          & TLH24985 = cell88 )
        | ( TLH24986 = cell79
          & TLH24985 = cell89 )
        | ( TLH24986 = cell80
          & TLH24985 = cell90 )
        | ( TLH24986 = cell81
          & TLH24985 = cell91 )
        | ( TLH24986 = cell82
          & TLH24985 = cell92 )
        | ( TLH24986 = cell83
          & TLH24985 = cell93 )
        | ( TLH24986 = cell84
          & TLH24985 = cell94 )
        | ( TLH24986 = cell85
          & TLH24985 = cell95 )
        | ( TLH24986 = cell86
          & TLH24985 = cell96 )
        | ( TLH24986 = cell87
          & TLH24985 = cell97 )
        | ( TLH24986 = cell88
          & TLH24985 = cell98 )
        | ( TLH24986 = cell89
          & TLH24985 = cell99 )
        | ( TLH24986 = cell90
          & TLH24985 = cell100 ) ) ) )).

fof(tlhfof24988,axiom,(
    ! [Y,X] :
      ( adj(X,Y)
    <=> ( edge(X,Y)
        | edge(Y,X) ) ) )).

fof(tlhfof24989,conjecture,(
    adj(cell12,cell13) )).

%------------------------------------------------------------------------------
