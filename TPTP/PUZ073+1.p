%------------------------------------------------------------------------------
% File     : PUZ073+1 : TPTP v5.2.0. Released v3.5.0.
% Domain   : Puzzles
% Problem  : The patient is not adjacent to the oxygen
% Version  : Especial.
% English  :

% Refs     : [Hin07] Hinrichs (2007), Email to Geoff Sutcliffe
% Source   : [Hin07]
% Names    : hospital [Hin07]

% Status   : Theorem
% Rating   : 0.64 v5.2.0, 0.25 v5.0.0, 0.50 v4.1.0, 0.91 v4.0.1, 0.87 v4.0.0, 0.88 v3.7.0, 0.86 v3.5.0
% Syntax   : Number of formulae    : 4958 (4950 unit)
%            Number of atoms       : 5433 (5412 equality)
%            Maximal formula depth :  184 (   2 average)
%            Number of connectives : 5426 (4951   ~; 287   |; 183   &)
%                                         (   2 <=>;   3  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    5 (   0 propositional; 1-2 arity)
%            Number of functors    :  100 ( 100 constant; 0-0 arity)
%            Number of variables   :   11 (   0 sgn;  11   !;   0   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_EPR

% Comments :
%------------------------------------------------------------------------------
fof(tlhfof44975,axiom,
    ( patient(cell12)
    | patient(cell11)
    | patient(cell2)
    | patient(cell13)
    | patient(cell22) )).

fof(tlhfof44976,axiom,
    ( oxygen(cell89)
    | oxygen(cell88)
    | oxygen(cell79)
    | oxygen(cell90)
    | oxygen(cell99) )).

fof(tlhfof44977,axiom,(
    ! [Y,X] :
      ( ( oxygen(X)
        & oxygen(Y) )
     => X = Y ) )).

fof(tlhfof44978,axiom,(
    ! [Y,X] :
      ( ( patient(X)
        & patient(Y) )
     => X = Y ) )).

fof(tlhfof44979,axiom,(
    ! [TLH44974,TLH44975] :
      ( edge(TLH44975,TLH44974)
    <=> ( ( TLH44975 = cell1
          & TLH44974 = cell2 )
        | ( TLH44975 = cell2
          & TLH44974 = cell3 )
        | ( TLH44975 = cell3
          & TLH44974 = cell4 )
        | ( TLH44975 = cell4
          & TLH44974 = cell5 )
        | ( TLH44975 = cell5
          & TLH44974 = cell6 )
        | ( TLH44975 = cell6
          & TLH44974 = cell7 )
        | ( TLH44975 = cell7
          & TLH44974 = cell8 )
        | ( TLH44975 = cell8
          & TLH44974 = cell9 )
        | ( TLH44975 = cell9
          & TLH44974 = cell10 )
        | ( TLH44975 = cell11
          & TLH44974 = cell12 )
        | ( TLH44975 = cell12
          & TLH44974 = cell13 )
        | ( TLH44975 = cell13
          & TLH44974 = cell14 )
        | ( TLH44975 = cell14
          & TLH44974 = cell15 )
        | ( TLH44975 = cell15
          & TLH44974 = cell16 )
        | ( TLH44975 = cell16
          & TLH44974 = cell17 )
        | ( TLH44975 = cell17
          & TLH44974 = cell18 )
        | ( TLH44975 = cell18
          & TLH44974 = cell19 )
        | ( TLH44975 = cell19
          & TLH44974 = cell20 )
        | ( TLH44975 = cell21
          & TLH44974 = cell22 )
        | ( TLH44975 = cell22
          & TLH44974 = cell23 )
        | ( TLH44975 = cell23
          & TLH44974 = cell24 )
        | ( TLH44975 = cell24
          & TLH44974 = cell25 )
        | ( TLH44975 = cell25
          & TLH44974 = cell26 )
        | ( TLH44975 = cell26
          & TLH44974 = cell27 )
        | ( TLH44975 = cell27
          & TLH44974 = cell28 )
        | ( TLH44975 = cell28
          & TLH44974 = cell29 )
        | ( TLH44975 = cell29
          & TLH44974 = cell30 )
        | ( TLH44975 = cell31
          & TLH44974 = cell32 )
        | ( TLH44975 = cell32
          & TLH44974 = cell33 )
        | ( TLH44975 = cell33
          & TLH44974 = cell34 )
        | ( TLH44975 = cell34
          & TLH44974 = cell35 )
        | ( TLH44975 = cell35
          & TLH44974 = cell36 )
        | ( TLH44975 = cell36
          & TLH44974 = cell37 )
        | ( TLH44975 = cell37
          & TLH44974 = cell38 )
        | ( TLH44975 = cell38
          & TLH44974 = cell39 )
        | ( TLH44975 = cell39
          & TLH44974 = cell40 )
        | ( TLH44975 = cell41
          & TLH44974 = cell42 )
        | ( TLH44975 = cell42
          & TLH44974 = cell43 )
        | ( TLH44975 = cell43
          & TLH44974 = cell44 )
        | ( TLH44975 = cell44
          & TLH44974 = cell45 )
        | ( TLH44975 = cell45
          & TLH44974 = cell46 )
        | ( TLH44975 = cell46
          & TLH44974 = cell47 )
        | ( TLH44975 = cell47
          & TLH44974 = cell48 )
        | ( TLH44975 = cell48
          & TLH44974 = cell49 )
        | ( TLH44975 = cell49
          & TLH44974 = cell50 )
        | ( TLH44975 = cell51
          & TLH44974 = cell52 )
        | ( TLH44975 = cell52
          & TLH44974 = cell53 )
        | ( TLH44975 = cell53
          & TLH44974 = cell54 )
        | ( TLH44975 = cell54
          & TLH44974 = cell55 )
        | ( TLH44975 = cell55
          & TLH44974 = cell56 )
        | ( TLH44975 = cell56
          & TLH44974 = cell57 )
        | ( TLH44975 = cell57
          & TLH44974 = cell58 )
        | ( TLH44975 = cell58
          & TLH44974 = cell59 )
        | ( TLH44975 = cell59
          & TLH44974 = cell60 )
        | ( TLH44975 = cell61
          & TLH44974 = cell62 )
        | ( TLH44975 = cell62
          & TLH44974 = cell63 )
        | ( TLH44975 = cell63
          & TLH44974 = cell64 )
        | ( TLH44975 = cell64
          & TLH44974 = cell65 )
        | ( TLH44975 = cell65
          & TLH44974 = cell66 )
        | ( TLH44975 = cell66
          & TLH44974 = cell67 )
        | ( TLH44975 = cell67
          & TLH44974 = cell68 )
        | ( TLH44975 = cell68
          & TLH44974 = cell69 )
        | ( TLH44975 = cell69
          & TLH44974 = cell70 )
        | ( TLH44975 = cell71
          & TLH44974 = cell72 )
        | ( TLH44975 = cell72
          & TLH44974 = cell73 )
        | ( TLH44975 = cell73
          & TLH44974 = cell74 )
        | ( TLH44975 = cell74
          & TLH44974 = cell75 )
        | ( TLH44975 = cell75
          & TLH44974 = cell76 )
        | ( TLH44975 = cell76
          & TLH44974 = cell77 )
        | ( TLH44975 = cell77
          & TLH44974 = cell78 )
        | ( TLH44975 = cell78
          & TLH44974 = cell79 )
        | ( TLH44975 = cell79
          & TLH44974 = cell80 )
        | ( TLH44975 = cell81
          & TLH44974 = cell82 )
        | ( TLH44975 = cell82
          & TLH44974 = cell83 )
        | ( TLH44975 = cell83
          & TLH44974 = cell84 )
        | ( TLH44975 = cell84
          & TLH44974 = cell85 )
        | ( TLH44975 = cell85
          & TLH44974 = cell86 )
        | ( TLH44975 = cell86
          & TLH44974 = cell87 )
        | ( TLH44975 = cell87
          & TLH44974 = cell88 )
        | ( TLH44975 = cell88
          & TLH44974 = cell89 )
        | ( TLH44975 = cell89
          & TLH44974 = cell90 )
        | ( TLH44975 = cell91
          & TLH44974 = cell92 )
        | ( TLH44975 = cell92
          & TLH44974 = cell93 )
        | ( TLH44975 = cell93
          & TLH44974 = cell94 )
        | ( TLH44975 = cell94
          & TLH44974 = cell95 )
        | ( TLH44975 = cell95
          & TLH44974 = cell96 )
        | ( TLH44975 = cell96
          & TLH44974 = cell97 )
        | ( TLH44975 = cell97
          & TLH44974 = cell98 )
        | ( TLH44975 = cell98
          & TLH44974 = cell99 )
        | ( TLH44975 = cell99
          & TLH44974 = cell100 )
        | ( TLH44975 = cell1
          & TLH44974 = cell11 )
        | ( TLH44975 = cell2
          & TLH44974 = cell12 )
        | ( TLH44975 = cell3
          & TLH44974 = cell13 )
        | ( TLH44975 = cell4
          & TLH44974 = cell14 )
        | ( TLH44975 = cell5
          & TLH44974 = cell15 )
        | ( TLH44975 = cell6
          & TLH44974 = cell16 )
        | ( TLH44975 = cell7
          & TLH44974 = cell17 )
        | ( TLH44975 = cell8
          & TLH44974 = cell18 )
        | ( TLH44975 = cell9
          & TLH44974 = cell19 )
        | ( TLH44975 = cell10
          & TLH44974 = cell20 )
        | ( TLH44975 = cell11
          & TLH44974 = cell21 )
        | ( TLH44975 = cell12
          & TLH44974 = cell22 )
        | ( TLH44975 = cell13
          & TLH44974 = cell23 )
        | ( TLH44975 = cell14
          & TLH44974 = cell24 )
        | ( TLH44975 = cell15
          & TLH44974 = cell25 )
        | ( TLH44975 = cell16
          & TLH44974 = cell26 )
        | ( TLH44975 = cell17
          & TLH44974 = cell27 )
        | ( TLH44975 = cell18
          & TLH44974 = cell28 )
        | ( TLH44975 = cell19
          & TLH44974 = cell29 )
        | ( TLH44975 = cell20
          & TLH44974 = cell30 )
        | ( TLH44975 = cell21
          & TLH44974 = cell31 )
        | ( TLH44975 = cell22
          & TLH44974 = cell32 )
        | ( TLH44975 = cell23
          & TLH44974 = cell33 )
        | ( TLH44975 = cell24
          & TLH44974 = cell34 )
        | ( TLH44975 = cell25
          & TLH44974 = cell35 )
        | ( TLH44975 = cell26
          & TLH44974 = cell36 )
        | ( TLH44975 = cell27
          & TLH44974 = cell37 )
        | ( TLH44975 = cell28
          & TLH44974 = cell38 )
        | ( TLH44975 = cell29
          & TLH44974 = cell39 )
        | ( TLH44975 = cell30
          & TLH44974 = cell40 )
        | ( TLH44975 = cell31
          & TLH44974 = cell41 )
        | ( TLH44975 = cell32
          & TLH44974 = cell42 )
        | ( TLH44975 = cell33
          & TLH44974 = cell43 )
        | ( TLH44975 = cell34
          & TLH44974 = cell44 )
        | ( TLH44975 = cell35
          & TLH44974 = cell45 )
        | ( TLH44975 = cell36
          & TLH44974 = cell46 )
        | ( TLH44975 = cell37
          & TLH44974 = cell47 )
        | ( TLH44975 = cell38
          & TLH44974 = cell48 )
        | ( TLH44975 = cell39
          & TLH44974 = cell49 )
        | ( TLH44975 = cell40
          & TLH44974 = cell50 )
        | ( TLH44975 = cell41
          & TLH44974 = cell51 )
        | ( TLH44975 = cell42
          & TLH44974 = cell52 )
        | ( TLH44975 = cell43
          & TLH44974 = cell53 )
        | ( TLH44975 = cell44
          & TLH44974 = cell54 )
        | ( TLH44975 = cell45
          & TLH44974 = cell55 )
        | ( TLH44975 = cell46
          & TLH44974 = cell56 )
        | ( TLH44975 = cell47
          & TLH44974 = cell57 )
        | ( TLH44975 = cell48
          & TLH44974 = cell58 )
        | ( TLH44975 = cell49
          & TLH44974 = cell59 )
        | ( TLH44975 = cell50
          & TLH44974 = cell60 )
        | ( TLH44975 = cell51
          & TLH44974 = cell61 )
        | ( TLH44975 = cell52
          & TLH44974 = cell62 )
        | ( TLH44975 = cell53
          & TLH44974 = cell63 )
        | ( TLH44975 = cell54
          & TLH44974 = cell64 )
        | ( TLH44975 = cell55
          & TLH44974 = cell65 )
        | ( TLH44975 = cell56
          & TLH44974 = cell66 )
        | ( TLH44975 = cell57
          & TLH44974 = cell67 )
        | ( TLH44975 = cell58
          & TLH44974 = cell68 )
        | ( TLH44975 = cell59
          & TLH44974 = cell69 )
        | ( TLH44975 = cell60
          & TLH44974 = cell70 )
        | ( TLH44975 = cell61
          & TLH44974 = cell71 )
        | ( TLH44975 = cell62
          & TLH44974 = cell72 )
        | ( TLH44975 = cell63
          & TLH44974 = cell73 )
        | ( TLH44975 = cell64
          & TLH44974 = cell74 )
        | ( TLH44975 = cell65
          & TLH44974 = cell75 )
        | ( TLH44975 = cell66
          & TLH44974 = cell76 )
        | ( TLH44975 = cell67
          & TLH44974 = cell77 )
        | ( TLH44975 = cell68
          & TLH44974 = cell78 )
        | ( TLH44975 = cell69
          & TLH44974 = cell79 )
        | ( TLH44975 = cell70
          & TLH44974 = cell80 )
        | ( TLH44975 = cell71
          & TLH44974 = cell81 )
        | ( TLH44975 = cell72
          & TLH44974 = cell82 )
        | ( TLH44975 = cell73
          & TLH44974 = cell83 )
        | ( TLH44975 = cell74
          & TLH44974 = cell84 )
        | ( TLH44975 = cell75
          & TLH44974 = cell85 )
        | ( TLH44975 = cell76
          & TLH44974 = cell86 )
        | ( TLH44975 = cell77
          & TLH44974 = cell87 )
        | ( TLH44975 = cell78
          & TLH44974 = cell88 )
        | ( TLH44975 = cell79
          & TLH44974 = cell89 )
        | ( TLH44975 = cell80
          & TLH44974 = cell90 )
        | ( TLH44975 = cell81
          & TLH44974 = cell91 )
        | ( TLH44975 = cell82
          & TLH44974 = cell92 )
        | ( TLH44975 = cell83
          & TLH44974 = cell93 )
        | ( TLH44975 = cell84
          & TLH44974 = cell94 )
        | ( TLH44975 = cell85
          & TLH44974 = cell95 )
        | ( TLH44975 = cell86
          & TLH44974 = cell96 )
        | ( TLH44975 = cell87
          & TLH44974 = cell97 )
        | ( TLH44975 = cell88
          & TLH44974 = cell98 )
        | ( TLH44975 = cell89
          & TLH44974 = cell99 )
        | ( TLH44975 = cell90
          & TLH44974 = cell100 ) ) ) )).

fof(tlhfof44980,axiom,(
    ! [Y,X] :
      ( adj(X,Y)
    <=> ( edge(X,Y)
        | edge(Y,X) ) ) )).

fof(tlhfof44981,axiom,(
    cell100 != cell98 )).

fof(tlhfof44982,axiom,(
    cell100 != cell97 )).

fof(tlhfof44983,axiom,(
    cell100 != cell96 )).

fof(tlhfof44984,axiom,(
    cell100 != cell95 )).

fof(tlhfof44985,axiom,(
    cell100 != cell94 )).

fof(tlhfof44986,axiom,(
    cell100 != cell93 )).

fof(tlhfof44987,axiom,(
    cell100 != cell91 )).

fof(tlhfof44988,axiom,(
    cell100 != cell92 )).

fof(tlhfof44989,axiom,(
    cell100 != cell87 )).

fof(tlhfof44990,axiom,(
    cell100 != cell86 )).

fof(tlhfof44991,axiom,(
    cell100 != cell85 )).

fof(tlhfof44992,axiom,(
    cell100 != cell84 )).

fof(tlhfof44993,axiom,(
    cell100 != cell83 )).

fof(tlhfof44994,axiom,(
    cell100 != cell81 )).

fof(tlhfof44995,axiom,(
    cell100 != cell82 )).

fof(tlhfof44996,axiom,(
    cell100 != cell80 )).

fof(tlhfof44997,axiom,(
    cell100 != cell78 )).

fof(tlhfof44998,axiom,(
    cell100 != cell77 )).

fof(tlhfof44999,axiom,(
    cell100 != cell76 )).

fof(tlhfof45000,axiom,(
    cell100 != cell75 )).

fof(tlhfof45001,axiom,(
    cell100 != cell74 )).

fof(tlhfof45002,axiom,(
    cell100 != cell73 )).

fof(tlhfof45003,axiom,(
    cell100 != cell71 )).

fof(tlhfof45004,axiom,(
    cell100 != cell72 )).

fof(tlhfof45005,axiom,(
    cell100 != cell70 )).

fof(tlhfof45006,axiom,(
    cell100 != cell69 )).

fof(tlhfof45007,axiom,(
    cell100 != cell68 )).

fof(tlhfof45008,axiom,(
    cell100 != cell67 )).

fof(tlhfof45009,axiom,(
    cell100 != cell66 )).

fof(tlhfof45010,axiom,(
    cell100 != cell65 )).

fof(tlhfof45011,axiom,(
    cell100 != cell64 )).

fof(tlhfof45012,axiom,(
    cell100 != cell63 )).

fof(tlhfof45013,axiom,(
    cell100 != cell61 )).

fof(tlhfof45014,axiom,(
    cell100 != cell62 )).

fof(tlhfof45015,axiom,(
    cell100 != cell60 )).

fof(tlhfof45016,axiom,(
    cell100 != cell59 )).

fof(tlhfof45017,axiom,(
    cell100 != cell58 )).

fof(tlhfof45018,axiom,(
    cell100 != cell57 )).

fof(tlhfof45019,axiom,(
    cell100 != cell56 )).

fof(tlhfof45020,axiom,(
    cell100 != cell55 )).

fof(tlhfof45021,axiom,(
    cell100 != cell54 )).

fof(tlhfof45022,axiom,(
    cell100 != cell53 )).

fof(tlhfof45023,axiom,(
    cell100 != cell51 )).

fof(tlhfof45024,axiom,(
    cell100 != cell52 )).

fof(tlhfof45025,axiom,(
    cell100 != cell50 )).

fof(tlhfof45026,axiom,(
    cell100 != cell49 )).

fof(tlhfof45027,axiom,(
    cell100 != cell48 )).

fof(tlhfof45028,axiom,(
    cell100 != cell47 )).

fof(tlhfof45029,axiom,(
    cell100 != cell46 )).

fof(tlhfof45030,axiom,(
    cell100 != cell45 )).

fof(tlhfof45031,axiom,(
    cell100 != cell44 )).

fof(tlhfof45032,axiom,(
    cell100 != cell43 )).

fof(tlhfof45033,axiom,(
    cell100 != cell41 )).

fof(tlhfof45034,axiom,(
    cell100 != cell42 )).

fof(tlhfof45035,axiom,(
    cell100 != cell40 )).

fof(tlhfof45036,axiom,(
    cell100 != cell39 )).

fof(tlhfof45037,axiom,(
    cell100 != cell38 )).

fof(tlhfof45038,axiom,(
    cell100 != cell37 )).

fof(tlhfof45039,axiom,(
    cell100 != cell36 )).

fof(tlhfof45040,axiom,(
    cell100 != cell35 )).

fof(tlhfof45041,axiom,(
    cell100 != cell34 )).

fof(tlhfof45042,axiom,(
    cell100 != cell33 )).

fof(tlhfof45043,axiom,(
    cell100 != cell31 )).

fof(tlhfof45044,axiom,(
    cell100 != cell32 )).

fof(tlhfof45045,axiom,(
    cell100 != cell30 )).

fof(tlhfof45046,axiom,(
    cell100 != cell29 )).

fof(tlhfof45047,axiom,(
    cell100 != cell28 )).

fof(tlhfof45048,axiom,(
    cell100 != cell27 )).

fof(tlhfof45049,axiom,(
    cell100 != cell26 )).

fof(tlhfof45050,axiom,(
    cell100 != cell25 )).

fof(tlhfof45051,axiom,(
    cell100 != cell24 )).

fof(tlhfof45052,axiom,(
    cell100 != cell23 )).

fof(tlhfof45053,axiom,(
    cell100 != cell21 )).

fof(tlhfof45054,axiom,(
    cell100 != cell20 )).

fof(tlhfof45055,axiom,(
    cell100 != cell19 )).

fof(tlhfof45056,axiom,(
    cell100 != cell18 )).

fof(tlhfof45057,axiom,(
    cell100 != cell17 )).

fof(tlhfof45058,axiom,(
    cell100 != cell16 )).

fof(tlhfof45059,axiom,(
    cell100 != cell15 )).

fof(tlhfof45060,axiom,(
    cell100 != cell14 )).

fof(tlhfof45061,axiom,(
    cell100 != cell10 )).

fof(tlhfof45062,axiom,(
    cell100 != cell9 )).

fof(tlhfof45063,axiom,(
    cell100 != cell8 )).

fof(tlhfof45064,axiom,(
    cell100 != cell7 )).

fof(tlhfof45065,axiom,(
    cell100 != cell6 )).

fof(tlhfof45066,axiom,(
    cell100 != cell5 )).

fof(tlhfof45067,axiom,(
    cell100 != cell4 )).

fof(tlhfof45068,axiom,(
    cell100 != cell3 )).

fof(tlhfof45069,axiom,(
    cell100 != cell1 )).

fof(tlhfof45070,axiom,(
    cell100 != cell89 )).

fof(tlhfof45071,axiom,(
    cell100 != cell88 )).

fof(tlhfof45072,axiom,(
    cell100 != cell79 )).

fof(tlhfof45073,axiom,(
    cell100 != cell90 )).

fof(tlhfof45074,axiom,(
    cell100 != cell99 )).

fof(tlhfof45075,axiom,(
    cell100 != cell12 )).

fof(tlhfof45076,axiom,(
    cell100 != cell11 )).

fof(tlhfof45077,axiom,(
    cell100 != cell2 )).

fof(tlhfof45078,axiom,(
    cell100 != cell13 )).

fof(tlhfof45079,axiom,(
    cell100 != cell22 )).

fof(tlhfof45080,axiom,(
    cell98 != cell97 )).

fof(tlhfof45081,axiom,(
    cell98 != cell96 )).

fof(tlhfof45082,axiom,(
    cell98 != cell95 )).

fof(tlhfof45083,axiom,(
    cell98 != cell94 )).

fof(tlhfof45084,axiom,(
    cell98 != cell93 )).

fof(tlhfof45085,axiom,(
    cell98 != cell91 )).

fof(tlhfof45086,axiom,(
    cell98 != cell92 )).

fof(tlhfof45087,axiom,(
    cell98 != cell87 )).

fof(tlhfof45088,axiom,(
    cell98 != cell86 )).

fof(tlhfof45089,axiom,(
    cell98 != cell85 )).

fof(tlhfof45090,axiom,(
    cell98 != cell84 )).

fof(tlhfof45091,axiom,(
    cell98 != cell83 )).

fof(tlhfof45092,axiom,(
    cell98 != cell81 )).

fof(tlhfof45093,axiom,(
    cell98 != cell82 )).

fof(tlhfof45094,axiom,(
    cell98 != cell80 )).

fof(tlhfof45095,axiom,(
    cell98 != cell78 )).

fof(tlhfof45096,axiom,(
    cell98 != cell77 )).

fof(tlhfof45097,axiom,(
    cell98 != cell76 )).

fof(tlhfof45098,axiom,(
    cell98 != cell75 )).

fof(tlhfof45099,axiom,(
    cell98 != cell74 )).

fof(tlhfof45100,axiom,(
    cell98 != cell73 )).

fof(tlhfof45101,axiom,(
    cell98 != cell71 )).

fof(tlhfof45102,axiom,(
    cell98 != cell72 )).

fof(tlhfof45103,axiom,(
    cell98 != cell70 )).

fof(tlhfof45104,axiom,(
    cell98 != cell69 )).

fof(tlhfof45105,axiom,(
    cell98 != cell68 )).

fof(tlhfof45106,axiom,(
    cell98 != cell67 )).

fof(tlhfof45107,axiom,(
    cell98 != cell66 )).

fof(tlhfof45108,axiom,(
    cell98 != cell65 )).

fof(tlhfof45109,axiom,(
    cell98 != cell64 )).

fof(tlhfof45110,axiom,(
    cell98 != cell63 )).

fof(tlhfof45111,axiom,(
    cell98 != cell61 )).

fof(tlhfof45112,axiom,(
    cell98 != cell62 )).

fof(tlhfof45113,axiom,(
    cell98 != cell60 )).

fof(tlhfof45114,axiom,(
    cell98 != cell59 )).

fof(tlhfof45115,axiom,(
    cell98 != cell58 )).

fof(tlhfof45116,axiom,(
    cell98 != cell57 )).

fof(tlhfof45117,axiom,(
    cell98 != cell56 )).

fof(tlhfof45118,axiom,(
    cell98 != cell55 )).

fof(tlhfof45119,axiom,(
    cell98 != cell54 )).

fof(tlhfof45120,axiom,(
    cell98 != cell53 )).

fof(tlhfof45121,axiom,(
    cell98 != cell51 )).

fof(tlhfof45122,axiom,(
    cell98 != cell52 )).

fof(tlhfof45123,axiom,(
    cell98 != cell50 )).

fof(tlhfof45124,axiom,(
    cell98 != cell49 )).

fof(tlhfof45125,axiom,(
    cell98 != cell48 )).

fof(tlhfof45126,axiom,(
    cell98 != cell47 )).

fof(tlhfof45127,axiom,(
    cell98 != cell46 )).

fof(tlhfof45128,axiom,(
    cell98 != cell45 )).

fof(tlhfof45129,axiom,(
    cell98 != cell44 )).

fof(tlhfof45130,axiom,(
    cell98 != cell43 )).

fof(tlhfof45131,axiom,(
    cell98 != cell41 )).

fof(tlhfof45132,axiom,(
    cell98 != cell42 )).

fof(tlhfof45133,axiom,(
    cell98 != cell40 )).

fof(tlhfof45134,axiom,(
    cell98 != cell39 )).

fof(tlhfof45135,axiom,(
    cell98 != cell38 )).

fof(tlhfof45136,axiom,(
    cell98 != cell37 )).

fof(tlhfof45137,axiom,(
    cell98 != cell36 )).

fof(tlhfof45138,axiom,(
    cell98 != cell35 )).

fof(tlhfof45139,axiom,(
    cell98 != cell34 )).

fof(tlhfof45140,axiom,(
    cell98 != cell33 )).

fof(tlhfof45141,axiom,(
    cell98 != cell31 )).

fof(tlhfof45142,axiom,(
    cell98 != cell32 )).

fof(tlhfof45143,axiom,(
    cell98 != cell30 )).

fof(tlhfof45144,axiom,(
    cell98 != cell29 )).

fof(tlhfof45145,axiom,(
    cell98 != cell28 )).

fof(tlhfof45146,axiom,(
    cell98 != cell27 )).

fof(tlhfof45147,axiom,(
    cell98 != cell26 )).

fof(tlhfof45148,axiom,(
    cell98 != cell25 )).

fof(tlhfof45149,axiom,(
    cell98 != cell24 )).

fof(tlhfof45150,axiom,(
    cell98 != cell23 )).

fof(tlhfof45151,axiom,(
    cell98 != cell21 )).

fof(tlhfof45152,axiom,(
    cell98 != cell20 )).

fof(tlhfof45153,axiom,(
    cell98 != cell19 )).

fof(tlhfof45154,axiom,(
    cell98 != cell18 )).

fof(tlhfof45155,axiom,(
    cell98 != cell17 )).

fof(tlhfof45156,axiom,(
    cell98 != cell16 )).

fof(tlhfof45157,axiom,(
    cell98 != cell15 )).

fof(tlhfof45158,axiom,(
    cell98 != cell14 )).

fof(tlhfof45159,axiom,(
    cell98 != cell10 )).

fof(tlhfof45160,axiom,(
    cell98 != cell9 )).

fof(tlhfof45161,axiom,(
    cell98 != cell8 )).

fof(tlhfof45162,axiom,(
    cell98 != cell7 )).

fof(tlhfof45163,axiom,(
    cell98 != cell6 )).

fof(tlhfof45164,axiom,(
    cell98 != cell5 )).

fof(tlhfof45165,axiom,(
    cell98 != cell4 )).

fof(tlhfof45166,axiom,(
    cell98 != cell3 )).

fof(tlhfof45167,axiom,(
    cell98 != cell1 )).

fof(tlhfof45168,axiom,(
    cell98 != cell89 )).

fof(tlhfof45169,axiom,(
    cell98 != cell88 )).

fof(tlhfof45170,axiom,(
    cell98 != cell79 )).

fof(tlhfof45171,axiom,(
    cell98 != cell90 )).

fof(tlhfof45172,axiom,(
    cell98 != cell99 )).

fof(tlhfof45173,axiom,(
    cell98 != cell12 )).

fof(tlhfof45174,axiom,(
    cell98 != cell11 )).

fof(tlhfof45175,axiom,(
    cell98 != cell2 )).

fof(tlhfof45176,axiom,(
    cell98 != cell13 )).

fof(tlhfof45177,axiom,(
    cell98 != cell22 )).

fof(tlhfof45178,axiom,(
    cell97 != cell96 )).

fof(tlhfof45179,axiom,(
    cell97 != cell95 )).

fof(tlhfof45180,axiom,(
    cell97 != cell94 )).

fof(tlhfof45181,axiom,(
    cell97 != cell93 )).

fof(tlhfof45182,axiom,(
    cell97 != cell91 )).

fof(tlhfof45183,axiom,(
    cell97 != cell92 )).

fof(tlhfof45184,axiom,(
    cell97 != cell87 )).

fof(tlhfof45185,axiom,(
    cell97 != cell86 )).

fof(tlhfof45186,axiom,(
    cell97 != cell85 )).

fof(tlhfof45187,axiom,(
    cell97 != cell84 )).

fof(tlhfof45188,axiom,(
    cell97 != cell83 )).

fof(tlhfof45189,axiom,(
    cell97 != cell81 )).

fof(tlhfof45190,axiom,(
    cell97 != cell82 )).

fof(tlhfof45191,axiom,(
    cell97 != cell80 )).

fof(tlhfof45192,axiom,(
    cell97 != cell78 )).

fof(tlhfof45193,axiom,(
    cell97 != cell77 )).

fof(tlhfof45194,axiom,(
    cell97 != cell76 )).

fof(tlhfof45195,axiom,(
    cell97 != cell75 )).

fof(tlhfof45196,axiom,(
    cell97 != cell74 )).

fof(tlhfof45197,axiom,(
    cell97 != cell73 )).

fof(tlhfof45198,axiom,(
    cell97 != cell71 )).

fof(tlhfof45199,axiom,(
    cell97 != cell72 )).

fof(tlhfof45200,axiom,(
    cell97 != cell70 )).

fof(tlhfof45201,axiom,(
    cell97 != cell69 )).

fof(tlhfof45202,axiom,(
    cell97 != cell68 )).

fof(tlhfof45203,axiom,(
    cell97 != cell67 )).

fof(tlhfof45204,axiom,(
    cell97 != cell66 )).

fof(tlhfof45205,axiom,(
    cell97 != cell65 )).

fof(tlhfof45206,axiom,(
    cell97 != cell64 )).

fof(tlhfof45207,axiom,(
    cell97 != cell63 )).

fof(tlhfof45208,axiom,(
    cell97 != cell61 )).

fof(tlhfof45209,axiom,(
    cell97 != cell62 )).

fof(tlhfof45210,axiom,(
    cell97 != cell60 )).

fof(tlhfof45211,axiom,(
    cell97 != cell59 )).

fof(tlhfof45212,axiom,(
    cell97 != cell58 )).

fof(tlhfof45213,axiom,(
    cell97 != cell57 )).

fof(tlhfof45214,axiom,(
    cell97 != cell56 )).

fof(tlhfof45215,axiom,(
    cell97 != cell55 )).

fof(tlhfof45216,axiom,(
    cell97 != cell54 )).

fof(tlhfof45217,axiom,(
    cell97 != cell53 )).

fof(tlhfof45218,axiom,(
    cell97 != cell51 )).

fof(tlhfof45219,axiom,(
    cell97 != cell52 )).

fof(tlhfof45220,axiom,(
    cell97 != cell50 )).

fof(tlhfof45221,axiom,(
    cell97 != cell49 )).

fof(tlhfof45222,axiom,(
    cell97 != cell48 )).

fof(tlhfof45223,axiom,(
    cell97 != cell47 )).

fof(tlhfof45224,axiom,(
    cell97 != cell46 )).

fof(tlhfof45225,axiom,(
    cell97 != cell45 )).

fof(tlhfof45226,axiom,(
    cell97 != cell44 )).

fof(tlhfof45227,axiom,(
    cell97 != cell43 )).

fof(tlhfof45228,axiom,(
    cell97 != cell41 )).

fof(tlhfof45229,axiom,(
    cell97 != cell42 )).

fof(tlhfof45230,axiom,(
    cell97 != cell40 )).

fof(tlhfof45231,axiom,(
    cell97 != cell39 )).

fof(tlhfof45232,axiom,(
    cell97 != cell38 )).

fof(tlhfof45233,axiom,(
    cell97 != cell37 )).

fof(tlhfof45234,axiom,(
    cell97 != cell36 )).

fof(tlhfof45235,axiom,(
    cell97 != cell35 )).

fof(tlhfof45236,axiom,(
    cell97 != cell34 )).

fof(tlhfof45237,axiom,(
    cell97 != cell33 )).

fof(tlhfof45238,axiom,(
    cell97 != cell31 )).

fof(tlhfof45239,axiom,(
    cell97 != cell32 )).

fof(tlhfof45240,axiom,(
    cell97 != cell30 )).

fof(tlhfof45241,axiom,(
    cell97 != cell29 )).

fof(tlhfof45242,axiom,(
    cell97 != cell28 )).

fof(tlhfof45243,axiom,(
    cell97 != cell27 )).

fof(tlhfof45244,axiom,(
    cell97 != cell26 )).

fof(tlhfof45245,axiom,(
    cell97 != cell25 )).

fof(tlhfof45246,axiom,(
    cell97 != cell24 )).

fof(tlhfof45247,axiom,(
    cell97 != cell23 )).

fof(tlhfof45248,axiom,(
    cell97 != cell21 )).

fof(tlhfof45249,axiom,(
    cell97 != cell20 )).

fof(tlhfof45250,axiom,(
    cell97 != cell19 )).

fof(tlhfof45251,axiom,(
    cell97 != cell18 )).

fof(tlhfof45252,axiom,(
    cell97 != cell17 )).

fof(tlhfof45253,axiom,(
    cell97 != cell16 )).

fof(tlhfof45254,axiom,(
    cell97 != cell15 )).

fof(tlhfof45255,axiom,(
    cell97 != cell14 )).

fof(tlhfof45256,axiom,(
    cell97 != cell10 )).

fof(tlhfof45257,axiom,(
    cell97 != cell9 )).

fof(tlhfof45258,axiom,(
    cell97 != cell8 )).

fof(tlhfof45259,axiom,(
    cell97 != cell7 )).

fof(tlhfof45260,axiom,(
    cell97 != cell6 )).

fof(tlhfof45261,axiom,(
    cell97 != cell5 )).

fof(tlhfof45262,axiom,(
    cell97 != cell4 )).

fof(tlhfof45263,axiom,(
    cell97 != cell3 )).

fof(tlhfof45264,axiom,(
    cell97 != cell1 )).

fof(tlhfof45265,axiom,(
    cell97 != cell89 )).

fof(tlhfof45266,axiom,(
    cell97 != cell88 )).

fof(tlhfof45267,axiom,(
    cell97 != cell79 )).

fof(tlhfof45268,axiom,(
    cell97 != cell90 )).

fof(tlhfof45269,axiom,(
    cell97 != cell99 )).

fof(tlhfof45270,axiom,(
    cell97 != cell12 )).

fof(tlhfof45271,axiom,(
    cell97 != cell11 )).

fof(tlhfof45272,axiom,(
    cell97 != cell2 )).

fof(tlhfof45273,axiom,(
    cell97 != cell13 )).

fof(tlhfof45274,axiom,(
    cell97 != cell22 )).

fof(tlhfof45275,axiom,(
    cell96 != cell95 )).

fof(tlhfof45276,axiom,(
    cell96 != cell94 )).

fof(tlhfof45277,axiom,(
    cell96 != cell93 )).

fof(tlhfof45278,axiom,(
    cell96 != cell91 )).

fof(tlhfof45279,axiom,(
    cell96 != cell92 )).

fof(tlhfof45280,axiom,(
    cell96 != cell87 )).

fof(tlhfof45281,axiom,(
    cell96 != cell86 )).

fof(tlhfof45282,axiom,(
    cell96 != cell85 )).

fof(tlhfof45283,axiom,(
    cell96 != cell84 )).

fof(tlhfof45284,axiom,(
    cell96 != cell83 )).

fof(tlhfof45285,axiom,(
    cell96 != cell81 )).

fof(tlhfof45286,axiom,(
    cell96 != cell82 )).

fof(tlhfof45287,axiom,(
    cell96 != cell80 )).

fof(tlhfof45288,axiom,(
    cell96 != cell78 )).

fof(tlhfof45289,axiom,(
    cell96 != cell77 )).

fof(tlhfof45290,axiom,(
    cell96 != cell76 )).

fof(tlhfof45291,axiom,(
    cell96 != cell75 )).

fof(tlhfof45292,axiom,(
    cell96 != cell74 )).

fof(tlhfof45293,axiom,(
    cell96 != cell73 )).

fof(tlhfof45294,axiom,(
    cell96 != cell71 )).

fof(tlhfof45295,axiom,(
    cell96 != cell72 )).

fof(tlhfof45296,axiom,(
    cell96 != cell70 )).

fof(tlhfof45297,axiom,(
    cell96 != cell69 )).

fof(tlhfof45298,axiom,(
    cell96 != cell68 )).

fof(tlhfof45299,axiom,(
    cell96 != cell67 )).

fof(tlhfof45300,axiom,(
    cell96 != cell66 )).

fof(tlhfof45301,axiom,(
    cell96 != cell65 )).

fof(tlhfof45302,axiom,(
    cell96 != cell64 )).

fof(tlhfof45303,axiom,(
    cell96 != cell63 )).

fof(tlhfof45304,axiom,(
    cell96 != cell61 )).

fof(tlhfof45305,axiom,(
    cell96 != cell62 )).

fof(tlhfof45306,axiom,(
    cell96 != cell60 )).

fof(tlhfof45307,axiom,(
    cell96 != cell59 )).

fof(tlhfof45308,axiom,(
    cell96 != cell58 )).

fof(tlhfof45309,axiom,(
    cell96 != cell57 )).

fof(tlhfof45310,axiom,(
    cell96 != cell56 )).

fof(tlhfof45311,axiom,(
    cell96 != cell55 )).

fof(tlhfof45312,axiom,(
    cell96 != cell54 )).

fof(tlhfof45313,axiom,(
    cell96 != cell53 )).

fof(tlhfof45314,axiom,(
    cell96 != cell51 )).

fof(tlhfof45315,axiom,(
    cell96 != cell52 )).

fof(tlhfof45316,axiom,(
    cell96 != cell50 )).

fof(tlhfof45317,axiom,(
    cell96 != cell49 )).

fof(tlhfof45318,axiom,(
    cell96 != cell48 )).

fof(tlhfof45319,axiom,(
    cell96 != cell47 )).

fof(tlhfof45320,axiom,(
    cell96 != cell46 )).

fof(tlhfof45321,axiom,(
    cell96 != cell45 )).

fof(tlhfof45322,axiom,(
    cell96 != cell44 )).

fof(tlhfof45323,axiom,(
    cell96 != cell43 )).

fof(tlhfof45324,axiom,(
    cell96 != cell41 )).

fof(tlhfof45325,axiom,(
    cell96 != cell42 )).

fof(tlhfof45326,axiom,(
    cell96 != cell40 )).

fof(tlhfof45327,axiom,(
    cell96 != cell39 )).

fof(tlhfof45328,axiom,(
    cell96 != cell38 )).

fof(tlhfof45329,axiom,(
    cell96 != cell37 )).

fof(tlhfof45330,axiom,(
    cell96 != cell36 )).

fof(tlhfof45331,axiom,(
    cell96 != cell35 )).

fof(tlhfof45332,axiom,(
    cell96 != cell34 )).

fof(tlhfof45333,axiom,(
    cell96 != cell33 )).

fof(tlhfof45334,axiom,(
    cell96 != cell31 )).

fof(tlhfof45335,axiom,(
    cell96 != cell32 )).

fof(tlhfof45336,axiom,(
    cell96 != cell30 )).

fof(tlhfof45337,axiom,(
    cell96 != cell29 )).

fof(tlhfof45338,axiom,(
    cell96 != cell28 )).

fof(tlhfof45339,axiom,(
    cell96 != cell27 )).

fof(tlhfof45340,axiom,(
    cell96 != cell26 )).

fof(tlhfof45341,axiom,(
    cell96 != cell25 )).

fof(tlhfof45342,axiom,(
    cell96 != cell24 )).

fof(tlhfof45343,axiom,(
    cell96 != cell23 )).

fof(tlhfof45344,axiom,(
    cell96 != cell21 )).

fof(tlhfof45345,axiom,(
    cell96 != cell20 )).

fof(tlhfof45346,axiom,(
    cell96 != cell19 )).

fof(tlhfof45347,axiom,(
    cell96 != cell18 )).

fof(tlhfof45348,axiom,(
    cell96 != cell17 )).

fof(tlhfof45349,axiom,(
    cell96 != cell16 )).

fof(tlhfof45350,axiom,(
    cell96 != cell15 )).

fof(tlhfof45351,axiom,(
    cell96 != cell14 )).

fof(tlhfof45352,axiom,(
    cell96 != cell10 )).

fof(tlhfof45353,axiom,(
    cell96 != cell9 )).

fof(tlhfof45354,axiom,(
    cell96 != cell8 )).

fof(tlhfof45355,axiom,(
    cell96 != cell7 )).

fof(tlhfof45356,axiom,(
    cell96 != cell6 )).

fof(tlhfof45357,axiom,(
    cell96 != cell5 )).

fof(tlhfof45358,axiom,(
    cell96 != cell4 )).

fof(tlhfof45359,axiom,(
    cell96 != cell3 )).

fof(tlhfof45360,axiom,(
    cell96 != cell1 )).

fof(tlhfof45361,axiom,(
    cell96 != cell89 )).

fof(tlhfof45362,axiom,(
    cell96 != cell88 )).

fof(tlhfof45363,axiom,(
    cell96 != cell79 )).

fof(tlhfof45364,axiom,(
    cell96 != cell90 )).

fof(tlhfof45365,axiom,(
    cell96 != cell99 )).

fof(tlhfof45366,axiom,(
    cell96 != cell12 )).

fof(tlhfof45367,axiom,(
    cell96 != cell11 )).

fof(tlhfof45368,axiom,(
    cell96 != cell2 )).

fof(tlhfof45369,axiom,(
    cell96 != cell13 )).

fof(tlhfof45370,axiom,(
    cell96 != cell22 )).

fof(tlhfof45371,axiom,(
    cell95 != cell94 )).

fof(tlhfof45372,axiom,(
    cell95 != cell93 )).

fof(tlhfof45373,axiom,(
    cell95 != cell91 )).

fof(tlhfof45374,axiom,(
    cell95 != cell92 )).

fof(tlhfof45375,axiom,(
    cell95 != cell87 )).

fof(tlhfof45376,axiom,(
    cell95 != cell86 )).

fof(tlhfof45377,axiom,(
    cell95 != cell85 )).

fof(tlhfof45378,axiom,(
    cell95 != cell84 )).

fof(tlhfof45379,axiom,(
    cell95 != cell83 )).

fof(tlhfof45380,axiom,(
    cell95 != cell81 )).

fof(tlhfof45381,axiom,(
    cell95 != cell82 )).

fof(tlhfof45382,axiom,(
    cell95 != cell80 )).

fof(tlhfof45383,axiom,(
    cell95 != cell78 )).

fof(tlhfof45384,axiom,(
    cell95 != cell77 )).

fof(tlhfof45385,axiom,(
    cell95 != cell76 )).

fof(tlhfof45386,axiom,(
    cell95 != cell75 )).

fof(tlhfof45387,axiom,(
    cell95 != cell74 )).

fof(tlhfof45388,axiom,(
    cell95 != cell73 )).

fof(tlhfof45389,axiom,(
    cell95 != cell71 )).

fof(tlhfof45390,axiom,(
    cell95 != cell72 )).

fof(tlhfof45391,axiom,(
    cell95 != cell70 )).

fof(tlhfof45392,axiom,(
    cell95 != cell69 )).

fof(tlhfof45393,axiom,(
    cell95 != cell68 )).

fof(tlhfof45394,axiom,(
    cell95 != cell67 )).

fof(tlhfof45395,axiom,(
    cell95 != cell66 )).

fof(tlhfof45396,axiom,(
    cell95 != cell65 )).

fof(tlhfof45397,axiom,(
    cell95 != cell64 )).

fof(tlhfof45398,axiom,(
    cell95 != cell63 )).

fof(tlhfof45399,axiom,(
    cell95 != cell61 )).

fof(tlhfof45400,axiom,(
    cell95 != cell62 )).

fof(tlhfof45401,axiom,(
    cell95 != cell60 )).

fof(tlhfof45402,axiom,(
    cell95 != cell59 )).

fof(tlhfof45403,axiom,(
    cell95 != cell58 )).

fof(tlhfof45404,axiom,(
    cell95 != cell57 )).

fof(tlhfof45405,axiom,(
    cell95 != cell56 )).

fof(tlhfof45406,axiom,(
    cell95 != cell55 )).

fof(tlhfof45407,axiom,(
    cell95 != cell54 )).

fof(tlhfof45408,axiom,(
    cell95 != cell53 )).

fof(tlhfof45409,axiom,(
    cell95 != cell51 )).

fof(tlhfof45410,axiom,(
    cell95 != cell52 )).

fof(tlhfof45411,axiom,(
    cell95 != cell50 )).

fof(tlhfof45412,axiom,(
    cell95 != cell49 )).

fof(tlhfof45413,axiom,(
    cell95 != cell48 )).

fof(tlhfof45414,axiom,(
    cell95 != cell47 )).

fof(tlhfof45415,axiom,(
    cell95 != cell46 )).

fof(tlhfof45416,axiom,(
    cell95 != cell45 )).

fof(tlhfof45417,axiom,(
    cell95 != cell44 )).

fof(tlhfof45418,axiom,(
    cell95 != cell43 )).

fof(tlhfof45419,axiom,(
    cell95 != cell41 )).

fof(tlhfof45420,axiom,(
    cell95 != cell42 )).

fof(tlhfof45421,axiom,(
    cell95 != cell40 )).

fof(tlhfof45422,axiom,(
    cell95 != cell39 )).

fof(tlhfof45423,axiom,(
    cell95 != cell38 )).

fof(tlhfof45424,axiom,(
    cell95 != cell37 )).

fof(tlhfof45425,axiom,(
    cell95 != cell36 )).

fof(tlhfof45426,axiom,(
    cell95 != cell35 )).

fof(tlhfof45427,axiom,(
    cell95 != cell34 )).

fof(tlhfof45428,axiom,(
    cell95 != cell33 )).

fof(tlhfof45429,axiom,(
    cell95 != cell31 )).

fof(tlhfof45430,axiom,(
    cell95 != cell32 )).

fof(tlhfof45431,axiom,(
    cell95 != cell30 )).

fof(tlhfof45432,axiom,(
    cell95 != cell29 )).

fof(tlhfof45433,axiom,(
    cell95 != cell28 )).

fof(tlhfof45434,axiom,(
    cell95 != cell27 )).

fof(tlhfof45435,axiom,(
    cell95 != cell26 )).

fof(tlhfof45436,axiom,(
    cell95 != cell25 )).

fof(tlhfof45437,axiom,(
    cell95 != cell24 )).

fof(tlhfof45438,axiom,(
    cell95 != cell23 )).

fof(tlhfof45439,axiom,(
    cell95 != cell21 )).

fof(tlhfof45440,axiom,(
    cell95 != cell20 )).

fof(tlhfof45441,axiom,(
    cell95 != cell19 )).

fof(tlhfof45442,axiom,(
    cell95 != cell18 )).

fof(tlhfof45443,axiom,(
    cell95 != cell17 )).

fof(tlhfof45444,axiom,(
    cell95 != cell16 )).

fof(tlhfof45445,axiom,(
    cell95 != cell15 )).

fof(tlhfof45446,axiom,(
    cell95 != cell14 )).

fof(tlhfof45447,axiom,(
    cell95 != cell10 )).

fof(tlhfof45448,axiom,(
    cell95 != cell9 )).

fof(tlhfof45449,axiom,(
    cell95 != cell8 )).

fof(tlhfof45450,axiom,(
    cell95 != cell7 )).

fof(tlhfof45451,axiom,(
    cell95 != cell6 )).

fof(tlhfof45452,axiom,(
    cell95 != cell5 )).

fof(tlhfof45453,axiom,(
    cell95 != cell4 )).

fof(tlhfof45454,axiom,(
    cell95 != cell3 )).

fof(tlhfof45455,axiom,(
    cell95 != cell1 )).

fof(tlhfof45456,axiom,(
    cell95 != cell89 )).

fof(tlhfof45457,axiom,(
    cell95 != cell88 )).

fof(tlhfof45458,axiom,(
    cell95 != cell79 )).

fof(tlhfof45459,axiom,(
    cell95 != cell90 )).

fof(tlhfof45460,axiom,(
    cell95 != cell99 )).

fof(tlhfof45461,axiom,(
    cell95 != cell12 )).

fof(tlhfof45462,axiom,(
    cell95 != cell11 )).

fof(tlhfof45463,axiom,(
    cell95 != cell2 )).

fof(tlhfof45464,axiom,(
    cell95 != cell13 )).

fof(tlhfof45465,axiom,(
    cell95 != cell22 )).

fof(tlhfof45466,axiom,(
    cell94 != cell93 )).

fof(tlhfof45467,axiom,(
    cell94 != cell91 )).

fof(tlhfof45468,axiom,(
    cell94 != cell92 )).

fof(tlhfof45469,axiom,(
    cell94 != cell87 )).

fof(tlhfof45470,axiom,(
    cell94 != cell86 )).

fof(tlhfof45471,axiom,(
    cell94 != cell85 )).

fof(tlhfof45472,axiom,(
    cell94 != cell84 )).

fof(tlhfof45473,axiom,(
    cell94 != cell83 )).

fof(tlhfof45474,axiom,(
    cell94 != cell81 )).

fof(tlhfof45475,axiom,(
    cell94 != cell82 )).

fof(tlhfof45476,axiom,(
    cell94 != cell80 )).

fof(tlhfof45477,axiom,(
    cell94 != cell78 )).

fof(tlhfof45478,axiom,(
    cell94 != cell77 )).

fof(tlhfof45479,axiom,(
    cell94 != cell76 )).

fof(tlhfof45480,axiom,(
    cell94 != cell75 )).

fof(tlhfof45481,axiom,(
    cell94 != cell74 )).

fof(tlhfof45482,axiom,(
    cell94 != cell73 )).

fof(tlhfof45483,axiom,(
    cell94 != cell71 )).

fof(tlhfof45484,axiom,(
    cell94 != cell72 )).

fof(tlhfof45485,axiom,(
    cell94 != cell70 )).

fof(tlhfof45486,axiom,(
    cell94 != cell69 )).

fof(tlhfof45487,axiom,(
    cell94 != cell68 )).

fof(tlhfof45488,axiom,(
    cell94 != cell67 )).

fof(tlhfof45489,axiom,(
    cell94 != cell66 )).

fof(tlhfof45490,axiom,(
    cell94 != cell65 )).

fof(tlhfof45491,axiom,(
    cell94 != cell64 )).

fof(tlhfof45492,axiom,(
    cell94 != cell63 )).

fof(tlhfof45493,axiom,(
    cell94 != cell61 )).

fof(tlhfof45494,axiom,(
    cell94 != cell62 )).

fof(tlhfof45495,axiom,(
    cell94 != cell60 )).

fof(tlhfof45496,axiom,(
    cell94 != cell59 )).

fof(tlhfof45497,axiom,(
    cell94 != cell58 )).

fof(tlhfof45498,axiom,(
    cell94 != cell57 )).

fof(tlhfof45499,axiom,(
    cell94 != cell56 )).

fof(tlhfof45500,axiom,(
    cell94 != cell55 )).

fof(tlhfof45501,axiom,(
    cell94 != cell54 )).

fof(tlhfof45502,axiom,(
    cell94 != cell53 )).

fof(tlhfof45503,axiom,(
    cell94 != cell51 )).

fof(tlhfof45504,axiom,(
    cell94 != cell52 )).

fof(tlhfof45505,axiom,(
    cell94 != cell50 )).

fof(tlhfof45506,axiom,(
    cell94 != cell49 )).

fof(tlhfof45507,axiom,(
    cell94 != cell48 )).

fof(tlhfof45508,axiom,(
    cell94 != cell47 )).

fof(tlhfof45509,axiom,(
    cell94 != cell46 )).

fof(tlhfof45510,axiom,(
    cell94 != cell45 )).

fof(tlhfof45511,axiom,(
    cell94 != cell44 )).

fof(tlhfof45512,axiom,(
    cell94 != cell43 )).

fof(tlhfof45513,axiom,(
    cell94 != cell41 )).

fof(tlhfof45514,axiom,(
    cell94 != cell42 )).

fof(tlhfof45515,axiom,(
    cell94 != cell40 )).

fof(tlhfof45516,axiom,(
    cell94 != cell39 )).

fof(tlhfof45517,axiom,(
    cell94 != cell38 )).

fof(tlhfof45518,axiom,(
    cell94 != cell37 )).

fof(tlhfof45519,axiom,(
    cell94 != cell36 )).

fof(tlhfof45520,axiom,(
    cell94 != cell35 )).

fof(tlhfof45521,axiom,(
    cell94 != cell34 )).

fof(tlhfof45522,axiom,(
    cell94 != cell33 )).

fof(tlhfof45523,axiom,(
    cell94 != cell31 )).

fof(tlhfof45524,axiom,(
    cell94 != cell32 )).

fof(tlhfof45525,axiom,(
    cell94 != cell30 )).

fof(tlhfof45526,axiom,(
    cell94 != cell29 )).

fof(tlhfof45527,axiom,(
    cell94 != cell28 )).

fof(tlhfof45528,axiom,(
    cell94 != cell27 )).

fof(tlhfof45529,axiom,(
    cell94 != cell26 )).

fof(tlhfof45530,axiom,(
    cell94 != cell25 )).

fof(tlhfof45531,axiom,(
    cell94 != cell24 )).

fof(tlhfof45532,axiom,(
    cell94 != cell23 )).

fof(tlhfof45533,axiom,(
    cell94 != cell21 )).

fof(tlhfof45534,axiom,(
    cell94 != cell20 )).

fof(tlhfof45535,axiom,(
    cell94 != cell19 )).

fof(tlhfof45536,axiom,(
    cell94 != cell18 )).

fof(tlhfof45537,axiom,(
    cell94 != cell17 )).

fof(tlhfof45538,axiom,(
    cell94 != cell16 )).

fof(tlhfof45539,axiom,(
    cell94 != cell15 )).

fof(tlhfof45540,axiom,(
    cell94 != cell14 )).

fof(tlhfof45541,axiom,(
    cell94 != cell10 )).

fof(tlhfof45542,axiom,(
    cell94 != cell9 )).

fof(tlhfof45543,axiom,(
    cell94 != cell8 )).

fof(tlhfof45544,axiom,(
    cell94 != cell7 )).

fof(tlhfof45545,axiom,(
    cell94 != cell6 )).

fof(tlhfof45546,axiom,(
    cell94 != cell5 )).

fof(tlhfof45547,axiom,(
    cell94 != cell4 )).

fof(tlhfof45548,axiom,(
    cell94 != cell3 )).

fof(tlhfof45549,axiom,(
    cell94 != cell1 )).

fof(tlhfof45550,axiom,(
    cell94 != cell89 )).

fof(tlhfof45551,axiom,(
    cell94 != cell88 )).

fof(tlhfof45552,axiom,(
    cell94 != cell79 )).

fof(tlhfof45553,axiom,(
    cell94 != cell90 )).

fof(tlhfof45554,axiom,(
    cell94 != cell99 )).

fof(tlhfof45555,axiom,(
    cell94 != cell12 )).

fof(tlhfof45556,axiom,(
    cell94 != cell11 )).

fof(tlhfof45557,axiom,(
    cell94 != cell2 )).

fof(tlhfof45558,axiom,(
    cell94 != cell13 )).

fof(tlhfof45559,axiom,(
    cell94 != cell22 )).

fof(tlhfof45560,axiom,(
    cell93 != cell91 )).

fof(tlhfof45561,axiom,(
    cell93 != cell92 )).

fof(tlhfof45562,axiom,(
    cell93 != cell87 )).

fof(tlhfof45563,axiom,(
    cell93 != cell86 )).

fof(tlhfof45564,axiom,(
    cell93 != cell85 )).

fof(tlhfof45565,axiom,(
    cell93 != cell84 )).

fof(tlhfof45566,axiom,(
    cell93 != cell83 )).

fof(tlhfof45567,axiom,(
    cell93 != cell81 )).

fof(tlhfof45568,axiom,(
    cell93 != cell82 )).

fof(tlhfof45569,axiom,(
    cell93 != cell80 )).

fof(tlhfof45570,axiom,(
    cell93 != cell78 )).

fof(tlhfof45571,axiom,(
    cell93 != cell77 )).

fof(tlhfof45572,axiom,(
    cell93 != cell76 )).

fof(tlhfof45573,axiom,(
    cell93 != cell75 )).

fof(tlhfof45574,axiom,(
    cell93 != cell74 )).

fof(tlhfof45575,axiom,(
    cell93 != cell73 )).

fof(tlhfof45576,axiom,(
    cell93 != cell71 )).

fof(tlhfof45577,axiom,(
    cell93 != cell72 )).

fof(tlhfof45578,axiom,(
    cell93 != cell70 )).

fof(tlhfof45579,axiom,(
    cell93 != cell69 )).

fof(tlhfof45580,axiom,(
    cell93 != cell68 )).

fof(tlhfof45581,axiom,(
    cell93 != cell67 )).

fof(tlhfof45582,axiom,(
    cell93 != cell66 )).

fof(tlhfof45583,axiom,(
    cell93 != cell65 )).

fof(tlhfof45584,axiom,(
    cell93 != cell64 )).

fof(tlhfof45585,axiom,(
    cell93 != cell63 )).

fof(tlhfof45586,axiom,(
    cell93 != cell61 )).

fof(tlhfof45587,axiom,(
    cell93 != cell62 )).

fof(tlhfof45588,axiom,(
    cell93 != cell60 )).

fof(tlhfof45589,axiom,(
    cell93 != cell59 )).

fof(tlhfof45590,axiom,(
    cell93 != cell58 )).

fof(tlhfof45591,axiom,(
    cell93 != cell57 )).

fof(tlhfof45592,axiom,(
    cell93 != cell56 )).

fof(tlhfof45593,axiom,(
    cell93 != cell55 )).

fof(tlhfof45594,axiom,(
    cell93 != cell54 )).

fof(tlhfof45595,axiom,(
    cell93 != cell53 )).

fof(tlhfof45596,axiom,(
    cell93 != cell51 )).

fof(tlhfof45597,axiom,(
    cell93 != cell52 )).

fof(tlhfof45598,axiom,(
    cell93 != cell50 )).

fof(tlhfof45599,axiom,(
    cell93 != cell49 )).

fof(tlhfof45600,axiom,(
    cell93 != cell48 )).

fof(tlhfof45601,axiom,(
    cell93 != cell47 )).

fof(tlhfof45602,axiom,(
    cell93 != cell46 )).

fof(tlhfof45603,axiom,(
    cell93 != cell45 )).

fof(tlhfof45604,axiom,(
    cell93 != cell44 )).

fof(tlhfof45605,axiom,(
    cell93 != cell43 )).

fof(tlhfof45606,axiom,(
    cell93 != cell41 )).

fof(tlhfof45607,axiom,(
    cell93 != cell42 )).

fof(tlhfof45608,axiom,(
    cell93 != cell40 )).

fof(tlhfof45609,axiom,(
    cell93 != cell39 )).

fof(tlhfof45610,axiom,(
    cell93 != cell38 )).

fof(tlhfof45611,axiom,(
    cell93 != cell37 )).

fof(tlhfof45612,axiom,(
    cell93 != cell36 )).

fof(tlhfof45613,axiom,(
    cell93 != cell35 )).

fof(tlhfof45614,axiom,(
    cell93 != cell34 )).

fof(tlhfof45615,axiom,(
    cell93 != cell33 )).

fof(tlhfof45616,axiom,(
    cell93 != cell31 )).

fof(tlhfof45617,axiom,(
    cell93 != cell32 )).

fof(tlhfof45618,axiom,(
    cell93 != cell30 )).

fof(tlhfof45619,axiom,(
    cell93 != cell29 )).

fof(tlhfof45620,axiom,(
    cell93 != cell28 )).

fof(tlhfof45621,axiom,(
    cell93 != cell27 )).

fof(tlhfof45622,axiom,(
    cell93 != cell26 )).

fof(tlhfof45623,axiom,(
    cell93 != cell25 )).

fof(tlhfof45624,axiom,(
    cell93 != cell24 )).

fof(tlhfof45625,axiom,(
    cell93 != cell23 )).

fof(tlhfof45626,axiom,(
    cell93 != cell21 )).

fof(tlhfof45627,axiom,(
    cell93 != cell20 )).

fof(tlhfof45628,axiom,(
    cell93 != cell19 )).

fof(tlhfof45629,axiom,(
    cell93 != cell18 )).

fof(tlhfof45630,axiom,(
    cell93 != cell17 )).

fof(tlhfof45631,axiom,(
    cell93 != cell16 )).

fof(tlhfof45632,axiom,(
    cell93 != cell15 )).

fof(tlhfof45633,axiom,(
    cell93 != cell14 )).

fof(tlhfof45634,axiom,(
    cell93 != cell10 )).

fof(tlhfof45635,axiom,(
    cell93 != cell9 )).

fof(tlhfof45636,axiom,(
    cell93 != cell8 )).

fof(tlhfof45637,axiom,(
    cell93 != cell7 )).

fof(tlhfof45638,axiom,(
    cell93 != cell6 )).

fof(tlhfof45639,axiom,(
    cell93 != cell5 )).

fof(tlhfof45640,axiom,(
    cell93 != cell4 )).

fof(tlhfof45641,axiom,(
    cell93 != cell3 )).

fof(tlhfof45642,axiom,(
    cell93 != cell1 )).

fof(tlhfof45643,axiom,(
    cell93 != cell89 )).

fof(tlhfof45644,axiom,(
    cell93 != cell88 )).

fof(tlhfof45645,axiom,(
    cell93 != cell79 )).

fof(tlhfof45646,axiom,(
    cell93 != cell90 )).

fof(tlhfof45647,axiom,(
    cell93 != cell99 )).

fof(tlhfof45648,axiom,(
    cell93 != cell12 )).

fof(tlhfof45649,axiom,(
    cell93 != cell11 )).

fof(tlhfof45650,axiom,(
    cell93 != cell2 )).

fof(tlhfof45651,axiom,(
    cell93 != cell13 )).

fof(tlhfof45652,axiom,(
    cell93 != cell22 )).

fof(tlhfof45653,axiom,(
    cell91 != cell92 )).

fof(tlhfof45654,axiom,(
    cell91 != cell87 )).

fof(tlhfof45655,axiom,(
    cell91 != cell86 )).

fof(tlhfof45656,axiom,(
    cell91 != cell85 )).

fof(tlhfof45657,axiom,(
    cell91 != cell84 )).

fof(tlhfof45658,axiom,(
    cell91 != cell83 )).

fof(tlhfof45659,axiom,(
    cell91 != cell81 )).

fof(tlhfof45660,axiom,(
    cell91 != cell82 )).

fof(tlhfof45661,axiom,(
    cell91 != cell80 )).

fof(tlhfof45662,axiom,(
    cell91 != cell78 )).

fof(tlhfof45663,axiom,(
    cell91 != cell77 )).

fof(tlhfof45664,axiom,(
    cell91 != cell76 )).

fof(tlhfof45665,axiom,(
    cell91 != cell75 )).

fof(tlhfof45666,axiom,(
    cell91 != cell74 )).

fof(tlhfof45667,axiom,(
    cell91 != cell73 )).

fof(tlhfof45668,axiom,(
    cell91 != cell71 )).

fof(tlhfof45669,axiom,(
    cell91 != cell72 )).

fof(tlhfof45670,axiom,(
    cell91 != cell70 )).

fof(tlhfof45671,axiom,(
    cell91 != cell69 )).

fof(tlhfof45672,axiom,(
    cell91 != cell68 )).

fof(tlhfof45673,axiom,(
    cell91 != cell67 )).

fof(tlhfof45674,axiom,(
    cell91 != cell66 )).

fof(tlhfof45675,axiom,(
    cell91 != cell65 )).

fof(tlhfof45676,axiom,(
    cell91 != cell64 )).

fof(tlhfof45677,axiom,(
    cell91 != cell63 )).

fof(tlhfof45678,axiom,(
    cell91 != cell61 )).

fof(tlhfof45679,axiom,(
    cell91 != cell62 )).

fof(tlhfof45680,axiom,(
    cell91 != cell60 )).

fof(tlhfof45681,axiom,(
    cell91 != cell59 )).

fof(tlhfof45682,axiom,(
    cell91 != cell58 )).

fof(tlhfof45683,axiom,(
    cell91 != cell57 )).

fof(tlhfof45684,axiom,(
    cell91 != cell56 )).

fof(tlhfof45685,axiom,(
    cell91 != cell55 )).

fof(tlhfof45686,axiom,(
    cell91 != cell54 )).

fof(tlhfof45687,axiom,(
    cell91 != cell53 )).

fof(tlhfof45688,axiom,(
    cell91 != cell51 )).

fof(tlhfof45689,axiom,(
    cell91 != cell52 )).

fof(tlhfof45690,axiom,(
    cell91 != cell50 )).

fof(tlhfof45691,axiom,(
    cell91 != cell49 )).

fof(tlhfof45692,axiom,(
    cell91 != cell48 )).

fof(tlhfof45693,axiom,(
    cell91 != cell47 )).

fof(tlhfof45694,axiom,(
    cell91 != cell46 )).

fof(tlhfof45695,axiom,(
    cell91 != cell45 )).

fof(tlhfof45696,axiom,(
    cell91 != cell44 )).

fof(tlhfof45697,axiom,(
    cell91 != cell43 )).

fof(tlhfof45698,axiom,(
    cell91 != cell41 )).

fof(tlhfof45699,axiom,(
    cell91 != cell42 )).

fof(tlhfof45700,axiom,(
    cell91 != cell40 )).

fof(tlhfof45701,axiom,(
    cell91 != cell39 )).

fof(tlhfof45702,axiom,(
    cell91 != cell38 )).

fof(tlhfof45703,axiom,(
    cell91 != cell37 )).

fof(tlhfof45704,axiom,(
    cell91 != cell36 )).

fof(tlhfof45705,axiom,(
    cell91 != cell35 )).

fof(tlhfof45706,axiom,(
    cell91 != cell34 )).

fof(tlhfof45707,axiom,(
    cell91 != cell33 )).

fof(tlhfof45708,axiom,(
    cell91 != cell31 )).

fof(tlhfof45709,axiom,(
    cell91 != cell32 )).

fof(tlhfof45710,axiom,(
    cell91 != cell30 )).

fof(tlhfof45711,axiom,(
    cell91 != cell29 )).

fof(tlhfof45712,axiom,(
    cell91 != cell28 )).

fof(tlhfof45713,axiom,(
    cell91 != cell27 )).

fof(tlhfof45714,axiom,(
    cell91 != cell26 )).

fof(tlhfof45715,axiom,(
    cell91 != cell25 )).

fof(tlhfof45716,axiom,(
    cell91 != cell24 )).

fof(tlhfof45717,axiom,(
    cell91 != cell23 )).

fof(tlhfof45718,axiom,(
    cell91 != cell21 )).

fof(tlhfof45719,axiom,(
    cell91 != cell20 )).

fof(tlhfof45720,axiom,(
    cell91 != cell19 )).

fof(tlhfof45721,axiom,(
    cell91 != cell18 )).

fof(tlhfof45722,axiom,(
    cell91 != cell17 )).

fof(tlhfof45723,axiom,(
    cell91 != cell16 )).

fof(tlhfof45724,axiom,(
    cell91 != cell15 )).

fof(tlhfof45725,axiom,(
    cell91 != cell14 )).

fof(tlhfof45726,axiom,(
    cell91 != cell10 )).

fof(tlhfof45727,axiom,(
    cell91 != cell9 )).

fof(tlhfof45728,axiom,(
    cell91 != cell8 )).

fof(tlhfof45729,axiom,(
    cell91 != cell7 )).

fof(tlhfof45730,axiom,(
    cell91 != cell6 )).

fof(tlhfof45731,axiom,(
    cell91 != cell5 )).

fof(tlhfof45732,axiom,(
    cell91 != cell4 )).

fof(tlhfof45733,axiom,(
    cell91 != cell3 )).

fof(tlhfof45734,axiom,(
    cell91 != cell1 )).

fof(tlhfof45735,axiom,(
    cell91 != cell89 )).

fof(tlhfof45736,axiom,(
    cell91 != cell88 )).

fof(tlhfof45737,axiom,(
    cell91 != cell79 )).

fof(tlhfof45738,axiom,(
    cell91 != cell90 )).

fof(tlhfof45739,axiom,(
    cell91 != cell99 )).

fof(tlhfof45740,axiom,(
    cell91 != cell12 )).

fof(tlhfof45741,axiom,(
    cell91 != cell11 )).

fof(tlhfof45742,axiom,(
    cell91 != cell2 )).

fof(tlhfof45743,axiom,(
    cell91 != cell13 )).

fof(tlhfof45744,axiom,(
    cell91 != cell22 )).

fof(tlhfof45745,axiom,(
    cell92 != cell87 )).

fof(tlhfof45746,axiom,(
    cell92 != cell86 )).

fof(tlhfof45747,axiom,(
    cell92 != cell85 )).

fof(tlhfof45748,axiom,(
    cell92 != cell84 )).

fof(tlhfof45749,axiom,(
    cell92 != cell83 )).

fof(tlhfof45750,axiom,(
    cell92 != cell81 )).

fof(tlhfof45751,axiom,(
    cell92 != cell82 )).

fof(tlhfof45752,axiom,(
    cell92 != cell80 )).

fof(tlhfof45753,axiom,(
    cell92 != cell78 )).

fof(tlhfof45754,axiom,(
    cell92 != cell77 )).

fof(tlhfof45755,axiom,(
    cell92 != cell76 )).

fof(tlhfof45756,axiom,(
    cell92 != cell75 )).

fof(tlhfof45757,axiom,(
    cell92 != cell74 )).

fof(tlhfof45758,axiom,(
    cell92 != cell73 )).

fof(tlhfof45759,axiom,(
    cell92 != cell71 )).

fof(tlhfof45760,axiom,(
    cell92 != cell72 )).

fof(tlhfof45761,axiom,(
    cell92 != cell70 )).

fof(tlhfof45762,axiom,(
    cell92 != cell69 )).

fof(tlhfof45763,axiom,(
    cell92 != cell68 )).

fof(tlhfof45764,axiom,(
    cell92 != cell67 )).

fof(tlhfof45765,axiom,(
    cell92 != cell66 )).

fof(tlhfof45766,axiom,(
    cell92 != cell65 )).

fof(tlhfof45767,axiom,(
    cell92 != cell64 )).

fof(tlhfof45768,axiom,(
    cell92 != cell63 )).

fof(tlhfof45769,axiom,(
    cell92 != cell61 )).

fof(tlhfof45770,axiom,(
    cell92 != cell62 )).

fof(tlhfof45771,axiom,(
    cell92 != cell60 )).

fof(tlhfof45772,axiom,(
    cell92 != cell59 )).

fof(tlhfof45773,axiom,(
    cell92 != cell58 )).

fof(tlhfof45774,axiom,(
    cell92 != cell57 )).

fof(tlhfof45775,axiom,(
    cell92 != cell56 )).

fof(tlhfof45776,axiom,(
    cell92 != cell55 )).

fof(tlhfof45777,axiom,(
    cell92 != cell54 )).

fof(tlhfof45778,axiom,(
    cell92 != cell53 )).

fof(tlhfof45779,axiom,(
    cell92 != cell51 )).

fof(tlhfof45780,axiom,(
    cell92 != cell52 )).

fof(tlhfof45781,axiom,(
    cell92 != cell50 )).

fof(tlhfof45782,axiom,(
    cell92 != cell49 )).

fof(tlhfof45783,axiom,(
    cell92 != cell48 )).

fof(tlhfof45784,axiom,(
    cell92 != cell47 )).

fof(tlhfof45785,axiom,(
    cell92 != cell46 )).

fof(tlhfof45786,axiom,(
    cell92 != cell45 )).

fof(tlhfof45787,axiom,(
    cell92 != cell44 )).

fof(tlhfof45788,axiom,(
    cell92 != cell43 )).

fof(tlhfof45789,axiom,(
    cell92 != cell41 )).

fof(tlhfof45790,axiom,(
    cell92 != cell42 )).

fof(tlhfof45791,axiom,(
    cell92 != cell40 )).

fof(tlhfof45792,axiom,(
    cell92 != cell39 )).

fof(tlhfof45793,axiom,(
    cell92 != cell38 )).

fof(tlhfof45794,axiom,(
    cell92 != cell37 )).

fof(tlhfof45795,axiom,(
    cell92 != cell36 )).

fof(tlhfof45796,axiom,(
    cell92 != cell35 )).

fof(tlhfof45797,axiom,(
    cell92 != cell34 )).

fof(tlhfof45798,axiom,(
    cell92 != cell33 )).

fof(tlhfof45799,axiom,(
    cell92 != cell31 )).

fof(tlhfof45800,axiom,(
    cell92 != cell32 )).

fof(tlhfof45801,axiom,(
    cell92 != cell30 )).

fof(tlhfof45802,axiom,(
    cell92 != cell29 )).

fof(tlhfof45803,axiom,(
    cell92 != cell28 )).

fof(tlhfof45804,axiom,(
    cell92 != cell27 )).

fof(tlhfof45805,axiom,(
    cell92 != cell26 )).

fof(tlhfof45806,axiom,(
    cell92 != cell25 )).

fof(tlhfof45807,axiom,(
    cell92 != cell24 )).

fof(tlhfof45808,axiom,(
    cell92 != cell23 )).

fof(tlhfof45809,axiom,(
    cell92 != cell21 )).

fof(tlhfof45810,axiom,(
    cell92 != cell20 )).

fof(tlhfof45811,axiom,(
    cell92 != cell19 )).

fof(tlhfof45812,axiom,(
    cell92 != cell18 )).

fof(tlhfof45813,axiom,(
    cell92 != cell17 )).

fof(tlhfof45814,axiom,(
    cell92 != cell16 )).

fof(tlhfof45815,axiom,(
    cell92 != cell15 )).

fof(tlhfof45816,axiom,(
    cell92 != cell14 )).

fof(tlhfof45817,axiom,(
    cell92 != cell10 )).

fof(tlhfof45818,axiom,(
    cell92 != cell9 )).

fof(tlhfof45819,axiom,(
    cell92 != cell8 )).

fof(tlhfof45820,axiom,(
    cell92 != cell7 )).

fof(tlhfof45821,axiom,(
    cell92 != cell6 )).

fof(tlhfof45822,axiom,(
    cell92 != cell5 )).

fof(tlhfof45823,axiom,(
    cell92 != cell4 )).

fof(tlhfof45824,axiom,(
    cell92 != cell3 )).

fof(tlhfof45825,axiom,(
    cell92 != cell1 )).

fof(tlhfof45826,axiom,(
    cell92 != cell89 )).

fof(tlhfof45827,axiom,(
    cell92 != cell88 )).

fof(tlhfof45828,axiom,(
    cell92 != cell79 )).

fof(tlhfof45829,axiom,(
    cell92 != cell90 )).

fof(tlhfof45830,axiom,(
    cell92 != cell99 )).

fof(tlhfof45831,axiom,(
    cell92 != cell12 )).

fof(tlhfof45832,axiom,(
    cell92 != cell11 )).

fof(tlhfof45833,axiom,(
    cell92 != cell2 )).

fof(tlhfof45834,axiom,(
    cell92 != cell13 )).

fof(tlhfof45835,axiom,(
    cell92 != cell22 )).

fof(tlhfof45836,axiom,(
    cell87 != cell86 )).

fof(tlhfof45837,axiom,(
    cell87 != cell85 )).

fof(tlhfof45838,axiom,(
    cell87 != cell84 )).

fof(tlhfof45839,axiom,(
    cell87 != cell83 )).

fof(tlhfof45840,axiom,(
    cell87 != cell81 )).

fof(tlhfof45841,axiom,(
    cell87 != cell82 )).

fof(tlhfof45842,axiom,(
    cell87 != cell80 )).

fof(tlhfof45843,axiom,(
    cell87 != cell78 )).

fof(tlhfof45844,axiom,(
    cell87 != cell77 )).

fof(tlhfof45845,axiom,(
    cell87 != cell76 )).

fof(tlhfof45846,axiom,(
    cell87 != cell75 )).

fof(tlhfof45847,axiom,(
    cell87 != cell74 )).

fof(tlhfof45848,axiom,(
    cell87 != cell73 )).

fof(tlhfof45849,axiom,(
    cell87 != cell71 )).

fof(tlhfof45850,axiom,(
    cell87 != cell72 )).

fof(tlhfof45851,axiom,(
    cell87 != cell70 )).

fof(tlhfof45852,axiom,(
    cell87 != cell69 )).

fof(tlhfof45853,axiom,(
    cell87 != cell68 )).

fof(tlhfof45854,axiom,(
    cell87 != cell67 )).

fof(tlhfof45855,axiom,(
    cell87 != cell66 )).

fof(tlhfof45856,axiom,(
    cell87 != cell65 )).

fof(tlhfof45857,axiom,(
    cell87 != cell64 )).

fof(tlhfof45858,axiom,(
    cell87 != cell63 )).

fof(tlhfof45859,axiom,(
    cell87 != cell61 )).

fof(tlhfof45860,axiom,(
    cell87 != cell62 )).

fof(tlhfof45861,axiom,(
    cell87 != cell60 )).

fof(tlhfof45862,axiom,(
    cell87 != cell59 )).

fof(tlhfof45863,axiom,(
    cell87 != cell58 )).

fof(tlhfof45864,axiom,(
    cell87 != cell57 )).

fof(tlhfof45865,axiom,(
    cell87 != cell56 )).

fof(tlhfof45866,axiom,(
    cell87 != cell55 )).

fof(tlhfof45867,axiom,(
    cell87 != cell54 )).

fof(tlhfof45868,axiom,(
    cell87 != cell53 )).

fof(tlhfof45869,axiom,(
    cell87 != cell51 )).

fof(tlhfof45870,axiom,(
    cell87 != cell52 )).

fof(tlhfof45871,axiom,(
    cell87 != cell50 )).

fof(tlhfof45872,axiom,(
    cell87 != cell49 )).

fof(tlhfof45873,axiom,(
    cell87 != cell48 )).

fof(tlhfof45874,axiom,(
    cell87 != cell47 )).

fof(tlhfof45875,axiom,(
    cell87 != cell46 )).

fof(tlhfof45876,axiom,(
    cell87 != cell45 )).

fof(tlhfof45877,axiom,(
    cell87 != cell44 )).

fof(tlhfof45878,axiom,(
    cell87 != cell43 )).

fof(tlhfof45879,axiom,(
    cell87 != cell41 )).

fof(tlhfof45880,axiom,(
    cell87 != cell42 )).

fof(tlhfof45881,axiom,(
    cell87 != cell40 )).

fof(tlhfof45882,axiom,(
    cell87 != cell39 )).

fof(tlhfof45883,axiom,(
    cell87 != cell38 )).

fof(tlhfof45884,axiom,(
    cell87 != cell37 )).

fof(tlhfof45885,axiom,(
    cell87 != cell36 )).

fof(tlhfof45886,axiom,(
    cell87 != cell35 )).

fof(tlhfof45887,axiom,(
    cell87 != cell34 )).

fof(tlhfof45888,axiom,(
    cell87 != cell33 )).

fof(tlhfof45889,axiom,(
    cell87 != cell31 )).

fof(tlhfof45890,axiom,(
    cell87 != cell32 )).

fof(tlhfof45891,axiom,(
    cell87 != cell30 )).

fof(tlhfof45892,axiom,(
    cell87 != cell29 )).

fof(tlhfof45893,axiom,(
    cell87 != cell28 )).

fof(tlhfof45894,axiom,(
    cell87 != cell27 )).

fof(tlhfof45895,axiom,(
    cell87 != cell26 )).

fof(tlhfof45896,axiom,(
    cell87 != cell25 )).

fof(tlhfof45897,axiom,(
    cell87 != cell24 )).

fof(tlhfof45898,axiom,(
    cell87 != cell23 )).

fof(tlhfof45899,axiom,(
    cell87 != cell21 )).

fof(tlhfof45900,axiom,(
    cell87 != cell20 )).

fof(tlhfof45901,axiom,(
    cell87 != cell19 )).

fof(tlhfof45902,axiom,(
    cell87 != cell18 )).

fof(tlhfof45903,axiom,(
    cell87 != cell17 )).

fof(tlhfof45904,axiom,(
    cell87 != cell16 )).

fof(tlhfof45905,axiom,(
    cell87 != cell15 )).

fof(tlhfof45906,axiom,(
    cell87 != cell14 )).

fof(tlhfof45907,axiom,(
    cell87 != cell10 )).

fof(tlhfof45908,axiom,(
    cell87 != cell9 )).

fof(tlhfof45909,axiom,(
    cell87 != cell8 )).

fof(tlhfof45910,axiom,(
    cell87 != cell7 )).

fof(tlhfof45911,axiom,(
    cell87 != cell6 )).

fof(tlhfof45912,axiom,(
    cell87 != cell5 )).

fof(tlhfof45913,axiom,(
    cell87 != cell4 )).

fof(tlhfof45914,axiom,(
    cell87 != cell3 )).

fof(tlhfof45915,axiom,(
    cell87 != cell1 )).

fof(tlhfof45916,axiom,(
    cell87 != cell89 )).

fof(tlhfof45917,axiom,(
    cell87 != cell88 )).

fof(tlhfof45918,axiom,(
    cell87 != cell79 )).

fof(tlhfof45919,axiom,(
    cell87 != cell90 )).

fof(tlhfof45920,axiom,(
    cell87 != cell99 )).

fof(tlhfof45921,axiom,(
    cell87 != cell12 )).

fof(tlhfof45922,axiom,(
    cell87 != cell11 )).

fof(tlhfof45923,axiom,(
    cell87 != cell2 )).

fof(tlhfof45924,axiom,(
    cell87 != cell13 )).

fof(tlhfof45925,axiom,(
    cell87 != cell22 )).

fof(tlhfof45926,axiom,(
    cell86 != cell85 )).

fof(tlhfof45927,axiom,(
    cell86 != cell84 )).

fof(tlhfof45928,axiom,(
    cell86 != cell83 )).

fof(tlhfof45929,axiom,(
    cell86 != cell81 )).

fof(tlhfof45930,axiom,(
    cell86 != cell82 )).

fof(tlhfof45931,axiom,(
    cell86 != cell80 )).

fof(tlhfof45932,axiom,(
    cell86 != cell78 )).

fof(tlhfof45933,axiom,(
    cell86 != cell77 )).

fof(tlhfof45934,axiom,(
    cell86 != cell76 )).

fof(tlhfof45935,axiom,(
    cell86 != cell75 )).

fof(tlhfof45936,axiom,(
    cell86 != cell74 )).

fof(tlhfof45937,axiom,(
    cell86 != cell73 )).

fof(tlhfof45938,axiom,(
    cell86 != cell71 )).

fof(tlhfof45939,axiom,(
    cell86 != cell72 )).

fof(tlhfof45940,axiom,(
    cell86 != cell70 )).

fof(tlhfof45941,axiom,(
    cell86 != cell69 )).

fof(tlhfof45942,axiom,(
    cell86 != cell68 )).

fof(tlhfof45943,axiom,(
    cell86 != cell67 )).

fof(tlhfof45944,axiom,(
    cell86 != cell66 )).

fof(tlhfof45945,axiom,(
    cell86 != cell65 )).

fof(tlhfof45946,axiom,(
    cell86 != cell64 )).

fof(tlhfof45947,axiom,(
    cell86 != cell63 )).

fof(tlhfof45948,axiom,(
    cell86 != cell61 )).

fof(tlhfof45949,axiom,(
    cell86 != cell62 )).

fof(tlhfof45950,axiom,(
    cell86 != cell60 )).

fof(tlhfof45951,axiom,(
    cell86 != cell59 )).

fof(tlhfof45952,axiom,(
    cell86 != cell58 )).

fof(tlhfof45953,axiom,(
    cell86 != cell57 )).

fof(tlhfof45954,axiom,(
    cell86 != cell56 )).

fof(tlhfof45955,axiom,(
    cell86 != cell55 )).

fof(tlhfof45956,axiom,(
    cell86 != cell54 )).

fof(tlhfof45957,axiom,(
    cell86 != cell53 )).

fof(tlhfof45958,axiom,(
    cell86 != cell51 )).

fof(tlhfof45959,axiom,(
    cell86 != cell52 )).

fof(tlhfof45960,axiom,(
    cell86 != cell50 )).

fof(tlhfof45961,axiom,(
    cell86 != cell49 )).

fof(tlhfof45962,axiom,(
    cell86 != cell48 )).

fof(tlhfof45963,axiom,(
    cell86 != cell47 )).

fof(tlhfof45964,axiom,(
    cell86 != cell46 )).

fof(tlhfof45965,axiom,(
    cell86 != cell45 )).

fof(tlhfof45966,axiom,(
    cell86 != cell44 )).

fof(tlhfof45967,axiom,(
    cell86 != cell43 )).

fof(tlhfof45968,axiom,(
    cell86 != cell41 )).

fof(tlhfof45969,axiom,(
    cell86 != cell42 )).

fof(tlhfof45970,axiom,(
    cell86 != cell40 )).

fof(tlhfof45971,axiom,(
    cell86 != cell39 )).

fof(tlhfof45972,axiom,(
    cell86 != cell38 )).

fof(tlhfof45973,axiom,(
    cell86 != cell37 )).

fof(tlhfof45974,axiom,(
    cell86 != cell36 )).

fof(tlhfof45975,axiom,(
    cell86 != cell35 )).

fof(tlhfof45976,axiom,(
    cell86 != cell34 )).

fof(tlhfof45977,axiom,(
    cell86 != cell33 )).

fof(tlhfof45978,axiom,(
    cell86 != cell31 )).

fof(tlhfof45979,axiom,(
    cell86 != cell32 )).

fof(tlhfof45980,axiom,(
    cell86 != cell30 )).

fof(tlhfof45981,axiom,(
    cell86 != cell29 )).

fof(tlhfof45982,axiom,(
    cell86 != cell28 )).

fof(tlhfof45983,axiom,(
    cell86 != cell27 )).

fof(tlhfof45984,axiom,(
    cell86 != cell26 )).

fof(tlhfof45985,axiom,(
    cell86 != cell25 )).

fof(tlhfof45986,axiom,(
    cell86 != cell24 )).

fof(tlhfof45987,axiom,(
    cell86 != cell23 )).

fof(tlhfof45988,axiom,(
    cell86 != cell21 )).

fof(tlhfof45989,axiom,(
    cell86 != cell20 )).

fof(tlhfof45990,axiom,(
    cell86 != cell19 )).

fof(tlhfof45991,axiom,(
    cell86 != cell18 )).

fof(tlhfof45992,axiom,(
    cell86 != cell17 )).

fof(tlhfof45993,axiom,(
    cell86 != cell16 )).

fof(tlhfof45994,axiom,(
    cell86 != cell15 )).

fof(tlhfof45995,axiom,(
    cell86 != cell14 )).

fof(tlhfof45996,axiom,(
    cell86 != cell10 )).

fof(tlhfof45997,axiom,(
    cell86 != cell9 )).

fof(tlhfof45998,axiom,(
    cell86 != cell8 )).

fof(tlhfof45999,axiom,(
    cell86 != cell7 )).

fof(tlhfof46000,axiom,(
    cell86 != cell6 )).

fof(tlhfof46001,axiom,(
    cell86 != cell5 )).

fof(tlhfof46002,axiom,(
    cell86 != cell4 )).

fof(tlhfof46003,axiom,(
    cell86 != cell3 )).

fof(tlhfof46004,axiom,(
    cell86 != cell1 )).

fof(tlhfof46005,axiom,(
    cell86 != cell89 )).

fof(tlhfof46006,axiom,(
    cell86 != cell88 )).

fof(tlhfof46007,axiom,(
    cell86 != cell79 )).

fof(tlhfof46008,axiom,(
    cell86 != cell90 )).

fof(tlhfof46009,axiom,(
    cell86 != cell99 )).

fof(tlhfof46010,axiom,(
    cell86 != cell12 )).

fof(tlhfof46011,axiom,(
    cell86 != cell11 )).

fof(tlhfof46012,axiom,(
    cell86 != cell2 )).

fof(tlhfof46013,axiom,(
    cell86 != cell13 )).

fof(tlhfof46014,axiom,(
    cell86 != cell22 )).

fof(tlhfof46015,axiom,(
    cell85 != cell84 )).

fof(tlhfof46016,axiom,(
    cell85 != cell83 )).

fof(tlhfof46017,axiom,(
    cell85 != cell81 )).

fof(tlhfof46018,axiom,(
    cell85 != cell82 )).

fof(tlhfof46019,axiom,(
    cell85 != cell80 )).

fof(tlhfof46020,axiom,(
    cell85 != cell78 )).

fof(tlhfof46021,axiom,(
    cell85 != cell77 )).

fof(tlhfof46022,axiom,(
    cell85 != cell76 )).

fof(tlhfof46023,axiom,(
    cell85 != cell75 )).

fof(tlhfof46024,axiom,(
    cell85 != cell74 )).

fof(tlhfof46025,axiom,(
    cell85 != cell73 )).

fof(tlhfof46026,axiom,(
    cell85 != cell71 )).

fof(tlhfof46027,axiom,(
    cell85 != cell72 )).

fof(tlhfof46028,axiom,(
    cell85 != cell70 )).

fof(tlhfof46029,axiom,(
    cell85 != cell69 )).

fof(tlhfof46030,axiom,(
    cell85 != cell68 )).

fof(tlhfof46031,axiom,(
    cell85 != cell67 )).

fof(tlhfof46032,axiom,(
    cell85 != cell66 )).

fof(tlhfof46033,axiom,(
    cell85 != cell65 )).

fof(tlhfof46034,axiom,(
    cell85 != cell64 )).

fof(tlhfof46035,axiom,(
    cell85 != cell63 )).

fof(tlhfof46036,axiom,(
    cell85 != cell61 )).

fof(tlhfof46037,axiom,(
    cell85 != cell62 )).

fof(tlhfof46038,axiom,(
    cell85 != cell60 )).

fof(tlhfof46039,axiom,(
    cell85 != cell59 )).

fof(tlhfof46040,axiom,(
    cell85 != cell58 )).

fof(tlhfof46041,axiom,(
    cell85 != cell57 )).

fof(tlhfof46042,axiom,(
    cell85 != cell56 )).

fof(tlhfof46043,axiom,(
    cell85 != cell55 )).

fof(tlhfof46044,axiom,(
    cell85 != cell54 )).

fof(tlhfof46045,axiom,(
    cell85 != cell53 )).

fof(tlhfof46046,axiom,(
    cell85 != cell51 )).

fof(tlhfof46047,axiom,(
    cell85 != cell52 )).

fof(tlhfof46048,axiom,(
    cell85 != cell50 )).

fof(tlhfof46049,axiom,(
    cell85 != cell49 )).

fof(tlhfof46050,axiom,(
    cell85 != cell48 )).

fof(tlhfof46051,axiom,(
    cell85 != cell47 )).

fof(tlhfof46052,axiom,(
    cell85 != cell46 )).

fof(tlhfof46053,axiom,(
    cell85 != cell45 )).

fof(tlhfof46054,axiom,(
    cell85 != cell44 )).

fof(tlhfof46055,axiom,(
    cell85 != cell43 )).

fof(tlhfof46056,axiom,(
    cell85 != cell41 )).

fof(tlhfof46057,axiom,(
    cell85 != cell42 )).

fof(tlhfof46058,axiom,(
    cell85 != cell40 )).

fof(tlhfof46059,axiom,(
    cell85 != cell39 )).

fof(tlhfof46060,axiom,(
    cell85 != cell38 )).

fof(tlhfof46061,axiom,(
    cell85 != cell37 )).

fof(tlhfof46062,axiom,(
    cell85 != cell36 )).

fof(tlhfof46063,axiom,(
    cell85 != cell35 )).

fof(tlhfof46064,axiom,(
    cell85 != cell34 )).

fof(tlhfof46065,axiom,(
    cell85 != cell33 )).

fof(tlhfof46066,axiom,(
    cell85 != cell31 )).

fof(tlhfof46067,axiom,(
    cell85 != cell32 )).

fof(tlhfof46068,axiom,(
    cell85 != cell30 )).

fof(tlhfof46069,axiom,(
    cell85 != cell29 )).

fof(tlhfof46070,axiom,(
    cell85 != cell28 )).

fof(tlhfof46071,axiom,(
    cell85 != cell27 )).

fof(tlhfof46072,axiom,(
    cell85 != cell26 )).

fof(tlhfof46073,axiom,(
    cell85 != cell25 )).

fof(tlhfof46074,axiom,(
    cell85 != cell24 )).

fof(tlhfof46075,axiom,(
    cell85 != cell23 )).

fof(tlhfof46076,axiom,(
    cell85 != cell21 )).

fof(tlhfof46077,axiom,(
    cell85 != cell20 )).

fof(tlhfof46078,axiom,(
    cell85 != cell19 )).

fof(tlhfof46079,axiom,(
    cell85 != cell18 )).

fof(tlhfof46080,axiom,(
    cell85 != cell17 )).

fof(tlhfof46081,axiom,(
    cell85 != cell16 )).

fof(tlhfof46082,axiom,(
    cell85 != cell15 )).

fof(tlhfof46083,axiom,(
    cell85 != cell14 )).

fof(tlhfof46084,axiom,(
    cell85 != cell10 )).

fof(tlhfof46085,axiom,(
    cell85 != cell9 )).

fof(tlhfof46086,axiom,(
    cell85 != cell8 )).

fof(tlhfof46087,axiom,(
    cell85 != cell7 )).

fof(tlhfof46088,axiom,(
    cell85 != cell6 )).

fof(tlhfof46089,axiom,(
    cell85 != cell5 )).

fof(tlhfof46090,axiom,(
    cell85 != cell4 )).

fof(tlhfof46091,axiom,(
    cell85 != cell3 )).

fof(tlhfof46092,axiom,(
    cell85 != cell1 )).

fof(tlhfof46093,axiom,(
    cell85 != cell89 )).

fof(tlhfof46094,axiom,(
    cell85 != cell88 )).

fof(tlhfof46095,axiom,(
    cell85 != cell79 )).

fof(tlhfof46096,axiom,(
    cell85 != cell90 )).

fof(tlhfof46097,axiom,(
    cell85 != cell99 )).

fof(tlhfof46098,axiom,(
    cell85 != cell12 )).

fof(tlhfof46099,axiom,(
    cell85 != cell11 )).

fof(tlhfof46100,axiom,(
    cell85 != cell2 )).

fof(tlhfof46101,axiom,(
    cell85 != cell13 )).

fof(tlhfof46102,axiom,(
    cell85 != cell22 )).

fof(tlhfof46103,axiom,(
    cell84 != cell83 )).

fof(tlhfof46104,axiom,(
    cell84 != cell81 )).

fof(tlhfof46105,axiom,(
    cell84 != cell82 )).

fof(tlhfof46106,axiom,(
    cell84 != cell80 )).

fof(tlhfof46107,axiom,(
    cell84 != cell78 )).

fof(tlhfof46108,axiom,(
    cell84 != cell77 )).

fof(tlhfof46109,axiom,(
    cell84 != cell76 )).

fof(tlhfof46110,axiom,(
    cell84 != cell75 )).

fof(tlhfof46111,axiom,(
    cell84 != cell74 )).

fof(tlhfof46112,axiom,(
    cell84 != cell73 )).

fof(tlhfof46113,axiom,(
    cell84 != cell71 )).

fof(tlhfof46114,axiom,(
    cell84 != cell72 )).

fof(tlhfof46115,axiom,(
    cell84 != cell70 )).

fof(tlhfof46116,axiom,(
    cell84 != cell69 )).

fof(tlhfof46117,axiom,(
    cell84 != cell68 )).

fof(tlhfof46118,axiom,(
    cell84 != cell67 )).

fof(tlhfof46119,axiom,(
    cell84 != cell66 )).

fof(tlhfof46120,axiom,(
    cell84 != cell65 )).

fof(tlhfof46121,axiom,(
    cell84 != cell64 )).

fof(tlhfof46122,axiom,(
    cell84 != cell63 )).

fof(tlhfof46123,axiom,(
    cell84 != cell61 )).

fof(tlhfof46124,axiom,(
    cell84 != cell62 )).

fof(tlhfof46125,axiom,(
    cell84 != cell60 )).

fof(tlhfof46126,axiom,(
    cell84 != cell59 )).

fof(tlhfof46127,axiom,(
    cell84 != cell58 )).

fof(tlhfof46128,axiom,(
    cell84 != cell57 )).

fof(tlhfof46129,axiom,(
    cell84 != cell56 )).

fof(tlhfof46130,axiom,(
    cell84 != cell55 )).

fof(tlhfof46131,axiom,(
    cell84 != cell54 )).

fof(tlhfof46132,axiom,(
    cell84 != cell53 )).

fof(tlhfof46133,axiom,(
    cell84 != cell51 )).

fof(tlhfof46134,axiom,(
    cell84 != cell52 )).

fof(tlhfof46135,axiom,(
    cell84 != cell50 )).

fof(tlhfof46136,axiom,(
    cell84 != cell49 )).

fof(tlhfof46137,axiom,(
    cell84 != cell48 )).

fof(tlhfof46138,axiom,(
    cell84 != cell47 )).

fof(tlhfof46139,axiom,(
    cell84 != cell46 )).

fof(tlhfof46140,axiom,(
    cell84 != cell45 )).

fof(tlhfof46141,axiom,(
    cell84 != cell44 )).

fof(tlhfof46142,axiom,(
    cell84 != cell43 )).

fof(tlhfof46143,axiom,(
    cell84 != cell41 )).

fof(tlhfof46144,axiom,(
    cell84 != cell42 )).

fof(tlhfof46145,axiom,(
    cell84 != cell40 )).

fof(tlhfof46146,axiom,(
    cell84 != cell39 )).

fof(tlhfof46147,axiom,(
    cell84 != cell38 )).

fof(tlhfof46148,axiom,(
    cell84 != cell37 )).

fof(tlhfof46149,axiom,(
    cell84 != cell36 )).

fof(tlhfof46150,axiom,(
    cell84 != cell35 )).

fof(tlhfof46151,axiom,(
    cell84 != cell34 )).

fof(tlhfof46152,axiom,(
    cell84 != cell33 )).

fof(tlhfof46153,axiom,(
    cell84 != cell31 )).

fof(tlhfof46154,axiom,(
    cell84 != cell32 )).

fof(tlhfof46155,axiom,(
    cell84 != cell30 )).

fof(tlhfof46156,axiom,(
    cell84 != cell29 )).

fof(tlhfof46157,axiom,(
    cell84 != cell28 )).

fof(tlhfof46158,axiom,(
    cell84 != cell27 )).

fof(tlhfof46159,axiom,(
    cell84 != cell26 )).

fof(tlhfof46160,axiom,(
    cell84 != cell25 )).

fof(tlhfof46161,axiom,(
    cell84 != cell24 )).

fof(tlhfof46162,axiom,(
    cell84 != cell23 )).

fof(tlhfof46163,axiom,(
    cell84 != cell21 )).

fof(tlhfof46164,axiom,(
    cell84 != cell20 )).

fof(tlhfof46165,axiom,(
    cell84 != cell19 )).

fof(tlhfof46166,axiom,(
    cell84 != cell18 )).

fof(tlhfof46167,axiom,(
    cell84 != cell17 )).

fof(tlhfof46168,axiom,(
    cell84 != cell16 )).

fof(tlhfof46169,axiom,(
    cell84 != cell15 )).

fof(tlhfof46170,axiom,(
    cell84 != cell14 )).

fof(tlhfof46171,axiom,(
    cell84 != cell10 )).

fof(tlhfof46172,axiom,(
    cell84 != cell9 )).

fof(tlhfof46173,axiom,(
    cell84 != cell8 )).

fof(tlhfof46174,axiom,(
    cell84 != cell7 )).

fof(tlhfof46175,axiom,(
    cell84 != cell6 )).

fof(tlhfof46176,axiom,(
    cell84 != cell5 )).

fof(tlhfof46177,axiom,(
    cell84 != cell4 )).

fof(tlhfof46178,axiom,(
    cell84 != cell3 )).

fof(tlhfof46179,axiom,(
    cell84 != cell1 )).

fof(tlhfof46180,axiom,(
    cell84 != cell89 )).

fof(tlhfof46181,axiom,(
    cell84 != cell88 )).

fof(tlhfof46182,axiom,(
    cell84 != cell79 )).

fof(tlhfof46183,axiom,(
    cell84 != cell90 )).

fof(tlhfof46184,axiom,(
    cell84 != cell99 )).

fof(tlhfof46185,axiom,(
    cell84 != cell12 )).

fof(tlhfof46186,axiom,(
    cell84 != cell11 )).

fof(tlhfof46187,axiom,(
    cell84 != cell2 )).

fof(tlhfof46188,axiom,(
    cell84 != cell13 )).

fof(tlhfof46189,axiom,(
    cell84 != cell22 )).

fof(tlhfof46190,axiom,(
    cell83 != cell81 )).

fof(tlhfof46191,axiom,(
    cell83 != cell82 )).

fof(tlhfof46192,axiom,(
    cell83 != cell80 )).

fof(tlhfof46193,axiom,(
    cell83 != cell78 )).

fof(tlhfof46194,axiom,(
    cell83 != cell77 )).

fof(tlhfof46195,axiom,(
    cell83 != cell76 )).

fof(tlhfof46196,axiom,(
    cell83 != cell75 )).

fof(tlhfof46197,axiom,(
    cell83 != cell74 )).

fof(tlhfof46198,axiom,(
    cell83 != cell73 )).

fof(tlhfof46199,axiom,(
    cell83 != cell71 )).

fof(tlhfof46200,axiom,(
    cell83 != cell72 )).

fof(tlhfof46201,axiom,(
    cell83 != cell70 )).

fof(tlhfof46202,axiom,(
    cell83 != cell69 )).

fof(tlhfof46203,axiom,(
    cell83 != cell68 )).

fof(tlhfof46204,axiom,(
    cell83 != cell67 )).

fof(tlhfof46205,axiom,(
    cell83 != cell66 )).

fof(tlhfof46206,axiom,(
    cell83 != cell65 )).

fof(tlhfof46207,axiom,(
    cell83 != cell64 )).

fof(tlhfof46208,axiom,(
    cell83 != cell63 )).

fof(tlhfof46209,axiom,(
    cell83 != cell61 )).

fof(tlhfof46210,axiom,(
    cell83 != cell62 )).

fof(tlhfof46211,axiom,(
    cell83 != cell60 )).

fof(tlhfof46212,axiom,(
    cell83 != cell59 )).

fof(tlhfof46213,axiom,(
    cell83 != cell58 )).

fof(tlhfof46214,axiom,(
    cell83 != cell57 )).

fof(tlhfof46215,axiom,(
    cell83 != cell56 )).

fof(tlhfof46216,axiom,(
    cell83 != cell55 )).

fof(tlhfof46217,axiom,(
    cell83 != cell54 )).

fof(tlhfof46218,axiom,(
    cell83 != cell53 )).

fof(tlhfof46219,axiom,(
    cell83 != cell51 )).

fof(tlhfof46220,axiom,(
    cell83 != cell52 )).

fof(tlhfof46221,axiom,(
    cell83 != cell50 )).

fof(tlhfof46222,axiom,(
    cell83 != cell49 )).

fof(tlhfof46223,axiom,(
    cell83 != cell48 )).

fof(tlhfof46224,axiom,(
    cell83 != cell47 )).

fof(tlhfof46225,axiom,(
    cell83 != cell46 )).

fof(tlhfof46226,axiom,(
    cell83 != cell45 )).

fof(tlhfof46227,axiom,(
    cell83 != cell44 )).

fof(tlhfof46228,axiom,(
    cell83 != cell43 )).

fof(tlhfof46229,axiom,(
    cell83 != cell41 )).

fof(tlhfof46230,axiom,(
    cell83 != cell42 )).

fof(tlhfof46231,axiom,(
    cell83 != cell40 )).

fof(tlhfof46232,axiom,(
    cell83 != cell39 )).

fof(tlhfof46233,axiom,(
    cell83 != cell38 )).

fof(tlhfof46234,axiom,(
    cell83 != cell37 )).

fof(tlhfof46235,axiom,(
    cell83 != cell36 )).

fof(tlhfof46236,axiom,(
    cell83 != cell35 )).

fof(tlhfof46237,axiom,(
    cell83 != cell34 )).

fof(tlhfof46238,axiom,(
    cell83 != cell33 )).

fof(tlhfof46239,axiom,(
    cell83 != cell31 )).

fof(tlhfof46240,axiom,(
    cell83 != cell32 )).

fof(tlhfof46241,axiom,(
    cell83 != cell30 )).

fof(tlhfof46242,axiom,(
    cell83 != cell29 )).

fof(tlhfof46243,axiom,(
    cell83 != cell28 )).

fof(tlhfof46244,axiom,(
    cell83 != cell27 )).

fof(tlhfof46245,axiom,(
    cell83 != cell26 )).

fof(tlhfof46246,axiom,(
    cell83 != cell25 )).

fof(tlhfof46247,axiom,(
    cell83 != cell24 )).

fof(tlhfof46248,axiom,(
    cell83 != cell23 )).

fof(tlhfof46249,axiom,(
    cell83 != cell21 )).

fof(tlhfof46250,axiom,(
    cell83 != cell20 )).

fof(tlhfof46251,axiom,(
    cell83 != cell19 )).

fof(tlhfof46252,axiom,(
    cell83 != cell18 )).

fof(tlhfof46253,axiom,(
    cell83 != cell17 )).

fof(tlhfof46254,axiom,(
    cell83 != cell16 )).

fof(tlhfof46255,axiom,(
    cell83 != cell15 )).

fof(tlhfof46256,axiom,(
    cell83 != cell14 )).

fof(tlhfof46257,axiom,(
    cell83 != cell10 )).

fof(tlhfof46258,axiom,(
    cell83 != cell9 )).

fof(tlhfof46259,axiom,(
    cell83 != cell8 )).

fof(tlhfof46260,axiom,(
    cell83 != cell7 )).

fof(tlhfof46261,axiom,(
    cell83 != cell6 )).

fof(tlhfof46262,axiom,(
    cell83 != cell5 )).

fof(tlhfof46263,axiom,(
    cell83 != cell4 )).

fof(tlhfof46264,axiom,(
    cell83 != cell3 )).

fof(tlhfof46265,axiom,(
    cell83 != cell1 )).

fof(tlhfof46266,axiom,(
    cell83 != cell89 )).

fof(tlhfof46267,axiom,(
    cell83 != cell88 )).

fof(tlhfof46268,axiom,(
    cell83 != cell79 )).

fof(tlhfof46269,axiom,(
    cell83 != cell90 )).

fof(tlhfof46270,axiom,(
    cell83 != cell99 )).

fof(tlhfof46271,axiom,(
    cell83 != cell12 )).

fof(tlhfof46272,axiom,(
    cell83 != cell11 )).

fof(tlhfof46273,axiom,(
    cell83 != cell2 )).

fof(tlhfof46274,axiom,(
    cell83 != cell13 )).

fof(tlhfof46275,axiom,(
    cell83 != cell22 )).

fof(tlhfof46276,axiom,(
    cell81 != cell82 )).

fof(tlhfof46277,axiom,(
    cell81 != cell80 )).

fof(tlhfof46278,axiom,(
    cell81 != cell78 )).

fof(tlhfof46279,axiom,(
    cell81 != cell77 )).

fof(tlhfof46280,axiom,(
    cell81 != cell76 )).

fof(tlhfof46281,axiom,(
    cell81 != cell75 )).

fof(tlhfof46282,axiom,(
    cell81 != cell74 )).

fof(tlhfof46283,axiom,(
    cell81 != cell73 )).

fof(tlhfof46284,axiom,(
    cell81 != cell71 )).

fof(tlhfof46285,axiom,(
    cell81 != cell72 )).

fof(tlhfof46286,axiom,(
    cell81 != cell70 )).

fof(tlhfof46287,axiom,(
    cell81 != cell69 )).

fof(tlhfof46288,axiom,(
    cell81 != cell68 )).

fof(tlhfof46289,axiom,(
    cell81 != cell67 )).

fof(tlhfof46290,axiom,(
    cell81 != cell66 )).

fof(tlhfof46291,axiom,(
    cell81 != cell65 )).

fof(tlhfof46292,axiom,(
    cell81 != cell64 )).

fof(tlhfof46293,axiom,(
    cell81 != cell63 )).

fof(tlhfof46294,axiom,(
    cell81 != cell61 )).

fof(tlhfof46295,axiom,(
    cell81 != cell62 )).

fof(tlhfof46296,axiom,(
    cell81 != cell60 )).

fof(tlhfof46297,axiom,(
    cell81 != cell59 )).

fof(tlhfof46298,axiom,(
    cell81 != cell58 )).

fof(tlhfof46299,axiom,(
    cell81 != cell57 )).

fof(tlhfof46300,axiom,(
    cell81 != cell56 )).

fof(tlhfof46301,axiom,(
    cell81 != cell55 )).

fof(tlhfof46302,axiom,(
    cell81 != cell54 )).

fof(tlhfof46303,axiom,(
    cell81 != cell53 )).

fof(tlhfof46304,axiom,(
    cell81 != cell51 )).

fof(tlhfof46305,axiom,(
    cell81 != cell52 )).

fof(tlhfof46306,axiom,(
    cell81 != cell50 )).

fof(tlhfof46307,axiom,(
    cell81 != cell49 )).

fof(tlhfof46308,axiom,(
    cell81 != cell48 )).

fof(tlhfof46309,axiom,(
    cell81 != cell47 )).

fof(tlhfof46310,axiom,(
    cell81 != cell46 )).

fof(tlhfof46311,axiom,(
    cell81 != cell45 )).

fof(tlhfof46312,axiom,(
    cell81 != cell44 )).

fof(tlhfof46313,axiom,(
    cell81 != cell43 )).

fof(tlhfof46314,axiom,(
    cell81 != cell41 )).

fof(tlhfof46315,axiom,(
    cell81 != cell42 )).

fof(tlhfof46316,axiom,(
    cell81 != cell40 )).

fof(tlhfof46317,axiom,(
    cell81 != cell39 )).

fof(tlhfof46318,axiom,(
    cell81 != cell38 )).

fof(tlhfof46319,axiom,(
    cell81 != cell37 )).

fof(tlhfof46320,axiom,(
    cell81 != cell36 )).

fof(tlhfof46321,axiom,(
    cell81 != cell35 )).

fof(tlhfof46322,axiom,(
    cell81 != cell34 )).

fof(tlhfof46323,axiom,(
    cell81 != cell33 )).

fof(tlhfof46324,axiom,(
    cell81 != cell31 )).

fof(tlhfof46325,axiom,(
    cell81 != cell32 )).

fof(tlhfof46326,axiom,(
    cell81 != cell30 )).

fof(tlhfof46327,axiom,(
    cell81 != cell29 )).

fof(tlhfof46328,axiom,(
    cell81 != cell28 )).

fof(tlhfof46329,axiom,(
    cell81 != cell27 )).

fof(tlhfof46330,axiom,(
    cell81 != cell26 )).

fof(tlhfof46331,axiom,(
    cell81 != cell25 )).

fof(tlhfof46332,axiom,(
    cell81 != cell24 )).

fof(tlhfof46333,axiom,(
    cell81 != cell23 )).

fof(tlhfof46334,axiom,(
    cell81 != cell21 )).

fof(tlhfof46335,axiom,(
    cell81 != cell20 )).

fof(tlhfof46336,axiom,(
    cell81 != cell19 )).

fof(tlhfof46337,axiom,(
    cell81 != cell18 )).

fof(tlhfof46338,axiom,(
    cell81 != cell17 )).

fof(tlhfof46339,axiom,(
    cell81 != cell16 )).

fof(tlhfof46340,axiom,(
    cell81 != cell15 )).

fof(tlhfof46341,axiom,(
    cell81 != cell14 )).

fof(tlhfof46342,axiom,(
    cell81 != cell10 )).

fof(tlhfof46343,axiom,(
    cell81 != cell9 )).

fof(tlhfof46344,axiom,(
    cell81 != cell8 )).

fof(tlhfof46345,axiom,(
    cell81 != cell7 )).

fof(tlhfof46346,axiom,(
    cell81 != cell6 )).

fof(tlhfof46347,axiom,(
    cell81 != cell5 )).

fof(tlhfof46348,axiom,(
    cell81 != cell4 )).

fof(tlhfof46349,axiom,(
    cell81 != cell3 )).

fof(tlhfof46350,axiom,(
    cell81 != cell1 )).

fof(tlhfof46351,axiom,(
    cell81 != cell89 )).

fof(tlhfof46352,axiom,(
    cell81 != cell88 )).

fof(tlhfof46353,axiom,(
    cell81 != cell79 )).

fof(tlhfof46354,axiom,(
    cell81 != cell90 )).

fof(tlhfof46355,axiom,(
    cell81 != cell99 )).

fof(tlhfof46356,axiom,(
    cell81 != cell12 )).

fof(tlhfof46357,axiom,(
    cell81 != cell11 )).

fof(tlhfof46358,axiom,(
    cell81 != cell2 )).

fof(tlhfof46359,axiom,(
    cell81 != cell13 )).

fof(tlhfof46360,axiom,(
    cell81 != cell22 )).

fof(tlhfof46361,axiom,(
    cell82 != cell80 )).

fof(tlhfof46362,axiom,(
    cell82 != cell78 )).

fof(tlhfof46363,axiom,(
    cell82 != cell77 )).

fof(tlhfof46364,axiom,(
    cell82 != cell76 )).

fof(tlhfof46365,axiom,(
    cell82 != cell75 )).

fof(tlhfof46366,axiom,(
    cell82 != cell74 )).

fof(tlhfof46367,axiom,(
    cell82 != cell73 )).

fof(tlhfof46368,axiom,(
    cell82 != cell71 )).

fof(tlhfof46369,axiom,(
    cell82 != cell72 )).

fof(tlhfof46370,axiom,(
    cell82 != cell70 )).

fof(tlhfof46371,axiom,(
    cell82 != cell69 )).

fof(tlhfof46372,axiom,(
    cell82 != cell68 )).

fof(tlhfof46373,axiom,(
    cell82 != cell67 )).

fof(tlhfof46374,axiom,(
    cell82 != cell66 )).

fof(tlhfof46375,axiom,(
    cell82 != cell65 )).

fof(tlhfof46376,axiom,(
    cell82 != cell64 )).

fof(tlhfof46377,axiom,(
    cell82 != cell63 )).

fof(tlhfof46378,axiom,(
    cell82 != cell61 )).

fof(tlhfof46379,axiom,(
    cell82 != cell62 )).

fof(tlhfof46380,axiom,(
    cell82 != cell60 )).

fof(tlhfof46381,axiom,(
    cell82 != cell59 )).

fof(tlhfof46382,axiom,(
    cell82 != cell58 )).

fof(tlhfof46383,axiom,(
    cell82 != cell57 )).

fof(tlhfof46384,axiom,(
    cell82 != cell56 )).

fof(tlhfof46385,axiom,(
    cell82 != cell55 )).

fof(tlhfof46386,axiom,(
    cell82 != cell54 )).

fof(tlhfof46387,axiom,(
    cell82 != cell53 )).

fof(tlhfof46388,axiom,(
    cell82 != cell51 )).

fof(tlhfof46389,axiom,(
    cell82 != cell52 )).

fof(tlhfof46390,axiom,(
    cell82 != cell50 )).

fof(tlhfof46391,axiom,(
    cell82 != cell49 )).

fof(tlhfof46392,axiom,(
    cell82 != cell48 )).

fof(tlhfof46393,axiom,(
    cell82 != cell47 )).

fof(tlhfof46394,axiom,(
    cell82 != cell46 )).

fof(tlhfof46395,axiom,(
    cell82 != cell45 )).

fof(tlhfof46396,axiom,(
    cell82 != cell44 )).

fof(tlhfof46397,axiom,(
    cell82 != cell43 )).

fof(tlhfof46398,axiom,(
    cell82 != cell41 )).

fof(tlhfof46399,axiom,(
    cell82 != cell42 )).

fof(tlhfof46400,axiom,(
    cell82 != cell40 )).

fof(tlhfof46401,axiom,(
    cell82 != cell39 )).

fof(tlhfof46402,axiom,(
    cell82 != cell38 )).

fof(tlhfof46403,axiom,(
    cell82 != cell37 )).

fof(tlhfof46404,axiom,(
    cell82 != cell36 )).

fof(tlhfof46405,axiom,(
    cell82 != cell35 )).

fof(tlhfof46406,axiom,(
    cell82 != cell34 )).

fof(tlhfof46407,axiom,(
    cell82 != cell33 )).

fof(tlhfof46408,axiom,(
    cell82 != cell31 )).

fof(tlhfof46409,axiom,(
    cell82 != cell32 )).

fof(tlhfof46410,axiom,(
    cell82 != cell30 )).

fof(tlhfof46411,axiom,(
    cell82 != cell29 )).

fof(tlhfof46412,axiom,(
    cell82 != cell28 )).

fof(tlhfof46413,axiom,(
    cell82 != cell27 )).

fof(tlhfof46414,axiom,(
    cell82 != cell26 )).

fof(tlhfof46415,axiom,(
    cell82 != cell25 )).

fof(tlhfof46416,axiom,(
    cell82 != cell24 )).

fof(tlhfof46417,axiom,(
    cell82 != cell23 )).

fof(tlhfof46418,axiom,(
    cell82 != cell21 )).

fof(tlhfof46419,axiom,(
    cell82 != cell20 )).

fof(tlhfof46420,axiom,(
    cell82 != cell19 )).

fof(tlhfof46421,axiom,(
    cell82 != cell18 )).

fof(tlhfof46422,axiom,(
    cell82 != cell17 )).

fof(tlhfof46423,axiom,(
    cell82 != cell16 )).

fof(tlhfof46424,axiom,(
    cell82 != cell15 )).

fof(tlhfof46425,axiom,(
    cell82 != cell14 )).

fof(tlhfof46426,axiom,(
    cell82 != cell10 )).

fof(tlhfof46427,axiom,(
    cell82 != cell9 )).

fof(tlhfof46428,axiom,(
    cell82 != cell8 )).

fof(tlhfof46429,axiom,(
    cell82 != cell7 )).

fof(tlhfof46430,axiom,(
    cell82 != cell6 )).

fof(tlhfof46431,axiom,(
    cell82 != cell5 )).

fof(tlhfof46432,axiom,(
    cell82 != cell4 )).

fof(tlhfof46433,axiom,(
    cell82 != cell3 )).

fof(tlhfof46434,axiom,(
    cell82 != cell1 )).

fof(tlhfof46435,axiom,(
    cell82 != cell89 )).

fof(tlhfof46436,axiom,(
    cell82 != cell88 )).

fof(tlhfof46437,axiom,(
    cell82 != cell79 )).

fof(tlhfof46438,axiom,(
    cell82 != cell90 )).

fof(tlhfof46439,axiom,(
    cell82 != cell99 )).

fof(tlhfof46440,axiom,(
    cell82 != cell12 )).

fof(tlhfof46441,axiom,(
    cell82 != cell11 )).

fof(tlhfof46442,axiom,(
    cell82 != cell2 )).

fof(tlhfof46443,axiom,(
    cell82 != cell13 )).

fof(tlhfof46444,axiom,(
    cell82 != cell22 )).

fof(tlhfof46445,axiom,(
    cell80 != cell78 )).

fof(tlhfof46446,axiom,(
    cell80 != cell77 )).

fof(tlhfof46447,axiom,(
    cell80 != cell76 )).

fof(tlhfof46448,axiom,(
    cell80 != cell75 )).

fof(tlhfof46449,axiom,(
    cell80 != cell74 )).

fof(tlhfof46450,axiom,(
    cell80 != cell73 )).

fof(tlhfof46451,axiom,(
    cell80 != cell71 )).

fof(tlhfof46452,axiom,(
    cell80 != cell72 )).

fof(tlhfof46453,axiom,(
    cell80 != cell70 )).

fof(tlhfof46454,axiom,(
    cell80 != cell69 )).

fof(tlhfof46455,axiom,(
    cell80 != cell68 )).

fof(tlhfof46456,axiom,(
    cell80 != cell67 )).

fof(tlhfof46457,axiom,(
    cell80 != cell66 )).

fof(tlhfof46458,axiom,(
    cell80 != cell65 )).

fof(tlhfof46459,axiom,(
    cell80 != cell64 )).

fof(tlhfof46460,axiom,(
    cell80 != cell63 )).

fof(tlhfof46461,axiom,(
    cell80 != cell61 )).

fof(tlhfof46462,axiom,(
    cell80 != cell62 )).

fof(tlhfof46463,axiom,(
    cell80 != cell60 )).

fof(tlhfof46464,axiom,(
    cell80 != cell59 )).

fof(tlhfof46465,axiom,(
    cell80 != cell58 )).

fof(tlhfof46466,axiom,(
    cell80 != cell57 )).

fof(tlhfof46467,axiom,(
    cell80 != cell56 )).

fof(tlhfof46468,axiom,(
    cell80 != cell55 )).

fof(tlhfof46469,axiom,(
    cell80 != cell54 )).

fof(tlhfof46470,axiom,(
    cell80 != cell53 )).

fof(tlhfof46471,axiom,(
    cell80 != cell51 )).

fof(tlhfof46472,axiom,(
    cell80 != cell52 )).

fof(tlhfof46473,axiom,(
    cell80 != cell50 )).

fof(tlhfof46474,axiom,(
    cell80 != cell49 )).

fof(tlhfof46475,axiom,(
    cell80 != cell48 )).

fof(tlhfof46476,axiom,(
    cell80 != cell47 )).

fof(tlhfof46477,axiom,(
    cell80 != cell46 )).

fof(tlhfof46478,axiom,(
    cell80 != cell45 )).

fof(tlhfof46479,axiom,(
    cell80 != cell44 )).

fof(tlhfof46480,axiom,(
    cell80 != cell43 )).

fof(tlhfof46481,axiom,(
    cell80 != cell41 )).

fof(tlhfof46482,axiom,(
    cell80 != cell42 )).

fof(tlhfof46483,axiom,(
    cell80 != cell40 )).

fof(tlhfof46484,axiom,(
    cell80 != cell39 )).

fof(tlhfof46485,axiom,(
    cell80 != cell38 )).

fof(tlhfof46486,axiom,(
    cell80 != cell37 )).

fof(tlhfof46487,axiom,(
    cell80 != cell36 )).

fof(tlhfof46488,axiom,(
    cell80 != cell35 )).

fof(tlhfof46489,axiom,(
    cell80 != cell34 )).

fof(tlhfof46490,axiom,(
    cell80 != cell33 )).

fof(tlhfof46491,axiom,(
    cell80 != cell31 )).

fof(tlhfof46492,axiom,(
    cell80 != cell32 )).

fof(tlhfof46493,axiom,(
    cell80 != cell30 )).

fof(tlhfof46494,axiom,(
    cell80 != cell29 )).

fof(tlhfof46495,axiom,(
    cell80 != cell28 )).

fof(tlhfof46496,axiom,(
    cell80 != cell27 )).

fof(tlhfof46497,axiom,(
    cell80 != cell26 )).

fof(tlhfof46498,axiom,(
    cell80 != cell25 )).

fof(tlhfof46499,axiom,(
    cell80 != cell24 )).

fof(tlhfof46500,axiom,(
    cell80 != cell23 )).

fof(tlhfof46501,axiom,(
    cell80 != cell21 )).

fof(tlhfof46502,axiom,(
    cell80 != cell20 )).

fof(tlhfof46503,axiom,(
    cell80 != cell19 )).

fof(tlhfof46504,axiom,(
    cell80 != cell18 )).

fof(tlhfof46505,axiom,(
    cell80 != cell17 )).

fof(tlhfof46506,axiom,(
    cell80 != cell16 )).

fof(tlhfof46507,axiom,(
    cell80 != cell15 )).

fof(tlhfof46508,axiom,(
    cell80 != cell14 )).

fof(tlhfof46509,axiom,(
    cell80 != cell10 )).

fof(tlhfof46510,axiom,(
    cell80 != cell9 )).

fof(tlhfof46511,axiom,(
    cell80 != cell8 )).

fof(tlhfof46512,axiom,(
    cell80 != cell7 )).

fof(tlhfof46513,axiom,(
    cell80 != cell6 )).

fof(tlhfof46514,axiom,(
    cell80 != cell5 )).

fof(tlhfof46515,axiom,(
    cell80 != cell4 )).

fof(tlhfof46516,axiom,(
    cell80 != cell3 )).

fof(tlhfof46517,axiom,(
    cell80 != cell1 )).

fof(tlhfof46518,axiom,(
    cell80 != cell89 )).

fof(tlhfof46519,axiom,(
    cell80 != cell88 )).

fof(tlhfof46520,axiom,(
    cell80 != cell79 )).

fof(tlhfof46521,axiom,(
    cell80 != cell90 )).

fof(tlhfof46522,axiom,(
    cell80 != cell99 )).

fof(tlhfof46523,axiom,(
    cell80 != cell12 )).

fof(tlhfof46524,axiom,(
    cell80 != cell11 )).

fof(tlhfof46525,axiom,(
    cell80 != cell2 )).

fof(tlhfof46526,axiom,(
    cell80 != cell13 )).

fof(tlhfof46527,axiom,(
    cell80 != cell22 )).

fof(tlhfof46528,axiom,(
    cell78 != cell77 )).

fof(tlhfof46529,axiom,(
    cell78 != cell76 )).

fof(tlhfof46530,axiom,(
    cell78 != cell75 )).

fof(tlhfof46531,axiom,(
    cell78 != cell74 )).

fof(tlhfof46532,axiom,(
    cell78 != cell73 )).

fof(tlhfof46533,axiom,(
    cell78 != cell71 )).

fof(tlhfof46534,axiom,(
    cell78 != cell72 )).

fof(tlhfof46535,axiom,(
    cell78 != cell70 )).

fof(tlhfof46536,axiom,(
    cell78 != cell69 )).

fof(tlhfof46537,axiom,(
    cell78 != cell68 )).

fof(tlhfof46538,axiom,(
    cell78 != cell67 )).

fof(tlhfof46539,axiom,(
    cell78 != cell66 )).

fof(tlhfof46540,axiom,(
    cell78 != cell65 )).

fof(tlhfof46541,axiom,(
    cell78 != cell64 )).

fof(tlhfof46542,axiom,(
    cell78 != cell63 )).

fof(tlhfof46543,axiom,(
    cell78 != cell61 )).

fof(tlhfof46544,axiom,(
    cell78 != cell62 )).

fof(tlhfof46545,axiom,(
    cell78 != cell60 )).

fof(tlhfof46546,axiom,(
    cell78 != cell59 )).

fof(tlhfof46547,axiom,(
    cell78 != cell58 )).

fof(tlhfof46548,axiom,(
    cell78 != cell57 )).

fof(tlhfof46549,axiom,(
    cell78 != cell56 )).

fof(tlhfof46550,axiom,(
    cell78 != cell55 )).

fof(tlhfof46551,axiom,(
    cell78 != cell54 )).

fof(tlhfof46552,axiom,(
    cell78 != cell53 )).

fof(tlhfof46553,axiom,(
    cell78 != cell51 )).

fof(tlhfof46554,axiom,(
    cell78 != cell52 )).

fof(tlhfof46555,axiom,(
    cell78 != cell50 )).

fof(tlhfof46556,axiom,(
    cell78 != cell49 )).

fof(tlhfof46557,axiom,(
    cell78 != cell48 )).

fof(tlhfof46558,axiom,(
    cell78 != cell47 )).

fof(tlhfof46559,axiom,(
    cell78 != cell46 )).

fof(tlhfof46560,axiom,(
    cell78 != cell45 )).

fof(tlhfof46561,axiom,(
    cell78 != cell44 )).

fof(tlhfof46562,axiom,(
    cell78 != cell43 )).

fof(tlhfof46563,axiom,(
    cell78 != cell41 )).

fof(tlhfof46564,axiom,(
    cell78 != cell42 )).

fof(tlhfof46565,axiom,(
    cell78 != cell40 )).

fof(tlhfof46566,axiom,(
    cell78 != cell39 )).

fof(tlhfof46567,axiom,(
    cell78 != cell38 )).

fof(tlhfof46568,axiom,(
    cell78 != cell37 )).

fof(tlhfof46569,axiom,(
    cell78 != cell36 )).

fof(tlhfof46570,axiom,(
    cell78 != cell35 )).

fof(tlhfof46571,axiom,(
    cell78 != cell34 )).

fof(tlhfof46572,axiom,(
    cell78 != cell33 )).

fof(tlhfof46573,axiom,(
    cell78 != cell31 )).

fof(tlhfof46574,axiom,(
    cell78 != cell32 )).

fof(tlhfof46575,axiom,(
    cell78 != cell30 )).

fof(tlhfof46576,axiom,(
    cell78 != cell29 )).

fof(tlhfof46577,axiom,(
    cell78 != cell28 )).

fof(tlhfof46578,axiom,(
    cell78 != cell27 )).

fof(tlhfof46579,axiom,(
    cell78 != cell26 )).

fof(tlhfof46580,axiom,(
    cell78 != cell25 )).

fof(tlhfof46581,axiom,(
    cell78 != cell24 )).

fof(tlhfof46582,axiom,(
    cell78 != cell23 )).

fof(tlhfof46583,axiom,(
    cell78 != cell21 )).

fof(tlhfof46584,axiom,(
    cell78 != cell20 )).

fof(tlhfof46585,axiom,(
    cell78 != cell19 )).

fof(tlhfof46586,axiom,(
    cell78 != cell18 )).

fof(tlhfof46587,axiom,(
    cell78 != cell17 )).

fof(tlhfof46588,axiom,(
    cell78 != cell16 )).

fof(tlhfof46589,axiom,(
    cell78 != cell15 )).

fof(tlhfof46590,axiom,(
    cell78 != cell14 )).

fof(tlhfof46591,axiom,(
    cell78 != cell10 )).

fof(tlhfof46592,axiom,(
    cell78 != cell9 )).

fof(tlhfof46593,axiom,(
    cell78 != cell8 )).

fof(tlhfof46594,axiom,(
    cell78 != cell7 )).

fof(tlhfof46595,axiom,(
    cell78 != cell6 )).

fof(tlhfof46596,axiom,(
    cell78 != cell5 )).

fof(tlhfof46597,axiom,(
    cell78 != cell4 )).

fof(tlhfof46598,axiom,(
    cell78 != cell3 )).

fof(tlhfof46599,axiom,(
    cell78 != cell1 )).

fof(tlhfof46600,axiom,(
    cell78 != cell89 )).

fof(tlhfof46601,axiom,(
    cell78 != cell88 )).

fof(tlhfof46602,axiom,(
    cell78 != cell79 )).

fof(tlhfof46603,axiom,(
    cell78 != cell90 )).

fof(tlhfof46604,axiom,(
    cell78 != cell99 )).

fof(tlhfof46605,axiom,(
    cell78 != cell12 )).

fof(tlhfof46606,axiom,(
    cell78 != cell11 )).

fof(tlhfof46607,axiom,(
    cell78 != cell2 )).

fof(tlhfof46608,axiom,(
    cell78 != cell13 )).

fof(tlhfof46609,axiom,(
    cell78 != cell22 )).

fof(tlhfof46610,axiom,(
    cell77 != cell76 )).

fof(tlhfof46611,axiom,(
    cell77 != cell75 )).

fof(tlhfof46612,axiom,(
    cell77 != cell74 )).

fof(tlhfof46613,axiom,(
    cell77 != cell73 )).

fof(tlhfof46614,axiom,(
    cell77 != cell71 )).

fof(tlhfof46615,axiom,(
    cell77 != cell72 )).

fof(tlhfof46616,axiom,(
    cell77 != cell70 )).

fof(tlhfof46617,axiom,(
    cell77 != cell69 )).

fof(tlhfof46618,axiom,(
    cell77 != cell68 )).

fof(tlhfof46619,axiom,(
    cell77 != cell67 )).

fof(tlhfof46620,axiom,(
    cell77 != cell66 )).

fof(tlhfof46621,axiom,(
    cell77 != cell65 )).

fof(tlhfof46622,axiom,(
    cell77 != cell64 )).

fof(tlhfof46623,axiom,(
    cell77 != cell63 )).

fof(tlhfof46624,axiom,(
    cell77 != cell61 )).

fof(tlhfof46625,axiom,(
    cell77 != cell62 )).

fof(tlhfof46626,axiom,(
    cell77 != cell60 )).

fof(tlhfof46627,axiom,(
    cell77 != cell59 )).

fof(tlhfof46628,axiom,(
    cell77 != cell58 )).

fof(tlhfof46629,axiom,(
    cell77 != cell57 )).

fof(tlhfof46630,axiom,(
    cell77 != cell56 )).

fof(tlhfof46631,axiom,(
    cell77 != cell55 )).

fof(tlhfof46632,axiom,(
    cell77 != cell54 )).

fof(tlhfof46633,axiom,(
    cell77 != cell53 )).

fof(tlhfof46634,axiom,(
    cell77 != cell51 )).

fof(tlhfof46635,axiom,(
    cell77 != cell52 )).

fof(tlhfof46636,axiom,(
    cell77 != cell50 )).

fof(tlhfof46637,axiom,(
    cell77 != cell49 )).

fof(tlhfof46638,axiom,(
    cell77 != cell48 )).

fof(tlhfof46639,axiom,(
    cell77 != cell47 )).

fof(tlhfof46640,axiom,(
    cell77 != cell46 )).

fof(tlhfof46641,axiom,(
    cell77 != cell45 )).

fof(tlhfof46642,axiom,(
    cell77 != cell44 )).

fof(tlhfof46643,axiom,(
    cell77 != cell43 )).

fof(tlhfof46644,axiom,(
    cell77 != cell41 )).

fof(tlhfof46645,axiom,(
    cell77 != cell42 )).

fof(tlhfof46646,axiom,(
    cell77 != cell40 )).

fof(tlhfof46647,axiom,(
    cell77 != cell39 )).

fof(tlhfof46648,axiom,(
    cell77 != cell38 )).

fof(tlhfof46649,axiom,(
    cell77 != cell37 )).

fof(tlhfof46650,axiom,(
    cell77 != cell36 )).

fof(tlhfof46651,axiom,(
    cell77 != cell35 )).

fof(tlhfof46652,axiom,(
    cell77 != cell34 )).

fof(tlhfof46653,axiom,(
    cell77 != cell33 )).

fof(tlhfof46654,axiom,(
    cell77 != cell31 )).

fof(tlhfof46655,axiom,(
    cell77 != cell32 )).

fof(tlhfof46656,axiom,(
    cell77 != cell30 )).

fof(tlhfof46657,axiom,(
    cell77 != cell29 )).

fof(tlhfof46658,axiom,(
    cell77 != cell28 )).

fof(tlhfof46659,axiom,(
    cell77 != cell27 )).

fof(tlhfof46660,axiom,(
    cell77 != cell26 )).

fof(tlhfof46661,axiom,(
    cell77 != cell25 )).

fof(tlhfof46662,axiom,(
    cell77 != cell24 )).

fof(tlhfof46663,axiom,(
    cell77 != cell23 )).

fof(tlhfof46664,axiom,(
    cell77 != cell21 )).

fof(tlhfof46665,axiom,(
    cell77 != cell20 )).

fof(tlhfof46666,axiom,(
    cell77 != cell19 )).

fof(tlhfof46667,axiom,(
    cell77 != cell18 )).

fof(tlhfof46668,axiom,(
    cell77 != cell17 )).

fof(tlhfof46669,axiom,(
    cell77 != cell16 )).

fof(tlhfof46670,axiom,(
    cell77 != cell15 )).

fof(tlhfof46671,axiom,(
    cell77 != cell14 )).

fof(tlhfof46672,axiom,(
    cell77 != cell10 )).

fof(tlhfof46673,axiom,(
    cell77 != cell9 )).

fof(tlhfof46674,axiom,(
    cell77 != cell8 )).

fof(tlhfof46675,axiom,(
    cell77 != cell7 )).

fof(tlhfof46676,axiom,(
    cell77 != cell6 )).

fof(tlhfof46677,axiom,(
    cell77 != cell5 )).

fof(tlhfof46678,axiom,(
    cell77 != cell4 )).

fof(tlhfof46679,axiom,(
    cell77 != cell3 )).

fof(tlhfof46680,axiom,(
    cell77 != cell1 )).

fof(tlhfof46681,axiom,(
    cell77 != cell89 )).

fof(tlhfof46682,axiom,(
    cell77 != cell88 )).

fof(tlhfof46683,axiom,(
    cell77 != cell79 )).

fof(tlhfof46684,axiom,(
    cell77 != cell90 )).

fof(tlhfof46685,axiom,(
    cell77 != cell99 )).

fof(tlhfof46686,axiom,(
    cell77 != cell12 )).

fof(tlhfof46687,axiom,(
    cell77 != cell11 )).

fof(tlhfof46688,axiom,(
    cell77 != cell2 )).

fof(tlhfof46689,axiom,(
    cell77 != cell13 )).

fof(tlhfof46690,axiom,(
    cell77 != cell22 )).

fof(tlhfof46691,axiom,(
    cell76 != cell75 )).

fof(tlhfof46692,axiom,(
    cell76 != cell74 )).

fof(tlhfof46693,axiom,(
    cell76 != cell73 )).

fof(tlhfof46694,axiom,(
    cell76 != cell71 )).

fof(tlhfof46695,axiom,(
    cell76 != cell72 )).

fof(tlhfof46696,axiom,(
    cell76 != cell70 )).

fof(tlhfof46697,axiom,(
    cell76 != cell69 )).

fof(tlhfof46698,axiom,(
    cell76 != cell68 )).

fof(tlhfof46699,axiom,(
    cell76 != cell67 )).

fof(tlhfof46700,axiom,(
    cell76 != cell66 )).

fof(tlhfof46701,axiom,(
    cell76 != cell65 )).

fof(tlhfof46702,axiom,(
    cell76 != cell64 )).

fof(tlhfof46703,axiom,(
    cell76 != cell63 )).

fof(tlhfof46704,axiom,(
    cell76 != cell61 )).

fof(tlhfof46705,axiom,(
    cell76 != cell62 )).

fof(tlhfof46706,axiom,(
    cell76 != cell60 )).

fof(tlhfof46707,axiom,(
    cell76 != cell59 )).

fof(tlhfof46708,axiom,(
    cell76 != cell58 )).

fof(tlhfof46709,axiom,(
    cell76 != cell57 )).

fof(tlhfof46710,axiom,(
    cell76 != cell56 )).

fof(tlhfof46711,axiom,(
    cell76 != cell55 )).

fof(tlhfof46712,axiom,(
    cell76 != cell54 )).

fof(tlhfof46713,axiom,(
    cell76 != cell53 )).

fof(tlhfof46714,axiom,(
    cell76 != cell51 )).

fof(tlhfof46715,axiom,(
    cell76 != cell52 )).

fof(tlhfof46716,axiom,(
    cell76 != cell50 )).

fof(tlhfof46717,axiom,(
    cell76 != cell49 )).

fof(tlhfof46718,axiom,(
    cell76 != cell48 )).

fof(tlhfof46719,axiom,(
    cell76 != cell47 )).

fof(tlhfof46720,axiom,(
    cell76 != cell46 )).

fof(tlhfof46721,axiom,(
    cell76 != cell45 )).

fof(tlhfof46722,axiom,(
    cell76 != cell44 )).

fof(tlhfof46723,axiom,(
    cell76 != cell43 )).

fof(tlhfof46724,axiom,(
    cell76 != cell41 )).

fof(tlhfof46725,axiom,(
    cell76 != cell42 )).

fof(tlhfof46726,axiom,(
    cell76 != cell40 )).

fof(tlhfof46727,axiom,(
    cell76 != cell39 )).

fof(tlhfof46728,axiom,(
    cell76 != cell38 )).

fof(tlhfof46729,axiom,(
    cell76 != cell37 )).

fof(tlhfof46730,axiom,(
    cell76 != cell36 )).

fof(tlhfof46731,axiom,(
    cell76 != cell35 )).

fof(tlhfof46732,axiom,(
    cell76 != cell34 )).

fof(tlhfof46733,axiom,(
    cell76 != cell33 )).

fof(tlhfof46734,axiom,(
    cell76 != cell31 )).

fof(tlhfof46735,axiom,(
    cell76 != cell32 )).

fof(tlhfof46736,axiom,(
    cell76 != cell30 )).

fof(tlhfof46737,axiom,(
    cell76 != cell29 )).

fof(tlhfof46738,axiom,(
    cell76 != cell28 )).

fof(tlhfof46739,axiom,(
    cell76 != cell27 )).

fof(tlhfof46740,axiom,(
    cell76 != cell26 )).

fof(tlhfof46741,axiom,(
    cell76 != cell25 )).

fof(tlhfof46742,axiom,(
    cell76 != cell24 )).

fof(tlhfof46743,axiom,(
    cell76 != cell23 )).

fof(tlhfof46744,axiom,(
    cell76 != cell21 )).

fof(tlhfof46745,axiom,(
    cell76 != cell20 )).

fof(tlhfof46746,axiom,(
    cell76 != cell19 )).

fof(tlhfof46747,axiom,(
    cell76 != cell18 )).

fof(tlhfof46748,axiom,(
    cell76 != cell17 )).

fof(tlhfof46749,axiom,(
    cell76 != cell16 )).

fof(tlhfof46750,axiom,(
    cell76 != cell15 )).

fof(tlhfof46751,axiom,(
    cell76 != cell14 )).

fof(tlhfof46752,axiom,(
    cell76 != cell10 )).

fof(tlhfof46753,axiom,(
    cell76 != cell9 )).

fof(tlhfof46754,axiom,(
    cell76 != cell8 )).

fof(tlhfof46755,axiom,(
    cell76 != cell7 )).

fof(tlhfof46756,axiom,(
    cell76 != cell6 )).

fof(tlhfof46757,axiom,(
    cell76 != cell5 )).

fof(tlhfof46758,axiom,(
    cell76 != cell4 )).

fof(tlhfof46759,axiom,(
    cell76 != cell3 )).

fof(tlhfof46760,axiom,(
    cell76 != cell1 )).

fof(tlhfof46761,axiom,(
    cell76 != cell89 )).

fof(tlhfof46762,axiom,(
    cell76 != cell88 )).

fof(tlhfof46763,axiom,(
    cell76 != cell79 )).

fof(tlhfof46764,axiom,(
    cell76 != cell90 )).

fof(tlhfof46765,axiom,(
    cell76 != cell99 )).

fof(tlhfof46766,axiom,(
    cell76 != cell12 )).

fof(tlhfof46767,axiom,(
    cell76 != cell11 )).

fof(tlhfof46768,axiom,(
    cell76 != cell2 )).

fof(tlhfof46769,axiom,(
    cell76 != cell13 )).

fof(tlhfof46770,axiom,(
    cell76 != cell22 )).

fof(tlhfof46771,axiom,(
    cell75 != cell74 )).

fof(tlhfof46772,axiom,(
    cell75 != cell73 )).

fof(tlhfof46773,axiom,(
    cell75 != cell71 )).

fof(tlhfof46774,axiom,(
    cell75 != cell72 )).

fof(tlhfof46775,axiom,(
    cell75 != cell70 )).

fof(tlhfof46776,axiom,(
    cell75 != cell69 )).

fof(tlhfof46777,axiom,(
    cell75 != cell68 )).

fof(tlhfof46778,axiom,(
    cell75 != cell67 )).

fof(tlhfof46779,axiom,(
    cell75 != cell66 )).

fof(tlhfof46780,axiom,(
    cell75 != cell65 )).

fof(tlhfof46781,axiom,(
    cell75 != cell64 )).

fof(tlhfof46782,axiom,(
    cell75 != cell63 )).

fof(tlhfof46783,axiom,(
    cell75 != cell61 )).

fof(tlhfof46784,axiom,(
    cell75 != cell62 )).

fof(tlhfof46785,axiom,(
    cell75 != cell60 )).

fof(tlhfof46786,axiom,(
    cell75 != cell59 )).

fof(tlhfof46787,axiom,(
    cell75 != cell58 )).

fof(tlhfof46788,axiom,(
    cell75 != cell57 )).

fof(tlhfof46789,axiom,(
    cell75 != cell56 )).

fof(tlhfof46790,axiom,(
    cell75 != cell55 )).

fof(tlhfof46791,axiom,(
    cell75 != cell54 )).

fof(tlhfof46792,axiom,(
    cell75 != cell53 )).

fof(tlhfof46793,axiom,(
    cell75 != cell51 )).

fof(tlhfof46794,axiom,(
    cell75 != cell52 )).

fof(tlhfof46795,axiom,(
    cell75 != cell50 )).

fof(tlhfof46796,axiom,(
    cell75 != cell49 )).

fof(tlhfof46797,axiom,(
    cell75 != cell48 )).

fof(tlhfof46798,axiom,(
    cell75 != cell47 )).

fof(tlhfof46799,axiom,(
    cell75 != cell46 )).

fof(tlhfof46800,axiom,(
    cell75 != cell45 )).

fof(tlhfof46801,axiom,(
    cell75 != cell44 )).

fof(tlhfof46802,axiom,(
    cell75 != cell43 )).

fof(tlhfof46803,axiom,(
    cell75 != cell41 )).

fof(tlhfof46804,axiom,(
    cell75 != cell42 )).

fof(tlhfof46805,axiom,(
    cell75 != cell40 )).

fof(tlhfof46806,axiom,(
    cell75 != cell39 )).

fof(tlhfof46807,axiom,(
    cell75 != cell38 )).

fof(tlhfof46808,axiom,(
    cell75 != cell37 )).

fof(tlhfof46809,axiom,(
    cell75 != cell36 )).

fof(tlhfof46810,axiom,(
    cell75 != cell35 )).

fof(tlhfof46811,axiom,(
    cell75 != cell34 )).

fof(tlhfof46812,axiom,(
    cell75 != cell33 )).

fof(tlhfof46813,axiom,(
    cell75 != cell31 )).

fof(tlhfof46814,axiom,(
    cell75 != cell32 )).

fof(tlhfof46815,axiom,(
    cell75 != cell30 )).

fof(tlhfof46816,axiom,(
    cell75 != cell29 )).

fof(tlhfof46817,axiom,(
    cell75 != cell28 )).

fof(tlhfof46818,axiom,(
    cell75 != cell27 )).

fof(tlhfof46819,axiom,(
    cell75 != cell26 )).

fof(tlhfof46820,axiom,(
    cell75 != cell25 )).

fof(tlhfof46821,axiom,(
    cell75 != cell24 )).

fof(tlhfof46822,axiom,(
    cell75 != cell23 )).

fof(tlhfof46823,axiom,(
    cell75 != cell21 )).

fof(tlhfof46824,axiom,(
    cell75 != cell20 )).

fof(tlhfof46825,axiom,(
    cell75 != cell19 )).

fof(tlhfof46826,axiom,(
    cell75 != cell18 )).

fof(tlhfof46827,axiom,(
    cell75 != cell17 )).

fof(tlhfof46828,axiom,(
    cell75 != cell16 )).

fof(tlhfof46829,axiom,(
    cell75 != cell15 )).

fof(tlhfof46830,axiom,(
    cell75 != cell14 )).

fof(tlhfof46831,axiom,(
    cell75 != cell10 )).

fof(tlhfof46832,axiom,(
    cell75 != cell9 )).

fof(tlhfof46833,axiom,(
    cell75 != cell8 )).

fof(tlhfof46834,axiom,(
    cell75 != cell7 )).

fof(tlhfof46835,axiom,(
    cell75 != cell6 )).

fof(tlhfof46836,axiom,(
    cell75 != cell5 )).

fof(tlhfof46837,axiom,(
    cell75 != cell4 )).

fof(tlhfof46838,axiom,(
    cell75 != cell3 )).

fof(tlhfof46839,axiom,(
    cell75 != cell1 )).

fof(tlhfof46840,axiom,(
    cell75 != cell89 )).

fof(tlhfof46841,axiom,(
    cell75 != cell88 )).

fof(tlhfof46842,axiom,(
    cell75 != cell79 )).

fof(tlhfof46843,axiom,(
    cell75 != cell90 )).

fof(tlhfof46844,axiom,(
    cell75 != cell99 )).

fof(tlhfof46845,axiom,(
    cell75 != cell12 )).

fof(tlhfof46846,axiom,(
    cell75 != cell11 )).

fof(tlhfof46847,axiom,(
    cell75 != cell2 )).

fof(tlhfof46848,axiom,(
    cell75 != cell13 )).

fof(tlhfof46849,axiom,(
    cell75 != cell22 )).

fof(tlhfof46850,axiom,(
    cell74 != cell73 )).

fof(tlhfof46851,axiom,(
    cell74 != cell71 )).

fof(tlhfof46852,axiom,(
    cell74 != cell72 )).

fof(tlhfof46853,axiom,(
    cell74 != cell70 )).

fof(tlhfof46854,axiom,(
    cell74 != cell69 )).

fof(tlhfof46855,axiom,(
    cell74 != cell68 )).

fof(tlhfof46856,axiom,(
    cell74 != cell67 )).

fof(tlhfof46857,axiom,(
    cell74 != cell66 )).

fof(tlhfof46858,axiom,(
    cell74 != cell65 )).

fof(tlhfof46859,axiom,(
    cell74 != cell64 )).

fof(tlhfof46860,axiom,(
    cell74 != cell63 )).

fof(tlhfof46861,axiom,(
    cell74 != cell61 )).

fof(tlhfof46862,axiom,(
    cell74 != cell62 )).

fof(tlhfof46863,axiom,(
    cell74 != cell60 )).

fof(tlhfof46864,axiom,(
    cell74 != cell59 )).

fof(tlhfof46865,axiom,(
    cell74 != cell58 )).

fof(tlhfof46866,axiom,(
    cell74 != cell57 )).

fof(tlhfof46867,axiom,(
    cell74 != cell56 )).

fof(tlhfof46868,axiom,(
    cell74 != cell55 )).

fof(tlhfof46869,axiom,(
    cell74 != cell54 )).

fof(tlhfof46870,axiom,(
    cell74 != cell53 )).

fof(tlhfof46871,axiom,(
    cell74 != cell51 )).

fof(tlhfof46872,axiom,(
    cell74 != cell52 )).

fof(tlhfof46873,axiom,(
    cell74 != cell50 )).

fof(tlhfof46874,axiom,(
    cell74 != cell49 )).

fof(tlhfof46875,axiom,(
    cell74 != cell48 )).

fof(tlhfof46876,axiom,(
    cell74 != cell47 )).

fof(tlhfof46877,axiom,(
    cell74 != cell46 )).

fof(tlhfof46878,axiom,(
    cell74 != cell45 )).

fof(tlhfof46879,axiom,(
    cell74 != cell44 )).

fof(tlhfof46880,axiom,(
    cell74 != cell43 )).

fof(tlhfof46881,axiom,(
    cell74 != cell41 )).

fof(tlhfof46882,axiom,(
    cell74 != cell42 )).

fof(tlhfof46883,axiom,(
    cell74 != cell40 )).

fof(tlhfof46884,axiom,(
    cell74 != cell39 )).

fof(tlhfof46885,axiom,(
    cell74 != cell38 )).

fof(tlhfof46886,axiom,(
    cell74 != cell37 )).

fof(tlhfof46887,axiom,(
    cell74 != cell36 )).

fof(tlhfof46888,axiom,(
    cell74 != cell35 )).

fof(tlhfof46889,axiom,(
    cell74 != cell34 )).

fof(tlhfof46890,axiom,(
    cell74 != cell33 )).

fof(tlhfof46891,axiom,(
    cell74 != cell31 )).

fof(tlhfof46892,axiom,(
    cell74 != cell32 )).

fof(tlhfof46893,axiom,(
    cell74 != cell30 )).

fof(tlhfof46894,axiom,(
    cell74 != cell29 )).

fof(tlhfof46895,axiom,(
    cell74 != cell28 )).

fof(tlhfof46896,axiom,(
    cell74 != cell27 )).

fof(tlhfof46897,axiom,(
    cell74 != cell26 )).

fof(tlhfof46898,axiom,(
    cell74 != cell25 )).

fof(tlhfof46899,axiom,(
    cell74 != cell24 )).

fof(tlhfof46900,axiom,(
    cell74 != cell23 )).

fof(tlhfof46901,axiom,(
    cell74 != cell21 )).

fof(tlhfof46902,axiom,(
    cell74 != cell20 )).

fof(tlhfof46903,axiom,(
    cell74 != cell19 )).

fof(tlhfof46904,axiom,(
    cell74 != cell18 )).

fof(tlhfof46905,axiom,(
    cell74 != cell17 )).

fof(tlhfof46906,axiom,(
    cell74 != cell16 )).

fof(tlhfof46907,axiom,(
    cell74 != cell15 )).

fof(tlhfof46908,axiom,(
    cell74 != cell14 )).

fof(tlhfof46909,axiom,(
    cell74 != cell10 )).

fof(tlhfof46910,axiom,(
    cell74 != cell9 )).

fof(tlhfof46911,axiom,(
    cell74 != cell8 )).

fof(tlhfof46912,axiom,(
    cell74 != cell7 )).

fof(tlhfof46913,axiom,(
    cell74 != cell6 )).

fof(tlhfof46914,axiom,(
    cell74 != cell5 )).

fof(tlhfof46915,axiom,(
    cell74 != cell4 )).

fof(tlhfof46916,axiom,(
    cell74 != cell3 )).

fof(tlhfof46917,axiom,(
    cell74 != cell1 )).

fof(tlhfof46918,axiom,(
    cell74 != cell89 )).

fof(tlhfof46919,axiom,(
    cell74 != cell88 )).

fof(tlhfof46920,axiom,(
    cell74 != cell79 )).

fof(tlhfof46921,axiom,(
    cell74 != cell90 )).

fof(tlhfof46922,axiom,(
    cell74 != cell99 )).

fof(tlhfof46923,axiom,(
    cell74 != cell12 )).

fof(tlhfof46924,axiom,(
    cell74 != cell11 )).

fof(tlhfof46925,axiom,(
    cell74 != cell2 )).

fof(tlhfof46926,axiom,(
    cell74 != cell13 )).

fof(tlhfof46927,axiom,(
    cell74 != cell22 )).

fof(tlhfof46928,axiom,(
    cell73 != cell71 )).

fof(tlhfof46929,axiom,(
    cell73 != cell72 )).

fof(tlhfof46930,axiom,(
    cell73 != cell70 )).

fof(tlhfof46931,axiom,(
    cell73 != cell69 )).

fof(tlhfof46932,axiom,(
    cell73 != cell68 )).

fof(tlhfof46933,axiom,(
    cell73 != cell67 )).

fof(tlhfof46934,axiom,(
    cell73 != cell66 )).

fof(tlhfof46935,axiom,(
    cell73 != cell65 )).

fof(tlhfof46936,axiom,(
    cell73 != cell64 )).

fof(tlhfof46937,axiom,(
    cell73 != cell63 )).

fof(tlhfof46938,axiom,(
    cell73 != cell61 )).

fof(tlhfof46939,axiom,(
    cell73 != cell62 )).

fof(tlhfof46940,axiom,(
    cell73 != cell60 )).

fof(tlhfof46941,axiom,(
    cell73 != cell59 )).

fof(tlhfof46942,axiom,(
    cell73 != cell58 )).

fof(tlhfof46943,axiom,(
    cell73 != cell57 )).

fof(tlhfof46944,axiom,(
    cell73 != cell56 )).

fof(tlhfof46945,axiom,(
    cell73 != cell55 )).

fof(tlhfof46946,axiom,(
    cell73 != cell54 )).

fof(tlhfof46947,axiom,(
    cell73 != cell53 )).

fof(tlhfof46948,axiom,(
    cell73 != cell51 )).

fof(tlhfof46949,axiom,(
    cell73 != cell52 )).

fof(tlhfof46950,axiom,(
    cell73 != cell50 )).

fof(tlhfof46951,axiom,(
    cell73 != cell49 )).

fof(tlhfof46952,axiom,(
    cell73 != cell48 )).

fof(tlhfof46953,axiom,(
    cell73 != cell47 )).

fof(tlhfof46954,axiom,(
    cell73 != cell46 )).

fof(tlhfof46955,axiom,(
    cell73 != cell45 )).

fof(tlhfof46956,axiom,(
    cell73 != cell44 )).

fof(tlhfof46957,axiom,(
    cell73 != cell43 )).

fof(tlhfof46958,axiom,(
    cell73 != cell41 )).

fof(tlhfof46959,axiom,(
    cell73 != cell42 )).

fof(tlhfof46960,axiom,(
    cell73 != cell40 )).

fof(tlhfof46961,axiom,(
    cell73 != cell39 )).

fof(tlhfof46962,axiom,(
    cell73 != cell38 )).

fof(tlhfof46963,axiom,(
    cell73 != cell37 )).

fof(tlhfof46964,axiom,(
    cell73 != cell36 )).

fof(tlhfof46965,axiom,(
    cell73 != cell35 )).

fof(tlhfof46966,axiom,(
    cell73 != cell34 )).

fof(tlhfof46967,axiom,(
    cell73 != cell33 )).

fof(tlhfof46968,axiom,(
    cell73 != cell31 )).

fof(tlhfof46969,axiom,(
    cell73 != cell32 )).

fof(tlhfof46970,axiom,(
    cell73 != cell30 )).

fof(tlhfof46971,axiom,(
    cell73 != cell29 )).

fof(tlhfof46972,axiom,(
    cell73 != cell28 )).

fof(tlhfof46973,axiom,(
    cell73 != cell27 )).

fof(tlhfof46974,axiom,(
    cell73 != cell26 )).

fof(tlhfof46975,axiom,(
    cell73 != cell25 )).

fof(tlhfof46976,axiom,(
    cell73 != cell24 )).

fof(tlhfof46977,axiom,(
    cell73 != cell23 )).

fof(tlhfof46978,axiom,(
    cell73 != cell21 )).

fof(tlhfof46979,axiom,(
    cell73 != cell20 )).

fof(tlhfof46980,axiom,(
    cell73 != cell19 )).

fof(tlhfof46981,axiom,(
    cell73 != cell18 )).

fof(tlhfof46982,axiom,(
    cell73 != cell17 )).

fof(tlhfof46983,axiom,(
    cell73 != cell16 )).

fof(tlhfof46984,axiom,(
    cell73 != cell15 )).

fof(tlhfof46985,axiom,(
    cell73 != cell14 )).

fof(tlhfof46986,axiom,(
    cell73 != cell10 )).

fof(tlhfof46987,axiom,(
    cell73 != cell9 )).

fof(tlhfof46988,axiom,(
    cell73 != cell8 )).

fof(tlhfof46989,axiom,(
    cell73 != cell7 )).

fof(tlhfof46990,axiom,(
    cell73 != cell6 )).

fof(tlhfof46991,axiom,(
    cell73 != cell5 )).

fof(tlhfof46992,axiom,(
    cell73 != cell4 )).

fof(tlhfof46993,axiom,(
    cell73 != cell3 )).

fof(tlhfof46994,axiom,(
    cell73 != cell1 )).

fof(tlhfof46995,axiom,(
    cell73 != cell89 )).

fof(tlhfof46996,axiom,(
    cell73 != cell88 )).

fof(tlhfof46997,axiom,(
    cell73 != cell79 )).

fof(tlhfof46998,axiom,(
    cell73 != cell90 )).

fof(tlhfof46999,axiom,(
    cell73 != cell99 )).

fof(tlhfof47000,axiom,(
    cell73 != cell12 )).

fof(tlhfof47001,axiom,(
    cell73 != cell11 )).

fof(tlhfof47002,axiom,(
    cell73 != cell2 )).

fof(tlhfof47003,axiom,(
    cell73 != cell13 )).

fof(tlhfof47004,axiom,(
    cell73 != cell22 )).

fof(tlhfof47005,axiom,(
    cell71 != cell72 )).

fof(tlhfof47006,axiom,(
    cell71 != cell70 )).

fof(tlhfof47007,axiom,(
    cell71 != cell69 )).

fof(tlhfof47008,axiom,(
    cell71 != cell68 )).

fof(tlhfof47009,axiom,(
    cell71 != cell67 )).

fof(tlhfof47010,axiom,(
    cell71 != cell66 )).

fof(tlhfof47011,axiom,(
    cell71 != cell65 )).

fof(tlhfof47012,axiom,(
    cell71 != cell64 )).

fof(tlhfof47013,axiom,(
    cell71 != cell63 )).

fof(tlhfof47014,axiom,(
    cell71 != cell61 )).

fof(tlhfof47015,axiom,(
    cell71 != cell62 )).

fof(tlhfof47016,axiom,(
    cell71 != cell60 )).

fof(tlhfof47017,axiom,(
    cell71 != cell59 )).

fof(tlhfof47018,axiom,(
    cell71 != cell58 )).

fof(tlhfof47019,axiom,(
    cell71 != cell57 )).

fof(tlhfof47020,axiom,(
    cell71 != cell56 )).

fof(tlhfof47021,axiom,(
    cell71 != cell55 )).

fof(tlhfof47022,axiom,(
    cell71 != cell54 )).

fof(tlhfof47023,axiom,(
    cell71 != cell53 )).

fof(tlhfof47024,axiom,(
    cell71 != cell51 )).

fof(tlhfof47025,axiom,(
    cell71 != cell52 )).

fof(tlhfof47026,axiom,(
    cell71 != cell50 )).

fof(tlhfof47027,axiom,(
    cell71 != cell49 )).

fof(tlhfof47028,axiom,(
    cell71 != cell48 )).

fof(tlhfof47029,axiom,(
    cell71 != cell47 )).

fof(tlhfof47030,axiom,(
    cell71 != cell46 )).

fof(tlhfof47031,axiom,(
    cell71 != cell45 )).

fof(tlhfof47032,axiom,(
    cell71 != cell44 )).

fof(tlhfof47033,axiom,(
    cell71 != cell43 )).

fof(tlhfof47034,axiom,(
    cell71 != cell41 )).

fof(tlhfof47035,axiom,(
    cell71 != cell42 )).

fof(tlhfof47036,axiom,(
    cell71 != cell40 )).

fof(tlhfof47037,axiom,(
    cell71 != cell39 )).

fof(tlhfof47038,axiom,(
    cell71 != cell38 )).

fof(tlhfof47039,axiom,(
    cell71 != cell37 )).

fof(tlhfof47040,axiom,(
    cell71 != cell36 )).

fof(tlhfof47041,axiom,(
    cell71 != cell35 )).

fof(tlhfof47042,axiom,(
    cell71 != cell34 )).

fof(tlhfof47043,axiom,(
    cell71 != cell33 )).

fof(tlhfof47044,axiom,(
    cell71 != cell31 )).

fof(tlhfof47045,axiom,(
    cell71 != cell32 )).

fof(tlhfof47046,axiom,(
    cell71 != cell30 )).

fof(tlhfof47047,axiom,(
    cell71 != cell29 )).

fof(tlhfof47048,axiom,(
    cell71 != cell28 )).

fof(tlhfof47049,axiom,(
    cell71 != cell27 )).

fof(tlhfof47050,axiom,(
    cell71 != cell26 )).

fof(tlhfof47051,axiom,(
    cell71 != cell25 )).

fof(tlhfof47052,axiom,(
    cell71 != cell24 )).

fof(tlhfof47053,axiom,(
    cell71 != cell23 )).

fof(tlhfof47054,axiom,(
    cell71 != cell21 )).

fof(tlhfof47055,axiom,(
    cell71 != cell20 )).

fof(tlhfof47056,axiom,(
    cell71 != cell19 )).

fof(tlhfof47057,axiom,(
    cell71 != cell18 )).

fof(tlhfof47058,axiom,(
    cell71 != cell17 )).

fof(tlhfof47059,axiom,(
    cell71 != cell16 )).

fof(tlhfof47060,axiom,(
    cell71 != cell15 )).

fof(tlhfof47061,axiom,(
    cell71 != cell14 )).

fof(tlhfof47062,axiom,(
    cell71 != cell10 )).

fof(tlhfof47063,axiom,(
    cell71 != cell9 )).

fof(tlhfof47064,axiom,(
    cell71 != cell8 )).

fof(tlhfof47065,axiom,(
    cell71 != cell7 )).

fof(tlhfof47066,axiom,(
    cell71 != cell6 )).

fof(tlhfof47067,axiom,(
    cell71 != cell5 )).

fof(tlhfof47068,axiom,(
    cell71 != cell4 )).

fof(tlhfof47069,axiom,(
    cell71 != cell3 )).

fof(tlhfof47070,axiom,(
    cell71 != cell1 )).

fof(tlhfof47071,axiom,(
    cell71 != cell89 )).

fof(tlhfof47072,axiom,(
    cell71 != cell88 )).

fof(tlhfof47073,axiom,(
    cell71 != cell79 )).

fof(tlhfof47074,axiom,(
    cell71 != cell90 )).

fof(tlhfof47075,axiom,(
    cell71 != cell99 )).

fof(tlhfof47076,axiom,(
    cell71 != cell12 )).

fof(tlhfof47077,axiom,(
    cell71 != cell11 )).

fof(tlhfof47078,axiom,(
    cell71 != cell2 )).

fof(tlhfof47079,axiom,(
    cell71 != cell13 )).

fof(tlhfof47080,axiom,(
    cell71 != cell22 )).

fof(tlhfof47081,axiom,(
    cell72 != cell70 )).

fof(tlhfof47082,axiom,(
    cell72 != cell69 )).

fof(tlhfof47083,axiom,(
    cell72 != cell68 )).

fof(tlhfof47084,axiom,(
    cell72 != cell67 )).

fof(tlhfof47085,axiom,(
    cell72 != cell66 )).

fof(tlhfof47086,axiom,(
    cell72 != cell65 )).

fof(tlhfof47087,axiom,(
    cell72 != cell64 )).

fof(tlhfof47088,axiom,(
    cell72 != cell63 )).

fof(tlhfof47089,axiom,(
    cell72 != cell61 )).

fof(tlhfof47090,axiom,(
    cell72 != cell62 )).

fof(tlhfof47091,axiom,(
    cell72 != cell60 )).

fof(tlhfof47092,axiom,(
    cell72 != cell59 )).

fof(tlhfof47093,axiom,(
    cell72 != cell58 )).

fof(tlhfof47094,axiom,(
    cell72 != cell57 )).

fof(tlhfof47095,axiom,(
    cell72 != cell56 )).

fof(tlhfof47096,axiom,(
    cell72 != cell55 )).

fof(tlhfof47097,axiom,(
    cell72 != cell54 )).

fof(tlhfof47098,axiom,(
    cell72 != cell53 )).

fof(tlhfof47099,axiom,(
    cell72 != cell51 )).

fof(tlhfof47100,axiom,(
    cell72 != cell52 )).

fof(tlhfof47101,axiom,(
    cell72 != cell50 )).

fof(tlhfof47102,axiom,(
    cell72 != cell49 )).

fof(tlhfof47103,axiom,(
    cell72 != cell48 )).

fof(tlhfof47104,axiom,(
    cell72 != cell47 )).

fof(tlhfof47105,axiom,(
    cell72 != cell46 )).

fof(tlhfof47106,axiom,(
    cell72 != cell45 )).

fof(tlhfof47107,axiom,(
    cell72 != cell44 )).

fof(tlhfof47108,axiom,(
    cell72 != cell43 )).

fof(tlhfof47109,axiom,(
    cell72 != cell41 )).

fof(tlhfof47110,axiom,(
    cell72 != cell42 )).

fof(tlhfof47111,axiom,(
    cell72 != cell40 )).

fof(tlhfof47112,axiom,(
    cell72 != cell39 )).

fof(tlhfof47113,axiom,(
    cell72 != cell38 )).

fof(tlhfof47114,axiom,(
    cell72 != cell37 )).

fof(tlhfof47115,axiom,(
    cell72 != cell36 )).

fof(tlhfof47116,axiom,(
    cell72 != cell35 )).

fof(tlhfof47117,axiom,(
    cell72 != cell34 )).

fof(tlhfof47118,axiom,(
    cell72 != cell33 )).

fof(tlhfof47119,axiom,(
    cell72 != cell31 )).

fof(tlhfof47120,axiom,(
    cell72 != cell32 )).

fof(tlhfof47121,axiom,(
    cell72 != cell30 )).

fof(tlhfof47122,axiom,(
    cell72 != cell29 )).

fof(tlhfof47123,axiom,(
    cell72 != cell28 )).

fof(tlhfof47124,axiom,(
    cell72 != cell27 )).

fof(tlhfof47125,axiom,(
    cell72 != cell26 )).

fof(tlhfof47126,axiom,(
    cell72 != cell25 )).

fof(tlhfof47127,axiom,(
    cell72 != cell24 )).

fof(tlhfof47128,axiom,(
    cell72 != cell23 )).

fof(tlhfof47129,axiom,(
    cell72 != cell21 )).

fof(tlhfof47130,axiom,(
    cell72 != cell20 )).

fof(tlhfof47131,axiom,(
    cell72 != cell19 )).

fof(tlhfof47132,axiom,(
    cell72 != cell18 )).

fof(tlhfof47133,axiom,(
    cell72 != cell17 )).

fof(tlhfof47134,axiom,(
    cell72 != cell16 )).

fof(tlhfof47135,axiom,(
    cell72 != cell15 )).

fof(tlhfof47136,axiom,(
    cell72 != cell14 )).

fof(tlhfof47137,axiom,(
    cell72 != cell10 )).

fof(tlhfof47138,axiom,(
    cell72 != cell9 )).

fof(tlhfof47139,axiom,(
    cell72 != cell8 )).

fof(tlhfof47140,axiom,(
    cell72 != cell7 )).

fof(tlhfof47141,axiom,(
    cell72 != cell6 )).

fof(tlhfof47142,axiom,(
    cell72 != cell5 )).

fof(tlhfof47143,axiom,(
    cell72 != cell4 )).

fof(tlhfof47144,axiom,(
    cell72 != cell3 )).

fof(tlhfof47145,axiom,(
    cell72 != cell1 )).

fof(tlhfof47146,axiom,(
    cell72 != cell89 )).

fof(tlhfof47147,axiom,(
    cell72 != cell88 )).

fof(tlhfof47148,axiom,(
    cell72 != cell79 )).

fof(tlhfof47149,axiom,(
    cell72 != cell90 )).

fof(tlhfof47150,axiom,(
    cell72 != cell99 )).

fof(tlhfof47151,axiom,(
    cell72 != cell12 )).

fof(tlhfof47152,axiom,(
    cell72 != cell11 )).

fof(tlhfof47153,axiom,(
    cell72 != cell2 )).

fof(tlhfof47154,axiom,(
    cell72 != cell13 )).

fof(tlhfof47155,axiom,(
    cell72 != cell22 )).

fof(tlhfof47156,axiom,(
    cell70 != cell69 )).

fof(tlhfof47157,axiom,(
    cell70 != cell68 )).

fof(tlhfof47158,axiom,(
    cell70 != cell67 )).

fof(tlhfof47159,axiom,(
    cell70 != cell66 )).

fof(tlhfof47160,axiom,(
    cell70 != cell65 )).

fof(tlhfof47161,axiom,(
    cell70 != cell64 )).

fof(tlhfof47162,axiom,(
    cell70 != cell63 )).

fof(tlhfof47163,axiom,(
    cell70 != cell61 )).

fof(tlhfof47164,axiom,(
    cell70 != cell62 )).

fof(tlhfof47165,axiom,(
    cell70 != cell60 )).

fof(tlhfof47166,axiom,(
    cell70 != cell59 )).

fof(tlhfof47167,axiom,(
    cell70 != cell58 )).

fof(tlhfof47168,axiom,(
    cell70 != cell57 )).

fof(tlhfof47169,axiom,(
    cell70 != cell56 )).

fof(tlhfof47170,axiom,(
    cell70 != cell55 )).

fof(tlhfof47171,axiom,(
    cell70 != cell54 )).

fof(tlhfof47172,axiom,(
    cell70 != cell53 )).

fof(tlhfof47173,axiom,(
    cell70 != cell51 )).

fof(tlhfof47174,axiom,(
    cell70 != cell52 )).

fof(tlhfof47175,axiom,(
    cell70 != cell50 )).

fof(tlhfof47176,axiom,(
    cell70 != cell49 )).

fof(tlhfof47177,axiom,(
    cell70 != cell48 )).

fof(tlhfof47178,axiom,(
    cell70 != cell47 )).

fof(tlhfof47179,axiom,(
    cell70 != cell46 )).

fof(tlhfof47180,axiom,(
    cell70 != cell45 )).

fof(tlhfof47181,axiom,(
    cell70 != cell44 )).

fof(tlhfof47182,axiom,(
    cell70 != cell43 )).

fof(tlhfof47183,axiom,(
    cell70 != cell41 )).

fof(tlhfof47184,axiom,(
    cell70 != cell42 )).

fof(tlhfof47185,axiom,(
    cell70 != cell40 )).

fof(tlhfof47186,axiom,(
    cell70 != cell39 )).

fof(tlhfof47187,axiom,(
    cell70 != cell38 )).

fof(tlhfof47188,axiom,(
    cell70 != cell37 )).

fof(tlhfof47189,axiom,(
    cell70 != cell36 )).

fof(tlhfof47190,axiom,(
    cell70 != cell35 )).

fof(tlhfof47191,axiom,(
    cell70 != cell34 )).

fof(tlhfof47192,axiom,(
    cell70 != cell33 )).

fof(tlhfof47193,axiom,(
    cell70 != cell31 )).

fof(tlhfof47194,axiom,(
    cell70 != cell32 )).

fof(tlhfof47195,axiom,(
    cell70 != cell30 )).

fof(tlhfof47196,axiom,(
    cell70 != cell29 )).

fof(tlhfof47197,axiom,(
    cell70 != cell28 )).

fof(tlhfof47198,axiom,(
    cell70 != cell27 )).

fof(tlhfof47199,axiom,(
    cell70 != cell26 )).

fof(tlhfof47200,axiom,(
    cell70 != cell25 )).

fof(tlhfof47201,axiom,(
    cell70 != cell24 )).

fof(tlhfof47202,axiom,(
    cell70 != cell23 )).

fof(tlhfof47203,axiom,(
    cell70 != cell21 )).

fof(tlhfof47204,axiom,(
    cell70 != cell20 )).

fof(tlhfof47205,axiom,(
    cell70 != cell19 )).

fof(tlhfof47206,axiom,(
    cell70 != cell18 )).

fof(tlhfof47207,axiom,(
    cell70 != cell17 )).

fof(tlhfof47208,axiom,(
    cell70 != cell16 )).

fof(tlhfof47209,axiom,(
    cell70 != cell15 )).

fof(tlhfof47210,axiom,(
    cell70 != cell14 )).

fof(tlhfof47211,axiom,(
    cell70 != cell10 )).

fof(tlhfof47212,axiom,(
    cell70 != cell9 )).

fof(tlhfof47213,axiom,(
    cell70 != cell8 )).

fof(tlhfof47214,axiom,(
    cell70 != cell7 )).

fof(tlhfof47215,axiom,(
    cell70 != cell6 )).

fof(tlhfof47216,axiom,(
    cell70 != cell5 )).

fof(tlhfof47217,axiom,(
    cell70 != cell4 )).

fof(tlhfof47218,axiom,(
    cell70 != cell3 )).

fof(tlhfof47219,axiom,(
    cell70 != cell1 )).

fof(tlhfof47220,axiom,(
    cell70 != cell89 )).

fof(tlhfof47221,axiom,(
    cell70 != cell88 )).

fof(tlhfof47222,axiom,(
    cell70 != cell79 )).

fof(tlhfof47223,axiom,(
    cell70 != cell90 )).

fof(tlhfof47224,axiom,(
    cell70 != cell99 )).

fof(tlhfof47225,axiom,(
    cell70 != cell12 )).

fof(tlhfof47226,axiom,(
    cell70 != cell11 )).

fof(tlhfof47227,axiom,(
    cell70 != cell2 )).

fof(tlhfof47228,axiom,(
    cell70 != cell13 )).

fof(tlhfof47229,axiom,(
    cell70 != cell22 )).

fof(tlhfof47230,axiom,(
    cell69 != cell68 )).

fof(tlhfof47231,axiom,(
    cell69 != cell67 )).

fof(tlhfof47232,axiom,(
    cell69 != cell66 )).

fof(tlhfof47233,axiom,(
    cell69 != cell65 )).

fof(tlhfof47234,axiom,(
    cell69 != cell64 )).

fof(tlhfof47235,axiom,(
    cell69 != cell63 )).

fof(tlhfof47236,axiom,(
    cell69 != cell61 )).

fof(tlhfof47237,axiom,(
    cell69 != cell62 )).

fof(tlhfof47238,axiom,(
    cell69 != cell60 )).

fof(tlhfof47239,axiom,(
    cell69 != cell59 )).

fof(tlhfof47240,axiom,(
    cell69 != cell58 )).

fof(tlhfof47241,axiom,(
    cell69 != cell57 )).

fof(tlhfof47242,axiom,(
    cell69 != cell56 )).

fof(tlhfof47243,axiom,(
    cell69 != cell55 )).

fof(tlhfof47244,axiom,(
    cell69 != cell54 )).

fof(tlhfof47245,axiom,(
    cell69 != cell53 )).

fof(tlhfof47246,axiom,(
    cell69 != cell51 )).

fof(tlhfof47247,axiom,(
    cell69 != cell52 )).

fof(tlhfof47248,axiom,(
    cell69 != cell50 )).

fof(tlhfof47249,axiom,(
    cell69 != cell49 )).

fof(tlhfof47250,axiom,(
    cell69 != cell48 )).

fof(tlhfof47251,axiom,(
    cell69 != cell47 )).

fof(tlhfof47252,axiom,(
    cell69 != cell46 )).

fof(tlhfof47253,axiom,(
    cell69 != cell45 )).

fof(tlhfof47254,axiom,(
    cell69 != cell44 )).

fof(tlhfof47255,axiom,(
    cell69 != cell43 )).

fof(tlhfof47256,axiom,(
    cell69 != cell41 )).

fof(tlhfof47257,axiom,(
    cell69 != cell42 )).

fof(tlhfof47258,axiom,(
    cell69 != cell40 )).

fof(tlhfof47259,axiom,(
    cell69 != cell39 )).

fof(tlhfof47260,axiom,(
    cell69 != cell38 )).

fof(tlhfof47261,axiom,(
    cell69 != cell37 )).

fof(tlhfof47262,axiom,(
    cell69 != cell36 )).

fof(tlhfof47263,axiom,(
    cell69 != cell35 )).

fof(tlhfof47264,axiom,(
    cell69 != cell34 )).

fof(tlhfof47265,axiom,(
    cell69 != cell33 )).

fof(tlhfof47266,axiom,(
    cell69 != cell31 )).

fof(tlhfof47267,axiom,(
    cell69 != cell32 )).

fof(tlhfof47268,axiom,(
    cell69 != cell30 )).

fof(tlhfof47269,axiom,(
    cell69 != cell29 )).

fof(tlhfof47270,axiom,(
    cell69 != cell28 )).

fof(tlhfof47271,axiom,(
    cell69 != cell27 )).

fof(tlhfof47272,axiom,(
    cell69 != cell26 )).

fof(tlhfof47273,axiom,(
    cell69 != cell25 )).

fof(tlhfof47274,axiom,(
    cell69 != cell24 )).

fof(tlhfof47275,axiom,(
    cell69 != cell23 )).

fof(tlhfof47276,axiom,(
    cell69 != cell21 )).

fof(tlhfof47277,axiom,(
    cell69 != cell20 )).

fof(tlhfof47278,axiom,(
    cell69 != cell19 )).

fof(tlhfof47279,axiom,(
    cell69 != cell18 )).

fof(tlhfof47280,axiom,(
    cell69 != cell17 )).

fof(tlhfof47281,axiom,(
    cell69 != cell16 )).

fof(tlhfof47282,axiom,(
    cell69 != cell15 )).

fof(tlhfof47283,axiom,(
    cell69 != cell14 )).

fof(tlhfof47284,axiom,(
    cell69 != cell10 )).

fof(tlhfof47285,axiom,(
    cell69 != cell9 )).

fof(tlhfof47286,axiom,(
    cell69 != cell8 )).

fof(tlhfof47287,axiom,(
    cell69 != cell7 )).

fof(tlhfof47288,axiom,(
    cell69 != cell6 )).

fof(tlhfof47289,axiom,(
    cell69 != cell5 )).

fof(tlhfof47290,axiom,(
    cell69 != cell4 )).

fof(tlhfof47291,axiom,(
    cell69 != cell3 )).

fof(tlhfof47292,axiom,(
    cell69 != cell1 )).

fof(tlhfof47293,axiom,(
    cell69 != cell89 )).

fof(tlhfof47294,axiom,(
    cell69 != cell88 )).

fof(tlhfof47295,axiom,(
    cell69 != cell79 )).

fof(tlhfof47296,axiom,(
    cell69 != cell90 )).

fof(tlhfof47297,axiom,(
    cell69 != cell99 )).

fof(tlhfof47298,axiom,(
    cell69 != cell12 )).

fof(tlhfof47299,axiom,(
    cell69 != cell11 )).

fof(tlhfof47300,axiom,(
    cell69 != cell2 )).

fof(tlhfof47301,axiom,(
    cell69 != cell13 )).

fof(tlhfof47302,axiom,(
    cell69 != cell22 )).

fof(tlhfof47303,axiom,(
    cell68 != cell67 )).

fof(tlhfof47304,axiom,(
    cell68 != cell66 )).

fof(tlhfof47305,axiom,(
    cell68 != cell65 )).

fof(tlhfof47306,axiom,(
    cell68 != cell64 )).

fof(tlhfof47307,axiom,(
    cell68 != cell63 )).

fof(tlhfof47308,axiom,(
    cell68 != cell61 )).

fof(tlhfof47309,axiom,(
    cell68 != cell62 )).

fof(tlhfof47310,axiom,(
    cell68 != cell60 )).

fof(tlhfof47311,axiom,(
    cell68 != cell59 )).

fof(tlhfof47312,axiom,(
    cell68 != cell58 )).

fof(tlhfof47313,axiom,(
    cell68 != cell57 )).

fof(tlhfof47314,axiom,(
    cell68 != cell56 )).

fof(tlhfof47315,axiom,(
    cell68 != cell55 )).

fof(tlhfof47316,axiom,(
    cell68 != cell54 )).

fof(tlhfof47317,axiom,(
    cell68 != cell53 )).

fof(tlhfof47318,axiom,(
    cell68 != cell51 )).

fof(tlhfof47319,axiom,(
    cell68 != cell52 )).

fof(tlhfof47320,axiom,(
    cell68 != cell50 )).

fof(tlhfof47321,axiom,(
    cell68 != cell49 )).

fof(tlhfof47322,axiom,(
    cell68 != cell48 )).

fof(tlhfof47323,axiom,(
    cell68 != cell47 )).

fof(tlhfof47324,axiom,(
    cell68 != cell46 )).

fof(tlhfof47325,axiom,(
    cell68 != cell45 )).

fof(tlhfof47326,axiom,(
    cell68 != cell44 )).

fof(tlhfof47327,axiom,(
    cell68 != cell43 )).

fof(tlhfof47328,axiom,(
    cell68 != cell41 )).

fof(tlhfof47329,axiom,(
    cell68 != cell42 )).

fof(tlhfof47330,axiom,(
    cell68 != cell40 )).

fof(tlhfof47331,axiom,(
    cell68 != cell39 )).

fof(tlhfof47332,axiom,(
    cell68 != cell38 )).

fof(tlhfof47333,axiom,(
    cell68 != cell37 )).

fof(tlhfof47334,axiom,(
    cell68 != cell36 )).

fof(tlhfof47335,axiom,(
    cell68 != cell35 )).

fof(tlhfof47336,axiom,(
    cell68 != cell34 )).

fof(tlhfof47337,axiom,(
    cell68 != cell33 )).

fof(tlhfof47338,axiom,(
    cell68 != cell31 )).

fof(tlhfof47339,axiom,(
    cell68 != cell32 )).

fof(tlhfof47340,axiom,(
    cell68 != cell30 )).

fof(tlhfof47341,axiom,(
    cell68 != cell29 )).

fof(tlhfof47342,axiom,(
    cell68 != cell28 )).

fof(tlhfof47343,axiom,(
    cell68 != cell27 )).

fof(tlhfof47344,axiom,(
    cell68 != cell26 )).

fof(tlhfof47345,axiom,(
    cell68 != cell25 )).

fof(tlhfof47346,axiom,(
    cell68 != cell24 )).

fof(tlhfof47347,axiom,(
    cell68 != cell23 )).

fof(tlhfof47348,axiom,(
    cell68 != cell21 )).

fof(tlhfof47349,axiom,(
    cell68 != cell20 )).

fof(tlhfof47350,axiom,(
    cell68 != cell19 )).

fof(tlhfof47351,axiom,(
    cell68 != cell18 )).

fof(tlhfof47352,axiom,(
    cell68 != cell17 )).

fof(tlhfof47353,axiom,(
    cell68 != cell16 )).

fof(tlhfof47354,axiom,(
    cell68 != cell15 )).

fof(tlhfof47355,axiom,(
    cell68 != cell14 )).

fof(tlhfof47356,axiom,(
    cell68 != cell10 )).

fof(tlhfof47357,axiom,(
    cell68 != cell9 )).

fof(tlhfof47358,axiom,(
    cell68 != cell8 )).

fof(tlhfof47359,axiom,(
    cell68 != cell7 )).

fof(tlhfof47360,axiom,(
    cell68 != cell6 )).

fof(tlhfof47361,axiom,(
    cell68 != cell5 )).

fof(tlhfof47362,axiom,(
    cell68 != cell4 )).

fof(tlhfof47363,axiom,(
    cell68 != cell3 )).

fof(tlhfof47364,axiom,(
    cell68 != cell1 )).

fof(tlhfof47365,axiom,(
    cell68 != cell89 )).

fof(tlhfof47366,axiom,(
    cell68 != cell88 )).

fof(tlhfof47367,axiom,(
    cell68 != cell79 )).

fof(tlhfof47368,axiom,(
    cell68 != cell90 )).

fof(tlhfof47369,axiom,(
    cell68 != cell99 )).

fof(tlhfof47370,axiom,(
    cell68 != cell12 )).

fof(tlhfof47371,axiom,(
    cell68 != cell11 )).

fof(tlhfof47372,axiom,(
    cell68 != cell2 )).

fof(tlhfof47373,axiom,(
    cell68 != cell13 )).

fof(tlhfof47374,axiom,(
    cell68 != cell22 )).

fof(tlhfof47375,axiom,(
    cell67 != cell66 )).

fof(tlhfof47376,axiom,(
    cell67 != cell65 )).

fof(tlhfof47377,axiom,(
    cell67 != cell64 )).

fof(tlhfof47378,axiom,(
    cell67 != cell63 )).

fof(tlhfof47379,axiom,(
    cell67 != cell61 )).

fof(tlhfof47380,axiom,(
    cell67 != cell62 )).

fof(tlhfof47381,axiom,(
    cell67 != cell60 )).

fof(tlhfof47382,axiom,(
    cell67 != cell59 )).

fof(tlhfof47383,axiom,(
    cell67 != cell58 )).

fof(tlhfof47384,axiom,(
    cell67 != cell57 )).

fof(tlhfof47385,axiom,(
    cell67 != cell56 )).

fof(tlhfof47386,axiom,(
    cell67 != cell55 )).

fof(tlhfof47387,axiom,(
    cell67 != cell54 )).

fof(tlhfof47388,axiom,(
    cell67 != cell53 )).

fof(tlhfof47389,axiom,(
    cell67 != cell51 )).

fof(tlhfof47390,axiom,(
    cell67 != cell52 )).

fof(tlhfof47391,axiom,(
    cell67 != cell50 )).

fof(tlhfof47392,axiom,(
    cell67 != cell49 )).

fof(tlhfof47393,axiom,(
    cell67 != cell48 )).

fof(tlhfof47394,axiom,(
    cell67 != cell47 )).

fof(tlhfof47395,axiom,(
    cell67 != cell46 )).

fof(tlhfof47396,axiom,(
    cell67 != cell45 )).

fof(tlhfof47397,axiom,(
    cell67 != cell44 )).

fof(tlhfof47398,axiom,(
    cell67 != cell43 )).

fof(tlhfof47399,axiom,(
    cell67 != cell41 )).

fof(tlhfof47400,axiom,(
    cell67 != cell42 )).

fof(tlhfof47401,axiom,(
    cell67 != cell40 )).

fof(tlhfof47402,axiom,(
    cell67 != cell39 )).

fof(tlhfof47403,axiom,(
    cell67 != cell38 )).

fof(tlhfof47404,axiom,(
    cell67 != cell37 )).

fof(tlhfof47405,axiom,(
    cell67 != cell36 )).

fof(tlhfof47406,axiom,(
    cell67 != cell35 )).

fof(tlhfof47407,axiom,(
    cell67 != cell34 )).

fof(tlhfof47408,axiom,(
    cell67 != cell33 )).

fof(tlhfof47409,axiom,(
    cell67 != cell31 )).

fof(tlhfof47410,axiom,(
    cell67 != cell32 )).

fof(tlhfof47411,axiom,(
    cell67 != cell30 )).

fof(tlhfof47412,axiom,(
    cell67 != cell29 )).

fof(tlhfof47413,axiom,(
    cell67 != cell28 )).

fof(tlhfof47414,axiom,(
    cell67 != cell27 )).

fof(tlhfof47415,axiom,(
    cell67 != cell26 )).

fof(tlhfof47416,axiom,(
    cell67 != cell25 )).

fof(tlhfof47417,axiom,(
    cell67 != cell24 )).

fof(tlhfof47418,axiom,(
    cell67 != cell23 )).

fof(tlhfof47419,axiom,(
    cell67 != cell21 )).

fof(tlhfof47420,axiom,(
    cell67 != cell20 )).

fof(tlhfof47421,axiom,(
    cell67 != cell19 )).

fof(tlhfof47422,axiom,(
    cell67 != cell18 )).

fof(tlhfof47423,axiom,(
    cell67 != cell17 )).

fof(tlhfof47424,axiom,(
    cell67 != cell16 )).

fof(tlhfof47425,axiom,(
    cell67 != cell15 )).

fof(tlhfof47426,axiom,(
    cell67 != cell14 )).

fof(tlhfof47427,axiom,(
    cell67 != cell10 )).

fof(tlhfof47428,axiom,(
    cell67 != cell9 )).

fof(tlhfof47429,axiom,(
    cell67 != cell8 )).

fof(tlhfof47430,axiom,(
    cell67 != cell7 )).

fof(tlhfof47431,axiom,(
    cell67 != cell6 )).

fof(tlhfof47432,axiom,(
    cell67 != cell5 )).

fof(tlhfof47433,axiom,(
    cell67 != cell4 )).

fof(tlhfof47434,axiom,(
    cell67 != cell3 )).

fof(tlhfof47435,axiom,(
    cell67 != cell1 )).

fof(tlhfof47436,axiom,(
    cell67 != cell89 )).

fof(tlhfof47437,axiom,(
    cell67 != cell88 )).

fof(tlhfof47438,axiom,(
    cell67 != cell79 )).

fof(tlhfof47439,axiom,(
    cell67 != cell90 )).

fof(tlhfof47440,axiom,(
    cell67 != cell99 )).

fof(tlhfof47441,axiom,(
    cell67 != cell12 )).

fof(tlhfof47442,axiom,(
    cell67 != cell11 )).

fof(tlhfof47443,axiom,(
    cell67 != cell2 )).

fof(tlhfof47444,axiom,(
    cell67 != cell13 )).

fof(tlhfof47445,axiom,(
    cell67 != cell22 )).

fof(tlhfof47446,axiom,(
    cell66 != cell65 )).

fof(tlhfof47447,axiom,(
    cell66 != cell64 )).

fof(tlhfof47448,axiom,(
    cell66 != cell63 )).

fof(tlhfof47449,axiom,(
    cell66 != cell61 )).

fof(tlhfof47450,axiom,(
    cell66 != cell62 )).

fof(tlhfof47451,axiom,(
    cell66 != cell60 )).

fof(tlhfof47452,axiom,(
    cell66 != cell59 )).

fof(tlhfof47453,axiom,(
    cell66 != cell58 )).

fof(tlhfof47454,axiom,(
    cell66 != cell57 )).

fof(tlhfof47455,axiom,(
    cell66 != cell56 )).

fof(tlhfof47456,axiom,(
    cell66 != cell55 )).

fof(tlhfof47457,axiom,(
    cell66 != cell54 )).

fof(tlhfof47458,axiom,(
    cell66 != cell53 )).

fof(tlhfof47459,axiom,(
    cell66 != cell51 )).

fof(tlhfof47460,axiom,(
    cell66 != cell52 )).

fof(tlhfof47461,axiom,(
    cell66 != cell50 )).

fof(tlhfof47462,axiom,(
    cell66 != cell49 )).

fof(tlhfof47463,axiom,(
    cell66 != cell48 )).

fof(tlhfof47464,axiom,(
    cell66 != cell47 )).

fof(tlhfof47465,axiom,(
    cell66 != cell46 )).

fof(tlhfof47466,axiom,(
    cell66 != cell45 )).

fof(tlhfof47467,axiom,(
    cell66 != cell44 )).

fof(tlhfof47468,axiom,(
    cell66 != cell43 )).

fof(tlhfof47469,axiom,(
    cell66 != cell41 )).

fof(tlhfof47470,axiom,(
    cell66 != cell42 )).

fof(tlhfof47471,axiom,(
    cell66 != cell40 )).

fof(tlhfof47472,axiom,(
    cell66 != cell39 )).

fof(tlhfof47473,axiom,(
    cell66 != cell38 )).

fof(tlhfof47474,axiom,(
    cell66 != cell37 )).

fof(tlhfof47475,axiom,(
    cell66 != cell36 )).

fof(tlhfof47476,axiom,(
    cell66 != cell35 )).

fof(tlhfof47477,axiom,(
    cell66 != cell34 )).

fof(tlhfof47478,axiom,(
    cell66 != cell33 )).

fof(tlhfof47479,axiom,(
    cell66 != cell31 )).

fof(tlhfof47480,axiom,(
    cell66 != cell32 )).

fof(tlhfof47481,axiom,(
    cell66 != cell30 )).

fof(tlhfof47482,axiom,(
    cell66 != cell29 )).

fof(tlhfof47483,axiom,(
    cell66 != cell28 )).

fof(tlhfof47484,axiom,(
    cell66 != cell27 )).

fof(tlhfof47485,axiom,(
    cell66 != cell26 )).

fof(tlhfof47486,axiom,(
    cell66 != cell25 )).

fof(tlhfof47487,axiom,(
    cell66 != cell24 )).

fof(tlhfof47488,axiom,(
    cell66 != cell23 )).

fof(tlhfof47489,axiom,(
    cell66 != cell21 )).

fof(tlhfof47490,axiom,(
    cell66 != cell20 )).

fof(tlhfof47491,axiom,(
    cell66 != cell19 )).

fof(tlhfof47492,axiom,(
    cell66 != cell18 )).

fof(tlhfof47493,axiom,(
    cell66 != cell17 )).

fof(tlhfof47494,axiom,(
    cell66 != cell16 )).

fof(tlhfof47495,axiom,(
    cell66 != cell15 )).

fof(tlhfof47496,axiom,(
    cell66 != cell14 )).

fof(tlhfof47497,axiom,(
    cell66 != cell10 )).

fof(tlhfof47498,axiom,(
    cell66 != cell9 )).

fof(tlhfof47499,axiom,(
    cell66 != cell8 )).

fof(tlhfof47500,axiom,(
    cell66 != cell7 )).

fof(tlhfof47501,axiom,(
    cell66 != cell6 )).

fof(tlhfof47502,axiom,(
    cell66 != cell5 )).

fof(tlhfof47503,axiom,(
    cell66 != cell4 )).

fof(tlhfof47504,axiom,(
    cell66 != cell3 )).

fof(tlhfof47505,axiom,(
    cell66 != cell1 )).

fof(tlhfof47506,axiom,(
    cell66 != cell89 )).

fof(tlhfof47507,axiom,(
    cell66 != cell88 )).

fof(tlhfof47508,axiom,(
    cell66 != cell79 )).

fof(tlhfof47509,axiom,(
    cell66 != cell90 )).

fof(tlhfof47510,axiom,(
    cell66 != cell99 )).

fof(tlhfof47511,axiom,(
    cell66 != cell12 )).

fof(tlhfof47512,axiom,(
    cell66 != cell11 )).

fof(tlhfof47513,axiom,(
    cell66 != cell2 )).

fof(tlhfof47514,axiom,(
    cell66 != cell13 )).

fof(tlhfof47515,axiom,(
    cell66 != cell22 )).

fof(tlhfof47516,axiom,(
    cell65 != cell64 )).

fof(tlhfof47517,axiom,(
    cell65 != cell63 )).

fof(tlhfof47518,axiom,(
    cell65 != cell61 )).

fof(tlhfof47519,axiom,(
    cell65 != cell62 )).

fof(tlhfof47520,axiom,(
    cell65 != cell60 )).

fof(tlhfof47521,axiom,(
    cell65 != cell59 )).

fof(tlhfof47522,axiom,(
    cell65 != cell58 )).

fof(tlhfof47523,axiom,(
    cell65 != cell57 )).

fof(tlhfof47524,axiom,(
    cell65 != cell56 )).

fof(tlhfof47525,axiom,(
    cell65 != cell55 )).

fof(tlhfof47526,axiom,(
    cell65 != cell54 )).

fof(tlhfof47527,axiom,(
    cell65 != cell53 )).

fof(tlhfof47528,axiom,(
    cell65 != cell51 )).

fof(tlhfof47529,axiom,(
    cell65 != cell52 )).

fof(tlhfof47530,axiom,(
    cell65 != cell50 )).

fof(tlhfof47531,axiom,(
    cell65 != cell49 )).

fof(tlhfof47532,axiom,(
    cell65 != cell48 )).

fof(tlhfof47533,axiom,(
    cell65 != cell47 )).

fof(tlhfof47534,axiom,(
    cell65 != cell46 )).

fof(tlhfof47535,axiom,(
    cell65 != cell45 )).

fof(tlhfof47536,axiom,(
    cell65 != cell44 )).

fof(tlhfof47537,axiom,(
    cell65 != cell43 )).

fof(tlhfof47538,axiom,(
    cell65 != cell41 )).

fof(tlhfof47539,axiom,(
    cell65 != cell42 )).

fof(tlhfof47540,axiom,(
    cell65 != cell40 )).

fof(tlhfof47541,axiom,(
    cell65 != cell39 )).

fof(tlhfof47542,axiom,(
    cell65 != cell38 )).

fof(tlhfof47543,axiom,(
    cell65 != cell37 )).

fof(tlhfof47544,axiom,(
    cell65 != cell36 )).

fof(tlhfof47545,axiom,(
    cell65 != cell35 )).

fof(tlhfof47546,axiom,(
    cell65 != cell34 )).

fof(tlhfof47547,axiom,(
    cell65 != cell33 )).

fof(tlhfof47548,axiom,(
    cell65 != cell31 )).

fof(tlhfof47549,axiom,(
    cell65 != cell32 )).

fof(tlhfof47550,axiom,(
    cell65 != cell30 )).

fof(tlhfof47551,axiom,(
    cell65 != cell29 )).

fof(tlhfof47552,axiom,(
    cell65 != cell28 )).

fof(tlhfof47553,axiom,(
    cell65 != cell27 )).

fof(tlhfof47554,axiom,(
    cell65 != cell26 )).

fof(tlhfof47555,axiom,(
    cell65 != cell25 )).

fof(tlhfof47556,axiom,(
    cell65 != cell24 )).

fof(tlhfof47557,axiom,(
    cell65 != cell23 )).

fof(tlhfof47558,axiom,(
    cell65 != cell21 )).

fof(tlhfof47559,axiom,(
    cell65 != cell20 )).

fof(tlhfof47560,axiom,(
    cell65 != cell19 )).

fof(tlhfof47561,axiom,(
    cell65 != cell18 )).

fof(tlhfof47562,axiom,(
    cell65 != cell17 )).

fof(tlhfof47563,axiom,(
    cell65 != cell16 )).

fof(tlhfof47564,axiom,(
    cell65 != cell15 )).

fof(tlhfof47565,axiom,(
    cell65 != cell14 )).

fof(tlhfof47566,axiom,(
    cell65 != cell10 )).

fof(tlhfof47567,axiom,(
    cell65 != cell9 )).

fof(tlhfof47568,axiom,(
    cell65 != cell8 )).

fof(tlhfof47569,axiom,(
    cell65 != cell7 )).

fof(tlhfof47570,axiom,(
    cell65 != cell6 )).

fof(tlhfof47571,axiom,(
    cell65 != cell5 )).

fof(tlhfof47572,axiom,(
    cell65 != cell4 )).

fof(tlhfof47573,axiom,(
    cell65 != cell3 )).

fof(tlhfof47574,axiom,(
    cell65 != cell1 )).

fof(tlhfof47575,axiom,(
    cell65 != cell89 )).

fof(tlhfof47576,axiom,(
    cell65 != cell88 )).

fof(tlhfof47577,axiom,(
    cell65 != cell79 )).

fof(tlhfof47578,axiom,(
    cell65 != cell90 )).

fof(tlhfof47579,axiom,(
    cell65 != cell99 )).

fof(tlhfof47580,axiom,(
    cell65 != cell12 )).

fof(tlhfof47581,axiom,(
    cell65 != cell11 )).

fof(tlhfof47582,axiom,(
    cell65 != cell2 )).

fof(tlhfof47583,axiom,(
    cell65 != cell13 )).

fof(tlhfof47584,axiom,(
    cell65 != cell22 )).

fof(tlhfof47585,axiom,(
    cell64 != cell63 )).

fof(tlhfof47586,axiom,(
    cell64 != cell61 )).

fof(tlhfof47587,axiom,(
    cell64 != cell62 )).

fof(tlhfof47588,axiom,(
    cell64 != cell60 )).

fof(tlhfof47589,axiom,(
    cell64 != cell59 )).

fof(tlhfof47590,axiom,(
    cell64 != cell58 )).

fof(tlhfof47591,axiom,(
    cell64 != cell57 )).

fof(tlhfof47592,axiom,(
    cell64 != cell56 )).

fof(tlhfof47593,axiom,(
    cell64 != cell55 )).

fof(tlhfof47594,axiom,(
    cell64 != cell54 )).

fof(tlhfof47595,axiom,(
    cell64 != cell53 )).

fof(tlhfof47596,axiom,(
    cell64 != cell51 )).

fof(tlhfof47597,axiom,(
    cell64 != cell52 )).

fof(tlhfof47598,axiom,(
    cell64 != cell50 )).

fof(tlhfof47599,axiom,(
    cell64 != cell49 )).

fof(tlhfof47600,axiom,(
    cell64 != cell48 )).

fof(tlhfof47601,axiom,(
    cell64 != cell47 )).

fof(tlhfof47602,axiom,(
    cell64 != cell46 )).

fof(tlhfof47603,axiom,(
    cell64 != cell45 )).

fof(tlhfof47604,axiom,(
    cell64 != cell44 )).

fof(tlhfof47605,axiom,(
    cell64 != cell43 )).

fof(tlhfof47606,axiom,(
    cell64 != cell41 )).

fof(tlhfof47607,axiom,(
    cell64 != cell42 )).

fof(tlhfof47608,axiom,(
    cell64 != cell40 )).

fof(tlhfof47609,axiom,(
    cell64 != cell39 )).

fof(tlhfof47610,axiom,(
    cell64 != cell38 )).

fof(tlhfof47611,axiom,(
    cell64 != cell37 )).

fof(tlhfof47612,axiom,(
    cell64 != cell36 )).

fof(tlhfof47613,axiom,(
    cell64 != cell35 )).

fof(tlhfof47614,axiom,(
    cell64 != cell34 )).

fof(tlhfof47615,axiom,(
    cell64 != cell33 )).

fof(tlhfof47616,axiom,(
    cell64 != cell31 )).

fof(tlhfof47617,axiom,(
    cell64 != cell32 )).

fof(tlhfof47618,axiom,(
    cell64 != cell30 )).

fof(tlhfof47619,axiom,(
    cell64 != cell29 )).

fof(tlhfof47620,axiom,(
    cell64 != cell28 )).

fof(tlhfof47621,axiom,(
    cell64 != cell27 )).

fof(tlhfof47622,axiom,(
    cell64 != cell26 )).

fof(tlhfof47623,axiom,(
    cell64 != cell25 )).

fof(tlhfof47624,axiom,(
    cell64 != cell24 )).

fof(tlhfof47625,axiom,(
    cell64 != cell23 )).

fof(tlhfof47626,axiom,(
    cell64 != cell21 )).

fof(tlhfof47627,axiom,(
    cell64 != cell20 )).

fof(tlhfof47628,axiom,(
    cell64 != cell19 )).

fof(tlhfof47629,axiom,(
    cell64 != cell18 )).

fof(tlhfof47630,axiom,(
    cell64 != cell17 )).

fof(tlhfof47631,axiom,(
    cell64 != cell16 )).

fof(tlhfof47632,axiom,(
    cell64 != cell15 )).

fof(tlhfof47633,axiom,(
    cell64 != cell14 )).

fof(tlhfof47634,axiom,(
    cell64 != cell10 )).

fof(tlhfof47635,axiom,(
    cell64 != cell9 )).

fof(tlhfof47636,axiom,(
    cell64 != cell8 )).

fof(tlhfof47637,axiom,(
    cell64 != cell7 )).

fof(tlhfof47638,axiom,(
    cell64 != cell6 )).

fof(tlhfof47639,axiom,(
    cell64 != cell5 )).

fof(tlhfof47640,axiom,(
    cell64 != cell4 )).

fof(tlhfof47641,axiom,(
    cell64 != cell3 )).

fof(tlhfof47642,axiom,(
    cell64 != cell1 )).

fof(tlhfof47643,axiom,(
    cell64 != cell89 )).

fof(tlhfof47644,axiom,(
    cell64 != cell88 )).

fof(tlhfof47645,axiom,(
    cell64 != cell79 )).

fof(tlhfof47646,axiom,(
    cell64 != cell90 )).

fof(tlhfof47647,axiom,(
    cell64 != cell99 )).

fof(tlhfof47648,axiom,(
    cell64 != cell12 )).

fof(tlhfof47649,axiom,(
    cell64 != cell11 )).

fof(tlhfof47650,axiom,(
    cell64 != cell2 )).

fof(tlhfof47651,axiom,(
    cell64 != cell13 )).

fof(tlhfof47652,axiom,(
    cell64 != cell22 )).

fof(tlhfof47653,axiom,(
    cell63 != cell61 )).

fof(tlhfof47654,axiom,(
    cell63 != cell62 )).

fof(tlhfof47655,axiom,(
    cell63 != cell60 )).

fof(tlhfof47656,axiom,(
    cell63 != cell59 )).

fof(tlhfof47657,axiom,(
    cell63 != cell58 )).

fof(tlhfof47658,axiom,(
    cell63 != cell57 )).

fof(tlhfof47659,axiom,(
    cell63 != cell56 )).

fof(tlhfof47660,axiom,(
    cell63 != cell55 )).

fof(tlhfof47661,axiom,(
    cell63 != cell54 )).

fof(tlhfof47662,axiom,(
    cell63 != cell53 )).

fof(tlhfof47663,axiom,(
    cell63 != cell51 )).

fof(tlhfof47664,axiom,(
    cell63 != cell52 )).

fof(tlhfof47665,axiom,(
    cell63 != cell50 )).

fof(tlhfof47666,axiom,(
    cell63 != cell49 )).

fof(tlhfof47667,axiom,(
    cell63 != cell48 )).

fof(tlhfof47668,axiom,(
    cell63 != cell47 )).

fof(tlhfof47669,axiom,(
    cell63 != cell46 )).

fof(tlhfof47670,axiom,(
    cell63 != cell45 )).

fof(tlhfof47671,axiom,(
    cell63 != cell44 )).

fof(tlhfof47672,axiom,(
    cell63 != cell43 )).

fof(tlhfof47673,axiom,(
    cell63 != cell41 )).

fof(tlhfof47674,axiom,(
    cell63 != cell42 )).

fof(tlhfof47675,axiom,(
    cell63 != cell40 )).

fof(tlhfof47676,axiom,(
    cell63 != cell39 )).

fof(tlhfof47677,axiom,(
    cell63 != cell38 )).

fof(tlhfof47678,axiom,(
    cell63 != cell37 )).

fof(tlhfof47679,axiom,(
    cell63 != cell36 )).

fof(tlhfof47680,axiom,(
    cell63 != cell35 )).

fof(tlhfof47681,axiom,(
    cell63 != cell34 )).

fof(tlhfof47682,axiom,(
    cell63 != cell33 )).

fof(tlhfof47683,axiom,(
    cell63 != cell31 )).

fof(tlhfof47684,axiom,(
    cell63 != cell32 )).

fof(tlhfof47685,axiom,(
    cell63 != cell30 )).

fof(tlhfof47686,axiom,(
    cell63 != cell29 )).

fof(tlhfof47687,axiom,(
    cell63 != cell28 )).

fof(tlhfof47688,axiom,(
    cell63 != cell27 )).

fof(tlhfof47689,axiom,(
    cell63 != cell26 )).

fof(tlhfof47690,axiom,(
    cell63 != cell25 )).

fof(tlhfof47691,axiom,(
    cell63 != cell24 )).

fof(tlhfof47692,axiom,(
    cell63 != cell23 )).

fof(tlhfof47693,axiom,(
    cell63 != cell21 )).

fof(tlhfof47694,axiom,(
    cell63 != cell20 )).

fof(tlhfof47695,axiom,(
    cell63 != cell19 )).

fof(tlhfof47696,axiom,(
    cell63 != cell18 )).

fof(tlhfof47697,axiom,(
    cell63 != cell17 )).

fof(tlhfof47698,axiom,(
    cell63 != cell16 )).

fof(tlhfof47699,axiom,(
    cell63 != cell15 )).

fof(tlhfof47700,axiom,(
    cell63 != cell14 )).

fof(tlhfof47701,axiom,(
    cell63 != cell10 )).

fof(tlhfof47702,axiom,(
    cell63 != cell9 )).

fof(tlhfof47703,axiom,(
    cell63 != cell8 )).

fof(tlhfof47704,axiom,(
    cell63 != cell7 )).

fof(tlhfof47705,axiom,(
    cell63 != cell6 )).

fof(tlhfof47706,axiom,(
    cell63 != cell5 )).

fof(tlhfof47707,axiom,(
    cell63 != cell4 )).

fof(tlhfof47708,axiom,(
    cell63 != cell3 )).

fof(tlhfof47709,axiom,(
    cell63 != cell1 )).

fof(tlhfof47710,axiom,(
    cell63 != cell89 )).

fof(tlhfof47711,axiom,(
    cell63 != cell88 )).

fof(tlhfof47712,axiom,(
    cell63 != cell79 )).

fof(tlhfof47713,axiom,(
    cell63 != cell90 )).

fof(tlhfof47714,axiom,(
    cell63 != cell99 )).

fof(tlhfof47715,axiom,(
    cell63 != cell12 )).

fof(tlhfof47716,axiom,(
    cell63 != cell11 )).

fof(tlhfof47717,axiom,(
    cell63 != cell2 )).

fof(tlhfof47718,axiom,(
    cell63 != cell13 )).

fof(tlhfof47719,axiom,(
    cell63 != cell22 )).

fof(tlhfof47720,axiom,(
    cell61 != cell62 )).

fof(tlhfof47721,axiom,(
    cell61 != cell60 )).

fof(tlhfof47722,axiom,(
    cell61 != cell59 )).

fof(tlhfof47723,axiom,(
    cell61 != cell58 )).

fof(tlhfof47724,axiom,(
    cell61 != cell57 )).

fof(tlhfof47725,axiom,(
    cell61 != cell56 )).

fof(tlhfof47726,axiom,(
    cell61 != cell55 )).

fof(tlhfof47727,axiom,(
    cell61 != cell54 )).

fof(tlhfof47728,axiom,(
    cell61 != cell53 )).

fof(tlhfof47729,axiom,(
    cell61 != cell51 )).

fof(tlhfof47730,axiom,(
    cell61 != cell52 )).

fof(tlhfof47731,axiom,(
    cell61 != cell50 )).

fof(tlhfof47732,axiom,(
    cell61 != cell49 )).

fof(tlhfof47733,axiom,(
    cell61 != cell48 )).

fof(tlhfof47734,axiom,(
    cell61 != cell47 )).

fof(tlhfof47735,axiom,(
    cell61 != cell46 )).

fof(tlhfof47736,axiom,(
    cell61 != cell45 )).

fof(tlhfof47737,axiom,(
    cell61 != cell44 )).

fof(tlhfof47738,axiom,(
    cell61 != cell43 )).

fof(tlhfof47739,axiom,(
    cell61 != cell41 )).

fof(tlhfof47740,axiom,(
    cell61 != cell42 )).

fof(tlhfof47741,axiom,(
    cell61 != cell40 )).

fof(tlhfof47742,axiom,(
    cell61 != cell39 )).

fof(tlhfof47743,axiom,(
    cell61 != cell38 )).

fof(tlhfof47744,axiom,(
    cell61 != cell37 )).

fof(tlhfof47745,axiom,(
    cell61 != cell36 )).

fof(tlhfof47746,axiom,(
    cell61 != cell35 )).

fof(tlhfof47747,axiom,(
    cell61 != cell34 )).

fof(tlhfof47748,axiom,(
    cell61 != cell33 )).

fof(tlhfof47749,axiom,(
    cell61 != cell31 )).

fof(tlhfof47750,axiom,(
    cell61 != cell32 )).

fof(tlhfof47751,axiom,(
    cell61 != cell30 )).

fof(tlhfof47752,axiom,(
    cell61 != cell29 )).

fof(tlhfof47753,axiom,(
    cell61 != cell28 )).

fof(tlhfof47754,axiom,(
    cell61 != cell27 )).

fof(tlhfof47755,axiom,(
    cell61 != cell26 )).

fof(tlhfof47756,axiom,(
    cell61 != cell25 )).

fof(tlhfof47757,axiom,(
    cell61 != cell24 )).

fof(tlhfof47758,axiom,(
    cell61 != cell23 )).

fof(tlhfof47759,axiom,(
    cell61 != cell21 )).

fof(tlhfof47760,axiom,(
    cell61 != cell20 )).

fof(tlhfof47761,axiom,(
    cell61 != cell19 )).

fof(tlhfof47762,axiom,(
    cell61 != cell18 )).

fof(tlhfof47763,axiom,(
    cell61 != cell17 )).

fof(tlhfof47764,axiom,(
    cell61 != cell16 )).

fof(tlhfof47765,axiom,(
    cell61 != cell15 )).

fof(tlhfof47766,axiom,(
    cell61 != cell14 )).

fof(tlhfof47767,axiom,(
    cell61 != cell10 )).

fof(tlhfof47768,axiom,(
    cell61 != cell9 )).

fof(tlhfof47769,axiom,(
    cell61 != cell8 )).

fof(tlhfof47770,axiom,(
    cell61 != cell7 )).

fof(tlhfof47771,axiom,(
    cell61 != cell6 )).

fof(tlhfof47772,axiom,(
    cell61 != cell5 )).

fof(tlhfof47773,axiom,(
    cell61 != cell4 )).

fof(tlhfof47774,axiom,(
    cell61 != cell3 )).

fof(tlhfof47775,axiom,(
    cell61 != cell1 )).

fof(tlhfof47776,axiom,(
    cell61 != cell89 )).

fof(tlhfof47777,axiom,(
    cell61 != cell88 )).

fof(tlhfof47778,axiom,(
    cell61 != cell79 )).

fof(tlhfof47779,axiom,(
    cell61 != cell90 )).

fof(tlhfof47780,axiom,(
    cell61 != cell99 )).

fof(tlhfof47781,axiom,(
    cell61 != cell12 )).

fof(tlhfof47782,axiom,(
    cell61 != cell11 )).

fof(tlhfof47783,axiom,(
    cell61 != cell2 )).

fof(tlhfof47784,axiom,(
    cell61 != cell13 )).

fof(tlhfof47785,axiom,(
    cell61 != cell22 )).

fof(tlhfof47786,axiom,(
    cell62 != cell60 )).

fof(tlhfof47787,axiom,(
    cell62 != cell59 )).

fof(tlhfof47788,axiom,(
    cell62 != cell58 )).

fof(tlhfof47789,axiom,(
    cell62 != cell57 )).

fof(tlhfof47790,axiom,(
    cell62 != cell56 )).

fof(tlhfof47791,axiom,(
    cell62 != cell55 )).

fof(tlhfof47792,axiom,(
    cell62 != cell54 )).

fof(tlhfof47793,axiom,(
    cell62 != cell53 )).

fof(tlhfof47794,axiom,(
    cell62 != cell51 )).

fof(tlhfof47795,axiom,(
    cell62 != cell52 )).

fof(tlhfof47796,axiom,(
    cell62 != cell50 )).

fof(tlhfof47797,axiom,(
    cell62 != cell49 )).

fof(tlhfof47798,axiom,(
    cell62 != cell48 )).

fof(tlhfof47799,axiom,(
    cell62 != cell47 )).

fof(tlhfof47800,axiom,(
    cell62 != cell46 )).

fof(tlhfof47801,axiom,(
    cell62 != cell45 )).

fof(tlhfof47802,axiom,(
    cell62 != cell44 )).

fof(tlhfof47803,axiom,(
    cell62 != cell43 )).

fof(tlhfof47804,axiom,(
    cell62 != cell41 )).

fof(tlhfof47805,axiom,(
    cell62 != cell42 )).

fof(tlhfof47806,axiom,(
    cell62 != cell40 )).

fof(tlhfof47807,axiom,(
    cell62 != cell39 )).

fof(tlhfof47808,axiom,(
    cell62 != cell38 )).

fof(tlhfof47809,axiom,(
    cell62 != cell37 )).

fof(tlhfof47810,axiom,(
    cell62 != cell36 )).

fof(tlhfof47811,axiom,(
    cell62 != cell35 )).

fof(tlhfof47812,axiom,(
    cell62 != cell34 )).

fof(tlhfof47813,axiom,(
    cell62 != cell33 )).

fof(tlhfof47814,axiom,(
    cell62 != cell31 )).

fof(tlhfof47815,axiom,(
    cell62 != cell32 )).

fof(tlhfof47816,axiom,(
    cell62 != cell30 )).

fof(tlhfof47817,axiom,(
    cell62 != cell29 )).

fof(tlhfof47818,axiom,(
    cell62 != cell28 )).

fof(tlhfof47819,axiom,(
    cell62 != cell27 )).

fof(tlhfof47820,axiom,(
    cell62 != cell26 )).

fof(tlhfof47821,axiom,(
    cell62 != cell25 )).

fof(tlhfof47822,axiom,(
    cell62 != cell24 )).

fof(tlhfof47823,axiom,(
    cell62 != cell23 )).

fof(tlhfof47824,axiom,(
    cell62 != cell21 )).

fof(tlhfof47825,axiom,(
    cell62 != cell20 )).

fof(tlhfof47826,axiom,(
    cell62 != cell19 )).

fof(tlhfof47827,axiom,(
    cell62 != cell18 )).

fof(tlhfof47828,axiom,(
    cell62 != cell17 )).

fof(tlhfof47829,axiom,(
    cell62 != cell16 )).

fof(tlhfof47830,axiom,(
    cell62 != cell15 )).

fof(tlhfof47831,axiom,(
    cell62 != cell14 )).

fof(tlhfof47832,axiom,(
    cell62 != cell10 )).

fof(tlhfof47833,axiom,(
    cell62 != cell9 )).

fof(tlhfof47834,axiom,(
    cell62 != cell8 )).

fof(tlhfof47835,axiom,(
    cell62 != cell7 )).

fof(tlhfof47836,axiom,(
    cell62 != cell6 )).

fof(tlhfof47837,axiom,(
    cell62 != cell5 )).

fof(tlhfof47838,axiom,(
    cell62 != cell4 )).

fof(tlhfof47839,axiom,(
    cell62 != cell3 )).

fof(tlhfof47840,axiom,(
    cell62 != cell1 )).

fof(tlhfof47841,axiom,(
    cell62 != cell89 )).

fof(tlhfof47842,axiom,(
    cell62 != cell88 )).

fof(tlhfof47843,axiom,(
    cell62 != cell79 )).

fof(tlhfof47844,axiom,(
    cell62 != cell90 )).

fof(tlhfof47845,axiom,(
    cell62 != cell99 )).

fof(tlhfof47846,axiom,(
    cell62 != cell12 )).

fof(tlhfof47847,axiom,(
    cell62 != cell11 )).

fof(tlhfof47848,axiom,(
    cell62 != cell2 )).

fof(tlhfof47849,axiom,(
    cell62 != cell13 )).

fof(tlhfof47850,axiom,(
    cell62 != cell22 )).

fof(tlhfof47851,axiom,(
    cell60 != cell59 )).

fof(tlhfof47852,axiom,(
    cell60 != cell58 )).

fof(tlhfof47853,axiom,(
    cell60 != cell57 )).

fof(tlhfof47854,axiom,(
    cell60 != cell56 )).

fof(tlhfof47855,axiom,(
    cell60 != cell55 )).

fof(tlhfof47856,axiom,(
    cell60 != cell54 )).

fof(tlhfof47857,axiom,(
    cell60 != cell53 )).

fof(tlhfof47858,axiom,(
    cell60 != cell51 )).

fof(tlhfof47859,axiom,(
    cell60 != cell52 )).

fof(tlhfof47860,axiom,(
    cell60 != cell50 )).

fof(tlhfof47861,axiom,(
    cell60 != cell49 )).

fof(tlhfof47862,axiom,(
    cell60 != cell48 )).

fof(tlhfof47863,axiom,(
    cell60 != cell47 )).

fof(tlhfof47864,axiom,(
    cell60 != cell46 )).

fof(tlhfof47865,axiom,(
    cell60 != cell45 )).

fof(tlhfof47866,axiom,(
    cell60 != cell44 )).

fof(tlhfof47867,axiom,(
    cell60 != cell43 )).

fof(tlhfof47868,axiom,(
    cell60 != cell41 )).

fof(tlhfof47869,axiom,(
    cell60 != cell42 )).

fof(tlhfof47870,axiom,(
    cell60 != cell40 )).

fof(tlhfof47871,axiom,(
    cell60 != cell39 )).

fof(tlhfof47872,axiom,(
    cell60 != cell38 )).

fof(tlhfof47873,axiom,(
    cell60 != cell37 )).

fof(tlhfof47874,axiom,(
    cell60 != cell36 )).

fof(tlhfof47875,axiom,(
    cell60 != cell35 )).

fof(tlhfof47876,axiom,(
    cell60 != cell34 )).

fof(tlhfof47877,axiom,(
    cell60 != cell33 )).

fof(tlhfof47878,axiom,(
    cell60 != cell31 )).

fof(tlhfof47879,axiom,(
    cell60 != cell32 )).

fof(tlhfof47880,axiom,(
    cell60 != cell30 )).

fof(tlhfof47881,axiom,(
    cell60 != cell29 )).

fof(tlhfof47882,axiom,(
    cell60 != cell28 )).

fof(tlhfof47883,axiom,(
    cell60 != cell27 )).

fof(tlhfof47884,axiom,(
    cell60 != cell26 )).

fof(tlhfof47885,axiom,(
    cell60 != cell25 )).

fof(tlhfof47886,axiom,(
    cell60 != cell24 )).

fof(tlhfof47887,axiom,(
    cell60 != cell23 )).

fof(tlhfof47888,axiom,(
    cell60 != cell21 )).

fof(tlhfof47889,axiom,(
    cell60 != cell20 )).

fof(tlhfof47890,axiom,(
    cell60 != cell19 )).

fof(tlhfof47891,axiom,(
    cell60 != cell18 )).

fof(tlhfof47892,axiom,(
    cell60 != cell17 )).

fof(tlhfof47893,axiom,(
    cell60 != cell16 )).

fof(tlhfof47894,axiom,(
    cell60 != cell15 )).

fof(tlhfof47895,axiom,(
    cell60 != cell14 )).

fof(tlhfof47896,axiom,(
    cell60 != cell10 )).

fof(tlhfof47897,axiom,(
    cell60 != cell9 )).

fof(tlhfof47898,axiom,(
    cell60 != cell8 )).

fof(tlhfof47899,axiom,(
    cell60 != cell7 )).

fof(tlhfof47900,axiom,(
    cell60 != cell6 )).

fof(tlhfof47901,axiom,(
    cell60 != cell5 )).

fof(tlhfof47902,axiom,(
    cell60 != cell4 )).

fof(tlhfof47903,axiom,(
    cell60 != cell3 )).

fof(tlhfof47904,axiom,(
    cell60 != cell1 )).

fof(tlhfof47905,axiom,(
    cell60 != cell89 )).

fof(tlhfof47906,axiom,(
    cell60 != cell88 )).

fof(tlhfof47907,axiom,(
    cell60 != cell79 )).

fof(tlhfof47908,axiom,(
    cell60 != cell90 )).

fof(tlhfof47909,axiom,(
    cell60 != cell99 )).

fof(tlhfof47910,axiom,(
    cell60 != cell12 )).

fof(tlhfof47911,axiom,(
    cell60 != cell11 )).

fof(tlhfof47912,axiom,(
    cell60 != cell2 )).

fof(tlhfof47913,axiom,(
    cell60 != cell13 )).

fof(tlhfof47914,axiom,(
    cell60 != cell22 )).

fof(tlhfof47915,axiom,(
    cell59 != cell58 )).

fof(tlhfof47916,axiom,(
    cell59 != cell57 )).

fof(tlhfof47917,axiom,(
    cell59 != cell56 )).

fof(tlhfof47918,axiom,(
    cell59 != cell55 )).

fof(tlhfof47919,axiom,(
    cell59 != cell54 )).

fof(tlhfof47920,axiom,(
    cell59 != cell53 )).

fof(tlhfof47921,axiom,(
    cell59 != cell51 )).

fof(tlhfof47922,axiom,(
    cell59 != cell52 )).

fof(tlhfof47923,axiom,(
    cell59 != cell50 )).

fof(tlhfof47924,axiom,(
    cell59 != cell49 )).

fof(tlhfof47925,axiom,(
    cell59 != cell48 )).

fof(tlhfof47926,axiom,(
    cell59 != cell47 )).

fof(tlhfof47927,axiom,(
    cell59 != cell46 )).

fof(tlhfof47928,axiom,(
    cell59 != cell45 )).

fof(tlhfof47929,axiom,(
    cell59 != cell44 )).

fof(tlhfof47930,axiom,(
    cell59 != cell43 )).

fof(tlhfof47931,axiom,(
    cell59 != cell41 )).

fof(tlhfof47932,axiom,(
    cell59 != cell42 )).

fof(tlhfof47933,axiom,(
    cell59 != cell40 )).

fof(tlhfof47934,axiom,(
    cell59 != cell39 )).

fof(tlhfof47935,axiom,(
    cell59 != cell38 )).

fof(tlhfof47936,axiom,(
    cell59 != cell37 )).

fof(tlhfof47937,axiom,(
    cell59 != cell36 )).

fof(tlhfof47938,axiom,(
    cell59 != cell35 )).

fof(tlhfof47939,axiom,(
    cell59 != cell34 )).

fof(tlhfof47940,axiom,(
    cell59 != cell33 )).

fof(tlhfof47941,axiom,(
    cell59 != cell31 )).

fof(tlhfof47942,axiom,(
    cell59 != cell32 )).

fof(tlhfof47943,axiom,(
    cell59 != cell30 )).

fof(tlhfof47944,axiom,(
    cell59 != cell29 )).

fof(tlhfof47945,axiom,(
    cell59 != cell28 )).

fof(tlhfof47946,axiom,(
    cell59 != cell27 )).

fof(tlhfof47947,axiom,(
    cell59 != cell26 )).

fof(tlhfof47948,axiom,(
    cell59 != cell25 )).

fof(tlhfof47949,axiom,(
    cell59 != cell24 )).

fof(tlhfof47950,axiom,(
    cell59 != cell23 )).

fof(tlhfof47951,axiom,(
    cell59 != cell21 )).

fof(tlhfof47952,axiom,(
    cell59 != cell20 )).

fof(tlhfof47953,axiom,(
    cell59 != cell19 )).

fof(tlhfof47954,axiom,(
    cell59 != cell18 )).

fof(tlhfof47955,axiom,(
    cell59 != cell17 )).

fof(tlhfof47956,axiom,(
    cell59 != cell16 )).

fof(tlhfof47957,axiom,(
    cell59 != cell15 )).

fof(tlhfof47958,axiom,(
    cell59 != cell14 )).

fof(tlhfof47959,axiom,(
    cell59 != cell10 )).

fof(tlhfof47960,axiom,(
    cell59 != cell9 )).

fof(tlhfof47961,axiom,(
    cell59 != cell8 )).

fof(tlhfof47962,axiom,(
    cell59 != cell7 )).

fof(tlhfof47963,axiom,(
    cell59 != cell6 )).

fof(tlhfof47964,axiom,(
    cell59 != cell5 )).

fof(tlhfof47965,axiom,(
    cell59 != cell4 )).

fof(tlhfof47966,axiom,(
    cell59 != cell3 )).

fof(tlhfof47967,axiom,(
    cell59 != cell1 )).

fof(tlhfof47968,axiom,(
    cell59 != cell89 )).

fof(tlhfof47969,axiom,(
    cell59 != cell88 )).

fof(tlhfof47970,axiom,(
    cell59 != cell79 )).

fof(tlhfof47971,axiom,(
    cell59 != cell90 )).

fof(tlhfof47972,axiom,(
    cell59 != cell99 )).

fof(tlhfof47973,axiom,(
    cell59 != cell12 )).

fof(tlhfof47974,axiom,(
    cell59 != cell11 )).

fof(tlhfof47975,axiom,(
    cell59 != cell2 )).

fof(tlhfof47976,axiom,(
    cell59 != cell13 )).

fof(tlhfof47977,axiom,(
    cell59 != cell22 )).

fof(tlhfof47978,axiom,(
    cell58 != cell57 )).

fof(tlhfof47979,axiom,(
    cell58 != cell56 )).

fof(tlhfof47980,axiom,(
    cell58 != cell55 )).

fof(tlhfof47981,axiom,(
    cell58 != cell54 )).

fof(tlhfof47982,axiom,(
    cell58 != cell53 )).

fof(tlhfof47983,axiom,(
    cell58 != cell51 )).

fof(tlhfof47984,axiom,(
    cell58 != cell52 )).

fof(tlhfof47985,axiom,(
    cell58 != cell50 )).

fof(tlhfof47986,axiom,(
    cell58 != cell49 )).

fof(tlhfof47987,axiom,(
    cell58 != cell48 )).

fof(tlhfof47988,axiom,(
    cell58 != cell47 )).

fof(tlhfof47989,axiom,(
    cell58 != cell46 )).

fof(tlhfof47990,axiom,(
    cell58 != cell45 )).

fof(tlhfof47991,axiom,(
    cell58 != cell44 )).

fof(tlhfof47992,axiom,(
    cell58 != cell43 )).

fof(tlhfof47993,axiom,(
    cell58 != cell41 )).

fof(tlhfof47994,axiom,(
    cell58 != cell42 )).

fof(tlhfof47995,axiom,(
    cell58 != cell40 )).

fof(tlhfof47996,axiom,(
    cell58 != cell39 )).

fof(tlhfof47997,axiom,(
    cell58 != cell38 )).

fof(tlhfof47998,axiom,(
    cell58 != cell37 )).

fof(tlhfof47999,axiom,(
    cell58 != cell36 )).

fof(tlhfof48000,axiom,(
    cell58 != cell35 )).

fof(tlhfof48001,axiom,(
    cell58 != cell34 )).

fof(tlhfof48002,axiom,(
    cell58 != cell33 )).

fof(tlhfof48003,axiom,(
    cell58 != cell31 )).

fof(tlhfof48004,axiom,(
    cell58 != cell32 )).

fof(tlhfof48005,axiom,(
    cell58 != cell30 )).

fof(tlhfof48006,axiom,(
    cell58 != cell29 )).

fof(tlhfof48007,axiom,(
    cell58 != cell28 )).

fof(tlhfof48008,axiom,(
    cell58 != cell27 )).

fof(tlhfof48009,axiom,(
    cell58 != cell26 )).

fof(tlhfof48010,axiom,(
    cell58 != cell25 )).

fof(tlhfof48011,axiom,(
    cell58 != cell24 )).

fof(tlhfof48012,axiom,(
    cell58 != cell23 )).

fof(tlhfof48013,axiom,(
    cell58 != cell21 )).

fof(tlhfof48014,axiom,(
    cell58 != cell20 )).

fof(tlhfof48015,axiom,(
    cell58 != cell19 )).

fof(tlhfof48016,axiom,(
    cell58 != cell18 )).

fof(tlhfof48017,axiom,(
    cell58 != cell17 )).

fof(tlhfof48018,axiom,(
    cell58 != cell16 )).

fof(tlhfof48019,axiom,(
    cell58 != cell15 )).

fof(tlhfof48020,axiom,(
    cell58 != cell14 )).

fof(tlhfof48021,axiom,(
    cell58 != cell10 )).

fof(tlhfof48022,axiom,(
    cell58 != cell9 )).

fof(tlhfof48023,axiom,(
    cell58 != cell8 )).

fof(tlhfof48024,axiom,(
    cell58 != cell7 )).

fof(tlhfof48025,axiom,(
    cell58 != cell6 )).

fof(tlhfof48026,axiom,(
    cell58 != cell5 )).

fof(tlhfof48027,axiom,(
    cell58 != cell4 )).

fof(tlhfof48028,axiom,(
    cell58 != cell3 )).

fof(tlhfof48029,axiom,(
    cell58 != cell1 )).

fof(tlhfof48030,axiom,(
    cell58 != cell89 )).

fof(tlhfof48031,axiom,(
    cell58 != cell88 )).

fof(tlhfof48032,axiom,(
    cell58 != cell79 )).

fof(tlhfof48033,axiom,(
    cell58 != cell90 )).

fof(tlhfof48034,axiom,(
    cell58 != cell99 )).

fof(tlhfof48035,axiom,(
    cell58 != cell12 )).

fof(tlhfof48036,axiom,(
    cell58 != cell11 )).

fof(tlhfof48037,axiom,(
    cell58 != cell2 )).

fof(tlhfof48038,axiom,(
    cell58 != cell13 )).

fof(tlhfof48039,axiom,(
    cell58 != cell22 )).

fof(tlhfof48040,axiom,(
    cell57 != cell56 )).

fof(tlhfof48041,axiom,(
    cell57 != cell55 )).

fof(tlhfof48042,axiom,(
    cell57 != cell54 )).

fof(tlhfof48043,axiom,(
    cell57 != cell53 )).

fof(tlhfof48044,axiom,(
    cell57 != cell51 )).

fof(tlhfof48045,axiom,(
    cell57 != cell52 )).

fof(tlhfof48046,axiom,(
    cell57 != cell50 )).

fof(tlhfof48047,axiom,(
    cell57 != cell49 )).

fof(tlhfof48048,axiom,(
    cell57 != cell48 )).

fof(tlhfof48049,axiom,(
    cell57 != cell47 )).

fof(tlhfof48050,axiom,(
    cell57 != cell46 )).

fof(tlhfof48051,axiom,(
    cell57 != cell45 )).

fof(tlhfof48052,axiom,(
    cell57 != cell44 )).

fof(tlhfof48053,axiom,(
    cell57 != cell43 )).

fof(tlhfof48054,axiom,(
    cell57 != cell41 )).

fof(tlhfof48055,axiom,(
    cell57 != cell42 )).

fof(tlhfof48056,axiom,(
    cell57 != cell40 )).

fof(tlhfof48057,axiom,(
    cell57 != cell39 )).

fof(tlhfof48058,axiom,(
    cell57 != cell38 )).

fof(tlhfof48059,axiom,(
    cell57 != cell37 )).

fof(tlhfof48060,axiom,(
    cell57 != cell36 )).

fof(tlhfof48061,axiom,(
    cell57 != cell35 )).

fof(tlhfof48062,axiom,(
    cell57 != cell34 )).

fof(tlhfof48063,axiom,(
    cell57 != cell33 )).

fof(tlhfof48064,axiom,(
    cell57 != cell31 )).

fof(tlhfof48065,axiom,(
    cell57 != cell32 )).

fof(tlhfof48066,axiom,(
    cell57 != cell30 )).

fof(tlhfof48067,axiom,(
    cell57 != cell29 )).

fof(tlhfof48068,axiom,(
    cell57 != cell28 )).

fof(tlhfof48069,axiom,(
    cell57 != cell27 )).

fof(tlhfof48070,axiom,(
    cell57 != cell26 )).

fof(tlhfof48071,axiom,(
    cell57 != cell25 )).

fof(tlhfof48072,axiom,(
    cell57 != cell24 )).

fof(tlhfof48073,axiom,(
    cell57 != cell23 )).

fof(tlhfof48074,axiom,(
    cell57 != cell21 )).

fof(tlhfof48075,axiom,(
    cell57 != cell20 )).

fof(tlhfof48076,axiom,(
    cell57 != cell19 )).

fof(tlhfof48077,axiom,(
    cell57 != cell18 )).

fof(tlhfof48078,axiom,(
    cell57 != cell17 )).

fof(tlhfof48079,axiom,(
    cell57 != cell16 )).

fof(tlhfof48080,axiom,(
    cell57 != cell15 )).

fof(tlhfof48081,axiom,(
    cell57 != cell14 )).

fof(tlhfof48082,axiom,(
    cell57 != cell10 )).

fof(tlhfof48083,axiom,(
    cell57 != cell9 )).

fof(tlhfof48084,axiom,(
    cell57 != cell8 )).

fof(tlhfof48085,axiom,(
    cell57 != cell7 )).

fof(tlhfof48086,axiom,(
    cell57 != cell6 )).

fof(tlhfof48087,axiom,(
    cell57 != cell5 )).

fof(tlhfof48088,axiom,(
    cell57 != cell4 )).

fof(tlhfof48089,axiom,(
    cell57 != cell3 )).

fof(tlhfof48090,axiom,(
    cell57 != cell1 )).

fof(tlhfof48091,axiom,(
    cell57 != cell89 )).

fof(tlhfof48092,axiom,(
    cell57 != cell88 )).

fof(tlhfof48093,axiom,(
    cell57 != cell79 )).

fof(tlhfof48094,axiom,(
    cell57 != cell90 )).

fof(tlhfof48095,axiom,(
    cell57 != cell99 )).

fof(tlhfof48096,axiom,(
    cell57 != cell12 )).

fof(tlhfof48097,axiom,(
    cell57 != cell11 )).

fof(tlhfof48098,axiom,(
    cell57 != cell2 )).

fof(tlhfof48099,axiom,(
    cell57 != cell13 )).

fof(tlhfof48100,axiom,(
    cell57 != cell22 )).

fof(tlhfof48101,axiom,(
    cell56 != cell55 )).

fof(tlhfof48102,axiom,(
    cell56 != cell54 )).

fof(tlhfof48103,axiom,(
    cell56 != cell53 )).

fof(tlhfof48104,axiom,(
    cell56 != cell51 )).

fof(tlhfof48105,axiom,(
    cell56 != cell52 )).

fof(tlhfof48106,axiom,(
    cell56 != cell50 )).

fof(tlhfof48107,axiom,(
    cell56 != cell49 )).

fof(tlhfof48108,axiom,(
    cell56 != cell48 )).

fof(tlhfof48109,axiom,(
    cell56 != cell47 )).

fof(tlhfof48110,axiom,(
    cell56 != cell46 )).

fof(tlhfof48111,axiom,(
    cell56 != cell45 )).

fof(tlhfof48112,axiom,(
    cell56 != cell44 )).

fof(tlhfof48113,axiom,(
    cell56 != cell43 )).

fof(tlhfof48114,axiom,(
    cell56 != cell41 )).

fof(tlhfof48115,axiom,(
    cell56 != cell42 )).

fof(tlhfof48116,axiom,(
    cell56 != cell40 )).

fof(tlhfof48117,axiom,(
    cell56 != cell39 )).

fof(tlhfof48118,axiom,(
    cell56 != cell38 )).

fof(tlhfof48119,axiom,(
    cell56 != cell37 )).

fof(tlhfof48120,axiom,(
    cell56 != cell36 )).

fof(tlhfof48121,axiom,(
    cell56 != cell35 )).

fof(tlhfof48122,axiom,(
    cell56 != cell34 )).

fof(tlhfof48123,axiom,(
    cell56 != cell33 )).

fof(tlhfof48124,axiom,(
    cell56 != cell31 )).

fof(tlhfof48125,axiom,(
    cell56 != cell32 )).

fof(tlhfof48126,axiom,(
    cell56 != cell30 )).

fof(tlhfof48127,axiom,(
    cell56 != cell29 )).

fof(tlhfof48128,axiom,(
    cell56 != cell28 )).

fof(tlhfof48129,axiom,(
    cell56 != cell27 )).

fof(tlhfof48130,axiom,(
    cell56 != cell26 )).

fof(tlhfof48131,axiom,(
    cell56 != cell25 )).

fof(tlhfof48132,axiom,(
    cell56 != cell24 )).

fof(tlhfof48133,axiom,(
    cell56 != cell23 )).

fof(tlhfof48134,axiom,(
    cell56 != cell21 )).

fof(tlhfof48135,axiom,(
    cell56 != cell20 )).

fof(tlhfof48136,axiom,(
    cell56 != cell19 )).

fof(tlhfof48137,axiom,(
    cell56 != cell18 )).

fof(tlhfof48138,axiom,(
    cell56 != cell17 )).

fof(tlhfof48139,axiom,(
    cell56 != cell16 )).

fof(tlhfof48140,axiom,(
    cell56 != cell15 )).

fof(tlhfof48141,axiom,(
    cell56 != cell14 )).

fof(tlhfof48142,axiom,(
    cell56 != cell10 )).

fof(tlhfof48143,axiom,(
    cell56 != cell9 )).

fof(tlhfof48144,axiom,(
    cell56 != cell8 )).

fof(tlhfof48145,axiom,(
    cell56 != cell7 )).

fof(tlhfof48146,axiom,(
    cell56 != cell6 )).

fof(tlhfof48147,axiom,(
    cell56 != cell5 )).

fof(tlhfof48148,axiom,(
    cell56 != cell4 )).

fof(tlhfof48149,axiom,(
    cell56 != cell3 )).

fof(tlhfof48150,axiom,(
    cell56 != cell1 )).

fof(tlhfof48151,axiom,(
    cell56 != cell89 )).

fof(tlhfof48152,axiom,(
    cell56 != cell88 )).

fof(tlhfof48153,axiom,(
    cell56 != cell79 )).

fof(tlhfof48154,axiom,(
    cell56 != cell90 )).

fof(tlhfof48155,axiom,(
    cell56 != cell99 )).

fof(tlhfof48156,axiom,(
    cell56 != cell12 )).

fof(tlhfof48157,axiom,(
    cell56 != cell11 )).

fof(tlhfof48158,axiom,(
    cell56 != cell2 )).

fof(tlhfof48159,axiom,(
    cell56 != cell13 )).

fof(tlhfof48160,axiom,(
    cell56 != cell22 )).

fof(tlhfof48161,axiom,(
    cell55 != cell54 )).

fof(tlhfof48162,axiom,(
    cell55 != cell53 )).

fof(tlhfof48163,axiom,(
    cell55 != cell51 )).

fof(tlhfof48164,axiom,(
    cell55 != cell52 )).

fof(tlhfof48165,axiom,(
    cell55 != cell50 )).

fof(tlhfof48166,axiom,(
    cell55 != cell49 )).

fof(tlhfof48167,axiom,(
    cell55 != cell48 )).

fof(tlhfof48168,axiom,(
    cell55 != cell47 )).

fof(tlhfof48169,axiom,(
    cell55 != cell46 )).

fof(tlhfof48170,axiom,(
    cell55 != cell45 )).

fof(tlhfof48171,axiom,(
    cell55 != cell44 )).

fof(tlhfof48172,axiom,(
    cell55 != cell43 )).

fof(tlhfof48173,axiom,(
    cell55 != cell41 )).

fof(tlhfof48174,axiom,(
    cell55 != cell42 )).

fof(tlhfof48175,axiom,(
    cell55 != cell40 )).

fof(tlhfof48176,axiom,(
    cell55 != cell39 )).

fof(tlhfof48177,axiom,(
    cell55 != cell38 )).

fof(tlhfof48178,axiom,(
    cell55 != cell37 )).

fof(tlhfof48179,axiom,(
    cell55 != cell36 )).

fof(tlhfof48180,axiom,(
    cell55 != cell35 )).

fof(tlhfof48181,axiom,(
    cell55 != cell34 )).

fof(tlhfof48182,axiom,(
    cell55 != cell33 )).

fof(tlhfof48183,axiom,(
    cell55 != cell31 )).

fof(tlhfof48184,axiom,(
    cell55 != cell32 )).

fof(tlhfof48185,axiom,(
    cell55 != cell30 )).

fof(tlhfof48186,axiom,(
    cell55 != cell29 )).

fof(tlhfof48187,axiom,(
    cell55 != cell28 )).

fof(tlhfof48188,axiom,(
    cell55 != cell27 )).

fof(tlhfof48189,axiom,(
    cell55 != cell26 )).

fof(tlhfof48190,axiom,(
    cell55 != cell25 )).

fof(tlhfof48191,axiom,(
    cell55 != cell24 )).

fof(tlhfof48192,axiom,(
    cell55 != cell23 )).

fof(tlhfof48193,axiom,(
    cell55 != cell21 )).

fof(tlhfof48194,axiom,(
    cell55 != cell20 )).

fof(tlhfof48195,axiom,(
    cell55 != cell19 )).

fof(tlhfof48196,axiom,(
    cell55 != cell18 )).

fof(tlhfof48197,axiom,(
    cell55 != cell17 )).

fof(tlhfof48198,axiom,(
    cell55 != cell16 )).

fof(tlhfof48199,axiom,(
    cell55 != cell15 )).

fof(tlhfof48200,axiom,(
    cell55 != cell14 )).

fof(tlhfof48201,axiom,(
    cell55 != cell10 )).

fof(tlhfof48202,axiom,(
    cell55 != cell9 )).

fof(tlhfof48203,axiom,(
    cell55 != cell8 )).

fof(tlhfof48204,axiom,(
    cell55 != cell7 )).

fof(tlhfof48205,axiom,(
    cell55 != cell6 )).

fof(tlhfof48206,axiom,(
    cell55 != cell5 )).

fof(tlhfof48207,axiom,(
    cell55 != cell4 )).

fof(tlhfof48208,axiom,(
    cell55 != cell3 )).

fof(tlhfof48209,axiom,(
    cell55 != cell1 )).

fof(tlhfof48210,axiom,(
    cell55 != cell89 )).

fof(tlhfof48211,axiom,(
    cell55 != cell88 )).

fof(tlhfof48212,axiom,(
    cell55 != cell79 )).

fof(tlhfof48213,axiom,(
    cell55 != cell90 )).

fof(tlhfof48214,axiom,(
    cell55 != cell99 )).

fof(tlhfof48215,axiom,(
    cell55 != cell12 )).

fof(tlhfof48216,axiom,(
    cell55 != cell11 )).

fof(tlhfof48217,axiom,(
    cell55 != cell2 )).

fof(tlhfof48218,axiom,(
    cell55 != cell13 )).

fof(tlhfof48219,axiom,(
    cell55 != cell22 )).

fof(tlhfof48220,axiom,(
    cell54 != cell53 )).

fof(tlhfof48221,axiom,(
    cell54 != cell51 )).

fof(tlhfof48222,axiom,(
    cell54 != cell52 )).

fof(tlhfof48223,axiom,(
    cell54 != cell50 )).

fof(tlhfof48224,axiom,(
    cell54 != cell49 )).

fof(tlhfof48225,axiom,(
    cell54 != cell48 )).

fof(tlhfof48226,axiom,(
    cell54 != cell47 )).

fof(tlhfof48227,axiom,(
    cell54 != cell46 )).

fof(tlhfof48228,axiom,(
    cell54 != cell45 )).

fof(tlhfof48229,axiom,(
    cell54 != cell44 )).

fof(tlhfof48230,axiom,(
    cell54 != cell43 )).

fof(tlhfof48231,axiom,(
    cell54 != cell41 )).

fof(tlhfof48232,axiom,(
    cell54 != cell42 )).

fof(tlhfof48233,axiom,(
    cell54 != cell40 )).

fof(tlhfof48234,axiom,(
    cell54 != cell39 )).

fof(tlhfof48235,axiom,(
    cell54 != cell38 )).

fof(tlhfof48236,axiom,(
    cell54 != cell37 )).

fof(tlhfof48237,axiom,(
    cell54 != cell36 )).

fof(tlhfof48238,axiom,(
    cell54 != cell35 )).

fof(tlhfof48239,axiom,(
    cell54 != cell34 )).

fof(tlhfof48240,axiom,(
    cell54 != cell33 )).

fof(tlhfof48241,axiom,(
    cell54 != cell31 )).

fof(tlhfof48242,axiom,(
    cell54 != cell32 )).

fof(tlhfof48243,axiom,(
    cell54 != cell30 )).

fof(tlhfof48244,axiom,(
    cell54 != cell29 )).

fof(tlhfof48245,axiom,(
    cell54 != cell28 )).

fof(tlhfof48246,axiom,(
    cell54 != cell27 )).

fof(tlhfof48247,axiom,(
    cell54 != cell26 )).

fof(tlhfof48248,axiom,(
    cell54 != cell25 )).

fof(tlhfof48249,axiom,(
    cell54 != cell24 )).

fof(tlhfof48250,axiom,(
    cell54 != cell23 )).

fof(tlhfof48251,axiom,(
    cell54 != cell21 )).

fof(tlhfof48252,axiom,(
    cell54 != cell20 )).

fof(tlhfof48253,axiom,(
    cell54 != cell19 )).

fof(tlhfof48254,axiom,(
    cell54 != cell18 )).

fof(tlhfof48255,axiom,(
    cell54 != cell17 )).

fof(tlhfof48256,axiom,(
    cell54 != cell16 )).

fof(tlhfof48257,axiom,(
    cell54 != cell15 )).

fof(tlhfof48258,axiom,(
    cell54 != cell14 )).

fof(tlhfof48259,axiom,(
    cell54 != cell10 )).

fof(tlhfof48260,axiom,(
    cell54 != cell9 )).

fof(tlhfof48261,axiom,(
    cell54 != cell8 )).

fof(tlhfof48262,axiom,(
    cell54 != cell7 )).

fof(tlhfof48263,axiom,(
    cell54 != cell6 )).

fof(tlhfof48264,axiom,(
    cell54 != cell5 )).

fof(tlhfof48265,axiom,(
    cell54 != cell4 )).

fof(tlhfof48266,axiom,(
    cell54 != cell3 )).

fof(tlhfof48267,axiom,(
    cell54 != cell1 )).

fof(tlhfof48268,axiom,(
    cell54 != cell89 )).

fof(tlhfof48269,axiom,(
    cell54 != cell88 )).

fof(tlhfof48270,axiom,(
    cell54 != cell79 )).

fof(tlhfof48271,axiom,(
    cell54 != cell90 )).

fof(tlhfof48272,axiom,(
    cell54 != cell99 )).

fof(tlhfof48273,axiom,(
    cell54 != cell12 )).

fof(tlhfof48274,axiom,(
    cell54 != cell11 )).

fof(tlhfof48275,axiom,(
    cell54 != cell2 )).

fof(tlhfof48276,axiom,(
    cell54 != cell13 )).

fof(tlhfof48277,axiom,(
    cell54 != cell22 )).

fof(tlhfof48278,axiom,(
    cell53 != cell51 )).

fof(tlhfof48279,axiom,(
    cell53 != cell52 )).

fof(tlhfof48280,axiom,(
    cell53 != cell50 )).

fof(tlhfof48281,axiom,(
    cell53 != cell49 )).

fof(tlhfof48282,axiom,(
    cell53 != cell48 )).

fof(tlhfof48283,axiom,(
    cell53 != cell47 )).

fof(tlhfof48284,axiom,(
    cell53 != cell46 )).

fof(tlhfof48285,axiom,(
    cell53 != cell45 )).

fof(tlhfof48286,axiom,(
    cell53 != cell44 )).

fof(tlhfof48287,axiom,(
    cell53 != cell43 )).

fof(tlhfof48288,axiom,(
    cell53 != cell41 )).

fof(tlhfof48289,axiom,(
    cell53 != cell42 )).

fof(tlhfof48290,axiom,(
    cell53 != cell40 )).

fof(tlhfof48291,axiom,(
    cell53 != cell39 )).

fof(tlhfof48292,axiom,(
    cell53 != cell38 )).

fof(tlhfof48293,axiom,(
    cell53 != cell37 )).

fof(tlhfof48294,axiom,(
    cell53 != cell36 )).

fof(tlhfof48295,axiom,(
    cell53 != cell35 )).

fof(tlhfof48296,axiom,(
    cell53 != cell34 )).

fof(tlhfof48297,axiom,(
    cell53 != cell33 )).

fof(tlhfof48298,axiom,(
    cell53 != cell31 )).

fof(tlhfof48299,axiom,(
    cell53 != cell32 )).

fof(tlhfof48300,axiom,(
    cell53 != cell30 )).

fof(tlhfof48301,axiom,(
    cell53 != cell29 )).

fof(tlhfof48302,axiom,(
    cell53 != cell28 )).

fof(tlhfof48303,axiom,(
    cell53 != cell27 )).

fof(tlhfof48304,axiom,(
    cell53 != cell26 )).

fof(tlhfof48305,axiom,(
    cell53 != cell25 )).

fof(tlhfof48306,axiom,(
    cell53 != cell24 )).

fof(tlhfof48307,axiom,(
    cell53 != cell23 )).

fof(tlhfof48308,axiom,(
    cell53 != cell21 )).

fof(tlhfof48309,axiom,(
    cell53 != cell20 )).

fof(tlhfof48310,axiom,(
    cell53 != cell19 )).

fof(tlhfof48311,axiom,(
    cell53 != cell18 )).

fof(tlhfof48312,axiom,(
    cell53 != cell17 )).

fof(tlhfof48313,axiom,(
    cell53 != cell16 )).

fof(tlhfof48314,axiom,(
    cell53 != cell15 )).

fof(tlhfof48315,axiom,(
    cell53 != cell14 )).

fof(tlhfof48316,axiom,(
    cell53 != cell10 )).

fof(tlhfof48317,axiom,(
    cell53 != cell9 )).

fof(tlhfof48318,axiom,(
    cell53 != cell8 )).

fof(tlhfof48319,axiom,(
    cell53 != cell7 )).

fof(tlhfof48320,axiom,(
    cell53 != cell6 )).

fof(tlhfof48321,axiom,(
    cell53 != cell5 )).

fof(tlhfof48322,axiom,(
    cell53 != cell4 )).

fof(tlhfof48323,axiom,(
    cell53 != cell3 )).

fof(tlhfof48324,axiom,(
    cell53 != cell1 )).

fof(tlhfof48325,axiom,(
    cell53 != cell89 )).

fof(tlhfof48326,axiom,(
    cell53 != cell88 )).

fof(tlhfof48327,axiom,(
    cell53 != cell79 )).

fof(tlhfof48328,axiom,(
    cell53 != cell90 )).

fof(tlhfof48329,axiom,(
    cell53 != cell99 )).

fof(tlhfof48330,axiom,(
    cell53 != cell12 )).

fof(tlhfof48331,axiom,(
    cell53 != cell11 )).

fof(tlhfof48332,axiom,(
    cell53 != cell2 )).

fof(tlhfof48333,axiom,(
    cell53 != cell13 )).

fof(tlhfof48334,axiom,(
    cell53 != cell22 )).

fof(tlhfof48335,axiom,(
    cell51 != cell52 )).

fof(tlhfof48336,axiom,(
    cell51 != cell50 )).

fof(tlhfof48337,axiom,(
    cell51 != cell49 )).

fof(tlhfof48338,axiom,(
    cell51 != cell48 )).

fof(tlhfof48339,axiom,(
    cell51 != cell47 )).

fof(tlhfof48340,axiom,(
    cell51 != cell46 )).

fof(tlhfof48341,axiom,(
    cell51 != cell45 )).

fof(tlhfof48342,axiom,(
    cell51 != cell44 )).

fof(tlhfof48343,axiom,(
    cell51 != cell43 )).

fof(tlhfof48344,axiom,(
    cell51 != cell41 )).

fof(tlhfof48345,axiom,(
    cell51 != cell42 )).

fof(tlhfof48346,axiom,(
    cell51 != cell40 )).

fof(tlhfof48347,axiom,(
    cell51 != cell39 )).

fof(tlhfof48348,axiom,(
    cell51 != cell38 )).

fof(tlhfof48349,axiom,(
    cell51 != cell37 )).

fof(tlhfof48350,axiom,(
    cell51 != cell36 )).

fof(tlhfof48351,axiom,(
    cell51 != cell35 )).

fof(tlhfof48352,axiom,(
    cell51 != cell34 )).

fof(tlhfof48353,axiom,(
    cell51 != cell33 )).

fof(tlhfof48354,axiom,(
    cell51 != cell31 )).

fof(tlhfof48355,axiom,(
    cell51 != cell32 )).

fof(tlhfof48356,axiom,(
    cell51 != cell30 )).

fof(tlhfof48357,axiom,(
    cell51 != cell29 )).

fof(tlhfof48358,axiom,(
    cell51 != cell28 )).

fof(tlhfof48359,axiom,(
    cell51 != cell27 )).

fof(tlhfof48360,axiom,(
    cell51 != cell26 )).

fof(tlhfof48361,axiom,(
    cell51 != cell25 )).

fof(tlhfof48362,axiom,(
    cell51 != cell24 )).

fof(tlhfof48363,axiom,(
    cell51 != cell23 )).

fof(tlhfof48364,axiom,(
    cell51 != cell21 )).

fof(tlhfof48365,axiom,(
    cell51 != cell20 )).

fof(tlhfof48366,axiom,(
    cell51 != cell19 )).

fof(tlhfof48367,axiom,(
    cell51 != cell18 )).

fof(tlhfof48368,axiom,(
    cell51 != cell17 )).

fof(tlhfof48369,axiom,(
    cell51 != cell16 )).

fof(tlhfof48370,axiom,(
    cell51 != cell15 )).

fof(tlhfof48371,axiom,(
    cell51 != cell14 )).

fof(tlhfof48372,axiom,(
    cell51 != cell10 )).

fof(tlhfof48373,axiom,(
    cell51 != cell9 )).

fof(tlhfof48374,axiom,(
    cell51 != cell8 )).

fof(tlhfof48375,axiom,(
    cell51 != cell7 )).

fof(tlhfof48376,axiom,(
    cell51 != cell6 )).

fof(tlhfof48377,axiom,(
    cell51 != cell5 )).

fof(tlhfof48378,axiom,(
    cell51 != cell4 )).

fof(tlhfof48379,axiom,(
    cell51 != cell3 )).

fof(tlhfof48380,axiom,(
    cell51 != cell1 )).

fof(tlhfof48381,axiom,(
    cell51 != cell89 )).

fof(tlhfof48382,axiom,(
    cell51 != cell88 )).

fof(tlhfof48383,axiom,(
    cell51 != cell79 )).

fof(tlhfof48384,axiom,(
    cell51 != cell90 )).

fof(tlhfof48385,axiom,(
    cell51 != cell99 )).

fof(tlhfof48386,axiom,(
    cell51 != cell12 )).

fof(tlhfof48387,axiom,(
    cell51 != cell11 )).

fof(tlhfof48388,axiom,(
    cell51 != cell2 )).

fof(tlhfof48389,axiom,(
    cell51 != cell13 )).

fof(tlhfof48390,axiom,(
    cell51 != cell22 )).

fof(tlhfof48391,axiom,(
    cell52 != cell50 )).

fof(tlhfof48392,axiom,(
    cell52 != cell49 )).

fof(tlhfof48393,axiom,(
    cell52 != cell48 )).

fof(tlhfof48394,axiom,(
    cell52 != cell47 )).

fof(tlhfof48395,axiom,(
    cell52 != cell46 )).

fof(tlhfof48396,axiom,(
    cell52 != cell45 )).

fof(tlhfof48397,axiom,(
    cell52 != cell44 )).

fof(tlhfof48398,axiom,(
    cell52 != cell43 )).

fof(tlhfof48399,axiom,(
    cell52 != cell41 )).

fof(tlhfof48400,axiom,(
    cell52 != cell42 )).

fof(tlhfof48401,axiom,(
    cell52 != cell40 )).

fof(tlhfof48402,axiom,(
    cell52 != cell39 )).

fof(tlhfof48403,axiom,(
    cell52 != cell38 )).

fof(tlhfof48404,axiom,(
    cell52 != cell37 )).

fof(tlhfof48405,axiom,(
    cell52 != cell36 )).

fof(tlhfof48406,axiom,(
    cell52 != cell35 )).

fof(tlhfof48407,axiom,(
    cell52 != cell34 )).

fof(tlhfof48408,axiom,(
    cell52 != cell33 )).

fof(tlhfof48409,axiom,(
    cell52 != cell31 )).

fof(tlhfof48410,axiom,(
    cell52 != cell32 )).

fof(tlhfof48411,axiom,(
    cell52 != cell30 )).

fof(tlhfof48412,axiom,(
    cell52 != cell29 )).

fof(tlhfof48413,axiom,(
    cell52 != cell28 )).

fof(tlhfof48414,axiom,(
    cell52 != cell27 )).

fof(tlhfof48415,axiom,(
    cell52 != cell26 )).

fof(tlhfof48416,axiom,(
    cell52 != cell25 )).

fof(tlhfof48417,axiom,(
    cell52 != cell24 )).

fof(tlhfof48418,axiom,(
    cell52 != cell23 )).

fof(tlhfof48419,axiom,(
    cell52 != cell21 )).

fof(tlhfof48420,axiom,(
    cell52 != cell20 )).

fof(tlhfof48421,axiom,(
    cell52 != cell19 )).

fof(tlhfof48422,axiom,(
    cell52 != cell18 )).

fof(tlhfof48423,axiom,(
    cell52 != cell17 )).

fof(tlhfof48424,axiom,(
    cell52 != cell16 )).

fof(tlhfof48425,axiom,(
    cell52 != cell15 )).

fof(tlhfof48426,axiom,(
    cell52 != cell14 )).

fof(tlhfof48427,axiom,(
    cell52 != cell10 )).

fof(tlhfof48428,axiom,(
    cell52 != cell9 )).

fof(tlhfof48429,axiom,(
    cell52 != cell8 )).

fof(tlhfof48430,axiom,(
    cell52 != cell7 )).

fof(tlhfof48431,axiom,(
    cell52 != cell6 )).

fof(tlhfof48432,axiom,(
    cell52 != cell5 )).

fof(tlhfof48433,axiom,(
    cell52 != cell4 )).

fof(tlhfof48434,axiom,(
    cell52 != cell3 )).

fof(tlhfof48435,axiom,(
    cell52 != cell1 )).

fof(tlhfof48436,axiom,(
    cell52 != cell89 )).

fof(tlhfof48437,axiom,(
    cell52 != cell88 )).

fof(tlhfof48438,axiom,(
    cell52 != cell79 )).

fof(tlhfof48439,axiom,(
    cell52 != cell90 )).

fof(tlhfof48440,axiom,(
    cell52 != cell99 )).

fof(tlhfof48441,axiom,(
    cell52 != cell12 )).

fof(tlhfof48442,axiom,(
    cell52 != cell11 )).

fof(tlhfof48443,axiom,(
    cell52 != cell2 )).

fof(tlhfof48444,axiom,(
    cell52 != cell13 )).

fof(tlhfof48445,axiom,(
    cell52 != cell22 )).

fof(tlhfof48446,axiom,(
    cell50 != cell49 )).

fof(tlhfof48447,axiom,(
    cell50 != cell48 )).

fof(tlhfof48448,axiom,(
    cell50 != cell47 )).

fof(tlhfof48449,axiom,(
    cell50 != cell46 )).

fof(tlhfof48450,axiom,(
    cell50 != cell45 )).

fof(tlhfof48451,axiom,(
    cell50 != cell44 )).

fof(tlhfof48452,axiom,(
    cell50 != cell43 )).

fof(tlhfof48453,axiom,(
    cell50 != cell41 )).

fof(tlhfof48454,axiom,(
    cell50 != cell42 )).

fof(tlhfof48455,axiom,(
    cell50 != cell40 )).

fof(tlhfof48456,axiom,(
    cell50 != cell39 )).

fof(tlhfof48457,axiom,(
    cell50 != cell38 )).

fof(tlhfof48458,axiom,(
    cell50 != cell37 )).

fof(tlhfof48459,axiom,(
    cell50 != cell36 )).

fof(tlhfof48460,axiom,(
    cell50 != cell35 )).

fof(tlhfof48461,axiom,(
    cell50 != cell34 )).

fof(tlhfof48462,axiom,(
    cell50 != cell33 )).

fof(tlhfof48463,axiom,(
    cell50 != cell31 )).

fof(tlhfof48464,axiom,(
    cell50 != cell32 )).

fof(tlhfof48465,axiom,(
    cell50 != cell30 )).

fof(tlhfof48466,axiom,(
    cell50 != cell29 )).

fof(tlhfof48467,axiom,(
    cell50 != cell28 )).

fof(tlhfof48468,axiom,(
    cell50 != cell27 )).

fof(tlhfof48469,axiom,(
    cell50 != cell26 )).

fof(tlhfof48470,axiom,(
    cell50 != cell25 )).

fof(tlhfof48471,axiom,(
    cell50 != cell24 )).

fof(tlhfof48472,axiom,(
    cell50 != cell23 )).

fof(tlhfof48473,axiom,(
    cell50 != cell21 )).

fof(tlhfof48474,axiom,(
    cell50 != cell20 )).

fof(tlhfof48475,axiom,(
    cell50 != cell19 )).

fof(tlhfof48476,axiom,(
    cell50 != cell18 )).

fof(tlhfof48477,axiom,(
    cell50 != cell17 )).

fof(tlhfof48478,axiom,(
    cell50 != cell16 )).

fof(tlhfof48479,axiom,(
    cell50 != cell15 )).

fof(tlhfof48480,axiom,(
    cell50 != cell14 )).

fof(tlhfof48481,axiom,(
    cell50 != cell10 )).

fof(tlhfof48482,axiom,(
    cell50 != cell9 )).

fof(tlhfof48483,axiom,(
    cell50 != cell8 )).

fof(tlhfof48484,axiom,(
    cell50 != cell7 )).

fof(tlhfof48485,axiom,(
    cell50 != cell6 )).

fof(tlhfof48486,axiom,(
    cell50 != cell5 )).

fof(tlhfof48487,axiom,(
    cell50 != cell4 )).

fof(tlhfof48488,axiom,(
    cell50 != cell3 )).

fof(tlhfof48489,axiom,(
    cell50 != cell1 )).

fof(tlhfof48490,axiom,(
    cell50 != cell89 )).

fof(tlhfof48491,axiom,(
    cell50 != cell88 )).

fof(tlhfof48492,axiom,(
    cell50 != cell79 )).

fof(tlhfof48493,axiom,(
    cell50 != cell90 )).

fof(tlhfof48494,axiom,(
    cell50 != cell99 )).

fof(tlhfof48495,axiom,(
    cell50 != cell12 )).

fof(tlhfof48496,axiom,(
    cell50 != cell11 )).

fof(tlhfof48497,axiom,(
    cell50 != cell2 )).

fof(tlhfof48498,axiom,(
    cell50 != cell13 )).

fof(tlhfof48499,axiom,(
    cell50 != cell22 )).

fof(tlhfof48500,axiom,(
    cell49 != cell48 )).

fof(tlhfof48501,axiom,(
    cell49 != cell47 )).

fof(tlhfof48502,axiom,(
    cell49 != cell46 )).

fof(tlhfof48503,axiom,(
    cell49 != cell45 )).

fof(tlhfof48504,axiom,(
    cell49 != cell44 )).

fof(tlhfof48505,axiom,(
    cell49 != cell43 )).

fof(tlhfof48506,axiom,(
    cell49 != cell41 )).

fof(tlhfof48507,axiom,(
    cell49 != cell42 )).

fof(tlhfof48508,axiom,(
    cell49 != cell40 )).

fof(tlhfof48509,axiom,(
    cell49 != cell39 )).

fof(tlhfof48510,axiom,(
    cell49 != cell38 )).

fof(tlhfof48511,axiom,(
    cell49 != cell37 )).

fof(tlhfof48512,axiom,(
    cell49 != cell36 )).

fof(tlhfof48513,axiom,(
    cell49 != cell35 )).

fof(tlhfof48514,axiom,(
    cell49 != cell34 )).

fof(tlhfof48515,axiom,(
    cell49 != cell33 )).

fof(tlhfof48516,axiom,(
    cell49 != cell31 )).

fof(tlhfof48517,axiom,(
    cell49 != cell32 )).

fof(tlhfof48518,axiom,(
    cell49 != cell30 )).

fof(tlhfof48519,axiom,(
    cell49 != cell29 )).

fof(tlhfof48520,axiom,(
    cell49 != cell28 )).

fof(tlhfof48521,axiom,(
    cell49 != cell27 )).

fof(tlhfof48522,axiom,(
    cell49 != cell26 )).

fof(tlhfof48523,axiom,(
    cell49 != cell25 )).

fof(tlhfof48524,axiom,(
    cell49 != cell24 )).

fof(tlhfof48525,axiom,(
    cell49 != cell23 )).

fof(tlhfof48526,axiom,(
    cell49 != cell21 )).

fof(tlhfof48527,axiom,(
    cell49 != cell20 )).

fof(tlhfof48528,axiom,(
    cell49 != cell19 )).

fof(tlhfof48529,axiom,(
    cell49 != cell18 )).

fof(tlhfof48530,axiom,(
    cell49 != cell17 )).

fof(tlhfof48531,axiom,(
    cell49 != cell16 )).

fof(tlhfof48532,axiom,(
    cell49 != cell15 )).

fof(tlhfof48533,axiom,(
    cell49 != cell14 )).

fof(tlhfof48534,axiom,(
    cell49 != cell10 )).

fof(tlhfof48535,axiom,(
    cell49 != cell9 )).

fof(tlhfof48536,axiom,(
    cell49 != cell8 )).

fof(tlhfof48537,axiom,(
    cell49 != cell7 )).

fof(tlhfof48538,axiom,(
    cell49 != cell6 )).

fof(tlhfof48539,axiom,(
    cell49 != cell5 )).

fof(tlhfof48540,axiom,(
    cell49 != cell4 )).

fof(tlhfof48541,axiom,(
    cell49 != cell3 )).

fof(tlhfof48542,axiom,(
    cell49 != cell1 )).

fof(tlhfof48543,axiom,(
    cell49 != cell89 )).

fof(tlhfof48544,axiom,(
    cell49 != cell88 )).

fof(tlhfof48545,axiom,(
    cell49 != cell79 )).

fof(tlhfof48546,axiom,(
    cell49 != cell90 )).

fof(tlhfof48547,axiom,(
    cell49 != cell99 )).

fof(tlhfof48548,axiom,(
    cell49 != cell12 )).

fof(tlhfof48549,axiom,(
    cell49 != cell11 )).

fof(tlhfof48550,axiom,(
    cell49 != cell2 )).

fof(tlhfof48551,axiom,(
    cell49 != cell13 )).

fof(tlhfof48552,axiom,(
    cell49 != cell22 )).

fof(tlhfof48553,axiom,(
    cell48 != cell47 )).

fof(tlhfof48554,axiom,(
    cell48 != cell46 )).

fof(tlhfof48555,axiom,(
    cell48 != cell45 )).

fof(tlhfof48556,axiom,(
    cell48 != cell44 )).

fof(tlhfof48557,axiom,(
    cell48 != cell43 )).

fof(tlhfof48558,axiom,(
    cell48 != cell41 )).

fof(tlhfof48559,axiom,(
    cell48 != cell42 )).

fof(tlhfof48560,axiom,(
    cell48 != cell40 )).

fof(tlhfof48561,axiom,(
    cell48 != cell39 )).

fof(tlhfof48562,axiom,(
    cell48 != cell38 )).

fof(tlhfof48563,axiom,(
    cell48 != cell37 )).

fof(tlhfof48564,axiom,(
    cell48 != cell36 )).

fof(tlhfof48565,axiom,(
    cell48 != cell35 )).

fof(tlhfof48566,axiom,(
    cell48 != cell34 )).

fof(tlhfof48567,axiom,(
    cell48 != cell33 )).

fof(tlhfof48568,axiom,(
    cell48 != cell31 )).

fof(tlhfof48569,axiom,(
    cell48 != cell32 )).

fof(tlhfof48570,axiom,(
    cell48 != cell30 )).

fof(tlhfof48571,axiom,(
    cell48 != cell29 )).

fof(tlhfof48572,axiom,(
    cell48 != cell28 )).

fof(tlhfof48573,axiom,(
    cell48 != cell27 )).

fof(tlhfof48574,axiom,(
    cell48 != cell26 )).

fof(tlhfof48575,axiom,(
    cell48 != cell25 )).

fof(tlhfof48576,axiom,(
    cell48 != cell24 )).

fof(tlhfof48577,axiom,(
    cell48 != cell23 )).

fof(tlhfof48578,axiom,(
    cell48 != cell21 )).

fof(tlhfof48579,axiom,(
    cell48 != cell20 )).

fof(tlhfof48580,axiom,(
    cell48 != cell19 )).

fof(tlhfof48581,axiom,(
    cell48 != cell18 )).

fof(tlhfof48582,axiom,(
    cell48 != cell17 )).

fof(tlhfof48583,axiom,(
    cell48 != cell16 )).

fof(tlhfof48584,axiom,(
    cell48 != cell15 )).

fof(tlhfof48585,axiom,(
    cell48 != cell14 )).

fof(tlhfof48586,axiom,(
    cell48 != cell10 )).

fof(tlhfof48587,axiom,(
    cell48 != cell9 )).

fof(tlhfof48588,axiom,(
    cell48 != cell8 )).

fof(tlhfof48589,axiom,(
    cell48 != cell7 )).

fof(tlhfof48590,axiom,(
    cell48 != cell6 )).

fof(tlhfof48591,axiom,(
    cell48 != cell5 )).

fof(tlhfof48592,axiom,(
    cell48 != cell4 )).

fof(tlhfof48593,axiom,(
    cell48 != cell3 )).

fof(tlhfof48594,axiom,(
    cell48 != cell1 )).

fof(tlhfof48595,axiom,(
    cell48 != cell89 )).

fof(tlhfof48596,axiom,(
    cell48 != cell88 )).

fof(tlhfof48597,axiom,(
    cell48 != cell79 )).

fof(tlhfof48598,axiom,(
    cell48 != cell90 )).

fof(tlhfof48599,axiom,(
    cell48 != cell99 )).

fof(tlhfof48600,axiom,(
    cell48 != cell12 )).

fof(tlhfof48601,axiom,(
    cell48 != cell11 )).

fof(tlhfof48602,axiom,(
    cell48 != cell2 )).

fof(tlhfof48603,axiom,(
    cell48 != cell13 )).

fof(tlhfof48604,axiom,(
    cell48 != cell22 )).

fof(tlhfof48605,axiom,(
    cell47 != cell46 )).

fof(tlhfof48606,axiom,(
    cell47 != cell45 )).

fof(tlhfof48607,axiom,(
    cell47 != cell44 )).

fof(tlhfof48608,axiom,(
    cell47 != cell43 )).

fof(tlhfof48609,axiom,(
    cell47 != cell41 )).

fof(tlhfof48610,axiom,(
    cell47 != cell42 )).

fof(tlhfof48611,axiom,(
    cell47 != cell40 )).

fof(tlhfof48612,axiom,(
    cell47 != cell39 )).

fof(tlhfof48613,axiom,(
    cell47 != cell38 )).

fof(tlhfof48614,axiom,(
    cell47 != cell37 )).

fof(tlhfof48615,axiom,(
    cell47 != cell36 )).

fof(tlhfof48616,axiom,(
    cell47 != cell35 )).

fof(tlhfof48617,axiom,(
    cell47 != cell34 )).

fof(tlhfof48618,axiom,(
    cell47 != cell33 )).

fof(tlhfof48619,axiom,(
    cell47 != cell31 )).

fof(tlhfof48620,axiom,(
    cell47 != cell32 )).

fof(tlhfof48621,axiom,(
    cell47 != cell30 )).

fof(tlhfof48622,axiom,(
    cell47 != cell29 )).

fof(tlhfof48623,axiom,(
    cell47 != cell28 )).

fof(tlhfof48624,axiom,(
    cell47 != cell27 )).

fof(tlhfof48625,axiom,(
    cell47 != cell26 )).

fof(tlhfof48626,axiom,(
    cell47 != cell25 )).

fof(tlhfof48627,axiom,(
    cell47 != cell24 )).

fof(tlhfof48628,axiom,(
    cell47 != cell23 )).

fof(tlhfof48629,axiom,(
    cell47 != cell21 )).

fof(tlhfof48630,axiom,(
    cell47 != cell20 )).

fof(tlhfof48631,axiom,(
    cell47 != cell19 )).

fof(tlhfof48632,axiom,(
    cell47 != cell18 )).

fof(tlhfof48633,axiom,(
    cell47 != cell17 )).

fof(tlhfof48634,axiom,(
    cell47 != cell16 )).

fof(tlhfof48635,axiom,(
    cell47 != cell15 )).

fof(tlhfof48636,axiom,(
    cell47 != cell14 )).

fof(tlhfof48637,axiom,(
    cell47 != cell10 )).

fof(tlhfof48638,axiom,(
    cell47 != cell9 )).

fof(tlhfof48639,axiom,(
    cell47 != cell8 )).

fof(tlhfof48640,axiom,(
    cell47 != cell7 )).

fof(tlhfof48641,axiom,(
    cell47 != cell6 )).

fof(tlhfof48642,axiom,(
    cell47 != cell5 )).

fof(tlhfof48643,axiom,(
    cell47 != cell4 )).

fof(tlhfof48644,axiom,(
    cell47 != cell3 )).

fof(tlhfof48645,axiom,(
    cell47 != cell1 )).

fof(tlhfof48646,axiom,(
    cell47 != cell89 )).

fof(tlhfof48647,axiom,(
    cell47 != cell88 )).

fof(tlhfof48648,axiom,(
    cell47 != cell79 )).

fof(tlhfof48649,axiom,(
    cell47 != cell90 )).

fof(tlhfof48650,axiom,(
    cell47 != cell99 )).

fof(tlhfof48651,axiom,(
    cell47 != cell12 )).

fof(tlhfof48652,axiom,(
    cell47 != cell11 )).

fof(tlhfof48653,axiom,(
    cell47 != cell2 )).

fof(tlhfof48654,axiom,(
    cell47 != cell13 )).

fof(tlhfof48655,axiom,(
    cell47 != cell22 )).

fof(tlhfof48656,axiom,(
    cell46 != cell45 )).

fof(tlhfof48657,axiom,(
    cell46 != cell44 )).

fof(tlhfof48658,axiom,(
    cell46 != cell43 )).

fof(tlhfof48659,axiom,(
    cell46 != cell41 )).

fof(tlhfof48660,axiom,(
    cell46 != cell42 )).

fof(tlhfof48661,axiom,(
    cell46 != cell40 )).

fof(tlhfof48662,axiom,(
    cell46 != cell39 )).

fof(tlhfof48663,axiom,(
    cell46 != cell38 )).

fof(tlhfof48664,axiom,(
    cell46 != cell37 )).

fof(tlhfof48665,axiom,(
    cell46 != cell36 )).

fof(tlhfof48666,axiom,(
    cell46 != cell35 )).

fof(tlhfof48667,axiom,(
    cell46 != cell34 )).

fof(tlhfof48668,axiom,(
    cell46 != cell33 )).

fof(tlhfof48669,axiom,(
    cell46 != cell31 )).

fof(tlhfof48670,axiom,(
    cell46 != cell32 )).

fof(tlhfof48671,axiom,(
    cell46 != cell30 )).

fof(tlhfof48672,axiom,(
    cell46 != cell29 )).

fof(tlhfof48673,axiom,(
    cell46 != cell28 )).

fof(tlhfof48674,axiom,(
    cell46 != cell27 )).

fof(tlhfof48675,axiom,(
    cell46 != cell26 )).

fof(tlhfof48676,axiom,(
    cell46 != cell25 )).

fof(tlhfof48677,axiom,(
    cell46 != cell24 )).

fof(tlhfof48678,axiom,(
    cell46 != cell23 )).

fof(tlhfof48679,axiom,(
    cell46 != cell21 )).

fof(tlhfof48680,axiom,(
    cell46 != cell20 )).

fof(tlhfof48681,axiom,(
    cell46 != cell19 )).

fof(tlhfof48682,axiom,(
    cell46 != cell18 )).

fof(tlhfof48683,axiom,(
    cell46 != cell17 )).

fof(tlhfof48684,axiom,(
    cell46 != cell16 )).

fof(tlhfof48685,axiom,(
    cell46 != cell15 )).

fof(tlhfof48686,axiom,(
    cell46 != cell14 )).

fof(tlhfof48687,axiom,(
    cell46 != cell10 )).

fof(tlhfof48688,axiom,(
    cell46 != cell9 )).

fof(tlhfof48689,axiom,(
    cell46 != cell8 )).

fof(tlhfof48690,axiom,(
    cell46 != cell7 )).

fof(tlhfof48691,axiom,(
    cell46 != cell6 )).

fof(tlhfof48692,axiom,(
    cell46 != cell5 )).

fof(tlhfof48693,axiom,(
    cell46 != cell4 )).

fof(tlhfof48694,axiom,(
    cell46 != cell3 )).

fof(tlhfof48695,axiom,(
    cell46 != cell1 )).

fof(tlhfof48696,axiom,(
    cell46 != cell89 )).

fof(tlhfof48697,axiom,(
    cell46 != cell88 )).

fof(tlhfof48698,axiom,(
    cell46 != cell79 )).

fof(tlhfof48699,axiom,(
    cell46 != cell90 )).

fof(tlhfof48700,axiom,(
    cell46 != cell99 )).

fof(tlhfof48701,axiom,(
    cell46 != cell12 )).

fof(tlhfof48702,axiom,(
    cell46 != cell11 )).

fof(tlhfof48703,axiom,(
    cell46 != cell2 )).

fof(tlhfof48704,axiom,(
    cell46 != cell13 )).

fof(tlhfof48705,axiom,(
    cell46 != cell22 )).

fof(tlhfof48706,axiom,(
    cell45 != cell44 )).

fof(tlhfof48707,axiom,(
    cell45 != cell43 )).

fof(tlhfof48708,axiom,(
    cell45 != cell41 )).

fof(tlhfof48709,axiom,(
    cell45 != cell42 )).

fof(tlhfof48710,axiom,(
    cell45 != cell40 )).

fof(tlhfof48711,axiom,(
    cell45 != cell39 )).

fof(tlhfof48712,axiom,(
    cell45 != cell38 )).

fof(tlhfof48713,axiom,(
    cell45 != cell37 )).

fof(tlhfof48714,axiom,(
    cell45 != cell36 )).

fof(tlhfof48715,axiom,(
    cell45 != cell35 )).

fof(tlhfof48716,axiom,(
    cell45 != cell34 )).

fof(tlhfof48717,axiom,(
    cell45 != cell33 )).

fof(tlhfof48718,axiom,(
    cell45 != cell31 )).

fof(tlhfof48719,axiom,(
    cell45 != cell32 )).

fof(tlhfof48720,axiom,(
    cell45 != cell30 )).

fof(tlhfof48721,axiom,(
    cell45 != cell29 )).

fof(tlhfof48722,axiom,(
    cell45 != cell28 )).

fof(tlhfof48723,axiom,(
    cell45 != cell27 )).

fof(tlhfof48724,axiom,(
    cell45 != cell26 )).

fof(tlhfof48725,axiom,(
    cell45 != cell25 )).

fof(tlhfof48726,axiom,(
    cell45 != cell24 )).

fof(tlhfof48727,axiom,(
    cell45 != cell23 )).

fof(tlhfof48728,axiom,(
    cell45 != cell21 )).

fof(tlhfof48729,axiom,(
    cell45 != cell20 )).

fof(tlhfof48730,axiom,(
    cell45 != cell19 )).

fof(tlhfof48731,axiom,(
    cell45 != cell18 )).

fof(tlhfof48732,axiom,(
    cell45 != cell17 )).

fof(tlhfof48733,axiom,(
    cell45 != cell16 )).

fof(tlhfof48734,axiom,(
    cell45 != cell15 )).

fof(tlhfof48735,axiom,(
    cell45 != cell14 )).

fof(tlhfof48736,axiom,(
    cell45 != cell10 )).

fof(tlhfof48737,axiom,(
    cell45 != cell9 )).

fof(tlhfof48738,axiom,(
    cell45 != cell8 )).

fof(tlhfof48739,axiom,(
    cell45 != cell7 )).

fof(tlhfof48740,axiom,(
    cell45 != cell6 )).

fof(tlhfof48741,axiom,(
    cell45 != cell5 )).

fof(tlhfof48742,axiom,(
    cell45 != cell4 )).

fof(tlhfof48743,axiom,(
    cell45 != cell3 )).

fof(tlhfof48744,axiom,(
    cell45 != cell1 )).

fof(tlhfof48745,axiom,(
    cell45 != cell89 )).

fof(tlhfof48746,axiom,(
    cell45 != cell88 )).

fof(tlhfof48747,axiom,(
    cell45 != cell79 )).

fof(tlhfof48748,axiom,(
    cell45 != cell90 )).

fof(tlhfof48749,axiom,(
    cell45 != cell99 )).

fof(tlhfof48750,axiom,(
    cell45 != cell12 )).

fof(tlhfof48751,axiom,(
    cell45 != cell11 )).

fof(tlhfof48752,axiom,(
    cell45 != cell2 )).

fof(tlhfof48753,axiom,(
    cell45 != cell13 )).

fof(tlhfof48754,axiom,(
    cell45 != cell22 )).

fof(tlhfof48755,axiom,(
    cell44 != cell43 )).

fof(tlhfof48756,axiom,(
    cell44 != cell41 )).

fof(tlhfof48757,axiom,(
    cell44 != cell42 )).

fof(tlhfof48758,axiom,(
    cell44 != cell40 )).

fof(tlhfof48759,axiom,(
    cell44 != cell39 )).

fof(tlhfof48760,axiom,(
    cell44 != cell38 )).

fof(tlhfof48761,axiom,(
    cell44 != cell37 )).

fof(tlhfof48762,axiom,(
    cell44 != cell36 )).

fof(tlhfof48763,axiom,(
    cell44 != cell35 )).

fof(tlhfof48764,axiom,(
    cell44 != cell34 )).

fof(tlhfof48765,axiom,(
    cell44 != cell33 )).

fof(tlhfof48766,axiom,(
    cell44 != cell31 )).

fof(tlhfof48767,axiom,(
    cell44 != cell32 )).

fof(tlhfof48768,axiom,(
    cell44 != cell30 )).

fof(tlhfof48769,axiom,(
    cell44 != cell29 )).

fof(tlhfof48770,axiom,(
    cell44 != cell28 )).

fof(tlhfof48771,axiom,(
    cell44 != cell27 )).

fof(tlhfof48772,axiom,(
    cell44 != cell26 )).

fof(tlhfof48773,axiom,(
    cell44 != cell25 )).

fof(tlhfof48774,axiom,(
    cell44 != cell24 )).

fof(tlhfof48775,axiom,(
    cell44 != cell23 )).

fof(tlhfof48776,axiom,(
    cell44 != cell21 )).

fof(tlhfof48777,axiom,(
    cell44 != cell20 )).

fof(tlhfof48778,axiom,(
    cell44 != cell19 )).

fof(tlhfof48779,axiom,(
    cell44 != cell18 )).

fof(tlhfof48780,axiom,(
    cell44 != cell17 )).

fof(tlhfof48781,axiom,(
    cell44 != cell16 )).

fof(tlhfof48782,axiom,(
    cell44 != cell15 )).

fof(tlhfof48783,axiom,(
    cell44 != cell14 )).

fof(tlhfof48784,axiom,(
    cell44 != cell10 )).

fof(tlhfof48785,axiom,(
    cell44 != cell9 )).

fof(tlhfof48786,axiom,(
    cell44 != cell8 )).

fof(tlhfof48787,axiom,(
    cell44 != cell7 )).

fof(tlhfof48788,axiom,(
    cell44 != cell6 )).

fof(tlhfof48789,axiom,(
    cell44 != cell5 )).

fof(tlhfof48790,axiom,(
    cell44 != cell4 )).

fof(tlhfof48791,axiom,(
    cell44 != cell3 )).

fof(tlhfof48792,axiom,(
    cell44 != cell1 )).

fof(tlhfof48793,axiom,(
    cell44 != cell89 )).

fof(tlhfof48794,axiom,(
    cell44 != cell88 )).

fof(tlhfof48795,axiom,(
    cell44 != cell79 )).

fof(tlhfof48796,axiom,(
    cell44 != cell90 )).

fof(tlhfof48797,axiom,(
    cell44 != cell99 )).

fof(tlhfof48798,axiom,(
    cell44 != cell12 )).

fof(tlhfof48799,axiom,(
    cell44 != cell11 )).

fof(tlhfof48800,axiom,(
    cell44 != cell2 )).

fof(tlhfof48801,axiom,(
    cell44 != cell13 )).

fof(tlhfof48802,axiom,(
    cell44 != cell22 )).

fof(tlhfof48803,axiom,(
    cell43 != cell41 )).

fof(tlhfof48804,axiom,(
    cell43 != cell42 )).

fof(tlhfof48805,axiom,(
    cell43 != cell40 )).

fof(tlhfof48806,axiom,(
    cell43 != cell39 )).

fof(tlhfof48807,axiom,(
    cell43 != cell38 )).

fof(tlhfof48808,axiom,(
    cell43 != cell37 )).

fof(tlhfof48809,axiom,(
    cell43 != cell36 )).

fof(tlhfof48810,axiom,(
    cell43 != cell35 )).

fof(tlhfof48811,axiom,(
    cell43 != cell34 )).

fof(tlhfof48812,axiom,(
    cell43 != cell33 )).

fof(tlhfof48813,axiom,(
    cell43 != cell31 )).

fof(tlhfof48814,axiom,(
    cell43 != cell32 )).

fof(tlhfof48815,axiom,(
    cell43 != cell30 )).

fof(tlhfof48816,axiom,(
    cell43 != cell29 )).

fof(tlhfof48817,axiom,(
    cell43 != cell28 )).

fof(tlhfof48818,axiom,(
    cell43 != cell27 )).

fof(tlhfof48819,axiom,(
    cell43 != cell26 )).

fof(tlhfof48820,axiom,(
    cell43 != cell25 )).

fof(tlhfof48821,axiom,(
    cell43 != cell24 )).

fof(tlhfof48822,axiom,(
    cell43 != cell23 )).

fof(tlhfof48823,axiom,(
    cell43 != cell21 )).

fof(tlhfof48824,axiom,(
    cell43 != cell20 )).

fof(tlhfof48825,axiom,(
    cell43 != cell19 )).

fof(tlhfof48826,axiom,(
    cell43 != cell18 )).

fof(tlhfof48827,axiom,(
    cell43 != cell17 )).

fof(tlhfof48828,axiom,(
    cell43 != cell16 )).

fof(tlhfof48829,axiom,(
    cell43 != cell15 )).

fof(tlhfof48830,axiom,(
    cell43 != cell14 )).

fof(tlhfof48831,axiom,(
    cell43 != cell10 )).

fof(tlhfof48832,axiom,(
    cell43 != cell9 )).

fof(tlhfof48833,axiom,(
    cell43 != cell8 )).

fof(tlhfof48834,axiom,(
    cell43 != cell7 )).

fof(tlhfof48835,axiom,(
    cell43 != cell6 )).

fof(tlhfof48836,axiom,(
    cell43 != cell5 )).

fof(tlhfof48837,axiom,(
    cell43 != cell4 )).

fof(tlhfof48838,axiom,(
    cell43 != cell3 )).

fof(tlhfof48839,axiom,(
    cell43 != cell1 )).

fof(tlhfof48840,axiom,(
    cell43 != cell89 )).

fof(tlhfof48841,axiom,(
    cell43 != cell88 )).

fof(tlhfof48842,axiom,(
    cell43 != cell79 )).

fof(tlhfof48843,axiom,(
    cell43 != cell90 )).

fof(tlhfof48844,axiom,(
    cell43 != cell99 )).

fof(tlhfof48845,axiom,(
    cell43 != cell12 )).

fof(tlhfof48846,axiom,(
    cell43 != cell11 )).

fof(tlhfof48847,axiom,(
    cell43 != cell2 )).

fof(tlhfof48848,axiom,(
    cell43 != cell13 )).

fof(tlhfof48849,axiom,(
    cell43 != cell22 )).

fof(tlhfof48850,axiom,(
    cell41 != cell42 )).

fof(tlhfof48851,axiom,(
    cell41 != cell40 )).

fof(tlhfof48852,axiom,(
    cell41 != cell39 )).

fof(tlhfof48853,axiom,(
    cell41 != cell38 )).

fof(tlhfof48854,axiom,(
    cell41 != cell37 )).

fof(tlhfof48855,axiom,(
    cell41 != cell36 )).

fof(tlhfof48856,axiom,(
    cell41 != cell35 )).

fof(tlhfof48857,axiom,(
    cell41 != cell34 )).

fof(tlhfof48858,axiom,(
    cell41 != cell33 )).

fof(tlhfof48859,axiom,(
    cell41 != cell31 )).

fof(tlhfof48860,axiom,(
    cell41 != cell32 )).

fof(tlhfof48861,axiom,(
    cell41 != cell30 )).

fof(tlhfof48862,axiom,(
    cell41 != cell29 )).

fof(tlhfof48863,axiom,(
    cell41 != cell28 )).

fof(tlhfof48864,axiom,(
    cell41 != cell27 )).

fof(tlhfof48865,axiom,(
    cell41 != cell26 )).

fof(tlhfof48866,axiom,(
    cell41 != cell25 )).

fof(tlhfof48867,axiom,(
    cell41 != cell24 )).

fof(tlhfof48868,axiom,(
    cell41 != cell23 )).

fof(tlhfof48869,axiom,(
    cell41 != cell21 )).

fof(tlhfof48870,axiom,(
    cell41 != cell20 )).

fof(tlhfof48871,axiom,(
    cell41 != cell19 )).

fof(tlhfof48872,axiom,(
    cell41 != cell18 )).

fof(tlhfof48873,axiom,(
    cell41 != cell17 )).

fof(tlhfof48874,axiom,(
    cell41 != cell16 )).

fof(tlhfof48875,axiom,(
    cell41 != cell15 )).

fof(tlhfof48876,axiom,(
    cell41 != cell14 )).

fof(tlhfof48877,axiom,(
    cell41 != cell10 )).

fof(tlhfof48878,axiom,(
    cell41 != cell9 )).

fof(tlhfof48879,axiom,(
    cell41 != cell8 )).

fof(tlhfof48880,axiom,(
    cell41 != cell7 )).

fof(tlhfof48881,axiom,(
    cell41 != cell6 )).

fof(tlhfof48882,axiom,(
    cell41 != cell5 )).

fof(tlhfof48883,axiom,(
    cell41 != cell4 )).

fof(tlhfof48884,axiom,(
    cell41 != cell3 )).

fof(tlhfof48885,axiom,(
    cell41 != cell1 )).

fof(tlhfof48886,axiom,(
    cell41 != cell89 )).

fof(tlhfof48887,axiom,(
    cell41 != cell88 )).

fof(tlhfof48888,axiom,(
    cell41 != cell79 )).

fof(tlhfof48889,axiom,(
    cell41 != cell90 )).

fof(tlhfof48890,axiom,(
    cell41 != cell99 )).

fof(tlhfof48891,axiom,(
    cell41 != cell12 )).

fof(tlhfof48892,axiom,(
    cell41 != cell11 )).

fof(tlhfof48893,axiom,(
    cell41 != cell2 )).

fof(tlhfof48894,axiom,(
    cell41 != cell13 )).

fof(tlhfof48895,axiom,(
    cell41 != cell22 )).

fof(tlhfof48896,axiom,(
    cell42 != cell40 )).

fof(tlhfof48897,axiom,(
    cell42 != cell39 )).

fof(tlhfof48898,axiom,(
    cell42 != cell38 )).

fof(tlhfof48899,axiom,(
    cell42 != cell37 )).

fof(tlhfof48900,axiom,(
    cell42 != cell36 )).

fof(tlhfof48901,axiom,(
    cell42 != cell35 )).

fof(tlhfof48902,axiom,(
    cell42 != cell34 )).

fof(tlhfof48903,axiom,(
    cell42 != cell33 )).

fof(tlhfof48904,axiom,(
    cell42 != cell31 )).

fof(tlhfof48905,axiom,(
    cell42 != cell32 )).

fof(tlhfof48906,axiom,(
    cell42 != cell30 )).

fof(tlhfof48907,axiom,(
    cell42 != cell29 )).

fof(tlhfof48908,axiom,(
    cell42 != cell28 )).

fof(tlhfof48909,axiom,(
    cell42 != cell27 )).

fof(tlhfof48910,axiom,(
    cell42 != cell26 )).

fof(tlhfof48911,axiom,(
    cell42 != cell25 )).

fof(tlhfof48912,axiom,(
    cell42 != cell24 )).

fof(tlhfof48913,axiom,(
    cell42 != cell23 )).

fof(tlhfof48914,axiom,(
    cell42 != cell21 )).

fof(tlhfof48915,axiom,(
    cell42 != cell20 )).

fof(tlhfof48916,axiom,(
    cell42 != cell19 )).

fof(tlhfof48917,axiom,(
    cell42 != cell18 )).

fof(tlhfof48918,axiom,(
    cell42 != cell17 )).

fof(tlhfof48919,axiom,(
    cell42 != cell16 )).

fof(tlhfof48920,axiom,(
    cell42 != cell15 )).

fof(tlhfof48921,axiom,(
    cell42 != cell14 )).

fof(tlhfof48922,axiom,(
    cell42 != cell10 )).

fof(tlhfof48923,axiom,(
    cell42 != cell9 )).

fof(tlhfof48924,axiom,(
    cell42 != cell8 )).

fof(tlhfof48925,axiom,(
    cell42 != cell7 )).

fof(tlhfof48926,axiom,(
    cell42 != cell6 )).

fof(tlhfof48927,axiom,(
    cell42 != cell5 )).

fof(tlhfof48928,axiom,(
    cell42 != cell4 )).

fof(tlhfof48929,axiom,(
    cell42 != cell3 )).

fof(tlhfof48930,axiom,(
    cell42 != cell1 )).

fof(tlhfof48931,axiom,(
    cell42 != cell89 )).

fof(tlhfof48932,axiom,(
    cell42 != cell88 )).

fof(tlhfof48933,axiom,(
    cell42 != cell79 )).

fof(tlhfof48934,axiom,(
    cell42 != cell90 )).

fof(tlhfof48935,axiom,(
    cell42 != cell99 )).

fof(tlhfof48936,axiom,(
    cell42 != cell12 )).

fof(tlhfof48937,axiom,(
    cell42 != cell11 )).

fof(tlhfof48938,axiom,(
    cell42 != cell2 )).

fof(tlhfof48939,axiom,(
    cell42 != cell13 )).

fof(tlhfof48940,axiom,(
    cell42 != cell22 )).

fof(tlhfof48941,axiom,(
    cell40 != cell39 )).

fof(tlhfof48942,axiom,(
    cell40 != cell38 )).

fof(tlhfof48943,axiom,(
    cell40 != cell37 )).

fof(tlhfof48944,axiom,(
    cell40 != cell36 )).

fof(tlhfof48945,axiom,(
    cell40 != cell35 )).

fof(tlhfof48946,axiom,(
    cell40 != cell34 )).

fof(tlhfof48947,axiom,(
    cell40 != cell33 )).

fof(tlhfof48948,axiom,(
    cell40 != cell31 )).

fof(tlhfof48949,axiom,(
    cell40 != cell32 )).

fof(tlhfof48950,axiom,(
    cell40 != cell30 )).

fof(tlhfof48951,axiom,(
    cell40 != cell29 )).

fof(tlhfof48952,axiom,(
    cell40 != cell28 )).

fof(tlhfof48953,axiom,(
    cell40 != cell27 )).

fof(tlhfof48954,axiom,(
    cell40 != cell26 )).

fof(tlhfof48955,axiom,(
    cell40 != cell25 )).

fof(tlhfof48956,axiom,(
    cell40 != cell24 )).

fof(tlhfof48957,axiom,(
    cell40 != cell23 )).

fof(tlhfof48958,axiom,(
    cell40 != cell21 )).

fof(tlhfof48959,axiom,(
    cell40 != cell20 )).

fof(tlhfof48960,axiom,(
    cell40 != cell19 )).

fof(tlhfof48961,axiom,(
    cell40 != cell18 )).

fof(tlhfof48962,axiom,(
    cell40 != cell17 )).

fof(tlhfof48963,axiom,(
    cell40 != cell16 )).

fof(tlhfof48964,axiom,(
    cell40 != cell15 )).

fof(tlhfof48965,axiom,(
    cell40 != cell14 )).

fof(tlhfof48966,axiom,(
    cell40 != cell10 )).

fof(tlhfof48967,axiom,(
    cell40 != cell9 )).

fof(tlhfof48968,axiom,(
    cell40 != cell8 )).

fof(tlhfof48969,axiom,(
    cell40 != cell7 )).

fof(tlhfof48970,axiom,(
    cell40 != cell6 )).

fof(tlhfof48971,axiom,(
    cell40 != cell5 )).

fof(tlhfof48972,axiom,(
    cell40 != cell4 )).

fof(tlhfof48973,axiom,(
    cell40 != cell3 )).

fof(tlhfof48974,axiom,(
    cell40 != cell1 )).

fof(tlhfof48975,axiom,(
    cell40 != cell89 )).

fof(tlhfof48976,axiom,(
    cell40 != cell88 )).

fof(tlhfof48977,axiom,(
    cell40 != cell79 )).

fof(tlhfof48978,axiom,(
    cell40 != cell90 )).

fof(tlhfof48979,axiom,(
    cell40 != cell99 )).

fof(tlhfof48980,axiom,(
    cell40 != cell12 )).

fof(tlhfof48981,axiom,(
    cell40 != cell11 )).

fof(tlhfof48982,axiom,(
    cell40 != cell2 )).

fof(tlhfof48983,axiom,(
    cell40 != cell13 )).

fof(tlhfof48984,axiom,(
    cell40 != cell22 )).

fof(tlhfof48985,axiom,(
    cell39 != cell38 )).

fof(tlhfof48986,axiom,(
    cell39 != cell37 )).

fof(tlhfof48987,axiom,(
    cell39 != cell36 )).

fof(tlhfof48988,axiom,(
    cell39 != cell35 )).

fof(tlhfof48989,axiom,(
    cell39 != cell34 )).

fof(tlhfof48990,axiom,(
    cell39 != cell33 )).

fof(tlhfof48991,axiom,(
    cell39 != cell31 )).

fof(tlhfof48992,axiom,(
    cell39 != cell32 )).

fof(tlhfof48993,axiom,(
    cell39 != cell30 )).

fof(tlhfof48994,axiom,(
    cell39 != cell29 )).

fof(tlhfof48995,axiom,(
    cell39 != cell28 )).

fof(tlhfof48996,axiom,(
    cell39 != cell27 )).

fof(tlhfof48997,axiom,(
    cell39 != cell26 )).

fof(tlhfof48998,axiom,(
    cell39 != cell25 )).

fof(tlhfof48999,axiom,(
    cell39 != cell24 )).

fof(tlhfof49000,axiom,(
    cell39 != cell23 )).

fof(tlhfof49001,axiom,(
    cell39 != cell21 )).

fof(tlhfof49002,axiom,(
    cell39 != cell20 )).

fof(tlhfof49003,axiom,(
    cell39 != cell19 )).

fof(tlhfof49004,axiom,(
    cell39 != cell18 )).

fof(tlhfof49005,axiom,(
    cell39 != cell17 )).

fof(tlhfof49006,axiom,(
    cell39 != cell16 )).

fof(tlhfof49007,axiom,(
    cell39 != cell15 )).

fof(tlhfof49008,axiom,(
    cell39 != cell14 )).

fof(tlhfof49009,axiom,(
    cell39 != cell10 )).

fof(tlhfof49010,axiom,(
    cell39 != cell9 )).

fof(tlhfof49011,axiom,(
    cell39 != cell8 )).

fof(tlhfof49012,axiom,(
    cell39 != cell7 )).

fof(tlhfof49013,axiom,(
    cell39 != cell6 )).

fof(tlhfof49014,axiom,(
    cell39 != cell5 )).

fof(tlhfof49015,axiom,(
    cell39 != cell4 )).

fof(tlhfof49016,axiom,(
    cell39 != cell3 )).

fof(tlhfof49017,axiom,(
    cell39 != cell1 )).

fof(tlhfof49018,axiom,(
    cell39 != cell89 )).

fof(tlhfof49019,axiom,(
    cell39 != cell88 )).

fof(tlhfof49020,axiom,(
    cell39 != cell79 )).

fof(tlhfof49021,axiom,(
    cell39 != cell90 )).

fof(tlhfof49022,axiom,(
    cell39 != cell99 )).

fof(tlhfof49023,axiom,(
    cell39 != cell12 )).

fof(tlhfof49024,axiom,(
    cell39 != cell11 )).

fof(tlhfof49025,axiom,(
    cell39 != cell2 )).

fof(tlhfof49026,axiom,(
    cell39 != cell13 )).

fof(tlhfof49027,axiom,(
    cell39 != cell22 )).

fof(tlhfof49028,axiom,(
    cell38 != cell37 )).

fof(tlhfof49029,axiom,(
    cell38 != cell36 )).

fof(tlhfof49030,axiom,(
    cell38 != cell35 )).

fof(tlhfof49031,axiom,(
    cell38 != cell34 )).

fof(tlhfof49032,axiom,(
    cell38 != cell33 )).

fof(tlhfof49033,axiom,(
    cell38 != cell31 )).

fof(tlhfof49034,axiom,(
    cell38 != cell32 )).

fof(tlhfof49035,axiom,(
    cell38 != cell30 )).

fof(tlhfof49036,axiom,(
    cell38 != cell29 )).

fof(tlhfof49037,axiom,(
    cell38 != cell28 )).

fof(tlhfof49038,axiom,(
    cell38 != cell27 )).

fof(tlhfof49039,axiom,(
    cell38 != cell26 )).

fof(tlhfof49040,axiom,(
    cell38 != cell25 )).

fof(tlhfof49041,axiom,(
    cell38 != cell24 )).

fof(tlhfof49042,axiom,(
    cell38 != cell23 )).

fof(tlhfof49043,axiom,(
    cell38 != cell21 )).

fof(tlhfof49044,axiom,(
    cell38 != cell20 )).

fof(tlhfof49045,axiom,(
    cell38 != cell19 )).

fof(tlhfof49046,axiom,(
    cell38 != cell18 )).

fof(tlhfof49047,axiom,(
    cell38 != cell17 )).

fof(tlhfof49048,axiom,(
    cell38 != cell16 )).

fof(tlhfof49049,axiom,(
    cell38 != cell15 )).

fof(tlhfof49050,axiom,(
    cell38 != cell14 )).

fof(tlhfof49051,axiom,(
    cell38 != cell10 )).

fof(tlhfof49052,axiom,(
    cell38 != cell9 )).

fof(tlhfof49053,axiom,(
    cell38 != cell8 )).

fof(tlhfof49054,axiom,(
    cell38 != cell7 )).

fof(tlhfof49055,axiom,(
    cell38 != cell6 )).

fof(tlhfof49056,axiom,(
    cell38 != cell5 )).

fof(tlhfof49057,axiom,(
    cell38 != cell4 )).

fof(tlhfof49058,axiom,(
    cell38 != cell3 )).

fof(tlhfof49059,axiom,(
    cell38 != cell1 )).

fof(tlhfof49060,axiom,(
    cell38 != cell89 )).

fof(tlhfof49061,axiom,(
    cell38 != cell88 )).

fof(tlhfof49062,axiom,(
    cell38 != cell79 )).

fof(tlhfof49063,axiom,(
    cell38 != cell90 )).

fof(tlhfof49064,axiom,(
    cell38 != cell99 )).

fof(tlhfof49065,axiom,(
    cell38 != cell12 )).

fof(tlhfof49066,axiom,(
    cell38 != cell11 )).

fof(tlhfof49067,axiom,(
    cell38 != cell2 )).

fof(tlhfof49068,axiom,(
    cell38 != cell13 )).

fof(tlhfof49069,axiom,(
    cell38 != cell22 )).

fof(tlhfof49070,axiom,(
    cell37 != cell36 )).

fof(tlhfof49071,axiom,(
    cell37 != cell35 )).

fof(tlhfof49072,axiom,(
    cell37 != cell34 )).

fof(tlhfof49073,axiom,(
    cell37 != cell33 )).

fof(tlhfof49074,axiom,(
    cell37 != cell31 )).

fof(tlhfof49075,axiom,(
    cell37 != cell32 )).

fof(tlhfof49076,axiom,(
    cell37 != cell30 )).

fof(tlhfof49077,axiom,(
    cell37 != cell29 )).

fof(tlhfof49078,axiom,(
    cell37 != cell28 )).

fof(tlhfof49079,axiom,(
    cell37 != cell27 )).

fof(tlhfof49080,axiom,(
    cell37 != cell26 )).

fof(tlhfof49081,axiom,(
    cell37 != cell25 )).

fof(tlhfof49082,axiom,(
    cell37 != cell24 )).

fof(tlhfof49083,axiom,(
    cell37 != cell23 )).

fof(tlhfof49084,axiom,(
    cell37 != cell21 )).

fof(tlhfof49085,axiom,(
    cell37 != cell20 )).

fof(tlhfof49086,axiom,(
    cell37 != cell19 )).

fof(tlhfof49087,axiom,(
    cell37 != cell18 )).

fof(tlhfof49088,axiom,(
    cell37 != cell17 )).

fof(tlhfof49089,axiom,(
    cell37 != cell16 )).

fof(tlhfof49090,axiom,(
    cell37 != cell15 )).

fof(tlhfof49091,axiom,(
    cell37 != cell14 )).

fof(tlhfof49092,axiom,(
    cell37 != cell10 )).

fof(tlhfof49093,axiom,(
    cell37 != cell9 )).

fof(tlhfof49094,axiom,(
    cell37 != cell8 )).

fof(tlhfof49095,axiom,(
    cell37 != cell7 )).

fof(tlhfof49096,axiom,(
    cell37 != cell6 )).

fof(tlhfof49097,axiom,(
    cell37 != cell5 )).

fof(tlhfof49098,axiom,(
    cell37 != cell4 )).

fof(tlhfof49099,axiom,(
    cell37 != cell3 )).

fof(tlhfof49100,axiom,(
    cell37 != cell1 )).

fof(tlhfof49101,axiom,(
    cell37 != cell89 )).

fof(tlhfof49102,axiom,(
    cell37 != cell88 )).

fof(tlhfof49103,axiom,(
    cell37 != cell79 )).

fof(tlhfof49104,axiom,(
    cell37 != cell90 )).

fof(tlhfof49105,axiom,(
    cell37 != cell99 )).

fof(tlhfof49106,axiom,(
    cell37 != cell12 )).

fof(tlhfof49107,axiom,(
    cell37 != cell11 )).

fof(tlhfof49108,axiom,(
    cell37 != cell2 )).

fof(tlhfof49109,axiom,(
    cell37 != cell13 )).

fof(tlhfof49110,axiom,(
    cell37 != cell22 )).

fof(tlhfof49111,axiom,(
    cell36 != cell35 )).

fof(tlhfof49112,axiom,(
    cell36 != cell34 )).

fof(tlhfof49113,axiom,(
    cell36 != cell33 )).

fof(tlhfof49114,axiom,(
    cell36 != cell31 )).

fof(tlhfof49115,axiom,(
    cell36 != cell32 )).

fof(tlhfof49116,axiom,(
    cell36 != cell30 )).

fof(tlhfof49117,axiom,(
    cell36 != cell29 )).

fof(tlhfof49118,axiom,(
    cell36 != cell28 )).

fof(tlhfof49119,axiom,(
    cell36 != cell27 )).

fof(tlhfof49120,axiom,(
    cell36 != cell26 )).

fof(tlhfof49121,axiom,(
    cell36 != cell25 )).

fof(tlhfof49122,axiom,(
    cell36 != cell24 )).

fof(tlhfof49123,axiom,(
    cell36 != cell23 )).

fof(tlhfof49124,axiom,(
    cell36 != cell21 )).

fof(tlhfof49125,axiom,(
    cell36 != cell20 )).

fof(tlhfof49126,axiom,(
    cell36 != cell19 )).

fof(tlhfof49127,axiom,(
    cell36 != cell18 )).

fof(tlhfof49128,axiom,(
    cell36 != cell17 )).

fof(tlhfof49129,axiom,(
    cell36 != cell16 )).

fof(tlhfof49130,axiom,(
    cell36 != cell15 )).

fof(tlhfof49131,axiom,(
    cell36 != cell14 )).

fof(tlhfof49132,axiom,(
    cell36 != cell10 )).

fof(tlhfof49133,axiom,(
    cell36 != cell9 )).

fof(tlhfof49134,axiom,(
    cell36 != cell8 )).

fof(tlhfof49135,axiom,(
    cell36 != cell7 )).

fof(tlhfof49136,axiom,(
    cell36 != cell6 )).

fof(tlhfof49137,axiom,(
    cell36 != cell5 )).

fof(tlhfof49138,axiom,(
    cell36 != cell4 )).

fof(tlhfof49139,axiom,(
    cell36 != cell3 )).

fof(tlhfof49140,axiom,(
    cell36 != cell1 )).

fof(tlhfof49141,axiom,(
    cell36 != cell89 )).

fof(tlhfof49142,axiom,(
    cell36 != cell88 )).

fof(tlhfof49143,axiom,(
    cell36 != cell79 )).

fof(tlhfof49144,axiom,(
    cell36 != cell90 )).

fof(tlhfof49145,axiom,(
    cell36 != cell99 )).

fof(tlhfof49146,axiom,(
    cell36 != cell12 )).

fof(tlhfof49147,axiom,(
    cell36 != cell11 )).

fof(tlhfof49148,axiom,(
    cell36 != cell2 )).

fof(tlhfof49149,axiom,(
    cell36 != cell13 )).

fof(tlhfof49150,axiom,(
    cell36 != cell22 )).

fof(tlhfof49151,axiom,(
    cell35 != cell34 )).

fof(tlhfof49152,axiom,(
    cell35 != cell33 )).

fof(tlhfof49153,axiom,(
    cell35 != cell31 )).

fof(tlhfof49154,axiom,(
    cell35 != cell32 )).

fof(tlhfof49155,axiom,(
    cell35 != cell30 )).

fof(tlhfof49156,axiom,(
    cell35 != cell29 )).

fof(tlhfof49157,axiom,(
    cell35 != cell28 )).

fof(tlhfof49158,axiom,(
    cell35 != cell27 )).

fof(tlhfof49159,axiom,(
    cell35 != cell26 )).

fof(tlhfof49160,axiom,(
    cell35 != cell25 )).

fof(tlhfof49161,axiom,(
    cell35 != cell24 )).

fof(tlhfof49162,axiom,(
    cell35 != cell23 )).

fof(tlhfof49163,axiom,(
    cell35 != cell21 )).

fof(tlhfof49164,axiom,(
    cell35 != cell20 )).

fof(tlhfof49165,axiom,(
    cell35 != cell19 )).

fof(tlhfof49166,axiom,(
    cell35 != cell18 )).

fof(tlhfof49167,axiom,(
    cell35 != cell17 )).

fof(tlhfof49168,axiom,(
    cell35 != cell16 )).

fof(tlhfof49169,axiom,(
    cell35 != cell15 )).

fof(tlhfof49170,axiom,(
    cell35 != cell14 )).

fof(tlhfof49171,axiom,(
    cell35 != cell10 )).

fof(tlhfof49172,axiom,(
    cell35 != cell9 )).

fof(tlhfof49173,axiom,(
    cell35 != cell8 )).

fof(tlhfof49174,axiom,(
    cell35 != cell7 )).

fof(tlhfof49175,axiom,(
    cell35 != cell6 )).

fof(tlhfof49176,axiom,(
    cell35 != cell5 )).

fof(tlhfof49177,axiom,(
    cell35 != cell4 )).

fof(tlhfof49178,axiom,(
    cell35 != cell3 )).

fof(tlhfof49179,axiom,(
    cell35 != cell1 )).

fof(tlhfof49180,axiom,(
    cell35 != cell89 )).

fof(tlhfof49181,axiom,(
    cell35 != cell88 )).

fof(tlhfof49182,axiom,(
    cell35 != cell79 )).

fof(tlhfof49183,axiom,(
    cell35 != cell90 )).

fof(tlhfof49184,axiom,(
    cell35 != cell99 )).

fof(tlhfof49185,axiom,(
    cell35 != cell12 )).

fof(tlhfof49186,axiom,(
    cell35 != cell11 )).

fof(tlhfof49187,axiom,(
    cell35 != cell2 )).

fof(tlhfof49188,axiom,(
    cell35 != cell13 )).

fof(tlhfof49189,axiom,(
    cell35 != cell22 )).

fof(tlhfof49190,axiom,(
    cell34 != cell33 )).

fof(tlhfof49191,axiom,(
    cell34 != cell31 )).

fof(tlhfof49192,axiom,(
    cell34 != cell32 )).

fof(tlhfof49193,axiom,(
    cell34 != cell30 )).

fof(tlhfof49194,axiom,(
    cell34 != cell29 )).

fof(tlhfof49195,axiom,(
    cell34 != cell28 )).

fof(tlhfof49196,axiom,(
    cell34 != cell27 )).

fof(tlhfof49197,axiom,(
    cell34 != cell26 )).

fof(tlhfof49198,axiom,(
    cell34 != cell25 )).

fof(tlhfof49199,axiom,(
    cell34 != cell24 )).

fof(tlhfof49200,axiom,(
    cell34 != cell23 )).

fof(tlhfof49201,axiom,(
    cell34 != cell21 )).

fof(tlhfof49202,axiom,(
    cell34 != cell20 )).

fof(tlhfof49203,axiom,(
    cell34 != cell19 )).

fof(tlhfof49204,axiom,(
    cell34 != cell18 )).

fof(tlhfof49205,axiom,(
    cell34 != cell17 )).

fof(tlhfof49206,axiom,(
    cell34 != cell16 )).

fof(tlhfof49207,axiom,(
    cell34 != cell15 )).

fof(tlhfof49208,axiom,(
    cell34 != cell14 )).

fof(tlhfof49209,axiom,(
    cell34 != cell10 )).

fof(tlhfof49210,axiom,(
    cell34 != cell9 )).

fof(tlhfof49211,axiom,(
    cell34 != cell8 )).

fof(tlhfof49212,axiom,(
    cell34 != cell7 )).

fof(tlhfof49213,axiom,(
    cell34 != cell6 )).

fof(tlhfof49214,axiom,(
    cell34 != cell5 )).

fof(tlhfof49215,axiom,(
    cell34 != cell4 )).

fof(tlhfof49216,axiom,(
    cell34 != cell3 )).

fof(tlhfof49217,axiom,(
    cell34 != cell1 )).

fof(tlhfof49218,axiom,(
    cell34 != cell89 )).

fof(tlhfof49219,axiom,(
    cell34 != cell88 )).

fof(tlhfof49220,axiom,(
    cell34 != cell79 )).

fof(tlhfof49221,axiom,(
    cell34 != cell90 )).

fof(tlhfof49222,axiom,(
    cell34 != cell99 )).

fof(tlhfof49223,axiom,(
    cell34 != cell12 )).

fof(tlhfof49224,axiom,(
    cell34 != cell11 )).

fof(tlhfof49225,axiom,(
    cell34 != cell2 )).

fof(tlhfof49226,axiom,(
    cell34 != cell13 )).

fof(tlhfof49227,axiom,(
    cell34 != cell22 )).

fof(tlhfof49228,axiom,(
    cell33 != cell31 )).

fof(tlhfof49229,axiom,(
    cell33 != cell32 )).

fof(tlhfof49230,axiom,(
    cell33 != cell30 )).

fof(tlhfof49231,axiom,(
    cell33 != cell29 )).

fof(tlhfof49232,axiom,(
    cell33 != cell28 )).

fof(tlhfof49233,axiom,(
    cell33 != cell27 )).

fof(tlhfof49234,axiom,(
    cell33 != cell26 )).

fof(tlhfof49235,axiom,(
    cell33 != cell25 )).

fof(tlhfof49236,axiom,(
    cell33 != cell24 )).

fof(tlhfof49237,axiom,(
    cell33 != cell23 )).

fof(tlhfof49238,axiom,(
    cell33 != cell21 )).

fof(tlhfof49239,axiom,(
    cell33 != cell20 )).

fof(tlhfof49240,axiom,(
    cell33 != cell19 )).

fof(tlhfof49241,axiom,(
    cell33 != cell18 )).

fof(tlhfof49242,axiom,(
    cell33 != cell17 )).

fof(tlhfof49243,axiom,(
    cell33 != cell16 )).

fof(tlhfof49244,axiom,(
    cell33 != cell15 )).

fof(tlhfof49245,axiom,(
    cell33 != cell14 )).

fof(tlhfof49246,axiom,(
    cell33 != cell10 )).

fof(tlhfof49247,axiom,(
    cell33 != cell9 )).

fof(tlhfof49248,axiom,(
    cell33 != cell8 )).

fof(tlhfof49249,axiom,(
    cell33 != cell7 )).

fof(tlhfof49250,axiom,(
    cell33 != cell6 )).

fof(tlhfof49251,axiom,(
    cell33 != cell5 )).

fof(tlhfof49252,axiom,(
    cell33 != cell4 )).

fof(tlhfof49253,axiom,(
    cell33 != cell3 )).

fof(tlhfof49254,axiom,(
    cell33 != cell1 )).

fof(tlhfof49255,axiom,(
    cell33 != cell89 )).

fof(tlhfof49256,axiom,(
    cell33 != cell88 )).

fof(tlhfof49257,axiom,(
    cell33 != cell79 )).

fof(tlhfof49258,axiom,(
    cell33 != cell90 )).

fof(tlhfof49259,axiom,(
    cell33 != cell99 )).

fof(tlhfof49260,axiom,(
    cell33 != cell12 )).

fof(tlhfof49261,axiom,(
    cell33 != cell11 )).

fof(tlhfof49262,axiom,(
    cell33 != cell2 )).

fof(tlhfof49263,axiom,(
    cell33 != cell13 )).

fof(tlhfof49264,axiom,(
    cell33 != cell22 )).

fof(tlhfof49265,axiom,(
    cell31 != cell32 )).

fof(tlhfof49266,axiom,(
    cell31 != cell30 )).

fof(tlhfof49267,axiom,(
    cell31 != cell29 )).

fof(tlhfof49268,axiom,(
    cell31 != cell28 )).

fof(tlhfof49269,axiom,(
    cell31 != cell27 )).

fof(tlhfof49270,axiom,(
    cell31 != cell26 )).

fof(tlhfof49271,axiom,(
    cell31 != cell25 )).

fof(tlhfof49272,axiom,(
    cell31 != cell24 )).

fof(tlhfof49273,axiom,(
    cell31 != cell23 )).

fof(tlhfof49274,axiom,(
    cell31 != cell21 )).

fof(tlhfof49275,axiom,(
    cell31 != cell20 )).

fof(tlhfof49276,axiom,(
    cell31 != cell19 )).

fof(tlhfof49277,axiom,(
    cell31 != cell18 )).

fof(tlhfof49278,axiom,(
    cell31 != cell17 )).

fof(tlhfof49279,axiom,(
    cell31 != cell16 )).

fof(tlhfof49280,axiom,(
    cell31 != cell15 )).

fof(tlhfof49281,axiom,(
    cell31 != cell14 )).

fof(tlhfof49282,axiom,(
    cell31 != cell10 )).

fof(tlhfof49283,axiom,(
    cell31 != cell9 )).

fof(tlhfof49284,axiom,(
    cell31 != cell8 )).

fof(tlhfof49285,axiom,(
    cell31 != cell7 )).

fof(tlhfof49286,axiom,(
    cell31 != cell6 )).

fof(tlhfof49287,axiom,(
    cell31 != cell5 )).

fof(tlhfof49288,axiom,(
    cell31 != cell4 )).

fof(tlhfof49289,axiom,(
    cell31 != cell3 )).

fof(tlhfof49290,axiom,(
    cell31 != cell1 )).

fof(tlhfof49291,axiom,(
    cell31 != cell89 )).

fof(tlhfof49292,axiom,(
    cell31 != cell88 )).

fof(tlhfof49293,axiom,(
    cell31 != cell79 )).

fof(tlhfof49294,axiom,(
    cell31 != cell90 )).

fof(tlhfof49295,axiom,(
    cell31 != cell99 )).

fof(tlhfof49296,axiom,(
    cell31 != cell12 )).

fof(tlhfof49297,axiom,(
    cell31 != cell11 )).

fof(tlhfof49298,axiom,(
    cell31 != cell2 )).

fof(tlhfof49299,axiom,(
    cell31 != cell13 )).

fof(tlhfof49300,axiom,(
    cell31 != cell22 )).

fof(tlhfof49301,axiom,(
    cell32 != cell30 )).

fof(tlhfof49302,axiom,(
    cell32 != cell29 )).

fof(tlhfof49303,axiom,(
    cell32 != cell28 )).

fof(tlhfof49304,axiom,(
    cell32 != cell27 )).

fof(tlhfof49305,axiom,(
    cell32 != cell26 )).

fof(tlhfof49306,axiom,(
    cell32 != cell25 )).

fof(tlhfof49307,axiom,(
    cell32 != cell24 )).

fof(tlhfof49308,axiom,(
    cell32 != cell23 )).

fof(tlhfof49309,axiom,(
    cell32 != cell21 )).

fof(tlhfof49310,axiom,(
    cell32 != cell20 )).

fof(tlhfof49311,axiom,(
    cell32 != cell19 )).

fof(tlhfof49312,axiom,(
    cell32 != cell18 )).

fof(tlhfof49313,axiom,(
    cell32 != cell17 )).

fof(tlhfof49314,axiom,(
    cell32 != cell16 )).

fof(tlhfof49315,axiom,(
    cell32 != cell15 )).

fof(tlhfof49316,axiom,(
    cell32 != cell14 )).

fof(tlhfof49317,axiom,(
    cell32 != cell10 )).

fof(tlhfof49318,axiom,(
    cell32 != cell9 )).

fof(tlhfof49319,axiom,(
    cell32 != cell8 )).

fof(tlhfof49320,axiom,(
    cell32 != cell7 )).

fof(tlhfof49321,axiom,(
    cell32 != cell6 )).

fof(tlhfof49322,axiom,(
    cell32 != cell5 )).

fof(tlhfof49323,axiom,(
    cell32 != cell4 )).

fof(tlhfof49324,axiom,(
    cell32 != cell3 )).

fof(tlhfof49325,axiom,(
    cell32 != cell1 )).

fof(tlhfof49326,axiom,(
    cell32 != cell89 )).

fof(tlhfof49327,axiom,(
    cell32 != cell88 )).

fof(tlhfof49328,axiom,(
    cell32 != cell79 )).

fof(tlhfof49329,axiom,(
    cell32 != cell90 )).

fof(tlhfof49330,axiom,(
    cell32 != cell99 )).

fof(tlhfof49331,axiom,(
    cell32 != cell12 )).

fof(tlhfof49332,axiom,(
    cell32 != cell11 )).

fof(tlhfof49333,axiom,(
    cell32 != cell2 )).

fof(tlhfof49334,axiom,(
    cell32 != cell13 )).

fof(tlhfof49335,axiom,(
    cell32 != cell22 )).

fof(tlhfof49336,axiom,(
    cell30 != cell29 )).

fof(tlhfof49337,axiom,(
    cell30 != cell28 )).

fof(tlhfof49338,axiom,(
    cell30 != cell27 )).

fof(tlhfof49339,axiom,(
    cell30 != cell26 )).

fof(tlhfof49340,axiom,(
    cell30 != cell25 )).

fof(tlhfof49341,axiom,(
    cell30 != cell24 )).

fof(tlhfof49342,axiom,(
    cell30 != cell23 )).

fof(tlhfof49343,axiom,(
    cell30 != cell21 )).

fof(tlhfof49344,axiom,(
    cell30 != cell20 )).

fof(tlhfof49345,axiom,(
    cell30 != cell19 )).

fof(tlhfof49346,axiom,(
    cell30 != cell18 )).

fof(tlhfof49347,axiom,(
    cell30 != cell17 )).

fof(tlhfof49348,axiom,(
    cell30 != cell16 )).

fof(tlhfof49349,axiom,(
    cell30 != cell15 )).

fof(tlhfof49350,axiom,(
    cell30 != cell14 )).

fof(tlhfof49351,axiom,(
    cell30 != cell10 )).

fof(tlhfof49352,axiom,(
    cell30 != cell9 )).

fof(tlhfof49353,axiom,(
    cell30 != cell8 )).

fof(tlhfof49354,axiom,(
    cell30 != cell7 )).

fof(tlhfof49355,axiom,(
    cell30 != cell6 )).

fof(tlhfof49356,axiom,(
    cell30 != cell5 )).

fof(tlhfof49357,axiom,(
    cell30 != cell4 )).

fof(tlhfof49358,axiom,(
    cell30 != cell3 )).

fof(tlhfof49359,axiom,(
    cell30 != cell1 )).

fof(tlhfof49360,axiom,(
    cell30 != cell89 )).

fof(tlhfof49361,axiom,(
    cell30 != cell88 )).

fof(tlhfof49362,axiom,(
    cell30 != cell79 )).

fof(tlhfof49363,axiom,(
    cell30 != cell90 )).

fof(tlhfof49364,axiom,(
    cell30 != cell99 )).

fof(tlhfof49365,axiom,(
    cell30 != cell12 )).

fof(tlhfof49366,axiom,(
    cell30 != cell11 )).

fof(tlhfof49367,axiom,(
    cell30 != cell2 )).

fof(tlhfof49368,axiom,(
    cell30 != cell13 )).

fof(tlhfof49369,axiom,(
    cell30 != cell22 )).

fof(tlhfof49370,axiom,(
    cell29 != cell28 )).

fof(tlhfof49371,axiom,(
    cell29 != cell27 )).

fof(tlhfof49372,axiom,(
    cell29 != cell26 )).

fof(tlhfof49373,axiom,(
    cell29 != cell25 )).

fof(tlhfof49374,axiom,(
    cell29 != cell24 )).

fof(tlhfof49375,axiom,(
    cell29 != cell23 )).

fof(tlhfof49376,axiom,(
    cell29 != cell21 )).

fof(tlhfof49377,axiom,(
    cell29 != cell20 )).

fof(tlhfof49378,axiom,(
    cell29 != cell19 )).

fof(tlhfof49379,axiom,(
    cell29 != cell18 )).

fof(tlhfof49380,axiom,(
    cell29 != cell17 )).

fof(tlhfof49381,axiom,(
    cell29 != cell16 )).

fof(tlhfof49382,axiom,(
    cell29 != cell15 )).

fof(tlhfof49383,axiom,(
    cell29 != cell14 )).

fof(tlhfof49384,axiom,(
    cell29 != cell10 )).

fof(tlhfof49385,axiom,(
    cell29 != cell9 )).

fof(tlhfof49386,axiom,(
    cell29 != cell8 )).

fof(tlhfof49387,axiom,(
    cell29 != cell7 )).

fof(tlhfof49388,axiom,(
    cell29 != cell6 )).

fof(tlhfof49389,axiom,(
    cell29 != cell5 )).

fof(tlhfof49390,axiom,(
    cell29 != cell4 )).

fof(tlhfof49391,axiom,(
    cell29 != cell3 )).

fof(tlhfof49392,axiom,(
    cell29 != cell1 )).

fof(tlhfof49393,axiom,(
    cell29 != cell89 )).

fof(tlhfof49394,axiom,(
    cell29 != cell88 )).

fof(tlhfof49395,axiom,(
    cell29 != cell79 )).

fof(tlhfof49396,axiom,(
    cell29 != cell90 )).

fof(tlhfof49397,axiom,(
    cell29 != cell99 )).

fof(tlhfof49398,axiom,(
    cell29 != cell12 )).

fof(tlhfof49399,axiom,(
    cell29 != cell11 )).

fof(tlhfof49400,axiom,(
    cell29 != cell2 )).

fof(tlhfof49401,axiom,(
    cell29 != cell13 )).

fof(tlhfof49402,axiom,(
    cell29 != cell22 )).

fof(tlhfof49403,axiom,(
    cell28 != cell27 )).

fof(tlhfof49404,axiom,(
    cell28 != cell26 )).

fof(tlhfof49405,axiom,(
    cell28 != cell25 )).

fof(tlhfof49406,axiom,(
    cell28 != cell24 )).

fof(tlhfof49407,axiom,(
    cell28 != cell23 )).

fof(tlhfof49408,axiom,(
    cell28 != cell21 )).

fof(tlhfof49409,axiom,(
    cell28 != cell20 )).

fof(tlhfof49410,axiom,(
    cell28 != cell19 )).

fof(tlhfof49411,axiom,(
    cell28 != cell18 )).

fof(tlhfof49412,axiom,(
    cell28 != cell17 )).

fof(tlhfof49413,axiom,(
    cell28 != cell16 )).

fof(tlhfof49414,axiom,(
    cell28 != cell15 )).

fof(tlhfof49415,axiom,(
    cell28 != cell14 )).

fof(tlhfof49416,axiom,(
    cell28 != cell10 )).

fof(tlhfof49417,axiom,(
    cell28 != cell9 )).

fof(tlhfof49418,axiom,(
    cell28 != cell8 )).

fof(tlhfof49419,axiom,(
    cell28 != cell7 )).

fof(tlhfof49420,axiom,(
    cell28 != cell6 )).

fof(tlhfof49421,axiom,(
    cell28 != cell5 )).

fof(tlhfof49422,axiom,(
    cell28 != cell4 )).

fof(tlhfof49423,axiom,(
    cell28 != cell3 )).

fof(tlhfof49424,axiom,(
    cell28 != cell1 )).

fof(tlhfof49425,axiom,(
    cell28 != cell89 )).

fof(tlhfof49426,axiom,(
    cell28 != cell88 )).

fof(tlhfof49427,axiom,(
    cell28 != cell79 )).

fof(tlhfof49428,axiom,(
    cell28 != cell90 )).

fof(tlhfof49429,axiom,(
    cell28 != cell99 )).

fof(tlhfof49430,axiom,(
    cell28 != cell12 )).

fof(tlhfof49431,axiom,(
    cell28 != cell11 )).

fof(tlhfof49432,axiom,(
    cell28 != cell2 )).

fof(tlhfof49433,axiom,(
    cell28 != cell13 )).

fof(tlhfof49434,axiom,(
    cell28 != cell22 )).

fof(tlhfof49435,axiom,(
    cell27 != cell26 )).

fof(tlhfof49436,axiom,(
    cell27 != cell25 )).

fof(tlhfof49437,axiom,(
    cell27 != cell24 )).

fof(tlhfof49438,axiom,(
    cell27 != cell23 )).

fof(tlhfof49439,axiom,(
    cell27 != cell21 )).

fof(tlhfof49440,axiom,(
    cell27 != cell20 )).

fof(tlhfof49441,axiom,(
    cell27 != cell19 )).

fof(tlhfof49442,axiom,(
    cell27 != cell18 )).

fof(tlhfof49443,axiom,(
    cell27 != cell17 )).

fof(tlhfof49444,axiom,(
    cell27 != cell16 )).

fof(tlhfof49445,axiom,(
    cell27 != cell15 )).

fof(tlhfof49446,axiom,(
    cell27 != cell14 )).

fof(tlhfof49447,axiom,(
    cell27 != cell10 )).

fof(tlhfof49448,axiom,(
    cell27 != cell9 )).

fof(tlhfof49449,axiom,(
    cell27 != cell8 )).

fof(tlhfof49450,axiom,(
    cell27 != cell7 )).

fof(tlhfof49451,axiom,(
    cell27 != cell6 )).

fof(tlhfof49452,axiom,(
    cell27 != cell5 )).

fof(tlhfof49453,axiom,(
    cell27 != cell4 )).

fof(tlhfof49454,axiom,(
    cell27 != cell3 )).

fof(tlhfof49455,axiom,(
    cell27 != cell1 )).

fof(tlhfof49456,axiom,(
    cell27 != cell89 )).

fof(tlhfof49457,axiom,(
    cell27 != cell88 )).

fof(tlhfof49458,axiom,(
    cell27 != cell79 )).

fof(tlhfof49459,axiom,(
    cell27 != cell90 )).

fof(tlhfof49460,axiom,(
    cell27 != cell99 )).

fof(tlhfof49461,axiom,(
    cell27 != cell12 )).

fof(tlhfof49462,axiom,(
    cell27 != cell11 )).

fof(tlhfof49463,axiom,(
    cell27 != cell2 )).

fof(tlhfof49464,axiom,(
    cell27 != cell13 )).

fof(tlhfof49465,axiom,(
    cell27 != cell22 )).

fof(tlhfof49466,axiom,(
    cell26 != cell25 )).

fof(tlhfof49467,axiom,(
    cell26 != cell24 )).

fof(tlhfof49468,axiom,(
    cell26 != cell23 )).

fof(tlhfof49469,axiom,(
    cell26 != cell21 )).

fof(tlhfof49470,axiom,(
    cell26 != cell20 )).

fof(tlhfof49471,axiom,(
    cell26 != cell19 )).

fof(tlhfof49472,axiom,(
    cell26 != cell18 )).

fof(tlhfof49473,axiom,(
    cell26 != cell17 )).

fof(tlhfof49474,axiom,(
    cell26 != cell16 )).

fof(tlhfof49475,axiom,(
    cell26 != cell15 )).

fof(tlhfof49476,axiom,(
    cell26 != cell14 )).

fof(tlhfof49477,axiom,(
    cell26 != cell10 )).

fof(tlhfof49478,axiom,(
    cell26 != cell9 )).

fof(tlhfof49479,axiom,(
    cell26 != cell8 )).

fof(tlhfof49480,axiom,(
    cell26 != cell7 )).

fof(tlhfof49481,axiom,(
    cell26 != cell6 )).

fof(tlhfof49482,axiom,(
    cell26 != cell5 )).

fof(tlhfof49483,axiom,(
    cell26 != cell4 )).

fof(tlhfof49484,axiom,(
    cell26 != cell3 )).

fof(tlhfof49485,axiom,(
    cell26 != cell1 )).

fof(tlhfof49486,axiom,(
    cell26 != cell89 )).

fof(tlhfof49487,axiom,(
    cell26 != cell88 )).

fof(tlhfof49488,axiom,(
    cell26 != cell79 )).

fof(tlhfof49489,axiom,(
    cell26 != cell90 )).

fof(tlhfof49490,axiom,(
    cell26 != cell99 )).

fof(tlhfof49491,axiom,(
    cell26 != cell12 )).

fof(tlhfof49492,axiom,(
    cell26 != cell11 )).

fof(tlhfof49493,axiom,(
    cell26 != cell2 )).

fof(tlhfof49494,axiom,(
    cell26 != cell13 )).

fof(tlhfof49495,axiom,(
    cell26 != cell22 )).

fof(tlhfof49496,axiom,(
    cell25 != cell24 )).

fof(tlhfof49497,axiom,(
    cell25 != cell23 )).

fof(tlhfof49498,axiom,(
    cell25 != cell21 )).

fof(tlhfof49499,axiom,(
    cell25 != cell20 )).

fof(tlhfof49500,axiom,(
    cell25 != cell19 )).

fof(tlhfof49501,axiom,(
    cell25 != cell18 )).

fof(tlhfof49502,axiom,(
    cell25 != cell17 )).

fof(tlhfof49503,axiom,(
    cell25 != cell16 )).

fof(tlhfof49504,axiom,(
    cell25 != cell15 )).

fof(tlhfof49505,axiom,(
    cell25 != cell14 )).

fof(tlhfof49506,axiom,(
    cell25 != cell10 )).

fof(tlhfof49507,axiom,(
    cell25 != cell9 )).

fof(tlhfof49508,axiom,(
    cell25 != cell8 )).

fof(tlhfof49509,axiom,(
    cell25 != cell7 )).

fof(tlhfof49510,axiom,(
    cell25 != cell6 )).

fof(tlhfof49511,axiom,(
    cell25 != cell5 )).

fof(tlhfof49512,axiom,(
    cell25 != cell4 )).

fof(tlhfof49513,axiom,(
    cell25 != cell3 )).

fof(tlhfof49514,axiom,(
    cell25 != cell1 )).

fof(tlhfof49515,axiom,(
    cell25 != cell89 )).

fof(tlhfof49516,axiom,(
    cell25 != cell88 )).

fof(tlhfof49517,axiom,(
    cell25 != cell79 )).

fof(tlhfof49518,axiom,(
    cell25 != cell90 )).

fof(tlhfof49519,axiom,(
    cell25 != cell99 )).

fof(tlhfof49520,axiom,(
    cell25 != cell12 )).

fof(tlhfof49521,axiom,(
    cell25 != cell11 )).

fof(tlhfof49522,axiom,(
    cell25 != cell2 )).

fof(tlhfof49523,axiom,(
    cell25 != cell13 )).

fof(tlhfof49524,axiom,(
    cell25 != cell22 )).

fof(tlhfof49525,axiom,(
    cell24 != cell23 )).

fof(tlhfof49526,axiom,(
    cell24 != cell21 )).

fof(tlhfof49527,axiom,(
    cell24 != cell20 )).

fof(tlhfof49528,axiom,(
    cell24 != cell19 )).

fof(tlhfof49529,axiom,(
    cell24 != cell18 )).

fof(tlhfof49530,axiom,(
    cell24 != cell17 )).

fof(tlhfof49531,axiom,(
    cell24 != cell16 )).

fof(tlhfof49532,axiom,(
    cell24 != cell15 )).

fof(tlhfof49533,axiom,(
    cell24 != cell14 )).

fof(tlhfof49534,axiom,(
    cell24 != cell10 )).

fof(tlhfof49535,axiom,(
    cell24 != cell9 )).

fof(tlhfof49536,axiom,(
    cell24 != cell8 )).

fof(tlhfof49537,axiom,(
    cell24 != cell7 )).

fof(tlhfof49538,axiom,(
    cell24 != cell6 )).

fof(tlhfof49539,axiom,(
    cell24 != cell5 )).

fof(tlhfof49540,axiom,(
    cell24 != cell4 )).

fof(tlhfof49541,axiom,(
    cell24 != cell3 )).

fof(tlhfof49542,axiom,(
    cell24 != cell1 )).

fof(tlhfof49543,axiom,(
    cell24 != cell89 )).

fof(tlhfof49544,axiom,(
    cell24 != cell88 )).

fof(tlhfof49545,axiom,(
    cell24 != cell79 )).

fof(tlhfof49546,axiom,(
    cell24 != cell90 )).

fof(tlhfof49547,axiom,(
    cell24 != cell99 )).

fof(tlhfof49548,axiom,(
    cell24 != cell12 )).

fof(tlhfof49549,axiom,(
    cell24 != cell11 )).

fof(tlhfof49550,axiom,(
    cell24 != cell2 )).

fof(tlhfof49551,axiom,(
    cell24 != cell13 )).

fof(tlhfof49552,axiom,(
    cell24 != cell22 )).

fof(tlhfof49553,axiom,(
    cell23 != cell21 )).

fof(tlhfof49554,axiom,(
    cell23 != cell20 )).

fof(tlhfof49555,axiom,(
    cell23 != cell19 )).

fof(tlhfof49556,axiom,(
    cell23 != cell18 )).

fof(tlhfof49557,axiom,(
    cell23 != cell17 )).

fof(tlhfof49558,axiom,(
    cell23 != cell16 )).

fof(tlhfof49559,axiom,(
    cell23 != cell15 )).

fof(tlhfof49560,axiom,(
    cell23 != cell14 )).

fof(tlhfof49561,axiom,(
    cell23 != cell10 )).

fof(tlhfof49562,axiom,(
    cell23 != cell9 )).

fof(tlhfof49563,axiom,(
    cell23 != cell8 )).

fof(tlhfof49564,axiom,(
    cell23 != cell7 )).

fof(tlhfof49565,axiom,(
    cell23 != cell6 )).

fof(tlhfof49566,axiom,(
    cell23 != cell5 )).

fof(tlhfof49567,axiom,(
    cell23 != cell4 )).

fof(tlhfof49568,axiom,(
    cell23 != cell3 )).

fof(tlhfof49569,axiom,(
    cell23 != cell1 )).

fof(tlhfof49570,axiom,(
    cell23 != cell89 )).

fof(tlhfof49571,axiom,(
    cell23 != cell88 )).

fof(tlhfof49572,axiom,(
    cell23 != cell79 )).

fof(tlhfof49573,axiom,(
    cell23 != cell90 )).

fof(tlhfof49574,axiom,(
    cell23 != cell99 )).

fof(tlhfof49575,axiom,(
    cell23 != cell12 )).

fof(tlhfof49576,axiom,(
    cell23 != cell11 )).

fof(tlhfof49577,axiom,(
    cell23 != cell2 )).

fof(tlhfof49578,axiom,(
    cell23 != cell13 )).

fof(tlhfof49579,axiom,(
    cell23 != cell22 )).

fof(tlhfof49580,axiom,(
    cell21 != cell20 )).

fof(tlhfof49581,axiom,(
    cell21 != cell19 )).

fof(tlhfof49582,axiom,(
    cell21 != cell18 )).

fof(tlhfof49583,axiom,(
    cell21 != cell17 )).

fof(tlhfof49584,axiom,(
    cell21 != cell16 )).

fof(tlhfof49585,axiom,(
    cell21 != cell15 )).

fof(tlhfof49586,axiom,(
    cell21 != cell14 )).

fof(tlhfof49587,axiom,(
    cell21 != cell10 )).

fof(tlhfof49588,axiom,(
    cell21 != cell9 )).

fof(tlhfof49589,axiom,(
    cell21 != cell8 )).

fof(tlhfof49590,axiom,(
    cell21 != cell7 )).

fof(tlhfof49591,axiom,(
    cell21 != cell6 )).

fof(tlhfof49592,axiom,(
    cell21 != cell5 )).

fof(tlhfof49593,axiom,(
    cell21 != cell4 )).

fof(tlhfof49594,axiom,(
    cell21 != cell3 )).

fof(tlhfof49595,axiom,(
    cell21 != cell1 )).

fof(tlhfof49596,axiom,(
    cell21 != cell89 )).

fof(tlhfof49597,axiom,(
    cell21 != cell88 )).

fof(tlhfof49598,axiom,(
    cell21 != cell79 )).

fof(tlhfof49599,axiom,(
    cell21 != cell90 )).

fof(tlhfof49600,axiom,(
    cell21 != cell99 )).

fof(tlhfof49601,axiom,(
    cell21 != cell12 )).

fof(tlhfof49602,axiom,(
    cell21 != cell11 )).

fof(tlhfof49603,axiom,(
    cell21 != cell2 )).

fof(tlhfof49604,axiom,(
    cell21 != cell13 )).

fof(tlhfof49605,axiom,(
    cell21 != cell22 )).

fof(tlhfof49606,axiom,(
    cell20 != cell19 )).

fof(tlhfof49607,axiom,(
    cell20 != cell18 )).

fof(tlhfof49608,axiom,(
    cell20 != cell17 )).

fof(tlhfof49609,axiom,(
    cell20 != cell16 )).

fof(tlhfof49610,axiom,(
    cell20 != cell15 )).

fof(tlhfof49611,axiom,(
    cell20 != cell14 )).

fof(tlhfof49612,axiom,(
    cell20 != cell10 )).

fof(tlhfof49613,axiom,(
    cell20 != cell9 )).

fof(tlhfof49614,axiom,(
    cell20 != cell8 )).

fof(tlhfof49615,axiom,(
    cell20 != cell7 )).

fof(tlhfof49616,axiom,(
    cell20 != cell6 )).

fof(tlhfof49617,axiom,(
    cell20 != cell5 )).

fof(tlhfof49618,axiom,(
    cell20 != cell4 )).

fof(tlhfof49619,axiom,(
    cell20 != cell3 )).

fof(tlhfof49620,axiom,(
    cell20 != cell1 )).

fof(tlhfof49621,axiom,(
    cell20 != cell89 )).

fof(tlhfof49622,axiom,(
    cell20 != cell88 )).

fof(tlhfof49623,axiom,(
    cell20 != cell79 )).

fof(tlhfof49624,axiom,(
    cell20 != cell90 )).

fof(tlhfof49625,axiom,(
    cell20 != cell99 )).

fof(tlhfof49626,axiom,(
    cell20 != cell12 )).

fof(tlhfof49627,axiom,(
    cell20 != cell11 )).

fof(tlhfof49628,axiom,(
    cell20 != cell2 )).

fof(tlhfof49629,axiom,(
    cell20 != cell13 )).

fof(tlhfof49630,axiom,(
    cell20 != cell22 )).

fof(tlhfof49631,axiom,(
    cell19 != cell18 )).

fof(tlhfof49632,axiom,(
    cell19 != cell17 )).

fof(tlhfof49633,axiom,(
    cell19 != cell16 )).

fof(tlhfof49634,axiom,(
    cell19 != cell15 )).

fof(tlhfof49635,axiom,(
    cell19 != cell14 )).

fof(tlhfof49636,axiom,(
    cell19 != cell10 )).

fof(tlhfof49637,axiom,(
    cell19 != cell9 )).

fof(tlhfof49638,axiom,(
    cell19 != cell8 )).

fof(tlhfof49639,axiom,(
    cell19 != cell7 )).

fof(tlhfof49640,axiom,(
    cell19 != cell6 )).

fof(tlhfof49641,axiom,(
    cell19 != cell5 )).

fof(tlhfof49642,axiom,(
    cell19 != cell4 )).

fof(tlhfof49643,axiom,(
    cell19 != cell3 )).

fof(tlhfof49644,axiom,(
    cell19 != cell1 )).

fof(tlhfof49645,axiom,(
    cell19 != cell89 )).

fof(tlhfof49646,axiom,(
    cell19 != cell88 )).

fof(tlhfof49647,axiom,(
    cell19 != cell79 )).

fof(tlhfof49648,axiom,(
    cell19 != cell90 )).

fof(tlhfof49649,axiom,(
    cell19 != cell99 )).

fof(tlhfof49650,axiom,(
    cell19 != cell12 )).

fof(tlhfof49651,axiom,(
    cell19 != cell11 )).

fof(tlhfof49652,axiom,(
    cell19 != cell2 )).

fof(tlhfof49653,axiom,(
    cell19 != cell13 )).

fof(tlhfof49654,axiom,(
    cell19 != cell22 )).

fof(tlhfof49655,axiom,(
    cell18 != cell17 )).

fof(tlhfof49656,axiom,(
    cell18 != cell16 )).

fof(tlhfof49657,axiom,(
    cell18 != cell15 )).

fof(tlhfof49658,axiom,(
    cell18 != cell14 )).

fof(tlhfof49659,axiom,(
    cell18 != cell10 )).

fof(tlhfof49660,axiom,(
    cell18 != cell9 )).

fof(tlhfof49661,axiom,(
    cell18 != cell8 )).

fof(tlhfof49662,axiom,(
    cell18 != cell7 )).

fof(tlhfof49663,axiom,(
    cell18 != cell6 )).

fof(tlhfof49664,axiom,(
    cell18 != cell5 )).

fof(tlhfof49665,axiom,(
    cell18 != cell4 )).

fof(tlhfof49666,axiom,(
    cell18 != cell3 )).

fof(tlhfof49667,axiom,(
    cell18 != cell1 )).

fof(tlhfof49668,axiom,(
    cell18 != cell89 )).

fof(tlhfof49669,axiom,(
    cell18 != cell88 )).

fof(tlhfof49670,axiom,(
    cell18 != cell79 )).

fof(tlhfof49671,axiom,(
    cell18 != cell90 )).

fof(tlhfof49672,axiom,(
    cell18 != cell99 )).

fof(tlhfof49673,axiom,(
    cell18 != cell12 )).

fof(tlhfof49674,axiom,(
    cell18 != cell11 )).

fof(tlhfof49675,axiom,(
    cell18 != cell2 )).

fof(tlhfof49676,axiom,(
    cell18 != cell13 )).

fof(tlhfof49677,axiom,(
    cell18 != cell22 )).

fof(tlhfof49678,axiom,(
    cell17 != cell16 )).

fof(tlhfof49679,axiom,(
    cell17 != cell15 )).

fof(tlhfof49680,axiom,(
    cell17 != cell14 )).

fof(tlhfof49681,axiom,(
    cell17 != cell10 )).

fof(tlhfof49682,axiom,(
    cell17 != cell9 )).

fof(tlhfof49683,axiom,(
    cell17 != cell8 )).

fof(tlhfof49684,axiom,(
    cell17 != cell7 )).

fof(tlhfof49685,axiom,(
    cell17 != cell6 )).

fof(tlhfof49686,axiom,(
    cell17 != cell5 )).

fof(tlhfof49687,axiom,(
    cell17 != cell4 )).

fof(tlhfof49688,axiom,(
    cell17 != cell3 )).

fof(tlhfof49689,axiom,(
    cell17 != cell1 )).

fof(tlhfof49690,axiom,(
    cell17 != cell89 )).

fof(tlhfof49691,axiom,(
    cell17 != cell88 )).

fof(tlhfof49692,axiom,(
    cell17 != cell79 )).

fof(tlhfof49693,axiom,(
    cell17 != cell90 )).

fof(tlhfof49694,axiom,(
    cell17 != cell99 )).

fof(tlhfof49695,axiom,(
    cell17 != cell12 )).

fof(tlhfof49696,axiom,(
    cell17 != cell11 )).

fof(tlhfof49697,axiom,(
    cell17 != cell2 )).

fof(tlhfof49698,axiom,(
    cell17 != cell13 )).

fof(tlhfof49699,axiom,(
    cell17 != cell22 )).

fof(tlhfof49700,axiom,(
    cell16 != cell15 )).

fof(tlhfof49701,axiom,(
    cell16 != cell14 )).

fof(tlhfof49702,axiom,(
    cell16 != cell10 )).

fof(tlhfof49703,axiom,(
    cell16 != cell9 )).

fof(tlhfof49704,axiom,(
    cell16 != cell8 )).

fof(tlhfof49705,axiom,(
    cell16 != cell7 )).

fof(tlhfof49706,axiom,(
    cell16 != cell6 )).

fof(tlhfof49707,axiom,(
    cell16 != cell5 )).

fof(tlhfof49708,axiom,(
    cell16 != cell4 )).

fof(tlhfof49709,axiom,(
    cell16 != cell3 )).

fof(tlhfof49710,axiom,(
    cell16 != cell1 )).

fof(tlhfof49711,axiom,(
    cell16 != cell89 )).

fof(tlhfof49712,axiom,(
    cell16 != cell88 )).

fof(tlhfof49713,axiom,(
    cell16 != cell79 )).

fof(tlhfof49714,axiom,(
    cell16 != cell90 )).

fof(tlhfof49715,axiom,(
    cell16 != cell99 )).

fof(tlhfof49716,axiom,(
    cell16 != cell12 )).

fof(tlhfof49717,axiom,(
    cell16 != cell11 )).

fof(tlhfof49718,axiom,(
    cell16 != cell2 )).

fof(tlhfof49719,axiom,(
    cell16 != cell13 )).

fof(tlhfof49720,axiom,(
    cell16 != cell22 )).

fof(tlhfof49721,axiom,(
    cell15 != cell14 )).

fof(tlhfof49722,axiom,(
    cell15 != cell10 )).

fof(tlhfof49723,axiom,(
    cell15 != cell9 )).

fof(tlhfof49724,axiom,(
    cell15 != cell8 )).

fof(tlhfof49725,axiom,(
    cell15 != cell7 )).

fof(tlhfof49726,axiom,(
    cell15 != cell6 )).

fof(tlhfof49727,axiom,(
    cell15 != cell5 )).

fof(tlhfof49728,axiom,(
    cell15 != cell4 )).

fof(tlhfof49729,axiom,(
    cell15 != cell3 )).

fof(tlhfof49730,axiom,(
    cell15 != cell1 )).

fof(tlhfof49731,axiom,(
    cell15 != cell89 )).

fof(tlhfof49732,axiom,(
    cell15 != cell88 )).

fof(tlhfof49733,axiom,(
    cell15 != cell79 )).

fof(tlhfof49734,axiom,(
    cell15 != cell90 )).

fof(tlhfof49735,axiom,(
    cell15 != cell99 )).

fof(tlhfof49736,axiom,(
    cell15 != cell12 )).

fof(tlhfof49737,axiom,(
    cell15 != cell11 )).

fof(tlhfof49738,axiom,(
    cell15 != cell2 )).

fof(tlhfof49739,axiom,(
    cell15 != cell13 )).

fof(tlhfof49740,axiom,(
    cell15 != cell22 )).

fof(tlhfof49741,axiom,(
    cell14 != cell10 )).

fof(tlhfof49742,axiom,(
    cell14 != cell9 )).

fof(tlhfof49743,axiom,(
    cell14 != cell8 )).

fof(tlhfof49744,axiom,(
    cell14 != cell7 )).

fof(tlhfof49745,axiom,(
    cell14 != cell6 )).

fof(tlhfof49746,axiom,(
    cell14 != cell5 )).

fof(tlhfof49747,axiom,(
    cell14 != cell4 )).

fof(tlhfof49748,axiom,(
    cell14 != cell3 )).

fof(tlhfof49749,axiom,(
    cell14 != cell1 )).

fof(tlhfof49750,axiom,(
    cell14 != cell89 )).

fof(tlhfof49751,axiom,(
    cell14 != cell88 )).

fof(tlhfof49752,axiom,(
    cell14 != cell79 )).

fof(tlhfof49753,axiom,(
    cell14 != cell90 )).

fof(tlhfof49754,axiom,(
    cell14 != cell99 )).

fof(tlhfof49755,axiom,(
    cell14 != cell12 )).

fof(tlhfof49756,axiom,(
    cell14 != cell11 )).

fof(tlhfof49757,axiom,(
    cell14 != cell2 )).

fof(tlhfof49758,axiom,(
    cell14 != cell13 )).

fof(tlhfof49759,axiom,(
    cell14 != cell22 )).

fof(tlhfof49760,axiom,(
    cell10 != cell9 )).

fof(tlhfof49761,axiom,(
    cell10 != cell8 )).

fof(tlhfof49762,axiom,(
    cell10 != cell7 )).

fof(tlhfof49763,axiom,(
    cell10 != cell6 )).

fof(tlhfof49764,axiom,(
    cell10 != cell5 )).

fof(tlhfof49765,axiom,(
    cell10 != cell4 )).

fof(tlhfof49766,axiom,(
    cell10 != cell3 )).

fof(tlhfof49767,axiom,(
    cell10 != cell1 )).

fof(tlhfof49768,axiom,(
    cell10 != cell89 )).

fof(tlhfof49769,axiom,(
    cell10 != cell88 )).

fof(tlhfof49770,axiom,(
    cell10 != cell79 )).

fof(tlhfof49771,axiom,(
    cell10 != cell90 )).

fof(tlhfof49772,axiom,(
    cell10 != cell99 )).

fof(tlhfof49773,axiom,(
    cell10 != cell12 )).

fof(tlhfof49774,axiom,(
    cell10 != cell11 )).

fof(tlhfof49775,axiom,(
    cell10 != cell2 )).

fof(tlhfof49776,axiom,(
    cell10 != cell13 )).

fof(tlhfof49777,axiom,(
    cell10 != cell22 )).

fof(tlhfof49778,axiom,(
    cell9 != cell8 )).

fof(tlhfof49779,axiom,(
    cell9 != cell7 )).

fof(tlhfof49780,axiom,(
    cell9 != cell6 )).

fof(tlhfof49781,axiom,(
    cell9 != cell5 )).

fof(tlhfof49782,axiom,(
    cell9 != cell4 )).

fof(tlhfof49783,axiom,(
    cell9 != cell3 )).

fof(tlhfof49784,axiom,(
    cell9 != cell1 )).

fof(tlhfof49785,axiom,(
    cell9 != cell89 )).

fof(tlhfof49786,axiom,(
    cell9 != cell88 )).

fof(tlhfof49787,axiom,(
    cell9 != cell79 )).

fof(tlhfof49788,axiom,(
    cell9 != cell90 )).

fof(tlhfof49789,axiom,(
    cell9 != cell99 )).

fof(tlhfof49790,axiom,(
    cell9 != cell12 )).

fof(tlhfof49791,axiom,(
    cell9 != cell11 )).

fof(tlhfof49792,axiom,(
    cell9 != cell2 )).

fof(tlhfof49793,axiom,(
    cell9 != cell13 )).

fof(tlhfof49794,axiom,(
    cell9 != cell22 )).

fof(tlhfof49795,axiom,(
    cell8 != cell7 )).

fof(tlhfof49796,axiom,(
    cell8 != cell6 )).

fof(tlhfof49797,axiom,(
    cell8 != cell5 )).

fof(tlhfof49798,axiom,(
    cell8 != cell4 )).

fof(tlhfof49799,axiom,(
    cell8 != cell3 )).

fof(tlhfof49800,axiom,(
    cell8 != cell1 )).

fof(tlhfof49801,axiom,(
    cell8 != cell89 )).

fof(tlhfof49802,axiom,(
    cell8 != cell88 )).

fof(tlhfof49803,axiom,(
    cell8 != cell79 )).

fof(tlhfof49804,axiom,(
    cell8 != cell90 )).

fof(tlhfof49805,axiom,(
    cell8 != cell99 )).

fof(tlhfof49806,axiom,(
    cell8 != cell12 )).

fof(tlhfof49807,axiom,(
    cell8 != cell11 )).

fof(tlhfof49808,axiom,(
    cell8 != cell2 )).

fof(tlhfof49809,axiom,(
    cell8 != cell13 )).

fof(tlhfof49810,axiom,(
    cell8 != cell22 )).

fof(tlhfof49811,axiom,(
    cell7 != cell6 )).

fof(tlhfof49812,axiom,(
    cell7 != cell5 )).

fof(tlhfof49813,axiom,(
    cell7 != cell4 )).

fof(tlhfof49814,axiom,(
    cell7 != cell3 )).

fof(tlhfof49815,axiom,(
    cell7 != cell1 )).

fof(tlhfof49816,axiom,(
    cell7 != cell89 )).

fof(tlhfof49817,axiom,(
    cell7 != cell88 )).

fof(tlhfof49818,axiom,(
    cell7 != cell79 )).

fof(tlhfof49819,axiom,(
    cell7 != cell90 )).

fof(tlhfof49820,axiom,(
    cell7 != cell99 )).

fof(tlhfof49821,axiom,(
    cell7 != cell12 )).

fof(tlhfof49822,axiom,(
    cell7 != cell11 )).

fof(tlhfof49823,axiom,(
    cell7 != cell2 )).

fof(tlhfof49824,axiom,(
    cell7 != cell13 )).

fof(tlhfof49825,axiom,(
    cell7 != cell22 )).

fof(tlhfof49826,axiom,(
    cell6 != cell5 )).

fof(tlhfof49827,axiom,(
    cell6 != cell4 )).

fof(tlhfof49828,axiom,(
    cell6 != cell3 )).

fof(tlhfof49829,axiom,(
    cell6 != cell1 )).

fof(tlhfof49830,axiom,(
    cell6 != cell89 )).

fof(tlhfof49831,axiom,(
    cell6 != cell88 )).

fof(tlhfof49832,axiom,(
    cell6 != cell79 )).

fof(tlhfof49833,axiom,(
    cell6 != cell90 )).

fof(tlhfof49834,axiom,(
    cell6 != cell99 )).

fof(tlhfof49835,axiom,(
    cell6 != cell12 )).

fof(tlhfof49836,axiom,(
    cell6 != cell11 )).

fof(tlhfof49837,axiom,(
    cell6 != cell2 )).

fof(tlhfof49838,axiom,(
    cell6 != cell13 )).

fof(tlhfof49839,axiom,(
    cell6 != cell22 )).

fof(tlhfof49840,axiom,(
    cell5 != cell4 )).

fof(tlhfof49841,axiom,(
    cell5 != cell3 )).

fof(tlhfof49842,axiom,(
    cell5 != cell1 )).

fof(tlhfof49843,axiom,(
    cell5 != cell89 )).

fof(tlhfof49844,axiom,(
    cell5 != cell88 )).

fof(tlhfof49845,axiom,(
    cell5 != cell79 )).

fof(tlhfof49846,axiom,(
    cell5 != cell90 )).

fof(tlhfof49847,axiom,(
    cell5 != cell99 )).

fof(tlhfof49848,axiom,(
    cell5 != cell12 )).

fof(tlhfof49849,axiom,(
    cell5 != cell11 )).

fof(tlhfof49850,axiom,(
    cell5 != cell2 )).

fof(tlhfof49851,axiom,(
    cell5 != cell13 )).

fof(tlhfof49852,axiom,(
    cell5 != cell22 )).

fof(tlhfof49853,axiom,(
    cell4 != cell3 )).

fof(tlhfof49854,axiom,(
    cell4 != cell1 )).

fof(tlhfof49855,axiom,(
    cell4 != cell89 )).

fof(tlhfof49856,axiom,(
    cell4 != cell88 )).

fof(tlhfof49857,axiom,(
    cell4 != cell79 )).

fof(tlhfof49858,axiom,(
    cell4 != cell90 )).

fof(tlhfof49859,axiom,(
    cell4 != cell99 )).

fof(tlhfof49860,axiom,(
    cell4 != cell12 )).

fof(tlhfof49861,axiom,(
    cell4 != cell11 )).

fof(tlhfof49862,axiom,(
    cell4 != cell2 )).

fof(tlhfof49863,axiom,(
    cell4 != cell13 )).

fof(tlhfof49864,axiom,(
    cell4 != cell22 )).

fof(tlhfof49865,axiom,(
    cell3 != cell1 )).

fof(tlhfof49866,axiom,(
    cell3 != cell89 )).

fof(tlhfof49867,axiom,(
    cell3 != cell88 )).

fof(tlhfof49868,axiom,(
    cell3 != cell79 )).

fof(tlhfof49869,axiom,(
    cell3 != cell90 )).

fof(tlhfof49870,axiom,(
    cell3 != cell99 )).

fof(tlhfof49871,axiom,(
    cell3 != cell12 )).

fof(tlhfof49872,axiom,(
    cell3 != cell11 )).

fof(tlhfof49873,axiom,(
    cell3 != cell2 )).

fof(tlhfof49874,axiom,(
    cell3 != cell13 )).

fof(tlhfof49875,axiom,(
    cell3 != cell22 )).

fof(tlhfof49876,axiom,(
    cell1 != cell89 )).

fof(tlhfof49877,axiom,(
    cell1 != cell88 )).

fof(tlhfof49878,axiom,(
    cell1 != cell79 )).

fof(tlhfof49879,axiom,(
    cell1 != cell90 )).

fof(tlhfof49880,axiom,(
    cell1 != cell99 )).

fof(tlhfof49881,axiom,(
    cell1 != cell12 )).

fof(tlhfof49882,axiom,(
    cell1 != cell11 )).

fof(tlhfof49883,axiom,(
    cell1 != cell2 )).

fof(tlhfof49884,axiom,(
    cell1 != cell13 )).

fof(tlhfof49885,axiom,(
    cell1 != cell22 )).

fof(tlhfof49886,axiom,(
    cell89 != cell88 )).

fof(tlhfof49887,axiom,(
    cell89 != cell79 )).

fof(tlhfof49888,axiom,(
    cell89 != cell90 )).

fof(tlhfof49889,axiom,(
    cell89 != cell99 )).

fof(tlhfof49890,axiom,(
    cell89 != cell12 )).

fof(tlhfof49891,axiom,(
    cell89 != cell11 )).

fof(tlhfof49892,axiom,(
    cell89 != cell2 )).

fof(tlhfof49893,axiom,(
    cell89 != cell13 )).

fof(tlhfof49894,axiom,(
    cell89 != cell22 )).

fof(tlhfof49895,axiom,(
    cell88 != cell79 )).

fof(tlhfof49896,axiom,(
    cell88 != cell90 )).

fof(tlhfof49897,axiom,(
    cell88 != cell99 )).

fof(tlhfof49898,axiom,(
    cell88 != cell12 )).

fof(tlhfof49899,axiom,(
    cell88 != cell11 )).

fof(tlhfof49900,axiom,(
    cell88 != cell2 )).

fof(tlhfof49901,axiom,(
    cell88 != cell13 )).

fof(tlhfof49902,axiom,(
    cell88 != cell22 )).

fof(tlhfof49903,axiom,(
    cell79 != cell90 )).

fof(tlhfof49904,axiom,(
    cell79 != cell99 )).

fof(tlhfof49905,axiom,(
    cell79 != cell12 )).

fof(tlhfof49906,axiom,(
    cell79 != cell11 )).

fof(tlhfof49907,axiom,(
    cell79 != cell2 )).

fof(tlhfof49908,axiom,(
    cell79 != cell13 )).

fof(tlhfof49909,axiom,(
    cell79 != cell22 )).

fof(tlhfof49910,axiom,(
    cell90 != cell99 )).

fof(tlhfof49911,axiom,(
    cell90 != cell12 )).

fof(tlhfof49912,axiom,(
    cell90 != cell11 )).

fof(tlhfof49913,axiom,(
    cell90 != cell2 )).

fof(tlhfof49914,axiom,(
    cell90 != cell13 )).

fof(tlhfof49915,axiom,(
    cell90 != cell22 )).

fof(tlhfof49916,axiom,(
    cell99 != cell12 )).

fof(tlhfof49917,axiom,(
    cell99 != cell11 )).

fof(tlhfof49918,axiom,(
    cell99 != cell2 )).

fof(tlhfof49919,axiom,(
    cell99 != cell13 )).

fof(tlhfof49920,axiom,(
    cell99 != cell22 )).

fof(tlhfof49921,axiom,(
    cell12 != cell11 )).

fof(tlhfof49922,axiom,(
    cell12 != cell2 )).

fof(tlhfof49923,axiom,(
    cell12 != cell13 )).

fof(tlhfof49924,axiom,(
    cell12 != cell22 )).

fof(tlhfof49925,axiom,(
    cell11 != cell2 )).

fof(tlhfof49926,axiom,(
    cell11 != cell13 )).

fof(tlhfof49927,axiom,(
    cell11 != cell22 )).

fof(tlhfof49928,axiom,(
    cell2 != cell13 )).

fof(tlhfof49929,axiom,(
    cell2 != cell22 )).

fof(tlhfof49930,axiom,(
    cell13 != cell22 )).

fof(tlhfof49931,axiom,(
    ! [X] :
      ( X = cell100
      | X = cell98
      | X = cell97
      | X = cell96
      | X = cell95
      | X = cell94
      | X = cell93
      | X = cell91
      | X = cell92
      | X = cell87
      | X = cell86
      | X = cell85
      | X = cell84
      | X = cell83
      | X = cell81
      | X = cell82
      | X = cell80
      | X = cell78
      | X = cell77
      | X = cell76
      | X = cell75
      | X = cell74
      | X = cell73
      | X = cell71
      | X = cell72
      | X = cell70
      | X = cell69
      | X = cell68
      | X = cell67
      | X = cell66
      | X = cell65
      | X = cell64
      | X = cell63
      | X = cell61
      | X = cell62
      | X = cell60
      | X = cell59
      | X = cell58
      | X = cell57
      | X = cell56
      | X = cell55
      | X = cell54
      | X = cell53
      | X = cell51
      | X = cell52
      | X = cell50
      | X = cell49
      | X = cell48
      | X = cell47
      | X = cell46
      | X = cell45
      | X = cell44
      | X = cell43
      | X = cell41
      | X = cell42
      | X = cell40
      | X = cell39
      | X = cell38
      | X = cell37
      | X = cell36
      | X = cell35
      | X = cell34
      | X = cell33
      | X = cell31
      | X = cell32
      | X = cell30
      | X = cell29
      | X = cell28
      | X = cell27
      | X = cell26
      | X = cell25
      | X = cell24
      | X = cell23
      | X = cell21
      | X = cell20
      | X = cell19
      | X = cell18
      | X = cell17
      | X = cell16
      | X = cell15
      | X = cell14
      | X = cell10
      | X = cell9
      | X = cell8
      | X = cell7
      | X = cell6
      | X = cell5
      | X = cell4
      | X = cell3
      | X = cell1
      | X = cell89
      | X = cell88
      | X = cell79
      | X = cell90
      | X = cell99
      | X = cell12
      | X = cell11
      | X = cell2
      | X = cell13
      | X = cell22 ) )).

fof(tlhfof49932,conjecture,(
    ! [X,Y] :
      ( ( patient(X)
        & oxygen(Y) )
     => ~ adj(X,Y) ) )).

%------------------------------------------------------------------------------
