%------------------------------------------------------------------------------
% File     : PUZ076+1 : TPTP v5.2.0. Released v3.5.0.
% Domain   : Puzzles
% Problem  : Cell is not west of the other in the Wumpus world
% Version  : Especial.
% English  :

% Refs     : [Hin07] Hinrichs (2007), Email to Geoff Sutcliffe
% Source   : [Hin07]
% Names    : westof-neg [Hin07]

% Status   : Theorem
% Rating   : 0.96 v5.2.0, 1.00 v3.5.0
% Syntax   : Number of formulae    : 4957 (4951 unit)
%            Number of atoms       : 5412 (5310 equality)
%            Maximal formula depth :   94 (   2 average)
%            Number of connectives : 5406 (4951   ~; 196   |; 253   &)
%                                         (   0 <=>;   6  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    7 (   0 propositional; 2-2 arity)
%            Number of functors    :  100 ( 100 constant; 0-0 arity)
%            Number of variables   :   85 (   0 sgn;  12   !;  73   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments :
%------------------------------------------------------------------------------
fof(tlhfof35060,axiom,(
    ! [TLH34981,TLH34982] :
      ( west(TLH34982,TLH34981)
     => ( ( TLH34982 = cell1
          & TLH34981 = cell11 )
        | ( TLH34982 = cell2
          & TLH34981 = cell12 )
        | ( TLH34982 = cell3
          & TLH34981 = cell13 )
        | ( TLH34982 = cell4
          & TLH34981 = cell14 )
        | ( TLH34982 = cell5
          & TLH34981 = cell15 )
        | ( TLH34982 = cell6
          & TLH34981 = cell16 )
        | ( TLH34982 = cell7
          & TLH34981 = cell17 )
        | ( TLH34982 = cell8
          & TLH34981 = cell18 )
        | ( TLH34982 = cell9
          & TLH34981 = cell19 )
        | ( TLH34982 = cell10
          & TLH34981 = cell20 )
        | ( TLH34982 = cell11
          & TLH34981 = cell21 )
        | ( TLH34982 = cell12
          & TLH34981 = cell22 )
        | ( TLH34982 = cell13
          & TLH34981 = cell23 )
        | ( TLH34982 = cell14
          & TLH34981 = cell24 )
        | ( TLH34982 = cell15
          & TLH34981 = cell25 )
        | ( TLH34982 = cell16
          & TLH34981 = cell26 )
        | ( TLH34982 = cell17
          & TLH34981 = cell27 )
        | ( TLH34982 = cell18
          & TLH34981 = cell28 )
        | ( TLH34982 = cell19
          & TLH34981 = cell29 )
        | ( TLH34982 = cell20
          & TLH34981 = cell30 )
        | ( TLH34982 = cell21
          & TLH34981 = cell31 )
        | ( TLH34982 = cell22
          & TLH34981 = cell32 )
        | ( TLH34982 = cell23
          & TLH34981 = cell33 )
        | ( TLH34982 = cell24
          & TLH34981 = cell34 )
        | ( TLH34982 = cell25
          & TLH34981 = cell35 )
        | ( TLH34982 = cell26
          & TLH34981 = cell36 )
        | ( TLH34982 = cell27
          & TLH34981 = cell37 )
        | ( TLH34982 = cell28
          & TLH34981 = cell38 )
        | ( TLH34982 = cell29
          & TLH34981 = cell39 )
        | ( TLH34982 = cell30
          & TLH34981 = cell40 )
        | ( TLH34982 = cell31
          & TLH34981 = cell41 )
        | ( TLH34982 = cell32
          & TLH34981 = cell42 )
        | ( TLH34982 = cell33
          & TLH34981 = cell43 )
        | ( TLH34982 = cell34
          & TLH34981 = cell44 )
        | ( TLH34982 = cell35
          & TLH34981 = cell45 )
        | ( TLH34982 = cell36
          & TLH34981 = cell46 )
        | ( TLH34982 = cell37
          & TLH34981 = cell47 )
        | ( TLH34982 = cell38
          & TLH34981 = cell48 )
        | ( TLH34982 = cell39
          & TLH34981 = cell49 )
        | ( TLH34982 = cell40
          & TLH34981 = cell50 )
        | ( TLH34982 = cell41
          & TLH34981 = cell51 )
        | ( TLH34982 = cell42
          & TLH34981 = cell52 )
        | ( TLH34982 = cell43
          & TLH34981 = cell53 )
        | ( TLH34982 = cell44
          & TLH34981 = cell54 )
        | ( TLH34982 = cell45
          & TLH34981 = cell55 )
        | ( TLH34982 = cell46
          & TLH34981 = cell56 )
        | ( TLH34982 = cell47
          & TLH34981 = cell57 )
        | ( TLH34982 = cell48
          & TLH34981 = cell58 )
        | ( TLH34982 = cell49
          & TLH34981 = cell59 )
        | ( TLH34982 = cell50
          & TLH34981 = cell60 )
        | ( TLH34982 = cell51
          & TLH34981 = cell61 )
        | ( TLH34982 = cell52
          & TLH34981 = cell62 )
        | ( TLH34982 = cell53
          & TLH34981 = cell63 )
        | ( TLH34982 = cell54
          & TLH34981 = cell64 )
        | ( TLH34982 = cell55
          & TLH34981 = cell65 )
        | ( TLH34982 = cell56
          & TLH34981 = cell66 )
        | ( TLH34982 = cell57
          & TLH34981 = cell67 )
        | ( TLH34982 = cell58
          & TLH34981 = cell68 )
        | ( TLH34982 = cell59
          & TLH34981 = cell69 )
        | ( TLH34982 = cell60
          & TLH34981 = cell70 )
        | ( TLH34982 = cell61
          & TLH34981 = cell71 )
        | ( TLH34982 = cell62
          & TLH34981 = cell72 )
        | ( TLH34982 = cell63
          & TLH34981 = cell73 )
        | ( TLH34982 = cell64
          & TLH34981 = cell74 )
        | ( TLH34982 = cell65
          & TLH34981 = cell75 )
        | ( TLH34982 = cell66
          & TLH34981 = cell76 )
        | ( TLH34982 = cell67
          & TLH34981 = cell77 )
        | ( TLH34982 = cell68
          & TLH34981 = cell78 )
        | ( TLH34982 = cell69
          & TLH34981 = cell79 )
        | ( TLH34982 = cell70
          & TLH34981 = cell80 )
        | ( TLH34982 = cell71
          & TLH34981 = cell81 )
        | ( TLH34982 = cell72
          & TLH34981 = cell82 )
        | ( TLH34982 = cell73
          & TLH34981 = cell83 )
        | ( TLH34982 = cell74
          & TLH34981 = cell84 )
        | ( TLH34982 = cell75
          & TLH34981 = cell85 )
        | ( TLH34982 = cell76
          & TLH34981 = cell86 )
        | ( TLH34982 = cell77
          & TLH34981 = cell87 )
        | ( TLH34982 = cell78
          & TLH34981 = cell88 )
        | ( TLH34982 = cell79
          & TLH34981 = cell89 )
        | ( TLH34982 = cell80
          & TLH34981 = cell90 )
        | ( TLH34982 = cell81
          & TLH34981 = cell91 )
        | ( TLH34982 = cell82
          & TLH34981 = cell92 )
        | ( TLH34982 = cell83
          & TLH34981 = cell93 )
        | ( TLH34982 = cell84
          & TLH34981 = cell94 )
        | ( TLH34982 = cell85
          & TLH34981 = cell95 )
        | ( TLH34982 = cell86
          & TLH34981 = cell96 )
        | ( TLH34982 = cell87
          & TLH34981 = cell97 )
        | ( TLH34982 = cell88
          & TLH34981 = cell98 )
        | ( TLH34982 = cell89
          & TLH34981 = cell99 )
        | ( TLH34982 = cell90
          & TLH34981 = cell100 ) ) ) )).

fof(tlhfof35061,axiom,(
    ! [TLH34983,TLH34984] :
      ( north(TLH34984,TLH34983)
     => ( ( TLH34984 = cell1
          & TLH34983 = cell2 )
        | ( TLH34984 = cell2
          & TLH34983 = cell3 )
        | ( TLH34984 = cell3
          & TLH34983 = cell4 )
        | ( TLH34984 = cell4
          & TLH34983 = cell5 )
        | ( TLH34984 = cell5
          & TLH34983 = cell6 )
        | ( TLH34984 = cell6
          & TLH34983 = cell7 )
        | ( TLH34984 = cell7
          & TLH34983 = cell8 )
        | ( TLH34984 = cell8
          & TLH34983 = cell9 )
        | ( TLH34984 = cell9
          & TLH34983 = cell10 )
        | ( TLH34984 = cell11
          & TLH34983 = cell12 )
        | ( TLH34984 = cell12
          & TLH34983 = cell13 )
        | ( TLH34984 = cell13
          & TLH34983 = cell14 )
        | ( TLH34984 = cell14
          & TLH34983 = cell15 )
        | ( TLH34984 = cell15
          & TLH34983 = cell16 )
        | ( TLH34984 = cell16
          & TLH34983 = cell17 )
        | ( TLH34984 = cell17
          & TLH34983 = cell18 )
        | ( TLH34984 = cell18
          & TLH34983 = cell19 )
        | ( TLH34984 = cell19
          & TLH34983 = cell20 )
        | ( TLH34984 = cell21
          & TLH34983 = cell22 )
        | ( TLH34984 = cell22
          & TLH34983 = cell23 )
        | ( TLH34984 = cell23
          & TLH34983 = cell24 )
        | ( TLH34984 = cell24
          & TLH34983 = cell25 )
        | ( TLH34984 = cell25
          & TLH34983 = cell26 )
        | ( TLH34984 = cell26
          & TLH34983 = cell27 )
        | ( TLH34984 = cell27
          & TLH34983 = cell28 )
        | ( TLH34984 = cell28
          & TLH34983 = cell29 )
        | ( TLH34984 = cell29
          & TLH34983 = cell30 )
        | ( TLH34984 = cell31
          & TLH34983 = cell32 )
        | ( TLH34984 = cell32
          & TLH34983 = cell33 )
        | ( TLH34984 = cell33
          & TLH34983 = cell34 )
        | ( TLH34984 = cell34
          & TLH34983 = cell35 )
        | ( TLH34984 = cell35
          & TLH34983 = cell36 )
        | ( TLH34984 = cell36
          & TLH34983 = cell37 )
        | ( TLH34984 = cell37
          & TLH34983 = cell38 )
        | ( TLH34984 = cell38
          & TLH34983 = cell39 )
        | ( TLH34984 = cell39
          & TLH34983 = cell40 )
        | ( TLH34984 = cell41
          & TLH34983 = cell42 )
        | ( TLH34984 = cell42
          & TLH34983 = cell43 )
        | ( TLH34984 = cell43
          & TLH34983 = cell44 )
        | ( TLH34984 = cell44
          & TLH34983 = cell45 )
        | ( TLH34984 = cell45
          & TLH34983 = cell46 )
        | ( TLH34984 = cell46
          & TLH34983 = cell47 )
        | ( TLH34984 = cell47
          & TLH34983 = cell48 )
        | ( TLH34984 = cell48
          & TLH34983 = cell49 )
        | ( TLH34984 = cell49
          & TLH34983 = cell50 )
        | ( TLH34984 = cell51
          & TLH34983 = cell52 )
        | ( TLH34984 = cell52
          & TLH34983 = cell53 )
        | ( TLH34984 = cell53
          & TLH34983 = cell54 )
        | ( TLH34984 = cell54
          & TLH34983 = cell55 )
        | ( TLH34984 = cell55
          & TLH34983 = cell56 )
        | ( TLH34984 = cell56
          & TLH34983 = cell57 )
        | ( TLH34984 = cell57
          & TLH34983 = cell58 )
        | ( TLH34984 = cell58
          & TLH34983 = cell59 )
        | ( TLH34984 = cell59
          & TLH34983 = cell60 )
        | ( TLH34984 = cell61
          & TLH34983 = cell62 )
        | ( TLH34984 = cell62
          & TLH34983 = cell63 )
        | ( TLH34984 = cell63
          & TLH34983 = cell64 )
        | ( TLH34984 = cell64
          & TLH34983 = cell65 )
        | ( TLH34984 = cell65
          & TLH34983 = cell66 )
        | ( TLH34984 = cell66
          & TLH34983 = cell67 )
        | ( TLH34984 = cell67
          & TLH34983 = cell68 )
        | ( TLH34984 = cell68
          & TLH34983 = cell69 )
        | ( TLH34984 = cell69
          & TLH34983 = cell70 )
        | ( TLH34984 = cell71
          & TLH34983 = cell72 )
        | ( TLH34984 = cell72
          & TLH34983 = cell73 )
        | ( TLH34984 = cell73
          & TLH34983 = cell74 )
        | ( TLH34984 = cell74
          & TLH34983 = cell75 )
        | ( TLH34984 = cell75
          & TLH34983 = cell76 )
        | ( TLH34984 = cell76
          & TLH34983 = cell77 )
        | ( TLH34984 = cell77
          & TLH34983 = cell78 )
        | ( TLH34984 = cell78
          & TLH34983 = cell79 )
        | ( TLH34984 = cell79
          & TLH34983 = cell80 )
        | ( TLH34984 = cell81
          & TLH34983 = cell82 )
        | ( TLH34984 = cell82
          & TLH34983 = cell83 )
        | ( TLH34984 = cell83
          & TLH34983 = cell84 )
        | ( TLH34984 = cell84
          & TLH34983 = cell85 )
        | ( TLH34984 = cell85
          & TLH34983 = cell86 )
        | ( TLH34984 = cell86
          & TLH34983 = cell87 )
        | ( TLH34984 = cell87
          & TLH34983 = cell88 )
        | ( TLH34984 = cell88
          & TLH34983 = cell89 )
        | ( TLH34984 = cell89
          & TLH34983 = cell90 )
        | ( TLH34984 = cell91
          & TLH34983 = cell92 )
        | ( TLH34984 = cell92
          & TLH34983 = cell93 )
        | ( TLH34984 = cell93
          & TLH34983 = cell94 )
        | ( TLH34984 = cell94
          & TLH34983 = cell95 )
        | ( TLH34984 = cell95
          & TLH34983 = cell96 )
        | ( TLH34984 = cell96
          & TLH34983 = cell97 )
        | ( TLH34984 = cell97
          & TLH34983 = cell98 )
        | ( TLH34984 = cell98
          & TLH34983 = cell99 )
        | ( TLH34984 = cell99
          & TLH34983 = cell100 ) ) ) )).

fof(tlhfof35062,axiom,(
    ! [TLH34986,TLH34985] :
      ( duewest(TLH34985,TLH34986)
     => ( west(TLH34985,TLH34986)
        | ? [TLH34987] :
            ( west(TLH34985,TLH34987)
            & west(TLH34987,TLH34986) )
        | ? [TLH34988,TLH34989] :
            ( west(TLH34985,TLH34988)
            & west(TLH34988,TLH34989)
            & west(TLH34989,TLH34986) )
        | ? [TLH34990,TLH34991,TLH34992] :
            ( west(TLH34985,TLH34990)
            & west(TLH34990,TLH34991)
            & west(TLH34991,TLH34992)
            & west(TLH34992,TLH34986) )
        | ? [TLH34993,TLH34994,TLH34995,TLH34996] :
            ( west(TLH34985,TLH34993)
            & west(TLH34993,TLH34994)
            & west(TLH34994,TLH34995)
            & west(TLH34995,TLH34996)
            & west(TLH34996,TLH34986) )
        | ? [TLH34997,TLH34998,TLH34999,TLH35000,TLH35001] :
            ( west(TLH34985,TLH34997)
            & west(TLH34997,TLH34998)
            & west(TLH34998,TLH34999)
            & west(TLH34999,TLH35000)
            & west(TLH35000,TLH35001)
            & west(TLH35001,TLH34986) )
        | ? [TLH35002,TLH35003,TLH35004,TLH35005,TLH35006,TLH35007] :
            ( west(TLH34985,TLH35002)
            & west(TLH35002,TLH35003)
            & west(TLH35003,TLH35004)
            & west(TLH35004,TLH35005)
            & west(TLH35005,TLH35006)
            & west(TLH35006,TLH35007)
            & west(TLH35007,TLH34986) )
        | ? [TLH35008,TLH35009,TLH35010,TLH35011,TLH35012,TLH35013,TLH35014] :
            ( west(TLH34985,TLH35008)
            & west(TLH35008,TLH35009)
            & west(TLH35009,TLH35010)
            & west(TLH35010,TLH35011)
            & west(TLH35011,TLH35012)
            & west(TLH35012,TLH35013)
            & west(TLH35013,TLH35014)
            & west(TLH35014,TLH34986) )
        | ? [TLH35015,TLH35016,TLH35017,TLH35018,TLH35019,TLH35020,TLH35021,TLH35022] :
            ( west(TLH34985,TLH35015)
            & west(TLH35015,TLH35016)
            & west(TLH35016,TLH35017)
            & west(TLH35017,TLH35018)
            & west(TLH35018,TLH35019)
            & west(TLH35019,TLH35020)
            & west(TLH35020,TLH35021)
            & west(TLH35021,TLH35022)
            & west(TLH35022,TLH34986) ) ) ) )).

fof(tlhfof35063,axiom,(
    ! [TLH35024,TLH35023] :
      ( duenorth(TLH35023,TLH35024)
     => ( north(TLH35023,TLH35024)
        | ? [TLH35025] :
            ( north(TLH35023,TLH35025)
            & north(TLH35025,TLH35024) )
        | ? [TLH35026,TLH35027] :
            ( north(TLH35023,TLH35026)
            & north(TLH35026,TLH35027)
            & north(TLH35027,TLH35024) )
        | ? [TLH35028,TLH35029,TLH35030] :
            ( north(TLH35023,TLH35028)
            & north(TLH35028,TLH35029)
            & north(TLH35029,TLH35030)
            & north(TLH35030,TLH35024) )
        | ? [TLH35031,TLH35032,TLH35033,TLH35034] :
            ( north(TLH35023,TLH35031)
            & north(TLH35031,TLH35032)
            & north(TLH35032,TLH35033)
            & north(TLH35033,TLH35034)
            & north(TLH35034,TLH35024) )
        | ? [TLH35035,TLH35036,TLH35037,TLH35038,TLH35039] :
            ( north(TLH35023,TLH35035)
            & north(TLH35035,TLH35036)
            & north(TLH35036,TLH35037)
            & north(TLH35037,TLH35038)
            & north(TLH35038,TLH35039)
            & north(TLH35039,TLH35024) )
        | ? [TLH35040,TLH35041,TLH35042,TLH35043,TLH35044,TLH35045] :
            ( north(TLH35023,TLH35040)
            & north(TLH35040,TLH35041)
            & north(TLH35041,TLH35042)
            & north(TLH35042,TLH35043)
            & north(TLH35043,TLH35044)
            & north(TLH35044,TLH35045)
            & north(TLH35045,TLH35024) )
        | ? [TLH35046,TLH35047,TLH35048,TLH35049,TLH35050,TLH35051,TLH35052] :
            ( north(TLH35023,TLH35046)
            & north(TLH35046,TLH35047)
            & north(TLH35047,TLH35048)
            & north(TLH35048,TLH35049)
            & north(TLH35049,TLH35050)
            & north(TLH35050,TLH35051)
            & north(TLH35051,TLH35052)
            & north(TLH35052,TLH35024) )
        | ? [TLH35053,TLH35054,TLH35055,TLH35056,TLH35057,TLH35058,TLH35059,TLH35060] :
            ( north(TLH35023,TLH35053)
            & north(TLH35053,TLH35054)
            & north(TLH35054,TLH35055)
            & north(TLH35055,TLH35056)
            & north(TLH35056,TLH35057)
            & north(TLH35057,TLH35058)
            & north(TLH35058,TLH35059)
            & north(TLH35059,TLH35060)
            & north(TLH35060,TLH35024) ) ) ) )).

fof(tlhfof35064,axiom,(
    ! [Y,X] :
      ( vert(X,Y)
     => ( duenorth(X,Y)
        | duenorth(Y,X) ) ) )).

fof(tlhfof35065,axiom,(
    ! [Y,X] :
      ( westof(X,Y)
     => ( duewest(X,Y)
        | ? [Z] :
            ( vert(X,Z)
            & duewest(Z,Y) ) ) ) )).

fof(tlhfof35066,axiom,(
    cell100 != cell99 )).

fof(tlhfof35067,axiom,(
    cell100 != cell98 )).

fof(tlhfof35068,axiom,(
    cell100 != cell97 )).

fof(tlhfof35069,axiom,(
    cell100 != cell96 )).

fof(tlhfof35070,axiom,(
    cell100 != cell95 )).

fof(tlhfof35071,axiom,(
    cell100 != cell94 )).

fof(tlhfof35072,axiom,(
    cell100 != cell93 )).

fof(tlhfof35073,axiom,(
    cell100 != cell92 )).

fof(tlhfof35074,axiom,(
    cell100 != cell91 )).

fof(tlhfof35075,axiom,(
    cell100 != cell90 )).

fof(tlhfof35076,axiom,(
    cell100 != cell89 )).

fof(tlhfof35077,axiom,(
    cell100 != cell88 )).

fof(tlhfof35078,axiom,(
    cell100 != cell87 )).

fof(tlhfof35079,axiom,(
    cell100 != cell86 )).

fof(tlhfof35080,axiom,(
    cell100 != cell85 )).

fof(tlhfof35081,axiom,(
    cell100 != cell84 )).

fof(tlhfof35082,axiom,(
    cell100 != cell83 )).

fof(tlhfof35083,axiom,(
    cell100 != cell82 )).

fof(tlhfof35084,axiom,(
    cell100 != cell81 )).

fof(tlhfof35085,axiom,(
    cell100 != cell80 )).

fof(tlhfof35086,axiom,(
    cell100 != cell79 )).

fof(tlhfof35087,axiom,(
    cell100 != cell78 )).

fof(tlhfof35088,axiom,(
    cell100 != cell77 )).

fof(tlhfof35089,axiom,(
    cell100 != cell76 )).

fof(tlhfof35090,axiom,(
    cell100 != cell75 )).

fof(tlhfof35091,axiom,(
    cell100 != cell74 )).

fof(tlhfof35092,axiom,(
    cell100 != cell73 )).

fof(tlhfof35093,axiom,(
    cell100 != cell72 )).

fof(tlhfof35094,axiom,(
    cell100 != cell71 )).

fof(tlhfof35095,axiom,(
    cell100 != cell70 )).

fof(tlhfof35096,axiom,(
    cell100 != cell69 )).

fof(tlhfof35097,axiom,(
    cell100 != cell68 )).

fof(tlhfof35098,axiom,(
    cell100 != cell67 )).

fof(tlhfof35099,axiom,(
    cell100 != cell66 )).

fof(tlhfof35100,axiom,(
    cell100 != cell65 )).

fof(tlhfof35101,axiom,(
    cell100 != cell64 )).

fof(tlhfof35102,axiom,(
    cell100 != cell63 )).

fof(tlhfof35103,axiom,(
    cell100 != cell62 )).

fof(tlhfof35104,axiom,(
    cell100 != cell61 )).

fof(tlhfof35105,axiom,(
    cell100 != cell60 )).

fof(tlhfof35106,axiom,(
    cell100 != cell59 )).

fof(tlhfof35107,axiom,(
    cell100 != cell58 )).

fof(tlhfof35108,axiom,(
    cell100 != cell57 )).

fof(tlhfof35109,axiom,(
    cell100 != cell56 )).

fof(tlhfof35110,axiom,(
    cell100 != cell55 )).

fof(tlhfof35111,axiom,(
    cell100 != cell54 )).

fof(tlhfof35112,axiom,(
    cell100 != cell53 )).

fof(tlhfof35113,axiom,(
    cell100 != cell52 )).

fof(tlhfof35114,axiom,(
    cell100 != cell51 )).

fof(tlhfof35115,axiom,(
    cell100 != cell50 )).

fof(tlhfof35116,axiom,(
    cell100 != cell49 )).

fof(tlhfof35117,axiom,(
    cell100 != cell48 )).

fof(tlhfof35118,axiom,(
    cell100 != cell47 )).

fof(tlhfof35119,axiom,(
    cell100 != cell46 )).

fof(tlhfof35120,axiom,(
    cell100 != cell45 )).

fof(tlhfof35121,axiom,(
    cell100 != cell44 )).

fof(tlhfof35122,axiom,(
    cell100 != cell43 )).

fof(tlhfof35123,axiom,(
    cell100 != cell42 )).

fof(tlhfof35124,axiom,(
    cell100 != cell41 )).

fof(tlhfof35125,axiom,(
    cell100 != cell40 )).

fof(tlhfof35126,axiom,(
    cell100 != cell39 )).

fof(tlhfof35127,axiom,(
    cell100 != cell38 )).

fof(tlhfof35128,axiom,(
    cell100 != cell37 )).

fof(tlhfof35129,axiom,(
    cell100 != cell36 )).

fof(tlhfof35130,axiom,(
    cell100 != cell35 )).

fof(tlhfof35131,axiom,(
    cell100 != cell34 )).

fof(tlhfof35132,axiom,(
    cell100 != cell33 )).

fof(tlhfof35133,axiom,(
    cell100 != cell32 )).

fof(tlhfof35134,axiom,(
    cell100 != cell31 )).

fof(tlhfof35135,axiom,(
    cell100 != cell30 )).

fof(tlhfof35136,axiom,(
    cell100 != cell29 )).

fof(tlhfof35137,axiom,(
    cell100 != cell28 )).

fof(tlhfof35138,axiom,(
    cell100 != cell27 )).

fof(tlhfof35139,axiom,(
    cell100 != cell26 )).

fof(tlhfof35140,axiom,(
    cell100 != cell25 )).

fof(tlhfof35141,axiom,(
    cell100 != cell24 )).

fof(tlhfof35142,axiom,(
    cell100 != cell23 )).

fof(tlhfof35143,axiom,(
    cell100 != cell22 )).

fof(tlhfof35144,axiom,(
    cell100 != cell21 )).

fof(tlhfof35145,axiom,(
    cell100 != cell10 )).

fof(tlhfof35146,axiom,(
    cell100 != cell9 )).

fof(tlhfof35147,axiom,(
    cell100 != cell19 )).

fof(tlhfof35148,axiom,(
    cell100 != cell8 )).

fof(tlhfof35149,axiom,(
    cell100 != cell18 )).

fof(tlhfof35150,axiom,(
    cell100 != cell7 )).

fof(tlhfof35151,axiom,(
    cell100 != cell17 )).

fof(tlhfof35152,axiom,(
    cell100 != cell6 )).

fof(tlhfof35153,axiom,(
    cell100 != cell16 )).

fof(tlhfof35154,axiom,(
    cell100 != cell5 )).

fof(tlhfof35155,axiom,(
    cell100 != cell15 )).

fof(tlhfof35156,axiom,(
    cell100 != cell4 )).

fof(tlhfof35157,axiom,(
    cell100 != cell14 )).

fof(tlhfof35158,axiom,(
    cell100 != cell3 )).

fof(tlhfof35159,axiom,(
    cell100 != cell13 )).

fof(tlhfof35160,axiom,(
    cell100 != cell2 )).

fof(tlhfof35161,axiom,(
    cell100 != cell12 )).

fof(tlhfof35162,axiom,(
    cell100 != cell1 )).

fof(tlhfof35163,axiom,(
    cell100 != cell11 )).

fof(tlhfof35164,axiom,(
    cell100 != cell20 )).

fof(tlhfof35165,axiom,(
    cell99 != cell98 )).

fof(tlhfof35166,axiom,(
    cell99 != cell97 )).

fof(tlhfof35167,axiom,(
    cell99 != cell96 )).

fof(tlhfof35168,axiom,(
    cell99 != cell95 )).

fof(tlhfof35169,axiom,(
    cell99 != cell94 )).

fof(tlhfof35170,axiom,(
    cell99 != cell93 )).

fof(tlhfof35171,axiom,(
    cell99 != cell92 )).

fof(tlhfof35172,axiom,(
    cell99 != cell91 )).

fof(tlhfof35173,axiom,(
    cell99 != cell90 )).

fof(tlhfof35174,axiom,(
    cell99 != cell89 )).

fof(tlhfof35175,axiom,(
    cell99 != cell88 )).

fof(tlhfof35176,axiom,(
    cell99 != cell87 )).

fof(tlhfof35177,axiom,(
    cell99 != cell86 )).

fof(tlhfof35178,axiom,(
    cell99 != cell85 )).

fof(tlhfof35179,axiom,(
    cell99 != cell84 )).

fof(tlhfof35180,axiom,(
    cell99 != cell83 )).

fof(tlhfof35181,axiom,(
    cell99 != cell82 )).

fof(tlhfof35182,axiom,(
    cell99 != cell81 )).

fof(tlhfof35183,axiom,(
    cell99 != cell80 )).

fof(tlhfof35184,axiom,(
    cell99 != cell79 )).

fof(tlhfof35185,axiom,(
    cell99 != cell78 )).

fof(tlhfof35186,axiom,(
    cell99 != cell77 )).

fof(tlhfof35187,axiom,(
    cell99 != cell76 )).

fof(tlhfof35188,axiom,(
    cell99 != cell75 )).

fof(tlhfof35189,axiom,(
    cell99 != cell74 )).

fof(tlhfof35190,axiom,(
    cell99 != cell73 )).

fof(tlhfof35191,axiom,(
    cell99 != cell72 )).

fof(tlhfof35192,axiom,(
    cell99 != cell71 )).

fof(tlhfof35193,axiom,(
    cell99 != cell70 )).

fof(tlhfof35194,axiom,(
    cell99 != cell69 )).

fof(tlhfof35195,axiom,(
    cell99 != cell68 )).

fof(tlhfof35196,axiom,(
    cell99 != cell67 )).

fof(tlhfof35197,axiom,(
    cell99 != cell66 )).

fof(tlhfof35198,axiom,(
    cell99 != cell65 )).

fof(tlhfof35199,axiom,(
    cell99 != cell64 )).

fof(tlhfof35200,axiom,(
    cell99 != cell63 )).

fof(tlhfof35201,axiom,(
    cell99 != cell62 )).

fof(tlhfof35202,axiom,(
    cell99 != cell61 )).

fof(tlhfof35203,axiom,(
    cell99 != cell60 )).

fof(tlhfof35204,axiom,(
    cell99 != cell59 )).

fof(tlhfof35205,axiom,(
    cell99 != cell58 )).

fof(tlhfof35206,axiom,(
    cell99 != cell57 )).

fof(tlhfof35207,axiom,(
    cell99 != cell56 )).

fof(tlhfof35208,axiom,(
    cell99 != cell55 )).

fof(tlhfof35209,axiom,(
    cell99 != cell54 )).

fof(tlhfof35210,axiom,(
    cell99 != cell53 )).

fof(tlhfof35211,axiom,(
    cell99 != cell52 )).

fof(tlhfof35212,axiom,(
    cell99 != cell51 )).

fof(tlhfof35213,axiom,(
    cell99 != cell50 )).

fof(tlhfof35214,axiom,(
    cell99 != cell49 )).

fof(tlhfof35215,axiom,(
    cell99 != cell48 )).

fof(tlhfof35216,axiom,(
    cell99 != cell47 )).

fof(tlhfof35217,axiom,(
    cell99 != cell46 )).

fof(tlhfof35218,axiom,(
    cell99 != cell45 )).

fof(tlhfof35219,axiom,(
    cell99 != cell44 )).

fof(tlhfof35220,axiom,(
    cell99 != cell43 )).

fof(tlhfof35221,axiom,(
    cell99 != cell42 )).

fof(tlhfof35222,axiom,(
    cell99 != cell41 )).

fof(tlhfof35223,axiom,(
    cell99 != cell40 )).

fof(tlhfof35224,axiom,(
    cell99 != cell39 )).

fof(tlhfof35225,axiom,(
    cell99 != cell38 )).

fof(tlhfof35226,axiom,(
    cell99 != cell37 )).

fof(tlhfof35227,axiom,(
    cell99 != cell36 )).

fof(tlhfof35228,axiom,(
    cell99 != cell35 )).

fof(tlhfof35229,axiom,(
    cell99 != cell34 )).

fof(tlhfof35230,axiom,(
    cell99 != cell33 )).

fof(tlhfof35231,axiom,(
    cell99 != cell32 )).

fof(tlhfof35232,axiom,(
    cell99 != cell31 )).

fof(tlhfof35233,axiom,(
    cell99 != cell30 )).

fof(tlhfof35234,axiom,(
    cell99 != cell29 )).

fof(tlhfof35235,axiom,(
    cell99 != cell28 )).

fof(tlhfof35236,axiom,(
    cell99 != cell27 )).

fof(tlhfof35237,axiom,(
    cell99 != cell26 )).

fof(tlhfof35238,axiom,(
    cell99 != cell25 )).

fof(tlhfof35239,axiom,(
    cell99 != cell24 )).

fof(tlhfof35240,axiom,(
    cell99 != cell23 )).

fof(tlhfof35241,axiom,(
    cell99 != cell22 )).

fof(tlhfof35242,axiom,(
    cell99 != cell21 )).

fof(tlhfof35243,axiom,(
    cell99 != cell10 )).

fof(tlhfof35244,axiom,(
    cell99 != cell9 )).

fof(tlhfof35245,axiom,(
    cell99 != cell19 )).

fof(tlhfof35246,axiom,(
    cell99 != cell8 )).

fof(tlhfof35247,axiom,(
    cell99 != cell18 )).

fof(tlhfof35248,axiom,(
    cell99 != cell7 )).

fof(tlhfof35249,axiom,(
    cell99 != cell17 )).

fof(tlhfof35250,axiom,(
    cell99 != cell6 )).

fof(tlhfof35251,axiom,(
    cell99 != cell16 )).

fof(tlhfof35252,axiom,(
    cell99 != cell5 )).

fof(tlhfof35253,axiom,(
    cell99 != cell15 )).

fof(tlhfof35254,axiom,(
    cell99 != cell4 )).

fof(tlhfof35255,axiom,(
    cell99 != cell14 )).

fof(tlhfof35256,axiom,(
    cell99 != cell3 )).

fof(tlhfof35257,axiom,(
    cell99 != cell13 )).

fof(tlhfof35258,axiom,(
    cell99 != cell2 )).

fof(tlhfof35259,axiom,(
    cell99 != cell12 )).

fof(tlhfof35260,axiom,(
    cell99 != cell1 )).

fof(tlhfof35261,axiom,(
    cell99 != cell11 )).

fof(tlhfof35262,axiom,(
    cell99 != cell20 )).

fof(tlhfof35263,axiom,(
    cell98 != cell97 )).

fof(tlhfof35264,axiom,(
    cell98 != cell96 )).

fof(tlhfof35265,axiom,(
    cell98 != cell95 )).

fof(tlhfof35266,axiom,(
    cell98 != cell94 )).

fof(tlhfof35267,axiom,(
    cell98 != cell93 )).

fof(tlhfof35268,axiom,(
    cell98 != cell92 )).

fof(tlhfof35269,axiom,(
    cell98 != cell91 )).

fof(tlhfof35270,axiom,(
    cell98 != cell90 )).

fof(tlhfof35271,axiom,(
    cell98 != cell89 )).

fof(tlhfof35272,axiom,(
    cell98 != cell88 )).

fof(tlhfof35273,axiom,(
    cell98 != cell87 )).

fof(tlhfof35274,axiom,(
    cell98 != cell86 )).

fof(tlhfof35275,axiom,(
    cell98 != cell85 )).

fof(tlhfof35276,axiom,(
    cell98 != cell84 )).

fof(tlhfof35277,axiom,(
    cell98 != cell83 )).

fof(tlhfof35278,axiom,(
    cell98 != cell82 )).

fof(tlhfof35279,axiom,(
    cell98 != cell81 )).

fof(tlhfof35280,axiom,(
    cell98 != cell80 )).

fof(tlhfof35281,axiom,(
    cell98 != cell79 )).

fof(tlhfof35282,axiom,(
    cell98 != cell78 )).

fof(tlhfof35283,axiom,(
    cell98 != cell77 )).

fof(tlhfof35284,axiom,(
    cell98 != cell76 )).

fof(tlhfof35285,axiom,(
    cell98 != cell75 )).

fof(tlhfof35286,axiom,(
    cell98 != cell74 )).

fof(tlhfof35287,axiom,(
    cell98 != cell73 )).

fof(tlhfof35288,axiom,(
    cell98 != cell72 )).

fof(tlhfof35289,axiom,(
    cell98 != cell71 )).

fof(tlhfof35290,axiom,(
    cell98 != cell70 )).

fof(tlhfof35291,axiom,(
    cell98 != cell69 )).

fof(tlhfof35292,axiom,(
    cell98 != cell68 )).

fof(tlhfof35293,axiom,(
    cell98 != cell67 )).

fof(tlhfof35294,axiom,(
    cell98 != cell66 )).

fof(tlhfof35295,axiom,(
    cell98 != cell65 )).

fof(tlhfof35296,axiom,(
    cell98 != cell64 )).

fof(tlhfof35297,axiom,(
    cell98 != cell63 )).

fof(tlhfof35298,axiom,(
    cell98 != cell62 )).

fof(tlhfof35299,axiom,(
    cell98 != cell61 )).

fof(tlhfof35300,axiom,(
    cell98 != cell60 )).

fof(tlhfof35301,axiom,(
    cell98 != cell59 )).

fof(tlhfof35302,axiom,(
    cell98 != cell58 )).

fof(tlhfof35303,axiom,(
    cell98 != cell57 )).

fof(tlhfof35304,axiom,(
    cell98 != cell56 )).

fof(tlhfof35305,axiom,(
    cell98 != cell55 )).

fof(tlhfof35306,axiom,(
    cell98 != cell54 )).

fof(tlhfof35307,axiom,(
    cell98 != cell53 )).

fof(tlhfof35308,axiom,(
    cell98 != cell52 )).

fof(tlhfof35309,axiom,(
    cell98 != cell51 )).

fof(tlhfof35310,axiom,(
    cell98 != cell50 )).

fof(tlhfof35311,axiom,(
    cell98 != cell49 )).

fof(tlhfof35312,axiom,(
    cell98 != cell48 )).

fof(tlhfof35313,axiom,(
    cell98 != cell47 )).

fof(tlhfof35314,axiom,(
    cell98 != cell46 )).

fof(tlhfof35315,axiom,(
    cell98 != cell45 )).

fof(tlhfof35316,axiom,(
    cell98 != cell44 )).

fof(tlhfof35317,axiom,(
    cell98 != cell43 )).

fof(tlhfof35318,axiom,(
    cell98 != cell42 )).

fof(tlhfof35319,axiom,(
    cell98 != cell41 )).

fof(tlhfof35320,axiom,(
    cell98 != cell40 )).

fof(tlhfof35321,axiom,(
    cell98 != cell39 )).

fof(tlhfof35322,axiom,(
    cell98 != cell38 )).

fof(tlhfof35323,axiom,(
    cell98 != cell37 )).

fof(tlhfof35324,axiom,(
    cell98 != cell36 )).

fof(tlhfof35325,axiom,(
    cell98 != cell35 )).

fof(tlhfof35326,axiom,(
    cell98 != cell34 )).

fof(tlhfof35327,axiom,(
    cell98 != cell33 )).

fof(tlhfof35328,axiom,(
    cell98 != cell32 )).

fof(tlhfof35329,axiom,(
    cell98 != cell31 )).

fof(tlhfof35330,axiom,(
    cell98 != cell30 )).

fof(tlhfof35331,axiom,(
    cell98 != cell29 )).

fof(tlhfof35332,axiom,(
    cell98 != cell28 )).

fof(tlhfof35333,axiom,(
    cell98 != cell27 )).

fof(tlhfof35334,axiom,(
    cell98 != cell26 )).

fof(tlhfof35335,axiom,(
    cell98 != cell25 )).

fof(tlhfof35336,axiom,(
    cell98 != cell24 )).

fof(tlhfof35337,axiom,(
    cell98 != cell23 )).

fof(tlhfof35338,axiom,(
    cell98 != cell22 )).

fof(tlhfof35339,axiom,(
    cell98 != cell21 )).

fof(tlhfof35340,axiom,(
    cell98 != cell10 )).

fof(tlhfof35341,axiom,(
    cell98 != cell9 )).

fof(tlhfof35342,axiom,(
    cell98 != cell19 )).

fof(tlhfof35343,axiom,(
    cell98 != cell8 )).

fof(tlhfof35344,axiom,(
    cell98 != cell18 )).

fof(tlhfof35345,axiom,(
    cell98 != cell7 )).

fof(tlhfof35346,axiom,(
    cell98 != cell17 )).

fof(tlhfof35347,axiom,(
    cell98 != cell6 )).

fof(tlhfof35348,axiom,(
    cell98 != cell16 )).

fof(tlhfof35349,axiom,(
    cell98 != cell5 )).

fof(tlhfof35350,axiom,(
    cell98 != cell15 )).

fof(tlhfof35351,axiom,(
    cell98 != cell4 )).

fof(tlhfof35352,axiom,(
    cell98 != cell14 )).

fof(tlhfof35353,axiom,(
    cell98 != cell3 )).

fof(tlhfof35354,axiom,(
    cell98 != cell13 )).

fof(tlhfof35355,axiom,(
    cell98 != cell2 )).

fof(tlhfof35356,axiom,(
    cell98 != cell12 )).

fof(tlhfof35357,axiom,(
    cell98 != cell1 )).

fof(tlhfof35358,axiom,(
    cell98 != cell11 )).

fof(tlhfof35359,axiom,(
    cell98 != cell20 )).

fof(tlhfof35360,axiom,(
    cell97 != cell96 )).

fof(tlhfof35361,axiom,(
    cell97 != cell95 )).

fof(tlhfof35362,axiom,(
    cell97 != cell94 )).

fof(tlhfof35363,axiom,(
    cell97 != cell93 )).

fof(tlhfof35364,axiom,(
    cell97 != cell92 )).

fof(tlhfof35365,axiom,(
    cell97 != cell91 )).

fof(tlhfof35366,axiom,(
    cell97 != cell90 )).

fof(tlhfof35367,axiom,(
    cell97 != cell89 )).

fof(tlhfof35368,axiom,(
    cell97 != cell88 )).

fof(tlhfof35369,axiom,(
    cell97 != cell87 )).

fof(tlhfof35370,axiom,(
    cell97 != cell86 )).

fof(tlhfof35371,axiom,(
    cell97 != cell85 )).

fof(tlhfof35372,axiom,(
    cell97 != cell84 )).

fof(tlhfof35373,axiom,(
    cell97 != cell83 )).

fof(tlhfof35374,axiom,(
    cell97 != cell82 )).

fof(tlhfof35375,axiom,(
    cell97 != cell81 )).

fof(tlhfof35376,axiom,(
    cell97 != cell80 )).

fof(tlhfof35377,axiom,(
    cell97 != cell79 )).

fof(tlhfof35378,axiom,(
    cell97 != cell78 )).

fof(tlhfof35379,axiom,(
    cell97 != cell77 )).

fof(tlhfof35380,axiom,(
    cell97 != cell76 )).

fof(tlhfof35381,axiom,(
    cell97 != cell75 )).

fof(tlhfof35382,axiom,(
    cell97 != cell74 )).

fof(tlhfof35383,axiom,(
    cell97 != cell73 )).

fof(tlhfof35384,axiom,(
    cell97 != cell72 )).

fof(tlhfof35385,axiom,(
    cell97 != cell71 )).

fof(tlhfof35386,axiom,(
    cell97 != cell70 )).

fof(tlhfof35387,axiom,(
    cell97 != cell69 )).

fof(tlhfof35388,axiom,(
    cell97 != cell68 )).

fof(tlhfof35389,axiom,(
    cell97 != cell67 )).

fof(tlhfof35390,axiom,(
    cell97 != cell66 )).

fof(tlhfof35391,axiom,(
    cell97 != cell65 )).

fof(tlhfof35392,axiom,(
    cell97 != cell64 )).

fof(tlhfof35393,axiom,(
    cell97 != cell63 )).

fof(tlhfof35394,axiom,(
    cell97 != cell62 )).

fof(tlhfof35395,axiom,(
    cell97 != cell61 )).

fof(tlhfof35396,axiom,(
    cell97 != cell60 )).

fof(tlhfof35397,axiom,(
    cell97 != cell59 )).

fof(tlhfof35398,axiom,(
    cell97 != cell58 )).

fof(tlhfof35399,axiom,(
    cell97 != cell57 )).

fof(tlhfof35400,axiom,(
    cell97 != cell56 )).

fof(tlhfof35401,axiom,(
    cell97 != cell55 )).

fof(tlhfof35402,axiom,(
    cell97 != cell54 )).

fof(tlhfof35403,axiom,(
    cell97 != cell53 )).

fof(tlhfof35404,axiom,(
    cell97 != cell52 )).

fof(tlhfof35405,axiom,(
    cell97 != cell51 )).

fof(tlhfof35406,axiom,(
    cell97 != cell50 )).

fof(tlhfof35407,axiom,(
    cell97 != cell49 )).

fof(tlhfof35408,axiom,(
    cell97 != cell48 )).

fof(tlhfof35409,axiom,(
    cell97 != cell47 )).

fof(tlhfof35410,axiom,(
    cell97 != cell46 )).

fof(tlhfof35411,axiom,(
    cell97 != cell45 )).

fof(tlhfof35412,axiom,(
    cell97 != cell44 )).

fof(tlhfof35413,axiom,(
    cell97 != cell43 )).

fof(tlhfof35414,axiom,(
    cell97 != cell42 )).

fof(tlhfof35415,axiom,(
    cell97 != cell41 )).

fof(tlhfof35416,axiom,(
    cell97 != cell40 )).

fof(tlhfof35417,axiom,(
    cell97 != cell39 )).

fof(tlhfof35418,axiom,(
    cell97 != cell38 )).

fof(tlhfof35419,axiom,(
    cell97 != cell37 )).

fof(tlhfof35420,axiom,(
    cell97 != cell36 )).

fof(tlhfof35421,axiom,(
    cell97 != cell35 )).

fof(tlhfof35422,axiom,(
    cell97 != cell34 )).

fof(tlhfof35423,axiom,(
    cell97 != cell33 )).

fof(tlhfof35424,axiom,(
    cell97 != cell32 )).

fof(tlhfof35425,axiom,(
    cell97 != cell31 )).

fof(tlhfof35426,axiom,(
    cell97 != cell30 )).

fof(tlhfof35427,axiom,(
    cell97 != cell29 )).

fof(tlhfof35428,axiom,(
    cell97 != cell28 )).

fof(tlhfof35429,axiom,(
    cell97 != cell27 )).

fof(tlhfof35430,axiom,(
    cell97 != cell26 )).

fof(tlhfof35431,axiom,(
    cell97 != cell25 )).

fof(tlhfof35432,axiom,(
    cell97 != cell24 )).

fof(tlhfof35433,axiom,(
    cell97 != cell23 )).

fof(tlhfof35434,axiom,(
    cell97 != cell22 )).

fof(tlhfof35435,axiom,(
    cell97 != cell21 )).

fof(tlhfof35436,axiom,(
    cell97 != cell10 )).

fof(tlhfof35437,axiom,(
    cell97 != cell9 )).

fof(tlhfof35438,axiom,(
    cell97 != cell19 )).

fof(tlhfof35439,axiom,(
    cell97 != cell8 )).

fof(tlhfof35440,axiom,(
    cell97 != cell18 )).

fof(tlhfof35441,axiom,(
    cell97 != cell7 )).

fof(tlhfof35442,axiom,(
    cell97 != cell17 )).

fof(tlhfof35443,axiom,(
    cell97 != cell6 )).

fof(tlhfof35444,axiom,(
    cell97 != cell16 )).

fof(tlhfof35445,axiom,(
    cell97 != cell5 )).

fof(tlhfof35446,axiom,(
    cell97 != cell15 )).

fof(tlhfof35447,axiom,(
    cell97 != cell4 )).

fof(tlhfof35448,axiom,(
    cell97 != cell14 )).

fof(tlhfof35449,axiom,(
    cell97 != cell3 )).

fof(tlhfof35450,axiom,(
    cell97 != cell13 )).

fof(tlhfof35451,axiom,(
    cell97 != cell2 )).

fof(tlhfof35452,axiom,(
    cell97 != cell12 )).

fof(tlhfof35453,axiom,(
    cell97 != cell1 )).

fof(tlhfof35454,axiom,(
    cell97 != cell11 )).

fof(tlhfof35455,axiom,(
    cell97 != cell20 )).

fof(tlhfof35456,axiom,(
    cell96 != cell95 )).

fof(tlhfof35457,axiom,(
    cell96 != cell94 )).

fof(tlhfof35458,axiom,(
    cell96 != cell93 )).

fof(tlhfof35459,axiom,(
    cell96 != cell92 )).

fof(tlhfof35460,axiom,(
    cell96 != cell91 )).

fof(tlhfof35461,axiom,(
    cell96 != cell90 )).

fof(tlhfof35462,axiom,(
    cell96 != cell89 )).

fof(tlhfof35463,axiom,(
    cell96 != cell88 )).

fof(tlhfof35464,axiom,(
    cell96 != cell87 )).

fof(tlhfof35465,axiom,(
    cell96 != cell86 )).

fof(tlhfof35466,axiom,(
    cell96 != cell85 )).

fof(tlhfof35467,axiom,(
    cell96 != cell84 )).

fof(tlhfof35468,axiom,(
    cell96 != cell83 )).

fof(tlhfof35469,axiom,(
    cell96 != cell82 )).

fof(tlhfof35470,axiom,(
    cell96 != cell81 )).

fof(tlhfof35471,axiom,(
    cell96 != cell80 )).

fof(tlhfof35472,axiom,(
    cell96 != cell79 )).

fof(tlhfof35473,axiom,(
    cell96 != cell78 )).

fof(tlhfof35474,axiom,(
    cell96 != cell77 )).

fof(tlhfof35475,axiom,(
    cell96 != cell76 )).

fof(tlhfof35476,axiom,(
    cell96 != cell75 )).

fof(tlhfof35477,axiom,(
    cell96 != cell74 )).

fof(tlhfof35478,axiom,(
    cell96 != cell73 )).

fof(tlhfof35479,axiom,(
    cell96 != cell72 )).

fof(tlhfof35480,axiom,(
    cell96 != cell71 )).

fof(tlhfof35481,axiom,(
    cell96 != cell70 )).

fof(tlhfof35482,axiom,(
    cell96 != cell69 )).

fof(tlhfof35483,axiom,(
    cell96 != cell68 )).

fof(tlhfof35484,axiom,(
    cell96 != cell67 )).

fof(tlhfof35485,axiom,(
    cell96 != cell66 )).

fof(tlhfof35486,axiom,(
    cell96 != cell65 )).

fof(tlhfof35487,axiom,(
    cell96 != cell64 )).

fof(tlhfof35488,axiom,(
    cell96 != cell63 )).

fof(tlhfof35489,axiom,(
    cell96 != cell62 )).

fof(tlhfof35490,axiom,(
    cell96 != cell61 )).

fof(tlhfof35491,axiom,(
    cell96 != cell60 )).

fof(tlhfof35492,axiom,(
    cell96 != cell59 )).

fof(tlhfof35493,axiom,(
    cell96 != cell58 )).

fof(tlhfof35494,axiom,(
    cell96 != cell57 )).

fof(tlhfof35495,axiom,(
    cell96 != cell56 )).

fof(tlhfof35496,axiom,(
    cell96 != cell55 )).

fof(tlhfof35497,axiom,(
    cell96 != cell54 )).

fof(tlhfof35498,axiom,(
    cell96 != cell53 )).

fof(tlhfof35499,axiom,(
    cell96 != cell52 )).

fof(tlhfof35500,axiom,(
    cell96 != cell51 )).

fof(tlhfof35501,axiom,(
    cell96 != cell50 )).

fof(tlhfof35502,axiom,(
    cell96 != cell49 )).

fof(tlhfof35503,axiom,(
    cell96 != cell48 )).

fof(tlhfof35504,axiom,(
    cell96 != cell47 )).

fof(tlhfof35505,axiom,(
    cell96 != cell46 )).

fof(tlhfof35506,axiom,(
    cell96 != cell45 )).

fof(tlhfof35507,axiom,(
    cell96 != cell44 )).

fof(tlhfof35508,axiom,(
    cell96 != cell43 )).

fof(tlhfof35509,axiom,(
    cell96 != cell42 )).

fof(tlhfof35510,axiom,(
    cell96 != cell41 )).

fof(tlhfof35511,axiom,(
    cell96 != cell40 )).

fof(tlhfof35512,axiom,(
    cell96 != cell39 )).

fof(tlhfof35513,axiom,(
    cell96 != cell38 )).

fof(tlhfof35514,axiom,(
    cell96 != cell37 )).

fof(tlhfof35515,axiom,(
    cell96 != cell36 )).

fof(tlhfof35516,axiom,(
    cell96 != cell35 )).

fof(tlhfof35517,axiom,(
    cell96 != cell34 )).

fof(tlhfof35518,axiom,(
    cell96 != cell33 )).

fof(tlhfof35519,axiom,(
    cell96 != cell32 )).

fof(tlhfof35520,axiom,(
    cell96 != cell31 )).

fof(tlhfof35521,axiom,(
    cell96 != cell30 )).

fof(tlhfof35522,axiom,(
    cell96 != cell29 )).

fof(tlhfof35523,axiom,(
    cell96 != cell28 )).

fof(tlhfof35524,axiom,(
    cell96 != cell27 )).

fof(tlhfof35525,axiom,(
    cell96 != cell26 )).

fof(tlhfof35526,axiom,(
    cell96 != cell25 )).

fof(tlhfof35527,axiom,(
    cell96 != cell24 )).

fof(tlhfof35528,axiom,(
    cell96 != cell23 )).

fof(tlhfof35529,axiom,(
    cell96 != cell22 )).

fof(tlhfof35530,axiom,(
    cell96 != cell21 )).

fof(tlhfof35531,axiom,(
    cell96 != cell10 )).

fof(tlhfof35532,axiom,(
    cell96 != cell9 )).

fof(tlhfof35533,axiom,(
    cell96 != cell19 )).

fof(tlhfof35534,axiom,(
    cell96 != cell8 )).

fof(tlhfof35535,axiom,(
    cell96 != cell18 )).

fof(tlhfof35536,axiom,(
    cell96 != cell7 )).

fof(tlhfof35537,axiom,(
    cell96 != cell17 )).

fof(tlhfof35538,axiom,(
    cell96 != cell6 )).

fof(tlhfof35539,axiom,(
    cell96 != cell16 )).

fof(tlhfof35540,axiom,(
    cell96 != cell5 )).

fof(tlhfof35541,axiom,(
    cell96 != cell15 )).

fof(tlhfof35542,axiom,(
    cell96 != cell4 )).

fof(tlhfof35543,axiom,(
    cell96 != cell14 )).

fof(tlhfof35544,axiom,(
    cell96 != cell3 )).

fof(tlhfof35545,axiom,(
    cell96 != cell13 )).

fof(tlhfof35546,axiom,(
    cell96 != cell2 )).

fof(tlhfof35547,axiom,(
    cell96 != cell12 )).

fof(tlhfof35548,axiom,(
    cell96 != cell1 )).

fof(tlhfof35549,axiom,(
    cell96 != cell11 )).

fof(tlhfof35550,axiom,(
    cell96 != cell20 )).

fof(tlhfof35551,axiom,(
    cell95 != cell94 )).

fof(tlhfof35552,axiom,(
    cell95 != cell93 )).

fof(tlhfof35553,axiom,(
    cell95 != cell92 )).

fof(tlhfof35554,axiom,(
    cell95 != cell91 )).

fof(tlhfof35555,axiom,(
    cell95 != cell90 )).

fof(tlhfof35556,axiom,(
    cell95 != cell89 )).

fof(tlhfof35557,axiom,(
    cell95 != cell88 )).

fof(tlhfof35558,axiom,(
    cell95 != cell87 )).

fof(tlhfof35559,axiom,(
    cell95 != cell86 )).

fof(tlhfof35560,axiom,(
    cell95 != cell85 )).

fof(tlhfof35561,axiom,(
    cell95 != cell84 )).

fof(tlhfof35562,axiom,(
    cell95 != cell83 )).

fof(tlhfof35563,axiom,(
    cell95 != cell82 )).

fof(tlhfof35564,axiom,(
    cell95 != cell81 )).

fof(tlhfof35565,axiom,(
    cell95 != cell80 )).

fof(tlhfof35566,axiom,(
    cell95 != cell79 )).

fof(tlhfof35567,axiom,(
    cell95 != cell78 )).

fof(tlhfof35568,axiom,(
    cell95 != cell77 )).

fof(tlhfof35569,axiom,(
    cell95 != cell76 )).

fof(tlhfof35570,axiom,(
    cell95 != cell75 )).

fof(tlhfof35571,axiom,(
    cell95 != cell74 )).

fof(tlhfof35572,axiom,(
    cell95 != cell73 )).

fof(tlhfof35573,axiom,(
    cell95 != cell72 )).

fof(tlhfof35574,axiom,(
    cell95 != cell71 )).

fof(tlhfof35575,axiom,(
    cell95 != cell70 )).

fof(tlhfof35576,axiom,(
    cell95 != cell69 )).

fof(tlhfof35577,axiom,(
    cell95 != cell68 )).

fof(tlhfof35578,axiom,(
    cell95 != cell67 )).

fof(tlhfof35579,axiom,(
    cell95 != cell66 )).

fof(tlhfof35580,axiom,(
    cell95 != cell65 )).

fof(tlhfof35581,axiom,(
    cell95 != cell64 )).

fof(tlhfof35582,axiom,(
    cell95 != cell63 )).

fof(tlhfof35583,axiom,(
    cell95 != cell62 )).

fof(tlhfof35584,axiom,(
    cell95 != cell61 )).

fof(tlhfof35585,axiom,(
    cell95 != cell60 )).

fof(tlhfof35586,axiom,(
    cell95 != cell59 )).

fof(tlhfof35587,axiom,(
    cell95 != cell58 )).

fof(tlhfof35588,axiom,(
    cell95 != cell57 )).

fof(tlhfof35589,axiom,(
    cell95 != cell56 )).

fof(tlhfof35590,axiom,(
    cell95 != cell55 )).

fof(tlhfof35591,axiom,(
    cell95 != cell54 )).

fof(tlhfof35592,axiom,(
    cell95 != cell53 )).

fof(tlhfof35593,axiom,(
    cell95 != cell52 )).

fof(tlhfof35594,axiom,(
    cell95 != cell51 )).

fof(tlhfof35595,axiom,(
    cell95 != cell50 )).

fof(tlhfof35596,axiom,(
    cell95 != cell49 )).

fof(tlhfof35597,axiom,(
    cell95 != cell48 )).

fof(tlhfof35598,axiom,(
    cell95 != cell47 )).

fof(tlhfof35599,axiom,(
    cell95 != cell46 )).

fof(tlhfof35600,axiom,(
    cell95 != cell45 )).

fof(tlhfof35601,axiom,(
    cell95 != cell44 )).

fof(tlhfof35602,axiom,(
    cell95 != cell43 )).

fof(tlhfof35603,axiom,(
    cell95 != cell42 )).

fof(tlhfof35604,axiom,(
    cell95 != cell41 )).

fof(tlhfof35605,axiom,(
    cell95 != cell40 )).

fof(tlhfof35606,axiom,(
    cell95 != cell39 )).

fof(tlhfof35607,axiom,(
    cell95 != cell38 )).

fof(tlhfof35608,axiom,(
    cell95 != cell37 )).

fof(tlhfof35609,axiom,(
    cell95 != cell36 )).

fof(tlhfof35610,axiom,(
    cell95 != cell35 )).

fof(tlhfof35611,axiom,(
    cell95 != cell34 )).

fof(tlhfof35612,axiom,(
    cell95 != cell33 )).

fof(tlhfof35613,axiom,(
    cell95 != cell32 )).

fof(tlhfof35614,axiom,(
    cell95 != cell31 )).

fof(tlhfof35615,axiom,(
    cell95 != cell30 )).

fof(tlhfof35616,axiom,(
    cell95 != cell29 )).

fof(tlhfof35617,axiom,(
    cell95 != cell28 )).

fof(tlhfof35618,axiom,(
    cell95 != cell27 )).

fof(tlhfof35619,axiom,(
    cell95 != cell26 )).

fof(tlhfof35620,axiom,(
    cell95 != cell25 )).

fof(tlhfof35621,axiom,(
    cell95 != cell24 )).

fof(tlhfof35622,axiom,(
    cell95 != cell23 )).

fof(tlhfof35623,axiom,(
    cell95 != cell22 )).

fof(tlhfof35624,axiom,(
    cell95 != cell21 )).

fof(tlhfof35625,axiom,(
    cell95 != cell10 )).

fof(tlhfof35626,axiom,(
    cell95 != cell9 )).

fof(tlhfof35627,axiom,(
    cell95 != cell19 )).

fof(tlhfof35628,axiom,(
    cell95 != cell8 )).

fof(tlhfof35629,axiom,(
    cell95 != cell18 )).

fof(tlhfof35630,axiom,(
    cell95 != cell7 )).

fof(tlhfof35631,axiom,(
    cell95 != cell17 )).

fof(tlhfof35632,axiom,(
    cell95 != cell6 )).

fof(tlhfof35633,axiom,(
    cell95 != cell16 )).

fof(tlhfof35634,axiom,(
    cell95 != cell5 )).

fof(tlhfof35635,axiom,(
    cell95 != cell15 )).

fof(tlhfof35636,axiom,(
    cell95 != cell4 )).

fof(tlhfof35637,axiom,(
    cell95 != cell14 )).

fof(tlhfof35638,axiom,(
    cell95 != cell3 )).

fof(tlhfof35639,axiom,(
    cell95 != cell13 )).

fof(tlhfof35640,axiom,(
    cell95 != cell2 )).

fof(tlhfof35641,axiom,(
    cell95 != cell12 )).

fof(tlhfof35642,axiom,(
    cell95 != cell1 )).

fof(tlhfof35643,axiom,(
    cell95 != cell11 )).

fof(tlhfof35644,axiom,(
    cell95 != cell20 )).

fof(tlhfof35645,axiom,(
    cell94 != cell93 )).

fof(tlhfof35646,axiom,(
    cell94 != cell92 )).

fof(tlhfof35647,axiom,(
    cell94 != cell91 )).

fof(tlhfof35648,axiom,(
    cell94 != cell90 )).

fof(tlhfof35649,axiom,(
    cell94 != cell89 )).

fof(tlhfof35650,axiom,(
    cell94 != cell88 )).

fof(tlhfof35651,axiom,(
    cell94 != cell87 )).

fof(tlhfof35652,axiom,(
    cell94 != cell86 )).

fof(tlhfof35653,axiom,(
    cell94 != cell85 )).

fof(tlhfof35654,axiom,(
    cell94 != cell84 )).

fof(tlhfof35655,axiom,(
    cell94 != cell83 )).

fof(tlhfof35656,axiom,(
    cell94 != cell82 )).

fof(tlhfof35657,axiom,(
    cell94 != cell81 )).

fof(tlhfof35658,axiom,(
    cell94 != cell80 )).

fof(tlhfof35659,axiom,(
    cell94 != cell79 )).

fof(tlhfof35660,axiom,(
    cell94 != cell78 )).

fof(tlhfof35661,axiom,(
    cell94 != cell77 )).

fof(tlhfof35662,axiom,(
    cell94 != cell76 )).

fof(tlhfof35663,axiom,(
    cell94 != cell75 )).

fof(tlhfof35664,axiom,(
    cell94 != cell74 )).

fof(tlhfof35665,axiom,(
    cell94 != cell73 )).

fof(tlhfof35666,axiom,(
    cell94 != cell72 )).

fof(tlhfof35667,axiom,(
    cell94 != cell71 )).

fof(tlhfof35668,axiom,(
    cell94 != cell70 )).

fof(tlhfof35669,axiom,(
    cell94 != cell69 )).

fof(tlhfof35670,axiom,(
    cell94 != cell68 )).

fof(tlhfof35671,axiom,(
    cell94 != cell67 )).

fof(tlhfof35672,axiom,(
    cell94 != cell66 )).

fof(tlhfof35673,axiom,(
    cell94 != cell65 )).

fof(tlhfof35674,axiom,(
    cell94 != cell64 )).

fof(tlhfof35675,axiom,(
    cell94 != cell63 )).

fof(tlhfof35676,axiom,(
    cell94 != cell62 )).

fof(tlhfof35677,axiom,(
    cell94 != cell61 )).

fof(tlhfof35678,axiom,(
    cell94 != cell60 )).

fof(tlhfof35679,axiom,(
    cell94 != cell59 )).

fof(tlhfof35680,axiom,(
    cell94 != cell58 )).

fof(tlhfof35681,axiom,(
    cell94 != cell57 )).

fof(tlhfof35682,axiom,(
    cell94 != cell56 )).

fof(tlhfof35683,axiom,(
    cell94 != cell55 )).

fof(tlhfof35684,axiom,(
    cell94 != cell54 )).

fof(tlhfof35685,axiom,(
    cell94 != cell53 )).

fof(tlhfof35686,axiom,(
    cell94 != cell52 )).

fof(tlhfof35687,axiom,(
    cell94 != cell51 )).

fof(tlhfof35688,axiom,(
    cell94 != cell50 )).

fof(tlhfof35689,axiom,(
    cell94 != cell49 )).

fof(tlhfof35690,axiom,(
    cell94 != cell48 )).

fof(tlhfof35691,axiom,(
    cell94 != cell47 )).

fof(tlhfof35692,axiom,(
    cell94 != cell46 )).

fof(tlhfof35693,axiom,(
    cell94 != cell45 )).

fof(tlhfof35694,axiom,(
    cell94 != cell44 )).

fof(tlhfof35695,axiom,(
    cell94 != cell43 )).

fof(tlhfof35696,axiom,(
    cell94 != cell42 )).

fof(tlhfof35697,axiom,(
    cell94 != cell41 )).

fof(tlhfof35698,axiom,(
    cell94 != cell40 )).

fof(tlhfof35699,axiom,(
    cell94 != cell39 )).

fof(tlhfof35700,axiom,(
    cell94 != cell38 )).

fof(tlhfof35701,axiom,(
    cell94 != cell37 )).

fof(tlhfof35702,axiom,(
    cell94 != cell36 )).

fof(tlhfof35703,axiom,(
    cell94 != cell35 )).

fof(tlhfof35704,axiom,(
    cell94 != cell34 )).

fof(tlhfof35705,axiom,(
    cell94 != cell33 )).

fof(tlhfof35706,axiom,(
    cell94 != cell32 )).

fof(tlhfof35707,axiom,(
    cell94 != cell31 )).

fof(tlhfof35708,axiom,(
    cell94 != cell30 )).

fof(tlhfof35709,axiom,(
    cell94 != cell29 )).

fof(tlhfof35710,axiom,(
    cell94 != cell28 )).

fof(tlhfof35711,axiom,(
    cell94 != cell27 )).

fof(tlhfof35712,axiom,(
    cell94 != cell26 )).

fof(tlhfof35713,axiom,(
    cell94 != cell25 )).

fof(tlhfof35714,axiom,(
    cell94 != cell24 )).

fof(tlhfof35715,axiom,(
    cell94 != cell23 )).

fof(tlhfof35716,axiom,(
    cell94 != cell22 )).

fof(tlhfof35717,axiom,(
    cell94 != cell21 )).

fof(tlhfof35718,axiom,(
    cell94 != cell10 )).

fof(tlhfof35719,axiom,(
    cell94 != cell9 )).

fof(tlhfof35720,axiom,(
    cell94 != cell19 )).

fof(tlhfof35721,axiom,(
    cell94 != cell8 )).

fof(tlhfof35722,axiom,(
    cell94 != cell18 )).

fof(tlhfof35723,axiom,(
    cell94 != cell7 )).

fof(tlhfof35724,axiom,(
    cell94 != cell17 )).

fof(tlhfof35725,axiom,(
    cell94 != cell6 )).

fof(tlhfof35726,axiom,(
    cell94 != cell16 )).

fof(tlhfof35727,axiom,(
    cell94 != cell5 )).

fof(tlhfof35728,axiom,(
    cell94 != cell15 )).

fof(tlhfof35729,axiom,(
    cell94 != cell4 )).

fof(tlhfof35730,axiom,(
    cell94 != cell14 )).

fof(tlhfof35731,axiom,(
    cell94 != cell3 )).

fof(tlhfof35732,axiom,(
    cell94 != cell13 )).

fof(tlhfof35733,axiom,(
    cell94 != cell2 )).

fof(tlhfof35734,axiom,(
    cell94 != cell12 )).

fof(tlhfof35735,axiom,(
    cell94 != cell1 )).

fof(tlhfof35736,axiom,(
    cell94 != cell11 )).

fof(tlhfof35737,axiom,(
    cell94 != cell20 )).

fof(tlhfof35738,axiom,(
    cell93 != cell92 )).

fof(tlhfof35739,axiom,(
    cell93 != cell91 )).

fof(tlhfof35740,axiom,(
    cell93 != cell90 )).

fof(tlhfof35741,axiom,(
    cell93 != cell89 )).

fof(tlhfof35742,axiom,(
    cell93 != cell88 )).

fof(tlhfof35743,axiom,(
    cell93 != cell87 )).

fof(tlhfof35744,axiom,(
    cell93 != cell86 )).

fof(tlhfof35745,axiom,(
    cell93 != cell85 )).

fof(tlhfof35746,axiom,(
    cell93 != cell84 )).

fof(tlhfof35747,axiom,(
    cell93 != cell83 )).

fof(tlhfof35748,axiom,(
    cell93 != cell82 )).

fof(tlhfof35749,axiom,(
    cell93 != cell81 )).

fof(tlhfof35750,axiom,(
    cell93 != cell80 )).

fof(tlhfof35751,axiom,(
    cell93 != cell79 )).

fof(tlhfof35752,axiom,(
    cell93 != cell78 )).

fof(tlhfof35753,axiom,(
    cell93 != cell77 )).

fof(tlhfof35754,axiom,(
    cell93 != cell76 )).

fof(tlhfof35755,axiom,(
    cell93 != cell75 )).

fof(tlhfof35756,axiom,(
    cell93 != cell74 )).

fof(tlhfof35757,axiom,(
    cell93 != cell73 )).

fof(tlhfof35758,axiom,(
    cell93 != cell72 )).

fof(tlhfof35759,axiom,(
    cell93 != cell71 )).

fof(tlhfof35760,axiom,(
    cell93 != cell70 )).

fof(tlhfof35761,axiom,(
    cell93 != cell69 )).

fof(tlhfof35762,axiom,(
    cell93 != cell68 )).

fof(tlhfof35763,axiom,(
    cell93 != cell67 )).

fof(tlhfof35764,axiom,(
    cell93 != cell66 )).

fof(tlhfof35765,axiom,(
    cell93 != cell65 )).

fof(tlhfof35766,axiom,(
    cell93 != cell64 )).

fof(tlhfof35767,axiom,(
    cell93 != cell63 )).

fof(tlhfof35768,axiom,(
    cell93 != cell62 )).

fof(tlhfof35769,axiom,(
    cell93 != cell61 )).

fof(tlhfof35770,axiom,(
    cell93 != cell60 )).

fof(tlhfof35771,axiom,(
    cell93 != cell59 )).

fof(tlhfof35772,axiom,(
    cell93 != cell58 )).

fof(tlhfof35773,axiom,(
    cell93 != cell57 )).

fof(tlhfof35774,axiom,(
    cell93 != cell56 )).

fof(tlhfof35775,axiom,(
    cell93 != cell55 )).

fof(tlhfof35776,axiom,(
    cell93 != cell54 )).

fof(tlhfof35777,axiom,(
    cell93 != cell53 )).

fof(tlhfof35778,axiom,(
    cell93 != cell52 )).

fof(tlhfof35779,axiom,(
    cell93 != cell51 )).

fof(tlhfof35780,axiom,(
    cell93 != cell50 )).

fof(tlhfof35781,axiom,(
    cell93 != cell49 )).

fof(tlhfof35782,axiom,(
    cell93 != cell48 )).

fof(tlhfof35783,axiom,(
    cell93 != cell47 )).

fof(tlhfof35784,axiom,(
    cell93 != cell46 )).

fof(tlhfof35785,axiom,(
    cell93 != cell45 )).

fof(tlhfof35786,axiom,(
    cell93 != cell44 )).

fof(tlhfof35787,axiom,(
    cell93 != cell43 )).

fof(tlhfof35788,axiom,(
    cell93 != cell42 )).

fof(tlhfof35789,axiom,(
    cell93 != cell41 )).

fof(tlhfof35790,axiom,(
    cell93 != cell40 )).

fof(tlhfof35791,axiom,(
    cell93 != cell39 )).

fof(tlhfof35792,axiom,(
    cell93 != cell38 )).

fof(tlhfof35793,axiom,(
    cell93 != cell37 )).

fof(tlhfof35794,axiom,(
    cell93 != cell36 )).

fof(tlhfof35795,axiom,(
    cell93 != cell35 )).

fof(tlhfof35796,axiom,(
    cell93 != cell34 )).

fof(tlhfof35797,axiom,(
    cell93 != cell33 )).

fof(tlhfof35798,axiom,(
    cell93 != cell32 )).

fof(tlhfof35799,axiom,(
    cell93 != cell31 )).

fof(tlhfof35800,axiom,(
    cell93 != cell30 )).

fof(tlhfof35801,axiom,(
    cell93 != cell29 )).

fof(tlhfof35802,axiom,(
    cell93 != cell28 )).

fof(tlhfof35803,axiom,(
    cell93 != cell27 )).

fof(tlhfof35804,axiom,(
    cell93 != cell26 )).

fof(tlhfof35805,axiom,(
    cell93 != cell25 )).

fof(tlhfof35806,axiom,(
    cell93 != cell24 )).

fof(tlhfof35807,axiom,(
    cell93 != cell23 )).

fof(tlhfof35808,axiom,(
    cell93 != cell22 )).

fof(tlhfof35809,axiom,(
    cell93 != cell21 )).

fof(tlhfof35810,axiom,(
    cell93 != cell10 )).

fof(tlhfof35811,axiom,(
    cell93 != cell9 )).

fof(tlhfof35812,axiom,(
    cell93 != cell19 )).

fof(tlhfof35813,axiom,(
    cell93 != cell8 )).

fof(tlhfof35814,axiom,(
    cell93 != cell18 )).

fof(tlhfof35815,axiom,(
    cell93 != cell7 )).

fof(tlhfof35816,axiom,(
    cell93 != cell17 )).

fof(tlhfof35817,axiom,(
    cell93 != cell6 )).

fof(tlhfof35818,axiom,(
    cell93 != cell16 )).

fof(tlhfof35819,axiom,(
    cell93 != cell5 )).

fof(tlhfof35820,axiom,(
    cell93 != cell15 )).

fof(tlhfof35821,axiom,(
    cell93 != cell4 )).

fof(tlhfof35822,axiom,(
    cell93 != cell14 )).

fof(tlhfof35823,axiom,(
    cell93 != cell3 )).

fof(tlhfof35824,axiom,(
    cell93 != cell13 )).

fof(tlhfof35825,axiom,(
    cell93 != cell2 )).

fof(tlhfof35826,axiom,(
    cell93 != cell12 )).

fof(tlhfof35827,axiom,(
    cell93 != cell1 )).

fof(tlhfof35828,axiom,(
    cell93 != cell11 )).

fof(tlhfof35829,axiom,(
    cell93 != cell20 )).

fof(tlhfof35830,axiom,(
    cell92 != cell91 )).

fof(tlhfof35831,axiom,(
    cell92 != cell90 )).

fof(tlhfof35832,axiom,(
    cell92 != cell89 )).

fof(tlhfof35833,axiom,(
    cell92 != cell88 )).

fof(tlhfof35834,axiom,(
    cell92 != cell87 )).

fof(tlhfof35835,axiom,(
    cell92 != cell86 )).

fof(tlhfof35836,axiom,(
    cell92 != cell85 )).

fof(tlhfof35837,axiom,(
    cell92 != cell84 )).

fof(tlhfof35838,axiom,(
    cell92 != cell83 )).

fof(tlhfof35839,axiom,(
    cell92 != cell82 )).

fof(tlhfof35840,axiom,(
    cell92 != cell81 )).

fof(tlhfof35841,axiom,(
    cell92 != cell80 )).

fof(tlhfof35842,axiom,(
    cell92 != cell79 )).

fof(tlhfof35843,axiom,(
    cell92 != cell78 )).

fof(tlhfof35844,axiom,(
    cell92 != cell77 )).

fof(tlhfof35845,axiom,(
    cell92 != cell76 )).

fof(tlhfof35846,axiom,(
    cell92 != cell75 )).

fof(tlhfof35847,axiom,(
    cell92 != cell74 )).

fof(tlhfof35848,axiom,(
    cell92 != cell73 )).

fof(tlhfof35849,axiom,(
    cell92 != cell72 )).

fof(tlhfof35850,axiom,(
    cell92 != cell71 )).

fof(tlhfof35851,axiom,(
    cell92 != cell70 )).

fof(tlhfof35852,axiom,(
    cell92 != cell69 )).

fof(tlhfof35853,axiom,(
    cell92 != cell68 )).

fof(tlhfof35854,axiom,(
    cell92 != cell67 )).

fof(tlhfof35855,axiom,(
    cell92 != cell66 )).

fof(tlhfof35856,axiom,(
    cell92 != cell65 )).

fof(tlhfof35857,axiom,(
    cell92 != cell64 )).

fof(tlhfof35858,axiom,(
    cell92 != cell63 )).

fof(tlhfof35859,axiom,(
    cell92 != cell62 )).

fof(tlhfof35860,axiom,(
    cell92 != cell61 )).

fof(tlhfof35861,axiom,(
    cell92 != cell60 )).

fof(tlhfof35862,axiom,(
    cell92 != cell59 )).

fof(tlhfof35863,axiom,(
    cell92 != cell58 )).

fof(tlhfof35864,axiom,(
    cell92 != cell57 )).

fof(tlhfof35865,axiom,(
    cell92 != cell56 )).

fof(tlhfof35866,axiom,(
    cell92 != cell55 )).

fof(tlhfof35867,axiom,(
    cell92 != cell54 )).

fof(tlhfof35868,axiom,(
    cell92 != cell53 )).

fof(tlhfof35869,axiom,(
    cell92 != cell52 )).

fof(tlhfof35870,axiom,(
    cell92 != cell51 )).

fof(tlhfof35871,axiom,(
    cell92 != cell50 )).

fof(tlhfof35872,axiom,(
    cell92 != cell49 )).

fof(tlhfof35873,axiom,(
    cell92 != cell48 )).

fof(tlhfof35874,axiom,(
    cell92 != cell47 )).

fof(tlhfof35875,axiom,(
    cell92 != cell46 )).

fof(tlhfof35876,axiom,(
    cell92 != cell45 )).

fof(tlhfof35877,axiom,(
    cell92 != cell44 )).

fof(tlhfof35878,axiom,(
    cell92 != cell43 )).

fof(tlhfof35879,axiom,(
    cell92 != cell42 )).

fof(tlhfof35880,axiom,(
    cell92 != cell41 )).

fof(tlhfof35881,axiom,(
    cell92 != cell40 )).

fof(tlhfof35882,axiom,(
    cell92 != cell39 )).

fof(tlhfof35883,axiom,(
    cell92 != cell38 )).

fof(tlhfof35884,axiom,(
    cell92 != cell37 )).

fof(tlhfof35885,axiom,(
    cell92 != cell36 )).

fof(tlhfof35886,axiom,(
    cell92 != cell35 )).

fof(tlhfof35887,axiom,(
    cell92 != cell34 )).

fof(tlhfof35888,axiom,(
    cell92 != cell33 )).

fof(tlhfof35889,axiom,(
    cell92 != cell32 )).

fof(tlhfof35890,axiom,(
    cell92 != cell31 )).

fof(tlhfof35891,axiom,(
    cell92 != cell30 )).

fof(tlhfof35892,axiom,(
    cell92 != cell29 )).

fof(tlhfof35893,axiom,(
    cell92 != cell28 )).

fof(tlhfof35894,axiom,(
    cell92 != cell27 )).

fof(tlhfof35895,axiom,(
    cell92 != cell26 )).

fof(tlhfof35896,axiom,(
    cell92 != cell25 )).

fof(tlhfof35897,axiom,(
    cell92 != cell24 )).

fof(tlhfof35898,axiom,(
    cell92 != cell23 )).

fof(tlhfof35899,axiom,(
    cell92 != cell22 )).

fof(tlhfof35900,axiom,(
    cell92 != cell21 )).

fof(tlhfof35901,axiom,(
    cell92 != cell10 )).

fof(tlhfof35902,axiom,(
    cell92 != cell9 )).

fof(tlhfof35903,axiom,(
    cell92 != cell19 )).

fof(tlhfof35904,axiom,(
    cell92 != cell8 )).

fof(tlhfof35905,axiom,(
    cell92 != cell18 )).

fof(tlhfof35906,axiom,(
    cell92 != cell7 )).

fof(tlhfof35907,axiom,(
    cell92 != cell17 )).

fof(tlhfof35908,axiom,(
    cell92 != cell6 )).

fof(tlhfof35909,axiom,(
    cell92 != cell16 )).

fof(tlhfof35910,axiom,(
    cell92 != cell5 )).

fof(tlhfof35911,axiom,(
    cell92 != cell15 )).

fof(tlhfof35912,axiom,(
    cell92 != cell4 )).

fof(tlhfof35913,axiom,(
    cell92 != cell14 )).

fof(tlhfof35914,axiom,(
    cell92 != cell3 )).

fof(tlhfof35915,axiom,(
    cell92 != cell13 )).

fof(tlhfof35916,axiom,(
    cell92 != cell2 )).

fof(tlhfof35917,axiom,(
    cell92 != cell12 )).

fof(tlhfof35918,axiom,(
    cell92 != cell1 )).

fof(tlhfof35919,axiom,(
    cell92 != cell11 )).

fof(tlhfof35920,axiom,(
    cell92 != cell20 )).

fof(tlhfof35921,axiom,(
    cell91 != cell90 )).

fof(tlhfof35922,axiom,(
    cell91 != cell89 )).

fof(tlhfof35923,axiom,(
    cell91 != cell88 )).

fof(tlhfof35924,axiom,(
    cell91 != cell87 )).

fof(tlhfof35925,axiom,(
    cell91 != cell86 )).

fof(tlhfof35926,axiom,(
    cell91 != cell85 )).

fof(tlhfof35927,axiom,(
    cell91 != cell84 )).

fof(tlhfof35928,axiom,(
    cell91 != cell83 )).

fof(tlhfof35929,axiom,(
    cell91 != cell82 )).

fof(tlhfof35930,axiom,(
    cell91 != cell81 )).

fof(tlhfof35931,axiom,(
    cell91 != cell80 )).

fof(tlhfof35932,axiom,(
    cell91 != cell79 )).

fof(tlhfof35933,axiom,(
    cell91 != cell78 )).

fof(tlhfof35934,axiom,(
    cell91 != cell77 )).

fof(tlhfof35935,axiom,(
    cell91 != cell76 )).

fof(tlhfof35936,axiom,(
    cell91 != cell75 )).

fof(tlhfof35937,axiom,(
    cell91 != cell74 )).

fof(tlhfof35938,axiom,(
    cell91 != cell73 )).

fof(tlhfof35939,axiom,(
    cell91 != cell72 )).

fof(tlhfof35940,axiom,(
    cell91 != cell71 )).

fof(tlhfof35941,axiom,(
    cell91 != cell70 )).

fof(tlhfof35942,axiom,(
    cell91 != cell69 )).

fof(tlhfof35943,axiom,(
    cell91 != cell68 )).

fof(tlhfof35944,axiom,(
    cell91 != cell67 )).

fof(tlhfof35945,axiom,(
    cell91 != cell66 )).

fof(tlhfof35946,axiom,(
    cell91 != cell65 )).

fof(tlhfof35947,axiom,(
    cell91 != cell64 )).

fof(tlhfof35948,axiom,(
    cell91 != cell63 )).

fof(tlhfof35949,axiom,(
    cell91 != cell62 )).

fof(tlhfof35950,axiom,(
    cell91 != cell61 )).

fof(tlhfof35951,axiom,(
    cell91 != cell60 )).

fof(tlhfof35952,axiom,(
    cell91 != cell59 )).

fof(tlhfof35953,axiom,(
    cell91 != cell58 )).

fof(tlhfof35954,axiom,(
    cell91 != cell57 )).

fof(tlhfof35955,axiom,(
    cell91 != cell56 )).

fof(tlhfof35956,axiom,(
    cell91 != cell55 )).

fof(tlhfof35957,axiom,(
    cell91 != cell54 )).

fof(tlhfof35958,axiom,(
    cell91 != cell53 )).

fof(tlhfof35959,axiom,(
    cell91 != cell52 )).

fof(tlhfof35960,axiom,(
    cell91 != cell51 )).

fof(tlhfof35961,axiom,(
    cell91 != cell50 )).

fof(tlhfof35962,axiom,(
    cell91 != cell49 )).

fof(tlhfof35963,axiom,(
    cell91 != cell48 )).

fof(tlhfof35964,axiom,(
    cell91 != cell47 )).

fof(tlhfof35965,axiom,(
    cell91 != cell46 )).

fof(tlhfof35966,axiom,(
    cell91 != cell45 )).

fof(tlhfof35967,axiom,(
    cell91 != cell44 )).

fof(tlhfof35968,axiom,(
    cell91 != cell43 )).

fof(tlhfof35969,axiom,(
    cell91 != cell42 )).

fof(tlhfof35970,axiom,(
    cell91 != cell41 )).

fof(tlhfof35971,axiom,(
    cell91 != cell40 )).

fof(tlhfof35972,axiom,(
    cell91 != cell39 )).

fof(tlhfof35973,axiom,(
    cell91 != cell38 )).

fof(tlhfof35974,axiom,(
    cell91 != cell37 )).

fof(tlhfof35975,axiom,(
    cell91 != cell36 )).

fof(tlhfof35976,axiom,(
    cell91 != cell35 )).

fof(tlhfof35977,axiom,(
    cell91 != cell34 )).

fof(tlhfof35978,axiom,(
    cell91 != cell33 )).

fof(tlhfof35979,axiom,(
    cell91 != cell32 )).

fof(tlhfof35980,axiom,(
    cell91 != cell31 )).

fof(tlhfof35981,axiom,(
    cell91 != cell30 )).

fof(tlhfof35982,axiom,(
    cell91 != cell29 )).

fof(tlhfof35983,axiom,(
    cell91 != cell28 )).

fof(tlhfof35984,axiom,(
    cell91 != cell27 )).

fof(tlhfof35985,axiom,(
    cell91 != cell26 )).

fof(tlhfof35986,axiom,(
    cell91 != cell25 )).

fof(tlhfof35987,axiom,(
    cell91 != cell24 )).

fof(tlhfof35988,axiom,(
    cell91 != cell23 )).

fof(tlhfof35989,axiom,(
    cell91 != cell22 )).

fof(tlhfof35990,axiom,(
    cell91 != cell21 )).

fof(tlhfof35991,axiom,(
    cell91 != cell10 )).

fof(tlhfof35992,axiom,(
    cell91 != cell9 )).

fof(tlhfof35993,axiom,(
    cell91 != cell19 )).

fof(tlhfof35994,axiom,(
    cell91 != cell8 )).

fof(tlhfof35995,axiom,(
    cell91 != cell18 )).

fof(tlhfof35996,axiom,(
    cell91 != cell7 )).

fof(tlhfof35997,axiom,(
    cell91 != cell17 )).

fof(tlhfof35998,axiom,(
    cell91 != cell6 )).

fof(tlhfof35999,axiom,(
    cell91 != cell16 )).

fof(tlhfof36000,axiom,(
    cell91 != cell5 )).

fof(tlhfof36001,axiom,(
    cell91 != cell15 )).

fof(tlhfof36002,axiom,(
    cell91 != cell4 )).

fof(tlhfof36003,axiom,(
    cell91 != cell14 )).

fof(tlhfof36004,axiom,(
    cell91 != cell3 )).

fof(tlhfof36005,axiom,(
    cell91 != cell13 )).

fof(tlhfof36006,axiom,(
    cell91 != cell2 )).

fof(tlhfof36007,axiom,(
    cell91 != cell12 )).

fof(tlhfof36008,axiom,(
    cell91 != cell1 )).

fof(tlhfof36009,axiom,(
    cell91 != cell11 )).

fof(tlhfof36010,axiom,(
    cell91 != cell20 )).

fof(tlhfof36011,axiom,(
    cell90 != cell89 )).

fof(tlhfof36012,axiom,(
    cell90 != cell88 )).

fof(tlhfof36013,axiom,(
    cell90 != cell87 )).

fof(tlhfof36014,axiom,(
    cell90 != cell86 )).

fof(tlhfof36015,axiom,(
    cell90 != cell85 )).

fof(tlhfof36016,axiom,(
    cell90 != cell84 )).

fof(tlhfof36017,axiom,(
    cell90 != cell83 )).

fof(tlhfof36018,axiom,(
    cell90 != cell82 )).

fof(tlhfof36019,axiom,(
    cell90 != cell81 )).

fof(tlhfof36020,axiom,(
    cell90 != cell80 )).

fof(tlhfof36021,axiom,(
    cell90 != cell79 )).

fof(tlhfof36022,axiom,(
    cell90 != cell78 )).

fof(tlhfof36023,axiom,(
    cell90 != cell77 )).

fof(tlhfof36024,axiom,(
    cell90 != cell76 )).

fof(tlhfof36025,axiom,(
    cell90 != cell75 )).

fof(tlhfof36026,axiom,(
    cell90 != cell74 )).

fof(tlhfof36027,axiom,(
    cell90 != cell73 )).

fof(tlhfof36028,axiom,(
    cell90 != cell72 )).

fof(tlhfof36029,axiom,(
    cell90 != cell71 )).

fof(tlhfof36030,axiom,(
    cell90 != cell70 )).

fof(tlhfof36031,axiom,(
    cell90 != cell69 )).

fof(tlhfof36032,axiom,(
    cell90 != cell68 )).

fof(tlhfof36033,axiom,(
    cell90 != cell67 )).

fof(tlhfof36034,axiom,(
    cell90 != cell66 )).

fof(tlhfof36035,axiom,(
    cell90 != cell65 )).

fof(tlhfof36036,axiom,(
    cell90 != cell64 )).

fof(tlhfof36037,axiom,(
    cell90 != cell63 )).

fof(tlhfof36038,axiom,(
    cell90 != cell62 )).

fof(tlhfof36039,axiom,(
    cell90 != cell61 )).

fof(tlhfof36040,axiom,(
    cell90 != cell60 )).

fof(tlhfof36041,axiom,(
    cell90 != cell59 )).

fof(tlhfof36042,axiom,(
    cell90 != cell58 )).

fof(tlhfof36043,axiom,(
    cell90 != cell57 )).

fof(tlhfof36044,axiom,(
    cell90 != cell56 )).

fof(tlhfof36045,axiom,(
    cell90 != cell55 )).

fof(tlhfof36046,axiom,(
    cell90 != cell54 )).

fof(tlhfof36047,axiom,(
    cell90 != cell53 )).

fof(tlhfof36048,axiom,(
    cell90 != cell52 )).

fof(tlhfof36049,axiom,(
    cell90 != cell51 )).

fof(tlhfof36050,axiom,(
    cell90 != cell50 )).

fof(tlhfof36051,axiom,(
    cell90 != cell49 )).

fof(tlhfof36052,axiom,(
    cell90 != cell48 )).

fof(tlhfof36053,axiom,(
    cell90 != cell47 )).

fof(tlhfof36054,axiom,(
    cell90 != cell46 )).

fof(tlhfof36055,axiom,(
    cell90 != cell45 )).

fof(tlhfof36056,axiom,(
    cell90 != cell44 )).

fof(tlhfof36057,axiom,(
    cell90 != cell43 )).

fof(tlhfof36058,axiom,(
    cell90 != cell42 )).

fof(tlhfof36059,axiom,(
    cell90 != cell41 )).

fof(tlhfof36060,axiom,(
    cell90 != cell40 )).

fof(tlhfof36061,axiom,(
    cell90 != cell39 )).

fof(tlhfof36062,axiom,(
    cell90 != cell38 )).

fof(tlhfof36063,axiom,(
    cell90 != cell37 )).

fof(tlhfof36064,axiom,(
    cell90 != cell36 )).

fof(tlhfof36065,axiom,(
    cell90 != cell35 )).

fof(tlhfof36066,axiom,(
    cell90 != cell34 )).

fof(tlhfof36067,axiom,(
    cell90 != cell33 )).

fof(tlhfof36068,axiom,(
    cell90 != cell32 )).

fof(tlhfof36069,axiom,(
    cell90 != cell31 )).

fof(tlhfof36070,axiom,(
    cell90 != cell30 )).

fof(tlhfof36071,axiom,(
    cell90 != cell29 )).

fof(tlhfof36072,axiom,(
    cell90 != cell28 )).

fof(tlhfof36073,axiom,(
    cell90 != cell27 )).

fof(tlhfof36074,axiom,(
    cell90 != cell26 )).

fof(tlhfof36075,axiom,(
    cell90 != cell25 )).

fof(tlhfof36076,axiom,(
    cell90 != cell24 )).

fof(tlhfof36077,axiom,(
    cell90 != cell23 )).

fof(tlhfof36078,axiom,(
    cell90 != cell22 )).

fof(tlhfof36079,axiom,(
    cell90 != cell21 )).

fof(tlhfof36080,axiom,(
    cell90 != cell10 )).

fof(tlhfof36081,axiom,(
    cell90 != cell9 )).

fof(tlhfof36082,axiom,(
    cell90 != cell19 )).

fof(tlhfof36083,axiom,(
    cell90 != cell8 )).

fof(tlhfof36084,axiom,(
    cell90 != cell18 )).

fof(tlhfof36085,axiom,(
    cell90 != cell7 )).

fof(tlhfof36086,axiom,(
    cell90 != cell17 )).

fof(tlhfof36087,axiom,(
    cell90 != cell6 )).

fof(tlhfof36088,axiom,(
    cell90 != cell16 )).

fof(tlhfof36089,axiom,(
    cell90 != cell5 )).

fof(tlhfof36090,axiom,(
    cell90 != cell15 )).

fof(tlhfof36091,axiom,(
    cell90 != cell4 )).

fof(tlhfof36092,axiom,(
    cell90 != cell14 )).

fof(tlhfof36093,axiom,(
    cell90 != cell3 )).

fof(tlhfof36094,axiom,(
    cell90 != cell13 )).

fof(tlhfof36095,axiom,(
    cell90 != cell2 )).

fof(tlhfof36096,axiom,(
    cell90 != cell12 )).

fof(tlhfof36097,axiom,(
    cell90 != cell1 )).

fof(tlhfof36098,axiom,(
    cell90 != cell11 )).

fof(tlhfof36099,axiom,(
    cell90 != cell20 )).

fof(tlhfof36100,axiom,(
    cell89 != cell88 )).

fof(tlhfof36101,axiom,(
    cell89 != cell87 )).

fof(tlhfof36102,axiom,(
    cell89 != cell86 )).

fof(tlhfof36103,axiom,(
    cell89 != cell85 )).

fof(tlhfof36104,axiom,(
    cell89 != cell84 )).

fof(tlhfof36105,axiom,(
    cell89 != cell83 )).

fof(tlhfof36106,axiom,(
    cell89 != cell82 )).

fof(tlhfof36107,axiom,(
    cell89 != cell81 )).

fof(tlhfof36108,axiom,(
    cell89 != cell80 )).

fof(tlhfof36109,axiom,(
    cell89 != cell79 )).

fof(tlhfof36110,axiom,(
    cell89 != cell78 )).

fof(tlhfof36111,axiom,(
    cell89 != cell77 )).

fof(tlhfof36112,axiom,(
    cell89 != cell76 )).

fof(tlhfof36113,axiom,(
    cell89 != cell75 )).

fof(tlhfof36114,axiom,(
    cell89 != cell74 )).

fof(tlhfof36115,axiom,(
    cell89 != cell73 )).

fof(tlhfof36116,axiom,(
    cell89 != cell72 )).

fof(tlhfof36117,axiom,(
    cell89 != cell71 )).

fof(tlhfof36118,axiom,(
    cell89 != cell70 )).

fof(tlhfof36119,axiom,(
    cell89 != cell69 )).

fof(tlhfof36120,axiom,(
    cell89 != cell68 )).

fof(tlhfof36121,axiom,(
    cell89 != cell67 )).

fof(tlhfof36122,axiom,(
    cell89 != cell66 )).

fof(tlhfof36123,axiom,(
    cell89 != cell65 )).

fof(tlhfof36124,axiom,(
    cell89 != cell64 )).

fof(tlhfof36125,axiom,(
    cell89 != cell63 )).

fof(tlhfof36126,axiom,(
    cell89 != cell62 )).

fof(tlhfof36127,axiom,(
    cell89 != cell61 )).

fof(tlhfof36128,axiom,(
    cell89 != cell60 )).

fof(tlhfof36129,axiom,(
    cell89 != cell59 )).

fof(tlhfof36130,axiom,(
    cell89 != cell58 )).

fof(tlhfof36131,axiom,(
    cell89 != cell57 )).

fof(tlhfof36132,axiom,(
    cell89 != cell56 )).

fof(tlhfof36133,axiom,(
    cell89 != cell55 )).

fof(tlhfof36134,axiom,(
    cell89 != cell54 )).

fof(tlhfof36135,axiom,(
    cell89 != cell53 )).

fof(tlhfof36136,axiom,(
    cell89 != cell52 )).

fof(tlhfof36137,axiom,(
    cell89 != cell51 )).

fof(tlhfof36138,axiom,(
    cell89 != cell50 )).

fof(tlhfof36139,axiom,(
    cell89 != cell49 )).

fof(tlhfof36140,axiom,(
    cell89 != cell48 )).

fof(tlhfof36141,axiom,(
    cell89 != cell47 )).

fof(tlhfof36142,axiom,(
    cell89 != cell46 )).

fof(tlhfof36143,axiom,(
    cell89 != cell45 )).

fof(tlhfof36144,axiom,(
    cell89 != cell44 )).

fof(tlhfof36145,axiom,(
    cell89 != cell43 )).

fof(tlhfof36146,axiom,(
    cell89 != cell42 )).

fof(tlhfof36147,axiom,(
    cell89 != cell41 )).

fof(tlhfof36148,axiom,(
    cell89 != cell40 )).

fof(tlhfof36149,axiom,(
    cell89 != cell39 )).

fof(tlhfof36150,axiom,(
    cell89 != cell38 )).

fof(tlhfof36151,axiom,(
    cell89 != cell37 )).

fof(tlhfof36152,axiom,(
    cell89 != cell36 )).

fof(tlhfof36153,axiom,(
    cell89 != cell35 )).

fof(tlhfof36154,axiom,(
    cell89 != cell34 )).

fof(tlhfof36155,axiom,(
    cell89 != cell33 )).

fof(tlhfof36156,axiom,(
    cell89 != cell32 )).

fof(tlhfof36157,axiom,(
    cell89 != cell31 )).

fof(tlhfof36158,axiom,(
    cell89 != cell30 )).

fof(tlhfof36159,axiom,(
    cell89 != cell29 )).

fof(tlhfof36160,axiom,(
    cell89 != cell28 )).

fof(tlhfof36161,axiom,(
    cell89 != cell27 )).

fof(tlhfof36162,axiom,(
    cell89 != cell26 )).

fof(tlhfof36163,axiom,(
    cell89 != cell25 )).

fof(tlhfof36164,axiom,(
    cell89 != cell24 )).

fof(tlhfof36165,axiom,(
    cell89 != cell23 )).

fof(tlhfof36166,axiom,(
    cell89 != cell22 )).

fof(tlhfof36167,axiom,(
    cell89 != cell21 )).

fof(tlhfof36168,axiom,(
    cell89 != cell10 )).

fof(tlhfof36169,axiom,(
    cell89 != cell9 )).

fof(tlhfof36170,axiom,(
    cell89 != cell19 )).

fof(tlhfof36171,axiom,(
    cell89 != cell8 )).

fof(tlhfof36172,axiom,(
    cell89 != cell18 )).

fof(tlhfof36173,axiom,(
    cell89 != cell7 )).

fof(tlhfof36174,axiom,(
    cell89 != cell17 )).

fof(tlhfof36175,axiom,(
    cell89 != cell6 )).

fof(tlhfof36176,axiom,(
    cell89 != cell16 )).

fof(tlhfof36177,axiom,(
    cell89 != cell5 )).

fof(tlhfof36178,axiom,(
    cell89 != cell15 )).

fof(tlhfof36179,axiom,(
    cell89 != cell4 )).

fof(tlhfof36180,axiom,(
    cell89 != cell14 )).

fof(tlhfof36181,axiom,(
    cell89 != cell3 )).

fof(tlhfof36182,axiom,(
    cell89 != cell13 )).

fof(tlhfof36183,axiom,(
    cell89 != cell2 )).

fof(tlhfof36184,axiom,(
    cell89 != cell12 )).

fof(tlhfof36185,axiom,(
    cell89 != cell1 )).

fof(tlhfof36186,axiom,(
    cell89 != cell11 )).

fof(tlhfof36187,axiom,(
    cell89 != cell20 )).

fof(tlhfof36188,axiom,(
    cell88 != cell87 )).

fof(tlhfof36189,axiom,(
    cell88 != cell86 )).

fof(tlhfof36190,axiom,(
    cell88 != cell85 )).

fof(tlhfof36191,axiom,(
    cell88 != cell84 )).

fof(tlhfof36192,axiom,(
    cell88 != cell83 )).

fof(tlhfof36193,axiom,(
    cell88 != cell82 )).

fof(tlhfof36194,axiom,(
    cell88 != cell81 )).

fof(tlhfof36195,axiom,(
    cell88 != cell80 )).

fof(tlhfof36196,axiom,(
    cell88 != cell79 )).

fof(tlhfof36197,axiom,(
    cell88 != cell78 )).

fof(tlhfof36198,axiom,(
    cell88 != cell77 )).

fof(tlhfof36199,axiom,(
    cell88 != cell76 )).

fof(tlhfof36200,axiom,(
    cell88 != cell75 )).

fof(tlhfof36201,axiom,(
    cell88 != cell74 )).

fof(tlhfof36202,axiom,(
    cell88 != cell73 )).

fof(tlhfof36203,axiom,(
    cell88 != cell72 )).

fof(tlhfof36204,axiom,(
    cell88 != cell71 )).

fof(tlhfof36205,axiom,(
    cell88 != cell70 )).

fof(tlhfof36206,axiom,(
    cell88 != cell69 )).

fof(tlhfof36207,axiom,(
    cell88 != cell68 )).

fof(tlhfof36208,axiom,(
    cell88 != cell67 )).

fof(tlhfof36209,axiom,(
    cell88 != cell66 )).

fof(tlhfof36210,axiom,(
    cell88 != cell65 )).

fof(tlhfof36211,axiom,(
    cell88 != cell64 )).

fof(tlhfof36212,axiom,(
    cell88 != cell63 )).

fof(tlhfof36213,axiom,(
    cell88 != cell62 )).

fof(tlhfof36214,axiom,(
    cell88 != cell61 )).

fof(tlhfof36215,axiom,(
    cell88 != cell60 )).

fof(tlhfof36216,axiom,(
    cell88 != cell59 )).

fof(tlhfof36217,axiom,(
    cell88 != cell58 )).

fof(tlhfof36218,axiom,(
    cell88 != cell57 )).

fof(tlhfof36219,axiom,(
    cell88 != cell56 )).

fof(tlhfof36220,axiom,(
    cell88 != cell55 )).

fof(tlhfof36221,axiom,(
    cell88 != cell54 )).

fof(tlhfof36222,axiom,(
    cell88 != cell53 )).

fof(tlhfof36223,axiom,(
    cell88 != cell52 )).

fof(tlhfof36224,axiom,(
    cell88 != cell51 )).

fof(tlhfof36225,axiom,(
    cell88 != cell50 )).

fof(tlhfof36226,axiom,(
    cell88 != cell49 )).

fof(tlhfof36227,axiom,(
    cell88 != cell48 )).

fof(tlhfof36228,axiom,(
    cell88 != cell47 )).

fof(tlhfof36229,axiom,(
    cell88 != cell46 )).

fof(tlhfof36230,axiom,(
    cell88 != cell45 )).

fof(tlhfof36231,axiom,(
    cell88 != cell44 )).

fof(tlhfof36232,axiom,(
    cell88 != cell43 )).

fof(tlhfof36233,axiom,(
    cell88 != cell42 )).

fof(tlhfof36234,axiom,(
    cell88 != cell41 )).

fof(tlhfof36235,axiom,(
    cell88 != cell40 )).

fof(tlhfof36236,axiom,(
    cell88 != cell39 )).

fof(tlhfof36237,axiom,(
    cell88 != cell38 )).

fof(tlhfof36238,axiom,(
    cell88 != cell37 )).

fof(tlhfof36239,axiom,(
    cell88 != cell36 )).

fof(tlhfof36240,axiom,(
    cell88 != cell35 )).

fof(tlhfof36241,axiom,(
    cell88 != cell34 )).

fof(tlhfof36242,axiom,(
    cell88 != cell33 )).

fof(tlhfof36243,axiom,(
    cell88 != cell32 )).

fof(tlhfof36244,axiom,(
    cell88 != cell31 )).

fof(tlhfof36245,axiom,(
    cell88 != cell30 )).

fof(tlhfof36246,axiom,(
    cell88 != cell29 )).

fof(tlhfof36247,axiom,(
    cell88 != cell28 )).

fof(tlhfof36248,axiom,(
    cell88 != cell27 )).

fof(tlhfof36249,axiom,(
    cell88 != cell26 )).

fof(tlhfof36250,axiom,(
    cell88 != cell25 )).

fof(tlhfof36251,axiom,(
    cell88 != cell24 )).

fof(tlhfof36252,axiom,(
    cell88 != cell23 )).

fof(tlhfof36253,axiom,(
    cell88 != cell22 )).

fof(tlhfof36254,axiom,(
    cell88 != cell21 )).

fof(tlhfof36255,axiom,(
    cell88 != cell10 )).

fof(tlhfof36256,axiom,(
    cell88 != cell9 )).

fof(tlhfof36257,axiom,(
    cell88 != cell19 )).

fof(tlhfof36258,axiom,(
    cell88 != cell8 )).

fof(tlhfof36259,axiom,(
    cell88 != cell18 )).

fof(tlhfof36260,axiom,(
    cell88 != cell7 )).

fof(tlhfof36261,axiom,(
    cell88 != cell17 )).

fof(tlhfof36262,axiom,(
    cell88 != cell6 )).

fof(tlhfof36263,axiom,(
    cell88 != cell16 )).

fof(tlhfof36264,axiom,(
    cell88 != cell5 )).

fof(tlhfof36265,axiom,(
    cell88 != cell15 )).

fof(tlhfof36266,axiom,(
    cell88 != cell4 )).

fof(tlhfof36267,axiom,(
    cell88 != cell14 )).

fof(tlhfof36268,axiom,(
    cell88 != cell3 )).

fof(tlhfof36269,axiom,(
    cell88 != cell13 )).

fof(tlhfof36270,axiom,(
    cell88 != cell2 )).

fof(tlhfof36271,axiom,(
    cell88 != cell12 )).

fof(tlhfof36272,axiom,(
    cell88 != cell1 )).

fof(tlhfof36273,axiom,(
    cell88 != cell11 )).

fof(tlhfof36274,axiom,(
    cell88 != cell20 )).

fof(tlhfof36275,axiom,(
    cell87 != cell86 )).

fof(tlhfof36276,axiom,(
    cell87 != cell85 )).

fof(tlhfof36277,axiom,(
    cell87 != cell84 )).

fof(tlhfof36278,axiom,(
    cell87 != cell83 )).

fof(tlhfof36279,axiom,(
    cell87 != cell82 )).

fof(tlhfof36280,axiom,(
    cell87 != cell81 )).

fof(tlhfof36281,axiom,(
    cell87 != cell80 )).

fof(tlhfof36282,axiom,(
    cell87 != cell79 )).

fof(tlhfof36283,axiom,(
    cell87 != cell78 )).

fof(tlhfof36284,axiom,(
    cell87 != cell77 )).

fof(tlhfof36285,axiom,(
    cell87 != cell76 )).

fof(tlhfof36286,axiom,(
    cell87 != cell75 )).

fof(tlhfof36287,axiom,(
    cell87 != cell74 )).

fof(tlhfof36288,axiom,(
    cell87 != cell73 )).

fof(tlhfof36289,axiom,(
    cell87 != cell72 )).

fof(tlhfof36290,axiom,(
    cell87 != cell71 )).

fof(tlhfof36291,axiom,(
    cell87 != cell70 )).

fof(tlhfof36292,axiom,(
    cell87 != cell69 )).

fof(tlhfof36293,axiom,(
    cell87 != cell68 )).

fof(tlhfof36294,axiom,(
    cell87 != cell67 )).

fof(tlhfof36295,axiom,(
    cell87 != cell66 )).

fof(tlhfof36296,axiom,(
    cell87 != cell65 )).

fof(tlhfof36297,axiom,(
    cell87 != cell64 )).

fof(tlhfof36298,axiom,(
    cell87 != cell63 )).

fof(tlhfof36299,axiom,(
    cell87 != cell62 )).

fof(tlhfof36300,axiom,(
    cell87 != cell61 )).

fof(tlhfof36301,axiom,(
    cell87 != cell60 )).

fof(tlhfof36302,axiom,(
    cell87 != cell59 )).

fof(tlhfof36303,axiom,(
    cell87 != cell58 )).

fof(tlhfof36304,axiom,(
    cell87 != cell57 )).

fof(tlhfof36305,axiom,(
    cell87 != cell56 )).

fof(tlhfof36306,axiom,(
    cell87 != cell55 )).

fof(tlhfof36307,axiom,(
    cell87 != cell54 )).

fof(tlhfof36308,axiom,(
    cell87 != cell53 )).

fof(tlhfof36309,axiom,(
    cell87 != cell52 )).

fof(tlhfof36310,axiom,(
    cell87 != cell51 )).

fof(tlhfof36311,axiom,(
    cell87 != cell50 )).

fof(tlhfof36312,axiom,(
    cell87 != cell49 )).

fof(tlhfof36313,axiom,(
    cell87 != cell48 )).

fof(tlhfof36314,axiom,(
    cell87 != cell47 )).

fof(tlhfof36315,axiom,(
    cell87 != cell46 )).

fof(tlhfof36316,axiom,(
    cell87 != cell45 )).

fof(tlhfof36317,axiom,(
    cell87 != cell44 )).

fof(tlhfof36318,axiom,(
    cell87 != cell43 )).

fof(tlhfof36319,axiom,(
    cell87 != cell42 )).

fof(tlhfof36320,axiom,(
    cell87 != cell41 )).

fof(tlhfof36321,axiom,(
    cell87 != cell40 )).

fof(tlhfof36322,axiom,(
    cell87 != cell39 )).

fof(tlhfof36323,axiom,(
    cell87 != cell38 )).

fof(tlhfof36324,axiom,(
    cell87 != cell37 )).

fof(tlhfof36325,axiom,(
    cell87 != cell36 )).

fof(tlhfof36326,axiom,(
    cell87 != cell35 )).

fof(tlhfof36327,axiom,(
    cell87 != cell34 )).

fof(tlhfof36328,axiom,(
    cell87 != cell33 )).

fof(tlhfof36329,axiom,(
    cell87 != cell32 )).

fof(tlhfof36330,axiom,(
    cell87 != cell31 )).

fof(tlhfof36331,axiom,(
    cell87 != cell30 )).

fof(tlhfof36332,axiom,(
    cell87 != cell29 )).

fof(tlhfof36333,axiom,(
    cell87 != cell28 )).

fof(tlhfof36334,axiom,(
    cell87 != cell27 )).

fof(tlhfof36335,axiom,(
    cell87 != cell26 )).

fof(tlhfof36336,axiom,(
    cell87 != cell25 )).

fof(tlhfof36337,axiom,(
    cell87 != cell24 )).

fof(tlhfof36338,axiom,(
    cell87 != cell23 )).

fof(tlhfof36339,axiom,(
    cell87 != cell22 )).

fof(tlhfof36340,axiom,(
    cell87 != cell21 )).

fof(tlhfof36341,axiom,(
    cell87 != cell10 )).

fof(tlhfof36342,axiom,(
    cell87 != cell9 )).

fof(tlhfof36343,axiom,(
    cell87 != cell19 )).

fof(tlhfof36344,axiom,(
    cell87 != cell8 )).

fof(tlhfof36345,axiom,(
    cell87 != cell18 )).

fof(tlhfof36346,axiom,(
    cell87 != cell7 )).

fof(tlhfof36347,axiom,(
    cell87 != cell17 )).

fof(tlhfof36348,axiom,(
    cell87 != cell6 )).

fof(tlhfof36349,axiom,(
    cell87 != cell16 )).

fof(tlhfof36350,axiom,(
    cell87 != cell5 )).

fof(tlhfof36351,axiom,(
    cell87 != cell15 )).

fof(tlhfof36352,axiom,(
    cell87 != cell4 )).

fof(tlhfof36353,axiom,(
    cell87 != cell14 )).

fof(tlhfof36354,axiom,(
    cell87 != cell3 )).

fof(tlhfof36355,axiom,(
    cell87 != cell13 )).

fof(tlhfof36356,axiom,(
    cell87 != cell2 )).

fof(tlhfof36357,axiom,(
    cell87 != cell12 )).

fof(tlhfof36358,axiom,(
    cell87 != cell1 )).

fof(tlhfof36359,axiom,(
    cell87 != cell11 )).

fof(tlhfof36360,axiom,(
    cell87 != cell20 )).

fof(tlhfof36361,axiom,(
    cell86 != cell85 )).

fof(tlhfof36362,axiom,(
    cell86 != cell84 )).

fof(tlhfof36363,axiom,(
    cell86 != cell83 )).

fof(tlhfof36364,axiom,(
    cell86 != cell82 )).

fof(tlhfof36365,axiom,(
    cell86 != cell81 )).

fof(tlhfof36366,axiom,(
    cell86 != cell80 )).

fof(tlhfof36367,axiom,(
    cell86 != cell79 )).

fof(tlhfof36368,axiom,(
    cell86 != cell78 )).

fof(tlhfof36369,axiom,(
    cell86 != cell77 )).

fof(tlhfof36370,axiom,(
    cell86 != cell76 )).

fof(tlhfof36371,axiom,(
    cell86 != cell75 )).

fof(tlhfof36372,axiom,(
    cell86 != cell74 )).

fof(tlhfof36373,axiom,(
    cell86 != cell73 )).

fof(tlhfof36374,axiom,(
    cell86 != cell72 )).

fof(tlhfof36375,axiom,(
    cell86 != cell71 )).

fof(tlhfof36376,axiom,(
    cell86 != cell70 )).

fof(tlhfof36377,axiom,(
    cell86 != cell69 )).

fof(tlhfof36378,axiom,(
    cell86 != cell68 )).

fof(tlhfof36379,axiom,(
    cell86 != cell67 )).

fof(tlhfof36380,axiom,(
    cell86 != cell66 )).

fof(tlhfof36381,axiom,(
    cell86 != cell65 )).

fof(tlhfof36382,axiom,(
    cell86 != cell64 )).

fof(tlhfof36383,axiom,(
    cell86 != cell63 )).

fof(tlhfof36384,axiom,(
    cell86 != cell62 )).

fof(tlhfof36385,axiom,(
    cell86 != cell61 )).

fof(tlhfof36386,axiom,(
    cell86 != cell60 )).

fof(tlhfof36387,axiom,(
    cell86 != cell59 )).

fof(tlhfof36388,axiom,(
    cell86 != cell58 )).

fof(tlhfof36389,axiom,(
    cell86 != cell57 )).

fof(tlhfof36390,axiom,(
    cell86 != cell56 )).

fof(tlhfof36391,axiom,(
    cell86 != cell55 )).

fof(tlhfof36392,axiom,(
    cell86 != cell54 )).

fof(tlhfof36393,axiom,(
    cell86 != cell53 )).

fof(tlhfof36394,axiom,(
    cell86 != cell52 )).

fof(tlhfof36395,axiom,(
    cell86 != cell51 )).

fof(tlhfof36396,axiom,(
    cell86 != cell50 )).

fof(tlhfof36397,axiom,(
    cell86 != cell49 )).

fof(tlhfof36398,axiom,(
    cell86 != cell48 )).

fof(tlhfof36399,axiom,(
    cell86 != cell47 )).

fof(tlhfof36400,axiom,(
    cell86 != cell46 )).

fof(tlhfof36401,axiom,(
    cell86 != cell45 )).

fof(tlhfof36402,axiom,(
    cell86 != cell44 )).

fof(tlhfof36403,axiom,(
    cell86 != cell43 )).

fof(tlhfof36404,axiom,(
    cell86 != cell42 )).

fof(tlhfof36405,axiom,(
    cell86 != cell41 )).

fof(tlhfof36406,axiom,(
    cell86 != cell40 )).

fof(tlhfof36407,axiom,(
    cell86 != cell39 )).

fof(tlhfof36408,axiom,(
    cell86 != cell38 )).

fof(tlhfof36409,axiom,(
    cell86 != cell37 )).

fof(tlhfof36410,axiom,(
    cell86 != cell36 )).

fof(tlhfof36411,axiom,(
    cell86 != cell35 )).

fof(tlhfof36412,axiom,(
    cell86 != cell34 )).

fof(tlhfof36413,axiom,(
    cell86 != cell33 )).

fof(tlhfof36414,axiom,(
    cell86 != cell32 )).

fof(tlhfof36415,axiom,(
    cell86 != cell31 )).

fof(tlhfof36416,axiom,(
    cell86 != cell30 )).

fof(tlhfof36417,axiom,(
    cell86 != cell29 )).

fof(tlhfof36418,axiom,(
    cell86 != cell28 )).

fof(tlhfof36419,axiom,(
    cell86 != cell27 )).

fof(tlhfof36420,axiom,(
    cell86 != cell26 )).

fof(tlhfof36421,axiom,(
    cell86 != cell25 )).

fof(tlhfof36422,axiom,(
    cell86 != cell24 )).

fof(tlhfof36423,axiom,(
    cell86 != cell23 )).

fof(tlhfof36424,axiom,(
    cell86 != cell22 )).

fof(tlhfof36425,axiom,(
    cell86 != cell21 )).

fof(tlhfof36426,axiom,(
    cell86 != cell10 )).

fof(tlhfof36427,axiom,(
    cell86 != cell9 )).

fof(tlhfof36428,axiom,(
    cell86 != cell19 )).

fof(tlhfof36429,axiom,(
    cell86 != cell8 )).

fof(tlhfof36430,axiom,(
    cell86 != cell18 )).

fof(tlhfof36431,axiom,(
    cell86 != cell7 )).

fof(tlhfof36432,axiom,(
    cell86 != cell17 )).

fof(tlhfof36433,axiom,(
    cell86 != cell6 )).

fof(tlhfof36434,axiom,(
    cell86 != cell16 )).

fof(tlhfof36435,axiom,(
    cell86 != cell5 )).

fof(tlhfof36436,axiom,(
    cell86 != cell15 )).

fof(tlhfof36437,axiom,(
    cell86 != cell4 )).

fof(tlhfof36438,axiom,(
    cell86 != cell14 )).

fof(tlhfof36439,axiom,(
    cell86 != cell3 )).

fof(tlhfof36440,axiom,(
    cell86 != cell13 )).

fof(tlhfof36441,axiom,(
    cell86 != cell2 )).

fof(tlhfof36442,axiom,(
    cell86 != cell12 )).

fof(tlhfof36443,axiom,(
    cell86 != cell1 )).

fof(tlhfof36444,axiom,(
    cell86 != cell11 )).

fof(tlhfof36445,axiom,(
    cell86 != cell20 )).

fof(tlhfof36446,axiom,(
    cell85 != cell84 )).

fof(tlhfof36447,axiom,(
    cell85 != cell83 )).

fof(tlhfof36448,axiom,(
    cell85 != cell82 )).

fof(tlhfof36449,axiom,(
    cell85 != cell81 )).

fof(tlhfof36450,axiom,(
    cell85 != cell80 )).

fof(tlhfof36451,axiom,(
    cell85 != cell79 )).

fof(tlhfof36452,axiom,(
    cell85 != cell78 )).

fof(tlhfof36453,axiom,(
    cell85 != cell77 )).

fof(tlhfof36454,axiom,(
    cell85 != cell76 )).

fof(tlhfof36455,axiom,(
    cell85 != cell75 )).

fof(tlhfof36456,axiom,(
    cell85 != cell74 )).

fof(tlhfof36457,axiom,(
    cell85 != cell73 )).

fof(tlhfof36458,axiom,(
    cell85 != cell72 )).

fof(tlhfof36459,axiom,(
    cell85 != cell71 )).

fof(tlhfof36460,axiom,(
    cell85 != cell70 )).

fof(tlhfof36461,axiom,(
    cell85 != cell69 )).

fof(tlhfof36462,axiom,(
    cell85 != cell68 )).

fof(tlhfof36463,axiom,(
    cell85 != cell67 )).

fof(tlhfof36464,axiom,(
    cell85 != cell66 )).

fof(tlhfof36465,axiom,(
    cell85 != cell65 )).

fof(tlhfof36466,axiom,(
    cell85 != cell64 )).

fof(tlhfof36467,axiom,(
    cell85 != cell63 )).

fof(tlhfof36468,axiom,(
    cell85 != cell62 )).

fof(tlhfof36469,axiom,(
    cell85 != cell61 )).

fof(tlhfof36470,axiom,(
    cell85 != cell60 )).

fof(tlhfof36471,axiom,(
    cell85 != cell59 )).

fof(tlhfof36472,axiom,(
    cell85 != cell58 )).

fof(tlhfof36473,axiom,(
    cell85 != cell57 )).

fof(tlhfof36474,axiom,(
    cell85 != cell56 )).

fof(tlhfof36475,axiom,(
    cell85 != cell55 )).

fof(tlhfof36476,axiom,(
    cell85 != cell54 )).

fof(tlhfof36477,axiom,(
    cell85 != cell53 )).

fof(tlhfof36478,axiom,(
    cell85 != cell52 )).

fof(tlhfof36479,axiom,(
    cell85 != cell51 )).

fof(tlhfof36480,axiom,(
    cell85 != cell50 )).

fof(tlhfof36481,axiom,(
    cell85 != cell49 )).

fof(tlhfof36482,axiom,(
    cell85 != cell48 )).

fof(tlhfof36483,axiom,(
    cell85 != cell47 )).

fof(tlhfof36484,axiom,(
    cell85 != cell46 )).

fof(tlhfof36485,axiom,(
    cell85 != cell45 )).

fof(tlhfof36486,axiom,(
    cell85 != cell44 )).

fof(tlhfof36487,axiom,(
    cell85 != cell43 )).

fof(tlhfof36488,axiom,(
    cell85 != cell42 )).

fof(tlhfof36489,axiom,(
    cell85 != cell41 )).

fof(tlhfof36490,axiom,(
    cell85 != cell40 )).

fof(tlhfof36491,axiom,(
    cell85 != cell39 )).

fof(tlhfof36492,axiom,(
    cell85 != cell38 )).

fof(tlhfof36493,axiom,(
    cell85 != cell37 )).

fof(tlhfof36494,axiom,(
    cell85 != cell36 )).

fof(tlhfof36495,axiom,(
    cell85 != cell35 )).

fof(tlhfof36496,axiom,(
    cell85 != cell34 )).

fof(tlhfof36497,axiom,(
    cell85 != cell33 )).

fof(tlhfof36498,axiom,(
    cell85 != cell32 )).

fof(tlhfof36499,axiom,(
    cell85 != cell31 )).

fof(tlhfof36500,axiom,(
    cell85 != cell30 )).

fof(tlhfof36501,axiom,(
    cell85 != cell29 )).

fof(tlhfof36502,axiom,(
    cell85 != cell28 )).

fof(tlhfof36503,axiom,(
    cell85 != cell27 )).

fof(tlhfof36504,axiom,(
    cell85 != cell26 )).

fof(tlhfof36505,axiom,(
    cell85 != cell25 )).

fof(tlhfof36506,axiom,(
    cell85 != cell24 )).

fof(tlhfof36507,axiom,(
    cell85 != cell23 )).

fof(tlhfof36508,axiom,(
    cell85 != cell22 )).

fof(tlhfof36509,axiom,(
    cell85 != cell21 )).

fof(tlhfof36510,axiom,(
    cell85 != cell10 )).

fof(tlhfof36511,axiom,(
    cell85 != cell9 )).

fof(tlhfof36512,axiom,(
    cell85 != cell19 )).

fof(tlhfof36513,axiom,(
    cell85 != cell8 )).

fof(tlhfof36514,axiom,(
    cell85 != cell18 )).

fof(tlhfof36515,axiom,(
    cell85 != cell7 )).

fof(tlhfof36516,axiom,(
    cell85 != cell17 )).

fof(tlhfof36517,axiom,(
    cell85 != cell6 )).

fof(tlhfof36518,axiom,(
    cell85 != cell16 )).

fof(tlhfof36519,axiom,(
    cell85 != cell5 )).

fof(tlhfof36520,axiom,(
    cell85 != cell15 )).

fof(tlhfof36521,axiom,(
    cell85 != cell4 )).

fof(tlhfof36522,axiom,(
    cell85 != cell14 )).

fof(tlhfof36523,axiom,(
    cell85 != cell3 )).

fof(tlhfof36524,axiom,(
    cell85 != cell13 )).

fof(tlhfof36525,axiom,(
    cell85 != cell2 )).

fof(tlhfof36526,axiom,(
    cell85 != cell12 )).

fof(tlhfof36527,axiom,(
    cell85 != cell1 )).

fof(tlhfof36528,axiom,(
    cell85 != cell11 )).

fof(tlhfof36529,axiom,(
    cell85 != cell20 )).

fof(tlhfof36530,axiom,(
    cell84 != cell83 )).

fof(tlhfof36531,axiom,(
    cell84 != cell82 )).

fof(tlhfof36532,axiom,(
    cell84 != cell81 )).

fof(tlhfof36533,axiom,(
    cell84 != cell80 )).

fof(tlhfof36534,axiom,(
    cell84 != cell79 )).

fof(tlhfof36535,axiom,(
    cell84 != cell78 )).

fof(tlhfof36536,axiom,(
    cell84 != cell77 )).

fof(tlhfof36537,axiom,(
    cell84 != cell76 )).

fof(tlhfof36538,axiom,(
    cell84 != cell75 )).

fof(tlhfof36539,axiom,(
    cell84 != cell74 )).

fof(tlhfof36540,axiom,(
    cell84 != cell73 )).

fof(tlhfof36541,axiom,(
    cell84 != cell72 )).

fof(tlhfof36542,axiom,(
    cell84 != cell71 )).

fof(tlhfof36543,axiom,(
    cell84 != cell70 )).

fof(tlhfof36544,axiom,(
    cell84 != cell69 )).

fof(tlhfof36545,axiom,(
    cell84 != cell68 )).

fof(tlhfof36546,axiom,(
    cell84 != cell67 )).

fof(tlhfof36547,axiom,(
    cell84 != cell66 )).

fof(tlhfof36548,axiom,(
    cell84 != cell65 )).

fof(tlhfof36549,axiom,(
    cell84 != cell64 )).

fof(tlhfof36550,axiom,(
    cell84 != cell63 )).

fof(tlhfof36551,axiom,(
    cell84 != cell62 )).

fof(tlhfof36552,axiom,(
    cell84 != cell61 )).

fof(tlhfof36553,axiom,(
    cell84 != cell60 )).

fof(tlhfof36554,axiom,(
    cell84 != cell59 )).

fof(tlhfof36555,axiom,(
    cell84 != cell58 )).

fof(tlhfof36556,axiom,(
    cell84 != cell57 )).

fof(tlhfof36557,axiom,(
    cell84 != cell56 )).

fof(tlhfof36558,axiom,(
    cell84 != cell55 )).

fof(tlhfof36559,axiom,(
    cell84 != cell54 )).

fof(tlhfof36560,axiom,(
    cell84 != cell53 )).

fof(tlhfof36561,axiom,(
    cell84 != cell52 )).

fof(tlhfof36562,axiom,(
    cell84 != cell51 )).

fof(tlhfof36563,axiom,(
    cell84 != cell50 )).

fof(tlhfof36564,axiom,(
    cell84 != cell49 )).

fof(tlhfof36565,axiom,(
    cell84 != cell48 )).

fof(tlhfof36566,axiom,(
    cell84 != cell47 )).

fof(tlhfof36567,axiom,(
    cell84 != cell46 )).

fof(tlhfof36568,axiom,(
    cell84 != cell45 )).

fof(tlhfof36569,axiom,(
    cell84 != cell44 )).

fof(tlhfof36570,axiom,(
    cell84 != cell43 )).

fof(tlhfof36571,axiom,(
    cell84 != cell42 )).

fof(tlhfof36572,axiom,(
    cell84 != cell41 )).

fof(tlhfof36573,axiom,(
    cell84 != cell40 )).

fof(tlhfof36574,axiom,(
    cell84 != cell39 )).

fof(tlhfof36575,axiom,(
    cell84 != cell38 )).

fof(tlhfof36576,axiom,(
    cell84 != cell37 )).

fof(tlhfof36577,axiom,(
    cell84 != cell36 )).

fof(tlhfof36578,axiom,(
    cell84 != cell35 )).

fof(tlhfof36579,axiom,(
    cell84 != cell34 )).

fof(tlhfof36580,axiom,(
    cell84 != cell33 )).

fof(tlhfof36581,axiom,(
    cell84 != cell32 )).

fof(tlhfof36582,axiom,(
    cell84 != cell31 )).

fof(tlhfof36583,axiom,(
    cell84 != cell30 )).

fof(tlhfof36584,axiom,(
    cell84 != cell29 )).

fof(tlhfof36585,axiom,(
    cell84 != cell28 )).

fof(tlhfof36586,axiom,(
    cell84 != cell27 )).

fof(tlhfof36587,axiom,(
    cell84 != cell26 )).

fof(tlhfof36588,axiom,(
    cell84 != cell25 )).

fof(tlhfof36589,axiom,(
    cell84 != cell24 )).

fof(tlhfof36590,axiom,(
    cell84 != cell23 )).

fof(tlhfof36591,axiom,(
    cell84 != cell22 )).

fof(tlhfof36592,axiom,(
    cell84 != cell21 )).

fof(tlhfof36593,axiom,(
    cell84 != cell10 )).

fof(tlhfof36594,axiom,(
    cell84 != cell9 )).

fof(tlhfof36595,axiom,(
    cell84 != cell19 )).

fof(tlhfof36596,axiom,(
    cell84 != cell8 )).

fof(tlhfof36597,axiom,(
    cell84 != cell18 )).

fof(tlhfof36598,axiom,(
    cell84 != cell7 )).

fof(tlhfof36599,axiom,(
    cell84 != cell17 )).

fof(tlhfof36600,axiom,(
    cell84 != cell6 )).

fof(tlhfof36601,axiom,(
    cell84 != cell16 )).

fof(tlhfof36602,axiom,(
    cell84 != cell5 )).

fof(tlhfof36603,axiom,(
    cell84 != cell15 )).

fof(tlhfof36604,axiom,(
    cell84 != cell4 )).

fof(tlhfof36605,axiom,(
    cell84 != cell14 )).

fof(tlhfof36606,axiom,(
    cell84 != cell3 )).

fof(tlhfof36607,axiom,(
    cell84 != cell13 )).

fof(tlhfof36608,axiom,(
    cell84 != cell2 )).

fof(tlhfof36609,axiom,(
    cell84 != cell12 )).

fof(tlhfof36610,axiom,(
    cell84 != cell1 )).

fof(tlhfof36611,axiom,(
    cell84 != cell11 )).

fof(tlhfof36612,axiom,(
    cell84 != cell20 )).

fof(tlhfof36613,axiom,(
    cell83 != cell82 )).

fof(tlhfof36614,axiom,(
    cell83 != cell81 )).

fof(tlhfof36615,axiom,(
    cell83 != cell80 )).

fof(tlhfof36616,axiom,(
    cell83 != cell79 )).

fof(tlhfof36617,axiom,(
    cell83 != cell78 )).

fof(tlhfof36618,axiom,(
    cell83 != cell77 )).

fof(tlhfof36619,axiom,(
    cell83 != cell76 )).

fof(tlhfof36620,axiom,(
    cell83 != cell75 )).

fof(tlhfof36621,axiom,(
    cell83 != cell74 )).

fof(tlhfof36622,axiom,(
    cell83 != cell73 )).

fof(tlhfof36623,axiom,(
    cell83 != cell72 )).

fof(tlhfof36624,axiom,(
    cell83 != cell71 )).

fof(tlhfof36625,axiom,(
    cell83 != cell70 )).

fof(tlhfof36626,axiom,(
    cell83 != cell69 )).

fof(tlhfof36627,axiom,(
    cell83 != cell68 )).

fof(tlhfof36628,axiom,(
    cell83 != cell67 )).

fof(tlhfof36629,axiom,(
    cell83 != cell66 )).

fof(tlhfof36630,axiom,(
    cell83 != cell65 )).

fof(tlhfof36631,axiom,(
    cell83 != cell64 )).

fof(tlhfof36632,axiom,(
    cell83 != cell63 )).

fof(tlhfof36633,axiom,(
    cell83 != cell62 )).

fof(tlhfof36634,axiom,(
    cell83 != cell61 )).

fof(tlhfof36635,axiom,(
    cell83 != cell60 )).

fof(tlhfof36636,axiom,(
    cell83 != cell59 )).

fof(tlhfof36637,axiom,(
    cell83 != cell58 )).

fof(tlhfof36638,axiom,(
    cell83 != cell57 )).

fof(tlhfof36639,axiom,(
    cell83 != cell56 )).

fof(tlhfof36640,axiom,(
    cell83 != cell55 )).

fof(tlhfof36641,axiom,(
    cell83 != cell54 )).

fof(tlhfof36642,axiom,(
    cell83 != cell53 )).

fof(tlhfof36643,axiom,(
    cell83 != cell52 )).

fof(tlhfof36644,axiom,(
    cell83 != cell51 )).

fof(tlhfof36645,axiom,(
    cell83 != cell50 )).

fof(tlhfof36646,axiom,(
    cell83 != cell49 )).

fof(tlhfof36647,axiom,(
    cell83 != cell48 )).

fof(tlhfof36648,axiom,(
    cell83 != cell47 )).

fof(tlhfof36649,axiom,(
    cell83 != cell46 )).

fof(tlhfof36650,axiom,(
    cell83 != cell45 )).

fof(tlhfof36651,axiom,(
    cell83 != cell44 )).

fof(tlhfof36652,axiom,(
    cell83 != cell43 )).

fof(tlhfof36653,axiom,(
    cell83 != cell42 )).

fof(tlhfof36654,axiom,(
    cell83 != cell41 )).

fof(tlhfof36655,axiom,(
    cell83 != cell40 )).

fof(tlhfof36656,axiom,(
    cell83 != cell39 )).

fof(tlhfof36657,axiom,(
    cell83 != cell38 )).

fof(tlhfof36658,axiom,(
    cell83 != cell37 )).

fof(tlhfof36659,axiom,(
    cell83 != cell36 )).

fof(tlhfof36660,axiom,(
    cell83 != cell35 )).

fof(tlhfof36661,axiom,(
    cell83 != cell34 )).

fof(tlhfof36662,axiom,(
    cell83 != cell33 )).

fof(tlhfof36663,axiom,(
    cell83 != cell32 )).

fof(tlhfof36664,axiom,(
    cell83 != cell31 )).

fof(tlhfof36665,axiom,(
    cell83 != cell30 )).

fof(tlhfof36666,axiom,(
    cell83 != cell29 )).

fof(tlhfof36667,axiom,(
    cell83 != cell28 )).

fof(tlhfof36668,axiom,(
    cell83 != cell27 )).

fof(tlhfof36669,axiom,(
    cell83 != cell26 )).

fof(tlhfof36670,axiom,(
    cell83 != cell25 )).

fof(tlhfof36671,axiom,(
    cell83 != cell24 )).

fof(tlhfof36672,axiom,(
    cell83 != cell23 )).

fof(tlhfof36673,axiom,(
    cell83 != cell22 )).

fof(tlhfof36674,axiom,(
    cell83 != cell21 )).

fof(tlhfof36675,axiom,(
    cell83 != cell10 )).

fof(tlhfof36676,axiom,(
    cell83 != cell9 )).

fof(tlhfof36677,axiom,(
    cell83 != cell19 )).

fof(tlhfof36678,axiom,(
    cell83 != cell8 )).

fof(tlhfof36679,axiom,(
    cell83 != cell18 )).

fof(tlhfof36680,axiom,(
    cell83 != cell7 )).

fof(tlhfof36681,axiom,(
    cell83 != cell17 )).

fof(tlhfof36682,axiom,(
    cell83 != cell6 )).

fof(tlhfof36683,axiom,(
    cell83 != cell16 )).

fof(tlhfof36684,axiom,(
    cell83 != cell5 )).

fof(tlhfof36685,axiom,(
    cell83 != cell15 )).

fof(tlhfof36686,axiom,(
    cell83 != cell4 )).

fof(tlhfof36687,axiom,(
    cell83 != cell14 )).

fof(tlhfof36688,axiom,(
    cell83 != cell3 )).

fof(tlhfof36689,axiom,(
    cell83 != cell13 )).

fof(tlhfof36690,axiom,(
    cell83 != cell2 )).

fof(tlhfof36691,axiom,(
    cell83 != cell12 )).

fof(tlhfof36692,axiom,(
    cell83 != cell1 )).

fof(tlhfof36693,axiom,(
    cell83 != cell11 )).

fof(tlhfof36694,axiom,(
    cell83 != cell20 )).

fof(tlhfof36695,axiom,(
    cell82 != cell81 )).

fof(tlhfof36696,axiom,(
    cell82 != cell80 )).

fof(tlhfof36697,axiom,(
    cell82 != cell79 )).

fof(tlhfof36698,axiom,(
    cell82 != cell78 )).

fof(tlhfof36699,axiom,(
    cell82 != cell77 )).

fof(tlhfof36700,axiom,(
    cell82 != cell76 )).

fof(tlhfof36701,axiom,(
    cell82 != cell75 )).

fof(tlhfof36702,axiom,(
    cell82 != cell74 )).

fof(tlhfof36703,axiom,(
    cell82 != cell73 )).

fof(tlhfof36704,axiom,(
    cell82 != cell72 )).

fof(tlhfof36705,axiom,(
    cell82 != cell71 )).

fof(tlhfof36706,axiom,(
    cell82 != cell70 )).

fof(tlhfof36707,axiom,(
    cell82 != cell69 )).

fof(tlhfof36708,axiom,(
    cell82 != cell68 )).

fof(tlhfof36709,axiom,(
    cell82 != cell67 )).

fof(tlhfof36710,axiom,(
    cell82 != cell66 )).

fof(tlhfof36711,axiom,(
    cell82 != cell65 )).

fof(tlhfof36712,axiom,(
    cell82 != cell64 )).

fof(tlhfof36713,axiom,(
    cell82 != cell63 )).

fof(tlhfof36714,axiom,(
    cell82 != cell62 )).

fof(tlhfof36715,axiom,(
    cell82 != cell61 )).

fof(tlhfof36716,axiom,(
    cell82 != cell60 )).

fof(tlhfof36717,axiom,(
    cell82 != cell59 )).

fof(tlhfof36718,axiom,(
    cell82 != cell58 )).

fof(tlhfof36719,axiom,(
    cell82 != cell57 )).

fof(tlhfof36720,axiom,(
    cell82 != cell56 )).

fof(tlhfof36721,axiom,(
    cell82 != cell55 )).

fof(tlhfof36722,axiom,(
    cell82 != cell54 )).

fof(tlhfof36723,axiom,(
    cell82 != cell53 )).

fof(tlhfof36724,axiom,(
    cell82 != cell52 )).

fof(tlhfof36725,axiom,(
    cell82 != cell51 )).

fof(tlhfof36726,axiom,(
    cell82 != cell50 )).

fof(tlhfof36727,axiom,(
    cell82 != cell49 )).

fof(tlhfof36728,axiom,(
    cell82 != cell48 )).

fof(tlhfof36729,axiom,(
    cell82 != cell47 )).

fof(tlhfof36730,axiom,(
    cell82 != cell46 )).

fof(tlhfof36731,axiom,(
    cell82 != cell45 )).

fof(tlhfof36732,axiom,(
    cell82 != cell44 )).

fof(tlhfof36733,axiom,(
    cell82 != cell43 )).

fof(tlhfof36734,axiom,(
    cell82 != cell42 )).

fof(tlhfof36735,axiom,(
    cell82 != cell41 )).

fof(tlhfof36736,axiom,(
    cell82 != cell40 )).

fof(tlhfof36737,axiom,(
    cell82 != cell39 )).

fof(tlhfof36738,axiom,(
    cell82 != cell38 )).

fof(tlhfof36739,axiom,(
    cell82 != cell37 )).

fof(tlhfof36740,axiom,(
    cell82 != cell36 )).

fof(tlhfof36741,axiom,(
    cell82 != cell35 )).

fof(tlhfof36742,axiom,(
    cell82 != cell34 )).

fof(tlhfof36743,axiom,(
    cell82 != cell33 )).

fof(tlhfof36744,axiom,(
    cell82 != cell32 )).

fof(tlhfof36745,axiom,(
    cell82 != cell31 )).

fof(tlhfof36746,axiom,(
    cell82 != cell30 )).

fof(tlhfof36747,axiom,(
    cell82 != cell29 )).

fof(tlhfof36748,axiom,(
    cell82 != cell28 )).

fof(tlhfof36749,axiom,(
    cell82 != cell27 )).

fof(tlhfof36750,axiom,(
    cell82 != cell26 )).

fof(tlhfof36751,axiom,(
    cell82 != cell25 )).

fof(tlhfof36752,axiom,(
    cell82 != cell24 )).

fof(tlhfof36753,axiom,(
    cell82 != cell23 )).

fof(tlhfof36754,axiom,(
    cell82 != cell22 )).

fof(tlhfof36755,axiom,(
    cell82 != cell21 )).

fof(tlhfof36756,axiom,(
    cell82 != cell10 )).

fof(tlhfof36757,axiom,(
    cell82 != cell9 )).

fof(tlhfof36758,axiom,(
    cell82 != cell19 )).

fof(tlhfof36759,axiom,(
    cell82 != cell8 )).

fof(tlhfof36760,axiom,(
    cell82 != cell18 )).

fof(tlhfof36761,axiom,(
    cell82 != cell7 )).

fof(tlhfof36762,axiom,(
    cell82 != cell17 )).

fof(tlhfof36763,axiom,(
    cell82 != cell6 )).

fof(tlhfof36764,axiom,(
    cell82 != cell16 )).

fof(tlhfof36765,axiom,(
    cell82 != cell5 )).

fof(tlhfof36766,axiom,(
    cell82 != cell15 )).

fof(tlhfof36767,axiom,(
    cell82 != cell4 )).

fof(tlhfof36768,axiom,(
    cell82 != cell14 )).

fof(tlhfof36769,axiom,(
    cell82 != cell3 )).

fof(tlhfof36770,axiom,(
    cell82 != cell13 )).

fof(tlhfof36771,axiom,(
    cell82 != cell2 )).

fof(tlhfof36772,axiom,(
    cell82 != cell12 )).

fof(tlhfof36773,axiom,(
    cell82 != cell1 )).

fof(tlhfof36774,axiom,(
    cell82 != cell11 )).

fof(tlhfof36775,axiom,(
    cell82 != cell20 )).

fof(tlhfof36776,axiom,(
    cell81 != cell80 )).

fof(tlhfof36777,axiom,(
    cell81 != cell79 )).

fof(tlhfof36778,axiom,(
    cell81 != cell78 )).

fof(tlhfof36779,axiom,(
    cell81 != cell77 )).

fof(tlhfof36780,axiom,(
    cell81 != cell76 )).

fof(tlhfof36781,axiom,(
    cell81 != cell75 )).

fof(tlhfof36782,axiom,(
    cell81 != cell74 )).

fof(tlhfof36783,axiom,(
    cell81 != cell73 )).

fof(tlhfof36784,axiom,(
    cell81 != cell72 )).

fof(tlhfof36785,axiom,(
    cell81 != cell71 )).

fof(tlhfof36786,axiom,(
    cell81 != cell70 )).

fof(tlhfof36787,axiom,(
    cell81 != cell69 )).

fof(tlhfof36788,axiom,(
    cell81 != cell68 )).

fof(tlhfof36789,axiom,(
    cell81 != cell67 )).

fof(tlhfof36790,axiom,(
    cell81 != cell66 )).

fof(tlhfof36791,axiom,(
    cell81 != cell65 )).

fof(tlhfof36792,axiom,(
    cell81 != cell64 )).

fof(tlhfof36793,axiom,(
    cell81 != cell63 )).

fof(tlhfof36794,axiom,(
    cell81 != cell62 )).

fof(tlhfof36795,axiom,(
    cell81 != cell61 )).

fof(tlhfof36796,axiom,(
    cell81 != cell60 )).

fof(tlhfof36797,axiom,(
    cell81 != cell59 )).

fof(tlhfof36798,axiom,(
    cell81 != cell58 )).

fof(tlhfof36799,axiom,(
    cell81 != cell57 )).

fof(tlhfof36800,axiom,(
    cell81 != cell56 )).

fof(tlhfof36801,axiom,(
    cell81 != cell55 )).

fof(tlhfof36802,axiom,(
    cell81 != cell54 )).

fof(tlhfof36803,axiom,(
    cell81 != cell53 )).

fof(tlhfof36804,axiom,(
    cell81 != cell52 )).

fof(tlhfof36805,axiom,(
    cell81 != cell51 )).

fof(tlhfof36806,axiom,(
    cell81 != cell50 )).

fof(tlhfof36807,axiom,(
    cell81 != cell49 )).

fof(tlhfof36808,axiom,(
    cell81 != cell48 )).

fof(tlhfof36809,axiom,(
    cell81 != cell47 )).

fof(tlhfof36810,axiom,(
    cell81 != cell46 )).

fof(tlhfof36811,axiom,(
    cell81 != cell45 )).

fof(tlhfof36812,axiom,(
    cell81 != cell44 )).

fof(tlhfof36813,axiom,(
    cell81 != cell43 )).

fof(tlhfof36814,axiom,(
    cell81 != cell42 )).

fof(tlhfof36815,axiom,(
    cell81 != cell41 )).

fof(tlhfof36816,axiom,(
    cell81 != cell40 )).

fof(tlhfof36817,axiom,(
    cell81 != cell39 )).

fof(tlhfof36818,axiom,(
    cell81 != cell38 )).

fof(tlhfof36819,axiom,(
    cell81 != cell37 )).

fof(tlhfof36820,axiom,(
    cell81 != cell36 )).

fof(tlhfof36821,axiom,(
    cell81 != cell35 )).

fof(tlhfof36822,axiom,(
    cell81 != cell34 )).

fof(tlhfof36823,axiom,(
    cell81 != cell33 )).

fof(tlhfof36824,axiom,(
    cell81 != cell32 )).

fof(tlhfof36825,axiom,(
    cell81 != cell31 )).

fof(tlhfof36826,axiom,(
    cell81 != cell30 )).

fof(tlhfof36827,axiom,(
    cell81 != cell29 )).

fof(tlhfof36828,axiom,(
    cell81 != cell28 )).

fof(tlhfof36829,axiom,(
    cell81 != cell27 )).

fof(tlhfof36830,axiom,(
    cell81 != cell26 )).

fof(tlhfof36831,axiom,(
    cell81 != cell25 )).

fof(tlhfof36832,axiom,(
    cell81 != cell24 )).

fof(tlhfof36833,axiom,(
    cell81 != cell23 )).

fof(tlhfof36834,axiom,(
    cell81 != cell22 )).

fof(tlhfof36835,axiom,(
    cell81 != cell21 )).

fof(tlhfof36836,axiom,(
    cell81 != cell10 )).

fof(tlhfof36837,axiom,(
    cell81 != cell9 )).

fof(tlhfof36838,axiom,(
    cell81 != cell19 )).

fof(tlhfof36839,axiom,(
    cell81 != cell8 )).

fof(tlhfof36840,axiom,(
    cell81 != cell18 )).

fof(tlhfof36841,axiom,(
    cell81 != cell7 )).

fof(tlhfof36842,axiom,(
    cell81 != cell17 )).

fof(tlhfof36843,axiom,(
    cell81 != cell6 )).

fof(tlhfof36844,axiom,(
    cell81 != cell16 )).

fof(tlhfof36845,axiom,(
    cell81 != cell5 )).

fof(tlhfof36846,axiom,(
    cell81 != cell15 )).

fof(tlhfof36847,axiom,(
    cell81 != cell4 )).

fof(tlhfof36848,axiom,(
    cell81 != cell14 )).

fof(tlhfof36849,axiom,(
    cell81 != cell3 )).

fof(tlhfof36850,axiom,(
    cell81 != cell13 )).

fof(tlhfof36851,axiom,(
    cell81 != cell2 )).

fof(tlhfof36852,axiom,(
    cell81 != cell12 )).

fof(tlhfof36853,axiom,(
    cell81 != cell1 )).

fof(tlhfof36854,axiom,(
    cell81 != cell11 )).

fof(tlhfof36855,axiom,(
    cell81 != cell20 )).

fof(tlhfof36856,axiom,(
    cell80 != cell79 )).

fof(tlhfof36857,axiom,(
    cell80 != cell78 )).

fof(tlhfof36858,axiom,(
    cell80 != cell77 )).

fof(tlhfof36859,axiom,(
    cell80 != cell76 )).

fof(tlhfof36860,axiom,(
    cell80 != cell75 )).

fof(tlhfof36861,axiom,(
    cell80 != cell74 )).

fof(tlhfof36862,axiom,(
    cell80 != cell73 )).

fof(tlhfof36863,axiom,(
    cell80 != cell72 )).

fof(tlhfof36864,axiom,(
    cell80 != cell71 )).

fof(tlhfof36865,axiom,(
    cell80 != cell70 )).

fof(tlhfof36866,axiom,(
    cell80 != cell69 )).

fof(tlhfof36867,axiom,(
    cell80 != cell68 )).

fof(tlhfof36868,axiom,(
    cell80 != cell67 )).

fof(tlhfof36869,axiom,(
    cell80 != cell66 )).

fof(tlhfof36870,axiom,(
    cell80 != cell65 )).

fof(tlhfof36871,axiom,(
    cell80 != cell64 )).

fof(tlhfof36872,axiom,(
    cell80 != cell63 )).

fof(tlhfof36873,axiom,(
    cell80 != cell62 )).

fof(tlhfof36874,axiom,(
    cell80 != cell61 )).

fof(tlhfof36875,axiom,(
    cell80 != cell60 )).

fof(tlhfof36876,axiom,(
    cell80 != cell59 )).

fof(tlhfof36877,axiom,(
    cell80 != cell58 )).

fof(tlhfof36878,axiom,(
    cell80 != cell57 )).

fof(tlhfof36879,axiom,(
    cell80 != cell56 )).

fof(tlhfof36880,axiom,(
    cell80 != cell55 )).

fof(tlhfof36881,axiom,(
    cell80 != cell54 )).

fof(tlhfof36882,axiom,(
    cell80 != cell53 )).

fof(tlhfof36883,axiom,(
    cell80 != cell52 )).

fof(tlhfof36884,axiom,(
    cell80 != cell51 )).

fof(tlhfof36885,axiom,(
    cell80 != cell50 )).

fof(tlhfof36886,axiom,(
    cell80 != cell49 )).

fof(tlhfof36887,axiom,(
    cell80 != cell48 )).

fof(tlhfof36888,axiom,(
    cell80 != cell47 )).

fof(tlhfof36889,axiom,(
    cell80 != cell46 )).

fof(tlhfof36890,axiom,(
    cell80 != cell45 )).

fof(tlhfof36891,axiom,(
    cell80 != cell44 )).

fof(tlhfof36892,axiom,(
    cell80 != cell43 )).

fof(tlhfof36893,axiom,(
    cell80 != cell42 )).

fof(tlhfof36894,axiom,(
    cell80 != cell41 )).

fof(tlhfof36895,axiom,(
    cell80 != cell40 )).

fof(tlhfof36896,axiom,(
    cell80 != cell39 )).

fof(tlhfof36897,axiom,(
    cell80 != cell38 )).

fof(tlhfof36898,axiom,(
    cell80 != cell37 )).

fof(tlhfof36899,axiom,(
    cell80 != cell36 )).

fof(tlhfof36900,axiom,(
    cell80 != cell35 )).

fof(tlhfof36901,axiom,(
    cell80 != cell34 )).

fof(tlhfof36902,axiom,(
    cell80 != cell33 )).

fof(tlhfof36903,axiom,(
    cell80 != cell32 )).

fof(tlhfof36904,axiom,(
    cell80 != cell31 )).

fof(tlhfof36905,axiom,(
    cell80 != cell30 )).

fof(tlhfof36906,axiom,(
    cell80 != cell29 )).

fof(tlhfof36907,axiom,(
    cell80 != cell28 )).

fof(tlhfof36908,axiom,(
    cell80 != cell27 )).

fof(tlhfof36909,axiom,(
    cell80 != cell26 )).

fof(tlhfof36910,axiom,(
    cell80 != cell25 )).

fof(tlhfof36911,axiom,(
    cell80 != cell24 )).

fof(tlhfof36912,axiom,(
    cell80 != cell23 )).

fof(tlhfof36913,axiom,(
    cell80 != cell22 )).

fof(tlhfof36914,axiom,(
    cell80 != cell21 )).

fof(tlhfof36915,axiom,(
    cell80 != cell10 )).

fof(tlhfof36916,axiom,(
    cell80 != cell9 )).

fof(tlhfof36917,axiom,(
    cell80 != cell19 )).

fof(tlhfof36918,axiom,(
    cell80 != cell8 )).

fof(tlhfof36919,axiom,(
    cell80 != cell18 )).

fof(tlhfof36920,axiom,(
    cell80 != cell7 )).

fof(tlhfof36921,axiom,(
    cell80 != cell17 )).

fof(tlhfof36922,axiom,(
    cell80 != cell6 )).

fof(tlhfof36923,axiom,(
    cell80 != cell16 )).

fof(tlhfof36924,axiom,(
    cell80 != cell5 )).

fof(tlhfof36925,axiom,(
    cell80 != cell15 )).

fof(tlhfof36926,axiom,(
    cell80 != cell4 )).

fof(tlhfof36927,axiom,(
    cell80 != cell14 )).

fof(tlhfof36928,axiom,(
    cell80 != cell3 )).

fof(tlhfof36929,axiom,(
    cell80 != cell13 )).

fof(tlhfof36930,axiom,(
    cell80 != cell2 )).

fof(tlhfof36931,axiom,(
    cell80 != cell12 )).

fof(tlhfof36932,axiom,(
    cell80 != cell1 )).

fof(tlhfof36933,axiom,(
    cell80 != cell11 )).

fof(tlhfof36934,axiom,(
    cell80 != cell20 )).

fof(tlhfof36935,axiom,(
    cell79 != cell78 )).

fof(tlhfof36936,axiom,(
    cell79 != cell77 )).

fof(tlhfof36937,axiom,(
    cell79 != cell76 )).

fof(tlhfof36938,axiom,(
    cell79 != cell75 )).

fof(tlhfof36939,axiom,(
    cell79 != cell74 )).

fof(tlhfof36940,axiom,(
    cell79 != cell73 )).

fof(tlhfof36941,axiom,(
    cell79 != cell72 )).

fof(tlhfof36942,axiom,(
    cell79 != cell71 )).

fof(tlhfof36943,axiom,(
    cell79 != cell70 )).

fof(tlhfof36944,axiom,(
    cell79 != cell69 )).

fof(tlhfof36945,axiom,(
    cell79 != cell68 )).

fof(tlhfof36946,axiom,(
    cell79 != cell67 )).

fof(tlhfof36947,axiom,(
    cell79 != cell66 )).

fof(tlhfof36948,axiom,(
    cell79 != cell65 )).

fof(tlhfof36949,axiom,(
    cell79 != cell64 )).

fof(tlhfof36950,axiom,(
    cell79 != cell63 )).

fof(tlhfof36951,axiom,(
    cell79 != cell62 )).

fof(tlhfof36952,axiom,(
    cell79 != cell61 )).

fof(tlhfof36953,axiom,(
    cell79 != cell60 )).

fof(tlhfof36954,axiom,(
    cell79 != cell59 )).

fof(tlhfof36955,axiom,(
    cell79 != cell58 )).

fof(tlhfof36956,axiom,(
    cell79 != cell57 )).

fof(tlhfof36957,axiom,(
    cell79 != cell56 )).

fof(tlhfof36958,axiom,(
    cell79 != cell55 )).

fof(tlhfof36959,axiom,(
    cell79 != cell54 )).

fof(tlhfof36960,axiom,(
    cell79 != cell53 )).

fof(tlhfof36961,axiom,(
    cell79 != cell52 )).

fof(tlhfof36962,axiom,(
    cell79 != cell51 )).

fof(tlhfof36963,axiom,(
    cell79 != cell50 )).

fof(tlhfof36964,axiom,(
    cell79 != cell49 )).

fof(tlhfof36965,axiom,(
    cell79 != cell48 )).

fof(tlhfof36966,axiom,(
    cell79 != cell47 )).

fof(tlhfof36967,axiom,(
    cell79 != cell46 )).

fof(tlhfof36968,axiom,(
    cell79 != cell45 )).

fof(tlhfof36969,axiom,(
    cell79 != cell44 )).

fof(tlhfof36970,axiom,(
    cell79 != cell43 )).

fof(tlhfof36971,axiom,(
    cell79 != cell42 )).

fof(tlhfof36972,axiom,(
    cell79 != cell41 )).

fof(tlhfof36973,axiom,(
    cell79 != cell40 )).

fof(tlhfof36974,axiom,(
    cell79 != cell39 )).

fof(tlhfof36975,axiom,(
    cell79 != cell38 )).

fof(tlhfof36976,axiom,(
    cell79 != cell37 )).

fof(tlhfof36977,axiom,(
    cell79 != cell36 )).

fof(tlhfof36978,axiom,(
    cell79 != cell35 )).

fof(tlhfof36979,axiom,(
    cell79 != cell34 )).

fof(tlhfof36980,axiom,(
    cell79 != cell33 )).

fof(tlhfof36981,axiom,(
    cell79 != cell32 )).

fof(tlhfof36982,axiom,(
    cell79 != cell31 )).

fof(tlhfof36983,axiom,(
    cell79 != cell30 )).

fof(tlhfof36984,axiom,(
    cell79 != cell29 )).

fof(tlhfof36985,axiom,(
    cell79 != cell28 )).

fof(tlhfof36986,axiom,(
    cell79 != cell27 )).

fof(tlhfof36987,axiom,(
    cell79 != cell26 )).

fof(tlhfof36988,axiom,(
    cell79 != cell25 )).

fof(tlhfof36989,axiom,(
    cell79 != cell24 )).

fof(tlhfof36990,axiom,(
    cell79 != cell23 )).

fof(tlhfof36991,axiom,(
    cell79 != cell22 )).

fof(tlhfof36992,axiom,(
    cell79 != cell21 )).

fof(tlhfof36993,axiom,(
    cell79 != cell10 )).

fof(tlhfof36994,axiom,(
    cell79 != cell9 )).

fof(tlhfof36995,axiom,(
    cell79 != cell19 )).

fof(tlhfof36996,axiom,(
    cell79 != cell8 )).

fof(tlhfof36997,axiom,(
    cell79 != cell18 )).

fof(tlhfof36998,axiom,(
    cell79 != cell7 )).

fof(tlhfof36999,axiom,(
    cell79 != cell17 )).

fof(tlhfof37000,axiom,(
    cell79 != cell6 )).

fof(tlhfof37001,axiom,(
    cell79 != cell16 )).

fof(tlhfof37002,axiom,(
    cell79 != cell5 )).

fof(tlhfof37003,axiom,(
    cell79 != cell15 )).

fof(tlhfof37004,axiom,(
    cell79 != cell4 )).

fof(tlhfof37005,axiom,(
    cell79 != cell14 )).

fof(tlhfof37006,axiom,(
    cell79 != cell3 )).

fof(tlhfof37007,axiom,(
    cell79 != cell13 )).

fof(tlhfof37008,axiom,(
    cell79 != cell2 )).

fof(tlhfof37009,axiom,(
    cell79 != cell12 )).

fof(tlhfof37010,axiom,(
    cell79 != cell1 )).

fof(tlhfof37011,axiom,(
    cell79 != cell11 )).

fof(tlhfof37012,axiom,(
    cell79 != cell20 )).

fof(tlhfof37013,axiom,(
    cell78 != cell77 )).

fof(tlhfof37014,axiom,(
    cell78 != cell76 )).

fof(tlhfof37015,axiom,(
    cell78 != cell75 )).

fof(tlhfof37016,axiom,(
    cell78 != cell74 )).

fof(tlhfof37017,axiom,(
    cell78 != cell73 )).

fof(tlhfof37018,axiom,(
    cell78 != cell72 )).

fof(tlhfof37019,axiom,(
    cell78 != cell71 )).

fof(tlhfof37020,axiom,(
    cell78 != cell70 )).

fof(tlhfof37021,axiom,(
    cell78 != cell69 )).

fof(tlhfof37022,axiom,(
    cell78 != cell68 )).

fof(tlhfof37023,axiom,(
    cell78 != cell67 )).

fof(tlhfof37024,axiom,(
    cell78 != cell66 )).

fof(tlhfof37025,axiom,(
    cell78 != cell65 )).

fof(tlhfof37026,axiom,(
    cell78 != cell64 )).

fof(tlhfof37027,axiom,(
    cell78 != cell63 )).

fof(tlhfof37028,axiom,(
    cell78 != cell62 )).

fof(tlhfof37029,axiom,(
    cell78 != cell61 )).

fof(tlhfof37030,axiom,(
    cell78 != cell60 )).

fof(tlhfof37031,axiom,(
    cell78 != cell59 )).

fof(tlhfof37032,axiom,(
    cell78 != cell58 )).

fof(tlhfof37033,axiom,(
    cell78 != cell57 )).

fof(tlhfof37034,axiom,(
    cell78 != cell56 )).

fof(tlhfof37035,axiom,(
    cell78 != cell55 )).

fof(tlhfof37036,axiom,(
    cell78 != cell54 )).

fof(tlhfof37037,axiom,(
    cell78 != cell53 )).

fof(tlhfof37038,axiom,(
    cell78 != cell52 )).

fof(tlhfof37039,axiom,(
    cell78 != cell51 )).

fof(tlhfof37040,axiom,(
    cell78 != cell50 )).

fof(tlhfof37041,axiom,(
    cell78 != cell49 )).

fof(tlhfof37042,axiom,(
    cell78 != cell48 )).

fof(tlhfof37043,axiom,(
    cell78 != cell47 )).

fof(tlhfof37044,axiom,(
    cell78 != cell46 )).

fof(tlhfof37045,axiom,(
    cell78 != cell45 )).

fof(tlhfof37046,axiom,(
    cell78 != cell44 )).

fof(tlhfof37047,axiom,(
    cell78 != cell43 )).

fof(tlhfof37048,axiom,(
    cell78 != cell42 )).

fof(tlhfof37049,axiom,(
    cell78 != cell41 )).

fof(tlhfof37050,axiom,(
    cell78 != cell40 )).

fof(tlhfof37051,axiom,(
    cell78 != cell39 )).

fof(tlhfof37052,axiom,(
    cell78 != cell38 )).

fof(tlhfof37053,axiom,(
    cell78 != cell37 )).

fof(tlhfof37054,axiom,(
    cell78 != cell36 )).

fof(tlhfof37055,axiom,(
    cell78 != cell35 )).

fof(tlhfof37056,axiom,(
    cell78 != cell34 )).

fof(tlhfof37057,axiom,(
    cell78 != cell33 )).

fof(tlhfof37058,axiom,(
    cell78 != cell32 )).

fof(tlhfof37059,axiom,(
    cell78 != cell31 )).

fof(tlhfof37060,axiom,(
    cell78 != cell30 )).

fof(tlhfof37061,axiom,(
    cell78 != cell29 )).

fof(tlhfof37062,axiom,(
    cell78 != cell28 )).

fof(tlhfof37063,axiom,(
    cell78 != cell27 )).

fof(tlhfof37064,axiom,(
    cell78 != cell26 )).

fof(tlhfof37065,axiom,(
    cell78 != cell25 )).

fof(tlhfof37066,axiom,(
    cell78 != cell24 )).

fof(tlhfof37067,axiom,(
    cell78 != cell23 )).

fof(tlhfof37068,axiom,(
    cell78 != cell22 )).

fof(tlhfof37069,axiom,(
    cell78 != cell21 )).

fof(tlhfof37070,axiom,(
    cell78 != cell10 )).

fof(tlhfof37071,axiom,(
    cell78 != cell9 )).

fof(tlhfof37072,axiom,(
    cell78 != cell19 )).

fof(tlhfof37073,axiom,(
    cell78 != cell8 )).

fof(tlhfof37074,axiom,(
    cell78 != cell18 )).

fof(tlhfof37075,axiom,(
    cell78 != cell7 )).

fof(tlhfof37076,axiom,(
    cell78 != cell17 )).

fof(tlhfof37077,axiom,(
    cell78 != cell6 )).

fof(tlhfof37078,axiom,(
    cell78 != cell16 )).

fof(tlhfof37079,axiom,(
    cell78 != cell5 )).

fof(tlhfof37080,axiom,(
    cell78 != cell15 )).

fof(tlhfof37081,axiom,(
    cell78 != cell4 )).

fof(tlhfof37082,axiom,(
    cell78 != cell14 )).

fof(tlhfof37083,axiom,(
    cell78 != cell3 )).

fof(tlhfof37084,axiom,(
    cell78 != cell13 )).

fof(tlhfof37085,axiom,(
    cell78 != cell2 )).

fof(tlhfof37086,axiom,(
    cell78 != cell12 )).

fof(tlhfof37087,axiom,(
    cell78 != cell1 )).

fof(tlhfof37088,axiom,(
    cell78 != cell11 )).

fof(tlhfof37089,axiom,(
    cell78 != cell20 )).

fof(tlhfof37090,axiom,(
    cell77 != cell76 )).

fof(tlhfof37091,axiom,(
    cell77 != cell75 )).

fof(tlhfof37092,axiom,(
    cell77 != cell74 )).

fof(tlhfof37093,axiom,(
    cell77 != cell73 )).

fof(tlhfof37094,axiom,(
    cell77 != cell72 )).

fof(tlhfof37095,axiom,(
    cell77 != cell71 )).

fof(tlhfof37096,axiom,(
    cell77 != cell70 )).

fof(tlhfof37097,axiom,(
    cell77 != cell69 )).

fof(tlhfof37098,axiom,(
    cell77 != cell68 )).

fof(tlhfof37099,axiom,(
    cell77 != cell67 )).

fof(tlhfof37100,axiom,(
    cell77 != cell66 )).

fof(tlhfof37101,axiom,(
    cell77 != cell65 )).

fof(tlhfof37102,axiom,(
    cell77 != cell64 )).

fof(tlhfof37103,axiom,(
    cell77 != cell63 )).

fof(tlhfof37104,axiom,(
    cell77 != cell62 )).

fof(tlhfof37105,axiom,(
    cell77 != cell61 )).

fof(tlhfof37106,axiom,(
    cell77 != cell60 )).

fof(tlhfof37107,axiom,(
    cell77 != cell59 )).

fof(tlhfof37108,axiom,(
    cell77 != cell58 )).

fof(tlhfof37109,axiom,(
    cell77 != cell57 )).

fof(tlhfof37110,axiom,(
    cell77 != cell56 )).

fof(tlhfof37111,axiom,(
    cell77 != cell55 )).

fof(tlhfof37112,axiom,(
    cell77 != cell54 )).

fof(tlhfof37113,axiom,(
    cell77 != cell53 )).

fof(tlhfof37114,axiom,(
    cell77 != cell52 )).

fof(tlhfof37115,axiom,(
    cell77 != cell51 )).

fof(tlhfof37116,axiom,(
    cell77 != cell50 )).

fof(tlhfof37117,axiom,(
    cell77 != cell49 )).

fof(tlhfof37118,axiom,(
    cell77 != cell48 )).

fof(tlhfof37119,axiom,(
    cell77 != cell47 )).

fof(tlhfof37120,axiom,(
    cell77 != cell46 )).

fof(tlhfof37121,axiom,(
    cell77 != cell45 )).

fof(tlhfof37122,axiom,(
    cell77 != cell44 )).

fof(tlhfof37123,axiom,(
    cell77 != cell43 )).

fof(tlhfof37124,axiom,(
    cell77 != cell42 )).

fof(tlhfof37125,axiom,(
    cell77 != cell41 )).

fof(tlhfof37126,axiom,(
    cell77 != cell40 )).

fof(tlhfof37127,axiom,(
    cell77 != cell39 )).

fof(tlhfof37128,axiom,(
    cell77 != cell38 )).

fof(tlhfof37129,axiom,(
    cell77 != cell37 )).

fof(tlhfof37130,axiom,(
    cell77 != cell36 )).

fof(tlhfof37131,axiom,(
    cell77 != cell35 )).

fof(tlhfof37132,axiom,(
    cell77 != cell34 )).

fof(tlhfof37133,axiom,(
    cell77 != cell33 )).

fof(tlhfof37134,axiom,(
    cell77 != cell32 )).

fof(tlhfof37135,axiom,(
    cell77 != cell31 )).

fof(tlhfof37136,axiom,(
    cell77 != cell30 )).

fof(tlhfof37137,axiom,(
    cell77 != cell29 )).

fof(tlhfof37138,axiom,(
    cell77 != cell28 )).

fof(tlhfof37139,axiom,(
    cell77 != cell27 )).

fof(tlhfof37140,axiom,(
    cell77 != cell26 )).

fof(tlhfof37141,axiom,(
    cell77 != cell25 )).

fof(tlhfof37142,axiom,(
    cell77 != cell24 )).

fof(tlhfof37143,axiom,(
    cell77 != cell23 )).

fof(tlhfof37144,axiom,(
    cell77 != cell22 )).

fof(tlhfof37145,axiom,(
    cell77 != cell21 )).

fof(tlhfof37146,axiom,(
    cell77 != cell10 )).

fof(tlhfof37147,axiom,(
    cell77 != cell9 )).

fof(tlhfof37148,axiom,(
    cell77 != cell19 )).

fof(tlhfof37149,axiom,(
    cell77 != cell8 )).

fof(tlhfof37150,axiom,(
    cell77 != cell18 )).

fof(tlhfof37151,axiom,(
    cell77 != cell7 )).

fof(tlhfof37152,axiom,(
    cell77 != cell17 )).

fof(tlhfof37153,axiom,(
    cell77 != cell6 )).

fof(tlhfof37154,axiom,(
    cell77 != cell16 )).

fof(tlhfof37155,axiom,(
    cell77 != cell5 )).

fof(tlhfof37156,axiom,(
    cell77 != cell15 )).

fof(tlhfof37157,axiom,(
    cell77 != cell4 )).

fof(tlhfof37158,axiom,(
    cell77 != cell14 )).

fof(tlhfof37159,axiom,(
    cell77 != cell3 )).

fof(tlhfof37160,axiom,(
    cell77 != cell13 )).

fof(tlhfof37161,axiom,(
    cell77 != cell2 )).

fof(tlhfof37162,axiom,(
    cell77 != cell12 )).

fof(tlhfof37163,axiom,(
    cell77 != cell1 )).

fof(tlhfof37164,axiom,(
    cell77 != cell11 )).

fof(tlhfof37165,axiom,(
    cell77 != cell20 )).

fof(tlhfof37166,axiom,(
    cell76 != cell75 )).

fof(tlhfof37167,axiom,(
    cell76 != cell74 )).

fof(tlhfof37168,axiom,(
    cell76 != cell73 )).

fof(tlhfof37169,axiom,(
    cell76 != cell72 )).

fof(tlhfof37170,axiom,(
    cell76 != cell71 )).

fof(tlhfof37171,axiom,(
    cell76 != cell70 )).

fof(tlhfof37172,axiom,(
    cell76 != cell69 )).

fof(tlhfof37173,axiom,(
    cell76 != cell68 )).

fof(tlhfof37174,axiom,(
    cell76 != cell67 )).

fof(tlhfof37175,axiom,(
    cell76 != cell66 )).

fof(tlhfof37176,axiom,(
    cell76 != cell65 )).

fof(tlhfof37177,axiom,(
    cell76 != cell64 )).

fof(tlhfof37178,axiom,(
    cell76 != cell63 )).

fof(tlhfof37179,axiom,(
    cell76 != cell62 )).

fof(tlhfof37180,axiom,(
    cell76 != cell61 )).

fof(tlhfof37181,axiom,(
    cell76 != cell60 )).

fof(tlhfof37182,axiom,(
    cell76 != cell59 )).

fof(tlhfof37183,axiom,(
    cell76 != cell58 )).

fof(tlhfof37184,axiom,(
    cell76 != cell57 )).

fof(tlhfof37185,axiom,(
    cell76 != cell56 )).

fof(tlhfof37186,axiom,(
    cell76 != cell55 )).

fof(tlhfof37187,axiom,(
    cell76 != cell54 )).

fof(tlhfof37188,axiom,(
    cell76 != cell53 )).

fof(tlhfof37189,axiom,(
    cell76 != cell52 )).

fof(tlhfof37190,axiom,(
    cell76 != cell51 )).

fof(tlhfof37191,axiom,(
    cell76 != cell50 )).

fof(tlhfof37192,axiom,(
    cell76 != cell49 )).

fof(tlhfof37193,axiom,(
    cell76 != cell48 )).

fof(tlhfof37194,axiom,(
    cell76 != cell47 )).

fof(tlhfof37195,axiom,(
    cell76 != cell46 )).

fof(tlhfof37196,axiom,(
    cell76 != cell45 )).

fof(tlhfof37197,axiom,(
    cell76 != cell44 )).

fof(tlhfof37198,axiom,(
    cell76 != cell43 )).

fof(tlhfof37199,axiom,(
    cell76 != cell42 )).

fof(tlhfof37200,axiom,(
    cell76 != cell41 )).

fof(tlhfof37201,axiom,(
    cell76 != cell40 )).

fof(tlhfof37202,axiom,(
    cell76 != cell39 )).

fof(tlhfof37203,axiom,(
    cell76 != cell38 )).

fof(tlhfof37204,axiom,(
    cell76 != cell37 )).

fof(tlhfof37205,axiom,(
    cell76 != cell36 )).

fof(tlhfof37206,axiom,(
    cell76 != cell35 )).

fof(tlhfof37207,axiom,(
    cell76 != cell34 )).

fof(tlhfof37208,axiom,(
    cell76 != cell33 )).

fof(tlhfof37209,axiom,(
    cell76 != cell32 )).

fof(tlhfof37210,axiom,(
    cell76 != cell31 )).

fof(tlhfof37211,axiom,(
    cell76 != cell30 )).

fof(tlhfof37212,axiom,(
    cell76 != cell29 )).

fof(tlhfof37213,axiom,(
    cell76 != cell28 )).

fof(tlhfof37214,axiom,(
    cell76 != cell27 )).

fof(tlhfof37215,axiom,(
    cell76 != cell26 )).

fof(tlhfof37216,axiom,(
    cell76 != cell25 )).

fof(tlhfof37217,axiom,(
    cell76 != cell24 )).

fof(tlhfof37218,axiom,(
    cell76 != cell23 )).

fof(tlhfof37219,axiom,(
    cell76 != cell22 )).

fof(tlhfof37220,axiom,(
    cell76 != cell21 )).

fof(tlhfof37221,axiom,(
    cell76 != cell10 )).

fof(tlhfof37222,axiom,(
    cell76 != cell9 )).

fof(tlhfof37223,axiom,(
    cell76 != cell19 )).

fof(tlhfof37224,axiom,(
    cell76 != cell8 )).

fof(tlhfof37225,axiom,(
    cell76 != cell18 )).

fof(tlhfof37226,axiom,(
    cell76 != cell7 )).

fof(tlhfof37227,axiom,(
    cell76 != cell17 )).

fof(tlhfof37228,axiom,(
    cell76 != cell6 )).

fof(tlhfof37229,axiom,(
    cell76 != cell16 )).

fof(tlhfof37230,axiom,(
    cell76 != cell5 )).

fof(tlhfof37231,axiom,(
    cell76 != cell15 )).

fof(tlhfof37232,axiom,(
    cell76 != cell4 )).

fof(tlhfof37233,axiom,(
    cell76 != cell14 )).

fof(tlhfof37234,axiom,(
    cell76 != cell3 )).

fof(tlhfof37235,axiom,(
    cell76 != cell13 )).

fof(tlhfof37236,axiom,(
    cell76 != cell2 )).

fof(tlhfof37237,axiom,(
    cell76 != cell12 )).

fof(tlhfof37238,axiom,(
    cell76 != cell1 )).

fof(tlhfof37239,axiom,(
    cell76 != cell11 )).

fof(tlhfof37240,axiom,(
    cell76 != cell20 )).

fof(tlhfof37241,axiom,(
    cell75 != cell74 )).

fof(tlhfof37242,axiom,(
    cell75 != cell73 )).

fof(tlhfof37243,axiom,(
    cell75 != cell72 )).

fof(tlhfof37244,axiom,(
    cell75 != cell71 )).

fof(tlhfof37245,axiom,(
    cell75 != cell70 )).

fof(tlhfof37246,axiom,(
    cell75 != cell69 )).

fof(tlhfof37247,axiom,(
    cell75 != cell68 )).

fof(tlhfof37248,axiom,(
    cell75 != cell67 )).

fof(tlhfof37249,axiom,(
    cell75 != cell66 )).

fof(tlhfof37250,axiom,(
    cell75 != cell65 )).

fof(tlhfof37251,axiom,(
    cell75 != cell64 )).

fof(tlhfof37252,axiom,(
    cell75 != cell63 )).

fof(tlhfof37253,axiom,(
    cell75 != cell62 )).

fof(tlhfof37254,axiom,(
    cell75 != cell61 )).

fof(tlhfof37255,axiom,(
    cell75 != cell60 )).

fof(tlhfof37256,axiom,(
    cell75 != cell59 )).

fof(tlhfof37257,axiom,(
    cell75 != cell58 )).

fof(tlhfof37258,axiom,(
    cell75 != cell57 )).

fof(tlhfof37259,axiom,(
    cell75 != cell56 )).

fof(tlhfof37260,axiom,(
    cell75 != cell55 )).

fof(tlhfof37261,axiom,(
    cell75 != cell54 )).

fof(tlhfof37262,axiom,(
    cell75 != cell53 )).

fof(tlhfof37263,axiom,(
    cell75 != cell52 )).

fof(tlhfof37264,axiom,(
    cell75 != cell51 )).

fof(tlhfof37265,axiom,(
    cell75 != cell50 )).

fof(tlhfof37266,axiom,(
    cell75 != cell49 )).

fof(tlhfof37267,axiom,(
    cell75 != cell48 )).

fof(tlhfof37268,axiom,(
    cell75 != cell47 )).

fof(tlhfof37269,axiom,(
    cell75 != cell46 )).

fof(tlhfof37270,axiom,(
    cell75 != cell45 )).

fof(tlhfof37271,axiom,(
    cell75 != cell44 )).

fof(tlhfof37272,axiom,(
    cell75 != cell43 )).

fof(tlhfof37273,axiom,(
    cell75 != cell42 )).

fof(tlhfof37274,axiom,(
    cell75 != cell41 )).

fof(tlhfof37275,axiom,(
    cell75 != cell40 )).

fof(tlhfof37276,axiom,(
    cell75 != cell39 )).

fof(tlhfof37277,axiom,(
    cell75 != cell38 )).

fof(tlhfof37278,axiom,(
    cell75 != cell37 )).

fof(tlhfof37279,axiom,(
    cell75 != cell36 )).

fof(tlhfof37280,axiom,(
    cell75 != cell35 )).

fof(tlhfof37281,axiom,(
    cell75 != cell34 )).

fof(tlhfof37282,axiom,(
    cell75 != cell33 )).

fof(tlhfof37283,axiom,(
    cell75 != cell32 )).

fof(tlhfof37284,axiom,(
    cell75 != cell31 )).

fof(tlhfof37285,axiom,(
    cell75 != cell30 )).

fof(tlhfof37286,axiom,(
    cell75 != cell29 )).

fof(tlhfof37287,axiom,(
    cell75 != cell28 )).

fof(tlhfof37288,axiom,(
    cell75 != cell27 )).

fof(tlhfof37289,axiom,(
    cell75 != cell26 )).

fof(tlhfof37290,axiom,(
    cell75 != cell25 )).

fof(tlhfof37291,axiom,(
    cell75 != cell24 )).

fof(tlhfof37292,axiom,(
    cell75 != cell23 )).

fof(tlhfof37293,axiom,(
    cell75 != cell22 )).

fof(tlhfof37294,axiom,(
    cell75 != cell21 )).

fof(tlhfof37295,axiom,(
    cell75 != cell10 )).

fof(tlhfof37296,axiom,(
    cell75 != cell9 )).

fof(tlhfof37297,axiom,(
    cell75 != cell19 )).

fof(tlhfof37298,axiom,(
    cell75 != cell8 )).

fof(tlhfof37299,axiom,(
    cell75 != cell18 )).

fof(tlhfof37300,axiom,(
    cell75 != cell7 )).

fof(tlhfof37301,axiom,(
    cell75 != cell17 )).

fof(tlhfof37302,axiom,(
    cell75 != cell6 )).

fof(tlhfof37303,axiom,(
    cell75 != cell16 )).

fof(tlhfof37304,axiom,(
    cell75 != cell5 )).

fof(tlhfof37305,axiom,(
    cell75 != cell15 )).

fof(tlhfof37306,axiom,(
    cell75 != cell4 )).

fof(tlhfof37307,axiom,(
    cell75 != cell14 )).

fof(tlhfof37308,axiom,(
    cell75 != cell3 )).

fof(tlhfof37309,axiom,(
    cell75 != cell13 )).

fof(tlhfof37310,axiom,(
    cell75 != cell2 )).

fof(tlhfof37311,axiom,(
    cell75 != cell12 )).

fof(tlhfof37312,axiom,(
    cell75 != cell1 )).

fof(tlhfof37313,axiom,(
    cell75 != cell11 )).

fof(tlhfof37314,axiom,(
    cell75 != cell20 )).

fof(tlhfof37315,axiom,(
    cell74 != cell73 )).

fof(tlhfof37316,axiom,(
    cell74 != cell72 )).

fof(tlhfof37317,axiom,(
    cell74 != cell71 )).

fof(tlhfof37318,axiom,(
    cell74 != cell70 )).

fof(tlhfof37319,axiom,(
    cell74 != cell69 )).

fof(tlhfof37320,axiom,(
    cell74 != cell68 )).

fof(tlhfof37321,axiom,(
    cell74 != cell67 )).

fof(tlhfof37322,axiom,(
    cell74 != cell66 )).

fof(tlhfof37323,axiom,(
    cell74 != cell65 )).

fof(tlhfof37324,axiom,(
    cell74 != cell64 )).

fof(tlhfof37325,axiom,(
    cell74 != cell63 )).

fof(tlhfof37326,axiom,(
    cell74 != cell62 )).

fof(tlhfof37327,axiom,(
    cell74 != cell61 )).

fof(tlhfof37328,axiom,(
    cell74 != cell60 )).

fof(tlhfof37329,axiom,(
    cell74 != cell59 )).

fof(tlhfof37330,axiom,(
    cell74 != cell58 )).

fof(tlhfof37331,axiom,(
    cell74 != cell57 )).

fof(tlhfof37332,axiom,(
    cell74 != cell56 )).

fof(tlhfof37333,axiom,(
    cell74 != cell55 )).

fof(tlhfof37334,axiom,(
    cell74 != cell54 )).

fof(tlhfof37335,axiom,(
    cell74 != cell53 )).

fof(tlhfof37336,axiom,(
    cell74 != cell52 )).

fof(tlhfof37337,axiom,(
    cell74 != cell51 )).

fof(tlhfof37338,axiom,(
    cell74 != cell50 )).

fof(tlhfof37339,axiom,(
    cell74 != cell49 )).

fof(tlhfof37340,axiom,(
    cell74 != cell48 )).

fof(tlhfof37341,axiom,(
    cell74 != cell47 )).

fof(tlhfof37342,axiom,(
    cell74 != cell46 )).

fof(tlhfof37343,axiom,(
    cell74 != cell45 )).

fof(tlhfof37344,axiom,(
    cell74 != cell44 )).

fof(tlhfof37345,axiom,(
    cell74 != cell43 )).

fof(tlhfof37346,axiom,(
    cell74 != cell42 )).

fof(tlhfof37347,axiom,(
    cell74 != cell41 )).

fof(tlhfof37348,axiom,(
    cell74 != cell40 )).

fof(tlhfof37349,axiom,(
    cell74 != cell39 )).

fof(tlhfof37350,axiom,(
    cell74 != cell38 )).

fof(tlhfof37351,axiom,(
    cell74 != cell37 )).

fof(tlhfof37352,axiom,(
    cell74 != cell36 )).

fof(tlhfof37353,axiom,(
    cell74 != cell35 )).

fof(tlhfof37354,axiom,(
    cell74 != cell34 )).

fof(tlhfof37355,axiom,(
    cell74 != cell33 )).

fof(tlhfof37356,axiom,(
    cell74 != cell32 )).

fof(tlhfof37357,axiom,(
    cell74 != cell31 )).

fof(tlhfof37358,axiom,(
    cell74 != cell30 )).

fof(tlhfof37359,axiom,(
    cell74 != cell29 )).

fof(tlhfof37360,axiom,(
    cell74 != cell28 )).

fof(tlhfof37361,axiom,(
    cell74 != cell27 )).

fof(tlhfof37362,axiom,(
    cell74 != cell26 )).

fof(tlhfof37363,axiom,(
    cell74 != cell25 )).

fof(tlhfof37364,axiom,(
    cell74 != cell24 )).

fof(tlhfof37365,axiom,(
    cell74 != cell23 )).

fof(tlhfof37366,axiom,(
    cell74 != cell22 )).

fof(tlhfof37367,axiom,(
    cell74 != cell21 )).

fof(tlhfof37368,axiom,(
    cell74 != cell10 )).

fof(tlhfof37369,axiom,(
    cell74 != cell9 )).

fof(tlhfof37370,axiom,(
    cell74 != cell19 )).

fof(tlhfof37371,axiom,(
    cell74 != cell8 )).

fof(tlhfof37372,axiom,(
    cell74 != cell18 )).

fof(tlhfof37373,axiom,(
    cell74 != cell7 )).

fof(tlhfof37374,axiom,(
    cell74 != cell17 )).

fof(tlhfof37375,axiom,(
    cell74 != cell6 )).

fof(tlhfof37376,axiom,(
    cell74 != cell16 )).

fof(tlhfof37377,axiom,(
    cell74 != cell5 )).

fof(tlhfof37378,axiom,(
    cell74 != cell15 )).

fof(tlhfof37379,axiom,(
    cell74 != cell4 )).

fof(tlhfof37380,axiom,(
    cell74 != cell14 )).

fof(tlhfof37381,axiom,(
    cell74 != cell3 )).

fof(tlhfof37382,axiom,(
    cell74 != cell13 )).

fof(tlhfof37383,axiom,(
    cell74 != cell2 )).

fof(tlhfof37384,axiom,(
    cell74 != cell12 )).

fof(tlhfof37385,axiom,(
    cell74 != cell1 )).

fof(tlhfof37386,axiom,(
    cell74 != cell11 )).

fof(tlhfof37387,axiom,(
    cell74 != cell20 )).

fof(tlhfof37388,axiom,(
    cell73 != cell72 )).

fof(tlhfof37389,axiom,(
    cell73 != cell71 )).

fof(tlhfof37390,axiom,(
    cell73 != cell70 )).

fof(tlhfof37391,axiom,(
    cell73 != cell69 )).

fof(tlhfof37392,axiom,(
    cell73 != cell68 )).

fof(tlhfof37393,axiom,(
    cell73 != cell67 )).

fof(tlhfof37394,axiom,(
    cell73 != cell66 )).

fof(tlhfof37395,axiom,(
    cell73 != cell65 )).

fof(tlhfof37396,axiom,(
    cell73 != cell64 )).

fof(tlhfof37397,axiom,(
    cell73 != cell63 )).

fof(tlhfof37398,axiom,(
    cell73 != cell62 )).

fof(tlhfof37399,axiom,(
    cell73 != cell61 )).

fof(tlhfof37400,axiom,(
    cell73 != cell60 )).

fof(tlhfof37401,axiom,(
    cell73 != cell59 )).

fof(tlhfof37402,axiom,(
    cell73 != cell58 )).

fof(tlhfof37403,axiom,(
    cell73 != cell57 )).

fof(tlhfof37404,axiom,(
    cell73 != cell56 )).

fof(tlhfof37405,axiom,(
    cell73 != cell55 )).

fof(tlhfof37406,axiom,(
    cell73 != cell54 )).

fof(tlhfof37407,axiom,(
    cell73 != cell53 )).

fof(tlhfof37408,axiom,(
    cell73 != cell52 )).

fof(tlhfof37409,axiom,(
    cell73 != cell51 )).

fof(tlhfof37410,axiom,(
    cell73 != cell50 )).

fof(tlhfof37411,axiom,(
    cell73 != cell49 )).

fof(tlhfof37412,axiom,(
    cell73 != cell48 )).

fof(tlhfof37413,axiom,(
    cell73 != cell47 )).

fof(tlhfof37414,axiom,(
    cell73 != cell46 )).

fof(tlhfof37415,axiom,(
    cell73 != cell45 )).

fof(tlhfof37416,axiom,(
    cell73 != cell44 )).

fof(tlhfof37417,axiom,(
    cell73 != cell43 )).

fof(tlhfof37418,axiom,(
    cell73 != cell42 )).

fof(tlhfof37419,axiom,(
    cell73 != cell41 )).

fof(tlhfof37420,axiom,(
    cell73 != cell40 )).

fof(tlhfof37421,axiom,(
    cell73 != cell39 )).

fof(tlhfof37422,axiom,(
    cell73 != cell38 )).

fof(tlhfof37423,axiom,(
    cell73 != cell37 )).

fof(tlhfof37424,axiom,(
    cell73 != cell36 )).

fof(tlhfof37425,axiom,(
    cell73 != cell35 )).

fof(tlhfof37426,axiom,(
    cell73 != cell34 )).

fof(tlhfof37427,axiom,(
    cell73 != cell33 )).

fof(tlhfof37428,axiom,(
    cell73 != cell32 )).

fof(tlhfof37429,axiom,(
    cell73 != cell31 )).

fof(tlhfof37430,axiom,(
    cell73 != cell30 )).

fof(tlhfof37431,axiom,(
    cell73 != cell29 )).

fof(tlhfof37432,axiom,(
    cell73 != cell28 )).

fof(tlhfof37433,axiom,(
    cell73 != cell27 )).

fof(tlhfof37434,axiom,(
    cell73 != cell26 )).

fof(tlhfof37435,axiom,(
    cell73 != cell25 )).

fof(tlhfof37436,axiom,(
    cell73 != cell24 )).

fof(tlhfof37437,axiom,(
    cell73 != cell23 )).

fof(tlhfof37438,axiom,(
    cell73 != cell22 )).

fof(tlhfof37439,axiom,(
    cell73 != cell21 )).

fof(tlhfof37440,axiom,(
    cell73 != cell10 )).

fof(tlhfof37441,axiom,(
    cell73 != cell9 )).

fof(tlhfof37442,axiom,(
    cell73 != cell19 )).

fof(tlhfof37443,axiom,(
    cell73 != cell8 )).

fof(tlhfof37444,axiom,(
    cell73 != cell18 )).

fof(tlhfof37445,axiom,(
    cell73 != cell7 )).

fof(tlhfof37446,axiom,(
    cell73 != cell17 )).

fof(tlhfof37447,axiom,(
    cell73 != cell6 )).

fof(tlhfof37448,axiom,(
    cell73 != cell16 )).

fof(tlhfof37449,axiom,(
    cell73 != cell5 )).

fof(tlhfof37450,axiom,(
    cell73 != cell15 )).

fof(tlhfof37451,axiom,(
    cell73 != cell4 )).

fof(tlhfof37452,axiom,(
    cell73 != cell14 )).

fof(tlhfof37453,axiom,(
    cell73 != cell3 )).

fof(tlhfof37454,axiom,(
    cell73 != cell13 )).

fof(tlhfof37455,axiom,(
    cell73 != cell2 )).

fof(tlhfof37456,axiom,(
    cell73 != cell12 )).

fof(tlhfof37457,axiom,(
    cell73 != cell1 )).

fof(tlhfof37458,axiom,(
    cell73 != cell11 )).

fof(tlhfof37459,axiom,(
    cell73 != cell20 )).

fof(tlhfof37460,axiom,(
    cell72 != cell71 )).

fof(tlhfof37461,axiom,(
    cell72 != cell70 )).

fof(tlhfof37462,axiom,(
    cell72 != cell69 )).

fof(tlhfof37463,axiom,(
    cell72 != cell68 )).

fof(tlhfof37464,axiom,(
    cell72 != cell67 )).

fof(tlhfof37465,axiom,(
    cell72 != cell66 )).

fof(tlhfof37466,axiom,(
    cell72 != cell65 )).

fof(tlhfof37467,axiom,(
    cell72 != cell64 )).

fof(tlhfof37468,axiom,(
    cell72 != cell63 )).

fof(tlhfof37469,axiom,(
    cell72 != cell62 )).

fof(tlhfof37470,axiom,(
    cell72 != cell61 )).

fof(tlhfof37471,axiom,(
    cell72 != cell60 )).

fof(tlhfof37472,axiom,(
    cell72 != cell59 )).

fof(tlhfof37473,axiom,(
    cell72 != cell58 )).

fof(tlhfof37474,axiom,(
    cell72 != cell57 )).

fof(tlhfof37475,axiom,(
    cell72 != cell56 )).

fof(tlhfof37476,axiom,(
    cell72 != cell55 )).

fof(tlhfof37477,axiom,(
    cell72 != cell54 )).

fof(tlhfof37478,axiom,(
    cell72 != cell53 )).

fof(tlhfof37479,axiom,(
    cell72 != cell52 )).

fof(tlhfof37480,axiom,(
    cell72 != cell51 )).

fof(tlhfof37481,axiom,(
    cell72 != cell50 )).

fof(tlhfof37482,axiom,(
    cell72 != cell49 )).

fof(tlhfof37483,axiom,(
    cell72 != cell48 )).

fof(tlhfof37484,axiom,(
    cell72 != cell47 )).

fof(tlhfof37485,axiom,(
    cell72 != cell46 )).

fof(tlhfof37486,axiom,(
    cell72 != cell45 )).

fof(tlhfof37487,axiom,(
    cell72 != cell44 )).

fof(tlhfof37488,axiom,(
    cell72 != cell43 )).

fof(tlhfof37489,axiom,(
    cell72 != cell42 )).

fof(tlhfof37490,axiom,(
    cell72 != cell41 )).

fof(tlhfof37491,axiom,(
    cell72 != cell40 )).

fof(tlhfof37492,axiom,(
    cell72 != cell39 )).

fof(tlhfof37493,axiom,(
    cell72 != cell38 )).

fof(tlhfof37494,axiom,(
    cell72 != cell37 )).

fof(tlhfof37495,axiom,(
    cell72 != cell36 )).

fof(tlhfof37496,axiom,(
    cell72 != cell35 )).

fof(tlhfof37497,axiom,(
    cell72 != cell34 )).

fof(tlhfof37498,axiom,(
    cell72 != cell33 )).

fof(tlhfof37499,axiom,(
    cell72 != cell32 )).

fof(tlhfof37500,axiom,(
    cell72 != cell31 )).

fof(tlhfof37501,axiom,(
    cell72 != cell30 )).

fof(tlhfof37502,axiom,(
    cell72 != cell29 )).

fof(tlhfof37503,axiom,(
    cell72 != cell28 )).

fof(tlhfof37504,axiom,(
    cell72 != cell27 )).

fof(tlhfof37505,axiom,(
    cell72 != cell26 )).

fof(tlhfof37506,axiom,(
    cell72 != cell25 )).

fof(tlhfof37507,axiom,(
    cell72 != cell24 )).

fof(tlhfof37508,axiom,(
    cell72 != cell23 )).

fof(tlhfof37509,axiom,(
    cell72 != cell22 )).

fof(tlhfof37510,axiom,(
    cell72 != cell21 )).

fof(tlhfof37511,axiom,(
    cell72 != cell10 )).

fof(tlhfof37512,axiom,(
    cell72 != cell9 )).

fof(tlhfof37513,axiom,(
    cell72 != cell19 )).

fof(tlhfof37514,axiom,(
    cell72 != cell8 )).

fof(tlhfof37515,axiom,(
    cell72 != cell18 )).

fof(tlhfof37516,axiom,(
    cell72 != cell7 )).

fof(tlhfof37517,axiom,(
    cell72 != cell17 )).

fof(tlhfof37518,axiom,(
    cell72 != cell6 )).

fof(tlhfof37519,axiom,(
    cell72 != cell16 )).

fof(tlhfof37520,axiom,(
    cell72 != cell5 )).

fof(tlhfof37521,axiom,(
    cell72 != cell15 )).

fof(tlhfof37522,axiom,(
    cell72 != cell4 )).

fof(tlhfof37523,axiom,(
    cell72 != cell14 )).

fof(tlhfof37524,axiom,(
    cell72 != cell3 )).

fof(tlhfof37525,axiom,(
    cell72 != cell13 )).

fof(tlhfof37526,axiom,(
    cell72 != cell2 )).

fof(tlhfof37527,axiom,(
    cell72 != cell12 )).

fof(tlhfof37528,axiom,(
    cell72 != cell1 )).

fof(tlhfof37529,axiom,(
    cell72 != cell11 )).

fof(tlhfof37530,axiom,(
    cell72 != cell20 )).

fof(tlhfof37531,axiom,(
    cell71 != cell70 )).

fof(tlhfof37532,axiom,(
    cell71 != cell69 )).

fof(tlhfof37533,axiom,(
    cell71 != cell68 )).

fof(tlhfof37534,axiom,(
    cell71 != cell67 )).

fof(tlhfof37535,axiom,(
    cell71 != cell66 )).

fof(tlhfof37536,axiom,(
    cell71 != cell65 )).

fof(tlhfof37537,axiom,(
    cell71 != cell64 )).

fof(tlhfof37538,axiom,(
    cell71 != cell63 )).

fof(tlhfof37539,axiom,(
    cell71 != cell62 )).

fof(tlhfof37540,axiom,(
    cell71 != cell61 )).

fof(tlhfof37541,axiom,(
    cell71 != cell60 )).

fof(tlhfof37542,axiom,(
    cell71 != cell59 )).

fof(tlhfof37543,axiom,(
    cell71 != cell58 )).

fof(tlhfof37544,axiom,(
    cell71 != cell57 )).

fof(tlhfof37545,axiom,(
    cell71 != cell56 )).

fof(tlhfof37546,axiom,(
    cell71 != cell55 )).

fof(tlhfof37547,axiom,(
    cell71 != cell54 )).

fof(tlhfof37548,axiom,(
    cell71 != cell53 )).

fof(tlhfof37549,axiom,(
    cell71 != cell52 )).

fof(tlhfof37550,axiom,(
    cell71 != cell51 )).

fof(tlhfof37551,axiom,(
    cell71 != cell50 )).

fof(tlhfof37552,axiom,(
    cell71 != cell49 )).

fof(tlhfof37553,axiom,(
    cell71 != cell48 )).

fof(tlhfof37554,axiom,(
    cell71 != cell47 )).

fof(tlhfof37555,axiom,(
    cell71 != cell46 )).

fof(tlhfof37556,axiom,(
    cell71 != cell45 )).

fof(tlhfof37557,axiom,(
    cell71 != cell44 )).

fof(tlhfof37558,axiom,(
    cell71 != cell43 )).

fof(tlhfof37559,axiom,(
    cell71 != cell42 )).

fof(tlhfof37560,axiom,(
    cell71 != cell41 )).

fof(tlhfof37561,axiom,(
    cell71 != cell40 )).

fof(tlhfof37562,axiom,(
    cell71 != cell39 )).

fof(tlhfof37563,axiom,(
    cell71 != cell38 )).

fof(tlhfof37564,axiom,(
    cell71 != cell37 )).

fof(tlhfof37565,axiom,(
    cell71 != cell36 )).

fof(tlhfof37566,axiom,(
    cell71 != cell35 )).

fof(tlhfof37567,axiom,(
    cell71 != cell34 )).

fof(tlhfof37568,axiom,(
    cell71 != cell33 )).

fof(tlhfof37569,axiom,(
    cell71 != cell32 )).

fof(tlhfof37570,axiom,(
    cell71 != cell31 )).

fof(tlhfof37571,axiom,(
    cell71 != cell30 )).

fof(tlhfof37572,axiom,(
    cell71 != cell29 )).

fof(tlhfof37573,axiom,(
    cell71 != cell28 )).

fof(tlhfof37574,axiom,(
    cell71 != cell27 )).

fof(tlhfof37575,axiom,(
    cell71 != cell26 )).

fof(tlhfof37576,axiom,(
    cell71 != cell25 )).

fof(tlhfof37577,axiom,(
    cell71 != cell24 )).

fof(tlhfof37578,axiom,(
    cell71 != cell23 )).

fof(tlhfof37579,axiom,(
    cell71 != cell22 )).

fof(tlhfof37580,axiom,(
    cell71 != cell21 )).

fof(tlhfof37581,axiom,(
    cell71 != cell10 )).

fof(tlhfof37582,axiom,(
    cell71 != cell9 )).

fof(tlhfof37583,axiom,(
    cell71 != cell19 )).

fof(tlhfof37584,axiom,(
    cell71 != cell8 )).

fof(tlhfof37585,axiom,(
    cell71 != cell18 )).

fof(tlhfof37586,axiom,(
    cell71 != cell7 )).

fof(tlhfof37587,axiom,(
    cell71 != cell17 )).

fof(tlhfof37588,axiom,(
    cell71 != cell6 )).

fof(tlhfof37589,axiom,(
    cell71 != cell16 )).

fof(tlhfof37590,axiom,(
    cell71 != cell5 )).

fof(tlhfof37591,axiom,(
    cell71 != cell15 )).

fof(tlhfof37592,axiom,(
    cell71 != cell4 )).

fof(tlhfof37593,axiom,(
    cell71 != cell14 )).

fof(tlhfof37594,axiom,(
    cell71 != cell3 )).

fof(tlhfof37595,axiom,(
    cell71 != cell13 )).

fof(tlhfof37596,axiom,(
    cell71 != cell2 )).

fof(tlhfof37597,axiom,(
    cell71 != cell12 )).

fof(tlhfof37598,axiom,(
    cell71 != cell1 )).

fof(tlhfof37599,axiom,(
    cell71 != cell11 )).

fof(tlhfof37600,axiom,(
    cell71 != cell20 )).

fof(tlhfof37601,axiom,(
    cell70 != cell69 )).

fof(tlhfof37602,axiom,(
    cell70 != cell68 )).

fof(tlhfof37603,axiom,(
    cell70 != cell67 )).

fof(tlhfof37604,axiom,(
    cell70 != cell66 )).

fof(tlhfof37605,axiom,(
    cell70 != cell65 )).

fof(tlhfof37606,axiom,(
    cell70 != cell64 )).

fof(tlhfof37607,axiom,(
    cell70 != cell63 )).

fof(tlhfof37608,axiom,(
    cell70 != cell62 )).

fof(tlhfof37609,axiom,(
    cell70 != cell61 )).

fof(tlhfof37610,axiom,(
    cell70 != cell60 )).

fof(tlhfof37611,axiom,(
    cell70 != cell59 )).

fof(tlhfof37612,axiom,(
    cell70 != cell58 )).

fof(tlhfof37613,axiom,(
    cell70 != cell57 )).

fof(tlhfof37614,axiom,(
    cell70 != cell56 )).

fof(tlhfof37615,axiom,(
    cell70 != cell55 )).

fof(tlhfof37616,axiom,(
    cell70 != cell54 )).

fof(tlhfof37617,axiom,(
    cell70 != cell53 )).

fof(tlhfof37618,axiom,(
    cell70 != cell52 )).

fof(tlhfof37619,axiom,(
    cell70 != cell51 )).

fof(tlhfof37620,axiom,(
    cell70 != cell50 )).

fof(tlhfof37621,axiom,(
    cell70 != cell49 )).

fof(tlhfof37622,axiom,(
    cell70 != cell48 )).

fof(tlhfof37623,axiom,(
    cell70 != cell47 )).

fof(tlhfof37624,axiom,(
    cell70 != cell46 )).

fof(tlhfof37625,axiom,(
    cell70 != cell45 )).

fof(tlhfof37626,axiom,(
    cell70 != cell44 )).

fof(tlhfof37627,axiom,(
    cell70 != cell43 )).

fof(tlhfof37628,axiom,(
    cell70 != cell42 )).

fof(tlhfof37629,axiom,(
    cell70 != cell41 )).

fof(tlhfof37630,axiom,(
    cell70 != cell40 )).

fof(tlhfof37631,axiom,(
    cell70 != cell39 )).

fof(tlhfof37632,axiom,(
    cell70 != cell38 )).

fof(tlhfof37633,axiom,(
    cell70 != cell37 )).

fof(tlhfof37634,axiom,(
    cell70 != cell36 )).

fof(tlhfof37635,axiom,(
    cell70 != cell35 )).

fof(tlhfof37636,axiom,(
    cell70 != cell34 )).

fof(tlhfof37637,axiom,(
    cell70 != cell33 )).

fof(tlhfof37638,axiom,(
    cell70 != cell32 )).

fof(tlhfof37639,axiom,(
    cell70 != cell31 )).

fof(tlhfof37640,axiom,(
    cell70 != cell30 )).

fof(tlhfof37641,axiom,(
    cell70 != cell29 )).

fof(tlhfof37642,axiom,(
    cell70 != cell28 )).

fof(tlhfof37643,axiom,(
    cell70 != cell27 )).

fof(tlhfof37644,axiom,(
    cell70 != cell26 )).

fof(tlhfof37645,axiom,(
    cell70 != cell25 )).

fof(tlhfof37646,axiom,(
    cell70 != cell24 )).

fof(tlhfof37647,axiom,(
    cell70 != cell23 )).

fof(tlhfof37648,axiom,(
    cell70 != cell22 )).

fof(tlhfof37649,axiom,(
    cell70 != cell21 )).

fof(tlhfof37650,axiom,(
    cell70 != cell10 )).

fof(tlhfof37651,axiom,(
    cell70 != cell9 )).

fof(tlhfof37652,axiom,(
    cell70 != cell19 )).

fof(tlhfof37653,axiom,(
    cell70 != cell8 )).

fof(tlhfof37654,axiom,(
    cell70 != cell18 )).

fof(tlhfof37655,axiom,(
    cell70 != cell7 )).

fof(tlhfof37656,axiom,(
    cell70 != cell17 )).

fof(tlhfof37657,axiom,(
    cell70 != cell6 )).

fof(tlhfof37658,axiom,(
    cell70 != cell16 )).

fof(tlhfof37659,axiom,(
    cell70 != cell5 )).

fof(tlhfof37660,axiom,(
    cell70 != cell15 )).

fof(tlhfof37661,axiom,(
    cell70 != cell4 )).

fof(tlhfof37662,axiom,(
    cell70 != cell14 )).

fof(tlhfof37663,axiom,(
    cell70 != cell3 )).

fof(tlhfof37664,axiom,(
    cell70 != cell13 )).

fof(tlhfof37665,axiom,(
    cell70 != cell2 )).

fof(tlhfof37666,axiom,(
    cell70 != cell12 )).

fof(tlhfof37667,axiom,(
    cell70 != cell1 )).

fof(tlhfof37668,axiom,(
    cell70 != cell11 )).

fof(tlhfof37669,axiom,(
    cell70 != cell20 )).

fof(tlhfof37670,axiom,(
    cell69 != cell68 )).

fof(tlhfof37671,axiom,(
    cell69 != cell67 )).

fof(tlhfof37672,axiom,(
    cell69 != cell66 )).

fof(tlhfof37673,axiom,(
    cell69 != cell65 )).

fof(tlhfof37674,axiom,(
    cell69 != cell64 )).

fof(tlhfof37675,axiom,(
    cell69 != cell63 )).

fof(tlhfof37676,axiom,(
    cell69 != cell62 )).

fof(tlhfof37677,axiom,(
    cell69 != cell61 )).

fof(tlhfof37678,axiom,(
    cell69 != cell60 )).

fof(tlhfof37679,axiom,(
    cell69 != cell59 )).

fof(tlhfof37680,axiom,(
    cell69 != cell58 )).

fof(tlhfof37681,axiom,(
    cell69 != cell57 )).

fof(tlhfof37682,axiom,(
    cell69 != cell56 )).

fof(tlhfof37683,axiom,(
    cell69 != cell55 )).

fof(tlhfof37684,axiom,(
    cell69 != cell54 )).

fof(tlhfof37685,axiom,(
    cell69 != cell53 )).

fof(tlhfof37686,axiom,(
    cell69 != cell52 )).

fof(tlhfof37687,axiom,(
    cell69 != cell51 )).

fof(tlhfof37688,axiom,(
    cell69 != cell50 )).

fof(tlhfof37689,axiom,(
    cell69 != cell49 )).

fof(tlhfof37690,axiom,(
    cell69 != cell48 )).

fof(tlhfof37691,axiom,(
    cell69 != cell47 )).

fof(tlhfof37692,axiom,(
    cell69 != cell46 )).

fof(tlhfof37693,axiom,(
    cell69 != cell45 )).

fof(tlhfof37694,axiom,(
    cell69 != cell44 )).

fof(tlhfof37695,axiom,(
    cell69 != cell43 )).

fof(tlhfof37696,axiom,(
    cell69 != cell42 )).

fof(tlhfof37697,axiom,(
    cell69 != cell41 )).

fof(tlhfof37698,axiom,(
    cell69 != cell40 )).

fof(tlhfof37699,axiom,(
    cell69 != cell39 )).

fof(tlhfof37700,axiom,(
    cell69 != cell38 )).

fof(tlhfof37701,axiom,(
    cell69 != cell37 )).

fof(tlhfof37702,axiom,(
    cell69 != cell36 )).

fof(tlhfof37703,axiom,(
    cell69 != cell35 )).

fof(tlhfof37704,axiom,(
    cell69 != cell34 )).

fof(tlhfof37705,axiom,(
    cell69 != cell33 )).

fof(tlhfof37706,axiom,(
    cell69 != cell32 )).

fof(tlhfof37707,axiom,(
    cell69 != cell31 )).

fof(tlhfof37708,axiom,(
    cell69 != cell30 )).

fof(tlhfof37709,axiom,(
    cell69 != cell29 )).

fof(tlhfof37710,axiom,(
    cell69 != cell28 )).

fof(tlhfof37711,axiom,(
    cell69 != cell27 )).

fof(tlhfof37712,axiom,(
    cell69 != cell26 )).

fof(tlhfof37713,axiom,(
    cell69 != cell25 )).

fof(tlhfof37714,axiom,(
    cell69 != cell24 )).

fof(tlhfof37715,axiom,(
    cell69 != cell23 )).

fof(tlhfof37716,axiom,(
    cell69 != cell22 )).

fof(tlhfof37717,axiom,(
    cell69 != cell21 )).

fof(tlhfof37718,axiom,(
    cell69 != cell10 )).

fof(tlhfof37719,axiom,(
    cell69 != cell9 )).

fof(tlhfof37720,axiom,(
    cell69 != cell19 )).

fof(tlhfof37721,axiom,(
    cell69 != cell8 )).

fof(tlhfof37722,axiom,(
    cell69 != cell18 )).

fof(tlhfof37723,axiom,(
    cell69 != cell7 )).

fof(tlhfof37724,axiom,(
    cell69 != cell17 )).

fof(tlhfof37725,axiom,(
    cell69 != cell6 )).

fof(tlhfof37726,axiom,(
    cell69 != cell16 )).

fof(tlhfof37727,axiom,(
    cell69 != cell5 )).

fof(tlhfof37728,axiom,(
    cell69 != cell15 )).

fof(tlhfof37729,axiom,(
    cell69 != cell4 )).

fof(tlhfof37730,axiom,(
    cell69 != cell14 )).

fof(tlhfof37731,axiom,(
    cell69 != cell3 )).

fof(tlhfof37732,axiom,(
    cell69 != cell13 )).

fof(tlhfof37733,axiom,(
    cell69 != cell2 )).

fof(tlhfof37734,axiom,(
    cell69 != cell12 )).

fof(tlhfof37735,axiom,(
    cell69 != cell1 )).

fof(tlhfof37736,axiom,(
    cell69 != cell11 )).

fof(tlhfof37737,axiom,(
    cell69 != cell20 )).

fof(tlhfof37738,axiom,(
    cell68 != cell67 )).

fof(tlhfof37739,axiom,(
    cell68 != cell66 )).

fof(tlhfof37740,axiom,(
    cell68 != cell65 )).

fof(tlhfof37741,axiom,(
    cell68 != cell64 )).

fof(tlhfof37742,axiom,(
    cell68 != cell63 )).

fof(tlhfof37743,axiom,(
    cell68 != cell62 )).

fof(tlhfof37744,axiom,(
    cell68 != cell61 )).

fof(tlhfof37745,axiom,(
    cell68 != cell60 )).

fof(tlhfof37746,axiom,(
    cell68 != cell59 )).

fof(tlhfof37747,axiom,(
    cell68 != cell58 )).

fof(tlhfof37748,axiom,(
    cell68 != cell57 )).

fof(tlhfof37749,axiom,(
    cell68 != cell56 )).

fof(tlhfof37750,axiom,(
    cell68 != cell55 )).

fof(tlhfof37751,axiom,(
    cell68 != cell54 )).

fof(tlhfof37752,axiom,(
    cell68 != cell53 )).

fof(tlhfof37753,axiom,(
    cell68 != cell52 )).

fof(tlhfof37754,axiom,(
    cell68 != cell51 )).

fof(tlhfof37755,axiom,(
    cell68 != cell50 )).

fof(tlhfof37756,axiom,(
    cell68 != cell49 )).

fof(tlhfof37757,axiom,(
    cell68 != cell48 )).

fof(tlhfof37758,axiom,(
    cell68 != cell47 )).

fof(tlhfof37759,axiom,(
    cell68 != cell46 )).

fof(tlhfof37760,axiom,(
    cell68 != cell45 )).

fof(tlhfof37761,axiom,(
    cell68 != cell44 )).

fof(tlhfof37762,axiom,(
    cell68 != cell43 )).

fof(tlhfof37763,axiom,(
    cell68 != cell42 )).

fof(tlhfof37764,axiom,(
    cell68 != cell41 )).

fof(tlhfof37765,axiom,(
    cell68 != cell40 )).

fof(tlhfof37766,axiom,(
    cell68 != cell39 )).

fof(tlhfof37767,axiom,(
    cell68 != cell38 )).

fof(tlhfof37768,axiom,(
    cell68 != cell37 )).

fof(tlhfof37769,axiom,(
    cell68 != cell36 )).

fof(tlhfof37770,axiom,(
    cell68 != cell35 )).

fof(tlhfof37771,axiom,(
    cell68 != cell34 )).

fof(tlhfof37772,axiom,(
    cell68 != cell33 )).

fof(tlhfof37773,axiom,(
    cell68 != cell32 )).

fof(tlhfof37774,axiom,(
    cell68 != cell31 )).

fof(tlhfof37775,axiom,(
    cell68 != cell30 )).

fof(tlhfof37776,axiom,(
    cell68 != cell29 )).

fof(tlhfof37777,axiom,(
    cell68 != cell28 )).

fof(tlhfof37778,axiom,(
    cell68 != cell27 )).

fof(tlhfof37779,axiom,(
    cell68 != cell26 )).

fof(tlhfof37780,axiom,(
    cell68 != cell25 )).

fof(tlhfof37781,axiom,(
    cell68 != cell24 )).

fof(tlhfof37782,axiom,(
    cell68 != cell23 )).

fof(tlhfof37783,axiom,(
    cell68 != cell22 )).

fof(tlhfof37784,axiom,(
    cell68 != cell21 )).

fof(tlhfof37785,axiom,(
    cell68 != cell10 )).

fof(tlhfof37786,axiom,(
    cell68 != cell9 )).

fof(tlhfof37787,axiom,(
    cell68 != cell19 )).

fof(tlhfof37788,axiom,(
    cell68 != cell8 )).

fof(tlhfof37789,axiom,(
    cell68 != cell18 )).

fof(tlhfof37790,axiom,(
    cell68 != cell7 )).

fof(tlhfof37791,axiom,(
    cell68 != cell17 )).

fof(tlhfof37792,axiom,(
    cell68 != cell6 )).

fof(tlhfof37793,axiom,(
    cell68 != cell16 )).

fof(tlhfof37794,axiom,(
    cell68 != cell5 )).

fof(tlhfof37795,axiom,(
    cell68 != cell15 )).

fof(tlhfof37796,axiom,(
    cell68 != cell4 )).

fof(tlhfof37797,axiom,(
    cell68 != cell14 )).

fof(tlhfof37798,axiom,(
    cell68 != cell3 )).

fof(tlhfof37799,axiom,(
    cell68 != cell13 )).

fof(tlhfof37800,axiom,(
    cell68 != cell2 )).

fof(tlhfof37801,axiom,(
    cell68 != cell12 )).

fof(tlhfof37802,axiom,(
    cell68 != cell1 )).

fof(tlhfof37803,axiom,(
    cell68 != cell11 )).

fof(tlhfof37804,axiom,(
    cell68 != cell20 )).

fof(tlhfof37805,axiom,(
    cell67 != cell66 )).

fof(tlhfof37806,axiom,(
    cell67 != cell65 )).

fof(tlhfof37807,axiom,(
    cell67 != cell64 )).

fof(tlhfof37808,axiom,(
    cell67 != cell63 )).

fof(tlhfof37809,axiom,(
    cell67 != cell62 )).

fof(tlhfof37810,axiom,(
    cell67 != cell61 )).

fof(tlhfof37811,axiom,(
    cell67 != cell60 )).

fof(tlhfof37812,axiom,(
    cell67 != cell59 )).

fof(tlhfof37813,axiom,(
    cell67 != cell58 )).

fof(tlhfof37814,axiom,(
    cell67 != cell57 )).

fof(tlhfof37815,axiom,(
    cell67 != cell56 )).

fof(tlhfof37816,axiom,(
    cell67 != cell55 )).

fof(tlhfof37817,axiom,(
    cell67 != cell54 )).

fof(tlhfof37818,axiom,(
    cell67 != cell53 )).

fof(tlhfof37819,axiom,(
    cell67 != cell52 )).

fof(tlhfof37820,axiom,(
    cell67 != cell51 )).

fof(tlhfof37821,axiom,(
    cell67 != cell50 )).

fof(tlhfof37822,axiom,(
    cell67 != cell49 )).

fof(tlhfof37823,axiom,(
    cell67 != cell48 )).

fof(tlhfof37824,axiom,(
    cell67 != cell47 )).

fof(tlhfof37825,axiom,(
    cell67 != cell46 )).

fof(tlhfof37826,axiom,(
    cell67 != cell45 )).

fof(tlhfof37827,axiom,(
    cell67 != cell44 )).

fof(tlhfof37828,axiom,(
    cell67 != cell43 )).

fof(tlhfof37829,axiom,(
    cell67 != cell42 )).

fof(tlhfof37830,axiom,(
    cell67 != cell41 )).

fof(tlhfof37831,axiom,(
    cell67 != cell40 )).

fof(tlhfof37832,axiom,(
    cell67 != cell39 )).

fof(tlhfof37833,axiom,(
    cell67 != cell38 )).

fof(tlhfof37834,axiom,(
    cell67 != cell37 )).

fof(tlhfof37835,axiom,(
    cell67 != cell36 )).

fof(tlhfof37836,axiom,(
    cell67 != cell35 )).

fof(tlhfof37837,axiom,(
    cell67 != cell34 )).

fof(tlhfof37838,axiom,(
    cell67 != cell33 )).

fof(tlhfof37839,axiom,(
    cell67 != cell32 )).

fof(tlhfof37840,axiom,(
    cell67 != cell31 )).

fof(tlhfof37841,axiom,(
    cell67 != cell30 )).

fof(tlhfof37842,axiom,(
    cell67 != cell29 )).

fof(tlhfof37843,axiom,(
    cell67 != cell28 )).

fof(tlhfof37844,axiom,(
    cell67 != cell27 )).

fof(tlhfof37845,axiom,(
    cell67 != cell26 )).

fof(tlhfof37846,axiom,(
    cell67 != cell25 )).

fof(tlhfof37847,axiom,(
    cell67 != cell24 )).

fof(tlhfof37848,axiom,(
    cell67 != cell23 )).

fof(tlhfof37849,axiom,(
    cell67 != cell22 )).

fof(tlhfof37850,axiom,(
    cell67 != cell21 )).

fof(tlhfof37851,axiom,(
    cell67 != cell10 )).

fof(tlhfof37852,axiom,(
    cell67 != cell9 )).

fof(tlhfof37853,axiom,(
    cell67 != cell19 )).

fof(tlhfof37854,axiom,(
    cell67 != cell8 )).

fof(tlhfof37855,axiom,(
    cell67 != cell18 )).

fof(tlhfof37856,axiom,(
    cell67 != cell7 )).

fof(tlhfof37857,axiom,(
    cell67 != cell17 )).

fof(tlhfof37858,axiom,(
    cell67 != cell6 )).

fof(tlhfof37859,axiom,(
    cell67 != cell16 )).

fof(tlhfof37860,axiom,(
    cell67 != cell5 )).

fof(tlhfof37861,axiom,(
    cell67 != cell15 )).

fof(tlhfof37862,axiom,(
    cell67 != cell4 )).

fof(tlhfof37863,axiom,(
    cell67 != cell14 )).

fof(tlhfof37864,axiom,(
    cell67 != cell3 )).

fof(tlhfof37865,axiom,(
    cell67 != cell13 )).

fof(tlhfof37866,axiom,(
    cell67 != cell2 )).

fof(tlhfof37867,axiom,(
    cell67 != cell12 )).

fof(tlhfof37868,axiom,(
    cell67 != cell1 )).

fof(tlhfof37869,axiom,(
    cell67 != cell11 )).

fof(tlhfof37870,axiom,(
    cell67 != cell20 )).

fof(tlhfof37871,axiom,(
    cell66 != cell65 )).

fof(tlhfof37872,axiom,(
    cell66 != cell64 )).

fof(tlhfof37873,axiom,(
    cell66 != cell63 )).

fof(tlhfof37874,axiom,(
    cell66 != cell62 )).

fof(tlhfof37875,axiom,(
    cell66 != cell61 )).

fof(tlhfof37876,axiom,(
    cell66 != cell60 )).

fof(tlhfof37877,axiom,(
    cell66 != cell59 )).

fof(tlhfof37878,axiom,(
    cell66 != cell58 )).

fof(tlhfof37879,axiom,(
    cell66 != cell57 )).

fof(tlhfof37880,axiom,(
    cell66 != cell56 )).

fof(tlhfof37881,axiom,(
    cell66 != cell55 )).

fof(tlhfof37882,axiom,(
    cell66 != cell54 )).

fof(tlhfof37883,axiom,(
    cell66 != cell53 )).

fof(tlhfof37884,axiom,(
    cell66 != cell52 )).

fof(tlhfof37885,axiom,(
    cell66 != cell51 )).

fof(tlhfof37886,axiom,(
    cell66 != cell50 )).

fof(tlhfof37887,axiom,(
    cell66 != cell49 )).

fof(tlhfof37888,axiom,(
    cell66 != cell48 )).

fof(tlhfof37889,axiom,(
    cell66 != cell47 )).

fof(tlhfof37890,axiom,(
    cell66 != cell46 )).

fof(tlhfof37891,axiom,(
    cell66 != cell45 )).

fof(tlhfof37892,axiom,(
    cell66 != cell44 )).

fof(tlhfof37893,axiom,(
    cell66 != cell43 )).

fof(tlhfof37894,axiom,(
    cell66 != cell42 )).

fof(tlhfof37895,axiom,(
    cell66 != cell41 )).

fof(tlhfof37896,axiom,(
    cell66 != cell40 )).

fof(tlhfof37897,axiom,(
    cell66 != cell39 )).

fof(tlhfof37898,axiom,(
    cell66 != cell38 )).

fof(tlhfof37899,axiom,(
    cell66 != cell37 )).

fof(tlhfof37900,axiom,(
    cell66 != cell36 )).

fof(tlhfof37901,axiom,(
    cell66 != cell35 )).

fof(tlhfof37902,axiom,(
    cell66 != cell34 )).

fof(tlhfof37903,axiom,(
    cell66 != cell33 )).

fof(tlhfof37904,axiom,(
    cell66 != cell32 )).

fof(tlhfof37905,axiom,(
    cell66 != cell31 )).

fof(tlhfof37906,axiom,(
    cell66 != cell30 )).

fof(tlhfof37907,axiom,(
    cell66 != cell29 )).

fof(tlhfof37908,axiom,(
    cell66 != cell28 )).

fof(tlhfof37909,axiom,(
    cell66 != cell27 )).

fof(tlhfof37910,axiom,(
    cell66 != cell26 )).

fof(tlhfof37911,axiom,(
    cell66 != cell25 )).

fof(tlhfof37912,axiom,(
    cell66 != cell24 )).

fof(tlhfof37913,axiom,(
    cell66 != cell23 )).

fof(tlhfof37914,axiom,(
    cell66 != cell22 )).

fof(tlhfof37915,axiom,(
    cell66 != cell21 )).

fof(tlhfof37916,axiom,(
    cell66 != cell10 )).

fof(tlhfof37917,axiom,(
    cell66 != cell9 )).

fof(tlhfof37918,axiom,(
    cell66 != cell19 )).

fof(tlhfof37919,axiom,(
    cell66 != cell8 )).

fof(tlhfof37920,axiom,(
    cell66 != cell18 )).

fof(tlhfof37921,axiom,(
    cell66 != cell7 )).

fof(tlhfof37922,axiom,(
    cell66 != cell17 )).

fof(tlhfof37923,axiom,(
    cell66 != cell6 )).

fof(tlhfof37924,axiom,(
    cell66 != cell16 )).

fof(tlhfof37925,axiom,(
    cell66 != cell5 )).

fof(tlhfof37926,axiom,(
    cell66 != cell15 )).

fof(tlhfof37927,axiom,(
    cell66 != cell4 )).

fof(tlhfof37928,axiom,(
    cell66 != cell14 )).

fof(tlhfof37929,axiom,(
    cell66 != cell3 )).

fof(tlhfof37930,axiom,(
    cell66 != cell13 )).

fof(tlhfof37931,axiom,(
    cell66 != cell2 )).

fof(tlhfof37932,axiom,(
    cell66 != cell12 )).

fof(tlhfof37933,axiom,(
    cell66 != cell1 )).

fof(tlhfof37934,axiom,(
    cell66 != cell11 )).

fof(tlhfof37935,axiom,(
    cell66 != cell20 )).

fof(tlhfof37936,axiom,(
    cell65 != cell64 )).

fof(tlhfof37937,axiom,(
    cell65 != cell63 )).

fof(tlhfof37938,axiom,(
    cell65 != cell62 )).

fof(tlhfof37939,axiom,(
    cell65 != cell61 )).

fof(tlhfof37940,axiom,(
    cell65 != cell60 )).

fof(tlhfof37941,axiom,(
    cell65 != cell59 )).

fof(tlhfof37942,axiom,(
    cell65 != cell58 )).

fof(tlhfof37943,axiom,(
    cell65 != cell57 )).

fof(tlhfof37944,axiom,(
    cell65 != cell56 )).

fof(tlhfof37945,axiom,(
    cell65 != cell55 )).

fof(tlhfof37946,axiom,(
    cell65 != cell54 )).

fof(tlhfof37947,axiom,(
    cell65 != cell53 )).

fof(tlhfof37948,axiom,(
    cell65 != cell52 )).

fof(tlhfof37949,axiom,(
    cell65 != cell51 )).

fof(tlhfof37950,axiom,(
    cell65 != cell50 )).

fof(tlhfof37951,axiom,(
    cell65 != cell49 )).

fof(tlhfof37952,axiom,(
    cell65 != cell48 )).

fof(tlhfof37953,axiom,(
    cell65 != cell47 )).

fof(tlhfof37954,axiom,(
    cell65 != cell46 )).

fof(tlhfof37955,axiom,(
    cell65 != cell45 )).

fof(tlhfof37956,axiom,(
    cell65 != cell44 )).

fof(tlhfof37957,axiom,(
    cell65 != cell43 )).

fof(tlhfof37958,axiom,(
    cell65 != cell42 )).

fof(tlhfof37959,axiom,(
    cell65 != cell41 )).

fof(tlhfof37960,axiom,(
    cell65 != cell40 )).

fof(tlhfof37961,axiom,(
    cell65 != cell39 )).

fof(tlhfof37962,axiom,(
    cell65 != cell38 )).

fof(tlhfof37963,axiom,(
    cell65 != cell37 )).

fof(tlhfof37964,axiom,(
    cell65 != cell36 )).

fof(tlhfof37965,axiom,(
    cell65 != cell35 )).

fof(tlhfof37966,axiom,(
    cell65 != cell34 )).

fof(tlhfof37967,axiom,(
    cell65 != cell33 )).

fof(tlhfof37968,axiom,(
    cell65 != cell32 )).

fof(tlhfof37969,axiom,(
    cell65 != cell31 )).

fof(tlhfof37970,axiom,(
    cell65 != cell30 )).

fof(tlhfof37971,axiom,(
    cell65 != cell29 )).

fof(tlhfof37972,axiom,(
    cell65 != cell28 )).

fof(tlhfof37973,axiom,(
    cell65 != cell27 )).

fof(tlhfof37974,axiom,(
    cell65 != cell26 )).

fof(tlhfof37975,axiom,(
    cell65 != cell25 )).

fof(tlhfof37976,axiom,(
    cell65 != cell24 )).

fof(tlhfof37977,axiom,(
    cell65 != cell23 )).

fof(tlhfof37978,axiom,(
    cell65 != cell22 )).

fof(tlhfof37979,axiom,(
    cell65 != cell21 )).

fof(tlhfof37980,axiom,(
    cell65 != cell10 )).

fof(tlhfof37981,axiom,(
    cell65 != cell9 )).

fof(tlhfof37982,axiom,(
    cell65 != cell19 )).

fof(tlhfof37983,axiom,(
    cell65 != cell8 )).

fof(tlhfof37984,axiom,(
    cell65 != cell18 )).

fof(tlhfof37985,axiom,(
    cell65 != cell7 )).

fof(tlhfof37986,axiom,(
    cell65 != cell17 )).

fof(tlhfof37987,axiom,(
    cell65 != cell6 )).

fof(tlhfof37988,axiom,(
    cell65 != cell16 )).

fof(tlhfof37989,axiom,(
    cell65 != cell5 )).

fof(tlhfof37990,axiom,(
    cell65 != cell15 )).

fof(tlhfof37991,axiom,(
    cell65 != cell4 )).

fof(tlhfof37992,axiom,(
    cell65 != cell14 )).

fof(tlhfof37993,axiom,(
    cell65 != cell3 )).

fof(tlhfof37994,axiom,(
    cell65 != cell13 )).

fof(tlhfof37995,axiom,(
    cell65 != cell2 )).

fof(tlhfof37996,axiom,(
    cell65 != cell12 )).

fof(tlhfof37997,axiom,(
    cell65 != cell1 )).

fof(tlhfof37998,axiom,(
    cell65 != cell11 )).

fof(tlhfof37999,axiom,(
    cell65 != cell20 )).

fof(tlhfof38000,axiom,(
    cell64 != cell63 )).

fof(tlhfof38001,axiom,(
    cell64 != cell62 )).

fof(tlhfof38002,axiom,(
    cell64 != cell61 )).

fof(tlhfof38003,axiom,(
    cell64 != cell60 )).

fof(tlhfof38004,axiom,(
    cell64 != cell59 )).

fof(tlhfof38005,axiom,(
    cell64 != cell58 )).

fof(tlhfof38006,axiom,(
    cell64 != cell57 )).

fof(tlhfof38007,axiom,(
    cell64 != cell56 )).

fof(tlhfof38008,axiom,(
    cell64 != cell55 )).

fof(tlhfof38009,axiom,(
    cell64 != cell54 )).

fof(tlhfof38010,axiom,(
    cell64 != cell53 )).

fof(tlhfof38011,axiom,(
    cell64 != cell52 )).

fof(tlhfof38012,axiom,(
    cell64 != cell51 )).

fof(tlhfof38013,axiom,(
    cell64 != cell50 )).

fof(tlhfof38014,axiom,(
    cell64 != cell49 )).

fof(tlhfof38015,axiom,(
    cell64 != cell48 )).

fof(tlhfof38016,axiom,(
    cell64 != cell47 )).

fof(tlhfof38017,axiom,(
    cell64 != cell46 )).

fof(tlhfof38018,axiom,(
    cell64 != cell45 )).

fof(tlhfof38019,axiom,(
    cell64 != cell44 )).

fof(tlhfof38020,axiom,(
    cell64 != cell43 )).

fof(tlhfof38021,axiom,(
    cell64 != cell42 )).

fof(tlhfof38022,axiom,(
    cell64 != cell41 )).

fof(tlhfof38023,axiom,(
    cell64 != cell40 )).

fof(tlhfof38024,axiom,(
    cell64 != cell39 )).

fof(tlhfof38025,axiom,(
    cell64 != cell38 )).

fof(tlhfof38026,axiom,(
    cell64 != cell37 )).

fof(tlhfof38027,axiom,(
    cell64 != cell36 )).

fof(tlhfof38028,axiom,(
    cell64 != cell35 )).

fof(tlhfof38029,axiom,(
    cell64 != cell34 )).

fof(tlhfof38030,axiom,(
    cell64 != cell33 )).

fof(tlhfof38031,axiom,(
    cell64 != cell32 )).

fof(tlhfof38032,axiom,(
    cell64 != cell31 )).

fof(tlhfof38033,axiom,(
    cell64 != cell30 )).

fof(tlhfof38034,axiom,(
    cell64 != cell29 )).

fof(tlhfof38035,axiom,(
    cell64 != cell28 )).

fof(tlhfof38036,axiom,(
    cell64 != cell27 )).

fof(tlhfof38037,axiom,(
    cell64 != cell26 )).

fof(tlhfof38038,axiom,(
    cell64 != cell25 )).

fof(tlhfof38039,axiom,(
    cell64 != cell24 )).

fof(tlhfof38040,axiom,(
    cell64 != cell23 )).

fof(tlhfof38041,axiom,(
    cell64 != cell22 )).

fof(tlhfof38042,axiom,(
    cell64 != cell21 )).

fof(tlhfof38043,axiom,(
    cell64 != cell10 )).

fof(tlhfof38044,axiom,(
    cell64 != cell9 )).

fof(tlhfof38045,axiom,(
    cell64 != cell19 )).

fof(tlhfof38046,axiom,(
    cell64 != cell8 )).

fof(tlhfof38047,axiom,(
    cell64 != cell18 )).

fof(tlhfof38048,axiom,(
    cell64 != cell7 )).

fof(tlhfof38049,axiom,(
    cell64 != cell17 )).

fof(tlhfof38050,axiom,(
    cell64 != cell6 )).

fof(tlhfof38051,axiom,(
    cell64 != cell16 )).

fof(tlhfof38052,axiom,(
    cell64 != cell5 )).

fof(tlhfof38053,axiom,(
    cell64 != cell15 )).

fof(tlhfof38054,axiom,(
    cell64 != cell4 )).

fof(tlhfof38055,axiom,(
    cell64 != cell14 )).

fof(tlhfof38056,axiom,(
    cell64 != cell3 )).

fof(tlhfof38057,axiom,(
    cell64 != cell13 )).

fof(tlhfof38058,axiom,(
    cell64 != cell2 )).

fof(tlhfof38059,axiom,(
    cell64 != cell12 )).

fof(tlhfof38060,axiom,(
    cell64 != cell1 )).

fof(tlhfof38061,axiom,(
    cell64 != cell11 )).

fof(tlhfof38062,axiom,(
    cell64 != cell20 )).

fof(tlhfof38063,axiom,(
    cell63 != cell62 )).

fof(tlhfof38064,axiom,(
    cell63 != cell61 )).

fof(tlhfof38065,axiom,(
    cell63 != cell60 )).

fof(tlhfof38066,axiom,(
    cell63 != cell59 )).

fof(tlhfof38067,axiom,(
    cell63 != cell58 )).

fof(tlhfof38068,axiom,(
    cell63 != cell57 )).

fof(tlhfof38069,axiom,(
    cell63 != cell56 )).

fof(tlhfof38070,axiom,(
    cell63 != cell55 )).

fof(tlhfof38071,axiom,(
    cell63 != cell54 )).

fof(tlhfof38072,axiom,(
    cell63 != cell53 )).

fof(tlhfof38073,axiom,(
    cell63 != cell52 )).

fof(tlhfof38074,axiom,(
    cell63 != cell51 )).

fof(tlhfof38075,axiom,(
    cell63 != cell50 )).

fof(tlhfof38076,axiom,(
    cell63 != cell49 )).

fof(tlhfof38077,axiom,(
    cell63 != cell48 )).

fof(tlhfof38078,axiom,(
    cell63 != cell47 )).

fof(tlhfof38079,axiom,(
    cell63 != cell46 )).

fof(tlhfof38080,axiom,(
    cell63 != cell45 )).

fof(tlhfof38081,axiom,(
    cell63 != cell44 )).

fof(tlhfof38082,axiom,(
    cell63 != cell43 )).

fof(tlhfof38083,axiom,(
    cell63 != cell42 )).

fof(tlhfof38084,axiom,(
    cell63 != cell41 )).

fof(tlhfof38085,axiom,(
    cell63 != cell40 )).

fof(tlhfof38086,axiom,(
    cell63 != cell39 )).

fof(tlhfof38087,axiom,(
    cell63 != cell38 )).

fof(tlhfof38088,axiom,(
    cell63 != cell37 )).

fof(tlhfof38089,axiom,(
    cell63 != cell36 )).

fof(tlhfof38090,axiom,(
    cell63 != cell35 )).

fof(tlhfof38091,axiom,(
    cell63 != cell34 )).

fof(tlhfof38092,axiom,(
    cell63 != cell33 )).

fof(tlhfof38093,axiom,(
    cell63 != cell32 )).

fof(tlhfof38094,axiom,(
    cell63 != cell31 )).

fof(tlhfof38095,axiom,(
    cell63 != cell30 )).

fof(tlhfof38096,axiom,(
    cell63 != cell29 )).

fof(tlhfof38097,axiom,(
    cell63 != cell28 )).

fof(tlhfof38098,axiom,(
    cell63 != cell27 )).

fof(tlhfof38099,axiom,(
    cell63 != cell26 )).

fof(tlhfof38100,axiom,(
    cell63 != cell25 )).

fof(tlhfof38101,axiom,(
    cell63 != cell24 )).

fof(tlhfof38102,axiom,(
    cell63 != cell23 )).

fof(tlhfof38103,axiom,(
    cell63 != cell22 )).

fof(tlhfof38104,axiom,(
    cell63 != cell21 )).

fof(tlhfof38105,axiom,(
    cell63 != cell10 )).

fof(tlhfof38106,axiom,(
    cell63 != cell9 )).

fof(tlhfof38107,axiom,(
    cell63 != cell19 )).

fof(tlhfof38108,axiom,(
    cell63 != cell8 )).

fof(tlhfof38109,axiom,(
    cell63 != cell18 )).

fof(tlhfof38110,axiom,(
    cell63 != cell7 )).

fof(tlhfof38111,axiom,(
    cell63 != cell17 )).

fof(tlhfof38112,axiom,(
    cell63 != cell6 )).

fof(tlhfof38113,axiom,(
    cell63 != cell16 )).

fof(tlhfof38114,axiom,(
    cell63 != cell5 )).

fof(tlhfof38115,axiom,(
    cell63 != cell15 )).

fof(tlhfof38116,axiom,(
    cell63 != cell4 )).

fof(tlhfof38117,axiom,(
    cell63 != cell14 )).

fof(tlhfof38118,axiom,(
    cell63 != cell3 )).

fof(tlhfof38119,axiom,(
    cell63 != cell13 )).

fof(tlhfof38120,axiom,(
    cell63 != cell2 )).

fof(tlhfof38121,axiom,(
    cell63 != cell12 )).

fof(tlhfof38122,axiom,(
    cell63 != cell1 )).

fof(tlhfof38123,axiom,(
    cell63 != cell11 )).

fof(tlhfof38124,axiom,(
    cell63 != cell20 )).

fof(tlhfof38125,axiom,(
    cell62 != cell61 )).

fof(tlhfof38126,axiom,(
    cell62 != cell60 )).

fof(tlhfof38127,axiom,(
    cell62 != cell59 )).

fof(tlhfof38128,axiom,(
    cell62 != cell58 )).

fof(tlhfof38129,axiom,(
    cell62 != cell57 )).

fof(tlhfof38130,axiom,(
    cell62 != cell56 )).

fof(tlhfof38131,axiom,(
    cell62 != cell55 )).

fof(tlhfof38132,axiom,(
    cell62 != cell54 )).

fof(tlhfof38133,axiom,(
    cell62 != cell53 )).

fof(tlhfof38134,axiom,(
    cell62 != cell52 )).

fof(tlhfof38135,axiom,(
    cell62 != cell51 )).

fof(tlhfof38136,axiom,(
    cell62 != cell50 )).

fof(tlhfof38137,axiom,(
    cell62 != cell49 )).

fof(tlhfof38138,axiom,(
    cell62 != cell48 )).

fof(tlhfof38139,axiom,(
    cell62 != cell47 )).

fof(tlhfof38140,axiom,(
    cell62 != cell46 )).

fof(tlhfof38141,axiom,(
    cell62 != cell45 )).

fof(tlhfof38142,axiom,(
    cell62 != cell44 )).

fof(tlhfof38143,axiom,(
    cell62 != cell43 )).

fof(tlhfof38144,axiom,(
    cell62 != cell42 )).

fof(tlhfof38145,axiom,(
    cell62 != cell41 )).

fof(tlhfof38146,axiom,(
    cell62 != cell40 )).

fof(tlhfof38147,axiom,(
    cell62 != cell39 )).

fof(tlhfof38148,axiom,(
    cell62 != cell38 )).

fof(tlhfof38149,axiom,(
    cell62 != cell37 )).

fof(tlhfof38150,axiom,(
    cell62 != cell36 )).

fof(tlhfof38151,axiom,(
    cell62 != cell35 )).

fof(tlhfof38152,axiom,(
    cell62 != cell34 )).

fof(tlhfof38153,axiom,(
    cell62 != cell33 )).

fof(tlhfof38154,axiom,(
    cell62 != cell32 )).

fof(tlhfof38155,axiom,(
    cell62 != cell31 )).

fof(tlhfof38156,axiom,(
    cell62 != cell30 )).

fof(tlhfof38157,axiom,(
    cell62 != cell29 )).

fof(tlhfof38158,axiom,(
    cell62 != cell28 )).

fof(tlhfof38159,axiom,(
    cell62 != cell27 )).

fof(tlhfof38160,axiom,(
    cell62 != cell26 )).

fof(tlhfof38161,axiom,(
    cell62 != cell25 )).

fof(tlhfof38162,axiom,(
    cell62 != cell24 )).

fof(tlhfof38163,axiom,(
    cell62 != cell23 )).

fof(tlhfof38164,axiom,(
    cell62 != cell22 )).

fof(tlhfof38165,axiom,(
    cell62 != cell21 )).

fof(tlhfof38166,axiom,(
    cell62 != cell10 )).

fof(tlhfof38167,axiom,(
    cell62 != cell9 )).

fof(tlhfof38168,axiom,(
    cell62 != cell19 )).

fof(tlhfof38169,axiom,(
    cell62 != cell8 )).

fof(tlhfof38170,axiom,(
    cell62 != cell18 )).

fof(tlhfof38171,axiom,(
    cell62 != cell7 )).

fof(tlhfof38172,axiom,(
    cell62 != cell17 )).

fof(tlhfof38173,axiom,(
    cell62 != cell6 )).

fof(tlhfof38174,axiom,(
    cell62 != cell16 )).

fof(tlhfof38175,axiom,(
    cell62 != cell5 )).

fof(tlhfof38176,axiom,(
    cell62 != cell15 )).

fof(tlhfof38177,axiom,(
    cell62 != cell4 )).

fof(tlhfof38178,axiom,(
    cell62 != cell14 )).

fof(tlhfof38179,axiom,(
    cell62 != cell3 )).

fof(tlhfof38180,axiom,(
    cell62 != cell13 )).

fof(tlhfof38181,axiom,(
    cell62 != cell2 )).

fof(tlhfof38182,axiom,(
    cell62 != cell12 )).

fof(tlhfof38183,axiom,(
    cell62 != cell1 )).

fof(tlhfof38184,axiom,(
    cell62 != cell11 )).

fof(tlhfof38185,axiom,(
    cell62 != cell20 )).

fof(tlhfof38186,axiom,(
    cell61 != cell60 )).

fof(tlhfof38187,axiom,(
    cell61 != cell59 )).

fof(tlhfof38188,axiom,(
    cell61 != cell58 )).

fof(tlhfof38189,axiom,(
    cell61 != cell57 )).

fof(tlhfof38190,axiom,(
    cell61 != cell56 )).

fof(tlhfof38191,axiom,(
    cell61 != cell55 )).

fof(tlhfof38192,axiom,(
    cell61 != cell54 )).

fof(tlhfof38193,axiom,(
    cell61 != cell53 )).

fof(tlhfof38194,axiom,(
    cell61 != cell52 )).

fof(tlhfof38195,axiom,(
    cell61 != cell51 )).

fof(tlhfof38196,axiom,(
    cell61 != cell50 )).

fof(tlhfof38197,axiom,(
    cell61 != cell49 )).

fof(tlhfof38198,axiom,(
    cell61 != cell48 )).

fof(tlhfof38199,axiom,(
    cell61 != cell47 )).

fof(tlhfof38200,axiom,(
    cell61 != cell46 )).

fof(tlhfof38201,axiom,(
    cell61 != cell45 )).

fof(tlhfof38202,axiom,(
    cell61 != cell44 )).

fof(tlhfof38203,axiom,(
    cell61 != cell43 )).

fof(tlhfof38204,axiom,(
    cell61 != cell42 )).

fof(tlhfof38205,axiom,(
    cell61 != cell41 )).

fof(tlhfof38206,axiom,(
    cell61 != cell40 )).

fof(tlhfof38207,axiom,(
    cell61 != cell39 )).

fof(tlhfof38208,axiom,(
    cell61 != cell38 )).

fof(tlhfof38209,axiom,(
    cell61 != cell37 )).

fof(tlhfof38210,axiom,(
    cell61 != cell36 )).

fof(tlhfof38211,axiom,(
    cell61 != cell35 )).

fof(tlhfof38212,axiom,(
    cell61 != cell34 )).

fof(tlhfof38213,axiom,(
    cell61 != cell33 )).

fof(tlhfof38214,axiom,(
    cell61 != cell32 )).

fof(tlhfof38215,axiom,(
    cell61 != cell31 )).

fof(tlhfof38216,axiom,(
    cell61 != cell30 )).

fof(tlhfof38217,axiom,(
    cell61 != cell29 )).

fof(tlhfof38218,axiom,(
    cell61 != cell28 )).

fof(tlhfof38219,axiom,(
    cell61 != cell27 )).

fof(tlhfof38220,axiom,(
    cell61 != cell26 )).

fof(tlhfof38221,axiom,(
    cell61 != cell25 )).

fof(tlhfof38222,axiom,(
    cell61 != cell24 )).

fof(tlhfof38223,axiom,(
    cell61 != cell23 )).

fof(tlhfof38224,axiom,(
    cell61 != cell22 )).

fof(tlhfof38225,axiom,(
    cell61 != cell21 )).

fof(tlhfof38226,axiom,(
    cell61 != cell10 )).

fof(tlhfof38227,axiom,(
    cell61 != cell9 )).

fof(tlhfof38228,axiom,(
    cell61 != cell19 )).

fof(tlhfof38229,axiom,(
    cell61 != cell8 )).

fof(tlhfof38230,axiom,(
    cell61 != cell18 )).

fof(tlhfof38231,axiom,(
    cell61 != cell7 )).

fof(tlhfof38232,axiom,(
    cell61 != cell17 )).

fof(tlhfof38233,axiom,(
    cell61 != cell6 )).

fof(tlhfof38234,axiom,(
    cell61 != cell16 )).

fof(tlhfof38235,axiom,(
    cell61 != cell5 )).

fof(tlhfof38236,axiom,(
    cell61 != cell15 )).

fof(tlhfof38237,axiom,(
    cell61 != cell4 )).

fof(tlhfof38238,axiom,(
    cell61 != cell14 )).

fof(tlhfof38239,axiom,(
    cell61 != cell3 )).

fof(tlhfof38240,axiom,(
    cell61 != cell13 )).

fof(tlhfof38241,axiom,(
    cell61 != cell2 )).

fof(tlhfof38242,axiom,(
    cell61 != cell12 )).

fof(tlhfof38243,axiom,(
    cell61 != cell1 )).

fof(tlhfof38244,axiom,(
    cell61 != cell11 )).

fof(tlhfof38245,axiom,(
    cell61 != cell20 )).

fof(tlhfof38246,axiom,(
    cell60 != cell59 )).

fof(tlhfof38247,axiom,(
    cell60 != cell58 )).

fof(tlhfof38248,axiom,(
    cell60 != cell57 )).

fof(tlhfof38249,axiom,(
    cell60 != cell56 )).

fof(tlhfof38250,axiom,(
    cell60 != cell55 )).

fof(tlhfof38251,axiom,(
    cell60 != cell54 )).

fof(tlhfof38252,axiom,(
    cell60 != cell53 )).

fof(tlhfof38253,axiom,(
    cell60 != cell52 )).

fof(tlhfof38254,axiom,(
    cell60 != cell51 )).

fof(tlhfof38255,axiom,(
    cell60 != cell50 )).

fof(tlhfof38256,axiom,(
    cell60 != cell49 )).

fof(tlhfof38257,axiom,(
    cell60 != cell48 )).

fof(tlhfof38258,axiom,(
    cell60 != cell47 )).

fof(tlhfof38259,axiom,(
    cell60 != cell46 )).

fof(tlhfof38260,axiom,(
    cell60 != cell45 )).

fof(tlhfof38261,axiom,(
    cell60 != cell44 )).

fof(tlhfof38262,axiom,(
    cell60 != cell43 )).

fof(tlhfof38263,axiom,(
    cell60 != cell42 )).

fof(tlhfof38264,axiom,(
    cell60 != cell41 )).

fof(tlhfof38265,axiom,(
    cell60 != cell40 )).

fof(tlhfof38266,axiom,(
    cell60 != cell39 )).

fof(tlhfof38267,axiom,(
    cell60 != cell38 )).

fof(tlhfof38268,axiom,(
    cell60 != cell37 )).

fof(tlhfof38269,axiom,(
    cell60 != cell36 )).

fof(tlhfof38270,axiom,(
    cell60 != cell35 )).

fof(tlhfof38271,axiom,(
    cell60 != cell34 )).

fof(tlhfof38272,axiom,(
    cell60 != cell33 )).

fof(tlhfof38273,axiom,(
    cell60 != cell32 )).

fof(tlhfof38274,axiom,(
    cell60 != cell31 )).

fof(tlhfof38275,axiom,(
    cell60 != cell30 )).

fof(tlhfof38276,axiom,(
    cell60 != cell29 )).

fof(tlhfof38277,axiom,(
    cell60 != cell28 )).

fof(tlhfof38278,axiom,(
    cell60 != cell27 )).

fof(tlhfof38279,axiom,(
    cell60 != cell26 )).

fof(tlhfof38280,axiom,(
    cell60 != cell25 )).

fof(tlhfof38281,axiom,(
    cell60 != cell24 )).

fof(tlhfof38282,axiom,(
    cell60 != cell23 )).

fof(tlhfof38283,axiom,(
    cell60 != cell22 )).

fof(tlhfof38284,axiom,(
    cell60 != cell21 )).

fof(tlhfof38285,axiom,(
    cell60 != cell10 )).

fof(tlhfof38286,axiom,(
    cell60 != cell9 )).

fof(tlhfof38287,axiom,(
    cell60 != cell19 )).

fof(tlhfof38288,axiom,(
    cell60 != cell8 )).

fof(tlhfof38289,axiom,(
    cell60 != cell18 )).

fof(tlhfof38290,axiom,(
    cell60 != cell7 )).

fof(tlhfof38291,axiom,(
    cell60 != cell17 )).

fof(tlhfof38292,axiom,(
    cell60 != cell6 )).

fof(tlhfof38293,axiom,(
    cell60 != cell16 )).

fof(tlhfof38294,axiom,(
    cell60 != cell5 )).

fof(tlhfof38295,axiom,(
    cell60 != cell15 )).

fof(tlhfof38296,axiom,(
    cell60 != cell4 )).

fof(tlhfof38297,axiom,(
    cell60 != cell14 )).

fof(tlhfof38298,axiom,(
    cell60 != cell3 )).

fof(tlhfof38299,axiom,(
    cell60 != cell13 )).

fof(tlhfof38300,axiom,(
    cell60 != cell2 )).

fof(tlhfof38301,axiom,(
    cell60 != cell12 )).

fof(tlhfof38302,axiom,(
    cell60 != cell1 )).

fof(tlhfof38303,axiom,(
    cell60 != cell11 )).

fof(tlhfof38304,axiom,(
    cell60 != cell20 )).

fof(tlhfof38305,axiom,(
    cell59 != cell58 )).

fof(tlhfof38306,axiom,(
    cell59 != cell57 )).

fof(tlhfof38307,axiom,(
    cell59 != cell56 )).

fof(tlhfof38308,axiom,(
    cell59 != cell55 )).

fof(tlhfof38309,axiom,(
    cell59 != cell54 )).

fof(tlhfof38310,axiom,(
    cell59 != cell53 )).

fof(tlhfof38311,axiom,(
    cell59 != cell52 )).

fof(tlhfof38312,axiom,(
    cell59 != cell51 )).

fof(tlhfof38313,axiom,(
    cell59 != cell50 )).

fof(tlhfof38314,axiom,(
    cell59 != cell49 )).

fof(tlhfof38315,axiom,(
    cell59 != cell48 )).

fof(tlhfof38316,axiom,(
    cell59 != cell47 )).

fof(tlhfof38317,axiom,(
    cell59 != cell46 )).

fof(tlhfof38318,axiom,(
    cell59 != cell45 )).

fof(tlhfof38319,axiom,(
    cell59 != cell44 )).

fof(tlhfof38320,axiom,(
    cell59 != cell43 )).

fof(tlhfof38321,axiom,(
    cell59 != cell42 )).

fof(tlhfof38322,axiom,(
    cell59 != cell41 )).

fof(tlhfof38323,axiom,(
    cell59 != cell40 )).

fof(tlhfof38324,axiom,(
    cell59 != cell39 )).

fof(tlhfof38325,axiom,(
    cell59 != cell38 )).

fof(tlhfof38326,axiom,(
    cell59 != cell37 )).

fof(tlhfof38327,axiom,(
    cell59 != cell36 )).

fof(tlhfof38328,axiom,(
    cell59 != cell35 )).

fof(tlhfof38329,axiom,(
    cell59 != cell34 )).

fof(tlhfof38330,axiom,(
    cell59 != cell33 )).

fof(tlhfof38331,axiom,(
    cell59 != cell32 )).

fof(tlhfof38332,axiom,(
    cell59 != cell31 )).

fof(tlhfof38333,axiom,(
    cell59 != cell30 )).

fof(tlhfof38334,axiom,(
    cell59 != cell29 )).

fof(tlhfof38335,axiom,(
    cell59 != cell28 )).

fof(tlhfof38336,axiom,(
    cell59 != cell27 )).

fof(tlhfof38337,axiom,(
    cell59 != cell26 )).

fof(tlhfof38338,axiom,(
    cell59 != cell25 )).

fof(tlhfof38339,axiom,(
    cell59 != cell24 )).

fof(tlhfof38340,axiom,(
    cell59 != cell23 )).

fof(tlhfof38341,axiom,(
    cell59 != cell22 )).

fof(tlhfof38342,axiom,(
    cell59 != cell21 )).

fof(tlhfof38343,axiom,(
    cell59 != cell10 )).

fof(tlhfof38344,axiom,(
    cell59 != cell9 )).

fof(tlhfof38345,axiom,(
    cell59 != cell19 )).

fof(tlhfof38346,axiom,(
    cell59 != cell8 )).

fof(tlhfof38347,axiom,(
    cell59 != cell18 )).

fof(tlhfof38348,axiom,(
    cell59 != cell7 )).

fof(tlhfof38349,axiom,(
    cell59 != cell17 )).

fof(tlhfof38350,axiom,(
    cell59 != cell6 )).

fof(tlhfof38351,axiom,(
    cell59 != cell16 )).

fof(tlhfof38352,axiom,(
    cell59 != cell5 )).

fof(tlhfof38353,axiom,(
    cell59 != cell15 )).

fof(tlhfof38354,axiom,(
    cell59 != cell4 )).

fof(tlhfof38355,axiom,(
    cell59 != cell14 )).

fof(tlhfof38356,axiom,(
    cell59 != cell3 )).

fof(tlhfof38357,axiom,(
    cell59 != cell13 )).

fof(tlhfof38358,axiom,(
    cell59 != cell2 )).

fof(tlhfof38359,axiom,(
    cell59 != cell12 )).

fof(tlhfof38360,axiom,(
    cell59 != cell1 )).

fof(tlhfof38361,axiom,(
    cell59 != cell11 )).

fof(tlhfof38362,axiom,(
    cell59 != cell20 )).

fof(tlhfof38363,axiom,(
    cell58 != cell57 )).

fof(tlhfof38364,axiom,(
    cell58 != cell56 )).

fof(tlhfof38365,axiom,(
    cell58 != cell55 )).

fof(tlhfof38366,axiom,(
    cell58 != cell54 )).

fof(tlhfof38367,axiom,(
    cell58 != cell53 )).

fof(tlhfof38368,axiom,(
    cell58 != cell52 )).

fof(tlhfof38369,axiom,(
    cell58 != cell51 )).

fof(tlhfof38370,axiom,(
    cell58 != cell50 )).

fof(tlhfof38371,axiom,(
    cell58 != cell49 )).

fof(tlhfof38372,axiom,(
    cell58 != cell48 )).

fof(tlhfof38373,axiom,(
    cell58 != cell47 )).

fof(tlhfof38374,axiom,(
    cell58 != cell46 )).

fof(tlhfof38375,axiom,(
    cell58 != cell45 )).

fof(tlhfof38376,axiom,(
    cell58 != cell44 )).

fof(tlhfof38377,axiom,(
    cell58 != cell43 )).

fof(tlhfof38378,axiom,(
    cell58 != cell42 )).

fof(tlhfof38379,axiom,(
    cell58 != cell41 )).

fof(tlhfof38380,axiom,(
    cell58 != cell40 )).

fof(tlhfof38381,axiom,(
    cell58 != cell39 )).

fof(tlhfof38382,axiom,(
    cell58 != cell38 )).

fof(tlhfof38383,axiom,(
    cell58 != cell37 )).

fof(tlhfof38384,axiom,(
    cell58 != cell36 )).

fof(tlhfof38385,axiom,(
    cell58 != cell35 )).

fof(tlhfof38386,axiom,(
    cell58 != cell34 )).

fof(tlhfof38387,axiom,(
    cell58 != cell33 )).

fof(tlhfof38388,axiom,(
    cell58 != cell32 )).

fof(tlhfof38389,axiom,(
    cell58 != cell31 )).

fof(tlhfof38390,axiom,(
    cell58 != cell30 )).

fof(tlhfof38391,axiom,(
    cell58 != cell29 )).

fof(tlhfof38392,axiom,(
    cell58 != cell28 )).

fof(tlhfof38393,axiom,(
    cell58 != cell27 )).

fof(tlhfof38394,axiom,(
    cell58 != cell26 )).

fof(tlhfof38395,axiom,(
    cell58 != cell25 )).

fof(tlhfof38396,axiom,(
    cell58 != cell24 )).

fof(tlhfof38397,axiom,(
    cell58 != cell23 )).

fof(tlhfof38398,axiom,(
    cell58 != cell22 )).

fof(tlhfof38399,axiom,(
    cell58 != cell21 )).

fof(tlhfof38400,axiom,(
    cell58 != cell10 )).

fof(tlhfof38401,axiom,(
    cell58 != cell9 )).

fof(tlhfof38402,axiom,(
    cell58 != cell19 )).

fof(tlhfof38403,axiom,(
    cell58 != cell8 )).

fof(tlhfof38404,axiom,(
    cell58 != cell18 )).

fof(tlhfof38405,axiom,(
    cell58 != cell7 )).

fof(tlhfof38406,axiom,(
    cell58 != cell17 )).

fof(tlhfof38407,axiom,(
    cell58 != cell6 )).

fof(tlhfof38408,axiom,(
    cell58 != cell16 )).

fof(tlhfof38409,axiom,(
    cell58 != cell5 )).

fof(tlhfof38410,axiom,(
    cell58 != cell15 )).

fof(tlhfof38411,axiom,(
    cell58 != cell4 )).

fof(tlhfof38412,axiom,(
    cell58 != cell14 )).

fof(tlhfof38413,axiom,(
    cell58 != cell3 )).

fof(tlhfof38414,axiom,(
    cell58 != cell13 )).

fof(tlhfof38415,axiom,(
    cell58 != cell2 )).

fof(tlhfof38416,axiom,(
    cell58 != cell12 )).

fof(tlhfof38417,axiom,(
    cell58 != cell1 )).

fof(tlhfof38418,axiom,(
    cell58 != cell11 )).

fof(tlhfof38419,axiom,(
    cell58 != cell20 )).

fof(tlhfof38420,axiom,(
    cell57 != cell56 )).

fof(tlhfof38421,axiom,(
    cell57 != cell55 )).

fof(tlhfof38422,axiom,(
    cell57 != cell54 )).

fof(tlhfof38423,axiom,(
    cell57 != cell53 )).

fof(tlhfof38424,axiom,(
    cell57 != cell52 )).

fof(tlhfof38425,axiom,(
    cell57 != cell51 )).

fof(tlhfof38426,axiom,(
    cell57 != cell50 )).

fof(tlhfof38427,axiom,(
    cell57 != cell49 )).

fof(tlhfof38428,axiom,(
    cell57 != cell48 )).

fof(tlhfof38429,axiom,(
    cell57 != cell47 )).

fof(tlhfof38430,axiom,(
    cell57 != cell46 )).

fof(tlhfof38431,axiom,(
    cell57 != cell45 )).

fof(tlhfof38432,axiom,(
    cell57 != cell44 )).

fof(tlhfof38433,axiom,(
    cell57 != cell43 )).

fof(tlhfof38434,axiom,(
    cell57 != cell42 )).

fof(tlhfof38435,axiom,(
    cell57 != cell41 )).

fof(tlhfof38436,axiom,(
    cell57 != cell40 )).

fof(tlhfof38437,axiom,(
    cell57 != cell39 )).

fof(tlhfof38438,axiom,(
    cell57 != cell38 )).

fof(tlhfof38439,axiom,(
    cell57 != cell37 )).

fof(tlhfof38440,axiom,(
    cell57 != cell36 )).

fof(tlhfof38441,axiom,(
    cell57 != cell35 )).

fof(tlhfof38442,axiom,(
    cell57 != cell34 )).

fof(tlhfof38443,axiom,(
    cell57 != cell33 )).

fof(tlhfof38444,axiom,(
    cell57 != cell32 )).

fof(tlhfof38445,axiom,(
    cell57 != cell31 )).

fof(tlhfof38446,axiom,(
    cell57 != cell30 )).

fof(tlhfof38447,axiom,(
    cell57 != cell29 )).

fof(tlhfof38448,axiom,(
    cell57 != cell28 )).

fof(tlhfof38449,axiom,(
    cell57 != cell27 )).

fof(tlhfof38450,axiom,(
    cell57 != cell26 )).

fof(tlhfof38451,axiom,(
    cell57 != cell25 )).

fof(tlhfof38452,axiom,(
    cell57 != cell24 )).

fof(tlhfof38453,axiom,(
    cell57 != cell23 )).

fof(tlhfof38454,axiom,(
    cell57 != cell22 )).

fof(tlhfof38455,axiom,(
    cell57 != cell21 )).

fof(tlhfof38456,axiom,(
    cell57 != cell10 )).

fof(tlhfof38457,axiom,(
    cell57 != cell9 )).

fof(tlhfof38458,axiom,(
    cell57 != cell19 )).

fof(tlhfof38459,axiom,(
    cell57 != cell8 )).

fof(tlhfof38460,axiom,(
    cell57 != cell18 )).

fof(tlhfof38461,axiom,(
    cell57 != cell7 )).

fof(tlhfof38462,axiom,(
    cell57 != cell17 )).

fof(tlhfof38463,axiom,(
    cell57 != cell6 )).

fof(tlhfof38464,axiom,(
    cell57 != cell16 )).

fof(tlhfof38465,axiom,(
    cell57 != cell5 )).

fof(tlhfof38466,axiom,(
    cell57 != cell15 )).

fof(tlhfof38467,axiom,(
    cell57 != cell4 )).

fof(tlhfof38468,axiom,(
    cell57 != cell14 )).

fof(tlhfof38469,axiom,(
    cell57 != cell3 )).

fof(tlhfof38470,axiom,(
    cell57 != cell13 )).

fof(tlhfof38471,axiom,(
    cell57 != cell2 )).

fof(tlhfof38472,axiom,(
    cell57 != cell12 )).

fof(tlhfof38473,axiom,(
    cell57 != cell1 )).

fof(tlhfof38474,axiom,(
    cell57 != cell11 )).

fof(tlhfof38475,axiom,(
    cell57 != cell20 )).

fof(tlhfof38476,axiom,(
    cell56 != cell55 )).

fof(tlhfof38477,axiom,(
    cell56 != cell54 )).

fof(tlhfof38478,axiom,(
    cell56 != cell53 )).

fof(tlhfof38479,axiom,(
    cell56 != cell52 )).

fof(tlhfof38480,axiom,(
    cell56 != cell51 )).

fof(tlhfof38481,axiom,(
    cell56 != cell50 )).

fof(tlhfof38482,axiom,(
    cell56 != cell49 )).

fof(tlhfof38483,axiom,(
    cell56 != cell48 )).

fof(tlhfof38484,axiom,(
    cell56 != cell47 )).

fof(tlhfof38485,axiom,(
    cell56 != cell46 )).

fof(tlhfof38486,axiom,(
    cell56 != cell45 )).

fof(tlhfof38487,axiom,(
    cell56 != cell44 )).

fof(tlhfof38488,axiom,(
    cell56 != cell43 )).

fof(tlhfof38489,axiom,(
    cell56 != cell42 )).

fof(tlhfof38490,axiom,(
    cell56 != cell41 )).

fof(tlhfof38491,axiom,(
    cell56 != cell40 )).

fof(tlhfof38492,axiom,(
    cell56 != cell39 )).

fof(tlhfof38493,axiom,(
    cell56 != cell38 )).

fof(tlhfof38494,axiom,(
    cell56 != cell37 )).

fof(tlhfof38495,axiom,(
    cell56 != cell36 )).

fof(tlhfof38496,axiom,(
    cell56 != cell35 )).

fof(tlhfof38497,axiom,(
    cell56 != cell34 )).

fof(tlhfof38498,axiom,(
    cell56 != cell33 )).

fof(tlhfof38499,axiom,(
    cell56 != cell32 )).

fof(tlhfof38500,axiom,(
    cell56 != cell31 )).

fof(tlhfof38501,axiom,(
    cell56 != cell30 )).

fof(tlhfof38502,axiom,(
    cell56 != cell29 )).

fof(tlhfof38503,axiom,(
    cell56 != cell28 )).

fof(tlhfof38504,axiom,(
    cell56 != cell27 )).

fof(tlhfof38505,axiom,(
    cell56 != cell26 )).

fof(tlhfof38506,axiom,(
    cell56 != cell25 )).

fof(tlhfof38507,axiom,(
    cell56 != cell24 )).

fof(tlhfof38508,axiom,(
    cell56 != cell23 )).

fof(tlhfof38509,axiom,(
    cell56 != cell22 )).

fof(tlhfof38510,axiom,(
    cell56 != cell21 )).

fof(tlhfof38511,axiom,(
    cell56 != cell10 )).

fof(tlhfof38512,axiom,(
    cell56 != cell9 )).

fof(tlhfof38513,axiom,(
    cell56 != cell19 )).

fof(tlhfof38514,axiom,(
    cell56 != cell8 )).

fof(tlhfof38515,axiom,(
    cell56 != cell18 )).

fof(tlhfof38516,axiom,(
    cell56 != cell7 )).

fof(tlhfof38517,axiom,(
    cell56 != cell17 )).

fof(tlhfof38518,axiom,(
    cell56 != cell6 )).

fof(tlhfof38519,axiom,(
    cell56 != cell16 )).

fof(tlhfof38520,axiom,(
    cell56 != cell5 )).

fof(tlhfof38521,axiom,(
    cell56 != cell15 )).

fof(tlhfof38522,axiom,(
    cell56 != cell4 )).

fof(tlhfof38523,axiom,(
    cell56 != cell14 )).

fof(tlhfof38524,axiom,(
    cell56 != cell3 )).

fof(tlhfof38525,axiom,(
    cell56 != cell13 )).

fof(tlhfof38526,axiom,(
    cell56 != cell2 )).

fof(tlhfof38527,axiom,(
    cell56 != cell12 )).

fof(tlhfof38528,axiom,(
    cell56 != cell1 )).

fof(tlhfof38529,axiom,(
    cell56 != cell11 )).

fof(tlhfof38530,axiom,(
    cell56 != cell20 )).

fof(tlhfof38531,axiom,(
    cell55 != cell54 )).

fof(tlhfof38532,axiom,(
    cell55 != cell53 )).

fof(tlhfof38533,axiom,(
    cell55 != cell52 )).

fof(tlhfof38534,axiom,(
    cell55 != cell51 )).

fof(tlhfof38535,axiom,(
    cell55 != cell50 )).

fof(tlhfof38536,axiom,(
    cell55 != cell49 )).

fof(tlhfof38537,axiom,(
    cell55 != cell48 )).

fof(tlhfof38538,axiom,(
    cell55 != cell47 )).

fof(tlhfof38539,axiom,(
    cell55 != cell46 )).

fof(tlhfof38540,axiom,(
    cell55 != cell45 )).

fof(tlhfof38541,axiom,(
    cell55 != cell44 )).

fof(tlhfof38542,axiom,(
    cell55 != cell43 )).

fof(tlhfof38543,axiom,(
    cell55 != cell42 )).

fof(tlhfof38544,axiom,(
    cell55 != cell41 )).

fof(tlhfof38545,axiom,(
    cell55 != cell40 )).

fof(tlhfof38546,axiom,(
    cell55 != cell39 )).

fof(tlhfof38547,axiom,(
    cell55 != cell38 )).

fof(tlhfof38548,axiom,(
    cell55 != cell37 )).

fof(tlhfof38549,axiom,(
    cell55 != cell36 )).

fof(tlhfof38550,axiom,(
    cell55 != cell35 )).

fof(tlhfof38551,axiom,(
    cell55 != cell34 )).

fof(tlhfof38552,axiom,(
    cell55 != cell33 )).

fof(tlhfof38553,axiom,(
    cell55 != cell32 )).

fof(tlhfof38554,axiom,(
    cell55 != cell31 )).

fof(tlhfof38555,axiom,(
    cell55 != cell30 )).

fof(tlhfof38556,axiom,(
    cell55 != cell29 )).

fof(tlhfof38557,axiom,(
    cell55 != cell28 )).

fof(tlhfof38558,axiom,(
    cell55 != cell27 )).

fof(tlhfof38559,axiom,(
    cell55 != cell26 )).

fof(tlhfof38560,axiom,(
    cell55 != cell25 )).

fof(tlhfof38561,axiom,(
    cell55 != cell24 )).

fof(tlhfof38562,axiom,(
    cell55 != cell23 )).

fof(tlhfof38563,axiom,(
    cell55 != cell22 )).

fof(tlhfof38564,axiom,(
    cell55 != cell21 )).

fof(tlhfof38565,axiom,(
    cell55 != cell10 )).

fof(tlhfof38566,axiom,(
    cell55 != cell9 )).

fof(tlhfof38567,axiom,(
    cell55 != cell19 )).

fof(tlhfof38568,axiom,(
    cell55 != cell8 )).

fof(tlhfof38569,axiom,(
    cell55 != cell18 )).

fof(tlhfof38570,axiom,(
    cell55 != cell7 )).

fof(tlhfof38571,axiom,(
    cell55 != cell17 )).

fof(tlhfof38572,axiom,(
    cell55 != cell6 )).

fof(tlhfof38573,axiom,(
    cell55 != cell16 )).

fof(tlhfof38574,axiom,(
    cell55 != cell5 )).

fof(tlhfof38575,axiom,(
    cell55 != cell15 )).

fof(tlhfof38576,axiom,(
    cell55 != cell4 )).

fof(tlhfof38577,axiom,(
    cell55 != cell14 )).

fof(tlhfof38578,axiom,(
    cell55 != cell3 )).

fof(tlhfof38579,axiom,(
    cell55 != cell13 )).

fof(tlhfof38580,axiom,(
    cell55 != cell2 )).

fof(tlhfof38581,axiom,(
    cell55 != cell12 )).

fof(tlhfof38582,axiom,(
    cell55 != cell1 )).

fof(tlhfof38583,axiom,(
    cell55 != cell11 )).

fof(tlhfof38584,axiom,(
    cell55 != cell20 )).

fof(tlhfof38585,axiom,(
    cell54 != cell53 )).

fof(tlhfof38586,axiom,(
    cell54 != cell52 )).

fof(tlhfof38587,axiom,(
    cell54 != cell51 )).

fof(tlhfof38588,axiom,(
    cell54 != cell50 )).

fof(tlhfof38589,axiom,(
    cell54 != cell49 )).

fof(tlhfof38590,axiom,(
    cell54 != cell48 )).

fof(tlhfof38591,axiom,(
    cell54 != cell47 )).

fof(tlhfof38592,axiom,(
    cell54 != cell46 )).

fof(tlhfof38593,axiom,(
    cell54 != cell45 )).

fof(tlhfof38594,axiom,(
    cell54 != cell44 )).

fof(tlhfof38595,axiom,(
    cell54 != cell43 )).

fof(tlhfof38596,axiom,(
    cell54 != cell42 )).

fof(tlhfof38597,axiom,(
    cell54 != cell41 )).

fof(tlhfof38598,axiom,(
    cell54 != cell40 )).

fof(tlhfof38599,axiom,(
    cell54 != cell39 )).

fof(tlhfof38600,axiom,(
    cell54 != cell38 )).

fof(tlhfof38601,axiom,(
    cell54 != cell37 )).

fof(tlhfof38602,axiom,(
    cell54 != cell36 )).

fof(tlhfof38603,axiom,(
    cell54 != cell35 )).

fof(tlhfof38604,axiom,(
    cell54 != cell34 )).

fof(tlhfof38605,axiom,(
    cell54 != cell33 )).

fof(tlhfof38606,axiom,(
    cell54 != cell32 )).

fof(tlhfof38607,axiom,(
    cell54 != cell31 )).

fof(tlhfof38608,axiom,(
    cell54 != cell30 )).

fof(tlhfof38609,axiom,(
    cell54 != cell29 )).

fof(tlhfof38610,axiom,(
    cell54 != cell28 )).

fof(tlhfof38611,axiom,(
    cell54 != cell27 )).

fof(tlhfof38612,axiom,(
    cell54 != cell26 )).

fof(tlhfof38613,axiom,(
    cell54 != cell25 )).

fof(tlhfof38614,axiom,(
    cell54 != cell24 )).

fof(tlhfof38615,axiom,(
    cell54 != cell23 )).

fof(tlhfof38616,axiom,(
    cell54 != cell22 )).

fof(tlhfof38617,axiom,(
    cell54 != cell21 )).

fof(tlhfof38618,axiom,(
    cell54 != cell10 )).

fof(tlhfof38619,axiom,(
    cell54 != cell9 )).

fof(tlhfof38620,axiom,(
    cell54 != cell19 )).

fof(tlhfof38621,axiom,(
    cell54 != cell8 )).

fof(tlhfof38622,axiom,(
    cell54 != cell18 )).

fof(tlhfof38623,axiom,(
    cell54 != cell7 )).

fof(tlhfof38624,axiom,(
    cell54 != cell17 )).

fof(tlhfof38625,axiom,(
    cell54 != cell6 )).

fof(tlhfof38626,axiom,(
    cell54 != cell16 )).

fof(tlhfof38627,axiom,(
    cell54 != cell5 )).

fof(tlhfof38628,axiom,(
    cell54 != cell15 )).

fof(tlhfof38629,axiom,(
    cell54 != cell4 )).

fof(tlhfof38630,axiom,(
    cell54 != cell14 )).

fof(tlhfof38631,axiom,(
    cell54 != cell3 )).

fof(tlhfof38632,axiom,(
    cell54 != cell13 )).

fof(tlhfof38633,axiom,(
    cell54 != cell2 )).

fof(tlhfof38634,axiom,(
    cell54 != cell12 )).

fof(tlhfof38635,axiom,(
    cell54 != cell1 )).

fof(tlhfof38636,axiom,(
    cell54 != cell11 )).

fof(tlhfof38637,axiom,(
    cell54 != cell20 )).

fof(tlhfof38638,axiom,(
    cell53 != cell52 )).

fof(tlhfof38639,axiom,(
    cell53 != cell51 )).

fof(tlhfof38640,axiom,(
    cell53 != cell50 )).

fof(tlhfof38641,axiom,(
    cell53 != cell49 )).

fof(tlhfof38642,axiom,(
    cell53 != cell48 )).

fof(tlhfof38643,axiom,(
    cell53 != cell47 )).

fof(tlhfof38644,axiom,(
    cell53 != cell46 )).

fof(tlhfof38645,axiom,(
    cell53 != cell45 )).

fof(tlhfof38646,axiom,(
    cell53 != cell44 )).

fof(tlhfof38647,axiom,(
    cell53 != cell43 )).

fof(tlhfof38648,axiom,(
    cell53 != cell42 )).

fof(tlhfof38649,axiom,(
    cell53 != cell41 )).

fof(tlhfof38650,axiom,(
    cell53 != cell40 )).

fof(tlhfof38651,axiom,(
    cell53 != cell39 )).

fof(tlhfof38652,axiom,(
    cell53 != cell38 )).

fof(tlhfof38653,axiom,(
    cell53 != cell37 )).

fof(tlhfof38654,axiom,(
    cell53 != cell36 )).

fof(tlhfof38655,axiom,(
    cell53 != cell35 )).

fof(tlhfof38656,axiom,(
    cell53 != cell34 )).

fof(tlhfof38657,axiom,(
    cell53 != cell33 )).

fof(tlhfof38658,axiom,(
    cell53 != cell32 )).

fof(tlhfof38659,axiom,(
    cell53 != cell31 )).

fof(tlhfof38660,axiom,(
    cell53 != cell30 )).

fof(tlhfof38661,axiom,(
    cell53 != cell29 )).

fof(tlhfof38662,axiom,(
    cell53 != cell28 )).

fof(tlhfof38663,axiom,(
    cell53 != cell27 )).

fof(tlhfof38664,axiom,(
    cell53 != cell26 )).

fof(tlhfof38665,axiom,(
    cell53 != cell25 )).

fof(tlhfof38666,axiom,(
    cell53 != cell24 )).

fof(tlhfof38667,axiom,(
    cell53 != cell23 )).

fof(tlhfof38668,axiom,(
    cell53 != cell22 )).

fof(tlhfof38669,axiom,(
    cell53 != cell21 )).

fof(tlhfof38670,axiom,(
    cell53 != cell10 )).

fof(tlhfof38671,axiom,(
    cell53 != cell9 )).

fof(tlhfof38672,axiom,(
    cell53 != cell19 )).

fof(tlhfof38673,axiom,(
    cell53 != cell8 )).

fof(tlhfof38674,axiom,(
    cell53 != cell18 )).

fof(tlhfof38675,axiom,(
    cell53 != cell7 )).

fof(tlhfof38676,axiom,(
    cell53 != cell17 )).

fof(tlhfof38677,axiom,(
    cell53 != cell6 )).

fof(tlhfof38678,axiom,(
    cell53 != cell16 )).

fof(tlhfof38679,axiom,(
    cell53 != cell5 )).

fof(tlhfof38680,axiom,(
    cell53 != cell15 )).

fof(tlhfof38681,axiom,(
    cell53 != cell4 )).

fof(tlhfof38682,axiom,(
    cell53 != cell14 )).

fof(tlhfof38683,axiom,(
    cell53 != cell3 )).

fof(tlhfof38684,axiom,(
    cell53 != cell13 )).

fof(tlhfof38685,axiom,(
    cell53 != cell2 )).

fof(tlhfof38686,axiom,(
    cell53 != cell12 )).

fof(tlhfof38687,axiom,(
    cell53 != cell1 )).

fof(tlhfof38688,axiom,(
    cell53 != cell11 )).

fof(tlhfof38689,axiom,(
    cell53 != cell20 )).

fof(tlhfof38690,axiom,(
    cell52 != cell51 )).

fof(tlhfof38691,axiom,(
    cell52 != cell50 )).

fof(tlhfof38692,axiom,(
    cell52 != cell49 )).

fof(tlhfof38693,axiom,(
    cell52 != cell48 )).

fof(tlhfof38694,axiom,(
    cell52 != cell47 )).

fof(tlhfof38695,axiom,(
    cell52 != cell46 )).

fof(tlhfof38696,axiom,(
    cell52 != cell45 )).

fof(tlhfof38697,axiom,(
    cell52 != cell44 )).

fof(tlhfof38698,axiom,(
    cell52 != cell43 )).

fof(tlhfof38699,axiom,(
    cell52 != cell42 )).

fof(tlhfof38700,axiom,(
    cell52 != cell41 )).

fof(tlhfof38701,axiom,(
    cell52 != cell40 )).

fof(tlhfof38702,axiom,(
    cell52 != cell39 )).

fof(tlhfof38703,axiom,(
    cell52 != cell38 )).

fof(tlhfof38704,axiom,(
    cell52 != cell37 )).

fof(tlhfof38705,axiom,(
    cell52 != cell36 )).

fof(tlhfof38706,axiom,(
    cell52 != cell35 )).

fof(tlhfof38707,axiom,(
    cell52 != cell34 )).

fof(tlhfof38708,axiom,(
    cell52 != cell33 )).

fof(tlhfof38709,axiom,(
    cell52 != cell32 )).

fof(tlhfof38710,axiom,(
    cell52 != cell31 )).

fof(tlhfof38711,axiom,(
    cell52 != cell30 )).

fof(tlhfof38712,axiom,(
    cell52 != cell29 )).

fof(tlhfof38713,axiom,(
    cell52 != cell28 )).

fof(tlhfof38714,axiom,(
    cell52 != cell27 )).

fof(tlhfof38715,axiom,(
    cell52 != cell26 )).

fof(tlhfof38716,axiom,(
    cell52 != cell25 )).

fof(tlhfof38717,axiom,(
    cell52 != cell24 )).

fof(tlhfof38718,axiom,(
    cell52 != cell23 )).

fof(tlhfof38719,axiom,(
    cell52 != cell22 )).

fof(tlhfof38720,axiom,(
    cell52 != cell21 )).

fof(tlhfof38721,axiom,(
    cell52 != cell10 )).

fof(tlhfof38722,axiom,(
    cell52 != cell9 )).

fof(tlhfof38723,axiom,(
    cell52 != cell19 )).

fof(tlhfof38724,axiom,(
    cell52 != cell8 )).

fof(tlhfof38725,axiom,(
    cell52 != cell18 )).

fof(tlhfof38726,axiom,(
    cell52 != cell7 )).

fof(tlhfof38727,axiom,(
    cell52 != cell17 )).

fof(tlhfof38728,axiom,(
    cell52 != cell6 )).

fof(tlhfof38729,axiom,(
    cell52 != cell16 )).

fof(tlhfof38730,axiom,(
    cell52 != cell5 )).

fof(tlhfof38731,axiom,(
    cell52 != cell15 )).

fof(tlhfof38732,axiom,(
    cell52 != cell4 )).

fof(tlhfof38733,axiom,(
    cell52 != cell14 )).

fof(tlhfof38734,axiom,(
    cell52 != cell3 )).

fof(tlhfof38735,axiom,(
    cell52 != cell13 )).

fof(tlhfof38736,axiom,(
    cell52 != cell2 )).

fof(tlhfof38737,axiom,(
    cell52 != cell12 )).

fof(tlhfof38738,axiom,(
    cell52 != cell1 )).

fof(tlhfof38739,axiom,(
    cell52 != cell11 )).

fof(tlhfof38740,axiom,(
    cell52 != cell20 )).

fof(tlhfof38741,axiom,(
    cell51 != cell50 )).

fof(tlhfof38742,axiom,(
    cell51 != cell49 )).

fof(tlhfof38743,axiom,(
    cell51 != cell48 )).

fof(tlhfof38744,axiom,(
    cell51 != cell47 )).

fof(tlhfof38745,axiom,(
    cell51 != cell46 )).

fof(tlhfof38746,axiom,(
    cell51 != cell45 )).

fof(tlhfof38747,axiom,(
    cell51 != cell44 )).

fof(tlhfof38748,axiom,(
    cell51 != cell43 )).

fof(tlhfof38749,axiom,(
    cell51 != cell42 )).

fof(tlhfof38750,axiom,(
    cell51 != cell41 )).

fof(tlhfof38751,axiom,(
    cell51 != cell40 )).

fof(tlhfof38752,axiom,(
    cell51 != cell39 )).

fof(tlhfof38753,axiom,(
    cell51 != cell38 )).

fof(tlhfof38754,axiom,(
    cell51 != cell37 )).

fof(tlhfof38755,axiom,(
    cell51 != cell36 )).

fof(tlhfof38756,axiom,(
    cell51 != cell35 )).

fof(tlhfof38757,axiom,(
    cell51 != cell34 )).

fof(tlhfof38758,axiom,(
    cell51 != cell33 )).

fof(tlhfof38759,axiom,(
    cell51 != cell32 )).

fof(tlhfof38760,axiom,(
    cell51 != cell31 )).

fof(tlhfof38761,axiom,(
    cell51 != cell30 )).

fof(tlhfof38762,axiom,(
    cell51 != cell29 )).

fof(tlhfof38763,axiom,(
    cell51 != cell28 )).

fof(tlhfof38764,axiom,(
    cell51 != cell27 )).

fof(tlhfof38765,axiom,(
    cell51 != cell26 )).

fof(tlhfof38766,axiom,(
    cell51 != cell25 )).

fof(tlhfof38767,axiom,(
    cell51 != cell24 )).

fof(tlhfof38768,axiom,(
    cell51 != cell23 )).

fof(tlhfof38769,axiom,(
    cell51 != cell22 )).

fof(tlhfof38770,axiom,(
    cell51 != cell21 )).

fof(tlhfof38771,axiom,(
    cell51 != cell10 )).

fof(tlhfof38772,axiom,(
    cell51 != cell9 )).

fof(tlhfof38773,axiom,(
    cell51 != cell19 )).

fof(tlhfof38774,axiom,(
    cell51 != cell8 )).

fof(tlhfof38775,axiom,(
    cell51 != cell18 )).

fof(tlhfof38776,axiom,(
    cell51 != cell7 )).

fof(tlhfof38777,axiom,(
    cell51 != cell17 )).

fof(tlhfof38778,axiom,(
    cell51 != cell6 )).

fof(tlhfof38779,axiom,(
    cell51 != cell16 )).

fof(tlhfof38780,axiom,(
    cell51 != cell5 )).

fof(tlhfof38781,axiom,(
    cell51 != cell15 )).

fof(tlhfof38782,axiom,(
    cell51 != cell4 )).

fof(tlhfof38783,axiom,(
    cell51 != cell14 )).

fof(tlhfof38784,axiom,(
    cell51 != cell3 )).

fof(tlhfof38785,axiom,(
    cell51 != cell13 )).

fof(tlhfof38786,axiom,(
    cell51 != cell2 )).

fof(tlhfof38787,axiom,(
    cell51 != cell12 )).

fof(tlhfof38788,axiom,(
    cell51 != cell1 )).

fof(tlhfof38789,axiom,(
    cell51 != cell11 )).

fof(tlhfof38790,axiom,(
    cell51 != cell20 )).

fof(tlhfof38791,axiom,(
    cell50 != cell49 )).

fof(tlhfof38792,axiom,(
    cell50 != cell48 )).

fof(tlhfof38793,axiom,(
    cell50 != cell47 )).

fof(tlhfof38794,axiom,(
    cell50 != cell46 )).

fof(tlhfof38795,axiom,(
    cell50 != cell45 )).

fof(tlhfof38796,axiom,(
    cell50 != cell44 )).

fof(tlhfof38797,axiom,(
    cell50 != cell43 )).

fof(tlhfof38798,axiom,(
    cell50 != cell42 )).

fof(tlhfof38799,axiom,(
    cell50 != cell41 )).

fof(tlhfof38800,axiom,(
    cell50 != cell40 )).

fof(tlhfof38801,axiom,(
    cell50 != cell39 )).

fof(tlhfof38802,axiom,(
    cell50 != cell38 )).

fof(tlhfof38803,axiom,(
    cell50 != cell37 )).

fof(tlhfof38804,axiom,(
    cell50 != cell36 )).

fof(tlhfof38805,axiom,(
    cell50 != cell35 )).

fof(tlhfof38806,axiom,(
    cell50 != cell34 )).

fof(tlhfof38807,axiom,(
    cell50 != cell33 )).

fof(tlhfof38808,axiom,(
    cell50 != cell32 )).

fof(tlhfof38809,axiom,(
    cell50 != cell31 )).

fof(tlhfof38810,axiom,(
    cell50 != cell30 )).

fof(tlhfof38811,axiom,(
    cell50 != cell29 )).

fof(tlhfof38812,axiom,(
    cell50 != cell28 )).

fof(tlhfof38813,axiom,(
    cell50 != cell27 )).

fof(tlhfof38814,axiom,(
    cell50 != cell26 )).

fof(tlhfof38815,axiom,(
    cell50 != cell25 )).

fof(tlhfof38816,axiom,(
    cell50 != cell24 )).

fof(tlhfof38817,axiom,(
    cell50 != cell23 )).

fof(tlhfof38818,axiom,(
    cell50 != cell22 )).

fof(tlhfof38819,axiom,(
    cell50 != cell21 )).

fof(tlhfof38820,axiom,(
    cell50 != cell10 )).

fof(tlhfof38821,axiom,(
    cell50 != cell9 )).

fof(tlhfof38822,axiom,(
    cell50 != cell19 )).

fof(tlhfof38823,axiom,(
    cell50 != cell8 )).

fof(tlhfof38824,axiom,(
    cell50 != cell18 )).

fof(tlhfof38825,axiom,(
    cell50 != cell7 )).

fof(tlhfof38826,axiom,(
    cell50 != cell17 )).

fof(tlhfof38827,axiom,(
    cell50 != cell6 )).

fof(tlhfof38828,axiom,(
    cell50 != cell16 )).

fof(tlhfof38829,axiom,(
    cell50 != cell5 )).

fof(tlhfof38830,axiom,(
    cell50 != cell15 )).

fof(tlhfof38831,axiom,(
    cell50 != cell4 )).

fof(tlhfof38832,axiom,(
    cell50 != cell14 )).

fof(tlhfof38833,axiom,(
    cell50 != cell3 )).

fof(tlhfof38834,axiom,(
    cell50 != cell13 )).

fof(tlhfof38835,axiom,(
    cell50 != cell2 )).

fof(tlhfof38836,axiom,(
    cell50 != cell12 )).

fof(tlhfof38837,axiom,(
    cell50 != cell1 )).

fof(tlhfof38838,axiom,(
    cell50 != cell11 )).

fof(tlhfof38839,axiom,(
    cell50 != cell20 )).

fof(tlhfof38840,axiom,(
    cell49 != cell48 )).

fof(tlhfof38841,axiom,(
    cell49 != cell47 )).

fof(tlhfof38842,axiom,(
    cell49 != cell46 )).

fof(tlhfof38843,axiom,(
    cell49 != cell45 )).

fof(tlhfof38844,axiom,(
    cell49 != cell44 )).

fof(tlhfof38845,axiom,(
    cell49 != cell43 )).

fof(tlhfof38846,axiom,(
    cell49 != cell42 )).

fof(tlhfof38847,axiom,(
    cell49 != cell41 )).

fof(tlhfof38848,axiom,(
    cell49 != cell40 )).

fof(tlhfof38849,axiom,(
    cell49 != cell39 )).

fof(tlhfof38850,axiom,(
    cell49 != cell38 )).

fof(tlhfof38851,axiom,(
    cell49 != cell37 )).

fof(tlhfof38852,axiom,(
    cell49 != cell36 )).

fof(tlhfof38853,axiom,(
    cell49 != cell35 )).

fof(tlhfof38854,axiom,(
    cell49 != cell34 )).

fof(tlhfof38855,axiom,(
    cell49 != cell33 )).

fof(tlhfof38856,axiom,(
    cell49 != cell32 )).

fof(tlhfof38857,axiom,(
    cell49 != cell31 )).

fof(tlhfof38858,axiom,(
    cell49 != cell30 )).

fof(tlhfof38859,axiom,(
    cell49 != cell29 )).

fof(tlhfof38860,axiom,(
    cell49 != cell28 )).

fof(tlhfof38861,axiom,(
    cell49 != cell27 )).

fof(tlhfof38862,axiom,(
    cell49 != cell26 )).

fof(tlhfof38863,axiom,(
    cell49 != cell25 )).

fof(tlhfof38864,axiom,(
    cell49 != cell24 )).

fof(tlhfof38865,axiom,(
    cell49 != cell23 )).

fof(tlhfof38866,axiom,(
    cell49 != cell22 )).

fof(tlhfof38867,axiom,(
    cell49 != cell21 )).

fof(tlhfof38868,axiom,(
    cell49 != cell10 )).

fof(tlhfof38869,axiom,(
    cell49 != cell9 )).

fof(tlhfof38870,axiom,(
    cell49 != cell19 )).

fof(tlhfof38871,axiom,(
    cell49 != cell8 )).

fof(tlhfof38872,axiom,(
    cell49 != cell18 )).

fof(tlhfof38873,axiom,(
    cell49 != cell7 )).

fof(tlhfof38874,axiom,(
    cell49 != cell17 )).

fof(tlhfof38875,axiom,(
    cell49 != cell6 )).

fof(tlhfof38876,axiom,(
    cell49 != cell16 )).

fof(tlhfof38877,axiom,(
    cell49 != cell5 )).

fof(tlhfof38878,axiom,(
    cell49 != cell15 )).

fof(tlhfof38879,axiom,(
    cell49 != cell4 )).

fof(tlhfof38880,axiom,(
    cell49 != cell14 )).

fof(tlhfof38881,axiom,(
    cell49 != cell3 )).

fof(tlhfof38882,axiom,(
    cell49 != cell13 )).

fof(tlhfof38883,axiom,(
    cell49 != cell2 )).

fof(tlhfof38884,axiom,(
    cell49 != cell12 )).

fof(tlhfof38885,axiom,(
    cell49 != cell1 )).

fof(tlhfof38886,axiom,(
    cell49 != cell11 )).

fof(tlhfof38887,axiom,(
    cell49 != cell20 )).

fof(tlhfof38888,axiom,(
    cell48 != cell47 )).

fof(tlhfof38889,axiom,(
    cell48 != cell46 )).

fof(tlhfof38890,axiom,(
    cell48 != cell45 )).

fof(tlhfof38891,axiom,(
    cell48 != cell44 )).

fof(tlhfof38892,axiom,(
    cell48 != cell43 )).

fof(tlhfof38893,axiom,(
    cell48 != cell42 )).

fof(tlhfof38894,axiom,(
    cell48 != cell41 )).

fof(tlhfof38895,axiom,(
    cell48 != cell40 )).

fof(tlhfof38896,axiom,(
    cell48 != cell39 )).

fof(tlhfof38897,axiom,(
    cell48 != cell38 )).

fof(tlhfof38898,axiom,(
    cell48 != cell37 )).

fof(tlhfof38899,axiom,(
    cell48 != cell36 )).

fof(tlhfof38900,axiom,(
    cell48 != cell35 )).

fof(tlhfof38901,axiom,(
    cell48 != cell34 )).

fof(tlhfof38902,axiom,(
    cell48 != cell33 )).

fof(tlhfof38903,axiom,(
    cell48 != cell32 )).

fof(tlhfof38904,axiom,(
    cell48 != cell31 )).

fof(tlhfof38905,axiom,(
    cell48 != cell30 )).

fof(tlhfof38906,axiom,(
    cell48 != cell29 )).

fof(tlhfof38907,axiom,(
    cell48 != cell28 )).

fof(tlhfof38908,axiom,(
    cell48 != cell27 )).

fof(tlhfof38909,axiom,(
    cell48 != cell26 )).

fof(tlhfof38910,axiom,(
    cell48 != cell25 )).

fof(tlhfof38911,axiom,(
    cell48 != cell24 )).

fof(tlhfof38912,axiom,(
    cell48 != cell23 )).

fof(tlhfof38913,axiom,(
    cell48 != cell22 )).

fof(tlhfof38914,axiom,(
    cell48 != cell21 )).

fof(tlhfof38915,axiom,(
    cell48 != cell10 )).

fof(tlhfof38916,axiom,(
    cell48 != cell9 )).

fof(tlhfof38917,axiom,(
    cell48 != cell19 )).

fof(tlhfof38918,axiom,(
    cell48 != cell8 )).

fof(tlhfof38919,axiom,(
    cell48 != cell18 )).

fof(tlhfof38920,axiom,(
    cell48 != cell7 )).

fof(tlhfof38921,axiom,(
    cell48 != cell17 )).

fof(tlhfof38922,axiom,(
    cell48 != cell6 )).

fof(tlhfof38923,axiom,(
    cell48 != cell16 )).

fof(tlhfof38924,axiom,(
    cell48 != cell5 )).

fof(tlhfof38925,axiom,(
    cell48 != cell15 )).

fof(tlhfof38926,axiom,(
    cell48 != cell4 )).

fof(tlhfof38927,axiom,(
    cell48 != cell14 )).

fof(tlhfof38928,axiom,(
    cell48 != cell3 )).

fof(tlhfof38929,axiom,(
    cell48 != cell13 )).

fof(tlhfof38930,axiom,(
    cell48 != cell2 )).

fof(tlhfof38931,axiom,(
    cell48 != cell12 )).

fof(tlhfof38932,axiom,(
    cell48 != cell1 )).

fof(tlhfof38933,axiom,(
    cell48 != cell11 )).

fof(tlhfof38934,axiom,(
    cell48 != cell20 )).

fof(tlhfof38935,axiom,(
    cell47 != cell46 )).

fof(tlhfof38936,axiom,(
    cell47 != cell45 )).

fof(tlhfof38937,axiom,(
    cell47 != cell44 )).

fof(tlhfof38938,axiom,(
    cell47 != cell43 )).

fof(tlhfof38939,axiom,(
    cell47 != cell42 )).

fof(tlhfof38940,axiom,(
    cell47 != cell41 )).

fof(tlhfof38941,axiom,(
    cell47 != cell40 )).

fof(tlhfof38942,axiom,(
    cell47 != cell39 )).

fof(tlhfof38943,axiom,(
    cell47 != cell38 )).

fof(tlhfof38944,axiom,(
    cell47 != cell37 )).

fof(tlhfof38945,axiom,(
    cell47 != cell36 )).

fof(tlhfof38946,axiom,(
    cell47 != cell35 )).

fof(tlhfof38947,axiom,(
    cell47 != cell34 )).

fof(tlhfof38948,axiom,(
    cell47 != cell33 )).

fof(tlhfof38949,axiom,(
    cell47 != cell32 )).

fof(tlhfof38950,axiom,(
    cell47 != cell31 )).

fof(tlhfof38951,axiom,(
    cell47 != cell30 )).

fof(tlhfof38952,axiom,(
    cell47 != cell29 )).

fof(tlhfof38953,axiom,(
    cell47 != cell28 )).

fof(tlhfof38954,axiom,(
    cell47 != cell27 )).

fof(tlhfof38955,axiom,(
    cell47 != cell26 )).

fof(tlhfof38956,axiom,(
    cell47 != cell25 )).

fof(tlhfof38957,axiom,(
    cell47 != cell24 )).

fof(tlhfof38958,axiom,(
    cell47 != cell23 )).

fof(tlhfof38959,axiom,(
    cell47 != cell22 )).

fof(tlhfof38960,axiom,(
    cell47 != cell21 )).

fof(tlhfof38961,axiom,(
    cell47 != cell10 )).

fof(tlhfof38962,axiom,(
    cell47 != cell9 )).

fof(tlhfof38963,axiom,(
    cell47 != cell19 )).

fof(tlhfof38964,axiom,(
    cell47 != cell8 )).

fof(tlhfof38965,axiom,(
    cell47 != cell18 )).

fof(tlhfof38966,axiom,(
    cell47 != cell7 )).

fof(tlhfof38967,axiom,(
    cell47 != cell17 )).

fof(tlhfof38968,axiom,(
    cell47 != cell6 )).

fof(tlhfof38969,axiom,(
    cell47 != cell16 )).

fof(tlhfof38970,axiom,(
    cell47 != cell5 )).

fof(tlhfof38971,axiom,(
    cell47 != cell15 )).

fof(tlhfof38972,axiom,(
    cell47 != cell4 )).

fof(tlhfof38973,axiom,(
    cell47 != cell14 )).

fof(tlhfof38974,axiom,(
    cell47 != cell3 )).

fof(tlhfof38975,axiom,(
    cell47 != cell13 )).

fof(tlhfof38976,axiom,(
    cell47 != cell2 )).

fof(tlhfof38977,axiom,(
    cell47 != cell12 )).

fof(tlhfof38978,axiom,(
    cell47 != cell1 )).

fof(tlhfof38979,axiom,(
    cell47 != cell11 )).

fof(tlhfof38980,axiom,(
    cell47 != cell20 )).

fof(tlhfof38981,axiom,(
    cell46 != cell45 )).

fof(tlhfof38982,axiom,(
    cell46 != cell44 )).

fof(tlhfof38983,axiom,(
    cell46 != cell43 )).

fof(tlhfof38984,axiom,(
    cell46 != cell42 )).

fof(tlhfof38985,axiom,(
    cell46 != cell41 )).

fof(tlhfof38986,axiom,(
    cell46 != cell40 )).

fof(tlhfof38987,axiom,(
    cell46 != cell39 )).

fof(tlhfof38988,axiom,(
    cell46 != cell38 )).

fof(tlhfof38989,axiom,(
    cell46 != cell37 )).

fof(tlhfof38990,axiom,(
    cell46 != cell36 )).

fof(tlhfof38991,axiom,(
    cell46 != cell35 )).

fof(tlhfof38992,axiom,(
    cell46 != cell34 )).

fof(tlhfof38993,axiom,(
    cell46 != cell33 )).

fof(tlhfof38994,axiom,(
    cell46 != cell32 )).

fof(tlhfof38995,axiom,(
    cell46 != cell31 )).

fof(tlhfof38996,axiom,(
    cell46 != cell30 )).

fof(tlhfof38997,axiom,(
    cell46 != cell29 )).

fof(tlhfof38998,axiom,(
    cell46 != cell28 )).

fof(tlhfof38999,axiom,(
    cell46 != cell27 )).

fof(tlhfof39000,axiom,(
    cell46 != cell26 )).

fof(tlhfof39001,axiom,(
    cell46 != cell25 )).

fof(tlhfof39002,axiom,(
    cell46 != cell24 )).

fof(tlhfof39003,axiom,(
    cell46 != cell23 )).

fof(tlhfof39004,axiom,(
    cell46 != cell22 )).

fof(tlhfof39005,axiom,(
    cell46 != cell21 )).

fof(tlhfof39006,axiom,(
    cell46 != cell10 )).

fof(tlhfof39007,axiom,(
    cell46 != cell9 )).

fof(tlhfof39008,axiom,(
    cell46 != cell19 )).

fof(tlhfof39009,axiom,(
    cell46 != cell8 )).

fof(tlhfof39010,axiom,(
    cell46 != cell18 )).

fof(tlhfof39011,axiom,(
    cell46 != cell7 )).

fof(tlhfof39012,axiom,(
    cell46 != cell17 )).

fof(tlhfof39013,axiom,(
    cell46 != cell6 )).

fof(tlhfof39014,axiom,(
    cell46 != cell16 )).

fof(tlhfof39015,axiom,(
    cell46 != cell5 )).

fof(tlhfof39016,axiom,(
    cell46 != cell15 )).

fof(tlhfof39017,axiom,(
    cell46 != cell4 )).

fof(tlhfof39018,axiom,(
    cell46 != cell14 )).

fof(tlhfof39019,axiom,(
    cell46 != cell3 )).

fof(tlhfof39020,axiom,(
    cell46 != cell13 )).

fof(tlhfof39021,axiom,(
    cell46 != cell2 )).

fof(tlhfof39022,axiom,(
    cell46 != cell12 )).

fof(tlhfof39023,axiom,(
    cell46 != cell1 )).

fof(tlhfof39024,axiom,(
    cell46 != cell11 )).

fof(tlhfof39025,axiom,(
    cell46 != cell20 )).

fof(tlhfof39026,axiom,(
    cell45 != cell44 )).

fof(tlhfof39027,axiom,(
    cell45 != cell43 )).

fof(tlhfof39028,axiom,(
    cell45 != cell42 )).

fof(tlhfof39029,axiom,(
    cell45 != cell41 )).

fof(tlhfof39030,axiom,(
    cell45 != cell40 )).

fof(tlhfof39031,axiom,(
    cell45 != cell39 )).

fof(tlhfof39032,axiom,(
    cell45 != cell38 )).

fof(tlhfof39033,axiom,(
    cell45 != cell37 )).

fof(tlhfof39034,axiom,(
    cell45 != cell36 )).

fof(tlhfof39035,axiom,(
    cell45 != cell35 )).

fof(tlhfof39036,axiom,(
    cell45 != cell34 )).

fof(tlhfof39037,axiom,(
    cell45 != cell33 )).

fof(tlhfof39038,axiom,(
    cell45 != cell32 )).

fof(tlhfof39039,axiom,(
    cell45 != cell31 )).

fof(tlhfof39040,axiom,(
    cell45 != cell30 )).

fof(tlhfof39041,axiom,(
    cell45 != cell29 )).

fof(tlhfof39042,axiom,(
    cell45 != cell28 )).

fof(tlhfof39043,axiom,(
    cell45 != cell27 )).

fof(tlhfof39044,axiom,(
    cell45 != cell26 )).

fof(tlhfof39045,axiom,(
    cell45 != cell25 )).

fof(tlhfof39046,axiom,(
    cell45 != cell24 )).

fof(tlhfof39047,axiom,(
    cell45 != cell23 )).

fof(tlhfof39048,axiom,(
    cell45 != cell22 )).

fof(tlhfof39049,axiom,(
    cell45 != cell21 )).

fof(tlhfof39050,axiom,(
    cell45 != cell10 )).

fof(tlhfof39051,axiom,(
    cell45 != cell9 )).

fof(tlhfof39052,axiom,(
    cell45 != cell19 )).

fof(tlhfof39053,axiom,(
    cell45 != cell8 )).

fof(tlhfof39054,axiom,(
    cell45 != cell18 )).

fof(tlhfof39055,axiom,(
    cell45 != cell7 )).

fof(tlhfof39056,axiom,(
    cell45 != cell17 )).

fof(tlhfof39057,axiom,(
    cell45 != cell6 )).

fof(tlhfof39058,axiom,(
    cell45 != cell16 )).

fof(tlhfof39059,axiom,(
    cell45 != cell5 )).

fof(tlhfof39060,axiom,(
    cell45 != cell15 )).

fof(tlhfof39061,axiom,(
    cell45 != cell4 )).

fof(tlhfof39062,axiom,(
    cell45 != cell14 )).

fof(tlhfof39063,axiom,(
    cell45 != cell3 )).

fof(tlhfof39064,axiom,(
    cell45 != cell13 )).

fof(tlhfof39065,axiom,(
    cell45 != cell2 )).

fof(tlhfof39066,axiom,(
    cell45 != cell12 )).

fof(tlhfof39067,axiom,(
    cell45 != cell1 )).

fof(tlhfof39068,axiom,(
    cell45 != cell11 )).

fof(tlhfof39069,axiom,(
    cell45 != cell20 )).

fof(tlhfof39070,axiom,(
    cell44 != cell43 )).

fof(tlhfof39071,axiom,(
    cell44 != cell42 )).

fof(tlhfof39072,axiom,(
    cell44 != cell41 )).

fof(tlhfof39073,axiom,(
    cell44 != cell40 )).

fof(tlhfof39074,axiom,(
    cell44 != cell39 )).

fof(tlhfof39075,axiom,(
    cell44 != cell38 )).

fof(tlhfof39076,axiom,(
    cell44 != cell37 )).

fof(tlhfof39077,axiom,(
    cell44 != cell36 )).

fof(tlhfof39078,axiom,(
    cell44 != cell35 )).

fof(tlhfof39079,axiom,(
    cell44 != cell34 )).

fof(tlhfof39080,axiom,(
    cell44 != cell33 )).

fof(tlhfof39081,axiom,(
    cell44 != cell32 )).

fof(tlhfof39082,axiom,(
    cell44 != cell31 )).

fof(tlhfof39083,axiom,(
    cell44 != cell30 )).

fof(tlhfof39084,axiom,(
    cell44 != cell29 )).

fof(tlhfof39085,axiom,(
    cell44 != cell28 )).

fof(tlhfof39086,axiom,(
    cell44 != cell27 )).

fof(tlhfof39087,axiom,(
    cell44 != cell26 )).

fof(tlhfof39088,axiom,(
    cell44 != cell25 )).

fof(tlhfof39089,axiom,(
    cell44 != cell24 )).

fof(tlhfof39090,axiom,(
    cell44 != cell23 )).

fof(tlhfof39091,axiom,(
    cell44 != cell22 )).

fof(tlhfof39092,axiom,(
    cell44 != cell21 )).

fof(tlhfof39093,axiom,(
    cell44 != cell10 )).

fof(tlhfof39094,axiom,(
    cell44 != cell9 )).

fof(tlhfof39095,axiom,(
    cell44 != cell19 )).

fof(tlhfof39096,axiom,(
    cell44 != cell8 )).

fof(tlhfof39097,axiom,(
    cell44 != cell18 )).

fof(tlhfof39098,axiom,(
    cell44 != cell7 )).

fof(tlhfof39099,axiom,(
    cell44 != cell17 )).

fof(tlhfof39100,axiom,(
    cell44 != cell6 )).

fof(tlhfof39101,axiom,(
    cell44 != cell16 )).

fof(tlhfof39102,axiom,(
    cell44 != cell5 )).

fof(tlhfof39103,axiom,(
    cell44 != cell15 )).

fof(tlhfof39104,axiom,(
    cell44 != cell4 )).

fof(tlhfof39105,axiom,(
    cell44 != cell14 )).

fof(tlhfof39106,axiom,(
    cell44 != cell3 )).

fof(tlhfof39107,axiom,(
    cell44 != cell13 )).

fof(tlhfof39108,axiom,(
    cell44 != cell2 )).

fof(tlhfof39109,axiom,(
    cell44 != cell12 )).

fof(tlhfof39110,axiom,(
    cell44 != cell1 )).

fof(tlhfof39111,axiom,(
    cell44 != cell11 )).

fof(tlhfof39112,axiom,(
    cell44 != cell20 )).

fof(tlhfof39113,axiom,(
    cell43 != cell42 )).

fof(tlhfof39114,axiom,(
    cell43 != cell41 )).

fof(tlhfof39115,axiom,(
    cell43 != cell40 )).

fof(tlhfof39116,axiom,(
    cell43 != cell39 )).

fof(tlhfof39117,axiom,(
    cell43 != cell38 )).

fof(tlhfof39118,axiom,(
    cell43 != cell37 )).

fof(tlhfof39119,axiom,(
    cell43 != cell36 )).

fof(tlhfof39120,axiom,(
    cell43 != cell35 )).

fof(tlhfof39121,axiom,(
    cell43 != cell34 )).

fof(tlhfof39122,axiom,(
    cell43 != cell33 )).

fof(tlhfof39123,axiom,(
    cell43 != cell32 )).

fof(tlhfof39124,axiom,(
    cell43 != cell31 )).

fof(tlhfof39125,axiom,(
    cell43 != cell30 )).

fof(tlhfof39126,axiom,(
    cell43 != cell29 )).

fof(tlhfof39127,axiom,(
    cell43 != cell28 )).

fof(tlhfof39128,axiom,(
    cell43 != cell27 )).

fof(tlhfof39129,axiom,(
    cell43 != cell26 )).

fof(tlhfof39130,axiom,(
    cell43 != cell25 )).

fof(tlhfof39131,axiom,(
    cell43 != cell24 )).

fof(tlhfof39132,axiom,(
    cell43 != cell23 )).

fof(tlhfof39133,axiom,(
    cell43 != cell22 )).

fof(tlhfof39134,axiom,(
    cell43 != cell21 )).

fof(tlhfof39135,axiom,(
    cell43 != cell10 )).

fof(tlhfof39136,axiom,(
    cell43 != cell9 )).

fof(tlhfof39137,axiom,(
    cell43 != cell19 )).

fof(tlhfof39138,axiom,(
    cell43 != cell8 )).

fof(tlhfof39139,axiom,(
    cell43 != cell18 )).

fof(tlhfof39140,axiom,(
    cell43 != cell7 )).

fof(tlhfof39141,axiom,(
    cell43 != cell17 )).

fof(tlhfof39142,axiom,(
    cell43 != cell6 )).

fof(tlhfof39143,axiom,(
    cell43 != cell16 )).

fof(tlhfof39144,axiom,(
    cell43 != cell5 )).

fof(tlhfof39145,axiom,(
    cell43 != cell15 )).

fof(tlhfof39146,axiom,(
    cell43 != cell4 )).

fof(tlhfof39147,axiom,(
    cell43 != cell14 )).

fof(tlhfof39148,axiom,(
    cell43 != cell3 )).

fof(tlhfof39149,axiom,(
    cell43 != cell13 )).

fof(tlhfof39150,axiom,(
    cell43 != cell2 )).

fof(tlhfof39151,axiom,(
    cell43 != cell12 )).

fof(tlhfof39152,axiom,(
    cell43 != cell1 )).

fof(tlhfof39153,axiom,(
    cell43 != cell11 )).

fof(tlhfof39154,axiom,(
    cell43 != cell20 )).

fof(tlhfof39155,axiom,(
    cell42 != cell41 )).

fof(tlhfof39156,axiom,(
    cell42 != cell40 )).

fof(tlhfof39157,axiom,(
    cell42 != cell39 )).

fof(tlhfof39158,axiom,(
    cell42 != cell38 )).

fof(tlhfof39159,axiom,(
    cell42 != cell37 )).

fof(tlhfof39160,axiom,(
    cell42 != cell36 )).

fof(tlhfof39161,axiom,(
    cell42 != cell35 )).

fof(tlhfof39162,axiom,(
    cell42 != cell34 )).

fof(tlhfof39163,axiom,(
    cell42 != cell33 )).

fof(tlhfof39164,axiom,(
    cell42 != cell32 )).

fof(tlhfof39165,axiom,(
    cell42 != cell31 )).

fof(tlhfof39166,axiom,(
    cell42 != cell30 )).

fof(tlhfof39167,axiom,(
    cell42 != cell29 )).

fof(tlhfof39168,axiom,(
    cell42 != cell28 )).

fof(tlhfof39169,axiom,(
    cell42 != cell27 )).

fof(tlhfof39170,axiom,(
    cell42 != cell26 )).

fof(tlhfof39171,axiom,(
    cell42 != cell25 )).

fof(tlhfof39172,axiom,(
    cell42 != cell24 )).

fof(tlhfof39173,axiom,(
    cell42 != cell23 )).

fof(tlhfof39174,axiom,(
    cell42 != cell22 )).

fof(tlhfof39175,axiom,(
    cell42 != cell21 )).

fof(tlhfof39176,axiom,(
    cell42 != cell10 )).

fof(tlhfof39177,axiom,(
    cell42 != cell9 )).

fof(tlhfof39178,axiom,(
    cell42 != cell19 )).

fof(tlhfof39179,axiom,(
    cell42 != cell8 )).

fof(tlhfof39180,axiom,(
    cell42 != cell18 )).

fof(tlhfof39181,axiom,(
    cell42 != cell7 )).

fof(tlhfof39182,axiom,(
    cell42 != cell17 )).

fof(tlhfof39183,axiom,(
    cell42 != cell6 )).

fof(tlhfof39184,axiom,(
    cell42 != cell16 )).

fof(tlhfof39185,axiom,(
    cell42 != cell5 )).

fof(tlhfof39186,axiom,(
    cell42 != cell15 )).

fof(tlhfof39187,axiom,(
    cell42 != cell4 )).

fof(tlhfof39188,axiom,(
    cell42 != cell14 )).

fof(tlhfof39189,axiom,(
    cell42 != cell3 )).

fof(tlhfof39190,axiom,(
    cell42 != cell13 )).

fof(tlhfof39191,axiom,(
    cell42 != cell2 )).

fof(tlhfof39192,axiom,(
    cell42 != cell12 )).

fof(tlhfof39193,axiom,(
    cell42 != cell1 )).

fof(tlhfof39194,axiom,(
    cell42 != cell11 )).

fof(tlhfof39195,axiom,(
    cell42 != cell20 )).

fof(tlhfof39196,axiom,(
    cell41 != cell40 )).

fof(tlhfof39197,axiom,(
    cell41 != cell39 )).

fof(tlhfof39198,axiom,(
    cell41 != cell38 )).

fof(tlhfof39199,axiom,(
    cell41 != cell37 )).

fof(tlhfof39200,axiom,(
    cell41 != cell36 )).

fof(tlhfof39201,axiom,(
    cell41 != cell35 )).

fof(tlhfof39202,axiom,(
    cell41 != cell34 )).

fof(tlhfof39203,axiom,(
    cell41 != cell33 )).

fof(tlhfof39204,axiom,(
    cell41 != cell32 )).

fof(tlhfof39205,axiom,(
    cell41 != cell31 )).

fof(tlhfof39206,axiom,(
    cell41 != cell30 )).

fof(tlhfof39207,axiom,(
    cell41 != cell29 )).

fof(tlhfof39208,axiom,(
    cell41 != cell28 )).

fof(tlhfof39209,axiom,(
    cell41 != cell27 )).

fof(tlhfof39210,axiom,(
    cell41 != cell26 )).

fof(tlhfof39211,axiom,(
    cell41 != cell25 )).

fof(tlhfof39212,axiom,(
    cell41 != cell24 )).

fof(tlhfof39213,axiom,(
    cell41 != cell23 )).

fof(tlhfof39214,axiom,(
    cell41 != cell22 )).

fof(tlhfof39215,axiom,(
    cell41 != cell21 )).

fof(tlhfof39216,axiom,(
    cell41 != cell10 )).

fof(tlhfof39217,axiom,(
    cell41 != cell9 )).

fof(tlhfof39218,axiom,(
    cell41 != cell19 )).

fof(tlhfof39219,axiom,(
    cell41 != cell8 )).

fof(tlhfof39220,axiom,(
    cell41 != cell18 )).

fof(tlhfof39221,axiom,(
    cell41 != cell7 )).

fof(tlhfof39222,axiom,(
    cell41 != cell17 )).

fof(tlhfof39223,axiom,(
    cell41 != cell6 )).

fof(tlhfof39224,axiom,(
    cell41 != cell16 )).

fof(tlhfof39225,axiom,(
    cell41 != cell5 )).

fof(tlhfof39226,axiom,(
    cell41 != cell15 )).

fof(tlhfof39227,axiom,(
    cell41 != cell4 )).

fof(tlhfof39228,axiom,(
    cell41 != cell14 )).

fof(tlhfof39229,axiom,(
    cell41 != cell3 )).

fof(tlhfof39230,axiom,(
    cell41 != cell13 )).

fof(tlhfof39231,axiom,(
    cell41 != cell2 )).

fof(tlhfof39232,axiom,(
    cell41 != cell12 )).

fof(tlhfof39233,axiom,(
    cell41 != cell1 )).

fof(tlhfof39234,axiom,(
    cell41 != cell11 )).

fof(tlhfof39235,axiom,(
    cell41 != cell20 )).

fof(tlhfof39236,axiom,(
    cell40 != cell39 )).

fof(tlhfof39237,axiom,(
    cell40 != cell38 )).

fof(tlhfof39238,axiom,(
    cell40 != cell37 )).

fof(tlhfof39239,axiom,(
    cell40 != cell36 )).

fof(tlhfof39240,axiom,(
    cell40 != cell35 )).

fof(tlhfof39241,axiom,(
    cell40 != cell34 )).

fof(tlhfof39242,axiom,(
    cell40 != cell33 )).

fof(tlhfof39243,axiom,(
    cell40 != cell32 )).

fof(tlhfof39244,axiom,(
    cell40 != cell31 )).

fof(tlhfof39245,axiom,(
    cell40 != cell30 )).

fof(tlhfof39246,axiom,(
    cell40 != cell29 )).

fof(tlhfof39247,axiom,(
    cell40 != cell28 )).

fof(tlhfof39248,axiom,(
    cell40 != cell27 )).

fof(tlhfof39249,axiom,(
    cell40 != cell26 )).

fof(tlhfof39250,axiom,(
    cell40 != cell25 )).

fof(tlhfof39251,axiom,(
    cell40 != cell24 )).

fof(tlhfof39252,axiom,(
    cell40 != cell23 )).

fof(tlhfof39253,axiom,(
    cell40 != cell22 )).

fof(tlhfof39254,axiom,(
    cell40 != cell21 )).

fof(tlhfof39255,axiom,(
    cell40 != cell10 )).

fof(tlhfof39256,axiom,(
    cell40 != cell9 )).

fof(tlhfof39257,axiom,(
    cell40 != cell19 )).

fof(tlhfof39258,axiom,(
    cell40 != cell8 )).

fof(tlhfof39259,axiom,(
    cell40 != cell18 )).

fof(tlhfof39260,axiom,(
    cell40 != cell7 )).

fof(tlhfof39261,axiom,(
    cell40 != cell17 )).

fof(tlhfof39262,axiom,(
    cell40 != cell6 )).

fof(tlhfof39263,axiom,(
    cell40 != cell16 )).

fof(tlhfof39264,axiom,(
    cell40 != cell5 )).

fof(tlhfof39265,axiom,(
    cell40 != cell15 )).

fof(tlhfof39266,axiom,(
    cell40 != cell4 )).

fof(tlhfof39267,axiom,(
    cell40 != cell14 )).

fof(tlhfof39268,axiom,(
    cell40 != cell3 )).

fof(tlhfof39269,axiom,(
    cell40 != cell13 )).

fof(tlhfof39270,axiom,(
    cell40 != cell2 )).

fof(tlhfof39271,axiom,(
    cell40 != cell12 )).

fof(tlhfof39272,axiom,(
    cell40 != cell1 )).

fof(tlhfof39273,axiom,(
    cell40 != cell11 )).

fof(tlhfof39274,axiom,(
    cell40 != cell20 )).

fof(tlhfof39275,axiom,(
    cell39 != cell38 )).

fof(tlhfof39276,axiom,(
    cell39 != cell37 )).

fof(tlhfof39277,axiom,(
    cell39 != cell36 )).

fof(tlhfof39278,axiom,(
    cell39 != cell35 )).

fof(tlhfof39279,axiom,(
    cell39 != cell34 )).

fof(tlhfof39280,axiom,(
    cell39 != cell33 )).

fof(tlhfof39281,axiom,(
    cell39 != cell32 )).

fof(tlhfof39282,axiom,(
    cell39 != cell31 )).

fof(tlhfof39283,axiom,(
    cell39 != cell30 )).

fof(tlhfof39284,axiom,(
    cell39 != cell29 )).

fof(tlhfof39285,axiom,(
    cell39 != cell28 )).

fof(tlhfof39286,axiom,(
    cell39 != cell27 )).

fof(tlhfof39287,axiom,(
    cell39 != cell26 )).

fof(tlhfof39288,axiom,(
    cell39 != cell25 )).

fof(tlhfof39289,axiom,(
    cell39 != cell24 )).

fof(tlhfof39290,axiom,(
    cell39 != cell23 )).

fof(tlhfof39291,axiom,(
    cell39 != cell22 )).

fof(tlhfof39292,axiom,(
    cell39 != cell21 )).

fof(tlhfof39293,axiom,(
    cell39 != cell10 )).

fof(tlhfof39294,axiom,(
    cell39 != cell9 )).

fof(tlhfof39295,axiom,(
    cell39 != cell19 )).

fof(tlhfof39296,axiom,(
    cell39 != cell8 )).

fof(tlhfof39297,axiom,(
    cell39 != cell18 )).

fof(tlhfof39298,axiom,(
    cell39 != cell7 )).

fof(tlhfof39299,axiom,(
    cell39 != cell17 )).

fof(tlhfof39300,axiom,(
    cell39 != cell6 )).

fof(tlhfof39301,axiom,(
    cell39 != cell16 )).

fof(tlhfof39302,axiom,(
    cell39 != cell5 )).

fof(tlhfof39303,axiom,(
    cell39 != cell15 )).

fof(tlhfof39304,axiom,(
    cell39 != cell4 )).

fof(tlhfof39305,axiom,(
    cell39 != cell14 )).

fof(tlhfof39306,axiom,(
    cell39 != cell3 )).

fof(tlhfof39307,axiom,(
    cell39 != cell13 )).

fof(tlhfof39308,axiom,(
    cell39 != cell2 )).

fof(tlhfof39309,axiom,(
    cell39 != cell12 )).

fof(tlhfof39310,axiom,(
    cell39 != cell1 )).

fof(tlhfof39311,axiom,(
    cell39 != cell11 )).

fof(tlhfof39312,axiom,(
    cell39 != cell20 )).

fof(tlhfof39313,axiom,(
    cell38 != cell37 )).

fof(tlhfof39314,axiom,(
    cell38 != cell36 )).

fof(tlhfof39315,axiom,(
    cell38 != cell35 )).

fof(tlhfof39316,axiom,(
    cell38 != cell34 )).

fof(tlhfof39317,axiom,(
    cell38 != cell33 )).

fof(tlhfof39318,axiom,(
    cell38 != cell32 )).

fof(tlhfof39319,axiom,(
    cell38 != cell31 )).

fof(tlhfof39320,axiom,(
    cell38 != cell30 )).

fof(tlhfof39321,axiom,(
    cell38 != cell29 )).

fof(tlhfof39322,axiom,(
    cell38 != cell28 )).

fof(tlhfof39323,axiom,(
    cell38 != cell27 )).

fof(tlhfof39324,axiom,(
    cell38 != cell26 )).

fof(tlhfof39325,axiom,(
    cell38 != cell25 )).

fof(tlhfof39326,axiom,(
    cell38 != cell24 )).

fof(tlhfof39327,axiom,(
    cell38 != cell23 )).

fof(tlhfof39328,axiom,(
    cell38 != cell22 )).

fof(tlhfof39329,axiom,(
    cell38 != cell21 )).

fof(tlhfof39330,axiom,(
    cell38 != cell10 )).

fof(tlhfof39331,axiom,(
    cell38 != cell9 )).

fof(tlhfof39332,axiom,(
    cell38 != cell19 )).

fof(tlhfof39333,axiom,(
    cell38 != cell8 )).

fof(tlhfof39334,axiom,(
    cell38 != cell18 )).

fof(tlhfof39335,axiom,(
    cell38 != cell7 )).

fof(tlhfof39336,axiom,(
    cell38 != cell17 )).

fof(tlhfof39337,axiom,(
    cell38 != cell6 )).

fof(tlhfof39338,axiom,(
    cell38 != cell16 )).

fof(tlhfof39339,axiom,(
    cell38 != cell5 )).

fof(tlhfof39340,axiom,(
    cell38 != cell15 )).

fof(tlhfof39341,axiom,(
    cell38 != cell4 )).

fof(tlhfof39342,axiom,(
    cell38 != cell14 )).

fof(tlhfof39343,axiom,(
    cell38 != cell3 )).

fof(tlhfof39344,axiom,(
    cell38 != cell13 )).

fof(tlhfof39345,axiom,(
    cell38 != cell2 )).

fof(tlhfof39346,axiom,(
    cell38 != cell12 )).

fof(tlhfof39347,axiom,(
    cell38 != cell1 )).

fof(tlhfof39348,axiom,(
    cell38 != cell11 )).

fof(tlhfof39349,axiom,(
    cell38 != cell20 )).

fof(tlhfof39350,axiom,(
    cell37 != cell36 )).

fof(tlhfof39351,axiom,(
    cell37 != cell35 )).

fof(tlhfof39352,axiom,(
    cell37 != cell34 )).

fof(tlhfof39353,axiom,(
    cell37 != cell33 )).

fof(tlhfof39354,axiom,(
    cell37 != cell32 )).

fof(tlhfof39355,axiom,(
    cell37 != cell31 )).

fof(tlhfof39356,axiom,(
    cell37 != cell30 )).

fof(tlhfof39357,axiom,(
    cell37 != cell29 )).

fof(tlhfof39358,axiom,(
    cell37 != cell28 )).

fof(tlhfof39359,axiom,(
    cell37 != cell27 )).

fof(tlhfof39360,axiom,(
    cell37 != cell26 )).

fof(tlhfof39361,axiom,(
    cell37 != cell25 )).

fof(tlhfof39362,axiom,(
    cell37 != cell24 )).

fof(tlhfof39363,axiom,(
    cell37 != cell23 )).

fof(tlhfof39364,axiom,(
    cell37 != cell22 )).

fof(tlhfof39365,axiom,(
    cell37 != cell21 )).

fof(tlhfof39366,axiom,(
    cell37 != cell10 )).

fof(tlhfof39367,axiom,(
    cell37 != cell9 )).

fof(tlhfof39368,axiom,(
    cell37 != cell19 )).

fof(tlhfof39369,axiom,(
    cell37 != cell8 )).

fof(tlhfof39370,axiom,(
    cell37 != cell18 )).

fof(tlhfof39371,axiom,(
    cell37 != cell7 )).

fof(tlhfof39372,axiom,(
    cell37 != cell17 )).

fof(tlhfof39373,axiom,(
    cell37 != cell6 )).

fof(tlhfof39374,axiom,(
    cell37 != cell16 )).

fof(tlhfof39375,axiom,(
    cell37 != cell5 )).

fof(tlhfof39376,axiom,(
    cell37 != cell15 )).

fof(tlhfof39377,axiom,(
    cell37 != cell4 )).

fof(tlhfof39378,axiom,(
    cell37 != cell14 )).

fof(tlhfof39379,axiom,(
    cell37 != cell3 )).

fof(tlhfof39380,axiom,(
    cell37 != cell13 )).

fof(tlhfof39381,axiom,(
    cell37 != cell2 )).

fof(tlhfof39382,axiom,(
    cell37 != cell12 )).

fof(tlhfof39383,axiom,(
    cell37 != cell1 )).

fof(tlhfof39384,axiom,(
    cell37 != cell11 )).

fof(tlhfof39385,axiom,(
    cell37 != cell20 )).

fof(tlhfof39386,axiom,(
    cell36 != cell35 )).

fof(tlhfof39387,axiom,(
    cell36 != cell34 )).

fof(tlhfof39388,axiom,(
    cell36 != cell33 )).

fof(tlhfof39389,axiom,(
    cell36 != cell32 )).

fof(tlhfof39390,axiom,(
    cell36 != cell31 )).

fof(tlhfof39391,axiom,(
    cell36 != cell30 )).

fof(tlhfof39392,axiom,(
    cell36 != cell29 )).

fof(tlhfof39393,axiom,(
    cell36 != cell28 )).

fof(tlhfof39394,axiom,(
    cell36 != cell27 )).

fof(tlhfof39395,axiom,(
    cell36 != cell26 )).

fof(tlhfof39396,axiom,(
    cell36 != cell25 )).

fof(tlhfof39397,axiom,(
    cell36 != cell24 )).

fof(tlhfof39398,axiom,(
    cell36 != cell23 )).

fof(tlhfof39399,axiom,(
    cell36 != cell22 )).

fof(tlhfof39400,axiom,(
    cell36 != cell21 )).

fof(tlhfof39401,axiom,(
    cell36 != cell10 )).

fof(tlhfof39402,axiom,(
    cell36 != cell9 )).

fof(tlhfof39403,axiom,(
    cell36 != cell19 )).

fof(tlhfof39404,axiom,(
    cell36 != cell8 )).

fof(tlhfof39405,axiom,(
    cell36 != cell18 )).

fof(tlhfof39406,axiom,(
    cell36 != cell7 )).

fof(tlhfof39407,axiom,(
    cell36 != cell17 )).

fof(tlhfof39408,axiom,(
    cell36 != cell6 )).

fof(tlhfof39409,axiom,(
    cell36 != cell16 )).

fof(tlhfof39410,axiom,(
    cell36 != cell5 )).

fof(tlhfof39411,axiom,(
    cell36 != cell15 )).

fof(tlhfof39412,axiom,(
    cell36 != cell4 )).

fof(tlhfof39413,axiom,(
    cell36 != cell14 )).

fof(tlhfof39414,axiom,(
    cell36 != cell3 )).

fof(tlhfof39415,axiom,(
    cell36 != cell13 )).

fof(tlhfof39416,axiom,(
    cell36 != cell2 )).

fof(tlhfof39417,axiom,(
    cell36 != cell12 )).

fof(tlhfof39418,axiom,(
    cell36 != cell1 )).

fof(tlhfof39419,axiom,(
    cell36 != cell11 )).

fof(tlhfof39420,axiom,(
    cell36 != cell20 )).

fof(tlhfof39421,axiom,(
    cell35 != cell34 )).

fof(tlhfof39422,axiom,(
    cell35 != cell33 )).

fof(tlhfof39423,axiom,(
    cell35 != cell32 )).

fof(tlhfof39424,axiom,(
    cell35 != cell31 )).

fof(tlhfof39425,axiom,(
    cell35 != cell30 )).

fof(tlhfof39426,axiom,(
    cell35 != cell29 )).

fof(tlhfof39427,axiom,(
    cell35 != cell28 )).

fof(tlhfof39428,axiom,(
    cell35 != cell27 )).

fof(tlhfof39429,axiom,(
    cell35 != cell26 )).

fof(tlhfof39430,axiom,(
    cell35 != cell25 )).

fof(tlhfof39431,axiom,(
    cell35 != cell24 )).

fof(tlhfof39432,axiom,(
    cell35 != cell23 )).

fof(tlhfof39433,axiom,(
    cell35 != cell22 )).

fof(tlhfof39434,axiom,(
    cell35 != cell21 )).

fof(tlhfof39435,axiom,(
    cell35 != cell10 )).

fof(tlhfof39436,axiom,(
    cell35 != cell9 )).

fof(tlhfof39437,axiom,(
    cell35 != cell19 )).

fof(tlhfof39438,axiom,(
    cell35 != cell8 )).

fof(tlhfof39439,axiom,(
    cell35 != cell18 )).

fof(tlhfof39440,axiom,(
    cell35 != cell7 )).

fof(tlhfof39441,axiom,(
    cell35 != cell17 )).

fof(tlhfof39442,axiom,(
    cell35 != cell6 )).

fof(tlhfof39443,axiom,(
    cell35 != cell16 )).

fof(tlhfof39444,axiom,(
    cell35 != cell5 )).

fof(tlhfof39445,axiom,(
    cell35 != cell15 )).

fof(tlhfof39446,axiom,(
    cell35 != cell4 )).

fof(tlhfof39447,axiom,(
    cell35 != cell14 )).

fof(tlhfof39448,axiom,(
    cell35 != cell3 )).

fof(tlhfof39449,axiom,(
    cell35 != cell13 )).

fof(tlhfof39450,axiom,(
    cell35 != cell2 )).

fof(tlhfof39451,axiom,(
    cell35 != cell12 )).

fof(tlhfof39452,axiom,(
    cell35 != cell1 )).

fof(tlhfof39453,axiom,(
    cell35 != cell11 )).

fof(tlhfof39454,axiom,(
    cell35 != cell20 )).

fof(tlhfof39455,axiom,(
    cell34 != cell33 )).

fof(tlhfof39456,axiom,(
    cell34 != cell32 )).

fof(tlhfof39457,axiom,(
    cell34 != cell31 )).

fof(tlhfof39458,axiom,(
    cell34 != cell30 )).

fof(tlhfof39459,axiom,(
    cell34 != cell29 )).

fof(tlhfof39460,axiom,(
    cell34 != cell28 )).

fof(tlhfof39461,axiom,(
    cell34 != cell27 )).

fof(tlhfof39462,axiom,(
    cell34 != cell26 )).

fof(tlhfof39463,axiom,(
    cell34 != cell25 )).

fof(tlhfof39464,axiom,(
    cell34 != cell24 )).

fof(tlhfof39465,axiom,(
    cell34 != cell23 )).

fof(tlhfof39466,axiom,(
    cell34 != cell22 )).

fof(tlhfof39467,axiom,(
    cell34 != cell21 )).

fof(tlhfof39468,axiom,(
    cell34 != cell10 )).

fof(tlhfof39469,axiom,(
    cell34 != cell9 )).

fof(tlhfof39470,axiom,(
    cell34 != cell19 )).

fof(tlhfof39471,axiom,(
    cell34 != cell8 )).

fof(tlhfof39472,axiom,(
    cell34 != cell18 )).

fof(tlhfof39473,axiom,(
    cell34 != cell7 )).

fof(tlhfof39474,axiom,(
    cell34 != cell17 )).

fof(tlhfof39475,axiom,(
    cell34 != cell6 )).

fof(tlhfof39476,axiom,(
    cell34 != cell16 )).

fof(tlhfof39477,axiom,(
    cell34 != cell5 )).

fof(tlhfof39478,axiom,(
    cell34 != cell15 )).

fof(tlhfof39479,axiom,(
    cell34 != cell4 )).

fof(tlhfof39480,axiom,(
    cell34 != cell14 )).

fof(tlhfof39481,axiom,(
    cell34 != cell3 )).

fof(tlhfof39482,axiom,(
    cell34 != cell13 )).

fof(tlhfof39483,axiom,(
    cell34 != cell2 )).

fof(tlhfof39484,axiom,(
    cell34 != cell12 )).

fof(tlhfof39485,axiom,(
    cell34 != cell1 )).

fof(tlhfof39486,axiom,(
    cell34 != cell11 )).

fof(tlhfof39487,axiom,(
    cell34 != cell20 )).

fof(tlhfof39488,axiom,(
    cell33 != cell32 )).

fof(tlhfof39489,axiom,(
    cell33 != cell31 )).

fof(tlhfof39490,axiom,(
    cell33 != cell30 )).

fof(tlhfof39491,axiom,(
    cell33 != cell29 )).

fof(tlhfof39492,axiom,(
    cell33 != cell28 )).

fof(tlhfof39493,axiom,(
    cell33 != cell27 )).

fof(tlhfof39494,axiom,(
    cell33 != cell26 )).

fof(tlhfof39495,axiom,(
    cell33 != cell25 )).

fof(tlhfof39496,axiom,(
    cell33 != cell24 )).

fof(tlhfof39497,axiom,(
    cell33 != cell23 )).

fof(tlhfof39498,axiom,(
    cell33 != cell22 )).

fof(tlhfof39499,axiom,(
    cell33 != cell21 )).

fof(tlhfof39500,axiom,(
    cell33 != cell10 )).

fof(tlhfof39501,axiom,(
    cell33 != cell9 )).

fof(tlhfof39502,axiom,(
    cell33 != cell19 )).

fof(tlhfof39503,axiom,(
    cell33 != cell8 )).

fof(tlhfof39504,axiom,(
    cell33 != cell18 )).

fof(tlhfof39505,axiom,(
    cell33 != cell7 )).

fof(tlhfof39506,axiom,(
    cell33 != cell17 )).

fof(tlhfof39507,axiom,(
    cell33 != cell6 )).

fof(tlhfof39508,axiom,(
    cell33 != cell16 )).

fof(tlhfof39509,axiom,(
    cell33 != cell5 )).

fof(tlhfof39510,axiom,(
    cell33 != cell15 )).

fof(tlhfof39511,axiom,(
    cell33 != cell4 )).

fof(tlhfof39512,axiom,(
    cell33 != cell14 )).

fof(tlhfof39513,axiom,(
    cell33 != cell3 )).

fof(tlhfof39514,axiom,(
    cell33 != cell13 )).

fof(tlhfof39515,axiom,(
    cell33 != cell2 )).

fof(tlhfof39516,axiom,(
    cell33 != cell12 )).

fof(tlhfof39517,axiom,(
    cell33 != cell1 )).

fof(tlhfof39518,axiom,(
    cell33 != cell11 )).

fof(tlhfof39519,axiom,(
    cell33 != cell20 )).

fof(tlhfof39520,axiom,(
    cell32 != cell31 )).

fof(tlhfof39521,axiom,(
    cell32 != cell30 )).

fof(tlhfof39522,axiom,(
    cell32 != cell29 )).

fof(tlhfof39523,axiom,(
    cell32 != cell28 )).

fof(tlhfof39524,axiom,(
    cell32 != cell27 )).

fof(tlhfof39525,axiom,(
    cell32 != cell26 )).

fof(tlhfof39526,axiom,(
    cell32 != cell25 )).

fof(tlhfof39527,axiom,(
    cell32 != cell24 )).

fof(tlhfof39528,axiom,(
    cell32 != cell23 )).

fof(tlhfof39529,axiom,(
    cell32 != cell22 )).

fof(tlhfof39530,axiom,(
    cell32 != cell21 )).

fof(tlhfof39531,axiom,(
    cell32 != cell10 )).

fof(tlhfof39532,axiom,(
    cell32 != cell9 )).

fof(tlhfof39533,axiom,(
    cell32 != cell19 )).

fof(tlhfof39534,axiom,(
    cell32 != cell8 )).

fof(tlhfof39535,axiom,(
    cell32 != cell18 )).

fof(tlhfof39536,axiom,(
    cell32 != cell7 )).

fof(tlhfof39537,axiom,(
    cell32 != cell17 )).

fof(tlhfof39538,axiom,(
    cell32 != cell6 )).

fof(tlhfof39539,axiom,(
    cell32 != cell16 )).

fof(tlhfof39540,axiom,(
    cell32 != cell5 )).

fof(tlhfof39541,axiom,(
    cell32 != cell15 )).

fof(tlhfof39542,axiom,(
    cell32 != cell4 )).

fof(tlhfof39543,axiom,(
    cell32 != cell14 )).

fof(tlhfof39544,axiom,(
    cell32 != cell3 )).

fof(tlhfof39545,axiom,(
    cell32 != cell13 )).

fof(tlhfof39546,axiom,(
    cell32 != cell2 )).

fof(tlhfof39547,axiom,(
    cell32 != cell12 )).

fof(tlhfof39548,axiom,(
    cell32 != cell1 )).

fof(tlhfof39549,axiom,(
    cell32 != cell11 )).

fof(tlhfof39550,axiom,(
    cell32 != cell20 )).

fof(tlhfof39551,axiom,(
    cell31 != cell30 )).

fof(tlhfof39552,axiom,(
    cell31 != cell29 )).

fof(tlhfof39553,axiom,(
    cell31 != cell28 )).

fof(tlhfof39554,axiom,(
    cell31 != cell27 )).

fof(tlhfof39555,axiom,(
    cell31 != cell26 )).

fof(tlhfof39556,axiom,(
    cell31 != cell25 )).

fof(tlhfof39557,axiom,(
    cell31 != cell24 )).

fof(tlhfof39558,axiom,(
    cell31 != cell23 )).

fof(tlhfof39559,axiom,(
    cell31 != cell22 )).

fof(tlhfof39560,axiom,(
    cell31 != cell21 )).

fof(tlhfof39561,axiom,(
    cell31 != cell10 )).

fof(tlhfof39562,axiom,(
    cell31 != cell9 )).

fof(tlhfof39563,axiom,(
    cell31 != cell19 )).

fof(tlhfof39564,axiom,(
    cell31 != cell8 )).

fof(tlhfof39565,axiom,(
    cell31 != cell18 )).

fof(tlhfof39566,axiom,(
    cell31 != cell7 )).

fof(tlhfof39567,axiom,(
    cell31 != cell17 )).

fof(tlhfof39568,axiom,(
    cell31 != cell6 )).

fof(tlhfof39569,axiom,(
    cell31 != cell16 )).

fof(tlhfof39570,axiom,(
    cell31 != cell5 )).

fof(tlhfof39571,axiom,(
    cell31 != cell15 )).

fof(tlhfof39572,axiom,(
    cell31 != cell4 )).

fof(tlhfof39573,axiom,(
    cell31 != cell14 )).

fof(tlhfof39574,axiom,(
    cell31 != cell3 )).

fof(tlhfof39575,axiom,(
    cell31 != cell13 )).

fof(tlhfof39576,axiom,(
    cell31 != cell2 )).

fof(tlhfof39577,axiom,(
    cell31 != cell12 )).

fof(tlhfof39578,axiom,(
    cell31 != cell1 )).

fof(tlhfof39579,axiom,(
    cell31 != cell11 )).

fof(tlhfof39580,axiom,(
    cell31 != cell20 )).

fof(tlhfof39581,axiom,(
    cell30 != cell29 )).

fof(tlhfof39582,axiom,(
    cell30 != cell28 )).

fof(tlhfof39583,axiom,(
    cell30 != cell27 )).

fof(tlhfof39584,axiom,(
    cell30 != cell26 )).

fof(tlhfof39585,axiom,(
    cell30 != cell25 )).

fof(tlhfof39586,axiom,(
    cell30 != cell24 )).

fof(tlhfof39587,axiom,(
    cell30 != cell23 )).

fof(tlhfof39588,axiom,(
    cell30 != cell22 )).

fof(tlhfof39589,axiom,(
    cell30 != cell21 )).

fof(tlhfof39590,axiom,(
    cell30 != cell10 )).

fof(tlhfof39591,axiom,(
    cell30 != cell9 )).

fof(tlhfof39592,axiom,(
    cell30 != cell19 )).

fof(tlhfof39593,axiom,(
    cell30 != cell8 )).

fof(tlhfof39594,axiom,(
    cell30 != cell18 )).

fof(tlhfof39595,axiom,(
    cell30 != cell7 )).

fof(tlhfof39596,axiom,(
    cell30 != cell17 )).

fof(tlhfof39597,axiom,(
    cell30 != cell6 )).

fof(tlhfof39598,axiom,(
    cell30 != cell16 )).

fof(tlhfof39599,axiom,(
    cell30 != cell5 )).

fof(tlhfof39600,axiom,(
    cell30 != cell15 )).

fof(tlhfof39601,axiom,(
    cell30 != cell4 )).

fof(tlhfof39602,axiom,(
    cell30 != cell14 )).

fof(tlhfof39603,axiom,(
    cell30 != cell3 )).

fof(tlhfof39604,axiom,(
    cell30 != cell13 )).

fof(tlhfof39605,axiom,(
    cell30 != cell2 )).

fof(tlhfof39606,axiom,(
    cell30 != cell12 )).

fof(tlhfof39607,axiom,(
    cell30 != cell1 )).

fof(tlhfof39608,axiom,(
    cell30 != cell11 )).

fof(tlhfof39609,axiom,(
    cell30 != cell20 )).

fof(tlhfof39610,axiom,(
    cell29 != cell28 )).

fof(tlhfof39611,axiom,(
    cell29 != cell27 )).

fof(tlhfof39612,axiom,(
    cell29 != cell26 )).

fof(tlhfof39613,axiom,(
    cell29 != cell25 )).

fof(tlhfof39614,axiom,(
    cell29 != cell24 )).

fof(tlhfof39615,axiom,(
    cell29 != cell23 )).

fof(tlhfof39616,axiom,(
    cell29 != cell22 )).

fof(tlhfof39617,axiom,(
    cell29 != cell21 )).

fof(tlhfof39618,axiom,(
    cell29 != cell10 )).

fof(tlhfof39619,axiom,(
    cell29 != cell9 )).

fof(tlhfof39620,axiom,(
    cell29 != cell19 )).

fof(tlhfof39621,axiom,(
    cell29 != cell8 )).

fof(tlhfof39622,axiom,(
    cell29 != cell18 )).

fof(tlhfof39623,axiom,(
    cell29 != cell7 )).

fof(tlhfof39624,axiom,(
    cell29 != cell17 )).

fof(tlhfof39625,axiom,(
    cell29 != cell6 )).

fof(tlhfof39626,axiom,(
    cell29 != cell16 )).

fof(tlhfof39627,axiom,(
    cell29 != cell5 )).

fof(tlhfof39628,axiom,(
    cell29 != cell15 )).

fof(tlhfof39629,axiom,(
    cell29 != cell4 )).

fof(tlhfof39630,axiom,(
    cell29 != cell14 )).

fof(tlhfof39631,axiom,(
    cell29 != cell3 )).

fof(tlhfof39632,axiom,(
    cell29 != cell13 )).

fof(tlhfof39633,axiom,(
    cell29 != cell2 )).

fof(tlhfof39634,axiom,(
    cell29 != cell12 )).

fof(tlhfof39635,axiom,(
    cell29 != cell1 )).

fof(tlhfof39636,axiom,(
    cell29 != cell11 )).

fof(tlhfof39637,axiom,(
    cell29 != cell20 )).

fof(tlhfof39638,axiom,(
    cell28 != cell27 )).

fof(tlhfof39639,axiom,(
    cell28 != cell26 )).

fof(tlhfof39640,axiom,(
    cell28 != cell25 )).

fof(tlhfof39641,axiom,(
    cell28 != cell24 )).

fof(tlhfof39642,axiom,(
    cell28 != cell23 )).

fof(tlhfof39643,axiom,(
    cell28 != cell22 )).

fof(tlhfof39644,axiom,(
    cell28 != cell21 )).

fof(tlhfof39645,axiom,(
    cell28 != cell10 )).

fof(tlhfof39646,axiom,(
    cell28 != cell9 )).

fof(tlhfof39647,axiom,(
    cell28 != cell19 )).

fof(tlhfof39648,axiom,(
    cell28 != cell8 )).

fof(tlhfof39649,axiom,(
    cell28 != cell18 )).

fof(tlhfof39650,axiom,(
    cell28 != cell7 )).

fof(tlhfof39651,axiom,(
    cell28 != cell17 )).

fof(tlhfof39652,axiom,(
    cell28 != cell6 )).

fof(tlhfof39653,axiom,(
    cell28 != cell16 )).

fof(tlhfof39654,axiom,(
    cell28 != cell5 )).

fof(tlhfof39655,axiom,(
    cell28 != cell15 )).

fof(tlhfof39656,axiom,(
    cell28 != cell4 )).

fof(tlhfof39657,axiom,(
    cell28 != cell14 )).

fof(tlhfof39658,axiom,(
    cell28 != cell3 )).

fof(tlhfof39659,axiom,(
    cell28 != cell13 )).

fof(tlhfof39660,axiom,(
    cell28 != cell2 )).

fof(tlhfof39661,axiom,(
    cell28 != cell12 )).

fof(tlhfof39662,axiom,(
    cell28 != cell1 )).

fof(tlhfof39663,axiom,(
    cell28 != cell11 )).

fof(tlhfof39664,axiom,(
    cell28 != cell20 )).

fof(tlhfof39665,axiom,(
    cell27 != cell26 )).

fof(tlhfof39666,axiom,(
    cell27 != cell25 )).

fof(tlhfof39667,axiom,(
    cell27 != cell24 )).

fof(tlhfof39668,axiom,(
    cell27 != cell23 )).

fof(tlhfof39669,axiom,(
    cell27 != cell22 )).

fof(tlhfof39670,axiom,(
    cell27 != cell21 )).

fof(tlhfof39671,axiom,(
    cell27 != cell10 )).

fof(tlhfof39672,axiom,(
    cell27 != cell9 )).

fof(tlhfof39673,axiom,(
    cell27 != cell19 )).

fof(tlhfof39674,axiom,(
    cell27 != cell8 )).

fof(tlhfof39675,axiom,(
    cell27 != cell18 )).

fof(tlhfof39676,axiom,(
    cell27 != cell7 )).

fof(tlhfof39677,axiom,(
    cell27 != cell17 )).

fof(tlhfof39678,axiom,(
    cell27 != cell6 )).

fof(tlhfof39679,axiom,(
    cell27 != cell16 )).

fof(tlhfof39680,axiom,(
    cell27 != cell5 )).

fof(tlhfof39681,axiom,(
    cell27 != cell15 )).

fof(tlhfof39682,axiom,(
    cell27 != cell4 )).

fof(tlhfof39683,axiom,(
    cell27 != cell14 )).

fof(tlhfof39684,axiom,(
    cell27 != cell3 )).

fof(tlhfof39685,axiom,(
    cell27 != cell13 )).

fof(tlhfof39686,axiom,(
    cell27 != cell2 )).

fof(tlhfof39687,axiom,(
    cell27 != cell12 )).

fof(tlhfof39688,axiom,(
    cell27 != cell1 )).

fof(tlhfof39689,axiom,(
    cell27 != cell11 )).

fof(tlhfof39690,axiom,(
    cell27 != cell20 )).

fof(tlhfof39691,axiom,(
    cell26 != cell25 )).

fof(tlhfof39692,axiom,(
    cell26 != cell24 )).

fof(tlhfof39693,axiom,(
    cell26 != cell23 )).

fof(tlhfof39694,axiom,(
    cell26 != cell22 )).

fof(tlhfof39695,axiom,(
    cell26 != cell21 )).

fof(tlhfof39696,axiom,(
    cell26 != cell10 )).

fof(tlhfof39697,axiom,(
    cell26 != cell9 )).

fof(tlhfof39698,axiom,(
    cell26 != cell19 )).

fof(tlhfof39699,axiom,(
    cell26 != cell8 )).

fof(tlhfof39700,axiom,(
    cell26 != cell18 )).

fof(tlhfof39701,axiom,(
    cell26 != cell7 )).

fof(tlhfof39702,axiom,(
    cell26 != cell17 )).

fof(tlhfof39703,axiom,(
    cell26 != cell6 )).

fof(tlhfof39704,axiom,(
    cell26 != cell16 )).

fof(tlhfof39705,axiom,(
    cell26 != cell5 )).

fof(tlhfof39706,axiom,(
    cell26 != cell15 )).

fof(tlhfof39707,axiom,(
    cell26 != cell4 )).

fof(tlhfof39708,axiom,(
    cell26 != cell14 )).

fof(tlhfof39709,axiom,(
    cell26 != cell3 )).

fof(tlhfof39710,axiom,(
    cell26 != cell13 )).

fof(tlhfof39711,axiom,(
    cell26 != cell2 )).

fof(tlhfof39712,axiom,(
    cell26 != cell12 )).

fof(tlhfof39713,axiom,(
    cell26 != cell1 )).

fof(tlhfof39714,axiom,(
    cell26 != cell11 )).

fof(tlhfof39715,axiom,(
    cell26 != cell20 )).

fof(tlhfof39716,axiom,(
    cell25 != cell24 )).

fof(tlhfof39717,axiom,(
    cell25 != cell23 )).

fof(tlhfof39718,axiom,(
    cell25 != cell22 )).

fof(tlhfof39719,axiom,(
    cell25 != cell21 )).

fof(tlhfof39720,axiom,(
    cell25 != cell10 )).

fof(tlhfof39721,axiom,(
    cell25 != cell9 )).

fof(tlhfof39722,axiom,(
    cell25 != cell19 )).

fof(tlhfof39723,axiom,(
    cell25 != cell8 )).

fof(tlhfof39724,axiom,(
    cell25 != cell18 )).

fof(tlhfof39725,axiom,(
    cell25 != cell7 )).

fof(tlhfof39726,axiom,(
    cell25 != cell17 )).

fof(tlhfof39727,axiom,(
    cell25 != cell6 )).

fof(tlhfof39728,axiom,(
    cell25 != cell16 )).

fof(tlhfof39729,axiom,(
    cell25 != cell5 )).

fof(tlhfof39730,axiom,(
    cell25 != cell15 )).

fof(tlhfof39731,axiom,(
    cell25 != cell4 )).

fof(tlhfof39732,axiom,(
    cell25 != cell14 )).

fof(tlhfof39733,axiom,(
    cell25 != cell3 )).

fof(tlhfof39734,axiom,(
    cell25 != cell13 )).

fof(tlhfof39735,axiom,(
    cell25 != cell2 )).

fof(tlhfof39736,axiom,(
    cell25 != cell12 )).

fof(tlhfof39737,axiom,(
    cell25 != cell1 )).

fof(tlhfof39738,axiom,(
    cell25 != cell11 )).

fof(tlhfof39739,axiom,(
    cell25 != cell20 )).

fof(tlhfof39740,axiom,(
    cell24 != cell23 )).

fof(tlhfof39741,axiom,(
    cell24 != cell22 )).

fof(tlhfof39742,axiom,(
    cell24 != cell21 )).

fof(tlhfof39743,axiom,(
    cell24 != cell10 )).

fof(tlhfof39744,axiom,(
    cell24 != cell9 )).

fof(tlhfof39745,axiom,(
    cell24 != cell19 )).

fof(tlhfof39746,axiom,(
    cell24 != cell8 )).

fof(tlhfof39747,axiom,(
    cell24 != cell18 )).

fof(tlhfof39748,axiom,(
    cell24 != cell7 )).

fof(tlhfof39749,axiom,(
    cell24 != cell17 )).

fof(tlhfof39750,axiom,(
    cell24 != cell6 )).

fof(tlhfof39751,axiom,(
    cell24 != cell16 )).

fof(tlhfof39752,axiom,(
    cell24 != cell5 )).

fof(tlhfof39753,axiom,(
    cell24 != cell15 )).

fof(tlhfof39754,axiom,(
    cell24 != cell4 )).

fof(tlhfof39755,axiom,(
    cell24 != cell14 )).

fof(tlhfof39756,axiom,(
    cell24 != cell3 )).

fof(tlhfof39757,axiom,(
    cell24 != cell13 )).

fof(tlhfof39758,axiom,(
    cell24 != cell2 )).

fof(tlhfof39759,axiom,(
    cell24 != cell12 )).

fof(tlhfof39760,axiom,(
    cell24 != cell1 )).

fof(tlhfof39761,axiom,(
    cell24 != cell11 )).

fof(tlhfof39762,axiom,(
    cell24 != cell20 )).

fof(tlhfof39763,axiom,(
    cell23 != cell22 )).

fof(tlhfof39764,axiom,(
    cell23 != cell21 )).

fof(tlhfof39765,axiom,(
    cell23 != cell10 )).

fof(tlhfof39766,axiom,(
    cell23 != cell9 )).

fof(tlhfof39767,axiom,(
    cell23 != cell19 )).

fof(tlhfof39768,axiom,(
    cell23 != cell8 )).

fof(tlhfof39769,axiom,(
    cell23 != cell18 )).

fof(tlhfof39770,axiom,(
    cell23 != cell7 )).

fof(tlhfof39771,axiom,(
    cell23 != cell17 )).

fof(tlhfof39772,axiom,(
    cell23 != cell6 )).

fof(tlhfof39773,axiom,(
    cell23 != cell16 )).

fof(tlhfof39774,axiom,(
    cell23 != cell5 )).

fof(tlhfof39775,axiom,(
    cell23 != cell15 )).

fof(tlhfof39776,axiom,(
    cell23 != cell4 )).

fof(tlhfof39777,axiom,(
    cell23 != cell14 )).

fof(tlhfof39778,axiom,(
    cell23 != cell3 )).

fof(tlhfof39779,axiom,(
    cell23 != cell13 )).

fof(tlhfof39780,axiom,(
    cell23 != cell2 )).

fof(tlhfof39781,axiom,(
    cell23 != cell12 )).

fof(tlhfof39782,axiom,(
    cell23 != cell1 )).

fof(tlhfof39783,axiom,(
    cell23 != cell11 )).

fof(tlhfof39784,axiom,(
    cell23 != cell20 )).

fof(tlhfof39785,axiom,(
    cell22 != cell21 )).

fof(tlhfof39786,axiom,(
    cell22 != cell10 )).

fof(tlhfof39787,axiom,(
    cell22 != cell9 )).

fof(tlhfof39788,axiom,(
    cell22 != cell19 )).

fof(tlhfof39789,axiom,(
    cell22 != cell8 )).

fof(tlhfof39790,axiom,(
    cell22 != cell18 )).

fof(tlhfof39791,axiom,(
    cell22 != cell7 )).

fof(tlhfof39792,axiom,(
    cell22 != cell17 )).

fof(tlhfof39793,axiom,(
    cell22 != cell6 )).

fof(tlhfof39794,axiom,(
    cell22 != cell16 )).

fof(tlhfof39795,axiom,(
    cell22 != cell5 )).

fof(tlhfof39796,axiom,(
    cell22 != cell15 )).

fof(tlhfof39797,axiom,(
    cell22 != cell4 )).

fof(tlhfof39798,axiom,(
    cell22 != cell14 )).

fof(tlhfof39799,axiom,(
    cell22 != cell3 )).

fof(tlhfof39800,axiom,(
    cell22 != cell13 )).

fof(tlhfof39801,axiom,(
    cell22 != cell2 )).

fof(tlhfof39802,axiom,(
    cell22 != cell12 )).

fof(tlhfof39803,axiom,(
    cell22 != cell1 )).

fof(tlhfof39804,axiom,(
    cell22 != cell11 )).

fof(tlhfof39805,axiom,(
    cell22 != cell20 )).

fof(tlhfof39806,axiom,(
    cell21 != cell10 )).

fof(tlhfof39807,axiom,(
    cell21 != cell9 )).

fof(tlhfof39808,axiom,(
    cell21 != cell19 )).

fof(tlhfof39809,axiom,(
    cell21 != cell8 )).

fof(tlhfof39810,axiom,(
    cell21 != cell18 )).

fof(tlhfof39811,axiom,(
    cell21 != cell7 )).

fof(tlhfof39812,axiom,(
    cell21 != cell17 )).

fof(tlhfof39813,axiom,(
    cell21 != cell6 )).

fof(tlhfof39814,axiom,(
    cell21 != cell16 )).

fof(tlhfof39815,axiom,(
    cell21 != cell5 )).

fof(tlhfof39816,axiom,(
    cell21 != cell15 )).

fof(tlhfof39817,axiom,(
    cell21 != cell4 )).

fof(tlhfof39818,axiom,(
    cell21 != cell14 )).

fof(tlhfof39819,axiom,(
    cell21 != cell3 )).

fof(tlhfof39820,axiom,(
    cell21 != cell13 )).

fof(tlhfof39821,axiom,(
    cell21 != cell2 )).

fof(tlhfof39822,axiom,(
    cell21 != cell12 )).

fof(tlhfof39823,axiom,(
    cell21 != cell1 )).

fof(tlhfof39824,axiom,(
    cell21 != cell11 )).

fof(tlhfof39825,axiom,(
    cell21 != cell20 )).

fof(tlhfof39826,axiom,(
    cell10 != cell9 )).

fof(tlhfof39827,axiom,(
    cell10 != cell19 )).

fof(tlhfof39828,axiom,(
    cell10 != cell8 )).

fof(tlhfof39829,axiom,(
    cell10 != cell18 )).

fof(tlhfof39830,axiom,(
    cell10 != cell7 )).

fof(tlhfof39831,axiom,(
    cell10 != cell17 )).

fof(tlhfof39832,axiom,(
    cell10 != cell6 )).

fof(tlhfof39833,axiom,(
    cell10 != cell16 )).

fof(tlhfof39834,axiom,(
    cell10 != cell5 )).

fof(tlhfof39835,axiom,(
    cell10 != cell15 )).

fof(tlhfof39836,axiom,(
    cell10 != cell4 )).

fof(tlhfof39837,axiom,(
    cell10 != cell14 )).

fof(tlhfof39838,axiom,(
    cell10 != cell3 )).

fof(tlhfof39839,axiom,(
    cell10 != cell13 )).

fof(tlhfof39840,axiom,(
    cell10 != cell2 )).

fof(tlhfof39841,axiom,(
    cell10 != cell12 )).

fof(tlhfof39842,axiom,(
    cell10 != cell1 )).

fof(tlhfof39843,axiom,(
    cell10 != cell11 )).

fof(tlhfof39844,axiom,(
    cell10 != cell20 )).

fof(tlhfof39845,axiom,(
    cell9 != cell19 )).

fof(tlhfof39846,axiom,(
    cell9 != cell8 )).

fof(tlhfof39847,axiom,(
    cell9 != cell18 )).

fof(tlhfof39848,axiom,(
    cell9 != cell7 )).

fof(tlhfof39849,axiom,(
    cell9 != cell17 )).

fof(tlhfof39850,axiom,(
    cell9 != cell6 )).

fof(tlhfof39851,axiom,(
    cell9 != cell16 )).

fof(tlhfof39852,axiom,(
    cell9 != cell5 )).

fof(tlhfof39853,axiom,(
    cell9 != cell15 )).

fof(tlhfof39854,axiom,(
    cell9 != cell4 )).

fof(tlhfof39855,axiom,(
    cell9 != cell14 )).

fof(tlhfof39856,axiom,(
    cell9 != cell3 )).

fof(tlhfof39857,axiom,(
    cell9 != cell13 )).

fof(tlhfof39858,axiom,(
    cell9 != cell2 )).

fof(tlhfof39859,axiom,(
    cell9 != cell12 )).

fof(tlhfof39860,axiom,(
    cell9 != cell1 )).

fof(tlhfof39861,axiom,(
    cell9 != cell11 )).

fof(tlhfof39862,axiom,(
    cell9 != cell20 )).

fof(tlhfof39863,axiom,(
    cell19 != cell8 )).

fof(tlhfof39864,axiom,(
    cell19 != cell18 )).

fof(tlhfof39865,axiom,(
    cell19 != cell7 )).

fof(tlhfof39866,axiom,(
    cell19 != cell17 )).

fof(tlhfof39867,axiom,(
    cell19 != cell6 )).

fof(tlhfof39868,axiom,(
    cell19 != cell16 )).

fof(tlhfof39869,axiom,(
    cell19 != cell5 )).

fof(tlhfof39870,axiom,(
    cell19 != cell15 )).

fof(tlhfof39871,axiom,(
    cell19 != cell4 )).

fof(tlhfof39872,axiom,(
    cell19 != cell14 )).

fof(tlhfof39873,axiom,(
    cell19 != cell3 )).

fof(tlhfof39874,axiom,(
    cell19 != cell13 )).

fof(tlhfof39875,axiom,(
    cell19 != cell2 )).

fof(tlhfof39876,axiom,(
    cell19 != cell12 )).

fof(tlhfof39877,axiom,(
    cell19 != cell1 )).

fof(tlhfof39878,axiom,(
    cell19 != cell11 )).

fof(tlhfof39879,axiom,(
    cell19 != cell20 )).

fof(tlhfof39880,axiom,(
    cell8 != cell18 )).

fof(tlhfof39881,axiom,(
    cell8 != cell7 )).

fof(tlhfof39882,axiom,(
    cell8 != cell17 )).

fof(tlhfof39883,axiom,(
    cell8 != cell6 )).

fof(tlhfof39884,axiom,(
    cell8 != cell16 )).

fof(tlhfof39885,axiom,(
    cell8 != cell5 )).

fof(tlhfof39886,axiom,(
    cell8 != cell15 )).

fof(tlhfof39887,axiom,(
    cell8 != cell4 )).

fof(tlhfof39888,axiom,(
    cell8 != cell14 )).

fof(tlhfof39889,axiom,(
    cell8 != cell3 )).

fof(tlhfof39890,axiom,(
    cell8 != cell13 )).

fof(tlhfof39891,axiom,(
    cell8 != cell2 )).

fof(tlhfof39892,axiom,(
    cell8 != cell12 )).

fof(tlhfof39893,axiom,(
    cell8 != cell1 )).

fof(tlhfof39894,axiom,(
    cell8 != cell11 )).

fof(tlhfof39895,axiom,(
    cell8 != cell20 )).

fof(tlhfof39896,axiom,(
    cell18 != cell7 )).

fof(tlhfof39897,axiom,(
    cell18 != cell17 )).

fof(tlhfof39898,axiom,(
    cell18 != cell6 )).

fof(tlhfof39899,axiom,(
    cell18 != cell16 )).

fof(tlhfof39900,axiom,(
    cell18 != cell5 )).

fof(tlhfof39901,axiom,(
    cell18 != cell15 )).

fof(tlhfof39902,axiom,(
    cell18 != cell4 )).

fof(tlhfof39903,axiom,(
    cell18 != cell14 )).

fof(tlhfof39904,axiom,(
    cell18 != cell3 )).

fof(tlhfof39905,axiom,(
    cell18 != cell13 )).

fof(tlhfof39906,axiom,(
    cell18 != cell2 )).

fof(tlhfof39907,axiom,(
    cell18 != cell12 )).

fof(tlhfof39908,axiom,(
    cell18 != cell1 )).

fof(tlhfof39909,axiom,(
    cell18 != cell11 )).

fof(tlhfof39910,axiom,(
    cell18 != cell20 )).

fof(tlhfof39911,axiom,(
    cell7 != cell17 )).

fof(tlhfof39912,axiom,(
    cell7 != cell6 )).

fof(tlhfof39913,axiom,(
    cell7 != cell16 )).

fof(tlhfof39914,axiom,(
    cell7 != cell5 )).

fof(tlhfof39915,axiom,(
    cell7 != cell15 )).

fof(tlhfof39916,axiom,(
    cell7 != cell4 )).

fof(tlhfof39917,axiom,(
    cell7 != cell14 )).

fof(tlhfof39918,axiom,(
    cell7 != cell3 )).

fof(tlhfof39919,axiom,(
    cell7 != cell13 )).

fof(tlhfof39920,axiom,(
    cell7 != cell2 )).

fof(tlhfof39921,axiom,(
    cell7 != cell12 )).

fof(tlhfof39922,axiom,(
    cell7 != cell1 )).

fof(tlhfof39923,axiom,(
    cell7 != cell11 )).

fof(tlhfof39924,axiom,(
    cell7 != cell20 )).

fof(tlhfof39925,axiom,(
    cell17 != cell6 )).

fof(tlhfof39926,axiom,(
    cell17 != cell16 )).

fof(tlhfof39927,axiom,(
    cell17 != cell5 )).

fof(tlhfof39928,axiom,(
    cell17 != cell15 )).

fof(tlhfof39929,axiom,(
    cell17 != cell4 )).

fof(tlhfof39930,axiom,(
    cell17 != cell14 )).

fof(tlhfof39931,axiom,(
    cell17 != cell3 )).

fof(tlhfof39932,axiom,(
    cell17 != cell13 )).

fof(tlhfof39933,axiom,(
    cell17 != cell2 )).

fof(tlhfof39934,axiom,(
    cell17 != cell12 )).

fof(tlhfof39935,axiom,(
    cell17 != cell1 )).

fof(tlhfof39936,axiom,(
    cell17 != cell11 )).

fof(tlhfof39937,axiom,(
    cell17 != cell20 )).

fof(tlhfof39938,axiom,(
    cell6 != cell16 )).

fof(tlhfof39939,axiom,(
    cell6 != cell5 )).

fof(tlhfof39940,axiom,(
    cell6 != cell15 )).

fof(tlhfof39941,axiom,(
    cell6 != cell4 )).

fof(tlhfof39942,axiom,(
    cell6 != cell14 )).

fof(tlhfof39943,axiom,(
    cell6 != cell3 )).

fof(tlhfof39944,axiom,(
    cell6 != cell13 )).

fof(tlhfof39945,axiom,(
    cell6 != cell2 )).

fof(tlhfof39946,axiom,(
    cell6 != cell12 )).

fof(tlhfof39947,axiom,(
    cell6 != cell1 )).

fof(tlhfof39948,axiom,(
    cell6 != cell11 )).

fof(tlhfof39949,axiom,(
    cell6 != cell20 )).

fof(tlhfof39950,axiom,(
    cell16 != cell5 )).

fof(tlhfof39951,axiom,(
    cell16 != cell15 )).

fof(tlhfof39952,axiom,(
    cell16 != cell4 )).

fof(tlhfof39953,axiom,(
    cell16 != cell14 )).

fof(tlhfof39954,axiom,(
    cell16 != cell3 )).

fof(tlhfof39955,axiom,(
    cell16 != cell13 )).

fof(tlhfof39956,axiom,(
    cell16 != cell2 )).

fof(tlhfof39957,axiom,(
    cell16 != cell12 )).

fof(tlhfof39958,axiom,(
    cell16 != cell1 )).

fof(tlhfof39959,axiom,(
    cell16 != cell11 )).

fof(tlhfof39960,axiom,(
    cell16 != cell20 )).

fof(tlhfof39961,axiom,(
    cell5 != cell15 )).

fof(tlhfof39962,axiom,(
    cell5 != cell4 )).

fof(tlhfof39963,axiom,(
    cell5 != cell14 )).

fof(tlhfof39964,axiom,(
    cell5 != cell3 )).

fof(tlhfof39965,axiom,(
    cell5 != cell13 )).

fof(tlhfof39966,axiom,(
    cell5 != cell2 )).

fof(tlhfof39967,axiom,(
    cell5 != cell12 )).

fof(tlhfof39968,axiom,(
    cell5 != cell1 )).

fof(tlhfof39969,axiom,(
    cell5 != cell11 )).

fof(tlhfof39970,axiom,(
    cell5 != cell20 )).

fof(tlhfof39971,axiom,(
    cell15 != cell4 )).

fof(tlhfof39972,axiom,(
    cell15 != cell14 )).

fof(tlhfof39973,axiom,(
    cell15 != cell3 )).

fof(tlhfof39974,axiom,(
    cell15 != cell13 )).

fof(tlhfof39975,axiom,(
    cell15 != cell2 )).

fof(tlhfof39976,axiom,(
    cell15 != cell12 )).

fof(tlhfof39977,axiom,(
    cell15 != cell1 )).

fof(tlhfof39978,axiom,(
    cell15 != cell11 )).

fof(tlhfof39979,axiom,(
    cell15 != cell20 )).

fof(tlhfof39980,axiom,(
    cell4 != cell14 )).

fof(tlhfof39981,axiom,(
    cell4 != cell3 )).

fof(tlhfof39982,axiom,(
    cell4 != cell13 )).

fof(tlhfof39983,axiom,(
    cell4 != cell2 )).

fof(tlhfof39984,axiom,(
    cell4 != cell12 )).

fof(tlhfof39985,axiom,(
    cell4 != cell1 )).

fof(tlhfof39986,axiom,(
    cell4 != cell11 )).

fof(tlhfof39987,axiom,(
    cell4 != cell20 )).

fof(tlhfof39988,axiom,(
    cell14 != cell3 )).

fof(tlhfof39989,axiom,(
    cell14 != cell13 )).

fof(tlhfof39990,axiom,(
    cell14 != cell2 )).

fof(tlhfof39991,axiom,(
    cell14 != cell12 )).

fof(tlhfof39992,axiom,(
    cell14 != cell1 )).

fof(tlhfof39993,axiom,(
    cell14 != cell11 )).

fof(tlhfof39994,axiom,(
    cell14 != cell20 )).

fof(tlhfof39995,axiom,(
    cell3 != cell13 )).

fof(tlhfof39996,axiom,(
    cell3 != cell2 )).

fof(tlhfof39997,axiom,(
    cell3 != cell12 )).

fof(tlhfof39998,axiom,(
    cell3 != cell1 )).

fof(tlhfof39999,axiom,(
    cell3 != cell11 )).

fof(tlhfof40000,axiom,(
    cell3 != cell20 )).

fof(tlhfof40001,axiom,(
    cell13 != cell2 )).

fof(tlhfof40002,axiom,(
    cell13 != cell12 )).

fof(tlhfof40003,axiom,(
    cell13 != cell1 )).

fof(tlhfof40004,axiom,(
    cell13 != cell11 )).

fof(tlhfof40005,axiom,(
    cell13 != cell20 )).

fof(tlhfof40006,axiom,(
    cell2 != cell12 )).

fof(tlhfof40007,axiom,(
    cell2 != cell1 )).

fof(tlhfof40008,axiom,(
    cell2 != cell11 )).

fof(tlhfof40009,axiom,(
    cell2 != cell20 )).

fof(tlhfof40010,axiom,(
    cell12 != cell1 )).

fof(tlhfof40011,axiom,(
    cell12 != cell11 )).

fof(tlhfof40012,axiom,(
    cell12 != cell20 )).

fof(tlhfof40013,axiom,(
    cell1 != cell11 )).

fof(tlhfof40014,axiom,(
    cell1 != cell20 )).

fof(tlhfof40015,axiom,(
    cell11 != cell20 )).

fof(tlhfof40016,conjecture,(
    ~ westof(cell11,cell20) )).

%------------------------------------------------------------------------------
