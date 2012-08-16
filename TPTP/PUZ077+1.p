%------------------------------------------------------------------------------
% File     : PUZ077+1 : TPTP v5.2.0. Released v3.5.0.
% Domain   : Puzzles
% Problem  : Cell is west of the other in the Wumpus world
% Version  : Especial.
% English  :

% Refs     : [Hin07] Hinrichs (2007), Email to Geoff Sutcliffe
% Source   : [Hin07]
% Names    : westof-pos [Hin07]

% Status   : Theorem
% Rating   : 0.85 v5.2.0, 0.90 v5.0.0, 0.88 v4.1.0, 0.87 v4.0.0, 0.88 v3.7.0, 0.71 v3.5.0
% Syntax   : Number of formulae    :    7 (   1 unit)
%            Number of atoms       :  462 ( 360 equality)
%            Maximal formula depth :   94 (  37 average)
%            Number of connectives :  455 (   0   ~; 196   |; 253   &)
%                                         (   6 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    7 (   0 propositional; 2-2 arity)
%            Number of functors    :  100 ( 100 constant; 0-0 arity)
%            Number of variables   :   85 (   0 sgn;  12   !;  73   ?)
%            Maximal term depth    :    1 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments :
%------------------------------------------------------------------------------
fof(tlhfof34975,axiom,(
    ! [TLH34896,TLH34897] :
      ( west(TLH34897,TLH34896)
    <=> ( ( TLH34897 = cell1
          & TLH34896 = cell11 )
        | ( TLH34897 = cell2
          & TLH34896 = cell12 )
        | ( TLH34897 = cell3
          & TLH34896 = cell13 )
        | ( TLH34897 = cell4
          & TLH34896 = cell14 )
        | ( TLH34897 = cell5
          & TLH34896 = cell15 )
        | ( TLH34897 = cell6
          & TLH34896 = cell16 )
        | ( TLH34897 = cell7
          & TLH34896 = cell17 )
        | ( TLH34897 = cell8
          & TLH34896 = cell18 )
        | ( TLH34897 = cell9
          & TLH34896 = cell19 )
        | ( TLH34897 = cell10
          & TLH34896 = cell20 )
        | ( TLH34897 = cell11
          & TLH34896 = cell21 )
        | ( TLH34897 = cell12
          & TLH34896 = cell22 )
        | ( TLH34897 = cell13
          & TLH34896 = cell23 )
        | ( TLH34897 = cell14
          & TLH34896 = cell24 )
        | ( TLH34897 = cell15
          & TLH34896 = cell25 )
        | ( TLH34897 = cell16
          & TLH34896 = cell26 )
        | ( TLH34897 = cell17
          & TLH34896 = cell27 )
        | ( TLH34897 = cell18
          & TLH34896 = cell28 )
        | ( TLH34897 = cell19
          & TLH34896 = cell29 )
        | ( TLH34897 = cell20
          & TLH34896 = cell30 )
        | ( TLH34897 = cell21
          & TLH34896 = cell31 )
        | ( TLH34897 = cell22
          & TLH34896 = cell32 )
        | ( TLH34897 = cell23
          & TLH34896 = cell33 )
        | ( TLH34897 = cell24
          & TLH34896 = cell34 )
        | ( TLH34897 = cell25
          & TLH34896 = cell35 )
        | ( TLH34897 = cell26
          & TLH34896 = cell36 )
        | ( TLH34897 = cell27
          & TLH34896 = cell37 )
        | ( TLH34897 = cell28
          & TLH34896 = cell38 )
        | ( TLH34897 = cell29
          & TLH34896 = cell39 )
        | ( TLH34897 = cell30
          & TLH34896 = cell40 )
        | ( TLH34897 = cell31
          & TLH34896 = cell41 )
        | ( TLH34897 = cell32
          & TLH34896 = cell42 )
        | ( TLH34897 = cell33
          & TLH34896 = cell43 )
        | ( TLH34897 = cell34
          & TLH34896 = cell44 )
        | ( TLH34897 = cell35
          & TLH34896 = cell45 )
        | ( TLH34897 = cell36
          & TLH34896 = cell46 )
        | ( TLH34897 = cell37
          & TLH34896 = cell47 )
        | ( TLH34897 = cell38
          & TLH34896 = cell48 )
        | ( TLH34897 = cell39
          & TLH34896 = cell49 )
        | ( TLH34897 = cell40
          & TLH34896 = cell50 )
        | ( TLH34897 = cell41
          & TLH34896 = cell51 )
        | ( TLH34897 = cell42
          & TLH34896 = cell52 )
        | ( TLH34897 = cell43
          & TLH34896 = cell53 )
        | ( TLH34897 = cell44
          & TLH34896 = cell54 )
        | ( TLH34897 = cell45
          & TLH34896 = cell55 )
        | ( TLH34897 = cell46
          & TLH34896 = cell56 )
        | ( TLH34897 = cell47
          & TLH34896 = cell57 )
        | ( TLH34897 = cell48
          & TLH34896 = cell58 )
        | ( TLH34897 = cell49
          & TLH34896 = cell59 )
        | ( TLH34897 = cell50
          & TLH34896 = cell60 )
        | ( TLH34897 = cell51
          & TLH34896 = cell61 )
        | ( TLH34897 = cell52
          & TLH34896 = cell62 )
        | ( TLH34897 = cell53
          & TLH34896 = cell63 )
        | ( TLH34897 = cell54
          & TLH34896 = cell64 )
        | ( TLH34897 = cell55
          & TLH34896 = cell65 )
        | ( TLH34897 = cell56
          & TLH34896 = cell66 )
        | ( TLH34897 = cell57
          & TLH34896 = cell67 )
        | ( TLH34897 = cell58
          & TLH34896 = cell68 )
        | ( TLH34897 = cell59
          & TLH34896 = cell69 )
        | ( TLH34897 = cell60
          & TLH34896 = cell70 )
        | ( TLH34897 = cell61
          & TLH34896 = cell71 )
        | ( TLH34897 = cell62
          & TLH34896 = cell72 )
        | ( TLH34897 = cell63
          & TLH34896 = cell73 )
        | ( TLH34897 = cell64
          & TLH34896 = cell74 )
        | ( TLH34897 = cell65
          & TLH34896 = cell75 )
        | ( TLH34897 = cell66
          & TLH34896 = cell76 )
        | ( TLH34897 = cell67
          & TLH34896 = cell77 )
        | ( TLH34897 = cell68
          & TLH34896 = cell78 )
        | ( TLH34897 = cell69
          & TLH34896 = cell79 )
        | ( TLH34897 = cell70
          & TLH34896 = cell80 )
        | ( TLH34897 = cell71
          & TLH34896 = cell81 )
        | ( TLH34897 = cell72
          & TLH34896 = cell82 )
        | ( TLH34897 = cell73
          & TLH34896 = cell83 )
        | ( TLH34897 = cell74
          & TLH34896 = cell84 )
        | ( TLH34897 = cell75
          & TLH34896 = cell85 )
        | ( TLH34897 = cell76
          & TLH34896 = cell86 )
        | ( TLH34897 = cell77
          & TLH34896 = cell87 )
        | ( TLH34897 = cell78
          & TLH34896 = cell88 )
        | ( TLH34897 = cell79
          & TLH34896 = cell89 )
        | ( TLH34897 = cell80
          & TLH34896 = cell90 )
        | ( TLH34897 = cell81
          & TLH34896 = cell91 )
        | ( TLH34897 = cell82
          & TLH34896 = cell92 )
        | ( TLH34897 = cell83
          & TLH34896 = cell93 )
        | ( TLH34897 = cell84
          & TLH34896 = cell94 )
        | ( TLH34897 = cell85
          & TLH34896 = cell95 )
        | ( TLH34897 = cell86
          & TLH34896 = cell96 )
        | ( TLH34897 = cell87
          & TLH34896 = cell97 )
        | ( TLH34897 = cell88
          & TLH34896 = cell98 )
        | ( TLH34897 = cell89
          & TLH34896 = cell99 )
        | ( TLH34897 = cell90
          & TLH34896 = cell100 ) ) ) )).

fof(tlhfof34976,axiom,(
    ! [TLH34898,TLH34899] :
      ( north(TLH34899,TLH34898)
    <=> ( ( TLH34899 = cell1
          & TLH34898 = cell2 )
        | ( TLH34899 = cell2
          & TLH34898 = cell3 )
        | ( TLH34899 = cell3
          & TLH34898 = cell4 )
        | ( TLH34899 = cell4
          & TLH34898 = cell5 )
        | ( TLH34899 = cell5
          & TLH34898 = cell6 )
        | ( TLH34899 = cell6
          & TLH34898 = cell7 )
        | ( TLH34899 = cell7
          & TLH34898 = cell8 )
        | ( TLH34899 = cell8
          & TLH34898 = cell9 )
        | ( TLH34899 = cell9
          & TLH34898 = cell10 )
        | ( TLH34899 = cell11
          & TLH34898 = cell12 )
        | ( TLH34899 = cell12
          & TLH34898 = cell13 )
        | ( TLH34899 = cell13
          & TLH34898 = cell14 )
        | ( TLH34899 = cell14
          & TLH34898 = cell15 )
        | ( TLH34899 = cell15
          & TLH34898 = cell16 )
        | ( TLH34899 = cell16
          & TLH34898 = cell17 )
        | ( TLH34899 = cell17
          & TLH34898 = cell18 )
        | ( TLH34899 = cell18
          & TLH34898 = cell19 )
        | ( TLH34899 = cell19
          & TLH34898 = cell20 )
        | ( TLH34899 = cell21
          & TLH34898 = cell22 )
        | ( TLH34899 = cell22
          & TLH34898 = cell23 )
        | ( TLH34899 = cell23
          & TLH34898 = cell24 )
        | ( TLH34899 = cell24
          & TLH34898 = cell25 )
        | ( TLH34899 = cell25
          & TLH34898 = cell26 )
        | ( TLH34899 = cell26
          & TLH34898 = cell27 )
        | ( TLH34899 = cell27
          & TLH34898 = cell28 )
        | ( TLH34899 = cell28
          & TLH34898 = cell29 )
        | ( TLH34899 = cell29
          & TLH34898 = cell30 )
        | ( TLH34899 = cell31
          & TLH34898 = cell32 )
        | ( TLH34899 = cell32
          & TLH34898 = cell33 )
        | ( TLH34899 = cell33
          & TLH34898 = cell34 )
        | ( TLH34899 = cell34
          & TLH34898 = cell35 )
        | ( TLH34899 = cell35
          & TLH34898 = cell36 )
        | ( TLH34899 = cell36
          & TLH34898 = cell37 )
        | ( TLH34899 = cell37
          & TLH34898 = cell38 )
        | ( TLH34899 = cell38
          & TLH34898 = cell39 )
        | ( TLH34899 = cell39
          & TLH34898 = cell40 )
        | ( TLH34899 = cell41
          & TLH34898 = cell42 )
        | ( TLH34899 = cell42
          & TLH34898 = cell43 )
        | ( TLH34899 = cell43
          & TLH34898 = cell44 )
        | ( TLH34899 = cell44
          & TLH34898 = cell45 )
        | ( TLH34899 = cell45
          & TLH34898 = cell46 )
        | ( TLH34899 = cell46
          & TLH34898 = cell47 )
        | ( TLH34899 = cell47
          & TLH34898 = cell48 )
        | ( TLH34899 = cell48
          & TLH34898 = cell49 )
        | ( TLH34899 = cell49
          & TLH34898 = cell50 )
        | ( TLH34899 = cell51
          & TLH34898 = cell52 )
        | ( TLH34899 = cell52
          & TLH34898 = cell53 )
        | ( TLH34899 = cell53
          & TLH34898 = cell54 )
        | ( TLH34899 = cell54
          & TLH34898 = cell55 )
        | ( TLH34899 = cell55
          & TLH34898 = cell56 )
        | ( TLH34899 = cell56
          & TLH34898 = cell57 )
        | ( TLH34899 = cell57
          & TLH34898 = cell58 )
        | ( TLH34899 = cell58
          & TLH34898 = cell59 )
        | ( TLH34899 = cell59
          & TLH34898 = cell60 )
        | ( TLH34899 = cell61
          & TLH34898 = cell62 )
        | ( TLH34899 = cell62
          & TLH34898 = cell63 )
        | ( TLH34899 = cell63
          & TLH34898 = cell64 )
        | ( TLH34899 = cell64
          & TLH34898 = cell65 )
        | ( TLH34899 = cell65
          & TLH34898 = cell66 )
        | ( TLH34899 = cell66
          & TLH34898 = cell67 )
        | ( TLH34899 = cell67
          & TLH34898 = cell68 )
        | ( TLH34899 = cell68
          & TLH34898 = cell69 )
        | ( TLH34899 = cell69
          & TLH34898 = cell70 )
        | ( TLH34899 = cell71
          & TLH34898 = cell72 )
        | ( TLH34899 = cell72
          & TLH34898 = cell73 )
        | ( TLH34899 = cell73
          & TLH34898 = cell74 )
        | ( TLH34899 = cell74
          & TLH34898 = cell75 )
        | ( TLH34899 = cell75
          & TLH34898 = cell76 )
        | ( TLH34899 = cell76
          & TLH34898 = cell77 )
        | ( TLH34899 = cell77
          & TLH34898 = cell78 )
        | ( TLH34899 = cell78
          & TLH34898 = cell79 )
        | ( TLH34899 = cell79
          & TLH34898 = cell80 )
        | ( TLH34899 = cell81
          & TLH34898 = cell82 )
        | ( TLH34899 = cell82
          & TLH34898 = cell83 )
        | ( TLH34899 = cell83
          & TLH34898 = cell84 )
        | ( TLH34899 = cell84
          & TLH34898 = cell85 )
        | ( TLH34899 = cell85
          & TLH34898 = cell86 )
        | ( TLH34899 = cell86
          & TLH34898 = cell87 )
        | ( TLH34899 = cell87
          & TLH34898 = cell88 )
        | ( TLH34899 = cell88
          & TLH34898 = cell89 )
        | ( TLH34899 = cell89
          & TLH34898 = cell90 )
        | ( TLH34899 = cell91
          & TLH34898 = cell92 )
        | ( TLH34899 = cell92
          & TLH34898 = cell93 )
        | ( TLH34899 = cell93
          & TLH34898 = cell94 )
        | ( TLH34899 = cell94
          & TLH34898 = cell95 )
        | ( TLH34899 = cell95
          & TLH34898 = cell96 )
        | ( TLH34899 = cell96
          & TLH34898 = cell97 )
        | ( TLH34899 = cell97
          & TLH34898 = cell98 )
        | ( TLH34899 = cell98
          & TLH34898 = cell99 )
        | ( TLH34899 = cell99
          & TLH34898 = cell100 ) ) ) )).

fof(tlhfof34977,axiom,(
    ! [TLH34901,TLH34900] :
      ( duewest(TLH34900,TLH34901)
    <=> ( west(TLH34900,TLH34901)
        | ? [TLH34902] :
            ( west(TLH34900,TLH34902)
            & west(TLH34902,TLH34901) )
        | ? [TLH34903,TLH34904] :
            ( west(TLH34900,TLH34903)
            & west(TLH34903,TLH34904)
            & west(TLH34904,TLH34901) )
        | ? [TLH34905,TLH34906,TLH34907] :
            ( west(TLH34900,TLH34905)
            & west(TLH34905,TLH34906)
            & west(TLH34906,TLH34907)
            & west(TLH34907,TLH34901) )
        | ? [TLH34908,TLH34909,TLH34910,TLH34911] :
            ( west(TLH34900,TLH34908)
            & west(TLH34908,TLH34909)
            & west(TLH34909,TLH34910)
            & west(TLH34910,TLH34911)
            & west(TLH34911,TLH34901) )
        | ? [TLH34912,TLH34913,TLH34914,TLH34915,TLH34916] :
            ( west(TLH34900,TLH34912)
            & west(TLH34912,TLH34913)
            & west(TLH34913,TLH34914)
            & west(TLH34914,TLH34915)
            & west(TLH34915,TLH34916)
            & west(TLH34916,TLH34901) )
        | ? [TLH34917,TLH34918,TLH34919,TLH34920,TLH34921,TLH34922] :
            ( west(TLH34900,TLH34917)
            & west(TLH34917,TLH34918)
            & west(TLH34918,TLH34919)
            & west(TLH34919,TLH34920)
            & west(TLH34920,TLH34921)
            & west(TLH34921,TLH34922)
            & west(TLH34922,TLH34901) )
        | ? [TLH34923,TLH34924,TLH34925,TLH34926,TLH34927,TLH34928,TLH34929] :
            ( west(TLH34900,TLH34923)
            & west(TLH34923,TLH34924)
            & west(TLH34924,TLH34925)
            & west(TLH34925,TLH34926)
            & west(TLH34926,TLH34927)
            & west(TLH34927,TLH34928)
            & west(TLH34928,TLH34929)
            & west(TLH34929,TLH34901) )
        | ? [TLH34930,TLH34931,TLH34932,TLH34933,TLH34934,TLH34935,TLH34936,TLH34937] :
            ( west(TLH34900,TLH34930)
            & west(TLH34930,TLH34931)
            & west(TLH34931,TLH34932)
            & west(TLH34932,TLH34933)
            & west(TLH34933,TLH34934)
            & west(TLH34934,TLH34935)
            & west(TLH34935,TLH34936)
            & west(TLH34936,TLH34937)
            & west(TLH34937,TLH34901) ) ) ) )).

fof(tlhfof34978,axiom,(
    ! [TLH34939,TLH34938] :
      ( duenorth(TLH34938,TLH34939)
    <=> ( north(TLH34938,TLH34939)
        | ? [TLH34940] :
            ( north(TLH34938,TLH34940)
            & north(TLH34940,TLH34939) )
        | ? [TLH34941,TLH34942] :
            ( north(TLH34938,TLH34941)
            & north(TLH34941,TLH34942)
            & north(TLH34942,TLH34939) )
        | ? [TLH34943,TLH34944,TLH34945] :
            ( north(TLH34938,TLH34943)
            & north(TLH34943,TLH34944)
            & north(TLH34944,TLH34945)
            & north(TLH34945,TLH34939) )
        | ? [TLH34946,TLH34947,TLH34948,TLH34949] :
            ( north(TLH34938,TLH34946)
            & north(TLH34946,TLH34947)
            & north(TLH34947,TLH34948)
            & north(TLH34948,TLH34949)
            & north(TLH34949,TLH34939) )
        | ? [TLH34950,TLH34951,TLH34952,TLH34953,TLH34954] :
            ( north(TLH34938,TLH34950)
            & north(TLH34950,TLH34951)
            & north(TLH34951,TLH34952)
            & north(TLH34952,TLH34953)
            & north(TLH34953,TLH34954)
            & north(TLH34954,TLH34939) )
        | ? [TLH34955,TLH34956,TLH34957,TLH34958,TLH34959,TLH34960] :
            ( north(TLH34938,TLH34955)
            & north(TLH34955,TLH34956)
            & north(TLH34956,TLH34957)
            & north(TLH34957,TLH34958)
            & north(TLH34958,TLH34959)
            & north(TLH34959,TLH34960)
            & north(TLH34960,TLH34939) )
        | ? [TLH34961,TLH34962,TLH34963,TLH34964,TLH34965,TLH34966,TLH34967] :
            ( north(TLH34938,TLH34961)
            & north(TLH34961,TLH34962)
            & north(TLH34962,TLH34963)
            & north(TLH34963,TLH34964)
            & north(TLH34964,TLH34965)
            & north(TLH34965,TLH34966)
            & north(TLH34966,TLH34967)
            & north(TLH34967,TLH34939) )
        | ? [TLH34968,TLH34969,TLH34970,TLH34971,TLH34972,TLH34973,TLH34974,TLH34975] :
            ( north(TLH34938,TLH34968)
            & north(TLH34968,TLH34969)
            & north(TLH34969,TLH34970)
            & north(TLH34970,TLH34971)
            & north(TLH34971,TLH34972)
            & north(TLH34972,TLH34973)
            & north(TLH34973,TLH34974)
            & north(TLH34974,TLH34975)
            & north(TLH34975,TLH34939) ) ) ) )).

fof(tlhfof34979,axiom,(
    ! [Y,X] :
      ( vert(X,Y)
    <=> ( duenorth(X,Y)
        | duenorth(Y,X) ) ) )).

fof(tlhfof34980,axiom,(
    ! [Y,X] :
      ( westof(X,Y)
    <=> ( duewest(X,Y)
        | ? [Z] :
            ( vert(X,Z)
            & duewest(Z,Y) ) ) ) )).

fof(tlhfof34981,conjecture,(
    westof(cell1,cell100) )).

%------------------------------------------------------------------------------
