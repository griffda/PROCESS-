! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initial

    !! Routine to initialise
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use, intrinsic :: iso_fortran_env, only: dp=>real64
    use process_output
    use stellarator_module
    use stellarator_variables
    use numerics
    use define_iteration_variables

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  See which type of device is being modelled

    call devtyp

    !! boundl(ipnvars) /../ : lower bounds on iteration variables 
    !! boundu(ipnvars) /../ : upper bounds on iteration variables 

    ! Issue #287  The initialization subroutines for the iteration variables are called
    call init_itv_1       
    call init_itv_2
    call init_itv_3
    call init_itv_4
    call init_itv_5
    call init_itv_6
    call init_itv_7
    call init_itv_8
    call init_itv_9
    call init_itv_10
    call init_itv_11
    call init_itv_12
    call init_itv_13
    call init_itv_14
    call init_itv_15
    call init_itv_16
    call init_itv_17
    call init_itv_18
    call init_itv_19
    call init_itv_20
    call init_itv_21
    
    call init_itv_23
    
    call init_itv_25
    call init_itv_26
    call init_itv_27
    call init_itv_28
    call init_itv_29
    call init_itv_30
    call init_itv_31
    call init_itv_32
    call init_itv_33
    call init_itv_34
    call init_itv_35
    call init_itv_36
    call init_itv_37
    call init_itv_38
    call init_itv_39
    call init_itv_40
    call init_itv_41
    call init_itv_42
    
    call init_itv_44
    call init_itv_45
    call init_itv_46
    call init_itv_47
    call init_itv_48
    call init_itv_49
    call init_itv_50
    call init_itv_51
    call init_itv_52
    call init_itv_53
    call init_itv_54
    
    call init_itv_56
    call init_itv_57
    call init_itv_58
    call init_itv_59
    call init_itv_60
    call init_itv_61
    call init_itv_62
    call init_itv_63
    call init_itv_64
    call init_itv_65
    call init_itv_66
    call init_itv_67
    call init_itv_68
    call init_itv_69
    call init_itv_70
    call init_itv_71
    call init_itv_72
    call init_itv_73
    call init_itv_74
    call init_itv_75
    
    
    
    call init_itv_79
    
    
    
    
    
    
    
    
    
    call init_itv_89
    call init_itv_90
    call init_itv_91
    call init_itv_92
    call init_itv_93
    call init_itv_94
    call init_itv_95
    call init_itv_96
    call init_itv_97
    call init_itv_98
    !Not used
    call init_itv_102
    call init_itv_103
    call init_itv_104
    call init_itv_105
    call init_itv_106
    call init_itv_107
    call init_itv_108
    call init_itv_109
    call init_itv_110
    call init_itv_111
    call init_itv_112
    call init_itv_113
    call init_itv_114
    call init_itv_115
    call init_itv_116
    call init_itv_117
    call init_itv_118
    call init_itv_119
    call init_itv_120
    call init_itv_121
    call init_itv_122
    call init_itv_123
    call init_itv_124
    call init_itv_125
    call init_itv_126
    call init_itv_127
    call init_itv_128
    call init_itv_129
    call init_itv_130
    call init_itv_131
    call init_itv_132
    call init_itv_133
    call init_itv_134
    call init_itv_135
    call init_itv_136
    call init_itv_137
    call init_itv_138
    call init_itv_139
    call init_itv_140
    call init_itv_141
    call init_itv_142
    call init_itv_143
    call init_itv_144
    call init_itv_145
    call init_itv_146
    call init_itv_147
    call init_itv_148
    call init_itv_149
    call init_itv_150
    call init_itv_151
    call init_itv_152
    call init_itv_153
    call init_itv_154
    call init_itv_155
    call init_itv_156
    call init_itv_157
    call init_itv_158
    call init_itv_159
    call init_itv_160
    call init_itv_161
    call init_itv_162
    call init_itv_163
    call init_itv_164
    call init_itv_165
    call init_itv_166
    call init_itv_167
    call init_itv_168
    call init_itv_169
    call init_itv_170


    !    call init_itv_1
    !    !!  <LI> ( 1) aspect!!!
    !    call init_itv_2  
    !    !!  <LI> ( 2) bt!!!!
    !    call init_itv_3
    !    !!  <LI> ( 3) rmajor
    !    call init_itv_4
    !    !!  <LI> ( 4) te
    !    call init_itv_5
    !    !!  <LI> ( 5) beta
    !    call init_itv_6
    !    !!  <LI> ( 6) dene
    !    call init_itv_7
    !    !!  <LI> ( 7) rnbeam
    !    call init_itv_8  
    !    !!  <LI> ( 8) fbeta (f-value for equation 6)
    !    call init_itv_9 
    !    !!  <LI> ( 9) fdene (f-value for equation 5)
    !    lablxc(10) = 'hfact         '; boundl(10) = 0.100D0 ; boundu(10) = 3.000D0  
    !    !!  <LI> (10) hfact
    !    lablxc(11) = 'pheat         '; boundl(11) = 1.00D-3 ; boundu(11) = 1.000D3  
    !    !!  <LI> (11) pheat
    !    lablxc(12) = 'oacdcp        '; boundl(12) = 1.000D5 ; boundu(12) =  1.500D8 
    !    !!  <LI> (12) oacdcp
    !    lablxc(13) = 'tfcth         '; boundl(13) = 0.100D0 ; boundu(13) = 5.000D0
    !    !!  <LI> (13) tfcth (NOT RECOMMENDED)
    !    lablxc(14) = 'fwalld        '; boundl(14) = 0.001D0 ; boundu(14) = 1.000D0
    !    !!  <LI> (14) fwalld (f-value for equation 8)
    !    lablxc(15) = 'fvs           '; boundl(15) = 0.001D0 ; boundu(15) = 1.000D0
    !    !!  <LI> (15) fvs (f-value for equation 12)
    !    lablxc(16) = 'ohcth         '; boundl(16) = 0.010D0 ; boundu(16) = 10.00D0
    !    !!  <LI> (16) ohcth
    !    lablxc(17) = 'tdwell        '; boundl(17) = 0.100D0 ; boundu(17) = 1.000D8
    !    !!  <LI> (17) tdwell
    !    lablxc(18) = 'q             '; boundl(18) = 2.000D0 ; boundu(18) = 50.00D0
    !    !!  <LI> (18) q
    !    lablxc(19) = 'enbeam        '; boundl(19) = 1.000D0 ; boundu(19) = 1.000D6
    !    !!  <LI> (19) enbeam
    !    lablxc(20) = 'tcpav         '; boundl(20) = 40.00D0 ; boundu(20) = 3.000D2
    !    !!  <LI> (20) tcpav
    !    lablxc(21) = 'ftburn        '; boundl(21) = 0.001D0 ; boundu(21) = 1.000D0
    !    !!  <LI> (21) ftburn (f-value for equation 13)
    !    !!  <LI> (23) fcoolcp
    !    !!  <LI> (22) NOT USED
    !    lablxc(23) = 'fcoolcp       '; boundl(23) = 0.100D0 ; boundu(23) = 0.500D0
    !    !!  <LI> (25) fpnetel (f-value for equation 16)
    !    !!  <LI> (24) NOT USED
    !    lablxc(25) = 'fpnetel       '; boundl(25) = 0.001D0; boundu(25) = 1.000D0
    !    lablxc(26) = 'ffuspow       '; boundl(26) = 0.001D0; boundu(26) = 1.000D0
    !    !!  <LI> (26) ffuspow (f-value for equation 9)
    !    lablxc(27) = 'fhldiv        ';  boundl(27) = 0.001D0; boundu(27) = 1.000D0
    !    !!  <LI> (27) fhldiv (f-value for equation 18)
    !    lablxc(28) = 'fradpwr       '; boundl(28) = 0.001D0; boundu(28) = 0.990D0
    !    !!  <LI> (28) fradpwr (f-value for equation 17), total radiation fraction
    !    lablxc(29) = 'bore          '; boundl(29) = 0.100D0; boundu(29) = 10.00D0
    !    !!  <LI> (29) bore
    !    lablxc(30) = 'fmva          '; boundl(30) = 0.010D0; boundu(30) = 1.000D0
    !    !!  <LI> (30) fmva (f-value for equation 19)
    !    lablxc(31) = 'gapomin       '; boundl(31) = 0.001D0; boundu(31) = 1.000D1
    !    !!  <LI> (31) gapomin
    !    lablxc(32) = 'frminor       '; boundl(32) = 0.001D0; boundu(32) = 1.000D0
    !    !!  <LI> (32) frminor (f-value for equation 21)
    !    lablxc(33) = 'fportsz       '; boundl(33) = 0.001D0; boundu(33) = 1.000D0
    !    !!  <LI> (33) fportsz (f-value for equation 20)
    !    lablxc(34) = 'fdivcol       '; boundl(34) = 0.001D0; boundu(34) = 1.000D0
    !    !!  <LI> (34) fdivcol (f-value for equation 22)
    !    lablxc(35) = 'fpeakb        '; boundl(35) = 0.001D0; boundu(35) = 1.000D0
    !    !!  <LI> (35) fpeakb (f-value for equation 25)
    !    lablxc(36) = 'fbetatry      '; boundl(36) = 0.001D0; boundu(36) = 1.000D0
    !    !!  <LI> (36) fbetatry (f-value for equation 24)
    !    lablxc(37) = 'coheof        '; boundl(37) = 1.000D5; boundu(37) = 1.000D8
    !    !!  <LI> (37) coheof
    !    lablxc(38) = 'fjohc         '; boundl(38) = 0.010D0; boundu(38) = 1.000D0
    !    !!  <LI> (38) fjohc (f-value for equation 26)
    !    lablxc(39) = 'fjohc0        '; boundl(39) = 0.001D0; boundu(39) = 1.000D0
    !    !!  <LI> (39) fjohc0 (f-value for equation 27)
    !    lablxc(40) = 'fgamcd        '; boundl(40) = 0.001D0; boundu(40) = 1.000D0
    !    !!  <LI> (40) fgamcd (f-value for equation 37)
    !    lablxc(41) = 'fcohbop       '; boundl(41) = 0.001D0; boundu(41) = 1.000D0
    !    !!  <LI> (41) fcohbop
    !    lablxc(42) = 'gapoh         '; boundl(42) = 0.001D0; boundu(42) = 10.00D0
    !    !!  <LI> (42) gapoh
    !    !!  <LI> (44) fvsbrnni
    !    !!  <LI> (43) NOT USED
    !    lablxc(44) = 'fvsbrnni      '; boundl(44) = 0.001D0; boundu(44) = 1.000D0
    !    lablxc(45) = 'fqval         '; boundl(45) = 0.001D0; boundu(45) = 1.000D0
    !    !!  <LI> (45) fqval (f-value for equation 28)
    !    lablxc(46) = 'fpinj         '; boundl(46) = 0.001D0; boundu(46) = 1.000D0
    !    !!  <LI> (46) fpinj (f-value for equation 30)
    !    lablxc(47) = 'feffcd        '; boundl(47) = 0.001D0; boundu(47) = 1.000D0
    !    !!  <LI> (47) feffcd
    !    lablxc(48) = 'fstrcase      '; boundl(48) = 0.001D0; boundu(48) = 1.000D0
    !    !!  <LI> (48) fstrcase (f-value for equation 31)
    !    lablxc(49) = 'fstrcond      '; boundl(49) = 0.001D0; boundu(49) = 1.000D0
    !    !!  <LI> (49) fstrcond (f-value for equation 32)
    !    lablxc(50) = 'fiooic        '; boundl(50) = 0.001D0; boundu(50) = 1.000D0
    !    !!  <LI> (50) fiooic (f-value for equation 33)
    !    lablxc(51) = 'fvdump        '; boundl(51) = 0.001D0; boundu(51) = 1.000D0
    !    !!  <LI> (51) fvdump (f-value for equation 34)
    !    lablxc(52) = 'vdalw         '; boundl(52) = 0.001D0; boundu(52) = 1.000D6
    !    !!  <LI> (52) vdalw
    !    lablxc(53) = 'fjprot        '; boundl(53) = 0.001D0; boundu(53) = 1.000D0
    !    !!  <LI> (53) fjprot (f-value for equation 35)
    !    lablxc(54) = 'ftmargtf      '; boundl(54) = 0.001D0; boundu(54) = 1.000D0
    !    !!  <LI> (54) ftmargtf (f-value for equation 36)
    !    !!  <LI> (56) tdmptf
    !    !!  <LI> (55) obsolete
    !    lablxc(56) = 'tdmptf        '; boundl(56) = 0.100D0; boundu(56) = 100.0D0
    !    lablxc(57) = 'thkcas        '; boundl(57) = 0.050D0; boundu(57) = 1.000D0
    !    !!  <LI> (57) thkcas
    !    lablxc(58) = 'thwcndut      '; boundl(58) = 0.001D0; boundu(58) = 0.100D0
    !    !!  <LI> (58) thwcndut
    !    lablxc(59) = 'fcutfsu       '; boundl(59) = 0.001D0; boundu(59) = 1.000D0
    !    !!  <LI> (59) fcutfsu
    !    lablxc(60) = 'cpttf         '; boundl(60) = 0.001D0; boundu(60) = 4.000D4
    !    !!  <LI> (60) cpttf
    !    lablxc(61) = 'gapds         '; boundl(61) = 0.001D0; boundu(61) = 10.00D0
    !    !!  <LI> (61) gapds
    !    lablxc(62) = 'fdtmp         '; boundl(62) = 0.001D0; boundu(62) = 1.000D0
    !    !!  <LI> (62) fdtmp (f-value for equation 38)
    !    lablxc(63) = 'ftpeak        '; boundl(63) = 0.001D0; boundu(63) = 1.000D0
    !    !!  <LI> (63) ftpeak (f-value for equation 39)
    !    lablxc(64) = 'fauxmn        '; boundl(64) = 0.001D0; boundu(64) = 1.000D0
    !    !!  <LI> (64) fauxmn (f-value for equation 40)
    !    lablxc(65) = 'tohs          '; boundl(65) = 0.100D0; boundu(65) = 1.000D3
    !    !!  <LI> (65) tohs
    !    lablxc(66) = 'ftohs         '; boundl(66) = 0.001D0; boundu(66) = 1.000D0
    !    !!  <LI> (66) ftohs (f-value for equation 41)
    !    lablxc(67) = 'ftcycl        '; boundl(67) = 0.001D0; boundu(67) = 1.000D0
    !    !!  <LI> (67) ftcycl (f-value for equation 42)
    !    lablxc(68) = 'fptemp        '; boundl(68) = 0.001D0; boundu(68) = 1.000D0
    !    !!  <LI> (68) fptemp (f-value for equation 44)
    !    lablxc(69) = 'rcool         '; boundl(69) = 0.001D0; boundu(69) = 0.010D0
    !    !!  <LI> (69) rcool
    !    lablxc(70) = 'vcool         '; boundl(70) = 1.000D0; boundu(70) = 1.000D2
    !    !!  <LI> (70) vcool
    !    lablxc(71) = 'fq            '; boundl(71) = 0.001D0; boundu(71) = 1.000D0
    !    !!  <LI> (71) fq (f-value for equation 45)
    !    lablxc(72) = 'fipir         '; boundl(72) = 0.001D0; boundu(72) = 1.000D0
    !    !!  <LI> (72) fipir (f-value for equation 46)
    !    lablxc(73) = 'scrapli       '; boundl(73) = 0.001D0; boundu(73) = 10.00D0
    !    !!  <LI> (73) scrapli
    !    lablxc(74) = 'scraplo       '; boundl(74) = 0.001D0; boundu(74) = 10.00D0
    !    !!  <LI> (74) scraplo
    !    lablxc(75) = 'tfootfi       '; boundl(75) = 0.200D0; boundu(75) = 5.000D0
    !    !!  <LI> (75) tfootfi
    !    !!  <LI> (77) NOT USED
    !    !!  <LI> (76) NOT USED
    !    !!  <LI> (78) NOT USED
    !    !!  <LI> (79) fbetap (f-value for equation 48)
    !    lablxc(79) = 'fbetap        '; boundl(79) = 0.001D0; boundu(79) = 1.000D0
    !    !!  <LI> (81) NOT USED
    !    !!  <LI> (80) NOT USED
    !    !!  <LI> (82) NOT USED
    !    !!  <LI> (83) NOT USED
    !    !!  <LI> (84) NOT USED
    !    !!  <LI> (85) NOT USED
    !    !!  <LI> (86) NOT USED
    !    !!  <LI> (87) NOT USED
    !    !!  <LI> (88) NOT USED
    !    !!  <LI> (89) ftbr (f-value for equation 52)
    !    lablxc(89) = 'ftbr          '; boundl(89) = 0.001D0; boundu(89) = 1.000D0
    !    lablxc(90) = 'blbuith       '; boundl(90) = 0.001D0; boundu(90) = 2.000D0
    !    !!  <LI> (90) blbuith
    !    lablxc(91) = 'blbuoth       '; boundl(91) = 0.001D0; boundu(91) = 2.000D0
    !    !!  <LI> (91) blbuoth
    !    lablxc(92) = 'fflutf        '; boundl(92) = 0.001D0; boundu(92) = 1.000D0
    !    !!  <LI> (92) fflutf (f-value for equation 53)
    !    lablxc(93) = 'shldith       '; boundl(93) = 0.001D0; boundu(93) = 10.00D0
    !    !!  <LI> (93) shldith
    !    lablxc(94) = 'shldoth       '; boundl(94) = 0.001D0; boundu(94) = 10.00D0
    !    !!  <LI> (94) shldoth
    !    lablxc(95) = 'fptfnuc       '; boundl(95) = 0.001D0; boundu(95) = 1.000D0
    !    !!  <LI> (95) fptfnuc (f-value for equation 54)
    !    lablxc(96) = 'fvvhe         '; boundl(96) = 0.001D0; boundu(96) = 1.000D0
    !    !!  <LI> (96) fvvhe (f-value for equation 55)
    !    lablxc(97) = 'fpsepr        '; boundl(97) = 0.001D0; boundu(97) = 1.000D0
    !    !!  <LI> (97) fpsepr (f-value for equation 56)
    !    lablxc(98) = 'li6enrich     '; boundl(98) = 10.00D0; boundu(98) = 100.0D0
    !    !!  <LI> (98) li6enrich
    !    !!  <LI> (100) NOT USED
    !    !!  <LI> (99) NOT USED
    !    !!  <LI> (101) NOT USED
    !    !!  <LI> (102) fimpvar
    !    lablxc(102) = 'fimpvar       '; boundl(102) = 1.00D-6; boundu(102) = 0.010D0
    !    lablxc(103) = 'flhthresh     '; boundl(103) = 1.000D0; boundu(103) = 1.000D6
    !    !!  <LI> (103) flhthresh (f-value for equation 15)
    !    lablxc(104) = 'fcwr          '; boundl(104) = 0.001D0; boundu(104) = 1.000D0
    !    !!  <LI> (104) fcwr (f-value for equation 23)
    !    lablxc(105) = 'fnbshinef     '; boundl(105) = 0.001D0; boundu(105) = 1.000D0
    !    !!  <LI> (105) fnbshinef (f-value for equation 59)
    !    lablxc(106) = 'ftmargoh      '; boundl(106) = 0.001D0; boundu(106) = 1.000D0
    !    !!  <LI> (106) ftmargoh (f-value for equation 60)
    !    lablxc(107) = 'favail        '; boundl(107) = 0.001D0; boundu(107) = 1.000D0
    !    !!  <LI> (107) favail (f-value for equation 61)
    !    lablxc(108) = 'breeder_f     '; boundl(108) = 0.060D0; boundu(108) = 1.000D0
    !    !!  <LI> (108) breeder_f: Volume of Li4SiO4 / (Volume of Be12Ti + Li4SiO4)
    !    lablxc(109) = 'ralpne        '; boundl(109) = 0.050D0; boundu(109) = 0.150D0
    !    !!  <LI> (109) ralpne: thermal alpha density / electron density
    !    !!       to energy confinement times (f-value for equation 62)
    !    !!  <LI> (110) ftaulimit: Lower limit on taup/taueff the ratio of alpha particle
    !    lablxc(110) = 'ftaulimit     '; boundl(110) = 0.001D0; boundu(110) = 1.000D0
    !    !!       number of vacuum pumps <  TF coils (f-value for equation 63)
    !    !!  <LI> (111) fniterpump: f-value for constraint that
    !    lablxc(111) = 'fniterpump    '; boundl(111) = 0.001D0; boundu(111) = 1.000D0
    !    lablxc(112) = 'fzeffmax      '; boundl(112) = 0.001D0; boundu(112) = 1.000D0
    !    !!  <LI> (112) fzeffmax: f-value for max Zeff (f-value for equation 64)
    !    lablxc(113) = 'ftaucq        '; boundl(113) = 0.001D0; boundu(113) = 1.000D0
    !    !!  <LI> (113) ftaucq: f-value for minimum quench time (f-value for equation 65)
    !    lablxc(114) = 'fw_channel_l  '; boundl(114) = 0.001D0; boundu(114) = 1.000D3
    !    !!  <LI> (114) fw_channel_length: Length of a single first wall channel
    !    !!             (f-value for equation 66)
    !    !!  <LI> (115) fpoloidalpower: f-value for max rate of change of energy in poloidal field
    !    lablxc(115) = 'fpoloidalpower'; boundl(26) = 0.001D0; boundu(26) = 1.000D0
    !    lablxc(116) = 'fradwall      '; boundl(116) = 0.001D0; boundu(116) = 1.000D0
    !    !!  <LI> (116) fradwall: f-value for radiation wall load limit (eq. 67)
    !    lablxc(117) = 'fpsepbqar     '; boundl(117) = 0.001D0; boundu(117) = 1.000D0
    !    !!  <LI> (117) fpsepbqar: f-value for  Psep*Bt/qar upper limit (eq. 68)
    !    !!            (f-value for equation 69)
    !    !!  <LI> (118) fpsep: f-value to ensure separatrix power is less than value from Kallenbach divertor
    !    lablxc(118) = 'fpsep         '; boundl(118) = 0.001D0; boundu(118) = 1.000D0
    !    lablxc(119) = 'tesep         '; boundl(119) = 0.000D0; boundu(119) = 1.000D1
    !    !!  <LI> (119) tesep:  separatrix temperature calculated by the Kallenbach divertor model
    !    lablxc(120) = 'ttarget       '; boundl(120) = 1.000D0; boundu(120) = 1.000D4
    !    !!  <LI> (120) ttarget: Plasma temperature adjacent to divertor sheath [eV]
    !    lablxc(121) = 'neratio       '; boundl(121) = 0.001D0; boundu(121) = 1.000D0
    !    !!  <LI> (121) neratio: ratio of mean SOL density at OMP to separatrix density at OMP
    !    lablxc(122) = 'oh_steel_frac '; boundl(122) = 0.001D0; boundu(122) = 0.950D0
    !    !!  <LI> (122) oh_steel_frac : streel fraction of Central Solenoid
    !    lablxc(123) = 'foh_stress    '; boundl(123) = 0.001D0; boundu(123) = 1.000D0
    !    !!  <LI> (123) foh_stress : f-value for CS coil Tresca stress limit (f-value for eq. 72)
    !    lablxc(124) = 'qtargettotal  '; boundl(124) = 0.001D0; boundu(124) = 1.000D7
    !    !!  <LI> (124) qtargettotal : Power density on target including surface recombination [W/m2]
    !    lablxc(125) = 'fimp(03)      '; boundl(125) = 1.00D-8; boundu(125) = 0.010D0
    !    !!  <LI> (125) fimp(3) :  Beryllium density fraction relative to electron density
    !    lablxc(126) = 'fimp(04)      '; boundl(126) = 1.00D-8; boundu(126) = 0.010D0
    !    !!  <LI> (126) fimp(4) :  Carbon density fraction relative to electron density
    !    lablxc(127) = 'fimp(05)      '; boundl(127) = 1.00D-8; boundu(127) = 0.010D0
    !    !!  <LI> (127) fimp(5) :  Nitrogen fraction relative to electron density
    !    lablxc(128) = 'fimp(06)      '; boundl(128) = 1.00D-8; boundu(128) = 0.010D0
    !    !!  <LI> (128) fimp(6) :  Oxygen density fraction relative to electron density
    !    lablxc(129) = 'fimp(07)      '; boundl(129) = 1.00D-8; boundu(129) = 0.010D0
    !    !!  <LI> (129) fimp(7) :  Neon density fraction relative to electron density
    !    lablxc(130) = 'fimp(08)      '; boundl(130) = 1.00D-8; boundu(130) = 0.010D0
    !    !!  <LI> (130) fimp(8) :  Silicon density fraction relative to electron density
    !    lablxc(131) = 'fimp(09)      '; boundl(131) = 1.00D-8; boundu(131) = 0.010D0
    !    !!  <LI> (131) fimp(9) :  Argon density fraction relative to electron density
    !    lablxc(132) = 'fimp(10)      '; boundl(132) = 1.00D-8; boundu(132) = 0.010D0
    !    !!  <LI> (132) fimp(10) :  Iron density fraction relative to electron density
    !    lablxc(133) = 'fimp(11)      '; boundl(133) = 1.00D-8; boundu(133) = 0.010D0
    !    !!  <LI> (133) fimp(11) :  Nickel density fraction relative to electron density
    !    lablxc(134) = 'fimp(12)      '; boundl(134) = 1.00D-8; boundu(134) = 0.010D0
    !    !!  <LI> (134) fimp(12) :  Krypton density fraction relative to electron density
    !    lablxc(135) = 'fimp(13)      '; boundl(135) = 1.00D-8; boundu(135) = 0.010D0
    !    !!  <LI> (135) fimp(13) :  Xenon density fraction relative to electron density
    !    lablxc(136) = 'fimp(14)      '; boundl(136) = 1.00D-8; boundu(136) = 0.010D0
    !    !!  <LI> (136) fimp(14) :  Tungsten density fraction relative to electron density
    !    lablxc(137) = 'fplhsep       '; boundl(137) = 0.001D0; boundu(137) = 1.000D0
    !    !!  <LI> (137) fplhsep (f-value for equation 73)
    !    lablxc(138) = 'rebco_thicknes'; boundl(138) = 0.01D-6; boundu(138) = 100.0D-6
    !    !!  <LI> (138) rebco_thickness : thickness of REBCO layer in tape (m)
    !    lablxc(139) = 'copper_thick  '; boundl(139) = 1.00D-6; boundu(139) = 1.00D-3
    !    !!  <LI> (139) copper_thick : thickness of copper layer in tape (m)
    !    lablxc(140) = 'thkwp         '; boundl(140) = 0.001D0; boundu(140) = 2.000D0
    !    !!  <LI> (140) thkwp : radial thickness of TFC winding pack (m)
    !    lablxc(141) = 'fcqt          '; boundl(141) = 0.001D0; boundu(141) = 1.000D0
    !    !!  <LI> (141) fcqt : TF coil quench temperature < tmax_croco (f-value for equation 74)
    !    lablxc(142) = 'nesep         '; boundl(142) = 1.00D17; boundu(142) = 1.00D20
    !    !!  <LI> (142) nesep : electron density at separatrix [m-3]
    !    lablxc(143) = 'f_coppera_m2  '; boundl(143) = 0.001D0; boundu(143) = 1.000D0
    !    !!  <LI> (143) f_coppera_m2 : TF coil current / copper area < Maximum value (f-value for equation 75)
    !    lablxc(144) = 'fnesep        '; boundl(144) = 0.001D0; boundu(144) = 1.000D0
    !    !!  <LI> (144) fnesep : Eich critical electron density at separatrix (f-value for constraint equation 76) [m-3]
    !    lablxc(145) = 'fgwped        '; boundl(145) = 0.500D0; boundu(145) = 1.000D0
    !    !!  <LI> (145) fgwped :  fraction of Greenwald density to set as pedestal-top density
    !    lablxc(146) = 'fcpttf        '; boundl(146) = 0.001D0; boundu(146) = 1.000D0
    !    !!  <LI> (146) fcpttf : F-value for TF coil current per turn limit (constraint equation 77)
    !    lablxc(147) = 'freinke       '; boundl(147) = 0.001D0; boundu(147) = 1.000D0
    !    !!  <LI> (147) freinke : F-value for Reinke detachment criterion (constraint equation 78)
    !    lablxc(148) = 'fzactual      '; boundl(148) = 1.00D-8; boundu(148) = 1.000D0
    !    !!  <LI> (148) fzactual : fraction of impurity at SOL with Reinke detachment criterion
    !    lablxc(149) = 'fbmaxcs       '; boundl(149) = 0.001D0; boundu(149) = 1.000D0
    !    !!  <LI> (149) fbmaxcs : F-value for max peak CS field (con. 79, itvar 149)
    !    lablxc(150) = 'plasmod_fcdp  '; boundl(150) = 0.000D0; boundu(150) = 1.000D0
    !     !!  <LI> (150) plasmod_fcdp : (P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power
    !    lablxc(151) = 'plasmod_fradc '; boundl(151) = 0.001D0; boundu(151) = 1.000D0
    !    !!  <LI> (151) plasmod_fradc : Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad)
    !    lablxc(152) = 'fgwsep        '; boundl(152) = 0.001D0; boundu(152) = 1.000D0
    !    !!  <LI> (152) fbmaxcs : Ratio of separatrix density to Greenwald density
    !    lablxc(153) = 'fpdivlim      '; boundl(153) = 0.001D0; boundu(153) = 1.000D0
    !    !!  <LI> (153) fpdivlim : F-value for minimum pdivt (con. 80)
    !    lablxc(154) = 'fpdivlim      '; boundl(154) = 0.001D0; boundu(154) = 1.000D0
    !    !!  <LI> (154) fne0 : F-value for ne(0) > ne(ped) (con. 81)</UL>


    !  Initialise stellarator parameters if necessary
    !  This overrides some of the bounds of the tokamak parameters.
    if (istell /= 0) call stinit

end subroutine initial

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine devtyp

    !! Routine to determine which type of device is to be modelled
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine uses the contents of an input file,
    !! <CODE>device.dat</CODE>, to determine which type of device
    !! is to be modelled. If the file is not present in the current
    !! directory, a standard tokamak model is assumed.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use error_handling
    use global_variables
    use ife_variables
    use stellarator_variables

    implicit none

    !  Local variables

    integer :: idev
    integer :: iost
    logical :: iexist
    character(len = 20) :: devFile
    character(len = 5) :: line
    line = ' '
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    devFile = 'device.dat'
    istell = 0
    ife    = 0
    idev   = 0      ! Default value MK

    !  Read a second input file. If the file does not exist or
    !  blank, then the standard tokamak option is assumed.

    inquire(file = devFile, exist = iexist)

    if (iexist) then
        open(unit = 101, file = 'device.dat', status = 'old')
        DO
            read(101,'(A)', IOSTAT = iost) line
            read(line, '(I2)') idev
            if(iost < 0 .or. idev > 0) exit
        END DO
        close(unit = 101)

        !  Set relevant switch

        select case (idev)

        case (1)  !  Stellarator model
            istell = 1

        case (2)  !  ! ISSUE #508 Remove RFP option
            call report_error(228)
        case (3)  !  Inertial Fusion Energy model
            ife = 1
            icase = 'Inertial Fusion model'

        case default  !  Tokamak model
            continue

        end select
    end if

end subroutine devtyp

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine check

    !! Routine to reset specific variables if certain options are
    !! being used
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine performs a sanity check of the input variables
    !! and ensures other dependent variables are given suitable values.

    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables
    use buildings_variables
    use current_drive_variables
    use divertor_kallenbach_variables
    use error_handling
    use fwbs_variables
    use global_variables
    use heat_transport_variables
    use ife_variables
    use impurity_radiation_module
    use numerics
    use pfcoil_variables
    use physics_variables
    use plasmod_variables
    use process_output
    use pulse_variables
    use reinke_variables
    use tfcoil_variables
    use stellarator_variables
    use sctfcoil_module
    use vacuum_variables

    implicit none

    !  Local variables

    integer :: i,j,k,imp
    real(dp) :: fsum

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    errors_on = .true.

    !  Check that there are sufficient iteration variables
    if (nvar < neqns) then
        idiags(1) = nvar ; idiags(2) = neqns
        call report_error(137)
    end if

    !  Check that sufficient elements of ixc and icc have been specified
    if ( any(ixc(1:nvar) == 0) ) then
        idiags(1) = nvar
        call report_error(139)
    end if


    if ( any(icc(1:neqns+nineqns) == 0) ) then
        idiags(1) = neqns ; idiags(2) = nineqns
        call report_error(140)
    end if

    !  Deprecate constraints 3 and 4
    if ( any(icc(1:neqns+nineqns) == 3) ) then
        call report_error(162)
        write(*,*) 'PROCESS stopping'
        stop
    end if

    if ( any(icc(1:neqns+nineqns) == 4) ) then
        call report_error(163)
        write(*,*) 'PROCESS stopping'
        stop
    end if


    ! MDK Report error if constraint 63 is used with old vacuum model
    if (any(icc(1:neqns+nineqns) == 63).and.(vacuum_model.ne.'simple') ) then
        write(*,*) 'Constraint 63 is requested without the correct vacuum model ("simple").'
        write(*,*) 'vacuum_model = ', vacuum_model
        write(*,*) 'PROCESS stopping'
        stop
    end if

    if ( any(icc(1:neqns+nineqns) == 74) ) then
        write(*,*)'Constraint 74 (TF coil quench temperature for Croco HTS conductor) is not yet implemented'
        write(*,*) 'PROCESS stopping'
        stop
    end if

    !  Fuel ion fractions must add up to 1.0
    if (abs(1.0D0 - fdeut - ftrit - fhe3) > 1.0D-6) then
        fdiags(1) = fdeut; fdiags(2) = ftrit ; fdiags(3) = fhe3
        call report_error(36)
    end if

    if (ftrit < 1.0D-3) then  !  tritium fraction is negligible
        triv = 0.0D0
        ifispact = 0
        trithtmw = 0.0D0
    end if

    !  Impurity fractions
    do imp = 1,nimp
        impurity_arr(imp)%frac = fimp(imp)
    end do

    ! The 1/R B field dependency constraint variable is being depreciated
    ! Stop the run if the constraint 10 is used
    if ( any( icc == 10 ) ) then
        call report_error(236)
        stop
    end if

    ! Stop the run if oacdcp is used as an optimisation variable
    ! As the current density is now calculated from bt without constraint 10
    if ( any( ixc == 12 ) ) then
        call report_error(236)
        stop
    end if 

    !  Warn if ion power balance equation is being used with the new radiation model
    if (any(icc == 3)) then
        call report_error(138)
    end if

    ! Check if the kallenbach model is used with a variable target temperature
    ! is so a discontinuity might appears that is highly likely to prevent VMCON
    ! to converge.
    if ( any(ixc == 120 ) .and. kallenbach_switch == 1 ) then
        call report_error(237)
    end if 

    !  Plasma profile consistency checks
    if (ife /= 1) then
        if (ipedestal == 1 .or. ipedestal == 2) then

            !  Temperature checks
            if (teped < tesep) then
                fdiags(1) = teped ; fdiags(2) = tesep
                call report_error(146)
            end if

            if ((abs(rhopedt-1.0D0) <= 1.0D-7).and.((teped-tesep) >= 1.0D-7)) then
                fdiags(1) = rhopedt ; fdiags(2) = teped ; fdiags(3) = tesep
                call report_error(147)
            end if

            !  Core temperature should always be calculated (later) as being
            !  higher than the pedestal temperature, if and only if the
            !  volume-averaged temperature never drops below the pedestal
            !  temperature. Prevent this by adjusting te, and its lower bound
            !  (which will only have an effect if this is an optimisation run)
            if (te <= teped) then
                fdiags(1) = te ; fdiags(2) = teped
                te = teped*1.001D0
                call report_error(149)
            end if

            if ((ioptimz >= 0).and.(any(ixc == 4)).and.(boundl(4) < teped*1.001D0)) then
                call report_error(150)
                boundl(4) = teped*1.001D0
                boundu(4) = max(boundu(4), boundl(4))
            end if

             !  Density checks
             !  Case where pedestal density is set manually
             ! ---------------
             if ( (fgwped < 0) .or. (.not.any(ixc==145)) ) then
            
                 ! Issue #589 Pedestal density is set manually using neped but it is less than nesep.
                 if ( neped < nesep ) then
                     fdiags(1) = neped ; fdiags(2) = nesep
                     call report_error(151)
                 end if  

                 ! Issue #589 Pedestal density is set manually using neped,
                 ! but pedestal width = 0.
                 if ( (abs(rhopedn-1.0D0) <= 1.0D-7).and.((neped-nesep) >= 1.0D-7) ) then
                     fdiags(1) = rhopedn ; fdiags(2) = neped ; fdiags(3) = nesep
                     call report_error(152)
                 end if
             end if 

             ! Issue #862 : Variable ne0/neped ratio without constraint eq 81 (ne0>neped)
             !  -> Potential hollowed density profile
             if ( (ioptimz >= 0) .and. (.not.any(icc==81)) ) then
                 if ( any(ixc == 145 )) call report_error(154)
                 if ( any(ixc ==   6 )) call report_error(155)
             end if
         end if
     end if
     ! ---------------

     
     ! Cannot use Psep/R and PsepB/qAR limits at the same time
     if(any(icc == 68) .and. any(icc == 56)) then
        call report_error(178)
     endif

     if(ieped > 0) then
        if(eped_sf > 1.0) then
           call report_error(214)
        endif
     endif

     if(ipedestal == 2 .or. ipedestal == 3) then

        !PLASMOD automatically takes both pseprmax and psepbqarmax as input
        !It uses which ever one leads to the lower Psep value. Not to use
        !one of them set them to a very large number
        call report_error(199)

        if (fgwped .le. 0 )then
           call report_error(176)
        endif
        if (fgwsep .le. 0 ) then
           call report_error(177)
        endif


        !need to enforce H-mode using Martin scaling, if using PLASMOD
        !HL Todo: The L-H threshold is checked inside PLASMOD do we need to duplicate in PROCESS??
        !if(.not. any(icc == 15)) then
     !      call report_error(179)
        !endif

        if (boundl(103) < 1.) then
           call report_error(181)
        endif

        ! Initialise value for gamcd for use in PLASMOD first iteration
        gamcd = 0.3


        ! PLASMOD only uses the core radiation for the H-factor correction.
        ! It calculates the power balance using the total radiation.
        if(iradloss .ne. 0) then
           call report_error(184)
        endif

        ! Mutually exclusive variables - issue #632
        if ((plasmod_contrpovs.ne.0).and.(plasmod_contrpovr.ne.0))then
           call report_error(187)
        endif

        !kappa95 and triang95 are input to PLASMOD
        !kappa and triang are calculated
        if (ishape .ne. 4) then
           call report_error(196)
        endif

        !PLASMOD uses its own NBI current drive routine
        if (.not.((iefrf == 5) .or. (iefrf == 8)) )then
           call report_error(197)
        endif

        !PLASMOD always uses current drive
        if(irfcd == 0) then
           call report_error(198)
        endif

     endif

     if ((any(ixc==145)) .and. (boundl(145) < fgwsep)) then  !if lower bound of fgwped < fgwsep
        fdiags(1) = boundl(145); fdiags(2) = fgwsep
        call report_error(186)
     end if

     if(ipedestal==3)then

        !as beta is an output of PLASMOD, its consistency does not need to be enforced
        if(any(icc == 1)) then
           call report_error(188)
        endif

        ! The global power balance cannot be enforced when running PLASMOD
        ! PROCESS cannot vary any of the relevant inputs as these are all
        ! outputs of PLASMOD issue #631
        if (any(icc == 2)) then
           call report_error(185)
        endif

        ! density upper limit cannot be used with PLASMOD
        if (any(icc == 5)) then
           call report_error(183)
        endif

        ! LH power threshold limit cannot be used with PLASMOD
        if (any(icc == 15)) then
           call report_error(209)
        endif

        ! Beta upper limit does not apply with PLASMOD
        if (any(icc == 24)) then
           call report_error(211)
        endif

        ! ratio of particle/energy confinement times cannot be used with PLASMOD
        if (any(icc == 62)) then
           call report_error(210)
        endif

        ! Psep * Bt / qAR upper limit cannot be used with PLASMOD
        if (any(icc == 68)) then
           call report_error(208)
        endif

        !plasmod_i_modeltype = 1 enforces a given H-factor
        !plasmod_i_modeltype > 1 calculates transport from the transport models
        if (plasmod_i_modeltype > 1) then
           !beta limit is enforced inside PLASMOD
           !icc(6)  eps * betap upper limit
           !ixc(8)  fbeta
           !icc(24) beta upper limit
           !ixc(36) fbetatry
           !icc(48) poloidal beta upper limit
           !ixc(79) fbetap

           if (any((icc == 6) .or. (icc == 24) .or. (icc == 48) ) .or. any((ixc == 8) .or.(ixc == 36) .or. (ixc == 79))) then
              call report_error(191)
           endif

        endif

        ! Stop PROCESS if certain iteration variables have been requested while using PLASMOD
        ! These are outputs of PLASMOD
        ! ixc(4) = te, ixc(5) = beta, ixc(6) = dene, ixc(9) = fdene,
        !ixc(44) = fvsbrnni
        ! ixc(102) = fimpvar, ixc(103) = flhthresh, ixc(109) = ralpne,
        ! ixc(110) = ftaulimit,
        if(any((ixc==4).or.(ixc==5).or.(ixc==6).or.(ixc==9).or.(ixc==36)&
             .or.(ixc==44).or.(ixc==102).or.(ixc==103).or.(ixc==109).or.&
             (ixc==110).or.(ixc==117)))then
           call report_error(182)
        endif

        if (plasmod_i_equiltype == 2) then

           !Warning, this is a not recommended option
           !operation is inconsistent with PROCESS.
           call report_error(189)

           !cannot use q as iteration variable, if plasma current is input for EMEQ
           if (any(ixc == 18)) then
              call report_error(190)
           endif
        endif

     endif


     if (any(icc == 78)) then

        !If Reinke criterion is used tesep is calculated and cannot be an
        !iteration variable
        if (any(ixc == 119)) then
           call report_error(219)
        endif

        !If Reinke criterion is used need to enforce LH-threshold
        !using Martin scaling for consistency
        if (.not. ilhthresh == 6) then
           call report_error(218)
        endif
        if  (.not. any(icc==15) .and. (ipedestal .ne. 3)) then
           call report_error(218)
        endif


     endif

     if (any(icc == 78)) then

        !If Reinke criterion is used tesep is calculated and cannot be an
        !iteration variable
        if (any(ixc == 119)) then
           call report_error(219)
        endif

        !If Reinke criterion is used need to enforce LH-threshold
        !using Martin scaling for consistency
        if (.not. ilhthresh == 6) then
           call report_error(218)
        endif
        if  (.not. any(icc==15) .and. (ipedestal .ne. 3)) then
           call report_error(218)
        endif


     endif

     !if using Reinke iteration variable fzactual, then assign to imp. array
     ! This is also done in iteration_variables.f90 - leave it in for the moment.
     if (any(ixc == 148)) then
        impurity_arr(impvardiv)%frac = fzactual / impurity_enrichment(impvardiv)
     endif


    !  Tight aspect ratio options (ST)
    ! --------------------------------
    if ( itart == 1 ) then

        icase  = 'Tight aspect ratio tokamak model'

        ! Forcing that no inboard breeding blanket is used
        iblnkith = 0

        ! Check if the choice of plasma current is addapted for ST
        ! 2 : Peng Ip scaling (See STAR code documentation)
        ! 9 : Fiesta Ip scaling
        if (icurr /= 2 .and. icurr /= 9) then
            idiags(1) = icurr ; call report_error(37)
        end if

        ! Location of the TF coils 
        ! 2 : PF coil on top of TF coil;
        ! 3 : PF coil outside of TF coil</UL>
        ipfloc(1) = 2
        ipfloc(2) = 3
        ipfloc(3) = 3

        ! Water cooled copper magnets initalisation / checks
        if ( i_tf_sup == 0 ) then
            ! Check if the initial centrepost coolant loop adapted to the magnet technology
            ! Ice cannot flow so tcoolin > 273.15 K 
            if ( tcoolin < 273.15D0 ) call report_error(234)

            ! Temperature of the TF legs cannot be cooled down 
            if ( abs(tlegav+1.0D0) > epsilon(tlegav) .and. tlegav < 273.15D0 ) call report_error(239)

            ! Check if conductor upper limit is properly set to 50 K or below
            if ( any(ixc == 20 ) .and. boundu(20) < 273.15D0 ) call report_error(241)

        ! Call a lvl 3 error if superconductor magnets are used
        else if ( i_tf_sup == 1 ) then 
            call report_error(233)

        ! Helium cooled cryogenic aluminium magnets initalisation / checks
        ! Initialize the CP conductor temperature to cryogenic temperature for cryo-al magnets (20 K)
        else  if ( i_tf_sup == 2 ) then

            ! Call a lvl 3 error if the inlet coolant temperature is too large
            ! Motivation : ill-defined aluminium resistivity fit for T > 40-50 K
            if ( tcoolin > 40.0D0 ) call report_error(235)
            
            ! Check if the leg average temperature is low enough for the resisitivity fit
            if ( tlegav > 50.0D0 ) call report_error(238)

            ! Check if conductor upper limit is properly set to 50 K or below
            if ( any(ixc == 20 ) .and. boundu(20) > 50.0D0 ) call report_error(240)

            ! Otherwise intitialise the average conductor temperature at 
            tcpav = tcoolin
        
        end if

        ! Check if the boostrap current selection is addapted to ST
        if (ibss  == 1) call report_error(38)

        ! Check if a single null divertor is used
        if (i_single_null == 1) call report_error(39)

        ! Set the TF coil shape to picture frame (if default value)
        if ( i_tf_shape == 0 ) i_tf_shape = 2
    ! --------------------------------

    
    ! Conventionnal aspect ratios specific
    ! ------------------------------------
    else

        if (icurr == 2 .or. icurr == 9) call report_error(40)

        if (i_single_null == 0) then
            idivrt = 2
        else  !  i_single_null == 1
            idivrt = 1
        end if

        ! Set the TF coil shape to PROCESS D-shape (if default value)
        if ( i_tf_shape == 0 ) i_tf_shape = 1

        !  Check PF coil configurations
        j = 0 ; k = 0
        do i = 1, ngrp
            if ((ipfloc(i) /= 2).and.(ncls(i) /= 2)) then
                idiags(1) = i ; idiags(2) = ncls(i)
                call report_error(41)
            end if

            if (ipfloc(i) == 2) then
                j = j + 1
                k = k + ncls(i)
            end if
        end do

        if (k == 1) call report_error(42)
        if (k > 2) call report_error(43)
        if ((i_single_null == 1).and.(j < 2)) call report_error(44)

    end if
    ! ------------------------------------

    !  Pulsed power plant model
    if (lpulse == 1) then
        icase = 'Pulsed tokamak model'
    else
        esbldgm3 = 0.0D0
    end if

    !  Ensure minimum cycle time constraint is turned off
    !  (not currently available, as routine thrmal has been commented out)
    if ( any(icc == 42) ) then
        call report_error(164)
    end if

    !  Ensure that if TF coils are non-superconducting,
    !  only simple stress calculations are performed
    ! See Issue #781
    ! if (i_tf_sup /= 1) tfc_model = 0

    ! TF coil
    ! -------
    ! TF stress model not defined of r_tf_inboard = 0
    ! -> If bore + gapoh + ohcth = 0 and fixed and stress constraint is used
    !    Generate a lvl 3 error proposing not to use any stress constraints
    if (       ( .not. ( any(ixc == 16 ) .or. any(ixc == 29 ) .or. any(ixc == 42 ) ) ) & ! No bore,gapoh, ohcth iteration  
         .and. ( abs(bore + gapoh + ohcth) < epsilon(bore) )                           & ! bore + gapoh + ohcth = 0
         .and. any(icc == 31) ) then                                                     ! Stress constraint (31) is used 

        call report_error(246)
        stop
    end if
     

    ! If TFC sidewall has not been set by user
    if(casths<0.1d-10) tfc_sidewall_is_fraction = .true.
    ! If inboard TF coil case plasma side thickness has not been set by user
    if(casthi<0.1d-10) casthi_is_fraction = .true.

    ! Issue #514 Radial dimensions of inboard leg
    ! Ensure that tfcth is defined if thkwp is an iteration variable (140)
    ! if (any(ixc(1:nvar) == 140) ) then
    !     tfcth = thkwp + casthi + thkcas + 2.0D0*tinstf + 2.0d0*tfinsgap
    ! endif

    !  PF coil resistivity is zero if superconducting
    if (ipfres == 0) pfclres = 0.0D0

    !  If there is no NBI, then hot beam density should be zero
    if (irfcd == 1) then
        if ((iefrf /= 5).and.(iefrf /= 8)) rnbeam = 0.0D0
    else
        rnbeam = 0.0D0
    end if

    ! Set inboard blanket thickness to zero if no inboard blanket switch
    ! used (Issue #732)
    if (iblnkith == 0) blnkith = 0.0D0

    !  Solid breeder assumed if ipowerflow=0

    !if (ipowerflow == 0) blkttype = 3

    !  Set coolant fluid type

    !if ((blkttype == 1).or.(blkttype == 2)) then
    !   coolwh = 2  !  water
    !else
    !   coolwh = 1  !  helium
    !end if

    !  But... set coolant to water if blktmodel > 0
    !  Although the *blanket* is by definition helium-cooled in this case,
    !  the shield etc. are assumed to be water-cooled, and since water is
    !  heavier (and the unit cost of pumping it is higher), the calculation
    !  for coolmass is better done with coolwh=2 if blktmodel > 0 to give
    !  slightly pessimistic results.

    !if (blktmodel > 0) then
    !   secondary_cycle = 0
    !   blkttype = 3  !  HCPB
    !   coolwh = 2
    !end if

    !  Ensure that blanket material fractions allow non-zero space for steel
    !  CCFE HCPB Model

    if (istell == 0) then
        if ((iblanket == 1).or.(iblanket == 3)) then
            fsum = breeder_multiplier + vfcblkt + vfpblkt
            if (fsum >= 1.0D0) then
                idiags(1) = iblanket
                fdiags(2) = breeder_multiplier
                fdiags(3) = vfcblkt
                fdiags(4) = vfpblkt
                fdiags(5) = fsum
                call report_error(165)
            end if
        end if
    end if

    ! Initialise superconductor cable parameters
    if(i_tf_sup==1)then
        call initialise_cables()
    end if

    ! Check that the temperature margins are not overdetermined
    if(tmargmin>0.0001d0)then
        ! This limit has been input and will be applied to both TFC and CS
        if(tmargmin_tf>0.0001d0)then
            write(*,*)'tmargmin_tf and tmargmin should not both be specified in IN.DAT.'
            write(*,*)'tmargmin_tf has been ignored.'
        end if
        if(tmargmin_cs>0.0001d0)then
            write(*,*)'tmargmin_cs and tmargmin should not both be specified in IN.DAT.'
            write(*,*)'tmargmin_cs has been ignored.'
        end if
        tmargmin_tf = tmargmin
        tmargmin_cs = tmargmin
     end if

     if (tauee_in.ge.1.0D-10.and.isc.ne.48) then
        ! Report error if confinement time is in the input
        ! but the scaling to use it is not selected.
        call report_error(220)
     end if

     if (aspect.gt.1.7D0.and.isc.eq.46) then
        ! NSTX scaling is for A<1.7
        call report_error(221)
     end if

    if (icurr.eq.2.and.isc.eq.42) then
        call report_error(222)
    end if

    errors_on = .false.


end subroutine check
