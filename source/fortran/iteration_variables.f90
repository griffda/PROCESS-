! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine loadxc

  !+ad_name  loadxc
  !+ad_summ  Routine to load the physics and engineering variables into the
  !+ad_summ  optimisation variables array
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This subroutine loads the physics and engineering variables
  !+ad_desc  into the optimisation variables array <CODE>XCM</CODE>.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constraint_variables
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables

  !+ad_call  impurity_radiation_module
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables

  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  pulse_variables
  !+ad_call  report_error
  !+ad_hist  22/10/92 PJK Removed original arguments (xc,nn)
  !+ad_hist  14/11/11 PJK Changed NaN error check
  !+ad_hist  09/10/12 PJK Initial F90 version
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  06/11/12 PJK Renamed source file iteration_variables.f90 from xc.f90
  !+ad_hist  04/06/13 PJK Added ftbr (89), blbuith (90), blbuoth (91),
  !+ad_hisc               fflutf (92), shldith (93), shldoth (94),
  !+ad_hisc               fptfnuc (95), fvvhe (96)
  !+ad_hist  19/06/13 PJK fjtfc (var.28) is obsolete - added error trap
  !+ad_hist  30/09/13 PJK Added fpsepr (97)
  !+ad_hist  17/10/13 PJK Modified logic for cdtfleg usage
  !+ad_hist  28/11/13 PJK Added li6enrich (98)
  !+ad_hist  26/02/14 PJK Added ftftort (99) and ftfthko (100)
  !+ad_hist  05/03/14 PJK Added warnings for usage of some iteration variables if
  !+ad_hisc               istell=1
  !+ad_hist  30/04/14 PJK Added prp (101)
  !+ad_hist  08/05/14 PJK Replaced itfmod with tfc_model
  !+ad_hist  19/05/14 PJK Reassigned (28) to fradpwr
  !+ad_hist  02/06/14 PJK Added fimpvar (102)
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  30/07/14 PJK Changed tftort clause
  !+ad_hist  01/10/14 PJK Added flhthresh (103)
  !+ad_hist  02/10/14 PJK Added fcwr (104)
  !+ad_hist  06/10/14 PJK Added fnbshinef (105)
  !+ad_hist  11/11/14 PJK Added ftmargoh (106)
  !+ad_hist  25/11/14 JM  Added favail (107)
  !+ad_hist  27/05/15 MDK Added breeder_f (108)
  !+ad_hist  03/08/15 MDK Create list of iteration variable names
  !+ad_hist  05/08/15 MDK Added ralpne, ftaulimit
  !+ad_hist  26/08/15 MDK Added fniterpump (111)
  !+ad_hist  18/11/15 RK  Added fzeffmax (112)
  !+ad_hist  26/11/15 RK  Added ftaucq (113)
  !+ad_hist  10/11/16 HL  Added fradwall (116)
  !+ad_hist  19/01/17 JM  Added fpsepbqar (117)
  !+ad_hist  08/02/17 JM  Added fpsep, tesep and ttarget  (118, 119, 120)
  !+ad_hist  22/02/17 JM  Added neratio (121)
  !+ad_hist  27/02/17 JM  Added oh_steel_frac (122)
  !+ad_hist  27/02/17 JM  Added foh_stress (123)
  !+ad_hist  15/03/17 MDK  Added qtargettotal (124)
  !+ad_hist  17/03/17 MDK  Added impurities fimp(3-14) (125-136)
  !+ad_hist  12/01/18 KE  Added fnesep (144)
  !+ad_hist  22/06/18 SIM cdtfleg (itv 24) no longer used
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constraint_variables
  use cost_variables
  use current_drive_variables
  use define_iteration_variables 
  use divertor_kallenbach_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables
  use impurity_radiation_module
  use numerics
  use pfcoil_variables
  use physics_variables
  use plasmod_variables
  use pulse_variables
  use reinke_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables
  use global_variables
  use rebco_variables

  implicit none

  !  Arguments

  !  Local variables

  integer :: i
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  do i = 1,nvar

     select case (ixc(i))
         case(1);  xcm(i) = itv_1()
         case(2);  xcm(i) = itv_2()
         case(3);  xcm(i) = itv_3()
         case(4);  xcm(i) = itv_4()
         case(5);  xcm(i) = itv_5()
         case(6);  xcm(i) = itv_6()
         case(7);  xcm(i) = itv_7()
         case(8);  xcm(i) = itv_8()
         case(9);  xcm(i) = itv_9()
         case(10);  xcm(i) = itv_10()
         case(11);  xcm(i) = itv_11()
         case(12);  xcm(i) = itv_12()
         case(13);  xcm(i) = itv_13()
         case(14);  xcm(i) = itv_14()
         case(15);  xcm(i) = itv_15()
         case(16);  xcm(i) = itv_16()
         case(17);  xcm(i) = itv_17()
         case(18);  xcm(i) = itv_18()
         case(19);  xcm(i) = itv_19()
         case(20);  xcm(i) = itv_20()
         case(21);  xcm(i) = itv_21()
         case(22);  
         case(23);  xcm(i) = itv_23()
         case(24);  
         case(25);  xcm(i) = itv_25()
         case(26);  xcm(i) = itv_26()
         case(27);  xcm(i) = itv_27()
         case(28);  xcm(i) = itv_28()
         case(29);  xcm(i) = itv_29()
         case(30);  xcm(i) = itv_30()
         case(31);  xcm(i) = itv_31()
         case(32);  xcm(i) = itv_32()
         case(33);  xcm(i) = itv_33()
         case(34);  xcm(i) = itv_34()
         case(35);  xcm(i) = itv_35()
         case(36);  xcm(i) = itv_36()
         case(37);  xcm(i) = itv_37()
         case(38);  xcm(i) = itv_38()
         case(39);  xcm(i) = itv_39()
         case(40);  xcm(i) = itv_40()
         ! case(41);  xcm(i) = itv_41()
         ! case(42);  xcm(i) = itv_42()
         ! case(43);  xcm(i) = itv_43()
         ! case(44);  xcm(i) = itv_44()
         ! case(45);  xcm(i) = itv_45()
         ! case(46);  xcm(i) = itv_46()
         ! case(47);  xcm(i) = itv_47()
         ! case(48);  xcm(i) = itv_48()
         ! case(49);  xcm(i) = itv_49()
         ! case(50);  xcm(i) = itv_50()
         ! case(51);  xcm(i) = itv_51()
         ! case(52);  xcm(i) = itv_52()
         ! case(53);  xcm(i) = itv_53()
         ! case(54);  xcm(i) = itv_54()
         ! case(55);  xcm(i) = itv_55()
         ! case(56);  xcm(i) = itv_56()
         ! case(57);  xcm(i) = itv_57()
         ! case(58);  xcm(i) = itv_58()
         ! case(59);  xcm(i) = itv_59()
         ! case(60);  xcm(i) = itv_60()
         ! case(61);  xcm(i) = itv_61()
         ! case(62);  xcm(i) = itv_62()
         ! case(63);  xcm(i) = itv_63()
         ! case(64);  xcm(i) = itv_64()
         ! case(65);  xcm(i) = itv_65()
         ! case(66);  xcm(i) = itv_66()
         ! case(67);  xcm(i) = itv_67()
         ! case(68);  xcm(i) = itv_68()
         ! case(69);  xcm(i) = itv_69()
         ! case(70);  xcm(i) = itv_70()
         ! case(71);  xcm(i) = itv_71()
         ! case(72);  xcm(i) = itv_72()
         ! case(73);  xcm(i) = itv_73()
         ! case(74);  xcm(i) = itv_74()
         ! case(75);  xcm(i) = itv_75()
         ! case(76);  xcm(i) = itv_76()
         ! case(77);  xcm(i) = itv_77()
         ! case(78);  xcm(i) = itv_78()
         ! case(79);  xcm(i) = itv_79()
         ! case(80);  xcm(i) = itv_80()
         ! case(81);  xcm(i) = itv_81()
         ! case(82);  xcm(i) = itv_82()
         ! case(83);  xcm(i) = itv_83()
         ! case(84);  xcm(i) = itv_84()
         ! case(85);  xcm(i) = itv_85()
         ! case(86);  xcm(i) = itv_86()
         ! case(87);  xcm(i) = itv_87()
         ! case(88);  xcm(i) = itv_88()
         ! case(89);  xcm(i) = itv_89()
         ! case(90);  xcm(i) = itv_90()
         ! case(91);  xcm(i) = itv_91()
         ! case(92);  xcm(i) = itv_92()
         ! case(93);  xcm(i) = itv_93()
         ! case(94);  xcm(i) = itv_94()
         ! case(95);  xcm(i) = itv_95()
         ! case(96);  xcm(i) = itv_96()
         ! case(97);  xcm(i) = itv_97()
         ! case(98);  xcm(i) = itv_98()
         ! case(99);  xcm(i) = itv_99()
         ! case(100);  xcm(i) = itv_100()
         ! case(101);  xcm(i) = itv_101()
         ! case(102);  xcm(i) = itv_102()
         ! case(103);  xcm(i) = itv_103()
         ! case(104);  xcm(i) = itv_104()
         ! case(105);  xcm(i) = itv_105()
         ! case(106);  xcm(i) = itv_106()
         ! case(107);  xcm(i) = itv_107()
         ! case(108);  xcm(i) = itv_108()
         ! case(109);  xcm(i) = itv_109()
         ! case(110);  xcm(i) = itv_110()
         ! case(111);  xcm(i) = itv_111()
         ! case(112);  xcm(i) = itv_112()
         ! case(113);  xcm(i) = itv_113()
         ! case(114);  xcm(i) = itv_114()
         ! case(115);  xcm(i) = itv_115()
         ! case(116);  xcm(i) = itv_116()
         ! case(117);  xcm(i) = itv_117()
         ! case(118);  xcm(i) = itv_118()
         ! case(119);  xcm(i) = itv_119()
         ! case(120);  xcm(i) = itv_120()
         ! case(121);  xcm(i) = itv_121()
         ! case(122);  xcm(i) = itv_122()
         ! case(123);  xcm(i) = itv_123()
         ! case(124);  xcm(i) = itv_124()
         ! case(125);  xcm(i) = itv_125()
         ! case(126);  xcm(i) = itv_126()
         ! case(127);  xcm(i) = itv_127()
         ! case(128);  xcm(i) = itv_128()
         ! case(129);  xcm(i) = itv_129()
         ! case(130);  xcm(i) = itv_130()
         ! case(131);  xcm(i) = itv_131()
         ! case(132);  xcm(i) = itv_132()
         ! case(133);  xcm(i) = itv_133()
         ! case(134);  xcm(i) = itv_134()
         ! case(135);  xcm(i) = itv_135()
         ! case(136);  xcm(i) = itv_136()
         ! case(137);  xcm(i) = itv_137()
         ! case(138);  xcm(i) = itv_138()
         ! case(139);  xcm(i) = itv_139()
         ! case(140);  xcm(i) = itv_140()
         ! case(141);  xcm(i) = itv_141()
         ! case(142);  xcm(i) = itv_142()
         ! case(143);  xcm(i) = itv_143()
         ! case(144);  xcm(i) = itv_144()
         ! case(145);  xcm(i) = itv_145()
         ! case(146);  xcm(i) = itv_146()
         ! case(147);  xcm(i) = itv_147()
         ! case(148);  xcm(i) = itv_148()
         ! case(149);  xcm(i) = itv_149()
         ! case(150);  xcm(i) = itv_150()
         ! case(151);  xcm(i) = itv_151()
         ! case(152);  xcm(i) = itv_152()
         ! case(153);  xcm(i) = itv_153()
         ! case(154);  xcm(i) = itv_154()


   !   case (10) ; xcm(i) = hfact
   !   case (11) ; xcm(i) = pheat
   !   case (12) ; xcm(i) = oacdcp
   !   case (13) ; xcm(i) = tfcth
   !      if (istell == 1) call report_error(46)
   !   case (14) ; xcm(i) = fwalld
   !   case (15) ; xcm(i) = fvs
   !   case (16) ; xcm(i) = ohcth
   !   case (17) ; xcm(i) = tdwell
   !   case (18) ; xcm(i) = q
   !   case (19) ; xcm(i) = enbeam
   !   case (20) ; xcm(i) = tcpav
   !   case (21) ; xcm(i) = ftburn
   !   case (22) ; write(*,*) 'Iteration variable 22 is not supported.'
   !   case (23) ; xcm(i) = fcoolcp
   !   case (24) ; write(*,*) 'Iteration variable 24 is not supported.'
   !   case (25) ; xcm(i) = fpnetel
   !   case (26) ; xcm(i) = ffuspow
   !   case (27) ; xcm(i) = fhldiv
   !   case (28) ; xcm(i) = fradpwr
   !   case (29) ; xcm(i) = bore
   !   case (30) ; xcm(i) = fmva
   !   case (31) ; xcm(i) = gapomin
   !   case (32) ; xcm(i) = frminor
   !   case (33) ; xcm(i) = fportsz
   !   case (34) ; xcm(i) = fdivcol
   !   case (35) ; xcm(i) = fpeakb
   !   case (36) ; xcm(i) = fbetatry
   !   case (37) ; xcm(i) = coheof
   !   case (38) ; xcm(i) = fjohc
   !   case (39) ; xcm(i) = fjohc0
   !   case (40) ; xcm(i) = fgamcd
     case (41) ; xcm(i) = fcohbop
     case (42) ; xcm(i) = gapoh
     case (43) ; write(*,*) 'Iteration variable 43 is not supported.'
     case (44) ; xcm(i) = fvsbrnni
     case (45) ; xcm(i) = fqval
     case (46) ; xcm(i) = fpinj
     case (47) ; xcm(i) = feffcd
     case (48) ; xcm(i) = fstrcase
     case (49) ; xcm(i) = fstrcond
     case (50) ; xcm(i) = fiooic
     case (51) ; xcm(i) = fvdump
     case (52) ; xcm(i) = vdalw
     case (53) ; xcm(i) = fjprot
     case (54) ; xcm(i) = ftmargtf
     case (55) ; write(*,*) 'Iteration variable 55 is not supported.'
     case (56) ; xcm(i) = tdmptf
     case (57) ; xcm(i) = thkcas
      !   if ((tfc_model == 0).or.(istell == 1)) call report_error(48)
        if (istell == 1) call report_error(48)
     case (58) ; xcm(i) = thwcndut
     case (59) ; xcm(i) = fcutfsu
     case (60) ; xcm(i) = cpttf
        if ((istell == 1).or.(itfsup /= 1)) call report_error(49)
     case (61) ; xcm(i) = gapds
     case (62) ; xcm(i) = fdtmp
     case (63) ; xcm(i) = ftpeak
     case (64) ; xcm(i) = fauxmn
     case (65) ; xcm(i) = tohs
        if (lpulse /= 1) call report_error(50)
     case (66) ; xcm(i) = ftohs
     case (67) ; xcm(i) = ftcycl
     case (68) ; xcm(i) = fptemp
     case (69) ; xcm(i) = rcool
     case (70) ; xcm(i) = vcool
     case (71) ; xcm(i) = fq
     case (72) ; xcm(i) = fipir
     case (73) ; xcm(i) = scrapli
     case (74) ; xcm(i) = scraplo
     case (75) ; xcm(i) = tfootfi
     case (76) ; write(*,*) 'Iteration variable 76 is not supported.'
     case (77) ; write(*,*) 'Iteration variable 77 is not supported.'
     case (78) ; write(*,*) 'Iteration variable 78 is not supported.'
     case (79) ; xcm(i) = fbetap
     case (80) ; write(*,*) 'Iteration variable 80 is not supported.'
     case (81) ; write(*,*) 'Iteration variable 81 is not supported.'
     case (82) ; write(*,*) 'Iteration variable 82 is not supported.'
     case (83) ; write(*,*) 'Iteration variable 83 is not supported.'
     case (84) ; write(*,*) 'Iteration variable 84 is not supported.'
     case (85) ; write(*,*) 'Iteration variable 85 is not supported.'
     case (86) ; write(*,*) 'Iteration variable 86 is not supported.'
     case (87) ; write(*,*) 'Iteration variable 87 is not supported.'
     case (88) ; write(*,*) 'Iteration variable 87 is not supported.'
     case (89) ; xcm(i) = ftbr
     case (90) ; xcm(i) = blbuith
     case (91) ; xcm(i) = blbuoth
     case (92) ; xcm(i) = fflutf
     case (93) ; xcm(i) = shldith
     case (94) ; xcm(i) = shldoth
     case (95) ; xcm(i) = fptfnuc
     case (96) ; xcm(i) = fvvhe
     case (97) ; xcm(i) = fpsepr
     case (98) ; xcm(i) = li6enrich
     case (99) ; write(*,*) 'Iteration variable 99 is not supported.'
     case (100) ; write(*,*) 'Iteration variable 100 is not supported.'
     case (101) ; write(*,*) 'Iteration variable 101 is not supported.'
     case (102) ; xcm(i) = impurity_arr(impvar)%frac
     case (103) ; xcm(i) = flhthresh
     case (104) ; xcm(i) = fcwr
     case (105) ; xcm(i) = fnbshinef
     case (106) ; xcm(i) = ftmargoh
     case (107) ; xcm(i) = favail
     case (108) ; xcm(i) = breeder_f
     case (109) ; xcm(i) = ralpne
     case (110) ; xcm(i) = ftaulimit
     case (111) ; xcm(i) = fniterpump
     case (112) ; xcm(i) = fzeffmax
     case (113) ; xcm(i) = ftaucq
     case (114) ; xcm(i) = fw_channel_length
     case (115) ; xcm(i) = fpoloidalpower
     case (116) ; xcm(i) = fradwall
     case (117) ; xcm(i) = fpsepbqar
     case (118) ; xcm(i) = fpsep
     case (119) ; xcm(i) = tesep
     case (120) ; xcm(i) = ttarget
     case (121) ; xcm(i) = neratio
     case (122) ; xcm(i) = oh_steel_frac
     case (123) ; xcm(i) = foh_stress
     case (124) ; xcm(i) = qtargettotal
     case (125) ; xcm(i) = impurity_arr(3)%frac
     case (126) ; xcm(i) = impurity_arr(4)%frac
     case (127) ; xcm(i) = impurity_arr(5)%frac
     case (128) ; xcm(i) = impurity_arr(6)%frac
     case (129) ; xcm(i) = impurity_arr(7)%frac
     case (130) ; xcm(i) = impurity_arr(8)%frac
     case (131) ; xcm(i) = impurity_arr(9)%frac
     case (132) ; xcm(i) = impurity_arr(10)%frac
     case (133) ; xcm(i) = impurity_arr(11)%frac
     case (134) ; xcm(i) = impurity_arr(12)%frac
     case (135) ; xcm(i) = impurity_arr(13)%frac
     case (136) ; xcm(i) = impurity_arr(14)%frac
     case (137) ; xcm(i) = fplhsep
     case (138) ; xcm(i) = rebco_thickness
     case (139) ; xcm(i) = copper_thick
     case (140) ; xcm(i) = thkwp
     case (141) ; xcm(i) = fcqt
     case (142) ; xcm(i) = nesep
     case (143) ; xcm(i) = f_copperA_m2
     case (144) ; xcm(i) = fnesep
     case (145) ; xcm(i) = fgwped
     case (146) ; xcm(i) = fcpttf
     case (147) ; xcm(i) = freinke
     case (148) ; xcm(i) = impurity_arr(impvardiv)%frac*impurity_enrichment(impvardiv)   !fzactual
     case (149) ; xcm(i) = fbmaxcs
     case (150) ; xcm(i) = plasmod_fcdp
     case (151) ; xcm(i) = plasmod_fradc
     case (152) ; xcm(i) = fgwsep
     case (153) ; xcm(i) = fpdivlim
     case (154) ; xcm(i) = fne0

     case default
        idiags(1) = i ; idiags(2) = ixc(i)
        call report_error(54)

     end select
     

      ! Simple list of iteration variable names
      name_xc(i) = lablxc(ixc(i))
      ! Note that iteration variable 18 has more than one name:
      if ((ixc(i) == 18).and.(icurr /= 2)) name_xc(i) = 'q95'
      if ((ixc(i) == 18).and.(icurr == 2)) name_xc(i) = 'qbar'


       ! MDK Check if sweep variable is also an iteration variable
       if (name_xc(i) == vlabel) then
            write(nout,*) 'WARNING: The sweep variable is also an iteration variable.'
            write(nout,*) 'The values of the sweep variable will be overwritten by the optimiser.'
            write(*,*) 'WARNING: The sweep variable is also an iteration variable.'
       end if

     !  Check that no iteration variable is zero

     if (abs(xcm(i)) <= 1.0D-12) then
        idiags(1) = i ; idiags(2) = ixc(i)
        write(*,*) 'Iteration variable ',ixc(i),'(',trim(lablxc(ixc(i))),') is zero.'
        call report_error(55)
     end if

     !  Crude method of catching NaN errors

     !if ( (abs(xcm(i)) > 9.99D99).or.(xcm(i) /= xcm(i)) ) then
     if ( variable_error(xcm(i)) ) then
        idiags(1) = i ; idiags(2) = ixc(i) ; fdiags(1) = xcm(i)
        call report_error(56)
     end if

  end do

  do i = 1,nvar
     if (abs(xcm(i)) > 1.0D-99) then
        scale(i) = 1.0D0/xcm(i)
     else
        scale(i) = 1.0D0
     end if
     scafc(i) = 1.0D0/scale(i)
     xcm(i) = xcm(i)*scale(i)
  end do

end subroutine loadxc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine convxc(xc,nn)

  !+ad_name  convxc
  !+ad_summ  Routine to convert scaled iteration variables back to
  !+ad_summ  their real values
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  xc(ipnvars) : input/output real array : scaled iteration variable values
  !+ad_args  nn : input integer : number of iteration variables
  !+ad_desc  This subroutine converts the scaled iteration variables back to
  !+ad_desc  their real values.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constraint_variables
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables

  !+ad_call  impurity_radiation_module
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables

  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  report_error
  !+ad_hist  14/11/11 PJK Changed NaN error check
  !+ad_hist  16/11/11 PJK Initial F90 version
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  17/01/13 PJK Removed bounds checking of iteration variables
  !+ad_hist  11/04/13 PJK Removed ti recalculation (moved to physics)
  !+ad_hist  04/06/13 PJK Added ftbr (89), blbuith (90), blbuoth (91),
  !+ad_hisc               fflutf (92), shldith (93), shldoth (94),
  !+ad_hisc               fptfnuc (95), fvvhe (96)
  !+ad_hist  19/06/13 PJK fjtfc (var.28) is obsolete - added error trap
  !+ad_hist  30/09/13 PJK Added fpsepr (97)
  !+ad_hist  28/11/13 PJK Added li6enrich (98)
  !+ad_hist  26/02/14 PJK Added ftftort (99) and ftfthko (100)
  !+ad_hist  30/04/14 PJK Added prp (101)
  !+ad_hist  19/05/14 PJK Reassigned (28) to fradpwr
  !+ad_hist  02/06/14 PJK Added fimpvar (102); special treatment required
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  01/10/14 PJK Added flhthresh (103)
  !+ad_hist  02/10/14 PJK Added fcwr (104)
  !+ad_hist  06/10/14 PJK Added fnbshinef (105)
  !+ad_hist  11/11/14 PJK Added ftmargoh (106)
  !+ad_hist  25/11/14 JM  Added favail (107)
  !+ad_hist  27/05/15 MDK Added breeder_f (108)
  !+ad_hist  05/08/15 MDK Added ralpne (109), ftaulimit (110)
  !+ad_hist  18/11/15 RK  Added fzeffmax (112)
  !+ad_hist  26/11/15 RK  Added ftaucq (113)
  !+ad_hist  10/11/16 HL  Added fradwall (116)
  !+ad_hist  19/01/17 JM  Added fpsepbqar (117)
  !+ad_hist  08/02/17 JM  Added fpsep, tesep, ttarget (118, 119, 120)
  !+ad_hist  22/02/17 JM  Added neratio (121)
  !+ad_hist  12/01/18 KE  Added fnesep (144)
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constraint_variables
  use cost_variables
  use current_drive_variables
  use define_iteration_variables 
  use divertor_kallenbach_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables

  use impurity_radiation_module
  use numerics
  use pfcoil_variables
  use physics_variables
  use plasmod_variables
  use rebco_variables
  use reinke_variables
  use tfcoil_variables
  use times_variables

  implicit none

  !  Arguments

  integer, intent(in) :: nn
  real(kind(1.0D0)), dimension(ipnvars), intent(in) :: xc

  !  Local variables

  integer :: i
  real(kind(1.0D0))::ratio

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,nn
     ratio = xc(i)/scale(i)

     select case (ixc(i))

     case (1);  call set_itv_1(ratio)
     case (2);  call set_itv_2(ratio)
     case (3);  call set_itv_3(ratio)
     case (4);  call set_itv_4(ratio)
     case (5);  call set_itv_5(ratio)
     case (6);  call set_itv_6(ratio)
     case (7);  call set_itv_7(ratio)
     case (8);  call set_itv_8(ratio)
     case (9);  call set_itv_9(ratio)
     case (10);  call set_itv_10(ratio)
     case (11);  call set_itv_11(ratio)
     case (12);  call set_itv_12(ratio)
     case (13);  call set_itv_13(ratio)
     case (14);  call set_itv_14(ratio)
     case (15);  call set_itv_15(ratio)
     case (16);  call set_itv_16(ratio)
     case (17);  call set_itv_17(ratio)
     case (18);  call set_itv_18(ratio)
     case (19);  call set_itv_19(ratio)
     case (20);  call set_itv_20(ratio)
     case (21);  call set_itv_21(ratio)
     case (22);  
     case (23);  call set_itv_23(ratio)
     case (24);  
     case (25);  call set_itv_25(ratio)
     case (26);  call set_itv_26(ratio)
     case (27);  call set_itv_27(ratio)
     case (28);  call set_itv_28(ratio)
     case (29);  call set_itv_29(ratio)
     case (30);  call set_itv_30(ratio)
     case (31);  call set_itv_31(ratio)
     case (32);  call set_itv_32(ratio)
     case (33);  call set_itv_33(ratio)
     case (34);  call set_itv_34(ratio)
     case (35);  call set_itv_35(ratio)
     case (36);  call set_itv_36(ratio)
     case (37);  call set_itv_37(ratio)
     case (38);  call set_itv_38(ratio)
     case (39);  call set_itv_39(ratio)
     case (40);  call set_itv_40(ratio)
   !   case (41);  call set_itv_41(ratio)
   !   case (42);  call set_itv_42(ratio)
   !   case (43);  call set_itv_43(ratio)
   !   case (44);  call set_itv_44(ratio)
   !   case (45);  call set_itv_45(ratio)
   !   case (46);  call set_itv_46(ratio)
   !   case (47);  call set_itv_47(ratio)
   !   case (48);  call set_itv_48(ratio)
   !   case (49);  call set_itv_49(ratio)
   !   case (50);  call set_itv_50(ratio)
   !   case (51);  call set_itv_51(ratio)
   !   case (52);  call set_itv_52(ratio)
   !   case (53);  call set_itv_53(ratio)
   !   case (54);  call set_itv_54(ratio)
   !   case (55);  call set_itv_55(ratio)
   !   case (56);  call set_itv_56(ratio)
   !   case (57);  call set_itv_57(ratio)
   !   case (58);  call set_itv_58(ratio)
   !   case (59);  call set_itv_59(ratio)
   !   case (60);  call set_itv_60(ratio)
   !   case (61);  call set_itv_61(ratio)
   !   case (62);  call set_itv_62(ratio)
   !   case (63);  call set_itv_63(ratio)
   !   case (64);  call set_itv_64(ratio)
   !   case (65);  call set_itv_65(ratio)
   !   case (66);  call set_itv_66(ratio)
   !   case (67);  call set_itv_67(ratio)
   !   case (68);  call set_itv_68(ratio)
   !   case (69);  call set_itv_69(ratio)
   !   case (70);  call set_itv_70(ratio)
   !   case (71);  call set_itv_71(ratio)
   !   case (72);  call set_itv_72(ratio)
   !   case (73);  call set_itv_73(ratio)
   !   case (74);  call set_itv_74(ratio)
   !   case (75);  call set_itv_75(ratio)
   !   case (76);  call set_itv_76(ratio)
   !   case (77);  call set_itv_77(ratio)
   !   case (78);  call set_itv_78(ratio)
   !   case (79);  call set_itv_79(ratio)
   !   case (80);  call set_itv_80(ratio)
   !   case (81);  call set_itv_81(ratio)
   !   case (82);  call set_itv_82(ratio)
   !   case (83);  call set_itv_83(ratio)
   !   case (84);  call set_itv_84(ratio)
   !   case (85);  call set_itv_85(ratio)
   !   case (86);  call set_itv_86(ratio)
   !   case (87);  call set_itv_87(ratio)
   !   case (88);  call set_itv_88(ratio)
   !   case (89);  call set_itv_89(ratio)
   !   case (90);  call set_itv_90(ratio)
   !   case (91);  call set_itv_91(ratio)
   !   case (92);  call set_itv_92(ratio)
   !   case (93);  call set_itv_93(ratio)
   !   case (94);  call set_itv_94(ratio)
   !   case (95);  call set_itv_95(ratio)
   !   case (96);  call set_itv_96(ratio)
   !   case (97);  call set_itv_97(ratio)
   !   case (98);  call set_itv_98(ratio)
   !   case (99);  call set_itv_99(ratio)
   !   case (100);  call set_itv_100(ratio)
   !   case (101);  call set_itv_101(ratio)
   !   case (102);  call set_itv_102(ratio)
   !   case (103);  call set_itv_103(ratio)
   !   case (104);  call set_itv_104(ratio)
   !   case (105);  call set_itv_105(ratio)
   !   case (106);  call set_itv_106(ratio)
   !   case (107);  call set_itv_107(ratio)
   !   case (108);  call set_itv_108(ratio)
   !   case (109);  call set_itv_109(ratio)
   !   case (110);  call set_itv_110(ratio)
   !   case (111);  call set_itv_111(ratio)
   !   case (112);  call set_itv_112(ratio)
   !   case (113);  call set_itv_113(ratio)
   !   case (114);  call set_itv_114(ratio)
   !   case (115);  call set_itv_115(ratio)
   !   case (116);  call set_itv_116(ratio)
   !   case (117);  call set_itv_117(ratio)
   !   case (118);  call set_itv_118(ratio)
   !   case (119);  call set_itv_119(ratio)
   !   case (120);  call set_itv_120(ratio)
   !   case (121);  call set_itv_121(ratio)
   !   case (122);  call set_itv_122(ratio)
   !   case (123);  call set_itv_123(ratio)
   !   case (124);  call set_itv_124(ratio)
   !   case (125);  call set_itv_125(ratio)
   !   case (126);  call set_itv_126(ratio)
   !   case (127);  call set_itv_127(ratio)
   !   case (128);  call set_itv_128(ratio)
   !   case (129);  call set_itv_129(ratio)
   !   case (130);  call set_itv_130(ratio)
   !   case (131);  call set_itv_131(ratio)
   !   case (132);  call set_itv_132(ratio)
   !   case (133);  call set_itv_133(ratio)
   !   case (134);  call set_itv_134(ratio)
   !   case (135);  call set_itv_135(ratio)
   !   case (136);  call set_itv_136(ratio)
   !   case (137);  call set_itv_137(ratio)
   !   case (138);  call set_itv_138(ratio)
   !   case (139);  call set_itv_139(ratio)
   !   case (140);  call set_itv_140(ratio)
   !   case (141);  call set_itv_141(ratio)
   !   case (142);  call set_itv_142(ratio)
   !   case (143);  call set_itv_143(ratio)
   !   case (144);  call set_itv_144(ratio)
   !   case (145);  call set_itv_145(ratio)
   !   case (146);  call set_itv_146(ratio)
   !   case (147);  call set_itv_147(ratio)
   !   case (148);  call set_itv_148(ratio)
   !   case (149);  call set_itv_149(ratio)
   !   case (150);  call set_itv_150(ratio)
   !   case (151);  call set_itv_151(ratio)
   !   case (152);  call set_itv_152(ratio)
   !   case (153);  call set_itv_153(ratio)
   !   case (154);  call set_itv_154(ratio)

      
     case (41) ; fcohbop   = xc(i)/scale(i)
     case (42) ; gapoh     = xc(i)/scale(i)
     case (43) ; write(*,*) 'Iteration variable 43 is not supported.'
     case (44) ; fvsbrnni  = xc(i)/scale(i)
     case (45) ; fqval     = xc(i)/scale(i)
     case (46) ; fpinj     = xc(i)/scale(i)
     case (47) ; feffcd    = xc(i)/scale(i)
     case (48) ; fstrcase  = xc(i)/scale(i)
     case (49) ; fstrcond  = xc(i)/scale(i)
     case (50) ; fiooic    = xc(i)/scale(i)
     case (51) ; fvdump    = xc(i)/scale(i)
     case (52) ; vdalw     = xc(i)/scale(i)
     case (53) ; fjprot    = xc(i)/scale(i)
     case (54) ; ftmargtf  = xc(i)/scale(i)
     case (55) ; write(*,*) 'Iteration variable 55 is not supported.'
     case (56) ; tdmptf    = xc(i)/scale(i)
     case (57) ; thkcas    = xc(i)/scale(i)
     case (58) ; thwcndut  = xc(i)/scale(i)
     case (59) ; fcutfsu   = xc(i)/scale(i)
     case (60) ; cpttf     = xc(i)/scale(i)
     case (61) ; gapds     = xc(i)/scale(i)
     case (62) ; fdtmp     = xc(i)/scale(i)
     case (63) ; ftpeak    = xc(i)/scale(i)
     case (64) ; fauxmn    = xc(i)/scale(i)
     case (65) ; tohs      = xc(i)/scale(i)
     case (66) ; ftohs     = xc(i)/scale(i)
     case (67) ; ftcycl    = xc(i)/scale(i)
     case (68) ; fptemp    = xc(i)/scale(i)
     case (69) ; rcool     = xc(i)/scale(i)
     case (70) ; vcool     = xc(i)/scale(i)
     case (71) ; fq        = xc(i)/scale(i)
     case (72) ; fipir     = xc(i)/scale(i)
     case (73) ; scrapli   = xc(i)/scale(i)
     case (74) ; scraplo   = xc(i)/scale(i)
     case (75) ; tfootfi   = xc(i)/scale(i)
     case (76) ; write(*,*) 'Iteration variable 76 is not supported.'
     case (77) ; write(*,*) 'Iteration variable 77 is not supported.'
     case (78) ; write(*,*) 'Iteration variable 78 is not supported.'
     case (79) ; fbetap    = xc(i)/scale(i)
     case (80) ; write(*,*) 'Iteration variable 80 is not supported.'
     case (81) ; write(*,*) 'Iteration variable 81 is not supported.'
     case (82) ; write(*,*) 'Iteration variable 82 is not supported.'
     case (83) ; write(*,*) 'Iteration variable 83 is not supported.'
     case (84) ; write(*,*) 'Iteration variable 84 is not supported.'
     case (85) ; write(*,*) 'Iteration variable 85 is not supported.'
     case (86) ; write(*,*) 'Iteration variable 86 is not supported.'
     case (87) ; write(*,*) 'Iteration variable 87 is not supported.'
     case (88) ; write(*,*) 'Iteration variable 88 is not supported.'
     case (89) ; ftbr      = xc(i)/scale(i)
     case (90) ; blbuith   = xc(i)/scale(i)
     case (91) ; blbuoth   = xc(i)/scale(i)
     case (92) ; fflutf    = xc(i)/scale(i)
     case (93) ; shldith   = xc(i)/scale(i)
     case (94) ; shldoth   = xc(i)/scale(i)
     case (95) ; fptfnuc   = xc(i)/scale(i)
     case (96) ; fvvhe     = xc(i)/scale(i)
     case (97) ; fpsepr    = xc(i)/scale(i)
     case (98) ; li6enrich = xc(i)/scale(i)
     case (99) ; write(*,*) 'Iteration variable 99 is not supported.'
     case (100); write(*,*) 'Iteration variable 100 is not supported.'
     case (101); write(*,*) 'Iteration variable 101 is not supported.'
     case (102)
        fimpvar = xc(i)/scale(i)
        impurity_arr(impvar)%frac = fimpvar
     case (103) ; flhthresh = xc(i)/scale(i)
     case (104) ; fcwr      = xc(i)/scale(i)
     case (105) ; fnbshinef = xc(i)/scale(i)
     case (106) ; ftmargoh  = xc(i)/scale(i)
     case (107) ; favail    = xc(i)/scale(i)
     case (108) ; breeder_f = xc(i)/scale(i)
     case (109) ; ralpne    = xc(i)/scale(i)
     case (110) ; ftaulimit = xc(i)/scale(i)
     case (111) ; fniterpump = xc(i)/scale(i)
     case (112) ; fzeffmax = xc(i)/scale(i)
     case (113) ; ftaucq = xc(i)/scale(i)
     case (114) ; fw_channel_length = xc(i)/scale(i)
     case (115) ; fpoloidalpower = xc(i)/scale(i)
     case (116) ; fradwall = xc(i)/scale(i)
     case (117) ; fpsepbqar = xc(i)/scale(i)
     case (118) ; fpsep = xc(i)/scale(i)
     case (119) ; tesep = xc(i)/scale(i)
     case (120) ; ttarget = xc(i)/scale(i)
     case (121) ; neratio = xc(i)/scale(i)
     case (122) ; oh_steel_frac = xc(i)/scale(i)
     case (123) ; foh_stress = xc(i)/scale(i)
     case (124) ; qtargettotal = xc(i)/scale(i)
     case (125) ; impurity_arr(3)%frac = xc(i)/scale(i)
     case (126) ; impurity_arr(4)%frac = xc(i)/scale(i)
     case (127) ; impurity_arr(5)%frac = xc(i)/scale(i)
     case (128) ; impurity_arr(6)%frac = xc(i)/scale(i)
     case (129) ; impurity_arr(7)%frac = xc(i)/scale(i)
     case (130) ; impurity_arr(8)%frac = xc(i)/scale(i)
     case (131) ; impurity_arr(9)%frac = xc(i)/scale(i)
     case (132) ; impurity_arr(10)%frac = xc(i)/scale(i)
     case (133) ; impurity_arr(11)%frac = xc(i)/scale(i)
     case (134) ; impurity_arr(12)%frac = xc(i)/scale(i)
     case (135) ; impurity_arr(13)%frac = xc(i)/scale(i)
     case (136) ; impurity_arr(14)%frac = xc(i)/scale(i)
     case (137) ; fplhsep = xc(i)/scale(i)
     case (138) ; rebco_thickness = xc(i)/scale(i)
     case (139) ; copper_thick = xc(i)/scale(i)
     case (140) ; thkwp = xc(i)/scale(i)
     case (141) ; fcqt = xc(i)/scale(i)
     case (142) ; nesep = xc(i)/scale(i)
     case (143) ; f_copperA_m2 = xc(i)/scale(i)
     case (144) ; fnesep = xc(i)/scale(i)
     case (145) ; fgwped = xc(i)/scale(i)
     case (146) ; fcpttf = xc(i)/scale(i)
     case (147) ; freinke = xc(i)/scale(i)
     case (148)
        fzactual = xc(i)/scale(i)
        impurity_arr(impvardiv)%frac = fzactual / impurity_enrichment(impvardiv)
        write(*,*) 'fzactual = ', fzactual
     case (149) ; fbmaxcs = xc(i)/scale(i)
     case (150) ; plasmod_fcdp = xc(i)/scale(i)
     case (151) ; plasmod_fradc = xc(i)/scale(i)
     case (152) ; fgwsep = xc(i)/scale(i)
     case (153) ; fpdivlim = xc(i)/scale(i)
     case (154) ; fne0 = xc(i)/scale(i)

     case default

        call report_error(57)

     end select

     !  Check that no iteration variable is zero

     if (abs(xc(i)) <= 1.0D-12) then
        idiags(1) = i ; idiags(2) = ixc(i)
        write(*,*) 'Iteration variable ',ixc(i),'(',trim(lablxc(ixc(i))),') is zero.'
        call report_error(58)
     end if

     !  Crude method of catching NaN errors

     !if ((abs(xc(i)) > 9.99D99).or.(xc(i) /= xc(i))) then
     if(variable_error(xc(i)))then
        idiags(1) = i ; idiags(2) = ixc(i) ; fdiags(1) = xc(i)
        call report_error(59)
     end if

     if (abs(scale(i)) < 1.0D-99) then
        idiags(1) = i ; idiags(2) = ixc(i)
        call report_error(60)
     end if

  end do

end subroutine convxc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine boundxc

  !+ad_name  boundxc
  !+ad_summ  Routine to convert variable bounds to their real values
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This subroutine converts the scaled iteration variable bounds
  !+ad_desc  back to their real values.
  !+ad_prob  None
  !+ad_call  numerics
  !+ad_hist  22/10/92 PJK Removed arguments (xc,nn)
  !+ad_hist  16/11/11 PJK Initial F90 version
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use numerics

  implicit none

  !  Local variables

  integer :: i

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,nvar
     bondl(i) = boundl(ixc(i))*scale(i)
     bondu(i) = boundu(ixc(i))*scale(i)
  end do

end subroutine boundxc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module define_iteration_variables
   use build_variables
   use constraint_variables
   use cost_variables
   use current_drive_variables
   use divertor_kallenbach_variables
   use divertor_variables
   use error_handling
   use fwbs_variables
   use heat_transport_variables
   use impurity_radiation_module
   use numerics
   use pfcoil_variables
   use physics_variables
   use plasmod_variables
   use rebco_variables
   use reinke_variables
   use tfcoil_variables
   use times_variables
   implicit none; public
   contains
   ! Iteration variables --------------------------------
   !+ad_vars  lablxc(ipnvars) : labels describing iteration variables<UL> 
   
   !---------------------------------
   subroutine init_itv_1
      !+ad_varc  <LI> ( 1) aspect
      lablxc(1) = 'aspect        '; boundl(1) = 1.100D0; boundu(1) = 10.00D0 
   end subroutine

   real(kind(1.d0)) function itv_1(); itv_1 = aspect; end 
   subroutine set_itv_1(ratio)
      real(kind(1.d0))::ratio
      aspect = ratio
   end subroutine
   !---------------------------------
   subroutine init_itv_2
      !+ad_varc  <LI> ( 2) bt
      lablxc(2) = 'bt            '; boundl(2) = 0.010D0 ; boundu(2) = 30.00D0
   end subroutine

   real(kind(1.d0)) function itv_2(); itv_2 = bt; end 
   subroutine set_itv_2(ratio)
      real(kind(1.d0))::ratio
      bt = ratio
   end subroutine
  !---------------------------------
  subroutine init_itv_3
       !+ad_varc  <LI> ( 3) rmajor
       lablxc(3) = 'rmajor        '; boundl(3) = 0.100D0 ; boundu(3) = 50.00D0  
  end subroutine

  real(kind(1.d0)) function itv_3(); itv_3 = rmajor; end

  subroutine set_itv_3(ratio)
    real(kind(1.d0))::ratio
    rmajor = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_4
       !+ad_varc  <LI> ( 4) te
       lablxc(4) = 'te            '; boundl(4) = 5.000D0 ; boundu(4) = 150.0D0  
  end subroutine

  real(kind(1.d0)) function itv_4(); itv_4 = te; end

  subroutine set_itv_4(ratio)
    real(kind(1.d0))::ratio
    te = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_5
       !+ad_varc  <LI> ( 5) beta
       lablxc(5) = 'beta          '; boundl(5) = 0.001D0 ; boundu(5) = 1.000D0  
  end subroutine

  real(kind(1.d0)) function itv_5(); itv_5 = beta; end

  subroutine set_itv_5(ratio)
    real(kind(1.d0))::ratio
    beta = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_6
       !+ad_varc  <LI> ( 6) dene
       lablxc(6) = 'dene          '; boundl(6) = 1.00D19 ; boundu(6) = 1.00D21 
  end subroutine

  real(kind(1.d0)) function itv_6(); itv_6 = dene; end

  subroutine set_itv_6(ratio)
    real(kind(1.d0))::ratio
    dene = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_7
       !+ad_varc  <LI> ( 7) rnbeam
       lablxc(7) = 'rnbeam        '; boundl(7) = 1.00D-6 ; boundu(7) = 1.000D0
  end subroutine

  real(kind(1.d0)) function itv_7(); itv_7 = rnbeam; end

  subroutine set_itv_7(ratio)
    real(kind(1.d0))::ratio
    rnbeam = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_8
       !+ad_varc  <LI> ( 8) fbeta (f-value for equation 6)
       lablxc(8) = 'fbeta         '; boundl(8) = 0.001D0 ; boundu(8) = 1.000D0  
  end subroutine

  real(kind(1.d0)) function itv_8(); itv_8 = fbeta; end

  subroutine set_itv_8(ratio)
    real(kind(1.d0))::ratio
    fbeta = ratio
  end subroutine
  !---------------------------------
  subroutine init_itv_9  
       !+ad_varc  <LI> ( 9) fdene (f-value for equation 5)
       lablxc(9) = 'fdene         '; boundl(9) =  0.001D0 ; boundu(9) = 1.000D0
  end subroutine

  real(kind(1.d0)) function itv_9(); itv_9 = fdene; end

  subroutine set_itv_9(ratio)
    real(kind(1.d0))::ratio
    fdene = ratio
  end subroutine

!---------------------------------
subroutine init_itv_10
       !+ad_varc  <LI> (10) hfact
       lablxc(10) = 'hfact         '; boundl(10) = 0.100D0 ; boundu(10) = 3.000D0  
end subroutine

real(kind(1.d0)) function itv_10()
   itv_10 =  hfact
end function

subroutine set_itv_10(ratio)
  real(kind(1.d0))::ratio
   hfact = ratio
end subroutine
!---------------------------------
subroutine init_itv_11 
       !+ad_varc  <LI> (11) pheat
       lablxc(11) = 'pheat         '; boundl(11) = 1.00D-3 ; boundu(11) = 1.000D3
end subroutine

real(kind(1.d0)) function itv_11()
   itv_11 =  pheat
end function

subroutine set_itv_11(ratio)
  real(kind(1.d0))::ratio
   pheat = ratio
end subroutine
!---------------------------------
subroutine init_itv_12
       !+ad_varc  <LI> (12) oacdcp
       lablxc(12) = 'oacdcp        '; boundl(12) = 1.000D5 ; boundu(12) =  1.500D8 
end subroutine

real(kind(1.d0)) function itv_12()
   itv_12 =  oacdcp
end function

subroutine set_itv_12(ratio)
  real(kind(1.d0))::ratio
   oacdcp = ratio
end subroutine
!---------------------------------
subroutine init_itv_13
       !+ad_varc  <LI> (13) tfcth (NOT RECOMMENDED)
       lablxc(13) = 'tfcth         '; boundl(13) = 0.100D0 ; boundu(13) = 5.000D0
end subroutine

real(kind(1.d0)) function itv_13()
   itv_13 =  tfcth
end function

subroutine set_itv_13(ratio)
  real(kind(1.d0))::ratio
   tfcth = ratio
end subroutine
!---------------------------------
subroutine init_itv_14
       !+ad_varc  <LI> (14) fwalld (f-value for equation 8)
       lablxc(14) = 'fwalld        '; boundl(14) = 0.001D0 ; boundu(14) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_14()
   itv_14 =  fwalld
end function

subroutine set_itv_14(ratio)
  real(kind(1.d0))::ratio
   fwalld = ratio
end subroutine
!---------------------------------
subroutine init_itv_15
       !+ad_varc  <LI> (15) fvs (f-value for equation 12)
       lablxc(15) = 'fvs           '; boundl(15) = 0.001D0 ; boundu(15) = 1.000
end subroutine

real(kind(1.d0)) function itv_15()
   itv_15 =  fvs
end function

subroutine set_itv_15(ratio)
  real(kind(1.d0))::ratio
   fvs = ratio
end subroutine
!---------------------------------
subroutine init_itv_16
       !+ad_varc  <LI> (16) ohcth
       lablxc(16) = 'ohcth         '; boundl(16) = 0.010D0 ; boundu(16) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_16()
   itv_16 =  ohcth
end function

subroutine set_itv_16(ratio)
  real(kind(1.d0))::ratio
   ohcth = ratio
end subroutine
!---------------------------------
subroutine init_itv_17
       !+ad_varc  <LI> (17) tdwell
       lablxc(17) = 'tdwell        '; boundl(17) = 0.100D0 ; boundu(17) = 1.000D8
end subroutine

real(kind(1.d0)) function itv_17()
   itv_17 =  tdwell
end function

subroutine set_itv_17(ratio)
  real(kind(1.d0))::ratio
   tdwell = ratio
end subroutine
!---------------------------------
subroutine init_itv_18
       !+ad_varc  <LI> (18) q
       lablxc(18) = 'q             '; boundl(18) = 2.000D0 ; boundu(18) = 50.00D0
end subroutine

real(kind(1.d0)) function itv_18()
   itv_18 =  q
end function

subroutine set_itv_18(ratio)
  real(kind(1.d0))::ratio
   q = ratio
end subroutine
!---------------------------------
subroutine init_itv_19
       !+ad_varc  <LI> (19) enbeam
       lablxc(19) = 'enbeam        '; boundl(19) = 1.000D0 ; boundu(19) = 1.000D6
end subroutine

real(kind(1.d0)) function itv_19()
   itv_19 =  enbeam
end function

subroutine set_itv_19(ratio)
  real(kind(1.d0))::ratio
  enbeam = ratio
end subroutine
!---------------------------------
subroutine init_itv_20
       !+ad_varc  <LI> (20) tcpav
       lablxc(20) = 'tcpav         '; boundl(20) = 40.00D0 ; boundu(20) = 3.000D2
end subroutine

real(kind(1.d0)) function itv_20()
   itv_20 = tcpav 
end function

subroutine set_itv_20(ratio)
  real(kind(1.d0))::ratio
  tcpav = ratio
end subroutine
!---------------------------------
subroutine init_itv_21
       !+ad_varc  <LI> (21) ftburn (f-value for equation 13)
       lablxc(21) = 'ftburn        '; boundl(21) = 0.001D0 ; boundu(21) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_21()
   itv_21 =  ftburn
end function

subroutine set_itv_21(ratio)
  real(kind(1.d0))::ratio
  ftburn = ratio
end subroutine
       !+ad_varc  <LI> (22) NOT USED
!---------------------------------
subroutine init_itv_23
       !+ad_varc  <LI> (23) fcoolcp
       lablxc(23) = 'fcoolcp       '; boundl(23) = 0.100D0 ; boundu(23) = 0.500D0
end subroutine

real(kind(1.d0)) function itv_23()
   itv_23 =  fcoolcp
end function

subroutine set_itv_23(ratio)
  real(kind(1.d0))::ratio
  fcoolcp = ratio
end subroutine
!---------------------------------
       !+ad_varc  <LI> (24) NOT USED
!---------------------------------
subroutine init_itv_25
       !+ad_varc  <LI> (25) fpnetel (f-value for equation 16)
       lablxc(25) = 'fpnetel       '; boundl(25) = 0.001D0; boundu(25) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_25()
   itv_25 =  fpnetel
end function

subroutine set_itv_25(ratio)
  real(kind(1.d0))::ratio
  fpnetel = ratio
end subroutine
!---------------------------------
subroutine init_itv_26
       !+ad_varc  <LI> (26) ffuspow (f-value for equation 9)
       lablxc(26) = 'ffuspow       '; boundl(26) = 0.001D0; boundu(26) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_26()
   itv_26 =  ffuspow
end function

subroutine set_itv_26(ratio)
  real(kind(1.d0))::ratio
  ffuspow = ratio
end subroutine
!---------------------------------
subroutine init_itv_27
       !+ad_varc  <LI> (27) fhldiv (f-value for equation 18)
       lablxc(27) = 'fhldiv        ';  boundl(27) = 0.001D0; boundu(27) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_27()
   itv_27 =  fhldiv
end function

subroutine set_itv_27(ratio)
  real(kind(1.d0))::ratio
  fhldiv = ratio
end subroutine
!---------------------------------
subroutine init_itv_28
       !+ad_varc  <LI> (28) fradpwr (f-value for equation 17), total radiation fraction
       lablxc(28) = 'fradpwr       '; boundl(28) = 0.001D0; boundu(28) = 0.990D0
end subroutine

real(kind(1.d0)) function itv_28()
   itv_28 =  fradpwr
end function

subroutine set_itv_28(ratio)
  real(kind(1.d0))::ratio
  fradpwr = ratio
end subroutine
!---------------------------------
subroutine init_itv_29
       !+ad_varc  <LI> (29) bore
       lablxc(29) = 'bore          '; boundl(29) = 0.100D0; boundu(29) = 10.00D0
end subroutine

real(kind(1.d0)) function itv_29()
   itv_29 =  bore
end function

subroutine set_itv_29(ratio)
  real(kind(1.d0))::ratio
  bore = ratio
end subroutine
!---------------------------------
subroutine init_itv_30
       !+ad_varc  <LI> (30) fmva (f-value for equation 19)
       lablxc(30) = 'fmva          '; boundl(30) = 0.010D0; boundu(30) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_30()
   itv_30 =  fmva
end function

subroutine set_itv_30(ratio)
  real(kind(1.d0))::ratio
  fmva = ratio
end subroutine
!---------------------------------
subroutine init_itv_31
       !+ad_varc  <LI> (31) gapomin
       lablxc(31) = 'gapomin       '; boundl(31) = 0.001D0; boundu(31) = 1.000D1
end subroutine

real(kind(1.d0)) function itv_31()
   itv_31 =  gapomin
end function

subroutine set_itv_31(ratio)
  real(kind(1.d0))::ratio
  gapomin = ratio
end subroutine
!---------------------------------
subroutine init_itv_32
       !+ad_varc  <LI> (32) frminor (f-value for equation 21)
       lablxc(32) = 'frminor       '; boundl(32) = 0.001D0; boundu(32) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_32()
   itv_32 =  frminor
end function

subroutine set_itv_32(ratio)
  real(kind(1.d0))::ratio
  frminor = ratio
end subroutine
!---------------------------------
subroutine init_itv_33
       !+ad_varc  <LI> (33) fportsz (f-value for equation 20)
       lablxc(33) = 'fportsz       '; boundl(33) = 0.001D0; boundu(33) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_33()
   itv_33 =  fportsz
end function

subroutine set_itv_33(ratio)
  real(kind(1.d0))::ratio
  fportsz = ratio
end subroutine
!---------------------------------
subroutine init_itv_34
       !+ad_varc  <LI> (34) fdivcol (f-value for equation 22)
       lablxc(34) = 'fdivcol       '; boundl(34) = 0.001D0; boundu(34) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_34()
   itv_34 =  fdivcol
end function

subroutine set_itv_34(ratio)
  real(kind(1.d0))::ratio
  fdivcol = ratio
end subroutine
!---------------------------------
subroutine init_itv_35
       !+ad_varc  <LI> (35) fpeakb (f-value for equation 25)
       lablxc(35) = 'fpeakb        '; boundl(35) = 0.001D0; boundu(35) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_35()
   itv_35 =  fpeakb
end function

subroutine set_itv_35(ratio)
  real(kind(1.d0))::ratio
  fpeakb = ratio
end subroutine
!---------------------------------
subroutine init_itv_36
       !+ad_varc  <LI> (36) fbetatry (f-value for equation 24)
       lablxc(36) = 'fbetatry      '; boundl(36) = 0.001D0; boundu(36) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_36()
   itv_36 =  fbetatry
end function

subroutine set_itv_36(ratio)
  real(kind(1.d0))::ratio
  fbetatry = ratio
end subroutine
!---------------------------------
subroutine init_itv_37
       !+ad_varc  <LI> (37) coheof
       lablxc(37) = 'coheof        '; boundl(37) = 1.000D5; boundu(37) = 1.000D8
end subroutine

real(kind(1.d0)) function itv_37()
   itv_37 =  coheof
end function

subroutine set_itv_37(ratio)
  real(kind(1.d0))::ratio
  coheof = ratio
end subroutine
!---------------------------------
subroutine init_itv_38
       !+ad_varc  <LI> (38) fjohc (f-value for equation 26)
       lablxc(38) = 'fjohc         '; boundl(38) = 0.010D0; boundu(38) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_38()
   itv_38 =  fjohc
end function

subroutine set_itv_38(ratio)
  real(kind(1.d0))::ratio
  fjohc = ratio
end subroutine
!---------------------------------
subroutine init_itv_39
       !+ad_varc  <LI> (39) fjohc0 (f-value for equation 27)
       lablxc(39) = 'fjohc0        '; boundl(39) = 0.001D0; boundu(39) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_39()
   itv_39 =  fjohc0
end function

subroutine set_itv_39(ratio)
  real(kind(1.d0))::ratio
  fjohc0 = ratio
end subroutine
!---------------------------------
subroutine init_itv_40
       !+ad_varc  <LI> (40) fgamcd (f-value for equation 37)
       lablxc(40) = 'fgamcd        '; boundl(40) = 0.001D0; boundu(40) = 1.000D0
end subroutine

real(kind(1.d0)) function itv_40()
   itv_40 =  fgamcd
end function

subroutine set_itv_40(ratio)
  real(kind(1.d0))::ratio
  fgamcd = ratio
end subroutine
!---------------------------------
! subroutine init_itv_41
! end subroutine

! real(kind(1.d0)) function itv_41()
!    itv_41 =  
! end function

! subroutine set_itv_41(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_42
! end subroutine

! real(kind(1.d0)) function itv_42()
!    itv_42 =  
! end function

! subroutine set_itv_42(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_43
! end subroutine

! real(kind(1.d0)) function itv_43()
!    itv_43 =  
! end function

! subroutine set_itv_43(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_44
! end subroutine

! real(kind(1.d0)) function itv_44()
!    itv_44 =  
! end function

! subroutine set_itv_44(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_45
! end subroutine

! real(kind(1.d0)) function itv_45()
!    itv_45 =  
! end function

! subroutine set_itv_45(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_46
! end subroutine

! real(kind(1.d0)) function itv_46()
!    itv_46 =  
! end function

! subroutine set_itv_46(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_47
! end subroutine

! real(kind(1.d0)) function itv_47()
!    itv_47 =  
! end function

! subroutine set_itv_47(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_48
! end subroutine

! real(kind(1.d0)) function itv_48()
!    itv_48 =  
! end function

! subroutine set_itv_48(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_49
! end subroutine

! real(kind(1.d0)) function itv_49()
!    itv_49 =  
! end function

! subroutine set_itv_49(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_50
! end subroutine

! real(kind(1.d0)) function itv_50()
!    itv_50 =  
! end function

! subroutine set_itv_50(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_51
! end subroutine

! real(kind(1.d0)) function itv_51()
!    itv_51 =  
! end function

! subroutine set_itv_51(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_52
! end subroutine

! real(kind(1.d0)) function itv_52()
!    itv_52 =  
! end function

! subroutine set_itv_52(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_53
! end subroutine

! real(kind(1.d0)) function itv_53()
!    itv_53 =  
! end function

! subroutine set_itv_53(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_54
! end subroutine

! real(kind(1.d0)) function itv_54()
!    itv_54 =  
! end function

! subroutine set_itv_54(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_55
! end subroutine

! real(kind(1.d0)) function itv_55()
!    itv_55 =  
! end function

! subroutine set_itv_55(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_56
! end subroutine

! real(kind(1.d0)) function itv_56()
!    itv_56 =  
! end function

! subroutine set_itv_56(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_57
! end subroutine

! real(kind(1.d0)) function itv_57()
!    itv_57 =  
! end function

! subroutine set_itv_57(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_58
! end subroutine

! real(kind(1.d0)) function itv_58()
!    itv_58 =  
! end function

! subroutine set_itv_58(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_59
! end subroutine

! real(kind(1.d0)) function itv_59()
!    itv_59 =  
! end function

! subroutine set_itv_59(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_60
! end subroutine

! real(kind(1.d0)) function itv_60()
!    itv_60 =  
! end function

! subroutine set_itv_60(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_61
! end subroutine

! real(kind(1.d0)) function itv_61()
!    itv_61 =  
! end function

! subroutine set_itv_61(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_62
! end subroutine

! real(kind(1.d0)) function itv_62()
!    itv_62 =  
! end function

! subroutine set_itv_62(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_63
! end subroutine

! real(kind(1.d0)) function itv_63()
!    itv_63 =  
! end function

! subroutine set_itv_63(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_64
! end subroutine

! real(kind(1.d0)) function itv_64()
!    itv_64 =  
! end function

! subroutine set_itv_64(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_65
! end subroutine

! real(kind(1.d0)) function itv_65()
!    itv_65 =  
! end function

! subroutine set_itv_65(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_66
! end subroutine

! real(kind(1.d0)) function itv_66()
!    itv_66 =  
! end function

! subroutine set_itv_66(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_67
! end subroutine

! real(kind(1.d0)) function itv_67()
!    itv_67 =  
! end function

! subroutine set_itv_67(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_68
! end subroutine

! real(kind(1.d0)) function itv_68()
!    itv_68 =  
! end function

! subroutine set_itv_68(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_69
! end subroutine

! real(kind(1.d0)) function itv_69()
!    itv_69 =  
! end function

! subroutine set_itv_69(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_70
! end subroutine

! real(kind(1.d0)) function itv_70()
!    itv_70 =  
! end function

! subroutine set_itv_70(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_71
! end subroutine

! real(kind(1.d0)) function itv_71()
!    itv_71 =  
! end function

! subroutine set_itv_71(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_72
! end subroutine

! real(kind(1.d0)) function itv_72()
!    itv_72 =  
! end function

! subroutine set_itv_72(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_73
! end subroutine

! real(kind(1.d0)) function itv_73()
!    itv_73 =  
! end function

! subroutine set_itv_73(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_74
! end subroutine

! real(kind(1.d0)) function itv_74()
!    itv_74 =  
! end function

! subroutine set_itv_74(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_75
! end subroutine

! real(kind(1.d0)) function itv_75()
!    itv_75 =  
! end function

! subroutine set_itv_75(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_76
! end subroutine

! real(kind(1.d0)) function itv_76()
!    itv_76 =  
! end function

! subroutine set_itv_76(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_77
! end subroutine

! real(kind(1.d0)) function itv_77()
!    itv_77 =  
! end function

! subroutine set_itv_77(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_78
! end subroutine

! real(kind(1.d0)) function itv_78()
!    itv_78 =  
! end function

! subroutine set_itv_78(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_79
! end subroutine

! real(kind(1.d0)) function itv_79()
!    itv_79 =  
! end function

! subroutine set_itv_79(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_80
! end subroutine

! real(kind(1.d0)) function itv_80()
!    itv_80 =  
! end function

! subroutine set_itv_80(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_81
! end subroutine

! real(kind(1.d0)) function itv_81()
!    itv_81 =  
! end function

! subroutine set_itv_81(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_82
! end subroutine

! real(kind(1.d0)) function itv_82()
!    itv_82 =  
! end function

! subroutine set_itv_82(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_83
! end subroutine

! real(kind(1.d0)) function itv_83()
!    itv_83 =  
! end function

! subroutine set_itv_83(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_84
! end subroutine

! real(kind(1.d0)) function itv_84()
!    itv_84 =  
! end function

! subroutine set_itv_84(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_85
! end subroutine

! real(kind(1.d0)) function itv_85()
!    itv_85 =  
! end function

! subroutine set_itv_85(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_86
! end subroutine

! real(kind(1.d0)) function itv_86()
!    itv_86 =  
! end function

! subroutine set_itv_86(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_87
! end subroutine

! real(kind(1.d0)) function itv_87()
!    itv_87 =  
! end function

! subroutine set_itv_87(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_88
! end subroutine

! real(kind(1.d0)) function itv_88()
!    itv_88 =  
! end function

! subroutine set_itv_88(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_89
! end subroutine

! real(kind(1.d0)) function itv_89()
!    itv_89 =  
! end function

! subroutine set_itv_89(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_90
! end subroutine

! real(kind(1.d0)) function itv_90()
!    itv_90 =  
! end function

! subroutine set_itv_90(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_91
! end subroutine

! real(kind(1.d0)) function itv_91()
!    itv_91 =  
! end function

! subroutine set_itv_91(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_92
! end subroutine

! real(kind(1.d0)) function itv_92()
!    itv_92 =  
! end function

! subroutine set_itv_92(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_93
! end subroutine

! real(kind(1.d0)) function itv_93()
!    itv_93 =  
! end function

! subroutine set_itv_93(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_94
! end subroutine

! real(kind(1.d0)) function itv_94()
!    itv_94 =  
! end function

! subroutine set_itv_94(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_95
! end subroutine

! real(kind(1.d0)) function itv_95()
!    itv_95 =  
! end function

! subroutine set_itv_95(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_96
! end subroutine

! real(kind(1.d0)) function itv_96()
!    itv_96 =  
! end function

! subroutine set_itv_96(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_97
! end subroutine

! real(kind(1.d0)) function itv_97()
!    itv_97 =  
! end function

! subroutine set_itv_97(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_98
! end subroutine

! real(kind(1.d0)) function itv_98()
!    itv_98 =  
! end function

! subroutine set_itv_98(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_99
! end subroutine

! real(kind(1.d0)) function itv_99()
!    itv_99 =  
! end function

! subroutine set_itv_99(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_100
! end subroutine

! real(kind(1.d0)) function itv_100()
!    itv_100 =  
! end function

! subroutine set_itv_100(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_101
! end subroutine

! real(kind(1.d0)) function itv_101()
!    itv_101 =  
! end function

! subroutine set_itv_101(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_102
! end subroutine

! real(kind(1.d0)) function itv_102()
!    itv_102 =  
! end function

! subroutine set_itv_102(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_103
! end subroutine

! real(kind(1.d0)) function itv_103()
!    itv_103 =  
! end function

! subroutine set_itv_103(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_104
! end subroutine

! real(kind(1.d0)) function itv_104()
!    itv_104 =  
! end function

! subroutine set_itv_104(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_105
! end subroutine

! real(kind(1.d0)) function itv_105()
!    itv_105 =  
! end function

! subroutine set_itv_105(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_106
! end subroutine

! real(kind(1.d0)) function itv_106()
!    itv_106 =  
! end function

! subroutine set_itv_106(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_107
! end subroutine

! real(kind(1.d0)) function itv_107()
!    itv_107 =  
! end function

! subroutine set_itv_107(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_108
! end subroutine

! real(kind(1.d0)) function itv_108()
!    itv_108 =  
! end function

! subroutine set_itv_108(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_109
! end subroutine

! real(kind(1.d0)) function itv_109()
!    itv_109 =  
! end function

! subroutine set_itv_109(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_110
! end subroutine

! real(kind(1.d0)) function itv_110()
!    itv_110 =  
! end function

! subroutine set_itv_110(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_111
! end subroutine

! real(kind(1.d0)) function itv_111()
!    itv_111 =  
! end function

! subroutine set_itv_111(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_112
! end subroutine

! real(kind(1.d0)) function itv_112()
!    itv_112 =  
! end function

! subroutine set_itv_112(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_113
! end subroutine

! real(kind(1.d0)) function itv_113()
!    itv_113 =  
! end function

! subroutine set_itv_113(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_114
! end subroutine

! real(kind(1.d0)) function itv_114()
!    itv_114 =  
! end function

! subroutine set_itv_114(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_115
! end subroutine

! real(kind(1.d0)) function itv_115()
!    itv_115 =  
! end function

! subroutine set_itv_115(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_116
! end subroutine

! real(kind(1.d0)) function itv_116()
!    itv_116 =  
! end function

! subroutine set_itv_116(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_117
! end subroutine

! real(kind(1.d0)) function itv_117()
!    itv_117 =  
! end function

! subroutine set_itv_117(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_118
! end subroutine

! real(kind(1.d0)) function itv_118()
!    itv_118 =  
! end function

! subroutine set_itv_118(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_119
! end subroutine

! real(kind(1.d0)) function itv_119()
!    itv_119 =  
! end function

! subroutine set_itv_119(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_120
! end subroutine

! real(kind(1.d0)) function itv_120()
!    itv_120 =  
! end function

! subroutine set_itv_120(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_121
! end subroutine

! real(kind(1.d0)) function itv_121()
!    itv_121 =  
! end function

! subroutine set_itv_121(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_122
! end subroutine

! real(kind(1.d0)) function itv_122()
!    itv_122 =  
! end function

! subroutine set_itv_122(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_123
! end subroutine

! real(kind(1.d0)) function itv_123()
!    itv_123 =  
! end function

! subroutine set_itv_123(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_124
! end subroutine

! real(kind(1.d0)) function itv_124()
!    itv_124 =  
! end function

! subroutine set_itv_124(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_125
! end subroutine

! real(kind(1.d0)) function itv_125()
!    itv_125 =  
! end function

! subroutine set_itv_125(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_126
! end subroutine

! real(kind(1.d0)) function itv_126()
!    itv_126 =  
! end function

! subroutine set_itv_126(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_127
! end subroutine

! real(kind(1.d0)) function itv_127()
!    itv_127 =  
! end function

! subroutine set_itv_127(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_128
! end subroutine

! real(kind(1.d0)) function itv_128()
!    itv_128 =  
! end function

! subroutine set_itv_128(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_129
! end subroutine

! real(kind(1.d0)) function itv_129()
!    itv_129 =  
! end function

! subroutine set_itv_129(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_130
! end subroutine

! real(kind(1.d0)) function itv_130()
!    itv_130 =  
! end function

! subroutine set_itv_130(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_131
! end subroutine

! real(kind(1.d0)) function itv_131()
!    itv_131 =  
! end function

! subroutine set_itv_131(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_132
! end subroutine

! real(kind(1.d0)) function itv_132()
!    itv_132 =  
! end function

! subroutine set_itv_132(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_133
! end subroutine

! real(kind(1.d0)) function itv_133()
!    itv_133 =  
! end function

! subroutine set_itv_133(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_134
! end subroutine

! real(kind(1.d0)) function itv_134()
!    itv_134 =  
! end function

! subroutine set_itv_134(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_135
! end subroutine

! real(kind(1.d0)) function itv_135()
!    itv_135 =  
! end function

! subroutine set_itv_135(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_136
! end subroutine

! real(kind(1.d0)) function itv_136()
!    itv_136 =  
! end function

! subroutine set_itv_136(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_137
! end subroutine

! real(kind(1.d0)) function itv_137()
!    itv_137 =  
! end function

! subroutine set_itv_137(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_138
! end subroutine

! real(kind(1.d0)) function itv_138()
!    itv_138 =  
! end function

! subroutine set_itv_138(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_139
! end subroutine

! real(kind(1.d0)) function itv_139()
!    itv_139 =  
! end function

! subroutine set_itv_139(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_140
! end subroutine

! real(kind(1.d0)) function itv_140()
!    itv_140 =  
! end function

! subroutine set_itv_140(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_141
! end subroutine

! real(kind(1.d0)) function itv_141()
!    itv_141 =  
! end function

! subroutine set_itv_141(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_142
! end subroutine

! real(kind(1.d0)) function itv_142()
!    itv_142 =  
! end function

! subroutine set_itv_142(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_143
! end subroutine

! real(kind(1.d0)) function itv_143()
!    itv_143 =  
! end function

! subroutine set_itv_143(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_144
! end subroutine

! real(kind(1.d0)) function itv_144()
!    itv_144 =  
! end function

! subroutine set_itv_144(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_145
! end subroutine

! real(kind(1.d0)) function itv_145()
!    itv_145 =  
! end function

! subroutine set_itv_145(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_146
! end subroutine

! real(kind(1.d0)) function itv_146()
!    itv_146 =  
! end function

! subroutine set_itv_146(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_147
! end subroutine

! real(kind(1.d0)) function itv_147()
!    itv_147 =  
! end function

! subroutine set_itv_147(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_148
! end subroutine

! real(kind(1.d0)) function itv_148()
!    itv_148 =  
! end function

! subroutine set_itv_148(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_149
! end subroutine

! real(kind(1.d0)) function itv_149()
!    itv_149 =  
! end function

! subroutine set_itv_149(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_150
! end subroutine

! real(kind(1.d0)) function itv_150()
!    itv_150 =  
! end function

! subroutine set_itv_150(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_151
! end subroutine

! real(kind(1.d0)) function itv_151()
!    itv_151 =  
! end function

! subroutine set_itv_151(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_152
! end subroutine

! real(kind(1.d0)) function itv_152()
!    itv_152 =  
! end function

! subroutine set_itv_152(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_153
! end subroutine

! real(kind(1.d0)) function itv_153()
!    itv_153 =  
! end function

! subroutine set_itv_153(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine
! !---------------------------------
! subroutine init_itv_154
! end subroutine

! real(kind(1.d0)) function itv_154()
!    itv_154 =  
! end function

! subroutine set_itv_154(ratio)
!   real(kind(1.d0))::ratio
!    = ratio
! end subroutine


   !+ad_varc  </UL>
end module define_iteration_variables