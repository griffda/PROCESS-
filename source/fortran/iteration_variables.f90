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

     case (1)  ; xcm(i) = aspect
     case (2)  ; xcm(i) = bt
     case (3)  ; xcm(i) = rmajor
     case (4)  ; xcm(i) = te
     case (5)  ; xcm(i) = beta
     case (6)  ; xcm(i) = dene
     case (7)  ; xcm(i) = rnbeam
     case (8)  ; xcm(i) = fbeta
     case (9)  ; xcm(i) = fdene
     case (10) ; xcm(i) = hfact
     case (11) ; xcm(i) = pheat
     case (12) ; xcm(i) = oacdcp
     case (13) ; xcm(i) = tfcth
        if (istell == 1) call report_error(46)
     case (14) ; xcm(i) = fwalld
     case (15) ; xcm(i) = fvs
     case (16) ; xcm(i) = ohcth
     case (17) ; xcm(i) = tdwell
     case (18) ; xcm(i) = q
     case (19) ; xcm(i) = enbeam
     case (20) ; xcm(i) = tcpav
     case (21) ; xcm(i) = ftburn
     case (22) ; write(*,*) 'Iteration variable 22 is not supported.'
     case (23) ; xcm(i) = fcoolcp
     case (24) ; write(*,*) 'Iteration variable 24 is not supported.'
     case (25) ; xcm(i) = fpnetel
     case (26) ; xcm(i) = ffuspow
     case (27) ; xcm(i) = fhldiv
     case (28) ; xcm(i) = fradpwr
     case (29) ; xcm(i) = bore
     case (30) ; xcm(i) = fmva
     case (31) ; xcm(i) = gapomin
     case (32) ; xcm(i) = frminor
     case (33) ; xcm(i) = fportsz
     case (34) ; xcm(i) = fdivcol
     case (35) ; xcm(i) = fpeakb
     case (36) ; xcm(i) = fbetatry
     case (37) ; xcm(i) = coheof
     case (38) ; xcm(i) = fjohc
     case (39) ; xcm(i) = fjohc0
     case (40) ; xcm(i) = fgamcd
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
        if ((istell == 1).or.(itfsup == 0)) call report_error(49)
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do i = 1,nn

     select case (ixc(i))

     case (1) ;  aspect    = xc(i)/scale(i)
     case (2) ;  bt        = xc(i)/scale(i)
     case (3) ;  rmajor    = xc(i)/scale(i)
     case (4) ;  te        = xc(i)/scale(i)
     case (5) ;  beta      = xc(i)/scale(i)
     case (6) ;  dene      = xc(i)/scale(i)
     case (7) ;  rnbeam    = xc(i)/scale(i)
     case (8) ;  fbeta     = xc(i)/scale(i)
     case (9) ;  fdene     = xc(i)/scale(i)
     case (10) ; hfact     = xc(i)/scale(i)
     case (11) ; pheat     = xc(i)/scale(i)
     case (12) ; oacdcp    = xc(i)/scale(i)
     case (13) ; tfcth     = xc(i)/scale(i)
     case (14) ; fwalld    = xc(i)/scale(i)
     case (15) ; fvs       = xc(i)/scale(i)
     case (16) ; ohcth     = xc(i)/scale(i)
     case (17) ; tdwell    = xc(i)/scale(i)
     case (18) ; q         = xc(i)/scale(i)
     case (19) ; enbeam    = xc(i)/scale(i)
     case (20) ; tcpav     = xc(i)/scale(i)
     case (21) ; ftburn    = xc(i)/scale(i)
     case (22) ; write(*,*) 'Iteration variable 22 is not supported.'
     case (23) ; fcoolcp   = xc(i)/scale(i)
     case (24) ; cdtfleg   = xc(i)/scale(i)
     case (25) ; fpnetel   = xc(i)/scale(i)
     case (26) ; ffuspow   = xc(i)/scale(i)
     case (27) ; fhldiv    = xc(i)/scale(i)
     case (28) ; fradpwr   = xc(i)/scale(i)
     case (29) ; bore      = xc(i)/scale(i)
     case (30) ; fmva      = xc(i)/scale(i)
     case (31) ; gapomin   = xc(i)/scale(i)
     case (32) ; frminor   = xc(i)/scale(i)
     case (33) ; fportsz   = xc(i)/scale(i)
     case (34) ; fdivcol   = xc(i)/scale(i)
     case (35) ; fpeakb    = xc(i)/scale(i)
     case (36) ; fbetatry  = xc(i)/scale(i)
     case (37) ; coheof    = xc(i)/scale(i)
     case (38) ; fjohc     = xc(i)/scale(i)
     case (39) ; fjohc0    = xc(i)/scale(i)
     case (40) ; fgamcd    = xc(i)/scale(i)
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
