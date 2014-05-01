!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine loadxc

  !+ad_name  loadxc
  !+ad_summ  Routine to load the physics and engineering variables into the
  !+ad_summ  optimisation variables array
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This subroutine loads the physics and engineering variables
  !+ad_desc  into the optimisation variables array <CODE>XCM</CODE>.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  ife_variables
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  rfp_variables
  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  pulse_variables
  !+ad_hist  22/10/92 PJK Removed original arguments (xc,nn)
  !+ad_hist  14/11/11 PJK Changed NaN error check
  !+ad_hist  09/10/12 PJK Initial F90 version
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  18/10/12 PJK Added fwbs_variables
  !+ad_hist  18/10/12 PJK Added pfcoil_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Added constraint_variables
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  05/11/12 PJK Added ife_variables
  !+ad_hist  05/11/12 PJK Added pulse_variables
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constraint_variables
  use current_drive_variables
  use divertor_variables
  use fwbs_variables
  use heat_transport_variables
  use ife_variables
  use numerics
  use pfcoil_variables
  use physics_variables
  use pulse_variables
  use rfp_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables

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
        if (istell == 1) then
           write(*,*) 'Error in routine LOADXC:'
           write(*,*) 'TFCTH should not be used as an iteration'
           write(*,*) 'variable if ISTELL=1, as it is'
           write(*,*) 'calculated elsewhere.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
     case (14) ; xcm(i) = fwalld
     case (15) ; xcm(i) = fvs
     case (16) ; xcm(i) = ohcth
     case (17) ; xcm(i) = tdwell
     case (18) ; xcm(i) = q
     case (19) ; xcm(i) = enbeam
     case (20) ; xcm(i) = tcpav
     case (21) ; xcm(i) = ftburn
     case (22) ; xcm(i) = tbrnmn
     case (23) ; xcm(i) = fcoolcp
     case (24) ; xcm(i) = cdtfleg
        if ((itfsup == 1).or.(irfp == 1)) then
           write(*,*) 'Error in routine LOADXC:'
           write(*,*) 'CDTFLEG should not be used as an iteration'
           write(*,*) 'variable if ITFSUP=1 or IRFP=1.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
     case (25) ; xcm(i) = fpnetel
     case (26) ; xcm(i) = ffuspow
     case (27) ; xcm(i) = fhldiv
     case (28) ;
        write(*,*) 'Iteration variable 28 is currently not in use...'
        write(*,*) 'PROCESS stopping.'
        stop
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
     case (43) ; xcm(i) = cfe0
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
     case (55) ; xcm(i) = tmargmin
     case (56) ; xcm(i) = tdmptf
     case (57) ; xcm(i) = thkcas
        if ((itfmod /= 1).or.(istell == 1)) then
           write(*,*) 'Error in routine LOADXC:'
           write(*,*) 'THKCAS cannot be used as an iteration'
           write(*,*) 'variable if ITFMOD .ne. 1 or ISTELL=1, as it is'
           write(*,*) 'calculated elsewhere.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
     case (58) ; xcm(i) = thwcndut
     case (59) ; xcm(i) = fcutfsu
     case (60) ; xcm(i) = cpttf
        if ((istell == 1).or.((irfp == 0).and.(itfsup == 0))) then
           write(*,*) 'Error in routine LOADXC:'
           write(*,*) 'CPTTF cannot be used as an iteration'
           write(*,*) 'variable if ITFSUP=0 or ISTELL=1, as it is'
           write(*,*) 'calculated elsewhere.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
     case (61) ; xcm(i) = gapds
     case (62) ; xcm(i) = fdtmp
     case (63) ; xcm(i) = ftpeak
     case (64) ; xcm(i) = fauxmn
     case (65) ; xcm(i) = tohs
        if (lpulse /= 1) then
           write(*,*) 'Error in routine LOADXC:'
           write(*,*) 'TOHS cannot be used as an iteration'
           write(*,*) 'variable if LPULSE .ne. 1, as it is'
           write(*,*) 'calculated elsewhere.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
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
     case (76) ; xcm(i) = frfptf
     case (77) ; xcm(i) = tftort
        if (istell == 1) then
           write(*,*) 'Error in routine LOADXC:'
           write(*,*) 'TFTORT should not be used as an iteration'
           write(*,*) 'variable if ISTELL=1, as it is'
           write(*,*) 'calculated elsewhere.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
     case (78) ; xcm(i) = rfpth
     case (79) ; xcm(i) = fbetap
     case (80) ; xcm(i) = frfpf
     case (81) ; xcm(i) = edrive
     case (82) ; xcm(i) = drveff
     case (83) ; xcm(i) = tgain
     case (84) ; xcm(i) = chrad
     case (85) ; xcm(i) = pdrive
     case (86) ; xcm(i) = frrmax
     case (87) ; xcm(i) = helecmw
        if ((ihplant < 1).or.(ihplant > 3)) then
           write(*,*) 'Error in routine LOADXC :'
           write(*,*) 'HELECMW cannot be used as an iteration'
           write(*,*) 'variable if IHPLANT=',ihplant
           write(*,*) 'PROCESS stopping.'
           stop
        end if
     case (88) ; xcm(i) = hthermmw
        if (ihplant < 4) then
           write(*,*) 'Error in routine LOADXC :'
           write(*,*) 'HTHERMMW cannot be used as an iteration'
           write(*,*) 'variable if IHPLANT=',ihplant
           write(*,*) 'as it is calculated elsewhere.'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
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
     case (99) ; xcm(i) = ftftort
     case (100) ; xcm(i) = ftfthko
     case (101) ; xcm(i) = prp

     case default
        write(*,*) 'Error in routine LOADXC :'
        write(*,*) 'Illegal variable number, = ',ixc(i)
        write(*,*) 'PROCESS stopping.'
        stop

     end select

     !  Check that no iteration variable is zero

     if (abs(xcm(i)) <= 1.0D-12) then
        write(*,*) 'Error in routine LOADXC :'
        write(*,*) 'Iteration variable ',ixc(i), &
             '(',trim(lablxc(ixc(i))),') is zero.'
        write(*,*) 'Change initial value or lower bound.'
        write(*,*) 'PROCESS stopping.'
        stop
     end if

     !  Crude method of catching NaN errors

     if ( (abs(xcm(i)) > 9.99D99).or.(xcm(i) /= xcm(i)) ) then
        write(*,*) 'Error in routine LOADXC :'
        write(*,*) 'NaN error for iteration variable ',ixc(i)
        write(*,*) 'PROCESS stopping.'
        stop
     end if

  end do

  do i = 1,nvar
     if (xcm(i) /= 0.0D0) then
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
  !+ad_cont  N/A
  !+ad_args  xc(ipnvars) : input/output real array : scaled iteration variable values
  !+ad_args  nn : input integer : number of iteration variables
  !+ad_desc  This subroutine converts the scaled iteration variables back to
  !+ad_desc  their real values.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  ife_variables
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  rfp_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  14/11/11 PJK Changed NaN error check
  !+ad_hist  16/11/11 PJK Initial F90 version
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  18/10/12 PJK Added fwbs_variables
  !+ad_hist  18/10/12 PJK Added pfcoil_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Added constraint_variables
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  05/11/12 PJK Added ife_variables
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constraint_variables
  use current_drive_variables
  use divertor_variables
  use fwbs_variables
  use heat_transport_variables
  use ife_variables
  use numerics
  use pfcoil_variables
  use physics_variables
  use rfp_variables
  use tfcoil_variables
  use times_variables

  implicit none

  !  Arguments

  integer, intent(in) :: nn
  real(kind(1.0D0)), dimension(ipnvars), intent(inout) :: xc

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
     case (22) ; tbrnmn    = xc(i)/scale(i)
     case (23) ; fcoolcp   = xc(i)/scale(i)
     case (24) ; cdtfleg   = xc(i)/scale(i)
     case (25) ; fpnetel   = xc(i)/scale(i)
     case (26) ; ffuspow   = xc(i)/scale(i)
     case (27) ; fhldiv    = xc(i)/scale(i)
     case (28) ; 
        write(*,*) 'Iteration variable 28 is currently not in use...'
        write(*,*) 'PROCESS stopping.'
        stop
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
     case (43) ; cfe0      = xc(i)/scale(i)
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
     case (55) ; tmargmin  = xc(i)/scale(i)
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
     case (76) ; frfptf    = xc(i)/scale(i)
     case (77) ; tftort    = xc(i)/scale(i)
     case (78) ; rfpth     = xc(i)/scale(i)
     case (79) ; fbetap    = xc(i)/scale(i)
     case (80) ; frfpf     = xc(i)/scale(i)
     case (81) ; edrive    = xc(i)/scale(i)
     case (82) ; drveff    = xc(i)/scale(i)
     case (83) ; tgain     = xc(i)/scale(i)
     case (84) ; chrad     = xc(i)/scale(i)
     case (85) ; pdrive    = xc(i)/scale(i)
     case (86) ; frrmax    = xc(i)/scale(i)
     case (87) ; helecmw   = xc(i)/scale(i)
     case (88) ; hthermmw  = xc(i)/scale(i)
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
     case (99) ; ftftort   = xc(i)/scale(i)
     case (100) ; ftfthko  = xc(i)/scale(i)
     case (101) ; prp      = xc(i)/scale(i)

     case default
        write(*,*) 'Error in routine CONVXC :'
        write(*,*) 'Illegal variable number, = ',ixc(i)
        write(*,*) 'PROCESS stopping.'
        stop

     end select

     !  Check that no iteration variable is zero

     if (abs(xc(i)) <= 1.0D-12) then
        write(*,*) 'Error in routine CONVXC :'
        write(*,*) 'Iteration variable ',ixc(i),' &
             (',trim(lablxc(ixc(i))),') is zero.'
        write(*,*) 'Change initial value or lower bound.'
        write(*,*) 'PROCESS stopping.'
        stop
     end if

     !  Crude method of catching NaN errors

     if ((abs(xc(i)) > 9.99D99).or.(xc(i) /= xc(i))) then
        write(*,*) 'Error in routine CONVXC :'
        write(*,*) 'NaN error for iteration variable ',ixc(i)
        write(*,*) 'PROCESS stopping.'
        stop
     end if

     if (scale(i) == 0.0D0) then
        write(*,*) 'Error in routine CONVXC :'
        write(*,*) 'scale(i) = 0 for iteration variable ',ixc(i)
        write(*,*) 'PROCESS stopping.'
        stop
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
