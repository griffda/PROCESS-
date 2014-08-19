! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initial

  !+ad_name  initial
  !+ad_summ  Routine to initialise all the COMMON block variables
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine initialises all the COMMON block variables.
  !+ad_desc  N.B. Many of these variables are re-initialised elsewhere in the
  !+ad_desc  code, but are set to zero in this routine anyway.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  stellarator_module
  !+ad_call  stellarator_variables
  !+ad_call  devtyp
  !+ad_call  stinit
  !+ad_hist  08/10/96 PJK Initial upgraded version
  !+ad_hist  20/01/97 PJK Added htpmw
  !+ad_hist  22/01/97 PJK Subsumed heattr.h, heatrinp.h and pfelect.h into
  !+ad_hisc               htpwr.h
  !+ad_hist  05/02/97 PJK Added Martensitic steel fractions fms...
  !+ad_hist  13/02/97 PJK Changed initial value of fwlife to zero (no
  !+ad_hisc               longer an input parameter, but calculated)
  !+ad_hist  21/03/97 PJK Added call to initialise IFE variables
  !+ad_hisc               and added new iteration variables 81--86
  !+ad_hist  18/11/97 PJK Added ITER-97 scaling laws, IVMS,DRTOP,DZTOP
  !+ad_hist  01/04/98 PJK Added ITER-96P scaling law, IGNITE, DNLA
  !+ad_hist  24/04/98 PJK Added IMPC,IMPFE,IMPO,FKBLKT
  !+ad_hist  17/07/98 PJK Added PTHRMW
  !+ad_hist  08/10/98 PJK Added new ITER H98 scaling laws
  !+ad_hist  19/01/99 PJK Added IGEOM, POWERHT
  !+ad_hist  19/05/99 PJK Added new availability model variables
  !+ad_hist  06/07/99 PJK Added BCRITSC, JCRITSC, TCRITSC
  !+ad_hist  25/04/02 PJK Added ZEFFDIV
  !+ad_hist  15/06/04 PJK Added IPRIMHTP
  !+ad_hist  22/05/06 PJK Added IFALPHAP
  !+ad_hist  22/05/07 PJK Added hydrogen plant variables
  !+ad_hist  21/03/11 PJK Changed default value of FEFFCD to 1.0
  !+ad_hist  26/07/11 PJK Added JCRIT_MODEL
  !+ad_hist  09/11/11 PJK Removed ICULCR
  !+ad_hist  19/09/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Removed IVMS
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Removed physics variables from list
  !+ad_hist  15/10/12 PJK Removed numerics variables from list
  !+ad_hist  16/10/12 PJK Removed current drive variables from list
  !+ad_hist  17/10/12 PJK Removed divertor variables from list
  !+ad_hist  18/10/12 PJK Removed first wall, blanket, shield variables
  !+ad_hist  18/10/12 PJK Removed PF coil variables
  !+ad_hist  18/10/12 PJK Removed TF coil variables
  !+ad_hist  29/10/12 PJK Removed structure variables
  !+ad_hist  29/10/12 PJK Removed vacuum variables
  !+ad_hist  29/10/12 PJK Removed PF coil power conversion variables
  !+ad_hist  30/10/12 PJK Removed heat transport variables
  !+ad_hist  30/10/12 PJK Removed times variables
  !+ad_hist  30/10/12 PJK Removed buildings variables
  !+ad_hist  30/10/12 PJK Removed build variables
  !+ad_hist  31/10/12 PJK Removed cost variables
  !+ad_hist  31/10/12 PJK Removed inequality variables
  !+ad_hist  31/10/12 PJK Added stellarator_variables
  !+ad_hist  31/10/12 PJK Added stellarator_module
  !+ad_hist  31/10/12 PJK Removed RFP variables
  !+ad_hist  05/11/12 PJK Removed call to ifeini
  !+ad_hist  05/11/12 PJK Removed pulsed reactor variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output
  use stellarator_module
  use stellarator_variables

  implicit none

  !  Arguments

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  See which type of device is being modelled

  call devtyp

  !  Initialise stellarator parameters if necessary

  if (istell == 1) call stinit

end subroutine initial

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine devtyp

  !+ad_name  devtyp
  !+ad_summ  Routine to determine which type of device is to be modelled
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine uses the contents of an input file,
  !+ad_desc  <CODE>device.dat</CODE>, to determine which type of device
  !+ad_desc  is to be modelled. If the file is not present in the current
  !+ad_desc  directory, a standard tokamak model is assumed.
  !+ad_prob  None
  !+ad_call  ife_variables
  !+ad_call  rfp_variables
  !+ad_call  stellarator_variables
  !+ad_hist  27/02/96 PJK Initial version
  !+ad_hist  08/10/96 PJK Fixed error: (istell.gt.2) should be (idev.gt.2)
  !+ad_hist  14/03/97 PJK idev=3 ==> inertial fusion power plant
  !+ad_hist  19/09/12 PJK Initial F90 version
  !+ad_hist  31/10/12 PJK Added stellarator_variables
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  05/11/12 PJK Added ife_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use ife_variables
  use rfp_variables
  use stellarator_variables

  implicit none

  !  Local variables

  integer :: idev
  logical :: iexist

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  istell = 0
  irfp   = 0
  ife    = 0

  !  Read a second input file. If the file does not exist, then the
  !  standard tokamak option is assumed.

  inquire(file='device.dat',exist=iexist)

  if (iexist) then

     open(unit=1,file='device.dat',status='old')
     read(1,*) idev
     close(unit=1)

     !  Set relevant switch

     select case (idev)

     case (1)  !  Stellarator model
        istell = 1

     case (2)  !  Reversed Field Pinch model
        irfp = 1

     case (3)  !  Inertial Fusion Energy model
        ife = 1

     case default  !  Tokamak model
        continue

     end select

  end if

end subroutine devtyp

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine check

  !+ad_name  check
  !+ad_summ  Routine to reset specific variables if certain options are
  !+ad_summ  being used
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine performs a sanity check of the input variables
  !+ad_desc  and ensures other dependent variables are given suitable values.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  buildings_variables
  !+ad_call  current_drive_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  global_variables
  !+ad_call  heat_transport_variables
  !+ad_call  ife_variables
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  pulse_variables
  !+ad_call  rfp_variables
  !+ad_call  tfcoil_variables
  !+ad_call  report_error
  !+ad_hist  08/10/96 PJK Initial upgraded version
  !+ad_hist  23/01/97 PJK Moved resetting of trithtmw from POWER
  !+ad_hist  14/03/97 PJK Added coding relevant to IFE device
  !+ad_hist  01/04/98 PJK Added rnbeam reset for no NBI
  !+ad_hist  19/01/99 PJK Added warning about iiter flag with non-ITER profiles
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added global_variables module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  18/10/12 PJK Added pfcoil_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  30/10/12 PJK Added buildings_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  05/11/12 PJK Added ife_variables
  !+ad_hist  05/11/12 PJK Added pulse_variables
  !+ad_hist  18/12/12 PJK Added snull and other PF coil location checks
  !+ad_hist  11/04/13 PJK Energy storage building volume set to zero if lpulse=0
  !+ad_hist  23/05/13 PJK Coolant type set to water if blktmodel>0
  !+ad_hist  10/06/13 PJK Removed enforcement of ishape=0 for non-TART tokamaks
  !+ad_hist  11/09/13 PJK Added check for fuel ion fractions; removed idhe3 setting;
  !+ad_hisc               removed iiter usage
  !+ad_hist  08/05/14 PJK Replaced itfmod with tfc_model
  !+ad_hist  13/05/14 PJK Added impurity fraction initialisations
  !+ad_hist  02/06/14 PJK Added fimpvar usage
  !+ad_hist  24/06/14 PJK Removed refs to bcylth
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  23/07/14 PJK Modified icase descriptions
  !+ad_hist  19/08/14 PJK Added trap for nvar < neqns
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use current_drive_variables
  use error_handling
  use fwbs_variables
  use global_variables
  use heat_transport_variables
  use impurity_radiation_module
  use ife_variables
  use numerics
  use pfcoil_variables
  use physics_variables
  use process_output
  use pulse_variables
  use rfp_variables
  use tfcoil_variables

  implicit none

  !  Local variables

  integer :: i,j,k,imp

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  errors_on = .true.

  !  Check that there are sufficient iteration variables

  if (nvar < neqns) then
     idiags(1) = nvar ; idiags(2) = neqns
     call report_error(137)
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

  if (imprad_model == 1) then
     do imp = 1,nimp
        impurity_arr(imp)%frac = fimp(imp)
     end do
  end if

  !  Set relevant impurity fraction if iteration variable 102 is turned on

  if ( any(ixc == 102) ) then
     impurity_arr(impvar)%frac = fimpvar
  end if

  !  Warn if ion power balance equation is being used with the new radiation model

  if ((imprad_model == 1).and.(any(icc == 3))) then
     call report_error(138)
  end if

  !  Tight aspect ratio options 

  if (itart == 1) then

     icase  = 'Tight aspect ratio tokamak model'

     bore   = 0.0D0
     gapoh  = 0.0D0
     ohcth  = 0.0D0
     ddwi   = 0.0D0

     if (icurr /= 2) then
        idiags(1) = icurr ; call report_error(37)
     end if
     iohcl  = 0
     ipfloc(1) = 2
     ipfloc(2) = 3
     ipfloc(3) = 3
     itfsup = 0

     if (ibss == 1) call report_error(38)
     if (snull == 1) call report_error(39)

  else

     if (icurr == 2) call report_error(40)

     if (snull == 0) then
        idivrt = 2
     else  !  snull == 1
        idivrt = 1
     end if

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
     if ((snull == 1).and.(j < 2)) call report_error(44)

  end if

  !  Reversed Field Pinch model

  if (irfp == 1) then

     if (itart == 1) call report_error(45)

     ddwi     = 0.0D0
     kappa    = 1.0D0
     icase    = 'Reversed field pinch model'
     iefrf    = 9
     ifispact = 0
     iohcl    = 0
     ipfres   = 1
     isumatpf = 3
     tfc_model = 0
     itfsup   = 0
     ohcth    = 0.0D0
     pfclres  = 1.7D-8
     tfootfi  = 1.0D0
     triang   = 0.0D0
     vf(:) = 0.1D0
  end if

  !  Inertial Fusion Energy model

  if (ife == 1) then
     icase    = 'Inertial Fusion Energy model'
     lpulse   = 0
  end if

  !  Pulsed power plant model

  if (lpulse == 1) then
     icase = 'Pulsed tokamak model'
  else
     esbldgm3 = 0.0D0
  end if

  !  Ensure that if TF coils are non-superconducting,
  !  only simple stress calculations are performed

  if (itfsup == 0) tfc_model = 0

  !  PF coil resistivity is zero if superconducting

  if (ipfres == 0) pfclres = 0.0D0

  !  If there is no NBI, then hot beam density should be zero

  if (irfcd == 1) then
     if ((iefrf /= 5).and.(iefrf /= 8)) rnbeam = 0.0D0
  else
     rnbeam = 0.0D0
  end if

  !  Coolant set to water if blktmodel > 0
  !  Although the blanket is by definition helium-cooled in this case,
  !  the shield etc. are assumed to be water-cooled, and since water is
  !  heavier (and the unit cost of pumping it is higher), the calculation
  !  for coolmass is better done with costr=2 if blktmodel > 0 to give
  !  slightly pessimistic results.
  !  However, this also means that if lblnkt=1 the wrong coolant will be assumed
  !  in the thermodynamic blanket model...

  if (blktmodel > 0) costr = 2

  errors_on = .false.

end subroutine check
