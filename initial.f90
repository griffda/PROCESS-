!  $Id::                                                                $
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
  !+ad_call  pulse.h90
  !+ad_call  rfp.h90
  !+ad_call  ife.h90
  !+ad_call  devtyp
  !+ad_call  ifeini
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output
  use stellarator_module
  use stellarator_variables

  implicit none

  include 'pulse.h90'
  include 'rfp.h90'
  include 'ife.h90'

  !  Arguments

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Pulsed reactor

  afw = 0.005D0
  bctmp = 320.0D0
  bfw = 0.0D0
  coolp = 15.5D6
  dtstor = 300.0D0
  fwlife = 0.0D0
  istore = 1
  itcycl = 1
  lpulse = 0
  tmprse = 40.0D0
  tpeak = 0.0D0

  !  Reversed field pinch parameters

  irfp     = 0
  pfmmax   = 0.0D0
  pfrmax   = 0.0D0
  rfpf     = 0.0D0
  rfpth    = 1.5D0
  tftort   = 0.33D0
  rrpf(:)   = 0.0D0
  zzpf(:)   = 0.0D0
  drpf(:)   = 0.0D0
  dzpf(:)   = 0.0D0
  nturns(:) = 0.0D0
  cptrfp(:) = 0.0D0
  resrfp(:) = 0.0D0

  !  See which type of device is being modelled

  call devtyp

  !  Initialise stellarator parameters if necessary

  if (istell == 1) call stinit

  !  Initialise inertial fusion energy parameters if necessary

  if (ife == 1) call ifeini

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
  !+ad_call  stellarator_variables
  !+ad_call  rfp.h90
  !+ad_call  ife.h90
  !+ad_hist  27/02/96 PJK Initial version
  !+ad_hist  08/10/96 PJK Fixed error: (istell.gt.2) should be (idev.gt.2)
  !+ad_hist  14/03/97 PJK idev=3 ==> inertial fusion power plant
  !+ad_hist  19/09/12 PJK Initial F90 version
  !+ad_hist  31/10/12 PJK Added stellarator_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use stellarator_variables

  implicit none

  include 'rfp.h90'
  include 'ife.h90'

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

     case (3)  !  Inertial Fusion Energy  model
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
  !+ad_call  global_variables
  !+ad_call  heat_transport_variables
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  tfcoil_variables
  !+ad_call  pulse.h90
  !+ad_call  rfp.h90
  !+ad_call  ife.h90
  !+ad_hist  08/10/96 PJK Initial upgraded version
  !+ad_hist  23/01/97 PJK Moved resetting of trithtmw from POWER
  !+ad_hist  14/03/97 PJK Added coding relevant to IFE device
  !+ad_hist  01/04/98 PJK Added rnbeam reset for no NBI
  !+ad_hist  19/01/99 PJK Added warning about IITER flag with non-ITER profiles
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use current_drive_variables
  use global_variables
  use heat_transport_variables
  use pfcoil_variables
  use physics_variables
  use process_output
  use tfcoil_variables

  implicit none

  include 'pulse.h90'
  include 'rfp.h90'
  include 'ife.h90'

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Pulsed power plant model

  if (lpulse == 1) icase = 'PROCESS pulsed tokamak model'

  !  D-He3 option

  if (idhe3 == 1) then
     icase = 'PROCESS D-He3 tokamak model'
     ftr = max(ftrit,1.0D-6)
     triv = 0.0D0
     ifispact = 0
     trithtmw = 0.0D0
  end if

  !  Tight aspect ratio options 

  if (itart == 1) then

     icase  = 'PROCESS tight aspect ratio tokamak model'

     bore   = 0.0D0
     gapoh  = 0.0D0
     ohcth  = 0.0D0
     ddwi   = 0.0D0

     if (icurr /= 2) then
        write(iotty,*) 'Warning in routine CHECK:'
        write(iotty,*) 'Normal current scaling for TARTs, ICURR=2,'
        write(iotty,*) 'is not being used.'
        write(iotty,*) 'PROCESS continuing...'
        write(iotty,*) ' '
     end if
     iohcl  = 0
     ipfloc(1) = 2
     ipfloc(2) = 3
     ipfloc(3) = 3
     itfsup = 0
     if (ibss == 1) then
        write(nout,*) 'ibss=1 is not a valid option for a TART device'
        write(nout,*) 'PROCESS stopping.'
        stop
     end if

  else

     ishape = 0
     if (icurr == 2) then
        write(nout,*) &
             'icurr=2 is not a valid option for a non-TART device'
        write(nout,*) 'PROCESS stopping.'
        stop
     end if

  end if

  !  Reversed Field Pinch model

  if (irfp == 1) then

     if (itart == 1) then
        write(nout,*) 'itart=1 is not a valid option for the RFP model'
        write(nout,*) 'PROCESS stopping.'
        stop
     end if

     bcylth   = 0.0D0
     ddwi     = 0.0D0
     kappa    = 1.0D0
     icase    = 'PROCESS reversed field pinch model'
     iefrf    = 9
     ifispact = 0
     iohcl    = 0
     ipfres   = 1
     isumatpf = 3
     itfmod   = 0
     itfsup   = 0
     ohcth    = 0.0D0
     pfclres  = 1.7D-8
     tfootfi  = 1.0D0
     triang   = 0.0D0
     vf(:) = 0.1D0
  end if

  !  Inertial Fusion Energy model

  if (ife == 1) then
     icase    = 'PROCESS inertial fusion energy model'
     lpulse   = 0
     idhe3    = 0
  end if

  !  Ensure that if TF coils are non-superconducting,
  !  only simple stress calculations are performed

  if (itfsup == 0) itfmod = 0

  !  PF coil resistivity is zero if superconducting

  if (ipfres == 0) pfclres = 0.0D0

  !  If there is no NBI, then hot beam density should be zero

  if (irfcd == 1) then
     if ((iefrf /= 5).and.(iefrf /= 8)) rnbeam = 0.0D0
  else
     rnbeam = 0.0D0
  end if

  !  Check on use of iiter

  if ( (iiter /= 0).and. &
       ((alphan /= 0.5D0).or.(alphat /= 1.0D0)) ) then
     write(iotty,*) 'Warning: IITER = 1 should only be used if'
     write(iotty,*) '         ALPHAN = 0.5 and ALPHAT = 1.0'
     write(iotty,*) 'PROCESS continuing...'
     write(iotty,*) ' '
  end if

end subroutine check
