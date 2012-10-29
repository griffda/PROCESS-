!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine caller(xc,nvars)

  !+ad_name  caller
  !+ad_summ  Routine to call the physics and engineering modules
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  xc(ipnvars) : input real : Array of iteration variables
  !+ad_args  nvars : input integer : Number of active iteration variables
  !+ad_desc  This routine is the principal caller of all the physics and
  !+ad_desc  engineering modules.
  !+ad_prob  None
  !+ad_call  costs_module
  !+ad_call  current_drive_module
  !+ad_call  divertor_module
  !+ad_call  fwbs_module
  !+ad_call  numerics
  !+ad_call  pfcoil_module
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  sctfcoil_module
  !+ad_call  structure_module
  !+ad_call  tfcoil_module
  !+ad_call  ife.h90
  !+ad_call  rfp.h90
  !+ad_call  stella.h90
  !+ad_call  acpow
  !+ad_call  avail
  !+ad_call  bldgcall
  !+ad_call  cntrpst
  !+ad_call  convxc
  !+ad_call  costs
  !+ad_call  divcall
  !+ad_call  ech
  !+ad_call  fispac
  !+ad_call  fwbs
  !+ad_call  geomty
  !+ad_call  ifecll
  !+ad_call  induct
  !+ad_call  loca
  !+ad_call  lwhymod
  !+ad_call  nbeam
  !+ad_call  pfcoil
  !+ad_call  pfpwr
  !+ad_call  physics
  !+ad_call  power1
  !+ad_call  power2
  !+ad_call  pulse
  !+ad_call  radialb
  !+ad_call  rfppfc
  !+ad_call  rfppfp
  !+ad_call  rfpphy
  !+ad_call  rfptfc
  !+ad_call  startup
  !+ad_call  stcall
  !+ad_call  strucall
  !+ad_call  tfcoil
  !+ad_call  tfpwr
  !+ad_call  tfspcall
  !+ad_call  vaccall
  !+ad_call  vbuild
  !+ad_call  vsec
  !+ad_hist  28/06/94 PJK Improved code layout
  !+ad_hist  29/01/96 PJK Added routine CNTRPST
  !+ad_hist  23/01/97 PJK Split routine POWER into POWER1 and POWER2
  !+ad_hist  06/02/97 PJK Added routine LOCA
  !+ad_hist  21/03/97 PJK Added routine IFECLL
  !+ad_hist  18/11/97 PJK Removed NOUT argument from FISPAC call
  !+ad_hist  19/05/99 PJK Added routine AVAIL
  !+ad_hist  24/05/06 PJK Moved call to STRUCALL after DIVCALL
  !+ad_hist  27/07/11 PJK Initial F90 version
  !+ad_hist  24/09/12 PJK Swapped argument order of RADIALB, DIVCALL, INDUCT
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added costs_module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added physics_module
  !+ad_hist  17/10/12 PJK Added current_drive_module
  !+ad_hist  17/10/12 PJK Added divertor_module
  !+ad_hist  18/10/12 PJK Added fwbs_module
  !+ad_hist  18/10/12 PJK Added pfcoil_module
  !+ad_hist  29/10/12 PJK Added tfcoil_module
  !+ad_hist  29/10/12 PJK Added sctfcoil_module
  !+ad_hist  29/10/12 PJK Added structure_module
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use costs_module
  use current_drive_module
  use divertor_module
  use fwbs_module
  use numerics
  use pfcoil_module
  use physics_module
  use physics_variables
  use process_output
  use sctfcoil_module
  use structure_module
  use tfcoil_module

  implicit none

  include 'stella.h90'
  include 'rfp.h90'
  include 'ife.h90'

  !  Arguments

  real(kind(1.0D0)), dimension(ipnvars), intent(in) :: xc
  integer, intent(in) :: nvars

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Increment the call counter

  ncalls = ncalls + 1

  !  Convert variables

  call convxc(xc,nvars)

  !  Perform the various function calls

  !  Stellarator calls

  if (istell /= 0) then
     call stcall
     return
  end if

  !  Inertial Fusion Energy calls

  if (ife /= 0) then
     call ifecll
     return
  end if

  !  Tokamak and RFP calls

  call geomty
  call radialb(nout,0)
  call vbuild

  if (irfp == 0) then
     call physics
  else
     call rfpphy
  end if

  !  Commented out start-up option as it takes a long time
  !  call startup(0)

  call ech(nout,0)
  call lwhymod(nout,0)
  call nbeam(nout,0) 

  if (irfp == 0) then
     call tfcoil(nout,0)
  else
     call rfptfc(nout,0)
  end if

  call tfspcall(nout,0)

  if (irfp == 0) then
     call pfcoil
     call induct(nout,0)
     call vsec
  else
     call rfppfc(nout,0)
  end if

  call pulse(nout,0)
  call fwbs(nout,0)
  call divcall(nout,0)
  call strucall(nout,0)

  if (itart == 1) call cntrpst(nout,0)

  call tfpwr(nout,0)

  if (irfp == 0) then
     call pfpwr(nout,0)
  else
     call rfppfp(nout,0)
  end if

  call power1
  call vaccall(nout,0)
  call bldgcall(nout,0)
  call acpow(nout,0)
  call power2(nout,0)
  call avail(nout,0)
  call costs(nout,0)

  !+**PJK  if (ifispact.eq.1) then
  !+**PJK     call fispac(0)
  !+**PJK     call loca(nout,0)
  !+**PJK  end if

end subroutine caller
