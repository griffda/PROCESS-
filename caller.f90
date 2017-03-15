! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine caller(xc,nvars)

  !+ad_name  caller
  !+ad_summ  Routine to call the physics and engineering modules
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  xc(ipnvars) : input real : Array of iteration variables
  !+ad_args  nvars : input integer : Number of active iteration variables
  !+ad_desc  This routine is the principal caller of all the physics and
  !+ad_desc  engineering modules.
  !+ad_prob  None
  !+ad_call  availability_module
  !+ad_call  build_module
  !+ad_call  buildings_module
  !+ad_call  ccfe_hcpb_module
  !+ad_call  costs_module
  !+ad_call  current_drive_module
  !+ad_call  divertor_module
  !+ad_call  fwbs_module
  !+ad_call  ife_module
  !+ad_call  ife_variables
  !+ad_call  kit_hcpb_module
  !+ad_call  numerics
  !+ad_call  pfcoil_module
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  plasma_geometry_module
  !+ad_call  power_module
  !+ad_call  process_output
  !+ad_call  pulse_module
  !+ad_call  rfp_module
  !+ad_call  rfp_variables
  !+ad_call  sctfcoil_module
  !+ad_call  startup_module
  !+ad_call  stellarator_module
  !+ad_call  stellarator_variables
  !+ad_call  structure_module
  !+ad_call  tfcoil_module
  !+ad_call  vacuum_module
  !+ad_call  acpow
  !+ad_call  avail
  !+ad_call  avail_2
  !+ad_call  bldgcall
  !+ad_call  cntrpst
  !+ad_call  convxc
  !+ad_call  costs
  !+ad_call  divcall
  !+ad_call  fispac
  !+ad_call  fwbs
  !+ad_call  geomty
  !+ad_call  ifecll
  !+ad_call  induct
  !+ad_call  loca
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
  !+ad_hist  29/10/12 PJK Added vacuum_module
  !+ad_hist  30/10/12 PJK Added power_module
  !+ad_hist  30/10/12 PJK Added buildings_module
  !+ad_hist  30/10/12 PJK Added build_module
  !+ad_hist  31/10/12 PJK Added stellarator_variables
  !+ad_hist  31/10/12 PJK Added stellarator_module
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  05/11/12 PJK Added rfp_module
  !+ad_hist  05/11/12 PJK Added ife_variables
  !+ad_hist  05/11/12 PJK Added ife_module
  !+ad_hist  05/11/12 PJK Added pulse_module
  !+ad_hist  06/11/12 PJK Added startup_module
  !+ad_hist  06/11/12 PJK Added availability_module
  !+ad_hist  06/11/12 PJK Added plasma_geometry_module
  !+ad_hist  19/06/14 PJK Removed obsolete calls to nbeam, ech, lwhymod
  !+ad_hist  02/12/14 JM  Added new availability model in caller (avail_2)
  !+ad_hist  13/03/15 JM  Changed calling of blanket models and import of blanket modules
  !+ad_hist  20/05/16 JM  Added more detailed comments to the caller
  !+ad_hist  10/02/17 JM  Added divertor Kallenbach divertor model
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use availability_module
  use build_module
  use buildings_module
  use costs_module
  use costs_2015_module
  use current_drive_module
  use divertor_module
  use divertor_ode
  use divertor_Kallenbach_variables
  use fwbs_variables
  use ife_module
  use ife_variables
  use numerics
  use pfcoil_module
  use physics_module
  use physics_variables
  use plasma_geometry_module
  use power_module
  use process_output
  use pulse_module
  use rfp_module
  use rfp_variables
  use sctfcoil_module
  use startup_module
  use stellarator_module
  use stellarator_variables
  use structure_module
  use tfcoil_module
  use vacuum_module
  use cost_variables

  ! Import blanket modules !
  !!!!!!!!!!!!!!!!!!!!!!!!!!

  use ccfe_hcpb_module
  use kit_hcpb_module
  use kit_hcll_module

  implicit none

  !  Arguments !
  !!!!!!!!!!!!!!

  real(kind(1.0D0)), dimension(ipnvars), intent(in) :: xc
  integer, intent(in) :: nvars

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Increment the call counter
  ncalls = ncalls + 1

  !  Convert variables !
  !!!!!!!!!!!!!!!!!!!!!!

  call convxc(xc,nvars)

  !  Perform the various function calls !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Stellarator caller !
  !!!!!!!!!!!!!!!!!!!!!!

  if (istell /= 0) then
     call stcall
     return
  end if

  !  Inertial Fusion Energy caller !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (ife /= 0) then
     call ifecll
     return
  end if

  !  Tokamak and RFP calls !
  !!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Plasma geometry model !
  !!!!!!!!!!!!!!!!!!!!!!!!!

  call geomty

  ! Machine Build Model !
  !!!!!!!!!!!!!!!!!!!!!!!

  ! Radial build
  call radialb(nout,0)

  ! Vertical build
  call vbuild(nout,0)

  ! Physics model !
  !!!!!!!!!!!!!!!!!

  if (irfp == 0) then
     ! if a tokamak
     call physics
  else
     ! if a rfp machine
     call rfpphy
  end if

  ! startup model (not used) !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !call startup(nout,0)  !  commented-out for speed reasons

  ! Toroidal field coil model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (irfp == 0) then
     ! if a tokamak
     call tfcoil(nout,0)
  else
     ! if a rfp machine
     call rfptfc(nout,0)
  end if

  ! Toroidal field coil superconductor model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call tfspcall(nout,0)

  ! Poloidal field coil model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (irfp == 0) then
     ! if a tokamak

     ! Poloidal field and OH coil model
     call pfcoil

     ! Poloidal field coil inductance calculation
     call induct(nout,0)

     ! Volt-second capability of PF coil set
     call vsec

  else
     ! if a rfp machine
     call rfppfc(nout,0)

  end if

  ! Pulsed reactor model !
  !!!!!!!!!!!!!!!!!!!!!!!!

  call pulse(nout,0)

  ! Blanket model !
  !!!!!!!!!!!!!!!!!!

  ! Blanket switch values
  ! No.  |  model
  ! ---- | ------
  ! 1    |  CCFE HCPB model
  ! 2    |  KIT HCPB model
  ! 3    |  CCFE HCPB model with Tritium Breeding Ratio calculation
  ! 4    |  KIT HCLL model

  if (iblanket == 1) then           ! CCFE HCPB model
	   call ccfe_hcpb(nout, 0)
  else if (iblanket == 2) then      ! KIT HCPB model
     call kit_hcpb(nout, 0)
  else if (iblanket == 3) then      ! CCFE HCPB model with Tritium Breeding Ratio calculation
     call ccfe_hcpb(nout, 0)
	   call tbr_shimwell(nout, 0, breeder_f, li6enrich, iblanket_thickness, tbr)
  else if (iblanket == 4) then      ! KIT HCLL model
     call kit_hcll(nout, 0)
  end if

  ! Divertor Model !
  !!!!!!!!!!!!!!!!!!

  ! New divertor model
  if(kallenbach_switch.eq.1) then

    call divertor_Kallenbach(rmajor=rmajor,rminor=rminor, &
        bt=bt,plascur=plascur,                                &
        bvert=bvert,q=q,                                      &
        verboseset=.false.,                                   &
        lambda_tar=lambda_target,lambda_omp=lambda_q,         &
        Ttarget=Ttarget,qtargettotal=qtargettotal,            &
        targetangle=targetangle,Lcon=Lcon, &
        netau_in=netau,unit_test=.false.,abserrset=1.d-6,     &
        helium_enrichment=helium_enrichment,                  &
        impurity_enrichment=impurity_enrichment,              &
        psep_kallenbach=psep_kallenbach, teomp=teomp, neomp=neomp, &
        outfile=nout,iprint=0 )                               

  else if(kallenbach_switch.eq.0) then
  
    ! Old Divertor Model ! Comment this out MDK 30/11/16
    call divcall(nout,0)

  end if

  ! Structure Model !
  !!!!!!!!!!!!!!!!!!!

  call strucall(nout,0)

  ! Tight aspect ratio machine model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (itart == 1) then
     call cntrpst(nout,0)
  end if

  ! Toroidal field coil power model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call tfpwr(nout,0)

  ! Poloidal field coil power model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (irfp == 0) then
     ! if a tokamak
     call pfpwr(nout,0)
  else
     ! if a rfp machine
     call rfppfp(nout,0)
  end if

  ! Plant heat transport part 1 !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call power1

  ! Vacuum model !
  !!!!!!!!!!!!!!!!

  call vaccall(nout,0)

  ! Buildings model (1990 costs model only) !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (cost_model==0) then
     call bldgcall(nout,0)
  end if

  ! Plant AC power requirements !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call acpow(nout,0)

  ! Plant heat transport pt 2 & 3!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call power2(nout,0)

  call power3(nout,0)

  ! Availability model !
  !!!!!!!!!!!!!!!!!!!!!!

  ! Availability switch values
  ! No.  |  model
  ! ---- | ------
  ! 0    |  Input value for cfactr
  ! 1    |  Ward and Taylor model (1999)
  ! 2    |  Morris model (2015)

  if (iavail > 1) then
     call avail_2(nout,0)  ! Morris model (2015)
  else
     call avail(nout,0)    ! Taylor and Ward model (1999)
  end if

  ! Costs model !
  !!!!!!!!!!!!!!!

  ! Cost switch values
  ! No.  |  model
  ! ---- | ------
  ! 0    |  1990 costs model
  ! 1    |  2015 Kovari model

  if (cost_model == 1) then
     call costs_2015(0,0)
  else
     call costs(nout,0)
  end if

  ! FISPACT and LOCA model (not used) !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  if (ifispact.eq.1) then
  !     call fispac(0)
  !     call loca(nout,0)
  !  end if

end subroutine caller
