! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine caller(xc,nvars)
  !! Routine to call the physics and engineering modules
  !! author: P J Knight, CCFE, Culham Science Centre
  !! author: J Morris, CCFE, Culham Science Centre
  !! xc(ipnvars) : input real : Array of iteration variables
  !! nvars : input integer : Number of active iteration variables
  !! This routine is the principal caller of all the physics and
  !! engineering modules.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use availability_module, only: avail, avail_2
  use build_module, only: radialb, vbuild
  use buildings_module, only: bldgcall
  use costs_module, only: costs
  use costs_2015_module, only: costs_2015
  use costs_step_module, only: costs_step
  use divertor_module, only: divcall
  use divertor_ode, only: divertor_Kallenbach
  use div_kal_vars, only: kallenbach_switch, ttarget, qtargettotal, &
    targetangle, psep_kallenbach, teomp, neomp
  use fwbs_variables, only: iblanket, breeder_f, li6enrich, iblanket_thickness, tbr
  use ife_module, only: ifecll
  use ife_variables, only: ife
  use pfcoil_module, only: pfcoil, induct, vsec
  use physics_module, only: physics
  use physics_variables, only: rmajor, rminor, bt, plascur, q, bp, ipedestal, itart
  use plasma_geometry_module, only: geomty
  use power_module, only: tfpwr, pfpwr, power1, power2, power3, acpow
  use numerics, only: ncalls, ipnvars
  use pulse_module, only: pulse
  use sctfcoil_module, only: tfspcall
  use stellarator_module, only: stcall
  use stellarator_variables, only: istell
  use structure_module, only: strucall
  use tfcoil_module, only: tfcoil, cntrpst
  use tfcoil_variables, only: i_tf_sup
  use vacuum_module, only: vaccall
  use water_use_module, only: waterusecall
  use cost_variables, only: iavail, cost_model
  use constants, only: nout
  use global_variables, only: verbose

  ! Import blanket modules
  use ccfe_hcpb_module, only: ccfe_hcpb, tbr_shimwell
  use kit_hcpb_module, only: kit_hcpb
  use kit_hcll_module, only: kit_hcll

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  !  Arguments !
  ! !!!!!!!!!!!!!
  real(dp), dimension(ipnvars), intent(in) :: xc
  integer, intent(in) :: nvars
  logical :: verbose_logical

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Increment the call counter
  ncalls = ncalls + 1

  !  Convert variables !
  ! !!!!!!!!!!!!!!!!!!!!!

  call convxc(xc,nvars)

  !  Perform the various function calls !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Stellarator caller !
  ! !!!!!!!!!!!!!!!!!!!!!

  if (istell /= 0) then
    call stcall
    return
  end if

  !  Inertial Fusion Energy calls !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (ife /= 0) then
    call ifecll
    return
  end if

  !  Tokamak calls !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!

  ! Plasma geometry model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!

  call geomty

  ! Machine Build Model !
  ! !!!!!!!!!!!!!!!!!!!!!!

  ! Radial build
  call radialb(nout,0)

  ! Vertical build
  call vbuild(nout,0)

  call physics

  !call build subroutines again if PLASMOD used, issue #650
  if (ipedestal == 3) then
    ! Radial build
    call radialb(nout,0)

    ! Vertical build
    call vbuild(nout,0)
  endif

  ! startup model (not used) !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!
  !call startup(nout,0)  !  commented-out for speed reasons

  ! Toroidal field coil model !
  call tfcoil(nout,0)

  ! Toroidal field coil superconductor model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if ( i_tf_sup == 1 ) then
    call tfspcall(nout,0)
  end if 

  ! Poloidal field and Central Solenoid model
  call pfcoil

  ! Poloidal field coil inductance calculation
  call induct(nout,0)

  ! Volt-second capability of PF coil set
  call vsec

  ! Pulsed reactor model !
  ! !!!!!!!!!!!!!!!!!!!!!!!
  call pulse(nout,0)

  ! Blanket model !
  ! !!!!!!!!!!!!!!!!!

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
  ! !!!!!!!!!!!!!!!!!
  if(verbose==1) then
      verbose_logical = .true.
  else
      verbose_logical = .false.
  endif

  ! New divertor model
  if(kallenbach_switch.eq.1) then
    call divertor_Kallenbach(rmajor=rmajor,rminor=rminor, &
        bt=bt,plascur=plascur,                                &
        q=q,                                      &
        verboseset=verbose_logical,                                   &
        Ttarget=Ttarget,qtargettotal=qtargettotal,            &
        targetangle=targetangle, &
        unit_test=.false.,     &
        bp = bp,   &
        psep_kallenbach=psep_kallenbach, teomp=teomp, neomp=neomp, &
        outfile=nout,iprint=0)
  else if(kallenbach_switch.eq.0) then

    ! Old Divertor Model ! Comment this out MDK 30/11/16
    call divcall(nout,0)

  end if


  ! Structure Model !
  ! !!!!!!!!!!!!!!!!!!

  call strucall(nout,0)

  ! Tight aspect ratio machine model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if ( itart == 1 .and. i_tf_sup /= 1 ) then
     call cntrpst(nout,0)
  end if

  ! Toroidal field coil power model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call tfpwr(nout,0)

  ! Poloidal field coil power model !
  call pfpwr(nout,0)

  ! Plant heat transport part 1 !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call power1

  ! Vacuum model !
  ! !!!!!!!!!!!!!!!

  call vaccall(nout,0)

  ! Buildings model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call bldgcall(nout,0)

  ! Plant AC power requirements !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call acpow(nout,0)

  ! Plant heat transport pt 2 & 3!
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call power2(nout,0)

  call power3(nout,0)

  ! Availability model !
  ! !!!!!!!!!!!!!!!!!!!!!

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

  ! Water usage in secondary cooling system !
  ! !!!!!!!!!!!!!!!!!!!!!

  call waterusecall(nout,0)

  ! Costs model !
  ! !!!!!!!!!!!!!!

  ! Cost switch values
  ! No.  |  model
  ! ---- | ------
  ! 0    |  1990 costs model
  ! 1    |  2015 Kovari model
  ! 2    |  2019 STEP model

  if (cost_model == 0) then
    call costs(nout,0)
  else if (cost_model == 1) then
    call costs_2015(0,0)
  else if (cost_model == 2) then
    call costs_step(nout,0)
  end if

  ! FISPACT and LOCA model (not used) !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  if (ifispact.eq.1) then
  !     call fispac(0)
  !     call loca(nout,0)
  !  end if

end subroutine caller
