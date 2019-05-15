module output_module
contains

subroutine output(outfile)

  !+ad_name  output
  !+ad_summ  Subroutine to write the results to the main output file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : Fortran output unit identifier
  !+ad_desc  This routine writes the program results to a file,
  !+ad_desc  in a tidy format.
  !+ad_prob  None
  !+ad_call  availability_module
  !+ad_call  build_module
  !+ad_call  buildings_module
  !+ad_call  costs_module
  !+ad_call  current_drive_module
  !+ad_call  divertor_module
  !+ad_call  fwbs_module


  !+ad_call  pfcoil_module
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  power_module
  !+ad_call  pulse_module


  !+ad_call  sctfcoil_module
  !+ad_call  startup_module
  !+ad_call  stellarator_module
  !+ad_call  stellarator_variables
  !+ad_call  structure_module
  !+ad_call  tfcoil_module
  !+ad_call  vaccum_module
  !+ad_call  acpow
  !+ad_call  avail
  !+ad_call  avail_2
  !+ad_call  bldgcall
  !+ad_call  cntrpst
  !+ad_call  costs
  !+ad_call  cudriv
  !+ad_call  divcall
  !+ad_call  fispac
  !+ad_call  fwbs
  !+ad_call  ifeout
  !+ad_call  igmarcal
  !+ad_call  induct
  !+ad_call  loca
  !+ad_call  outpf
  !+ad_call  outplas
  !+ad_call  outtim
  !+ad_call  outvolt
  !+ad_call  pfpwr
  !+ad_call  power2
  !+ad_call  pulse
  !+ad_call  radialb
  !+ad_call  startup
  !+ad_call  stout
  !+ad_call  strucall
  !+ad_call  tfcoil
  !+ad_call  tfpwr
  !+ad_call  tfspcall
  !+ad_call  vaccall
  !+ad_hist  23/01/97 PJK Initial upgraded version. Split routine POWER
  !+ad_hisc               into POWER1 and POWER2
  !+ad_hist  06/02/97 PJK Added routine LOCA
  !+ad_hist  21/03/97 PJK Added routine IFEOUT
  !+ad_hist  18/11/97 PJK Removed NOUT argument from FISPAC call
  !+ad_hist  19/05/99 PJK Added routine AVAIL
  !+ad_hist  20/09/11 PJK Initial F90 version
  !+ad_hist  24/09/12 PJK Swapped argument order of RADIALB, DIVCALL, INDUCT
  !+ad_hist  10/10/12 PJK Moved routine from output.f90 to aamain.f90
  !+ad_hist  19/06/14 PJK Removed obsolete calls to nbeam, ech, lwhymod
  !+ad_hist  09/07/14 PJK Turned on error handling
  !+ad_hist  07/06/16  JM Added some extra comments
  !+ad_hist  27/02/2018 KE Added plasmod output routine
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use availability_module
  use build_module
  use buildings_module
  use costs_module
  use costs_2015_module
  use cost_variables
  use current_drive_module
  use divertor_kallenbach_variables
  use divertor_ode 
  ! , only: divertor_kallenbach
  use divertor_module
  use error_handling
  use fwbs_module
  use fwbs_variables
  use pfcoil_module
  use physics_module
  use physics_variables
  use plasmod_module
  !use plasmod_variables
  use power_module
  use pulse_module
  use sctfcoil_module
  use startup_module
  use stellarator_module
  use stellarator_variables
  use structure_module
  use tfcoil_module
  use vacuum_module

  ! Import blanket modules
  use ccfe_hcpb_module
  use kit_hcpb_module
  use kit_hcll_module

  implicit none

  ! Arguments
  integer, intent(in) :: outfile

  ! Local variables
  logical :: verbose_logical

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Turn on error reporting
  ! (warnings etc. encountered in previous iterations may have cleared themselves
  ! during the solution process)
  errors_on = .true.

  !  Call stellarator output routine instead if relevant
  if (istell /= 0) then
     call stout(outfile)
     return
  end if

  ! Costs model !
  !!!!!!!!!!!!!!!

  ! Cost switch values
  ! No.  |  model
  ! ---- | ------
  ! 0    |  1990 costs model
  ! 1    |  2015 Kovari model

  if (cost_model == 1) then
     call costs_2015(outfile,1)
  else
     call costs(outfile,1)
  end if

  ! Availability model !
  !!!!!!!!!!!!!!!!!!!!!!

  ! Availability switch values
  ! No.  |  model
  ! ---- | ------
  ! 0    |  Input value for cfactr
  ! 1    |  Ward and Taylor model (1999)
  ! 2    |  Morris model (2015)

  if (iavail > 1) then
     call avail_2(outfile, 1)  ! Morris model (2015)
  else
     call avail(outfile,1)  ! Taylor and Ward model (1999)
  end if


  ! Writing the output from physics.f90 into OUT.DAT + MFILE.DAT
  call outplas(outfile)


  ! Writing
  if (ipedestal == 2 .or. ipedestal == 3) then
     call outputPlasmod(outfile)
  endif


  ! startup model (not used) !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !call startup(outfile,1)  !  commented-out for speed reasons

  ! TODO what is this? not in caller.f90
  call igmarcal(outfile)

  ! TODO what is this? Not in caller.f90?
  call cudriv(outfile,1)

  ! Pulsed reactor model !
  !!!!!!!!!!!!!!!!!!!!!!!!

  call pulse(outfile,1)


  call outtim(outfile)

  ! Divertor Model !
  !!!!!!!!!!!!!!!!!!
  if(verbose==1) then
    verbose_logical = .true.
  else
    verbose_logical = .false.
  endif

  call ovarin(mfile, 'kallenbach_switch','(kallenbach_switch)', kallenbach_switch)
  if(Kallenbach_switch.eq.1) then
    !  call divertor_Kallenbach(rmajor=rmajor,rminor=rminor, &
    !    bt=bt,plascur=plascur, bvert=bvert,q=q, &
    !    verboseset=.false.,  &
    !    Ttarget=Ttarget,qtargettotal=qtargettotal,            &
    !    targetangle=targetangle,lcon_factor=lcon_factor, netau_in=netau, &
    !    unit_test=.false.,abserrset=1.d-5,  &
    !    bp = bp,   &
    !    psep_kallenbach=psep_kallenbach, teomp=teomp, neomp=neomp, &
    !    outfile=nout,iprint=1 )
    call divertor_Kallenbach(rmajor=rmajor,rminor=rminor, &
        bt=bt,plascur=plascur,                                &
        q=q,                                      &
        verboseset=verbose_logical,                                   &
        Ttarget=Ttarget,qtargettotal=qtargettotal,            &
        targetangle=targetangle, &
        unit_test=.false.,     &
        bp = bp,   &
        psep_kallenbach=psep_kallenbach, teomp=teomp, neomp=neomp, &
        outfile=nout,iprint=1)

  else
    ! Old Divertor Model ! Comment this out MDK 30/11/16
    call divcall(outfile,1)

  end if

  ! Machine Build Model !
  !!!!!!!!!!!!!!!!!!!!!!!

  ! Radial build
  call radialb(outfile,1)

  ! Vertical build
  call vbuild(outfile,1)

  ! Toroidal field coil model !
  call tfcoil(outfile,1)

  ! Toroidal field coil superconductor model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if ( itfsup /= 1 ) then
     call tfspcall(outfile,1)
  end if


  ! Tight aspect ratio machine model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (itart == 1) then
    call cntrpst(outfile,1)
  end if

  ! Poloidal field coil model !
  call outpf(outfile)

  ! TODO what is outvolt?
  call outvolt(outfile)

  ! Structure Model !
  !!!!!!!!!!!!!!!!!!!

  call strucall(outfile,1)

  ! Poloidal field coil inductance calculation
  call induct(outfile,1)

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
      call ccfe_hcpb(nout, 1)
  else if (iblanket == 2) then      ! KIT HCPB model
     call kit_hcpb(nout, 1)
  else if (iblanket == 3) then      ! CCFE HCPB model with Tritium Breeding Ratio calculation
     call ccfe_hcpb(nout, 1)
     call tbr_shimwell(nout, 1, breeder_f, li6enrich, iblanket_thickness, tbr)
  else if (iblanket == 4) then      ! KIT HCLL model
     call kit_hcll(nout, 1)
  end if

  ! FISPACT and LOCA model (not used) !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! if (ifispact == 1) then
  !   call fispac(0)
  !   call fispac(1)
  !   call loca(outfile,0)
  !   call loca(outfile,1)
  !end if

  ! Toroidal field coil power model !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call tfpwr(outfile,1)

  ! Poloidal field coil power model !
  call pfpwr(outfile,1)

  ! Vacuum model !
  !!!!!!!!!!!!!!!!

  call vaccall(outfile,1)

  ! Buildings model (1990 costs model only) !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (cost_model==0) then
    call bldgcall(outfile,1)
  end if

  ! Plant AC power requirements !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call acpow(outfile,1)

  ! Plant heat transport pt 2 & 3 !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call power2(outfile,1)

  call power3(nout,1)

end subroutine output

end module output_module
