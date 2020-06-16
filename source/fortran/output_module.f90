module output_module

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

contains

subroutine output(outfile)

  !! Subroutine to write the results to the main output file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! outfile : input integer : Fortran output unit identifier
  !! This routine writes the program results to a file,
  !! in a tidy format.


  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use availability_module, only: avail_2, avail
  use build_module, only: radialb, vbuild
  use buildings_module, only: bldgcall
  use costs_module, only: costs
  use costs_2015_module, only: costs_2015
  use costs_step_module, only: costs_step
  use cost_variables, only: iavail, cost_model
  use current_drive_module, only: cudriv
  use divertor_ode, only: divertor_Kallenbach
  use divertor_module, only: divcall
  use error_handling, only: errors_on
  use fwbs_variables, only: tbr, iblanket, li6enrich, iblanket_thickness, &
    breeder_f
  use ife_module, only: ifeout
  use ife_variables, only: ife
  use pfcoil_module, only: outpf, outvolt, induct
  use physics_module, only: igmarcal, outtim, outplas
  use physics_variables, only: ipedestal, q, plascur, itart, rminor, bp, &
    rmajor, bt
  use plasmod_module, only: outputplasmod
  use power_module, only: pfpwr, acpow, power2, tfpwr, power3
  use pulse_module, only: pulse
  use sctfcoil_module, only: tfspcall
  use stellarator_module, only: stout
  use stellarator_variables, only: istell
  use structure_module, only: strucall
  use tfcoil_module, only: tfcoil, cntrpst
  use vacuum_module, only: vaccall
  use ccfe_hcpb_module, only: tbr_shimwell, ccfe_hcpb
  use kit_hcpb_module, only: kit_hcpb
  use kit_hcll_module, only: kit_hcll
  use tfcoil_variables, only: i_tf_sup
  use global_variables, only: verbose
  use process_output, only: ovarin
  use constants, only: mfile, nout
  use div_kal_vars, only: kallenbach_switch, neomp, &
    ttarget, targetangle, psep_kallenbach, qtargettotal, teomp
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

  !  Call IFE output routine instead if relevant
  if (ife /= 0) then
   call ifeout(outfile)
   return
  end if

  ! Costs model !
  ! !!!!!!!!!!!!!!

  ! Cost switch values
  ! No.  |  model
  ! ---- | ------
  ! 0    |  1990 costs model
  ! 1    |  2015 Kovari model
  ! 2    |  2019 STEP model

  if (cost_model == 0) then
      call costs(outfile,1)
  else if (cost_model == 1) then
     call costs_2015(outfile,1)
  else if (cost_model == 2) then
     call costs_step(outfile,1)
  end if

  ! Availability model !
  ! !!!!!!!!!!!!!!!!!!!!!

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
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!

  !call startup(outfile,1)  !  commented-out for speed reasons

  ! TODO what is this? not in caller.f90
  call igmarcal(outfile)

  ! TODO what is this? Not in caller.f90?
  call cudriv(outfile,1)

  ! Pulsed reactor model !
  ! !!!!!!!!!!!!!!!!!!!!!!!

  call pulse(outfile,1)


  call outtim(outfile)

  ! Divertor Model !
  ! !!!!!!!!!!!!!!!!!
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
  ! !!!!!!!!!!!!!!!!!!!!!!

  ! Radial build
  call radialb(outfile,1)

  ! Vertical build
  call vbuild(outfile,1)

  ! Toroidal field coil model !
  call tfcoil(outfile,1)

  ! Toroidal field coil superconductor model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if ( i_tf_sup == 1 ) then
     call tfspcall(outfile,1)
  end if


  ! Tight aspect ratio machine model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if (itart == 1) then
    call cntrpst(outfile,1)
  end if

  ! Poloidal field coil model !
  call outpf(outfile)

  ! TODO what is outvolt?
  call outvolt(outfile)

  ! Structure Model !
  ! !!!!!!!!!!!!!!!!!!
  call strucall(outfile,1)

  ! Poloidal field coil inductance calculation
  call induct(outfile,1)

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
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! if (ifispact == 1) then
  !   call fispac(0)
  !   call fispac(1)
  !   call loca(outfile,0)
  !   call loca(outfile,1)
  !end if

  ! Toroidal field coil power model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call tfpwr(outfile,1)

  ! Poloidal field coil power model !
  call pfpwr(outfile,1)

  ! Vacuum model !
  ! !!!!!!!!!!!!!!!
  call vaccall(outfile,1)

  ! Buildings model !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call bldgcall(outfile,1)

  ! Plant AC power requirements !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call acpow(outfile,1)

  ! Plant heat transport pt 2 & 3 !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  call power2(outfile,1)
  call power3(nout,1)

end subroutine output

end module output_module
