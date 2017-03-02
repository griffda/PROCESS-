! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pulse_module

  !+ad_name  pulse_module
  !+ad_summ  Module containing pulsed reactor device routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  pulse
  !+ad_cont  thrmal
  !+ad_cont  tohswg
  !+ad_cont  burn
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of a pulsed fusion power plant.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  pf_power_variables
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  pulse_variables
  !+ad_call  times_variables
  !+ad_hist  05/11/12 PJK Initial version of module
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  29/07/14 PJK Added heat_transport_variables
  !+ad_hist  23/04/15 MDK Removed fhole
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use constraint_variables
  use current_drive_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables
  use pf_power_variables
  use pfcoil_variables
  use physics_variables
  use process_output
  use pulse_variables
  use times_variables

  implicit none

  private
  public :: pulse

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pulse(outfile,iprint)

    !+ad_name  pulse
    !+ad_summ  Caller for the pulsed reactor model
    !+ad_type  Subroutine
    !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This calls the routines relevant to a pulsed reactor scenario.
    !+ad_prob  None
    !+ad_call  thrmal
    !+ad_call  tohswg
    !+ad_call  burn
    !+ad_call  25/11/93 CAG/PJK Implementation within PROCESS
    !+ad_call  10/06/96 PJK Commented out call to STARTUP
    !+ad_hist  01/10/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  05/11/12 PJK Added pulse_variables
    !+ad_hist  27/06/13 PJK Comment change
    !+ad_hist  29/10/14 PJK Commented out call to thrmal
    !+ad_stat  Okay
    !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Thermal cycling package

    !call thrmal(outfile,iprint)

    !  Evaluate minimum plasma current ramp-up time

    call tohswg(outfile,iprint)

    !  Burn time calculation

    call burn(outfile,iprint)

  end subroutine pulse

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tohswg(outfile,iprint)

    !+ad_name  tohswg
    !+ad_summ  Routine to calculate the plasma current ramp-up time
    !+ad_type  Subroutine
    !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the plasma current ramp-up time
    !+ad_desc  for a pulsed reactor.
    !+ad_prob  None
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_hist  25/11/93 PJK Incorporation into PROCESS
    !+ad_hist  22/05/06 PJK Corrected error in tohsmn calculation
    !+ad_hist  01/10/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  29/10/12 PJK Added pf_power_variables
    !+ad_hist  31/10/12 PJK Added constraint_variables
    !+ad_hist  04/02/13 PJK Comment change
    !+ad_hist  11/06/13 PJK Modified ipdot and tohsmn equations
    !+ad_hist  27/06/13 PJK Modified output heading
    !+ad_hist  24/04/14 PJK Calculation always proceeds irrespective of iprint
    !+ad_hist  29/10/14 PJK Label changed from OH to CS
    !+ad_hist  27/11/15 MDK The whole of subroutine thrmal commented out.
    !+ad_stat  Okay
    !+ad_docs  Work File Note F/MPE/MOD/CAG/PROCESS/PULSE/0013
    !+ad_docs  Work File Note F/PL/PJK/PROCESS/CODE/050
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)) :: albusa,ioht1,ioht2,ipdot,loh,m,pfbusl,r,rho,v

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (lpulse /= 1) return

    !  Current/turn in OH coil at beginning of pulse (A/turn)

    ioht1 = cpt(nohc,2)

    !  Current/turn in OH coil at start of flat-top (A/turn)

    ioht2 = cpt(nohc,3)

    !  OH coil resistance (ohms)

    if (ipfres == 0) then
       r = 0.0D0
    else
       r = powohres/( 1.0D6*ric(nohc) )**2
    end if

    !  OH coil bus resistance (ohms) (assumed to include power supply)
    !  Bus parameters taken from routine PFPWR.

    pfbusl = 8.0D0 * rmajor + 140.0D0
    albusa = abs(cptdin(nohc))/100.0D0

    rho = 1.5D0 * 2.62D-4 * pfbusl/albusa

    !  OH coil power source emf (volts)

    v = vpfskv * 1.0D3

    !  Mutual inductance between OH coil and plasma (H)

    m = sxlg(nohc,ncirt)

    !  Self inductance of OH coil (H)

    loh = sxlg(nohc,nohc)

    !  Maximum rate of change of plasma current (A/s)
    !  - now a function of the plasma current itself (previously just 0.5D6)

    ipdot = 0.0455D0*plascur

    !  Minimum plasma current ramp-up time (s)
    !  - corrected (bus resistance is not a function of turns)

    tohsmn = loh*(ioht2 - ioht1) / &
         (ioht2*(r*turns(nohc) + rho) - v + m*ipdot)

    !  Output section

    if (iprint == 1) then
       if(active_constraints(41) .eqv. .true.)then
          call osubhd(outfile,'Central solenoid considerations:')
          call ovarre(outfile,'Minimum plasma current ramp-up time (s)', '(tohsmn)',tohsmn)
      end if
    end if

  end subroutine tohswg

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine burn(outfile,iprint)

    !+ad_name  burn
    !+ad_summ  Routine to calculate the burn time for a pulsed reactor
    !+ad_type  Subroutine
    !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the burn time for a pulsed reactor.
    !+ad_prob  None
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_call  report_error
    !+ad_hist  25/11/93 PJK Incorporation into PROCESS
    !+ad_hist  25/05/06 PJK Corrected error in tohsmn calculation
    !+ad_hist  01/10/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  17/12/12 PJK Modified burn volt-seconds calculation (RK)
    !+ad_hist  27/11/13 PJK Deducted theat from tburn
    !+ad_hist  24/04/14 PJK Calculation always proceeds irrespective of iprint
    !+ad_hist  19/05/14 PJK Added warning if tburn is negative
    !+ad_hist  19/05/14 PJK Changed abs(vsbn) to -vsbn; added error line
    !+ad_hist  16/06/14 PJK Removed duplicate outputs
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  Work File Note F/MPE/MOD/CAG/PROCESS/PULSE/0012
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)) :: tb,vburn,vsmax,vssoft

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (lpulse /= 1) return

    !  Volt-seconds required to produce plasma current during start-up
    !  (i.e. up to start of flat top)

    vssoft = vsres + vsind

    !  Total volt-seconds available during flat-top (heat + burn)
    !  (Previously calculated as (abs(vstot) - vssoft) )

    vsmax = -vsbn  !  vsbn is (or should be...) negative

    !  Loop voltage during flat-top (including MHD sawtooth enhancement)

    vburn = plascur * rplas * facoh * csawth

    !  Burn time (s)

    tb = vsmax/vburn - theat
    if (tb < 0.0D0) then
       fdiags(1) = tb ; fdiags(2) = vsmax ; fdiags(3) = vburn ; fdiags(4) = theat
       call report_error(93)
    end if
    tburn = max(0.0D0, tb)

    !  Output section

    if (iprint == 1) then

       call osubhd(outfile,'Volt-second considerations:')

       call ovarre(outfile,'Total V-s capability of OH/PF coils (Wb)', &
            '(abs(vstot))',abs(vstot))
       call ovarre(outfile,'Required volt-seconds during start-up (Wb)', &
            '(vssoft)',vssoft)
       call ovarre(outfile,'Available volt-seconds during burn (Wb)', &
            '(vsmax)',vsmax)

       if (tb <= 0.0D0) then
          call ocmmnt(outfile, &
               '   Error... burn time is zero; insufficient volt-seconds!')
       end if

    end if

  end subroutine burn

end module pulse_module
