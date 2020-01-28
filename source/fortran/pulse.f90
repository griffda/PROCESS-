! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pulse_module

  !! Module containing pulsed reactor device routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! parameters of a pulsed fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  private
  public :: pulse

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pulse(outfile,iprint)

    !! Caller for the pulsed reactor model
    !! author: C A Gardner, AEA Fusion, Culham Laboratory
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This calls the routines relevant to a pulsed reactor scenario.
    !! Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
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

    !! Routine to calculate the plasma current ramp-up time
    !! author: C A Gardner, AEA Fusion, Culham Laboratory
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates the plasma current ramp-up time
    !! for a pulsed reactor.
    !! Work File Note F/MPE/MOD/CAG/PROCESS/PULSE/0013
    !! Work File Note F/PL/PJK/PROCESS/CODE/050
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constraint_variables, only: tohsmn
    use pf_power_variables, only: vpfskv
    use pfcoil_variables, only: ncirt, ipfres, nohc, powohres, sxlg, cpt, ric, &
      turns, cptdin
    use physics_variables, only: plascur, rmajor
    use process_output, only: active_constraints, ovarre, osubhd
    use pulse_variables, only: lpulse
    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)) :: albusa,ioht1,ioht2,ipdot,loh,m,pfbusl,r,rho,v

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (lpulse /= 1) return

    !  Current/turn in Central Solenoid at beginning of pulse (A/turn)

    ioht1 = cpt(nohc,2)

    !  Current/turn in Central Solenoid at start of flat-top (A/turn)

    ioht2 = cpt(nohc,3)

    !  Central Solenoid resistance (ohms)

    if (ipfres == 0) then
       r = 0.0D0
    else
       r = powohres/( 1.0D6*ric(nohc) )**2
    end if

    !  Central Solenoid bus resistance (ohms) (assumed to include power supply)
    !  Bus parameters taken from routine PFPWR.

    pfbusl = 8.0D0 * rmajor + 140.0D0
    albusa = abs(cptdin(nohc))/100.0D0

    rho = 1.5D0 * 2.62D-4 * pfbusl/albusa

    !  Central Solenoid power source emf (volts)

    v = vpfskv * 1.0D3

    !  Mutual inductance between Central Solenoid and plasma (H)

    m = sxlg(nohc,ncirt)

    !  Self inductance of Central Solenoid (H)

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

    !! Routine to calculate the burn time for a pulsed reactor
    !! author: C A Gardner, AEA Fusion, Culham Laboratory
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: R Kemp, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates the burn time for a pulsed reactor.
    !! Work File Note F/MPE/MOD/CAG/PROCESS/PULSE/0012
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use error_handling, only: fdiags, report_error
		use pfcoil_variables, only: vsbn, vstot
		use physics_variables, only: rplas, vsres, vsind, plascur, facoh, csawth
		use process_output, only: ocmmnt, ovarre, osubhd
		use pulse_variables, only: lpulse
		use times_variables, only: tburn, theat
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

       call ovarre(outfile,'Total V-s capability of Central Solenoid/PF coils (Wb)', &
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
