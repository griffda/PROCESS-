! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module availability_module
  !! Module containing plant availability routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! plant availability and component lifetimes for a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Modules to import
#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
  implicit none

  ! Module subroutine and variable declarations !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(dp), parameter :: year = 31557600.0D0
  !! seconds in a year [s]

  real(dp), parameter :: day = 86400.0D0
  !! seconds in a day [s]

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_fwbs(outfile, iprint, u_unplanned_fwbs)
    !! Calculates the unplanned unavailability of the first wall and blanket
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_fwbs : output real : unplanned unavailability of first wall and blanket
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use times_variables, only: tcycle
    use process_output, only: ocmmnt, ovarre, oblnkl
    use fwbs_variables, only: bktlife
    use cost_variables, only: fwbs_prob_fail, fwbs_umain_time, fwbs_nu, fwbs_nref

    implicit none

    ! Arguments
    integer, intent(in) :: outfile, iprint
    real(dp), intent(out) :: u_unplanned_fwbs

    ! Local Variables !
    ! !!!!!!!!!!!!!!!!!!

    real(dp) :: a0, fwbs_avail, n, pf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate cycle limit in terms of days

    ! Number of cycles between planned blanket replacements, N
    n = bktlife * year / tcycle

    ! The probability of failure in one pulse cycle
    ! (before the reference cycle life)
    pf = (fwbs_prob_fail / day) * tcycle
    a0 = 1.0D0 - pf * fwbs_umain_time * year / tcycle

    if (fwbs_nu <= fwbs_nref) then
      write(*,*) 'fwbs_nu <= fwbs_nref'
      write(*,*) 'The cycle when the blanket fails with 100% probability <= &
        &Reference value for cycle life of blanket'
      call ocmmnt(outfile,'EROROR: The cycle when the blanket fails with 100% probability&
        & <= Reference value for cycle life of blanket')
    end if

    ! Integrating the instantaneous availability gives the mean
    ! availability over the planned cycle life N
    if (n <= fwbs_nref) then
      fwbs_avail = a0
    else if (n >= fwbs_nu) then
      fwbs_avail = 0.0D0
    else
      fwbs_avail = (a0/(fwbs_nu-fwbs_nref))*(fwbs_nu - 0.5D0*fwbs_nref**2.0D0/n -0.5D0*n)
    end if

    ! First wall / blanket unplanned unavailability
    u_unplanned_fwbs = 1.0D0 - fwbs_avail

    ! Output
    if (iprint /= 1) return
    call ocmmnt(outfile,'First wall / Blanket:')
    call oblnkl(outfile)
    call ovarre(outfile,'Probability of failure per operational day', '(fwbs_prob_fail)', &
      fwbs_prob_fail)
    call ovarre(outfile,'Repair time (years)', '(fwbs_umain_time)',fwbs_umain_time)
    call ovarre(outfile,'Reference value for cycle life', '(fwbs_nref)',fwbs_nref)
    call ovarre(outfile,'The cycle when failure is 100% certain', '(fwbs_nu)',fwbs_nu)
    call ovarre(outfile,'Number of cycles between planned replacements', '(n)',n)
    call ovarre(outfile,'Unplanned unavailability', '(u_unplanned_fwbs)', u_unplanned_fwbs, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_fwbs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_bop(outfile, iprint, u_unplanned_bop)
    !! Calculates the unplanned unavailability of the balance of plant
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_bop : output real : unplanned unavailability of balance of plant
    !! This routine calculates the unplanned unavailability of the balance of plant,
    !! using the methodology outlined in the 2014 EUROfusion
    !! RAMI report.
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use process_output, only: ocmmnt, ovarre, oblnkl, ovarin
    use cost_variables, only: t_operation
    use constants, only : n_day_year

    implicit none

    ! Arguments
    integer, intent(in) :: outfile, iprint
    real(dp), intent(out) :: u_unplanned_bop

    ! Local variables !
    ! !!!!!!!!!!!!!!!!!!

    real(dp) :: bop_fail_rate, bop_mttr
    integer :: bop_num_failures

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Balance of plant failure rate (failures per hour)
    ! ENEA study WP13-DTM02-T01
    bop_fail_rate = 9.39D-5

    ! Number of balance of plant failures in plant operational lifetime
    bop_num_failures = nint(bop_fail_rate * n_day_year * 24.0D0 * t_operation)

    ! Balance of plant mean time to repair (years)
    ! ENEA study WP13-DTM02-T01
    bop_mttr = 96.0D0 / (24.0D0 * n_day_year)

    ! Unplanned downtime balance of plant
    u_unplanned_bop = (bop_mttr * bop_num_failures)/(t_operation)

    ! Output
    if (iprint /= 1) return
    call ocmmnt(outfile,'Balance of plant:')
    call oblnkl(outfile)
    call ovarre(outfile,'Failure rate (1/h)', '(bop_fail_rate)', bop_fail_rate)
    call ovarin(outfile,'Number of failures in lifetime', '(bop_num_failures)', &
      bop_num_failures, 'OP ')
    call ovarre(outfile,'Balance of plant MTTR', '(bop_mttr)', bop_mttr)
    call ovarre(outfile,'Balance of plant unplanned unavailability', '(u_unplanned_bop)', &
      u_unplanned_bop, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_bop

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_hcd(u_unplanned_hcd)
    !! Calculates the unplanned unavailability of the heating and current drive system
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_hcd : output real : unplanned unavailability of hcd
    !! This routine calculates the unplanned unavailability of the heating
    !! and current drive system, using the methodology outlined in the
    !! 2014 EUROfusion RAMI report.
    !! 2014 EUROfusion RAMI report, &quot;Availability in PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!

    real(dp), intent(out) :: u_unplanned_hcd

    ! Currently just a fixed value until more information available or Q.
    ! Tran's response provides useful data.
    u_unplanned_hcd = 0.02D0

  end subroutine calc_u_unplanned_hcd

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine calc_u_unplanned_vacuum(outfile, iprint, u_unplanned_vacuum)
    !! Calculates the unplanned unavailability of the vacuum system
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! u_unplanned_vacuum : output real : unplanned unavailability of vacuum system
    !! This routine calculates the unplanned unavailability of the vacuum system,
    !! using the methodology outlined in the 2014 EUROfusion
    !! RAMI report.
    !! 2014 EUROfusion RAMI report, &quot;Availability in
    ! PROCESS&quot;
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use vacuum_variables, only: vpumpn
    use maths_library, only: binomial
    use process_output, only: ocmmnt, ovarre, oblnkl, ovarin
    use cost_variables, only: redun_vac, tlife, t_operation, num_rh_systems
    use constants, only: n_day_year
    implicit none

    ! Arguments
    integer, intent(in) :: outfile, iprint
    real(dp), intent(out) :: u_unplanned_vacuum

    ! Local variables
    integer :: total_pumps, n
    real(dp) :: cryo_failure_rate, cryo_main_time
    real(dp) :: cryo_nfailure_rate, t_down
    real(dp) :: n_shutdown, t_op_bt, sum_prob

    real(dp), dimension(vpumpn + redun_vac + 1) :: vac_fail_p

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Number of shutdowns
    n_shutdown = anint((tlife - t_operation)/ ((21.0D0 * &
      num_rh_systems**(-0.9D0) + 2.0D0) / 12.0D0))

    ! Operational time between shutdowns
    t_op_bt = t_operation/(n_shutdown + 1.0D0)

    ! Cryopump maintenance time (y) = 2 months
    cryo_main_time = 1.0D0/6.0D0

    ! Total pumps = pumps + redundant pumps
    total_pumps = vpumpn + redun_vac

    ! Cryopump failure rate per machine operational period
    ! From "Selected component failure rate values from fusion
    ! safety assessment tasks", Cadwallader (1994)

    ! probability of pump failure per operational period
    cryo_failure_rate = 2.0D-6 * n_day_year * 24.0D0 * t_op_bt

    ! probability of no pump failure per operational period
    cryo_nfailure_rate = 1.0D0 - cryo_failure_rate

    sum_prob = 0.0D0
    do n = redun_vac + 1, total_pumps

      ! Probability for n failures in the operational period, n > number of redundant pumps
      vac_fail_p(n) = binomial(total_pumps,n) * (cryo_nfailure_rate**(total_pumps-n)) &
        *(cryo_failure_rate**n)

      ! calculate sum in formula for downtime
      sum_prob = sum_prob + vac_fail_p(n) * (n - redun_vac)

    end do

    ! Total down-time in reactor life
    t_down = (n_shutdown + 1.0D0) * cryo_main_time * sum_prob

    ! Total vacuum unplanned unavailability
    u_unplanned_vacuum = max(0.005, t_down / (t_operation + t_down))

    ! Output
    if (iprint /= 1) return
    call ocmmnt(outfile,'Vacuum:')
    call oblnkl(outfile)
    call ovarin(outfile,'Number of pumps (excluding redundant pumps)', '(vpumpn)', vpumpn, 'OP ')
    call ovarin(outfile,'Number of redundant pumps', '(redun_vac)', redun_vac, 'OP ')
    call ovarre(outfile,'Total unplanned down-time due to pumps, excl fixed 0.5% (years)', &
      '(t_down)', t_down, 'OP ')
    call ovarre(outfile,'Vacuum unplanned unavailability', '(u_unplanned_vacuum)', &
      u_unplanned_vacuum, 'OP ')
    call oblnkl(outfile)

  end subroutine calc_u_unplanned_vacuum

end module availability_module
