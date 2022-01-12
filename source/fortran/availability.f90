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
