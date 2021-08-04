module primary_pumping_variables
  !! author: J. Morris, M. Kovari (UKAEA)
  !!
  !! Module containing global variables relating to `the primary_pumping=3` option.
  !! (Mechanical pumping power is calculated using specified pressure drop)
  !!
  !!### References
  !!
  !! - issue #503

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp), parameter :: gamma_he = 1.667D0
  !! ratio of specific heats for helium (`primary_pumping=3`)

  real(dp), parameter :: t_in_bb =573.13D0
  !! temperature in FW and blanket coolant at blanket entrance (`primary_pumping=3`) [K]

  real(dp), parameter :: t_out_bb =773.13D0
  !! temperature in FW and blanket coolant at blanket exit (`primary_pumping=3`) [K]

  real(dp), parameter :: p_he =8.0D6
  !! pressure in FW and blanket coolant at pump exit (`primary_pumping=3`) [Pa]

  real(dp), parameter :: dp_he =5.5D5
  !! pressure drop in FW and blanket coolant including heat exchanger and pipes (`primary_pumping=3`) [Pa]

  real(dp) :: htpmw_fw_blkt
  !! mechanical pumping power for FW and blanket including heat exchanger and 
  !! pipes (`primary_pumping=3`) [MW]

  contains

  subroutine init_primary_pumping_variables
    !! Initialise module variables
    implicit none

    htpmw_fw_blkt = 0.0d0
  end subroutine init_primary_pumping_variables
end module primary_pumping_variables