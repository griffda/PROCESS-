module div_kal_vars
  !! author: J. Morri (UKAEA), M. Kovari (UKAEA)
  !!
  !! Previously divertor_kallenbach_variables
  !! Module containing global variables relating to the tokamak divertor components,
  !! Kallenbach model, issue #400.
  !!
  !!### References
  !!
  !! - Johner, Fusion Science and Technology 59 (2011), pp 308-349
  !! - Sertoli, private communication
  !! - Kallenbach et al., Plasma Phys. Control. Fus. 55(2013) 124041

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  integer :: kallenbach_switch
  !! switch to turn on the 1D Kallenbach divertor model:
  !!
  !! - =1 on
  !! - =0 off

  integer :: kallenbach_tests
  !! switch to run tests of 1D Kallenbach divertor model:
  !!
  !! - =1 on
  !! - =0 off

  integer :: kallenbach_test_option
  !! switch to choose kallenbach test option:
  !!
  !! - =0 Test case with user inputs
  !! - =1 Test case for Kallenbach paper

  integer :: kallenbach_scan_switch
  !! switch to run scan of 1D Kallenbach divertor model:
  !!
  !! - =1 on
  !! - =0 off

  integer :: kallenbach_scan_var
  !! switch for parameter to scan for kallenbach scan test:
  !!
  !! - =0 ttarget
  !! - =1 qtargettotal
  !! - =2 targetangle
  !! - =3 lambda_q_omp
  !! - =4 netau_sol

  real(8) :: kallenbach_scan_start
  !! start value for kallenbach scan parameter

  real(8) :: kallenbach_scan_end
  !! end value for kallenbach scan parameter

  integer :: kallenbach_scan_num
  !! number of scans for kallenbach scan test

  real(8) :: target_spread
  !! increase in SOL power fall-off length due to spreading, mapped to OMP [m]

  real(8) :: lambda_q_omp
  !! SOL power fall-off length at the outer midplane, perpendicular to field [m]

  real(8) :: lcon_factor
  !! Correction factor for connection length from OMP to divertor = connection length/(pi*q*rmajor)

  real(8) :: netau_sol
  !! Parameter describing the departure from local ionisation equilibrium in the SOL. [ms.1e20/m3]

  real(8) :: targetangle
  !! Angle between field-line and divertor target (degrees)

  real(8) :: ttarget
  !! Plasma temperature adjacent to divertor sheath [eV] (`iteration variable 120`)
  !! Rem : 5 eV is the current limit for tungsten sputtering from argon impurity

  real(8) :: qtargettotal
  !! Power density on target including surface recombination [W/m2] (`iteration variable 124`)

  real(8), dimension(14) :: impurity_enrichment
  !! Ratio of each impurity concentration in SOL to confined plasma + the enrichment for Argon
  !! is also propagated for PLASMOD (`ipedestal=3`)

  real(8) :: psep_kallenbach
  !! Power conducted through the separatrix, as calculated by the divertor model [W]
  !! Not equal to pdivt unless `constraint 69` is imposed.

  real(8) :: teomp
  !! separatrix temperature calculated by the Kallenbach divertor model [eV] (issue #457)

  real(8) :: neomp
  !! Mean SOL density at OMP calculated by the Kallenbach divertor model [m-3]

  real(8) :: neratio
  !! Ratio of mean SOL density at OMP to separatrix density at OMP (`iteration variable 121`)

  real(8) :: pressure0
  !! Total plasma pressure near target (thermal+dynamic) [Pa]

  real(8) :: fractionwidesol
  !! Distance from target at which SOL gets broader as a fraction of connection length

  real(8), public :: fmom
  !! momentum factor [-]

  real(8), public :: totalpowerlost
  !! Total power lost due to radiation, ionisation and recombination [W]

  real(8), public :: impuritypowerlost
  !! Power lost due to impurity radiation [W]

  real(8), public :: hydrogenicpowerlost
  !! Power lost due to hydrogenic radiation [W]

  real(8), public :: exchangepowerlost
  !! Power lost due to charge exchange  [W]

  real(8), public :: ionisationpowerlost
  !! Power lost due to electron impact ionisation [W]

  real(8), public :: abserr_sol
  !! Absolute contribution to the error tolerance in the Kallenbach divertor model

  real(8), public :: relerr_sol
  !! Relative contribution to the error tolerance in the Kallenbach divertor model

  real(8), public :: mach0
  !! Mach number at target (must be just less than 1)

  contains

  subroutine init_div_kal_vars
    !! Initialise module variables
    implicit none

    kallenbach_switch = 0
    kallenbach_tests = 0
    kallenbach_test_option = 0
    kallenbach_scan_switch = 0
    kallenbach_scan_var = 0
    kallenbach_scan_start = 2.0
    kallenbach_scan_end = 10.0
    kallenbach_scan_num = 1
    target_spread = 0.003D0
    lambda_q_omp = 0.002D0
    lcon_factor = 1.0D0
    netau_sol = 0.5D0
    targetangle = 30.0D0
    ttarget = 5.0D0
    qtargettotal = 5.0D6
    impurity_enrichment = 5.0D0
    psep_kallenbach = 0.0D0
    teomp = 0.0D0
    neomp = 0.0D0
    neratio = 0.75D0
    pressure0 = 0.0D0
    fractionwidesol = 0.1D0
    abserr_sol = 1.d-4
    relerr_sol = 1.d-4
    mach0 = 0.999
  end subroutine init_div_kal_vars
end module div_kal_vars