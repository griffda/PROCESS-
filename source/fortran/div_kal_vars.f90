module div_kal_vars
    !! Previously divertor_kallenbach_variables
    !! Module containing global variables relating to the
    !! tokamak divertor components, Kallenbach model, issue #400
    !! This module contains global variables relating to tokamak
    !! divertor components.
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    use, intrinsic :: iso_fortran_env, only: dp=>real64
  
    implicit none
  
    public
  
    integer :: kallenbach_switch = 0
    !! kallenbach_switch /0/ : Switch to turn on the 1D Kallenbach divertor model (1=on, 0=off)
  
    integer :: kallenbach_tests = 0
    !! kallenbach_tests /0/ : Switch to run tests of 1D Kallenbach divertor model (1=on, 0=off)
  
    integer :: kallenbach_test_option = 0
    !! kallenbach_test_option /0/ : Switch to choose kallenbach test option: <UL>
    !!            <LI> = 0 Test case with user inputs;
    !!            <LI> = 1 Test case for Kallenbach paper;</UL>
  
    integer :: kallenbach_scan_switch = 0
    !! kallenbach_scan_switch /0/ : Switch to run scan of 1D Kallenbach divertor model (1=on, 0=off)
  
    integer :: kallenbach_scan_var = 0
    !! kallenbach_scan_var /0/ : Switch for parameter to scan for kallenbach scan test:<UL>
    !!                 <LI> = 0 ttarget
    !!                 <LI> = 1 qtargettotal
    !!                 <LI> = 2 targetangle
    !!                 <LI> = 3 lambda_q_omp
    !!                 <LI> = 4 netau_sol</UL>
  
    real(dp) :: kallenbach_scan_start = 2.0
    !! kallenbach_scan_start /2.0/ : Start value for kallenbach scan parameter
  
    real(dp) :: kallenbach_scan_end = 10.0
    !! kallenbach_scan_end /10.0/ : End value for kallenbach scan parameter
  
    integer :: kallenbach_scan_num = 1
    !! kallenbach_scan_num /1/ : Number of scans for kallenbach scan test
  
    real(dp) :: target_spread = 0.003D0
    !! target_spread /0.003/ : Increase in SOL power fall-off length due to spreading, mapped to OMP [m]
  
    real(dp) :: lambda_q_omp = 0.002D0
    !! lambda_q_omp /0.002/ : SOL power fall-off length at the outer midplane, perpendicular to field [m]
  
    real(dp) :: lcon_factor = 1.0D0
    !! lcon_factor /1.0/ : Correction factor for connection length from OMP to divertor =
    !!                     connection length/(pi*q*rmajor)
  
    real(dp) :: netau_sol = 0.5D0
    !! netau_sol /0.5/ : Parameter describing the departure from local ionisation equilibrium in the SOL. [ms.1e20/m3]
  
    real(dp) :: targetangle = 30.0D0
    !! targetangle /30.0/ : Angle between field-line and divertor target (degrees)
  
    real(dp) :: ttarget = 5.0D0
     !! ttarget /5.0/ : Plasma temperature adjacent to divertor sheath [eV] (iteration variable 120)
    !!                 Rem : 5 eV is the current limit for tungsten sputtering from argon impurity
  
    real(dp) :: qtargettotal = 5.0D6
    !! qtargettotal /5.0e6/ : Power density on target including surface recombination [W/m2]
    !!(iteration variable 124)
  
    ! real(dp) :: helium_enrichment = 1.0D0
    ! real(dp) :: impurity_enrichment = 5.0D0
  
    real(dp), dimension(14) :: impurity_enrichment = 5.0D0
    !! impurity_enrichment(14) /5.0/ : Ratio of each impurity concentration in SOL to confined plasma+
    !!the enrichment for Argon is also propagated for PLASMOD (ipedestal=3)
  
    real(dp) :: psep_kallenbach = 0.0D0
    !! psep_kallenbach : Power conducted through the separatrix, as calculated by the divertor model [W]
    !!                   Not equal to pdivt unless constraint is imposed.
  
    real(dp) :: teomp = 0.0D0
    !! teomp : separatrix temperature calculated by the Kallenbach divertor model [eV]
  
    ! Issue #457
    real(dp) :: neomp = 0.0D0
    !! neomp : Mean SOL density at OMP calculated by the Kallenbach divertor model [m-3]
  
    real(dp) :: neratio = 0.75D0
    !! neratio /0.75/ : Ratio of mean SOL density at OMP to separatrix density at OMP (iteration variable 121)
  
    real(dp) :: pressure0 = 0.0D0
    !! pressure0 : Total plasma pressure near target (thermal+dynamic) [Pa]
  
    real(dp) :: fractionwidesol = 0.1D0
    !! fractionwidesol /0.1/ : Distance from target at which SOL gets broader as a fraction of connection length
  
    real(dp), public :: fmom
    !! fmom : momentum factor [-]
  
    real(dp), public :: totalpowerlost
    !! totalpowerlost : Total power lost due to radiation, ionisation and recombination [W]
  
    real(dp), public :: impuritypowerlost
    !! impuritypowerlost : Power lost due to impurity radiation [W]
  
    real(dp), public :: hydrogenicpowerlost
    !! hydrogenicpowerlost : Power lost due to hydrogenic radiation [W]
  
    real(dp), public :: exchangepowerlost
    !! exchangepowerlost : Power lost due to charge exchange  [W]
  
    real(dp), public :: ionisationpowerlost
    !! ionisationpowerlost : Power lost due to electron impact ionisation [W]
  
    real(dp), public :: abserr_sol = 1.d-4
    !! abserr_sol : Absolute contribution to the error tolerance in the Kallenbach divertor model
  
    real(dp), public :: relerr_sol = 1.d-4
    !! relerr_sol : Relative contribution to the error tolerance in the Kallenbach divertor model
  
    real(dp), public :: mach0 = 0.999
    !! mach0 : Mach number at target (must be just less than 1)
  
  
  end module div_kal_vars