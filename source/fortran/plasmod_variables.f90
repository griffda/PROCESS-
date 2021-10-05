module plasmod_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to PLASMOD. PLASMOD is a 1-D plasma transport 
  !! solver written by E. Fable (IPP Garching).
  !!
  !!### References
  !!
  !! - E. Fable et al., Fusion Engineering and Design, Volume 130, May 2018, Pages 131-136
  
  use structs, only: geometry, pedestal, inputs, radial_profiles, &
    power_losses, numerics_transp, MHD_EQ, composition
#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
  implicit none

  public

  real(dp) :: plasmod_tol
  !! tolerance to be reached at each time step (%)

  real(dp) :: plasmod_dtmin
  !! plasmod min time step

  real(dp) :: plasmod_dtmax
  !! plasmod max time step
  
  real(dp) :: plasmod_dt
  !! plasmod time step

  real(dp) :: plasmod_dtinc
  !! decrease of dt

  real(dp) :: plasmod_Ainc
  !! increase of dt

  real(dp) :: plasmod_test
  !! max number of plasmod iterations

  real(dp) :: plasmod_tolmin
  !! multiplier of etolm which can not be exceeded

  real(dp) :: plasmod_eopt
  !! exponent of jipperdo 
  !#TODO: what?

  real(dp) :: plasmod_dtmaxmin
  !! exponent of jipperdo2 
  !#TODO: likewise?

  real(dp) :: plasmod_dtmaxmax
  !! stabilizing coefficient

  real(dp) :: plasmod_capA
  !! first radial grid point

  real(dp) :: plasmod_maxA
  !! diagz 0 or 1

  real(dp) :: plasmod_dgy
  !! Newton differential

  integer :: plasmod_iprocess
  !! switch for determining which functions to use:
  !!
  !! - =0 use PLASMOD functions
  !! - =1 use PROCESS functions
  
  integer :: plasmod_i_modeltype
  !! switch for the transport model:
  !!
  !! - =1 Simple gyrobohm scaling with imposed H factor > 1. Other values give H factor as output
  !! - =111 roughly calibrated to give H=1 for DEMO, but not fixed H 
  !#TODO: why 111?

  integer :: plasmod_i_equiltype
  !! switch for EMEQ setting to use either q95 or Ip as input:
  !!
  !! - =1 EMEQ, solve with sawteeth and inputted q95.
  !! - =2 EMEQ, solve with sawteeth and inputted Ip (not recommended!).

  integer :: plasmod_isawt
  !! switch to determine if plasmod solves with sawteeth:
  !!
  !! - =0 no sawteeth
  !! - =1 solve with sawteeth

  integer :: plasmod_nx
  !! number of interpolated grid points

  integer :: plasmod_nxt
  !! number of solved grid points

  integer :: plasmod_nchannels
  !! leave this at 3
  !#TODO: change to parameter?

  integer :: plasmod_i_impmodel
  !! switch for plasma inpurity concentration setting:
  !!
  !! - =0 fixed concentration
  !! - =1 fixed concentration at pedestal top, then fixed density

  real(dp), dimension(5) :: plasmod_globtau
  !! tauparticle/tauE for D, T, He, Xe, Ar (**not used for Xe**)

  real(dp) :: plasmod_psepplh_sup
  !! Psep/PLH if above this, use Xe

  real(dp) :: plasmod_qdivt
  !! divertor heat flux in MW/m^2, if 0, dont use SOL model

  integer, dimension(3) :: plasmod_imptype
  !! Impurities array:
  !!
  !! - [1] - intrinsic impurity
  !! - [2] - Psep control
  !! - [3] - seeding for SOL (defaults: W, Xe, Ar)

  real(dp) :: plasmod_qnbi_psepfac
  !! dqnbi/d(1-Psep/PLH)

  real(dp) :: plasmod_cxe_psepfac
  !! dcxe/d(1-Psep/PLH)

  real(dp) :: plasmod_car_qdivt
  !! dcar/d(qdivt)

  real(dp) :: plasmod_maxpauxor
  !! max allowed auxiliary power / R
  
  real(dp), dimension(2) :: plasmod_x_heat
  !! plasmod auxiliary heating array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_x_cd
  !! plasmod auxiliary current drive array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_x_fus
  !! plasmod fusion power array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_x_control
  !! plasmod control array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_dx_heat
  !! plasmod change in auxiliary heating array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_dx_cd
  !! plasmod change in auxiliary current drive array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_dx_fus
  !! plasmod change in fusion power array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_dx_control
  !! plasmod change in control array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp) :: plasmod_contrpovs
  !! control power in Paux/lateral_area (MW/m2)

  real(dp) :: plasmod_contrpovr
  !! control power in Paux/R (MW/m)

  real(dp) :: plasmod_nbi_energy
  !! NBI energy [keV]

  real(dp) :: plasmod_v_loop
  !! target loop voltage. If lower than -1.e5 do not use.

  real(dp) :: plasmod_pfus
  !! plasmod fusion power. If 0. not used (otherwise controlled with Pauxheat).

  real(dp) :: plasmod_eccdeff
  !! current drive multiplier: CD = eccdeff*PCD*TE/NE (not in use yet)

  real(dp) :: plasmod_fcdp
  !! (P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power (`iteration variable 150`)

  real(dp) :: plasmod_fradc
  !! Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad) (`iteration variable 151`)

  real(dp) :: plasmod_pech
  !! ech power (not in use yet) #TODO: units?

  real(dp) :: plasmod_gamcdothers
  !! efficiency multiplier for non-CD heating. If 0.0 pheat treated as if had no CD associated

  real(dp) :: plasmod_chisawpos
  !! position where artificial sawtooth diffusivity is added, -1 - uses q=1 position

  real(dp) :: plasmod_chisaw
  !! artificial diffusivity in m^2/s

  real(dp) :: plasmod_sawpertau
  !! ratio between sawtooth period and confinement time

  real(dp) :: plasmod_spellet
  !! pellet mass in units of D in 10^19

  real(dp) :: plasmod_fpellet
  !! pellet frequency [Hz]

  real(dp) :: plasmod_pedscal
  !! multiplication factor of the pedestal scaling in PLASMOD can be used to scan the pedestal height.

  type (geometry) :: geom
  !! Derived type containing all geometry information for PLASMOD

  type (composition) :: comp
  !! Derived type containing all composition information for PLASMOD

  type (pedestal) :: ped
  !! Derived type containing all pedestal information for PLASMOD

  type (inputs) :: inp0
  !! Derived type containing miscellaneous input information for PLASMOD

  type (radial_profiles) :: radp
  !! Derived type containing all radial profile information for PLASMOD

  type (MHD_EQ) :: mhd
  !! Derived type containing all mhd information for PLASMOD

  type (power_losses) :: loss
  !! Derived type containing all power loss information for PLASMOD

  type (numerics_transp) :: num
  !! Derived type containing all numerics information for PLASMOD

  integer :: i_flag
  !! Error flag for PLASMOD

  contains

  subroutine init_plasmod_variables
    !! Initialise module variables
    implicit none
  
    plasmod_tol = 1.0d-10
    plasmod_dtmin = 0.05d0
    plasmod_dtmax = 0.1d0
    plasmod_dt = 0.01d0
    plasmod_dtinc = 2.0d0
    plasmod_Ainc = 1.1d0
    plasmod_test = 1000000.0d0
    plasmod_tolmin = 10.1d0
    plasmod_eopt = 0.15d0
    plasmod_dtmaxmin = 0.15d0
    plasmod_dtmaxmax = 0.0d0
    plasmod_capA = 0.1d0
    plasmod_maxA = 0.0d0
    plasmod_dgy = 1.0d-5
    plasmod_iprocess = 1
    plasmod_isawt = 1
    plasmod_nx = 41
    plasmod_nxt = 7
    plasmod_nchannels = 3
    plasmod_i_impmodel = 1
    plasmod_globtau = (/ 5.0d0, 5.0d0, 7.0d0, 5.0d0, 1.0d0 /)
    plasmod_psepplh_sup = 12000.0d0
    plasmod_qdivt = 0.0d0
    plasmod_qnbi_psepfac = 50.0d0
    plasmod_cxe_psepfac = 1.0d-4
    plasmod_car_qdivt = 1.0d-4
    plasmod_maxpauxor = 20.0d0
    plasmod_x_heat = (/ 0.0d0, 0.0d0 /)
    plasmod_x_cd = (/ 0.0d0, 0.0d0 /)
    plasmod_x_fus = (/ 0.0d0, 0.0d0 /)
    plasmod_x_control = (/ 0.0d0, 0.0d0 /)
    plasmod_dx_heat = (/ 0.2d0, 0.03d0 /)
    plasmod_dx_cd = (/ 0.2d0, 0.03d0 /)
    plasmod_dx_fus = (/ 0.2d0, 0.03d0 /)
    plasmod_dx_control = (/ 0.2d0, 0.03d0 /)
    plasmod_contrpovs = 0.0d0
    plasmod_contrpovr = 0.0d0
    plasmod_nbi_energy = 1000.0d0
    plasmod_v_loop = -1.0d-6
    plasmod_pfus = 0.0d0
    plasmod_eccdeff = 0.3d0
    plasmod_fcdp = -1.0d0
    plasmod_fradc = -1.0d0
    plasmod_pech = 0.0d0
    plasmod_gamcdothers = 1.0d0
    plasmod_chisawpos = -1.0d0
    plasmod_chisaw = 0.0d0
    plasmod_sawpertau = 1.0d-6
    plasmod_spellet = 0.0d0
    plasmod_fpellet = 0.5d0
    plasmod_pedscal = 1.0d0
    plasmod_imptype = (/ 14, 13, 9 /)
    plasmod_i_equiltype = 1
    plasmod_i_modeltype = 1
  end subroutine init_plasmod_variables
end module plasmod_variables