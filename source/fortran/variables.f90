module constants
  !! author: J. Morris (UAKEA)
  !!
  !! Module containing miscellaneous numerical and physical constants
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  ! File output indexes
  integer, parameter :: iotty    = 6
  !! Standard output unit identifier

  integer, parameter :: nout     = 11 
  !! Output file unit identifier

  integer, parameter :: nplot    = 12 
  !! Plot data file unit identifier

  integer, parameter :: mfile    = 13
  !! Machine-optimised output file unit

  integer, parameter :: vfile    = 14
  !! Verbose diagnostics file

  integer, parameter :: opt_file = 15 
  !! Optimisation information output file number

  integer, parameter :: sig_file = 16 
  !! TF inboard stress radial distributions file number

  real(dp), parameter :: degrad = 0.01745329251D0
  !! degrees to radians, = pi/180

  real(dp), parameter :: echarge = 1.60217733D-19
  !! electron charge [C]

  real(dp), parameter :: mproton = 1.6726231D-27
  !! proton mass [kg]

  real(dp), parameter :: pi = 3.1415926535897932D0
  !! pi

  real(dp), parameter :: rmu0 = 1.256637062D-6
  !! permeability of free space  [H/m]

  real(dp), parameter :: twopi = 6.2831853071795862D0
  !! 2 pi

  real(dp), parameter :: umass = 1.660538921D-27
  !! unified atomic mass unit [kg

  real(dp), parameter :: epsilon0 = 8.85418781D-12
  !! permittivity of free space [Farad/m]

  real(dp), parameter :: cph2o = 4180.0D0
  !! specific heat capacity of water (J/kg/K)

  real(dp) :: dcopper = 8900.0D0
  !! density of copper (kg/m3)

  real(dp) :: dalu = 2700.0D0
  !! density of aluminium (kg/m3)

  real(dp), parameter :: denh2o = 985.0D0
  !! density of water (kg/m3)

  real(dp), parameter :: k_copper = 330.0D0
  !! Copper thermal conductivity (W/m/K)

  real(dp), parameter :: kh2o = 0.651D0
  !! thermal conductivity of water (W/m/K)

  real(dp), parameter :: muh2o = 4.71D-4
  !! water dynamic viscosity (kg/m/s)

end module constants

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
  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  public

  real(dp) :: plasmod_tol = 1.0d-10
  !! tolerance to be reached at each time step (%)

  real(dp) :: plasmod_dtmin = 0.05d0
  !! plasmod min time step

  real(dp) :: plasmod_dtmax = 0.1d0
  !! plasmod max time step
  
  real(dp) :: plasmod_dt = 0.01d0
  !! plasmod time step

  real(dp) :: plasmod_dtinc = 2.0d0
  !! decrease of dt

  real(dp) :: plasmod_Ainc = 1.1d0
  !! increase of dt

  real(dp) :: plasmod_test = 1000000.0d0
  !! max number of plasmod iterations

  real(dp) :: plasmod_tolmin = 10.1d0
  !! multiplier of etolm which can not be exceeded

  real(dp) :: plasmod_eopt = 0.15d0
  !! exponent of jipperdo 
  !#TODO: what?

  real(dp) :: plasmod_dtmaxmin = 0.15d0
  !! exponent of jipperdo2 
  !#TODO: likewise?

  real(dp) :: plasmod_dtmaxmax = 0.0d0
  !! stabilizing coefficient

  real(dp) :: plasmod_capA = 0.1d0
  !! first radial grid point

  real(dp) :: plasmod_maxA = 0.0d0
  !! diagz 0 or 1

  real(dp) :: plasmod_dgy = 1.0d-5
  !! Newton differential

  integer :: plasmod_iprocess = 1
  !! switch for determining which functions to use:
  !!
  !! - =0 use PLASMOD functions
  !! - =1 use PROCESS functions
  
  integer :: plasmod_i_modeltype = 1
  !! switch for the transport model:
  !!
  !! - =1 Simple gyrobohm scaling with imposed H factor > 1. Other values give H factor as output
  !! - =111 roughly calibrated to give H=1 for DEMO, but not fixed H 
  !#TODO: why 111?

  integer :: plasmod_i_equiltype = 1
  !! switch for EMEQ setting to use either q95 or Ip as input:
  !!
  !! - =1 EMEQ, solve with sawteeth and inputted q95.
  !! - =2 EMEQ, solve with sawteeth and inputted Ip (not recommended!).

  integer :: plasmod_isawt = 1
  !! switch to determine if plasmod solves with sawteeth:
  !!
  !! - =0 no sawteeth
  !! - =1 solve with sawteeth

  integer :: plasmod_nx = 41
  !! number of interpolated grid points

  integer :: plasmod_nxt = 7
  !! number of solved grid points

  integer :: plasmod_nchannels = 3
  !! leave this at 3
  !#TODO: change to parameter?

  integer :: plasmod_i_impmodel = 1
  !! switch for plasma inpurity concentration setting:
  !!
  !! - =0 fixed concentration
  !! - =1 fixed concentration at pedestal top, then fixed density

  real(dp), dimension(5) :: plasmod_globtau = (/ 5.0d0, 5.0d0, 7.0d0, 5.0d0, 1.0d0 /)
  !! tauparticle/tauE for D, T, He, Xe, Ar (**not used for Xe**)

  real(dp) :: plasmod_psepplh_sup = 12000.0d0
  !! Psep/PLH if above this, use Xe

  real(dp) :: plasmod_qdivt = 0.0d0
  !! divertor heat flux in MW/m^2, if 0, dont use SOL model

  integer, dimension(3) :: plasmod_imptype = (/ 14, 13, 9 /)
  !! Impurities array:
  !!
  !! - [1] - intrinsic impurity
  !! - [2] - Psep control
  !! - [3] - seeding for SOL (defaults: W, Xe, Ar)

  real(dp) :: plasmod_qnbi_psepfac = 50.0d0
  !! dqnbi/d(1-Psep/PLH)

  real(dp) :: plasmod_cxe_psepfac = 1.0d-4
  !! dcxe/d(1-Psep/PLH)

  real(dp) :: plasmod_car_qdivt = 1.0d-4
  !! dcar/d(qdivt)

  real(dp) :: plasmod_maxpauxor = 20.0d0
  !! max allowed auxiliary power / R
  
  real(dp), dimension(2) :: plasmod_x_heat = (/ 0.0d0, 0.0d0 /)
  !! plasmod auxiliary heating array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_x_cd = (/ 0.0d0, 0.0d0 /)
  !! plasmod auxiliary current drive array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_x_fus = (/ 0.0d0, 0.0d0 /)
  !! plasmod fusion power array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_x_control = (/ 0.0d0, 0.0d0 /)
  !! plasmod control array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_dx_heat = (/ 0.2d0, 0.03d0 /)
  !! plasmod change in auxiliary heating array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_dx_cd = (/ 0.2d0, 0.03d0 /)
  !! plasmod change in auxiliary current drive array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_dx_fus = (/ 0.2d0, 0.03d0 /)
  !! plasmod change in fusion power array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp), dimension(2) :: plasmod_dx_control = (/ 0.2d0, 0.03d0 /)
  !! plasmod change in control array:
  !!
  !! - [1] - nbi
  !! - [2] - ech

  real(dp) :: plasmod_contrpovs = 0.0d0
  !! control power in Paux/lateral_area (MW/m2)

  real(dp) :: plasmod_contrpovr = 0.0d0
  !! control power in Paux/R (MW/m)

  real(dp) :: plasmod_nbi_energy = 1000.0d0
  !! NBI energy [keV]

  real(dp) :: plasmod_v_loop = -1.0d-6
  !! target loop voltage. If lower than -1.e5 do not use.

  real(dp) :: plasmod_pfus = 0.0d0
  !! plasmod fusion power. If 0. not used (otherwise controlled with Pauxheat).

  real(dp) :: plasmod_eccdeff = 0.3d0
  !! current drive multiplier: CD = eccdeff*PCD*TE/NE (not in use yet)

  real(dp) :: plasmod_fcdp = -1.0d0
  !! (P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power (`iteration variable 150`)

  real(dp) :: plasmod_fradc = -1.0d0
  !! Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad) (`iteration variable 151`)

  real(dp) :: plasmod_pech = 0.0d0
  !! ech power (not in use yet) #TODO: units?

  real(dp) :: plasmod_gamcdothers = 1.0d0
  !! efficiency multiplier for non-CD heating. If 0.0 pheat treated as if had no CD associated

  real(dp) :: plasmod_chisawpos = -1.0d0
  !! position where artificial sawtooth diffusivity is added, -1 - uses q=1 position

  real(dp) :: plasmod_chisaw = 0.0d0
  !! artificial diffusivity in m^2/s

  real(dp) :: plasmod_sawpertau = 1.0d-6
  !! ratio between sawtooth period and confinement time

  real(dp) :: plasmod_spellet = 0.0d0
  !! pellet mass in units of D in 10^19

  real(dp) :: plasmod_fpellet = 0.5d0
  !! pellet frequency [Hz]

  real(dp) :: plasmod_pedscal = 1.0d0
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

end module plasmod_variables

module current_drive_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the current drive system
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: beamwd = 0.58D0
  !! width of neutral beam duct where it passes between the TF coils (m)
  !! T Inoue et al, Design of neutral beam system for ITER-FEAT, 
  !! <A HREF=http://dx.doi.org/10.1016/S0920-3796(01)00339-8>
  !! Fusion Engineering and Design, Volumes 56-57, October 2001, Pages 517-521</A>)

  real(dp) :: bigq = 0.0D0
  !! Fusion gain; P_fusion / (P_injection + P_ohmic)

  real(dp) :: bootipf = 0.0D0
  !! bootstrap current fraction (enforced; see ibss)

  real(dp) :: bscfmax = 0.9D0
  !! maximum fraction of plasma current from bootstrap; if `bscfmax < 0`, 
  !! bootstrap fraction = abs(bscfmax)

  real(dp) :: bscf_iter89 = 0.0D0
  !! bootstrap current fraction, ITER 1989 model

  real(dp) :: bscf_nevins = 0.0D0
  !! bootstrap current fraction, Nevins et al model

  real(dp) :: bscf_sauter = 0.0D0
  !! bootstrap current fraction, Sauter et al model

  real(dp) :: bscf_wilson = 0.0D0
  !! bootstrap current fraction, Wilson et al model

  real(dp) :: cboot = 1.0D0
  !! bootstrap current fraction multiplier (`ibss=1`)

  real(dp) :: cnbeam = 0.0D0
  !! neutral beam current (A)

  real(dp) :: diacf_hender = 0.0D0
  !! diamagnetic current fraction, Hender fit

  real(dp) :: diacf_scene = 0.0D0
  !! diamagnetic current fraction, SCENE fit

  real(dp) :: diaipf = 0.0D0
  !! diamagnetic current fraction

  real(dp) :: echpwr = 0.0D0
  !! ECH power (MW)

  real(dp) :: echwpow = 0.0D0
  !! ECH wall plug power (MW)

  real(dp) :: effcd = 0.0D0
  !! current drive efficiency (A/W)

  real(dp) :: enbeam = 1.0D3
  !! neutral beam energy (keV) (`iteration variable 19`)

  real(dp) :: etacd = 0.0D0
  !! auxiliary power wall plug to injector efficiency

  real(dp) :: etacdfix = 0.0D0
  !! secondary auxiliary power wall plug to injector efficiency

  real(dp) :: etaech = 0.3D0
  !! ECH wall plug to injector efficiency

  real(dp) :: etalh = 0.3D0
  !! lower hybrid wall plug to injector efficiency

  real(dp) :: etanbi = 0.3D0
  !! neutral beam wall plug to injector efficiency

  real(dp) :: fpion = 0.5D0
  !! fraction of beam energy to ions

  real(dp) :: pnbitot = 0.0D0
  !! neutral beam power entering vacuum vessel

  real(dp) :: pscf_scene = 0.0D0
  !! Pfirsch-Schlüter current fraction, SCENE fit

  real(dp) :: nbshinemw = 0.0D0
  !! neutral beam shine-through power

  real(dp) :: feffcd = 1.0D0
  !! current drive efficiency fudge factor (`iteration variable 47`)

  real(dp) :: forbitloss = 0.0D0
  !! fraction of neutral beam power lost after ionisation but before 
  !! thermalisation (orbit loss fraction)

  real(dp) :: frbeam = 1.05D0
  !! R_tangential / R_major for neutral beam injection

  real(dp) :: ftritbm = 1.0D-6
  !! fraction of beam that is tritium

  real(dp) :: gamcd = 0.0D0
  !! normalised current drive efficiency (1.0e20 A/(W m^2))
  
  real(dp) :: gamma_ecrh = 0.35D0
  !! User input ECRH gamma (1.0e20 A/(W m^2))

  real(dp) :: rho_ecrh = 0.1D0
  !! normalised minor radius at which electron cyclotron current drive is maximum

  integer :: iefrf = 5
  !! Switch for current drive efficiency model:
  !!
  !!  - =1 Fenstermacher Lower Hybrid
  !!  - =2 Ion Cyclotron current drive
  !!  - =3 Fenstermacher ECH
  !!  - =4 Ehst Lower Hybrid
  !!  - =5 ITER Neutral Beam
  !!  - =6 new Culham Lower Hybrid model
  !!  - =7 new Culham ECCD model
  !!  - =8 new Culham Neutral Beam model
  !!  - =9 Simple NBI model (see SYCOMORE HELIOS paper)
  !!  - =10 ECRH user input gamma
  !!  - =11 ECRH "HARE" model (E. Poli, Physics of Plasmas 2019)
  !!  - =12 Simple NBI model
  
  integer :: iefrffix = 0 
  !! Switch for 2nd current drive efficiency model:
  !! 
  !! - =0 No fixed current drive
  !! - =1 Fenstermacher Lower Hybrid
  !! - =2 Ion Cyclotron current drive
  !! - =3 Fenstermacher ECH
  !! - =4 Ehst Lower Hybrid
  !! - =5 ITER Neutral Beam
  !! - =6 new Culham Lower Hybrid model
  !! - =7 new Culham ECCD model
  !! - =8 new Culham Neutral Beam model
  !! - =9 Simple NBI model (see SYCOMORE HELIOS paper)
  !! - =10 ECRH user input gamma
  !! - =11 ECRH "HARE" model (E. Poli, Physics of Plasmas 2019)

  integer :: irfcd = 1
  !! Switch for current drive calculation:
  !!
  !! - =0 turned off
  !! - =1 turned on

  real(dp) :: nbshinef = 0.0D0
  !! neutral beam shine-through fraction

  real(dp) :: nbshield = 0.5D0
  !! neutral beam duct shielding thickness (m)

  real(dp) :: pheat = 0.0D0
  !! heating power not used for current drive (MW) (`iteration variable 11`)

  real(dp) :: pheatfix = 0.0D0
  !! secondary fixed heating power not used for current drive (MW)

  real(dp) :: pinjalw = 150.0D0
  !! maximum allowable value for injected power (MW) (`constraint equation 30`)

  real(dp) :: pinjemw = 0.0D0
  !! auxiliary injected power to electrons (MW)

  real(dp) :: pinjimw = 0.0D0
  !! auxiliary injected power to ions (MW)

  real(dp) :: pinjmw = 0.0D0
  !! total auxiliary injected power (MW)

  real(dp)  :: pinjfixmw = 0.0D0
  !! secondary total fixed auxiliary injected power (MW)

  real(dp) :: plasipf = 0.0D0
  !! plasma driven current fraction (Bootstrap + Diamagnetic + PS)

  real(dp) :: plhybd = 0.0D0
  !! lower hybrid injection power (MW)

  real(dp) :: pnbeam = 0.0D0
  !! neutral beam injection power (MW)

  real(dp) :: porbitlossmw = 0.0D0
  !! neutral beam power lost after ionisation but before thermalisation (orbit loss power) (MW)

  real(dp) :: psipf = 0.0D0
  !! Pfirsch-Schlüter current fraction

  real(dp) :: pwplh = 0.0D0
  !! lower hybrid wall plug power (MW)

  real(dp) :: pwpnb = 0.0D0
  !! neutral beam wall plug power (MW)

  real(dp) :: rtanbeam = 0.0D0
  !! neutral beam centreline tangency radius (m)

  real(dp) :: rtanmax = 0.0D0
  !! maximum tangency radius for centreline of beam (m)

  real(dp) :: taubeam = 0.0D0
  !! neutral beam e-decay lengths to plasma centre

  real(dp) :: tbeamin = 3.0D0
  !! permitted neutral beam e-decay lengths to plasma centre

end module current_drive_variables

module divertor_kallenbach_variables
  !! author: J. Morri (UKAEA), M. Kovari (UKAEA)
  !!
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

  integer :: kallenbach_switch = 0
  !! switch to turn on the 1D Kallenbach divertor model:
  !!
  !! - =1 on
  !! - =0 off

  integer :: kallenbach_tests = 0
  !! switch to run tests of 1D Kallenbach divertor model:
  !!
  !! - =1 on
  !! - =0 off

  integer :: kallenbach_test_option = 0
  !! switch to choose kallenbach test option:
  !!
  !! - =0 Test case with user inputs
  !! - =1 Test case for Kallenbach paper

  integer :: kallenbach_scan_switch = 0
  !! switch to run scan of 1D Kallenbach divertor model:
  !!
  !! - =1 on
  !! - =0 off

  integer :: kallenbach_scan_var = 0
  !! switch for parameter to scan for kallenbach scan test:
  !!
  !! - =0 ttarget
  !! - =1 qtargettotal
  !! - =2 targetangle
  !! - =3 lambda_q_omp
  !! - =4 netau_sol

  real(dp) :: kallenbach_scan_start = 2.0
  !! start value for kallenbach scan parameter

  real(dp) :: kallenbach_scan_end = 10.0
  !! end value for kallenbach scan parameter

  integer :: kallenbach_scan_num = 1
  !! number of scans for kallenbach scan test

  real(dp) :: target_spread = 0.003D0
  !! increase in SOL power fall-off length due to spreading, mapped to OMP [m]

  real(dp) :: lambda_q_omp = 0.002D0
  !! SOL power fall-off length at the outer midplane, perpendicular to field [m]

  real(dp) :: lcon_factor = 1.0D0
  !! Correction factor for connection length from OMP to divertor = connection length/(pi*q*rmajor)

  real(dp) :: netau_sol = 0.5D0
  !! Parameter describing the departure from local ionisation equilibrium in the SOL. [ms.1e20/m3]

  real(dp) :: targetangle = 30.0D0
  !! Angle between field-line and divertor target (degrees)

  real(dp) :: ttarget = 5.0D0
  !! Plasma temperature adjacent to divertor sheath [eV] (`iteration variable 120`)
  !! Rem : 5 eV is the current limit for tungsten sputtering from argon impurity

  real(dp) :: qtargettotal = 5.0D6
  !! Power density on target including surface recombination [W/m2] (`iteration variable 124`)

  real(dp), dimension(14) :: impurity_enrichment = 5.0D0
  !! Ratio of each impurity concentration in SOL to confined plasma + the enrichment for Argon 
  !! is also propagated for PLASMOD (`ipedestal=3`)

  real(dp) :: psep_kallenbach = 0.0D0
  !! Power conducted through the separatrix, as calculated by the divertor model [W]
  !! Not equal to pdivt unless `constraint 69` is imposed.

  real(dp) :: teomp = 0.0D0
  !! separatrix temperature calculated by the Kallenbach divertor model [eV] (issue #457)

  real(dp) :: neomp = 0.0D0
  !! Mean SOL density at OMP calculated by the Kallenbach divertor model [m-3]

  real(dp) :: neratio = 0.75D0
  !! Ratio of mean SOL density at OMP to separatrix density at OMP (`iteration variable 121`)

  real(dp) :: pressure0 = 0.0D0
  !! Total plasma pressure near target (thermal+dynamic) [Pa]

  real(dp) :: fractionwidesol = 0.1D0
  !! Distance from target at which SOL gets broader as a fraction of connection length

  real(dp), public :: fmom
  !! momentum factor [-]

  real(dp), public :: totalpowerlost
  !! Total power lost due to radiation, ionisation and recombination [W]

  real(dp), public :: impuritypowerlost
  !! Power lost due to impurity radiation [W]

  real(dp), public :: hydrogenicpowerlost
  !! Power lost due to hydrogenic radiation [W]

  real(dp), public :: exchangepowerlost
  !! Power lost due to charge exchange  [W]

  real(dp), public :: ionisationpowerlost
  !! Power lost due to electron impact ionisation [W]

  real(dp), public :: abserr_sol = 1.d-4
  !! Absolute contribution to the error tolerance in the Kallenbach divertor model

  real(dp), public :: relerr_sol = 1.d-4
  !! Relative contribution to the error tolerance in the Kallenbach divertor model

  real(dp), public :: mach0 = 0.999
  !! Mach number at target (must be just less than 1)

end module divertor_kallenbach_variables

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

  real(dp), parameter :: cp_he = 5195.0D0
  !! specific heat capacity at constant pressure: helium (`primary_pumping=3`) [J/(kg.K)]

  real(dp), parameter :: t_in_bb =573.13D0
  !! temperature in FW and blanket coolant at blanket entrance (`primary_pumping=3`) [K]

  real(dp), parameter :: t_out_bb =773.13D0
  !! temperature in FW and blanket coolant at blanket exit (`primary_pumping=3`) [K]

  real(dp), parameter :: p_he =8.0D6
  !! pressure in FW and blanket coolant at pump exit (`primary_pumping=3`) [Pa]

  real(dp), parameter :: dp_he =5.5D5
  !! pressure drop in FW and blanket coolant including heat exchanger and pipes (`primary_pumping=3`) [Pa]

  real(dp) :: htpmw_fw_blkt = 0.0d0
  !! mechanical pumping power for FW and blanket including heat exchanger and 
  !! pipes (`primary_pumping=3`) [MW]

end module primary_pumping_variables

module pfcoil_variables
  !! author: J. Morris, M. Kovari (UKAEA)
  !!
  !! Module containing global variables relating to the poloidal field coil systems
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  integer, parameter :: ngrpmx = 8
  !! maximum number of groups of PF coils

  integer, parameter :: nclsmx = 2
  !! maximum number of PF coils in a given group
  
  integer, parameter :: nptsmx = 32
  !! maximum number of points across the midplane of the plasma at which the field from 
  !! the PF coils is fixed

  integer, parameter :: nfixmx = 64
  !! maximum number of fixed current PF coils

  integer, parameter :: ngc = ngrpmx*nclsmx
  !! maximum total number of coils across all groups

  integer, parameter :: ngc2 = ngc+2
  !! new variable to include 2 additional circuits: plasma and central solenoid

  real(dp) :: alfapf = 5.0D-10
  !! smoothing parameter used in PF coil current calculation at the beginning of pulse (BoP)

  real(dp) :: alstroh = 4.0D8
  !! allowable hoop stress in Central Solenoid structural material (Pa)

  integer :: i_cs_stress = 0
  !! Switch for CS stress calculation:
  !!
  !! - =0 Hoop stress only
  !! - =1 Hoop + Axial stress

  real(dp) :: areaoh = 0.0D0
  !! Central solenoid vertical cross-sectional area (m2)

  real(dp) :: a_oh_turn = 0.0D0
  !! Central solenoid (OH) trun cross-sectional area (m2)

  real(dp) :: awpoh = 0.0D0
  !! central solenoid conductor+void area (m2)

  real(dp) :: bmaxoh = 0.0D0
  !! maximum field in central solenoid at end of flat-top (EoF) (T)

  real(dp) :: bmaxoh0 = 0.0D0
  !! maximum field in central solenoid at beginning of pulse (T)

  real(dp), dimension(ngc2) :: bpf = 0.0D0
  !! peak field at coil i (T)
  
  real(dp) :: cohbop = 0.0D0
  !! Central solenoid overall current density at beginning of pulse (A/m2)
  
  real(dp) :: coheof = 1.85D7
  !! Central solenoid overall current density at end of flat-top (A/m2) (`iteration variable 37`)
  
  real(dp), dimension(ngc2,6) :: cpt = 0.0D0
  !! current per turn in coil i at time j (A)

  real(dp), dimension(ngc2) :: cptdin = 4.0D4
  !! peak current per turn input for PF coil i (A)

  real(dp), dimension(ngc2) :: curpfb = 0.0D0
  !! PF coil current work array beginning of pulse

  real(dp), dimension(ngc2) :: curpff = 0.0D0
  !! PF coil current work array flat top

  real(dp), dimension(ngc2) :: curpfs = 0.0D0
  !! PF coil current work array end of pulse

  real(dp) :: etapsu = 0.9D0
  !! Efficiency of transfer of PF stored energy into or out of storage.

  real(dp) :: fcohbof = 0.0D0
  !! ratio of central solenoid overall current density at beginning of flat-top / end of flat-top

  real(dp) :: fcohbop = 0.9D0
  !! ratio of central solenoid overall current density at beginning of pulse / end of flat-top
  !! (`iteration variable 41`)

  real(dp) :: fcuohsu = 0.7D0
  !! copper fraction of strand in central solenoid

  real(dp) :: fcupfsu = 0.69D0
  !! copper fraction of cable conductor (PF coils)

  real(dp) :: fvssu = 1.0
  !! F-value for `constraint equation 51` 

  integer, dimension(ngc) :: ipfloc = (/2,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  !! switch for locating scheme of PF coil group i:
  !!
  !! - =1 PF coil on top of central solenoid
  !! - =2 PF coil on top of TF coil
  !! - =3 PF coil outside of TF coil

  integer :: ipfres = 0
  !! switch for PF coil type:
  !!
  !! - =0 superconducting PF coils
  !! - =1 resistive PF coils
  ! 
  real(dp) :: itr_sum = 0.0D0
  !! total sum of I x turns x radius for all PF coils and CS (Am)

  integer :: isumatoh = 1
  !! switch for superconductor material in central solenoid:
  !!
  !! - =1 ITER Nb3Sn critical surface model with standard
  !!   ITER parameters
  !! - =2 Bi-2212 high temperature superconductor (range of
  !!   validity T < 20K, adjusted field b < 104 T, B > 6 T)
  !! - =3 NbTi
  !! - =4 ITER Nb3Sn model with user-specified parameters
  !! - =5 WST Nb3Sn parameterisation
  !! - =6 REBCO HTS parameterisation

  integer :: isumatpf = 1
  !! switch for superconductor material in PF coils:
  !!
  !! - =1 ITER Nb3Sn critical surface model with standard
  !!   ITER parameters
  !! - =2 Bi-2212 high temperature superconductor (range of
  !!   validity T < 20K, adjusted field b < 104 T, B > 6 T)
  !! - =3 NbTi
  !! - =4 ITER Nb3Sn model with user-specified parameters
  !! - =5 WST Nb3Sn parameterisation

  real(dp) :: jscoh_bop = 0.0D0
  !! central solenoid superconductor critical current density (A/m2) at beginning-of-pulse

  real(dp) :: jscoh_eof = 0.0D0
  !! central solenoid superconductor critical current density (A/m2) at end-of-flattop

  real(dp) :: jstrandoh_bop = 0.0D0
  !! central solenoid strand critical current density (A/m2) at beginning-of-pulse

  real(dp) :: jstrandoh_eof = 0.0D0
  !! central solenoid strand critical current density (A/m2) at end-of-flattop

  integer :: ncirt = 0
  !! number of PF circuits (including central solenoid and plasma)

  integer, dimension(ngrpmx+2) :: ncls = (/1,1,2,0,0,0,0,0,0,0/)
  !! number of PF coils in group j

  integer :: nfxfh = 7
  !! number of filaments the top and bottom of the central solenoid should be broken 
  !! into during scaling (5 - 10 is good)

  integer :: ngrp = 3
  !! number of groups of PF coils. Symmetric coil pairs should all be in the same group

  integer :: nohc = 0
  !! number of PF coils (excluding the central solenoid) + 1
  
  real(dp) :: ohhghf = 0.71D0
  !! Central solenoid height / TF coil internal height
  
  real(dp) :: oh_steel_frac = 0.5D0
  !! central solenoid steel fraction (`iteration variable 122`)

  real(dp), dimension(ngc2) :: pfcaseth = 0.0D0
  !! steel case thickness for PF coil i (m)

  real(dp) :: pfclres = 2.5D-8
  !! PF coil resistivity (if ipfres=1) (Ohm-m)

  real(dp) :: pfmmax = 0.0D0
  !! mass of heaviest PF coil (tonnes)

  real(dp) :: pfrmax = 0.0D0
  !! radius of largest PF coil (m)

  real(dp) :: pfwpmw = 0.0D0
  !! Total mean wall plug power dissipated in PFC and CS power supplies (MW) (issue #713)

  real(dp) :: powohres = 0.0D0
  !! central solenoid resistive power during flattop (W)

  real(dp) :: powpfres = 0.0D0
  !! total PF coil resistive losses during flattop (W)

  real(dp), dimension(ngc2) :: ra = 0.0D0
  !! inner radius of coil i (m)

  real(dp), dimension(ngc2) :: rb = 0.0D0
  !! outer radius of coil i (m)

  real(dp), dimension(ngc2) :: ric = 0.0D0
  !! peak current in coil i (MA-turns)

  real(dp), dimension(ngc2) :: rjconpf = 3.0D7
  !! average winding pack current density of PF coil i (A/m2) at time of peak 
  !! current in that coil (calculated for `ipfloc=1` coils)

  real(dp) :: rjohc = 0.0D0
  !! allowable central solenoid current density at end of flat-top (A/m2)

  real(dp) :: rjohc0 = 0.0D0
  !! allowable central solenoid current density at beginning of pulse (A/m2)

  real(dp), dimension(ngc2) :: rjpfalw = 0.0D0
  !! allowable winding pack current density of PF coil i (A/m2)

  real(dp) :: rohc = 0.0D0
  !! radius to the centre of the central solenoid (m)

  real(dp) :: routr = 1.5D0
  !! radial distance (m) from outboard TF coil leg to centre of `ipfloc=3` PF coils

  real(dp), dimension(ngc2) :: rpf = 0.0D0
  !! radius of PF coil i (m)

  real(dp) :: rpf1 = 0.0D0
  !! offset (m) of radial position of `ipfloc=1` PF coils from being directly above
  !! the central solenoid

  real(dp) :: rpf2 = -1.63D0
  !! offset (m) of radial position of `ipfloc=2` PF coils from being at 
  !! rmajor (offset = rpf2*triang*rminor)

  real(dp) :: s_tresca_oh = 0.0D0
  !! Tresca stress coils/central solenoid [MPa]

  real(dp) :: sigpfcalw = 500.0D0
  !! maximum permissible tensile stress (MPa) in steel coil cases for superconducting 
  !! PF coils (`ipfres=0`)

  real(dp) :: sigpfcf = 0.666D0
  !! fraction of JxB hoop force supported by steel case for superconducting PF coils (`ipfres=0`)

  real(dp), dimension(ngc2,ngc2) :: sxlg = 0.0D0
  !! mutual inductance matrix (H)

  real(dp) :: tmargoh = 0.0D0
  !! Central solenoid temperature margin (K)

  real(dp), dimension(ngc2) :: turns = 0.0D0
  !! number of turns in PF coil i

  real(dp), dimension(ngc2) :: vf = 0.3D0
  !! winding pack void fraction of PF coil i for coolant

  real(dp) :: vfohc = 0.3D0
  !! void fraction of central solenoid conductor for coolant

  real(dp) :: vsbn = 0.0D0
  !! total flux swing available for burn (Wb)

  real(dp) :: vsefbn = 0.0D0
  !! flux swing from PF coils for burn (Wb)

  real(dp) :: vsefsu = 0.0D0
  !! flux swing from PF coils for startup (Wb)

  real(dp) :: vseft = 0.0D0
  !! total flux swing from PF coils (Wb)
  
  real(dp) :: vsoh = 0.0D0
  !! total flux swing from the central solenoid (Wb)

  real(dp) :: vsohbn = 0.0D0
  !! central solenoid flux swing for burn (Wb)

  real(dp) :: vsohsu = 0.0D0
  !! central solenoid flux swing for startup (Wb)

  real(dp) :: vssu = 0.0D0
  !! total flux swing for startup (`constraint eqn 51` to enforce vssu=vsres+vsind) (Wb)

  real(dp) :: vstot = 0.0D0
  !! total flux swing for pulse (Wb)
  
  real(dp), dimension(ngc2,6) :: waves = 0.0D0
  !! used in current waveform of PF coils/central solenoid

  real(dp) :: whtpf = 0.0D0
  !! total mass of the PF coil conductor (kg)

  real(dp) :: whtpfs = 0.0D0
  !! total mass of the PF coil structure (kg)

  real(dp), dimension(ngc2) :: wtc = 0.0D0
  !! conductor mass for PF coil i (kg)

  real(dp), dimension(ngc2) :: wts = 0.0D0
  !! structure mass for PF coil i (kg)

  real(dp), dimension(ngc2) :: zh = 0.0D0
  !! upper point of PF coil i (m)

  real(dp), dimension(ngc2) :: zl = 0.0D0
  !! lower point of PF coil i (m)

  real(dp), dimension(ngc2) :: zpf = 0.0D0
  !! z (height) location of PF coil i (m)

  real(dp), dimension(ngrpmx) :: zref = (/3.6D0, 1.2D0, 2.5D0, &
    1.0D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0/)
  !! PF coil vertical positioning adjuster:
  !!
  !! - for groups j with ipfloc(j) = 1; zref(j) is ignored
  !! - for groups j with ipfloc(j) = 2 AND itart=1 (only);
  !!   zref(j) is distance of centre of PF coil from inside
  !!   edge of TF coil (remember that PF coils for STs lie
  !!   within the TF coil)
  !! - for groups j with ipfloc(j) = 3; zref(j) = ratio of
  !!   height of coil group j to plasma minor radius</UL>

  real(dp) :: bmaxcs_lim = 13.0
  !! Central solenoid max field limit [T]

  real(dp) :: fbmaxcs = 13.0
  !! F-value for CS mmax field (`cons. 79`, `itvar 149`)

end module pfcoil_variables

module structure_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the support structure
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: aintmass = 0.0D0
  !! intercoil structure mass (kg)

  real(dp) :: clgsmass = 0.0D0
  !! gravity support structure for TF coil, PF coil and intercoil support systems (kg)

  real(dp) :: coldmass = 0.0D0
  !! total mass of components at cryogenic temperatures (kg)

  real(dp) :: fncmass = 0.0D0
  !! PF coil outer support fence mass (kg)

  real(dp) :: gsmass = 0.0D0
  !! reactor core gravity support mass (kg)

end module structure_variables

module vacuum_variables
  !! author: J. Morris, M. Kovari (UKAEA)
  !!
  !! Module containing global variables relating to the vacuum system
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public
  character(len=6) :: vacuum_model = 'old'
  !! switch for vacuum pumping model:
  !!
  !! - ='old' for old detailed ETR model
  !! - ='simple' for simple steady-state model with comparison to ITER cryopumps
  !#TODO: old and simple not suitable names.

  real(dp) :: niterpump = 0.0D0
  !! number of high vacuum pumps (real number), each with the throughput of one 
  !! ITER cryopump (50 Pa m3 s-1), all operating at the same time (`vacuum_model='simple'`)

  integer :: ntype = 1
  !! switch for vacuum pump type:
  !!
  !! - =0 - for turbomolecular pump (magnetic bearing) with speed of 2.0 m3/s
  !!   (1.95 for N2, 1.8 for He, 1.8 for DT)
  !! - =1 - for compound cryopump with nominal speed of 10.0 m3/s
  !!   (9.0 for N2, 5.0 for He and 25.0 for DT)

  integer :: nvduct = 0
  !! number of ducts (torus to pumps)

  real(dp) :: dlscal = 0.0D0
  !! vacuum system duct length scaling

  real(dp) :: pbase = 5.0D-4
  !! base pressure during dwell before gas pre-fill(Pa)

  real(dp) :: prdiv = 0.36D0
  !! divertor chamber pressure during burn (Pa)

  real(dp) :: pumptp = 1.2155D22
  !! Pump throughput (molecules/s) (default is ITER value)

  real(dp) :: rat = 1.3D-8
  !! plasma chamber wall outgassing rate (Pa-m/s)

  real(dp) :: tn = 300.0D0
  !! neutral gas temperature in chamber (K)

  real(dp) :: vacdshm = 0.0D0
  !! mass of vacuum duct shield (kg)

  real(dp) :: vcdimax = 0.0D0
  !! diameter of duct passage (m)

  integer :: vpumpn = 0
  !! number of high vacuum pumps

  integer :: dwell_pump = 0
  !! switch for dwell pumping options:
  !!
  !! - =0 pumping only during tdwell
  !! - =1 pumping only during tramp
  !! - =2 pumping during tdwell + tramp

  ! The following are used in the Battes, Day and Rohde pump-down model
  ! See "Basic considerations on the pump-down time in the dwell phase of a pulsed fusion DEMO"
  ! http://dx.doi.org/10.1016/j.fusengdes.2015.07.011)(vacuum_model=simple')

  real(dp) :: pumpareafraction = 0.0203D0
  !! area of one pumping port as a fraction of plasma surface area

  real(dp) :: pumpspeedmax = 27.3D0
  !! maximum pumping speed per unit area for deuterium & tritium, molecular flow

  real(dp) :: pumpspeedfactor = 0.167D0
  !! effective pumping speed reduction factor due to duct impedance

  real(dp) :: initialpressure = 1.0D0
  !! initial neutral pressure at the beginning of the dwell phase (Pa)
  
  real(dp) :: outgasindex = 1.0D0
  !! outgassing decay index

  real(dp) :: outgasfactor = 0.0235D0
  !! outgassing prefactor kw: outgassing rate at 1 s per unit area (Pa m s-1)

end module vacuum_variables

module pf_power_variables
  !! author: J. Morris, M. Kovari (UKAEA)
  !!
  !! Module containing global variables relating to the PF coil power conversion system
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: acptmax = 0.0D0
  !! average of currents in PF circuits (kA)

  real(dp) :: ensxpfm = 0.0D0
  !! maximum stored energy in the PF circuits (MJ)

  integer :: iscenr = 2
  !! Switch for PF coil energy storage option:
  !!
  !! - =1 all power from MGF (motor-generator flywheel) units
  !! - =2 all pulsed power from line
  !! - =3 PF power from MGF, heating from line
  !   (In fact, options 1 and 3 are not treated differently)

  real(dp) :: pfckts = 0.0D0
  !! number of PF coil circuits

  real(dp) :: spfbusl = 0.0D0
  !! total PF coil circuit bus length (m)

  real(dp) :: spsmva = 0.0D0
  !! sum of PF power supply ratings (MVA)

  real(dp) :: srcktpm = 0.0D0
  !! sum of resistive PF coil power (kW)

  real(dp) :: vpfskv = 0.0D0
  !! PF coil voltage (kV)

  real(dp) :: peakpoloidalpower = 0.0D0
  !! Peak absolute rate of change of stored energy in poloidal field (MW)

  real(dp) :: maxpoloidalpower = 1000.0D0
  !! Maximum permitted absolute rate of change of stored energy in poloidal field (MW)

  real(dp), dimension(5) :: poloidalpower = 0.0D0
  !! Poloidal power usage at time t (MW)

end module pf_power_variables

module build_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the machine's radial and vertical build
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: aplasmin = 0.25D0
  !! minimum minor radius (m)

  real(dp) ::   available_radial_space = 0.0D0
  !! Minimal radial space between plasma and coils (m)

  real(dp) :: blarea = 0.0D0
  !! blanket total surface area (m2)

  real(dp) :: blareaib = 0.0D0
  !! inboard blanket surface area (m2)

  real(dp) :: blareaob = 0.0D0
  !! outboard blanket surface area (m2)
  
  real(dp) :: blbmith = 0.17D0
  !! inboard blanket box manifold thickness (m) (`blktmodel>0`)
  !#TODO: remove blktmodel and similar below

  real(dp) :: blbmoth = 0.27D0
  !! outboard blanket box manifold thickness (m) (`blktmodel>0`)

  real(dp) :: blbpith = 0.30D0
  !! inboard blanket base plate thickness (m) (`blktmodel>0`)

  real(dp) :: blbpoth = 0.35D0
  !! outboard blanket base plate thickness (m) (`blktmodel>0`)

  real(dp) :: blbuith = 0.365D0
  !! inboard blanket breeding zone thickness (m) (`blktmodel>0`) (`iteration variable 90`)

  real(dp) :: blbuoth = 0.465D0
  !! outboard blanket breeding zone thickness (m) (`blktmodel>0`) (`iteration variable 91`)
  
  real(dp) :: blnkith = 0.115D0
  !! inboard blanket thickness (m); (calculated if `blktmodel>0`) (=0.0 if `iblnkith=0`)

  real(dp) :: blnkoth = 0.235D0
  !! outboard blanket thickness (m); calculated if `blktmodel>0`

  real(dp) :: blnktth = 0.0D0
  !! top blanket thickness (m), = mean of inboard and outboard blanket thicknesses

  real(dp) :: bore = 1.42D0
  !! central solenoid inboard radius (m) (`iteration variable 29`)

  real(dp) :: clhsf = 4.268D0
  !! cryostat lid height scaling factor (tokamaks)
  
  real(dp) :: ddwex = 0.07D0
  !! cryostat thickness (m)

  real(dp) :: ddwi = 0.07D0
  !! vacuum vessel thickness (TF coil / shield) (m)

  real(dp) :: f_avspace = 1.0D0
  !! F-value for stellarator radial space check (`constraint equation 83`)

  real(dp) :: fcspc = 0.6D0
  !! Fraction of space occupied by CS pre-compression structure

  real(dp) :: fmsbc = 0.0D0
  !! Martensitic fraction of steel in (non-existent!) bucking cylinder

  real(dp) :: fmsbl = 0.0D0
  !! Martensitic fraction of steel in blanket

  real(dp) :: fmsdwe = 0.0D0
  !! Martensitic fraction of steel in cryostat

  real(dp) :: fmsdwi = 0.0D0
  !! Martensitic fraction of steel in vacuum vessel

  real(dp) :: fmsfw = 0.0D0
  !! Martensitic fraction of steel in first wall

  real(dp) :: fmsoh = 0.0D0
  !! Martensitic fraction of steel in central solenoid

  real(dp) :: fmssh = 0.0D0
  !! Martensitic fraction of steel in shield

  real(dp) :: fmstf = 0.0D0
  !! Martensitic fraction of steel in TF coil

  real(dp) :: fseppc = 3.5D8
  !! Separation force in CS coil pre-compression structure

  real(dp) :: fwarea = 0.0D0
  !! first wall total surface area (m2)

  real(dp) :: fwareaib = 0.0D0
  !! inboard first wall surface area (m2)

  real(dp) :: fwareaob = 0.0D0
  !! outboard first wall surface area (m2)

  real(dp) :: fwith = 0.0D0
  !! inboard first wall thickness, initial estimate as calculated (m)

  real(dp) :: fwoth = 0.0D0
  !! outboard first wall thickness, initial estimate as calculated (m)
  
  real(dp) :: gapds = 0.155D0
  !! gap between inboard vacuum vessel and thermal shield (m) (`iteration variable 61`)

  real(dp) :: gapoh = 0.08D0
  !! gap between central solenoid and TF coil (m) (`iteration variable 42`)

  real(dp) :: gapomin = 0.234D0
  !! minimum gap between outboard vacuum vessel and TF coil (m) (`iteration variable 31`)

  real(dp) :: gapsto = 0.0D0
  !! gap between outboard vacuum vessel and TF coil (m)

  real(dp) :: hmax = 0.0D0
  !! maximum (half-)height of TF coil (inside edge) (m)

  real(dp) :: hpfdif = 0.0D0
  !! difference in distance from midplane of upper and lower portions of TF 
  !! legs (non-zero for single-null devices) (m)

  real(dp) :: hpfu = 0.0D0
  !! height to top of (upper) TF coil leg (m)

  real(dp) :: hr1 = 0.0D0
  !! half-height of TF coil inboard leg straight section (m)

  integer :: iohcl = 1
  !! Switch for existence of central solenoid:
  !!
  !! - =0 central solenoid not present
  !! - =1 central solenoid exists

  integer :: iprecomp = 1
  !! Switch for existence of central solenoid pre-compression structure:
  !!
  !! - =0 no pre-compression structure
  !! - =1 calculated pre-compression structure

  real(dp) :: ohcth = 0.811D0
  !! Central solenoid thickness (m) (`iteration variable 16`)

  real(dp) :: precomp = 0.0D0
  !! CS coil precompression structure thickness (m)

  real(dp) :: rbld = 0.0D0
  !! sum of thicknesses to the major radius (m)

  real(dp) :: required_radial_space = 0.0D0
  !! Required space between coil and plasma for blanket shield wall etc (m)

  real(dp) :: rinboard = 0.651D0
  !! plasma inboard radius (m) (`consistency equation 29`)

  real(dp) :: rsldi = 0.0D0
  !! radius to inboard shield (inside point) (m)

  real(dp) :: rsldo = 0.0D0
  !! radius to outboard shield (outside point) (m)

  real(dp) :: r_vv_inboard_out = 0.0D0
  !! Radial plasma facing side position of inboard vacuum vessel [m]

  real(dp) :: r_sh_inboard_out = 0.0D0
  !! Radial plasma facing side position of inboard neutronic shield [m]

  real(dp) :: r_tf_inboard_in = 0.0D0
  !! Mid-plane Outer radius of inner of inboard TF leg (m)

  real(dp) :: r_tf_inboard_mid = 0.0D0
  !! Mid-plane Outer radius of centre of inboard TF leg (m)
       
  real(dp) :: r_tf_inboard_out = 0.0D0
  !! Mid-plane Outer radius of centre of inboard TF leg (m)
       
  real(dp) :: r_tf_outboard_mid = 0.0D0
  !! radius to the centre of the outboard TF coil leg (m)

  real(dp) :: r_cp_top = 0.0D0
  !! Top outer radius of the centropost (ST only) (m)

  real(dp) :: dr_tf_inner_bore = 0.0D0
  !! TF coil horizontal inner bore (m)

  real(dp) :: dh_tf_inner_bore = 0.0D0
  !! TF coil vertical inner bore (m)

  real(dp) :: scrapli = 0.14D0
  !! Gap between plasma and first wall, inboard side (m) (if `iscrp=1`) (`iteration variable 73`)

  real(dp) :: scraplo = 0.15D0
  !! gap between plasma and first wall, outboard side (m) (if `iscrp=1`) (`iteration variable 74`)

  real(dp) :: sharea = 0.0D0
  !! shield total surface area (m2)

  real(dp) :: shareaib = 0.0D0
  !! inboard shield surface area (m2)

  real(dp) :: shareaob = 0.0D0
  !! outboard shield surface area (m2)

  real(dp) :: shldith = 0.69D0
  !! inboard shield thickness (m) (`iteration variable 93`)

  real(dp) :: shldlth = 0.7D0
  !! lower (under divertor) shield thickness (m)

  real(dp) :: shldoth = 1.05D0
  !! outboard shield thickness (m) (`iteration variable 94`)

  real(dp) :: shldtth = 0.6D0
  !! upper/lower shield thickness (m); calculated if `blktmodel > 0`

  real(dp) :: sigallpc = 3.0D8
  !! allowable stress in CSpre-compression structure (Pa)

  !#TODO: Issue #514 Make tfcth an output not an iteration variable
  real(dp) :: tfcth = 0.0D0
  !! inboard TF coil thickness, (centrepost for ST) (m)
  !! (input, calculated or `iteration variable 13`)

  real(dp) :: tfoffset = 0.0D0
  !! vertical distance between centre of TF coils and centre of plasma (m)

  real(dp) :: tfootfi = 1.19D0
  !! TF coil outboard leg / inboard leg radial thickness
  !! ratio (`i_tf_sup=0` only) (`iteration variable 75`)

  real(dp) :: tfthko = 0.0D0
  !! Outboard TF coil thickness (m)

  real(dp) :: tftsgap = 0.05D0
  !! Minimum metal-to-metal gap between TF coil and thermal shield (m)

  real(dp) :: thshield = 0.05D0
  !! TF-VV thermal shield thickness (m)

  real(dp) :: vgap2 = 0.163D0
  !! vertical gap between vacuum vessel and thermal shields (m)

  real(dp) :: vgap= 0.0D0
  !! vertical gap between x-point and divertor (m) (if = 0, it is calculated)

  real(dp) :: vgaptop = 0.60D0
  !! vertical gap between top of plasma and first wall (m)

  real(dp) :: vvblgap = 0.05D0
  !! gap between vacuum vessel and blanket (m)

  real(dp) :: plleni = 1.0D0
  !! length of inboard divertor plate (m)
  
  real(dp) :: plleno = 1.0D0
  !! length of outboard divertor plate (m)

  real(dp) :: plsepi = 1.0D0
  !! poloidal length, x-point to inboard strike point (m)

  real(dp) :: plsepo = 1.5D0
  !! poloidal length, x-point to outboard strike point (m)

  real(dp) :: rspo = 0.0D0
  !! outboard strike point radius (m)

end module build_variables

module constraint_variables
  !! author: J. Morris (UKAEA)
  !!
  !! This module contains global variables relating to the constraint 
  !! equations (f-values, limits, etc.).
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: auxmin = 0.1D0
  !! minimum auxiliary power (MW) (`constraint equation 40`)

  real(dp) :: betpmx = 0.19D0
  !! maximum poloidal beta (`constraint equation 48`)

  real(dp) :: bigqmin = 10.0D0
  !! minimum fusion gain Q (`constraint equation 28`)

  real(dp) :: bmxlim = 12.0D0
  !! maximum peak toroidal field (T) (`constraint equation 25`)

  real(dp) :: fauxmn = 1.0D0
  !! f-value for minimum auxiliary power (`constraint equation 40`, `iteration variable 64`)

  real(dp) :: fbeta = 1.0D0
  !! f-value for epsilon beta-poloidal (`constraint equation 6`, `iteration variable 8`)

  real(dp) :: fbetap = 1.0D0
  !! f-value for poloidal beta (`constraint equation 48`, `iteration variable 79`)
  
  real(dp) :: fbetatry = 1.0D0
  !! f-value for beta limit (`constraint equation 24`, `iteration variable 36`)

  real(dp) :: fbetatry_lower = 1.0D0
  !! f-value for (lower) beta limit (`constraint equation 84`, `iteration variable 173`)
  
  real(dp) :: fcpttf = 1.0D0
  !! f-value for TF coil current per turn upper limit
  !! (`constraint equation 77`, `iteration variable 146`)

  real(dp) :: fcwr = 1.0D0
  !! f-value for conducting wall radius / rminor limit 
  !! (`constraint equation 23`, `iteration variable 104`)

  real(dp) :: fdene = 1.0D0
  !! f-value for density limit (`constraint equation 5`, `iteration variable 9`)
  !! (invalid if `ipedestal=3`)

  real(dp) :: fdivcol = 1.0D0
  !! f-value for divertor collisionality (`constraint equation 22`, `iteration variable 34`)

  real(dp) :: fdtmp = 1.0D0
  !! f-value for first wall coolant temperature rise 
  !! (`constraint equation 38`, `iteration variable 62`)

  real(dp) :: fflutf = 1.0D0
  !! f-value for neutron fluence on TF coil (`constraint equation 53`, `iteration variable 92`)

  real(dp) :: ffuspow = 1.0D0
  !! f-value for maximum fusion power (`constraint equation 9`, `iteration variable 26`)

  real(dp) :: fgamcd = 1.0D0
  !! f-value for current drive gamma (`constraint equation 37`, `iteration variable 40`)

  real(dp) :: fhldiv = 1.0D0
  !! f-value for divertor heat load (`constraint equation 18`, `iteration variable 27`)

  real(dp) :: fiooic = 0.5D0
  !! f-value for TF coil operating current / critical current ratio
  !! (`constraint equation 33`, `iteration variable 50`)

  real(dp) :: fipir = 1.0D0
  !! f-value for Ip/Irod limit (`constraint equation 46`, `iteration variable 72`)

  real(dp) :: fjohc = 1.0D0
  !! f-value for central solenoid current at end-of-flattop
  !! (`constraint equation 26`, `iteration variable 38`)

  real(dp) :: fjohc0 = 1.0D0
  !! f-value for central solenoid current at beginning of pulse
  !! (`constraint equation 27`, `iteration variable 39`)

  real(dp) :: fjprot = 1.0D0
  !! f-value for TF coil winding pack current density 
  !! (`constraint equation 35`, `iteration variable 53`)

  real(dp) :: flhthresh = 1.0D0
  !! f-value for L-H power threshold (`constraint equation 15`, `iteration variable 103`)

  real(dp) :: fmva = 1.0D0
  !! f-value for maximum MVA (`constraint equation 19`, `iteration variable 30`)
  
  real(dp) :: fnbshinef = 1.0D0
  !! f-value for maximum neutral beam shine-through fraction 
  !! (`constraint equation 59`, `iteration variable 105`)

  real(dp) :: fnesep = 1.0D0
  !! f-value for Eich critical separatrix density 
  !! (`constraint equation 76`, `iteration variable 144`)

  real(dp) :: foh_stress = 1.0D0
  !! f-value for Tresca stress in Central Solenoid
  !! (`constraint equation 72`, `iteration variable 123`)

  real(dp) :: fpeakb = 1.0D0
  !! f-value for maximum toroidal field (`constraint equation 25`, `iteration variable 35`)

  real(dp) :: fpinj = 1.0D0
  !! f-value for injection power (`constraint equation 30`, `iteration variable 46`)

  real(dp) :: fpnetel = 1.0D0
  !! f-value for net electric power (`constraint equation 16`, `iteration variable 25`)

  real(dp) :: fportsz = 1.0D0
  !! f-value for neutral beam tangency radius limit
  !! (`constraint equation 20`, `iteration variable 33`)

  real(dp) :: fpsepbqar = 1.0D0
  !! f-value for maximum Psep*Bt/qAR limit (`constraint equation 68`, `iteration variable 117`)

  real(dp) :: fpsepr = 1.0D0
  !! f-value for maximum Psep/R limit (`constraint equation 56`, `iteration variable 97`)

  real(dp) :: fptemp = 1.0D0
  !! f-value for peak centrepost temperature (`constraint equation 44`, `iteration variable 68`)

  real(dp) :: fptfnuc = 1.0D0
  !! f-value for maximum TF coil nuclear heating (`constraint equation 54`, `iteration variable 95`)

  real(dp) :: fq = 1.0D0
  !! f-value for edge safety factor (`constraint equation 45`, `iteration variable 71`)

  real(dp) :: fqval = 1.0D0
  !! f-value for Q (`constraint equation 28`, `iteration variable 45`)

  real(dp) :: fradpwr = 1.0D0
  !! f-value for core radiation power limit (`constraint equation 17`, `iteration variable 28`)

  real(dp) :: fradwall = 1.0D0
  !! f-value for upper limit on radiation wall load (`constr. equ. 67`, `iteration variable 116`)

  real(dp) :: freinke = 1.0D0
  !! f-value for Reinke detachment criterion (`constr. equ. 78`, `iteration variable 147`)

  real(dp) :: frminor = 1.0D0
  !! f-value for minor radius limit (`constraint equation 21`, `iteration variable 32`)

  real(dp) :: fstrcase = 1.0D0
  !! f-value for maximum TF coil case TRESCA stress 
  !! (`constraint equation 31`, `iteration variable 48`)

  real(dp) :: fstrcond = 1.0D0
  !! f-value for maxiumum TF coil conduit TRESCA stress
  !! (`constraint equation 32`, `iteration variable 49`)

  real(dp) :: ftaucq = 1.0D0
  !! f-value for calculated minimum TF quench time 
  !! (`constraint equation 65`, `iteration variable 113`)

  real(dp) :: ftbr = 1.0D0
  !! f-value for minimum tritium breeding ratio (`constraint equation 52`, `iteration variable 89`)
  
  real(dp) :: ftburn = 1.0D0
  !! f-value for minimum burn time (`constraint equation 13`, `iteration variable 21`)

  real(dp) :: ftcycl = 1.0D0
  !! f-value for cycle time (`constraint equation 42`, `iteration variable 67`)

  real(dp) :: ftmargoh = 1.0D0
  !! f-value for central solenoid temperature margin
  !! (`constraint equation 60`, `iteration variable 106`)

  real(dp) :: ftmargtf = 1.0D0
  !! f-value for TF coil temperature margin (`constraint equation 36`, `iteration variable 54`)

  real(dp) :: ftohs = 1.0D0
  !! f-value for plasma current ramp-up time (`constraint equation 41`, `iteration variable 66`)

  real(dp) :: ftpeak = 1.0D0
  !! f-value for first wall peak temperature (`constraint equation 39`, `iteration variable 63`)
  
  real(dp) :: fvdump = 1.0D0
  !! f-value for dump voltage (`constraint equation 34`, `iteration variable 51`)

  real(dp) :: fvs = 1.0D0
  !! f-value for flux-swing (V-s) requirement (STEADY STATE)
  !! (`constraint equation 12`, `iteration variable 15`)

  real(dp) :: fvvhe = 1.0D0
  !! f-value for vacuum vessel He concentration limit (`iblanket = 2`)
  !! (`constraint equation 55`, `iteration variable 96`)

  real(dp) :: fwalld = 1.0D0
  !! f-value for maximum wall load (`constraint equation 8`, `iteration variable 14`)

  real(dp) :: fzeffmax = 1.0D0
  !! f-value for maximum zeff (`constraint equation 64`, `iteration variable 112`)

  real(dp) :: gammax = 2.0D0
  !! maximum current drive gamma (`constraint equation 37`)

  real(dp) :: maxradwallload = 1.0D0
  !!  Maximum permitted radiation wall load (MW/m^2) (`constraint equation 67`)

  real(dp) :: mvalim = 40.0D0
  !! maximum MVA limit (`constraint equation 19`)

  real(dp) :: nbshinefmax = 1.0D-3
  !! maximum neutral beam shine-through fraction (`constraint equation 59`)

  real(dp) :: nflutfmax = 1.0D23
  !! max fast neutron fluence on TF coil (n/m2) (`blktmodel>0`) (`constraint equation 53`)

  real(dp) :: pdivtlim = 150.0D0
  !! Minimum pdivt [MW] (`constraint equation 80`)
  
  real(dp) :: peakfactrad = 3.33D0
  !! peaking factor for radiation wall load (`constraint equation 67`)

  real(dp) :: peakradwallload = 0.0D0
  !! Peak radiation wall load (MW/m^2) (`constraint equation 67`)

  real(dp) :: pnetelin = 1.0D3
  !! required net electric power (MW) (`constraint equation 16`)

  real(dp) :: powfmax = 1.5D3
  !! maximum fusion power (MW) (`constraint equation 9`)

  real(dp) :: psepbqarmax = 9.5D0
  !! maximum ratio of Psep*Bt/qAR (MWT/m) (`constraint equation 68`)

  real(dp) :: pseprmax = 25.0D0
  !! maximum ratio of power crossing the separatrix to plasma major radius (Psep/R) (MW/m)
  !! (`constraint equation 56`)

  real(dp) :: ptfnucmax = 1.0D-3
  !! maximum nuclear heating in TF coil (MW/m3) (`constraint equation 54`)

  real(dp) :: tbrmin = 1.1D0
  !! minimum tritium breeding ratio (`constraint equation 52`)

  real(dp) :: tbrnmn = 1.0D0
  !! minimum burn time (s) (KE - no longer itv., see issue #706)

  real(dp) :: tcycmn = 0.0D0
  !! minimum cycle time (s) (`constraint equation 42`)

  real(dp) :: tohsmn = 1.0D0
  !! minimum plasma current ramp-up time (s) (`constraint equation 41`)

  real(dp) :: vvhealw = 1.0D0
  !! allowed maximum helium concentration in vacuum vessel at end of plant life (appm)
  !! (`iblanket =2`) (`constraint equation 55`)

  real(dp) :: walalw = 1.0D0
  !! allowable wall-load (MW/m2) (`constraint equation 8`)

  real(dp) :: taulimit = 5.0D0
  !! Lower limit on taup/taueff the ratio of alpha particle to energy confinement 
  !! times (`constraint equation 62`)

  real(dp) :: ftaulimit = 1.0D0
  !! f-value for lower limit on taup/taueff the ratio of alpha particle to energy 
  !! confinement times (`constraint equation 62`, `iteration variable 110`)

  real(dp) :: fniterpump = 1.0D0
  !! f-value for constraint that number of pumps < tfno 
  !! (`constraint equation 63`, `iteration variable 111`)

  real(dp) :: zeffmax = 3.6D0
  !! maximum value for Zeff (`constraint equation 64`)

  real(dp) :: fpoloidalpower = 1.0D0
  !! f-value for constraint on rate of change of energy in poloidal field
  !! (`constraint equation 66`, `iteration variable 115`)
  
  real(dp) :: fpsep = 1.0D0
  !! f-value to ensure separatrix power is less than value from Kallenbach divertor
  !! (Not required as constraint 69 is an equality)

  real(dp) :: fcqt = 1.0D0
  !! TF coil quench temparature remains below tmax_croco 
  !! (`constraint equation 74`, `iteration variable 141`)

end module constraint_variables

module stellarator_variables
  !! author: S. Muldrew (UKAEA), F. Warmer, J. Lion (IPP Greifswald)
  !!
  !! Module containing global variables relating to the stellarator model
  !!
  !!### References
  !!
  !! - Stellarator Plasma Geometry Model for the Systems Code PROCESS, F. Warmer, 19/06/2013
  !! - Stellarator Divertor Model for the Systems Code PROCESS, F. Warmer, 21/06/2013
  !! - Stellarator Coil Model for the Systems Code PROCESS, F. Warmer and F. Schauer, 07/10/2013

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  integer :: istell = 0
  !! Switch for stellarator option (set via `device.dat`):
  !!
  !! - =0 use tokamak model
  !! - =1 use stellarator model: Helias5-b
  !! - =2 use stellarator model: Helias4-b
  !! - =3 use stellarator model: Helias3-b

  real(dp) :: bmn = 1.0D-3
  !! relative radial field perturbation

  real(dp) :: f_asym = 1.0D0
  !! divertor heat load peaking factor

  real(dp) :: f_rad = 0.85D0
  !! radiated power fraction in SOL

  real(dp) :: f_w = 0.5D0
  !! island size fraction factor

  real(dp) :: fdivwet = 0.333333333333333D0
  !! wetted fraction of the divertor area

  real(dp) :: flpitch = 1.0D-3
  !! field line pitch (rad)

  real(dp) :: hportamax = 0.0D0
  !! maximum available area for horizontal ports (m2)

  real(dp) :: hportpmax = 0.0D0
  !! maximum available poloidal extent for horizontal ports (m)

  real(dp) :: hporttmax = 0.0D0
  !! maximum available toroidal extent for horizontal ports (m)

  real(dp) :: iotabar = 1.0D0
  !! rotational transform (reciprocal of tokamak q) for stellarator confinement time scaling laws

  integer :: isthtr = 3
  !! Switch for stellarator auxiliary heating method:
  !!
  !! - = 1electron cyclotron resonance heating
  !! - = 2lower hybrid heating
  !! - = 3neutral beam injection

  integer :: m_res = 5
  !! poloidal resonance number

  integer :: n_res = 5
  !! toroidal resonance number

  real(dp) :: shear = 0.5D0
  !! magnetic shear, derivative of iotabar

  real(dp) :: vportamax = 0.0D0
  !! maximum available area for vertical ports (m2)

  real(dp) :: vportpmax = 0.0D0
  !! maximum available poloidal extent for vertical ports (m)

  real(dp) :: vporttmax = 0.0D0
  !! maximum available toroidal extent for vertical ports (m)
  
end module stellarator_variables

module pulse_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the pulsed reactor model
  !!
  !!### References
  !!
  !! - Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: bctmp = 320.0D0
  !! first wall bulk coolant temperature (C)

  real(dp) :: bfw = 0.0D0
  !! outer radius of each first wall structural tube (m) (0.5 * average of fwith and fwoth)

  real(dp) :: dtstor = 300.0D0
  !! maximum allowable temperature change in stainless steel thermal storage block (K) (`istore=3`)

  integer :: istore = 1
  !! Switch for thermal storage method:
  !!
  !! - =1 option 1 of Electrowatt report, AEA FUS 205
  !! - =2 option 2 of Electrowatt report, AEA FUS 205
  !! - =3 stainless steel block

  integer :: itcycl = 1
  !! Switch for first wall axial stress model:
  !!
  !! - =1 total axial constraint, no bending
  !! - =2 no axial constraint, no bending
  !! - =3 no axial constraint, bending

  integer :: lpulse = 0
  !! Switch for reactor model:
  !!
  !! - =0 continuous operation
  !! - =1 pulsed operation

end module pulse_variables

module startup_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the plasma start-up model
  !!
  !!### References
  !!
  !! - Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: ftaue = 0.0D0
  !! factor in energy confinement time formula

  real(dp) :: gtaue  = 0.0D0
  !! offset term in energy confinement time scaling

  real(dp) :: nign  = 0.0D0
  !! electron density at ignition (start-up) (/m3)

  real(dp) :: ptaue  = 0.0D0
  !! exponent for density term in energy confinement time formula

  real(dp) :: qtaue  = 0.0D0
  !! exponent for temperature term in energy confinement time formula

  real(dp) :: rtaue  = 0.0D0
  !! exponent for power term in energy confinement time formula

  real(dp) :: tign  = 0.0D0
  !! electron temperature at ignition (start-up) (keV)

end module startup_variables

module fispact_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the fispact routines
  !!
  !!### References
  !!
  !! -AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  !! Fispact arrays with 3 elements contain the results at the following times:
  !!
  !! - (1) - at end of component life
  !! - (2) - after 3 months cooling time
  !! - (3) - 100 years after end of plant life

  real(dp), dimension(3) :: bliact = 0.0D0
  !! inboard blanket total activity (Bq)

  real(dp), dimension(3) :: bligdr = 0.0D0
  !! inboard blanket total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: blihkw = 0.0D0
  !! inboard blanket total heat output (kW)

  real(dp) :: bliizp = 0.0D0
  !! inboard blanket integrated zone power / neutron

  real(dp) :: blimzp = 0.0D0
  !! inboard blanket mean zone power density / neutron

  real(dp), dimension(3) :: bloact = 0.0D0
  !! outboard blanket total activity (Bq)

  real(dp), dimension(3) :: blogdr = 0.0D0
  !! outboard blanket total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: blohkw = 0.0D0
  !! outboard blanket total heat output (kW)

  real(dp) :: bloizp = 0.0D0
  !! outboard blanket integrated zone power / neutron

  real(dp) :: blomzp = 0.0D0
  !! outboard blanket mean zone power density / neutron

  real(dp), dimension(3) :: fwiact = 0.0D0
  !! inboard first wall total activity (Bq)

  real(dp), dimension(3) :: fwigdr = 0.0D0
  !! inboard first wall total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: fwihkw = 0.0D0
  !! inboard first wall total heat output (kW)

  real(dp) :: fwiizp = 0.0D0
  !! inboard first wall integrated zone power / neutron

  real(dp) :: fwimzp = 0.0D0
  !! inboard first wall mean zone power density/neutron

  real(dp), dimension(3) :: fwoact = 0.0D0
  !! outboard first wall total activity (Bq)

  real(dp), dimension(3) :: fwogdr = 0.0D0
  !! outboard first wall total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: fwohkw = 0.0D0
  !! outboard first wall total heat output (kW)

  real(dp) :: fwoizp = 0.0D0
  !! outboard first wall integrated zone power / neutron

  real(dp) :: fwomzp = 0.0D0
  !! outboard first wall mean zone power density/neutron

  real(dp) :: fwtemp = 0.0D0
  !! outboard first wall temperature after a LOCA (K)

end module fispact_variables

module rebco_variables
  !! author: M. Kovari
  !! 
  !! Module for the REBCO HTS superconductor variables
  !!
  !! Variables relating to the REBCO HTS tape, strand and conductor
  !! Conduit information is in the modules relating to each coil.
  !!
  !!### References
  !!
  !! - Updated 13/11/18 using data from Lewandowska et al 2018.

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  real(dp) :: rebco_thickness = 1.0D-6
  !! thickness of REBCO layer in tape (m) (`iteration variable 138`)

  real(dp) :: copper_thick = 100.0D-6
  !! thickness of copper layer in tape (m) (`iteration variable 139`)

  real(dp) :: hastelloy_thickness = 50.0D-6
  !! thickness of Hastelloy layer in tape (m)

  real(dp) :: tape_width = 0.0D0
  !! Mean width of tape (m)
  
  real(dp) :: croco_od = 0.0D0
  !! Outer diameter of CroCo strand (m)

  real(dp) :: croco_id = 0.0D0
  !! Inner diameter of CroCo copper tube (m)

  real(dp) :: croco_thick = 2.5D-3
  !! Thickness of CroCo copper tube (m) (`iteration variable 158`)
  
  real(dp) :: copper_rrr = 100d0
  !! residual resistivity ratio copper in TF superconducting cable

  real(dp) :: coppera_m2_max = 1D8
  !! Maximum TF coil current / copper area (A/m2)

  real(dp) :: f_coppera_m2 = 1d0
  !! f-value for constraint 75: TF coil current / copper area < copperA_m2_max

  !#TODO: variables need descriptions and units
  real(dp) :: tape_thickness
  real(dp) :: stack_thickness
  real(dp) :: tapes
  real(dp) :: rebco_area
  real(dp) :: copper_area
  real(dp) :: hastelloy_area
  real(dp) :: solder_area
  real(dp) :: croco_area
  real(dp) :: copperA_m2       
  !! TF coil current / copper area (A/m2) 

end module rebco_variables

module resistive_materials
  !! author: M. Kovari
  !!  
  !! Variables relating to resistive materials in superconducting conductors
  
  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  type resistive_material
    real(dp) :: cp            
    !! Specific heat capacity J/(K kg)

    real(dp) :: rrr           
    !! Residual resistivity ratio

    real(dp) :: resistivity   
    !! Resistivity [ohm.m]

    real(dp) :: density       
    !! kg/m3

    real(dp) :: cp_density    
    !! Cp x density J/K/m3
  end type resistive_material

  type supercon_strand
     real(dp) :: area
     !! Superconducting strand area [m2]

     real(dp) :: critical_current
     !! Superconducting strand critical current [A]
  end type supercon_strand

  !#TODO: variables need descriptions
  type volume_fractions
    real(dp) :: copper_area,  copper_fraction
    real(dp) :: copper_bar_area
    real(dp) :: hastelloy_area, hastelloy_fraction
    real(dp) :: helium_area, helium_fraction
    real(dp) :: solder_area, solder_fraction
    real(dp) :: jacket_area, jacket_fraction
    real(dp) :: rebco_area,  rebco_fraction
    real(dp) :: critical_current
    real(dp) :: acs                  
    !! Area of cable space inside jacket
    real(dp) :: area
  end type volume_fractions
end module resistive_materials

module reinke_variables
  !! author: S. Muldrew (UKAEA)
  !!
  !! This module contains global variables relating to the minimum impurity fraction 
  !! for detached divertor conditions Reinke criterion. It furthermore uses 
  !! several parameters from Kallenbach model like netau and empurity_enrichment.
  !!
  !!### References
  !!
  !! - M.L. Reinke 2017 Nucl. Fusion 57 034004

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  integer       :: impvardiv = 9
  !! Index of impurity to be iterated for Reinke divertor detachment criterion

  real(dp) :: lhat = 4.33D0
  !! Connection length factor L|| = lhat qstar R for Reinke criterion, default value from
  !! Post et al. 1995 J. Nucl. Mat.  220-2 1014

  real(dp) :: fzmin = 0.0D0
  !! Minimum impurity fraction necessary for detachment. This is the impurity at the SOL/Div.

  real(dp) :: fzactual = 0.001D0
  !! Actual impurity fraction of divertor impurity (impvardiv) in the SoL (taking 
  !! impurity_enrichment into account) (`iteration variable 148`)

  integer       :: reinke_mode = 0
  !! Switch for Reinke criterion H/I mode:
  !!
  !! - =0 H-mode
  !! - =1 I-mode

end module reinke_variables
