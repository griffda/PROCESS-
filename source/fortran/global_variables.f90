! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module global_variables
  !! author: J. Morris (UKAEA)
  !!
  !! This module contains miscellaneous global variables not well-suited to any 
  !! of the other 'variables' modules.
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none
  
  public

  character(len=48) :: icase = 'Steady-state tokamak model'
  !! power plant type

  character(len=180) :: runtitle = "Run Title (change this line using input variable 'runtitle')"
  !! short descriptive title for the run

  integer :: verbose = 0
  !! switch for turning on/off diagnostic messages
  !!
  !! - =0 turn off diagnostics
  !! - =1 turn on diagnostics

  integer :: run_tests = 0
  !! turns on built-in tests if set to 1

  integer :: maxcal = 200
  !! maximum number of VMCON iterations

  character(len=200) :: fileprefix = "" 
  !! input file prefix

  character(len=200) :: output_prefix = ""
  !! output file prefix

  character(len=25) :: xlabel
  !! scan parameter description label

  character(len=25) :: vlabel
  !! scan value name label
  
  character(len=25) :: xlabel_2
  !! scan parameter description label (2nd dimension)

  character(len=25) :: vlabel_2
  !! scan value name label (2nd dimension)

  integer :: iscan_global=0
  !! Makes iscan available globally.

  real(dp):: convergence_parameter
  !! VMCON convergence parameter "sum"

end module global_variables

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

  real(dp), parameter :: n_day_year = 365.2425D0
  !! Average number of days in a year

end module constants

module physics_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the plasma physics
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  integer, parameter :: ipnlaws = 48
  !! number of energy confinement time scaling laws

  real(dp) :: abeam = 0.0D0
  !! beam ion mass (amu)

  real(dp), bind(C) :: afuel = 0.0D0
  !! average mass of fuel portion of ions (amu)

  real(dp) :: aion = 0.0D0
  !! average mass of all ions (amu)

  real(dp) :: alphaj = 1.0D0
  !! current profile index (calculated from q_0, q if `iprofile=1`)

  real(dp) :: alphan = 0.25D0
  !! density profile index

  real(dp) :: alphap = 0.0D0
  !! pressure profile index

  real(dp) :: alpharate = 0.0D0
  !! alpha particle production rate (particles/m3/sec)

  real(dp) :: alphat = 0.5D0
  !! temperature profile index

  real(dp) :: aspect = 2.907D0
  !! aspect ratio (`iteration variable 1`)

  real(dp) :: beamfus0 = 1.0D0
  !! multiplier for beam-background fusion calculation

  real(dp), bind(C) :: beta = 0.042D0
  !! total plasma beta (`iteration variable 5`) (calculated if `ipedestal=3` or stellarator)

  real(dp) :: betaft = 0.0D0
  !! fast alpha beta component

  real(dp) :: betalim = 0.0D0
  !! allowable beta

  real(dp) :: betalim_lower = 0.0D0
  !! allowable lower beta

  real(dp) :: betanb = 0.0D0
  !! neutral beam beta component

  real(dp) :: betap = 0.0D0
  !! poloidal beta

  real(dp), bind(C) :: normalised_total_beta = 0.0D0
  !! normaised total beta

  real(dp) :: betbm0 = 1.5D0
  !! leading coefficient for NB beta fraction

  real(dp), bind(C) :: bp = 0.0D0
  !! poloidal field (T)

  real(dp), bind(C) :: bt = 5.68D0
  !! toroidal field on axis (T) (`iteration variable 2`)

  real(dp), bind(C) :: btot = 0.0D0
  !! total toroidal + poloidal field (T)

  real(dp) :: burnup = 0.0D0
  !! fractional plasma burnup

  real(dp) :: bvert = 0.0D0
  !! vertical field at plasma (T)

  real(dp) :: csawth = 1.0D0
  !! coeff. for sawteeth effects on burn V-s requirement

  real(dp) :: cvol = 1.0D0
  !! multiplying factor times plasma volume (normally=1)

  real(dp) :: cwrmax = 1.35D0
  !! maximum ratio of conducting wall distance to plasma minor radius for 
  !! vertical stability (`constraint equation 23`)

  real(dp) :: dene = 9.8D19
  !! electron density (/m3) (`iteration variable 6`) (calculated if `ipedestal=3`)

  real(dp) :: deni = 0.0D0
  !! fuel ion density (/m3)

  real(dp) :: dlamee = 0.0D0
  !! electron-electron coulomb logarithm

  real(dp) :: dlamie = 0.0D0
  !! ion-electron coulomb logarithm

  real(dp), dimension(7) :: dlimit = 0.0D0
  !! density limit (/m3) as calculated using various models

  real(dp) :: dnalp = 0.0D0
  !! thermal alpha density (/m3)

  real(dp) :: dnbeam = 0.0D0
  !! hot beam ion density, variable (/m3)

  real(dp) :: dnbeam2 = 0.0D0
  !! hot beam ion density from calculation (/m3)

  real(dp) :: dnbeta = 3.5D0
  !! Troyon-like coefficient for beta scaling calculated 
  !! as 4*rli if `iprofile=1` (see also gtscale option)

  real(dp) :: dnelimt = 0.0D0
  !! density limit (/m3)

  real(dp) :: dnitot = 0.0D0
  !! total ion density (/m3)

  real(dp) :: dnla = 0.0D0
  !! line averaged electron density (/m3)

  real(dp) :: dnprot = 0.0D0
  !! proton ash density (/m3)

  real(dp) :: dntau = 0.0D0
  !! plasma average "n-tau" (seconds/m3)

  real(dp) :: dnz = 0.0D0
  !! high Z ion density (/m3)

  real(dp), parameter :: ealphadt = 3520.0D0
  !! alpha birth energy in D-T reaction (keV)

  real(dp) :: epbetmax = 1.38D0
  !! maximum (eps*beta_poloidal) (`constraint equation 6`). Note: revised issue #346
  !! "Operation at the tokamak equilibrium poloidal beta-limit in TFTR", 1992 Nucl. Fusion 32 1468

  real(dp) :: eps = 0.34399724802D0
  !! inverse aspect ratio

  real(dp) :: faccd = 0.0D0
  !! fraction of plasma current produced by auxiliary current drive

  real(dp) :: facoh = 0.0D0
  !! fraction of plasma current produced inductively

  real(dp) :: falpe = 0.0D0
  !! fraction of alpha energy to electrons

  real(dp) :: falpha = 0.95D0
  !! fraction of alpha power deposited in plasma (Physics of Energetic Ions, p.2489)

  real(dp) :: falpi = 0.0D0
  !! fraction of alpha power to ions

  real(dp) :: fdeut = 0.5D0
  !! deuterium fuel fraction

  real(dp) :: ftar = 1.0D0
  !! fraction of power to the lower divertor in double null configuration 
  !! (`i_single_null = 0` only) (default assumes SN)

  real(dp) :: ffwal = 0.92D0
  !! factor to convert plasma surface area to first wall area in neutron wall 
  !! load calculation (`iwalld=1`)

  real(dp) :: fgwped = 0.85D0
  !! fraction of Greenwald density to set as pedestal-top density. If `<0`, pedestal-top 
  !! density set manually using neped (`ipedestal>=1`). Needs to be `>0` if `ipedestal = 3`.
  !! (`iteration variable 145`)

  real(dp) :: fgwsep = 0.50D0
  !! fraction of Greenwald density to set as separatrix density. If `<0`, separatrix 
  !! density set manually using nesep (`ipedestal>=1`). Needs to be `>0` if `ipedestal = 3`.
  !! (`iteration variable 152`)

  real(dp) :: fhe3 = 0.0D0
  !! helium-3 fuel fraction

  real(dp) :: figmer = 0.0D0
  !! physics figure of merit (= plascur*aspect**sbar, where `sbar=1`)

  real(dp) :: fkzohm = 1.0D0
  !! Zohm elongation scaling adjustment factor (`ishape=2, 3`)
  
  real(dp) :: fplhsep = 1.0D0
  !! F-value for Psep >= Plh + Paux (`constraint equation 73`)
  
  real(dp) :: fpdivlim = 1.0D0
  !! F-value for minimum pdivt (`constraint equation 80`)

  real(dp) :: fne0 = 1.0D0
  !! f-value for the constraint ne(0) > ne(sep) (`constraint equation 81`)
  !! (`Iteration variable 154`) 

  real(dp), bind(C) :: ftrit = 0.5D0
  !! tritium fuel fraction

  real(dp) :: fusionrate = 0.0D0
  !! fusion reaction rate (reactions/m3/sec)

  real(dp) :: fvsbrnni = 1.0D0
  !! fraction of the plasma current produced by non-inductive means (`iteration variable 44`)

  real(dp) :: gamma = 0.4D0
  !! Ejima coefficient for resistive startup V-s formula

  real(dp) :: gammaft = 0.0D0
  !! ratio of (fast alpha + neutral beam beta) to thermal beta

  integer :: gtscale = 0
  !! switch for a/R scaling of dnbeta (`iprofile=0` only):
  !! 
  !! - =0 do not scale dnbeta with eps
  !! - =1 scale dnbeta with eps

  real(dp), dimension(ipnlaws) :: hfac = 0.0D0
  !! H factors for an ignited plasma for each energy confinement time scaling law

  real(dp) :: hfact = 1.0D0
  !! H factor on energy confinement times, radiation corrected (`iteration variable 10`). If 
  !! `ipedestal=2,3` and `hfact=0`, not used in PLASMOD (see also `plasmod_i_modeltype`) issue #219

  real(dp) :: taumax = 10.0D0
  !! Maximum allowed energy confinement time (s)

  integer :: ibss = 3
  !! switch for bootstrap current scaling
  !!
  !! - =1 ITER 1989 bootstrap scaling (high R/a only)
  !! - =2 for Nevins et al general scaling
  !! - =3 for Wilson et al numerical scaling
  !! - =4 for Sauter et al scaling

  integer :: iculbl = 0
  !! switch for beta limit scaling (`constraint equation 24`)
  !!
  !! - =0 apply limit to total beta
  !! - =1 apply limit to thermal beta
  !! - =2 apply limit to thermal + neutral beam beta

  integer :: icurr = 4
  !! switch for plasma current scaling to use
  !!
  !! - =1 Peng analytic fit
  !! - =2 Peng double null divertor scaling (ST)
  !! - =3 simple ITER scaling (k = 2.2, d = 0.6)
  !! - =4 later ITER scaling, a la Uckan
  !! - =5 Todd empirical scaling I
  !! - =6 Todd empirical scaling II
  !! - =7 Connor-Hastie model
  !! - =8 Sauter scaling allowing negative triangularity
  !! - =9 FIESTA ST fit

  integer :: idia = 0
  !! switch for diamagnetic current scaling
  !!
  !! - =0 Do not calculate
  !! - =1 Use original TART scaling
  !! - =2 Use SCENE scaling

  integer :: idensl = 7
  !! switch for density limit to enforce (`constraint equation 5`)
  !!
  !! - =1 old ASDEX
  !! - =2 Borrass model for ITER (I)
  !! - =3 Borrass model for ITER (II)
  !! - =4 JET edge radiation
  !! - =5 JET simplified
  !! - =6 Hugill-Murakami Mq limit
  !! - =7 Greenwald limit

  integer :: idivrt = 2
  !! number of divertors (calculated from `i_single_null`)

  integer :: ifalphap = 1
  !! switch for fast alpha pressure calculation
  !!
  !! - =0 ITER physics rules (Uckan) fit
  !! - =1 Modified fit (D. Ward) - better at high temperature

  integer :: ifispact = 0
  !! switch for neutronics calculations:
  !!
  !! - =0 neutronics calculations turned off
  !! - =1 neutronics calculations turned on

  integer :: igeom = 1
  !! switch for plasma geometry calculation:
  !!
  !! - =0 original method (possibly based on Peng ST modelling)
  !! - =1 improved (and traceable) method

  integer :: ignite = 0
  !! switch for ignition assumption. Obviously, ignite must be zero if current drive 
  !! is required. If ignite is 1, any auxiliary power is assumed to be used only during 
  !! plasma start-up, and is excluded from all steady-state power balance calculations.
  !!
  !! - =0 do not assume plasma ignition
  !! - =1 assume ignited (but include auxiliary power in costs)</UL
  
  integer :: iinvqd = 1
  !! switch for inverse quadrature in L-mode scaling laws 5 and 9:
  !!
  !! - =0 inverse quadrature not used
  !! - =1 inverse quadrature with Neo-Alcator tau-E used

  integer :: ipedestal = 1
  !! switch for pedestal profiles:
  !!
  !! - =0 use original parabolic profiles
  !! - =1 use pedestal profile
  !! - =2 use pedestal profiles and run PLASMOD on final outpu
  !! - =3 use PLASMOD transport model only to calculate pedestal profiles

  integer :: ips = 0
  !! switch for Pfirsch-Schlüter current scaling (issue #413):
  !!
  !! - =0 Do not calculate
  !! - =1 Use SCENE scaling

  integer :: ieped = 0
  !! switch for scaling pedestal-top temperature with plasma parameters (issue #730):
  !!
  !! - =0 set pedestal-top temperature manually using teped
  !! - =1 set pedestal-top temperature using EPED scaling (PLASMOD implementation 
  !!   of scaling within PLASMOD, `ipedestal =2,3 (ttps://idm.euro-fusion.org/?uid=2MSZ4T)

  real(dp), bind(C) :: eped_sf = 1.0D0
  !! Adjustment factor for EPED scaling to reduce pedestal temperature or pressure 
  !! to mitigate or prevent ELMs

  real(dp) :: neped = 4.0D19
  !! electron density of pedestal [m-3] (`ipedestal=1,2, calculated if 3`)

  real(dp) :: nesep = 3.0D19
  !! electron density at separatrix [m-3] (`ipedestal=1,2, calculated if 3`)

  real(dp) :: alpha_crit = 0.0D0
  !! critical ballooning parameter value

  real(dp) :: nesep_crit = 0.0D0
  !! critical electron density at separatrix [m-3]

  real(dp) :: plasma_res_factor = 1.0D0
  !! plasma resistivity pre-factor

  real(dp) :: rhopedn = 1.0D0
  !! r/a of density pedestal (`ipedestal>=1`)

  real(dp) :: rhopedt = 1.0D0
  !! r/a of temperature pedestal (`ipedestal>=1`)

  real(dp) :: tbeta = 2.0D0
  !! temperature profile index beta  (`ipedestal=1,2`)

  real(dp) :: teped = 1.0D0
  !! electron temperature of pedestal (keV) (`ipedestal>=1, ieped=0, calculated for ieped=1`)

  real(dp) :: tesep = 0.1D0
  !! electron temperature at separatrix (keV) (`ipedestal>=1`) calculated if reinke 
  !! criterion is used (`icc=78`)

  integer :: iprofile = 1
  !! switch for current profile consistency:
  !!
  !! - =0 use input values for alphaj, rli, dnbeta (but see gtscale option)
  !! - =1 make these consistent with input q, q_0 values (recommend `icurr=4` with this option)

  integer :: iradloss = 1
  !! switch for radiation loss term usage in power balance (see User Guide):
  !!
  !! - =0 total power lost is scaling power plus radiation (needed for `ipedestal=2,3`)
  !! - =1 total power lost is scaling power plus core radiation only
  !! - =2 total power lost is scaling power only, with no additional
  !!   allowance for radiation. This is not recommended for power plant models.

  integer :: isc = 34
  !! switch for energy confinement time scaling law (see description in `tauscl`)

  character(len=24), dimension(ipnlaws) :: tauscl = (/ &
  !! tauscl(ipnlaws) : labels describing energy confinement scaling laws:<UL>
    'Neo-Alcator      (ohmic)', &
    !! <LI> ( 1)  Neo-Alcator (ohmic)
    'Mirnov               (H)', &
    !! <LI> ( 2)  Mirnov (H-mode)
    'Merezkhin-Muhkovatov (L)', &
    !! <LI> ( 3)  Merezkhin-Muhkovatov (L-mode)
    'Shimomura            (H)', &
    !! <LI> ( 4)  Shimomura (H-mode)
    'Kaye-Goldston        (L)', &
    !! <LI> ( 5)  Kaye-Goldston (L-mode)
    'ITER 89-P            (L)', &
    !! <LI> ( 6)  ITER 89-P (L-mode)
    'ITER 89-O            (L)', &
    !! <LI> ( 7)  ITER 89-O (L-mode)
    'Rebut-Lallia         (L)', &
    !! <LI> ( 8)  Rebut-Lallia (L-mode)
    'Goldston             (L)', &
    !! <LI> ( 9)  Goldston (L-mode)
    'T10                  (L)', &
    !! <LI> (10)  T10 (L-mode)
    'JAERI-88             (L)', &
    !! <LI> (11)  JAERI-88 (L-mode)
    'Kaye-Big Complex     (L)', &
    !! <LI> (12)  Kaye-Big Complex (L-mode)
    'ITER H90-P           (H)', &
    !! <LI> (13)  ITER H90-P (H-mode)
    'ITER Mix             (L)', &
    !! <LI> (14)  ITER Mix (L-mode)
    'Riedel               (L)', &
    !! <LI> (15)  Riedel (L-mode)
    'Christiansen         (L)', &
    !! <LI> (16)  Christiansen (L-mode)
    'Lackner-Gottardi     (L)', &
    !! <LI> (17)  Lackner-Gottardi (L-mode)
    'Neo-Kaye             (L)', &
    !! <LI> (18)  Neo-Kaye (L-mode)
    'Riedel               (H)', &
    !! <LI> (19)  Riedel (H-mode)
    'ITER H90-P amended   (H)', &
    !! <LI> (20)  ITER H90-P amended (H-mode)
    'LHD              (stell)', &
    !! <LI> (21)  LHD (stellarator)
    'Gyro-reduced Bohm(stell)', &
    !! <LI> (22)  Gyro-reduced Bohm (stellarator)
    'Lackner-Gottardi (stell)', &
    !! <LI> (23)  Lackner-Gottardi (stellarator)
    'ITER-93H             (H)', &
    !! <LI> (24)  ITER-93H (H-mode)
    'TITAN RFP OBSOLETE      ', &
    !! <LI> (25) OBSOLETE
    'ITER H-97P ELM-free  (H)', &
    !! <LI> (26)  ITER H-97P ELM-free (H-mode)
    'ITER H-97P ELMy      (H)', &
    !! <LI> (27)  ITER H-97P ELMy (H-mode)
    'ITER-96P             (L)', &
    !! <LI> (28)  ITER-96P (=ITER-97L) (L-mode)
    'Valovic modified ELMy(H)', &
    !! <LI> (29)  Valovic modified ELMy (H-mode)
    'Kaye PPPL April 98   (L)', &
    !! <LI> (30)  Kaye PPPL April 98 (L-mode)
    'ITERH-PB98P(y)       (H)', &
    !! <LI> (31)  ITERH-PB98P(y) (H-mode)
    'IPB98(y)             (H)', &
    !! <LI> (32)  IPB98(y) (H-mode)
    'IPB98(y,1)           (H)', &
    !! <LI> (33)  IPB98(y,1) (H-mode)
    'IPB98(y,2)           (H)', &
    !! <LI> (34)  IPB98(y,2) (H-mode)
    'IPB98(y,3)           (H)', &
    !! <LI> (35)  IPB98(y,3) (H-mode)
    'IPB98(y,4)           (H)', &
    !! <LI> (36)  IPB98(y,4) (H-mode)
    'ISS95            (stell)', &
    !! <LI> (37)  ISS95 (stellarator)
    'ISS04            (stell)', &
    !! <LI> (38)  ISS04 (stellarator)
    'DS03                 (H)', &
    !! <LI> (39)  DS03 (H-mode)
    'Murari et al NPL     (H)', &
    !! <LI> (40)  Murari et al non-power law (H-mode)
    'Petty 2008           (H)', &
    !! <LI> (41)  Petty 2008 (H-mode)
    'Lang et al. 2012     (H)', &
    !! <LI> (42)  Lang et al. 2012 (H-mode)
    'Hubbard 2017 - nom   (I)', &
    !! <LI> (43)  Hubbard 2017 (I-mode) - nominal
    'Hubbard 2017 - lower (I)', &
    !! <LI> (44)  Hubbard 2017 (I-mode) - lower bound
    'Hubbard 2017 - upper (I)', &
    !! <LI> (45)  Hubbard 2017 (I-mode) - upper bound
    'NSTX (Spherical)     (H)', &
    !! <LI> (46)  NSTX (H-mode; Spherical tokamak)
    'NSTX-Petty08 Hybrid  (H)', &
    !! <LI> (47)  NSTX-Petty08 Hybrid (H-mode)
    'Input tauee_in          ' /)
    !! <LI> (48)  Use input tauee_in </UL>

  integer :: iscrp = 1
  !! switch for plasma-first wall clearances:
  !!
  !! - =0 use 10% of rminor
  !! - =1 use input (scrapli and scraplo)

  integer :: ishape = 0
  !! switch for plasma cross-sectional shape calculation:
  !!
  !! - =0 use input kappa, triang to calculate 95% values
  !! - =1 scale qlim, kappa, triang with aspect ratio (ST)
  !! - =2 set kappa to the natural elongation value (Zohm ITER scaling), triang input
  !! - =3 set kappa to the natural elongation value (Zohm ITER scaling), triang95 input
  !! - =4 use input kappa95, triang95 to calculate separatrix values
  !! - =5 use input kappa95, triang95 to calculate separatrix values based on MAST scaling (ST)
  !! - =6 use input kappa, triang to calculate 95% values based on MAST scaling (ST)
  !! - =7 use input kappa95, triang95 to calculate separatrix values based on fit to FIESTA (ST)
  !! - =8 use input kappa, triang to calculate 95% values based on fit to FIESTA (ST)


  integer, bind(C) :: itart = 0
  !! switch for spherical tokamak (ST) models:
  !!
  !! - =0 use conventional aspect ratio models
  !! - =1 use spherical tokamak models

  integer :: itartpf = 0
  !! switch for Spherical Tokamak PF models:
  !!
  !! - =0 use Peng and Strickler (1986) model
  !! - =1 use conventional aspect ratio model

  integer :: iwalld = 1
  !! switch for neutron wall load calculation:
  !!
  !! - =1 use scaled plasma surface area
  !! - =2 use first wall area directly

  real(dp), bind(C) :: kappa = 1.792D0
  !! plasma separatrix elongation (calculated if `ishape = 1-5 or 7`)

  real(dp), bind(C) :: kappa95 = 1.6D0
  !! plasma elongation at 95% surface (calculated if `ishape = 0-3, 6 or 8`)

  real(dp) :: kappaa = 0.0D0
  !! plasma elongation calculated as xarea/(pi.a^2)

  real(dp) :: kappaa_IPB = 0.d0
  !! Volume measure of plasma elongation

  real(dp) :: ne0 = 0.0D0
  !! central electron density (/m3)

  real(dp) :: ni0 = 0.0D0
  !! central ion density (/m3)

  real(dp) :: p0 = 0.0D0
  !! central total plasma pressure (Pa)

  real(dp) :: palppv = 0.0D0
  !! alpha power per volume (MW/m3)
  
  real(dp) :: palpepv = 0.0D0
  !! alpha power per volume to electrons (MW/m3)

  real(dp) :: palpfwmw = 0.0D0
  !! alpha power escaping plasma and reaching first wall (MW)

  real(dp) :: palpipv = 0.0D0
  !! alpha power per volume to ions (MW/m3)

  real(dp) :: palpmw = 0.0D0
  !! alpha power (MW)

  real(dp) :: palpnb = 0.0D0
  !! alpha power from hot neutral beam ions (MW)

  real(dp) :: pbrempv = 0.0D0
  !! bremsstrahlung power per volume (MW/m3)

  real(dp) :: pchargemw = 0.0D0
  !! non-alpha charged particle fusion power (MW)

  real(dp) :: pchargepv = 0.0D0
  !! non-alpha charged particle fusion power per volume (MW/m3)

  real(dp) :: pcoef = 0.0D0
  !! profile factor (= n-weighted T / average T)

  real(dp) :: pcoreradmw = 0.0D0
  !! total core radiation power (MW)

  real(dp) :: pcoreradpv = 0.0D0
  !! total core radiation power per volume (MW/m3)

  real(dp) :: pdd = 0.0D0
  !! deuterium-deuterium fusion power (MW)

  real(dp) :: pdhe3 = 0.0D0
  !! deuterium-helium3 fusion power (MW)

  real(dp) :: pdivt = 0.0D0
  !! power to conducted to the divertor region (MW)

  real(dp) :: pdivl = 0.0D0
  !! power conducted to the lower divertor region (calculated if `i_single_null = 0`) (MW)

  real(dp) :: pdivu = 0.0D0
  !! power conducted to the upper divertor region (calculated if `i_single_null = 0`) (MW)

  real(dp) :: pdivmax = 0.0D0
  !! power conducted to the divertor with most load (calculated if `i_single_null = 0`) (MW)

  real(dp) :: pdt = 0.0D0
  !! deuterium-tritium fusion power (MW)

  real(dp) :: pedgeradmw = 0.0D0
  !! edge radiation power (MW)

  real(dp) :: pedgeradpv = 0.0D0
  !! edge radiation power per volume (MW/m3)

  real(dp) :: pfuscmw = 0.0D0
  !! charged particle fusion power (MW)

  real(dp) :: phiint = 0.0D0
  !! internal plasma V-s

  real(dp) :: photon_wall = 0.0D0
  !! Nominal mean radiation load on inside surface of reactor (MW/m2)

  real(dp) :: piepv = 0.0D0
  !! ion/electron equilibration power per volume (MW/m3)

  real(dp), bind(C) :: plascur = 0.0D0
  !! plasma current (A)

  real(dp) :: plinepv = 0.0D0
  !! line radiation power per volume (MW/m3)

  real(dp) :: pneutmw = 0.0D0
  !! neutron fusion power (MW)

  real(dp) :: pneutpv = 0.0D0
  !! neutron fusion power per volume (MW/m3)

  real(dp) :: pohmmw = 0.0D0
  !! ohmic heating power (MW)

  real(dp) :: pohmpv = 0.0D0
  !! ohmic heating power per volume (MW/m3)

  real(dp) :: powerht = 0.0D0
  !! heating power (= transport loss power) (MW) used in confinement time calculation

  real(dp), bind(C) :: powfmw = 0.0D0
  !! fusion power (MW)

  real(dp) :: pperim = 0.0D0
  !! plasma poloidal perimeter (m)

  real(dp) :: pradmw = 0.0D0
  !! total radiation power (MW)

  real(dp) :: pradpv = 0.0D0
  !! total radiation power per volume (MW/m3)

  real(dp) :: protonrate = 0.0D0
  !! proton production rate (particles/m3/sec)

  real(dp) :: psolradmw = 0.0D0
  !! SOL radiation power (MW) (`stellarator only`)

  real(dp) :: psyncpv = 0.0D0
  !! synchrotron radiation power per volume (MW/m3)

  integer :: ilhthresh = 19
  !! switch for L-H mode power threshold scaling to use (see pthrmw for list)

  real(dp) :: plhthresh = 0.0D0
  !! L-H mode power threshold (MW) (chosen via ilhthresh, and enforced if 
  !! constraint equation 15 is on)

  real(dp), dimension(21) :: pthrmw = 0.0D0
  !! L-H power threshold for various scalings (MW)
  !!
  !! - =1 ITER 1996 scaling: nominal
  !! - =2 ITER 1996 scaling: upper bound
  !! - =3 ITER 1996 scaling: lower bound
  !! - =4 ITER 1997 scaling: excluding elongation
  !! - =5 ITER 1997 scaling: including elongation
  !! - =6 Martin 2008 scaling: nominal
  !! - =7 Martin 2008 scaling: 95% upper bound
  !! - =8 Martin 2008 scaling: 95% lower bound
  !! - =9 Snipes 2000 scaling: nominal
  !! - =10 Snipes 2000 scaling: upper bound
  !! - =11 Snipes 2000 scaling: lower bound
  !! - =12 Snipes 2000 scaling (closed divertor): nominal
  !! - =13 Snipes 2000 scaling (closed divertor): upper bound
  !! - =14 Snipes 2000 scaling (closed divertor): lower bound
  !! - =15 Hubbard et al. 2012 L-I threshold scaling: nominal
  !! - =16 Hubbard et al. 2012 L-I threshold scaling: lower bound
  !! - =17 Hubbard et al. 2012 L-I threshold scaling: upper bound
  !! - =18 Hubbard et al. 2017 L-I threshold scaling
  !! - =19 Martin 2008 aspect ratio corrected scaling: nominal
  !! - =20 Martin 2008 aspect ratio corrected scaling: 95% upper bound
  !! - =21 Martin 2008 aspect ratio corrected scaling: 95% lower bound

  real(dp) :: ptremw = 0.0D0
  !! electron transport power (MW)
  
  real(dp) :: ptrepv = 0.0D0
  !! electron transport power per volume (MW/m3)

  real(dp) :: ptrimw = 0.0D0
  !! ion transport power (MW)

  real(dp) :: pscalingmw = 0.0D0
  !! Total transport power from scaling law (MW)

  real(dp) :: ptripv = 0.0D0
  !! ion transport power per volume (MW/m3)

  real(dp) :: q = 3.0D0
  !! safety factor 'near' plasma edge (`iteration variable 18`) equal to q95 
  !! (unless `icurr=2` (ST current scaling), in which case q = mean edge safety factor qbar)

  real(dp) :: q0 = 1.0D0
  !! safety factor on axis

  real(dp) :: q95 = 0.0D0
  !! safety factor at 95% surface

  real(dp) :: qfuel = 0.0D0
  !! plasma fuelling rate (nucleus-pairs/s)

  real(dp) :: tauratio = 1.0D0
  !! tauratio /1.0/ : ratio of He and pellet particle confinement times

  real(dp) :: qlim = 0.0D0
  !! lower limit for edge safety factor

  real(dp) :: qstar = 0.0D0
  !! cylindrical safety factor

  real(dp) :: rad_fraction_sol = 0.8D0
  !! SoL radiation fraction 

  real(dp) :: rad_fraction = 0.0D0
  !! Radiation fraction = total radiation / total power deposited in plasma

  real(dp) :: ralpne = 0.10D0
  !! thermal alpha density/electron density (`iteration variable 109`) (calculated if `ipedestal=3`)

  real(dp) :: protium = 0.0D0
  !! Seeded protium density / electron density.

  real(dp) :: rli = 0.9D0
  !! plasma normalised internal inductance (calculated from alphaj if `iprofile=1`)

  real(dp) :: rlp = 0.0D0
  !! plasma inductance (H)

  real(dp), bind(C) :: rmajor = 8.14D0
  !! plasma major radius (m) (`iteration variable 3`)

  real(dp), bind(C) :: rminor = 0.0D0
  !! plasma minor radius (m)

  real(dp) :: rnbeam = 0.005D0
  !! hot beam density / n_e (`iteration variable 7`)

  real(dp) :: rncne = 0.0D0
  !! n_carbon / n_e

  real(dp), bind(C) :: rndfuel = 0.0D0
  !! fuel burnup rate (reactions/second)

  real(dp) :: rnfene = 0.0D0
  !! n_highZ / n_e

  real(dp) :: rnone = 0.0D0
  !! n_oxygen / n_e

  real(dp) :: rpfac = 0.0D0
  !! neo-classical correction factor to rplas

  real(dp), bind(C) :: rplas = 0.0D0
  !! plasma resistance (ohm)

  real(dp) :: res_time = 0.0D0
  !! plasma current resistive diffusion time (s)

  real(dp) :: sarea = 0.0D0
  !! plasma surface area

  real(dp) :: sareao = 0.0D0
  !! outboard plasma surface area

  real(dp) :: sf = 0.0D0
  !! shape factor = plasma poloidal perimeter / (2.pi.rminor)

  integer :: i_single_null = 1
  !! switch for single null / double null plasma:
  !!
  !! - =0 for double null
  !! - =1 for single null (diverted side down)

  real(dp) :: ssync = 0.6D0
  !! synchrotron wall reflectivity factor

  real(dp) :: tauee = 0.0D0
  !! electron energy confinement time (sec)

  real(dp) :: tauee_in = 0.0D0
  !! Input electron energy confinement time (sec) (`isc=48 only`)

  real(dp) :: taueff = 0.0D0
  !! global thermal energy confinement time (sec)

  real(dp) :: tauei = 0.0D0
  !! ion energy confinement time (sec)

  real(dp) :: taup = 0.0D0
  !! alpha particle confinement time (sec)

  real(dp) :: te = 12.9D0
  !! volume averaged electron temperature (keV) (`iteration variable 4`)
  !! (`calculated if ipedestal=3`)

  real(dp) :: te0 = 0.0D0
  !! central electron temperature (keV)

  real(dp) :: ten = 0.0D0
  !! density weighted average electron temperature (keV)

  real(dp) :: ti = 12.9D0
  !! volume averaged ion temperature (keV). N.B. calculated from te if `tratio > 0.0`

  real(dp) :: ti0 = 0.0D0
  !! central ion temperature (keV)

  real(dp) :: tin = 0.0D0
  !! density weighted average ion temperature (keV)

  real(dp) :: tratio = 1.0D0
  !! ion temperature / electron temperature(used to calculate ti if `tratio > 0.0`

  real(dp), bind(C) :: triang = 0.36D0
  !! plasma separatrix triangularity (calculated if `ishape = 1, 3-5 or 7`)

  real(dp) :: triang95 = 0.24D0
  !! plasma triangularity at 95% surface (calculated if `ishape = 0-2, 6 or 8`)

  real(dp), bind(C) :: vol = 0.0D0
  !! plasma volume (m3)

  real(dp) :: vsbrn = 0.0D0
  !! V-s needed during flat-top (heat + burn times) (Wb)

  real(dp) :: vshift = 0.0D0
  !! plasma/device midplane vertical shift - single null

  real(dp) :: vsind = 0.0D0
  !! internal and external plasma inductance V-s (Wb)

  real(dp) :: vsres = 0.0D0
  !! resistive losses in startup V-s (Wb)

  real(dp) :: vsstt = 0.0D0
  !! total V-s needed (Wb)

  real(dp), bind(C) :: wallmw = 0.0D0
  !! average neutron wall load (MW/m2)

  real(dp), bind(C) :: wtgpd = 0.0D0
  !! mass of fuel used per day (g)

  real(dp) :: xarea = 0.0D0
  !! plasma cross-sectional area (m2)

  real(dp) :: zeff = 0.0D0
  !! plasma effective charge

  real(dp) :: zeffai = 0.0D0
  !! mass weighted plasma effective charge

end module physics_variables

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

module divertor_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the tokamak divertor components
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: adas = 0.0D0
  !! area divertor / area main plasma (along separatrix)

  real(dp) :: anginc = 0.262D0
  !! angle of incidence of field line on plate (rad)

  real(dp) :: betai = 1.0D0
  !! poloidal plane angle between divertor plate and leg, inboard (rad)

  real(dp) :: betao = 1.0D0
  !! poloidal plane angle between divertor plate and leg, outboard (rad)

  real(dp) :: bpsout = 0.60D0
  !! reference B_p at outboard divertor strike point (T)

  real(dp) :: c1div = 0.45D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c2div = -7.0D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c3div = 0.54D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c4div = -3.6D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c5div = 0.7D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: c6div = 0.0D0
  !! fitting coefficient to adjust ptpdiv, ppdiv

  real(dp) :: delld = 1.0D0
  !! coeff for power distribution along main plasma

  real(dp) :: dendiv = 0.0D0
  !! plasma density at divertor (10**20 /m3)

  real(dp) :: densin = 0.0D0
  !! density at plate (on separatrix) (10**20 /m3)

  real(dp) :: divclfr = 0.3D0
  !! divertor coolant fraction

  real(dp) :: divdens = 1.0D4
  !! divertor structure density (kg/m3)

  integer :: divdum = 0
  !! switch for divertor Zeff model:
  !!
  !! - =0 calc
  !! - =1 input
  !#TODO: switch name should be changed to i_<something>

  real(dp) :: divfix = 0.2D0
  !! divertor structure vertical thickness (m)

  real(dp) :: divmas = 0.0D0
  !! divertor plate mass (kg)

  real(dp) :: divplt = 0.035D0
  !! divertor plate thickness (m) (from Spears, Sept 1990)

  real(dp) :: divsur = 0.0D0
  !! divertor surface area (m2)

  real(dp) :: fdfs = 10.0D0
  !! radial gradient ratio

  real(dp) :: fdiva = 1.11D0
  !! divertor area fudge factor (for ITER, Sept 1990)

  real(dp) :: fgamp = 1.0D0
  !! sheath potential factor (not used)

  real(dp) :: fhout = 0.0D0
  !! fraction of power to outboard divertor (for single null)

  real(dp) :: fififi = 4.0D-3
  !! coefficient for gamdiv
  !#TODO: what the hell is this variable name...

  real(dp) :: frrp = 0.4D0
  !! fraction of radiated power to plate

  real(dp), bind(C) :: hldiv = 0.0D0
  !! divertor heat load (MW/m2)

  real(dp) :: hldivlim = 5.0D0
  !! heat load limit (MW/m2)

  real(dp) :: ksic = 0.8D0
  !! power fraction for outboard double-null scrape-off plasma

  real(dp) :: lamp = 0.0D0
  !! power flow width (m)

  real(dp) :: minstang = 0.0D0
  !! minimum strike angle for heat flux calculation

  real(dp) :: omegan = 1.0D0
  !! pressure ratio (nT)_plasma / (nT)_scrape-off

  real(dp) :: omlarg = 0.0D0
  !! power spillage to private flux factor

  real(dp) :: ppdivr = 0.0D0
  !! peak heat load at plate (with radiation) (MW/m2)

  real(dp) :: prn1 = 0.285D0
  !! n-scrape-off / n-average plasma; (input for `ipedestal=0`, = nesep/dene if `ipedestal>=1`)
  
  real(dp) :: ptpdiv = 0.0D0
  !! peak temperature at the plate (eV)

  real(dp) :: rconl = 0.0D0
  !! connection length ratio, outboard side

  real(dp) :: rlclolcn = 0.0D0
  !! ratio of collision length / connection length

  real(dp) :: rlenmax = 0.5D0
  !! maximum value for length ratio (rlclolcn) (`constraintg eqn 22`)

  real(dp) :: rsrd = 0.0D0
  !! effective separatrix/divertor radius ratio

  real(dp) :: tconl = 0.0D0
  !! main plasma connection length (m)

  real(dp) :: tdiv = 2.0D0
  !! temperature at divertor (eV) (input for stellarator only, calculated for tokamaks)

  real(dp) :: tsep = 0.0D0
  !! temperature at the separatrix (eV)

  real(dp) :: xparain = 2.1D3
  !! parallel heat transport coefficient (m2/s)

  real(dp) :: xpertin = 2.0D0
  !! perpendicular heat transport coefficient (m2/s)

  real(dp) :: zeffdiv = 1.0D0
  !! Zeff in the divertor region (if `divdum /= 0`)

end module divertor_variables

module fwbs_variables
  !! author: J. Morris (UKAEA), M. Kovari (UKAEA)
  !! 
  !! Module containing global variables relating to the first wall, blanket and 
  !! shield components
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp), bind(C):: bktlife = 0.0D0
  !! Full power blanket lifetime (years)

  real(dp) :: coolmass = 0.0D0
  !! mass of water coolant (in shield, blanket, first wall, divertor) (kg)

  real(dp) :: vvmass = 0.0D0
  !! vacuum vessel mass (kg)

  real(dp) :: denstl = 7800.0D0
  !! density of steel (kg/m3)
  !#TODO: should this be in constants. Is currently an input. Should be a list of preapproved options?

  real(dp) :: denw = 19250.0D0
  !! density of tungsten (kg/m3)
  !#TODO: same as above with steel?

  real(dp) :: dewmkg = 0.0D0
  !! total mass of vacuum vessel + cryostat (kg) (calculated if blktmodel>0)
  !# TODO: blktmodel needs consolidating with iblanket

  real(dp) :: emult = 1.269D0
  !! energy multiplication in blanket and shield

  real(dp) :: emultmw = 0.0D0
  !! power due to energy multiplication in blanket and shield [MW]

  real(dp) :: fblss = 0.09705D0
  !! KIT blanket model: steel fraction of breeding zone

  real(dp) :: fdiv = 0.115D0
  !! Solid angle fraction taken by one divertor

  real(dp) :: fhcd = 0.0D0
  !! area fraction covered by heating/current drive apparatus plus diagnostics

  real(dp) :: fhole = 0.0D0
  !! area fraction taken up by other holes (IFE)

  integer :: fwbsshape = 2
  !! switch for first wall, blanket, shield and vacuum vessel shape:
  !!
  !! - =1 D-shaped (cylinder inboard + ellipse outboard)
  !! - =2 defined by two ellipses
  !#TODO: change to adopt switch naming convention

  real(dp) :: fwlife = 0.0D0
  !! first wall full-power year lifetime (y)

  real(dp) :: fwmass = 0.0D0
  !! first wall mass (kg)

  real(dp) :: fw_armour_mass = 0.0D0
  !! first wall armour mass (kg)

  real(dp) :: fw_armour_thickness = 0.005D0
  !! first wall armour thickness (m)

  real(dp) :: fw_armour_vol = 0.0D0
  !! first wall armour volume (m3)

  integer :: iblanket = 1
  !! switch for blanket model:
  !!
  !! - =1 CCFE HCPB model
  !! - =2 KIT HCPB model
  !! - =3 CCFE HCPB model with Tritium Breeding Ratio calculation
  !! - =4 KIT HCLL model

  integer :: iblnkith = 1
  !! switch for inboard blanket:
  !!
  !! - =0 No inboard blanket (blnkith=0.0)
  !! - =1 Inboard blanket present

  integer :: inuclear = 0
  !! switch for nuclear heating in the coils:
  !!
  !! - =0 Frances Fox model (default)
  !! - =1 Fixed by user (qnuc)

  real(dp) :: qnuc = 0.0D0
  !! nuclear heating in the coils (W) (`inuclear=1`)

  real(dp) :: li6enrich = 30.0D0
  !! lithium-6 enrichment of breeding material (%)

  real(dp), bind(C) :: pnucblkt = 0.0D0
  !! nuclear heating in the blanket (MW)

  real(dp) :: pnuc_cp = 0.0D0
  !! Total nuclear heating in the ST centrepost (MW)

  real(dp) :: pnuc_cp_sh = 0.0D0
  !! Neutronic shield nuclear heating in the ST centrepost (MW)

  real(dp) :: pnuc_cp_tf = 0.0D0
  !! TF neutronic nuclear heating in the ST centrepost (MW)

  real(dp) :: pnucdiv = 0.0D0
  !! nuclear heating in the divertor (MW)

  real(dp) :: pnucfw = 0.0D0
  !! nuclear heating in the first wall (MW)

  real(dp) :: pnuchcd = 0.0D0
  !! nuclear heating in the HCD apparatus and diagnostics (MW)

  real(dp) :: pnucloss = 0.0D0
  !! nuclear heating lost via holes (MW)

  real(dp) :: pnucvvplus = 0.0D0
  !! nuclear heating to vacuum vessel and beyond(MW)

  real(dp), bind(C) :: pnucshld = 0.0D0
  !! nuclear heating in the shield (MW)

  real(dp) :: whtblkt = 0.0D0
  !! mass of blanket (kg)

  real(dp) :: whtblss = 0.0D0
  !! mass of blanket - steel part (kg)

  real(dp) :: armour_fw_bl_mass = 0.0D0
  !! Total mass of armour, first wall and blanket (kg)

  ! CCFE HCPB Blanket Model (with or without TBR calculation) iblanket=1,3
  ! ----------

  real(dp) :: breeder_f = 0.5D0
  !! Volume ratio: Li4SiO4/(Be12Ti+Li4SiO4) (`iteration variable 108`)
  
  real(dp) :: breeder_multiplier = 0.75D0
  !! combined breeder/multipler fraction of blanket by volume
  
  real(dp) :: vfcblkt = 0.05295D0
  !! He coolant fraction of blanket by volume (`iblanket= 1,3` (CCFE HCPB))
  
  real(dp) :: vfpblkt = 0.1D0
  !! He purge gas fraction of blanket by volume (`iblanket= 1,3` (CCFE HCPB))

  real(dp) :: whtblli4sio4 = 0.0D0
  !! mass of lithium orthosilicate in blanket (kg) (`iblanket=1,3` (CCFE HCPB))
  
  real(dp) :: whtbltibe12 = 0.0D0
  !! mass of titanium beryllide in blanket (kg) (`iblanket=1,3` (CCFE HCPB))

  real(dp) :: neut_flux_cp = 0.0D0
  !! Centrepost TF fast neutron flux (E > 0.1 MeV) [m^(-2).^(-1)]
  !! This variable is only calculated for superconducting (i_tf_sup = 1 )
  !! spherical tokamal magnet designs (itart = 0)

  real(dp) :: f_neut_shield = -1.0D0
  !! Fraction of nuclear power shielded before the CP magnet (ST)
  !! ( neut_absorb = -1 --> a fit on simplified MCNP neutronic
  !! calculation is used assuming water cooled (13%) tungesten carbyde )
  
  !  KIT HCPB blanket model (iblanket = 2)
  ! ----------

  integer :: breedmat = 1
  !! breeder material switch (iblanket=2 (KIT HCPB)):
  !!
  !! - =1 Lithium orthosilicate
  !! - =2 Lithium methatitanate
  !! - =3 Lithium zirconate

  real(dp) :: densbreed = 0.0D0
  !! density of breeder material (kg/m3) (`iblanket=2` (KIT HCPB))

  real(dp) :: fblbe = 0.6D0
  !! beryllium fraction of blanket by volume (if `iblanket=2`, is Be fraction of breeding zone)

  real(dp) :: fblbreed = 0.154D0
  !! breeder fraction of blanket breeding zone by volume (`iblanket=2` (KIT HCPB))

  real(dp) :: fblhebmi = 0.4D0
  !! helium fraction of inboard blanket box manifold by volume (`iblanket=2` (KIT HCPB))

  real(dp) :: fblhebmo = 0.4D0
  !! helium fraction of outboard blanket box manifold by volume (`iblanket=2` (KIT HCPB))

  real(dp) :: fblhebpi = 0.6595D0
  !! helium fraction of inboard blanket back plate by volume (`iblanket=2` (KIT HCPB))

  real(dp) :: fblhebpo = 0.6713D0
  !! helium fraction of outboard blanket back plate by volume (`iblanket=2` (KIT HCPB))
  
  integer :: hcdportsize = 1
  !! switch for size of heating/current drive ports (`iblanket=2` (KIT HCPB)):
  !!
  !! - =1 'small'
  !! - =2 'large'
  !#TODO: switch name and also large and small not descriptive enough
  
  real(dp) :: nflutf = 0.0D0
  !! peak fast neutron fluence on TF coil superconductor (n/m2) (`iblanket=2` (KIT HCPB))

  integer :: npdiv = 2
  !! number of divertor ports (`iblanket=2` (KIT HCPB))

  integer :: nphcdin = 2
  !! number of inboard ports for heating/current drive (`iblanket=2` (KIT HCPB))

  integer :: nphcdout = 2
  !! number of outboard ports for heating/current drive (`iblanket=2` (KIT HCPB))

  real(dp) :: tbr = 0.0D0
  !! tritium breeding ratio (`iblanket=2,3` (KIT HCPB/HCLL))

  real(dp) :: tritprate = 0.0D0
  !! tritium production rate (g/day) (`iblanket=2` (KIT HCPB))

  real(dp) :: vvhemax = 0.0D0
  !! maximum helium concentration in vacuum vessel at end of plant life (appm) 
  !! (`iblanket=2` (KIT HCPB))

  real(dp) :: wallpf = 1.21D0
  !! neutron wall load peaking factor (`iblanket=2` (KIT HCPB))

  real(dp) :: whtblbreed = 0.0D0
  !! mass of blanket - breeder part (kg) (`iblanket=2` (KIT HCPB))

  real(dp) :: whtblbe = 0.0D0
  !! mass of blanket - beryllium part (kg)

  ! CCFE HCPB model with Tritium Breeding Ratio calculation (iblanket=3)
  ! ---------------

  integer :: iblanket_thickness = 2
  !! Blanket thickness switch (Do not set blnkith, blnkoth, fwith or fwoth when `iblanket=3`):
  !!
  !! - =1 thin    0.53 m inboard, 0.91 m outboard
  !! - =2 medium  0.64 m inboard, 1.11 m outboard
  !! - =3 thick   0.75 m inboard, 1.30 m outboard

  integer :: primary_pumping = 2
  !! Switch for pumping power for primary coolant (mechanical power only and peak first wall 
  !! temperature is only calculated if `primary_pumping=2`):
  !!
  !! - =0 User sets pump power directly (htpmw_blkt, htpmw_fw, htpmw_div, htpmw_shld)
  !! - =1 User sets pump power as a fraction of thermal power (fpumpblkt, fpumpfw, fpumpdiv, fpumpshld)
  !! - =2 Mechanical pumping power is calculated
  !! - =3 Mechanical pumping power is calculated using specified pressure drop

  integer :: secondary_cycle = 0
  !! Switch for power conversion cycle:
  !!
  !! - =0 Set efficiency for chosen blanket, from detailed models (divertor heat not used)
  !! - =1 Set efficiency for chosen blanket, from detailed models (divertor heat used)
  !! - =2 user input thermal-electric efficiency (etath)
  !! - =3 steam Rankine cycle
  !! - =4 supercritical CO2 cycle

  integer, bind(C) :: coolwh = 1
  !! Switch for blanket coolant (set via blkttype):
  !!
  !! - =1 helium
  !! - =2 pressurized water
  !#TODO: change switch name to satisfy convention

  real(dp) :: afwi = 0.008D0
  !! inner radius of inboard first wall/blanket coolant channels (stellarator only) (m)
  !#TODO move to stellarator?

  real(dp) :: afwo = 0.008D0
  !! inner radius of outboard first wall/blanket coolant channels (stellarator only) (m)
  !#TODO move to stellarator?

  character(len=6) :: fwcoolant = 'helium'
  !! switch for first wall coolant (can be different from blanket coolant):
  !!
  !! - 'helium' 
  !! - 'water'

  real(dp) :: fw_wall = 0.003D0
  !! wall thickness of first wall coolant channels (m)

  real(dp) :: afw = 0.006D0
  !! radius of first wall cooling channels (m)
  
  real(dp) :: pitch = 0.020D0
  !! pitch of first wall cooling channels (m)

  real(dp) :: fwinlet = 573.0D0
  !! inlet temperature of first wall coolant (K)

  real(dp) :: fwoutlet = 823.0D0
  !! outlet temperature of first wall coolant (K)

  real(dp) :: fwpressure = 15.5D6
  !! first wall coolant pressure (Pa) (`secondary_cycle>1`)

  real(dp) :: tpeak = 873.0D0
  !! peak first wall temperature (K)

  real(dp) :: roughness = 1.0D-6
  !! first wall channel roughness epsilon (m)

  real(dp) :: fw_channel_length = 4.0D0
  !! Length of a single first wall channel (all in parallel) (m)
  !! (`iteration variable 114`, useful for `constraint equation 39`)

  real(dp) :: peaking_factor = 1.0D0
  !! peaking factor for first wall heat loads. (Applied separately to inboard and outboard loads.
  !! Applies to both neutron and surface loads. Only used to calculate peak temperature - not 
  !! the coolant flow rate.)

  real(dp) :: blpressure = 15.5D6
  !! blanket coolant pressure (Pa) (`secondary_cycle>1`)

  real(dp) :: inlet_temp = 573.0D0
  !! inlet temperature of blanket coolant  (K) (`secondary_cycle>1`)

  real(dp) :: outlet_temp = 823.0D0
  !! Outlet temperature of blanket coolant (K) (`secondary_cycle>1`)
  !!
  !! - input if `coolwh=1` (helium)
  !! - calculated if `coolwh=2` (water)

  real(dp) :: coolp = 15.5D6
  !! blanket coolant pressure (Pa) (stellarator only)

  integer :: nblktmodpo = 8
  !! number of outboard blanket modules in poloidal direction (`secondary_cycle>1`)

  integer :: nblktmodpi = 7
  !! number of inboard blanket modules in poloidal direction (`secondary_cycle>1`)

  integer :: nblktmodto = 48
  !! number of outboard blanket modules in toroidal direction (`secondary_cycle>1`)

  integer :: nblktmodti = 32
  !! number of inboard blanket modules in toroidal direction (`secondary_cycle>1`)

  real(dp) :: tfwmatmax = 823.0D0
  !! maximum temperature of first wall material (K) (`secondary_cycle>1`)

  real(dp) :: fw_th_conductivity = 28.34D0
  !! thermal conductivity of first wall material at 293 K (W/m/K) (Temperature dependence 
  !! is as for unirradiated Eurofer)

  real(dp) :: fvoldw = 1.74D0
  !! area coverage factor for vacuum vessel volume

  real(dp) :: fvolsi = 1.0D0
  !! area coverage factor for inboard shield volume

  real(dp) :: fvolso = 0.64D0
  !! area coverage factor for outboard shield volume

  real(dp) :: fwclfr = 0.15D0
  !! first wall coolant fraction (calculated if `lpulse=1` or `ipowerflow=1`)

  real(dp) :: praddiv = 0.0D0
  !! radiation power incident on the divertor (MW)

  real(dp) :: pradfw = 0.0D0
  !! radiation power incident on the divertor (MW)

  real(dp) :: pradhcd = 0.0D0
  !! radiation power incident on the divertor (MW)

  real(dp) :: pradloss = 0.0D0
  !! radiation power incident on the divertor (MW)

  real(dp) :: ptfnuc = 0.0D0
  !! nuclear heating in the TF coil (MW)

  real(dp) :: ptfnucpm3 = 0.0D0
  !! nuclear heating in the TF coil (MW/m3) (`blktmodel>0`)
  !#TODO: check usage of old blktmodel. Update to iblanket

  real(dp) :: rdewex = 0.0D0
  !! cryostat radius (m)

  real(dp) :: zdewex = 0.0D0
  !! cryostat height (m)

  real(dp) :: rpf2dewar = 0.5D0
  !! radial distance between outer edge of largest (`ipfloc=3`) PF coil (or stellarator 
  !! modular coil) and cryostat (m)

  real(dp) :: vdewex = 0.0D0
  !! cryostat volume (m3)

  real(dp) :: vdewin = 0.0D0
  !! vacuum vessel volume (m3)

  real(dp) :: vfshld = 0.25D0
  !! coolant void fraction in shield

  real(dp) :: volblkt = 0.0D0
  !! volume of blanket (m3)

  real(dp) :: volblkti = 0.0D0
  !! volume of inboard blanket (m3)

  real(dp) :: volblkto = 0.0D0
  !! volume of outboard blanket (m3)

  real(dp) :: volshld = 0.0D0
  !! volume of shield (m3)

  real(dp) :: whtshld = 0.0D0
  !! mass of shield (kg)

  real(dp) :: wpenshld = 0.0D0
  !! mass of the penetration shield (kg)

  real(dp) :: wtshldi = 0.0D0
  !! mass of inboard shield (kg)
  
  real(dp) :: wtshldo = 0.0D0
  !! mass of outboard shield (kg)
  
  integer :: irefprop = 1
  !! Switch to use REFPROP routines (stellarator only)
  !#TODO: number of stellarator only items here. Also appear in fispact. Tidy needed

  real(dp) :: fblli = 0.0D0
  !! lithium fraction of blanket by volume (stellarator only)

  real(dp) :: fblli2o = 0.08D0
  !! lithium oxide fraction of blanket by volume (stellarator only)

  real(dp) :: fbllipb = 0.68D0
  !! lithium lead fraction of blanket by volume (stellarator only)

  real(dp) :: fblvd = 0.0D0
  !! vanadium fraction of blanket by volume (stellarator only)

  real(dp) :: wtblli2o = 0.0D0
  !! mass of blanket - Li_2O part (kg)

  real(dp) :: wtbllipb = 0.0D0
  !! mass of blanket - Li-Pb part (kg)
  
  real(dp) :: whtblvd = 0.0D0
  !! mass of blanket - vanadium part (kg)

  real(dp) :: whtblli = 0.0D0
  !! mass of blanket - lithium part (kg)

  real(dp) :: vfblkt = 0.25D0
  !! coolant void fraction in blanket (`blktmodel=0`), (calculated if `blktmodel > 0`)

  integer :: blktmodel = 0
  !! switch for blanket/tritium breeding model (see iblanket):
  !!
  !! - =0 original simple model
  !! - =1 KIT model based on a helium-cooled pebble-bed blanket (HCPB) reference design
  !#TODO: this needs investigating and removing after any required functionality is in iblanket

  real(dp) :: declblkt = 0.075D0
  !! neutron power deposition decay length of blanket structural material (m) (stellarators only)

  real(dp) :: declfw = 0.075D0
  !! neutron power deposition decay length of first wall structural material (m) (stellarators only)

  real(dp) :: declshld = 0.075D0
  !! neutron power deposition decay length of shield structural material (m) (stellarators only)

  integer :: blkttype = 3
  !! Switch for blanket type:
  !!
  !! - =1 WCLL; efficiency taken from WP13-DAS08-T02, EFDA_D_2M97B7
  !! - =2 HCLL; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX
  !! - =3 HCPB; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX
  !#TODO: this needs to be merged into iblanket and then removed.

  real(dp) :: etaiso = 0.85D0
  !! isentropic efficiency of FW and blanket coolant pumps

  real(dp) :: etahtp = 0.95D0
  !! electrical efficiency of primary coolant pumps

end module fwbs_variables

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

module tfcoil_variables
  !! author: J. Morris, M. Kovari, S. Kahn (UKAEA)
  !!
  !! Module containing global variables relating to the toroidal field coil systems
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !! - ITER Magnets design description document DDD11-2 v2 2 (2009)

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: acasetf = 0.0D0
  !! external case area per coil (inboard leg) (m2)

  real(dp) :: acasetfo = 0.0D0
  !! external case area per coil (outboard leg) (m2)

  real(dp) :: acndttf = 0.0D0
  !! area of the cable conduit (m2)

  real(dp) :: acond = 0.0D0
  !! conductor area (winding pack) (m2)

  real(dp) :: acstf = 0.0D0
  !! internal area of the cable space (m2)

  real(dp) :: insulation_area = 0.0D0
  !! single turn insulation area (m2)

  real(dp) :: aiwp = 0.0D0
  !! winding pack insulation area (m2)

  real(dp) :: alstrtf = 6.0D8
  !! Allowable Tresca stress in TF coil structural material (Pa)

  real(dp) :: arealeg = 0.0D0
  !! outboard TF leg area (m2)

  real(dp) :: aswp = 0.0D0
  !! winding pack structure area (m2)

  real(dp) :: avwp = 0.0D0
  !! winding pack void (He coolant) area (m2)

  real(dp) :: awphec = 0.0D0
  !! winding pack He coil area (m2)

  real(dp) :: bcritsc = 24.0D0
  !! upper critical field (T) for Nb3Sn superconductor at zero temperature and 
  !! strain (`i_tf_sc_mat=4, =bc20m`)

  real(dp) :: bmaxtf = 0.0D0
  !! mean peak field at TF coil (T)

  real(dp) :: bmaxtfrp = 0.0D0
  !! peak field at TF conductor with ripple (T)

  real(dp) :: casestr = 0.0D0
  !! case strain

  real(dp) :: casthi = 0.0D0
  !! inboard TF coil case plasma side thickness (m) (calculated for stellarators)

  real(dp) :: casthi_fraction = 0.05D0
  !! inboard TF coil case plasma side thickness as a fraction of tfcth

  logical :: casthi_is_fraction
  !! logical switch to make casthi a fraction of TF coil thickness (`casthi_fraction`)

  real(dp) :: casths = 0.0D0
  !! inboard TF coil sidewall case thickness (m) (calculated for stellarators)

  real(dp) :: casths_fraction = 0.03D0
  !! inboard TF coil sidewall case thickness as a fraction of tftort

  logical :: tfc_sidewall_is_fraction
  !! logical switch to make casths a fraction of TF coil thickness (`casths_fraction`)

  real(dp) :: t_conductor = 0.0D0
  !! Conductor (cable + steel conduit) area averaged dimension [m]
  
  real(dp) :: t_turn = 0.0D0
  !! WP turn squared dimensions [m]

  real(dp) :: acs = 0.0D0
  !! Area of space inside conductor (m2)

  real(dp) :: cdtfleg = 0.0D0
  !! TF outboard leg current density (A/m2) (resistive coils only)
  
  real(dp) :: cforce = 0.0D0
  !! centering force on inboard leg (per coil) (N/m)

  real(dp) :: cpttf = 7.0e4
  !! TF coil current per turn (A). (calculated for stellarators) (calculated for 
  !! integer-turn TF coils `i_tf_turns_integer=1`) (`iteration variable 60`)

  real(dp) :: cpttf_max = 9.0e4
  !! Max TF coil current per turn [A]. (for stellarators and `i_tf_turns_integer=1`) 
  !! (`constraint equation 77`)

  real(dp) :: dcase = 8000.0D0
  !! density of coil case (kg/m3)

  real(dp), dimension(7) :: dcond = 9000.0D0
  !! density of superconductor type given by i_tf_sc_mat/isumatoh/isumatpf (kg/m3)
  
  real(dp) :: dcondins = 1800.0D0
  !! density of conduit + ground-wall insulation (kg/m3)

  real(dp) :: dhecoil = 0.005D0
  !! diameter of He coil in TF winding (m)

  real(dp) :: estotftgj = 0.0D0
  !! total stored energy in the toroidal field (GJ)

  real(dp) :: farc4tf = 0.7D0
  !! factor to size height of point 4 on TF coil
  real(kind(1.0D0)) :: b_crit_upper_nbti = 14.86D0
  !! upper critical field of GL_nbti
  real(kind(1.0D0)) :: t_crit_nbti = 9.04D0
  !! critical temperature of GL_nbti
  real(kind(1.0D0)) :: max_force_density = 0.0D0
  !! Maximal (WP averaged) force density in TF coils at 1 point. (MN/m3)
  real(kind(1.0D0)) :: fcutfsu = 0.69D0
  !! copper fraction of cable conductor (TF coils)
  !! (iteration variable 59)
  real(dp) :: fhts = 0.5D0
  !! technology adjustment factor for critical current density fit for isumat..=2 
  !! Bi-2212 superconductor, to describe the level of technology assumed (i.e. to 
  !! account for stress, fatigue, radiation, AC losses, joints or manufacturing 
  !! variations; 1.0 would be very optimistic)
  
  real(dp) :: insstrain = 0.0D0
  !! Radial strain in insulator

  integer :: i_tf_plane_stress = 1
  !! Switch for the TF coil stress model
  !!   0 : New generalized plane strain formulation 
  !!   1 : Old plane stress model (only for SC)

  integer :: i_tf_tresca = 0
  !! Switch for TF coil conduit Tresca stress criterion:
  !!   0 : Tresca (no adjustment);
  !!   1 : Tresca with CEA adjustment factors (radial+2%, vertical+60%) </UL>
  
  integer :: i_tf_wp_geom = -1
  !! Switch for TF WP geometry selection
  !!   0 : Rectangular geometry 
  !!   1 : Double rectangular geometry 
  !!   2 : Trapezoidal geometry (constant lateral casing thickness)
  !! Default setting for backward compatibility 
  !!   if i_tf_turns_integer = 0 : Double rectangular
  !!   if i_tf_turns_integer = 1 : Rectangular 

  integer :: i_tf_case_geom = 0
  !! Switch for TF case geometry selection
  !!   0 : Circular front case (ITER design)
  !!   1 : Straight front case

  integer :: i_tf_turns_integer = 0
  !! Switch for TF coil integer/non-integer turns:
  !!   0 : non-integer turns
  !!   1 : integer turns

  integer :: i_tf_sc_mat = 1
  !! Switch for superconductor material in TF coils:
  !!
  !! - =1 ITER Nb3Sn critical surface model with standard
  !!   ITER parameters
  !! - =2 Bi-2212 high temperature superconductor (range of
  !!   validity T < 20K, adjusted field b < 104 T, B > 6 T)
  !! - =3 NbTi
  !! - =4 ITER Nb3Sn model with user-specified parameters
  !! - =5 WST Nb3Sn parameterisation
  !! - =6 REBCO HTS tape in CroCo strand

  integer :: i_tf_sup = 1
  !! Switch for TF coil conductor model:
  !!
  !! - =0 copper
  !! - =1 superconductor
  !! - =2 Cryogenic aluminium

  integer :: i_tf_shape = 0
  !! Switch for TF coil toroidal shape:
  !!
  !! - =0  Default value : Picture frame coil for TART / PROCESS D-shape for non itart
  !! - =1  PROCESS D-shape : parametrise with 2 arcs 
  !! - =2  Picture frame coils 

  integer :: n_pancake = 10
  !! Number of pancakes in TF coil. Only used if `i_tf_turns_integer=1`

  integer :: n_layer = 20
  !! Number of layers in TF coil. Only used if `i_tf_turns_integer=1`
  
  integer :: n_rad_per_layer = 100
  !! Size of the arrays per layers storing the radial dependent stress 
  !! quantities (stresses, strain displacement etc..)

  integer :: i_tf_bucking = -1
  !! Switch for TF inboard suport structure design:
  !! 
  !! Default setting for backward compatibility
  !!     - if copper resistive TF (i_tf_sup = 0) : Free standing TF without bucking structure 
  !!     - if Superconducting TF  (i_tf_sup = 1) : Free standing TF with a steel casing  
  !!     - if aluminium  TF       (i_tf_sup = 2) : Free standing TF with a bucking structure
  !!     Rem : the case is a bucking structure
  !! - =0 : Free standing TF without case/bucking cyliner (only a conductor layer)
  !! - =1 : Free standing TF with a case/bucking cylinder made of 
  !!     - if copper resistive     TF (i_tf_sup = 0) : used defined bucking cylinder
  !!     - if Superconducting      TF (i_tf_sup = 1) : Steel casing
  !!     - if aluminium resisitive TF (i_tf_sup = 2) : used defined bucking cylinder
  !! - =2 : The TF is in contact with the CS : "bucked and weged design"
  !!       Fast version : thin TF-CS interface neglected in the stress calculations (3 layers)
  !! - =3 : The TF is in contact with the CS : "bucked and weged design"
  !!       Full version : thin TF-CS Kapton interface introduced in the stress calculations (4 layers)

  integer :: n_tf_graded_layers = 1
  !! Number of layers of different stress properties in the WP. If `n_tf_graded_layers > 1`, 
  !! a graded coil is condidered

  integer :: n_tf_stress_layers = 0
  !! Number of layers considered for the inboard TF stress calculations
  !! set in initial.f90 from i_tf_bucking and n_tf_graded_layers

  real(dp) :: jbus = 1.25D6
  !! bussing current density (A/m2)
  
  real(dp) :: jwdgcrt = 0.0D0
  !! critical current density for winding pack (A/m2)

  real(dp) :: jwdgpro = 0.0D0
  !! allowable TF coil winding pack current density, for dump temperature rise protection (A/m2)

  real(dp) :: jwptf = 0.0D0
  !! winding pack engineering current density (A/m2)

  real(dp) :: oacdcp = 0.0D0
  !! Overall current density in TF coil inboard legs midplane (A/m2)
  !! Rem SK : Not used in tfcoil to set the current any more. Should not be used as
  !! iteration variable 12 any more. It is now calculated.

  real(dp) :: eyzwp = 0.0D0
  !! Winding pack vertical Young's modulus (Pa)

  real(dp) :: eyoung_ins = 1.0D8
  !! Insulator Young's modulus [Pa]. Default value (1.0D8) setup the following values
  !!  - SC TF, eyoung_ins = 20 Gpa (default value from DDD11-2 v2 2 (2009))
  !!  - Al TF, eyoung_ins = 2.5 GPa (Kapton polymer)

  real(dp) :: eyoung_steel = 2.05D11
  !! Steel case Young's modulus (Pa) (default value from DDD11-2 v2 2 (2009))

  real(dp) :: eyoung_winding = 6.6D8
  !! SC TF coil winding Young's modulus (Pa)
  
  real(dp) :: eyoung_res_tf_buck = 150.0D9 
  !! Resistive TF magnets bucking cylinder young modulus (Pa)

  real(dp) :: eyoung_copper = 117.0D9
  !! Copper young modulus. Default value taken from wikipedia

  real(dp) :: eyoung_al = 69.0D9 
  !! Aluminium young modulus.  Default value taken from wikipedia
  
  real(dp) :: poisson_steel = 0.3D0
  !! Steel Poisson's ratio 
  
  real(dp):: poisson_copper = 0.35D0
  !! Copper Poisson's ratio. Source : https://www.engineeringtoolbox.com/poissons-ratio-d_1224.html

  real(dp):: poisson_al = 0.35D0
  !! Aluminium Poisson's ratio. 
  !! Source : https://www.engineeringtoolbox.com/poissons-ratio-d_1224.html

  real(dp) :: rbmax = 0.0D0
  !! Radius of maximum TF B-field (m)

  real(dp) :: tflegres = 0.0D0
  !! TF coil leg resistance (ohm)

  real(dp) :: toroidalgap = 1.0D0 ![m]
  !! Minimal distance between two toroidal coils. (m)

  real(dp) :: ftoroidalgap = 1.0D0
  !! F-value for minimum tftort (`constraint equation 82`)

  real(dp) :: ripmax = 1.0D0
  !! aximum allowable toroidal field ripple amplitude at plasma edge (%)

  real(dp) :: ripple = 0.0D0
  !! peak/average toroidal field ripple at plasma edge (%)

  real(dp) :: ritfc = 0.0D0
  !! total (summed) current in TF coils (A)
  
  integer, parameter :: n_radial_array = 50
  !! Size of the radial distribution arrays per layers
  !! used for stress, strain and displacement distibution

  real(dp), dimension(2*n_radial_array) :: radial_array = 0.0D0
  !! Array refining the radii of the stress calculations arrays

  real(dp), dimension(2*n_radial_array) :: sig_tf_r = 0.0D0
  !! TF Inboard leg radial stress in steel r distribution at mid-plane [Pa]
  
  real(dp), dimension(2*n_radial_array) :: sig_tf_t = 0.0D0
  !! TF Inboard leg tangential stress in steel r distribution at mid-plane [Pa]
  
  real(dp), dimension(2*n_radial_array) :: deflect = 0.0D0
  !! TF coil radial deflection (displacement) radial distribution [m]

  real(dp) :: sig_tf_z = 0.0D0
  !! TF Inboard leg vertical tensile stress in steel at mid-plane [Pa]
    
  real(dp), dimension(2*n_radial_array) :: sig_tf_vmises = 0.0D0
  !! TF Inboard leg Von-Mises stress in steel r distribution at mid-plane [Pa]
      
  real(dp), dimension(2*n_radial_array) :: sig_tf_tresca = 0.0D0 
  !! TF Inboard leg TRESCA stress in steel r distribution at mid-plane [Pa]

  real(dp) :: strtf0 = 0.0D0
  !! Maximum TRESCA stress in CS structures at CS flux swing [Pa]:
  !!
  !!  - If superconducting CS (ipfres = 0): turn steel conduits TRESCA stress
  !!  - If resistive       CS (ipfres = 1): copper conductor TRESCA stress 
  !!
  !! Quantity only computed for bucked and wedged design (`i_tf_bucking >= 2`)
  !! Def : CS Flux swing, instant when the current changes sign in CS (null current) 

  real(dp) :: strtf1 = 0.0D0
  !! Maximum TRESCA stress in TF casing steel structures (Pa)
  
  real(dp) :: strtf2 = 0.0D0
  !! Maximum TRESCA stress in TF WP conduit steel structures (Pa)
  
  real(dp) :: sigvvall = 9.3D7
  !! allowable stress from TF quench in vacuum vessel (Pa)

  real(dp) :: strncon_cs = -0.005D0
  !! strain in CS superconductor material (used in Nb3Sn critical surface model `isumatoh=1,4,5`)

  real(dp) :: strncon_pf = -0.005D0
  !! strain in PF superconductor material (used in Nb3Sn critical surface model `isumatph=1,4,5`)

  real(dp) :: strncon_tf = -0.005D0
  !! strain in TF superconductor material (used in Nb3Sn critical surface model `i_tf_sc_mat=1,4,5`)

  character(len=12) :: quench_model = 'exponential'
  !! switch for TF coil quench model (Only applies to REBCO magnet at present, issue #522):
  !!
  !! - ='exponential' exponential quench with constant discharge resistor
  !! - ='linear' quench with constant voltage

  real(dp) :: quench_detection_ef = 0D0
  !! Electric field at which TF quench is detected and discharge begins (V/m)

  real(dp) :: time1 = 0D0
  !! Time at which TF quench is detected (s)

  real(dp) :: taucq = 30.0D0
  !! allowable TF quench time (s)

  real(dp) :: tcritsc = 16.0D0
  !! critical temperature (K) for superconductor at zero field and strain (`i_tf_sc_mat=4, =tc0m`)

  real(dp) :: tdmptf = 10.0D0
  !! fast discharge time for TF coil in event of quench (s) (`iteration variable 56`)
  !!
  !! For REBCO model, meaning depends on quench_model:
  !!
  !! - exponential quench : e-folding time (s)`
  !! - linear quench : discharge time (s)

  real(dp) :: tfareain = 0.0D0
  !! Area of inboard midplane TF legs (m2)

  real(dp) :: tfbusl = 0.0D0
  !! TF coil bus length (m)

  real(dp) :: tfbusmas = 0.0D0
  !! TF coil bus mass (kg)

  real(dp) :: tfckw = 0.0D0
  !! available DC power for charging the TF coils (kW)

  !#TODO: issue #781
  ! integer :: tfc_model = 1
  ! !! tfc_model /1/ : switch for TF coil magnet stress model:<UL>
  ! !!                 <LI> = 0 simple model (solid copper coil)
  ! !!                 <LI> = 1 CCFE two-layer stress model; superconductor</UL>

  real(dp), bind(C) :: tfcmw = 0.0D0
  !! Peak power per TF power supply (MW)
  
  real(dp) :: tfcpmw = 0.0D0
  !! Peak resistive TF coil inboard leg power (MW)

  real(dp) :: tfjtsmw = 0.0D0
  !! TF joints resistive power losses (MW)

  real(dp) :: tfcryoarea = 0.0D0
  !! surface area of toroidal shells covering TF coils (m2)

  real(dp) :: tficrn = 0.0D0
  !! TF coil half-width - inner bore (m)

  real(dp) :: tfind = 0.0D0
  !! TF coil inductance (H)

  real(dp) :: tfinsgap = 0.010D0
  !! TF coil WP insertion gap (m)
  
  real(dp) :: tflegmw = 0.0D0
  !! TF coil outboard leg resistive power (MW)

  real(dp) :: rhocp = 0.0D0
  !! TF coil inboard leg resistivity [Ohm-m]. If `itart=0`, this variable is the 
  !! average resistivity over the whole magnet

  real(dp) :: rhotfleg = 0.0D0
  !! Resistivity of a TF coil leg (Ohm-m)

  real(dp) :: rhotfbus = -1.0D0 ! 2.5D-8
  !! Resistivity of a TF coil bus (Ohm-m). Default value takes the same res as the leg one
 
  real(dp) :: frhocp = 1.0D0
  !! Centrepost resistivity enhancement factor. For `itart=0`, this factor 
  !! is used for the whole magnet 
  
  real(dp) :: frholeg = 1.0D0
  !! Ouboard legs resistivity enhancement factor. Only used for `itart=1`.
  
  integer :: i_cp_joints = -1 
  !! Switch for CP demoutable joints type
  !!  -= 0 : Clampled joints
  !!  -= 1 : Sliding joints
  !! Default value (-1) choses : 
  !!   Sliding joints for resistive magnets (i_tf_sup = 0, 2)  
  !!   Clampled joints for superconducting magents (i_tf_sup = 1)

  real(dp) :: rho_tf_joints = 2.5D-10
  !! TF joints surfacic resistivity [ohm.m]. Feldmetal joints assumed.

  integer :: n_tf_joints_contact = 6
  !! Number of contact per sliding joint

  integer :: n_tf_joints = 4
  !! Number of joint per turn

  real(dp) :: th_joint_contact = 0.03D0
  !! TF sliding joints contact pad width [m]

  real(dp) :: pres_joints = 0.0D0
  !! Calculated TF joints resistive power losses [W]

  real(dp) :: tfleng = 0.0D0
  !! TF coil circumference (m)

  real(dp) :: eff_tf_cryo = -1.0D0
  !! TF cryoplant efficiency (compared to pefect Carnot cycle).
  !! Using -1 set the default value depending on magnet technology:
  !!
  !!  - i_tf_sup = 1 : SC magnet, eff_tf_cryo = 0.13 (ITER design)
  !!  - i_tf_sup = 2 : Cryo-aluminium, eff_tf_cryo = 0.4

  real(dp) :: n_tf = 16.0D0
  !! Number of TF coils (default = 50 for stellarators). Number of TF coils outer legs for ST

  real(dp) :: tfocrn = 0.0D0
  !! TF coil half-width - outer bore (m)

  real(dp) :: tfsai = 0.0D0
  !! area of the inboard TF coil legs (m2)

  real(dp) :: tfsao = 0.0D0
  !! area of the outboard TF coil legs (m2)

  real(dp), bind(C) :: tftmp = 4.5D0
  !! peak helium coolant temperature in TF coils and PF coils (K)

  real(dp) :: tftort = 1.0D0
  !! TF coil toroidal thickness (m)

  real(dp) :: thicndut = 8.0D-4
  !! conduit insulation thickness (m)

  real(dp) :: layer_ins = 0.0D0
  !! Additional insulation thickness between layers (m)

  real(dp) :: thkcas = 0.3D0
  !! inboard TF coil case outer (non-plasma side) thickness (m) (`iteration variable 57`)
  !! (calculated for stellarators)

  real(dp) :: dr_tf_wp = 0.0D0
  !! radial thickness of winding pack (m) (`iteration variable 140`) (issue #514)

  real(dp) :: thwcndut = 8.0D-3
  !! TF coil conduit case thickness (m) (`iteration variable 58`)
  
  real(dp) :: tinstf = 0.018D0
  !! Thickness of the ground insulation layer surrounding (m) 
  !! 
  !!   - Superconductor TF (`i_tf_sup == 1`) : The TF Winding packs
  !!   - Resistive magnets (`i_tf_sup /= 1`) : The TF turns
  !!
  !! Rem : The default value includes allowance for 10 mm insertion gap.
  !! Rem : Thickness calculated for stellarators.

  real(dp), bind(C) :: tmargmin_tf = 0D0
  !! minimum allowable temperature margin : TF coils (K)

  real(dp), bind(C) :: tmargmin_cs = 0D0
  !! minimum allowable temperature margin : CS (K)

  real(dp) :: tmargmin = 0D0
  !! minimum allowable temperature margin : TFC AND CS (K)

  real(dp), bind(C) :: temp_margin = 0.00D0
  !! temperature margin (K)

  real(dp) :: tmargtf = 0.0D0
  !! TF coil temperature margin (K)

  real(dp) :: tmaxpro = 150.0D0
  !! maximum temp rise during a quench for protection (K)

  real(dp) :: tmax_croco = 200.0D0
  !! CroCo strand: maximum permitted temp during a quench (K)

  real(dp) :: croco_quench_temperature = 0D0
  !! CroCo strand: Actual temp reached during a quench (K)

  real(dp) :: tmpcry = 4.5D0
  !! coil temperature for cryogenic plant power calculation (K)

  real(dp) :: n_tf_turn = 0.0D0
  !! number of turns per TF coil

  real(dp) :: vdalw = 20.0D0
  !! max voltage across TF coil during quench (kV) (`iteration variable 52`)

  real(dp) :: vforce = 0.0D0
  !! vertical tension on inboard leg/coil (N)
  
  real(dp) :: f_vforce_inboard = 0.5D0
  !! Fraction of the total vertical force taken by the TF inboard leg tension
  !! Not used for resistive `itart=1` (sliding joints)

  real(dp) :: vforce_outboard = 0.0D0
  !! Vertical tension on outboard leg/coil (N)

  real(dp) :: vftf = 0.4D0
  !! coolant fraction of TFC 'cable' (`i_tf_sup=1`), or of TFC leg (`i_tf_ssup=0`)

  real(dp) :: voltfleg = 0.0D0
  !! volume of each TF coil outboard leg (m3)

  real(dp) :: vtfkv = 0.0D0
  !! TF coil voltage for resistive coil including bus (kV)

  real(dp) :: vtfskv = 0.0D0
  !! voltage across a TF coil during quench (kV)

  real(dp) :: whtcas = 0.0D0
  !! mass per coil of external case (kg)

  real(dp) :: whtcon = 0.0D0
  !! TF coil conductor mass per coil (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf

  real(dp) :: whtconcu = 0.0D0
  !! copper mass in TF coil conductor (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf
  
  real(dp) :: whtconal = 0.0D0
  !! Aluminium mass in TF coil conductor (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf
  
  real(dp) :: whtconin = 0.0D0
  !! conduit insulation mass in TF coil conductor (kg/coil)

  real(dp) :: whtconsc = 0.0D0
  !! superconductor mass in TF coil cable (kg/coil)

  real(dp) :: whtconsh = 0.0D0
  !! steel conduit mass in TF coil conductor (kg/coil)

  real(dp) :: whtgw = 0.0D0
  !! mass of ground-wall insulation layer per coil (kg/coil)

  real(dp) :: whttf = 0.0D0
  !! total mass of the TF coils (kg)

  real(dp) :: windstrain = 0.0D0
  !! longitudinal strain in winding pack

  real(dp) :: wwp1 = 0.0D0
  !! width of first step of winding pack (m)

  real(dp) :: wwp2 = 0.0D0
  !! width of second step of winding pack (m)

  ! Superconducting TF coil shape parameters</B> (see also farc4tf);
  ! the TF inner surface top half is approximated by four circular arcs.
  ! Arc 1 goes through points 1 and 2 on the inner surface. Arc 2
  ! goes through points 2 and 3, etc.
  real(dp), dimension(4) :: dthet = 0.0D0
  !! angle of arc i (rad)

  real(dp), dimension(4) :: radctf = 0.0D0
  !! radius of arc i (m)

  real(dp), dimension(5) :: xarc = 0.0D0
  !! x location of arc point i on surface (m)

  real(dp), dimension(4) :: xctfc = 0.0D0
  !! x location of arc centre i (m)

  real(dp), dimension(5) :: yarc = 0.0D0
  !! y location of arc point i on surface (m)

  real(dp), dimension(4) :: yctfc = 0.0D0
  !! y location of arc centre i (m)

  ! New TF shape:  Horizontal and vertical radii of inside edge of TF coil
  ! Arcs are numbered clockwise:
  ! 1=upper inboard, 2=upper outboard, 3=lower ouboard, 4=lower inboard

  real(dp), dimension(4) :: tfa = 0.0D0
  !! Horizontal radius of inside edge of TF coil (m)

  real(dp), dimension(4) :: tfb = 0.0D0
  !! Vertical radius of inside edge of TF coil (m)
  ! Quantities relating to the spherical tokamak model (itart=1)
  ! (and in some cases, also to resistive TF coils, i_tf_sup=0):

  real(dp) :: drtop = 0.0D0
  !! centrepost taper maximum radius adjustment (m)

  real(dp) :: dztop = 0.0D0
  !! centrepost taper height adjustment (m)

  real(dp) :: etapump = 0.8D0
  !! centrepost coolant pump efficiency

  real(dp) :: fcoolcp = 0.3D0
  !! coolant fraction of TF coil inboard legs (`iteration variable 23`)

  real(dp) :: fcoolleg = 0.2D0
  !! coolant fraction of TF coil inboard legs
  
  real(dp) :: a_cp_cool = 0.0D0
  !! Centrepost cooling area toroidal cross-section (constant over the whole CP)

  real(dp) :: ncool = 0.0D0
  !! number of centrepost coolant tubes

  real(dp) :: ppump = 0.0D0
  !! centrepost coolant pump power (W)

  real(dp) :: prescp = 0.0D0
  !! resistive power in the centrepost (itart=1) [W].
  !! If `itart=0`, this variable is the ressitive power on the whole magnet

  real(dp) :: presleg = 0.0D0
  !! Summed resistive power in the TF coil legs [W]. Remain 0 if `itart=0`.
   
  real(dp) :: ptempalw = 473.15D0   ! 200 C
  !! maximum peak centrepost temperature (K) (`constraint equation 44`)

  real(dp) :: rcool = 0.005D0
  !! average radius of coolant channel (m) (`iteration variable 69`)

  real(dp) :: tcoolin = 313.15D0   ! 40 C
  !! centrepost coolant inlet temperature (K)

  real(dp) :: dtiocool = 0.0D0
  !! inlet / outlet TF coil coolant temperature rise (K)  

  real(dp) :: tcpav = 373.15D0     ! 100 C
  !! Average temperature of centrepost called CP (K). Only used for resistive coils 
  !! to compute the resisitive heating. Must be an iteration variable for 
  !! ST (`itart=1`) (`iteration variable 20`)

  real(dp) :: tcpav2 = 0.0D0
  !! Computed centrepost average temperature (K) (for consistency)

  real(dp) :: tlegav = -1.0D0 
  !! Average temperature of the TF outboard legs [K]. If `tlegav=-1.0`, the ouboard 
  !! legs and CP temperatures are the same. Fixed for now, should use a contraints eq like tcpav 

  real(dp) :: tcpmax = 0.0D0
  !! peak centrepost temperature (K)
  
  real(dp) :: vcool = 20.0D0
  !! inlet centrepost coolant flow speed at midplane (m/s) (`iteration variable 70`)

  real(dp) :: vol_cond_cp = 0.0D0
  !! Exact conductor volume in the centrepost (m3)
  
  real(dp) :: whtcp = 0.0D0
  !! mass of TF coil inboard legs (kg)

  real(dp) :: whttflgs = 0.0D0
  !! mass of the TF coil legs (kg)

end module tfcoil_variables

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

module heat_transport_variables
  !! author: J. Morris, M. Kovari (UKAEA)
  !!
  !! This module contains global variables relating to the heat transport system 
  !! of a fusion power plant, and also those for a hydrogen production plant.
  !!
  !!### References 
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: baseel = 5.0D6
  !! base plant electric load (W)

  real(dp), bind(C) :: crypmw = 0.0D0
  !! cryogenic plant power (MW)

  real(dp) :: etatf = 0.9D0
  !! AC to resistive power conversion for TF coils

  real(dp) :: etath = 0.35D0
  !! thermal to electric conversion efficiency if `secondary_cycle=2`; otherwise calculated.

  real(dp), bind(C) :: fachtmw = 0.0D0
  !! facility heat removal (MW)

  real(dp), bind(C) :: fcsht = 0.0D0
  !! total baseline power required at all times (MW)

  real(dp) :: fgrosbop = 0.0D0
  !! scaled fraction of gross power to balance-of-plant

  real(dp) :: fmgdmw = 0.0D0
  !! power to mgf (motor-generator flywheel) units (MW) (ignored if `iscenr=2`)

  real(dp) :: fpumpblkt = 0.005D0
  !! fraction of total blanket thermal power required to drive the blanket 
  !! coolant pumps (default assumes water coolant) (`secondary_cycle=0`)

  real(dp) :: fpumpdiv = 0.005D0
  !! fraction of total divertor thermal power required to drive the divertor 
  !! coolant pumps (default assumes water coolant)

  real(dp) :: fpumpfw = 0.005D0
  !! fraction of total first wall thermal power required to drive the FW coolant 
  !! pumps (default assumes water coolant) (`secondary_cycle=0`)

  real(dp) :: fpumpshld = 0.005D0
  !! fraction of total shield thermal power required to drive the shield coolant 
  !! pumps (default assumes water coolant)

  real(dp) :: htpmw_min = 0.0D0
  !! Minimum total electrical power for primary coolant pumps (MW) (NOT RECOMMENDED)

  real(dp), bind(C) :: helpow = 0.0D0
  !! heat removal at cryogenic temperatures (W)

  real(dp) :: htpmw = 0.0D0
  !! heat transport system electrical pump power (MW)

  real(dp) :: htpmw_blkt = 0.0D0
  !! blanket coolant mechanical pumping power (MW)

  real(dp) :: htpmw_div = 0.0D0
  !! divertor coolant mechanical pumping power (MW)

  real(dp) :: htpmw_fw = 0.0D0
  !! first wall coolant mechanical pumping power (MW)

  real(dp) :: htpmw_shld = 0.0D0
  !! shield and vacuum vessel coolant mechanical pumping power (MW)

  real(dp) :: htpsecmw = 0.0D0
  !! Waste power lost from primary coolant pumps (MW)

  integer :: ipowerflow = 1
  !! switch for power flow model:
  !!
  !! - =0 pre-2014 version
  !! - =1 comprehensive 2014 model

  integer :: iprimnloss = 0
  !! switch for lost neutron power through holes destiny (ipowerflow=0):
  !!
  !! - =0 does not contribute to energy generation cycle
  !! - =1 contributes to energy generation cycle

  integer :: iprimshld = 1
  !! Switch for shield thermal power destiny:
  !!
  !! - =0 does not contribute to energy generation cycle
  !! - =1 contributes to energy generation cycle

  integer, bind(C) :: nphx = 0
  !! number of primary heat exchangers

  real(dp), bind(C) :: pacpmw = 0.0D0
  !! total pulsed power system load (MW)

  real(dp) :: peakmva = 0.0D0
  !! peak MVA requirement

  real(dp), bind(C) :: pfwdiv = 0.0D0
  !! heat removal from first wall/divertor (MW)

  real(dp), bind(C) :: pgrossmw = 0.0D0
  !! gross electric power (MW)

  real(dp), bind(C) :: pinjht = 0.0D0
  !! power dissipated in heating and current drive system (MW)

  real(dp) :: pinjmax = 120.0D0
  !! maximum injector power during pulse (heating and ramp-up/down phase) (MW)

  real(dp), bind(C) :: pinjwp = 0.0D0
  !! injector wall plug power (MW)

  real(dp) :: pinjwpfix = 0.0D0
  !! secondary injector wall plug power (MW)

  real(dp) :: pnetelmw = 0.0D0
  !! net electric power (MW)

  real(dp) :: precircmw = 0.0D0
  !! recirculating electric power (MW)

  real(dp) :: priheat = 0.0D0
  !! total thermal power removed from fusion core (MW)

  real(dp) :: psecdiv = 0.0D0
  !! Low-grade heat lost in divertor (MW)

  real(dp) :: psechcd = 0.0D0
  !! Low-grade heat lost into HCD apparatus (MW)

  real(dp) :: psechtmw = 0.0D0
  !! Low-grade heat (MW)

  real(dp) :: pseclossmw = 0.0D0
  !! Low-grade heat (VV + lost)(MW)

  real(dp) :: psecshld = 0.0D0
  !! Low-grade heat deposited in shield (MW)

  real(dp), bind(C) :: pthermmw = 0.0D0
  !! High-grade heat useful for electric production (MW)

  real(dp) :: pwpm2 = 150.0D0
  !! base AC power requirement per unit floor area (W/m2)

  real(dp) :: tfacpd = 0.0D0
  !! total steady state TF coil AC power demand (MW)

  real(dp), bind(C) :: tlvpmw = 0.0D0
  !! estimate of total low voltage power (MW)

  real(dp), bind(C) :: trithtmw = 15.0D0
  !! power required for tritium processing (MW)

  real(dp) :: tturb = 0.0D0
  !! coolant temperature at turbine inlet (K) (`secondary_cycle = 3,4`)

  real(dp), bind(C) :: vachtmw = 0.5D0
  !! vacuum pump power (MW)

end module heat_transport_variables

module times_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the plasma pulse timings
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: pulsetimings = 1.0D0
  !! Switch for pulse timings (if lpulse=1):
  !!
  !! - =0, tohs = Ip(MA)/0.1 tramp, tqnch = input
  !! - =1, tohs = iteration var or input. tramp/tqnch max of input or tohs

  real(dp), bind(C) :: tburn = 1000.0D0
  !! burn time (s) (calculated if `lpulse=1`)

  real(dp) :: tburn0 = 0.0D0
  !! burn time (s) - used for internal consistency

  real(dp), bind(C) :: tcycle = 0.0D0
  !! full cycle time (s)

  real(dp), bind(C) :: tdown = 0.0D0
  !! down time (s)

  real(dp), bind(C) :: tdwell = 1800.0D0
  !! time between pulses in a pulsed reactor (s) (`iteration variable 17`)

  real(dp), bind(C) :: theat = 10.0D0
  !! heating time, after current ramp up (s)

  real(dp), dimension(6) :: tim = 0.0D0
  !! array of time points during plasma pulse (s)

  character(len=11), dimension(6) :: timelabel = (/ 'Start', &
    'BOP  ', &
    'EOR  ', &
    'BOF  ', &
    'EOF  ', &
    'EOP  ' /)
  !! array of time labels during plasma pulse (s)

  character(len=11), dimension(5) :: intervallabel = (/ 'tramp', &
    'tohs ', &
    'theat', &
    'tburn', &
    'tqnch' /)
  !! time intervals - as strings (s)

  real(dp), bind(C) :: tohs = 30.0D0
  !! plasma current ramp-up time for current initiation (s) (calculated if `lpulse=0`)
  !! (`iteration variable 65`)

  real(dp) :: tohsin = 0.0D0
  !! Switch for plasma current ramp-up time (if lpulse=0):
  !!
  !! - = 0, tohs = tramp = tqnch = Ip(MA)/0.5
  !! - <>0, tohs = tohsin; tramp, tqnch are input

  real(dp) :: tpulse = 0.0D0
  !! pulse length = tohs + theat + tburn + tqnch

  real(dp) :: tqnch = 15.0D0
  !! shut down time for PF coils (s); if pulsed, = tohs

  real(dp) :: tramp = 15.0D0
  !! initial PF coil charge time (s); if pulsed, = tohs

end module times_variables

module buildings_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the plant buildings
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: admv = 1.0D5
  !! administration building volume (m3)

  real(dp) :: admvol = 0.0D0
  !! volume of administration buildings (m3)

  real(dp) :: clh1 = 2.5D0
  !! vertical clearance from TF coil to cryostat (m) (calculated for tokamaks)

  real(dp) :: clh2 = 15.0D0
  !! clearance beneath TF coil to foundation (including basement) (m)

  real(dp) :: conv = 6.0D4
  !! control building volume (m3)

  real(dp) :: convol = 0.0D0
  !! volume of control, protection and i&c building (m3)

  real(dp) :: cryvol = 0.0D0
  !! volume of cryoplant building (m3)

  real(dp) :: efloor = 0.0D0
  !! effective total floor space (m2)

  real(dp) :: elevol = 0.0D0
  !! volume of electrical equipment building (m3)

  real(dp) :: esbldgm3 = 1.0D3
  !! volume of energy storage equipment building (m3) (not used if `lpulse=0`)

  real(dp) :: fndt = 2.0D0
  !! foundation thickness (m)

  real(dp) :: hccl = 5.0D0
  !! clearance around components in hot cell (m)

  real(dp) :: hcwt = 1.5D0
  !! hot cell wall thickness (m)

  real(dp) :: mbvfac = 2.8D0
  !! maintenance building volume multiplication factor

  real(dp) :: pfbldgm3 = 2.0D4
  !! volume of PF coil power supply building (m3)

  real(dp) :: pibv = 2.0D4
  !! power injection building volume (m3)

  real(dp) :: rbrt = 1.0D0
  !! reactor building roof thickness (m)

  real(dp) :: rbvfac = 1.6D0
  !! reactor building volume multiplication factor

  real(dp) :: rbvol = 0.0D0
  !! reactor building volume (m3)

  real(dp) :: rbwt = 2.0D0
  !! reactor building wall thickness (m)

  real(dp) :: rmbvol = 0.0D0
  !! volume of maintenance and assembly building (m3)

  real(dp) :: row = 4.0D0
  !! clearance to building wall for crane operation (m)

  real(dp) :: rxcl = 4.0D0
  !! clearance around reactor (m)

  real(dp) :: shmf = 0.5D0
  !! fraction of shield mass per TF coil to be moved in the maximum shield lift

  real(dp) :: shov = 1.0D5
  !! shops and warehouse volume (m3)

  real(dp) :: shovol = 0.0D0
  !! volume of shops and buildings for plant auxiliaries (m3)

  real(dp) :: stcl = 3.0D0
  !! clearance above crane to roof (m)

  real(dp) :: tfcbv = 2.0D4
  !! volume of TF coil power supply building (m3) (calculated if TF coils are superconducting)

  real(dp) :: trcl = 1.0D0
  !! transportation clearance between components (m)

  real(dp) :: triv = 4.0D4
  !! volume of tritium, fuel handling and health physics buildings (m3)

  real(dp) :: volnucb = 0.0D0
  !! sum of nuclear buildings volumes (m3)

  real(dp), bind(C) :: volrci = 0.0D0
  !! internal volume of reactor building (m3)

  real(dp) :: wgt = 5.0D5
  !! reactor building crane capacity (kg) (calculated if 0 is input)

  real(dp) :: wgt2 = 1.0D5
  !! hot cell crane capacity (kg) (calculated if 0 is input)

  real(dp) :: wrbi = 0.0D0
  !! distance from centre of machine to building wall (m), i.e. reactor building half-width

  real(dp) :: wsvfac = 1.9D0
  !! warm shop building volume multiplication factor

  real(dp), bind(C) :: wsvol = 0.0D0
  !! volume of warm shop building (m3)

end module buildings_variables

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

  real(dp) :: available_radial_space = 0.0D0
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

  real(dp) :: r_sh_inboard_in = 0.0D0
  !! Radial inner side position of inboard neutronic shield [m]

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

module cost_variables
  !! author: J. Morris, S. Muldrew, M. Kovari (UKAEA)
  !!
  !! Module containing global variables relating to the costing algorithms of a fusion power plant.
  !!
  !!### References 
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp), bind(C) :: abktflnc = 5.0D0
  !! allowable first wall/blanket neutron fluence (MW-yr/m2) (`blktmodel=0`)

  real(dp), bind(C) :: adivflnc = 7.0D0
  !! allowable divertor heat fluence (MW-yr/m2)

  real(dp) :: blkcst = 0.0D0
  !! blanket direct cost (M$)

  real(dp) :: c221 = 0.0D0
  !! total account 221 cost (M$) - first wall, blanket, shield, support structure and div plates

  real(dp) :: c222 = 0.0D0
  !! total account 222 cost (M$) - TF coils + PF coils

  real(dp) :: capcost = 0.0D0
  !! total capital cost including interest (M$)

  real(dp) :: cconfix = 80.0D0
  !! fixed cost of superconducting cable ($/m)

  real(dp) :: cconshpf = 70.0D0
  !! cost of PF coil steel conduit/sheath ($/m)

  real(dp) :: cconshtf = 75.0D0
  !! cost of TF coil steel conduit/sheath ($/m)

  real(dp) :: cdcost = 0.0D0
  !! current drive direct costs (M$)

  real(dp), bind(C) :: cdirt = 0.0D0
  !! total plant direct cost (M$)

  real(dp), bind(C) :: cdrlife = 0.0D0
  !! lifetime of heating/current drive system (y)

  real(dp) :: cfactr = 0.75D0
  !! Total plant availability fraction; input if `iavail=0`

  real(dp) :: cpfact = 0.0D0
  !! Total plant capacity factor
  
  real(dp), dimension(4) :: cfind = (/0.244D0, 0.244D0, 0.244D0, 0.29D0/)
  !! indirect cost factor (func of lsa)

  real(dp) :: cland = 19.2D0
  !! cost of land (M$)

  real(dp) :: coe = 0.0D0
  !! cost of electricity ($/MW-hr)

  real(dp) :: coecap = 0.0D0
  !! capital cost of electricity (m$/kW-hr)

  real(dp) :: coefuelt = 0.0D0
  !! 'fuel' (including replaceable components) contribution to cost of electricity (m$/kW-hr)

  real(dp) :: coeoam = 0.0D0
  !! operation and maintenance contribution to cost of electricity (m$/kW-hr)

  real(dp) :: concost = 0.0D0
  !! plant construction cost (M$)

  real(dp) :: costexp = 0.8D0
  !! cost exponent for scaling in 2015 costs model

  real(dp) :: costexp_pebbles = 0.6D0
  !! cost exponent for pebbles in 2015 costs model

  real(dp) :: cost_factor_buildings = 1.0D0
  !! cost scaling factor for buildings

  real(dp) :: cost_factor_land = 1.0D0
  !! cost scaling factor for land

  real(dp) :: cost_factor_tf_coils = 1.0D0
  !! cost scaling factor for TF coils

  real(dp) :: cost_factor_fwbs = 1.0D0
  !! cost scaling factor for fwbs

  real(dp) :: cost_factor_rh = 1.0D0
  !! cost scaling factor for remote handling

  real(dp) :: cost_factor_vv = 1.0D0
  !! cost scaling factor for vacuum vessel

  real(dp) :: cost_factor_bop = 1.0D0
  !! cost scaling factor for energy conversion system

  real(dp) :: cost_factor_misc = 1.0D0
  !! cost scaling factor for remaining subsystems

  real(dp) :: maintenance_fwbs = 0.2D0
  !! Maintenance cost factor: first wall, blanket, shield, divertor

  real(dp) :: maintenance_gen = 0.05D0
  !! Maintenance cost factor: All other components except coils, vacuum vessel, 
  !! thermal shield, cryostat, land

  real(dp) :: amortization = 13.6D0
  !! amortization factor (fixed charge factor) "A" (years)

  integer :: cost_model = 1
  !! Switch for cost model:
  !!
  !! - =0 use $ 1990 PROCESS model
  !! - =1 use $ 2014 Kovari model
  !! - =2 use $ 1980 STEP model (NOT RECOMMENDED - Under Development)

  integer :: i_cp_lifetime = 0
  !! Switch for the centrepost lifetime constraint 
  !!  0 : The CP full power year lifetime is set by the user
  !!  1 : The CP lifetime is equal to the divertor lifetime
  !!  2 : The CP lifetime is equal to the breeding blankets lifetime
  !!  3 : The CP lifetime is equal to the plant lifetime

  real(dp), bind(C) :: cowner = 0.15D0
  !! owner cost factor

  real(dp) :: cplife_input = 2.0D0
  !! User input full power year lifetime of the centrepost (years)

  real(dp) :: cplife = 0.0D0
  !! Calculated full power year lifetime of centrepost (years)

  real(dp) :: cpstcst = 0.0D0
  !! ST centrepost direct cost (M$)

  real(dp), bind(C) :: cpstflnc = 10.0D0
  !! allowable ST centrepost neutron fluence (MW-yr/m2)

  real(dp) :: crctcore = 0.0D0
  !! reactor core costs (categories 221, 222 and 223)

  real(dp) :: csi = 16.0D0
  !! allowance for site costs (M$)

  real(dp) :: cturbb = 38.0D0
  !! cost of turbine building (M$)

  real(dp) :: decomf = 0.1D0
  !! proportion of constructed cost required for decommissioning fund

  real(dp) :: dintrt = 0.0D0
  !! diff between borrowing and saving interest rates

  real(dp) :: divcst = 0.0D0
  !! divertor direct cost (M$)

  real(dp), bind(C) :: divlife = 0.0D0
  !! Full power lifetime of divertor (y)

  real(dp) :: dtlife = 0.0D0
  !! period prior to the end of the plant life that the decommissioning fund is used (years)

  real(dp) :: fcap0 = 1.165D0
  !! average cost of money for construction of plant assuming design/construction time of six years

  real(dp) :: fcap0cp = 1.08D0
  !! average cost of money for replaceable components assuming lead time for these of two years

  real(dp) :: fcdfuel = 0.1D0
  !! fraction of current drive cost treated as fuel (if `ifueltyp = 1`)

  real(dp), bind(C) :: fcontng = 0.195D0
  !! project contingency factor

  real(dp) :: fcr0 = 0.0966D0
  !! fixed charge rate during construction

  real(dp), bind(C) :: fkind = 1.0D0
  !! multiplier for Nth of a kind costs

  real(dp) :: fwallcst = 0.0D0
  !! first wall cost (M$)

  integer :: iavail= 2
  !! Switch for plant availability model:
  !!
  !! - =0 use input value for cfactr
  !! - =1 calculate cfactr using Taylor and Ward 1999 model
  !! - =2 calculate cfactr using new (2015) model

  real(dp) :: avail_min = 0.75D0
  !! Minimum availability (`constraint equation 61`)

  real(dp) :: tok_build_cost_per_vol = 1283.0D0
  !! Unit cost for tokamak complex buildings, including building and site services ($/m3)

  real(dp) :: light_build_cost_per_vol = 270.0D0
  !! Unit cost for unshielded non-active buildings ($/m3)

  real(dp) :: favail = 1.0D0
  !! F-value for minimum availability (`constraint equation 61`)

  integer, bind(C) :: num_rh_systems = 4
  !! Number of remote handling systems (1-10)

  real(dp), bind(C) :: conf_mag = 0.99D0
  !! c parameter, which determines the temperature margin at which magnet lifetime starts to decline

  real(dp), bind(C) :: div_prob_fail = 0.0002D0
  !! Divertor probability of failure (per op day)

  real(dp), bind(C) :: div_umain_time = 0.25D0
  !! Divertor unplanned maintenance time (years)

  real(dp), bind(C) :: div_nref = 7000.0D0
  !! Reference value for cycle cycle life of divertor

  real(dp), bind(C) :: div_nu = 14000.0D0
  !! The cycle when the divertor fails with 100% probability

  real(dp),bind(C) :: fwbs_nref = 20000.0D0
  !! Reference value for cycle life of blanket

  real(dp), bind(C) :: fwbs_nu = 40000.0D0
  !! The cycle when the blanket fails with 100% probability

  real(dp), bind(C) :: fwbs_prob_fail = 0.0002D0
  !! Fwbs probability of failure (per op day)

  real(dp), bind(C) :: fwbs_umain_time = 0.25D0
  !! Fwbs unplanned maintenance time (years)

  real(dp) :: redun_vacp = 25.0D0
  !! Vacuum system pump redundancy level (%)

  integer :: redun_vac = 0
  !! Number of redundant vacuum pumps

  real(dp), bind(C) :: t_operation = 0.0D0
  !! Operational time (yrs)

  real(dp) :: tbktrepl = 0.5D0
  !! time taken to replace blanket (y) (`iavail=1`)

  real(dp) :: tcomrepl = 0.5D0
  !! time taken to replace both blanket and divertor (y) (`iavail=1`)

  real(dp) :: tdivrepl = 0.25D0
  !! time taken to replace divertor (y) (`iavail=1`)

  real(dp) :: uubop = 0.02D0
  !! unplanned unavailability factor for balance of plant (`iavail=1`)

  real(dp) :: uucd = 0.02D0
  !! unplanned unavailability factor for current drive (`iavail=1`)

  real(dp) :: uudiv = 0.04D0
  !! unplanned unavailability factor for divertor (`iavail=1`)

  real(dp) :: uufuel = 0.02D0
  !! unplanned unavailability factor for fuel system (`iavail=1`)

  real(dp) :: uufw = 0.04D0
  !! unplanned unavailability factor for first wall (`iavail=1`)

  real(dp) :: uumag = 0.02D0
  !! unplanned unavailability factor for magnets (`iavail=1`)

  real(dp) :: uuves = 0.04D0
  !! unplanned unavailability factor for vessel (`iavail=1`)

  integer :: ifueltyp = 0
  !! Switch for fuel type:
  !!
  !! - =2 treat initial blanket, divertor, first wall
  !!   as capital costs. Treat all later items and 
  !!   fraction fcdfuel of CD equipment as fuel costs
  !! - =1 treat blanket divertor, first wall and
  !!   fraction fcdfuel of CD equipment as fuel cost
  !! - =0 treat these as capital cost

  integer :: ipnet = 0
  !! Switch for net electric power calculation:
  !!
  !! - =0 scale so that always > 0
  !! - =1 let go < 0 (no c-o-e)

  integer, bind(C) :: ireactor = 1
  !! Switch for net electric power and cost of electricity calculations:
  !!
  !! - =0 do not calculate MW(electric) or c-o-e
  !! - =1 calculate MW(electric) and c-o-e

  integer, bind(C) :: lsa = 4
  !! Level of safety assurance switch (generally, use 3 or 4):
  !!
  !! - =1 truly passively safe plant
  !! - =2,3 in-between
  !! - =4 like current fission plant

  real(dp) :: moneyint = 0.0D0
  !! interest portion of capital cost (M$)

  integer :: output_costs = 1
  !! Switch for costs output:
  !!
  !! - =0 do not write cost-related outputs to file
  !! - =1 write cost-related outputs to file

  real(dp) :: ratecdol = 0.0435D0
  !! effective cost of money in constant dollars

  real(dp) :: step_con = 1.5D-1
  !! Contingency Percentage

  real(dp), dimension(68) :: step_ref = &
    (/ 3.0D0, 3.0D-1, 1.115D1, 1.5744D2, 3.592D1, 7.96D0, 9.16D0, 3.26D0, 5.369D1, &
    1.88D0, 6.6D-1, 8.63D0, 3.1D0, 2.05D0, 8.7D-1, 8.7D-1, 9.1D-1, 3.1D-1, 1.81D0, &
    8.236D1, 1.8607D2, 1.2572D2, 3.46D1, 7.25D0, 4.0D0, 3.349D1, 5.274D1, 4.86D0, &
    5.29D1, 2.45D0, 2.82D0, 1.676D1, 6.984D1, 7.7D0, 3.6D0, 2.8D0, 8.0D-1, 1.7D0, &
    1.8D0, 1.3D0, 3.86D1, 3.83D1, 0.0D0, 2.4D-1, 8.0D-2, 0.0D0, 2.0D0, 1.97D0, 1.16D0, &
    2.341D1, 7.733D1, 4.37D0, 4.434D1, 1.918D1, 9.39D0, 5.084D1, 8.7D0, 1.239D1, &
    1.704D1, 7.8D0, 2.11D0, 1.74D1, 3.599D1, 8.2D0, 1.568D1, 1.235D1, 6.22D0, 7.5D-1 /)
  !! Reference values for cost model 2

  real(dp), bind(C) :: tlife = 30.0D0
  !! Full power year plant lifetime (years)

  real(dp), parameter :: ucad = 180.0D0
  !! unit cost for administration buildings (M$/m3)

  real(dp), parameter :: ucaf = 1.5D6
  !! unit cost for aux facility power equipment ($)

  real(dp), parameter :: ucahts = 31.0D0
  !! unit cost for aux heat transport equipment ($/W**exphts)

  real(dp), parameter :: ucap = 17.0D0
  !! unit cost of auxiliary transformer ($/kVA)

  real(dp) :: ucblbe = 260.0D0
  !! unit cost for blanket beryllium ($/kg)

  real(dp) :: ucblbreed = 875.0D0
  !! unit cost for breeder material ($/kg) (`blktmodel>0`)

  real(dp) :: ucblli = 875.0D0
  !! unit cost for blanket lithium ($/kg) (30% Li6)

  real(dp) :: ucblli2o = 600.0D0
  !! unit cost for blanket Li_2O ($/kg)

  real(dp) :: ucbllipb = 10.3D0
  !! unit cost for blanket Li-Pb ($/kg) (30% Li6)

  real(dp) :: ucblss = 90.0D0
  !! unit cost for blanket stainless steel ($/kg)

  real(dp) :: ucblvd = 200.0D0
  !! unit cost for blanket vanadium ($/kg)

  real(dp), parameter :: ucbpmp = 2.925D5
  !! vacuum system backing pump cost ($)

  real(dp) :: ucbus = 0.123D0
  !! cost of aluminium bus for TF coil ($/A-m)

  real(dp) :: uccase = 50.0D0
  !! cost of superconductor case ($/kg)

  real(dp), parameter :: ucco = 350.0D0
  !! unit cost for control buildings (M$/m3)

  real(dp) :: uccpcl1 = 250.0D0
  !! cost of high strength tapered copper ($/kg)

  real(dp) :: uccpclb = 150.0D0
  !! cost of TF outboard leg plate coils ($/kg)

  real(dp), parameter :: uccpmp = 3.9D5
  !! vacuum system cryopump cost ($)

  real(dp), parameter :: uccr = 460.0D0
  !! unit cost for cryogenic building (M$/vol)

  real(dp), bind(C) :: uccry = 9.3D4
  !! heat transport system cryoplant costs ($/W**expcry)

  real(dp) :: uccryo = 32.0D0
  !! unit cost for vacuum vessel ($/kg)

  real(dp) :: uccu = 75.0D0
  !! unit cost for copper in superconducting cable ($/kg)

  real(dp), parameter :: ucdgen = 1.7D6
  !! cost per 8 MW diesel generator ($)

  real(dp) :: ucdiv = 2.8D5
  !! cost of divertor blade ($)

  real(dp), parameter :: ucdtc = 13.0D0
  !! detritiation, air cleanup cost ($/10000m3/hr)

  real(dp), parameter :: ucduct = 4.225D4
  !! vacuum system duct cost ($/m)

  real(dp) :: ucech = 3.0D0
  !! ECH system cost ($/W)

  real(dp), parameter :: ucel = 380.0D0
  !! unit cost for electrical equipment building (M$/m3)

  real(dp), parameter :: uces1 = 3.2D4
  !! MGF (motor-generator flywheel) cost factor ($/MVA**0.8)
  
  real(dp), parameter :: uces2 = 8.8D3
  !! MGF (motor-generator flywheel) cost factor ($/MJ**0.8)

  real(dp), bind(C) :: ucf1 = 2.23D7
  !! cost of fuelling system ($)

  real(dp) :: ucfnc = 35.0D0
  !! outer PF coil fence support cost ($/kg)

  real(dp), parameter :: ucfpr = 4.4D7
  !! cost of 60g/day tritium processing unit ($)

  real(dp) :: ucfuel = 3.45D0
  !! unit cost of D-T fuel (M$/year/1200MW)

  real(dp), parameter :: ucfwa = 6.0D4
  !! first wall armour cost ($/m2)

  real(dp), parameter :: ucfwps = 1.0D7
  !! first wall passive stabiliser cost ($)

  real(dp), parameter :: ucfws = 5.3D4
  !! first wall structure cost ($/m2)
  
  real(dp), parameter :: ucgss = 35.0D0
  !! cost of reactor structure ($/kg)

  real(dp) :: uche3 = 1.0D6
  !! cost of helium-3 ($/kg)

  real(dp), bind(C) :: uchrs = 87.9D6
  !! cost of heat rejection system ($)

  real(dp), dimension(2) :: uchts = (/15.3D0, 19.1D0/)
  !! cost of heat transport system equipment per loop ($/W); dependent on coolant type (coolwh)

  real(dp), bind(C) :: uciac = 1.5D8
  !! cost of instrumentation, control & diagnostics ($)

  real(dp) :: ucich = 3.0D0
  !! ICH system cost ($/W)

  real(dp), parameter :: ucint = 35.0D0
  !! superconductor intercoil structure cost ($/kg)

  real(dp) :: uclh = 3.3D0
  !! lower hybrid system cost ($/W)

  real(dp), parameter :: uclv = 16.0D0
  !! low voltage system cost ($/kVA)

  real(dp), parameter :: ucmb = 260.0D0
  !! unit cost for reactor maintenance building (M$/m3)

  real(dp), bind(C) :: ucme = 1.25D8
  !! cost of maintenance equipment ($)

  real(dp), bind(C) :: ucmisc = 2.5D7
  !! miscellaneous plant allowance ($)

  real(dp) :: ucnbi = 3.3D0
  !! NBI system cost ($/W)

  real(dp), parameter :: ucnbv = 1000.0D0
  !! cost of nuclear building ventilation ($/m3)

  real(dp), dimension(4) :: ucoam = (/68.8D0, 68.8D0, 68.8D0, 74.4D0/)
  !! annual cost of operation and maintenance (M$/year/1200MW**0.5)

  real(dp) :: ucpens = 32.0D0
  !! penetration shield cost ($/kg)

  real(dp) :: ucpfb = 210.0D0
  !! cost of PF coil buses ($/kA-m)

  real(dp) :: ucpfbk = 1.66D4
  !! cost of PF coil DC breakers ($/MVA**0.7)

  real(dp) :: ucpfbs = 4.9D3
  !! cost of PF burn power supplies ($/kW**0.7)

  real(dp) :: ucpfcb = 7.5D4
  !! cost of PF coil AC breakers ($/circuit)

  real(dp) :: ucpfdr1 = 150.0D0
  !! cost factor for dump resistors ($/MJ)

  real(dp) :: ucpfic = 1.0D4
  !! cost of PF instrumentation and control ($/channel)

  real(dp) :: ucpfps = 3.5D4
  !! cost of PF coil pulsed power supplies ($/MVA)

  real(dp), parameter :: ucphx = 15.0D0
  !! primary heat transport cost ($/W**exphts)

  real(dp), parameter :: ucpp = 48.0D0
  !! cost of primary power transformers ($/kVA**0.9)

  real(dp) :: ucrb = 400.0D0
  !! cost of reactor building (M$/m3)

  real(dp), dimension(7) :: ucsc = &
      (/600.0D0, 600.0D0, 300.0D0, 600.0D0, 600.0D0, 600.0D0,300.0D0/)
  !! cost of superconductor ($/kg)

  real(dp), parameter :: ucsh = 115.0D0
  !! cost of shops and warehouses (M$/m3)

  real(dp) :: ucshld = 32.0D0
  !! cost of shield structural steel ($/kg)

  real(dp), parameter :: ucswyd = 1.84D7
  !! switchyard equipment costs ($)

  real(dp) :: uctfbr = 1.22D0
  !! cost of TF coil breakers ($/W**0.7)

  real(dp) :: uctfbus = 100.0D0
  !! cost of TF coil bus ($/kg)

  real(dp), parameter :: uctfdr = 1.75D-4
  !! cost of TF coil dump resistors ($/J)

  real(dp), parameter :: uctfgr = 5000.0D0
  !! additional cost of TF coil dump resistors ($/coil)

  real(dp), parameter :: uctfic = 1.0D4
  !! cost of TF coil instrumentation and control ($/coil/30)

  real(dp) :: uctfps = 24.0D0
  !! cost of TF coil power supplies ($/W**0.7)

  real(dp) :: uctfsw = 1.0D0
  !! cost of TF coil slow dump switches ($/A)

  real(dp), parameter :: uctpmp = 1.105D5
  !! cost of turbomolecular pump ($)

  real(dp), parameter :: uctr = 370.0D0
  !! cost of tritium building ($/m3)

  real(dp), dimension(2), bind(C) :: ucturb = (/230.0D6, 245.0D6/)
  !! cost of turbine plant equipment ($) (dependent on coolant type coolwh)

  real(dp), parameter :: ucvalv = 3.9D5
  !! vacuum system valve cost ($)

  real(dp), parameter :: ucvdsh = 26.0D0
  !! vacuum duct shield cost ($/kg)

  real(dp), parameter :: ucviac = 1.3D6
  !! vacuum system instrumentation and control cost ($)

  real(dp) :: ucwindpf = 465.0D0
  !! cost of PF coil superconductor windings ($/m)

  real(dp) :: ucwindtf = 480.0D0
  !! cost of TF coil superconductor windings ($/m)

  real(dp), parameter :: ucws = 460.0D0
  !! cost of active assembly shop ($/m3)

  real(dp), dimension(4) :: ucwst = (/0.0D0, 3.94D0, 5.91D0, 7.88D0/)
  !! cost of waste disposal (M$/y/1200MW)

end module cost_variables

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
  !! Also used for demontable magnets (itart = 1) and superconducting coils (i_tf_sup = 1)
  !! To set the CP lifetime

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
  !! allowable neutron wall-load (MW/m2) (`constraint equation 8`)

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

module ife_variables
  !! author: S. Muldrew (UKAEA)
  !!
  !! Module containing global variables relating to the inertial fusion energy model
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code
   
  use, intrinsic :: iso_fortran_env, only: dp=>real64 

  implicit none

  public
   
   
  !! Default IFE builds and material volumes are those for the SOMBRERO device.
  !! The 2-dimensional arrays have indices (region, material), where 'region'
  !! is the region and maxmat is the 'material':
  !!
  !! - 'region' = 1 radially outside chamber
  !! - 'region' = 2 above chamber
  !! - 'region' = 3 below chamber
  
  integer, parameter ::  maxmat = 8
  !! Total number of materials in IFE device. Material numbers are as follows:
  !!
  !! - =0 void
  !! - =1 steel
  !! - =2 carbon cloth
  !! - =3 FLiBe
  !! - =4 lithium oxide Li2O
  !! - =5 concrete
  !! - =6 helium
  !! - =7 xenon
  !! - =8 lithium
   
  real(dp) :: bldr   = 1.0D0
  !! radial thickness of IFE blanket (m; calculated `if ifetyp=4`)

  real(dp) :: bldrc   = 1.0D0
  !! radial thickness of IFE curtain (m; `ifetyp=4`)

  real(dp) :: bldzl  = 4.0D0
  !! vertical thickness of IFE blanket below chamber (m)

  real(dp) :: bldzu  = 4.0D0
  !! vertical thickness of IFE blanket above chamber (m)

  real(dp), dimension(3,0:maxmat) :: blmatf = reshape( (/ &
    0.05D0,0.05D0,0.05D0, &
    0.0D0, 0.0D0, 0.0D0,  &
    0.45D0,0.45D0,0.45D0, &
    0.0D0, 0.0D0, 0.0D0,  &
    0.20D0,0.20D0,0.20D0, &
    0.0D0, 0.0D0, 0.0D0,  &
    0.30D0,0.30D0,0.30D0, &
    0.0D0,0.0D0,0.0D0,    &
    0.0D0, 0.0D0, 0.0D0  /), shape(blmatf))
  !! IFE blanket material fractions

  real(dp), dimension(3,0:maxmat) :: blmatm = 0.0D0
  !! IFE blanket material masses (kg)

  real(dp), dimension(3,0:maxmat) :: blmatv = 0.0D0
  !! IFE blanket material volumes (m3)

  real(dp), dimension(3) :: blvol = 0.0D0
  !! IFE blanket volume (m3)

  real(dp) :: cdriv0 = 154.3D0
  !! IFE generic/laser driver cost at edrive=0 (M$)

  real(dp) :: cdriv1 = 163.2D0
  !! IFE low energy heavy ion beam driver cost extrapolated to `edrive=0` (M$)

  real(dp) :: cdriv2 = 244.9D0
  !! IFE high energy heavy ion beam driver cost extrapolated to `edrive=0` (M$)

  real(dp) :: cdriv3 = 1.463D0
  !! IFE driver cost ($/J wall plug) (`ifedrv==3`)

  real(dp) :: chdzl = 9.0D0
  !! vertical thickness of IFE chamber below centre (m)

  real(dp) :: chdzu = 9.0D0
  !! vertical thickness of IFE chamber above centre (m)

  real(dp), dimension(0:maxmat) :: chmatf = &
    (/1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/)
  !! IFE chamber material fractions

  real(dp), dimension(0:maxmat) :: chmatm = 0.0D0
  !! IFE chamber material masses (kg)

  real(dp), dimension(0:maxmat) :: chmatv = 0.0D0
  !! IFE chamber material volumes (m3)

  real(dp) :: chrad = 6.5D0
  !! radius of IFE chamber (m) (`iteration variable 84`)

  real(dp) :: chvol = 0.0D0
  !! IFE chamber volume (m3)

  real(dp) :: dcdrv0 = 111.4D0
  !! IFE generic/laser driver cost gradient (M$/MJ)

  real(dp) :: dcdrv1 = 78.0D0
  !! HIB driver cost gradient at low energy (M$/MJ)

  real(dp) :: dcdrv2 = 59.9D0
  !! HIB driver cost gradient at high energy (M$/MJ)

  real(dp) :: drveff = 0.28D0
  !! IFE driver wall plug to target efficiency (`ifedrv=0,3`) (`iteration variable 82`)

  real(dp) :: edrive = 5.0D6
  !! IFE driver energy (J) (`iteration variable 81`)

  real(dp) :: etadrv = 0.0D0
  !! IFE driver wall plug to target efficiency

  real(dp) :: etali = 0.4D0
  !! IFE lithium pump wall plug efficiency (`ifetyp=4`)

  real(dp), dimension(10) :: etave = (/ &
    0.082D0,0.079D0,0.076D0,0.073D0,0.069D0, &
    0.066D0,0.062D0,0.059D0,0.055D0,0.051D0 /)
  !! IFE driver efficiency vs driver energy (`ifedrv=-1`)

  real(dp) :: fauxbop = 0.06D0
  !! fraction of gross electric power to balance-of-plant (IFE)

  real(dp) :: fbreed = 0.51D0
  !! fraction of breeder external to device core

  real(dp) :: fburn  = 0.3333D0
  !! IFE burn fraction (fraction of tritium fused/target)

  real(dp) :: flirad = 0.78D0
  !! radius of FLiBe/lithium inlet (m) (`ifetyp=3,4`)

  real(dp) :: frrmax = 1.0D0
  !! f-value for maximum IFE repetition rate (`constraint equation 50`, `iteration variable 86`)

  real(dp) :: fwdr = 0.01D0
  !! radial thickness of IFE first wall (m)

  real(dp) :: fwdzl = 0.01D0
  !! vertical thickness of IFE first wall below chamber (m)

  real(dp) :: fwdzu = 0.01D0
  !! vertical thickness of IFE first wall above chamber (m)

  real(dp), dimension(3,0:maxmat) :: fwmatf = reshape( (/ &
    0.05D0,0.05D0,0.05D0, &
    0.0D0, 0.0D0, 0.0D0,  &
    0.95D0,0.95D0,0.95D0, &
    0.0D0, 0.0D0, 0.0D0,  &
    0.0D0, 0.0D0, 0.0D0,  &
    0.0D0, 0.0D0, 0.0D0,  &
    0.0D0, 0.0D0, 0.0D0,  &
    0.0D0, 0.0D0, 0.0D0,  &
    0.0D0, 0.0D0, 0.0D0  /), shape(fwmatf))
  !! IFE first wall material fractions

  real(dp), dimension(3,0:maxmat) :: fwmatm = 0.0D0
  !! IFE first wall material masses (kg)

  real(dp), dimension(3,0:maxmat) :: fwmatv = 0.0D0
  !! IFE first wall material volumes (kg)

  real(dp), dimension(3) :: fwvol = 0.0D0
  !! IFE first wall volume (m3)

  real(dp) :: gain = 0.0D0
  !! IFE target gain

  real(dp), dimension(10) :: gainve = (/ &
    60.0D0, 95.0D0,115.0D0,125.0D0,133.0D0, &
    141.0D0,152.0D0,160.0D0,165.0D0,170.0D0 /)
  !! IFE target gain vs driver energy (`ifedrv=-1`)

  real(dp) :: htpmw_ife = 0.0D0         
  !! IFE heat transport system electrical pump power (MW)

  integer :: ife = 0
  !! Switch for IFE option (set via `device.dat`):
  !!
  !! - =0 use tokamak, RFP or stellarator model
  !! - =1 use IFE model

  integer :: ifedrv = 2
  !! Switch for type of IFE driver:
  !!
  !! - =-1 use gainve, etave for gain and driver efficiency
  !! - =0 use tgain, drveff for gain and driver efficiency
  !! - =1 use laser driver based on SOMBRERO design
  !! - =2 use heavy ion beam driver based on OSIRIS
  !! - =3 Input pfusife, rrin and drveff

  integer :: ifetyp = 0
  !! Switch for type of IFE device build:
  !!
  !! - =0 generic (cylindrical) build
  !! - =1 OSIRIS-like build
  !! - =2 SOMBRERO-like build
  !! - =3 HYLIFE-II-like build
  !! - =4 2019 build

  real(dp) :: lipmw = 0.0D0
  !! IFE lithium pump power (MW; `ifetyp=4`)

  real(dp) :: mcdriv = 1.0D0
  !! IFE driver cost multiplier

  real(dp) :: mflibe = 0.0D0
  !! total mass of FLiBe (kg)

  real(dp) :: pdrive = 23.0D6
  !! IFE driver power reaching target (W) (`iteration variable 85`)
  
  real(dp) :: pfusife = 1000.0D0
  !! IFE input fusion power (MW) (`ifedrv=3 only`; `itv 155`)

  real(dp) :: pifecr = 10.0D0
  !! IFE cryogenic power requirements (MW)

  real(dp), bind(C) :: ptargf = 2.0D0
  !! IFE target factory power at 6 Hz repetition rate (MW)

  real(dp) :: r1 = 0.0D0
  !! IFE device radial build (m)

  real(dp) :: r2 = 0.0D0
  !! IFE device radial build (m)

  real(dp) :: r3 = 0.0D0
  !! IFE device radial build (m)

  real(dp) :: r4 = 0.0D0
  !! IFE device radial build (m)

  real(dp) :: r5 = 0.0D0
  !! IFE device radial build (m)

  real(dp) :: r6 = 0.0D0
  !! IFE device radial build (m)

  real(dp) :: r7 = 0.0D0
  !! IFE device radial build (m)

  real(dp), bind(C) :: reprat = 0.0D0
  !! IFE driver repetition rate (Hz)

  real(dp) :: rrin = 6.0D0
  !! Input IFE repetition rate (Hz) (`ifedrv=3 only`; `itv 156`)

  real(dp) :: rrmax = 20.0D0
  !! maximum IFE repetition rate (Hz)

  real(dp) :: shdr = 1.7D0
  !! radial thickness of IFE shield (m)

  real(dp) :: shdzl = 5.0D0
  !! vertical thickness of IFE shield below chamber (m)

  real(dp) :: shdzu  = 5.0D0
  !! vertical thickness of IFE shield above chamber (m)

  real(dp), dimension(3,0:maxmat) :: shmatf = reshape( (/ &
    0.05D0,0.05D0,0.05D0, &
    0.19D0,0.19D0,0.19D0, &
    0.0D0, 0.0D0, 0.0D0,  &
    0.0D0, 0.0D0, 0.0D0,  &
    0.0D0, 0.0D0, 0.0D0,  &
    0.665D0,0.665D0,0.665D0, &
    0.095D0,0.095D0,0.095D0, &
    0.0D0, 0.0D0, 0.0D0,  &
    0.0D0, 0.0D0, 0.0D0  /), shape(shmatf))
  !! IFE shield material fractions

  real(dp), dimension(3,0:maxmat) :: shmatm = 0.0D0
  !! IFE shield material masses (kg)

  real(dp), dimension(3,0:maxmat) :: shmatv = 0.0D0
  !! IFE shield material volumes (kg)

  real(dp), dimension(3) :: shvol = 0.0D0
  !! IFE shield volume (m3)

  real(dp) :: sombdr = 2.7D0
  !! radius of cylindrical blanket section below chamber (`ifetyp=2`)

  real(dp) :: somtdr = 2.7D0
  !! radius of cylindrical blanket section above chamber (`ifetyp=2`)

  real(dp) :: taufall = 0.0D0
  !! Lithium Fall Time (s)

  real(dp) :: tdspmw = 0.01D0
  !! IFE target delivery system power (MW)

  real(dp), bind(C) :: tfacmw = 0.0D0
  !! IFE target factory power (MW)

  real(dp) :: tgain = 85.0D0
  !! IFE target gain (if `ifedrv = 0`) (`iteration variable 83`)

  real(dp) :: uccarb = 50.0D0
  !! cost of carbon cloth ($/kg)

  real(dp) :: ucconc = 0.1D0
  !! cost of concrete ($/kg)

  real(dp) :: ucflib = 84.0D0
  !! cost of FLiBe ($/kg)

  real(dp) :: uctarg = 0.3D0
  !! cost of IFE target ($/target)

  real(dp) :: v1dr = 0.0D0
  !! radial thickness of IFE void between first wall and blanket (m)

  real(dp) :: v1dzl = 0.0D0
  !! vertical thickness of IFE void 1 below chamber (m)

  real(dp) :: v1dzu = 0.0D0
  !! vertical thickness of IFE void 1 above chamber (m)

  real(dp), dimension(3,0:maxmat) :: v1matf = reshape( (/ &
    1.0D0, 1.0D0, 1.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0  /), shape(v1matf))
  !! IFE void 1 material fractions

  real(dp), dimension(3,0:maxmat) :: v1matm = 0.0D0
  !! IFE void 1 material masses (kg)

  real(dp), dimension(3,0:maxmat) :: v1matv = 0.0D0
  !! IFE void 1 material volumes (kg)

  real(dp), dimension(3) :: v1vol = 0.0D0
  !! IFE void 1 volume (m3)

  real(dp) :: v2dr = 2.0D0
  !! radial thickness of IFE void between blanket and shield (m)

  real(dp) :: v2dzl = 7.0D0
  !! vertical thickness of IFE void 2 below chamber (m)

  real(dp) :: v2dzu = 7.0D0
  !! vertical thickness of IFE void 2 above chamber (m)

  real(dp), dimension(3,0:maxmat) :: v2matf = reshape( (/ &
    1.0D0, 1.0D0, 1.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0  /), shape(v2matf))
  !! IFE void 2 material fractions

  real(dp), dimension(3,0:maxmat) :: v2matm = 0.0D0
  !! IFE void 2 material masses (kg)

  real(dp), dimension(3,0:maxmat) :: v2matv = 0.0D0
  !! IFE void 2 material volumes (kg)

  real(dp), dimension(3) :: v2vol = 0.0D0
  !! IFE void 2 volume (m3)

  real(dp) :: v3dr   = 43.3D0
  !! radial thickness of IFE void outside shield (m)

  real(dp) :: v3dzl  = 30.0D0
  !! vertical thickness of IFE void 3 below chamber (m)

  real(dp) :: v3dzu  = 20.0D0
  !! vertical thickness of IFE void 3 above chamber (m)

  real(dp), dimension(3,0:maxmat) :: v3matf = reshape( (/ &
    1.0D0, 1.0D0, 1.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0, &
    0.0D0, 0.0D0, 0.0D0  /), shape(v3matf))
  !! IFE void 3 material fractions

  real(dp), dimension(3,0:maxmat) :: v3matm = 0.0D0
  !! IFE void 3 material masses (kg)

  real(dp), dimension(3,0:maxmat) :: v3matv = 0.0D0
  !! IFE void 3 material volumes (kg)

  real(dp), dimension(3) :: v3vol = 0.0D0
  !! IFE void 3 volume (m3)

  real(dp) :: zl1 = 0.0D0
  !! IFE vertical build below centre (m)

  real(dp) :: zl2 = 0.0D0
  !! IFE vertical build below centre (m)

  real(dp) :: zl3 = 0.0D0
  !! IFE vertical build below centre (m)

  real(dp) :: zl4 = 0.0D0
  !! IFE vertical build below centre (m)

  real(dp) :: zl5 = 0.0D0
  !! IFE vertical build below centre (m)

  real(dp) :: zl6 = 0.0D0
  !! IFE vertical build below centre (m)

  real(dp) :: zl7 = 0.0D0
  !! IFE vertical build below centre (m)

  real(dp) :: zu1 = 0.0D0
  !! IFE vertical build above centre (m)

  real(dp) :: zu2 = 0.0D0
  !! IFE vertical build above centre (m)

  real(dp) :: zu3 = 0.0D0
  !! IFE vertical build above centre (m)

  real(dp) :: zu4 = 0.0D0
  !! IFE vertical build above centre (m)

  real(dp) :: zu5 = 0.0D0
  !! IFE vertical build above centre (m)

  real(dp) :: zu6 = 0.0D0
  !! IFE vertical build above centre (m)

  real(dp) :: zu7 = 0.0D0
  !! IFE vertical build above centre (m)
   
end module ife_variables

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
