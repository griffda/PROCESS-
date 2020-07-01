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

  integer :: itart = 0
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
  !! plasma separatrix elongation (calculated if `ishape > 0`)

  real(dp), bind(C) :: kappa95 = 1.6D0
  !! plasma elongation at 95% surface (calculated if `ishape < 4`)

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
  !! plasma separatrix triangularity (calculated if `ishape=1, 3 or 4`)

  real(dp) :: triang95 = 0.24D0
  !! plasma triangularity at 95% surface (calculated if `ishape < 3`)

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