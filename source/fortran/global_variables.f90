! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module global_variables

  !! Module containing miscellaneous global variables
  !! This module contains miscellaneous global variables not
  !! well-suited to any of the other 'variables' modules.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code

  implicit none
  public

  character(len=48) :: icase = 'Steady-state tokamak model'
  !! icase : power plant type
  character(len=180) :: runtitle = &
       "Run Title (change this line using input variable 'runtitle')"
       !! runtitle /Run Title/ : short descriptive title for the run

  integer :: verbose = 0
  !! verbose /0/ : switch for turning on/off diagnostic messages:<UL>
  !!           <LI> = 0 turn off diagnostics
  !!           <LI> = 1 turn on diagnostics</UL>
  integer :: run_tests = 0
  !! run_tests /0/ : Turns on built-in tests if set to 1

  integer :: maxcal = 200
  !! maxcal /200/ : maximum number of VMCON iterations

  character(len=30) :: fileprefix = "" !'dummy_file_prefix'
  character(len=50) :: output_prefix = "" ! output file prefix
  character(len=25) :: xlabel, vlabel
  character(len=25) :: xlabel_2, vlabel_2
  integer :: iscan_global=0    ! Makes iscan available globally.
  real(kind(1.0D0)):: convergence_parameter  ! VMCON convergence parameter "sum"

end module global_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module constants

  !! Module containing miscellaneous numerical and physical constants
  !! This module contains miscellaneous numerical and
  !! physical constants.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  ! File output indexes
  integer, parameter :: iotty    = 6  !  Standard output unit identifier
  integer, parameter :: nout     = 11 !  Output file unit identifier
  integer, parameter :: nplot    = 12 !  Plot data file unit identifier
  integer, parameter :: mfile    = 13 !  Machine-optimised output file unit
  integer, parameter :: vfile    = 14 !  Verbose diagnostics file
  integer, parameter :: opt_file = 15 !  Optimisation information output file number

  real(kind(1.0D0)), parameter :: degrad = 0.01745329251D0
  !! degrad FIX : degrees to radians, = pi/180
  real(kind(1.0D0)), parameter :: echarge = 1.60217733D-19
  !! echarge FIX : electron charge (C)
  real(kind(1.0D0)), parameter :: mproton = 1.6726231D-27
  !! mproton FIX : proton mass (kg)
  real(kind(1.0D0)), parameter :: pi = 3.1415926535897932D0
  !! pi FIX : famous number
  real(kind(1.0D0)), parameter :: rmu0 = 1.256637062D-6
  !! rmu0 FIX : permeability of free space, 4.pi x 10^(-7) H/m
  real(kind(1.0D0)), parameter :: twopi = 6.2831853071795862D0
  !! twopi FIX : 2 pi
  real(kind(1.0D0)), parameter :: umass = 1.660538921D-27
  !! umass FIX : unified atomic mass unit (kg)
  real(kind(1.0D0)), parameter :: epsilon0 = 8.85418781D-12
  !! epsilon0 FIX : permittivity of free space (Farad/m)

contains


end module constants

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module physics_variables

  !! Module containing global variables relating to the plasma physics
  !! This module contains global variables relating to the plasma
  !! physics.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  implicit none

  public

  integer, parameter :: ipnlaws = 48
  !! ipnlaws /48/ FIX : number of energy confinement time scaling laws

  real(kind(1.0D0)) :: abeam = 0.0D0
  !! abeam : beam ion mass (amu)

  real(kind(1.0D0)), bind(C) :: afuel = 0.0D0
  !! afuel : average mass of fuel portion of ions (amu)

  real(kind(1.0D0)) :: aion = 0.0D0
  !! aion : average mass of all ions (amu)

  real(kind(1.0D0)) :: alphaj = 1.0D0
  !! alphaj /1.0/ : current profile index;
  !!                calculated from q0, q if iprofile=1

  real(kind(1.0D0)) :: alphan = 0.25D0
  !! alphan /0.25/ : density profile index
  real(kind(1.0D0)) :: alphap = 0.0D0
  !! alphap : pressure profile index
  real(kind(1.0D0)) :: alpharate = 0.0D0
  !! alpharate : alpha particle production rate (particles/m3/sec)
  real(kind(1.0D0)) :: alphat = 0.5D0
  !! alphat /0.5/ : temperature profile index
  real(kind(1.0D0)) :: aspect = 2.907D0
  !! aspect /2.907/ : aspect ratio (iteration variable 1)
  real(kind(1.0D0)) :: beamfus0 = 1.0D0
  !! beamfus0 /1.0/ : multiplier for beam-background fusion calculation
  real(kind(1.0D0)), bind(C) :: beta = 0.042D0
  !! beta /0.042/ : total plasma beta (iteration variable 5)
  !!            (calculated if ipedestal =3)
  real(kind(1.0D0)) :: betaft = 0.0D0
  !! betaft : fast alpha beta component
  real(kind(1.0D0)) :: betalim = 0.0D0
  !! betalim : allowable beta
  real(kind(1.0D0)) :: betanb = 0.0D0
  !! betanb : neutral beam beta component
  real(kind(1.0D0)) :: betap = 0.0D0
  !! betap : poloidal beta
  real(kind(1.0D0)), bind(C) :: normalised_total_beta = 0.0D0
  !! normalised_total_beta : normaised total beta

  real(kind(1.0D0)) :: betbm0 = 1.5D0
  !! betbm0 /1.5/ : leading coefficient for NB beta fraction
  real(kind(1.0D0)), bind(C) :: bp = 0.0D0
  !! bp : poloidal field (T)
  real(kind(1.0D0)), bind(C) :: bt = 5.68D0
  !! bt /5.68/ : toroidal field on axis (T) (iteration variable 2)
  real(kind(1.0D0)), bind(C) :: btot = 0.0D0
  !! btot : total toroidal + poloidal field (T)
  real(kind(1.0D0)) :: burnup = 0.0D0
  !! burnup : fractional plasma burnup
  real(kind(1.0D0)) :: bvert = 0.0D0
  !! bvert : vertical field at plasma (T)
  real(kind(1.0D0)) :: csawth = 1.0D0
  !! csawth /1.0/ : coeff. for sawteeth effects on burn V-s requirement
  real(kind(1.0D0)) :: cvol = 1.0D0
  !! cvol /1.0/ : multiplying factor times plasma volume (normally=1)
  real(kind(1.0D0)) :: cwrmax = 1.35D0
  !! cwrmax /1.35/ : maximum ratio of conducting wall distance to
  !!                 plasma minor radius for vertical stability
  !!                 (constraint equation 23)
  real(kind(1.0D0)) :: dene = 9.8D19
  !! dene /9.8e19/ : electron density (/m3) (iteration variable 6)
  !!                 (calculated if ipedestal=3)
  real(kind(1.0D0)) :: deni = 0.0D0
  !! deni : fuel ion density (/m3)
  real(kind(1.0D0)) :: dlamee = 0.0D0
  !! dlamee : electron-electron coulomb logarithm
  real(kind(1.0D0)) :: dlamie = 0.0D0
  !! dlamie : ion-electron coulomb logarithm
  real(kind(1.0D0)), dimension(7) :: dlimit = 0.0D0
  !! dlimit(7) : density limit (/m3) as calculated using various models
  real(kind(1.0D0)) :: dnalp = 0.0D0
  !! dnalp : thermal alpha density (/m3)
  real(kind(1.0D0)) :: dnbeam = 0.0D0
  !! dnbeam : hot beam ion density, variable (/m3)
  real(kind(1.0D0)) :: dnbeam2 = 0.0D0
  !! dnbeam2 : hot beam ion density from calculation (/m3)
  real(kind(1.0D0)) :: dnbeta = 3.5D0
  !! dnbeta /3.5/ : (Troyon-like) coefficient for beta scaling;
  !!                calculated as (4.0*rli) if iprofile=1
  !!                (see also gtscale option)
  real(kind(1.0D0)) :: dnelimt = 0.0D0
  !! dnelimt : density limit (/m3)
  real(kind(1.0D0)) :: dnitot = 0.0D0
  !! dnitot : total ion density (/m3)
  real(kind(1.0D0)) :: dnla = 0.0D0
  !! dnla : line averaged electron density (/m3)
  real(kind(1.0D0)) :: dnprot = 0.0D0
  !! dnprot : proton ash density (/m3)
  real(kind(1.0D0)) :: dntau = 0.0D0
  !! dntau : plasma average "n-tau" (seconds/m3)
  real(kind(1.0D0)) :: dnz = 0.0D0
  !! dnz : high Z ion density (/m3)
  real(kind(1.0D0)), parameter :: ealphadt = 3520.0D0
  !! ealphadt /3520.0/ FIX : alpha birth energy in D-T reaction (keV)
  real(kind(1.0D0)) :: epbetmax = 1.38D0
  !! epbetmax /1.38/ : maximum (eps*beta_poloidal) (constraint equation 6)
  !!                   revised (07/01/16) Issue #346
  !!                   "Operation at the tokamak equilibrium poloidal beta-limit in TFTR"
  !!                   1992 Nucl. Fusion 32 1468
  real(kind(1.0D0)) :: eps = 0.34399724802D0
  !! eps : inverse aspect ratio
  real(kind(1.0D0)) :: faccd = 0.0D0
  !! faccd : fraction of plasma current produced by auxiliary current drive
  real(kind(1.0D0)) :: facoh = 0.0D0
  !! facoh : fraction of plasma current produced inductively
  real(kind(1.0D0)) :: falpe = 0.0D0
  !! falpe : fraction of alpha energy to electrons
  real(kind(1.0D0)) :: falpha = 0.95D0
  !! falpha /0.95/ : fraction of alpha power deposited in plasma
  !!                 (Physics of Energetic Ions, p.2489)

  real(kind(1.0D0)) :: falpi = 0.0D0
  !! falpi : fraction of alpha power to ions

  real(kind(1.0D0)) :: fdeut = 0.5D0
  !! fdeut /0.5/ : deuterium fuel fraction

  real(kind(1.0D0)) :: ftar = 1.0D0
  !! ftar  /1.0/ : fraction of power to the lower divertor in double null
  !!               configuration (i_single_null = 0 only) (default assumes SN)
  real(kind(1.0D0)) :: ffwal = 0.92D0
  !! ffwal /0.92/ : factor to convert plasma surface area to first wall
  !!                area in neutron wall load calculation (iwalld=1)
  real(kind(1.0D0)) :: fgwped = 0.85D0
  !! fgwped /0.85/ : fraction of Greenwald density to set as pedestal-top density
  !!                 If <0, pedestal-top density set manually using neped (ipedestal>=1)
  !!                 Needs to be >0 if ipedestal = 3
  !!                 (iteration variable 145)
  real(kind(1.0D0)) :: fgwsep = 0.50D0
  !! fgwsep /0.50/ : fraction of Greenwald density to set as separatrix density
  !!                 If <0, separatrix density set manually using nesep (ipedestal>=1)
  !!                 Needs to be >0 if ipedestal = 3
  real(kind(1.0D0)) :: fhe3 = 0.0D0
  !! fhe3 /0.0/ : helium-3 fuel fraction
  real(kind(1.0D0)) :: figmer = 0.0D0
  !! figmer : physics figure of merit (= plascur*aspect**sbar, where sbar=1)
  real(kind(1.0D0)) :: fkzohm = 1.0D0
  !! fkzohm /1.0/ : Zohm elongation scaling adjustment factor (ishape=2, 3)
  real(kind(1.0D0)) :: fplhsep = 1.0D0
  !! fplhsep /1.0/ : F-value for Psep >= Plh + Paux (constraint equation 73)
  
  real(kind(1.0D0)) :: fpdivlim = 1.0D0
  !! fpdivlim /1.0/ : F-value for minimum pdivt (constraint equation 80)

  real(kind(1.0D0)) :: fne0 = 1.0D0
  !! fne0 /1.0/ : F-value for minimum pdivt (constraint equation 81)

  real(kind(1.0D0)), bind(C) :: ftrit = 0.5D0
  !! ftrit /0.5/ : tritium fuel fraction
  real(kind(1.0D0)) :: fusionrate = 0.0D0
  !! fusionrate : fusion reaction rate (reactions/m3/sec)
  real(kind(1.0D0)) :: fvsbrnni = 1.0D0
  !! fvsbrnni /1.0/ : fraction of the plasma current produced by
  !!                  non-inductive means (iteration variable 44)
  real(kind(1.0D0)) :: gamma = 0.4D0
  !! gamma /0.4/ : Ejima coefficient for resistive startup V-s formula
  real(kind(1.0D0)) :: gammaft = 0.0D0
  !! gammaft : ratio of (fast alpha + neutral beam beta) to thermal beta
  integer :: gtscale = 0
  !! gtscale /0/ : switch for a/R scaling of dnbeta (iprofile=0 only):<UL>
  !!         <LI>  = 0 do not scale dnbeta with eps;
  !!         <LI>  = 1 scale dnbeta with eps</UL>
  real(kind(1.0D0)), dimension(ipnlaws) :: hfac = 0.0D0
  !! hfac(ipnlaws) : H factors for an ignited plasma for each energy confinement
  !!                 time scaling law
  real(kind(1.0D0)) :: hfact = 1.0D0
  !! hfact /1.0/ : H factor on energy confinement times, radiation corrected
  !!               (iteration variable 10).
  !!               If ipedestal=2 or 3 and hfact = 0, not used in PLASMOD
  !!               (see also plasmod_i_modeltype)
  ! Issue #219
  real(kind(1.0D0)) :: taumax = 10.0D0
  !! taumax /10/ : Maximum allowed energy confinement time (s)
  integer :: ibss = 3
  !! ibss /3/ : switch for bootstrap current scaling:<UL>
  !!       <LI> = 1 ITER 1989 bootstrap scaling (high R/a only);
  !!       <LI> = 2 for Nevins et al general scaling;
  !!       <LI> = 3 for Wilson et al numerical scaling;
  !!       <LI> = 4 for Sauter et al scaling</UL>
  integer :: iculbl = 0
  !! iculbl /0/ : switch for beta limit scaling (constraint equation 24):<UL>
  !!         <LI> = 0 apply limit to total beta;
  !!         <LI> = 1 apply limit to thermal beta;
  !!         <LI> = 2 apply limit to thermal + neutral beam beta</UL>
  integer :: icurr = 4
  !! icurr /4/ : switch for plasma current scaling to use:<UL>
  !!        <LI> = 1 Peng analytic fit;
  !!        <LI> = 2 Peng double null divertor scaling (ST);
  !!        <LI> = 3 simple ITER scaling (k = 2.2, d = 0.6);
  !!        <LI> = 4 later ITER scaling, a la Uckan;
  !!        <LI> = 5 Todd empirical scaling I;
  !!        <LI> = 6 Todd empirical scaling II;
  !!        <LI> = 7 Connor-Hastie model;
  !!        <LI> = 8 Sauter scaling allowing negative triangularity;
  !!        <LI> = 9 FIESTA ST fit </UL>
  
  integer :: idensl = 7
  !! idensl /7/ : switch for density limit to enforce (constraint equation 5):<UL>
  !!         <LI> = 1 old ASDEX;
  !!         <LI> = 2 Borrass model for ITER (I);
  !!         <LI> = 3 Borrass model for ITER (II);
  !!         <LI> = 4 JET edge radiation;
  !!         <LI> = 5 JET simplified;
  !!         <LI> = 6 Hugill-Murakami Mq limit;
  !!         <LI> = 7 Greenwald limit</UL>

  integer :: idivrt = 2
  !! idivrt : number of divertors (calculated from i_single_null)

  integer :: ifalphap = 1
  !! ifalphap /1/ : switch for fast alpha pressure calculation:<UL>
  !!           <LI> = 0 ITER physics rules (Uckan) fit;
  !!           <LI> = 1 Modified fit (D. Ward) - better at high temperature</UL>
  integer :: ifispact = 0
  !! ifispact /0/ : switch for neutronics calculations:<UL>
  !!           <LI> = 0 neutronics calculations turned off;
  !!           <LI> = 1 neutronics calculations turned on</UL>
  integer :: igeom = 1
  !! igeom /1/ : switch for plasma geometry calculation:<UL>
  !!        <LI> = 0 original method (possibly based on Peng ST modelling);
  !!        <LI> = 1 improved (and traceable) method</UL>
  integer :: ignite = 0
  !! ignite /0/ : switch for ignition assumption. Obviously, ignite must 
  !!              be zero if current drive is required. If ignite is 1, any 
  !!              auxiliary power is assumed to be used only during plasma 
  !!              start-up, and is excluded from all steady-state power 
  !!              balance calculations.<UL>
  !!         <LI> = 0 do not assume plasma ignition;
  !!         <LI> = 1 assume ignited (but include auxiliary power in costs)</UL>
  !! 
  integer :: iinvqd = 1
  !! iinvqd /1/ : switch for inverse quadrature in L-mode scaling laws 5 and 9:<UL>
  !!         <LI> = 0 inverse quadrature not used;
  !!         <LI> = 1 inverse quadrature with Neo-Alcator tau-E used</UL>

  integer :: ipedestal = 1
  !! ipedestal /1/ : switch for pedestal profiles:<UL>
  !!            <LI> = 0 use original parabolic profiles;
  !!            <LI> = 1 use pedestal profiles
  !!            <LI> = 2 use pedestal profiles and run PLASMOD on final output
  !!            <LI> = 3 use PLASMOD transport model only to calculate pedestal profiles</UL>

  ! Issue #413
  integer :: ieped = 0
  !! ieped /0/ : switch for scaling pedestal-top temperature with plasma parameters:<UL>
  !!            <LI> = 0 set pedestal-top temperature manually using teped;
  !!            <LI> = 1 set pedestal-top temperature using EPED scaling;
  !!                  (PLASMOD implementation of scaling within PLASMOD, ipedestal =2,3)
  !!            <LI>    https://idm.euro-fusion.org/?uid=2MSZ4T </UL>

  ! Issue #730
  real(kind(1.0D0)), bind(C) :: eped_sf = 1.0D0
  !! eped_sf /1.0/ : Adjustment factor for EPED scaling to reduce 
  !! pedestal temperature or pressure to mitigate or prevent ELMs

  real(kind(1.0D0)) :: neped = 4.0D19
  !! neped /4.0e19/ : electron density of pedestal [m-3] (ipedestal=1,2, calculated if 3)
  real(kind(1.0D0)) :: nesep = 3.0D19
  !! nesep /3.0e19/ : electron density at separatrix [m-3] (ipedestal=1,2, calculated if 3)
  real(kind(1.0D0)) :: alpha_crit = 0.0D0
  !! alpha_crit : critical ballooning parameter value
  real(kind(1.0D0)) :: nesep_crit = 0.0D0
  !! nesep_crit : critical electron density at separatrix [m-3]
  real(kind(1.0D0)) :: plasma_res_factor = 1.0D0
  !! plasma_res_factor /1.0/ : plasma resistivity pre-factor
  real(kind(1.0D0)) :: rhopedn = 1.0D0
  !! rhopedn /1.0/ : r/a of density pedestal (ipedestal>=1)
  real(kind(1.0D0)) :: rhopedt = 1.0D0
  !! rhopedt /1.0/ : r/a of temperature pedestal (ipedestal>=1)
  real(kind(1.0D0)) :: tbeta = 2.0D0
  !! tbeta /2.0/ : temperature profile index beta  (ipedestal=1,2)
  real(kind(1.0D0)) :: teped = 1.0D0
  !! teped /1.0/ : electron temperature of pedestal (keV) (ipedestal>=1, ieped=0, calculated for ieped=1)
  real(kind(1.0D0)) :: tesep = 0.1D0
  !! tesep /0.1/ : electron temperature at separatrix (keV) (ipedestal>=1)
  !!               calculated if reinke criterion is used (icc = 78)

  integer :: iprofile = 1
  !! iprofile /1/ : switch for current profile consistency:<UL>
  !!            <LI> = 0 use input values for alphaj, rli, dnbeta
  !!                     (but see gtscale option);
  !!            <LI> = 1 make these consistent with input q, q0 values
  !!                     (recommendation: use icurr=4 with this option) </UL>
  integer :: iradloss = 1
  !! iradloss /1/ : switch for radiation loss term usage in power balance (see User Guide):<UL>
  !!            <LI> = 0 total power lost is scaling power plus radiation (needed for ipedestal=2,3)
  !!            <LI> = 1 total power lost is scaling power plus core radiation only
  !!            <LI> = 2 total power lost is scaling power only, with no additional
  !!                     allowance for radiation. This is not recommended for power plant models.</UL>

  integer :: isc = 34
  !! isc /34 (=IPB98(y,2))/ : switch for energy confinement time scaling law
  !!         (see description in tauscl)
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
  !! iscrp /1/ : switch for plasma-first wall clearances:<UL>
  !!        <LI> = 0 use 10% of rminor;
  !!        <LI> = 1 use input (scrapli and scraplo)</UL>
  integer :: ishape = 0
  !! ishape /0/ : switch for plasma cross-sectional shape calculation:<UL>
  !!         <LI> = 0 use input kappa, triang to calculate 95% values;
  !!         <LI> = 1 scale qlim, kappa, triang with aspect ratio (ST);
  !!         <LI> = 2 set kappa to the natural elongation value (Zohm ITER scaling),
  !!                  triang input;
  !!         <LI> = 3 set kappa to the natural elongation value (Zohm ITER scaling),
  !!                  triang95 input;
  !!         <LI> = 4 use input kappa95, triang95 to calculate separatrix values</UL>
  integer, bind(C) :: itart = 0
  !! itart /0/ : switch for spherical tokamak (ST) models:<UL>
  !!        <LI> = 0 use conventional aspect ratio models;
  !!        <LI> = 1 use spherical tokamak models</UL>
  integer :: itartpf = 0
  !! itartpf /0/ : switch for Spherical Tokamak PF models:<UL>
  !!        <LI> = 0 use Peng and Strickler (1986) model;
  !!        <LI> = 1 use conventional aspect ratio model</UL>
  integer :: iwalld = 1
  !! iwalld /1/ : switch for neutron wall load calculation:<UL>
  !!         <LI> = 1 use scaled plasma surface area;
  !!         <LI> = 2 use first wall area directly</UL>
  real(kind(1.0D0)), bind(C) :: kappa = 1.792D0
  !! kappa /1.792/ : plasma separatrix elongation (calculated if ishape > 0)
  real(kind(1.0D0)), bind(C) :: kappa95 = 1.6D0
  !! kappa95 /1.6/ : plasma elongation at 95% surface (calculated if ishape < 4)
  real(kind(1.0D0)) :: kappaa = 0.0D0
  !! kappaa : plasma elongation calculated as xarea/(pi.a2)
  real(kind(1.0D0)) :: kappaa_IPB = 0.d0
  !! kappaa_IPB : Volume measure of plasma elongation
  real(kind(1.0D0)) :: ne0 = 0.0D0
  !! ne0 : central electron density (/m3)
  real(kind(1.0D0)) :: ni0 = 0.0D0
  !! ni0 : central ion density (/m3)
  real(kind(1.0D0)) :: p0 = 0.0D0
  !! p0 : central total plasma pressure (Pa)
  real(kind(1.0D0)) :: palppv = 0.0D0
  !! palppv : alpha power per volume (MW/m3)
  real(kind(1.0D0)) :: palpepv = 0.0D0
  !! palpepv : alpha power per volume to electrons (MW/m3)
  real(kind(1.0D0)) :: palpfwmw = 0.0D0
  !! palpfwmw : alpha power escaping plasma and reaching first wall (MW)
  real(kind(1.0D0)) :: palpipv = 0.0D0
  !! palpipv : alpha power per volume to ions (MW/m3)
  real(kind(1.0D0)) :: palpmw = 0.0D0
  !! palpmw : alpha power (MW)
  real(kind(1.0D0)) :: palpnb = 0.0D0
  !! palpnb : alpha power from hot neutral beam ions (MW)
  real(kind(1.0D0)) :: pbrempv = 0.0D0
  !! pbrempv : bremsstrahlung power per volume (MW/m3)
  real(kind(1.0D0)) :: pchargemw = 0.0D0
  !! pchargemw : non-alpha charged particle fusion power (MW)
  real(kind(1.0D0)) :: pchargepv = 0.0D0
  !! pchargepv : non-alpha charged particle fusion power per volume (MW/m3)
  real(kind(1.0D0)) :: pcoef = 0.0D0
  !! pcoef : profile factor (= n-weighted T / average T)
  real(kind(1.0D0)) :: pcoreradmw = 0.0D0
  !! pcoreradmw : total core radiation power (MW)
  real(kind(1.0D0)) :: pcoreradpv = 0.0D0
  !! pcoreradpv : total core radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: pdd = 0.0D0
  !! pdd : deuterium-deuterium fusion power (MW)
  real(kind(1.0D0)) :: pdhe3 = 0.0D0
  !! pdhe3 : deuterium-helium3 fusion power (MW)
  real(kind(1.0D0)) :: pdivt = 0.0D0
  !! pdivt : power to conducted to the divertor region (MW)

  real(kind(1.0D0)) :: pdivl = 0.0D0
  !!pdivl : power conducted to the lower divertor region (calculated if i_single_null = 0) (MW)

  real(kind(1.0D0)) :: pdivu = 0.0D0
  !!pdivu : power conducted to the upper divertor region (calculated if i_single_null = 0) (MW)

  real(kind(1.0D0)) :: pdivmax = 0.0D0
  !!pdivmax : power conducted to the divertor with most load (calculated if i_single_null = 0) (MW)
  real(kind(1.0D0)) :: pdt = 0.0D0
  !! pdt : deuterium-tritium fusion power (MW)
  real(kind(1.0D0)) :: pedgeradmw = 0.0D0
  !! pedgeradmw : edge radiation power (MW)
  real(kind(1.0D0)) :: pedgeradpv = 0.0D0
  !! pedgeradpv : edge radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: pfuscmw = 0.0D0
  !! pfuscmw : charged particle fusion power (MW)
  real(kind(1.0D0)) :: phiint = 0.0D0
  !! phiint : internal plasma V-s
  real(kind(1.0D0)) :: photon_wall = 0.0D0
  !! photon_wall : Nominal mean radiation load on inside surface of reactor (MW/m2)
  real(kind(1.0D0)) :: piepv = 0.0D0
  !! piepv : ion/electron equilibration power per volume (MW/m3)
  real(kind(1.0D0)), bind(C) :: plascur = 0.0D0
  !! plascur : plasma current (A)
  real(kind(1.0D0)) :: plinepv = 0.0D0
  !! plinepv : line radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: pneutmw = 0.0D0
  !! pneutmw : neutron fusion power (MW)
  real(kind(1.0D0)) :: pneutpv = 0.0D0
  !! pneutpv : neutron fusion power per volume (MW/m3)
  real(kind(1.0D0)) :: pohmmw = 0.0D0
  !! pohmmw : ohmic heating power (MW)
  real(kind(1.0D0)) :: pohmpv = 0.0D0
  !! pohmpv : ohmic heating power per volume (MW/m3)
  real(kind(1.0D0)) :: powerht = 0.0D0
  !! powerht : heating power (= transport loss power) (MW) used in
  !!           confinement time calculation
  real(kind(1.0D0)), bind(C) :: powfmw = 0.0D0
  !! powfmw : fusion power (MW)
  real(kind(1.0D0)) :: pperim = 0.0D0
  !! pperim : plasma poloidal perimeter (m)
  real(kind(1.0D0)) :: pradmw = 0.0D0
  !! pradmw : total radiation power (MW)
  real(kind(1.0D0)) :: pradpv = 0.0D0
  !! pradpv : total radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: protonrate = 0.0D0
  !! protonrate : proton production rate (particles/m3/sec)
  real(kind(1.0D0)) :: psolradmw = 0.0D0
  !! psolradmw : SOL radiation power (MW) (stellarator only)
  real(kind(1.0D0)) :: psyncpv = 0.0D0
  !! psyncpv : synchrotron radiation power per volume (MW/m3)
  integer :: ilhthresh = 6
  !! ilhthresh /6/ : switch for L-H mode power threshold scaling to use
  !!                 (see pthrmw for list)
  real(kind(1.0D0)) :: plhthresh = 0.0D0
  !! plhthresh : L-H mode power threshold (MW)
  !!             (chosen via ilhthresh, and enforced if constraint equation 15 is on)
  real(kind(1.0D0)), dimension(18) :: pthrmw = 0.0D0
  !! pthrmw(18) : L-H power threshold for various scalings (MW): <OL>
  !!        <LI> ITER 1996 scaling: nominal
  !!        <LI> ITER 1996 scaling: upper bound
  !!        <LI> ITER 1996 scaling: lower bound
  !!        <LI> ITER 1997 scaling: excluding elongation
  !!        <LI> ITER 1997 scaling: including elongation
  !!        <LI> Martin 2008 scaling: nominal
  !!        <LI> Martin 2008 scaling: 95% upper bound
  !!        <LI> Martin 2008 scaling: 95% lower bound
  !!        <LI> Snipes 2000 scaling: nominal
  !!        <LI> Snipes 2000 scaling: upper bound
  !!        <LI> Snipes 2000 scaling: lower bound
  !!        <LI> Snipes 2000 scaling (closed divertor): nominal
  !!        <LI> Snipes 2000 scaling (closed divertor): upper bound
  !!        <LI> Snipes 2000 scaling (closed divertor): lower bound
  !!        <LI> Hubbard et al. 2012 L-I threshold scaling: nominal
  !!        <LI> Hubbard et al. 2012 L-I threshold scaling: lower bound
  !!        <LI> Hubbard et al. 2012 L-I threshold scaling: upper bound
  !!        <LI> Hubbard et al. 2017 L-I threshold scaling</OL>
  real(kind(1.0D0)) :: ptremw = 0.0D0
  !! ptremw : electron transport power (MW)
  real(kind(1.0D0)) :: ptrepv = 0.0D0
  !! ptrepv : electron transport power per volume (MW/m3)
  real(kind(1.0D0)) :: ptrimw = 0.0D0
  !! ptrimw : ion transport power (MW)
  real(kind(1.0D0)) :: pscalingmw = 0.0D0
  !! pscalingmw : Total transport power from scaling law (MW)
  real(kind(1.0D0)) :: ptripv = 0.0D0
  !! ptripv : ion transport power per volume (MW/m3)
  real(kind(1.0D0)) :: q = 3.0D0
  !! q /3.0/ : safety factor 'near' plasma edge (iteration variable 18):
  !!           equal to q95 (unless icurr = 2 (ST current scaling),
  !!           in which case q = mean edge safety factor qbar)
  real(kind(1.0D0)) :: q0 = 1.0D0
  !! q0 /1.0/ : safety factor on axis
  real(kind(1.0D0)) :: q95 = 0.0D0
  !! q95 : safety factor at 95% surface
  real(kind(1.0D0)) :: qfuel = 0.0D0
  !! qfuel : plasma fuelling rate (nucleus-pairs/s)

  real(kind(1.0D0)) :: tauratio = 1.0D0
  !! tauratio /1.0/ : ratio of He and pellet particle confinement times


  real(kind(1.0D0)) :: qlim = 0.0D0
  !! qlim : lower limit for edge safety factor
  real(kind(1.0D0)) :: qstar = 0.0D0
  !! qstar : cylindrical safety factor
  real(kind(1.0D0)) :: rad_fraction_sol = 0.8D0
  !! rad_fraction_sol /0.8/ : SoL radiation fraction 
  real(kind(1.0D0)) :: rad_fraction = 0.0D0
  !!rad_fraction : Radiation fraction = total radiation / total power deposited in plasma
  real(kind(1.0D0)) :: ralpne = 0.10D0
  !! ralpne /0.1/ : thermal alpha density / electron density (iteration variable 109)
  !!           (calculated if ipedestal=3)
  real(kind(1.0D0)) :: protium = 0.0D0
  !! protium /0.0/ : Seeded protium density / electron density.

  real(kind(1.0D0)) :: rli = 0.9D0
  !! rli /0.9/ : plasma normalised internal inductance;
  !!             calculated from alphaj if iprofile=1
  real(kind(1.0D0)) :: rlp = 0.0D0
  !! rlp : plasma inductance (H)
  real(kind(1.0D0)), bind(C) :: rmajor = 8.14D0
  !! rmajor /8.14/ : plasma major radius (m) (iteration variable 3)
  real(kind(1.0D0)), bind(C) :: rminor = 0.0D0
  !! rminor : plasma minor radius (m)
  real(kind(1.0D0)) :: rnbeam = 0.005D0
  !! rnbeam /0.005/ : hot beam density / n_e (iteration variable 7)
  real(kind(1.0D0)) :: rncne = 0.0D0
  !! rncne : n_carbon / n_e
  real(kind(1.0D0)), bind(C) :: rndfuel = 0.0D0
  !! rndfuel : fuel burnup rate (reactions/second)
  real(kind(1.0D0)) :: rnfene = 0.0D0
  !! rnfene : n_highZ / n_e
  real(kind(1.0D0)) :: rnone = 0.0D0
  !! rnone : n_oxygen / n_e
  real(kind(1.0D0)) :: rpfac = 0.0D0
  !! rpfac : neo-classical correction factor to rplas
  real(kind(1.0D0)), bind(C) :: rplas = 0.0D0
  !! rplas : plasma resistance (ohm)

  real(kind(1.0D0)) :: res_time = 0.0D0
  !! res_time : plasma current resistive diffusion time (s)


  real(kind(1.0D0)) :: sarea = 0.0D0
  !! sarea : plasma surface area
  real(kind(1.0D0)) :: sareao = 0.0D0
  !! sareao : outboard plasma surface area
  real(kind(1.0D0)) :: sf = 0.0D0
  !! sf : shape factor = plasma poloidal perimeter / (2.pi.rminor)

  integer :: i_single_null = 1
  !! i_single_null /1/ : switch for single null / double null plasma:<UL>
  !!         <LI> = 0 for double null;
  !!         <LI> = 1 for single null (diverted side down)</UL>

  real(kind(1.0D0)) :: ssync = 0.6D0
  !! ssync /0.6/ : synchrotron wall reflectivity factor
  real(kind(1.0D0)) :: tauee = 0.0D0
  !! tauee : electron energy confinement time (sec)
  real(kind(1.0D0)) :: tauee_in = 0.0D0
  !! tauee_in /0.0/  : Input electron energy confinement time (sec) (isc=48 only)
  real(kind(1.0D0)) :: taueff = 0.0D0
  !! taueff : global thermal energy confinement time (sec)
  real(kind(1.0D0)) :: tauei = 0.0D0
  !! tauei : ion energy confinement time (sec)
  real(kind(1.0D0)) :: taup = 0.0D0
  !! taup : alpha particle confinement time (sec)

  real(kind(1.0D0)) :: te = 12.9D0
  !! te /12.9/ : volume averaged electron temperature (keV)
  !!             (iteration variable 4)
  !!             (calculated if ipedestal = 3)
  real(kind(1.0D0)) :: te0 = 0.0D0
  !! te0 : central electron temperature (keV)
  real(kind(1.0D0)) :: ten = 0.0D0
  !! ten : density weighted average electron temperature (keV)
  real(kind(1.0D0)) :: ti = 12.9D0
  !! ti /12.9/ : volume averaged ion temperature (keV);
  !!             N.B. calculated from te if tratio > 0.0
  real(kind(1.0D0)) :: ti0 = 0.0D0
  !! ti0 : central ion temperature (keV)
  real(kind(1.0D0)) :: tin = 0.0D0
  !! tin : density weighted average ion temperature (keV)
  real(kind(1.0D0)) :: tratio = 1.0D0
  !! tratio /1.0/ : ion temperature / electron temperature;
  !!                used to calculate ti if tratio > 0.0
  real(kind(1.0D0)), bind(C) :: triang = 0.36D0
  !! triang /0.36/ : plasma separatrix triangularity (calculated if ishape=1, 3 or 4)
  real(kind(1.0D0)) :: triang95 = 0.24D0
  !! triang95 /0.24/ : plasma triangularity at 95% surface (calculated if ishape < 3)
  real(kind(1.0D0)), bind(C) :: vol = 0.0D0
  !! vol : plasma volume (m3)
  real(kind(1.0D0)) :: vsbrn = 0.0D0
  !! vsbrn : V-s needed during flat-top (heat + burn times) (Wb)
  real(kind(1.0D0)) :: vshift = 0.0D0
  !! vshift : plasma/device midplane vertical shift - single null
  real(kind(1.0D0)) :: vsind = 0.0D0
  !! vsind : internal and external plasma inductance V-s (Wb)
  real(kind(1.0D0)) :: vsres = 0.0D0
  !! vsres : resistive losses in startup V-s (Wb)
  real(kind(1.0D0)) :: vsstt = 0.0D0
  !! vsstt : total V-s needed (Wb)
  real(kind(1.0D0)), bind(C) :: wallmw = 0.0D0
  !! wallmw : average neutron wall load (MW/m2)
  real(kind(1.0D0)), bind(C) :: wtgpd = 0.0D0
  !! wtgpd : mass of fuel used per day (g)
  real(kind(1.0D0)) :: xarea = 0.0D0
  !! xarea : plasma cross-sectional area (m2)
  real(kind(1.0D0)) :: zeff = 0.0D0
  !! zeff : plasma effective charge
  real(kind(1.0D0)) :: zeffai = 0.0D0
  !! zeffai : mass weighted plasma effective charge

end module physics_variables


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module plasmod_variables

  !! Module containing global variables relating to PLASMOD
  !! This module contains global variables relating to PLASMOD
  !! E. Fable et al., Fusion Engineering and Design, Volume 130, May 2018, Pages 131-136
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use structs


  implicit none


  public

 !Derived type numerics_transp
  real(kind(1.0D0)) :: plasmod_tol = 1.0d-10
  !! plasmod_tol /1.0d-10/ : tolerance to be reached at each time step (%)
  real(kind(1.0D0)) :: plasmod_dtmin = 0.05d0
  !! plasmod_dtmin /0.05d0/ : min time step
  real(kind(1.0D0)) :: plasmod_dtmax = 0.1d0
  !! plasmod_dtmax /0.1d0/ : max time step
  real(kind(1.0D0)) :: plasmod_dt = 0.01d0
  !! plasmod_dt /0.01d0/ : time step
  real(kind(1.0D0)) :: plasmod_dtinc = 2.0d0
  !! plasmod_dtinc /2.0d0/ : decrease of dt
  real(kind(1.0D0)) :: plasmod_Ainc = 1.1d0
  !! plasmod_Ainc /1.1d0/ : increase of dt
  real(kind(1.0D0)) :: plasmod_test = 1000000.0d0
  !! plasmod_test /100000.0d0/ : max number of iterations
  real(kind(1.0D0)) :: plasmod_tolmin = 10.1d0
  !! plasmod_tolmin /10.1d0/ : multiplier of etolm which can not be exceeded
  real(kind(1.0D0)) :: plasmod_eopt = 0.15d0
  !! plasmod_eopt /0.15d0/ : exponent of jipperdo
  real(kind(1.0D0)) :: plasmod_dtmaxmin = 0.15d0
  !! plasmod_dtmaxmin /0.15d0/ : exponent of jipperdo2
  real(kind(1.0D0)) :: plasmod_dtmaxmax = 0.0d0
  !! plasmod_dtmaxmax /0.0d0/ : stabilizing coefficient
  real(kind(1.0D0)) :: plasmod_capA = 0.1d0
  !! plasmod_capA /0.1d0/ : first radial grid point
  real(kind(1.0D0)) :: plasmod_maxA = 0.0d0
  !! plasmod_maxA /0.0d0/ : diagz 0 or 1
  real(kind(1.0D0)) :: plasmod_dgy = 1.0d-5
  !! plasmod_dgy /1.0d-5/ : Newton differential

  integer :: plasmod_iprocess = 1
  !! plasmod_iprocess /1/ : 0 - use PLASMOD functions, 1 - use PROCESS functions
  integer :: plasmod_i_modeltype = 1
  !! plasmod_i_modeltype /1/ : switch for the transport model <UL>
  !! <LI> 1 - Simple gyrobohm scaling with imposed
  !! H factor > 1. Other values give H factor as output
  !! <LI> 111 - roughly calibrated to give H=1 for DEMO, but not fixed H </UL>

  integer :: plasmod_i_equiltype = 1
  !! plasmod_i_equiltype /1/ : 1 - EMEQ, solve with sawteeth and inputted q95.
  !! 2 - EMEQ, solve with sawteeth and inputted Ip (not recommended!).

  integer :: plasmod_isawt = 1
  !! plasmod_isawt /1/ : 0 - no sawteeth, 1 - solve with sawteeth.

  integer :: plasmod_nx = 41
  !! plasmod_nx /41/ : number of interpolated grid points
  integer :: plasmod_nxt = 7
  !! plasmod_nxt /7/ : number of solved grid points
  integer :: plasmod_nchannels = 3
  !! plasmod_nchannels /3/ : leave this at 3
  integer :: plasmod_i_impmodel = 1
  !! plasmod_i_impmodel /1/ : impurity model: 0 - fixed concentration,
  !! 1 - fixed concentration at pedestal top, then fixed density.

 !Derived type composition
  real(kind(1.0D0)), dimension(5) :: plasmod_globtau = (/ 5.0d0, 5.0d0, 7.0d0, 5.0d0, 1.0d0 /)
  !! plasmod_globtau(5) /5.0d0, 5.0d0, 7.0d0, 5.0d0, 1.0d0/ : tauparticle/tauE for D, T, He, Xe, Ar
  !! (NOT used for Xe!)
  real(kind(1.0D0)) :: plasmod_psepplh_sup = 12000.0d0
  !! plasmod_psepplh_sup /12000.0d0/ : Psep/PLH if above this, use Xe
  real(kind(1.0D0)) :: plasmod_qdivt = 0.0d0
  !! plasmod_qdivt /0.0d0/ : divertor heat flux in MW/m^2, if 0, dont use SOL model
  integer, dimension(3) :: plasmod_imptype = (/ 14, 13, 9 /)
  !! plasmod_imptype(3) /14, 13, 9/ : Impurities: element 1 - intrinsic impurity, element 2 - Psep control, element 3 - seeding for SOL (defaults: W, Xe, Ar)

 !Derived type inputs
  real(kind(1.0D0)) :: plasmod_qnbi_psepfac = 50.0d0
  !! plasmod_qnbi_psepfac /50.0d0/ : dqnbi/d(1-Psep/PLH)
  real(kind(1.0D0)) :: plasmod_cxe_psepfac = 1.0d-4
  !! plasmod_cxe_psepfac /1.0d-4/ : dcxe/d(1-Psep/PLH)
  real(kind(1.0D0)) :: plasmod_car_qdivt = 1.0d-4
  !! plasmod_car_qdivt /1.0d-4/ : dcar/d(qdivt)
  real(kind(1.0D0)) :: plasmod_maxpauxor = 20.0d0
  !! plasmod_maxpauxor /20.0d0/ : max allowed auxiliary power / R
    !deposition locations
  real(kind(1.0D0)), dimension(2) :: plasmod_x_heat = (/ 0.0d0, 0.0d0 /)
  !! plasmod_x_heat(2) /0.0d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_cd = (/ 0.0d0, 0.0d0 /)
  !! plasmod_x_cd(2) /0.0d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_fus = (/ 0.0d0, 0.0d0 /)
  !! plasmod_x_fus(2) /0.0d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_control = (/ 0.0d0, 0.0d0 /)
  !! plasmod_x_control(2) /0.0d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_heat = (/ 0.2d0, 0.03d0 /)
  !! plasmod_dx_heat(2) /0.2d0, 0.03d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_cd = (/ 0.2d0, 0.03d0 /)
  !! plasmod_dx_cd(2) /0.2d0, 0.03/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_fus = (/ 0.2d0, 0.03d0 /)
  !! plasmod_dx_fus(2) /0.2d0, 0.03d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_control = (/ 0.2d0, 0.03d0 /)
  !! plasmod_dx_control(2) /0.2d0, 0.03d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)) :: plasmod_contrpovs = 0.0d0
  !! plasmod_contrpovs /0.0d0/ :: control power in Paux/lateral_area (MW/m2)
  real(kind(1.0D0)) :: plasmod_contrpovr = 0.0d0
  !! plasmod_contrpovr /0.0d0/ :: control power in Paux/R (MW/m)
  real(kind(1.0D0)) :: plasmod_nbi_energy = 1000.0d0
  !! plasmod_nbi_energy /1000.0d0/ :: In keV
  real(kind(1.0D0)) :: plasmod_v_loop = -1.0d-6
  !! plasmod_v_loop /-1.0d-6/ :: target loop voltage. If lower than -1.e5 do not use
  real(kind(1.0D0)) :: plasmod_pfus = 0.0d0
  !! plasmod_pfus /0.0d0/ :: if 0. not used (otherwise controlled with Pauxheat)
  real(kind(1.0D0)) :: plasmod_eccdeff = 0.3d0
  !! plasmod_eccdeff /0.3d0/ :: current drive multiplier: CD = eccdeff*PCD*TE/NE (not in use yet)
  real(kind(1.0D0)) :: plasmod_fcdp = -1.0d0
  !! plasmod_fcdp /-1.0d0/ :: (P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power (iteration variable 147)
  real(kind(1.0D0)) :: plasmod_fradc = -1.0d0
  !! plasmod_fradc /-1.0d0/ :: Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad) (iteration variable 148)
  real(kind(1.0D0)) :: plasmod_pech = 0.0d0
  !! plasmod_pech /0.0d0/ :: ech power (not in use yet)
  real(kind(1.0D0)) :: plasmod_gamcdothers = 1.0d0
  !! plasmod_gamcdothers /1.0d0/ :: efficiency multiplier for non-CD heating. If 0.0 pheat treated as if it had no current drive associated
  real(kind(1.0D0)) :: plasmod_chisawpos = -1.0d0
  !! plasmod_chisawpos /-1.0d0/ :: position where artificial sawtooth diffusivity is added, -1 - uses q=1 position
  real(kind(1.0D0)) :: plasmod_chisaw = 0.0d0
  !! plasmod_chisaw /0.0d0/ :: artificial diffusivity in m^2/s
  real(kind(1.0D0)) :: plasmod_sawpertau = 1.0d-6
  !! plasmod_sawpertau /1.0d-6/ :: ratio between sawtooth period and confinement time
  real(kind(1.0D0)) :: plasmod_spellet = 0.0d0
  !! plasmod_spellet /0.0d0/ :: pellet mass in units of D in 10^19
  real(kind(1.0D0)) :: plasmod_fpellet = 0.5d0
  !! plasmod_fpellet /0.5d0/ :: pellet frequency in Hz

  !Derived type pedestal
  real(kind(1.0D0)) :: plasmod_pedscal = 1.0d0
  !! plasmod_pedscal /1.0d0/ :: multiplication factor of the pedestal scaling in PLASMOD
  !!                            can be used to scan the pedestal height.


  type (geometry) :: geom
  !! geom ::  Derived type containing all geometry information for PLASMOD
  type (composition) :: comp
  !! comp ::  Derived type containing all composition information for PLASMOD
  type (pedestal) :: ped
  !! ped ::  Derived type containing all pedestal information for PLASMOD
  type (inputs) :: inp0
  !! inp0 ::  Derived type containing miscellaneous input information for PLASMOD
  type (radial_profiles) :: radp
  !! radp ::  Derived type containing all radial profile information for PLASMOD
  type (MHD_EQ) :: mhd
  !! mhd ::  Derived type containing all mhd information for PLASMOD
  type (power_losses) :: loss
  !! loss ::  Derived type containing all power loss information for PLASMOD
  type (numerics_transp) :: num
  !! num ::  Derived type containing all numerics information for PLASMOD
  integer :: i_flag
  !! i_flag ::  Error flag for PLASMOD


end module plasmod_variables


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module current_drive_variables

  !! Module containing global variables relating to the
  !! current drive system
  !! This module contains global variables relating to tokamak
  !! current drive systems.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: beamwd = 0.58D0
  !! beamwd /0.58/ : width of neutral beam duct where it passes
  !!                 between the TF coils (m)
  !!   (T Inoue et al, Design of neutral beam system for ITER-FEAT,
  !!    <A HREF=http://dx.doi.org/10.1016/S0920-3796(01)00339-8>
  !!     Fusion Engineering and Design, Volumes 56-57, October 2001, Pages 517-521</A>)
  real(kind(1.0D0)) :: bigq = 0.0D0
  !! bigq : Fusion gain; P_fusion / (P_injection + P_ohmic)
  real(kind(1.0D0)) :: bootipf = 0.0D0
  !! bootipf : bootstrap current fraction (enforced; see ibss)
  real(kind(1.0D0)) :: bscfmax = 0.9D0
  !! bscfmax /0.9/ : maximum fraction of plasma current from bootstrap;
  !!                 if bscfmax < 0, bootstrap fraction = abs(bscfmax)
  real(kind(1.0D0)) :: bscf_iter89 = 0.0D0
  !! bscf_iter89 : bootstrap current fraction, ITER 1989 model
  real(kind(1.0D0)) :: bscf_nevins = 0.0D0
  !! bscf_nevins : bootstrap current fraction, Nevins et al model
  real(kind(1.0D0)) :: bscf_sauter = 0.0D0
  !! bscf_sauter : bootstrap current fraction, Sauter et al model
  real(kind(1.0D0)) :: bscf_wilson = 0.0D0
  !! bscf_wilson : bootstrap current fraction, Wilson et al model
  real(kind(1.0D0)) :: cboot = 1.0D0
  !! cboot /1.0/ : bootstrap current fraction multiplier (ibss=1)
  real(kind(1.0D0)) :: cnbeam = 0.0D0
  !! cnbeam : neutral beam current (A)
  real(kind(1.0D0)) :: echpwr = 0.0D0
  !! echpwr : ECH power (MW)
  real(kind(1.0D0)) :: echwpow = 0.0D0
  !! echwpow : ECH wall plug power (MW)
  real(kind(1.0D0)) :: effcd = 0.0D0
  !! effcd : current drive efficiency (A/W)
  real(kind(1.0D0)) :: enbeam = 1.0D3
  !! enbeam /1.0e3/ : neutral beam energy (keV) (iteration variable 19)
  real(kind(1.0D0)) :: etacd = 0.0D0
  !! etacd : auxiliary power wall plug to injector efficiency
  real(kind(1.0D0)) :: etacdfix = 0.0D0
  !! etacdfix : secondary auxiliary power wall plug to injector efficiency
  real(kind(1.0D0)) :: etaech = 0.3D0
  !! etaech /0.3/ : ECH wall plug to injector efficiency
  real(kind(1.0D0)) :: etalh = 0.3D0
  !! etalh /0.3/ : lower hybrid wall plug to injector efficiency
  real(kind(1.0D0)) :: etanbi = 0.3D0
  !! etanbi /0.3/ : neutral beam wall plug to injector efficiency
  real(kind(1.0D0)) :: fpion = 0.5D0
  !!fpion  :  fraction of beam energy to ions
  real(kind(1.0D0)) :: pnbitot = 0.0D0
  !! pnbitot : neutral beam power entering vacuum vessel
  real(kind(1.0D0)) :: nbshinemw = 0.0D0
  !! nbshinemw : neutral beam shine-through power
  real(kind(1.0D0)) :: feffcd = 1.0D0
  !! feffcd /1.0/ : current drive efficiency fudge factor (iteration variable 47)
  real(kind(1.0D0)) :: forbitloss = 0.0D0
  !! forbitloss /0.0/ : fraction of neutral beam power lost after ionisation but
  !!                    before thermalisation (orbit loss fraction)
  real(kind(1.0D0)) :: frbeam = 1.05D0
  !! frbeam /1.05/ : R_tangential / R_major for neutral beam injection
  real(kind(1.0D0)) :: ftritbm = 1.0D-6
  !! ftritbm /1.0e-6/ : fraction of beam that is tritium
  real(kind(1.0D0)) :: gamcd = 0.0D0
  !! gamcd : normalised current drive efficiency (1.0e20 A/(W m^2))
  real(kind(1.0D0)) :: gamma_ecrh = 0.35D0
  !! gamma_ecrh /0.35/ : user input ECRH gamma (1.0e20 A/(W m^2))
  real(kind(1.0D0)) :: rho_ecrh = 0.1D0
  !! rho_ecrh /0.1/ : normalised minor radius at which electron cyclotron current drive is maximum

  integer :: iefrf = 5
  !! iefrf /5/ : switch for current drive efficiency model: <OL>
  !!        <LI> Fenstermacher Lower Hybrid
  !!        <LI> Ion Cyclotron current drive
  !!        <LI> Fenstermacher ECH
  !!        <LI> Ehst Lower Hybrid
  !!        <LI> ITER Neutral Beam
  !!        <LI> new Culham Lower Hybrid model
  !!        <LI> new Culham ECCD model
  !!        <LI> new Culham Neutral Beam model
  !!        <LI> Empty (Oscillating field CD removed)
  !!        <LI> ECRH user input gamma
  !!        <LI> ECRH "HARE" model (E. Poli, Physics of Plasmas 2019) </OL>
  integer :: iefrffix = 0 
  !! iefrffix /0/ : switch for 2nd current drive efficiency model <UL>
  !!        <LI> = 0 No fixed current drive
  !!        <LI> = 1 Fenstermacher Lower Hybrid
  !!        <LI> = 2 Ion Cyclotron current drive
  !!        <LI> = 3 Fenstermacher ECH
  !!        <LI> = 4 Ehst Lower Hybrid
  !!        <LI> = 5 ITER Neutral Beam
  !!        <LI> = 6 new Culham Lower Hybrid model
  !!        <LI> = 7 new Culham ECCD model
  !!        <LI> = 8 new Culham Neutral Beam model
  !!        <LI> = 9 Empty (Oscillating field CD removed)
  !!        <LI> = 10 ECRH user input gamma
  !!        <LI> = 11 ECRH "HARE" model (E. Poli, Physics of Plasmas 2019) </UL>
  integer :: irfcd = 1
  !! irfcd /1/ : switch for current drive calculation:<UL>
  !!        <LI> = 0 turned off;
  !!        <LI> = 1 turned on</UL>
  real(kind(1.0D0)) :: nbshinef = 0.0D0
  !! nbshinef : neutral beam shine-through fraction
  real(kind(1.0D0)) :: nbshield = 0.5D0
  !! nbshield /0.5/ : neutral beam duct shielding thickness (m)
  real(kind(1.0D0)) :: pheat = 0.0D0
  !! pheat /0.0/ : heating power not used for current drive (MW)
  !!               (iteration variable 11)
  real(kind(1.0D0)) :: pheatfix = 0.0D0
  !! pheatfix /0.0/ : secondary fixed heating power not used for current drive (MW)
  real(kind(1.0D0)) :: pinjalw = 150.0D0
  !! pinjalw /150.0/ : Maximum allowable value for injected power (MW)
  !!                  (constraint equation 30)
  real(kind(1.0D0)) :: pinjemw = 0.0D0
  !! pinjemw : auxiliary injected power to electrons (MW)
  real(kind(1.0D0)) :: pinjimw = 0.0D0
  !! pinjimw : auxiliary injected power to ions (MW)
  real(kind(1.0D0)) :: pinjmw = 0.0D0
  !! pinjmw : total auxiliary injected power (MW)
  real(kind(1.0D0))  :: pinjfixmw = 0.0D0
  !! pinjfixmw : secondary total fixed auxiliary injected power (MW)
  real(kind(1.0D0)) :: plhybd = 0.0D0
  !! plhybd : lower hybrid injection power (MW)
  real(kind(1.0D0)) :: pnbeam = 0.0D0
  !! pnbeam : neutral beam injection power (MW)
  real(kind(1.0D0)) :: porbitlossmw = 0.0D0
  !! porbitlossmw : neutral beam power lost after ionisation but before
  !!                thermalisation (orbit loss power) (MW)
  real(kind(1.0D0)) :: pwplh = 0.0D0
  !! pwplh : lower hybrid wall plug power (MW)
  real(kind(1.0D0)) :: pwpnb = 0.0D0
  !! pwpnb : neutral beam wall plug power (MW)
  real(kind(1.0D0)) :: rtanbeam = 0.0D0
  !! rtanbeam : neutral beam centreline tangency radius (m)
  real(kind(1.0D0)) :: rtanmax = 0.0D0
  !! rtanmax : maximum tangency radius for centreline of beam (m)
  real(kind(1.0D0)) :: taubeam = 0.0D0
  !! taubeam : neutral beam e-decay lengths to plasma centre
  real(kind(1.0D0)) :: tbeamin = 3.0D0
  !! tbeamin /3.0/ : permitted neutral beam e-decay lengths to plasma centre

end module current_drive_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_kallenbach_variables

  !! Module containing global variables relating to the
  !! tokamak divertor components, Kallenbach model, issue #400
  !! This module contains global variables relating to tokamak
  !! divertor components.
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

  real(kind(1.0D0)) :: kallenbach_scan_start = 2.0
  !! kallenbach_scan_start /2.0/ : Start value for kallenbach scan parameter

  real(kind(1.0D0)) :: kallenbach_scan_end = 10.0
  !! kallenbach_scan_end /10.0/ : End value for kallenbach scan parameter

  integer :: kallenbach_scan_num = 1
  !! kallenbach_scan_num /1/ : Number of scans for kallenbach scan test

  real(kind(1.0D0)) :: target_spread = 0.003D0
  !! target_spread /0.003/ : Increase in SOL power fall-off length due to spreading, mapped to OMP [m]

  real(kind(1.0D0)) :: lambda_q_omp = 0.002D0
  !! lambda_q_omp /0.002/ : SOL power fall-off length at the outer midplane, perpendicular to field [m]

  real(kind(1.0D0)) :: lcon_factor = 1.0D0
  !! lcon_factor /1.0/ : Correction factor for connection length from OMP to divertor =
  !!                     connection length/(pi*q*rmajor)

  real(kind(1.0D0)) :: netau_sol = 0.5D0
  !! netau_sol /0.5/ : Parameter describing the departure from local ionisation equilibrium in the SOL. [ms.1e20/m3]

  real(kind(1.0D0)) :: targetangle = 30.0D0
  !! targetangle /30.0/ : Angle between field-line and divertor target (degrees)

  real(kind(1.0D0)) :: ttarget = 5.0D0
  !! ttarget /5.0/ : Plasma temperature adjacent to divertor sheath [eV] (iteration variable 120)
  !!                 Rem : 5 eV is the current limit for tungsten sputtering from argon impurity

  real(kind(1.0D0)) :: qtargettotal = 5.0D6
  !! qtargettotal /5.0e6/ : Power density on target including surface recombination [W/m2]
  !!(iteration variable 124)

  ! real(kind(1.0D0)) :: helium_enrichment = 1.0D0
  ! real(kind(1.0D0)) :: impurity_enrichment = 5.0D0

  real(kind(1.0D0)), dimension(14) :: impurity_enrichment = 5.0D0
  !! impurity_enrichment(14) /5.0/ : Ratio of each impurity concentration in SOL to confined plasma+
  !!the enrichment for Argon is also propagated for PLASMOD (ipedestal=3)

  real(kind(1.0D0)) :: psep_kallenbach = 0.0D0
  !! psep_kallenbach : Power conducted through the separatrix, as calculated by the divertor model [W]
  !!                   Not equal to pdivt unless constraint is imposed.

  real(kind(1.0D0)) :: teomp = 0.0D0
  !! teomp : separatrix temperature calculated by the Kallenbach divertor model [eV]

  ! Issue #457
  real(kind(1.0D0)) :: neomp = 0.0D0
  !! neomp : Mean SOL density at OMP calculated by the Kallenbach divertor model [m-3]

  real(kind(1.0D0)) :: neratio = 0.75D0
  !! neratio /0.75/ : Ratio of mean SOL density at OMP to separatrix density at OMP (iteration variable 121)

  real(kind(1.0D0)) :: pressure0 = 0.0D0
  !! pressure0 : Total plasma pressure near target (thermal+dynamic) [Pa]

  real(kind(1.0D0)) :: fractionwidesol = 0.1D0
  !! fractionwidesol /0.1/ : Distance from target at which SOL gets broader as a fraction of connection length

  real(kind(1.0D0)), public :: fmom
  !! fmom : momentum factor [-]

  real(kind(1.0D0)), public :: totalpowerlost
  !! totalpowerlost : Total power lost due to radiation, ionisation and recombination [W]

  real(kind(1.0D0)), public :: impuritypowerlost
  !! impuritypowerlost : Power lost due to impurity radiation [W]

  real(kind(1.0D0)), public :: hydrogenicpowerlost
  !! hydrogenicpowerlost : Power lost due to hydrogenic radiation [W]

  real(kind(1.0D0)), public :: exchangepowerlost
  !! exchangepowerlost : Power lost due to charge exchange  [W]

  real(kind(1.0D0)), public :: ionisationpowerlost
  !! ionisationpowerlost : Power lost due to electron impact ionisation [W]

  real(kind(1.0D0)), public :: abserr_sol = 1.d-4
  !! abserr_sol : Absolute contribution to the error tolerance in the Kallenbach divertor model

  real(kind(1.0D0)), public :: relerr_sol = 1.d-4
  !! relerr_sol : Relative contribution to the error tolerance in the Kallenbach divertor model

  real(kind(1.0D0)), public :: mach0 = 0.999
  !! mach0 : Mach number at target (must be just less than 1)


end module divertor_kallenbach_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_variables

  !! Module containing global variables relating to the
  !! tokamak divertor components
  !! This module contains global variables relating to tokamak
  !! divertor components.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: adas = 0.0D0
  !! adas : area divertor / area main plasma (along separatrix)
  real(kind(1.0D0)) :: anginc = 0.262D0
  !! anginc /0.262/ : angle of incidence of field line on plate (rad)
  real(kind(1.0D0)) :: betai = 1.0D0
  !! betai /1.0/ : poloidal plane angle between divertor plate and leg, inboard (rad)
  real(kind(1.0D0)) :: betao = 1.0D0
  !! betao /1.0/ : poloidal plane angle between divertor plate and leg, outboard (rad)
  real(kind(1.0D0)) :: bpsout = 0.60D0
  !! bpsout /0.6/ : reference B_p at outboard divertor strike point (T)
  real(kind(1.0D0)) :: c1div = 0.45D0
  !! c1div /0.45/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c2div = -7.0D0
  !! c2div /-7.0/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c3div = 0.54D0
  !! c3div /0.54/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c4div = -3.6D0
  !! c4div /-3.6/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c5div = 0.7D0
  !! c5div /0.7/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c6div = 0.0D0
  !! c6div /0.0/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: delld = 1.0D0
  !! delld /1.0/ : coeff for power distribution along main plasma
  real(kind(1.0D0)) :: dendiv = 0.0D0
  !! dendiv : plasma density at divertor (10**20 /m3)
  real(kind(1.0D0)) :: densin = 0.0D0
  !! densin : density at plate (on separatrix) (10**20 /m3)
  real(kind(1.0D0)) :: divclfr = 0.3D0
  !! divclfr /0.3/ : divertor coolant fraction
  real(kind(1.0D0)) :: divdens = 1.0D4
  !! divdens /1.0e4/ : divertor structure density (kg/m3)
  integer :: divdum = 0
  !! divdum /0/ : switch for divertor Zeff model: 0=calc, 1=input
  real(kind(1.0D0)) :: divfix = 0.2D0
  !! divfix /0.2/ : divertor structure vertical thickness (m)
  real(kind(1.0D0)) :: divmas = 0.0D0
  !! divmas : divertor plate mass (kg)
  real(kind(1.0D0)) :: divplt = 0.035D0
  !! divplt /0.035/ : divertor plate thickness (m) (from Spears, Sept 1990)
  real(kind(1.0D0)) :: divsur = 0.0D0
  !! divsur : divertor surface area (m2)
  real(kind(1.0D0)) :: fdfs = 10.0D0
  !! fdfs /10.0/ : radial gradient ratio
  real(kind(1.0D0)) :: fdiva = 1.11D0
  !! fdiva /1.11/ : divertor area fudge factor (for ITER, Sept 1990)
  real(kind(1.0D0)) :: fgamp = 1.0D0
  !! fgamp /1.0/ : sheath potential factor (not used)
  real(kind(1.0D0)) :: fhout = 0.0D0
  !! fhout : fraction of power to outboard divertor (for single null)
  real(kind(1.0D0)) :: fififi = 4.0D-3
  !! fififi /0.004/ : coefficient for gamdiv
  real(kind(1.0D0)) :: frrp = 0.4D0
  !! frrp /0.4/ : fraction of radiated power to plate
  real(kind(1.0D0)), bind(C) :: hldiv = 0.0D0
  !! hldiv : divertor heat load (MW/m2)
  real(kind(1.0D0)) :: hldivlim = 5.0D0
  !! hldivlim /5.0/ : heat load limit (MW/m2)
  real(kind(1.0D0)) :: ksic = 0.8D0
  !! ksic /0.8/ : power fraction for outboard double-null scrape-off plasma
  real(kind(1.0D0)) :: lamp = 0.0D0
  !! lamp : power flow width (m)
  real(kind(1.0D0)) :: minstang = 0.0D0
  !! minstang : minimum strike angle for heat flux calculation
  real(kind(1.0D0)) :: omegan = 1.0D0
  !! omegan /1.0/ : pressure ratio (nT)_plasma / (nT)_scrape-off
  real(kind(1.0D0)) :: omlarg = 0.0D0
  !! omlarg : power spillage to private flux factor
  real(kind(1.0D0)) :: ppdivr = 0.0D0
  !! ppdivr : peak heat load at plate (with radiation) (MW/m2)
  real(kind(1.0D0)) :: prn1 = 0.285D0
  !! prn1 /0.285/ : n-scrape-off / n-average plasma;
  !!                (input for ipedestal=0, = nesep/dene if ipedestal>=1)
  real(kind(1.0D0)) :: ptpdiv = 0.0D0
  !! ptpdiv : peak temperature at the plate (eV)
  real(kind(1.0D0)) :: rconl = 0.0D0
  !! rconl : connection length ratio, outboard side
  real(kind(1.0D0)) :: rlclolcn = 0.0D0
  !! rlclolcn : ratio of collision length / connection length
  real(kind(1.0D0)) :: rlenmax = 0.5D0
  !! rlenmax /0.5/ : maximum value for length ratio (rlclolcn) (eqn.22)
  real(kind(1.0D0)) :: rsrd = 0.0D0
  !! rsrd : effective separatrix/divertor radius ratio
  real(kind(1.0D0)) :: tconl = 0.0D0
  !! tconl : main plasma connection length (m)
  real(kind(1.0D0)) :: tdiv = 2.0D0
  !! tdiv /2.0/ : temperature at divertor (eV)
  !!              (input for stellarator only, calculated for tokamaks)
  real(kind(1.0D0)) :: tsep = 0.0D0
  !! tsep : temperature at the separatrix (eV)
  real(kind(1.0D0)) :: xparain = 2.1D3
  !! xparain /2.1e3/ : parallel heat transport coefficient (m2/s)
  real(kind(1.0D0)) :: xpertin = 2.0D0
  !! xpertin /2.0/ : perpendicular heat transport coefficient (m2/s)
  real(kind(1.0D0)) :: zeffdiv = 1.0D0
  !! zeffdiv /1.0/ : Zeff in the divertor region (if divdum /= 0)

end module divertor_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module fwbs_variables

  !! Module containing global variables relating to the
  !! first wall, blanket and shield components
  !! This module contains global variables relating to the first
  !! wall, blanket and shield components.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  ! General blanket parameters

  real(kind(1.0D0)), bind(C):: bktlife = 0.0D0
  !! bktlife : blanket lifetime (years)

  real(kind(1.0D0)) :: coolmass = 0.0D0
  !! coolmass : mass of water coolant (in shield, blanket,
  !!            first wall, divertor) (kg)

  ! Formerly known as cryomass.
  !! vvmass : vacuum vessel mass (kg)
  real(kind(1.0D0)) :: vvmass = 0.0D0

  real(kind(1.0D0)) :: denstl = 7800.0D0
  !! denstl /7800.0/ : density of steel (kg/m3)

  real(kind(1.0D0)) :: denw = 19250.0D0
  !! denw /19250.0/ : density of tungsten (kg/m3)

  real(kind(1.0D0)) :: dewmkg = 0.0D0
  !! dewmkg : total mass of vacuum vessel + cryostat (kg)

  !                         (calculated if blktmodel>0)
  !! emult /1.269/ : energy multiplication in blanket and shield
  real(kind(1.0D0)) :: emult = 1.269D0

  real(kind(1.0D0)) :: emultmw = 0.0D0
  !! emultmw : power due to energy multiplication in blanket and shield [MW]

  real(kind(1.0D0)) :: fblss = 0.09705D0
  !! fblss /0.09705/ : KIT blanket model: steel fraction of breeding zone

  real(kind(1.0D0)) :: fdiv = 0.115D0
  !! fdiv /0.115/ : area fraction taken up by divertor

  real(kind(1.0D0)) :: fhcd = 0.0D0
  !! fhcd /0.0/ : area fraction covered by heating/current drive
  !!              apparatus plus diagnostics

  real(kind(1.0D0)) :: fhole = 0.0D0
  !! fhole /0.0/ : area fraction taken up by other holes (IFE)

  integer :: fwbsshape = 2
  !! fwbsshape /2/ : first wall, blanket, shield and vacuum vessel shape:<UL>
  !!                 <LI> = 1 D-shaped (cylinder inboard + ellipse outboard);
  !!                 <LI> = 2 defined by two ellipses</UL>

  real(kind(1.0D0)) :: fwlife = 0.0D0
  !! fwlife : first wall full-power lifetime (y)

  real(kind(1.0D0)) :: fwmass = 0.0D0
  !! fwmass : first wall mass (kg)

  real(kind(1.0D0)) :: fw_armour_mass = 0.0D0
  !! fw_armour_mass : first wall armour mass (kg)

  real(kind(1.0D0)) :: fw_armour_thickness = 0.005D0
  !! fw_armour_thickness /0.005/ : first wall armour thickness (m)

  real(kind(1.0D0)) :: fw_armour_vol = 0.0D0
  !! fw_armour_vol : first wall armour volume (m3)

  integer :: iblanket = 1
  !! iblanket /1/ : switch for blanket model: <UL>
  !!            <LI> = 1 CCFE HCPB model;
  !!            <LI> = 2 KIT HCPB model;
  !!            <LI> = 3 CCFE HCPB model with Tritium Breeding Ratio calculation;
  !!            <LI> = 4 KIT HCLL model</UL>

  integer :: iblnkith = 1
  !! iblnkith /1/ : switch for inboard blanket: <UL>
  !!            <LI> = 0 No inboard blanket (blnkith=0.0);
  !!            <LI> = 1 Inboard blanket present</UL>

  integer :: inuclear = 0
  !! inuclear /0/ : switch for nuclear heating in the coils: <UL>
  !!            <LI> = 0 Frances Fox model (default);
  !!            <LI> = 1 Fixed by user (qnuc)</UL>
  real(kind(1.0D0)) :: qnuc = 0.0D0
  !! qnuc /0.0/ : nuclear heating in the coils (W) (inuclear=1)

  real(kind(1.0D0)) :: li6enrich = 30.0D0
  !! li6enrich /30.0/ : lithium-6 enrichment of breeding material (%)

  real(kind(1.0D0)), bind(C) :: pnucblkt = 0.0D0
  !! pnucblkt : nuclear heating in the blanket (MW)

  real(kind(1.0D0)) :: pnuccp = 0.0D0
  !! pnuccp : nuclear heating in the ST centrepost (MW)

  real(kind(1.0D0)) :: pnucdiv = 0.0D0
  !! pnucdiv : nuclear heating in the divertor (MW)

  real(kind(1.0D0)) :: pnucfw = 0.0D0
  !! pnucfw : nuclear heating in the first wall (MW)

  real(kind(1.0D0)) :: pnuchcd = 0.0D0
  !! pnuchcd : nuclear heating in the HCD apparatus and diagnostics (MW)

  real(kind(1.0D0)) :: pnucloss = 0.0D0
  !! pnucloss : nuclear heating lost via holes (MW)

  real(kind(1.0D0)) :: pnucvvplus = 0.0D0
  !! pnucloss : nuclear heating to vacuum vessel and beyond(MW)

  real(kind(1.0D0)), bind(C) :: pnucshld = 0.0D0
  !! pnucshld : nuclear heating in the shield (MW)

  real(kind(1.0D0)) :: whtblkt = 0.0D0
  !! whtblkt : mass of blanket (kg)

  real(kind(1.0D0)) :: whtblss = 0.0D0
  !! whtblss : mass of blanket - steel part (kg)

  real(kind(1.0D0)) :: armour_fw_bl_mass = 0.0D0
  !! armour_fw_bl_mass : Total mass of armour, first wall and blanket (kg)


  ! CCFE HCPB Blanket Model (with or without TBR calculation)
  ! ----------

  !! <P><B>The following are used only in the CCFE HCPB blanket model
  !! (iblanket=1):</B><P>
  real(kind(1.0D0)) :: breeder_f = 0.5D0
  !! breeder_f /0.5/ :  Volume ratio: Li4SiO4/(Be12Ti+Li4SiO4) (iteration variable 108)
  
  real(kind(1.0D0)) :: breeder_multiplier = 0.75D0
  !! breeder_multiplier /0.75/ : combined breeder/multipler fraction of blanket by volume
  
  real(kind(1.0D0)) :: vfcblkt = 0.05295D0
  !! vfcblkt /0.05295/ : He coolant fraction of blanket by volume
  !!                  (iblanket = 1 or 3 (CCFE HCPB))
  
  real(kind(1.0D0)) :: vfpblkt = 0.1D0
  !! vfpblkt /0.1/ : He purge gas fraction of blanket by volume
  !!                  (iblanket = 1 or 3 (CCFE HCPB))

  real(kind(1.0D0)) :: whtblli4sio4 = 0.0D0
  !! whtblli4sio4 : mass of lithium orthosilicate in blanket (kg)
  !!                  (iblanket = 1 or 3 (CCFE HCPB))
  
  real(kind(1.0D0)) :: whtbltibe12 = 0.0D0
  !! whtbltibe12 : mass of titanium beryllide in blanket (kg)
  !!                  (iblanket = 1 or 3 (CCFE HCPB))

  real(kind(1.0D0)) :: f_neut_shield = -1.0D0
  !! f_neut_shield : Fraction of nuclear power shielded before the CP magnet (ST)
  !!                 ( neut_absorb = -1 --> a fit on simplified MCNP neutronic
  !!                   calculation is used assuming water cooled (13%) tungesten carbyde )
  ! ----------


  !  KIT HCPB blanket model

  !! <P><B>The following are used in the KIT HCPB blanket model
  !! (iblanket=2):</B><P>
  integer :: breedmat = 1
  !! breedmat /1/ : breeder material switch (iblanket=2 (KIT HCPB)):<UL>
  !!                 <LI> = 1 Lithium orthosilicate;
  !!                 <LI> = 2 Lithium methatitanate;
  !!                 <LI> = 3 Lithium zirconate</UL>
  real(kind(1.0D0)) :: densbreed = 0.0D0
  !! densbreed : density of breeder material (kg/m3) (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblbe = 0.6D0
  !! fblbe /0.6/ : beryllium fraction of blanket by volume
  !!               (if (iblanket=2 (KIT HCPB)), Be fraction of breeding zone)
  real(kind(1.0D0)) :: fblbreed = 0.154D0
  !! fblbreed /0.154/ : breeder fraction of blanket breeding zone by volume
  !!                    (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblhebmi = 0.4D0
  !! fblhebmi /0.40/ : helium fraction of inboard blanket box manifold by volume
  !!                    (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblhebmo = 0.4D0
  !! fblhebmo /0.40/ : helium fraction of outboard blanket box manifold by volume
  !!                    (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblhebpi = 0.6595D0
  !! fblhebpi /0.6595/ : helium fraction of inboard blanket back plate by volume
  !!                    (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblhebpo = 0.6713D0
  !! fblhebpo /0.6713/ : helium fraction of outboard blanket back plate by volume
  !!                    (iblanket=2 (KIT HCPB))
  integer :: hcdportsize = 1
  !! hcdportsize /1/ : size of heating/current drive ports (iblanket=2 (KIT HCPB)): <UL>
  !!                 <LI> = 1 'small'
  !!                 <LI> = 2 'large'</UL>
  real(kind(1.0D0)) :: nflutf = 0.0D0
  !! nflutf : peak fast neutron fluence on TF coil superconductor (n/m2)
  !!          (iblanket=2 (KIT HCPB))
  integer :: npdiv = 2
  !! npdiv /2/ : number of divertor ports (iblanket=2 (KIT HCPB))
  integer :: nphcdin = 2
  !! nphcdin /2/ : number of inboard ports for heating/current drive
  !!               (iblanket=2 (KIT HCPB))
  integer :: nphcdout = 2
  !! nphcdout /2/ : number of outboard ports for heating/current drive
  !!                (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: tbr = 0.0D0
  !! tbr : tritium breeding ratio (iblanket=2,3 (KIT HCPB/HCLL))
  real(kind(1.0D0)) :: tritprate = 0.0D0
  !! tritprate : tritium production rate (g/day) (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: vvhemax = 0.0D0
  !! vvhemax : maximum helium concentration in vacuum vessel at end of
  !!           plant life (appm) (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: wallpf = 1.21D0
  !! wallpf /1.21/ : neutron wall load peaking factor (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: whtblbreed = 0.0D0
  !! whtblbreed : mass of blanket - breeder part (kg) (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: whtblbe = 0.0D0
  !! whtblbe : mass of blanket - beryllium part (kg)


  !! <P><B>CCFE HCPB model with Tritium Breeding Ratio calculation
  !! (iblanket=3):</B><P>

  !! tbrmin /1.1/ : minimum tritium breeding ratio (constraint equation 52)
  !!                (If iblanket=1, tbrmin=minimum 5-year time-averaged tritium breeding ratio)
  integer :: iblanket_thickness = 2
  !! iblanket_thickness /2/ : Blanket thickness switch:<UL>
  !!    <LI> = 1 thin    0.53 m inboard, 0.91 m outboard
  !!    <LI> = 2 medium  0.64 m inboard, 1.11 m outboard
  !!    <LI> = 3 thick   0.75 m inboard, 1.30 m outboard</UL>
  !! Do not set blnkith, blnkoth, fwith or fwoth when iblanket=3.

  integer :: primary_pumping = 2
  !! primary_pumping /2/ : Switch for pumping power for primary coolant (06/01/2016):
  !!      (mechanical power only)<UL>
  !!    <LI> = 0 User sets pump power directly (htpmw_blkt, htpmw_fw, htpmw_div, htpmw_shld)
  !!    <LI> = 1 User sets pump power as a fraction of thermal power (fpumpblkt, fpumpfw, fpumpdiv, fpumpshld)
  !!    <LI> = 2 Mechanical pumping power is calculated
  !!    <LI> = 3 Mechanical pumping power is calculated using specified pressure drop</UL>
  !! (peak first wall temperature is only calculated if primary_pumping = 2)

  integer :: secondary_cycle = 0
  !! secondary_cycle /0/ : Switch for power conversion cycle:<UL>
  !!    <LI> = 0 Set efficiency for chosen blanket, from detailed models (divertor heat not used)
  !!    <LI> = 1 Set efficiency for chosen blanket, from detailed models (divertor heat used)
  !!    <LI> = 2 user input thermal-electric efficiency (etath)
  !!    <LI> = 3 steam Rankine cycle
  !!    <LI> = 4 supercritical CO2 cycle</UL>
  integer, bind(C) :: coolwh = 1
  !! coolwh : Blanket coolant (set via blkttype):<UL>
  !!        <LI> = 1 helium;
  !!        <LI> = 2 pressurized water</UL>
  real(kind(1.0D0)) :: afwi = 0.008D0
  !! afwi /0.008/ : inner radius of inboard first wall/blanket coolant channels OBSOLETE (m)
  real(kind(1.0D0)) :: afwo = 0.008D0
  !! afwo /0.008/ : inner radius of outboard first wall/blanket coolant channels OBSOLETE (m)

  ! MDK New first wall calculation
  character(len=6) :: fwcoolant = 'helium'
  !! fwcoolant /helium/ : first wall coolant (can be different from blanket coolant)
  !!                      'helium' or 'water'  (27/11/2015)
  real(kind(1.0D0)) :: fw_wall = 0.003D0
  !! fw_wall /0.003/ : wall thickness of first wall coolant channels (m) (27/11/2015)
  real(kind(1.0D0)) :: afw = 0.006D0
  !! afw /0.006/ : radius of first wall cooling channels (m) (27/11/15)
  real(kind(1.0D0)) :: pitch = 0.020D0
  !! pitch /0.020/ : pitch of first wall cooling channels (m) (27/11/15)
  real(kind(1.0D0)) :: fwinlet = 573.0D0
  !! fwinlet /573/ : inlet temperature of first wall coolant (K) (27/11/2015)
  real(kind(1.0D0)) :: fwoutlet = 823.0D0
  !! fwoutlet /823/ : outlet temperature of first wall coolant (K) (27/11/2015)
  real(kind(1.0D0)) :: fwpressure = 15.5D6
  !! fwpressure /15.5e6/ : first wall coolant pressure (Pa) (secondary_cycle>1)
  real(kind(1.0D0)) :: tpeak = 873.0D0
  !! tpeak : peak first wall temperature (K) (27/11/2015)
  real(kind(1.0D0)) :: roughness = 1.0D-6
  !! roughness /1e-6/ : first wall channel roughness epsilon (m) (27/11/2015)
  real(kind(1.0D0)) :: fw_channel_length = 4.0D0
  !! fw_channel_length /4.0/ : Length of a single first wall channel (all in parallel) (m) (27/11/2015)
  !!                           (iteration variable 114, useful for constraint equation 39)
  real(kind(1.0D0)) :: peaking_factor = 1.0D0
  !! peaking_factor /1.0/ : peaking factor for first wall heat loads (27/11/2015)
  !!                        (Applied separately to inboard and outboard loads.
  !!                        Applies to both neutron and surface loads.
  !!                        Only used to calculate peak temperature - not the coolant flow rate.)

  ! MDK Blanket has not changed as much, but some new variable names
  real(kind(1.0D0)) :: blpressure = 15.5D6
  !! blpressure /15.5e6/ : blanket coolant pressure (Pa) (secondary_cycle>1) (27/11/2015)
  real(kind(1.0D0)) :: inlet_temp = 573.0D0
  !! inlet_temp /573.0/ : inlet temperature of blanket coolant  (K) (secondary_cycle>1) (27/11/2015)
  real(kind(1.0D0)) :: outlet_temp = 823.0D0
  !! outlet_temp /823.0/ : outlet temperature of blanket coolant (K) (27/11/2015)<UL>
  !!        <LI> (secondary_cycle>1);
  !!        <LI> input if coolwh=1 (helium), calculated if coolwh=2 (water)</UL>


  real(kind(1.0D0)) :: coolp = 15.5D6
  !! coolp /15.5e6/ : blanket coolant pressure (Pa) stellarator ONLY (27/11/2015)


  integer :: nblktmodpo = 8
  !! nblktmodpo /8/ : number of outboard blanket modules in poloidal direction (secondary_cycle>1)
  integer :: nblktmodpi = 7
  !! nblktmodpi /7/ : number of inboard blanket modules in poloidal direction (secondary_cycle>1)
  integer :: nblktmodto = 48
  !! nblktmodto /48/ : number of outboard blanket modules in toroidal direction (secondary_cycle>1)
  integer :: nblktmodti = 32
  !! nblktmodti /32/ : number of inboard blanket modules in toroidal direction (secondary_cycle>1)
  real(kind(1.0D0)) :: tfwmatmax = 823.0D0
  !! tfwmatmax /823.0/ : maximum temperature of first wall material (K) (secondary_cycle>1)
  real(kind(1.0D0)) :: fw_th_conductivity = 28.34D0
  !! fw_th_conductivity /28.34/ : thermal conductivity of first wall material at
  !!       293 K (W/m/K) (Temperature dependence is as for unirradiated Eurofer)


  real(kind(1.0D0)) :: fvoldw = 1.74D0
  !! fvoldw /1.74/ : area coverage factor for vacuum vessel volume
  real(kind(1.0D0)) :: fvolsi = 1.0D0
  !! fvolsi /1.0/ : area coverage factor for inboard shield volume
  real(kind(1.0D0)) :: fvolso = 0.64D0
  !! fvolso /0.64/ : area coverage factor for outboard shield volume
  real(kind(1.0D0)) :: fwclfr = 0.15D0
  !! fwclfr /0.15/ : first wall coolant fraction
  !!                 (calculated if lpulse=1 or ipowerflow=1)
  real(kind(1.0D0)) :: praddiv = 0.0D0
  !! praddiv : radiation power incident on the divertor (MW)
  real(kind(1.0D0)) :: pradfw = 0.0D0
  !! pradfw : radiation power incident on the divertor (MW)
  real(kind(1.0D0)) :: pradhcd = 0.0D0
  !! pradhcd : radiation power incident on the divertor (MW)
  real(kind(1.0D0)) :: pradloss = 0.0D0
  !! pradloss : radiation power incident on the divertor (MW)
  real(kind(1.0D0)) :: ptfnuc = 0.0D0
  !! ptfnuc : nuclear heating in the TF coil (MW)
  real(kind(1.0D0)) :: ptfnucpm3 = 0.0D0
  !! ptfnucpm3 : nuclear heating in the TF coil (MW/m3) (blktmodel>0)
  real(kind(1.0D0)) :: rdewex = 0.0D0
  !! rdewex : cryostat radius (m)
  real(kind(1.0D0)) :: zdewex = 0.0D0
  !! zdewex : cryostat height (m)
  real(kind(1.0D0)) :: rpf2dewar = 0.5D0
  !! rpf2dewar /0.5/ : radial distance between outer edge of largest
  !!                   ipfloc=3 PF coil (or stellarator modular coil)
  !!                   and cryostat (m)
  real(kind(1.0D0)) :: vdewex = 0.0D0
  !! vdewex : cryostat volume (m3)
  real(kind(1.0D0)) :: vdewin = 0.0D0
  !! vdewin : vacuum vessel volume (m3)
  real(kind(1.0D0)) :: vfshld = 0.25D0
  !! vfshld /0.25/ : coolant void fraction in shield
  real(kind(1.0D0)) :: volblkt = 0.0D0
  !! volblkt : volume of blanket (m3)
  real(kind(1.0D0)) :: volblkti = 0.0D0
  !! volblkti : volume of inboard blanket (m3)
  real(kind(1.0D0)) :: volblkto = 0.0D0
  !! volblkto : volume of outboard blanket (m3)
  real(kind(1.0D0)) :: volshld = 0.0D0
  !! volshld : volume of shield (m3)
  real(kind(1.0D0)) :: whtshld = 0.0D0
  !! whtshld : mass of shield (kg)
  real(kind(1.0D0)) :: wpenshld = 0.0D0
  !! wpenshld : mass of the penetration shield (kg)
  real(kind(1.0D0)) :: wtshldi = 0.0D0
  !! wtshldi : mass of inboard shield (kg)
  real(kind(1.0D0)) :: wtshldo = 0.0D0
  !! wtshldo : mass of outboard shield (kg)

  integer :: irefprop = 1
  !! irefprop /1/ : obsolete

  real(kind(1.0D0)) :: fblli = 0.0D0
  real(kind(1.0D0)) :: fblli2o = 0.08D0
  !! fblli2o /0.08/ : lithium oxide fraction of blanket by volume
  !!                  (blktmodel=0)
  real(kind(1.0D0)) :: fbllipb = 0.68D0
  !! fbllipb /0.68/ : lithium lead fraction of blanket by volume
  !!                  (blktmodel=0)
  real(kind(1.0D0)) :: fblvd = 0.0D0
  !! fblvd /0.0/ : vanadium fraction of blanket by volume
  !!               (blktmodel=0)
  real(kind(1.0D0)) :: wtblli2o = 0.0D0
  !! wtblli2o : mass of blanket - Li_2O part (kg)
  real(kind(1.0D0)) :: wtbllipb = 0.0D0
  !! wtbllipb : mass of blanket - Li-Pb part (kg)
  real(kind(1.0D0)) :: whtblvd = 0.0D0
  !! whtblvd : mass of blanket - vanadium part (kg)
  real(kind(1.0D0)) :: whtblli = 0.0D0
  !! whtblli : mass of blanket - lithium part (kg)
  real(kind(1.0D0)) :: vfblkt = 0.25D0
  !! vfblkt /0.25/ : coolant void fraction in blanket (blktmodel=0),
  !!                 (calculated if blktmodel > 0)
  integer :: blktmodel = 0
  !! blktmodel /0/ : switch for blanket/tritium breeding model
  !!                 (but see <CODE>iblanket</CODE>):<UL>
  !!            <LI> = 0 original simple model;
  !!            <LI> = 1 KIT model based on a helium-cooled pebble-bed
  !!                     blanket (HCPB) reference design</UL>
  real(kind(1.0D0)) :: declblkt = 0.075D0
  !! declblkt /0.075/ : neutron power deposition decay length of blanket structural material (m)
  !! (Stellarators only)
  real(kind(1.0D0)) :: declfw = 0.075D0
  !! declfw /0.075/ : neutron power deposition decay length of first wall structural material (m)
  !!(Stellarators only)
  real(kind(1.0D0)) :: declshld = 0.075D0
  !! declshld /0.075/ : neutron power deposition decay length of shield structural material (m)
  !! (Stellarators only)
  integer :: blkttype = 3
  !! blkttype /3/ : Switch for blanket type:<UL>
  !!           <LI> = 1 WCLL; efficiency taken from WP13-DAS08-T02, EFDA_D_2M97B7
  !!           <LI> = 2 HCLL; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX
  !!           <LI> = 3 HCPB; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX</UL>

  real(kind(1.0D0)) :: etaiso = 0.85D0
  !! etaiso /0.85/ : isentropic efficiency of FW and blanket coolant pumps
  real(kind(1.0D0)) :: etahtp = 0.95D0
  !! etahtp /0.95/ : electrical efficiency of primary coolant pumps


end module fwbs_variables
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module primary_pumping_variables

  !! Module containing global variables relating to the priamry_pumping
  !! primary_pumping=3 option  (Mechanical pumping power is calculated using specified pressure drop)
  !! This module contains global variables relating to the
  !! primary pumping information
  !!  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  ! Issue #503
  real(kind(1.0D0)), parameter :: gamma_he = 1.667D0
  !! gamma_he /1.667/ FIX : ratio of specific heats for helium (primary_pumping=3)
  real(kind(1.0D0)), parameter :: cp_he = 5195.0D0
  !! cp_he /5195/ FIX: specific heat capacity at constant pressure: helium (primary_pumping=3) [J/(kg.K)]
  real(kind(1.0D0)), parameter :: t_in_bb =573.13D0
  !! t_in_bb /573.13/ FIX: temperature in FW and blanket coolant at blanket entrance (primary_pumping=3) [K]
  real(kind(1.0D0)), parameter :: t_out_bb =773.13D0
  !! t_out_bb /773.13/ FIX: temperature in FW and blanket coolant at blanket exit (primary_pumping=3) [K]
  real(kind(1.0D0)), parameter :: p_he =8.0D6
  !! p_he /8.0e6/ FIX: pressure in FW and blanket coolant at pump exit (primary_pumping=3) [Pa]
  real(kind(1.0D0)), parameter :: dp_he =5.5D5
  !! dp_he /5.5e5/ FIX: pressure drop in FW and blanket coolant including heat exchanger and pipes (primary_pumping=3) [Pa]
  real(kind(1.0D0)) :: htpmw_fw_blkt = 0.0d0
  !! htpmw_fw_blkt : mechanical pumping power for FW and blanket including heat exchanger and pipes (primary_pumping=3) [MW]

end module primary_pumping_variables
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pfcoil_variables

  !! Module containing global variables relating to the
  !! poloidal field coil systems
  !! This module contains global variables relating to the
  !! poloidal field coil systems of a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  integer, parameter :: ngrpmx = 8
  !! ngrpmx /8/ FIX : maximum number of groups of PF coils
  integer, parameter :: nclsmx = 2
  !! nclsmx /2/ FIX : maximum number of PF coils in a given group
  integer, parameter :: nptsmx = 32
  !! nptsmx /32/ FIX : maximum number of points across the midplane of the
  !!          plasma at which the field from the PF coils is fixed
  integer, parameter :: nfixmx = 64
  !! nfixmx /64/ FIX : maximum number of fixed current PF coils

  integer, parameter :: ngc = ngrpmx*nclsmx
  integer, parameter :: ngc2 = ngc+2

  real(kind(1.0D0)) :: alfapf = 5.0D-10
  !! alfapf /5.0e-10/ : smoothing parameter used in PF coil
  !!                    current calculation at the beginning of pulse (BoP)

  real(kind(1.0D0)) :: alstroh = 4.0D8
  !! alstroh /4.0D8/ : allowable hoop stress in Central Solenoid structural material (Pa)

  integer :: i_cs_stress = 0
  !! i_cs_stress /0/ : Switch for CS stress calculation:<UL>
  !!                  <LI> = 0 Hoop stress only;
  !!                  <LI> = 1 Hoop + Axial stress</UL>

  real(kind(1.0D0)) :: areaoh = 0.0D0
  !! areaoh : central solenoid cross-sectional area (m2)
  real(kind(1.0D0)) :: awpoh = 0.0D0
  !! awpoh : central solenoid conductor+void area (m2)
  real(kind(1.0D0)) :: bmaxoh = 0.0D0
  !! bmaxoh : maximum field in central solenoid at end of flat-top (EoF) (T)
  real(kind(1.0D0)) :: bmaxoh0 = 0.0D0
  !! bmaxoh0 : maximum field in central solenoid at beginning of pulse (T)
  real(kind(1.0D0)), dimension(ngc2) :: bpf = 0.0D0
  !! bpf(ngc2) : peak field at coil i (T)
  real(kind(1.0D0)) :: cohbop = 0.0D0
  !! cohbop : central solenoid overall current density at beginning of pulse (A/m2)
  real(kind(1.0D0)) :: coheof = 1.85D7
  !! coheof /1.85e7/ : central solenoid overall current density at end of flat-top (A/m2)
  !!                   (iteration variable 37)
  real(kind(1.0D0)), dimension(ngc2,6) :: cpt = 0.0D0
  !! cpt(ngc2,6) : current per turn in coil i at time j (A)
  real(kind(1.0D0)), dimension(ngc2) :: cptdin = 4.0D4
  !! cptdin(ngc2) /4.0e4/: peak current per turn input for PF coil i (A)
  real(kind(1.0D0)), dimension(ngc2) :: curpfb = 0.0D0
  !! curpfb(ngc2) : work array
  real(kind(1.0D0)), dimension(ngc2) :: curpff = 0.0D0
  !! curpff(ngc2) : work array
  real(kind(1.0D0)), dimension(ngc2) :: curpfs = 0.0D0
  !! curpfs(ngc2) : work array
  real(kind(1.0D0)) :: etapsu = 0.9D0
  !! etapsu /0.9/ : Efficiency of transfer of PF stored energy into or out of storage.
  real(kind(1.0D0)) :: fcohbof = 0.0D0
  !! fcohbof : ratio of central solenoid overall current density at
  !!           beginning of flat-top / end of flat-top
  real(kind(1.0D0)) :: fcohbop = 0.9D0
  !! fcohbop /0.9/ : ratio of central solenoid overall current density at
  !!                 beginning of pulse / end of flat-top
  !!                 (iteration variable 41)
  real(kind(1.0D0)) :: fcuohsu = 0.7D0
  !! fcuohsu /0.7/ : copper fraction of strand in central solenoid
  real(kind(1.0D0)) :: fcupfsu = 0.69D0
  !! fcupfsu /0.69/ : copper fraction of cable conductor (PF coils)
  real(kind(1.0D0)) :: fvssu = 1.0
  !! fvssu   /1.0/  : F-value for constraint equation 51 
  integer, dimension(ngc) :: ipfloc = (/2,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  !! ipfloc(ngc) /2,2,3/ : switch for locating scheme of PF coil group i:<UL>
  !!                  <LI> = 1 PF coil on top of central solenoid;
  !!                  <LI> = 2 PF coil on top of TF coil;
  !!                  <LI> = 3 PF coil outside of TF coil</UL>
  integer :: ipfres = 0
  !! ipfres /0/ : switch for PF coil type:<UL>
  !!         <LI> = 0 superconducting PF coils;
  !!         <LI> = 1 resistive PF coils</UL>
  real(kind(1.0D0)) :: itr_sum = 0.0D0
  !! itr_sum : total sum of I x turns x radius for all PF coils and CS (Am)
  integer :: isumatoh = 1
  !! isumatoh /1/ : switch for superconductor material in central solenoid:<UL>
  !!           <LI> = 1 ITER Nb3Sn critical surface model with standard
  !!                    ITER parameters;
  !!           <LI> = 2 Bi-2212 high temperature superconductor (range of
  !!                    validity T < 20K, adjusted field b < 104 T, B > 6 T);
  !!           <LI> = 3 NbTi;
  !!           <LI> = 4 ITER Nb3Sn model with user-specified parameters
  !!           <LI> = 5 WST Nb3Sn parameterisation
  !!           <LI> = 6 REBCO HTS parameterisation</UL>

  !! isumatpf /1/ : switch for superconductor material in PF coils:<UL>
  !!           <LI> = 1 ITER Nb3Sn critical surface model with standard
  !!                    ITER parameters;
  !!           <LI> = 2 Bi-2212 high temperature superconductor (range of
  !!                    validity T < 20K, adjusted field b < 104 T, B > 6 T);
  !!           <LI> = 3 NbTi;
  !!           <LI> = 4 ITER Nb3Sn model with user-specified parameters
  !!           <LI> = 5 WST Nb3Sn parameterisation</UL>
  integer :: isumatpf = 1
  real(kind(1.0D0)) :: jscoh_bop = 0.0D0
  !! jscoh_bop : central solenoid superconductor critical current density (A/m2)
  !!                 at beginning-of-pulse
  real(kind(1.0D0)) :: jscoh_eof = 0.0D0
  !! jscoh_eof : central solenoid superconductor critical current density (A/m2)
  !!                 at end-of-flattop
  real(kind(1.0D0)) :: jstrandoh_bop = 0.0D0
  !! jstrandoh_bop : central solenoid strand critical current density (A/m2)
  !!                 at beginning-of-pulse
  real(kind(1.0D0)) :: jstrandoh_eof = 0.0D0
  !! jstrandoh_eof : central solenoid strand critical current density (A/m2)
  !!                 at end-of-flattop
  integer :: ncirt = 0
  !! ncirt : number of PF circuits (including central solenoid and plasma)
  integer, dimension(ngrpmx+2) :: ncls = (/1,1,2,0,0,0,0,0,0,0/)
  !! ncls(ngrpmx+2) /1,1,2/ : number of PF coils in group j
  integer :: nfxfh = 7
  !! nfxfh /7/ : number of filaments the top and bottom of the central solenoid
  !!             should be broken into during scaling (5 - 10 is good)
  integer :: ngrp = 3
  !! ngrp /3/ : number of groups of PF coils.
  !!            Symmetric coil pairs should all be in the same group
  integer :: nohc = 0
  !! nohc : number of PF coils (excluding the central solenoid) + 1
  real(kind(1.0D0)) :: ohhghf = 0.71D0
  !! ohhghf /0.71/ : central solenoid height / TF coil internal height
  real(kind(1.0D0)) :: oh_steel_frac = 0.5D0
  !! oh_steel_frac /0.5/ : central solenoid steel fraction (iteration variable 122)
  real(kind(1.0D0)), dimension(ngc2) :: pfcaseth = 0.0D0
  !! pfcaseth(ngc2) : steel case thickness for PF coil i (m)
  real(kind(1.0D0)) :: pfclres = 2.5D-8
  !! pfclres /2.5e-8/ : PF coil resistivity (if ipfres=1) (Ohm-m)
  real(kind(1.0D0)) :: pfmmax = 0.0D0
  !! pfmmax : mass of heaviest PF coil (tonnes)
  real(kind(1.0D0)) :: pfrmax = 0.0D0
  !! pfrmax : radius of largest PF coil (m)
  !  real(kind(1.0D0)) :: pfsec = 0.0D0
  ! !! pfsec : PF Coil waste heat (MW)
  real(kind(1.0D0)) :: pfwpmw = 0.0D0
  !! pfwpmw : Total mean wall plug power dissipated in PFC and CS power supplies.  Issue #713 (MW)
  real(kind(1.0D0)) :: powohres = 0.0D0
  !! powohres : central solenoid resistive power during flattop (W)
  real(kind(1.0D0)) :: powpfres = 0.0D0
  !! powpfres : total PF coil resistive losses during flattop (W)
  real(kind(1.0D0)), dimension(ngc2) :: ra = 0.0D0
  !! ra(ngc2) : inner radius of coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: rb = 0.0D0
  !! rb(ngc2) : outer radius of coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: ric = 0.0D0
  !! ric(ngc2) : peak current in coil i (MA-turns)
  real(kind(1.0D0)), dimension(ngc2) :: rjconpf = 3.0D7
  !! rjconpf(ngc2) /3.0e7/ : average winding pack current density of PF coil i (A/m2)
  !!                         at time of peak current in that coil
  !!                         (calculated for ipfloc=1 coils)
  real(kind(1.0D0)) :: rjohc = 0.0D0
  !! rjohc : allowable central solenoid current density at end of flat-top (A/m2)
  real(kind(1.0D0)) :: rjohc0 = 0.0D0
  !! rjohc0 : allowable central solenoid current density at beginning of pulse (A/m2)
  real(kind(1.0D0)), dimension(ngc2) :: rjpfalw = 0.0D0
  !! rjpfalw(ngc2) : allowable winding pack current density of PF coil i (A/m2)
  real(kind(1.0D0)) :: rohc = 0.0D0
  !! rohc : radius to the centre of the central solenoid (m)
  real(kind(1.0D0)) :: routr = 1.5D0
  !! routr /1.5/ : radial distance (m) from outboard TF coil leg to centre of
  !!               ipfloc=3 PF coils
  real(kind(1.0D0)), dimension(ngc2) :: rpf = 0.0D0
  !! rpf(ngc2) : radius of PF coil i (m)
  real(kind(1.0D0)) :: rpf1 = 0.0D0
  !! rpf1 /0.0/ : offset (m) of radial position of ipfloc=1 PF coils
  !!              from being directly above the central solenoid
  real(kind(1.0D0)) :: rpf2 = -1.63D0
  !! rpf2 /-1.63/ : offset (m) of radial position of ipfloc=2 PF coils
  !!                from being at rmajor (offset = rpf2*triang*rminor)
  real(kind(1.0D0)) :: s_tresca_oh = 0.0D0
  !! s_tresca_oh : Tresca stress coils/central solenoid [MPa]
  real(kind(1.0D0)) :: sigpfcalw = 500.0D0
  !! sigpfcalw /500.0/ : maximum permissible tensile stress (MPa) in
  !!                     steel coil cases for superconducting PF coils
  !!                     (ipfres=0)
  real(kind(1.0D0)) :: sigpfcf = 0.666D0
  !! sigpfcf /0.666/ : fraction of JxB hoop force supported by steel case
  !!                   for superconducting PF coils (ipfres=0)
  real(kind(1.0D0)), dimension(ngc2,ngc2) :: sxlg = 0.0D0
  !! sxlg(ngc2,ngc2) : mutual inductance matrix (H)
  real(kind(1.0D0)) :: tmargoh = 0.0D0
  !! tmargoh :  Central solenoid temperature margin (K)
  real(kind(1.0D0)), dimension(ngc2) :: turns = 0.0D0
  !! turns(ngc2) : number of turns in PF coil i
  real(kind(1.0D0)), dimension(ngc2) :: vf = 0.3D0
  !! vf(ngc2) /0.3/ : winding pack void fraction of PF coil i for coolant
  real(kind(1.0D0)) :: vfohc = 0.3D0
  !! vfohc /0.3/ : void fraction of central solenoid conductor for coolant
  real(kind(1.0D0)) :: vsbn = 0.0D0
  !! vsbn : total flux swing available for burn (Wb)
  real(kind(1.0D0)) :: vsefbn = 0.0D0
  !! vsefbn : flux swing from PF coils for burn (Wb)
  real(kind(1.0D0)) :: vsefsu = 0.0D0
  !! vsefsu : flux swing from PF coils for startup (Wb)
  real(kind(1.0D0)) :: vseft = 0.0D0
  !! vseft : total flux swing from PF coils (Wb)
  real(kind(1.0D0)) :: vsoh = 0.0D0
  !! vsoh : total flux swing from the central solenoid (Wb)
  real(kind(1.0D0)) :: vsohbn = 0.0D0
  !! vsohbn : central solenoid flux swing for burn (Wb)
  real(kind(1.0D0)) :: vsohsu = 0.0D0
  !! vsohsu : central solenoid flux swing for startup (Wb)
  real(kind(1.0D0)) :: vssu = 0.0D0
  !! vssu : total flux swing for startup (eqn 51 to enforce vssu=vsres+vsind) (Wb)
  real(kind(1.0D0)) :: vstot = 0.0D0
  !! vstot : total flux swing for pulse (Wb)
  real(kind(1.0D0)), dimension(ngc2,6) :: waves = 0.0D0
  !! waves(ngc2, 6) : used in current waveform of PF coils/central solenoid
  real(kind(1.0D0)) :: whtpf = 0.0D0
  !! whtpf : total mass of the PF coil conductor (kg)
  real(kind(1.0D0)) :: whtpfs = 0.0D0
  !! whtpfs : total mass of the PF coil structure (kg)
  real(kind(1.0D0)), dimension(ngc2) :: wtc = 0.0D0
  !! wtc(ngc2) : conductor mass for PF coil i (kg)
  real(kind(1.0D0)), dimension(ngc2) :: wts = 0.0D0
  !! wts(ngc2) : structure mass for PF coil i (kg)
  real(kind(1.0D0)), dimension(ngc2) :: zh = 0.0D0
  !! zh(ngc2) : upper point of PF coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: zl = 0.0D0
  !! zl(ngc2) : lower point of PF coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: zpf = 0.0D0
  !! zpf(ngc2) : z (height) location of PF coil i (m)
  real(kind(1.0D0)), dimension(ngrpmx) :: zref = (/3.6D0, 1.2D0, 2.5D0, &
  !! zref(ngrpmx) /../ : PF coil vertical positioning adjuster:<UL>
  !!       <LI> - for groups j with ipfloc(j) = 1; zref(j) is ignored
  !!       <LI> - for groups j with ipfloc(j) = 2 AND itart=1 (only);
  !!              zref(j) is distance of centre of PF coil from inside
  !!              edge of TF coil (remember that PF coils for STs lie
  !!              within the TF coil)
  !!       <LI> - for groups j with ipfloc(j) = 3; zref(j) = ratio of
  !!              height of coil group j to plasma minor radius</UL>
       1.0D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0/)

  real(kind(1.0D0)) :: bmaxcs_lim = 13.0
  !! bmaxcs_lim : Central solenoid max field limit [T]
  real(kind(1.0D0)) :: fbmaxcs = 13.0
  !! fbmaxcs : F-value for CS mmax field (cons. 79, itvar 149)


end module pfcoil_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module tfcoil_variables

  !! Module containing global variables relating to the
  !! toroidal field coil systems
  !! This module contains global variables relating to the
  !! toroidal field coil systems of a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !! ITER Magnets design description document DDD11-2 v2 2 (2009)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: acasetf = 0.0D0
  !! acasetf : external case area per coil (inboard leg) (m2)
  real(kind(1.0D0)) :: acasetfo = 0.0D0
  !! acasetfo : external case area per coil (outboard leg) (m2)
  real(kind(1.0D0)) :: acndttf = 0.0D0
  !! acndttf : area of the cable conduit (m2)
  real(kind(1.0D0)) :: acond = 0.0D0
  !! acond : conductor area (winding pack) (m2)
  real(kind(1.0D0)) :: acstf = 0.0D0
  !! acstf : internal area of the cable space (m2)
  real(kind(1.0D0)) :: insulation_area = 0.0D0
  !! insulation_area : single turn insulation area (m2)
  real(kind(1.0D0)) :: aiwp = 0.0D0
  !! aiwp : winding pack insulation area (m2)
  real(kind(1.0D0)) :: alstrtf = 6.0D8
  !! alstrtf /6.0D8/ : allowable Tresca stress in TF coil structural material (Pa)

  real(kind(1.0D0)) :: arealeg = 0.0D0
  !! arealeg : outboard TF leg area (m2)
  real(kind(1.0D0)) :: aswp = 0.0D0
  !! aswp : winding pack structure area (m2)
  real(kind(1.0D0)) :: avwp = 0.0D0
  !! avwp : winding pack void (He coolant) area (m2)
  real(kind(1.0D0)) :: awphec = 0.0D0
  !! awphec : winding pack He coil area (m2)
  real(kind(1.0D0)) :: bcritsc = 24.0D0
  !! bcritsc /24.0/ : upper critical field (T) for Nb3Sn superconductor
  !!                  at zero temperature and strain (isumattf=4, =bc20m)
  real(kind(1.0D0)) :: bmaxtf = 0.0D0
  !! bmaxtf : mean peak field at TF coil (T)
  real(kind(1.0D0)) :: bmaxtfrp = 0.0D0
  !! bmaxtfrp : peak field at TF conductor with ripple (T)
  real(kind(1.0D0)) :: casestr = 0.0D0
  !! casestr : case strain

  real(kind(1.0D0)) :: casthi = 0.0D0
  !! casthi /0.0/ : EITHER: inboard TF coil case plasma side thickness (m)
  !!                 (calculated for stellarators)
  real(kind(1.0D0)) :: casthi_fraction = 0.05D0
  !! casthi_fraction /0.05/ : OR: inboard TF coil case plasma side thickness as a fraction of tfcth
  logical :: casthi_is_fraction

  real(kind(1.0D0)) :: casths = 0.0D0
  !! casths /0.0/ : EITHER: inboard TF coil sidewall case thickness (m)
  !!                 (calculated for stellarators)
  real(kind(1.0D0)) :: casths_fraction = 0.03D0
  !! casths_fraction /0.03/ : OR: inboard TF coil sidewall case thickness as a fraction of tftort
  logical :: tfc_sidewall_is_fraction

  real(kind(1.0D0)) :: conductor_width
  !! conductor_width : Width of square conductor (m)
  real(kind(1.0D0)) :: leno
  !! leno : Dimension of each turn including inter-turn insulation (m)


  real(kind(1.0D0)) :: leni
  !! leni : Dimension of space inside conductor (m)
  real(kind(1.0D0)) :: acs
  !! acs : Area of space inside conductor (m2)

  real(kind(1.0D0)) :: cdtfleg = 0.0D0
  !! cdtfleg : TF outboard leg current density (A/m2) (resistive coils only)
  real(kind(1.0D0)) :: cforce = 0.0D0
  !! cforce : centering force on inboard leg (per coil) (N/m)
  real(kind(1.0D0)), parameter :: cph2o = 4180.0D0
  !! cph2o /4180.0/ FIX : specific heat capacity of water (J/kg/K)

  real(kind(1.0D0)) :: cpttf = 7.0e4
  !! cpttf /7.0e4/ : TF coil current per turn (A).
  !!                 (calculated for stellarators)
  !!                 (calculated for integer-turn TF coils i_tf_turns_integer=1)
  !!                 (iteration variable 60)

  real(kind(1.0D0)) :: cpttf_max = 9.0e4
  !! cpttf_max /9.0e4/ : Max TF coil current per turn [A].
  !!                 (For stellarators and i_tf_turns_integer=1)
  !!                 (constraint equation 77)

  real(kind(1.0D0)) :: dcase = 8000.0D0
  !! dcase /8000.0/ : density of coil case (kg/m3)
  real(kind(1.0D0)), dimension(6) :: dcond = 9000.0D0
  !! dcond(6) /9000.0/ : density of superconductor type given by isumattf/isumatoh/isumatpf (kg/m3)
  real(kind(1.0D0)) :: dcondins = 1800.0D0
  !! dcondins /1800.0/ : density of conduit + ground-wall insulation (kg/m3)

  real(kind(1.0D0)) :: dcopper = 8900.0D0
  !! dcopper /8900.0/ : density of copper (kg/m3)

  real(kind(1.0D0)) :: dalu = 2700.0D0
  !! dalu /2700.0/ : density of aluminium (kg/m3)
  real(kind(1.0D0)) :: deflect = 0.0D0
  !! deflect : TF coil deflection at full field (m)
  real(kind(1.0D0)), parameter :: denh2o = 985.0D0
  !! denh2o /985.0/ FIX : density of water (kg/m3)
  real(kind(1.0D0)) :: dhecoil = 0.005D0
  !! dhecoil /0.005/ : diameter of He coil in TF winding (m)

  real(kind(1.0D0)) :: estotftgj = 0.0D0
  !! estotftgj : total stored energy in the toroidal field (GJ)

  real(kind(1.0D0)) :: eyins = 2.0D10
  !! eyins /2.0e10/ : insulator Young's modulus (Pa)
  !!                  (default value from DDD11-2 v2 2 (2009))
  real(kind(1.0D0)), dimension(2) :: eyoung = 0.0D0
  !! eyoung(2) : work array used in stress calculation (Pa)
  real(kind(1.0D0)) :: eystl = 2.05D11
  !! eystl /2.05e11/ : steel case Young's modulus (Pa)
  !!                   (default value from DDD11-2 v2 2 (2009))
  real(kind(1.0D0)) :: eywp = 6.6D8
  !! eywp /6.6e8/ : winding pack Young's modulus (Pa)
  real(kind(1.0D0)) :: eyzwp = 0.0D0
  !! eyzwp : winding pack vertical Young's modulus (Pa)
  real(kind(1.0D0)) :: farc4tf = 0.7D0
  !! farc4tf /0.7/ : factor to size height of point 4 on TF coil
  real(kind(1.0D0)) :: fcutfsu = 0.69D0
  !! fcutfsu /0.69/ : copper fraction of cable conductor (TF coils)
  !!                  (iteration variable 59)
  real(kind(1.0D0)) :: fhts = 0.5D0
  !! fhts /0.5/ : technology adjustment factor for critical current density fit
  !!              for isumat..=2 Bi-2212 superconductor, to describe the level
  !!              of technology assumed (i.e. to account for stress, fatigue,
  !!              radiation, AC losses, joints or manufacturing variations;
  !!              1.0 would be very optimistic)
  real(kind(1.0D0)) :: insstrain = 0.0D0
  !! insstrain : radial strain in insulator
  integer :: i_tf_tresca = 0
  !! i_tf_tresca /0/ : switch for TF coil conduit Tresca stress criterion:<UL>
  !!         <LI> = 0 Tresca (no adjustment);
  !!         <LI> = 1 Tresca with CEA adjustment factors (radial+2%, vertical+60%) </UL>
  integer :: i_tf_turns_integer = 0
  !! i_tf_turns_integer /0/ : switch for TF coil integer/non-integer turns<UL>
  !!         <LI> = 0 non-integer turns;
  !!         <LI> = 1 integer turns</UL>
  integer :: isumattf = 1
  !! isumattf /1/ : switch for superconductor material in TF coils:<UL>
  !!           <LI> = 1 ITER Nb3Sn critical surface model with standard
  !!                    ITER parameters;
  !!           <LI> = 2 Bi-2212 high temperature superconductor (range of
  !!                    validity T < 20K, adjusted field b < 104 T, B > 6 T);
  !!           <LI> = 3 NbTi;
  !!           <LI> = 4 ITER Nb3Sn model with user-specified parameters
  !!           <LI> = 5 WST Nb3Sn parameterisation
  !!           <LI> = 6 REBCO HTS tape in CroCo strand</UL>

  integer :: i_tf_sup = 1
  !! i_tf_sup /1/ : switch for TF coil conductor model:<UL>
  !!         <LI> = 0 copper;
  !!         <LI> = 1 superconductor</UL>

  real(kind(1.0D0)) :: jbus = 1.25D6
  !! jbus /1.25e6/ : bussing current density (A/m2)
  real(kind(1.0D0)), dimension(2) :: jeff = 0.0D0
  !! jeff(2) : work array used in stress calculation (A/m2)
  real(kind(1.0D0)) :: jwdgcrt = 0.0D0
  !! jwdgcrt : critical current density for winding pack (A/m2)
  real(kind(1.0D0)) :: jwdgpro = 0.0D0
  !! jwdgpro : allowable TF coil winding pack current density,
  !!           for dump temperature rise protection (A/m2)
  real(kind(1.0D0)) :: jwptf = 0.0D0
  !! jwptf : winding pack current density (A/m2)

  integer :: n_pancake = 10
  !! n_pancake /10/ : Number of pancakes in TF coil (i_tf_turns_integer=1)

  integer :: n_layer = 20
  !! n_layer /20/ : Number of layers in TF coil (i_tf_turns_integer=1)

  real(kind(1.0D0)) :: oacdcp = 1.4D7
  !! oacdcp /1.4e7/ : overall current density in TF coil inboard legs midplane (A/m2)
  !!                  Rem SK : Not used in tfcoil to set the current any more
  !!                            -> SHOULD NOT BE USED AS ITERATION VARIABLE 12 ANY MORE
  !!                            -> This variable is calculated

  real(kind(1.0D0)) :: poisson = 0.3D0
  !! poisson /0.3/ : Poisson's ratio for TF stress calculation
  !!                 (assumed constant over entire coil)
  real(kind(1.0D0)), dimension(3) :: radtf = 0.0D0
  !! radtf(3) : work array used in stress calculation (m)
  real(kind(1.0D0)) :: rbmax = 0.0D0
  !! rbmax : radius of maximum TF B-field (m)
  real(kind(1.0D0)) :: tflegres = 0.0D0
  !! tflegres : TF coil leg resistance (ohm)
  real(kind(1.0D0)) :: ripmax = 1.0D0
  !! ripmax /1.0/ : maximum allowable toroidal field ripple amplitude
  !!                at plasma edge (%)
  real(kind(1.0D0)) :: ripple = 0.0D0
  !! ripple : peak/average toroidal field ripple at plasma edge (%)
  real(kind(1.0D0)) :: ritfc = 0.0D0
  !! ritfc : total (summed) current in TF coils (A)
  real(kind(1.0D0)) :: sigrad = 0.0D0
  !! sigrad : radial TF coil stress (MPa)
  real(kind(1.0D0)) :: sigrcon = 0.0D0
  !! sigrcon : radial stress in the conductor conduit (Pa)
  real(kind(1.0D0)), dimension(2) :: sigrtf = 0.0D0
  !! sigrtf(2) : radial stress in TF coil regions (Pa)
  real(kind(1.0D0)) :: sigtan = 0.0D0
  !! sigtan : transverse TF coil stress (MPa)
  real(kind(1.0D0)) :: sigtcon = 0.0D0
  !! sigtcon : tangential stress in the conductor conduit (Pa)
  real(kind(1.0D0)), dimension(2) :: sigttf = 0.0D0
  !! sigttf(2) : tangential stress in TF coil regions (Pa)
  real(kind(1.0D0)) :: s_tresca_case  = 0.0D0
  !! s_tresca_case : TF coil case Tresca stress (MPa)
  real(kind(1.0D0)) :: s_tresca_cond  = 0.0D0
  !! s_tresca_cond : TF coil conduit Tresca stress (MPa)
  real(kind(1.0D0)) :: s_vmises_case  = 0.0D0
  !! s_vmises_case : TF coil case von Mises stress (MPa)
  real(kind(1.0D0)) :: s_vmises_cond  = 0.0D0
  !! s_vmises_cond : TF coil conduit von Mises stress (MPa)
  real(kind(1.0D0)) :: sigver  = 0.0D0
  !! sigver : vertical TF coil stress (MPa)
  real(kind(1.0D0)) :: sigvert = 0.0D0
  !! sigvert : vertical tensile stress in TF coil (Pa)
  real(kind(1.0D0)) :: sigvvall = 9.3D7
  !! sigvvall /9.3e7/ : allowable stress from TF quench in vacuum vessel (Pa)
  real(kind(1.0D0)) :: strncon_cs = -0.005D0
  !! strncon_cs /-0.005/ : strain in CS superconductor material
  !!                    (used in Nb3Sn critical surface model, isumatoh=1, 4 or 5)
  real(kind(1.0D0)) :: strncon_pf = -0.005D0
  !! strncon_pf /-0.005/ : strain in PF superconductor material
  !!                    (used in Nb3Sn critical surface model, isumatph=1, 4 or 5)
  real(kind(1.0D0)) :: strncon_tf = -0.005D0
  !! strncon_tf /-0.005/ : strain in TF superconductor material
  !!                    (used in Nb3Sn critical surface model, isumattf=1, 4 or 5)
  real(kind(1.0D0)) :: strtf1 = 0.0D0
  !! strtf1 : Constrained stress in TF conductor conduit (Pa)
  real(kind(1.0D0)) :: strtf2 = 0.0D0
  !! strtf2 : Constrained stress in TF coil case (Pa)

  ! Issue #522: Quench models
  character(len=12) :: quench_model = 'exponential'
  !! quench_model /exponential/ : switch for TF coil quench model,
  !!                   Only applies to REBCO magnet at present.<UL>
  !!                 <LI> = 'exponential' exponential quench with constant discharge resistor
  !!                 <LI> = 'linear' quench with constant voltage</UL>

  real(kind(1.0D0)) :: quench_detection_ef = 0D0
  !! quench_detection_ef /0.0/ : Electric field at which TF quench is detected and discharge begins (V/m)
  real(kind(1.0D0)) :: time1 = 0D0
  !! time1 : Time at which TF quench is detected (s)

  real(kind(1.0D0)) :: taucq = 30.0D0
  !! taucq : allowable TF quench time (s)
  real(kind(1.0D0)) :: tcritsc = 16.0D0
  !! tcritsc /16.0/ : critical temperature (K) for superconductor
  !!                  at zero field and strain (isumattf=4, =tc0m)
  real(kind(1.0D0)) :: tdmptf = 10.0D0
  !! tdmptf /10.0/ : fast discharge time for TF coil in event of quench (s)
  !!                 (iteration variable 56)
  !!                 For REBCO model, meaning depends on quench_model:
  !!                 <LI> exponential quench : e-folding time (s)
  !!                 <LI> linear quench : discharge time (s)

  real(kind(1.0D0)) :: tfareain = 0.0D0
  !! tfareain : area of inboard midplane TF legs (m2)

  real(kind(1.0D0)) :: tfbusl = 0.0D0
  !! tfbusl : TF coil bus length (m)

  real(kind(1.0D0)) :: tfbusmas = 0.0D0
  !! tfbusmas : TF coil bus mass (kg)

  real(kind(1.0D0)) :: tfckw = 0.0D0
  !! tfckw :  available DC power for charging the TF coils (kW)
! Issue #781
!   integer :: tfc_model = 1
!   !! tfc_model /1/ : switch for TF coil magnet stress model:<UL>
!   !!                 <LI> = 0 simple model (solid copper coil)
!   !!                 <LI> = 1 CCFE two-layer stress model; superconductor</UL>
  real(kind(1.0D0)), bind(C) :: tfcmw = 0.0D0
  !! tfcmw : peak power per TF power supply (MW)
  real(kind(1.0D0)) :: tfcpmw = 0.0D0
  !! tfcpmw : peak resistive TF coil inboard leg power (MW)
  real(kind(1.0D0)) :: tfcryoarea = 0.0D0
  !! tfcryoarea : surface area of toroidal shells covering TF coils (m2)
  real(kind(1.0D0)) :: tficrn = 0.0D0
  !! tficrn : TF coil half-width - inner bore (m)
  real(kind(1.0D0)) :: tfind = 0.0D0
  !! tfind : TF coil inductance (H)
  real(kind(1.0D0)) :: tfinsgap = 0.010D0
  !! tfinsgap /0.010/ : TF coil WP insertion gap (m)
  real(kind(1.0D0)) :: tflegmw = 0.0D0
  !! tflegmw : TF coil outboard leg resistive power (MW)
  real(kind(1.0D0)) :: rhotfleg = -1.0D0 ! 2.5D-8
  !! rhotfleg /2.5e-8/ : resistivity of a TF coil leg and bus(Ohm-m)
  real(kind(1.0D0)) :: tfleng = 0.0D0
  !! tfleng : TF coil circumference (m)

  real(kind(1.0D0)) :: n_tf = 16.0D0
  !! n_tf /16.0/ : number of TF coils (default = 50 for stellarators)
  !!               number of TF coils outer legs for ST

  real(kind(1.0D0)) :: tfocrn = 0.0D0
  !! tfocrn : TF coil half-width - outer bore (m)

  real(kind(1.0D0)) :: tfsai = 0.0D0
  !! tfsai : area of the inboard TF coil legs (m2)

  real(kind(1.0D0)) :: tfsao = 0.0D0
  !! tfsao : area of the outboard TF coil legs (m2)

  real(kind(1.0D0)), bind(C) :: tftmp = 4.5D0
  !! tftmp /4.5/ : peak helium coolant temperature in TF coils and PF coils (K)
  ! ISSUE #508 Remove RFP option: frfpf, frfptf, sccufac
! SJP Issue #863
!! tftort : TF coil toroidal thickness (m)
! tftort physically to large, reduced to 1.0
  real(kind(1.0D0)) :: tftort = 1.0D0

  real(kind(1.0D0)) :: thicndut = 8.0D-4
  !! thicndut /8.0e-4/ : conduit insulation thickness (m)
  real(kind(1.0D0)) :: layer_ins = 0.0D0
  !! layer_ins /0/ : Additional insulation thickness between layers (m)
  real(kind(1.0D0)) :: thkcas = 0.3D0
  !! thkcas /0.3/ : inboard TF coil case outer (non-plasma side) thickness (m)
  !!                (iteration variable 57)
  !!                (calculated for stellarators)
  ! Issue #514 Make thkwp an iteration variable
  real(kind(1.0D0)) :: thkwp = 0.0D0
  !! thkwp /0.0/ : radial thickness of winding pack (m) (iteration variable 140)
  real(kind(1.0D0)) :: thwcndut = 8.0D-3
  !! thwcndut /8.0e-3/ : TF coil conduit case thickness (m) (iteration variable 58)
  real(kind(1.0D0)) :: tinstf = 0.018D0
  !! tinstf /0.018/ : ground insulation thickness surrounding winding pack (m)
  !!                  Includes allowance for 10 mm insertion gap.
  !!                  (calculated for stellarators)

  real(kind(1.0D0)), bind(C) :: tmargmin_tf = 0D0
  !! tmargmin_tf /0/ : minimum allowable temperature margin : TF coils (K)
  real(kind(1.0D0)), bind(C) :: tmargmin_cs = 0D0
  !! tmargmin_cs /0/ : minimum allowable temperature margin : CS (K)
  real(kind(1.0D0)) :: tmargmin = 0D0
  !! tmargmin /0/ : minimum allowable temperature margin : TFC AND CS (K)

  real(kind(1.0D0)), bind(C) :: temp_margin = 0.00D0
  !! temp_margin  : temperature margin (K)
  real(kind(1.0D0)) :: tmargtf = 0.0D0
  !! tmargtf :  TF coil temperature margin (K)
  real(kind(1.0D0)) :: tmaxpro = 150.0D0
  !! tmaxpro /150.0/ : maximum temp rise during a quench for protection (K)

  real(kind(1.0D0)) :: tmax_croco = 200.0D0
  !! tmax_croco /200.0/ : CroCo strand: maximum permitted temp during a quench (K)
  ! real(kind(1.0D0)) :: tmax_jacket = 150.0D0
  ! !! tmax_jacket /150.0/ : Jacket: maximum temp during a quench (K)

  real(kind(1.0D0)) :: croco_quench_temperature = 0D0
  !! croco_quench_temperature : CroCo strand: Actual temp reached during a quench (K)

  real(kind(1.0D0)) :: tmpcry = 4.5D0
  !! tmpcry /4.5/ : coil temperature for cryogenic plant power calculation (K)
  real(kind(1.0D0)) :: turnstf = 0.0D0
  !! turnstf : number of turns per TF coil
  real(kind(1.0D0)) :: vdalw = 20.0D0
  !! vdalw /20.0/ : max voltage across TF coil during quench (kV)
  !!                (iteration variable 52)

  real(kind(1.0D0)) :: vforce = 0.0D0
  !! vforce : vertical separating force on inboard leg/coil (N)
  
  real(kind(1.0D0)) :: f_vforce_inboard = 0.58D0
  !! f_vforce_inboard /0.58/ : Fraction of the total vertical force taken by the TF inboard leg
  !!                           Not used for resistive itart = 1 (sliding joints)
  
  real(kind(1.0D0)) :: vftf = 0.4D0
  !! vftf /0.4/ : coolant fraction of TFC 'cable' (i_tf_sup=1), or of TFC leg (i_tf_ssup=0)
  real(kind(1.0D0)) :: voltfleg = 0.0D0
  !! voltfleg : volume of each TF coil outboard leg (m3)
  real(kind(1.0D0)) :: vtfkv = 0.0D0
  !! vtfkv : TF coil voltage for resistive coil including bus (kV)
  real(kind(1.0D0)) :: vtfskv = 0.0D0
  !! vtfskv : voltage across a TF coil during quench (kV)
  real(kind(1.0D0)) :: whtcas = 0.0D0
  !! whtcas : mass per coil of external case (kg)
  real(kind(1.0D0)) :: whtcon = 0.0D0
  !! whtcon : TF coil conductor mass per coil (kg)
  real(kind(1.0D0)) :: whtconcu = 0.0D0
  !! whtconcu : copper mass in TF coil conductor (kg/coil)
  real(kind(1.0D0)) :: whtconin = 0.0D0
  !! whtconin : conduit insulation mass in TF coil conductor (kg/coil)
  real(kind(1.0D0)) :: whtconsc = 0.0D0
  !! whtconsc : superconductor mass in TF coil cable (kg/coil)
  real(kind(1.0D0)) :: whtconsh = 0.0D0
  !! whtconsh : steel conduit mass in TF coil conductor (kg/coil)
  real(kind(1.0D0)) :: whtgw = 0.0D0
  !! whtgw : mass of ground-wall insulation layer per coil (kg/coil)
  real(kind(1.0D0)) :: whttf = 0.0D0
  !! whttf : total mass of the TF coils (kg)
  real(kind(1.0D0)) :: windstrain = 0.0D0
  !! windstrain : longitudinal strain in winding pack
  real(kind(1.0D0)) :: wwp1 = 0.0D0
  !! wwp1 : width of first step of winding pack (m)
  real(kind(1.0D0)) :: wwp2 = 0.0D0
  !! wwp2 : width of second step of winding pack (m)


  !! <P><B>Superconducting TF coil shape parameters</B> (see also farc4tf);
  !! <BR>the TF inner surface top half is approximated by four circular arcs.
  !! Arc 1 goes through points 1 and 2 on the inner surface. Arc 2
  !! goes through points 2 and 3, etc.<P>
  real(kind(1.0D0)), dimension(4) :: dthet = 0.0D0
  !! dthet(4) : angle of arc i (rad)
  real(kind(1.0D0)), dimension(4) :: radctf = 0.0D0
  !! radctf(4) : radius of arc i (m)
  real(kind(1.0D0)), dimension(5) :: xarc = 0.0D0
  !! xarc(5) : x location of arc point i on surface (m)
  real(kind(1.0D0)), dimension(4) :: xctfc = 0.0D0
  !! xctfc(4) : x location of arc centre i (m)
  real(kind(1.0D0)), dimension(5) :: yarc = 0.0D0
  !! yarc(5) : y location of arc point i on surface (m)
  real(kind(1.0D0)), dimension(4) :: yctfc = 0.0D0
  !! yctfc(4) : y location of arc centre i (m)

  ! New TF shape:  Horizontal and vertical radii of inside edge of TF coil
  ! Arcs are numbered clockwise:
  ! 1=upper inboard, 2=upper outboard, 3=lower ouboard, 4=lower inboard
  real(kind(1.0D0)), dimension(4) :: tfa = 0.0D0
  !! tfa(4) : Horizontal radius of inside edge of TF coil (m)
  real(kind(1.0D0)), dimension(4) :: tfb = 0.0D0
  !! tfb(4) : Vertical radius of inside edge of TF coil (m)
  !! <P><B>Quantities relating to the spherical tokamak model (itart=1)</B>
  !!       (and in some cases, also to resistive TF coils, i_tf_sup=0):<P>

  real(kind(1.0D0)) :: drtop = 0.0D0
  !! drtop /0.0/ : centrepost taper maximum radius adjustment (m)

  real(kind(1.0D0)) :: dztop = 0.0D0
  !! dztop /0.0/ : centrepost taper height adjustment (m)

  real(kind(1.0D0)) :: etapump = 0.8D0
  !! etapump /0.8/ : centrepost coolant pump efficiency
  real(kind(1.0D0)) :: fcoolcp = 0.3D0
  !! fcoolcp /0.3/ : coolant fraction of TF coil inboard legs
  !!                 (iteration variable 23)
  real(kind(1.0D0)) :: frhocp = 1.0D0
  !! frhocp /1.0/ : centrepost resistivity enhancement factor
  real(kind(1.0D0)), parameter :: k_copper = 330.0D0
  !! k_copper /330.0/ FIX : Copper thermal conductivity (W/m/K)
  real(kind(1.0D0)), parameter :: kh2o = 0.651D0
  !! kh2o /0.651/ FIX : thermal conductivity of water (W/m/K)
  real(kind(1.0D0)), parameter :: muh2o = 4.71D-4
  !! muh2o /4.71e-4/ FIX : water dynamic viscosity (kg/m/s)
  real(kind(1.0D0)) :: ncool = 0.0D0
  !! ncool : number of centrepost coolant tubes

  real(kind(1.0D0)) :: ppump = 0.0D0
  !! ppump : centrepost coolant pump power (W)

  real(kind(1.0D0)) :: prescp = 0.0D0
  !! prescp : resistive power in the centrepost (W)

  real(kind(1.0D0)) :: presleg = 0.0D0
  !! presleg : resistive power in the centrepost (W)

  real(kind(1.0D0)) :: ptempalw = 473.15D0   ! 200 C
  !! ptempalw /473.15/ : maximum peak centrepost temperature (K)
  !!                    (constraint equation 44)

  real(kind(1.0D0)) :: rcool = 0.005D0
  !! rcool /0.005/ : average radius of coolant channel (m)
  !!                 (iteration variable 69)

  real(kind(1.0D0)) :: rhocp = 0.0D0
  !! rhocp : TF coil inboard leg resistivity (Ohm-m)

  real(kind(1.0D0)) :: tcoolin = 313.15D0   ! 40 C
  !! tcoolin /313.15/ : centrepost coolant inlet temperature (K)

  real(kind(1.0D0)) :: dtiocool = 0.0D0
  !! dtiocool : inlet / outlet TF coil coolant temperature rise (K)  

  real(kind(1.0D0)) :: tcpav = 373.15D0     ! 100 C
  !! tcpav /373.15/ : Assumed temperature of centrepost called CP (K)
  !!                  Only used for resistive coils to compute the resisitive heating 
  !!                  Must be an iteration variable for ST (itart == 1) (iteration variable 20)

  real(kind(1.0D0)) :: tcpav2 = 0.0D0
  !! tcpav2 : Computed centrepost average temperature (K) (for consistency)

  real(kind(1.0D0)) :: tcpmax = 0.0D0
  !! tcpmax : peak centrepost temperature (K)
  
  real(kind(1.0D0)) :: vcool = 20.0D0
  !! vcool /20.0/ : max centrepost coolant flow speed at midplane (m/s)
  !!                (iteration variable 70)
  real(kind(1.0D0)) :: volcp = 0.0D0
  !! volcp : total volume of TF coil inboard legs (m3)
  real(kind(1.0D0)) :: whtcp = 0.0D0
  !! whtcp : mass of TF coil inboard legs (kg)
  real(kind(1.0D0)) :: whttflgs = 0.0D0
  !! whttflgs : mass of the TF coil legs (kg)

end module tfcoil_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module structure_variables

  !! Module containing global variables relating to the
  !! support structure
  !! This module contains global variables relating to the
  !! support structure of a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: aintmass = 0.0D0
  !! aintmass : intercoil structure mass (kg)
  real(kind(1.0D0)) :: clgsmass = 0.0D0
  !! clgsmass : gravity support structure for TF coil, PF coil
  !!            and intercoil support systems (kg)
  real(kind(1.0D0)) :: coldmass = 0.0D0
  !! coldmass : total mass of components at cryogenic temperatures (kg)
  real(kind(1.0D0)) :: fncmass = 0.0D0
  !! fncmass : PF coil outer support fence mass (kg)
  real(kind(1.0D0)) :: gsmass = 0.0D0
  !! gsmass : reactor core gravity support mass (kg)

end module structure_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module vacuum_variables

  !! Module containing global variables relating to the
  !! vacuum system
  !! This module contains global variables relating to the
  !! vacuum system of a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public
  character(len=6) :: vacuum_model = 'old'
  !! vacuum_model /old/ : switch for vacuum pumping model:<UL>
  !!        <LI> = 'old' for old detailed ETR model;
  !!        <LI> = 'simple' for simple steady-state model with comparison to ITER cryopumps</UL>
  real(kind(1.0D0)) :: niterpump = 0.0D0
  !! niterpump : number of high vacuum pumps (real number), each with the throughput
  !!             of one ITER cryopump (50 Pa m3 s-1), all operating at the same time
  !!             (vacuum_model = 'simple')
  integer :: ntype = 1
  !! ntype /1/ : switch for vacuum pump type:<UL>
  !!        <LI> = 0 for turbomolecular pump (magnetic bearing)
  !!                 with speed of 2.0 m3/s
  !!                 (1.95 for N2, 1.8 for He, 1.8 for DT);
  !!        <LI> = 1 for compound cryopump with nominal speed of 10.0 m3/s
  !!                 (9.0 for N2, 5.0 for He and 25.0 for DT)</UL>
  integer :: nvduct = 0
  !! nvduct : number of ducts (torus to pumps)
  real(kind(1.0D0)) :: dlscal = 0.0D0
  !! dlscal : vacuum system duct length scaling
  real(kind(1.0D0)) :: pbase = 5.0D-4
  !! pbase /5.0e-4/ : base pressure during dwell before gas pre-fill(Pa)
  real(kind(1.0D0)) :: prdiv = 0.36D0
  !! prdiv /0.36/ : divertor chamber pressure during burn (Pa)
  real(kind(1.0D0)) :: pumptp = 1.2155D22
  !! pumptp /1.2155D22/ : Pump throughput (molecules/s) (default is ITER value)
  real(kind(1.0D0)) :: rat = 1.3D-8
  !! rat /1.3e-8/ : plasma chamber wall outgassing rate (Pa-m/s)
  real(kind(1.0D0)) :: tn = 300.0D0
  !! tn /300.0/ : neutral gas temperature in chamber (K)
  real(kind(1.0D0)) :: vacdshm = 0.0D0
  !! vacdshm : mass of vacuum duct shield (kg)
  real(kind(1.0D0)) :: vcdimax = 0.0D0
  !! vcdimax : diameter of duct passage (m)
  integer :: vpumpn = 0
  !! vpumpn : number of high vacuum pumps
  integer :: dwell_pump = 0
  !! dwell_pump /0/ : switch for dwell pumping options:<UL>
  !!             <LI> = 0 pumping only during tdwell;
  !!             <LI> = 1 pumping only during tramp
  !!             <LI> = 2 pumping during tdwell + tramp</UL>

  real(kind(1.0D0)) :: pumpareafraction = 0.0203D0
  !! <P><B>The following are used in the Battes, Day and Rohde pump-down model
  !! See "Basic considerations on the pump-down time in the dwell phase of a pulsed fusion DEMO"
  !! http://dx.doi.org/10.1016/j.fusengdes.2015.07.011)
  !! (vacuum_model=simple'):</B><P>
  !! pumpareafraction /0.0203/ : area of one pumping port as a fraction of plasma surface area
  real(kind(1.0D0)) :: pumpspeedmax = 27.3D0
  !! pumpspeedmax /27.3/ : maximum pumping speed per unit area for deuterium & tritium, molecular flow
  real(kind(1.0D0)) :: pumpspeedfactor = 0.167D0
  !! pumpspeedfactor /0.167/ : effective pumping speed reduction factor due to duct impedance
  real(kind(1.0D0)) :: initialpressure = 1.0D0
  !! initialpressure /1.0/ : initial neutral pressure at the beginning of the dwell phase (Pa)
  ! (duplicate message)
  !! pbase /5.0e-4/ : base pressure during dwell before gas pre-fill (Pa)
  real(kind(1.0D0)) :: outgasindex = 1.0D0
  !! outgasindex /1.0/ : outgassing decay index
  real(kind(1.0D0)) :: outgasfactor = 0.0235D0
  !! outgasfactor /0.0235/ : outgassing prefactor kw: outgassing rate at 1 s per unit area (Pa m s-1)

end module vacuum_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pf_power_variables

  !! Module containing global variables relating to the
  !! PF coil power conversion system
  !! This module contains global variables relating to the
  !! PF coil power conversion system of a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: acptmax = 0.0D0
  !! acptmax : average of currents in PF circuits (kA)
  real(kind(1.0D0)) :: ensxpfm = 0.0D0
  !! ensxpfm : maximum stored energy in the PF circuits (MJ)
  integer :: iscenr = 2
  !! iscenr /2/ : Switch for PF coil energy storage option:<UL>
  !!         <LI> = 1 all power from MGF (motor-generator flywheel) units;
  !!         <LI> = 2 all pulsed power from line;
  !!         <LI> = 3 PF power from MGF, heating from line</UL>
  !!         (In fact, options 1 and 3 are not treated differently)
  real(kind(1.0D0)) :: pfckts = 0.0D0
  !! pfckts : number of PF coil circuits
  real(kind(1.0D0)) :: spfbusl = 0.0D0
  !! spfbusl : total PF coil circuit bus length (m)
  real(kind(1.0D0)) :: spsmva = 0.0D0
  !! spsmva : sum of PF power supply ratings (MVA)
  real(kind(1.0D0)) :: srcktpm = 0.0D0
  !! srcktpm : sum of resistive PF coil power (kW)
  real(kind(1.0D0)) :: vpfskv = 0.0D0
  !! vpfskv : PF coil voltage (kV)

  real(kind(1.0D0)) :: peakpoloidalpower = 0.0D0
  !! peakpoloidalpower : Peak absolute rate of change of stored energy in poloidal field (MW) (11/01/16)
  real(kind(1.0D0)) :: maxpoloidalpower = 1000.0D0
  !! maxpoloidalpower /1000/ : Maximum permitted absolute rate of change of stored energy in poloidal field (MW)
  real(kind(1.0D0)), dimension(5) :: poloidalpower = 0.0D0
  !! poloidalpower : Poloidal power usage at time t (MW)


end module pf_power_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module heat_transport_variables

  !! Module containing global variables relating to the
  !! heat transport system
  !! This module contains global variables relating to the
  !! heat transport system of a fusion power plant, and
  !! also those for a hydrogen production plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: baseel = 5.0D6
  !! baseel /5.0e6/ : base plant electric load (W)
  real(kind(1.0D0)), bind(C) :: crypmw = 0.0D0
  !! crypmw : cryogenic plant power (MW)

  real(kind(1.0D0)) :: etatf = 0.9D0
  !! etatf /0.9/ : AC to resistive power conversion for TF coils
  real(kind(1.0D0)) :: etath = 0.35D0
  !! etath /0.35/ : thermal to electric conversion efficiency
  !!                if secondary_cycle=2; otherwise calculated
  real(kind(1.0D0)), bind(C) :: fachtmw = 0.0D0
  !! fachtmw : facility heat removal (MW)
  real(kind(1.0D0)), bind(C) :: fcsht = 0.0D0
  !! fcsht : total baseline power required at all times (MW)
  real(kind(1.0D0)) :: fgrosbop = 0.0D0
  !! fgrosbop : scaled fraction of gross power to balance-of-plant
  real(kind(1.0D0)) :: fmgdmw = 0.0D0
  !! fmgdmw /0.0/ : power to mgf (motor-generator flywheel) units (MW)
  !!                (ignored if iscenr=2)
  real(kind(1.0D0)) :: fpumpblkt = 0.005D0
  !! fpumpblkt /0.005/ : fraction of total blanket thermal power required
  !!                     to drive the blanket coolant pumps (default assumes
  !!                     water coolant) (secondary_cycle=0)
  real(kind(1.0D0)) :: fpumpdiv = 0.005D0
  !! fpumpdiv /0.005/ : fraction of total divertor thermal power required
  !!                    to drive the divertor coolant pumps (default assumes
  !!                    water coolant)
  real(kind(1.0D0)) :: fpumpfw = 0.005D0
  !! fpumpfw /0.005/ : fraction of total first wall thermal power required
  !!                   to drive the FW coolant pumps (default assumes water
  !!                   coolant) (secondary_cycle=0)
  real(kind(1.0D0)) :: fpumpshld = 0.005D0
  !! fpumpshld /0.005/ : fraction of total shield thermal power required
  !!                     to drive the shield coolant pumps (default assumes
  !!                     water coolant)
  real(kind(1.0D0)) :: htpmw_min = 0.0D0
  !! htpmw_min /0.0/ : Minimum total electrical power for primary coolant pumps (MW) NOT RECOMMENDED

  real(kind(1.0D0)), bind(C) :: helpow = 0.0D0
  !! helpow : heat removal at cryogenic temperatures (W)
  real(kind(1.0D0)) :: htpmw = 0.0D0
  !! htpmw  :: heat transport system electrical pump power (MW)
  real(kind(1.0D0)) :: htpmw_blkt = 0.0D0
  !! htpmw_blkt /0.0/ : blanket coolant mechanical pumping power (MW)
  real(kind(1.0D0)) :: htpmw_div = 0.0D0
  !! htpmw_div /0.0/ : divertor coolant mechanical pumping power (MW)
  real(kind(1.0D0)) :: htpmw_fw = 0.0D0
  !! htpmw_fw /0.0/ : first wall coolant mechanical pumping power (MW)
  real(kind(1.0D0)) :: htpmw_shld = 0.0D0
  !! htpmw_shld /.0/ : shield and vacuum vessel coolant mechanical pumping power (MW)
  real(kind(1.0D0)) :: htpsecmw = 0.0D0
  !! htpsecmw : Waste power lost from primary coolant pumps (MW)
  !!

  ! Issue #506 Hydrogen production removed

  !  To be REMOVED
  integer :: ipowerflow = 1
  !! ipowerflow /1/ : switch for power flow model:<UL>
  !!             <LI> = 0 pre-2014 version;
  !!             <LI> = 1 comprehensive 2014 model</UL>
  integer :: iprimnloss = 0
  !! iprimnloss /0/ : switch for lost neutron power through holes destiny (ipowerflow=0):<UL>
  !!             <LI> = 0 does not contribute to energy generation cycle;
  !!             <LI> = 1 contributes to energy generation cycle</UL>

  ! KEEP
  integer :: iprimshld = 1
  !! iprimshld /1/ : switch for shield thermal power destiny:<UL>
  !!            <LI> = 0 does not contribute to energy generation cycle;
  !!            <LI> = 1 contributes to energy generation cycle</UL>

  integer, bind(C) :: nphx = 0
  !! nphx : number of primary heat exchangers
  real(kind(1.0D0)), bind(C) :: pacpmw = 0.0D0
  !! pacpmw : total pulsed power system load (MW)
  real(kind(1.0D0)) :: peakmva = 0.0D0
  !! peakmva : peak MVA requirement
  real(kind(1.0D0)), bind(C) :: pfwdiv = 0.0D0
  !! pfwdiv : heat removal from first wall/divertor (MW)
  real(kind(1.0D0)), bind(C) :: pgrossmw = 0.0D0
  !! pgrossmw : gross electric power (MW)
  real(kind(1.0D0)), bind(C) :: pinjht = 0.0D0
  !! pinjht : power dissipated in heating and current drive system (MW)
  real(kind(1.0D0)) :: pinjmax = 120.0D0
  !! pinjmax : maximum injector power during pulse (heating and ramp-up/down phase) (MW)
  real(kind(1.0D0)), bind(C) :: pinjwp = 0.0D0
  !! pinjwp : injector wall plug power (MW)
  real(kind(1.0D0)) :: pinjwpfix = 0.0D0
  !! pinjwpfix : secondary injector wall plug power (MW)
  real(kind(1.0D0)) :: pnetelmw = 0.0D0
  !! pnetelmw : net electric power (MW)
  real(kind(1.0D0)) :: precircmw = 0.0D0
  !! precircmw : recirculating electric power (MW)
  real(kind(1.0D0)) :: priheat = 0.0D0
  !! priheat : total thermal power removed from fusion core (MW)
  real(kind(1.0D0)) :: psecdiv = 0.0D0
  !! psecdiv : Low-grade heat lost in divertor (MW)
  real(kind(1.0D0)) :: psechcd = 0.0D0
  !! psechcd : Low-grade heat lost into HCD apparatus (MW)
  real(kind(1.0D0)) :: psechtmw = 0.0D0
  !! psechtmw : Low-grade heat (MW)

  ! NEW
  real(kind(1.0D0)) :: pseclossmw = 0.0D0
  !! pseclossmw : Low-grade heat (VV + lost)(MW)

  real(kind(1.0D0)) :: psecshld = 0.0D0
  !! psecshld : Low-grade heat deposited in shield (MW)
  real(kind(1.0D0)), bind(C) :: pthermmw = 0.0D0
  !! pthermmw : High-grade heat useful for electric production (MW)
  real(kind(1.0D0)) :: pwpm2 = 150.0D0
  !! pwpm2 /150.0/ : base AC power requirement per unit floor area (W/m2)
  real(kind(1.0D0)) :: tfacpd = 0.0D0
  !! tfacpd : total steady state TF coil AC power demand (MW)
  real(kind(1.0D0)), bind(C) :: tlvpmw = 0.0D0
  !! tlvpmw : estimate of total low voltage power (MW)
  real(kind(1.0D0)), bind(C) :: trithtmw = 15.0D0
  !! trithtmw /15.0/ : power required for tritium processing (MW)
  real(kind(1.0D0)) :: tturb = 0.0D0
  !! tturb : coolant temperature at turbine inlet (K) (secondary_cycle = 3,4)
  real(kind(1.0D0)), bind(C) :: vachtmw = 0.5D0
  !! vachtmw /0.5/ : vacuum pump power (MW)

end module heat_transport_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module times_variables

  !! Module containing global variables relating to the
  !! plasma pulse timings
  !! This module contains global variables relating to the
  !! plasma pulse timings.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: pulsetimings = 1.0D0
  !! pulsetimings /0.0/ : switch for pulse timings (if lpulse=1):<UL>
  !!           <LI> = 0, tohs = Ip(MA)/0.1 tramp, tqnch = input;
  !!           <LI> = 1, tohs = iteration var or input. tramp/tqnch max of input or tohs</UL>
  real(kind(1.0D0)), bind(C) :: tburn = 1000.0D0
  !! tburn /1000.0/ : burn time (s) (calculated if lpulse=1)
  real(kind(1.0D0)) :: tburn0 = 0.0D0
  !! tburn0 : burn time (s) - used for internal consistency
  real(kind(1.0D0)), bind(C) :: tcycle = 0.0D0
  !! tcycle : full cycle time (s)
  real(kind(1.0D0)), bind(C) :: tdown = 0.0D0
  !! tdown : down time (s)
  real(kind(1.0D0)), bind(C) :: tdwell = 1800.0D0
  !! tdwell /1800.0/ : time between pulses in a pulsed reactor (s)
  !!                  (iteration variable 17)
  real(kind(1.0D0)), bind(C) :: theat = 10.0D0
  !! theat /10.0/ : heating time, after current ramp up (s)
  real(kind(1.0D0)), dimension(6) :: tim = 0.0D0
  !! tim(6) : array of time points during plasma pulse (s)
  character(len=11), dimension(6) :: timelabel = (/ 'Start',   &
  !! timelabel(6) : array of time labels during plasma pulse (s)
                                                    'BOP  ',     &
                                                    'EOR  ',     &
                                                    'BOF  ',     &
                                                    'EOF  ',     &
                                                    'EOP  ' /)
  character(len=11), dimension(5) :: intervallabel = (/ 'tramp',     &
  !! intervallabel(6) : time intervals - as strings (s)
                                                        'tohs ',      &
                                                        'theat',     &
                                                        'tburn',     &
                                                        'tqnch' /)
  real(kind(1.0D0)), bind(C) :: tohs = 30.0D0
  !! tohs /30.0/ : plasma current ramp-up time for current initiation (s)
  !!               (but calculated if lpulse=0)
  !!               (iteration variable 65)
  real(kind(1.0D0)) :: tohsin = 0.0D0
  !! tohsin /0.0/ : switch for plasma current ramp-up time (if lpulse=0):<UL>
  !!           <LI> = 0, tohs = tramp = tqnch = Ip(MA)/0.5;
  !!           <LI> <>0, tohs = tohsin; tramp, tqnch are input</UL>
  real(kind(1.0D0)) :: tpulse = 0.0D0
  !! tpulse : pulse length = tohs + theat + tburn + tqnch
  real(kind(1.0D0)) :: tqnch = 15.0D0
  !! tqnch /15.0/ : shut down time for PF coils (s); if pulsed, = tohs
  real(kind(1.0D0)) :: tramp = 15.0D0
  !! tramp /15.0/ : initial PF coil charge time (s); if pulsed, = tohs

end module times_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module buildings_variables

  !! Module containing global variables relating to the
  !! plant buildings
  !! This module contains global variables relating to the
  !! plant buildings.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: admv = 1.0D5
  !! admv /1.0e5/ : administration building volume (m3)

  real(kind(1.0D0)) :: admvol = 0.0D0
  !! admvol : volume of administration buildings (m3)

  real(kind(1.0D0)) :: clh1 = 2.5D0
  !! clh1 /2.5/ : vertical clearance from TF coil to cryostat (m)
  !!              (calculated for tokamaks)

  real(kind(1.0D0)) :: clh2 = 15.0D0
  !! clh2 /15.0/ : clearance beneath TF coil to foundation
  !!               (including basement) (m)

  real(kind(1.0D0)) :: conv = 6.0D4
  !! conv /6.0e4/ : control building volume (m3)
  real(kind(1.0D0)) :: convol = 0.0D0
  !! convol : volume of control, protection and i&c building (m3)
  real(kind(1.0D0)) :: cryvol = 0.0D0
  !! cryvol : volume of cryoplant building (m3)
  real(kind(1.0D0)) :: efloor = 0.0D0
  !! efloor : effective total floor space (m2)
  real(kind(1.0D0)) :: elevol = 0.0D0
  !! elevol : volume of electrical equipment building (m3)
  real(kind(1.0D0)) :: esbldgm3 = 1.0D3
  !! esbldgm3 /1.0e3/ : volume of energy storage equipment building (m3)
  !!                    (not used if lpulse=0)
  real(kind(1.0D0)) :: fndt = 2.0D0
  !! fndt /2.0/ : foundation thickness (m)
  real(kind(1.0D0)) :: hccl = 5.0D0
  !! hccl /5.0/ : clearance around components in hot cell (m)
  real(kind(1.0D0)) :: hcwt = 1.5D0
  !! hcwt /1.5/ : hot cell wall thickness (m)
  real(kind(1.0D0)) :: mbvfac = 2.8D0
  !! mbvfac /2.8/ : maintenance building volume multiplication factor
  real(kind(1.0D0)) :: pfbldgm3 = 2.0D4
  !! pfbldgm3 /2.0e4/ : volume of PF coil power supply building (m3)
  real(kind(1.0D0)) :: pibv = 2.0D4
  !! pibv /2.0e4/ : power injection building volume (m3)
  real(kind(1.0D0)) :: rbrt = 1.0D0
  !! rbrt /1.0/ : reactor building roof thickness (m)
  real(kind(1.0D0)) :: rbvfac = 1.6D0
  !! rbvfac /1.6/ : reactor building volume multiplication factor
  real(kind(1.0D0)) :: rbvol = 0.0D0
  !! rbvol : reactor building volume (m3)
  real(kind(1.0D0)) :: rbwt = 2.0D0
  !! rbwt /2.0/ : reactor building wall thickness (m)
  real(kind(1.0D0)) :: rmbvol = 0.0D0
  !! rmbvol : volume of maintenance and assembly building (m3)
  real(kind(1.0D0)) :: row = 4.0D0
  !! row /4.0/ : clearance to building wall for crane operation (m)
  real(kind(1.0D0)) :: rxcl = 4.0D0
  !! rxcl /4.0/ : clearance around reactor (m)
  real(kind(1.0D0)) :: shmf = 0.5D0
  !! shmf /0.5/ : fraction of shield mass per TF coil
  !!              to be moved in the maximum shield lift
  real(kind(1.0D0)) :: shov = 1.0D5
  !! shov /1.0e5/ : shops and warehouse volume (m3)
  real(kind(1.0D0)) :: shovol = 0.0D0
  !! shovol :volume of shops and buildings for plant auxiliaries (m3)
  real(kind(1.0D0)) :: stcl = 3.0D0
  !! stcl /3.0/ : clearance above crane to roof (m)
  real(kind(1.0D0)) :: tfcbv = 2.0D4
  !! tfcbv /2.0e4/ : volume of TF coil power supply building (m3)
  !!                 (calculated if TF coils are superconducting)
  real(kind(1.0D0)) :: trcl = 1.0D0
  !! trcl /1.0/ : transportation clearance between components (m)
  real(kind(1.0D0)) :: triv = 4.0D4
  !! triv /4.0e4/ : volume of tritium, fuel handling and
  !!                health physics buildings (m3)
  real(kind(1.0D0)) :: volnucb = 0.0D0
  !! volnucb : sum of nuclear buildings volumes (m3)
  real(kind(1.0D0)), bind(C) :: volrci = 0.0D0
  !! volrci : internal volume of reactor building (m3)
  real(kind(1.0D0)) :: wgt = 5.0D5
  !! wgt /5.0e5/ : reactor building crane capacity (kg)
  !!               (calculated if 0 is input)
  real(kind(1.0D0)) :: wgt2 = 1.0D5
  !! wgt2 /1.0e5/ : hot cell crane capacity (kg)
  !!                (calculated if 0 is input)
  real(kind(1.0D0)) :: wrbi = 0.0D0
  !! wrbi : distance from centre of machine to building wall (m),
  !!        i.e. reactor building half-width
  real(kind(1.0D0)) :: wsvfac = 1.9D0
  !! wsvfac /1.9/ : warm shop building volume multiplication factor
  real(kind(1.0D0)), bind(C) :: wsvol = 0.0D0
  !! wsvol : volume of warm shop building (m3)

end module buildings_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module build_variables

  !! Module containing global variables relating to the
  !! machine's radial and vertical build
  !! This module contains global variables relating to the
  !! fusion power core's radial and vertical geometry (build).
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: aplasmin = 0.25D0
  !! aplasmin /0.25/ : minimum minor radius (m)

  real(kind(1.0D0)) :: blarea = 0.0D0
  !! blarea : blanket total surface area (m2)

  real(kind(1.0D0)) :: blareaib = 0.0D0
  !! blareaib : inboard blanket surface area (m2)

  real(kind(1.0D0)) :: blareaob = 0.0D0
  !! blareaob : outboard blanket surface area (m2)
  
  real(kind(1.0D0)) :: blbmith = 0.17D0
  !! blbmith /0.17/ : inboard blanket box manifold thickness (m)
  !!                   (blktmodel>0)
  real(kind(1.0D0)) :: blbmoth = 0.27D0
  !! blbmoth /0.27/ : outboard blanket box manifold thickness (m)
  !!                   (blktmodel>0)

  real(kind(1.0D0)) :: blbpith = 0.30D0
  !! blbpith /0.30/ : inboard blanket base plate thickness (m)
  !!                   (blktmodel>0)

  real(kind(1.0D0)) :: blbpoth = 0.35D0
  !! blbpoth /0.35/ : outboard blanket base plate thickness (m)
  !!                   (blktmodel>0)
  real(kind(1.0D0)) :: blbuith = 0.365D0
  !! blbuith /0.365/ : inboard blanket breeding zone thickness (m)
  !!                   (blktmodel>0)
  !!                   (iteration variable 90)
  real(kind(1.0D0)) :: blbuoth = 0.465D0
  !! blbuoth /0.465/ : outboard blanket breeding zone thickness (m)
  !!                   (blktmodel>0)
  !!                   (iteration variable 91)

  real(kind(1.0D0)) :: blnkith = 0.115D0
  !! blnkith /0.115/ : inboard blanket thickness (m);
  !!                   (calculated if blktmodel > 0)
  !!                   (=0.0 if iblnkith=0)

  real(kind(1.0D0)) :: blnkoth = 0.235D0
  !! blnkoth /0.235/ : outboard blanket thickness (m);
  !!                   calculated if blktmodel > 0

  real(kind(1.0D0)) :: blnktth = 0.0D0
  !! blnktth : top blanket thickness (m),
  !!           = mean of inboard and outboard blanket thicknesses

  real(kind(1.0D0)) :: bore = 1.42D0
  !! bore /1.42/ : central solenoid inboard radius (m)
  !!               (iteration variable 29)

  real(kind(1.0D0)) :: clhsf = 4.268D0
  !! clhsf /4.268/ : cryostat lid height scaling factor (tokamaks)
  real(kind(1.0D0)) :: ddwex = 0.07D0
  !! ddwex /0.07/ : cryostat thickness (m)
  real(kind(1.0D0)) :: ddwi = 0.07D0
  !! ddwi /0.07/ : vacuum vessel thickness (TF coil / shield) (m)
  real(kind(1.0D0)) :: fcspc = 0.6D0
  !! fcspc /0.6/ : Fraction of space occupied by CS pre-compression structure
  real(kind(1.0D0)) :: fmsbc = 0.0D0
  !! fmsbc /0.0/ : Martensitic fraction of steel in (non-existent!) bucking cylinder
  real(kind(1.0D0)) :: fmsbl = 0.0D0
  !! fmsbl /0.0/ : Martensitic fraction of steel in blanket
  real(kind(1.0D0)) :: fmsdwe = 0.0D0
  !! fmsdwe /0.0/ : Martensitic fraction of steel in cryostat
  real(kind(1.0D0)) :: fmsdwi = 0.0D0
  !! fmsdwi /0.0/ : Martensitic fraction of steel in vacuum vessel
  real(kind(1.0D0)) :: fmsfw = 0.0D0
  !! fmsfw /0.0/ : Martensitic fraction of steel in first wall
  real(kind(1.0D0)) :: fmsoh = 0.0D0
  !! fmsoh /0.0/ : Martensitic fraction of steel in central solenoid
  real(kind(1.0D0)) :: fmssh = 0.0D0
  !! fmssh /0.0/ : Martensitic fraction of steel in shield
  real(kind(1.0D0)) :: fmstf = 0.0D0
  !! fmstf /0.0/ : Martensitic fraction of steel in TF coil

  real(kind(1.0D0)) :: fseppc = 3.5D8
  !! fseppc /3.5d8/ : Separation force in CS coil pre-compression structure

  real(kind(1.0D0)) :: fwarea = 0.0D0
  !! fwarea : first wall total surface area (m2)

  real(kind(1.0D0)) :: fwareaib = 0.0D0
  !! fwareaib : inboard first wall surface area (m2)

  real(kind(1.0D0)) :: fwareaob = 0.0D0
  !! fwareaob : outboard first wall surface area (m2)

  ! MDK These are now calculated
  real(kind(1.0D0)) :: fwith = 0.0D0
  !! fwith : inboard first wall thickness, initial estimate (m)
  real(kind(1.0D0)) :: fwoth = 0.0D0
  !! fwoth : outboard first wall thickness, initial estimate (m)
  ! Issue #481 Rename gapds
  real(kind(1.0D0)) :: gapds = 0.155D0
  !! gapds /0.155/ : gap between inboard vacuum vessel and thermal shield (m)
  !!               (iteration variable 61)
  real(kind(1.0D0)) :: gapoh = 0.08D0
  !! gapoh /0.08/ : gap between central solenoid and TF coil (m)
  !!               (iteration variable 42)
  real(kind(1.0D0)) :: gapomin = 0.234D0
  !! gapomin /0.234/ : minimum gap between outboard vacuum vessel and TF coil (m)
  !!                  (iteration variable 31)
  real(kind(1.0D0)) :: gapsto = 0.0D0
  !! gapsto : gap between outboard vacuum vessel and TF coil (m)

  real(kind(1.0D0)) :: hmax = 0.0D0
  !! hmax : maximum (half-)height of TF coil (inside edge) (m)

  real(kind(1.0D0)) :: hpfdif = 0.0D0
  !! hpfdif : difference in distance from midplane of upper and lower
  !!          portions of TF legs (non-zero for single-null devices) (m)
  real(kind(1.0D0)) :: hpfu = 0.0D0
  !! hpfu : height to top of (upper) TF coil leg (m)
  real(kind(1.0D0)) :: hr1 = 0.0D0
  !! hr1 : half-height of TF coil inboard leg straight section (m)
  integer :: iohcl = 1
  !! iohcl /1/ : switch for existence of central solenoid:<UL>
  !!        <LI> = 0 central solenoid not present;
  !!        <LI> = 1 central solenoid exists</UL>

  integer :: iprecomp = 1
  !! iprecomp /1/ : switch for existence of central solenoid pre-compression structure:<UL>
  !!        <LI> = 0 no pre-compression structure;
  !!        <LI> = 1 calculated pre-compression structure</UL>

  real(kind(1.0D0)) :: ohcth = 0.811D0
  !! ohcth /0.811/ : central solenoid thickness (m)
  !!                (iteration variable 16)

  real(kind(1.0D0)) :: precomp = 0.0D0
  !! precomp : CS coil precompression structure thickness (m)
  real(kind(1.0D0)) :: rbld = 0.0D0
  !! rbld : sum of thicknesses to the major radius (m)
  real(kind(1.0D0)) :: rinboard = 0.651D0
  !! rinboard /0.651/ : plasma inboard radius (m)
  !!                    (consistency equation 29)
  real(kind(1.0D0)) :: rsldi = 0.0D0
  !! rsldi : radius to inboard shield (inside point) (m)
  real(kind(1.0D0)) :: rsldo = 0.0D0
  !! rsldo : radius to outboard shield (outside point) (m)

  real(kind(1.0D0)) :: r_vv_inboard_out = 0.0D0
  !! r_vv_inboard_out : Radial position of vacuum vessel [m]

  real(kind(1.0D0)) :: r_tf_inboard_mid = 0.0D0
  !!  r_tf_inboard_mid : Mid-plane Outer radius of centre of inboard TF leg (m)
       
  real(kind(1.0D0)) :: r_tf_outboard_mid = 0.0D0
  !!  r_tf_outboard_mid : radius to the centre of the outboard TF coil leg (m)

  real(kind(1.0D0)) :: rtop = 0.0D0
  !!  rtop : Top outer radius of centre of the centropost (ST only) (m)

  real(kind(1.0D0)) :: dr_tf_inner_bore = 0.0D0
  !!  dr_tf_inner_bore : TF coil horizontal inner bore (m)

  real(kind(1.0D0)) :: dh_tf_inner_bore = 0.0D0
  !!  dh_tf_inner_bore : TF coil vertical inner bore (m)

  real(kind(1.0D0)) :: scrapli = 0.14D0
  !! scrapli /0.14/ : gap between plasma and first wall, inboard side (m)
  !!                  (used if iscrp=1) (iteration variable 73)

  real(kind(1.0D0)) :: scraplo = 0.15D0
  !! scraplo /0.15/ : gap between plasma and first wall, outboard side (m)
  !!                  (used if iscrp=1) (iteration variable 74)
  real(kind(1.0D0)) :: sharea = 0.0D0
  !! sharea : shield total surface area (m2)
  real(kind(1.0D0)) :: shareaib = 0.0D0
  !! shareaib : inboard shield surface area (m2)
  real(kind(1.0D0)) :: shareaob = 0.0D0
  !! shareaob : outboard shield surface area (m2)
  real(kind(1.0D0)) :: shldith = 0.69D0
  !! shldith /0.69/ : inboard shield thickness (m)
  !!                  (iteration variable 93)
   real(kind(1.0D0)) :: shldlth = 0.7D0
   !! shldlth /0.7/ : lower (under divertor) shield thickness (m)
  real(kind(1.0D0)) :: shldoth = 1.05D0
  !! shldoth /1.05/ : outboard shield thickness (m)
  !!                  (iteration variable 94)
  real(kind(1.0D0)) :: shldtth = 0.6D0
  !! shldtth /0.60/ : upper/lower shield thickness (m);
  !!                  calculated if blktmodel > 0
  real(kind(1.0D0)) :: sigallpc = 3.0D8
  !! sigallpc /3.0d8/ : allowable stress in CSpre-compression structure (Pa);

  ! Issue #514 Make tfcth an output not an iteration variable
  !real(kind(1.0D0)) :: tfcth = 1.173D0
  ! !!! tfcth /1.173/ : inboard TF coil(s  ) thickness (m)
  ! !!!               (calculated for stellarators)
  ! !!!               (iteration variable 13)

  real(kind(1.0D0)) :: tfcth = 0.0D0
  !! tfcth : inboard TF coil thickness, (centrepost for ST) (m)
  !!               (calculated, NOT an iteration variable)

  real(kind(1.0D0)) :: tfoffset = 0.0D0
  !! tfoffset : vertical distance between centre of TF coils and centre of plasma (m)

  real(kind(1.0D0)) :: tfootfi = 1.19D0
  !! tfootfi /1.19/ : TF coil outboard leg / inboard leg radial thickness
  !!                 ratio (i_tf_sup=0 only)
  !!                 (iteration variable 75)

  real(kind(1.0D0)) :: tfthko = 0.0D0
  !! tfthko : outboard TF coil thickness (m)

  real(kind(1.0D0)) :: tftsgap = 0.05D0
  !! tftsgap /0.05/ : Minimum metal-to-metal gap between TF coil and thermal shield (m)

  real(kind(1.0D0)) :: thshield = 0.05D0
  !! thshield /0.05/ : TF-VV thermal shield thickness (m)

  real(kind(1.0D0)) :: vgap2 = 0.163D0
  !! vgap2 /0.163/ : vertical gap between vacuum vessel and thermal shields (m)
  ! Issue #481 Remove vgaptf
  real(kind(1.0D0)) :: vgap= 0.0D0
  !! vgap /0.0/ : vertical gap between x-point and divertor (m)
  !!               (if = 0, it is calculated)
  real(kind(1.0D0)) :: vgaptop = 0.60D0
  !! vgaptop /0.60/ : vertical gap between top of plasma and first wall (m)
  real(kind(1.0D0)) :: vvblgap = 0.05D0
  !! vvblgap /0.05/ : gap between vacuum vessel and blanket (m)

  real(kind(1.0D0)) :: plleni = 1.0D0
  !! plleni /1.0/ : length of inboard divertor plate (m)
  real(kind(1.0D0)) :: plleno = 1.0D0
  !! plleno /1.0/ : length of outboard divertor plate (m)
  real(kind(1.0D0)) :: plsepi = 1.0D0
  !! plsepi /1.0/ : poloidal length, x-point to inboard strike point (m)
  real(kind(1.0D0)) :: plsepo = 1.5D0
  !! plsepo /1.5/ : poloidal length, x-point to outboard strike point (m)
  real(kind(1.0D0)) :: rspo = 0.0D0
  !! rspo : outboard strike point radius (m)


end module build_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module cost_variables

  !! Module containing global variables relating to the
  !! costing algorithms
  !! This module contains global variables relating to the
  !! costing algorithms of a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)), bind(C) :: abktflnc = 5.0D0
  !! abktflnc /5.0/ : allowable first wall/blanket neutron
  !!                   fluence (MW-yr/m2) (blktmodel=0)
  real(kind(1.0D0)), bind(C) :: adivflnc = 7.0D0
  !! adivflnc /7.0/ : allowable divertor heat fluence (MW-yr/m2)
  real(kind(1.0D0)) :: blkcst = 0.0D0
  !! blkcst : blanket direct cost (M$)
  real(kind(1.0D0)) :: c221 = 0.0D0
  !! c221 : total account 221 cost (M$) (first wall, blanket, shield,
  !!        support structure and divertor plates)
  real(kind(1.0D0)) :: c222 = 0.0D0
  !! c222 : total account 222 cost (M$) (TF coils + PF coils)
  real(kind(1.0D0)) :: capcost = 0.0D0
  !! capcost : total capital cost including interest (M$)
  real(kind(1.0D0)) :: cconfix = 80.0D0
  !! cconfix /80.0/ : fixed cost of superconducting cable ($/m)
  real(kind(1.0D0)) :: cconshpf = 70.0D0
  !! cconshpf /70.0/ : cost of PF coil steel conduit/sheath ($/m)
  real(kind(1.0D0)) :: cconshtf = 75.0D0
  !! cconshtf /75.0/ : cost of TF coil steel conduit/sheath ($/m)
  real(kind(1.0D0)) :: cdcost = 0.0D0
  !! cdcost : current drive direct costs (M$)
  real(kind(1.0D0)), bind(C) :: cdirt = 0.0D0
  !! cdirt : total plant direct cost (M$)
  real(kind(1.0D0)), bind(C) :: cdrlife = 0.0D0
  !! cdrlife : lifetime of heating/current drive system (y)
  real(kind(1.0D0)) :: cfactr = 0.75D0
  !! cfactr /0.75/ : Total plant availability fraction;
  !!                 input if iavail = 0
  real(kind(1.0D0)) :: cpfact = 0.0D0
  !! cpfact : Total plant capacity factor

  real(kind(1.0D0)), dimension(4) :: cfind = &
  !! cfind(4) /0.244,0.244,0.244,0.29/ : indirect cost factor (func of lsa)
       (/0.244D0, 0.244D0, 0.244D0, 0.29D0/)

  real(kind(1.0D0)) :: cland = 19.2D0
  !! cland /19.2/ : cost of land (M$)
  real(kind(1.0D0)) :: coe = 0.0D0
  !! coe : cost of electricity ($/MW-hr)
  real(kind(1.0D0)) :: coecap = 0.0D0
  !! coecap : capital cost of electricity (m$/kW-hr)
  real(kind(1.0D0)) :: coefuelt = 0.0D0
  !! coefuelt : 'fuel' (including replaceable components) contribution to
  !!            cost of electricity (m$/kW-hr)
  real(kind(1.0D0)) :: coeoam = 0.0D0
  !! coeoam : operation and maintenance contribution to
  !!          cost of electricity (m$/kW-hr)
  real(kind(1.0D0)) :: concost = 0.0D0
  !! concost : plant construction cost (M$)
  real(kind(1.0D0)) :: costexp = 0.8D0
  !! costexp /0.8/ : cost exponent for scaling in 2015 costs model
  real(kind(1.0D0)) :: costexp_pebbles = 0.6D0
  !! costexp_pebbles /0.6/ : cost exponent for pebbles in 2015 costs model
  real(kind(1.0D0)) :: cost_factor_buildings = 1.0D0
  !! cost_factor_buildings /1.0/ : cost scaling factor for buildings
  real(kind(1.0D0)) :: cost_factor_land = 1.0D0
  !! cost_factor_land /1.0/ : cost scaling factor for land
  real(kind(1.0D0)) :: cost_factor_tf_coils = 1.0D0
  !! cost_factor_tf_coils /1.0/ : cost scaling factor for TF coils
  real(kind(1.0D0)) :: cost_factor_fwbs = 1.0D0
  !! cost_factor_fwbs /1.0/ : cost scaling factor for fwbs
  real(kind(1.0D0)) :: cost_factor_rh = 1.0D0
  !! cost_factor_rh /1.0/ : cost scaling factor for remote handling
  real(kind(1.0D0)) :: cost_factor_vv = 1.0D0
  !! cost_factor_vv /1.0/ : cost scaling factor for vacuum vessel
  real(kind(1.0D0)) :: cost_factor_bop = 1.0D0
  !! cost_factor_bop /1.0/ : cost scaling factor for energy conversion system
  real(kind(1.0D0)) :: cost_factor_misc = 1.0D0
  !! cost_factor_misc /1.0/ : cost scaling factor for remaining subsystems

  real(kind(1.0D0)) :: maintenance_fwbs = 0.2D0
  !! maintenance_fwbs /0.2/ : Maintenance cost factor:
  !!                          first wall, blanket, shield, divertor
  real(kind(1.0D0)) :: maintenance_gen = 0.05D0
  !! maintenance_gen /0.05/ : Maintenance cost factor: All other components except
  !!                          coils, vacuum vessel, thermal shield, cryostat, land
  real(kind(1.0D0)) :: amortization = 13.6D0
  !! amortization /13.6/ : amortization factor (fixed charge factor) "A" (years)

  integer :: cost_model = 1
  !! cost_model /1/ : switch for cost model:<UL>
  !!         <LI> = 0 use $ 1990 PROCESS model
  !!         <LI> = 1 use $ 2014 Kovari model
  !!         <LI> = 2 use $ 1980 STEP model (NOT RECOMMENDED - Under Development)</UL>
  real(kind(1.0D0)), bind(C) :: cowner = 0.15D0
  !! cowner /0.15/ : owner cost factor
  real(kind(1.0D0)) :: cplife = 0.0D0
  !! cplife : lifetime of centrepost (y)
  real(kind(1.0D0)) :: cpstcst = 0.0D0
  !! cpstcst : ST centrepost direct cost (M$)
  real(kind(1.0D0)), bind(C) :: cpstflnc = 10.0D0
  !! cpstflnc /10.0/ : allowable ST centrepost neutron fluence (MW-yr/m2)
  real(kind(1.0D0)) :: crctcore = 0.0D0
  !! crctcore : reactor core costs (categories 221, 222 and 223)
  real(kind(1.0D0)) :: csi = 16.0D0
  !! csi /16.0/ : allowance for site costs (M$)
  real(kind(1.0D0)) :: cturbb = 38.0D0
  !! cturbb /38.0/ : cost of turbine building (M$)
  real(kind(1.0D0)) :: decomf = 0.1D0
  !! decomf /0.1/ : proportion of constructed cost required for
  !!                decommissioning fund
  real(kind(1.0D0)) :: dintrt = 0.0D0
  !! dintrt /0.0/ : diff between borrowing and saving interest rates
  real(kind(1.0D0)) :: divcst = 0.0D0
  !! divcst : divertor direct cost (M$)
  real(kind(1.0D0)), bind(C) :: divlife = 0.0D0
  !! divlife : lifetime of divertor (y)
  real(kind(1.0D0)) :: dtlife = 0.0D0
  !! dtlife /0.0/ : period prior to the end of the plant life that
  !!                the decommissioning fund is used (years)
  real(kind(1.0D0)) :: fcap0 = 1.165D0
  !! fcap0 /1.165/ : average cost of money for construction of plant
  !!                 assuming design/construction time of six years
  real(kind(1.0D0)) :: fcap0cp = 1.08D0
  !! fcap0cp /1.08/ : average cost of money for replaceable components
  !!                  assuming lead time for these of two years
  real(kind(1.0D0)) :: fcdfuel = 0.1D0
  !! fcdfuel /0.1/ : fraction of current drive cost treated as fuel
  !!                 (if ifueltyp = 1)
  real(kind(1.0D0)), bind(C) :: fcontng = 0.195D0
  !! fcontng /0.195/ : project contingency factor
  real(kind(1.0D0)) :: fcr0 = 0.0966D0
  !! fcr0 /0.0966/ : fixed charge rate during construction
  real(kind(1.0D0)), bind(C) :: fkind = 1.0D0
  !! fkind /1.0/ : multiplier for Nth of a kind costs
  real(kind(1.0D0)) :: fwallcst = 0.0D0
  !! fwallcst : first wall cost (M$)
  integer :: iavail= 2
  !! iavail /2/ : switch for plant availability model:<UL>
  !!         <LI> = 0 use input value for cfactr;
  !!         <LI> = 1 calculate cfactr using Taylor and Ward 1999 model;
  !!         <LI> = 2 calculate cfactr using new (2015) model</UL>
  real(kind(1.0D0)) :: avail_min = 0.75D0
  !! avail_min /0.75/ : Minimum availability (constraint equation 61)
  real(kind(1.0D0)) :: tok_build_cost_per_vol = 1283.0D0
  !! tok_build_cost_per_vol /1283.0/ : Unit cost for tokamak complex buildings,
  !!                                   including building and site services ($/m3)
  real(kind(1.0D0)) :: light_build_cost_per_vol = 270.0D0
  !! light_build_cost_per_vol /270.0/ : Unit cost for unshielded non-active buildings ($/m3)
  real(kind(1.0D0)) :: favail = 1.0D0
  !! favail /1.0/ : F-value for minimum availability (constraint equation 61)
  integer, bind(C) :: num_rh_systems = 4
  !! num_rh_systems /4/ : Number of remote handling systems (1-10)
  real(kind(1.0D0)), bind(C) :: conf_mag = 0.99D0
  !! conf_mag /0.99/ : c parameter, which determines the temperature margin at which magnet lifetime starts to decline
  real(kind(1.0D0)), bind(C) :: div_prob_fail = 0.0002D0
  !! div_prob_fail /0.0002/ : Divertor probability of failure (per op day)
  real(kind(1.0D0)), bind(C) :: div_umain_time = 0.25D0
  !! div_umain_time /0.25/ : Divertor unplanned maintenance time (years)
  real(kind(1.0D0)), bind(C) :: div_nref = 7000.0D0
  !! div_nref /7000/ : Reference value for cycle cycle life of divertor
  real(kind(1.0D0)), bind(C) :: div_nu = 14000.0D0
  !! div_nu /14000/ : The cycle when the divertor fails with 100% probability
  real(kind(1.0D0)),bind(C) :: fwbs_nref = 20000.0D0
  !! fwbs_nref /20000/ : Reference value for cycle life of blanket
  real(kind(1.0D0)), bind(C) :: fwbs_nu = 40000.0D0
  !! fwbs_nu /40000/ : The cycle when the blanket fails with 100% probability
  real(kind(1.0D0)), bind(C) :: fwbs_prob_fail = 0.0002D0
  !! fwbs_prob_fail /0.0002/ : Fwbs probability of failure (per op day)
  real(kind(1.0D0)), bind(C) :: fwbs_umain_time = 0.25D0
  !! fwbs_umain_time /0.25/ : Fwbs unplanned maintenance time (years)
  real(kind(1.0D0)) :: redun_vacp = 25.0D0
  !! redun_vacp /25/ : Vacuum system pump redundancy level (%)
  integer :: redun_vac = 0
  !! redun_vac : Number of redundant vacuum pumps
  real(kind(1.0D0)), bind(C) :: t_operation = 0.0D0
  !! t_operation : Operational time (yrs)
  real(kind(1.0D0)) :: tbktrepl = 0.5D0
  !! tbktrepl /0.5/ : time taken to replace blanket (y)
  !!                (iavail=1)
  real(kind(1.0D0)) :: tcomrepl = 0.5D0
  !! tcomrepl /0.5/ : time taken to replace both blanket and divertor (y)
  !!                (iavail=1)
  real(kind(1.0D0)) :: tdivrepl = 0.25D0
  !! tdivrepl /0.25/ : time taken to replace divertor (y)
  !!                (iavail=1)
  real(kind(1.0D0)) :: uubop = 0.02D0
  !! uubop /0.02/ : unplanned unavailability factor for balance of plant
  !!                (iavail=1)
  real(kind(1.0D0)) :: uucd = 0.02D0
  !! uucd /0.02/ : unplanned unavailability factor for current drive
  !!                (iavail=1)
  real(kind(1.0D0)) :: uudiv = 0.04D0
  !! uudiv /0.04/ : unplanned unavailability factor for divertor
  !!                (iavail=1)
  real(kind(1.0D0)) :: uufuel = 0.02D0
  !! uufuel /0.02/ : unplanned unavailability factor for fuel system
  !!                (iavail=1)
  real(kind(1.0D0)) :: uufw = 0.04D0
  !! uufw /0.04/ : unplanned unavailability factor for first wall
  !!                (iavail=1)
  real(kind(1.0D0)) :: uumag = 0.02D0
  !! uumag /0.02/ : unplanned unavailability factor for magnets
  !!                (iavail=1)
  real(kind(1.0D0)) :: uuves = 0.04D0
  !! uuves /0.04/ : unplanned unavailability factor for vessel
  !!                (iavail=1)
  integer :: ifueltyp = 0
  !! ifueltyp /0/ : switch:<UL>
  !!           <LI> = 1 treat blanket divertor, first wall and
  !!                    fraction fcdfuel of CD equipment as fuel cost;
  !!           <LI> = 0 treat these as capital cost</UL>
  integer :: ipnet = 0
  !! ipnet /0/ : switch for net electric power calculation:<UL>
  !!        <LI> = 0 scale so that always > 0;
  !!        <LI> = 1 let go < 0 (no c-o-e)</UL>
  integer, bind(C) :: ireactor = 1
  !! ireactor /1/ : switch for net electric power and cost of
  !!                electricity calculations:<UL>
  !!           <LI> = 0 do not calculate MW(electric) or c-o-e;
  !!           <LI> = 1 calculate MW(electric) and c-o-e</UL>
  integer, bind(C) :: lsa = 4
  !! lsa /4/ : level of safety assurance switch (generally, use 3 or 4):<UL>
  !!      <LI> = 1 truly passively safe plant;
  !!      <LI> = 2,3 in-between;
  !!      <LI> = 4 like current fission plant</UL>
  real(kind(1.0D0)) :: moneyint = 0.0D0
  !! moneyint : interest portion of capital cost (M$)
  integer :: output_costs = 1
  !! output_costs /1/ : switch for costs output:<UL>
  !!           <LI> = 0 do not write cost-related outputs to file;
  !!           <LI> = 1 write cost-related outputs to file</UL>
  real(kind(1.0D0)) :: ratecdol = 0.0435D0
  !! ratecdol /0.0435/ : effective cost of money in constant dollars
  real(kind(1.0D0)) :: step_con = 1.5D-1
  !! step_con /0.15/ : Contingency Percentage
  real(kind(1.0D0)), dimension(68) :: step_ref = &
  !! step_ref(68) /.../ : Reference values for cost model 2
  (/ 3.0D0, 3.0D-1, 1.115D1, 1.5744D2, 3.592D1, 7.96D0, 9.16D0, 3.26D0, 5.369D1, &
  1.88D0, 6.6D-1, 8.63D0, 3.1D0, 2.05D0, 8.7D-1, 8.7D-1, 9.1D-1, 3.1D-1, 1.81D0, &
  8.236D1, 1.8607D2, 1.2572D2, 3.46D1, 7.25D0, 4.0D0, 3.349D1, 5.274D1, 4.86D0, &
  5.29D1, 2.45D0, 2.82D0, 1.676D1, 6.984D1, 7.7D0, 3.6D0, 2.8D0, 8.0D-1, 1.7D0, &
  1.8D0, 1.3D0, 3.86D1, 3.83D1, 0.0D0, 2.4D-1, 8.0D-2, 0.0D0, 2.0D0, 1.97D0, 1.16D0, &
  2.341D1, 7.733D1, 4.37D0, 4.434D1, 1.918D1, 9.39D0, 5.084D1, 8.7D0, 1.239D1, &
  1.704D1, 7.8D0, 2.11D0, 1.74D1, 3.599D1, 8.2D0, 1.568D1, 1.235D1, 6.22D0, 7.5D-1 /)
  real(kind(1.0D0)), bind(C) :: tlife = 30.0D0
  !! tlife /30.0/ : plant life (years)
  real(kind(1.0D0)), parameter :: ucad = 180.0D0
  !! ucad /180.0/ FIX : unit cost for administration buildings (M$/m3)
  real(kind(1.0D0)), parameter :: ucaf = 1.5D6
  !! ucaf /1.5e6/ FIX : unit cost for aux facility power equipment ($)
  real(kind(1.0D0)), parameter :: ucahts = 31.0D0
  !! ucahts /31.0/ FIX : unit cost for aux heat transport equipment ($/W**exphts)
  real(kind(1.0D0)), parameter :: ucap = 17.0D0
  !! ucap /17.0/ FIX : unit cost of auxiliary transformer ($/kVA)
  real(kind(1.0D0)) :: ucblbe = 260.0D0
  !! ucblbe /260.0/ : unit cost for blanket beryllium ($/kg)
  real(kind(1.0D0)) :: ucblbreed = 875.0D0
  !! ucblbreed /875.0/ : unit cost for breeder material ($/kg) (blktmodel>0)
  real(kind(1.0D0)) :: ucblli = 875.0D0
  !! ucblli /875.0/ : unit cost for blanket lithium ($/kg) (30% Li6)
  real(kind(1.0D0)) :: ucblli2o = 600.0D0
  !! ucblli2o /600.0/ : unit cost for blanket Li_2O ($/kg)
  real(kind(1.0D0)) :: ucbllipb = 10.3D0
  !! ucbllipb /10.3/ : unit cost for blanket Li-Pb ($/kg) (30% Li6)
  real(kind(1.0D0)) :: ucblss = 90.0D0
  !! ucblss /90.0/ : unit cost for blanket stainless steel ($/kg)
  real(kind(1.0D0)) :: ucblvd = 200.0D0
  !! ucblvd /200.0/ : unit cost for blanket vanadium ($/kg)
  real(kind(1.0D0)), parameter :: ucbpmp = 2.925D5
  !! ucbpmp /2.925e5/ FIX : vacuum system backing pump cost ($)
  real(kind(1.0D0)) :: ucbus = 0.123D0
  !! ucbus /0.123/ : cost of aluminium bus for TF coil ($/A-m)
  real(kind(1.0D0)) :: uccase = 50.0D0
  !! uccase /50.0/ : cost of superconductor case ($/kg)
  real(kind(1.0D0)), parameter :: ucco = 350.0D0
  !! ucco /350.0/ FIX : unit cost for control buildings (M$/m3)
  real(kind(1.0D0)) :: uccpcl1 = 250.0D0
  !! uccpcl1 /250.0/ : cost of high strength tapered copper ($/kg)
  real(kind(1.0D0)) :: uccpclb = 150.0D0
  !! uccpclb /150.0/ : cost of TF outboard leg plate coils ($/kg)
  real(kind(1.0D0)), parameter :: uccpmp = 3.9D5
  !! uccpmp /3.9e5/ FIX : vacuum system cryopump cost ($)
  real(kind(1.0D0)), parameter :: uccr = 460.0D0
  !! uccr /460.0/ FIX : unit cost for cryogenic building (M$/vol)
  real(kind(1.0D0)), bind(C) :: uccry = 9.3D4
  !! uccry /9.3e4/ : heat transport system cryoplant costs ($/W**expcry)
  real(kind(1.0D0)) :: uccryo = 32.0D0
  !! uccryo /32.0/ : unit cost for vacuum vessel ($/kg)
  real(kind(1.0D0)) :: uccu = 75.0D0
  !! uccu /75.0/ : unit cost for copper in superconducting cable ($/kg)
  real(kind(1.0D0)), parameter :: ucdgen = 1.7D6
  !! ucdgen /1.7e6/ FIX : cost per 8 MW diesel generator ($)
  real(kind(1.0D0)) :: ucdiv = 2.8D5
  !! ucdiv /2.8e5/ : cost of divertor blade ($)
  real(kind(1.0D0)), parameter :: ucdtc = 13.0D0
  !! ucdtc /13.0/ FIX : detritiation, air cleanup cost ($/10000m3/hr)
  real(kind(1.0D0)), parameter :: ucduct = 4.225D4
  !! ucduct /4.225e4/ FIX : vacuum system duct cost ($/m)
  real(kind(1.0D0)) :: ucech = 3.0D0
  !! ucech /3.0/ : ECH system cost ($/W)
  real(kind(1.0D0)), parameter :: ucel = 380.0D0
  !! ucel /380.0/ FIX : unit cost for electrical equipment building (M$/m3)
  real(kind(1.0D0)), parameter :: uces1 = 3.2D4
  !! uces1 /3.2e4/ FIX : MGF (motor-generator flywheel) cost factor ($/MVA**0.8)
  real(kind(1.0D0)), parameter :: uces2 = 8.8D3
  !! uces2 /8.8e3/ FIX : MGF (motor-generator flywheel) cost factor ($/MJ**0.8)
  real(kind(1.0D0)), bind(C) :: ucf1 = 2.23D7
  !! ucf1 /2.23e7/ : cost of fuelling system ($)
  real(kind(1.0D0)) :: ucfnc = 35.0D0
  !! ucfnc /35.0/ : outer PF coil fence support cost ($/kg)
  real(kind(1.0D0)), parameter :: ucfpr = 4.4D7
  !! ucfpr /4.4e7/ FIX : cost of 60g/day tritium processing unit ($)
  real(kind(1.0D0)) :: ucfuel = 3.45D0
  !! ucfuel /3.45/ : unit cost of D-T fuel (M$/year/1200MW)
  real(kind(1.0D0)), parameter :: ucfwa = 6.0D4
  !! ucfwa /6.0e4/ FIX : first wall armour cost ($/m2)
  real(kind(1.0D0)), parameter :: ucfwps = 1.0D7
  !! ucfwps /1.0e7/ FIX : first wall passive stabiliser cost ($)
  real(kind(1.0D0)), parameter :: ucfws = 5.3D4
  !! ucfws /5.3e4/ FIX : first wall structure cost ($/m2)
  real(kind(1.0D0)), parameter :: ucgss = 35.0D0
  !! ucgss /35.0/ FIX : cost of reactor structure ($/kg)
  real(kind(1.0D0)) :: uche3 = 1.0D6
  !! uche3 /1.0e6/ : cost of helium-3 ($/kg)
  real(kind(1.0D0)), bind(C) :: uchrs = 87.9D6
  !! uchrs /87.9e6/ : cost of heat rejection system ($)
  real(kind(1.0D0)), dimension(2) :: uchts = (/15.3D0, 19.1D0/)
  !! uchts(2) /15.3,19.1/ : cost of heat transport system equipment
  !!                        per loop ($/W); dependent on coolant type (coolwh)
  real(kind(1.0D0)), bind(C) :: uciac = 1.5D8
  !! uciac /1.5e8/ : cost of instrumentation, control & diagnostics ($)
  real(kind(1.0D0)) :: ucich = 3.0D0
  !! ucich /3.0/ : ICH system cost ($/W)
  real(kind(1.0D0)), parameter :: ucint = 35.0D0
  !! ucint /35.0/ FIX : superconductor intercoil structure cost ($/kg)
  real(kind(1.0D0)) :: uclh = 3.3D0
  !! uclh /3.3/ : lower hybrid system cost ($/W)
  real(kind(1.0D0)), parameter :: uclv = 16.0D0
  !! uclv /16.0/ FIX : low voltage system cost ($/kVA)
  real(kind(1.0D0)), parameter :: ucmb = 260.0D0
  !! ucmb /260.0/ FIX: unit cost for reactor maintenance building (M$/m3)
  real(kind(1.0D0)), bind(C) :: ucme = 1.25D8
  !! ucme /1.25e8/ : cost of maintenance equipment ($)
  real(kind(1.0D0)), bind(C) :: ucmisc = 2.5D7
  !! ucmisc /2.5e7/ : miscellaneous plant allowance ($)
  real(kind(1.0D0)) :: ucnbi = 3.3D0
  !! ucnbi /3.3/ : NBI system cost ($/W)
  real(kind(1.0D0)), parameter :: ucnbv = 1000.0D0
  !! ucnbv /1000.0/ FIX : cost of nuclear building ventilation ($/m3)
  real(kind(1.0D0)), dimension(4) :: ucoam = &
  !! ucoam(4) /68.8,68.8,68.8,74.4/ : annual cost of operation and
  !!                                  maintenance (M$/year/1200MW**0.5)
       (/68.8D0, 68.8D0, 68.8D0, 74.4D0/)
  real(kind(1.0D0)) :: ucpens = 32.0D0
  !! ucpens /32.0/ : penetration shield cost ($/kg)
  real(kind(1.0D0)) :: ucpfb = 210.0D0
  !! ucpfb /210.0/ : cost of PF coil buses ($/kA-m)
  real(kind(1.0D0)) :: ucpfbk = 1.66D4
  !! ucpfbk /1.66e4/ : cost of PF coil DC breakers ($/MVA**0.7)
  real(kind(1.0D0)) :: ucpfbs = 4.9D3
  !! ucpfbs /4.9e3/ : cost of PF burn power supplies ($/kW**0.7)
  real(kind(1.0D0)) :: ucpfcb = 7.5D4
  !! ucpfcb /7.5e4/ : cost of PF coil AC breakers ($/circuit)
  real(kind(1.0D0)) :: ucpfdr1 = 150.0D0
  !! ucpfdr1 /150.0/ : cost factor for dump resistors ($/MJ)
  real(kind(1.0D0)) :: ucpfic = 1.0D4
  !! ucpfic /1.0e4/ : cost of PF instrumentation and control ($/channel)
  real(kind(1.0D0)) :: ucpfps = 3.5D4
  !! ucpfps /3.5e4/ : cost of PF coil pulsed power supplies ($/MVA)
  real(kind(1.0D0)), parameter :: ucphx = 15.0D0
  !! ucphx /15.0/ FIX : primary heat transport cost ($/W**exphts)
  real(kind(1.0D0)), parameter :: ucpp = 48.0D0
  !! ucpp /48.0/ FIX : cost of primary power transformers ($/kVA**0.9)
  real(kind(1.0D0)) :: ucrb = 400.0D0
  !! ucrb /400.0/ : cost of reactor building (M$/m3)
  real(kind(1.0D0)), dimension(6) :: ucsc = &
  !! ucsc(6) /600.0,600.0,300.0,600.0/ : cost of superconductor ($/kg)
       (/600.0D0, 600.0D0, 300.0D0, 600.0D0, 600.0D0, 600.0D0/)
  real(kind(1.0D0)), parameter :: ucsh = 115.0D0
  !! ucsh /115.0/ FIX : cost of shops and warehouses (M$/m3)
  real(kind(1.0D0)) :: ucshld = 32.0D0
  !! ucshld /32.0/ : cost of shield structural steel ($/kg)
  real(kind(1.0D0)), parameter :: ucswyd = 1.84D7
  !! ucswyd /1.84e7/ FIX : switchyard equipment costs ($)
  real(kind(1.0D0)) :: uctfbr = 1.22D0
  !! uctfbr /1.22/ : cost of TF coil breakers ($/W**0.7)
  real(kind(1.0D0)) :: uctfbus = 100.0D0
  !! uctfbus /100.0/ : cost of TF coil bus ($/kg)
  real(kind(1.0D0)), parameter :: uctfdr = 1.75D-4
  !! uctfdr /1.75e-4/ FIX : cost of TF coil dump resistors ($/J)
  real(kind(1.0D0)), parameter :: uctfgr = 5000.0D0
  !! uctfgr /5000.0/ FIX : additional cost of TF coil dump resistors ($/coil)
  real(kind(1.0D0)), parameter :: uctfic = 1.0D4
  !! uctfic /1.0e4/ FIX : cost of TF coil instrumentation and control ($/coil/30)
  real(kind(1.0D0)) :: uctfps = 24.0D0
  !! uctfps /24.0/ : cost of TF coil power supplies ($/W**0.7)
  real(kind(1.0D0)) :: uctfsw = 1.0D0
  !! uctfsw /1.0/ : cost of TF coil slow dump switches ($/A)
  real(kind(1.0D0)), parameter :: uctpmp = 1.105D5
  !! uctpmp /1.105e5/ FIX : cost of turbomolecular pump ($)
  real(kind(1.0D0)), parameter :: uctr = 370.0D0
  !! uctr /370.0/ FIX : cost of tritium building ($/m3)
  real(kind(1.0D0)), dimension(2), bind(C) :: ucturb = (/230.0D6, 245.0D6/)
  !! ucturb(2) /230.0e6, 245.0e6/: cost of turbine plant equipment ($)
  !!                              (dependent on coolant type coolwh)
  real(kind(1.0D0)), parameter :: ucvalv = 3.9D5
  !! ucvalv /3.9e5/ FIX : vacuum system valve cost ($)
  real(kind(1.0D0)), parameter :: ucvdsh = 26.0D0
  !! ucvdsh /26.0/ FIX : vacuum duct shield cost ($/kg)
  real(kind(1.0D0)), parameter :: ucviac = 1.3D6
  !! ucviac /1.3e6/ FIX : vacuum system instrumentation and control cost ($)
  real(kind(1.0D0)) :: ucwindpf = 465.0D0
  !! ucwindpf /465.0/ : cost of PF coil superconductor windings ($/m)
  real(kind(1.0D0)) :: ucwindtf = 480.0D0
  !! ucwindtf /480.0/ : cost of TF coil superconductor windings ($/m)
  real(kind(1.0D0)), parameter :: ucws = 460.0D0
  !! ucws /460.0/ FIX : cost of active assembly shop ($/m3)
  real(kind(1.0D0)), dimension(4) :: ucwst = &
  !! ucwst(4) /0.0,3.94,5.91,7.88/ : cost of waste disposal (M$/y/1200MW)
       (/0.0D0, 3.94D0, 5.91D0, 7.88D0/)

end module cost_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module constraint_variables

  !! Module containing global variables relating to the
  !! constraint equations
  !! This module contains global variables relating to the
  !! constraint equations (f-values, limits, etc.).
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: auxmin = 0.1D0
  !! auxmin /0.1/ : minimum auxiliary power (MW)
  !!                (constraint equation 40)
  real(kind(1.0D0)) :: betpmx = 0.19D0
  !! betpmx /0.19/ : maximum poloidal beta
  !!                 (constraint equation 48)
  real(kind(1.0D0)) :: bigqmin = 10.0D0
  !! bigqmin /10.0/ : minimum fusion gain Q
  !!                 (constraint equation 28)
  real(kind(1.0D0)) :: bmxlim = 12.0D0
  !! bmxlim /12.0/ : maximum peak toroidal field (T)
  !!                 (constraint equation 25)
  real(kind(1.0D0)) :: fauxmn = 1.0D0
  !! fauxmn /1.0/ : f-value for minimum auxiliary power
  !!                (constraint equation 40, iteration variable 64)
  real(kind(1.0D0)) :: fbeta = 1.0D0
  !! fbeta /1.0/ : f-value for epsilon beta-poloidal
  !!               (constraint equation 6, iteration variable 8)
  real(kind(1.0D0)) :: fbetap = 1.0D0
  !! fbetap /1.0/ : f-value for poloidal beta
  !!                (constraint equation 48, iteration variable 79)
  real(kind(1.0D0)) :: fbetatry = 1.0D0
  !! fbetatry /1.0/ : f-value for beta limit
  !!                  (constraint equation 24, iteration variable 36)
  real(kind(1.0D0)) :: fcpttf = 1.0D0
  !! fcpttf /1.0/ : f-value for TF coil current per turn upper limit
  !!              (constraint equation 77, iteration variable 146)
  real(kind(1.0D0)) :: fcwr = 1.0D0
  !! fcwr /1.0/ : f-value for conducting wall radius / rminor limit
  !!              (constraint equation 23, iteration variable 104)
  real(kind(1.0D0)) :: fdene = 1.0D0
  !! fdene /1.0/ : f-value for density limit
  !!               (constraint equation 5, iteration variable 9)
  !!               (invalid if ipedestal = 3)
  real(kind(1.0D0)) :: fdivcol = 1.0D0
  !! fdivcol /1.0/ : f-value for divertor collisionality
  !!                 (constraint equation 22, iteration variable 34)
  real(kind(1.0D0)) :: fdtmp = 1.0D0
  !! fdtmp /1.0/ : f-value for first wall coolant temperature rise
  !!               (constraint equation 38, iteration variable 62)
  real(kind(1.0D0)) :: fflutf = 1.0D0
  !! fflutf /1.0/ : f-value for neutron fluence on TF coil
  !!                 (constraint equation 53, iteration variable 92)
  real(kind(1.0D0)) :: ffuspow = 1.0D0
  !! ffuspow /1.0/ : f-value for maximum fusion power
  !!                 (constraint equation 9, iteration variable 26)
  real(kind(1.0D0)) :: fgamcd = 1.0D0
  !! fgamcd /1.0/ : f-value for current drive gamma
  !!                (constraint equation 37, iteration variable 40)
  real(kind(1.0D0)) :: fhldiv = 1.0D0
  !! fhldiv /1.0/ : f-value for divertor heat load
  !!                (constraint equation 18, iteration variable 27)
  real(kind(1.0D0)) :: fiooic = 0.5D0
  !! fiooic /0.5/ : f-value for TF coil operating current / critical
  !!                current ratio
  !!                (constraint equation 33, iteration variable 50)
  real(kind(1.0D0)) :: fipir = 1.0D0
  !! fipir /1.0/ : f-value for Ip/Irod limit
  !!               (constraint equation 46, iteration variable 72)
  real(kind(1.0D0)) :: fjohc = 1.0D0
  !! fjohc /1.0/ : f-value for central solenoid current at end-of-flattop
  !!               (constraint equation 26, iteration variable 38)
  real(kind(1.0D0)) :: fjohc0 = 1.0D0
  !! fjohc0 /1.0/ : f-value for central solenoid current at beginning of pulse
  !!                (constraint equation 27, iteration variable 39)
  real(kind(1.0D0)) :: fjprot = 1.0D0
  !! fjprot /1.0/ : f-value for TF coil winding pack current density
  !!                (constraint equation 35, iteration variable 53)
  real(kind(1.0D0)) :: flhthresh = 1.0D0
  !! flhthresh /1.0/ : f-value for L-H power threshold
  !!                   (constraint equation 15, iteration variable 103)
  real(kind(1.0D0)) :: fmva = 1.0D0
  !! fmva /1.0/ : f-value for maximum MVA
  !!              (constraint equation 19, iteration variable 30)
  real(kind(1.0D0)) :: fnbshinef = 1.0D0
  !! fnbshinef /1.0/ : f-value for maximum neutral beam shine-through fraction
  !!                   (constraint equation 59, iteration variable 105)
  real(kind(1.0D0)) :: fnesep = 1.0D0
  !! fnesep /1.0/ : f-value for Eich critical separatrix density
  !!                   (constraint equation 76, iteration variable 144)
  real(kind(1.0D0)) :: foh_stress = 1.0D0
  !! foh_stress /1.0/ : f-value for Tresca stress in Central Solenoid
  !!                   (constraint equation 72, iteration variable 123)
  real(kind(1.0D0)) :: fpeakb = 1.0D0
  !! fpeakb /1.0/ : f-value for maximum toroidal field
  !!                (constraint equation 25, iteration variable 35)
  real(kind(1.0D0)) :: fpinj = 1.0D0
  !! fpinj /1.0/ : f-value for injection power
  !!               (constraint equation 30, iteration variable 46)
  real(kind(1.0D0)) :: fpnetel = 1.0D0
  !! fpnetel /1.0/ : f-value for net electric power
  !!                 (constraint equation 16, iteration variable 25)
  real(kind(1.0D0)) :: fportsz = 1.0D0
  !! fportsz /1.0/ : f-value for neutral beam tangency radius limit
  !!                 (constraint equation 20, iteration variable 33)
  real(kind(1.0D0)) :: fpsepbqar = 1.0D0
  !! fpsepbqar /1.0/ : f-value for maximum Psep*Bt/qAR limit
  !!                (constraint equation 68, iteration variable 117)
  real(kind(1.0D0)) :: fpsepr = 1.0D0
  !! fpsepr /1.0/ : f-value for maximum Psep/R limit
  !!                (constraint equation 56, iteration variable 97)
  real(kind(1.0D0)) :: fptemp = 1.0D0
  !! fptemp /1.0/ : f-value for peak centrepost temperature
  !!                (constraint equation 44, iteration variable 68)
  real(kind(1.0D0)) :: fptfnuc = 1.0D0
  !! fptfnuc /1.0/ : f-value for maximum TF coil nuclear heating
  !!                 (constraint equation 54, iteration variable 95)
  real(kind(1.0D0)) :: fq = 1.0D0
  !! fq /1.0/ : f-value for edge safety factor
  !!            (constraint equation 45, iteration variable 71)
  real(kind(1.0D0)) :: fqval = 1.0D0
  !! fqval /1.0/ : f-value for Q
  !!               (constraint equation 28, iteration variable 45)
  real(kind(1.0D0)) :: fradpwr = 1.0D0
  !! fradpwr /1.0/ : f-value for core radiation power limit
  !!                 (constraint equation 17, iteration variable 28)
  real(kind(1.0D0)) :: fradwall = 1.0D0
  !! fradwall /1.0/ : f-value for upper limit on radiation wall load
  !!                  (constr. equ. 67, iteration variable 116 )
  real(kind(1.0D0)) :: freinke = 1.0D0
  !! freinke /1.0/ : f-value for Reinke detachment criterion
  !!                  (constr. equ. 78, iteration variable 147)
  real(kind(1.0D0)) :: frminor = 1.0D0
  !! frminor /1.0/ : f-value for minor radius limit
  !!                 (constraint equation 21, iteration variable 32)
  real(kind(1.0D0)) :: fstrcase = 1.0D0
  !! fstrcase /1.0/ : f-value for TF coil case stress
  !!                  (constraint equation 31, iteration variable 48)
  real(kind(1.0D0)) :: fstrcond = 1.0D0
  !! fstrcond /1.0/ : f-value for TF coil conduit stress
  !!                  (constraint equation 32, iteration variable 49)
  real(kind(1.0D0)) :: ftaucq = 1.0D0
  !! ftaucq /1.0/ : f-value for calculated minimum TF quench time
  !!                (constraint equation 65, iteration variable 113)
  real(kind(1.0D0)) :: ftbr = 1.0D0
  !! ftbr /1.0/ : f-value for minimum tritium breeding ratio
  !!                (constraint equation 52, iteration variable 89)
  real(kind(1.0D0)) :: ftburn = 1.0D0
  !! ftburn /1.0/ : f-value for minimum burn time
  !!                (constraint equation 13, iteration variable 21)
  real(kind(1.0D0)) :: ftcycl = 1.0D0
  !! ftcycl /1.0/ : f-value for cycle time
  !!                (constraint equation 42, iteration variable 67)
  real(kind(1.0D0)) :: ftmargoh = 1.0D0
  !! ftmargoh /1.0/ : f-value for central solenoid temperature margin
  !!                  (constraint equation 60, iteration variable 106)
  real(kind(1.0D0)) :: ftmargtf = 1.0D0
  !! ftmargtf /1.0/ : f-value for TF coil temperature margin
  !!                  (constraint equation 36, iteration variable 54)
  real(kind(1.0D0)) :: ftohs = 1.0D0
  !! ftohs /1.0/ : f-value for plasma current ramp-up time
  !!               (constraint equation 41, iteration variable 66)
  real(kind(1.0D0)) :: ftpeak = 1.0D0
  !! ftpeak /1.0/ : f-value for first wall peak temperature
  !!                (constraint equation 39, iteration variable 63)
  real(kind(1.0D0)) :: fvdump = 1.0D0
  !! fvdump /1.0/ : f-value for dump voltage
  !!                (constraint equation 34, iteration variable 51)
  real(kind(1.0D0)) :: fvs = 1.0D0
  !! fvs /1.0/ : f-value for flux-swing (V-s) requirement (STEADY STATE)
  !!             (constraint equation 12, iteration variable 15)
  real(kind(1.0D0)) :: fvvhe = 1.0D0
  !! fvvhe /1.0/ : f-value for vacuum vessel He concentration limit
  !!               (iblanket = 2)
  !!               (constraint equation 55, iteration variable 96)
  real(kind(1.0D0)) :: fwalld = 1.0D0
  !! fwalld /1.0/ : f-value for maximum wall load
  !!                (constraint equation 8, iteration variable 14)
  real(kind(1.0D0)) :: fzeffmax = 1.0D0
  !! fzeffmax /1.0/ : f-value for maximum zeff
  !!                (constraint equation 64, iteration variable 112)
  real(kind(1.0D0)) :: gammax = 2.0D0
  !! gammax /2.0/ : maximum current drive gamma
  !!                (constraint equation 37)
  real(kind(1.0D0)) :: maxradwallload = 1.0D0
  !!maxradwallload /1.0/ :  Maximum permitted radiation wall load (MW/m^2)
  !!                        (constraint equation 67)
  real(kind(1.0D0)) :: mvalim = 40.0D0
  !! mvalim /40.0/ : maximum MVA limit
  !!                 (constraint equation 19)
  real(kind(1.0D0)) :: nbshinefmax = 1.0D-3
  !! nbshinefmax /1.0e-3/ : maximum neutral beam shine-through fraction
  !!                        (constraint equation 59)
  real(kind(1.0D0)) :: nflutfmax = 1.0D23
  !! nflutfmax /1.0e23/ : max fast neutron fluence on TF coil (n/m2)
  !!                     (blktmodel>0)
  !!                     (constraint equation 53)
  real(kind(1.0D0)) :: pdivtlim = 150.0D0
  !! pdivtlim /150.0/  : Minimum pdivt [MW] (constraint equation 80)
  real(kind(1.0D0)) :: peakfactrad = 3.33D0
  !! peakfactrad /3.33/  : peaking factor for radiation wall load
  !!                      (constraint equation 67)
  real(kind(1.0D0)) :: peakradwallload = 0.0D0
  !! peakradwallload : Peak radiation wall load (MW/m^2)
  !!                       (constraint equation 67)
  real(kind(1.0D0)) :: pnetelin = 1.0D3
  !! pnetelin /1000.0/ : required net electric power (MW)
  !!                     (constraint equation 16)
  real(kind(1.0D0)) :: powfmax = 1.5D3
  !! powfmax /1500.0/ : maximum fusion power (MW)
  !!                    (constraint equation 9)
  real(kind(1.0D0)) :: psepbqarmax = 9.5D0
  !! psepbqarmax /9.5/ : maximum ratio of Psep*Bt/qAR (MWT/m)
  !!                     (constraint equation 68)
  real(kind(1.0D0)) :: pseprmax = 25.0D0
  !! pseprmax /25.0/ : maximum ratio of power crossing the separatrix to
  !!                     plasma major radius (Psep/R) (MW/m)
  !!                     (constraint equation 56)
  real(kind(1.0D0)) :: ptfnucmax = 1.0D-3
  !! ptfnucmax /1.0e-3/ : maximum nuclear heating in TF coil (MW/m3)
  !!                      (constraint equation 54)
  real(kind(1.0D0)) :: tbrmin = 1.1D0
  !! tbrmin /1.1/ : minimum tritium breeding ratio
  !!                (constraint equation 52)
  real(kind(1.0D0)) :: tbrnmn = 1.0D0
  !! tbrnmn /1.0/ : minimum burn time (s)
  !!                (KE - no longer itv., see issue 706)
  real(kind(1.0D0)) :: tcycmn = 0.0D0
  !! tcycmn : minimum cycle time (s)
  !!          (constraint equation 42)
  real(kind(1.0D0)) :: tohsmn = 1.0D0
  !! tohsmn : minimum plasma current ramp-up time (s)
  !!          (constraint equation 41)
  real(kind(1.0D0)) :: vvhealw = 1.0D0
  !! vvhealw /1.0/ : allowed maximum helium concentration in vacuum vessel
  !!                 at end of plant life (appm) (iblanket =2)
  !!                 (constraint equation 55)
  real(kind(1.0D0)) :: walalw = 1.0D0
  !! walalw /1.0/ : allowable wall-load (MW/m2)
  !!                (constraint equation 8)

  real(kind(1.0D0)) :: taulimit = 5.0D0
  !! taulimit /5.0/ : Lower limit on taup/taueff the ratio of alpha particle
  !!                 to energy confinement times (constraint equation 62)
  real(kind(1.0D0)) :: ftaulimit = 1.0D0
  !! ftaulimit /1.0/ : f-value for lower limit on taup/taueff the ratio
  !!                   of alpha particle to energy confinement times
  !!                  (constraint equation 62, iteration variable 110)

  real(kind(1.0D0)) :: fniterpump = 1.0D0
  !! fniterpump /1.0/ : f-value for constraint that number of pumps < tfno
  !!                  (constraint equation 63, iteration variable 111)
  real(kind(1.0D0)) :: zeffmax = 3.6D0
  !! zeffmax /3.6/ : maximum value for Zeff
  !!                  (constraint equation 64)
  real(kind(1.0D0)) :: fpoloidalpower = 1.0D0
  !! fpoloidalpower /1.0/ : f-value for constraint on rate of change of energy in poloidal field
  !!                  (constraint equation 66, iteration variable 115)

  real(kind(1.0D0)) :: fpsep = 1.0D0
  !! fpsep /1.0/ : f-value to ensure separatrix power is less than value from Kallenbach divertor
  !!                  (Not required as constraint 69 is an equality)

  real(kind(1.0D0)) :: fcqt = 1.0D0
  !! fcqt /1.0/ : f-value: TF coil quench temparature remains below tmax_croco
  !!                  (constraint equation 74, iteration variable 141)


end module constraint_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module stellarator_variables

  !! Module containing global variables relating to the
  !! stellarator model
  !! This module contains global variables relating to the
  !! stellarator model.
  !! Stellarator Plasma Geometry Model for the Systems
  !! Code PROCESS, F. Warmer, 19/06/2013
  !! Stellarator Divertor Model for the Systems
  !! Code PROCESS, F. Warmer, 21/06/2013
  !! Stellarator Coil Model for the Systems
  !! Code PROCESS, F. Warmer and F. Schauer, 07/10/2013
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  integer :: istell = 0
  !! istell /0/ : switch for stellarator option
  !!              (set via <CODE>device.dat</CODE>):<UL>
  !!         <LI> = 0 use tokamak model;
  !!         <LI> = 1 use stellarator model</UL>

  real(kind(1.0D0)) :: bmn = 1.0D-3
  !! bmn /0.001/ : relative radial field perturbation
  real(kind(1.0D0)) :: f_asym = 1.0D0
  !! f_asym /1.0/ : divertor heat load peaking factor
  real(kind(1.0D0)) :: f_rad = 0.85D0
  !! f_rad /0.85/ : radiated power fraction in SOL
  real(kind(1.0D0)) :: f_w = 0.5D0
  !! f_w /0.5/ : island size fraction factor
  real(kind(1.0D0)) :: fdivwet = 0.33333333333333333333333333333D0
  !! fdivwet /0.3333/ : wetted fraction of the divertor area
  real(kind(1.0D0)) :: flpitch = 1.0D-3
  !! flpitch /0.001/ : field line pitch (rad)
  real(kind(1.0D0)) :: hportamax = 0.0D0
  !! hportamax : maximum available area for horizontal ports (m2)
  real(kind(1.0D0)) :: hportpmax = 0.0D0
  !! hportpmax : maximum available poloidal extent for horizontal ports (m)
  real(kind(1.0D0)) :: hporttmax = 0.0D0
  !! hporttmax : maximum available toroidal extent for horizontal ports (m)
  real(kind(1.0D0)) :: iotabar = 1.0D0
  !! iotabar /1.0/ : rotational transform (reciprocal of tokamak q)
  !!                 for stellarator confinement time scaling laws
  integer :: isthtr = 3
  !! isthtr /3/ : switch for stellarator auxiliary heating method:<UL>
  !!         <LI> = 1 electron cyclotron resonance heating;
  !!         <LI> = 2 lower hybrid heating;
  !!         <LI> = 3 neutral beam injection</UL>
  integer :: m_res = 5
  !! m_res /5/ : poloidal resonance number
  integer :: n_res = 5
  !! n_res /5/ : toroidal resonance number
  real(kind(1.0D0)) :: shear = 0.5D0
  !! shear /0.5/ : magnetic shear, derivative of iotabar
  character(len=48) :: vmec_info_file = 'vmec_info.dat'
  !! vmec_info_file /vmec_info.dat/ : file containing general VMEC settings
  character(len=48) :: vmec_rmn_file = 'vmec_Rmn.dat'
  !! vmec_rmn_file /vmec_Rmn.dat/ : file containing plasma boundary R(m,n)
  !!                                Fourier components
  character(len=48) :: vmec_zmn_file = 'vmec_Zmn.dat'
  !! vmec_zmn_file /vmec_Zmn.dat/ : file containing plasma boundary Z(m,n)
  !!                                Fourier components
  real(kind(1.0D0)) :: vportamax = 0.0D0
  !! vportamax : maximum available area for vertical ports (m2)
  real(kind(1.0D0)) :: vportpmax = 0.0D0
  !! vportpmax : maximum available poloidal extent for vertical ports (m)
  real(kind(1.0D0)) :: vporttmax = 0.0D0
  !! vporttmax : maximum available toroidal extent for vertical ports (m)

end module stellarator_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Issue #508 Remove RFP option: module rfp_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module ife_variables

     !! Module containing global variables relating to the
     !! inertial fusion energy model
     !! This module contains global variables relating to the
     !! inertial fusion energy model.
     !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
     implicit none
   
     public
   
   
     !! Default IFE builds and material volumes are those for the SOMBRERO device.
     !! The 2-dimensional arrays have indices (region, material), where 'region'
     !! is the region and maxmat is the 'material'<UL>
     !! <LI>'region' = 1 radially outside chamber
     !! <LI>         = 2 above chamber
     !! <LI>         = 3 below chamber</UL>
     integer, parameter ::  maxmat = 8
     !! maxmat /8/ FIX : total number of materials in IFE device.
     !!                  Material numbers are as follows:<UL>
     !!             <LI> = 0 void;
     !!             <LI> = 1 steel;
     !!             <LI> = 2 carbon cloth;
     !!             <LI> = 3 FLiBe;
     !!             <LI> = 4 lithium oxide Li2O;
     !!             <LI> = 5 concrete;
     !!             <LI> = 6 helium;
     !!             <LI> = 7 xenon;
     !!             <LI> = 8 lithium </UL>
   
     real(kind(1.0D0)) :: bldr   = 1.0D0
     !! bldr /1.0/ : radial thickness of IFE blanket (m; calculated if ifetyp=4)
     real(kind(1.0D0)) :: bldrc   = 1.0D0
     !! bldrc /1.0/ : radial thickness of IFE curtain (m; ifetyp=4)
     real(kind(1.0D0)) :: bldzl  = 4.0D0
     !! bldzl /4.0/ : vertical thickness of IFE blanket below chamber (m)
     real(kind(1.0D0)) :: bldzu  = 4.0D0
     !! bldzu /4.0/ : vertical thickness of IFE blanket above chamber (m)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: blmatf = reshape( (/ &
     !! blmatf(3,0:maxmat) /.../ : IFE blanket material fractions
          0.05D0,0.05D0,0.05D0, &
          0.0D0, 0.0D0, 0.0D0,  &
          0.45D0,0.45D0,0.45D0, &
          0.0D0, 0.0D0, 0.0D0,  &
          0.20D0,0.20D0,0.20D0, &
          0.0D0, 0.0D0, 0.0D0,  &
          0.30D0,0.30D0,0.30D0, &
          0.0D0,0.0D0,0.0D0,    &
          0.0D0, 0.0D0, 0.0D0  /), shape(blmatf))
     real(kind(1.0D0)), dimension(3,0:maxmat) :: blmatm = 0.0D0
     !! blmatm(3,0:maxmat) : IFE blanket material masses (kg)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: blmatv = 0.0D0
     !! blmatv(3,0:maxmat) : IFE blanket material volumes (m3)
     real(kind(1.0D0)), dimension(3) :: blvol = 0.0D0
     !! blvol(3) : IFE blanket volume (m3)
     real(kind(1.0D0)) :: cdriv0 = 154.3D0
     !! cdriv0 /154.3/ : IFE generic/laser driver cost at edrive=0 (M$)
     real(kind(1.0D0)) :: cdriv1 = 163.2D0
     !! cdriv1 /163.2/ : IFE low energy heavy ion beam driver cost
     !!                  extrapolated to edrive=0 (M$)
     real(kind(1.0D0)) :: cdriv2 = 244.9D0
     !! cdriv2 /244.9/ : IFE high energy heavy ion beam driver cost
     !!                  extrapolated to edrive=0 (M$)
     real(kind(1.0D0)) :: cdriv3 = 1.463D0
     !! cdriv3 /1.463/ : IFE driver cost ($/J wall plug) (ifedrv==3)
     real(kind(1.0D0)) :: chdzl = 9.0D0
     !! chdzl /9.0/ : vertical thickness of IFE chamber below centre (m)
     real(kind(1.0D0)) :: chdzu = 9.0D0
     !! chdzu /9.0/ : vertical thickness of IFE chamber above centre (m)
     real(kind(1.0D0)), dimension(0:maxmat) :: chmatf = &
     !! chmatf(0:maxmat) : IFE chamber material fractions
          (/1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/)
     real(kind(1.0D0)), dimension(0:maxmat) :: chmatm = 0.0D0
     !! chmatm(0:maxmat) : IFE chamber material masses (kg)
     real(kind(1.0D0)), dimension(0:maxmat) :: chmatv = 0.0D0
     !! chmatv(0:maxmat) : IFE chamber material volumes (m3)
     real(kind(1.0D0)) :: chrad = 6.5D0
     !! chrad /6.5/ : radius of IFE chamber (m)
     !!               (iteration variable 84)
     real(kind(1.0D0)) :: chvol = 0.0D0
     !! chvol : IFE chamber volume (m3)
     real(kind(1.0D0)) :: dcdrv0 = 111.4D0
     !! dcdrv0 /111.4/ : IFE generic/laser driver cost gradient (M$/MJ)
     real(kind(1.0D0)) :: dcdrv1 = 78.0D0
     !! dcdrv1 /78.0/ : HIB driver cost gradient at low energy (M$/MJ)
     real(kind(1.0D0)) :: dcdrv2 = 59.9D0
     !! dcdrv2 /59.9/ : HIB driver cost gradient at high energy (M$/MJ)
     real(kind(1.0D0)) :: drveff = 0.28D0
     !! drveff /0.28/ : IFE driver wall plug to target efficiency (ifedrv=0,3)
     !!                 (iteration variable 82)
     real(kind(1.0D0)) :: edrive = 5.0D6
     !! edrive /5.0D6/ : IFE driver energy (J)
     !!                  (iteration variable 81)
     real(kind(1.0D0)) :: etadrv = 0.0D0
     !! etadrv : IFE driver wall plug to target efficiency
     real(kind(1.0D0)) :: etali = 0.4D0
     !! etali /0.40/ : IFE lithium pump wall plug efficiency (ifetyp=4)
     real(kind(1.0D0)), dimension(10) :: etave = (/ &
      0.082D0,0.079D0,0.076D0,0.073D0,0.069D0, &
      0.066D0,0.062D0,0.059D0,0.055D0,0.051D0 /)
     !! etave(10) : IFE driver efficiency vs driver energy (ifedrv=-1)
     real(kind(1.0D0)) :: fauxbop = 0.06D0
     !! fauxbop /0.06/ : fraction of gross electric power to balance-of-plant (IFE)
     real(kind(1.0D0)) :: fbreed = 0.51D0
     !! fbreed /0.51/ : fraction of breeder external to device core
     real(kind(1.0D0)) :: fburn  = 0.3333D0
     !! fburn /0.3333/ : IFE burn fraction (fraction of tritium fused/target)
     real(kind(1.0D0)) :: flirad = 0.78D0
     !! flirad /0.78/ : radius of FLiBe/lithium inlet (m) (ifetyp=3,4)
     real(kind(1.0D0)) :: frrmax = 1.0D0
     !! frrmax /1.0/ : f-value for maximum IFE repetition rate
     !!                (constraint equation 50, iteration variable 86)
     real(kind(1.0D0)) :: fwdr = 0.01D0
     !! fwdr /0.01/ : radial thickness of IFE first wall (m)
     real(kind(1.0D0)) :: fwdzl = 0.01D0
     !! fwdzl /0.01/ : vertical thickness of IFE first wall below chamber (m)
     real(kind(1.0D0)) :: fwdzu = 0.01D0
     !! fwdzu /0.01/ : vertical thickness of IFE first wall above chamber (m)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: fwmatf = reshape( (/ &
     !! fwmatf(3,0:maxmat) /.../ : IFE first wall material fractions
          0.05D0,0.05D0,0.05D0, &
          0.0D0, 0.0D0, 0.0D0,  &
          0.95D0,0.95D0,0.95D0, &
          0.0D0, 0.0D0, 0.0D0,  &
          0.0D0, 0.0D0, 0.0D0,  &
          0.0D0, 0.0D0, 0.0D0,  &
          0.0D0, 0.0D0, 0.0D0,  &
          0.0D0, 0.0D0, 0.0D0,  &
          0.0D0, 0.0D0, 0.0D0  /), shape(fwmatf))
     real(kind(1.0D0)), dimension(3,0:maxmat) :: fwmatm = 0.0D0
     !! fwmatm(3,0:maxmat) : IFE first wall material masses (kg)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: fwmatv = 0.0D0
     !! fwmatv(3,0:maxmat) : IFE first wall material volumes (kg)
     real(kind(1.0D0)), dimension(3) :: fwvol = 0.0D0
     !! fwvol(3) : IFE first wall volume (m3)
     real(kind(1.0D0)) :: gain = 0.0D0
     !! gain : IFE target gain
     real(kind(1.0D0)), dimension(10) :: gainve = (/ &
     !! gainve(10) /.../ : IFE target gain vs driver energy (ifedrv=-1)
           60.0D0, 95.0D0,115.0D0,125.0D0,133.0D0, &
          141.0D0,152.0D0,160.0D0,165.0D0,170.0D0 /)
     real(kind(1.0D0)) :: htpmw_ife = 0.0D0         
     !! htpmw_ife /0.0/ : IFE heat transport system electrical pump power (MW)
     integer :: ife = 0
     !! ife /0/ : switch for IFE option
     !!           (set via <CODE>device.dat</CODE>):<UL>
     !!      <LI> = 0 use tokamak, RFP or stellarator model;
     !!      <LI> = 1 use IFE model</UL>
     integer :: ifedrv = 2
     !! ifedrv /2/ : switch for type of IFE driver:<UL>
     !!         <LI> = -1 use gainve, etave for gain and driver efficiency;
     !!         <LI> =  0 use tgain, drveff for gain and driver efficiency;
     !!         <LI> =  1 use laser driver based on SOMBRERO design;
     !!         <LI> =  2 use heavy ion beam driver based on OSIRIS;
     !!         <LI> =  3 Input pfusife, rrin and drveff</UL>
     integer :: ifetyp = 0
     !! ifetyp /0/ : switch for type of IFE device build:<UL>
     !!         <LI> = 0 generic (cylindrical) build;
     !!         <LI> = 1 OSIRIS-like build;
     !!         <LI> = 2 SOMBRERO-like build;
     !!         <LI> = 3 HYLIFE-II-like build;
     !!         <LI> = 4 2019 build</UL>
     real(kind(1.0D0)) :: lipmw = 0.0D0
     !! lipmw : IFE lithium pump power (MW; ifetyp=4)
     real(kind(1.0D0)) :: mcdriv = 1.0D0
     !! mcdriv /1.0/ : IFE driver cost multiplier
     real(kind(1.0D0)) :: mflibe = 0.0D0
     !! mflibe : total mass of FLiBe (kg)
     real(kind(1.0D0)) :: pdrive = 23.0D6
     !! pdrive /23.0D6/ : IFE driver power reaching target (W)
     !!                   (iteration variable 85)
     real(kind(1.0D0)) :: pfusife = 1000.0D0
     !! pfusife /1000.0/ : IFE input fusion power (MW) (ifedrv=3 only; itv 155)
     real(kind(1.0D0)) :: pifecr = 10.0D0
     !! pifecr /10.0/ : IFE cryogenic power requirements (MW)
     real(kind(1.0D0)), bind(C) :: ptargf = 2.0D0
     !! ptargf /2.0/ : IFE target factory power at 6 Hz repetition rate (MW)
     real(kind(1.0D0)) :: r1 = 0.0D0
     !! r1 : IFE device radial build (m)
     real(kind(1.0D0)) :: r2 = 0.0D0
     !! r2 : IFE device radial build (m)
     real(kind(1.0D0)) :: r3 = 0.0D0
     !! r3 : IFE device radial build (m)
     real(kind(1.0D0)) :: r4 = 0.0D0
     !! r4 : IFE device radial build (m)
     real(kind(1.0D0)) :: r5 = 0.0D0
     !! r5 : IFE device radial build (m)
     real(kind(1.0D0)) :: r6 = 0.0D0
     !! r6 : IFE device radial build (m)
     real(kind(1.0D0)) :: r7 = 0.0D0
     !! r7 : IFE device radial build (m)
     real(kind(1.0D0)), bind(C) :: reprat = 0.0D0
     !! reprat : IFE driver repetition rate (Hz)
     real(kind(1.0D0)) :: rrin = 6.0D0
     !! rrin /6.0/ : Input IFE repetition rate (Hz) (ifedrv=3 only; itv 156)
     real(kind(1.0D0)) :: rrmax = 20.0D0
     !! rrmax /20.0/ : maximum IFE repetition rate (Hz)
     real(kind(1.0D0)) :: shdr = 1.7D0
     !! shdr /1.7/ : radial thickness of IFE shield (m)
     real(kind(1.0D0)) :: shdzl = 5.0D0
     !! shdzl /5.0/ : vertical thickness of IFE shield below chamber (m)
     real(kind(1.0D0)) :: shdzu  = 5.0D0
     !! shdzu /5.0/ : vertical thickness of IFE shield above chamber (m)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: shmatf = reshape( (/ &
     !! shmatf(3,0:maxmat) /.../ : IFE shield material fractions
          0.05D0,0.05D0,0.05D0, &
          0.19D0,0.19D0,0.19D0, &
          0.0D0, 0.0D0, 0.0D0,  &
          0.0D0, 0.0D0, 0.0D0,  &
          0.0D0, 0.0D0, 0.0D0,  &
          0.665D0,0.665D0,0.665D0, &
          0.095D0,0.095D0,0.095D0, &
          0.0D0, 0.0D0, 0.0D0,  &
          0.0D0, 0.0D0, 0.0D0  /), shape(shmatf))
     real(kind(1.0D0)), dimension(3,0:maxmat) :: shmatm = 0.0D0
     !! shmatm(3,0:maxmat) : IFE shield material masses (kg)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: shmatv = 0.0D0
     !! shmatv(3,0:maxmat) : IFE shield material volumes (kg)
     real(kind(1.0D0)), dimension(3) :: shvol = 0.0D0
     !! shvol(3) : IFE shield volume (m3)
     real(kind(1.0D0)) :: sombdr = 2.7D0
     !! sombdr /2.7/ : radius of cylindrical blanket section below chamber (ifetyp=2)
     real(kind(1.0D0)) :: somtdr = 2.7D0
     !! somtdr /2.7/ : radius of cylindrical blanket section above chamber (ifetyp=2)
     real(kind(1.0D0)) :: taufall = 0.0D0
     !! taufall : Lithium Fall Time (s)
     real(kind(1.0D0)) :: tdspmw = 0.01D0
     !! tdspmw /0.01/ FIX : IFE target delivery system power (MW)
     real(kind(1.0D0)), bind(C) :: tfacmw = 0.0D0
     !! tfacmw : IFE target factory power (MW)
     real(kind(1.0D0)) :: tgain = 85.0D0
     !! tgain /85.0/ : IFE target gain (if ifedrv = 0)
     !!                (iteration variable 83)
     real(kind(1.0D0)) :: uccarb = 50.0D0
     !! uccarb /50.0/ : cost of carbon cloth ($/kg)
     real(kind(1.0D0)) :: ucconc = 0.1D0
     !! ucconc /0.1/ : cost of concrete ($/kg)
     real(kind(1.0D0)) :: ucflib = 84.0D0
     !! ucflib /84.0/ : cost of FLiBe ($/kg)
     real(kind(1.0D0)) :: uctarg = 0.3D0
     !! uctarg /0.3/ : cost of IFE target ($/target)
     real(kind(1.0D0)) :: v1dr = 0.0D0
     !! v1dr /0.0/ : radial thickness of IFE void between first wall and blanket (m)
     real(kind(1.0D0)) :: v1dzl = 0.0D0
     !! v1dzl /0.0/ : vertical thickness of IFE void 1 below chamber (m)
     real(kind(1.0D0)) :: v1dzu = 0.0D0
     !! v1dzu /0.0/ : vertical thickness of IFE void 1 above chamber (m)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v1matf = reshape( (/ &
     !! v1matf(3,0:maxmat) /.../ : IFE void 1 material fractions
          1.0D0, 1.0D0, 1.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0  /), shape(v1matf))
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v1matm = 0.0D0
     !! v1matm(3,0:maxmat) : IFE void 1 material masses (kg)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v1matv = 0.0D0
     !! v1matv(3,0:maxmat) : IFE void 1 material volumes (kg)
     real(kind(1.0D0)), dimension(3) :: v1vol = 0.0D0
     !! v1vol(3) : IFE void 1 volume (m3)
     real(kind(1.0D0)) :: v2dr = 2.0D0
     !! v2dr /2.0/ : radial thickness of IFE void between blanket and shield (m)
     real(kind(1.0D0)) :: v2dzl = 7.0D0
     !! v2dzl /7.0/ : vertical thickness of IFE void 2 below chamber (m)
     real(kind(1.0D0)) :: v2dzu = 7.0D0
     !! v2dzu /7.0/ : vertical thickness of IFE void 2 above chamber (m)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v2matf = reshape( (/ &
     !! v2matf(3,0:maxmat) /.../ : IFE void 2 material fractions
          1.0D0, 1.0D0, 1.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0  /), shape(v2matf))
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v2matm = 0.0D0
     !! v2matm(3,0:maxmat) : IFE void 2 material masses (kg)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v2matv = 0.0D0
     !! v2matv(3,0:maxmat) : IFE void 2 material volumes (kg)
     real(kind(1.0D0)), dimension(3) :: v2vol = 0.0D0
     !! v2vol(3) : IFE void 2 volume (m3)
     real(kind(1.0D0)) :: v3dr   = 43.3D0
     !! v3dr /43.3/ : radial thickness of IFE void outside shield (m)
     real(kind(1.0D0)) :: v3dzl  = 30.0D0
     !! v3dzl /30.0/ : vertical thickness of IFE void 3 below chamber (m)
     real(kind(1.0D0)) :: v3dzu  = 20.0D0
     !! v3dzu /20.0/ : vertical thickness of IFE void 3 above chamber (m)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v3matf = reshape( (/ &
     !! v3matf(3,0:maxmat) /.../ : IFE void 3 material fractions
          1.0D0, 1.0D0, 1.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0, &
          0.0D0, 0.0D0, 0.0D0  /), shape(v3matf))
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v3matm = 0.0D0
     !! v3matm(3,0:maxmat) : IFE void 3 material masses (kg)
     real(kind(1.0D0)), dimension(3,0:maxmat) :: v3matv = 0.0D0
     !! v3matv(3,0:maxmat) : IFE void 3 material volumes (kg)
     real(kind(1.0D0)), dimension(3) :: v3vol = 0.0D0
     !! v3vol(3) : IFE void 3 volume (m3)
     real(kind(1.0D0)) :: zl1 = 0.0D0
     !! zl1 : IFE vertical build below centre (m)
     real(kind(1.0D0)) :: zl2 = 0.0D0
     !! zl2 : IFE vertical build below centre (m)
     real(kind(1.0D0)) :: zl3 = 0.0D0
     !! zl3 : IFE vertical build below centre (m)
     real(kind(1.0D0)) :: zl4 = 0.0D0
     !! zl4 : IFE vertical build below centre (m)
     real(kind(1.0D0)) :: zl5 = 0.0D0
     !! zl5 : IFE vertical build below centre (m)
     real(kind(1.0D0)) :: zl6 = 0.0D0
     !! zl6 : IFE vertical build below centre (m)
     real(kind(1.0D0)) :: zl7 = 0.0D0
     !! zl7 : IFE vertical build below centre (m)
     real(kind(1.0D0)) :: zu1 = 0.0D0
     !! zu1 : IFE vertical build above centre (m)
     real(kind(1.0D0)) :: zu2 = 0.0D0
     !! zu2 : IFE vertical build above centre (m)
     real(kind(1.0D0)) :: zu3 = 0.0D0
     !! zu3 : IFE vertical build above centre (m)
     real(kind(1.0D0)) :: zu4 = 0.0D0
     !! zu4 : IFE vertical build above centre (m)
     real(kind(1.0D0)) :: zu5 = 0.0D0
     !! zu5 : IFE vertical build above centre (m)
     real(kind(1.0D0)) :: zu6 = 0.0D0
     !! zu6 : IFE vertical build above centre (m)
     real(kind(1.0D0)) :: zu7 = 0.0D0
     !! zu7 : IFE vertical build above centre (m)
   
   end module ife_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pulse_variables

  !! Module containing global variables relating to the
  !! pulsed reactor model
  !! This module contains global variables relating to the
  !! pulsed reactor model.
  !! Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: bctmp = 320.0D0
  !! bctmp /320.0/ : first wall bulk coolant temperature (C)
  real(kind(1.0D0)) :: bfw = 0.0D0
  !! bfw : outer radius of each first wall structural tube (m)
  !!       (0.5 * average of fwith and fwoth)
  real(kind(1.0D0)) :: dtstor = 300.0D0
  !! dtstor /300.0/ : maximum allowable temperature change in stainless
  !!                  steel thermal storage block (K) (istore=3)
  integer :: istore = 1
  !! istore /1/ : switch for thermal storage method:<UL>
  !!         <LI> = 1 option 1 of Electrowatt report, AEA FUS 205;
  !!         <LI> = 2 option 2 of Electrowatt report, AEA FUS 205;
  !!         <LI> = 3 stainless steel block</UL>
  integer :: itcycl = 1
  !! itcycl /1/ : switch for first wall axial stress model:<UL>
  !!         <LI> = 1 total axial constraint, no bending;
  !!         <LI> = 2 no axial constraint, no bending;
  !!         <LI> = 3 no axial constraint, bending</UL>
  integer :: lpulse = 0
  !! lpulse /0/ : switch for reactor model:<UL>
  !!         <LI> = 0 continuous operation;
  !!         <LI> = 1 pulsed operation</UL>

end module pulse_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module startup_variables

  !! Module containing global variables relating to the
  !! plasma start-up model
  !! This module contains global variables relating to the
  !! plasma start-up model.
  !! Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  real(kind(1.0D0)) :: ftaue = 0.0D0
  !! ftaue : factor in energy confinement time formula
  real(kind(1.0D0)) :: gtaue  = 0.0D0
  !! gtaue : offset term in energy confinement time scaling
  real(kind(1.0D0)) :: nign  = 0.0D0
  !! nign : electron density at ignition (start-up) (/m3)
  real(kind(1.0D0)) :: ptaue  = 0.0D0
  !! ptaue : exponent for density term in energy confinement time formula
  real(kind(1.0D0)) :: qtaue  = 0.0D0
  !! qtaue : exponent for temperature term in energy confinement time formula
  real(kind(1.0D0)) :: rtaue  = 0.0D0
  !! rtaue : exponent for power term in energy confinement time formula
  real(kind(1.0D0)) :: tign  = 0.0D0
  !! tign : electron temperature at ignition (start-up) (keV)

end module startup_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module fispact_variables

  !! Module containing global variables relating to the
  !! fispact routines
  !! This module contains global variables relating to the
  !! nuclear data (fispact) routines.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public


  !! Fispact arrays with 3 elements contain the results at the following times:
  !!   (1) - at end of component life
  !!   (2) - after 3 months cooling time
  !!   (3) - 100 years after end of plant life
  real(kind(1.0D0)), dimension(3) :: bliact = 0.0D0
  !! bliact(3) : inboard blanket total activity (Bq)
  real(kind(1.0D0)), dimension(3) :: bligdr = 0.0D0
  !! bligdr(3) : inboard blanket total gamma dose rate (Sv/hr)
  real(kind(1.0D0)), dimension(3) :: blihkw = 0.0D0
  !! blihkw(3) : inboard blanket total heat output (kW)
  real(kind(1.0D0)) :: bliizp = 0.0D0
  !! bliizp : inboard blanket integrated zone power / neutron
  real(kind(1.0D0)) :: blimzp = 0.0D0
  !! blimzp : inboard blanket mean zone power density / neutron
  real(kind(1.0D0)), dimension(3) :: bloact = 0.0D0
  !! bloact(3) : outboard blanket total activity (Bq)
  real(kind(1.0D0)), dimension(3) :: blogdr = 0.0D0
  !! blogdr(3) : outboard blanket total gamma dose rate (Sv/hr)
  real(kind(1.0D0)), dimension(3) :: blohkw = 0.0D0
  !! blohkw(3) : outboard blanket total heat output (kW)
  real(kind(1.0D0)) :: bloizp = 0.0D0
  !! bloizp : outboard blanket integrated zone power / neutron
  real(kind(1.0D0)) :: blomzp = 0.0D0
  !! blomzp : outboard blanket mean zone power density / neutron
  real(kind(1.0D0)), dimension(3) :: fwiact = 0.0D0
  !! fwiact(3) : inboard first wall total activity (Bq)
  real(kind(1.0D0)), dimension(3) :: fwigdr = 0.0D0
  !! fwigdr(3) : inboard first wall total gamma dose rate (Sv/hr)
  real(kind(1.0D0)), dimension(3) :: fwihkw = 0.0D0
  !! fwihkw(3) : inboard first wall total heat output (kW)
  real(kind(1.0D0)) :: fwiizp = 0.0D0
  !! fwiizp : inboard first wall integrated zone power / neutron
  real(kind(1.0D0)) :: fwimzp = 0.0D0
  !! fwimzp : inboard first wall mean zone power density/neutron
  real(kind(1.0D0)), dimension(3) :: fwoact = 0.0D0
  !! fwoact(3) : outboard first wall total activity (Bq)
  real(kind(1.0D0)), dimension(3) :: fwogdr = 0.0D0
  !! fwogdr(3) : outboard first wall total gamma dose rate (Sv/hr)
  real(kind(1.0D0)), dimension(3) :: fwohkw = 0.0D0
  !! fwohkw(3) : outboard first wall total heat output (kW)
  real(kind(1.0D0)) :: fwoizp = 0.0D0
  !! fwoizp : outboard first wall integrated zone power / neutron
  real(kind(1.0D0)) :: fwomzp = 0.0D0
  !! fwomzp : outboard first wall mean zone power density/neutron
  real(kind(1.0D0)) :: fwtemp = 0.0D0
  !! fwtemp : outboard first wall temperature after a LOCA (K)

end module fispact_variables

!------------------------------------------------------------------------

module rebco_variables

  !! Variables relating to the REBCO HTS tape, strand and conductor
  !! Conduit information is in the modules relating to each coil.
  !! TODO
  implicit none ! ---------------------------------------------------------
  ! Updated 13/11/18 using data from Lewandowska et al 2018.

  real(kind(1.0D0)) :: rebco_thickness = 1.0D-6
  !! rebco_thickness /1.0e-6/ : thickness of REBCO layer in tape (m) (iteration variable 138)
  real(kind(1.0D0)) :: copper_thick = 100.0D-6
  !! copper_thick /100e-6/ : thickness of copper layer in tape (m) (iteration variable 139)
  real(kind(1.0D0)) :: hastelloy_thickness = 50.0D-6
  !! hastelloy_thickness /50/e-6 : thickness of Hastelloy layer in tape (m)
  real(kind(1.0D0)) :: tape_width = 0.0D0
  !! tape_width : Mean width of tape (m)

  real(kind(1.0D0)) :: croco_od = 0.0D0
  !! croco_od : Outer diameter of CroCo strand (m)
  real(kind(1.0D0)) :: croco_id = 0.0D0
  !! croco_id : Inner diameter of CroCo copper tube (m)
  real(kind(1.0D0)) :: croco_thick = 2.5D-3
  !! croco_thick /2.5e-3/ : Thickness of CroCo copper tube (m) (iteration variable 149)

  !real(kind(1.0D0)) :: copper_bar = 0.23d0
  ! !! copper_bar /1.0/ : area of central copper bar, as a fraction of the cable space
  real(kind(1.0D0)) :: copper_rrr = 100d0
  !! copper_rrr /100.0/ : residual resistivity ratio copper in TF superconducting cable

  !real(kind(1.0D0)) :: cable_helium_fraction = 0.284D0
  ! !! cable_helium_fraction /0.284/ : Helium area as a fraction of the cable space.

  real(kind(1.0D0)) :: coppera_m2_max = 1D8
  !! copperA_m2_max /1e8/ : Maximum TF coil current / copper area (A/m2)
  real(kind(1.0D0)) :: f_coppera_m2 = 1d0
  !! f_copperA_m2 /1/ : f-value for constraint 75: TF coil current / copper area < copperA_m2_max


  real(kind(1.0D0)) :: tape_thickness
  real(kind(1.0D0)) :: stack_thickness
  real(kind(1.0D0)) :: tapes
  real(kind(1.0D0)) :: rebco_area
  real(kind(1.0D0)) :: copper_area
  real(kind(1.0D0)) :: hastelloy_area
  real(kind(1.0D0)) :: solder_area
  real(kind(1.0D0)) :: croco_area
  real(kind(1.0D0)) :: copperA_m2       ! TF coil current / copper area (A/m2)
  !real(kind(1.0D0)) :: croco_od

end module rebco_variables

!------------------------------------------------------------------------

module resistive_materials

  !! Variables relating to resistive materials in superconducting conductors
  !! TODO
  implicit none ! ---------------------------------------------------------
  type resistive_material
     real(kind(1.0D0)) :: cp            ! Specific heat capacity J/(K kg).
     real(kind(1.0D0)) :: rrr           ! Residual resistivity ratio
     real(kind(1.0D0)) :: resistivity   ! ohm.m
     real(kind(1.0D0)) :: density       ! kg/m3
     real(kind(1.0D0)) :: cp_density    ! Cp x density J/K/m3
  end type resistive_material
  type supercon_strand
     real(kind(1.0D0)) :: area
     real(kind(1.0D0)) :: critical_current
  end type supercon_strand
  type volume_fractions
     real(kind(1.0D0)) :: copper_area,    copper_fraction
     real(kind(1.0D0)) :: copper_bar_area  !,copper_bar_fraction
     real(kind(1.0D0)) :: hastelloy_area, hastelloy_fraction
     real(kind(1.0D0)) :: helium_area,    helium_fraction
     real(kind(1.0D0)) :: solder_area,    solder_fraction
     real(kind(1.0D0)) :: jacket_area,    jacket_fraction
     real(kind(1.0D0)) :: rebco_area,     rebco_fraction
     real(kind(1.0D0)) :: critical_current
     !real(kind(1.0D0)) :: number_croco         ! Number of CroCo strands (not an integer)
     real(kind(1.0D0)) :: acs                  ! area of cable space inside jacket
     real(kind(1.0D0)) :: area
     !real(kind(1.0D0)) :: tmax                 ! Maximum permitted temperature in quench
  end type volume_fractions
end module resistive_materials

!------------------------------------------------------------------------

module reinke_variables
  !! Module containing global variables relating to the
  !! Reinke Criterion
  !! This module contains global variables relating to the
  !! minimum impurity fraction for detached divertor conditions
  !! Reinke criterion. It furthermore uses several parameters from
  !! Kallenbach model like netau and empurity_enrichment.
  !! M.L. Reinke 2017 Nucl. Fusion 57 034004
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public


  integer       :: impvardiv = 9
  !! impvardiv /9/ : index of impurity to be iterated for
  !!          Reinke divertor detachment criterion

  real(kind(1.0D0)) :: lhat = 4.33D0
  !! lhat /4.33/ : connection length factor L|| = lhat qstar R
  !!               for Reinke criterion, default value from
  !!               Post et al. 1995 J. Nucl. Mat.  220-2 1014

  real(kind(1.0D0)) :: fzmin = 0.0D0
  !! fzmin : Minimum impurity fraction necessary for detachment
  !!         This is the impurity at the SOL/Div

  real(kind(1.0D0)) :: fzactual = 0.001D0
  !! fzactual : Actual impurity fraction of divertor impurity
  !!            (impvardiv) in the SoL (taking impurity_enrichment
  !!            into account) (iteration variable 148)

  integer       :: reinke_mode = 0
  !! reinke_mode /0/ : Switch for Reinke criterion H/I mode
  !!         <LI> = 0 H-mode;
  !!         <LI> = 1 I-mode;</UL>

end module reinke_variables

  !------------------------------------------------------------------------
