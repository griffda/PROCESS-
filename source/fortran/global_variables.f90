! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module global_variables

  !+ad_name  global_variables
  !+ad_summ  Module containing miscellaneous global variables
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains miscellaneous global variables not
  !+ad_desc  well-suited to any of the other 'variables' modules.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  15/10/12 PJK Initial version of module
  !+ad_hist  23/07/14 PJK Added runtitle; modified icase
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code

  implicit none
  public

  !+ad_vars  icase : power plant type
  character(len=48) :: icase = 'Steady-state tokamak model'
  !+ad_vars  runtitle /Run Title/ : short descriptive title for the run
  character(len=180) :: runtitle = &
       "Run Title (change this line using input variable 'runtitle')"

  !+ad_vars  verbose /0/ : switch for turning on/off diagnostic messages:<UL>
  !+ad_varc            <LI> = 0 turn off diagnostics
  !+ad_varc            <LI> = 1 turn on diagnostics</UL>
  integer :: verbose = 0
  !+ad_vars  run_tests /0/ : Turns on built-in tests if set to 1
  integer :: run_tests = 0

  !+ad_vars  maxcal /200/ : maximum number of VMCON iterations
  integer :: maxcal = 200

  character(len=30) :: fileprefix = "" !'dummy_file_prefix'
  character(len=50) :: output_prefix = "" ! output file prefix
  character(len=25) :: xlabel, vlabel
  character(len=25) :: xlabel_2, vlabel_2
  integer :: iscan_global=0    ! Makes iscan available globally.
  real(kind(1.0D0)):: convergence_parameter  ! VMCON convergence parameter "sum"

end module global_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module constants

  !+ad_name  constants
  !+ad_summ  Module containing miscellaneous numerical and physical constants
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains miscellaneous numerical and
  !+ad_desc  physical constants.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  16/10/12 PJK Initial version of module
  !+ad_hist  12/06/13 PJK Added umass
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
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

  !+ad_vars  degrad FIX : degrees to radians, = pi/180
  real(kind(1.0D0)), parameter :: degrad = 0.01745329251D0
  !+ad_vars  echarge FIX : electron charge (C)
  real(kind(1.0D0)), parameter :: echarge = 1.60217733D-19
  !+ad_vars  mproton FIX : proton mass (kg)
  real(kind(1.0D0)), parameter :: mproton = 1.6726231D-27
  !+ad_vars  pi FIX : famous number
  real(kind(1.0D0)), parameter :: pi = 3.1415926535897932D0
  !+ad_vars  rmu0 FIX : permeability of free space, 4.pi x 10^(-7) H/m
  real(kind(1.0D0)), parameter :: rmu0 = 1.256637062D-6
  !+ad_vars  twopi FIX : 2 pi
  real(kind(1.0D0)), parameter :: twopi = 6.2831853071795862D0
  !+ad_vars  umass FIX : unified atomic mass unit (kg)
  real(kind(1.0D0)), parameter :: umass = 1.660538921D-27
  !+ad_vars  epsilon0 FIX : permittivity of free space (Farad/m)
  real(kind(1.0D0)), parameter :: epsilon0 = 8.85418781D-12

contains


end module constants

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module physics_variables

  !+ad_name  physics_variables
  !+ad_summ  Module containing global variables relating to the plasma physics
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the plasma
  !+ad_desc  physics.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  15/10/12 PJK Initial version of module
  !+ad_hist  17/12/12 PJK modified impfe, cfe0, rnfene, fbfe comments
  !+ad_hist  18/12/12 PJK Added pthrmw(6 to 8)
  !+ad_hist  18/12/12 PJK Added snull; modified idivrt
  !+ad_hist  03/01/13 PJK Removed iculdl
  !+ad_hist  08/01/13 PJK Modified iinvqd, iiter, ires comments
  !+ad_hist  22/01/13 PJK Added two stellarator scaling laws; modified comments
  !+ad_hist  11/04/13 PJK Removed ires, rtpte; changed isc, ifispact default values
  !+ad_hist  10/06/13 PJK Modified ishape
  !+ad_hist  12/06/13 PJK Added gammaft, taup; changed rndfuel, qfuel units
  !+ad_hist  18/06/13 PJK Removed dign; changed ffwal, ishape comments
  !+ad_hist  27/06/13 PJK Changed iculbl comment
  !+ad_hist  03/07/13 PJK Changed zeffai comment
  !+ad_hist  10/09/13 PJK Added alpharate, fusionrate, protonrate
  !+ad_hist  11/09/13 PJK Removed ftr, idhe3, iiter; changed ealpha to ealphadt
  !+ad_hist  10/10/13 PJK Modified prad comment
  !+ad_hist  27/11/13 PJK Modified vsbrn description
  !+ad_hist  28/11/13 PJK Added pdd, pdhe3, pdt
  !+ad_hist  28/11/13 PJK Added iprofile
  !+ad_hist  06/03/14 PJK Clarified effect of ishape on kappa, triang
  !+ad_hist  10/03/14 PJK Removed carea
  !+ad_hist  01/04/14 PJK Added ibss=4 option
  !+ad_hist  02/04/14 PJK Added iprofile=1 recommendation to use icurr=4
  !+ad_hist  23/04/14 PJK Added bvert
  !+ad_hist  01/05/14 PJK Changed dnbeta, gtscale comments
  !+ad_hist  14/05/14 PJK added pcorerad
  !+ad_hist  15/05/14 PJK Changed ffwal comment
  !+ad_hist  19/05/14 PJK Changed plrad to pedgerad; removed fradmin;
  !+ad_hisc               added iradloss
  !+ad_hist  21/05/14 PJK Changed ignite wording
  !+ad_hist  22/05/14 PJK Name changes to power quantities
  !+ad_hist  11/06/14 PJK Added pchargemw, ptremw, ptrimw
  !+ad_hist  17/06/14 PJK Added scaling law 39
  !+ad_hist  19/08/14 PJK Removed recyle, impfe
  !+ad_hist  01/09/14 PJK Minor comment changes
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_hist  01/10/14 PJK Added more ishape options
  !+ad_hist  01/10/14 PJK Modified q wording
  !+ad_hist  01/10/14 PJK Added ilhthresh, plhthresh
  !+ad_hist  02/10/14 PJK Added cwrmax
  !+ad_hist  13/11/14 PJK Added fkzohm
  !+ad_hist  13/11/14 PJK Modified iradloss usage
  !+ad_hist  17/11/14 PJK Added palpfwmw
  !+ad_hist  20/05/15 RK  Added iscdens, fgwped for pedestal density scaling
  !+ad_hist  17/06/15 MDK Added Murari scaling (isc=40)
  !+ad_hist  11/09/15 MDK res_time
  !+ad_hist  02/11/16 HL  Added Petty and Lang confinement scalings (isc=41/42)
  !+ad_hist  08/02/17 JM  Added fgwsep the fraction of Greenwald density to set as separatrix density
  !+ad_hist  08/02/17 JM  Gave teped, tesep, neped and nesep non-zero defaults
  !+ad_hist  02/05/18 SIM Added pthrmw(9-14)
  !+ad_hist  17/01/19 SIM Moved photon_wall and rad_fraction to global from physics
  !+ad_hist  13/05/19 SIM Added isc=46-48 and tauee_in
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  implicit none

  public

  !+ad_vars  ipnlaws /48/ FIX : number of energy confinement time scaling laws
  integer, parameter :: ipnlaws = 48

  !+ad_vars  abeam : beam ion mass (amu)
  real(kind(1.0D0)) :: abeam = 0.0D0

  !+ad_vars  afuel : average mass of fuel portion of ions (amu)
  real(kind(1.0D0)) :: afuel = 0.0D0

  !+ad_vars  aion : average mass of all ions (amu)
  real(kind(1.0D0)) :: aion = 0.0D0

  !+ad_vars  alphaj /1.0/ : current profile index;
  !+ad_varc                 calculated from q0, q if iprofile=1
  real(kind(1.0D0)) :: alphaj = 1.0D0

  !+ad_vars  alphan /0.25/ : density profile index
  real(kind(1.0D0)) :: alphan = 0.25D0
  !+ad_vars  alphap : pressure profile index
  real(kind(1.0D0)) :: alphap = 0.0D0
  !+ad_vars  alpharate : alpha particle production rate (particles/m3/sec)
  real(kind(1.0D0)) :: alpharate = 0.0D0
  !+ad_vars  alphat /0.5/ : temperature profile index
  real(kind(1.0D0)) :: alphat = 0.5D0
  !+ad_vars  aspect /2.907/ : aspect ratio (iteration variable 1)
  real(kind(1.0D0)) :: aspect = 2.907D0
  !+ad_vars  beamfus0 /1.0/ : multiplier for beam-background fusion calculation
  real(kind(1.0D0)) :: beamfus0 = 1.0D0
  !+ad_vars  beta /0.042/ : total plasma beta (iteration variable 5)
  !+ad_varc             (calculated if ipedestal =3)
  real(kind(1.0D0)) :: beta = 0.042D0
  !+ad_vars  betaft : fast alpha beta component
  real(kind(1.0D0)) :: betaft = 0.0D0
  !+ad_vars  betalim : allowable beta
  real(kind(1.0D0)) :: betalim = 0.0D0
  !+ad_vars  betanb : neutral beam beta component
  real(kind(1.0D0)) :: betanb = 0.0D0
  !+ad_vars  betap : poloidal beta
  real(kind(1.0D0)) :: betap = 0.0D0
  !+ad_vars  normalised_total_beta : normaised total beta
  real(kind(1.0D0)), bind(C) :: normalised_total_beta = 0.0D0

  !+ad_vars  betbm0 /1.5/ : leading coefficient for NB beta fraction
  real(kind(1.0D0)) :: betbm0 = 1.5D0
  !+ad_vars  bp : poloidal field (T)
  real(kind(1.0D0)) :: bp = 0.0D0
  !+ad_vars  bt /5.68/ : toroidal field on axis (T) (iteration variable 2)
  real(kind(1.0D0)) :: bt = 5.68D0
  !+ad_vars  btot : total toroidal + poloidal field (T)
  real(kind(1.0D0)) :: btot = 0.0D0
  !+ad_vars  burnup : fractional plasma burnup
  real(kind(1.0D0)) :: burnup = 0.0D0
  !+ad_vars  bvert : vertical field at plasma (T)
  real(kind(1.0D0)) :: bvert = 0.0D0
  !+ad_vars  csawth /1.0/ : coeff. for sawteeth effects on burn V-s requirement
  real(kind(1.0D0)) :: csawth = 1.0D0
  !+ad_vars  cvol /1.0/ : multiplying factor times plasma volume (normally=1)
  real(kind(1.0D0)) :: cvol = 1.0D0
  !+ad_vars  cwrmax /1.35/ : maximum ratio of conducting wall distance to
  !+ad_varc                  plasma minor radius for vertical stability
  !+ad_varc                  (constraint equation 23)
  real(kind(1.0D0)) :: cwrmax = 1.35D0
  !+ad_vars  dene /9.8e19/ : electron density (/m3) (iteration variable 6)
  !+ad_varc                  (calculated if ipedestal=3)
  real(kind(1.0D0)) :: dene = 9.8D19
  !+ad_vars  deni : fuel ion density (/m3)
  real(kind(1.0D0)) :: deni = 0.0D0
  !+ad_vars  dlamee : electron-electron coulomb logarithm
  real(kind(1.0D0)) :: dlamee = 0.0D0
  !+ad_vars  dlamie : ion-electron coulomb logarithm
  real(kind(1.0D0)) :: dlamie = 0.0D0
  !+ad_vars  dlimit(7) : density limit (/m3) as calculated using various models
  real(kind(1.0D0)), dimension(7) :: dlimit = 0.0D0
  !+ad_vars  dnalp : thermal alpha density (/m3)
  real(kind(1.0D0)) :: dnalp = 0.0D0
  !+ad_vars  dnbeam : hot beam ion density, variable (/m3)
  real(kind(1.0D0)) :: dnbeam = 0.0D0
  !+ad_vars  dnbeam2 : hot beam ion density from calculation (/m3)
  real(kind(1.0D0)) :: dnbeam2 = 0.0D0
  !+ad_vars  dnbeta /3.5/ : (Troyon-like) coefficient for beta scaling;
  !+ad_varc                 calculated as (4.0*rli) if iprofile=1
  !+ad_varc                 (see also gtscale option)
  real(kind(1.0D0)) :: dnbeta = 3.5D0
  !+ad_vars  dnelimt : density limit (/m3)
  real(kind(1.0D0)) :: dnelimt = 0.0D0
  !+ad_vars  dnitot : total ion density (/m3)
  real(kind(1.0D0)) :: dnitot = 0.0D0
  !+ad_vars  dnla : line averaged electron density (/m3)
  real(kind(1.0D0)) :: dnla = 0.0D0
  !+ad_vars  dnprot : proton ash density (/m3)
  real(kind(1.0D0)) :: dnprot = 0.0D0
  !+ad_vars  dntau : plasma average "n-tau" (seconds/m3)
  real(kind(1.0D0)) :: dntau = 0.0D0
  !+ad_vars  dnz : high Z ion density (/m3)
  real(kind(1.0D0)) :: dnz = 0.0D0
  !+ad_vars  ealphadt /3520.0/ FIX : alpha birth energy in D-T reaction (keV)
  real(kind(1.0D0)), parameter :: ealphadt = 3520.0D0
  !+ad_vars  epbetmax /1.38/ : maximum (eps*beta_poloidal) (constraint equation 6)
  !+ad_varc                    revised (07/01/16) Issue #346
  !+ad_varc                    "Operation at the tokamak equilibrium poloidal beta-limit in TFTR"
  !+ad_varc                    1992 Nucl. Fusion 32 1468
  real(kind(1.0D0)) :: epbetmax = 1.38D0
  !+ad_vars  eps : inverse aspect ratio
  real(kind(1.0D0)) :: eps = 0.34399724802D0
  !+ad_vars  faccd : fraction of plasma current produced by auxiliary current drive
  real(kind(1.0D0)) :: faccd = 0.0D0
  !+ad_vars  facoh : fraction of plasma current produced inductively
  real(kind(1.0D0)) :: facoh = 0.0D0
  !+ad_vars  falpe : fraction of alpha energy to electrons
  real(kind(1.0D0)) :: falpe = 0.0D0
  !+ad_vars  falpha /0.95/ : fraction of alpha power deposited in plasma
  !+ad_varc                  (Physics of Energetic Ions, p.2489)
  real(kind(1.0D0)) :: falpha = 0.95D0
  !+ad_vars  falpi : fraction of alpha power to ions
  real(kind(1.0D0)) :: falpi = 0.0D0
  !+ad_vars  fdeut /0.5/ : deuterium fuel fraction
  real(kind(1.0D0)) :: fdeut = 0.5D0
  !+ad_vars  ftar  /0.5/ : fraction of power to the  lower divertor in double null
  !+ad_vars                configuration (snull = 0 only)
  real(kind(1.0D0)) :: ftar = 0.5D0
  !+ad_vars  ffwal /0.92/ : factor to convert plasma surface area to first wall
  !+ad_varc                 area in neutron wall load calculation (iwalld=1)
  real(kind(1.0D0)) :: ffwal = 0.92D0
  !+ad_vars  fgwped /0.85/ : fraction of Greenwald density to set as pedestal-top density
  !+ad_varc                  If <0, pedestal-top density set manually using neped (ipedestal>=1)
  !+ad_varc                  Needs to be >0 if ipedestal = 3
  !+ad_varc                  (iteration variable 145)
  real(kind(1.0D0)) :: fgwped = 0.85D0
  !+ad_vars  fgwsep /0.50/ : fraction of Greenwald density to set as separatrix density
  !+ad_varc                  If <0, separatrix density set manually using nesep (ipedestal>=1)
  !+ad_varc                  Needs to be >0 if ipedestal = 3
  real(kind(1.0D0)) :: fgwsep = 0.50D0
  !+ad_vars  fhe3 /0.0/ : helium-3 fuel fraction
  real(kind(1.0D0)) :: fhe3 = 0.0D0
  !+ad_vars  figmer : physics figure of merit (= plascur*aspect**sbar, where sbar=1)
  real(kind(1.0D0)) :: figmer = 0.0D0
  !+ad_vars  fkzohm /1.0/ : Zohm elongation scaling adjustment factor (ishape=2, 3)
  real(kind(1.0D0)) :: fkzohm = 1.0D0
  !+ad_vars  fplhsep /1.0/ : F-value for Psep >= Plh + Paux (constraint equation 73)
  real(kind(1.0D0)) :: fplhsep = 1.0D0
  
  !+ad_vars  fpdivlim /1.0/ : F-value for minimum pdivt (constraint equation 80)
  real(kind(1.0D0)) :: fpdivlim = 1.0D0

  !+ad_vars  fne0 /1.0/ : F-value for minimum pdivt (constraint equation 81)
  real(kind(1.0D0)) :: fne0 = 1.0D0

  !+ad_vars  ftrit /0.5/ : tritium fuel fraction
  real(kind(1.0D0)) :: ftrit = 0.5D0
  !+ad_vars  fusionrate : fusion reaction rate (reactions/m3/sec)
  real(kind(1.0D0)) :: fusionrate = 0.0D0
  !+ad_vars  fvsbrnni /1.0/ : fraction of the plasma current produced by
  !+ad_varc                   non-inductive means (iteration variable 44)
  real(kind(1.0D0)) :: fvsbrnni = 1.0D0
  !+ad_vars  gamma /0.4/ : Ejima coefficient for resistive startup V-s formula
  real(kind(1.0D0)) :: gamma = 0.4D0
  !+ad_vars  gammaft : ratio of (fast alpha + neutral beam beta) to thermal beta
  real(kind(1.0D0)) :: gammaft = 0.0D0
  !+ad_vars  gtscale /0/ : switch for a/R scaling of dnbeta (iprofile=0 only):<UL>
  !+ad_varc          <LI>  = 0 do not scale dnbeta with eps;
  !+ad_varc          <LI>  = 1 scale dnbeta with eps</UL>
  integer :: gtscale = 0
  !+ad_vars  hfac(ipnlaws) : H factors for an ignited plasma for each energy confinement
  !+ad_varc                  time scaling law
  real(kind(1.0D0)), dimension(ipnlaws) :: hfac = 0.0D0
  !+ad_vars  hfact /1.0/ : H factor on energy confinement times, radiation corrected
  !+ad_varc                (iteration variable 10).
  !+ad_varc                If ipedestal=2 or 3 and hfact = 0, not used in PLASMOD
  !+ad_varc                (see also plasmod_i_modeltype)
  real(kind(1.0D0)) :: hfact = 1.0D0
  ! Issue #219
  !+ad_vars  taumax /10/ : Maximum allowed energy confinement time (s)
  real(kind(1.0D0)) :: taumax = 10.0D0
  !+ad_vars  ibss /3/ : switch for bootstrap current scaling:<UL>
  !+ad_varc        <LI> = 1 ITER 1989 bootstrap scaling (high R/a only);
  !+ad_varc        <LI> = 2 for Nevins et al general scaling;
  !+ad_varc        <LI> = 3 for Wilson et al numerical scaling;
  !+ad_varc        <LI> = 4 for Sauter et al scaling</UL>
  integer :: ibss = 3
  !+ad_vars  iculbl /0/ : switch for beta limit scaling (constraint equation 24):<UL>
  !+ad_varc          <LI> = 0 apply limit to total beta;
  !+ad_varc          <LI> = 1 apply limit to thermal beta;
  !+ad_varc          <LI> = 2 apply limit to thermal + neutral beam beta</UL>
  integer :: iculbl = 0
  !+ad_vars  icurr /4/ : switch for plasma current scaling to use:<UL>
  !+ad_varc         <LI> = 1 Peng analytic fit;
  !+ad_varc         <LI> = 2 Peng double null divertor scaling (ST);
  !+ad_varc         <LI> = 3 simple ITER scaling (k = 2.2, d = 0.6);
  !+ad_varc         <LI> = 4 later ITER scaling, a la Uckan;
  !+ad_varc         <LI> = 5 Todd empirical scaling I;
  !+ad_varc         <LI> = 6 Todd empirical scaling II;
  !+ad_varc         <LI> = 7 Connor-Hastie model;
  !+ad_varc         <LI> = 8 Sauter scaling allowing negative triangularity;
  !+ad_varc         <LI> = 9 FIESTA ST fit </UL>
  integer :: icurr = 4
  !+ad_vars  idensl /7/ : switch for density limit to enforce (constraint equation 5):<UL>
  !+ad_varc          <LI> = 1 old ASDEX;
  !+ad_varc          <LI> = 2 Borrass model for ITER (I);
  !+ad_varc          <LI> = 3 Borrass model for ITER (II);
  !+ad_varc          <LI> = 4 JET edge radiation;
  !+ad_varc          <LI> = 5 JET simplified;
  !+ad_varc          <LI> = 6 Hugill-Murakami Mq limit;
  !+ad_varc          <LI> = 7 Greenwald limit</UL>
  integer :: idensl = 7
  !+ad_vars  idivrt : number of divertors (calculated from snull)
  integer :: idivrt = 2
  !+ad_vars  ifalphap /1/ : switch for fast alpha pressure calculation:<UL>
  !+ad_varc            <LI> = 0 ITER physics rules (Uckan) fit;
  !+ad_varc            <LI> = 1 Modified fit (D. Ward) - better at high temperature</UL>
  integer :: ifalphap = 1
  !+ad_vars  ifispact /0/ : switch for neutronics calculations:<UL>
  !+ad_varc            <LI> = 0 neutronics calculations turned off;
  !+ad_varc            <LI> = 1 neutronics calculations turned on</UL>
  integer :: ifispact = 0
  !+ad_vars  igeom /1/ : switch for plasma geometry calculation:<UL>
  !+ad_varc         <LI> = 0 original method (possibly based on Peng ST modelling);
  !+ad_varc         <LI> = 1 improved (and traceable) method</UL>
  integer :: igeom = 1
  !+ad_vars  ignite /0/ : switch for ignition assumption:<UL>
  !+ad_varc          <LI> = 0 do not assume plasma ignition;
  !+ad_varc          <LI> = 1 assume ignited (but include auxiliary power in costs)</UL>
  !+ad_varc       Obviously, ignite must be zero if current drive is required.
  !+ad_varc       If ignite=1, any auxiliary power is assumed to be used only
  !+ad_varc       during plasma start-up, and is excluded from all steady-state
  !+ad_varc       power balance calculations.
  integer :: ignite = 0
  !+ad_vars  iinvqd /1/ : switch for inverse quadrature in L-mode scaling laws 5 and 9:<UL>
  !+ad_varc          <LI> = 0 inverse quadrature not used;
  !+ad_varc          <LI> = 1 inverse quadrature with Neo-Alcator tau-E used</UL>
  integer :: iinvqd = 1

  !+ad_vars  ipedestal /1/ : switch for pedestal profiles:<UL>
  !+ad_varc             <LI> = 0 use original parabolic profiles;
  !+ad_varc             <LI> = 1 use pedestal profiles
  !+ad_varc             <LI> = 2 use pedestal profiles and run PLASMOD on final output
  !+ad_varc             <LI> = 3 use PLASMOD transport model only to calculate pedestal profiles</UL>
  integer :: ipedestal = 1
  ! Issue #589 remove iscdens
  !+ad_vars  iscdens /0/ : switch for pedestal profiles: OBSOLETE
  ! integer :: iscdens = 0

  ! Issue #413
  !+ad_vars  ieped /0/ : switch for scaling pedestal-top temperature with plasma parameters:<UL>
  !+ad_varc             <LI> = 0 set pedestal-top temperature manually using teped;
  !+ad_varc             <LI> = 1 set pedestal-top temperature using EPED scaling;
  !+ad_varc                   (PLASMOD implementation of scaling within PLASMOD, ipedestal =2,3)
  !+ad_varc             <LI>    https://idm.euro-fusion.org/?uid=2MSZ4T </UL>
  integer :: ieped = 0

  ! Issue #730
  !+ad_vars  eped_sf /1.0/ : Adjustment factor for EPED scaling to reduce 
  !+ad_varc  pedestal temperature or pressure to mitigate or prevent ELMs
  real(kind(1.0D0)), bind(C) :: eped_sf = 1.0D0

  !+ad_vars  neped /4.0e19/ : electron density of pedestal [m-3] (ipedestal=1,2, calculated if 3)
  real(kind(1.0D0)) :: neped = 4.0D19
  !+ad_vars  nesep /3.0e19/ : electron density at separatrix [m-3] (ipedestal=1,2, calculated if 3)
  real(kind(1.0D0)) :: nesep = 3.0D19
  !+ad_vars  alpha_crit : critical ballooning parameter value
  real(kind(1.0D0)) :: alpha_crit = 0.0D0
  !+ad_vars  nesep_crit : critical electron density at separatrix [m-3]
  real(kind(1.0D0)) :: nesep_crit = 0.0D0
  !+ad_vars  plasma_res_factor /1.0/ : plasma resistivity pre-factor
  real(kind(1.0D0)) :: plasma_res_factor = 1.0D0
  !+ad_vars  rhopedn /1.0/ : r/a of density pedestal (ipedestal>=1)
  real(kind(1.0D0)) :: rhopedn = 1.0D0
  !+ad_vars  rhopedt /1.0/ : r/a of temperature pedestal (ipedestal>=1)
  real(kind(1.0D0)) :: rhopedt = 1.0D0
  !+ad_vars  tbeta /2.0/ : temperature profile index beta  (ipedestal=1,2)
  real(kind(1.0D0)) :: tbeta = 2.0D0
  !+ad_vars  teped /1.0/ : electron temperature of pedestal (keV) (ipedestal>=1, ieped=0, calculated for ieped=1)
  real(kind(1.0D0)) :: teped = 1.0D0
  !+ad_vars  tesep /0.1/ : electron temperature at separatrix (keV) (ipedestal>=1)
  !+ad_varc                calculated if reinke criterion is used (icc = 78)
  real(kind(1.0D0)) :: tesep = 0.1D0

  !+ad_vars  iprofile /1/ : switch for current profile consistency:<UL>
  !+ad_varc             <LI> = 0 use input values for alphaj, rli, dnbeta
  !+ad_varc                      (but see gtscale option);
  !+ad_varc             <LI> = 1 make these consistent with input q, q0 values
  !+ad_varc                      (recommendation: use icurr=4 with this option) </UL>
  integer :: iprofile = 1
  !+ad_vars  iradloss /1/ : switch for radiation loss term usage in power balance (see User Guide):<UL>
  !+ad_varc             <LI> = 0 total power lost is scaling power plus radiation (needed for ipedestal=2,3)
  !+ad_varc             <LI> = 1 total power lost is scaling power plus core radiation only
  !+ad_varc             <LI> = 2 total power lost is scaling power only, with no additional
  !+ad_varc                      allowance for radiation. This is not recommended for power plant models.</UL>
  integer :: iradloss = 1

  !+ad_vars  isc /34 (=IPB98(y,2))/ : switch for energy confinement time scaling law
  !+ad_varc          (see description in tauscl)
  integer :: isc = 34
  !+ad_vars  tauscl(ipnlaws) : labels describing energy confinement scaling laws:<UL>
  character(len=24), dimension(ipnlaws) :: tauscl = (/ &
  !+ad_varc  <LI> ( 1)  Neo-Alcator (ohmic)
       'Neo-Alcator      (ohmic)', &
  !+ad_varc  <LI> ( 2)  Mirnov (H-mode)
       'Mirnov               (H)', &
  !+ad_varc  <LI> ( 3)  Merezkhin-Muhkovatov (L-mode)
       'Merezkhin-Muhkovatov (L)', &
  !+ad_varc  <LI> ( 4)  Shimomura (H-mode)
       'Shimomura            (H)', &
  !+ad_varc  <LI> ( 5)  Kaye-Goldston (L-mode)
       'Kaye-Goldston        (L)', &
  !+ad_varc  <LI> ( 6)  ITER 89-P (L-mode)
       'ITER 89-P            (L)', &
  !+ad_varc  <LI> ( 7)  ITER 89-O (L-mode)
       'ITER 89-O            (L)', &
  !+ad_varc  <LI> ( 8)  Rebut-Lallia (L-mode)
       'Rebut-Lallia         (L)', &
  !+ad_varc  <LI> ( 9)  Goldston (L-mode)
       'Goldston             (L)', &
  !+ad_varc  <LI> (10)  T10 (L-mode)
       'T10                  (L)', &
  !+ad_varc  <LI> (11)  JAERI-88 (L-mode)
       'JAERI-88             (L)', &
  !+ad_varc  <LI> (12)  Kaye-Big Complex (L-mode)
       'Kaye-Big Complex     (L)', &
  !+ad_varc  <LI> (13)  ITER H90-P (H-mode)
       'ITER H90-P           (H)', &
  !+ad_varc  <LI> (14)  ITER Mix (L-mode)
       'ITER Mix             (L)', &
  !+ad_varc  <LI> (15)  Riedel (L-mode)
       'Riedel               (L)', &
  !+ad_varc  <LI> (16)  Christiansen (L-mode)
       'Christiansen         (L)', &
  !+ad_varc  <LI> (17)  Lackner-Gottardi (L-mode)
       'Lackner-Gottardi     (L)', &
  !+ad_varc  <LI> (18)  Neo-Kaye (L-mode)
       'Neo-Kaye             (L)', &
  !+ad_varc  <LI> (19)  Riedel (H-mode)
       'Riedel               (H)', &
  !+ad_varc  <LI> (20)  ITER H90-P amended (H-mode)
       'ITER H90-P amended   (H)', &
  !+ad_varc  <LI> (21)  LHD (stellarator)
       'LHD              (stell)', &
  !+ad_varc  <LI> (22)  Gyro-reduced Bohm (stellarator)
       'Gyro-reduced Bohm(stell)', &
  !+ad_varc  <LI> (23)  Lackner-Gottardi (stellarator)
       'Lackner-Gottardi (stell)', &
  !+ad_varc  <LI> (24)  ITER-93H (H-mode)
       'ITER-93H             (H)', &
  !+ad_varc  <LI> (25) OBSOLETE
       'TITAN RFP OBSOLETE      ', &
  !+ad_varc  <LI> (26)  ITER H-97P ELM-free (H-mode)
       'ITER H-97P ELM-free  (H)', &
  !+ad_varc  <LI> (27)  ITER H-97P ELMy (H-mode)
       'ITER H-97P ELMy      (H)', &
  !+ad_varc  <LI> (28)  ITER-96P (=ITER-97L) (L-mode)
       'ITER-96P             (L)', &
  !+ad_varc  <LI> (29)  Valovic modified ELMy (H-mode)
       'Valovic modified ELMy(H)', &
  !+ad_varc  <LI> (30)  Kaye PPPL April 98 (L-mode)
       'Kaye PPPL April 98   (L)', &
  !+ad_varc  <LI> (31)  ITERH-PB98P(y) (H-mode)
       'ITERH-PB98P(y)       (H)', &
  !+ad_varc  <LI> (32)  IPB98(y) (H-mode)
       'IPB98(y)             (H)', &
  !+ad_varc  <LI> (33)  IPB98(y,1) (H-mode)
       'IPB98(y,1)           (H)', &
  !+ad_varc  <LI> (34)  IPB98(y,2) (H-mode)
       'IPB98(y,2)           (H)', &
  !+ad_varc  <LI> (35)  IPB98(y,3) (H-mode)
       'IPB98(y,3)           (H)', &
  !+ad_varc  <LI> (36)  IPB98(y,4) (H-mode)
       'IPB98(y,4)           (H)', &
  !+ad_varc  <LI> (37)  ISS95 (stellarator)
       'ISS95            (stell)', &
  !+ad_varc  <LI> (38)  ISS04 (stellarator)
       'ISS04            (stell)', &
  !+ad_varc  <LI> (39)  DS03 (H-mode)
       'DS03                 (H)', &
  !+ad_varc  <LI> (40)  Murari et al non-power law (H-mode)
       'Murari et al NPL     (H)', &
  !+ad_varc  <LI> (41)  Petty 2008 (H-mode)
       'Petty 2008           (H)', &
  !+ad_varc  <LI> (42)  Lang et al. 2012 (H-mode)
       'Lang et al. 2012     (H)', &
  !+ad_varc  <LI> (43)  Hubbard 2017 (I-mode) - nominal
       'Hubbard 2017 - nom   (I)', &
  !+ad_varc  <LI> (44)  Hubbard 2017 (I-mode) - lower bound
       'Hubbard 2017 - lower (I)', &
  !+ad_varc  <LI> (45)  Hubbard 2017 (I-mode) - upper bound
       'Hubbard 2017 - upper (I)', &
  !+ad_varc  <LI> (46)  NSTX (H-mode; Spherical tokamak)
       'NSTX (Spherical)     (H)', &
  !+ad_varc  <LI> (47)  NSTX-Petty08 Hybrid (H-mode)
       'NSTX-Petty08 Hybrid  (H)', &
  !+ad_varc  <LI> (48)  Use input tauee_in </UL>
       'Input tauee_in          ' /)

  !+ad_vars  iscrp /1/ : switch for plasma-first wall clearances:<UL>
  !+ad_varc         <LI> = 0 use 10% of rminor;
  !+ad_varc         <LI> = 1 use input (scrapli and scraplo)</UL>
  integer :: iscrp = 1
  !+ad_vars  ishape /0/ : switch for plasma cross-sectional shape calculation:<UL>
  !+ad_varc          <LI> = 0 use input kappa, triang to calculate 95% values;
  !+ad_varc          <LI> = 1 scale qlim, kappa, triang with aspect ratio (ST);
  !+ad_varc          <LI> = 2 set kappa to the natural elongation value (Zohm ITER scaling),
  !+ad_varc                   triang input;
  !+ad_varc          <LI> = 3 set kappa to the natural elongation value (Zohm ITER scaling),
  !+ad_varc                   triang95 input;
  !+ad_varc          <LI> = 4 use input kappa95, triang95 to calculate separatrix values</UL>
  integer :: ishape = 0
  !+ad_vars  itart /0/ : switch for spherical tokamak (ST) models:<UL>
  !+ad_varc         <LI> = 0 use conventional aspect ratio models;
  !+ad_varc         <LI> = 1 use spherical tokamak models</UL>
  integer, bind(C) :: itart = 0
  !+ad_vars  itartpf /0/ : switch for Spherical Tokamak PF models:<UL>
  !+ad_varc         <LI> = 0 use Peng and Strickler (1986) model;
  !+ad_varc         <LI> = 1 use conventional aspect ratio model</UL>
  integer :: itartpf = 0
  !+ad_vars  iwalld /1/ : switch for neutron wall load calculation:<UL>
  !+ad_varc          <LI> = 1 use scaled plasma surface area;
  !+ad_varc          <LI> = 2 use first wall area directly</UL>
  integer :: iwalld = 1
  !+ad_vars  kappa /1.792/ : plasma separatrix elongation (calculated if ishape > 0)
  real(kind(1.0D0)), bind(C) :: kappa = 1.792D0
  !+ad_vars  kappa95 /1.6/ : plasma elongation at 95% surface (calculated if ishape < 4)
  real(kind(1.0D0)) :: kappa95 = 1.6D0
  !+ad_vars  kappaa : plasma elongation calculated as xarea/(pi.a2)
  real(kind(1.0D0)) :: kappaa = 0.0D0
  !+ad_vars  kappaa_IPB : Volume measure of plasma elongation
  real(kind(1.0D0)) :: kappaa_IPB = 0.d0
  !+ad_vars  ne0 : central electron density (/m3)
  real(kind(1.0D0)) :: ne0 = 0.0D0
  !+ad_vars  ni0 : central ion density (/m3)
  real(kind(1.0D0)) :: ni0 = 0.0D0
  !+ad_vars  p0 : central total plasma pressure (Pa)
  real(kind(1.0D0)) :: p0 = 0.0D0
  !+ad_vars  palppv : alpha power per volume (MW/m3)
  real(kind(1.0D0)) :: palppv = 0.0D0
  !+ad_vars  palpepv : alpha power per volume to electrons (MW/m3)
  real(kind(1.0D0)) :: palpepv = 0.0D0
  !+ad_vars  palpfwmw : alpha power escaping plasma and reaching first wall (MW)
  real(kind(1.0D0)) :: palpfwmw = 0.0D0
  !+ad_vars  palpipv : alpha power per volume to ions (MW/m3)
  real(kind(1.0D0)) :: palpipv = 0.0D0
  !+ad_vars  palpmw : alpha power (MW)
  real(kind(1.0D0)) :: palpmw = 0.0D0
  !+ad_vars  palpnb : alpha power from hot neutral beam ions (MW)
  real(kind(1.0D0)) :: palpnb = 0.0D0
  !+ad_vars  pbrempv : bremsstrahlung power per volume (MW/m3)
  real(kind(1.0D0)) :: pbrempv = 0.0D0
  !+ad_vars  pchargemw : non-alpha charged particle fusion power (MW)
  real(kind(1.0D0)) :: pchargemw = 0.0D0
  !+ad_vars  pchargepv : non-alpha charged particle fusion power per volume (MW/m3)
  real(kind(1.0D0)) :: pchargepv = 0.0D0
  !+ad_vars  pcoef : profile factor (= n-weighted T / average T)
  real(kind(1.0D0)) :: pcoef = 0.0D0
  !+ad_vars  pcoreradmw : total core radiation power (MW)
  real(kind(1.0D0)) :: pcoreradmw = 0.0D0
  !+ad_vars  pcoreradpv : total core radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: pcoreradpv = 0.0D0
  !+ad_vars  pdd : deuterium-deuterium fusion power (MW)
  real(kind(1.0D0)) :: pdd = 0.0D0
  !+ad_vars  pdhe3 : deuterium-helium3 fusion power (MW)
  real(kind(1.0D0)) :: pdhe3 = 0.0D0
  !+ad_vars  pdivt : power to conducted to the divertor region (MW)
  real(kind(1.0D0)) :: pdivt = 0.0D0
  !+ad_vars pdivl : power conducted to the lower divertor region (calculated if snull = 0) (MW)
  real(kind(1.0D0)) :: pdivl = 0.0D0
  !+ad_vars pdivu : power conducted to the upper divertor region (calculated if snull = 0) (MW)
  real(kind(1.0D0)) :: pdivu = 0.0D0
  !+ad_vars pdivmax : power conducted to the divertor with most load (calculated if snull = 0) (MW)
  real(kind(1.0D0)) :: pdivmax = 0.0D0
  !+ad_vars  pdt : deuterium-tritium fusion power (MW)
  real(kind(1.0D0)) :: pdt = 0.0D0
  !+ad_vars  pedgeradmw : edge radiation power (MW)
  real(kind(1.0D0)) :: pedgeradmw = 0.0D0
  !+ad_vars  pedgeradpv : edge radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: pedgeradpv = 0.0D0
  !+ad_vars  pfuscmw : charged particle fusion power (MW)
  real(kind(1.0D0)) :: pfuscmw = 0.0D0
  !+ad_vars  phiint : internal plasma V-s
  real(kind(1.0D0)) :: phiint = 0.0D0
  !+ad_vars  photon_wall : Nominal mean radiation load on inside surface of reactor (MW/m2)
  real(kind(1.0D0)) :: photon_wall = 0.0D0
  !+ad_vars  piepv : ion/electron equilibration power per volume (MW/m3)
  real(kind(1.0D0)) :: piepv = 0.0D0
  !+ad_vars  plascur : plasma current (A)
  real(kind(1.0D0)), bind(C) :: plascur = 0.0D0
  !+ad_vars  plinepv : line radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: plinepv = 0.0D0
  !+ad_vars  pneutmw : neutron fusion power (MW)
  real(kind(1.0D0)) :: pneutmw = 0.0D0
  !+ad_vars  pneutpv : neutron fusion power per volume (MW/m3)
  real(kind(1.0D0)) :: pneutpv = 0.0D0
  !+ad_vars  pohmmw : ohmic heating power (MW)
  real(kind(1.0D0)) :: pohmmw = 0.0D0
  !+ad_vars  pohmpv : ohmic heating power per volume (MW/m3)
  real(kind(1.0D0)) :: pohmpv = 0.0D0
  !+ad_vars  powerht : heating power (= transport loss power) (MW) used in
  !+ad_varc            confinement time calculation
  real(kind(1.0D0)) :: powerht = 0.0D0
  !+ad_vars  powfmw : fusion power (MW)
  real(kind(1.0D0)), bind(C) :: powfmw = 0.0D0
  !+ad_vars  pperim : plasma poloidal perimeter (m)
  real(kind(1.0D0)) :: pperim = 0.0D0
  !+ad_vars  pradmw : total radiation power (MW)
  real(kind(1.0D0)) :: pradmw = 0.0D0
  !+ad_vars  pradpv : total radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: pradpv = 0.0D0
  !+ad_vars  protonrate : proton production rate (particles/m3/sec)
  real(kind(1.0D0)) :: protonrate = 0.0D0
  !+ad_vars  psolradmw : SOL radiation power (MW) (stellarator only)
  real(kind(1.0D0)) :: psolradmw = 0.0D0
  !+ad_vars  psyncpv : synchrotron radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: psyncpv = 0.0D0
  !+ad_vars  ilhthresh /6/ : switch for L-H mode power threshold scaling to use
  !+ad_varc                  (see pthrmw for list)
  integer :: ilhthresh = 6
  !+ad_vars  plhthresh : L-H mode power threshold (MW)
  !+ad_varc              (chosen via ilhthresh, and enforced if constraint equation 15 is on)
  real(kind(1.0D0)) :: plhthresh = 0.0D0
  !+ad_vars  pthrmw(18) : L-H power threshold for various scalings (MW): <OL>
  !+ad_varc         <LI> ITER 1996 scaling: nominal
  !+ad_varc         <LI> ITER 1996 scaling: upper bound
  !+ad_varc         <LI> ITER 1996 scaling: lower bound
  !+ad_varc         <LI> ITER 1997 scaling: excluding elongation
  !+ad_varc         <LI> ITER 1997 scaling: including elongation
  !+ad_varc         <LI> Martin 2008 scaling: nominal
  !+ad_varc         <LI> Martin 2008 scaling: 95% upper bound
  !+ad_varc         <LI> Martin 2008 scaling: 95% lower bound
  !+ad_varc         <LI> Snipes 2000 scaling: nominal
  !+ad_varc         <LI> Snipes 2000 scaling: upper bound
  !+ad_varc         <LI> Snipes 2000 scaling: lower bound
  !+ad_varc         <LI> Snipes 2000 scaling (closed divertor): nominal
  !+ad_varc         <LI> Snipes 2000 scaling (closed divertor): upper bound
  !+ad_varc         <LI> Snipes 2000 scaling (closed divertor): lower bound
  !+ad_varc         <LI> Hubbard et al. 2012 L-I threshold scaling: nominal
  !+ad_varc         <LI> Hubbard et al. 2012 L-I threshold scaling: lower bound
  !+ad_varc         <LI> Hubbard et al. 2012 L-I threshold scaling: upper bound
  !+ad_varc         <LI> Hubbard et al. 2017 L-I threshold scaling</OL>
  real(kind(1.0D0)), dimension(18) :: pthrmw = 0.0D0
  !+ad_vars  ptremw : electron transport power (MW)
  real(kind(1.0D0)) :: ptremw = 0.0D0
  !+ad_vars  ptrepv : electron transport power per volume (MW/m3)
  real(kind(1.0D0)) :: ptrepv = 0.0D0
  !+ad_vars  ptrimw : ion transport power (MW)
  real(kind(1.0D0)) :: ptrimw = 0.0D0
  !+ad_vars  pscalingmw : Total transport power from scaling law (MW)
  real(kind(1.0D0)) :: pscalingmw = 0.0D0
  !+ad_vars  ptripv : ion transport power per volume (MW/m3)
  real(kind(1.0D0)) :: ptripv = 0.0D0
  !+ad_vars  q /3.0/ : safety factor 'near' plasma edge (iteration variable 18):
  !+ad_varc            equal to q95 (unless icurr = 2 (ST current scaling),
  !+ad_varc            in which case q = mean edge safety factor qbar)
  real(kind(1.0D0)) :: q = 3.0D0
  !+ad_vars  q0 /1.0/ : safety factor on axis
  real(kind(1.0D0)) :: q0 = 1.0D0
  !+ad_vars  q95 : safety factor at 95% surface
  real(kind(1.0D0)) :: q95 = 0.0D0
  !+ad_vars  qfuel : plasma fuelling rate (nucleus-pairs/s)
  real(kind(1.0D0)) :: qfuel = 0.0D0

  !+ad_vars  tauratio /1.0/ : ratio of He and pellet particle confinement times
  real(kind(1.0D0)) :: tauratio = 1.0D0


  !+ad_vars  qlim : lower limit for edge safety factor
  real(kind(1.0D0)) :: qlim = 0.0D0
  !+ad_vars  qstar : cylindrical safety factor
  real(kind(1.0D0)) :: qstar = 0.0D0
  !+ad_vars  rad_fraction_sol /0.8/ : SoL radiation fraction 
  real(kind(1.0D0)) :: rad_fraction_sol = 0.8D0
  !+ad_vars rad_fraction : Radiation fraction = total radiation / total power deposited in plasma
  real(kind(1.0D0)) :: rad_fraction = 0.0D0
  !+ad_vars  ralpne /0.1/ : thermal alpha density / electron density (iteration variable 109)
  !+ad_varc            (calculated if ipedestal=3)
  real(kind(1.0D0)) :: ralpne = 0.10D0
  !+ad_vars  protium /0.0/ : Seeded protium density / electron density.
  real(kind(1.0D0)) :: protium = 0.0D0

  !+ad_vars  rli /0.9/ : plasma normalised internal inductance;
  !+ad_varc              calculated from alphaj if iprofile=1
  real(kind(1.0D0)) :: rli = 0.9D0
  !+ad_vars  rlp : plasma inductance (H)
  real(kind(1.0D0)) :: rlp = 0.0D0
  !+ad_vars  rmajor /8.14/ : plasma major radius (m) (iteration variable 3)
  real(kind(1.0D0)), bind(C) :: rmajor = 8.14D0
  !+ad_vars  rminor : plasma minor radius (m)
  real(kind(1.0D0)), bind(C) :: rminor = 0.0D0
  !+ad_vars  rnbeam /0.005/ : hot beam density / n_e (iteration variable 7)
  real(kind(1.0D0)) :: rnbeam = 0.005D0
  !+ad_vars  rncne : n_carbon / n_e
  real(kind(1.0D0)) :: rncne = 0.0D0
  !+ad_vars  rndfuel : fuel burnup rate (reactions/second)
  real(kind(1.0D0)) :: rndfuel = 0.0D0
  !+ad_vars  rnfene : n_highZ / n_e
  real(kind(1.0D0)) :: rnfene = 0.0D0
  !+ad_vars  rnone : n_oxygen / n_e
  real(kind(1.0D0)) :: rnone = 0.0D0
  !+ad_vars  rpfac : neo-classical correction factor to rplas
  real(kind(1.0D0)) :: rpfac = 0.0D0
  !+ad_vars  rplas : plasma resistance (ohm)
  real(kind(1.0D0)) :: rplas = 0.0D0

  !+ad_vars  res_time : plasma current resistive diffusion time (s)
  real(kind(1.0D0)) :: res_time = 0.0D0


  !+ad_vars  sarea : plasma surface area
  real(kind(1.0D0)) :: sarea = 0.0D0
  !+ad_vars  sareao : outboard plasma surface area
  real(kind(1.0D0)) :: sareao = 0.0D0
  !+ad_vars  sf : shape factor = plasma poloidal perimeter / (2.pi.rminor)
  real(kind(1.0D0)) :: sf = 0.0D0
  !+ad_vars  snull /1/ : switch for single null / double null plasma:<UL>
  !+ad_varc          <LI> = 0 for double null;
  !+ad_varc          <LI> = 1 for single null (diverted side down)</UL>
  integer :: snull = 1
  !+ad_vars  ssync /0.6/ : synchrotron wall reflectivity factor
  real(kind(1.0D0)) :: ssync = 0.6D0
  !+ad_vars  tauee : electron energy confinement time (sec)
  real(kind(1.0D0)) :: tauee = 0.0D0
  !+ad_vars  tauee_in /0.0/  : Input electron energy confinement time (sec) (isc=48 only)
  real(kind(1.0D0)) :: tauee_in = 0.0D0
  !+ad_vars  taueff : global thermal energy confinement time (sec)
  real(kind(1.0D0)) :: taueff = 0.0D0
  !+ad_vars  tauei : ion energy confinement time (sec)
  real(kind(1.0D0)) :: tauei = 0.0D0
  !+ad_vars  taup : alpha particle confinement time (sec)
  real(kind(1.0D0)) :: taup = 0.0D0

  !+ad_vars  te /12.9/ : volume averaged electron temperature (keV)
  !+ad_varc              (iteration variable 4)
  !+ad_varc              (calculated if ipedestal = 3)
  real(kind(1.0D0)) :: te = 12.9D0
  !+ad_vars  te0 : central electron temperature (keV)
  real(kind(1.0D0)) :: te0 = 0.0D0
  !+ad_vars  ten : density weighted average electron temperature (keV)
  real(kind(1.0D0)) :: ten = 0.0D0
  !+ad_vars  ti /12.9/ : volume averaged ion temperature (keV);
  !+ad_varc              N.B. calculated from te if tratio > 0.0
  real(kind(1.0D0)) :: ti = 12.9D0
  !+ad_vars  ti0 : central ion temperature (keV)
  real(kind(1.0D0)) :: ti0 = 0.0D0
  !+ad_vars  tin : density weighted average ion temperature (keV)
  real(kind(1.0D0)) :: tin = 0.0D0
  !+ad_vars  tratio /1.0/ : ion temperature / electron temperature;
  !+ad_varc                 used to calculate ti if tratio > 0.0
  real(kind(1.0D0)) :: tratio = 1.0D0
  !+ad_vars  triang /0.36/ : plasma separatrix triangularity (calculated if ishape=1, 3 or 4)
  real(kind(1.0D0)), bind(C) :: triang = 0.36D0
  !+ad_vars  triang95 /0.24/ : plasma triangularity at 95% surface (calculated if ishape < 3)
  real(kind(1.0D0)) :: triang95 = 0.24D0
  !+ad_vars  vol : plasma volume (m3)
  real(kind(1.0D0)) :: vol = 0.0D0
  !+ad_vars  vsbrn : V-s needed during flat-top (heat + burn times) (Wb)
  real(kind(1.0D0)) :: vsbrn = 0.0D0
  !+ad_vars  vshift : plasma/device midplane vertical shift - single null
  real(kind(1.0D0)) :: vshift = 0.0D0
  !+ad_vars  vsind : internal and external plasma inductance V-s (Wb)
  real(kind(1.0D0)) :: vsind = 0.0D0
  !+ad_vars  vsres : resistive losses in startup V-s (Wb)
  real(kind(1.0D0)) :: vsres = 0.0D0
  !+ad_vars  vsstt : total V-s needed (Wb)
  real(kind(1.0D0)) :: vsstt = 0.0D0
  !+ad_vars  wallmw : average neutron wall load (MW/m2)
  real(kind(1.0D0)), bind(C) :: wallmw = 0.0D0
  !+ad_vars  wtgpd : mass of fuel used per day (g)
  real(kind(1.0D0)) :: wtgpd = 0.0D0
  !+ad_vars  xarea : plasma cross-sectional area (m2)
  real(kind(1.0D0)) :: xarea = 0.0D0
  !+ad_vars  zeff : plasma effective charge
  real(kind(1.0D0)) :: zeff = 0.0D0
  !+ad_vars  zeffai : mass weighted plasma effective charge
  real(kind(1.0D0)) :: zeffai = 0.0D0

end module physics_variables


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module plasmod_variables

  !+ad_name  plasmod_variables
  !+ad_summ  Module containing global variables relating to PLASMOD
  !+ad_type  Module
  !+ad_auth  K Ellis, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to PLASMOD
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  26/02/18 KE Initial version of module
  !+ad_stat  Okay
  !+ad_docs  E. Fable et al., Fusion Engineering and Design, Volume 130, May 2018, Pages 131-136
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use structs


  implicit none


  public

 !Derived type numerics_transp
  !+ad_vars  plasmod_tol /1.0d-10/ : tolerance to be reached at each time step (%)
  real(kind(1.0D0)) :: plasmod_tol = 1.0d-10
  !+ad_vars  plasmod_dtmin /0.05d0/ : min time step
  real(kind(1.0D0)) :: plasmod_dtmin = 0.05d0
  !+ad_vars  plasmod_dtmax /0.1d0/ : max time step
  real(kind(1.0D0)) :: plasmod_dtmax = 0.1d0
  !+ad_vars  plasmod_dt /0.01d0/ : time step
  real(kind(1.0D0)) :: plasmod_dt = 0.01d0
  !+ad_vars  plasmod_dtinc /2.0d0/ : decrease of dt
  real(kind(1.0D0)) :: plasmod_dtinc = 2.0d0
  !+ad_vars  plasmod_ainc /1.1d0/ : increase of dt
  real(kind(1.0D0)) :: plasmod_Ainc = 1.1d0
  !+ad_vars  plasmod_test /100000.0d0/ : max number of iterations
  real(kind(1.0D0)) :: plasmod_test = 1000000.0d0
  !+ad_vars  plasmod_tolmin /10.1d0/ : multiplier of etolm which can not be exceeded
  real(kind(1.0D0)) :: plasmod_tolmin = 10.1d0
  !+ad_vars  plasmod_eopt /0.15d0/ : exponent of jipperdo
  real(kind(1.0D0)) :: plasmod_eopt = 0.15d0
  !+ad_vars  plasmod_dtmaxmin /0.15d0/ : exponent of jipperdo2
  real(kind(1.0D0)) :: plasmod_dtmaxmin = 0.15d0
  !+ad_vars  plasmod_dtmaxmax /0.0d0/ : stabilizing coefficient
  real(kind(1.0D0)) :: plasmod_dtmaxmax = 0.0d0
  !+ad_vars  plasmod_capa /0.1d0/ : first radial grid point
  real(kind(1.0D0)) :: plasmod_capA = 0.1d0
  !+ad_vars  plasmod_maxa /0.0d0/ : diagz 0 or 1
  real(kind(1.0D0)) :: plasmod_maxA = 0.0d0
  !+ad_vars  plasmod_dgy /1.0d-5/ : Newton differential
  real(kind(1.0D0)) :: plasmod_dgy = 1.0d-5

  !+ad_vars  plasmod_iprocess /1/ : 0 - use PLASMOD functions, 1 - use PROCESS functions
  integer :: plasmod_iprocess = 1
  !+ad_vars  plasmod_i_modeltype /1/ : switch for the transport model <UL>
  !+ad_varc  <LI> 1 - Simple gyrobohm scaling with imposed
  !+ad_varc  H factor > 1. Other values give H factor as output
  !+ad_varc  <LI> 111 - roughly calibrated to give H=1 for DEMO, but not fixed H </UL>
  integer :: plasmod_i_modeltype = 1

  !+ad_vars  plasmod_i_equiltype /1/ : 1 - EMEQ, solve with sawteeth and inputted q95.
  !+ad_varc  2 - EMEQ, solve with sawteeth and inputted Ip (not recommended!).
  integer :: plasmod_i_equiltype = 1

  !+ad_vars  plasmod_isawt /1/ : 0 - no sawteeth, 1 - solve with sawteeth.
  integer :: plasmod_isawt = 1

  !+ad_vars  plasmod_nx /41/ : number of interpolated grid points
  integer :: plasmod_nx = 41
  !+ad_vars  plasmod_nxt /7/ : number of solved grid points
  integer :: plasmod_nxt = 7
  !+ad_vars  plasmod_nchannels /3/ : leave this at 3
  integer :: plasmod_nchannels = 3
  !+ad_vars  plasmod_i_impmodel /1/ : impurity model: 0 - fixed concentration,
  !+ad_varc  1 - fixed concentration at pedestal top, then fixed density.
  integer :: plasmod_i_impmodel = 1

 !Derived type composition
  !+ad_vars  plasmod_globtau(5) /5.0d0, 5.0d0, 7.0d0, 5.0d0, 1.0d0/ : tauparticle/tauE for D, T, He, Xe, Ar
  !+ad_varc  (NOT used for Xe!)
  real(kind(1.0D0)), dimension(5) :: plasmod_globtau = (/ 5.0d0, 5.0d0, 7.0d0, 5.0d0, 1.0d0 /)
  !+ad_vars  plasmod_psepplh_sup /12000.0d0/ : Psep/PLH if above this, use Xe
  real(kind(1.0D0)) :: plasmod_psepplh_sup = 12000.0d0
  !+ad_vars  plasmod_qdivt /0.0d0/ : divertor heat flux in MW/m^2, if 0, dont use SOL model
  real(kind(1.0D0)) :: plasmod_qdivt = 0.0d0
  !+ad_vars  plasmod_imptype(3) /14, 13, 9/ : Impurities: element 1 - intrinsic impurity, element 2 - Psep control, element 3 - seeding for SOL (defaults: W, Xe, Ar)
  integer, dimension(3) :: plasmod_imptype = (/ 14, 13, 9 /)

 !Derived type inputs
  !+ad_vars  plasmod_qnbi_psepfac /50.0d0/ : dqnbi/d(1-Psep/PLH)
  real(kind(1.0D0)) :: plasmod_qnbi_psepfac = 50.0d0
  !+ad_vars  plasmod_cxe_psepfac /1.0d-4/ : dcxe/d(1-Psep/PLH)
  real(kind(1.0D0)) :: plasmod_cxe_psepfac = 1.0d-4
  !+ad_vars  plasmod_car_qdivt /1.0d-4/ : dcar/d(qdivt)
  real(kind(1.0D0)) :: plasmod_car_qdivt = 1.0d-4
  !+ad_vars  plasmod_maxpauxor /20.0d0/ : max allowed auxiliary power / R
  real(kind(1.0D0)) :: plasmod_maxpauxor = 20.0d0
    !deposition locations
  !+ad_vars  plasmod_x_heat(2) /0.0d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_heat = (/ 0.0d0, 0.0d0 /)
  !+ad_vars  plasmod_x_cd(2) /0.0d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_cd = (/ 0.0d0, 0.0d0 /)
  !+ad_vars  plasmod_x_fus(2) /0.0d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_fus = (/ 0.0d0, 0.0d0 /)
  !+ad_vars  plasmod_x_control(2) /0.0d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_x_control = (/ 0.0d0, 0.0d0 /)
  !+ad_vars  plasmod_dx_heat(2) /0.2d0, 0.03d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_heat = (/ 0.2d0, 0.03d0 /)
  !+ad_vars  plasmod_dx_cd(2) /0.2d0, 0.03/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_cd = (/ 0.2d0, 0.03d0 /)
  !+ad_vars  plasmod_dx_fus(2) /0.2d0, 0.03d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_fus = (/ 0.2d0, 0.03d0 /)
  !+ad_vars  plasmod_dx_control(2) /0.2d0, 0.03d0/ : element 1 - nbi, element 2 - ech
  real(kind(1.0D0)), dimension(2) :: plasmod_dx_control = (/ 0.2d0, 0.03d0 /)
  !+ad_vars  plasmod_contrpovs /0.0d0/ :: control power in Paux/lateral_area (MW/m2)
  real(kind(1.0D0)) :: plasmod_contrpovs = 0.0d0
  !+ad_vars  plasmod_contrpovr /0.0d0/ :: control power in Paux/R (MW/m)
  real(kind(1.0D0)) :: plasmod_contrpovr = 0.0d0
  !+ad_vars  plasmod_nbi_energy /1000.0d0/ :: In keV
  real(kind(1.0D0)) :: plasmod_nbi_energy = 1000.0d0
  !+ad_vars  plasmod_v_loop /-1.0d-6/ :: target loop voltage. If lower than -1.e5 do not use
  real(kind(1.0D0)) :: plasmod_v_loop = -1.0d-6
  !+ad_vars  plasmod_pfus /0.0d0/ :: if 0. not used (otherwise controlled with Pauxheat)
  real(kind(1.0D0)) :: plasmod_pfus = 0.0d0
  !+ad_vars  plasmod_eccdeff /0.3d0/ :: current drive multiplier: CD = eccdeff*PCD*TE/NE (not in use yet)
  real(kind(1.0D0)) :: plasmod_eccdeff = 0.3d0
  !+ad_vars  plasmod_fcdp /-1.0d0/ :: (P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power (iteration variable 147)
  real(kind(1.0D0)) :: plasmod_fcdp = -1.0d0
  !+ad_vars  plasmod_fradc /-1.0d0/ :: Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad) (iteration variable 148)
  real(kind(1.0D0)) :: plasmod_fradc = -1.0d0
  !+ad_vars  plasmod_pech /0.0d0/ :: ech power (not in use yet)
  real(kind(1.0D0)) :: plasmod_pech = 0.0d0
  !+ad_vars  plasmod_gamcdothers /1.0d0/ :: efficiency multiplier for non-CD heating. If 0.0 pheat treated as if it had no current drive associated
  real(kind(1.0D0)) :: plasmod_gamcdothers = 1.0d0
  !+ad_vars  plasmod_chisawpos /-1.0d0/ :: position where artificial sawtooth diffusivity is added, -1 - uses q=1 position
  real(kind(1.0D0)) :: plasmod_chisawpos = -1.0d0
  !+ad_vars  plasmod_chisaw /0.0d0/ :: artificial diffusivity in m^2/s
  real(kind(1.0D0)) :: plasmod_chisaw = 0.0d0
  !+ad_vars  plasmod_sawpertau /1.0d-6/ :: ratio between sawtooth period and confinement time
  real(kind(1.0D0)) :: plasmod_sawpertau = 1.0d-6
  !+ad_vars  plasmod_spellet /0.0d0/ :: pellet mass in units of D in 10^19
  real(kind(1.0D0)) :: plasmod_spellet = 0.0d0
  !+ad_vars  plasmod_fpellet /0.5d0/ :: pellet frequency in Hz
  real(kind(1.0D0)) :: plasmod_fpellet = 0.5d0

  !Derived type pedestal
  !+ad_vars  plasmod_pedscal /1.0d0/ :: multiplication factor of the pedestal scaling in PLASMOD
  !+ad_varc                             can be used to scan the pedestal height.
  real(kind(1.0D0)) :: plasmod_pedscal = 1.0d0


  !+ad_vars  geom ::  Derived type containing all geometry information for PLASMOD
  type (geometry) :: geom
  !+ad_vars  comp ::  Derived type containing all composition information for PLASMOD
  type (composition) :: comp
  !+ad_vars  ped ::  Derived type containing all pedestal information for PLASMOD
  type (pedestal) :: ped
  !+ad_vars  inp0 ::  Derived type containing miscellaneous input information for PLASMOD
  type (inputs) :: inp0
  !+ad_vars  radp ::  Derived type containing all radial profile information for PLASMOD
  type (radial_profiles) :: radp
  !+ad_vars  mhd ::  Derived type containing all mhd information for PLASMOD
  type (MHD_EQ) :: mhd
  !+ad_vars  loss ::  Derived type containing all power loss information for PLASMOD
  type (power_losses) :: loss
  !+ad_vars  num ::  Derived type containing all numerics information for PLASMOD
  type (numerics_transp) :: num
  !+ad_vars  i_flag ::  Error flag for PLASMOD
  integer :: i_flag


end module plasmod_variables


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module current_drive_variables

  !+ad_name  current_drive_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  current drive system
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to tokamak
  !+ad_desc  current drive systems.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  16/10/12 PJK Initial version of module
  !+ad_hist  08/01/13 PJK Modified irfcd comments
  !+ad_hist  14/01/13 PJK Corrected some more comments; removed echpwr0
  !+ad_hist  25/09/13 PJK Added rtanbeam, rtanmax, nbshield
  !+ad_hist  27/11/13 PJK Modified bigq description
  !+ad_hist  06/03/14 PJK Modified gamcd units
  !+ad_hist  26/03/14 PJK Added extra boostrap current fraction variables
  !+ad_hist  01/05/14 PJK Changed bigq description
  !+ad_hist  19/06/14 PJK Added effcd, etacd
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_hist  06/10/14 PJK Added nbshinef
  !+ad_hist  06/10/14 PJK Added forbitloss, porbitlossmw
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  beamwd /0.58/ : width of neutral beam duct where it passes
  !+ad_varc                  between the TF coils (m)
  !+ad_varc    (T Inoue et al, Design of neutral beam system for ITER-FEAT,
  !+ad_varc     <A HREF=http://dx.doi.org/10.1016/S0920-3796(01)00339-8>
  !+ad_varc      Fusion Engineering and Design, Volumes 56-57, October 2001, Pages 517-521</A>)
  real(kind(1.0D0)) :: beamwd = 0.58D0
  !+ad_vars  bigq : Fusion gain; P_fusion / (P_injection + P_ohmic)
  real(kind(1.0D0)) :: bigq = 0.0D0
  !+ad_vars  bootipf : bootstrap current fraction (enforced; see ibss)
  real(kind(1.0D0)) :: bootipf = 0.0D0
  !+ad_vars  bscfmax /0.9/ : maximum fraction of plasma current from bootstrap;
  !+ad_varc                  if bscfmax < 0, bootstrap fraction = abs(bscfmax)
  real(kind(1.0D0)) :: bscfmax = 0.9D0
  !+ad_vars  bscf_iter89 : bootstrap current fraction, ITER 1989 model
  real(kind(1.0D0)) :: bscf_iter89 = 0.0D0
  !+ad_vars  bscf_nevins : bootstrap current fraction, Nevins et al model
  real(kind(1.0D0)) :: bscf_nevins = 0.0D0
  !+ad_vars  bscf_sauter : bootstrap current fraction, Sauter et al model
  real(kind(1.0D0)) :: bscf_sauter = 0.0D0
  !+ad_vars  bscf_wilson : bootstrap current fraction, Wilson et al model
  real(kind(1.0D0)) :: bscf_wilson = 0.0D0
  !+ad_vars  cboot /1.0/ : bootstrap current fraction multiplier (ibss=1)
  real(kind(1.0D0)) :: cboot = 1.0D0
  !+ad_vars  cnbeam : neutral beam current (A)
  real(kind(1.0D0)) :: cnbeam = 0.0D0
  !+ad_vars  echpwr : ECH power (MW)
  real(kind(1.0D0)) :: echpwr = 0.0D0
  !+ad_vars  echwpow : ECH wall plug power (MW)
  real(kind(1.0D0)) :: echwpow = 0.0D0
  !+ad_vars  effcd : current drive efficiency (A/W)
  real(kind(1.0D0)) :: effcd = 0.0D0
  !+ad_vars  enbeam /1.0e3/ : neutral beam energy (keV) (iteration variable 19)
  real(kind(1.0D0)) :: enbeam = 1.0D3
  !+ad_vars  etacd : auxiliary power wall plug to injector efficiency
  real(kind(1.0D0)) :: etacd = 0.0D0
  !+ad_vars  etaech /0.3/ : ECH wall plug to injector efficiency
  real(kind(1.0D0)) :: etaech = 0.3D0
  !+ad_vars  etalh /0.3/ : lower hybrid wall plug to injector efficiency
  real(kind(1.0D0)) :: etalh = 0.3D0
  !+ad_vars  etanbi /0.3/ : neutral beam wall plug to injector efficiency
  real(kind(1.0D0)) :: etanbi = 0.3D0
  !+ad_vars fpion  :  fraction of beam energy to ions
  real(kind(1.0D0)) :: fpion = 0.5D0
  !+ad_vars  pnbitot : neutral beam power entering vacuum vessel
  real(kind(1.0D0)) :: pnbitot = 0.0D0
  !+ad_vars  nbshinemw : neutral beam shine-through power
  real(kind(1.0D0)) :: nbshinemw = 0.0D0
  !+ad_vars  feffcd /1.0/ : current drive efficiency fudge factor (iteration variable 47)
  real(kind(1.0D0)) :: feffcd = 1.0D0
  !+ad_vars  forbitloss /0.0/ : fraction of neutral beam power lost after ionisation but
  !+ad_varc                     before thermalisation (orbit loss fraction)
  real(kind(1.0D0)) :: forbitloss = 0.0D0
  !+ad_vars  frbeam /1.05/ : R_tangential / R_major for neutral beam injection
  real(kind(1.0D0)) :: frbeam = 1.05D0
  !+ad_vars  ftritbm /1.0e-6/ : fraction of beam that is tritium
  real(kind(1.0D0)) :: ftritbm = 1.0D-6
  !+ad_vars  gamcd : normalised current drive efficiency (1.0e20 A/(W m^2))
  real(kind(1.0D0)) :: gamcd = 0.0D0
  !+ad_vars  gamma_ecrh /0.35/ : user input ECRH gamma (1.0e20 A/(W m^2))
  real(kind(1.0D0)) :: gamma_ecrh = 0.35D0
  !+ad_vars  rho_ecrh /0.1/ : normalised minor radius at which electron cyclotron current drive is maximum
  real(kind(1.0D0)) :: rho_ecrh = 0.1D0

  !+ad_vars  iefrf /5/ : switch for current drive efficiency model: <OL>
  !+ad_varc         <LI> Fenstermacher Lower Hybrid
  !+ad_varc         <LI> Ion Cyclotron current drive
  !+ad_varc         <LI> Fenstermacher ECH
  !+ad_varc         <LI> Ehst Lower Hybrid
  !+ad_varc         <LI> ITER Neutral Beam
  !+ad_varc         <LI> new Culham Lower Hybrid model
  !+ad_varc         <LI> new Culham ECCD model
  !+ad_varc         <LI> new Culham Neutral Beam model
  !+ad_varc         <LI> Empty (Oscillating field CD removed)
  !+ad_varc         <LI> ECRH user input gamma
  !+ad_varc         <LI> ECRH "HARE" model (E. Poli, Physics of Plasmas 2019) </OL>
  integer :: iefrf = 5
  !+ad_vars  irfcd /1/ : switch for current drive calculation:<UL>
  !+ad_varc         <LI> = 0 turned off;
  !+ad_varc         <LI> = 1 turned on</UL>
  integer :: irfcd = 1
  !+ad_vars  nbshinef : neutral beam shine-through fraction
  real(kind(1.0D0)) :: nbshinef = 0.0D0
  !+ad_vars  nbshield /0.5/ : neutral beam duct shielding thickness (m)
  real(kind(1.0D0)) :: nbshield = 0.5D0
  !+ad_vars  pheat /0.0/ : heating power not used for current drive (MW)
  !+ad_varc                (iteration variable 11)
  real(kind(1.0D0)) :: pheat = 0.0D0
  !+ad_vars  pinjalw /150.0/ : Maximum allowable value for injected power (MW)
  !+ad_varc                   (constraint equation 30)
  real(kind(1.0D0)) :: pinjalw = 150.0D0
  !+ad_vars  pinjemw : auxiliary injected power to electrons (MW)
  real(kind(1.0D0)) :: pinjemw = 0.0D0
  !+ad_vars  pinjimw : auxiliary injected power to ions (MW)
  real(kind(1.0D0)) :: pinjimw = 0.0D0
  !+ad_vars  pinjmw : total auxiliary injected power (MW)
  real(kind(1.0D0)) :: pinjmw = 0.0D0
  !+ad_vars  plhybd : lower hybrid injection power (MW)
  real(kind(1.0D0)) :: plhybd = 0.0D0
  !+ad_vars  pnbeam : neutral beam injection power (MW)
  real(kind(1.0D0)) :: pnbeam = 0.0D0
  !+ad_vars  porbitlossmw : neutral beam power lost after ionisation but before
  !+ad_varc                 thermalisation (orbit loss power) (MW)
  real(kind(1.0D0)) :: porbitlossmw = 0.0D0
  !+ad_vars  pwplh : lower hybrid wall plug power (MW)
  real(kind(1.0D0)) :: pwplh = 0.0D0
  !+ad_vars  pwpnb : neutral beam wall plug power (MW)
  real(kind(1.0D0)) :: pwpnb = 0.0D0
  !+ad_vars  rtanbeam : neutral beam centreline tangency radius (m)
  real(kind(1.0D0)) :: rtanbeam = 0.0D0
  !+ad_vars  rtanmax : maximum tangency radius for centreline of beam (m)
  real(kind(1.0D0)) :: rtanmax = 0.0D0
  !+ad_vars  taubeam : neutral beam e-decay lengths to plasma centre
  real(kind(1.0D0)) :: taubeam = 0.0D0
  !+ad_vars  tbeamin /3.0/ : permitted neutral beam e-decay lengths to plasma centre
  real(kind(1.0D0)) :: tbeamin = 3.0D0

end module current_drive_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_kallenbach_variables

  !+ad_name  divertor_kallenbach_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  tokamak divertor components, Kallenbach model, issue #400
  !+ad_type  Module
  !+ad_auth  Michael Kovari, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to tokamak
  !+ad_desc  divertor components.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  04/07/16 MDK Initial version of module
  !+ad_stat  Okay
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  kallenbach_switch /0/ : Switch to turn on the 1D Kallenbach divertor model (1=on, 0=off)
  integer :: kallenbach_switch = 0

  !+ad_vars  kallenbach_tests /0/ : Switch to run tests of 1D Kallenbach divertor model (1=on, 0=off)
  integer :: kallenbach_tests = 0

  !+ad_vars  kallenbach_test_option /0/ : Switch to choose kallenbach test option: <UL>
  !+ad_varc             <LI> = 0 Test case with user inputs;
  !+ad_varc             <LI> = 1 Test case for Kallenbach paper;</UL>
  integer :: kallenbach_test_option = 0

  !+ad_vars  kallenbach_scan_switch /0/ : Switch to run scan of 1D Kallenbach divertor model (1=on, 0=off)
  integer :: kallenbach_scan_switch = 0

  !+ad_vars  kallenbach_scan_var /0/ : Switch for parameter to scan for kallenbach scan test:<UL>
  !+ad_varc                  <LI> = 0 ttarget
  !+ad_varc                  <LI> = 1 qtargettotal
  !+ad_varc                  <LI> = 2 targetangle
  !+ad_varc                  <LI> = 3 lambda_q_omp
  !+ad_varc                  <LI> = 4 netau_sol</UL>
  integer :: kallenbach_scan_var = 0

  !+ad_vars  kallenbach_scan_start /2.0/ : Start value for kallenbach scan parameter
  real(kind(1.0D0)) :: kallenbach_scan_start = 2.0

  !+ad_vars  kallenbach_scan_end /10.0/ : End value for kallenbach scan parameter
  real(kind(1.0D0)) :: kallenbach_scan_end = 10.0

  !+ad_vars  kallenbach_scan_num /1/ : Number of scans for kallenbach scan test
  integer :: kallenbach_scan_num = 1

  !+ad_vars  target_spread /0.003/ : Increase in SOL power fall-off length due to spreading, mapped to OMP [m]
  real(kind(1.0D0)) :: target_spread = 0.003D0

  !+ad_vars  lambda_q_omp /0.002/ : SOL power fall-off length at the outer midplane, perpendicular to field [m]
  real(kind(1.0D0)) :: lambda_q_omp = 0.002D0

  !+ad_vars  lcon_factor /1.0/ : Correction factor for connection length from OMP to divertor =
  !+ad_varc                      connection length/(pi*q*rmajor)
  real(kind(1.0D0)) :: lcon_factor = 1.0D0

  !+ad_vars  netau_sol /0.5/ : Parameter describing the departure from local ionisation equilibrium in the SOL. [ms.1e20/m3]
  real(kind(1.0D0)) :: netau_sol = 0.5D0

  !+ad_vars  targetangle /30.0/ : Angle between field-line and divertor target (degrees)
  real(kind(1.0D0)) :: targetangle = 30.0D0

  !+ad_vars  ttarget /2.3/ : Plasma temperature adjacent to divertor sheath [eV] (iteration variable 120)
  real(kind(1.0D0)) :: ttarget = 2.3D0

  !+ad_vars  qtargettotal /5.0e6/ : Power density on target including surface recombination [W/m2]
  !+ad_varc (iteration variable 124)
  real(kind(1.0D0)) :: qtargettotal = 5.0D6

  ! real(kind(1.0D0)) :: helium_enrichment = 1.0D0
  ! real(kind(1.0D0)) :: impurity_enrichment = 5.0D0

  !+ad_vars  impurity_enrichment(14) /5.0/ : Ratio of each impurity concentration in SOL to confined plasma+
  !+ad_varc the enrichment for Argon is also propagated for PLASMOD (ipedestal=3)
  real(kind(1.0D0)), dimension(14) :: impurity_enrichment = 5.0D0

  !+ad_vars  psep_kallenbach : Power conducted through the separatrix, as calculated by the divertor model [W]
  !+ad_varc                    Not equal to pdivt unless constraint is imposed.
  real(kind(1.0D0)) :: psep_kallenbach = 0.0D0

  !+ad_vars  teomp : separatrix temperature calculated by the Kallenbach divertor model [eV]
  real(kind(1.0D0)) :: teomp = 0.0D0

  ! Issue #457
  !+ad_vars  neomp : Mean SOL density at OMP calculated by the Kallenbach divertor model [m-3]
  real(kind(1.0D0)) :: neomp = 0.0D0

  !+ad_vars  neratio /0.75/ : Ratio of mean SOL density at OMP to separatrix density at OMP (iteration variable 121)
  real(kind(1.0D0)) :: neratio = 0.75D0

  !+ad_vars  pressure0 : Total plasma pressure near target (thermal+dynamic) [Pa]
  real(kind(1.0D0)) :: pressure0 = 0.0D0

  !+ad_vars  fractionwidesol /0.1/ : Distance from target at which SOL gets broader as a fraction of connection length
  real(kind(1.0D0)) :: fractionwidesol = 0.1D0

  !+ad_vars  fmom : momentum factor [-]
  real(kind(1.0D0)), public :: fmom

  !+ad_vars  totalpowerlost : Total power lost due to radiation, ionisation and recombination [W]
  real(kind(1.0D0)), public :: totalpowerlost

  !+ad_vars  impuritypowerlost : Power lost due to impurity radiation [W]
  real(kind(1.0D0)), public :: impuritypowerlost

  !+ad_vars  hydrogenicpowerlost : Power lost due to hydrogenic radiation [W]
  real(kind(1.0D0)), public :: hydrogenicpowerlost

  !+ad_vars  exchangepowerlost : Power lost due to charge exchange  [W]
  real(kind(1.0D0)), public :: exchangepowerlost

  !+ad_vars  ionisationpowerlost : Power lost due to electron impact ionisation [W]
  real(kind(1.0D0)), public :: ionisationpowerlost

  !+ad_vars  abserr_sol : Absolute contribution to the error tolerance in the Kallenbach divertor model
  real(kind(1.0D0)), public :: abserr_sol = 1.d-4

  !+ad_vars  relerr_sol : Relative contribution to the error tolerance in the Kallenbach divertor model
  real(kind(1.0D0)), public :: relerr_sol = 1.d-4

  !+ad_vars  mach0 : Mach number at target (must be just less than 1)
  real(kind(1.0D0)), public :: mach0 = 0.999


end module divertor_kallenbach_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_variables

  !+ad_name  divertor_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  tokamak divertor components
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to tokamak
  !+ad_desc  divertor components.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  17/10/12 PJK Initial version of module
  !+ad_hist  13/08/13 PJK Changed hldiv comment (no 'outboard');
  !+ad_hisc               tdiv now an input for stellarators
  !+ad_hist  02/02/17 JM  Replaced rstrko with rspo
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  adas : area divertor / area main plasma (along separatrix)
  real(kind(1.0D0)) :: adas = 0.0D0
  !+ad_vars  anginc /0.262/ : angle of incidence of field line on plate (rad)
  real(kind(1.0D0)) :: anginc = 0.262D0
  !+ad_vars  betai /1.0/ : poloidal plane angle between divertor plate and leg, inboard (rad)
  real(kind(1.0D0)) :: betai = 1.0D0
  !+ad_vars  betao /1.0/ : poloidal plane angle between divertor plate and leg, outboard (rad)
  real(kind(1.0D0)) :: betao = 1.0D0
  !+ad_vars  bpsout /0.6/ : reference B_p at outboard divertor strike point (T)
  real(kind(1.0D0)) :: bpsout = 0.60D0
  !+ad_vars  c1div /0.45/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c1div = 0.45D0
  !+ad_vars  c2div /-7.0/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c2div = -7.0D0
  !+ad_vars  c3div /0.54/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c3div = 0.54D0
  !+ad_vars  c4div /-3.6/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c4div = -3.6D0
  !+ad_vars  c5div /0.7/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c5div = 0.7D0
  !+ad_vars  c6div /0.0/ : fitting coefficient to adjust ptpdiv, ppdiv
  real(kind(1.0D0)) :: c6div = 0.0D0
  !+ad_vars  delld /1.0/ : coeff for power distribution along main plasma
  real(kind(1.0D0)) :: delld = 1.0D0
  !+ad_vars  dendiv : plasma density at divertor (10**20 /m3)
  real(kind(1.0D0)) :: dendiv = 0.0D0
  !+ad_vars  densin : density at plate (on separatrix) (10**20 /m3)
  real(kind(1.0D0)) :: densin = 0.0D0
  !+ad_vars  divclfr /0.3/ : divertor coolant fraction
  real(kind(1.0D0)) :: divclfr = 0.3D0
  !+ad_vars  divdens /1.0e4/ : divertor structure density (kg/m3)
  real(kind(1.0D0)) :: divdens = 1.0D4
  !+ad_vars  divdum /0/ : switch for divertor Zeff model: 0=calc, 1=input
  integer :: divdum = 0
  !+ad_vars  divfix /0.2/ : divertor structure vertical thickness (m)
  real(kind(1.0D0)) :: divfix = 0.2D0
  !+ad_vars  divmas : divertor plate mass (kg)
  real(kind(1.0D0)) :: divmas = 0.0D0
  !+ad_vars  divplt /0.035/ : divertor plate thickness (m) (from Spears, Sept 1990)
  real(kind(1.0D0)) :: divplt = 0.035D0
  !+ad_vars  divsur : divertor surface area (m2)
  real(kind(1.0D0)) :: divsur = 0.0D0
  !+ad_vars  fdfs /10.0/ : radial gradient ratio
  real(kind(1.0D0)) :: fdfs = 10.0D0
  !+ad_vars  fdiva /1.11/ : divertor area fudge factor (for ITER, Sept 1990)
  real(kind(1.0D0)) :: fdiva = 1.11D0
  !+ad_vars  fgamp /1.0/ : sheath potential factor (not used)
  real(kind(1.0D0)) :: fgamp = 1.0D0
  !+ad_vars  fhout : fraction of power to outboard divertor (for single null)
  real(kind(1.0D0)) :: fhout = 0.0D0
  !+ad_vars  fififi /0.004/ : coefficient for gamdiv
  real(kind(1.0D0)) :: fififi = 4.0D-3
  !+ad_vars  frrp /0.4/ : fraction of radiated power to plate
  real(kind(1.0D0)) :: frrp = 0.4D0
  !+ad_vars  hldiv : divertor heat load (MW/m2)
  real(kind(1.0D0)), bind(C) :: hldiv = 0.0D0
  !+ad_vars  hldivlim /5.0/ : heat load limit (MW/m2)
  real(kind(1.0D0)) :: hldivlim = 5.0D0
  !+ad_vars  ksic /0.8/ : power fraction for outboard double-null scrape-off plasma
  real(kind(1.0D0)) :: ksic = 0.8D0
  !+ad_vars  lamp : power flow width (m)
  real(kind(1.0D0)) :: lamp = 0.0D0
  !+ad_vars  minstang : minimum strike angle for heat flux calculation
  real(kind(1.0D0)) :: minstang = 0.0D0
  !+ad_vars  omegan /1.0/ : pressure ratio (nT)_plasma / (nT)_scrape-off
  real(kind(1.0D0)) :: omegan = 1.0D0
  !+ad_vars  omlarg : power spillage to private flux factor
  real(kind(1.0D0)) :: omlarg = 0.0D0
  !+ad_vars  ppdivr : peak heat load at plate (with radiation) (MW/m2)
  real(kind(1.0D0)) :: ppdivr = 0.0D0
  !+ad_vars  prn1 /0.285/ : n-scrape-off / n-average plasma;
  !+ad_varc                 (input for ipedestal=0, = nesep/dene if ipedestal>=1)
  real(kind(1.0D0)) :: prn1 = 0.285D0
  !+ad_vars  ptpdiv : peak temperature at the plate (eV)
  real(kind(1.0D0)) :: ptpdiv = 0.0D0
  !+ad_vars  rconl : connection length ratio, outboard side
  real(kind(1.0D0)) :: rconl = 0.0D0
  !+ad_vars  rlclolcn : ratio of collision length / connection length
  real(kind(1.0D0)) :: rlclolcn = 0.0D0
  !+ad_vars  rlenmax /0.5/ : maximum value for length ratio (rlclolcn) (eqn.22)
  real(kind(1.0D0)) :: rlenmax = 0.5D0
  !+ad_vars  rsrd : effective separatrix/divertor radius ratio
  real(kind(1.0D0)) :: rsrd = 0.0D0
  !+ad_vars  tconl : main plasma connection length (m)
  real(kind(1.0D0)) :: tconl = 0.0D0
  !+ad_vars  tdiv /2.0/ : temperature at divertor (eV)
  !+ad_varc               (input for stellarator only, calculated for tokamaks)
  real(kind(1.0D0)) :: tdiv = 2.0D0
  !+ad_vars  tsep : temperature at the separatrix (eV)
  real(kind(1.0D0)) :: tsep = 0.0D0
  !+ad_vars  xparain /2.1e3/ : parallel heat transport coefficient (m2/s)
  real(kind(1.0D0)) :: xparain = 2.1D3
  !+ad_vars  xpertin /2.0/ : perpendicular heat transport coefficient (m2/s)
  real(kind(1.0D0)) :: xpertin = 2.0D0
  !+ad_vars  zeffdiv /1.0/ : Zeff in the divertor region (if divdum /= 0)
  real(kind(1.0D0)) :: zeffdiv = 1.0D0

end module divertor_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module fwbs_variables

  !+ad_name  fwbs_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  first wall, blanket and shield components
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the first
  !+ad_desc  wall, blanket and shield components.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_hist  09/04/13 PJK Added rdewex, rpf2dewar; changed some labels
  !+ad_hist  17/04/13 PJK Removed fvolcry
  !+ad_hist  09/05/13 PJK Added fwbsshape
  !+ad_hist  16/05/13 PJK Changed default value of fwbsshape
  !+ad_hist  20/05/13 PJK Added KIT PPCS model variables
  !+ad_hist  18/06/13 PJK Changed cryomass description
  !+ad_hist  14/11/13 PJK Changed 'breeding unit' to 'breeding zone'
  !+ad_hist  03/06/14 PJK Added new power flow variables
  !+ad_hist  21/08/14 PJK Added new thermodynamic blanket model variables
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_hist  30/10/14 PJK Changed blkttype default from 1 to 3
  !+ad_hist  05/11/14 PJK Added praddiv etc.
  !+ad_hist  24/11/14 PJK Modified coolwh comments
  !+ad_hist  10/12/14 PJK Modified secondary_cycle, blkttype descriptions
  !+ad_hist  17/12/14 PJK Added irefprop
  !+ad_hist  08/01/15 JM  Changed default blanket now has (li4sio4 and tibe12)
  !+ad_hist  25/02/15 JM  Removed redundant blanket fractions and switches
  !+ad_hist  02/04/15 JM  Removed fwerlim
  !+ad_hist  12/04/15 JM  Removed costr, astr, bstr, estr, lblnkt
  !+ad_hist  01/06/16 JM  Added denw
  !+ad_hist  01/06/16 JM  Added option 4 to iblanket
  !+ad_hist  28/06/18 SIM Added iblnkith (Issue #732)
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  ! General blanket parameters

  !+ad_vars  bktlife : blanket lifetime (years)
  real(kind(1.0D0)), bind(C):: bktlife = 0.0D0

  !+ad_vars  coolmass : mass of water coolant (in shield, blanket,
  !+ad_varc             first wall, divertor) (kg)
  real(kind(1.0D0)) :: coolmass = 0.0D0

  !+ad_vars  vvmass : vacuum vessel mass (kg)
  ! Formerly known as cryomass.
  real(kind(1.0D0)) :: vvmass = 0.0D0

  !+ad_vars  denstl /7800.0/ : density of steel (kg/m3)
  real(kind(1.0D0)) :: denstl = 7800.0D0

  !+ad_vars  denw /19250.0/ : density of tungsten (kg/m3)
  real(kind(1.0D0)) :: denw = 19250.0D0

  !+ad_vars  dewmkg : total mass of vacuum vessel + cryostat (kg)
  real(kind(1.0D0)) :: dewmkg = 0.0D0

  !+ad_vars  emult /1.269/ : energy multiplication in blanket and shield
  !                         (calculated if blktmodel>0)
  real(kind(1.0D0)) :: emult = 1.269D0

  !+ad_vars  emultmw : power due to energy multiplication in blanket and shield [MW]
  real(kind(1.0D0)) :: emultmw = 0.0D0

  !+ad_vars  fblss /0.09705/ : KIT blanket model: steel fraction of breeding zone
  real(kind(1.0D0)) :: fblss = 0.09705D0

  !+ad_vars  fdiv /0.115/ : area fraction taken up by divertor
  real(kind(1.0D0)) :: fdiv = 0.115D0

  !+ad_vars  fhcd /0.0/ : area fraction covered by heating/current drive
  !+ad_varc               apparatus plus diagnostics
  real(kind(1.0D0)) :: fhcd = 0.0D0

  !+ad_vars  fhole /0.0/ : area fraction taken up by other holes (not used)
  real(kind(1.0D0)) :: fhole = 0.0D0

  !+ad_vars  fwbsshape /2/ : first wall, blanket, shield and vacuum vessel shape:<UL>
  !+ad_varc                  <LI> = 1 D-shaped (cylinder inboard + ellipse outboard);
  !+ad_varc                  <LI> = 2 defined by two ellipses</UL>
  integer :: fwbsshape = 2

  !+ad_vars  fwlife : first wall full-power lifetime (y)
  real(kind(1.0D0)) :: fwlife = 0.0D0

  !+ad_vars  fwmass : first wall mass (kg)
  real(kind(1.0D0)) :: fwmass = 0.0D0

  !+ad_vars  fw_armour_mass : first wall armour mass (kg)
  real(kind(1.0D0)) :: fw_armour_mass = 0.0D0

  !+ad_vars  fw_armour_thickness /0.005/ : first wall armour thickness (m)
  real(kind(1.0D0)) :: fw_armour_thickness = 0.005D0

  !+ad_vars  fw_armour_vol : first wall armour volume (m3)
  real(kind(1.0D0)) :: fw_armour_vol = 0.0D0

  !+ad_vars  iblanket /1/ : switch for blanket model: <UL>
  !+ad_varc             <LI> = 1 CCFE HCPB model;
  !+ad_varc             <LI> = 2 KIT HCPB model;
  !+ad_varc             <LI> = 3 CCFE HCPB model with Tritium Breeding Ratio calculation;
  !+ad_varc             <LI> = 4 KIT HCLL model</UL>
  integer :: iblanket = 1

  !+ad_vars  iblnkith /1/ : switch for inboard blanket: <UL>
  !+ad_varc             <LI> = 0 No inboard blanket (blnkith=0.0);
  !+ad_varc             <LI> = 1 Inboard blanket present</UL>
  integer :: iblnkith = 1

  !+ad_vars  inuclear /0/ : switch for nuclear heating in the coils: <UL>
  !+ad_varc             <LI> = 0 Frances Fox model (default);
  !+ad_varc             <LI> = 1 Fixed by user (qnuc)</UL>
  integer :: inuclear = 0
  !+ad_vars  qnuc /0.0/ : nuclear heating in the coils (W) (inuclear=1)
  real(kind(1.0D0)) :: qnuc = 0.0D0

  !+ad_vars  li6enrich /30.0/ : lithium-6 enrichment of breeding material (%)
  real(kind(1.0D0)) :: li6enrich = 30.0D0

  !+ad_vars  pnucblkt : nuclear heating in the blanket (MW)
  real(kind(1.0D0)) :: pnucblkt = 0.0D0

  !+ad_vars  pnuccp : nuclear heating in the ST centrepost (MW)
  real(kind(1.0D0)) :: pnuccp = 0.0D0

  !+ad_vars  pnucdiv : nuclear heating in the divertor (MW)
  real(kind(1.0D0)) :: pnucdiv = 0.0D0

  !+ad_vars  pnucfw : nuclear heating in the first wall (MW)
  real(kind(1.0D0)) :: pnucfw = 0.0D0

  !+ad_vars  pnuchcd : nuclear heating in the HCD apparatus and diagnostics (MW)
  real(kind(1.0D0)) :: pnuchcd = 0.0D0

  !+ad_vars  pnucloss : nuclear heating lost via holes (MW)
  real(kind(1.0D0)) :: pnucloss = 0.0D0

  !+ad_vars  pnucloss : nuclear heating to vacuum vessel and beyond(MW)
  real(kind(1.0D0)) :: pnucvvplus = 0.0D0

  !+ad_vars  pnucshld : nuclear heating in the shield (MW)
  real(kind(1.0D0)) :: pnucshld = 0.0D0

  !+ad_vars  whtblkt : mass of blanket (kg)
  real(kind(1.0D0)) :: whtblkt = 0.0D0

  !+ad_vars  whtblss : mass of blanket - steel part (kg)
  real(kind(1.0D0)) :: whtblss = 0.0D0

  !+ad_vars  armour_fw_bl_mass : Total mass of armour, first wall and blanket (kg)
  real(kind(1.0D0)) :: armour_fw_bl_mass = 0.0D0

  ! CCFE HCPB Blanket Model (with or without TBR calculation)

  !+ad_vars  <P><B>The following are used only in the CCFE HCPB blanket model
  !+ad_varc  (iblanket=1):</B><P>

  !+ad_vars  breeder_f /0.5/ :  Volume ratio: Li4SiO4/(Be12Ti+Li4SiO4) (iteration variable 108)
  real(kind(1.0D0)) :: breeder_f = 0.5D0
  !+ad_vars  breeder_multiplier /0.75/ : combined breeder/multipler fraction of blanket by volume
  real(kind(1.0D0)) :: breeder_multiplier = 0.75D0
  !+ad_vars  vfcblkt /0.05295/ : He coolant fraction of blanket by volume
  !+ad_varc                   (iblanket = 1 or 3 (CCFE HCPB))
  real(kind(1.0D0)) :: vfcblkt = 0.05295D0
  !+ad_vars  vfpblkt /0.1/ : He purge gas fraction of blanket by volume
  !+ad_varc                   (iblanket = 1 or 3 (CCFE HCPB))
  real(kind(1.0D0)) :: vfpblkt = 0.1D0
  !+ad_vars  whtblli4sio4 : mass of lithium orthosilicate in blanket (kg)
  !+ad_varc                   (iblanket = 1 or 3 (CCFE HCPB))
  real(kind(1.0D0)) :: whtblli4sio4 = 0.0D0
  !+ad_vars  whtbltibe12 : mass of titanium beryllide in blanket (kg)
  !+ad_varc                   (iblanket = 1 or 3 (CCFE HCPB))
  real(kind(1.0D0)) :: whtbltibe12 = 0.0D0

  !  KIT HCPB blanket model

  !+ad_vars  <P><B>The following are used in the KIT HCPB blanket model
  !+ad_varc  (iblanket=2):</B><P>

  !+ad_vars  breedmat /1/ : breeder material switch (iblanket=2 (KIT HCPB)):<UL>
  !+ad_varc                  <LI> = 1 Lithium orthosilicate;
  !+ad_varc                  <LI> = 2 Lithium methatitanate;
  !+ad_varc                  <LI> = 3 Lithium zirconate</UL>
  integer :: breedmat = 1
  !+ad_vars  densbreed : density of breeder material (kg/m3) (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: densbreed = 0.0D0
  !+ad_vars  fblbe /0.6/ : beryllium fraction of blanket by volume
  !+ad_varc                (if (iblanket=2 (KIT HCPB)), Be fraction of breeding zone)
  real(kind(1.0D0)) :: fblbe = 0.6D0
  !+ad_vars  fblbreed /0.154/ : breeder fraction of blanket breeding zone by volume
  !+ad_varc                     (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblbreed = 0.154D0
  !+ad_vars  fblhebmi /0.40/ : helium fraction of inboard blanket box manifold by volume
  !+ad_varc                     (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblhebmi = 0.4D0
  !+ad_vars  fblhebmo /0.40/ : helium fraction of outboard blanket box manifold by volume
  !+ad_varc                     (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblhebmo = 0.4D0
  !+ad_vars  fblhebpi /0.6595/ : helium fraction of inboard blanket back plate by volume
  !+ad_varc                     (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblhebpi = 0.6595D0
  !+ad_vars  fblhebpo /0.6713/ : helium fraction of outboard blanket back plate by volume
  !+ad_varc                     (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: fblhebpo = 0.6713D0
  !+ad_vars  hcdportsize /1/ : size of heating/current drive ports (iblanket=2 (KIT HCPB)): <UL>
  !+ad_varc                  <LI> = 1 'small'
  !+ad_varc                  <LI> = 2 'large'</UL>
  integer :: hcdportsize = 1
  !+ad_vars  nflutf : peak fast neutron fluence on TF coil superconductor (n/m2)
  !+ad_varc           (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: nflutf = 0.0D0
  !+ad_vars  npdiv /2/ : number of divertor ports (iblanket=2 (KIT HCPB))
  integer :: npdiv = 2
  !+ad_vars  nphcdin /2/ : number of inboard ports for heating/current drive
  !+ad_varc                (iblanket=2 (KIT HCPB))
  integer :: nphcdin = 2
  !+ad_vars  nphcdout /2/ : number of outboard ports for heating/current drive
  !+ad_varc                 (iblanket=2 (KIT HCPB))
  integer :: nphcdout = 2
  !+ad_vars  tbr : tritium breeding ratio (iblanket=2,3 (KIT HCPB/HCLL))
  real(kind(1.0D0)) :: tbr = 0.0D0
  !+ad_vars  tritprate : tritium production rate (g/day) (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: tritprate = 0.0D0
  !+ad_vars  vvhemax : maximum helium concentration in vacuum vessel at end of
  !+ad_varc            plant life (appm) (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: vvhemax = 0.0D0
  !+ad_vars  wallpf /1.21/ : neutron wall load peaking factor (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: wallpf = 1.21D0
  !+ad_vars  whtblbreed : mass of blanket - breeder part (kg) (iblanket=2 (KIT HCPB))
  real(kind(1.0D0)) :: whtblbreed = 0.0D0
  !+ad_vars  whtblbe : mass of blanket - beryllium part (kg)
  real(kind(1.0D0)) :: whtblbe = 0.0D0

  !+ad_vars  <P><B>CCFE HCPB model with Tritium Breeding Ratio calculation
  !+ad_varc  (iblanket=3):</B><P>

  !+ad_vars  tbrmin /1.1/ : minimum tritium breeding ratio (constraint equation 52)
  !+ad_varc                 (If iblanket=1, tbrmin=minimum 5-year time-averaged tritium breeding ratio)

  !+ad_vars  iblanket_thickness /2/ : Blanket thickness switch:<UL>
  !+ad_varc     <LI> = 1 thin    0.53 m inboard, 0.91 m outboard
  !+ad_varc     <LI> = 2 medium  0.64 m inboard, 1.11 m outboard
  !+ad_varc     <LI> = 3 thick   0.75 m inboard, 1.30 m outboard</UL>
  !+ad_vars  Do not set blnkith, blnkoth, fwith or fwoth when iblanket=3.
  integer :: iblanket_thickness = 2

  !+ad_vars  primary_pumping /2/ : Switch for pumping power for primary coolant (06/01/2016):
  !+ad_varc       (mechanical power only)<UL>
  !+ad_varc     <LI> = 0 User sets pump power directly (htpmw_blkt, htpmw_fw, htpmw_div, htpmw_shld)
  !+ad_varc     <LI> = 1 User sets pump power as a fraction of thermal power (fpumpblkt, fpumpfw, fpumpdiv, fpumpshld)
  !+ad_varc     <LI> = 2 Mechanical pumping power is calculated
  !+ad_varc     <LI> = 3 Mechanical pumping power is calculated using specified pressure drop</UL>
  !+ad_vars  (peak first wall temperature is only calculated if primary_pumping = 2)
  integer :: primary_pumping = 2

  !+ad_vars  secondary_cycle /0/ : Switch for power conversion cycle:<UL>
  !+ad_varc     <LI> = 0 Set efficiency for chosen blanket, from detailed models (divertor heat not used)
  !+ad_varc     <LI> = 1 Set efficiency for chosen blanket, from detailed models (divertor heat used)
  !+ad_varc     <LI> = 2 user input thermal-electric efficiency (etath)
  !+ad_varc     <LI> = 3 steam Rankine cycle
  !+ad_varc     <LI> = 4 supercritical CO2 cycle</UL>
  integer :: secondary_cycle = 0
  !+ad_vars  coolwh : Blanket coolant (set via blkttype):<UL>
  !+ad_varc         <LI> = 1 helium;
  !+ad_varc         <LI> = 2 pressurized water</UL>
  integer, bind(C) :: coolwh = 1
  !+ad_vars  afwi /0.008/ : inner radius of inboard first wall/blanket coolant channels OBSOLETE (m)
  real(kind(1.0D0)) :: afwi = 0.008D0
  !+ad_vars  afwo /0.008/ : inner radius of outboard first wall/blanket coolant channels OBSOLETE (m)
  real(kind(1.0D0)) :: afwo = 0.008D0

  ! MDK New first wall calculation
  !+ad_vars  fwcoolant /helium/ : first wall coolant (can be different from blanket coolant)
  !+ad_varc                       'helium' or 'water'  (27/11/2015)
  character(len=6) :: fwcoolant = 'helium'
  !+ad_vars  fw_wall /0.003/ : wall thickness of first wall coolant channels (m) (27/11/2015)
  real(kind(1.0D0)) :: fw_wall = 0.003D0
  !+ad_vars  afw /0.006/ : radius of first wall cooling channels (m) (27/11/15)
  real(kind(1.0D0)) :: afw = 0.006D0
  !+ad_vars  pitch /0.020/ : pitch of first wall cooling channels (m) (27/11/15)
  real(kind(1.0D0)) :: pitch = 0.020D0
  !+ad_vars  fwinlet /573/ : inlet temperature of first wall coolant (K) (27/11/2015)
  real(kind(1.0D0)) :: fwinlet = 573.0D0
  !+ad_vars  fwoutlet /823/ : outlet temperature of first wall coolant (K) (27/11/2015)
  real(kind(1.0D0)) :: fwoutlet = 823.0D0
  !+ad_vars  fwpressure /15.5e6/ : first wall coolant pressure (Pa) (secondary_cycle>1)
  real(kind(1.0D0)) :: fwpressure = 15.5D6
  !+ad_vars  tpeak : peak first wall temperature (K) (27/11/2015)
  real(kind(1.0D0)) :: tpeak = 873.0D0
  !+ad_vars  roughness /1e-6/ : first wall channel roughness epsilon (m) (27/11/2015)
  real(kind(1.0D0)) :: roughness = 1.0D-6
  !+ad_vars  fw_channel_length /4.0/ : Length of a single first wall channel (all in parallel) (m) (27/11/2015)
  !+ad_varc                            (iteration variable 114, useful for constraint equation 39)
  real(kind(1.0D0)) :: fw_channel_length = 4.0D0
  !+ad_vars  peaking_factor /1.0/ : peaking factor for first wall heat loads (27/11/2015)
  !+ad_varc                         (Applied separately to inboard and outboard loads.
  !+ad_varc                         Applies to both neutron and surface loads.
  !+ad_varc                         Only used to calculate peak temperature - not the coolant flow rate.)
  real(kind(1.0D0)) :: peaking_factor = 1.0D0

  ! MDK Blanket has not changed as much, but some new variable names
  !+ad_vars  blpressure /15.5e6/ : blanket coolant pressure (Pa) (secondary_cycle>1) (27/11/2015)
  real(kind(1.0D0)) :: blpressure = 15.5D6
  !+ad_vars  inlet_temp /573.0/ : inlet temperature of blanket coolant  (K) (secondary_cycle>1) (27/11/2015)
  real(kind(1.0D0)) :: inlet_temp = 573.0D0
  !+ad_vars  outlet_temp /823.0/ : outlet temperature of blanket coolant (K) (27/11/2015)<UL>
  !+ad_varc         <LI> (secondary_cycle>1);
  !+ad_varc         <LI> input if coolwh=1 (helium), calculated if coolwh=2 (water)</UL>
  real(kind(1.0D0)) :: outlet_temp = 823.0D0


  !+ad_vars  coolp /15.5e6/ : blanket coolant pressure (Pa) stellarator ONLY (27/11/2015)
  real(kind(1.0D0)) :: coolp = 15.5D6


  !+ad_vars  nblktmodpo /8/ : number of outboard blanket modules in poloidal direction (secondary_cycle>1)
  integer :: nblktmodpo = 8
  !+ad_vars  nblktmodpi /7/ : number of inboard blanket modules in poloidal direction (secondary_cycle>1)
  integer :: nblktmodpi = 7
  !+ad_vars  nblktmodto /48/ : number of outboard blanket modules in toroidal direction (secondary_cycle>1)
  integer :: nblktmodto = 48
  !+ad_vars  nblktmodti /32/ : number of inboard blanket modules in toroidal direction (secondary_cycle>1)
  integer :: nblktmodti = 32
  !+ad_vars  tfwmatmax /823.0/ : maximum temperature of first wall material (K) (secondary_cycle>1)
  real(kind(1.0D0)) :: tfwmatmax = 823.0D0
  !+ad_vars  fw_th_conductivity /28.34/ : thermal conductivity of first wall material at
  !+ad_varc        293 K (W/m/K) (Temperature dependence is as for unirradiated Eurofer)
  real(kind(1.0D0)) :: fw_th_conductivity = 28.34D0


  !+ad_vars  fvoldw /1.74/ : area coverage factor for vacuum vessel volume
  real(kind(1.0D0)) :: fvoldw = 1.74D0
  !+ad_vars  fvolsi /1.0/ : area coverage factor for inboard shield volume
  real(kind(1.0D0)) :: fvolsi = 1.0D0
  !+ad_vars  fvolso /0.64/ : area coverage factor for outboard shield volume
  real(kind(1.0D0)) :: fvolso = 0.64D0
  !+ad_vars  fwclfr /0.15/ : first wall coolant fraction
  !+ad_varc                  (calculated if lpulse=1 or ipowerflow=1)
  real(kind(1.0D0)) :: fwclfr = 0.15D0
  !+ad_vars  praddiv : radiation power incident on the divertor (MW)
  real(kind(1.0D0)) :: praddiv = 0.0D0
  !+ad_vars  pradfw : radiation power incident on the divertor (MW)
  real(kind(1.0D0)) :: pradfw = 0.0D0
  !+ad_vars  pradhcd : radiation power incident on the divertor (MW)
  real(kind(1.0D0)) :: pradhcd = 0.0D0
  !+ad_vars  pradloss : radiation power incident on the divertor (MW)
  real(kind(1.0D0)) :: pradloss = 0.0D0
  !+ad_vars  ptfnuc : nuclear heating in the TF coil (MW)
  real(kind(1.0D0)) :: ptfnuc = 0.0D0
  !+ad_vars  ptfnucpm3 : nuclear heating in the TF coil (MW/m3) (blktmodel>0)
  real(kind(1.0D0)) :: ptfnucpm3 = 0.0D0
  !+ad_vars  rdewex : cryostat radius (m)
  real(kind(1.0D0)) :: rdewex = 0.0D0
  !+ad_vars  zdewex : cryostat height (m)
  real(kind(1.0D0)) :: zdewex = 0.0D0
  !+ad_vars  rpf2dewar /0.5/ : radial distance between outer edge of largest
  !+ad_varc                    ipfloc=3 PF coil (or stellarator modular coil)
  !+ad_varc                    and cryostat (m)
  real(kind(1.0D0)) :: rpf2dewar = 0.5D0
  !+ad_vars  vdewex : cryostat volume (m3)
  real(kind(1.0D0)) :: vdewex = 0.0D0
  !+ad_vars  vdewin : vacuum vessel volume (m3)
  real(kind(1.0D0)) :: vdewin = 0.0D0
  !+ad_vars  vfshld /0.25/ : coolant void fraction in shield
  real(kind(1.0D0)) :: vfshld = 0.25D0
  !+ad_vars  volblkt : volume of blanket (m3)
  real(kind(1.0D0)) :: volblkt = 0.0D0
  !+ad_vars  volblkti : volume of inboard blanket (m3)
  real(kind(1.0D0)) :: volblkti = 0.0D0
  !+ad_vars  volblkto : volume of outboard blanket (m3)
  real(kind(1.0D0)) :: volblkto = 0.0D0
  !+ad_vars  volshld : volume of shield (m3)
  real(kind(1.0D0)) :: volshld = 0.0D0
  !+ad_vars  whtshld : mass of shield (kg)
  real(kind(1.0D0)) :: whtshld = 0.0D0
  !+ad_vars  wpenshld : mass of the penetration shield (kg)
  real(kind(1.0D0)) :: wpenshld = 0.0D0
  !+ad_vars  wtshldi : mass of inboard shield (kg)
  real(kind(1.0D0)) :: wtshldi = 0.0D0
  !+ad_vars  wtshldo : mass of outboard shield (kg)
  real(kind(1.0D0)) :: wtshldo = 0.0D0

  !+ad_vars  irefprop /1/ : obsolete
  integer :: irefprop = 1

  real(kind(1.0D0)) :: fblli = 0.0D0
  !+ad_vars  fblli2o /0.08/ : lithium oxide fraction of blanket by volume
  !+ad_varc                   (blktmodel=0)
  real(kind(1.0D0)) :: fblli2o = 0.08D0
  !+ad_vars  fbllipb /0.68/ : lithium lead fraction of blanket by volume
  !+ad_varc                   (blktmodel=0)
  real(kind(1.0D0)) :: fbllipb = 0.68D0
  !+ad_vars  fblvd /0.0/ : vanadium fraction of blanket by volume
  !+ad_varc                (blktmodel=0)
  real(kind(1.0D0)) :: fblvd = 0.0D0
  !+ad_vars  wtblli2o : mass of blanket - Li_2O part (kg)
  real(kind(1.0D0)) :: wtblli2o = 0.0D0
  !+ad_vars  wtbllipb : mass of blanket - Li-Pb part (kg)
  real(kind(1.0D0)) :: wtbllipb = 0.0D0
  !+ad_vars  whtblvd : mass of blanket - vanadium part (kg)
  real(kind(1.0D0)) :: whtblvd = 0.0D0
  !+ad_vars  whtblli : mass of blanket - lithium part (kg)
  real(kind(1.0D0)) :: whtblli = 0.0D0
  !+ad_vars  vfblkt /0.25/ : coolant void fraction in blanket (blktmodel=0),
  !+ad_varc                  (calculated if blktmodel > 0)
  real(kind(1.0D0)) :: vfblkt = 0.25D0
  !+ad_vars  blktmodel /0/ : switch for blanket/tritium breeding model
  !+ad_varc                  (but see <CODE>iblanket</CODE>):<UL>
  !+ad_varc             <LI> = 0 original simple model;
  !+ad_varc             <LI> = 1 KIT model based on a helium-cooled pebble-bed
  !+ad_varc                      blanket (HCPB) reference design</UL>
  integer :: blktmodel = 0
  !+ad_vars  declblkt /0.075/ : neutron power deposition decay length of blanket structural material (m)
  !+ad_varc  (Stellarators only)
  real(kind(1.0D0)) :: declblkt = 0.075D0
  !+ad_vars  declfw /0.075/ : neutron power deposition decay length of first wall structural material (m)
  !+ad_varc (Stellarators only)
  real(kind(1.0D0)) :: declfw = 0.075D0
  !+ad_vars  declshld /0.075/ : neutron power deposition decay length of shield structural material (m)
  !+ad_varc  (Stellarators only)
  real(kind(1.0D0)) :: declshld = 0.075D0
  !+ad_vars  blkttype /3/ : Switch for blanket type:<UL>
  !+ad_varc            <LI> = 1 WCLL; efficiency taken from WP13-DAS08-T02, EFDA_D_2M97B7
  !+ad_varc            <LI> = 2 HCLL; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX
  !+ad_varc            <LI> = 3 HCPB; efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX</UL>
  integer :: blkttype = 3

  !+ad_vars  etaiso /0.85/ : isentropic efficiency of FW and blanket coolant pumps
  real(kind(1.0D0)) :: etaiso = 0.85D0
  !+ad_vars  etahtp /0.95/ : electrical efficiency of primary coolant pumps
  real(kind(1.0D0)) :: etahtp = 0.95D0


end module fwbs_variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module primary_pumping_variables

  !+ad_name  primary_pumping_variables
  !+ad_summ  Module containing global variables relating to the priamry_pumping
  !+ad_summ  primary_pumping=3 option  (Mechanical pumping power is calculated using specified pressure drop)
  !+ad_type  Module
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  primary pumping information
  !+ad_prob  None
  !+ad_call  None
  !+ad_stat  Okay
  !+ad_docs
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  ! Issue #503
  !+ad_vars  gamma_he /1.667/ FIX : ratio of specific heats for helium (primary_pumping=3)
  real(kind(1.0D0)) :: gamma_he = 1.667D0
  !+ad_vars  cp_he /5195/ FIX: specific heat capacity at constant pressure: helium (primary_pumping=3) [J/(kg.K)]
  real(kind(1.0D0)) :: cp_he = 5195.0D0
  !+ad_vars  t_in_bb /573.13/ FIX: temperature in FW and blanket coolant at blanket entrance (primary_pumping=3) [K]
  real(kind(1.0D0)) :: t_in_bb =573.13D0
  !+ad_vars  t_out_bb /773.13/ FIX: temperature in FW and blanket coolant at blanket exit (primary_pumping=3) [K]
  real(kind(1.0D0)) :: t_out_bb =773.13D0
  !+ad_vars  p_he /8.0e6/ FIX: pressure in FW and blanket coolant at pump exit (primary_pumping=3) [Pa]
  real(kind(1.0D0)) :: p_he =8.0D6
  !+ad_vars  dp_he /5.5e5/ FIX: pressure drop in FW and blanket coolant including heat exchanger and pipes (primary_pumping=3) [Pa]
  real(kind(1.0D0)) :: dp_he =5.5D5
  !+ad_vars  htpmw_fw_blkt : mechanical pumping power for FW and blanket including heat exchanger and pipes (primary_pumping=3) [MW]
  real(kind(1.0D0)) :: htpmw_fw_blkt = 0.0d0

end module primary_pumping_variables
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pfcoil_variables

  !+ad_name  pfcoil_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  poloidal field coil systems
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  poloidal field coil systems of a fusion power plant.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_hist  15/04/13 PJK Added sigpfcf
  !+ad_hist  16/04/13 PJK Added sigpfcalw
  !+ad_hist  17/04/13 PJK Removed cohbof; changed fcohbof initial value
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_hist  22/09/14 PJK Attempted to clarify zref description
  !+ad_hist  16/10/14 PJK Added pfcaseth,isumatoh,fcupfsu,awpoh
  !+ad_hist  20/10/14 PJK Added alstroh
  !+ad_hist  06/11/14 PJK Added areaoh,jstrandoh_bop,jstrandoh_eof,jscoh_bop,jscoh_eof
  !+ad_hist  11/11/14 PJK Changed default values for fcuohsu, vfohc
  !+ad_hist  11/11/14 PJK Added tmargoh
  !+ad_hist  22/04/15 JM  Added etapsu, pfwp and pfsec
  !+ad_hist  24/02/17 JM  Added oh_steel_frac
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  ngrpmx /8/ FIX : maximum number of groups of PF coils
  integer, parameter :: ngrpmx = 8
  !+ad_vars  nclsmx /2/ FIX : maximum number of PF coils in a given group
  integer, parameter :: nclsmx = 2
  !+ad_vars  nptsmx /32/ FIX : maximum number of points across the midplane of the
  !+ad_varc           plasma at which the field from the PF coils is fixed
  integer, parameter :: nptsmx = 32
  !+ad_vars  nfixmx /64/ FIX : maximum number of fixed current PF coils
  integer, parameter :: nfixmx = 64

  integer, parameter :: ngc = ngrpmx*nclsmx
  integer, parameter :: ngc2 = ngc+2

  !+ad_vars  alfapf /5.0e-10/ : smoothing parameter used in PF coil
  !+ad_varc                     current calculation at the beginning of pulse (BoP)
  real(kind(1.0D0)) :: alfapf = 5.0D-10

  !+ad_vars  alstroh /4.0D8/ : allowable hoop stress in Central Solenoid structural material (Pa)
  real(kind(1.0D0)) :: alstroh = 4.0D8

  !+ad_vars  i_cs_stress /0/ : Switch for CS stress calculation:<UL>
  !+ad_varc                   <LI> = 0 Hoop stress only;
  !+ad_varc                   <LI> = 1 Hoop + Axial stress</UL>
  integer :: i_cs_stress = 0

  !+ad_vars  areaoh : central solenoid cross-sectional area (m2)
  real(kind(1.0D0)) :: areaoh = 0.0D0
  !+ad_vars  awpoh : central solenoid conductor+void area (m2)
  real(kind(1.0D0)) :: awpoh = 0.0D0
  !+ad_vars  bmaxoh : maximum field in central solenoid at end of flat-top (EoF) (T)
  real(kind(1.0D0)) :: bmaxoh = 0.0D0
  !+ad_vars  bmaxoh0 : maximum field in central solenoid at beginning of pulse (T)
  real(kind(1.0D0)) :: bmaxoh0 = 0.0D0
  !+ad_vars  bpf(ngc2) : peak field at coil i (T)
  real(kind(1.0D0)), dimension(ngc2) :: bpf = 0.0D0
  !+ad_vars  cohbop : central solenoid overall current density at beginning of pulse (A/m2)
  real(kind(1.0D0)) :: cohbop = 0.0D0
  !+ad_vars  coheof /1.85e7/ : central solenoid overall current density at end of flat-top (A/m2)
  !+ad_varc                    (iteration variable 37)
  real(kind(1.0D0)) :: coheof = 1.85D7
  !+ad_vars  cpt(ngc2,6) : current per turn in coil i at time j (A)
  real(kind(1.0D0)), dimension(ngc2,6) :: cpt = 0.0D0
  !+ad_vars  cptdin(ngc2) /4.0e4/: peak current per turn input for PF coil i (A)
  real(kind(1.0D0)), dimension(ngc2) :: cptdin = 4.0D4
  !+ad_vars  curpfb(ngc2) : work array
  real(kind(1.0D0)), dimension(ngc2) :: curpfb = 0.0D0
  !+ad_vars  curpff(ngc2) : work array
  real(kind(1.0D0)), dimension(ngc2) :: curpff = 0.0D0
  !+ad_vars  curpfs(ngc2) : work array
  real(kind(1.0D0)), dimension(ngc2) :: curpfs = 0.0D0
  !+ad_vars  etapsu /0.9/ : Efficiency of transfer of PF stored energy into or out of storage.
  real(kind(1.0D0)) :: etapsu = 0.9D0
  !+ad_vars  fcohbof : ratio of central solenoid overall current density at
  !+ad_varc            beginning of flat-top / end of flat-top
  real(kind(1.0D0)) :: fcohbof = 0.0D0
  !+ad_vars  fcohbop /0.9/ : ratio of central solenoid overall current density at
  !+ad_varc                  beginning of pulse / end of flat-top
  !+ad_varc                  (iteration variable 41)
  real(kind(1.0D0)) :: fcohbop = 0.9D0
  !+ad_vars  fcuohsu /0.7/ : copper fraction of strand in central solenoid
  real(kind(1.0D0)) :: fcuohsu = 0.7D0
  !+ad_vars  fcupfsu /0.69/ : copper fraction of cable conductor (PF coils)
  real(kind(1.0D0)) :: fcupfsu = 0.69D0
  !+ad_vars  ipfloc(ngc) /2,2,3/ : switch for locating scheme of PF coil group i:<UL>
  !+ad_varc                   <LI> = 1 PF coil on top of central solenoid;
  !+ad_varc                   <LI> = 2 PF coil on top of TF coil;
  !+ad_varc                   <LI> = 3 PF coil outside of TF coil</UL>
  integer, dimension(ngc) :: ipfloc = (/2,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  !+ad_vars  ipfres /0/ : switch for PF coil type:<UL>
  !+ad_varc          <LI> = 0 superconducting PF coils;
  !+ad_varc          <LI> = 1 resistive PF coils</UL>
  integer :: ipfres = 0
  !+ad_vars  itr_sum : total sum of I x turns x radius for all PF coils and CS (Am)
  real(kind(1.0D0)) :: itr_sum = 0.0D0
  !+ad_vars  isumatoh /1/ : switch for superconductor material in central solenoid:<UL>
  !+ad_varc            <LI> = 1 ITER Nb3Sn critical surface model with standard
  !+ad_varc                     ITER parameters;
  !+ad_varc            <LI> = 2 Bi-2212 high temperature superconductor (range of
  !+ad_varc                     validity T < 20K, adjusted field b < 104 T, B > 6 T);
  !+ad_varc            <LI> = 3 NbTi;
  !+ad_varc            <LI> = 4 ITER Nb3Sn model with user-specified parameters
  !+ad_varc            <LI> = 5 WST Nb3Sn parameterisation
  !+ad_varc            <LI> = 6 REBCO HTS parameterisation</UL>
  integer :: isumatoh = 1
  !+ad_vars  isumatpf /1/ : switch for superconductor material in PF coils:<UL>
  !+ad_varc            <LI> = 1 ITER Nb3Sn critical surface model with standard
  !+ad_varc                     ITER parameters;
  !+ad_varc            <LI> = 2 Bi-2212 high temperature superconductor (range of
  !+ad_varc                     validity T < 20K, adjusted field b < 104 T, B > 6 T);
  !+ad_varc            <LI> = 3 NbTi;
  !+ad_varc            <LI> = 4 ITER Nb3Sn model with user-specified parameters
  !+ad_varc            <LI> = 5 WST Nb3Sn parameterisation</UL>

  integer :: isumatpf = 1
  !+ad_vars  jscoh_bop : central solenoid superconductor critical current density (A/m2)
  !+ad_varc                  at beginning-of-pulse
  real(kind(1.0D0)) :: jscoh_bop = 0.0D0
  !+ad_vars  jscoh_eof : central solenoid superconductor critical current density (A/m2)
  !+ad_varc                  at end-of-flattop
  real(kind(1.0D0)) :: jscoh_eof = 0.0D0
  !+ad_vars  jstrandoh_bop : central solenoid strand critical current density (A/m2)
  !+ad_varc                  at beginning-of-pulse
  real(kind(1.0D0)) :: jstrandoh_bop = 0.0D0
  !+ad_vars  jstrandoh_eof : central solenoid strand critical current density (A/m2)
  !+ad_varc                  at end-of-flattop
  real(kind(1.0D0)) :: jstrandoh_eof = 0.0D0
  !+ad_vars  ncirt : number of PF circuits (including central solenoid and plasma)
  integer :: ncirt = 0
  !+ad_vars  ncls(ngrpmx+2) /1,1,2/ : number of PF coils in group j
  integer, dimension(ngrpmx+2) :: ncls = (/1,1,2,0,0,0,0,0,0,0/)
  !+ad_vars  nfxfh /7/ : number of filaments the top and bottom of the central solenoid
  !+ad_varc              should be broken into during scaling (5 - 10 is good)
  integer :: nfxfh = 7
  !+ad_vars  ngrp /3/ : number of groups of PF coils.
  !+ad_varc             Symmetric coil pairs should all be in the same group
  integer :: ngrp = 3
  !+ad_vars  nohc : number of PF coils (excluding the central solenoid) + 1
  integer :: nohc = 0
  !+ad_vars  ohhghf /0.71/ : central solenoid height / TF coil internal height
  real(kind(1.0D0)) :: ohhghf = 0.71D0
  !+ad_vars  oh_steel_frac /0.5/ : central solenoid steel fraction (iteration variable 122)
  real(kind(1.0D0)) :: oh_steel_frac = 0.5D0
  !+ad_vars  pfcaseth(ngc2) : steel case thickness for PF coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: pfcaseth = 0.0D0
  !+ad_vars  pfclres /2.5e-8/ : PF coil resistivity (if ipfres=1) (Ohm-m)
  real(kind(1.0D0)) :: pfclres = 2.5D-8
  !+ad_vars  pfmmax : mass of heaviest PF coil (tonnes)
  real(kind(1.0D0)) :: pfmmax = 0.0D0
  !+ad_vars  pfrmax : radius of largest PF coil (m)
  real(kind(1.0D0)) :: pfrmax = 0.0D0
  ! !+ad_vars  pfsec : PF Coil waste heat (MW)
  !  real(kind(1.0D0)) :: pfsec = 0.0D0
  !+ad_vars  pfwpmw : Total mean wall plug power dissipated in PFC and CS power supplies.  Issue #713 (MW)
  real(kind(1.0D0)) :: pfwpmw = 0.0D0
  !+ad_vars  powohres : central solenoid resistive power during flattop (W)
  real(kind(1.0D0)) :: powohres = 0.0D0
  !+ad_vars  powpfres : total PF coil resistive losses during flattop (W)
  real(kind(1.0D0)) :: powpfres = 0.0D0
  !+ad_vars  ra(ngc2) : inner radius of coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: ra = 0.0D0
  !+ad_vars  rb(ngc2) : outer radius of coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: rb = 0.0D0
  !+ad_vars  ric(ngc2) : peak current in coil i (MA-turns)
  real(kind(1.0D0)), dimension(ngc2) :: ric = 0.0D0
  !+ad_vars  rjconpf(ngc2) /3.0e7/ : average winding pack current density of PF coil i (A/m2)
  !+ad_varc                          at time of peak current in that coil
  !+ad_varc                          (calculated for ipfloc=1 coils)
  real(kind(1.0D0)), dimension(ngc2) :: rjconpf = 3.0D7
  !+ad_vars  rjohc : allowable central solenoid current density at end of flat-top (A/m2)
  real(kind(1.0D0)) :: rjohc = 0.0D0
  !+ad_vars  rjohc0 : allowable central solenoid current density at beginning of pulse (A/m2)
  real(kind(1.0D0)) :: rjohc0 = 0.0D0
  !+ad_vars  rjpfalw(ngc2) : allowable winding pack current density of PF coil i (A/m2)
  real(kind(1.0D0)), dimension(ngc2) :: rjpfalw = 0.0D0
  !+ad_vars  rohc : radius to the centre of the central solenoid (m)
  real(kind(1.0D0)) :: rohc = 0.0D0
  !+ad_vars  routr /1.5/ : radial distance (m) from outboard TF coil leg to centre of
  !+ad_varc                ipfloc=3 PF coils
  real(kind(1.0D0)) :: routr = 1.5D0
  !+ad_vars  rpf(ngc2) : radius of PF coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: rpf = 0.0D0
  !+ad_vars  rpf1 /0.0/ : offset (m) of radial position of ipfloc=1 PF coils
  !+ad_varc               from being directly above the central solenoid
  real(kind(1.0D0)) :: rpf1 = 0.0D0
  !+ad_vars  rpf2 /-1.63/ : offset (m) of radial position of ipfloc=2 PF coils
  !+ad_varc                 from being at rmajor (offset = rpf2*triang*rminor)
  real(kind(1.0D0)) :: rpf2 = -1.63D0
  !+ad_vars  s_tresca_oh : Tresca stress coils/central solenoid [MPa]
  real(kind(1.0D0)) :: s_tresca_oh = 0.0D0
  !+ad_vars  sigpfcalw /500.0/ : maximum permissible tensile stress (MPa) in
  !+ad_varc                      steel coil cases for superconducting PF coils
  !+ad_varc                      (ipfres=0)
  real(kind(1.0D0)) :: sigpfcalw = 500.0D0
  !+ad_vars  sigpfcf /0.666/ : fraction of JxB hoop force supported by steel case
  !+ad_varc                    for superconducting PF coils (ipfres=0)
  real(kind(1.0D0)) :: sigpfcf = 0.666D0
  !+ad_vars  sxlg(ngc2,ngc2) : mutual inductance matrix (H)
  real(kind(1.0D0)), dimension(ngc2,ngc2) :: sxlg = 0.0D0
  !+ad_vars  tmargoh :  Central solenoid temperature margin (K)
  real(kind(1.0D0)) :: tmargoh = 0.0D0
  !+ad_vars  turns(ngc2) : number of turns in PF coil i
  real(kind(1.0D0)), dimension(ngc2) :: turns = 0.0D0
  !+ad_vars  vf(ngc2) /0.3/ : winding pack void fraction of PF coil i for coolant
  real(kind(1.0D0)), dimension(ngc2) :: vf = 0.3D0
  !+ad_vars  vfohc /0.3/ : void fraction of central solenoid conductor for coolant
  real(kind(1.0D0)) :: vfohc = 0.3D0
  !+ad_vars  vsbn : total flux swing available for burn (Wb)
  real(kind(1.0D0)) :: vsbn = 0.0D0
  !+ad_vars  vsefbn : flux swing from PF coils for burn (Wb)
  real(kind(1.0D0)) :: vsefbn = 0.0D0
  !+ad_vars  vsefsu : flux swing from PF coils for startup (Wb)
  real(kind(1.0D0)) :: vsefsu = 0.0D0
  !+ad_vars  vseft : total flux swing from PF coils (Wb)
  real(kind(1.0D0)) :: vseft = 0.0D0
  !+ad_vars  vsoh : total flux swing from the central solenoid (Wb)
  real(kind(1.0D0)) :: vsoh = 0.0D0
  !+ad_vars  vsohbn : central solenoid flux swing for burn (Wb)
  real(kind(1.0D0)) :: vsohbn = 0.0D0
  !+ad_vars  vsohsu : central solenoid flux swing for startup (Wb)
  real(kind(1.0D0)) :: vsohsu = 0.0D0
  !+ad_vars  vssu : total flux swing for startup (eqn 51 to enforce vssu=vsres+vsind) (Wb)
  real(kind(1.0D0)) :: vssu = 0.0D0
  !+ad_vars  vstot : total flux swing for pulse (Wb)
  real(kind(1.0D0)) :: vstot = 0.0D0
  !+ad_vars  waves(ngc2, 6) : used in current waveform of PF coils/central solenoid
  real(kind(1.0D0)), dimension(ngc2,6) :: waves = 0.0D0
  !+ad_vars  whtpf : total mass of the PF coil conductor (kg)
  real(kind(1.0D0)) :: whtpf = 0.0D0
  !+ad_vars  whtpfs : total mass of the PF coil structure (kg)
  real(kind(1.0D0)) :: whtpfs = 0.0D0
  !+ad_vars  wtc(ngc2) : conductor mass for PF coil i (kg)
  real(kind(1.0D0)), dimension(ngc2) :: wtc = 0.0D0
  !+ad_vars  wts(ngc2) : structure mass for PF coil i (kg)
  real(kind(1.0D0)), dimension(ngc2) :: wts = 0.0D0
  !+ad_vars  zh(ngc2) : upper point of PF coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: zh = 0.0D0
  !+ad_vars  zl(ngc2) : lower point of PF coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: zl = 0.0D0
  !+ad_vars  zpf(ngc2) : z (height) location of PF coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: zpf = 0.0D0
  !+ad_vars  zref(ngrpmx) /../ : PF coil vertical positioning adjuster:<UL>
  !+ad_varc        <LI> - for groups j with ipfloc(j) = 1; zref(j) is ignored
  !+ad_varc        <LI> - for groups j with ipfloc(j) = 2 AND itart=1 (only);
  !+ad_varc               zref(j) is distance of centre of PF coil from inside
  !+ad_varc               edge of TF coil (remember that PF coils for STs lie
  !+ad_varc               within the TF coil)
  !+ad_varc        <LI> - for groups j with ipfloc(j) = 3; zref(j) = ratio of
  !+ad_varc               height of coil group j to plasma minor radius</UL>
  real(kind(1.0D0)), dimension(ngrpmx) :: zref = (/3.6D0, 1.2D0, 2.5D0, &
       1.0D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0/)

  !+ad_vars  bmaxcs_lim : Central solenoid max field limit [T]
  real(kind(1.0D0)) :: bmaxcs_lim = 13.0
  !+ad_vars  fbmaxcs : F-value for CS mmax field (cons. 79, itvar 149)
  real(kind(1.0D0)) :: fbmaxcs = 13.0


end module pfcoil_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module tfcoil_variables

  !+ad_name  tfcoil_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  toroidal field coil systems
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  toroidal field coil systems of a fusion power plant.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_hist  30/01/13 PJK Modified vftf comments
  !+ad_hist  08/04/13 PJK Modified cpttf, tfno comments
  !+ad_hist  15/04/13 PJK Modified tfckw comments
  !+ad_hist  16/04/13 PJK Redefined isumattf; removed jcrit_model;
  !+ad_hisc               changed dcond dimensions
  !+ad_hist  19/06/13 PJK Removed rjtfsual
  !+ad_hist  08/10/13 PJK Reassigned isumattf=2; added fhts
  !+ad_hist  17/10/13 PJK Modified cdtfleg comment
  !+ad_hist  06/11/13 PJK Modified various comments; removed obsolete switch magnt
  !+ad_hist  01/05/14 PJK Changed TF coil stress model limits to recent ITER values;
  !+ad_hisc               added stress_model etc.; corrected arc array lengths
  !+ad_hist  01/05/14 PJK Lowered ripmax default value from 5.0 to 1.0
  !+ad_hist  06/05/14 PJK Removed wpvf
  !+ad_hist  07/05/14 PJK removed rnltf;
  !+ad_hisc               replaced itfmod and stress_model with tfc_model
  !+ad_hist  08/05/14 PJK Changed ripmax description
  !+ad_hist  12/05/14 PJK Added insstrain
  !+ad_hist  24/06/14 PJK Removed wtbc
  !+ad_hist  30/07/14 PJK Renamed borev to tfborev
  !+ad_hist  31/07/14 PJK Added acasetfo, dcondins, whtconin, whtgw, whtrp;
  !+ad_hisc               removed aspcstf
  !+ad_hist  19/08/14 PJK Removed casfact
  !+ad_hist  16/09/14 PJK Added tfcryoarea
  !+ad_hist  16/09/14 PJK Modified array sizes in TF coil stress calculations;
  !+ad_hisc               changed tfc_model switch values
  !+ad_hist  11/06/15 MDK Mods to TF coil defaults
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_hist  26/11/15 RK  Added variables for quench time calculation: taucq, sigvvall
  !+ad_hist  22/06/18 SIM Made cdtfleg an output instead of an input
  !+ad_hist  24/05/19 SIM Removed estotf, previously marked obsolete (#199 #847)
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !+ad_docs  ITER Magnets design description document DDD11-2 v2 2 (2009)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  acasetf : external case area per coil (inboard leg) (m2)
  real(kind(1.0D0)) :: acasetf = 0.0D0
  !+ad_vars  acasetfo : external case area per coil (outboard leg) (m2)
  real(kind(1.0D0)) :: acasetfo = 0.0D0
  !+ad_vars  acndttf : area of the cable conduit (m2)
  real(kind(1.0D0)) :: acndttf = 0.0D0
  !+ad_vars  acond : conductor area (winding pack) (m2)
  real(kind(1.0D0)) :: acond = 0.0D0
  !+ad_vars  acstf : internal area of the cable space (m2)
  real(kind(1.0D0)) :: acstf = 0.0D0
  !+ad_vars  insulation_area : single turn insulation area (m2)
  real(kind(1.0D0)) :: insulation_area = 0.0D0
  !+ad_vars  aiwp : winding pack insulation area (m2)
  real(kind(1.0D0)) :: aiwp = 0.0D0
  !+ad_vars  alstrtf /6.0D8/ : allowable Tresca stress in TF coil structural material (Pa)
  real(kind(1.0D0)) :: alstrtf = 6.0D8

  !+ad_vars  arealeg : outboard TF leg area (m2)
  real(kind(1.0D0)) :: arealeg = 0.0D0
  !+ad_vars  aswp : winding pack structure area (m2)
  real(kind(1.0D0)) :: aswp = 0.0D0
  !+ad_vars  avwp : winding pack void (He coolant) area (m2)
  real(kind(1.0D0)) :: avwp = 0.0D0
  !+ad_vars  awphec : winding pack He coil area (m2)
  real(kind(1.0D0)) :: awphec = 0.0D0
  !+ad_vars  bcritsc /24.0/ : upper critical field (T) for Nb3Sn superconductor
  !+ad_varc                   at zero temperature and strain (isumattf=4, =bc20m)
  real(kind(1.0D0)) :: bcritsc = 24.0D0
  !+ad_vars  bmaxtf : mean peak field at TF coil (T)
  real(kind(1.0D0)) :: bmaxtf = 0.0D0
  !+ad_vars  bmaxtfrp : peak field at TF conductor with ripple (T)
  real(kind(1.0D0)) :: bmaxtfrp = 0.0D0
  !+ad_vars  casestr : case strain
  real(kind(1.0D0)) :: casestr = 0.0D0

  !+ad_vars  casthi /0.0/ : EITHER: inboard TF coil case plasma side thickness (m)
  !+ad_varc                  (calculated for stellarators)
  real(kind(1.0D0)) :: casthi = 0.0D0
  !+ad_vars  casthi_fraction /0.05/ : OR: inboard TF coil case plasma side thickness as a fraction of tfcth
  real(kind(1.0D0)) :: casthi_fraction = 0.05D0
  logical :: casthi_is_fraction

  !+ad_vars  casths /0.0/ : EITHER: inboard TF coil sidewall case thickness (m)
  !+ad_varc                  (calculated for stellarators)
  real(kind(1.0D0)) :: casths = 0.0D0
  !+ad_vars  casths_fraction /0.03/ : OR: inboard TF coil sidewall case thickness as a fraction of tftort
  real(kind(1.0D0)) :: casths_fraction = 0.03D0
  logical :: tfc_sidewall_is_fraction

  !+ad_vars  conductor_width : Width of square conductor (m)
  real(kind(1.0D0)) :: conductor_width
  !+ad_vars  leno : Dimension of each turn including inter-turn insulation (m)
  real(kind(1.0D0)) :: leno


  !+ad_vars  leni : Dimension of space inside conductor (m)
  real(kind(1.0D0)) :: leni
  !+ad_vars  acs : Area of space inside conductor (m2)
  real(kind(1.0D0)) :: acs

  !+ad_vars  cdtfleg : TF outboard leg current density (A/m2) (resistive coils only)
  real(kind(1.0D0)) :: cdtfleg = 0.0D0
  !+ad_vars  cforce : centering force on inboard leg (per coil) (N/m)
  real(kind(1.0D0)) :: cforce = 0.0D0
  !+ad_vars  cph2o /4180.0/ FIX : specific heat capacity of water (J/kg/K)
  real(kind(1.0D0)) :: cph2o = 4180.0D0

  !+ad_vars  cpttf /7.0e4/ : TF coil current per turn (A).
  !+ad_varc                  (calculated for stellarators)
  !+ad_varc                  (calculated for integer-turn TF coils i_tf_turns_integer=1)
  !+ad_varc                  (iteration variable 60)
  real(kind(1.0D0)) :: cpttf = 7.0e4

  !+ad_vars  cpttf_max /9.0e4/ : Max TF coil current per turn [A].
  !+ad_varc                  (For stellarators and i_tf_turns_integer=1)
  !+ad_varc                  (constraint equation 77)
  real(kind(1.0D0)) :: cpttf_max = 9.0e4

  !+ad_vars  dcase /8000.0/ : density of coil case (kg/m3)
  real(kind(1.0D0)) :: dcase = 8000.0D0
  !+ad_vars  dcond(6) /9000.0/ : density of superconductor type given by isumattf/isumatoh/isumatpf (kg/m3)
  real(kind(1.0D0)), dimension(6) :: dcond = 9000.0D0
  !+ad_vars  dcondins /1800.0/ : density of conduit + ground-wall insulation (kg/m3)
  real(kind(1.0D0)) :: dcondins = 1800.0D0

  !+ad_vars  dcopper /8900.0/ : density of copper (kg/m3)
  real(kind(1.0D0)) :: dcopper = 8900.0D0

  !+ad_vars  dalu /2700.0/ : density of aluminium (kg/m3)
  real(kind(1.0D0)) :: dalu = 2700.0D0
  !+ad_vars  deflect : TF coil deflection at full field (m)
  real(kind(1.0D0)) :: deflect = 0.0D0
  !+ad_vars  denh2o /985.0/ FIX : density of water (kg/m3)
  real(kind(1.0D0)) :: denh2o = 985.0D0
  !+ad_vars  dhecoil /0.005/ : diameter of He coil in TF winding (m)
  real(kind(1.0D0)) :: dhecoil = 0.005D0

  !+ad_vars  estotftgj : total stored energy in the toroidal field (GJ)
  real(kind(1.0D0)) :: estotftgj = 0.0D0

  !+ad_vars  eyins /2.0e10/ : insulator Young's modulus (Pa)
  !+ad_varc                   (default value from DDD11-2 v2 2 (2009))
  real(kind(1.0D0)) :: eyins = 2.0D10
  !+ad_vars  eyoung(2) : work array used in stress calculation (Pa)
  real(kind(1.0D0)), dimension(2) :: eyoung = 0.0D0
  !+ad_vars  eystl /2.05e11/ : steel case Young's modulus (Pa)
  !+ad_varc                    (default value from DDD11-2 v2 2 (2009))
  real(kind(1.0D0)) :: eystl = 2.05D11
  !+ad_vars  eywp /6.6e8/ : winding pack Young's modulus (Pa)
  real(kind(1.0D0)) :: eywp = 6.6D8
  !+ad_vars  eyzwp : winding pack vertical Young's modulus (Pa)
  real(kind(1.0D0)) :: eyzwp = 0.0D0
  !+ad_vars  farc4tf /0.7/ : factor to size height of point 4 on TF coil
  real(kind(1.0D0)) :: farc4tf = 0.7D0
  !+ad_vars  fcutfsu /0.69/ : copper fraction of cable conductor (TF coils)
  !+ad_varc                   (iteration variable 59)
  real(kind(1.0D0)) :: fcutfsu = 0.69D0
  !+ad_vars  fhts /0.5/ : technology adjustment factor for critical current density fit
  !+ad_varc               for isumat..=2 Bi-2212 superconductor, to describe the level
  !+ad_varc               of technology assumed (i.e. to account for stress, fatigue,
  !+ad_varc               radiation, AC losses, joints or manufacturing variations;
  !+ad_varc               1.0 would be very optimistic)
  real(kind(1.0D0)) :: fhts = 0.5D0
  !+ad_vars  insstrain : radial strain in insulator
  real(kind(1.0D0)) :: insstrain = 0.0D0
  !+ad_vars  i_tf_tresca /0/ : switch for TF coil conduit Tresca stress criterion:<UL>
  !+ad_varc          <LI> = 0 Tresca (no adjustment);
  !+ad_varc          <LI> = 1 Tresca with CEA adjustment factors (radial+2%, vertical+60%) </UL>
  integer :: i_tf_tresca = 0
  !+ad_vars  i_tf_turns_integer /0/ : switch for TF coil integer/non-integer turns<UL>
  !+ad_varc          <LI> = 0 non-integer turns;
  !+ad_varc          <LI> = 1 integer turns</UL>
  integer :: i_tf_turns_integer = 0
  !+ad_vars  isumattf /1/ : switch for superconductor material in TF coils:<UL>
  !+ad_varc            <LI> = 1 ITER Nb3Sn critical surface model with standard
  !+ad_varc                     ITER parameters;
  !+ad_varc            <LI> = 2 Bi-2212 high temperature superconductor (range of
  !+ad_varc                     validity T < 20K, adjusted field b < 104 T, B > 6 T);
  !+ad_varc            <LI> = 3 NbTi;
  !+ad_varc            <LI> = 4 ITER Nb3Sn model with user-specified parameters
  !+ad_varc            <LI> = 5 WST Nb3Sn parameterisation
  !+ad_varc            <LI> = 6 REBCO HTS tape in CroCo strand</UL>
  integer :: isumattf = 1
  !+ad_vars  itfsup /1/ : switch for TF coil conductor model:<UL>
  !+ad_varc          <LI> = 0 copper;
  !+ad_varc          <LI> = 1 superconductor</UL>
  integer :: itfsup = 1
  !+ad_vars  jbus /1.25e6/ : bussing current density (A/m2)
  real(kind(1.0D0)) :: jbus = 1.25D6
  !+ad_vars  jeff(2) : work array used in stress calculation (A/m2)
  real(kind(1.0D0)), dimension(2) :: jeff = 0.0D0
  !+ad_vars  jwdgcrt : critical current density for winding pack (A/m2)
  real(kind(1.0D0)) :: jwdgcrt = 0.0D0
  !+ad_vars  jwdgpro : allowable TF coil winding pack current density,
  !+ad_varc            for dump temperature rise protection (A/m2)
  real(kind(1.0D0)) :: jwdgpro = 0.0D0
  !+ad_vars  jwptf : winding pack current density (A/m2)
  real(kind(1.0D0)) :: jwptf = 0.0D0

  !+ad_vars  n_pancake /10/ : Number of pancakes in TF coil (i_tf_turns_integer=1)
  integer :: n_pancake = 10

  !+ad_vars  n_layer /20/ : Number of layers in TF coil (i_tf_turns_integer=1)
  integer :: n_layer = 20

  !+ad_vars  oacdcp /1.4e7/ : overall current density in TF coil inboard legs (A/m2)
  !+ad_varc                   (iteration variable 12)
  real(kind(1.0D0)) :: oacdcp = 1.4D7
  !+ad_vars  poisson /0.3/ : Poisson's ratio for TF stress calculation
  !+ad_varc                  (assumed constant over entire coil)
  real(kind(1.0D0)) :: poisson = 0.3D0
  !+ad_vars  radtf(3) : work array used in stress calculation (m)
  real(kind(1.0D0)), dimension(3) :: radtf = 0.0D0
  !+ad_vars  rbmax : radius of maximum TF B-field (m)
  real(kind(1.0D0)) :: rbmax = 0.0D0
  !+ad_vars  rhotfleg : TF coil leg resistance (ohm)
  real(kind(1.0D0)) :: rhotfleg = 0.0D0
  !+ad_vars  ripmax /1.0/ : maximum allowable toroidal field ripple amplitude
  !+ad_varc                 at plasma edge (%)
  real(kind(1.0D0)) :: ripmax = 1.0D0
  !+ad_vars  ripple : peak/average toroidal field ripple at plasma edge (%)
  real(kind(1.0D0)) :: ripple = 0.0D0
  !+ad_vars  ritfc : total (summed) current in TF coils (A)
  real(kind(1.0D0)) :: ritfc = 0.0D0
  !+ad_vars  sigrad : radial TF coil stress (MPa)
  real(kind(1.0D0)) :: sigrad = 0.0D0
  !+ad_vars  sigrcon : radial stress in the conductor conduit (Pa)
  real(kind(1.0D0)) :: sigrcon = 0.0D0
  !+ad_vars  sigrtf(2) : radial stress in TF coil regions (Pa)
  real(kind(1.0D0)), dimension(2) :: sigrtf = 0.0D0
  !+ad_vars  sigtan : transverse TF coil stress (MPa)
  real(kind(1.0D0)) :: sigtan = 0.0D0
  !+ad_vars  sigtcon : tangential stress in the conductor conduit (Pa)
  real(kind(1.0D0)) :: sigtcon = 0.0D0
  !+ad_vars  sigttf(2) : tangential stress in TF coil regions (Pa)
  real(kind(1.0D0)), dimension(2) :: sigttf = 0.0D0
  !+ad_vars  s_tresca_case : TF coil case Tresca stress (MPa)
  real(kind(1.0D0)) :: s_tresca_case  = 0.0D0
  !+ad_vars  s_tresca_cond : TF coil conduit Tresca stress (MPa)
  real(kind(1.0D0)) :: s_tresca_cond  = 0.0D0
  !+ad_vars  s_vmises_case : TF coil case von Mises stress (MPa)
  real(kind(1.0D0)) :: s_vmises_case  = 0.0D0
  !+ad_vars  s_vmises_cond : TF coil conduit von Mises stress (MPa)
  real(kind(1.0D0)) :: s_vmises_cond  = 0.0D0
  !+ad_vars  sigver : vertical TF coil stress (MPa)
  real(kind(1.0D0)) :: sigver  = 0.0D0
  !+ad_vars  sigvert : vertical tensile stress in TF coil (Pa)
  real(kind(1.0D0)) :: sigvert = 0.0D0
  !+ad_vars  sigvvall /9.3e7/ : allowable stress from TF quench in vacuum vessel (Pa)
  real(kind(1.0D0)) :: sigvvall = 9.3D7
  !+ad_vars  strncon_cs /-0.005/ : strain in CS superconductor material
  !+ad_varc                     (used in Nb3Sn critical surface model, isumatoh=1, 4 or 5)
  real(kind(1.0D0)) :: strncon_cs = -0.005D0
  !+ad_vars  strncon_pf /-0.005/ : strain in PF superconductor material
  !+ad_varc                     (used in Nb3Sn critical surface model, isumatph=1, 4 or 5)
  real(kind(1.0D0)) :: strncon_pf = -0.005D0
  !+ad_vars  strncon_tf /-0.005/ : strain in TF superconductor material
  !+ad_varc                     (used in Nb3Sn critical surface model, isumattf=1, 4 or 5)
  real(kind(1.0D0)) :: strncon_tf = -0.005D0
  !+ad_vars  strtf1 : Constrained stress in TF conductor conduit (Pa)
  real(kind(1.0D0)) :: strtf1 = 0.0D0
  !+ad_vars  strtf2 : Constrained stress in TF coil case (Pa)
  real(kind(1.0D0)) :: strtf2 = 0.0D0

  ! Issue #522: Quench models
  !+ad_vars  quench_model /exponential/ : switch for TF coil quench model:<UL>
  !+ad_varc                  <LI> = 'exponential' exponential quench with constant discharge resistor
  !+ad_varc                  <LI> = 'linear' quench with constant voltage</UL>
  !+ad_varc                    Only applies to REBCO magnet at present
  character(len=12) :: quench_model = 'exponential'

  !+ad_vars  quench_detection_ef /0.0/ : Electric field at which TF quench is detected and discharge begins (V/m)
  real(kind(1.0D0)) :: quench_detection_ef = 0D0
  !+ad_vars  time1 : Time at which TF quench is detected (s)
  real(kind(1.0D0)) :: time1 = 0D0

  !+ad_vars  taucq : allowable TF quench time (s)
  real(kind(1.0D0)) :: taucq = 30.0D0
  !+ad_vars  tcritsc /16.0/ : critical temperature (K) for superconductor
  !+ad_varc                   at zero field and strain (isumattf=4, =tc0m)
  real(kind(1.0D0)) :: tcritsc = 16.0D0
  !+ad_vars  tdmptf /10.0/ : fast discharge time for TF coil in event of quench (s)
  !+ad_varc                  (iteration variable 56)
  !+ad_varc                  For REBCO model, meaning depends on quench_model:
  !+ad_varc                  <LI> exponential quench : e-folding time (s)
  !+ad_varc                  <LI> linear quench : discharge time (s)
  real(kind(1.0D0)) :: tdmptf = 10.0D0
  !+ad_vars  tfareain : area of inboard midplane TF legs (m2)
  real(kind(1.0D0)) :: tfareain = 0.0D0

  !+ad_vars  tfboreh : TF coil horizontal inner bore (m)
  real(kind(1.0D0)) :: tfboreh = 0.0D0

  !+ad_vars  tf_total_h_width : TF coil horizontal inner bore (m)
  real(kind(1.0D0)) :: tf_total_h_width = 0.0D0

  !+ad_vars  tfborev : TF coil vertical inner bore (m)
  real(kind(1.0D0)) :: tfborev = 0.0D0
  !+ad_vars  tfbusl : TF coil bus length (m)
  real(kind(1.0D0)) :: tfbusl = 0.0D0
  !+ad_vars  tfbusmas : TF coil bus mass (kg)
  real(kind(1.0D0)) :: tfbusmas = 0.0D0
  !+ad_vars  tfckw :  available DC power for charging the TF coils (kW)
  real(kind(1.0D0)) :: tfckw = 0.0D0
! Issue #781
!   !+ad_vars  tfc_model /1/ : switch for TF coil magnet stress model:<UL>
!   !+ad_varc                  <LI> = 0 simple model (solid copper coil)
!   !+ad_varc                  <LI> = 1 CCFE two-layer stress model; superconductor</UL>
!   integer :: tfc_model = 1
  !+ad_vars  tfcmw : peak power per TF power supply (MW)
  real(kind(1.0D0)), bind(C) :: tfcmw = 0.0D0
  !+ad_vars  tfcpmw : peak resistive TF coil inboard leg power (MW)
  real(kind(1.0D0)) :: tfcpmw = 0.0D0
  !+ad_vars  tfcryoarea : surface area of toroidal shells covering TF coils (m2)
  real(kind(1.0D0)) :: tfcryoarea = 0.0D0
  !+ad_vars  tficrn : TF coil half-width - inner bore (m)
  real(kind(1.0D0)) :: tficrn = 0.0D0
  !+ad_vars  tfind : TF coil inductance (H)
  real(kind(1.0D0)) :: tfind = 0.0D0
  !+ad_vars  tfinsgap /0.010/ : TF coil WP insertion gap (m)
  real(kind(1.0D0)) :: tfinsgap = 0.010D0
  !+ad_vars  tflegmw : TF coil outboard leg resistive power (MW)
  real(kind(1.0D0)) :: tflegmw = 0.0D0
  !+ad_vars  tflegres /2.5e-8/ : resistivity of a TF coil leg and bus(Ohm-m)
  real(kind(1.0D0)) :: tflegres = 2.5D-8
  !+ad_vars  tfleng : TF coil circumference (m)
  real(kind(1.0D0)) :: tfleng = 0.0D0
  !+ad_vars  tfno /16.0/ : number of TF coils (default = 50 for stellarators)
  !+ad_varc                number of TF coils outer legs for ST
  real(kind(1.0D0)) :: tfno = 16.0D0
  !+ad_vars  tfocrn : TF coil half-width - outer bore (m)
  real(kind(1.0D0)) :: tfocrn = 0.0D0
  !+ad_vars  tfsai : area of the inboard TF coil legs (m2)
  real(kind(1.0D0)) :: tfsai = 0.0D0
  !+ad_vars  tfsao : area of the outboard TF coil legs (m2)
  real(kind(1.0D0)) :: tfsao = 0.0D0
  !+ad_vars  tftmp /4.5/ : peak helium coolant temperature in TF coils and PF coils (K)
  real(kind(1.0D0)) :: tftmp = 4.5D0
  ! ISSUE #508 Remove RFP option: frfpf, frfptf, sccufac
  !+ad_vars  tftort : TF coil toroidal thickness (m)
! SJP Issue #863
! tftort physically to large, reduced to 1.0
  real(kind(1.0D0)) :: tftort = 1.0D0

  !+ad_vars  thicndut /8.0e-4/ : conduit insulation thickness (m)
  real(kind(1.0D0)) :: thicndut = 8.0D-4
  !+ad_vars  layer_ins /0/ : Additional insulation thickness between layers (m)
  real(kind(1.0D0)) :: layer_ins = 0.0D0
  !+ad_vars  thkcas /0.3/ : inboard TF coil case outer (non-plasma side) thickness (m)
  !+ad_varc                 (iteration variable 57)
  !+ad_varc                 (calculated for stellarators)
  real(kind(1.0D0)) :: thkcas = 0.3D0
  ! Issue #514 Make thkwp an iteration variable
  !+ad_vars  thkwp /0.0/ : radial thickness of winding pack (m) (iteration variable 140)
  real(kind(1.0D0)) :: thkwp = 0.0D0
  !+ad_vars  thwcndut /8.0e-3/ : TF coil conduit case thickness (m) (iteration variable 58)
  real(kind(1.0D0)) :: thwcndut = 8.0D-3
  !+ad_vars  tinstf /0.018/ : ground insulation thickness surrounding winding pack (m)
  !+ad_vars                   Includes allowance for 10 mm insertion gap.
  !+ad_varc                   (calculated for stellarators)
  real(kind(1.0D0)) :: tinstf = 0.018D0

  !+ad_vars  tmargmin_tf /0/ : minimum allowable temperature margin : TF coils (K)
  real(kind(1.0D0)), bind(C) :: tmargmin_tf = 0D0
  !+ad_vars  tmargmin_cs /0/ : minimum allowable temperature margin : CS (K)
  real(kind(1.0D0)), bind(C) :: tmargmin_cs = 0D0
  !+ad_vars  tmargmin /0/ : minimum allowable temperature margin : TFC AND CS (K)
  real(kind(1.0D0)) :: tmargmin = 0D0

  !+ad_vars  temp_margin  : temperature margin (K)
  real(kind(1.0D0)), bind(C) :: temp_margin = 0.00D0
  !+ad_vars  tmargtf :  TF coil temperature margin (K)
  real(kind(1.0D0)) :: tmargtf = 0.0D0
  !+ad_vars  tmaxpro /150.0/ : maximum temp rise during a quench for protection (K)
  real(kind(1.0D0)) :: tmaxpro = 150.0D0

  !+ad_vars  tmax_croco /200.0/ : CroCo strand: maximum permitted temp during a quench (K)
  real(kind(1.0D0)) :: tmax_croco = 200.0D0
  ! !+ad_vars  tmax_jacket /150.0/ : Jacket: maximum temp during a quench (K)
  ! real(kind(1.0D0)) :: tmax_jacket = 150.0D0

  !+ad_vars  croco_quench_temperature : CroCo strand: Actual temp reached during a quench (K)
  real(kind(1.0D0)) :: croco_quench_temperature = 0D0

  !+ad_vars  tmpcry /4.5/ : coil temperature for cryogenic plant power calculation (K)
  real(kind(1.0D0)) :: tmpcry = 4.5D0
  !+ad_vars  turnstf : number of turns per TF coil
  real(kind(1.0D0)) :: turnstf = 0.0D0
  !+ad_vars  vdalw /20.0/ : max voltage across TF coil during quench (kV)
  !+ad_varc                 (iteration variable 52)
  real(kind(1.0D0)) :: vdalw = 20.0D0
  !+ad_vars  vforce : vertical separating force on inboard leg/coil (N)
  real(kind(1.0D0)) :: vforce = 0.0D0
  !+ad_vars  vftf /0.4/ : coolant fraction of TFC 'cable' (itfsup=1), or of TFC leg (itfsup=0)
  real(kind(1.0D0)) :: vftf = 0.4D0
  !+ad_vars  voltfleg : volume of each TF coil outboard leg (m3)
  real(kind(1.0D0)) :: voltfleg = 0.0D0
  !+ad_vars  vtfkv : TF coil voltage for resistive coil including bus (kV)
  real(kind(1.0D0)) :: vtfkv = 0.0D0
  !+ad_vars  vtfskv : voltage across a TF coil during quench (kV)
  real(kind(1.0D0)) :: vtfskv = 0.0D0
  !+ad_vars  whtcas : mass per coil of external case (kg)
  real(kind(1.0D0)) :: whtcas = 0.0D0
  !+ad_vars  whtcon : TF coil conductor mass per coil (kg)
  real(kind(1.0D0)) :: whtcon = 0.0D0
  !+ad_vars  whtconcu : copper mass in TF coil conductor (kg/coil)
  real(kind(1.0D0)) :: whtconcu = 0.0D0
  !+ad_vars  whtconin : conduit insulation mass in TF coil conductor (kg/coil)
  real(kind(1.0D0)) :: whtconin = 0.0D0
  !+ad_vars  whtconsc : superconductor mass in TF coil cable (kg/coil)
  real(kind(1.0D0)) :: whtconsc = 0.0D0
  !+ad_vars  whtconsh : steel conduit mass in TF coil conductor (kg/coil)
  real(kind(1.0D0)) :: whtconsh = 0.0D0
  !+ad_vars  whtgw : mass of ground-wall insulation layer per coil (kg/coil)
  real(kind(1.0D0)) :: whtgw = 0.0D0
  !+ad_vars  whttf : total mass of the TF coils (kg)
  real(kind(1.0D0)) :: whttf = 0.0D0
  !+ad_vars  windstrain : longitudinal strain in winding pack
  real(kind(1.0D0)) :: windstrain = 0.0D0
  !+ad_vars  wwp1 : width of first step of winding pack (m)
  real(kind(1.0D0)) :: wwp1 = 0.0D0
  !+ad_vars  wwp2 : width of second step of winding pack (m)
  real(kind(1.0D0)) :: wwp2 = 0.0D0

  !+ad_vars  <P><B>Superconducting TF coil shape parameters</B> (see also farc4tf);
  !+ad_varc  <BR>the TF inner surface top half is approximated by four circular arcs.
  !+ad_varc  Arc 1 goes through points 1 and 2 on the inner surface. Arc 2
  !+ad_varc  goes through points 2 and 3, etc.<P>

  !+ad_vars  dthet(4) : angle of arc i (rad)
  real(kind(1.0D0)), dimension(4) :: dthet = 0.0D0
  !+ad_vars  radctf(4) : radius of arc i (m)
  real(kind(1.0D0)), dimension(4) :: radctf = 0.0D0
  !+ad_vars  xarc(5) : x location of arc point i on surface (m)
  real(kind(1.0D0)), dimension(5) :: xarc = 0.0D0
  !+ad_vars  xctfc(4) : x location of arc centre i (m)
  real(kind(1.0D0)), dimension(4) :: xctfc = 0.0D0
  !+ad_vars  yarc(5) : y location of arc point i on surface (m)
  real(kind(1.0D0)), dimension(5) :: yarc = 0.0D0
  !+ad_vars  yctfc(4) : y location of arc centre i (m)
  real(kind(1.0D0)), dimension(4) :: yctfc = 0.0D0

  ! New TF shape:  Horizontal and vertical radii of inside edge of TF coil
  ! Arcs are numbered clockwise:
  ! 1=upper inboard, 2=upper outboard, 3=lower ouboard, 4=lower inboard
  !+ad_vars  tfa(4) : Horizontal radius of inside edge of TF coil (m)
  real(kind(1.0D0)), dimension(4) :: tfa = 0.0D0
  !+ad_vars  tfb(4) : Vertical radius of inside edge of TF coil (m)
  real(kind(1.0D0)), dimension(4) :: tfb = 0.0D0

  !+ad_vars  <P><B>Quantities relating to the spherical tokamak model (itart=1)</B>
  !+ad_varc        (and in some cases, also to resistive TF coils, itfsup=0):<P>

  !+ad_vars  drtop /0.0/ : centrepost taper maximum radius adjustment (m)
  real(kind(1.0D0)) :: drtop = 0.0D0
  !+ad_vars  dztop /0.0/ : centrepost taper height adjustment (m)
  real(kind(1.0D0)) :: dztop = 0.0D0
  !+ad_vars  etapump /0.8/ : centrepost coolant pump efficiency
  real(kind(1.0D0)) :: etapump = 0.8D0
  !+ad_vars  fcoolcp /0.3/ : coolant fraction of TF coil inboard legs
  !+ad_varc                  (iteration variable 23)
  real(kind(1.0D0)) :: fcoolcp = 0.3D0
  !+ad_vars  frhocp /1.0/ : centrepost resistivity enhancement factor
  real(kind(1.0D0)) :: frhocp = 1.0D0
  !+ad_vars  k_copper /330.0/ FIX : Copper thermal conductivity (W/m/K)
  real(kind(1.0D0)) :: k_copper = 330.0D0
  !+ad_vars  kh2o /0.651/ FIX : thermal conductivity of water (W/m/K)
  real(kind(1.0D0)) :: kh2o = 0.651D0
  !+ad_vars  muh2o /4.71e-4/ FIX : water dynamic viscosity (kg/m/s)
  real(kind(1.0D0)) :: muh2o = 4.71D-4
  !+ad_vars  ncool : number of centrepost coolant tubes
  real(kind(1.0D0)) :: ncool = 0.0D0
  !+ad_vars  ppump : centrepost coolant pump power (W)
  real(kind(1.0D0)) :: ppump = 0.0D0
  !+ad_vars  prescp : resistive power in the centrepost (W)
  real(kind(1.0D0)) :: prescp = 0.0D0
  !+ad_vars  ptempalw /200.0/ : maximum peak centrepost temperature (C)
  !+ad_varc                     (constraint equation 44)
  real(kind(1.0D0)) :: ptempalw = 200.0D0
  !+ad_vars  rcool /0.005/ : average radius of coolant channel (m)
  !+ad_varc                  (iteration variable 69)
  real(kind(1.0D0)) :: rcool = 0.005D0
  !+ad_vars  rhocp : TF coil inboard leg resistivity (Ohm-m)
  real(kind(1.0D0)) :: rhocp = 0.0D0
  !+ad_vars  tcoolin /40.0/ : centrepost coolant inlet temperature (C)
  real(kind(1.0D0)) :: tcoolin = 40.0D0
  !+ad_vars  tcpav /100.0/ : average temp of TF coil inboard leg conductor (C)
  !+ad_varc                  (resistive coils) (iteration variable 20)
  real(kind(1.0D0)) :: tcpav = 100.0D0
  !+ad_vars  tcpav2 : centrepost average temperature (C) (for consistency)
  real(kind(1.0D0)) :: tcpav2 = 0.0D0
  !+ad_vars  tcpmax : peak centrepost temperature (C)
  real(kind(1.0D0)) :: tcpmax = 0.0D0
  !+ad_vars  vcool /20.0/ : max centrepost coolant flow speed at midplane (m/s)
  !+ad_varc                 (iteration variable 70)
  real(kind(1.0D0)) :: vcool = 20.0D0
  !+ad_vars  volcp : total volume of TF coil inboard legs (m3)
  real(kind(1.0D0)) :: volcp = 0.0D0
  !+ad_vars  whtcp : mass of TF coil inboard legs (kg)
  real(kind(1.0D0)) :: whtcp = 0.0D0
  !+ad_vars  whttflgs : mass of the TF coil legs (kg)
  real(kind(1.0D0)) :: whttflgs = 0.0D0

end module tfcoil_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module structure_variables

  !+ad_name  structure_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  support structure
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  support structure of a fusion power plant.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  29/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  aintmass : intercoil structure mass (kg)
  real(kind(1.0D0)) :: aintmass = 0.0D0
  !+ad_vars  clgsmass : gravity support structure for TF coil, PF coil
  !+ad_varc             and intercoil support systems (kg)
  real(kind(1.0D0)) :: clgsmass = 0.0D0
  !+ad_vars  coldmass : total mass of components at cryogenic temperatures (kg)
  real(kind(1.0D0)) :: coldmass = 0.0D0
  !+ad_vars  fncmass : PF coil outer support fence mass (kg)
  real(kind(1.0D0)) :: fncmass = 0.0D0
  !+ad_vars  gsmass : reactor core gravity support mass (kg)
  real(kind(1.0D0)) :: gsmass = 0.0D0

end module structure_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module vacuum_variables

  !+ad_name  vacuum_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  vacuum system
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  vacuum system of a fusion power plant.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  29/10/12 PJK Initial version of module
  !+ad_hist  12/08/15 MDK vacuum_model and associated variables (#304 section 1)
  !+ad_hist  22/09/15 MDK Battes, Day and Rohde pump-down model (#304 section 2)
  !+ad_hist  20/01/16 JM  Added pump throughput defaulted to ITER value
  !+ad_hist  02/02/17 JM  Changed vpumpn to integer
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public
  !+ad_vars  vacuum_model /old/ : switch for vacuum pumping model:<UL>
  !+ad_varc         <LI> = 'old' for old detailed ETR model;
  !+ad_varc         <LI> = 'simple' for simple steady-state model with comparison to ITER cryopumps</UL>
  character(len=6) :: vacuum_model = 'old'
  !+ad_vars  niterpump : number of high vacuum pumps (real number), each with the throughput
  !+ad_varc              of one ITER cryopump (50 Pa m3 s-1), all operating at the same time
  !+ad_varc              (vacuum_model = 'simple')
  real(kind(1.0D0)) :: niterpump = 0.0D0
  !+ad_vars  ntype /1/ : switch for vacuum pump type:<UL>
  !+ad_varc         <LI> = 0 for turbomolecular pump (magnetic bearing)
  !+ad_varc                  with speed of 2.0 m3/s
  !+ad_varc                  (1.95 for N2, 1.8 for He, 1.8 for DT);
  !+ad_varc         <LI> = 1 for compound cryopump with nominal speed of 10.0 m3/s
  !+ad_varc                  (9.0 for N2, 5.0 for He and 25.0 for DT)</UL>
  integer :: ntype = 1
  !+ad_vars  nvduct : number of ducts (torus to pumps)
  integer :: nvduct = 0
  !+ad_vars  dlscal : vacuum system duct length scaling
  real(kind(1.0D0)) :: dlscal = 0.0D0
  !+ad_vars  pbase /5.0e-4/ : base pressure during dwell before gas pre-fill(Pa)
  real(kind(1.0D0)) :: pbase = 5.0D-4
  !+ad_vars  prdiv /0.36/ : divertor chamber pressure during burn (Pa)
  real(kind(1.0D0)) :: prdiv = 0.36D0
  !+ad_vars  pumptp /1.2155D22/ : Pump throughput (molecules/s) (default is ITER value)
  real(kind(1.0D0)) :: pumptp = 1.2155D22
  !+ad_vars  rat /1.3e-8/ : plasma chamber wall outgassing rate (Pa-m/s)
  real(kind(1.0D0)) :: rat = 1.3D-8
  !+ad_vars  tn /300.0/ : neutral gas temperature in chamber (K)
  real(kind(1.0D0)) :: tn = 300.0D0
  !+ad_vars  vacdshm : mass of vacuum duct shield (kg)
  real(kind(1.0D0)) :: vacdshm = 0.0D0
  !+ad_vars  vcdimax : diameter of duct passage (m)
  real(kind(1.0D0)) :: vcdimax = 0.0D0
  !+ad_vars  vpumpn : number of high vacuum pumps
  integer :: vpumpn = 0
  !+ad_vars  dwell_pump /0/ : switch for dwell pumping options:<UL>
  !+ad_varc              <LI> = 0 pumping only during tdwell;
  !+ad_varc              <LI> = 1 pumping only during tramp
  !+ad_varc              <LI> = 2 pumping during tdwell + tramp</UL>
  integer :: dwell_pump = 0

  !+ad_vars  <P><B>The following are used in the Battes, Day and Rohde pump-down model
  !+ad_varc  See "Basic considerations on the pump-down time in the dwell phase of a pulsed fusion DEMO"
  !+ad_varc  http://dx.doi.org/10.1016/j.fusengdes.2015.07.011)
  !+ad_varc  (vacuum_model=simple'):</B><P>
  !+ad_vars  pumpareafraction /0.0203/ : area of one pumping port as a fraction of plasma surface area
  real(kind(1.0D0)) :: pumpareafraction = 0.0203D0
  !+ad_vars  pumpspeedmax /27.3/ : maximum pumping speed per unit area for deuterium & tritium, molecular flow
  real(kind(1.0D0)) :: pumpspeedmax = 27.3D0
  !+ad_vars  pumpspeedfactor /0.167/ : effective pumping speed reduction factor due to duct impedance
  real(kind(1.0D0)) :: pumpspeedfactor = 0.167D0
  !+ad_vars  initialpressure /1.0/ : initial neutral pressure at the beginning of the dwell phase (Pa)
  real(kind(1.0D0)) :: initialpressure = 1.0D0
  !+ad_vars  pbase /5.0e-4/ : base pressure during dwell before gas pre-fill (Pa)
  ! (duplicate message)
  !+ad_vars  outgasindex /1.0/ : outgassing decay index
  real(kind(1.0D0)) :: outgasindex = 1.0D0
  !+ad_vars  outgasfactor /0.0235/ : outgassing prefactor kw: outgassing rate at 1 s per unit area (Pa m s-1)
  real(kind(1.0D0)) :: outgasfactor = 0.0235D0

end module vacuum_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pf_power_variables

  !+ad_name  pf_power_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  PF coil power conversion system
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  PF coil power conversion system of a fusion power plant.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  29/10/12 PJK Initial version of module
  !+ad_hist  27/03/13 PJK Comment change to ISCENR
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  acptmax : average of currents in PF circuits (kA)
  real(kind(1.0D0)) :: acptmax = 0.0D0
  !+ad_vars  ensxpfm : maximum stored energy in the PF circuits (MJ)
  real(kind(1.0D0)) :: ensxpfm = 0.0D0
  !+ad_vars  iscenr /2/ : Switch for PF coil energy storage option:<UL>
  !+ad_varc          <LI> = 1 all power from MGF (motor-generator flywheel) units;
  !+ad_varc          <LI> = 2 all pulsed power from line;
  !+ad_varc          <LI> = 3 PF power from MGF, heating from line</UL>
  !+ad_varc          (In fact, options 1 and 3 are not treated differently)
  integer :: iscenr = 2
  !+ad_vars  pfckts : number of PF coil circuits
  real(kind(1.0D0)) :: pfckts = 0.0D0
  !+ad_vars  spfbusl : total PF coil circuit bus length (m)
  real(kind(1.0D0)) :: spfbusl = 0.0D0
  !+ad_vars  spsmva : sum of PF power supply ratings (MVA)
  real(kind(1.0D0)) :: spsmva = 0.0D0
  !+ad_vars  srcktpm : sum of resistive PF coil power (kW)
  real(kind(1.0D0)) :: srcktpm = 0.0D0
  !+ad_vars  vpfskv : PF coil voltage (kV)
  real(kind(1.0D0)) :: vpfskv = 0.0D0

  !+ad_vars  peakpoloidalpower : Peak absolute rate of change of stored energy in poloidal field (MW) (11/01/16)
  real(kind(1.0D0)) :: peakpoloidalpower = 0.0D0
  !+ad_vars  maxpoloidalpower /1000/ : Maximum permitted absolute rate of change of stored energy in poloidal field (MW)
  real(kind(1.0D0)) :: maxpoloidalpower = 1000.0D0
  !+ad_vars  poloidalpower : Poloidal power usage at time t (MW)
  real(kind(1.0D0)), dimension(5) :: poloidalpower = 0.0D0


end module pf_power_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module heat_transport_variables

  !+ad_name  heat_transport_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  heat transport system
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  heat transport system of a fusion power plant, and
  !+ad_desc  also those for a hydrogen production plant.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  27/03/13 PJK Comment change to fmgdmw
  !+ad_hist  11/04/13 PJK Comment change to tfacpd
  !+ad_hist  17/04/13 PJK Comment change to fcsht, priheat
  !+ad_hist  17/04/13 PJK Added iprimnloss
  !+ad_hist  04/06/14 PJK Added/modified various quantities for new power flow method
  !+ad_hist  17/06/14 PJK Comment change to pfwdiv, ctht
  !+ad_hist  21/08/14 PJK Added etathdiv
  !+ad_hist  27/08/14 PJK Replaced etahtp* with just etahtp
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_hist  22/10/14 PJK Removed psechole, etathdiv
  !+ad_hist  05/11/14 PJK Added htpmw_*
  !+ad_hist  10/12/14 PJK Replaced real rnphx with integer nphx;
  !+ad_hisc               deleted ctht, rnihx;
  !+ad_hisc               modified some descriptions
  !+ad_hist  17/12/14 PJK Modified htpmw_* descriptions
  !+ad_hist  13/01/15 PJK Changed pinjht description
  !+ad_hist  08/03/17 JM  Added time-dependent power reqs
  !+ad_hist  10/03/17 JM  Removed ffwlg (issue #473)
  !+ad_hist  22/06/18 SIM Added etatf (previously hardwired)
  !+ad_hist  22/06/18 SIM tfacpd now always an output
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  baseel /5.0e6/ : base plant electric load (W)
  real(kind(1.0D0)) :: baseel = 5.0D6
  !+ad_vars  crypmw : cryogenic plant power (MW)
  real(kind(1.0D0)) :: crypmw = 0.0D0

  !+ad_vars  etatf /0.9/ : AC to resistive power conversion for TF coils
  real(kind(1.0D0)) :: etatf = 0.9D0
  !+ad_vars  etath /0.35/ : thermal to electric conversion efficiency
  !+ad_varc                 if secondary_cycle=2; otherwise calculated
  real(kind(1.0D0)) :: etath = 0.35D0
  !+ad_vars  fachtmw : facility heat removal (MW)
  real(kind(1.0D0)) :: fachtmw = 0.0D0
  !+ad_vars  fcsht : total baseline power required at all times (MW)
  real(kind(1.0D0)) :: fcsht = 0.0D0
  !+ad_vars  fgrosbop : scaled fraction of gross power to balance-of-plant
  real(kind(1.0D0)) :: fgrosbop = 0.0D0
  !+ad_vars  fmgdmw /0.0/ : power to mgf (motor-generator flywheel) units (MW)
  !+ad_varc                 (ignored if iscenr=2)
  real(kind(1.0D0)) :: fmgdmw = 0.0D0
  !+ad_vars  fpumpblkt /0.005/ : fraction of total blanket thermal power required
  !+ad_varc                      to drive the blanket coolant pumps (default assumes
  !+ad_varc                      water coolant) (secondary_cycle=0)
  real(kind(1.0D0)) :: fpumpblkt = 0.005D0
  !+ad_vars  fpumpdiv /0.005/ : fraction of total divertor thermal power required
  !+ad_varc                     to drive the divertor coolant pumps (default assumes
  !+ad_varc                     water coolant)
  real(kind(1.0D0)) :: fpumpdiv = 0.005D0
  !+ad_vars  fpumpfw /0.005/ : fraction of total first wall thermal power required
  !+ad_varc                    to drive the FW coolant pumps (default assumes water
  !+ad_varc                    coolant) (secondary_cycle=0)
  real(kind(1.0D0)) :: fpumpfw = 0.005D0
  !+ad_vars  fpumpshld /0.005/ : fraction of total shield thermal power required
  !+ad_varc                      to drive the shield coolant pumps (default assumes
  !+ad_varc                      water coolant)
  real(kind(1.0D0)) :: fpumpshld = 0.005D0
  !+ad_vars  htpmw_min /0.0/ : Minimum total electrical power for primary coolant pumps (MW) NOT RECOMMENDED
  real(kind(1.0D0)) :: htpmw_min = 0.0D0

  !+ad_vars  helpow : heat removal at cryogenic temperatures (W)
  real(kind(1.0D0)) :: helpow = 0.0D0
  !+ad_vars  htpmw  :: heat transport system electrical pump power (MW)
  real(kind(1.0D0)) :: htpmw = 0.0D0
  !+ad_vars  htpmw_blkt /0.0/ : blanket coolant mechanical pumping power (MW)
  real(kind(1.0D0)) :: htpmw_blkt = 0.0D0
  !+ad_vars  htpmw_div /0.0/ : divertor coolant mechanical pumping power (MW)
  real(kind(1.0D0)) :: htpmw_div = 0.0D0
  !+ad_vars  htpmw_fw /0.0/ : first wall coolant mechanical pumping power (MW)
  real(kind(1.0D0)) :: htpmw_fw = 0.0D0
  !+ad_vars  htpmw_shld /.0/ : shield and vacuum vessel coolant mechanical pumping power (MW)
  real(kind(1.0D0)) :: htpmw_shld = 0.0D0
  !+ad_vars  htpsecmw : Waste power lost from primary coolant pumps (MW)
  !+ad_varc
  real(kind(1.0D0)) :: htpsecmw = 0.0D0


  ! Issue #506 Hydrogen production removed

  !  To be REMOVED
  !+ad_vars  ipowerflow /1/ : switch for power flow model:<UL>
  !+ad_varc              <LI> = 0 pre-2014 version;
  !+ad_varc              <LI> = 1 comprehensive 2014 model</UL>
  integer :: ipowerflow = 1
  !+ad_vars  iprimnloss /0/ : switch for lost neutron power through holes destiny:<UL>
  !+ad_varc              <LI> = 0 does not contribute to energy generation cycle;
  !+ad_varc              <LI> = 1 contributes to energy generation cycle</UL>
  !+ad_varc              (ipowerflow=0)
  integer :: iprimnloss = 0

  ! KEEP
  !+ad_vars  iprimshld /1/ : switch for shield thermal power destiny:<UL>
  !+ad_varc             <LI> = 0 does not contribute to energy generation cycle;
  !+ad_varc             <LI> = 1 contributes to energy generation cycle</UL>
  !+ad_varc
  integer :: iprimshld = 1



  !+ad_vars  nphx : number of primary heat exchangers
  integer :: nphx = 0
  !+ad_vars  pacpmw : total pulsed power system load (MW)
  real(kind(1.0D0)) :: pacpmw = 0.0D0
  !+ad_vars  peakmva : peak MVA requirement
  real(kind(1.0D0)) :: peakmva = 0.0D0
  !+ad_vars  pfwdiv : heat removal from first wall/divertor (MW)
  real(kind(1.0D0)) :: pfwdiv = 0.0D0
  !+ad_vars  pgrossmw : gross electric power (MW)
  real(kind(1.0D0)), bind(C) :: pgrossmw = 0.0D0
  !+ad_vars  pinjht : power dissipated in heating and current drive system (MW)
  real(kind(1.0D0)) :: pinjht = 0.0D0
  !+ad_vars  pinjmax : maximum injector power during pulse (heating and ramp-up/down phase) (MW)
  real(kind(1.0D0)) :: pinjmax = 120.0D0
  !+ad_vars  pinjwp : injector wall plug power (MW)
  real(kind(1.0D0)), bind(C) :: pinjwp = 0.0D0
  !+ad_vars  pnetelmw : net electric power (MW)
  real(kind(1.0D0)) :: pnetelmw = 0.0D0
  !+ad_vars  precircmw : recirculating electric power (MW)
  real(kind(1.0D0)) :: precircmw = 0.0D0
  !+ad_vars  priheat : total thermal power removed from fusion core (MW)
  real(kind(1.0D0)) :: priheat = 0.0D0
  !+ad_vars  psecdiv : Low-grade heat lost in divertor (MW)
  real(kind(1.0D0)) :: psecdiv = 0.0D0
  !+ad_vars  psechcd : Low-grade heat lost into HCD apparatus (MW)
  real(kind(1.0D0)) :: psechcd = 0.0D0
  !+ad_vars  psechtmw : Low-grade heat (MW)
  real(kind(1.0D0)) :: psechtmw = 0.0D0

  ! NEW
  !+ad_vars  pseclossmw : Low-grade heat (VV + lost)(MW)
  real(kind(1.0D0)) :: pseclossmw = 0.0D0

  !+ad_vars  psecshld : Low-grade heat deposited in shield (MW)
  real(kind(1.0D0)) :: psecshld = 0.0D0
  !+ad_vars  pthermmw : High-grade heat useful for electric production (MW)
  real(kind(1.0D0)), bind(C) :: pthermmw = 0.0D0
  !+ad_vars  pwpm2 /150.0/ : base AC power requirement per unit floor area (W/m2)
  real(kind(1.0D0)) :: pwpm2 = 150.0D0
  !+ad_vars  tfacpd : total steady state TF coil AC power demand (MW)
  real(kind(1.0D0)) :: tfacpd = 0.0D0
  !+ad_vars  tlvpmw : estimate of total low voltage power (MW)
  real(kind(1.0D0)) :: tlvpmw = 0.0D0
  !+ad_vars  trithtmw /15.0/ : power required for tritium processing (MW)
  real(kind(1.0D0)) :: trithtmw = 15.0D0
  !+ad_vars  tturb : coolant temperature at turbine inlet (K) (secondary_cycle = 3,4)
  real(kind(1.0D0)) :: tturb = 0.0D0
  !+ad_vars  vachtmw /0.5/ : vacuum pump power (MW)
  real(kind(1.0D0)) :: vachtmw = 0.5D0

end module heat_transport_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module times_variables

  !+ad_name  times_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  plasma pulse timings
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  plasma pulse timings.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  27/06/13 PJK Relabelled tohs, tohsin
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  12/11/14 PJK Added tcycle; tdwell default changed from 100s to 1800s
  !+ad_hist  06/03/17 JM  Added pulsetimings switch
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  pulsetimings /0.0/ : switch for pulse timings (if lpulse=1):<UL>
  !+ad_varc            <LI> = 0, tohs = Ip(MA)/0.1 tramp, tqnch = input;
  !+ad_varc            <LI> = 1, tohs = iteration var or input. tramp/tqnch max of input or tohs</UL>
  real(kind(1.0D0)) :: pulsetimings = 1.0D0
  !+ad_vars  tburn /1000.0/ : burn time (s) (calculated if lpulse=1)
  real(kind(1.0D0)) :: tburn = 1000.0D0
  !+ad_vars  tburn0 : burn time (s) - used for internal consistency
  real(kind(1.0D0)) :: tburn0 = 0.0D0
  !+ad_vars  tcycle : full cycle time (s)
  real(kind(1.0D0)), bind(C) :: tcycle = 0.0D0
  !+ad_vars  tdown : down time (s)
  real(kind(1.0D0)) :: tdown = 0.0D0
  !+ad_vars  tdwell /1800.0/ : time between pulses in a pulsed reactor (s)
  !+ad_varc                   (iteration variable 17)
  real(kind(1.0D0)) :: tdwell = 1800.0D0
  !+ad_vars  theat /10.0/ : heating time, after current ramp up (s)
  real(kind(1.0D0)) :: theat = 10.0D0
  !+ad_vars  tim(6) : array of time points during plasma pulse (s)
  real(kind(1.0D0)), dimension(6) :: tim = 0.0D0
  !+ad_vars  timelabel(6) : array of time labels during plasma pulse (s)
  character(len=11), dimension(6) :: timelabel = (/ 'Start',   &
                                                    'BOP  ',     &
                                                    'EOR  ',     &
                                                    'BOF  ',     &
                                                    'EOF  ',     &
                                                    'EOP  ' /)
  !+ad_vars  intervallabel(6) : time intervals - as strings (s)
  character(len=11), dimension(5) :: intervallabel = (/ 'tramp',     &
                                                        'tohs ',      &
                                                        'theat',     &
                                                        'tburn',     &
                                                        'tqnch' /)
  !+ad_vars  tohs /30.0/ : plasma current ramp-up time for current initiation (s)
  !+ad_varc                (but calculated if lpulse=0)
  !+ad_varc                (iteration variable 65)
  real(kind(1.0D0)) :: tohs = 30.0D0
  !+ad_vars  tohsin /0.0/ : switch for plasma current ramp-up time (if lpulse=0):<UL>
  !+ad_varc            <LI> = 0, tohs = tramp = tqnch = Ip(MA)/0.5;
  !+ad_varc            <LI> <>0, tohs = tohsin; tramp, tqnch are input</UL>
  real(kind(1.0D0)) :: tohsin = 0.0D0
  !+ad_vars  tpulse : pulse length = tohs + theat + tburn + tqnch
  real(kind(1.0D0)) :: tpulse = 0.0D0
  !+ad_vars  tqnch /15.0/ : shut down time for PF coils (s); if pulsed, = tohs
  real(kind(1.0D0)) :: tqnch = 15.0D0
  !+ad_vars  tramp /15.0/ : initial PF coil charge time (s); if pulsed, = tohs
  real(kind(1.0D0)) :: tramp = 15.0D0

end module times_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module buildings_variables

  !+ad_name  buildings_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  plant buildings
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  plant buildings.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  08/04/13 PJK Modified wrbi comment
  !+ad_hist  09/04/13 PJK Changed clh1 default from 8.0 to 2.5
  !+ad_hist  09/04/13 PJK Added building volume multipliers rbvfac, mbvfac, wsvfac
  !+ad_hist  11/04/13 PJK Comment change to esbldgm3
  !+ad_hist  03/09/14 PJK Comment change to clh1
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  admv /1.0e5/ : administration building volume (m3)
  real(kind(1.0D0)) :: admv = 1.0D5
  !+ad_vars  admvol : volume of administration buildings (m3)
  real(kind(1.0D0)) :: admvol = 0.0D0
  !+ad_vars  clh1 /2.5/ : vertical clearance from TF coil to cryostat (m)
  !+ad_varc               (calculated for tokamaks)
  real(kind(1.0D0)) :: clh1 = 2.5D0
  !+ad_vars  clh2 /15.0/ : clearance beneath TF coil to foundation
  !+ad_varc                (including basement) (m)
  real(kind(1.0D0)) :: clh2 = 15.0D0
  !+ad_vars  conv /6.0e4/ : control building volume (m3)
  real(kind(1.0D0)) :: conv = 6.0D4
  !+ad_vars  convol : volume of control, protection and i&c building (m3)
  real(kind(1.0D0)) :: convol = 0.0D0
  !+ad_vars  cryvol : volume of cryoplant building (m3)
  real(kind(1.0D0)) :: cryvol = 0.0D0
  !+ad_vars  efloor : effective total floor space (m2)
  real(kind(1.0D0)) :: efloor = 0.0D0
  !+ad_vars  elevol : volume of electrical equipment building (m3)
  real(kind(1.0D0)) :: elevol = 0.0D0
  !+ad_vars  esbldgm3 /1.0e3/ : volume of energy storage equipment building (m3)
  !+ad_varc                     (not used if lpulse=0)
  real(kind(1.0D0)) :: esbldgm3 = 1.0D3
  !+ad_vars  fndt /2.0/ : foundation thickness (m)
  real(kind(1.0D0)) :: fndt = 2.0D0
  !+ad_vars  hccl /5.0/ : clearance around components in hot cell (m)
  real(kind(1.0D0)) :: hccl = 5.0D0
  !+ad_vars  hcwt /1.5/ : hot cell wall thickness (m)
  real(kind(1.0D0)) :: hcwt = 1.5D0
  !+ad_vars  mbvfac /2.8/ : maintenance building volume multiplication factor
  real(kind(1.0D0)) :: mbvfac = 2.8D0
  !+ad_vars  pfbldgm3 /2.0e4/ : volume of PF coil power supply building (m3)
  real(kind(1.0D0)) :: pfbldgm3 = 2.0D4
  !+ad_vars  pibv /2.0e4/ : power injection building volume (m3)
  real(kind(1.0D0)) :: pibv = 2.0D4
  !+ad_vars  rbrt /1.0/ : reactor building roof thickness (m)
  real(kind(1.0D0)) :: rbrt = 1.0D0
  !+ad_vars  rbvfac /1.6/ : reactor building volume multiplication factor
  real(kind(1.0D0)) :: rbvfac = 1.6D0
  !+ad_vars  rbvol : reactor building volume (m3)
  real(kind(1.0D0)) :: rbvol = 0.0D0
  !+ad_vars  rbwt /2.0/ : reactor building wall thickness (m)
  real(kind(1.0D0)) :: rbwt = 2.0D0
  !+ad_vars  rmbvol : volume of maintenance and assembly building (m3)
  real(kind(1.0D0)) :: rmbvol = 0.0D0
  !+ad_vars  row /4.0/ : clearance to building wall for crane operation (m)
  real(kind(1.0D0)) :: row = 4.0D0
  !+ad_vars  rxcl /4.0/ : clearance around reactor (m)
  real(kind(1.0D0)) :: rxcl = 4.0D0
  !+ad_vars  shmf /0.5/ : fraction of shield mass per TF coil
  !+ad_varc               to be moved in the maximum shield lift
  real(kind(1.0D0)) :: shmf = 0.5D0
  !+ad_vars  shov /1.0e5/ : shops and warehouse volume (m3)
  real(kind(1.0D0)) :: shov = 1.0D5
  !+ad_vars  shovol :volume of shops and buildings for plant auxiliaries (m3)
  real(kind(1.0D0)) :: shovol = 0.0D0
  !+ad_vars  stcl /3.0/ : clearance above crane to roof (m)
  real(kind(1.0D0)) :: stcl = 3.0D0
  !+ad_vars  tfcbv /2.0e4/ : volume of TF coil power supply building (m3)
  !+ad_varc                  (calculated if TF coils are superconducting)
  real(kind(1.0D0)) :: tfcbv = 2.0D4
  !+ad_vars  trcl /1.0/ : transportation clearance between components (m)
  real(kind(1.0D0)) :: trcl = 1.0D0
  !+ad_vars  triv /4.0e4/ : volume of tritium, fuel handling and
  !+ad_varc                 health physics buildings (m3)
  real(kind(1.0D0)) :: triv = 4.0D4
  !+ad_vars  volnucb : sum of nuclear buildings volumes (m3)
  real(kind(1.0D0)) :: volnucb = 0.0D0
  !+ad_vars  volrci : internal volume of reactor building (m3)
  real(kind(1.0D0)) :: volrci = 0.0D0
  !+ad_vars  wgt /5.0e5/ : reactor building crane capacity (kg)
  !+ad_varc                (calculated if 0 is input)
  real(kind(1.0D0)) :: wgt = 5.0D5
  !+ad_vars  wgt2 /1.0e5/ : hot cell crane capacity (kg)
  !+ad_varc                 (calculated if 0 is input)
  real(kind(1.0D0)) :: wgt2 = 1.0D5
  !+ad_vars  wrbi : distance from centre of machine to building wall (m),
  !+ad_varc         i.e. reactor building half-width
  real(kind(1.0D0)) :: wrbi = 0.0D0
  !+ad_vars  wsvfac /1.9/ : warm shop building volume multiplication factor
  real(kind(1.0D0)) :: wsvfac = 1.9D0
  !+ad_vars  wsvol : volume of warm shop building (m3)
  real(kind(1.0D0)) :: wsvol = 0.0D0

end module buildings_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module build_variables

  !+ad_name  build_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  machine's radial and vertical build
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  fusion power core's radial and vertical geometry (build).
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  18/12/12 PJK Added hpfdif, hpfu
  !+ad_hist  09/04/13 PJK Relabelled ddwex, ddwi etc.
  !+ad_hist  10/04/13 PJK Relabelled gapsto, gapomin etc.
  !+ad_hist  15/05/13 PJK Relabelled gapds, gapomin, gapsto, vgap2; added blnktth
  !+ad_hist  22/05/13 PJK Added blanket subcomponent thicknesses
  !+ad_hist  05/06/13 PJK Modified shldtth comment
  !+ad_hist  25/09/13 PJK Removed prtsz, prtszreq
  !+ad_hist  24/06/14 PJK Removed bcylth
  !+ad_hist  03/09/14 PJK Added clhsf
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  19/11/15 RK  Added precompression variables, thshield, vgaptop, shldlth, tftsgap, vvblgap
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  aplasmin /0.25/ : minimum minor radius (m)
  real(kind(1.0D0)) :: aplasmin = 0.25D0
  !+ad_vars  blarea : blanket total surface area (m2)
  real(kind(1.0D0)) :: blarea = 0.0D0
  !+ad_vars  blareaib : inboard blanket surface area (m2)
  real(kind(1.0D0)) :: blareaib = 0.0D0
  !+ad_vars  blareaob : outboard blanket surface area (m2)
  real(kind(1.0D0)) :: blareaob = 0.0D0
  !+ad_vars  blbmith /0.17/ : inboard blanket box manifold thickness (m)
  !+ad_varc                    (blktmodel>0)
  real(kind(1.0D0)) :: blbmith = 0.17D0
  !+ad_vars  blbmoth /0.27/ : outboard blanket box manifold thickness (m)
  !+ad_varc                    (blktmodel>0)
  real(kind(1.0D0)) :: blbmoth = 0.27D0
  !+ad_vars  blbpith /0.30/ : inboard blanket base plate thickness (m)
  !+ad_varc                    (blktmodel>0)
  real(kind(1.0D0)) :: blbpith = 0.30D0
  !+ad_vars  blbpoth /0.35/ : outboard blanket base plate thickness (m)
  !+ad_varc                    (blktmodel>0)
  real(kind(1.0D0)) :: blbpoth = 0.35D0
  !+ad_vars  blbuith /0.365/ : inboard blanket breeding zone thickness (m)
  !+ad_varc                    (blktmodel>0)
  !+ad_varc                    (iteration variable 90)
  real(kind(1.0D0)) :: blbuith = 0.365D0
  !+ad_vars  blbuoth /0.465/ : outboard blanket breeding zone thickness (m)
  !+ad_varc                    (blktmodel>0)
  !+ad_varc                    (iteration variable 91)
  real(kind(1.0D0)) :: blbuoth = 0.465D0
  !+ad_vars  blnkith /0.115/ : inboard blanket thickness (m);
  !+ad_varc                    (calculated if blktmodel > 0)
  !+ad_varc                    (=0.0 if iblnkith=0)
  real(kind(1.0D0)) :: blnkith = 0.115D0
  !+ad_vars  blnkoth /0.235/ : outboard blanket thickness (m);
  !+ad_varc                    calculated if blktmodel > 0
  real(kind(1.0D0)) :: blnkoth = 0.235D0
  !+ad_vars  blnktth : top blanket thickness (m),
  !+ad_varc            = mean of inboard and outboard blanket thicknesses
  real(kind(1.0D0)) :: blnktth = 0.0D0
  !+ad_vars  bore /1.42/ : central solenoid inboard radius (m)
  !+ad_varc                (iteration variable 29)
  real(kind(1.0D0)) :: bore = 1.42D0
  !+ad_vars  clhsf /4.268/ : cryostat lid height scaling factor (tokamaks)
  real(kind(1.0D0)) :: clhsf = 4.268D0
  !+ad_vars  ddwex /0.07/ : cryostat thickness (m)
  real(kind(1.0D0)) :: ddwex = 0.07D0
  !+ad_vars  ddwi /0.07/ : vacuum vessel thickness (TF coil / shield) (m)
  real(kind(1.0D0)) :: ddwi = 0.07D0
  !+ad_vars  fcspc /0.6/ : Fraction of space occupied by CS pre-compression structure
  real(kind(1.0D0)) :: fcspc = 0.6D0
  !+ad_vars  fmsbc /0.0/ : Martensitic fraction of steel in (non-existent!) bucking cylinder
  real(kind(1.0D0)) :: fmsbc = 0.0D0
  !+ad_vars  fmsbl /0.0/ : Martensitic fraction of steel in blanket
  real(kind(1.0D0)) :: fmsbl = 0.0D0
  !+ad_vars  fmsdwe /0.0/ : Martensitic fraction of steel in cryostat
  real(kind(1.0D0)) :: fmsdwe = 0.0D0
  !+ad_vars  fmsdwi /0.0/ : Martensitic fraction of steel in vacuum vessel
  real(kind(1.0D0)) :: fmsdwi = 0.0D0
  !+ad_vars  fmsfw /0.0/ : Martensitic fraction of steel in first wall
  real(kind(1.0D0)) :: fmsfw = 0.0D0
  !+ad_vars  fmsoh /0.0/ : Martensitic fraction of steel in central solenoid
  real(kind(1.0D0)) :: fmsoh = 0.0D0
  !+ad_vars  fmssh /0.0/ : Martensitic fraction of steel in shield
  real(kind(1.0D0)) :: fmssh = 0.0D0
  !+ad_vars  fmstf /0.0/ : Martensitic fraction of steel in TF coil
  real(kind(1.0D0)) :: fmstf = 0.0D0
  !+ad_vars  fseppc /3.5d8/ : Separation force in CS coil pre-compression structure
  real(kind(1.0D0)) :: fseppc = 3.5D8
  !+ad_vars  fwarea : first wall total surface area (m2)
  real(kind(1.0D0)) :: fwarea = 0.0D0
  !+ad_vars  fwareaib : inboard first wall surface area (m2)
  real(kind(1.0D0)) :: fwareaib = 0.0D0
  !+ad_vars  fwareaob : outboard first wall surface area (m2)
  real(kind(1.0D0)) :: fwareaob = 0.0D0

  ! MDK These are now calculated
  !+ad_vars  fwith : inboard first wall thickness, initial estimate (m)
  real(kind(1.0D0)) :: fwith = 0.0D0
  !+ad_vars  fwoth : outboard first wall thickness, initial estimate (m)
  real(kind(1.0D0)) :: fwoth = 0.0D0
  ! Issue #481 Rename gapds
  !+ad_vars  gapds /0.155/ : gap between inboard vacuum vessel and thermal shield (m)
  !+ad_varc                (iteration variable 61)
  real(kind(1.0D0)) :: gapds = 0.155D0
  !+ad_vars  gapoh /0.08/ : gap between central solenoid and TF coil (m)
  !+ad_varc                (iteration variable 42)
  real(kind(1.0D0)) :: gapoh = 0.08D0
  !+ad_vars  gapomin /0.234/ : minimum gap between outboard vacuum vessel and TF coil (m)
  !+ad_varc                   (iteration variable 31)
  real(kind(1.0D0)) :: gapomin = 0.234D0
  !+ad_vars  gapsto : gap between outboard vacuum vessel and TF coil (m)
  real(kind(1.0D0)) :: gapsto = 0.0D0
  !+ad_vars  hmax : maximum (half-)height of TF coil (inside edge) (m)
  real(kind(1.0D0)) :: hmax = 0.0D0
  !+ad_vars  hpfdif : difference in distance from midplane of upper and lower
  !+ad_varc           portions of TF legs (non-zero for single-null devices) (m)
  real(kind(1.0D0)) :: hpfdif = 0.0D0
  !+ad_vars  hpfu : height to top of (upper) TF coil leg (m)
  real(kind(1.0D0)) :: hpfu = 0.0D0
  !+ad_vars  hr1 : half-height of TF coil inboard leg straight section (m)
  real(kind(1.0D0)) :: hr1 = 0.0D0
  !+ad_vars  iohcl /1/ : switch for existence of central solenoid:<UL>
  !+ad_varc         <LI> = 0 central solenoid not present;
  !+ad_varc         <LI> = 1 central solenoid exists</UL>
  integer :: iohcl = 1

  !+ad_vars  iprecomp /1/ : switch for existence of central solenoid pre-compression structure:<UL>
  !+ad_varc         <LI> = 0 no pre-compression structure;
  !+ad_varc         <LI> = 1 calculated pre-compression structure</UL>
  integer :: iprecomp = 1


  !+ad_vars  ohcth /0.811/ : central solenoid thickness (m)
  !+ad_varc                 (iteration variable 16)
  real(kind(1.0D0)) :: ohcth = 0.811D0
  !+ad_vars  precomp : CS coil precompression structure thickness (m)
  real(kind(1.0D0)) :: precomp = 0.0D0
  !+ad_vars  rbld : sum of thicknesses to the major radius (m)
  real(kind(1.0D0)) :: rbld = 0.0D0
  !+ad_vars  rinboard /0.651/ : plasma inboard radius (m)
  !+ad_varc                     (consistency equation 29)
  real(kind(1.0D0)) :: rinboard = 0.651D0
  !+ad_vars  rsldi : radius to inboard shield (inside point) (m)
  real(kind(1.0D0)) :: rsldi = 0.0D0
  !+ad_vars  rsldo : radius to outboard shield (outside point) (m)
  real(kind(1.0D0)) :: rsldo = 0.0D0
  !+ad_vars  r_tf_inleg_mid : radius of centre of inboard TF leg (m)
  real(kind(1.0D0)) :: r_tf_inleg_mid = 0.0D0
  !+ad_vars  rtot : radius to the centre of the outboard TF coil leg (m)
  real(kind(1.0D0)) :: rtot = 0.0D0
  !+ad_vars  scrapli /0.14/ : gap between plasma and first wall, inboard side (m)
  !+ad_varc                   (used if iscrp=1) (iteration variable 73)
  real(kind(1.0D0)) :: scrapli = 0.14D0
  !+ad_vars  scraplo /0.15/ : gap between plasma and first wall, outboard side (m)
  !+ad_varc                   (used if iscrp=1) (iteration variable 74)
  real(kind(1.0D0)) :: scraplo = 0.15D0
  !+ad_vars  sharea : shield total surface area (m2)
  real(kind(1.0D0)) :: sharea = 0.0D0
  !+ad_vars  shareaib : inboard shield surface area (m2)
  real(kind(1.0D0)) :: shareaib = 0.0D0
  !+ad_vars  shareaob : outboard shield surface area (m2)
  real(kind(1.0D0)) :: shareaob = 0.0D0
  !+ad_vars  shldith /0.69/ : inboard shield thickness (m)
  !+ad_varc                   (iteration variable 93)
  real(kind(1.0D0)) :: shldith = 0.69D0
  !+ad_vars  shldlth /0.7/ : lower (under divertor) shield thickness (m)
   real(kind(1.0D0)) :: shldlth = 0.7D0
  !+ad_vars  shldoth /1.05/ : outboard shield thickness (m)
  !+ad_varc                   (iteration variable 94)
  real(kind(1.0D0)) :: shldoth = 1.05D0
  !+ad_vars  shldtth /0.60/ : upper/lower shield thickness (m);
  !+ad_varc                   calculated if blktmodel > 0
  real(kind(1.0D0)) :: shldtth = 0.6D0
  !+ad_vars  sigallpc /3.0d8/ : allowable stress in CSpre-compression structure (Pa);
  real(kind(1.0D0)) :: sigallpc = 3.0D8

  ! Issue #514 Make tfcth an output not an iteration variable
  !!!+ad_vars  tfcth /1.173/ : inboard TF coil(s  ) thickness (m)
  !!!+ad_varc                (calculated for stellarators)
  !!!+ad_varc                (iteration variable 13)
  !real(kind(1.0D0)) :: tfcth = 1.173D0

  !+ad_vars  tfcth : inboard TF coil thickness, (centrepost for ST) (m)
  !+ad_varc                (calculated, NOT an iteration variable)
  real(kind(1.0D0)) :: tfcth = 0.0D0

  !+ad_vars  tfoffset : vertical distance between centre of TF coils and centre of plasma (m)
  real(kind(1.0D0)) :: tfoffset = 0.0D0
  !+ad_vars  tfootfi /1.19/ : TF coil outboard leg / inboard leg radial thickness
  !+ad_varc                  ratio (itfsup=0 only)
  !+ad_varc                  (iteration variable 75)
  real(kind(1.0D0)) :: tfootfi = 1.19D0
  !+ad_vars  tfthko : outboard TF coil thickness (m)
  real(kind(1.0D0)) :: tfthko = 0.0D0
  !+ad_vars  tftsgap /0.05/ : Minimum metal-to-metal gap between TF coil and thermal shield (m)
  real(kind(1.0D0)) :: tftsgap = 0.05D0
  !+ad_vars  thshield /0.05/ : TF-VV thermal shield thickness (m)
  real(kind(1.0D0)) :: thshield = 0.05D0

  !+ad_vars  vgap2 /0.163/ : vertical gap between vacuum vessel and thermal shields (m)
  real(kind(1.0D0)) :: vgap2 = 0.163D0
  ! Issue #481 Remove vgaptf
  !+ad_vars  vgap /0.0/ : vertical gap between x-point and divertor (m)
  !+ad_varc                (if = 0, it is calculated)
  real(kind(1.0D0)) :: vgap= 0.0D0
  !+ad_vars  vgaptop /0.60/ : vertical gap between top of plasma and first wall (m)
  real(kind(1.0D0)) :: vgaptop = 0.60D0
  !+ad_vars  vvblgap /0.05/ : gap between vacuum vessel and blanket (m)
  real(kind(1.0D0)) :: vvblgap = 0.05D0

  !+ad_vars  plleni /1.0/ : length of inboard divertor plate (m)
  real(kind(1.0D0)) :: plleni = 1.0D0
  !+ad_vars  plleno /1.0/ : length of outboard divertor plate (m)
  real(kind(1.0D0)) :: plleno = 1.0D0
  !+ad_vars  plsepi /1.0/ : poloidal length, x-point to inboard strike point (m)
  real(kind(1.0D0)) :: plsepi = 1.0D0
  !+ad_vars  plsepo /1.5/ : poloidal length, x-point to outboard strike point (m)
  real(kind(1.0D0)) :: plsepo = 1.5D0
  !+ad_vars  rspo : outboard strike point radius (m)
  real(kind(1.0D0)) :: rspo = 0.0D0


end module build_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module cost_variables

  !+ad_name  cost_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  costing algorithms
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  costing algorithms of a fusion power plant.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  31/10/12 PJK Initial version of module
  !+ad_hist  16/04/13 PJK Changed ucsc dimensions
  !+ad_hist  18/06/13 PJK Changed uccryo from cryostat to vacuum vessel
  !+ad_hist  15/08/13 PJK Changed cdrlife description
  !+ad_hist  03/12/13 PJK Changed ucfwps units from $/m2 to $
  !+ad_hist  19/11/14 PJK Modified iavail wording
  !+ad_hist  25/11/14 JM  Added new availability model variables
  !+ad_hist  02/12/14 PJK Changed abktflnc, adivflnc default values
  !+ad_hist  10/12/14 PJK Removed ucihx
  !+ad_hist  05/01/15 JM  Added 2015 costs model variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  abktflnc /5.0/ : allowable first wall/blanket neutron
  !+ad_varc                    fluence (MW-yr/m2) (blktmodel=0)
  real(kind(1.0D0)), bind(C) :: abktflnc = 5.0D0
  !+ad_vars  adivflnc /7.0/ : allowable divertor heat fluence (MW-yr/m2)
  real(kind(1.0D0)), bind(C) :: adivflnc = 7.0D0
  !+ad_vars  blkcst : blanket direct cost (M$)
  real(kind(1.0D0)) :: blkcst = 0.0D0
  !+ad_vars  c221 : total account 221 cost (M$) (first wall, blanket, shield,
  !+ad_varc         support structure and divertor plates)
  real(kind(1.0D0)) :: c221 = 0.0D0
  !+ad_vars  c222 : total account 222 cost (M$) (TF coils + PF coils)
  real(kind(1.0D0)) :: c222 = 0.0D0
  !+ad_vars  capcost : total capital cost including interest (M$)
  real(kind(1.0D0)) :: capcost = 0.0D0
  !+ad_vars  cconfix /80.0/ : fixed cost of superconducting cable ($/m)
  real(kind(1.0D0)) :: cconfix = 80.0D0
  !+ad_vars  cconshpf /70.0/ : cost of PF coil steel conduit/sheath ($/m)
  real(kind(1.0D0)) :: cconshpf = 70.0D0
  !+ad_vars  cconshtf /75.0/ : cost of TF coil steel conduit/sheath ($/m)
  real(kind(1.0D0)) :: cconshtf = 75.0D0
  !+ad_vars  cdcost : current drive direct costs (M$)
  real(kind(1.0D0)) :: cdcost = 0.0D0
  !+ad_vars  cdirt : total plant direct cost (M$)
  real(kind(1.0D0)), bind(C) :: cdirt = 0.0D0
  !+ad_vars  cdrlife : lifetime of heating/current drive system (y)
  real(kind(1.0D0)), bind(C) :: cdrlife = 0.0D0
  !+ad_vars  cfactr /0.75/ : Total plant availability fraction;
  !+ad_varc                  input if iavail = 0
  real(kind(1.0D0)) :: cfactr = 0.75D0
  !+ad_vars  cpfact : Total plant capacity factor
  real(kind(1.0D0)) :: cpfact = 0.0D0

  !+ad_vars  cfind(4) /0.244,0.244,0.244,0.29/ : indirect cost factor (func of lsa)
  real(kind(1.0D0)), dimension(4) :: cfind = &
       (/0.244D0, 0.244D0, 0.244D0, 0.29D0/)

  !+ad_vars  cland /19.2/ : cost of land (M$)
  real(kind(1.0D0)) :: cland = 19.2D0
  !+ad_vars  coe : cost of electricity ($/MW-hr)
  real(kind(1.0D0)) :: coe = 0.0D0
  !+ad_vars  coecap : capital cost of electricity (m$/kW-hr)
  real(kind(1.0D0)) :: coecap = 0.0D0
  !+ad_vars  coefuelt : 'fuel' (including replaceable components) contribution to
  !+ad_varc             cost of electricity (m$/kW-hr)
  real(kind(1.0D0)) :: coefuelt = 0.0D0
  !+ad_vars  coeoam : operation and maintenance contribution to
  !+ad_varc           cost of electricity (m$/kW-hr)
  real(kind(1.0D0)) :: coeoam = 0.0D0
  !+ad_vars  concost : plant construction cost (M$)
  real(kind(1.0D0)) :: concost = 0.0D0
  !+ad_vars  costexp /0.8/ : cost exponent for scaling in 2015 costs model
  real(kind(1.0D0)) :: costexp = 0.8D0
  !+ad_vars  costexp_pebbles /0.6/ : cost exponent for pebbles in 2015 costs model
  real(kind(1.0D0)) :: costexp_pebbles = 0.6D0
  !+ad_vars  cost_factor_buildings /1.0/ : cost scaling factor for buildings
  real(kind(1.0D0)) :: cost_factor_buildings = 1.0D0
  !+ad_vars  cost_factor_land /1.0/ : cost scaling factor for land
  real(kind(1.0D0)) :: cost_factor_land = 1.0D0
  !+ad_vars  cost_factor_tf_coils /1.0/ : cost scaling factor for TF coils
  real(kind(1.0D0)) :: cost_factor_tf_coils = 1.0D0
  !+ad_vars  cost_factor_fwbs /1.0/ : cost scaling factor for fwbs
  real(kind(1.0D0)) :: cost_factor_fwbs = 1.0D0
  !+ad_vars  cost_factor_rh /1.0/ : cost scaling factor for remote handling
  real(kind(1.0D0)) :: cost_factor_rh = 1.0D0
  !+ad_vars  cost_factor_vv /1.0/ : cost scaling factor for vacuum vessel
  real(kind(1.0D0)) :: cost_factor_vv = 1.0D0
  !+ad_vars  cost_factor_bop /1.0/ : cost scaling factor for energy conversion system
  real(kind(1.0D0)) :: cost_factor_bop = 1.0D0
  !+ad_vars  cost_factor_misc /1.0/ : cost scaling factor for remaining subsystems
  real(kind(1.0D0)) :: cost_factor_misc = 1.0D0

  !+ad_vars  maintenance_fwbs /0.2/ : Maintenance cost factor:
  !+ad_varc                           first wall, blanket, shield, divertor
  real(kind(1.0D0)) :: maintenance_fwbs = 0.2D0
  !+ad_vars  maintenance_gen /0.05/ : Maintenance cost factor: All other components except
  !+ad_varc                           coils, vacuum vessel, thermal shield, cryostat, land
  real(kind(1.0D0)) :: maintenance_gen = 0.05D0
  !+ad_vars  amortization /13.6/ : amortization factor (fixed charge factor) "A" (years)
  real(kind(1.0D0)) :: amortization = 13.6D0

  !+ad_vars  cost_model /1/ : switch for cost model:<UL>
  !+ad_varc          <LI> = 0 use $ 1990 PROCESS model
  !+ad_varc          <LI> = 1 use $ 2015 Kovari model</UL>
  integer :: cost_model = 1
  !+ad_vars  cowner /0.15/ : owner cost factor
  real(kind(1.0D0)), bind(C) :: cowner = 0.15D0
  !+ad_vars  cplife : lifetime of centrepost (y)
  real(kind(1.0D0)) :: cplife = 0.0D0
  !+ad_vars  cpstcst : ST centrepost direct cost (M$)
  real(kind(1.0D0)) :: cpstcst = 0.0D0
  !+ad_vars  cpstflnc /10.0/ : allowable ST centrepost neutron fluence (MW-yr/m2)
  real(kind(1.0D0)), bind(C) :: cpstflnc = 10.0D0
  !+ad_vars  crctcore : reactor core costs (categories 221, 222 and 223)
  real(kind(1.0D0)) :: crctcore = 0.0D0
  !+ad_vars  csi /16.0/ : allowance for site costs (M$)
  real(kind(1.0D0)) :: csi = 16.0D0
  !+ad_vars  cturbb /380.0/ : cost of turbine building (M$)
  real(kind(1.0D0)) :: cturbb = 380.0D0
  !+ad_vars  decomf /0.1/ : proportion of constructed cost required for
  !+ad_varc                 decommissioning fund
  real(kind(1.0D0)) :: decomf = 0.1D0
  !+ad_vars  dintrt /0.0/ : diff between borrowing and saving interest rates
  real(kind(1.0D0)) :: dintrt = 0.0D0
  !+ad_vars  divcst : divertor direct cost (M$)
  real(kind(1.0D0)) :: divcst = 0.0D0
  !+ad_vars  divlife : lifetime of divertor (y)
  real(kind(1.0D0)), bind(C) :: divlife = 0.0D0
  !+ad_vars  dtlife /0.0/ : period prior to the end of the plant life that
  !+ad_varc                 the decommissioning fund is used (years)
  real(kind(1.0D0)) :: dtlife = 0.0D0
  !+ad_vars  fcap0 /1.165/ : average cost of money for construction of plant
  !+ad_varc                  assuming design/construction time of six years
  real(kind(1.0D0)) :: fcap0 = 1.165D0
  !+ad_vars  fcap0cp /1.08/ : average cost of money for replaceable components
  !+ad_varc                   assuming lead time for these of two years
  real(kind(1.0D0)) :: fcap0cp = 1.08D0
  !+ad_vars  fcdfuel /0.1/ : fraction of current drive cost treated as fuel
  !+ad_varc                  (if ifueltyp = 1)
  real(kind(1.0D0)) :: fcdfuel = 0.1D0
  !+ad_vars  fcontng /0.195/ : project contingency factor
  real(kind(1.0D0)), bind(C) :: fcontng = 0.195D0
  !+ad_vars  fcr0 /0.0966/ : fixed charge rate during construction
  real(kind(1.0D0)) :: fcr0 = 0.0966D0
  !+ad_vars  fkind /1.0/ : multiplier for Nth of a kind costs
  real(kind(1.0D0)), bind(C) :: fkind = 1.0D0
  !+ad_vars  fwallcst : first wall cost (M$)
  real(kind(1.0D0)) :: fwallcst = 0.0D0
  !+ad_vars  iavail /2/ : switch for plant availability model:<UL>
  !+ad_varc          <LI> = 0 use input value for cfactr;
  !+ad_varc          <LI> = 1 calculate cfactr using Taylor and Ward 1999 model;
  !+ad_varc          <LI> = 2 calculate cfactr using new (2015) model</UL>
  integer :: iavail= 2
  !+ad_vars  avail_min /0.75/ : Minimum availability (constraint equation 61)
  real(kind(1.0D0)) :: avail_min = 0.75D0

  !+ad_vars  tok_build_cost_per_vol /1283.0/ : Unit cost for tokamak complex buildings,
  !+ad_varc                                    including building and site services ($/m3)
  real(kind(1.0D0)) :: tok_build_cost_per_vol = 1283.0D0
  !+ad_vars  light_build_cost_per_vol /270.0/ : Unit cost for unshielded non-active buildings ($/m3)
  real(kind(1.0D0)) :: light_build_cost_per_vol = 270.0D0

  !+ad_vars  favail /1.0/ : F-value for minimum availability (constraint equation 61)
  real(kind(1.0D0)) :: favail = 1.0D0
  !+ad_vars  num_rh_systems /4/ : Number of remote handling systems (1-10)
  integer, bind(C) :: num_rh_systems = 4
  !+ad_vars  conf_mag /0.99/ : c parameter, which determines the temperature margin at which magnet lifetime starts to decline
  real(kind(1.0D0)), bind(C) :: conf_mag = 0.99D0
  !+ad_vars  div_prob_fail /0.0002/ : Divertor probability of failure (per op day)
  real(kind(1.0D0)), bind(C) :: div_prob_fail = 0.0002D0
  !+ad_vars  div_umain_time /0.25/ : Divertor unplanned maintenance time (years)
  real(kind(1.0D0)), bind(C) :: div_umain_time = 0.25D0

  ! MDK
  !+ad_vars  div_nref /7000/ : Reference value for cycle cycle life of divertor
  real(kind(1.0D0)), bind(C) :: div_nref = 7000.0D0
  !+ad_vars  div_nu /14000/ : The cycle when the divertor fails with 100% probability
  real(kind(1.0D0)), bind(C) :: div_nu = 14000.0D0


  !+ad_vars  fwbs_nref /20000/ : Reference value for cycle life of blanket
  real(kind(1.0D0)),bind(C) :: fwbs_nref = 20000.0D0
  !+ad_vars  fwbs_nu /40000/ : The cycle when the blanket fails with 100% probability
  real(kind(1.0D0)), bind(C) :: fwbs_nu = 40000.0D0


  !+ad_vars  fwbs_prob_fail /0.0002/ : Fwbs probability of failure (per op day)
  real(kind(1.0D0)), bind(C) :: fwbs_prob_fail = 0.0002D0
  !+ad_vars  fwbs_umain_time /0.25/ : Fwbs unplanned maintenance time (years)
  real(kind(1.0D0)), bind(C) :: fwbs_umain_time = 0.25D0
  !+ad_vars  redun_vacp /25/ : Vacuum system pump redundancy level (%)
  real(kind(1.0D0)) :: redun_vacp = 25.0D0
  !+ad_vars  redun_vac : Number of redundant vacuum pumps
  integer :: redun_vac = 0
  !+ad_vars  t_operation : Operational time (yrs)
  real(kind(1.0D0)), bind(C) :: t_operation = 0.0D0
  !+ad_vars  tbktrepl /0.5/ : time taken to replace blanket (y)
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: tbktrepl = 0.5D0
  !+ad_vars  tcomrepl /0.5/ : time taken to replace both blanket and divertor (y)
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: tcomrepl = 0.5D0
  !+ad_vars  tdivrepl /0.25/ : time taken to replace divertor (y)
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: tdivrepl = 0.25D0
  !+ad_vars  uubop /0.02/ : unplanned unavailability factor for balance of plant
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: uubop = 0.02D0
  !+ad_vars  uucd /0.02/ : unplanned unavailability factor for current drive
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: uucd = 0.02D0
  !+ad_vars  uudiv /0.04/ : unplanned unavailability factor for divertor
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: uudiv = 0.04D0
  !+ad_vars  uufuel /0.02/ : unplanned unavailability factor for fuel system
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: uufuel = 0.02D0
  !+ad_vars  uufw /0.04/ : unplanned unavailability factor for first wall
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: uufw = 0.04D0
  !+ad_vars  uumag /0.02/ : unplanned unavailability factor for magnets
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: uumag = 0.02D0
  !+ad_vars  uuves /0.04/ : unplanned unavailability factor for vessel
  !+ad_varc                 (iavail=1)
  real(kind(1.0D0)) :: uuves = 0.04D0

  !+ad_vars  ifueltyp /0/ : switch:<UL>
  !+ad_varc            <LI> = 1 treat blanket divertor, first wall and
  !+ad_varc                     fraction fcdfuel of CD equipment as fuel cost;
  !+ad_varc            <LI> = 0 treat these as capital cost</UL>
  integer :: ifueltyp = 0
  !+ad_vars  ipnet /0/ : switch for net electric power calculation:<UL>
  !+ad_varc         <LI> = 0 scale so that always > 0;
  !+ad_varc         <LI> = 1 let go < 0 (no c-o-e)</UL>
  integer :: ipnet = 0


  !+ad_vars  ireactor /1/ : switch for net electric power and cost of
  !+ad_varc                 electricity calculations:<UL>
  !+ad_varc            <LI> = 0 do not calculate MW(electric) or c-o-e;
  !+ad_varc            <LI> = 1 calculate MW(electric) and c-o-e</UL>
  integer, bind(C) :: ireactor = 1
  !+ad_vars  lsa /4/ : level of safety assurance switch (generally, use 3 or 4):<UL>
  !+ad_varc       <LI> = 1 truly passively safe plant;
  !+ad_varc       <LI> = 2,3 in-between;
  !+ad_varc       <LI> = 4 like current fission plant</UL>
  integer, bind(C) :: lsa = 4
  !+ad_vars  moneyint : interest portion of capital cost (M$)
  real(kind(1.0D0)) :: moneyint = 0.0D0
  !+ad_vars  output_costs /1/ : switch for costs output:<UL>
  !+ad_varc            <LI> = 0 do not write cost-related outputs to file;
  !+ad_varc            <LI> = 1 write cost-related outputs to file</UL>
  integer :: output_costs = 1
  !+ad_vars  ratecdol /0.0435/ : effective cost of money in constant dollars
  real(kind(1.0D0)) :: ratecdol = 0.0435D0
  !+ad_vars  tlife /30.0/ : plant life (years)
  real(kind(1.0D0)), bind(C) :: tlife = 30.0D0
  !+ad_vars  ucad /180.0/ FIX : unit cost for administration buildings (M$/m3)
  real(kind(1.0D0)) :: ucad = 180.0D0
  !+ad_vars  ucaf /1.5e6/ FIX : unit cost for aux facility power equipment ($)
  real(kind(1.0D0)) :: ucaf = 1.5D6
  !+ad_vars  ucahts /31.0/ FIX : unit cost for aux heat transport equipment ($/W**exphts)
  real(kind(1.0D0)) :: ucahts = 31.0D0
  !+ad_vars  ucap /17.0/ FIX : unit cost of auxiliary transformer ($/kVA)
  real(kind(1.0D0)) :: ucap = 17.0D0
  !+ad_vars  ucblbe /260.0/ : unit cost for blanket beryllium ($/kg)
  real(kind(1.0D0)) :: ucblbe = 260.0D0
  !+ad_vars  ucblbreed /875.0/ : unit cost for breeder material ($/kg) (blktmodel>0)
  real(kind(1.0D0)) :: ucblbreed = 875.0D0
  !+ad_vars  ucblli /875.0/ : unit cost for blanket lithium ($/kg) (30% Li6)
  real(kind(1.0D0)) :: ucblli = 875.0D0
  !+ad_vars  ucblli2o /600.0/ : unit cost for blanket Li_2O ($/kg)
  real(kind(1.0D0)) :: ucblli2o = 600.0D0
  !+ad_vars  ucbllipb /10.3/ : unit cost for blanket Li-Pb ($/kg) (30% Li6)
  real(kind(1.0D0)) :: ucbllipb = 10.3D0
  !+ad_vars  ucblss /90.0/ : unit cost for blanket stainless steel ($/kg)
  real(kind(1.0D0)) :: ucblss = 90.0D0
  !+ad_vars  ucblvd /200.0/ : unit cost for blanket vanadium ($/kg)
  real(kind(1.0D0)) :: ucblvd = 200.0D0
  !+ad_vars  ucbpmp /2.925e5/ FIX : vacuum system backing pump cost ($)
  real(kind(1.0D0)) :: ucbpmp = 2.925D5
  !+ad_vars  ucbus /0.123/ : cost of aluminium bus for TF coil ($/A-m)
  real(kind(1.0D0)) :: ucbus = 0.123D0
  !+ad_vars  uccase /50.0/ : cost of superconductor case ($/kg)
  real(kind(1.0D0)) :: uccase = 50.0D0
  !+ad_vars  ucco /350.0/ FIX : unit cost for control buildings (M$/m3)
  real(kind(1.0D0)) :: ucco = 350.0D0
  !+ad_vars  uccpcl1 /250.0/ : cost of high strength tapered copper ($/kg)
  real(kind(1.0D0)) :: uccpcl1 = 250.0D0
  !+ad_vars  uccpclb /150.0/ : cost of TF outboard leg plate coils ($/kg)
  real(kind(1.0D0)) :: uccpclb = 150.0D0
  !+ad_vars  uccpmp /3.9e5/ FIX : vacuum system cryopump cost ($)
  real(kind(1.0D0)) :: uccpmp = 3.9D5
  !+ad_vars  uccr /460.0/ FIX : unit cost for cryogenic building (M$/vol)
  real(kind(1.0D0)) :: uccr = 460.0D0
  !+ad_vars  uccry /9.3e4/ : heat transport system cryoplant costs ($/W**expcry)
  real(kind(1.0D0)) :: uccry = 9.3D4
  !+ad_vars  uccryo /32.0/ : unit cost for vacuum vessel ($/kg)
  real(kind(1.0D0)) :: uccryo = 32.0D0
  !+ad_vars  uccu /75.0/ : unit cost for copper in superconducting cable ($/kg)
  real(kind(1.0D0)) :: uccu = 75.0D0
  !+ad_vars  ucdgen /1.7e6/ FIX : cost per 8 MW diesel generator ($)
  real(kind(1.0D0)) :: ucdgen = 1.7D6
  !+ad_vars  ucdiv /2.8e5/ : cost of divertor blade ($)
  real(kind(1.0D0)) :: ucdiv = 2.8D5
  !+ad_vars  ucdtc /13.0/ FIX : detritiation, air cleanup cost ($/10000m3/hr)
  real(kind(1.0D0)) :: ucdtc = 13.0D0
  !+ad_vars  ucduct /4.225e4/ FIX : vacuum system duct cost ($/m)
  real(kind(1.0D0)) :: ucduct = 4.225D4
  !+ad_vars  ucech /3.0/ : ECH system cost ($/W)
  real(kind(1.0D0)) :: ucech = 3.0D0
  !+ad_vars  ucel /380.0/ FIX : unit cost for electrical equipment building (M$/m3)
  real(kind(1.0D0)) :: ucel = 380.0D0
  !+ad_vars  uces1 /3.2e4/ FIX : MGF (motor-generator flywheel) cost factor ($/MVA**0.8)
  real(kind(1.0D0)) :: uces1 = 3.2D4
  !+ad_vars  uces2 /8.8e3/ FIX : MGF (motor-generator flywheel) cost factor ($/MJ**0.8)
  real(kind(1.0D0)) :: uces2 = 8.8D3
  !+ad_vars  ucf1 /2.23e7/ : cost of fuelling system ($)
  real(kind(1.0D0)) :: ucf1 = 2.23D7
  !+ad_vars  ucfnc /35.0/ : outer PF coil fence support cost ($/kg)
  real(kind(1.0D0)) :: ucfnc = 35.0D0
  !+ad_vars  ucfpr /4.4e7/ FIX : cost of 60g/day tritium processing unit ($)
  real(kind(1.0D0)) :: ucfpr = 4.4D7
  !+ad_vars  ucfuel /3.45/ : unit cost of D-T fuel (M$/year/1200MW)
  real(kind(1.0D0)) :: ucfuel = 3.45D0
  !+ad_vars  ucfwa /6.0e4/ FIX : first wall armour cost ($/m2)
  real(kind(1.0D0)) :: ucfwa = 6.0D4
  !+ad_vars  ucfwps /1.0e7/ FIX : first wall passive stabiliser cost ($)
  real(kind(1.0D0)) :: ucfwps = 1.0D7
  !+ad_vars  ucfws /5.3e4/ FIX : first wall structure cost ($/m2)
  real(kind(1.0D0)) :: ucfws = 5.3D4
  !+ad_vars  ucgss /35.0/ FIX : cost of reactor structure ($/kg)
  real(kind(1.0D0)) :: ucgss = 35.0D0
  !+ad_vars  uche3 /1.0e6/ : cost of helium-3 ($/kg)
  real(kind(1.0D0)) :: uche3 = 1.0D6
  !+ad_vars  uchrs /87.9e6/ : cost of heat rejection system ($)
  real(kind(1.0D0)), bind(C) :: uchrs = 87.9D6
  !+ad_vars  uchts(2) /15.3,19.1/ : cost of heat transport system equipment
  !+ad_varc                         per loop ($/W); dependent on coolant type (coolwh)
  real(kind(1.0D0)), dimension(2) :: uchts = (/15.3D0, 19.1D0/)
  !+ad_vars  uciac /1.5e8/ : cost of instrumentation, control & diagnostics ($)
  real(kind(1.0D0)), bind(C) :: uciac = 1.5D8
  !+ad_vars  ucich /3.0/ : ICH system cost ($/W)
  real(kind(1.0D0)) :: ucich = 3.0D0
  !+ad_vars  ucint /35.0/ FIX : superconductor intercoil structure cost ($/kg)
  real(kind(1.0D0)) :: ucint = 35.0D0
  !+ad_vars  uclh /3.3/ : lower hybrid system cost ($/W)
  real(kind(1.0D0)) :: uclh = 3.3D0
  !+ad_vars  uclv /16.0/ FIX : low voltage system cost ($/kVA)
  real(kind(1.0D0)) :: uclv = 16.0D0
  !+ad_vars  ucmb /260.0/ FIX: unit cost for reactor maintenance building (M$/m3)
  real(kind(1.0D0)) :: ucmb = 260.0D0
  !+ad_vars  ucme /1.25e8/ : cost of maintenance equipment ($/)
  real(kind(1.0D0)), bind(C) :: ucme = 1.25D8
  !+ad_vars  ucmisc /2.5e7/ : miscellaneous plant allowance ($)
  real(kind(1.0D0)), bind(C) :: ucmisc = 2.5D7
  !+ad_vars  ucnbi /3.3/ : NBI system cost ($/W)
  real(kind(1.0D0)) :: ucnbi = 3.3D0
  !+ad_vars  ucnbv /1000.0/ FIX : cost of nuclear building ventilation ($/m3)
  real(kind(1.0D0)) :: ucnbv = 1000.0D0
  !+ad_vars  ucoam(4) /68.8,68.8,68.8,74.4/ : annual cost of operation and
  !+ad_varc                                   maintenance (M$/year/1200MW**0.5)
  real(kind(1.0D0)), dimension(4) :: ucoam = &
       (/68.8D0, 68.8D0, 68.8D0, 74.4D0/)
  !+ad_vars  ucpens /32.0/ : penetration shield cost ($/kg)
  real(kind(1.0D0)) :: ucpens = 32.0D0
  !+ad_vars  ucpfb /210.0/ : cost of PF coil buses ($/kA-m)
  real(kind(1.0D0)) :: ucpfb = 210.0D0
  !+ad_vars  ucpfbk /1.66e4/ : cost of PF coil DC breakers ($/MVA**0.7)
  real(kind(1.0D0)) :: ucpfbk = 1.66D4
  !+ad_vars  ucpfbs /4.9e3/ : cost of PF burn power supplies ($/kW**0.7)
  real(kind(1.0D0)) :: ucpfbs = 4.9D3
  !+ad_vars  ucpfcb /7.5e4/ : cost of PF coil AC breakers ($/circuit)
  real(kind(1.0D0)) :: ucpfcb = 7.5D4
  !+ad_vars  ucpfdr1 /150.0/ : cost factor for dump resistors ($/MJ)
  real(kind(1.0D0)) :: ucpfdr1 = 150.0D0
  !+ad_vars  ucpfic /1.0e4/ : cost of PF instrumentation and control ($/channel)
  real(kind(1.0D0)) :: ucpfic = 1.0D4
  !+ad_vars  ucpfps /3.5e4/ : cost of PF coil pulsed power supplies ($/MVA)
  real(kind(1.0D0)) :: ucpfps = 3.5D4
  !+ad_vars  ucphx /15.0/ FIX : primary heat transport cost ($/W**exphts)
  real(kind(1.0D0)) :: ucphx = 15.0D0
  !+ad_vars  ucpp /48.0/ FIX : cost of primary power transformers ($/kVA**0.9)
  real(kind(1.0D0)) :: ucpp = 48.0D0
  !+ad_vars  ucrb /400.0/ : cost of reactor building (M$/m3)
  real(kind(1.0D0)) :: ucrb = 400.0D0
  !+ad_vars  ucsc(6) /600.0,600.0,300.0,600.0/ : cost of superconductor ($/kg)
  real(kind(1.0D0)), dimension(6) :: ucsc = &
       (/600.0D0, 600.0D0, 300.0D0, 600.0D0, 600.0D0, 600.0D0/)
  !+ad_vars  ucsh /115.0/ FIX : cost of shops and warehouses (M$/m3)
  real(kind(1.0D0)) :: ucsh = 115.0D0
  !+ad_vars  ucshld /32.0/ : cost of shield structural steel ($/kg)
  real(kind(1.0D0)) :: ucshld = 32.0D0
  !+ad_vars  ucswyd /1.84e7/ FIX : switchyard equipment costs ($)
  real(kind(1.0D0)) :: ucswyd = 1.84D7
  !+ad_vars  uctfbr /1.22/ : cost of TF coil breakers ($/W**0.7)
  real(kind(1.0D0)) :: uctfbr = 1.22D0
  !+ad_vars  uctfbus /100.0/ : cost of TF coil bus ($/kg)
  real(kind(1.0D0)) :: uctfbus = 100.0D0
  !+ad_vars  uctfdr /1.75e-4/ FIX : cost of TF coil dump resistors ($/J)
  real(kind(1.0D0)) :: uctfdr = 1.75D-4
  !+ad_vars  uctfgr /5000.0/ FIX : additional cost of TF coil dump resistors ($/coil)
  real(kind(1.0D0)) :: uctfgr = 5000.0D0
  !+ad_vars  uctfic /1.0e4/ FIX : cost of TF coil instrumentation and control ($/coil/30)
  real(kind(1.0D0)) :: uctfic = 1.0D4
  !+ad_vars  uctfps /24.0/ : cost of TF coil power supplies ($/W**0.7)
  real(kind(1.0D0)) :: uctfps = 24.0D0
  !+ad_vars  uctfsw /1.0/ : cost of TF coil slow dump switches ($/A)
  real(kind(1.0D0)) :: uctfsw = 1.0D0
  !+ad_vars  uctpmp /1.105e5/ FIX : cost of turbomolecular pump ($)
  real(kind(1.0D0)) :: uctpmp = 1.105D5
  !+ad_vars  uctr /370.0/ FIX : cost of tritium building ($/m3)
  real(kind(1.0D0)) :: uctr = 370.0D0
  !+ad_vars  ucturb(2) /230.0e6, 245.0e6/: cost of turbine plant equipment ($)
  !+ad_varc                               (dependent on coolant type coolwh)
  real(kind(1.0D0)), dimension(2), bind(C) :: ucturb = (/230.0D6, 245.0D6/)
  !+ad_vars  ucvalv /3.9e5/ FIX : vacuum system valve cost ($)
  real(kind(1.0D0)) :: ucvalv = 3.9D5
  !+ad_vars  ucvdsh /26.0/ FIX : vacuum duct shield cost ($/kg)
  real(kind(1.0D0)) :: ucvdsh = 26.0D0
  !+ad_vars  ucviac /1.3e6/ FIX : vacuum system instrumentation and control cost ($)
  real(kind(1.0D0)) :: ucviac = 1.3D6
  !+ad_vars  ucwindpf /465.0/ : cost of PF coil superconductor windings ($/m)
  real(kind(1.0D0)) :: ucwindpf = 465.0D0
  !+ad_vars  ucwindtf /480.0/ : cost of TF coil superconductor windings ($/m)
  real(kind(1.0D0)) :: ucwindtf = 480.0D0
  !+ad_vars  ucws /460.0/ FIX : cost of active assembly shop ($/m3)
  real(kind(1.0D0)) :: ucws = 460.0D0
  !+ad_vars  ucwst(4) /0.0,3.94,5.91,7.88/ : cost of waste disposal (M$/y/1200MW)
  real(kind(1.0D0)), dimension(4) :: ucwst = &
       (/0.0D0, 3.94D0, 5.91D0, 7.88D0/)

end module cost_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module constraint_variables

  !+ad_name  constraint_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  constraint equations
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  constraint equations (f-values, limits, etc.).
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  31/10/12 PJK Initial version of module
  !+ad_hist  19/06/13 PJK Removed fjtfc
  !+ad_hist  27/06/13 PJK Relabelled ftohs, tohsmn, fbetatry
  !+ad_hist  25/09/13 PJK Changed fportsz description
  !+ad_hist  30/09/13 PJK Added pseprmax, fpsepr
  !+ad_hist  28/10/13 PJK Corrected fdene comment
  !+ad_hist  26/02/14 PJK Added ftfthko
  !+ad_hist  08/05/14 PJK Added bigqmin
  !+ad_hist  19/05/14 PJK Added fradpwr
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  01/10/14 PJK Added flhthresh
  !+ad_hist  02/10/14 PJK Added fcwr
  !+ad_hist  06/10/14 PJK Added fnbshinef, nbshinefmax
  !+ad_hist  11/11/14 PJK Added ftmargoh
  !+ad_hist  06/08/15 MDK ftaulimit
  !+ad_hist  18/11/15 RK  fzeffmax and zeffmax
  !+ad_hist  23/06/16 JM  Removed dtmpmx as no longer used anywhere
  !+ad_hist  09/11/16 HL  Added fradwall, maxradwalload, peakfactrad and peakradwalload
  !+ad_hist  19/01/17 JM  Added variables for constraint equation for psepbqar (68)
  !+ad_hist  12/01/18 KE  Added fnesep f-value for Eich critical separatrix density
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  auxmin /0.1/ : minimum auxiliary power (MW)
  !+ad_varc                 (constraint equation 40)
  real(kind(1.0D0)) :: auxmin = 0.1D0
  !+ad_vars  betpmx /0.19/ : maximum poloidal beta
  !+ad_varc                  (constraint equation 48)
  real(kind(1.0D0)) :: betpmx = 0.19D0
  !+ad_vars  bigqmin /10.0/ : minimum fusion gain Q
  !+ad_varc                  (constraint equation 28)
  real(kind(1.0D0)) :: bigqmin = 10.0D0
  !+ad_vars  bmxlim /12.0/ : maximum peak toroidal field (T)
  !+ad_varc                  (constraint equation 25)
  real(kind(1.0D0)) :: bmxlim = 12.0D0
  !+ad_vars  fauxmn /1.0/ : f-value for minimum auxiliary power
  !+ad_varc                 (constraint equation 40, iteration variable 64)
  real(kind(1.0D0)) :: fauxmn = 1.0D0
  !+ad_vars  fbeta /1.0/ : f-value for epsilon beta-poloidal
  !+ad_varc                (constraint equation 6, iteration variable 8)
  real(kind(1.0D0)) :: fbeta = 1.0D0
  !+ad_vars  fbetap /1.0/ : f-value for poloidal beta
  !+ad_varc                 (constraint equation 48, iteration variable 79)
  real(kind(1.0D0)) :: fbetap = 1.0D0
  !+ad_vars  fbetatry /1.0/ : f-value for beta limit
  !+ad_varc                   (constraint equation 24, iteration variable 36)
  real(kind(1.0D0)) :: fbetatry = 1.0D0
  !+ad_vars  fcpttf /1.0/ : f-value for TF coil current per turn upper limit
  !+ad_varc               (constraint equation 77, iteration variable 146)
  real(kind(1.0D0)) :: fcpttf = 1.0D0
  !+ad_vars  fcwr /1.0/ : f-value for conducting wall radius / rminor limit
  !+ad_varc               (constraint equation 23, iteration variable 104)
  real(kind(1.0D0)) :: fcwr = 1.0D0
  !+ad_vars  fdene /1.0/ : f-value for density limit
  !+ad_varc                (constraint equation 5, iteration variable 9)
  !+ad_varc                (invalid if ipedestal = 3)
  real(kind(1.0D0)) :: fdene = 1.0D0
  !+ad_vars  fdivcol /1.0/ : f-value for divertor collisionality
  !+ad_varc                  (constraint equation 22, iteration variable 34)
  real(kind(1.0D0)) :: fdivcol = 1.0D0
  !+ad_vars  fdtmp /1.0/ : f-value for first wall coolant temperature rise
  !+ad_varc                (constraint equation 38, iteration variable 62)
  real(kind(1.0D0)) :: fdtmp = 1.0D0
  !+ad_vars  fflutf /1.0/ : f-value for neutron fluence on TF coil
  !+ad_varc                  (constraint equation 53, iteration variable 92)
  real(kind(1.0D0)) :: fflutf = 1.0D0
  !+ad_vars  ffuspow /1.0/ : f-value for maximum fusion power
  !+ad_varc                  (constraint equation 9, iteration variable 26)
  real(kind(1.0D0)) :: ffuspow = 1.0D0
  !+ad_vars  fgamcd /1.0/ : f-value for current drive gamma
  !+ad_varc                 (constraint equation 37, iteration variable 40)
  real(kind(1.0D0)) :: fgamcd = 1.0D0
  !+ad_vars  fhldiv /1.0/ : f-value for divertor heat load
  !+ad_varc                 (constraint equation 18, iteration variable 27)
  real(kind(1.0D0)) :: fhldiv = 1.0D0
  !+ad_vars  fiooic /0.5/ : f-value for TF coil operating current / critical
  !+ad_varc                 current ratio
  !+ad_varc                 (constraint equation 33, iteration variable 50)
  real(kind(1.0D0)) :: fiooic = 0.5D0
  !+ad_vars  fipir /1.0/ : f-value for Ip/Irod limit
  !+ad_varc                (constraint equation 46, iteration variable 72)
  real(kind(1.0D0)) :: fipir = 1.0D0
  !+ad_vars  fjohc /1.0/ : f-value for central solenoid current at end-of-flattop
  !+ad_varc                (constraint equation 26, iteration variable 38)
  real(kind(1.0D0)) :: fjohc = 1.0D0
  !+ad_vars  fjohc0 /1.0/ : f-value for central solenoid current at beginning of pulse
  !+ad_varc                 (constraint equation 27, iteration variable 39)
  real(kind(1.0D0)) :: fjohc0 = 1.0D0
  !+ad_vars  fjprot /1.0/ : f-value for TF coil winding pack current density
  !+ad_varc                 (constraint equation 35, iteration variable 53)
  real(kind(1.0D0)) :: fjprot = 1.0D0
  !+ad_vars  flhthresh /1.0/ : f-value for L-H power threshold
  !+ad_varc                    (constraint equation 15, iteration variable 103)
  real(kind(1.0D0)) :: flhthresh = 1.0D0
  !+ad_vars  fmva /1.0/ : f-value for maximum MVA
  !+ad_varc               (constraint equation 19, iteration variable 30)
  real(kind(1.0D0)) :: fmva = 1.0D0
  !+ad_vars  fnbshinef /1.0/ : f-value for maximum neutral beam shine-through fraction
  !+ad_varc                    (constraint equation 59, iteration variable 105)
  real(kind(1.0D0)) :: fnbshinef = 1.0D0
  !+ad_vars  fnesep /1.0/ : f-value for Eich critical separatrix density
  !+ad_varc                    (constraint equation 76, iteration variable 144)
  real(kind(1.0D0)) :: fnesep = 1.0D0
  !+ad_vars  foh_stress /1.0/ : f-value for Tresca stress in Central Solenoid
  !+ad_varc                    (constraint equation 72, iteration variable 123)
  real(kind(1.0D0)) :: foh_stress = 1.0D0
  !+ad_vars  fpeakb /1.0/ : f-value for maximum toroidal field
  !+ad_varc                 (constraint equation 25, iteration variable 35)
  real(kind(1.0D0)) :: fpeakb = 1.0D0
  !+ad_vars  fpinj /1.0/ : f-value for injection power
  !+ad_varc                (constraint equation 30, iteration variable 46)
  real(kind(1.0D0)) :: fpinj = 1.0D0
  !+ad_vars  fpnetel /1.0/ : f-value for net electric power
  !+ad_varc                  (constraint equation 16, iteration variable 25)
  real(kind(1.0D0)) :: fpnetel = 1.0D0
  !+ad_vars  fportsz /1.0/ : f-value for neutral beam tangency radius limit
  !+ad_varc                  (constraint equation 20, iteration variable 33)
  real(kind(1.0D0)) :: fportsz = 1.0D0
  !+ad_vars  fpsepbqar /1.0/ : f-value for maximum Psep*Bt/qAR limit
  !+ad_varc                 (constraint equation 68, iteration variable 117)
  real(kind(1.0D0)) :: fpsepbqar = 1.0D0
  !+ad_vars  fpsepr /1.0/ : f-value for maximum Psep/R limit
  !+ad_varc                 (constraint equation 56, iteration variable 97)
  real(kind(1.0D0)) :: fpsepr = 1.0D0
  !+ad_vars  fptemp /1.0/ : f-value for peak centrepost temperature
  !+ad_varc                 (constraint equation 44, iteration variable 68)
  real(kind(1.0D0)) :: fptemp = 1.0D0
  !+ad_vars  fptfnuc /1.0/ : f-value for maximum TF coil nuclear heating
  !+ad_varc                  (constraint equation 54, iteration variable 95)
  real(kind(1.0D0)) :: fptfnuc = 1.0D0
  !+ad_vars  fq /1.0/ : f-value for edge safety factor
  !+ad_varc             (constraint equation 45, iteration variable 71)
  real(kind(1.0D0)) :: fq = 1.0D0
  !+ad_vars  fqval /1.0/ : f-value for Q
  !+ad_varc                (constraint equation 28, iteration variable 45)
  real(kind(1.0D0)) :: fqval = 1.0D0
  !+ad_vars  fradpwr /1.0/ : f-value for core radiation power limit
  !+ad_varc                  (constraint equation 17, iteration variable 28)
  real(kind(1.0D0)) :: fradpwr = 1.0D0
  !+ad_vars  fradwall /1.0/ : f-value for upper limit on radiation wall load
  !+ad_varc                   (constr. equ. 67, iteration variable 116 )
  real(kind(1.0D0)) :: fradwall = 1.0D0
  !+ad_vars  freinke /1.0/ : f-value for Reinke detachment criterion
  !+ad_varc                   (constr. equ. 78, iteration variable 147)
  real(kind(1.0D0)) :: freinke = 1.0D0
  !+ad_vars  frminor /1.0/ : f-value for minor radius limit
  !+ad_varc                  (constraint equation 21, iteration variable 32)
  real(kind(1.0D0)) :: frminor = 1.0D0
  !+ad_vars  fstrcase /1.0/ : f-value for TF coil case stress
  !+ad_varc                   (constraint equation 31, iteration variable 48)
  real(kind(1.0D0)) :: fstrcase = 1.0D0
  !+ad_vars  fstrcond /1.0/ : f-value for TF coil conduit stress
  !+ad_varc                   (constraint equation 32, iteration variable 49)
  real(kind(1.0D0)) :: fstrcond = 1.0D0
  !+ad_vars  ftaucq /1.0/ : f-value for calculated minimum TF quench time
  !+ad_varc                 (constraint equation 65, iteration variable 113)
  real(kind(1.0D0)) :: ftaucq = 1.0D0
  !+ad_vars  ftbr /1.0/ : f-value for minimum tritium breeding ratio
  !+ad_varc                 (constraint equation 52, iteration variable 89)
  real(kind(1.0D0)) :: ftbr = 1.0D0
  !+ad_vars  ftburn /1.0/ : f-value for minimum burn time
  !+ad_varc                 (constraint equation 13, iteration variable 21)
  real(kind(1.0D0)) :: ftburn = 1.0D0
  !+ad_vars  ftcycl /1.0/ : f-value for cycle time
  !+ad_varc                 (constraint equation 42, iteration variable 67)
  real(kind(1.0D0)) :: ftcycl = 1.0D0
  !+ad_vars  ftmargoh /1.0/ : f-value for central solenoid temperature margin
  !+ad_varc                   (constraint equation 60, iteration variable 106)
  real(kind(1.0D0)) :: ftmargoh = 1.0D0
  !+ad_vars  ftmargtf /1.0/ : f-value for TF coil temperature margin
  !+ad_varc                   (constraint equation 36, iteration variable 54)
  real(kind(1.0D0)) :: ftmargtf = 1.0D0
  !+ad_vars  ftohs /1.0/ : f-value for plasma current ramp-up time
  !+ad_varc                (constraint equation 41, iteration variable 66)
  real(kind(1.0D0)) :: ftohs = 1.0D0
  !+ad_vars  ftpeak /1.0/ : f-value for first wall peak temperature
  !+ad_varc                 (constraint equation 39, iteration variable 63)
  real(kind(1.0D0)) :: ftpeak = 1.0D0
  !+ad_vars  fvdump /1.0/ : f-value for dump voltage
  !+ad_varc                 (constraint equation 34, iteration variable 51)
  real(kind(1.0D0)) :: fvdump = 1.0D0
  !+ad_vars  fvs /1.0/ : f-value for flux-swing (V-s) requirement (STEADY STATE)
  !+ad_varc              (constraint equation 12, iteration variable 15)
  real(kind(1.0D0)) :: fvs = 1.0D0
  !+ad_vars  fvvhe /1.0/ : f-value for vacuum vessel He concentration limit
  !+ad_varc                (iblanket = 2)
  !+ad_varc                (constraint equation 55, iteration variable 96)
  real(kind(1.0D0)) :: fvvhe = 1.0D0
  !+ad_vars  fwalld /1.0/ : f-value for maximum wall load
  !+ad_varc                 (constraint equation 8, iteration variable 14)
  real(kind(1.0D0)) :: fwalld = 1.0D0
  !+ad_vars  fzeffmax /1.0/ : f-value for maximum zeff
  !+ad_varc                 (constraint equation 64, iteration variable 112)
  real(kind(1.0D0)) :: fzeffmax = 1.0D0
  !+ad_vars  gammax /2.0/ : maximum current drive gamma
  !+ad_varc                 (constraint equation 37)
  real(kind(1.0D0)) :: gammax = 2.0D0
  !+ad_vars maxradwallload /1.0/ :  Maximum permitted radiation wall load (MW/m^2)
  !+ad_varc                         (constraint equation 67)
  real(kind(1.0D0)) :: maxradwallload = 1.0D0
  !+ad_vars  mvalim /40.0/ : maximum MVA limit
  !+ad_varc                  (constraint equation 19)
  real(kind(1.0D0)) :: mvalim = 40.0D0
  !+ad_vars  nbshinefmax /1.0e-3/ : maximum neutral beam shine-through fraction
  !+ad_varc                         (constraint equation 59)
  real(kind(1.0D0)) :: nbshinefmax = 1.0D-3
  !+ad_vars  nflutfmax /1.0e23/ : max fast neutron fluence on TF coil (n/m2)
  !+ad_varc                      (blktmodel>0)
  !+ad_varc                      (constraint equation 53)
  real(kind(1.0D0)) :: nflutfmax = 1.0D23
  !+ad_vars  pdivtlim /150.0/  : Minimum pdivt [MW] (constraint equation 80)
  real(kind(1.0D0)) :: pdivtlim = 150.0D0
  !+ad_vars  peakfactrad /3.33/  : peaking factor for radiation wall load
  !+ad_varc                       (constraint equation 67)
  real(kind(1.0D0)) :: peakfactrad = 3.33D0
  !+ad_vars  peakradwallload : Peak radiation wall load (MW/m^2)
  !+ad_varc                        (constraint equation 67)
  real(kind(1.0D0)) :: peakradwallload = 0.0D0
  !+ad_vars  pnetelin /1000.0/ : required net electric power (MW)
  !+ad_varc                      (constraint equation 16)
  real(kind(1.0D0)) :: pnetelin = 1.0D3
  !+ad_vars  powfmax /1500.0/ : maximum fusion power (MW)
  !+ad_varc                     (constraint equation 9)
  real(kind(1.0D0)) :: powfmax = 1.5D3
  !+ad_vars  psepbqarmax /9.5/ : maximum ratio of Psep*Bt/qAR (MWT/m)
  !+ad_varc                      (constraint equation 68)
  real(kind(1.0D0)) :: psepbqarmax = 9.5D0
  !+ad_vars  pseprmax /25.0/ : maximum ratio of power crossing the separatrix to
  !+ad_varc                      plasma major radius (Psep/R) (MW/m)
  !+ad_varc                      (constraint equation 56)
  real(kind(1.0D0)) :: pseprmax = 25.0D0
  !+ad_vars  ptfnucmax /1.0e-3/ : maximum nuclear heating in TF coil (MW/m3)
  !+ad_varc                       (constraint equation 54)
  real(kind(1.0D0)) :: ptfnucmax = 1.0D-3
  !+ad_vars  tbrmin /1.1/ : minimum tritium breeding ratio
  !+ad_varc                 (constraint equation 52)
  real(kind(1.0D0)) :: tbrmin = 1.1D0
  !+ad_vars  tbrnmn /1.0/ : minimum burn time (s)
  !+ad_varc                 (KE - no longer itv., see issue 706)
  real(kind(1.0D0)) :: tbrnmn = 1.0D0
  !+ad_vars  tcycmn : minimum cycle time (s)
  !+ad_varc           (constraint equation 42)
  real(kind(1.0D0)) :: tcycmn = 0.0D0
  !+ad_vars  tohsmn : minimum plasma current ramp-up time (s)
  !+ad_varc           (constraint equation 41)
  real(kind(1.0D0)) :: tohsmn = 1.0D0
  !+ad_vars  vvhealw /1.0/ : allowed maximum helium concentration in vacuum vessel
  !+ad_varc                  at end of plant life (appm) (iblanket =2)
  !+ad_varc                  (constraint equation 55)
  real(kind(1.0D0)) :: vvhealw = 1.0D0
  !+ad_vars  walalw /1.0/ : allowable wall-load (MW/m2)
  !+ad_varc                 (constraint equation 8)
  real(kind(1.0D0)) :: walalw = 1.0D0

  !+ad_vars  taulimit /5.0/ : Lower limit on taup/taueff the ratio of alpha particle
  !+ad_varc                  to energy confinement times (constraint equation 62)
  real(kind(1.0D0)) :: taulimit = 5.0D0
  !+ad_vars  ftaulimit /1.0/ : f-value for lower limit on taup/taueff the ratio
  !+ad_varc                    of alpha particle to energy confinement times
  !+ad_varc                   (constraint equation 62, iteration variable 110)
  real(kind(1.0D0)) :: ftaulimit = 1.0D0

  !+ad_vars  fniterpump /1.0/ : f-value for constraint that number of pumps < tfno
  !+ad_varc                   (constraint equation 63, iteration variable 111)
  real(kind(1.0D0)) :: fniterpump = 1.0D0
  !+ad_vars  zeffmax /3.6/ : maximum value for Zeff
  !+ad_varc                   (constraint equation 64)
  real(kind(1.0D0)) :: zeffmax = 3.6D0
  !+ad_vars  fpoloidalpower /1.0/ : f-value for constraint on rate of change of energy in poloidal field
  !+ad_varc                   (constraint equation 66, iteration variable 115)
  real(kind(1.0D0)) :: fpoloidalpower = 1.0D0

  !+ad_vars  fpsep /1.0/ : f-value to ensure separatrix power is less than value from Kallenbach divertor
  !+ad_varc                   (Not required as constraint 69 is an equality)
  real(kind(1.0D0)) :: fpsep = 1.0D0

  !+ad_vars  fcqt /1.0/ : f-value: TF coil quench temparature remains below tmax_croco
  !+ad_varc                   (constraint equation 74, iteration variable 141)
  real(kind(1.0D0)) :: fcqt = 1.0D0


end module constraint_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module stellarator_variables

  !+ad_name  stellarator_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  stellarator model
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  F Warmer, IPP Greifswald
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  stellarator model.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  31/10/12 PJK Initial version of module
  !+ad_hist  23/01/13 PJK Added iotabar
  !+ad_hist  14/08/13 PJK/FW Added stellarator divertor variables
  !+ad_hist  05/03/14 PJK Added fdivwet and port size variables
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_stat  Okay
  !+ad_docs  Stellarator Plasma Geometry Model for the Systems
  !+ad_docc  Code PROCESS, F. Warmer, 19/06/2013
  !+ad_docs  Stellarator Divertor Model for the Systems
  !+ad_docc  Code PROCESS, F. Warmer, 21/06/2013
  !+ad_docs  Stellarator Coil Model for the Systems
  !+ad_docc  Code PROCESS, F. Warmer and F. Schauer, 07/10/2013
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  istell /0/ : switch for stellarator option
  !+ad_varc               (set via <CODE>device.dat</CODE>):<UL>
  !+ad_varc          <LI> = 0 use tokamak model;
  !+ad_varc          <LI> = 1 use stellarator model</UL>
  integer :: istell = 0

  !+ad_vars  bmn /0.001/ : relative radial field perturbation
  real(kind(1.0D0)) :: bmn = 1.0D-3
  !+ad_vars  f_asym /1.0/ : divertor heat load peaking factor
  real(kind(1.0D0)) :: f_asym = 1.0D0
  !+ad_vars  f_rad /0.85/ : radiated power fraction in SOL
  real(kind(1.0D0)) :: f_rad = 0.85D0
  !+ad_vars  f_w /0.5/ : island size fraction factor
  real(kind(1.0D0)) :: f_w = 0.5D0
  !+ad_vars  fdivwet /0.3333/ : wetted fraction of the divertor area
  real(kind(1.0D0)) :: fdivwet = 0.33333333333333333333333333333D0
  !+ad_vars  flpitch /0.001/ : field line pitch (rad)
  real(kind(1.0D0)) :: flpitch = 1.0D-3
  !+ad_vars  hportamax : maximum available area for horizontal ports (m2)
  real(kind(1.0D0)) :: hportamax = 0.0D0
  !+ad_vars  hportpmax : maximum available poloidal extent for horizontal ports (m)
  real(kind(1.0D0)) :: hportpmax = 0.0D0
  !+ad_vars  hporttmax : maximum available toroidal extent for horizontal ports (m)
  real(kind(1.0D0)) :: hporttmax = 0.0D0
  !+ad_vars  iotabar /1.0/ : rotational transform (reciprocal of tokamak q)
  !+ad_varc                  for stellarator confinement time scaling laws
  real(kind(1.0D0)) :: iotabar = 1.0D0
  !+ad_vars  isthtr /3/ : switch for stellarator auxiliary heating method:<UL>
  !+ad_varc          <LI> = 1 electron cyclotron resonance heating;
  !+ad_varc          <LI> = 2 lower hybrid heating;
  !+ad_varc          <LI> = 3 neutral beam injection</UL>
  integer :: isthtr = 3
  !+ad_vars  m_res /5/ : poloidal resonance number
  integer :: m_res = 5
  !+ad_vars  n_res /5/ : toroidal resonance number
  integer :: n_res = 5
  !+ad_vars  shear /0.5/ : magnetic shear, derivative of iotabar
  real(kind(1.0D0)) :: shear = 0.5D0
  !+ad_vars  vmec_info_file /vmec_info.dat/ : file containing general VMEC settings
  character(len=48) :: vmec_info_file = 'vmec_info.dat'
  !+ad_vars  vmec_rmn_file /vmec_Rmn.dat/ : file containing plasma boundary R(m,n)
  !+ad_varc                                 Fourier components
  character(len=48) :: vmec_rmn_file = 'vmec_Rmn.dat'
  !+ad_vars  vmec_zmn_file /vmec_Zmn.dat/ : file containing plasma boundary Z(m,n)
  !+ad_varc                                 Fourier components
  character(len=48) :: vmec_zmn_file = 'vmec_Zmn.dat'
  !+ad_vars  vportamax : maximum available area for vertical ports (m2)
  real(kind(1.0D0)) :: vportamax = 0.0D0
  !+ad_vars  vportpmax : maximum available poloidal extent for vertical ports (m)
  real(kind(1.0D0)) :: vportpmax = 0.0D0
  !+ad_vars  vporttmax : maximum available toroidal extent for vertical ports (m)
  real(kind(1.0D0)) :: vporttmax = 0.0D0

end module stellarator_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Issue #508 Remove RFP option: module rfp_variables
! Issue #508 Remove IFE option: module ife_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module pulse_variables

  !+ad_name  pulse_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  pulsed reactor model
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  pulsed reactor model.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  05/11/12 PJK Initial version of module
  !+ad_hist  21/08/14 PJK Moved some variables into fwbs_variables
  !+ad_hist  23/06/16 JM  Removed tmprse as no longer used anywhere
  !+ad_stat  Okay
  !+ad_docs  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  bctmp /320.0/ : first wall bulk coolant temperature (C)
  real(kind(1.0D0)) :: bctmp = 320.0D0
  !+ad_vars  bfw : outer radius of each first wall structural tube (m)
  !+ad_varc        (0.5 * average of fwith and fwoth)
  real(kind(1.0D0)) :: bfw = 0.0D0
  !+ad_vars  dtstor /300.0/ : maximum allowable temperature change in stainless
  !+ad_varc                   steel thermal storage block (K) (istore=3)
  real(kind(1.0D0)) :: dtstor = 300.0D0
  !+ad_vars  istore /1/ : switch for thermal storage method:<UL>
  !+ad_varc          <LI> = 1 option 1 of Electrowatt report, AEA FUS 205;
  !+ad_varc          <LI> = 2 option 2 of Electrowatt report, AEA FUS 205;
  !+ad_varc          <LI> = 3 stainless steel block</UL>
  integer :: istore = 1
  !+ad_vars  itcycl /1/ : switch for first wall axial stress model:<UL>
  !+ad_varc          <LI> = 1 total axial constraint, no bending;
  !+ad_varc          <LI> = 2 no axial constraint, no bending;
  !+ad_varc          <LI> = 3 no axial constraint, bending</UL>
  integer :: itcycl = 1
  !+ad_vars  lpulse /0/ : switch for reactor model:<UL>
  !+ad_varc          <LI> = 0 continuous operation;
  !+ad_varc          <LI> = 1 pulsed operation</UL>
  integer :: lpulse = 0

end module pulse_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module startup_variables

  !+ad_name  startup_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  plasma start-up model
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  plasma start-up model.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  05/11/12 PJK Initial version of module
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_stat  Okay
  !+ad_docs  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  ftaue : factor in energy confinement time formula
  real(kind(1.0D0)) :: ftaue = 0.0D0
  !+ad_vars  gtaue : offset term in energy confinement time scaling
  real(kind(1.0D0)) :: gtaue  = 0.0D0
  !+ad_vars  nign : electron density at ignition (start-up) (/m3)
  real(kind(1.0D0)) :: nign  = 0.0D0
  !+ad_vars  ptaue : exponent for density term in energy confinement time formula
  real(kind(1.0D0)) :: ptaue  = 0.0D0
  !+ad_vars  qtaue : exponent for temperature term in energy confinement time formula
  real(kind(1.0D0)) :: qtaue  = 0.0D0
  !+ad_vars  rtaue : exponent for power term in energy confinement time formula
  real(kind(1.0D0)) :: rtaue  = 0.0D0
  !+ad_vars  tign : electron temperature at ignition (start-up) (keV)
  real(kind(1.0D0)) :: tign  = 0.0D0

end module startup_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module fispact_variables

  !+ad_name  fispact_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  fispact routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  nuclear data (fispact) routines.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  06/11/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  Fispact arrays with 3 elements contain the results at the following times:
  !+ad_varc    (1) - at end of component life
  !+ad_varc    (2) - after 3 months cooling time
  !+ad_varc    (3) - 100 years after end of plant life

  !+ad_vars  bliact(3) : inboard blanket total activity (Bq)
  real(kind(1.0D0)), dimension(3) :: bliact = 0.0D0
  !+ad_vars  bligdr(3) : inboard blanket total gamma dose rate (Sv/hr)
  real(kind(1.0D0)), dimension(3) :: bligdr = 0.0D0
  !+ad_vars  blihkw(3) : inboard blanket total heat output (kW)
  real(kind(1.0D0)), dimension(3) :: blihkw = 0.0D0
  !+ad_vars  bliizp : inboard blanket integrated zone power / neutron
  real(kind(1.0D0)) :: bliizp = 0.0D0
  !+ad_vars  blimzp : inboard blanket mean zone power density / neutron
  real(kind(1.0D0)) :: blimzp = 0.0D0
  !+ad_vars  bloact(3) : outboard blanket total activity (Bq)
  real(kind(1.0D0)), dimension(3) :: bloact = 0.0D0
  !+ad_vars  blogdr(3) : outboard blanket total gamma dose rate (Sv/hr)
  real(kind(1.0D0)), dimension(3) :: blogdr = 0.0D0
  !+ad_vars  blohkw(3) : outboard blanket total heat output (kW)
  real(kind(1.0D0)), dimension(3) :: blohkw = 0.0D0
  !+ad_vars  bloizp : outboard blanket integrated zone power / neutron
  real(kind(1.0D0)) :: bloizp = 0.0D0
  !+ad_vars  blomzp : outboard blanket mean zone power density / neutron
  real(kind(1.0D0)) :: blomzp = 0.0D0
  !+ad_vars  fwiact(3) : inboard first wall total activity (Bq)
  real(kind(1.0D0)), dimension(3) :: fwiact = 0.0D0
  !+ad_vars  fwigdr(3) : inboard first wall total gamma dose rate (Sv/hr)
  real(kind(1.0D0)), dimension(3) :: fwigdr = 0.0D0
  !+ad_vars  fwihkw(3) : inboard first wall total heat output (kW)
  real(kind(1.0D0)), dimension(3) :: fwihkw = 0.0D0
  !+ad_vars  fwiizp : inboard first wall integrated zone power / neutron
  real(kind(1.0D0)) :: fwiizp = 0.0D0
  !+ad_vars  fwimzp : inboard first wall mean zone power density/neutron
  real(kind(1.0D0)) :: fwimzp = 0.0D0
  !+ad_vars  fwoact(3) : outboard first wall total activity (Bq)
  real(kind(1.0D0)), dimension(3) :: fwoact = 0.0D0
  !+ad_vars  fwogdr(3) : outboard first wall total gamma dose rate (Sv/hr)
  real(kind(1.0D0)), dimension(3) :: fwogdr = 0.0D0
  !+ad_vars  fwohkw(3) : outboard first wall total heat output (kW)
  real(kind(1.0D0)), dimension(3) :: fwohkw = 0.0D0
  !+ad_vars  fwoizp : outboard first wall integrated zone power / neutron
  real(kind(1.0D0)) :: fwoizp = 0.0D0
  !+ad_vars  fwomzp : outboard first wall mean zone power density/neutron
  real(kind(1.0D0)) :: fwomzp = 0.0D0
  !+ad_vars  fwtemp : outboard first wall temperature after a LOCA (K)
  real(kind(1.0D0)) :: fwtemp = 0.0D0

end module fispact_variables

!------------------------------------------------------------------------

module rebco_variables

  !+ad_name  rebco_variables
  !+ad_summ  Variables relating to the REBCO HTS tape, strand and conductor
  !+ad_summ  Conduit information is in the modules relating to each coil.
  !+ad_type  Module
  !+ad_docs  TODO
  implicit none ! ---------------------------------------------------------
  ! Updated 13/11/18 using data from Lewandowska et al 2018.

  !+ad_vars  rebco_thickness /1.0e-6/ : thickness of REBCO layer in tape (m) (iteration variable 138)
  real(kind(1.0D0)) :: rebco_thickness = 1.0D-6
  !+ad_vars  copper_thick /100e-6/ : thickness of copper layer in tape (m) (iteration variable 139)
  real(kind(1.0D0)) :: copper_thick = 100.0D-6
  !+ad_vars  hastelloy_thickness /50/e-6 : thickness of Hastelloy layer in tape (m)
  real(kind(1.0D0)) :: hastelloy_thickness = 50.0D-6
  !+ad_vars  tape_width : Mean width of tape (m)
  real(kind(1.0D0)) :: tape_width = 0.0D0

  !+ad_vars  croco_od : Outer diameter of CroCo strand (m)
  real(kind(1.0D0)) :: croco_od = 0.0D0
  !+ad_vars  croco_id : Inner diameter of CroCo copper tube (m)
  real(kind(1.0D0)) :: croco_id = 0.0D0
  !+ad_vars  croco_thick /2.5e-3/ : Thickness of CroCo copper tube (m) (iteration variable 149)
  real(kind(1.0D0)) :: croco_thick = 2.5D-3

  !!+ad_vars  copper_bar /1.0/ : area of central copper bar, as a fraction of the cable space
  !real(kind(1.0D0)) :: copper_bar = 0.23d0
  !+ad_vars  copper_rrr /100.0/ : residual resistivity ratio copper in TF superconducting cable
  real(kind(1.0D0)) :: copper_rrr = 100d0

  !!+ad_vars  cable_helium_fraction /0.284/ : Helium area as a fraction of the cable space.
  !real(kind(1.0D0)) :: cable_helium_fraction = 0.284D0

  !+ad_vars  copperA_m2_max /1e8/ : Maximum TF coil current / copper area (A/m2)
  real(kind(1.0D0)) :: copperA_m2_max = 1D8
  !+ad_vars  f_copperA_m2 /1/ : f-value for constraint 75: TF coil current / copper area < copperA_m2_max
  real(kind(1.0D0)) :: f_copperA_m2 = 1d0


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

  !+ad_name  resistive_material
  !+ad_summ  Variables relating to resistive materials in superconducting conductors
  !+ad_type  Module
  !+ad_docs  TODO
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
  !+ad_name  reinke_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  Reinke Criterion
  !+ad_type  Module
  !+ad_auth  H Lux, CCFE/UKAEA, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  minimum impurity fraction for detached divertor conditions
  !+ad_desc  Reinke criterion. It furthermore uses several parameters from
  !+ad_desc  Kallenbach model like netau and empurity_enrichment.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  22/05/18 HL Initial version of module
  !+ad_stat  Okay
  !+ad_docs  M.L. Reinke 2017 Nucl. Fusion 57 034004
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public


  !+ad_vars  impvardiv /9/ : index of impurity to be iterated for
  !+ad_varc           Reinke divertor detachment criterion
  integer       :: impvardiv = 9

  !+ad_vars  lhat /4.33/ : connection length factor L|| = lhat qstar R
  !+ad_varc                for Reinke criterion, default value from
  !+ad_varc                Post et al. 1995 J. Nucl. Mat.  220-2 1014
  real(kind(1.0D0)) :: lhat = 4.33D0

  !+ad_vars  fzmin : Minimum impurity fraction necessary for detachment
  !+ad_varc          This is the impurity at the SOL/Div
  real(kind(1.0D0)) :: fzmin = 0.0D0

  !+ad_vars  fzactual : Actual impurity fraction of divertor impurity
  !+ad_varc             (impvardiv) in the SoL (taking impurity_enrichment
  !+ad_varc             into account) (iteration variable 148)
  real(kind(1.0D0)) :: fzactual = 0.001D0

  !+ad_vars  reinke_mode /0/ : Switch for Reinke criterion H/I mode
  !+ad_varc          <LI> = 0 H-mode;
  !+ad_varc          <LI> = 1 I-mode;</UL>
  integer       :: reinke_mode = 0

end module reinke_variables

  !------------------------------------------------------------------------
