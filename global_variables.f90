!  $Id::                                                                $
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  icase : string : description of run or PROCESS version number
  character(len=48) :: icase = 'PROCESS standard D-T tokamak model'

end module global_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module physics_variables

  !+ad_name  physics_variables
  !+ad_summ  Module containing global variables relating to the plasma physics
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the plasma
  !+ad_desc  physics. It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>phydat.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  15/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  ipnlaws : number of energy confinement time scaling laws
  integer, parameter :: ipnlaws = 36

  !+ad_vars  tauscl(ipnlaws) : labels describing energy confinement scaling laws
  character(len=24), dimension(ipnlaws) :: tauscl = (/ &
       'Neo-Alcator      (ohmic)', &  !  1
       'Mirnov               (H)', &  !  2
       'Merezkhin-Muhkovatov (L)', &  !  3
       'Shimomura            (H)', &  !  4
       'Kaye-Goldston        (L)', &  !  5
       'ITER 89-P            (L)', &  !  6
       'ITER 89-O            (L)', &  !  7
       'Rebut-Lallia         (L)', &  !  8
       'Goldston             (L)', &  !  9
       'T10                     ', &  !  10
       'JAERI-88                ', &  !  11
       'Kaye-Big Complex        ', &  !  12
       'ITER H90-P           (H)', &  !  13
       'ITER Mix                ', &  !  14
       'Riedel               (L)', &  !  15
       'Christiansen         (L)', &  !  16
       'Lackner-Gottardi     (L)', &  !  17
       'Neo-Kaye             (L)', &  !  18
       'Riedel               (H)', &  !  19
       'ITER H90-P amended   (H)', &  !  20
       'LHD              (stell)', &  !  21
       'Gyro-reduced Bohm(stell)', &  !  22
       'Lackner-Gottardi (stell)', &  !  23
       'ITER-93H             (H)', &  !  24
       'TITAN RFP               ', &  !  25
       'ITER H-97P ELM-free  (H)', &  !  26
       'ITER H-97P ELMy      (H)', &  !  27
       'ITER-96P             (L)', &  !  28
       'Valovic modified ELMy(H)', &  !  29
       'Kaye PPPL April 98   (L)', &  !  30
       'ITERH-PB98P(y)       (H)', &  !  31
       'IPB98(y)             (H)', &  !  32
       'IPB98(y,1)           (H)', &  !  33
       'IPB98(y,2)           (H)', &  !  34
       'IPB98(y,3)           (H)', &  !  35
       'IPB98(y,4)           (H)' /)  !  36

  !+ad_vars  abeam : beam ion mass (amu)
  real(kind(1.0D0)) :: abeam = 0.0D0
  !+ad_vars  afuel : average mass of fuel portion of ions (amu)
  real(kind(1.0D0)) :: afuel = 0.0D0
  !+ad_vars  aion : average mass of all ions (amu)
  real(kind(1.0D0)) :: aion = 0.0D0
  !+ad_vars  alphaj /1.0/ : current profile index
  real(kind(1.0D0)) :: alphaj = 1.0D0
  !+ad_vars  alphan /0.5/ : density profile index
  real(kind(1.0D0)) :: alphan = 0.5D0
  !+ad_vars  alphat /1.0/ : temperature profile index
  real(kind(1.0D0)) :: alphat = 1.0D0
  !+ad_vars  alpmw : alpha power (MW)
  real(kind(1.0D0)) :: alpmw = 0.0D0
  !+ad_vars  aspect /3.5/ : aspect ratio (iteration variable 1)
  real(kind(1.0D0)) :: aspect = 3.5D0
  !+ad_vars  beamfus0 /1.0/ : multiplier for beam-background fusion calculation
  real(kind(1.0D0)) :: beamfus0 = 1.0D0
  !+ad_vars  beta /0.042/ : total plasma beta (iteration variable 5)
  real(kind(1.0D0)) :: beta = 0.042D0
  !+ad_vars  betaft : fast alpha beta component
  real(kind(1.0D0)) :: betaft = 0.0D0
  !+ad_vars  betalim : allowable beta
  real(kind(1.0D0)) :: betalim = 0.0D0
  !+ad_vars  betanb : neutral beam beta component
  real(kind(1.0D0)) :: betanb = 0.0D0
  !+ad_vars  betap : poloidal beta
  real(kind(1.0D0)) :: betap = 0.0D0
  !+ad_vars  betbm0 /1.5/ : leading coefficient for NB beta fraction
  real(kind(1.0D0)) :: betbm0 = 1.5D0
  !+ad_vars  bp : poloidal field (T)
  real(kind(1.0D0)) :: bp = 0.0D0
  !+ad_vars  bt /6.0/ : toroidal field on axis (T) (iteration variable 2)
  real(kind(1.0D0)) :: bt = 6.0D0
  !+ad_vars  btot : total toroidal + poloidal field (T)
  real(kind(1.0D0)) :: btot = 0.0D0
  !+ad_vars  burnup : fractional plasma burnup
  real(kind(1.0D0)) :: burnup = 0.0D0
  !+ad_vars  carea /1.0/ : multiplying factor times plasma surface area
  real(kind(1.0D0)) :: carea = 1.0D0
  !+ad_vars  cfe0 /0.0/ : additional iron impurity fraction (n_fe/n_e)
  !+ad_varc               (iteration variable 43)
  real(kind(1.0D0)) :: cfe0 = 0.0D0
  !+ad_vars  csawth /1.0/ : coeff. for sawteeth effects on burn V-s requirement
  real(kind(1.0D0)) :: csawth = 1.0D0
  !+ad_vars  cvol /1.0/ : multiplying factor times plasma volume (normally=1)
  real(kind(1.0D0)) :: cvol = 1.0D0
  !+ad_vars  dene /1.5D20/ : electron density (m**-3) (iteration variable 6)
  real(kind(1.0D0)) :: dene = 1.5D20
  !+ad_vars  deni : fuel ion density (m**-3)
  real(kind(1.0D0)) :: deni = 0.0D0
  !+ad_vars  dign /1.0/ : ignition margin
  real(kind(1.0D0)) :: dign = 1.0D0
  !+ad_vars  dlamee : electron-electron coulomb logarithm
  real(kind(1.0D0)) :: dlamee = 0.0D0
  !+ad_vars  dlamie : ion-electron coulomb logarithm
  real(kind(1.0D0)) :: dlamie = 0.0D0
  !+ad_vars  dlimit(7) : density limit (m**-3) as calculated using various models
  real(kind(1.0D0)), dimension(7) :: dlimit = 0.0D0
  !+ad_vars  dnalp : fast alpha density (m**-3)
  real(kind(1.0D0)) :: dnalp = 0.0D0
  !+ad_vars  dnbeam : hot beam ion density, variable (m**-3)
  real(kind(1.0D0)) :: dnbeam = 0.0D0
  !+ad_vars  dnbeam2 : hot beam ion density from calculation (m**-3)
  real(kind(1.0D0)) :: dnbeam2 = 0.0D0
  !+ad_vars  dnbeta /3.5/ : coefficient for Troyon beta scaling
  real(kind(1.0D0)) :: dnbeta = 3.5D0
  !+ad_vars  dnelimt : density limit (m**-3)
  real(kind(1.0D0)) :: dnelimt = 0.0D0
  !+ad_vars  dnitot : total ion density (m**-3)
  real(kind(1.0D0)) :: dnitot = 0.0D0
  !+ad_vars  dnla : line averaged electron density (m**-3)
  real(kind(1.0D0)) :: dnla = 0.0D0
  !+ad_vars  dnprot : proton ash density (m**-3) (idhe3=1)
  real(kind(1.0D0)) :: dnprot = 0.0D0
  !+ad_vars  dntau : plasma average "n-tau" (m**-3-sec)
  real(kind(1.0D0)) :: dntau = 0.0D0
  !+ad_vars  dnz : high Z ion density (m**-3)
  real(kind(1.0D0)) :: dnz = 0.0D0
  !+ad_vars  ealpha : alpha birth energy in D-T reaction (= 3520 keV)
  real(kind(1.0D0)), parameter :: ealpha = 3520.0D0
  !+ad_vars  epbetmax /0.6/ : max (eps*beta_poloidal) for beta limit scaling 1
  real(kind(1.0D0)) :: epbetmax = 0.6D0
  !+ad_vars  eps : inverse aspect ratio
  real(kind(1.0D0)) :: eps = 0.2857142857D0
  !+ad_vars  faccd : fraction of plasma current produced by aux. current drive
  real(kind(1.0D0)) :: faccd = 0.0D0
  !+ad_vars  facoh : fraction of plasma current produced inductively
  real(kind(1.0D0)) :: facoh = 0.0D0
  !+ad_vars  falpe : fraction of alpha energy to electrons
  real(kind(1.0D0)) :: falpe = 0.0D0
  !+ad_vars  falpha /1.0/ : fraction of alpha power deposited to plasma
  real(kind(1.0D0)) :: falpha = 1.0D0
  !+ad_vars  falpi : fraction of alpha power to ions
  real(kind(1.0D0)) :: falpi = 0.0D0
  !+ad_vars  fbfe /0.35/ : fraction of Fe radiation to Bremsstrahlung
  real(kind(1.0D0)) :: fbfe = 0.35D0
  !+ad_vars  fdeut /0.5/ : deuterium fuel fraction (idhe3=1)
  real(kind(1.0D0)) :: fdeut = 0.5D0
  !+ad_vars  ffwal /0.92/ : fudge factor for the wall load calculation
  real(kind(1.0D0)) :: ffwal = 0.92D0
  !+ad_vars  fhe3 /0.0/ : helium-3 fuel fraction (idhe3=1)
  real(kind(1.0D0)) :: fhe3 = 0.0D0
  !+ad_vars  figmer : physics figure of merit (= plascur*aspect**sbar)
  real(kind(1.0D0)) :: figmer = 0.0D0
  !+ad_vars  fradmin /0.0/ : minimum ratio of radiation to heating power
  real(kind(1.0D0)) :: fradmin = 0.0D0
  !+ad_vars  ftr : /0.5/ : fraction of DT ions that are tritium (idhe3=0)
  !+ad_varc                (if ftr < 0.001, H plasma is used)
  real(kind(1.0D0)) :: ftr = 0.5D0
  !+ad_vars  ftrit /0.5/ : tritium fuel fraction (idhe3=1)
  real(kind(1.0D0)) :: ftrit = 0.5D0
  !+ad_vars  fvsbrnni /1.0/ : fraction of burn V-s from non-inductive means
  !+ad_varc                   (iteration variable 44)
  real(kind(1.0D0)) :: fvsbrnni = 1.0D0
  !+ad_vars  gamma /0.4/ : coefficient for resistive startup V-s formula
  real(kind(1.0D0)) :: gamma = 0.4D0
  !+ad_vars  hfac(ipnlaws) : H factors for an ignited plasma for each scaling law
  real(kind(1.0D0)), dimension(ipnlaws) :: hfac = 0.0D0
  !+ad_vars  hfact /2.0/ : H factor on energy confinement times (iteration variable 10)
  real(kind(1.0D0)) :: hfact = 2.0D0
  !+ad_vars  impc /1.0/ : carbon impurity multiplier
  real(kind(1.0D0)) :: impc = 1.0D0
  !+ad_vars  impfe /1.0/ : iron impurity multiplier
  real(kind(1.0D0)) :: impfe = 1.0D0
  !+ad_vars  impo /1.0/ : oxygen impurity multiplier
  real(kind(1.0D0)) :: impo = 1.0D0
  !+ad_vars  kappa /2.218/ : plasma separatrix elongation
  real(kind(1.0D0)) :: kappa = 2.218D0
  !+ad_vars  kappa95 : 95% plasma elongation
  real(kind(1.0D0)) :: kappa95 = 0.0D0
  !+ad_vars  kappaa : plasma elongation calculated as xarea/(pi.a**2)
  real(kind(1.0D0)) :: kappaa = 0.0D0
  !+ad_vars  palp : alpha power per volume (MW/m**3)
  real(kind(1.0D0)) :: palp = 0.0D0
  !+ad_vars  palpe : alpha power per volume to electrons (MW/m**3)
  real(kind(1.0D0)) :: palpe = 0.0D0
  !+ad_vars  palpi : alpha power per volume to ions (MW/m**3)
  real(kind(1.0D0)) :: palpi = 0.0D0
  !+ad_vars  palpnb : alpha power from hot neutral beam ions (MW)
  real(kind(1.0D0)) :: palpnb = 0.0D0
  !+ad_vars  pbrem : bremstrahhlung power per volume (MW/m**3)
  real(kind(1.0D0)) :: pbrem = 0.0D0
  !+ad_vars  pcharge : non-alpha charged particle fusion power (MW/m**3)
  real(kind(1.0D0)) :: pcharge = 0.0D0
  !+ad_vars  pcoef : profile factor ( = average T / n-weighted T )
  real(kind(1.0D0)) :: pcoef = 0.0D0
  !+ad_vars  pdivt : power to divertor (MW)
  real(kind(1.0D0)) :: pdivt = 0.0D0
  !+ad_vars  pfuscmw : charged particle fusion power (MW)
  real(kind(1.0D0)) :: pfuscmw = 0.0D0
  !+ad_vars  phiint : internal plasma V-s
  real(kind(1.0D0)) :: phiint = 0.0D0
  !+ad_vars  pi : famous number
  real(kind(1.0D0)), parameter :: pi = 3.141592653589793D0
  !+ad_vars  pie : ion/electron equilibration power (MW/m**3)
  real(kind(1.0D0)) :: pie = 0.0D0
  !+ad_vars  plascur : plasma current (A)
  real(kind(1.0D0)) :: plascur = 0.0D0
  !+ad_vars  plrad : edge line radiation power per volume (MW/m**3)
  real(kind(1.0D0)) :: plrad = 0.0D0
  !+ad_vars  pneut : neutron fusion power per volume (MW/m**3)
  real(kind(1.0D0)) :: pneut = 0.0D0
  !+ad_vars  pohmpv : ohmic heating per volume (MW/m**3)
  real(kind(1.0D0)) :: pohmpv = 0.0D0
  !+ad_vars  powerht : heating power (MW) used in confinement time calculation
  real(kind(1.0D0)) :: powerht = 0.0D0
  !+ad_vars  powfmw : fusion power, max (MW)
  real(kind(1.0D0)) :: powfmw = 0.0D0
  !+ad_vars  prad : total core radiation power (MW/m**3)
  real(kind(1.0D0)) :: prad = 0.0D0
  !+ad_vars  psync : synchrotron radiation power (MW/m**3)
  real(kind(1.0D0)) :: psync = 0.0D0
  !+ad_vars  pthrmw(5) : L-H power threshold (MW): <OL>
  !+ad_varc              <LI> ITER 1996 nominal
  !+ad_varc              <LI> ITER 1996 upper bound
  !+ad_varc              <LI> ITER 1996 lower bound
  !+ad_varc              <LI> ITER 1997 excluding elongation
  !+ad_varc              <LI> ITER 1997 including elongation</OL>
  real(kind(1.0D0)), dimension(5) :: pthrmw = 0.0D0
  !+ad_vars  ptre : electron transport power (MW/m**3)
  real(kind(1.0D0)) :: ptre = 0.0D0
  !+ad_vars  ptri : ion transport power (MW/m**3)
  real(kind(1.0D0)) :: ptri = 0.0D0
  !+ad_vars  q /3.0/ : safety factor at plasma edge (q-"psi") (iteration variable 18):
  !+ad_varc            icurr = 2, q = mean safety factor qbar for divertors;
  !+ad_varc            icurr = 3,4, q = safety factor at 95% surface
  real(kind(1.0D0)) :: q = 3.0D0
  !+ad_vars  q0 /1.0/ : safety factor on axis
  real(kind(1.0D0)) :: q0 = 1.0D0
  !+ad_vars  q95 : safety factor at 95% surface
  real(kind(1.0D0)) :: q95 = 0.0D0
  !+ad_vars  qfuel : fuelling rate for D-T (A)
  real(kind(1.0D0)) :: qfuel = 0.0D0
  !+ad_vars  qlim : lower limit for edge safety factor
  real(kind(1.0D0)) :: qlim = 0.0D0
  !+ad_vars  qstar : cylindrical safety factor
  real(kind(1.0D0)) :: qstar = 0.0D0
  !+ad_vars  ralpne /0.1/ : thermal alpha density / electron density
  real(kind(1.0D0)) :: ralpne = 0.10D0
  !+ad_vars  recyle /0.7/ : alpha fraction recycled to main plasma
  real(kind(1.0D0)) :: recyle = 0.7D0
  !+ad_vars  rli /0.65/ : normalised inductivity, energy definition
  real(kind(1.0D0)) :: rli = 0.65D0
  !+ad_vars  rlp : plasma inductance (H)
  real(kind(1.0D0)) :: rlp = 0.0D0
  !+ad_vars  rmajor /7.0/ plasma major radius (m) (iteration variable 3)
  real(kind(1.0D0)) :: rmajor = 7.0D0
  !+ad_vars  rminor : plasma minor radius (m)
  real(kind(1.0D0)) :: rminor = 2.0D0
  !+ad_vars  rmu0 : permeability of free space, 4.pi x 10^(-7) H/m
  real(kind(1.0D0)), parameter :: rmu0 = 1.256637D-6
  !+ad_vars  rnbeam /0.005/ : hot beam density / n_e (iteration variable 7)
  real(kind(1.0D0)) :: rnbeam = 0.005D0
  !+ad_vars  rncne : n_carbon / n_e
  real(kind(1.0D0)) :: rncne = 0.0D0
  !+ad_vars  rndfuel : fuel burnup rate (A)
  real(kind(1.0D0)) :: rndfuel = 0.0D0
  !+ad_vars  rnfene : n_iron / n_e
  real(kind(1.0D0)) :: rnfene = 0.0D0
  !+ad_vars  rnone : n_oxygen / n_e
  real(kind(1.0D0)) :: rnone = 0.0D0
  !+ad_vars  rpfac : neo-classical correction factor to rplas
  real(kind(1.0D0)) :: rpfac = 0.0D0
  !+ad_vars  rplas : plasma resistance (ohm)
  real(kind(1.0D0)) :: rplas = 0.0D0
  !+ad_vars  rtpte /5.0/ He part. confinement time / plasma energy confinement time
  real(kind(1.0D0)) :: rtpte = 5.0D0
  !+ad_vars  sarea : plasma surface area
  real(kind(1.0D0)) :: sarea = 0.0D0
  !+ad_vars  sareao : outboard plasma surface area
  real(kind(1.0D0)) :: sareao = 0.0D0
  !+ad_vars  sf : shape factor
  real(kind(1.0D0)) :: sf = 0.0D0
  !+ad_vars  ssync /0.8/ : synchrotron wall reflectivity factor
  real(kind(1.0D0)) :: ssync = 0.8D0
  !+ad_vars  tauee : electron energy confinement time (sec)
  real(kind(1.0D0)) :: tauee = 0.0D0
  !+ad_vars  taueff : global energy confinement time (sec)
  real(kind(1.0D0)) :: taueff = 0.0D0
  !+ad_vars  tauei : ion energy confinement time (sec)
  real(kind(1.0D0)) :: tauei = 0.0D0
  !+ad_vars  te : /15.0/ : volume averaged electron temperature (keV)
  !+ad_varc                (iteration variable 4)
  real(kind(1.0D0)) :: te = 15.0D0
  !+ad_vars  ten : density weighted average electron temperature (keV)
  real(kind(1.0D0)) :: ten = 0.0D0
  !+ad_vars  ti /8.33/ : volume averaged ion temperature (keV)
  real(kind(1.0D0)) :: ti = 8.33D0
  !+ad_vars  tin : density weighted average ion temperature (keV)
  real(kind(1.0D0)) :: tin = 0.0D0
  !+ad_vars  tratio /1.0/ : ion temperature / electron temperature
  real(kind(1.0D0)) :: tratio = 1.0D0
  !+ad_vars  triang /0.6/ : plasma separatrix triangularity
  real(kind(1.0D0)) :: triang = 0.6D0
  !+ad_vars  triang95 : plasma triangularity at 95% surface
  real(kind(1.0D0)) :: triang95 = 0.0D0
  !+ad_vars  vol : plasma volume (m**3)
  real(kind(1.0D0)) :: vol = 0.0D0
  !+ad_vars  vsbrn : V-s needed during burn (Wb)
  real(kind(1.0D0)) :: vsbrn = 0.0D0
  !+ad_vars  vshift : plasma/device midplane vertical shift - single null
  real(kind(1.0D0)) :: vshift = 0.0D0
  !+ad_vars  vsind : internal and external plasma inductance V-s (Wb)
  real(kind(1.0D0)) :: vsind = 0.0D0
  !+ad_vars  vsres : resistive losses in startup V-s (Wb)
  real(kind(1.0D0)) :: vsres = 0.0D0
  !+ad_vars  vsstt : total V-s needed (Wb)
  real(kind(1.0D0)) :: vsstt = 0.0D0
  !+ad_vars  wallmw : average neutron wall load, max (MW/m**2)
  real(kind(1.0D0)) :: wallmw = 0.0D0
  !+ad_vars  wtgpd : mass of fuel used per day (g)
  real(kind(1.0D0)) :: wtgpd = 0.0D0
  !+ad_vars  xarea : plasma cross-sectional area (m**2)
  real(kind(1.0D0)) :: xarea = 0.0D0
  !+ad_vars  zeff : plasma effective charge
  real(kind(1.0D0)) :: zeff = 0.0D0
  !+ad_vars  zeffai : density weighted plasma effective charge
  real(kind(1.0D0)) :: zeffai = 0.0D0

  !+ad_vars  gtscale /0/ : switch for a/R scaling of dnbeta:
  !+ad_varc                = 0 do not scale dnbeta with eps; 
  !+ad_varc                otherwise scale dnbeta with eps  
  integer :: gtscale = 0
  !+ad_vars  ibss /1/ : switch for bootstrap current scaling:
  !+ad_varc             = 1 ITER bootstrap scaling (high R/a only);
  !+ad_varc             = 2 for more general scaling;
  !+ad_varc             = 3 for new Culham scaling as in AEA FUS 172
  integer :: ibss  = 1
  !+ad_vars  iculbl /0/ : switch for Troyon beta limit scaling:
  !+ad_varc               = 0 apply limit to total beta;
  !+ad_varc               = 1 apply limit to thermal beta;
  !+ad_varc               = 2 apply limit to thermal + neutral beam beta
  integer :: iculbl = 0
  !+ad_vars  iculdl /0/ : switch for density limit:
  !+ad_varc               = 0 use old method;
  !+ad_varc               = 1 use new method (seven formulae to choose from)
  integer :: iculdl = 0
  !+ad_vars  icurr /4/ : switch for plasma current scaling to use:
  !+ad_varc              = 1 Peng analytic fit;
  !+ad_varc              = 2 Peng double null divertor scaling (TART);
  !+ad_varc              = 3 simple ITER scaling (k = 2.2, d = 0.6);
  !+ad_varc              = 4 later ITER scaling, a la Uckan;
  !+ad_varc              = 5 Todd empirical scaling I;
  !+ad_varc              = 6 Todd empirical scaling II;
  !+ad_varc              = 7 Connor-Hastie model
  integer :: icurr = 4
  !+ad_vars  idensl /3/ : switch for density limit to enforce (if ICULDL=1):
  !+ad_varc               = 1 old ASDEX;
  !+ad_varc               = 2 Borrass model for ITER (I);
  !+ad_varc               = 3 Borrass model for ITER (II);
  !+ad_varc               = 4 JET edge radiation;
  !+ad_varc               = 5 JET simplified;
  !+ad_varc               = 6 Hugill-Murakami Mq limit;
  !+ad_varc               = 7 Greenwald limit
  integer :: idensl = 3
  !+ad_vars  idhe3 /0/ : switch for main fusion reaction:
  !+ad_varc              = 0 D-T reaction;
  !+ad_varc              = 1 D-He3 reaction (+ daughters)
  integer :: idhe3 = 0
  !+ad_vars  idivrt /2/ : shape switch (use only idivrt= 2 for now):
  !+ad_varc               = 0 for limiter;
  !+ad_varc               = 1 for single null (diverted side down);
  !+ad_varc               = 2 for double null
  integer :: idivrt = 2
  !+ad_vars  ifalphap /0/ : switch for fast alpha pressure calculation:
  !+ad_varc                 = 0 ITER physics rules (Uckan) fit;
  !+ad_varc                 = 1 Modified fit (D. Ward) - better at high temperature
  integer :: ifalphap = 0
  !+ad_vars  ifispact /1/ : switch for neutronics calculations:
  !+ad_varc                 = 0 neutronics calculations turned off;
  !+ad_varc                 = 1 neutronics calculations turned on
  integer :: ifispact = 1
  !+ad_vars  igeom /0/ : switch for plasma geometry calculation:
  !+ad_varc              = 0 original method;
  !+ad_varc              = 1 new method
  integer :: igeom = 0
  !+ad_vars  ignite /0/ : switch for ignition assumption:
  !+ad_varc               = 0 do not assume plasma ignition;
  !+ad_varc               = 1 assume ignited (but include aux power in costs)
  !+ad_varc               Obviously, ignite must be zero if current drive
  !+ad_varc               is required. Note that whole code is not quite
  !+ad_varc               consistent yet...
  integer :: ignite = 0
  !+ad_vars  iinvqd /1/ : switch for inverse quadrature in tauee laws (1=yes)
  integer :: iinvqd = 1
  !+ad_vars  iiter /1/ : switch for ITER fusion power calculations, (1=yes)
  !+ad_varc              (bad fit if alphan /= 0.5 and/or alphat /= 1.0)
  integer :: iiter = 1
  !+ad_vars  ires /1/ : switch for neo-classical plasma resistivity (1=yes)
  integer :: ires = 1
  !+ad_vars  isc /6/ switch for energy confinement time scaling law
  !+ad_varc          (see description in tauscl)
  integer :: isc = 6
  !+ad_vars  iscrp /1/ : switch for scrapeoff width:
  !+ad_varc              = 0 use 10% of rminor;
  !+ad_varc              = 1 use input (scrapli and scraplo)
  integer :: iscrp = 1
  !+ad_vars  ishape /0/ : switch for plasma cross-sectional shape calculation:
  !+ad_varc               = 0 use input kappa, triang;
  !+ad_varc               = 1 scale qlim, kappa, triang (TART)
  integer :: ishape = 0
  !+ad_vars  itart /0/ : switch for tight aspect ratio models:
  !+ad_varc              = 0 use conventional aspect ratio models;
  !+ad_varc              = 1 use tight aspect ratio models
  integer :: itart = 0
  !+ad_vars  iwalld /1/ : switch for neutron wall load calculation:
  !+ad_varc               = 1 use scaled plasma surface area;
  !+ad_varc               = 2 use first wall area directly
  integer :: iwalld = 1

end module physics_variables

module wibble

  !  ex-blanket.h90
  real(kind(1.0D0)) :: &
       xtfi,xtfo,xtb,xpf,xdo,xdi,ph,pr,pin,pc,etahp,etainp, &
       etalp,etafp,etacp,fkblkt,sgeff
  common /blkre/ &
       xtfi,xtfo,xtb,xpf,xdo,xdi,ph,pr,pin,pc,etahp,etainp, &
       etalp,etafp,etacp,fkblkt,sgeff

  integer :: nipfwh,nlpfwh,lblnkt,estr,astr,bstr,costr,smstr
  common /blki/ nipfwh,nlpfwh,lblnkt,estr,astr,bstr,costr,smstr

  !  ex bldgcom.h90

  real(kind(1.0D0)) :: &
       admv,clh1,clh2,conv,fndt,hccl,hcwt,pibv,rbrt,rbwt,row, &
       rxcl,shmf,shov,stcl,trcl,wgt,wgt2
  common /bldg1/ &
       admv,clh1,clh2,conv,fndt,hccl,hcwt,pibv,rbrt,rbwt,row, &
       rxcl,shmf,shov,stcl,trcl,wgt,wgt2

  !  ex bldgvol.h90

  real(kind(1.0D0)) :: &
       admvol,convol,cryvol,efloor,elevol,esbldgm3,pfbldgm3, &
       rbvol,rmbvol,shovol,tfcbv,triv,volnucb,volrci,wrbi,wsvol 
  common /bldgv1/ &
       admvol,convol,cryvol,efloor,elevol,esbldgm3,pfbldgm3, &
       rbvol,rmbvol,shovol,tfcbv,triv,volnucb,volrci,wrbi,wsvol 

  !  ex build.h90

  real(kind(1.0D0)) :: &
       aplasmin,bcylth,blnkith,blnkoth,bore,ddwex,ddwi,gapds, &
       gapoh,gapomin,gapsto,fwarea,fwith,fwoth,hmax,hr1,ohcth, &
       prtsz,prtszreq,rbld,rinboard,rsldi,rsldo,rtfcin,rtot, &
       scrapli,scraplo,shldith,shldoth,shldtth,tfcth,tfootfi, &
       tfthko,vgap,vgaptf,vgap2
  common /build0/ &
       aplasmin,bcylth,blnkith,blnkoth,bore,ddwex,ddwi,gapds, &
       gapoh,gapomin,gapsto,fwarea,fwith,fwoth,hmax,hr1,ohcth, &
       prtsz,prtszreq,rbld,rinboard,rsldi,rsldo,rtfcin,rtot, &
       scrapli,scraplo,shldith,shldoth,shldtth,tfcth,tfootfi, &
       tfthko,vgap,vgaptf,vgap2

  integer :: iohcl
  common /build1/ iohcl

  real(kind(1.0D0)) :: &
       fmsbc,fmsbl,fmsdwe,fmsdwi,fmsfw,fmsoh,fmssh,fmstf
  common /marten/ &
       fmsbc,fmsbl,fmsdwe,fmsdwi,fmsfw,fmsoh,fmssh,fmstf

  !  ex cdriv.h90

  real(kind(1.0D0)) :: &
       beamwd,bigq,bootipf,bscfmax,cboot,cnbeam,echpwr,&
       echpwr0,echwpow,enbeam,etaech,etalh,etanbi,etaof,&
       feffcd,frbeam,ftritbm,gamcd,pheat,pinjalw,pinje,&
       pinji,plhybd,pnbeam,pofcd,pwplh,pwpnb,taubeam,&
       tbeamin
  common /cdriv0/ &
       beamwd,bigq,bootipf,bscfmax,cboot,cnbeam,echpwr,&
       echpwr0,echwpow,enbeam,etaech,etalh,etanbi,etaof,&
       feffcd,frbeam,ftritbm,gamcd,pheat,pinjalw,pinje,&
       pinji,plhybd,pnbeam,pofcd,pwplh,pwpnb,taubeam,&
       tbeamin

  integer :: iefrf,irfcd
  common /cdriv1/ iefrf,irfcd

  !  ex cost.h90

  real(kind(1.0D0)), dimension(4) :: cfind
  real(kind(1.0D0)), dimension(2) :: uchts
  real(kind(1.0D0)), dimension(4) :: ucoam
  real(kind(1.0D0)), dimension(5) :: ucsc
  real(kind(1.0D0)), dimension(2) :: ucturb
  real(kind(1.0D0)), dimension(4) :: ucwst
  common /cost00/ cfind,uchts,ucoam,ucsc,ucturb,ucwst

  real(kind(1.0D0)) :: &
       abktflnc,adivflnc,blkcst,capcost,cdcost,cdirt,cdrlife, &
       cfactr,chplant,coe,coecap,coefuelt,coeoam,concost, &
       cplife,cpstcst,cpstflnc,crctcore,c221,c222,decomf, &
       dintrt,divcst,divlife,dtlife,fcap0,fcap0cp,fcdfuel, &
       fcontng,fcr0,fkind,fwallcst,moneyint,ratecdol,tbktrepl, &
       tcomrepl,tdivrepl,tlife,uubop,uucd,uudiv,uufuel,uufw, &
       uumag,uuves
  common /cost0/ &
       abktflnc,adivflnc,blkcst,capcost,cdcost,cdirt,cdrlife, &
       cfactr,chplant,coe,coecap,coefuelt,coeoam,concost, &
       cplife,cpstcst,cpstflnc,crctcore,c221,c222,decomf, &
       dintrt,divcst,divlife,dtlife,fcap0,fcap0cp,fcdfuel, &
       fcontng,fcr0,fkind,fwallcst,moneyint,ratecdol,tbktrepl, &
       tcomrepl,tdivrepl,tlife,uubop,uucd,uudiv,uufuel,uufw, &
       uumag,uuves

  integer :: iavail,ifueltyp,ipnet,ireactor,lsa
  common /cost1/ iavail,ifueltyp,ipnet,ireactor,lsa

  real(kind(1.0D0)) :: &
       cconfix,cconshpf,cconshtf,cland,cowner,csi,cturbb
  common /ucost0/ &
       cconfix,cconshpf,cconshtf,cland,cowner,csi,cturbb

  real(kind(1.0D0)) :: &
       ucad,ucaf,ucahts,ucap,ucblbe,ucblli,ucblli2o,ucbllipb, &
       ucblss,ucblvd,ucbpmp,ucbus,uccase,ucco,uccpclb,uccpcl1, &
       uccpmp,uccr,uccry,uccryo,uccu,ucdgen,ucdiv,ucdtc,ucduct, &
       ucech,ucel,uces1,uces2,ucfnc,ucfpr,ucfuel,ucfwa,ucfwps, &
       ucfws,ucf1,ucgss,uche3,uchhten,uchhtex,uchlte,uchrs,uchth, &
       uciac,ucich,ucihx,ucint,uclh,uclv,ucmb,ucme
  common /ucost1/ &
       ucad,ucaf,ucahts,ucap,ucblbe,ucblli,ucblli2o,ucbllipb, &
       ucblss,ucblvd,ucbpmp,ucbus,uccase,ucco,uccpclb,uccpcl1, &
       uccpmp,uccr,uccry,uccryo,uccu,ucdgen,ucdiv,ucdtc,ucduct, &
       ucech,ucel,uces1,uces2,ucfnc,ucfpr,ucfuel,ucfwa,ucfwps, &
       ucfws,ucf1,ucgss,uche3,uchhten,uchhtex,uchlte,uchrs,uchth, &
       uciac,ucich,ucihx,ucint,uclh,uclv,ucmb,ucme

  real(kind(1.0D0)) :: &
       ucmisc,ucnbi,ucnbv,ucof,ucpens,ucpfb,ucpfbk,ucpfbs,ucpfcb, &
       ucpfdr1,ucpfic,ucpfps,ucphx,ucpp,ucrb,ucsh,ucshld,ucswyd, &
       uctfbr,uctfbus,uctfdr,uctfgr,uctfic,uctfps,uctfsw,uctpmp, &
       uctr,ucvalv,ucvdsh,ucviac,ucwindpf,ucwindtf,ucws
  common /ucost2/ &
       ucmisc,ucnbi,ucnbv,ucof,ucpens,ucpfb,ucpfbk,ucpfbs,ucpfcb, &
       ucpfdr1,ucpfic,ucpfps,ucphx,ucpp,ucrb,ucsh,ucshld,ucswyd, &
       uctfbr,uctfbus,uctfdr,uctfgr,uctfic,uctfps,uctfsw,uctpmp, &
       uctr,ucvalv,ucvdsh,ucviac,ucwindpf,ucwindtf,ucws

  !  ex divrt.h90

  integer :: divdum
  common /divrti/ divdum

  real(kind(1.0D0)) :: &
       adas,anginc,bpsout,c1div,c2div,c3div,c4div,c5div,c6div,delld, &
       dendiv,densin,divclfr,divdens,divmas,divplt,divsur,fdfs,fdiva, &
       fgamp,fhout,fififi,frrp,hldiv,hldivlim,ksic,lamp,minstang, &
       omegan,omlarg,plsepo,ppdivr,prn1,ptpdiv,rconl,rlclolcn,rlenmax, &
       rsrd,rstrko,tconl,tdiv,tsep,xparain,xpertin,zeffdiv
  common /divrt/ &
       adas,anginc,bpsout,c1div,c2div,c3div,c4div,c5div,c6div,delld, &
       dendiv,densin,divclfr,divdens,divmas,divplt,divsur,fdfs,fdiva, &
       fgamp,fhout,fififi,frrp,hldiv,hldivlim,ksic,lamp,minstang, &
       omegan,omlarg,plsepo,ppdivr,prn1,ptpdiv,rconl,rlclolcn,rlenmax, &
       rsrd,rstrko,tconl,tdiv,tsep,xparain,xpertin,zeffdiv

  !  ex estocom.h90

  integer :: iscenr
  common /est1/ iscenr

  !  ex fispact.h90

!--Version number 1.000
!
!--Description
!  INCLUDE file containing values calculated by FISPACT routines
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  19 February 1997
!
!--Reference
!  None
!  
!--History
!  06/02/97 PJK 1.000 Initial version
!
!--Contents
!  BLIIZP : Inboard blanket integrated zone power
!  BLIMZP : Inboard blanket mean zone power density
!  BLOIZP : Outboard blanket integrated zone power
!  BLOMZP : Outboard blanket mean zone power density
!  FWIIZP : Inboard first wall integrated zone power
!  FWIMZP : Inboard first wall mean zone power density
!  FWOIZP : Outboard first wall integrated zone power
!  FWOMZP : Outboard first wall mean zone power density
!  BLIACT : Inboard blanket total activity (Bq)
!  BLIGDR : Inboard blanket total gamma dose rate (Sv/hr)
!  BLIHKW : Inboard blanket total heat output (kW)
!  BLOACT : Outboard blanket total activity (Bq)
!  BLOGDR : Outboard blanket total gamma dose rate (Sv/hr)
!  BLOHKW : Outboard blanket total heat output (kW)
!  FWIACT : Inboard first wall total activity (Bq)
!  FWIGDR : Inboard first wall total gamma dose rate (Sv/hr)
!  FWIHKW : Inboard first wall total heat output (kW)
!  FWOACT : Outboard first wall total activity (Bq)
!  FWOGDR : Outboard first wall total gamma dose rate (Sv/hr)
!  FWOHKW : Outboard first wall total heat output (kW)
!  FWTEMP : Outboard first wall temperature after a LOCA (K)

  real(kind(1.0D0)) :: &
       bliizp,blimzp,bloizp,blomzp,fwiizp,fwimzp,fwoizp,fwomzp,fwtemp
  common /fisp1/ &
       bliizp,blimzp,bloizp,blomzp,fwiizp,fwimzp,fwoizp,fwomzp,fwtemp

  real(kind(1.0D0)), dimension(3) :: &
       bliact,bligdr,blihkw,bloact,blogdr,blohkw,fwiact,fwigdr, &
       fwihkw,fwoact,fwogdr,fwohkw
  common /fisp2/ &
       bliact,bligdr,blihkw,bloact,blogdr,blohkw,fwiact,fwigdr, &
       fwihkw,fwoact,fwogdr,fwohkw

  !  ex fwblsh.h90

  real(kind(1.0D0)) :: &
       bktlife,coolmass,cryomass,denstl,dewmkg,emult,fblbe,fblli2o, &
       fbllipb,fblli,fblss,fblvd,fhole,fvolbi,fvolbo,fvolcry,fvoldw, &
       fvolsi,fvolso,fwclfr,fwmass,pnucblkt,pnuccp,pnucloss,pnucshld, &
       ptfnuc,vdewex,vdewin,vfblkt,vfshld,volblkt,volblkti,volblkto, &
       volshld,whtblbe,whtblkt,wtblli2o,wtbllipb,whtblli,whtblss, &
       whtblvd,whtshld,wpenshld,wtshldi,wtshldo
  common /fwbsxx/ &
       bktlife,coolmass,cryomass,denstl,dewmkg,emult,fblbe,fblli2o, &
       fbllipb,fblli,fblss,fblvd,fhole,fvolbi,fvolbo,fvolcry,fvoldw, &
       fvolsi,fvolso,fwclfr,fwmass,pnucblkt,pnuccp,pnucloss,pnucshld, &
       ptfnuc,vdewex,vdewin,vfblkt,vfshld,volblkt,volblkti,volblkto, &
       volshld,whtblbe,whtblkt,wtblli2o,wtbllipb,whtblli,whtblss, &
       whtblvd,whtshld,wpenshld,wtshldi,wtshldo

  !  ex htpwr.h90

  real(kind(1.0D0)) :: &
       baseel,crypmw,ctht,etahhten,etahhtex,etahlte,etahth,etath, &
       facht,fauxbop,fcsht,ffwlg,fgrosbop,fmgdmw,helecmw,hpower, &
       hthermmw,hvolume,helpow,htpmw,pacpmw,peakmva,pfwdiv,pgrossmw, &
       pinjht,pinjwp,pnetelmw,ppmphemw,priheat,psecht,pthermmw,pwpm2, &
       rnihx,rnphx,tfacpd,tlvpmw,trithtmw,vachtmw
  common /htpwr0/ &
       baseel,crypmw,ctht,etahhten,etahhtex,etahlte,etahth,etath, &
       facht,fauxbop,fcsht,ffwlg,fgrosbop,fmgdmw,helecmw,hpower, &
       hthermmw,hvolume,helpow,htpmw,pacpmw,peakmva,pfwdiv,pgrossmw, &
       pinjht,pinjwp,pnetelmw,ppmphemw,priheat,psecht,pthermmw,pwpm2, &
       rnihx,rnphx,tfacpd,tlvpmw,trithtmw,vachtmw

  integer :: ihplant,iprimhtp
  common /htpwr1/ ihplant,iprimhtp

  !  ex ife.h90

  !  Main switches

  integer :: ife,ifetyp,ifedrv
  common /ifei/ ife,ifetyp,ifedrv

  !  Limits, f-values

  real(kind(1.0D0)) :: frrmax,rrmax
  common /ifelim/ frrmax,rrmax

  !  Physics

  real(kind(1.0D0)) :: &
       drveff,edrive,fburn,pdrive,tgain,gain,etadrv,reprat
  common /ifep1/ &
       drveff,edrive,fburn,pdrive,tgain,gain,etadrv,reprat

  real(kind(1.0D0)), dimension(10) :: etave,gainve
  common /ifep2/ etave,gainve

  !  Costs

  real(kind(1.0D0)) :: &
       uctarg,uccarb,ucconc,ucflib,cdriv0,cdriv1,cdriv2,dcdrv0, &
       dcdrv1,dcdrv2,mcdriv
  common /ifec1/ &
       uctarg,uccarb,ucconc,ucflib,cdriv0,cdriv1,cdriv2,dcdrv0, &
       dcdrv1,dcdrv2,mcdriv

  !  Device build and material fractions and masses

  integer, parameter ::  maxmat = 7

  real(kind(1.0D0)) :: &
       bldr,bldzl,bldzu,chrad,chdzl,chdzu,chvol,fwdr,fwdzl,fwdzu, &
       shdr,shdzl,shdzu,v1dr,v1dzl,v1dzu,v2dr,v2dzl,v2dzu,v3dr,v3dzl, &
       v3dzu,sombdr,somtdr,flirad,mflibe,fbreed
  common /ifeb1/ &
       bldr,bldzl,bldzu,chrad,chdzl,chdzu,chvol,fwdr,fwdzl,fwdzu, &
       shdr,shdzl,shdzu,v1dr,v1dzl,v1dzu,v2dr,v2dzl,v2dzu,v3dr,v3dzl, &
       v3dzu,sombdr,somtdr,flirad,mflibe,fbreed

  real(kind(1.0D0)), dimension(3,0:maxmat) :: &
       blmatf,blmatm,blmatv,fwmatf,fwmatm,fwmatv,shmatf,shmatm, &
       shmatv,v1matf,v1matm,v1matv,v2matf,v2matm,v2matv,v3matf, &
       v3matm,v3matv

  real(kind(1.0D0)), dimension(0:maxmat) :: chmatf,chmatm,chmatv

  real(kind(1.0D0)), dimension(3) :: blvol,fwvol,shvol,v1vol,v2vol,v3vol

  common /ifeb2/ &
     blmatf,blmatm,blmatv,blvol,chmatf,chmatm,chmatv,fwmatf,fwmatm, &
     fwmatv,fwvol,shmatf,shmatm,shmatv,shvol,v1matf,v1matm,v1matv, &
     v1vol,v2matf,v2matm,v2matv,v2vol,v3matf,v3matm,v3matv,v3vol

  real(kind(1.0D0)) :: &
       r1,r2,r3,r4,r5,r6,r7,zl1,zl2,zl3,zl4,zl5,zl6,zl7, &
       zu1,zu2,zu3,zu4,zu5,zu6,zu7
  common /ifeb3/ &
       r1,r2,r3,r4,r5,r6,r7,zl1,zl2,zl3,zl4,zl5,zl6,zl7, &
       zu1,zu2,zu3,zu4,zu5,zu6,zu7

  !  Heat transport

  real(kind(1.0D0)) :: pifecr,tdspmw,tfacmw,ptargf
  common /ifep1/ pifecr,tdspmw,tfacmw,ptargf

  !  ex ineq.h90

  real(kind(1.0D0)) :: &
       auxmin,betpmx,bmxlim,dtmpmx,fauxmn,fbeta,fbetap,fbetatry, &
       fdene,fdivcol,fdtmp,ffuspow,fgamcd,fhldiv,fiooic,fipir,fjohc, &
       fjohc0,fjprot,fjtfc,fmva,fpeakb,fpinj,fpnetel,fportsz,fptemp, &
       fq,fqval,frfpf,frfptf,frminor,fstrcase,fstrcond,ftburn,ftcycl, &
       ftmargtf,ftohs,ftpeak,fvdump,fvs,fwalld,gammax,mvalim,pnetelin, &
       powfmax,tbrnmn,tcycmn,tohsmn,tpkmax,walalw
  common /ineq/ &
       auxmin,betpmx,bmxlim,dtmpmx,fauxmn,fbeta,fbetap,fbetatry, &
       fdene,fdivcol,fdtmp,ffuspow,fgamcd,fhldiv,fiooic,fipir,fjohc, &
       fjohc0,fjprot,fjtfc,fmva,fpeakb,fpinj,fpnetel,fportsz,fptemp, &
       fq,fqval,frfpf,frfptf,frminor,fstrcase,fstrcond,ftburn,ftcycl, &
       ftmargtf,ftohs,ftpeak,fvdump,fvs,fwalld,gammax,mvalim,pnetelin, &
       powfmax,tbrnmn,tcycmn,tohsmn,tpkmax,walalw

  !  ex pfcoil.h90

  !  ngrpmx is the maximum number of PF coil groups
  !  nclsmx is the maximum number of coils in one group
  !  nptsmx is the maximum number of points across the plasma midplane
  !         at which the magnetic field is fixed
  !  nfixmx is the maximum number of fixed current coils

  integer, parameter :: ngrpmx = 8
  integer, parameter :: nclsmx = 2
  integer, parameter :: nptsmx = 32
  integer, parameter :: nfixmx = 64
  integer, parameter :: ngc = ngrpmx*nclsmx
  integer, parameter :: ngc2 = ngc+2

  real(kind(1.0D0)) :: &
       acsoh,ac1oh,alfapf,bmaxoh,bmaxoh0,cohbof,cohbop,coheof,cptoh, &
       fcohbof,fcohbop,fcuoh,ohhghf,pfclres,powohres,powpfres,rjohc, &
       rjohc0,rohc,rpf1,rpf2,sccufac,sigpfalw,vfohc,whtpf,whtpfs
  common /pfc0/ &
       acsoh,ac1oh,alfapf,bmaxoh,bmaxoh0,cohbof,cohbop,coheof,cptoh, &
       fcohbof,fcohbop,fcuoh,ohhghf,pfclres,powohres,powpfres,rjohc, &
       rjohc0,rohc,rpf1,rpf2,sccufac,sigpfalw,vfohc,whtpf,whtpfs

  integer ::ipfres,isumatpf,ncirt,ngrp,nohc
  common /pfc1/ ipfres,isumatpf,ncirt,ngrp,nohc

  real(kind(1.0D0)), dimension(ngc2) :: &
       bpf,cptdin,curpfb,curpff,curpfs,ra,rb,ric,rjconpf,rjpfalw, &
       rpf,turns,vf,wtc,wts,zh,zl,zpf
  real(kind(1.0D0)), dimension(ngc2,6) :: cpt,waves
  common /pfc2/ &
       bpf,cpt,cptdin,curpfb,curpff,curpfs,ra,rb,ric,rjconpf,rjpfalw, &
       rpf,turns,vf,waves,wtc,wts,zh,zl,zpf

  integer, dimension(ngc) :: ipfloc
  integer, dimension(ngrpmx+2) :: ncls
  common /pfc3/ ipfloc,ncls

  !  PF scaling variables :

  integer :: nfxfh
  common /pfscl1/ nfxfh

  real(kind(1.0D0)) :: routr
  common /pfscl2/ routr

  real(kind(1.0D0)), dimension(ngrpmx) :: zref
  common /pfscl3/ zref

!  ex pulse.h90

!--Version number 1.100
!
!--Description
!  Include file containing pulsed reactor variables
!
!--Author
!  Chris Gardner, c/o
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  11 April 1994
!
!--Reference
!  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
!  
!--History
!  08/11/93 PJK 1.000 Initial version
!  11/04/94 PJK 1.100 Changed ITPULS to ITCYCL
!
!--Contents
!  afw    : inner radius of each first wall structural cylinder (m)
!  bfw    : outer radius of each first wall structural cylinder (m)
!  bctmp  : bulk coolant temperature (C)
!  coolp  : coolant pressure (Pa)
!  dtstor : maximum allowable temperature change within the stainless
!           steel thermal storage block (K)
!  fwlife : first wall lifetime (yrs)
!  tmprse : temperature rise in coolant along toroidal
!           extent of first wall (C)
!  tpeak  : peak temperature in first wall (C)
!  istore : switch for thermal storage method (1/2/3)
!  itcycl : switch for first wall axial stress model (1/2/3)
!  lpulse : switch for reactor model : 1 = pulsed, 0 = continuous

  real(kind(1.0D0)) :: &
       afw,bfw,bctmp,coolp,dtstor,fwlife,tmprse,tpeak
  common /pulse1/ &
       afw,bfw,bctmp,coolp,dtstor,fwlife,tmprse,tpeak

  integer ::istore,itcycl,lpulse
  common /pulse2/ istore,itcycl,lpulse

!  ex pwrcom.h90

  real(kind(1.0D0)) :: &
       acptmax,ensxpfm,pfckts,spfbusl,spsmva,srcktpm,vpfskv
  common /pwrcom/ &
       acptmax,ensxpfm,pfckts,spfbusl,spsmva,srcktpm,vpfskv

!  ex rfp.h90

!--Version number 1.000
!
!--Description
!  INCLUDE file for reversed-field pinch module in PROCESS.
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  27 February 1996
!
!--Reference
!  None
!  
!--History
!  27/02/96 PJK 1.000 Initial version
!
!--Contents
!  irfp   : Switch for rfp option (0=off)
!  nrfppf : number of RFP PF coils
!  rrpf   : radius of each RFP PF coil (m)
!  zzpf   : vertical position of each RFP PF coil (m)
!  drpf   : radial cross-section of each RFP PF coil (m)
!  dzpf   : vertical cross-section of each RFP PF coil (m)
!  nturns : number of turns of each RFP PF coil
!  cptrfp : current per turn in each RFP PF coil (A/m2)
!  resrfp : resistance of each RFP PF coil
!  tftort : TF coil toroidal thickness (m)
!  pfrmax : radius of largest PF coil (m)
!  pfmmax : mass of heaviest PF coil (tonnes)
!  rfpf   : reversal parameter F
!  rfpth  : pinch parameter theta

  integer, parameter :: nrfppf = 16

  real(kind(1.0D0)), dimension(nrfppf) :: &
       rrpf,zzpf,drpf,dzpf,nturns,cptrfp,resrfp
  common /rfpdar/ &
       rrpf,zzpf,drpf,dzpf,nturns,cptrfp,resrfp

  real(kind(1.0D0)) :: tftort,pfrmax,pfmmax,rfpf,rfpth
  common /rfpdbl/ tftort,pfrmax,pfmmax,rfpf,rfpth

  integer :: irfp
  common /rfpint/ irfp

!  ex start.h90

!--Version number 1.000
!
!--Description
!  INCLUDE file for plasma start-up routine
!
!--Author
!  Peter Knight D3/012 Culham Laboratory, ext.3330
!
!--Date
!  08 November 1993
!
!--Reference
!  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
!  
!--History
!  08/11/93 PJK 1.000 Initial version
!
!--Contents
!  nign   : electron density at start-up (m**-3)
!  tign   : electron temperature at start-up (keV)
!  ptaue  : exponent in taue formula
!  qtaue  : exponent in taue formula
!  rtaue  : exponent in taue formula
!  gtaue  : factor in taue formula
!  ftaue  : factor in taue formula
!  aa     : constant
!  bb     : constant
!  cc     : constant
!  dd     : constant
!  s      : constant

  real(kind(1.0D0)) :: &
       nign,tign,ptaue,qtaue,rtaue,gtaue,ftaue,aa,bb,cc,dd
  common /strt1/ &
       nign,tign,ptaue,qtaue,rtaue,gtaue,ftaue,aa,bb,cc,dd

  integer :: s
  common /strt2/ s

!  ex stella.h90

!--Version number 1.000
!
!--Description
!  INCLUDE file for stellarator module in PROCESS.
!
!--Author
!  Peter Knight D3/G12 Culham Laboratory, ext.3330
!
!--Date
!  28 June 1994
!
!--Reference
!  None
!  
!--History
!  28/06/94 PJK 1.000 Initial version
!
!--Contents
!  istell : Switch for stellarator option (0=off)
!  isthtr : Switch for different auxiliary heating methods

  integer :: istell,isthtr
  common /stlint/ istell,isthtr

!  ex struccom.h90

  real(kind(1.0D0)) :: aintmass,clgsmass,coldmass,fncmass,gsmass
  common /struc1/ aintmass,clgsmass,coldmass,fncmass,gsmass

!  ex tfcoil.h90

  integer :: itfsup
  common /tfcom0/ itfsup

  !  Resistive TF coil variables

  real(kind(1.0D0)) :: &
       arealeg,bmaxtf,cdtfleg,cforce,cpres,cpttf,drtop,dztop,estotf, &
       fcoolcp,jbus,oacdcp,prescp,rbmax,rhocp,rhotfleg,ripmax,ripple, &
       ritfc,sigrad,sigtan,sigver,tcpav,tfareain,tfboreh,tfbusl, &
       tfbusmas,tfcmw,tfcpmw,tflegmw,tflegres,tfno,tmpcry,turnstf, &
       vforce,vftf,volcp,voltfleg,vtfkv,whtcp,whttf,whttflgs,wpvf,wtbc
  common /tfcom1/ &
       arealeg,bmaxtf,cdtfleg,cforce,cpres,cpttf,drtop,dztop,estotf, &
       fcoolcp,jbus,oacdcp,prescp,rbmax,rhocp,rhotfleg,ripmax,ripple, &
       ritfc,sigrad,sigtan,sigver,tcpav,tfareain,tfboreh,tfbusl, &
       tfbusmas,tfcmw,tfcpmw,tflegmw,tflegres,tfno,tmpcry,turnstf, &
       vforce,vftf,volcp,voltfleg,vtfkv,whtcp,whttf,whttflgs,wpvf,wtbc

  !  Centrepost variables

  real(kind(1.0D0)) :: &
       cph2o,denh2o,etapump,frhocp,kcp,kh2o,muh2o,ncool,ppump, &
       ptempalw,rcool,tcoolin,tcpav2,tcpmax,vcool
  common /tfcom2/ &
       cph2o,denh2o,etapump,frhocp,kcp,kh2o,muh2o,ncool,ppump, &
       ptempalw,rcool,tcoolin,tcpav2,tcpmax,vcool

  !  Superconducting TF coil variables

  integer :: isumattf,itfmod,magnt,jcrit_model
  common /tfcom3/ isumattf,itfmod,magnt,jcrit_model

  real(kind(1.0D0)) :: &
       acasetf,acndttf,acond,acstf,aiwp,alstrtf,aspcstf,aswp,avwp, &
       bcritsc,bmaxtfrp,borev,casestr,casfact,casthi,casths,csutf, &
       csytf,dcase,dcopper,deflect,eyins,eystl,eywp,fcutfsu,jcritsc, &
       jwdgcrt,jwdgpro,jwptf,poisson,rjtfsual,rnltf,sigrcon,sigtcon, &
       sigvert,strncon,strtf1,strtf2,tcritsc,tdmptf,tfckw,tficrn, &
       tfind,tfleng,tfocrn,tfsai,tfsao,tftmp,thicndut,thkcas,thkwp, &
       thwcndut,tinstf,tmargmin,tmargtf,tmaxpro,vdalw,vtfskv,whtcas, &
       whtcon,whtconcu,whtconsc,whtconsh,wwp1,wwp2
  common /tfcom4/ &
       acasetf,acndttf,acond,acstf,aiwp,alstrtf,aspcstf,aswp,avwp, &
       bcritsc,bmaxtfrp,borev,casestr,casfact,casthi,casths,csutf, &
       csytf,dcase,dcopper,deflect,eyins,eystl,eywp,fcutfsu,jcritsc, &
       jwdgcrt,jwdgpro,jwptf,poisson,rjtfsual,rnltf,sigrcon,sigtcon, &
       sigvert,strncon,strtf1,strtf2,tcritsc,tdmptf,tfckw,tficrn, &
       tfind,tfleng,tfocrn,tfsai,tfsao,tftmp,thicndut,thkcas,thkwp, &
       thwcndut,tinstf,tmargmin,tmargtf,tmaxpro,vdalw,vtfskv,whtcas, &
       whtcon,whtconcu,whtconsc,whtconsh,wwp1,wwp2

  real(kind(1.0D0)), dimension(5) :: dcond,eyoung,jeff,sigrtf,sigttf
  real(kind(1.0D0)), dimension(6) :: radtf
  common /tfcom5/ dcond,eyoung,jeff,radtf,sigrtf,sigttf

  real(kind(1.0D0)) :: farc4tf
  common /tfcom6/ farc4tf

  real(kind(1.0D0)), dimension(5) :: dthet,radctf,xarc,xctfc,yarc,yctfc
  common /tfcom7/ dthet,radctf,xarc,xctfc,yarc,yctfc    

!  ex times.h90

  real(kind(1.0D0)) :: &
       tburn,tburn0,tdown,tdwell,theat,tohs,tohsin,tpulse,tqnch,tramp
  common /times0/ &
       tburn,tburn0,tdown,tdwell,theat,tohs,tohsin,tpulse,tqnch,tramp

  real(kind(1.0D0)), dimension(6) :: tim
  common /times1/ tim

!  ex torsdat.h90

  real(kind(1.0D0)) :: dlscal,vacdshm,vcdimax,vpumpn
  common /tors0/ dlscal,vacdshm,vcdimax,vpumpn

  integer :: nvduct,nvtype
  common /tors1/ nvduct,nvtype

!  ex vaccom.h90

  real(kind(1.0D0)) :: pbase,prdiv,rat,tn
  common /vac0/ pbase,prdiv,rat,tn

  integer :: ntype
  common /vac1/ ntype

!  ex vltcom.h90

  real(kind(1.0D0)) :: &
       vsbn,vsefbn,vsefsu,vseft,vsoh,vsohbn,vsohsu,vssu,vstot
  common /vltcm0/ &
       vsbn,vsefbn,vsefsu,vseft,vsoh,vsohbn,vsohsu,vssu,vstot

  real(kind(1.0D0)), dimension(ngc2,ngc2) :: sxlg
  common /vltcm1/ sxlg

end module wibble
