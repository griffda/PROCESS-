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

  !+ad_vars  icase : description of run or PROCESS version number
  character(len=48) :: icase = 'PROCESS standard D-T tokamak model'

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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  degrad : degrees to radians, = pi/180
  real(kind(1.0D0)), parameter :: degrad = 0.01745329251D0
  !+ad_vars  echarge : electron charge (C)
  real(kind(1.0D0)), parameter :: echarge = 1.60217733D-19
  !+ad_vars  mproton : proton mass (kg)
  real(kind(1.0D0)), parameter :: mproton = 1.6726231D-27
  !+ad_vars  pi : famous number
  real(kind(1.0D0)), parameter :: pi = 3.1415926535897932D0
  !+ad_vars  rmu0 : permeability of free space, 4.pi x 10^(-7) H/m
  real(kind(1.0D0)), parameter :: rmu0 = 1.256637062D-6
  !+ad_vars  twopi : 2 pi
  real(kind(1.0D0)), parameter :: twopi = 6.2831853071795862D0

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
  !+ad_desc  current drive systems. It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>cdriv.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  16/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  beamwd /0.31/ : beam width (m)
  real(kind(1.0D0)) :: beamwd = 0.31D0
  !+ad_vars  bigq : P_fusion / P_injection
  real(kind(1.0D0)) :: bigq = 0.0D0
  !+ad_vars  bootipf : bootstrap current fraction
  real(kind(1.0D0)) :: bootipf = 0.0D0
  !+ad_vars  bscfmax /0.9/ : max fraction of plasma current from bootstrap;
  !+ad_varc                  if bscfmax < 0, bootstrap fraction = abs(bscfmax)
  real(kind(1.0D0)) :: bscfmax = 0.9D0
  !+ad_vars  cboot /1.0/ : bootstrap current fraction multiplier
  real(kind(1.0D0)) :: cboot = 1.0D0
  !+ad_vars  cnbeam : neutral beam current (A)
  real(kind(1.0D0)) :: cnbeam = 0.0D0
  !+ad_vars  echpwr : ECH power (W)
  real(kind(1.0D0)) :: echpwr = 0.0D0
  !+ad_vars  echpwr0 /2.0D6/ : startup ECH power (W)
  real(kind(1.0D0)) :: echpwr0 = 2.0D6
  !+ad_vars  echwpow : ECH wall plug power (W)
  real(kind(1.0D0)) :: echwpow = 0.0D0
  !+ad_vars  enbeam /1.0D3/ : neutral beam energy (keV) (iteration variable 19)
  real(kind(1.0D0)) :: enbeam = 1.0D3
  !+ad_vars  etaech /0.5/ : ECH wall plug to injector efficiency
  real(kind(1.0D0)) :: etaech = 0.5D0
  !+ad_vars  etalh /0.5/ : lower hybrid wall plug to injector efficiency
  real(kind(1.0D0)) :: etalh = 0.5D0
  !+ad_vars  etanbi /0.5/ : neutral beam wall plug to injector efficiency
  real(kind(1.0D0)) :: etanbi = 0.5D0
  !+ad_vars  etaof /0.5/ : OFCD wall plug to injector efficiency
  real(kind(1.0D0)) :: etaof = 0.5D0
  !+ad_vars  feffcd /1.0/ : current drive efficiency fudge factor (iteration variable 47)
  real(kind(1.0D0)) :: feffcd = 1.0D0
  !+ad_vars  frbeam /1.05/ : R_tangential / R_major for neutral beam injection
  real(kind(1.0D0)) :: frbeam = 1.05D0
  !+ad_vars  ftritbm /1.0D-6/ : fraction of beam that is tritium
  real(kind(1.0D0)) :: ftritbm = 1.0D-6
  !+ad_vars  gamcd : normalised current drive efficiency (A/W-m2)
  real(kind(1.0D0)) :: gamcd = 0.0D0
  !+ad_vars  iefrf /5/ : switch for current drive efficiency model: <OL>
  !+ad_varc                <LI> Fenstermacher Lower Hybrid
  !+ad_varc                <LI> Ion Cyclotron current drive
  !+ad_varc                <LI> Fenstermacher ECH
  !+ad_varc                <LI> Ehst Lower Hybrid
  !+ad_varc                <LI> ITER Neutral Beam
  !+ad_varc                <LI> new Culham Lower Hybrid model
  !+ad_varc                <LI> new Culham ECCD model
  !+ad_varc                <LI> new Culham Neutral Beam model
  !+ad_varc                <LI> RFP Oscillating Field current drive </OL>
  integer :: iefrf = 5
  !+ad_vars  irfcd /1/ : switch for current drive calculation (1=yes,0=no)
  integer :: irfcd = 1
  !+ad_vars  pheat /0.0/ : heating power not used for current drive (W)
  !+ad_varc                (iteration variable 11)
  real(kind(1.0D0)) :: pheat = 0.0D0
  !+ad_vars  pinjalw /25.0/ : Maximum allowable value for injected power (MW)
  real(kind(1.0D0)) :: pinjalw = 25.0D0
  !+ad_vars  pinje : auxiliary power to electrons (W)
  real(kind(1.0D0)) :: pinje = 0.0D0
  !+ad_vars  pinji : auxiliary power to ions (W)
  real(kind(1.0D0)) :: pinji = 0.0D0
  !+ad_vars  plhybd : lower hybrid injection power (W)
  real(kind(1.0D0)) :: plhybd = 0.0D0
  !+ad_vars  pnbeam : neutral beam injection power (W)
  real(kind(1.0D0)) :: pnbeam = 0.0D0
  !+ad_vars  pofcd : OFCD injection power (W)
  real(kind(1.0D0)) :: pofcd = 0.0D0
  !+ad_vars  pwplh : lower hybrid wall plug power (W)
  real(kind(1.0D0)) :: pwplh = 0.0D0
  !+ad_vars  pwpnb : neutral beam wall plug power (W)
  real(kind(1.0D0)) :: pwpnb = 0.0D0
  !+ad_vars  taubeam : neutral beam e-decay lengths to plasma centre
  real(kind(1.0D0)) :: taubeam = 0.0D0
  !+ad_vars  tbeamin /3.0/ : permitted neutral beam e-decay lengths to plasma centre
  real(kind(1.0D0)) :: tbeamin = 3.0D0

end module current_drive_variables

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
  !+ad_desc  divertor components. It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>divrt.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  17/10/12 PJK Initial version of module
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
  !+ad_vars  bpsout /0.6/ : reference B_p at outer divertor strike point (T)
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
  !+ad_vars  dendiv : plasma density at divertor (10**20 m**-3)
  real(kind(1.0D0)) :: dendiv = 0.0D0
  !+ad_vars  densin : density at plate (on separatrix) (10**20 m**-3)
  real(kind(1.0D0)) :: densin = 0.0D0
  !+ad_vars  divclfr /0.3/ : divertor coolant fraction
  real(kind(1.0D0)) :: divclfr = 0.3D0
  !+ad_vars  divdens /1.0D4/ : divertor structure density (kg/m**3)
  real(kind(1.0D0)) :: divdens = 1.0D4
  !+ad_vars  divdum /0/ : switch for divertor Zeff model: 0=calc, 1=input
  integer :: divdum = 0
  !+ad_vars  divmas : divertor plate mass (kg)
  real(kind(1.0D0)) :: divmas = 0.0D0
  !+ad_vars  divplt /0.035/ : divertor plate thickness (m) (from Spears, Sept 1990)
  real(kind(1.0D0)) :: divplt = 0.035D0
  !+ad_vars  divsur : divertor surface area (m**2)
  real(kind(1.0D0)) :: divsur = 0.0D0
  !+ad_vars  fdfs /10.0/ : radial gradient ratio
  real(kind(1.0D0)) :: fdfs = 10.0D0
  !+ad_vars  fdiva /1.11/ : divertor area fudge factor (for ITER, Sept 1990)
  real(kind(1.0D0)) :: fdiva = 1.11D0
  !+ad_vars  fgamp /1.0/ : sheath potential factor (not used)
  real(kind(1.0D0)) :: fgamp = 1.0D0
  !+ad_vars  fhout : fraction of power to outer divertor (for single null)
  real(kind(1.0D0)) :: fhout = 0.0D0
  !+ad_vars  fififi /0.004/ : coefficient for gamdiv
  real(kind(1.0D0)) :: fififi = 4.0D-3
  !+ad_vars  frrp /0.4/ : fraction of radiated power to plate
  real(kind(1.0D0)) :: frrp = 0.4D0
  !+ad_vars  hldiv : outer divertor heat load (MW/m**2)
  real(kind(1.0D0)) :: hldiv = 0.0D0
  !+ad_vars  hldivlim /5.0/ : heat load limit (MW/m**2)
  real(kind(1.0D0)) :: hldivlim = 5.0D0
  !+ad_vars  ksic /0.8/ : power fraction for outer double-null scrape-off plasma
  real(kind(1.0D0)) :: ksic = 0.8D0
  !+ad_vars  lamp : power flow width (m)
  real(kind(1.0D0)) :: lamp = 0.0D0
  !+ad_vars  minstang : minimum strike angle for heat flux calculation
  real(kind(1.0D0)) :: minstang = 0.0D0
  !+ad_vars  omegan /1.0/ : pressure ratio (nT)_plasma / (nT)_scrape-off
  real(kind(1.0D0)) :: omegan = 1.0D0
  !+ad_vars  omlarg : power spillage to private flux factor
  real(kind(1.0D0)) :: omlarg = 0.0D0
  !+ad_vars  plsepo /1.5/ : poloidal length, x-point to outer strike point (m)
  real(kind(1.0D0)) :: plsepo = 1.5D0
  !+ad_vars  ppdivr : peak heat load at plate (with radiation) (MW/m**2)
  real(kind(1.0D0)) :: ppdivr = 0.0D0
  !+ad_vars  prn1 /0.285/ : n-scrape-off / n-average plasma
  real(kind(1.0D0)) :: prn1 = 0.285D0
  !+ad_vars  ptpdiv : peak temperature at the plate (eV)
  real(kind(1.0D0)) :: ptpdiv = 0.0D0
  !+ad_vars  rconl : connection length ratio, outer side
  real(kind(1.0D0)) :: rconl = 0.0D0
  !+ad_vars  rlclolcn : ratio of collision length / connection length
  real(kind(1.0D0)) :: rlclolcn = 0.0D0
  !+ad_vars  rlenmax /0.5/ : maximum value for length ratio (rlclolcn) (eqn.22)
  real(kind(1.0D0)) :: rlenmax = 0.5D0
  !+ad_vars  rsrd : effective separatrix/divertor radius ratio
  real(kind(1.0D0)) :: rsrd = 0.0D0
  !+ad_vars  rstrko : outer strike point radius (m)
  real(kind(1.0D0)) :: rstrko = 0.0D0
  !+ad_vars  tconl : main plasma connection length (m)
  real(kind(1.0D0)) :: tconl = 0.0D0
  !+ad_vars  tdiv : temperature at divertor (eV)
  real(kind(1.0D0)) :: tdiv = 0.0D0
  !+ad_vars  tsep : temperature at the separatrix (eV)
  real(kind(1.0D0)) :: tsep = 0.0D0
  !+ad_vars  xparain /2.1D3/ : parallel heat transport coefficient (m**2/s)
  real(kind(1.0D0)) :: xparain = 2.1D3
  !+ad_vars  xpertin /2.0/ : perpendicular heat transport coefficient (m**2/s)
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
  !+ad_desc  wall, blanket and shield components. It is derived from
  !+ad_desc  <CODE>include</CODE> files <CODE>fwblsh.h90</CODE> and
  !+ad_desc  <CODE>blanket.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  bktlife : blanket lifetime (years)
  real(kind(1.0D0)) :: bktlife = 0.0D0
  !+ad_vars  coolmass : Mass of water coolant (in shield, blanket,
  !+ad_varc             first wall, divertor) (kg)
  real(kind(1.0D0)) :: coolmass = 0.0D0
  !+ad_vars  cryomass : cryostat mass (kg)
  real(kind(1.0D0)) :: cryomass = 0.0D0
  !+ad_vars  denstl /7800.0/ : density of steel (kg/m**3)
  real(kind(1.0D0)) :: denstl = 7800.0D0
  !+ad_vars  dewmkg : Dewar mass (kg)
  real(kind(1.0D0)) :: dewmkg = 0.0D0
  !+ad_vars  emult /1.27/ : energy multiplication in blanket and shield
  real(kind(1.0D0)) :: emult = 1.27D0
  !+ad_vars  fblbe /0.6/ : beryllium fraction of blanket
  real(kind(1.0D0)) :: fblbe = 0.6D0
  !+ad_vars  fblli /0.0/ : lithium fraction of blanket
  real(kind(1.0D0)) :: fblli = 0.0D0
  !+ad_vars  fblli2o /0.08/ : lithium oxide fraction of blanket
  real(kind(1.0D0)) :: fblli2o = 0.08D0
  !+ad_vars  fbllipb /0.68/ : lithium lead fraction of blanket
  real(kind(1.0D0)) :: fbllipb = 0.68D0
  !+ad_vars  fblss /0.07/ : stainless steel fraction of blanket
  real(kind(1.0D0)) :: fblss = 0.07D0
  !+ad_vars  fblvd /0.0/ : vanadium fraction of blanket
  real(kind(1.0D0)) :: fblvd = 0.0D0
  !+ad_vars  fhole /0.15/ : hole fraction of the 1st wall - that neutrons see
  real(kind(1.0D0)) :: fhole = 0.15D0
  !+ad_vars  fvolbi /1.0/ : fudge factor for inner blanket volume
  real(kind(1.0D0)) :: fvolbi = 1.0D0
  !+ad_vars  fvolbo /0.75/ : fudge factor for outer blanket volume
  real(kind(1.0D0)) :: fvolbo = 0.75D0
  !+ad_vars  fvolcry /1.4/ : fudge factor for cryostat volume
  real(kind(1.0D0)) :: fvolcry = 1.4D0
  !+ad_vars  fvoldw /1.4/ : fudge factor for dewar
  real(kind(1.0D0)) :: fvoldw = 1.4D0
  !+ad_vars  fvolsi /0.64/ : fudge factor for inner shield volume
  real(kind(1.0D0)) :: fvolsi = 0.64D0
  !+ad_vars  fvolso /0.64/ : fudge factor for outer shield volume
  real(kind(1.0D0)) :: fvolso = 0.64D0
  !+ad_vars  fwclfr /0.15/ : first wall coolant fraction
  !+ad_varc                  (calculated if lpulse=1)
  real(kind(1.0D0)) :: fwclfr = 0.15D0
  !+ad_vars  fwmass : first wall mass (kg)
  real(kind(1.0D0)) :: fwmass = 0.0D0
  !+ad_vars  pnucblkt : nuclear heating in the blanket (MW)
  real(kind(1.0D0)) :: pnucblkt = 0.0D0
  !+ad_vars  pnuccp : nuclear heating in the centrepost (MW)
  real(kind(1.0D0)) :: pnuccp = 0.0D0
  !+ad_vars  pnucloss : nuclear heating lost via holes (MW)
  real(kind(1.0D0)) :: pnucloss = 0.0D0
  !+ad_vars  pnucshld : nuclear heating in the shield (MW)
  real(kind(1.0D0)) :: pnucshld = 0.0D0
  !+ad_vars  ptfnuc : nuclear heating in the TF coil (MW)
  real(kind(1.0D0)) :: ptfnuc = 0.0D0
  !+ad_vars  vdewex : external dewar volume (m**3)
  real(kind(1.0D0)) :: vdewex = 0.0D0
  !+ad_vars  vdewin : internal dewar volume (m**3)
  real(kind(1.0D0)) :: vdewin = 0.0D0
  !+ad_vars  vfblkt /0.25/ : coolant void fraction in blanket
  real(kind(1.0D0)) :: vfblkt = 0.25D0
  !+ad_vars  vfshld /0.25/ : coolant void fraction in shield
  real(kind(1.0D0)) :: vfshld = 0.25D0
  !+ad_vars  volblkt : volume of blanket (m**3)
  real(kind(1.0D0)) :: volblkt = 0.0D0
  !+ad_vars  volblkti : volume of inboard blanket (m**3)
  real(kind(1.0D0)) :: volblkti = 0.0D0
  !+ad_vars  volblkto : volume of outboard blanket (m**3)
  real(kind(1.0D0)) :: volblkto = 0.0D0
  !+ad_vars  volshld : volume of shield (m**3)
  real(kind(1.0D0)) :: volshld = 0.0D0
  !+ad_vars  whtblbe : mass of blanket - Be part (kg)
  real(kind(1.0D0)) :: whtblbe = 0.0D0
  !+ad_vars  whtblkt : mass of blanket (kg)
  real(kind(1.0D0)) :: whtblkt = 0.0D0
  !+ad_vars  whtblli : mass of blanket - Li part (kg)
  real(kind(1.0D0)) :: whtblli = 0.0D0
  !+ad_vars  whtblss : mass of blanket - stainless steel part (kg)
  real(kind(1.0D0)) :: whtblss = 0.0D0
  !+ad_vars  whtblvd : mass of blanket - Vanadium part (kg)
  real(kind(1.0D0)) :: whtblvd = 0.0D0
  !+ad_vars  whtshld : mass of shield (kg)
  real(kind(1.0D0)) :: whtshld = 0.0D0
  !+ad_vars  wpenshld : mass of the penetration shield (kg)
  real(kind(1.0D0)) :: wpenshld = 0.0D0
  !+ad_vars  wtblli2o : mass of blanket - Li_2O part (kg)
  real(kind(1.0D0)) :: wtblli2o = 0.0D0
  !+ad_vars  wtbllipb : mass of blanket - Li-Pb part (kg)
  real(kind(1.0D0)) :: wtbllipb = 0.0D0
  !+ad_vars  wtshldi : mass of inner shield (kg)
  real(kind(1.0D0)) :: wtshldi = 0.0D0
  !+ad_vars  wtshldo : mass of outer shield (kg)
  real(kind(1.0D0)) :: wtshldo = 0.0D0

  !  Following are used in the full thermodynamic blanket model (lblnkt=1)

  !+ad_vars  etacp /0.75/ : condenser isentropic efficiency
  real(kind(1.0D0)) :: etacp = 0.75D0
  !+ad_vars  etafp /0.75/ : feed water pumps' isentropic efficiency
  real(kind(1.0D0)) :: etafp = 0.75D0
  !+ad_vars  etahp /0.85/ : high pressure turbine isentropic efficiency
  real(kind(1.0D0)) :: etahp = 0.85D0
  !+ad_vars  etainp /0.85/ : intermediate pressure turbine isentropic efficiency
  real(kind(1.0D0)) :: etainp = 0.85D0
  !+ad_vars  etalp /0.85/ : low pressure turbine isentropic efficiency
  real(kind(1.0D0)) :: etalp = 0.85D0
  !+ad_vars  fkblkt /1.0/ : blanket elongation / plasma elongation
  real(kind(1.0D0)) :: fkblkt = 1.0D0
  !+ad_vars  pc /0.005/ : low pressure turbine outlet pressure (MPa)
  real(kind(1.0D0)) :: pc = 0.005D0
  !+ad_vars  ph /8.6/ : high pressure turbine inlet pressure (MPa)
  real(kind(1.0D0)) :: ph = 8.6D0
  !+ad_vars  pin /0.2/ : low pressure turbine inlet pressure (MPa)
  real(kind(1.0D0)) :: pin = 0.2D0
  !+ad_vars  pr /1.0/ : intermediate pressure turbine inlet pressure (MPa)
  real(kind(1.0D0)) :: pr = 1.0D0
  !+ad_vars  sgeff /1.0/ : steam generator effectiveness
  real(kind(1.0D0)) :: sgeff = 1.0D0
  !+ad_vars  xdi /2.0/ : inner cooling channel diameter (cm)
  real(kind(1.0D0)) :: xdi = 2.0D0
  !+ad_vars  xdo /2.4/ : outer cooling channel diameter (cm)
  real(kind(1.0D0)) :: xdo = 2.4D0
  !+ad_vars  xpf /8.6/ : blanket coolant inlet pressure (MPa)
  real(kind(1.0D0)) :: xpf = 8.6D0
  !+ad_vars  xtb /350.0/ : maximum blanket temperature (C)
  real(kind(1.0D0)) :: xtb = 350.0D0
  !+ad_vars  xtfi /200.0/ : inlet coolant temperature (C)
  real(kind(1.0D0)) :: xtfi = 200.0D0
  !+ad_vars  xtfo /300.0/ : outlet coolant temperature (C)
  real(kind(1.0D0)) :: xtfo = 300.0D0

  !+ad_vars  astr /2/ : Switch for blanket cooling channel geometry (lblnkt=1):
  !+ad_varc             = 1 circular cross section;
  !+ad_varc             = 2 annular cross section
  integer :: astr = 2
  !+ad_vars  bstr /1/ : Switch for blanket boundary condition (lblnkt=1):
  !+ad_varc             = 1 coolant outlet temperature fixed;
  !+ad_varc             = 2 maximum blanket temperature fixed
  integer :: bstr = 1
  !+ad_vars  costr /2/ : Switch for blanket coolant material (lblnkt=1):
  !+ad_varc              = 1 Gaseous helium coolant;
  !+ad_varc              = 2 Pressurized water coolant
  integer :: costr = 2
  !+ad_vars  estr /1/ : Switch for cooling channel orientation (lblnkt=1):
  !+ad_varc             = 1 radially orientated;
  !+ad_varc             = 2 poloidally orientated
  integer :: estr = 1
  !+ad_vars  lblnkt /1/ : Switch for blanket model:
  !+ad_varc               = 0 original model;
  !+ad_varc               = 1 full thermodynamic model
  integer :: lblnkt = 1
  !+ad_vars  nipfwh /1/ : Number of intermediate pressure feed water heater pumps
  integer :: nipfwh = 1
  !+ad_vars  nlpfwh /1/ : Number of low pressure feed water heater pumps
  integer :: nlpfwh = 1
  !+ad_vars  smstr /1/ : Switch for blanket material (lblnkt=1):
  !+ad_varc              = 1 Li2O/Be (solid blanket);
  !+ad_varc              = 2 LiPb/Li (liquid blanket)
  integer :: smstr = 1

end module fwbs_variables

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
  !+ad_desc  It is derived from <CODE>include</CODE> files
  !+ad_desc  <CODE>pfcoil.h90</CODE> and <CODE>vltcom.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  ngrpmx : maximum number of groups of PF coils
  integer, parameter :: ngrpmx = 8
  !+ad_vars  nclsmx : maximum number of PF coils in a given group
  integer, parameter :: nclsmx = 2
  !+ad_vars  nptsmx : maximum number of points across the midplane of the
  !+ad_varc           plasma at which the field from the PF coils is fixed
  integer, parameter :: nptsmx = 32
  !+ad_vars  nfixmx : maximum number of fixed current PF coils
  integer, parameter :: nfixmx = 64

  integer, parameter :: ngc = ngrpmx*nclsmx
  integer, parameter :: ngc2 = ngc+2

  !+ad_vars  ac1oh /0.0/ : OH coil cable conduit area (m**2)
  real(kind(1.0D0)) :: ac1oh = 0.0D0
  !+ad_vars  acsoh /3.0D-4/ : conduit conductor cross section (m**2)
  real(kind(1.0D0)) :: acsoh = 3.0D-4
  !+ad_vars  alfapf /5.0D-10/ : smoothing parameter used in BOP PF coil current calculation
  real(kind(1.0D0)) :: alfapf = 5.0D-10
  !+ad_vars  bmaxoh : B-max in OH coil at EOF (T)
  real(kind(1.0D0)) :: bmaxoh = 0.0D0
  !+ad_vars  bmaxoh0 : B-max in OH coil at BOP (T)
  real(kind(1.0D0)) :: bmaxoh0 = 0.0D0
  !+ad_vars  bpf(ngc2) : peak field at coil i (T)
  real(kind(1.0D0)), dimension(ngc2) :: bpf = 0.0D0
  !+ad_vars  cohbof : OH coil overall current density at BOF (A/m**2)
  real(kind(1.0D0)) :: cohbof = 0.0D0
  !+ad_vars  cohbop : OH coil overall current density at BOP (A/m**2)
  real(kind(1.0D0)) :: cohbop = 0.0D0
  !+ad_vars  coheof /1.85D7/ : OH coil overall current density at EOF (A/m**2)
  !+ad_varc                    (iteration variable 37)
  real(kind(1.0D0)) :: coheof = 1.85D7
  !+ad_vars  cpt(ngc2,6) : current per turn in coil i at time j (A)
  real(kind(1.0D0)), dimension(ngc2,6) :: cpt = 0.0D0
  !+ad_vars  cptdin(ngc2) /4.0D4/: current per turn input for PF coil i (A)
  real(kind(1.0D0)), dimension(ngc2) :: cptdin = 4.0D4
  !+ad_vars  curpfb(ngc2) : work array
  real(kind(1.0D0)), dimension(ngc2) :: curpfb = 0.0D0
  !+ad_vars  curpff(ngc2) : work array
  real(kind(1.0D0)), dimension(ngc2) :: curpff = 0.0D0
  !+ad_vars  curpfs(ngc2) : work array
  real(kind(1.0D0)), dimension(ngc2) :: curpfs = 0.0D0
  !+ad_vars  fcohbof /0.9/ : = cohbof / coheof
  real(kind(1.0D0)) :: fcohbof = 0.9D0
  !+ad_vars  fcohbop /0.9/ : = cohbop / coheof (iteration variable 41)
  real(kind(1.0D0)) :: fcohbop = 0.9D0
  !+ad_vars  fcuoh /0.4/ : copper fraction of conductor in OH coil cable
  real(kind(1.0D0)) :: fcuoh = 0.4D0
  !+ad_vars  ipfloc(ngc) /1,2,3/ : switch for locating scheme of PF coil group i:
  !+ad_varc           = 1, PF coil on top of OH coil;
  !+ad_varc           = 2, PF coil on top of TF coil;
  !+ad_varc           = 3, PF coil outside of TF coil
  integer, dimension(ngc) :: ipfloc = (/1,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  !+ad_vars  ipfres /0/ : switch for PF coil type:
  !+ad_varc               = 0 superconducting PF coils;
  !+ad_varc               = 1 resistive PF coils
  integer :: ipfres = 0
  !+ad_vars  isumatpf /1/ : switch for superconductor material in PF coils:
  !+ad_varc                 = 1 binary Nb3Sn;
  !+ad_varc                 = 2 ternary Nb3Sn;
  !+ad_varc                 = 3 NbTi
  integer :: isumatpf = 1
  !+ad_vars  ncirt : number of PF coils (including OH coil and plasma)
  integer :: ncirt = 0
  !+ad_vars  ncls(ngrpmx+2) /2,2,2,1/ : number of PF coils in group j
  integer, dimension(ngrpmx+2) :: ncls = (/2,2,2,1,0,0,0,0,0,0/)
  !+ad_vars  nfxfh /7/ : number of coils the top and bottom of the OH coil
  !+ad_varc              should be broken into during scaling (5 - 10 is good)
  integer :: nfxfh = 7
  !+ad_vars  ngrp /3/ : number of groups of PF coils.
  !+ad_varc             Symmetric coil pairs should all be in the same group
  integer :: ngrp = 3
  !+ad_vars  nohc : number of PF coils (excluding the OH coil) + 1
  integer :: nohc = 0
  !+ad_vars  ohhghf /0.71/ : OH coil height / TF coil height
  real(kind(1.0D0)) :: ohhghf = 0.71D0
  !+ad_vars  pfclres /2.5D-8/ : PF coil resistivity if ipfres=1 (Ohm-m)
  real(kind(1.0D0)) :: pfclres = 2.5D-8
  !+ad_vars  powohres : OH coil resistive power during flattop (W)
  real(kind(1.0D0)) :: powohres = 0.0D0
  !+ad_vars  powpfres : total PF coil resistive losses during flattop (W)
  real(kind(1.0D0)) :: powpfres = 0.0D0
  !+ad_vars  ra(ngc2) : inner radius of coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: ra = 0.0D0
  !+ad_vars  rb(ngc2) : outer radius of coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: rb = 0.0D0
  !+ad_vars  ric(ngc2) : peak current in coil i (MA-turns)
  real(kind(1.0D0)), dimension(ngc2) :: ric = 0.0D0
  !+ad_vars  rjconpf(ngc2) /3.0D7/ : average current density of PF coil i (A/m**2)
  real(kind(1.0D0)), dimension(ngc2) :: rjconpf = 3.0D7
  !+ad_vars  rjohc : allowable OH coil current density at EOF (A/m**2)
  real(kind(1.0D0)) :: rjohc = 0.0D0
  !+ad_vars  rjohc0 : allowable OH coil current density at BOP (A/m**2)
  real(kind(1.0D0)) :: rjohc0 = 0.0D0
  !+ad_vars  rjpfalw(ngc2) : allowable current density of PF coil i (A/m**2)
  real(kind(1.0D0)), dimension(ngc2) :: rjpfalw = 0.0D0
  !+ad_vars  rohc : radius to the centre of the OH coil (m)
  real(kind(1.0D0)) :: rohc = 0.0D0
  !+ad_vars  routr /1.5/ : distance (m) from outer TF coil leg to centre of ipfloc=3 PF coils
  real(kind(1.0D0)) :: routr = 1.5D0
  !+ad_vars  rpf(ngc2) : radius of PF coil i (m)
  real(kind(1.0D0)), dimension(ngc2) :: rpf = 0.0D0
  !+ad_vars  rpf1 /0.0/ : offset (m) of radial position of ipfloc=1 PF coils
  !+ad_varc               from being directly above the OH coil
  real(kind(1.0D0)) :: rpf1 = 0.0D0
  !+ad_vars  rpf2 /-1.63/ : offset of radial position of ipfloc=2 PF coils
  !+ad_varc                 from being at rmajor (offset = rpf2*triang*rminor)
  real(kind(1.0D0)) :: rpf2 = -1.63D0
  !+ad_vars  sccufac /0.0188/ : ratio of superconductor to copper in PF/OH coil
  !+ad_varc                     cable at a magnetic field of 1T
  real(kind(1.0D0)) :: sccufac = 0.0188D0
  !+ad_vars  sigpfalw /335.0/ : allowable stress in PF/OH coils (MPa)
  real(kind(1.0D0)) :: sigpfalw = 335.0D0
  !+ad_vars  sxlg(ngc2,ngc2) : mutual inductance matrix (H)
  real(kind(1.0D0)), dimension(ngc2,ngc2) :: sxlg = 0.0D0
  !+ad_vars  turns(ngc2) : number of turns in PF coil i
  real(kind(1.0D0)), dimension(ngc2) :: turns = 0.0D0
  !+ad_vars  vf(ngc2) /0.3/ : void fraction of PF coil i
  real(kind(1.0D0)), dimension(ngc2) :: vf = 0.3D0
  !+ad_vars  vfohc /0.4/ : OH coil void fraction for coolant
  real(kind(1.0D0)) :: vfohc = 0.4D0
  !+ad_vars  vsbn : total flux swing available for burn (Wb)
  real(kind(1.0D0)) :: vsbn = 0.0D0
  !+ad_vars  vsefbn : flux swing from PF coils for burn (Wb)
  real(kind(1.0D0)) :: vsefbn = 0.0D0
  !+ad_vars  vsefsu : flux swing from PF coils for startup (Wb)
  real(kind(1.0D0)) :: vsefsu = 0.0D0
  !+ad_vars  vseft : total flux swing from PF coils (Wb)
  real(kind(1.0D0)) :: vseft = 0.0D0
  !+ad_vars  vsoh : total flux swing from the OH coil (Wb)
  real(kind(1.0D0)) :: vsoh = 0.0D0
  !+ad_vars  vsohbn : OH coil flux swing for burn (Wb)
  real(kind(1.0D0)) :: vsohbn = 0.0D0
  !+ad_vars  vsohsu : OH coil flux swing for startup (Wb)
  real(kind(1.0D0)) :: vsohsu = 0.0D0
  !+ad_vars  vssu : total flux swing for startup (Wb)
  real(kind(1.0D0)) :: vssu = 0.0D0
  !+ad_vars  vstot : total flux swing for pulse (Wb)
  real(kind(1.0D0)) :: vstot = 0.0D0
  !+ad_vars  waves(ngc2,6) : used in current waveform of PF/OH coils
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
  !+ad_vars  zref(ngrpmx) /../ : (height of coil group j) / minor radius,
  !+ad_vars                      for groups with ipfloc = 3
  real(kind(1.0D0)), dimension(ngrpmx) :: zref = (/3.6D0, 1.2D0, 2.5D0, &
       1.0D0, 1.0D0, 1.0D0, 1.0D0, 1.0D0/)

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
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>tfcoil.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  acasetf : total external case area (inboard) (m**2)
  real(kind(1.0D0)) :: acasetf = 0.0D0
  !+ad_vars  acndttf : area of the cable conduit (m**2)
  real(kind(1.0D0)) :: acndttf = 0.0D0
  !+ad_vars  acond : conductor area (winding pack) (m**2)
  real(kind(1.0D0)) :: acond = 0.0D0
  !+ad_vars  acstf : internal area of the cable space (m**2)
  real(kind(1.0D0)) :: acstf = 0.0D0
  !+ad_vars  aiwp : winding pack insulation area (m**2)
  real(kind(1.0D0)) :: aiwp = 0.0D0
  !+ad_vars  alstrtf : allowable stress in TF coil (Pa)
  real(kind(1.0D0)) :: alstrtf = 0.0D0
  !+ad_vars  arealeg : outboard TF leg area (m**2)
  real(kind(1.0D0)) :: arealeg = 0.0D0
  !+ad_vars  aspcstf /1.0/ : TF conductor cable aspect ratio (radial/toroidal)
  real(kind(1.0D0)) :: aspcstf = 1.0D0
  !+ad_vars  aswp : winding pack structure area (m**2)
  real(kind(1.0D0)) :: aswp = 0.0D0
  !+ad_vars  avwp : winding pack void (He coolant) area (m**2)
  real(kind(1.0D0)) :: avwp = 0.0D0
  !+ad_vars  bcritsc /24.0/ : critical field for superconductor (isumattf=4 or 5)
  real(kind(1.0D0)) :: bcritsc = 24.0D0
  !+ad_vars  bmaxtf : peak field at TF coil (T)
  real(kind(1.0D0)) :: bmaxtf = 0.0D0
  !+ad_vars  bmaxtfrp : peak field at conductor with ripple (T)
  real(kind(1.0D0)) :: bmaxtfrp = 0.0D0
  !+ad_vars  borev : vertical inner bore of TF coil (m)
  real(kind(1.0D0)) :: borev = 0.0D0
  !+ad_vars  casestr : case strain
  real(kind(1.0D0)) :: casestr = 0.0D0
  !+ad_vars  casfact /4.0/ : TF coil case thickness factor
  !+ad_varc                  = (average outboard / average inboard thickness)
  real(kind(1.0D0)) :: casfact = 4.0D0
  !+ad_vars  casthi /0.05/ : TF coil case inner (plasma side) thickness (m)
  real(kind(1.0D0)) :: casthi = 0.05D0
  !+ad_vars  casths /0.07/ : TF coil sidewall case thickness (m)
  real(kind(1.0D0)) :: casths = 0.07D0
  !+ad_vars  cdtfleg /1.0D6/ : TF leg overall current density (A/m**2)
  !+ad_varc                    (iteration variable 24)
  real(kind(1.0D0)) :: cdtfleg = 1.0D6
  !+ad_vars  cforce : centering force on inner leg (per coil) (N/m)
  real(kind(1.0D0)) :: cforce = 0.0D0
  !+ad_vars  cph2o : specific heat of water (= 4180 J/kg/K)
  real(kind(1.0D0)) :: cph2o = 4180.0D0
  !+ad_vars  cpttf /3.79D4/ : current per turn (A) (single turn legs)
  real(kind(1.0D0)) :: cpttf = 3.79D4
  !+ad_vars  csutf /1.4D9/ : ultimate strength of case (Pa)
  real(kind(1.0D0)) :: csutf = 1.4D9
  !+ad_vars  csytf /8.25D8/ : yield strength of case (Pa)
  real(kind(1.0D0)) :: csytf = 8.25D8
  !+ad_vars  dcase /8000.0/ : density of coil case (kg/m**3)
  real(kind(1.0D0)) :: dcase = 8000.0D0
  !+ad_vars  dcond(5) /9000.0/ : density of superconductor (kg/m**3)
  real(kind(1.0D0)), dimension(5) :: dcond = 9000.0D0
  !+ad_vars  dcopper /8900.0/ : density of copper (kg/m**3)
  real(kind(1.0D0)) :: dcopper = 8900.0D0
  !+ad_vars  deflect : TF coil deflection at full field (m)
  real(kind(1.0D0)) :: deflect = 0.0D0
  !+ad_vars  denh2o : density of water (= 985 kg/m**3)
  real(kind(1.0D0)) :: denh2o = 985.0D0
  !+ad_vars  drtop /0.0/ : centrepost taper maximum radius adjustment (m)
  real(kind(1.0D0)) :: drtop = 0.0D0
  !+ad_vars  dztop /0.0/ : centrepost taper height adjustment (m)
  real(kind(1.0D0)) :: dztop = 0.0D0
  !+ad_vars  estotf : stored energy in TF coils (GJ)
  real(kind(1.0D0)) :: estotf = 0.0D0
  !+ad_vars  etapump /0.8/ : centrepost coolant pump efficiency
  real(kind(1.0D0)) :: etapump = 0.8D0
  !+ad_vars  eyins /1.5D10/ : insulator Young's modulus (Pa)
  real(kind(1.0D0)) :: eyins = 1.5D10
  !+ad_vars  eyoung(5) : work array used in stress calculation (Pa)
  real(kind(1.0D0)), dimension(5) :: eyoung = 0.0D0
  !+ad_vars  eystl /2.0D11/ : steel case Young's modulus (Pa)
  real(kind(1.0D0)) :: eystl = 2.0D11
  !+ad_vars  eywp /6.6D8/ : winding pack Young's modulus (Pa)
  real(kind(1.0D0)) :: eywp = 6.6D8
  !+ad_vars  farc4tf /0.7/ : factor to size height of point 4 on TF coil
  real(kind(1.0D0)) :: farc4tf = 0.7D0
  !+ad_vars  fcoolcp /0.3/ : coolant fraction of TF coil inner legs
  !+ad_varc                  (iteration variable 23)
  real(kind(1.0D0)) :: fcoolcp = 0.3D0
  !+ad_vars  fcutfsu /0.69/ : copper fraction of cable conductor
  !+ad_varc                   (iteration variable 59)
  real(kind(1.0D0)) :: fcutfsu = 0.69D0
  !+ad_vars  frhocp /1.0/ centrepost resistivity enhancement factor
  real(kind(1.0D0)) :: frhocp = 1.0D0
  !+ad_vars  isumattf /1/ : switch for superconductor material in TF coils:
  !+ad_varc                 = 1 binary Nb3Sn;
  !+ad_varc                 = 2 ternary Nb3Sn;
  !+ad_varc                 = 3 NbTi;
  !+ad_varc                 = 4 generic, but uses Nb3Sn current density model;
  !+ad_varc                 = 5 generic, but uses NbTi current density model
  integer :: isumattf = 1
  !+ad_vars  itfmod /1/ : switch for TF coil magnet model:
  !+ad_varc               = 0 use simple model;
  !+ad_varc               = 1 use complex stress/superconductor models
  integer :: itfmod = 1
  !+ad_vars  itfsup /1/ : switch for TF coil conductor model:
  !+ad_varc               = 0 conventional copper;
  !+ad_varc               = 1 superconductor
  integer :: itfsup = 1
  !+ad_vars  jbus /1.25D6/ : bussing current density (A/m**2)
  real(kind(1.0D0)) :: jbus = 1.25D6
  !+ad_vars  jcrit_model /0/ : switch for binary Nb3Sn critical J model (isumattf=1):
  !+ad_varc                    = 0 use original model;
  !+ad_varc                    = 1 use ITER critical surface model
  integer :: jcrit_model = 0
  !+ad_vars  jcritsc /2.225D10/ : critical current density (A/m**2) for
  !+ad_varc                       superconductor (isumattf=4 or 5)
  real(kind(1.0D0)) :: jcritsc = 2.225D10
  !+ad_vars  jeff(5) : work array used in stress calculation (A/m**2)
  real(kind(1.0D0)), dimension(5) :: jeff = 0.0D0
  !+ad_vars  jwdgcrt : critical current density for winding pack (A/m**2)
  real(kind(1.0D0)) :: jwdgcrt = 0.0D0
  !+ad_vars  jwdgpro : allowable TF coil winding pack current density,
  !+ad_varc            for dump temperature rise protection (A/m**2)
  real(kind(1.0D0)) :: jwdgpro = 0.0D0
  !+ad_vars  jwptf : winding pack current density (A/m**2)
  real(kind(1.0D0)) :: jwptf = 0.0D0
  !+ad_vars  kcp : thermal conductivity of centrepost (= 330 W/m/K)
  real(kind(1.0D0)) :: kcp = 330.0D0
  !+ad_vars  kh2o : thermal conductivity of water (= 0.651 W/m/K)
  real(kind(1.0D0)) :: kh2o = 0.651D0
  !+ad_vars  magnt /2/ : switch for TF coil stress model:
  !+ad_varc              = 1 for FER type configuration with bucking cylinder;
  !+ad_varc              = 2 for NET type wedging; set casfi = 0.0;
  !+ad_varc              = 3 for LLNL type buck on OH coil
  integer :: magnt = 2
  !+ad_vars  muh2o : water dynamic viscosity (= 4.71D-4 kg/m/s)
  real(kind(1.0D0)) :: muh2o = 4.71D-4
  !+ad_vars  ncool : number of centrepost coolant tubes
  real(kind(1.0D0)) :: ncool = 0.0D0
  !+ad_vars  oacdcp /1.4D7/ : overall current density in TF coil inner legs (A/m**2)
  !+ad_varc                   (iteration variable 12)
  real(kind(1.0D0)) :: oacdcp = 1.4D7
  !+ad_vars  poisson /0.3/ : Poisson's ratio for TF stress calculation
  real(kind(1.0D0)) :: poisson = 0.3D0
  !+ad_vars  ppump : centrepost coolant pump power (W)
  real(kind(1.0D0)) :: ppump = 0.0D0
  !+ad_vars  prescp : resistive power in the centrepost (W)
  real(kind(1.0D0)) :: prescp = 0.0D0
  !+ad_vars  ptempalw /200.0/ : maximum peak centrepost temperature (C)
  !+ad_varc                     (constraint equation 44)
  real(kind(1.0D0)) :: ptempalw = 200.0D0
  !+ad_vars  radtf(6) : work array used in stress calculation (m)
  real(kind(1.0D0)), dimension(6) :: radtf = 0.0D0
  !+ad_vars  rbmax : radius of maximum TF B-field (m)
  real(kind(1.0D0)) :: rbmax = 0.0D0
  !+ad_vars  rcool /0.005/ : average radius of coolant channel (m)
  !+ad_varc                  (iteration variable 69)
  real(kind(1.0D0)) :: rcool = 0.005D0
  !+ad_vars  rhocp : TF coil inner leg resistance (Ohm)
  real(kind(1.0D0)) :: rhocp = 0.0D0
  !+ad_vars  rhotfleg : TF coil leg resistance (Ohm)
  real(kind(1.0D0)) :: rhotfleg = 0.0D0
  !+ad_vars  ripmax /5.0/ : maximum peak/average ripple at plasma edge (%)
  real(kind(1.0D0)) :: ripmax = 5.0D0
  !+ad_vars  ripple : peak/average ripple at plasma edge (%)
  real(kind(1.0D0)) :: ripple = 0.0D0
  !+ad_vars  ritfc : total current in TF coil (A)
  real(kind(1.0D0)) :: ritfc = 0.0D0
  !+ad_vars  rjtfsual : allowable overall coil current density (A/m**2)
  real(kind(1.0D0)) :: rjtfsual = 0.0D0
  !+ad_vars  rnltf : number of TF turns/pancake (radial direction)
  real(kind(1.0D0)) :: rnltf = 0.0D0
  !+ad_vars  sigrad : radial TF coil stress (MPa)
  real(kind(1.0D0)) :: sigrad = 0.0D0
  !+ad_vars  sigrcon : radial stress in the cable conduit (Pa)
  real(kind(1.0D0)) :: sigrcon = 0.0D0
  !+ad_vars  sigrtf(5) : radial stress in TF coil regions (Pa)
  real(kind(1.0D0)), dimension(5) :: sigrtf = 0.0D0
  !+ad_vars  sigtan : transverse TF coil stress (MPa)
  real(kind(1.0D0)) :: sigtan = 0.0D0
  !+ad_vars  sigtcon : tangential stress in the cable conduit (Pa)
  real(kind(1.0D0)) :: sigtcon = 0.0D0
  !+ad_vars  sigttf(5) : tangential stress in TF coil regions (Pa)
  real(kind(1.0D0)), dimension(5) :: sigttf = 0.0D0
  !+ad_vars  sigver : vertical TF coil stress (MPa)
  real(kind(1.0D0)) :: sigver  = 0.0D0
  !+ad_vars  sigvert : vertical tensile stress in TF coil (Pa)
  real(kind(1.0D0)) :: sigvert = 0.0D0
  !+ad_vars  strncon /-0.005/ : strain in superconductor material
  real(kind(1.0D0)) :: strncon = -0.005D0
  !+ad_vars  strtf1 : Von Mises stress in TF cable conduit (Pa)
  real(kind(1.0D0)) :: strtf1 = 0.0D0
  !+ad_vars  strtf2 : Von Mises stress in TF coil case (Pa)
  real(kind(1.0D0)) :: strtf2 = 0.0D0
  !+ad_vars  tcoolin /40.0/ : centrepost coolant inlet temperature (C)
  real(kind(1.0D0)) :: tcoolin = 40.0D0
  !+ad_vars  tcpav /100.0/ : average temp of TF coil inner leg conductor (C)
  !+ad_varc                  (iteration variable 20)
  real(kind(1.0D0)) :: tcpav = 100.0D0
  !+ad_vars  tcpav2 : centrepost average temperature (C) (for consistency)
  real(kind(1.0D0)) :: tcpav2 = 0.0D0
  !+ad_vars  tcpmax : peak centrepost temperature (C)
  real(kind(1.0D0)) :: tcpmax = 0.0D0
  !+ad_vars  tcritsc /16.0/ : critical temperature (K) for superconductor
  !+ad_varc                   (isumattf = 4 or 5)
  real(kind(1.0D0)) :: tcritsc = 16.0D0
  !+ad_vars  tdmptf /10.0/ : dump time for TF coil (s)
  !+ad_varc                  (iteration variable 56)
  real(kind(1.0D0)) :: tdmptf = 10.0D0
  !+ad_vars  tfareain : area of inboard midplane TF legs (m**2)
  real(kind(1.0D0)) :: tfareain = 0.0D0
  !+ad_vars  tfboreh : TF coil horizontal bore (m)
  real(kind(1.0D0)) :: tfboreh = 0.0D0
  !+ad_vars  tfbusl : TF coil bus length (m)
  real(kind(1.0D0)) :: tfbusl = 0.0D0
  !+ad_vars  tfbusmas : TF coil bus mass (kg)
  real(kind(1.0D0)) :: tfbusmas = 0.0D0
  !+ad_vars  tfckw :  TF coil peak voltage in dump (kV)
  real(kind(1.0D0)) :: tfckw = 0.0D0
  !+ad_vars  tfcmw : peak power per TF power supply (MW)
  real(kind(1.0D0)) :: tfcmw = 0.0D0
  !+ad_vars  tfcpmw : peak resistive TF coil inner leg power (MW)
  real(kind(1.0D0)) :: tfcpmw = 0.0D0
  !+ad_vars  tficrn : TF coil half-width - inner bore (m)
  real(kind(1.0D0)) :: tficrn = 0.0D0
  !+ad_vars  tfind : TF coil inductance (H)
  real(kind(1.0D0)) :: tfind = 0.0D0
  !+ad_vars  tflegmw : TF coil outer leg resistive power (MW)
  real(kind(1.0D0)) :: tflegmw = 0.0D0
  !+ad_vars  tflegres /2.5D-8/ : resistivity of a TF coil leg (Ohm-m)
  real(kind(1.0D0)) :: tflegres = 2.5D-8
  !+ad_vars  tfleng : TF coil circumference (m)
  real(kind(1.0D0)) :: tfleng = 0.0D0
  !+ad_vars  tfno : number of TF coils
  real(kind(1.0D0)) :: tfno = 16.0D0
  !+ad_vars  tfocrn : TF coil half-width - outer bore (m)
  real(kind(1.0D0)) :: tfocrn = 0.0D0
  !+ad_vars  tfsai : area of the inboard TF coil legs
  real(kind(1.0D0)) :: tfsai = 0.0D0
  !+ad_vars  tfsao : area of the outboard TF coil legs
  real(kind(1.0D0)) :: tfsao = 0.0D0
  !+ad_vars  tftmp /4.5/ : peak TF coil He coolant temperature (K)
  real(kind(1.0D0)) :: tftmp = 4.5D0
  !+ad_vars  thicndut /8.0D-4/ : conduit insulation thickness (m)
  real(kind(1.0D0)) :: thicndut = 8.0D-4
  !+ad_vars  thkcas /0.3/ : external case thickness for superconductor (m)
  !+ad_varc                 (iteration variable 57)
  real(kind(1.0D0)) :: thkcas = 0.3D0
  !+ad_vars  thkwp : radial thickness of winding pack (m)
  real(kind(1.0D0)) :: thkwp = 0.0D0
  !+ad_vars  thwcndut /3.0D-3/ : TF coil conduit case thickness (m)
  !+ad_varc                      (iteration variable 58)
  real(kind(1.0D0)) :: thwcndut = 3.0D-3
  !+ad_vars  tinstf /0.01/ : ground wall insulation thickness (m)
  real(kind(1.0D0)) :: tinstf = 0.01D0
  !+ad_vars  tmargmin /2.5/ : minimum allowable temperature margin (K)
  !+ad_varc                   (iteration variable 55)
  real(kind(1.0D0)) :: tmargmin = 2.5D0
  !+ad_vars  tmargtf :  TF coil temperature margin (K)
  real(kind(1.0D0)) :: tmargtf = 0.0D0
  !+ad_vars  tmaxpro /150.0/ : maximum temp rise during a quench for protection (K)
  real(kind(1.0D0)) :: tmaxpro = 150.0D0
  !+ad_vars  tmpcry /4.5/ : cryostat temperature for superconductor analysis (K)
  real(kind(1.0D0)) :: tmpcry = 4.5D0
  !+ad_vars  turnstf : number of turns per TF coil
  real(kind(1.0D0)) :: turnstf = 0.0D0
  !+ad_vars  vcool /20.0/ : max centrepost coolant flow speed at midplane (m/s)
  !+ad_varc                 (iteration variable 70)
  real(kind(1.0D0)) :: vcool = 20.0D0
  !+ad_vars  vdalw /20.0/ : max voltage across TF coil during quench (kV)
  !+ad_varc                 (iteration variable 52)
  real(kind(1.0D0)) :: vdalw = 20.0D0
  !+ad_vars  vforce : vertical separating force on inner leg/coil (N)
  real(kind(1.0D0)) :: vforce = 0.0D0
  !+ad_vars  vftf /0.4/ : coolant fraction of TF coil leg
  real(kind(1.0D0)) :: vftf = 0.4D0
  !+ad_vars  volcp : total volume of TF coil inner legs (m**3)
  real(kind(1.0D0)) :: volcp = 0.0D0
  !+ad_vars  voltfleg : volume of each TF coil outer leg (m**3)
  real(kind(1.0D0)) :: voltfleg = 0.0D0
  !+ad_vars  vtfkv : peak TF coil voltage (kV)
  real(kind(1.0D0)) :: vtfkv = 0.0D0
  !+ad_vars  vtfskv : voltage across a TF coil during quench (kV)
  real(kind(1.0D0)) :: vtfskv = 0.0D0
  !+ad_vars  whtcas : mass per coil of external case (kg)
  real(kind(1.0D0)) :: whtcas = 0.0D0
  !+ad_vars  whtcon : TF coil conductor cable mass per coil (kg)
  real(kind(1.0D0)) :: whtcon = 0.0D0
  !+ad_vars  whtconcu : copper mass in TF coil conductor cable (kg/coil)
  real(kind(1.0D0)) :: whtconcu = 0.0D0
  !+ad_vars  whtconsc : superconductor mass in TF coil conductor cable (kg/coil)
  real(kind(1.0D0)) :: whtconsc = 0.0D0
  !+ad_vars  whtconsh : steel conduit mass in TF coil conductor cable (kg/coil)
  real(kind(1.0D0)) :: whtconsh = 0.0D0
  !+ad_vars  whtcp : mass of TF coil inner legs (kg)
  real(kind(1.0D0)) :: whtcp = 0.0D0
  !+ad_vars  whttf : total mass of the TF coils (kg)
  real(kind(1.0D0)) :: whttf = 0.0D0
  !+ad_vars  whttflgs : mass of the TF coil legs (kg)
  real(kind(1.0D0)) :: whttflgs = 0.0D0
  !+ad_vars  wpvf /0.0/ : inter-turn void fraction of winding pack
  real(kind(1.0D0)) :: wpvf = 0.0D0
  !+ad_vars  wtbc : bucking cylinder mass (kg)
  real(kind(1.0D0)) :: wtbc = 0.0D0
  !+ad_vars  wwp1 : width of first step of winding pack (m)
  real(kind(1.0D0)) :: wwp1 = 0.0D0
  !+ad_vars  wwp2 : width of second step of winding pack (m)
  real(kind(1.0D0)) :: wwp2 = 0.0D0

  !  Superconducting TF coil shape parameters (see also farc4tf)
  !  The TF inner surface top half is approximated by circular arcs
  !  Arc 1 goes through points 1 and 2 on the inner surface. Arc 2
  !  goes through points 2-3, etc.

  !+ad_vars  dthet(5) : angle of arc i (rad)
  real(kind(1.0D0)), dimension(5) :: dthet = 0.0D0
  !+ad_vars  radctf(5) : radius of arc i (m)
  real(kind(1.0D0)), dimension(5) :: radctf = 0.0D0
  !+ad_vars  xarc(5) : x location of arc point i on surface (m)
  real(kind(1.0D0)), dimension(5) :: xarc = 0.0D0
  !+ad_vars  xctfc(5) : x location of arc centre i (m)
  real(kind(1.0D0)), dimension(5) :: xctfc = 0.0D0
  !+ad_vars  yarc(5) : y location of arc point i on surface (m)
  real(kind(1.0D0)), dimension(5) :: yarc = 0.0D0
  !+ad_vars  yctfc(5) : y location of arc centre i (m)
  real(kind(1.0D0)), dimension(5) :: yctfc = 0.0D0

end module tfcoil_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module structure_variables

  !+ad_name  tfcoil_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  support structure
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  suppot structure of a fusion power plant.
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>struccom.h90</CODE>.
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

module wibble

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

end module wibble
