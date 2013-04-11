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

  !+ad_vars  icase : description of plant plant model being used
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
  !+ad_hist  17/12/12 PJK Added zfear; modified impfe, cfe0, rnfene, fbfe comments
  !+ad_hist  18/12/12 PJK Added pthrmw(6 to 8)
  !+ad_hist  18/12/12 PJK Added snull; modified idivrt
  !+ad_hist  03/01/13 PJK Removed iculdl
  !+ad_hist  08/01/13 PJK Modified iinvqd, iiter, ires comments
  !+ad_hist  22/01/13 PJK Added two stellarator scaling laws; modified comments
  !+ad_hist  11/04/13 PJK Removed ires, rtpte; changed isc, ifispact default values
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  ipnlaws /38/ FIX : number of energy confinement time scaling laws
  integer, parameter :: ipnlaws = 38

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
  !+ad_varc  <LI> (25)  TITAN (RFP)
       'TITAN RFP               ', &
  !+ad_varc  <LI> (26)  ITER H-97P ELM-free (H-mode)
       'ITER H-97P ELM-free  (H)', &
  !+ad_varc  <LI> (27)  ITER H-97P ELMy (H-mode)
       'ITER H-97P ELMy      (H)', &
  !+ad_varc  <LI> (28)  ITER-96P (L-mode)
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
  !+ad_varc  <LI> (38)  ISS04 (stellarator)</UL>
       'ISS04            (stell)' /)

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
  !+ad_vars  cfe0 /0.0/ : seeded high-Z impurity fraction (n_highZ / n_e)
  !+ad_varc               (iteration variable 43)
  real(kind(1.0D0)) :: cfe0 = 0.0D0
  !+ad_vars  csawth /1.0/ : coeff. for sawteeth effects on burn V-s requirement
  real(kind(1.0D0)) :: csawth = 1.0D0
  !+ad_vars  cvol /1.0/ : multiplying factor times plasma volume (normally=1)
  real(kind(1.0D0)) :: cvol = 1.0D0
  !+ad_vars  dene /1.5D20/ : electron density (/m3) (iteration variable 6)
  real(kind(1.0D0)) :: dene = 1.5D20
  !+ad_vars  deni : fuel ion density (/m3)
  real(kind(1.0D0)) :: deni = 0.0D0
  !+ad_vars  dign /1.0/ : ignition margin
  real(kind(1.0D0)) :: dign = 1.0D0
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
  !+ad_vars  dnbeta /3.5/ : coefficient for Troyon beta scaling
  real(kind(1.0D0)) :: dnbeta = 3.5D0
  !+ad_vars  dnelimt : density limit (/m3)
  real(kind(1.0D0)) :: dnelimt = 0.0D0
  !+ad_vars  dnitot : total ion density (/m3)
  real(kind(1.0D0)) :: dnitot = 0.0D0
  !+ad_vars  dnla : line averaged electron density (/m3)
  real(kind(1.0D0)) :: dnla = 0.0D0
  !+ad_vars  dnprot : proton ash density (/m3) (idhe3=1)
  real(kind(1.0D0)) :: dnprot = 0.0D0
  !+ad_vars  dntau : plasma average "n-tau" (seconds/m3)
  real(kind(1.0D0)) :: dntau = 0.0D0
  !+ad_vars  dnz : high Z ion density (/m3)
  real(kind(1.0D0)) :: dnz = 0.0D0
  !+ad_vars  ealpha /3520.0/ FIX : alpha birth energy in D-T reaction (keV)
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
  !+ad_vars  fbfe /0.35/ : fraction of high-Z radiation to Bremsstrahlung
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
  !+ad_vars  gtscale /0/ : switch for a/R scaling of dnbeta:<UL>
  !+ad_varc          <LI>  = 0 do not scale dnbeta with eps; 
  !+ad_varc          <LI>  otherwise scale dnbeta with eps  </UL>
  integer :: gtscale = 0
  !+ad_vars  hfac(ipnlaws) : H factors for an ignited plasma for each scaling law
  real(kind(1.0D0)), dimension(ipnlaws) :: hfac = 0.0D0
  !+ad_vars  hfact /2.0/ : H factor on energy confinement times (iteration variable 10)
  real(kind(1.0D0)) :: hfact = 2.0D0
  !+ad_vars  ibss /1/ : switch for bootstrap current scaling:<UL>
  !+ad_varc        <LI> = 1 ITER bootstrap scaling (high R/a only);
  !+ad_varc        <LI> = 2 for Nevins general scaling;
  !+ad_varc        <LI> = 3 for Wilson numerical scaling</UL>
  integer :: ibss  = 1
  !+ad_vars  iculbl /0/ : switch for Troyon beta limit scaling:<UL>
  !+ad_varc          <LI> = 0 apply limit to total beta;
  !+ad_varc          <LI> = 1 apply limit to thermal beta;
  !+ad_varc          <LI> = 2 apply limit to thermal + neutral beam beta</UL>
  integer :: iculbl = 0
  !+ad_vars  icurr /4/ : switch for plasma current scaling to use:<UL>
  !+ad_varc         <LI> = 1 Peng analytic fit;
  !+ad_varc         <LI> = 2 Peng double null divertor scaling (TART);
  !+ad_varc         <LI> = 3 simple ITER scaling (k = 2.2, d = 0.6);
  !+ad_varc         <LI> = 4 later ITER scaling, a la Uckan;
  !+ad_varc         <LI> = 5 Todd empirical scaling I;
  !+ad_varc         <LI> = 6 Todd empirical scaling II;
  !+ad_varc         <LI> = 7 Connor-Hastie model</UL>
  integer :: icurr = 4
  !+ad_vars  idensl /3/ : switch for density limit to enforce:<UL>
  !+ad_varc          <LI> = 1 old ASDEX;
  !+ad_varc          <LI> = 2 Borrass model for ITER (I);
  !+ad_varc          <LI> = 3 Borrass model for ITER (II);
  !+ad_varc          <LI> = 4 JET edge radiation;
  !+ad_varc          <LI> = 5 JET simplified;
  !+ad_varc          <LI> = 6 Hugill-Murakami Mq limit;
  !+ad_varc          <LI> = 7 Greenwald limit</UL>
  integer :: idensl = 3
  !+ad_vars  idhe3 /0/ : switch for main fusion reaction:<UL>
  !+ad_varc         <LI> = 0 D-T reaction;
  !+ad_varc         <LI> = 1 D-He3 reaction (+ daughters)</UL>
  integer :: idhe3 = 0
  !+ad_vars  idivrt : number of divertors (calculated from snull)
  integer :: idivrt = 2
  !+ad_vars  ifalphap /0/ : switch for fast alpha pressure calculation:<UL>
  !+ad_varc            <LI> = 0 ITER physics rules (Uckan) fit;
  !+ad_varc            <LI> = 1 Modified fit (D. Ward) - better at high temperature</UL>
  integer :: ifalphap = 0
  !+ad_vars  ifispact /0/ : switch for neutronics calculations:<UL>
  !+ad_varc            <LI> = 0 neutronics calculations turned off;
  !+ad_varc            <LI> = 1 neutronics calculations turned on</UL>
  integer :: ifispact = 0
  !+ad_vars  igeom /0/ : switch for plasma geometry calculation:<UL>
  !+ad_varc         <LI> = 0 original method;
  !+ad_varc         <LI> = 1 new method</UL>
  integer :: igeom = 0
  !+ad_vars  ignite /0/ : switch for ignition assumption:<UL>
  !+ad_varc          <LI> = 0 do not assume plasma ignition;
  !+ad_varc          <LI> = 1 assume ignited (but include aux power in costs).
  !+ad_varc               Obviously, ignite must be zero if current drive
  !+ad_varc               is required. Note that whole code is not quite
  !+ad_varc               consistent yet...</UL>
  integer :: ignite = 0
  !+ad_vars  iinvqd /1/ : switch for inverse quadrature in tauee laws:<UL>
  !+ad_varc          <LI> = 0 inverse quadrature not used;
  !+ad_varc          <LI> = 1 inverse quadrature used</UL>
  integer :: iinvqd = 1
  !+ad_vars  iiter /1/ : switch for ITER fusion power calculations:<UL>
  !+ad_varc         <LI> = 0 fusion power integrated over plasma profiles;
  !+ad_varc         <LI> = 1 fusion power calculated from ITER analytical formula
  !+ad_varc                  (bad fit if alphan /= 0.5 and/or alphat /= 1.0)</UL>
  integer :: iiter = 1
  !+ad_vars  impc /1.0/ : carbon impurity multiplier
  real(kind(1.0D0)) :: impc = 1.0D0
  !+ad_vars  impfe /1.0/ : iron impurity multiplier (OBSOLETE)
  real(kind(1.0D0)) :: impfe = 1.0D0
  !+ad_vars  impo /1.0/ : oxygen impurity multiplier
  real(kind(1.0D0)) :: impo = 1.0D0
  !+ad_vars  isc /34 (=IPB98(y,2))/ switch for energy confinement time scaling law
  !+ad_varc          (see description in tauscl)
  integer :: isc = 34
  !+ad_vars  iscrp /1/ : switch for scrapeoff width:<UL>
  !+ad_varc         <LI> = 0 use 10% of rminor;
  !+ad_varc         <LI> = 1 use input (scrapli and scraplo)</UL>
  integer :: iscrp = 1
  !+ad_vars  ishape /0/ : switch for plasma cross-sectional shape calculation:<UL>
  !+ad_varc          <LI> = 0 use input kappa, triang;
  !+ad_varc          <LI> = 1 scale qlim, kappa, triang (TART)</UL>
  integer :: ishape = 0
  !+ad_vars  itart /0/ : switch for tight aspect ratio models:<UL>
  !+ad_varc         <LI> = 0 use conventional aspect ratio models;
  !+ad_varc         <LI> = 1 use tight aspect ratio models</UL>
  integer :: itart = 0
  !+ad_vars  iwalld /1/ : switch for neutron wall load calculation:<UL>
  !+ad_varc          <LI> = 1 use scaled plasma surface area;
  !+ad_varc          <LI> = 2 use first wall area directly</UL>
  integer :: iwalld = 1
  !+ad_vars  kappa /2.218/ : plasma separatrix elongation
  real(kind(1.0D0)) :: kappa = 2.218D0
  !+ad_vars  kappa95 : 95% plasma elongation
  real(kind(1.0D0)) :: kappa95 = 0.0D0
  !+ad_vars  kappaa : plasma elongation calculated as xarea/(pi.a2)
  real(kind(1.0D0)) :: kappaa = 0.0D0
  !+ad_vars  palp : alpha power per volume (MW/m3)
  real(kind(1.0D0)) :: palp = 0.0D0
  !+ad_vars  palpe : alpha power per volume to electrons (MW/m3)
  real(kind(1.0D0)) :: palpe = 0.0D0
  !+ad_vars  palpi : alpha power per volume to ions (MW/m3)
  real(kind(1.0D0)) :: palpi = 0.0D0
  !+ad_vars  palpnb : alpha power from hot neutral beam ions (MW)
  real(kind(1.0D0)) :: palpnb = 0.0D0
  !+ad_vars  pbrem : bremstrahhlung power per volume (MW/m3)
  real(kind(1.0D0)) :: pbrem = 0.0D0
  !+ad_vars  pcharge : non-alpha charged particle fusion power (MW/m3)
  real(kind(1.0D0)) :: pcharge = 0.0D0
  !+ad_vars  pcoef : profile factor (= n-weighted T / average T)
  real(kind(1.0D0)) :: pcoef = 0.0D0
  !+ad_vars  pdivt : power to divertor (MW)
  real(kind(1.0D0)) :: pdivt = 0.0D0
  !+ad_vars  pfuscmw : charged particle fusion power (MW)
  real(kind(1.0D0)) :: pfuscmw = 0.0D0
  !+ad_vars  phiint : internal plasma V-s
  real(kind(1.0D0)) :: phiint = 0.0D0
  !+ad_vars  pie : ion/electron equilibration power (MW/m3)
  real(kind(1.0D0)) :: pie = 0.0D0
  !+ad_vars  plascur : plasma current (A)
  real(kind(1.0D0)) :: plascur = 0.0D0
  !+ad_vars  plrad : edge line radiation power per volume (MW/m3)
  real(kind(1.0D0)) :: plrad = 0.0D0
  !+ad_vars  pneut : neutron fusion power per volume (MW/m3)
  real(kind(1.0D0)) :: pneut = 0.0D0
  !+ad_vars  pohmpv : ohmic heating per volume (MW/m3)
  real(kind(1.0D0)) :: pohmpv = 0.0D0
  !+ad_vars  powerht : heating power (MW) used in confinement time calculation
  real(kind(1.0D0)) :: powerht = 0.0D0
  !+ad_vars  powfmw : fusion power, max (MW)
  real(kind(1.0D0)) :: powfmw = 0.0D0
  !+ad_vars  prad : total core radiation power (MW/m3)
  real(kind(1.0D0)) :: prad = 0.0D0
  !+ad_vars  psync : synchrotron radiation power (MW/m3)
  real(kind(1.0D0)) :: psync = 0.0D0
  !+ad_vars  pthrmw(8) : L-H power threshold (MW): <OL>
  !+ad_varc         <LI> ITER 1996 nominal
  !+ad_varc         <LI> ITER 1996 upper bound
  !+ad_varc         <LI> ITER 1996 lower bound
  !+ad_varc         <LI> ITER 1997 excluding elongation
  !+ad_varc         <LI> ITER 1997 including elongation
  !+ad_varc         <LI> 2008 Martin scaling: nominal
  !+ad_varc         <LI> 2008 Martin scaling: 95% upper bound
  !+ad_varc         <LI> 2008 Martin scaling: 95% lower bound</OL>
  real(kind(1.0D0)), dimension(8) :: pthrmw = 0.0D0
  !+ad_vars  ptre : electron transport power (MW/m3)
  real(kind(1.0D0)) :: ptre = 0.0D0
  !+ad_vars  ptri : ion transport power (MW/m3)
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
  !+ad_vars  rnfene : n_highZ / n_e
  real(kind(1.0D0)) :: rnfene = 0.0D0
  !+ad_vars  rnone : n_oxygen / n_e
  real(kind(1.0D0)) :: rnone = 0.0D0
  !+ad_vars  rpfac : neo-classical correction factor to rplas
  real(kind(1.0D0)) :: rpfac = 0.0D0
  !+ad_vars  rplas : plasma resistance (ohm)
  real(kind(1.0D0)) :: rplas = 0.0D0
  !+ad_vars  sarea : plasma surface area
  real(kind(1.0D0)) :: sarea = 0.0D0
  !+ad_vars  sareao : outboard plasma surface area
  real(kind(1.0D0)) :: sareao = 0.0D0
  !+ad_vars  sf : shape factor
  real(kind(1.0D0)) :: sf = 0.0D0
  !+ad_vars  snull /0/ : switch for single null / double null plasma:<UL>
  !+ad_varc          <LI> = 0 for double null;
  !+ad_varc          <LI> = 1 for single null (diverted side down)</UL>
  integer :: snull = 0
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
  !+ad_vars  ti /8.33/ : volume averaged ion temperature (keV);
  !+ad_varc              N.B. calculated from te if tratio > 0.0
  real(kind(1.0D0)) :: ti = 8.33D0
  !+ad_vars  tin : density weighted average ion temperature (keV)
  real(kind(1.0D0)) :: tin = 0.0D0
  !+ad_vars  tratio /1.0/ : ion temperature / electron temperature;
  !+ad_varc                 used to calculate ti if tratio > 0.0
  real(kind(1.0D0)) :: tratio = 1.0D0
  !+ad_vars  triang /0.6/ : plasma separatrix triangularity
  real(kind(1.0D0)) :: triang = 0.6D0
  !+ad_vars  triang95 : plasma triangularity at 95% surface
  real(kind(1.0D0)) :: triang95 = 0.0D0
  !+ad_vars  vol : plasma volume (m3)
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
  !+ad_vars  wallmw : average neutron wall load, max (MW/m2)
  real(kind(1.0D0)) :: wallmw = 0.0D0
  !+ad_vars  wtgpd : mass of fuel used per day (g)
  real(kind(1.0D0)) :: wtgpd = 0.0D0
  !+ad_vars  xarea : plasma cross-sectional area (m2)
  real(kind(1.0D0)) :: xarea = 0.0D0
  !+ad_vars  zeff : plasma effective charge
  real(kind(1.0D0)) :: zeff = 0.0D0
  !+ad_vars  zeffai : density weighted plasma effective charge
  real(kind(1.0D0)) :: zeffai = 0.0D0
  !+ad_vars  zfear /0/ : high-Z impurity switch; 0=iron, 1=argon
  integer :: zfear = 0

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
  !+ad_hist  08/01/13 PJK Modified irfcd comments
  !+ad_hist  14/01/13 PJK Corrected some more comments; removed echpwr0
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
  !+ad_varc         <LI> Fenstermacher Lower Hybrid
  !+ad_varc         <LI> Ion Cyclotron current drive
  !+ad_varc         <LI> Fenstermacher ECH
  !+ad_varc         <LI> Ehst Lower Hybrid
  !+ad_varc         <LI> ITER Neutral Beam
  !+ad_varc         <LI> new Culham Lower Hybrid model
  !+ad_varc         <LI> new Culham ECCD model
  !+ad_varc         <LI> new Culham Neutral Beam model
  !+ad_varc         <LI> RFP Oscillating Field current drive </OL>
  integer :: iefrf = 5
  !+ad_vars  irfcd /1/ : switch for current drive calculation:<UL>
  !+ad_varc         <LI> = 0 turned off;
  !+ad_varc         <LI> = 1 turned on</UL>
  integer :: irfcd = 1
  !+ad_vars  pheat /0.0/ : heating power not used for current drive (W)
  !+ad_varc                (iteration variable 11)
  real(kind(1.0D0)) :: pheat = 0.0D0
  !+ad_vars  pinjalw /25.0/ : Maximum allowable value for injected power (MW)
  !+ad_varc                   (constraint equation 30)
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
  !+ad_vars  divdens /1.0D4/ : divertor structure density (kg/m3)
  real(kind(1.0D0)) :: divdens = 1.0D4
  !+ad_vars  divdum /0/ : switch for divertor Zeff model: 0=calc, 1=input
  integer :: divdum = 0
  !+ad_vars  divfix /0.2/ FIX : divertor structure vertical thickness (m)
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
  !+ad_vars  hldiv : outboard divertor heat load (MW/m2)
  real(kind(1.0D0)) :: hldiv = 0.0D0
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
  !+ad_vars  plsepo /1.5/ : poloidal length, x-point to outboard strike point (m)
  real(kind(1.0D0)) :: plsepo = 1.5D0
  !+ad_vars  ppdivr : peak heat load at plate (with radiation) (MW/m2)
  real(kind(1.0D0)) :: ppdivr = 0.0D0
  !+ad_vars  prn1 /0.285/ : n-scrape-off / n-average plasma
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
  !+ad_vars  rstrko : outboard strike point radius (m)
  real(kind(1.0D0)) :: rstrko = 0.0D0
  !+ad_vars  tconl : main plasma connection length (m)
  real(kind(1.0D0)) :: tconl = 0.0D0
  !+ad_vars  tdiv : temperature at divertor (eV)
  real(kind(1.0D0)) :: tdiv = 0.0D0
  !+ad_vars  tsep : temperature at the separatrix (eV)
  real(kind(1.0D0)) :: tsep = 0.0D0
  !+ad_vars  xparain /2.1D3/ : parallel heat transport coefficient (m2/s)
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
  !+ad_desc  wall, blanket and shield components. It is derived from
  !+ad_desc  <CODE>include</CODE> files <CODE>fwblsh.h90</CODE> and
  !+ad_desc  <CODE>blanket.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_hist  09/04/13 PJK Added rdewex, rpf2dewar; changed some labels
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
  !+ad_vars  cryomass : vacuum vessel mass (kg)
  real(kind(1.0D0)) :: cryomass = 0.0D0
  !+ad_vars  denstl /7800.0/ : density of steel (kg/m3)
  real(kind(1.0D0)) :: denstl = 7800.0D0
  !+ad_vars  dewmkg : total mass of vacuum vessel + cryostat (kg)
  real(kind(1.0D0)) :: dewmkg = 0.0D0
  !+ad_vars  emult /1.27/ : energy multiplication in blanket and shield
  real(kind(1.0D0)) :: emult = 1.27D0
  !+ad_vars  fblbe /0.6/ : beryllium fraction of blanket by volume
  real(kind(1.0D0)) :: fblbe = 0.6D0
  !+ad_vars  fblli /0.0/ : lithium fraction of blanket by volume
  real(kind(1.0D0)) :: fblli = 0.0D0
  !+ad_vars  fblli2o /0.08/ : lithium oxide fraction of blanket by volume
  real(kind(1.0D0)) :: fblli2o = 0.08D0
  !+ad_vars  fbllipb /0.68/ : lithium lead fraction of blanket by volume
  real(kind(1.0D0)) :: fbllipb = 0.68D0
  !+ad_vars  fblss /0.07/ : stainless steel fraction of blanket by volume
  real(kind(1.0D0)) :: fblss = 0.07D0
  !+ad_vars  fblvd /0.0/ : vanadium fraction of blanket by volume
  real(kind(1.0D0)) :: fblvd = 0.0D0
  !+ad_vars  fhole /0.15/ : hole fraction of the 1st wall - that neutrons see
  real(kind(1.0D0)) :: fhole = 0.15D0
  !+ad_vars  fvolbi /1.0/ : fudge factor for inboard blanket volume
  real(kind(1.0D0)) :: fvolbi = 1.0D0
  !+ad_vars  fvolbo /0.75/ : fudge factor for outboard blanket volume
  real(kind(1.0D0)) :: fvolbo = 0.75D0
  !+ad_vars  fvolcry /1.4/ : fudge factor for cryostat volume (obsolete)
  real(kind(1.0D0)) :: fvolcry = 1.4D0
  !+ad_vars  fvoldw /1.4/ : fudge factor for vacuum vessel volume
  real(kind(1.0D0)) :: fvoldw = 1.4D0
  !+ad_vars  fvolsi /0.64/ : fudge factor for inboard shield volume
  real(kind(1.0D0)) :: fvolsi = 0.64D0
  !+ad_vars  fvolso /0.64/ : fudge factor for outboard shield volume
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
  !+ad_vars  rdewex : external cryostat radius (m)
  real(kind(1.0D0)) :: rdewex = 0.0D0
  !+ad_vars  rpf2dewar /0.5/ : radial distance between centre of ipfloc=3
  !+ad_varc                    PF coil conductor and external cryostat (m)
  real(kind(1.0D0)) :: rpf2dewar = 0.5D0
  !+ad_vars  vdewex : external cryostat volume (m3)
  real(kind(1.0D0)) :: vdewex = 0.0D0
  !+ad_vars  vdewin : vacuum vessel volume (m3)
  real(kind(1.0D0)) :: vdewin = 0.0D0
  !+ad_vars  vfblkt /0.25/ : coolant void fraction in blanket
  real(kind(1.0D0)) :: vfblkt = 0.25D0
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
  !+ad_vars  wtshldi : mass of inboard shield (kg)
  real(kind(1.0D0)) :: wtshldi = 0.0D0
  !+ad_vars  wtshldo : mass of outboard shield (kg)
  real(kind(1.0D0)) :: wtshldo = 0.0D0

  !+ad_vars  <P><B>The following are used in the full thermodynamic blanket model
  !+ad_varc  (lblnkt=1):</B><P>

  !+ad_vars  astr /2/ : Switch for blanket cooling channel geometry (lblnkt=1):<UL>
  !+ad_varc        <LI> = 1 circular cross section;
  !+ad_varc        <LI> = 2 annular cross section</UL>
  integer :: astr = 2
  !+ad_vars  bstr /1/ : Switch for blanket boundary condition (lblnkt=1):<UL>
  !+ad_varc        <LI> = 1 coolant outlet temperature fixed;
  !+ad_varc        <LI> = 2 maximum blanket temperature fixed</UL>
  integer :: bstr = 1
  !+ad_vars  costr /2/ : Switch for blanket coolant material (lblnkt=1):<UL>
  !+ad_varc         <LI> = 1 Gaseous helium coolant;
  !+ad_varc         <LI> = 2 Pressurized water coolant</UL>
  integer :: costr = 2
  !+ad_vars  estr /1/ : Switch for cooling channel orientation (lblnkt=1):<UL>
  !+ad_varc        <LI> = 1 radially orientated;
  !+ad_varc        <LI> = 2 poloidally orientated</UL>
  integer :: estr = 1
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
  !+ad_vars  lblnkt /1/ : Switch for blanket model:<UL>
  !+ad_varc          <LI> = 0 original model;
  !+ad_varc          <LI> = 1 full thermodynamic model</UL>
  integer :: lblnkt = 1
  !+ad_vars  nipfwh /1/ : Number of intermediate pressure feed water heater pumps
  integer :: nipfwh = 1
  !+ad_vars  nlpfwh /1/ : Number of low pressure feed water heater pumps
  integer :: nlpfwh = 1
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
  !+ad_vars  smstr /1/ : Switch for blanket material (lblnkt=1):<UL>
  !+ad_varc         <LI> = 1 Li2O/Be (solid blanket);
  !+ad_varc         <LI> = 2 LiPb/Li (liquid blanket)</UL>
  integer :: smstr = 1
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

  !+ad_vars  ac1oh /0.0/ : OH coil cable conduit area (m2)
  real(kind(1.0D0)) :: ac1oh = 0.0D0
  !+ad_vars  acsoh /3.0D-4/ : conduit conductor cross section (m2)
  real(kind(1.0D0)) :: acsoh = 3.0D-4
  !+ad_vars  alfapf /5.0D-10/ : smoothing parameter used in BOP PF coil
  !+ad_varc                     current calculation
  real(kind(1.0D0)) :: alfapf = 5.0D-10
  !+ad_vars  bmaxoh : B-max in OH coil at EOF (T)
  real(kind(1.0D0)) :: bmaxoh = 0.0D0
  !+ad_vars  bmaxoh0 : B-max in OH coil at BOP (T)
  real(kind(1.0D0)) :: bmaxoh0 = 0.0D0
  !+ad_vars  bpf(ngc2) : peak field at coil i (T)
  real(kind(1.0D0)), dimension(ngc2) :: bpf = 0.0D0
  !+ad_vars  cohbof : OH coil overall current density at BOF (A/m2)
  real(kind(1.0D0)) :: cohbof = 0.0D0
  !+ad_vars  cohbop : OH coil overall current density at BOP (A/m2)
  real(kind(1.0D0)) :: cohbop = 0.0D0
  !+ad_vars  coheof /1.85D7/ : OH coil overall current density at EOF (A/m2)
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
  !+ad_vars  ipfloc(ngc) /1,2,3/ : switch for locating scheme of PF coil group i:<UL>
  !+ad_varc      <LI> = 1 PF coil on top of OH coil;
  !+ad_varc      <LI> = 2 PF coil on top of TF coil;
  !+ad_varc      <LI> = 3 PF coil outside of TF coil</UL>
  integer, dimension(ngc) :: ipfloc = (/1,2,3,0,0,0,0,0,0,0,0,0,0,0,0,0/)
  !+ad_vars  ipfres /0/ : switch for PF coil type:<UL>
  !+ad_varc          <LI> = 0 superconducting PF coils;
  !+ad_varc          <LI> = 1 resistive PF coils</UL>
  integer :: ipfres = 0
  !+ad_vars  isumatpf /1/ : switch for superconductor material in PF coils:<UL>
  !+ad_varc            <LI> = 1 binary Nb3Sn;
  !+ad_varc            <LI> = 2 ternary Nb3Sn;
  !+ad_varc            <LI> = 3 NbTi</UL>
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
  !+ad_vars  rjconpf(ngc2) /3.0D7/ : average current density of PF coil i (A/m2)
  !+ad_varc                          at time of peak current in that coil
  !+ad_varc                          (calculated for ipfloc=1 coils)
  real(kind(1.0D0)), dimension(ngc2) :: rjconpf = 3.0D7
  !+ad_vars  rjohc : allowable OH coil current density at EOF (A/m2)
  real(kind(1.0D0)) :: rjohc = 0.0D0
  !+ad_vars  rjohc0 : allowable OH coil current density at BOP (A/m2)
  real(kind(1.0D0)) :: rjohc0 = 0.0D0
  !+ad_vars  rjpfalw(ngc2) : allowable current density of PF coil i (A/m2)
  real(kind(1.0D0)), dimension(ngc2) :: rjpfalw = 0.0D0
  !+ad_vars  rohc : radius to the centre of the OH coil (m)
  real(kind(1.0D0)) :: rohc = 0.0D0
  !+ad_vars  routr /1.5/ : distance (m) from outboard TF coil leg to centre of
  !+ad_varc                ipfloc=3 PF coils
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
  !+ad_varc                      for groups with ipfloc = 3
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
  !+ad_hist  30/01/13 PJK Modified vftf comments
  !+ad_hist  08/04/13 PJK Modified cpttf, tfno comments
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  acasetf : total external case area (inboard) (m2)
  real(kind(1.0D0)) :: acasetf = 0.0D0
  !+ad_vars  acndttf : area of the cable conduit (m2)
  real(kind(1.0D0)) :: acndttf = 0.0D0
  !+ad_vars  acond : conductor area (winding pack) (m2)
  real(kind(1.0D0)) :: acond = 0.0D0
  !+ad_vars  acstf : internal area of the cable space (m2)
  real(kind(1.0D0)) :: acstf = 0.0D0
  !+ad_vars  aiwp : winding pack insulation area (m2)
  real(kind(1.0D0)) :: aiwp = 0.0D0
  !+ad_vars  alstrtf : allowable stress in TF coil (Pa)
  real(kind(1.0D0)) :: alstrtf = 0.0D0
  !+ad_vars  arealeg : outboard TF leg area (m2)
  real(kind(1.0D0)) :: arealeg = 0.0D0
  !+ad_vars  aspcstf /1.0/ : TF conductor cable aspect ratio (radial/toroidal)
  real(kind(1.0D0)) :: aspcstf = 1.0D0
  !+ad_vars  aswp : winding pack structure area (m2)
  real(kind(1.0D0)) :: aswp = 0.0D0
  !+ad_vars  avwp : winding pack void (He coolant) area (m2)
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
  !+ad_vars  cdtfleg /1.0D6/ : TF leg overall current density (A/m2)
  !+ad_varc                    (iteration variable 24)
  real(kind(1.0D0)) :: cdtfleg = 1.0D6
  !+ad_vars  cforce : centering force on inboard leg (per coil) (N/m)
  real(kind(1.0D0)) :: cforce = 0.0D0
  !+ad_vars  cph2o /4180.0/ FIX : specific heat of water (J/kg/K)
  real(kind(1.0D0)) :: cph2o = 4180.0D0
  !+ad_vars  cpttf /3.79D4/ : TF coil current per turn (A)
  !+ad_varc                  (iteration variable 60)
  real(kind(1.0D0)) :: cpttf = 3.79D4
  !+ad_vars  csutf /1.4D9/ : ultimate strength of case (Pa)
  real(kind(1.0D0)) :: csutf = 1.4D9
  !+ad_vars  csytf /8.25D8/ : yield strength of case (Pa)
  real(kind(1.0D0)) :: csytf = 8.25D8
  !+ad_vars  dcase /8000.0/ : density of coil case (kg/m3)
  real(kind(1.0D0)) :: dcase = 8000.0D0
  !+ad_vars  dcond(5) /9000.0/ : density of superconductor (kg/m3)
  real(kind(1.0D0)), dimension(5) :: dcond = 9000.0D0
  !+ad_vars  dcopper /8900.0/ : density of copper (kg/m3)
  real(kind(1.0D0)) :: dcopper = 8900.0D0
  !+ad_vars  deflect : TF coil deflection at full field (m)
  real(kind(1.0D0)) :: deflect = 0.0D0
  !+ad_vars  denh2o /985.0/ FIX : density of water (kg/m3)
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
  !+ad_vars  fcoolcp /0.3/ : coolant fraction of TF coil inboard legs
  !+ad_varc                  (iteration variable 23)
  real(kind(1.0D0)) :: fcoolcp = 0.3D0
  !+ad_vars  fcutfsu /0.69/ : copper fraction of cable conductor
  !+ad_varc                   (iteration variable 59)
  real(kind(1.0D0)) :: fcutfsu = 0.69D0
  !+ad_vars  frhocp /1.0/ centrepost resistivity enhancement factor
  real(kind(1.0D0)) :: frhocp = 1.0D0
  !+ad_vars  isumattf /1/ : switch for superconductor material in TF coils:<UL>
  !+ad_varc            <LI> = 1 binary Nb3Sn;
  !+ad_varc            <LI> = 2 ternary Nb3Sn;
  !+ad_varc            <LI> = 3 NbTi;
  !+ad_varc            <LI> = 4 generic, but uses Nb3Sn current density model;
  !+ad_varc            <LI> = 5 generic, but uses NbTi current density model</UL>
  integer :: isumattf = 1
  !+ad_vars  itfmod /1/ : switch for TF coil magnet model:<UL>
  !+ad_varc          <LI> = 0 use simple model;
  !+ad_varc          <LI> = 1 use complex stress/superconductor models</UL>
  integer :: itfmod = 1
  !+ad_vars  itfsup /1/ : switch for TF coil conductor model:<UL>
  !+ad_varc          <LI> = 0 conventional copper;
  !+ad_varc          <LI> = 1 superconductor</UL>
  integer :: itfsup = 1
  !+ad_vars  jbus /1.25D6/ : bussing current density (A/m2)
  real(kind(1.0D0)) :: jbus = 1.25D6
  !+ad_vars  jcrit_model /0/ : switch for binary Nb3Sn critical J model (isumattf=1):<UL>
  !+ad_varc               <LI> = 0 use original model;
  !+ad_varc               <LI> = 1 use ITER critical surface model</UL>
  integer :: jcrit_model = 0
  !+ad_vars  jcritsc /2.225D10/ : critical current density (A/m2) for
  !+ad_varc                       superconductor (isumattf=4 or 5)
  real(kind(1.0D0)) :: jcritsc = 2.225D10
  !+ad_vars  jeff(5) : work array used in stress calculation (A/m2)
  real(kind(1.0D0)), dimension(5) :: jeff = 0.0D0
  !+ad_vars  jwdgcrt : critical current density for winding pack (A/m2)
  real(kind(1.0D0)) :: jwdgcrt = 0.0D0
  !+ad_vars  jwdgpro : allowable TF coil winding pack current density,
  !+ad_varc            for dump temperature rise protection (A/m2)
  real(kind(1.0D0)) :: jwdgpro = 0.0D0
  !+ad_vars  jwptf : winding pack current density (A/m2)
  real(kind(1.0D0)) :: jwptf = 0.0D0
  !+ad_vars  kcp /330.0/ FIX : thermal conductivity of centrepost (W/m/K)
  real(kind(1.0D0)) :: kcp = 330.0D0
  !+ad_vars  kh2o /0.651/ FIX : thermal conductivity of water (W/m/K)
  real(kind(1.0D0)) :: kh2o = 0.651D0
  !+ad_vars  magnt /2/ : switch for TF coil stress model:<UL>
  !+ad_varc         <LI> = 1 for FER type configuration with bucking cylinder;
  !+ad_varc         <LI> = 2 for NET type wedging; set casfi = 0.0;
  !+ad_varc         <LI> = 3 for LLNL type buck on OH coil</UL>
  integer :: magnt = 2
  !+ad_vars  muh2o /4.71D-4/ FIX : water dynamic viscosity (kg/m/s)
  real(kind(1.0D0)) :: muh2o = 4.71D-4
  !+ad_vars  ncool : number of centrepost coolant tubes
  real(kind(1.0D0)) :: ncool = 0.0D0
  !+ad_vars  oacdcp /1.4D7/ : overall current density in TF coil inboard legs (A/m2)
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
  !+ad_vars  rhocp : TF coil inboard leg resistance (Ohm)
  real(kind(1.0D0)) :: rhocp = 0.0D0
  !+ad_vars  rhotfleg : TF coil leg resistance (Ohm)
  real(kind(1.0D0)) :: rhotfleg = 0.0D0
  !+ad_vars  ripmax /5.0/ : maximum peak/average ripple at plasma edge (%)
  real(kind(1.0D0)) :: ripmax = 5.0D0
  !+ad_vars  ripple : peak/average ripple at plasma edge (%)
  real(kind(1.0D0)) :: ripple = 0.0D0
  !+ad_vars  ritfc : total current in TF coil (A)
  real(kind(1.0D0)) :: ritfc = 0.0D0
  !+ad_vars  rjtfsual : allowable overall coil current density (A/m2)
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
  !+ad_vars  tcpav /100.0/ : average temp of TF coil inboard leg conductor (C)
  !+ad_varc                  (resistive coils) (iteration variable 20)
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
  !+ad_vars  tfareain : area of inboard midplane TF legs (m2)
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
  !+ad_vars  tfcpmw : peak resistive TF coil inboard leg power (MW)
  real(kind(1.0D0)) :: tfcpmw = 0.0D0
  !+ad_vars  tficrn : TF coil half-width - inner bore (m)
  real(kind(1.0D0)) :: tficrn = 0.0D0
  !+ad_vars  tfind : TF coil inductance (H)
  real(kind(1.0D0)) :: tfind = 0.0D0
  !+ad_vars  tflegmw : TF coil outboard leg resistive power (MW)
  real(kind(1.0D0)) :: tflegmw = 0.0D0
  !+ad_vars  tflegres /2.5D-8/ : resistivity of a TF coil leg (Ohm-m)
  real(kind(1.0D0)) :: tflegres = 2.5D-8
  !+ad_vars  tfleng : TF coil circumference (m)
  real(kind(1.0D0)) :: tfleng = 0.0D0
  !+ad_vars  tfno /16.0/ : number of TF coils (default = 50 for stellarators)
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
  !+ad_vars  vforce : vertical separating force on inboard leg/coil (N)
  real(kind(1.0D0)) :: vforce = 0.0D0
  !+ad_vars  vftf /0.4/ : coolant fraction of TF coil leg (itfsup=0)
  !+ad_varc               or of TF coil cable space (itfsup=1)
  real(kind(1.0D0)) :: vftf = 0.4D0
  !+ad_vars  volcp : total volume of TF coil inboard legs (m3)
  real(kind(1.0D0)) :: volcp = 0.0D0
  !+ad_vars  voltfleg : volume of each TF coil outboard leg (m3)
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
  !+ad_vars  whtcp : mass of TF coil inboard legs (kg)
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

  !+ad_vars  <P><B>Superconducting TF coil shape parameters</B> (see also farc4tf);
  !+ad_varc  <BR>the TF inner surface top half is approximated by circular arcs.
  !+ad_varc  Arc 1 goes through points 1 and 2 on the inner surface. Arc 2
  !+ad_varc  goes through points 2-3, etc.<P>

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

  !+ad_name  structure_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  support structure
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  support structure of a fusion power plant.
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
  !+ad_desc  It is derived from <CODE>include</CODE> files
  !+ad_desc  <CODE>vaccom.h90</CODE> and <CODE>torsdat.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  29/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

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
  !+ad_vars  pbase /2.6D-6/ : base pressure (Pa)
  real(kind(1.0D0)) :: pbase = 2.6D-6
  !+ad_vars  prdiv /0.36/ : divertor chamber pressure during burn (Pa)
  real(kind(1.0D0)) :: prdiv = 0.36D0
  !+ad_vars  rat /1.3D-8/ : plasma chamber wall outgassing rate (Pa-m/s)
  real(kind(1.0D0)) :: rat = 1.3D-8
  !+ad_vars  tn /300.0/ : neutral gas temperature in chamber (K)
  real(kind(1.0D0)) :: tn = 300.0D0
  !+ad_vars  vacdshm : mass of vacuum duct shield (kg)
  real(kind(1.0D0)) :: vacdshm = 0.0D0
  !+ad_vars  vcdimax : diameter of duct passage (m)
  real(kind(1.0D0)) :: vcdimax = 0.0D0
  !+ad_vars  vpumpn : number of high vacuum pumps
  real(kind(1.0D0)) :: vpumpn = 0.0D0

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
  !+ad_desc  It is derived from <CODE>include</CODE> files
  !+ad_desc  <CODE>pwrcom.h90</CODE> and <CODE>estocom.h90</CODE>.
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

  !+ad_vars  acptmax : average of currents in PF circuits (A)
  real(kind(1.0D0)) :: acptmax = 0.0D0
  !+ad_vars  ensxpfm : maximum stored energy in the PF circuits (MJ)
  real(kind(1.0D0)) :: ensxpfm = 0.0D0
  !+ad_vars  iscenr /2/ : Switch for energy storage option:<UL>
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
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>htpwr.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  27/03/13 PJK Comment change to FMGDMW
  !+ad_hist  11/04/13 PJK Comment change to TFACPD
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  baseel /5.0D6/ : base plant electric load (W)
  real(kind(1.0D0)) :: baseel = 5.0D6
  !+ad_vars  crypmw : cryogenic plant power (MW)
  real(kind(1.0D0)) :: crypmw = 0.0D0
  !+ad_vars  ctht : total plant heat removal (MW)
  real(kind(1.0D0)) :: ctht = 0.0D0
  !+ad_vars  etahhten /1.35/ : efficiency of H production for ihplant=2
  real(kind(1.0D0)) :: etahhten = 1.35D0
  !+ad_vars  etahhtex /1.12/ : efficiency of H production for ihplant=3
  real(kind(1.0D0)) :: etahhtex = 1.12D0
  !+ad_vars  etahlte /0.75/ : efficiency of H production for ihplant=1
  real(kind(1.0D0)) :: etahlte = 0.75D0
  !+ad_vars  etahth /0.5/ : efficiency of H production for ihplant=4
  real(kind(1.0D0)) :: etahth = 0.5D0
  !+ad_vars  etath /0.35/ : thermal to electric conversion efficiency
  !+ad_varc                 if lblnkt=0, otherwise calculated
  real(kind(1.0D0)) :: etath = 0.35D0
  !+ad_vars  facht : facility heat removal (MW)
  real(kind(1.0D0)) :: facht = 0.0D0
  !+ad_vars  fauxbop /0.06/ : fraction of gross electric power to balance-of-plant
  real(kind(1.0D0)) :: fauxbop = 0.06D0
  !+ad_vars  fcsht : total power to facility loads (MW)
  real(kind(1.0D0)) :: fcsht = 0.0D0
  !+ad_vars  ffwlg /1.0/ : fraction of first wall / divertor power to low grade heat
  real(kind(1.0D0)) :: ffwlg = 1.0D0
  !+ad_vars  fgrosbop : scaled fraction of gross power to balance-of-plant
  real(kind(1.0D0)) :: fgrosbop = 0.0D0
  !+ad_vars  fmgdmw /0.0/ : power to mgf (motor-generator flywheel) units (MW)
  !+ad_varc                 (ignored if iscenr=2)
  real(kind(1.0D0)) :: fmgdmw = 0.0D0
  !+ad_vars  helecmw /0.0/ : electrical power required for H production (MW)
  !+ad_varc                  (iteration variable 87)
  real(kind(1.0D0)) :: helecmw = 0.0D0
  !+ad_vars  helpow : heat removal at cryogenic temperatures (W)
  real(kind(1.0D0)) :: helpow = 0.0D0
  !+ad_vars  hpower : hydrogen production (MW equivalent)
  real(kind(1.0D0)) :: hpower = 0.0D0
  !+ad_vars  hthermmw /0.0/ : thermal power required for H production (MW)
  !+ad_varc                   (iteration variable 88)
  !+ad_varc                   (N.B. calculated for ihplant=1,2,3)
  real(kind(1.0D0)) :: hthermmw = 0.0D0
  !+ad_vars  htpmw /10.0/ : heat transport system pump power (MW)
  real(kind(1.0D0)) :: htpmw = 10.0D0
  !+ad_vars  hvolume : hydrogen production (Normal m3/second)
  real(kind(1.0D0)) :: hvolume = 0.0D0
  !+ad_vars  ihplant /0/ : switch for hydrogen production plant:<UL>
  !+ad_varc           <LI> = 0 no hydrogen plant;
  !+ad_varc           <LI> = 1 Low Temperature Electrolysis;
  !+ad_varc           <LI> = 2 High Temperature Electrolysis - endothermic;
  !+ad_varc           <LI> = 3 High Temperature Electrolysis - exothermic;
  !+ad_varc           <LI> = 4 Thermo-chemical</UL>
  integer :: ihplant = 0
  !+ad_vars  iprimhtp /0/ : switch for heat transport pump power:<UL>
  !+ad_varc            <LI> = 0 contributes to secondary heat;
  !+ad_varc            <LI> = 1 contributes to primary heat</UL>
  integer :: iprimhtp = 0
  !+ad_vars  pacpmw : total pulsed power system load (MW)
  real(kind(1.0D0)) :: pacpmw = 0.0D0
  !+ad_vars  peakmva : peak MVA requirement
  real(kind(1.0D0)) :: peakmva = 0.0D0
  !+ad_vars  pfwdiv : heat removal from first wall/divertor (MW)
  real(kind(1.0D0)) :: pfwdiv = 0.0D0
  !+ad_vars  pgrossmw : gross electric power (MW)
  real(kind(1.0D0)) :: pgrossmw = 0.0D0
  !+ad_vars  pinjht : heat removal from injection power (MW)
  real(kind(1.0D0)) :: pinjht = 0.0D0
  !+ad_vars  pinjwp : injector wall plug power (MW)
  real(kind(1.0D0)) :: pinjwp = 0.0D0
  !+ad_vars  pnetelmw : net electric power (MW)
  real(kind(1.0D0)) :: pnetelmw = 0.0D0
  !+ad_vars  priheat : primary nuclear heating (MW)
  real(kind(1.0D0)) :: priheat = 0.0D0
  !+ad_vars  psecht : secondary heat (MW)
  real(kind(1.0D0)) :: psecht = 0.0D0
  !+ad_vars  pthermmw : primary heat (useful for electric production) (MW)
  real(kind(1.0D0)) :: pthermmw = 0.0D0
  !+ad_vars  pwpm2 /150.0/ : base AC power requirement (W/m2)
  real(kind(1.0D0)) :: pwpm2 = 150.0D0
  !+ad_vars  rnihx : number of intermediate heat exchangers
  real(kind(1.0D0)) :: rnihx = 0.0D0
  !+ad_vars  rnphx : number of primary heat exchangers
  real(kind(1.0D0)) :: rnphx = 0.0D0
  !+ad_vars  tfacpd /0.0/ : total steady state TF coil AC power demand (MW)
  !+ad_varc                 (itfsup=0 only; calculated for itfsup=1)
  real(kind(1.0D0)) :: tfacpd = 0.0D0
  !+ad_vars  tlvpmw : estimate of total low voltage power (MW)
  real(kind(1.0D0)) :: tlvpmw = 0.0D0
  !+ad_vars  trithtmw /15.0/ : power required for tritium processing (MW)
  real(kind(1.0D0)) :: trithtmw = 15.0D0
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
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>times.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  tburn /227.9/ : burn time (s) (calculated if lpulse=1)
  real(kind(1.0D0)) :: tburn = 227.9D0
  !+ad_vars  tburn0 : burn time (s) - used in consistency equation 15
  real(kind(1.0D0)) :: tburn0 = 0.0D0
  !+ad_vars  tdown : down time (s)
  real(kind(1.0D0)) :: tdown = 0.0D0
  !+ad_vars  tdwell /100.0/ : time between pulses in a pulsed reactor (s)
  !+ad_varc                   (iteration variable 17)
  real(kind(1.0D0)) :: tdwell = 100.0D0
  !+ad_vars  theat /10.0/ : heating time, after current ramp up (s)
  real(kind(1.0D0)) :: theat = 10.0D0
  !+ad_vars  tim(6) : array of time points during plasma pulse (s)
  real(kind(1.0D0)), dimension(6) :: tim = 0.0D0
  !+ad_vars  tohs /30.0/ : OH coil swing time for current initiation (s)
  !+ad_varc                (iteration variable 65)
  real(kind(1.0D0)) :: tohs = 30.0D0
  !+ad_vars  tohsin /0.0/ : switch for OH coil swing time (if lpulse=0):<UL>
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
  !+ad_desc  It is derived from <CODE>include</CODE> files
  !+ad_desc  <CODE>bldgcom.h90</CODE> and <CODE>bldgvol.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  08/04/13 PJK Modified wrbi comment
  !+ad_hist  09/04/13 PJK Changed clh1 default from 8.0 to 2.5
  !+ad_hist  09/04/13 PJK Added building volume multipliers rbvfac, mbvfac, wsvfac
  !+ad_hist  11/04/13 PJK Comment change to esbldgm3
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  admv /1.0D5/ : administration building volume (m3)
  real(kind(1.0D0)) :: admv = 1.0D5
  !+ad_vars  admvol : volume of administration buildings (m3)
  real(kind(1.0D0)) :: admvol = 0.0D0
  !+ad_vars  clh1 /2.5/ : minimum vertical clearance from TF coil to cryostat (m)
  real(kind(1.0D0)) :: clh1 = 2.5D0
  !+ad_vars  clh2 /15.0/ : clearance beneath TF coil to foundation
  !+ad_varc                (including basement) (m)
  real(kind(1.0D0)) :: clh2 = 15.0D0
  !+ad_vars  conv /6.0D4/ : control building volume (m3)
  real(kind(1.0D0)) :: conv = 6.0D4
  !+ad_vars  convol : volume of control, protection and i&c building (m3)
  real(kind(1.0D0)) :: convol = 0.0D0
  !+ad_vars  cryvol : volume of cryoplant building (m3)
  real(kind(1.0D0)) :: cryvol = 0.0D0
  !+ad_vars  efloor : effective total floor space (m2)
  real(kind(1.0D0)) :: efloor = 0.0D0
  !+ad_vars  elevol : volume of electrical equipment building (m3)
  real(kind(1.0D0)) :: elevol = 0.0D0
  !+ad_vars  esbldgm3 /1.0D3/ : volume of energy storage equipment building (m3)
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
  !+ad_vars  pfbldgm3 /2.0D4/ : volume of PF coil power supply building (m3)
  real(kind(1.0D0)) :: pfbldgm3 = 2.0D4
  !+ad_vars  pibv /2.0D4/ : power injection building volume (m3)
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
  !+ad_vars  shov /1.0D5/ : shops and warehouse volume (m3)
  real(kind(1.0D0)) :: shov = 1.0D5
  !+ad_vars  shovol :volume of shops and buildings for plant auxiliaries (m3)
  real(kind(1.0D0)) :: shovol = 0.0D0
  !+ad_vars  stcl /3.0/ : clearance above crane to roof (m)
  real(kind(1.0D0)) :: stcl = 3.0D0
  !+ad_vars  tfcbv /2.0D4/ : volume of TF coil power supply building (m3)
  !+ad_varc                  (calculated if TF coils are superconducting)
  real(kind(1.0D0)) :: tfcbv = 2.0D4
  !+ad_vars  trcl /1.0/ : transportation clearance between components (m)
  real(kind(1.0D0)) :: trcl = 1.0D0
  !+ad_vars  triv /4.0D4/ : volume of tritium, fuel handling and
  !+ad_varc                 health physics buildings (m3)
  real(kind(1.0D0)) :: triv = 4.0D4
  !+ad_vars  volnucb : sum of nuclear buildings volumes (m3)
  real(kind(1.0D0)) :: volnucb = 0.0D0
  !+ad_vars  volrci : internal volume of reactor building (m3)
  real(kind(1.0D0)) :: volrci = 0.0D0
  !+ad_vars  wgt /5.0D5/ : reactor building crane capacity (kg)
  !+ad_varc                (calculated if 0 is input)
  real(kind(1.0D0)) :: wgt = 5.0D5
  !+ad_vars  wgt2 /1.0D5/ : hot cell crane capacity (kg)
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
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>build.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  18/12/12 PJK Added hpfdif, hpfu
  !+ad_hist  09/04/13 PJK Relabelled ddwex, ddwi etc.
  !+ad_hist  10/04/13 PJK Relabelled gapsto, gapomin etc.
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  aplasmin /0.25/ : minimum minor radius (m)
  real(kind(1.0D0)) :: aplasmin = 0.25D0
  !+ad_vars  bcylth /0.0/ : bucking cylinder thickness (m)
  real(kind(1.0D0)) :: bcylth = 0.0D0
  !+ad_vars  blnkith /0.115/ : inboard blanket thickness (m)
  real(kind(1.0D0)) :: blnkith = 0.115D0
  !+ad_vars  blnkoth /0.235/ : outboard blanket thickness (m)
  real(kind(1.0D0)) :: blnkoth = 0.235D0
  !+ad_vars  bore /1.42/ : OH coil inboard radius (m)
  !+ad_varc                (iteration variable 29)
  real(kind(1.0D0)) :: bore = 1.42D0
  !+ad_vars  ddwex /0.07/ : external cryostat thickness (m)
  real(kind(1.0D0)) :: ddwex = 0.07D0
  !+ad_vars  ddwi /0.07/ : vacuum vessel thickness (TF coil / shield) (m)
  real(kind(1.0D0)) :: ddwi = 0.07D0
  !+ad_vars  fmsbc /0.0/ : Martensitic fraction of steel in bucking cylinder
  real(kind(1.0D0)) :: fmsbc = 0.0D0
  !+ad_vars  fmsbl /0.0/ : Martensitic fraction of steel in blanket
  real(kind(1.0D0)) :: fmsbl = 0.0D0
  !+ad_vars  fmsdwe /0.0/ : Martensitic fraction of steel in external cryostat
  real(kind(1.0D0)) :: fmsdwe = 0.0D0
  !+ad_vars  fmsdwi /0.0/ : Martensitic fraction of steel in vacuum vessel
  real(kind(1.0D0)) :: fmsdwi = 0.0D0
  !+ad_vars  fmsfw /0.0/ : Martensitic fraction of steel in first wall
  real(kind(1.0D0)) :: fmsfw = 0.0D0
  !+ad_vars  fmsoh /0.0/ : Martensitic fraction of steel in OH coil
  real(kind(1.0D0)) :: fmsoh = 0.0D0
  !+ad_vars  fmssh /0.0/ : Martensitic fraction of steel in shield
  real(kind(1.0D0)) :: fmssh = 0.0D0
  !+ad_vars  fmstf /0.0/ : Martensitic fraction of steel in TF coil
  real(kind(1.0D0)) :: fmstf = 0.0D0
  !+ad_vars  fwarea : first wall surface area (m2)
  real(kind(1.0D0)) :: fwarea = 0.0D0
  !+ad_vars  fwith /0.035/ : inboard first wall thickness (m) (if lpulse=1, =2*bfw)
  real(kind(1.0D0)) :: fwith = 0.035D0
  !+ad_vars  fwoth /0.035/ : outboard first wall thickness (m) (if lpulse=1, =2*bfw)
  real(kind(1.0D0)) :: fwoth = 0.035D0
  !+ad_vars  gapds /0.0/ : gap between vacuum vessel and inboard shield
  !+ad_varc                (iteration variable 61)
  real(kind(1.0D0)) :: gapds = 0.0D0
  !+ad_vars  gapoh /0.08/ : gap between OH coil and bucking cylinder
  !+ad_varc                (iteration variable 42)
  real(kind(1.0D0)) :: gapoh = 0.08D0
  !+ad_vars  gapomin /0.21/ : minimum gap between outboard shield and vacuum vessel (m)
  !+ad_varc                   (iteration variable 31)
  real(kind(1.0D0)) :: gapomin = 0.21D0
  !+ad_vars  gapsto : gap between outboard shield and vacuum vessel (m)
  real(kind(1.0D0)) :: gapsto = 0.0D0
  !+ad_vars  hmax : maximum (half-)height of TF coil (m)
  real(kind(1.0D0)) :: hmax = 0.0D0
  !+ad_vars  hpfdif : difference in distance from midplane of upper and lower
  !+ad_varc           portions of TF legs (non-zero for single-null devices) (m)
  real(kind(1.0D0)) :: hpfdif = 0.0D0
  !+ad_vars  hpfu : height to top of (upper) TF coil leg (m)
  real(kind(1.0D0)) :: hpfu = 0.0D0
  !+ad_vars  hr1 : half-height of TF coil inboard leg straight section (m)
  real(kind(1.0D0)) :: hr1 = 0.0D0
  !+ad_vars  iohcl /1/ : switch for existence of OH coil:<UL>
  !+ad_varc         <LI> = 0 OH coil not present;
  !+ad_varc         <LI> = 1 OH coil exists</UL>
  integer :: iohcl = 1
  !+ad_vars  ohcth /0.63/ : OH coil thickness (m)
  !+ad_varc                 (iteration variable 16)
  real(kind(1.0D0)) :: ohcth = 0.63D0
  !+ad_vars  prtsz : port size - width (m)
  real(kind(1.0D0)) :: prtsz = 0.0D0
  !+ad_vars  prtszreq : port size required for beam access (m)
  real(kind(1.0D0)) :: prtszreq = 0.0D0
  !+ad_vars  rbld : sum of thicknesses to the major radius (m)
  real(kind(1.0D0)) :: rbld = 0.0D0
  !+ad_vars  rinboard /0.651/ : plasma inboard radius (m)
  !+ad_varc                     (consistency equation 29)
  real(kind(1.0D0)) :: rinboard = 0.651D0
  !+ad_vars  rsldi : radius to inboard shield (inside point) (m)
  real(kind(1.0D0)) :: rsldi = 0.0D0
  !+ad_vars  rsldo : radius to outboard shield (outside point) (m)
  real(kind(1.0D0)) :: rsldo = 0.0D0
  !+ad_vars  rtfcin : radius of centre of inboard TF leg (m)
  real(kind(1.0D0)) :: rtfcin = 0.0D0
  !+ad_vars  rtot : radius to the centre of the outboard TF coil leg (m)
  real(kind(1.0D0)) :: rtot = 0.0D0
  !+ad_vars  scrapli /0.14/ : inboard scrapeoff length (m) (used if iscrp=1)
  !+ad_varc                   (iteration variable 73)
  real(kind(1.0D0)) :: scrapli = 0.14D0
  !+ad_vars  scraplo /0.15/ : outboard scrapeoff length (m) (used if iscrp=1)
  !+ad_varc                   (iteration variable 74)
  real(kind(1.0D0)) :: scraplo = 0.15D0
  !+ad_vars  shldith /0.69/ : inboard shield thickness (m)
  real(kind(1.0D0)) :: shldith = 0.69D0
  !+ad_vars  shldoth /1.05/ : outboard shield thickness (m)
  real(kind(1.0D0)) :: shldoth = 1.05D0
  !+ad_vars  shldtth /0.60/ : upper shield thickness (m)
  real(kind(1.0D0)) :: shldtth = 0.6D0
  !+ad_vars  tfcth /0.9/ : inboard TF coil thickness, (centrepost for ST) (m)
  !+ad_varc                (iteration variable 13)
  real(kind(1.0D0)) :: tfcth = 0.9D0
  !+ad_vars  tfootfi /1.8/ : TF coil outboard leg / inboard leg thickness ratio
  !+ad_varc                  (iteration variable 75)
  real(kind(1.0D0)) :: tfootfi = 1.8D0
  !+ad_vars  tfthko : outboard TF coil thickness (m)
  real(kind(1.0D0)) :: tfthko = 0.0D0
  !+ad_vars  vgap : (see vgaptf)
  real(kind(1.0D0)) :: vgap = 0.0D0
  !+ad_vars  vgap2 /0.163/ : vertical gap between the TF coil and shield (m)
  real(kind(1.0D0)) :: vgap2 = 0.163D0
  !+ad_vars  vgaptf /0.0/ : vertical gap between x-point and divertor (m)
  !+ad_varc                (if = 0, it is calculated)
  real(kind(1.0D0)) :: vgaptf = 0.0D0

end module build_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module cost_variables

  !+ad_name  cost_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  costing algorithms
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  costing algorithms of a fusion power plant.
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>cost.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  31/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  abktflnc /20.0/ : allowable first wall/blanket neutron
  !+ad_varc                    fluence (MW-yr/m2)
  real(kind(1.0D0)) :: abktflnc = 20.0D0
  !+ad_vars  adivflnc /25.0/ : allowable divertor heat fluence (MW-yr/m2)
  real(kind(1.0D0)) :: adivflnc = 25.0D0
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
  real(kind(1.0D0)) :: cdirt = 0.0D0
  !+ad_vars  cdrlife : lifetime of current drive system (y)
  real(kind(1.0D0)) :: cdrlife = 0.0D0
  !+ad_vars  cfactr /0.75/ : plant capacity factor, availability;
  !+ad_varc                  input if iavail = 0
  real(kind(1.0D0)) :: cfactr = 0.75D0
  !+ad_vars  cfind(4) /0.244,0.244,0.244,0.29/ : indirect cost factor (func of lsa)
  real(kind(1.0D0)), dimension(4) :: cfind = &
       (/0.244D0, 0.244D0, 0.244D0, 0.29D0/)
  !+ad_vars  chplant : capital cost of hydrogen plant (M$)
  real(kind(1.0D0)) :: chplant = 0.0D0
  !+ad_vars  cland /19.2/ : cost of land (M$)
  real(kind(1.0D0)) :: cland = 19.2D0
  !+ad_vars  coe : cost of electricity (m$/kW-hr)
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
  !+ad_vars  cowner /0.15/ : owner cost factor
  real(kind(1.0D0)) :: cowner = 0.15D0
  !+ad_vars  cplife : lifetime of centrepost (y)
  real(kind(1.0D0)) :: cplife = 0.0D0
  !+ad_vars  cpstcst : TART centrepost direct cost (M$)
  real(kind(1.0D0)) :: cpstcst = 0.0D0
  !+ad_vars  cpstflnc /10.0/ : allowable TART centrepost neutron fluence (MW-yr/m2)
  real(kind(1.0D0)) :: cpstflnc = 10.0D0
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
  real(kind(1.0D0)) :: divlife = 0.0D0
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
  real(kind(1.0D0)) :: fcontng = 0.195D0
  !+ad_vars  fcr0 /0.0966/ : fixed charge rate during construction
  real(kind(1.0D0)) :: fcr0 = 0.0966D0
  !+ad_vars  fkind /1.0/ : multiplier for Nth of a kind costs
  real(kind(1.0D0)) :: fkind = 1.0D0
  !+ad_vars  fwallcst : first wall cost (M$)
  real(kind(1.0D0)) :: fwallcst = 0.0D0
  !+ad_vars  iavail /0/ : switch for plant availability model:<UL>
  !+ad_varc          <LI> = 0 use input value for cfactr;
  !+ad_varc          <LI> = 1 calculate cfactr using model</UL>
  integer :: iavail= 0
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
  integer :: ireactor = 1
  !+ad_vars  lsa /4/ : level of safety assurance switch (generally, use 3 or 4):<UL>
  !+ad_varc       <LI> = 1 truly passively safe plant;
  !+ad_varc       <LI> = 2,3 in-between;
  !+ad_varc       <LI> = 4 like current fission plant</UL>
  integer :: lsa = 4
  !+ad_vars  moneyint : interest portion of capital cost (M$)
  real(kind(1.0D0)) :: moneyint = 0.0D0
  !+ad_vars  ratecdol /0.0435/ : effective cost of money in constant dollars
  real(kind(1.0D0)) :: ratecdol = 0.0435D0
  !+ad_vars  tbktrepl /0.5/ : time taken to replace blanket (y)
  real(kind(1.0D0)) :: tbktrepl = 0.5D0
  !+ad_vars  tcomrepl /0.5/ : time taken to replace both blanket and divertor (y)
  real(kind(1.0D0)) :: tcomrepl = 0.5D0
  !+ad_vars  tdivrepl /0.25/ : time taken to replace divertor (y)
  real(kind(1.0D0)) :: tdivrepl = 0.25D0
  !+ad_vars  tlife /30.0/ : plant life (years)
  real(kind(1.0D0)) :: tlife = 30.0D0
  !+ad_vars  ucad /180.0/ FIX : unit cost for administration buildings (M$/m3)
  real(kind(1.0D0)) :: ucad = 180.0D0
  !+ad_vars  ucaf /1.5D6/ FIX : unit cost for aux facility power equipment ($)
  real(kind(1.0D0)) :: ucaf = 1.5D6
  !+ad_vars  ucahts /31.0/ FIX : unit cost for aux heat transport equipment ($/W**exphts)
  real(kind(1.0D0)) :: ucahts = 31.0D0
  !+ad_vars  ucap /17.0/ FIX : unit cost of auxiliary transformer ($/kVA)
  real(kind(1.0D0)) :: ucap = 17.0D0
  !+ad_vars  ucblbe /260.0/ : unit cost for blanket beryllium ($/kg)
  real(kind(1.0D0)) :: ucblbe = 260.0D0
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
  !+ad_vars  ucbpmp /2.925D5/ FIX : vacuum system backing pump cost ($)
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
  !+ad_vars  uccpmp /3.9D5/ FIX : vacuum system cryopump cost ($)
  real(kind(1.0D0)) :: uccpmp = 3.9D5
  !+ad_vars  uccr /460.0/ FIX : unit cost for cryogenic building (M$/vol)
  real(kind(1.0D0)) :: uccr = 460.0D0
  !+ad_vars  uccry /9.3D4/ : heat transport system cryoplant costs ($/W**expcry)
  real(kind(1.0D0)) :: uccry = 9.3D4
  !+ad_vars  uccryo /32.0/ : unit cost for cryostat ($/kg)
  real(kind(1.0D0)) :: uccryo = 32.0D0
  !+ad_vars  uccu /75.0/ : unit cost for copper in superconducting cable ($/kg)
  real(kind(1.0D0)) :: uccu = 75.0D0
  !+ad_vars  ucdgen /1.7D6/ FIX : cost per 8 MW diesel generator ($)
  real(kind(1.0D0)) :: ucdgen = 1.7D6
  !+ad_vars  ucdiv /2.8D5/ : cost of divertor blade ($)
  real(kind(1.0D0)) :: ucdiv = 2.8D5
  !+ad_vars  ucdtc /13.0/ FIX : detritiation, air cleanup cost ($/10000m3/hr)
  real(kind(1.0D0)) :: ucdtc = 13.0D0
  !+ad_vars  ucduct /4.225D4/ FIX : vacuum system duct cost ($/m)
  real(kind(1.0D0)) :: ucduct = 4.225D4
  !+ad_vars  ucech /3.0/ : ECH system cost ($/W)
  real(kind(1.0D0)) :: ucech = 3.0D0
  !+ad_vars  ucel /380.0/ FIX : unit cost for electrical equipment building (M$/m3)
  real(kind(1.0D0)) :: ucel = 380.0D0
  !+ad_vars  uces1 /3.2D4/ FIX : MGF (motor-generator flywheel) cost factor ($/MVA**0.8)
  real(kind(1.0D0)) :: uces1 = 3.2D4
  !+ad_vars  uces2 /8.8D3/ FIX : MGF (motor-generator flywheel) cost factor ($/MJ**0.8)
  real(kind(1.0D0)) :: uces2 = 8.8D3
  !+ad_vars  ucf1 /2.23D7/ : cost of fuelling system ($)
  real(kind(1.0D0)) :: ucf1 = 2.23D7
  !+ad_vars  ucfnc /35.0/ : outer PF coil fence support cost ($/kg)
  real(kind(1.0D0)) :: ucfnc = 35.0D0
  !+ad_vars  ucfpr /4.4D7/ FIX : cost of 60g/day tritium processing unit ($)
  real(kind(1.0D0)) :: ucfpr = 4.4D7
  !+ad_vars  ucfuel /3.45/ : unit cost of fuel (M$/year/1200MW)
  real(kind(1.0D0)) :: ucfuel = 3.45D0
  !+ad_vars  ucfwa /6.0D4/ FIX : first wall armour cost ($/m2)
  real(kind(1.0D0)) :: ucfwa = 6.0D4
  !+ad_vars  ucfwps /1.0D7/ FIX : first wall passive stabiliser cost ($/m2)
  real(kind(1.0D0)) :: ucfwps = 1.0D7
  !+ad_vars  ucfws /5.3D4/ FIX : first wall structure cost ($/m2)
  real(kind(1.0D0)) :: ucfws = 5.3D4
  !+ad_vars  ucgss /35.0/ FIX : cost of reactor structure ($/kg)
  real(kind(1.0D0)) :: ucgss = 35.0D0
  !+ad_vars  uche3 /1.0D6/ : cost of helium-3 ($/kg)
  real(kind(1.0D0)) :: uche3 = 1.0D6
  !+ad_vars  uchhten /1350.0/ : cost of H production (HTE - endothermic) ($/kW Hyd)
  real(kind(1.0D0)) :: uchhten = 1350.0D0
  !+ad_vars  uchhtex /900.0/ : cost of H production (HTE - exothermic) ($/kW Hyd)
  real(kind(1.0D0)) :: uchhtex = 900.0D0
  !+ad_vars  uchlte /400.0/ : cost of H production (LTE) ($/kW Hydrogen)
  real(kind(1.0D0)) :: uchlte = 400.0D0
  !+ad_vars  uchrs /87.9D6/ : cost of heat rejection system ($)
  real(kind(1.0D0)) :: uchrs = 87.9D6
  !+ad_vars  uchth /700.0/ : cost of H production (thermo-chemical) ($/kW Hydrogen)
  real(kind(1.0D0)) :: uchth = 700.0D0
  !+ad_vars  uchts(2) /15.3,19.1/ : cost of heat transport system equipment
  !+ad_varc                         per loop ($/W); dependent on coolant type
  real(kind(1.0D0)), dimension(2) :: uchts = (/15.3D0, 19.1D0/)
  !+ad_vars  uciac /1.5D8/ : cost of instrumentation, control & diagnostics ($/W)
  real(kind(1.0D0)) :: uciac = 1.5D8
  !+ad_vars  ucich /3.0/ : ICH system cost ($/W)
  real(kind(1.0D0)) :: ucich = 3.0D0
  !+ad_vars  ucihx /0.0/ : cost of intermediate heat exchangers ($/W**exphts)
  real(kind(1.0D0)) :: ucihx = 0.0D0
  !+ad_vars  ucint /35.0/ FIX : superconductor intercoil structure cost ($/kg)
  real(kind(1.0D0)) :: ucint = 35.0D0
  !+ad_vars  uclh /3.3/ : LH system cost ($/W)
  real(kind(1.0D0)) :: uclh = 3.3D0
  !+ad_vars  uclv /16.0/ FIX : low voltage system cost ($/kVA)
  real(kind(1.0D0)) :: uclv = 16.0D0
  !+ad_vars  ucmb /260.0/ FIX: unit cost for reactor maintenance building (M$/m3)
  real(kind(1.0D0)) :: ucmb = 260.0D0
  !+ad_vars  ucme /1.25D8/ : unit cost of maintenance equipment ($/W**0.3)
  real(kind(1.0D0)) :: ucme = 1.25D8
  !+ad_vars  ucmisc /2.5D7/ : miscellaneous plant allowance ($)
  real(kind(1.0D0)) :: ucmisc = 2.5D7
  !+ad_vars  ucnbi /3.3/ : NBI system cost ($/W)
  real(kind(1.0D0)) :: ucnbi = 3.3D0
  !+ad_vars  ucnbv /1000.0/ FIX : cost of nuclear building ventilation ($/m3)
  real(kind(1.0D0)) :: ucnbv = 1000.0D0
  !+ad_vars  ucoam(4) /68.8,68.8,68.8,74.4/ : annual cost of operation and
  !+ad_varc                                   maintenance (M$/year/1200MW**0.5)
  real(kind(1.0D0)), dimension(4) :: ucoam = &
       (/68.8D0, 68.8D0, 68.8D0, 74.4D0/)
  !+ad_vars  ucof /3.3/ : oscillating field current drive cost ($/W)
  real(kind(1.0D0)) :: ucof = 3.3D0
  !+ad_vars  ucpens /32.0/ : penetration shield cost ($/kg)
  real(kind(1.0D0)) :: ucpens = 32.0D0
  !+ad_vars  ucpfb /210.0/ : cost of PF coil buses ($/kA/m)
  real(kind(1.0D0)) :: ucpfb = 210.0D0
  !+ad_vars  ucpfbk /1.66D4/ : cost of PF coil DC breakers ($/MVA)
  real(kind(1.0D0)) :: ucpfbk = 1.66D4
  !+ad_vars  ucpfbs /4.9D3/ : cost of PF burn power supplies ($/kW**0.7)
  real(kind(1.0D0)) :: ucpfbs = 4.9D3
  !+ad_vars  ucpfcb /7.5D4/ : cost of PF coil AC breakers ($/circuit)
  real(kind(1.0D0)) :: ucpfcb = 7.5D4
  !+ad_vars  ucpfdr1 /150.0/ : cost factor for dump resistors ($/MJ)
  real(kind(1.0D0)) :: ucpfdr1 = 150.0D0
  !+ad_vars  ucpfic /1.0D4/ : cost of PF instrumentation and control ($/channel)
  real(kind(1.0D0)) :: ucpfic = 1.0D4
  !+ad_vars  ucpfps /3.5D4/ : cost of PF coil pulsed power supplies ($/MVA)
  real(kind(1.0D0)) :: ucpfps = 3.5D4
  !+ad_vars  ucphx /15.0/ FIX : primary heat transport cost ($/W**exphts)
  real(kind(1.0D0)) :: ucphx = 15.0D0
  !+ad_vars  ucpp /48.0/ FIX : cost of primary power transformers ($/kVA**0.9)
  real(kind(1.0D0)) :: ucpp = 48.0D0
  !+ad_vars  ucrb /400.0/ : cost of reactor building (M$/m3)
  real(kind(1.0D0)) :: ucrb = 400.0D0
  !+ad_vars  ucsc(5) /600.0,600.0,300.0,600.0,600.0/ : cost of superconductor ($/kg)
  real(kind(1.0D0)), dimension(5) :: ucsc = &
       (/600.0D0, 600.0D0, 300.0D0, 600.0D0, 600.0D0/)
  !+ad_vars  ucsh /115.0/ FIX : cost of shops and warehouses (M$/m3)
  real(kind(1.0D0)) :: ucsh = 115.0D0
  !+ad_vars  ucshld /32.0/ : cost of shield structural steel ($/kg)
  real(kind(1.0D0)) :: ucshld = 32.0D0
  !+ad_vars  ucswyd /1.84D7/ FIX : switchyard equipment costs ($)
  real(kind(1.0D0)) :: ucswyd = 1.84D7
  !+ad_vars  uctfbr /1.22/ : cost of TF coil breakers ($/W**0.7)
  real(kind(1.0D0)) :: uctfbr = 1.22D0
  !+ad_vars  uctfbus /100.0/ : cost of TF coil bus ($/kg)
  real(kind(1.0D0)) :: uctfbus = 100.0D0
  !+ad_vars  uctfdr /1.75D-4/ FIX : cost of TF coil dump resistors ($/J)
  real(kind(1.0D0)) :: uctfdr = 1.75D-4
  !+ad_vars  uctfgr /5000.0/ FIX : additional cost of TF coil dump resistors ($/coil)
  real(kind(1.0D0)) :: uctfgr = 5000.0D0
  !+ad_vars  uctfic /1.0D4/ FIX : cost of TF coil instrumentation and control ($/coil/30)
  real(kind(1.0D0)) :: uctfic = 1.0D4
  !+ad_vars  uctfps /24.0/ : cost of TF coil power supplies ($/W**0.7)
  real(kind(1.0D0)) :: uctfps = 24.0D0
  !+ad_vars  uctfsw /1.0/ : cost of TF coil slow dump switches ($/A)
  real(kind(1.0D0)) :: uctfsw = 1.0D0
  !+ad_vars  uctpmp /1.105D5/ FIX : cost of turbomolecular pump ($)
  real(kind(1.0D0)) :: uctpmp = 1.105D5
  !+ad_vars  uctr /370.0/ FIX : cost of tritium building ($/m3)
  real(kind(1.0D0)) :: uctr = 370.0D0
  !+ad_vars  ucturb(2) /230.0D6,245.0D6/: cost of turbine plant equipment ($)
  !+ad_varc                               (dependent on coolant type)
  real(kind(1.0D0)), dimension(2) :: ucturb = (/230.0D6, 245.0D6/)
  !+ad_vars  ucvalv /3.9D5/ FIX : vacuum system valve cost ($)
  real(kind(1.0D0)) :: ucvalv = 3.9D5
  !+ad_vars  ucvdsh /26.0/ FIX : vacuum duct shield cost ($/kg)
  real(kind(1.0D0)) :: ucvdsh = 26.0D0
  !+ad_vars  ucviac /1.3D6/ FIX : vacuum system instrumentation and control cost ($)
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
  !+ad_vars  uubop /0.02/ : unplanned unavailability factor for balance of plant
  real(kind(1.0D0)) :: uubop = 0.02D0
  !+ad_vars  uucd /0.02/ : unplanned unavailability factor for current drive
  real(kind(1.0D0)) :: uucd = 0.02D0
  !+ad_vars  uudiv /0.04/ : unplanned unavailability factor for divertor
  real(kind(1.0D0)) :: uudiv = 0.04D0
  !+ad_vars  uufuel /0.02/ : unplanned unavailability factor for fuel system
  real(kind(1.0D0)) :: uufuel = 0.02D0
  !+ad_vars  uufw /0.04/ : unplanned unavailability factor for first wall
  real(kind(1.0D0)) :: uufw = 0.04D0
  !+ad_vars  uumag /0.02/ : unplanned unavailability factor for magnets
  real(kind(1.0D0)) :: uumag = 0.02D0
  !+ad_vars  uuves /0.04/ : unplanned unavailability factor for vessel
  real(kind(1.0D0)) :: uuves = 0.04D0

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
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>ineq.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  31/10/12 PJK Initial version of module
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
  !+ad_vars  bmxlim /12.0/ : maximum toroidal field (T)
  !+ad_varc                  (constraint equation 25)
  real(kind(1.0D0)) :: bmxlim = 12.0D0
  !+ad_vars  dtmpmx /1.0D3/ : maximum first wall coolant temperature rise (K)
  !+ad_varc                   (constraint equation 38)
  real(kind(1.0D0)) :: dtmpmx = 1.0D3
  !+ad_vars  fauxmn /1.0/ : f-value for minimum auxiliary power
  !+ad_varc                 (constraint equation 40, iteration variable 64)
  real(kind(1.0D0)) :: fauxmn = 1.0D0
  !+ad_vars  fbeta /1.0/ : f-value for epsilon beta-poloidal
  !+ad_varc                (constraint equation 6, iteration variable 8)
  real(kind(1.0D0)) :: fbeta = 1.0D0
  !+ad_vars  fbetap /1.0/ : f-value for poloidal beta
  !+ad_varc                 (constraint equation 48, iteration variable 79)
  real(kind(1.0D0)) :: fbetap = 1.0D0
  !+ad_vars  fbetatry /1.0/ : f-value for Troyon beta limit
  !+ad_varc                   (constraint equation 24, iteration variable 36)
  real(kind(1.0D0)) :: fbetatry = 1.0D0
  !+ad_vars  fdene /1.0/ : f-value for density limit
  !+ad_varc                (constraint equation 5, iteration variable 5)
  real(kind(1.0D0)) :: fdene = 1.0D0
  !+ad_vars  fdivcol /1.0/ : f-value for divertor collisionality
  !+ad_varc                  (constraint equation 22, iteration variable 34)
  real(kind(1.0D0)) :: fdivcol = 1.0D0
  !+ad_vars  fdtmp /1.0/ : f-value for first wall coolant temperature rise
  !+ad_varc                (constraint equation 38, iteration variable 62)
  real(kind(1.0D0)) :: fdtmp = 1.0D0
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
  !+ad_vars  fjohc /1.0/ : f-value for OH coil current at end-of-flattop
  !+ad_varc                (constraint equation 26, iteration variable 38)
  real(kind(1.0D0)) :: fjohc = 1.0D0
  !+ad_vars  fjohc0 /1.0/ : f-value for OH coil current at beginning of pulse
  !+ad_varc                 (constraint equation 27, iteration variable 39)
  real(kind(1.0D0)) :: fjohc0 = 1.0D0
  !+ad_vars  fjprot /1.0/ : f-value for TF coil winding pack current density
  !+ad_varc                 (constraint equation 35, iteration variable 53)
  real(kind(1.0D0)) :: fjprot = 1.0D0
  !+ad_vars  fjtfc /1.0/ : f-value for TF coil current density
  !+ad_varc                (constraint equation 23, iteration variable 28)
  real(kind(1.0D0)) :: fjtfc = 1.0D0
  !+ad_vars  fmva /1.0/ : f-value for maximum MVA
  !+ad_varc               (constraint equation 19, iteration variable 30)
  real(kind(1.0D0)) :: fmva = 1.0D0
  !+ad_vars  fpeakb /1.0/ : f-value for maximum toroidal field
  !+ad_varc                 (constraint equation 25, iteration variable 35)
  real(kind(1.0D0)) :: fpeakb = 1.0D0
  !+ad_vars  fpinj /1.0/ : f-value for injection power
  !+ad_varc                (constraint equation 30, iteration variable 46)
  real(kind(1.0D0)) :: fpinj = 1.0D0
  !+ad_vars  fpnetel /1.0/ : f value for net electric power
  !+ad_varc                  (constraint equation 16, iteration variable 25)
  real(kind(1.0D0)) :: fpnetel = 1.0D0
  !+ad_vars  fportsz /1.0/ : f-value for port size
  !+ad_varc                  (constraint equation 20, iteration variable 33)
  real(kind(1.0D0)) :: fportsz = 1.0D0
  !+ad_vars  fptemp /1.0/ : f-value for peak centrepost temperature
  !+ad_varc                 (constraint equation 44, iteration variable 68)
  real(kind(1.0D0)) :: fptemp = 1.0D0
  !+ad_vars  fq /1.0/ : f-value for edge safety factor
  !+ad_varc             (constraint equation 45, iteration variable 71)
  real(kind(1.0D0)) :: fq = 1.0D0
  !+ad_vars  fqval /1.0/ : f-value for Q
  !+ad_varc                (constraint equation 28, iteration variable 45)
  real(kind(1.0D0)) :: fqval = 1.0D0
  !+ad_vars  frfpf /1.0/ : f-value for RFP reversal parameter
  !+ad_varc                (constraint equation 49, iteration variable 80)
  real(kind(1.0D0)) :: frfpf = 1.0D0
  !+ad_vars  frfptf /1.0/ : f-value for RFP TF coil toroidal thickness
  !+ad_varc                 (constraint equation 47, iteration variable 76)
  real(kind(1.0D0)) :: frfptf = 1.0D0
  !+ad_vars  frminor /1.0/ : f-value for minor radius limit
  !+ad_varc                  (constraint equation 21, iteration variable 32)
  real(kind(1.0D0)) :: frminor = 1.0D0
  !+ad_vars  fstrcase /1.0/ : f-value for TF coil case stress
  !+ad_varc                   (constraint equation 31, iteration variable 48)
  real(kind(1.0D0)) :: fstrcase = 1.0D0
  !+ad_vars  fstrcond /1.0/ : f-value for TF coil conduit stress
  !+ad_varc                   (constraint equation 32, iteration variable 49)
  real(kind(1.0D0)) :: fstrcond = 1.0D0
  !+ad_vars  ftburn /1.0/ : f-value for minimum burn time
  !+ad_varc                 (constraint equation 13, iteration variable 21)
  real(kind(1.0D0)) :: ftburn = 1.0D0
  !+ad_vars  ftcycl /1.0/ : f-value for cycle time
  !+ad_varc                 (constraint equation 42, iteration variable 67)
  real(kind(1.0D0)) :: ftcycl = 1.0D0
  !+ad_vars  ftmargtf /1.0/ : f-value for TF coil temperature margin
  !+ad_varc                   (constraint equation 36, iteration variable 54)
  real(kind(1.0D0)) :: ftmargtf = 1.0D0
  !+ad_vars  ftohs /1.0/ : f-value for OH coil swing time
  !+ad_varc                (constraint equation 41, iteration variable 66)
  real(kind(1.0D0)) :: ftohs = 1.0D0
  !+ad_vars  ftpeak /1.0/ : f-value for first wall peak temperature
  !+ad_varc                 (constraint equation 39, iteration variable 63)
  real(kind(1.0D0)) :: ftpeak = 1.0D0
  !+ad_vars  fvdump /1.0/ : f-value for dump voltage
  !+ad_varc                 (constraint equation 34, iteration variable 51)
  real(kind(1.0D0)) :: fvdump = 1.0D0
  !+ad_vars  fvs /1.0/ : f-value for flux-swing (V-s) requirement
  !+ad_varc              (constraint equation 12, iteration variable 15)
  real(kind(1.0D0)) :: fvs = 1.0D0
  !+ad_vars  fwalld /1.0/ : f-value for minimum wall load
  !+ad_varc                 (constraint equation 8, iteration variable 14)
  real(kind(1.0D0)) :: fwalld = 1.0D0
  !+ad_vars  gammax /2.0/ : maximum current drive gamma
  !+ad_varc                 (constraint equation 37)
  real(kind(1.0D0)) :: gammax = 2.0D0
  !+ad_vars  mvalim /40.0/ : maximum MVA limit
  !+ad_varc                  (constraint equation 19)
  real(kind(1.0D0)) :: mvalim = 40.0D0
  !+ad_vars  pnetelin /1000.0/ : required net electric power (MW)
  !+ad_varc                      (constraint equation 16)
  real(kind(1.0D0)) :: pnetelin = 1.0D3
  !+ad_vars  powfmax /1000.0/ : maximum fusion power (MW)
  !+ad_varc                     (constraint equation 9)
  real(kind(1.0D0)) :: powfmax = 1.0D3
  !+ad_vars  tbrnmn /1.0/ : minimum burn time (s)
  !+ad_varc                 (constraint equation 13)
  real(kind(1.0D0)) :: tbrnmn = 1.0D0
  !+ad_vars  tcycmn : minimum cycle time (s)
  !+ad_varc           (constraint equation 42)
  real(kind(1.0D0)) :: tcycmn = 0.0D0
  !+ad_vars  tohsmn : minimum OH coil swing time (s)
  !+ad_varc           (constraint equation 41)
  real(kind(1.0D0)) :: tohsmn = 1.0D0
  !+ad_vars  tpkmax /600.0/ : maximum first wall peak temperature (C)
  !+ad_varc                   (constraint equation 39)
  real(kind(1.0D0)) :: tpkmax = 600.0D0
  !+ad_vars  walalw /1.0/ : allowable wall-load (MW/m2)
  !+ad_varc                 (constraint equation 8)
  real(kind(1.0D0)) :: walalw = 1.0D0

end module constraint_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module stellarator_variables

  !+ad_name  stellarator_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  stellarator model
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  stellarator model.
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>stella.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  31/10/12 PJK Initial version of module
  !+ad_hist  23/01/13 PJK Added iotabar
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  iotabar /1.0/ : rotational transform (reciprocal of tokamak q)
  !+ad_varc                  for stellarator confinement time scaling laws
  real(kind(1.0D0)) :: iotabar = 1.0D0
  !+ad_vars  istell /0/ : switch for stellarator option
  !+ad_varc               (set via <CODE>device.dat</CODE>):<UL>
  !+ad_varc          <LI> = 0 use tokamak, RFP or IFE model;
  !+ad_varc          <LI> = 1 use stellarator model</UL>
  integer :: istell = 0
  !+ad_vars  isthtr /3/ : switch for stellarator auxiliary heating method:<UL>
  !+ad_varc          <LI> = 1 electron cyclotron resonance heating;
  !+ad_varc          <LI> = 2 lower hybrid heating;
  !+ad_varc          <LI> = 3 neutral beam injection</UL>
  integer :: isthtr = 3

end module stellarator_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module rfp_variables

  !+ad_name  rfp_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  reversed field pinch model
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  reversed field pinch model.
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>rfp.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  31/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  nrfppf /16/ FIX : number of RFP PF coils (final two are SC EF coils)
  integer, parameter :: nrfppf = 16

  !+ad_vars  cptrfp(nrfppf) : current per turn in each RFP PF coil (A/m2)
  real(kind(1.0D0)), dimension(nrfppf) :: cptrfp = 0.0D0
  !+ad_vars  drpf(nrfppf) : radial cross-section of each RFP PF coil (m)
  real(kind(1.0D0)), dimension(nrfppf) :: drpf = 0.0D0
  !+ad_vars  dzpf(nrfppf) : vertical cross-section of each RFP PF coil (m)
  real(kind(1.0D0)), dimension(nrfppf) :: dzpf = 0.0D0
  !+ad_vars  irfp /0/ : switch for RFP option
  !+ad_varc             (set via <CODE>device.dat</CODE>):<UL>
  !+ad_varc        <LI> = 0 use tokamak, stellarator or IFE model;
  !+ad_varc        <LI> = 1 use RFP model</UL>
  integer :: irfp = 0
  !+ad_vars  nturns(nrfppf) : number of turns of each RFP PF coil
  real(kind(1.0D0)), dimension(nrfppf) :: nturns = 0.0D0
  !+ad_vars  pfmmax : mass of heaviest PF coil (tonnes)
  real(kind(1.0D0)) :: pfmmax = 0.0D0
  !+ad_vars  pfrmax : radius of largest PF coil (m)
  real(kind(1.0D0)) :: pfrmax = 0.0D0
  !+ad_vars  resrfp(nrfppf) : resistance of each RFP PF coil (ohms)
  real(kind(1.0D0)), dimension(nrfppf) :: resrfp = 0.0D0
  !+ad_vars  rfpf : RFP reversal parameter F
  real(kind(1.0D0)) :: rfpf = 0.0D0
  !+ad_vars  rfpth /1.5/ : RFP pinch parameter theta
  !+ad_varc                (iteration variable 78)
  real(kind(1.0D0)) :: rfpth = 1.5D0
  !+ad_vars  rrpf(nrfppf) : radius of each RFP PF coil (m)
  real(kind(1.0D0)), dimension(nrfppf) :: rrpf = 0.0D0
  !+ad_vars  tftort /0.33/ : TF coil toroidal thickness (m) (RFP, stellarator only)
  !+ad_varc                  (iteration variable 77)
  real(kind(1.0D0)) :: tftort = 0.33D0
  !+ad_vars  zzpf(nrfppf) : vertical position of each RFP PF coil (m)
  real(kind(1.0D0)), dimension(nrfppf) :: zzpf = 0.0D0

end module rfp_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module ife_variables

  !+ad_name  ife_variables
  !+ad_summ  Module containing global variables relating to the
  !+ad_summ  inertial fusion energy model
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains global variables relating to the
  !+ad_desc  inertial fusion energy model.
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>ife.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  05/11/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  Default IFE builds and material volumes are those for the SOMBRERO device.
  !+ad_varc  The 2-dimensional arrays have indices (region, material), where<UL>
  !+ad_varc  <LI>'region' = 1 radially outside chamber
  !+ad_varc  <LI>         = 2 above chamber
  !+ad_varc  <LI>         = 3 below chamber</UL>
  !+ad_varc  and 'material' is defined as described in maxmat below.<P>

  !+ad_vars  maxmat /7/ FIX : total number of materials in IFE device.
  !+ad_varc                   Material numbers are as follows:<UL>
  !+ad_varc              <LI> = 0 void;
  !+ad_varc              <LI> = 1 steel;
  !+ad_varc              <LI> = 2 carbon cloth;
  !+ad_varc              <LI> = 3 FLiBe;
  !+ad_varc              <LI> = 4 lithium oxide Li2O;
  !+ad_varc              <LI> = 5 concrete;
  !+ad_varc              <LI> = 6 helium;
  !+ad_varc              <LI> = 7 xenon</UL>
  integer, parameter ::  maxmat = 7

  !+ad_vars  bldr /1.0/ : radial thickness of IFE blanket (m)
  real(kind(1.0D0)) :: bldr   = 1.0D0
  !+ad_vars  bldzl /4.0/ : vertical thickness of IFE blanket below chamber (m)
  real(kind(1.0D0)) :: bldzl  = 4.0D0
  !+ad_vars  bldzu /4.0/ : vertical thickness of IFE blanket above chamber (m)
  real(kind(1.0D0)) :: bldzu  = 4.0D0
  !+ad_vars  blmatf(3,0:maxmat) /.../ : IFE blanket material fractions
  real(kind(1.0D0)), dimension(3,0:maxmat) :: blmatf = (/ &
       0.05D0,0.05D0,0.05D0, &
       0.0D0, 0.0D0, 0.0D0,  &
       0.45D0,0.45D0,0.45D0, &
       0.0D0, 0.0D0, 0.0D0,  &
       0.20D0,0.20D0,0.20D0, &
       0.0D0, 0.0D0, 0.0D0,  &
       0.30D0,0.30D0,0.30D0, &
       0.0D0, 0.0D0, 0.0D0  /)
  !+ad_vars  blmatm(3,0:maxmat) : IFE blanket material masses (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: blmatm = 0.0D0
  !+ad_vars  blmatv(3,0:maxmat) : IFE blanket material volumes (m3)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: blmatv = 0.0D0
  !+ad_vars  blvol(3) : IFE blanket volume (m3)
  real(kind(1.0D0)), dimension(3) :: blvol = 0.0D0
  !+ad_vars  cdriv0 /154.3/ : IFE generic/laser driver cost at edrive=0 (M$)
  real(kind(1.0D0)) :: cdriv0 = 154.3D0
  !+ad_vars  cdriv1 /163.2/ : IFE low energy heavy ion beam driver cost
  !+ad_varc                   extrapolated to edrive=0 (M$)
  real(kind(1.0D0)) :: cdriv1 = 163.2D0
  !+ad_vars  cdriv2 /244.9/ : IFE high energy heavy ion beam driver cost
  !+ad_varc                   extrapolated to edrive=0 (M$)
  real(kind(1.0D0)) :: cdriv2 = 244.9D0
  !+ad_vars  chdzl /9.0/ : vertical thickness of IFE chamber below centre (m)
  real(kind(1.0D0)) :: chdzl = 9.0D0
  !+ad_vars  chdzu /9.0/ : vertical thickness of IFE chamber above centre (m)
  real(kind(1.0D0)) :: chdzu = 9.0D0
  !+ad_vars  chmatf(0:maxmat) : IFE chamber material fractions
  real(kind(1.0D0)), dimension(0:maxmat) :: chmatf = &
       (/1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/)
  !+ad_vars  chmatm(0:maxmat) : IFE chamber material masses (kg)
  real(kind(1.0D0)), dimension(0:maxmat) :: chmatm = 0.0D0
  !+ad_vars  chmatv(0:maxmat) : IFE chamber material volumes (m3)
  real(kind(1.0D0)), dimension(0:maxmat) :: chmatv = 0.0D0
  !+ad_vars  chrad /6.5/ : radius of IFE chamber (m)
  !+ad_varc                (iteration variable 84)
  real(kind(1.0D0)) :: chrad = 6.5D0
  !+ad_vars  chvol : IFE chamber volume (m3)
  real(kind(1.0D0)) :: chvol = 0.0D0
  !+ad_vars  dcdrv0 /111.4/ : IFE generic/laser driver cost gradient (M$/MJ)
  real(kind(1.0D0)) :: dcdrv0 = 111.4D0
  !+ad_vars  dcdrv1 /78.0/ : HIB driver cost gradient at low energy (M$/MJ)
  real(kind(1.0D0)) :: dcdrv1 = 78.0D0
  !+ad_vars  dcdrv2 /59.9/ : HIB driver cost gradient at high energy (M$/MJ)
  real(kind(1.0D0)) :: dcdrv2 = 59.9D0
  !+ad_vars  drveff /0.28/ : IFE driver wall plug to target efficiency (ifedrv=0)
  !+ad_varc                  (iteration variable 82)
  real(kind(1.0D0)) :: drveff = 0.28D0
  !+ad_vars  edrive /5.0D6/ : IFE driver energy (J)
  !+ad_varc                   (iteration variable 81)
  real(kind(1.0D0)) :: edrive = 5.0D6
  !+ad_vars  etadrv : IFE driver wall plug to target efficiency
  real(kind(1.0D0)) :: etadrv = 0.0D0
  !+ad_vars  etave(10) : IFE driver efficiency vs driver energy (ifedrv=-1)
  real(kind(1.0D0)), dimension(10) :: etave = (/ &
       0.082D0,0.079D0,0.076D0,0.073D0,0.069D0, &
       0.066D0,0.062D0,0.059D0,0.055D0,0.051D0 /)
  !+ad_vars  fbreed /0.51/ : fraction of breeder external to device core
  real(kind(1.0D0)) :: fbreed = 0.51D0
  !+ad_vars  fburn /0.3333/ : IFE burn fraction (fraction of tritium fused/target)
  real(kind(1.0D0)) :: fburn  = 0.3333D0
  !+ad_vars  flirad /0.78/ : radius of FLiBe inlet (m) (ifetyp=3)
  real(kind(1.0D0)) :: flirad = 0.78D0
  !+ad_vars  frrmax /1.0/ : f-value for maximum IFE repetition rate
  !+ad_varc                 (constraint equation 50, iteration variable 86)
  real(kind(1.0D0)) :: frrmax = 1.0D0
  !+ad_vars  fwdr /0.01/ : radial thickness of IFE first wall (m)
  real(kind(1.0D0)) :: fwdr = 0.01D0
  !+ad_vars  fwdzl /0.01/ : vertical thickness of IFE first wall below chamber (m)
  real(kind(1.0D0)) :: fwdzl = 0.01D0
  !+ad_vars  fwdzu /0.01/ : vertical thickness of IFE first wall above chamber (m)
  real(kind(1.0D0)) :: fwdzu = 0.01D0
  !+ad_vars  fwmatf(3,0:maxmat) /.../ : IFE first wall material fractions
  real(kind(1.0D0)), dimension(3,0:maxmat) :: fwmatf = (/ &
       0.05D0,0.05D0,0.05D0, &
       0.0D0, 0.0D0, 0.0D0,  &
       0.95D0,0.95D0,0.95D0, &
       0.0D0, 0.0D0, 0.0D0,  &
       0.0D0, 0.0D0, 0.0D0,  &
       0.0D0, 0.0D0, 0.0D0,  &
       0.0D0, 0.0D0, 0.0D0,  &
       0.0D0, 0.0D0, 0.0D0  /)
  !+ad_vars  fwmatm(3,0:maxmat) : IFE first wall material masses (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: fwmatm = 0.0D0
  !+ad_vars  fwmatv(3,0:maxmat) : IFE first wall material volumes (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: fwmatv = 0.0D0
  !+ad_vars  fwvol(3) : IFE first wall volume (m3)
  real(kind(1.0D0)), dimension(3) :: fwvol = 0.0D0
  !+ad_vars  gain : IFE target gain
  real(kind(1.0D0)) :: gain = 0.0D0
  !+ad_vars  gainve(10) /.../ : IFE target gain vs driver energy (ifedrv=-1)
  real(kind(1.0D0)), dimension(10) :: gainve = (/ &
        60.0D0, 95.0D0,115.0D0,125.0D0,133.0D0, &
       141.0D0,152.0D0,160.0D0,165.0D0,170.0D0 /)
  !+ad_vars  ife /0/ : switch for IFE option
  !+ad_varc            (set via <CODE>device.dat</CODE>):<UL>
  !+ad_varc       <LI> = 0 use tokamak, RFP or stellarator model;
  !+ad_varc       <LI> = 1 use IFE model</UL>
  integer :: ife = 0
  !+ad_vars  ifedrv /2/ : switch for type of IFE driver:<UL>
  !+ad_varc          <LI> = -1 use gainve, etave for gain and driver efficiency;
  !+ad_varc          <LI> =  0 use tgain, drveff for gain and driver efficiency;
  !+ad_varc          <LI> =  1 use laser driver based on SOMBRERO design;
  !+ad_varc          <LI> =  2 use heavy ion beam driver based on OSIRIS</UL>
  integer :: ifedrv = 2
  !+ad_vars  ifetyp /0/ : switch for type of IFE device build:<UL>
  !+ad_varc          <LI> = 0 generic (cylindrical) build;
  !+ad_varc          <LI> = 1 OSIRIS-like build;
  !+ad_varc          <LI> = 2 SOMBRERO-like build;
  !+ad_varc          <LI> = 3 HYLIFE-II-like build</UL>
  integer :: ifetyp = 0
  !+ad_vars  mcdriv /1.0/ : IFE driver cost multiplier
  real(kind(1.0D0)) :: mcdriv = 1.0D0
  !+ad_vars  mflibe : total mass of FLiBe (kg)
  real(kind(1.0D0)) :: mflibe = 0.0D0
  !+ad_vars  pdrive /23.0D6/ : IFE driver power reaching target (W)
  !+ad_varc                    (iteration variable 85)
  real(kind(1.0D0)) :: pdrive = 23.0D6
  !+ad_vars  pifecr /10.0/ : IFE cryogenic power requirements (MW)
  real(kind(1.0D0)) :: pifecr = 10.0D0
  !+ad_vars  ptargf /2.0/ : IFE target factory power at 6 Hz repetition rate (MW)
  real(kind(1.0D0)) :: ptargf = 2.0D0
  !+ad_vars  r1 : IFE device radial build (m)
  real(kind(1.0D0)) :: r1 = 0.0D0
  !+ad_vars  r2 : IFE device radial build (m)
  real(kind(1.0D0)) :: r2 = 0.0D0
  !+ad_vars  r3 : IFE device radial build (m)
  real(kind(1.0D0)) :: r3 = 0.0D0
  !+ad_vars  r4 : IFE device radial build (m)
  real(kind(1.0D0)) :: r4 = 0.0D0
  !+ad_vars  r5 : IFE device radial build (m)
  real(kind(1.0D0)) :: r5 = 0.0D0
  !+ad_vars  r6 : IFE device radial build (m)
  real(kind(1.0D0)) :: r6 = 0.0D0
  !+ad_vars  r7 : IFE device radial build (m)
  real(kind(1.0D0)) :: r7 = 0.0D0
  !+ad_vars  reprat : IFE driver repetition rate (Hz)
  real(kind(1.0D0)) :: reprat = 0.0D0
  !+ad_vars  rrmax /20.0/ : maximum IFE repetition rate (Hz)
  real(kind(1.0D0)) :: rrmax = 20.0D0
  !+ad_vars  shdr /1.7/ : radial thickness of IFE shield (m)
  real(kind(1.0D0)) :: shdr = 1.7D0
  !+ad_vars  shdzl /5.0/ : vertical thickness of IFE shield below chamber (m)
  real(kind(1.0D0)) :: shdzl = 5.0D0
  !+ad_vars  shdzu /5.0/ : vertical thickness of IFE shield above chamber (m)
  real(kind(1.0D0)) :: shdzu  = 5.0D0
  !+ad_vars  shmatf(3,0:maxmat) /.../ : IFE shield material fractions
  real(kind(1.0D0)), dimension(3,0:maxmat) :: shmatf = (/ &
       0.05D0,0.05D0,0.05D0, &
       0.19D0,0.19D0,0.19D0, &
       0.0D0, 0.0D0, 0.0D0,  &
       0.0D0, 0.0D0, 0.0D0,  &
       0.0D0, 0.0D0, 0.0D0,  &
       0.665D0,0.665D0,0.665D0, &
       0.095D0,0.095D0,0.095D0, &
       0.0D0, 0.0D0, 0.0D0  /)
  !+ad_vars  shmatm(3,0:maxmat) : IFE shield material masses (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: shmatm = 0.0D0
  !+ad_vars  shmatv(3,0:maxmat) : IFE shield material volumes (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: shmatv = 0.0D0
  !+ad_vars  shvol(3) : IFE shield volume (m3)
  real(kind(1.0D0)), dimension(3) :: shvol = 0.0D0
  !+ad_vars  sombdr /2.7/ : radius of cylindrical blanket section below chamber (ifetyp=2)
  real(kind(1.0D0)) :: sombdr = 2.7D0
  !+ad_vars  somtdr /2.7/ : radius of cylindrical blanket section above chamber (ifetyp=2)
  real(kind(1.0D0)) :: somtdr = 2.7D0
  !+ad_vars  tdspmw /0.01/ FIX : IFE target delivery system power (MW)
  real(kind(1.0D0)) :: tdspmw = 0.01D0
  !+ad_vars  tfacmw : IFE target factory power (MW)
  real(kind(1.0D0)) :: tfacmw = 0.0D0
  !+ad_vars  tgain /85.0/ : IFE target gain (if ifedrv = 0)
  !+ad_varc                 (iteration variable 83)
  real(kind(1.0D0)) :: tgain = 85.0D0
  !+ad_vars  uccarb /50.0/ : cost of carbon cloth ($/kg)
  real(kind(1.0D0)) :: uccarb = 50.0D0
  !+ad_vars  ucconc /0.1/ : cost of concrete ($/kg)
  real(kind(1.0D0)) :: ucconc = 0.1D0
  !+ad_vars  ucflib /84.0/ : cost of FLiBe ($/kg)
  real(kind(1.0D0)) :: ucflib = 84.0D0
  !+ad_vars  uctarg /0.3/ : cost of IFE target ($/target)
  real(kind(1.0D0)) :: uctarg = 0.3D0
  !+ad_vars  v1dr /0.0/ : radial thickness of IFE void between first wall and blanket (m)
  real(kind(1.0D0)) :: v1dr = 0.0D0
  !+ad_vars  v1dzl /0.0/ : vertical thickness of IFE void 1 below chamber (m)
  real(kind(1.0D0)) :: v1dzl = 0.0D0
  !+ad_vars  v1dzu /0.0/ : vertical thickness of IFE void 1 above chamber (m)
  real(kind(1.0D0)) :: v1dzu = 0.0D0
  !+ad_vars  v1matf(3,0:maxmat) /.../ : IFE void 1 material fractions
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v1matf = (/ &
       1.0D0, 1.0D0, 1.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0  /)
  !+ad_vars  v1matm(3,0:maxmat) : IFE void 1 material masses (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v1matm = 0.0D0
  !+ad_vars  v1matv(3,0:maxmat) : IFE void 1 material volumes (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v1matv = 0.0D0
  !+ad_vars  v1vol(3) : IFE void 1 volume (m3)
  real(kind(1.0D0)), dimension(3) :: v1vol = 0.0D0
  !+ad_vars  v2dr /2.0/ : radial thickness of IFE void between blanket and shield (m)
  real(kind(1.0D0)) :: v2dr = 2.0D0
  !+ad_vars  v2dzl /7.0/ : vertical thickness of IFE void 2 below chamber (m)
  real(kind(1.0D0)) :: v2dzl = 7.0D0
  !+ad_vars  v2dzu /7.0/ : vertical thickness of IFE void 2 above chamber (m)
  real(kind(1.0D0)) :: v2dzu = 7.0D0
  !+ad_vars  v2matf(3,0:maxmat) /.../ : IFE void 2 material fractions
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v2matf = (/ &
       1.0D0, 1.0D0, 1.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0  /)
  !+ad_vars  v2matm(3,0:maxmat) : IFE void 2 material masses (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v2matm = 0.0D0
  !+ad_vars  v2matv(3,0:maxmat) : IFE void 2 material volumes (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v2matv = 0.0D0
  !+ad_vars  v2vol(3) : IFE void 2 volume (m3)
  real(kind(1.0D0)), dimension(3) :: v2vol = 0.0D0
  !+ad_vars  v3dr /43.3/ : radial thickness of IFE void outside shield (m)
  real(kind(1.0D0)) :: v3dr   = 43.3D0
  !+ad_vars  v3dzl /30.0/ : vertical thickness of IFE void 3 below chamber (m)
  real(kind(1.0D0)) :: v3dzl  = 30.0D0
  !+ad_vars  v3dzu /20.0/ : vertical thickness of IFE void 3 above chamber (m)
  real(kind(1.0D0)) :: v3dzu  = 20.0D0
  !+ad_vars  v3matf(3,0:maxmat) /.../ : IFE void 3 material fractions
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v3matf = (/ &
       1.0D0, 1.0D0, 1.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0  /)
  !+ad_vars  v3matm(3,0:maxmat) : IFE void 3 material masses (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v3matm = 0.0D0
  !+ad_vars  v3matv(3,0:maxmat) : IFE void 3 material volumes (kg)
  real(kind(1.0D0)), dimension(3,0:maxmat) :: v3matv = 0.0D0
  !+ad_vars  v3vol(3) : IFE void 3 volume (m3)
  real(kind(1.0D0)), dimension(3) :: v3vol = 0.0D0
  !+ad_vars  zl1 : IFE vertical build below centre (m)
  real(kind(1.0D0)) :: zl1 = 0.0D0
  !+ad_vars  zl2 : IFE vertical build below centre (m)
  real(kind(1.0D0)) :: zl2 = 0.0D0
  !+ad_vars  zl3 : IFE vertical build below centre (m)
  real(kind(1.0D0)) :: zl3 = 0.0D0
  !+ad_vars  zl4 : IFE vertical build below centre (m)
  real(kind(1.0D0)) :: zl4 = 0.0D0
  !+ad_vars  zl5 : IFE vertical build below centre (m)
  real(kind(1.0D0)) :: zl5 = 0.0D0
  !+ad_vars  zl6 : IFE vertical build below centre (m)
  real(kind(1.0D0)) :: zl6 = 0.0D0
  !+ad_vars  zl7 : IFE vertical build below centre (m)
  real(kind(1.0D0)) :: zl7 = 0.0D0
  !+ad_vars  zu1 : IFE vertical build above centre (m)
  real(kind(1.0D0)) :: zu1 = 0.0D0
  !+ad_vars  zu2 : IFE vertical build above centre (m)
  real(kind(1.0D0)) :: zu2 = 0.0D0
  !+ad_vars  zu3 : IFE vertical build above centre (m)
  real(kind(1.0D0)) :: zu3 = 0.0D0
  !+ad_vars  zu4 : IFE vertical build above centre (m)
  real(kind(1.0D0)) :: zu4 = 0.0D0
  !+ad_vars  zu5 : IFE vertical build above centre (m)
  real(kind(1.0D0)) :: zu5 = 0.0D0
  !+ad_vars  zu6 : IFE vertical build above centre (m)
  real(kind(1.0D0)) :: zu6 = 0.0D0
  !+ad_vars  zu7 : IFE vertical build above centre (m)
  real(kind(1.0D0)) :: zu7 = 0.0D0

end module ife_variables

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
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>pulse.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  05/11/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  afw /0.005/ : inner radius of each first wall structural tube (m)
  real(kind(1.0D0)) :: afw = 0.005D0
  !+ad_vars  bctmp /320.0/ : first wall bulk coolant temperature (C)
  real(kind(1.0D0)) :: bctmp = 320.0D0
  !+ad_vars  bfw : outer radius of each first wall structural tube (m)
  !+ad_varc        (0.5 * average of fwith and fwoth)
  real(kind(1.0D0)) :: bfw = 0.0D0
  !+ad_vars  coolp /15.5D6/ : first wall coolant pressure (Pa)
  real(kind(1.0D0)) :: coolp = 15.5D6
  !+ad_vars  dtstor /300.0/ : maximum allowable temperature change in stainless
  !+ad_varc                   steel thermal storage block (K) (istore=3)
  real(kind(1.0D0)) :: dtstor = 300.0D0
  !+ad_vars  fwlife : first wall lifetime (y)
  real(kind(1.0D0)) :: fwlife = 0.0D0
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
  !+ad_vars  tmprse /40.0/ : first wall coolant temperature rise (C)
  real(kind(1.0D0)) :: tmprse = 40.0D0
  !+ad_vars  tpeak : peak first wall temperature (C)
  real(kind(1.0D0)) :: tpeak = 0.0D0

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
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>start.h90</CODE>.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  05/11/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  Work File Notes in F/MPE/MOD/CAG/PROCESS/PULSE
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  !+ad_vars  ftaue : factor in energy confinement time formula
  real(kind(1.0D0)) :: ftaue = 0.0D0
  !+ad_vars  gtaue : factor in energy confinement time formula
  real(kind(1.0D0)) :: gtaue  = 0.0D0
  !+ad_vars  nign : electron density at ignition (start-up) (/m3)
  real(kind(1.0D0)) :: nign  = 0.0D0
  !+ad_vars  ptaue : exponent in energy confinement time formula
  real(kind(1.0D0)) :: ptaue  = 0.0D0
  !+ad_vars  qtaue : exponent in energy confinement time formula
  real(kind(1.0D0)) :: qtaue  = 0.0D0
  !+ad_vars  rtaue : exponent in energy confinement time formula
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
  !+ad_desc  It is derived from <CODE>include</CODE> file
  !+ad_desc  <CODE>fispact.h90</CODE>.
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
