!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine initial

  !+ad_name  initial
  !+ad_summ  Routine to initialise all the COMMON block variables
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine initialises all the COMMON block variables.
  !+ad_desc  N.B. Many of these variables are re-initialised elsewhere in the
  !+ad_desc  code, but are set to zero in this routine anyway.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  ineq.h90
  !+ad_call  cdriv.h90
  !+ad_call  times.h90
  !+ad_call  divrt.h90
  !+ad_call  build.h90
  !+ad_call  tfcoil.h90
  !+ad_call  pfcoil.h90
  !+ad_call  vltcom.h90
  !+ad_call  pwrcom.h90
  !+ad_call  fwblsh.h90
  !+ad_call  htpwr.h90
  !+ad_call  cost.h90
  !+ad_call  bldgvol.h90
  !+ad_call  estocom.h90
  !+ad_call  bldgcom.h90
  !+ad_call  struccom.h90
  !+ad_call  torsdat.h90
  !+ad_call  vaccom.h90
  !+ad_call  blanket.h90
  !+ad_call  pulse.h90
  !+ad_call  stella.h90
  !+ad_call  rfp.h90
  !+ad_call  ife.h90
  !+ad_call  devtyp
  !+ad_call  ifeini
  !+ad_call  stinit
  !+ad_hist  08/10/96 PJK Initial upgraded version
  !+ad_hist  20/01/97 PJK Added htpmw
  !+ad_hist  22/01/97 PJK Subsumed heattr.h, heatrinp.h and pfelect.h into
  !+ad_hisc               htpwr.h
  !+ad_hist  05/02/97 PJK Added Martensitic steel fractions fms...
  !+ad_hist  13/02/97 PJK Changed initial value of fwlife to zero (no
  !+ad_hisc               longer an input parameter, but calculated)
  !+ad_hist  21/03/97 PJK Added call to initialise IFE variables
  !+ad_hisc               and added new iteration variables 81--86
  !+ad_hist  18/11/97 PJK Added ITER-97 scaling laws, IVMS,DRTOP,DZTOP
  !+ad_hist  01/04/98 PJK Added ITER-96P scaling law, IGNITE, DNLA
  !+ad_hist  24/04/98 PJK Added IMPC,IMPFE,IMPO,FKBLKT
  !+ad_hist  17/07/98 PJK Added PTHRMW
  !+ad_hist  08/10/98 PJK Added new ITER H98 scaling laws
  !+ad_hist  19/01/99 PJK Added IGEOM, POWERHT
  !+ad_hist  19/05/99 PJK Added new availability model variables
  !+ad_hist  06/07/99 PJK Added BCRITSC, JCRITSC, TCRITSC
  !+ad_hist  25/04/02 PJK Added ZEFFDIV
  !+ad_hist  15/06/04 PJK Added IPRIMHTP
  !+ad_hist  22/05/06 PJK Added IFALPHAP
  !+ad_hist  22/05/07 PJK Added hydrogen plant variables
  !+ad_hist  21/03/11 PJK Changed default value of FEFFCD to 1.0
  !+ad_hist  26/07/11 PJK Added JCRIT_MODEL
  !+ad_hist  09/11/11 PJK Removed ICULCR
  !+ad_hist  19/09/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Removed IVMS
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Removed physics variables from list
  !+ad_hist  15/10/12 PJK Removed numerics variables from list
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  include 'ineq.h90'
  include 'cdriv.h90'
  include 'times.h90'
  include 'divrt.h90'
  include 'build.h90'
  include 'tfcoil.h90'
  include 'pfcoil.h90'
  include 'vltcom.h90'
  include 'pwrcom.h90'
  include 'fwblsh.h90'
  include 'htpwr.h90'
  include 'cost.h90'
  include 'bldgvol.h90'
  include 'estocom.h90'
  include 'bldgcom.h90'
  include 'struccom.h90'
  include 'torsdat.h90'
  include 'vaccom.h90'
  include 'blanket.h90'
  include 'pulse.h90'
  include 'stella.h90'
  include 'rfp.h90'
  include 'ife.h90'

  !  Arguments

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Quantities used in inequality constraints

  auxmin   = 0.1D0
  betpmx   = 0.19D0
  bmxlim   = 12.0D0
  dtmpmx   = 1.0D3
  fauxmn   = 1.0D0
  fbeta    = 1.0D0
  fbetap   = 1.0D0
  fbetatry = 1.0D0
  fdene    = 1.0D0
  fdivcol  = 1.0D0
  fdtmp    = 1.0D0
  ffuspow  = 1.0D0
  fgamcd   = 1.0D0
  fhldiv   = 1.0D0
  fiooic   = 0.5D0
  fipir    = 1.0D0
  fjohc    = 1.0D0
  fjohc0   = 1.0D0
  fjprot   = 1.0D0
  fjtfc    = 1.0D0
  fmva     = 1.0D0
  fpeakb   = 1.0D0
  fpinj    = 1.0D0
  fpnetel  = 1.0D0
  fportsz  = 1.0D0
  fptemp   = 1.0D0
  fq       = 1.0D0
  fqval    = 1.0D0
  frfpf    = 1.0D0
  frfptf   = 1.0D0
  frminor  = 1.0D0
  fstrcase = 1.0D0
  fstrcond = 1.0D0
  ftburn   = 1.0D0
  ftcycl   = 1.0D0
  ftmargtf = 1.0D0
  ftohs    = 1.0D0
  ftpeak   = 1.0D0
  fvdump   = 1.0D0
  fvs      = 1.0D0
  fwalld   = 1.0D0
  gammax   = 2.0D0
  mvalim   = 40.0D0
  pnetelin = 1000.0D0
  powfmax  = 1.0D3
  tbrnmn   = 1.0D0
  tcycmn   = 0.0D0
  tohsmn   = 1.0D0
  tpkmax   = 600.0D0
  walalw   = 1.0D0

  !  Current drive quantities

  beamwd   = 0.31D0
  bigq     = 0.0D0
  bootipf  = 0.0D0
  bscfmax  = 0.9D0
  cboot    = 1.0D0
  cnbeam   = 0.0D0
  echpwr   = 0.0D0
  echpwr0  = 2.0D6
  echwpow  = 0.0D0
  enbeam   = 1.0D3
  etaech   = 0.5D0
  etalh    = 0.5D0
  etanbi   = 0.5D0
  etaof    = 0.5D0
  feffcd   = 1.0D0
  frbeam   = 1.05D0
  ftritbm  = 1.0D-6
  gamcd    = 0.0D0
  iefrf    = 5
  irfcd    = 1
  pheat    = 0.0D0
  pinjalw  = 25.0D0
  pinje    = 0.0D0
  pinji    = 0.0D0
  plhybd   = 0.0D0
  pnbeam   = 0.0D0
  pofcd    = 0.0D0
  pwplh    = 0.0D0
  pwpnb    = 0.0D0
  taubeam  = 0.0D0
  tbeamin  = 3.0D0

  !  Times for different phases

  tburn    = 227.9D0
  tburn0   = 0.0D0
  tdown    = 0.0D0
  tdwell   = 100.0D0
  theat    = 10.0D0
  tim(:) = 0.0D0
  tohs     = 30.0D0
  tohsin   = 0.0D0
  tpulse   = 0.0D0
  tqnch    = 15.0D0
  tramp    = 15.0D0

  !  Divertor model

  adas     = 0.0D0
  anginc   = 0.262D0
  bpsout   = 0.60D0
  c1div    = 0.45D0
  c2div    = -7.0D0
  c3div    = 0.54D0
  c4div    = -3.60D0
  c5div    = 0.7D0
  c6div    = 0.0D0
  delld    = 1.00D0
  dendiv   = 0.0D0
  densin   = 0.0D0
  divclfr  = 0.3D0
  divdens  = 1.0D4
  divdum   = 0
  divmas   = 0.0D0
  divplt   = 0.035D0
  divsur   = 0.0D0
  fdfs     = 10.0D0
  fdiva    = 1.11D0
  fgamp    = 1.0D0
  fhout    = 0.0D0
  fififi   = 4.0D-3
  frrp     = 0.40D0
  hldiv    = 0.0D0
  hldivlim = 5.0D0
  ksic     = 0.80D0
  lamp     = 0.0D0
  minstang = 0.0D0
  omegan   = 1.00D0
  omlarg   = 0.0D0
  plsepo   = 1.50D0
  ppdivr   = 0.0D0
  prn1     = 0.285D0
  ptpdiv   = 0.0D0
  rconl    = 0.00D0
  rlclolcn = 0.0D0
  rlenmax  = 0.5D0
  rsrd     = 0.0D0
  rstrko   = 0.0D0
  tconl    = 0.0D0
  tdiv     = 0.0D0
  tsep     = 0.0D0
  xparain  = 2.1D3
  xpertin  = 2.0D0
  zeffdiv  = 1.0D0

  !  Build description

  aplasmin = 0.25D0
  bcylth   = 0.0D0
  blnkith  = 0.115D0
  blnkoth  = 0.235D0
  bore     = 1.42D0
  ddwex    = 0.07D0
  ddwi     = 0.07D0
  fmsbc    = 0.0D0
  fmsbl    = 0.0D0
  fmsdwe   = 0.0D0
  fmsdwi   = 0.0D0
  fmsfw    = 0.0D0
  fmsoh    = 0.0D0
  fmssh    = 0.0D0
  fmstf    = 0.0D0
  fwarea   = 0.0D0
  fwith    = 0.035D0
  fwoth    = 0.035D0
  gapds    = 0.0D0
  gapoh    = 0.08D0
  gapomin  = 0.21D0
  gapsto   = 0.0D0
  hmax     = 0.0D0
  hr1      = 0.0D0
  iohcl    = 1
  ohcth    = 0.63D0
  prtsz    = 0.0D0
  prtszreq = 0.0D0
  rbld     = 0.0D0
  rinboard = 0.651D0
  rsldi    = 0.0D0
  rsldo    = 0.0D0
  rtfcin   = 0.0D0
  rtot     = 0.0D0
  scrapli  = 0.14D0
  scraplo  = 0.15D0
  shldith  = 0.69D0
  shldoth  = 1.05D0
  shldtth  = 0.60D0
  tfcth    = 0.9D0
  tfootfi  = 1.8D0
  tfthko   = 0.0D0
  vgap     = 0.0D0
  vgap2    = 0.163D0
  vgaptf   = 0.0D0

  !  TF coil quantities

  acasetf  = 0.0D0
  acndttf  = 0.0D0
  acond    = 0.0D0
  acstf    = 0.0D0
  aiwp     = 0.0D0
  alstrtf  = 0.0D0
  arealeg  = 0.0D0
  aspcstf  = 1.0D0
  aswp     = 0.0D0
  avwp     = 0.0D0
  bcritsc  = 24.0D0
  bmaxtf   = 0.0D0
  bmaxtfrp = 0.0D0
  borev    = 0.0D0
  casestr  = 0.0D0
  casfact  = 4.0D0
  casthi   = 0.05D0
  casths   = 0.07D0
  cdtfleg  = 1.0D6
  cforce   = 0.0D0
  cph2o    = 4180.0D0
  cpttf    = 3.79D4
  csutf    = 1.4D9
  csytf    = 8.25D8
  dcase    = 8000.0D0
  dcond(:) = 9000.0D0
  dcopper  = 8900.0D0
  deflect  = 0.0D0
  denh2o   = 985.0D0
  drtop    = 0.0D0
  dthet(:) = 0.0D0
  dztop    = 0.0D0
  estotf   = 0.0D0
  etapump  = 0.8D0
  eyins    = 1.5D10
  eyoung(:) = 0.0D0
  eystl    = 2.0D11
  eywp     = 6.6D8
  farc4tf  = 0.7D0
  fcoolcp  = 0.3D0
  fcutfsu  = 0.69D0
  frhocp   = 1.0D0
  isumattf = 1
  jcrit_model = 0
  itfmod   = 1
  itfsup   = 1
  jcritsc  = 2.225D10
  jbus     = 1.25D6
  jeff(:) = 0.0D0
  jwdgcrt  = 0.0D0
  jwdgpro  = 0.0D0
  jwptf    = 0.0D0
  kcp      = 330.0D0
  kh2o     = 0.651D0
  magnt    = 2
  muh2o    = 4.71D-4
  ncool    = 0.0D0
  oacdcp   = 1.4D7
  poisson  = 0.30D0
  ppump    = 0.0D0
  prescp   = 0.0D0
  ptempalw = 200.0D0
  radctf(:) = 0.0D0
  radtf(:) = 0.0D0
  rbmax    = 0.0D0
  rcool    = 0.005D0
  rhocp    = 0.0D0
  rhotfleg = 0.0D0
  ripmax   = 5.0D0
  ripple   = 0.0D0
  ritfc    = 0.0D0
  rjtfsual = 0.0D0
  rnltf    = 0.0D0
  sigrad   = 0.0D0
  sigrcon  = 0.0D0
  sigrtf(:) = 0.0D0
  sigtan   = 0.0D0
  sigtcon  = 0.0D0
  sigttf(:) = 0.0D0
  sigver   = 0.0D0
  sigvert  = 0.0D0
  strncon  = -0.005D0
  strtf1   = 0.0D0
  strtf2   = 0.0D0
  tcoolin  = 40.0D0
  tcpav    = 100.0D0
  tcpav2   = 0.0D0
  tcpmax   = 0.0D0
  tcritsc  = 16.0D0
  tdmptf   = 10.0D0
  tfareain = 0.0D0
  tfboreh  = 0.0D0
  tfbusl   = 0.0D0
  tfbusmas = 0.0D0
  tfckw    = 0.0D0
  tfcmw    = 0.0D0
  tfcpmw   = 0.0D0
  tficrn   = 0.0D0
  tfind    = 0.0D0
  tflegmw  = 0.0D0
  tflegres = 2.5D-8
  tfleng   = 0.0D0
  tfno     = 16.0D0
  tfocrn   = 0.0D0
  tfsai    = 0.0D0
  tfsao    = 0.0D0
  tftmp    = 4.5D0
  thicndut = 8.0D-4
  thkcas   = 0.3D0
  thkwp    = 0.0D0
  thwcndut = 3.0D-3
  tinstf   = 0.01D0
  tmargmin = 2.5D0
  tmargtf  = 0.0D0
  tmaxpro  = 150.0D0
  tmpcry   = 4.5D0
  turnstf  = 0.0D0
  vcool    = 20.0D0
  vdalw    = 20.0D0
  vforce   = 0.0D0
  vftf     = 0.4D0
  volcp    = 0.0D0
  voltfleg = 0.0D0
  vtfkv    = 0.0D0
  vtfskv   = 0.0D0
  whtcas   = 0.0D0
  whtcon   = 0.0D0
  whtconcu = 0.0D0
  whtconsc = 0.0D0
  whtconsh = 0.0D0
  whtcp    = 0.0D0
  whttf    = 0.0D0
  whttflgs = 0.0D0
  wpvf     = 0.0D0
  wtbc     = 0.0D0
  wwp1     = 0.0D0
  wwp2     = 0.0D0
  xarc(:)  = 0.0D0
  xctfc(:) = 0.0D0
  yarc(:)  = 0.0D0
  yctfc(:) = 0.0D0

  !  PF coil data

  ac1oh    = 0.0D0
  acsoh    = 3.0D-4
  alfapf   = 5.0D-10
  bmaxoh   = 0.0D0
  bmaxoh0  = 0.0D0
  bpf(:) = 0.0D0
  cohbof   = 0.0D0
  cohbop   = 0.0D0
  coheof   = 1.85D7
  cpt(:,:) = 0.0D0
  cptoh    = 0.0D0
  cptdin(:) = 4.0D4
  curpfb(:) = 0.0D0
  curpff(:) = 0.0D0
  curpfs(:) = 0.0D0
  fcohbof  = 0.9D0
  fcohbop  = 0.9D0
  fcuoh    = 0.4D0
  ipfloc(:) = 3
  ipfloc(1) = 1
  ipfloc(2) = 2
  ipfloc(3) = 3
  ipfres   = 0
  isumatpf = 1
  ncirt    = 0
  ncls(:) = 0
  ncls(1)  = 2
  ncls(2)  = 2
  ncls(3)  = 2
  ncls(4)  = 1 
  nfxfh    = 7
  ngrp     = 3
  nohc     = 0
  ohhghf   = 0.71D0
  pfclres  = 2.5D-8
  powohres = 0.0D0
  powpfres = 0.0D0
  ra(:)    = 0.0D0
  rb(:)    = 0.0D0
  ric(:)   = 0.0D0
  rjconpf(:) = 3.0D7
  rjohc    = 0.0D0
  rjohc0   = 0.0D0
  rjpfalw(:) = 0.0D0
  rohc     = 0.0D0
  routr    = 1.5D0
  rpf(:)   = 0.0D0
  rpf1     = 0.0D0
  rpf2     = -1.63D0
  sccufac = 0.0188D0
  sigpfalw = 335.0D0
  turns(:) = 0.0D0
  vf(:)    = 0.3D0
  vfohc    = 0.4D0
  waves(:,:) = 0.0D0
  whtpf    = 0.0D0
  whtpfs   = 0.0D0
  wtc(:) = 0.0D0
  wts(:) = 0.0D0
  zh(:) = 0.0D0
  zl(:) = 0.0D0
  zpf(:) = 0.0D0
  zref(:) = 1.0D0
  zref(1)  = 3.6D0
  zref(2)  = 1.2D0
  zref(3)  = 2.5D0

  !  PF coil volt second and inductance information

  vsbn     = 0.0D0
  vsefbn   = 0.0D0
  vsefsu   = 0.0D0
  vseft    = 0.0D0
  vsoh     = 0.0D0
  vsohbn   = 0.0D0
  vsohsu   = 0.0D0
  vssu     = 0.0D0
  vstot    = 0.0D0
  sxlg(:,:) = 0.0D0

  !  PF coil power conversion

  acptmax  = 0.0D0
  ensxpfm  = 0.0D0
  pfckts   = 0.0D0
  spfbusl  = 0.0D0
  spsmva   = 0.0D0
  srcktpm  = 0.0D0
  vpfskv   = 0.0D0

  !  First wall, blanket and shield

  bktlife  = 0.0D0
  coolmass = 0.0D0
  cryomass = 0.0D0
  denstl   = 7800.0D0
  dewmkg   = 0.0D0
  emult    = 1.27D0
  fblbe    = 0.60D0
  fblli2o  = 0.08D0
  fbllipb  = 0.68D0
  fblli    = 0.0D0
  fblss    = 0.07D0
  fblvd    = 0.0D0
  fhole    = 0.15D0
  fvolbi   = 1.0D0
  fvolbo   = 0.75D0
  fvolcry  = 1.4D0
  fvoldw   = 1.4D0
  fvolsi   = 0.64D0
  fvolso   = 0.64D0
  fwclfr   = 0.15D0
  fwmass   = 0.0D0
  pnucblkt = 0.0D0
  pnuccp   = 0.0D0
  pnucloss = 0.0D0
  pnucshld = 0.0D0
  ptfnuc   = 0.0D0
  vdewex   = 0.0D0
  vdewin   = 0.0D0
  vfblkt   = 0.25D0
  vfshld   = 0.25D0
  volblkt  = 0.0D0
  volshld  = 0.0D0
  whtshld  = 0.0D0
  wpenshld = 0.0D0
  wtshldi  = 0.0D0
  wtshldo  = 0.0D0
  astr = 2
  bstr = 1
  costr = 2
  estr = 1
  etacp = 0.75D0
  etafp = 0.75D0
  etahp = 0.85D0
  etainp = 0.85D0
  etalp = 0.85D0
  fkblkt = 1.0D0
  lblnkt = 1
  nipfwh = 1
  nlpfwh = 1
  pc = 0.005D0
  ph = 8.6D0
  pin = 0.2D0
  pr = 1.0D0
  sgeff = 1.0D0
  smstr = 1
  xdi = 2.0D0
  xdo = 2.4D0
  xpf = 8.6D0
  xtb = 350.0D0
  xtfi = 200.0D0
  xtfo = 300.0D0

  !  Pulsed reactor

  afw = 0.005D0
  bctmp = 320.0D0
  bfw = 0.0D0
  coolp = 15.5D6
  dtstor = 300.0D0
  fwlife = 0.0D0
  istore = 1
  itcycl = 1
  lpulse = 0
  tmprse = 40.0D0
  tpeak = 0.0D0

  !  Heat transport / power

  baseel   = 5.0D6
  crypmw   = 0.0D0
  ctht     = 0.0D0
  etath    = 0.35D0
  facht    = 0.0D0
  fauxbop  = 0.06D0
  fcsht    = 0.0D0
  ffwlg    = 1.0D0
  fgrosbop = 0.0D0
  fmgdmw   = 0.0D0
  helpow   = 0.0D0
  htpmw    = 10.0D0
  iprimhtp = 0
  pacpmw   = 0.0D0
  peakmva  = 0.0D0
  pfwdiv   = 0.0D0
  pgrossmw = 0.0D0
  pinjht   = 0.0D0
  pinjwp   = 0.0D0
  pnetelmw = 0.0D0
  ppmphemw = 0.0D0
  priheat  = 0.0D0
  psecht   = 0.0D0
  pthermmw = 0.0D0
  pwpm2    = 150.0D0
  rnihx    = 0.0D0
  rnphx    = 0.0D0
  tfacpd   = 0.0D0
  tlvpmw   = 0.0D0
  trithtmw = 15.0D0
  vachtmw  = 0.5D0

  !  Hydrogen plant

  chplant  = 0.0D0
  etahhten = 1.35D0
  etahhtex = 1.12D0
  etahlte  = 0.75D0
  etahth   = 0.5D0
  helecmw  = 0.0D0
  hpower   = 0.0D0
  hthermmw = 0.0D0
  hvolume  = 0.0D0
  ihplant  = 0
  uchhten  = 1350.0D0
  uchhtex  = 900.0D0
  uchlte   = 400.0D0
  uchth    = 700.0D0

  !  Cost information

  abktflnc = 20.0D0
  adivflnc = 25.0D0
  blkcst   = 0.0D0
  c221     = 0.0D0
  c222     = 0.0D0
  capcost  = 0.0D0
  cdcost   = 0.0D0
  cdirt    = 0.0D0
  cdrlife  = 0.0D0
  cfactr   = 0.75D0
  cfind(1) = 0.244D0
  cfind(2) = 0.244D0
  cfind(3) = 0.244D0
  cfind(4) = 0.29D0
  coe      = 0.0D0
  coecap   = 0.0D0
  coefuelt = 0.0D0
  coeoam   = 0.0D0
  concost  = 0.0D0
  cplife   = 0.0D0
  cpstcst  = 0.0D0
  cpstflnc  = 10.0D0
  crctcore = 0.0D0
  decomf   = 0.1D0
  dintrt   = 0.0D0
  divcst   = 0.0D0
  divlife  = 0.0D0
  dtlife   = 0.0D0
  fcap0    = 1.165D0
  fcap0cp  = 1.08D0
  fcdfuel  = 0.1D0
  fcr0     = 0.0966D0
  fkind    = 1.0D0
  fwallcst = 0.0D0
  iavail   = 0
  ifueltyp = 0
  ipnet    = 0
  ireactor = 1
  lsa      = 4
  moneyint = 0.0D0
  ratecdol = 0.0435D0
  tbktrepl = 0.5D0
  tcomrepl = 0.5D0
  tdivrepl = 0.25D0
  tlife    = 30.0D0
  uubop    = 0.02D0
  uucd     = 0.02D0
  uudiv    = 0.04D0
  uufuel   = 0.02D0
  uufw     = 0.04D0
  uumag    = 0.02D0
  uuves    = 0.04D0

  !  Unit costs

  cconfix  = 80.0D0
  cconshpf = 70.0D0
  cconshtf = 75.0D0
  cland    = 19.2D0
  cowner   = 0.15D0
  csi      = 16.0D0
  cturbb   = 380.0D0
  fcontng  = 0.195D0
  ucad     = 180.0D0
  ucaf     = 1.5D6
  ucahts   = 31.0D0
  ucap     = 17.0D0
  ucblbe   = 260.0D0
  ucblli   = 875.0D0
  ucblli2o = 600.0D0
  ucbllipb = 10.3D0
  ucblss   = 90.0D0
  ucblvd   = 200.0D0
  ucbpmp   = 2.925D5
  ucbus    = 0.123D0
  uccase   = 50.0D0
  ucco     = 350.0D0
  uccpcl1  = 250.0D0
  uccpclb  = 150.0D0
  uccpmp   = 3.9D5
  uccr     = 460.0D0
  uccry    = 9.3D4
  uccryo   = 32.0D0
  uccu     = 75.0D0
  ucdgen   = 1.7D6
  ucdiv    = 2.8D5
  ucdtc    = 13.0D0
  ucduct   = 4.225D4
  ucech    = 3.0D0
  ucel     = 380.0D0
  uces1    = 3.2D4
  uces2    = 8.8D3
  ucf1     = 2.23D7
  ucfnc    = 35.0D0
  ucfpr    = 4.4D7
  ucfuel   = 3.45D0
  ucfwa    = 6.0D4
  ucfwps   = 1.0D7
  ucfws    = 5.3D4
  ucgss    = 35.0D0
  uche3    = 1.0D6
  uchrs    = 87.9D6
  uchts(1) = 15.3D0
  uchts(2) = 19.1D0
  uciac    = 1.5D8
  ucich    = 3.0D0
  ucihx    = 0.0D0
  ucint    = 35.0D0
  uclh     = 3.3D0
  uclv     = 16.0D0
  ucmb     = 260.0D0
  ucme     = 1.25D8
  ucmisc   = 2.5D7
  ucnbi    = 3.3D0
  ucnbv    = 1000.0D0
  ucoam(1) = 68.8D0
  ucoam(2) = 68.8D0
  ucoam(3) = 68.8D0
  ucoam(4) = 74.4D0
  ucof     = 3.3D0
  ucpens   = 32.0D0
  ucpfb    = 210.0D0
  ucpfbk   = 1.66D4
  ucpfbs   = 4.9D3
  ucpfcb   = 7.5D4
  ucpfdr1  = 150.0D0
  ucpfic   = 1.0D4
  ucpfps   = 3.5D4
  ucphx    = 15.0D0
  ucpp     = 48.0D0
  ucrb     = 400.0D0
  ucsc(1)  = 600.0D0
  ucsc(2)  = 600.0D0
  ucsc(3)  = 300.0D0
  ucsc(4)  = 600.0D0
  ucsc(5)  = 600.0D0
  ucsh     = 115.0D0
  ucshld   = 32.0D0
  ucswyd   = 1.84D7
  uctfbr   = 1.22D0
  uctfbus  = 100.0D0
  uctfdr   = 1.75D-4
  uctfgr   = 5000.0D0
  uctfic   = 1.0D4
  uctfps   = 24.0D0
  uctfsw   = 1.0D0
  uctpmp   = 1.105D5
  uctr     = 370.0D0
  ucturb(1) = 230.0D6
  ucturb(2) = 245.0D6
  ucvalv   = 3.9D5
  ucvdsh   = 26.0D0
  ucviac   = 1.3D6
  ucwindpf = 465.0D0
  ucwindtf = 480.0D0
  ucws     = 460.0D0
  ucwst(1) = 0.0D0
  ucwst(2) = 3.94D0
  ucwst(3) = 5.91D0
  ucwst(4) = 7.88D0

  !  Building volumes

  admvol   = 0.0D0
  convol   = 0.0D0
  cryvol   = 0.0D0
  efloor   = 0.0D0
  elevol   = 0.0D0
  esbldgm3 = 1.0D3
  pfbldgm3 = 2.0D4
  rbvol    = 0.0D0
  rmbvol   = 0.0D0
  shovol   = 0.0D0
  tfcbv    = 2.0D4
  triv     = 4.0D4
  volnucb  = 0.0D0
  volrci   = 0.0D0
  wrbi     = 0.0D0
  wsvol    = 0.0D0

  !  Energy storage

  iscenr   = 2

  !  Buildings

  admv     = 1.0D5
  clh1     = 8.0D0
  clh2     = 15.0D0
  conv     = 6.0D4
  fndt     = 2.0D0
  hccl     = 5.0D0
  hcwt     = 1.5D0
  pibv     = 2.0D4
  rbrt     = 1.0D0
  rbwt     = 2.0D0
  row      = 4.0D0
  rxcl     = 4.0D0
  shmf     = 0.5D0
  shov     = 1.0D5
  stcl     = 3.0D0
  trcl     = 1.0D0
  wgt      = 5.0D5
  wgt2     = 1.0D5

  !  Structural masses

  aintmass = 0.0D0
  clgsmass = 0.0D0
  coldmass = 0.0D0
  fncmass  = 0.0D0
  gsmass   = 0.0D0

  !  Vacuum parameters

  ntype    = 1
  pbase    = 2.6D-6
  prdiv    = 0.36D0
  rat      = 1.3D-8
  tn       = 300.0D0

  !  Vacuum systems

  dlscal   = 0.0D0
  nvduct   = 0
  nvtype   = ntype
  vacdshm  = 0.0D0
  vcdimax  = 0.0D0
  vpumpn   = 0.0D0

!  !  Output file parameters
!
!  sect01   = 1
!  sect02   = 1
!  sect03   = 1
!  sect04   = 1
!  sect05   = 1
!  sect06   = 1
!  sect07   = 1
!  sect08   = 1
!  sect09   = 1
!  sect10   = 1
!  sect11   = 1
!  sect12   = 1
!  sect13   = 1
!  sect14   = 1
!  sect15   = 1
!  sect16   = 1
!  sect17   = 1
!  sect18   = 1
!  sect19   = 1
!  sect20   = 1
!  sect21   = 1

  !  Reversed field pinch parameters

  irfp     = 0
  pfmmax   = 0.0D0
  pfrmax   = 0.0D0
  rfpf     = 0.0D0
  rfpth    = 1.5D0
  tftort   = 0.33D0
  rrpf(:)   = 0.0D0
  zzpf(:)   = 0.0D0
  drpf(:)   = 0.0D0
  dzpf(:)   = 0.0D0
  nturns(:) = 0.0D0
  cptrfp(:) = 0.0D0
  resrfp(:) = 0.0D0

  !  See which type of device is being modelled

  call devtyp

  !  Initialise stellarator parameters if necessary

  if (istell == 1) call stinit

  !  Initialise inertial fusion energy parameters if necessary

  if (ife == 1) call ifeini

end subroutine initial

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine devtyp

  !+ad_name  devtyp
  !+ad_summ  Routine to determine which type of device is to be modelled
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine uses the contents of an input file,
  !+ad_desc  <CODE>device.dat</CODE>, to determine which type of device
  !+ad_desc  is to be modelled. If the file is not present in the current
  !+ad_desc  directory, a standard tokamak model is assumed.
  !+ad_prob  None
  !+ad_call  stella.h90
  !+ad_call  rfp.h90
  !+ad_call  ife.h90
  !+ad_hist  27/02/96 PJK Initial version
  !+ad_hist  08/10/96 PJK Fixed error: (istell.gt.2) should be (idev.gt.2)
  !+ad_hist  14/03/97 PJK idev=3 ==> inertial fusion power plant
  !+ad_hist  19/09/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'stella.h90'
  include 'rfp.h90'
  include 'ife.h90'

  !  Local variables

  integer :: idev
  logical :: iexist

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  istell = 0
  irfp   = 0
  ife    = 0

  !  Read a second input file. If the file does not exist, then the
  !  standard tokamak option is assumed.

  inquire(file='device.dat',exist=iexist)

  if (iexist) then

     open(unit=1,file='device.dat',status='old')
     read(1,*) idev
     close(unit=1)

     !  Set relevant switch

     select case (idev)

     case (1)  !  Stellarator model
        istell = 1

     case (2)  !  Reversed Field Pinch model
        irfp = 1

     case (3)  !  Inertial Fusion Energy  model
        ife = 1

     case default  !  Tokamak model
        continue

     end select

  end if

end subroutine devtyp

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine check

  !+ad_name  check
  !+ad_summ  Routine to reset specific variables if certain options are
  !+ad_summ  being used
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine performs a sanity check of the input variables
  !+ad_desc  and ensures other dependent variables are given suitable values.
  !+ad_prob  None
  !+ad_call  physics_variables
  !+ad_call  global_variables
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  bldgvol.h90
  !+ad_call  build.h90
  !+ad_call  cdriv.h90
  !+ad_call  htpwr.h90
  !+ad_call  pfcoil.h90
  !+ad_call  pulse.h90
  !+ad_call  rfp.h90
  !+ad_call  tfcoil.h90
  !+ad_call  ife.h90
  !+ad_hist  08/10/96 PJK Initial upgraded version
  !+ad_hist  23/01/97 PJK Moved resetting of trithtmw from POWER
  !+ad_hist  14/03/97 PJK Added coding relevant to IFE device
  !+ad_hist  01/04/98 PJK Added rnbeam reset for no NBI
  !+ad_hist  19/01/99 PJK Added warning about IITER flag with non-ITER profiles
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added global_variables module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use global_variables
  use physics_variables
  use process_output

  implicit none

  include 'bldgvol.h90'
  include 'build.h90'
  include 'cdriv.h90'
  include 'htpwr.h90'
  include 'pfcoil.h90'
  include 'pulse.h90'
  include 'rfp.h90'
  include 'tfcoil.h90'
  include 'ife.h90'

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Pulsed power plant model

  if (lpulse == 1) icase = 'PROCESS pulsed tokamak model'

  !  D-He3 option

  if (idhe3 == 1) then
     icase = 'PROCESS D-He3 tokamak model'
     ftr = max(ftrit,1.0D-6)
     triv = 0.0D0
     ifispact = 0
     trithtmw = 0.0D0
  end if

  !  Tight aspect ratio options 

  if (itart == 1) then

     icase  = 'PROCESS tight aspect ratio tokamak model'

     bore   = 0.0D0
     gapoh  = 0.0D0
     ohcth  = 0.0D0
     ddwi   = 0.0D0

     if (icurr /= 2) then
        write(iotty,*) 'Warning in routine CHECK:'
        write(iotty,*) 'Normal current scaling for TARTs, ICURR=2,'
        write(iotty,*) 'is not being used.'
        write(iotty,*) 'PROCESS continuing...'
        write(iotty,*) ' '
     end if
     iohcl  = 0
     ipfloc(1) = 2
     ipfloc(2) = 3
     ipfloc(3) = 3
     itfsup = 0
     if (ibss == 1) then
        write(nout,*) 'ibss=1 is not a valid option for a TART device'
        write(nout,*) 'PROCESS stopping.'
        stop
     end if

  else

     ishape = 0
     if (icurr == 2) then
        write(nout,*) &
             'icurr=2 is not a valid option for a non-TART device'
        write(nout,*) 'PROCESS stopping.'
        stop
     end if

  end if

  !  Reversed Field Pinch model

  if (irfp == 1) then

     if (itart == 1) then
        write(nout,*) 'itart=1 is not a valid option for the RFP model'
        write(nout,*) 'PROCESS stopping.'
        stop
     end if

     bcylth   = 0.0D0
     ddwi     = 0.0D0
     kappa    = 1.0D0
     icase    = 'PROCESS reversed field pinch model'
     iefrf    = 9
     ifispact = 0
     iohcl    = 0
     ipfres   = 1
     isumatpf = 3
     itfmod   = 0
     itfsup   = 0
     ohcth    = 0.0D0
     pfclres  = 1.7D-8
     tfootfi  = 1.0D0
     triang   = 0.0D0
     vf(:) = 0.1D0
  end if

  !  Inertial Fusion Energy model

  if (ife == 1) then
     icase    = 'PROCESS inertial fusion energy model'
     lpulse   = 0
     idhe3    = 0
  end if

  !  Ensure that if TF coils are non-superconducting,
  !  only simple stress calculations are performed

  if (itfsup == 0) itfmod = 0

  !  PF coil resistivity is zero if superconducting

  if (ipfres == 0) pfclres = 0.0D0

  !  If there is no NBI, then hot beam density should be zero

  if (irfcd == 1) then
     if ((iefrf /= 5).and.(iefrf /= 8)) rnbeam = 0.0D0
  else
     rnbeam = 0.0D0
  end if

  !  Check on use of iiter

  if ( (iiter /= 0).and. &
       ((alphan /= 0.5D0).or.(alphat /= 1.0D0)) ) then
     write(iotty,*) 'Warning: IITER = 1 should only be used if'
     write(iotty,*) '         ALPHAN = 0.5 and ALPHAT = 1.0'
     write(iotty,*) 'PROCESS continuing...'
     write(iotty,*) ' '
  end if

end subroutine check
