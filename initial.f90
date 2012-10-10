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
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  ineq.h90
  !+ad_call  phydat.h90
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use numerics
  use process_output

  implicit none

  include 'ineq.h90'
  include 'phydat.h90'
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

  !  Numerics quantities and labels

  boundl(1)  = 1.100D0
  boundl(2)  = 0.010D0
  boundl(3)  = 0.100D0
  boundl(4)  = 5.000D0
  boundl(5)  = 0.001D0
  boundl(6)  = 1.00D19
  boundl(7)  = 1.00D-6
  boundl(8)  = 0.001D0
  boundl(9)  = 0.001D0
  boundl(10) = 0.100D0
  boundl(11) = 1.000D6
  boundl(12) = 1.000D5
  boundl(13) = 0.100D0
  boundl(14) = 0.001D0
  boundl(15) = 0.001D0
  boundl(16) = 0.001D0
  boundl(17) = 0.100D0
  boundl(18) = 2.000D0
  boundl(19) = 1.000D0
  boundl(20) = 40.00D0
  boundl(21) = 0.001D0
  boundl(22) = 0.001D0
  boundl(23) = 0.100D0
  boundl(24) = 1.000D4
  boundl(25) = 1.000D0
  boundl(26) = 0.001D0
  boundl(27) = 0.001D0
  boundl(28) = 0.100D0
  boundl(29) = 0.100D0
  boundl(30) = 0.010D0
  boundl(31) = 0.001D0
  boundl(32) = 0.001D0
  boundl(33) = 0.010D0
  boundl(34) = 0.001D0
  boundl(35) = 0.001D0
  boundl(36) = 0.001D0
  boundl(37) = 1.000D5
  boundl(38) = 0.010D0
  boundl(39) = 0.001D0
  boundl(40) = 0.001D0
  boundl(41) = 0.001D0
  !+PJK 20/08/2012 should boundl(42)=0.000 ???
  boundl(42) = 0.000D0
  boundl(43) = 1.00D-6
  boundl(44) = 0.001D0
  boundl(45) = 0.010D0
  boundl(46) = 0.001D0
  boundl(47) = 0.001D0
  boundl(48) = 0.001D0
  boundl(49) = 0.001D0
  boundl(50) = 0.001D0
  boundl(51) = 0.001D0
  boundl(52) = 0.001D0
  boundl(53) = 0.001D0
  boundl(54) = 0.001D0
  boundl(55) = 0.001D0
  boundl(56) = 10.00D0
  boundl(57) = 0.050D0
  boundl(58) = 0.001D0
  boundl(59) = 0.001D0
  boundl(60) = 0.001D0
  boundl(61) = 0.001D0
  boundl(62) = 0.001D0
  boundl(63) = 0.001D0
  boundl(64) = 0.001D0
  boundl(65) = 0.100D0
  boundl(66) = 0.001D0
  boundl(67) = 0.001D0
  boundl(68) = 0.001D0
  boundl(69) = 0.001D0
  boundl(70) = 1.000D0
  boundl(71) = 0.001D0
  boundl(72) = 0.001D0
  boundl(73) = 0.001D0
  boundl(74) = 0.001D0
  boundl(75) = 0.200D0
  boundl(76) = 0.001D0
  boundl(77) = 0.050D0
  boundl(78) = 0.010D0
  boundl(79) = 0.001D0
  boundl(80) = 0.001D0
  boundl(81) = 1.000D5
  boundl(82) = 0.010D0
  boundl(83) = 1.000D0
  boundl(84) = 0.100D0
  boundl(85) = 1.000D6
  boundl(86) = 0.001D0
  boundl(87) = 1.000D0
  boundl(88) = 1.000D0

  boundu(1)  = 10.00D0
  boundu(2)  = 100.0D0
  boundu(3)  = 10.00D0
  boundu(4)  = 500.0D0
  boundu(5)  = 1.000D0
  boundu(6)  = 1.00D21
  boundu(7)  = 1.00D20
  boundu(8)  = 1.000D0
  boundu(9)  = 1.000D0
  boundu(10) = 3.000D0
  boundu(11) = 1.000D9
  boundu(12) = 1.500D8
  boundu(13) = 5.000D0
  boundu(14) = 1.000D0
  boundu(15) = 1.000D0
  boundu(16) = 1.000D2
  boundu(17) = 1.000D8
  boundu(18) = 100.0D0
  boundu(19) = 1.000D6
  boundu(20) = 1.000D3
  boundu(21) = 1.000D0
  boundu(22) = 1.000D6
  boundu(23) = 0.500D0
  boundu(24) = 1.000D8
  boundu(25) = 1.000D0
  boundu(26) = 1.000D0
  boundu(27) = 1.000D0
  boundu(28) = 1.000D0
  boundu(29) = 10.00D0
  boundu(30) = 1.000D0
  boundu(31) = 1.000D1
  boundu(32) = 1.000D0
  boundu(33) = 1.000D0
  boundu(34) = 1.000D0
  boundu(35) = 1.000D0
  boundu(36) = 1.000D0
  boundu(37) = 1.000D8
  boundu(38) = 1.000D0
  boundu(39) = 1.000D0
  boundu(40) = 1.000D0
  boundu(41) = 1.000D0
  boundu(42) = 10.00D0
  boundu(43) = 3.00D-3
  boundu(44) = 1.000D0
  boundu(45) = 0.330D0
  boundu(46) = 1.000D0
  boundu(47) = 1.000D0
  boundu(48) = 1.000D0
  boundu(49) = 1.000D0
  boundu(50) = 0.500D0
  boundu(51) = 1.000D0
  boundu(52) = 1.000D6
  boundu(53) = 1.000D0
  boundu(54) = 1.000D0
  boundu(55) = 100.0D0
  boundu(56) = 1.000D6
  boundu(57) = 1.000D0
  boundu(58) = 1.000D0
  boundu(59) = 1.000D0
  boundu(60) = 4.000D4
  boundu(61) = 10.00D0
  boundu(62) = 1.000D0
  boundu(63) = 1.000D0
  boundu(64) = 1.000D0
  boundu(65) = 1.000D3
  boundu(66) = 1.000D0
  boundu(67) = 1.000D0
  boundu(68) = 1.000D0
  boundu(69) = 0.010D0
  boundu(70) = 1.000D2
  boundu(71) = 1.000D0
  boundu(72) = 1.000D0
  boundu(73) = 10.00D0
  boundu(74) = 10.00D0
  boundu(75) = 5.000D0
  boundu(76) = 1.000D0
  boundu(77) = 2.000D0
  boundu(78) = 1.800D0
  boundu(79) = 1.000D0
  boundu(80) = 1.000D0
  boundu(81) = 5.000D7
  boundu(82) = 1.000D0
  boundu(83) = 500.0D0
  boundu(84) = 20.00D0
  boundu(85) = 200.0D6
  boundu(86) = 1.000D0
  boundu(87) = 4.000D3
  boundu(88) = 4.000D3

  lablxc(1)  = 'aspect  '
  lablxc(2)  = 'bt      '
  lablxc(3)  = 'rmajor  '
  lablxc(4)  = 'te      '
  lablxc(5)  = 'beta    '
  lablxc(6)  = 'dene    '
  lablxc(7)  = 'rnbeam  '
  lablxc(8)  = 'fbeta   '
  lablxc(9)  = 'fdene   '
  lablxc(10) = 'hfact   '
  lablxc(11) = 'pheat   '
  lablxc(12) = 'oacdcp  '
  lablxc(13) = 'tfcth   '
  lablxc(14) = 'fwalld  '
  lablxc(15) = 'fvs     '
  lablxc(16) = 'ohcth   '
  lablxc(17) = 'tdwell  '
  lablxc(18) = 'q       '
  lablxc(19) = 'enbeam  '
  lablxc(20) = 'tcpav   '
  lablxc(21) = 'ftburn  '
  lablxc(22) = 'tbrnmn  '
  lablxc(23) = 'fcoolcp '
  lablxc(24) = 'cdtfleg '
  lablxc(25) = 'fpnetel '
  lablxc(26) = 'ffuspow '
  lablxc(27) = 'fhldiv  '
  lablxc(28) = 'fjtfc   '
  lablxc(29) = 'bore    '
  lablxc(30) = 'fmva    '
  lablxc(31) = 'gapomin '
  lablxc(32) = 'frminor '
  lablxc(33) = 'fportsz '
  lablxc(34) = 'fdivcol '
  lablxc(35) = 'fpeakb  '
  lablxc(36) = 'fbetatry'
  lablxc(37) = 'coheof  '
  lablxc(38) = 'fjohc   '
  lablxc(39) = 'fjohc0  '
  lablxc(40) = 'fgamcd  '
  lablxc(41) = 'fcohbop '
  lablxc(42) = 'gapoh   '
  lablxc(43) = 'cfe0    '
  lablxc(44) = 'fvsbrnni'
  lablxc(45) = 'fqval   '
  lablxc(46) = 'fpinj   '
  lablxc(47) = 'feffcd  '
  lablxc(48) = 'fstrcase'
  lablxc(49) = 'fstrcond'
  lablxc(50) = 'fiooic  '
  lablxc(51) = 'fvdump  '
  lablxc(52) = 'vdalw   '
  lablxc(53) = 'fjprot  '
  lablxc(54) = 'ftmargtf'
  lablxc(55) = 'tmargmin'
  lablxc(56) = 'tdmptf  '
  lablxc(57) = 'thkcas  '
  lablxc(58) = 'thwcndut'
  lablxc(59) = 'fcutfsu '
  lablxc(60) = 'cpttf   '
  lablxc(61) = 'gapds   '
  lablxc(62) = 'fdtmp   '
  lablxc(63) = 'ftpeak  '
  lablxc(64) = 'fauxmn  '
  lablxc(65) = 'tohs    '
  lablxc(66) = 'ftohs   '
  lablxc(67) = 'ftcycl  '
  lablxc(68) = 'fptemp  '
  lablxc(69) = 'rcool   '
  lablxc(70) = 'vcool   '
  lablxc(71) = 'fq      '
  lablxc(72) = 'fipir   '
  lablxc(73) = 'scrapli '
  lablxc(74) = 'scraplo '
  lablxc(75) = 'tfootfi '
  lablxc(76) = 'frfptf  '
  lablxc(77) = 'tftort  '
  lablxc(78) = 'rfpth   '
  lablxc(79) = 'fbetap  '
  lablxc(80) = 'frfpf   '
  lablxc(81) = 'edrive  '
  lablxc(82) = 'drveff  '
  lablxc(83) = 'tgain   '
  lablxc(84) = 'chrad   '
  lablxc(85) = 'pdrive  '
  lablxc(86) = 'frrmax  '
  lablxc(87) = 'helecmw '
  lablxc(88) = 'hthermmw'

  lablcc(1)  = 'Beta consistency                 '
  lablcc(2)  = 'Global power balance consistency '
  lablcc(3)  = 'Ion power balance                '
  lablcc(4)  = 'Electron power balance           '
  lablcc(5)  = 'Density limit                    '
  lablcc(6)  = 'Epsilon * beta poloidal limit    '
  lablcc(7)  = 'Beam ion density consistency     '
  lablcc(8)  = 'Neutron wall load limit          '
  lablcc(9)  = 'Fusion power limit               '
  lablcc(10) = 'Toroidal field 1/R consistency   '
  lablcc(11) = 'Radial build consistency         '
  lablcc(12) = 'Volt second limit                '
  lablcc(13) = 'Burn time limit                  '
  lablcc(14) = 'Energy of neutral beam           '
  lablcc(15) = 'Burn time consistency            '
  lablcc(16) = 'Net electric power limit         '
  lablcc(17) = 'Stellarator build consistency    '
  lablcc(18) = 'Divertor heat load limit         '
  lablcc(19) = 'MVA limit                        '
  lablcc(20) = 'Port size constraint             '
  lablcc(21) = 'Plasma minor radius limit        '
  lablcc(22) = 'Divertor collisionality limit    '
  lablcc(23) = 'TF coil current density limit    '
  lablcc(24) = 'Troyon beta limit                '
  lablcc(25) = 'Peak toroidal field limit        '
  lablcc(26) = 'OH coil EOF current density limit'
  lablcc(27) = 'OH coil BOP current density limit'
  lablcc(28) = 'Energy multiplication Q limit    '
  lablcc(29) = 'Inboard radial build constraint  '
  lablcc(30) = 'Allowable injection power        '
  lablcc(31) = 'TF coil case stress limit        '
  lablcc(32) = 'TF coil conduit stress limit     '
  lablcc(33) = 'I_op / I_critical (TF coil)      '
  lablcc(34) = 'Dump voltage limit               '
  lablcc(35) = 'J_winding pack/J_protection limit'
  lablcc(36) = 'TF coil temperature margin limit '
  lablcc(37) = 'Current drive gamma limit        '
  lablcc(38) = '1st wall coolant temp rise limit '
  lablcc(39) = 'First wall peak temperature limit'
  lablcc(40) = 'Start-up injection power limit   '
  lablcc(41) = 'OH coil swing time limit         '
  lablcc(42) = 'Cycle time limit                 '
  lablcc(43) = 'Average centrepost temperature   '
  lablcc(44) = 'Peak centrepost temperature limit'
  lablcc(45) = 'Edge safety factor limit         '
  lablcc(46) = 'Ip/Irod limit                    '
  lablcc(47) = 'TF coil toroidal thickness limit '
  lablcc(48) = 'Poloidal beta limit              '
  lablcc(49) = 'RFP reversal parameter < 0       '
  lablcc(50) = 'IFE repetition rate limit        '

  lablmm(1)  = 'major radius.         ' 
  lablmm(2)  = 'P_fus P_in-total.     ' 
  lablmm(3)  = 'neutron wall load.    ' 
  lablmm(4)  = 'P_tf + P_pf.          ' 
  lablmm(5)  = 'energy multiplication.' 
  lablmm(6)  = 'cost of electricity.  ' 
  lablmm(7)  = 'capital cost.         ' 
  lablmm(8)  = 'aspect ratio.         ' 
  lablmm(9)  = 'divertor heat load.   '
  lablmm(10) = 'toroidal field.       '
  lablmm(11) = 'total injected power. '
  lablmm(12) = 'H plant capital cost. '
  lablmm(13) = 'H production rate.    '

  icc(1)   = 2
  icc(2)   = 10
  icc(3)   = 11
  icc(4)   = 24
  icc(5)   = 31
  icc(6)   = 32
  icc(7)   = 33
  icc(8)   = 34
  icc(9)   = 35
  icc(10)  = 36
  icc(11)  = 1
  icc(12)  = 7
  icc(13)  = 14
  icc(14)  = 16

  ixc(1)   = 10
  ixc(2)   = 12
  ixc(3)   = 3
  ixc(4)   = 36
  ixc(5)   = 48
  ixc(6)   = 49
  ixc(7)   = 50
  ixc(8)   = 51
  ixc(9)   = 53
  ixc(10)  = 54
  ixc(11)  = 5
  ixc(12)  = 7
  ixc(13)  = 19
  ixc(14)  = 1
  ixc(15)  = 2
  ixc(16)  = 6
  ixc(17)  = 13
  ixc(18)  = 16
  ixc(19)  = 29
  ixc(20)  = 56
  ixc(21)  = 57
  ixc(22)  = 58
  ixc(23)  = 59
  ixc(24)  = 60
  ixc(25)  = 4

  !  Physics data

  abeam    = 2.0D0
  afuel    = 0.0D0
  aion     = 0.0D0
  alphaj   = 1.0D0
  alphan   = 0.5D0
  alphat   = 1.0D0
  alpmw    = 0.0D0
  aspect   = 3.5D0
  beamfus0 = 1.0D0
  beta     = 0.042D0
  betaft   = 0.0D0
  betalim  = 0.0D0
  betanb   = 0.0D0
  betap    = 0.0D0
  betbm0   = 1.5D0
  bp       = 0.0D0
  bt       = 6.0D0
  btot     = 0.0D0
  burnup   = 0.0D0
  capa     = 0.0D0
  carea    = 1.0D0
  cfe0     = 0.0D0
  csawth   = 1.0D0
  cvol     = 1.0D0
  deltdn   = 0.0D0
  deltup   = 0.0D0
  dene     = 1.5D20
  deni     = 0.0D0
  dign     = 1.0D0
  dlamee   = 0.0D0
  dlamie   = 0.0D0
  dlimit(:) = 0.0D0
  dnalp    = 0.0D0
  dnbeam   = 0.0D0
  dnbeam2  = 0.0D0
  dnbeta   = 3.5D0
  dnelimt  = 0.0D0
  dnitot   = 0.0D0
  dnla     = 0.0D0
  dnprot   = 0.0D0
  dntau    = 0.0D0
  dnz      = 0.0D0
  ealpha   = 3520.0D0
  epbetmax = 0.6D0
  eps      = 1.0D0/aspect
  faccd    = 0.0D0
  facoh    = 0.0D0
  falpe    = 0.0D0
  falpha   = 1.0D0
  falpi    = 0.0D0
  faux     = 0.0D0
  fbfe     = 0.35D0
  fdeut    = 0.5D0
  ffwal    = 0.92D0
  fhe3     = 0.0D0
  figmer   = 0.0D0
  fradmin  = 0.0D0
  ftr      = 0.5D0
  ftrit    = 0.5D0
  fvsbrnni = 1.0D0
  gamma    = 0.4D0
  gtscale  = 0
  hfac(:) = 0.0D0
  hfact    = 2.0D0
  ibss     = 1
  iculbl   = 0
  iculdl   = 0
  icurr    = 4
  idensl   = 3
  idhe3    = 0
  idivrt   = 2
  ifalphap = 0
  ifispact = 1
  igeom    = 0
  iinvqd   = 1
  iiter    = 1
  impc     = 1.0D0
  impfe    = 1.0D0
  impo     = 1.0D0
  ires     = 1
  isc      = 6
  iscrp    = 1
  ishape   = 0
  itart    = 0
  ignite   = 0
  iwalld   = 1
  kappa    = 2.218D0
  kappa95  = 0.0D0
  palp     = 0.0D0
  palpe    = 0.0D0
  palpi    = 0.0D0
  palpnb   = 0.0D0
  pbrem    = 0.0D0
  pcharge  = 0.0D0
  pcoef    = 0.0D0
  pdivt    = 0.0D0
  pfuscmw  = 0.0D0
  phiint   = 0.0D0
  pi       = 3.141592653589793D0
  pie      = 0.0D0
  plascur  = 0.0D0
  plrad    = 0.0D0
  pneut    = 0.0D0
  pohmpv   = 0.0D0
  powerht  = 0.0D0
  powfmw   = 0.0D0
  prad     = 0.0D0
  psync    = 0.0D0
  pthrmw(:) = 0.0D0
  ptre     = 0.0D0
  ptri     = 0.0D0
  q        = 3.0D0
  q0       = 1.0D0
  q95      = 0.0D0
  qfuel    = 0.0D0
  qlim     = 0.0D0
  qstar    = 0.0D0
  ralpne   = 0.10D0
  recyle   = 0.7D0
  rli      = 0.65D0
  rlp      = 0.0D0
  rmajor   = 7.0D0
  rminor   = rmajor / aspect
  rmu0     = 1.256637D-6
  rnbeam   = 0.005D0
  rncne    = 0.0D0
  rndfuel  = 0.0D0
  rnfene   = 0.0D0
  rnone    = 0.0D0
  rpfac    = 0.0D0
  rplas    = 0.0D0
  rtpte    = 5.0D0
  sarea    = 0.0D0
  sareao   = 0.0D0
  sf       = 0.0D0
  ssync    = 0.8D0
  tauee    = 0.0D0
  taueff   = 0.0D0
  tauei    = 0.0D0
  te       = 15.0D0
  ten      = 0.0D0
  ti       = 8.33D0
  tin      = 0.0D0
  tratio   = 1.0D0
  triang   = 0.6D0
  triang95 = 0.0D0
  vol      = 0.0D0
  vsbrn    = 0.0D0
  vshift   = 0.0D0
  vsind    = 0.0D0
  vsres    = 0.0D0
  vsstt    = 0.0D0
  wallmw   = 0.0D0
  zeff     = 0.0D0
  zeffai   = 0.0D0

  tauscl(1)  = 'Neo-Alcator      (ohmic)'
  tauscl(2)  = 'Mirnov               (H)'
  tauscl(3)  = 'Merezkhin-Muhkovatov (L)'
  tauscl(4)  = 'Shimomura            (H)'
  tauscl(5)  = 'Kaye-Goldston        (L)'
  tauscl(6)  = 'ITER 89-P            (L)'
  tauscl(7)  = 'ITER 89-O            (L)'
  tauscl(8)  = 'Rebut-Lallia         (L)'
  tauscl(9)  = 'Goldston             (L)'
  tauscl(10) = 'T10                     '
  tauscl(11) = 'JAERI-88                '
  tauscl(12) = 'Kaye-Big Complex        '
  tauscl(13) = 'ITER H90-P           (H)'
  tauscl(14) = 'ITER Mix                '
  tauscl(15) = 'Riedel               (L)'
  tauscl(16) = 'Christiansen         (L)'
  tauscl(17) = 'Lackner-Gottardi     (L)'
  tauscl(18) = 'Neo-Kaye             (L)'
  tauscl(19) = 'Riedel               (H)'
  tauscl(20) = 'ITER H90-P amended   (H)'
  tauscl(21) = 'LHD              (stell)'
  tauscl(22) = 'Gyro-reduced Bohm(stell)'
  tauscl(23) = 'Lackner-Gottardi (stell)'
  tauscl(24) = 'ITER-93H             (H)'
  tauscl(25) = 'TITAN RFP               '
  tauscl(26) = 'ITER H-97P ELM-free  (H)'
  tauscl(27) = 'ITER H-97P ELMy      (H)'
  tauscl(28) = 'ITER-96P             (L)'
  tauscl(29) = 'Valovic modified ELMy(H)'
  tauscl(30) = 'Kaye PPPL April 98   (L)'
  tauscl(31) = 'ITERH-PB98P(y)       (H)'
  tauscl(32) = 'IPB98(y)             (H)'
  tauscl(33) = 'IPB98(y,1)           (H)'
  tauscl(34) = 'IPB98(y,2)           (H)'
  tauscl(35) = 'IPB98(y,3)           (H)'
  tauscl(36) = 'IPB98(y,4)           (H)'

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
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  bldgvol.h90
  !+ad_call  build.h90
  !+ad_call  cdriv.h90
  !+ad_call  htpwr.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use numerics  !  icase only
  use process_output

  implicit none

  include 'bldgvol.h90'
  include 'build.h90'
  include 'cdriv.h90'
  include 'htpwr.h90'
  include 'pfcoil.h90'
  include 'phydat.h90'
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
