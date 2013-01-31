!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module numerics

  !+ad_name  numerics
  !+ad_summ  Module containing callers to the main equation solvers
  !+ad_summ  HYBRD and VMCON
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  eqsolv
  !+ad_cont  optimiz
  !+ad_args  N/A
  !+ad_desc  This module contains the primary numerics variables and the
  !+ad_desc  calling routines for the two equation solvers in the code.
  !+ad_prob  None
  !+ad_call  maths_library
  !+ad_hist  10/10/12 PJK Initial version of module
  !+ad_hist  15/10/12 PJK Modified comment lines, and added default array values
  !+ad_hist  06/11/12 PJK Modified comment lines
  !+ad_hist  11/12/12 PJK Comment typo fixed
  !+ad_hist  17/12/12 PJK New constraint equation 51 added
  !+ad_hist  17/12/12 PJK New figure of merit 14 added
  !+ad_hist  13/01/13 PJK Modified lablcc comment for eqn.47
  !+ad_hist  31/01/13 PJK Changed FACTOR comment
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use maths_library

  implicit none

  public

  !+ad_vars  ipnvars /88/ FIX : total number of variables available for iteration
  integer, parameter :: ipnvars = 88
  !+ad_vars  ipeqns /51/ FIX : number of constraint equations available
  integer, parameter :: ipeqns  = 51
  !+ad_vars  ipnfoms /14/ FIX : number of available figures of merit
  integer, parameter :: ipnfoms = 14

  integer, parameter :: ipvlam  = ipeqns+2*ipnvars+1
  integer, parameter :: iptnt   = (ipeqns*(3*ipeqns+13))/2
  integer, parameter :: ipvp1   = ipnvars+1

  !+ad_vars  ioptimz /1/ : code operation switch:<UL>
  !+ad_varc           <LI> < 0 for no optimisation, HYBRD only;
  !+ad_varc           <LI> = 0 for HYBRD and VMCON;
  !+ad_varc           <LI> > 0 for optimisation, VMCON only</UL>
  integer :: ioptimz = 1
  !+ad_vars  maxcal /200/ : maximum number of VMCON iterations
  integer :: maxcal  = 200
  !+ad_vars  minmax /6/ : switch for figure-of-merit (see lablmm for descriptions)
  !+ad_varc               negative => maximise, positive => minimise
  integer :: minmax  = 6
  !+ad_vars  ncalls : number of function calls during solution
  integer :: ncalls  = 0
  !+ad_vars  neqns /14/ : number of equations root solver must satisfy
  integer :: neqns   = 14
  integer :: nfev1   = 0
  integer :: nfev2   = 0
  !+ad_vars  nineqns : number of inequalities VMCON must satisfy (keep at zero)
  integer :: nineqns = 0
  !+ad_vars  nvar /25/ : number of iteration (independent) variables
  integer :: nvar    = 25
  integer :: nvrbl   = 0

  !+ad_vars  icc(ipeqns) : array defining which constraint equations to activate
  !+ad_varc                (see lablcc for descriptions)
  integer, dimension(ipeqns) :: icc = (/ &
       2,  &  !  1
       10, &  !  2
       11, &  !  3
       24, &  !  4
       31, &  !  5
       32, &  !  6
       33, &  !  7
       34, &  !  8
       35, &  !  9
       36, &  !  10
       1,  &  !  11
       7,  &  !  12
       14, &  !  13
       16, &  !  14
       0,  &  !  15
       0,  &  !  16
       0,  &  !  17
       0,  &  !  18
       0,  &  !  19
       0,  &  !  20
       0,  &  !  21
       0,  &  !  22
       0,  &  !  23
       0,  &  !  24
       0,  &  !  25
       0,  &  !  26
       0,  &  !  27
       0,  &  !  28
       0,  &  !  29
       0,  &  !  30
       0,  &  !  31
       0,  &  !  32
       0,  &  !  33
       0,  &  !  34
       0,  &  !  35
       0,  &  !  36
       0,  &  !  37
       0,  &  !  38
       0,  &  !  39
       0,  &  !  40
       0,  &  !  41
       0,  &  !  42
       0,  &  !  43
       0,  &  !  44
       0,  &  !  45
       0,  &  !  46
       0,  &  !  47
       0,  &  !  48
       0,  &  !  49
       0,  &  !  50
       0  /)  !  51

  !+ad_vars  ixc(ipnvars) : array defining which iteration variables to
  !+ad_varc                 activate (see lablxc for descriptions)
  integer, dimension(ipnvars) :: ixc = (/ &
       10, &  !  1
       12, &  !  2
       3,  &  !  3
       36, &  !  4
       48, &  !  5
       49, &  !  6
       50, &  !  7
       51, &  !  8
       53, &  !  9
       54, &  !  10
       5,  &  !  11
       7,  &  !  12
       19, &  !  13
       1,  &  !  14
       2,  &  !  15
       6,  &  !  16
       13, &  !  17
       16, &  !  18
       29, &  !  19
       56, &  !  20
       57, &  !  21
       58, &  !  22
       59, &  !  23
       60, &  !  24
       4,  &  !  25
       0,  &  !  26
       0,  &  !  27
       0,  &  !  28
       0,  &  !  29
       0,  &  !  30
       0,  &  !  31
       0,  &  !  32
       0,  &  !  33
       0,  &  !  34
       0,  &  !  35
       0,  &  !  36
       0,  &  !  37
       0,  &  !  38
       0,  &  !  39
       0,  &  !  40
       0,  &  !  41
       0,  &  !  42
       0,  &  !  43
       0,  &  !  44
       0,  &  !  45
       0,  &  !  46
       0,  &  !  47
       0,  &  !  48
       0,  &  !  49
       0,  &  !  50
       0,  &  !  51
       0,  &  !  52
       0,  &  !  53
       0,  &  !  54
       0,  &  !  55
       0,  &  !  56
       0,  &  !  57
       0,  &  !  58
       0,  &  !  59
       0,  &  !  60
       0,  &  !  61
       0,  &  !  62
       0,  &  !  63
       0,  &  !  64
       0,  &  !  65
       0,  &  !  66
       0,  &  !  67
       0,  &  !  68
       0,  &  !  69
       0,  &  !  70
       0,  &  !  71
       0,  &  !  72
       0,  &  !  73
       0,  &  !  74
       0,  &  !  75
       0,  &  !  76
       0,  &  !  77
       0,  &  !  78
       0,  &  !  79
       0,  &  !  80
       0,  &  !  81
       0,  &  !  82
       0,  &  !  83
       0,  &  !  84
       0,  &  !  85
       0,  &  !  86
       0,  &  !  87
       0  /)  !  88

  !+ad_vars  lablcc(ipeqns) : labels describing constraint equations:<UL>
  character(len=34), dimension(ipeqns) :: lablcc = (/ &
       !+ad_varc  <LI> ( 1) Beta (consistency equation)
       'Beta consistency                 ', &
       !+ad_varc  <LI> ( 2) Global power balance (consistency equation)
       'Global power balance consistency ', &
       !+ad_varc  <LI> ( 3) Ion power balance
       'Ion power balance                ', &
       !+ad_varc  <LI> ( 4) Electron power balance
       'Electron power balance           ', &
       !+ad_varc  <LI> ( 5) Density limit
       'Density limit                    ', &
       !+ad_varc  <LI> ( 6) Epsilon * beta poloidal limit
       'Epsilon * beta poloidal limit    ', &
       !+ad_varc  <LI> ( 7) Beam ion density (NBI) (consistency equation)
       'Beam ion density consistency     ', &
       !+ad_varc  <LI> ( 8) Neutron wall load limit
       'Neutron wall load limit          ', &
       !+ad_varc  <LI> ( 9) Fusion power limit
       'Fusion power limit               ', &
       !+ad_varc  <LI> (10) Toroidal field 1/R (consistency equation)
       'Toroidal field 1/R consistency   ', &
       !+ad_varc  <LI> (11) Radial build (consistency equation)
       'Radial build consistency         ', &
       !+ad_varc  <LI> (12) Volt second limit
       'Volt second limit                ', &
       !+ad_varc  <LI> (13) Burn time limit (PULSE)
       'Burn time limit                  ', &
       !+ad_varc  <LI> (14) Energy of neutral beam (NBI)
       'Energy of neutral beam           ', &
       !+ad_varc  <LI> (15) Burn time (PULSE) (consistency equation)
       'Burn time consistency            ', &
       !+ad_varc  <LI> (16) Net electric power limit
       'Net electric power limit         ', &
       !+ad_varc  <LI> (17) Stellarator build (STELL) (consistency equation)
       'Stellarator build consistency    ', &
       !+ad_varc  <LI> (18) Divertor heat load limit
       'Divertor heat load limit         ', &
       !+ad_varc  <LI> (19) MVA limit
       'MVA limit                        ', &
       !+ad_varc  <LI> (20) Port size constraint
       'Port size constraint             ', &
       !+ad_varc  <LI> (21) Plasma minor radius limit
       'Plasma minor radius limit        ', &
       !+ad_varc  <LI> (22) Divertor collisionality limit
       'Divertor collisionality limit    ', &
       !+ad_varc  <LI> (23) TF coil current density limit
       'TF coil current density limit    ', &
       !+ad_varc  <LI> (24) Troyon beta limit
       'Troyon beta limit                ', &
       !+ad_varc  <LI> (25) Peak toroidal field limit
       'Peak toroidal field limit        ', &
       !+ad_varc  <LI> (26) OH coil EOF current density limit
       'OH coil EOF current density limit', &
       !+ad_varc  <LI> (27) OH coil BOP current density limit
       'OH coil BOP current density limit', &
       !+ad_varc  <LI> (28) Energy multiplication Q limit
       'Energy multiplication Q limit    ', &
       !+ad_varc  <LI> (29) Inboard radial build constraint
       'Inboard radial build constraint  ', &
       !+ad_varc  <LI> (30) Allowable injection power
       'Allowable injection power        ', &
       !+ad_varc  <LI> (31) TF coil case stress limit (SCTF)
       'TF coil case stress limit        ', &
       !+ad_varc  <LI> (32) TF coil conduit stress limit (SCTF)
       'TF coil conduit stress limit     ', &
       !+ad_varc  <LI> (33) I_op / I_critical (TF coil) (SCTF)
       'I_op / I_critical (TF coil)      ', &
       !+ad_varc  <LI> (34) Dump voltage limit (SCTF)
       'Dump voltage limit               ', &
       !+ad_varc  <LI> (35) J_winding pack/J_protection limit (SCTF)
       'J_winding pack/J_protection limit', &
       !+ad_varc  <LI> (36) TF coil temperature margin limit (SCTF)
       'TF coil temperature margin limit ', &
       !+ad_varc  <LI> (37) Current drive gamma limit
       'Current drive gamma limit        ', &
       !+ad_varc  <LI> (38) 1st wall coolant temp rise limit (PULSE)
       '1st wall coolant temp rise limit ', &
       !+ad_varc  <LI> (39) First wall peak temperature limit (PULSE)
       'First wall peak temperature limit', &
       !+ad_varc  <LI> (40) Start-up injection power limit (PULSE)
       'Start-up injection power limit   ', &
       !+ad_varc  <LI> (41) OH coil swing time limit (PULSE)
       'OH coil swing time limit         ', &
       !+ad_varc  <LI> (42) Cycle time limit (PULSE)
       'Cycle time limit                 ', &
       !+ad_varc  <LI> (43) Average centrepost temperature
       !+ad_varc            (TART) (consistency equation)
       'Average centrepost temperature   ', &
       !+ad_varc  <LI> (44) Peak centrepost temperature limit (TART)
       'Peak centrepost temperature limit', &
       !+ad_varc  <LI> (45) Edge safety factor limit (TART)
       'Edge safety factor limit         ', &
       !+ad_varc  <LI> (46) Ip/Irod limit (TART)
       'Ip/Irod limit                    ', &
       !+ad_varc  <LI> (47) TF coil toroidal thickness limit (RFP, STELL)
       'TF coil toroidal thickness limit ', &
       !+ad_varc  <LI> (48) Poloidal beta limit
       'Poloidal beta limit              ', &
       !+ad_varc  <LI> (49) RFP reversal parameter &lt; 0 (RFP)
       'RFP reversal parameter < 0       ', &
       !+ad_varc  <LI> (50) IFE repetition rate limit (IFE)
       'IFE repetition rate limit        ', &
       !+ad_varc  <LI> (51) Startup volt-seconds consistency (PULSE)</UL>
       'Startup volt-seconds consistency ' /)

  !+ad_vars  lablmm(ipnfoms) : labels describing figures of merit:<UL>
  character(len=22), dimension(ipnfoms) :: lablmm = (/ &
       !+ad_varc  <LI> ( 1) major radius
       'major radius.         ', &
       !+ad_varc  <LI> ( 2) P_fus P_in-total
       'P_fus P_in-total.     ', &
       !+ad_varc  <LI> ( 3) neutron wall load
       'neutron wall load.    ', &
       !+ad_varc  <LI> ( 4) P_tf + P_pf
       'P_tf + P_pf.          ', &
       !+ad_varc  <LI> ( 5) energy multiplication
       'energy multiplication.', &
       !+ad_varc  <LI> ( 6) cost of electricity
       'cost of electricity.  ', &
       !+ad_varc  <LI> ( 7) capital cost (direct cost if ireactor=0,
       !+ad_varc                          constructed cost otherwise)
       'capital cost.         ', &
       !+ad_varc  <LI> ( 8) aspect ratio
       'aspect ratio.         ', &
       !+ad_varc  <LI> ( 9) divertor heat load
       'divertor heat load.   ', &
       !+ad_varc  <LI> (10) toroidal field
       'toroidal field.       ', &
       !+ad_varc  <LI> (11) total injected power
       'total injected power. ', &
       !+ad_varc  <LI> (12) hydrogen plant capital cost
       'H plant capital cost. ', &
       !+ad_varc  <LI> (13) hydrogen production rate
       'H production rate.    ', &
       !+ad_varc  <LI> (14) pulse length</UL>
       'pulse length.         ' /)

  !+ad_vars  lablxc(ipnvars) : labels describing iteration variables:<UL>
  character(len=8), dimension(ipnvars) :: lablxc = (/ &
       !+ad_varc  <LI> ( 1) aspect
       'aspect  ', &
       !+ad_varc  <LI> ( 2) bt
       'bt      ', &
       !+ad_varc  <LI> ( 3) rmajor
       'rmajor  ', &
       !+ad_varc  <LI> ( 4) te
       'te      ', &
       !+ad_varc  <LI> ( 5) beta
       'beta    ', &
       !+ad_varc  <LI> ( 6) dene
       'dene    ', &
       !+ad_varc  <LI> ( 7) rnbeam
       'rnbeam  ', &
       !+ad_varc  <LI> ( 8) fbeta (f-value for equation 6)
       'fbeta   ', &
       !+ad_varc  <LI> ( 9) fdene (f-value for equation 5)
       'fdene   ', &
       !+ad_varc  <LI> (10) hfact
       'hfact   ', &
       !+ad_varc  <LI> (11) pheat
       'pheat   ', &
       !+ad_varc  <LI> (12) oacdcp
       'oacdcp  ', &
       !+ad_varc  <LI> (13) tfcth
       'tfcth   ', &
       !+ad_varc  <LI> (14) fwalld (f-value for equation 8)
       'fwalld  ', &
       !+ad_varc  <LI> (15) fvs (f-value for equation 12)
       'fvs     ', &
       !+ad_varc  <LI> (16) ohcth
       'ohcth   ', &
       !+ad_varc  <LI> (17) tdwell
       'tdwell  ', &
       !+ad_varc  <LI> (18) q
       'q       ', &
       !+ad_varc  <LI> (19) enbeam
       'enbeam  ', &
       !+ad_varc  <LI> (20) tcpav
       'tcpav   ', &
       !+ad_varc  <LI> (21) ftburn (f-value for equation 13)
       'ftburn  ', &
       !+ad_varc  <LI> (22) tbrnmn
       'tbrnmn  ', &
       !+ad_varc  <LI> (23) fcoolcp
       'fcoolcp ', &
       !+ad_varc  <LI> (24) cdtfleg
       'cdtfleg ', &
       !+ad_varc  <LI> (25) fpnetel (f-value for equation 16)
       'fpnetel ', &
       !+ad_varc  <LI> (26) ffuspow (f-value for equation 9)
       'ffuspow ', &
       !+ad_varc  <LI> (27) fhldiv (f-value for equation 18)
       'fhldiv  ', &
       !+ad_varc  <LI> (28) fjtfc (f-value for equation 23)
       'fjtfc   ', &
       !+ad_varc  <LI> (29) bore
       'bore    ', &
       !+ad_varc  <LI> (30) fmva (f-value for equation 19)
       'fmva    ', &
       !+ad_varc  <LI> (31) gapomin
       'gapomin ', &
       !+ad_varc  <LI> (32) frminor (f-value for equation 21)
       'frminor ', &
       !+ad_varc  <LI> (33) fportsz (f-value for equation 20)
       'fportsz ', &
       !+ad_varc  <LI> (34) fdivcol (f-value for equation 22)
       'fdivcol ', &
       !+ad_varc  <LI> (35) fpeakb (f-value for equation 25)
       'fpeakb  ', &
       !+ad_varc  <LI> (36) fbetatry (f-value for equation 24)
       'fbetatry', &
       !+ad_varc  <LI> (37) coheof
       'coheof  ', &
       !+ad_varc  <LI> (38) fjohc (f-value for equation 26)
       'fjohc   ', &
       !+ad_varc  <LI> (39) fjohc0 (f-value for equation 27)
       'fjohc0  ', &
       !+ad_varc  <LI> (40) fgamcd (f-value for equation 37)
       'fgamcd  ', &
       !+ad_varc  <LI> (41) fcohbop
       'fcohbop ', &
       !+ad_varc  <LI> (42) gapoh
       'gapoh   ', &
       !+ad_varc  <LI> (43) cfe0
       'cfe0    ', &
       !+ad_varc  <LI> (44) fvsbrnni
       'fvsbrnni', &
       !+ad_varc  <LI> (45) fqval (f-value for equation 28)
       'fqval   ', &
       !+ad_varc  <LI> (46) fpinj (f-value for equation 30)
       'fpinj   ', &
       !+ad_varc  <LI> (47) feffcd
       'feffcd  ', &
       !+ad_varc  <LI> (48) fstrcase 48 (f-value for equation 31)
       'fstrcase', &
       !+ad_varc  <LI> (49) fstrcond (f-value for equation 32)
       'fstrcond', &
       !+ad_varc  <LI> (50) fiooic (f-value for equation 33)
       'fiooic  ', &
       !+ad_varc  <LI> (51) fvdump (f-value for equation 34)
       'fvdump  ', &
       !+ad_varc  <LI> (52) vdalw
       'vdalw   ', &
       !+ad_varc  <LI> (53) fjprot (f-value for equation 35)
       'fjprot  ', &
       !+ad_varc  <LI> (54) ftmargtf (f-value for equation 36)
       'ftmargtf', &
       !+ad_varc  <LI> (55) tmargmin
       'tmargmin', &
       !+ad_varc  <LI> (56) tdmptf
       'tdmptf  ', &
       !+ad_varc  <LI> (57) thkcas
       'thkcas  ', &
       !+ad_varc  <LI> (58) thwcndut
       'thwcndut', &
       !+ad_varc  <LI> (59) fcutfsu
       'fcutfsu ', &
       !+ad_varc  <LI> (60) cpttf
       'cpttf   ', &
       !+ad_varc  <LI> (61) gapds
       'gapds   ', &
       !+ad_varc  <LI> (62) fdtmp (f-value for equation 38)
       'fdtmp   ', &
       !+ad_varc  <LI> (63) ftpeak (f-value for equation 39)
       'ftpeak  ', &
       !+ad_varc  <LI> (64) fauxmn (f-value for equation 40)
       'fauxmn  ', &
       !+ad_varc  <LI> (65) tohs
       'tohs    ', &
       !+ad_varc  <LI> (66) ftohs (f-value for equation 41)
       'ftohs   ', &
       !+ad_varc  <LI> (67) ftcycl (f-value for equation 42)
       'ftcycl  ', &
       !+ad_varc  <LI> (68) fptemp (f-value for equation 44)
       'fptemp  ', &
       !+ad_varc  <LI> (69) rcool
       'rcool   ', &
       !+ad_varc  <LI> (70) vcool
       'vcool   ', &
       !+ad_varc  <LI> (71) fq (f-value for equation 45)
       'fq      ', &
       !+ad_varc  <LI> (72) fipir (f-value for equation 46)
       'fipir   ', &
       !+ad_varc  <LI> (73) scrapli
       'scrapli ', &
       !+ad_varc  <LI> (74) scraplo
       'scraplo ', &
       !+ad_varc  <LI> (75) tfootfi
       'tfootfi ', &
       !+ad_varc  <LI> (76) frfptf (f-value for equation 47)
       'frfptf  ', &
       !+ad_varc  <LI> (77) tftort
       'tftort  ', &
       !+ad_varc  <LI> (78) rfpth
       'rfpth   ', &
       !+ad_varc  <LI> (79) fbetap (f-value for equation 48)
       'fbetap  ', &
       !+ad_varc  <LI> (80) frfpf (f-value for equation 49)
       'frfpf   ', &
       !+ad_varc  <LI> (81) edrive
       'edrive  ', &
       !+ad_varc  <LI> (82) drveff
       'drveff  ', &
       !+ad_varc  <LI> (83) tgain
       'tgain   ', &
       !+ad_varc  <LI> (84) chrad
       'chrad   ', &
       !+ad_varc  <LI> (85) pdrive
       'pdrive  ', &
       !+ad_varc  <LI> (86) frrmax (f-value for equation 50)
       'frrmax  ', &
       !+ad_varc  <LI> (87) helecmw
       'helecmw ', &
       !+ad_varc  <LI> (88) hthermmw </UL>
       'hthermmw' /)

  !+ad_vars  sqsumsq : sqrt of the sum of the square of the constraint residuals
  real(kind(1.0D0)) :: sqsumsq = 0.0D0
  !+ad_vars  epsfcn /1.0D-3/ : finite difference step length for HYBRD derivatives
  real(kind(1.0D0)) :: epsfcn = 1.0D-3
  !+ad_vars  epsvmc /1.0D-3/ : error tolerance for VMCON
  real(kind(1.0D0)) :: epsvmc = 1.0D-3
  !+ad_vars  factor /0.1/ : used in HYBRD for first step size
  real(kind(1.0D0)) :: factor = 0.1D0
  !+ad_vars  ftol /1.0D-4/ : error tolerance for HYBRD
  real(kind(1.0D0)) :: ftol = 1.0D-4

  !+ad_vars  boundl(ipnvars) : lower bounds used on ixc variables during
  !+ad_varc                    VMCON optimisation runs
  real(kind(1.0D0)), dimension(ipnvars) :: boundl = (/ &
       1.100D0, &  !  1 
       0.010D0, &  !  2 
       0.100D0, &  !  3 
       5.000D0, &  !  4 
       0.001D0, &  !  5 
       1.00D19, &  !  6 
       1.00D-6, &  !  7 
       0.001D0, &  !  8 
       0.001D0, &  !  9 
       0.100D0, &  !  10
       1.000D6, &  !  11
       1.000D5, &  !  12
       0.100D0, &  !  13
       0.001D0, &  !  14
       0.001D0, &  !  15
       0.001D0, &  !  16
       0.100D0, &  !  17
       2.000D0, &  !  18
       1.000D0, &  !  19
       40.00D0, &  !  20
       0.001D0, &  !  21
       0.001D0, &  !  22
       0.100D0, &  !  23
       1.000D4, &  !  24
       1.000D0, &  !  25
       0.001D0, &  !  26
       0.001D0, &  !  27
       0.100D0, &  !  28
       0.100D0, &  !  29
       0.010D0, &  !  30
       0.001D0, &  !  31
       0.001D0, &  !  32
       0.010D0, &  !  33
       0.001D0, &  !  34
       0.001D0, &  !  35
       0.001D0, &  !  36
       1.000D5, &  !  37
       0.010D0, &  !  38
       0.001D0, &  !  39
       0.001D0, &  !  40
       0.001D0, &  !  41
       0.000D0, &  !  42  N.B. Is 0.0 wise?
       1.00D-6, &  !  43
       0.001D0, &  !  44
       0.010D0, &  !  45
       0.001D0, &  !  46
       0.001D0, &  !  47
       0.001D0, &  !  48
       0.001D0, &  !  49
       0.001D0, &  !  50
       0.001D0, &  !  51
       0.001D0, &  !  52
       0.001D0, &  !  53
       0.001D0, &  !  54
       0.001D0, &  !  55
       10.00D0, &  !  56
       0.050D0, &  !  57
       0.001D0, &  !  58
       0.001D0, &  !  59
       0.001D0, &  !  60
       0.001D0, &  !  61
       0.001D0, &  !  62
       0.001D0, &  !  63
       0.001D0, &  !  64
       0.100D0, &  !  65
       0.001D0, &  !  66
       0.001D0, &  !  67
       0.001D0, &  !  68
       0.001D0, &  !  69
       1.000D0, &  !  70
       0.001D0, &  !  71
       0.001D0, &  !  72
       0.001D0, &  !  73
       0.001D0, &  !  74
       0.200D0, &  !  75
       0.001D0, &  !  76
       0.050D0, &  !  77
       0.010D0, &  !  78
       0.001D0, &  !  79
       0.001D0, &  !  80
       1.000D5, &  !  81
       0.010D0, &  !  82
       1.000D0, &  !  83
       0.100D0, &  !  84
       1.000D6, &  !  85
       0.001D0, &  !  86
       1.000D0, &  !  87
       1.000D0 /)  !  88
       
  !+ad_vars  boundu(ipnvars) : upper bounds used on ixc variables during
  !+ad_varc                    VMCON optimisation runs
  real(kind(1.0D0)), dimension(ipnvars) :: boundu = (/ &
       10.00D0, &  !  1 
       100.0D0, &  !  2 
       10.00D0, &  !  3 
       500.0D0, &  !  4 
       1.000D0, &  !  5 
       1.00D21, &  !  6 
       1.00D20, &  !  7 
       1.000D0, &  !  8 
       1.000D0, &  !  9 
       3.000D0, &  !  10
       1.000D9, &  !  11
       1.500D8, &  !  12
       5.000D0, &  !  13
       1.000D0, &  !  14
       1.000D0, &  !  15
       1.000D2, &  !  16
       1.000D8, &  !  17
       100.0D0, &  !  18
       1.000D6, &  !  19
       1.000D3, &  !  20
       1.000D0, &  !  21
       1.000D6, &  !  22
       0.500D0, &  !  23
       1.000D8, &  !  24
       1.000D0, &  !  25
       1.000D0, &  !  26
       1.000D0, &  !  27
       1.000D0, &  !  28
       10.00D0, &  !  29
       1.000D0, &  !  30
       1.000D1, &  !  31
       1.000D0, &  !  32
       1.000D0, &  !  33
       1.000D0, &  !  34
       1.000D0, &  !  35
       1.000D0, &  !  36
       1.000D8, &  !  37
       1.000D0, &  !  38
       1.000D0, &  !  39
       1.000D0, &  !  40
       1.000D0, &  !  41
       10.00D0, &  !  42
       3.00D-3, &  !  43
       1.000D0, &  !  44
       0.330D0, &  !  45
       1.000D0, &  !  46
       1.000D0, &  !  47
       1.000D0, &  !  48
       1.000D0, &  !  49
       0.500D0, &  !  50
       1.000D0, &  !  51
       1.000D6, &  !  52
       1.000D0, &  !  53
       1.000D0, &  !  54
       100.0D0, &  !  55
       1.000D6, &  !  56
       1.000D0, &  !  57
       1.000D0, &  !  58
       1.000D0, &  !  59
       4.000D4, &  !  60
       10.00D0, &  !  61
       1.000D0, &  !  62
       1.000D0, &  !  63
       1.000D0, &  !  64
       1.000D3, &  !  65
       1.000D0, &  !  66
       1.000D0, &  !  67
       1.000D0, &  !  68
       0.010D0, &  !  69
       1.000D2, &  !  70
       1.000D0, &  !  71
       1.000D0, &  !  72
       10.00D0, &  !  73
       10.00D0, &  !  74
       5.000D0, &  !  75
       1.000D0, &  !  76
       2.000D0, &  !  77
       1.800D0, &  !  78
       1.000D0, &  !  79
       1.000D0, &  !  80
       5.000D7, &  !  81
       1.000D0, &  !  82
       500.0D0, &  !  83
       20.00D0, &  !  84
       200.0D6, &  !  85
       1.000D0, &  !  86
       4.000D3, &  !  87
       4.000D3 /)  !  88

  real(kind(1.0D0)), dimension(ipnvars) :: bondl = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: bondu = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: rcm = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: resdl = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: scafc = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: scale = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: xcm = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: xcs = 0.0D0
  real(kind(1.0D0)), dimension(ipvlam)  :: vlam = 0.0D0

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine eqsolv(fcnhyb,n,x,fvec,tol,epsfcn,factor,nprint,info, &
       wa,lwa,resdl,nfev)

    !+ad_name  eqsolv
    !+ad_summ  Find the non-optimising HYBRID solution to the problem
    !+ad_type  Subroutine
    !+ad_auth  Argonne National Laboratory. Minpack Project. March 1980.
    !+ad_auth  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  fcnhyb : external routine name : see below
    !+ad_args  n : input integer : number of functions and variables
    !+ad_args  x(n) : input/output real array : On input X must contain
    !+ad_argc    an initial estimate of the solution vector. On output X
    !+ad_argc    contains the final estimate of the solution vector.
    !+ad_args  fvec(n) : output real array : Functions evaluated at output X
    !+ad_args  tol : input real : Termination occurs when the algorithm
    !+ad_argc    estimates that the relative error between X and the solution
    !+ad_argc    is at most TOL.
    !+ad_args  epsfcn : input real : Used in determining a suitable
    !+ad_argc    step length for the forward-difference approximation
    !+ad_argc    (see <A HREF="hybrd.html">hybrd</A>)
    !+ad_args  factor : input real : Used in determining the initial step bound
    !+ad_argc    (see <A HREF="hybrd.html">hybrd</A>)
    !+ad_args  nprint : input integer : Number of iterations between print-outs
    !+ad_args  info : output integer : If the user has terminated execution,
    !+ad_argc    INFO is set to the (negative) value of IFLAG, see description below.
    !+ad_argc    Otherwise, INFO is set as follows:
    !+ad_argc    <PRE>
    !+ad_argc    INFO = 0   Improper input parameters.
    !+ad_argc    INFO = 1   Algorithm estimates that the relative error
    !+ad_argc               between X and the solution is at most TOL.
    !+ad_argc    INFO = 2   Number of calls to FCNHYB has reached or exceeded
    !+ad_argc               200*(N+1).
    !+ad_argc    INFO = 3   TOL is too small. No further improvement in
    !+ad_argc               the approximate solution X is possible.
    !+ad_argc    INFO = 4   Iteration is not making good progress.
    !+ad_argc    </PRE>
    !+ad_args  wa(lwa) : input/output real array : work array
    !+ad_args  lwa : input integer : work array size, not less than (N*(3*N+13))/2
    !+ad_args  resdl(n) : output real array : residuals
    !+ad_args  nfev : output integer : number of iterations performed
    !+ad_desc  Routine EQSOLV is the Argonne Minpack subroutine HYBRD1
    !+ad_desc  which has been modified by D.T. Blackfield FEDC/TRW.
    !+ad_desc  The routine is the same except some of the arguments are
    !+ad_desc  user supplied rather than 'hardwired'.
    !+ad_desc  <P>The purpose of EQSOLV is to find a zero of a system of
    !+ad_desc  N nonlinear functions in N variables by a modification
    !+ad_desc  of the Powell hybrid method. This is done by using the
    !+ad_desc  more general nonlinear equation solver <A HREF="hybrd.html">HYBRD</A>.
    !+ad_desc  The user must provide a subroutine which calculates the functions.
    !+ad_desc  The Jacobian is then calculated by a forward-difference
    !+ad_desc  approximation.
    !+ad_desc  <P>FCNHYB is the name of a user-supplied subroutine which
    !+ad_desc  calculates the functions. FCNHYB must be declared
    !+ad_desc  in an external statement in the user calling
    !+ad_desc  program, and should be written as follows:
    !+ad_desc  <PRE>
    !+ad_desc  subroutine fcnhyb(n,x,fvec,iflag)
    !+ad_desc  integer n,iflag
    !+ad_desc  double precision x(n),fvec(n)
    !+ad_desc  ----------
    !+ad_desc  calculate the functions at x and
    !+ad_desc  return this vector in fvec.
    !+ad_desc  ---------
    !+ad_desc  return
    !+ad_desc  end
    !+ad_desc  </PRE>
    !+ad_desc  The value of iflag should not be changed by FCNHYB unless
    !+ad_desc  the user wants to terminate execution of EQSOLV.
    !+ad_desc  In this case set IFLAG to a negative integer.
    !+ad_prob  None
    !+ad_call  fcnhyb
    !+ad_call  hybrd
    !+ad_hist  27/07/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    external :: fcnhyb
    integer, intent(in) :: n, nprint, lwa
    real(kind(1.0D0)), dimension(n), intent(inout) :: x
    real(kind(1.0D0)), dimension(n), intent(out) :: fvec, resdl
    real(kind(1.0D0)), dimension(lwa), intent(out) :: wa
    real(kind(1.0D0)), intent(in) :: tol, epsfcn, factor
    integer, intent(out) :: info, nfev

    !  Local variables

    integer :: n1,indx,lr,maxfev,ml,mode,mu
    real(kind(1.0D0)), parameter :: one = 1.0D0
    real(kind(1.0D0)), parameter :: zero = 0.0D0
    real(kind(1.0D0)) :: xtol

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    info = 0

    !  Check the input parameters for errors

    if ( (n == 0).or.(tol < zero).or.(lwa < ((n*(3*n + 13))/2) ) ) return

    !  Call HYBRD

    maxfev = 200*(n + 1)
    xtol = tol
    ml = n - 1
    mu = n - 1
    mode = 2

    wa(:) = one

    lr = (n*(n + 1))/2
    indx = 6*n + lr
    n1 = n

    !+**PJK 23/10/92 Warning produced by QA Fortran :
    !+**PJK 23/10/92 Arg 16 in call to HYBRD has wrong dimensions.
    !+**PJK 23/10/92 Code works at present, but beware of future
    !+**PJK 23/10/92 modifications.

    call hybrd(fcnhyb,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,wa(1),mode, &
         factor,nprint,info,nfev,wa(indx+1),n1,wa(6*n+1),lr, &
         wa(n+1),wa(2*n+1),wa(3*n+1),wa(4*n+1),wa(5*n+1), &
         resdl)

    if (info == 5) info = 4

  end subroutine eqsolv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine optimiz(fcnvmc1,fcnvmc2,ifail,f)

    !+ad_name  optimiz
    !+ad_summ  Calls the minimisation/maximisation routine VMCON
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  fcnvmc1 : external routine : objective function evaluator
    !+ad_args  fcnvmc2 : external routine : gradient objective function evaluator
    !+ad_args  ifail   : output integer : error flag
    !+ad_args  f       : output real    : value of objective function at the output point
    !+ad_desc  This routine calls the minimisation/maximisation routine VMCON,
    !+ad_desc  developed by Argonne National Laboratory.
    !+ad_desc  On exit, the (normalised) value of the variable being maximised
    !+ad_desc  or minimised (i.e. the figure of merit) is returned in argument
    !+ad_desc  <CODE>f</CODE>.
    !+ad_prob  None
    !+ad_call  fcnvmc1
    !+ad_call  fcnvmc2
    !+ad_call  vmcon
    !+ad_hist  02/10/96 PJK Initial upgraded version
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  10/10/12 Added arguments fcnvmc1, fcnvmc2
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    external :: fcnvmc1, fcnvmc2
    integer, intent(out) :: ifail
    real(kind(1.0D0)), intent(out) :: f

    !  Local variables

    integer :: ii,lb,lcnorm,ldel,lh,liwa,lwa,m,meq,mode,n
    integer, parameter :: ippn1  = ipnvars+1
    integer, parameter :: ipldel = 7*ippn1
    integer, parameter :: iplh   = 2*ippn1
    integer, parameter :: ipvmu  = ipeqns+2*ipnvars+1
    integer, parameter :: ipliwa = 6*ippn1+ipeqns
    integer, dimension(ipliwa) :: iwa
    integer, dimension(ipnvars) :: ilower,iupper

    real(kind(1.0D0)) :: xtol
    real(kind(1.0D0)), dimension(ipnvars) :: bdelta,bndl,bndu,etav,fgrd, &
         gammv,glag,glaga,xa,xv
    real(kind(1.0D0)), dimension(ipeqns) :: cm,conf
    real(kind(1.0D0)), dimension(ippn1) :: bdl,bdu,gm
    real(kind(1.0D0)), dimension(ipvmu) :: vmu
    real(kind(1.0D0)), dimension(ipldel) :: delta
    real(kind(1.0D0)), dimension(iplh) :: wa
    real(kind(1.0D0)), dimension(ippn1,ipeqns) :: cnorm
    real(kind(1.0D0)), dimension(ippn1,ippn1) :: b
    real(kind(1.0D0)), dimension(iplh,iplh) :: h

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    n = nvar
    m = neqns
    meq = neqns-nineqns
    xtol = epsvmc
    mode = 0
    lb = ippn1
    lcnorm = ippn1
    ldel = ipldel
    lh = iplh
    lwa = iplh
    liwa = ipliwa

    do ii = 1,n
       ilower(ii) = 1
       iupper(ii) = 1
       bndl(ii) = bondl(ii)
       bndu(ii) = bondu(ii)
       xv(ii) = xcm(ii)
    end do

    nvrbl = nvar
    call vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
         lcnorm,b,lb,xtol,maxcal,ifail,nfev2,vlam,glag,vmu,cm,glaga, &
         gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
         liwa,ilower,iupper,bndl,bndu)

    do ii = 1,n
       xcm(ii) = xv(ii)
    end do

    do ii = 1,m
       rcm(ii) = conf(ii)
    end do

  end subroutine optimiz

end module numerics
