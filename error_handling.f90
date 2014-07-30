! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module error_handling

  !+ad_name  error_handling
  !+ad_summ  Error handling module for PROCESS
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  initialise_error_list
  !+ad_cont  report_error
  !+ad_cont  show_errors
  !+ad_args  N/A
  !+ad_desc  This module provides a centralised method for dealing with
  !+ad_desc  errors generated by PROCESS.
  !+ad_desc  <P>All possible informational/error messages are initialised
  !+ad_desc  via a call to <A HREF="initialise_error_list.html">
  !+ad_desc  <CODE>initialise_error_list</CODE></A>. Thereafter, any routine
  !+ad_desc  that needs to flag a message should call <A HREF="report_error.html">
  !+ad_desc  <CODE>report_error</CODE></A> with the relevant error identifier as
  !+ad_desc  the argument. Up to eight integer and eight floating-point diagnostic
  !+ad_desc  values may be saved by the user in arrays <CODE>idiags</CODE> and
  !+ad_desc  <CODE>fdiags</CODE>, respectively, for debugging purposes.
  !+ad_desc  <P>The list of messages reported during the course of a run
  !+ad_desc  may be displayed by calling routine
  !+ad_desc  <A HREF="show_errors.html"><CODE>show_errors</CODE></A>.
  !+ad_desc  <P>The <CODE>error_status</CODE> variable returns the highest severity
  !+ad_desc  level that has been encountered; if a severe error is flagged
  !+ad_desc  (level 3) the program is terminated immediately.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_hist  25/06/14 PJK Initial version
  !+ad_hist  09/07/14 PJK Added errors_on switch
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  private
  public :: errors_on, error_status, idiags, fdiags
  public :: initialise_error_list, report_error, show_errors

  !  Switch to turn error handling on
  !  Error reporting is turned off, until either a severe error is found, or
  !  during an output step.  Warnings during intermediate iteration steps
  !  may be premature and might clear themselves at the final converged point.

  logical :: errors_on = .false.

  !  Levels of severity

  integer, parameter :: ERROR_OKAY = 0
  integer, parameter :: ERROR_INFO = 1
  integer, parameter :: ERROR_WARN = 2
  integer, parameter :: ERROR_SEVERE = 3

  !+ad_vars  error_id : identifier for final message encountered
  integer :: error_id = 0

  !  Overall status

  !+ad_vars  error_status : overall status flag for a run; on exit:<UL>
  !+ad_varc                 <LI> 0  all okay
  !+ad_varc                 <LI> 1  informational messages have been encountered
  !+ad_varc                 <LI> 2  warning (non-fatal) messages have been encountered
  !+ad_varc                 <LI> 3  severe (fatal) errors have occurred</UL>
  integer :: error_status = ERROR_OKAY

  integer, parameter :: INT_DEFAULT = -999999
  real(kind(1.0D0)), parameter :: FLT_DEFAULT = real(INT_DEFAULT, kind(1.0D0))

  !  Arrays for diagnostic output

  integer, dimension(8) :: idiags = INT_DEFAULT
  real(kind(1.0D0)), dimension(8) :: fdiags = FLT_DEFAULT

  !  Individual error item
  !  int and float arrays may be useful to provide diagnostic information

  type :: error
     integer           :: level    !  severity level
     character(len=80) :: message  !  information string
     integer, dimension(8) :: idiags = INT_DEFAULT
     real(kind(1.0D0)), dimension(8) :: fdiags = FLT_DEFAULT
  end type error

  !  Individual element in an error list

  type :: error_list_item
     integer                         :: id    !  identifier
     type (error)                    :: data  !  error details
     type (error_list_item), pointer :: ptr   !  linked list pointer
  end type error_list_item

  !  Pointers to head and tail of the error list

  type (error_list_item), pointer :: error_head => null()
  type (error_list_item), pointer :: error_tail => null()

  !  List of messages

  integer, parameter :: n_errortypes = 136
  type(error), dimension(n_errortypes) :: error_type

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialise_error_list

    !+ad_name  initialise_error_list
    !+ad_summ  Initialises the informational/error message list
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine sets all the possible informational/error messages
    !+ad_desc  that may be used during the course of a run. Thus, it needs
    !+ad_desc  to be called during the initialisation phase.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  25/06/14 PJK Initial version
    !+ad_hist  09/07/14 PJK Added errors 131-135
    !+ad_hist  29/07/14 PJK Added error 136
    !+ad_hist  30/07/14 PJK Modified 51, 63, 103
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  N.B. Don't forget to increment n_errortypes in the module
    !  header if a new entry is required

    error_type(:)%level = ERROR_OKAY
    error_type(:)%message = ' '

    error_type(1)%level = ERROR_SEVERE
    error_type(1)%message = &
         'CONSTRAINTS: Do not use constraint 7 if ignite=1'
    error_type(2)%level = ERROR_SEVERE
    error_type(2)%message = &
         'CONSTRAINTS: Constraint equation 15 is redundant'
    error_type(3)%level = ERROR_SEVERE
    error_type(3)%message = &
         'CONSTRAINTS: Constraint equation 23 is redundant; use 33 and/or 35 instead'
    error_type(4)%level = ERROR_SEVERE
    error_type(4)%message = &
         'CONSTRAINTS: Do not use constraint 28 if ignite=1'
    error_type(5)%level = ERROR_SEVERE
    error_type(5)%message = &
         'CONSTRAINTS: tpeak = 0 ==> lpulse=0; do not use constraint 39 if lpulse=0'
    error_type(6)%level = ERROR_SEVERE
    error_type(6)%message = &
         'CONSTRAINTS: tcycmn = 0 ==> lpulse=0; do not use constraint 42 if lpulse=0'
    error_type(7)%level = ERROR_SEVERE
    error_type(7)%message = &
         'CONSTRAINTS: Do not use constraint 43 if itart=0'
    error_type(8)%level = ERROR_SEVERE
    error_type(8)%message = &
         'CONSTRAINTS: Do not use constraint 44 if itart=0'
    error_type(9)%level = ERROR_SEVERE
    error_type(9)%message = &
         'CONSTRAINTS: Do not use constraint 45 if itart=0'
    error_type(10)%level = ERROR_SEVERE
    error_type(10)%message = &
         'CONSTRAINTS: Do not use constraint 46 if itart=0'
    error_type(11)%level = ERROR_SEVERE
    error_type(11)%message = &
         'CONSTRAINTS: Do not use constraint 47 if irfp=0'
    error_type(12)%level = ERROR_SEVERE
    error_type(12)%message = &
         'CONSTRAINTS: Do not use constraint 50 if ife=0'
    error_type(13)%level = ERROR_SEVERE
    error_type(13)%message = &
         'CONSTRAINTS: No such constraint equation number...'
    error_type(14)%level = ERROR_SEVERE
    error_type(14)%message = &
         'CONSTRAINTS: NaN/infty error for constraint equation...'
    error_type(15)%level = ERROR_SEVERE
    error_type(15)%message = &
         'ITERNB: Imminent negative square root argument; NBI will miss plasma completely'
    error_type(16)%level = ERROR_WARN
    error_type(16)%message = &
         'LHRAD: LH penetration radius not found after lapno iterations, using 0.8*rminor'
    error_type(17)%level = ERROR_SEVERE
    error_type(17)%message = &
         'ECCDEF: Negative normalised current drive efficiency'
    error_type(18)%level = ERROR_SEVERE
    error_type(18)%message = &
         'LEGEND: Invalid argument'
    error_type(19)%level = ERROR_SEVERE
    error_type(19)%message = &
         'LEGEND: Solution has not converged'
    error_type(20)%level = ERROR_SEVERE
    error_type(20)%message = &
         'CULNBI: Imminent negative square root argument; NBI will miss plasma completely'
    error_type(21)%level = ERROR_SEVERE
    error_type(21)%message = &
         'ETANB2: Imminent negative square root argument; NBI will miss plasma completely'
    error_type(22)%level = ERROR_SEVERE
    error_type(22)%message = &
         'DIVCALL: Non-positive vgap'
    error_type(23)%level = ERROR_SEVERE
    error_type(23)%message = &
         'FUNFOM: Figure of merit cfactr requires iavail=1'
    error_type(24)%level = ERROR_SEVERE
    error_type(24)%message = &
         'FUNFOM: No such figure of merit'
    error_type(25)%level = ERROR_SEVERE
    error_type(25)%message = &
         'FUNFOM: NaN error in figure of merit calculation'
    error_type(26)%level = ERROR_SEVERE
    error_type(26)%message = &
         'IFEFBS: Illegal fbreed value'
    error_type(27)%level = ERROR_SEVERE
    error_type(27)%message = &
         'INIT_IMP_ELEMENT: Illegal impurity number'
    error_type(28)%level = ERROR_SEVERE
    error_type(28)%message = &
         'INIT_IMP_ELEMENT: Allocation problem'
    error_type(29)%level = ERROR_WARN
    error_type(29)%message = &
         'INIT_IMP_ELEMENT: Impurity datafiles missing; switching to original model'
    error_type(30)%level = ERROR_SEVERE
    error_type(30)%message = &
         'IMPORT_IMPDATA: Problem opening data file'
    error_type(31)%level = ERROR_SEVERE
    error_type(31)%message = &
         'IMPORT_IMPDATA: Problem reading data file header'
    error_type(32)%level = ERROR_SEVERE
    error_type(32)%message = &
         'IMPORT_IMPDATA: Problem reading data file contents'
    error_type(33)%level = ERROR_SEVERE
    error_type(33)%message = &
         'Z2INDEX: Element with the given charge is not in the impurity array'
    error_type(34)%level = ERROR_SEVERE
    error_type(34)%message = &
         'ELEMENT2INDEX: Element with given label not found in impurity array'
    error_type(35)%level = ERROR_WARN
    error_type(35)%message = &
         'PIMPDEN: Impurity radiation model is inaccurate at such low temperatures'
    error_type(36)%level = ERROR_SEVERE
    error_type(36)%message = &
         'CHECK: Fuel ion fractions do not sum to 1.0; check fdeut, ftrit, fhe3 values'
    error_type(37)%level = ERROR_WARN
    error_type(37)%message = &
         'CHECK: Usual current scaling for TARTs (icurr=2) is not being used'
    error_type(38)%level = ERROR_SEVERE
    error_type(38)%message = &
         'CHECK: ibss=1 is not a valid option for a TART device'
    error_type(39)%level = ERROR_SEVERE
    error_type(39)%message = &
         'CHECK: snull=1 is not a valid option for a TART device'
    error_type(40)%level = ERROR_SEVERE
    error_type(40)%message = &
         'CHECK: icurr=2 is not a valid option for a non-TART device'
    error_type(41)%level = ERROR_SEVERE
    error_type(41)%message = &
         'CHECK: ncls(i) .ne. 2 is not a valid option except for (ipfloc = 2)'
    error_type(42)%level = ERROR_SEVERE
    error_type(42)%message = &
         'CHECK: Only 1 divertor coil (ipfloc = 2) is not a valid configuration'
    error_type(43)%level = ERROR_SEVERE
    error_type(43)%message = &
         'CHECK: More than 2 divertor coils (ipfloc = 2) is not a valid configuration'
    error_type(44)%level = ERROR_SEVERE
    error_type(44)%message = &
         'CHECK: If snull=1, use 2 individual divertor coils (ipfloc = 2, 2; ncls = 1, 1)'
    error_type(45)%level = ERROR_SEVERE
    error_type(45)%message = &
         'CHECK: itart=1 is not a valid option for the RFP model'
    error_type(46)%level = ERROR_SEVERE
    error_type(46)%message = &
         'LOADXC: Do not use tfcth as an iteration variable if istell=1'
    error_type(47)%level = ERROR_SEVERE
    error_type(47)%message = &
         'LOADXC: Do not use cdtfleg as an iteration variable if itfsup=1 or irfp=1'
    error_type(48)%level = ERROR_SEVERE
    error_type(48)%message = &
         'LOADXC: Do not use thkcas as an iteration variable if tfc_model=0 or istell=1'
    error_type(49)%level = ERROR_SEVERE
    error_type(49)%message = &
         'LOADXC: Do not use cpttf as an iteration variable if itfsup=0 or istell=1'
    error_type(50)%level = ERROR_SEVERE
    error_type(50)%message = &
         'LOADXC: Do not use tohs as an iteration variable if lpulse /= 1'
    error_type(51)%level = ERROR_SEVERE
    error_type(51)%message = &
         'LOADXC: Do not use tftort as an iteration variable if irfp=0'
    error_type(52)%level = ERROR_SEVERE
    error_type(52)%message = &
         'LOADXC: Do not use helecmw as an iteration variable if ihplant is not 1,2 or 3'
    error_type(53)%level = ERROR_SEVERE
    error_type(53)%message = &
         'LOADXC: Do not use hthermmw as an iteration variable if ihplant < 4'
    error_type(54)%level = ERROR_SEVERE
    error_type(54)%message = &
         'LOADXC: Illegal iteration variable number'
    error_type(55)%level = ERROR_SEVERE
    error_type(55)%message = &
         'LOADXC: Iteration variable is zero; change its initial value or lower bound'
    error_type(56)%level = ERROR_SEVERE
    error_type(56)%message = &
         'LOADXC: NaN error for iteration variable'
    error_type(57)%level = ERROR_SEVERE
    error_type(57)%message = &
         'CONVXC: Illegal iteration variable number'
    error_type(58)%level = ERROR_SEVERE
    error_type(58)%message = &
         'CONVXC: Iteration variable is zero; change its initial value or lower bound'
    error_type(59)%level = ERROR_SEVERE
    error_type(59)%message = &
         'CONVXC: NaN error for iteration variable'
    error_type(60)%level = ERROR_SEVERE
    error_type(60)%message = &
         'CONVXC: scale(i) = 0 for iteration variable'
    error_type(61)%level = ERROR_SEVERE
    error_type(61)%message = &
         'RADIALB: fhole+fdiv+fhcd is too high for a credible outboard wall area'
    error_type(62)%level = ERROR_INFO
    error_type(62)%message = &
         'RADIALB: Ripple result may be inaccurate, as the fit has been extrapolated'
    error_type(63)%level = ERROR_WARN
    error_type(63)%message = &
         'PORTSZ: Max beam tangency radius set =0 temporarily; change beamwd'
    error_type(64)%level = ERROR_SEVERE
    error_type(64)%message = &
         'PFCOIL: ngrp is larger than ngrpmx'
    error_type(65)%level = ERROR_SEVERE
    error_type(65)%message = &
         'PFCOIL: Too many coils in a PF coil group'
    error_type(66)%level = ERROR_SEVERE
    error_type(66)%message = &
         'PFCOIL: Too many filaments nfxf repesenting the OH coil'
    error_type(67)%level = ERROR_SEVERE
    error_type(67)%message = &
         'PFCOIL: Illegal ipfloc value'
    error_type(68)%level = ERROR_SEVERE
    error_type(68)%message = &
         'PFCOIL: Too many test points npts across plasma midplane'
    error_type(69)%level = ERROR_SEVERE
    error_type(69)%message = &
         'PFCOIL: ipfloc(i) should not be 1 if itart=1'
    error_type(70)%level = ERROR_SEVERE
    error_type(70)%message = &
         'PFCOIL: Illegal value of ipfloc(i)'
    error_type(71)%level = ERROR_WARN
    error_type(71)%message = &
         'PFCOIL: OH coil not present; check volt-second calculations...'
    error_type(72)%level = ERROR_SEVERE
    error_type(72)%message = &
         'PEAKB: Illegal value of it; possible rounding error'
    error_type(73)%level = ERROR_WARN
    error_type(73)%message = &
         'INDUCT: Max no. of segments noh for OH coil > nohmax; increase ohcth lower bound'
    error_type(74)%level = ERROR_SEVERE
    error_type(74)%message = &
         'INDUCT: Negative square root imminent; raise ohcth or its upper limit'
    error_type(75)%level = ERROR_SEVERE
    error_type(75)%message = &
         'PHYSICS: Illegal value of ibss'
    error_type(76)%level = ERROR_SEVERE
    error_type(76)%message = &
         'BOOTSTRAP_FRACTION_WILSON: Illegal profile value found'
    error_type(77)%level = ERROR_SEVERE
    error_type(77)%message = &
         'CULCUR: Illegal value for icurr'
    error_type(78)%level = ERROR_WARN
    error_type(78)%message = &
         'PLASMA_COMPOSITION: Fuel ion density is zero or negative; forced to be positive'
    error_type(79)%level = ERROR_SEVERE
    error_type(79)%message = &
         'CULDLM: Illegal value for idensl'
    error_type(80)%level = ERROR_WARN
    error_type(80)%message = &
         'CULDLM: qcyl < 4/3; dlimit(4) set to zero; model 5 will be enforced instead'
    error_type(81)%level = ERROR_SEVERE
    error_type(81)%message = &
         'PCOND: Illegal value for isc'
    error_type(82)%level = ERROR_SEVERE
    error_type(82)%message = &
         'RADPWR: Illegal value for imprad_model'
    error_type(83)%level = ERROR_WARN
    error_type(83)%message = &
         'POHM: Negative plasma resistance rplas'
    error_type(84)%level = ERROR_SEVERE
    error_type(84)%message = &
         'SGVHOT: Illegal value for iabm'
    error_type(85)%level = ERROR_SEVERE
    error_type(85)%message = &
         'OUTPLAS: Illegal value of idivrt'
    error_type(86)%level = ERROR_SEVERE
    error_type(86)%message = &
         'OUTPLAS: Illegal value of ishape'
    error_type(87)%level = ERROR_WARN
    error_type(87)%message = &
         'OUTPLAS: Possible problem with high radiation power, forcing pdivt to odd values'
    error_type(88)%level = ERROR_WARN
    error_type(88)%message = &
         'THRMAL: Optimisation has failed within 100 iterations; possible NaN problems'
    error_type(89)%level = ERROR_SEVERE
    error_type(89)%message = &
         'THRMAL: Inner pipe radius afw >= outer pipe radius bfw'
    error_type(90)%level = ERROR_WARN
    error_type(90)%message = &
         'THRMAL: Swelling limit exceeded; optimisation failing to find a FW thickness'
    error_type(91)%level = ERROR_SEVERE
    error_type(91)%message = &
         'SMT: No reliable data for SMT stress for temperatures > 600 degrees Celsius'
    error_type(92)%level = ERROR_SEVERE
    error_type(92)%message = &
         'CYCLES: Fatigue data unreliable for T > 649 degrees Celsius'
    error_type(93)%level = ERROR_WARN
    error_type(93)%message = &
         'BURN: Negative burn time available; reduce theat or raise PF coil V-s capability'
    error_type(94)%level = ERROR_SEVERE
    error_type(94)%message = &
         'SCAN: Illegal value of isweep'
    error_type(95)%level = ERROR_SEVERE
    error_type(95)%message = &
         'SCAN: Do not scan cfactr if iavail=1'
    error_type(96)%level = ERROR_SEVERE
    error_type(96)%message = &
         'SCAN: Illegal scan variable number nsweep'
    error_type(97)%level = ERROR_WARN
    error_type(97)%message = &
         'SCTFCOIL: Negative TF coil current; ritfc forced to be positive...'
    error_type(98)%level = ERROR_WARN
    error_type(98)%message = &
         'SCTFCOIL: Negative winding pack thickness; increase tfcth or its lower bound'
    error_type(99)%level = ERROR_WARN
    error_type(99)%message = &
         'SCTFCOIL: Winding pack cross-section problem...'
    error_type(100)%level = ERROR_WARN
    error_type(100)%message = &
         'SCTFCOIL: Negative cable space dimension; reduce thicknesses or raise cpttf'
    error_type(101)%level = ERROR_WARN
    error_type(101)%message = &
         'SCTFCOIL: Negative cable space dimension'
    error_type(102)%level = ERROR_WARN
    error_type(102)%message = &
         'SCTFCOIL: Cable space area problem; artificially set rounded corner radius to 0'
    error_type(103)%level = ERROR_WARN
    error_type(103)%message = &
         'SCTFCOIL: Inconsistent TF leg toroidal thickness; raise tftort via constraint 57'
    error_type(104)%level = ERROR_SEVERE
    error_type(104)%message = &
         'STRESSCL: Illegal value for tfc_model'
    error_type(105)%level = ERROR_SEVERE
    error_type(105)%message = &
         'SUPERCON: Illegal value for isumattf'
    error_type(106)%level = ERROR_WARN
    error_type(106)%message = &
         'BI2212: Fit extrapolated outside of range of validity'
    error_type(107)%level = ERROR_SEVERE
    error_type(107)%message = &
         'STHEAT: Illegal value for isthtr'
    error_type(108)%level = ERROR_SEVERE
    error_type(108)%message = &
         'STDLIM: Negative square root imminent'
    error_type(109)%level = ERROR_SEVERE
    error_type(109)%message = &
         'STCOIL: Illegal value for nbticool'
    error_type(110)%level = ERROR_SEVERE
    error_type(110)%message = &
         'STCOIL: Use isumattf = 1 or 3 with the stellarator model'
    error_type(111)%level = ERROR_WARN
    error_type(111)%message = &
         'INTERSECT: X ranges not overlapping'
    error_type(112)%level = ERROR_WARN
    error_type(112)%message = &
         'INTERSECT: X has dropped below Xmin; X has been set equal to Xmin'
    error_type(113)%level = ERROR_WARN
    error_type(113)%message = &
         'INTERSECT: X has risen above Xmax; X has been set equal to Xmax'
    error_type(114)%level = ERROR_WARN
    error_type(114)%message = &
         'INTERSECT: Convergence too slow; X may be wrong...'
    error_type(115)%level = ERROR_SEVERE
    error_type(115)%message = &
         'CPOST: Illegal value for rtop'
    error_type(116)%level = ERROR_SEVERE
    error_type(116)%message = &
         'CPOST: Illegal value for ztop'
    error_type(117)%level = ERROR_SEVERE
    error_type(117)%message = &
         'CPOST: Illegal value for rmid'
    error_type(118)%level = ERROR_SEVERE
    error_type(118)%message = &
         'CPOST: Illegal value for hmax'
    error_type(119)%level = ERROR_SEVERE
    error_type(119)%message = &
         'CPOST: Illegal value for fcool'
    error_type(120)%level = ERROR_SEVERE
    error_type(120)%message = &
         'CPOST: rtop < rmid'
    error_type(121)%level = ERROR_SEVERE
    error_type(121)%message = &
         'CPOST: hmax < ztop'
    error_type(122)%level = ERROR_WARN
    error_type(122)%message = &
         'CPOST: Silly answers because fcool=1.0...'
    error_type(123)%level = ERROR_SEVERE
    error_type(123)%message = &
         'CPOST: R(Z) is negative'
    error_type(124)%level = ERROR_WARN
    error_type(124)%message = &
         'VACUUM: Newton''s method not converging; check fusion power, te'
    error_type(125)%level = ERROR_SEVERE
    error_type(125)%message = &
         'ACC2253: Illegal value for istore'
    error_type(126)%level = ERROR_SEVERE
    error_type(126)%message = &
         'CUDRIV: Illegal value for iefrf'
    error_type(127)%level = ERROR_SEVERE
    error_type(127)%message = &
         'IFE: Illegal value for ifedrv'
    error_type(128)%level = ERROR_SEVERE
    error_type(128)%message = &
         'TBR_BREED: Unknown tritium breeder specified'
    error_type(129)%level = ERROR_SEVERE
    error_type(129)%message = &
         'CULLHY: Normalised LH efficiency < 0; use a different value of iefrf'
    error_type(130)%level = ERROR_SEVERE
    error_type(130)%message = &
         'REPORT_INPUT_ERROR: Error detected in input file - see OUT.DAT for more details'
    error_type(131)%level = ERROR_WARN
    error_type(131)%message = &
         'EQSLV: Non-optimisation solver HYBRD returns with ifail /= 1'
    error_type(132)%level = ERROR_WARN
    error_type(132)%message = &
         'DOOPT: Optimisation solver VMCON returns with ifail /= 1'
    error_type(133)%level = ERROR_WARN
    error_type(133)%message = &
         'EQSLV: High final HYBRD constraint residues'
    error_type(134)%level = ERROR_WARN
    error_type(134)%message = &
         'DOOPT: High final VMCON constraint residues'
    error_type(135)%level = ERROR_INFO
    error_type(135)%message = &
         'OUTPF: CS coil not using maximum current density: further optimisation possible'
    error_type(136)%level = ERROR_WARN
    error_type(136)%message = &
         'BETCOM: Low density... carbon impurity fraction forced to be no more than 0.05'
    !error_type()%level = ERROR_SEVERE
    !error_type()%message = &
    !     ''

    !     12345678901234567890123456789012345678901234567890123456789012345678901234567890

  end subroutine initialise_error_list

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine report_error(error_id)

    !+ad_name  report_error
    !+ad_summ  Adds the latest error message to the list already specified
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  error_id : input integer : identifier (error_type element number)
    !+ad_argc                             for the relevant error
    !+ad_desc  This routine should be called if a informational, warning
    !+ad_desc  or error message needs to be flagged.
    !+ad_desc  It uses a linked list (see references) to provide
    !+ad_desc  an audit trail for any such messages during the program
    !+ad_desc  execution.
    !+ad_desc  <P>Up to eight integer and eight floating-point diagnostic
    !+ad_desc  values may be saved by the user in arrays <CODE>idiags</CODE> and
    !+ad_desc  <CODE>fdiags</CODE>, respectively, for debugging; these arrays must
    !+ad_desc  be assigned with up to eight values each prior to calling this routine.
    !+ad_desc  <P>The <CODE>error_status</CODE> variable returns the highest severity
    !+ad_desc  level that has been encountered; if a severe error is flagged
    !+ad_desc  (level 3) the program is terminated immediately.
    !+ad_prob  None
    !+ad_call  show_errors
    !+ad_hist  24/06/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Introduction to Fortran 90/95, Stephen J, Chapman, pp.467-472,
    !+ad_docc    McGraw-Hill, ISBN 0-07-115896-0
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: error_id

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Turn on error handling if a severe error has been encountered

    if (error_type(error_id)%level == ERROR_SEVERE) errors_on = .true.

    !  Error handling is only turned on during an output step, not during
    !  intermediate iteration steps

    if (.not.errors_on) then
       idiags = INT_DEFAULT
       fdiags = FLT_DEFAULT
       return
    end if

    if (.not.associated(error_head)) then
       allocate(error_head)
       error_tail => error_head
    else
       allocate(error_tail%ptr)
       error_tail => error_tail%ptr
    end if

    error_tail%id           = error_id
    error_tail%data%level   = error_type(error_id)%level
    error_tail%data%message = error_type(error_id)%message
    error_tail%data%idiags = idiags ; idiags = INT_DEFAULT
    error_tail%data%fdiags = fdiags ; fdiags = FLT_DEFAULT

    nullify (error_tail%ptr)

    !  Update the overall error status (highest severity level encountered)
    !  and stop the program if a severe error has occurred

    error_status = max(error_status, error_type(error_id)%level)

    if (error_status == ERROR_SEVERE) then
       call show_errors
       write(*,*) 'PROCESS stopping.'
       stop
    end if

  end subroutine report_error

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine show_errors

    !+ad_name  show_errors
    !+ad_summ  Reports all informational/error messages encountered
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine provides a summary audit trail of all the errors
    !+ad_desc  encountered during the program's execution.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  oheadr
    !+ad_call  ovarin
    !+ad_hist  24/06/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Introduction to Fortran 90/95, Stephen J, Chapman, pp.467-472,
    !+ad_docc    McGraw-Hill, ISBN 0-07-115896-0
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    type (error_list_item), pointer :: ptr
    integer :: i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(iotty,'Program Error Report')
    write(iotty,'(a,i1)') 'PROCESS error status flag (error_status) = ',error_status
    call oblnkl(iotty)

    call oheadr(nout,'Program Error Report')
    call ovarin(nout,'PROCESS error status flag','(error_status)',error_status)

    ptr => error_head

    if (.not.associated(ptr)) then
       call ovarin(nout,'Final error identifier','(error_id)',error_id)
       return
    end if

    write(*,*) 'ID  LEVEL  MESSAGE'

    output: do
       if (.not.associated(ptr)) exit output

       error_id = ptr%id
       write(nout,'(i3,t7,i3,t13,a80)') ptr%id,ptr%data%level,ptr%data%message
       write(*,   '(i3,t7,i3,t13,a80)') ptr%id,ptr%data%level,ptr%data%message

       if (any(ptr%data%idiags /= INT_DEFAULT)) then
          write(*,*) 'Integer diagnostic values for this error:'
          do i = 1,8
             if (ptr%data%idiags(i) /= INT_DEFAULT) &
                  write(*,'(i4,a,i14)') i,') ',ptr%data%idiags(i)
          end do
       end if
       if (any(ptr%data%fdiags /= FLT_DEFAULT)) then
          write(*,*) 'Floating point diagnostic values for this error:'
          do i = 1,8
             if (ptr%data%fdiags(i) /= FLT_DEFAULT) &
                  write(*,'(i4,a,1pe14.5)') i,') ',ptr%data%fdiags(i)
          end do
       end if
       write(*,*) ' '

       ptr => ptr%ptr
    end do output

    call ovarin(nout,'Final error identifier','(error_id)',error_id)

  end subroutine show_errors

end module error_handling

