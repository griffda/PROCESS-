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
  !+ad_call  global_variables
  !+ad_call  maths_library
  !+ad_hist  11/01/18 KE  Added new constraint eqn 76, Eich formula for nesep
  !+ad_hist  22/06/18 SIM cdtfleg (itv 24) no longer used
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use global_variables
  use constants
  use maths_library
  use plasmod_variables

  implicit none

  public

  !+ad_vars  ipnvars FIX : total number of variables available for iteration
  integer, parameter :: ipnvars = 170
  !+ad_vars  ipeqns  FIX : number of constraint equations available
  integer, parameter :: ipeqns = 81
  !+ad_vars  ipnfoms FIX : number of available figures of merit
  integer, parameter :: ipnfoms = 19

  integer, parameter :: ipvlam  = ipeqns+2*ipnvars+1
  integer, parameter :: iptnt   = (ipeqns*(3*ipeqns+13))/2
  integer, parameter :: ipvp1   = ipnvars+1

  !+ad_vars  ioptimz /1/ : code operation switch:<UL>
  !+ad_varc           <LI> = -1 for no optimisation, HYBRD only;
  !+ad_varc           <LI> = 0  for HYBRD and VMCON (not recommended);
  !+ad_varc           <LI> = 1  for optimisation, VMCON only</UL>
  integer :: ioptimz = 1

  !+ad_vars  minmax /7/ : switch for figure-of-merit (see lablmm for descriptions)
  !+ad_varc               negative => maximise, positive => minimise
  integer :: minmax = 7
  !+ad_vars  lablmm(ipnfoms) : labels describing figures of merit:<UL>
  character(len=22), dimension(ipnfoms) :: lablmm = (/ &
       !+ad_varc  <LI> ( 1) major radius
       'major radius.         ', &
       !+ad_varc  <LI> ( 2) not used
       'not used.             ', &
       !+ad_varc  <LI> ( 3) neutron wall load
       'neutron wall load.    ', &
       !+ad_varc  <LI> ( 4) P_tf + P_pf
       'P_tf + P_pf.          ', &
       !+ad_varc  <LI> ( 5) fusion gain Q
       'fusion gain.          ', &
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
       !+ad_varc  <LI> (12) hydrogen plant capital cost OBSOLETE
       'H plant capital cost. ', &
       !+ad_varc  <LI> (13) hydrogen production rate OBSOLETE
       'H production rate.    ', &
       !+ad_varc  <LI> (14) pulse length
       'pulse length.         ', &
       !+ad_varc  <LI> (15) plant availability factor (N.B. requires
       !+ad_varc            iavail=1 to be set)
       'plant availability.   ', &
       !+ad_varc  <LI> (16) linear combination of major radius (minimised) and pulse length (maximised)
       !+ad_varc              note: FoM should be minimised only!
       'min R0, max tau_burn. ', &
       !+ad_varc  <LI> (17) net electrical output
       'net electrical output.', &
       !+ad_varc  <LI> (18) Null Figure of Merit
       'Null figure of merit. ',  &
       !+ad_varc  <LI> (19) linear combination of big Q and pulse length (maximised)
       !+ad_varc              note: FoM should be minimised only!</UL>
       'max Q, max t_burn.    ' &
        /)
  !+ad_vars  ncalls : number of function calls during solution
  integer :: ncalls = 0
  !+ad_vars  neqns /0/ : number of equality constraints to be satisfied
  integer :: neqns = 0
  !+ad_vars  nfev1 : number of calls to FCNHYB (HYBRD function caller) made
  integer :: nfev1 = 0
  !+ad_vars  nfev2 : number of calls to FCNVMC1 (VMCON function caller) made
  integer :: nfev2 = 0
  !+ad_vars  nineqns /0/ : number of inequality constraints VMCON must satisfy
  !+ad_varc                (leave at zero for now)
  integer :: nineqns = 0
  !+ad_vars  nvar /16/ : number of iteration variables to use
  integer :: nvar = 16
  !+ad_vars  nviter : number of VMCON iterations performed
  integer :: nviter = 0

  !+ad_vars  icc(ipeqns) /0/ :
  !+ad_varc           array defining which constraint equations to activate
  !+ad_varc           (see lablcc for descriptions)

  ! TODO Check the dictionaries are created correctly.
  ! Issue #491 Default constraints removed.
  integer, dimension(ipeqns) :: icc = 0

  !+ad_vars  active_constraints(ipeqns) : Logical array showing which constraints are active
  logical, dimension(ipeqns) :: active_constraints = .false.

  !+ad_vars  lablcc(ipeqns) : labels describing constraint equations (corresponding itvs)<UL>
  character(len=33), dimension(ipeqns) :: lablcc = (/ &
       !+ad_varc  <LI> ( 1) Beta (consistency equation) (itv 5)
       'Beta consistency                 ', &
       !+ad_varc  <LI> ( 2) Global power balance (consistency equation) (itv 10,1,2,3,4,6,11)
       'Global power balance consistency ', &
       !+ad_varc  <LI> ( 3) Ion power balance DEPRECATED (itv 10,1,2,3,4,6,11)
       'Ion power balance                ', &
       !+ad_varc  <LI> ( 4) Electron power balance DEPRECATED (itv 10,1,2,3,4,6,11)
       'Electron power balance           ', &
       !+ad_varc  <LI> ( 5) Density upper limit (itv 9,1,2,3,4,5,6)
       'Density upper limit              ', &
       !+ad_varc  <LI> ( 6) (Epsilon x beta poloidal) upper limit (itv 8,1,2,3,4,6)
       '(Epsilon x beta-pol) upper limit ', &
       !+ad_varc  <LI> ( 7) Beam ion density (NBI) (consistency equation) (itv 7)
       'Beam ion density consistency     ', &
       !+ad_varc  <LI> ( 8) Neutron wall load upper limit (itv 14,1,2,3,4,6)
       'Neutron wall load upper limit    ', &
       !+ad_varc  <LI> ( 9) Fusion power upper limit (itv 26,1,2,3,4,6)
       'Fusion power upper limit         ', &
       !+ad_varc  <LI> (10) Toroidal field 1/R (consistency equation) (itv 12,1,2,3,13 )
       'Toroidal field 1/R consistency   ', &
       !+ad_varc  <LI> (11) Radial build (consistency equation) (itv 3,1,13,16,29,42,61)
       'Radial build consistency         ', &
       !+ad_varc  <LI> (12) Volt second lower limit (STEADY STATE) (itv 15,1,2,3)
       'Volt second lower limit          ', &
       !+ad_varc  <LI> (13) Burn time lower limit (PULSE) (itv 21,1,16,17,29,42,44,61)
       'Burn time lower limit            ', &
       !+ad_varc  <LI> (14) Neutral beam decay lengths to plasma centre (NBI) (consistency equation)
       !+ac_varc            (itv 19,1,2,3,6)
       'NBI decay lengths consistency    ', &
       !+ad_varc  <LI> (15) LH power threshold limit (itv 103)
       'L-H power threshold limit        ', &
       !+ad_varc  <LI> (16) Net electric power lower limit (itv 25,1,2,3)
       'Net electric power lower limit   ', &
       !+ad_varc  <LI> (17) Radiation fraction upper limit (itv 28)
       'Radiation fraction upper limit   ', &
       !+ad_varc  <LI> (18) Divertor heat load upper limit (itv 27)
       'Divertor heat load upper limit   ', &
       !+ad_varc  <LI> (19) MVA upper limit (itv 30)
       'MVA upper limit                  ', &
       !+ad_varc  <LI> (20) Neutral beam tangency radius upper limit (NBI) (itv 33,31,3,13)
       'Beam tangency radius upper limit ', &
       !+ad_varc  <LI> (21) Plasma minor radius lower limit (itv 32)
       'Plasma minor radius lower limit  ', &
       !+ad_varc  <LI> (22) Divertor collisionality upper limit (itv 34,43)
       'Divertor collisionality upper lim', &
       !+ad_varc  <LI> (23) Conducting shell to plasma minor radius ratio upper limit
       !+ad_varc            (itv 104,1,74)
       'Conducting shell radius upper lim', &
       !+ad_varc  <LI> (24) Beta upper limit (itv 36,1,2,3,4,6,18)
       'Beta upper limit                 ', &
       !+ad_varc  <LI> (25) Peak toroidal field upper limit (itv 35,3,13,29)
       'Peak toroidal field upper limit  ', &
       !+ad_varc  <LI> (26) Central solenoid EOF current density upper limit (ipfres=0)
       !+ad_varc            (itv 38,37,41,12)
       'CS coil EOF current density limit', &
       !+ad_varc  <LI> (27) Central solenoid BOP current density upper limit (ipfres=0)
       !+ad_varc            (itv 39,37,41,12)
       'CS coil BOP current density limit', &
       !+ad_varc  <LI> (28) Fusion gain Q lower limit (itv 45,47,40)
       'Fusion gain Q lower limit        ', &
       !+ad_varc  <LI> (29) Inboard radial build consistency (itv 3,1,13,16,29,42,61)
       'Inboard radial build consistency ', &
       !+ad_varc  <LI> (30) Injection power upper limit (itv 46,47,11)
       'Injection power upper limit      ', &
       !+ad_varc  <LI> (31) TF coil case stress upper limit (SCTF) (itv 48,56,57,58,59,60,24)
       'TF coil case stress upper limit  ', &
       !+ad_varc  <LI> (32) TF coil conduit stress upper limit (SCTF) (itv 49,56,57,58,59,60,24)
       'TF coil conduit stress upper lim ', &
       !+ad_varc  <LI> (33) I_op / I_critical (TF coil) (SCTF) (itv 50,56,57,58,59,60,24)
       'I_op / I_critical (TF coil)      ', &
       !+ad_varc  <LI> (34) Dump voltage upper limit (SCTF) (itv 51,52,56,57,58,59,60,24)
       'Dump voltage upper limit         ', &
       !+ad_varc  <LI> (35) J_winding pack/J_protection upper limit (SCTF) (itv 53,56,57,58,59,60,24)
       'J_winding pack/J_protection limit', &
       !+ad_varc  <LI> (36) TF coil temperature margin lower limit (SCTF) (itv 54,55,56,57,58,59,60,24)
       'TF coil temp. margin lower limit ', &
       !+ad_varc  <LI> (37) Current drive gamma upper limit (itv 40,47)
       'Current drive gamma limit        ', &
       !+ad_varc  <LI> (38) First wall coolant temperature rise upper limit (itv 62)
       '1st wall coolant temp rise limit ', &
       !+ad_varc  <LI> (39) First wall peak temperature upper limit (itv 63)
       'First wall peak temperature limit', &
       !+ad_varc  <LI> (40) Start-up injection power lower limit (PULSE) (itv 64)
       'Start-up inj. power lower limit  ', &
       !+ad_varc  <LI> (41) Plasma current ramp-up time lower limit (PULSE) (itv  66,65)
       'Plasma curr. ramp time lower lim ', &
       !+ad_varc  <LI> (42) Cycle time lower limit (PULSE) (itv 67,65,17)
       'Cycle time lower limit           ', &
       !+ad_varc  <LI> (43) Average centrepost temperature
       !+ad_varc            (TART) (consistency equation) (itv 69,70,13)
       'Average centrepost temperature   ', &
       !+ad_varc  <LI> (44) Peak centrepost temperature upper limit (TART) (itv 68,69,70)
       'Peak centrepost temp. upper limit', &
       !+ad_varc  <LI> (45) Edge safety factor lower limit (TART) (itv 71,1,2,3)
       'Edge safety factor lower limit   ', &
       !+ad_varc  <LI> (46) Ip/Irod upper limit (TART) (itv 72,2,60)
       'Ip/Irod upper limit              ', &
       !+ad_varc  <LI> (47) NOT USED
       'TF coil tor. thickness upper lim ', &
       !+ad_varc  <LI> (48) Poloidal beta upper limit (itv 79,2,3,18)
       'Poloidal beta upper limit        ', &
       !+ad_varc  <LI> (49) NOT USED
       'RFP reversal parameter < 0       ', &
       !+ad_varc  <LI> (50) NOT USED
       'IFE repetition rate upper limit  ', &
       !+ad_varc  <LI> (51) Startup volt-seconds consistency (PULSE) (itv 16,29,3,1)
       'Startup volt-seconds consistency ', &
       !+ad_varc  <LI> (52) Tritium breeding ratio lower limit (itv 89,90,91)
       'Tritium breeding ratio lower lim ', &
       !+ad_varc  <LI> (53) Neutron fluence on TF coil upper limit (itv 92,93,94)
       'Neutron fluence on TF coil limit ', &
       !+ad_varc  <LI> (54) Peak TF coil nuclear heating upper limit (itv 95,93,94)
       'Peak TF coil nucl. heating limit ', &
       !+ad_varc  <LI> (55) Vacuum vessel helium concentration upper limit iblanket =2 (itv 96,93,94)
       'Vessel helium concentration limit', &
       !+ad_varc  <LI> (56) Pseparatrix/Rmajor upper limit (itv 97,1,3,102)
       'Psep / R upper limit             ', &
       !+ad_varc  <LI> (57) NOT USED
       'TF coil leg tor width lower limit', &
       !+ad_varc  <LI> (58) NOT USED
       'TF coil leg rad width lower limit', &
       !+ad_varc  <LI> (59) Neutral beam shine-through fraction upper limit (NBI) (itv 105,6,19,4 )
       'NB shine-through frac upper limit', &
       !+ad_varc  <LI> (60) Central solenoid temperature margin lower limit (SCTF) (itv 106)
       'CS temperature margin lower limit', &
       !+ad_varc  <LI> (61) Minimum availability value (itv 107)
       'Minimum availability value       ',  &
       !+ad_varc  <LI> (62) taup/taueff the ratio of particle to energy confinement times (itv 110)
       'taup/taueff                      ', &
       !+ad_varc  <LI> (63) The number of ITER-like vacuum pumps niterpump < tfno (itv 111)
       'number of ITER-like vacuum pumps ',  &
       !+ad_varc  <LI> (64) Zeff less than or equal to zeffmax (itv 112)
       'Zeff limit                       ',  &
       !+ad_varc  <LI> (65) Dump time set by VV loads (itv 56, 113)
       'Dump time set by VV stress       ',   &
       !+ad_varc  <LI> (66) Limit on rate of change of energy in poloidal field
       !+ad_varc            (Use iteration variable 65(tohs), 115)
       'Rate of change of energy in field',   &
       !+ad_varc  <LI> (67) Simple Radiation Wall load limit (itv 116, 102, 4,6)
       'Upper Lim. on Radiation Wall load',   &
       !+ad_varc  <LI> (68) Psep * Bt / qAR upper limit (itv 117)
       'Upper Lim. on Psep * Bt / q A R  ',   &
       !+ad_varc  <LI> (69) ensure separatrix power = the value from Kallenbach divertor (itv 118)
       'pdivt < psep_kallenbach divertor ',   &
       !+ad_varc  <LI> (70) ensure that teomp = separatrix temperature in the pedestal profile,
       !+ad_varc            (itv 119 (tesep))
       'Separatrix temp consistency      ',   &
       !+ad_varc  <LI> (71) ensure that neomp = separatrix density (nesep) x neratio
       'Separatrix density consistency   ',    &
       !+ad_varc  <LI> (72) central solenoid Tresca stress limit (itv 123 foh_stress)
       'CS Tresca stress limit           ',    &
       !+ad_varc  <LI> (73) Psep >= Plh + Paux (itv 137 (fplhsep))
       'Psep >= Plh + Paux               ',   &
       !+ad_varc  <LI> (74) TFC quench < tmax_croco (itv 141 (fcqt))
       'TFC quench < tmax_croco          ',    &
       !+ad_varc  <LI> (75) TFC current/copper area < Maximum (itv 143 f_copperA_m2)
       'TFC current/copper area < Max    ',    &
       !+ad_varc  <LI> (76) Eich critical separatrix density
       'Eich critical separatrix density ',   &
       !+ad_varc  <LI> (77) TF coil current per turn upper limit
       'TFC current per turn upper limit ',    &
       !+ad_varc  <LI> (78) Reinke criterion impurity fraction lower limit (itv  147 freinke)
       'Reinke criterion fZ lower limit  ',   &
       !+ad_varc  <LI> (79) F-value for max peak CS field (itv  149 fbmaxcs)
       'Peak CS field upper limit        ',   &
       !+ad_varc  <LI> (80) F-value for min pdivt (itv  153 fpdivlim)
       'pdivt lower limit                ',   &
       !+ad_varc  <LI> (81) F-value for ne(0) > ne(ped) constraint (itv  154 fne0)</UL>
       'ne0 > neped                      '    &
       /)
       ! Please note: All strings between '...' above must be exactly 33 chars long
       ! Each line of code has a comma before the ampersand, except the last one.
       ! The last ad_varc line ends with the html tag "</UL>".

  ! Issue #495.  Remove default iteration variables
  !+ad_vars  ixc(ipnvars) /0/ :
  !+ad_varc               array defining which iteration variables to activate
  !+ad_varc               (see lablxc for descriptions)
  integer, dimension(ipnvars) :: ixc = 0
  
  ! WARNING These labels are used as variable names by write_new_in_dat.py, and possibly
  ! other python utilities, so they cannot easily be changed.
   character(len=14), dimension(ipnvars) :: lablxc = ''
   ! Issue 287 iteration variables are now defined in module define_iteration_variables in iteration variables.f90

  character(len=14), dimension(:), allocatable :: name_xc

  !+ad_vars  sqsumsq : sqrt of the sum of the square of the constraint residuals
  real(kind(1.0D0)) :: sqsumsq = 0.0D0
  !+ad_vars  epsfcn /1.0e-3/ : finite difference step length for HYBRD/VMCON derivatives
  real(kind(1.0D0)) :: epsfcn = 1.0D-3
  !+ad_vars  epsvmc /1.0e-6/ : error tolerance for VMCON
  real(kind(1.0D0)) :: epsvmc = 1.0D-6
  !+ad_vars  factor /0.1/ : used in HYBRD for first step size
  real(kind(1.0D0)) :: factor = 0.1D0
  !+ad_vars  ftol /1.0e-4/ : error tolerance for HYBRD
  real(kind(1.0D0)) :: ftol = 1.0D-4

  !+ad_vars  boundl(ipnvars) /../ : lower bounds used on ixc variables during
  !+ad_varc                         VMCON optimisation runs
  real(kind(1.0D0)), dimension(ipnvars) :: boundl = 9.d-99

  ! !+ad_vars  boundu(ipnvars) /../ : upper bounds used on ixc variables 
  ! Issue #287 These bounds now defined in initial.f90
  real(kind(1.0D0)), dimension(ipnvars) :: boundu = 9.d99

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
    !+ad_summ  Find the non-optimising HYBRD solution to the problem
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
    !+ad_hist  27/02/14 PJK Corrected usage of m, meq in case of inequalities
    !+ad_hist  08/07/14 PJK Added attempt to fix problems if VMCON exits with ifail=5
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

    ! Array defined for optimizer data output only
    integer, dimension(nvar)          :: ixc_opt_out
    integer, dimension(neqns+nineqns) :: icc_opt_out

    real(kind(1.0D0)), parameter :: zero = 0.0D0
    real(kind(1.0D0)), parameter :: bfactor = 2.0D0
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
    m = neqns + nineqns
    meq = neqns
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

    ! Write the VMCON setup in OPT.DAT
    do ii = 1, m
      icc_opt_out(ii) = icc(ii)
    end do
    do ii = 1, n
      ixc_opt_out(ii) = ixc(ii)
    end do
    
    write(opt_file, *) ' number of constrains'
    write(opt_file, '(I4)') m
    write(opt_file, *) ' '
    write(opt_file, *) ' Constrains selection'
    write(opt_file, '(I3,*(I4))') icc_opt_out
    write(opt_file, *) ' '
    write(opt_file, *) ' number of variables'
    write(opt_file, '(I4)') n
    write(opt_file, *) ' '
    write(opt_file, *) ' Variables selection'    
    write(opt_file, '(I3,*(I4))') ixc_opt_out
    write(opt_file, *) ' '
    write(opt_file, *) ' '
    write(opt_file, *) ' n VMCOM iter | Figure of merit | VMCON conv      | constrains quad sum |   residual,   input values &
                    &and  FoM input gradients'
    write(opt_file, '(A,*(I18))') '  niter          abs(objf)         sum                sqsumsq ', icc_opt_out, ixc_opt_out&
                    &, ixc_opt_out
 
    call vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
         lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
         gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
         liwa,ilower,iupper,bndl,bndu,convergence_parameter)

    write(*,*) ""

    ! If fail then alter value of epsfcn - this can be improved
    if (ifail /= 1) then
       write(*,*) 'Trying again with new epsfcn'
       epsfcn = epsfcn * 10.0D0 !try new larger value
       write(*,*) 'new epsfcn = ', epsfcn
       call vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
            lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
            gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
            liwa,ilower,iupper,bndl,bndu,convergence_parameter)
       epsfcn = epsfcn / 10.0D0 !reset value
    end if
    if (ifail /= 1) then
       write(*,*) 'Trying again with new epsfcn'
       epsfcn = epsfcn / 10.0D0 !try new smaller value
       write(*,*) 'new epsfcn = ', epsfcn
       call vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
            lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
            gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
            liwa,ilower,iupper,bndl,bndu,convergence_parameter)
       epsfcn = epsfcn * 10.0D0 !reset value
    end if


    !  If VMCON has exited with error code 5 try another run using a multiple of
    !  the identity matrix as input for the Hessian b(n,n).
    !  Only do this if VMCON has not iterated (nviter=1).

    if ((ifail == 5).and.(nviter < 2)) then
       mode = 1
       b(:,:) = zero
       do ii = 1, n
          b(ii,ii) = bfactor
          xv(ii) = xcm(ii)      !  Re-initialise iteration values
       end do
       ! if (verbose == 1) then
       write(*,*) 'VMCON error code = 5.  Rerunning VMCON with a new initial estimate of the second derivative matrix.'
       ! end if

       call vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
            lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
            gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
            liwa,ilower,iupper,bndl,bndu,convergence_parameter)
    end if

    do ii = 1,n
       xcm(ii) = xv(ii)
    end do

    do ii = 1,m
       rcm(ii) = conf(ii)
    end do

  end subroutine optimiz

end module numerics
