! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module numerics
  !! Module containing callers to the main equation solvers
  !! HYBRD and VMCON
  !! author: P J Knight, CCFE, Culham Science Centre
  !! This module contains the primary numerics variables and the
  !! calling routines for the two equation solvers in the code.
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  public

  integer, parameter :: ipnvars = 174
  !!  ipnvars FIX : total number of variables available for iteration
  integer, parameter :: ipeqns = 84
  !!  ipeqns  FIX : number of constraint equations available
  integer, parameter :: ipnfoms = 19
  !!  ipnfoms FIX : number of available figures of merit

  integer, parameter :: ipvlam  = ipeqns+2*ipnvars+1
  integer, parameter :: iptnt   = (ipeqns*(3*ipeqns+13))/2
  integer, parameter :: ipvp1   = ipnvars+1

  integer :: ioptimz = 1
  !!  ioptimz /1/ : code operation switch:<UL>
  !!           <LI> = -2 for no optimisation, no VMCOM or HYBRD;
  !!           <LI> = -1 for no optimisation, HYBRD only;
  !!           <LI> = 0  for HYBRD and VMCON (not recommended);
  !!           <LI> = 1  for optimisation, VMCON only</UL>

  !!  minmax /7/ : switch for figure-of-merit (see lablmm for descriptions)
  !!               negative => maximise, positive => minimise
  integer :: minmax = 7
  character(len=22), dimension(ipnfoms) :: lablmm = (/ &
       'major radius.         ', &
       'not used.             ', &
       'neutron wall load.    ', &
       'P_tf + P_pf.          ', &
       'fusion gain.          ', &
       'cost of electricity.  ', &
       'capital cost.         ', &
       'aspect ratio.         ', &
       'divertor heat load.   ', &
       'toroidal field.       ', &
       'total injected power. ', &
       'H plant capital cost. ', &
       'H production rate.    ', &
       'pulse length.         ', &
       'plant availability.   ', &
       'min R0, max tau_burn. ', &
       'net electrical output.', &
       'Null figure of merit. ',  &
       'max Q, max t_burn.    ' &
       /)
  !!  lablmm(ipnfoms) : labels describing figures of merit:<UL>
  !!  <LI> ( 1) major radius
  !!  <LI> ( 2) not used
  !!  <LI> ( 3) neutron wall load
  !!  <LI> ( 4) P_tf + P_pf
  !!  <LI> ( 5) fusion gain Q
  !!  <LI> ( 6) cost of electricity
  !!  <LI> ( 7) capital cost (direct cost if ireactor=0,
  !!                          constructed cost otherwise)
  !!  <LI> ( 8) aspect ratio
  !!  <LI> ( 9) divertor heat load
  !!  <LI> (10) toroidal field
  !!  <LI> (11) total injected power
  !!  <LI> (12) hydrogen plant capital cost OBSOLETE
  !!  <LI> (13) hydrogen production rate OBSOLETE
  !!  <LI> (14) pulse length
  !!  <LI> (15) plant availability factor (N.B. requires
  !!            iavail=1 to be set)
  !!  <LI> (16) linear combination of major radius (minimised) and pulse length (maximised)
  !!              note: FoM should be minimised only!
  !!  <LI> (17) net electrical output
  !!  <LI> (18) Null Figure of Merit
  !!  <LI> (19) linear combination of big Q and pulse length (maximised)
  !!              note: FoM should be minimised only!</UL>

  integer :: ncalls = 0
  !!  ncalls : number of function calls during solution
  integer :: neqns = 0
  !!  neqns /0/ : number of equality constraints to be satisfied
  integer :: nfev1 = 0
  !!  nfev1 : number of calls to FCNHYB (HYBRD function caller) made
  integer :: nfev2 = 0
  !!  nfev2 : number of calls to FCNVMC1 (VMCON function caller) made
  integer :: nineqns = 0
  !!  nineqns /0/ : number of inequality constraints VMCON must satisfy
  !!                (leave at zero for now)
  integer :: nvar = 16
  !!  nvar /16/ : number of iteration variables to use
  integer :: nviter = 0
  !!  nviter : number of VMCON iterations performed

  !!  icc(ipeqns) /0/ :
  !!           array defining which constraint equations to activate
  !!           (see lablcc for descriptions)

  ! TODO Check the dictionaries are created correctly.
  ! Issue #491 Default constraints removed.
  integer, dimension(ipeqns) :: icc = 0

  logical, dimension(ipeqns) :: active_constraints = .false.
  !!  active_constraints(ipeqns) : Logical array showing which constraints are active

  character(len=33), dimension(ipeqns) :: lablcc = (/ &
       'Beta consistency                 ', &
       'Global power balance consistency ', &
       'Ion power balance                ', &
       'Electron power balance           ', &
       'Density upper limit              ', &
       '(Epsilon x beta-pol) upper limit ', &
       'Beam ion density consistency     ', &
       'Neutron wall load upper limit    ', &
       'Fusion power upper limit         ', &
       'Toroidal field 1/R consistency   ', &
       'Radial build consistency         ', &
       'Volt second lower limit          ', &
       'Burn time lower limit            ', &
       'NBI decay lengths consistency    ', &
       'L-H power threshold limit        ', &
       'Net electric power lower limit   ', &
       'Radiation fraction upper limit   ', &
       'Divertor heat load upper limit   ', &
       'MVA upper limit                  ', &
       'Beam tangency radius upper limit ', &
       'Plasma minor radius lower limit  ', &
       'Divertor collisionality upper lim', &
       'Conducting shell radius upper lim', &
       'Beta upper limit                 ', &
       'Peak toroidal field upper limit  ', &
       'CS coil EOF current density limit', &
       'CS coil BOP current density limit', &
       'Fusion gain Q lower limit        ', &
       'Inboard radial build consistency ', &
       'Injection power upper limit      ', &
       'TF coil case stress upper limit  ', &
       'TF coil conduit stress upper lim ', &
       'I_op / I_critical (TF coil)      ', &
       'Dump voltage upper limit         ', &
       'J_winding pack/J_protection limit', &
       'TF coil temp. margin lower limit ', &
       'Current drive gamma limit        ', &
       '1st wall coolant temp rise limit ', &
       'First wall peak temperature limit', &
       'Start-up inj. power lower limit  ', &
       'Plasma curr. ramp time lower lim ', &
       'Cycle time lower limit           ', &
       'Average centrepost temperature   ', &
       'Peak centrepost temp. upper limit', &
       'Edge safety factor lower limit   ', &
       'Ip/Irod upper limit              ', &
       'TF coil tor. thickness upper lim ', &
       'Poloidal beta upper limit        ', &
       'RFP reversal parameter < 0       ', &
       'IFE repetition rate upper limit  ', &
       'Startup volt-seconds consistency ', &
       'Tritium breeding ratio lower lim ', &
       'Neutron fluence on TF coil limit ', &
       'Peak TF coil nucl. heating limit ', &
       'Vessel helium concentration limit', &
       'Psep / R upper limit             ', &
       'TF coil leg rad width lower limit', &
       'TF coil leg rad width lower limit', &
       'NB shine-through frac upper limit', &
       'CS temperature margin lower limit', &
       'Minimum availability value       ',  &
       'taup/taueff                      ', &
       'number of ITER-like vacuum pumps ',  &
       'Zeff limit                       ',  &
       'Dump time set by VV stress       ',   &
       'Rate of change of energy in field',   &
       'Upper Lim. on Radiation Wall load',   &
       'Upper Lim. on Psep * Bt / q A R  ',   &
       'pdivt < psep_kallenbach divertor ',   &
       'Separatrix temp consistency      ',   &
       'Separatrix density consistency   ',    &
       'CS Tresca stress limit           ',    &
       'Psep >= Plh + Paux               ',   &
       'TFC quench < tmax_croco          ',    &
       'TFC current/copper area < Max    ',    &
       'Eich critical separatrix density ',   &
       'TFC current per turn upper limit ',    &
       'Reinke criterion fZ lower limit  ',   &
       'Peak CS field upper limit        ',   &
       'pdivt lower limit                ',   &
       'ne0 > neped                      ',   &
       'toroidalgap >  tftort            ',   &
       'available_space > required_space ',   &
       'beta > betalim_lower             '    &
       /)
  !!  lablcc(ipeqns) : labels describing constraint equations (corresponding itvs)<UL>
  !!  <LI> ( 1) Beta (consistency equation) (itv 5)
  !!  <LI> ( 2) Global power balance (consistency equation) (itv 10,1,2,3,4,6,11)
  !!  <LI> ( 3) Ion power balance DEPRECATED (itv 10,1,2,3,4,6,11)
  !!  <LI> ( 4) Electron power balance DEPRECATED (itv 10,1,2,3,4,6,11)
  !!  <LI> ( 5) Density upper limit (itv 9,1,2,3,4,5,6)
  !!  <LI> ( 6) (Epsilon x beta poloidal) upper limit (itv 8,1,2,3,4,6)
  !!  <LI> ( 7) Beam ion density (NBI) (consistency equation) (itv 7)
  !!  <LI> ( 8) Neutron wall load upper limit (itv 14,1,2,3,4,6)
  !!  <LI> ( 9) Fusion power upper limit (itv 26,1,2,3,4,6)
  !!  <LI> (10) Toroidal field 1/R (consistency equation) (itv 12,1,2,3,13 )
  !!  <LI> (11) Radial build (consistency equation) (itv 3,1,13,16,29,42,61)
  !!  <LI> (12) Volt second lower limit (STEADY STATE) (itv 15,1,2,3)
  !!  <LI> (13) Burn time lower limit (PULSE) (itv 21,1,16,17,29,42,44,61)
  !!            (itv 19,1,2,3,6)
  !!  <LI> (14) Neutral beam decay lengths to plasma centre (NBI) (consistency equation)
  !!  <LI> (15) LH power threshold limit (itv 103)
  !!  <LI> (16) Net electric power lower limit (itv 25,1,2,3)
  !!  <LI> (17) Radiation fraction upper limit (itv 28)
  !!  <LI> (18) Divertor heat load upper limit (itv 27)
  !!  <LI> (19) MVA upper limit (itv 30)
  !!  <LI> (20) Neutral beam tangency radius upper limit (NBI) (itv 33,31,3,13)
  !!  <LI> (21) Plasma minor radius lower limit (itv 32)
  !!  <LI> (22) Divertor collisionality upper limit (itv 34,43)
  !!  <LI> (23) Conducting shell to plasma minor radius ratio upper limit
  !!            (itv 104,1,74)
  !!  <LI> (24) Beta upper limit (itv 36,1,2,3,4,6,18)
  !!  <LI> (25) Peak toroidal field upper limit (itv 35,3,13,29)
  !!  <LI> (26) Central solenoid EOF current density upper limit (ipfres=0)
  !!            (itv 38,37,41,12)
  !!  <LI> (27) Central solenoid BOP current density upper limit (ipfres=0)
  !!            (itv 39,37,41,12)
  !!  <LI> (28) Fusion gain Q lower limit (itv 45,47,40)
  !!  <LI> (29) Inboard radial build consistency (itv 3,1,13,16,29,42,61)
  !!  <LI> (30) Injection power upper limit (itv 46,47,11)
  !!  <LI> (31) TF coil case stress upper limit (SCTF) (itv 48,56,57,58,59,60,24)
  !!  <LI> (32) TF coil conduit stress upper limit (SCTF) (itv 49,56,57,58,59,60,24)
  !!  <LI> (33) I_op / I_critical (TF coil) (SCTF) (itv 50,56,57,58,59,60,24)
  !!  <LI> (34) Dump voltage upper limit (SCTF) (itv 51,52,56,57,58,59,60,24)
  !!  <LI> (35) J_winding pack/J_protection upper limit (SCTF) (itv 53,56,57,58,59,60,24)
  !!  <LI> (36) TF coil temperature margin lower limit (SCTF) (itv 54,55,56,57,58,59,60,24)
  !!  <LI> (37) Current drive gamma upper limit (itv 40,47)
  !!  <LI> (38) First wall coolant temperature rise upper limit (itv 62)
  !!  <LI> (39) First wall peak temperature upper limit (itv 63)
  !!  <LI> (40) Start-up injection power lower limit (PULSE) (itv 64)
  !!  <LI> (41) Plasma current ramp-up time lower limit (PULSE) (itv  66,65)
  !!  <LI> (42) Cycle time lower limit (PULSE) (itv 17,67,65)
  !!  <LI> (43) Average centrepost temperature
  !!            (TART) (consistency equation) (itv 13,20,69,70)
  !!  <LI> (44) Peak centrepost temperature upper limit (TART) (itv 68,69,70)
  !!  <LI> (45) Edge safety factor lower limit (TART) (itv 71,1,2,3)
  !!  <LI> (46) Ip/Irod upper limit (TART) (itv 72,2,60)
  !!  <LI> (47) NOT USED
  !!  <LI> (48) Poloidal beta upper limit (itv 79,2,3,18)
  !!  <LI> (49) NOT USED
  !!  <LI> (50) IFE repetition rate upper limit (IFE)
  !!  <LI> (51) Startup volt-seconds consistency (PULSE) (itv 16,29,3,1)
  !!  <LI> (52) Tritium breeding ratio lower limit (itv 89,90,91)
  !!  <LI> (53) Neutron fluence on TF coil upper limit (itv 92,93,94)
  !!  <LI> (54) Peak TF coil nuclear heating upper limit (itv 95,93,94)
  !!  <LI> (55) Vacuum vessel helium concentration upper limit iblanket =2 (itv 96,93,94)
  !!  <LI> (56) Pseparatrix/Rmajor upper limit (itv 97,1,3,102)
  !!  <LI> (57) NOT USED
  !!  <LI> (58) NOT USED
  !!  <LI> (59) Neutral beam shine-through fraction upper limit (NBI) (itv 105,6,19,4 )
  !!  <LI> (60) Central solenoid temperature margin lower limit (SCTF) (itv 106)
  !!  <LI> (61) Minimum availability value (itv 107)
  !!  <LI> (62) taup/taueff the ratio of particle to energy confinement times (itv 110)
  !!  <LI> (63) The number of ITER-like vacuum pumps niterpump < tfno (itv 111)
  !!  <LI> (64) Zeff less than or equal to zeffmax (itv 112)
  !!  <LI> (65) Dump time set by VV loads (itv 56, 113)
  !!  <LI> (66) Limit on rate of change of energy in poloidal field
  !!            (Use iteration variable 65(tohs), 115)
  !!  <LI> (67) Simple Radiation Wall load limit (itv 116, 102, 4,6)
  !!  <LI> (68) Psep * Bt / qAR upper limit (itv 117)
  !!  <LI> (69) ensure separatrix power = the value from Kallenbach divertor (itv 118)
  !!  <LI> (70) ensure that teomp = separatrix temperature in the pedestal profile,
  !!            (itv 119 (tesep))
  !!  <LI> (71) ensure that neomp = separatrix density (nesep) x neratio
  !!  <LI> (72) central solenoid Tresca stress limit (itv 123 foh_stress)
  !!  <LI> (73) Psep >= Plh + Paux (itv 137 (fplhsep))
  !!  <LI> (74) TFC quench < tmax_croco (itv 141 (fcqt))
  !!  <LI> (75) TFC current/copper area < Maximum (itv 143 f_coppera_m2)
  !!  <LI> (76) Eich critical separatrix density
  !!  <LI> (77) TF coil current per turn upper limit
  !!  <LI> (78) Reinke criterion impurity fraction lower limit (itv  147 freinke)
  !!  <LI> (79) Peak CS field upper limit (itv  149 fbmaxcs)
  !!  <LI> (80) Divertor power lower limit pdivt (itv  153 fpdivlim)
  !!  <LI> (81) Ne(0) > ne(ped) constraint (itv  154 fne0)</UL>
  !!  <LI> (82) toroidalgap >  tftort constraint (itv  171 ftoroidalgap)</UL>
  !!  <LI> (83) Radial build consistency for stellarators (itv 172 f_avspace)
  !!  <LI> (84) Lower limit for beta (itv 173 fbetatry_lower)

       ! Please note: All strings between '...' above must be exactly 33 chars long
       ! Each line of code has a comma before the ampersand, except the last one.
       ! The last ad_varc line ends with the html tag "</UL>".

  ! Issue #495.  Remove default iteration variables
  !!  ixc(ipnvars) /0/ :
  !!               array defining which iteration variables to activate
  !!               (see lablxc for descriptions)
  integer, dimension(ipnvars) :: ixc = 0
  
  ! WARNING These labels are used as variable names by write_new_in_dat.py, and possibly
  ! other python utilities, so they cannot easily be changed.
  character(len=14), dimension(ipnvars) :: lablxc = ''
  ! Issue 287 iteration variables are now defined in module define_iteration_variables in iteration variables.f90

  character(len=14), dimension(:), allocatable :: name_xc

  real(dp) :: sqsumsq = 0.0D0
  !!  sqsumsq : sqrt of the sum of the square of the constraint residuals
  real(dp) :: epsfcn = 1.0D-3
  !!  epsfcn /1.0e-3/ : finite difference step length for HYBRD/VMCON derivatives
  real(dp) :: epsvmc = 1.0D-6
  !!  epsvmc /1.0e-6/ : error tolerance for VMCON
  real(dp) :: factor = 0.1D0
  !!  factor /0.1/ : used in HYBRD for first step size
  real(dp) :: ftol = 1.0D-4
  !!  ftol /1.0e-4/ : error tolerance for HYBRD

  real(dp), dimension(ipnvars) :: boundl = 9.d-99
  !!  boundl(ipnvars) /../ : lower bounds used on ixc variables during
  !!                         VMCON optimisation runs

  ! Issue #287 These bounds now defined in initial.f90
  real(dp), dimension(ipnvars) :: boundu = 9.d99
  ! !!  boundu(ipnvars) /../ : upper bounds used on ixc variables 

  real(dp), dimension(ipnvars) :: bondl = 0.0D0
  real(dp), dimension(ipnvars) :: bondu = 0.0D0
  real(dp), dimension(ipnvars) :: rcm = 0.0D0
  real(dp), dimension(ipnvars) :: resdl = 0.0D0
  real(dp), dimension(ipnvars) :: scafc = 0.0D0
  real(dp), dimension(ipnvars) :: scale = 0.0D0
  real(dp), dimension(ipnvars) :: xcm = 0.0D0
  real(dp), dimension(ipnvars) :: xcs = 0.0D0
  real(dp), dimension(ipvlam)  :: vlam = 0.0D0

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine eqsolv(fcnhyb,n,x,fvec,tol,epsfcn,factor,nprint,info, &
       wa,lwa,resdl,nfev)

    !! Find the non-optimising HYBRD solution to the problem
    !! author: Argonne National Laboratory. Minpack Project. March 1980.
    !! author: Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !! author: P J Knight, CCFE, Culham Science Centre
    !! fcnhyb : external routine name : see below
    !! n : input integer : number of functions and variables
    !! x(n) : input/output real array : On input X must contain
    !! an initial estimate of the solution vector. On output X
    !! contains the final estimate of the solution vector.
    !! fvec(n) : output real array : Functions evaluated at output X
    !! tol : input real : Termination occurs when the algorithm
    !! estimates that the relative error between X and the solution
    !! is at most TOL.
    !! epsfcn : input real : Used in determining a suitable
    !! step length for the forward-difference approximation
    !! (see <A HREF="hybrd.html">hybrd</A>)
    !! factor : input real : Used in determining the initial step bound
    !! (see <A HREF="hybrd.html">hybrd</A>)
    !! nprint : input integer : Number of iterations between print-outs
    !! info : output integer : If the user has terminated execution,
    !! INFO is set to the (negative) value of IFLAG, see description below.
    !! Otherwise, INFO is set as follows:
    !! <PRE>
    !! INFO = 0   Improper input parameters.
    !! INFO = 1   Algorithm estimates that the relative error
    !! between X and the solution is at most TOL.
    !! INFO = 2   Number of calls to FCNHYB has reached or exceeded
    !! 200*(N+1).
    !! INFO = 3   TOL is too small. No further improvement in
    !! the approximate solution X is possible.
    !! INFO = 4   Iteration is not making good progress.
    !! </PRE>
    !! wa(lwa) : input/output real array : work array
    !! lwa : input integer : work array size, not less than (N*(3*N+13))/2
    !! resdl(n) : output real array : residuals
    !! nfev : output integer : number of iterations performed
    !! Routine EQSOLV is the Argonne Minpack subroutine HYBRD1
    !! which has been modified by D.T. Blackfield FEDC/TRW.
    !! The routine is the same except some of the arguments are
    !! user supplied rather than 'hardwired'.
    !! <P>The purpose of EQSOLV is to find a zero of a system of
    !! N nonlinear functions in N variables by a modification
    !! of the Powell hybrid method. This is done by using the
    !! more general nonlinear equation solver <A HREF="hybrd.html">HYBRD</A>.
    !! The user must provide a subroutine which calculates the functions.
    !! The Jacobian is then calculated by a forward-difference
    !! approximation.
    !! <P>FCNHYB is the name of a user-supplied subroutine which
    !! calculates the functions. FCNHYB must be declared
    !! in an external statement in the user calling
    !! program, and should be written as follows:
    !! <PRE>
    !! subroutine fcnhyb(n,x,fvec,iflag)
    !! integer n,iflag
    !! double precision x(n),fvec(n)
    !! ----------
    !! calculate the functions at x and
    !! return this vector in fvec.
    !! ---------
    !! return
    !! end
    !! </PRE>
    !! The value of iflag should not be changed by FCNHYB unless
    !! the user wants to terminate execution of EQSOLV.
    !! In this case set IFLAG to a negative integer.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use global_variables, only: icase, verbose, vlabel
    use constants, only: mfile, nout
    use maths_library, only: HYBRD
    use plasmod_variables, only: geom, power_losses, i_flag
    implicit none

    ! Interface for the external fcnhyb subroutine argument
    ! This interface is necessary when wrapping with f2py
    interface
      subroutine fcnhyb(n, x, fvec, iflag)
        use, intrinsic :: iso_fortran_env, only: dp=>real64
        integer, intent(in) :: n
        real(dp), dimension(n), intent(inout) :: x
        real(dp), dimension(n), intent(out) :: fvec
        integer, intent(inout) :: iflag
      end subroutine fcnhyb
    end interface
    
    !  Arguments

    external :: fcnhyb
    integer, intent(in) :: n, nprint, lwa
    real(dp), dimension(n), intent(inout) :: x
    real(dp), dimension(n), intent(out) :: fvec, resdl
    real(dp), dimension(lwa), intent(out) :: wa
    real(dp), intent(in) :: tol, epsfcn, factor
    integer, intent(out) :: info, nfev

    !  Local variables

    integer :: n1,indx,lr,maxfev,ml,mode,mu
    real(dp), parameter :: one = 1.0D0
    real(dp), parameter :: zero = 0.0D0
    real(dp) :: xtol

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

    !! Calls the minimisation/maximisation routine VMCON
    !! author: P J Knight, CCFE, Culham Science Centre
    !! fcnvmc1 : external routine : objective function evaluator
    !! fcnvmc2 : external routine : gradient objective function evaluator
    !! ifail   : output integer : error flag
    !! f       : output real    : value of objective function at the output point
    !! This routine calls the minimisation/maximisation routine VMCON,
    !! developed by Argonne National Laboratory.
    !! On exit, the (normalised) value of the variable being maximised
    !! or minimised (i.e. the figure of merit) is returned in argument
    !! <CODE>f</CODE>.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use global_variables, only: maxcal, convergence_parameter, iscan_global, &
      xlabel_2
		use constants, only: mfile, nplot, nout, pi, opt_file
		use maths_library, only: vmcon
		use plasmod_variables, only: plasmod_i_impmodel
    implicit none

    ! Interface for the external fcnvmc1 subroutine argument
    ! This interface is necessary when wrapping with f2py
    interface
      subroutine fcnvmc1(n,m,xv,objf,conf,ifail)
        use, intrinsic :: iso_fortran_env, only: dp=>real64
        integer, intent(in) :: n,m
        real(dp), dimension(n), intent(in) :: xv
        real(dp), intent(out) :: objf
        real(dp), dimension(m), intent(out) :: conf
        integer, intent(inout) :: ifail
      end subroutine fcnvmc1
    end interface

    !  Arguments

    external :: fcnvmc1, fcnvmc2
    integer, intent(out) :: ifail
    real(dp), intent(out) :: f

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

    real(dp), parameter :: zero = 0.0D0
    real(dp), parameter :: bfactor = 2.0D0
    real(dp) :: xtol
    real(dp), dimension(ipnvars) :: bdelta,bndl,bndu,etav,fgrd, &
         gammv,glag,glaga,xa,xv
    real(dp), dimension(ipeqns) :: cm,conf
    real(dp), dimension(ippn1) :: bdl,bdu,gm
    real(dp), dimension(ipvmu) :: vmu
    real(dp), dimension(ipldel) :: delta
    real(dp), dimension(iplh) :: wa
    real(dp), dimension(ippn1,ipeqns) :: cnorm
    real(dp), dimension(ippn1,ippn1) :: b
    real(dp), dimension(iplh,iplh) :: h

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
