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
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use maths_library

  implicit none

  public

  integer, parameter :: ipnvars = 88  !  total number of variables available for iteration
  integer, parameter :: ipeqns  = 50  !  number of constraint equations available
  integer, parameter :: ipnfoms = 13  !  number of available figures of merit
  integer, parameter :: ipvlam  = ipeqns+2*ipnvars+1
  integer, parameter :: iptnt   = (ipeqns*(3*ipeqns+13))/2
  integer, parameter :: ipvp1   = ipnvars+1

  integer :: ioptimz = 1
  integer :: maxcal  = 200
  integer :: minmax  = 6
  integer :: ncalls  = 0
  integer :: neqns   = 14
  integer :: nfev1   = 0
  integer :: nfev2   = 0
  integer :: nineqns = 0
  integer :: nvar    = 25
  integer :: nvrbl   = 0

  integer, dimension(ipeqns) :: icc = 0
  integer, dimension(ipnvars) :: ixc = 0

  character(len=34), dimension(ipeqns) :: lablcc = ''
  character(len=22), dimension(ipnfoms) :: lablmm = ''
  character(len=8), dimension(ipnvars) :: lablxc = ''

  character(len=48) :: icase = 'PROCESS standard D-T tokamak model'

  real(kind(1.0D0)) :: sqsumsq = 0.0D0
  real(kind(1.0D0)) :: epsfcn = 1.0D-3
  real(kind(1.0D0)) :: epsvmc = 1.0D-3
  real(kind(1.0D0)) :: factor = 0.1D0
  real(kind(1.0D0)) :: ftol = 1.0D-4

  real(kind(1.0D0)), dimension(ipnvars) :: bondl = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: bondu = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: boundl = 1.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: boundu = 1.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: rcm = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: resdl = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: scafc = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: scale = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: xcm = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: xcs = 0.0D0
  real(kind(1.0D0)), dimension(ipvlam) :: vlam = 0.0D0

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
