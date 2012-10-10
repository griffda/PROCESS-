!  $Id::                                                                $
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
  !+ad_call  maths_library
  !+ad_call  fcnhyb
  !+ad_call  hybrd
  !+ad_hist  27/07/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use maths_library

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

subroutine fcnhyb(n,xc,rc,iflag)

  !+ad_name  fcnhyb
  !+ad_summ  Function evaluator for EQSOLV
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  n : input integer : Number of equations and unknowns
  !+ad_args  xc(n) : input/output real array : On input XC must contain
  !+ad_argc  an initial estimate of the solution vector. On output XC
  !+ad_argc  contains the final estimate of the solution vector.
  !+ad_args  rc(n) : output real array : Functions evaluated at the output XC
  !+ad_args  iflag : input/output integer : Terminate execution of EQSOLV
  !+ad_argc                                 by setting IFLAG to a negative integer.
  !+ad_desc  This subroutine is the function evaluator for
  !+ad_desc  <A HREF="eqsolv.html">EQSOLV</A> (q.v.).
  !+ad_prob  None
  !+ad_call  numer.h90
  !+ad_call  param.h90
  !+ad_call  caller
  !+ad_call  con1
  !+ad_hist  27/07/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'

  !  Arguments

  integer, intent(in) :: n
  real(kind(1.0D0)), dimension(n), intent(inout) :: xc
  real(kind(1.0D0)), dimension(n), intent(out) :: rc
  integer, intent(inout) :: iflag

  !  Local variables

  integer :: ncon, nvars

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  nvars = neqns
  ncon = neqns

  call caller(xc,nvars)
  call con1(ncon,rc)

  !  Set iflag < 0 if program is to be terminated here.

  iflag = 1 * iflag

end subroutine fcnhyb
