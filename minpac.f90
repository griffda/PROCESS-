!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine vmcon( &
     fcnvmc1,fcnvmc2,mode,n,m,meq,x,objf,fgrd,conf,cnorm,lcnorm, &
     b,lb,tol,maxfev,info,nfev,vlam,glag,vmu,cm,glaga,gamma,eta, &
     xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa,liwa,ilower, &
     iupper,bndl,bndu)

  !+ad_name  vmcon
  !+ad_summ  Calculates the least value of a function of several variables
  !+ad_summ  subject to linear and/or nonlinear equality and inequality
  !+ad_summ  constraints
  !+ad_type  Subroutine
  !+ad_auth  R L Crane, K E Hillstrom, M Minkoff, Argonne National Lab
  !+ad_auth  J Galambos, FEDC/ORNL
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  fcnvmc1 : external subroutine : routine to calculate the
  !+ad_argc    objective and constraint functions
  !+ad_args  fcnvmc2 : external subroutine : routine to calculate the
  !+ad_argc    gradients of the objective and constraint functions
  !+ad_args  mode : input integer : 1 if B is provided by the user,
  !+ad_argc                         0 if B is to be initialized to
  !+ad_argc                           the identity matrix
  !+ad_args  n : input integer : number of variables
  !+ad_args  m : input integer : number of constraints
  !+ad_args  meq : input integer : number of equality constraints
  !+ad_args  x(n) : input/output real array : initial/final estimates of
  !+ad_argc    solution vector
  !+ad_args  objf : output real : objective function at output X
  !+ad_args  fgrd(n) : output real array : components of the gradient of
  !+ad_argc    the objective function at output X
  !+ad_args  conf(m) : output real array : values of the constraint
  !+ad_argc    functions at output X (equality constraints must precede
  !+ad_argc    the inequality constraints)
  !+ad_args  cnorm(lcnorm,m) : output real array : constraint normals at
  !+ad_argc    output X
  !+ad_args  lcnorm : input integer : array dimension, >= n+1
  !+ad_args  b(lb,lb) : input/output real array : approximation to the
  !+ad_argc    second derivative matrix of the Lagrangian function.
  !+ad_argc    Often, an adequate initial B matrix can be obtained by
  !+ad_argc    approximating the hessian of the objective function.
  !+ad_argc    On input, the approximation is provided by the user if
  !+ad_argc    MODE = 1 and is set to the identity matrix if MODE = 0.
  !+ad_args  lb : input integer : array dimension, >= n+1
  !+ad_args  tol : input real : A normal return occurs when the
  !+ad_argc    objective function plus suitably weighted multiples
  !+ad_argc    of the constraint functions are predicted to differ from
  !+ad_argc    their optimal values by at most TOL.
  !+ad_args  maxfev : input integer : maximum the number of calls to FCNVMC1
  !+ad_args  info : output integer : error flag<BR>
  !+ad_argc    INFO < 0 : user termination<BR>
  !+ad_argc    INFO = 0 : improper input parameters<BR>
  !+ad_argc    INFO = 1 : normal return<BR>
  !+ad_argc    INFO = 2 : number of calls to FCNVMC1 is at least MAXFEV<BR>
  !+ad_argc    INFO = 3 : line search required ten calls of FCNVMC1<BR>
  !+ad_argc    INFO = 4 : uphill search direction was calculated<BR>
  !+ad_argc    INFO = 5 : quadratic programming technique was unable to find
  !+ad_argc               a feasible point<BR>
  !+ad_argc    INFO = 6 : quadratic programming technique was restricted by
  !+ad_argc               an artificial bound or failed due to a singular
  !+ad_argc               matrix
  !+ad_args  nfev : output integer : number of calls to FCNVMC1
  !+ad_args  vlam(m+2n+1) : output real array : Lagrange multipliers at output X.
  !+ad_argc    The Lagrange multipliers provide the sensitivity of the objective
  !+ad_argc    function to changes in the constraint functions.
  !+ad_argc    Note that VLAM(M+I), I=1,...,N gives the multipliers for
  !+ad_argc    the lower bound constraints.  VLAM(M+N+1+I), I=1,...,N
  !+ad_argc    gives the multipliers for the upper bound constraints.
  !+ad_args  glag(n) : output real array : components of the gradient of
  !+ad_argc    the Lagrangian function at the output X.
  !+ad_args  cm(m) : output real array : work array
  !+ad_args  vmu(m+2n+1) : output real array : work array
  !+ad_args  glaga(n) : output real array : work array
  !+ad_args  gamma(n) : output real array : work array
  !+ad_args  eta(n) : output real array : work array
  !+ad_args  xa(n) : output real array : work array
  !+ad_args  bdelta(n) : output real array : work array
  !+ad_args  delta(ldel) : output real array : work array
  !+ad_args  ldel : input integer : array dimension, >= max(7*(N+1),4*(N+1)+M)
  !+ad_args  gm(n+1) : output real array : work array
  !+ad_args  bdl(n+1) : output real array : work array
  !+ad_args  bdu(n+1) : output real array : work array
  !+ad_args  h(lh,lh) : output real array : work array
  !+ad_args  lh : input integer : array dimension, >= 2*(N+1)
  !+ad_args  wa(lwa) : output real array : work array
  !+ad_args  lwa : input integer : array dimension, >= 2*(N+1)
  !+ad_args  iwa(liwa) : output integer array : work array
  !+ad_args  liwa : input integer : array dimension, >= 6*(N+1) + M
  !+ad_args  ilower(n) : input integer array : If X(I) has a lower bound,
  !+ad_argc    ILOWER(I) is set to 1 on input, otherwise 0
  !+ad_args  bndl(n) : input real array : lower bound of X(I)
  !+ad_args  iupper(n) : input integer array : If X(I) has an upper bound,
  !+ad_argc    IUPPER(I) is set to 1 on input, otherwise 0
  !+ad_args  bndu(n) : input real array : upper bound of X(I)
  !+ad_desc  This subroutine calculates the least value of a function of
  !+ad_desc  several variables subject to linear and/or nonlinear equality
  !+ad_desc  and inequality constraints.
  !+ad_desc  <P>More particularly, it solves the problem
  !+ad_desc  <PRE>
  !+ad_desc  Minimize f(x)
  !+ad_desc     subject to c (x) =  0.0 ,  i = 1,...,meq
  !+ad_desc                 i
  !+ad_desc            and c (x) >= 0.0 ,  i = meq+1,...,m
  !+ad_desc                 i
  !+ad_desc            and l <= x <= u  ,  i = 1,...,n
  !+ad_desc                 i    i    i
  !+ad_desc  </PRE>
  !+ad_desc  The subroutine implements a variable metric method for
  !+ad_desc  constrained optimization developed by M.J.D. Powell.
  !+ad_prob  None
  !+ad_call  fcnvmc1
  !+ad_call  fcnvmc2
  !+ad_call  qpsub
  !+ad_hist  --/06/79 MM  Modified for simple bounds
  !+ad_hist  18/11/83 MM  Modified for separate function and gradient evaluation
  !+ad_hist  21/05/91 JG  Modified argument list
  !+ad_hist  21/05/91 JG  Modified for separate fcnvmc1, fcnvmc2 functions
  !+ad_hist  09/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: mode,n,m,meq,lcnorm,lb,maxfev,ldel,lh,lwa,liwa
  integer, intent(out) :: info,nfev

  integer, dimension(liwa), intent(out) :: iwa
  integer, dimension(n), intent(in) :: ilower,iupper
  real(kind(1.0D0)), intent(out) :: objf
  real(kind(1.0D0)), intent(in) :: tol
  real(kind(1.0D0)), dimension(n), intent(inout) :: x
  real(kind(1.0D0)), dimension(n), intent(in) :: bndl,bndu
  real(kind(1.0D0)), dimension(n), intent(out) :: fgrd
  real(kind(1.0D0)), dimension(m), intent(out) :: conf
  real(kind(1.0D0)), dimension(n), intent(out) :: glag,glaga,gamma,eta,xa,bdelta
  real(kind(1.0D0)), dimension(m), intent(out) :: cm
  real(kind(1.0D0)), dimension(ldel), intent(out) :: delta
  real(kind(1.0D0)), dimension(lwa), intent(out) :: wa
  real(kind(1.0D0)), dimension(lcnorm,m), intent(out) :: cnorm
  real(kind(1.0D0)), dimension(lh,lh), intent(out) :: h
  real(kind(1.0D0)), dimension(lb,lb), intent(inout) :: b
  real(kind(1.0D0)), dimension(*), intent(out) :: vlam,vmu,gm,bdl,bdu

  !  Local variables

  integer :: i,j,k,mact,nfinit,nls,np1,np1j,npp,nqp,nsix,nsixi
  integer :: inx,ki,ml,mlp1,mcon,mp1,mpn,mpnpp1,mpnppn

  real(kind(1.0D0)) :: alpha,aux,auxa,calpha,dbd,dflsa,dg, &
       fls,flsa,spgdel,sum,temp,thcomp,theta
  real(kind(1.0D0)), parameter :: zero = 0.0D0
  real(kind(1.0D0)), parameter :: cp1 = 0.1D0
  real(kind(1.0D0)), parameter :: cp2 = 0.2D0
  real(kind(1.0D0)), parameter :: cp5 = 0.5D0
  real(kind(1.0D0)), parameter :: one = 1.0D0

  !  External routines

  external :: fcnvmc1,fcnvmc2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  np1 = n + 1
  npp = 2*np1
  info = 0

  !  Check input parameters for errors

  if ( &
       (n <= 0)           .or. &
       (m <= 0)           .or. &
       (meq > n)         .or. &
       (lcnorm < (n+1))  .or. &
       (lb < (n+1))      .or. &
       (tol < zero)      .or. &
       (ldel < max(7*(n+1),4*(n+1)+m)).or. &
       (lh < (2*(n+1)))  .or. &
       (lwa < (2*(n+1))) .or. &
       (liwa < (6*(n+1)+m)) &
       ) return

  !  Set the initial elements of b and vmu. vmu is the weighting
  !  vector to be used in the line search.
  !  Use hessian estimate provided by user if mode = 1 on input

  if (mode /= 1) then
     !  Use identity matrix for hessian estimate
     do j = 1, n
        do i = 1, n
           b(i,j) = zero
        end do
        b(j,j) = one
     end do
  end if

  mp1 = m + 1
  mpn = m + n
  mpnpp1 = m + np1 + 1
  mpnppn = m + np1 + n

  !  Set mcon to total number of actual constraints

  mcon = m
  do i = 1,n
     if (ilower(i) == 1) mcon = mcon + 1
  end do

  !  Set ml to m + number of lower bounds

  ml = mcon

  !  Set mlp1 to ml + 1

  mlp1 = ml + 1
  do i = 1, n
     if (iupper(i) == 1) mcon = mcon + 1
  end do
  do k = 1, mpnppn
     vmu(k) = zero
  end do

  !  Set initial values of some variables
  !  nfev is the number of calls of fcnvmc1
  !  nsix is the length of an array
  !  nqp is the number of quadratic subproblems

  nfev = 1
  nsix = 6*np1
  nqp = 0

  !  Calculate the initial functions and gradients

  call fcnvmc1(n,m,x,objf,conf,info)
  if (info < 0) return

  call fcnvmc2(n,m,x,fgrd,cnorm,lcnorm,info)
  if (info < 0) return

  !  Start the iteration by calling the quadratic programming
  !  subroutine

  iteration: do

     !  Increment the quadratic subproblem counter

     nqp = nqp + 1

     !  Set the linear term of the quadratic problem objective function
     !  to the negative gradient of objf

     do i = 1, n
        gm(i) = -fgrd(i)
     end do
     do i = 1, mpnppn
        vlam(i) = zero
     end do

     call qpsub( &
          n,m,meq,conf,cnorm,lcnorm,b,lb,gm,bdl,bdu,info,x,delta, &
          ldel,cm,h,lh,mact,wa,lwa,iwa,liwa,ilower,iupper, &
          bndl,bndu)

     !  The following return is made if the quadratic problem solver
     !  failed to find a feasible point, if an artificial bound was
     !  active, or if a singular matrix was detected

     if ((info == 5).or.(info == 6)) return

     !  Initialize the line search iteration counter

     nls = 0

     !  Calculate the Lagrange multipliers

     do j = 1, mact
        k = iwa(j) - npp
        if (k > 0) goto 59
        ki = iwa(j)
        k = ki + m
        if (ki > np1) goto 58
        if (ki == np1) cycle
        if (ilower(ki) == 1) goto 59
        cycle
58      continue
        ki = iwa(j) - np1
        k = ki + m + np1
        if (ki == np1) cycle
        if (iupper(ki) == 1) goto 59
        cycle
59      continue
        do i = 1, n
           np1j = np1 + j
           nsixi = nsix + i
           vlam(k) = vlam(k) + h(np1j,i)*delta(nsixi)
        end do
     end do

     !  Calculate the gradient of the Lagrangian function
     !  nfinit is the value of nfev at the start of an iteration

     nfinit = nfev
     do i = 1, n
        glag(i) = fgrd(i)
     end do
     do k = 1, m
        if (vlam(k) /= zero) then
           do i = 1, n
              glag(i) = glag(i) - cnorm(i,k)*vlam(k)
           end do
        end if
     end do
     do k = mp1, mpn
        if (vlam(k) /= zero) then
           inx = k - m
           if (ilower(inx) /= 0) glag(inx) = glag(inx) - vlam(k)
        end if
     end do
     do k = mpnpp1, mpnppn
        if (vlam(k) /= zero) then
           inx = k - m - np1
           if (iupper(inx) /= 0) glag(inx) = glag(inx) + vlam(k)
        end if
     end do

     !  Set spgdel to the scalar product of fgrd and delta
     !  Store the elements of glag and x

     spgdel = zero
     do i = 1, n
        spgdel = spgdel + fgrd(i)*delta(i)
        glaga(i) = glag(i)
        xa(i) = x(i)
     end do

     !  Revise the vector vmu and test for convergence

     sum = abs(spgdel)
     do k = 1, mpnppn
        aux = abs(vlam(k))
        vmu(k) = max(aux,cp5*(aux + vmu(k)))
        temp = 0.0D0
        if (k <= m) then
           temp = conf(k)
        else
           if (k <= mpn) then
              inx = k - m
              if (ilower(inx) == 0) cycle
              temp = x(inx) - bndl(inx)
           else
              inx = k - m - np1
              if ((inx == 0) .or. (inx > n)) cycle
              if (iupper(inx) == 0) cycle
              temp = bndu(inx) - x(inx)
           end if
        end if
        sum = sum + abs(vlam(k)*temp)
     end do

     !  Exit if convergence criterion is satisfied

     if (sum <= tol) return

     !  Set sum to the weighted sum of infeasibilities
     !  Set fls to the line search objective function

     line_search: do

        !  Increment the line search iteration counter

        nls = nls + 1
        sum = zero
        do k = 1, mpnppn
           aux = 0.0D0
           if (k <= meq) aux = conf(k)
           temp = 0.0D0
           if (k <= m) then
              temp = conf(k)
           else
              if (k <= mpn) then
                 inx = k - m
                 if (ilower(inx) == 0) cycle
                 temp = x(inx) - bndl(inx)
              else
                 inx = k - m - np1
                 if ((inx == 0) .or. (inx > n)) cycle
                 if (iupper(inx) == 0) cycle
                 temp = bndu(inx) - x(inx)
              end if
           end if
           sum = sum + vmu(k)*max(aux,-temp)
        end do
        fls = objf + sum

        if (nfev == nfinit) then

           !  Set the initial conditions for the line search
           !  flsa is the initial value of the line search function
           !  dflsa is its first derivative (if delta(np1) = 1)
           !  alpha is the next reduction in the step-length

           flsa = fls
           dflsa = spgdel - delta(np1)*sum
           if (dflsa >= zero) then
              !  Error return because uphill search direction was calculated
              info = 4
              return
           end if

           !  Set initial multiplying factor for stepsize
           !  Set initial value of stepsize for output

           alpha = one
           calpha = one

        else

           !  Test whether line search is complete

           aux = fls - flsa

           !  Exit line search if function difference is small

           if (aux <= (cp1*dflsa)) exit line_search

           !  Exit if the line search requires ten or more function
           !  evaluations

           if (nfev >= (nfinit + 10)) then
              do i = 1, n
                 x(i) = xa(i)
              end do
              nfev = nfev + 1
              call fcnvmc1(n,m,x,objf,conf,info)
              if (info >= 0) info = 3
              !  Error return because line search required 10 calls of fcnvmc1
              return
           end if

           !  Calculate next reduction in the line step assuming
           !  a quadratic fit

           alpha = max(cp1,cp5*dflsa/(dflsa - aux))

        end if

        !  Multiply delta by alpha and calculate the new x

        calpha = alpha*calpha
        do i = 1, n
           delta(i) = alpha*delta(i)
           x(i) = xa(i) + delta(i)
        end do

        dflsa = alpha*dflsa

        !  Test nfev against maxfev, call fcnvmc1 and resume line search

        if (nfev >= maxfev) then
           do i = 1, n
              x(i) = xa(i)
           end do
           !  Error return because there have been maxfev calls of fcnvmc1
           info = 2
           return
        end if

        nfev = nfev + 1

        call fcnvmc1(n,m,x,objf,conf,info)

        if (info < 0) return

     end do line_search

     !  Line search is complete. Calculate gradient of Lagrangian
     !  function for use in updating hessian of Lagrangian

     call fcnvmc1(n,m,x,objf,conf,info)
     call fcnvmc2(n,m,x,fgrd,cnorm,lcnorm,info)
     if (info < 0) return

     do i = 1, n
        glag(i) = fgrd(i)
     end do
     do k = 1, m
        if (vlam(k) /= zero) then
           do i = 1, n
              glag(i) = glag(i) - cnorm(i,k)*vlam(k)
           end do
        end if
     end do
     do k = mp1, mpn
        if (vlam(k) /= zero) then
           inx = k - m
           if (ilower(inx) /= 0) glag(inx) = glag(inx) - vlam(k)
        end if
     end do
     do k = mpnpp1, mpnppn
        if (vlam(k) /= zero) then
           inx = k - m - np1
           if (iupper(inx) /= 0) glag(inx) = glag(inx) + vlam(k)
        end if
     end do

     !  Calculate gamma and bdelta in order to revise b
     !  Set dg to the scalar product of delta and gamma
     !  Set dbd to the scalar product of delta and bdelta

     dg = zero
     dbd = zero
     do i = 1, n
        gamma(i) = glag(i) - glaga(i)
        bdelta(i) = zero
        do j = 1, n
           bdelta(i) = bdelta(i) + b(i,j)*delta(j)
        end do
        dg = dg + delta(i)*gamma(i)
        dbd = dbd + delta(i)*bdelta(i)
     end do

     !  Calculate the vector eta for the b-f-g-s formula
     !  replace dg by the scalar product of delta and eta

     aux = cp2*dbd
     theta = one
     if (dg < aux) theta = (dbd - aux)/(dbd - dg)
     thcomp = one - theta
     do i = 1, n
        eta(i) = theta*gamma(i) + thcomp*bdelta(i)
     end do
     if (dg < aux) dg = aux

     !  Revise the matrix b and begin new iteration

     do i = 1, n
        aux = bdelta(i)/dbd
        auxa = eta(i)/dg
        do j = i, n
           b(i,j) = b(i,j) - aux*bdelta(j) + auxa*eta(j)
           b(j,i) = b(i,j)
        end do
     end do

  end do iteration

end subroutine vmcon
!______________________________________________________________________
SUBROUTINE QPSUB( &
     n,m,meq,conf,cnorm,lcnorm,b,lb,gm,bdl,bdu,info, &
     x,delta,ldel,cm,h,lh,mact,wa,lwa,iwa,liwa, &
     ilower,iupper,bndl,bndu)

  !  This subroutine finds the value of the solution vector which
  !  minimizes a quadratic function of several variables subject to
  !  equality and inequality constraints. This is accomplished
  !  by invoking subroutine harwqp, a modified version of subroutine
  !  ve02ad, the Harwell Library subroutine for general quadratic
  !  programming.
  !
  !  The subroutine statement is
  !
  !  subroutine qpsub(n,m,meq,conf,cnorm,lcnorm,b,lb,gm,bdl,bdu,
  !                   info,delta,ldel,cm,h,lh,mact,wa,lwa,iwa,liwa)
  !
  !  where
  !
  !  N is a positive integer input variable set to the number of
  !  variables.
  !
  !  M is a positive integer input variable set to the number of
  !  constraints.
  !
  !  MEQ is a non-negative integer input variable set to the number
  !  of equality constraints. MEQ must be less than or equal to N.
  !
  !  CONF is a real input array of length M which contains the
  !  constraint functions.
  !
  !  CNORM is a real LCNORM by M array whose columns contain the
  !  constraint normals in the first N positions.  The (N+1)st
  !  row of CNORM is used for work space.
  !
  !  LCNORM is a positive integer input variable set to the row
  !  dimension of CNORM which is at least N+1.
  !
  !  B is a real LB by LB array whose first N rows and columns
  !  contain the hessian approximation on input.  The (N+1)st
  !  row and column are used for work space.
  !
  !  LB is a positive integer input variable set to the row
  !  dimension of B which is at least N+1.
  !
  !  GM is a real array of length N+1 which, on input, contains
  !  the negative components of the function gradient in the
  !  first N elements. The (N+1)st element is used for work space.
  !
  !  BDL, BDU are real work arrays of length N+1.
  !
  !  INFO is an integer variable. It must be set to zero before
  !  the initial call to qpsub and should not otherwise be
  !  changed.  On output, INFO is set as follows
  !
  !   INFO = 1  a normal return.
  !
  !   INFO = 5  a feasible point was not found.
  !
  !   INFO = 6  solution is restricted by an artificial bound or
  !             failed due to a singular matrix.
  !
  !  DELTA is a real array of length LDEL.  It need not be set
  !  before the first call to QPSUB, but before each subsequent
  !  call, the first N locations should contain an estimate of
  !  the solution vector. (Zero is used as the estimate for the
  !  first call.)  On output, the value of the solution vector
  !  which minimizes the quadratic function is contained in the
  !  first N locations.  The remainder of the array is used for
  !  work space.
  !
  !  LDEL is a positive integer input variable set to the length
  !  of DELTA which is at least MAX(7*(N+1),4*(N+1)+M).
  !
  !  CM is a real work array of length M.
  !
  !  H is a real LH by LH work array.
  !
  !  LH is a positive integer input variable set to the dimension
  !  of the square array H which is at least 2*(N+1).
  !
  !  MACT is an integer output variable set to the number of
  !  constraints in the basis.
  !
  !  WA is a real work array of length LWA.
  !
  !  LWA is a positive integer input variable set equal to the
  !  dimension of WA which is at least 2*(N+1).
  !
  !  IWA is an integer work array of length LIWA.
  !
  !  LIWA is a positive integer input variable set to the length
  !  of IWA which is at least 6*(N+1) + M.
  !
  !  ILOWER is an integer array of length N.
  !  If X(I) has a lower bound, ILOWER(I) is set to 1
  !  on input.  If no bound is provided, ILOWER(I) should
  !  be 0 (the default value).
  !
  !  BNDL is a real array of length N.
  !  If X(I) has a lower bound, it should be given in BNDL(I).
  !
  !  IUPPER is an integer array of length N.
  !  If X(I) has a upper bound, IUPPER(I) is set to 1
  !  on input.  If no bound is provided, IUPPER(I) should
  !  be 0 (the default value).
  !
  !  BNDU is a real array of length N.
  !  If X(I) has a upper bound, it should be given in BNDU(I).
  !
  !  Roger L. Crane, Kenneth E. Hillstrom, Michael Minkoff
  !  Modified for simple bounds, M. Minkoff (10/26/82)
  !
  !  Modified to pass ILOWER,IUPPER, BNDL,BNDU in through argument list
  !  instead of through COMMON, J. Galambos, (5/21/91)

  IMPLICIT NONE

  INTEGER n,m,meq,lcnorm,lb,info,ldel,lh,mact,lwa,liwa
  INTEGER iwa(liwa),ilower(n),iupper(n)
  INTEGER i,iflag,j,k,mode,mtotal,np1,npp
  INTEGER inx

  real(kind(1.0D0)) conf(m),cnorm(lcnorm,m),b(lb,lb),gm(*),bdl(*), &
       bdu(*),delta(ldel),cm(m),h(lh,lh),wa(lwa)
  real(kind(1.0D0)) x(n),bndu(n),bndl(n)
  real(kind(1.0D0)) cd6,cdm6,cp9,one,zero

  !+**PJK 24/05/06 Added SAVE command (as a number of variables are
  !+**PJK 24/05/06 initialised only if info=0)

  SAVE

  cd6 = 1.0D6
  cdm6 = 1.0D-6
  cp9 = 0.9D0
  one = 1.0D0
  zero = 0.0D0

  np1 = n + 1
  npp = 2*np1
  if (info > 0) goto 50
  mtotal = m + npp

  !  Set initial values of some variables

  info = 1
  mact = meq + 1
  mode = 1

  !  Set the initial elements of bdl, bdu, and delta where
  !  bdl are artificial lower bounds,
  !  bdu are artificial upper bounds and
  !  delta is an initial solution estimate

  do i = 1, n
     bdl(i) = -cd6
     bdu(i) = cd6
  end do

  !  Bound the artificial variables in qp

  bdl(np1) = zero
  delta(np1) = one
  if (meq <= 0) goto 30

  !  Set indices of equality constraints
  !  The bounds are the first npp constraints

  do k = 1, meq
     iwa(k) = k + npp
  end do

30 continue

  !  Set index of upper bound of delta(np1) active

  iwa(mact) = npp

  !  Extend gm and b because of the extra variable that is introduced
  !  to allow for feasibility. Set linear term of cost function to
  !  a large value

  gm(np1) = cd6
  do i = 1, np1
     b(i,np1) = zero
     b(np1,i) = zero
  end do

  !  Set the elements of cm and cnorm(np1,*)

50 continue
  do i = 1, n
     if (ilower(i) == 1) bdl(i) = bndl(i) - x(i)
     if (iupper(i) == 1) bdu(i) = bndu(i) - x(i)
     delta(i) = max(zero,bdl(i))
     delta(i) = min(delta(i),bdu(i))
  end do
  do k = 1, m
     if (k <= meq) goto 70
     if (conf(k) < zero) goto 70

     !  If an inequality constraint is satisfied set the constant term
     !  in the constraint vector to the violation and put zero in the
     !  constraint matrix for the (n+1)st variable

     cm(k) = -conf(k)
     cnorm(np1,k) = zero
     goto 90
70   continue

     !  If the constraint is an equality or a violated inequality set
     !  the constant term to zero and put the function value in the
     !  constraint matrix for the (n+1)st variable

     cm(k) = zero
     cnorm(np1,k) = conf(k)
90   continue
  end do

  !  Set the upper bound of the (n+1)st variable
  !  Set iflag. iflag will be used in checking active constraints
  !  Call subroutine harwqp to solve quadratic programming problem

  bdu(np1) = one
  iflag = -1
100 continue

  call harwqp(np1,mtotal,b,lb,gm,cnorm,lcnorm,cm,bdl,bdu,delta, &
       mact,meq,h,lh,iwa,wa,iwa(4*(n+1)+m+1),mode,info)

  if (info  /=  1) goto 130

  !  Check whether the required feasibility conditions hold
  !  If delta(np1) is sufficiently small there is no feasible
  !  solution

  if (delta(np1) <= cdm6) goto 120

  !  Check whether active constraints are bounds

  do j = 1, mact
     if (iwa(j) > npp) goto 110
     if (iwa(j) == npp) goto 101
     if (iwa(j) > np1) goto 105
     if (iwa(j) == np1) goto 130
     if (ilower(iwa(j)) == 0) goto 130
     goto 110
105  continue
     inx = iwa(j) - np1
     if (iupper(inx) == 0) goto 130
     goto 110
101  continue

     !  The active constraint is blu(np1)

     iflag = 1
110  continue
  end do

  !  Normal exit

  if (iflag >= 1) goto 140

  !  A second call to harwqp found blu(np1) to still be inactive
  !  thus an error exit is made

  if (iflag >= 0) goto 120

  !  Reduce bdu(np1) and retry harwqp

  bdu(np1) = cp9*delta(np1)
  iflag = 0
  goto 100

  !  Error return because of infeasibility

120 continue
  info = 5
  goto 140

  !  Error return because of restriction by an artificial bound
  !  or detection of a singular matrix

130 continue
  info = 6

140 continue

  return
end SUBROUTINE QPSUB
!______________________________________________________________________
SUBROUTINE HARWQP( &
     n,m,a,ia,b,c,ic,d,bdl,bdu,x,k,ke,h,ih,lt,wa,iwa,mode,info)

  !  This program is a modified version of the Harwell library
  !  subroutine VE02AD dated 11/06/70.  The modifications were made
  !  to substitute the subroutines HINV and DOTPMC for Harwell
  !  subroutines MB01B and MC03AS.  The calling sequence above
  !  includes three entries, WA, IWA, and INFO not present in
  !  the original program.  WA and IWA are real and
  !  integer work arrays, respectively, and must be dimensioned
  !  at least 2*N.  INFO is an output variable set to one for a
  !  normal return and set to two when a singular matrix is detected
  !  in HINV.  All other entries in the calling sequence are as
  !  described in the Harwell documentation.
  !
  !  Modified 5/22/91 to use implicit none (J. Galambos)
  !
  !+**PJK 02/11/92 Throughout this routine, argument 1 of DOTPMC has
  !+**PJK 02/11/92 different dimensions than are declared in the source
  !+**PJK 02/11/92 code of the routine itself. The program runs without
  !+**PJK 02/11/92 error but beware of future modifications.

  IMPLICIT NONE

  INTEGER n,m,ia,ic,k,ke,ih,mode,info
  INTEGER iwa(*),lt(*)
  INTEGER i, ial, ib, ii, j, li, ni, nk, nn, n3,n4,n5,n6
  INTEGER i0,i1,i2,i3

  real(kind(1.0D0)) a(ia,*),b(*),c(ic,*),d(*),bdl(*),bdu(*),x(*), &
       h(ih,*),wa(*)
  real(kind(1.0D0)) alpha, cac, cc, chc, ghc, y, z, zz
  real(kind(1.0D0)) r0

  LOGICAL retest,passiv,postiv

  i0 = 0
  i1 = 1
  i2 = 2
  i3 = 3
  r0 = 0.0D0

  info = 1
  retest = .false.
  nn = n+n
  n3 = nn+n
  n4 = nn+nn
  n5 = n4+n
  n6 = n5+n

  if (mode >= 3) goto 99

  !  Call feasible vertex routine

8 continue
  call harwfp(n,m,c,ic,d,bdl,bdu,x,k,ke,h,ih,lt,wa,iwa,info)
  if (info  /=  1) goto 1000
  if (k == 0) goto 1000
  if ((mode == 2).and.(.not.retest)) goto 100

  !  Initial operators h=0 and cstar=c(-1) from ve02b
  do i = 1,n
     do j = 1,n
        h(n+i,j) = h(i,j)
        h(i,j) = 0.0D0
     end do
  end do
  goto 120

99 continue
  do i = 1,m
     lt(nn+i) = 1
  end do

  !  Constraints indexed as  -1=equality, 0=active, 1=inactive
  if (k == 0) goto 100
  do i=1,k
     j=0
     if (i <= ke)j=-1
     lt(nn+lt(i))=j
  end do

100 continue
  if ((mode == 5).and.(.not.retest)) goto 109

  !  Set up matrix and rhs of equations governing equality problem
  do i = 1,n
     x(n+i) = b(i)
     do j = 1,n
        h(i,j) = a(i,j)
     end do
  end do

  if (((mode == 2).or.(mode == 3)).and.(.not.retest)) goto 200
  if (k == 0) goto 107
  do i = 1,k
     li = lt(i)
     if (li > nn) goto 105
     do j = 1,n
        h(j,n+i) = 0.0D0
        h(n+i,j) = 0.0D0
     end do
     if (li > n) goto 104
     h(n+i,li) = 1.0D0
     h(li,n+i) = 1.0D0
     x(nn+i) = bdl(li)
     goto 108

104  continue
     li = li-n
     h(n+i,li) = -1.0D0
     h(li,n+i) = -1.0D0
     x(nn+i) = -bdu(li)
     goto 108

105  continue
     li = li-nn
     do j = 1,n
        h(n+i,j) = c(j,li)
        h(j,n+i) = c(j,li)
     end do
     x(nn+i) = d(li)

108  continue
     do j = 1,k
        h(n+i,n+j) = 0.0D0
     end do
  end do

107 continue
  nk = n+k

  !  Invert matrix giving operators h and cstar

  call hinv(h,ih,nk,iwa,info)
  if (info  /=  1) goto 1000
  goto 118

  !  Set up rhs only

109 continue
  do i = 1,n
     x(n+i) = b(i)
  end do

  do i = 1,k
     li = lt(i)
     if (li > nn) goto 117
     if (li > n) goto 116
     x(nn+i) = bdl(li)
     goto 115

116  continue
     x(nn+i) = -bdu(li-n)
     goto 115

117  continue
     x(nn+i)=d(li-nn)
115  continue
  end do

  !  Solve for solution point x

  nk = n+k

118 continue
  do i = 1,n
     call dotpmc(h(1,i),i1,x(n+1),i1,r0,x(i),nk,i0)
  end do

  !  Check feasibility, if not exit to 8

  do i = 1,m
     if (lt(nn+i) <= 0) goto 110
     if (i > n) goto 111
     z = x(i)-bdl(i)
     goto 114

111  continue
     if (i > nn) goto 112
     z = bdu(i-n)-x(i-n)
     goto 114

112  continue
     j = i-nn
     call dotpmc(c(1,j),i1,x(1),i1,d(j),z,n,i2)

114  continue
     if (z < 0.0D0) goto 8
110  continue
  end do

120 continue

  !  Calculate gradient g and Lagrange multipliers -cstar.g,
  !  Find largest multiplier,  exit if not positive

  do i = 1,n
     call dotpmc(a(i,1),ia,x(1),i1,b(i),x(n6+i),n,i2)
  end do
  if (k == 0) goto 1000

  !+**PJK 17/11/97 D999 reduced to D99
  z = -1.0D99
  do i = 1,k
     if (lt(nn+lt(i)) == -1) goto 122
     call dotpmc(h(n+i,1),ih,x(n6+1),i1,r0,zz,n,i3)
     if (zz <= z) goto 122
     z = zz
     ii = i
122  continue
  end do

  if (z > 0.0D0) goto 130
  if ((retest).or.(mode >= 4)) goto 137
  retest = .true.
  goto 100

137 continue
  if (z /= 0.0D0) goto 1000
  goto 1000

  !  Set direction of search as corresponding row of cstar

130 continue
  do i = 1,n
     x(nn+i) = h(n+ii,i)
  end do

136 continue
  do i = 1,n
     call dotpmc(a(i,1),ia,x(nn+1),i1,r0,x(n+i),n,i0)
  end do
  call dotpmc(x(nn+1),i1,x(n+1),i1,r0,cac,n,i0)
  if (cac > 0.0D0) goto 134
  postiv = .false.
  y = 1.0D0
  goto 135

134 continue
  postiv = .true.
  y = z/cac

135 continue
  do i = 1,n
     x(n5+i) = x(nn+i)*y
  end do
  passiv = .true.

139 continue
  !+**PJK 17/11/97 D999 reduced to D99
  alpha = 1.0D99
  nk = n+k

  !  Linear search along direction of search,  passiv indicates
  !  a constraint has been removed to get search direction,
  !  postiv indicates positive curvature along the direction

  do i = 1,m
     if (lt(nn+i) <= 0) goto 140
     if (i > n) goto 141
     if (x(n5+i) >= 0.0D0) goto 140
     cc = (bdl(i)-x(i))/x(n5+i)
     goto 143

141  continue
     if (i > nn) goto 142
     if (x(n4+i) <= 0.0D0) goto 140
     cc = (bdu(i-n)-x(i-n))/x(n4+i)
     goto 143

142  continue
     j = i-nn
     call dotpmc(c(1,j),i1,x(n5+1),i1,r0,zz,n,i0)
     if (zz >= 0.0D0) goto 140
     call dotpmc(c(1,j),i1,x(1),i1,d(j),cc,n,i1)
     cc = cc/zz

143  continue
     if (cc >= alpha) goto 140
     alpha = cc
     ial = i
140  continue
  end do
  if (passiv) lt(nn+lt(ii)) = 1

  !  If minimum found, goto  170

  if ((postiv).and.(alpha >= 1.0D0)) goto 170

  !  Calculate h.c and cstar.c

  do i=1,n
     x(i) = x(i)+alpha*x(n5+i)
  end do
  alpha = alpha*y
  j = 1
  if (k == n) j = n+1
  if (ial > n) goto 146
  do i = j,nk
     x(n3+i) = h(i,ial)
  end do
  chc = x(n3+ial)
  goto 151

146 continue
  ib = ial-n
  if (ib > n) goto 148
  do i = j,nk
     x(n3+i) = -h(i,ib)
  end do
  chc = -x(n3+ib)
  goto 151

148 continue
  ib = ib-n
  do i = 1,n
     x(n5+i) = c(i,ib)
  end do
  do i = j,nk
     call dotpmc(h(i,1),ih,x(n5+1),i1,r0,x(n3+i),n,i0)
  end do
  if (k /= n) call dotpmc(x(n5+1),i1,x(n3+1),i1,r0,chc,n,i0)

151 continue
  lt(nn+ial) = 0
  if (k == n) goto 180
  if (passiv) goto 160

  !  Apply formula for adding a constraint

156 continue
  if (k == 0) goto 157
  do i = 1,k
     alpha = x(n4+i)/chc
     ni = n+i
     do j = 1,n
        h(ni,j) = h(ni,j)-alpha*x(n3+j)
     end do
  end do

157 continue
  k = k+1
  lt(k) = ial
  do j = 1,n
     h(n+k,j) = x(n3+j)/chc
  end do
  if (k < n) goto 154
  do i = 1,n
     do j = 1,n
        h(i,j) = 0.0D0
     end do
  end do
  goto 159

154 continue
  do i = 1,n
     alpha = x(n3+i)/chc
     do j = 1,i
        h(i,j) = h(i,j)-alpha*x(n3+j)
        h(j,i) = h(i,j)
     end do
  end do

159 continue
  if (.not.passiv) goto 167

  !  Removal of a constraint has been deferred,  set up as if
  !  the constraint is being removed from augmented basis

  do i=1,n
     call dotpmc(a(i,1),ia,x(1),i1,b(i),x(n6+i),n,i2)
     x(nn+i) = h(n+ii,i)
  end do
  call dotpmc(x(n6+1),i1,x(nn+1),i1,r0,z,n,i3)
  if (z == 0.0D0) goto 178
  goto 136

160 continue
  cc = x(n4+ii)
  y = chc*cac+cc**2
  call dotpmc(x(n6+1),i1,x(n3+1),i1,r0,ghc,n,i0)
  if ((alpha*y) < (chc*(z-alpha*cac)+ghc*cc)) goto 156

  !  Apply formula for exchanging new constraint
  !  with passive constraint

  do i = 1,k
     ni = n+i
     call dotpmc(h(ni,1),ih,x(n+1),i1,r0,x(n5+i),n,i0)
  end do
  do i = 1,n
     x(n+i) = (chc*x(nn+i)-cc*x(n3+i))/y
     x(n6+i) = (cac*x(n3+i)+cc*x(nn+i))/y
  end do
  do i = 1,n
     do j = 1,i
        h(i,j) = h(i,j)+x(n+i)*x(nn+j)-x(n6+i)*x(n3+j)
        h(j,i) = h(i,j)
     end do
  end do
  x(n4+ii) = x(n4+ii)-1.0D0
  do i = 1,k
     ni = n+i
     do j = 1,n
        h(ni,j) = h(ni,j)-x(n4+i)*x(n6+j)-x(n5+i)*x(n+j)
     end do
  end do
  lt(ii) = ial

167 continue
  if (k == n) goto 120

  !  Calculate g,  new search direction is -h.g

  do i = 1,n
     call dotpmc(a(i,1),ia,x(1),i1,b(i),x(n+i),n,i2)
  end do
  z = 0.0D0
  do i = 1,n
     call dotpmc(h(i,1),ih,x(n+1),i1,r0,x(n5+i),n,i3)
     if (x(n5+i) /= 0.0D0) z = 1.0D0
  end do
  passiv = .false.
  if (z == 0.0D0) goto 120
  postiv = .true.
  goto 139

170 continue
  do i = 1,n
     x(i) = x(i)+x(n5+i)
  end do

  !  x is now the minimum point in the basis
  !  Update the operators if a constraint had been removed

  if (.not.passiv) goto 120

178 continue
  do i = 1,n
     alpha = x(nn+i)/cac
     do j = 1,i
        h(i,j) = h(i,j)+alpha*x(nn+j)
        h(j,i) = h(i,j)
     end do
  end do
  if (k > 1) goto 177
  k = 0
  goto 120

177 continue
  if (ii == k) goto 175
  do i=1,n
     h(n+ii,i) = h(n+k,i)
  end do
  lt(ii) = lt(k)

175 continue
  k = k-1
  do i = 1,k
     ni = n+i
     call dotpmc(h(ni,1),ih,x(n+1),i1,r0,x(n3+i),n,i0)
  end do
  do i = 1,k
     alpha = x(n3+i)/cac
     ni = n+i
     do j = 1,n
        h(ni,j) = h(ni,j)-alpha*x(nn+j)
     end do
  end do
  goto 120

180 continue
  z = 1.0D0/x(n4+ii)

  !  Apply simplex formula to exchange constraints

  do i = 1,n
     ni = n+i
     if (i /= ii) goto 182
     do j = 1,n
        h(ni,j) = h(ni,j)*z
     end do
     goto 181

182  continue
     zz = z*x(n4+i)
     do j = 1,n
        h(ni,j) = h(ni,j)-zz*x(nn+j)
     end do
181  continue
  end do
  lt(ii) = ial
  goto 120

200 continue
  k = 0

  ke = 0
  do i = 1,m
     lt(nn+i) = 1
  end do
  call hinv(h,ih,n,iwa,info)
  if (info  /=  1) goto 1000

  !  Start with empty basis from feasible point
  !  Search direction is -a(-1).b

  goto 167

1000 continue

  return
end SUBROUTINE HARWQP
!______________________________________________________________________
SUBROUTINE HARWFP( &
     n,m,c,ic,d,bdl,bdu,x,k,ke,h,ih,lt,wa,iwa,info)

  !  This program is a modified version of the Harwell Library
  !  subroutine LA02AD.  The modifications were made to substitute
  !  the subroutines HINV and DOTPMC for Harwell subroutines
  !  MB01B and MC03AS.  The calling sequence above includes
  !  three entries, WA, IWA, and INFO not present in the
  !  original program.  WA and IWA are real and
  !  integer work arrays, respectively, and must be dimensioned
  !  at least 2*N.  INFO is an output variable set to one for a
  !  normal return and set to two when a singular matrix is detected
  !  in HINV.  All other entries in the calling sequence are as
  !  described in the Harwell documentation.
  !
  !  Modified  5/22/91 to use implicit none (J. Galambos)
  !
  !+**PJK 02/11/92 Throughout this routine, argument 1 of DOTPMC has
  !+**PJK 02/11/92 different dimensions than are declared in the source
  !+**PJK 02/11/92 code of the routine itself. The program runs without
  !+**PJK 02/11/92 error but beware of future modifications.

  IMPLICIT NONE

  INTEGER n,m,ic,k,ke,ih,info
  INTEGER i, ial, ib, ii, j, jj, kv, li, ni, nj, nn, n3
  INTEGER iwa(*), lt(*)
  INTEGER i0,i1,i2,i3

  real(kind(1.0D0)) c(ic,*),d(*),bdl(*),bdu(*),x(*),h(ih,*)
  real(kind(1.0D0)) wa(*)
  real(kind(1.0D0)) alpha, beta, y, z, zz
  real(kind(1.0D0)) r0

  i0 = 0
  i1 = 1
  i2 = 2
  i3 = 3
  r0 = 0.0D0

  info = 1
  nn = n+n
  n3 = nn+n
  do i = 1,m
     lt(nn+i) = 1
  end do

  !  Constraints indexed as
  !  -1=equality,  0=active,  1=inactive,  2=violated

  if (k /= 0) goto 10

  !  No designated constraints, vertex chosen from upper and
  !  lower bounds, inverse matrix trivial

  do i = 1,n
     do j = 1,n
        h(i,j) = 0.0D0
     end do
     if ((x(i)-bdl(i)) > (bdu(i)-x(i))) goto 6
     lt(i) = i
     h(i,i) = 1.0D0
     goto 998

6    continue
     lt(i) = n+i
     h(i,i) = -1.0D0
998  continue
     lt(nn+lt(i)) = 0
  end do
  k = n
  goto 40

  !  Set up normals v of the k designated constraints in basis

10 continue
  do i = 1,k
     j = 0
     if (i <= ke) j = -1
     lt(nn+lt(i)) = j
     li = lt(i)
     ni = n+i
     if (li > nn) goto 14
     do j = 1,n
        h(j,ni) = 0.0D0
     end do
     if (li > n) goto 13
     h(li,ni) = 1.0D0
     goto 11

13   continue
     h(li-n,ni) = -1.0D0
     goto 11

14   continue
     li = li-nn
     do j = 1,n
        h(j,ni) = c(j,li)
     end do
11   continue
  end do

  if (k /= n) goto 19
  do j = 1,n
     nj = n+j
     do i = 1,n
        h(i,j) = h(i,nj)
     end do
  end do
  call hinv(h,ih,n,iwa,info)
  if (info  /=  1) goto 1000
  goto 40

19 continue

  !  Form m = (vtranspose.v)(-1)
  do i = 1,k
     do j = i,k
        call dotpmc(h(1,n+i),i1,h(1,n+j),i1,r0,h(i,j),n,i0)
        h(j,i) = h(i,j)
     end do
  end do
  if (k /= 1) goto 200
  h(1,1) = 1.0D0/h(1,1)
  goto 201

200 continue
  call hinv(h,ih,k,iwa,info)
  if (info  /=  1) goto 1000

201 continue

  !  Calculate generalized inverse of v,  vplus = m.vtranspose

  do i = 1,k
     do j = 1,k
        x(n+j) = h(i,j)
     end do
     do j = 1,n
        call dotpmc(x(n+1),i1,h(j,n+1),ih,r0,h(i,j),k,i0)
     end do
  end do

  !  Set up diagonal elements of the projection matrix  p = v.vplus

  do i = 1,n
     call dotpmc(h(1,i),i1,h(i,n+1),ih,r0,x(n+i),k,i0)
  end do
  do i = 1,n
     lt(n+i) = 0
  end do
  kv = k

  !  Add bound e(i) corresponding to the smallest diag(p)

29 continue
  z = 1.0D0
  do i = 1,n
     if (lt(n+i) == 1) goto 25
     if (x(n+i) >= z) goto 25
     z = x(n+i)
     ii = i
25   continue
  end do
  y = 1.0D0
  if ( (x(ii)-bdl(ii)) > (bdu(ii)-x(ii)) ) y = -1.0D0

  !  Calculate vectors vplus.e(i) and  u = e(i)-v.vplus.e(i)

  if (y /= 1.0D0) goto 27
  do i = 1,k
     x(nn+i) = h(i,ii)
  end do
  goto 30

27 continue
  do i = 1,k
     x(nn+i) = -h(i,ii)
  end do

30 continue
  do i = 1,n
     if (lt(n+i) == 1) goto 31
     call dotpmc(h(i,n+1),ih,x(nn+1),i1,r0,x(n3+i),kv,i3)
31   continue
  end do
  do i = 1,n
     h(i,ii) = 0.0D0
  end do
  lt(n+ii) = 1
  z = 1.0D0+x(n3+ii)*y

  !  Update vplus and diag(p)

  do i = 1,n
     if (lt(n+i) == 1) goto 33
     alpha = x(n3+i)/z
     h(k+1,i) = alpha
     do j = 1,k
        h(j,i) = h(j,i)-x(nn+j)*alpha
     end do
33   continue
  end do

  do i = 1,n
     if (lt(n+i) == 1) goto 35
     x(n+i) = x(n+i)+x(n3+i)**2/z
35   continue
  end do
  k = k+1
  h(k,ii) = y
  if (y /= 1.0D0) ii = ii+n
  lt(nn+ii) = 0
  lt(k) = ii
  if (k /= n) goto 29

  !  Set up rhs of constraints in basis

40 continue
  do i = 1,n
     li = lt(i)
     if (li > n) goto 42
     x(n+i) = bdl(li)
     goto 41

42   continue
     if (li > nn) goto 43
     x(n+i) = -bdu(li-n)
     goto 41

43   continue
     x(n+i) = d(li-nn)

41   continue
  end do

  !  Calculate position of vertex

  do i = 1,n
     call dotpmc(h(1,i),i1,x(n+1),i1,r0,x(i),n,i0)
  end do

  !  Calculate the constraint residuals, the number of violated
  !  constraints, and the sum of their normals

50 continue
  kv = 0
  do i = 1,n
     x(n+i) = 0.0D0
  end do
  do i = 1,m
     if (lt(nn+i) <= 0) goto 52
     if (i > n) goto 53
     z = x(i)-bdl(i)
     goto 55

53   continue
     if (i > nn) goto 54
     z = bdu(i-n)-x(i-n)
     goto 55

54   continue
     j = i-nn
     call dotpmc(c(1,j),i1,x(1),i1,d(j),z,n,i2)

55   continue
     x(nn+i) = z
     if (z >= 0.0D0) goto 52
     kv = kv+1
     lt(nn+i) = 2
     if (i > n) goto 56
     x(n+i) = x(n+i)+1.0D0
     goto 52

56   continue
     if (i > nn) goto 57
     x(i) = x(i)-1.0D0
     goto 52

57   continue
     do ii = 1,n
        x(n+ii) = x(n+ii)+c(ii,j)
     end do
52   continue
  end do
  if (kv /= 0) goto 63
  goto 1000

  !  Possible directions of search obtainable by removing a
  !  constraint are rows of h,  calculate the optimum direction

63 continue
  z = 0.0D0
  do i = 1,n
     if (lt(nn+lt(i)) == -1) goto 64
     call dotpmc(h(i,1),ih,x(n+1),i1,r0,y,n,i0)
     if (y <= z) goto 64
     z = y
     ii = i
64   continue
  end do
  if (z > 0.0D0) goto 70
  k = 0
  goto 1000

  !  Search for the nearest of the furthest violated constraint
  !  and the nearest nonviolated nonbasic constraint

70 continue
  !+**PJK 17/11/97 D999 reduced to D99
  alpha = 1.0D99
  beta = 0.0D0
  do i = 1,n
     x(n+i) = h(ii,i)
  end do
  do i = 1,m
     if (lt(nn+i) <= 0) goto 72
     if (i > n) goto 73
     z = -x(n+i)
     goto 75

73   continue
     if (i > nn) goto 74
     z = x(i)
     goto 75

74   continue
     jj = i-nn
     call dotpmc(x(n+1),i1,c(1,jj),i1,r0,z,n,i3)

75   continue
     if (lt(nn+i) == 2) goto 76
     if (z <= 0.0D0) goto 72
     z = x(nn+i)/z
     if (z >= alpha) goto 72
     alpha = z
     ial = i
     goto 72

76   continue
     lt(nn+i) = 1
     if (z >= 0.0D0) goto 72
     z = x(nn+i)/z
     if (z <= beta) goto 72
     beta = z
     ib = i
72   continue
  end do

  if (alpha > beta) goto 80
  ib = ial
  beta = alpha

  !  Exchange with the constraint being removed from the basis,
  !  using simplex formula for new h

80 continue
  lt(nn+lt(ii)) = 1
  lt(nn+ib) = 0
  lt(ii) = ib
  if (ib > n) goto 82
  do i = 1,n
     x(nn+i) = h(i,ib)
  end do
  goto 90

82 continue
  ib = ib-n
  if (ib > n) goto 84
  do i = 1,n
     x(nn+i) = -h(i,ib)
  end do
  goto 90

84 continue
  ib = ib-n
  do i = 1,n
     x(n3+i) = c(i,ib)
  end do
  do i = 1,n
     call dotpmc(h(i,1),ih,x(n3+1),i1,r0,x(nn+i),n,i0)
  end do

90 continue
  z = 1.0D0/x(nn+ii)
  do i = 1,n
     x(i) = x(i)+beta*x(n+i)
     if (i /= ii) goto 92
     do j = 1,n
        h(i,j) = h(i,j)*z
     end do
     goto 91

92   continue
     zz = z*x(nn+i)
     do j = 1,n
        h(i,j) = h(i,j)-zz*x(n+j)
     end do
91   continue
  end do

  goto 50

1000 continue

  return
end SUBROUTINE HARWFP
!______________________________________________________________________
SUBROUTINE HYBRD( &
     fcnhyb,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,diag, &
     mode,factor,nprint,info,nfev,fjac,ldfjac,r,lr, &
     qtf,wa1,wa2,wa3,wa4,resdl)

  !  The purpose of HYBRD is to find a zero of a system of
  !  N nonlinear functions in N variables by a modification
  !  of the Powell Hybrid method. The user must provide a
  !  subroutine which calculates the functions. The Jacobian is
  !  then calculated by a forward-difference approximation.
  !
  !  The subroutine statement is
  !
  !  subroutine hybrd(fcnhyb,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,
  !                   diag,mode,factor,nprint,info,nfev,fjac,
  !                   ldfjac,r,lr,qtf,wa1,wa2,wa3,wa4)
  !
  !  where
  !
  !  FCNHYB is the name of the user-supplied subroutine which
  !  calculates the functions. FCNHYB must be declared
  !  in an external statement in the user calling
  !  program, and should be written as follows.
  !
  !   subroutine fcnhyb(n,x,fvec,iflag)
  !   integer n,iflag
  !   real x(n),fvec(n)
  !   ----------
  !   calculate the functions at x and
  !   return this vector in fvec.
  !   ---------
  !   return
  !   end
  !
  !  The value of IFLAG should not be changed by FCNHYB unless
  !  the user wants to terminate execution of HYBRD.
  !  In this case set IFLAG to a negative integer.
  !
  !  N is a positive integer input variable set to the number
  !  of functions and variables.
  !
  !  X is an array of length N. On input X must contain
  !  an initial estimate of the solution vector. On output X
  !  contains the final estimate of the solution vector.
  !
  !  FVEC is an output array of length N which contains
  !  the functions evaluated at the output X.
  !
  !  XTOL is a nonnegative input variable. Termination
  !  occurs when the relative error between two consecutive
  !  iterations is at most XTOL.
  !
  !  MAXFEV is a positive integer input variable. Termination
  !  occurs when the number of calls to FCNHYB is at least MAXFEV
  !  by the end of an iteration.
  !
  !  ML is a nonnegative integer input variable which specifies
  !  the number of subdiagonals within the band of the
  !  Jacobian matrix. If the Jacobian is not banded, set
  !  ML to at least N - 1.
  !
  !  MU is a nonnegative integer input variable which specifies
  !  the number of superdiagonals within the band of the
  !  Jacobian matrix. If the Jacobian is not banded, set
  !  MU to at least N - 1.
  !
  !  EPSFCN is an input variable used in determining a suitable
  !  step length for the forward-difference approximation. This
  !  approximation assumes that the relative errors in the
  !  functions are of the order of EPSFCN. If EPSFCN is less
  !  than the machine precision, it is assumed that the relative
  !  errors in the functions are of the order of the machine
  !  precision.
  !
  !  DIAG is an array of length N. If MODE = 1 (see
  !  below), DIAG is internally set. If MODE = 2, DIAG
  !  must contain positive entries that serve as
  !  multiplicative scale factors for the variables.
  !
  !  MODE is an integer input variable. If MODE = 1, the
  !  variables will be scaled internally. If MODE = 2,
  !  the scaling is specified by the input DIAG. Other
  !  values of MODE are equivalent to MODE = 1.
  !
  !  FACTOR is a positive input variable used in determining the
  !  initial step bound. This bound is set to the product of
  !  FACTOR and the Euclidean norm of DIAG*X if nonzero, or else
  !  to FACTOR itself. In most cases FACTOR should lie in the
  !  interval (.1,100.). 100. is a generally recommended value.
  !
  !  NPRINT is an integer input variable that enables controlled
  !  printing of iterations if it is positive. In this case,
  !  FCNHYB is called with IFLAG = 0 at the beginning of the first
  !  iteration and every NPRINT iterations thereafter and
  !  immediately prior to return, with X and FVEC available
  !  for printing. If NPRINT is not positive, no special calls
  !  of FCNHYB with IFLAG = 0 are made.
  !
  !  INFO is an integer output variable. If the user has
  !  terminated execution, INFO is set to the (negative)
  !  value of IFLAG. see description of FCNHYB. Otherwise,
  !  INFO is set as follows.
  !
  !   INFO = 0   improper input parameters.
  !
  !   INFO = 1   relative error between two consecutive iterates
  !              is at most XTOL.
  !
  !   INFO = 2   number of calls to FCNHYB has reached or exceeded
  !              MAXFEV.
  !
  !   INFO = 3   XTOL is too small. No further improvement in
  !              the approximate solution X is possible.
  !
  !   INFO = 4   iteration is not making good progress, as
  !              measured by the improvement from the last
  !              five Jacobian evaluations.
  !
  !   INFO = 5   iteration is not making good progress, as
  !              measured by the improvement from the last
  !              ten iterations.
  !
  !  NFEV is an integer output variable set to the number of
  !  calls to FCNHYB.
  !
  !  FJAC is an output N by N array which contains the
  !  orthogonal matrix Q produced by the QR factorization
  !  of the final approximate Jacobian.
  !
  !  LDFJAC is a positive integer input variable not less than N
  !  which specifies the leading dimension of the array FJAC.
  !
  !  R is an output array of length LR which contains the
  !  upper triangular matrix produced by the QR factorization
  !  of the final approximate Jacobian, stored rowwise.
  !
  !  LR is a positive integer input variable not less than
  !  (N*(N+1))/2.
  !
  !  QTF is an output array of length N which contains
  !  the vector (Q transpose)*FVEC.
  !
  !  WA1, WA2, WA3, and WA4 are work arrays of length N.
  !
  !  Subprograms called
  !
  !   user-supplied ...... fcnhyb
  !
  !   minpack-supplied ... dogleg,spmpar,enorm,fdjac1,
  !                        qform,qrfac,r1mpyq,r1updt
  !
  !  Argonne National Laboratory. Minpack project. March 1980.
  !  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

  IMPLICIT NONE

  INTEGER n,maxfev,ml,mu,mode,nprint,info,nfev,ldfjac,lr,irr
  INTEGER i,iflag,iter,j,jm1,l,msum,ncfail,ncsuc,nslow1,nslow2

  !+**PJK 08/10/92 Possible problems with the following declaration:
  INTEGER iwa(1)

  real(kind(1.0D0)) xtol,epsfcn,factor
  real(kind(1.0D0)) x(n),fvec(n),diag(n),fjac(ldfjac,n),r(lr), &
       qtf(n),wa1(n),wa2(n),wa3(n),wa4(n),resdl(n)
  real(kind(1.0D0)) actred,delta,epsmch,fnorm,fnorm1,one,pnorm, &
       prered,p1,p5,p001,p0001,ratio,sum,temp,xnorm,zero
  logical jeval,sing

  real(kind(1.0D0)) spmpar,enorm
  EXTERNAL     fcnhyb,spmpar,enorm

  one = 1.0D0
  p1 = 0.1D0
  p5 = 0.5D0
  p001 = 1.0D-3
  p0001 = 1.0D-4
  zero = 0.0D0

  !  Machine precision

  epsmch = spmpar(1)

  info = 0
  iflag = 0
  nfev = 0

  !  Check the input parameters for errors.

  if ( &
       (n <= 0)         .or. &
       (xtol < zero)   .or. &
       (maxfev <= 0)    .or. &
       (ml < 0)        .or. &
       (mu < 0)        .or. &
       (factor <= zero) .or. &
       (ldfjac < n)    .or. &
       (lr < ( ( n*(n + 1) ) /2)) &
       ) goto 300

  if (mode  /=  2) goto 20
  do j = 1, n
     if (diag(j) <= zero) goto 300
  end do

20 continue

  !  Evaluate the function at the starting point
  !  and calculate its norm.

  iflag = 1
  call fcnhyb(n,x,fvec,iflag)
  nfev = 1

  if (iflag < 0) goto 300
  fnorm = enorm(n,fvec)

  !  Determine the number of calls to FCNHYB needed to compute
  !  the Jacobian matrix.

  msum = min(ml+mu+1,n)

  !  Initialize iteration counter and monitors.

  iter = 1
  ncsuc = 0
  ncfail = 0
  nslow1 = 0
  nslow2 = 0

  !  Beginning of the outer loop.

30 continue
  jeval = .true.

  !  Calculate the Jacobian matrix.

  iflag = 2
  call fdjac1( &
       fcnhyb,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,wa1,wa2)
  nfev = nfev + msum
  if (iflag < 0) goto 300

  !  Compute the qr factorization of the Jacobian.

  call qrfac(n,n,fjac,ldfjac,.false.,iwa,1,wa1,wa2,wa3)

  !  On the first iteration and if mode is 1, scale according
  !  to the norms of the columns of the initial Jacobian.

  if (iter  /=  1) goto 70
  if (mode == 2) goto 50
  do j = 1, n
     diag(j) = wa2(j)
     if (wa2(j) == zero) diag(j) = one
  end do

50 continue

  !  On the first iteration, calculate the norm of the scaled x
  !  and initialize the step bound delta.

  do j = 1, n
     wa3(j) = diag(j)*x(j)
  end do
  xnorm = enorm(n,wa3)
  delta = factor*xnorm
  if (delta == zero) delta = factor

70 continue

  !  Form (q transpose)*fvec and store in qtf.

  do i = 1, n
     qtf(i) = fvec(i)
  end do
  do j = 1, n
     if (fjac(j,j) == zero) goto 110
     sum = zero
     do i = j, n
        sum = sum + fjac(i,j)*qtf(i)
     end do
     temp = -sum/fjac(j,j)
     do i = j, n
        qtf(i) = qtf(i) + fjac(i,j)*temp
     end do

110  continue
  end do

  !  Copy the triangular factor of the qr factorization into r.

  sing = .false.
  do j = 1, n
     l = j
     jm1 = j - 1
     if (jm1 < 1) goto 140
     do i = 1, jm1
        r(l) = fjac(i,j)
        l = l + n - i
     end do

140  continue
     r(l) = wa1(j)
     if (wa1(j) == zero) sing = .true.
  end do

  !  Accumulate the orthogonal factor in fjac.

  call qform(n,n,fjac,ldfjac,wa1)

  !  Rescale if necessary.

  if (mode == 2) goto 170
  do j = 1, n
     diag(j) = max(diag(j),wa2(j))
  end do

170 continue

  !  Beginning of the inner loop.

180 continue

  !  If requested, call FCNHYB to enable printing of iterates.

  if (nprint <= 0) goto 190
  iflag = 0
  if (mod(iter-1,nprint) == 0) call fcnhyb(n,x,fvec,iflag)
  if (iflag < 0) goto 300

190 continue

  !  Determine the direction p.

  call dogleg(n,r,lr,diag,qtf,delta,wa1,wa2,wa3)

  !  Store the direction p and x + p. Calculate the norm of p.

  do j = 1, n
     wa1(j) = -wa1(j)
     wa2(j) = x(j) + wa1(j)
     wa3(j) = diag(j)*wa1(j)
  end do
  pnorm = enorm(n,wa3)

  !  On the first iteration, adjust the initial step bound.

  if (iter == 1) delta = min(delta,pnorm)

  !  Evaluate the function at x + p and calculate its norm.

  iflag = 1
  call fcnhyb(n,wa2,wa4,iflag)
  nfev = nfev + 1
  if (iflag < 0) goto 300
  fnorm1 = enorm(n,wa4)

  !  Compute the scaled actual reduction.

  actred = -one
  if (fnorm1 < fnorm) actred = one - (fnorm1/fnorm)**2

  !  Compute the scaled predicted reduction.

  l = 1
  do i = 1, n
     sum = zero
     do j = i, n
        sum = sum + r(l)*wa1(j)
        l = l + 1
     end do
     wa3(i) = qtf(i) + sum
  end do
  temp = enorm(n,wa3)
  prered = zero
  if (temp < fnorm) prered = one - (temp/fnorm)**2

  !  Compute the ratio of the actual to the predicted reduction.

  ratio = zero
  if (prered > zero) ratio = actred/prered

  !  Update the step bound.

  if (ratio >= p1) goto 230
  ncsuc = 0
  ncfail = ncfail + 1
  delta = p5*delta
  goto 240
230 continue
  ncfail = 0
  ncsuc = ncsuc + 1
  if (ratio >= p5 .or. ncsuc > 1) &
       delta = max(delta,pnorm/p5)
  if (abs(ratio-one) <= p1) delta = pnorm/p5
240 continue

  !  Test for successful iteration.

  if (ratio < p0001) goto 260

  !  Successful iteration. Update x, fvec, and their norms.

  do j = 1, n
     x(j) = wa2(j)
     wa2(j) = diag(j)*x(j)
     fvec(j) = wa4(j)
  end do
  xnorm = enorm(n,wa2)
  fnorm = fnorm1
  iter = iter + 1
260 continue

  !  Determine the progress of the iteration.

  nslow1 = nslow1 + 1
  if (actred >= p001) nslow1 = 0
  if (jeval) nslow2 = nslow2 + 1
  if (actred >= p1) nslow2 = 0

  !  Test for convergence.

  if ((delta <= (xtol*xnorm)) .or. (fnorm == zero)) info = 1
  if (info  /=  0) goto 300

  !  Tests for termination and stringent tolerances.

  if (nfev >= maxfev) info = 2
  if ((p1*max(p1*delta,pnorm)) <= (epsmch*xnorm)) info = 3
  if (nslow2 == 5) info = 4
  if (nslow1 == 10) info = 5
  if (info  /=  0) goto 300

  !  Criterion for recalculating Jacobian approximation
  !  by forward differences.

  if (ncfail == 2) goto 290

  !  Calculate the rank one modification to the Jacobian
  !  and update qtf if necessary.

  do j = 1, n
     sum = zero
     do i = 1, n
        sum = sum + fjac(i,j)*wa4(i)
     end do
     wa2(j) = (sum - wa3(j))/pnorm
     wa1(j) = diag(j)*((diag(j)*wa1(j))/pnorm)
     if (ratio >= p0001) qtf(j) = sum
  end do

  !  Compute the qr factorization of the updated Jacobian.

  call r1updt(n,n,r,lr,wa1,wa2,wa3,sing)
  call r1mpyq(n,n,fjac,ldfjac,wa2,wa3)

  !+**PJK 02/11/92 Warning produced by QA Fortran :
  !+**PJK 02/11/92 Arg 3 in call to R1MPYQ has wrong dimensions.
  !+**PJK 02/11/92 Code works at present, but beware of future
  !+**PJK 02/11/92 modifications.

  call r1mpyq(1,n,qtf,1,wa2,wa3)

  !  End of the inner loop.

  jeval = .false.
  goto 180

290 continue

  !  End of the outer loop.

  goto 30

300 continue

  !  Termination, either normal or user imposed.

  if (iflag < 0) info = iflag
  iflag = 0
  if (nprint > 0) call fcnhyb(n,x,fvec,iflag)

  do irr=1,n
     resdl(irr)=abs(qtf(irr))
  end do

  return
end SUBROUTINE HYBRD
!______________________________________________________________________
SUBROUTINE DOGLEG(n,r,lr,diag,qtb,delta,x,wa1,wa2)

  !  Given an M by N matrix A, an N by N nonsingular diagonal
  !  matrix D, an M-vector B, and a positive number DELTA, the
  !  problem is to determine the convex combination X of the
  !  Gauss-Newton and scaled gradient directions that minimizes
  !  (A*X - B) in the least squares sense, subject to the
  !  restriction that the Euclidean norm of D*X be at most DELTA.
  !
  !  This subroutine completes the solution of the problem
  !  if it is provided with the necessary information from the
  !  QR factorization of A. That is, if A = Q*R, where Q has
  !  orthogonal columns and R is an upper triangular matrix,
  !  then DOGLEG expects the full upper triangle of R and
  !  the first N components of (Q transpose)*B.
  !
  !  The subroutine statement is
  !
  !  subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)
  !
  !  where
  !
  !  N is a positive integer input variable set to the order of R.
  !
  !  R is an input array of length LR which must contain the upper
  !  triangular matrix R stored by rows.
  !
  !  LR is a positive integer input variable not less than
  !  (N*(N+1))/2.
  !
  !  DIAG is an input array of length N which must contain the
  !  diagonal elements of the matrix D.
  !
  !  QTB is an input array of length N which must contain the first
  !  N elements of the vector (Q transpose)*B.
  !
  !  DELTA is a positive input variable which specifies an upper
  !  bound on the Euclidean norm of D*X.
  !
  !  X is an output array of length N which contains the desired
  !  convex combination of the Gauss-Newton direction and the
  !  scaled gradient direction.
  !
  !  WA1 and WA2 are work arrays of length N.
  !
  !  Argonne National Laboratory. Minpack project. March 1980.
  !  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

  IMPLICIT NONE

  INTEGER n,lr,i,j,jj,jp1,k,l

  real(kind(1.0D0)) delta
  real(kind(1.0D0)) r(lr),diag(n),qtb(n),x(n),wa1(n),wa2(n)
  real(kind(1.0D0)) alpha,bnorm,epsmch,gnorm,one,qnorm,sgnorm, &
       sum,temp,zero

  real(kind(1.0D0)) spmpar,enorm
  EXTERNAL         spmpar,enorm

  one = 1.0D0
  zero = 0.0D0

  !  Machine precision

  epsmch = spmpar(1)

  !  First, calculate the Gauss-Newton direction.

  jj = (n*(n + 1))/2 + 1
  do k = 1, n
     j = n - k + 1
     jp1 = j + 1
     jj = jj - k
     l = jj + 1
     sum = zero
     if (n < jp1) goto 20
     do i = jp1, n
        sum = sum + r(l)*x(i)
        l = l + 1
     end do

20   continue
     temp = r(jj)
     if (temp  /=  zero) goto 40
     l = j
     do i = 1, j
        temp = max(temp,abs(r(l)))
        l = l + n - i
     end do
     temp = epsmch*temp
     if (temp == zero) temp = epsmch

40   continue
     x(j) = (qtb(j) - sum)/temp
  end do

  !  Test whether the Gauss-Newton direction is acceptable.

  do j = 1, n
     wa1(j) = zero
     wa2(j) = diag(j)*x(j)
  end do
  qnorm = enorm(n,wa2)
  if (qnorm <= delta) goto 140

  !  The Gauss-Newton direction is not acceptable.
  !  Next, calculate the scaled gradient direction.

  l = 1
  do j = 1, n
     temp = qtb(j)
     do i = j, n
        wa1(i) = wa1(i) + r(l)*temp
        l = l + 1
     end do
     wa1(j) = wa1(j)/diag(j)
  end do

  !  Calculate the norm of the scaled gradient and test for
  !  the special case in which the scaled gradient is zero.

  gnorm = enorm(n,wa1)
  sgnorm = zero
  alpha = delta/qnorm
  if (gnorm == zero) goto 120

  !  Calculate the point along the scaled gradient
  !  at which the quadratic is minimized.

  do j = 1, n
     wa1(j) = (wa1(j)/gnorm)/diag(j)
  end do
  l = 1
  do j = 1, n
     sum = zero
     do i = j, n
        sum = sum + r(l)*wa1(i)
        l = l + 1
     end do
     wa2(j) = sum
  end do
  temp = enorm(n,wa2)
  sgnorm = (gnorm/temp)/temp

  !  Test whether the scaled gradient direction is acceptable.

  alpha = zero
  if (sgnorm >= delta) goto 120

  !  The scaled gradient direction is not acceptable.
  !  Finally, calculate the point along the dogleg
  !  at which the quadratic is minimized.

  bnorm = enorm(n,qtb)
  temp = (bnorm/gnorm)*(bnorm/qnorm)*(sgnorm/delta)
  temp = temp - (delta/qnorm)*(sgnorm/delta)**2 &
       + sqrt((temp-(delta/qnorm))**2 &
       + (one-(delta/qnorm)**2)*(one-(sgnorm/delta)**2))
  alpha = ((delta/qnorm)*(one - (sgnorm/delta)**2))/temp

120 continue

  !  Form appropriate convex combination of the Gauss-Newton
  !  direction and the scaled gradient direction.

  temp = (one - alpha)*min(sgnorm,delta)
  do j = 1, n
     x(j) = temp*wa1(j) + alpha*x(j)
  end do

140 continue

  return
end SUBROUTINE DOGLEG
!______________________________________________________________________
real(kind(1.0D0)) FUNCTION ENORM(n,x)

  !  Given an N-vector X, this function calculates the
  !  Euclidean norm of X.
  !
  !  The Euclidean norm is computed by accumulating the sum of
  !  squares in three different sums. The sums of squares for the
  !  small and large components are scaled so that no overflows
  !  occur. Non-destructive underflows are permitted. Underflows
  !  and overflows do not occur in the computation of the unscaled
  !  sum of squares for the intermediate components.
  !  The definitions of small, intermediate and large components
  !  depend on two constants, RDWARF and RGIANT. The main
  !  restrictions on these constants are that RDWARF**2 not
  !  underflow and RGIANT**2 not overflow. The constants
  !  given here are suitable for every known computer.
  !
  !  The function statement is
  !
  !  real function enorm(n,x)
  !
  !  where
  !
  !  N is a positive integer input variable.
  !
  !  X is an input array of length N.
  !
  !  Argonne National Laboratory. Minpack project. March 1980.
  !  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

  IMPLICIT NONE

  INTEGER n,i

  real(kind(1.0D0)) x(n)
  real(kind(1.0D0)) agiant,floatn,one,rdwarf,rgiant,s1,s2,s3,xabs, &
       x1max,x3max,zero

  one = 1.0D0
  zero = 0.0D0
  rdwarf = 3.834d-20
  rgiant = 1.304d19

  s1 = zero
  s2 = zero
  s3 = zero
  x1max = zero
  x3max = zero
  floatn = dble(n)
  agiant = rgiant/floatn
  do i = 1, n
     xabs = abs(x(i))
     if ((xabs > rdwarf) .and. (xabs < agiant)) goto 70
     if (xabs <= rdwarf) goto 30

     !  Sum for large components.

     if (xabs <= x1max) goto 10
     s1 = one + s1*(x1max/xabs)**2
     x1max = xabs
     goto 20

10   continue
     s1 = s1 + (xabs/x1max)**2

20   continue
     goto 60

30   continue

     !  Sum for small components.

     if (xabs <= x3max) goto 40
     s3 = one + s3*(x3max/xabs)**2
     x3max = xabs
     goto 50

40   continue
     if (xabs  /=  zero) s3 = s3 + (xabs/x3max)**2

50   continue
60   continue
     goto 80

70   continue

     !  Sum for intermediate components.

     s2 = s2 + xabs**2

80   continue
  end do

  !  Calculation of norm.

  if (s1 == zero) goto 100
  enorm = x1max*sqrt(s1+(s2/x1max)/x1max)
  goto 130

100 continue
  if (s2 == zero) goto 110
  if (s2 >= x3max) &
       enorm = sqrt(s2*(one+(x3max/s2)*(x3max*s3)))
  if (s2 < x3max) &
       enorm = sqrt(x3max*((s2/x3max)+(x3max*s3)))
  goto 120
110 continue
  enorm = x3max*sqrt(s3)

120 continue
130 continue

  return
end FUNCTION ENORM
!______________________________________________________________________
SUBROUTINE FDJAC1( &
     fcnhyb,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,wa1,wa2)

  !  This subroutine computes a forward-difference approximation
  !  to the N by N Jacobian matrix associated with a specified
  !  problem of N functions in N variables. If the Jacobian has
  !  a banded form, then function evaluations are saved by only
  !  approximating the nonzero terms.
  !
  !  The subroutine statement is
  !
  !  subroutine fdjac1(fcnhyb,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,
  !                    wa1,wa2)
  !
  !  where
  !
  !  FCNHYB is the name of the user-supplied subroutine which
  !  calculates the functions. FCNHYB must be declared
  !  in an external statement in the user calling
  !  program, and should be written as follows.
  !
  !   subroutine fcnhyb(n,x,fvec,iflag)
  !   integer n,iflag
  !   real x(n),fvec(n)
  !   ----------
  !   calculate the functions at x and
  !   return this vector in fvec.
  !   ----------
  !   return
  !   end
  !
  !  The value of IFLAG should not be changed by FCNHYB unless
  !  the user wants to terminate execution of FDJAC1.
  !  In this case set IFLAG to a negative integer.
  !
  !  N is a positive integer input variable set to the number
  !  of functions and variables.
  !
  !  X is an input array of length N.
  !
  !  FVEC is an input array of length N which must contain the
  !  functions evaluated at X.
  !
  !  FJAC is an output N by N array which contains the
  !  approximation to the Jacobian matrix evaluated at X.
  !
  !  LDFJAC is a positive integer input variable not less than N
  !  which specifies the leading dimension of the array FJAC.
  !
  !  IFLAG is an integer variable which can be used to terminate
  !  the execution of FDJAC1. See description of FCNHYB.
  !
  !  ML is a nonnegative integer input variable which specifies
  !  the number of subdiagonals within the band of the
  !  Jacobian matrix. If the Jacobian is not banded, set
  !  ML to at least N - 1.
  !
  !  EPSFCN is an input variable used in determining a suitable
  !  step length for the forward-difference approximation. This
  !  approximation assumes that the relative errors in the
  !  functions are of the order of EPSFCN. If EPSFCN is less
  !  than the machine precision, it is assumed that the relative
  !  errors in the functions are of the order of the machine
  !  precision.
  !
  !  MU is a nonnegative integer input variable which specifies
  !  the number of superdiagonals within the band of the
  !  Jacobian matrix. If the Jacobian is not banded, set
  !  MU to at least N - 1.
  !
  !  WA1 and WA2 are work arrays of length N. If ML + MU + 1 is at
  !  least N, then the Jacobian is considered dense, and WA2 is
  !  not referenced.
  !
  !  Argonne National Laboratory. Minpack project. March 1980.
  !  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

  IMPLICIT NONE

  INTEGER n,ldfjac,iflag,ml,mu,i,j,k,msum

  real(kind(1.0D0)) epsfcn
  real(kind(1.0D0)) x(n),fvec(n),fjac(ldfjac,n),wa1(n),wa2(n)
  real(kind(1.0D0)) eps,epsmch,h,temp,zero

  real(kind(1.0D0)) spmpar
  EXTERNAL  fcnhyb,spmpar

  zero = 0.0D0

  !  Machine precision

  epsmch = spmpar(1)

  eps = sqrt(max(epsfcn,epsmch))
  msum = ml + mu + 1
  if (msum < n) goto 40

  !  Computation of dense approximate Jacobian.

  do j = 1, n
     temp = x(j)
     h = eps*abs(temp)
     if (h == zero) h = eps
     x(j) = temp + h
     call fcnhyb(n,x,wa1,iflag)
     if (iflag < 0) goto 30
     x(j) = temp
     do i = 1, n
        fjac(i,j) = (wa1(i) - fvec(i))/h
     end do
  end do

30 continue
  goto 110

40 continue

  !  Computation of banded approximate Jacobian.

  do k = 1, msum
     do j = k, n, msum
        wa2(j) = x(j)
        h = eps*abs(wa2(j))
        if (h == zero) h = eps
        x(j) = wa2(j) + h
     end do
     call fcnhyb(n,x,wa1,iflag)
     if (iflag < 0) goto 100
     do j = k, n, msum
        x(j) = wa2(j)
        h = eps*abs(wa2(j))
        if (h == zero) h = eps
        do i = 1, n
           fjac(i,j) = zero
           if ((i >= (j - mu)).and.(i <= (j + ml))) &
                fjac(i,j) = (wa1(i) - fvec(i))/h
        end do
     end do
  end do

100 continue
110 continue

  return
end SUBROUTINE FDJAC1
!______________________________________________________________________
SUBROUTINE QFORM(m,n,q,ldq,wa)

  !  This subroutine proceeds from the computed QR factorization of
  !  an M by N matrix A to accumulate the M by M orthogonal matrix
  !  Q from its factored form.
  !
  !  The subroutine statement is
  !
  !  subroutine qform(m,n,q,ldq,wa)
  !
  !  where
  !
  !  M is a positive integer input variable set to the number
  !  of rows of A and the order of Q.
  !
  !  N is a positive integer input variable set to the number
  !  of columns of A.
  !
  !  Q is an M by M array. On input the full lower trapezoid in
  !  the first min(M,N) columns of Q contains the factored form.
  !  On output Q has been accumulated into a square matrix.
  !
  !  LDQ is a positive integer input variable not less than M
  !  which specifies the leading dimension of the array Q.
  !
  !  WA is a work array of length M.

  !  Argonne National Laboratory. Minpack project. March 1980.
  !  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

  IMPLICIT NONE

  INTEGER m,n,ldq,i,j,jm1,k,l,minmn,np1

  real(kind(1.0D0)) q(ldq,m),wa(m)
  real(kind(1.0D0)) one,sum,temp,zero

  one = 1.0D0
  zero = 0.0D0

  !  Zero out upper triangle of q in the first min(m,n) columns.

  minmn = min(m,n)
  if (minmn < 2) goto 30
  do j = 2, minmn
     jm1 = j - 1
     do i = 1, jm1
        q(i,j) = zero
     end do
  end do

30 continue

  !  Initialize remaining columns to those of the identity matrix.

  np1 = n + 1
  if (m < np1) goto 60
  do j = np1, m
     do i = 1, m
        q(i,j) = zero
     end do
     q(j,j) = one
  end do

60 continue

  !  Accumulate q from its factored form.

  do l = 1, minmn
     k = minmn - l + 1
     do i = k, m
        wa(i) = q(i,k)
        q(i,k) = zero
     end do
     q(k,k) = one
     if (wa(k) == zero) goto 110
     do j = k, m
        sum = zero
        do i = k, m
           sum = sum + q(i,j)*wa(i)
        end do
        temp = sum/wa(k)
        do i = k, m
           q(i,j) = q(i,j) - temp*wa(i)
        end do
     end do

110  continue
  end do

  return
end SUBROUTINE QFORM
!______________________________________________________________________
SUBROUTINE QRFAC(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)

  !  This subroutine uses householder transformations with column
  !  pivoting (optional) to compute a QR factorization of the
  !  M by N matrix A. That is, QRFAC determines an orthogonal
  !  matrix Q, a permutation matrix P, and an upper trapezoidal
  !  matrix R with diagonal elements of nonincreasing magnitude,
  !  such that A*P = Q*R. The householder transformation for
  !  column K, K = 1,2,...,min(M,N), is of the form
  !
  !  i - (1/u(k))*u*u
  !
  !  where U has zeros in the first K-1 positions. The form of
  !  this transformation and the method of pivoting first
  !  appeared in the corresponding Linpack subroutine.
  !
  !  The subroutine statement is
  !
  !  subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
  !
  !  where
  !
  !  M is a positive integer input variable set to the number
  !  of rows of A.
  !
  !  N is a positive integer input variable set to the number
  !  of columns of A.
  !
  !  A is an M by N array. On input A contains the matrix for
  !  which the QR factorization is to be computed. On output
  !  the strict upper trapezoidal part of A contains the strict
  !  upper trapezoidal part of R, and the lower trapezoidal
  !  part of A contains a factored form of Q (the non-trivial
  !  elements of the U vectors described above).
  !
  !  LDA is a positive integer input variable not less than M
  !  which specifies the leading dimension of the array A.
  !
  !  PIVOT is a logical input variable. If PIVOT is set true,
  !  then column pivoting is enforced. If PIVOT is set false,
  !  then no column pivoting is done.
  !
  !  IPVT is an integer output array of length LIPVT. IPVT
  !  defines the permutation matrix P such that A*P = Q*R.
  !  Column J of P is column IPVT(J) of the identity matrix.
  !  If PIVOT is false, IPVT is not referenced.
  !
  !  LIPVT is a positive integer input variable. If PIVOT is false,
  !  then LIPVT may be as small as 1. If PIVOT is true, then
  !  LIPVT must be at least N.
  !
  !  RDIAG is an output array of length N which contains the
  !  diagonal elements of R.
  !
  !  ACNORM is an output array of length N which contains the
  !  norms of the corresponding columns of the input matrix A.
  !  If this information is not needed, then ACNORM can coincide
  !  with RDIAG.
  !
  !  WA is a work array of length N. If PIVOT is false, then WA
  !  can coincide with RDIAG.
  !
  !  Argonne National Laboratory. Minpack project. March 1980.
  !  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

  IMPLICIT NONE

  INTEGER m,n,lda,lipvt,i,j,jp1,k,kmax,minmn
  INTEGER ipvt(lipvt)

  LOGICAL pivot

  real(kind(1.0D0)) a(lda,n),rdiag(n),acnorm(n),wa(n)
  real(kind(1.0D0)) ajnorm,epsmch,one,p05,sum,temp,zero

  real(kind(1.0D0)) spmpar,enorm
  EXTERNAL         spmpar,enorm

  one = 1.0D0
  p05 = 0.05D0
  zero = 0.0D0

  !  Machine precision

  epsmch = spmpar(1)

  !  Compute the initial column norms and initialize several arrays.

  do j = 1, n
     acnorm(j) = enorm(m,a(1,j))
     rdiag(j) = acnorm(j)
     wa(j) = rdiag(j)
     if (pivot) ipvt(j) = j
  end do

  !  Reduce a to r with householder transformations.

  minmn = min(m,n)
  do j = 1, minmn
     if (.not.pivot) goto 40

     !  Bring the column of largest norm into the pivot position.

     kmax = j
     do k = j, n
        if (rdiag(k) > rdiag(kmax)) kmax = k
     end do
     if (kmax == j) goto 40
     do i = 1, m
        temp = a(i,j)
        a(i,j) = a(i,kmax)
        a(i,kmax) = temp
     end do
     rdiag(kmax) = rdiag(j)
     wa(kmax) = wa(j)
     k = ipvt(j)
     ipvt(j) = ipvt(kmax)
     ipvt(kmax) = k

40   continue

     !  Compute the householder transformation to reduce the
     !  j-th column of a to a multiple of the j-th unit vector.

     ajnorm = enorm(m-j+1,a(j,j))
     if (ajnorm == zero) goto 100
     if (a(j,j) < zero) ajnorm = -ajnorm
     do i = j, m
        a(i,j) = a(i,j)/ajnorm
     end do
     a(j,j) = a(j,j) + one

     !  Apply the transformation to the remaining columns
     !  and update the norms.

     jp1 = j + 1
     if (n < jp1) goto 100
     do k = jp1, n
        sum = zero
        do i = j, m
           sum = sum + a(i,j)*a(i,k)
        end do
        temp = sum/a(j,j)
        do i = j, m
           a(i,k) = a(i,k) - temp*a(i,j)
        end do
        if ((.not.pivot).or.(rdiag(k) == zero)) goto 80
        temp = a(j,k)/rdiag(k)
        rdiag(k) = rdiag(k)*sqrt(max(zero,one-temp**2))
        if ((p05*(rdiag(k)/wa(k))**2) > epsmch) goto 80
        rdiag(k) = enorm(m-j,a(jp1,k))
        wa(k) = rdiag(k)

80      continue
     end do

100  continue
     rdiag(j) = -ajnorm
  end do

  return
end SUBROUTINE QRFAC
!______________________________________________________________________
SUBROUTINE R1MPYQ(m,n,a,lda,v,w)

  !  Given an M by N matrix A, this subroutine computes A*Q where
  !  Q is the product of 2*(N - 1) transformations
  !
  !  gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
  !
  !  and GV(I), GW(i) are Givens rotations in the (I,N) plane which
  !  eliminate elements in the I-th and N-th planes, respectively.
  !  Q itself is not given, rather the information to recover the
  !  GV, GW rotations is supplied.
  !
  !  The subroutine statement is
  !
  !  subroutine r1mpyq(m,n,a,lda,v,w)
  !
  !  where
  !
  !  M is a positive integer input variable set to the number
  !  of rows of A.
  !
  !  N is a positive integer input variable set to the number
  !  of columns of A.
  !
  !  A is an M by N array. On input A must contain the matrix
  !  to be postmultiplied by the orthogonal matrix Q
  !  described above. On output A*Q has replaced A.
  !
  !  LDA is a positive integer input variable not less than M
  !  which specifies the leading dimension of the array A.
  !
  !  V is an input array of length N. V(I) must contain the
  !  information necessary to recover the Givens rotation GV(I)
  !  described above.
  !
  !  W is an input array of length N. W(I) must contain the
  !  information necessary to recover the Givens rotation GW(I)
  !  described above.
  !
  !  Argonne National Laboratory. Minpack project. March 1980.
  !  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

  IMPLICIT NONE

  INTEGER m,n,lda,i,j,nmj,nm1

  real(kind(1.0D0)) a(lda,n),v(n),w(n)
  real(kind(1.0D0)) cos1,one,sin1,temp

  one = 1.0D0

  !  Apply the first set of givens rotations to a.

  nm1 = n - 1
  if (nm1 < 1) goto 50
  do nmj = 1, nm1
     j = n - nmj
     if (abs(v(j)) > one) cos1 = one/v(j)
     if (abs(v(j)) > one) sin1 = sqrt(one-cos1**2)
     if (abs(v(j)) <= one) sin1 = v(j)
     if (abs(v(j)) <= one) cos1 = sqrt(one-sin1**2)
     do i = 1, m
        temp  = cos1*a(i,j) - sin1*a(i,n)
        a(i,n) = sin1*a(i,j) + cos1*a(i,n)
        a(i,j) = temp
     end do
  end do

  !  Apply the second set of givens rotations to a.

  do j = 1, nm1
     if (abs(w(j)) > one) cos1 = one/w(j)
     if (abs(w(j)) > one) sin1 = sqrt(one-cos1**2)
     if (abs(w(j)) <= one) sin1 = w(j)
     if (abs(w(j)) <= one) cos1 = sqrt(one-sin1**2)
     do i = 1, m
        temp   =  cos1*a(i,j) + sin1*a(i,n)
        a(i,n) = -sin1*a(i,j) + cos1*a(i,n)
        a(i,j) = temp
     end do
  end do

50 continue

  return
end SUBROUTINE R1MPYQ
!______________________________________________________________________
SUBROUTINE R1UPDT(m,n,s,ls,u,v,w,sing)

  !  Given an M by N lower trapezoidal matrix S, an M-vector U,
  !  and an N-vector V, the problem is to determine an
  !  orthogonal matrix Q such that
  !
  !          t
  !  (s + u*v )*q
  !
  !  is again lower trapezoidal.
  !
  !  This subroutine determines Q as the product of 2*(N - 1)
  !  transformations
  !
  !  gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
  !
  !  where GV(I), GW(I) are Givens rotations in the (I,N) plane
  !  which eliminate elements in the I-th and N-th planes,
  !  respectively. Q itself is not accumulated, rather the
  !  information to recover the GV, GW rotations is returned.
  !
  !  The subroutine statement is
  !
  !  subroutine r1updt(m,n,s,ls,u,v,w,sing)
  !
  !  where
  !
  !  M is a positive integer input variable set to the number
  !  of rows of S.
  !
  !  N is a positive integer input variable set to the number
  !  of columns of S. N must not exceed M.
  !
  !  S is an array of length LS. On input S must contain the lower
  !  trapezoidal matrix S stored by columns. On output S contains
  !  the lower trapezoidal matrix produced as described above.
  !
  !  LS is a positive integer input variable not less than
  !  (N*(2*M-N+1))/2.
  !
  !  U is an input array of length M which must contain the
  !  vector U.
  !
  !  V is an array of length N. On input V must contain the vector
  !  V. On output V(I) contains the information necessary to
  !  recover the Givens rotation GV(I) described above.
  !
  !  W is an output array of length M. W(I) contains information
  !  necessary to recover the Givens rotation GW(I) described
  !  above.
  !
  !  SING is a logical output variable. SING is set true if any
  !  of the diagonal elements of the output S are zero. Otherwise
  !  SING is set false.
  !
  !  Argonne National Laboratory. Minpack project. March 1980.
  !  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More,
  !  John L. Nazareth

  IMPLICIT NONE

  INTEGER m,n,ls,i,j,jj,l,nmj,nm1

  LOGICAL sing

  real(kind(1.0D0)) s(ls),u(m),v(n),w(m),cos1,cotan,giant,one
  real(kind(1.0D0)) p5,p25,sin1,tan1,tau,temp,zero

  real(kind(1.0D0)) spmpar
  EXTERNAL         spmpar

  one = 1.0D0
  p5 = 0.5D0
  p25 = 0.25D0
  zero = 0.0D0

  !  giant is the largest magnitude in the computer's arithmetic range

  giant = spmpar(3)

  !  Initialize the diagonal element pointer.

  jj = (n*(2*m - n + 1))/2 - (m - n)

  !  Move the nontrivial part of the last column of s into w.

  l = jj
  do i = n, m
     w(i) = s(l)
     l = l + 1
  end do

  !  Rotate the vector v into a multiple of the n-th unit vector
  !  in such a way that a spike is introduced into w.

  nm1 = n - 1
  if (nm1 < 1) goto 70
  do nmj = 1, nm1
     j = n - nmj
     jj = jj - (m - j + 1)
     w(j) = zero
     if (v(j) == zero) goto 50

     !  Determine a givens rotation which eliminates the
     !  j-th element of v.

     if (abs(v(n)) >= abs(v(j))) goto 20
     cotan = v(n)/v(j)
     sin1 = p5/sqrt(p25+p25*cotan**2)
     cos1 = sin1*cotan
     tau = one
     if (abs(cos1)*giant > one) tau = one/cos1
     goto 30

20   continue
     tan1 = v(j)/v(n)
     cos1 = p5/sqrt(p25+p25*tan1**2)
     sin1 = cos1*tan1
     tau = sin1

30   continue

     !  Apply the transformation to v and store the information
     !  necessary to recover the givens rotation.

     v(n) = sin1*v(j) + cos1*v(n)
     v(j) = tau

     !  Apply the transformation to s and extend the spike in w.

     l = jj
     do i = j, m
        temp = cos1*s(l) - sin1*w(i)
        w(i) = sin1*s(l) + cos1*w(i)
        s(l) = temp
        l = l + 1
     end do

50   continue
  end do

70 continue

  !  Add the spike from the rank 1 update to w.

  do i = 1, m
     w(i) = w(i) + v(n)*u(i)
  end do

  !  Eliminate the spike.

  sing = .false.
  if (nm1 < 1) goto 140
  do j = 1, nm1
     if (w(j) == zero) goto 120

     !  Determine a givens rotation which eliminates the
     !  j-th element of the spike.

     if (abs(s(jj)) >= abs(w(j))) goto 90
     cotan = s(jj)/w(j)
     sin1 = p5/sqrt(p25+p25*cotan**2)
     cos1 = sin1*cotan
     tau = one
     if ((abs(cos1)*giant) > one) tau = one/cos1
     goto 100

90   continue
     tan1 = w(j)/s(jj)
     cos1 = p5/sqrt(p25+p25*tan1**2)
     sin1 = cos1*tan1
     tau = sin1

100  continue

     !  Apply the transformation to s and reduce the spike in w.

     l = jj
     do i = j, m
        temp =  cos1*s(l) + sin1*w(i)
        w(i) = -sin1*s(l) + cos1*w(i)
        s(l) = temp
        l = l + 1
     end do

     !  Store the information necessary to recover the
     !  givens rotation.

     w(j) = tau

120  continue

     !  Test for zero diagonal elements in the output s.

     if (s(jj) == zero) sing = .true.
     jj = jj + (m - j + 1)
  end do

140 continue

  !  Move w back into the last column of the output s.

  l = jj
  do i = n, m
     s(l) = w(i)
     l = l + 1
  end do
  if (s(jj) == zero) sing = .true.

  return
end SUBROUTINE R1UPDT
!______________________________________________________________________
real(kind(1.0D0)) FUNCTION SPMPAR(i)

  !  This function provides certain machine parameters.
  !
  !  I is an integer input variable set to 1, 2, or 3 which
  !  selects the desired machine parameter. If the machine has
  !  P base B digits and its smallest and largest exponents are
  !  EMIN and EMAX, respectively, then these parameters are
  !
  !  SPMPAR(1) = B**(1 - P), the machine precision,
  !
  !  SPMPAR(2) = B**(EMIN - 1), the smallest magnitude,
  !
  !  SPMPAR(3) = B**EMAX*(1 - B**(-P)), the largest magnitude.
  !
  !  Note that the values of these parameters can be found for a given
  !  machine if the Mark 12 or later NAg library is installed on it.
  !
  !  SPMPAR(1) is equivalent to X02AJF()
  !  SPMPAR(2) is equivalent to X02AKF()
  !  SPMPAR(3) is equivalent to X02ALF()

  IMPLICIT NONE

  INTEGER i

  real(kind(1.0D0)) rmach(3)

  !  Original single-precision values

  !      rmach(1) = 0.000001D0
  !      rmach(2) = 0.5D-37
  !      rmach(3) = 0.1D+38

  !+**PJK 15/11/11 Values relevant for the present FUN machines

  rmach(1) = 1.110223024625157D-016
  rmach(2) = 2.3D-308  !  actual value 2.225073858507201D-308
  rmach(3) = 1.797693134862316D+308

  spmpar = rmach(i)

  return
end FUNCTION SPMPAR
