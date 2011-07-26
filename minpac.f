CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
C                                                                     C
C  Source Code Control System                                         C
C                                                                     S
C  Information header for the PROCESS systems code modules            C
C                                                                     C
C  P.J.Knight 22 May 1992                                             S
C                                                                     C
C                                                                     C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
C
C  Module         : $Id$
C
C  Module name    : $RCSfile: minpac.f,v $
C  Version no.    : $Revision: 3.3 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE VMCON(
     +     fcnvmc1,fcnvmc2,mode,n,m,meq,x,objf,fgrd,conf,cnorm,lcnorm,
     +     b,lb,tol,maxfev,info,nfev,vlam,glag,vmu,cm,glaga,gamma,eta,
     +     xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa,liwa,ilower,
     +     iupper,bndl,bndu)

c  This subroutine calculates the least value of a function of
c  several variables subject to linear and/or nonlinear equality
c  and inequality constraints.  More particularly, it solves the
c  problem
c
c            minimize f(x)
c
c     subject to c (x) =  0.0 ,  i = 1,...,meq
c                 i
c
c            and c (x) >= 0.0 ,  i = meq+1,...,m
c                 i
c
c            and l <= x <= u  ,  i = 1,...n
c                 i    i    i
c
c
c  The subroutine implements a variable metric method for
c  constrained optimization developed by M.J.D. Powell.
c
c  The subroutine statement is
c
c    subroutine vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,x,objf,fgrd,conf,
c                     cnorm,lcnorm,b,lb,tol,maxfev,info,
c                     nfev,vlam,glag,vmu,cm,glaga,gamma,eta,xa,
c                     bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa,
c                     liwa)
c
c  where
c
c  FCNVMC1 is the name of the user supplied subroutine which
c  calculates the objective and constraint functions. FCNVMC1
c  should be declared in an external statement in the user
c  calling program, and should be written as follows:
c
c   subroutine fcnvmc1(n,m,x,objf,conf,info)
c   integer n,m,info
c   real objf
c   real x(n),fgrd(n),conf(m)
c   ---------------
c   statements to calculate the objective and constraint at x.
c   the objective and constraint functions and must be returned in
c   objf, conf. note that the equality
c   constraints must precede the inequality constraints in conf.
c   ---------------
c   return
c   end
c
c  FCNVMC2 is the name of the user supplied subroutine which
c  calculates the gradients (first derivative vectors)
c  of the objective and constraint functions. FCNVMC2 should be
c  declared in an external statement in the user calling
c  program, and should be written as follows :
c
c   subroutine fcnvmc2(n,m,x,fgrd,cnorm,lcnorm,info)
c   integer n,m,lcnorm,info
c   real objf
c   real x(n),cnorm(lcnorm,m)
c   ---------------
c   statements to calculate the gradients of the objective and
c   constraint functions at x. the gradient of the objective
c   function must be returned in fgrd. note that the equality
c   constraints must precede the inequality constraints in conf.
c   the constraint gradients or normals must be returned as the
c   columns of cnorm.
c   ---------------
c   return
c   end
c
c  The value of INFO should not be changed by FCNVMC2 unless the
c  user wants to terminate execution of VMCON. In this case
c  set INFO to a negative integer.
c
c  MODE is a non-negative integer input variable set to 1 if the
c  second derivative matrix in b (see below) is provided by the
c  user, and to 0 if it is to be initialized to the identity
c  matrix.
c
c  N is a positive integer input variable set to the number of
c  variables.
c
c  M is a positive integer input variable set to the number of
c  constraints.
c
c  MEQ is a non-negative integer input variable set to the number
c  of equality constraints. MEQ must be less than or equal to N.
c
c  X is a real array of length N. On input it must contain an
c  initial estimate of the solution vector. On output X
c  contains the final estimate of the solution vector.
c
c  OBJF is a real output variable that contains the value of the
c  objective function at the output x.
c
c  FGRD is a real output array of length N which contains the
c  components of the gradient of the objective function at
c  the output X.
c
c  CONF is a real output array of length M which contains the
c  values of the constraint functions at the output X. The
c  equality constraints must precede the inequality constraints.
c
c  CNORM is a real LCNORM by M array whose columns contain the
c  constraint normals at the output X in the first N positions.
c
c  LCNORM is a positive integer input variable set to the row
c  dimension of CNORM which is at least N+1.  The (N+1)st row
c  of CNORM is used for work space.
c
c  B is a real LB by LB array whose first N rows and columns
c  contain the approximation to the second derivative matrix
c  of the Lagrangian function. Often, an adequate initial
c  B matrix can be obtained by approximating the hessian
c  of the objective function.  On input, the approximation is
c  provided by the user if MODE = 1 and is set to the identity
c  matrix if MODE = 0. the (N+1)st row and column are used for
c  work space.
c
c  LB is a positive integer input variable set to the row
c  dimension of B which is at least N+1.
c
c  TOL is a non-negative input variable. A normal return occurs
c  when the objective function plus suitably weighted multiples
c  of the constraint functions are predicted to differ from
c  their optimal values by at most TOL.
c
c  MAXFEV is a positive integer input variable set to the limit
c  on the number of calls to FCNVMC1.
c
c  INFO is an integer output variable set as follows
c
c   if INFO is negative then user termination. otherwise
c
c   INFO = 0  improper input parameters. Tests are made to ensure
c             that N and M are positive, TOL is non-negative,
c             MEQ is less than or equal to N, and that LCNORM,
c             LB, LDEL, LH, LWA, and LIWA are sufficiently large.
c
c   INFO = 1  a normal return. see description of TOL.
c
c   INFO = 2  number of calls to FCNVMC1 is at least MAXFEV.
c
c   INFO = 3  line search required ten calls of FCNVMC1.
c
c   INFO = 4  uphill search direction was calculated.
c
c   INFO = 5  quadratic programming technique was unable to find
c             a feasible point.
c
c   INFO = 6  quadratic programming technique was restricted by
c             an artificial bound or failed due to a singular
c             matrix.
c
c  NFEV is an integer output variable set to the number of calls
c  to FCNVMC1.
c
c  VLAM is a real output array of length M+2N+1 which contains
c  the Lagrange multipliers at the output X.  The Lagrange
c  multipliers provide the sensitivity of the objective
c  function to changes in the constraint functions.
c  note that VLAM(M+I), I=1,...,N gives the multipliers for
c  the lower bound constraints.  VLAM(M+N+1+I), I=1,...,N
c  gives the multipliers for the upper bound constraints.
c
c  GLAG is a real output array of length N which contains the
c  components of the gradient of the Lagrangian function at
c  the output x.
c
c  CM is a real work array of length M.
c
c  VMU is a real work array of length M+2N+1.
c
c  GLAGA, GAMMA, ETA, XA, BDELTA are real work arrays of
c  length N.
c
c  DELTA is a real work array of length LDEL.
c
c  LDEL is a positive integer input variable set equal to the
c  length of DELTA which is at least MAX(7*(N+1),4*(N+1)+M).
c
c  GM, BDL, BDU are real work arrays of length N+1.
c
c  H is a real LH by LH work array.
c
c  LH is a positive integer input variable set to the dimension
c  of the square array H which is at least 2*(N+1).
c
c  WA is a real work array of length LWA.
c
c  LWA is a positive integer input variable set equal to the
c  dimension of WA which is at least 2*(N+1).
c
c  IWA is an integer work array of length LIWA.
c
c  LIWA is a positive integer input variable set equal to the
c  dimension of IWA which is at least 6*(N+1) + M.
c
c  ILOWER is an integer array of length N.
c  If X(I) has a lower bound, ILOWER(I) is set to 1
c  on input.  If no bound is provided, ILOWER(i) should
c  be 0 (the default value).
c
c  BNDL is a real array of length N.
c  If X(I) has a lower bound, it should be given in BNDL(I).
c
c  IUPPER is an integer array of length N.
c  If X(I) has an upper bound, IUPPER(I) is set to 1
c  on input.  If no bound is provided, IUPPER(I) should
c  be 0 (the default value).
c
c  BNDU is a real array of length N.
c  If X(I) has a upper bound, it should be given in BNDU(I).
c
c  Algorithm version of June 1979.
c
c  Roger L. Crane, Kenneth E. Hillstrom, Michael Minkoff
c  Modified for simple bounds, M. Minkoff (10/26/82)
c  Modified for separate function and gradient evaluation,
c  M. Minkoff (11/18/83)
c  Modified to pass ILOWER, IUPPER,BNDL,BDNU through argument
c  list instead of through common block, and remove write
c  capability J. Galambos (5/21/91)
c  Modified for separate FCNVMC1 and FCNVMC2 functions to avoid passing
c  info via a common block. J. Galambos (5/21/91)

      IMPLICIT NONE

      INTEGER mode,n,m,meq,lcnorm,lb,maxfev,info,nfev,ldel,lh,lwa,liwa
      INTEGER iwa(liwa),ilower(n),iupper(n)
      INTEGER i,j,k,mact,nfinit,nls,np1,np1j,npp,nqp,nsix,nsixi
      INTEGER inx,ki,ml,mlp1,mcon,mp1,mpn,mpnpp1,mpnppn

      DOUBLE PRECISION objf,tol
      DOUBLE PRECISION x(n),fgrd(n),conf(m),cnorm(lcnorm,m),b(lb,lb),
     +     vlam(*),glag(n),vmu(*),cm(m),glaga(n),gamma(n),
     +     eta(n),xa(n),bdelta(n),delta(ldel),gm(*),
     +     bdl(*),bdu(*),h(lh,lh),wa(lwa),bndl(n),bndu(n)
      DOUBLE PRECISION alpha,aux,auxa,calpha,cp1,cp2,cp5,dbd,dflsa,dg,
     +     fls,flsa,one,spgdel,sum,temp,thcomp,theta,zero

      EXTERNAL fcnvmc1,fcnvmc2

      zero = 0.0D0
      cp1 = 0.1D0
      cp2 = 0.2D0
      cp5 = 0.5D0
      one = 1.0D0

      np1 = n + 1
      npp = 2*np1
      info = 0

c  Check input parameters for errors

      if (
     +     (n.le.0)           .or.
     +     (m.le.0)           .or.
     +     (meq.gt.n)         .or.
     +     (lcnorm.lt.(n+1))  .or.
     +     (lb.lt.(n+1))      .or.
     +     (tol.lt.zero)      .or.
     +     (ldel.lt.max(7*(n+1),4*(n+1)+m)).or.
     +     (lh.lt.(2*(n+1)))  .or.
     +     (lwa.lt.(2*(n+1))) .or.
     +     (liwa.lt.(6*(n+1)+m))
     +     ) goto 450

c  Set the initial elements of b and vmu. vmu is the weighting
c  vector to be used in the line search.
c  Use hessian estimate provided by user if mode = 1 on input

      if (mode .eq. 1) goto 25

c  Use identity matrix for hessian estimate

      do 20 j = 1, n
         do 10 i = 1, n
            b(i,j) = zero
 10      continue
         b(j,j) = one
 20   continue

 25   continue

c  Set m + 1 to mp1
c  Set m + n to mpn (these are limits for lower bound indices)
c  Set m + np1 + 1 to mpnpp1
c  Set m + np1 + n to mpnppn (these are limits for upper bound
c  indices)

      mp1 = m + 1
      mpn = m + n
      mpnpp1 = m + np1 + 1
      mpnppn = m + np1 + n

c  Set mcon to total number of actual constraints

      mcon = m
      do 26 i = 1, n
         if (ilower(i) .eq. 1) mcon = mcon + 1
 26   continue

c  Set ml to m + number of lower bounds

      ml = mcon

c  Set mlp1 to ml + 1

      mlp1 = ml + 1
      do 27 i = 1, n
         if (iupper(i) .eq. 1) mcon = mcon + 1
 27   continue
      do 30 k = 1, mpnppn
         vmu(k) = zero
 30   continue

c  Set initial values of some variables
c  nfev is the number of calls of fcnvmc1
c  nsix is the length of an array
c  nqp is the number of quadratic subproblems

      nfev = 1
      nsix = 6*np1
      nqp = 0

c  Calculate the initial functions and gradients

      call fcnvmc1(n,m,x,objf,conf,info)

      if (info .lt. 0) goto 450

      call fcnvmc2(n,m,x,fgrd,cnorm,lcnorm,info)

      if (info .lt. 0) goto 450

c  Start the iteration by calling the quadratic programming
c  subroutine

 40   continue

c  Increment the quadratic subproblem counter

      nqp = nqp + 1

c  Set the linear term of the quadratic problem objective function
c  to the negative gradient of objf

      do 50 i = 1, n
         gm(i) = -fgrd(i)
 50   continue
      do 55 i = 1, mpnppn
         vlam(i) = zero
 55   continue

      call qpsub(
     +     n,m,meq,conf,cnorm,lcnorm,b,lb,gm,bdl,bdu,info,x,delta,
     +     ldel,cm,h,lh,mact,wa,lwa,iwa,liwa,ilower,iupper,
     +     bndl,bndu)

c  The following return is made if the quadratic problem solver
c  failed to find a feasible point, if an artificial bound was
c  active, or if a singular matrix was detected

      if ((info .eq. 5) .or. (info .eq. 6)) goto 450

c  Initialize the line search iteration counter

      nls = 0

c  Calculate the Lagrange multipliers

      do 70 j = 1, mact
         k = iwa(j) - npp
         if (k .gt. 0) goto 59
         ki = iwa(j)
         k = ki + m
         if (ki .gt. np1) goto 58
         if (ki .eq. np1) goto 70
         if (ilower(ki) .eq. 1) goto 59
         goto 70
 58      continue
         ki = iwa(j) - np1
         k = ki + m + np1
         if (ki .eq. np1) goto 70
         if (iupper(ki) .eq. 1) goto 59
         goto 70
 59      continue
         do 60 i = 1, n
            np1j = np1 + j
            nsixi = nsix + i
            vlam(k) = vlam(k) + h(np1j,i)*delta(nsixi)
 60      continue
 70   continue

c  Calculate the gradient of the Lagrangian function
c  nfinit is the value of nfev at the start of an iteration

      nfinit = nfev
      do 80 i = 1, n
         glag(i) = fgrd(i)
 80   continue
      do 100 k = 1, m
         if (vlam(k) .eq. zero) goto 100
         do 90 i = 1, n
            glag(i) = glag(i) - cnorm(i,k)*vlam(k)
 90      continue
 100  continue
      do 105 k = mp1, mpn
         if (vlam(k) .eq. zero) goto 105
         inx = k - m
         if (ilower(inx) .eq. 0) goto 105
         glag(inx) = glag(inx) - vlam(k)
 105  continue
      do 106 k = mpnpp1, mpnppn
         if (vlam(k) .eq. zero) goto 106
         inx = k - m - np1
         if (iupper(inx) .eq. 0) goto 106
         glag(inx) = glag(inx) + vlam(k)
 106  continue

c  Set spgdel to the scalar product of fgrd and delta
c  Store the elements of glag and x

      spgdel = zero
      do 110 i = 1, n
         spgdel = spgdel + fgrd(i)*delta(i)
         glaga(i) = glag(i)
         xa(i) = x(i)
 110  continue

c  Revise the vector vmu and test for convergence

      sum = abs(spgdel)
      do 120 k = 1, mpnppn
         aux = abs(vlam(k))
         vmu(k) = max(aux,cp5*(aux + vmu(k)))
         temp = 0.00D0
         if (k .gt. m) goto 111
         temp = conf(k)
         goto 119
 111     continue
         if (k .gt. mpn) goto 112
         inx = k - m
         if (ilower(inx) .eq. 0) goto 120
         temp = x(inx) - bndl(inx)
         goto 119
 112     continue
         inx = k - m - np1
         if ((inx .eq. 0) .or. (inx .gt. n)) goto 120
         if (iupper(inx) .eq. 0) goto 120
         temp = bndu(inx) - x(inx)
 119     continue
         sum = sum + abs(vlam(k)*temp)
 120  continue

c  Exit if convergence criterion is satisfied

      if (sum .le. tol) goto 450

c  Set sum to the weighted sum of infeasibilities
c  Set fls to the line search objective function

 130  continue

c  Increment the line search iteration counter

      nls = nls + 1
      sum = zero
      do 140 k = 1, mpnppn
         aux = 0.0D0
         if (k .le. meq) aux = conf(k)
         temp = 0.0D0
         if (k .gt. m) goto 131
         temp = conf(k)
         goto 139
 131     continue
         if (k .gt. mpn) goto 132
         inx = k - m
         if (ilower(inx) .eq. 0) goto 140
         temp = x(inx) - bndl(inx)
         goto 139
 132     continue
         inx = k - m - np1
         if ((inx .eq. 0) .or. (inx .gt. n)) goto 140
         if (iupper(inx) .eq. 0) goto 140
         temp = bndu(inx) - x(inx)
 139     continue
         sum = sum + vmu(k)*max(aux,-temp)
 140  continue
      fls = objf + sum

      if (nfev .ne. nfinit) goto 150

c  Set the initial conditions for the line search
c  flsa is the initial value of the line search function
c  dflsa is its first derivative (if delta(np1) = 1)
c  alpha is the next reduction in the step-length

      flsa = fls
      dflsa = spgdel - delta(np1)*sum
      if (dflsa .ge. zero) goto 420

c  Set initial multiplying factor for stepsize
c  Set initial value of stepsize for output

      alpha = one
      calpha = one
      goto 210

c  Test whether line search is complete

 150  continue
      aux = fls - flsa

c  Exit line search if function difference is small

      if (aux .le. (cp1*dflsa)) goto 260

c  Exit if the line search requires ten or more function
c  evaluations

      if (nfev .ge. (nfinit + 10)) goto 380

c  Calculate next reduction in the line step assuming a quadratic
c  fit.

      alpha = max(cp1,cp5*dflsa/(dflsa - aux))

c  Multiply delta by alpha and calculate the new x

 210  continue
      calpha = alpha*calpha

      do 220 i = 1, n
         delta(i) = alpha*delta(i)
         x(i) = xa(i) + delta(i)
 220  continue

      dflsa = alpha*dflsa

c  Test nfev against maxfev, call fcnvmc1 and resume line search

      if (nfev .ge. maxfev) goto 380
      nfev = nfev + 1

      call fcnvmc1(n,m,x,objf,conf,info)

      if (info .lt. 0) goto 450
      goto 130

c  Line search is complete. Calculate gradient of Lagrangian
c  function for use in updating hessian of Lagrangian

 260  continue

      call fcnvmc1(n,m,x,objf,conf,info)
      call fcnvmc2(n,m,x,fgrd,cnorm,lcnorm,info)

      if (info .lt. 0) goto 450
      do 270 i = 1, n
         glag(i) = fgrd(i)
 270  continue
      do 290 k = 1, m
         if (vlam(k) .eq. zero) goto 290
         do 280 i = 1, n
            glag(i) = glag(i) - cnorm(i,k)*vlam(k)
 280     continue
 290  continue
      do 291 k = mp1, mpn
         if (vlam(k) .eq. zero) goto 291
         inx = k - m
         if (ilower(inx) .eq. 0) goto 291
         glag(inx) = glag(inx) - vlam(k)
 291  continue
      do 292 k = mpnpp1, mpnppn
         if (vlam(k) .eq. zero) goto 292
         inx = k - m - np1
         if (iupper(inx) .eq. 0) goto 292
         glag(inx) = glag(inx) + vlam(k)
 292  continue

c  Calculate gamma and bdelta in order to revise b
c  Set dg to the scalar product of delta and gamma
c  Set dbd to the scalar product of delta and bdelta

      dg = zero
      dbd = zero
      do 340 i = 1, n
         gamma(i) = glag(i) - glaga(i)
         bdelta(i) = zero
         do 330 j = 1, n
            bdelta(i) = bdelta(i) + b(i,j)*delta(j)
 330     continue
         dg = dg + delta(i)*gamma(i)
         dbd = dbd + delta(i)*bdelta(i)
 340  continue

c  Calculate the vector eta for the b-f-g-s formula
c  replace dg by the scalar product of delta and eta

      aux = cp2*dbd
      theta = one
      if (dg .lt. aux) theta = (dbd - aux)/(dbd - dg)
      thcomp = one - theta
      do 350 i = 1, n
         eta(i) = theta*gamma(i) + thcomp*bdelta(i)
 350  continue
      if (dg .lt. aux) dg = aux

c  Revise the matrix b and begin new iteration

      do 375 i = 1, n
         aux = bdelta(i)/dbd
         auxa = eta(i)/dg
         do 370 j = i, n
            b(i,j) = b(i,j) - aux*bdelta(j) + auxa*eta(j)
            b(j,i) = b(i,j)
 370     continue
 375  continue
      goto 40

c  Error returns. restore previous solution

 380  continue
      do 390 i = 1, n
         x(i) = xa(i)
 390  continue
      if (nfev .ge. maxfev) goto 400
      nfev = nfev + 1

      call fcnvmc1(n,m,x,objf,conf,info)

      if (info .lt. 0) goto 450
      goto 410

c  Error return because there have been maxfev calls of fcnvmc1

 400  continue
      info = 2
      goto 450

c  Error return because line search required 10 calls of fcnvmc1

 410  continue
      info = 3
      goto 450

c  Error return because uphill search direction was calculated

 420  continue
      info = 4

 450  continue

      return
      end
c______________________________________________________________________
      SUBROUTINE QPSUB(
     +     n,m,meq,conf,cnorm,lcnorm,b,lb,gm,bdl,bdu,info,
     +     x,delta,ldel,cm,h,lh,mact,wa,lwa,iwa,liwa,
     +     ilower,iupper,bndl,bndu)

c  This subroutine finds the value of the solution vector which
c  minimizes a quadratic function of several variables subject to
c  equality and inequality constraints. This is accomplished
c  by invoking subroutine harwqp, a modified version of subroutine
c  ve02ad, the Harwell Library subroutine for general quadratic
c  programming.
c
c  The subroutine statement is
c
c  subroutine qpsub(n,m,meq,conf,cnorm,lcnorm,b,lb,gm,bdl,bdu,
c                   info,delta,ldel,cm,h,lh,mact,wa,lwa,iwa,liwa)
c
c  where
c
c  N is a positive integer input variable set to the number of
c  variables.
c
c  M is a positive integer input variable set to the number of
c  constraints.
c
c  MEQ is a non-negative integer input variable set to the number
c  of equality constraints. MEQ must be less than or equal to N.
c
c  CONF is a real input array of length M which contains the
c  constraint functions.
c
c  CNORM is a real LCNORM by M array whose columns contain the
c  constraint normals in the first N positions.  The (N+1)st
c  row of CNORM is used for work space.
c
c  LCNORM is a positive integer input variable set to the row
c  dimension of CNORM which is at least N+1.
c
c  B is a real LB by LB array whose first N rows and columns
c  contain the hessian approximation on input.  The (N+1)st
c  row and column are used for work space.
c
c  LB is a positive integer input variable set to the row
c  dimension of B which is at least N+1.
c
c  GM is a real array of length N+1 which, on input, contains
c  the negative components of the function gradient in the
c  first N elements. The (N+1)st element is used for work space.
c
c  BDL, BDU are real work arrays of length N+1.
c
c  INFO is an integer variable. It must be set to zero before
c  the initial call to qpsub and should not otherwise be
c  changed.  On output, INFO is set as follows
c
c   INFO = 1  a normal return.
c
c   INFO = 5  a feasible point was not found.
c
c   INFO = 6  solution is restricted by an artificial bound or
c             failed due to a singular matrix.
c
c  DELTA is a real array of length LDEL.  It need not be set
c  before the first call to QPSUB, but before each subsequent
c  call, the first N locations should contain an estimate of
c  the solution vector. (Zero is used as the estimate for the
c  first call.)  On output, the value of the solution vector
c  which minimizes the quadratic function is contained in the
c  first N locations.  The remainder of the array is used for
c  work space.
c
c  LDEL is a positive integer input variable set to the length
c  of DELTA which is at least MAX(7*(N+1),4*(N+1)+M).
c
c  CM is a real work array of length M.
c
c  H is a real LH by LH work array.
c
c  LH is a positive integer input variable set to the dimension
c  of the square array H which is at least 2*(N+1).
c
c  MACT is an integer output variable set to the number of
c  constraints in the basis.
c
c  WA is a real work array of length LWA.
c
c  LWA is a positive integer input variable set equal to the
c  dimension of WA which is at least 2*(N+1).
c
c  IWA is an integer work array of length LIWA.
c
c  LIWA is a positive integer input variable set to the length
c  of IWA which is at least 6*(N+1) + M.
c
c  ILOWER is an integer array of length N.
c  If X(I) has a lower bound, ILOWER(I) is set to 1
c  on input.  If no bound is provided, ILOWER(I) should
c  be 0 (the default value).
c
c  BNDL is a real array of length N.
c  If X(I) has a lower bound, it should be given in BNDL(I).
c
c  IUPPER is an integer array of length N.
c  If X(I) has a upper bound, IUPPER(I) is set to 1
c  on input.  If no bound is provided, IUPPER(I) should
c  be 0 (the default value).
c
c  BNDU is a real array of length N.
c  If X(I) has a upper bound, it should be given in BNDU(I).
c
c  Roger L. Crane, Kenneth E. Hillstrom, Michael Minkoff
c  Modified for simple bounds, M. Minkoff (10/26/82)
c
c  Modified to pass ILOWER,IUPPER, BNDL,BNDU in through argument list
c  instead of through COMMON, J. Galambos, (5/21/91)

      IMPLICIT NONE

      INTEGER n,m,meq,lcnorm,lb,info,ldel,lh,mact,lwa,liwa
      INTEGER iwa(liwa),ilower(n),iupper(n)
      INTEGER i,iflag,j,k,mode,mtotal,np1,npp
      INTEGER inx

      DOUBLE PRECISION conf(m),cnorm(lcnorm,m),b(lb,lb),gm(*),bdl(*),
     +     bdu(*),delta(ldel),cm(m),h(lh,lh),wa(lwa)
      DOUBLE PRECISION x(n),bndu(n),bndl(n)
      DOUBLE PRECISION cd6,cdm6,cp9,one,zero

C+**PJK 24/05/06 Added SAVE command (as a number of variables are
C+**PJK 24/05/06 initialised only if info=0)

      SAVE

      cd6 = 1.0D6
      cdm6 = 1.0D-6
      cp9 = 0.9D0
      one = 1.0D0
      zero = 0.0D0

      np1 = n + 1
      npp = 2*np1
      if (info .gt. 0) goto 50
      mtotal = m + npp

c  Set initial values of some variables

      info = 1
      mact = meq + 1
      mode = 1

c  Set the initial elements of bdl, bdu, and delta where
c  bdl are artificial lower bounds,
c  bdu are artificial upper bounds and
c  delta is an initial solution estimate

      do 10 i = 1, n
         bdl(i) = -cd6
         bdu(i) = cd6
 10   continue

c  Bound the artificial variables in qp

      bdl(np1) = zero
      delta(np1) = one
      if (meq .le. 0) goto 30

c  Set indices of equality constraints
c  The bounds are the first npp constraints

      do 20 k = 1, meq
         iwa(k) = k + npp
 20   continue

 30   continue

c  Set index of upper bound of delta(np1) active

      iwa(mact) = npp

c  Extend gm and b because of the extra variable that is introduced
c  to allow for feasibility. Set linear term of cost function to
c  a large value

      gm(np1) = cd6
      do 40 i = 1, np1
         b(i,np1) = zero
         b(np1,i) = zero
 40   continue

c  Set the elements of cm and cnorm(np1,*)

 50   continue
      do 60 i = 1, n
         if (ilower(i) .eq. 1) bdl(i) = bndl(i) - x(i)
         if (iupper(i) .eq. 1) bdu(i) = bndu(i) - x(i)
         delta(i) = max(zero,bdl(i))
         delta(i) = min(delta(i),bdu(i))
 60   continue
      do 90 k = 1, m
         if (k .le. meq) goto 70
         if (conf(k) .lt. zero) goto 70

c  If an inequality constraint is satisfied set the constant term
c  in the constraint vector to the violation and put zero in the
c  constraint matrix for the (n+1)st variable

         cm(k) = -conf(k)
         cnorm(np1,k) = zero
         goto 90
 70      continue

c  If the constraint is an equality or a violated inequality set
c  the constant term to zero and put the function value in the
c  constraint matrix for the (n+1)st variable

         cm(k) = zero
         cnorm(np1,k) = conf(k)
 90   continue

c  Set the upper bound of the (n+1)st variable
c  Set iflag. iflag will be used in checking active constraints
c  Call subroutine harwqp to solve quadratic programming problem

      bdu(np1) = one
      iflag = -1
 100  continue

      call harwqp(np1,mtotal,b,lb,gm,cnorm,lcnorm,cm,bdl,bdu,delta,
     +     mact,meq,h,lh,iwa,wa,iwa(4*(n+1)+m+1),mode,info)

      if (info .ne. 1) goto 130

c  Check whether the required feasibility conditions hold
c  If delta(np1) is sufficiently small there is no feasible
c  solution

      if (delta(np1) .le. cdm6) goto 120

c  Check whether active constraints are bounds

      do 110 j = 1, mact
         if (iwa(j) .gt. npp) goto 110
         if (iwa(j) .eq. npp) goto 101
         if (iwa(j) .gt. np1) goto 105
         if (iwa(j) .eq. np1) goto 130
         if (ilower(iwa(j)) .eq. 0) goto 130
         goto 110
 105     continue
         inx = iwa(j) - np1
         if (iupper(inx) .eq. 0) goto 130
         goto 110
 101     continue

c  The active constraint is blu(np1)

         iflag = 1
 110  continue

c  Normal exit

      if (iflag .ge. 1) goto 140

c  A second call to harwqp found blu(np1) to still be inactive
c  thus an error exit is made

      if (iflag .ge. 0) goto 120

c  Reduce bdu(np1) and retry harwqp

      bdu(np1) = cp9*delta(np1)
      iflag = 0
      goto 100

c  Error return because of infeasibility

 120  continue
      info = 5
      goto 140

c  Error return because of restriction by an artificial bound
c  or detection of a singular matrix

 130  continue
      info = 6

 140  continue

      return
      end
c______________________________________________________________________
      SUBROUTINE HARWQP(
     +     n,m,a,ia,b,c,ic,d,bdl,bdu,x,k,ke,h,ih,lt,wa,iwa,mode,info)

c  This program is a modified version of the Harwell library
c  subroutine VE02AD dated 11/06/70.  The modifications were made
c  to substitute the subroutines HINV and DOTPMC for Harwell
c  subroutines MB01B and MC03AS.  The calling sequence above
c  includes three entries, WA, IWA, and INFO not present in
c  the original program.  WA and IWA are real and
c  integer work arrays, respectively, and must be dimensioned
c  at least 2*N.  INFO is an output variable set to one for a
c  normal return and set to two when a singular matrix is detected
c  in HINV.  All other entries in the calling sequence are as
c  described in the Harwell documentation.
c
c  Modified 5/22/91 to use implicit none (J. Galambos)
c
c+**PJK 02/11/92 Throughout this routine, argument 1 of DOTPMC has
c+**PJK 02/11/92 different dimensions than are declared in the source
c+**PJK 02/11/92 code of the routine itself. The program runs without
c+**PJK 02/11/92 error but beware of future modifications.

      IMPLICIT NONE

      INTEGER n,m,ia,ic,k,ke,ih,mode,info
      INTEGER iwa(*),lt(*)
      INTEGER i, ial, ib, ii, j, li, ni, nk, nn, n3,n4,n5,n6
      INTEGER i0,i1,i2,i3

      DOUBLE PRECISION a(ia,*),b(*),c(ic,*),d(*),bdl(*),bdu(*),x(*),
     +     h(ih,*),wa(*)
      DOUBLE PRECISION alpha, cac, cc, chc, ghc, y, z, zz
      DOUBLE PRECISION r0

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

      if (mode.ge.3) goto 99

c  Call feasible vertex routine

 8    continue
      call harwfp(n,m,c,ic,d,bdl,bdu,x,k,ke,h,ih,lt,wa,iwa,info)
      if (info .ne. 1) goto 1000
      if (k.eq.0) goto 1000
      if ((mode.eq.2).and.(.not.retest)) goto 100

c  Initial operators h=0 and cstar=c(-1) from ve02b
      do 65 i = 1,n
         do 60 j = 1,n
            h(n+i,j) = h(i,j)
            h(i,j) = 0.0D0
 60      continue
 65   continue
      goto 120

 99   continue
      do 1 i = 1,m
         lt(nn+i) = 1
 1    continue

c  Constraints indexed as  -1=equality, 0=active, 1=inactive
      if (k.eq.0) goto 100
      do 2 i=1,k
         j=0
         if (i.le.ke)j=-1
         lt(nn+lt(i))=j
 2    continue

 100  continue
      if ((mode.eq.5).and.(.not.retest)) goto 109

c  Set up matrix and rhs of equations governing equality problem
      do 1011 i = 1,n
         x(n+i) = b(i)
         do 101 j = 1,n
            h(i,j) = a(i,j)
 101     continue
 1011 continue

      if (((mode.eq.2).or.(mode.eq.3)).and.(.not.retest)) goto 200
      if (k.eq.0) goto 107
      do 1021 i = 1,k
         li = lt(i)
         if (li.gt.nn) goto 105
         do 103 j = 1,n
            h(j,n+i) = 0.0D0
            h(n+i,j) = 0.0D0
 103     continue
         if (li.gt.n) goto 104
         h(n+i,li) = 1.0D0
         h(li,n+i) = 1.0D0
         x(nn+i) = bdl(li)
         goto 108

 104     continue
         li = li-n
         h(n+i,li) = -1.0D0
         h(li,n+i) = -1.0D0
         x(nn+i) = -bdu(li)
         goto 108

 105     continue
         li = li-nn
         do 106 j = 1,n
            h(n+i,j) = c(j,li)
            h(j,n+i) = c(j,li)
 106     continue
         x(nn+i) = d(li)

 108     continue
         do 102 j = 1,k
            h(n+i,n+j) = 0.0D0
 102     continue
 1021 continue

 107  continue
      nk = n+k

c  Invert matrix giving operators h and cstar

      call hinv(h,ih,nk,wa,iwa,info)
      if (info .ne. 1) goto 1000
      goto 118

c  Set up rhs only

 109  continue
      do 113 i = 1,n
         x(n+i) = b(i)
 113  continue

      do 115 i = 1,k
         li = lt(i)
         if (li.gt.nn) goto 117
         if (li.gt.n) goto 116
         x(nn+i) = bdl(li)
         goto 115

 116     continue
         x(nn+i) = -bdu(li-n)
         goto 115

 117     continue
         x(nn+i)=d(li-nn)
 115  continue

c  Solve for solution point x

      nk = n+k

 118  continue
      do 119 i=1,n
         call dotpmc(h(1,i),i1,x(n+1),i1,r0,x(i),nk,i0)
 119  continue

c  Check feasibility, if not exit to 8

      do 110 i = 1,m
         if (lt(nn+i).le.0) goto 110
         if (i.gt.n) goto 111
         z = x(i)-bdl(i)
         goto 114

 111     continue
         if (i.gt.nn) goto 112
         z = bdu(i-n)-x(i-n)
         goto 114

 112     continue
         j = i-nn
         call dotpmc(c(1,j),i1,x(1),i1,d(j),z,n,i2)

 114     continue
         if (z.lt.0.0D0) goto 8
 110  continue

 120  continue

c  Calculate gradient g and Lagrange multipliers -cstar.g,
c  Find largest multiplier,  exit if not positive

      do 121 i = 1,n
         call dotpmc(a(i,1),ia,x(1),i1,b(i),x(n6+i),n,i2)
 121  continue
      if (k.eq.0) goto 1000

C+**PJK 17/11/97 D999 reduced to D99
      z = -1.0D99
      do 122 i = 1,k
         if (lt(nn+lt(i)).eq.-1) goto 122
         call dotpmc(h(n+i,1),ih,x(n6+1),i1,r0,zz,n,i3)
         if (zz.le.z) goto 122
         z = zz
         ii = i
 122  continue

      if (z.gt.0.0D0) goto 130
      if ((retest).or.(mode.ge.4)) goto 137
      retest = .true.
      goto 100

 137  continue
      if (z.ne.0.0D0) goto 1000
      goto 1000

c  Set direction of search as corresponding row of cstar

 130  continue
      do 131 i = 1,n
         x(nn+i) = h(n+ii,i)
 131  continue

 136  continue
      do 132 i = 1,n
         call dotpmc(a(i,1),ia,x(nn+1),i1,r0,x(n+i),n,i0)
 132  continue
      call dotpmc(x(nn+1),i1,x(n+1),i1,r0,cac,n,i0)
      if (cac.gt.0.0D0) goto 134
      postiv = .false.
      y = 1.0D0
      goto 135

 134  continue
      postiv = .true.
      y = z/cac

 135  continue
      do 133 i = 1,n
         x(n5+i) = x(nn+i)*y
 133  continue
      passiv = .true.

 139  continue
C+**PJK 17/11/97 D999 reduced to D99
      alpha = 1.0D99
      nk = n+k

c  Linear search along direction of search,  passiv indicates
c  a constraint has been removed to get search direction,
c  postiv indicates positive curvature along the direction

      do 140 i = 1,m
         if (lt(nn+i).le.0) goto 140
         if (i.gt.n) goto 141
         if (x(n5+i).ge.0.0D0) goto 140
         cc = (bdl(i)-x(i))/x(n5+i)
         goto 143

 141     continue
         if (i.gt.nn) goto 142
         if (x(n4+i).le.0.0D0) goto 140
         cc = (bdu(i-n)-x(i-n))/x(n4+i)
         goto 143

 142     continue
         j = i-nn
         call dotpmc(c(1,j),i1,x(n5+1),i1,r0,zz,n,i0)
         if (zz.ge.0.0D0) goto 140
         call dotpmc(c(1,j),i1,x(1),i1,d(j),cc,n,i1)
         cc = cc/zz

 143     continue
         if (cc.ge.alpha) goto 140
         alpha = cc
         ial = i
 140  continue
      if (passiv) lt(nn+lt(ii)) = 1

c  If minimum found, goto  170

      if ((postiv).and.(alpha.ge.1.0D0)) goto 170

c  Calculate h.c and cstar.c

      do 144 i=1,n
         x(i) = x(i)+alpha*x(n5+i)
 144  continue
      alpha = alpha*y
      j = 1
      if (k.eq.n) j = n+1
      if (ial.gt.n) goto 146
      do 145 i = j,nk
         x(n3+i) = h(i,ial)
 145  continue
      chc = x(n3+ial)
      goto 151

 146  continue
      ib = ial-n
      if (ib.gt.n) goto 148
      do 147 i = j,nk
         x(n3+i) = -h(i,ib)
 147  continue
      chc = -x(n3+ib)
      goto 151

 148  continue
      ib = ib-n
      do 149 i = 1,n
         x(n5+i) = c(i,ib)
 149  continue
      do 150 i = j,nk
         call dotpmc(h(i,1),ih,x(n5+1),i1,r0,x(n3+i),n,i0)
 150  continue
      if (k.ne.n) call dotpmc(x(n5+1),i1,x(n3+1),i1,r0,chc,n,i0)

 151  continue
      lt(nn+ial) = 0
      if (k.eq.n) goto 180
      if (passiv) goto 160

c  Apply formula for adding a constraint

 156  continue
      if (k.eq.0) goto 157
      do 1521 i = 1,k
         alpha = x(n4+i)/chc
         ni = n+i
         do 152 j = 1,n
            h(ni,j) = h(ni,j)-alpha*x(n3+j)
 152     continue
 1521 continue

 157  continue
      k = k+1
      lt(k) = ial
      do 158 j = 1,n
         h(n+k,j) = x(n3+j)/chc
 158  continue
      if (k.lt.n) goto 154
      do 1531 i = 1,n
         do 153 j = 1,n
            h(i,j) = 0.0D0
 153     continue
 1531 continue
      goto 159

 154  continue
      do 1551 i = 1,n
         alpha = x(n3+i)/chc
         do 155 j = 1,i
            h(i,j) = h(i,j)-alpha*x(n3+j)
            h(j,i) = h(i,j)
 155     continue
 1551 continue

 159  continue
      if (.not.passiv) goto 167

c  Removal of a constraint has been deferred,  set up as if
c  the constraint is being removed from augmented basis

      do 164 i=1,n
         call dotpmc(a(i,1),ia,x(1),i1,b(i),x(n6+i),n,i2)
         x(nn+i) = h(n+ii,i)
 164  continue
      call dotpmc(x(n6+1),i1,x(nn+1),i1,r0,z,n,i3)
      if (z.eq.0.0D0) goto 178
      goto 136

 160  continue
      cc = x(n4+ii)
      y = chc*cac+cc**2
      call dotpmc(x(n6+1),i1,x(n3+1),i1,r0,ghc,n,i0)
      if ((alpha*y).lt.(chc*(z-alpha*cac)+ghc*cc)) goto 156

c  Apply formula for exchanging new constraint
c  with passive constraint

      do 161 i = 1,k
         ni = n+i
         call dotpmc(h(ni,1),ih,x(n+1),i1,r0,x(n5+i),n,i0)
 161  continue
      do 162 i = 1,n
         x(n+i) = (chc*x(nn+i)-cc*x(n3+i))/y
         x(n6+i) = (cac*x(n3+i)+cc*x(nn+i))/y
 162  continue
      do 1631 i = 1,n
         do 163 j = 1,i
            h(i,j) = h(i,j)+x(n+i)*x(nn+j)-x(n6+i)*x(n3+j)
            h(j,i) = h(i,j)
 163     continue
 1631 continue
      x(n4+ii) = x(n4+ii)-1.0D0
      do 1661 i = 1,k
         ni = n+i
         do 166 j = 1,n
            h(ni,j) = h(ni,j)-x(n4+i)*x(n6+j)-x(n5+i)*x(n+j)
 166     continue
 1661 continue
      lt(ii) = ial

 167  continue
      if (k.eq.n) goto 120

c  Calculate g,  new search direction is -h.g

      do 168 i = 1,n
         call dotpmc(a(i,1),ia,x(1),i1,b(i),x(n+i),n,i2)
 168  continue
      z = 0.0D0
      do 169 i = 1,n
         call dotpmc(h(i,1),ih,x(n+1),i1,r0,x(n5+i),n,i3)
         if (x(n5+i).ne.0.0D0) z = 1.0D0
 169  continue
      passiv = .false.
      if (z.eq.0.0D0) goto 120
      postiv = .true.
      goto 139

 170  continue
      do 171 i = 1,n
         x(i) = x(i)+x(n5+i)
 171  continue

c  x is now the minimum point in the basis
c  Update the operators if a constraint had been removed

      if (.not.passiv) goto 120

 178  continue
      do 1721 i = 1,n
         alpha = x(nn+i)/cac
         do 172 j = 1,i
            h(i,j) = h(i,j)+alpha*x(nn+j)
            h(j,i) = h(i,j)
 172     continue
 1721 continue
      if (k.gt.1) goto 177
      k = 0
      goto 120

 177  continue
      if (ii.eq.k) goto 175
      do 174 i=1,n
         h(n+ii,i) = h(n+k,i)
 174  continue
      lt(ii) = lt(k)

 175  continue
      k = k-1
      do 173 i = 1,k
         ni = n+i
         call dotpmc(h(ni,1),ih,x(n+1),i1,r0,x(n3+i),n,i0)
 173  continue
      do 1761 i = 1,k
         alpha = x(n3+i)/cac
         ni = n+i
         do 176 j = 1,n
            h(ni,j) = h(ni,j)-alpha*x(nn+j)
 176     continue
 1761 continue
      goto 120

 180  continue
      z = 1.0D0/x(n4+ii)

c  Apply simplex formula to exchange constraints

      do 181 i = 1,n
         ni = n+i
         if (i.ne.ii) goto 182
         do 183 j = 1,n
            h(ni,j) = h(ni,j)*z
 183     continue
         goto 181

 182     continue
         zz = z*x(n4+i)
         do 184 j = 1,n
            h(ni,j) = h(ni,j)-zz*x(nn+j)
 184     continue
 181  continue
      lt(ii) = ial
      goto 120

 200  continue
      k = 0

      ke = 0
      do 202 i = 1,m
         lt(nn+i) = 1
 202  continue
      call hinv(h,ih,n,wa,iwa,info)
      if (info .ne. 1) goto 1000

c  Start with empty basis from feasible point
c  Search direction is -a(-1).b

      goto 167

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE HARWFP(
     +     n,m,c,ic,d,bdl,bdu,x,k,ke,h,ih,lt,wa,iwa,info)

c  This program is a modified version of the Harwell Library
c  subroutine LA02AD.  The modifications were made to substitute
c  the subroutines HINV and DOTPMC for Harwell subroutines
c  MB01B and MC03AS.  The calling sequence above includes
c  three entries, WA, IWA, and INFO not present in the
c  original program.  WA and IWA are real and
c  integer work arrays, respectively, and must be dimensioned
c  at least 2*N.  INFO is an output variable set to one for a
c  normal return and set to two when a singular matrix is detected
c  in HINV.  All other entries in the calling sequence are as
c  described in the Harwell documentation.
c
c  Modified  5/22/91 to use implicit none (J. Galambos)
c
c+**PJK 02/11/92 Throughout this routine, argument 1 of DOTPMC has
c+**PJK 02/11/92 different dimensions than are declared in the source
c+**PJK 02/11/92 code of the routine itself. The program runs without
c+**PJK 02/11/92 error but beware of future modifications.

      IMPLICIT NONE

      INTEGER n,m,ic,k,ke,ih,info
      INTEGER i, ial, ib, ii, j, jj, kv, li, ni, nj, nn, n3
      INTEGER iwa(*), lt(*)
      INTEGER i0,i1,i2,i3

      DOUBLE PRECISION c(ic,*),d(*),bdl(*),bdu(*),x(*),h(ih,*)
      DOUBLE PRECISION wa(*)
      DOUBLE PRECISION alpha, beta, y, z, zz
      DOUBLE PRECISION r0

      i0 = 0
      i1 = 1
      i2 = 2
      i3 = 3
      r0 = 0.0D0

      info = 1
      nn = n+n
      n3 = nn+n
      do 1 i = 1,m
         lt(nn+i) = 1
 1    continue

c  Constraints indexed as
c  -1=equality,  0=active,  1=inactive,  2=violated

      if (k.ne.0) goto 10

c  No designated constraints, vertex chosen from upper and
c  lower bounds, inverse matrix trivial

      do 4 i = 1,n
         do 5 j = 1,n
            h(i,j) = 0.0D0
 5       continue
         if ((x(i)-bdl(i)).gt.(bdu(i)-x(i))) goto 6
         lt(i) = i
         h(i,i) = 1.0D0
         goto 998

 6       continue
         lt(i) = n+i
         h(i,i) = -1.0D0
 998     continue
         lt(nn+lt(i)) = 0
 4    continue
      k = n
      goto 40

c  Set up normals v of the k designated constraints in basis

 10   continue
      do 11 i = 1,k
         j = 0
         if (i.le.ke) j = -1
         lt(nn+lt(i)) = j
         li = lt(i)
         ni = n+i
         if (li.gt.nn) goto 14
         do 12 j = 1,n
            h(j,ni) = 0.0D0
 12      continue
         if (li.gt.n) goto 13
         h(li,ni) = 1.0D0
         goto 11

 13      continue
         h(li-n,ni) = -1.0D0
         goto 11

 14      continue
         li = li-nn
         do 15 j = 1,n
            h(j,ni) = c(j,li)
 15      continue
 11   continue

      if (k.ne.n) goto 19
      do 161 j = 1,n
         nj = n+j
         do 16 i = 1,n
            h(i,j) = h(i,nj)
 16      continue
 161  continue
      call hinv(h,ih,n,wa,iwa,info)
      if (info .ne. 1) goto 1000
      goto 40

 19   continue

c  Form m = (vtranspose.v)(-1)
      do 2011 i = 1,k
         do 20 j = i,k
            call dotpmc(h(1,n+i),i1,h(1,n+j),i1,r0,h(i,j),n,i0)
            h(j,i) = h(i,j)
 20      continue
 2011 continue
      if (k.ne.1) goto 200
      h(1,1) = 1.0D0/h(1,1)
      goto  201

 200  continue
      call hinv(h,ih,k,wa,iwa,info)
      if (info .ne. 1) goto 1000

 201  continue

c  Calculate generalized inverse of v,  vplus = m.vtranspose

      do 211 i = 1,k
         do 22 j = 1,k
            x(n+j) = h(i,j)
 22      continue
         do 21 j = 1,n
            call dotpmc(x(n+1),i1,h(j,n+1),ih,r0,h(i,j),k,i0)
 21      continue
 211  continue

c  Set up diagonal elements of the projection matrix  p = v.vplus

      do 23 i = 1,n
         call dotpmc(h(1,i),i1,h(i,n+1),ih,r0,x(n+i),k,i0)
 23   continue
      do 24 i = 1,n
         lt(n+i) = 0
 24   continue
      kv = k

c  Add bound e(i) corresponding to the smallest diag(p)

 29   continue
      z = 1.0D0
      do 25 i = 1,n
         if (lt(n+i).eq.1) goto 25
         if (x(n+i).ge.z) goto 25
         z = x(n+i)
         ii = i
 25   continue
      y = 1.0D0
      if ( (x(ii)-bdl(ii)) .gt. (bdu(ii)-x(ii)) ) y = -1.0D0

c  Calculate vectors vplus.e(i) and  u = e(i)-v.vplus.e(i)

      if (y.ne.1.0D0) goto 27
      do 26 i = 1,k
         x(nn+i) = h(i,ii)
 26   continue
      goto 30

 27   continue
      do 28 i = 1,k
         x(nn+i) = -h(i,ii)
 28   continue

 30   continue
      do 31 i = 1,n
         if (lt(n+i).eq.1) goto 31
         call dotpmc(h(i,n+1),ih,x(nn+1),i1,r0,x(n3+i),kv,i3)
 31   continue
      do 32 i = 1,n
         h(i,ii) = 0.0D0
 32   continue
      lt(n+ii) = 1
      z = 1.0D0+x(n3+ii)*y

c  Update vplus and diag(p)

      do 33 i = 1,n
         if (lt(n+i).eq.1) goto 33
         alpha = x(n3+i)/z
         h(k+1,i) = alpha
         do 34 j = 1,k
            h(j,i) = h(j,i)-x(nn+j)*alpha
 34      continue
 33   continue

      do 35 i = 1,n
         if (lt(n+i).eq.1) goto 35
         x(n+i) = x(n+i)+x(n3+i)**2/z
 35   continue
      k = k+1
      h(k,ii) = y
      if (y.ne.1.0D0) ii = ii+n
      lt(nn+ii) = 0
      lt(k) = ii
      if (k.ne.n) goto 29

c  Set up rhs of constraints in basis

 40   continue
      do 41 i = 1,n
         li = lt(i)
         if (li.gt.n) goto 42
         x(n+i) = bdl(li)
         goto 41

 42      continue
         if (li.gt.nn) goto 43
         x(n+i) = -bdu(li-n)
         goto 41

 43      continue
         x(n+i) = d(li-nn)

 41   continue

c  Calculate position of vertex

      do 44 i = 1,n
         call dotpmc(h(1,i),i1,x(n+1),i1,r0,x(i),n,i0)
 44   continue

c  Calculate the constraint residuals, the number of violated
c  constraints, and the sum of their normals

 50   continue
      kv = 0
      do 51 i = 1,n
         x(n+i) = 0.0D0
 51   continue
      do 52 i = 1,m
         if (lt(nn+i).le.0) goto 52
         if (i.gt.n) goto 53
         z = x(i)-bdl(i)
         goto 55

 53      continue
         if (i.gt.nn) goto 54
         z = bdu(i-n)-x(i-n)
         goto 55

 54      continue
         j = i-nn
         call dotpmc(c(1,j),i1,x(1),i1,d(j),z,n,i2)

 55      continue
         x(nn+i) = z
         if (z.ge.0.0D0) goto 52
         kv = kv+1
         lt(nn+i) = 2
         if (i.gt.n) goto 56
         x(n+i) = x(n+i)+1.0D0
         goto 52

 56      continue
         if (i.gt.nn) goto 57
         x(i) = x(i)-1.0D0
         goto 52

 57      continue
         do 58 ii = 1,n
            x(n+ii) = x(n+ii)+c(ii,j)
 58      continue
 52   continue
      if (kv.ne.0) goto 63
      goto 1000

c  Possible directions of search obtainable by removing a
c  constraint are rows of h,  calculate the optimum direction

 63   continue
      z = 0.0D0
      do 64 i = 1,n
         if (lt(nn+lt(i)).eq.-1) goto 64
         call dotpmc(h(i,1),ih,x(n+1),i1,r0,y,n,i0)
         if (y.le.z) goto 64
         z = y
         ii = i
 64   continue
      if (z.gt.0.0D0) goto 70
      k = 0
      goto 1000

c  Search for the nearest of the furthest violated constraint
c  and the nearest nonviolated nonbasic constraint

 70   continue
C+**PJK 17/11/97 D999 reduced to D99
      alpha = 1.0D99
      beta = 0.0D0
      do 71 i = 1,n
         x(n+i) = h(ii,i)
 71   continue
      do 72 i = 1,m
         if (lt(nn+i).le.0) goto 72
         if (i.gt.n) goto 73
         z = -x(n+i)
         goto 75

 73      continue
         if (i.gt.nn) goto 74
         z = x(i)
         goto 75

 74      continue
         jj = i-nn
         call dotpmc(x(n+1),i1,c(1,jj),i1,r0,z,n,i3)

 75      continue
         if (lt(nn+i).eq.2) goto 76
         if (z.le.0.0D0) goto 72
         z = x(nn+i)/z
         if (z.ge.alpha) goto 72
         alpha = z
         ial = i
         goto 72

 76      continue
         lt(nn+i) = 1
         if (z.ge.0.0D0) goto 72
         z = x(nn+i)/z
         if (z.le.beta) goto 72
         beta = z
         ib = i
 72   continue

      if (alpha.gt.beta) goto 80
      ib = ial
      beta = alpha

c  Exchange with the constraint being removed from the basis,
c  using simplex formula for new h

 80   continue
      lt(nn+lt(ii)) = 1
      lt(nn+ib) = 0
      lt(ii) = ib
      if (ib.gt.n) goto 82
      do 81 i = 1,n
         x(nn+i) = h(i,ib)
 81   continue
      goto 90

 82   continue
      ib = ib-n
      if (ib.gt.n) goto 84
      do 83 i = 1,n
         x(nn+i) = -h(i,ib)
 83   continue
      goto 90

 84   continue
      ib = ib-n
      do 85 i = 1,n
         x(n3+i) = c(i,ib)
 85   continue
      do 86 i = 1,n
         call dotpmc(h(i,1),ih,x(n3+1),i1,r0,x(nn+i),n,i0)
 86   continue

 90   continue
      z = 1.0D0/x(nn+ii)
      do 91 i = 1,n
         x(i) = x(i)+beta*x(n+i)
         if (i.ne.ii) goto 92
         do 93 j = 1,n
            h(i,j) = h(i,j)*z
 93      continue
         goto 91

 92      continue
         zz = z*x(nn+i)
         do 94 j = 1,n
            h(i,j) = h(i,j)-zz*x(n+j)
 94      continue
 91   continue

      goto 50

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE HINV(h,ih,n,work,ipvt,info)

c  This subroutine inverts the matrix H by use of linpack software.
c
c  The subroutine statement is
c
c  subroutine hinv(h,ih,n,work,ipvt,info)
c
c  where
c
c  H is a real IH by IH array which contains the N by N
c  matrix to be inverted.  On output the N by N inverse
c  is stored in H.
c
c  IH is an input integer variable set to the fortran
c  declaration of the leading dimension in the H array.
c
c  N is the order of H.  N must be less than or equal to IH.
c
c  WORK is a real work array of length at least N.
c
c  IPVT is an integer work array of length at least N.
c
c  INFO is an integer output variable set as follows
c
c   INFO = 1  normal return
c
c   INFO = 2  H matrix is singular
c
c  Algorithm version of June 1979
c
c  Roger L. Crane, Kenneth E. Hillstrom, Michael Minkoff

      IMPLICIT NONE

      INTEGER ih,n,info
      INTEGER ipvt(n)
      DOUBLE PRECISION h(ih,ih),work(n)

c  Do lu decomposition of h

      call sgefa(h,ih,n,ipvt,info)

      if (info .eq. 0) goto 20
      info = 2
      goto 1000

c  Form inverse of h

 20   continue
      info = 1
      call sgedi(h,ih,n,ipvt,work,work,1)

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE DOTPMC(x,ix,y,iy,c,sum,n,iflag)

c  This subroutine computes
c   sum = (plus or minus c) plus or minus the dot product of x and y
c         by invoking the basic linear algebra (bla) routine dot.
c
c  The subroutine statement is
c
c  subroutine dotpmc(x,ix,y,iy,c,sum,n,iflag)
c
c  where
c
c  X is a real input array of length at least IX*N.
c
c  IX is a positive integer input variable set to the interval
c  in storage between elements in the X array.
c
c  Y is a real input array of length at least IY*N.
c
c  IY is a positive integer input variable set to the interval
c  in storage between elements in the Y array.
c
c  C is a real input variable that is added to or subtracted
c  from the dot product.
c
c  SUM is a real output variable set to the computed result.
c
c  N is a positive integer input variable set to the number
c  of terms in the dot product.
c
c  IFLAG is an integer input variable which determines the signs
c  in the calculation as follows:
c   IFLAG = 0    +c+x dot y is computed
c   IFLAG = 1    +c-x dot y is computed
c   IFLAG = 2    -c+x dot y is computed
c   IFLAG = 3    -c-x dot y is computed
c
c  Roger L. Crane, Kenneth E. Hillstrom, Michael Minkoff

      IMPLICIT NONE

      INTEGER ix,iy,n,iflag

      DOUBLE PRECISION c,sum
      DOUBLE PRECISION x(*),y(*)
      DOUBLE PRECISION prod

      DOUBLE PRECISION sdot
      EXTERNAL         sdot

c  Calculate dot product

      prod = sdot(n,x,ix,y,iy)
      if (mod(iflag,2) .ne. 0) prod = - prod

      sum = c + prod
      if (iflag .gt. 1) sum = - c + prod

      return
      end
c______________________________________________________________________
      SUBROUTINE SGEFA(a,lda,n,ipvt,info)

c  SGEFA factors a real matrix by gaussian elimination.
c
c  SGEFA is usually called by SGECO, but it can be called
c  directly with a saving in time if  RCOND  is not needed.
c  (time for SGECO) = (1 + 9/N)*(time for SGEFA) .
c
c  On entry :
c
c  a       real(lda, n)
c          the matrix to be factored.
c
c  lda     integer
c          the leading dimension of the array  a .
c
c  n       integer
c          the order of the matrix  a .
c
c  On return :
c
c  a       an upper triangular matrix and the multipliers
c          which were used to obtain it.
c          the factorization can be written  a = l*u  where
c          l  is a product of permutation and unit lower
c          triangular matrices and  u  is upper triangular.
c
c  ipvt    integer(n)
c          an integer vector of pivot indices.
c
c  info    integer
c          = 0  normal value.
c          = k  if  u(k,k) .eq. 0.0 .  this is not an error
c               condition for this subroutine, but it does
c               indicate that sgesl or sgedi will divide by zero
c               if called.  use  rcond  in sgeco for a reliable
c               indication of singularity.
c
c  Linpack. This version dated 08/14/78 .
c  Cleve Moler, University of New Mexico, Argonne National Lab.

      IMPLICIT NONE

      INTEGER lda,n,ipvt(*),info,j,k,kp1,l,nm1

      DOUBLE PRECISION a(lda,*),t

      INTEGER  isamax
      EXTERNAL isamax

c  Gaussian elimination with partial pivoting

      info = 0
      nm1 = n - 1
      if (nm1 .lt. 1) goto 70
      do 60 k = 1, nm1
         kp1 = k + 1

c  Find l = pivot index

         l = isamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k) = l

c  Zero pivot implies this column already triangularized

         if (a(l,k) .eq. 0.0D0) goto 40

c  Interchange if necessary

         if (l .eq. k) goto 10
         t = a(l,k)
         a(l,k) = a(k,k)
         a(k,k) = t

 10      continue

c  Compute multipliers

         t = -1.0D0/a(k,k)

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Arg 3 in call to SSCAL has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

         call sscal(n-k,t,a(k+1,k),1)

c  Row elimination with column indexing

         do 30 j = kp1, n
            t = a(l,j)
            if (l .eq. k) goto 20
            a(l,j) = a(k,j)
            a(k,j) = t

 20         continue

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Args 3 and 5 in call to SAXPY has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

            call saxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
 30      continue
         goto 50

 40      continue
         info = k

 50      continue
 60   continue

 70   continue
      ipvt(n) = n
      if (a(n,n) .eq. 0.0D0) info = n

      return
      end
c______________________________________________________________________
      SUBROUTINE SGEDI(a,lda,n,ipvt,det,work,job)

c  SGEDI computes the determinant and inverse of a matrix
c  using the factors computed by SGECO or SGEFA.
c
c  On entry :
c
c  a       real(lda, n)
c          the output from sgeco or sgefa.
c
c  lda     integer
c          the leading dimension of the array  a .
c
c  n       integer
c          the order of the matrix  a .
c
c  ipvt    integer(n)
c          the pivot vector from sgeco or sgefa.
c
c  work    real(n)
c          work vector.  contents destroyed.
c
c  job     integer
c          = 11   both determinant and inverse.
c          = 01   inverse only.
c          = 10   determinant only.
c
c  On return :
c
c  a       inverse of original matrix if requested.
c          otherwise unchanged.
c
c  det     real(2)
c          determinant of original matrix if requested.
c          otherwise not referenced.
c          determinant = det(1) * 10.0**det(2)
c          with  1.0 .le. abs(det(1)) .lt. 10.0
c          or  det(1) .eq. 0.0 .
c
c  Error condition :
c
c  A division by zero will occur if the input factor contains
c  a zero on the diagonal and the inverse is requested.
c  It will not occur if the subroutines are called correctly
c  and if SGECO has set RCOND .gt. 0.0 or SGEFA has set
c  INFO .eq. 0 .
c
c  Linpack. This version dated 08/14/78 .
c  Cleve Moler, University of New Mexico, Argonne National Lab.

      IMPLICIT NONE

      INTEGER lda,n,ipvt(*),job
      INTEGER i,j,k,kk,kb,kp1,l,nm1

      DOUBLE PRECISION a(lda,*),det(2),work(*)
      DOUBLE PRECISION t
      DOUBLE PRECISION ten

c  Compute determinant

      if ((job/10) .eq. 0) goto 70

      det(1) = 1.0D0
      det(2) = 0.0D0
      ten = 10.0D0

      do 50 i = 1, n
         if (ipvt(i) .ne. i) det(1) = -det(1)
         det(1) = a(i,i)*det(1)

c  Exit
         if (det(1) .eq. 0.0D0) goto 60

 10      continue
         if (abs(det(1)) .ge. 1.0D0) goto 20
         det(1) = ten*det(1)
         det(2) = det(2) - 1.0D0
         goto 10

 20      continue
 30      continue
         if (abs(det(1)) .lt. ten) goto 40
         det(1) = det(1)/ten
         det(2) = det(2) + 1.0D0
         goto 30

 40      continue
 50   continue

 60   continue
 70   continue

c  Compute inverse(u)

      if (mod(job,10) .eq. 0) goto 150
      do 100 k = 1, n
         a(k,k) = 1.0D0/a(k,k)
         t = -a(k,k)

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Arg 3 in call to SSCAL has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

         call sscal(k-1,t,a(1,k),1)
         kp1 = k + 1
         if (n .lt. kp1) goto 90
         do 80 j = kp1, n
            t = a(k,j)
            a(k,j) = 0.0D0
            kk = k

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Args 3 and 5 in call to SAXPY has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

            call saxpy(kk,t,a(1,k),1,a(1,j),1)
 80      continue

 90      continue
 100  continue

c  Form inverse(u)*inverse(l)

      nm1 = n - 1
      if (nm1 .lt. 1) goto 140
      do 130 kb = 1, nm1
         k = n - kb
         kp1 = k + 1

         do 110 i = kp1, n
            work(i) = a(i,k)
            a(i,k) = 0.0D0
 110     continue

         do 120 j = kp1, n
            t = work(j)

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Args 3 and 5 in call to SAXPY has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

            call saxpy(n,t,a(1,j),1,a(1,k),1)
 120     continue

         l = ipvt(k)

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Args 2 and 4 in call to SSWAP has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

         if (l .ne. k) call sswap(n,a(1,k),1,a(1,l),1)
 130  continue

 140  continue
 150  continue

      return
      end
c______________________________________________________________________
      SUBROUTINE SSCAL(n,sa,sx,incx)

c  Scales a vector by a constant.
c  Uses unrolled loops for increment equal to 1.
c  Jack Dongarra, Linpack, 3/11/78.
c  Modified to correct problem with negative increments, 9/29/88.

      IMPLICIT NONE

      DOUBLE PRECISION sa,sx(*)

      INTEGER i,ix,incx,m,mp1,n

      if (n.le.0) goto 1000
      if (incx.eq.1) goto 20

c  Code for increment not equal to 1

      ix = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      do 10 i = 1,n
         sx(ix) = sa*sx(ix)
         ix = ix + incx
 10   continue
      goto 1000

c  Code for increment equal to 1

c  Clean-up loop

 20   continue
      m = mod(n,5)
      if ( m .eq. 0 ) goto 40
      do 30 i = 1,m
         sx(i) = sa*sx(i)
 30   continue
      if ( n .lt. 5 ) goto 1000

 40   continue
      mp1 = m + 1
      do 50 i = mp1,n,5
         sx(i)     = sa*sx(i)
         sx(i + 1) = sa*sx(i + 1)
         sx(i + 2) = sa*sx(i + 2)
         sx(i + 3) = sa*sx(i + 3)
         sx(i + 4) = sa*sx(i + 4)
 50   continue

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE SAXPY(n,sa,sx,incx,sy,incy)

c  Constant times a vector plus a vector.
c  Uses unrolled loop for increments equal to one.
c  Jack Dongarra, Linpack, 3/11/78.

      IMPLICIT NONE

      DOUBLE PRECISION sx(*),sy(*),sa

      INTEGER i,incx,incy,ix,iy,m,mp1,n

      if (n.le.0) goto 1000
      if (sa .eq. 0.0D0) goto 1000
      if ((incx.eq.1).and.(incy.eq.1)) goto 20

c  Code for unequal increments or equal increments not equal to 1

      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
         sy(iy) = sy(iy) + sa*sx(ix)
         ix = ix + incx
         iy = iy + incy
 10   continue
      goto 1000

c  Code for both increments equal to 1

c  Clean-up loop

 20   continue
      m = mod(n,4)
      if ( m .eq. 0 ) goto 40
      do 30 i = 1,m
         sy(i) = sy(i) + sa*sx(i)
 30   continue
      if( n .lt. 4 ) goto 1000

 40   continue
      mp1 = m + 1
      do 50 i = mp1,n,4
         sy(i)     = sy(i)     + sa*sx(i)
         sy(i + 1) = sy(i + 1) + sa*sx(i + 1)
         sy(i + 2) = sy(i + 2) + sa*sx(i + 2)
         sy(i + 3) = sy(i + 3) + sa*sx(i + 3)
 50   continue

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE SSWAP(n,sx,incx,sy,incy)

c  Interchanges two vectors.
c  Uses unrolled loops for increments equal to 1.
c  Jack Dongarra, Linpack, 3/11/78.

      IMPLICIT NONE

      DOUBLE PRECISION sx(*),sy(*),stemp

      INTEGER i,incx,incy,ix,iy,m,mp1,n

      if (n.le.0) goto 1000
      if ((incx.eq.1).and.(incy.eq.1)) goto 20

c  Code for unequal increments or equal increments not equal to 1

      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
         stemp = sx(ix)
         sx(ix) = sy(iy)
         sy(iy) = stemp
         ix = ix + incx
         iy = iy + incy
 10   continue
      goto 1000

c  Code for both increments equal to 1

c  Clean-up loop

 20   continue
      m = mod(n,3)
      if ( m .eq. 0 ) goto 40
      do 30 i = 1,m
         stemp = sx(i)
         sx(i) = sy(i)
         sy(i) = stemp
 30   continue
      if ( n .lt. 3 ) goto 1000

 40   continue
      mp1 = m + 1
      do 50 i = mp1,n,3
         stemp = sx(i)
         sx(i) = sy(i)
         sy(i) = stemp
         stemp = sx(i + 1)
         sx(i + 1) = sy(i + 1)
         sy(i + 1) = stemp
         stemp = sx(i + 2)
         sx(i + 2) = sy(i + 2)
         sy(i + 2) = stemp
 50   continue

 1000 continue

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION SDOT(N,SX,INCX,SY,INCY)

C  Computes X * Y where X and Y are vectors
C
C  SDOT(N,SX,INCX,SY,INCY) computes
C
C  SW = sum (from i=1 to N) X(i)*Y(i)
C
C  where X and Y are real vectors.

C  N          is the number of elements in the vector.
C
C  SX         is the vector operand.
C
C  INCX       is the skip distance between elements of SX. For
C             contiguous elements, INCX=1.
C
C  SY         is the vector operand.
C
C  INCY       is the skip distance between elements of SY. For
C             contiguous elements, INCY=1.

      IMPLICIT NONE

      INTEGER n,incx,incy,ix,i,iy
      DOUBLE PRECISION sx(n*incx),sy(n*incy),sw

      sw = 0.0D0
      ix = 1
      iy = 1
      do 1000 i = 1,n
         sw = sw + (sx(ix) * sy(iy))
         ix = ix + incx
         iy = iy + incy
 1000 continue

      sdot = sw

      return
      end
c______________________________________________________________________
      INTEGER FUNCTION ISAMAX(n,sx,incx)

c  Finds the index of element having max. absolute value.
c  Jack Dongarra, Linpack, 3/11/78.
c  Modified to correct problem with negative increments, 9/29/88.

      IMPLICIT NONE

      DOUBLE PRECISION sx(*),smax

      INTEGER i,incx,ix,n

      isamax = 0
      if ( n .lt. 1 ) goto 1000
      isamax = 1
      if (n.eq.1) goto 1000
      if (incx.eq.1) goto 20

c  Code for increment not equal to 1

      ix = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      smax = abs(sx(ix))
      ix = ix + incx
      do 10 i = 2,n
         if (abs(sx(ix)).le.smax) goto 5
         isamax = i
         smax = abs(sx(ix))
 5       continue
         ix = ix + incx
 10   continue
      goto 1000

c  Code for increment equal to 1

 20   continue
      smax = abs(sx(1))
      do 30 i = 2,n
         if (abs(sx(i)).le.smax) goto 30
         isamax = i
         smax = abs(sx(i))
 30   continue

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE HYBRD(
     +     fcnhyb,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,diag,
     +     mode,factor,nprint,info,nfev,fjac,ldfjac,r,lr,
     +     qtf,wa1,wa2,wa3,wa4,resdl)

c  The purpose of HYBRD is to find a zero of a system of
c  N nonlinear functions in N variables by a modification
c  of the Powell Hybrid method. The user must provide a
c  subroutine which calculates the functions. The Jacobian is
c  then calculated by a forward-difference approximation.
c
c  The subroutine statement is
c
c  subroutine hybrd(fcnhyb,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,
c                   diag,mode,factor,nprint,info,nfev,fjac,
c                   ldfjac,r,lr,qtf,wa1,wa2,wa3,wa4)
c
c  where
c
c  FCNHYB is the name of the user-supplied subroutine which
c  calculates the functions. FCNHYB must be declared
c  in an external statement in the user calling
c  program, and should be written as follows.
c
c   subroutine fcnhyb(n,x,fvec,iflag)
c   integer n,iflag
c   real x(n),fvec(n)
c   ----------
c   calculate the functions at x and
c   return this vector in fvec.
c   ---------
c   return
c   end
c
c  The value of IFLAG should not be changed by FCNHYB unless
c  the user wants to terminate execution of HYBRD.
c  In this case set IFLAG to a negative integer.
c
c  N is a positive integer input variable set to the number
c  of functions and variables.
c
c  X is an array of length N. On input X must contain
c  an initial estimate of the solution vector. On output X
c  contains the final estimate of the solution vector.
c
c  FVEC is an output array of length N which contains
c  the functions evaluated at the output X.
c
c  XTOL is a nonnegative input variable. Termination
c  occurs when the relative error between two consecutive
c  iterations is at most XTOL.
c
c  MAXFEV is a positive integer input variable. Termination
c  occurs when the number of calls to FCNHYB is at least MAXFEV
c  by the end of an iteration.
c
c  ML is a nonnegative integer input variable which specifies
c  the number of subdiagonals within the band of the
c  Jacobian matrix. If the Jacobian is not banded, set
c  ML to at least N - 1.
c
c  MU is a nonnegative integer input variable which specifies
c  the number of superdiagonals within the band of the
c  Jacobian matrix. If the Jacobian is not banded, set
c  MU to at least N - 1.
c
c  EPSFCN is an input variable used in determining a suitable
c  step length for the forward-difference approximation. This
c  approximation assumes that the relative errors in the
c  functions are of the order of EPSFCN. If EPSFCN is less
c  than the machine precision, it is assumed that the relative
c  errors in the functions are of the order of the machine
c  precision.
c
c  DIAG is an array of length N. If MODE = 1 (see
c  below), DIAG is internally set. If MODE = 2, DIAG
c  must contain positive entries that serve as
c  multiplicative scale factors for the variables.
c
c  MODE is an integer input variable. If MODE = 1, the
c  variables will be scaled internally. If MODE = 2,
c  the scaling is specified by the input DIAG. Other
c  values of MODE are equivalent to MODE = 1.
c
c  FACTOR is a positive input variable used in determining the
c  initial step bound. This bound is set to the product of
c  FACTOR and the Euclidean norm of DIAG*X if nonzero, or else
c  to FACTOR itself. In most cases FACTOR should lie in the
c  interval (.1,100.). 100. is a generally recommended value.
c
c  NPRINT is an integer input variable that enables controlled
c  printing of iterations if it is positive. In this case,
c  FCNHYB is called with IFLAG = 0 at the beginning of the first
c  iteration and every NPRINT iterations thereafter and
c  immediately prior to return, with X and FVEC available
c  for printing. If NPRINT is not positive, no special calls
c  of FCNHYB with IFLAG = 0 are made.
c
c  INFO is an integer output variable. If the user has
c  terminated execution, INFO is set to the (negative)
c  value of IFLAG. see description of FCNHYB. Otherwise,
c  INFO is set as follows.
c
c   INFO = 0   improper input parameters.
c
c   INFO = 1   relative error between two consecutive iterates
c              is at most XTOL.
c
c   INFO = 2   number of calls to FCNHYB has reached or exceeded
c              MAXFEV.
c
c   INFO = 3   XTOL is too small. No further improvement in
c              the approximate solution X is possible.
c
c   INFO = 4   iteration is not making good progress, as
c              measured by the improvement from the last
c              five Jacobian evaluations.
c
c   INFO = 5   iteration is not making good progress, as
c              measured by the improvement from the last
c              ten iterations.
c
c  NFEV is an integer output variable set to the number of
c  calls to FCNHYB.
c
c  FJAC is an output N by N array which contains the
c  orthogonal matrix Q produced by the QR factorization
c  of the final approximate Jacobian.
c
c  LDFJAC is a positive integer input variable not less than N
c  which specifies the leading dimension of the array FJAC.
c
c  R is an output array of length LR which contains the
c  upper triangular matrix produced by the QR factorization
c  of the final approximate Jacobian, stored rowwise.
c
c  LR is a positive integer input variable not less than
c  (N*(N+1))/2.
c
c  QTF is an output array of length N which contains
c  the vector (Q transpose)*FVEC.
c
c  WA1, WA2, WA3, and WA4 are work arrays of length N.
c
c  Subprograms called
c
c   user-supplied ...... fcnhyb
c
c   minpack-supplied ... dogleg,spmpar,enorm,fdjac1,
c                        qform,qrfac,r1mpyq,r1updt
c
c  Argonne National Laboratory. Minpack project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

      IMPLICIT NONE

      INTEGER n,maxfev,ml,mu,mode,nprint,info,nfev,ldfjac,lr,irr
      INTEGER i,iflag,iter,j,jm1,l,msum,ncfail,ncsuc,nslow1,nslow2

C+**PJK 08/10/92 Possible problems with the following declaration:
      INTEGER iwa(1)

      DOUBLE PRECISION xtol,epsfcn,factor
      DOUBLE PRECISION x(n),fvec(n),diag(n),fjac(ldfjac,n),r(lr),
     +     qtf(n),wa1(n),wa2(n),wa3(n),wa4(n),resdl(n)
      DOUBLE PRECISION actred,delta,epsmch,fnorm,fnorm1,one,pnorm,
     +     prered,p1,p5,p001,p0001,ratio,sum,temp,xnorm,zero
      logical jeval,sing

      DOUBLE PRECISION spmpar,enorm
      EXTERNAL     fcnhyb,spmpar,enorm

      one = 1.0D0
      p1 = 0.1D0
      p5 = 0.5D0
      p001 = 1.0D-3
      p0001 = 1.0D-4
      zero = 0.0D0

c  Machine precision

      epsmch = spmpar(1)

      info = 0
      iflag = 0
      nfev = 0

c  Check the input parameters for errors.

      if (
     +     (n.le.0)         .or.
     +     (xtol.lt.zero)   .or.
     +     (maxfev.le.0)    .or.
     +     (ml.lt.0)        .or.
     +     (mu.lt.0)        .or.
     +     (factor.le.zero) .or.
     +     (ldfjac.lt.n)    .or.
     +     (lr.lt.( ( n*(n + 1) ) /2))
     +     ) goto 300

      if (mode .ne. 2) goto 20
      do 10 j = 1, n
         if (diag(j) .le. zero) goto 300
 10   continue

 20   continue

c  Evaluate the function at the starting point
c  and calculate its norm.

      iflag = 1
      call fcnhyb(n,x,fvec,iflag)
      nfev = 1

      if (iflag .lt. 0) goto 300
      fnorm = enorm(n,fvec)

c  Determine the number of calls to FCNHYB needed to compute
c  the Jacobian matrix.

      msum = min(ml+mu+1,n)

c  Initialize iteration counter and monitors.

      iter = 1
      ncsuc = 0
      ncfail = 0
      nslow1 = 0
      nslow2 = 0

c  Beginning of the outer loop.

 30   continue
      jeval = .true.

c  Calculate the Jacobian matrix.

      iflag = 2
      call fdjac1(
     +     fcnhyb,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,wa1,wa2)
      nfev = nfev + msum
      if (iflag .lt. 0) goto 300

c  Compute the qr factorization of the Jacobian.

      call qrfac(n,n,fjac,ldfjac,.false.,iwa,1,wa1,wa2,wa3)

c  On the first iteration and if mode is 1, scale according
c  to the norms of the columns of the initial Jacobian.

      if (iter .ne. 1) goto 70
      if (mode .eq. 2) goto 50
      do 40 j = 1, n
         diag(j) = wa2(j)
         if (wa2(j) .eq. zero) diag(j) = one
 40   continue

 50   continue

c  On the first iteration, calculate the norm of the scaled x
c  and initialize the step bound delta.

      do 60 j = 1, n
         wa3(j) = diag(j)*x(j)
 60   continue
      xnorm = enorm(n,wa3)
      delta = factor*xnorm
      if (delta .eq. zero) delta = factor

 70   continue

c  Form (q transpose)*fvec and store in qtf.

      do 80 i = 1, n
         qtf(i) = fvec(i)
 80   continue
      do 120 j = 1, n
         if (fjac(j,j) .eq. zero) goto 110
         sum = zero
         do 90 i = j, n
            sum = sum + fjac(i,j)*qtf(i)
 90      continue
         temp = -sum/fjac(j,j)
         do 100 i = j, n
            qtf(i) = qtf(i) + fjac(i,j)*temp
 100     continue

 110     continue
 120  continue

c  Copy the triangular factor of the qr factorization into r.

      sing = .false.
      do 150 j = 1, n
         l = j
         jm1 = j - 1
         if (jm1 .lt. 1) goto 140
         do 130 i = 1, jm1
            r(l) = fjac(i,j)
            l = l + n - i
 130     continue

 140     continue
         r(l) = wa1(j)
         if (wa1(j) .eq. zero) sing = .true.
 150  continue

c  Accumulate the orthogonal factor in fjac.

      call qform(n,n,fjac,ldfjac,wa1)

c  Rescale if necessary.

      if (mode .eq. 2) goto 170
      do 160 j = 1, n
         diag(j) = max(diag(j),wa2(j))
 160  continue

 170  continue

c  Beginning of the inner loop.

 180  continue

c  If requested, call FCNHYB to enable printing of iterates.

      if (nprint .le. 0) goto 190
      iflag = 0
      if (mod(iter-1,nprint) .eq. 0) call fcnhyb(n,x,fvec,iflag)
      if (iflag .lt. 0) goto 300

 190  continue

c  Determine the direction p.

      call dogleg(n,r,lr,diag,qtf,delta,wa1,wa2,wa3)

c  Store the direction p and x + p. Calculate the norm of p.

      do 200 j = 1, n
         wa1(j) = -wa1(j)
         wa2(j) = x(j) + wa1(j)
         wa3(j) = diag(j)*wa1(j)
 200  continue
      pnorm = enorm(n,wa3)

c  On the first iteration, adjust the initial step bound.

      if (iter .eq. 1) delta = min(delta,pnorm)

c  Evaluate the function at x + p and calculate its norm.

      iflag = 1
      call fcnhyb(n,wa2,wa4,iflag)
      nfev = nfev + 1
      if (iflag .lt. 0) goto 300
      fnorm1 = enorm(n,wa4)

c  Compute the scaled actual reduction.

      actred = -one
      if (fnorm1 .lt. fnorm) actred = one - (fnorm1/fnorm)**2

c  Compute the scaled predicted reduction.

      l = 1
      do 220 i = 1, n
         sum = zero
         do 210 j = i, n
            sum = sum + r(l)*wa1(j)
            l = l + 1
 210     continue
         wa3(i) = qtf(i) + sum
 220  continue
      temp = enorm(n,wa3)
      prered = zero
      if (temp .lt. fnorm) prered = one - (temp/fnorm)**2

c  Compute the ratio of the actual to the predicted reduction.

      ratio = zero
      if (prered .gt. zero) ratio = actred/prered

c  Update the step bound.

      if (ratio .ge. p1) goto 230
      ncsuc = 0
      ncfail = ncfail + 1
      delta = p5*delta
      goto 240
 230  continue
      ncfail = 0
      ncsuc = ncsuc + 1
      if (ratio .ge. p5 .or. ncsuc .gt. 1)
     +     delta = max(delta,pnorm/p5)
      if (abs(ratio-one) .le. p1) delta = pnorm/p5
 240  continue

c  Test for successful iteration.

      if (ratio .lt. p0001) goto 260

c  Successful iteration. Update x, fvec, and their norms.

      do 250 j = 1, n
         x(j) = wa2(j)
         wa2(j) = diag(j)*x(j)
         fvec(j) = wa4(j)
 250  continue
      xnorm = enorm(n,wa2)
      fnorm = fnorm1
      iter = iter + 1
 260  continue

c  Determine the progress of the iteration.

      nslow1 = nslow1 + 1
      if (actred .ge. p001) nslow1 = 0
      if (jeval) nslow2 = nslow2 + 1
      if (actred .ge. p1) nslow2 = 0

c  Test for convergence.

      if ((delta .le. (xtol*xnorm)) .or. (fnorm .eq. zero)) info = 1
      if (info .ne. 0) goto 300

c  Tests for termination and stringent tolerances.

      if (nfev .ge. maxfev) info = 2
      if ((p1*max(p1*delta,pnorm)).le.(epsmch*xnorm)) info = 3
      if (nslow2 .eq. 5) info = 4
      if (nslow1 .eq. 10) info = 5
      if (info .ne. 0) goto 300

c  Criterion for recalculating Jacobian approximation
c  by forward differences.

      if (ncfail .eq. 2) goto 290

c  Calculate the rank one modification to the Jacobian
c  and update qtf if necessary.

      do 280 j = 1, n
         sum = zero
         do 270 i = 1, n
            sum = sum + fjac(i,j)*wa4(i)
 270     continue
         wa2(j) = (sum - wa3(j))/pnorm
         wa1(j) = diag(j)*((diag(j)*wa1(j))/pnorm)
         if (ratio .ge. p0001) qtf(j) = sum
 280  continue

c  Compute the qr factorization of the updated Jacobian.

      call r1updt(n,n,r,lr,wa1,wa2,wa3,sing)
      call r1mpyq(n,n,fjac,ldfjac,wa2,wa3)

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Arg 3 in call to R1MPYQ has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

      call r1mpyq(1,n,qtf,1,wa2,wa3)

c  End of the inner loop.

      jeval = .false.
      goto 180

 290  continue

c  End of the outer loop.

      goto 30

 300  continue

c  Termination, either normal or user imposed.

      if (iflag .lt. 0) info = iflag
      iflag = 0
      if (nprint .gt. 0) call fcnhyb(n,x,fvec,iflag)

      do 999 irr=1,n
         resdl(irr)=abs(qtf(irr))
 999  continue

      return
      end
c______________________________________________________________________
      SUBROUTINE DOGLEG(n,r,lr,diag,qtb,delta,x,wa1,wa2)

c  Given an M by N matrix A, an N by N nonsingular diagonal
c  matrix D, an M-vector B, and a positive number DELTA, the
c  problem is to determine the convex combination X of the
c  Gauss-Newton and scaled gradient directions that minimizes
c  (A*X - B) in the least squares sense, subject to the
c  restriction that the Euclidean norm of D*X be at most DELTA.
c
c  This subroutine completes the solution of the problem
c  if it is provided with the necessary information from the
c  QR factorization of A. That is, if A = Q*R, where Q has
c  orthogonal columns and R is an upper triangular matrix,
c  then DOGLEG expects the full upper triangle of R and
c  the first N components of (Q transpose)*B.
c
c  The subroutine statement is
c
c  subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)
c
c  where
c
c  N is a positive integer input variable set to the order of R.
c
c  R is an input array of length LR which must contain the upper
c  triangular matrix R stored by rows.
c
c  LR is a positive integer input variable not less than
c  (N*(N+1))/2.
c
c  DIAG is an input array of length N which must contain the
c  diagonal elements of the matrix D.
c
c  QTB is an input array of length N which must contain the first
c  N elements of the vector (Q transpose)*B.
c
c  DELTA is a positive input variable which specifies an upper
c  bound on the Euclidean norm of D*X.
c
c  X is an output array of length N which contains the desired
c  convex combination of the Gauss-Newton direction and the
c  scaled gradient direction.
c
c  WA1 and WA2 are work arrays of length N.
c
c  Argonne National Laboratory. Minpack project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

      IMPLICIT NONE

      INTEGER n,lr,i,j,jj,jp1,k,l

      DOUBLE PRECISION delta
      DOUBLE PRECISION r(lr),diag(n),qtb(n),x(n),wa1(n),wa2(n)
      DOUBLE PRECISION alpha,bnorm,epsmch,gnorm,one,qnorm,sgnorm,
     +     sum,temp,zero

      DOUBLE PRECISION spmpar,enorm
      EXTERNAL         spmpar,enorm

      one = 1.0D0
      zero = 0.0D0

c  Machine precision

      epsmch = spmpar(1)

c  First, calculate the Gauss-Newton direction.

      jj = (n*(n + 1))/2 + 1
      do 50 k = 1, n
         j = n - k + 1
         jp1 = j + 1
         jj = jj - k
         l = jj + 1
         sum = zero
         if (n .lt. jp1) goto 20
         do 10 i = jp1, n
            sum = sum + r(l)*x(i)
            l = l + 1
 10      continue

 20      continue
         temp = r(jj)
         if (temp .ne. zero) goto 40
         l = j
         do 30 i = 1, j
            temp = max(temp,abs(r(l)))
            l = l + n - i
 30      continue
         temp = epsmch*temp
         if (temp .eq. zero) temp = epsmch

 40      continue
         x(j) = (qtb(j) - sum)/temp
 50   continue

c  Test whether the Gauss-Newton direction is acceptable.

      do 60 j = 1, n
         wa1(j) = zero
         wa2(j) = diag(j)*x(j)
 60   continue
      qnorm = enorm(n,wa2)
      if (qnorm .le. delta) goto 140

c  The Gauss-Newton direction is not acceptable.
c  Next, calculate the scaled gradient direction.

      l = 1
      do 80 j = 1, n
         temp = qtb(j)
         do 70 i = j, n
            wa1(i) = wa1(i) + r(l)*temp
            l = l + 1
 70      continue
         wa1(j) = wa1(j)/diag(j)
 80   continue

c  Calculate the norm of the scaled gradient and test for
c  the special case in which the scaled gradient is zero.

      gnorm = enorm(n,wa1)
      sgnorm = zero
      alpha = delta/qnorm
      if (gnorm .eq. zero) goto 120

c  Calculate the point along the scaled gradient
c  at which the quadratic is minimized.

      do 90 j = 1, n
         wa1(j) = (wa1(j)/gnorm)/diag(j)
 90   continue
      l = 1
      do 110 j = 1, n
         sum = zero
         do 100 i = j, n
            sum = sum + r(l)*wa1(i)
            l = l + 1
 100     continue
         wa2(j) = sum
 110  continue
      temp = enorm(n,wa2)
      sgnorm = (gnorm/temp)/temp

c  Test whether the scaled gradient direction is acceptable.

      alpha = zero
      if (sgnorm .ge. delta) goto 120

c  The scaled gradient direction is not acceptable.
c  Finally, calculate the point along the dogleg
c  at which the quadratic is minimized.

      bnorm = enorm(n,qtb)
      temp = (bnorm/gnorm)*(bnorm/qnorm)*(sgnorm/delta)
      temp = temp - (delta/qnorm)*(sgnorm/delta)**2
     +     + sqrt((temp-(delta/qnorm))**2
     +     + (one-(delta/qnorm)**2)*(one-(sgnorm/delta)**2))
      alpha = ((delta/qnorm)*(one - (sgnorm/delta)**2))/temp

 120  continue

c  Form appropriate convex combination of the Gauss-Newton
c  direction and the scaled gradient direction.

      temp = (one - alpha)*min(sgnorm,delta)
      do 130 j = 1, n
         x(j) = temp*wa1(j) + alpha*x(j)
 130  continue

 140  continue

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION ENORM(n,x)

c  Given an N-vector X, this function calculates the
c  Euclidean norm of X.
c
c  The Euclidean norm is computed by accumulating the sum of
c  squares in three different sums. The sums of squares for the
c  small and large components are scaled so that no overflows
c  occur. Non-destructive underflows are permitted. Underflows
c  and overflows do not occur in the computation of the unscaled
c  sum of squares for the intermediate components.
c  The definitions of small, intermediate and large components
c  depend on two constants, RDWARF and RGIANT. The main
c  restrictions on these constants are that RDWARF**2 not
c  underflow and RGIANT**2 not overflow. The constants
c  given here are suitable for every known computer.
c
c  The function statement is
c
c  real function enorm(n,x)
c
c  where
c
c  N is a positive integer input variable.
c
c  X is an input array of length N.
c
c  Argonne National Laboratory. Minpack project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

      IMPLICIT NONE

      INTEGER n,i

      DOUBLE PRECISION x(n)
      DOUBLE PRECISION agiant,floatn,one,rdwarf,rgiant,s1,s2,s3,xabs,
     +     x1max,x3max,zero

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
      do 90 i = 1, n
         xabs = abs(x(i))
         if ((xabs .gt. rdwarf) .and. (xabs .lt. agiant)) goto 70
         if (xabs .le. rdwarf) goto 30

c  Sum for large components.

         if (xabs .le. x1max) goto 10
         s1 = one + s1*(x1max/xabs)**2
         x1max = xabs
         goto 20

 10      continue
         s1 = s1 + (xabs/x1max)**2

 20      continue
         goto 60

 30      continue

c  Sum for small components.

         if (xabs .le. x3max) goto 40
         s3 = one + s3*(x3max/xabs)**2
         x3max = xabs
         goto 50

 40      continue
         if (xabs .ne. zero) s3 = s3 + (xabs/x3max)**2

 50      continue
 60      continue
         goto 80

 70      continue

c  Sum for intermediate components.

         s2 = s2 + xabs**2

 80      continue
 90   continue

c  Calculation of norm.

      if (s1 .eq. zero) goto 100
      enorm = x1max*sqrt(s1+(s2/x1max)/x1max)
      goto 130

 100  continue
      if (s2 .eq. zero) goto 110
      if (s2 .ge. x3max)
     +     enorm = sqrt(s2*(one+(x3max/s2)*(x3max*s3)))
      if (s2 .lt. x3max)
     +     enorm = sqrt(x3max*((s2/x3max)+(x3max*s3)))
      goto 120
 110  continue
      enorm = x3max*sqrt(s3)

 120  continue
 130  continue

      return
      end
c______________________________________________________________________
      SUBROUTINE FDJAC1(
     +     fcnhyb,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,wa1,wa2)

c  This subroutine computes a forward-difference approximation
c  to the N by N Jacobian matrix associated with a specified
c  problem of N functions in N variables. If the Jacobian has
c  a banded form, then function evaluations are saved by only
c  approximating the nonzero terms.
c
c  The subroutine statement is
c
c  subroutine fdjac1(fcnhyb,n,x,fvec,fjac,ldfjac,iflag,ml,mu,epsfcn,
c                    wa1,wa2)
c
c  where
c
c  FCNHYB is the name of the user-supplied subroutine which
c  calculates the functions. FCNHYB must be declared
c  in an external statement in the user calling
c  program, and should be written as follows.
c
c   subroutine fcnhyb(n,x,fvec,iflag)
c   integer n,iflag
c   real x(n),fvec(n)
c   ----------
c   calculate the functions at x and
c   return this vector in fvec.
c   ----------
c   return
c   end
c
c  The value of IFLAG should not be changed by FCNHYB unless
c  the user wants to terminate execution of FDJAC1.
c  In this case set IFLAG to a negative integer.
c
c  N is a positive integer input variable set to the number
c  of functions and variables.
c
c  X is an input array of length N.
c
c  FVEC is an input array of length N which must contain the
c  functions evaluated at X.
c
c  FJAC is an output N by N array which contains the
c  approximation to the Jacobian matrix evaluated at X.
c
c  LDFJAC is a positive integer input variable not less than N
c  which specifies the leading dimension of the array FJAC.
c
c  IFLAG is an integer variable which can be used to terminate
c  the execution of FDJAC1. See description of FCNHYB.
c
c  ML is a nonnegative integer input variable which specifies
c  the number of subdiagonals within the band of the
c  Jacobian matrix. If the Jacobian is not banded, set
c  ML to at least N - 1.
c
c  EPSFCN is an input variable used in determining a suitable
c  step length for the forward-difference approximation. This
c  approximation assumes that the relative errors in the
c  functions are of the order of EPSFCN. If EPSFCN is less
c  than the machine precision, it is assumed that the relative
c  errors in the functions are of the order of the machine
c  precision.
c
c  MU is a nonnegative integer input variable which specifies
c  the number of superdiagonals within the band of the
c  Jacobian matrix. If the Jacobian is not banded, set
c  MU to at least N - 1.
c
c  WA1 and WA2 are work arrays of length N. If ML + MU + 1 is at
c  least N, then the Jacobian is considered dense, and WA2 is
c  not referenced.
c
c  Argonne National Laboratory. Minpack project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

      IMPLICIT NONE

      INTEGER n,ldfjac,iflag,ml,mu,i,j,k,msum

      DOUBLE PRECISION epsfcn
      DOUBLE PRECISION x(n),fvec(n),fjac(ldfjac,n),wa1(n),wa2(n)
      DOUBLE PRECISION eps,epsmch,h,temp,zero

      DOUBLE PRECISION spmpar
      EXTERNAL  fcnhyb,spmpar

      zero = 0.0D0

c  Machine precision

      epsmch = spmpar(1)

      eps = sqrt(max(epsfcn,epsmch))
      msum = ml + mu + 1
      if (msum .lt. n) goto 40

c  Computation of dense approximate Jacobian.

      do 20 j = 1, n
         temp = x(j)
         h = eps*abs(temp)
         if (h .eq. zero) h = eps
         x(j) = temp + h
         call fcnhyb(n,x,wa1,iflag)
         if (iflag .lt. 0) goto 30
         x(j) = temp
         do 10 i = 1, n
            fjac(i,j) = (wa1(i) - fvec(i))/h
 10      continue
 20   continue

 30   continue
      goto 110

 40   continue

c  Computation of banded approximate Jacobian.

      do 90 k = 1, msum
         do 60 j = k, n, msum
            wa2(j) = x(j)
            h = eps*abs(wa2(j))
            if (h .eq. zero) h = eps
            x(j) = wa2(j) + h
 60      continue
         call fcnhyb(n,x,wa1,iflag)
         if (iflag .lt. 0) goto 100
         do 80 j = k, n, msum
            x(j) = wa2(j)
            h = eps*abs(wa2(j))
            if (h .eq. zero) h = eps
            do 70 i = 1, n
               fjac(i,j) = zero
               if ((i .ge. (j - mu)).and.(i .le. (j + ml)))
     +              fjac(i,j) = (wa1(i) - fvec(i))/h
 70         continue
 80      continue
 90   continue

 100  continue
 110  continue

      return
      end
c______________________________________________________________________
      SUBROUTINE QFORM(m,n,q,ldq,wa)

c  This subroutine proceeds from the computed QR factorization of
c  an M by N matrix A to accumulate the M by M orthogonal matrix
c  Q from its factored form.
c
c  The subroutine statement is
c
c  subroutine qform(m,n,q,ldq,wa)
c
c  where
c
c  M is a positive integer input variable set to the number
c  of rows of A and the order of Q.
c
c  N is a positive integer input variable set to the number
c  of columns of A.
c
c  Q is an M by M array. On input the full lower trapezoid in
c  the first min(M,N) columns of Q contains the factored form.
c  On output Q has been accumulated into a square matrix.
c
c  LDQ is a positive integer input variable not less than M
c  which specifies the leading dimension of the array Q.
c
c  WA is a work array of length M.

c  Argonne National Laboratory. Minpack project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

      IMPLICIT NONE

      INTEGER m,n,ldq,i,j,jm1,k,l,minmn,np1

      DOUBLE PRECISION q(ldq,m),wa(m)
      DOUBLE PRECISION one,sum,temp,zero

      one = 1.0D0
      zero = 0.0D0

c  Zero out upper triangle of q in the first min(m,n) columns.

      minmn = min(m,n)
      if (minmn .lt. 2) goto 30
      do 20 j = 2, minmn
         jm1 = j - 1
         do 10 i = 1, jm1
            q(i,j) = zero
 10      continue
 20   continue

 30   continue

c  Initialize remaining columns to those of the identity matrix.

      np1 = n + 1
      if (m .lt. np1) goto 60
      do 50 j = np1, m
         do 40 i = 1, m
            q(i,j) = zero
 40      continue
         q(j,j) = one
 50   continue

 60   continue

c  Accumulate q from its factored form.

      do 120 l = 1, minmn
         k = minmn - l + 1
         do 70 i = k, m
            wa(i) = q(i,k)
            q(i,k) = zero
 70      continue
         q(k,k) = one
         if (wa(k) .eq. zero) goto 110
         do 100 j = k, m
            sum = zero
            do 80 i = k, m
               sum = sum + q(i,j)*wa(i)
 80         continue
            temp = sum/wa(k)
            do 90 i = k, m
               q(i,j) = q(i,j) - temp*wa(i)
 90         continue
 100     continue

 110     continue
 120  continue

      return
      end
c______________________________________________________________________
      SUBROUTINE QRFAC(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)

c  This subroutine uses householder transformations with column
c  pivoting (optional) to compute a QR factorization of the
c  M by N matrix A. That is, QRFAC determines an orthogonal
c  matrix Q, a permutation matrix P, and an upper trapezoidal
c  matrix R with diagonal elements of nonincreasing magnitude,
c  such that A*P = Q*R. The householder transformation for
c  column K, K = 1,2,...,min(M,N), is of the form
c
c  i - (1/u(k))*u*u
c
c  where U has zeros in the first K-1 positions. The form of
c  this transformation and the method of pivoting first
c  appeared in the corresponding Linpack subroutine.
c
c  The subroutine statement is
c
c  subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)
c
c  where
c
c  M is a positive integer input variable set to the number
c  of rows of A.
c
c  N is a positive integer input variable set to the number
c  of columns of A.
c
c  A is an M by N array. On input A contains the matrix for
c  which the QR factorization is to be computed. On output
c  the strict upper trapezoidal part of A contains the strict
c  upper trapezoidal part of R, and the lower trapezoidal
c  part of A contains a factored form of Q (the non-trivial
c  elements of the U vectors described above).
c
c  LDA is a positive integer input variable not less than M
c  which specifies the leading dimension of the array A.
c
c  PIVOT is a logical input variable. If PIVOT is set true,
c  then column pivoting is enforced. If PIVOT is set false,
c  then no column pivoting is done.
c
c  IPVT is an integer output array of length LIPVT. IPVT
c  defines the permutation matrix P such that A*P = Q*R.
c  Column J of P is column IPVT(J) of the identity matrix.
c  If PIVOT is false, IPVT is not referenced.
c
c  LIPVT is a positive integer input variable. If PIVOT is false,
c  then LIPVT may be as small as 1. If PIVOT is true, then
c  LIPVT must be at least N.
c
c  RDIAG is an output array of length N which contains the
c  diagonal elements of R.
c
c  ACNORM is an output array of length N which contains the
c  norms of the corresponding columns of the input matrix A.
c  If this information is not needed, then ACNORM can coincide
c  with RDIAG.
c
c  WA is a work array of length N. If PIVOT is false, then WA
c  can coincide with RDIAG.
c
c  Argonne National Laboratory. Minpack project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

      IMPLICIT NONE

      INTEGER m,n,lda,lipvt,i,j,jp1,k,kmax,minmn
      INTEGER ipvt(lipvt)

      LOGICAL pivot

      DOUBLE PRECISION a(lda,n),rdiag(n),acnorm(n),wa(n)
      DOUBLE PRECISION ajnorm,epsmch,one,p05,sum,temp,zero

      DOUBLE PRECISION spmpar,enorm
      EXTERNAL         spmpar,enorm

      one = 1.0D0
      p05 = 0.05D0
      zero = 0.0D0

c  Machine precision

      epsmch = spmpar(1)

c  Compute the initial column norms and initialize several arrays.

      do 10 j = 1, n
         acnorm(j) = enorm(m,a(1,j))
         rdiag(j) = acnorm(j)
         wa(j) = rdiag(j)
         if (pivot) ipvt(j) = j
 10   continue

c  Reduce a to r with householder transformations.

      minmn = min(m,n)
      do 110 j = 1, minmn
         if (.not.pivot) goto 40

c  Bring the column of largest norm into the pivot position.

         kmax = j
         do 20 k = j, n
            if (rdiag(k) .gt. rdiag(kmax)) kmax = k
 20      continue
         if (kmax .eq. j) goto 40
         do 30 i = 1, m
            temp = a(i,j)
            a(i,j) = a(i,kmax)
            a(i,kmax) = temp
 30      continue
         rdiag(kmax) = rdiag(j)
         wa(kmax) = wa(j)
         k = ipvt(j)
         ipvt(j) = ipvt(kmax)
         ipvt(kmax) = k

 40      continue

c  Compute the householder transformation to reduce the
c  j-th column of a to a multiple of the j-th unit vector.

         ajnorm = enorm(m-j+1,a(j,j))
         if (ajnorm .eq. zero) goto 100
         if (a(j,j) .lt. zero) ajnorm = -ajnorm
         do 50 i = j, m
            a(i,j) = a(i,j)/ajnorm
 50      continue
         a(j,j) = a(j,j) + one

c  Apply the transformation to the remaining columns
c  and update the norms.

         jp1 = j + 1
         if (n .lt. jp1) goto 100
         do 90 k = jp1, n
            sum = zero
            do 60 i = j, m
               sum = sum + a(i,j)*a(i,k)
 60         continue
            temp = sum/a(j,j)
            do 70 i = j, m
               a(i,k) = a(i,k) - temp*a(i,j)
 70         continue
            if ((.not.pivot).or.(rdiag(k) .eq. zero)) goto 80
            temp = a(j,k)/rdiag(k)
            rdiag(k) = rdiag(k)*sqrt(max(zero,one-temp**2))
            if ((p05*(rdiag(k)/wa(k))**2).gt. epsmch) goto 80
            rdiag(k) = enorm(m-j,a(jp1,k))
            wa(k) = rdiag(k)

 80         continue
 90      continue

 100     continue
         rdiag(j) = -ajnorm
 110  continue

      return
      end
c______________________________________________________________________
      SUBROUTINE R1MPYQ(m,n,a,lda,v,w)

c  Given an M by N matrix A, this subroutine computes A*Q where
c  Q is the product of 2*(N - 1) transformations
c
c  gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
c
c  and GV(I), GW(i) are Givens rotations in the (I,N) plane which
c  eliminate elements in the I-th and N-th planes, respectively.
c  Q itself is not given, rather the information to recover the
c  GV, GW rotations is supplied.
c
c  The subroutine statement is
c
c  subroutine r1mpyq(m,n,a,lda,v,w)
c
c  where
c
c  M is a positive integer input variable set to the number
c  of rows of A.
c
c  N is a positive integer input variable set to the number
c  of columns of A.
c
c  A is an M by N array. On input A must contain the matrix
c  to be postmultiplied by the orthogonal matrix Q
c  described above. On output A*Q has replaced A.
c
c  LDA is a positive integer input variable not less than M
c  which specifies the leading dimension of the array A.
c
c  V is an input array of length N. V(I) must contain the
c  information necessary to recover the Givens rotation GV(I)
c  described above.
c
c  W is an input array of length N. W(I) must contain the
c  information necessary to recover the Givens rotation GW(I)
c  described above.
c
c  Argonne National Laboratory. Minpack project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

      IMPLICIT NONE

      INTEGER m,n,lda,i,j,nmj,nm1

      DOUBLE PRECISION a(lda,n),v(n),w(n)
      DOUBLE PRECISION cos1,one,sin1,temp

      one = 1.0D0

c  Apply the first set of givens rotations to a.

      nm1 = n - 1
      if (nm1 .lt. 1) goto 50
      do 20 nmj = 1, nm1
         j = n - nmj
         if (abs(v(j)) .gt. one) cos1 = one/v(j)
         if (abs(v(j)) .gt. one) sin1 = sqrt(one-cos1**2)
         if (abs(v(j)) .le. one) sin1 = v(j)
         if (abs(v(j)) .le. one) cos1 = sqrt(one-sin1**2)
         do 10 i = 1, m
            temp   = cos1*a(i,j) - sin1*a(i,n)
            a(i,n) = sin1*a(i,j) + cos1*a(i,n)
            a(i,j) = temp
 10      continue
 20   continue

c  Apply the second set of givens rotations to a.

      do 40 j = 1, nm1
         if (abs(w(j)) .gt. one) cos1 = one/w(j)
         if (abs(w(j)) .gt. one) sin1 = sqrt(one-cos1**2)
         if (abs(w(j)) .le. one) sin1 = w(j)
         if (abs(w(j)) .le. one) cos1 = sqrt(one-sin1**2)
         do 30 i = 1, m
            temp   =  cos1*a(i,j) + sin1*a(i,n)
            a(i,n) = -sin1*a(i,j) + cos1*a(i,n)
            a(i,j) = temp
 30      continue
 40   continue

 50   continue

      return
      end
c______________________________________________________________________
      SUBROUTINE R1UPDT(m,n,s,ls,u,v,w,sing)

c  Given an M by N lower trapezoidal matrix S, an M-vector U,
c  and an N-vector V, the problem is to determine an
c  orthogonal matrix Q such that
c
c          t
c  (s + u*v )*q
c
c  is again lower trapezoidal.
c
c  This subroutine determines Q as the product of 2*(N - 1)
c  transformations
c
c  gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)
c
c  where GV(I), GW(I) are Givens rotations in the (I,N) plane
c  which eliminate elements in the I-th and N-th planes,
c  respectively. Q itself is not accumulated, rather the
c  information to recover the GV, GW rotations is returned.
c
c  The subroutine statement is
c
c  subroutine r1updt(m,n,s,ls,u,v,w,sing)
c
c  where
c
c  M is a positive integer input variable set to the number
c  of rows of S.
c
c  N is a positive integer input variable set to the number
c  of columns of S. N must not exceed M.
c
c  S is an array of length LS. On input S must contain the lower
c  trapezoidal matrix S stored by columns. On output S contains
c  the lower trapezoidal matrix produced as described above.
c
c  LS is a positive integer input variable not less than
c  (N*(2*M-N+1))/2.
c
c  U is an input array of length M which must contain the
c  vector U.
c
c  V is an array of length N. On input V must contain the vector
c  V. On output V(I) contains the information necessary to
c  recover the Givens rotation GV(I) described above.
c
c  W is an output array of length M. W(I) contains information
c  necessary to recover the Givens rotation GW(I) described
c  above.
c
c  SING is a logical output variable. SING is set true if any
c  of the diagonal elements of the output S are zero. Otherwise
c  SING is set false.
c
c  Argonne National Laboratory. Minpack project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More,
c  John L. Nazareth

      IMPLICIT NONE

      INTEGER m,n,ls,i,j,jj,l,nmj,nm1

      LOGICAL sing

      DOUBLE PRECISION s(ls),u(m),v(n),w(m),cos1,cotan,giant,one
      DOUBLE PRECISION p5,p25,sin1,tan1,tau,temp,zero

      DOUBLE PRECISION spmpar
      EXTERNAL         spmpar

      one = 1.0D0
      p5 = 0.5D0
      p25 = 0.25D0
      zero = 0.0D0

c  giant is the largest magnitude in the computer's arithmetic range

      giant = spmpar(3)

c  Initialize the diagonal element pointer.

      jj = (n*(2*m - n + 1))/2 - (m - n)

c  Move the nontrivial part of the last column of s into w.

      l = jj
      do 10 i = n, m
         w(i) = s(l)
         l = l + 1
 10   continue

c  Rotate the vector v into a multiple of the n-th unit vector
c  in such a way that a spike is introduced into w.

      nm1 = n - 1
      if (nm1 .lt. 1) goto 70
      do 60 nmj = 1, nm1
         j = n - nmj
         jj = jj - (m - j + 1)
         w(j) = zero
         if (v(j) .eq. zero) goto 50

c  Determine a givens rotation which eliminates the
c  j-th element of v.

         if (abs(v(n)) .ge. abs(v(j))) goto 20
         cotan = v(n)/v(j)
         sin1 = p5/sqrt(p25+p25*cotan**2)
         cos1 = sin1*cotan
         tau = one
         if (abs(cos1)*giant .gt. one) tau = one/cos1
         goto 30

 20      continue
         tan1 = v(j)/v(n)
         cos1 = p5/sqrt(p25+p25*tan1**2)
         sin1 = cos1*tan1
         tau = sin1

 30      continue

c  Apply the transformation to v and store the information
c  necessary to recover the givens rotation.

         v(n) = sin1*v(j) + cos1*v(n)
         v(j) = tau

c  Apply the transformation to s and extend the spike in w.

         l = jj
         do 40 i = j, m
            temp = cos1*s(l) - sin1*w(i)
            w(i) = sin1*s(l) + cos1*w(i)
            s(l) = temp
            l = l + 1
 40      continue

 50      continue
 60   continue

 70   continue

c  Add the spike from the rank 1 update to w.

      do 80 i = 1, m
         w(i) = w(i) + v(n)*u(i)
 80   continue

c  Eliminate the spike.

      sing = .false.
      if (nm1 .lt. 1) goto 140
      do 130 j = 1, nm1
         if (w(j) .eq. zero) goto 120

c  Determine a givens rotation which eliminates the
c  j-th element of the spike.

         if (abs(s(jj)) .ge. abs(w(j))) goto 90
         cotan = s(jj)/w(j)
         sin1 = p5/sqrt(p25+p25*cotan**2)
         cos1 = sin1*cotan
         tau = one
         if ((abs(cos1)*giant).gt. one) tau = one/cos1
         goto 100

 90      continue
         tan1 = w(j)/s(jj)
         cos1 = p5/sqrt(p25+p25*tan1**2)
         sin1 = cos1*tan1
         tau = sin1

 100     continue

c  Apply the transformation to s and reduce the spike in w.

         l = jj
         do 110 i = j, m
            temp =  cos1*s(l) + sin1*w(i)
            w(i) = -sin1*s(l) + cos1*w(i)
            s(l) = temp
            l = l + 1
 110     continue

c  Store the information necessary to recover the
c  givens rotation.

         w(j) = tau

 120     continue

c  Test for zero diagonal elements in the output s.

         if (s(jj) .eq. zero) sing = .true.
         jj = jj + (m - j + 1)
 130  continue

 140  continue

c  Move w back into the last column of the output s.

      l = jj
      do 150 i = n, m
         s(l) = w(i)
         l = l + 1
 150  continue
      if (s(jj) .eq. zero) sing = .true.

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION SPMPAR(i)

c  This function provides certain machine parameters.
c
c  I is an integer input variable set to 1, 2, or 3 which
c  selects the desired machine parameter. If the machine has
c  P base B digits and its smallest and largest exponents are
c  EMIN and EMAX, respectively, then these parameters are
c
c  SPMPAR(1) = B**(1 - P), the machine precision,
c
c  SPMPAR(2) = B**(EMIN - 1), the smallest magnitude,
c
c  SPMPAR(3) = B**EMAX*(1 - B**(-P)), the largest magnitude.
c
c  Note that the values of these parameters can be found for a given
c  machine if the Mark 12 or later NAg library is installed on it.
c
c  SPMPAR(1) is equivalent to X02AJF()
c  SPMPAR(2) is equivalent to X02AKF()
c  SPMPAR(3) is equivalent to X02ALF()

      IMPLICIT NONE

      INTEGER i

      DOUBLE PRECISION rmach(3)

      rmach(1) = 0.000001D0
      rmach(2) = 0.5D-37
      rmach(3) = 0.1D+38

      spmpar = rmach(i)

      return
      end
