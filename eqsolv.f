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
C  Module         : $Id: eqsolv.f,v 3.1 1993/06/11 14:27:45 peter Exp $
C
C  Module name    : $RCSfile: eqsolv.f,v $
C  Version no.    : $Revision: 3.1 $
C
C  Creation date  : $Date: 1993/06/11 14:27:45 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE EQSOLV(fcnhyb,
     +     n,x,fvec,tol,epsfcn,factor,nprint,info,wa,lwa,resdl,nfev)

c  Routine eqsolv is the Argonne Minpack subroutine HYBRD1
c  which has been modified by D.T. Blackfield FEDC/TRW
c  The routine is the same except some of the arguments are
c  user supplied rather than 'hardwired'.
c
c  April 2, 1984
c
c  The purpose of HYBRD1 is to find a zero of a system of
c  n nonlinear functions in n variables by a modification
c  of the Powell hybrid method. This is done by using the
c  more general nonlinear equation solver HYBRD. The user
c  must provide a subroutine which calculates the functions.
c  The Jacobian is then calculated by a forward-difference
c  approximation.
c
c  The subroutine statement is
c
c  SUBROUTINE HYBRD1(FCNHYB,N,X,FVEC,TOL, ... ,INFO,WA,LWA, ...)
c
c  where
c
c  FCNHYB is the name of the user-supplied subroutine which
c  calculates the functions. FCNHYB must be declared
c  in an external statement in the user calling
c  program, and should be written as follows:
c
c  subroutine fcnhyb(n,x,fvec,iflag)
c  integer n,iflag
c  double precision x(n),fvec(n)
c  ----------
c  calculate the functions at x and
c  return this vector in fvec.
c  ---------
c  return
c  end
c
c  The value of iflag should not be changed by FCNHYB unless
c  the user wants to terminate execution of HYBRD1.
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
c  TOL is a non-negative input variable. Termination occurs
c  when the algorithm estimates that the relative error
c  between X and the solution is at most TOL.
c
c  INFO is an integer output variable. If the user has
c  terminated execution, INFO is set to the (negative)
c  value of IFLAG, see description of FCNHYB above. Otherwise,
c  INFO is set as follows:
c
c  INFO = 0   Improper input parameters.
c
c  INFO = 1   Algorithm estimates that the relative error
c             between X and the solution is at most TOL.
c
c  INFO = 2   Number of calls to FCNHYB has reached or exceeded
c             200*(N+1).
c
c  INFO = 3   TOL is too small. No further improvement in
c             the approximate solution X is possible.
c
c  INFO = 4   Iteration is not making good progress.
c
c  WA is a work array of length LWA.
c
c  LWA is a positive integer input variable not less than
c  (N*(3*N+13))/2 .
c
c  Subprograms called:
c
c  User-supplied ...... FCNHYB
c
c  Minpack-supplied ... HYBRD
c
c  Argonne National Laboratory. Minpack Project. March 1980.
c  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More

      IMPLICIT NONE

      INTEGER n,n1,info,lwa
      INTEGER indx,j,lr,maxfev,ml,mode,mu,nfev,nprint

      DOUBLE PRECISION tol
      DOUBLE PRECISION x(n),fvec(n),wa(lwa),resdl(n)
      DOUBLE PRECISION epsfcn,factor,one,xtol,zero

      EXTERNAL fcnhyb

      one = 1.0D0
      zero = 0.0D0

      info = 0

c  Check the input parameters for errors.

      if ( (n.le.0).or.
     +     (tol.lt.zero).or.
     +     (lwa.lt.((n*(3*n + 13))/2) ) )
     +     goto 20

c  Call HYBRD

      maxfev = 200*(n + 1)
      xtol = tol
      ml = n - 1
      mu = n - 1
      mode = 2

      do 10 j = 1, n
         wa(j) = one
 10   continue

      lr = (n*(n + 1))/2
      indx = 6*n + lr
      n1 = n

c+**PJK 23/10/92 Warning produced by QA Fortran :
c+**PJK 23/10/92 Arg 16 in call to HYBRD has wrong dimensions.
c+**PJK 23/10/92 Code works at present, but beware of future
c+**PJK 23/10/92 modifications.

      call hybrd(fcnhyb,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,wa(1),mode,
     +     factor,nprint,info,nfev,wa(indx+1),n1,wa(6*n+1),lr,
     +     wa(n+1),wa(2*n+1),wa(3*n+1),wa(4*n+1),wa(5*n+1),
     +     resdl)

      if (info.eq.5) info = 4

 20   continue

      return
      end
c______________________________________________________________________
      SUBROUTINE FCNHYB(n,xc,rc,iflag)

c  This subroutine is the function evaluator for EQSOLV (HYBRID1)

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'numer.h'

      INTEGER iflag,n,ncon,nvars

      DOUBLE PRECISION xc(n),rc(n)

      nvars = neqns
      ncon = neqns

      call caller(xc,nvars)

c+**PJK 23/10/92 Removed redundant arguments (nvars,....,xc,..)
c+**PJK 23/10/92 from call to routine CON1.

      call con1(ncon,rc)

c  Set iflag < 0 if program is to be terminated here.

      iflag = 1 * iflag

      return
      end
