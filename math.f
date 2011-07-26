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
C  Module         : $Id: math.f,v 3.1 1993/06/11 14:28:02 peter Exp $
C
C  Module name    : $RCSfile: math.f,v $
C  Version no.    : $Revision: 3.1 $
C
C  Creation date  : $Date: 1993/06/11 14:28:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      DOUBLE PRECISION FUNCTION GAMFUN(xxx)

c  Gamma function for xxx > 1

      IMPLICIT NONE

      DOUBLE PRECISION sqtwopi,sum,xxx

      sqtwopi = 2.5066282D0

      sum = 1.0D0 + 0.083333333333D0/xxx + 3.472222222D-3/xxx**2
     +     - 2.6813271D-3/xxx**3 - 2.2947209D-4/xxx**4

      gamfun = exp(-xxx) * xxx**(xxx-0.5D0) * sqtwopi * sum

      return
      end
c______________________________________________________________________
      SUBROUTINE QUANC8(fun,a,b,abserr,relerr,result,errest,nofun,flag)

c  Estimate the integral of fun(x) from a to b
c  to a user provided tolerance.
c  An automatic adaptive routine based on
c  the 8-panel Newton-Cotes rule.
c
c  INPUT :
c
c  fun     the name of the integrand function subprogram fun(x).
c  a       the lower limit of integration.
c  b       the upper limit of integration.(b may be less than a.)
c  relerr  a relative error tolerance. (should be non-negative)
c  abserr  an absolute error tolerance. (should be non-negative)
c
c  OUTPUT :
c
c  result  an approximation to the integral hopefully satisfying the
c          least stringent of the two error tolerances.
c  errest  an estimate of the magnitude of the actual error.
c  nofun   the number of function values used in calculation of result.
c  flag    a reliability indicator.  If flag is zero, then result
c          probably satisfies the error tolerance.  If flag is
c          xxx.yyy , then  xxx = the number of intervals which have
c          not converged and  0.yyy = the fraction of the interval
c          left to do when the limit on  nofun  was approached.

      IMPLICIT NONE

      DOUBLE PRECISION a, b, abserr, relerr, result, errest, flag
      DOUBLE PRECISION w0,w1,w2,w3,w4,area,x0,f0,stone,step,cor11,temp
      DOUBLE PRECISION qprev,qnow,qdiff,qleft,esterr,tolerr
      DOUBLE PRECISION qright(31),f(16),x(16),fsave(8,30),xsave(8,30)

      DOUBLE PRECISION fun
      EXTERNAL         fun

      INTEGER nofun
      INTEGER levmin,levmax,levout,nomax,nofin,lev,nim,i,j

c  Stage 1 ***   general initialization
c  Set constants.

      levmin = 1
      levmax = 30
      levout = 6
      nomax = 5000
      nofin = nomax - 8*(levmax-levout+2**(levout+1))

c  Trouble when nofun reaches nofin

      w0 =   3956.0D0 / 14175.0D0
      w1 =  23552.0D0 / 14175.0D0
      w2 =  -3712.0D0 / 14175.0D0
      w3 =  41984.0D0 / 14175.0D0
      w4 = -18160.0D0 / 14175.0D0

c  Initialize running sums to zero

      flag = 0.0D0
      result = 0.0D0
      cor11  = 0.0D0
      errest = 0.0D0
      area  = 0.0D0
      nofun = 0
      if (a.eq.b) goto 1000

c  Stage 2 ***   initialization for first interval

      lev = 0
      nim = 1
      x0 = a
      x(16) = b
      qprev  = 0.0D0
      f0 = fun(x0)
      stone = (b - a) / 16.0D0
      x(8)  =  (x0  + x(16)) / 2.0D0
      x(4)  =  (x0  + x(8))  / 2.0D0
      x(12) =  (x(8)  + x(16)) / 2.0D0
      x(2)  =  (x0  + x(4))  / 2.0D0
      x(6)  =  (x(4)  + x(8))  / 2.0D0
      x(10) =  (x(8)  + x(12)) / 2.0D0
      x(14) =  (x(12) + x(16)) / 2.0D0
      do 25 j = 2, 16, 2
         f(j) = fun(x(j))
 25   continue
      nofun = 9

c  Stage 3 ***   central calculation
c  Requires qprev,x0,x2,x4,...,x16,f0,f2,f4,...,f16.
c  Calculates x1,x3,...x15, f1,f3,...f15,qleft,qright,qnow,qdiff,area.

 30   continue
      x(1) = (x0 + x(2)) / 2.0D0
      f(1) = fun(x(1))
      do 35 j = 3, 15, 2
         x(j) = (x(j-1) + x(j+1)) / 2.0D0
         f(j) = fun(x(j))
 35   continue
      nofun = nofun + 8
      step = (x(16) - x0) / 16.0D0
      qleft = (w0*(f0 + f(8))  + w1*(f(1)+f(7))  + w2*(f(2)+f(6))
     +     + w3*(f(3)+f(5))  +  w4*f(4)) * step
      qright(lev+1) = (w0*(f(8)+f(16))+w1*(f(9)+f(15))+w2*(f(10)+f(14))
     +     + w3*(f(11)+f(13)) + w4*f(12)) * step
      qnow = qleft + qright(lev+1)
      qdiff = qnow - qprev
      area = area + qdiff

c  Stage 4 *** interval convergence test

      esterr = abs(qdiff) / 1023.0D0
      tolerr = max(abserr,relerr*abs(area)) * (step/stone)
      if (lev .lt. levmin) goto 50
      if (lev .ge. levmax) goto 62
      if (nofun .gt. nofin) goto 60
      if (esterr .le. tolerr) goto 70

c  Stage 5   ***   no convergence
c  Locate next interval.

 50   continue
      nim = 2*nim
      lev = lev+1

c  Store right hand elements for future use.

      do 52 i = 1, 8
         fsave(i,lev) = f(i+8)
         xsave(i,lev) = x(i+8)
 52   continue

c  Assemble left hand elements for immediate use.

      qprev = qleft
      do 55 i = 1, 8
         j = -i
         f(2*j+18) = f(j+9)
         x(2*j+18) = x(j+9)
 55   continue
      goto 30

c  Stage 6   ***   trouble section
c  Number of function values is about to exceed limit.

 60   continue
      nofin = 2*nofin
      levmax = levout
      flag = flag + (b - x0) / (b - a)
      goto 70

c  Current level is levmax.

 62   continue
      flag = flag + 1.0D0

c  Stage 7   ***   interval converged
c  Add contributions into running sums.

 70   continue
      result = result + qnow
      errest = errest + esterr
      cor11  = cor11  + qdiff / 1023.0D0

c  Locate next interval.

 72   continue
      if (nim .eq. (2*(nim/2))) goto 75
      nim = nim/2
      lev = lev-1
      goto 72
 75   continue
      nim = nim + 1
      if (lev .le. 0) goto 80

c  Assemble elements required for the next interval.

      qprev = qright(lev)
      x0 = x(16)
      f0 = f(16)
      do 78 i = 1, 8
         f(2*i) = fsave(i,lev)
         x(2*i) = xsave(i,lev)
 78   continue
      goto 30

c  Stage 8   ***   finalize and return

 80   continue
      result = result + cor11

c  Make sure errest not less than roundoff level.

      if (errest .eq. 0.0D0) goto 1000
 82   continue
      temp = abs(result) + errest
      if (temp .ne. abs(result)) goto 1000
      errest = 2.0D0*errest
      goto 82

 1000 continue

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION ZEROIN(ax,bx,fhz,tol)

c  A zero of the function  fhz(x)  is computed in the interval ax,bx
c
c  INPUT :
c
c  ax     left endpoint of initial interval
c  bx     right endpoint of initial interval
c  fhz    function subprogram which evaluates fhz(x) for any x in
c         the interval  ax,bx
c  tol    desired length of the interval of uncertainty of the
c         final result ( .ge. 0.0)
c
c  OUTPUT :
c
c  zeroin abcissa approximating a zero of  fhz  in the interval ax,bx
c
c  It is assumed that fhz(ax) and fhz(bx) have opposite signs without a
c  check.  zeroin  returns a zero  x  in the given interval ax,bx  to
c  within a tolerance  4*macheps*abs(x) + tol, where macheps is the
c  relative machine precision.  This function subprogram is a slightly
c  modified translation of the algol 60 procedure  zero  given in
c  Richard Brent, algorithms for minimization without derivatives,
c  Prentice - Hall, inc. (1973).

      IMPLICIT NONE

      DOUBLE PRECISION ax,bx,tol
      DOUBLE PRECISION a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s

      DOUBLE PRECISION fhz
      EXTERNAL         fhz

c  Compute eps, the relative machine precision

      eps = 1.0D0
 10   continue
      eps = eps/2.0D0
      tol1 = 1.0D0 + eps
      if (tol1 .gt. 1.0D0) goto 10

c  Initialization

      a = ax
      b = bx
      fa = fhz(a)
      fb = fhz(b)

c  Begin step

 20   continue
      c = a
      fc = fa
      d = b - a
      e = d
 30   continue
      if (abs(fc) .ge. abs(fb)) goto 40
      a = b
      b = c
      c = a
      fa = fb
      fb = fc
      fc = fa

c  Convergence test

 40   continue
      tol1 = 2.0D0 * eps * abs(b) + 0.5D0*tol
      xm = 0.5D0*(c - b)
      if (abs(xm) .le. tol1) goto 90
      if (fb .eq. 0.0D0) goto 90

c  Is bisection necessary

      if (abs(e) .lt. tol1) goto 70
      if (abs(fa) .le. abs(fb)) goto 70

c  Is quadratic interpolation possible

      if (a .ne. c) goto 50

c  Linear interpolation

      s = fb/fa
      p = 2.0D0*xm*s
      q = 1.0D0 - s
      goto 60

c  Inverse quadratic interpolation

 50   continue
      q = fa/fc
      r = fb/fc
      s = fb/fa
      p = s*(2.0D0*xm*q*(q - r) - (b - a)*(r - 1.0D0))
      q = (q - 1.0D0)*(r - 1.0D0)*(s - 1.0D0)

c  Adjust signs

 60   continue
      if (p .gt. 0.0D0) q = -q
      p = abs(p)

c  Is interpolation acceptable

      if ((2.0D0*p) .ge. (3.0D0*xm*q - abs(tol1*q))) goto 70
      if (p .ge. abs(0.5D0*e*q)) goto 70
      e = d
      d = p/q
      goto 80

c  Bisection

 70   continue
      d = xm
      e = d

c  Complete step

 80   continue
      a = b
      fa = fb
      if (abs(d) .gt. tol1) b = b + d
      if (abs(d) .le. tol1) b = b + sign(tol1, xm)
      fb = fhz(b)
      if ((fb*(fc/abs(fc))) .gt. 0.0D0) goto 20
      goto 30

c  Done

 90   continue
      zeroin = b

      return
      end
