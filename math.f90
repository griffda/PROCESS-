!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function gamfun(xxx)

  !+ad_name  gamfun
  !+ad_summ  Calculates the gamma function for xxx > 1
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  xxx : input real : gamma function argument
  !+ad_desc  This routine evaluates the gamma function, using an
  !+ad_desc  asymptotic expansion based on Stirling's approximation.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  28/07/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  http://en.wikipedia.org/wiki/Gamma_function
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: gamfun

  !  Arguments

  real(kind(1.0D0)), intent(in) :: xxx

  !  Local variables

  real(kind(1.0D0)) :: sqtwopi,sum

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  sqtwopi = 2.5066282D0

  sum = 1.0D0 + 0.083333333333D0/xxx + 3.472222222D-3/xxx**2 &
       - 2.6813271D-3/xxx**3 - 2.2947209D-4/xxx**4

  gamfun = exp(-xxx) * xxx**(xxx-0.5D0) * sqtwopi * sum

end function gamfun

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine quanc8(fun,a,b,abserr,relerr,result,errest,nofun,flag)

  !+ad_name  quanc8
  !+ad_summ  Estimate the integral of fun(x) from a to b
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  fun : external function : integrand function subprogram fun(x)
  !+ad_args  a : input real : lower limit of integration
  !+ad_args  b : input real : upper limit of integration (b may be less than a)
  !+ad_args  abserr : input real : absolute error tolerance (should be non-negative)
  !+ad_args  relerr : input real : relative error tolerance (should be non-negative)
  !+ad_args  result : output real : approximation to the integral hopefully
  !+ad_argc           satisfying the least stringent of the two error tolerances
  !+ad_args  errest : output real : estimate of the magnitude of the actual error
  !+ad_args  nofun : output integer : number of function values used in calculation
  !+ad_args  flag : output real : Reliability indicator; if flag is zero, then
  !+ad_argc         result probably satisfies the error tolerance.  If flag is
  !+ad_argc         xxx.yyy , then  xxx = the number of intervals which have
  !+ad_argc         not converged and  0.yyy = the fraction of the interval
  !+ad_argc         left to do when the limit on  nofun  was approached.
  !+ad_desc  This routine estimates the integral of fun(x) from a to b
  !+ad_desc  to a user provided tolerance. An automatic adaptive
  !+ad_desc  routine based on the 8-panel Newton-Cotes rule.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  28/07/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  http://www.netlib.org/fmm/index.html :
  !+ad_docc    Computer Methods for Mathematical Computations,
  !+ad_docc    G E Forsythe, M A Malcolm, and C B Moler,
  !+ad_docc    Prentice-Hall, Englewood Cliffs, New Jersey
  !+ad_docc    1977, ISBN 0-13-165332-6
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), external :: fun
  real(kind(1.0D0)), intent(in) :: a, b, abserr, relerr
  real(kind(1.0D0)), intent(out) :: result, errest, flag
  integer, intent(out) :: nofun

  !  Local variables

  real(kind(1.0D0)) :: w0,w1,w2,w3,w4,area,x0,f0,stone,step,cor11,temp
  real(kind(1.0D0)) :: qprev,qnow,qdiff,qleft,esterr,tolerr
  real(kind(1.0D0)), dimension(31) :: qright
  real(kind(1.0D0)), dimension(16) :: f, x
  real(kind(1.0D0)), dimension(8,30) :: fsave, xsave

  integer :: levmin,levmax,levout,nomax,nofin,lev,nim,i,j

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Stage 1:  general initialization
  !  Set constants

  levmin = 1
  levmax = 30
  levout = 6
  nomax = 5000
  nofin = nomax - 8*(levmax-levout+2**(levout+1))

  !  Trouble when nofun reaches nofin

  w0 =   3956.0D0 / 14175.0D0
  w1 =  23552.0D0 / 14175.0D0
  w2 =  -3712.0D0 / 14175.0D0
  w3 =  41984.0D0 / 14175.0D0
  w4 = -18160.0D0 / 14175.0D0

  !  Initialize running sums to zero

  flag = 0.0D0
  result = 0.0D0
  cor11  = 0.0D0
  errest = 0.0D0
  area  = 0.0D0
  nofun = 0

  if (a == b) return

  !  Stage 2:  initialization for first interval

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
  do j = 2, 16, 2
     f(j) = fun(x(j))
  end do
  nofun = 9

  !  Stage 3:  central calculation

  !  Requires qprev,x0,x2,x4,...,x16,f0,f2,f4,...,f16
  !  Calculates x1,x3,...x15, f1,f3,...f15,qleft,qright,qnow,qdiff,area

  main_loop: do

     x(1) = (x0 + x(2)) / 2.0D0
     f(1) = fun(x(1))
     do j = 3, 15, 2
        x(j) = (x(j-1) + x(j+1)) / 2.0D0
        f(j) = fun(x(j))
     end do
     nofun = nofun + 8
     step = (x(16) - x0) / 16.0D0
     qleft = (w0*(f0 + f(8))  + w1*(f(1)+f(7))  + w2*(f(2)+f(6)) &
          + w3*(f(3)+f(5))  +  w4*f(4)) * step
     qright(lev+1) = (w0*(f(8)+f(16))+w1*(f(9)+f(15))+w2*(f(10)+f(14)) &
          + w3*(f(11)+f(13)) + w4*f(12)) * step
     qnow = qleft + qright(lev+1)
     qdiff = qnow - qprev
     area = area + qdiff

     !  Stage 4:  interval convergence test

     esterr = abs(qdiff) / 1023.0D0
     tolerr = max(abserr,relerr*abs(area)) * (step/stone)

     if ( (lev < levmin).or. &
          ((lev < levmax).and.(nofun <= nofin) &
          .and.(esterr > tolerr)) ) then

        !  Stage 5:  no convergence
        !  Locate next interval

        nim = 2*nim
        lev = lev+1

        !  Store right hand elements for future use

        do i = 1, 8
           fsave(i,lev) = f(i+8)
           xsave(i,lev) = x(i+8)
        end do

        !  Assemble left hand elements for immediate use

        qprev = qleft
        do i = 1, 8
           j = -i
           f(2*j+18) = f(j+9)
           x(2*j+18) = x(j+9)
        end do

        cycle main_loop

     else if (lev >= levmax) then

        flag = flag + 1.0D0

     else if (nofun > nofin) then

        !  Stage 6:  trouble section
        !  Number of function values is about to exceed limit

        nofin = 2*nofin
        levmax = levout
        flag = flag + (b - x0) / (b - a)

     end if

     !  Stage 7:  interval converged
     !  Add contributions into running sums

     result = result + qnow
     errest = errest + esterr
     cor11  = cor11  + qdiff / 1023.0D0

     !  Locate next interval

     do
        if (nim == (2*(nim/2))) exit
        nim = nim/2
        lev = lev-1
     end do
     nim = nim + 1
     if (lev <= 0) exit main_loop

     !  Assemble elements required for the next interval

     qprev = qright(lev)
     x0 = x(16)
     f0 = f(16)
     do i = 1, 8
        f(2*i) = fsave(i,lev)
        x(2*i) = xsave(i,lev)
     end do

  end do main_loop

  !  Stage 8:  finalize and return

  result = result + cor11

  !  Make sure errest not less than roundoff level

  if (errest == 0.0D0) return
  estimate_error: do
     temp = abs(result) + errest
     if (temp /= abs(result)) exit estimate_error
     errest = 2.0D0*errest
  end do estimate_error

end subroutine quanc8

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function zeroin(ax,bx,fhz,tol)

  !+ad_name  zeroin
  !+ad_summ  Root-finding algorithm
  !+ad_type  Function returning real
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ax : input real : left endpoint of initial interval
  !+ad_args  bx : input real : right endpoint of initial interval
  !+ad_args  fhz : external real function : function subprogram which evaluates
  !+ad_argc        fhz(x) for any x in the interval  ax,bx
  !+ad_args  tol : input real : desired length of the interval of uncertainty
  !+ad_argc                     of the final result ( >= 0 )
  !+ad_desc  A zero of the function  fhz(x)  is computed in the interval ax,bx
  !+ad_desc  It is assumed that fhz(ax) and fhz(bx) have opposite signs without a
  !+ad_desc  check.  zeroin  returns a zero  x  in the given interval ax,bx  to
  !+ad_desc  within a tolerance  4*macheps*abs(x) + tol, where macheps is the
  !+ad_desc  relative machine precision.  This function subprogram is a slightly
  !+ad_desc  modified translation of the algol 60 procedure  zero  given in
  !+ad_desc  Richard Brent, algorithms for minimization without derivatives,
  !+ad_desc  Prentice - Hall, inc. (1973).
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  28/07/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  http://www.netlib.org/fmm/index.html :
  !+ad_docc    Computer Methods for Mathematical Computations,
  !+ad_docc    G E Forsythe, M A Malcolm, and C B Moler,
  !+ad_docc    Prentice-Hall, Englewood Cliffs, New Jersey
  !+ad_docc    1977, ISBN 0-13-165332-6
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  real(kind(1.0D0)) :: zeroin

  !  Arguments

  real(kind(1.0D0)), external :: fhz
  real(kind(1.0D0)), intent(in) :: ax,bx,tol

  !  Local variables

  real(kind(1.0D0)) :: a,b,c,d,e,eps,fa,fb,fc,tol1,xm,p,q,r,s

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Compute eps, the relative machine precision

  eps = 1.0D0
  do
     eps = eps/2.0D0
     tol1 = 1.0D0 + eps
     if (tol1 == 1.0D0) exit
  end do

  !  Initialization

  a = ax
  b = bx
  fa = fhz(a)
  fb = fhz(b)

  !  Begin step

  step: do
     c = a
     fc = fa
     d = b - a
     e = d

     inner_loop: do

        if (abs(fc) < abs(fb)) then
           a = b ; b = c ; c = a
           fa = fb ; fb = fc ; fc = fa
        end if

        !  Convergence test

        tol1 = 2.0D0 * eps * abs(b) + 0.5D0*tol
        xm = 0.5D0*(c - b)

        if ((abs(xm) <= tol1).or.(fb == 0.0D0)) then
           zeroin = b
           return
        end if

        !  Is bisection necessary

        if ( (abs(e) < tol1).or.(abs(fa) <= abs(fb)) ) then
           !  Bisection
           d = xm
           e = d
        else

           !  Is quadratic interpolation possible?

           if (a == c) then
              !  Linear interpolation
              s = fb/fa
              p = 2.0D0*xm*s
              q = 1.0D0 - s
           else
              !  Inverse quadratic interpolation
              q = fa/fc
              r = fb/fc
              s = fb/fa
              p = s*(2.0D0*xm*q*(q - r) - (b - a)*(r - 1.0D0))
              q = (q - 1.0D0)*(r - 1.0D0)*(s - 1.0D0)
           end if

           !  Adjust signs

           if (p > 0.0D0) q = -q
           p = abs(p)

           !  Is interpolation acceptable

           if ( ((2.0D0*p) >= (3.0D0*xm*q - abs(tol1*q))).or. &
                (p >= abs(0.5D0*e*q)) ) then
              !  Bisection
              d = xm
              e = d
           else
              e = d
              d = p/q
           end if

        end if

        !  Complete step

        a = b
        fa = fb
        if (abs(d) > tol1) then
           b = b + d
        else
           b = b + sign(tol1, xm)
        end if
        fb = fhz(b)
        if ((fb*(fc/abs(fc))) > 0.0D0) exit inner_loop

     end do inner_loop

  end do step

end function zeroin
