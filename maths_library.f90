! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  To perform standalone tests of vmcon:
!  1) Compile code as normal using 'make'
!  2) Uncomment #define line below
!  3) Re-compile using pre-processor:
!     ifort -cpp -c maths_library.f90
!     ifort -o vmcon_test maths_library.o numerics.o global_variables.o
!
!  Don't forget to comment the line below again afterwards!!!

!#define unit_test

module maths_library

  !+ad_name  maths_library
  !+ad_summ  Library of mathematical and numerical routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  dogleg
  !+ad_cont  dotpmc
  !+ad_cont  dshellarea
  !+ad_cont  dshellvol
  !+ad_cont  ellipke
  !+ad_cont  enorm
  !+ad_cont  eshellarea
  !+ad_cont  eshellvol
  !+ad_cont  fdjac1
  !+ad_cont  find_y_nonuniform_x
  !+ad_cont  gamfun
  !+ad_cont  harwfp
  !+ad_cont  harwqp
  !+ad_cont  hinv
  !+ad_cont  hybrd
  !+ad_cont  isamax
  !+ad_cont  linesolv
  !+ad_cont  qform
  !+ad_cont  qpsub
  !+ad_cont  qrfac
  !+ad_cont  quanc8
  !+ad_cont  r1mpyq
  !+ad_cont  r1updt
  !+ad_cont  saxpy
  !+ad_cont  sdot
  !+ad_cont  sgedi
  !+ad_cont  sgefa
  !+ad_cont  sgesl
  !+ad_cont  spmpar
  !+ad_cont  sscal
  !+ad_cont  sswap
  !+ad_cont  sumup2
  !+ad_cont  sumup3
  !+ad_cont  svd
  !+ad_cont  tril
  !+ad_cont  vmcon
  !+ad_cont  zeroin
  !+ad_args  N/A
  !+ad_desc  This module contains a large number of routines to enable
  !+ad_desc  PROCESS to perform a variety of numerical procedures, including
  !+ad_desc  linear algebra, zero finding, integration and minimisation.
  !+ad_desc  The module is an amalgamation of the contents of several
  !+ad_desc  different pre-existing PROCESS source files, which themselves
  !+ad_desc  were derived from a number of different numerical libraries
  !+ad_desc  including BLAS, MINPAC and the Harwell subroutine library.
  !+ad_prob  None
  !+ad_call  global_variables
  !+ad_hist  10/10/12 PJK Initial version of module
  !+ad_hist  25/02/14 PJK Added global_variables
  !+ad_hist  04/03/14 PJK Added sumup2, sumup3, tril, ellipke,
  !+ad_hisc               find_y_nonuniform_x
  !+ad_hist  04/03/14 PJK Added dshellvol, eshellvol, dshellarea, eshellarea
  !+ad_hist  08/02/17 JM  Added interpolate and binary_search for Kallenbach
  !+ad_stat  Okay
  !+ad_docs  http://en.wikipedia.org/wiki/Gamma_function
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use global_variables, only: verbose
  use constants
  ! MDK Remove this dependency, as iotty is now defined in global variables.
  !use process_output

  implicit none

  !  Precision variable
  integer, parameter :: double = 8

  private

  public :: ellipke,find_y_nonuniform_x,gamfun,hybrd,linesolv,qpsub, &
       quanc8,sumup3,svd,tril,vmcon,zeroin, eshellvol, dshellvol, &
       eshellarea, dshellarea, binomial, binarysearch, interpolate

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function find_y_nonuniform_x(x0,x,y,n)

    !+ad_name  find_y_nonuniform_x
    !+ad_summ  Routine to find y0 such that y0 = y(x0) given a set of
    !+ad_summ  values x(1:n), y(1:n)
    !+ad_type  Function returning a real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  x0 : input real : x value at which we want to find y
    !+ad_args  x(1:n) : input real array : monotonically de/increasing x values
    !+ad_args  y(1:n) : input real array : y values at each x
    !+ad_args  n : input integer : size of array
    !+ad_desc  This routine performs a simple linear interpolation method
    !+ad_desc  to find the y value at x = x0. If x0 lies outside the
    !+ad_desc  range [x(1),x(n)], the y value at the nearest 'end' of the data
    !+ad_desc  is returned.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  31/03/08 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: find_y_nonuniform_x

    !  Arguments

    integer, intent(in) :: n
    real(kind(1.0D0)), intent(in) :: x0
    real(kind(1.0D0)), dimension(n), intent(in) :: x
    real(kind(1.0D0)), dimension(n), intent(in) :: y

    !  Local variables

    integer :: i,j
    real(kind(1.0D0)) :: dx, ddx

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Step through arrays until x crosses the value of interest

    j = 0
    rough_search: do i = 1,n-1

       if (((x(i)-x0)*(x(i+1)-x0)) <= 0.0D0) then
          j = i
          exit rough_search
       end if

    end do rough_search

    if (j /= 0) then

       !  Simply do a linear interpolation between the two grid points
       !  spanning the point of interest

       find_y_nonuniform_x = y(j) + (y(j+1)-y(j))*(x0-x(j))/(x(j+1)-x(j))

    else  !  No points found, so return the 'nearest' y value

       if (x(n) > x(1)) then  !  values are monotonically increasing
          if (x0 > x(n)) then
             find_y_nonuniform_x = y(n)
          else
             find_y_nonuniform_x = y(1)
          end if

       else  !  values are monotonically decreasing
          if (x0 < x(n)) then
             find_y_nonuniform_x = y(n)
          else
             find_y_nonuniform_x = y(1)
          end if

       end if

    end if

  end function find_y_nonuniform_x

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sumup2(dx,y,inty,n)

    !+ad_name  sumup2
    !+ad_summ  Routine to integrate a 1-D array of y values, using a process
    !+ad_summ  similar to Simpson's Rule, and assuming equally-spaced x values.
    !+ad_summ  It returns the integral at all tabulated points.
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  dx : input real : (constant) spacing between adjacent x values
    !+ad_args  y(1:n) : input real array : y values to be integrated
    !+ad_args  inty(1:n) : input/output real array : calculated integral
    !+ad_argc    (see below)
    !+ad_args  n : input integer : length of arrays y and inty
    !+ad_desc  This routine uses a process similar to (but not quite the same
    !+ad_desc  as) Simpson's Rule to integrate an array y,
    !+ad_desc  returning the integral up to point i in array element inty(i).
    !+ad_desc  Note that the first element of inty is not calculated, and must
    !+ad_desc  be set to the required value on entry. Usually, but not always,
    !+ad_desc  this value will be zero.
    !+ad_prob  This routine does a very reasonable job; however, routine
    !+ad_prob  <A HREF="sumup3.html">sumup3</A> is more accurate if only the
    !+ad_prob  total integral over the given range is required.
    !+ad_call  None
    !+ad_hist  12/04/02 PJK Initial version
    !+ad_hist  24/02/2009 PJK Added third as parameter
    !+ad_stat  Okay
    !+ad_docs  The original source for this algorithm is not known...
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: n
    real(kind(1.0D0)), intent(in) :: dx
    real(kind(1.0D0)), intent(in), dimension(n) :: y
    real(kind(1.0D0)), intent(inout), dimension(n) :: inty

    !  Local variables

    integer :: ix
    real(kind(1.0D0)), parameter :: third = 1.0D0/3.0D0
    real(kind(1.0D0)) :: thirddx
    real(kind(1.0D0)), allocatable, dimension(:) :: yhalf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    allocate (yhalf(2:n))

    thirddx = third*dx

    do ix = 2,n-1
       yhalf(ix) = y(ix)-0.25D0*(y(ix+1)-y(ix-1))
    end do
    yhalf(n) = y(n-1)+0.25D0*(y(n)-y(n-2))

    do ix = 2,n
       inty(ix) = inty(ix-1) + thirddx*(y(ix)+yhalf(ix)+y(ix-1))
    end do

    deallocate (yhalf)

  end subroutine sumup2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sumup3(dx,y,integral,n)

    !+ad_name  sumup3
    !+ad_summ  Routine to integrate a 1-D array of y values using the
    !+ad_summ  Extended Simpson's Rule, assuming equally-spaced x values
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  dx : input real : (constant) spacing between adjacent x values
    !+ad_args  y(1:n) : input real array : y values to be integrated
    !+ad_args  integral : output real : calculated integral
    !+ad_args  n : input integer : length of array y
    !+ad_desc  This routine uses Simpson's Rule to integrate an array y.
    !+ad_desc  If n is even, routine <CODE>sumup2</CODE> is called to
    !+ad_desc  perform the calculation.
    !+ad_desc  <P>Note: unlike sumup1 and sumup2, this routine returns only
    !+ad_desc  the complete integral, not the intermediate values as well.
    !+ad_prob  None
    !+ad_call  sumup2
    !+ad_hist  28/06/06 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: n
    real(kind(1.0D0)), intent(in) :: dx
    real(kind(1.0D0)), intent(in), dimension(n) :: y
    real(kind(1.0D0)), intent(out) :: integral

    !  Local variables

    integer :: ix
    real(kind(1.0D0)) :: sum1
    real(kind(1.0D0)), allocatable, dimension(:) :: inty

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (mod(n,2) == 0) then

       !  Use sumup2 if the number of tabulated points is even

       allocate (inty(n))

       inty(1) = 0.0D0
       call sumup2(dx,y,inty,n)
       integral = inty(n)

       deallocate (inty)

    else

       sum1 = y(1)
       do ix = 2,n-3,2
          sum1 = sum1 + 4.0D0*y(ix) + 2.0D0*y(ix+1)
       end do
       integral = dx/3.0D0*(sum1 + 4.0D0*y(n-1) + y(n))

    end if

  end subroutine sumup3

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tril(a,n,alower)

    !+ad_name  tril
    !+ad_summ  Routine to extract the lower triangular part of a square matrix
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a(n,n) : input real array : input matrix
    !+ad_args  n      : input integer : number of rows and columns in A
    !+ad_args  a(n,n) : output real array : lower triangular part of A
    !+ad_desc  This routine extracts the lower triangular part of a square matrix,
    !+ad_desc  excluding the diagonal, into a new square matrix. The remainder
    !+ad_desc  of this matrix contains zeroes on exit.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  03/03/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: n
    real(kind(1.0D0)), dimension(n,n), intent(in) :: a
    real(kind(1.0D0)), dimension(n,n), intent(out) :: alower

    !  Local variables

    integer :: row,col

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    alower = 0.0D0
    do col = 1,n-1
       do row = col+1,n
          alower(row,col) = a(row,col)
       end do
    end do

  end subroutine tril

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ellipke(sqk,kk,ek)

    !+ad_name  ellipke
    !+ad_summ  Routine that calculates the complete elliptic integral
    !+ad_summ  of the first and second kinds
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  sqk : input real : square of the elliptic modulus
    !+ad_args  kk  : output real : complete elliptic integral of the first kind
    !+ad_args  ek  : output real : complete elliptic integral of the second kind
    !+ad_desc  This routine calculates the complete elliptic integral
    !+ad_desc  of the first and second kinds.
    !+ad_desc  <P>The method used is that described in the reference, and
    !+ad_desc  the code is taken from the Culham maglib library routine FN02A.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  03/03/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Approximations for Digital Computers, C. Hastings,
    !+ad_docc  Princeton University Press, 1955
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: sqk
    real(kind(1.0D0)), intent(out) :: kk,ek

    !  Local variables

    real(kind(1.0D0)) :: a,b,d,e

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    d = 1.0D0 - sqk
    e = log(d)

    !  Evaluate series for integral of first kind

    a = (((0.014511962D0*d + 0.037425637D0)*d + 0.035900924D0)*d &
         + 0.096663443D0)*d + 1.386294361D0
    b = (((0.004417870D0*d + 0.033283553D0)*d + 0.06880249D0)*d &
         + 0.12498594D0)*d + 0.5D0

    kk = a - b*e

    !  Evaluate series for integral of second kind

    a = (((0.017365065D0*d + 0.047573835D0)*d + 0.06260601D0)*d &
         + 0.44325141D0)*d + 1.0D0
    b = (((0.005264496D0*d + 0.040696975D0)*d + 0.09200180D0)*d &
         + 0.24998368D0)*d

    ek = a - b*e

  end subroutine ellipke

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  real(kind(1.0D0))  function binomial(n,k) result(coefficient)
    ! This outputs a real approximation to the coefficient
    ! http://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
    implicit none
    integer, intent(in) :: n, k
    integer :: numerator, i
    if (k == 0) then
        coefficient = 1
    else
        coefficient = 1.0D0
        do i = 1, k
            numerator = n + 1 -i
            coefficient = coefficient * real(numerator)/real(i)
        end do
    end if
  end function binomial
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive real(kind(1.0D0)) function gamfun(x) result(gamma)

    !+ad_name  gamfun
    !+ad_summ  Calculates the gamma function for arbitrary real x
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  x : input real : gamma function argument
    !+ad_desc  This routine evaluates the gamma function, using an
    !+ad_desc  asymptotic expansion based on Stirling's approximation.
    !+ad_prob  Beware, gamma(x) is (correctly) infinite if x is zero or
    !+ad_prob  a negative whole number.
    !+ad_call  None
    !+ad_hist  28/07/11 PJK Initial F90 version
    !+ad_hist  18/02/14 PJK Extended range of argument to all real x
    !+ad_stat  Okay
    !+ad_docs  http://en.wikipedia.org/wiki/Gamma_function
    !+ad_docs  T&amp;M/PKNIGHT/LOGBOOK24, p.5
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: x

    !  Local variables

    real(kind(1.0D0)), parameter :: sqtwopi = 2.5066282746310005D0
    real(kind(1.0D0)), parameter :: c1 = 8.3333333333333333D-2  !  1/12
    real(kind(1.0D0)), parameter :: c2 = 3.4722222222222222D-3  !  1/288
    real(kind(1.0D0)), parameter :: c3 = 2.6813271604938272D-3  !  139/51840
    real(kind(1.0D0)), parameter :: c4 = 2.2947209362139918D-4  !  571/2488320
    real(kind(1.0D0)) :: summ, denom
    integer :: i,n

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (x > 1.0D0) then

       summ = 1.0D0 + c1/x + c2/x**2 - c3/x**3 - c4/x**4
       gamma = exp(-x) * x**(x-0.5D0) * sqtwopi * summ

    else

       !  Use recurrence formula to shift the argument to >1
       !  gamma(x) = gamma(x+n) / (x*(x+1)*(x+2)*...*(x+n-1))
       !  where n is chosen to make x+n > 1

       n = int(-x) + 2
       denom = x
       do i = 1,n-1
          denom = denom*(x+i)
       end do
       gamma = gamfun(x+n)/denom

    end if

  end function gamfun

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function binarysearch(length, array, value, delta)
    ! Given an array and a value, returns the index of the element that
    ! is closest to, but less than, the given value.
    ! Uses a binary search algorithm.
    ! "delta" is the tolerance used to determine if two values are equal
    ! if ( abs(x1 - x2) <= delta) then
    !    assume x1 = x2
    ! endif

    implicit none
    integer, intent(in) :: length
    real(kind(1.0D0)), dimension(length), intent(in) :: array
    real(kind(1.0D0)), intent(in) :: value
    real(kind(1.0D0)), intent(in), optional :: delta
    integer :: binarysearch
    integer :: left, middle, right
    real(kind(1.0D0)) :: d

    if (present(delta) .eqv. .true.) then
        d = delta
    else
        d = 1e-9
    endif
        
    left = 1
    right = length
    do
        if (left > right) then
            exit
        endif
        middle = nint((real(left+right)) / 2.0)
        if ( abs(array(middle) - value) <= d) then
            binarySearch = middle
            return
        else if (array(middle) > value) then
            right = middle - 1
        else
            left = middle + 1
        end if
    end do
    binarysearch = right

  end function binarysearch

  real(kind(1.0D0)) function interpolate(x_len, x_array, y_len, y_array, f, x, y, delta)
    ! This function uses bilinear interpolation to estimate the value
    ! of a function f at point (x,y)
    ! f is assumed to be sampled on a regular grid, with the grid x values specified
    ! by x_array and the grid y values specified by y_array
    ! Reference: http://en.wikipedia.org/wiki/Bilinear_interpolation
    implicit none
    integer, intent(in) :: x_len, y_len           
    real(kind(1.0D0)), dimension(x_len), intent(in) :: x_array
    real(kind(1.0D0)), dimension(y_len), intent(in) :: y_array
    real(kind(1.0D0)), dimension(x_len, y_len), intent(in) :: f
    real(kind(1.0D0)), intent(in) :: x,y
    real(kind(1.0D0)), intent(in), optional :: delta   
    real(kind(1.0D0)) :: denom, x1, x2, y1, y2
    integer :: i,j

    i = binarysearch(x_len, x_array, x)
    j = binarysearch(y_len, y_array, y)

    x1 = x_array(i)
    x2 = x_array(i+1)

    y1 = y_array(j)
    y2 = y_array(j+1)
        
    denom = (x2 - x1)*(y2 - y1)

    interpolate = (f(i,j)*(x2-x)*(y2-y) + f(i+1,j)*(x-x1)*(y2-y) + &
      f(i,j+1)*(x2-x)*(y-y1) + f(i+1, j+1)*(x-x1)*(y-y1))/denom

  end function interpolate

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine linesolv(a, ndim, b, x)

    !+ad_name  linesolv
    !+ad_summ  Routine to solve the linear equation system Ax = b
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a(ndim,ndim) : in/out real array : array A
    !+ad_args  ndim         : input integer     : dimension of a
    !+ad_args  b(ndim)      : in/out real array : RHS vector
    !+ad_args  x(ndim)      : output real array : solution for Ax = b
    !+ad_desc  This routine solves the linear equation Ax = b.
    !+ad_desc  It calls (local copies of) the linpack routines sgefa and sgesl.
    !+ad_prob  The called routines overwrite their array arguments, which
    !+ad_prob  might save space but could be misleading.
    !+ad_call  sgefa
    !+ad_call  sgesl
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: ndim
    real(kind(1.0D0)), dimension(ndim,ndim), intent(inout) :: a
    real(kind(1.0D0)), dimension(ndim), intent(inout) :: b, x

    !  Local variables

    integer :: job, ndim1, info
    integer, dimension(ndim) :: ipvt

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    job = 0
    ndim1 = ndim

    call sgefa(a, ndim, ndim1, ipvt, info)
    call sgesl(a, ndim, ndim1, ipvt, b, job)

    x(:) = b(:)

  end subroutine linesolv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine hinv(h,ih,n,ipvt,info)

    !+ad_name  hinv
    !+ad_summ  Matrix inversion routine
    !+ad_type  Subroutine
    !+ad_auth  Roger L. Crane, Kenneth E. Hillstrom, Michael Minkoff; Linpack
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  h(ih,ih) : input/output real array : On input, matrix to be inverted
    !+ad_argc                                       On output, the calculated inverse
    !+ad_args  ih       : input integer : array size
    !+ad_args  n        : input integer : order of H; n <= ih
    !+ad_args  ipvt(n)  : output integer array : pivot vector
    !+ad_args  info     : output integer : info flag
    !+ad_argc                              = 1  normal return
    !+ad_argc                              = 2  H matrix is singular
    !+ad_desc  This routine inverts the matrix H by use of linpack software.
    !+ad_prob  None
    !+ad_call  sgedi
    !+ad_call  sgefa
    !+ad_hist  --/06/79 Linpack algorithm version
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_hist  25/02/14 PJK Added diagnostic output
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: ih, n
    integer, intent(out) :: info
    integer, dimension(n), intent(out) :: ipvt
    real(kind(1.0D0)), dimension(ih,ih), intent(inout) :: h

    !  Local variables

    real(kind(1.0D0)), dimension(2) :: det
    real(kind(1.0D0)), dimension(n) :: work

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Do LU decomposition of h

    call sgefa(h,ih,n,ipvt,info)

    if (info == 0) then  !  H is non-singular, so we can form its inverse
       call sgedi(h,ih,n,ipvt,det,1)
       info = 1
    else
       info = 2
       if (verbose == 1) then
          call sgedi(h,ih,n,ipvt,det,10)  !  Calculate determinant only
          write(*,*) 'Determinant = det(1) * 10.0**det(2)',det
          write(*,*) 'H matrix is singular in subroutine hinv'
       end if
    end if

  end subroutine hinv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dotpmc(x,ix,y,iy,c,total,n,iflag)

    !+ad_name  dotpmc
    !+ad_summ  Calculates +/-C +/- (X.dot.Y) for arrays X, Y and scalar C
    !+ad_type  Subroutine
    !+ad_auth  Roger L. Crane, Kenneth E. Hillstrom, Michael Minkoff; Linpack
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  x(ix*n) : input real array : X array
    !+ad_args  ix      : input integer : interval in storage between X array elements
    !+ad_args  y(iy*n) : input real array : Y array
    !+ad_args  iy      : input integer : interval in storage between Y array elements
    !+ad_args  c       : input real : C value
    !+ad_args  total   : output real : computed result
    !+ad_args  n       : input integer : number of terms in the dot product
    !+ad_args  iflag   : input integer : switch
    !+ad_argc                            = 0    +c + (x dot y) is computed
    !+ad_argc                            = 1    +c - (x dot y) is computed
    !+ad_argc                            = 2    -c + (x dot y) is computed
    !+ad_argc                            = 3    -c - (x dot y) is computed
    !+ad_desc  This subroutine computes
    !+ad_desc    total = (plus or minus c) plus or minus the dot product of x and y
    !+ad_desc  by invoking the basic linear algebra routine dot.
    !+ad_prob  None
    !+ad_call  sdot
    !+ad_hist  --/--/-- Linpack algorithm version
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: ix,iy,n,iflag
    real(kind(1.0D0)), dimension(ix*n), intent(in) :: x
    real(kind(1.0D0)), dimension(iy*n), intent(in) :: y
    real(kind(1.0D0)), intent(in) :: c
    real(kind(1.0D0)), intent(out) :: total

    !  Local variables

    real(kind(1.0D0)) :: prod

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate dot product

    prod = sdot(n,x,ix,y,iy)
    if (mod(iflag,2) /= 0) prod = -prod

    total = c + prod
    if (iflag > 1) total = -c + prod

  end subroutine dotpmc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sgesl(a,lda,n,ipvt,b,job)

    !+ad_name  sgesl
    !+ad_summ  Routine to solve the the real system  Ax = b  or  transp(A).x = b
    !+ad_type  Subroutine
    !+ad_auth  Cleve Moler, University of New Mexico, Argonne National Lab.
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a(lda,n) : input real array : output from <A HREF="sgefa.html">sgefa</A>
    !+ad_args  lda : input integer : leading dimension of the array A
    !+ad_args  n : input integer : order of the matrix A
    !+ad_args  ipvt(n) : input integer array : pivot vector from <CODE>sgefa</CODE>
    !+ad_args  b(n) : input/output real array : RHS vector on input,
    !+ad_argc                                   solution vector x on output
    !+ad_args  job : input integer : switch
    !+ad_argc                        = 0         to solve  A*x = b ,
    !+ad_argc                        = nonzero   to solve  transp(A)*x = b  where
    !+ad_argc                                    transp(A)  is the transpose
    !+ad_desc  This routine solves the real system  A*x = b  or  transp(A)*x = b
    !+ad_desc  using the factors computed by <A HREF="sgefa.html">sgefa</A>.
    !+ad_desc  <P>A division by zero will occur if the input factor contains a
    !+ad_desc  zero on the diagonal.  Technically this indicates singularity
    !+ad_desc  but it is often caused by improper arguments or improper
    !+ad_desc  setting of <CODE>lda</CODE>.
    !+ad_prob  None
    !+ad_call  saxpy
    !+ad_call  sdot
    !+ad_hist  14/08/78 CM  Linpack version
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: lda,n,job
    integer, dimension(n), intent(in) :: ipvt
    real(kind(1.0D0)), dimension(lda,n), intent(in) :: a
    real(kind(1.0D0)), dimension(n), intent(inout) :: b

    !  Local variables

    integer :: k,kb,l,nm1
    real(kind(1.0D0)) :: t

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nm1 = n - 1

    if (job == 0) then  !  Solve  A * x = b

       !  First solve  l*y = b

       if (nm1 >= 1) then
          do k = 1, nm1
             l = ipvt(k)
             t = b(l)
             if (l /= k) then
                b(l) = b(k)
                b(k) = t
             end if
             call saxpy(n-k,t,a(k+1:n,k),1,b(k+1:n),1)
          end do
       end if

       !  Now solve  u*x = y

       do kb = 1, n
          k = n + 1 - kb
          b(k) = b(k)/a(k,k)
          t = -b(k)
          call saxpy(k-1,t,a(1:n,k),1,b(1:n),1)
       end do

    else  !  Solve  transp(A) * x = b

       !  First solve  transp(u)*y = b

       do k = 1, n
          t = sdot(k-1,a(1,k),1,b(1),1)
          b(k) = (b(k) - t)/a(k,k)
       end do

       !  Now solve transp(l)*x = y

       if (nm1 >= 1) then
          do kb = 1, nm1
             k = n - kb
             b(k) = b(k) + sdot(n-k,a(k+1,k),1,b(k+1),1)
             l = ipvt(k)
             if (l /= k) then
                t = b(l)
                b(l) = b(k)
                b(k) = t
             end if
          end do
       end if

    end if

  end subroutine sgesl

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sgefa(a,lda,n,ipvt,info)

    !+ad_name  sgefa
    !+ad_summ  Routine to factor a real matrix by Gaussian elimination
    !+ad_type  Subroutine
    !+ad_auth  Cleve Moler, University of New Mexico, Argonne National Lab.
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a(lda,n) : input/output real array : On entry, matrix to be factored.
    !+ad_argc       On exit, an upper triangular matrix and the multipliers
    !+ad_argc       which were used to obtain it.
    !+ad_argc       The factorization can be written  A = L*U  where
    !+ad_argc       L is a product of permutation and unit lower
    !+ad_argc       triangular matrices and U is upper triangular.
    !+ad_args  lda : input integer : leading dimension of the array A
    !+ad_args  n : input integer : order of the matrix A
    !+ad_args  ipvt(n) : output integer array : pivot indices
    !+ad_args  info : output integer : info flag
    !+ad_argc                          = 0  normal completion
    !+ad_argc                          = k  if  u(k,k) == 0.0
    !+ad_desc  This routine factors a real matrix by Gaussian elimination.
    !+ad_prob  None
    !+ad_call  isamax
    !+ad_call  saxpy
    !+ad_call  sscal
    !+ad_hist  14/08/78 CM  Linpack version
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_hist  25/02/14 PJK Added diagnostic output
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: lda,n
    integer, intent(out) :: info
    integer, dimension(n), intent(out) :: ipvt
    real(kind(1.0D0)), dimension(lda,n), intent(inout) :: a

    !  Local variables

    integer :: j,k,kp1,l,nm1
    real(kind(1.0D0)) :: t

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (verbose == 1) then
       do j = 1,n
          if (all(a(j,:) == 0.0D0)) then
             write(*,*) 'Line ',j, &
                  ' in matrix a in subroutine sgefa is all zero'
          end if
       end do
    end if

    info = 0
    nm1 = n - 1

    if (nm1 >= 1) then

       do k = 1, nm1
          kp1 = k + 1

          !  Find L = pivot index

          l = isamax(n-k+1,a(k,k),1) + k - 1
          ipvt(k) = l

          !  Zero pivot implies this column already triangularized

          if (a(l,k) /= 0.0D0) then

             !  Interchange if necessary

             if (l /= k) then
                t = a(l,k)
                a(l,k) = a(k,k)
                a(k,k) = t
             end if

             !  Compute multipliers

             t = -1.0D0/a(k,k)
             call sscal(n-k,t,a(k+1:n,k),1)

             !  Row elimination with column indexing

             do j = kp1, n
                t = a(l,j)
                if (l /= k) then
                   a(l,j) = a(k,j)
                   a(k,j) = t
                end if
                call saxpy(n-k,t,a(k+1:n,k),1,a(k+1:n,j),1)
             end do

          else
             info = k
             if (verbose == 1) then
                write(*,*) 'a(l,k) = 0.0D0 in subroutine sgefa'
                write(*,*) 'info=k=',info
             end if
          end if
       end do

    end if

    ipvt(n) = n
    if (a(n,n) == 0.0D0) then
       info = n
       if (verbose == 1) then
          write(*,*) 'Error: a(n,n) == 0.0D0 in subroutine sgefa'
       end if
    end if

  end subroutine sgefa

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sgedi(a,lda,n,ipvt,det,job)

    !+ad_name  sgedi
    !+ad_summ  Routine to compute the determinant and inverse of a matrix
    !+ad_type  Subroutine
    !+ad_auth  Cleve Moler, University of New Mexico, Argonne National Lab.
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  a(lda,n) : input/output real array :
    !+ad_argc       On entry, output from <A HREF="sgefa.html">sgefa</A>.
    !+ad_argc       On exit, the inverse if requested, otherwise unchanged
    !+ad_args  lda      : input integer : leading dimension of the array A
    !+ad_args  n        : input integer : order of the matrix A
    !+ad_args  ipvt(n)  : input integer array : pivot vector from sgefa
    !+ad_args  det(2)   : output real array : determinant of original matrix if requested,
    !+ad_argc                                 otherwise not referenced.
    !+ad_argc         Determinant = det(1) * 10.0**det(2)
    !+ad_argc           with  1.0 .le. abs(det(1)) .lt. 10.0
    !+ad_argc           or  det(1) .eq. 0.0 .
    !+ad_args  job : input integer : switch for required outputs
    !+ad_argc                        = 11   both determinant and inverse.
    !+ad_argc                        = 01   inverse only.
    !+ad_argc                        = 10   determinant only.
    !+ad_desc  This routine computes the determinant and inverse of a matrix
    !+ad_desc  using the factors computed by (SGECO or) <A HREF="sgefa.html">SGEFA</A>.
    !+ad_desc  <P>A division by zero will occur if the input factor contains
    !+ad_desc  a zero on the diagonal and the inverse is requested.
    !+ad_prob  None
    !+ad_call  saxpy
    !+ad_call  sscal
    !+ad_call  sswap
    !+ad_hist  14/08/78 CM  Linpack version
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: lda,n,job
    integer, dimension(n), intent(in) :: ipvt
    real(kind(1.0D0)), dimension(lda,n), intent(inout) :: a

    !  Local variables

    integer :: i,j,k,kk,kb,kp1,l,nm1
    real(kind(1.0D0)), parameter :: ten = 10.0D0
    real(kind(1.0D0)) :: t
    real(kind(1.0D0)), dimension(2) :: det
    real(kind(1.0D0)), dimension(n) :: work

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if ((job/10) /= 0) then  !  Compute determinant

       det(1) = 1.0D0
       det(2) = 0.0D0

       do i = 1, n
          if (ipvt(i) /=  i) det(1) = -det(1)
          det(1) = a(i,i)*det(1)

          if (det(1) == 0.0D0) exit

          do
             if (abs(det(1)) >= 1.0D0) exit
             det(1) = ten*det(1)
             det(2) = det(2) - 1.0D0
          end do

          do
             if (abs(det(1)) < ten) exit
             det(1) = det(1)/ten
             det(2) = det(2) + 1.0D0
          end do
       end do

    end if

    !  Compute inverse(u)

    if (mod(job,10) /= 0) then

       do k = 1, n
          a(k,k) = 1.0D0/a(k,k)
          t = -a(k,k)

          call sscal(k-1,t,a(1:n,k),1)
          kp1 = k + 1
          if (n >= kp1) then
             do j = kp1, n
                t = a(k,j)
                a(k,j) = 0.0D0
                kk = k
                call saxpy(kk,t,a(1:n,k),1,a(1:n,j),1)
             end do
          end if
       end do

       !  Form inverse(u)*inverse(l)

       nm1 = n - 1
       if (nm1 >= 1) then
          do kb = 1, nm1
             k = n - kb
             kp1 = k + 1

             do i = kp1, n
                work(i) = a(i,k)
                a(i,k) = 0.0D0
             end do

             do j = kp1, n
                t = work(j)
                call saxpy(n,t,a(1:n,j),1,a(1:n,k),1)
             end do

             l = ipvt(k)

             if (l /= k) call sswap(n,a(1:n,k),1,a(1:n,l),1)
          end do

       end if
    end if

  end subroutine sgedi

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sscal(n,sa,sx,incx)

    !+ad_name  sscal
    !+ad_summ  Routine to scale a vector by a constant
    !+ad_type  Subroutine
    !+ad_auth  Jack Dongarra, Linpack
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  n        : input integer : order of the matrix sx
    !+ad_args  sa       : input real array : constant multiplier
    !+ad_args  sx(n*incx) : input/output real array : On entry, matrix to be scaled;
    !+ad_argc                                         On exit, the scaled matrix
    !+ad_args  incx     : input integer : interval in storage between sx array elements
    !+ad_desc  This routine scales a vector by a constant, using
    !+ad_desc  unrolled loops for increments equal to 1.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  11/03/78 JD  Linpack version
    !+ad_hist  29/09/88 JD  Corrected problem with negative increments
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: n, incx
    real(kind(1.0D0)), intent(in) :: sa
    real(kind(1.0D0)), dimension(n*incx), intent(inout) :: sx

    !  Local variables

    integer :: i,ix,m,mp1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (n <= 0) return

    if (incx /= 1) then

       ix = 1
       if (incx < 0) ix = (-n+1)*incx + 1
       do i = 1,n
          sx(ix) = sa*sx(ix)
          ix = ix + incx
       end do

    else

       m = mod(n,5)
       if ( m /= 0 ) then
          do i = 1,m
             sx(i) = sa*sx(i)
          end do
          if (n < 5) return
       end if

       mp1 = m + 1
       do i = mp1,n,5
          sx(i)     = sa*sx(i)
          sx(i + 1) = sa*sx(i + 1)
          sx(i + 2) = sa*sx(i + 2)
          sx(i + 3) = sa*sx(i + 3)
          sx(i + 4) = sa*sx(i + 4)
       end do

    end if

  end subroutine sscal

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine saxpy(n,sa,sx,incx,sy,incy)

    !+ad_name  saxpy
    !+ad_summ  Routine to scale a vector by a constant, then add another vector
    !+ad_type  Subroutine
    !+ad_auth  Jack Dongarra, Linpack
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  n        : input integer : order of the matrices sx, sy
    !+ad_args  sa       : input real array : constant multiplier
    !+ad_args  sx(n*incx) : input real array : matrix to be scaled
    !+ad_args  incx     : input integer : interval in storage between sx array elements
    !+ad_args  sy(n*incy) : input/output real array : On entry, matrix being added;
    !+ad_argc                                       On exit, the final result
    !+ad_args  incy     : input integer : interval in storage between sy array elements
    !+ad_desc  This routine calculates <CODE>sa*sx(:) + sy(:)</CODE>,
    !+ad_desc  using unrolled loops for increments equal to 1.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  11/03/78 JD  Linpack version
    !+ad_hist  29/09/88 JD  Corrected problem with negative increments
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: n,incx,incy
    real(kind(1.0D0)), intent(in) :: sa
    real(kind(1.0D0)), dimension(n*incx), intent(in) :: sx
    real(kind(1.0D0)), dimension(n*incy), intent(inout) :: sy

    !  Local variables

    integer :: i,ix,iy,m,mp1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if ((n <= 0).or.(sa == 0.0D0)) return

    if ((incx /= 1).or.(incy /= 1)) then

       ix = 1 ; iy = 1
       if (incx < 0) ix = (-n+1)*incx + 1
       if (incy < 0) iy = (-n+1)*incy + 1
       do i = 1,n
          sy(iy) = sy(iy) + sa*sx(ix)
          ix = ix + incx
          iy = iy + incy
       end do

    else

       m = mod(n,4)
       if (m /= 0) then
          do i = 1,m
             sy(i) = sy(i) + sa*sx(i)
          end do
          if (n < 4) return
       end if

       mp1 = m + 1
       do i = mp1,n,4
          sy(i)     = sy(i)     + sa*sx(i)
          sy(i + 1) = sy(i + 1) + sa*sx(i + 1)
          sy(i + 2) = sy(i + 2) + sa*sx(i + 2)
          sy(i + 3) = sy(i + 3) + sa*sx(i + 3)
       end do

    end if

  end subroutine saxpy

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sswap(n,sx,incx,sy,incy)

    !+ad_name  sswap
    !+ad_summ  Routine to interchange two vectors
    !+ad_type  Subroutine
    !+ad_auth  Jack Dongarra, Linpack
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  n        : input integer : order of the matrices sx, sy
    !+ad_args  sx(n*incx) : input/output real array : first vector
    !+ad_args  incx     : input integer : interval in storage between sx array elements
    !+ad_args  sy(n*incy) : input/output real array : second vector
    !+ad_args  incy     : input integer : interval in storage between sy array elements
    !+ad_desc  This routine swaps the contents of two vectors,
    !+ad_desc  using unrolled loops for increments equal to 1.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  11/03/78 JD  Linpack version
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: n, incx, incy
    real(kind(1.0D0)), dimension(n*incx), intent(inout) :: sx
    real(kind(1.0D0)), dimension(n*incy), intent(inout) :: sy

    !  Local variables

    integer :: i,ix,iy,m,mp1
    real(kind(1.0D0)) :: stemp

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (n <= 0) return

    if ((incx /= 1).or.(incy /= 1)) then

       ix = 1 ; iy = 1
       if (incx < 0) ix = (-n+1)*incx + 1
       if (incy < 0) iy = (-n+1)*incy + 1
       do i = 1,n
          stemp = sx(ix)
          sx(ix) = sy(iy)
          sy(iy) = stemp
          ix = ix + incx
          iy = iy + incy
       end do

    else

       m = mod(n,3)
       if (m /= 0) then
          do i = 1,m
             stemp = sx(i)
             sx(i) = sy(i)
             sy(i) = stemp
          end do
          if (n < 3) return
       end if

       mp1 = m + 1
       do i = mp1,n,3
          stemp = sx(i)
          sx(i) = sy(i)
          sy(i) = stemp
          stemp = sx(i + 1)
          sx(i + 1) = sy(i + 1)
          sy(i + 1) = stemp
          stemp = sx(i + 2)
          sx(i + 2) = sy(i + 2)
          sy(i + 2) = stemp
       end do

    end if

  end subroutine sswap

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function sdot(n,sx,incx,sy,incy)

    !+ad_name  sdot
    !+ad_summ  Routine to compute X*Y where X and Y are vectors
    !+ad_type  Function returning real
    !+ad_auth  Jack Dongarra, Linpack
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  n        : input integer : order of the matrices sx, sy
    !+ad_args  sx(n*incx) : input real array : first vector
    !+ad_args  incx     : input integer : interval in storage between sx array elements
    !+ad_args  sy(n*incy) : input real array : second vector
    !+ad_args  incy     : input integer : interval in storage between sy array elements
    !+ad_desc  This routine performs the dot product of two vectors, i.e.
    !+ad_desc  calculates the sum from i=1 to N, of X(i)*Y(i).
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  11/03/78 JD  Linpack version
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: sdot

    !  Arguments

    integer, intent(in) :: n,incx,incy
    real(kind(1.0D0)), dimension(n*incx), intent(in) :: sx
    real(kind(1.0D0)), dimension(n*incy), intent(in) :: sy

    !  Local variables

    integer :: ix,i,iy
    real(kind(1.0D0)) :: sw

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    sw = 0.0D0
    ix = 1
    iy = 1
    do i = 1,n
       sw = sw + (sx(ix) * sy(iy))
       ix = ix + incx
       iy = iy + incy
    end do

    sdot = sw

  end function sdot

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function isamax(n,sx,incx)

    !+ad_name  isamax
    !+ad_summ  Routine to finds the index of the array element having
    !+ad_summ  the maximum absolute value
    !+ad_type  Function returning integer
    !+ad_auth  Jack Dongarra, Linpack
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  n        : input integer : order of the matrix sx
    !+ad_args  sx(n*incx) : input real array : array being checked
    !+ad_args  incx     : input integer : interval in storage between sx array elements
    !+ad_desc  This routine finds the array element with the maximum
    !+ad_desc  absolute value, and returns the element index.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  11/03/78 JD  Linpack version
    !+ad_hist  29/09/88 JD  Corrected problem with negative increments
    !+ad_hist  15/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer :: isamax

    !  Arguments

    integer, intent(in) :: n, incx
    real(kind(1.0D0)), dimension(n*incx), intent(in) :: sx

    !  Local variables

    integer :: i,ix
    real(kind(1.0D0)) :: smax

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    isamax = 0
    if (n < 1) return

    isamax = 1
    if (n == 1) return

    if (incx /= 1) then

       ix = 1
       if (incx < 0) ix = (-n+1)*incx + 1
       smax = abs(sx(ix))
       ix = ix + incx
       do i = 2,n
          if (abs(sx(ix)) > smax) then
             isamax = i
             smax = abs(sx(ix))
          end if
          ix = ix + incx
       end do

    else

       smax = abs(sx(1))
       do i = 2,n
          if (abs(sx(i)) <= smax) cycle
          isamax = i
          smax = abs(sx(i))
       end do

    end if

  end function isamax

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine svd(nm,m,n,a,w,matu,u,matv,v,ierr,rv1)

    !+ad_name  svd
    !+ad_summ  Singular Value Decomposition
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  B. S. Garbow, Applied Mathematics Division, Argonne National Laboratory
    !+ad_cont  N/A
    !+ad_args  nm : input integer : Max number of rows of arrays a, u, v; >= m,n
    !+ad_args  m : input integer : Actual number of rows of arrays a, u
    !+ad_args  n : input integer : Number of columns of arrays a, u, and the order of v
    !+ad_args  a(nm,n) : input/output real array : On input matrix to be decomposed;
    !+ad_argc            on output, either unchanged or overwritten with u or v
    !+ad_args  w(n) : output real array : The n (non-negative) singular values of a
    !+ad_argc            (the diagonal elements of s); unordered.  If an error exit
    !+ad_argc            is made, the singular values should be correct for indices
    !+ad_argc            ierr+1,ierr+2,...,n.
    !+ad_args  matu : input logical : Set to .true. if the u matrix in the
    !+ad_argc            decomposition is desired, and to .false. otherwise.
    !+ad_args  u(nm,n) : output real array : The matrix u (orthogonal column vectors)
    !+ad_argc            of the decomposition if matu has been set to .true., otherwise
    !+ad_argc            u is used as a temporary array.  u may coincide with a.
    !+ad_argc            If an error exit is made, the columns of u corresponding
    !+ad_argc            to indices of correct singular values should be correct.
    !+ad_args  matv : input logical : Set to .true. if the v matrix in the
    !+ad_argc            decomposition is desired, and to .false. otherwise.
    !+ad_args  v(nm,n) : output real array : The matrix v (orthogonal) of the
    !+ad_argc            decomposition if matv has been set to .true., otherwise
    !+ad_argc            v is not referenced.  v may also coincide with a if u is
    !+ad_argc            not needed.  If an error exit is made, the columns of v
    !+ad_argc            corresponding to indices of correct singular values
    !+ad_argc            should be correct.
    !+ad_args  ierr : output integer :  zero for normal return, or <I>k</I> if the
    !+ad_argc            k-th singular value has not been determined after 30 iterations.
    !+ad_args  rv1(n) : output real array : work array
    !+ad_desc  This subroutine is a translation of the algol procedure SVD,
    !+ad_desc  Num. Math. 14, 403-420(1970) by Golub and Reinsch,
    !+ad_desc  Handbook for Auto. Comp., vol II - Linear Algebra, 134-151(1971).
    !+ad_desc  <P>It determines the singular value decomposition
    !+ad_desc  <I>a=usv<SUP>t</SUP></I> of a real m by n rectangular matrix.
    !+ad_desc  Householder bidiagonalization and a variant of the QR
    !+ad_desc  algorithm are used.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  19/09/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: nm, m, n
    logical, intent(in) :: matu, matv
    real(kind(1.0D0)), dimension(nm,n), intent(inout) :: a
    real(kind(1.0D0)), dimension(nm,n), intent(out) :: u, v
    real(kind(1.0D0)), dimension(n), intent(out) :: w, rv1
    integer, intent(out) :: ierr

    !  Local variables

    integer :: i,j,k,l,ii,i1,kk,k1,ll,l1,mn,its
    real(kind(1.0D0)) :: c,f,g,h,s,x,y,z,scale,anorm

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ierr = 0

    u = a

    !  Householder reduction to bidiagonal form

    g = 0.0D0
    scale = 0.0D0
    anorm = 0.0D0

    do i = 1, n

       l = i + 1
       rv1(i) = scale * g
       g = 0.0D0
       s = 0.0D0
       scale = 0.0D0

       if (i <= m) then

          do k = i, m
             scale = scale + abs(u(k,i))
          end do

          if (scale /= 0.0D0) then

             do k = i, m
                u(k,i) = u(k,i) / scale
                s = s + u(k,i)**2
             end do

             f = u(i,i)
             g = -sign(sqrt(s),f)
             h = f * g - s
             u(i,i) = f - g

             if (i /= n) then
                do j = l, n
                   s = 0.0D0
                   do k = i, m
                      s = s + u(k,i) * u(k,j)
                   end do
                   f = s / h
                   do k = i, m
                      u(k,j) = u(k,j) + f * u(k,i)
                   end do
                end do
             end if

             do k = i, m
                u(k,i) = scale * u(k,i)
             end do

          end if

       end if

       w(i) = scale * g
       g = 0.0D0
       s = 0.0D0
       scale = 0.0D0

       if (.not.((i > m) .or. (i == n))) then

          do k = l, n
             scale = scale + abs(u(i,k))
          end do

          if (scale /= 0.0D0) then

             do k = l, n
                u(i,k) = u(i,k) / scale
                s = s + u(i,k)**2
             end do

             f = u(i,l)
             g = -sign(sqrt(s),f)
             h = f * g - s
             u(i,l) = f - g

             do k = l, n
                rv1(k) = u(i,k) / h
             end do

             if (i /= m) then
                do j = l, m
                   s = 0.0D0
                   do k = l, n
                      s = s + u(j,k) * u(i,k)
                   end do
                   do k = l, n
                      u(j,k) = u(j,k) + s * rv1(k)
                   end do
                end do
             end if

             do k = l, n
                u(i,k) = scale * u(i,k)
             end do

          end if

       end if

       anorm = max(anorm,abs(w(i))+abs(rv1(i)))

    end do  ! i

    !  Accumulation of right-hand transformations

    if (matv) then

       !  For i=n step -1 until 1 do
       do ii = 1, n
          i = n + 1 - ii
          if (i /= n) then

             if (g /= 0.0D0) then
                do j = l, n
                   !  Double division avoids possible underflow
                   v(j,i) = (u(i,j) / u(i,l)) / g
                end do
                do j = l, n
                   s = 0.0D0
                   do k = l, n
                      s = s + u(i,k) * v(k,j)
                   end do
                   do k = l, n
                      v(k,j) = v(k,j) + s * v(k,i)
                   end do
                end do
             end if

             do j = l, n
                v(i,j) = 0.0D0
                v(j,i) = 0.0D0
             end do

          end if

          v(i,i) = 1.0D0
          g = rv1(i)
          l = i
       end do

    end if

    !  Accumulation of left-hand transformations

    if (matu) then

       !  For i=min(m,n) step -1 until 1 do
       mn = n
       if (m < n) mn = m

       do ii = 1, mn
          i = mn + 1 - ii
          l = i + 1
          g = w(i)
          if (i /= n) then
             do j = l, n
                u(i,j) = 0.0D0
             end do
          end if

          if (g /= 0.0D0) then

             if (i /= mn) then
                do j = l, n
                   s = 0.0D0
                   do k = l, m
                      s = s + u(k,i) * u(k,j)
                   end do
                   f = (s / u(i,i)) / g  !  Double division avoids possible underflow
                   do k = i, m
                      u(k,j) = u(k,j) + f * u(k,i)
                   end do
                end do
             end if

             do j = i, m
                u(j,i) = u(j,i) / g
             end do

          else
             do j = i, m
                u(j,i) = 0.0D0
             end do
          end if

          u(i,i) = u(i,i) + 1.0D0

       end do

    end if

    !  Diagonalization of the bidiagonal form
    !  For k=n step -1 until 1 do

    do kk = 1, n
       k1 = n - kk
       k = k1 + 1
       its = 0

       !  Test for splitting.
       !  For l=k step -1 until 1 do

       do
          do ll = 1, k
             l1 = k - ll
             l = l1 + 1
             if ((abs(rv1(l)) + anorm) == anorm) goto 470

             !  rv1(1) is always zero, so there is no exit
             !  through the bottom of the loop

             !+**PJK 23/05/06 Prevent problems from the code getting here with l1=0
             if (l1 == 0) then
                write(*,*) 'SVD: Shouldn''t get here...'
                goto 470
             end if

             if ((abs(w(l1)) + anorm) == anorm) exit
          end do

          !  Cancellation of rv1(l) if l greater than 1

          c = 0.0D0
          s = 1.0D0

          do i = l, k
             f = s * rv1(i)
             rv1(i) = c * rv1(i)
             if ((abs(f) + anorm) == anorm) exit
             g = w(i)
             h = sqrt(f*f+g*g)
             w(i) = h
             c = g / h
             s = -f / h
             if (.not. matu) cycle

             do j = 1, m
                y = u(j,l1)
                z = u(j,i)
                u(j,l1) = y * c + z * s
                u(j,i) = -y * s + z * c
             end do
          end do

470       continue

          !  Test for convergence

          z = w(k)
          if (l == k) exit

          !  Shift from bottom 2 by 2 minor

          if (its == 30) then
             !  Set error - no convergence to a
             !  singular value after 30 iterations
             ierr = k
             return
          end if

          its = its + 1
          x = w(l)
          y = w(k1)
          g = rv1(k1)
          h = rv1(k)
          f = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.D0 * h * y)
          g = sqrt(f*f+1.D0)
          f = ((x - z) * (x + z) + h * (y / (f + sign(g,f)) - h)) / x

          !  Next QR transformation

          c = 1.0D0
          s = 1.0D0

          do i1 = l, k1
             i = i1 + 1
             g = rv1(i)
             y = w(i)
             h = s * g
             g = c * g
             z = sqrt(f*f+h*h)
             rv1(i1) = z
             c = f / z
             s = h / z
             f = x * c + g * s
             g = -x * s + g * c
             h = y * s
             y = y * c

             if (matv) then
                do j = 1, n
                   x = v(j,i1)
                   z = v(j,i)
                   v(j,i1) = x * c + z * s
                   v(j,i) = -x * s + z * c
                end do
             end if

             z = sqrt(f*f+h*h)
             w(i1) = z

             !  Rotation can be arbitrary if z is zero

             if (z /= 0.0D0) then
                c = f / z
                s = h / z
             end if

             f = c * g + s * y
             x = -s * g + c * y
             if (.not. matu) cycle

             do j = 1, m
                y = u(j,i1)
                z = u(j,i)
                u(j,i1) = y * c + z * s
                u(j,i) = -y * s + z * c
             end do

          end do

          rv1(l) = 0.0D0
          rv1(k) = f
          w(k) = x

       end do

       !  Convergence

       if (z >= 0.0D0) cycle

       !  w(k) is made non-negative
       w(k) = -z
       if (.not. matv) cycle

       do j = 1, n
          v(j,k) = -v(j,k)
       end do

    end do

  end subroutine svd

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  recursive subroutine vmcon( &
       fcnvmc1,fcnvmc2,mode,n,m,meq,x,objf,fgrd,conf,cnorm,lcnorm, &
       b,lb,tol,maxfev,info,nfev,niter,vlam,glag,vmu,cm,glaga,gamma,eta, &
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
    !+ad_auth  M D Kovari, CCFE, Culham Science Centre
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
    !+ad_args  maxfev : input integer : maximum number of calls to FCNVMC1
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
    !+ad_argc    INFO = 7 : line search has been aborted
    !+ad_args  nfev : output integer : number of calls to FCNVMC1
    !+ad_args  niter : output integer : number of iterations
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
    !+ad_hist  06/11/12 PJK Added recursive attribute, for startup routine
    !+ad_hist  25/02/14 MDK Added an escape from the line search to help ensure
    !+ad_hisc               convergence
    !+ad_hist  26/02/14 PJK Added new output argument niter
    !+ad_hist  08/07/14 PJK/MDK Added a test of the residuals to the convergence
    !+ad_hisc               criteria
    !+ad_hist  10/09/14 PJK/MDK Added new info=7 value
    !+ad_stat  Okay
    !+ad_docs  ANL-80-64: Solution of the General Nonlinear Programming Problem
    !+ad_docc  with Subroutine VMCON, Roger L Crane, Kenneth E Hillstrom and
    !+ad_docc  Michael Minkoff, Argonne National Laboratory, 1980
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: mode,n,m,meq,lcnorm,lb,maxfev,ldel,lh,lwa,liwa
    integer, intent(out) :: info,nfev,niter

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
    real(kind(1.0D0)) :: summ, sqsumsq, sqsumsq_tol
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
    !  nqp is the number of quadratic subproblems (= number of iterations)

    nfev = 1
    nsix = 6*np1
    nqp = 0 ; niter = 0

    !  Calculate the initial functions and gradients

    call fcnvmc1(n,m,x,objf,conf,info)
    if (info < 0) return

    call fcnvmc2(n,m,x,fgrd,cnorm,lcnorm,info)
    if (info < 0) return

	!  Setup line overwrite for VMCON iterations output
	open(unit=iotty)
	write(*,*) ""
	! MDK To prevent circular dependencies in compilation, I will replace this
	! a simple write statement
    ! call oheadr(iotty, "VMCON Iterations")
    write(*,*) "VMCON Iterations"

    !  Start the iteration by calling the quadratic programming
    !  subroutine

    iteration: do
       !  Output to terminal number of VMCON iterations
       write(iotty, '("+", I20, "vmcon iterations")'), niter+1

       !  Increment the quadratic subproblem counter
       nqp = nqp + 1
       niter = nqp

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
58        continue
          ki = iwa(j) - np1
          k = ki + m + np1
          if (ki == np1) cycle
          if (iupper(ki) == 1) goto 59
          cycle
59        continue
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

       !  Check convergence of constraint residuals
       summ = 0.0D0
       do i = 1,m
          summ = summ + conf(i)*conf(i)
       end do
       sqsumsq = sqrt(summ)

       if (verbose == 1) then
          write(*,'(a,es13.5,a,es13.5)') &
               'Constraint residuals (sqsumsq) = ',sqsumsq, &
               ' Convergence parameter = ',sum
       end if

       !  Exit if both convergence criteria are satisfied
       !  (the original criterion, plus constraint residuals below the tolerance level)
       !  Temporarily set the two tolerances equal (should perhaps be an input parameter)
       sqsumsq_tol = tol
       if ((sum <= tol).and.(sqsumsq < sqsumsq_tol)) then
          if (verbose == 1) then
             write(*,*) 'Convergence parameter < convergence criterion (epsvmc)'
             write(*,*) 'Root of sum of squares of residuals < tolerance (sqsumsq_tol)'
          end if
          return
       end if

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

             !  Escape from the line search if the line search function is increasing
             !  Outer loop is forced to repeat

             info = 1  !  reset on each iteration
             if (aux > 0.0D0) then
                if (verbose == 1) then
                   write(*,*) 'VMCON optimiser line search attempt '// &
                        'failed - retrying...'
                end if
                info = 7
                exit line_search
             end if

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    !  25/02/14 PJK Diagnostic output added

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

30  continue

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

50  continue
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
70     continue

       !  If the constraint is an equality or a violated inequality set
       !  the constant term to zero and put the function value in the
       !  constraint matrix for the (n+1)st variable

       cm(k) = zero
       cnorm(np1,k) = conf(k)
90     continue
    end do

    !  Set the upper bound of the (n+1)st variable
    !  Set iflag. iflag will be used in checking active constraints
    !  Call subroutine harwqp to solve quadratic programming problem

    bdu(np1) = one
    iflag = -1
100 continue

    call harwqp(np1,mtotal,b,lb,gm,cnorm,lcnorm,cm,bdl,bdu,delta, &
         mact,meq,h,lh,iwa,wa,iwa(4*(n+1)+m+1),mode,info)

    if (info /= 1) then
       if (verbose == 1) then
          write(*,*) 'A singular matrix was detected in HINV: info /= 1'
       end if
       goto 130
    end if

    !  Check whether the required feasibility conditions hold
    !  If delta(np1) is sufficiently small there is no feasible
    !  solution

    if (delta(np1) <= cdm6) then
       if (verbose == 1) then
          write(*,*) &
               'QPSUB: delta(np1) is too small: no  solution'
          write(*,*) 'delta(np1)=',delta(np1),' np1=',np1
       end if
       goto 120
    end if

    !  Check whether active constraints are bounds

    do j = 1, mact
       if (iwa(j) > npp) goto 110
       if (iwa(j) == npp) goto 101
       if (iwa(j) > np1) goto 105
       if (iwa(j) == np1) then
          if (verbose == 1) write(*,*) &
               'QPSUB: iwa(j) == np1',' j=',j,' iwa(j)=',iwa(j)
          goto 130
       end if
       if (ilower(iwa(j)) == 0) then
          if (verbose == 1) then
             write(*,*) 'QPSUB: An artificial constraint is active:'
             write(*,*) 'ilower(iwa(j)) == 0',' j=',j,' iwa(j)=',iwa(j)
          end if
          goto 130
       end if
       goto 110
105    continue
       inx = iwa(j) - np1
       if (iupper(inx) == 0) then
          if (verbose == 1) then
             write(*,*) 'QPSUB: An artificial constraint is active:'
             write(*,*) 'iupper(inx) == 0',' inx=',inx
          end if
          goto 130
       end if
       goto 110
101    continue

       !  The active constraint is blu(np1)

       iflag = 1
110    continue
    end do

    !  Normal exit

    if (iflag >= 1) goto 140

    !  A second call to harwqp found blu(np1) to still be inactive
    !  thus an error exit is made

    if (iflag >= 0) then
       if (verbose == 1) then
          write(*,*) 'QPSUB: A second call to HARWQP '// &
               'found blu(np1) to still be inactive:'
          write(*,*) 'iflag = ',iflag
       end if
       goto 120
    end if

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    !  25/02/14 PJK Diagnostic output added

    IMPLICIT NONE

    INTEGER n,m,ia,ic,k,ke,ih,mode,info
    INTEGER iwa(*),lt(*)
    INTEGER i, ial, ib, ii, j, li, ni, nk, nn, n3,n4,n5,n6
    INTEGER i0,i1,i2,i3

    real(kind(1.0D0)) a(ia,*),b(*),c(ic,*),d(*),bdl(*),bdu(*),x(*), &
         h(ih,*),wa(*)
    real(kind(1.0D0)) alpha, cac, cc, chc, ghc, y, z, zz
    real(kind(1.0D0)) r0
    real(kind(1.0D0)), dimension(2) :: det

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

8   continue
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

99  continue
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

104    continue
       li = li-n
       h(n+i,li) = -1.0D0
       h(li,n+i) = -1.0D0
       x(nn+i) = -bdu(li)
       goto 108

105    continue
       li = li-nn
       do j = 1,n
          h(n+i,j) = c(j,li)
          h(j,n+i) = c(j,li)
       end do
       x(nn+i) = d(li)

108    continue
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

116    continue
       x(nn+i) = -bdu(li-n)
       goto 115

117    continue
       x(nn+i)=d(li-nn)
115    continue
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

111    continue
       if (i > nn) goto 112
       z = bdu(i-n)-x(i-n)
       goto 114

112    continue
       j = i-nn
       call dotpmc(c(1,j),i1,x(1),i1,d(j),z,n,i2)

114    continue
       if (z < 0.0D0) goto 8
110    continue
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
122    continue
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

141    continue
       if (i > nn) goto 142
       if (x(n4+i) <= 0.0D0) goto 140
       cc = (bdu(i-n)-x(i-n))/x(n4+i)
       goto 143

142    continue
       j = i-nn
       call dotpmc(c(1,j),i1,x(n5+1),i1,r0,zz,n,i0)
       if (zz >= 0.0D0) goto 140
       call dotpmc(c(1,j),i1,x(1),i1,d(j),cc,n,i1)
       cc = cc/zz

143    continue
       if (cc >= alpha) goto 140
       alpha = cc
       ial = i
140    continue
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

182    continue
       zz = z*x(n4+i)
       do j = 1,n
          h(ni,j) = h(ni,j)-zz*x(nn+j)
       end do
181    continue
    end do
    lt(ii) = ial
    goto 120

200 continue
    k = 0

    ke = 0
    do i = 1,m
       lt(nn+i) = 1
    end do

    !  MDK Check for singular matrix
    !  h matrix is unchanged if determinant only (job=10) is requested.

    if (verbose == 1) then
       call sgedi(h,ih,n,iwa,det,10)
       if (det(1) == 0.0D0) then
          write(*,*) 'HARWQP: Determinant=0 at checkpoint 1'
       end if
    end if

    call hinv(h,ih,n,iwa,info)
    if (info  /=  1) goto 1000

    !  Start with empty basis from feasible point
    !  Search direction is -a(-1).b

    goto 167

1000 continue

    return
  end SUBROUTINE HARWQP

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

6      continue
       lt(i) = n+i
       h(i,i) = -1.0D0
998    continue
       lt(nn+lt(i)) = 0
    end do
    k = n
    goto 40

    !  Set up normals v of the k designated constraints in basis

10  continue
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

13     continue
       h(li-n,ni) = -1.0D0
       goto 11

14     continue
       li = li-nn
       do j = 1,n
          h(j,ni) = c(j,li)
       end do
11     continue
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

19  continue

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

29  continue
    z = 1.0D0
    do i = 1,n
       if (lt(n+i) == 1) goto 25
       if (x(n+i) >= z) goto 25
       z = x(n+i)
       ii = i
25     continue
    end do
    y = 1.0D0
    if ( (x(ii)-bdl(ii)) > (bdu(ii)-x(ii)) ) y = -1.0D0

    !  Calculate vectors vplus.e(i) and  u = e(i)-v.vplus.e(i)

    if (y /= 1.0D0) goto 27
    do i = 1,k
       x(nn+i) = h(i,ii)
    end do
    goto 30

27  continue
    do i = 1,k
       x(nn+i) = -h(i,ii)
    end do

30  continue
    do i = 1,n
       if (lt(n+i) == 1) goto 31
       call dotpmc(h(i,n+1),ih,x(nn+1),i1,r0,x(n3+i),kv,i3)
31     continue
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
33     continue
    end do

    do i = 1,n
       if (lt(n+i) == 1) goto 35
       x(n+i) = x(n+i)+x(n3+i)**2/z
35     continue
    end do
    k = k+1
    h(k,ii) = y
    if (y /= 1.0D0) ii = ii+n
    lt(nn+ii) = 0
    lt(k) = ii
    if (k /= n) goto 29

    !  Set up rhs of constraints in basis

40  continue
    do i = 1,n
       li = lt(i)
       if (li > n) goto 42
       x(n+i) = bdl(li)
       goto 41

42     continue
       if (li > nn) goto 43
       x(n+i) = -bdu(li-n)
       goto 41

43     continue
       x(n+i) = d(li-nn)

41     continue
    end do

    !  Calculate position of vertex

    do i = 1,n
       call dotpmc(h(1,i),i1,x(n+1),i1,r0,x(i),n,i0)
    end do

    !  Calculate the constraint residuals, the number of violated
    !  constraints, and the sum of their normals

50  continue
    kv = 0
    do i = 1,n
       x(n+i) = 0.0D0
    end do
    do i = 1,m
       if (lt(nn+i) <= 0) goto 52
       if (i > n) goto 53
       z = x(i)-bdl(i)
       goto 55

53     continue
       if (i > nn) goto 54
       z = bdu(i-n)-x(i-n)
       goto 55

54     continue
       j = i-nn
       call dotpmc(c(1,j),i1,x(1),i1,d(j),z,n,i2)

55     continue
       x(nn+i) = z
       if (z >= 0.0D0) goto 52
       kv = kv+1
       lt(nn+i) = 2
       if (i > n) goto 56
       x(n+i) = x(n+i)+1.0D0
       goto 52

56     continue
       if (i > nn) goto 57
       x(i) = x(i)-1.0D0
       goto 52

57     continue
       do ii = 1,n
          x(n+ii) = x(n+ii)+c(ii,j)
       end do
52     continue
    end do
    if (kv /= 0) goto 63
    goto 1000

    !  Possible directions of search obtainable by removing a
    !  constraint are rows of h,  calculate the optimum direction

63  continue
    z = 0.0D0
    do i = 1,n
       if (lt(nn+lt(i)) == -1) goto 64
       call dotpmc(h(i,1),ih,x(n+1),i1,r0,y,n,i0)
       if (y <= z) goto 64
       z = y
       ii = i
64     continue
    end do
    if (z > 0.0D0) goto 70
    k = 0
    goto 1000

    !  Search for the nearest of the furthest violated constraint
    !  and the nearest nonviolated nonbasic constraint

70  continue
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

73     continue
       if (i > nn) goto 74
       z = x(i)
       goto 75

74     continue
       jj = i-nn
       call dotpmc(x(n+1),i1,c(1,jj),i1,r0,z,n,i3)

75     continue
       if (lt(nn+i) == 2) goto 76
       if (z <= 0.0D0) goto 72
       z = x(nn+i)/z
       if (z >= alpha) goto 72
       alpha = z
       ial = i
       goto 72

76     continue
       lt(nn+i) = 1
       if (z >= 0.0D0) goto 72
       z = x(nn+i)/z
       if (z <= beta) goto 72
       beta = z
       ib = i
72     continue
    end do

    if (alpha > beta) goto 80
    ib = ial
    beta = alpha

    !  Exchange with the constraint being removed from the basis,
    !  using simplex formula for new h

80  continue
    lt(nn+lt(ii)) = 1
    lt(nn+ib) = 0
    lt(ii) = ib
    if (ib > n) goto 82
    do i = 1,n
       x(nn+i) = h(i,ib)
    end do
    goto 90

82  continue
    ib = ib-n
    if (ib > n) goto 84
    do i = 1,n
       x(nn+i) = -h(i,ib)
    end do
    goto 90

84  continue
    ib = ib-n
    do i = 1,n
       x(n3+i) = c(i,ib)
    end do
    do i = 1,n
       call dotpmc(h(i,1),ih,x(n3+1),i1,r0,x(nn+i),n,i0)
    end do

90  continue
    z = 1.0D0/x(nn+ii)
    do i = 1,n
       x(i) = x(i)+beta*x(n+i)
       if (i /= ii) goto 92
       do j = 1,n
          h(i,j) = h(i,j)*z
       end do
       goto 91

92     continue
       zz = z*x(nn+i)
       do j = 1,n
          h(i,j) = h(i,j)-zz*x(n+j)
       end do
91     continue
    end do

    goto 50

1000 continue

    return
  end SUBROUTINE HARWFP

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE HYBRD( &
       fcnhyb,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,diag, &
       mode,factor,nprint,info,nfev,fjac,ldfjac,r,lr, &
       qtf,wa1,wa2,wa3,wa4,resdl)

    !  www.math.utah.edu/software/minpack/minpack/hybrd.html

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

    EXTERNAL fcnhyb

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

20  continue

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

30  continue
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

50  continue

    !  On the first iteration, calculate the norm of the scaled x
    !  and initialize the step bound delta.

    do j = 1, n
       wa3(j) = diag(j)*x(j)
    end do
    xnorm = enorm(n,wa3)
    delta = factor*xnorm
    if (delta == zero) delta = factor

70  continue

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

110    continue
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

140    continue
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

20     continue
       temp = r(jj)
       if (temp  /=  zero) goto 40
       l = j
       do i = 1, j
          temp = max(temp,abs(r(l)))
          l = l + n - i
       end do
       temp = epsmch*temp
       if (temp == zero) temp = epsmch

40     continue
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

10     continue
       s1 = s1 + (xabs/x1max)**2

20     continue
       goto 60

30     continue

       !  Sum for small components.

       if (xabs <= x3max) goto 40
       s3 = one + s3*(x3max/xabs)**2
       x3max = xabs
       goto 50

40     continue
       if (xabs  /=  zero) s3 = s3 + (xabs/x3max)**2

50     continue
60     continue
       goto 80

70     continue

       !  Sum for intermediate components.

       s2 = s2 + xabs**2

80     continue
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    EXTERNAL  fcnhyb

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

30  continue
    goto 110

40  continue

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

30  continue

    !  Initialize remaining columns to those of the identity matrix.

    np1 = n + 1
    if (m < np1) goto 60
    do j = np1, m
       do i = 1, m
          q(i,j) = zero
       end do
       q(j,j) = one
    end do

60  continue

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

110    continue
    end do

    return
  end SUBROUTINE QFORM

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

40     continue

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

80        continue
       end do

100    continue
       rdiag(j) = -ajnorm
    end do

    return
  end SUBROUTINE QRFAC

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

50  continue

    return
  end SUBROUTINE R1MPYQ

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

20     continue
       tan1 = v(j)/v(n)
       cos1 = p5/sqrt(p25+p25*tan1**2)
       sin1 = cos1*tan1
       tau = sin1

30     continue

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

50     continue
    end do

70  continue

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

90     continue
       tan1 = w(j)/s(jj)
       cos1 = p5/sqrt(p25+p25*tan1**2)
       sin1 = cos1*tan1
       tau = sin1

100    continue

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

120    continue

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function spmpar(i)

    !+ad_name  spmpar
    !+ad_summ  Calculates machine (computing) parameters
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  i : input integer : Switch for return value:
    !+ad_argc                      i=1 : B**(1 - P), the machine precision
    !+ad_argc                      i=2 : B**(EMIN - 1), the smallest magnitude
    !+ad_argc                      i=3 : B**EMAX*(1 - B**(-P)), the largest magnitude
    !+ad_argc     where the machine being used has P base B digits, and its smallest
    !+ad_arc      and largest exponents are EMIN and EMAX, respectively.
    !+ad_desc  This routine evaluates the numerical machine parameters of the
    !+ad_desc  computer being used to run the program, as defined above.
    !+ad_desc  <P>Note that the values of these parameters can be found for a given
    !+ad_desc  machine if the Mark 12 or later NAg library is installed on it.
    !+ad_desc  <P><CODE>SPMPAR(1)</CODE> is equivalent to <CODE>X02AJF()</CODE>;
    !+ad_desc  <BR><CODE>SPMPAR(2)</CODE> is equivalent to <CODE>X02AKF()</CODE>;
    !+ad_desc  <BR><CODE>SPMPAR(3)</CODE> is equivalent to <CODE>X02ALF()</CODE>.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  28/07/11 PJK Initial F90 version
    !+ad_hist  08/04/13 PJK Modified to use Fortran intrinsic functions
    !+ad_stat  Okay
    !+ad_docs  Metcalf and Reid, Fortran 90/95 Explained, 2nd Edition (section 8.7.2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: spmpar

    !  Arguments

    integer, intent(in) :: i

    !  Local variables

    real(kind(1.0D0)), dimension(3) :: rmach

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Previously-used hardwired values shown in comments

    !rmach(1) = 1.110223024625157D-016
    rmach(1) = epsilon(0.0D0)

    !rmach(2) = 2.3D-308
    rmach(2) = tiny(0.0D0)

    !rmach(3) = 1.797693134862316D+308
    rmach(3) = huge(0.0D0)

    spmpar = rmach(i)

  end function spmpar

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine eshellvol(rshell,rmini,rmino,zminor,drin,drout,dz,vin,vout,vtot)

    !+ad_name  eshellvol
    !+ad_summ  Routine to calculate the inboard, outboard and total volumes
    !+ad_summ  of a toroidal shell comprising two elliptical sections
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rshell : input real : major radius of centre of both ellipses (m)
    !+ad_args  rmini  : input real : horizontal distance from rshell to outer edge
    !+ad_argc                        of inboard elliptical shell (m)
    !+ad_args  rmino  : input real : horizontal distance from rshell to inner edge
    !+ad_argc                        of outboard elliptical shell (m)
    !+ad_args  zminor : input real : vertical internal half-height of shell (m)
    !+ad_args  drin   : input real : horiz. thickness of inboard shell at midplane (m)
    !+ad_args  drout  : input real : horiz. thickness of outboard shell at midplane (m)
    !+ad_args  dz     : input real : vertical thickness of shell at top/bottom (m)
    !+ad_args  vin    : output real : volume of inboard section (m3)
    !+ad_args  vout   : output real : volume of outboard section (m3)
    !+ad_args  vtot   : output real : total volume of shell (m3)
    !+ad_desc  This routine calculates the volume of the inboard and outboard sections
    !+ad_desc  of a toroidal shell defined by two co-centred semi-ellipses.
    !+ad_desc  Each section's internal and external surfaces are in turn defined
    !+ad_desc  by two semi-ellipses. The volumes of each section are calculated as
    !+ad_desc  the difference in those of the volumes of revolution enclosed by their
    !+ad_desc  inner and outer surfaces.
    !+ad_desc  <P>See also <A HREF="eshellarea.html"><CODE>eshellarea</CODE></A>
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/13 PJK Initial version
    !+ad_hist  13/02/15 JM  Moved to maths library
    !+ad_stat  Okay
    !+ad_docs  Internal CCFE note T&amp;M/PKNIGHT/PROCESS/009, P J Knight:
    !+ad_docc  Surface Area and Volume Calculations for Toroidal Shells
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: rshell, rmini, rmino, zminor, drin, drout, dz
    real(kind=double), intent(out) :: vin, vout, vtot

    !  Local variables
    real(kind=double) :: a, b, elong, v1, v2

    !  Global shared variables
    !  Input: pi,twopi
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Inboard section

    !  Volume enclosed by outer (higher R) surface of elliptical section
    !  and the vertical straight line joining its ends
    a = rmini ; b = zminor ; elong = b/a
    v1 = twopi * elong * (0.5D0*pi*rshell*a*a - 2.0D0/3.0D0*a*a*a)

    !  Volume enclosed by inner (lower R) surface of elliptical section
    !  and the vertical straight line joining its ends
    a = rmini+drin ; b = zminor+dz ; elong = b/a
    v2 = twopi * elong * (0.5D0*pi*rshell*a*a - 2.0D0/3.0D0*a*a*a)

    !  Volume of inboard section of shell
    vin = v2 - v1

    !  Outboard section

    !  Volume enclosed by inner (lower R) surface of elliptical section
    !  and the vertical straight line joining its ends
    a = rmino ; b = zminor ; elong = b/a
    v1 = twopi * elong * (0.5D0*pi*rshell*a*a + 2.0D0/3.0D0*a*a*a)

    !  Volume enclosed by outer (higher R) surface of elliptical section
    !  and the vertical straight line joining its ends
    a = rmino+drout ; b = zminor+dz ; elong = b/a
    v2 = twopi * elong * (0.5D0*pi*rshell*a*a + 2.0D0/3.0D0*a*a*a)

    !  Volume of outboard section of shell
    vout = v2 - v1

    !  Total shell volume
    vtot = vin + vout

  end subroutine eshellvol

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshellvol(rmajor,rminor,zminor,drin,drout,dz,vin,vout,vtot)

    !+ad_name  dshellvol
    !+ad_summ  Routine to calculate the inboard, outboard and total volumes
    !+ad_summ  of a D-shaped toroidal shell
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rmajor : input real : major radius to outer point of inboard
    !+ad_argc                        straight section of shell (m)
    !+ad_args  rminor : input real : horizontal internal width of shell (m)
    !+ad_args  zminor : input real : vertical internal half-height of shell (m)
    !+ad_args  drin   : input real : horiz. thickness of inboard shell at midplane (m)
    !+ad_args  drout  : input real : horiz. thickness of outboard shell at midplane (m)
    !+ad_args  dz     : input real : vertical thickness of shell at top/bottom (m)
    !+ad_args  vin    : output real : volume of inboard straight section (m3)
    !+ad_args  vout   : output real : volume of outboard curved section (m3)
    !+ad_args  vtot   : output real : total volume of shell (m3)
    !+ad_desc  This routine calculates the volume of the inboard and outboard sections
    !+ad_desc  of a D-shaped toroidal shell defined by the above input parameters.
    !+ad_desc  The inboard section is assumed to be a cylinder of uniform thickness.
    !+ad_desc  The outboard section's internal and external surfaces are defined
    !+ad_desc  by two semi-ellipses, centred on the outer edge of the inboard section;
    !+ad_desc  its volume is calculated as the difference in those of the volumes of
    !+ad_desc  revolution enclosed by the two surfaces.
    !+ad_desc  <P>See also <A HREF="dshellarea.html"><CODE>dshellarea</CODE></A>
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/13 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Internal CCFE note T&amp;M/PKNIGHT/PROCESS/009, P J Knight:
    !+ad_docc  Surface Area and Volume Calculations for Toroidal Shells
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: rmajor, rminor, zminor, drin, drout, dz
    real(kind=double), intent(out) :: vin, vout, vtot

    !  Local variables
    real(kind=double) :: a, b, elong, v1, v2

    !  Global shared variables
    !  Input: pi,twopi
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Volume of inboard cylindrical shell
    vin = 2.0D0*(zminor+dz) * pi*(rmajor**2 - (rmajor-drin)**2)

    !  Volume enclosed by inner surface of elliptical outboard section
    !  and the vertical straight line joining its ends
    a = rminor ; b = zminor ; elong = b/a
    v1 = twopi * elong * (0.5D0*pi*rmajor*a*a + 2.0D0/3.0D0*a*a*a)

    !  Volume enclosed by outer surface of elliptical outboard section
    !  and the vertical straight line joining its ends
    a = rminor+drout ; b = zminor+dz ; elong = b/a
    v2 = twopi * elong * (0.5D0*pi*rmajor*a*a + 2.0D0/3.0D0*a*a*a)

    !  Volume of elliptical outboard shell
    vout = v2 - v1

    !  Total shell volume
    vtot = vin + vout

  end subroutine dshellvol

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshellarea(rmajor,rminor,zminor,ain,aout,atot)

    !+ad_name  dshellarea
    !+ad_summ  Routine to calculate the inboard, outboard and total surface areas
    !+ad_summ  of a D-shaped toroidal shell
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rmajor : input real : major radius of inboard straight section (m)
    !+ad_args  rminor : input real : horizontal width of shell (m)
    !+ad_args  zminor : input real : vertical half-height of shell (m)
    !+ad_args  ain    : output real : surface area of inboard straight section (m3)
    !+ad_args  aout   : output real : surface area of outboard curved section (m3)
    !+ad_args  atot   : output real : total surface area of shell (m3)
    !+ad_desc  This routine calculates the surface area of the inboard and outboard
    !+ad_desc  sections of a D-shaped toroidal shell defined by the above input
    !+ad_desc  parameters.
    !+ad_desc  The inboard section is assumed to be a cylinder.
    !+ad_desc  The outboard section is defined by a semi-ellipse, centred on the
    !+ad_desc  major radius of the inboard section.
    !+ad_desc  <P>See also <A HREF="dshellvol.html"><CODE>dshellvol</CODE></A>
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/13 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Internal CCFE note T&amp;M/PKNIGHT/PROCESS/009, P J Knight:
    !+ad_docc  Surface Area and Volume Calculations for Toroidal Shells
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: rmajor,rminor,zminor
    real(kind=double), intent(out) :: ain,aout,atot

    !  Local variables
    real(kind=double) :: elong

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Area of inboard cylindrical shell
    ain = 4.0D0*zminor*pi*rmajor

    !  Area of elliptical outboard section
    elong = zminor/rminor
    aout = twopi * elong * (pi*rmajor*rminor + 2.0D0*rminor*rminor)

    !  Total surface area
    atot = ain + aout

  end subroutine dshellarea

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine eshellarea(rshell,rmini,rmino,zminor,ain,aout,atot)

    !+ad_name  eshellarea
    !+ad_summ  Routine to calculate the inboard, outboard and total surface areas
    !+ad_summ  of a toroidal shell comprising two elliptical sections
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rshell : input real : major radius of centre of both ellipses (m)
    !+ad_args  rmini  : input real : horizontal distance from rshell to
    !+ad_argc                        inboard elliptical shell (m)
    !+ad_args  rmino  : input real : horizontal distance from rshell to
    !+ad_argc                        outboard elliptical shell (m)
    !+ad_args  zminor : input real : vertical internal half-height of shell (m)
    !+ad_args  ain    : output real : surface area of inboard section (m3)
    !+ad_args  aout   : output real : surface area of outboard section (m3)
    !+ad_args  atot   : output real : total surface area of shell (m3)
    !+ad_desc  This routine calculates the surface area of the inboard and outboard
    !+ad_desc  sections of a toroidal shell defined by two co-centred semi-ellipses.
    !+ad_desc  <P>See also <A HREF="eshellvol.html"><CODE>eshellvol</CODE></A>
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/13 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Internal CCFE note T&amp;M/PKNIGHT/PROCESS/009, P J Knight:
    !+ad_docc  Surface Area and Volume Calculations for Toroidal Shells
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: rshell,rmini,rmino,zminor
    real(kind=double), intent(out) :: ain,aout,atot

    !  Local variables
    real(kind=double) :: elong

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Inboard section
    elong = zminor/rmini
    ain = twopi * elong * (pi*rshell*rmini - 2.0D0*rmini*rmini)

    !  Outboard section
    elong = zminor/rmino
    aout = twopi * elong * (pi*rshell*rmino + 2.0D0*rmino*rmino)

    !  Total surface area
    atot = ain + aout

  end subroutine eshellarea

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module maths_library

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef unit_test

module testdata

  integer :: nfun = 0  !  function call counter

  !  Choose test to run by changing itest in main program below.
  !  1 to 3 are recommended, others are included to probe the code's
  !  behaviour with different initial guesses for the solution vector x

  integer :: itest = 1

  !  Expected answers for tests 1 to 3 are given in
  !  VMCON documentation ANL-80-64

  real(kind(1.0D0)), dimension(2) :: x_exp
  real(kind(1.0D0)), dimension(2) :: c_exp, vlam_exp
  real(kind(1.0D0)) :: objf_exp, errlg_exp, errlm_exp
  real(kind(1.0D0)) :: errcom_exp, errcon_exp
  integer :: ifail_exp

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine inittest(nvar,neqns,nineqns,x,ilower,iupper,bndl,bndu)

    implicit none

    integer, intent(out) :: nvar
    integer, intent(out) :: neqns
    integer, intent(out) :: nineqns
    real(kind(1.0D0)), dimension(:), intent(out) :: x
    integer, dimension(:), intent(out) :: ilower, iupper
    real(kind(1.0D0)), dimension(:), intent(out) :: bndl, bndu

    select case (itest)

    case (1)

       !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
       !  subject to the following constraints:
       !  c1(x1,x2) = x1 - 2*x2 + 1 = 0
       !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
       !
       !  VMCON documentation ANL-80-64

       nvar = 2
       neqns = 1
       nineqns = 1
       x(1) = 2.0D0 ; x(2) = 2.0D0

       !  No bounds on x values set
       ilower(1) = 0 ; ilower(2) = 0
       iupper(1) = 0 ; iupper(2) = 0
       bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

       x_exp(1) = 8.2287565553287513D-01
       x_exp(2) = 9.1143782776643764D-01
       objf_exp = 1.3934649806878849D0
       c_exp(1) = 1.3877787807814457D-17
       c_exp(2) = -7.6716411001598317D-13
       vlam_exp(1) = -1.5944911182523063D0
       vlam_exp(2) = 1.8465914396061125D0
       errlg_exp = 3.3450880954077888D-12
       errlm_exp = 0.0D0
       errcom_exp = 1.4166608063379568D-12
       errcon_exp = 7.6717798780379098D-13
       ifail_exp = 1

    case (2)

       !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
       !  subject to the following constraints:
       !  c1(x1,x2) = x1 - 2*x2 + 1 >= 0
       !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
       !
       !  VMCON documentation ANL-80-64

       nvar = 2
       neqns = 0
       nineqns = 2
       x(1) = 2.0D0 ; x(2) = 2.0D0

       !  No bounds on x values set
       ilower(1) = 0 ; ilower(2) = 0
       iupper(1) = 0 ; iupper(2) = 0
       bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

       x_exp(1) = 1.6649685472365443D0
       x_exp(2) = 5.5404867491788852D-01
       objf_exp = 3.1111865868328270D-01
       c_exp(1) = 1.5568711974007674D0
       c_exp(2) = -1.0214051826551440D-14
       vlam_exp(1) = 0.0D0
       vlam_exp(2) = 8.0489557193146243D-01
       errlg_exp = 2.3433338602885101D-11
       errlm_exp = 0.0D0
       errcom_exp = 8.2212450866697197D-15
       errcon_exp = 1.0214051826551440D-14
       ifail_exp = 1

    case (3)

       !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
       !  subject to the following constraints:
       !  c1(x1,x2) = x1 + x2 - 3 = 0
       !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
       !
       !  Note that this test is supposed to fail with ifail=5
       !  as there is no feasible solution
       !
       !  VMCON documentation ANL-80-64

       nvar = 2
       neqns = 1
       nineqns = 1
       x(1) = 2.0D0 ; x(2) = 2.0D0

       !  No bounds on x values set
       ilower(1) = 0 ; ilower(2) = 0
       iupper(1) = 0 ; iupper(2) = 0
       bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

       x_exp(1) = 2.3999994310874733D0
       x_exp(2) = 6.0000056891252611D-01
       objf_exp = 3.1999908974060504D-01
       c_exp(1) = -6.6613381477509392D-16
       c_exp(2) = -8.000000000004035D-01
       vlam_exp(1) = 0.0D0
       vlam_exp(2) = 0.0D0
       errlg_exp = 1.599997724349894D0
       errlm_exp = 0.0D0
       errcom_exp = 0.0D0
       errcon_exp = 8.0000000000040417D-01
       ifail_exp = 5

    case (4)

       !  Maximise f(x1,x2) = x1 + x2
       !  subject to the following constraint:
       !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0
       !
       !  http://en.wikipedia.org/wiki/Lagrange_multiplier

       nvar = 2
       neqns = 1
       nineqns = 0

       !  N.B. results can flip to minimum instead of maximum
       !  if x(1), x(2) are initialised at different points...
       x(1) = 1.0D0 ; x(2) = 1.0D0

       !  No bounds on x values set
       ilower(1) = 0 ; ilower(2) = 0
       iupper(1) = 0 ; iupper(2) = 0
       bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

       x_exp(1) = 0.5D0*sqrt(2.0D0)
       x_exp(2) = 0.5D0*sqrt(2.0D0)
       objf_exp = sqrt(2.0D0)
       c_exp(1) = 0.0D0
       vlam_exp(1) = 1.0D0/sqrt(2.0D0)
       errlg_exp = 0.0D0
       errlm_exp = 0.0D0
       errcom_exp = 0.0D0
       errcon_exp = 0.0D0
       ifail_exp = 1

    case (5)

       !  Intersection of parabola x^2 with straight line 2x+3
       !  Unorthodox (and not recommended) method to find the root
       !  of an equation.
       !
       !  Maximise f(x1) = x1**2
       !  subject to the following constraint:
       !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0
       !
       !  Solutions to c1(x1) are x1 = -1 and x1 = 3, and depending on
       !  the initial guess for x1 either (or neither...) solution might
       !  be found. Since there is one constraint equation with one unknown
       !  the code cannot optimise properly.

       nvar = 1
       neqns = 1
       nineqns = 0

       x(1) = 5.0D0  !  Try different values, e.g. 5.0, 2.0, 1.0, 0.0...

       !  No bounds on x values set
       ilower(1) = 0
       iupper(1) = 0
       bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

       x_exp(1) = 3.0
       objf_exp = 9.0D0
       c_exp(1) = 0.0D0
       vlam_exp(1) = 1.5D0
       errlg_exp = 0.0D0
       errlm_exp = 0.0D0
       errcom_exp = 0.0D0
       errcon_exp = 0.0D0
       ifail_exp = 1

    end select

  end subroutine inittest

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine objfn(n,m,x,objf,conf,ifail)

    implicit none

    !  Arguments

    integer, intent(in) :: n,m
    real(kind(1.0D0)), dimension(n), intent(in) :: x
    real(kind(1.0D0)), intent(out) :: objf
    real(kind(1.0D0)), dimension(m), intent(out) :: conf
    integer, intent(inout) :: ifail

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    select case (itest)

    case (1,2)

       !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
       !  subject to the following constraints:
       !  c1(x1,x2) = x1 - 2*x2 + 1 = 0  (itest = 1)
       !  c1(x1,x2) = x1 - 2*x2 + 1 >= 0 (itest = 2)
       !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

       objf = (x(1) - 2.0D0)**2 + (x(2) - 1.0D0)**2

       conf(1) = x(1) - 2.0D0*x(2) + 1.0D0
       conf(2) = -0.25D0*x(1)**2 - x(2)*x(2) + 1.0D0

    case (3)

       !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
       !  subject to the following constraints:
       !  c1(x1,x2) = x1 + x2 - 3 = 0
       !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

       objf = (x(1) - 2.0D0)**2 + (x(2) - 1.0D0)**2

       conf(1) = x(1) + x(2) - 3.0D0
       conf(2) = -0.25D0*x(1)**2 - x(2)*x(2) + 1.0D0

    case (4)

       !  From Wikipedia: Lagrange Multiplier article
       !  Maximise f(x1,x2) = x1 + x2
       !  subject to the following constraint:
       !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0

       objf = x(1) + x(2)

       conf(1) = x(1)*x(1) + x(2)*x(2) - 1.0D0

    case (5)

       !  Intersection of parabola x^2 with straight line 2x+3
       !  Maximise f(x1) = x1**2
       !  subject to the following constraint:
       !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0

       objf = x(1)*x(1)

       conf(1) = x(1)*x(1) - 2.0D0*x(1) - 3.0D0

    end select

    nfun = nfun + 1

  end subroutine objfn

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dobjfn(n,m,x,fgrd,cnorm,lcnorm,ifail)

    implicit none

    !  Arguments

    integer, intent(in) :: n,m,lcnorm
    real(kind(1.0D0)), dimension(n), intent(in) :: x
    real(kind(1.0D0)), dimension(n), intent(out) :: fgrd
    real(kind(1.0D0)), dimension(lcnorm,m), intent(out) :: cnorm
    integer, intent(inout) :: ifail

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  fgrd(1...n) are the gradients of the objective function f
    !  with respect to x1...xn

    !  cnorm(1...n, 1...m) are the gradients of the constraints 1...m
    !  with respect to x1...xn

    select case (itest)

    case (1,2)

       !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
       !  subject to the following constraints:
       !  c1(x1,x2) = x1 - 2*x2 + 1 = 0  (itest = 1)
       !  c1(x1,x2) = x1 - 2*x2 + 1 >= 0 (itest = 2)
       !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

       fgrd(1) = 2.0D0*(x(1) - 2.0D0)
       fgrd(2) = 2.0D0*(x(2) - 1.0D0)

       cnorm(1,1) = 1.0D0
       cnorm(2,1) = -2.0D0
       cnorm(1,2) = -0.5D0*x(1)
       cnorm(2,2) = -2.0D0*x(2)

    case (3)

       !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
       !  subject to the following constraints:
       !  c1(x1,x2) = x1 + x2 - 3 = 0
       !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

       fgrd(1) = 2.0D0
       fgrd(2) = 2.0D0*(x(2) - 1.0D0)

       cnorm(1,1) = 1.0D0
       cnorm(2,1) = 1.0D0
       cnorm(1,2) = -0.5D0*x(1)
       cnorm(2,2) = -2.0D0*x(2)

    case (4)

       !  From Wikipedia: Lagrange Multiplier article
       !  Maximise f(x1,x2) = x1 + x2
       !  subject to the following constraint:
       !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0

       fgrd(1) = 1.0D0
       fgrd(2) = 1.0D0

       cnorm(1,1) = 2.0D0*x(1)
       cnorm(2,1) = 2.0D0*x(2)

    case (5)

       !  Intersection of parabola x^2 with straight line 2x+3
       !  Maximise f(x1) = x1**2
       !  subject to the following constraint:
       !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0

       fgrd(1) = 2.0D0*x(1)

       cnorm(1,1) = 2.0D0*x(1) - 2.0D0

    end select

  end subroutine dobjfn

end module testdata

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program test

  !  Unit testing program for VMCON

  use maths_library
  use numerics
  use testdata

  implicit none

  integer :: ifail = 1
  real(kind(1.0D0)) :: objf

  integer :: ii,jj,lb,lcnorm,ldel,lh,liwa,lwa,m,meq,mode,n
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

  real(kind(1.0D0)) :: summ,errlg,errlm,errcom,errcon

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Change the test being run by modifying the value of itest
  !  (defaults to 1)

  itest = 1

  call inittest(nvar,neqns,nineqns,xv,ilower,iupper,bndl,bndu)

  epsvmc = 1.0D-8

  n = nvar
  m = neqns+nineqns
  meq = neqns
  xtol = epsvmc
  mode = 0
  lb = ippn1
  lcnorm = ippn1
  ldel = ipldel
  lh = iplh
  lwa = iplh
  liwa = ipliwa

  write(*,*) 'Initial solution estimate:'
  do ii = 1,n
     write(*,*) 'x(',ii,') = ',xv(ii)
  end do
  write(*,*)

  call vmcon(objfn,dobjfn,mode,n,m,meq,xv,objf,fgrd,conf,cnorm, &
       lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
       gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
       liwa,ilower,iupper,bndl,bndu)

  write(*,*) 'ifail = ', ifail, '(expected value = ',ifail_exp,')'
  write(*,*) 'Number of function evaluations = ',nfun
  write(*,*)

  write(*,*) 'Final solution estimate: calculated vs expected'
  do ii = 1,n
     write(*,*) 'x(',ii,') = ',xv(ii),x_exp(ii)
  end do
  write(*,*)

  write(*,*) 'Final objective function value: calculated vs expected'
  write(*,*) 'f(x) = ',objf,objf_exp
  write(*,*)

  write(*,*) 'Constraints evaluated at x: calculated vs expected'
  do ii = 1,m
     write(*,*) conf(ii), c_exp(ii)
  end do
  write(*,*)

  write(*,*) 'Lagrange multiplier estimates: calculated vs expected'
  do ii = 1,m
     write(*,*) vlam(ii), vlam_exp(ii)
  end do
  write(*,*)

  write(*,*) 'Lagrangian gradient error: calculated vs expected'
  errlg = 0.0D0
  do ii = 1,n
     summ = fgrd(ii)
     do jj = 1,m
        summ = summ - vlam(jj)*cnorm(ii,jj)
     end do
     errlg = errlg + abs(summ)
  end do
  write(*,*) errlg, errlg_exp
  write(*,*)

  write(*,*) 'Lagrange multiplier error: calculated vs expected'
  errlm = 0.0D0
  do ii = 1,m
     if ((ii <= meq).or.(vlam(ii) >= 0.0D0)) cycle
     errlm = errlm + abs(vlam(ii))
  end do
  write(*,*) errlm, errlm_exp
  write(*,*)

  write(*,*) 'Complementarity error: calculated vs expected'
  errcom = 0.0D0
  do ii = 1,m
     errcom = errcom + abs(vlam(ii)*conf(ii))
  end do
  write(*,*) errcom, errcom_exp
  write(*,*)

  write(*,*) 'Constraint error: calculated vs expected'
  errcon = 0.0D0
  do ii = 1,m
     if ((ii > meq).and.(conf(ii) >= 0.0D0)) cycle
     errcon = errcon + abs(conf(ii))
  end do
  write(*,*) errcon, errcon_exp
  write(*,*)

end program test

#endif
