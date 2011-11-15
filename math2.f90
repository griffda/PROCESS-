!  $Id::                                                                $
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

  !  External functions

  real(kind(1.0D0)), external :: sdot

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

  !  External variables

  real(kind(1.0D0)), external :: sdot

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

  !  External functions

  integer, external :: isamax

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
        end if
     end do

  end if

  ipvt(n) = n
  if (a(n,n) == 0.0D0) info = n

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
