!  $Id::                                                                $
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

470     continue

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
