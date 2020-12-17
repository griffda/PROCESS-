! Returns the inverse of a matrix calculated by finding the LU
! decomposition.  Depends on LAPACK.
	subroutine migs(A,n,Ainv)
!function inv(A) result(Ainv)
  real(kind(1.0d0)), dimension(n,n), intent(in) :: A
  real(kind(1.0d0)), dimension(n,n) :: Ainv

  real(kind(1.0d0)), dimension(size(A,1)) :: work  ! work array for LAPACK
  integer, dimension(size(A,1)) :: ipiv   ! pivot indices
  integer :: n, info

  ! External procedures defined in LAPACK
  external DGETRF
  external DGETRI

  ! Store A in Ainv to prevent it from being overwritten by LAPACK
  Ainv = A
  ! DGETRF computes an LU factorization of a general M-by-N matrix A
  ! using partial pivoting with row interchanges.
  call DGETRF(n, n, Ainv, n, ipiv, info)

  if (info /= 0) then
   write(*,*) 'Matrix is numerically singular!'
   stop 1
  end if

  ! DGETRI computes the inverse of a matrix using the LU factorization
  ! computed by DGETRF.
  call DGETRI(n, Ainv, n, ipiv, work, n, info)

  if (info /= 0) then
   write(*,*) 'Matrix inversion failed!'
   stop 1
  end if
end 
