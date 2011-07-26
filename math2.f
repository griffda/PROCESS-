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
C  Module         : $Id: math2.f,v 3.1 1993/06/11 14:28:03 peter Exp $
C
C  Module name    : $RCSfile: math2.f,v $
C  Version no.    : $Revision: 3.1 $
C
C  Creation date  : $Date: 1993/06/11 14:28:03 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE LINESOLV(a, ndim, b, ipvt, x)

c  Routine to solve linear equation system : a . x = b
c  Calls linpack routines sgefa and sgesl.
c
c  INPUT :
c  a(ndim,ndim) - array a
c  ndim         - dimension of a 
c  b            - RHS vector 
c  ipvt         - integer array of length ndim (work space)
c
c  OUTPUT :
c  x            - solution for A . X = C

      IMPLICIT NONE

      INTEGER i, job, ndim, ndim1, info
      INTEGER ipvt(ndim)

      DOUBLE PRECISION  a(ndim,ndim), b(ndim), x(ndim)

      job = 0
      ndim1 = ndim

      call sgefa(a, ndim, ndim1, ipvt, info)

      call sgesl(a, ndim, ndim1, ipvt, b, job)

      do 10 i = 1,ndim
         x(i) = b(i)
 10   continue

      return
      end
c______________________________________________________________________
      SUBROUTINE SGESL(a,lda,n,ipvt,b,job)

c  sgesl solves the real system
c  a * x = b  or  trans(a) * x = b
c  using the factors computed by sgeco or sgefa.
c
c  On entry :
c
c  a     real(lda, n)
c        the output from sgeco or sgefa.
c
c  lda   integer
c        the leading dimension of the array  a .
c
c  n     integer
c        the order of the matrix  a .
c
c  ipvt  integer(n)
c        the pivot vector from sgeco or sgefa.
c
c  b     real(n)
c        the right hand side vector.
c
c  job   integer
c        = 0         to solve  a*x = b ,
c        = nonzero   to solve  trans(a)*x = b  where
c                    trans(a)  is the transpose.
c
c  On return :
c
c  b     the solution vector  x .
c
c  Error condition
c
c  A division by zero will occur if the input factor contains a
c  zero on the diagonal.  Technically this indicates singularity
c  but it is often caused by improper arguments or improper
c  setting of lda .  It will not occur if the subroutines are
c  called correctly and if sgeco has set rcond .gt. 0.0
c  or sgefa has set info .eq. 0 .
c
c  To compute  inverse(a) * c  where  c  is a matrix
c  with  p  columns,
c  call sgeco(a,lda,n,ipvt,rcond,z)
c  if (rcond is too small) goto ...
c  do 10 j = 1, p
c     call sgesl(a,lda,n,ipvt,c(1,j),0)
c10continue
c
c  Linpack. this version dated 08/14/78 .
c  Cleve Moler, University of New Mexico, Argonne National Lab.

      IMPLICIT NONE

      INTEGER lda,n,ipvt(*),job
      INTEGER k,kb,l,nm1

      DOUBLE PRECISION a(lda,*),b(*),t

      DOUBLE PRECISION sdot
      EXTERNAL         sdot

      nm1 = n - 1
      if (job .ne. 0) goto 50

c  job = 0 , solve  a * x = b
c  First solve  l*y = b

      if (nm1 .lt. 1) goto 30
      do 20 k = 1, nm1
         l = ipvt(k)
         t = b(l)
         if (l .eq. k) goto 10
         b(l) = b(k)
         b(k) = t
 10      continue

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Arg 3 in call to SAXPY has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

         call saxpy(n-k,t,a(k+1,k),1,b(k+1),1)
 20   continue

 30   continue

c  Now solve  u*x = y

      do 40 kb = 1, n
         k = n + 1 - kb
         b(k) = b(k)/a(k,k)
         t = -b(k)

c+**PJK 02/11/92 Warning produced by QA Fortran :
c+**PJK 02/11/92 Arg 3 in call to SAXPY has wrong dimensions.
c+**PJK 02/11/92 Code works at present, but beware of future
c+**PJK 02/11/92 modifications.

         call saxpy(k-1,t,a(1,k),1,b(1),1)
 40   continue
      goto 100

 50   continue

c  job = nonzero, solve  trans(a) * x = b
c  First solve  trans(u)*y = b

      do 60 k = 1, n
         t = sdot(k-1,a(1,k),1,b(1),1)
         b(k) = (b(k) - t)/a(k,k)
 60   continue

c  Now solve trans(l)*x = y

      if (nm1 .lt. 1) goto 90
      do 80 kb = 1, nm1
         k = n - kb
         b(k) = b(k) + sdot(n-k,a(k+1,k),1,b(k+1),1)
         l = ipvt(k)
         if (l .eq. k) goto 70
         t = b(l)
         b(l) = b(k)
         b(k) = t
 70      continue
 80   continue

 90   continue

 100  continue

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION SASUM(n,sx,incx)

c  Takes the sum of the absolute values.
c  Uses unrolled loops for increment equal to one.
c  Jack Dongarra, linpack, 3/11/78.

      IMPLICIT NONE

      DOUBLE PRECISION sx(*),stemp
      integer i,incx,m,mp1,n,nincx

      sasum = 0.0d0
      stemp = 0.0d0
      if (n.le.0) goto 1000
      if (incx.eq.1) goto 20

c  Code for increment not equal to 1

      nincx = n*incx
      do 10 i = 1,nincx,incx
         stemp = stemp + abs(sx(i))
 10   continue
      sasum = stemp
      goto 1000

c  Code for increment equal to 1
c  Clean-up loop

 20   continue
      m = mod(n,6)
      if (m.eq.0) goto 40
      do 30 i = 1,m
         stemp = stemp + abs(sx(i))
 30   continue
      if (n.lt.6) goto 60

 40   continue
      mp1 = m + 1
      do 50 i = mp1,n,6
         stemp = stemp + abs(sx(i)) + abs(sx(i + 1)) + abs(sx(i + 2))
     +        + abs(sx(i + 3)) + abs(sx(i + 4)) + abs(sx(i + 5))
 50   continue

 60   continue
      sasum = stemp

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE SCOPY(n,sx,incx,sy,incy)

c  Copies a vector, x, to a vector, y.
c  Uses unrolled loops for increments equal to 1.
c  Jack Dongarra, linpack, 3/11/78.

      IMPLICIT NONE

      DOUBLE PRECISION sx(*),sy(*)
      INTEGER i,incx,incy,ix,iy,m,mp1,n

      if (n.le.0) goto 1000
      if ((incx.eq.1).and.(incy.eq.1)) goto 20

c  Code for unequal increments or equal increments not equal to 1

      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
         sy(iy) = sx(ix)
         ix = ix + incx
         iy = iy + incy
 10   continue
      goto 1000

c  Code for both increments equal to 1
c  Clean-up loop

 20   continue
      m = mod(n,7)
      if (m.eq.0) goto 40
      do 30 i = 1,m
         sy(i) = sx(i)
 30   continue
      if (n.lt.7) goto 1000

 40   continue
      mp1 = m + 1
      do 50 i = mp1,n,7
         sy(i) = sx(i)
         sy(i + 1) = sx(i + 1)
         sy(i + 2) = sx(i + 2)
         sy(i + 3) = sx(i + 3)
         sy(i + 4) = sx(i + 4)
         sy(i + 5) = sx(i + 5)
         sy(i + 6) = sx(i + 6)
 50   continue

 1000 continue
      
      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION SMACH(job)

c  smach computes machine parameters of floating point
c  arithmetic for use in testing only.  Not required by
c  linpack proper.
c
c  If trouble with automatic computation of these quantities,
c  they can be set by direct assignment statements.
c  Assume the computer has
c
c  b = base of arithmetic
c  t = number of base  b  digits
c  l = smallest possible exponent
c  u = largest possible exponent
c
c  then
c
c  eps = b**(1-t)
c  tiny = 100.0*b**(-l+t)
c  huge = 0.01*b**(u-t)
c
c  job is 1, 2 or 3 for epsilon, tiny and huge, respectively.

      IMPLICIT NONE

      INTEGER job
      DOUBLE PRECISION eps,tiny,huge,s

      eps = 1.0D0
 10   continue
      eps = eps/2.0D0
      s = 1.0D0 + eps
      if (s.gt.1.0D0) goto 10
      eps = 2.0D0 * eps

      s = 1.0D0
 20   continue
      tiny = s
      s = s/16.0D0
      if ((s*100.0D0).ne.0.0D0) goto 20
      tiny = (tiny/eps) * 100.0D0
      huge = 1.0D0/tiny

      if (job.eq.1) smach = eps
      if (job.eq.2) smach = tiny
      if (job.eq.3) smach = huge

      return
      end
c______________________________________________________________________
      SUBROUTINE SROT(n,sx,incx,sy,incy,c,s)

c  Applies a plane rotation.
c  Jack Dongarra, linpack, 3/11/78.

      IMPLICIT NONE

      DOUBLE PRECISION sx(*),sy(*),stemp,c,s

      INTEGER i,incx,incy,ix,iy,n

      if (n.le.0) goto 1000
      if ((incx.eq.1).and.(incy.eq.1)) goto 20

c  Code for unequal increments or equal increments not equal to 1

      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do 10 i = 1,n
         stemp = c*sx(ix) + s*sy(iy)
         sy(iy) = c*sy(iy) - s*sx(ix)
         sx(ix) = stemp
         ix = ix + incx
         iy = iy + incy
 10   continue
      goto 1000

c  Code for both increments equal to 1

 20   continue
      do 30 i = 1,n
         stemp = c*sx(i) + s*sy(i)
         sy(i) = c*sy(i) - s*sx(i)
         sx(i) = stemp
 30   continue

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE SROTG(sa,sb,c,s)

c  Construct Givens plane rotation.
c  Jack Dongarra, linpack, 3/11/78.

      IMPLICIT NONE

      DOUBLE PRECISION sa,sb,c,s,roe,scale,r,z

      roe = sb
      if (abs(sa).gt.abs(sb)) roe = sa
      scale = abs(sa) + abs(sb)
      if (scale .ne. 0.0D0) goto 10
      c = 1.0D0
      s = 0.0D0
      r = 0.0D0
      goto 20
 10   continue
      r = scale*sqrt((sa/scale)**2 + (sb/scale)**2)
      r = sign(1.0D0,roe)*r
      c = sa/r
      s = sb/r
 20   continue
      z = 1.0D0
      if (abs(sa).gt.abs(sb)) z = s
      if ((abs(sb).ge.abs(sa)).and.(c.ne.0.0D0)) z = 1.0D0/c
      sa = r
      sb = z

      return
      end
C______________________________________________________________________
      SUBROUTINE SROTM (N,SX,INCX,SY,INCY,SPARAM)

c  Apply the Modified Givens Transformation, h, to the 2 by n matrix
c
c  (sx**t) , where **t indicates transpose. The elements of sx are in
c  (dx**t)
c
c  sx(lx+i*incx), i = 0 to n-1, where lx = 1 if incx .ge. 0, else
c  lx = (-incx)*n, and similarly for sy using using ly and incy.
c  with sparam(1)=sflag, h has one of the following forms..
c
c  sflag=-1.e0     sflag=0.e0        sflag=1.e0     sflag=-2.e0
c
c    (sh11  sh12)    (1.e0  sh12)    (sh11  1.e0)    (1.e0  0.e0)
c  h=(          )    (          )    (          )    (          )
c    (sh21  sh22),   (sh21  1.e0),   (-1.e0 sh22),   (0.e0  1.e0).
c  see  srotmg for a description of data storage in sparam.

      IMPLICIT NONE

      DOUBLE PRECISION sx(*),sy(*),sparam(5)
      DOUBLE PRECISION sflag,sh11,sh12,sh21,sh22,w,z
      DOUBLE PRECISION two,zero

      INTEGER n,incx,incy
      INTEGER i,kx,ky,nsteps

      zero = 0.0D0
      two = 2.0D0

      sflag = sparam(1)
      if ((n.le.0).or.((sflag+two).eq.zero)) goto 140
      if(.not.((incx.eq.incy).and.(incx.gt.0))) goto 70

      nsteps=n*incx

      if (sflag.lt.0.0D0) then
         goto 50
      else if (sflag.eq.0.0D0) then
         goto 10
      else
         goto 30
      end if

 10   continue
      sh12 = sparam(4)
      sh21 = sparam(3)
      do 20 i=1,nsteps,incx
         w = sx(i)
         z = sy(i)
         sx(i) = w+z*sh12
         sy(i) = w*sh21+z
 20   continue
      goto 140

 30   continue
      sh11 = sparam(2)
      sh22 = sparam(5)
      do 40 i = 1,nsteps,incx
         w = sx(i)
         z = sy(i)
         sx(i) =  w*sh11+z
         sy(i) = -w+sh22*z
 40   continue
      goto 140

 50   continue
      sh11 = sparam(2)
      sh12 = sparam(4)
      sh21 = sparam(3)
      sh22 = sparam(5)
      do 60 i = 1,nsteps,incx
         w = sx(i)
         z = sy(i)
         sx(i) = w*sh11+z*sh12
         sy(i) = w*sh21+z*sh22
 60   continue
      goto 140
 70   continue
      kx = 1
      ky = 1
      if (incx.lt.0) kx = 1+(1-n)*incx
      if (incy.lt.0) ky = 1+(1-n)*incy

      if (sflag.lt.0.0D0) then
         goto 120
      else if (sflag.eq.0.0D0) then
         goto 80
      else
         goto 100
      end if

 80   continue
      sh12 = sparam(4)
      sh21 = sparam(3)
      do 90 i = 1,n
         w = sx(kx)
         z = sy(ky)
         sx(kx) = w+z*sh12
         sy(ky) = w*sh21+z
         kx = kx+incx
         ky = ky+incy
 90   continue
      goto 140

 100  continue
      sh11 = sparam(2)
      sh22 = sparam(5)
      do 110 i = 1,n
         w = sx(kx)
         z = sy(ky)
         sx(kx) = w*sh11+z
         sy(ky) = -w+sh22*z
         kx = kx+incx
         ky = ky+incy
 110  continue
      goto 140

 120  continue
      sh11 = sparam(2)
      sh12 = sparam(4)
      sh21 = sparam(3)
      sh22 = sparam(5)
      do 130 i = 1,n
         w = sx(kx)
         z = sy(ky)
         sx(kx) = w*sh11+z*sh12
         sy(ky) = w*sh21+z*sh22
         kx = kx+incx
         ky = ky+incy
 130  continue

 140  continue

      return
      end
C______________________________________________________________________
      SUBROUTINE SROTMG(sd1,sd2,sx1,sy1,sparam)

c  Construct the Modified Givens Transformation matrix h which zeros
c  the second component of the 2-vector  (sqrt(sd1)*sx1,sqrt(sd2)*
c  sy2)**t.
c  With sparam(1)=sflag, h has one of the following forms..
c
c  sflag=-1.e0     sflag=0.e0        sflag=1.e0     sflag=-2.e0
c
c    (sh11  sh12)    (1.e0  sh12)    (sh11  1.e0)    (1.e0  0.e0)
c  h=(          )    (          )    (          )    (          )
c    (sh21  sh22),   (sh21  1.e0),   (-1.e0 sh22),   (0.e0  1.e0).
c  Locations 2-4 of sparam contain sh11,sh21,sh12, and sh22
c  respectively. (Values of 1.e0, -1.e0, or 0.e0 implied by the
c  value of sparam(1) are not stored in sparam.)
c
c  The values of gamsq and rgamsq set in the data statement may be
c  inexact.  This is OK as they are only used for testing the size
c  of sd1 and sd2.  All actual scaling of data is done using gam.

      IMPLICIT NONE

      DOUBLE PRECISION sd1,sd2,sx1,sy1,sparam(5)

      DOUBLE PRECISION sh11,sh12,sh21,sh22,sflag
      DOUBLE PRECISION sp1,sp2,sq1,sq2,stemp,su
      DOUBLE PRECISION zero,one,two
      DOUBLE PRECISION gam,gamsq,rgamsq

      INTEGER igo

      zero = 0.0D0
      one = 1.0D0
      two = 2.0D0

      gam = 4096.0D0
      gamsq = 1.67772D7
      rgamsq = 5.96046D-8

      if (.not.(sd1.lt.zero)) goto 10

c  go zero-h-d-and-sx1..
      goto 60

 10   continue
c  case-sd1-nonnegative
      sp2 = sd2*sy1
      if (.not.(sp2.eq.zero)) goto 20
      sflag =-two
      goto 260

c  regular-case..
 20   continue
      sp1 = sd1*sx1
      sq2 = sp2*sy1
      sq1 = sp1*sx1

      if (.not.(abs(sq1).gt.abs(sq2))) goto 40
      sh21 = -sy1/sx1
      sh12 = sp2/sp1

      su = one-sh12*sh21

      if (.not.(su.le.zero)) goto 30
c  go zero-h-d-and-sx1..
      goto 60

 30   continue
      sflag = zero
      sd1 = sd1/su
      sd2 = sd2/su
      sx1 = sx1*su

c  go scale-check..
      goto 100

 40   continue
      if (.not.(sq2.lt.zero)) goto 50

c  go zero-h-d-and-sx1..
      goto 60

 50   continue
      sflag = one
      sh11 = sp1/sp2
      sh22 = sx1/sy1
      su = one+sh11*sh22
      stemp = sd2/su
      sd2 = sd1/su
      sd1 = stemp
      sx1 = sy1*su

c  go scale-check
      goto 100

c  procedure..zero-h-d-and-sx1..
 60   continue
      sflag = -one
      sh11 = zero
      sh12 = zero
      sh21 = zero
      sh22 = zero

      sd1 = zero
      sd2 = zero
      sx1 = zero
c  return..
      goto 220

c  procedure..fix-h..
 70   continue
      if (.not.(sflag.ge.zero)) goto 90
      if (.not.(sflag.eq.zero)) goto 80

      sh11 = one
      sh22 = one
      sflag = -one
      goto 90

 80   continue
      sh21 = -one
      sh12 = one
      sflag = -one

 90   continue

C  go to igo,(120,150,180,210)
C  N.B. igo IS initialised before the code reaches this point
C  as this part is jumped over beforehand. Bad programming, though...

      if (igo.eq.120) goto 120
      if (igo.eq.150) goto 150
      if (igo.eq.180) goto 180
      if (igo.eq.210) goto 210

c  procedure..scale-check
 100  continue

 110  continue
      if (.not.(sd1.le.rgamsq)) goto 130
      if (sd1.eq.zero) goto 160
      igo = 120

c  fix-h..
      goto 70
 120  continue
      sd1 = sd1*gam**2
      sx1 = sx1/gam
      sh11 = sh11/gam
      sh12 = sh12/gam
      goto 110

 130  continue
 140  continue
      if (.not.(sd1.ge.gamsq)) goto 160
      igo = 150

c  fix-h..
      goto 70
 150  continue
      sd1 = sd1/gam**2
      sx1 = sx1*gam
      sh11 = sh11*gam
      sh12 = sh12*gam
      goto 140

 160  continue
 170  continue
      if (.not.(abs(sd2).le.rgamsq)) goto 190
      if (sd2.eq.zero) goto 220
      igo = 180

c  fix-h..
      goto 70
 180  continue
      sd2 = sd2*gam**2
      sh21 = sh21/gam
      sh22 = sh22/gam
      goto 170

 190  continue
 200  continue
      if (.not.(abs(sd2).ge.gamsq)) goto 220
      igo = 210

c  fix-h..
      goto 70
 210  continue
      sd2 = sd2/gam**2
      sh21 = sh21*gam
      sh22 = sh22*gam
      goto 200

 220  continue

      if (sflag.lt.0.0D0) then
         goto 250
      else if (sflag.eq.0.0D0) then
         goto 230
      else
         goto 240
      end if

 230  continue
      sparam(3) = sh21
      sparam(4) = sh12
      goto 260

 240  continue
      sparam(2) = sh11
      sparam(5) = sh22
      goto 260

 250  continue
      sparam(2) = sh11
      sparam(3) = sh21
      sparam(4) = sh12
      sparam(5) = sh22

 260  continue
      sparam(1) = sflag

      return
      end
