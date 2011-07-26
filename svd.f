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
C  Module         : $Id: svd.f,v 3.2 2006/05/25 09:27:02 pknight Exp $
C
C  Module name    : $RCSfile: svd.f,v $
C  Version no.    : $Revision: 3.2 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE SVD(nm,m,n,a,w,matu,u,matv,v,ierr,rv1)

c  This subroutine is a translation of the algol procedure SVD,
c  Num. Math. 14, 403-420(1970) by Golub and Reinsch.
c  Handbook for Auto. Comp., vol II - Linear Algebra, 134-151(1971).
c
c  This subroutine determines the singular value decomposition
c       t
c  a=usv  of a real m by n rectangular matrix.  Householder
c  bidiagonalization and a variant of the qr algorithm are used.
c
c  On input:
c
c  nm must be set to the row dimension of two-dimensional
c  array parameters as declared in the calling program
c  dimension statement.  note that nm must be at least
c  as large as the maximum of m and n.
c
c  m is the number of rows of a (and u).
c
c  n is the number of columns of a (and u) and the order of v.
c
c  a contains the rectangular input matrix to be decomposed.
c
c  matu should be set to .true. if the u matrix in the
c  decomposition is desired, and to .false. otherwise.
c
c  matv should be set to .true. if the v matrix in the
c  decomposition is desired, and to .false. otherwise.
c
c  On output:
c
c  a is unaltered (unless overwritten by u or v).
c
c  w contains the n (non-negative) singular values of a (the
c  diagonal elements of s).  they are unordered.  if an
c  error exit is made, the singular values should be correct
c  for indices ierr+1,ierr+2,...,n.
c
c  u contains the matrix u (orthogonal column vectors) of the
c  decomposition if matu has been set to .true.  otherwise
c  u is used as a temporary array.  u may coincide with a.
c  if an error exit is made, the columns of u corresponding
c  to indices of correct singular values should be correct.
c
c  v contains the matrix v (orthogonal) of the decomposition if
c  matv has been set to .true.  otherwise v is not referenced.
c  v may also coincide with a if u is not needed.  if an error
c  exit is made, the columns of v corresponding to indices of
c  correct singular values should be correct.
c
c  ierr is set to
c  zero       for normal return,
c  k          if the k-th singular value has not been
c             determined after 30 iterations.
c
c  rv1 is a temporary storage array.
c
c  questions and comments should be directed to b. s. garbow,
c  applied mathematics division, argonne national laboratory

      IMPLICIT NONE
      
      INTEGER i,j,k,l,m,n,ii,i1,kk,k1,ll,l1,mn,nm,its,ierr
      
      DOUBLE PRECISION a(nm,n),w(n),u(nm,n),v(nm,n),rv1(n)
      DOUBLE PRECISION c,f,g,h,s,x,y,z,scale,anorm
      
      LOGICAL matu,matv
      
      ierr = 0
      
      do 20 i = 1, m
         do 10 j = 1, n
            u(i,j) = a(i,j)
 10      continue
 20   continue
      
c  Householder reduction to bidiagonal form
      g = 0.0d0
      scale = 0.0d0
      anorm = 0.0d0
      
      do 200 i = 1, n
         l = i + 1
         rv1(i) = scale * g
         g = 0.0d0
         s = 0.0d0
         scale = 0.0d0
         if (i .gt. m) goto 100
         
         do 30 k = i, m
            scale = scale + abs(u(k,i))
 30      continue
         
         if (scale .eq. 0.0d0) goto 100
         
         do 40 k = i, m
            u(k,i) = u(k,i) / scale
            s = s + u(k,i)**2
 40      continue
         
         f = u(i,i)
         g = -sign(sqrt(s),f)
         h = f * g - s
         u(i,i) = f - g
         if (i .eq. n) goto 80
         
         do 70 j = l, n
            s = 0.0d0
            
            do 50 k = i, m
               s = s + u(k,i) * u(k,j)
 50        continue
            
            f = s / h
            
            do 60 k = i, m
               u(k,j) = u(k,j) + f * u(k,i)
 60         continue   
 70      continue
         
 80      continue
         do 90 k = i, m
            u(k,i) = scale * u(k,i)
 90      continue
         
 100     continue
         w(i) = scale * g
         g = 0.0d0
         s = 0.0d0
         scale = 0.0d0
         if ((i .gt. m) .or. (i .eq. n)) goto 190
         
         do 110 k = l, n
            scale = scale + abs(u(i,k))
 110     continue
         
         if (scale .eq. 0.0d0) goto 190
         
         do 120 k = l, n
            u(i,k) = u(i,k) / scale
            s = s + u(i,k)**2
 120     continue
         
         f = u(i,l)
         g = -sign(sqrt(s),f)
         h = f * g - s
         u(i,l) = f - g
         
         do 130 k = l, n
            rv1(k) = u(i,k) / h
 130     continue
         
         if (i .eq. m) goto 170
         
         do 160 j = l, m
            s = 0.0d0
            
            do 140 k = l, n
               s = s + u(j,k) * u(i,k)
 140        continue
            
            do 150 k = l, n
               u(j,k) = u(j,k) + s * rv1(k)
 150        continue   
 160     continue
         
 170     continue
         do 180 k = l, n
            u(i,k) = scale * u(i,k)
 180     continue
         
 190     continue
         anorm = max(anorm,abs(w(i))+abs(rv1(i)))
 200  continue
      
c  Accumulation of right-hand transformations
      if (.not. matv) goto 290
      
c  For i=n step -1 until 1 do
      do 280 ii = 1, n
         i = n + 1 - ii
         if (i .eq. n) goto 270
         if (g .eq. 0.0D0) goto 250
         
         do 210 j = l, n
            
c  Double division avoids possible underflow
            v(j,i) = (u(i,j) / u(i,l)) / g
 210     continue
         
         do 240 j = l, n
            s = 0.0d0
            
            do 220 k = l, n
               s = s + u(i,k) * v(k,j)
 220        continue
            
            do 230 k = l, n
               v(k,j) = v(k,j) + s * v(k,i)
 230        continue   
 240     continue
         
 250     continue
         do 260 j = l, n
            v(i,j) = 0.0d0
            v(j,i) = 0.0d0
 260     continue
         
 270     continue
         v(i,i) = 1.0d0
         g = rv1(i)
         l = i
 280  continue
      
c  Accumulation of left-hand transformations
 290  continue
      if (.not. matu) goto 410
      
c  For i=min(m,n) step -1 until 1 do
      mn = n
      if (m .lt. n) mn = m
      
      do 400 ii = 1, mn
         i = mn + 1 - ii
         l = i + 1
         g = w(i)
         if (i .eq. n) goto 310
         
         do 300 j = l, n
            u(i,j) = 0.0d0
 300     continue
         
 310     continue
         if (g .eq. 0.0d0) goto 370
         if (i .eq. mn) goto 350
         
         do 340 j = l, n
            s = 0.0d0
            
            do 320 k = l, m
               s = s + u(k,i) * u(k,j)
 320        continue
            
c  Double division avoids possible underflow
            f = (s / u(i,i)) / g
            
            do 330 k = i, m
               u(k,j) = u(k,j) + f * u(k,i)
 330        continue
 340     continue   
         
 350     continue
         do 360 j = i, m
            u(j,i) = u(j,i) / g
 360     continue
         
         goto 390
         
 370     continue
         do 380 j = i, m
            u(j,i) = 0.0d0
 380     continue
         
 390     continue
         u(i,i) = u(i,i) + 1.d0
 400  continue
      
c  Diagonalization of the bidiagonal form
c  For k=n step -1 until 1 do
 410  continue
      do 550 kk = 1, n
         k1 = n - kk
         k = k1 + 1
         its = 0
         
c  Test for splitting.
c  For l=k step -1 until 1 do
 420     continue
         do 430 ll = 1, k
            l1 = k - ll
            l = l1 + 1
            if ((abs(rv1(l)) + anorm) .eq. anorm) goto 470
            
c  rv1(1) is always zero, so there is no exit
c  through the bottom of the loop

C+**PJK 23/05/06 Prevent problems from the code getting here with l1=0
            if (l1.eq.0) then
               write(*,*) 'SVD: Shouldn''t get here...'
               goto 470
            end if

            if ((abs(w(l1)) + anorm) .eq. anorm) goto 440
 430     continue
         
c  Cancellation of rv1(l) if l greater than 1
 440     continue
         c = 0.0d0
         s = 1.0d0
         
         do 460 i = l, k
            f = s * rv1(i)
            rv1(i) = c * rv1(i)
            if ((abs(f) + anorm) .eq. anorm) goto 470
            g = w(i)
            h = sqrt(f*f+g*g)
            w(i) = h
            c = g / h
            s = -f / h
            if (.not. matu) goto 460
            
            do 450 j = 1, m
               y = u(j,l1)
               z = u(j,i)
               u(j,l1) = y * c + z * s
               u(j,i) = -y * s + z * c
 450        continue
            
 460     continue
         
c  Test for convergence
 470     continue
         z = w(k)
         if (l .eq. k) goto 530
         
c  Shift from bottom 2 by 2 minor
         if (its .eq. 30) goto 560
         its = its + 1
         x = w(l)
         y = w(k1)
         g = rv1(k1)
         h = rv1(k)
         f = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.d0 * h * y)
         g = sqrt(f*f+1.d0)
         f = ((x - z) * (x + z) + h * (y / (f + sign(g,f)) - h)) / x
         
c  Next qr transformation
         c = 1.d0
         s = 1.d0
         
         do 520 i1 = l, k1
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
            if (.not. matv) goto 490
            
            do 480 j = 1, n
               x = v(j,i1)
               z = v(j,i)
               v(j,i1) = x * c + z * s
               v(j,i) = -x * s + z * c
 480        continue
            
 490        continue
            z = sqrt(f*f+h*h)
            w(i1) = z
            
c  Rotation can be arbitrary if z is zero
            if (z .eq. 0.0d0) goto 500
            c = f / z
            s = h / z
 500        continue
            f = c * g + s * y
            x = -s * g + c * y
            if (.not. matu) goto 520
            
            do 510 j = 1, m
               y = u(j,i1)
               z = u(j,i)
               u(j,i1) = y * c + z * s
               u(j,i) = -y * s + z * c
 510        continue
            
 520     continue
         
         rv1(l) = 0.0d0
         rv1(k) = f
         w(k) = x
         goto 420
         
c  Convergence
 530     continue
         if (z .ge. 0.0d0) goto 550
         
c  w(k) is made non-negative
         w(k) = -z
         if (.not. matv) goto 550
         
         do 540 j = 1, n
            v(j,k) = -v(j,k)
 540     continue
         
 550  continue
      
      goto 1000
      
c  Set error - no convergence to a
c  singular value after 30 iterations
      
 560  continue
      ierr = k
      
 1000 continue
      
      return
      end
