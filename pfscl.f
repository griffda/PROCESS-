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
C  Module name    : $RCSfile: pfscl.f,v $
C  Version no.    : $Revision: 3.1 $
C
C  Creation date  : $Date: 1993/06/11 14:28:13 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE EFC(ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1,
     +     npts,rpts,zpts,brin,bzin,nfix,rfix,zfix,cfix,
     +     ngrp,ncls,rcls,zcls,alfa,work1,bfix,gmat,bvec,
     +     rc,zc,cc,xc,umat,vmat,sigma,work2,ssq,ccls)

c  This routine calculates the currents required in a group
c  of ring coils to produce a fixed field at prescribed
c  locations. Additional ring coils with fixed currents are
c  also allowed.
c
c  Written by D. Strickler, ORNL.
c  Modified by J. Galambos to have all arrays sized in the calling
c  routine (in a generalized fashion). (2/25/92)
c  Included in PROCESS in March 1992 by P. C. Shipe.
c  Modified by P. Knight to ensure all arrays have correct dimensions
c  throughout (22/01/93)
c
c  INPUT :
c
c  ngrpmx          maximum number of PF coil groups
c  nclsmx          maximum number of coils in one group
c  nptsmx          maximum number of points across the plasma midplane
c                  at which the magnetic field is fixed
c  nfixmx          maximum number of fixed current coils
c  lrow1           integer row length of arrays bfix, bvec, gmat,
c                  umat,vmat. Should be >= (2*nptsmx + ngrpmx)
c  lcol1           integer column length of array gmat, umat, vmat
c                  should be >= ngrpmx
c  npts            number of data points at which field is to be fixed
c                  should be <= nptsmx
c  rpts(i),zpts(i) coordinates of data points (m)
c                  dimension = (nptsmx)
c  brin(i),bzin(i) field components at data points (T)
c                  dimension = (nptsmx)
c  nfix            number of coils with fixed currents, <= nfixmx
c  rfix(i),zfix(i) coordinates of coils with fixed
c                  currents (m), dimension (nfixmx)
c  cfix(i)         vector containing fixed currents (A),
c                  dimension (nfixmx)
c  ngrp            number of coil groups, where all coils in a
c                  group have the same current, <= ngrpmx
c  ncls(i)         integer vector containing the number of coils
c                  in each group, each value <= nclsmx
c  rcls(i,j),zcls(i,j) coordinates of coil j in group i (m),
c                  should be dimensioned (ngrpmx,nclsmx)
c  alfa            smoothing parameter (0 = no smoothing,
c                  1.0D-9 = large smoothing)
c  work1           work vector of length (nfixmx)
c  bfix            work vector of length (lrow1)
c  gmat            work array of size (lrow1,lcol1)
c  bvec            work vector of length (lrow1)
c  rc              work vector of length (nclsmx)
c  zc              work vector of length (nclsmx)
c  cc              work vector of length (nclsmx)
c  xc              work vector of length (nclsmx)
c  umat            work array of length(lrow1,lcol1)
c  vmat            work array of length(lrow1,lcol1)
c  sigma           work vector of length (ngrpmx)
c  work2           work vector of length (ngrpmx)
c
c  OUTPUT :
c
c  ssq             sum of squares of elements of residual vector
c  ccls(i)         solution vector of coil currents in group i (A)

      IMPLICIT NONE

      INTEGER ngrpmx,nclsmx,nptsmx,nfixmx,lrow1,lcol1

      INTEGER ncls(ngrpmx+2)

      DOUBLE PRECISION
     +     rcls(ngrpmx,nclsmx), zcls(ngrpmx,nclsmx)

      DOUBLE PRECISION
     +     ccls(ngrpmx),
     +     sigma(ngrpmx), work2(ngrpmx)

      DOUBLE PRECISION
     +     rc(nclsmx), zc(nclsmx), cc(nclsmx), xc(nclsmx)

      DOUBLE PRECISION
     +     brin(nptsmx), bzin(nptsmx), rpts(nptsmx), zpts(nptsmx)

      DOUBLE PRECISION
     +     rfix(nfixmx), zfix(nfixmx), cfix(nfixmx),
     +     work1(nfixmx)

      DOUBLE PRECISION
     +     bfix(lrow1), bvec(lrow1),
     +     gmat(lrow1,lcol1), umat(lrow1,lcol1), vmat(lrow1,lcol1)

      DOUBLE PRECISION
     +     alfa, ssq,
     +     brssq, brnrm, bzssq, bznrm

      INTEGER ngrp,npts,nfix,nrws

C  Calculate field from the fixed current loops
      call fixb(nptsmx,nfixmx,lrow1,npts,rpts,zpts,nfix,rfix,zfix,cfix,
     +     work1,bfix)

C  Set up matrix equation
      call mtrx(nptsmx,ngrpmx,nclsmx,lrow1,lcol1,npts,rpts,zpts,
     +     brin,bzin,ngrp,ncls,rcls,zcls,alfa,nrws,bfix,gmat,bvec,
     +     rc,zc,cc,xc)

C  Solve matrix equation
      call solv(ngrpmx,lrow1,lcol1,ngrp,ccls,nrws,gmat,bvec,umat,
     +     vmat,sigma,work2)

C  Calculate the norm of the residual vectors
      call rsid(nptsmx,ngrpmx,lrow1,lcol1,npts,brin,bzin,nfix,ngrp,
     +     ccls,brssq,brnrm,bzssq,bznrm,ssq,bfix,gmat)

      return
      end
c______________________________________________________________________
      SUBROUTINE SOLV(ngrpmx,lrow1,lcol1,ngrp,ccls,nrws,gmat,bvec,umat,
     +     vmat,sigma,work2)

C  Solve a matrix using singular value decomposition

      IMPLICIT NONE

      INTEGER ngrpmx,lrow1,lcol1

      DOUBLE PRECISION
     +     ccls(ngrpmx),
     +     sigma(ngrpmx), work2(ngrpmx)

      DOUBLE PRECISION
     +     bvec(lrow1),
     +     gmat(lrow1,lcol1), umat(lrow1,lcol1), vmat(lrow1,lcol1)

      INTEGER ngrp,nrws,i,j,ierr

      DOUBLE PRECISION zvec,eps
      LOGICAL truth

      truth = .true.

      eps = 1.0D-10
      call svd(lrow1,nrws,ngrp,gmat,sigma,truth,umat,truth,vmat,
     +     ierr,work2)

      do 20 i = 1,ngrp
         work2(i) = 0.0D0
         do 10 j = 1,nrws
            work2(i) = work2(i)+umat(j,i)*bvec(j)
 10      continue
 20   continue

c  Compute currents
      do 40 i = 1,ngrp
         ccls(i) = 0.0D0
         zvec = 0.0D0
         do 30 j = 1,ngrp
            if (sigma(j) .gt. eps) zvec = work2(j)/sigma(j)
            ccls(i) = ccls(i)+vmat(i,j)*zvec
 30      continue
 40   continue

      return
      end
c______________________________________________________________________
      SUBROUTINE RSID(nptsmx,ngrpmx,lrow1,lcol1,npts,brin,bzin,nfix,
     +     ngrp,ccls,brssq,brnrm,bzssq,bznrm,ssq,bfix,gmat)

c  Compute the norm of the residual vectors
c
c+**PJK 03/11/92 Removed redundant arguments rpts and zpts

      IMPLICIT NONE

      INTEGER nptsmx,ngrpmx,lrow1,lcol1

      DOUBLE PRECISION
     +     ccls(ngrpmx)

      DOUBLE PRECISION
     +     brin(nptsmx), bzin(nptsmx)

      DOUBLE PRECISION
     +     bfix(lrow1),
     +     gmat(lrow1,lcol1)

      INTEGER npts,nfix,ngrp,i,j

      DOUBLE PRECISION brssq,brnrm,bzssq,bznrm,ssq,svec,rvec

      brnrm = 0.0D0
      brssq = 0.0D0

      do 50 i = 1,npts
         svec = 0.0D0
         if (nfix.gt.0) svec = bfix(i)
         do 40 j = 1,ngrp
            svec = svec+gmat(i,j)*ccls(j)
 40      continue
         rvec = svec - brin(i)
         brnrm = brnrm+brin(i)**2
         brssq = brssq+rvec**2
 50   continue

      bznrm = 0.0D0
      bzssq = 0.0D0

      do 70 i = 1,npts
         svec = 0.0D0
         if (nfix.gt.0) svec = bfix(i+npts)
         do 60 j = 1,ngrp
            svec = svec+gmat(i+npts,j)*ccls(j)
 60      continue
         rvec = svec-bzin(i)
         bznrm = bznrm+bzin(i)**2
         bzssq = bzssq + rvec**2
 70   continue
      ssq = brssq/(1.0D0 + brnrm) + bzssq/(1.0D0+bznrm)

      return
      end
c______________________________________________________________________
      SUBROUTINE FIXB(nptsmx,nfixmx,lrow1,npts,rpts,zpts,nfix,rfix,
     +     zfix,cfix,work1,bfix)

c  Calculate field from the fixed current loops

      IMPLICIT NONE

      INTEGER nptsmx,nfixmx,lrow1,i

      DOUBLE PRECISION
     +     rpts(nptsmx), zpts(nptsmx)

      DOUBLE PRECISION
     +     rfix(nfixmx), zfix(nfixmx), cfix(nfixmx),
     +     work1(nfixmx)

      DOUBLE PRECISION
     +     bfix(lrow1)

      INTEGER npts,nfix

      DOUBLE PRECISION brw,bzw,psw

      do 10 i = 1,npts
         bfix(i) = 0.0D0
         bfix(npts + i) = 0.0D0
 10   continue
      
      if (nfix.le.0) goto 1000
      
      do 20 i = 1,npts
         call bfield(nfixmx,nfix,rfix,zfix,cfix,work1,rpts(i),zpts(i),
     +        brw,bzw,psw)
         bfix(i) = brw
         bfix(npts + i) = bzw
 20   continue

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE MTRX(nptsmx,ngrpmx,nclsmx,lrow1,lcol1,npts,rpts,zpts,
     +     brin,bzin,ngrp,ncls,rcls,zcls,alfa,nrws,bfix,gmat,bvec,
     +     rc,zc,cc,xc)

c  Set up matrix equation

      IMPLICIT NONE

      INTEGER nptsmx,ngrpmx,nclsmx,lrow1,lcol1

      INTEGER ncls(ngrpmx+2)

      DOUBLE PRECISION
     +     rcls(ngrpmx,nclsmx), zcls(ngrpmx,nclsmx)

      DOUBLE PRECISION
     +     rc(nclsmx), zc(nclsmx), cc(nclsmx), xc(nclsmx)

      DOUBLE PRECISION
     +     brin(nptsmx), bzin(nptsmx), rpts(nptsmx), zpts(nptsmx)

      DOUBLE PRECISION
     +     bfix(lrow1), bvec(lrow1),
     +     gmat(lrow1,lcol1)

      INTEGER npts,ngrp,i,j,k,nc,nrws

      DOUBLE PRECISION alfa,brw,bzw,psw

      do 30 i = 1,npts
         bvec(i) = brin(i) - bfix(i)
         bvec(i+npts) = bzin(i) - bfix(i + npts)
         do 20 j = 1,ngrp
            nc = ncls(j)
            do 10 k = 1,nc
               rc(k) = rcls(j,k)
               zc(k) = zcls(j,k)
               cc(k) = 1.0D0
 10         continue
            call bfield(nclsmx,nc,rc,zc,cc,xc,rpts(i),zpts(i),
     +           brw,bzw,psw)
            gmat(i,j) = brw
            gmat(i+npts,j) = bzw
 20      continue
 30   continue

c  Add constraint equations

      nrws = 2 * npts

      do 50 j = 1,ngrp
         bvec(nrws + j) = 0.0D0
         do 40 i = 1,ngrp
            gmat(nrws + j,i) = 0.0D0
 40      continue
         nc = ncls(j)
         gmat(nrws + j,j) = DBLE(nc) * alfa
 50   continue

      nrws = 2*npts + ngrp

      return
      end
c______________________________________________________________________
      SUBROUTINE BFIELD(ncmax, nc, rc, zc, cc, xc, rp, zp, br, bz, psi)

c  Subroutine to calculate the field at point (rp,zp) due to 
c  currents at the nc filaments. Written by D. Strickler,
c  (ORNL), and modified by J. Galambos (FEDC/ORNL, 615-576-5482)
c  for incorporation into SuperCode.
c
c  Input :
c  ncmax   declared size of arrays rc, zc, cc, xc
c  nc      number of loops
c  rc,zc   coordinates of conductor loops (m)
c  cc      currents in conductor loops (A)
c  rp,zp   coordinates of field point (m)
c
c  Output :
c  br,bz   components of poloidal field (T)
c  psi     poloidal magnetic flux (Wb)
c  xc      mutual inductances (H)

      IMPLICIT NONE

      INTEGER i,nc,ncmax

      DOUBLE PRECISION rp,zp,br,bz,psi,a0,a1,a2,a3,a4,b0,b1,b2,b3,b4,
     +     c1,c2,c3,c4,d1,d2,d3,d4,xmu,twopi
      DOUBLE PRECISION zs,dr,d,s,t,a,xk,xe,dz,sd,brx,bzx

      DOUBLE PRECISION
     +     rc(ncmax),zc(ncmax),cc(ncmax),xc(ncmax)

      a0 = 1.38629436112D0
      a1 = 0.09666344259D0
      a2 = 0.03590092383D0
      a3 = 0.03742563713D0
      a4 = 0.01451196212D0
      b0 = 0.5D0
      b1 = 0.12498593597D0
      b2 = 0.06880248576D0
      b3 = 0.03328355346D0
      b4 = 0.00441787012D0
      c1 = 0.44325141463D0
      c2 = 0.06260601220D0
      c3 = 0.04757383546D0
      c4 = 0.01736506451D0
      d1 = 0.24998368310D0
      d2 = 0.09200180037D0
      d3 = 0.04069697526D0
      d4 = 0.00526449639D0
      xmu = 1.256637062D-6
      twopi = 6.283185308D0

      br  = 0.0D0
      bz  = 0.0D0
      psi = 0.0D0

      do 10 i=1,nc
         d = (rp + rc(i))**2 + (zp - zc(i))**2
         s = 4.0D0*rp*rc(i)/d

         t = 1.0D0 - s
         a = log(1.0D0/t)

         dz = zp - zc(i)
         zs = dz**2
         dr = rp - rc(i)
         sd = sqrt(d)

c  Evaluation of elliptic integrals

         xk = a0 + t*(a1 + t*(a2 + t*(a3 + a4*t)))
     +        + a*(b0 + t*(b1 + t*(b2 + t*(b3 + b4*t))))
         xe = 1.0D0 + t*(c1 + t*(c2 + t*(c3 + c4*t)))
     +        + a*t*(d1 + t*(d2 + t*(d3 + d4*t)))

c  Mutual inductances

         xc(i) = 0.5D0*xmu*sd*((2.0D0 - s)*xk - 2.0D0*xe)

c  Radial, vertical fields

         brx = xmu*cc(i)*dz/(twopi*rp*sd)*(- xk +
     +        (rc(i)**2 + rp**2 + zs)/(dr**2 + zs)*xe)
         bzx = xmu*cc(i)/(twopi*sd)*(xk +
     +        (rc(i)**2 - rp**2 - zs)/(dr**2 + zs)*xe)

c  Sum fields, flux

         br = br + brx
         bz = bz + bzx
         psi = psi + xc(i)*cc(i)

 10   continue

      return
      end
