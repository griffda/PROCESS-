CSCCS-*-Fortran-*-CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
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
C  Module name    : $RCSfile: numer.h,v $
C  Version no.    : $Revision: 3.3 $
C
C  Creation date  : $Date: 1997/11/19 09:41:22 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      double precision epsfcn, epsvmc, factor, ftol, sqsumsq
      common /cmcom0/  epsfcn, epsvmc, factor, ftol, sqsumsq

      double precision
     +     bondl(ipnvars),
     +     bondu(ipnvars),
     +     boundl(ipnvars),
     +     boundu(ipnvars),
     +     rcm(ipnvars)
      double precision
     +     resdl(ipnvars),
     +     scafc(ipnvars),
     +     scale(ipnvars),
     +     vlam(ipvlam),
     +     xcm(ipnvars),
     +     xcs(ipnvars)
      common /numer1/
     +     bondl, bondu, boundl, boundu, rcm, resdl, scafc, scale,
     +     xcm, xcs, vlam

C+**PJK 26/01/93 Removed redundant variables IIDATE,IIMACH,IITIME,IOUT

      integer
     +     ioptimz, maxcal, minmax, ncalls, neqns, nfev1, nfev2, nin,
     +     nineqns, nout, nvar, nvrbl
      common /numer2/
     +     ioptimz, maxcal, minmax, ncalls, neqns, nfev1, nfev2, nin,
     +     nineqns, nout, nvar, nvrbl

      integer icc(ipeqns), ixc(ipnvars)
      common /numer3/ icc, ixc

      character*(48)  icase
      common /numer4/ icase

      integer         iotty,ivms
      common /numer5/ iotty,ivms

