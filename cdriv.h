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
C  Module name    : $RCSfile: cdriv.h,v $
C  Version no.    : $Revision: 3.3 $
C
C  Creation date  : $Date: 1996/03/07 16:10:59 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      double precision
     +     beamwd, bigq, bootipf, bscfmax, cboot, cnbeam, echpwr,
     +     echpwr0, echwpow, enbeam, etaech, etalh, etanbi, etaof,
     +     feffcd, frbeam, ftritbm, gamcd, pheat, pinjalw, pinje,
     +     pinji, plhybd, pnbeam, pofcd, pwplh, pwpnb, taubeam,
     +     tbeamin
      common /cdriv0/
     +     beamwd, bigq, bootipf, bscfmax, cboot, cnbeam, echpwr,
     +     echpwr0, echwpow, enbeam, etaech, etalh, etanbi, etaof,
     +     feffcd, frbeam, ftritbm, gamcd, pheat, pinjalw, pinje,
     +     pinji, plhybd, pnbeam, pofcd, pwplh, pwpnb, taubeam,
     +     tbeamin

      integer         iefrf, irfcd
      common /cdriv1/ iefrf, irfcd

