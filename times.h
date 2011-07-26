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
C  Module name    : $RCSfile: times.h,v $
C  Version no.    : $Revision: 3.3 $
C
C  Creation date  : $Date: 1994/03/31 09:39:25 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      double precision
     +     tburn, tburn0, tdown, tdwell, theat, tohs, tohsin, tpulse,
     +     tqnch, tramp
      common /times0/
     +     tburn, tburn0, tdown, tdwell, theat, tohs, tohsin, tpulse,
     +     tqnch, tramp

      double precision tim(6)
      common /times1/  tim

