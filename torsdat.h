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
C  Module         : $Id: torsdat.h,v 3.2 1994/03/31 09:40:59 peter Exp $
C
C  Module name    : $RCSfile: torsdat.h,v $
C  Version no.    : $Revision: 3.2 $
C
C  Creation date  : $Date: 1994/03/31 09:40:59 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      double precision dlscal, vacdshm, vcdimax, vpumpn
      common /tors0/   dlscal, vacdshm, vcdimax, vpumpn

      integer        nvduct, nvtype
      common /tors1/ nvduct, nvtype

