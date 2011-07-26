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
C  Module name    : $RCSfile: vltcom.h,v $
C  Version no.    : $Revision: 3.2 $
C
C  Creation date  : $Date: 1994/03/31 09:41:01 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C+**PJK 26/01/93 Removed FVSEF from common block

      double precision
     +     vsbn, vsefbn, vsefsu, vseft, vsoh, vsohbn, vsohsu, vssu,
     +     vstot
      common /vltcm0/
     +     vsbn, vsefbn, vsefsu, vseft, vsoh, vsohbn, vsohsu, vssu,
     +     vstot

      double precision  sxlg(ngc2,ngc2)
      common /vltcm1/ sxlg

