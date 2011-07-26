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
C  Module name    : $RCSfile: labels.h,v $
C  Version no.    : $Revision: 3.2 $
C
C  Creation date  : $Date: 1994/03/31 09:40:48 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C+**PJK 10/12/92 Changed dimension of array LABLMM to ipnfoms
C+**PJK 10/12/92 Changed dimension of array TAUSCL to ipnlaws

      character*(34) lablcc(ipeqns)
      character*(22) lablmm(ipnfoms)
      character*(8)  lablxc(ipnvars)
      character*(24) tauscl(ipnlaws)

      common /label1/ lablcc, lablmm, lablxc
      common /label2/ tauscl

