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
C  Module name    : $RCSfile: param.h,v $
C  Version no.    : $Revision: 3.21 $
C
C  Creation date  : $Date: 1998/10/08 10:04:49 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C  ipnvars : total number of variables available for iteration
C  ipeqns  : number of constraint equations available
C  ipnlaws : number of energy confinement time scaling laws
C  ipnfoms : number of available figures of merit
C  ipnscnv : number of available scan variables
C  ipnscns : maximum number of scan points

      integer ipeqns, ipnfoms, ipnlaws, ipnscns, ipnscnv, ipnvars,
     +     iptnt, ipvlam, ipvp1

      parameter (
     +     ipnvars = 88,
     +     ipeqns  = 50,
     +     ipnlaws = 36,
     +     ipnfoms = 13,
     +     ipnscns = 50,
     +     ipnscnv = 26,
     +     ipvlam  = ipeqns+2*ipnvars+1,
     +     iptnt   = (ipeqns*(3*ipeqns+13))/2,
     +     ipvp1   = ipnvars+1
     +     )

