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
C  Module         : $Id: htpwr.h,v 3.5 1997/01/24 14:55:25 peter Exp pknight $
C
C  Module name    : $RCSfile: htpwr.h,v $
C  Version no.    : $Revision: 3.5 $
C
C  Creation date  : $Date: 1997/01/24 14:55:25 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C+**PJK 02/10/92 Added pinjht : used in costs
C+**PJK 20/01/97 Added htpmw
C+**PJK 22/01/97 Moved contents of pfelect.h, heattr.h and heatrinp.h
C+**PJK 22/01/97 into this file
C+**PJK 22/05/07 Added hydrogen plant variables

      double precision
     +     baseel, crypmw, ctht, etahhten, etahhtex, etahlte, etahth,
     +     etath, facht, fauxbop, fcsht, ffwlg, fgrosbop, fmgdmw,
     +     helecmw, hpower, hthermmw, hvolume, helpow, htpmw, pacpmw,
     +     peakmva, pfwdiv, pgrossmw, pinjht, pinjwp, pnetelmw,
     +     ppmphemw, priheat, psecht, pthermmw, pwpm2, rnihx, rnphx,
     +     tfacpd, tlvpmw, trithtmw, vachtmw
      common /htpwr0/
     +     baseel, crypmw, ctht, etahhten, etahhtex, etahlte, etahth,
     +     etath, facht, fauxbop, fcsht, ffwlg, fgrosbop, fmgdmw,
     +     helecmw, hpower, hthermmw, hvolume, helpow, htpmw, pacpmw,
     +     peakmva, pfwdiv, pgrossmw, pinjht, pinjwp, pnetelmw,
     +     ppmphemw, priheat, psecht, pthermmw, pwpm2, rnihx, rnphx,
     +     tfacpd, tlvpmw, trithtmw, vachtmw

      integer         ihplant, iprimhtp
      common /htpwr1/ ihplant, iprimhtp

