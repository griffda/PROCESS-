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
C  Module         : $Id: build.h,v 3.4 1997/02/05 11:43:40 peter Exp $
C
C  Module name    : $RCSfile: build.h,v $
C  Version no.    : $Revision: 3.4 $
C
C  Creation date  : $Date: 1997/02/05 11:43:40 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      double precision
     +     aplasmin, bcylth, blnkith, blnkoth, bore, ddwex, ddwi,
     +     gapds, gapoh, gapomin, gapsto, fwarea, fwith, fwoth,
     +     hmax, hr1, ohcth, prtsz, prtszreq, rbld, rinboard,
     +     rsldi, rsldo, rtfcin, rtot, scrapli, scraplo, shldith,
     +     shldoth, shldtth, tfcth, tfootfi, tfthko, vgap, vgaptf,
     +     vgap2
      common /build0/
     +     aplasmin, bcylth, blnkith, blnkoth, bore, ddwex, ddwi,
     +     gapds, gapoh, gapomin, gapsto, fwarea, fwith, fwoth,
     +     hmax, hr1, ohcth, prtsz, prtszreq, rbld, rinboard,
     +     rsldi, rsldo, rtfcin, rtot, scrapli, scraplo, shldith,
     +     shldoth, shldtth, tfcth, tfootfi, tfthko, vgap, vgaptf,
     +     vgap2

      integer         iohcl
      common /build1/ iohcl

      double precision
     +     fmsbc, fmsbl, fmsdwe, fmsdwi, fmsfw, fmsoh, fmssh, fmstf
      common /marten/
     +     fmsbc, fmsbl, fmsdwe, fmsdwi, fmsfw, fmsoh, fmssh, fmstf
