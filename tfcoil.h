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
C  Module name    : $RCSfile: tfcoil.h,v $
C  Version no.    : $Revision: 3.10 $
C
C  Creation date  : $Date: 1999/07/06 13:10:42 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      integer         itfsup
      common /tfcom0/ itfsup

C *** Resistive TF coil variables

C+**PJK 18/11/97 Added DRTOP,DZTOP
      double precision
     +     arealeg, bmaxtf, cdtfleg, cforce, cpres, cpttf, drtop, dztop,
     +     estotf, fcoolcp, jbus, oacdcp, prescp, rbmax, rhocp,
     +     rhotfleg, ripmax, ripple, ritfc, sigrad, sigtan, sigver,
     +     tcpav, tfareain, tfboreh, tfbusl, tfbusmas, tfcmw, tfcpmw,
     +     tflegmw, tflegres, tfno, tmpcry, turnstf, vforce, vftf,
     +     volcp, voltfleg, vtfkv, whtcp, whttf, whttflgs, wpvf, wtbc
      common /tfcom1/
     +     arealeg, bmaxtf, cdtfleg, cforce, cpres, cpttf, drtop, dztop,
     +     estotf, fcoolcp, jbus, oacdcp, prescp, rbmax, rhocp,
     +     rhotfleg, ripmax, ripple, ritfc, sigrad, sigtan, sigver,
     +     tcpav, tfareain, tfboreh, tfbusl, tfbusmas, tfcmw, tfcpmw,
     +     tflegmw, tflegres, tfno, tmpcry, turnstf, vforce, vftf,
     +     volcp, voltfleg, vtfkv, whtcp, whttf, whttflgs, wpvf, wtbc

C *** Centrepost variables

      double precision 
     +     cph2o,denh2o,etapump,frhocp,kcp,kh2o,muh2o,ncool,ppump,
     +     ptempalw,rcool,tcoolin,tcpav2,tcpmax,vcool
      common /tfcom2/
     +     cph2o,denh2o,etapump,frhocp,kcp,kh2o,muh2o,ncool,ppump,
     +     ptempalw,rcool,tcoolin,tcpav2,tcpmax,vcool

C *** Superconducting TF coil variables

C+**PJK 26/07/11 Added jcrit_model
      integer         isumattf, itfmod, magnt, jcrit_model
      common /tfcom3/ isumattf, itfmod, magnt, jcrit_model

      double precision
     +     acasetf, acndttf, acond, acstf, aiwp, alstrtf, aspcstf, aswp,
     +     avwp, bcritsc, bmaxtfrp, borev, casestr, casfact, casthi,
     +     casths, csutf, csytf, dcase, dcopper, deflect, eyins, eystl,
     +     eywp, fcutfsu, jcritsc, jwdgcrt, jwdgpro, jwptf, poisson,
     +     rjtfsual, rnltf, sigrcon, sigtcon, sigvert, strncon, strtf1,
     +     strtf2, tcritsc, tdmptf, tfckw, tficrn, tfind, tfleng,
     +     tfocrn, tfsai, tfsao, tftmp, thicndut, thkcas, thkwp,
     +     thwcndut, tinstf, tmargmin, tmargtf, tmaxpro, vdalw, vtfskv,
     +     whtcas, whtcon, whtconcu, whtconsc, whtconsh, wwp1, wwp2
      common /tfcom4/
     +     acasetf, acndttf, acond, acstf, aiwp, alstrtf, aspcstf, aswp,
     +     avwp, bcritsc, bmaxtfrp, borev, casestr, casfact, casthi,
     +     casths, csutf, csytf, dcase, dcopper, deflect, eyins, eystl,
     +     eywp, fcutfsu, jcritsc, jwdgcrt, jwdgpro, jwptf, poisson,
     +     rjtfsual, rnltf, sigrcon, sigtcon, sigvert, strncon, strtf1,
     +     strtf2, tcritsc, tdmptf, tfckw, tficrn, tfind, tfleng,
     +     tfocrn, tfsai, tfsao, tftmp, thicndut, thkcas, thkwp,
     +     thwcndut, tinstf, tmargmin, tmargtf, tmaxpro, vdalw, vtfskv,
     +     whtcas, whtcon, whtconcu, whtconsc, whtconsh, wwp1, wwp2

      double precision
     +     dcond(5),
     +     eyoung(5),
     +     jeff(5),
     +     radtf(6),
     +     sigrtf(5),
     +     sigttf(5)
      common /tfcom5/
     +     dcond, eyoung, jeff, radtf, sigrtf, sigttf

      double precision farc4tf
      common /tfcom6/ farc4tf

      double precision
     +     dthet(5),
     +     radctf(5),
     +     xarc(5),
     +     xctfc(5),
     +     yarc(5),
     +     yctfc(5)
      common /tfcom7/ dthet, radctf, xarc, xctfc, yarc, yctfc    

