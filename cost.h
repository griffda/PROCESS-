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
C  Module name    : $RCSfile: cost.h,v $
C  Version no.    : $Revision: 3.15 $
C
C  Creation date  : $Date: 1999/07/06 13:11:10 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      double precision
     +     cfind(4),
     +     uchts(2),
     +     ucoam(4),
     +     ucsc(5),
     +     ucturb(2),
     +     ucwst(4)
      common /cost00/
     +     cfind, uchts, ucoam, ucsc, ucturb, ucwst

      double precision
     +     abktflnc, adivflnc, blkcst, capcost, cdcost, cdirt, cdrlife,
     +     cfactr, chplant, coe, coecap, coefuelt, coeoam, concost,
     +     cplife, cpstcst, cpstflnc, crctcore, c221, c222, decomf,
     +     dintrt, divcst, divlife, dtlife, fcap0, fcap0cp, fcdfuel,
     +     fcontng, fcr0, fkind, fwallcst, moneyint, ratecdol,
     +     tbktrepl, tcomrepl, tdivrepl, tlife, uubop, uucd, uudiv,
     +     uufuel, uufw, uumag, uuves
      common /cost0/
     +     abktflnc, adivflnc, blkcst, capcost, cdcost, cdirt, cdrlife,
     +     cfactr, chplant, coe, coecap, coefuelt, coeoam, concost,
     +     cplife, cpstcst, cpstflnc, crctcore, c221, c222, decomf,
     +     dintrt, divcst, divlife, dtlife, fcap0, fcap0cp, fcdfuel,
     +     fcontng, fcr0, fkind, fwallcst, moneyint, ratecdol,
     +     tbktrepl, tcomrepl, tdivrepl, tlife, uubop, uucd, uudiv,
     +     uufuel, uufw, uumag, uuves

      integer        iavail, ifueltyp, ipnet, ireactor, lsa
      common /cost1/ iavail, ifueltyp, ipnet, ireactor, lsa

      double precision
     +     cconfix, cconshpf, cconshtf, cland, cowner, csi, cturbb
      common /ucost0/
     +     cconfix, cconshpf, cconshtf, cland, cowner, csi, cturbb

      double precision
     +     ucad, ucaf, ucahts, ucap, ucblbe, ucblli, ucblli2o,
     +     ucbllipb, ucblss, ucblvd, ucbpmp, ucbus, uccase, ucco,
     +     uccpclb, uccpcl1, uccpmp, uccr, uccry, uccryo, uccu,
     +     ucdgen, ucdiv, ucdtc, ucduct, ucech, ucel, uces1, uces2,
     +     ucfnc, ucfpr, ucfuel, ucfwa, ucfwps, ucfws, ucf1, ucgss,
     +     uche3, uchhten, uchhtex, uchlte, uchrs, uchth, uciac,
     +     ucich, ucihx, ucint, uclh, uclv, ucmb, ucme
      common /ucost1/
     +     ucad, ucaf, ucahts, ucap, ucblbe, ucblli, ucblli2o,
     +     ucbllipb, ucblss, ucblvd, ucbpmp, ucbus, uccase, ucco,
     +     uccpclb, uccpcl1, uccpmp, uccr, uccry, uccryo, uccu,
     +     ucdgen, ucdiv, ucdtc, ucduct, ucech, ucel, uces1, uces2,
     +     ucfnc, ucfpr, ucfuel, ucfwa, ucfwps, ucfws, ucf1, ucgss,
     +     uche3, uchhten, uchhtex, uchlte, uchrs, uchth, uciac,
     +     ucich, ucihx, ucint, uclh, uclv, ucmb, ucme

      double precision
     +     ucmisc, ucnbi, ucnbv, ucof, ucpens, ucpfb, ucpfbk, ucpfbs,
     +     ucpfcb, ucpfdr1, ucpfic, ucpfps, ucphx, ucpp, ucrb, ucsh,
     +     ucshld, ucswyd, uctfbr, uctfbus, uctfdr, uctfgr, uctfic,
     +     uctfps, uctfsw, uctpmp, uctr, ucvalv, ucvdsh, ucviac,
     +     ucwindpf, ucwindtf, ucws
      common /ucost2/
     +     ucmisc, ucnbi, ucnbv, ucof, ucpens, ucpfb, ucpfbk, ucpfbs,
     +     ucpfcb, ucpfdr1, ucpfic, ucpfps, ucphx, ucpp, ucrb, ucsh,
     +     ucshld, ucswyd, uctfbr, uctfbus, uctfdr, uctfgr, uctfic,
     +     uctfps, uctfsw, uctpmp, uctr, ucvalv, ucvdsh, ucviac,
     +     ucwindpf, ucwindtf, ucws
