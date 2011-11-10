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
C  Module name    : $RCSfile: phydat.h,v $
C  Version no.    : $Revision: 3.16 $
C
C  Creation date  : $Date: 2006/05/25 09:29:15 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

      double precision
     +     abeam, afuel, aion, alphaj, alphan, alphat, alpmw, aspect,
     +     beamfus0, beta, betaft, betalim, betanb, betap, betbm0,
     +     bp, bt, btot, burnup, capa, carea, cfe0, csawth, cvol,
     +     deltdn, deltup, dene, deni, dign, dlamee, dlamie, dnalp,
     +     dnbeam, dnbeam2, dnbeta, dnelimt, dnitot, dnla, dnprot,
     +     dntau, dnz, ealpha, epbetmax, eps, faccd, facoh, falpe,
     +     falpha, falpi, faux, fbfe, fdeut, ffwal, fhe3, figmer,
     +     fradmin, ftr, ftrit, fvsbrnni, gamma, hfact, impc, impfe,
     +     impo, kappa, kappa95, kappaa, palp, palpe, palpi, palpnb,
     +     pbrem, pcharge, pcoef
      double precision
     +     pdivt, pfuscmw, phiint, pi, pie, plascur, plrad, pneut,
     +     pohmpv, powerht, powfmw, prad, protonmw, psync, ptre, ptri,
     +     q, q0, q95, qfuel, qlim, qstar, ralpne, recyle, rli, rlp,
     +     rmajor, rminor, rmu0, rnbeam, rncne, rndfuel, rnfene, rnone,
     +     rpfac, rplas, rtpte, sarea, sareao, sf, ssync, tauee,
     +     taueff, tauei, te, ten, ti, tin, tratio, triang, triang95,
     +     vol, vsbrn, vshift, vsind, vsres, vsstt, wallmw, wtgpd,
     +     xarea, zeff, zeffai
      common /phydt0/
     +     abeam, afuel, aion, alphaj, alphan, alphat, alpmw, aspect,
     +     beamfus0, beta, betaft, betalim, betanb, betap, betbm0,
     +     bp, bt, btot, burnup, capa, carea, cfe0, csawth, cvol,
     +     deltdn, deltup, dene, deni, dign, dlamee, dlamie, dnalp,
     +     dnbeam, dnbeam2, dnbeta, dnelimt, dnitot, dnla, dnprot,
     +     dntau, dnz, ealpha, epbetmax, eps, faccd, facoh, falpe,
     +     falpha, falpi, faux, fbfe, fdeut, ffwal, fhe3, figmer,
     +     fradmin, ftr, ftrit, fvsbrnni, gamma, hfact, impc, impfe,
     +     impo, kappa, kappa95, kappaa, palp, palpe, palpi, palpnb,
     +     pbrem, pcharge, pcoef
      common /phydt1/
     +     pdivt, pfuscmw, phiint, pi, pie, plascur, plrad, pneut,
     +     pohmpv, powerht, powfmw, prad, protonmw, psync, ptre, ptri,
     +     q, q0, q95, qfuel, qlim, qstar, ralpne, recyle, rli, rlp,
     +     rmajor, rminor, rmu0, rnbeam, rncne, rndfuel, rnfene, rnone,
     +     rpfac, rplas, rtpte, sarea, sareao, sf, ssync, tauee,
     +     taueff, tauei, te, ten, ti, tin, tratio, triang, triang95,
     +     vol, vsbrn, vshift, vsind, vsres, vsstt, wallmw, wtgpd,
     +     xarea, zeff, zeffai

      double precision  dlimit(7),hfac(ipnlaws),pthrmw(5)
      common /phydt2/ dlimit,hfac,pthrmw

      integer
     +     gtscale, ibss, iculbl, iculdl, icurr, idensl,
     +     idhe3, idivrt, ifispact, igeom, iinvqd, iiter, ires, isc,
     +     iscrp, ishape, itart, ignite, iwalld, ifalphap
      common /phydt3/
     +     gtscale, ibss, iculbl, iculdl, icurr, idensl,
     +     idhe3, idivrt, ifispact, igeom, iinvqd, iiter, ires, isc,
     +     iscrp, ishape, itart, ignite, iwalld, ifalphap
