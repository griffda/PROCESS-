CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS
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
C  Module name    : $RCSfile: physics.f,v $
C  Version no.    : $Revision: 3.29 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

C----------------------------------------------------------------------
      SUBROUTINE PHYSICS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.230
C
C--Description
C  Subroutine to calculate tokamak plasma physics information
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 May 2006
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  20/06/94 PJK 1.000 Upgrade to higher standard of coding
C  04/12/95 PJK 1.100 Added D-He3 relevant coding
C  14/05/96 PJK 1.110 Modified poloidal beta used in bootstrap formula
C                     and added diamagnetic contribution
C  10/06/96 PJK 1.120 Added use of IWALLD in wall load calculation
C  07/10/96 PJK 1.130 Added new ICULBL=2 option
C  17/09/97 PJK 1.140 Added Greenwald density limit (added arguments
C                     to CULDLM)
C  01/04/98 PJK 1.150 Changed PBREM to PRAD in argument list of PCOND,
C                     added DNLA and IGNITE to arguments of BETCOM
C                     and PCOND, and added other effects of IGNITE
C  24/04/98 PJK 1.160 Added IMPC, IMPFE, IMPO to arguments of BETCOM
C  30/06/98 PJK 1.170 Added XAREA to arguments of PCOND
C  17/07/98 PJK 1.200 Added call to PTHRESH
C  19/01/99 PJK 1.210 Added POWERHT to argument list of PCOND
C  16/07/01 PJK 1.220 Added KAPPAA to argument list of PCOND
C  22/05/06 PJK 1.230 Added IFALPHAP to argument list of PALPH2
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'times.h'
      INCLUDE 'divrt.h'
      INCLUDE 'build.h'
      INCLUDE 'pulse.h'

C  Local variables
      DOUBLE PRECISION
     +     alphap,betat,betpth,fusrat,n0e,n0i,pht,pinj,p0,sbar,
     +     sigvdt,taup,t0e,t0i,zimp,zion

C  External functions
      DOUBLE PRECISION fnewbs,culbst
      EXTERNAL         fnewbs,culbst

C  External routines
      EXTERNAL beamfus,betcom,bootst,cudriv,culblm,culcur,culdlm,
     +     curren,denlim,palph,palph2,pcond,phyaux,pohm,pthresh,radpwr,
     +     rether,vscalc

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Calculate plasma composition

      call betcom(alphan,alphat,cfe0,dene,fdeut,ftrit,fhe3,ftr,ftritbm,
     +     idhe3,ignite,impc,impfe,impo,ralpne,rnbeam,te,zeff,abeam,
     +     afuel,aion,deni,dlamee,dlamie,dnalp,dnbeam,dnitot,dnla,
     +     dnprot,dnz,falpe,falpi,pcoef,rncne,rnone,rnfene,zeffai,zion)

      ten = te * pcoef
      tin = ti * pcoef

C *** Central values for temperature (keV), density (m**-3) and
C *** pressure (Pa). From ideal gas law : p = nkT

      t0e = te * (1.0D0+alphat)
      t0i = ti * (1.0D0+alphat)
      n0e =   dene * (1.0D0+alphan)
      n0i = dnitot * (1.0D0+alphan)
      p0 = (n0e*t0e + n0i*t0i) * 1.6022D-16

C *** Pressure profile index
C *** From ideal gas law : p = nkT

      alphap = alphan + alphat

C *** Calculate plasma current

C+**PJK 09/06/92 Changed call to plasma current routine to allow
C+**PJK 09/06/92 for use of the new routine CULCUR.

      if (iculcr.eq.0) then
C ***    Use old routine
         call curren(bt,eps,icurr,kappa,kappa95,q,rmajor,rminor,sf,
     +        triang,triang95,bp,qstar,plascur)

      else if (iculcr.eq.1) then
C ***    Use new routine CULCUR.
         call culcur(alphaj,alphap,bt,eps,icurr,kappa,kappa95,p0,q,
     +        rmajor,rminor,sf,triang,triang95,bp,qstar,plascur)

      else
C ***    Illegal value of ICULCR
         WRITE(*,*) 'Error in routine PHYSICS:'
         WRITE(*,*) 'Illegal value of ICULCR, = ',iculcr
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

C+**PJK 29/01/96 Added q95 scaling if ICURR=2
      if (icurr.eq.2) then
         q95 = q * 1.3D0 * (1.0D0 - eps)**0.6D0
      else
         q95 = q
      end if

      btot = sqrt(bt**2 + bp**2)
      betap = beta * ( btot/bp )**2

C *** Set PF coil ramp times

C+**PJK 11/11/93
      if (lpulse.ne.1) then

         if (tohsin.eq.0.0D0) then
            tohs = plascur/5.0D5
            tramp = tohs
            tqnch = tohs
         else
            tohs = tohsin
         end if

      else

C+**PJK 11/11/93 tohs is set either in INITIAL or INPUT, or by being
C+**PJK 11/11/93 iterated using limit equation 41.

         tramp = max(tramp,tohs)
         tqnch = max(tqnch,tohs)

      end if

C+**PJK 30/03/94 Reset second tburn value (tburn0).
C+**PJK 30/03/94 Use of consistency eqn.15 will ensure that the code
C+**PJK 30/03/94 uses a consistent value for the burn time throughout,
C+**PJK 30/03/94 for the pulsed reactor case.

      tburn0 = tburn

C *** Pulse and down times : The reactor is assumed to be 'down'
C *** at all times outside of the plasma current flat-top period.
C *** The pulse length is the duration of non-zero plasma current

      tpulse = tohs + theat + tburn + tqnch
      tdown  = tramp + tohs + tqnch + tdwell

C+**PJK 31/07/92 The block that has now been moved to the start of
C+**PJK 31/07/92 this routine was here originally.

C *** Calculate bootstrap current fraction

C+**PJK 03/06/92 Changed logic to allow the use of the new bootstrap
C+**PJK 03/06/92 current fraction method.

      if (bscfmax.lt.0.0D0) then

         bootipf = abs(bscfmax)

      else
         if (ibss.eq.1) then
            
            call bootst(aspect,beta,btot,cboot,plascur,pi,q95,q0,rmajor,
     +           vol,bootipf)

         else if (ibss.eq.2) then
            betat = beta * btot**2 / bt**2
            bootipf = fnewbs(alphan,alphat,betat,bt,dene,plascur,q95,
     +           q0,rmajor,rminor,ten,zeff)

         else if (ibss.eq.3) then
C+**PJK 02/05/96 Use thermal poloidal beta, not total
C+**PJK 14/05/96 Added beta and itart as arguments to CULBST
            betpth = (beta-betaft-betanb) * ( btot/bp )**2
            bootipf = culbst(alphaj,alphan,alphat,beta,betpth,q0,q95,
     +           rmajor,rminor,itart)

         else
C ***       Illegal value of IBSS
            WRITE(*,*) 'Error in routine PHYSICS:'
            WRITE(*,*) 'Illegal value of IBSS, = ',ibss
            WRITE(*,*) 'PROCESS stopping.'
            STOP
         end if

         bootipf = min(bootipf,bscfmax)

      end if

C+**PJK 12/04/94 Bootstrap current fraction constrained to be less than
C+**PJK 12/04/94 or equal to the total fraction of the plasma current
C+**PJK 12/04/94 produced by non-inductive means (which also includes
C+**PJK 12/04/94 the current drive proportion)

      bootipf = min(bootipf,fvsbrnni)

C *** Fraction of plasma current produced by inductive means

      facoh = max( 1.0D-10, (1.0D0 - fvsbrnni) )

C *** Fraction of plasma current produced by auxiliary current drive

      faccd = fvsbrnni - bootipf

C *** Do auxiliary current drive power calculations

      if (irfcd.ne.0) call cudriv(nout,0)

C *** Calculate fusion power

C+**PJK 05/12/95 Added D-He3 variables to argument list of PALPH

      call palph(alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit,
     +     idhe3,iiter,pcoef,pi,ti,palp,pcharge,pneut,sigvdt)

C *** Calculate neutral beam slowing down effects

C+**PJK 05/12/95 Added ealpha to argument list of BEAMFUS
C+**PJK 11/12/95 Changed ftr to ftrit, and added fdeut to argument
C+**PJK 11/12/95 list of BEAMFUS
C+**PJK 01/04/98 If ignited, then ignore beam fusion effects

      if ((pnbeam.ne.0.0D0).and.(ignite.eq.0)) then
         call beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie,
     +        ealpha,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol,
     +        zeffai,betanb,dnbeam2,palpnb)
      end if

C+**PJK 06/12/95 Added pcharge,pneut to argument list of PALPH2
C+**PJK 22/05/06 Added IFALPHAP switch to argument list of PALPH2

      call palph2(bt,bp,dene,deni,dnitot,ftr,falpe,falpi,palpnb,
     +     ifalphap,pcharge,pcoef,pneut,te,ti,vol,alpmw,betaft,
     +     palp,palpi,palpe,pfuscmw,powfmw)

C *** Neutron wall load

C+**PJK 07/12/95 Used pneut instead of 0.8*powfmw in wallmw calculation
C+**PJK 10/06/96 Added use of IWALLD

      if (iwalld.eq.1) then
         wallmw = ffwal * (pneut*vol) / sarea
      else
         wallmw = ffwal * (pneut*vol) / fwarea
      end if

C *** Calculate ion/electron equilibration power

      call rether(alphan,alphat,dene,dlamie,te,ti,zeffai,pie)

C *** Calculate radiation power

      call radpwr(alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,rmajor,
     +     rminor,ralpne,rncne,rnone,rnfene,ssync,ten,vol,pbrem,plrad,
     +     prad,psync)

C *** Limit for minimum radiation power

C+**PJK 07/12/95 Added pcharge*vol to pht

      pht = 1.0D-6 * (pinji + pinje) + alpmw + pcharge*vol
      pbrem = max( (fradmin*pht/vol), pbrem)
      prad = pbrem + psync

C *** Calculate ohmic power

      call pohm(facoh,ires,kappa95,plascur,rmajor,rminor,ten,vol,zeff,
     +     pohmpv,rpfac,rplas)

C *** Calculate L- to H-mode power threshold

      call pthresh(dene,dnla,bt,rmajor,kappa,pthrmw)

C *** Density limit

C+**PJK 07/12/95 Added pcharge*vol to pdivt

      pinj = 1.0D-6 * (pinje + pinji)/vol
      pdivt = vol * (palp + pcharge + pinj + pohmpv - prad - plrad)
      pdivt = max(0.001D0, pdivt)

C+**PJK 08/06/92 Changed logic to allow the use of the new density
C+**PJK 08/06/92 limit formulae.

      if (iculdl.eq.0) then
C ***    Use old method
         call denlim(bt,rmajor,prn1,pdivt,q95,sarea,dnelimt)

      else if (iculdl.eq.1) then
C ***    Use new method
C+**PJK 17/09/97 Added pi,plascur,rminor to CULDLM arguments
         call culdlm(bt,idensl,pdivt,pi,plascur,prn1,qstar,q95,
     +        rmajor,rminor,sarea,zeff,dlimit,dnelimt)

      else
C ***    Illegal value of ICULDL
         WRITE(*,*) 'Error in routine PHYSICS:'
         WRITE(*,*) 'Illegal value for ICULDL, = ',iculdl
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Calculate transport losses and energy confinement time using the
C *** chosen scaling law

      call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hfact,
     +     iinvqd,isc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji,
     +     plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q95,qstar,vol,
     +     xarea,zeff,ptre,ptri,tauee,tauei,taueff,powerht)

C *** Calculate volt-second requirements

      call vscalc(csawth,eps,facoh,gamma,kappa,rmajor,rmu0,rplas,
     +     plascur,tburn,phiint,rli,rlp,vsbrn,vsind,vsres,vsstt)

C *** Calculate auxiliary physics related information
C *** for the rest of the code

      sbar = 1.0D0

C+**PJK 07/12/95 Added idhe3,dnprot to argument list of phyaux

      call phyaux(aspect,dene,deni,idhe3,plascur,powfmw,sbar,dnalp,
     +     dnprot,taueff,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

C *** Calculate beta limit

      if (gtscale .ne. 0) then
         dnbeta = 2.7D0 * (1.0D0 + 5.0D0*eps**3.5D0)
      end if

C+**PJK 03/06/92 Added new routine to calculate the Troyon-type
C+**PJK 03/06/92 beta limit in the way suggested in AEA FUS 172.

      call culblm(bt,dnbeta,plascur,rminor,betalim)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE BOOTST(aspect,beta,bt,cboot,plascur,pi,q,q0,rmajor,vol,
     +     bootipf)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.010
C
C--Description
C  Original method for finding the bootstrap-driven fraction
C  of the plasma current. Used for ITER.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  23 May 2006
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  20/06/94 PJK 1.000 Upgrade to higher standard of coding
C  23/05/06 PJK 1.010 Prevented negative square roots from being attempted
C
C--Arguments
C  aspect : (INPUT)  plasma aspect ratio
C  beta   : (INPUT)  plasma total beta
C  bt     : (INPUT)  toroidal field on axis (T)
C  cboot  : (INPUT)  bootstrap current fraction multiplier
C  plascur: (INPUT)  plasma current (A)
C  pi     : (INPUT)  famous number: 3.14...
C  q      : (INPUT)  safety factor at 95% surface
C  q0     : (INPUT)  central safety factor
C  rmajor : (INPUT)  plasma major radius (m)
C  vol    : (INPUT)  plasma volume (m3)
C  bootipf: (OUTPUT) bootstrap current fraction
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION rmu0
      PARAMETER (rmu0 = 1.256637D-6)

C  Arguments
      DOUBLE PRECISION
     +     aspect,beta,bootipf,bt,cboot,pi,plascur,q,q0,rmajor,vol

C  Local variables
      DOUBLE PRECISION
     +     betapbs,bpbs,cbs,xbs

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      xbs = min( 10.0D0, q/q0 )
      cbs = cboot * (1.32D0 - 0.235D0*xbs + 0.0185D0*xbs**2 )
      bpbs = rmu0*plascur/(2.0D0*pi*sqrt(vol/(2.0D0* pi**2 *rmajor)) )
      betapbs = beta*bt**2 / bpbs**2

C+**PJK 23/05/06 Prevent problems with negative beta

      if (betapbs.le.0.0D0) then
         bootipf = 0.0D0
      else
         bootipf = cbs* ( betapbs/sqrt(aspect) )**1.3D0
      end if

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE CURREN(
     +     bt,eps,icurr,kappa,kappa95,q,rmajor,rminor,sf,triang,
     +     triang95,bp,qstar,plascur)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to calculate the plasma current
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  29 January 1996
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  20/06/94 PJK 1.000 Upgrade to higher standard of coding
C  29/01/96 PJK 1.100 Added icurr=2 TART option
C
C--Arguments
C  bt     : (INPUT)  toroidal field on axis (T)
C  eps    : (INPUT)  inverse aspect ratio
C  icurr  : (INPUT)  current scaling model to use:
C                    1 = Peng analytic fit
C                    2 = Peng divertor scaling
C                    3 = simple ITER scaling
C                    4 = revised ITER scaling
C  kappa  : (INPUT)  plasma elongation
C  kappa95: (INPUT)  plasma elongation at 95% surface
C  q      : (INPUT)  edge safety factor (= q-bar for icurr=2)
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  sf     : (INPUT)  shape factor
C  triang : (INPUT)  plasma triangularity
C  triang95:(INPUT)  plasma triangularity at 95% surface
C  bp     : (OUTPUT) poloidal field (T)
C  qstar  : (OUTPUT) cylindrical safety factor
C  plascur: (OUTPUT) plasma current (A)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION bp,bt,eps,kappa,kappa95,plascur,q,qstar,rmajor,
     +     rminor,sf,triang,triang95
      INTEGER icurr

C  Local variables
      DOUBLE PRECISION
     +     asp,ccur,curhat,feps,trifac

C  External functions
      DOUBLE PRECISION bpol,plasc
      EXTERNAL         bpol,plasc

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      trifac = 1.0D0 + 2.0D0*triang95**2 - 1.2D0*triang95**3
      asp = 1.0D0/eps

      if (icurr.eq.1) then
C ***    Peng fit
         ccur = (1.22D0-(0.68D0*eps))
         feps = ccur/(1.0D0 - eps**2)**2
         curhat = 5.0D6*rminor*ccur*eps*(sf**2)/
     +        (q*(1.0D0-(eps**2))**2)

      else if (icurr.eq.2) then
C ***    Peng analytic scaling for double null divertor (TART)
         curhat = 1.0D6 * plasc(q,asp,rminor,bt,kappa,triang)/bt

      else if (icurr.eq.3) then
C ***    Simple ITER current scaling
         curhat = 5.0D6*(rminor**2)/(q*rmajor)

      else if (icurr.eq.4) then
C ***    ITER scaling II
         feps = (1.17D0 - 0.65D0*eps)/(1.0D0 - eps**2)**2
         curhat = 5.0D6*rminor**2 * feps/(rmajor*q)*
     +        (1.0D0 + kappa95**2 * trifac)/2.0D0

      else
         write(*,*) 'Error in routine CURREN:'
         write(*,*) 'Illegal value for ICURR, = ',icurr
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Plasma current (Amps)

      qstar = 2.5D6*rminor**2/rmajor/curhat*
     +     (1.0D0 + kappa95**2 * trifac)
      plascur = curhat*bt

C *** Poloidal field

      bp = bpol(q,asp,bt,kappa,triang)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE CULCUR(
     +     alphaj,alphap,bt,eps,icurr,kappa,kappa95,p0,qpsi,rmajor,
     +     rminor,sf,triang,triang95,bp,qstar,plascur)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to calculate the plasma current, with a choice of formula
C  for the edge safety factor.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  26 January 1996
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  20/06/94 PJK 1.000 Upgrade to higher standard of coding
C  26/01/96 PJK 1.100 Added icurr=2 TART option
C
C--Arguments
C  alphaj  : (INPUT)  current profile index
C  alphap  : (INPUT)  pressure profile index
C  bt      : (INPUT)  toroidal field on axis (T)
C  eps     : (INPUT)  inverse aspect ratio
C  icurr   : (INPUT)  current scaling model to use
C                     1 = Peng analytic fit
C                     2 = Peng divertor scaling (TART)
C                     3 = simple ITER scaling
C                     4 = revised ITER scaling
C                     5 = Todd empirical scaling I
C                     6 = Todd empirical scaling II
C                     7 = Connor-Hastie model
C  kappa   : (INPUT)  plasma elongation
C  kappa95 : (INPUT)  plasma elongation at 95% surface
C  p0      : (INPUT)  central plasma pressure (Pa)
C  qpsi    : (INPUT)  plasma edge safety factor (= q-bar for icurr=2)
C  rmajor  : (INPUT)  major radius (m)
C  rminor  : (INPUT)  minor radius (m)
C  sf      : (INPUT)  shape factor for icurr=1  (=A/pi in documentation)
C  triang  : (INPUT)  plasma triangularity
C  triang95: (INPUT)  plasma triangularity at 95% surface
C  bp      : (OUTPUT) poloidal field (T)
C  qstar   : (OUTPUT) equivalent cylindrical safety factor (shaped)
C  plascur : (OUTPUT) plasma current (A)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     alphaj,alphap,bp,bt,eps,kappa,kappa95,p0,plascur,qpsi,
     +     qstar,rmajor,rminor,sf,triang,triang95
      INTEGER icurr

C  Local variables
      DOUBLE PRECISION
     +     asp,curhat,fq

C  External functions
      DOUBLE PRECISION bpol,plasc
      EXTERNAL         bpol,plasc

C  External routines
      EXTERNAL conhas

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Check for illegal values of ICURR

      if ((icurr.lt.1).or.(icurr.gt.7)) then
         write(*,*) 'Error in routine CULCUR:'
         write(*,*) 'Illegal value for ICURR, = ',icurr
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Aspect ratio

      asp = 1.0D0/eps

C *** Calculate the function Fq that scales the edge q from the
C *** circular cross-section cylindrical case.

C *** Peng analytical fit

      if (icurr.eq.1) then
         fq = (1.22D0-0.68D0*eps)/((1.0D0-eps*eps)**2) * sf**2
      end if

C *** Peng scaling for double null divertor; TARTs
C+**PJK 29/01/96
      if (icurr.eq.2) then
         curhat = 1.0D6 * plasc(qpsi,asp,rminor,bt,kappa,triang)/bt
      end if

C *** Simple ITER scaling (simply the cylindrical case)

      if (icurr.eq.3) then
         fq = 1.0D0
      end if

C *** ITER formula

      if (icurr.eq.4) then
         fq = 0.5D0 * (1.17D0-0.65D0*eps)/((1.0D0-eps*eps)**2) *
     +        (1.0D0 + kappa95**2 * 
     +        (1.0D0 + 2.0D0*triang95**2 - 1.2D0*triang95**3) )
      end if

C *** Todd empirical scalings :

      if ((icurr.eq.5).or.(icurr.eq.6)) then

         fq = (1.0D0+2.0D0*eps*eps) * 0.5D0*(1.0D0+kappa95**2) *
     +        (1.24D0-0.54D0*kappa95+0.3D0*(kappa95**2 + triang95**2) +
     +        0.125D0*triang95)

         if (icurr.eq.6) then
            fq = fq * (1.0D0 + ( abs(kappa95-1.2D0) )**3)
         end if

      end if

C *** Connor-Hastie asymptotically-correct expression (ICURR=7)

      if (icurr.eq.7) then
         call conhas(alphaj,alphap,bt,triang,eps,kappa,p0,fq)
      end if

C *** Calculate the ratio of plasma current to toroidal field

      if (icurr.ne.2) then
         curhat = 5.0D6 * rminor**2 / (rmajor*qpsi) * fq
      end if

C *** Calculate the equivalent edge safety factor

      qstar = 5.0D6 * rminor**2 / (rmajor*curhat) *
     +     0.5D0 * (1.0D0 + kappa95**2 *
     +     (1.0D0 + 2.0D0*triang95**2 - 1.2D0*triang95**3) )

C *** Calculate plasma current

      plascur = curhat * bt

C *** Calculate the poloidal field

      bp = bpol(qpsi,asp,bt,kappa,triang)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE CONHAS(alphaj,alphap,bt,delta,eps,kappa,p0,fq)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the F coefficient used for scaling the
C  plasma current, using the Connor-Hastie algorithm shown in
C  AEA FUS 172.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 172: Physics Assessment for the European Reactor Study
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  alphaj : (INPUT)  current profile index
C  alphap : (INPUT)  pressure profile index
C  bt     : (INPUT)  toroidal field on axis (T)
C  delta  : (INPUT)  plasma triangularity
C  eps    : (INPUT)  plasma inverse aspect ratio
C  kappa  : (INPUT)  plasma elongation
C  p0     : (INPUT)  central plasma pressure (Pa)
C  fq     : (OUTPUT) scaling for edge q from circular cross-section
C                    cylindrical case.
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION mu0
      PARAMETER(mu0 = 1.2566371D-6)

C  Arguments
      DOUBLE PRECISION
     +     alphaj,alphap,bt,delta,eps,kappa,p0,fq

C  Local variables
      DOUBLE PRECISION
     +     beta0,deltap,deltar,eprime,er,kap1,lambda,lamp1,li,
     +     nu,tprime,tr

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Exponent in Connor-Hastie current profile - matching total
C *** current gives the following trivial relation

      lambda = alphaj

C *** Exponent in Connor-Hastie pressure profile

      nu = alphap

C *** Central plasma beta

      beta0 = 2.0D0 * mu0 * p0 / (bt**2)

C *** Plasma internal inductance

      lamp1 = 1.0D0 + lambda
      li = lamp1/lambda * (lamp1/lambda * log(lamp1) - 1.0D0)

C *** T/r in AEA FUS 172

      kap1 = kappa + 1.0D0
      tr = kappa * delta / kap1**2

C *** E/r in AEA FUS 172

      er = (kappa-1.0D0)/kap1

C *** T primed in AEA FUS 172

      tprime = 2.0D0 * tr * lamp1/(1.0D0+0.5D0*lambda)

C *** E primed in AEA FUS 172

      eprime = er * lamp1/(1.0D0+lambda/3.0D0)

C *** Delta primed in AEA FUS 172

      deltap = 0.5D0*kap1 * eps * 0.5D0*li +
     +     beta0/(0.5D0*kap1*eps) * lamp1**2 / (1.0D0+nu)

C *** Delta/R0 in AEA FUS 172

      deltar = beta0/6.0D0 * (1.0D0 + 5.0D0*lambda/6.0D0 +
     +     0.25D0*lambda**2) + (0.5D0*kap1*eps)**2 *
     +     0.125D0*(1.0D0-(lambda**2)/3.0D0)

C *** F coefficient

      fq = (0.5D0*kap1)**2 *
     +     ( 1.0D0 + eps**2 * (0.5D0*kap1)**2 + 0.5D0*deltap**2 +
     +     2.0D0*deltar + 0.5D0*(eprime**2 + er**2) + 
     +     0.5D0*(tprime**2 + 4.0D0*tr**2) )

      return
      end
C----------------------------------------------------------------------
       SUBROUTINE CULBLM(bt,dnbeta,plascur,rminor,betalim)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This subroutine calculates the Troyon scaling beta limit, using
C  the algorithm documented in AEA FUS 172.
C
C  The limit applies to beta defined with respect to the total B-field.
C  Switch ICULBL determines which components of beta to include (see
C  file eqns.f for coding):
C
C  If ICULBL = 0, then the limit is applied to the total beta
C  If ICULBL = 1, then the limit is applied to the thermal beta only
C  If ICULBL = 2, then the limit is applied to the thermal +
C                 neutral beam beta components
C
C  The default value for the Troyon g coefficient is DNBETA = 3.5
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  bt      : (INPUT)  toroidal B-field on plasma axis (T)
C  dnbeta  : (INPUT)  Troyon g coefficient
C  plascur : (INPUT)  plasma current (A)
C  rminor  : (INPUT)  plasma minor axis (m)
C  betalim : (OUTPUT) beta limit as defined above.
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     betalim,bt,dnbeta,plascur,rminor

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      betalim = 0.01D0 * dnbeta * (plascur/1.0D6) / (rminor*bt)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE BETCOM(alphan,alphat,cfe0,dene,fdeut,ftrit,fhe3,ftr,
     +     ftritbm,idhe3,ignite,impc,impfe,impo,ralpne,rnbeam,te,zeff,
     +     abeam,afuel,aion,deni,dlamee,dlamie,dnalp,dnbeam,dnitot,
     +     dnla,dnprot,dnz,falpe,falpi,pcoef,rncne,rnone,rnfene,zeffai,
     +     zion)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.220
C
C--Description
C  This subroutine determines the various plasma component
C  fractional makeups
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  23 May 2006
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  F/MI/PJK/LOGBOOK11, p.38 for D-He3 deni calculation
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  06/12/95 PJK 1.100 Added D-He3 calculations
C  01/04/98 PJK 1.200 Added calculation of line-averaged density
C                     and effects of IGNITE
C  24/04/98 PJK 1.210 Added IMPC, IMPFE, IMPO impurity multipliers
C  23/05/06 PJK 1.220 Ensured that deni is positive
C
C--Arguments
C  alphan : (INPUT)  density profile index
C  alphat : (INPUT)  temperature profile index
C  cfe0   : (INPUT)  additional iron impurity fraction
C  dene   : (INPUT)  electron density (/m3)
C  fdeut  : (INPUT)  deuterium fraction of D-He3 fuel
C  ftrit  : (INPUT)  tritium fraction of D-He3 fuel
C  fhe3   : (INPUT)  helium-3 fraction of D-He3 fuel
C  ftr    : (INPUT)  tritium fraction of D-T ions
C  ftritbm: (INPUT)  tritium fraction of beam
C  idhe3  : (INPUT)  flag denoting whether to assume D-T or D-He3
C  ignite : (INPUT)  switch for ignited calculation
C  impc   : (INPUT)  carbon impurity multiplier
C  impfe  : (INPUT)  iron impurity multiplier
C  impo   : (INPUT)  oxygen impurity multiplier
C  ralpne : (INPUT)  thermal alpha density / electron density
C  rnbeam : (INPUT)  hot beam density / electron density
C  te     : (INPUT)  electron temperature (keV)
C  abeam  : (OUTPUT) beam ion mass (amu)
C  afuel  : (OUTPUT) average mass of fuel portion of ions (amu)
C  aion   : (OUTPUT) average mass of all ions (amu)
C  deni   : (OUTPUT) fuel ion density (/m3)
C  dlamee : (OUTPUT) electron-electron coulomb logarithm
C  dlamie : (OUTPUT) ion-electron coulomb logarithm
C  dnalp  : (OUTPUT) alpha ash density (/m3)
C  dnbeam : (OUTPUT) hot beam ion density (/m3)
C  dnitot : (OUTPUT) total ion density (/m3)
C  dnla   : (OUTPUT) line-averaged electron density (/m3)
C  dnprot : (OUTPUT) proton ash density (/m3)
C  dnz    : (OUTPUT) high Z ion density (/m3)
C  falpe  : (OUTPUT) fraction of alpha energy to electrons
C  falpi  : (OUTPUT) fraction of alpha energy to ions
C  pcoef  : (OUTPUT) profile factor (= average T / n-weighted T)
C  rncne  : (OUTPUT) carbon density / electron density
C  rnfene : (OUTPUT) iron density / electron density
C  rnone  : (OUTPUT) oxygen density / electron density
C  zeff   : (OUTPUT) plasma effective charge
C  zeffai : (OUTPUT) density weighted plasma effective charge
C  zion   : (OUTPUT) ?
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     abeam,afuel,aion,alphan,alphat,cfe0,dene,deni,dlamee,dlamie,
     +     dnalp,dnbeam,dnitot,dnla,dnprot,dnz,falpe,falpi,fdeut,fhe3,
     +     ftr,ftrit,ftritbm,impc,impfe,impo,pcoef,ralpne,rnbeam,rncne,
     +     rnfene,rnone,te,zeff,zeffai,zion
      INTEGER idhe3,ignite

C  Local variables
      DOUBLE PRECISION
     +     fc,ffe,fo,znfuel

C  External functions
      DOUBLE PRECISION gamfun
      EXTERNAL gamfun

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Profile factor

      pcoef = (1.0D0 + alphan)*(1.0D0 + alphat)/(1.0D0+alphan+alphat)

C *** Line averaged electron density

      dnla = 0.886227D0*dene*
     +     gamfun(alphan+1.0D0) / gamfun(alphan+1.5D0) * (1.0D0+alphan)

C *** Ion density components
C *** ======================

C *** Alpha ash portion

      dnalp = dene * ralpne

C *** Proton ash portion

      if (idhe3.eq.1) dnprot = dnalp

C *** Beam hot ion component
C+**PJK 01/04/98 If ignited, prevent beam fusion effects

      if (ignite.eq.0) then
         dnbeam = dene * rnbeam
      else
         dnbeam = 0.0D0
      end if

C *** Carbon portion

      fc = impc * (0.009D0 + 0.006D0 * (7.0D19/dene)**2.6D0)
      rncne = fc

C *** Oxygen portion

      fo = impo * 0.001D0
      rnone = fo

C *** Iron portion

      ffe = impfe * (0.0005D0 * (7.0D19/dene)**2.3D0 + cfe0)
      rnfene = ffe

C *** Fuel portion - conserve charge neutrality

      if (idhe3.eq.0) then

         deni = dene - 2.0D0*dnalp - dnprot - dnbeam -
     +        dene*(6.0D0*fc + 8.0D0*fo + 26.0D0*ffe)

      else

C *** znfuel is the sum of Zi.ni for the three fuel ions

         znfuel = dene - 2.0D0*dnalp - dnprot - dnbeam -
     +        dene*(6.0D0*fc + 8.0D0*fo + 26.0D0*ffe)

         deni = znfuel/(1.0D0+fhe3)

      end if

C+**PJK 23/05/06 Ensure that deni is never negative or zero

      deni = max(deni,1.0D0)

C *** Total ion density

      dnz = dene * (fc + fo + ffe)
      dnitot = deni + dnz + dnalp + dnprot + dnbeam

C *** Effective charge

      if (idhe3.eq.0) then
         zeff = (deni + dnbeam)/dene + 4.0D0*ralpne + 36.0D0*fc +
     +        64.0D0*fo + 676.0D0*ffe
      else
         zeff = (fdeut + ftrit)*deni/dene + 4.0D0*fhe3*deni/dene +
     +        dnbeam/dene + 4.0D0*ralpne + dnprot/dene + 36.0D0*fc +
     +        64.0D0*fo + 676.0D0*ffe
      end if

C *** Define coulomb logarithm
C *** (collisions: ion-electron, electron-electron)

      dlamee = 31.0D0 - (log(dene)/2.0D0) + log(te*1000.0D0)
      dlamie = 31.3D0 - (log(dene)/2.0D0) + log(te*1000.0D0)

C *** Fraction of alpha energy to ions and electrons
C *** From Max Fenstermacher
C *** (used with electron and ion power balance equations only)
C *** No consideration of pcharge here...

      falpe = 0.88155D0 * exp(-te*pcoef/67.4036D0)
      falpi = 1.0D0 - falpe

C *** Average atomic masses

      if (idhe3.eq.0) then
         if (ftr .lt. 1.0D-3) then
            afuel = 1.0D0
            abeam = 1.0D0
         else
            afuel = 2.0D0 * (1.0D0-ftr)     + 3.0D0 * ftr
            abeam = 2.0D0 * (1.0D0-ftritbm) + 3.0D0 * ftritbm
         end if
      else
         afuel = 2.0D0*fdeut + 3.0D0*ftrit + 3.0D0*fhe3
         abeam = 2.0D0 * (1.0D0-ftritbm) + 3.0D0 * ftritbm
      end if

C *** Density weighted masses and charges

      if (idhe3.eq.0) then

C *** aion = sum over ions: A_i . n_i / total ion density

         aion = ( afuel*deni + 4.0D0*dnalp + dnprot + abeam*dnbeam +
     +        dene*(6.0D0*fc + 8.0D0*fo + 28.0D0*ffe) )/ dnitot

C+**PJK 15/01/96 Possibly more correct...
C+**PJK 15/01/96 aion = ( afuel*deni + 4.0D0*dnalp + dnprot +
C+**PJK 15/01/96  +  abeam*dnbeam + dene*(12.0D0*fc + 16.0D0*fo +
C+**PJK 15/01/96  +  56.0D0*ffe) )/ dnitot

C *** zion = sum over ions: Z_i . n_i / total ion density

         zion = ( deni + 2.0D0*dnalp + dnprot + dnbeam +
     +        dene*(6.0D0*fc + 8.0D0*fo + 26.0D0*ffe) )/dnitot

C *** zeffai = sum over ions: (Z_i)**2 . n_i / (A_i . n_e)

         zeffai = ( deni/afuel + dnalp + dnprot + dnbeam/abeam +
     +        dene*(6.0D0*fc + 8.0D0*fo + 24.0D0*ffe) ) / dene

C+**PJK 15/01/96 Possibly more correct...
C+**PJK 15/01/96 zeffai = ( (1.0D0-ftr)*deni/2.0D0 + ftr*deni/3.0D0 +
C+**PJK 15/01/96  +  dnalp + dnprot + (1.0D0-ftritbm)*dnbeam/2.0D0 +
C+**PJK 15/01/96  +  ftritbm*dnbeam/3.0D0 + dene*(3.0D0*fc + 4.0D0*fo +
C+**PJK 15/01/96  +  12.0D0*ffe) )/dene

      else

         aion = ( afuel*deni + 4.0D0*dnalp + dnprot + abeam*dnbeam +
     +        dene*(12.0D0*fc + 16.0D0*fo + 56.0D0*ffe) )/ dnitot

         zion = ( fdeut*deni + ftrit*deni + 2.0D0*fhe3*deni +
     +        2.0D0*dnalp + dnprot + dnbeam + dene *
     +        (6.0D0*fc + 8.0D0*fo + 26.0D0*ffe) )/dnitot

         zeffai = ( fdeut*deni/2.0D0 + ftrit*deni/3.0D0 +
     +        4.0D0*fhe3*deni/3.0D0 + dnalp + dnprot +
     +        (1.0D0-ftritbm)*dnbeam/2.0D0 + ftritbm*dnbeam/3.0D0 +
     +        dene*(3.0D0*fc + 4.0D0*fo + 12.0D0*ffe) )/dene

      end if

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE DENLIM(bt,rmajor,prn1,pdiv,q,sarea,dnelimt)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Borrass density limit model
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  bt     : (INPUT)  toroidal field on axis (T)
C  rmajor : (INPUT)  major radius (m)
C  prn1   : (INPUT)  edge density / average plasma density
C  pdiv   : (INPUT)  power flowing to the edge plasma (MW)
C  q      : (INPUT)  safety factor at 95% surface
C  sarea  : (INPUT)  plasma surface area (m**2)
C  dnelimt: (OUTPUT) average plasma density limit (m**-3)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     bt,dnelimt,rmajor,prn1,pdiv,q,sarea

C  Local variables
      DOUBLE PRECISION
     +     denslim,qparll

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      qparll = pdiv/sarea

      denslim = 0.5D20 * qparll**0.57D0 * bt**0.31D0 /
     +     (q*rmajor)**0.1D0

      dnelimt = denslim/prn1

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE CULDLM(bt,idensl,pdivt,pi,plascur,prn1,qcyl,q95,
     +     rmajor,rminor,sarea,zeff,dlimit,dnelimt)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to calculate several different formulae for the
C  density limit, and enforce the one chosen by the user.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  17 September 1997
C
C--Reference
C  AEA FUS 172: Physics Assessment for the European Reactor Study
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  17/09/97 PJK 1.100 Added Greenwald limit (idensl=7)
C
C--Arguments
C  bt       : (INPUT)  toroidal field on axis (T)
C  idensl   : (INPUT)  switch denoting which formula to enforce
C  pdivt    : (INPUT)  power flowing to the edge plasma (MW)
C  pi       : (INPUT)  3.14...
C  plascur  : (INPUT)  plasma current (A)
C  prn1     : (INPUT)  edge density / average plasma density
C  qcyl     : (INPUT)  equivalent cylindrical safety factor (qstar)
C  q95      : (INPUT)  safety factor at 95% surface
C  rmajor   : (INPUT)  plasma major radius (m)
C  rminor   : (INPUT)  plasma minor radius (m)
C  sarea    : (INPUT)  plasma surface area (m**2)
C  zeff     : (INPUT)  plasma effective charge
C  dlimit(7): (OUTPUT) average plasma density limit using
C                      seven different models (m**-3)
C  dnelimt  : (OUTPUT) enforced average plasma density limit (m**-3)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION bt,dlimit(7),dnelimt,pdivt,pi,plascur,prn1,
     +     q95,qcyl,rmajor,rminor,sarea,zeff
      INTEGER idensl

C  Local variables
      DOUBLE PRECISION denom,dlim,qperp
      INTEGER i

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Check for illegal values of IDENSL

      if ((idensl.lt.1).or.(idensl.gt.7)) then
         write(*,*) 'Error in routine CULDLM:'
         write(*,*) 'Illegal value for IDENSL, = ',idensl
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Initialise DLIMIT array

      do 10 i = 1,7
         dlimit(i) = 0.0D0
 10   continue

C *** Power per unit area crossing the plasma edge

      qperp = pdivt/sarea

C *** Old ASDEX density limit formula.
C *** This applies to the density at the plasma edge, so must be scaled
C *** to give the density limit applying to the average plasma density.

      dlim = 1.54D20 * qperp**0.43D0 * bt**0.31D0 /(q95*rmajor)**0.45D0
      dlimit(1) = dlim/prn1

C *** Borrass density limit model for ITER (I).
C *** This applies to the density at the plasma edge, so must be scaled
C *** to give the density limit applying to the average plasma density.

      dlim = 1.8D20 * qperp**0.53D0 * bt**0.31D0 /(q95*rmajor)**0.22D0
      dlimit(2) = dlim/prn1

C *** Borrass density limit model for ITER (II).
C *** This applies to the density at the plasma edge, so must be scaled
C *** to give the density limit applying to the average plasma density.

      dlim = 0.5D20 * qperp**0.57D0 * bt**0.31D0 /(q95*rmajor)**0.09D0
      dlimit(3) = dlim/prn1

C *** JET edge radiation density limit model.
C *** This applies to the density at the plasma edge, so must be scaled
C *** to give the density limit applying to the average plasma density.
C *** qcyl=qstar here, but literature is not clear.

      denom = (zeff-1.0D0)*( 1.0D0-4.0D0/(3.0D0*qcyl) )
      if (denom.le.0.0D0) then
         if (idensl.eq.4) then
            write(*,*) 'Warning in routine CULDLM:'
            write(*,*) 'DENOM = ',denom,' i.e. qcyl < 4/3'
            write(*,*) 'Floating point error will occur.'
            write(*,*) 'DLIMIT(4) is artificially set to 0.0.'
            write(*,*) 'Model 5 will be used as the density limit.'
            write(*,*) 'PROCESS continuing.'
            idensl = 5
         end if
         dlimit(4) = 0.0D0
         goto 20
      end if

      dlim = 1.0D20 * sqrt(pdivt/denom)
      dlimit(4) = dlim/prn1

 20   continue

C *** JET simplified density limit model.
C *** This applies to the density at the plasma edge, so must be scaled
C *** to give the density limit applying to the average plasma density.

      dlim = 0.237D20 * bt * sqrt(pdivt)/rmajor
      dlimit(5) = dlim/prn1

C *** Hugill-Murakami M.q limit.
C *** qcyl=qstar here, which is okay according to the literature

      dlimit(6) = 3.0D20 * bt / (rmajor*qcyl)

C *** Greenwald limit.

      dlimit(7) = 1.0D14 * plascur/(pi*rminor*rminor)

C *** Enforce the chosen density limit

      dnelimt = dlimit(idensl)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE PALPH(alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit,
     +     idhe3,iiter,pcoef,pi,ti,palp,pcharge,pneut,sigvdt)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  This subroutine numerically integrates over plasma cross-section to
C  find the fusion power and fast alpha pressure
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  06 December 1995
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  06/12/95 PJK 1.100 Added D-He3 calculations
C
C--Arguments
C  alphan  : (INPUT)  density profile index
C  alphat  : (INPUT)  temperature profile index
C  deni    : (INPUT)  fuel ion density (/m3)
C  ealpha  : (INPUT)  alpha particle birth energy (D-T) (keV)
C  fdeut   : (INPUT)  deuterium fuel fraction
C  fhe3    : (INPUT)  helium-3 fuel fraction
C  ftr     : (INPUT)  tritium fraction of D-T ions
C  ftrit   : (INPUT)  tritium fuel fraction
C  idhe3   : (INPUT)  switch for D-T or D-He3 calculations
C  iiter   : (INPUT)  switch for ITER fusion power calculations
C  pcoef   : (INPUT)  profile factor (= average T / n-weighted T)
C  pi      : (INPUT)  3.14...
C  ti      : (INPUT)  ion temperature (keV)
C  palp    : (OUTPUT) alpha particle fusion power (MW/m3)
C  pcharge : (OUTPUT) other charged particle fusion power (MW/m3)
C  pneut   : (OUTPUT) neutron fusion power (MW/m3)
C  sigvdt  : (OUTPUT) profile averaged <sigma v DT> (m3/s)
C
C--Global variables passed via COMMON
C  alphand: (OUTPUT) density profile index
C  alphatd: (OUTPUT) temperature profile index
C  tidum  : (OUTPUT) ion temperature (keV)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit,palp,pcharge,
     +     pneut,pcoef,pi,sigvdt,ti
      INTEGER idhe3,iiter

C  Global variables
      DOUBLE PRECISION alphatd,alphand,tidum
      COMMON/fintcom/tidum,alphatd,alphand

C  Local variables
      DOUBLE PRECISION
     +     alow,bhigh,ealphaj,epsq8,errest,flag,ft,rint,svdt,tn
      DOUBLE PRECISION pa,pc,pn,sigmav
      INTEGER iopt,nofun

C  External functions
      DOUBLE PRECISION fint
      EXTERNAL         fint

C  External routines
      EXTERNAL ffus,fpower,quanc8

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Initialise local quantities

      alow = 0.0D0
      bhigh = 0.999D0
      epsq8 = 1.0D-9

      ealphaj = ealpha * 1.6022D-16

C *** Initialise global quantities

      tidum = ti
      alphatd = alphat
      alphand = alphan

C *** Find fusion power

      if (idhe3.eq.0) then

         if (iiter.ne.1) then

C ***       Integrate over plasma profiles to obtain
C ***       fusion reaction rate

            call quanc8(fint,alow,bhigh,epsq8,epsq8,rint,errest,nofun,
     +           flag)
            sigvdt = 2.0D0 * (1.0D0 + alphan)**2.0D0 * rint

         else

C ***       Use analytical formula for fusion reaction rate

            tn = ti * pcoef
            call ffus(tn,alphan,alphat,ft,svdt)
            sigvdt = svdt * ft

         end if

         palp = sigvdt * ftr*(1.0D0-ftr) * ealphaj * 1.0D-6 * deni**2
         pcharge = 0.0D0
         pneut = 4.0D0 * palp

      else

C ***    D-He3 option

         palp = 0.0D0
         pcharge = 0.0D0
         pneut = 0.0D0

C *** Sum over the four reactions: D-T, D-He3, first D-D, second D-D
C *** No profile averaging yet...

         do 10 iopt = 1,4
            call fpower(ftrit,fdeut,fhe3,deni,ti,pi,iopt,pa,pc,pn,
     +           sigmav)
            palp = palp + pa
            pcharge = pcharge + pc
            pneut = pneut + pn
            if (iopt.eq.1) sigvdt = sigmav
 10      continue

      end if

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE PALPH2(bt,bp,dene,deni,dnitot,ftr,falpe,falpi,
     +     palpnb,ifalphap,pcharge,pcoef,pneut,te,ti,vol,alpmw,betaft,
     +     palp,palpi,palpe,pfuscmw,powfmw)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.200
C
C--Description
C  Add neutral beam fusion power to alpha power. This routine used to
C  be an ENTRY point into routine PALPH.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 May 2006
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  06/12/95 PJK 1.100 Added D-He3 calculations
C  22/05/06 PJK 1.200 Added modified fit to fast alpha pressure
C
C--Arguments
C  bp       : (INPUT)  poloidal field (T)
C  bt       : (INPUT)  toroidal field on axis (T)
C  dene     : (INPUT)  electron density (/m3)
C  deni     : (INPUT)  fuel ion density (/m3)
C  dnitot   : (INPUT)  total ion density (/m3)
C  falpe    : (INPUT)  fraction of alpha energy to electrons
C  falpi    : (INPUT)  fraction of alpha energy to ions
C  ftr      : (INPUT)  tritium D-T fraction
C  ifalphap : (INPUT)  switch for fast alpha pressure method
C  palpnb   : (INPUT)  alpha power from hot neutral beam ions (MW)
C  pcharge  : (INPUT)  other charged particle fusion power (MW/m3)
C  pcoef    : (INPUT)  profile factor ( = average T / n-weighted T )
C  pneut    : (IN/OUT) neutron fusion power (MW/m3)
C  te       : (INPUT)  electron temperature (keV)
C  ti       : (INPUT)  ion temperature (keV)
C  vol      : (INPUT)  plasma volume (m3)
C  alpmw    : (OUTPUT) alpha power (MW)
C  betaft   : (OUTPUT) fast alpha beta component
C  palp     : (IN/OUT) alpha power per volume (MW/m3)
C  palpe    : (OUTPUT) alpha power per volume to electrons (MW/m3)
C  palpi    : (OUTPUT) alpha power per volume to ions (MW/m3)
C  pfuscmw  : (OUTPUT) charged particle fusion power (MW)
C  powfmw   : (OUTPUT) fusion power (MW)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     alpmw,betaft,bp,bt,dene,deni,dnitot,falpe,falpi,ftr,palp,
     +     palpe,palpi,palpnb,pcharge,pcoef,pfuscmw,pneut,powfmw,te,
     +     ti,vol

      INTEGER ifalphap

C  Local variables
      DOUBLE PRECISION
     +     betath,fact,fact2

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Add neutral beam alpha power / volume

      palp = palp + palpnb/vol

C *** Add extra neutron power

      pneut = pneut + 4.0D0*palpnb/vol

C *** Total alpha power

      alpmw = palp*vol

C *** Total fusion power

      powfmw = alpmw + (pneut + pcharge)*vol

C *** Charged particle fusion power

      pfuscmw = alpmw + pcharge*vol

C *** Power to electrons and ions (used with electron and ion
C *** power balance equations only)
C *** No consideration of pcharge here...

      palpi = palp*falpi
      palpe = palp*falpe

C *** Determine average fast alpha density

      betath = 4.021D-22 * pcoef * (dene*te + dnitot*ti)/(bt**2 + bp**2)

      if (ifalphap.eq.0) then

C *** N Uckan fast alpha scaling

         fact = MIN( 0.30D0,
     +        0.29D0*(deni/dene)**2 * ( pcoef*(te+ti)/20.0D0 - 0.37D0) )

      else

C *** Modified scaling, D J Ward, April 2006

         fact = MIN( 0.30D0,
     +        0.26D0*(deni/dene)**2 * 
     +        sqrt( MAX( 0.0D0, (pcoef*(te+ti)/20.0D0 - 0.65D0) ) ) )

      end if

      fact = MAX(fact,0.0D0)
      if (ftr.lt.1.0D-3) fact = 0.0D0

      fact2 = palp/(palp-(palpnb/vol))
      betaft = betath * fact*fact2

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE FFUS(t,alphan,alphat,ft,sigv)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Calculation of D-T fusion reaction rate
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  alphan : (INPUT)  density profile index
C  alphat : (INPUT)  temperature profile index
C  t      : (INPUT)  weighted ion temperature (keV)
C  ft     : (OUTPUT) ?
C  sigv   : (OUTPUT) fusion reaction rate
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION alphan,alphat,ft,sigv,t

C  Local variables
      DOUBLE PRECISION c1,c2,y

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      c1 = -0.395D0 + 1.128D0*alphan + 3.777D0*alphat -
     +     1.022D0*alphat*alphan

      c2 = 0.009D0 - 0.023D0*alphan - 0.385D0*alphat +
     +     0.15D0*alphan*alphat

      ft = c1 * t**c2

      y = log(t)
      sigv = 0.977D-22* exp(0.0382D0*y**3 - 1.007D0*y**2 +
     +     6.398D0*y - 9.75D0)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE PCOND(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hfact,
     +     iinvqd,isc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji,
     +     plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q,qstar,vol,
     +     xarea,zeff,ptre,ptri,tauee,tauei,taueff,powerht)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.720
C
C--Description
C  This subroutine calculates the confinement times and
C  the transport power loss terms.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  23 May 2006
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  30/06/94 PJK 1.100 Added stellarator scaling laws 20-23
C  07/12/95 PJK 1.200 Added pcharge to plasma input power
C  06/03/96 PJK 1.300 Added RFP scaling law
C  14/11/97 PJK 1.400 Added ITER-97 scaling laws (26,27)
C  01/04/98 PJK 1.500 Added ITER-96P scaling law (28) and moved
C                     calculation of dnla into BETCOM instead
C  26/06/98 PJK 1.600 Added scaling laws 29,30,31
C  08/10/98 PJK 1.700 Added scaling laws 32,33,34,35,36
C  16/07/01 PJK 1.710 Added KAPPAA to argument list
C  23/05/06 PJK 1.720 Ensured that powerht is always positive
C
C--Arguments
C  afuel  : (INPUT)  average mass of fuel (amu)
C  alpmw  : (INPUT)  alpha particle power (MW)
C  aspect : (INPUT)  aspect ratio
C  bt     : (INPUT)  toroidal field on axis (T)
C  dene   : (INPUT)  volume averaged electron density (m**-3)
C  dnitot : (INPUT)  total ion density (m**-3)
C  dnla   : (INPUT)  line-averaged electron density (m**-3)
C  eps    : (INPUT)  inverse aspect ratio
C  hfact  : (INPUT)  H factor on energy confinement scalings
C  iinvqd : (INPUT)  switch for inverse quadrature
C  isc    : (INPUT)  switch for energy confinement scaling to use
C  ignite : (INPUT)  switch for ignited calculation
C  kappa  : (INPUT)  plasma elongation
C  kappa95: (INPUT)  plasma elongation at 95% surface
C  kappaa:  (OUTPUT) plasma elongation calculated using area ratio
C  pcharge: (INPUT)  non-alpha charged particle fusion power (MW/m3)
C  pinje  : (INPUT)  auxiliary power to electrons (W)
C  pinji  : (INPUT)  auxiliary power to ions (W)
C  plascur: (INPUT)  plasma current (A)
C  pohmpv : (INPUT)  ohmic heating per unit volume (MW/m**3)
C  prad   : (INPUT)  total core radiation power (MW/m**3)
C  q      : (INPUT)  edge safety factor
C  qstar  : (INPUT)  equivalent cylindrical edge safety factor
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  te     : (INPUT)  average electron temperature (keV)
C  ten    : (INPUT)  density weighted average electron temperature (keV)
C  tin    : (INPUT)  density weighted average ion temperature (keV)
C  vol    : (INPUT)  plasma volume (m**3)
C  xarea  : (INPUT)  plasma cross-sectional area (m**2)
C  zeff   : (INPUT)  plasma effective charge
C  ptre   : (OUTPUT) electron transport power (MW/m**3)
C  ptri   : (OUTPUT) ion transport power (MW/m**3)
C  tauee  : (OUTPUT) electron energy confinement time (s)
C  taueff : (OUTPUT) global energy confinement time (s)
C  tauei  : (OUTPUT) ion energy confinement time (s)
C  powerht: (OUTPUT) heating power (MW) assumed in calculation
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'start.h'

C  Arguments
      DOUBLE PRECISION
     +     afuel,alpmw,aspect,bt,dene,dnitot,dnla,eps,hfact,kappa,
     +     kappa95,pcharge,pinje,pinji,plascur,pohmpv,powerht,prad,
     +     ptre,ptri,q,qstar,rmajor,rminor,tauee,taueff,tauei,te,ten,
     +     tin,vol,xarea,zeff,kappaa
      INTEGER iinvqd,isc,ignite

C  Local variables
      DOUBLE PRECISION
     +     chii,ck2,denfac,dnla19,dnla20,eps2,gjaeri,n20,pcur,
     +     pi,qhat,ratio,rll,str2,str5,taueena,tauit1,tauit2,term1,term2

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      pi = 3.141592653589793D0

C *** Neoclassical ion transport loss.
C *** Calculate ion energy confinement time

      eps2 = eps/2.0D0
      str5 = (2.0D0/(1.0D0+(kappa**2)))
      ck2 = (0.66D0+(1.88D0*(sqrt(eps2)))-(1.54D0*eps2))*
     +     (1.0D0+(1.5D0*(eps2**2)))
      chii = (6.5D-22)*ck2*zeff*(aspect**1.5D0)*dene*(q**2)*str5/
     +     ((sqrt(tin))*(bt**2))
      str2 = (2.0D0*(kappa**2)/(1.0D0+(kappa**2)))
      tauei = 0.375D0*rminor**2/chii*str2

C *** Calculate heating power (MW)
C *** If necessary, assume that the device is ignited, so exclude
C *** auxiliary power injected

      if (ignite.eq.0) then
         powerht = alpmw + pcharge*vol + (pinje+pinji)/1.0D6
     +        + pohmpv*vol - prad*vol
      else
         powerht = alpmw + pcharge*vol + pohmpv*vol - prad*vol
      end if

C+**PJK 23/05/06 Ensure heating power is positive

      powerht = max(powerht,1.0D-3)

C *** Line averaged electron density in scaled units

      dnla20 = dnla * 1.0D-20
      dnla19 = dnla * 1.0D-19

C *** Volume averaged electron density in units of 10**20 m**-3

      n20 = dene / 1.0D20

C *** Plasma current in MA

      pcur = plascur / 1.0D6

C *** kappaa = plasma X-sectional area/(pi*rminor*rminor) by definition

      kappaa = xarea/(pi*rminor*rminor)

C *** Calculate Neo-Alcator confinement time (used in several scalings)

      taueena = 0.07D0 * n20 * rminor * rmajor*rmajor * qstar

C *** Check that ISC lies within legal range

      if ((isc.lt.1).or.(isc.gt.ipnlaws)) then
         write(*,*) 'Error in routine PCOND:'
         write(*,*) 'Illegal value for ISC, = ',isc
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Neo-Alcator scaling (ohmic)

      if (isc.eq.1) then
         tauee = taueena
         gtaue = 0.0D0
         ptaue = 1.0D0
         qtaue = 0.0D0
         rtaue = 0.0D0
      end if

C *** Mirnov scaling (H-mode)

      if (isc.eq.2) then
         tauee = hfact * 0.2D0 * rminor * sqrt(kappa95) * pcur
         gtaue = 0.0D0
         ptaue = 0.0D0
         qtaue = 0.0D0
         rtaue = 0.0D0
      end if

C *** Merezhkin-Muhkovatov scaling (L-mode)
C+**PJK 29/06/92 Corrected te to ten in Merez-Muhkov scaling law
      if (isc.eq.3) then
         tauee = hfact * 3.5D-3 * rmajor**2.75D0 * rminor**0.25D0 *
     +        kappa95**0.125D0 * qstar * dnla20 * sqrt(afuel) /
     +        sqrt(ten/10.0D0)
         gtaue = 0.0D0
         ptaue = 1.0D0
         qtaue = -0.5D0
         rtaue = 0.0D0
      end if

C *** Shimomura scaling (H-mode)

      if (isc.eq.4) then
         tauee = hfact * 0.045D0 * rmajor * rminor * bt *
     +        sqrt(kappa95) * sqrt(afuel)
         gtaue = 0.0D0
         ptaue = 0.0D0
         qtaue = 0.0D0
         rtaue = 0.0D0
      end if

C *** Kaye-Goldston scaling (L-mode)

      if (isc.eq.5) then
         tauee = hfact * 0.055D0 * kappa95**0.28D0 * pcur**1.24D0 *
     +        n20**0.26D0 * rmajor**1.65D0 * sqrt(afuel/1.5D0) /
     +        ( bt**0.09D0 * rminor**0.49D0 * powerht**0.58D0 )
         gtaue = 0.0D0
         ptaue = 0.26D0
         qtaue = 0.0D0
         rtaue = -0.58D0
         if (iinvqd.ne.0) tauee = 1.0D0/ sqrt( 1.0D0/taueena**2 +
     +        1.0D0/tauee**2)
      end if

C *** ITER Power scaling - ITER 89-P (L-mode)

      if ((isc.eq.6).or.(isc.eq.14)) then
         tauee = hfact * 0.048D0 * pcur**0.85D0 * rmajor**1.2D0 *
     +        rminor**0.3D0 * sqrt(kappa) * dnla20**0.1D0 * bt**0.2D0 *
     +        sqrt(afuel) / sqrt(powerht)
         tauit1 = tauee
         gtaue = 0.0D0
         ptaue = 0.1D0
         qtaue = 0.0D0
         rtaue = -0.5D0
      end if

C *** ITER Offset linear scaling - ITER 89-O (L-mode)
C+**PJK 30/06/92 Corrected exponent on kappa in term2 from 0.2 to 0.5

      if ((isc.eq.7).or.(isc.eq.14)) then
         term1 = 0.04D0 * pcur**0.5D0 * rmajor**0.3D0 *
     +        rminor**0.8D0 * kappa**0.6D0 * afuel**0.5D0
         term2 = 0.064D0 * pcur**0.8D0 * rmajor**1.6D0 *
     +        rminor**0.6D0 * kappa**0.5D0 * dnla20**0.6D0 *
     +        bt**0.35D0 * afuel**0.2D0 / powerht
         tauee = hfact * (term1 + term2)
         tauit2 = tauee
         gtaue = hfact*term1
         ptaue = 0.6D0
         qtaue = 0.0D0
         rtaue = -1.0D0
         if (isc.eq.14) then
            tauee = min(tauit1,tauit2)
            if (tauit1.lt.tauit2) then
               gtaue = 0.0D0
               ptaue = 0.1D0
               qtaue = 0.0D0
               rtaue = -0.5D0
            end if
         end if
      end if

C *** Rebut-Lallia offset linear scaling (L-mode)

C+**PJK 10/06/92 I do not understand the additional factor of 1/2.
C+**PJK 30/06/92 Therefore I have removed it (suggestion of Tim Hender).

      if (isc.eq.8) then
         rll = (rminor**2 * rmajor * kappa95)**0.333D0
         tauee = hfact * 1.65D0 * sqrt(afuel/2.0D0) * (
     +        1.2D-2 * pcur * rll**1.5D0 / sqrt(zeff) +
     +        0.146D0 * dnla20**0.75D0 * sqrt(pcur) * sqrt(bt) *
     +        rll**2.75D0 * zeff**0.25D0 /powerht )
         gtaue = hfact * 1.65D0 * sqrt(afuel/2.0D0) * (
     +        1.2D-2 * pcur * rll**1.5D0 / sqrt(zeff) )
         ptaue = 0.75D0
         qtaue = 0.0D0
         rtaue = -1.0D0
      end if

C *** Goldston scaling (L-mode)

      if (isc.eq.9) then
         tauee = hfact * 0.037D0 * pcur * rmajor**1.75D0 *
     +        rminor**(-0.37D0) * sqrt(kappa95) * sqrt(afuel/1.5D0) /
     +        sqrt(powerht)
         gtaue = 0.0D0
         ptaue = 0.0D0
         qtaue = 0.0D0
         rtaue = -0.5D0
         if (iinvqd.ne.0) tauee = 1.0D0/
     +        sqrt( 1.0D0/taueena**2 + 1.0D0/tauee**2)
      end if

C *** T10 scaling

      if (isc.eq.10) then
         denfac = dnla20 * rmajor * qstar / (1.3D0*bt)
         denfac = min(1.0D0,denfac)
         tauee = hfact * 0.095D0 * rmajor * rminor * bt *
     +        sqrt(kappa95) * denfac / powerht**0.4D0 *
     +        ( zeff**2 * pcur**4 /
     +        (rmajor * rminor * qstar**3 * kappa95**1.5D0) )**0.08D0
         gtaue = 0.0D0
         ptaue = 1.0D0
         qtaue = 0.0D0
         rtaue = -0.4D0
      end if

C *** JAERI scaling

      if (isc.eq.11) then
         gjaeri = zeff**0.4D0 * ((15.0D0-zeff)/20.0D0)**0.6D0 *
     +        (3.0D0 * qstar * (qstar+5.0D0) / ((qstar+2.0D0) *
     +        (qstar+7.0D0)))**0.6D0
         tauee = hfact * (0.085D0 * kappa95 * rminor**2 * sqrt(afuel) +
     +        0.069D0 * n20**0.6D0 * pcur * bt**0.2D0 * rminor**0.4D0 *
     +        rmajor**1.6D0 * sqrt(afuel) * gjaeri * kappa95**0.2D0 /
     +        powerht)
         gtaue = hfact * 0.085D0 * kappa95 * rminor**2 * sqrt(afuel)
         ptaue = 0.6D0
         qtaue = 0.0D0
         rtaue = -1.0D0
      end if

C *** Kaye-Big scaling

      if (isc.eq.12) then
         tauee = hfact * 0.105D0 * sqrt(rmajor) * rminor**0.8D0 *
     +        bt**0.3D0 * kappa95**0.25D0 * pcur**0.85D0 *
     +        n20**0.1D0 * afuel**0.5D0 / powerht**0.5D0
         gtaue = 0.0D0
         ptaue = 0.1D0
         qtaue = 0.0D0
         rtaue = -0.5D0
      end if

C *** ITER H-mode scaling - ITER H90-P

      if (isc.eq.13) then
         tauee = hfact * 0.064D0 * pcur**0.87D0 * rmajor**1.82D0 *
     +        rminor**(-0.12D0) * kappa**0.35D0 * dnla20**0.09D0 *
     +        bt**0.15D0 * sqrt(afuel) / sqrt(powerht)
         gtaue = 0.0D0
         ptaue = 0.09D0
         qtaue = 0.0D0
         rtaue = -0.5D0
      end if

C *** Riedel scaling (L-mode)

      if (isc.eq.15) then
         tauee = hfact * 0.044D0 * pcur**0.93D0 * rmajor**1.37D0 *
     +        rminor**(-0.049D0) * kappa95**0.588D0 * dnla20**0.078D0 *
     +        bt**0.152D0 / powerht**0.537D0
         gtaue = 0.0D0
         ptaue = 0.078D0
         qtaue = 0.0D0
         rtaue = -0.537D0
      end if

C *** Christiansen et al scaling (L-mode)

      if (isc.eq.16) then
         tauee = hfact * 0.24D0 * pcur**0.79D0 * rmajor**0.56D0 *
     +        rminor**1.46D0 * kappa95**0.73D0 * dnla20**0.41D0 *
     +        bt**0.29D0 / (powerht**0.79D0 * afuel**0.02D0)
         gtaue = 0.0D0
         ptaue = 0.41D0
         qtaue = 0.0D0
         rtaue = -0.79D0
      end if

C *** Lackner-Gottardi scaling (L-mode)

      if (isc.eq.17) then
         qhat = (1.0D0+kappa95**2) * rminor**2 * bt /
     +        (0.4D0 * pcur * rmajor)
         tauee = hfact * 0.12D0 * pcur**0.8D0 * rmajor**1.8D0 *
     +        rminor**0.4D0 * kappa95 * (1.0D0+kappa95)**(-0.8D0) *
     +        dnla20**0.6D0 * qhat**0.4D0 / powerht**0.6D0
         gtaue = 0.0D0
         ptaue = 0.6D0
         qtaue = 0.0D0
         rtaue = -0.6D0
      end if

C *** Neo-Kaye scaling (L-mode)
C+**PJK 29/06/92 Added missing afuel term to Neo-Kaye scaling law

      if (isc.eq.18) then
         tauee = hfact * 0.063D0 * pcur**1.12D0 * rmajor**1.3D0 *
     +        rminor**(-0.04D0) * kappa95**0.28D0 * dnla20**0.14D0 *
     +        bt**0.04D0 * sqrt(afuel) / powerht**0.59D0
         gtaue = 0.0D0
         ptaue = 0.14D0
         qtaue = 0.0D0
         rtaue = -0.59D0
      end if

C *** Riedel scaling (H-mode)

      if (isc.eq.19) then
         tauee = hfact * 0.1D0 * sqrt(afuel) * pcur**0.884D0 *
     +        rmajor**1.24D0 * rminor**(-0.23D0) * kappa95**0.317D0 *
     +        bt**0.207D0 * dnla20**0.105D0 / powerht**0.486D0
         gtaue = 0.0D0
         ptaue = 0.105D0
         qtaue = 0.0D0
         rtaue = -0.486D0
      end if

C+**PJK 17/12/92 Added amended version of ITER H90-P law.
C+**PJK 17/12/92 Seems to bear no relation to the original...(isc=13)
C+**PJK 17/12/92 From Nuclear Fusion 32 (1992) 318.

      if (isc.eq.20) then
         tauee = hfact * 0.082D0 * pcur**1.02D0 *
     +        bt**0.15D0 * sqrt(afuel) * rmajor**1.60D0 /
     +        (powerht**0.47D0 * kappa**0.19D0)
         gtaue = 0.0D0
         ptaue = 0.0D0
         qtaue = 0.0D0
         rtaue = -0.47D0
      end if

C+**PJK 30/06/94 Added three new scaling laws, relevant to stellarators

C *** Large Helical Device scaling
C *** S.Sudo, Y.Takeiri, H.Zushi et al., Nuclear Fusion, 30, p.11 (1990)

      if (isc.eq.21) then
         tauee = hfact * 0.17D0 * rmajor**0.75D0 * rminor**2 *
     +        dnla20**0.69D0 * bt**0.84D0 * powerht**(-0.58D0)
         gtaue = 0.0D0
         ptaue = 0.69D0
         qtaue = 0.0D0
         rtaue = 0.58D0
      end if

C *** Gyro-reduced Bohm scaling
C *** R.J.Goldston, H.Biglari, G.W.Hammett et al., Bull.Am.Phys.Society,
C *** volume 34, 1964 (1989)

      if (isc.eq.22) then
         tauee = hfact * 0.25D0 * bt**0.8D0 * dnla20**0.6D0 *
     +        powerht**(-0.6D0) * rminor**2.4D0 * rmajor**0.6D0
         gtaue = 0.0D0
         ptaue = 0.6D0
         qtaue = 0.0D0
         rtaue = -0.6D0
      end if

C *** Lackner-Gottardi stellarator scaling
C *** (assumed q = 1/rotational transform)
C *** K.Lackner and N.A.O.Gottardi, Nuclear Fusion, 30, p.767 (1990)

      if (isc.eq.23) then
         tauee = hfact * 0.17D0 * rmajor * rminor**2 * dnla20**0.6D0 *
     +        bt**0.8D0 * powerht**(-0.6D0) * q**(-0.4D0)
         gtaue = 0.0D0
         ptaue = 0.6D0
         qtaue = 0.0D0
         rtaue = -0.6D0
      end if

C *** ITER-93H scaling (ELM-free; multiply by 0.85 for ELMy version)
C *** S.Kaye and the ITER Joint Central Team and Home Teams, in Plasma
C *** Physics and Controlled Nuclear Fusion Research (Proc. 15th
C *** Int. Conf., Seville, 1994) IAEA-CN-60/E-P-3

      if (isc.eq.24) then
         tauee = hfact * 0.053D0 * pcur**1.06D0 * bt**0.32D0 *
     +        powerht**(-0.67D0) * afuel**0.41D0 * rmajor**1.79D0 *
     +        dnla20**0.17D0 * aspect**0.11D0 * kappa**0.66D0

         gtaue = 0.0D0
         ptaue = 0.17D0
         qtaue = 0.0D0
         rtaue = -0.67D0
      end if

C *** TITAN RFP scaling
C *** TITAN RFP Fusion Reactor Study, Scoping Phase Report
C *** UCLA-PPG-1100, page 5-9, Jan 1987

      if (isc.eq.25) then
         tauee = hfact * 0.05D0 * pcur * rminor**2

         gtaue = 0.0D0
         ptaue = 0.0D0
         qtaue = 0.0D0
         rtaue = 0.0D0
      end if

C *** ITER-97 H-mode scalings
C *** J.G.Cordey et al., EPS Berchtesgaden, 1997

C *** ELM-free: ITERH-97P

      if (isc.eq.26) then
         tauee = hfact * 0.031D0 * pcur**0.95D0 * bt**0.25D0 *
     +        powerht**(-0.67D0) * dnla19**0.35D0 *
     +        rmajor**1.92D0 * aspect**(-0.08D0) * kappa**0.63D0 *
     +        afuel**0.42D0

         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=26
         ptaue = 0.35D0
         qtaue = 0.0D0
         rtaue = -0.67D0
      end if

C *** ELMy: ITERH-97P(y)

      if (isc.eq.27) then
         tauee = hfact * 0.029D0 * pcur**0.90D0 * bt**0.20D0 *
     +        powerht**(-0.66D0) * dnla19**0.40D0 *
     +        rmajor**2.03D0 * aspect**(-0.19D0) * kappa**0.92D0 *
     +        afuel**0.2D0

         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=27
         ptaue = 0.4D0
         qtaue = 0.0D0
         rtaue = -0.66D0
      end if

C *** ITER-96P L-mode scaling
C *** S.M.Kaye and the ITER Confinement Database Working Group,
C *** Nuclear Fusion vol.37 (1997) 1303

C *** N.B. tau_th formula used

      if (isc.eq.28) then
         tauee = hfact * 0.023D0 * pcur**0.96D0 * bt**0.03D0 *
     +        kappa95**0.64D0 * rmajor**1.83D0 * aspect**0.06D0 *
     +        dnla19**0.40D0 * afuel**0.20D0 * powerht**(-0.73D0)

         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=28
         ptaue = 0.4D0
         qtaue = 0.0D0
         rtaue = -0.73D0
      end if

C *** Valovic modified ELMy-H mode scaling

      if (isc.eq.29) then
         tauee = hfact * 0.067D0 * pcur**0.9D0 * bt**0.17D0 *
     +        dnla19**0.45D0 * afuel**0.05D0 * rmajor**1.316D0 *
     +        rminor**0.79D0 * kappa**0.56D0 * powerht**(-0.68D0)
         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=29
         ptaue = 0.45D0
         qtaue = 0.0D0
         rtaue = -0.68D0
      end if

C *** Kaye PPPL Workshop April 1998 L-mode scaling

      if (isc.eq.30) then
         tauee = hfact * 0.021D0 * pcur**0.81D0 * bt**0.14D0 *
     +        kappa**0.7D0 * rmajor**2.01D0 * aspect**(-0.18D0) *
     +        dnla19**0.47D0 * afuel**0.25D0 * powerht**(-0.73D0)
         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=30
         ptaue = 0.47D0
         qtaue = 0.0D0
         rtaue = -0.73D0
      end if

C *** ITERH-PB98P(y), ELMy H-mode scaling

      if (isc.eq.31) then
         tauee = hfact * 0.0615D0 * pcur**0.9D0 * bt**0.1D0 *
     +        dnla19**0.4D0 * powerht**(-0.66D0) * rmajor**2 *
     +        kappaa**0.75D0 * aspect**(-0.66D0) * afuel**0.2D0
         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=31
         ptaue = 0.4D0
         qtaue = 0.0D0
         rtaue = -0.66D0
      end if

C *** IPB98(y), ELMy H-mode scaling

      if (isc.eq.32) then
         tauee = hfact * 0.0365D0 * pcur**0.97D0 * bt**0.08D0 *
     +        dnla19**0.41D0 * powerht**(-0.63D0) * rmajor**1.93D0 *
     +        kappa**0.67D0 * aspect**(-0.23D0) * afuel**0.2D0
         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=32
         ptaue = 0.41D0
         qtaue = 0.0D0
         rtaue = -0.63D0
      end if

C *** IPB98(y,1), ELMy H-mode scaling

      if (isc.eq.33) then
         tauee = hfact * 0.0503D0 * pcur**0.91D0 * bt**0.15D0 *
     +        dnla19**0.44D0 * powerht**(-0.65D0) * rmajor**2.05D0 *
     +        kappaa**0.72D0 * aspect**(-0.57D0) * afuel**0.13D0
         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=33
         ptaue = 0.44D0
         qtaue = 0.0D0
         rtaue = -0.65D0
      end if

C *** IPB98(y,2), ELMy H-mode scaling

      if (isc.eq.34) then
         tauee = hfact * 0.0562D0 * pcur**0.93D0 * bt**0.15D0 *
     +        dnla19**0.41D0 * powerht**(-0.69D0) * rmajor**1.97D0 *
     +        kappaa**0.78D0 * aspect**(-0.58D0) * afuel**0.19D0
         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=34
         ptaue = 0.41D0
         qtaue = 0.0D0
         rtaue = -0.69D0
      end if

C *** IPB98(y,3), ELMy H-mode scaling

      if (isc.eq.35) then
         tauee = hfact * 0.0564D0 * pcur**0.88D0 * bt**0.07D0 *
     +        dnla19**0.40D0 * powerht**(-0.69D0) * rmajor**2.15D0 *
     +        kappaa**0.78D0 * aspect**(-0.64D0) * afuel**0.20D0
         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=35
         ptaue = 0.4D0
         qtaue = 0.0D0
         rtaue = -0.69D0
      end if

C *** IPB98(y,4), ELMy H-mode scaling

      if (isc.eq.36) then
         tauee = hfact * 0.0587D0 * pcur**0.85D0 * bt**0.29D0 *
     +        dnla19**0.39D0 * powerht**(-0.70D0) * rmajor**2.08D0 *
     +        kappaa**0.76D0 * aspect**(-0.69D0) * afuel**0.17D0
         gtaue = 0.0D0
C ***    N.B. problems with ptaue if pulsed option is used with isc=36
         ptaue = 0.39D0
         qtaue = 0.0D0
         rtaue = -0.70D0
      end if

C *** Ion energy confinement time

      tauei = tauee

C *** Calculation of the transport power loss terms
C *** Transport losses in Watts/m3 are 3/2 * n.e.T / tau , with T in eV
C *** (here, tin and ten are in keV, and ptre and ptri are in MW/m3)

      ptri = 2.403D-22 * dnitot*tin/tauei
      ptre = 2.403D-22 * dene*ten/tauee

      ratio = dnitot/dene * tin/ten

C *** Global energy confinement time

      taueff = ((ratio + 1.0D0)/(ratio/tauei + 1.0D0/tauee))

C+**PJK 08/11/93

      ftaue = (tauee-gtaue)/
     +     (n20**ptaue * (te/10.0D0)**qtaue * powerht**rtaue)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE VSCALC(csawth,eps,facoh,gamma,kappa,rmajor,rmu0,rplas,
     +     plascur,tburn,phiint,rli,rlp,vsbrn,vsind,vsres,vsstt)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This subroutine calculates the volt-second requirements and some
C  other related items.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  csawth : (INPUT)  coefficient for sawteeth effects
C  eps    : (INPUT)  inverse aspect ratio
C  facoh  : (INPUT)  fraction of plasma current produced inductively
C  gamma  : (INPUT)  coefficient for resistive start-up V-s component
C  kappa  : (INPUT)  plasma elongation
C  plascur: (INPUT)  plasma current (A)
C  rli    : (INPUT)  plasma normalised inductivity
C  rmajor : (INPUT)  plasma major radius (m)
C  rmu0   : (INPUT)  4.0D-7 * pi
C  rplas  : (INPUT)  plasma resistance (ohm)
C  tburn  : (INPUT)  burn time (s)
C  phiint : (OUTPUT) internal plasma volt-seconds (Wb)
C  rlp    : (OUTPUT) plasma inductance (H)
C  vsbrn  : (OUTPUT) volt-seconds needed during burn (Wb)
C  vsind  : (OUTPUT) internal and external plasma inductance V-s (Wb)
C  vsres  : (OUTPUT) resistive losses in start-up volt-seconds (Wb)
C  vsstt  : (OUTPUT) total volt-seconds needed (Wb)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     csawth,eps,facoh,gamma,kappa,phiint,plascur,rli,rlp,rmajor,
     +     rmu0,rplas,tburn,vsbrn,vsind,vsres,vsstt

C  Local variables
      DOUBLE PRECISION
     +     aeps,beps,rlpext,rlpint,vburn

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Internal inductance

      rlpint = rmu0 * rmajor * rli/2.0D0
      phiint = rlpint*plascur

C *** Start-up resistive component (use ITER formula without the
C *** extra 10 V-s add-on)

      vsres = gamma * rmu0*plascur*rmajor

C *** Hirshman, Neilson: Physics of Fluids, 29 (1986) p790
C *** fit for external inductance

      aeps = (1.0D0 + 1.81D0*sqrt(eps)+2.05D0*eps)*log(8.0D0/eps)
     +     - (2.0D0 + 9.25D0*sqrt(eps)-1.21D0*eps)
      beps = 0.73D0 * sqrt(eps) *(1.0D0 + 2.0D0*eps**4-6.0D0*eps**5
     +     + 3.7D0*eps**6)
      rlpext = rmajor*rmu0 * aeps*(1.0D0-eps)/(1.0D0-eps+beps*kappa)

      rlp = 1.25D0 * (rlpext + rlpint )

C *** Inductive V-s component

      vsind = rlp * plascur
      vsstt = vsres + vsind

C *** Loop voltage during burn

      vburn = plascur*rplas * facoh

C *** Include enhancement factor in flattop V-s requirement
C *** to account for MHD sawtooth effects.

C+**PJK 18/11/93 N.B. tburn on first iteration will not be correct
C+**PJK 18/11/93 if the pulsed reactor option is used, but the value
C+**PJK 18/11/93 will be correct on subsequent calls.

      vsbrn = csawth*vburn*tburn
      vsstt = vsstt + vsbrn

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE PHYAUX(aspect,dene,deni,idhe3,plascur,powfmw,sbar,
     +     dnalp,dnprot,taueff,burnup,dntau,figmer,fusrat,qfuel,rndfuel,
     +     taup)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  This subroutine calculates extra physics related items
C  needed by other parts of the code
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  07 December 1995
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  07/12/95 PJK 1.100 Added D-He3 calculations
C
C--Arguments
C  aspect : (INPUT)  plasma aspect ratio
C  dene   : (INPUT)  electron density (/m3)
C  deni   : (INPUT)  fuel ion density (/m3)
C  dnalp  : (INPUT)  alpha ash density (/m3)
C  dnprot : (INPUT)  proton ash density (/m3)
C  idhe3  : (INPUT)  switch denoting D-T or D-He3 reaction
C  plascur: (INPUT)  plasma current (A)
C  powfmw : (INPUT)  fusion power (MW)
C  sbar   : (INPUT)  exponent for aspect ratio (normally 1)
C  taueff : (INPUT)  global energy confinement time (s)
C  burnup : (OUTPUT) fractional plasma burnup
C  dntau  : (OUTPUT) plasma average n-tau (s/m3)
C  figmer : (OUTPUT) physics figure of merit
C  fusrat : (OUTPUT) number of fusion reactions per second
C  qfuel  : (OUTPUT) fuelling rate for D-T (A)
C  rndfuel: (OUTPUT) fuel burnup rate (A)
C  taup   : (OUTPUT) 5 * global energy confinement time
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     aspect,burnup,dene,deni,dnalp,dnprot,dntau,figmer,fusrat,
     +     plascur,powfmw,qfuel,rndfuel,sbar,taueff,taup
      INTEGER idhe3

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      figmer = 1.0D-6 * plascur * aspect**sbar

      dntau = taueff*dene

      taup = taueff*5.0D0

      if (idhe3.eq.0) then
         fusrat = powfmw /(17.6D0 * 1.6022D-19)
      else
         fusrat = powfmw /(18.3D0 * 1.6022D-19)
      end if

C *** Assume that the ash and fuel particle confinement times are equal

      if (idhe3.eq.0) then
         burnup = 1.0D0/ (1.0D0 + deni/(dnalp*2.0D0) )
      else
         burnup = 1.0D0/ (1.0D0 + deni/(dnalp+dnprot) )
      end if

C+**PJK 16/01/96 Factor 2 should be changed for D-He3?

      rndfuel = 1.6022D-19 * 2.0D0 * fusrat

      qfuel = rndfuel/burnup

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SVFDT_ORIG(t)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the D-T reaction rate in m3/s
C  Range of fit is 0 < t(keV) < 80
C
C--Author
C  Ronald L. Miller, CTR-12, MS-F641, P.O. Box 1663,
C    Los Alamos National, Los Alamos, New Mexico 87545 USA
C  Modified by J Galambos to use the fits by L Hivley
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  Jarmie, Brown, & Hardekopf, Phys Rev C 29, 2031 (1984)
C                  and erratum Phys Rev C 33,  385 (1986)
C  Hivley, Nuclear Fusion 17, 1977, p 873
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C  Copyright, 1987, the Regents of the University of California.
C  This software was produced under a U.S. Government contract
C  (W-7405-ENG-36) by the Los Alamos National Laboratory, which
C  is operated by the University of California for the U.S.
C  Department of Energy.  The U.S. Government is licensed to use,
C  reproduce, and distribute the software.  Permission is granted
C  to the public to copy and use this software without charge,
C  provided that this notice and any statement of authorship are
C  reproduced on all copies.  Neither the Government nor the
C  University makes any warranty, express or implied, or assumes
C  any liability or responsibility for the use of this software.
C
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  25/07/11 PJK Changed name to SVFDT_ORIG from SVFDT; superseded by
C               new routine coded by R. Kemp (below)
C
C--Arguments
C  t      : (INPUT)  Maxwellian density-weighted ion temperature (keV)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION t

C  Local variables
      DOUBLE PRECISION acoef(7),dummy,sdtjbh,ss,tau,tn(12),z
      INTEGER j

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      tn(1)  =  0.115919516928D2
      tn(2)  =  0.171469900884D1
      tn(3)  = -0.192406821958D2
      tn(4)  =  0.101380998163D3
      tn(5)  = -0.276406974777D3
      tn(6)  =  0.462387793663D3
      tn(7)  = -0.495796101221D3
      tn(8)  =  0.346206957555D3
      tn(9)  = -0.155600394210D3
      tn(10) =  0.431789037054D2
      tn(11) = -0.671263871904D1
      tn(12) =  0.446674956199D0

      acoef(1) = -21.377692D0
      acoef(2) = -25.204054D0
      acoef(3) = -0.071013427D0
      acoef(4) = 1.9375451D-4
      acoef(5) = 4.9246592D-6
      acoef(6) = -3.9836572D-8
      acoef(7) = 0.2935D0

      if (t.le.20.0D0) then

C *** Jarmie, Brown and Hardekopf method

         z = t**0.33333333333333D0
         ss = 0.0D0
         do 10 j = 1,12
            ss = ss+tn(j)*z**(j-1)
 10      continue
         tau = 19.983D0/z

         sdtjbh = 5.967D-22*ss*tau**2*exp(-tau)*
     +        (1.0D0+5.0D0/(12.0D0*tau))
         svfdt_orig = sdtjbh

      else

C *** Hivley fit

         dummy = acoef(1)/t**acoef(7) + acoef(2) +
     +        acoef(3)*t +    acoef(4)*t**2 +
     +        acoef(5)*t**3 + acoef(6)*t**4
         svfdt_orig = 1.0D-6* exp(dummy)

      end if

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SVFDT(t)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 0.86
C
C--Description
C  Routine to calculate the D-T reaction rate in m3/s
C  Range of fit is 0.2 < t(keV) < 100
C
C--Author
C  Richard Kemp D3/186 Culham, ext.6438
C
C--Date
C  1 June 2011
C
C--Reference
C  Bosch and Hale, Nuclear Fusion, 32, no.4 (1992) pp 611-631
C
C--History
C  1/06/11 RK 0.86 No particular standard of coding
C
C--Arguments
C  t      : (INPUT)  Maxwellian density-weighted ion temperature (keV)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION t

C  Local variables
      DOUBLE PRECISION acoef(7),bg, mrc2, zeta, theta1, theta, sdtjbh

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      bg = 34.3827
      mrc2 = 1.124656d6

      acoef(1) = 1.17302d-9
      acoef(2) = 1.51361d-2
      acoef(3) = 7.51886d-2
      acoef(4) = 4.60643d-3
      acoef(5) = 1.35000d-2
      acoef(6) = -1.06750d-4
      acoef(7) = 1.36600d-5

      theta1 = t*(acoef(2)+t*(acoef(4)+t*acoef(6)))/(1.d0+
     +   t*(acoef(3)+t*(acoef(5)+t*acoef(7))))
      theta = t/(1.d0 - theta1)
      zeta = ((bg**2)/(4.d0*theta))**0.3333333333

      sdtjbh = 1.d-6*acoef(1)*theta*sqrt(zeta/(mrc2*t**3))*
     +   exp(-3.d0 * zeta)

      svfdt = sdtjbh

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FINT(xy)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Function to find integrand for alpha power integration performed
C  using routine QUANC8 in routine PALPH.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  xy     : (INPUT)  Abscissa of the integration, = normalised
C                    plasma minor radius (0.0 .LE. xy .LT. 1.0)
C
C--Global variables passed via COMMON
C  alphand: (INPUT)  density profile index
C  alphatd: (INPUT)  temperature profile index
C  tidum  : (INPUT)  ion temperature (keV)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION xy

C  Global variables
      DOUBLE PRECISION alphand,alphatd,tidum
      COMMON/fintcom/tidum,alphatd,alphand

C  Local variables
      DOUBLE PRECISION dxy,sigv,tiofr

C  External functions
      DOUBLE PRECISION svfdt
      EXTERNAL         svfdt

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      tiofr = tidum*(1.0D0+alphatd)*(1.0D0-(xy**2))**alphatd
      sigv = svfdt(tiofr)
      dxy = (1.0D0-(xy**2))**alphand

      fint = xy*(dxy**2)*sigv

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RETHER(alphan,alphat,dene,dlamie,te,ti,zeffai,pie)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to find the equilibration power between the
C  ions and electrons.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  alphan : (INPUT)  density profile index
C  alphat : (INPUT)  temperature profile index
C  dene   : (INPUT)  electron density (/m3)
C  dlamie : (INPUT)  ion-electron coulomb logarithm
C  te     : (INPUT)  electron temperature (keV)
C  ti     : (INPUT)  ion temperature (keV)
C  zeffai : (INPUT)  density weighted plasma effective charge
C  pie    : (OUTPUT) ion/electron equilibration power (MW/m3)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     alphan,alphat,dene,dlamie,pie,te,ti,zeffai

C  Local variables
      DOUBLE PRECISION conie,profie

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      profie = (1.0D0+alphan)**2 /( (2.0D0*alphan -
     +     0.5D0*alphat + 1.0D0) * sqrt(1.0D0+alphat) )

      conie = 2.42165D-41 * dlamie * dene**2 * zeffai * profie

      pie = conie*(ti-te)/(te**1.5D0)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RADPWR_ORIG(alphan,alphat,aspect,bt,dene,deni,fbfe,
     +     kappa95,rmajor,rminor,ralpne,rncne,rnone,rnfene,ssync,ten,
     +     vol,pbrem,plrad,prad,psync)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This subroutine finds the radiation power in MW/m3.
C  Bremsstrahlung and synchrotron power included.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  25/07/11 PJK       Changed name to RADPWR_ORIG from RADPWR;
C                     Superseded by new routine coded by R. Kemp (below)
C
C--Arguments
C  alphan : (INPUT)  density profile index
C  alphat : (INPUT)  temperature profile index
C  aspect : (INPUT)  plasma aspect ratio
C  bt     : (INPUT)  toroidal field on axis (T)
C  dene   : (INPUT)  electron density (/m3)
C  deni   : (INPUT)  fuel ion density (/m3)
C  fbfe   : (INPUT)  fraction of iron radiation to bremsstrahlung
C  kappa95: (INPUT)  plasma elongation at 95% surface
C  ralpne : (INPUT)  thermal alpha density / electron density
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  rncne  : (INPUT)  carbon density / electron density
C  rnfene : (INPUT)  iron density / electron density
C  rnone  : (INPUT)  oxygen density / electron density
C  ssync  : (INPUT)  synchrotron wall reflectivity factor
C  ten    : (INPUT)  density weighted average electron temperature (keV)
C  vol    : (INPUT)  plasma volume (m3)
C  pbrem  : (OUTPUT) bremsstrahlung radiation power/volume (MW/m3)
C  plrad  : (OUTPUT) edge line radiation power/volume (MW/m3)
C  prad   : (OUTPUT) total core radiation power/volume (MW/m3)
C  psync  : (OUTPUT) synchrotron radiation power/volume (MW/m3)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,pbrem,plrad,
     +     prad,psync,ralpne,rmajor,rminor,rncne,rnfene,rnone,ssync,
     +     ten,vol

C  Local variables
      DOUBLE PRECISION
     +     den20,fbc,fbhe,fbo,pbremdt,pbremz,pc,pfe,phe,po,radexp,
     +     t10,vr,xfact

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      fbhe = 0.9D0
      fbc = 0.52D0
      fbo = 0.52D0

      radexp = (1.0D0 + alphan)**1.5D0 * sqrt(1.0D0 + alphan + alphat)/
     +     (1.0D0 + 2.0D0 * alphan + 0.5D0 * alphat)
      den20 = dene/1.0D20
      t10 = ten/10.0D0

C *** D-T Bremsstrahlung

      pbremdt = 1.6D-2 * radexp * den20**2 * (deni/dene) * sqrt(t10)

C *** High Z Bremsstrahlung

      vr = rmajor * (rminor*(1.0D0 + kappa95)/2.0D0)**2 /
     +     (58.652D0 *vol)
      phe = 65.8D0 * ralpne * (dene/7.0D19)**1.5D0 * vr
      pc  = 1120.0D0 * rncne * (dene/7.0D19)**1.5D0 * vr
      po  = 2240.0D0 * rnone * (dene/7.0D19)**1.5D0 * vr
      pfe = 44800.0D0 * rnfene * (dene/7.0D19)**2.5D0 * vr
      pbremz = fbhe*phe + fbc*pc + fbo*po + fbfe*pfe

      pbrem = pbremz + pbremdt

C *** Line radiation

      plrad = (1.0D0-fbhe)*phe + (1.0D0-fbc)*pc + (1.0D0-fbo)*po +
     +     (1.0D0-fbfe)*pfe

C *** Synchrotron power

      xfact = 5.7D0/(aspect*sqrt(t10))

      psync = 1.3D-4*(bt*t10)**2.5D0 * (1.0D0+xfact)**0.5D0
     +     * (den20/rminor)**0.5D0 * (1.0D0-ssync)**0.5D0

C *** Total continuum radiation power

      prad = pbrem + psync

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RADPWR(alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,
     +     rmajor,rminor,ralpne,rncne,rnone,rnfene,ssync,ten,vol,
     +     pbrem,plrad,prad,psync)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  This subroutine finds the radiation power in MW/m3.
C  Bremsstrahlung and synchrotron power included.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C  Richard Kemp D3/186  Culham, ext.6438
C
C--Date
C  25 July 2011
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  Albajar, Nucl. Fusion 41 (2001) p665
C  Fidone, Giruzzi, Granata, Nucl. Fusion 41 (2001) p1755
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  21/07/11 RK  1.100 Implement Albajar for P_sync
C
C--Arguments
C  alphan : (INPUT)  density profile index
C  alphat : (INPUT)  temperature profile index
C  aspect : (INPUT)  plasma aspect ratio
C  bt     : (INPUT)  toroidal field on axis (T)
C  dene   : (INPUT)  electron density (/m3)
C  deni   : (INPUT)  fuel ion density (/m3)
C  fbfe   : (INPUT)  fraction of iron radiation to bremsstrahlung
C  kappa95: (INPUT)  plasma elongation at 95% surface
C  ralpne : (INPUT)  thermal alpha density / electron density
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  rncne  : (INPUT)  carbon density / electron density
C  rnfene : (INPUT)  iron density / electron density
C  rnone  : (INPUT)  oxygen density / electron density
C  ssync  : (INPUT)  synchrotron wall reflectivity factor
C  ten    : (INPUT)  density weighted average electron temperature (keV)
C  vol    : (INPUT)  plasma volume (m3)
C  pbrem  : (OUTPUT) bremsstrahlung radiation power/volume (MW/m3)
C  plrad  : (OUTPUT) edge line radiation power/volume (MW/m3)
C  prad   : (OUTPUT) total core radiation power/volume (MW/m3)
C  psync  : (OUTPUT) synchrotron radiation power/volume (MW/m3)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,pbrem,plrad,
     +     prad,psync,ralpne,rmajor,rminor,rncne,rnfene,rnone,ssync,
     +     ten,vol

C  Local variables
      DOUBLE PRECISION
     +     den20,fbc,fbhe,fbo,pbremdt,pbremz,pc,pfe,phe,po,radexp,
     +     t10,vr,xfact, kfun, gfun, pao, de2o, teo, dum, tbet, rpow,
     +     kap, pi

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      fbhe = 0.9D0
      fbc = 0.52D0
      fbo = 0.52D0

      radexp = (1.0D0 + alphan)**1.5D0 * sqrt(1.0D0 + alphan + alphat)/
     +     (1.0D0 + 2.0D0 * alphan + 0.5D0 * alphat)
      den20 = dene/1.0D20
      t10 = ten/10.0D0

C *** D-T Bremsstrahlung

      pbremdt = 1.6D-2 * radexp * den20**2 * (deni/dene) * sqrt(t10)

C *** High Z Bremsstrahlung

      vr = rmajor * (rminor*(1.0D0 + kappa95)/2.0D0)**2 /
     +     (58.652D0 *vol)
      phe = 65.8D0 * ralpne * (dene/7.0D19)**1.5D0 * vr
      pc  = 1120.0D0 * rncne * (dene/7.0D19)**1.5D0 * vr
      po  = 2240.0D0 * rnone * (dene/7.0D19)**1.5D0 * vr
      pfe = 44800.0D0 * rnfene * (dene/7.0D19)**2.5D0 * vr
      pbremz = fbhe*phe + fbc*pc + fbo*po + fbfe*pfe

      pbrem = pbremz + pbremdt

C *** Line radiation

      plrad = (1.0D0-fbhe)*phe + (1.0D0-fbc)*pc + (1.0D0-fbo)*po +
     +     (1.0D0-fbfe)*pfe

C *** Synchrotron power

C      xfact = 5.7D0/(aspect*sqrt(t10))

C      psync = 1.3D-4*(bt*t10)**2.5D0 * (1.0D0+xfact)**0.5D0
C     +     * (den20/rminor)**0.5D0 * (1.0D0-ssync)**0.5D0

C *** tbet is betaT in Albajar, not to be confused with plasma beta
      tbet = 2.0d0
C *** rpow is the (1-Rsyn) power dependence based on plasma shape
C *** (see Fidone)
      rpow = 0.62d0
      pi = acos(-1.d0)
      kap = vol/(2.d0 * pi**2.d0 * rmajor * rminor**2.d0)
      
      de2o = (1.0d0+alphan) * den20
      teo = ten * (1.d0 + alphan + alphat)/(1.d0+alphan)
      pao = 6.04d3 * (rminor*de2o)/bt
      gfun = 0.93d0 * (1.d0 + 0.85d0*exp(-0.82d0 * rmajor/rminor) )
      kfun = (alphan + 3.87d0*alphat + 1.46d0)**(-0.79d0)
      kfun = kfun * (1.98d0+alphat)**1.36d0 * tbet**2.14d0
      kfun = kfun*(tbet**1.53d0 + 1.87d0*alphat - 0.16d0)**(-1.33d0)
      dum = (1.d0+0.12d0*(teo/(pao**0.41d0))*(1.d0-ssync)**0.41)
C *** this equation (for dum) contains "very high T modification" from Fidone
      dum = dum**(-1.51d0)
     
      psync = 3.84d-8 * (1.d0-ssync)**rpow * rmajor * rminor**1.38d0
      psync = psync * kap**0.79d0 * bt**2.62d0 * de2o**0.38d0
      psync = psync * teo *(16.d0+teo)**2.61d0 * dum * gfun * kfun
C *** psync should be in volume, Albajar gives it as total
      psync = psync/vol      

C *** Total continuum radiation power

      prad = pbrem + psync

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE POHM(facoh,ires,kappa,plascur,rmajor,rminor,ten,vol,
     +     zeff,pohmpv,rpfac,rplas)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.001
C
C--Description
C  This subroutine finds the ohmic heating power per unit volume.
C  The expression is a good fit for alphan = 0.5, alphat = 1.0,
C  alphaj = 1.5, aspect = 2.5 -- 4.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  25 July 2011
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  25/07/11 PJK 1.001 Correction to facoh coding
C
C--Arguments
C  facoh  : (INPUT)  fraction of plasma current produced inductively
C  ires   : (INPUT)  switch for neoclassical plasma resistivity
C  kappa  : (INPUT)  plasma elongation
C  plascur: (INPUT)  plasma current (A)
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  ten    : (INPUT)  density weighted average electron temperature (keV)
C  vol    : (INPUT)  plasma volume (m3)
C  zeff   : (INPUT)  plasma effective charge
C  pohmpv : (OUTPUT) ohmic heating power per unit volume (MW/m3)
C  rpfac  : (OUTPUT) neoclassical resistivity enhancement factor
C  rplas  : (OUTPUT) plasma resistance (ohm)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     facoh,kappa,plascur,pohmpv,rmajor,rminor,rpfac,rplas,ten,
     +     vol,zeff
      INTEGER ires

C  Local variables
      DOUBLE PRECISION t10

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Density weighted electron temperature in 10 keV units
C *** (not sure why ten is used instead of te)

      t10 = ten/10.0D0

C *** Plasma resistance

      rplas = 2.15D-9*zeff*rmajor/(kappa*rminor**2*t10**1.5D0)

C *** Neo-classical resistivity enhancement factor
C *** Taken from  N. A. Uckan et al, Fusion Technology 13 (1988) p.411.
C *** The expression is valid for aspect ratios in the range 2.5--4.

      if (ires.eq.1) then
         rpfac = 4.3D0 - 0.6D0*rmajor/rminor
         rplas = rplas * rpfac
      end if

C+**PJK 05/04/94 Check to see if plasma resistance is negative
C+**PJK 05/04/94 (possible if ires = 1 and aspect ratio is too high)

      if (rplas.le.0.0D0) then
         write(*,*) 'Warning in routine POHM:'
         write(*,*) 'Plasma resistance, rplas = ',rplas
         write(*,*) 'PROCESS continuing...'
      end if

C *** Ohmic heating power per unit volume

C+**PJK 25/07/11 Corrected from: pohmpv = (facoh*plascur)**2 * ...
      pohmpv = facoh * plascur**2 * rplas * 1.0D-6/vol

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE PTHRESH(dene,dnla,bt,rmajor,kappa,pthrmw)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This subroutine calculates the power threshold for the L-mode to
C  H-mode transition
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  17 July 1998
C
C--Reference
C  ITER Physics Design Description Document, p.2-2
C  ITER-FDR Plasma Performance Assessments, p.III-9
C  Snipes, 24th EPS Conference, Berchtesgaden 1997, p.961
C  
C--History
C  17/07/98 PJK 1.000 New routine
C
C--Arguments
C  dene   : (INPUT)  volume-averaged electron density (/m3)
C  dnla   : (INPUT)  line-averaged electron density (/m3)
C  bt     : (INPUT)  toroidal field on axis (T)
C  rmajor : (INPUT)  plasma major radius (m)
C  kappa  : (INPUT)  plasma elongation
C  pthrmw(5) : (OUTPUT) power threshold (different scalings)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     dene,dnla,bt,rmajor,kappa,pthrmw(5)

C  Local variables
      DOUBLE PRECISION dene20,dnla20

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      dene20 = 1.0D-20*dene
      dnla20 = 1.0D-20*dnla

C *** ITER-DDD, D.Boucher
C *** Fit to 1996 H-mode power threshold database: nominal

      pthrmw(1) = 0.45D0 * dene20**0.75D0 * bt * rmajor**2

C *** Fit to 1996 H-mode power threshold database: upper bound

      pthrmw(2) = 0.37D0 * dene20 * bt * rmajor**2.5D0

C *** Fit to 1996 H-mode power threshold database: lower bound

      pthrmw(3) = 0.54D0 * dene20**0.5D0 * bt * rmajor**1.5D0

C *** J. A. Snipes, ITER H-mode Threshold Database Working Group,
C *** Controlled Fusion and Plasma Physics, 24th EPS Conference,
C *** Berchtesgaden, June 1997, vol.21A, part III, p.961

      pthrmw(4) = 0.65D0 * dnla20**0.93D0 * bt**0.86D0 * rmajor**2.15D0

      pthrmw(5) = 0.42D0 * dnla20**0.80D0 * bt**0.90D0 * rmajor**1.99D0
     +     * kappa**0.76D0

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE IGMARCAL(nout)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.040
C
C--Description
C  Routine to calculate ignition margin at the final point
C  with different scalings
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  16 July 2001
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  01/04/98 PJK 1.010 Modified PCOND arguments
C  30/06/98 PJK 1.020 Modified PCOND arguments
C  19/01/99 PJK 1.030 Modified PCOND arguments
C  17/07/01 PJK 1.040 Modified PCOND arguments
C
C--Arguments
C  nout   : (INPUT)  Fortran unit specifier for main output
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'labels.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER nout

C  Local variables
      DOUBLE PRECISION d2,powerhtz,ptrez,ptriz,taueez,taueezz,taueffz,
     +     taueiz
      INTEGER i,iisc

C  External functions
      DOUBLE PRECISION fhfac
      EXTERNAL         fhfac

C  External routines
      EXTERNAL oblnkl,osubhd,pcond

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Only produce output if required

      if (sect03.eq.0) goto 1000

C *** Start output

      call osubhd(nout,'Confinement times, and required H-factors :')

      write(nout,10)
 10   format(
     +     t5,'scaling law',
     +     t30,'confinement time (s)',
     +     t55,'H-factor for')

      write(nout,20)
 20   format(
     +     t34,'for H = 2',
     +     t54,'power balance')

      call oblnkl(nout)

C *** Calculate power balances for all scaling laws assuming H = 2

      do 40 iisc = 1,ipnlaws
         i = iisc
         d2 = 2.0D0

         call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,d2,
     +        iinvqd,i,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji,
     +        plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q,qstar,vol,
     +        xarea,zeff,ptrez,ptriz,taueez,taueiz,taueffz,powerhtz)

         hfac(iisc) = fhfac(i)

         write(nout,30) tauscl(iisc),taueez,hfac(iisc)
 30      format(t2,a24,t34,f7.3,t58,f7.3)

 40   continue

 1000 continue

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FHFAC(is)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Function to find H-factor for power balance
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  21 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  is     : (INPUT)  Number of confinement time scaling law of interest
C
C--Global variables passed via COMMON
C  iscz   : (OUTPUT) Number of confinement time scaling law of interest
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER is

C  Global variables
      INTEGER iscz
      COMMON /chfac/ iscz

C  Local variables
      DOUBLE PRECISION
     +     abserr,xhigh,xlow

C  External functions
      DOUBLE PRECISION fhz,zeroin
      EXTERNAL         fhz,zeroin

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Initialise global variable

      iscz = is

C *** Set numerical tolerance and bounds on H-factor

      abserr = 0.003D0
      xlow = 0.01D0
      xhigh = 100.0D0

C *** Find value of H factor for which function FHZ is zero
C *** (this occurs at power balance)

      fhfac = zeroin(xlow,xhigh,fhz,abserr)

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FHZ(hhh)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.040
C
C--Description
C  Function that is used to find power balance.
C  FHZ is zero at power balance, which is achieved using routine
C  ZEROIN to adjust the value of hhh, the confinement time H-factor.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  16 July 2001
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  01/04/98 PJK 1.010 Modified PCOND arguments, and adding coding for
C                     use of IGNITE
C  30/06/98 PJK 1.020 Modified PCOND arguments
C  19/01/99 PJK 1.030 Modified PCOND arguments
C  16/07/01 PJK 1.040 Modified PCOND arguments
C
C--Arguments
C  hhh    : (INPUT)  test value for confinement time H-factor
C
C--Global variables passed via COMMON
C  iscz   : (INPUT)  number of confinement time scaling law of interest
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'

C  Arguments
      DOUBLE PRECISION hhh

C  Global variables
      INTEGER iscz
      COMMON /chfac/ iscz

C  Local variables
      DOUBLE PRECISION
     +     powerhtz,ptrez,ptriz,taueez,taueezz,taueiz,taueffz

C  External routines
      EXTERNAL pcond

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hhh,
     +     iinvqd,iscz,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji,
     +     plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q,qstar,vol,
     +     xarea,zeff,ptrez,ptriz,taueezz,taueiz,taueffz,powerhtz)

      if (iscz.lt.3) then
         ptrez = ptrez/hhh
         ptriz = ptriz/hhh
      end if

C *** At power balance, fhz is zero.
C *** Take into account whether injected power is included in tau_e
C *** calculation (i.e. whether device is ignited)

      if (ignite.eq.0) then
         fhz = ptrez + ptriz + prad - palp - pcharge -
     +        1.0D-6*(pinje+pinji)/vol - pohmpv
      else
         fhz = ptrez + ptriz + prad - palp - pcharge - pohmpv
      end if

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE BEAMFUS(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie,
     +     ealpha,enbeam,fdeut,ftr,ftritbm,sigvdt,ten,tin,vol,zeffai,
     +     betanb,dnbeam2,palpnb)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Subroutine to calculate beam slowing down properties
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  11 December 1995
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  21/06/94 PJK 1.000 Upgrade to higher standard of coding
C  05/12/95 PJK 1.100 Added ealpha to argument list
C  11/12/95 PJK 1.100 Added fdeut to argument list
C
C--Arguments
C  beamfus0:(INPUT)  multiplier for beam-background fusion calculation
C  betbm0 : (INPUT)  leading coefficient for neutral beam beta fraction
C  bp     : (INPUT)  poloidal field (T)
C  bt     : (INPUT)  toroidal field on axis (T)
C  cnbeam : (INPUT)  neutral beam current (A)
C  dene   : (INPUT)  electron density (/m3)
C  deni   : (INPUT)  fuel ion density (/m3)
C  dlamie : (INPUT)  ion-electron coulomb logarithm
C  ealpha : (INPUT)  alpha particle birth energy (D-T) (keV)
C  enbeam : (INPUT)  neutral beam energy (keV)
C  fdeut  : (INPUT)  deuterium fraction of main plasma
C  ftr    : (INPUT)  tritium fraction of main plasma
C  ftritbm: (INPUT)  tritium fraction of neutral beam
C  sigvdt : (INPUT)  profile averaged <sigma v DT> (m3/s)
C  ten    : (INPUT)  density weighted average electron temperature (keV)
C  tin    : (INPUT)  density weighted average ion temperature (keV)
C  vol    : (INPUT)  plasma volume (m3)
C  zeffai : (INPUT)  density weighted plasma effective charge
C  betanb : (OUTPUT) neutral beam beta component
C  dnbeam2: (OUTPUT) hot beam ion density (/m3)
C  palpnb : (OUTPUT) alpha power from hot neutral beam ions (MW)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     beamfus0,betanb,betbm0,bp,bt,cnbeam,dene,deni,dlamie,
     +     dnbeam2,ealpha,enbeam,fdeut,ftr,ftritbm,palpnb,sigvdt,ten,
     +     tin,vol,zeffai

C  Local variables
      DOUBLE PRECISION
     +     denid,denit,ecritd,ecritt,ehotnb,palpdb,palptb,tausl

C  External routines
      EXTERNAL beamcalc

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Velocity slowing down time

      tausl = 1.99D19* (2.0D0*(1.0D0-ftritbm)+3.0D0*ftritbm) *
     +     ten**1.5D0/dene / dlamie

C *** Critical energy for electron/ion slowing down of the beam ion
C *** (deuterium and tritium neutral beams, respectively) (keV)

      ecritd = 14.8D0 * ten * 2.0D0 * zeffai**0.6666D0 *
     +     (dlamie+4.0D0)/dlamie
      ecritt = ecritd * 1.5D0

C *** Deuterium and tritium ion densities

C+**PJK 11/12/95      denid = deni * (1.0D0-ftr)
      denid = deni * fdeut
      denit = deni * ftr

C *** Perform calculations
C+**PJK 05/12/95 Added ealpha to argument list of BEAMCALC

      call beamcalc(denid,denit,ealpha,enbeam,ecritd,ecritt,tausl,
     +     ftritbm,cnbeam,tin,vol,sigvdt,palpdb,palptb,dnbeam2,ehotnb)

C *** Neutral beam alpha power

      palpnb = beamfus0 * (palpdb + palptb)

C *** Neutral beam beta

      betanb = betbm0 * 4.03D-22 * 0.66666D0 * dnbeam2 * ehotnb /
     +     (bt**2 + bp**2)

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE BEAMCALC(nd,nt,ealpha,ebeam,ecritd,ecritt,tausbme,
     +     ftritbm,ibeam,ti,vol,svdt,palfdb,palftb,nhot,ehot)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to calculate neutral beam alpha power and ion energy
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  05 December 1995
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C  05/12/95 PJK 1.100 Added ealpha to argument list
C
C--Arguments
C  ealpha : (INPUT)  alpha particle birth energy (D-T) (keV)
C  ebeam  : (INPUT)  beam energy (keV)
C  ecritd : (INPUT)  critical energy for electron/ion slowing down of
C                    the beam ion (deuterium neutral beam) (keV)
C  ecritt : (INPUT)  critical energy for beam slowing down
C                    (tritium neutral beam) (keV)
C  ftritbm: (INPUT)  beam tritium fraction (0.0 = deuterium beam)
C  ibeam  : (INPUT)  beam current (A)
C  nd     : (INPUT)  thermal deuterium density (/m3)
C  nt     : (INPUT)  thermal tritium density   (/m3)
C  svdt   : (INPUT)  profile averaged <sigma v DT> (m3/s)
C  tausbme: (INPUT)  beam ion slowing down time on electrons (s)
C  ti     : (INPUT)  thermal ion temperature (keV)
C  vol    : (INPUT)  plasma volume (m3) (95% flux surface)
C  ehot   : (OUTPUT) average hot beam ion energy (keV)
C  nhot   : (OUTPUT) hot beam ion density (/m3)
C  palfdb : (OUTPUT) alpha power from deut. beam-background fusion (MW)
C  palftb : (OUTPUT) alpha power from trit. beam-background fusion (MW)
C
C--Global variables passed via COMMON
C  atmd   : (OUTPUT) atomic mass of deuterium
C  atmdt  : (OUTPUT) average atomic mass of D-T
C  atmt   : (OUTPUT) atomic mass of tritium
C  echarge: (OUTPUT) electron charge (Coulomb)
C  epsabs : (OUTPUT) absolute error
C  epsrel : (OUTPUT) relative error
C  xmprotn: (OUTPUT) proton mass (kg)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     ealpha,ebeam,ecritd,ecritt,ehot,ftritbm,ibeam,nd,nhot,nt,
     +     palfdb,palftb,svdt,tausbme,ti,vol

C  Global variables
      DOUBLE PRECISION
     +     atmd,atmdt,atmt,echarge,epsabs,epsrel,xmprotn
      COMMON/const/echarge,xmprotn,atmd,atmt,epsrel,epsabs,atmdt

C  Local variables
      DOUBLE PRECISION
     +     ebmratd,ebmratt,ehotd,ehott,ifbmd,ifbmt,ndhot,nhotmsd,
     +     nhotmst,nthot,presd,prest,s0d,s0t,svdhotn,svthotn,tauseffd,
     +     tausefft,vcds,vcritd,vcritt,vcts,xcoefd,xcoeft
      INTEGER iabm

C  External functions
      DOUBLE PRECISION palphabm,sgvhot,xbrak
      EXTERNAL         palphabm,sgvhot,xbrak

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Initialise global variables

      atmd = 2.0D0
      atmdt = 2.5D0
      atmt = 3.0D0
      echarge = 1.6022D-19
      epsabs = 1.0D-7
      epsrel = 1.0D-7
      xmprotn = 1.6726D-27

C *** D and T beam current fractions

      ifbmd = ibeam * (1.0D0 - ftritbm)
      ifbmt = ibeam * ftritbm

      ebmratd = ebeam/ecritd
      vcritd = sqrt(2.0D0*echarge*1000.0D0*ecritd/(xmprotn*atmd))
      tauseffd = tausbme/3.0D0 * log(1.0D0+(ebmratd)**1.5D0)
      nhotmsd = (1.0D0-ftritbm) * ibeam * tauseffd/(echarge * vol)

      ebmratt = ebeam/ecritt
      vcritt = sqrt(2.0D0*echarge*1000.0D0*ecritt/(xmprotn*atmt))
      tausefft = tausbme/3.0D0 * log(1.0D0+(ebmratt)**1.5D0)
      nhotmst = ftritbm * ibeam * tausefft/(echarge * vol)

      nhot = nhotmsd + nhotmst
      ndhot = nhotmsd
      nthot = nhotmst

C *** Average hot ion energy from Deng & Emmert, UWFDM-718, Jan 87

      vcds = 2.0D0 * ecritd * echarge * 1000.0D0/(2.0D0 * xmprotn)
      vcts = 2.0D0 * ecritt * echarge * 1000.0D0/(3.0D0 * xmprotn)

      s0d = ifbmd/(echarge * vol)
      s0t = ifbmt/(echarge * vol)

      xcoefd = atmd * xmprotn * tausbme * vcds * s0d/
     +     (echarge * 1000.0D0 * 3.0D0)
      xcoeft = atmt * xmprotn * tausbme * vcts * s0t/
     +     (echarge * 1000.0D0 * 3.0D0)

      presd = xcoefd * xbrak(ebeam,ecritd)
      prest = xcoeft * xbrak(ebeam,ecritt)

      ehotd = 1.5D0 * presd/ndhot
      ehott = 1.5D0 * prest/nthot
      ehot = (ndhot*ehotd + nthot*ehott)/nhot

C+**PJK 10/06/92 Changed calls to SGVHOT, to prevent atmd or atmt being
C+**PJK 10/06/92 used, as these are also in COMMON.

      iabm = 2
      svdhotn = 1.0D-4 * sgvhot(iabm,vcritd,ebeam)
      iabm = 3
      svthotn = 1.0D-4 * sgvhot(iabm,vcritt,ebeam)

C+**PJK 05/12/95 Added ealpha to argument list of PALPHABM

      palfdb = palphabm(ealpha,ndhot,nt,svdhotn,vol,ti,svdt)
      palftb = palphabm(ealpha,nthot,nd,svthotn,vol,ti,svdt)

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION XBRAK(e0,ec)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  ...
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  e0     : (INPUT)  neutral beam energy (keV)
C  ec     : (INPUT)  critical energy for electron/ion slowing down of
C                    the beam ion (keV)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION e0,ec

C  Local variables
      DOUBLE PRECISION ans,t1,t2,t3,t4,xarg,xc,xcs

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      xcs = e0/ec
      xc = sqrt(xcs)

      t1 = xcs/2.0D0
      t2 = (log((xcs + 2.0D0*xc + 1.0D0)/(xcs - xc + 1.0D0)))/6.0D0

      xarg = (2.0D0*xc -1.0D0)/sqrt(3.0D0)
      t3 = (atan(xarg))/sqrt(3.0D0)
      t4 = 0.3022999D0

      ans = t1 + t2 - t3 - t4
      xbrak = ans

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION PALPHABM(ealpha,nbm,nblk,sigv,vol,ti,
     +     svdt)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Routine to calculate alpha power from beam-background fusion
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  05 December 1995
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C  05/12/95 PJK 1.100 Moved ealpha to argument list
C
C--Arguments
C  ealpha : (INPUT)  alpha particle birth energy (D-T) (keV)
C  nblk   : (INPUT)  thermal ion density (/m3)
C  nbm    : (INPUT)  hot beam ion density (/m3)
C  sigv   : (INPUT)  hot beam fusion reaction rate (m3/s)
C  svdt   : (INPUT)  profile averaged <sigma v DT> (m3/s)
C  ti     : (INPUT)  thermal ion temperature (keV)
C  vol    : (INPUT)  plasma volume (m3)
C
C--Global variables passed via COMMON
C  atmd   : (INPUT)  atomic mass of deuterium
C  atmdt  : (INPUT)  average atomic mass of D-T
C  atmt   : (INPUT)  atomic mass of tritium
C  echarge: (INPUT)  electron charge (Coulomb)
C  epsabs : (INPUT)  absolute error
C  epsrel : (INPUT)  relative error
C  xmprotn: (INPUT)  proton mass (kg)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION ealpha,nblk,nbm,sigv,svdt,ti,vol

C  Global variables
      DOUBLE PRECISION
     +     atmd,atmdt,atmt,echarge,epsabs,epsrel,xmprotn
      COMMON/const/echarge,xmprotn,atmd,atmt,epsrel,epsabs,atmdt

C  Local variables
      DOUBLE PRECISION ans,result

C  External functions
      DOUBLE PRECISION svfdt
      EXTERNAL         svfdt

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      result = svdt/svfdt(ti)

      ans = echarge/1000.0D0 * nbm * nblk * sigv * ealpha *
     +     vol * result

      palphabm = ans

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SGVHOT(iabm,vcrx,ebeam)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to find the hot beam fusion reaction rate in m3/s
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  ebeam  : (INPUT)  neutral beam energy (keV)
C  iabm   : (INPUT)  switch denoting type of ion (2=D,3=T)
C  vcrx   : (INPUT)  critical velocity for electron/ion slowing down of
C                    the beam ion (m/s)
C
C--Global variables passed via COMMON
C  atmd   : (INPUT)  atomic mass of deuterium
C  atmdt  : (INPUT)  average atomic mass of D-T
C  atmt   : (INPUT)  atomic mass of tritium
C  echarge: (INPUT)  electron charge (Coulomb)
C  epsabs : (INPUT)  absolute error
C  epsrel : (INPUT)  relative error
C  xmprotn: (INPUT)  proton mass (kg)
C  echrge : (OUTPUT) electron charge (Coulomb)
C  promass: (OUTPUT) proton mass (kg)
C  vcritx : (OUTPUT) critical velocity for electron/ion slowing down of
C                    the beam ion (m/s)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION ebeam,vcrx
      INTEGER iabm

C  Global variables
      DOUBLE PRECISION
     +     atmd,atmdt,atmt,echarge,epsabs,epsrel,xmprotn
      COMMON/const/echarge,xmprotn,atmd,atmt,epsrel,epsabs,atmdt

      DOUBLE PRECISION echrge,promass,vcritx
      COMMON /fvcrit/ vcritx,promass,echrge

C  Local variables
      DOUBLE PRECISION
     +     abm,abserr,ans,epsabs1,flag,svint,t1,t2,vbeam,vbeams,xv
      INTEGER nofun

C  External functions
      DOUBLE PRECISION fsv
      EXTERNAL         fsv

C  External routines
      EXTERNAL quanc8

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C+**PJK 10/06/92 Changed first argument from ABM to IABM.
C+**PJK 10/06/92 This avoids the use of an argument in the calling
C+**PJK 10/06/92 routine BEAMCALC that is also in COMMON.

      epsabs1 = 1.0D-33

      if (iabm.eq.2) then
         abm = atmd
      else if (iabm.eq.3) then
         abm = atmt
      else
         write(*,*) 'Error in routine SGVHOT:'
         write(*,*) 'Illegal value for IABM, = ',iabm
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Initialise global variables

      echrge = echarge
      promass = xmprotn
      vcritx = vcrx

C *** Beam velocity

      vbeams = ebeam * echarge * 1000.0D0 * 2.0D0/(abm * xmprotn)
      vbeam = sqrt(vbeams)

      xv = vbeam/vcrx
      t1 = 3.0D0 *vcrx/log(1.0D0+(xv**3))

      call quanc8(fsv,0.0D0,xv,epsabs1,epsrel,svint,abserr,nofun,flag)
      t2 = svint

      ans = t1 * t2
      sgvhot = ans

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FSV(u)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Integrand function for the hot beam fusion reaction rate
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  u      : (INPUT)  abscissa of integration, = ratio of beam velocity
C                    to the critical velocity
C
C--Global variables passed via COMMON
C  echrge : (INPUT)  electron charge (Coulomb)
C  promass: (INPUT)  proton mass (kg)
C  vcritx : (INPUT)  critical velocity for electron/ion slowing down of
C                    the beam ion (m/s)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION u

C  Global variables
      DOUBLE PRECISION echrge,promass,vcritx
      common /fvcrit/ vcritx,promass,echrge

C  Local variables
      DOUBLE PRECISION ans,t1,t2,xvc,xvcs

C  External functions
      DOUBLE PRECISION sigbmfus
      EXTERNAL         sigbmfus

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      t1 = (u**3)/(1.0D0+u**3)

      xvc = vcritx*u
      xvcs = xvc * xvc * promass/(echrge * 1000.0D0)
      t2 = sigbmfus(xvcs)

      ans = t1 * t2
      fsv = ans

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION SIGBMFUS(vrelsq)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  This function evaluates the fusion reaction cross-section as a
C  function of beam ion velocity (squared).
C  Functional form of the cross-section is in terms of the equivalent
C  deuterium energy, i.e. for a tritium beam at 500 keV the energy
C  used in the cross-section function is 333 keV.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  vrelsq : (INPUT)  square of the speed of the beam ion (keV/amu)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION vrelsq

C  Local variables
      DOUBLE PRECISION a1,a2,a3,a4,a5,ans,atmd,ebm,t1,t2

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      a1 = 45.95D0
      a2 = 5.02D4
      a3 = 1.368D-2
      a4 = 1.076D0
      a5 = 4.09D2

C *** Deuterium atomic mass

      atmd = 2.0D0

      ebm = 0.5D0 * atmd * vrelsq

C *** Set limits on cross-section at low and high beam energies

      if (ebm.lt.10.0D0) then

         sigbmfus = 1.0D-27

      else if (ebm.gt.1.0D4) then

         sigbmfus = 8.0D-26

      else

         t1 = a2/(1.0D0 + (a3 * ebm - a4)**2) + a5
         t2 = ebm * (exp (a1/sqrt(ebm)) - 1.0D0)
         ans = 1.0D-24 * t1/t2
         sigbmfus = ans

      end if

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION PLASC(qbar,aspect,rminor,bt,kappa,delta)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Function to calculate plasma current in MA.
C  From M Peng's notes, 24 February 1989
C
C  This scaling is primarily used for Tight Aspect Ratio Tokamaks
C  (icurr=2).
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  aspect : (INPUT)  plasma aspect ratio
C  bt     : (INPUT)  toroidal field on axis (T)
C  delta  : (INPUT)  plasma triangularity
C  kappa  : (INPUT)  plasma elongation
C  qbar   : (INPUT)  edge q-bar
C  rminor : (INPUT)  plasma minor radius (m)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION pi
      PARAMETER (pi = 3.1415926536D0)

C  Arguments
      DOUBLE PRECISION
     +     aspect,bt,delta,kappa,qbar,rminor

C  Local variables
      DOUBLE PRECISION
     +     c1,c2,d1,d2,eps,e1,e2,f1,f2,ff1,ff2,g,h1,h2,y1,y2

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      eps = 1.0D0/aspect

C *** Some factors

      c1 = kappa**2/(1.0D0+delta) + delta
      c2 = kappa**2/(1.0D0-delta) - delta

      d1 = (kappa/(1.0D0+delta))**2 + 1.0D0
      d2 = (kappa/(1.0D0-delta))**2 + 1.0D0

      if (aspect.lt.c1) then
         y1 = sqrt( (c1*eps - 1.0D0)/(1.0D0+eps) ) *
     +        (1.0D0 + delta)/kappa
      else
         y1 = sqrt( (1.0D0 - c1*eps)/(1.0D0+eps) ) *
     +        (1.0D0 + delta)/kappa
      end if
      y2 = sqrt( (c2*eps+1.0D0)/(1.0D0-eps) ) * (1.0D0-delta)/kappa

      e1 = 2.0D0*kappa/(d1*(1.0D0+delta))
      e2 = 2.0D0*kappa/(d2*(1.0D0-delta))

      h2 = (1.0D0 + (c2-1.0D0)*eps/2.0D0) / sqrt( (1.0D0-eps)*
     +     (c2*eps+1.0D0))
      f2 = (d2*(1.0D0-delta)*eps) / ( (1.0D0-eps)*(c2*eps+1.0D0) )
      g = eps*kappa / (1.0D0 - eps*delta)
      ff2 = f2 * (g + 2.0D0*h2*atan(y2) )

      if (aspect.lt.c1) then
         h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / sqrt( (1.0D0+eps)*
     +        (c1*eps-1.0D0) )
         f1 = (d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
         ff1 = f1*(g - h1*log( (1.0D0+y1)/(1.0D0-y1) ) )
      else
         h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / sqrt( (1.0D0+eps)*
     +        (1.0D0-c1*eps) )
         f1 = -(d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
         ff1 = f1*( -g + 2.0D0*h1*atan(y1) )
      end if

      plasc = rminor*bt/qbar * 5.0D0*kappa/(2.0D0*pi**2) *
     +     ( asin(e1)/e1 + asin(e2)/e2 ) * (ff1 + ff2)

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION BPOL(qbar,aspect,bt,kappa,delta)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Function to calculate the poloidal field.
C  From M Peng's notes, 24 February 1989
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  aspect : (INPUT)  plasma aspect ratio
C  bt     : (INPUT)  toroidal field on axis (T)
C  delta  : (INPUT)  plasma triangularity
C  kappa  : (INPUT)  plasma elongation
C  qbar   : (INPUT)  edge q-bar
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION pi
      PARAMETER (pi = 3.1415926536D0)

C  Arguments
      DOUBLE PRECISION aspect,bt,delta,kappa,qbar

C  Local variables
      DOUBLE PRECISION
     +     c1,c2,d1,d2,eps,e1,e2,f1,f2,ff1,ff2,g,h1,h2,y1,y2

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      eps = 1.0D0/aspect

C  Some factors

      c1 = kappa**2/(1.0D0+delta) + delta
      c2 = kappa**2/(1.0D0-delta) - delta

      d1 = (kappa/(1.0D0+delta))**2 + 1.0D0
      d2 = (kappa/(1.0D0-delta))**2 + 1.0D0

      if (aspect.lt.c1) then
         y1 = sqrt( (c1*eps - 1.0D0)/(1.0D0+eps) ) *
     +        (1.0D0 + delta)/kappa
      else
         y1 = sqrt( (1.0D0 - c1*eps)/(1.0D0+eps) ) *
     +        (1.0D0 + delta)/kappa
      end if
      y2 = sqrt( (c2*eps+1.0D0)/(1.0D0-eps) ) * (1.0D0-delta)/kappa

      h2 = (1.0D0 + (c2-1.0D0)*eps/2.0D0) / sqrt( (1.0D0-eps)*
     +     (c2*eps+1.0D0))
      f2 = (d2*(1.0D0-delta)*eps) / ( (1.0D0-eps)*(c2*eps+1.0D0) )
      g = eps*kappa / (1.0D0 - eps*delta)
      ff2 = f2 * (g + 2.0D0*h2*atan(y2) )

      if (aspect.lt.c1) then
         h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / sqrt( (1.0D0+eps)*
     +        (c1*eps-1.0D0) )
         f1 = (d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
         ff1 = f1*(g - h1*log( (1.0D0+y1)/(1.0D0-y1) ) )
      else
         h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / sqrt( (1.0D0+eps)*
     +        (1.0D0-c1*eps) )
         f1 = -(d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
         ff1 = f1*( -g + 2.0D0*h1*atan(y1) )
      end if

      bpol = bt * (ff1 + ff2) / (2.0D0 * pi * qbar)

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION FNEWBS(alphan,alphat,betat,bt,dene,
     +     plascur,q95,q0,rmajor,rminor,ten,zeff)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Subroutine to calculate bootstrap scaling using Nevins et al
C  method, 4/11/90
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  alphan : (INPUT)  density profile index
C  alphat : (INPUT)  temperature profile index
C  betat  : (INPUT)  total plasma beta (with respect to the toroidal
C                    field)
C  bt     : (INPUT)  toroidal field on axis (T)
C  dene   : (INPUT)  electron density (/m3)
C  plascur: (INPUT)  plasma current (A)
C  q0     : (INPUT)  central safety factor
C  q95    : (INPUT)  safety factor at 95% surface
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  ten    : (INPUT)  density weighted average plasma temperature (keV)
C  zeff   : (INPUT)  plasma effective charge
C
C--Global variables passed via COMMON
C  alphanz: (OUTPUT) density profile index
C  alphatz: (OUTPUT) temperature profile index
C  betaz  : (OUTPUT) total plasma beta (with respect to the toroidal
C                    field)
C  btz    : (OUTPUT) toroidal field on axis (T)
C  denez  : (OUTPUT) electron density (/m3)
C  plascurz:(OUTPUT) plasma current (MA)
C  qaxis  : (OUTPUT) central safety factor
C  qpsi   : (OUTPUT) safety factor at 95% surface
C  rmajorz: (OUTPUT) plasma major radius (m)
C  rminorz: (OUTPUT) plasma minor radius (m)
C  tez    : (OUTPUT) density weighted average plasma temperature (keV)
C  zeffz  : (OUTPUT) plasma effective charge
C  
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION rmu0
      PARAMETER (rmu0 = 1.25664D-6)

C  Arguments
      DOUBLE PRECISION
     +     alphan,alphat,betat,bt,dene,plascur,q0,q95,rmajor,rminor,
     +     ten,zeff

C  Global variables
      DOUBLE PRECISION alphanz,alphatz,betaz,btz,denez,plascurz,qaxis,
     +     qpsi,rmajorz,rminorz,tez,zeffz
      COMMON/bss/ alphanz,alphatz,betaz,btz,denez,plascurz,qpsi,qaxis,
     +     rmajorz,tez,rminorz,zeffz

C  Local variables
      DOUBLE PRECISION aibs,ainteg,betae0,dum1,fibs,flag
      INTEGER nofun

C  External functions
      DOUBLE PRECISION bsinteg
      EXTERNAL         bsinteg

C  External routines
      EXTERNAL quanc8

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Initialise global variables

      alphanz = alphan
      alphatz = alphat
      betaz   = betat
      btz     = bt
      denez   = dene
      plascurz = plascur/1.0D6
      qaxis   = q0
      qpsi    = q95
      rmajorz = rmajor
      rminorz = rminor
      tez     = ten
      zeffz   = zeff

C *** Calculate peak electron beta

      betae0 = denez * tez *1.6022D-16/ ( btz**2 /(2.0D0*rmu0) )*
     +     (1.0D0+alphanz+alphatz)

C *** Call integration routine

C+**PJK 02/06/92 Corrected error in argument list of QUANC8.
C+**PJK 02/06/92 'flag' was wrongly written 'iflag'

      call quanc8(bsinteg,0.0D0,0.999D0,0.001D0,0.001D0,ainteg,dum1,
     +     nofun,flag)

C *** Calculate bootstrap current

      aibs = 2.5D0 * betae0 * rmajor * btz * qpsi * ainteg
      fibs = aibs / plascurz

      fnewbs = fibs

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION BSINTEG(y)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Integrand function for Nevins et al bootstrap current scaling
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 June 1994
C
C--Reference
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C
C--Arguments
C  y      : (INPUT)  abscissa of integration, = normalised minor radius
C
C--Global variables passed via COMMON
C  alphanz: (INPUT)  density profile index
C  alphatz: (INPUT)  temperature profile index
C  betaz  : (INPUT)  total plasma beta (with respect to the toroidal
C                    field)
C  btz    : (INPUT)  toroidal field on axis (T)
C  denez  : (INPUT)  electron density (/m3)
C  plascurz:(INPUT)  plasma current (MA)
C  qaxis  : (INPUT)  central safety factor
C  qpsi   : (INPUT)  safety factor at 95% surface
C  rmajorz: (INPUT)  plasma major radius (m)
C  rminorz: (INPUT)  plasma minor radius (m)
C  tez    : (INPUT)  density weighted average plasma temperature (keV)
C  zeffz  : (INPUT)  plasma effective charge
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Parameters
      DOUBLE PRECISION rmu0
      PARAMETER (rmu0 = 1.25664D-6)

C  Arguments
      DOUBLE PRECISION y

C  Global variables
      DOUBLE PRECISION alphanz,alphatz,betaz,btz,denez,plascurz,qaxis,
     +     qpsi,rmajorz,rminorz,tez,zeffz
      COMMON/bss/ alphanz,alphatz,betaz,btz,denez,plascurz,qpsi,qaxis,
     +     rmajorz,tez,rminorz,zeffz

C  Local variables
      DOUBLE PRECISION
     +     alphai,al1,al2,a1,a2,betae,c1,c2,c3,d,del,pratio,q,x,z

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Constants for fit to q-profile

      c1 = 1.0D0
      c2 = 1.0D0
      c3 = 1.0D0

C *** Compute average electron beta

      betae = denez*tez*1.6022D-16/(btz**2/(2.0D0*rmu0))

      del = rminorz*sqrt(y)/rmajorz
      x = (1.46D0*sqrt(del) + 2.4D0*del)/(1.0D0 - del)**1.5D0
      z = zeffz
      d = 1.414D0*z + z*z + x*(0.754D0 + 2.657D0*z + 2.0D0*z*z)
     +     + x*x*(0.348D0 + 1.243D0*z + z*z)
      al2 = -x*(0.884D0 + 2.074D0*z)/d
      a2 = alphatz*(1.0D0-y)**(alphanz+alphatz-1.0D0)
      alphai = -1.172D0/(1.0D0+ 0.462D0*x)
      a1 = (alphanz+alphatz)*(1.0D0-y)**(alphanz+alphatz-1.0D0)
      al1 = x*(0.754D0+2.21D0*z+z*z+x*(0.348D0+1.243D0*z+z*z))/d

C *** Q profile

      q = qaxis + (qpsi-qaxis)*(c1*y + c2*y*y + c3*y**3)/(c1+c2+c3)

      pratio = (betaz - betae) / betae

      bsinteg = (q/qpsi)*(al1*(a1 + pratio*(a1+alphai*a2) ) + al2*a2 )

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION CULBST(alphaj,alphan,alphat,beta,
     +     betpth,q0,qpsi,rmajor,rminor,itart)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.010
C
C--Description
C  Subroutine to calculate the bootstrap current fraction
C  using the algorithm written by Howard Wilson and described
C  in AEA FUS 172.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  14 May 1996
C
C--Reference
C  AEA FUS 172: Physics Assessment for the European Reactor Study
C  AEA FUS 251: A User's Guide to the PROCESS Systems Code
C  
C--History
C  22/06/94 PJK 1.000 Upgrade to higher standard of coding
C  14/05/96 PJK 1.010 Modified to use THERMAL poloidal beta, and
C                     added diamagnetic term at tight aspect ratio
C
C--Arguments
C  alphaj  : (INPUT)  current profile index
C  alphan  : (INPUT)  density profile index
C  alphat  : (INPUT)  temperature profile index
C  beta    : (INPUT)  total beta
C  betpth  : (INPUT)  thermal component of poloidal beta
C  q0      : (INPUT)  safety factor on axis
C  qpsi    : (INPUT)  edge safety factor
C  rmajor  : (INPUT)  major radius (m)
C  rminor  : (INPUT)  minor radius (m)
C  itart   : (INPUT)  switch denoting tight aspect ratio option
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION
     +     alphaj,alphan,alphat,beta,betpth,q0,qpsi,rmajor,rminor
      INTEGER itart

C  Local variables
      DOUBLE PRECISION
     +     a(12),aj,alfpnw,alftnw,alphap,b(12),eps1,r1,r2,
     +     saj,seps1,sum,termj,termp,termt,term1,term2,Z
      INTEGER i

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Convert density profile index to pressure profile index.
C *** From ideal gas law : p = nkT

      alphap = alphan + alphat

C *** Check for illegal argument values

      if (alphaj.le.0.0D0) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for alphaj, = ',alphaj
         write(*,*) 'PROCESS stopping.'
         STOP
      end if
      if (alphap.le.0.0D0) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for alphap, = ',alphap
         write(*,*) 'PROCESS stopping.'
         STOP
      end if
      if (alphat.le.0.0D0) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for alphat, = ',alphat
         write(*,*) 'PROCESS stopping.'
         STOP
      end if
      if (qpsi.le.0.0D0) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for qpsi, = ',qpsi
         write(*,*) 'PROCESS stopping.'
         STOP
      end if
      if (q0.le.0.0D0) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for q0, = ',q0
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** alphap, alphat and alphaj are indices relevant to profiles of
C *** the form
C ***            p = p0.(1-(r/a)**2)**alphap, etc.
C *** 
C *** Convert these indices to those relevant to profiles of the form
C ***            p = p0.psi**alfpnw, etc.

      term1 = log(0.5D0)
      term2 = log(q0/qpsi)

      termp = 1.0D0-0.5D0**(1.0D0/alphap)
      termt = 1.0D0-0.5D0**(1.0D0/alphat)
      termj = 1.0D0-0.5D0**(1.0D0/alphaj)

      alfpnw = term1/log( log( (q0+(qpsi-q0)*termp)/qpsi )/term2)
      alftnw = term1/log( log( (q0+(qpsi-q0)*termt)/qpsi )/term2)
      aj     = term1/log( log( (q0+(qpsi-q0)*termj)/qpsi )/term2)

C *** Crude check for NaN errors...

      if (aj.gt.9.99D99) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for aj, = ',aj
         write(*,*) 'PROCESS stopping.'
         STOP
      end if
      if (alfpnw.gt.9.99D99) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for alfpnw, = ',alfpnw
         write(*,*) 'PROCESS stopping.'
         STOP
      end if
      if (alftnw.gt.9.99D99) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for alftnw, = ',alftnw
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

      if (aj.le.0.0D0) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for aj, = ',aj
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Ratio of ionic charge to electron charge

      Z = 1.0D0

C *** Inverse aspect ratio: r2 = maximum plasma radius, r1 = minimum

      r2 = rmajor+rminor
      r1 = rmajor-rminor
      eps1 = (r2-r1)/(r2+r1)

      if (eps1.le.0.0D0) then
         write(*,*) 'Error in routine CULBST:'
         write(*,*) 'Illegal value for eps1, = ',eps1
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Coefficients fitted using least squares techniques

      saj = sqrt(aj)

      a(1)  =    1.41D0*(1.0D0-0.28D0*saj)*(1.0D0+0.12D0/Z)
      a(2)  =    0.36D0*(1.0D0-0.59D0*saj)*(1.0D0+0.8D0/Z)
      a(3)  =   -0.27D0*(1.0D0-0.47D0*saj)*(1.0D0+3.0D0/Z)
      a(4)  =  0.0053D0*(1.0D0+5.0D0/Z)
      a(5)  =   -0.93D0*(1.0D0-0.34D0*saj)*(1.0D0+0.15D0/Z)
      a(6)  =   -0.26D0*(1.0D0-0.57D0*saj)*(1.0D0-0.27D0*Z)
      a(7)  =   0.064D0*(1.0D0-0.6D0*aj+0.15D0*aj*aj)*(1.0D0+7.6D0/Z)
      a(8)  = -0.0011D0*(1.0D0+9.0D0/Z)
      a(9)  =   -0.33D0*(1.0D0-aj+0.33D0*aj*aj)
      a(10) =   -0.26D0*(1.0D0-0.87D0/saj-0.16D0*aj)
      a(11) =   -0.14D0*(1.0D0-1.14D0/saj-0.45D0*saj)
      a(12) = -0.0069D0

      seps1 = sqrt(eps1)

      b(1)  = 1.0D0
      b(2)  = alfpnw
      b(3)  = alftnw
      b(4)  = alfpnw*alftnw
      b(5)  = seps1
      b(6)  = alfpnw*seps1
      b(7)  = alftnw*seps1
      b(8)  = alfpnw*alftnw*seps1
      b(9)  = eps1
      b(10) = alfpnw*eps1
      b(11) = alftnw*eps1
      b(12) = alfpnw*alftnw*eps1

      sum = 0.0D0
      do 10 i = 1,12
         sum = sum+a(i)*b(i)
 10   continue

C *** Empirical bootstrap current fraction

      culbst = seps1 * betpth * sum

C *** Diamagnetic contribution to the bootstrap fraction
C *** at tight aspect ratio.
C *** Tim Hender fit

      if (itart.eq.1) then
         culbst = culbst + beta/2.8D0
      end if

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE FPOWER(ftrit,fdeut,fhe3,deni,ti,pi,iopt,palp,pcharge,
     +     pneut,sigmav)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.100
C
C--Description
C  Subroutine to calculate the fusion power given to the alpha
C  particles, neutrons and charged particles generated in the
C  following fusion processes:
C  
C  iopt = 1 : D-T reaction
C  iopt = 2 : D-3He reaction
C  iopt = 3 : D-D 1st reaction (50% probability)
C  iopt = 4 : D-D 2nd reaction (50% probability)
C
C  It is assumed that alphas and the protons give up all their energy
C  to the plasma.
C
C  The reaction rate equation is taken from:
C  'Fusion cross sections and thermonuclear reaction rates'
C  Asher Peres, J.Applied Physics 50(9), pp.5569-71, September 1979
C  The values are quoted to be valid for ti = 0 to 100 keV.
C
C--Author
C  Matthew Gardner D3 Culham
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  05 December 1995
C
C--Reference
C  As above
C  
C--History
C  20/04/95 MRG 1.000 Initial version (ypbeta.f)
C  04/05/95 MRG 1.010 Added N_LOCAL and T_LOCAL to arguments
C  05/12/95 PJK 1.100 Modified for inclusion into PROCESS code
C
C--Arguments
C  ftrit   : (INPUT)  tritium fuel fraction
C  fdeut   : (INPUT)  deuterium fuel fraction
C  fhe3    : (INPUT)  helium-3 fuel fraction
C  deni    : (INPUT)  fuel ion number density (/m3)
C  ti      : (INPUT)  ion temperature (keV)
C  pi      : (INPUT)  3.14...
C  iopt    : (INPUT)  flag for fusion reaction to use (see above)
C  palp    : (OUTPUT) alpha particle fusion power (MW/m3)
C  pcharge : (OUTPUT) other charged particle fusion power (MW/m3)
C  pneut   : (OUTPUT) neutron fusion power (MW/m3)
C  sigmav  : (OUTPUT) fusion reaction rate (m3/s)
C  
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER iopt
      DOUBLE PRECISION ftrit,fdeut,fhe3,deni,ti,pi
      DOUBLE PRECISION palp,pcharge,pneut,sigmav

C  Local variables
      DOUBLE PRECISION const,eta,num1,num2,reactrt,theta,wht,etot
      DOUBLE PRECISION mc2,p1,p2,p3,p4,p5,p6,fuspow,ch2

C  External functions
      DOUBLE PRECISION pade
      EXTERNAL         pade

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** const is the fine structure constant x pi

      const = pi * 7.2974D-3

C *** Check to see if ion temperature is zero

      if (ti.eq.0.0D0) then
         WRITE(*,*) 'Error in routine FPOWER:'
         WRITE(*,*) 'Ion temperature is zero.'
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** Check to see if ion temperature exceeds 200 keV for D-3He option

      if ((ti.gt.200.0D0).and.(iopt.eq.2)) then
         WRITE(*,*) 'Error in routine FPOWER:'
         WRITE(*,*) 'Ion temperature has exceeded 200 keV.'
         WRITE(*,*) 'Reactivity D-3He equation is no longer valid.'
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

C *** wht is the the probability weighting
C *** etot is the total fusion energy produced (J)
C *** ch2 is the product of the two reactant charges

      if (iopt.eq.1) then

C *** D + T --> 4He + n reaction

         num1 = fdeut*deni
         num2 = ftrit*deni

         ch2 = 1.0D0
         wht = 1.0D0
         etot = 17.58D6 * 1.6022D-19
         mc2 = 1124656.0D0
         p1 = 9.494748D-10
         p2 = 2.818421D-2
         p3 = 6.166184D-2
         p4 = 2.834474D-3
         p5 = 8.955113D-3
         p6 = -5.734052D-5

      else if (iopt.eq.2) then

C *** D + 3He --> 4He + p reaction

         num1 = fdeut*deni
         num2 = fhe3*deni

         ch2 = 2.0D0
         wht = 1.0D0
         etot = 18.34D6 * 1.6022D-19
         mc2 = 1124572.0D0
         p1 = 5.817887D-10
         p2 = 8.681070D-3
         p3 = 1.010217D-3
         p4 = -3.516677D-6
         p5 = 3.671273D-4
         p6 = 0.0D0

      else if (iopt.eq.3) then

C *** D + D --> 3He + n reaction

         num1 = fdeut*deni/2.0D0
         num2 = num1

         ch2 = 1.0D0
         wht = 0.5D0
         etot = 3.28D6 * 1.6022D-19
         mc2 = 937814.0D0
         p1 = 5.397811D-12
         p2 = 3.328985D-3
         p3 = 2.948735D-3
         p4 = 0.0D0
         p5 = 0.0D0
         p6 = 0.0D0

      else

C *** D + D --> T + p reaction

         num1 = fdeut*deni/2.0D0
         num2 = num1

         ch2 = 1.0D0
         wht = 0.5D0
         etot = 4.03D6 * 1.6022D-19
         mc2 = 937814.0D0
         p1 = 5.988513D-12
         p2 = 2.543079D-3
         p3 = 2.216964D-3
         p4 = 0.0D0
         p5 = 0.0D0
         p6 = 0.0D0

      end if

      theta = pade(ti,p2,p3,p4,p5,p6)

      eta = (ch2**2 * const**2 * mc2 /(2.0D0*theta))**(0.333333333D0)

C *** Reaction rate <sigma.v> (cm**3 / sec)

      reactrt = p1 * theta * sqrt(eta/(mc2 * ti**3)) *
     +     exp(-3.0D0*eta)

C *** Convert to m3/s

      sigmav = reactrt * 1.0D-6

C *** Total fusion power (MW/m3)

C+**PJK 30/01/96 Multiplied by wht
      fuspow = 1.0D-12 * reactrt * etot * num1 * num2 * wht

C *** Split this power into the various particle powers

      if (iopt.eq.1) then
         palp = 0.2D0 * fuspow
         pneut = 0.8D0 * fuspow
         pcharge = 0.0D0
      else if (iopt.eq.2) then
         palp = 0.2D0 * fuspow
         pneut = 0.0D0
         pcharge = 0.8D0 * fuspow
      else if (iopt.eq.3) then
         palp = 0.0D0
         pneut = 0.75D0 * fuspow
         pcharge = 0.25D0 * fuspow
      else
         palp = 0.0D0
         pneut = 0.0D0
         pcharge = 1.0D0 * fuspow
      end if

      return
      end
C----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION PADE(T,P2,P3,P4,P5,P6)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the Pade approximant used by routine FPOWER
C  in calculating the fusion reaction rate.
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  05 December 1995
C
C--Reference
C  'Fusion cross sections and thermonuclear reaction rates'
C  Asher Peres, J.Applied Physics 50(9), pp.5569-71, September 1979
C  
C--History
C  05/12/95 PJK 1.000 Initial version
C
C--Arguments
C  T        : (INPUT)  local temperature (keV)
C  P2       : (INPUT)  parameter
C  P3       : (INPUT)  parameter
C  P4       : (INPUT)  parameter
C  P5       : (INPUT)  parameter
C  P6       : (INPUT)  parameter
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      DOUBLE PRECISION t,p2,p3,p4,p5,p6

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      pade = t / ( 1.0D0 - t*( p2 + t*(p4 + t*p6) )/
     +                      ( 1.0D0 + t*(p3 + t*p5) ) )

      return
      end
