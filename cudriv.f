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
C  Module name    : $RCSfile: cudriv.f,v $
C  Version no.    : $Revision: 3.9 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE CUDRIV(nout,iprint)

C  This subroutine calculates the current drive power requirements.
C
C  OUTPUT:
C  cnbeam ... neutral beam current (A)
C  echpwr ... ech power (W)
C  enbeam ... required neutral beam energy (keV)
C  pinje  ... steady state current drive power to electrons (W)
C  pinji  ... steady state current drive power to ions (W)
C  pnbeam ... neutral beam power (W)
C  plhybd ... lower hybrid power (W)

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'cdriv.h'
      INCLUDE 'osections.h'

      DOUBLE PRECISION dene20,effnbss,effofss,effrfss,fpion,fshine,
     +     gamnb,gamof,gamrf

      INTEGER iprint,nout

      echpwr = 0.0D0
      pnbeam = 0.0D0
      plhybd = 0.0D0
      pofcd  = 0.0D0
      cnbeam = 0.0D0

      if (irfcd.eq.0) goto 10

      dene20 = dene * 1.0D-20

C  Calculate current drive efficiencies in units of Amps/Watt.

C+**PJK 12/06/92 Replaced computed-GOTO statement with IF structure.

      if (iefrf.eq.1) then
C        Fenstermacher Lower Hybrid model

         effrfss = (0.36D0*(1.0D0 + (te/25.0D0)**1.16D0))/
     +        (rmajor*dene20) * feffcd

      else if (iefrf.eq.2) then
C        Ion-Cyclotron current drive

C+**PJK 12/06/92 Added FEFFCD to formula for ICCD efficiency.
C+**PJK 16/06/92 Changed the multiplying factor in the ICCD formula
C+**PJK 16/06/92 from 0.70 to 0.63 to agree with the ITER Physics
C+**PJK 16/06/92 Design Guidelines.

         effrfss = 0.63D0 * ten/10.0D0 / (2.0D0 + zeff) /
     +        (rmajor*dene20) * feffcd

      else if (iefrf.eq.3) then
C        Fenstermacher Electron Cyclotron Resonance model

         effrfss = 0.21D0 * ten/ (rmajor * dene20 * dlamee) * feffcd

      else if (iefrf.eq.4) then
C        Ehst Lower Hybrid / Fast Wave current drive

         effrfss = te**0.77D0 * (0.034D0 + 0.196D0 * beta) /
     +        (rmajor*dene20) * ( 32.0D0/(5.0D0+zeff) + 2.0D0 + 
     +        (12.0D0*(6.0D0+zeff))/(5.0D0+zeff)/(3.0D0+zeff) +
     +        3.76D0/zeff) / 12.507D0 * feffcd

      else if (iefrf.eq.5) then
C        ITER Neutral Beam current drive

C+**PJK 15/06/92 Moved ITER NBI calculations into new routine ITERNB.

         call iternb(
     +        abeam,alphan,alphat,aspect,dene,deni,dlamie,enbeam,eps,
     +        feffcd,frbeam,ftr,ralpne,rmajor,rncne,rnfene,rnone,te,ten,
     +        zeff,zeffai,
     +        effnbss,fpion,fshine,taubeam)

      else if (iefrf.eq.6) then
C+**PJK 15/06/92 Added new Lower Hybrid CD model

         call cullhy(
     +     alphan,alphat,bt,dene,feffcd,rmajor,rminor,te,zeff,
     +     effrfss)

      else if (iefrf.eq.7) then
C+**PJK 16/06/92 Added new ECCD model

         call culecd(
     +     alphan,alphat,dene,feffcd,rmajor,rminor,te,zeff,
     +     effrfss)

      else if (iefrf.eq.8) then
C+**PJK 16/06/92 Added new Neutral Beam model
C+**PJK 01/04/98 Added dnla to CULNBI argument list

         call culnbi(
     +     abeam,alphan,alphat,aspect,dene,deni,dlamie,dnla,enbeam,eps,
     +     feffcd,frbeam,ftr,ralpne,rmajor,rminor,rncne,rnfene,rnone,
     +     te,ten,zeff,zeffai,
     +     effnbss,fpion,fshine,taubeam)

      else if (iefrf.eq.9) then
C+**PJK 06/03/96 Added new RFP OFCD model (trivial):
C+**PJK 06/03/96 Current drive efficiency = 0.8 A/W (TITAN quoted)

         effofss = 0.8D0

      else
C        Illegal value of IEFRF
         WRITE(*,*) 'Error in routine CUDRIV:'
         WRITE(*,*) 'Illegal value of IEFRF, = ',iefrf
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

C  Compute current drive powers (Watts)

C+**PJK 15/06/92 Added IEFRF=6
      if ( (iefrf.eq.1).or.
     +     (iefrf.eq.2).or.
     +     (iefrf.eq.4).or.
     +     (iefrf.eq.6)) then
C        LHCD or ICCD

         plhybd = faccd * plascur / effrfss + pheat
         pinji = 0.0D0
         pinje = plhybd

C+**PJK 16/06/92 Added IEFRF=7
      else if ((iefrf.eq.3).or.(iefrf.eq.7)) then
C        ECCD

         echpwr = faccd * plascur / effrfss + pheat
         pinji = 0.0D0
         pinje = echpwr

C+**PJK 17/06/92 Added IEFRF=8
      else if ((iefrf.eq.5).or.(iefrf.eq.8)) then
C        NBCD

         pnbeam = faccd * plascur / effnbss + pheat
         pinji = pnbeam * fpion
         pinje = pnbeam * (1.0D0-fpion)

C        Calculate neutral beam current
         if (ABS(pnbeam).gt.1.0D-8) then
            cnbeam = 1.0D-3 * pnbeam / enbeam
         else
            cnbeam = 0.0D0
         end if

C+**PJK 06/03/96 Added IEFRF=9
      else if (iefrf.eq.9) then
C        OFCD

         pofcd = faccd * plascur / effofss + pheat
         pinji = 0.0D0
         pinje = pofcd

      else
C        Illegal value of IEFRF
         WRITE(*,*) 'Error in routine CUDRIV:'
         WRITE(*,*) 'Illegal value of IEFRF, = ',iefrf
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

C  Ratio of fusion to injection power

C+**PJK 05/04/94 Additional code to prevent bigq from being infinite

      if ((pinje+pinji).eq.0.0D0) then
         bigq = 1.0D18
      else
         bigq = 1.0D6*powfmw / (pinje+pinji)
      end if

C  Normalised current drive efficiency
      if (abs(plhybd).gt.1.0D-8) then
         gamrf = effrfss * ( dene*rmajor/1.0D20)
         gamcd = gamrf
      end if

      if (abs(pnbeam).gt.1.0D-8) then
         gamnb = effnbss * (dene * rmajor/1.0D20)
         gamcd = gamnb
      end if

      if (abs(pofcd).gt.1.0D-8) then
         gamof = effofss * (dene * rmajor/1.0D20)
         gamcd = gamof
      end if

      if (iprint.ne.1) goto 1000

C  Output section
 10   continue

      if (sect04.eq.0) goto 1000

      call oheadr(nout,'Current Drive System')

      if (irfcd.eq.0) then
         call ocmmnt(nout,'No current drive used')
         call oblnkl(nout)
         goto 1000
      end if

C+**PJK 15/06/92 Added IEFRF=6
      if ((iefrf.eq.1).or.(iefrf.eq.4).or.(iefrf.eq.6)) then
         call ocmmnt(nout,'Lower Hybrid Current Drive')
         call oblnkl(nout)

      else if (iefrf.eq.2) then
         call ocmmnt(nout,'Ion Cyclotron Current Drive')
         call oblnkl(nout)

C+**PJK 16/06/92 Added IEFRF=7
      else if ((iefrf.eq.3).or.(iefrf.eq.7)) then
         call ocmmnt(nout,'Electron Cyclotron Current Drive')
         call oblnkl(nout)

C+**PJK 17/06/92 Added IEFRF=8
      else if ((iefrf.eq.5).or.(iefrf.eq.8)) then
         call ocmmnt(nout,'Neutral Beam Current Drive')
         call oblnkl(nout)

C+**PJK 06/03/96 Added IEFRF=9
      else if (iefrf.eq.9) then
         call ocmmnt(nout,'Oscillating Field Current Drive')
         call oblnkl(nout)

      else
C        Illegal value of IEFRF
         WRITE(*,*) 'Error in routine CUDRIV:'
         WRITE(*,*) 'Illegal value of IEFRF, = ',iefrf
         WRITE(*,*) 'PROCESS stopping.'
         STOP
      end if

      if (abs(facoh).gt.1.0D-8) then
         call ocmmnt(nout,'Current is driven by both inductive')
         call ocmmnt(nout,'and non-inductive means.')
      end if

      call ovarin(nout,'Current drive efficiency model','(iefrf)',iefrf)
      call ovarre(nout,'Steady state power requirement (W)',
     +     '(pinje+pinji)',pinje+pinji)
      call ovarre(nout,'CD power used for plasma heating only (W)',
     +     '(pheat)',pheat)
      call ovarre(nout,'Energy multiplication factor Q','(bigq)',bigq)

      call osubhd(nout,'Fractions of current drive :')
      call ovarrf(nout,'Bootstrap fraction','(bootipf)',bootipf)
      call ovarrf(nout,'Auxiliary current drive fraction',
     +     '(faccd)',faccd)
      call ovarrf(nout,'Inductive fraction','(facoh)',facoh)

      if (ABS(bootipf-bscfmax).lt.1.0D-8) then
         call ocmmnt(nout,'Warning : bootstrap current fraction is at')
         call ocmmnt(nout,'          its prescribed maximum.')
      end if

      call oblnkl(nout)

      if (abs(plhybd).gt.1.0D-8) then
         call ovarre(nout,'RF efficiency (A/W)','(effrfss)',effrfss)
         call ovarre(nout,'RF gamma (A/W-m2)','(gamrf)',gamrf)
         call ovarre(nout,'Lower hybrid power (W)','(plhybd)',plhybd)
      end if

      if (abs(pnbeam).gt.1.0D-8) then
         call ovarre(nout,'Beam efficiency (A/W)','(effnbss)',effnbss)
         call ovarre(nout,'Beam gamma (A/W-m2)','(gamnb)',gamnb)
         call ovarre(nout,'Neutral beam power (W)','(pnbeam)',pnbeam)
         call ovarre(nout,'Neutral beam energy (keV)','(enbeam)',enbeam)
         call ovarre(nout,'Neutral beam current (A)','(cnbeam)',cnbeam)
         call ovarre(nout,'Fraction of beam energy to ions','(fpion)',
     +        fpion)
         call ovarre(nout,'Neutral beam shine-through','(fshine)',
     +        fshine)
         call ovarre(nout,'R injection tangent / R-major','(frbeam)',
     +        frbeam)
         call ovarre(nout,'Beam decay lengths to centre','(taubeam)',
     +        taubeam)
      end if

      if (abs(echpwr).gt.1.0D-8) then
         call ovarre(nout,'Electron cyclotron power (W)','(echpwr)',
     +        echpwr)
      end if

C+**PJK 06/03/96
      if (abs(pofcd).gt.1.0D-8) then
         call ovarre(nout,'OFCD efficiency (A/W)','(effofss)',effofss)
         call ovarre(nout,'OFCD gamma (A/W-m2)','(gamof)',gamof)
         call ovarre(nout,'OFCD power (W)','(pofcd)',pofcd)
      end if

 1000 continue

      return
      end

c______________________________________________________________________
      subroutine iternb(
     +     abeam,alphan,alphat,aspect,dene,deni,dlamie,enbeam,eps,
     +     feffcd,frbeam,ftr,ralpne,rmajor,rncne,rnfene,rnone,te,ten,
     +     zeff,zeffai,
     +     effnbss,fpion,fshine,taubeam)

C  Routine to calculate ITER Neutral Beam current drive parameters.
C
C  abeam   : (INPUT)  beam ion mass (amu)
C  alphan  : (INPUT)  density profile factor
C  alphat  : (INPUT)  temperature profile factor
C  aspect  : (INPUT)  aspect ratio
C  dene    : (INPUT)  volume averaged electron density (m**-3)
C  deni    : (INPUT)  fuel ion density (m**-3)
C  dlamie  : (INPUT)  ion-electron coulomb logarithm
C  enbeam  : (INPUT)  neutral beam energy (keV)
C  eps     : (INPUT)  inverse aspect ratio
C  feffcd  : (INPUT)  current drive efficiency fudge factor
C  frbeam  : (INPUT)  R_tangent / R_major for neutral beam injection
C  ftr     : (INPUT)  tritium fraction of D-T ions in beam
C  ralpne  : (INPUT)  thermal alpha density / electron density
C  rmajor  : (INPUT)  plasma major radius (m)
C  rncne   : (INPUT)  beam carbon density / electron density
C  rnfene  : (INPUT)  beam iron density / electron density
C  rnone   : (INPUT)  beam oxygen density / electron density
C  te      : (INPUT)  volume averaged electron temperature (keV)
C  ten     : (INPUT)  density weighted average electron temp. (keV)
C  zeff    : (INPUT)  plasma effective charge
C  zeffai  : (INPUT)  density weighted plasma effective charge
C  effnbss : (OUTPUT) neutral beam current drive efficiency (A/W)
C  fpion   : (OUTPUT) fraction of NB power given to ions
C  fshine  : (OUTPUT) shine-through fraction of beam
C  taubeam : (OUTPUT) no of NB expon. decay lengths to plasma centre
C
C+**PJK 15/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     abeam,alphan,alphat,aspect,dene,deni,dlamie,enbeam,eps,
     +     feffcd,frbeam,ftr,ralpne,rmajor,rncne,rnfene,rnone,te,ten,
     +     zeff,zeffai,
     +     effnbss,fpion,fshine,taubeam,
     +     dend,dent,dpath,d1,d2,sigstop

      DOUBLE PRECISION ETANB,SIGBEAM
      EXTERNAL         ETANB,SIGBEAM
      
C  Calculate beam path length to centre

      d1 = rmajor * sqrt( (1.0D0 + eps)**2-frbeam**2)
      if (frbeam.lt.1.0D0) then
         d2 =  rmajor*sqrt(1.0D0-frbeam**2)
         dpath = d1 - d2
      else
         dpath = d1
      end if

C  Calculate beam stopping cross-section

      sigstop = sigbeam(enbeam/abeam,te,dene,ralpne,
     +     rncne,rnone,rnfene)

C  Calculate number of decay lengths to centre

      taubeam = dpath * dene * sigstop

C  Shine-through fraction of beam

      fshine = exp (-2.0D0 * d1*dene*sigstop)

C  Deuterium and tritium beam densities

      dend = deni * (1.0D0-ftr)
      dent = deni * ftr

C  Power split to ions / electrons

C+**PJK 12/06/92 Removed first argument (ZFAST) of CFNBI.

      call cfnbi(abeam,enbeam,ten,dene,dend,dent,zeffai,dlamie,fpion)

C+**PJK 12/06/92 Removed DLAMIE from argument list of ETANB.
C+**PJK 12/06/92 Removed ZEFFAI from argument list of ETANB.

C  Current drive efficiency

      effnbss = etanb(abeam,alphan,alphat,aspect,dene,
     +     enbeam,rmajor,ten,zeff) * frbeam * feffcd

      return
      end

c______________________________________________________________________
      double precision function etanb(
     +     abeam,alphan,alphat,aspect,dene,ebeam,rmajor,ten,zeff)

C  Routine to find beam current drive efficiency
C  using the ITER formulation.
C  Revised 3/10/89 for ITER rules, and again 6/13/89

C+**PJK 12/06/92 Removed DLAMIE and ZEFFAI from argument list.

C+**PJK 11/06/92 Replaced IMPLICIT DOUBLE PRECISION with IMPLICIT NONE.

      IMPLICIT NONE

C+**PJK 11/06/92 Explicitly declared all variables.

      DOUBLE PRECISION
     +     abeam,alphan,alphat,aspect,dene,ebeam,rmajor,ten,zeff,
     +     abd,bbd,dene20,dum,epseff,ffac,gfac,rjfunc,xj,xjs,yj,zbeam

C+**PJK 11/06/92 Replaced data statement with explicit initialisation

      zbeam = 1.0D0
      bbd = 1.0D0

      dene20 = dene/1.0D20

C  Ratio of E_beam/E_crit

      xjs = ebeam/(bbd*10.0D0*abeam*ten)
      xj = sqrt(xjs)

      yj = 0.8D0 * zeff/abeam

      rjfunc = xjs/(4.0D0 + 3.0D0*yj + xjs *(xj + 1.39D0 +
     +     0.61D0 * yj**0.7D0))

      epseff = 0.5D0 /aspect
      gfac = (1.55D0 + 0.85D0/zeff)*sqrt(epseff) -
     +     (0.2D0+1.55D0/zeff)*epseff
      ffac = 1.0D0/zbeam - (1.0D0 - gfac)/zeff

      abd = 0.107D0 * (1.0D0 - 0.35D0*alphan +0.14D0*alphan**2) *
     +     (1.0D0 - 0.21D0*alphat) * (1.0D0 - 0.2D-3*ebeam  +
     +     0.09D-6 * ebeam**2)
      dum = abd *(5.0D0/rmajor) * (0.1D0*ten/dene20) *
     +     rjfunc/0.2D0 * ffac
      etanb = dum

      return
      end

c______________________________________________________________________
      subroutine cfnbi(
     +     afast,efast,te,ne,nd,nt,zeffai,xlmbda,fpion)

C  Subroutine to calculate the fraction of the fast particle energy
C  coupled to the ions.
C
C  afast    - mass of fast particle (units of proton mass)
C  efast    - energy of fast particle (keV)
C  fpion   : (OUTPUT) fraction of fast particle energy coupled to ions

C+**PJK 12/06/92 Removed ZFAST from argument list

C+**PJK 11/06/92 Replaced IMPLICIT DOUBLE PRECISION with IMPLICIT NONE.

      IMPLICIT NONE

C+**PJK 11/06/92 Explicitly declared all variables.

      DOUBLE PRECISION
     +     afast,efast,te,ne,nd,nt,zeffai,xlmbda,fpion,
     +     ans,atmd,atmdt,atmt,c,echarge,ecritfi,ecritfix,me,mproton,
     +     pi,sum,sumln,thx,t1,t2,ve,x,xlbd,xlbt,xlmbdai,xlnrat,zd,zt

C+**PJK 11/06/92 Declared XLMBDABI as EXTERNAL

      DOUBLE PRECISION xlmbdabi
      EXTERNAL         xlmbdabi

C+**PJK 11/06/92 Replaced data statement with explicit initialisation

      zd = 1.0D0
      zt = 1.0D0
      atmd = 2.0D0
      atmt = 3.0D0
      atmdt = 2.5D0
      pi = 3.1415926D0
      mproton = 1.67D-27
      me = 9.1D-31
      c = 3.0D8
      echarge = 1.6022D-19

C+**PJK 12/06/92 Removed ZFAST from argument list of XLMBDABI.
C+**PJK 12/06/92 Removed ZD from argument list of XLMBDABI.

      xlbd = xlmbdabi(afast,atmd,efast,te,ne)

C+**PJK 12/06/92 Removed ZFAST from argument list of XLMBDABI.
C+**PJK 12/06/92 Removed ZT from argument list of XLMBDABI.

      xlbt = xlmbdabi(afast,atmt,efast,te,ne)

      sum = nd *zd*zd *xlbd/atmd + nt *zt*zt *xlbt/atmt
      ecritfix = 16.0D0 *te *afast *(sum/(ne *xlmbda))**(2.0D0/3.0D0)

C+**PJK 12/06/92 Removed ZFAST from argument list of XLMBDABI.
C+**PJK 12/06/92 Removed ZD from argument list of XLMBDABI.

      xlmbdai = xlmbdabi(afast,atmdt,efast,te,ne)
      sumln = zeffai * xlmbdai/xlmbda
      xlnrat = (3.0D0*sqrt(pi)/4.0D0 * me/mproton *
     +     sumln)**(2.0D0/3.0D0)
      ve = c *sqrt(2.0D0*te/511.0D0)

      ecritfi = afast * mproton * ve*ve * xlnrat/(2.0D0 *
     +     echarge *1000.0D0)

      x = sqrt(efast/ecritfi)
      t1 = log((x*x - x + 1.0D0)/((x +1.0D0)**2))
      thx = (2.0D0*x - 1.0D0)/sqrt(3.0D0)
      t2 = 2.0D0*sqrt(3.0D0) *(atan(thx) + pi/6.0D0)

      ans = (t1 + t2)/(3.0D0 * x*x)
      fpion = ans

      return
      end

c______________________________________________________________________
      double precision function xlmbdabi(
     +     mb,mth,eb,t,nelec)

C  Function to calculate the Coulomb logarithm for ion-ion
C  collisions where the relative velocity may be large compared
C  with the background ('mt') thermal velocity.
C
C  From: Mikkelson and Singer, Nuc Tech/Fus, 4, 237 (1983)

C+**PJK 12/06/92 Removed ZB and ZTH from argument list.

C+**PJK 11/06/92 Replaced IMPLICIT DOUBLE PRECISION with IMPLICIT NONE.

      IMPLICIT NONE

C+**PJK 11/06/92 Explicitly declared all variables.

      DOUBLE PRECISION
     +     mb,mth,eb,t,nelec,
     +     ans,x1,x2

      x1 = (t/10.0D0) * (eb/1000.0D0) * mb/(nelec/1.0D20)
      x2 = mth/(mth + mb)

      ans = 23.7D0 + log(x2 * sqrt(x1))
      xlmbdabi = ans

      return
      end

c______________________________________________________________________
      double precision function sigbeam(
     +     eb,te,ne,rnhe,rnc,rno,rnfe)

C  Function to calculate the stopping cross-section for a hydrogen
C  beam in a fusion plasma.
C
C  From Janev, Boley, Post - Nucl. Fus. 29, 1989, p. 2138.
C
C  INPUT :
C  eb  = beam energy (kev/amu)
C  te = electron temperature (keV)
C  ne = electron density (10^20m-3)
C  rnhe = alpha density / ne
C  rnc = carbon density /ne
C  rno = oxygen density /ne
C  rnfe = iron density /ne
C
C  OUTPUT :
C  beam stopping cross-section (m^2)

C+**PJK 12/06/92 Replaced IMPLICIT DOUBLE PRECISION with IMPLICIT NONE.

      IMPLICIT NONE

C+**PJK 12/06/92 Explicitly declared all variables.

      DOUBLE PRECISION
     +     eb,te,ne,rnhe,rnc,rno,rnfe,
     +     a(2,3,2),ans,b(3,2,2,4),nen,nn(4),sz,s1,z(4)

      INTEGER i,is,j,k

      data a/
     +      4.40D0 , 2.30D-1, 7.46D-2,-2.55D-3, 3.16D-3, 1.32D-3,
     +     -2.49D-2,-1.15D-2, 2.27D-3,-6.20D-4,-2.78D-5, 3.38D-5/
      data b/
     +     -2.36D0 , 8.49D-1,-5.88D-2,-2.50D-1, 6.77D-2,-4.48D-3,
     +      1.85D-1,-4.78D-2, 4.34D-3,-3.81D-2, 1.05D-2,-6.76D-4,
     +     -1.49D0 , 5.18D-1,-3.36D-2,-1.19D-1, 2.92D-2,-1.79D-3,
     +     -1.54D-2, 7.18D-3, 3.41D-4,-1.50D-2, 3.66D-3,-2.04D-4,
     +     -1.41D0 , 4.77D-1,-3.05D-2,-1.08D-1, 2.59D-2,-1.57D-3,
     +     -4.08D-4, 1.57D-3, 7.35D-4,-1.38D-2, 3.33D-3,-1.86D-4,
     +     -1.03D0 , 3.22D-1,-1.87D-2,-5.58D-2, 1.24D-2,-7.43D-4,
     +      1.06D-1,-3.75D-2, 3.53D-3,-3.72D-3, 8.61D-4,-5.12D-5/

C+**PJK 12/06/92 Replaced data statement with explicit initialisation.

      z(1) = 2.0D0
      z(2) = 6.0D0
      z(3) = 8.0D0
      z(4) = 26.0D0

      nn(1) = rnhe
      nn(2) = rnc
      nn(3) = rno
      nn(4) = rnfe
      nen = ne/1.0D19

      s1 = 0.0D0
      do 30 i = 1,2
         do 20 j = 1,3
            do 10 k = 1,2
               s1 = s1 + a(i,j,k) * (log(eb))**(i-1) *
     +              (log(nen))**(j-1) * (log(te))**(k-1)
 10         continue
 20      continue
 30   continue

C  Impurity term

      sz = 0.0D0
      do 70 is = 1,4
         do 60 i = 1,3
            do 50 j = 1,2
               do 40 k = 1,2
                  sz = sz + b(i,j,k,is)* (log(eb))**(i-1) *
     +                 (log(nen))**(j-1) * (log(te))**(k-1) * 
     +                 nn(is) * z(is) * (z(is) - 1.0D0)
 40            continue
 50         continue
 60      continue
 70   continue

      ans =  1.0D-20 * ( exp(s1)/eb * (1.0D0 + sz) )

C+**PJK 24/05/06 Prevented sigbeam from being negative

      sigbeam = MAX(ans,1.0D-23)
                        
      return
      end

c______________________________________________________________________
      subroutine cullhy(
     +     alphan,alphat,bt,dene,feffcd,rmajor,rminor,te,zeff,
     +     effrfss)

C  New Lower Hybrid efficiency model.
C
C  The algorithm is described in Culham Report AEA FUS 172.
C
C  alphan  : (INPUT)  density profile factor
C  alphat  : (INPUT)  temperature profile factor
C  bt      : (INPUT)  toroidal field on axis (T)
C  dene    : (INPUT)  volume averaged electron density (m**-3)
C  feffcd  : (INPUT)  current drive efficiency fudge factor
C  rmajor  : (INPUT)  plasma major radius (m)
C  rminor  : (INPUT)  plasma minor radius (m)
C  te      : (INPUT)  volume averaged electron temperature (m**-3)
C  zeff    : (INPUT)  plasma effective charge
C  effrfss : (OUTPUT) current drive efficiency (A/W)
C
C+**PJK 15/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     alphan,alphat,bt,dene,feffcd,rmajor,rminor,te,zeff,
     +     effrfss,
     +     blocal,dene19,dlocal,epslh,frac,gamlh,nplacc,rpenet,
     +     rratio,term01,term02,term03,term04,tlocal,x

C  Volume averaged electron density in units of 10**19 m**-3
      dene19 = dene / 1.0D19

C  Calculate the penetration radius of the LH waves

      call lhrad(alphan,alphat,bt,dene19,rmajor,rminor,te,rratio)
      rpenet = rratio*rminor

C  Local plasma parameters at this minor radius:

C  Density
      dlocal = dene19*(1.0D0+alphan)*(1.0D0-rratio**2)**alphan

C  Temperature
      tlocal = te*(1.0D0+alphat)*(1.0D0-rratio**2)**alphat

C  Toroidal field (evaluated at the inboard region of the flux surface)
      blocal = bt*rmajor/(rmajor-rpenet)

C  Parallel refractive index needed for plasma access

      frac = sqrt(dlocal)/blocal
      nplacc = frac + sqrt(1.0D0+frac*frac)

C  Local inverse aspect ratio
      epslh = rpenet/rmajor

C  LH normalised efficiency (A/W m**-2)

      x = 24.0D0/(nplacc*sqrt(tlocal))

      term01 = 6.1D0/(nplacc*nplacc*(zeff+5.0D0))
      term02 = 1.0D0+(tlocal/25.0D0)**1.16D0
      term03 = epslh**0.77D0 * sqrt(12.25D0+x*x)
      term04 = 3.5D0*epslh**0.77D0 + x

      if (term03.gt.term04) then
         write(*,*) 'Error in CULLHY :'
         write(*,*) 'Normalised LH efficiency < 0'
         write(*,*) 'Use a different value of IEFRF.'
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

      gamlh = term01*term02*(1.0D0-term03/term04)

C  Current drive efficiency (A/W)

      effrfss = gamlh/((0.1D0*dlocal)*rmajor) * feffcd

      return
      end

c______________________________________________________________________
      subroutine lhrad(
     +     alphan,alphat,bt,dene19,rmajor,rminor,te,
     +     rratio)

C  Routine to determine numerically the minor radius at which the
C  damping of Lower Hybrid waves occurs.
C
C  Part of the new LH current drive formula (iefrf=6).
C
C  alphan : (INPUT)  density profile factor
C  alphat : (INPUT)  temperature profile factor
C  bt     : (INPUT)  toroidal field on axis (T)
C  dene19 : (INPUT)  volume averaged electron density (10**19 m**-3)
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  te     : (INPUT)  volume averaged electron temperature (keV)
C  rratio : (OUTPUT) minor radius of penetration / plasma minor radius
C
C+**PJK 15/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     alphan,alphat,bt,dene19,rmajor,rminor,te,
     +     rratio,
     +     den0,dgdr,drfind,g0,g1,g2,rat0,rat1,r1,r2,t0

      INTEGER lapno,maxlap

C  Central density and temperature

      den0 = dene19*(1.0D0+alphan)
      t0 = te*(1.0D0+alphat)

C  Correction to refractive index (kept within valid bounds)

      drfind = min(0.7D0,max(0.1D0,12.5D0/t0))

C  Use Newton-Raphson method to establish the correct minor radius
C  ratio. g is calculated as a function of r / r_minor, where g is
C  the difference between the results of the two formulae for the
C  energy E given in AEA FUS 172, p.58. The required minor radius
C  ratio has been found when g is sufficiently close to zero.

C  Initial guess for the minor radius ratio
      rat0 = 0.8D0

C  Maximum number of laps
      maxlap = 100
      lapno = 0

 10   continue
      lapno = lapno+1

C  Minor radius ratios either side of the latest guess

      r1 = rat0 - 1.0D-3*rat0
      r2 = rat0 + 1.0D-3*rat0

C  Evaluate g at rat0

      call lheval(
     +     alphan,alphat,bt,den0,drfind,rmajor,rminor,rat0,t0,g0)

C  Evaluate g at r1

      call lheval(
     +     alphan,alphat,bt,den0,drfind,rmajor,rminor,r1,t0,g1)

C  Evaluate g at r2

      call lheval(
     +     alphan,alphat,bt,den0,drfind,rmajor,rminor,r2,t0,g2)

C  Calculate gradient of g with respect to minor radius ratio

      dgdr = (g2-g1)/(r2-r1)

C  New approximation

      rat1 = rat0-g0/dgdr

C  Force this approximation to lie within bounds

      rat1 = max(0.0001D0,rat1)
      rat1 = min(0.9999D0,rat1)

C  Check the number of laps for convergence
C+**PJK 29/06/92 Changed 'laps' to 'iterations'
C+**PJK 17/11/97 Fixed apostrophe errors
      if (lapno.ge.maxlap) then
         write(*,*) 'Problem in routine LHRAD:'
         write(*,*) 'LH penetration radius not found '
     +        ,'after ',lapno,' iterations.'
         write(*,*) 'A value of 0.8 * rminor will be used.'
         rat0 = 0.8D0
         goto 20
      end if

C  Is g sufficiently close to zero?

      if (ABS(g0).GT.0.01D0) then
C        No, so go around loop again
         rat0 = rat1
         goto 10
      end if

 20   continue

      rratio = rat0

      return
      end

c______________________________________________________________________
      subroutine lheval(
     +     alphan,alphat,bt,den0,drfind,rmajor,rminor,rratio,t0,
     +     result)

C  Routine to evaluate the difference between the values calculated
C  from the two equations for the energy E, given in AEA FUS 172, p.58.
C
C  The idea is to reduce this difference to zero using the
C  Newton-Raphson method to adjust the minor radius fraction rratio
C  at which the equations are evaluated. When this has been achieved
C  the location of the Lower Hybrid absorption has been found.
C
C  alphan : (INPUT)  density profile factor
C  alphat : (INPUT)  temperature profile factor
C  bt     : (INPUT)  toroidal field on axis (T)
C  den0   : (INPUT)  central electron density (10**19 m**-3)
C  drfind : (INPUT)  correction to parallel refractive index
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  rratio : (INPUT)  guess for penetration radius / rminor
C  t0     : (INPUT)  central electron temperature (keV)
C  result : (OUTPUT) difference between the E values (keV)
C
C+**PJK 15/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     alphan,alphat,bt,den0,drfind,rmajor,rminor,rratio,t0,
     +     result,
     +     blocal,dlocal,E1,E2,frac,nplacc,refind,tlocal

C  Local plasma parameters:

C  Density
      dlocal = den0*(1.0D0-rratio**2)**alphan

C  Temperature
      tlocal = t0*(1.0D0-rratio**2)**alphat

C  Toroidal field (evaluated at the inboard region of the flux surface)
      blocal = bt*rmajor/(rmajor-rratio*rminor)

C  Parallel refractive index needed for plasma access

      frac = sqrt(dlocal)/blocal
      nplacc = frac + sqrt(1.0D0+frac*frac)

C  Total Parallel refractive index

      refind = nplacc + drfind

C  First equation for E

      E1 = 511.0D0*(sqrt(1.0D0+1.0D0/(refind*refind))-1.0D0)

C  Second equation for E

      E2 = 7.0D0*tlocal

C  Difference

      result = E1-E2

      return
      end

c______________________________________________________________________
      subroutine culecd(
     +     alphan,alphat,dene,feffcd,rmajor,rminor,te,zeff,
     +     effrfss)

C  Routine to calculate the Electron Cyclotron current drive efficiency
C  using the method described in Culham Report AEA FUS 172.
C
C  alphan  : (INPUT)  density profile factor
C  alphat  : (INPUT)  temperature profile factor
C  dene    : (INPUT)  volume averaged electron density (m**-3)
C  feffcd  : (INPUT)  current drive efficiency fudge factor
C  rmajor  : (INPUT)  plasma major radius (m)
C  rminor  : (INPUT)  plasma minor radius (m)
C  te      : (INPUT)  volume averaged electron temperature (keV)
C  zeff    : (INPUT)  plasma effective charge
C  effrfss : (OUTPUT) current drive efficiency (A/W)
C
C+**PJK 16/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     alphan,alphat,dene,feffcd,rmajor,rminor,te,zeff,
     +     effrfss,
     +     cosang,coulog,dlocal,ecgam,ecgam1,ecgam2,ecgam3,ecgam4,
     +     epsloc,tlocal,zlocal

C  Local plasma parameters : take r = a/3

C  Temperature
      tlocal = te*(1.0D0+alphat)*(1.0D0-0.333D0**2)**alphat

C  Density (10**20 m**-3)
      dlocal = dene*1.0D-20*(1.0D0+alphan)*(1.0D0-0.333D0**2)**alphan

C  Inverse aspect ratio
      epsloc = 0.333D0 * rminor/rmajor

C  Effective charge (use average value)
      zlocal = zeff

C  Coulomb logarithm for ion-electron collisions
C  (From J. A. Wesson, 'Tokamaks', Clarendon Press, Oxford, p.293)
      coulog = 15.2D0 - 0.5D0*log(dlocal) + log(tlocal)

C  Calculate normalised current drive efficiency at four different
C  poloidal angles, and average.
C  cosang = cosine of the poloidal angle at which ECCD takes place
C         = +1 outside, -1 inside.

      cosang = 1.0D0
      call eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam1)

      cosang = 0.5D0
      call eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam2)

      cosang = -0.5D0
      call eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam3)

      cosang = -1.0D0
      call eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam4)

C  Normalised current drive efficiency (A/W m**-2)

      ecgam = (ecgam1+ecgam2+ecgam3+ecgam4)/4.0D0

C  Current drive efficiency (A/W)

      effrfss = ecgam/(dlocal*rmajor) * feffcd

      return
      end

C______________________________________________________________________
      subroutine eccdef(
     +     tlocal,epsloc,zlocal,cosang,coulog,
     +     ecgam)

C  Written by M R O'Brien, Tokamak Development Division, AEA Fusion,
C  16th August 1991.
C
C  This subroutine works out the ECCD efficiency using the formula
C  due to Cohen quoted in the ITER Physics Design Guidelines : 1989
C  (but including division by the Coulomb Logarithm omitted from
C  IPDG). We have assumed gamma**2-1 << 1, where gamma is the
C  relativistic factor. The notation follows that in IPDG.
C
C  The answer ECGAM is the normalised efficiency nIR/P with n the
C  local density in 10**20 /m**3, I the driven current in MAmps,
C  R the major radius in metres, and P the absorbed power in MWatts.
C
C  tlocal : (INPUT)  local electron temperature (keV)
C  epsloc : (INPUT)  local inverse aspect ratio
C  zlocal : (INPUT)  local plasma effective charge
C  cosang : (INPUT)  cosine of the poloidal angle at which ECCD takes
C                    place (+1 outside, -1 inside)
C  coulog : (INPUT)  local coulomb logarithm for ion-electron collisions
C  ecgam  : (OUTPUT) normalised current drive efficiency (A/W m**-2)
C
C+**PJK 16/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     tlocal,epsloc,zlocal,cosang,coulog,
     +     ecgam,
     +     f,facm,fp,h,hp,lam,lams,mcsq,palpha,palphap,palphaps,
     +     palphas,y

      mcsq = 9.1095D-31 * 2.9979D8**2 /(1.0D3*1.6022D-19)
      f = 16.0D0 * (tlocal/mcsq)**2

C  fp is the derivative of f with respect to gamma, the relativistic
C  factor, taken equal to 1 + 2T/(m c**2).

      fp = 16.0D0 * tlocal/mcsq

C  lam is IPDG's lambda. LEGEND calculates the Legendre function of
C  order alpha and argument lam, palpha, and its derivative, palphap.
C  Here alpha satisfies alpha(alpha+1) = -8/(1+zlocal). alpha is of the
C  form  (-1/2 + ix), with x a real number and i = sqrt(-1).

      lam = 1.0D0
      call legend(zlocal,lam,palpha,palphap)
      lams = sqrt(2.0D0*epsloc/(1.0D0+epsloc))
      call legend(zlocal,lams,palphas,palphaps)

C  hp is the derivative of IPDG's h function with respect to lam.

      h = -4.0D0*lam/(zlocal+5.0D0)*(1.0D0-lams*palpha/(lam*palphas))
      hp = -4.0D0/(zlocal+5.0D0)*(1.0D0-lams*palphap/palphas)

C  facm is IPDG's momentum conserving factor.

      facm = 1.5D0
      y = mcsq/(2.0D0*tlocal)*(1.0D0+epsloc*cosang)

C  We take the negative of the IPDG expression to get a positive
C  number.

      ecgam = - 7.8D0*facm*sqrt((1.0D0+epsloc)/(1.0D0-epsloc)) / coulog
     +     * (h*fp-0.5D0*y*f*hp)

      if (ecgam.lt.0.0D0) then
         write(*,*) 'Error in routine ECCDEF:'
         write(*,*) 'Negative normalised current drive efficiency.'
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

      return
      end

C______________________________________________________________________
      subroutine legend(
     +     zlocal,arg,
     +     palpha,palphap)

C  Written by M R O'Brien, Tokamak Development Division, AEA Fusion,
C  16th August 1991.
C
C  LEGEND calculates the Legendre function of argument ARG and order
C  ALPHA = - 0.5 + i sqrt(xisq), PALPHA, and its derivative PALPHAP.
C  This Legendre function is a conical function and we use the series
C  in xisq given in Abramowitz and Stegun, equation 8.12.1. The
C  derivative is calculated from the derivative of this series.
C
C  The derivatives were checked by calculating PALPHA for
C  neighbouring arguments. The calculation of PALPHA for zero
C  argument was checked by comparison with the expression
C  PALPHA(0) = 1/sqrt(pi) * cos(pi*alpha/2) * gam1 / gam2 (Abramowitz
C  and Stegun, eqn 8.6.1). Here gam1 and gam2 are the Gamma functions
C  of arguments 0.5*(1+alpha) and 0.5*(2+alpha) respectively.
C
C  zlocal  : (INPUT)  local plasma effective charge
C  arg     : (INPUT)  argument of Legendre function
C  palpha  : (OUTPUT) value of Legendre function
C  palphap : (OUTPUT) derivative of Lengendre function
C
C+**PJK 16/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     zlocal,arg,
     +     palpha,palphap,
     +     arg2,pold,poldp,pterm,sinsq,term1,term2,xisq

      INTEGER n

C  Check for invalid argument

      if (abs(arg).gt.(1.0D0+1.0D-10)) then
         write(*,*) 'Error in routine LEGEND:'
         write(*,*) 'Invalid argument ARG, = ',arg
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

      arg2 = min(arg,(1.0D0-1.0D-10))
      sinsq = 0.5D0*(1.0D0-arg2)
      xisq = 0.25D0*(32.0D0*zlocal/(zlocal+1.0D0)-1.0D0)
      palpha = 1.0D0
      pold = 1.0D0
      pterm = 1.0D0
      palphap = 0.0D0
      poldp = 0.0D0

      do 10 n = 1,10001

         if ((n.gt.1).and.(mod(n,20).eq.1)) then
            term1 = 1.0D-10 * max(abs(pold),abs(palpha))
            term2 = 1.0D-10 * max(abs(poldp),abs(palphap))
            if ( (abs(pold-palpha).lt.term1).and.
     +           (abs(poldp-palphap).lt.term2)) goto 20
            pold = palpha
            poldp = palphap
         end if

         pterm = pterm*(4.0D0*xisq+(2.0D0*DBLE(n)-1.0D0)**2)/
     +        (2.0D0*DBLE(n))**2 * sinsq
         palpha = palpha+pterm
         palphap = palphap - DBLE(n) * pterm/(1.0D0-arg2)

 10   continue

C  Check convergence of solution

      if (abs(arg).gt.(1.0D0+1.0D-10)) then
         write(*,*) 'Error in routine LEGEND:'
         write(*,*) 'Solution has not converged.'
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

 20   continue

      return
      end

c______________________________________________________________________
      subroutine culnbi(
     +     abeam,alphan,alphat,aspect,dene,deni,dlamie,dnla,enbeam,eps,
     +     feffcd,frbeam,ftr,ralpne,rmajor,rminor,rncne,rnfene,rnone,
     +     te,ten,zeff,zeffai,
     +     effnbss,fpion,fshine,taubeam)

C  Routine to calculate Neutral Beam current drive parameters
C  using the corrections outlined in AEA FUS 172 to the ITER method.
C
C  The result cannot be guaranteed for devices with aspect ratios far
C  from that of ITER (approx. 2.8).
C
C  abeam   : (INPUT)  beam ion mass (amu)
C  alphan  : (INPUT)  density profile factor
C  alphat  : (INPUT)  temperature profile factor
C  aspect  : (INPUT)  aspect ratio
C  dene    : (INPUT)  volume averaged electron density (m**-3)
C  deni    : (INPUT)  fuel ion density (m**-3)
C  dlamie  : (INPUT)  ion-electron coulomb logarithm
C  dnla    : (INPUT)  line averaged electron density (m**-3)
C  enbeam  : (INPUT)  neutral beam energy (keV)
C  eps     : (INPUT)  inverse aspect ratio
C  feffcd  : (INPUT)  current drive efficiency fudge factor
C  frbeam  : (INPUT)  R_tangent / R_major for neutral beam injection
C  ftr     : (INPUT)  tritium fraction of D-T ions
C  ralpne  : (INPUT)  thermal alpha density / electron density
C  rmajor  : (INPUT)  plasma major radius (m)
C  rminor  : (INPUT)  plasma minor radius (m)
C  rncne   : (INPUT)  beam carbon density / electron density
C  rnfene  : (INPUT)  beam iron density / electron density
C  rnone   : (INPUT)  beam oxygen density / electron density
C  te      : (INPUT)  volume averaged electron temperature (keV)
C  ten     : (INPUT)  density weighted average electron temp. (keV)
C  zeff    : (INPUT)  plasma effective charge
C  zeffai  : (INPUT)  density weighted plasma effective charge
C  effnbss : (OUTPUT) neutral beam current drive efficiency (A/W)
C  fpion   : (OUTPUT) fraction of NB power given to ions
C  fshine  : (OUTPUT) shine-through fraction of beam
C  taubeam : (OUTPUT) no of NB expon. decay lengths to plasma centre
C
C+**PJK 17/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     abeam,alphan,alphat,aspect,dene,deni,dlamie,dnla,enbeam,eps,
     +     feffcd,frbeam,ftr,ralpne,rmajor,rminor,rncne,rnfene,rnone,
     +     te,ten,zeff,zeffai,
     +     effnbss,fpion,fshine,taubeam,
     +     dend,dent,dpath,d1,d2,sigstop

      DOUBLE PRECISION ETANB2,SIGBEAM
      EXTERNAL         ETANB2,SIGBEAM
      
C  Calculate beam path length to centre

      d1 = rmajor * sqrt( (1.0D0 + eps)**2-frbeam**2)
      if (frbeam.lt.1.0D0) then
         d2 =  rmajor*sqrt(1.0D0-frbeam**2)
         dpath = d1 - d2
      else
         dpath = d1
      end if

C  Calculate beam stopping cross-section

      sigstop = sigbeam(enbeam/abeam,te,dene,ralpne,
     +     rncne,rnone,rnfene)

C  Calculate number of decay lengths to centre

      taubeam = dpath * dnla * sigstop

C  Shine-through fraction of beam

      fshine = exp (-2.0D0 * d1*dnla*sigstop)

C  Deuterium and tritium beam densities

      dend = deni * (1.0D0-ftr)
      dent = deni * ftr

C  Power split to ions / electrons

      call cfnbi(abeam,enbeam,ten,dene,dend,dent,zeffai,dlamie,fpion)

C  Current drive efficiency - new routine

      effnbss = etanb2(
     +     abeam,alphan,alphat,aspect,dene,dnla,enbeam,feffcd,frbeam,
     +     fshine,rmajor,rminor,ten,zeff)

      return
      end

c______________________________________________________________________
      double precision function etanb2(
     +     abeam,alphan,alphat,aspect,dene,dnla,enbeam,feffcd,frbeam,
     +     fshine,rmajor,rminor,ten,zeff)

C  Routine to find beam current drive efficiency
C  using the ITER formulation, plus correction terms outlined in
C  Culham Report AEA FUS 172.
C
C  The formulae are from AEA FUS 172, unless denoted by IPDG
C  (ITER Physics Design Guidelines: 1989).
C
C  abeam  : (INPUT)  beam ion mass (amu)
C  alphan : (INPUT)  density profile factor
C  alphat : (INPUT)  temperature profile factor
C  aspect : (INPUT)  aspect ratio
C  dene   : (INPUT)  volume averaged electron density (m**-3)
C  dnla   : (INPUT)  line averaged electron density (m**-3)
C  enbeam : (INPUT)  neutral beam energy (keV)
C  feffcd : (INPUT)  current drive efficiency fudge factor
C  frbeam : (INPUT)  R_tangent / R_major for neutral beam injection
C  fshine : (INPUT)  shine-through fraction of beam
C  rmajor : (INPUT)  plasma major radius (m)
C  rminor : (INPUT)  plasma minor radius (m)
C  ten    : (INPUT)  density weighted average electron temperature (keV)
C  zeff   : (INPUT)  plasma effective charge
C  etanb2 : (OUTPUT) current drive efficiency (A/W)
C
C+**PJK 17/06/92 Peter Knight

      IMPLICIT NONE

      DOUBLE PRECISION
     +     abeam,alphan,alphat,aspect,dene,dnla,enbeam,feffcd,frbeam,
     +     fshine,rmajor,rminor,ten,zeff,
     +     abd,bbd,d,dene20,dnla20,dnorm,ebmev,ecrit,enorm,epseff,
     +     epsitr,eps1,ffac,gamnb,gfac,j0,nnorm,r,xj,xjs,yj,zbeam

C  Charge of beam ions

      zbeam = 1.0D0

C  Fitting factor (IPDG)

      bbd = 1.0D0

C  Volume averaged electron density (10**20 m**-3)

      dene20 = dene/1.0D20

C  Line averaged electron density (10**20 m**-3)

      dnla20 = dnla/1.0D20

C  Critical energy (MeV) (power to electrons = power to ions) (IPDG)
C  N.B. ten is in keV

      ecrit = 0.01D0 * abeam * ten

C  Beam energy in MeV

      ebmev = enbeam/1.0D3

C  x and y coefficients of function J0(x,y) (IPDG)

      xjs = ebmev/(bbd*ecrit)
      xj = sqrt(xjs)

      yj = 0.8D0 * zeff/abeam

C  Fitting function J0(x,y)

      j0 = xjs/(4.0D0 + 3.0D0*yj + xjs *(xj + 1.39D0 +
     +     0.61D0 * yj**0.7D0))

C  Effective inverse aspect ratio, with a limit on its maximum value

      epseff = min(0.2D0, (0.5D0/aspect))

C  Reduction in the reverse electron current
C  due to neoclassical effects

      gfac = (1.55D0 + 0.85D0/zeff)*sqrt(epseff) -
     +     (0.2D0+1.55D0/zeff)*epseff

C  Reduction in the net beam driven current
C  due to the reverse electron current

      ffac = 1.0D0 - (zbeam/zeff) * (1.0D0 - gfac)

C  Normalisation to allow results to be valid for
C  non-ITER plasma size and density:

C  Line averaged electron density (10**20 m**-3) normalised to ITER

      nnorm = 1.0D0

C  Distance along beam to plasma centre

      r = max(rmajor,rmajor*frbeam)
      eps1 = rminor/r
      d = rmajor * sqrt( (1.0D0+eps1)**2 - frbeam**2)

C  Distance along beam to plasma centre for ITER
C  assuming a tangency radius equal to the major radius

      epsitr = 2.15D0/6.0D0
      dnorm = 6.0D0 * sqrt(2.0D0*epsitr + epsitr**2)

C  Normalisation to beam energy (assumes a simplified formula for
C  the beam stopping cross-section)

      enorm = ebmev * ( (nnorm*dnorm)/(dnla20*d) )**(1.0D0/0.78D0)

C  A_bd fitting coefficient, after normalisation with enorm

      abd = 0.107D0 * (1.0D0 - 0.35D0*alphan + 0.14D0*alphan**2) *
     +     (1.0D0 - 0.21D0*alphat) * (1.0D0 - 0.2D0*enorm +
     +     0.09D0 * enorm**2)

C  Normalised current drive efficiency (A/W m**-2) (IPDG)

      gamnb = 5.0D0 * abd * 0.1D0*ten * (1.0D0-fshine) * frbeam *
     +     j0/0.2D0 * ffac

C  Current drive efficiency (A/W)

      etanb2 = gamnb / (dene20*rmajor) * feffcd

      return
      end

