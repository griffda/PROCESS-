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
C  Module         : $Id: divtmod.f,v 3.5 2002/04/25 14:46:09 peter Exp $
C
C  Module name    : $RCSfile: divtmod.f,v $
C  Version no.    : $Revision: 3.5 $
C
C  Creation date  : $Date: 2002/04/25 14:46:09 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE DIVCALL(iprint,nout)

c  Caller for divertor routine. This routine scales dimensions,
c  powers and field levels which are used as input to the Harrison
c  divertor model.
c
c  History :
c  put in ITER divertor model (6/27/89)
c  major modification : 1/90 - 2/90
c  modified 11/27/90 to have a ST expanded divertor option
c  modified 3/13/91 - put in updated ITER model
c  modified 3/18/91 to use new scalings for connection lengths
c
c  PJK 29/01/96 Added TART gaseous divertor model
c  PJK 14/05/96 Improved calculation of TART divertor area
c  PJK 25/04/02 Added ZEFFDIV; Changed DIVDUM to integer
c
c  Divertor common block description:
c
c *aionso - aionso mass (use fuel only)
c *anginc - angle of incidence of field line on plate (rad)
c  coefl  - little 'l' in Harrison model
c *delne  - scrapeoff density by main plasma
c  delw   - energy flow thickness in scrapeoff
c  delta  - iteration relative error
c  eier   - ionization + radiation energy / recycle event
c *frgd   - separatrix area to divertor / total separatrix area
c  gamdiv - sheath coefficient
c  gamdt  - plasma flow to plate (10**20/s)
c *pdiv   - power flow to divertor (MW)
c  ppdiv  - 
c *qdiv   - heat flux across separatrix to divertor (pdiv / area)
c *rbpbt  - B_p / B_t at the strike point
c *zeffso - Zeff in the scrapeoff
c *xpara  - parallel diffusivity in the plasma scrapeoff (m2/s)
c *xpert  - perpendicular diffusivity in the plasma scrapeoff (m2/s)
c
c * = input to subroutine divertor

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'divrt.h'
      INCLUDE 'osections.h'

      DOUBLE PRECISION aionso,bpav,bpstk,btstk,dconl,delne,
     +     delta,delw,diva,dtheta,frgd,gamdt,pdiv,plrefo,plsep,
     +     ppdiv,pwr,qdiv,rbpbt,rnull,xpara,xperp,zeffso

      DOUBLE PRECISION r1,r2,a1,a2,a3,theta,areadv

      INTEGER iprint,nout

C+**PJK 29/01/96 TART divertor model
      if (itart.eq.1) goto 600

c  Scale geometric quantities.
c  WARNING: many of these are scaled from the Fall ITER point
c  (R=6.00, Bt = 4.85 T, Bp = 1.07 T, l_null-strike = 1.50 m).
c  Variation far from these parameters is uncertain !!!

c  Conventional tokamak divertor model :
c  Assume ions transport 33% of electron power
      xperp = xpertin * 1.33D0

c  Reference null to strike distances

      plsep = min(plsepo,pi*rminor)

c  Only set up for outer divertor for double-null

c  Scale 'plasma' quantities

      delne = prn1 * dene * 1.0D-20
      pwr = pdivt
      aionso = afuel
C *** PJK 25/04/02 Changed divdum to integer
      if (divdum.eq.0) then
         zeffso = 1.0D0 + 0.8D0*(zeff-1.0D0)
      else
C *** PJK 25/04/02 Changed RHS from 1.0 to zeffdiv (user-specifiable)
         zeffso = zeffdiv
      end if

      bpstk = bp * 0.45D0
      btstk = bt * rmajor/rstrko
      rbpbt = bpstk / btstk

      xpara = xparain/zeffso

c  Null radius
      rnull = rmajor - rminor*triang

c  Divertor area and radius ratio

      rsrd = (rnull + rmajor + rminor) / (rnull + rstrko)
      diva = pi* (rnull + rstrko) * plsep
      adas = diva/sarea

c  Main plasma separatrix area to divertor (and power fraction)

      frgd = (sareao)/(2.0D0*sarea)

c  Power flow to divertor

      pdiv = pwr * ksic/2.0D0
      qdiv = pdiv/(sarea*frgd)

c  Connection length scalings
c  (2.5 factor comes from normalization to ITER)

      tconl = 2.5D0 * rmajor * q * sqrt(1.0D0 + 1.0D0/(q*aspect)**2)
      dtheta = plsep/rminor
      dconl = 2.5D0 * rstrko * q * dtheta *
     +     sqrt(1.0D0 + 1.0D0/(q*aspect)**2)
      rconl = dconl / tconl

c  Minimum strike angle
      minstang = 0.5D0

c  Call divertor routine

      call divert(
     i     adas,aionso,anginc,delne,c1div,c2div,c3div,c4div,c5div,
     i     delld,fdfs,fififi,frgd,frrp,minstang,omegan,qdiv,
     i     pdiv,rbpbt,rconl,rmajor,rsrd,tconl,xpara,xperp,
     o     delta,delw,dendiv,densin,gamdt,lamp,omlarg,ppdiv,ppdivr,
     o     ptpdiv,tdiv,tsep)

      hldiv = ppdivr

c  Ratio of collision length to connection length

      rlclolcn = 1.44D-3 * tsep**2 / (delne*15.0D0*tconl)

      if ((iprint.eq.0).or.(sect05.eq.0)) goto 1000

      call oheadr(nout,'Divertor')
      call ocmmnt(nout,'Harrison (ITER) Model')
      call oblnkl(nout)

c  Fixed quantities to divertor model

      call ovarre(nout,'Ion mass (amu)','(aionso)',aionso)
      call ovarre(nout,'Fitting coefficient','(c1div)',c1div)
      call ovarre(nout,'Fitting coefficient','(c2div)',c2div)
      call ovarre(nout,'Fitting coefficient','(c3div)',c3div)
      call ovarre(nout,'Fitting coefficient','(c4div)',c4div)
      call ovarre(nout,'Fitting coefficient','(c5div)',c5div)
      call ovarre(nout,'Fitting coefficient','(c6div)',c6div)
      call ovarin(nout,'Dummy variable','(divdum)',divdum)
      call ovarre(nout,'Zeff in scrape-off region','(zeffso)',zeffso)
      call ovarre(nout,'Coeff of energy distrib. along conn length',
     +     '(delld)',delld)
      call ovarre(nout,'Separatrix plasma density (10**20 m-3)',
     +     '(delne)',delne)
      call ovarre(nout,'Radial gradient ratio','(fdfs)',fdfs)
      call ovarre(nout,'Sheath potential factor','(fgamp)',fgamp)
      call ovarre(nout,'Parameter for sheath coefficient','(fififi)',
     +     fififi)
      call ovarre(nout,'Fraction of radiated power to plate','(frrp)',
     +     frrp)
      call ovarre(nout,'Pressure ratio - (nT)_p/(nT)_s','(omegan)',
     +     omegan)
      call ovarre(nout,'ne-edge / ne-average','(prn1)',prn1)
      call ovarre(nout,'Parallel heat transport coefficient','(xpara)',
     +     xpara)
      call ovarre(nout,'Radial transport coefficient','(xperp)',xperp)

c  Input quantities scaled in divertor caller (dependent on geometry,
c  plasma parameters) - can be different for inner and outer plates.

      call osubhd(nout,'Scaled Input Quantities :')

      call ovarre(nout,'Fraction of areas','(adas)',adas)
      call ovarre(nout,'Angle of incidence (rad)','(anginc)',anginc)
      call ovarre(nout,'Area of divertor / area of separatrix','(frgd)'
     +     ,frgd)
      call ovarre(nout,'Power fraction to outer divertor','(ksic)',ksic)
      call ovarre(nout,'Power to divertor (MW)','(pdiv)',pdiv)
      call ovarre(nout,'Null to strike length (m)','(plsep)',plsep)
      call ovarre(nout,'B_p / B_t strike point','(rbpbtc)',rbpbt)
      call ovarre(nout,'Connection length ratio','(rconl)',rconl)
      call ovarre(nout,'Radius ratio R_s/R_d','(rsrd)',rsrd)
      call ovarre(nout,'Strike radius (m)','(rstrko)',rstrko)
      call ovarre(nout,'Connection length (m)','(tconl)',tconl)

c  Quantities calculated by the Harrison model

      call osubhd(nout,'Divertor Model Output :')
      call ovarre(nout,'Iteration relative error','(delta)',delta)
      call ovarre(nout,'Private flux power factor','(omlarg)',omlarg)
      call ovarre(nout,'Separatrix temperature (eV)','(tsep)',tsep)
      call ovarre(nout,'Divertor temperature (eV)','(tdiv)',tdiv)
      call ovarre(nout,'Divertor plasma density (10**20 m-3)',
     +     '(dendiv)',dendiv)
      call ovarre(nout,'Peak heat load (MW/m2)','(hldiv)',hldiv)
      call ovarre(nout,'Divertor peak temperature (eV)','(ptpdiv)',
     +     ptpdiv)
      call ovarre(nout,'D/T plate flux (10**20 m-3)','(gamdt)',gamdt)
      call ovarre(nout,'Scrape-off thickness (m)','(delw)',delw)
      call ovarre(nout,'Collision length / connection length',
     +     '(rlclolcn)',rlclolcn)

      goto 1000

 600  continue

C *** Tight aspect ratio tokamak divertor model
C *** =========================================

C *** Assume the power is evenly spread around the
C *** divertor chamber by the action of a gaseous target

C+**PJK 14/05/96 Improved calculation of divertor area
C+**PJK 14/05/96 Each divertor is approximately triangular in R,Z plane
C+**PJK 14/05/96 Estimated from AEA FUS 64, Figure 2
C+**PJK 14/05/96 Old formula : areadv = 2.0D0 * fwarea

C *** Thickness of centrepost+first wall at divertor height

      r1 = rmajor - rminor*triang - 3.0D0*scrapli

C *** Outer radius of divertor region

      r2 = rmajor + rminor

C *** Angle of diagonal divertor plate from horizontal

      if ((vgap.le.0.0D0).or.((r2-r1).le.0.0D0)) then
         write(nout,*) 'Error in routine DIVCALL:'
         write(nout,*) 'vgap = ',vgap
         write(nout,*) 'r1 = ',r1
         write(nout,*) 'r2 = ',r2
         write(nout,*) 'PROCESS stopping.'
         STOP
      end if

      theta = atan(vgap/(r2-r1))

C *** Vertical plate area

      a1 = 2.0D0 * pi * r1 * vgap

C *** Horizontal plate area

      a2 = pi * (r2*r2 - r1*r1)

C *** Diagonal plate area

      a3 = a2 / cos(theta)

C *** Total divertor area (N.B. there are two of them)

      areadv = 2.0D0 * (a1+a2+a3)

      hldiv = pdivt/areadv

      if ((iprint.eq.0).or.(sect05.eq.0)) goto 1000

      call osubhd(nout,'Divertor Heat Load')
      call ocmmnt(nout,
     +     'Assume an expanded divertor with a gaseous target')
      call oblnkl(nout)
      call ovarre(nout,'Power to the divertor (MW)','(pdivt)',pdivt)
      call ovarre(nout,'Divertor surface area (m2)','(areadv)',areadv)
      call ovarre(nout,'Divertor heat load (MW/m2)','(hldiv)',hldiv)

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE DIVERT(
     i     adas,aion,anginc,delne,c1div,c2div,c3div,c4div,c5div,
     i     delld,fdfs,fififi,frgd,frrp,minstang,omegan,qdiv,pdiv,
     i     rbpbt,rconl,rmaj,rsrd,tconl,xpara,xperp,
     o     delta,delw,dendiv,densin,gamdt,lamp,omlarg,ppdiv,ppdivr,
     o     ptpdiv,tdiv,tsep)

c  This subroutine performs the iteration described in M. Harrison's
c  (and Kukushkin's) analytic ITER divertor model.
c  See report ITER-IL-PH-13-9-e12
c  Coded by J. Galambos, FEDC/ORNL.
c  revised 2/17/91 to use SUPERCODE format.
c
c  INPUT :
c  adas     - divertor flux area / main plasma area (long separatrix)
c  aion     - ion mass (use fuel only) (AMU)
c  anginc   - poloidal angle of incidence of field line on plate (rad)
c  c1div    - fitting coefficient for plate temperature
c  c2div    - fitting coefficient for plate temperature
c  c3div    - fitting coefficient for heat load
c  c4div    - fitting coefficient for heat load
c  c5div    - fitting coefficient for 'omlarg'
c  delld    - coefficient for power distribution flow into scrapeoff
c  delne    - scrapeoff density by main plasma (10**20 m-3)
c  fdfs     - gradient ratio (private flux side/other side) in 'omlarg'
c  fififi   - coefficient used in sheath energy transfer factor calc.
c  frgd     - separatrix area to divertor / total separatrix area
c  frrp     - fraction of radiated power to plate
c  minstang - minimum strike angle (total) to be used in the heat flux
c  omegan   - pressure ratio of (plate / main plasma)
c  qdiv     - heat flux across separatrix to divertor, pdiv/area (MW/m2)
c  pdiv     - power flow to plate (MW)
c  rbpbt    - ratio of toroidal / poloidal field at strike point
c  rconl    - connection length ratio (dvrtr region/main plasma region)
c  rmaj     - major radius (m)
c  rsrd     - ratio of separatrix radius / divertor radius
c  tconl    - connection length along field line by main plasma (m)
c  xpara    - parallel diffusivity in the plasma scrapeoff (m2/s)
c  xperp    - perpendicular diffusivity in the plasma scrapeoff (m2/s)
c
c  OUTPUT :
c  delta  - iteration relative error
c  delw   - energy flow thickness in scrape-off (m)
c  dendiv - plasma density at divertor (10**20 m-3)
c  densin - peak plasma density at divertor (on separatrix) (10**20 m-3)
c  gamdt  - plasma flow to plate (10**20/s)
c  lamp   - power flow width (m)
c  omlarg - factor accounting for power flow to private flux region
c  ppdiv  - divertor heat load without radiation (MW/m2)
c  ppdivr - divertor heat load with radiation (MW/m2)
c  ptpdiv - peak plasma temperature at the divertor plate (eV)
c  tdiv   - temperature at the plate (eV)
c  tsep   - temperature at the separatrix (eV)
c
c  Local quantities (some) :
c  coefl  - little 'l' in Harrison model
c  eier   - ionization + radiation energy / recycle event
c  gamdiv - sheath coefficient

      IMPLICIT NONE

      DOUBLE PRECISION adas,aion,anginc,delne,c1div,c2div,c3div,
     +     c4div,c5div,delld,fdfs,fififi,frgd,frrp,minstang,omegan,
     +     qdiv,pdiv,rbpbt,rconl,rmaj,rsrd,tconl,xpara,xperp

      DOUBLE PRECISION delw,dendiv,densin,gamdt,lamp,ppdiv,ppdivr,
     +     ptpdiv,tdiv,tsep

C  Local variables
      DOUBLE PRECISION angle,coefl,cp,ct,c27,delta,deltx,delty,
     +     deltdiv,deltpts,denom,ei,eier,epsilon,facdenom,fprime,
     +     f1,f1dx,f1dy,f2,f2dx,f2dy,gamdiv,omlarg,pi,relerr,
     +     tdivges,tdivp,tpts,tptsges,tptsp

      DOUBLE PRECISION erprcy, ftdiv, ftpts, gammash
      EXTERNAL         erprcy, ftdiv, ftpts, gammash

      INTEGER i

      ei = 13.6D0
      c27 = 0.2857143D0
      relerr = 1.0d-9
      epsilon = 0.001D0
      pi = 3.141592653589793D0

      fprime = c5div * fdfs
      facdenom = fprime * rsrd*(adas/frgd)**2 /rconl
      facdenom = max(facdenom, 0.04D0)
      omlarg = 1.0D0/ ( rsrd * exp(-facdenom) )
      omlarg = min(omlarg, 2.0D0)
      coefl = 1.0D0/delld + rconl/omlarg

c  Start iteration on 2 simultaneous equations (Newton's method)

      tdivges = 150.0D0
      tptsges = 0.9D0
      tdiv = tdivges
      tpts = tptsges

      do 2 i = 1,15

c  Find derivatives for Newton's method

         tptsp = tpts * (1.0D0 + epsilon)
         deltx = tpts * epsilon
         tdivp = tdiv * (1.0D0 + epsilon)
         delty = tdiv * epsilon

         f1 = ftpts(aion,coefl,delne,fififi,omegan,omlarg,qdiv,
     +        tconl,xpara,xperp,tpts,tdiv)
         f2 = ftdiv(aion,coefl,delne,fififi,omegan,omlarg,qdiv,
     +        tconl,xpara,xperp,tpts,tdiv)
         
         f1dx = ( ftpts(aion,coefl,delne,fififi,omegan,omlarg,qdiv,
     +        tconl,xpara,xperp,tptsp,tdiv) - f1 ) / deltx
         f1dy = ( ftpts(aion,coefl,delne,fififi,omegan,omlarg,qdiv,
     +        tconl,xpara,xperp,tpts,tdivp) - f1 ) / delty
         f2dx = ( ftdiv(aion,coefl,delne,fififi,omegan,omlarg,qdiv,
     +        tconl,xpara,xperp,tptsp,tdiv) - f2 ) / deltx
         f2dy = ( ftdiv(aion,coefl,delne,fififi,omegan,omlarg,qdiv,
     +        tconl,xpara,xperp,tpts,tdivp) - f2 ) / delty

         denom = f1dx*f2dy - f1dy*f2dx
         if (denom.eq.0.0D0) denom = 1.0D-10
         deltpts = (-f2dy*f1 + f1dy*f2) / denom
         deltdiv = ( f2dx*f1 - f1dx*f2) / denom

c  New guess

         tdiv = tdiv + deltdiv
         tpts = tpts + deltpts
         delta = abs(deltdiv/tdiv + deltpts/tpts)

c  Satisfied yet ?

         if ( delta .lt. relerr) go to 3

 2    continue

 3    continue
      tdiv = max(tdiv, 0.1000D0)
      tpts = max(tpts, 0.0010D0)
      tpts = min(tpts, 0.9999D0)

c  Some other quantities

      ct = max( 0.1D0, (c1div + c2div/(tdiv)) )
      ptpdiv = tdiv * ct
      gamdiv = gammash(fififi,tdiv)
      dendiv = delne / (omegan*tpts)
      eier = erprcy(tdiv,dendiv)
      tsep = 251.0D0 * ( (qdiv*tconl)**2 /(c27*xpara*
     +     (1.0D0 - tpts**3.5D0) ) * coefl/(xperp*delne))**0.2222222D0

      cp = max(0.1D0, (c3div + c4div/(tdiv)) )
      angle = sin(anginc) * rbpbt
      if (minstang.ne.0.0D0) angle = max(angle, (minstang/57.3D0))
      ppdiv = 2.48D2 * (qdiv)**1.55556D0 / (xperp*delne)**0.777778D0
     +     *(c27*xpara)**0.2222222D0 * tconl**0.555556D0 *
     +     ( (1.0D0-tpts**3.5D0)/coefl)**0.222222D0 / omlarg *
     +     (1.0D0 + ei/(gamdiv*tdiv))/(1.0D0+eier/(gamdiv*tdiv)) *
     +     angle * cp
      ppdivr = ppdiv * (1.0D0 + frrp * (eier-ei) / (gamdiv*tdiv) )
      gamdt = 6.25D4 * ppdiv / (gamdiv*ptpdiv)
      densin = omegan * tsep * delne/ptpdiv
      delw = 4.01D-3 * (delne*xperp)**0.7777778D0 * tconl**0.4444444D0
     +     * coefl**0.2222222D0 / ( (qdiv)**0.55555556D0 *
     +     (c27 * xpara * (1.0D0 - tpts**3.5D0) )**0.22222D0 )
      lamp = pdiv*rsrd / (2.0D0 * pi * rmaj * ppdiv)

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION FTPTS(
     +     aion,coefl,delne,fififi,omegan,omlarg,qdiv,tconl,xpara,
     +     xperp,xx,yy)

c  Function for divertor model temperature ratio solution
c
c  Input: (see routine divert description)
c  xx = T_plate / T_separatrix guess
c  yy = T_plate guess (eV)
c
c  Output:
c  ftpts = T_plate / T_separatrix calculated - guess

      IMPLICIT NONE

      DOUBLE PRECISION aion,coefl,delne,fififi,omegan,omlarg,
     +     qdiv,tconl,xpara,xperp,xx,yy

c  Local variables
      DOUBLE PRECISION dendiv,eier,ff,gamdiv,xxs,yys

      DOUBLE PRECISION erprcy,gammash
      EXTERNAL         erprcy,gammash

      xxs = max(xx,0.001D0)
      xxs = min(xxs, 0.99999D0)
      yys = max(yy, 0.1D0)

      dendiv = delne * omegan/xxs
      gamdiv = gammash(fififi,yys)
      eier = erprcy(yys,dendiv)
      ff = xxs**3.5D0 + 9.66D0 * (xxs/aion)**0.9D0 *
     +     (xperp/(qdiv)**2)**0.8D0 * coefl/(2.0D0 * xpara/7.0D0)*
     +     tconl**0.2D0 * ( omlarg*omegan*gamdiv*
     +     (1.0D0+eier/(gamdiv*yys)))**1.8D0 * delne**2.6D0

      ftpts = 1.0D0 - ff

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION FTDIV(
     +     aion,coefl,delne,fififi,omegan,omlarg,qdiv,tconl,xpara,
     +     xperp,xx,yy)

c  Function for divertor temperature solution
c
c  Input: (see routine divert description)
c  xx = T_plate / T_separatrix guess
c  yy = T_plate guess (eV)
c
c  Output:
c  ftdiv = Divertor temperature calculated - guess

      IMPLICIT NONE

      DOUBLE PRECISION aion,coefl,delne,fififi,omegan,omlarg,qdiv,
     +     tconl,xpara,xperp,xx,yy

c  Local variables

      DOUBLE PRECISION c27,dendiv,eier,ff,gamdiv,xxs,yys

      DOUBLE PRECISION erprcy,gammash
      EXTERNAL         erprcy,gammash

      c27 = 0.28571428D0

      xxs = max(xx, 0.001D0)
      xxs = min(xxs, 0.99999D0)
      yys = max(yy, 0.1D0)

      dendiv = delne * omegan/xxs
      gamdiv = gammash(fififi,yys)
      eier = erprcy(yys,dendiv)
      ff = 20.16D0 * aion * ((qdiv)**10 * (c27*xpara)**4 /
     +     (xperp**5 * delne**14)*tconl*(1.0D0 - xxs**3.5D0)**4
     +     /coefl**4)**0.22222D0/(omegan*gamdiv*omlarg*
     +     (1.0D0+eier/(gamdiv*yys)))**2

      ftdiv = yys - ff

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION GAMMASH(gcoef,tdiv)

c  Function to provide the plasma sheath energy transfer coefficient
c  from the Harrison / Kukushkin ITER model.
c  Programmed by J. Galambos.
c
c  INPUT :
c  tdiv = electron temperature at the plate (eV)
c  gcoef = coefficient
c
c  Output :
c  gammash = energy transfer coefficient across the plate sheath

      IMPLICIT NONE

      DOUBLE PRECISION gcoef,tdiv

      gammash = ( 8.3D0 - 6.0D0*(0.07D0 - 0.18D0 * 
     +     log10(3.0D0*tdiv*gcoef) ) )

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION ERPRCY(tdiv,ndiv)

c  Function to provide the (energy radiated + ionized) / neutral
c  recycle event from the Harrison / Kukushkin ITER model.
c  Programmed by J. Galambos.
c
c  INPUT :
c  tdiv = electron temperature at the plate (eV)
c  ndiv = electron density at the plate (10**20 m-3)
c
c  OUTPUT :
c  erprcy = (energy radiated + ionized) / neutral recycle event (eV)

      IMPLICIT NONE

      DOUBLE PRECISION ans,ndiv,tdiv

      ans = 17.5D0 + (5.0D0 + 37.5D0/tdiv) * log10(10.0D0/ndiv)
      erprcy = max(ans, 0.001D0)

      return
      end
