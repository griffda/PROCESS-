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
C  Module name    : $RCSfile: blanket.f,v $
C  Version no.    : $Revision: 3.7 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS


      subroutine blanket(icalc,nout,iprint)

      include 'param.h'
      include 'phydat.h'
      include 'fwblsh.h'
      include 'htpwr.h'
      include 'build.h'
C+**PJK 22/01/97 heattr.h subsumed into htpwr.h
      include 'blanket.h'

      double precision Rm,ap,qfus(3),yc,xlr,xlp,xlf,volbl,wnet1
      double precision perim
      integer iprint,nout,icalc

      external perim

      xlr = (blnkith+blnkoth)/2.0d0
      xlf = (fwith+fwoth)/2.0d0
C+**PJK 24/04/98 Added use of fkblkt
      xlp = perim(rminor+xlf+xlr,fkblkt*kappa,triang)
      xlr = xlr*100.0d0
      xlp = xlp/4.0d0*100.0d0
      Rm = rmajor
      ap = rminor
      yc = vfblkt
      qfus(1) = pnucblkt
      qfus(2) = pnucshld
      qfus(3) = pfwdiv

c      if (iprint.eq.1) then
c         write(nout,*) Rm,ap
c         write(nout,*) xlr,xlp
c         write(nout,*) qfus(1),qfus(2),qfus(3)
c         write(nout,*) yc
c         write(nout,*) xtfi,xtfo
c      end if   

c *** Assume that the cooling channels in both the first wall/divertor
c *** and shield can be changed in such a fashion so that the
c *** temperature difference between the coolant inlet and outlet is
c *** identical to the difference calculated for the blanket. If one
c *** accepts this assumption then the shield and first wall/divertor
c *** can easily be incorporated into the Rankine cycle. 

      call blnkt(Rm,ap,qfus,yc,xlr,xlp,volbl,wnet1,icalc,nout,iprint)

c *** Volume and mass of blanket.

      volblkt = volbl

      if (smstr.eq.1) then
         wtblli2o = volblkt*fblli2o*2010.0d0
         whtblbe = volblkt*fblbe*1900.0d0
         whtblkt = wtblli2o+whtblbe
      else if (smstr.eq.2) then
         wtbllipb = volblkt*fbllipb*9400.0d0
         whtblli = volblkt*fblli*534.0d0
         whtblkt = wtbllipb+whtblli
      end if   

      whtblkt = whtblkt+volblkt*(5870.0d0*fblvd+denstl*fblss)

      if (icalc.eq.2) pgrossmw = wnet1

      return
      end

c *********************************************************************

c *** Synopsis of model
c *** -----------------
c *** 
c *** (a) Blanket structure
c *** ---------------------
c ***
c *** The blanket is assumed to consist of NR*NP*NT blocks of
c *** dimension LRxLPxW, with the coolant travelling through
c *** channels of: hydraulic diameter DH1; cross-sectional
c *** area AC; perimeter S; mass flow rate MC; and specific
c *** heat capacity CPC. The channel orientation can be either
c *** poloidal or radial; also a circular or annular channel
c *** can be chosen.
c ***    Energy balance on a elemental volume dV=ACdX (dX
c *** is either the radial or poloidal distance) leads to
c *** an equation of the form shown below.
c ***
c ***                        
c ***           d TC                        2           
c ***   MC CPC  ----     =     QT(X,THETA) W  ( 1 - YC )
c ***           d X     
c ***
c *** QT(X,THETA) is the volumetric heat deposition as a function
c *** of both X and the toroidal variable THETA.
c ***
c ***    The equation above can be integrated over the range
c *** (0--->LR or LP), this results in an expression for the
c *** outlet coolant temperature in terms of the inlet coolant
c *** temperature.
c ***
c *** (b) Coolant pumping power
c *** -------------------------
c ***
c *** Only the losses incurred in the cooling channel are considered, 
c *** with the pressure drop along the length LK (K=P or R) of a
c *** single channel given by:
c ***
c ***                             2
c ***   dP = fric 2 LK/DH1 (MC/AC)  1/rho
c ***
c *** fric: friction factor (dependent on the flow Reynolds number)
c *** rho: coolant mass density (kg/m**3)
c *** 
c *** Therefore the total pumping power for the NR*NP*NT cooling 
c *** channels is:
c ***
c ***   P = NR*NP*NT*MC*dP/rho
c ***
c *** (c) Thermal energy conversion
c *** -----------------------------
c ***
c *** A Rankine cycle with reheat and regeneration is employed
c *** with water/steam as the working fluid. The cycle is 
c *** summarised below:
c ***
c *** (i)    Water (condensate) is passed through the steam 
c ***        generator (SG) (TB1,HB1,PH)---->(TB2=TH1,HB2=HH1,PH)
c *** (ii)   The steam then enters a high pressure (HP) turbine where
c ***        it is partially expanded ----->(TH2,HH2,PR=PI)
c *** (iii)  It is then reheated and enters a intermediate pressure (IP)
c ***        turbine (TI1,HI1,PI)---->(TI2,HI2=HL1,PL)
c *** (iv)   It then enters a low pressure turbine (LP) 
c ***        (TL1,HL1,PL)---->(TL2=TC,HL2=HC,PC) to the condenser.
c *** 
c *** Syntax: `1' - inlet; `2' - outlet; `H' - high; `L' - low;
c ***         `I' - intermediate; `R' - reheat; `B' - boiler;
c ***         and `C' - condenser.
c ***
c *** Regeneration is achieved by extracting a small fraction of the
c *** superheated steam from the IP and LP turbines directing it to
c *** the feed water heaters, where the excess energy is given off.
c *** The type of feed water heaters used in the analysis are
c *** termed `open' and the condensate is compressed to the
c *** pressure of the extracted steam.
c ***    The enthalpy rise due to the feed pump compression is:
c ***                        
c *** 
c ***                      P  - P 
c ***                       k    k+1
c ***      DH      =  v    ---------
c ***        k+1       k+1   eta                 
c ***                           k+1
c ***
c *** v   : the specific volume (m**3/kg)
c ***  k+1
c *** eta   : isentropic efficiency 
c ***    k+1
c ***
c *** (d) Cycle efficiency
c *** --------------------
c ***
c *** The cycle efficiency is the net power output over the heat input.
c *** To calculate this quantity the enthalpies and mass flow rates
c *** must be known at all points in the cycle. 
c ***                           
c *** A more detailed description of the physical models
c *** used in the analysis can be found in the following
c *** reference: `Blanket and Energy Conversion Model for
c *** Fusion Reactors', Dr. P.J. Karditsas, AEA Technology,
c *** Theoretical and Strategic Studies Dept., Culham
c *** Laboratory, Abingdon.

      subroutine blnkt(Rm,ap,xqfus,yc,xlr,xlp,vol,wnet1,
     +     icalc,nout,iprint)

c *** Argument definitions
c *** --------------------
c ***
c *** Rm (INPUT) (DP): plasma major radius (m)
c *** ap (INPUT) (DP): plasma minor radius (m)
c *** xqfus(1) (INPUT) (DP): nuclear heating in blanket (MW)
c *** xqfus(2) (INPUT) (DP): nuclear heating in shield (MW)
c *** xqfus(3) (INPUT) (DP): fusion heating in first wall/divertor (MW)
c *** yc (INPUT) (DP): coolant fraction 
c *** xlr (INPUT) (DP): blanket thickness (cm)
c *** xlp (INPUT) (DP): quarter poloidal extent (cm)
c *** nout (INPUT) (INT): output unit identifier
c *** icalc (INPUT) (INT): calculation flag: `1' - calculate volume;
c ***                      `2' - calculate electric power 
c *** iprint (INPUT) (INT): print flag
c *** vol (OUTPUT) (DP): total blanket volume (m**3)
c *** wnet1 (OUTPUT) (DP): net electric power (MW)

      include 'blanket.h'

      double precision g0,a,pf,reyd,tol,tc,tso,xtso
      double precision hri,hre,sri,sre,hli,hle,sli,sle
      double precision tre,pi,xqfus(3),qfusion,xlr,xlp
      double precision lr,lp,Rm,ap,yc,lc
      double precision D0,per,ac,asurf,w,xo
      double precision di,pero,peri,fo,fi
      double precision atotal,vol,asurface
      double precision tfo,tb,mtb,cf,rhof
      double precision viscf,viscfs,pran,prans
      double precision kf,kfs,ksolid,mf,c1,c2,htc,ff,dpf
      double precision qpump,f0,mf1,ff0,mf2,fb0,df0,xmf
      double precision xqav,qav,ts
      double precision thsat,hhsat,hgh,sgh,trsat,hrsat,vrsat
      double precision hgr,sgr,tsi,tinsat,hinsat,vinsat
      double precision hgin,sgin,hc,vc,hgc,sgc,hhi,shi,rhohi
      double precision vhi,hso,rhori,vri,dhip,dhlp
      double precision ha1,ha2,ha3,ha4,ha5,ha6,ha7,g,dg      
      double precision mp,hsi,hhes,thes,hhe,hres,tres
      double precision tles,hles,tle,px,ti,hi,si,ms
      double precision qsg,eta,wcp,whpt,sum1,sum2,sum3,sum4
      double precision winpt,wlpt,wst,winpfp,wlpfp,qc,wnet
      double precision wnet1,etacycle,etaplant,xexit
      double precision qr,qin,vhsat,tfi,dh1,p1,wp1
      double precision tsat1,vsat1,sg1,hg1,hsat1
      double precision tsat(0:10),vsat(0:10),hsat(0:10)
      double precision sg(0:10),hg(0:10),p(0:10),h(0:10)
      double precision s(0:10),t(0:10),dh(0:10),m(0:10)
      double precision mb(0:10),wt(0:10),wp(0:10)
      integer ncc,nto,nti,nt,nro,nri,nr,npo,npi,np
      integer i,icalc,nout,iprint

      common/com2/g0,a,pf,reyd,ncc
      common/com3/tol,tc,tso
      common/com4/hri,hre,sre,sri,hli,hle,sli,sle,tre

      tol = 0.0001d0
      pi = 4.0d0*atan(1.0d0)

c *** Common block Variables
c *** ----------------------
c ***
c *** xtfi    (DP): blanket coolant inlet temperature (C)
c *** xtfo    (DP): blanket coolant outlet temperature (C)
c *** xtb     (DP): maximum blanket temperature (C)
c *** xpf     (DP): blanket coolant inlet pressure (MPa)
c *** estr   (INT): orientation of cooling channels
c *** astr   (INT): shape of coolant channel
c *** xdo     (DP): outside diameter of coolant channel (cm)
c *** xdi     (DP): inside diameter of coolant channel (cm)
c *** bstr   (INT): option for fixing the coolant outlet temperature
c ***               or the maximum blanket temperature.
c *** costr  (INT): option for deciding the coolant type
c *** smstr  (INT): option for deciding the blanket material
c *** ph      (DP): high pressure (turbine/SG inlet) (MPa)
c *** pr      (DP): intermed. turbine inlet (HP outlet) pressure (MPa)
c *** pin     (DP): low pressure turbine inlet (IP outlet) (MPa)
c *** pc      (DP): condenser pressure (LP outlet) (MPa)
c *** etahp   (DP): HP turbine isentropic efficiency
c *** etahp   (DP): IP turbine isentropic efficiency
c *** etahp   (DP): LP turbine isentropic efficiency
c *** etafp   (DP): feed pump isentropic efficiency
c *** etacp   (DP): condensate pump isentropic efficiency
c *** nipfwh (INT): number of IP feed water heaters
c *** nlpfwh (INT): number of LP feed water heaters
c *** sgeff   (DP): steam generator (SG) effectiveness

c *** Important local variables
c *** -------------------------
c ***
c *** nt     (INT): number of cooling channels in toroidal direction
c *** per     (DP): perimeter of cooling channel (m)
c *** ac      (DP): cooling channel cross-sectional area (m**2)
c *** asurf   (DP): cooling channel surface area (m**2)
c *** w       (DP): cooling channel dimension (m)
c *** xo      (DP): thickness of 1-D slab (m)
c *** g0      (DP): geometrical factor
c *** nr     (INT): number of cooling channels in radial direction
c *** np     (INT): number of cooling channels in poloidal direction
c *** ncc    (INT): total  number of cooling channels 
c *** atotal  (DP): total blanket cross-sectional area (m**2)
c *** asurface (DP): total blanket surface area (m**2)
c *** vol     (DP): total blanket volume (m**3)
c *** qav     (DP): average volumetric heating (MW/m**3)
c *** mf      (DP): coolant mass flow rate (kg/s)
c *** ms      (DP): flow rate of steam (kg/s)
c *** m(i)    (DP): condensate flow rate into i'th FWH (kg/s)
c *** mb(i)   (DP): mass flow rate at i'th extraction point (kg/s)
c *** tso     (DP): SG output temperature (K)
c *** tsi     (DP): SG input temperature (K)
c *** hso     (DP): SG output enthalpy (kJ/kg)
c *** hsi     (DP): SG input enthalpy (kJ/kg)
c *** hhe     (DP): HP outlet enthalpy (kJ/kg)
c *** hhi     (DP): HP inlet enthalpy (kJ/kg)
c *** shi     (DP): HP inlet entropy (kJ/kg)
c *** hhes    (DP): HP outlet isentropic enthalpy (kJ/kg)
c *** hri     (DP): IP inlet enthalpy (kJ/kg)
c *** hre     (DP): IP outlet enthalpy (kJ/kg)
c *** sri     (DP): IP inlet entropy (kJ/kg)
c *** sre     (DP): IP outlet entropy (kJ/kg)
c *** hres    (DP): IP outlet isentropic enthalpy (kJ/kg)
c *** hli     (DP): LP inlet enthalpy (kJ/kg)
c *** hle     (DP): LP outlet enthalpy (kJ/kg)
c *** sli     (DP): LP inlet entropy (kJ/kg)
c *** sle     (DP): LP outlet entropy (kJ/kg)
c *** hles    (DP): LP outlet isentropic enthalpy (kJ/kg)
c *** hc      (DP): Condenser outlet enthalpy (kJ/kg)
c *** p(i)    (DP): saturated liquid pressure at FWHPi inlet (Pa)
c *** hsat(i) (DP): saturated liquid enthalpy at FWHPi inlet (kJ/kg)
c *** tsat(i) (DP): saturated liquid temperature at FWHPi inlet (K)
c *** vsat(i) (DP): saturated liquid spec. vol. at FWHPi inlet (m**3/kg)
c *** sg(i)   (DP): saturated vapour entropy at FWHPi inlet (kJ/kg)
c *** hg(i)   (DP): saturated vapour enthalpy at FWHPi inlet (kJ/kg)
c *** h(i)    (DP): enthalpy at i'th extraction point (kJ/kg)
c *** s(i)    (DP): entropy at i'th extraction point (kJ/kg)
c *** t(i)    (DP): temperature at i'th extraction point (K)
c *** dh(i)   (DP): enthalpy rise due to i'th FWHP (kJ/kg)
c *** wt(*)   (DP): power output from turbines (MW)
c *** wp(*)   (DP): power input to FWHPs (MW)

c *** Ensure that W >= 1.25 DO for both circular and annular
c *** geometry.

      if (yc.gt.pi/4.0d0/(1.25d0)**2) yc = 0.99d0*pi/4.0d0/(1.25d0)**2
      if (astr.eq.2) then
         if (xdi/xdo.gt.sqrt(1.0d0-4.0d0*(1.25d0)**2*yc/pi))
     +        xdo = 1.01d0*xdi/(sqrt(1.0d0-4.0d0*(1.25d0)**2*yc/pi))
      end if   

      qfusion = xqfus(1)*1.0d6
      tfi = xtfi+273.15d0
      pf = xpf*1.0d6

      lr = xlr/100.0d0
      lp = xlp/100.0d0

      if (estr.eq.1) lc = lr
      if (estr.eq.2) lc = lp

c *** Calculate the number of toroidal blocks together with
c *** various geometric features of the cooling channels.
c     
      if (astr.eq.1) then
c
c *** Circular cooling channels.

         D0 = xdo/100.0d0
         dh1 = D0
         per = pi*D0
         ac = pi*D0**2/4.0d0
         asurf = per*lc
         w = sqrt(ac/yc)
         nto = int(2.0d0*pi*(Rm+ap)/w)
         nti = int(2.0d0*pi*(Rm-ap)/w)
         nt = nto+nti
         g0 = (1.0d0/16.0d0)*pi*D0*(1.0d0-D0/w)**2/(1.0d0-yc)
         xo = 0.5d0*(w-D0)
c     
      end if
c     
      if (astr.eq.2) then

c *** Annular cooling channels.
c     
         D0 = xdo/100.0d0
         Di = xdi/100.0d0
         dh1 = D0-Di
         per = pi*(D0+Di)
         ac = pi*(D0**2-Di**2)/4.0d0
         asurf = per*lc
         pero = pi*D0
         peri = pi*Di
         w = sqrt(ac/yc)
         fo = 1.0d0-0.25d0*pi*(D0/w)**2
         fi = 0.25d0*pi*(Di/w)**2
         nto = int(2.0d0*pi*(Rm+ap)/w)
         nti = int(2.0d0*pi*(Rm-ap)/w)
         nt = nto+nti
         g0 = (1.0d0/16.0d0)*(fo*pi*D0*(1.0d0-D0/w)**2/(1.0d0-yc)
     +        +fi*pi*Di*(Di/w)**2/(1.0d0-yc))
         xo = sqrt(fo*0.25d0*(pero/per)*(w-D0)**2
     +        +0.25d0*fi*(peri/per)*Di**2)
c     
      end if

c *** Calculate the total number of cooling channels (this is
c *** equivalent to the total number of blocks). The reactor
c *** vessel is not completely surrounded by the blanket;
c *** there are gaps between different sections of the 
c *** blanket (80 % coverage is assumed).

C+**PJK 24/05/06 Ensured that np,nt are initialised, whatever the
C+**PJK 24/05/06 cooling channel orientation

      if (estr.eq.1) then 
c     
c *** Cooling channels are orientated in the radial direction.

         nr = int(lp/w)
         np = 0
         ncc = int(2.0d0*0.8d0*dble(nt*nr))
c     
      end if
c     
      if (estr.eq.2) then
c     
c *** Cooling channels are orientated in the poloidal direction.

         np = int(lr/w)
         nr = 0
         ncc = int(2.0d0*0.8d0*dble(nt*np))
c     
      end if

c *** Calculate the total blanket area, volume and surface area.

      atotal = dble(ncc)*w*w
      vol = atotal*lc
      asurface = 4.0d0*pi*Rm*(lp*2.0d0)

      if (icalc.eq.1) goto 2000

c *** Boundary condition.

      if (bstr.eq.1) then
c     
         tfo = xtfo+273.15d0

      end if
c     
      if (bstr.eq.2) then
c     
         tb = xtb+273.15d0
c     
      end if

      if (iprint.eq.1) then

         call oblnkl(nout)
         call ocentr(nout,'Blanket Input Data',72)
         call oblnkl(nout)

         call ovarrf(nout,'fusion power (MW)','(xqfusion)',xqfus(1))
         call ovarrf(nout,'blanket coolant inlet temp (C)','(xtfi)',
     +        xtfi)
         call ovarrf(nout,'blanket coolant inlet pressure (MPa)',
     +        '(xpf)',xpf)
         call ovarrf(nout,'Radial blanket length (cm)','(xlr)',xlr)
         call ovarrf(nout,'Poloidal (1/4) blanket length (cm)','(xlp)'
     +        ,xlp)
         call ovarrf(nout,'Reactor major radius (m)','(Rm)',Rm)
         call ovarrf(nout,'Reactor minor radius (m)','(ap)',ap)
         call ovarrf(nout,'Coolant fraction','(yc)',yc)

         if (astr.eq.1) then
            call ovarrf(nout,'outside diameter (cm)','(xdo)',xdo)
         else if (astr.eq.2) then   
            call ovarrf(nout,'outside diameter (cm)','(xdo)',xdo)
            call ovarrf(nout,'inside diameter (cm)','(xdi)',xdi)
         end if   

         if (bstr.eq.1) call osubhd(nout,
     +        'Output coolant temperature is fixed')
         if (bstr.eq.2) call osubhd(nout,
     +        'Maximum blanket temperature is fixed')

         if (bstr.eq.1) then
            call ovarrf(nout,'blanket coolant outlet temp (C)','(xtfo)'
     +           ,xtfo)
         else if (bstr.eq.2) then
            call ovarrf(nout,'blanket max allowable temp (C)','(xtb)',
     +           xtb)
         end if   
c     
         if (estr.eq.1) call osubhd(nout,
     +        'Cooling channels radially orientated')
         if (estr.eq.2) call osubhd(nout,
     +        'Cooling channels poloidally orientated')
         if (astr.eq.1) call osubhd(nout,
     +        'Circular cooling channels')
         if (astr.eq.2) call osubhd(nout,
     +        'Annular cooling channels')
         if (costr.eq.1) call ocmmnt(nout,
     +        'Gaseous helium chosen to be coolant')
         if (costr.eq.2) call ocmmnt(nout,
     +        'Pressurized water chosen to be coolant')
         if (smstr.eq.1) call ocmmnt(nout,
     +        'Solid blanket chosen (Li2O/Be)')
         if (smstr.eq.2) call ocmmnt(nout,
     +        'Liquid blanket chosen (LiPb/Li)')

      end if   

c *** Blanket parameter calculation 
c *** -----------------------------
c ***
c *** Assuming one dimensional heat conduction in a slab of thickness
c *** XO with volumetric heat source QFUSION, an approximate expression
c *** relating the maximum block temperature to the surface temperature
c *** can be derived.

      if (bstr.eq.1) then

c *** Fixed outlet coolant temperature.

         tb = tfo
         mtb = tfo

 8       continue

         call props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans,
     +        kf,kfs,ksolid)

c *** Calculate the bulk coolant mass flow rate. This is found
c *** simply from steady state (E = mcdT) considerations.

         mf = qfusion/(cf*(tfo-tfi))

         call flow(mf,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff,
     +        dpf,qpump)

c *** Maximum blanket temperature.

         tb = tfi+qfusion*(1.0d0+(1.0d0+c2*mf**a)/
     +        (c1*mf**(a-1.0d0)))/(mf*cf)

c *** The calculation must be repeated until a converged solution
c *** has been obtained because the expression for the maximum blanket
c *** temperature involves material properties which are themselves
c *** functions of the blanket temperature.

         if (ABS(tb-mtb).lt.tol) goto 9
         mtb = tb

         goto 8

 9       continue

      end if

      if (bstr.eq.2) then

c *** Calculate the material properties of the coolant
c *** at a fixed coolant outlet temperature of 850 K.

         tfo = 850.0d0
         call props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans,
     +        kf,kfs,ksolid)

c *** Coolant mass flow rate at TFO = 850 K

         mf = qfusion/(cf*(tfo-tfi))

         call flow(mf,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff,
     +        dpf,qpump)

 10      continue

c *** Mass Flow Rate difference at coolant mass flow rate MF

         f0 = mf/(1.0d0+(1.0d0+c2*mf**a)/(c1*mf**(a-1.0d0)))
     +        -qfusion/(cf*(tb-tfi))
         mf1 = mf+10.0d0
         call flow(mf1,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff,
     +        dpf,qpump)

c *** Mass Flow Rate difference at coolant mass flow rate MF1

         ff0 = mf1/(1.0d0+(1.0d0+c2*mf1**a)/(c1*mf1**(a-1.0d0)))
     +        -qfusion/(cf*(tb-tfi))
         mf2 = mf-10.0d0
         call flow(mf2,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff,
     +        dpf,qpump)

c *** Mass Flow Rate difference at coolant mass flow rate MF2

         fb0 = mf2/(1.0d0+(1.0d0+c2*mf2**a)/(c1*mf2**(a-1.0d0)))
     +        -qfusion/(cf*(tb-tfi))

c *** Employ Newton Raphson method to find the flow rate MF
c *** at which XMF-MF = 0.

         df0 = (ff0-fb0)/20.0d0
         xmf = mf-f0/df0

c *** The calculation must be repeated until a converged solution
c *** has been obtained because the expression for the coolant outlet
c *** temperature involves material properties which are themselves
c *** functions of the coolant temperature.

         if (ABS(xmf-mf).lt.tol) goto 15

         mf = xmf

         goto 10

 15      continue

         mf = xmf
c 
c *** Coolant outlet temperature at converged coolant mass flow rate.

C+**PJK 09/12/93 Mysterious factor 'f' deleted
C+**PJK 09/12/93 tfo = f*qfusion/(mf*cf)+tfi

         tfo = qfusion/(mf*cf)+tfi
         call props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans,
     +        kf,kfs,ksolid)
         call flow(mf,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff,
     +        dpf,qpump)

      end if

c *** Volumetric heating power.      

      xqav = xqfus(1)/vol
      qav = qfusion/vol
c 
c *** Surface temperature.

      ts = tb-0.25d0*qav*xo*xo/ksolid

c *** Output of blanket variables.

      if (iprint.eq.1) then

         call oblnkl(nout)
         call ocentr(nout,'Blanket Output Data',72)
         call oblnkl(nout)
         call ovarrf(nout,'Max Blanket temp (C)','(tb)',tb-273.15d0)
         call ovarrf(nout,'Coolant outlet temp (C)','(tfo)',
     +        tfo-273.15d0)
         call ovarrf(nout,'Surface temp (C)','(ts)',ts-273.15d0)
         call ovarrf(nout,'Pump power (MW)','(qpump)',qpump/1.0d6)
         call ovarrf(nout,'Qfusion  (MW)','(xqfusion)',xqfus(1))
         call ovarrf(nout,'Coolant fraction (%)','(yc)',yc*100.0d0)
         call ovarrf(nout,'Coolant mass flow rate (kg/s)','(mf)',mf)
         call ovarrf(nout,'Heat transfer coeff (W/m**2/K)','(htc)',htc)
         call ovarrf(nout,'Reynolds number','(reyd)',reyd)
         call ovarrf(nout,'Dh (cm)','(dh1)',dh1*100.0d0)
         call ovarrf(nout,'Inlet press (MPa)','(xpf)',xpf)
         call ovarrf(nout,'Pressure drop (kPa)','(dpf)',dpf/1.0d3)
         call ovarrf(nout,'Exit press (MPa)','(pf-dpf)',(pf-dpf)/1.0d6)
         call ovarin(nout,'Total no. of cooling channels','(ncc)',ncc)
         call ovarin(nout,'Radial cooling channels','(4*nr)',4*nr)
         call ovarin(nout,'Poloidal cooling channels','(np)',np)
         call ovarin(nout,'Toroidal cooling channels','(nt)',nt)
         call ovarrf(nout,'Block side length (cm)','(w)',w*100.0d0)
         call ovarrf(nout,'Total cross sect. area (m**2)','(atotal)'
     +        ,atotal)
         call ovarrf(nout,'Blanket volume (m**3)','(vol)',vol)
         call ovarrf(nout,'Total surface area (m**2)','(asurface)'
     +        ,asurface)
         call ovarrf(nout,'Average heat (MW/m**3)','(xqav)',xqav)

         call oblnkl(nout)
         call ocentr(nout,'Steam Input Data',72)
         call oblnkl(nout)

         call ovarrf(nout,'High inlet pressure (MPa)','(ph)',ph)
         call ovarrf(nout,'Reheat intermediate pressure (MPa)','(pr)',
     +        pr)
         call ovarrf(nout,'Low inlet pressure (MPa)','(pin)',pin)
         call ovarrf(nout,'Condenser pressure (MPa)','(pc)',pc)
         call ovarrf(nout,'HP turbine isentropic efficiency','(etahp)',
     +        etahp)
         call ovarrf(nout,'IP turbine isentropic efficiency','(etainp)',
     +        etainp)
         call ovarrf(nout,'LP turbine isentropic efficiency','(etalp)',
     +        etalp)
         call ovarrf(nout,'Feed pump isentropic efficiency','(etafp)',
     +        etafp)
         call ovarrf(nout,'Condensate pump isentropic efficiency',
     +        '(etacp)',etacp)
         call ovarin(nout,'Number of IP feedwater heaters','(nipfwh)',
     +        nipfwh)
         call ovarin(nout,'Number of LP feedwater heaters','(nlpfwh)',
     +        nlpfwh)
         call ovarrf(nout,'Steam generator effectiveness','(sgeff)',
     +        sgeff)
         call oblnkl(nout)

      end if   
c     
c *** General steam cycle parameter calculation:

c *** Given - isentropic efficiency (eta)
c ***       - inlet pressure and temperature (p1,t1)
c ***       - exit pressure (p2)
c *** Determine - inlet enthalpy h1 = f(p1,t1) and entropy s1 = f(p1,t1)
c ***             from subroutine SUPVAP
c ***           - isentropic exit enthalpy h2s=f1(p1,s1) from subroutine
c ***             SUPVAP1 or TWOPHASE depending on whether the exit
c ***             conditions are in the single or two-phase region.
c ***           - exit enthalpy h2 = h1-eta*(h1-h2s)
c ***           - exit entropy and temperature from SUPVAP2 or 
c ***             TWOPHASE.
c ***           - for both the intermediate and low pressure
c ***             turbines evaluate the extraction point enthalpies
c ***             to the feedwater heaters.     
c ***
c *** Steam generator/High pressure phase
c *** -----------------------------------
c ***
c *** Water passes through the steam generator at (tb1,hb1,ph)
c *** and exits at (tb2 = th1,hb2 = hh1,ph). The water/steam mixture
c *** then enters the HP turbine where is expands to (th2,hh2,pr).
c *** 
c *** Calculate saturated liquid/vapour properties at ph and pr.

      call satliq(ph,thsat,hhsat,vhsat,hgh,sgh)
      call satliq(pr,trsat,hrsat,vrsat,hgr,sgr)

c *** Saturation temperature at SG inlet (a guess).

      tsi = trsat

c *** If the steam generation is completely effective 
c *** the saturation temperature at the SG outlet will 
c *** equal the outlet coolant temperature.

      tso = sgeff*tfo+(1.0d0-sgeff)*tsi

 82   continue

      xtso = tso

c *** Intermediate/low pressure turbines phase
c *** ----------------------------------------
c ***
c *** The mixture is then reheated and enters an intermediate
c *** pressure turbine at (ti1,hi1,pi=pr) and exits at (ti2,hi2=hl2,pl)
c *** and then enters a low pressure turbine at (tl1,hl1,pl) and 
c *** exits at (tl2 = tc,hl2 = hc,pc).

c *** Calculate saturated liquid/vapour properties at pl = pin and pc.

      call satliq(pin,tinsat,hinsat,vinsat,hgin,sgin)
      call satliq(pc,tc,hc,vc,hgc,sgc)

c *** Evaluate inlet enthalpies/entropies
c *** -----------------------------------
c ***
c *** SUPVAP calculates the enthalpy and entropy
c ***
c *** At the HP inlet (ph,tso)

      call supvap(ph,tso,hhi,shi,rhohi,vhi)
      hso = hhi

c *** At the IP inlet (after reheat) (pr,tso).

      call supvap(pr,tso,hri,sri,rhori,vri)

c *** Calculation of enthalpies for IP/LP feed water pump heaters
c *** -----------------------------------------------------------
c ***
c *** Array assignments
c ***
c *** Zeroth element: intermediate pressure turbine inlet value (fixed)
c *** first--->nipfwh element: intermediate pressure feed water pump 
c ***                          value (determined)
c *** nipfwh+1 element: intermediate pressure turbine outlet value
c ***                          (fixed)
c *** nipfwh+2--->nipfwh+nlpfwh+1 element: low pressure feed water pump 
c ***                          value (determined)
c *** nipfwh+nlpfwh+2 element: low pressure inlet value (fixed)
c ***
c *** These assigments are carried out for the saturated liquid 
c *** temperature, specific mass, super heated vapour enthalpy, 
c *** saturated liquid enthalpy and super heated vapour entropy. 

c *** Saturated liquid variables.

      tsat(0) = trsat
      tsat(nipfwh+nlpfwh+2) = tc
      tsat(nipfwh+1) = tinsat

      vsat(0) = vrsat
      vsat(nipfwh+nlpfwh+2) = vc
      vsat(nipfwh+1) = vinsat

      hsat(0) = hrsat
      hsat(nipfwh+nlpfwh+2) = hc
      hsat(nipfwh+1) = hinsat

c *** Superheated vapour variables.

      sg(0) = sgr
      sg(nipfwh+nlpfwh+2) = sgc
      sg(nipfwh+1) = sgin

      hg(0) = hgr
      hg(nipfwh+nlpfwh+2) = hgc
      hg(nipfwh+1) = hgin
c     
c *** Average enthalpy change across the feed water pump heaters
c *** between the intermediate inlet and intermediate outlet.

      dhip = (hrsat-hinsat)/dble(nipfwh+1)

c *** Average enthalpy change across the feed water pump heaters
c *** between the low pressure inlet and the condenser inlet.

      dhlp = (hinsat-hc)/dble(nlpfwh+1)

c *** Intermediate pressure feed water pump heaters.
c     
      do 1000 i = 1,nipfwh

c *** hsat(0) = hrsat

         hsat(i) = hsat(0)-dble(i)*dhip

 1000 continue

c *** Low pressure feed water pump heaters.

      do 1001 i = nipfwh+2,nipfwh+nlpfwh+1
c     
c *** hsat(nipfwh+nlpfwh+2) = hc

         hsat(i) = hsat(nipfwh+nlpfwh+2)+
     +        dble(nipfwh+nlpfwh+2-i)*dhlp
c     
 1001 continue   

c *** Calculation of pressures for IP/LP feed water pump heaters
c *** ---------------------------------------------------------
c ***

      ha1 = 702.603552d0
      ha2 = 1.289508d0
      ha3 = -0.002973d0
      ha4 = 131.378047d0
      ha5 = 59.892944d0
      ha6 = -2.739215d0
      ha7 = 0.079064d0
c     
c *** Assignment of pressures at three stages of the Rankine cycle.

      p1 = pr
      p(0) = pr
      p(nipfwh+1) = pin
      p(nipfwh+nlpfwh+2) = pc
c     
c *** From the enthalpies at each stage in the chain of
c *** intermediate pressure feed water pump heaters calculate
c *** the pressures.

      do 1002 i = 1,nipfwh
c     
 30      continue
c     
c *** Difference between the saturated liquid enthalpy at feed point
c *** `i' and the saturated liquid enthalpy at p1.

         g = -hsat(i)+(ha1+ha2/p1+ha3/p1**2+ha4*log(p1)+ha5*p1+
     +        ha6*p1*p1+ha7*p1*p1*p1)

c *** Derivative of this difference with pressure.

         dg = -ha2/p1**2 - 2.0D0*ha3/p1**3 + ha4/p1 + ha5 + 
     +        2.0D0*ha6*p1 + 3.0D0*ha7*p1*p1

c *** Newton Raphson method employed to determine the pressure at
c *** the point where the difference in the enthalpies is within
c *** the tolerance.

         mp = p1-0.005d0*g/dg

         if (ABS(mp-p1).lt.tol) goto 25

         p1 = mp

         goto 30
c     
 25      continue
c     
         p(i) = mp
c     
c *** Evaluate various saturated liquid/vapour quantities at
c *** this pressure.

         call satliq(mp,tsat1,hsat1,vsat1,hg1,sg1)
         tsat(i) = tsat1
         vsat(i) = vsat1
         sg(i) = sg1
         hg(i) = hg1

 1002 continue    

      p1 = pin
c     
c *** From the enthalpies at each stage in the chain of
c *** low pressure feed water pump heaters calculate
c *** the pressures.

      do 1003 i = nipfwh+2,nipfwh+nlpfwh+1
c     
 31      continue
c     
c *** Difference between the saturated liquid enthalpy at feed point
c *** `i' and the saturated liquid enthalpy at p1.

         g = -hsat(i)+(ha1+ha2/p1+ha3/p1**2+ha4*log(p1)+
     +        ha5*p1+ha6*p1*p1+ha7*p1*p1*p1)

c *** Derivative of this difference with pressure.

         dg = -ha2/p1**2 - 2.0D0*ha3/p1**3 + ha4/p1 + ha5 +
     +        2.0D0*ha6*p1 + 3.0D0*ha7*p1*p1

c *** Newton Raphson method employed to determine the pressure at
c *** the point where the difference in the enthalpies is within
c *** the tolerance.

         mp = p1-0.005d0*g/dg
c     
         if (ABS(mp-p1).lt.tol) goto 26
c     
         p1 = mp

         goto 31
c     
 26      continue
c     
         p(i) = mp
c     
c *** Evaluate various saturated liquid/vapour quantities at
c *** this pressure.

         call satliq(mp,tsat1,hsat1,vsat1,hg1,sg1)
         tsat(i) = tsat1
         vsat(i) = vsat1
         sg(i) = sg1
         hg(i) = hg1
c     
 1003 continue   
c     
      if (sgeff.ne.1.0d0) then

         tsi = tsat(1)
         tso = sgeff*tfo+(1.0d0-sgeff)*tsi

c *** Repeat above calculations for the feedwater pump saturated
c *** liquid variables to obtain a revised estimate of the
c *** saturated liquid entry temperature to the steam generator.

         if (abs(xtso-tso).gt.0.1d0) goto 82

      end if   

c *** Evaluation of isentropic exit enthalpies
c *** ----------------------------------------
c ***
c *** We have to check to see if the exit conditions are within
c *** the superheated vapour region or the two-phase region.

c *** Each turbine effects the process (t1,s1,p1)---->(t2,s2,p2).
c *** This process is modelled in two parts: firstly an isentropic
c *** part (t1,s1,h1,p1)---->(t1s,s1,h1s,p1s); followed by a
c *** non-isentropic part (t1s,s1,h1s,p1s)---->(t2,s2,h2,p2).
c *** Subroutines SUPVAP1 or TWOPHASE simulates the first part
c *** while SUPVAP2 or TWOPHAS1 evaluate the entropy at the
c *** system point (p2,h2).

c *** High pressure turbine
c *** ---------------------
c ***
c *** The isentropic enthalpy at the high pressure turbine 
c *** outlet (pr,shi). 
c 
      if (shi.gt.sgr) then
c     
c *** Superheated steam.

         call supvap1(pr,shi,hhes,thes) 
c     
      else 

c *** Two-phase state.
c     
         call twophase(pr,thes,shi,hhes)
c     
      end if   
c     
c *** High pressure outlet enthalpy (etahp is the
c *** isentropic efficiency of the high pressure turbine).

      hhe = hhi-etahp*(hhi-hhes)
c     
c *** Intermediate pressure turbine (post reheat)
c *** -------------------------------------------
c ***
c *** Evaluate the isentropic enthalpy at the intermediate 
c *** pressure outlet (pin,sri).

      if (sri.gt.sgin) then 
c     
c *** Superheated steam.

         call supvap1(pin,sri,hres,tres)
c     
      else 

c *** Two-phase state.

         call twophase(pin,tres,sri,hres)
c     
      end if   
c     
c *** Intermediate pressure outlet enthalpy (etainp is the
c *** isentropic efficiency of the intermediate pressure turbine).

      hre = hri-etainp*(hri-hres)

c *** Low pressure inlet enthalpy equals the intermediate pressure
c *** outlet enthalpy.

      hli = hre
c     
c *** Evaluate the isentropic entropy at intermediate pressure 
c *** outlet (pin,hre).

      if (hre.gt.hgin) then 

c *** Superheated steam.

         call supvap2(pin,hre,sre,tre)

      else 

c *** Steam/liquid mixture.
c     
         call twophas1(pin,tre,hre,sre)
c     
      end if
c     
c *** Low pressure inlet entropy equals the intermediate outlet
c *** entropy.

      sli = sre

c *** Low pressure turbine
c *** --------------------
c ***
c *** Evaluate the isentropic enthalpy at the low pressure turbine
c *** outlet (pc,sli). 
c     
      if (sli.lt.sgc) then 
c     
c *** Superheated steam.

         call twophase(pc,tles,sli,hles) 
c     
      else 
c     
c *** Steam/liquid mixture.

         call supvap1(pc,sli,hles,tles)
c     
      end if

c *** Low pressure outlet enthalpy (etalp is the isentropic 
c *** pressure efficiency for the low pressure turbine).
c     
      hle = hli-etalp*(hli-hles)
c     
c *** Evaluate the entropy at the low pressure turbine 
c *** outlet (pc,hle). 

      if (hle.lt.hgc) then 
c     
c *** Vapour/liquid mixture.
c     
         call twophas1(pc,tle,hle,sle)
c     
      else

c *** Superheated steam.

         call supvap2(pc,hle,sle,tle)
c     
      end if
c     
c *** Calculate extraction enthalpies/entropies to the IP/LP FWHs
c *** -----------------------------------------------------------
c ***
c *** Given the enthalpies and entropies at the IP inlet;
c *** the IP outlet; and the LP outlet find the enthalpies
c *** and entropies at each extraction point to the
c *** feed water pump heaters.
c ***   Once again what region the conditions are in determines
c *** whether SUPVAP3 or TWOPHAS2 are called.

      h(0) = hri
      h(nipfwh+nlpfwh+2) = hle
      h(nipfwh+1) = hre

      s(0) = sri
      s(nipfwh+nlpfwh+2) = sle
      s(nipfwh+1) = sre

      t(0) = tso
      t(nipfwh+nlpfwh+2) = tle
      t(nipfwh+1) = tre

c *** IP feed water pump heaters.
c     
      do 1005 i = 1,nipfwh
c     
         px = p(i)
         ti = 0.5d0*(tso+tre)
c     
         if (etainp.eq.1) then 
c     
c *** If the pump is 100 % efficient then the enthalpy does
c *** not change (equivalent to vertical trajectory on s-h diagram).

            if (sri.gt.sg(i)) then 
c     
c *** Superheated vapour.

               call supvap1(px,sri,hi,ti)
c     
            else
c     
c *** Two-phase region.

               call twophase(px,ti,sri,hi)
c     
            end if   
c     
         end if
c     
         if (etainp.ne.1) then
c     
c *** Superheated vapour.

            call supvap3(px,ti,hi,si)
c     
            if (si.gt.sg(i)) then
c     
               h(i) = hi
               t(i) = ti
               s(i) = si
c     
            else
c     
c *** Two-phase region.

               call twophas2(px,ti,hi,si)

               h(i) = hi
               t(i) = ti
               s(i) = si
c     
            end if
c     
         end if
c     
 1005 continue
c     
c *** IP feed water pump heaters.

      do 1006 i = nipfwh+2,nipfwh+nlpfwh+1
c     
         px = p(i)
         ti = 0.5d0*(tre+tle)
c     
         if (etalp.eq.1) then
c     
c *** If the pump is 100 % efficient then the enthalpy does
c *** not change.

            if (sri.gt.sg(i)) then 
c     
c *** Superheated vapour.

               call supvap1(px,sri,hi,ti) 
c     
            else
c     
c *** Two-phase region.

               call twophase(px,ti,sri,hi)
c     
            end if   
c     
         end if
c     
         if (etalp.ne.1) then
c     
c *** Superheated vapour.

            call supvap3(px,ti,hi,si)
c     
            if (si.gt.sg(i)) then
c     
               h(i) = hi
               t(i) = ti
               s(i) = si
c     
            else
c     
c *** Two-phase region.

               call twophas2(px,ti,hi,si)

               h(i) = hi
               t(i) = ti
               s(i) = si
c     
            end if
c     
         end if
c     
 1006 continue
c     
      dh(1) = vsat(1)*(ph-p(1))*1000.0d0/etafp
      hsi = hsat(1)+dh(1)
c     
c *** Steam mass flow rate: determined by consideration of 
c *** steam generator heat balance. The steam mass flow
c *** rate is altered by the presence of heated coolant coming
c *** from the shield and first wall/divertor.

      ms = ((mf*cf/1000.0d0)*(tfo-tfi)+xqfus(2)*1.0d3+
     +     xqfus(3)*1.0d3)/(hso-hsi+hri-hhe)

c *** Heat change (MW) in passing through the steam generator
c *** and during the reheat stage.

      qsg = ms*(hso-hsi)/1000.0d0
      qr = ms*(hri-hhe)/1000.0d0

c *** Boundary conditions of the mass flow rate.

      mb(0) = 0.0d0
      mb(nipfwh+nlpfwh+2) = 0.0d0
      m(0) = ms
      m(1) = m(0)-mb(0)
c     
      do 1008 i = 1,nipfwh+nlpfwh+1
c     
         if (i.eq.(nipfwh+nlpfwh+1)) then 
c     
            eta = etacp 
c     
         else
c     
            eta = etafp
c     
         end if   
c     
c *** Evaluate the enthalpy rise due to each feed pump and
c *** the corresponding mass flow rates.

         dh(i+1) = vsat(i+1)*(p(i)-p(i+1))*1000.0d0/eta
         mb(i) = m(i)*(hsat(i)-hsat(i+1)-dh(i+1))
     +        /(h(i)-hsat(i+1)-dh(i+1))
         m(i+1) = m(i)-mb(i)
c     
 1008 continue
c     
c *** Power calculation
c *** -----------------

      do 1009 i = 1,nipfwh+nlpfwh+2
c     
         wt(i) = m(i)*(h(i-1)-h(i))/1000.0d0
         wp(i) = m(i)*dh(i)/1000.0d0
c     
 1009 continue
c     
      wcp = wp(nipfwh+nlpfwh+2)
      whpt = ms*(hhi-hhe)/1000.0d0

c *** Sum powers (MW).

      sum1 = 0.0d0
      sum2 = 0.0d0
      sum3 = 0.0d0
      sum4 = 0.0d0
c     
      do 1010 i = 1,nipfwh+1
c     
         sum1 = sum1+wt(i)
         sum3 = sum3+wp(i)
c     
 1010 continue
c   
      do 1011 i = nipfwh+2,nipfwh+nlpfwh+2
c     
         sum2 = sum2+wt(i)
         sum4 = sum4+wp(i)
c     
 1011 continue
c     
      winpt = sum1
      wlpt = sum2
      wst = whpt+sum1+sum2
      winpfp = sum3
      wcp = wp(nipfwh+nlpfwh+2)
      wlpfp = sum4-wcp
      wp1 = sum3+sum4
      qc = m(nipfwh+nlpfwh+2)*(h(nipfwh+nlpfwh+2)-hc)/1000.0d0
      wnet = wst-wp1
c 
      wnet1 = wst-wp1-qpump/1000000.0d0
      qin = ms*(hso-hsi+hri-hhe)/1000.0d0

c *** Plant and cycle efficiencies.

      etacycle = wnet/qin
      etaplant = wnet1/qin
      xexit = (hle-hc)/(hgc-hc)
c     
      if (iprint.eq.1) then

         call oblnkl(nout)
         call ocentr(nout,'Thermal Cycle Data',72)
         call oblnkl(nout)
c     
         do 1012 i = 0,nipfwh+nlpfwh+2
c     
            call ovarrf(nout,'Pressure (MPa)','(p)',p(i))
            call ovarin(nout,'Extraction point','(i)',i)
            call ovarrf(nout,'Enthalpy (kJ/kg)','(h)',h(i))
            call ovarrf(nout,'Entropy (kJ/kg/K)','(s)',s(i))
            call ovarrf(nout,'Temperature (C)','(t)',t(i)-273.15d0)
            call ovarrf(nout,'Bled mass flow rate (kg/s)','(mb)',mb(i))
            call ovarin(nout,'Feedwater heater train point','(i)',i)
            call ovarrf(nout,'Saturated enthalpy (kJ/kg)','(hsat)',
     +           hsat(i))
            call ovarrf(nout,'Saturated temperature (C)','(tsat)',
     +           tsat(i))
            call ovarrf(nout,'Saturated specific volume (m**3/kg)',
     +           '(vsat)',vsat(i))
            call ovarrf(nout,'Mass flow rate (kg/s)','(m)',m(i))
            call oblnkl(nout)
c     
 1012    continue
c   
         call ocentr(nout,'Steam Output Data',72)
         call oblnkl(nout)
c     
         call ovarrf(nout,'SG inlet enthalpy (kJ/kg)','(hsi)',hsi)
         call ovarrf(nout,'SG exit enthalpy (kJ/kg)','(hso)',hso)
         call ovarrf(nout,'SG inlet temp (C)','(tsi)',tsi-273.15d0)
         call ovarrf(nout,'SG exit temp (C)','(tso)',tso-273.15d0)
         call ovarrf(nout,'HP turbine inlet  h (kJ/kg)','(hhi)',hhi)
         call ovarrf(nout,'HP turbine outlet  h (kJ/kg)','(hhe)',hhe)
         call ovarrf(nout,'Reheat inlet (IP inlet) h (kJ/kg)','(hri)',
     +        hri)
         call ovarrf(nout,'IP outlet (LP turb inlet) h (kJ/kg)','(hre)',
     +        hre)
         call ovarrf(nout,'LP turb outlet h (kJ/kg)','(hle)',hle)
         call ovarrf(nout,'Condenser enthalpy (kJ/kg)','(hc)',hc)
         call ovarrf(nout,'Coolant mass flow rate (kg/s)','(mf)',mf)
         call ovarrf(nout,'Steam mass flow rate (kg/s)','(ms)',ms)
         call ovarrf(nout,'LP turbine exit dryness','(xexit)',xexit)
         call ovarrf(nout,'HP turb. output (MW)','(whpt)',whpt)
         call ovarrf(nout,'IP turb. output (MW)','(winpt)',winpt)
         call ovarrf(nout,'LP turb. output (MW)','(wlpt)',wlpt)
         call ovarrf(nout,'IP-FP power (MW)','(winpfp)',winpfp)
         call ovarrf(nout,'LP-FP power (MW)','(wlpfp)',wlpfp)
         call ovarrf(nout,'CP power (MW)','(wcp)',wcp)
         call ovarrf(nout,'Qin (MW)','(qin)',qin)
         call ovarrf(nout,'Qs (MW)','(qsg)',qsg)
         call ovarrf(nout,'Qr (MW)','(qr)',qr)
         call ovarrf(nout,'Qcond (MW)','(qc)',qc)
         call ovarrf(nout,'Wnet plant (MW)','(wnet1)',wnet1)
         call ovarrf(nout,'Wnet cycle (MW)','(wnet)',wnet)
         call ovarrf(nout,'Plant Efficiency','(etaplant)',etaplant)
         call ovarrf(nout,'Cycle Efficiency','(etacycle)',etacycle)

      end if   

 2000 return
      end

c *********************************************************************

      subroutine flow(mf,tfo,tfi,tb,dh,lc,ac,asurf,per,c1,c2,htc,ff,
     +     dpf,qpump) 

c *********************************************************************

c *** Calculate fluid properties of the liquid coolant.

c *** mf (INPUT): coolant mass flow rate (kg/s)
c *** tfo (INPUT): coolant outlet temperature (K) 
c *** tfi (INPUT): coolant inlet temperature (K) 
c *** tb (INPUT): maximum blanket temperature (K)
c *** dh (INPUT): hydraulic diameter (m)
c *** lc (INPUT): radial or half the poloidal extent of blanket (m)
c *** ac (INPUT): cooling channel cross sectional area (m**2)
c *** asurf (INPUT): coolant channel surface area (m**2)
c *** per (INPUT): coolant channel circumference (m)
c *** c1 (OUTPUT): constant
c *** c2 (OUTPUT): constant
c *** htc (OUTPUT): heat transfer coefficient (W/m**2/K)
c *** ff (OUTPUT): coefficient of friction
c *** dpf (OUTPUT): pressure drop along coolant channel (Pa)
c *** qpump (OUTPUT): pumping power (W)

c
      include 'blanket.h'

      double precision mf,tfo,tfi,tb,dh,lc,ac,asurf,per,c1,c2,htc
      double precision ff,dpf,qpump,g0,a,pf,reyd,cf,rhof,viscf
      double precision viscfs,pran,prans,kf,kfs,ksolid,mf0
      double precision a0,c,b0,b,c0,has,mult
      integer ncc

      common/com2/g0,a,pf,reyd,ncc

      call props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans,
     +     kf,kfs,ksolid)

c *** Single coolant channel mass flow rate.

      mf0 = mf/dble(ncc)

c *** Coolant flow Reynolds number.

      reyd = mf0*dh/(ac*viscf)

c *** The 1.5 factor in the expression  for the coolant pumping
c *** power is equivalent to assigning the pump efficiency to 2/3
c *** (efficiency = power required to pump coolant/pumping power).

      if (reyd.gt.2300.0d0) then

c *** Turbulent flow.

         a0 = 0.023d0
         a = 0.8d0
         c = 0.4d0
         b0 = 0.046d0
         b = -0.2d0
         c0 = a0*kf*pran**c*(4.0d0/viscf)**a
         htc = c0*(mf0/per)**a/dh
         has = htc*asurf*dble(ncc)
         c1 = c0*asurf*dble(ncc)**(1.0d0-a)/(dh*per**a*cf)
         c2 = g0*c0/(ksolid*dh*(per*dble(ncc))**a)
         ff = b0*reyd**b
         dpf = 2.0d0*ff*(lc/dh)*(mf0/ac)**2/rhof

         if (estr.eq.1) mult = 2.0d0
         if (estr.eq.2) mult = 1.0d0

         qpump = 1.50d0*mult*dble(ncc)*mf0*dpf/(rhof)

      else

c *** Laminar flow.

         a0 = 3.66d0
         a = 0.0d0
         c = 0.0d0
         b0 = 16.0d0
         b = -1.0d0
         c0 = a0*kf*pran**c*(4.0d0/viscf)**a
         htc = c0*(mf0/per)**a/dh
         has = htc*asurf*dble(ncc)
         c1 = c0*asurf*dble(ncc)**(1.0d0-a)/(dh*per**a*cf)
         c2 = g0*c0/(ksolid*dh*(per*dble(ncc))**a)
         ff = b0*reyd**b
         dpf = 2.0d0*ff*(lc/dh)*(mf0/ac)**2/rhof

         if (estr.eq.1) mult = 2.0d0
         if (estr.eq.2) mult = 1.0d0

         qpump = 1.5d0*mult*dble(ncc)*mf0*dpf/(rhof)

      end if

      return
      end

c *********************************************************************

      subroutine props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans,
     +     kf,kfs,ksolid) 

c *********************************************************************

c *** Calculate various temperature dependent coolant properties.

c *** tfo (INPUT): coolant outlet temperature (K)
c *** tfi (INPUT): coolant inlet temperature (K)
c *** tb (INPUT): maximum blanket temperature (K)
c *** cf (OUTPUT): specific heat of coolant (J/kg/K)
c *** rhof (OUTPUT): coolant mass density (kg/m**3)
c *** viscf (OUTPUT): viscosity at T=x (kg/s/m)
c *** viscfs (OUTPUT): viscosity at T=y (kg/s/m)
c *** pran (OUTPUT): viscf*cf/kf 
c *** prans (OUTPUT): viscfs*cfs/kfs
c *** kf (OUTPUT): thermal conductivity of coolant at T=x (W/m/K)
c *** kfs (OUTPUT): thermal conductivity of coolant at T=y (W/m/K)
c *** ksolid (OUTPUT): thermal conductivity of blanket (W/m/K)

      include 'blanket.h'

      double precision tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans
      double precision kf,kfs,ksolid,g0,a,pf,reyd,x,y,gascf,cfs
      double precision ks1,ks2
      integer ncc

      common/com2/g0,a,pf,reyd,ncc

c *** Local variables
c *** ---------------
c ***
c *** x: average coolant temperature (K)
c *** y: average temperature difference between coolant and 
c ***    blanket (K)
c *** gascf: specific heat capacity of gaseous coolant (J/kg/K)

      x = 0.5d0*(tfo+tfi)
      y = 0.5d0*((tb-tfo)+(tb-tfi))

c *** The blanket is a composite of two materials,
c *** the smallest thermal conductivity is used.

      if (smstr.eq.1) then

         ks1 = 100.0d0/(1.4d0+0.01828d0*y)
         ks2 = 233.54d0-0.28168d0*y+1.8566d-04*y*y
     +     -4.4946d-08*y*y*y

      else if (smstr.eq.2) then 

         if (y.lt.508.0d0) then

            ks1 = 17.7d0+0.0294d0*y 

         else 

            ks1 = 1.95d0+0.0196d0*y

         end if   

         ks2 = 34.9246d0+0.019037d0*y

      end if

      ksolid = min(ks1,ks2)

c *** Calculate the specific heat of the blanket.

      if (costr.eq.1) then

c *** Helium coolant.

         gascf = 8314.3d0/4.0026d0
         kf = 0.033378d0+4.2674d-04*x-1.0807d-07*x*x
         viscf = 4.7744d-07*x**0.6567d0
         kfs = 0.033378d0+4.2674d-04*y-1.080d-07*y*y
         viscfs = 4.7744d-07*y**0.6567d0
         cf = 5193.0d0
         pran = viscf*cf/kf
         prans = viscfs*cf/kfs
         rhof = pf/(gascf*x)

      end if

      if (costr.eq.2) then

c *** Pressurized water coolant.

         kf = 8.9372d0-0.048702d0*x+9.6994d-05*x*x-6.5541d-08*x*x*x
c  viscf=(4341.0d0-22.027d0*x+0.038615d0*x*x-2.2887d-05*x*x*x)/1.0d6
         kfs = 8.9372d0-0.048702d0*y+9.6994d-05*y*y-6.5541d-08*y*y*y
c  viscfs=(4341.0d0-22.027d0*y+0.038615d0*y*y-2.2887d-05*y*y*y)/1.0d6
         cf = -371240.0d0+2188.7d0*x-4.2565d0*x*x+0.0027658d0*x*x*x
         cfs = -371240.0d0+2188.7d0*y-4.2565d0*y*y+0.0027658d0*y*y*y
         pran = -68.469d0+0.41786d0*x-8.3547d-04*x*x+5.5443d-07*x*x*x
         prans = -68.469d0+0.41786d0*y-8.3547d-04*y*y+5.5443d-07*y*y*y
         rhof = 15307.0d0-81.04d0*x+0.15318d0*x*x-9.8061d-05*x*x*x
         viscf = kf*pran/cf
         viscfs = kfs*prans/cfs

      end if

      return
      end

c *********************************************************************

      subroutine satliq(p,t,hf,vf,hg,sg) 

c *********************************************************************

c *** Calculate properties of saturated liquid and vapour
c *** at a given liquid/vapour pressure.

c *** p (INPUT): saturated liquid/steam pressure (MPa)
c *** t (OUTPUT): saturated temperature (K)
c *** hf (OUTPUT): saturated liquid enthalpy (kJ/kg)
c *** vf (OUTPUT): saturated liquid specific volume (m**3/kg)
c *** hg (OUTPUT): saturated vapour enthalpy (kJ/kg)
c *** sg (OUTPUT): saturated vapour entropy (kJ/kg)

      double precision p,t,hf,vf,hg,sg,ta1,ta2,ta3,ta4,ta5,ta6,ta7
      double precision ha1,ha2,ha3,ha4,ha5,ha6,ha7,va1,va2,va3,va4
      double precision va5,va6,va7

c *** Saturated liquid properties.

      ta1 = 168.396d0
      ta2 = 0.314653d0
      ta3 = -0.000728d0
      ta4 = 31.588979d0
      ta5 = 11.473141d0
      ta6 = -0.575335d0
      ta7 = 0.013165d0
      t = 273.15d0+ta1+ta2/p+ta3/p**2+ta4*log(p)+ta5*p+ta6*p*p+ta7*p*p*p
      ha1 = 702.603552d0
      ha2 = 1.289508d0
      ha3 = -0.002973d0
      ha4 = 131.378047d0
      ha5 = 59.892944d0
      ha6 = -2.739215d0
      ha7 = 0.079064d0
      hf = ha1+ha2/p+ha3/p**2+ha4*log(p)+ha5*p+ha6*p*p+ha7*p*p*p
      va1 = 1084.060198d0
      va2 = 0.217242d0
      va3 = -0.000394d0
      va4 = 19.993979d0
      va5 = 43.156082d0
      va6 = -2.293468d0
      va7 = 0.115701d0
      vf = (va1+va2/p+va3/p**2+va4*log(p)+va5*p+va6*p*p+va7*p*p*p)/1.0d6

c *** Saturated vapour properties.

      if ((p.ge.0.004d0).and.(p.lt.0.1d0)) then

         hg = 2765.4675d0*p**0.0147d0
         sg = 6.6542d0*p**(-0.0441d0)

      end if

      if ((p.gt.0.1d0).and.(p.lt.1.0d0)) then

         hg = 2777.9404d0*p**0.0162d0
         sg = 6.5914d0*p**(-0.0484d0)

      end if

      if ((p.ge.1.0d0).and.(p.lt.20.0d0)) then

         hg = 2741.1d0+48.918d0*p-13.46d0*p**2+1.6412d0*p**3
     +        -0.11537d0*p**4+0.0041868d0*p**5-6.226d-05*p**6
         sg = 6.9035d0-0.40221d0*p+0.077209d0*p**2-0.009548d0*p**3
     +        +6.5518d-04*p**4-2.3121d-05*p**5+3.2419d-06*p**6

      end if

      return
      end

c *********************************************************************

      subroutine supvap(p,t,h,s,rho,v) 

c *********************************************************************

c *** Analytical approximations to the following expressions.

c ***                     ,t              ,p
c ***                     |               |
c *** h(p,t) = h(pr,tr) + |  cp(pr,t)dt + |  ( v - T [dv/dT] )dp
c ***                     |               |                 p
c ***                     'tr             'pr
c ***
c ***                     ,t              ,p
c ***                     |               |
c *** s(p,t) = s(pr,tr) + |  cp(pr,t)dt + |  ( [dv/dT] )dp
c ***                     |          --   |           p
c ***                     'tr        t    'pr

c *** p (INPUT): superheated vapour pressure (MPa)
c *** t (INPUT): saturated vapour temperature (K)
c *** h (OUTPUT): superheated vapour enthalpy (kJ/kg)
c *** s (OUTPUT): superheated vapour entropy (kJ/kg)
c *** rho (OUTPUT): superheated vapour mass density (kg/m**3)
c *** v (OUTPUT): superheated vapour specific volume (m**3/kg)

      double precision p,t,h,s,rho,v,hr,sr,xtr,tr,pref,xp,mw
      double precision gasc,acap,bcap,ccap,a,b,a1,a2,a3,a4
      double precision beta,gamma,delta,theta,thetar,aa,x,ab
      double precision ac1,ac2,ac3,ac,ad2,ad3,ad4,ad5,ad,bb
      double precision saa,as0,as1,as2,as3,sbb

c *** Local variables
c *** ---------------
c ***
c *** hr: reference enthalpy (kJ/kg)
c *** sr: reference entropy (kJ/kg)
c *** tr: reference tmeperature (K)
c *** pref: reference pressure (Pa)
c *** xp: superheated vapour pressure (Pa)
c *** mw: molar mass of superheated vapour (kg/mol)
c *** gasc: specific heat of vapour (J/kg/K)

c *** Reference values.

      hr = 2501.6d0
      sr = 9.157d0
      xtr = 0.1d0
      tr = xtr+273.15d0
      pref = 611.2d0
      xp = p*1.0d6

c *** various constants. 

      mw = 18.015d0
      gasc = 8314.3d0/mw
      acap = 0.769185d0
      bcap = 0.00126d0

      if (p.lt.1.0d0) ccap = 1220000.0d0
      if ((p.ge.1.0d0).and.(p.le.10.0d0)) ccap = 1300000.0d0
      if ((p.gt.10.0d0).and.(p.le.40.0d0)) ccap = 1325000.0d0
      if ((p.gt.40.0d0).and.(p.le.60.0d0)) ccap = 1330000.0d0
      if ((p.gt.60.0d0).and.(p.lt.100.0d0)) ccap = 1335000.0d0
      if ((p.ge.100.0d0).and.(p.lt.180.0d0)) ccap = 1365000.0d0
      if (p.ge.180.0d0) ccap = 1380000.0d0

      a = 0.005318d0
      b = 0.002482d0
      a1 = 143.05d0/mw
      a2 = -183.54d0/mw
      a3 = 82.751d0/mw
      a4 = -3.6989d0/mw
      beta = gasc*t*bcap-acap-gasc*ccap/(t*t)
      gamma = acap*a-gasc*b*bcap*t-gasc*bcap*ccap/(t*t)
      delta = gasc*bcap*b*ccap/(t*t)

c *** Expressions for the superheated vapour entropy and enthalpy
c *** as a function of pressure and temperature (Beattie-Bridgeman
c *** equations).

      theta = t/100.0d0
      thetar = tr/100.0d0

c *** Constant pressure term.

      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      x = gasc*t
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t**3
      ac1 = -2.0d0*b*bcap-2.0d0*bcap**2-8.0d0*ccap**2/t**6+
     +     5.0d0*bcap*ccap/t**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0d0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t**3+27.0d0*bcap*ccap**2/t**6-
     +     24.0d0*ccap**3/t**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t**3-
     +     60.0d0*acap*ccap**2/t**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2
     +     -48.0d0*ccap*acap**2/t**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0

c *** Constant pressure term.

      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3

c *** Enthalpy.

      h = hr+aa*100.0d0+bb

c *** Specific volume and mass density.

      v = gasc*t/xp+beta/(gasc*t)+(gamma-beta*beta/(gasc*t))*
     +     xp/(gasc**2*t*t)+(delta-3.0d0*beta*gamma/(gasc*t)+
     +     2.0d0*beta**3/(gasc*t)**2)*xp*xp/(gasc**3*t*t*t)
      rho = 1.0d0/v

c *** Constant pressure term.

      saa = a1*log(theta/thetar)+a4*(theta-thetar)+4.0d0*a2*
     +     (theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t+3.0d0*gasc*ccap/t**3)/x
      as2 = (0.5d0*bcap*b/t+2.0d0*bcap*ccap/t**4)/x-(acap*a/t)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t**4)/(3.0d0*x**2)

c *** Constant temperature term.

      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-
     +     as3*(xp*xp*xp-pref*pref*pref))/1.0d3

c *** Entropy.

      s = sr+saa+sbb

      return
      end

c *********************************************************************

      subroutine supvap1(p,s,h,t) 

c *********************************************************************

c *** Calculates the enthalpy at (p,s)

c *** p (INPUT): superheated vapour pressure (MPa)
c *** s (INPUT): superheated vapour entropy (kJ/kg)
c *** h (OUTPUT): superheated vapour enthalpy (kJ/kg)
c *** t (OUTPUT): superheated vapour temperature (K)

      double precision p,s,h,t,tol,tc,tso,hr,sr,xtr,tr,pref,xp,mw
      double precision gasc,acap,bcap,ccap,a,b,a1,a2,a3,a4,x,theta
      double precision thetar,saa,as0,as1,as2,as3,sbb,gg,t1,ggf,t2
      double precision ggb,dgg,mt,beta,gamma,delta,aa,ab,ac1,ac2
      double precision ac3,ac,ad2,ad3,ad4,ad5,ad,bb

      common/com3/tol,tc,tso

c *** Reference values.

      hr = 2501.6d0
      sr = 9.157d0
      xtr = 0.1d0
      tr = xtr+273.15d0
      pref = 611.2d0
      xp = p*1.0d6

c *** various constants

      mw = 18.015d0
      gasc = 8314.3d0/mw
      acap = 0.769185d0
      bcap = 0.00126d0
      if (p.lt.1.0d0) ccap = 1220000.0d0
      if ((p.ge.1.0d0).and.(p.le.10.0d0)) ccap = 1300000.0d0
      if ((p.gt.10.0d0).and.(p.le.40.0d0)) ccap = 1325000.0d0
      if ((p.gt.40.0d0).and.(p.le.60.0d0)) ccap = 1330000.0d0
      if ((p.gt.60.0d0).and.(p.lt.100.0d0)) ccap = 1335000.0d0
      if ((p.ge.100.0d0).and.(p.lt.180.0d0)) ccap = 1365000.0d0
      if (p.ge.180.0d0) ccap = 1380000.0d0

      a = 0.005318d0
      b = 0.002482d0
      a1 = 143.05d0/mw
      a2 = -183.54d0/mw
      a3 = 82.751d0/mw
      a4 = -3.6989d0/mw

c *** Fix temperature to the outlet coolant temperature minus
c *** 100 K.

      t = tso-100.0d0

 110  continue

c *** Given the entropy and pressure find the temperature at
c *** which the Beattie-Bridgeman equations result in the
c *** same entropy.

      x = gasc*t
      theta = t/100.0d0
      thetar = tr/100.0d0

c *** Entropy term at constant pressure.

      saa = a1*log(theta/thetar)+a4*(theta-thetar)+
     +     4.0d0*a2*(theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t+3.0d0*gasc*ccap/t**3)/x
      as2 = (0.5d0*bcap*b/t+2.0d0*bcap*ccap/t**4)/x-(acap*a/t)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t**4)/(3.0d0*x**2)

c *** Entropy term at constant temperature.

      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-
     +     as3*(xp*xp*xp-pref*pref*pref))/1.0d3

c *** Difference between calculated entropy and entropy
c *** carried as an argument to this subroutine.

      gg = sr+saa+sbb-s

c ***Increment the temperature.

      t1 = t+1.0d0
      x = gasc*t1
      theta = t1/100.0d0
      thetar = tr/100.0d0

c *** Entropy term at constant pressure.

      saa = a1*log(theta/thetar)+a4*(theta-thetar)+
     +     4.0d0*a2*(theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t1+3.0D0*gasc*ccap/t1**3)/x
      as2 = (0.5d0*bcap*b/t1+2.0D0*bcap*ccap/t1**4)/x-(acap*a/t1)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t1**4)/(3.0d0*x**2)

c *** Entropy term at constant temperature.

      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-
     +     as3*(xp*xp*xp-pref*pref*pref))/1.0d3
      ggf = sr+saa+sbb-s

c *** Decrement the temperature.

      t2 = t-1.0d0
      x = gasc*t2
      theta = t2/100.0d0
      thetar = tr/100.0d0

c *** Entropy term at constant pressure.

      saa = a1*log(theta/thetar)+a4*(theta-thetar)+
     +     4.0d0*a2*(theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t2+3.0d0*gasc*ccap/t2**3)/x
      as2 = (0.5d0*bcap*b/t2+2.0D0*bcap*ccap/t2**4)/x-(acap*a/t2)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t2**4)/(3.0d0*x**2)

c *** Entropy term at constant temperature.

      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-
     +     as3*(xp*xp*xp-pref*pref*pref))/1.0d3
      ggb = sr+saa+sbb-s

c *** Derivative of the entropy change with temperature.

      dgg = (ggf-ggb)/2.0d0

c *** Employ Newton Raphson to find the temperature where
c *** the difference between the calculated and read in
c *** entropies is zero.

      mt = t-0.05d0*gg/dgg

c *** Has the solution converged.

      if (ABS(mt-t).lt.tol) goto 120

      t = mt

      goto 110

 120  continue

      t = mt

c *** Calculate the enthalpy at (p,t)

      beta = gasc*t*bcap-acap-gasc*ccap/(t*t)
      gamma = acap*a-gasc*b*bcap*t-gasc*bcap*ccap/(t*t)
      delta = gasc*bcap*b*ccap/(t*t)

      theta = t/100.0d0
      thetar = tr/100.0d0

c *** constant pressure term

      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      x = gasc*t
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t**3
      ac1 = -2.0d0*b*bcap-2.0D0*bcap**2-8.0d0*ccap**2/t**6+
     +     5.0d0*bcap*ccap/t**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0d0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t**3+27.0d0*bcap*ccap**2/t**6-
     +     24.0d0*ccap**3/t**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t**3-
     +     60.0d0*acap*ccap**2/t**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2
     +     -48.0d0*ccap*acap**2/t**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0

c *** constant temperature term.

      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1000.0d0
      h = hr+aa*100.0d0+bb

      return
      end

c *********************************************************************

      subroutine supvap2(p,h,s,t)

c *********************************************************************

c *** Calculate the entropy at (p,h).

c *** p (INPUT): superheated vapour pressure (MPa)
c *** h (INPUT): superheated vapour enthalpy (kJ/kg)
c *** s (OUTPUT): superheated vapour entropy (kJ/kg)
c *** t (OUTPUT): superheated vapour temperature (K)

      double precision p,h,s,t,tol,tc,tso,hr,sr,xtr,tr,pref,xp,mw
      double precision gasc,acap,bcap,ccap,b,a1,a2,a3,a4,theta
      double precision thetar,aa,x,ab,ac1,ac2,ac3,a,ac,ad2,ad3
      double precision ad4,ad5,ad,bb,gh,t1,ghf,t2,ghb,dgh,mt
      double precision saa,as0,as1,as2,as3,sbb

      common/com3/tol,tc,tso

c *** Local variables
c *** ---------------
c ***
c *** tc: saturated temperature (K)

c *** Reference values.

      hr = 2501.6d0
      sr = 9.157d0
      xtr = 0.1d0
      tr = xtr+273.15d0
      pref = 611.2d0
      xp = p*1.0d6

c *** various constants.

      mw = 18.015d0
      gasc = 8314.3d0/mw
      acap = 0.769185d0
      bcap = 0.00126d0

      if (p.lt.1) ccap = 1220000.0d0
      if ((p.ge.1.0d0).and.(p.le.10.0d0)) ccap = 1300000.0d0
      if ((p.gt.10.0d0).and.(p.le.40.0d0)) ccap = 1325000.0d0
      if ((p.gt.40.0d0).and.(p.le.60.0d0)) ccap = 1330000.0d0
      if ((p.gt.60.0d0).and.(p.lt.100.0d0)) ccap = 1335000.0d0
      if ((p.ge.100.0d0).and.(p.lt.180.0d0)) ccap = 1365000.0d0
      if (p.ge.180) ccap = 1380000.0d0

      a = 0.005318d0
      b = 0.002482d0
      a1 = 143.05d0/mw
      a2 = -183.54d0/mw
      a3 = 82.751d0/mw
      a4 = -3.6989d0/mw

      t = 0.5d0*(tso+tc)

c *** Calculate the temperature at which the Beattie-Bridgeman
c *** equations give an enthalpy indentical to H (within the
c *** tolerance).

 130  continue 

c *** Calculate the enthalpy at (p,t).

      theta = t/100.0d0
      thetar = tr/100.0d0
      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      x = gasc*t
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t**3
      ac1 = -2.0d0*b*bcap-2.0d0*bcap**2-8.0d0*ccap**2/t**6+
     +     5.0d0*bcap*ccap/t**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0d0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t**3+27.0d0*bcap*ccap**2/t**6-
     +     24.0d0*ccap**3/t**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t**3-
     +     60.0d0*acap*ccap**2/t**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2
     +     -48.0d0*ccap*acap**2/t**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0
      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      gh = h-(hr+aa*100.0D0+bb)

c *** Calculate the enthalpy at (p,t+1).

      t1 = t+1.0d0
      theta = t1/100.0d0
      thetar = tr/100.0d0
      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      x = gasc*t1
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t1**3
      ac1 = -2.0d0*b*bcap-2.0d0*bcap**2-8.0d0*ccap**2/t1**6+
     +     5.0d0*bcap*ccap/t1**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t1**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0D0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t1**3+27.0d0*bcap*ccap**2/t1**6-
     +     24.0d0*ccap**3/t1**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t1**3-
     +     60.0d0*acap*ccap**2/t1**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2-
     +     48.0d0*ccap*acap**2/t1**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0
      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      ghf = h-(hr+aa*100.0D0+bb)

c *** Calculate the enthalpy at (p,t-1).

      t2 = t-1.0d0
      theta = t2/100.0d0
      thetar = tr/100.0d0
      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      x = gasc*t2
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t2**3
      ac1 = -2.0d0*b*bcap-2.0d0*bcap**2-8.0d0*ccap**2/t2**6+
     +     5.0d0*bcap*ccap/t2**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t2**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0d0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t2**3+27.0d0*bcap*ccap**2/t2**6-
     +     24.0d0*ccap**3/t2**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t2**3-
     +     60.0d0*acap*ccap**2/t2**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2-
     +     48.0d0*ccap*acap**2/t2**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0
      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      ghb = h-(hr+aa*100.0d0+bb)

c *** Derivative of the enthaply difference with temperature.

      dgh = (ghf-ghb)/2.0d0

c *** Employ Newton Raphson to find the temperature at which
c *** this difference between the calculated and read in
c *** entropies is zero.

      mt = t-0.05d0*gh/dgh

      if (ABS(mt-t).lt.tol) goto 140

      t = mt

      goto 130

 140  continue

c *** Calculate the entropy at (p,t).

      t = mt
      x = gasc*t
      theta = t/100.0d0
      thetar = tr/100.0d0

c *** constant pressure term.

      saa = a1*log(theta/thetar)+a4*(theta-thetar)+
     +     4.0d0*a2*(theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t+3.0d0*gasc*ccap/t**3)/x
      as2 = (0.5d0*bcap*b/t+2.0d0*bcap*ccap/t**4)/x-(acap*a/t)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t**4)/(3.0d0*x**2)

c *** constant temperature term.

      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-as3*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      s = sr+saa+sbb

      return
      end

c *********************************************************************

      subroutine supvap3(p,t,h,s) 

c *********************************************************************

c *** Calculates the entropy and enthalpy at extraction points.

c *** p (INPUT): saturated vapour pressure (MPa)
c *** t (INPUT): saturated vapour temperature (K)
c *** h (OUTPUT): saturated vapour enthalpy (kJ/kg)
c *** s (OUTPUT): saturated vapour entropy (kJ/kg)

      double precision p,t,h,s,tol,tc,tso,hri,hre,sri,sre,hli,hle
      double precision sli,sle,tre,hr,sr,xtr,tr,pref,xp,mw,gasc
      double precision acap,bcap,ccap,a,b,a1,a2,a3,a4,theta
      double precision thetar,aa,x,ab,ac1,ac2,ac3,ac,ad2,ad3
      double precision ad4,ad5,ad,bb,saa,as0,as1,as2,as3,sbb
      double precision gt,t1,h1,s1,gtf,t2,h2,s2,gtb,dgt,mt

      common/com3/tol,tc,tso
      common/com4/hri,hre,sri,sre,hli,hle,sli,sle,tre

c *** reference variables

      hr = 2501.6d0
      sr = 9.157d0
      xtr = 0.1d0
      tr = xtr+273.15d0
      pref = 611.2d0
      xp = p*1.0d6

c *** various constants

      mw = 18.015d0
      gasc = 8314.3d0/mw
      acap = 0.769185d0
      bcap = 0.00126d0

      if (p.lt.1.0d0) ccap = 1220000.0d0
      if ((p.ge.1.0d0).and.(p.le.10.0d0)) ccap = 1300000.0d0
      if ((p.gt.10.0d0).and.(p.le.40.0d0)) ccap = 1325000.0d0
      if ((p.gt.40.0d0).and.(p.le.60.0d0)) ccap = 1330000.0d0
      if ((p.gt.60.0d0).and.(p.lt.100.0d0)) ccap = 1335000.0d0
      if ((p.ge.100.0d0).and.(p.lt.180.0d0)) ccap = 1365000.0d0
      if (p.ge.180.0d0) ccap = 1380000.0d0

      a = 0.005318d0
      b = 0.002482d0
      a1 = 143.05d0/mw
      a2 = -183.54d0/mw
      a3 = 82.751d0/mw
      a4 = -3.6989d0/mw

c *** Calculate the temperature by employing the identity:
c ***
c *** (h-hri)    (hri-hre)
c *** -------  = ---------
c *** (s-sri)    (sre-sri)
c ***
c *** sre: outlet entropy (kJ/kg) 
c *** sri: inlet entropy (kJ/kg)
c *** hre: outlet enthalpy (kJ/kg)
c *** hri: inlet enthalpy (kJ/kg)
c ***
c *** This is done by using a Newton Raphson scheme to
c *** calculate (s,h) which satisfies the above equation.

 180  continue

c *** Enthalpy at (p,t)

      theta = t/100.0d0
      thetar = tr/100.0d0
      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      x = gasc*t
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t**3
      ac1 = -2.0d0*b*bcap-2.0d0*bcap**2-8.0d0*ccap**2/t**6+
     +     5.0d0*bcap*ccap/t**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0d0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t**3+27.0d0*bcap*ccap**2/t**6-
     +     24.0d0*ccap**3/t**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t**3-
     +     60.0d0*acap*ccap**2/t**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2
     +     -48.0d0*ccap*acap**2/t**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0
      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      h = hr+aa*100.0d0+bb

c *** Entropy at (p,t)

      saa = a1*log(theta/thetar)+a4*(theta-thetar)+
     +     4.0d0*a2*(theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t+3.0d0*gasc*ccap/t**3)/x
      as2 = (0.5d0*bcap*b/t+2.0d0*bcap*ccap/t**4)/x-(acap*a/t)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t**4)/(3.0d0*x**2)
      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-as3*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      s = sr+saa+sbb
      gt = hri-h-(s-sri)*(hri-hre)/(sre-sri)

c *** Enthalpy at (p,t+1)

      t1 = t+1.0d0
      theta = t1/100.0d0
      thetar = tr/100.0d0
      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      x = gasc*t1
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t1**3
      ac1 = -2.0d0*b*bcap-2.0d0*bcap**2-8.0d0*ccap**2/t1**6+
     +     5.0d0*bcap*ccap/t1**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t1**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0d0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t1**3+27.0d0*bcap*ccap**2/t1**6-
     +     24.0d0*ccap**3/t1**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t1**3-
     +     60.0d0*acap*ccap**2/t1**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2-
     +     48.0d0*ccap*acap**2/t1**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0
      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      h1 = hr+aa*100.0d0+bb

c *** Entropy at (p,t+1)

      saa = a1*log(theta/thetar)+a4*(theta-thetar)+
     +     4.0d0*a2*(theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t1+3.0d0*gasc*ccap/t1**3)/x
      as2 = (0.5d0*bcap*b/t1+2.0d0*bcap*ccap/t1**4)/x-(acap*a/t1)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t1**4)/(3.0d0*x**2)
      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-as3*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      s1 = sr+saa+sbb
      gtf = hri-h1-(s1-sri)*(hri-hre)/(sre-sri)

c *** Enthalpy at (p,t-1)

      t2 = t-1.0d0
      theta = t2/100.0d0
      thetar = tr/100.0d0
      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      x = gasc*t2
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t2**3
      ac1 = -2.0d0*b*bcap-2.0d0*bcap**2-8.0d0*ccap**2/t2**6+
     +     5.0d0*bcap*ccap/t2**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t2**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0d0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t2**3+27.0d0*bcap*ccap**2/t2**6-
     +     24.0d0*ccap**3/t2**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t2**3-
     +     60.0d0*acap*ccap**2/t2**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2-
     +     48.0d0*ccap*acap**2/t2**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0
      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      h2 = hr+aa*100.0d0+bb

c *** Entropy at (p,t-1)

      saa = a1*log(theta/thetar)+a4*(theta-thetar)+
     +     4.0d0*a2*(theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t2+3.0d0*gasc*ccap/t2**3)/x
      as2 = (0.5d0*bcap*b/t2+2.0d0*bcap*ccap/t2**4)/x-(acap*a/t2)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t2**4)/(3.0d0*x**2)
      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-as3*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      s2 = sr+saa+sbb
      gtb = hri-h2-(s2-sri)*(hri-hre)/(sre-sri)

      dgt = (gtf-gtb)/2.0d0
      mt = t-0.05d0*gt/dgt

      if (ABS(mt-t).lt.tol) goto 190

      t = mt

      goto 180

 190  continue

c *** Calculate the entropy and enthalpy at (p,t) from 
c *** the Beattie-Bridgeman equations.

      t = mt
      x = gasc*t
      theta = t/100.0d0
      thetar = tr/100.0d0
      saa = a1*log(theta/thetar)+a4*(theta-thetar)+
     +     4.0d0*a2*(theta**0.25d0-thetar**0.25d0)+
     +     2.0d0*a3*(theta**0.5d0-thetar**0.5d0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t+3.0d0*gasc*ccap/t**3)/x
      as2 = (0.5d0*bcap*b/t+2.0d0*bcap*ccap/t**4)/x-(acap*a/t)/x**2
      as3 = -5.0d0*(bcap*b*ccap/t**4)/(3.0d0*x**2)
      sbb = (as0-as1*(xp-pref)-as2*(xp*xp-pref*pref)-as3*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      s = sr+saa+sbb
      aa = a1*(theta-thetar)+a2*(theta**1.25d0-thetar**1.25d0)/1.25d0+
     +     a3*(theta**1.5d0-thetar**1.5d0)/1.5d0+
     +     a4*(theta**2-thetar**2)/2.0d0
      ab = bcap-2.0d0*acap/x-4.0d0*ccap/t**3
      ac1 = -2.0d0*b*bcap-2.0d0*bcap**2-8.0d0*ccap**2/t**6+
     +     5.0d0*bcap*ccap/t**3
      ac2 = 3.0d0*a*acap+6.0d0*acap*bcap-12.0d0*acap*ccap/t**3
      ac3 = -4.0d0*acap**2
      ac = 0.5d0*(ac1/x+ac2/x**2+ac3/x**3)
      ad2 = 9.0d0*b*bcap**2+6.0d0*bcap**3-(18.0d0*ccap*bcap**2+
     +     12.0d0*b*bcap*ccap)/t**3+27.0d0*bcap*ccap**2/t**6-
     +     24.0d0*ccap**3/t**9
      ad3 = -24.0d0*acap*bcap**2-12.0d0*acap*bcap*(a+b)+
     +     (63.0d0*acap*bcap*ccap+21.0d0*acap*ccap*a)/t**3-
     +     60.0d0*acap*ccap**2/t**6
      ad4 = 30.0d0*bcap*acap**2+15.0d0*a*acap**2
     +     -48.0d0*ccap*acap**2/t**3
      ad5 = -12.0d0*acap**3
      ad = (ad2/x**2+ad3/x**3+ad4/x**4+ad5/x**5)/3.0d0
      bb = (ab*(xp-pref)+ac*(xp*xp-pref*pref)+ad*(xp*xp*xp-
     +     pref*pref*pref))/1.0d3
      h = hr+aa*100.0D0+bb

      return
      end

c *********************************************************************

      subroutine twophase(p,t,s,h) 

c *********************************************************************

c *** Calculate the two-phase temperature and enthalpy.

c *** p (INPUT): saturated liquid/gas pressure (MPa)
c *** s (INPUT): saturated liquid/gas entropy (kJ/kg)
c *** t (OUTPUT): saturated liquid/gas temperature (K)
c *** h (OUTPUT): saturated liquid/gas enthalpy (kJ/kg)

      double precision p,t,s,h,a,b,c,d,ta1,ta2,ta3,ta4,ta5,ta6,ta7

c *** Enthalpy from the entropy.

      if ((p.ge.0.004d0).and.(p.lt.0.02d0)) then

         a = 2443.2013d0
         b = 0.0544d0
         c = 431.6173d0
         d = 0.0659d0

      end if

      if ((p.ge.0.02d0).and.(p.lt.0.2d0)) then

         a = 2490.1152d0
         b = 0.0592d0
         c = 437.2028d0
         d = 0.0711d0

      end if

      if ((p.ge.0.2d0).and.(p.lt.2.0d0)) then

         a = 2515.6971d0
         b = 0.066d0
         c = 452.6615d0
         d = 0.092d0

      end if

      h = a*p**b+c*p**d*(s-6.0d0)

c *** Saturated liquid temperature from the pressure.

      ta1 = 168.396d0
      ta2 = 0.314653d0
      ta3 = -0.000728d0
      ta4 = 31.588979d0
      ta5 = 11.473141d0
      ta6 = -0.575335d0
      ta7 = 0.013165d0
      t = 273.15d0+ta1+ta2/p+ta3/p**2+ta4*log(p)+ta5*p+ta6*p*p+ta7*p*p*p

      return
      end

c *********************************************************************

      subroutine twophas1(p,t,h,s) 

c *********************************************************************

c *** Calculate the two-phase temperature and entropy.

c *** p (INPUT): saturated liquid/vapour pressure (MPa)
c *** h (INPUT): saturated liquid/vapour enthalpy (kJ/kg)
c *** s (OUTPUT): saturated liquid/vapour entropy (kJ/kg)
c *** t (OUTPUT): saturated liquid/vapour temperature (K)

      double precision p,t,h,s,a,b,c,d,ta1,ta2,ta3,ta4,ta5,ta6,ta7

c *** Calculate the entropy from the enthalpy.

      if ((p.ge.0.004d0).and.(p.lt.0.02d0)) then

         a = 2443.2013d0
         b = 0.0544d0
         c = 431.6173d0
         d = 0.0659d0

      end if

      if ((p.ge.0.02d0).and.(p.lt.0.2d0)) then

         a = 2490.1152d0
         b = 0.0592d0
         c = 437.2028d0
         d = 0.0711d0

      end if

      if ((p.ge.0.2d0).and.(p.lt.2.0d0)) then

         a = 2515.6971d0
         b = 0.066d0
         c = 452.6615d0
         d = 0.092d0

      end if

      s = 6.0d0+(h-a*p**b)/(c*p**d)

c *** Saturated liquid temperature from the pressure.

      ta1 = 168.396d0
      ta2 = 0.314653d0
      ta3 = -0.000728d0
      ta4 = 31.588979d0
      ta5 = 11.473141d0
      ta6 = -0.575335d0
      ta7 = 0.013165d0
      t = 273.15d0+ta1+ta2/p+ta3/p**2+ta4*log(p)+ta5*p+ta6*p*p+ta7*p*p*p

      return
      end

c *********************************************************************

      subroutine twophas2(p,t,h,s) 

c *********************************************************************

c *** Calculate the entropy, enthalpy and the temperature given
c *** the pressure.

c *** p (INPUT): saturated liquid/vapour pressure (MPa)
c *** t (OUTPUT): saturated liquid/vapour temperature (K)
c *** h (OUTPUT): saturated liquid/vapour enthalpy (kJ/kg)
c *** s (OUTPUT): saturated liquid/vapour entropy (kJ/kg)

      double precision p,t,h,s,tol,tc,tso,hri,hre,sri,sre,hli,hle
      double precision sli,sle,tre,a,b,c,d,a1,a2,m,ta1,ta2
      double precision ta3,ta4,ta5,ta6,ta7

      common/com3/tol,tc,tso
      common/com4/hri,hre,sri,sre,hli,hle,sli,sle,tre

c *** Calculate both the entropy and enthalpy by using the
c *** expression:
c ***
c *** (h-hli)   (hli-hle)
c *** ------- = ---------
c *** (s-sli)   (sli-sle)
c ***       

      if ((p.ge.0.004d0).and.(p.lt.0.02d0)) then

         a = 2443.2013d0
         b = 0.0544d0
         c = 431.6173d0
         d = 0.0659d0

      end if

      if ((p.ge.0.02d0).and.(p.lt.0.2d0)) then

         a = 2490.1152d0
         b = 0.0592d0
         c = 437.2028d0
         d = 0.0711d0

      end if

      if ((p.ge.0.2d0).and.(p.lt.2.0d0)) then

         a = 2515.6971d0
         b = 0.066d0
         c = 452.6615d0
         d = 0.092d0

      end if

c *** h = a1+a2*(s-6)
c *** m (s-sli)  =  (h-hli)

      a1 = a*p**b
      a2 = c*p**d
      m = (hle-hli)/(sle-sli)
      s = (hli+6.0D0*a2-a1-m*sli)/(a2-m)
      h = a*p**b+c*p**d*(s-6.0d0)

c *** Calculate the saturation temperature given the pressure.

      ta1 = 168.396d0
      ta2 = 0.314653d0
      ta3 = -0.000728d0
      ta4 = 31.588979d0
      ta5 = 11.473141d0
      ta6 = -0.575335d0
      ta7 = 0.013165d0
      t = 273.15d0+ta1+ta2/p+ta3/p**2+ta4*log(p)+ta5*p+ta6*p*p+ta7*p*p*p

      return
      end
