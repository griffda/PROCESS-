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
C  Module         : $Id: fwbs.f,v 3.9 1997/02/19 14:54:34 peter Exp pknight $
C
C  Module name    : $RCSfile: fwbs.f,v $
C  Version no.    : $Revision: 3.9 $
C
C  Creation date  : $Date: 1997/02/19 14:54:34 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c----------------------------------------------------------------------
      SUBROUTINE FWBS(nout,iprint)

c  This subroutine calculates the nuclear heating in the blanket /
c  shield, and estimates the volume and masses of the first wall,
c  blanket and shield.
c
c  The arrays coef(i,j) and decay(i,j) are used for exponential decay
c  approximations of the (Superconducting) TF coil nuclear parameters.
c  j = 1 => stainless steel shield
c  j = 2 => tungsten shield.
c  Note: the code is only set up for stainless steel now (for costing
c  and mass calculations.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'cost.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'divrt.h'
      INCLUDE 'osections.h'
      INCLUDE 'blanket.h'

      DOUBLE PRECISION fact(5),coef(5,2),decay(7,2)

      DOUBLE PRECISION coilhtmx,decaybl,dpacop,dshieq,dshoeq,elong,
     +     flumax,fpsdt,fpydt,frachit,hb1,hblnkt,hecan,ht1,htheci,
     +     pheci,pheco,pneut1,pneut2,ptfi,ptfiwp,ptfo,ptfowp,r1,r2,
     +     r3,r4,raddose,rdewex,volshldi,volshldo,wpthk

      INTEGER iprint,ishmat,nout

      fact(1) = 8.0D0
      fact(2) = 8.0D0
      fact(3) = 6.0D0
      fact(4) = 4.0D0
      fact(5) = 4.0D0

      coef(1,1) = 10.3D0
      coef(2,1) = 11.6D0
      coef(3,1) = 7.08D5
      coef(4,1) = 2.19D18
      coef(5,1) = 3.33D-7
      coef(1,2) = 8.32D0
      coef(2,2) = 10.6D0
      coef(3,2) = 7.16D5
      coef(4,2) = 2.39D18
      coef(5,2) = 3.84D-7

      decay(1,1) = 10.05D0
      decay(2,1) = 17.61D0
      decay(3,1) = 13.82D0
      decay(4,1) = 13.24D0
      decay(5,1) = 14.31D0
      decay(6,1) = 13.26D0
      decay(7,1) = 13.25D0
      decay(1,2) = 10.02D0
      decay(2,2) = 3.33D0
      decay(3,2) = 15.45D0
      decay(4,2) = 14.47D0
      decay(5,2) = 15.87D0
      decay(6,2) = 15.25D0
      decay(7,2) = 17.25D0

c  Neutron power from plasma

C+**PJK 07/12/95 Modified pneut1 definition from:
C+**PJK 07/12/95 pneut1 = powfmw * 0.8D0

      pneut1 = pneut*vol

c  Neutron power lost through 'holes'

      pnucloss = pneut1 * fhole

C+**PJK 29/01/96
c  Centrepost nuclear heating. Estimate fraction hitting from a
c  point source at the plasma centre, and assume average path length
c  of 2*tfcth, and e-fold decay length of 0.08m (copper water mixture).

      if (itart.eq.1) then
         frachit = hmax / sqrt(hmax**2 + (rmajor-tfcth)**2 ) *
     +        atan(tfcth/(rmajor-tfcth) )/pi
         pnuccp = pneut1 * frachit * (1.0D0 - exp(-2.0D0*tfcth/0.08D0))
      else
         pnuccp = 0.0D0
      end if

      pneut2 = (pneut1 - pnucloss - pnuccp) * emult

c  Nuclear heating in the blanket

c+**CAG 25/05/93 new blanket model
      if (lblnkt.eq.1) then

c *** Solid blanket

         if (smstr.eq.1) decaybl = 0.075D0 / 
     +        (1.0D0 - vfblkt - fblli2o - fblbe)

c *** Liquid blanket

         if (smstr.eq.2) decaybl = 0.075D0 / 
     +        (1.0D0 - vfblkt - fbllipb - fblli)

      else   
         decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
      end if   

      pnucblkt = pneut2 * (1.0D0 - exp(-blnkoth/decaybl) )

c  Nuclear heating in the shield

      pnucshld = pneut2 - pnucblkt

c  Full power DT operation years for replacement of TF Coil
c  (or Plant Life)

      fpydt = cfactr * tlife
      fpsdt = fpydt * 3.154D7

      if (itfsup .eq. 1) then

c  Option for Superconducting TF coil shielding calculations

c  Preliminary calculations

         dshieq = shldith + fwith + blnkith
         dshoeq = shldoth + fwoth + blnkoth

c  Assume case thickness on plasma side = 1/2 average thickness

         hecan = thkcas/2.0D0
         wpthk = tfcth - 1.5D0 * thkcas

c  Nuclear heating rate in inboard TF coil (MW/m**3)
c  Set shield material to stainless steel

         ishmat = 1
         coilhtmx = fact(1) * wallmw * coef(1,ishmat) *
     +        exp(-decay(6,ishmat) * (dshieq + hecan))

c  Total nuclear heating (MW)

         ptfiwp = coilhtmx * tfsai *
     +        (1.0D0-exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)
         ptfowp = fact(1) * wallmw * coef(1,ishmat) *
     +        exp(-decay(6,ishmat) * (dshoeq + hecan)) * tfsao *
     +        (1.0D0 - exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)

c  Nuclear heating in He can (MW)

         htheci = fact(2) * wallmw * coef(2,ishmat) *
     +        exp(-decay(7,ishmat) * dshieq)
         pheci = htheci * tfsai * (1.0D0-exp(-decay(2,ishmat)*hecan))/
     +        decay(2,ishmat)
         pheco = fact(2) * wallmw * coef(2,ishmat) *
     +        exp(-decay(7,ishmat) * dshoeq) * tfsao *
     +        (1.0D0-exp(-decay(2,ishmat)*hecan))/decay(2,ishmat)
         ptfi = ptfiwp + pheci
         ptfo = ptfowp + pheco
         ptfnuc = ptfi + ptfo

c  Insulator dose (rad)

         raddose = coef(3,ishmat) * fpsdt * fact(3) * wallmw *
     +        exp(-decay(3,ishmat) * (dshieq+hecan))

c  Maximum neutron fluence in superconductor ( n/m**2)

         flumax = fpsdt * fact(4) * wallmw * coef(4,ishmat) *
     +        exp(-decay(4,ishmat) * (dshieq+hecan))

c  Atomic displacement in copper stabilizer

         dpacop = fpsdt * fact(5) * wallmw * coef(5,ishmat) *
     +        exp(-decay(5,ishmat) * (dshieq + hecan) )

      else
         dshieq = 0.0D0
         dshoeq = 0.0D0
         hecan = 0.0D0
         wpthk = 0.0D0
         coilhtmx = 0.0D0
         ptfiwp = 0.0D0
         ptfowp = 0.0D0
         htheci = 0.0D0
         pheci = 0.0D0
         pheco = 0.0D0
         ptfi = 0.0D0
         ptfo = 0.0D0
         ptfnuc = 0.0D0
         raddose = 0.0D0
         flumax = 0.0D0
         dpacop = 0.0D0
      end if

c  Divertor mass

      divsur = fdiva * 2.0D0 * pi * rmajor * rminor
      if (idivrt.eq.2) divsur = divsur * 2.0D0
      divmas = divsur * divdens * (1.0D0 - divclfr) * divplt

c  Start adding components of the coolant mass

      coolmass = divsur * divclfr * divplt

c  Blanket and shield volumes and masses

C+**PJK 29/01/96
      if (itart.eq.1) then

         elong = hmax/rtot
         r1 = rsldo - shldoth
         r2 = rsldi + shldith
         volblkto = fvolbo * 1.333D0 * pi * elong *
     +        ( r1**3  - (r1-blnkoth)**3)
         volshldo = fvolso * 1.333D0 * pi * elong * (rsldo**3 - r1**3)

C  Approximate TART inboard shield and blanket volumes by
C  hollow cylinders the same height as the plasma

         volshldi = fvolsi * 2.0D0 * rminor*kappa * pi * 
     +        ( r2**2 - rsldi**2 )
         volblkti = fvolbi * 2.0D0 * rminor*kappa * pi * 
     +        ( (r2+blnkith)**2 - r2**2 )

         volblkt = volblkto + volblkti
         volshld = volshldo + volshldi

      else

         r1 = rsldo - rmajor + rminor
         r2 = r1 - shldoth
         r3 = r2 - blnkoth
         r4 = rsldi + shldith
         ht1 = rminor*kappa + vgap2 + vgap

         if (idivrt.eq.2) then
            hb1 = ht1
         else
            hb1 = ht1-vgap
         end if

         hblnkt = (ht1+hb1)/2.0D0

         volblkto = 1.333D0 * fvolbo * pi**2 * (r2**3 - r3**3)
         volblkti = fvolbi * hblnkt * 2.0D0*pi*((r4+blnkith)**2 - r4**2)
         volshldo = 1.333D0 * fvolso * pi**2 * (r1**3 - r2**3)
         volshldi = fvolsi * hblnkt * 2.0D0*pi*((r4**2 - rsldi**2) +
     +        ((r4+shldith)**2 - r4**2) /2.0D0)
         volshld = volshldi + volshldo
         volblkt = volblkti + volblkto

      end if

c  Blanket - water, stainless steel, Vanadium, Li2O, and Be options
c  ( assume 65% packing fraction for Be )

      whtblss = volblkt * denstl * fblss
      whtblbe = volblkt * 1900.0D0  * fblbe 
      whtblvd = volblkt * 5870.0D0  * fblvd
      wtblli2o = volblkt * 2010.0D0  * fblli2o
      whtblkt = whtblss + whtblvd + wtblli2o + whtblbe

      whtshld = volshld * denstl * (1.0D0 - vfshld)

c+**CAG 20/05/93 call new blanket model (supersedes above calculations)

      if (lblnkt.eq.1) then 
         call blanket(1,nout,iprint)

C+**PJK 10/02/97 Improved approximation for inboard/outboard
C+**PJK 10/02/97 blanket volumes: assume cylinders of equal heights
C+**PJK 10/02/97    volblkti = volblkt*blnkith/(blnkith+blnkoth)
C+**PJK 10/02/97    volblkto = volblkt*blnkoth/(blnkith+blnkoth)

         r1 = rsldi + shldith + 0.5D0*blnkith
         r2 = rsldo - shldoth - 0.5D0*blnkoth
         volblkti = volblkt * (r1*blnkith)/((r1*blnkith)+(r2*blnkoth))
         volblkto = volblkt * (r2*blnkoth)/((r1*blnkith)+(r2*blnkoth))

      end if   

c  Penetration shield (set = internal shield)

      wpenshld = whtshld
      coolmass = coolmass + volblkt*vfblkt + volshld*vfshld

c  First wall area
C+**PJK 12/11/93 Calculation moved to routine THRMAL
C+**PJK 12/11/93  fwarea = 4.0D0 * pi**2 * sf * rmajor * 
C+**PJK 12/11/93 +     (rminor+(scrapli+scraplo)/2.0D0) * 0.875D0

c  First wall mass

      fwmass = fwarea * (fwith+fwoth)/2.0D0 * denstl * (1.0D0-fwclfr)

c  Cryostat mass

      cryomass = fvolcry * 4.0D0 * (2.0D0*(rtot-rsldi) + 2.0D0*hmax) *
     +     2.0D0 * pi * rmajor * ddwi * denstl

c  Surface areas adjacent to plasma

      coolmass = coolmass + fwarea * (fwith+fwoth)/2.0D0 * fwclfr

c  Mass of coolant = volume * density

C+**PJK 27/04/94 Old blanket model can now have either type of coolant

c+**CAG 20/05/93 The densities have been calculated 
c+**CAG 20/05/93 at typical coolant temperatures and pressures.

      if (costr.eq.1) coolmass = coolmass*1.517d0
      if (costr.eq.2) coolmass = coolmass*806.719d0

c  Neutron wall loading (MW/m2)
C+**PJK 11/01/93 This wall load calculation is obsolete, although
C+**PJK 11/01/93 it is conceptually better than the other formula
C+**PJK 11/01/93 in routine PHYSICS which has a fiddle factor to
C+**PJK 11/01/93 convert between plasma and first wall areas.
C+**PJK 11/01/93 wallmw = (14.1D0/17.6D0) * powfmw/fwarea

c  Dewar volumes and mass

C+**PJK 06/10/92 Error found : no such variable as ddwex, thickness
C+**PJK 06/10/92 of external dewar. This has now been added to build.h
C+**PJK 06/10/92 and routines input and initial.

C+**PJK 25/07/11 (ht1+hb1) replaced by 2.0D0*hmax in two places

      rdewex = rtot + tfthko/2.0D0 + 2.0D0
      vdewex = ( (2.0D0*pi*rdewex) * (2.0D0*hmax + tfcth + 5.0D0) +
     +     (2.0D0*pi*rdewex**2) ) * ddwex

c  Factor of 2 to account for outside part of TF coil.
c  fvoldw accounts for ports, support, etc. additions.

      vdewin = (2.0D0*(2.0D0*hmax) + 2.0D0 * (rsldo-rsldi)) *
     +     2.0D0 * pi * rmajor * ddwi * 2.0D0 * fvoldw
      dewmkg = (vdewin + vdewex) * denstl

      if ((iprint.eq.0).or.(sect12.eq.0)) goto 1000

c  Output section

      call oheadr(nout,'Shield / Blanket')
      call ovarre(nout,'Average neutron wall load (MW)','(wallmw)',
     +     wallmw)
      call ovarre(nout,'DT full power TF coil operation (yrs)',
     +     '(fpydt)',fpydt)
      call ovarre(nout,'Inner shield thickness (m)','(shldith)',shldith)
      call ovarre(nout,'Outer shield thickness (m)','(shldoth)',shldoth)
      call ovarre(nout,'Inner blanket thickness (m)','(blnkith)',
     +     blnkith)
      call ovarre(nout,'Outer blanket thickness (m)','(blnkoth)',
     +     blnkoth)
      call ovarre(nout,'Inner side TF coil case thickness (m)',
     +     '(hecan)',hecan)

C+**PJK 29/01/96
      if (itart.eq.1) then
         call osubhd(nout,'(Copper centrepost used)')
         call ovarre(nout,'Centrepost heating (MW)','(pnuccp)',pnuccp)
      else
         call osubhd(nout,'TF coil nuclear parameters :')
         call ovarre(nout,'Peak magnet heating (MW/m3)','(coilhtmx)',
     +        coilhtmx)
         call ovarre(nout,'Inner TF coil winding pack heating (MW)',
     +        '(ptfiwp)',ptfiwp)
         call ovarre(nout,'Outer TF coil winding pack heating (MW)',
     +        '(ptfowp)',ptfowp)
         call ovarre(nout,'Peak He can heating (MW/m3)','(htheci)',
     +        htheci)
         call ovarre(nout,'Inner He can heating (MW)','(pheci)',pheci)
         call ovarre(nout,'Outer He can heating (MW)','(pheco)',pheco)
         call ovarre(nout,'Insulator dose (rad)','(raddose)',raddose)
         call ovarre(nout,'Maximum neutron fluence (n/m2)','(flumax)',
     +        flumax)
         call ovarre(nout,'Copper stabiliser displacements/atom',
     +        '(dpacop)',dpacop)
      end if

      call osubhd(nout,'Nuclear heating :')
      call ovarre(nout,'Blanket heating (MW)','(pnucblkt)',pnucblkt)
      call ovarre(nout,'Shield heating (MW)','(pnucshld)',pnucshld)

      call osubhd(nout,'Blanket / shield volumes and weights :')

c+** CAG 20/05/93 Alteration to output of blanket volumes/weights

      if (lblnkt.eq.1) then

         if (smstr.eq.1) write(nout,600) volblkti, volblkto, volblkt, 
     +        whtblkt, vfblkt, fblbe, whtblbe, fblli2o, wtblli2o, 
     +        fblss, whtblss, fblvd, whtblvd, volshldi, volshldo, 
     +        volshld, whtshld, vfshld, wpenshld

         if (smstr.eq.2) write(nout,601) volblkti, volblkto, volblkt, 
     +        whtblkt, vfblkt, fbllipb, wtbllipb, fblli, whtblli, 
     +        fblss, whtblss, fblvd, whtblvd, volshldi, volshldo, 
     +        volshld, whtshld, vfshld, wpenshld

      else

         write(nout,600) volblkti, volblkto, volblkt, whtblkt, vfblkt,
     +        fblbe, whtblbe, fblli2o, wtblli2o, fblss, whtblss, fblvd,
     +        whtblvd, volshldi, volshldo, volshld, whtshld, vfshld,
     +        wpenshld

      end if

 600  format(
     +     t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/
     +     t32,'-----------',t45,'------------',t62,'-----------'/
     +     '    Inner blanket' ,t32,1pe10.3,/
     +     '    Outer blanket' ,t32,1pe10.3,/
     +     '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/
     +     '       Void fraction' ,t45,1pe10.3,/
     +     '       Blanket Be   ',t45,1pe10.3,t62,1pe10.3/
     +     '       Blanket Li2O ',t45,1pe10.3,t62,1pe10.3/
     +     '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/
     +     '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/
     +     '    Inner shield'  ,t32,1pe10.3,/
     +     '    Outer shield'  ,t32,1pe10.3,/
     +     '    Primary shield',t32,1pe10.3,t62,1pe10.3/
     +     '       Void fraction' ,t45,1pe10.3,/
     +     '    Penetration shield'        ,t62,1pe10.3)

 601  format(
     +     t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/
     +     t32,'-----------',t45,'------------',t62,'-----------'/
     +     '    Inner blanket' ,t32,1pe10.3,/
     +     '    Outer blanket' ,t32,1pe10.3,/
     +     '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/
     +     '       Void fraction' ,t45,1pe10.3,/
     +     '       Blanket LiPb ',t45,1pe10.3,t62,1pe10.3/
     +     '       Blanket Li   ',t45,1pe10.3,t62,1pe10.3/
     +     '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/
     +     '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/
     +     '    Inner shield'  ,t32,1pe10.3,/
     +     '    Outer shield'  ,t32,1pe10.3,/
     +     '    Primary shield',t32,1pe10.3,t62,1pe10.3/
     +     '       Void fraction' ,t45,1pe10.3,/
     +     '    Penetration shield'        ,t62,1pe10.3)

      call osubhd(nout,'Other volumes, masses and areas :')
      call ovarre(nout,'First wall area (m2)','(fwarea)',fwarea)
      call ovarre(nout,'First wall mass (kg)','(fwmass)',fwmass)
      call ovarre(nout,'External dewar volume (m3)','(vdewex)',vdewex)
      call ovarre(nout,'External dewar mass (kg)','(dewmkg)',dewmkg)
      call ovarre(nout,'Internal dewar volume (m3)','(vdewin)',vdewin)
      call ovarre(nout,'Cryostat mass (kg)','(cryomass)',cryomass)
      call ovarre(nout,'Divertor area (m2)','(divsur)',divsur)
      call ovarre(nout,'Divertor mass (kg)','(divmas)',divmas)

 1000 continue

      return
      end
