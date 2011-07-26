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
C  Module         : $Id: sctfcoil.f,v 3.8 2006/05/25 09:27:02 pknight Exp pknight $
C
C  Module name    : $RCSfile: sctfcoil.f,v $
C  Version no.    : $Revision: 3.8 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE SCTFCOIL(nout,iprint)

c  This routine performs the TF coil calculations. The primary
c  outputs are coil size, shape, stress, and fields. It is 
c  a variant from the original FEDC/Tokamak systems code.
c
c  Responsible person: John Galambos, FEDC/ORNL, 615-576-5482.
c
c  Included in PROCESS in February 1992 by P. C. Shipe.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'build.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'fwblsh.h'

      DOUBLE PRECISION
     +     awpc, awptf, bcylir, dct, leni, leno, radwp, rbcndut, rcoil,
     +     rcoilp, tant, tcan1, tcan2, tcan3, thtcoil, wbtf

      integer i,iprint,narc,nout

c  Determine layout of the inboard midplane TF coil leg

c  Cross-sectional area of outboard leg

C+**PJK 04/11/92 Possible error: tfcth should be tfthko.
C+**PJK 11/03/93 This error has now been fixed.
C+**PJK 11/03/93 Unclear about algorithm used, however.

C+**PJK 25/07/11 New simpler algorithm (rectangular leg, aspect ratio = 2)
C+**PJK 25/07/11 replaces original opaque algorithm...
C+**PJK 25/07/11   arealeg = 4.d0 * tfthko * (rtot-tfthko/2.d0) *sin(pi/tfno)

      arealeg = 0.5D0 * tfthko*tfthko

c  Radius of centre of inboard TF coil leg

C+**PJK 21/04/93 This calculation can lead to negative values for
C+**PJK 21/04/93 the TF coil radial parameters later in the code.
C+**PJK 21/04/93 The new coding method prevents this.
C+**PJK 21/04/93      rtfcin = rsldi - gapds - ddwi - tfcth/2.d0

      if (itart.eq.1) then
         rtfcin = bore + bcylth + tfcth/2.0D0
      else
         rtfcin = bore + ohcth + gapoh + bcylth + tfcth/2.0D0
      end if

c  Radius of outer edge of inboard leg
      rcoil = rtfcin + tfcth/2.d0

c  Radius of inner edge of inboard leg
      rcoilp = rcoil - tfcth

c  Half toroidal angular extent of a single TF coil inboard leg
      thtcoil = pi/tfno
      tant = tan(thtcoil)

c  Circle/trapezoid distance
      dct = 0.d0

c  Annular area of midplane containing TF coil inboard legs
      tfareain = pi * (rcoil**2 - rcoilp**2) 

c  Total current in TF coils
      ritfc = oacdcp * tfareain
      if (ritfc.lt.0.0D0) then
         write(*,*) 'Error in routine SCTFCOIL:'
         write(*,*) 'TF coil current is negative, ritfc = ',ritfc
         write(*,*) '   Overall current density, oacdcp = ',oacdcp
         write(*,*) '    Cross-sectional area, tfareain = ',tfareain
         write(*,*) ' '
         write(*,*) ' PROCESS continuing, ritfc forced to be +ve...'
         ritfc = 1.0D0
      end if

c  Peak toroidal field and radius of its occurrence
      rbmax = rcoil
      bmaxtf = ritfc * rmu0 / (2.d0*pi*rbmax)

c  Peak field including ripple
      bmaxtfrp = bmaxtf * 1.09d0

c  Calculation of forces : centering and vertical
      cforce = bmaxtf*ritfc/(2.d0*tfno)
      vforce = 0.5d0 * bt * rmajor * ritfc/2.d0 *
     +     log(rtot/rtfcin) / tfno

c  Horizontal and vertical bores
      tfboreh = rtot - rtfcin - tfthko/2.d0 - tfcth/2.d0
      borev = 2.d0 * hmax

c  The rest of this routine deals with superconducting coils.

c  Define coil shape
      call coilshap

c  Calculation of TF coil magnetic energy - by Kalsi
      narc = 4
      call tfcind(narc,xarc,yarc,xctfc,yctfc,dthet,tfcth,tfind)

c  Find TF coil energy (GJ)
      estotf = 1.d-9 *  tfind/2.d0 / tfno * ritfc**2

c  Coil inside perimeter
      tfleng = 0.d0
      do 10 i = 1,4
         tfleng = tfleng + 2.d0*(radctf(i) + tfcth/2.d0) * dthet(i)
 10   continue

c  Case thicknesses
      if (itfmod .ne. 1) thkcas = tfcth * 0.5d0

c  Case thickness of side nearest torus centreline
      tcan1 = thkcas

c  Case thickness of side nearest plasma
      tcan2 = casthi

c  Case thickness of side wall
      tcan3 = casths

c  Winding pack dimensions (each leg)

c  Radial extent
      thkwp = tfcth - dct - tcan2 - tcan1 - 2.d0*tinstf

      if (thkwp.le.0.0D0) then
         write(*,*) 'Error in routine SCTFCOIL:'
         write(*,*) 'Winding pack thickness thkwp = ',thkwp
         write(*,*) 'Reduce case or insulation thicknesses,'
         write(*,*) 'or increase tfcth value or lower bound.'
         write(*,*) ' '
c         write(*,*) 'PROCESS stopping.'
c         goto 20
      end if

c  Radius of geometrical centre of winding pack
      radwp = rcoil - dct - tcan2 - tinstf - thkwp/2.d0

c  Thickness of winding pack section at R > radwp
      wwp1 = 2.d0 * ( radwp*tant - tcan3 - tinstf )

c  Thickness of winding pack section at R < radwp
      wwp2 = 2.d0 * ( (radwp-thkwp/2.d0)*tant - tcan3 - tinstf )

c  Total cross-sectional area of winding pack
      awptf = (thkwp/2.d0)*(wwp1 + wwp2)

c  Total cross-sectional area of winding pack, including insulation
      awpc = (wwp1 + 2.d0*tinstf) * (thkwp + 2.d0*tinstf)

c  Total cross-sectional area of surrounding case
      acasetf = (tfareain/tfno) - awpc

      if ((awptf.le.0.0D0).or.(awpc.le.0.0D0).or.(acasetf.le.0.0D0))
     +     then
         write(*,*) 'Error in routine SCTFCOIL:'
         write(*,*) 'Winding pack cross-section problem'
         write(*,*) 'awptf = ',awptf
         write(*,*) 'awpc = ',awpc
         write(*,*) 'acasetf = ',acasetf
         write(*,*) ' '
c         write(*,*) 'PROCESS stopping.'
c         goto 20
      end if

c  Winding pack current density
C+**PJK 05/02/96 Added wpvf term
C+**PJK 25/05/06 Ensured jwptf is positive
      jwptf = MAX(1.0D0, ritfc/(tfno*awptf*(1.0D0-wpvf)) )

c  Superconducting cable information
c  (number of turns not required to be an integer here for numerics)

c  Radius of rounded corners of cable space inside conduit
      rbcndut = thwcndut * 0.75d0

c  Dimension of square cross-section of each turn
      leno = sqrt(cpttf / jwptf)

c  Dimension of square cable space inside insulation and case of
c  the conduit of each turn
      leni = leno - 2.d0 * (thwcndut + thicndut)

      if (leni.le.0.0D0) then
         write(*,*) 'Error in routine SCTFCOIL:'
         write(*,*) 'Cable space dimension, leni = ',leni
         write(*,*) 'Reduce conduit case or insulation thicknesses,'
         write(*,*) 'or increase cpttf value or lower bound.'
         write(*,*) ' '
c         write(*,*) 'PROCESS stopping.'
c         goto 20
      end if

c  Cross-sectional area of cable space per turn
      acstf = sign(1.0D0,leni) * ( leni**2 - (4.d0-pi)*rbcndut**2 )

      if (acstf.le.0.0D0) then
         if (leni.lt.0.0D0) then
            write(*,*) 'Warning in routine SCTFCOIL:'
            write(*,*) '    Cable space area, acstf = ',acstf
            write(*,*) 'Cable space dimension, leni = ',leni
            write(*,*) ' '
         else
            write(*,*) 'Warning in routine SCTFCOIL:'
            write(*,*) '    Cable space area, acstf = ',acstf
            write(*,*) 'Cable space dimension, leni = ',leni
            write(*,*) 'Artificially set rounded corner radius to zero'
            write(*,*) ' '
            rbcndut = 0.0D0
            acstf = leni**2
         end if
      end if

c  Cross-sectional area of conduit case per turn
      acndttf = (leni + 2.d0*thwcndut)**2 - acstf

c  Total number of turns per TF coil
      turnstf = ritfc / (cpttf * tfno)

c  Number of turns per pancake (NEVER USED)
      rnltf = thkwp / (leno*sqrt(aspcstf))

c  Total conductor cross-sectional area, taking account of void area
      acond = acstf * turnstf * (1.d0 - vftf)

c  Void area in cable, for He
      avwp = acstf * turnstf * vftf

c  Insulation area (not including ground-wall)
      aiwp = turnstf * (leno**2 - acndttf - acstf)

c  Structure area for cable
      aswp = (turnstf*acndttf)

c  TF Coil areas and masses

c  Surface areas (for cryo system)
      wbtf = (rcoil*sin(thtcoil)-rcoilp*tant)
      tfocrn = rcoilp * tant
      tficrn = tfocrn + wbtf
      tfsai = 4.d0 * tfno * tficrn * hr1
      tfsao = 2.d0 * tfno * tficrn * (tfleng - 2.d0*hr1)

c  Mass of case : add correction factor to maintain enough
c  structure when the inboard case gets small
c  (normalized to ITER 9/19/90)

      whtcas = casfact * tfleng * acasetf * dcase *
     +     sqrt( 0.41d0 * tfareain/(acasetf *tfno) )

C+**PJK 12/01/94 Split conductor mass calculation into constituents

c  Masses of conductor constituents:

c+**      whtcon = tfleng*turnstf*( acstf*(1.d0-vftf)*(fcutfsu*8900.d0
c+**     +     +(1.d0-fcutfsu)*dcond(isumat)) + acndttf*dcase)

c  Superconductor
      whtconsc = tfleng * turnstf * acstf*(1.0D0-vftf) *
     +     (1.0D0-fcutfsu)*dcond(isumattf)

c  Copper
      whtconcu = tfleng * turnstf * acstf*(1.0D0-vftf) *
     +     fcutfsu*dcopper

c  Steel conduit (sheath)
      whtconsh = tfleng * turnstf * acndttf * denstl

c  Total conductor mass
      whtcon = whtconsc + whtconcu + whtconsh

c  Bucking cylinder
      bcylir = rcoilp - bcylth
      wtbc = pi *( (bcylir+bcylth)**2 - bcylir**2) *hr1*2.d0*7800.d0

      whttf = (whtcas+whtcon) * tfno

c  Do stress calculations
      call stresscl

      if (iprint.eq.1) call outtf(nout)

      goto 1000

c  Print out diagnostics if an error occurred
 20   continue

      write(*,*) '           tfcth = ',tfcth
      write(*,*) 'thkcas (= tcan1) = ',thkcas
      write(*,*) 'casthi (= tcan2) = ',casthi
      write(*,*) 'casths (= tcan3) = ',casths
      write(*,*) '          tinstf = ',tinstf
      write(*,*) '        thwcndut = ',thwcndut
      write(*,*) '        thicndut = ',thicndut
      write(*,*) '           cpttf = ',cpttf

      STOP

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE STRESSCL

c  Routine to set up and call the TF coil stress routine

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'build.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'

      DOUBLE PRECISION dummyv,eyngeff,seff,sigvm,svmxz,svmyz,tcbs
      integer i,itfst,tfstress

      external eyngeff,tfstress,sigvm

c  Allowable stress
      alstrtf = min ( (2.d0*csytf/3.d0), (csutf/2.d0) )

c  Simple stress model option

      if (itfmod .ne. 1 ) then
         call sctfjalw(bmaxtfrp,rtfcin,rtot,rbmax,(1.d-6*alstrtf),
     +        tdmptf,jwdgcrt)
         goto 1000
      end if

c  Set up graded stress model call information

      seff = sqrt(cpttf/jwptf)
C+**PJK 24/05/06 Prevented sqrt(negative)
      if (acstf.ge.0.0D0) then
         tcbs = sqrt (acstf)
      else
         tcbs = 0.0D0
      end if
      radtf(1) = rbmax

      do 10 i = 1,3
         eyoung(i) = eyngeff(eystl,eyins,eywp,thicndut,seff,
     +        thwcndut,tcbs)
         radtf(i+1) = radtf(i) - thkwp/3.d0
         dummyv = max(0.001d0, (1.d0 - casths*tfno/(pi*radtf(i)) ) )
         jeff(i) = jwptf * dummyv
 10   continue   

c  Outer ring section
      do 20 i = 4,5
         radtf(i+1) = rtfcin - tfcth/2.d0
         eyoung(i) = eystl
         jeff(i) = 0.d0
 20   continue  

c  Call stress routine
      itfst = tfstress(poisson,radtf,eyoung,jeff,sigrtf,sigttf,deflect)

      if (itfst.ne.1) then
         write(*,*) 'Error in routine STRESSCL:'
         write(*,*) 'TFSTRESS returned with value ',itfst
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

c  Convert to conduit + case
      sigrcon = sigrtf(3)/eyoung(3) * eystl*eyins*seff /
     +     (eyins*(seff-2.d0*thicndut) + 2.d0*thicndut*eystl)
      sigtcon = sigttf(3)/eyoung(3) * eystl*eyins*seff /
     +     (eyins*(seff-2.d0*thicndut) + 2.d0*thicndut*eystl)
      sigvert = vforce / (acasetf + acndttf*turnstf)

c  Find case strain
      casestr = sigvert / eystl

c  Find Von-Mises combinations in case
      strtf2 = sigvm(sigrtf(5), sigttf(5), sigvert, 0.d0,0.d0,0.d0 )

c  Find Von-Mises combinations in conduit walls (take worse
c  of 2 walls)
      svmxz = sigvm(sigrcon, 0.d0, sigvert, 0.d0, 0.d0, 0.d0)
      svmyz = sigvm(0.d0, sigtcon, sigvert, 0.d0, 0.d0, 0.d0)
      strtf1 = max(svmxz,svmyz)

 1000 continue

      return
      end
c______________________________________________________________________
      DOUBLE PRECISION FUNCTION EYNGEFF(estl,eins,ewp,tins,
     +     teff,tstl,tcs)

c  Function to find the effective Young's modulus of the TF coil
c  in the winding pack section. Programmed following J. Miller's
c  Lotus spreadsheet method 5/9/91, J. Galambos
c
c  INPUT :
c  estl - Young's modulus of steel (Pa)
c  eins - Young's modulus of insulator (Pa)
c  ewp  - Young's modulus of windings (Pa)
c  tins - insulator wrap thickness (m)
c  teff - dimension of total cable with insulator (m)
c  tstl - thickness of steel conduit (m)
c  tcs  - dimension of cable space area inside conduit (m)
c
c  OUTPUT :
c  eyngeff - effective Young's modulus (Pa)

      IMPLICIT NONE

      DOUBLE PRECISION estl,eins,ewp,tins,teff,tstl,tcs,tcond

      tcond = teff - 2.d0 * tins

      eyngeff = 2.d0*eins*tins/teff + 2.d0*eins*estl*tstl /
     +     (eins*tcond+2.d0*tins*estl) + estl*eins*ewp*tcs /
     +     (estl*eins*tcs + 2.d0*tins*eins*estl + 2.d0*tins*estl*ewp)

      return
      end
c______________________________________________________________________
      SUBROUTINE OUTTF(nout)

c  TF Coil output

      IMPLICIT NONE

      INCLUDE 'build.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'osections.h'

      DOUBLE PRECISION ap
      INTEGER nout,i

      if (sect07.eq.0) goto 1000

      call oheadr(nout,'TF Coils')
      call ocmmnt(nout,'Superconducting TF coils')

      if (magnt.eq.2) then
         call osubhd(nout,'Wedged TF Coils, with two-step winding')
      else
         call osubhd(nout,'Bucked TF Coils, with two-step winding')
      end if

      call ocmmnt(nout,'Current Density :')
      call oblnkl(nout)
      call ovarre(nout,'Winding pack current density (A/m2)','(jwptf)',
     +     jwptf)
      call ovarre(nout,'Overall current density (A/m2)','(oacdcp)',
     +     oacdcp)

      if (itfmod.ne.1) then
         call ovarre(nout,'Allowable overall current density (A/m2)',
     +        '(jwdgcrt)',jwdgcrt)
      end if

      call osubhd(nout,'General Coil Parameters :')
      call ovarre(nout,'Area per coil (m2)','(tfarea/tfno)',
     +     tfareain/tfno)
      call ovarre(nout,'Total inner leg radial thickness (m)',
     +     '(tfcth)',tfcth)
      call ovarre(nout,'Inside half-width (m)','(tficrn)',tficrn)
      call ovarre(nout,'Outside half width (m)','(tfocrn)',tfocrn)
      call ovarre(nout,'Total current (MA)','(ritfc/1.D6)',ritfc/1.0D6)
      call ovarre(nout,'Vertical separating force per coil (N)',
     +     '(vforce)',vforce)
      call ovarre(nout,'Centering force per coil (N/m)','(cforce)'
     +     ,cforce)
      call ovarre(nout,'Peak field (Amperes Law,T)','(bmaxtf)',bmaxtf)
      call ovarre(nout,'Peak field (with ripple,T)','(bmaxtfrp)',
     +     bmaxtfrp)
      call ovarre(nout,'Stored energy per coil (GJ)','(estotf)',estotf)
      call ovarre(nout,'Mean coil circumference (m)','(tfleng)',tfleng)
      call ovarre(nout,'Number of TF coils','(tfno)',tfno)
      call ovarre(nout,'Outer coil case thickness (m)','(thkcas)',
     +     thkcas)
      call ovarre(nout,'Outer coil case area (m2)','(acasetf)',acasetf)

      call osubhd(nout,'Coil Geometry :')
      call ovarre(nout,'Inner leg centre radius (m)','(rtfcin)',rtfcin)
      call ovarre(nout,'Outer leg centre radius (m)','(rtot)',rtot)
      call ovarre(nout,'Maximum inner edge height (m)','(hmax)',hmax)
      call ovarre(nout,'Clear bore (m)','(tfboreh)',tfboreh)
      call ovarre(nout,'Clear vertical bore (m)','(borev)',borev)

      call oblnkl(nout)
      call ocmmnt(nout,'TF coil inner surface shape is approximated')
      call ocmmnt(nout,'by arcs between the following points :')
      call oblnkl(nout)

      write(nout,10)
 10   format(t2,'point',t16,'x(m)',t31,'y(m)')

      do 30 i=1,5
         write(nout,20) i,xarc(i),yarc(i)
 20      format(i4,t10,f10.3,t25,f10.3)
 30   continue

      call osubhd(nout,'The centres of the arc are :')
      write(nout,40)
 40   format(t3,'arc',t16,'x(m)',t30,'y(m)')

      do 50 i=1,4
         write(nout,20) i,xctfc(i),yctfc(i)
 50   continue

      call osubhd(nout,'Conductor Information :')
      call ovarre(nout,'Total mass of TF coils (kg)','(whttf)',whttf)
      call ovarre(nout,'Superconductor mass per coil (kg)','(whtconsc)',
     +     whtconsc)
      call ovarre(nout,'Copper mass per coil (kg)','(whtconcu)',
     +     whtconcu)
      call ovarre(nout,'Steel conduit mass per coil (kg)','(whtconsh)',
     +     whtconsh)
      call ovarre(nout,'Total conductor cable mass per coil (kg)',
     +     '(whtcon)',whtcon)
      call ovarre(nout,'External case mass per coil (kg)','(whtcas)',
     +     whtcas)
      call ovarre(nout,'Cable conductor + void area (m2)','(acstf)',
     +     acstf)
      call ovarre(nout,'Conduit case thickness (m)','(thwcndut)',
     +     thwcndut)
      call ovarre(nout,'Cable insulation thickness (m)','(thicndut)',
     +     thicndut)
      call ovarre(nout,'Cable radial/toroidal aspect ratio','(aspcstf)'
     +     ,aspcstf)

      ap = acond + aswp + aiwp + avwp

      call osubhd(nout,'Winding Pack Information :')
      call ovarre(nout,'Conductor fraction of winding pack','(acond/ap)'
     +     ,acond/ap)
      call ovarre(nout,'Copper fraction of conductor','(fcutfsu)',
     +     fcutfsu)
      call ovarre(nout,'Structure fraction of winding pack','(aswp/ap)',
     +     aswp/ap)
      call ovarre(nout,'Insulator fraction of winding pack','(aiwp/ap)',
     +     aiwp/ap)
      call ovarre(nout,'Helium fraction of winding pack','(avwp/ap)',
     +     avwp/ap)
      call ovarre(nout,'Winding thickness (m)','(thkwp)',thkwp)
      call ovarre(nout,'Winding width 1 (m)','(wwp1)',wwp1)
      call ovarre(nout,'Winding width 2 (m)','(wwp2)',wwp2)
      call ovarre(nout,'Number of turns per TF coil','(turnstf)',
     +     turnstf)
      call ovarre(nout,'Current per turn (A)','(cpttf)',cpttf)
      
      call osubhd(nout,'TF Coil Stresses :')
      call ovarre(nout,'Vertical stress (Pa)','(sigvert)',sigvert)
      call ovarre(nout,'Conduit radial stress (Pa)','(sigrcon)',sigrcon)
      call ovarre(nout,'Conduit tangential stress (Pa)','(sigtcon)',
     +     sigtcon)
      call ovarre(nout,'Conduit Von Mises combination stress (Pa)',
     +     '(strtf1)',strtf1)
      call ovarre(nout,'Case radial stress (Pa)','(sigrtf(5))',
     +     sigrtf(5))
      call ovarre(nout,'Case tangential stress (Pa)','(sigttf(5))',
     +     sigttf(5))
      call ovarre(nout,'Case Von Mises combination stress (Pa)',
     +     '(strtf2)',strtf2)
      call ovarre(nout,'Allowable stress (Pa)','(alstrtf)',alstrtf)
      call ovarre(nout,'Deflection at midplane (m)','(deflect)',deflect)

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE COILSHAP

c  Calculates the TF coil shape. The coil is approximated by 4 arcs
c  along the edge facing the plasma.
c  Start with the inboard arc. (redone to fit ITER Physics
c  phase 3/30/89)

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'build.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'

      double precision thet2, thet3, thet4

C  Point on inboard midplane

      xarc(1) = rtfcin + tfcth/2.d0
      yarc(1) = 0.d0

c  Point at top of coil

      xarc(3) = rmajor - 0.2d0*rminor
      yarc(3) = hmax

c  Point at top of straight section

      xarc(2) = xarc(1) + 0.07d0*rminor*kappa
      yarc(2) = yarc(3) * 0.7d0

c  Point at outboard side

      xarc(5) = rtot - tfthko/2.d0
      yarc(5) = 0.0D0

c  Point no.4

      xarc(4) = xarc(3) + farc4tf*(xarc(5) - xarc(3))
      yarc(4) = 0.72d0 * yarc(3)

c  Find arc centres

      yctfc(4) = 0.d0
      xctfc(4) = (xarc(5)**2 - xarc(4)**2 - yarc(4)**2)/2.d0
     +     /(xarc(5) - xarc(4))
      thet4 = atan2(yarc(4) , (xarc(4)-xctfc(4)) )
      dthet(4) = abs(thet4)
      radctf(4) = sqrt( (yarc(4)-yctfc(4))**2 + (xarc(4)-xctfc(4))**2)

      xctfc(3) =(2.d0*(yarc(4) - yarc(3))* (yarc(4)-tan(thet4)*xarc(4))
     +     + xarc(3)**2 + yarc(3)**2 - xarc(4)**2 - yarc(4)**2 ) /
     +     2.d0/( (xarc(3) -xarc(4)) - (yarc(4) - yarc(3))*tan(thet4) )
      yctfc(3) = yarc(4) - tan(thet4) * (xarc(4) - xctfc(3))
      thet3 = atan2( (yarc(3)-yctfc(3)), (xarc(3) - xctfc(3)))
      dthet(3) = abs(thet3 - thet4)
      radctf(3) = sqrt( (yarc(3)-yctfc(3))**2 + (xarc(3)-xctfc(3))**2)

      xctfc(2) =(2.d0*(yarc(3) - yarc(2))*(yarc(3)-tan(thet3)*xarc(3))
     +     + xarc(2)**2 + yarc(2)**2 - xarc(3)**2 - yarc(3)**2) /
     +     2.d0/ ( (xarc(2) - xarc(3)) - (yarc(3) - yarc(2))*tan(thet3))
      yctfc(2) = yarc(3) - tan(thet3) * (xarc(3) - xctfc(2))
      thet2 = atan2( (yarc(2) - yctfc(2)), (xarc(2) - xctfc(2)) )
      dthet(2) = abs(abs(thet2) - thet3)
      radctf(2) = sqrt( (yarc(2)-yctfc(2))**2 + (xarc(2)-xctfc(2))**2 )

      xctfc(1) = ( xarc(2)**2 - xarc(1)**2 + yarc(2)**2)/
     +     (2.d0*(xarc(2)-xarc(1)))
      yctfc(1) = 0.d0
      radctf(1) = xctfc(1) - xarc(1)
      dthet(1) = atan2(yarc(2), (xctfc(1)-xarc(1)))

C  half-height of TF coil inboard leg straight section
      hr1 =yarc(2)

      return
      end
c______________________________________________________________________
      SUBROUTINE TFCIND(narc,xarc,yarc,xctfc,yctfc,dthet,tfthk,tfind)

c  This subroutine calculates the self inductance of a TF coil that
c  is simulated by a number of arcs.
c  Developed by S.S.Kalsi of FEDC on September 3,1985
c  Modified by J Galambos 1-27-88 to use arcs whose centres
c  don't have to lie on the radius of the adjacent arc.
c
c  INPUT :
c  narc         - Number of arcs used for describing a TF coil
c  xctfc, yctfc - X,Y coordinates of centre of each arc (m)
c  xarc, yarc   - X,Y coordinates of starting point of each arc (m)
c  dthet        - angle subtended by each arc (rad)
c  tfthk        - TF coil thickness (m)
c
c  OUTPUT :
c  tfind        - coil inductance (H)
c
c  Note: arcs start on the outboard side and go counter-clockwise
c  in Kalsi notation. Top/bottom symmetry is assumed

      IMPLICIT NONE

      DOUBLE PRECISION ang(6),dtht(6), xc(6),yc(6),xs(6),ys(6)
      DOUBLE PRECISION xarc(5),yarc(5),xctfc(4),yctfc(4),dthet(4)
      DOUBLE PRECISION al, amu, ax, ay, b, deltht, dr, h, hstar,
     +     pi, r, rc, rbore, t, tfind, tfthk, theta
      INTEGER npnt(6), ntot, ns, narc
      INTEGER i, k, n

      ntot = 100
      amu = 1.25664d-6
      pi = 3.14159d0

c  Convert to Kalsi notation
      ns = narc
      do 10 i = 1,ns
         dtht(i) = dthet(ns + 1 - i)
         xs(i) = xarc(ns + 2 - i)
         ys(i) = yarc(ns + 2 - i)
         xc(i) = xctfc(ns + 1 - i)
         yc(i) = yctfc(ns + 1 - i)
 10   continue

C+PJK 24/05/06 Added definition of xs(ns+1)...
      xs(ns+1) = xs(ns)

      rbore = xs(1) - xs(ns)
      tfind = 0.d0

      do 20 n = 1,ns
         npnt(n) = int(dble(ntot) * (xs(n)-xs(n+1))/rbore)
         ax = xs(n)-xc(n)
         ay = ys(n)-yc(n)
         ang(n) = atan2(ay,ax)
 20   continue
      npnt(ns) = 3

      do 40 k = 1,ns
         deltht = dtht(k)/dble(npnt(k))
         t = ang(k) - deltht/2.d0
         rc = sqrt( (xc(k)-xs(k))**2 + (yc(k)-ys(k))**2 )
         do 30 i = 1,npnt(k)
            theta = t + dble(i)*deltht
            r = xc(k)+rc*cos(theta)
            b = amu/(2.d0*pi*r)
            dr = rc*(cos(theta-deltht/2.d0)-cos(theta+deltht/2.d0))
            h = yc(k)+rc*sin(theta)

c  Assume B in TF coil = 1/2  B in bore
            hstar = tfthk/sin(theta)
            al = b*dr*( 2.d0*h + hstar)
            tfind = tfind+al
 30      continue
 40   continue

c  Add contribution in TF coil inner leg

      tfind = tfind + b*tfthk*ys(ns)

      return
      end
c______________________________________________________________________
      SUBROUTINE SCTFJALW (bmaxtf,rtfmi,rtfmo,rtf2,sigmatf,tdump,
     +     jtfalw)

c  Simple J(B) model for the TF Coil
c  Coded by J. Galambos, from algorithms from J. Perkins, 1/25/91
c
c  INPUT :
c  bmaxtf  - peak field including ripple (T)
c  rtfmi   - mean inner leg radius (m)
c  rtfmo   - mean outer leg radius (m)
c  rtf2    - radius of inner leg point nearest plasma (m)
c  sigmatf - allowable structure stress (MPa)
c  tdump   - dump time (s)
c
c  OUTPUT :
c  jtfalw  - overall allowable current density (A/m2)
c
c  FIXED DATA :
c  tdumprf - reference dump time (s)

      IMPLICIT NONE

      DOUBLE PRECISION bmaxtf, rtfmi, rtfmo, rtf2, sigmatf, 
     +     sqrtdmp, tdump, tdumprf,temp1,temp2,temp3, jtfalw

      tdumprf = 10.d0

      sqrtdmp = sqrt(tdump/tdumprf)
      temp1 = 125.94d0*bmaxtf*rtf2*log(rtfmo/rtfmi)/sigmatf
      temp2 = 0.036d0*sqrt(bmaxtf) / (1.d0-bmaxtf/23.d0)**2
      temp3 = 0.6d0 / (1.d0 - (1.d0 / (16.d0 * (1.d0
     +     - bmaxtf/23.d0)-5.d0) ) )
      jtfalw = 152.d6 / (temp1+temp2*temp3+sqrtdmp)

      return
      end
c_______________________________________________________________________
      INTEGER FUNCTION TFSTRESS(nu,rad,ey,j,sigr,sigt,deflect)

c  Function to calculate the stresses in a superconductor TF coil
c  inboard leg at midplane. The analysis of J. Myall (Aug. 1987) is
c  followed (obtained from J. Miller of LLNL  5/91). This model allows
c  5 regions in the coil, so graded conductors are possible. Regions
c  1-3 are the winding pack regions (going from high to low field),
c  and regions 4-5 are the solid steel ring and intermittent steel ring
c  regions, respectively. A conventional nongraded ring can be modelled
c  by inputting the same values of ey and j for regions 1-3, and
c  regions 4-5.
c  Coded by J. Galambos, FEDC/ORNL, 615-576-5482, 5/9/91.
c
c  INPUT : (all real)
c  nu       - Poisson's ratio (assumed constant over entire coil)
c  rad(i)   - radius points of regions (region i is bounded by
c             rad(i) and rad(i+1) ) (m)
c  ey(i)    - effective Young's modulus of region i (Pa)
c  j(i)     - effective current density of region i (A/m2)
c
c  OUTPUT :
c  sigr(i)  - radial stress in region i (Pa)
c  sigt(i)  - tangential stress in region i (Pa)
c  deflect  - deflection at point rad(6) (m)
c  tfstress - 1 for successful call, 0 for failure

      IMPLICIT NONE

      DOUBLE PRECISION nu, rad(6), ey(5), j(5), sigr(5), 
     +     sigt(5), deflect

      DOUBLE PRECISION  k2,k3,l4,l5,m6,m7,n8,n9, p1,p2,p3, 
     +     q2,q3,q4,q5,r4,r5,r6,r7,s6,s7,s8,s9, t8,t9,t10, 
     +     mu0, nufac, kk1, kk2
      DOUBLE PRECISION b07,b05,b03
      DOUBLE PRECISION a(10,10), b(10), c(10)

      INTEGER i, ii, ia, ipvt(10)

      tfstress = 0

      mu0 = 1.25663706d-6

c  Start to set up LHS matrix A elements (Myall notation)

      nufac = (1.d0 - nu)/(1.d0 + nu)
      t10 = -nufac / rad(6)**2
      n9 = -ey(5)/ey(4)
      n8 = -1.d0
      s9 = -nufac  / rad(5)**2
      s8 = 1.d0/rad(5)**2
      t9 = n9 * s9
      t8 = -s8
      m7 = -ey(4)/ey(3)
      m6 = -1.d0
      r7 = -nufac / rad(4)**2
      r6 = 1.d0/rad(4)**2
      s7 = m7*r7
      s6 = -r6
      l5 = -ey(3) / ey(2)
      l4 = -1.d0
      q5 = -nufac / rad(3)**2
      q4 = 1.d0/rad(3)**2
      r5 = l5 * q5
      r4 = -q4
      k3 = -ey(2) / ey(1)
      k2 = -1.d0
      p3 = -nufac / rad(2)**2
      p2 = 1.d0/rad(2)**2
      q3 = k3 * p3
      q2 = -p2
      p1 = -nufac / rad(1)**2

c  RHS vector b :

      b(10) = 0.d0
      b(9) = 0.d0
      b(8) = 0.d0

      b07 = mu0 * (1.d0-nu**2) * j(3)**2 * rad(4)**2/(16.d0 * ey(3) )
      b(7) = b07 * (4.d0 * log(rad(4)) + nufac )

      b(6) = b07 * (4.d0 * log(rad(4)) - 1.d0)

      b05 = mu0 * (1.d0-nu**2) * j(2)**2 * rad(3)**2/(16.d0 * ey(2) )
      b(5) = b05 * ( (1.d0 - j(3)/j(2) + j(3)/j(2)*
     +     (rad(4)/rad(3))**2 - (j(3)/j(2))**2*
     +     (rad(4)/rad(3))**2 ) * (4.d0*log(rad(3)) 
     +     + 4.d0/(1.d0+nu) ) -
     +     ( 1.d0-( j(3)/j(2) )**2 ) * (3.d0+nu)/(1.d0+nu)   )

      b(4) = b05 * (  ( 1.d0 - j(3)/j(2) + j(3)/j(2) *
     +     (rad(4)/rad(3))**2 - (j(3)/j(2))**2 *
     +     (rad(4)/rad(3))**2*ey(2)/ey(3) )*4.d0*log(rad(3)) -
     +     ( 1.d0 - (j(3)/j(2))**2*ey(2)/ey(3) )  )

      b03 = mu0 * (1.d0 - nu**2) * j(1)**2 * rad(2)**2/ (16.d0*ey(1) )
      b(3) = b03 * (  ( 1.d0 - j(2)/j(1) + (j(2)/j(1) -
     +     j(3)/j(1) ) * (rad(3)/rad(2))**2 + j(3)/j(1) *
     +     (rad(4)/rad(2))**2 - (j(2)/j(1))**2 * (rad(3)/rad(2))**2
     +     + j(3)/j(1)*j(2)/j(1)*(rad(3)/rad(2))**2 -
     +     j(2)/j(1)*j(3)/j(1)*(rad(4)/rad(2))**2 ) *
     +     ( 4.d0*log(rad(2)) + 4.d0/(1.d0+nu) ) 
     +     - (1.d0 - (j(2)/j(1) )**2 ) * (3.d0 + nu)/(1.d0 + nu)    )

      b(2) = b03 * (  ( 1.d0 - j(2)/j(1) + ( j(2)/j(1) - j(3)/j(1) ) *
     +     (rad(3)/rad(2) )**2 + j(3)/j(1)*( rad(4)/rad(2) )**2 -
     +     ey(1)/ey(2)*( j(2)/j(1) )**2 * ( rad(3)/rad(2) )**2  +
     +     ey(1)/ey(2) * j(2)/j(1)* j(3)/j(1) * (rad(3)/rad(2) )**2
     +     - ey(1)/ey(2) * j(2)/j(1) * j(3)/j(1) *(rad(4)/rad(2) )**2
     +     )  * 4.d0*log(rad(2)) 
     +     - ( 1.d0-ey(1)/ey(2)*(j(2)/j(1))**2 ) )

      b(1) = b03 *  ( ( 1.d0 - j(2)/j(1) + ( j(2)/j(1) - j(3)/j(1) ) *
     +     (rad(3)/rad(2) )**2 + j(3)/j(1) * (rad(4)/rad(2) )**2 )
     +     * (4.d0* log(rad(1))  + 4.d0/(1.d0+nu) ) -
     +     (3.d0+nu)/(1.d0+nu) * ( rad(1) / rad(2) )**2  )

c  LHS matrix A :

      do 20 i = 1,10
         do 10 ii = 1,10
            a(i,ii) = 0.d0
 10      continue
 20   continue

      a(1,1) = 1.d0
      a(1,6) = p1
      a(2,1) = 1.d0
      a(2,2) = k2
      a(2,6) = p2
      a(2,7) = q2
      a(3,1) = 1.d0
      a(3,2) = k3
      a(3,6) = p3
      a(3,7) = q3
      a(4,2) = 1.d0
      a(4,3) = l4
      a(4,7) = q4
      a(4,8) = r4
      a(5,2) = 1.d0
      a(5,3) = l5
      a(5,7) = q5
      a(5,8) = r5
      a(6,3) = 1.d0
      a(6,4) = m6
      a(6,8) = r6
      a(6,9) = s6
      a(7,3) = 1.d0
      a(7,4) = m7
      a(7,8) = r7
      a(7,9) = s7
      a(8,4) = 1.d0
      a(8,5) = n8
      a(8,9) = s8
      a(8,10) = t8
      a(9,4) = 1.d0
      a(9,5) = n9
      a(9,9) = s9
      a(9,10) = t9
      a(10,5) = 1.d0
      a(10,10) = t10

c  Find solution vector c, of:  A times c = b

c  Use LINPACK routine here:
      ia = 10
      call linesolv(a,ia,b,ipvt,c)

c  Find stresses in winding pack region

      do 30 i = 1,3
         if (i.eq.1) then
            kk2 = mu0 * (1.d0 - nu**2) * j(i) / (2.d0 * ey(i) ) *
     +           ( (j(1) - j(2) )*(rad(2))**2 + (j(2) - j(3)) *
     +           (rad(3))**2 + j(3)*(rad(4))**2 )
         else if (i.eq.2) then
            kk2 = mu0 * (1.d0 - nu**2) * j(i) / (2.d0 * ey(i) ) *
     +           ( (j(2) - j(3)) * (rad(3))**2 + j(3)*(rad(4))**2 )
         else if (i.eq.3) then
            kk2 = mu0 * (1.d0 - nu**2) * j(i) / (2.d0 * ey(i) ) *
     +           j(3)*(rad(4))**2
         else
c           (cannot possibly get here...)
            continue
         end if

         kk1 = mu0 * (1.d0 - nu**2) * j(i)**2 / (2.d0 * ey(i))

         sigt(i) = ey(i) / (1.d0 - nu**2) * ( (1.d0+nu) * c(i) +
     +        (1.d0-nu) * c(5+i)/(rad(i+1))**2 + 
     +        (1.d0+3.d0*nu)/8.d0*kk1*rad(i+1)**2
     +        - (1.d0+nu)/2.d0*kk2*log(rad(i+1)) - nu*kk2/2.d0 )

         sigr(i) = ey(i) / (1.d0 - nu**2) * ( (1.d0+nu) * c(i) -
     +        (1.d0-nu) * c(5+i)/(rad(i+1))**2 + 
     +        (3.d0+nu)/8.d0*kk1 *(rad(i+1))**2
     +        - (1.d0+nu)/2.d0* kk2*log(rad(i+1)) - kk2/2.d0 )

 30   continue

c  Stress in rings

      do 40 i = 4,5
         sigr(i) = ey(i) / (1.d0 - nu**2) * ( (1.d0+nu)*c(i) -
     +        (1.d0-nu) * c(5+i)/(rad(i+1))**2 )
         sigt(i) = ey(i) / (1.d0 - nu**2) * ( (1.d0+nu)*c(i) +
     +        (1.d0-nu) * c(5+i)/(rad(i+1))**2 )
 40   continue

c  Deflection

      deflect = c(5) * rad(6) + c(10)/rad(6)

      tfstress = 1

      return
      end
c_______________________________________________________________________
      DOUBLE PRECISION FUNCTION SIGVM(sx,sy,sz,txy,txz,tyz)

c  Function SIGVM to calculate Von Mises stress (Pa)
c  Programmed by B. Reimer FEDC, 7/1988
c
c  INPUT :
c  sx    - in-plane stress in X direction (Pa)
c  sy    - in-plane stress in Y direction (Pa)
c  sz    - in-plane stress in Z direction (Pa)
c  txy   - out of plane stress in X-Y plane (Pa)
c  txz   - out of plane stress in X-Z plane (Pa)
c  tyz   - out of plane stress in Y-Z plane (Pa)
c
c  OUTPUT :
c  sigvm - Von Mises combination of stresses (Pa)

      IMPLICIT NONE

      DOUBLE PRECISION sx,sy,sz,txy,txz,tyz

      sigvm = sqrt( ( (sx-sy)**2 + (sx-sz)**2 + (sz-sy)**2
     +     + 6.d0*(txy**2 + txz**2 + tyz**2) ) / 2.d0 )

      return
      end
