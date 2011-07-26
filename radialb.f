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
C  Module name    : $RCSfile: radialb.f,v $
C  Version no.    : $Revision: 3.6 $
C
C  Creation date  : $Date: 1996/06/10 13:20:27 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE RADIALB(iprint,nout)

c  This subroutine determines the radial build of the machine.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'osections.h'

      DOUBLE PRECISION divfix,rtotl,radius,vbuild
      INTEGER iprint,nout

      COMMON/dbld/divfix

c  Radial build to centre of plasma (should be equal to rmajor)
      rbld = bore + ohcth + gapoh + bcylth + tfcth + ddwi +
     +     gapds + shldith + blnkith + fwith + scrapli + rminor

c  Radius to inner edge of inboard shield
      rsldi = rmajor - rminor - scrapli  - fwith - blnkith - shldith

c  Radius to outer edge of outboard shield
      rsldo = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth

c  Thickness of outboard TF coil legs
      tfthko = tfootfi*tfcth 

c  Radius to centre of outboard TF coil legs
      rtot = rsldo + gapomin + ddwi + 0.5d0 * tfthko

c  Check ripple

      call rippl(ripmax,rmajor,rminor,rtot,tfno,ripple,rtotl)

c  If the ripple is too large then move the outer TF coil leg
      if (rtotl .gt. rtot) then
         rtot = rtotl
         gapsto = rtot - rsldo - ddwi - tfthko/2.d0
      else
         gapsto = gapomin
      end if

C+**PJK 10/06/96
C  Calculate first wall area (includes a mysterious factor 0.875...)

      fwarea = 4.0d0*pi**2*sf*rmajor*(rminor+(scrapli+scraplo)/2.d0)
     +     *0.875d0

      if ((iprint.eq.0).or.(sect06.eq.0)) goto 1000

c  Print out device build

      call oheadr(nout,'Radial Build')

      write(nout,10)
 10   format(t43,'Thickness (m)',t60,'Radius (m)')

      radius = 0.d0
      call obuild(nout,'Device centreline',0.0D0,radius)

      radius = radius + bore
      call obuild(nout,'Machine bore',bore,radius)

      if (itart.eq.1) then

C+**PJK 29/01/96 TART inboard radial build

         radius = radius + bcylth
         call obuild(nout,'Bucking cylinder',bcylth,radius)

         radius = radius + tfcth
         call obuild(nout,'TF coil inner leg',tfcth,radius)

         radius = radius + gapoh
         call obuild(nout,'Gap',gapoh,radius)

         radius = radius + ohcth
         call obuild(nout,'OH coil',ohcth,radius)

      else

         radius = radius + ohcth
         call obuild(nout,'OH coil',ohcth,radius)

         radius = radius + gapoh
         call obuild(nout,'Gap',gapoh,radius)

         radius = radius + bcylth
         call obuild(nout,'Bucking cylinder',bcylth,radius)

         radius = radius + tfcth
         call obuild(nout,'TF coil inner leg',tfcth,radius)

      end if

      radius = radius + ddwi
      call obuild(nout,'Vacuum vessel',ddwi,radius)

      radius = radius + gapds
      call obuild(nout,'Gap',gapds,radius)

      radius = radius + shldith
      call obuild(nout,'Inboard shield',shldith,radius)

      radius = radius + blnkith
      call obuild(nout,'Inboard blanket',blnkith,radius)

      radius = radius + fwith
      call obuild(nout,'Inboard first wall',fwith,radius)

      radius = radius + scrapli
      call obuild(nout,'Inboard scrape-off',scrapli,radius)

      radius = radius + rminor
      call obuild(nout,'Plasma geometric centre',rminor,radius)

      radius = radius + rminor
      call obuild(nout,'Plasma outer edge',rminor,radius)

      radius = radius + scraplo
      call obuild(nout,'Outboard scrape-off',scraplo,radius)

      radius = radius + fwoth
      call obuild(nout,'Outboard first wall',fwoth,radius)

      radius = radius + blnkoth
      call obuild(nout,'Outboard blanket',blnkoth,radius)

      radius = radius + shldoth
      call obuild(nout,'Outboard shield',shldoth,radius)

      radius = radius + gapsto
      call obuild(nout,'Gap',gapsto,radius)

      radius = radius + ddwi
      call obuild(nout,'Vacuum vessel',ddwi,radius)

      radius = radius + tfthko
      call obuild(nout,'TF coil outer leg',tfthko,radius)

c  Vertical build

      call oheadr(nout,'Vertical Build')

      call ocmmnt(nout,'Double null case')

      write(nout,20)
 20   format(t43,'Thickness (m)',t60,'Height (m)')

      vbuild = 0.d0
      call obuild(nout,'Midplane',0.0D0,vbuild)

      vbuild = vbuild + rminor * kappa
      call obuild(nout,'Plasma top',rminor*kappa,vbuild)

      vbuild = vbuild + vgap
      call obuild(nout,'Top scrape-off',vgap,vbuild)

      vbuild = vbuild + divfix
      call obuild(nout,'Divertor structure',divfix,vbuild)

      vbuild = vbuild + shldtth
      call obuild(nout,'Top shield',shldtth,vbuild)

      vbuild = vbuild + vgap2
      call obuild(nout,'Gap',vgap2,vbuild)

C+**PJK 25/07/11 Added dewar thickness to vertical build
      vbuild = vbuild + ddwi
      call obuild(nout,'Vacuum vessel',ddwi,vbuild)

      vbuild = vbuild + tfcth
      call obuild(nout,'TF coil',tfcth,vbuild)

C  Port size information

      call osubhd(nout,'Port Size Information :')
      call ovarre(nout,'Port width (m)','(prtsz)',prtsz)
      call ovarre(nout,'Port requirement for beams (m)','(prtszreq)',
     +     prtszreq)

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE VBUILD

C *** Routine to calculate the vertical build inside the TF coil

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'rfp.h'

      DOUBLE PRECISION divfix,divht

      common/dbld/divfix

      divfix = 0.2d0

C *** Divertor geometry
      call divgeom(divht)

      if (vgaptf.eq.0.0D0) then
         vgap = divht
      else
         vgap = vgaptf
      end if

C *** Height to inside edge of TF coil
C+**PJK 25/07/11 Added previously-missing ddwi
      if (irfp.eq.0) then
         hmax = rminor * kappa + vgap + shldtth + divfix + vgap2 + ddwi
      else
C ***    TF coil is circular
         hmax = 0.5D0 *
     +        (ddwi+gapds+shldith+blnkith+fwith+scrapli+rminor
     +        +rminor+scraplo+fwoth+blnkoth+shldoth+gapsto+ddwi)
      end if

      return
      end
c______________________________________________________________________
      SUBROUTINE DIVGEOM(divht)

c  Subroutine to calculate divertor geometry
c  Written by J Galambos, 2/4/88., updated 7/5/89, 11/89, 12/89, 1/90
c
C  PJK 29/01/96 Added TART option with expanded divertor
C               (Peng SOFT paper). From Galambos 27/11/90
c
c  Some routine specific input :
c  divfix    - fixed divertor height for top & bottom structure
c  anginc    - angle of incidence of scrape-off field lines on the
c              divertor (rad)
c  soleno    - length along outer divertor plate that s.o. hits
c  plsepi(o) - poloidal length along the separatrix from null to
c              strike point on inboard [1.0 m] (outboard) [1.5 m]
c
c  Method :
c
c  First find divertor height. Inner (i) and outer (o) plasma surface
c  are approximated by arcs, and followed past x point to determine
c  max height.
c  thetai(o) = arc angle between the strike point and the null point.

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'divrt.h'

      DOUBLE PRECISION kap
      DOUBLE PRECISION soleno,divht,tri,xpointo,rprimeo,phio,
     +     thetao,yspointo,xspointo,yprimeb 

      soleno = 0.2d0

C+**PJK 29/01/96 TART option with expanded divertor chamber

      if (itart.eq.1) then
         divht = 1.75D0 * rminor
         goto 1000
      end if

c  Conventional tokamak divertor model

      kap = kappa
      tri = triang

c  Outer side
      xpointo = rmajor + rminor/2.d0*(kap**2 + tri**2 - 1.d0) /
     +     (1.d0 - tri)
      rprimeo = (xpointo - rmajor + rminor)
      phio = asin(kap*rminor/rprimeo)
      thetao = plsepo/rprimeo

c  Initial strike point
      yspointo = rprimeo * sin(thetao + phio)
      xspointo = xpointo - rprimeo * cos(thetao + phio)

c  Outer strike point radius - normalized to ITER
      rstrko = xspointo + 0.14d0

c  Uppermost divertor strike point (end of power decay)
C+**PJK 25/07/11 Changed sign of anginc
      yprimeb = soleno* cos(thetao + phio - anginc)
      divht = yprimeb + yspointo - kap*rminor

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE RIPPL(ripmax,rmajor,rminor,rtot,tfno,
     +     ripple,rtotl)

c  Subroutine to calculate TFC ripple and outer TFC leg radius.
c  Input the max. ripple and default outer leg location and the
c  routine checks to see if the ripple is OK. If not it moves
c  the outer leg appropriately.
c
c  INPUT :
c  ripmax - max ripple at plasma edge (peak to average) (%)
c  rmajor - major radius (m)
c  rminor - plasma minor radius (m)
c  rtot   - default radius to the outer TFC leg
c  tfno   - number of TF coils
c
c  OUTPUT :
c  ripple - ripple at plasma edge (%)
c  rtotl  - required minimum radius to the centre of the outer
c           TF coil leg (m)

      IMPLICIT NONE

      DOUBLE PRECISION prip,rotrp,rtotl,pripc,ripple,coeff
      DOUBLE PRECISION ripmax,rmajor,rminor,rtot,tfno

      coeff = 1.03333d0
     +     + 0.210480d0 * tfno
     +     - 4.45253d-2 * tfno**2
     +     + 3.50210d-3 * tfno**3
     +     - 1.28945d-4 * tfno**4
     +     + 1.84776d-6 * tfno**5

      prip = 0.01D0 * ripmax/coeff
      rotrp = 1.023d0*(rmajor+rminor)/prip**(1.d0/tfno)

      if (rotrp .gt. rtot) then
         rtotl = rotrp
         pripc = prip * 100.d0
         ripple = pripc * coeff
      else
         rtotl = rtot
         prip = (1.023d0*(rmajor+rminor)/rtot)**(tfno)
         pripc = prip*100.d0
         ripple = pripc * coeff
      end if

      return
      end
c______________________________________________________________________
      SUBROUTINE PORTSZ

c  This subroutine finds the required distance between the TF legs
c  for adequate beam access.
c
c  Input / other info:
c  beamwd   - beam width (m)
c  rtan     - beam tangency radius (m)
c  rtot     - radius to the centre of the outboard TF leg (m)
c  tfno     - number of TF coils
c
c  OUTPUT :
c  prtsz    - available port size (m)
c  prtszreq - required port size (m)

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'cdriv.h'

      DOUBLE PRECISION rtan,tfoll,tfolw,rl1,ang1,ang2,ang3,
     +     ps1,rl2,ang4,rl3,ang5,ang6,ang7,ang8,ps2

      rtan = frbeam * rmajor

c  Assume the outer TF leg has a width / depth ratio of 1 / 2

C+**PJK 11/03/93 Converted tfcth to tfthko throughout.

      tfoll = 1.414d0 * sqrt(arealeg)
      tfolw = sqrt(arealeg) / 1.414d0

      rl1 = sqrt ( (rtot - tfthko/2.d0)**2 + tfolw**2)
      ang1 = asin ( (rtan + beamwd/2.d0) / rl1 )
      ang2 = pi/2.d0 - pi/tfno + asin( tfolw/(2.d0*rl1) )
      ang3 = ang1 + ang2 - pi/2.d0
      ps1 = beamwd / cos(ang3)

      if (rtan .gt. (beamwd/2.d0)) then
         rl2 = rtot - tfthko/2.d0 + tfoll
         ang4 = atan(tfolw/rl2)
         rl3 = rl2/cos(ang4)
         ang5 = acos ( (rl3**2 + tfoll**2 - rl1**2)/(2.d0*rl3*tfoll))
         ang6 = asin( (rtan - beamwd/2.d0) / rl3 )
         ang7 = ang6 - ang5
         ang8 = pi/2.d0 - ang3
         ps2 = tfoll * sin(ang7) / sin(ang8)
      else
         ps2 = 0.d0
      end if

      prtszreq = ps1 + ps2

c  Port size available

      prtsz = ( 2.d0 * pi * (rtot - tfthko/2.d0) - tfno* tfolw) / tfno

      return
      end    
