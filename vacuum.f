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
C  Module         : $Id: vacuum.f,v 3.1 1993/06/11 14:28:23 peter Exp $
C
C  Module name    : $RCSfile: vacuum.f,v $
C  Version no.    : $Revision: 3.1 $
C
C  Creation date  : $Date: 1993/06/11 14:28:23 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE VACCALL(nout,iprint)

c  Subroutine to call the vacuum module

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'build.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'times.h'
      INCLUDE 'torsdat.h'

      DOUBLE PRECISION qtorus,gasld
      INTEGER nout,iprint

c  Need to fix this if we get an NB module
C+**PJK 14/12/92 This comment is clearly out of date since NBI models
C+**PJK 14/12/92 do exist within the code, but it is not clear how
C+**PJK 14/12/92 to get the right value for qtorus (NBI gas load D2/s)
      qtorus = 0.d0

      gasld = qfuel * afuel * 1.d-8

      call vacuum(powfmw,rmajor,rminor,kappa,shldoth,shldith,tfcth,
     +     rsldi-gapds-ddwi,tfno,tdwell,dene,idivrt,qtorus,gasld,
     +     vpumpn,nvduct,nvtype,dlscal,vacdshm,vcdimax,iprint,nout)

      return
      end
c______________________________________________________________________
      SUBROUTINE VACUUM(pfusmw,r0,aw,kappa,thshldo,thshldi,
     +     thtf,ritf,tfno,tdwell,nplasma,ndiv,qtorus,gasld,
     +     pumpn,nduct,nvtype,dlscal,mvdsh,dimax,iprint,nout)

c  Vacuum package: John Haines, FEDC.
c
c  Modified for installation into system code by Paul Dubois, LLNL.
c
c  Modified 2/13/91 by J Galambos, to put in a caller routine, and
c  "polish" for Supercode format.
c
c  Included in PROCESS in February 1992 by P. C. Shipe.
c
c  INPUT :
c  pfusmw  - fusion power (MW)
c  r0      - major radius (m)
c  aw      - minor radius (m)
c  kappa   - plasma elongation
c  thshldo - outer shield thickness (m)
c  thshldi - inner shield thickness (m)
c  thtf    - TF Coil thickness
c  ritf    - radius of inner TF leg point nearest plasma (m)
c  tfno    - number of TF coils
c  tdwell  - dwell time between pulses (s)
c  nplasma - plasma density (m**-3)
c  ndiv    - number of divertors  with pumping (single null = 1,
c            double null = 2 if pumping provided at both locations)
c  qtorus  - gas load  from NBI
c  gasld   - total DT gas load
c  nvtype  - pump type (0 = turbo molecular, 1 = cryo)
c  iprint  - switch for output (1 = yes, 0 = no)
c  nout    - unit specifier for output
c
c  OUTPUT:
c
c  pumpn   - number of hi - vacuum pumps
c  nduct   - number of ducts
c  dlscal  - duct-length equivalent for costing purposed (m)
c  mvdsh   - mass of a single vacuum duct shield (kg)
c  dimax   - diameter of passage from divertor to pumping ducts (m)
c
c  Vacuum pumping system module definition of variables :
c
c  a1max   - area between adjacent tf coils available for pump ducts
c  aw      - minor radius (m)
c  d(imax) - diameter of passage from divertor to pumping ducts (m)
c  d1max   - equivalent diameter corresponding to a1max
c  dlscal  - duct-length equivalent for costing purposes
c  densh   - density of shielding material (kg/m2)
c  dout    - diameter of ducts from passage to hi-vac pumps (m)
c  fhe     - fraction of neutral gas in divertor chamber that is helium
c  frate   - feed rate of DT into chamber (pellets + gas puffing +
c            NBI + ...)   (kg/s)
c  fsolid  - fraction of duct shielding that is solid material
c  gasld   - total DT load from fueller (kg/s)
c  k       - Boltzmann's constant = 1.38 e-23 J/K
c  kappa   - plasma elongation
c  l1      - length of passage from divertor to ducts (m)
c  l2      - length of ducts from divertor passage to elbow (m)
c  l3      - length of ducts from elbow to hi-vac pumps (m)
c  ltot    - total length of duct (m)
c  mvdsh   - mass of a single vacuum duct shield
c  ndiv    - number of divertors  with pumping (single null = 1,
c            double null = 2 if pumping provided at both locations)
c  nduct   - number of ducts = number of tf coils
c  nflag   - control option if ducts are too small in x-sectional area
c            = 1 - problem is identified in output, but run continues.
c            = 0 - no problem.
c  nplasma - plasma density (m^-3)
c  pumpn   - number of hi - vacuum pumps
c  ntf     - number of tf coils
c  ntype   - pump type;  ntype = 0 for turbomolecular pump
c            (mag. bearing) with a nominal speed of 2.0 m**3/s
c            (1.95 for N2, 1.8 for He, 1.8 for DT)
c            ntype = 1 for compound cryopump with nominal speed
c            of 10. m^3/s (9.0 for N2, 5.0 for He and 25. for DT)
c  ogas    - outgassing rate (Pa-m^3/s)
c  pbase   - base pressure (Pa)
c  prdiv   - pressure in divertor chamber during burn (Pa)
c  pend    - pressure in pl. chamber after burn (Pa)
c  pfus    - fusion power (W)
c  pstart  - pressure in pl. chamber before start of burn (Pa)
c  qtorus  - gas load from NBI (d2/s)
c  r0      - major radius (m)
c  rat     - outgassing rate (effective for n2) of plasma chamber
c            surface
c  ritf    - outer radius of inner leg of tf coil (m)
c  s(1)    - net pump speed (N2) required for pumpdown to base
c            pressure (m**3/s)
c  s(2)    - net pump speed (DT) required for pumpdown between
c            burns (m^3/s)
c  s(3)    - net pump speed (He) required for helium ash
c            removal (m^3/s)
c  s(4)    - net speed (DT) required to remove dt at fuelling rate
c            (m^3/s)
c  snet(1) - net pump speed (N2) provided (m^3/s)
c  snet(2) - net pump speed (DT) provided (m^3/s)
c  snet(3) - net pump speed (He) provided (m^3/s)
c  snet(4) - snet(2)
c  sp(1)   - speed of hi-vacuum pumps for N2
c  sp(2)   - speed of hi-vacuum pumps for DT
c  sp(3)   - speed of hi-vacuum pumps for He
c  sp(4)   - sp(2)
c  tdwell  - dwell time between burns (s)
c  thcsh   - thickness of shielding required between TF coils
c            and duct (m)
c  thdsh   - thickness of shielding required for the ducts (m)
c  thshldi - thickness of inboard shield (m)
c  thshldo - outboard blanket/shield thickness (m)
c  thtf    - tf coil thickness (m)
c  tn      - temperature of neutral gas in chamber (K)
c  xmult(i) - multiplier to convert conductance from gas species i to
c            nitrogen;  xmult(1)=1.0; xmult(2&4)=0.423; xmult(3)=0.378

      IMPLICIT NONE

      INCLUDE 'vaccom.h'
      INCLUDE 'osections.h'

      INTEGER iprint, nout, ndiv, nduct, nvtype
      INTEGER i, imax, ntf, nflag

      DOUBLE PRECISION pi
      PARAMETER (pi = 3.14159d0)

      DOUBLE PRECISION pfusmw,r0,aw,kappa,thshldo,thshldi,thtf,
     +     ritf,tfno
      DOUBLE PRECISION tdwell,nplasma,qtorus,gasld
      DOUBLE PRECISION pumpn,dlscal,mvdsh,dimax
      DOUBLE PRECISION a1max,area,arsh,a1,a2,a3,cap,ccc,ceff1,
     +     cmax,cnew,c1,c2,c3,dcap,dc1,dc2,dc3,densh,dd, dout, dnew,
     +     dy,d1max,fhe,frate,fsolid,ogas,pend,pfus,pstart,pumpn1,
     +     pumpn2,source,sss,thcsh,thdsh,theta,y,volume
      DOUBLE PRECISION s(4), d(4), ceff(4), xmult(4), sp(4), snet(4)
      DOUBLE PRECISION k1,k2,k3,l1,l2,l3,ltot,k

c+**CAG 22/10/92 Converted hollerith to character string.
      CHARACTER*5 ipump

      densh = 7900.d0
      fsolid = 0.9d0

      nvtype = ntype

c  Fusion power (W)
      pfus = pfusmw*1.d6

c  Integer number of TF coils
      ntf = int(tfno)

c  Total gas load to torus = load from fueller + load from NBI
c  frate (kg/s) = gasld (kg/s) + qtorus (d2/s) * 6.64d-27

      frate = gasld + qtorus * 6.64d-27
      k = 1.38d-23

c  Set Duct shield thickness to zero for no biological shielding
c  instead of thshldo/3.0D0
      thdsh = 0.d0

c  Shielding between duct and TF coils is scaled from inboard shield
c  thickness
      thcsh = thshldi / 3.d0

      xmult(1) = 1.d0
      xmult(2) = 0.423d0
      xmult(3) = 0.378d0
      xmult(4) = xmult(2)
      nduct = ntf * ndiv

c  Characteristics of pumps

      if (ntype.eq.0) then
         sp(1) = 1.95d0
         sp(2) = 1.8d0
         sp(3) = 1.8d0
      else
         sp(1) = 9.d0
         sp(2) = 25.d0
         sp(3) = 5.d0
      endif
      sp(4) = sp(2)

c  Calculate required pumping speed
c  Initial pumpdown based on outgassing
c  area = vacuum chamber/fw area (m^2)  ;  outgassing area = 10 x area
      area = 4.d0 * pi*pi * r0 * aw * sqrt((1.d0+ kappa * kappa) / 2.d0)
      ogas = rat * area * 10.d0
      s(1) = ogas  / pbase

c  Pumpdown between burns
c  volume = plasma chamber volume (m^3)

      pend = nplasma/2.d0 * k * tn
      pstart = 0.01d0 * pend
      volume = 2.d0 * pi * pi * r0 * aw * aw * kappa
      s(2) = volume / tdwell * log(pend / pstart)

c  Helium ash removal
c  source = alpha production rate (pa - m^3/s)
      source = pfus * 1.47d-09
      fhe = source / (frate * 4.985d5 )
      s(3) = source / prdiv / fhe

c  Removal of dt on steady state basis
      s(4) = (frate * 4.985d5 - source) / ( prdiv * (1.d0 - fhe) )

c  Calculate conductance of a single duct
c  a1 = area of aperture and duct (m^2)
c  a1max = max area possible between tf coils
      imax = 1
      cmax = 0.01d0
      pumpn = 1.d0
      nflag = 0

      do 40 i = 1, 4

c+**CAG 22/10/92 cmax incorrectly written as c  max.
         sss = dble(nduct) / (1.d0/sp(i)/pumpn + 1.d0/cmax*
     +        xmult(i)/xmult(imax))
         if (sss .gt. s(i)) goto 40
         imax = i

         ccc = 2.d0* s(i) / dble(nduct)
         pumpn1 = 1.d0 / ( sp(i)* (dble(nduct)/s(i) - 1.d0/ccc) )
         pumpn2 = 1.01d0 * s(i) / (sp(i)*dble(nduct))
         pumpn = max(pumpn, pumpn1, pumpn2)
         ceff(i) = 1.d0/( dble(nduct)/s(i) - 1.d0/(sp(i)*pumpn) )
         l1 = thshldo + thtf
         l2 = thshldo +4.d0
         l3 = 2.d0
         ltot = l1 + l2 + l3

c  Newton's method solution for duct diameter
 10      continue
         d(i) = 1.d0

 20      continue
         a1 = pi * d(i) * d(i) / 4.d0
         a2 = 1.44d0  * a1
         a3 = a2
         k1 = 4.d0 /3.d0 * d(i) / (l1  + 4.d0/3.d0 * d(i))
         k2 = 4.d0/3.d0 * d(i) * 1.2d0 / (l2 + 4.d0/3.d0  
     +        * d(i) * 1.2d0)
         k3 = 4.d0/3.d0 * d(i) * 1.2d0 / (l3 + 4.d0/3.d0 
     +        *  d(i) * 1.2d0)
         cap = 119.d0 * a1 / xmult(i)
         dcap =  2.d0 * cap / d(i)
         c1 = 119.d0 *  a1 * k1 / xmult(i)
         dc1 = c1 / d(i) * (3.d0 - k1)
         c2 = 119.d0 * a2 * k2 / xmult(i)
         dc2 = c2 / d(i) / 1.2d0 * (3.d0 - k2)
         c3 = 119.d0 * a3 * k3 / xmult(i)
         dc3 = c3 / d(i) / 1.2d0 * (3.d0 - k3)
         cnew = 1.d0 / (1.d0/cap + 1.d0/c1 + 1.d0/c2 + 1.d0/c3)
         y = -ceff(i) + cnew
         dy = cnew*cnew *(dcap/cap/cap +dc1/c1/c1 +dc2/c2/c2 +dc3/c3/c3)
         dnew = d(i) - y / dy
         dd = abs((d(i) - dnew)/d(i))
         d(i) = dnew
         if (dd .gt. 0.01D0) goto 20
         theta = pi / dble(ntf)
         a1max = (r0 + aw - ritf - thcsh / tan(theta))**2 * tan(theta)
         d1max = sqrt (4.d0 * a1max / pi)
         if (a1 .lt. a1max)  goto 30
         ceff(i) = 0.9d0 * ceff(i)
         if (ceff(i) .gt. (1.1d0 * s(i))) goto  10

c  Ducts are not big enough. Flag and continue.
         nflag = 1

 30      continue
         cmax = ceff(i)

 40   continue

      pumpn = pumpn * dble(nduct)
      dout = d(imax) * 1.2d0

c  Net pumping speeds provided by vacuum pumping system

      do 50 i = 1,4
         ceff1 = ceff(imax) * dble(nduct)
         snet(i) = 1.d0/(1.d0/(ceff1*xmult(imax)/xmult(i)) 
     +        + 1.d0/sp(i)/pumpn)
 50   continue

c  If cryopumps are used then an additional pump is required
c  for continuous operation with regeneration.

      if (ntype .eq. 1) pumpn = pumpn*2.d0

c  Information for costing routine
      dlscal =  (l1 * d(imax) ** 1.4d0 + (ltot-l1)*
     +     (d(imax)*1.2d0)**1.4d0)

c  Mass of duct shielding
      arsh = ((d(imax)*1.2d0+thdsh)**2 - (d(imax)*1.2d0)**2)/4.d0*pi
      mvdsh =  arsh * (ltot-l1) * densh * fsolid

      dimax = d(imax)

c  Pumping performance output

      if ((iprint.eq.0).or.(sect15.eq.0)) goto 1000

c  Output section

      call oheadr(nout,'Vacuum System')

      call ocmmnt(nout,'Pumpdown to Base Pressure :')
      call oblnkl(nout)
      call ovarre(nout,'First wall outgassing rate (Pa m/s)','(rat)',
     +     rat)
      call ovarre(nout,'Total outgassing load (Pa m3/s)','(ogas)',ogas)
      call ovarre(nout,'Base pressure required (Pa)','(pbase)',pbase)
      call ovarre(nout,'Required N2 pump speed (m3/s)','(s(1))',s(1))
      call ovarre(nout,'N2 pump speed provided (m3/s)','(snet(1))',
     +     snet(1))

      call osubhd(nout,'Pumpdown between Burns :')
      call ovarre(nout,'Plasma chamber volume (m3)','(volume)',volume)
      call ovarre(nout,'Chamber pressure after burn (Pa)','(pend)',pend)
      call ovarre(nout,'Chamber pressure before burn (Pa)','(pstart)',
     +     pstart)
      call ovarre(nout,'Dwell time between burns (s)','(tdwell)',tdwell)
      call ovarre(nout,'Required D-T pump speed (m3/s)','(s(2))',s(2))
      call ovarre(nout,'D-T pump speed provided (m3/s)','(snet(2))',
     +     snet(2))

      call osubhd(nout,'Helium Ash Removal :')
      call ovarre(nout,'Divertor chamber gas pressure (Pa)','(prdiv)',
     +     prdiv)
      call ovarre(nout,'Helium gas fraction in divertor chamber',
     +     '(fhe)',fhe)
      call ovarre(nout,'Required helium pump speed (m3/s)','(s(3))',
     +     s(3))
      call ovarre(nout,'Helium pump speed provided (m3/s)','(snet(3))',
     +     snet(3))

      call osubhd(nout,'D-T Removal at Fuelling Rate :')
      call ovarre(nout,'D-T fuelling rate (kg/s)','(frate)',frate)
      call ovarre(nout,'Required D-T pump speed (m3/s)','(s(4))',s(4))
      call ovarre(nout,'D-T pump speed provided (m3/s)','(snet(4))',
     +     snet(4))

      if (nflag.eq.1) then
         call oblnkl(nout)
         call ocmmnt(nout,'Vacuum pumping ducts are space limited.')
         write(nout,60) d1max
 60      format(' Maximum duct diameter is only ',f8.2,'m')
         call ocmmnt(nout,'Conductance is inadequate.')
         call oblnkl(nout)
      end if

      if (ntype .eq. 1) then
         ipump = 'cryo '
      else
         ipump = 'turbo'
      end if

      call oblnkl(nout)
      call ocmmnt(nout,
     +     'The vacuum pumping system size is governed by the')

      if (imax .eq. 1) then
         call ocmmnt(nout,'requirements for pumpdown to base pressure.')
      else if (imax .eq. 2) then
         call ocmmnt(nout,'requirements for pumpdown between burns.')
      else if (imax .eq. 3) then
         call ocmmnt(nout,'requirements for helium ash removal.')
      else
         call ocmmnt(nout,
     +        'requirements for D-T removal at fuelling rate.')
      end if

      call oblnkl(nout)

      call ovarin(nout,'Number of large pump ducts','(nduct)',nduct)
      call ovarre(nout,'Passage diameter, divertor to ducts (m)',
     +     '(d(imax))',d(imax))
      call ovarre(nout,'Passage length (m)','(l1)',l1)
      call ovarre(nout,'Diameter of ducts (m)','(dout)',dout)
      call ovarre(nout,'Duct length, divertor to elbow (m)','(l2)',l2)
      call ovarre(nout,'Duct length, elbow to pumps (m)','(l3)',l3)
      call ovarre(nout,'Number of pumps','(pumpn)',pumpn)
      call oblnkl(nout)
      write(nout,70) ipump
 70   format(' The vacuum system uses ',a5,'pumps')

 1000 continue

      return
      end
