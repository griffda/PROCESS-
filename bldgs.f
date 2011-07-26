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
C  Module name    : $RCSfile: bldgs.f,v $
C  Version no.    : $Revision: 3.6 $
C
C  Creation date  : $Date: 1996/03/07 16:04:28 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE BLDGCALL(nout,iprint)

c  Subroutine to call building routine

      IMPLICIT NONE

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'build.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'times.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'bldgvol.h'
      INCLUDE 'struccom.h'
      INCLUDE 'rfp.h'

C+**PJK 04/03/96 Added pfrmax, pfmmax to rfp.h

      DOUBLE PRECISION crrad,tfh,tfmtn,tfri,tfro

      INTEGER i,iprint,nout

c  Find max pf coil radius and mass (already calculated for RFPs)

      if (irfp.eq.0) then
         pfrmax = 0.0D0
         pfmmax = 0.0D0
         do 10 i=1,ncirt
            pfrmax = max(pfrmax,rb(i) )
            pfmmax = max(pfmmax, (1.d-3*(wtc(i)+wts(i))) )
 10      continue
      end if
         
      tfmtn = 1.d-3 * whttf
      tfro = rtot + tfthko/2.0D0
      tfri = rtfcin - tfcth/2.0D0
      tfh = (hmax +tfcth)*2.0D0
      crrad = pfrmax + 0.5D0
      
C+**PJK 19/11/93 tpulse = tramp + tohs + theat + tburn

C+**PJK 31/03/94 Cryogenic load calculation not performed here
C+**PJK 31/03/94 any more as this is already done in routine POWER
C+**PJK 31/03/94 prior to the call to this routine.
C      call cryo(tfsai,coldmass,ptfnuc,ensxpfm,tpulse,cpttf,tfno, 
C     +     helpow)

c  Reactor vault wall and roof thicknesses are hardwired

C+**PJK 08/01/96 Added IDHE3 to argument list of BLDGS

      call bldgs(idhe3,pfrmax,pfmmax,tfro,tfri,tfh,tfmtn,tfno,rsldo,
     +     rsldi,(hmax*2.0D0),whtshld,crrad,tfcbv,pfbldgm3,
     +     esbldgm3,helpow,iprint,nout,
     +     cryvol,triv,volrci,efloor,rbvol,rmbvol,wsvol,elevol,wrbi,
     +     admvol,shovol,convol,volnucb)

      return
      end
c_______________________________________________________________________
      SUBROUTINE BLDGS(idhe3,pfr,pfm,tfro,tfri,tfh,tfm,tfno,shro,
     +     shri,shh,shm,crr,tfcbv,pfbldgm3,esbldgm3,helpow,
     +     iprint,nout,
     +     cryv,triv,vrci,efloor,rbv,rmbv,wsv,elev,wrbi,
     +     admvol,shovol,convol,volnucb)

c  PLANT BUILDINGS
c
c  This routine determines the size of the plant buildings.
c  The reactor building and maintenance building are sized
c  based on the tokamak dimensions. The cryogenic building volume is
c  scaled based on the total cryogenic load. The other building
c  sizes are input from other modules or by the user.
c
c  This routine was modified to include fudge factors (fac1,2,...)
c  to fit the ITER design, 9/17/90. (jdg)
c
c  This routine was included in PROCESS in January 1992 by 
c  P. C. Shipe.
c
c  Input, from other modules (argument list) :
c  (magnet descriptions include case and individual dewar if used)
c
c  pfr       largest pf coil outer radius, m
c  pfm       largest pf coil mass, Ton (metric)
c  tfro      outer radius of tf coil, m
c  tfri      inner radius of tf coil, m
c  tfh       height of tf coil, m
c  tfm       mass of one tf coil, ton (metric)
c  tfno      number of tf coils
c  shro      outer radius of attached shield, m
c  shri      inner radius of attached shield, m
c  shh       height of attached shield, m
c  shm       total mass of attached shield, kg
c  crr       outer radius of common cryostat, m
c  tfcbv     volume of tf coil power supply building, m3
c  pfbldgm3  volume of pf coil power supply building, m3
c  esbldgm3  volume of energy storage building, m3
c  helpow    total cryogenic load, W
c  iprint    switch for printing (.ne.0)
c  nout      unit specifier for printing
c            (from entry bldgs2) :
c
c  Input (from group Bldgcom in variable descriptor file) :
c
c  rxcl   clearance around reactor, m
c  trcl   transportation clearance between components, m
c  row    clearance to building wall for crane operation, m
c  stcl   clearance above crane to roof, m
c  clh1   clearance from tf coil to cryostat top, m
c  clh2   clearance beneath tf coil to foundation, incl basement, m
c  fndt   foundation thickness, m
c  rbwt   reactor building wall thickness, m
c  rbrt   reactor building roof thickness, m
c  wgt    reactor building crane capacity (kg); calculated by
c         module if 0 is input, kg [2.e5]
c  wgt2   hot cell crane capacity (kg); calculated by module if
c         0 is input, kg [1.e5]
c  shmf   fraction of shield mass per tf coil to be moved in
c         the maximum shield lift
c  hcwt   hot cell wall thickness, m
c  hccl   clearance around components in hot cell, m
c
c  pibv   power injection building volume, m3
c  conv   control building volume, m3
c  admv   administration building volume, m3
c  shov   shops and warehouse volume, m3
c  triv   volume of tritium building, m3
c
c  OUTPUT:
c
c  vrci   Inner volume of reactor bldg, m3
c  wrbi   distance from centre of tokamak to reactor
c         building wall, m
c  efloor effective floor area of buildings, m2
c  rbv    outer volume of reactor building, m3
c  rmbv   volume of reactor maintenance bldg., m3
c  wsv    volume of warm shop, m3
c  elev   volume of electrical buildings, m3
c  cryv   volume of cryogenic building, m3
c  volnucb = volume of nuclear controlled buildings, m3

      IMPLICIT NONE

      INCLUDE 'bldgcom.h'
      INCLUDE 'osections.h'

      DOUBLE PRECISION pfr,pfm,tfro,tfri,tfh,tfm,tfno,shro,shri,shh,
     +     shm,crr,tfcbv,pfbldgm3,esbldgm3,helpow,cryv,triv,vrci,
     +     efloor,rbv,rmbv,wsv,elev,wrbi,admvol,shovol,convol,volnucb

      DOUBLE PRECISION ang, bmr, coill, crcl, cran, dcl,dcw, drbi,
     +     fac1, fac2, fac3, hcl, hcw, hrbi, hy, rbh, rbl, rbw, rmbh,
     +     rmbl, rmbw, rwl, rww, sectl, tch, tcl, tcw, wgts, wsa, wt

      INTEGER idhe3,iprint,nout

c  Reactor building

c  Determine basic machine radius
      bmr = max (crr,pfr,tfro)

c  Determine largest transported piece
      sectl = shro-shri
      coill = tfro-tfri
      sectl = max (coill, sectl)

c  Calculate half width of building
      wrbi = bmr+rxcl+sectl+trcl+row

c  Calculate length to allow pf or cryostat laydown
      pfr = max(crr,pfr)
      hy = bmr+rxcl+sectl+trcl+pfr
      ang = (wrbi-trcl-pfr)/hy
      if (abs(ang).gt.1.0D0) ang = abs(ang)/ang
      drbi = trcl+pfr+hy*sin(acos(ang))+wrbi

c  Crane height based on maximum lift
      if (wgt .gt. 1.0D0) then
         wt = wgt
      else
         wt = shmf*shm/tfno
         wt = max (wt,pfm,tfm)
      end if
      crcl = 9.41D-6*wt+5.1D0

c  Building height
      hrbi=clh2+tfh+clh1+trcl+tfh+crcl+stcl

c  Internal volume
      fac1 = 1.6D0
      vrci = fac1 * 2.0D0*wrbi*drbi*hrbi

c  External dimensions of reactor building

      rbw = 2.0D0*wrbi+2.0D0*rbwt
      rbl = drbi+2.0D0*rbwt
      rbh = hrbi+rbrt+fndt
      rbv = fac1 * rbw*rbl*rbh

c  Maintenance building
c
c  The reactor maintenance building includes the hot cells, the
c  decontamination chamber, the transfer corridors, and the waste
c  treatment building.  The dimensions of these areas are scaled
c  from a reference design based on the shield sector size.

c  Transport corridor size
      tcw = shro-shri+4.0D0*trcl
      tcl = 5.0D0*tcw+2.0D0*hcwt

c  Decontamination cell size
      dcw = 2.0D0*tcw+1.0D0
      dcl = 2.0D0*tcw+1.0D0

c  Hot cell size
      hcw = shro-shri+3.0D0*hccl+2.0D0
      hcl = 3.0D0*(shro-shri)+4.0D0*hccl+tcw

c  Radioactive waste treatment
      rww = dcw
      rwl = hcl-dcl-hcwt

c  Maintenance building dimensions
      rmbw = hcw+dcw+3.0D0*hcwt
      rmbl = hcl+2.0D0*hcwt

c  Height
      if (wgt2.gt.1.0D0) then
         wgts = wgt2
      else
         wgts = shmf*shm/tfno
      end if
      cran = 9.41D-6*wgts+5.1D0
      rmbh = 10.0D0 + shh+trcl+cran+5.1D0+stcl+fndt
      tch = shh+stcl+fndt

c  Volume
      fac2 = 2.8D0
      rmbv = fac2 * rmbw*rmbl*rmbh+tcw*tcl*tch

c  Warm shop and hot cell gallery
      wsa = (rmbw+7.0D0)*20.0D0+rmbl*7.0D0
      fac3 = 1.9D0
      wsv = fac3 * wsa*rmbh

c  Cryogenic building volume

      cryv = 55.0D0 * sqrt(helpow)

c  Other building volumes (dimensions are input from other modules
c  or by the user)

      elev = tfcbv + pfbldgm3 + esbldgm3 + pibv

c  Calculate effective floor area for ac power module

      efloor = (rbv+rmbv+wsv+triv+elev+conv+cryv+admv+shov)/6.0D0
      admvol = admv
      shovol = shov
      convol = conv

c  Total volume of nuclear buildings

      volnucb = ( vrci + rmbv + wsv + triv + cryv )

c  Output section

      if ((iprint.eq.0).or.(sect16.eq.0)) goto 1000

      call oheadr(nout,'Plant Buildings System')
      call ovarre(nout,'Internal volume of reactor building (m3)',
     +     '(vrci)',vrci)
      call ovarre(nout,'Dist from centre of torus to bldg wall (m)',
     +     '(wrbi)',wrbi)
      call ovarre(nout,'Effective floor area (m2)','(efloor)',efloor)
      call ovarre(nout,'Reactor building volume (m3)','(rbv)',rbv)
      call ovarre(nout,'Reactor maintenance building volume (m3)',
     +     '(rmbv)',rmbv)
      call ovarre(nout,'Warmshop volume (m3)','(wsv)',wsv)
      call ovarre(nout,'Tritium building volume (m3)','(triv)',triv)
      call ovarre(nout,'Electrical building volume (m3)','(elev)',elev)
      call ovarre(nout,'Control building volume (m3)','(conv)',conv)
      call ovarre(nout,'Cryogenics building volume (m3)','(cryv)',cryv)
      call ovarre(nout,'Administration building volume (m3)','(admv)',
     +     admv)
      call ovarre(nout,'Shops volume (m3)','(shov)',shov)
      call ovarre(nout,'Total volume of nuclear buildings (m3)',
     +     '(volnucb)',volnucb)

 1000 continue

      return
      end
