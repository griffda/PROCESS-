C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id: rfp.f,v 1.9 2006/05/25 09:27:02 pknight Exp $
C  Module name    : $RCSfile: rfp.f,v $
C  Version no.    : $Revision: 1.9 $
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE RFPTFC(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate TF coil information for the reversed field
C  pinch model.
C  Coils are assumed to be circular.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  27 February 1996
C
C--Reference
C  None
C  
C--History
C  27/02/96 PJK 1.000 Initial version
C
C--Arguments
C  NOUT   : (INPUT)  Fortran output unit identifier
C  IPRINT : (INPUT)  Switch denoting whether to write output to file
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'build.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'rfp.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER iprint,nout

C  Local variables
      DOUBLE PRECISION ltfleg,rinr,routr,tfcind1

C  External routines
      EXTERNAL oheadr,osubhd,ovarre,portsz

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Radius of centre of inboard TF coil leg
      rtfcin = bore + ohcth + gapoh + bcylth + tfcth/2.0D0

C  Radius of outer edge of inboard TF coil leg
      rinr = rtfcin + tfcth/2.0D0

C *** Unlike in a tokamak, the inboard legs do not necessarily form
C *** a continuous ring. tftort is calculated ensuring that adjacent
C *** coils are not wider than their inboard edge centres are apart
C *** (constraint equation 47).

C *** Total area of inboard legs (coils have rectangular cross-section)

      tfareain = tfno * tfcth * tftort

C *** Total current in TF coils

      ritfc = oacdcp * tfareain

C *** Peak toroidal field and radius of its occurrence

      rbmax = rinr
      bmaxtf = 2.0D-7 * ritfc / rbmax

C  Radius of inner edge of outboard TF coil leg
      routr = rtot - tfcth/2.d0

C  Centering and vertical forces
      if (bore .eq. 0.0D0) then
         cforce = 0.d0
      else
         cforce = bmaxtf * ritfc/(2.d0*tfno)
      end if
      vforce = 0.55d0 * bt * rmajor * ritfc/2.d0 *
     +     log(routr/rinr)/tfno

C  Bore (gap between inner and outer TF coil legs)
      tfboreh = rtot - rbmax - tfcth/2.d0

C  Number of turns per leg
      turnstf = ritfc / (tfno * cpttf)

C  Outer leg information (per leg)
C  ===============================

C  Cross-sectional area
      arealeg = tfcth * tftort

C  Length (circular coils, half assigned to 'outboard')
      ltfleg = 0.5D0 * pi * (tfboreh+tfcth)

C  Volume
      voltfleg = ltfleg * arealeg

C  Resistance
      rhotfleg = ltfleg * tflegres/arealeg

C  Total weight (of all legs)
      whttflgs = voltfleg * tfno * (1.d0 - vftf) * 8900.d0

C  Inner leg information (all legs)
C  ================================

C  Volume
      volcp = voltfleg * tfno

C  Weight of conductor
      whtcp = volcp * 8900.d0 * (1.d0 - fcoolcp)

C  Total weight of TF coils
      whttf = whtcp + whttflgs

C  Resistance
      rhocp = 1.d-8 * (1.72d0 + 0.0039d0* tcpav ) / 0.92d0

C  Resistive power losses
      prescp = rhocp * (ritfc/( tfareain*(1.d0-fcoolcp)))**2*volcp*
     +     (1.d0-fcoolcp)

C  Stress information (radial, tangential, vertical)

      sigrad = 1.d-6 * bmaxtf**2* (5.d0 + 0.34d0)/  (8.d0*rmu0 *
     +     (1.d0 - fcoolcp) )
      sigtan = sigrad
      sigver = 0.d0

C  Inductance (N.B. hmax may not be equal to the coil radius)
      tfcind1 = hmax * rmu0/pi * log( routr / rinr )

C  Stored energy per coil (GJ)
      estotf = 0.5d-9 * tfcind1 * ritfc**2/tfno

C  Port size
      call portsz

      if ((iprint.eq.0).or.(sect07.eq.0)) goto 1000

C  Output section

      call oheadr(nout,'TF Coils')
      call ovarre(nout,'TF coil current (A)','(ritfc)',ritfc)
      call ovarre(nout,'TF coil current density (A/m2)','(oacdcp)',
     +     oacdcp)
      call ovarre(nout,'Peak field at the TF coils (T)','(bmaxtf)',
     +     bmaxtf)
      call ovarre(nout,'Ripple at plasma edge (%)','(ripple)',
     +     ripple)
      call ovarre(nout,'Allowable ripple (%)','(ripmax)',ripmax)
      call ovarre(nout,'Number of TF coil legs','(tfno)',tfno)
      
      call osubhd(nout,'Energy and Forces :')
      call ovarre(nout,'Stored energy per coil (GJ)','(estotf)',
     +     estotf)
      call ovarre(nout,'Vertical force on inner leg (N)','(vforce)',
     +     vforce)
      call ovarre(nout,'Centering force on inner leg (N/m)',
     +     '(cforce)',cforce)
      call ovarre(nout,'Radial stress (Pa)','(sigrad)',sigrad)
      call ovarre(nout,'Transverse stress (Pa)','(sigtan)',sigtan)
      call ovarre(nout,'Vertical stress (Pa)','(sigver)',sigver)
      call ovarre(nout,'Weight of inner legs (kg)','(whtcp)',whtcp)
      call ovarre(nout,'Total TF coil weight (kg)','(whttf)',whttf)
      call ovarre(nout,'Inner leg resistive power (W)','(prescp)',
     +     prescp)
      call ovarre(nout,'Average conductor temperature (C)','(tcpav)',
     +     tcpav)

 1000 continue

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE RFPPFC(NOUT,IPRINT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate PF coil information for the reversed field
C  pinch model.
C  Coils are scaled from the TITAN-I OH/EF coil set.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  29 February 1996
C
C--Reference
C  UCLA-PPG-1200 TITAN RFP Fusion Reactor Study
C  
C--History
C  29/02/96 PJK 1.000 Initial version
C
C--Arguments
C  NOUT   : (INPUT)  Fortran output unit identifier
C  IPRINT : (INPUT)  Switch denoting whether to write output to file
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'rfp.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'fwblsh.h'
      INCLUDE 'build.h'
      INCLUDE 'vltcom.h'
      INCLUDE 'osections.h'

C  Arguments
      INTEGER nout,iprint

C  Local variables
      DOUBLE PRECISION bpf2(nrfppf),cc(nrfppf),curdpf(nrfppf),
     +     curpf(nrfppf),drr(nrfppf),dzz(nrfppf),rc(nrfppf),xc(nrfppf),
     +     zc(nrfppf)

      DOUBLE PRECISION areaspf,br,bri,bro,bz,bzi,bzo,cmass,dens,
     +     forcepf,psi,rl,rp,zp

      INTEGER i,j,jj,k,nc

C  External routines
      EXTERNAL bfield,efcurr,oblnkl,oheadr,ovarre

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      if (iprint.eq.1) goto 130

C *** The OH coils are the first 14 PF coils
C *** The superconducting EF coils are PF coils 15 and 16

C *** Number of turns in each coil

      nturns(1) = 10.0D0
      nturns(2) = 28.0D0
      nturns(3) = 10.0D0
      nturns(4) = 30.0D0
      nturns(5) = 30.0D0
      nturns(6) = 52.0D0
      nturns(7) = 26.0D0

      nturns(15) = 88.0D0
      nturns(16) = 88.0D0

C *** Coil cross-sectional dimensions

      drpf(1) = 0.20D0
      drpf(2) = 0.40D0
      drpf(3) = 0.30D0
      drpf(4) = 0.30D0
      drpf(5) = 0.60D0
      drpf(6) = 0.40D0
      drpf(7) = 0.40D0

      drpf(15) = 0.70D0
      drpf(16) = 0.70D0

      dzpf(1) = 0.30D0
      dzpf(2) = 0.42D0
      dzpf(3) = 0.20D0
      dzpf(4) = 0.60D0
      dzpf(5) = 0.30D0
      dzpf(6) = 0.78D0
      dzpf(7) = 0.39D0

      dzpf(15) = 0.70D0
      dzpf(16) = 0.70D0

C *** Offset locations from plasma major radius normalised to the
C *** TF coil external radius (1.125m for TITAN-II)

      drr(1) =  1.8667D0
      drr(2) =  0.1867D0
      drr(3) = -0.5422D0
      drr(4) = -1.0578D0
      drr(5) = -1.4667D0
      drr(6) = -1.5556D0
      drr(7) = -1.5556D0

      dzz(1) = 1.0667D0
      dzz(2) = 2.1422D0
      dzz(3) = 2.1333D0
      dzz(4) = 1.4667D0
      dzz(5) = 1.2711D0
      dzz(6) = 0.7200D0
      dzz(7) = 0.1867D0

      do 10 i = 8,14
         drr(i) =  drr(i-7)
         dzz(i) = -dzz(i-7)
         nturns(i) = nturns(i-7)
         drpf(i) = drpf(i-7)
         dzpf(i) = dzpf(i-7)
 10   continue

      drr(15) = 2.3111D0
      drr(16) = 2.3111D0

      dzz(15) =  2.2133D0
      dzz(16) = -2.2133D0

C *** Scaled coil locations (largest coil radius is stored in pfrmax)

      pfrmax = 0.0D0
      do 20 i = 1,nrfppf
         rrpf(i) = rmajor + (hmax+tfcth)*drr(i)
         zzpf(i) =          (hmax+tfcth)*dzz(i)
         pfrmax = max(pfrmax,(rrpf(i)+0.5D0*drpf(i)))
 20   continue

C *** Current per turn in OH coils
C *** TITAN-I has 75 kA/turn for a plasma current of 17.8 MA

      do 30 i = 1,14
         cptrfp(i) = 7.5D4/17.8D6 * plascur
 30   continue

C *** Current per turn in EF coils
C *** TITAN-I has -109.32 kA/turn for a plasma current of 17.8 MA

      cptrfp(15) = -1.0932D5/17.8D6 * plascur
      cptrfp(16) = -1.0932D5/17.8D6 * plascur

C *** Scale correctly to obtain correct vertical field at plasma

      call efcurr(plascur,rmajor,rminor,betap,rli,nrfppf,rrpf,zzpf,
     +     nturns,cptrfp)

C *** Total currents

      do 40 i = 1,nrfppf
         curpf(i) =  cptrfp(i) * nturns(i)
 40   continue

C *** Current densities (ignoring void fractions)

      do 50 i = 1,nrfppf
         curdpf(i) = curpf(i)/(drpf(i)*dzpf(i))
 50   continue

C *** Resistances and resistive power losses
C *** (EF coils 15 and 16 are superconducting)

      powpfres = 0.0D0
      do 60 i = 1,14
         resrfp(i) = pfclres * 2.0D0 * pi * rrpf(i) *
     +        (nturns(i)**2) / (drpf(i)*dzpf(i)*(1.0D0-vf(i)))
         powpfres = powpfres + (curpf(i)/nturns(i))**2 * resrfp(i)
 60   continue
      resrfp(15) = 0.0D0
      resrfp(16) = 0.0D0

C *** Masses (highest individual coil mass (tonnes) is stored in pfmmax)

      pfmmax = 0.0D0
      whtpf = 0.0D0
      do 70 i = 1,nrfppf
         if (i.le.14) then
            dens = dcopper
         else
            dens = dcond(isumatpf)
         end if
         cmass = 2.0D0*pi*rrpf(i)*drpf(i)*dzpf(i)*dens*(1.0D0-vf(i))
         pfmmax = max(pfmmax,1.0D-3*cmass)
         whtpf = whtpf + cmass
 70   continue

C *** Peak fields at each coil

      do 80 i = 1,nrfppf

C *** Field at inside edge of coil
         call bfield(nrfppf,nrfppf,rrpf,zzpf,curpf,xc,rrpf(i)-drpf(i),
     +        zzpf(i),bri,bzi,psi)

C *** Field at outside edge of coil
         call bfield(nrfppf,nrfppf,rrpf,zzpf,curpf,xc,rrpf(i)+drpf(i),
     +        zzpf(i),bro,bzo,psi)

         bpf(i) = sqrt(bri**2 + bzi**2)
         bpf2(i) = sqrt(bro**2 + bzo**2)

 80   continue

C *** (J x B) force on each EF coil (ignored 2 x pi?)

      forcepf = 0.5D0 * (bpf(15)+bpf2(15)) * abs(curpf(15)) * rrpf(15)

C *** Stress ==> cross-sectional area of supporting steel to use
C *** Assume 500 MPa stress limit, 2/3 of the force is supported
C *** in the outer (steel) case

      areaspf = 0.666D0 * forcepf / 5.0D8

      whtpfs = areaspf * 2.0D0*pi*rrpf(15) * denstl

C *** Assume that the additional steel shielding the superconductors
C *** has the same cross-sectional area as the coils themselves

      whtpfs = whtpfs + 2.0D0*pi*rrpf(15)*drpf(15)*dzpf(15)*denstl

C *** Double the steel mass, as there are two coils

      whtpfs = 2.0D0*whtpfs

C *** Mutual inductances

      nc = nrfppf-1
      do 120 i = 1,nrfppf
         do 90 j = 1,nrfppf-1
            if (j.ge.i) then
               jj = j+1
            else
               jj = j
            end if
            zc(j) = zzpf(jj)
            rc(j) = rrpf(jj)
 90      continue
         rp = rrpf(i)
         zp = zzpf(i)
         call bfield(nrfppf,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)
         do 110 k = 1,nrfppf
            if (k.lt.i)  then
               sxlg(i,k) = xc(k) * nturns(k) * nturns(i)
               goto 100
            end if
            if (k.eq.i)  then
               rl = dzpf(k)/sqrt(pi)
               sxlg(k,k) = rmu0 * nturns(k)**2 * rrpf(k) *
     +              (log(8.0D0*rrpf(k)/rl) - 1.75D0)
               goto 100
            end if
            sxlg(i,k) = xc(k-1) * nturns(k) * nturns(i)
 100        continue
 110     continue
 120  continue

 130  continue

      if ((iprint.eq.0).or.(sect08.eq.0)) goto 1000

C *** Output section

      call oheadr(nout,'PF Coils')

      call ovarre(nout,'Total mass of PF coils (kg)','(whtpf)',whtpf)
      call ovarre(nout,'Mass of EF coil steel shield (kg)',
     +     '(whtpfs)',whtpfs)
      call ovarre(nout,'PF coil resistive power (W)','(powpfres)'
     +     ,powpfres)

      call oblnkl(nout)
      write(nout,140)
 140  format(' coil',t12,'R(m)',t20,'Z(m)',t28,'dR(m)',t36,'dZ(m)',
     +     t43,'turns',t51,'I(MA)',t58,'J(MA/m2)')
      call oblnkl(nout)

      write(nout,150) (i,rrpf(i),zzpf(i),drpf(i),dzpf(i),nturns(i),
     +     curpf(i)/1.0D6,curdpf(i)/1.0D6,i=1,nrfppf)
 150  format(i2,t8,7f8.2)

 1000 continue

      return
      end       
C----------------------------------------------------------------------
      SUBROUTINE RFPPFP(nout,iprint)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.001
C
C--Description
C  Routine to calculate the MVA, power and energy requirements
C  for the RFP PF coil systems.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  22 January 1997
C
C--Reference
C  None
C  
C--History
C  01/03/96 PJK 1.000 Initial version
C  22/01/97 PJK 1.001 Subsumed pfelect.h into htpwr.h
C
C--Arguments
C  NOUT   : (INPUT)  Fortran output unit identifier
C  IPRINT : (INPUT)  Switch denoting whether to write output to file
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'vltcom.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'build.h'
      INCLUDE 'times.h'
      INCLUDE 'htpwr.h'
      INCLUDE 'osections.h'
      INCLUDE 'rfp.h'

C  Arguments
      INTEGER iprint,nout

C  Local variables
      DOUBLE PRECISION albusa(nrfppf),cktr(nrfppf),pfbusr(nrfppf),
     +     powpfii(nrfppf),psmva(nrfppf),rcktpm(nrfppf),rcktvm(nrfppf),
     +     vpfi(nrfppf)

      DOUBLE PRECISION cptburn,delktim,di,engx,engxpc,ensxpf,pfbusl,
     +     pfbuspwr,powpfi,powpfr,powpfr2,vpfij

      INTEGER i,ic,ig,ipf,jjpf,jjpf2,jpf,ngrpt

C  External routines
      EXTERNAL oheadr,ovarre

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      do 10 i = 1,nrfppf
         powpfii(i) = 0.d0
         cktr(i) = 0.d0
 10   continue

      if (iprint.ne.0) goto 70

C *** Bus length
      pfbusl = 8.0D0 * rmajor + 140.0D0

C *** PF coil resistive power requirements
C *** Bussing losses assume aluminium bussing with 100 A/cm**2

      srcktpm = 0.0D0
      pfbuspwr = 0.0D0

      do 20 ic = 1,nrfppf

C *** Cross-sectional area of bus
         albusa(ic) = abs(cptrfp(ic)) / 100.0D0

C *** Include 50% enhancement for welds,joints etc, (G. Gorker)
         pfbusr(ic) = 1.5D0 * 2.62D-4 * pfbusl / albusa(ic)

C *** Total PF coil circuit resistance
         cktr(ic) = resrfp(ic) + pfbusr(ic)
         rcktvm(ic) = abs(cptrfp(ic))*cktr(ic)
         rcktpm(ic) = 1.0D-6*rcktvm(ic)*abs(cptrfp(ic))

C *** Compute the sum of resistive power in the PF circuits, kW
         pfbuspwr = pfbuspwr + 1.0D-3 * pfbusr(ic) * cptrfp(ic)**2
         srcktpm = srcktpm + 1.0D3*rcktpm(ic)
 20   continue

C *** Inductive MVA requirements, and stored energy

      delktim = tohs

C *** PF system inductive MVA requirements
C *** OH coils are assumed to swing symmetrically about zero in
C *** a period tohs.
C *** EF coils swing from zero to maximum current in the same time.

      powpfi = 0.0D0
      powpfr = 0.0D0
      powpfr2 = 0.0D0
      ensxpf = 0.0D0

      do 30 i = 1,nrfppf
         powpfii(i) = 0.0D0
         vpfi(i) = 0.0D0
 30   continue

      do 50 jpf = 1,nrfppf
         engx = 0.0D0
         do 40 ipf = 1,nrfppf
            if (ipf.lt.15) then
               di = 2.0D0 * cptrfp(ipf)
            else
               di = cptrfp(ipf)
            end if
            vpfij = sxlg(jpf,ipf) * abs(di)/delktim
            vpfi(jpf) = vpfi(jpf) + vpfij
            powpfii(jpf) = powpfii(jpf) + vpfij*abs(cptrfp(jpf))/1.0D6
            engx = engx + sxlg(jpf,ipf)*abs(cptrfp(ipf))
 40      continue

C *** Compute inductive energy of each PF coil circuit
         engxpc  = 0.5D0 * engx * abs(cptrfp(jpf))
         ensxpf = ensxpf + engxpc
         powpfr = powpfr + nturns(jpf)*abs(cptrfp(jpf))*cktr(jpf)/1.0D6
         powpfr2 = powpfr2 +
     +        nturns(jpf)*abs(cptrfp(jpf))*cktr(jpf)/1.0D6
         powpfi = powpfi + powpfii(jpf)

 50   continue

C *** Compute the maximum stored energy and the maximum dissipative
C *** energy in all the PF circuits over the entire cycle time, MJ

      ensxpfm = 1.0D-6 * ensxpf

C *** Check for peak MVA requirements

      peakmva = max( (powpfr + powpfi), powpfr2)

      vpfskv = 20.0D0
      pfckts = dble(nrfppf) + 6.0D0
      spfbusl = pfbusl*pfckts
      acptmax = 0.0D0
      spsmva = 0.0D0

      do 60 jpf = 1,nrfppf

C *** Compute the power supply MVA for each PF circuit
         psmva(jpf) = 1.0D-6 * abs(vpfi(jpf)*cptrfp(jpf))

C *** Compute the sum of the power supply MVA of the PF circuits
         spsmva = spsmva + psmva(jpf)

C *** Compute the average of the maximum currents in the PF circuits, kA
         acptmax = acptmax + 1.0D-3 * abs(cptrfp(jpf))/pfckts

 60   continue

      goto 1000

C *** Output Section

 70   continue

      if (sect13.eq.0) goto 1000

      call oheadr(nout,'PF Coil Power Conversion')
      call ovarre(nout,'Number of PF coil circuits','(pfckts)',pfckts)
      call ovarre(nout,'Total power supply MVA for PF circuits',
     +     '(spsmva)',spsmva)
      call ovarre(nout,'Av. max curr/turn of PF coil circuits (kA)',
     +     '(acptmax)',acptmax)
      call ovarre(nout,'Total PF coil circuit bus length (m)',
     +     '(spfbusl)',spfbusl)
      call ovarre(nout,'Total PF coil bus resistive power (kW)',
     +     '(pfbuspwr)',pfbuspwr)
      call ovarre(nout,'Total PF coil resistive power (kW)',
     +     '(srcktpm)',srcktpm)
      call ovarre(nout,'Maximum PF coil voltage (kV)','(vpfskv)',vpfskv)
      call ovarre(nout,'Max stored energy in PF coil circuits (MJ)',
     +     '(ensxpfm)',ensxpfm)

 1000 continue

      return
      end   
C----------------------------------------------------------------------
      SUBROUTINE RFPPHY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.016
C
C--Description
C  Subroutine to calculate reversed field pinch
C  plasma physics information
C
C--Author
C  Peter Knight D3/162a Culham, ext.6368
C
C--Date
C  22 May 2006
C
C--Reference
C  UCLA-PPG-1100 TITAN RFP Fusion Reactor Study,
C                Scoping Phase Report, January 1987
C  
C--History
C  05/03/96 PJK 1.000 Initial version
C  01/04/98 PJK 1.010 Modified calls to BETCOM and PCOND
C  01/04/98 PJK 1.011 Modified call to BETCOM
C  24/04/98 PJK 1.012 Modified call to BETCOM
C  30/06/98 PJK 1.013 Modified call to PCOND
C  19/01/99 PJK 1.014 Modified call to PCOND
C  16/07/01 PJK 1.015 Modified call to PCOND
C  22/05/06 PJK 1.016 Modified call to PALPH2
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
      INCLUDE 'rfp.h'

C  Local variables
      DOUBLE PRECISION
     +     alphap,betat,bphi,fusrat,n0e,n0i,pht,pinj,p0,sbar,
     +     sigvdt,taup,t0e,t0i,zimp,zion

C  External routines
      EXTERNAL beamfus,betcom,cudriv,ftheta,palph,palph2,pcond,
     +     phyaux,pohm,radpwr,rether

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

C *** Calculate reversal parameter F from F-theta curve

      call ftheta(rfpth,rfpf)

C *** Average value of toroidal field
C *** rfpf is constrained to be negative, but bt and TF coil
C *** current are positive in PROCESS

      bphi = ABS(bt/rfpf)

C *** Poloidal field at plasma edge

      bp = bphi * rfpth

C *** Plasma current

      plascur = 5.0D6 * rminor * bp

C *** Cylindrical safety factor (for compatibility with tokamaks)

      qstar = rminor * bt / (rmajor * bp)
      q95 = q

C *** Total field (using average toroidal field)

      btot = sqrt(bphi**2 + bp**2)

C *** Poloidal beta

      betap = beta * ( btot/bp )**2

C *** Set OH coil ramp times

      if (lpulse.ne.1) then

         if (tohsin.eq.0.0D0) then
            tohs = plascur/5.0D5
            tramp = tohs
            tqnch = tohs
         else
            tohs = tohsin
         end if

      else

         tramp = max(tramp,tohs)
         tqnch = max(tqnch,tohs)

      end if

      tburn0 = tburn

C *** Pulse and down times : The reactor is assumed to be 'down'
C *** at all times outside of the plasma current flat-top period.
C *** The pulse length is the duration of non-zero plasma current

      tpulse = tohs + theat + tburn + tqnch
      tdown  = tramp + tohs + tqnch + tdwell

C *** Bootstrap current fraction

      bootipf = 0.0D0

C *** Fraction of plasma current produced by inductive means

      facoh = max( 1.0D-10, (1.0D0 - fvsbrnni) )

C *** Fraction of plasma current produced by auxiliary current drive

      faccd = fvsbrnni - bootipf

C *** Do auxiliary current drive power calculations

      if (irfcd.ne.0) call cudriv(nout,0)

C *** Calculate fusion power

      call palph(alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit,
     +     idhe3,iiter,pcoef,pi,ti,palp,pcharge,pneut,sigvdt)

C *** Calculate neutral beam slowing down effects
C+**PJK 01/04/98 If ignited, then ignore beam fusion effects

      if ((pnbeam.ne.0.0D0).and.(ignite.eq.0)) then
         call beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie,
     +        ealpha,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol,
     +        zeffai,betanb,dnbeam2,palpnb)
      end if

C+**PJK 22/05/06 Added IFALPHAP switch to argument list of PALPH2

      call palph2(bt,bp,dene,deni,dnitot,ftr,falpe,falpi,palpnb,
     +     ifalphap,pcharge,pcoef,pneut,te,ti,vol,alpmw,betaft,
     +     palp,palpi,palpe,pfuscmw,powfmw)

C *** Neutron wall load

      wallmw = ffwal * (pneut*vol) / sarea

C *** Calculate ion/electron equilibration power

      call rether(alphan,alphat,dene,dlamie,te,ti,zeffai,pie)

C *** Calculate radiation power

      call radpwr(alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,rmajor,
     +     rminor,ralpne,rncne,rnone,rnfene,ssync,ten,vol,pbrem,plrad,
     +     prad,psync)

C *** Limit for minimum radiation power

      pht = 1.0D-6 * (pinji + pinje) + alpmw + pcharge*vol
      pbrem = max( (fradmin*pht/vol), pbrem)
      prad = pbrem + psync

C *** Calculate ohmic power

      call pohm(facoh,ires,kappa95,plascur,rmajor,rminor,ten,vol,zeff,
     +     pohmpv,rpfac,rplas)

C *** Density limit (arbitrary large number for now...)

      pinj = 1.0D-6 * (pinje + pinji)/vol
      pdivt = vol * (palp + pcharge + pinj + pohmpv - prad - plrad)
      pdivt = max(0.001D0, pdivt)

      dnelimt = 1.0D23

C *** Calculate transport losses and energy confinement time using the
C *** chosen scaling law

      call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hfact,
     +     iinvqd,isc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji,
     +     plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q95,qstar,vol,
     +     xarea,zeff,ptre,ptri,tauee,tauei,taueff,powerht)

C *** Calculate volt-second requirements

C      call vscalc(csawth,eps,facoh,gamma,kappa,rmajor,rmu0,rplas,
C     +     plascur,tburn,phiint,rli,rlp,vsbrn,vsind,vsres,vsstt)

C *** Calculate auxiliary physics related information
C *** for the rest of the code

      sbar = 1.0D0

      call phyaux(aspect,dene,deni,idhe3,plascur,powfmw,sbar,dnalp,
     +     dnprot,taueff,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

C *** Poloidal beta limit is set by input parameter betpmx

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE FTHETA(THETA,F)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the RFP reversal parameter F, given the
C  pinch parameter THETA
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  06 March 1996
C
C--Reference
C  UCLA-PPG-1100 TITAN RFP Fusion Reactor Study,
C                Scoping Phase Report, Jan 1987, Fig.4.2-2
C  
C--History
C  06/03/96 PJK 1.000 Initial version
C
C--Arguments
C  THETA  : (INPUT)  RFP pinch parameter
C  F      : (OUTPUT) RFP reversal parameter
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'numer.h'

C  Arguments
      DOUBLE PRECISION theta,f

C  External routines
      EXTERNAL ranger

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Check for sensible theta range

      call ranger(nout,'RFP theta',theta,0.0D0,1.8D0)

C *** Polynomial fit to experimental data

      f = 1.0D0
     +     + theta*(-2.45461D-02
     +     + theta*(-3.76376D-01
     +     + theta*(-1.89471D-01
     +     + theta*( 7.64083D-02
     +     + theta*(-1.31779D-02)))))

      return
      end
C----------------------------------------------------------------------
      SUBROUTINE EFCURR(plascur,rmajor,rminor,betap,rli,nrfppf,rrpf,
     +     zzpf,nturns,cptrfp)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--Description
C  Routine to calculate the current per turn in the EF coils required
C  to provide the correct vertical field at the plasma.
C
C--Author
C  Peter Knight D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  06/03/96
C
C--Reference
C  None
C  
C--History
C  06/03/96 PJK 1.000 Initial version
C
C--Arguments
C  plascur  : (INPUT)  plasma current (A)
C  rmajor   : (INPUT)  plasma major radius (m)
C  rminor   : (INPUT)  plasma minor radius (m)
C  betap    : (INPUT)  plasma poloidal beta
C  rli      : (INPUT)  plasma internal inductance
C  nrfppf   : (INPUT)  number of RFP PF coils
C  rrpf     : (INPUT)  radial positions of PF coils (m)
C  zzpf     : (INPUT)  vertical positions of PF coils (m)
C  nturns   : (INPUT)  number of turns in each PF coil
C  cptrfp   : (IN/OUT) current per turn in each PF coil (A)
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER nrfppf
      DOUBLE PRECISION plascur,rmajor,rminor,betap,rli
      DOUBLE PRECISION rrpf(nrfppf),zzpf(nrfppf),nturns(nrfppf),
     +     cptrfp(nrfppf)

C  Local variables
      DOUBLE PRECISION rc(2),zc(2),cc(2),xc(2)
      DOUBLE PRECISION br,bv,bz,psi

C  External routines
      EXTERNAL bfield

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Required vertical field at plasma centre

      bv = -1.0D-7 * plascur / rmajor *
     +     (log(8.0D0*rmajor/rminor) + betap + 0.5D0*rli -1.5D0)

C *** Actual vertical field at plasma centre, due to unscaled
C *** EF coil currents

      rc(1) = rrpf(15)
      rc(2) = rrpf(16)
      zc(1) = zzpf(15)
      zc(2) = zzpf(16)
      cc(1) = cptrfp(15)*nturns(15)
      cc(2) = cptrfp(16)*nturns(16)

      call bfield(2,2,rc,zc,cc,xc,rmajor,0.0D0,br,bz,psi)

C *** Scale EF coil current per turn (proportional to field)

      cptrfp(15) = cptrfp(15) * bv/bz
      cptrfp(16) = cptrfp(16) * bv/bz

      return
      end
