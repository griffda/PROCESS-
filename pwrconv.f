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
C  Module name    : $RCSfile: pwrconv.f,v $
C  Version no.    : $Revision: 3.6 $
C
C  Creation date  : $Date: 2006/05/25 09:27:02 $
C  Creation time  : 
C
C  The SCCS file corresponding to this source file is
C  %P%
C
CSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCSCCS

c______________________________________________________________________
      SUBROUTINE TFPWR(nout,iprint)

c  Subroutine to calculate TF Coil power supply needs
c
c  cpttf  - current per TFC turn (A)
c  jbus   - bus current density (A/m2)
c  tfbusl - TF coil bus length
c  vtfkv  - max voltage to TF coil (kV)
c  tfcmw  - max. power to TF coil (MW)
c
c  Superconducting TF coil power supply needs are calculated in
c  routine TFPWCALL.

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'tfcoil.h'
      INCLUDE 'times.h'
      INCLUDE 'cost.h'
      INCLUDE 'osections.h'

      DOUBLE PRECISION abus,rhobus,ztot,tfbusmw,tfreacmw
      INTEGER nout,iprint

      EXTERNAL oheadr,ovarre,tfpwcall

      if (itfsup.eq.0) then

C *** Non-superconducting TF coils

c  TF coil bus length 
c  Assume power supplies are 5m away

         tfbusl = 300.d0

C  Cross-sectional area of bus
         abus = cpttf/jbus

C  Bus resistance
         rhobus = 2.5d-8 * tfbusl/ abus

C  Bus mass
         tfbusmas = tfbusl * abus * 8000.d0

c  Total maximum impedance
         ztot = (tfno * rhotfleg + rhocp + rhobus)

c  No reactive portion of the voltage is included here - assume 
c  long ramp times

c  Peak voltage and power
         vtfkv = 1.d-3 * ztot * cpttf / tfno

c  Resistive powers

c  TF coil inner legs
         tfcpmw = 1.d-6 * prescp

c  TF coil outer legs
         tflegmw = 1.d-6 * (ritfc/tfno)**2 * rhotfleg * tfno

c  TF coil bus
         tfbusmw = 1.d-6 * cpttf**2 * rhobus

c  TF coil reactive power
c  Set reactive power to 0, since ramp up can be long
c        tfreacmw = 1.d-6 * 1.d9 * estotf/(tohs + tramp)
         tfreacmw = 0.0d0

c  Total power consumption
         tfcmw = tfcpmw + tflegmw + tfbusmw + tfreacmw

      else

c *** Superconducting TF coil option

         call tfpwcall(nout,iprint)
         goto 1000

      end if

      if ((iprint.eq.0).or.(sect13.eq.0)) goto 1000

      call oheadr(nout,'TF Coil Power Conversion')
      call ovarre(nout,'Bus resistance (ohm)','(rhobus)',rhobus)
      call ovarre(nout,'Bus current density (A/m2)','(jbus)',jbus)
      call ovarre(nout,'Bus length - all coils (m)','(tfbusl)',tfbusl)
      call ovarre(nout,'Bus mass (kg)','(tfbusmas)',tfbusmas)
      call ovarre(nout,'Maximum impedance (ohm)','(ztot)',ztot)
      call ovarre(nout,'Peak voltage per coil (kV)','(vtfkv)',vtfkv)
      call ovarre(nout,'Peak power (MW)','(tfcmw)',tfcmw)
      call ovarre(nout,'TF coil inner leg resistive power (MW)',
     +     '(tfcpmw)',tfcpmw)
      call ovarre(nout,'TF coil outer leg resistive power (MW)',
     +     '(tflegmw)',tflegmw)
      call ovarre(nout,'TF coil buswork resistive power','(tfbusmw)',
     +     tfbusmw)
      call ovarre(nout,'TF coil reactive power (MW)','(tfreacmw)',
     +     tfreacmw)

 1000 continue

      return
      end
c______________________________________________________________________
      SUBROUTINE PFPWR(nout,iprint)

c  This subroutine calculates the MVA, power and energy requirements
c  for the PF coil systems.
c  Units are MW and MVA for power terms.
c
c  INPUT :
c
c  (through commons)
c  cpt(i,j)   - current per turn of coil i at (end) time period j (A)
c  cptdin(i)  - max current per turn of coil i (A)
c  ncirt      - total number of PF coils (including OH coil and plasma)
c               plasma is # ncirt, and OH coil is # ncirt-1
c  rmajor     - plasma major radius (m)
c  ric(i)     - maximum current in coil i (a)
c  sxlg(i,j)  - mutual inductance between coil i and j
c               (H x # turns in i x # turns in j)
c  waves(i,j) - normalized current in coil i at time period j
c               time periods are input (sec). they are used in the
c               following order: tramp,tohs,theat,tburn,tqnch
c
c  (through argument list)
c  iprint     - switch for printing
c  nout       - unit number for printing
c
c  OUTPUT :
c  ensxpfm    - max stored energy requirement (J)
c  peakmva    - max total MVA requirement of the pf coils
c  pfcr(i)    - extra resistance for coil i (normally=0) (ohm)
c  psmva(i)   - peak MVA for coil i
c  rcktpm(i)  - peak resistive power for coil i (MW)
c  rcktvm(i)  - peak resistive voltage for coil i (V)
c  vpfmax(min) (i) - max (min) voltage on coil i (V)
c
c  This subroutine checks at the beginning of the flattop for the
c  peak MVA, and at the end of flattop for the peak stored energy.
c
c  The reactive (inductive) components use waves to calculate the
c  dI / dt at the time periods.
c
c  Bus terminology:
c  ----------------
c
c  pfbusl     - bus length for each PF circuit (m)
c  pfbusr(i)  - resistance of bussing for circuit i (ohm)
c  albusa(i)  - section area of aluminium bussing for circuit i (cm**2)
c
c  Power terminology:
c  ------------------
c
c  cktr(i)    - total resistance of circuit i (ohms)
c  rctvm(i)   - max. voltage drop in ciruit i (volts)
c  rctpm(i)   - max. power loss in circuit i (MW)
c
c  powpfr     - resistive power in all circuits at time tim(5) (MW)
c  powpfr2    - resistive power in all circuits at time tim(3) (MW)
c
c  peakmva    - maximum of all inductive and resistive power in all
c               circuits at time tim(3) (MW)
c
c  Other terminology:
c  ------------------
c
c  vpfij      - voltage in circuit j due to change in current from
c               circuit i.
c
c  vpfi(j)    - voltage in circuit j at time, tim(3), due to changes
c               in coil currents.
c
c  powpfii(j) - MVA in circuit j at time, tim(3) due to changes
c               in current

      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'pfcoil.h'
      INCLUDE 'vltcom.h'
      INCLUDE 'pwrcom.h'
      INCLUDE 'build.h'
      INCLUDE 'times.h'
C+**PJK 22/01/97 pfelect.h subsumed into htpwr.h
      INCLUDE 'htpwr.h'
      INCLUDE 'osections.h'

      DOUBLE PRECISION albusa(ngc2),pfbusr(ngc2),cktr(ngc2),
     +     powpfii(ngc2),vpfi(ngc2),psmva(ngc2),pfcr(ngc2),
     +     rcktvm(ngc2),rcktpm(ngc2)
      DOUBLE PRECISION pfbusl,powpfr,pfbuspwr,cptburn,delktim,powpfi,
     +     powpfr2,ensxpf,engx,vpfij,engxpc

      INTEGER i,iprint,nout,ic,ngrpt,ig,ipf,jjpf,jjpf2,jpf

      EXTERNAL oheadr,ovarre

C+**PJK 24/05/06 Added SAVE line

      SAVE pfbuspwr

      do 10 i = 1,ngc2
         powpfii(i) = 0.d0
         cktr(i) = 0.d0
         pfcr(i) = 0.d0
 10   continue

      if (iprint.ne.0) goto 80

c  Bus length
      pfbusl = 8.d0 * rmajor + 140.d0

c  Find power requirements for PF coils at tim(ktim)

c  PF coil resistive power requirements

c  Bussing losses assume aluminium bussing with 100 A/cm**2

      ic = 0
      ngrpt = ngrp
      if (iohcl.ne.0) ngrpt = ngrpt + 1

      srcktpm = 0.d0
      pfbuspwr = 0.d0

      do 20 ig = 1,ngrpt
         ic = ic + ncls(ig)
         albusa(ig) = abs(cptdin(ic)) / 100.d0

c  Include 50% enhancement for welds,joints etc, (g. gorker)
         pfbusr(ig) = 1.5d0 * 2.62d-4 * pfbusl / albusa(ig)

c  Total PF coil resistance (during burn)
         pfcr(ig) = pfclres * 2.d0 * pi * rpf(ic) * abs(rjconpf(ic) /
     +        ( (1.d0-vf(ic))*1.d6*ric(ic)) ) * turns(ic)**2 *
     +        DBLE(ncls(ig))

         cktr(ig) = pfcr(ig) + pfbusr(ig)
         cptburn = cptdin(ic) * curpfb(ic)/ric(ic)
         rcktvm(ig) = abs(cptburn)*cktr(ig)
         rcktpm(ig) = 1.d-6*rcktvm(ig)*abs(cptburn)

c  Compute the sum of resistive power in the PF circuits, kW
         pfbuspwr = pfbuspwr + 1.d-3 * pfbusr(ig) * cptburn**2
         srcktpm = srcktpm + 1.d3*rcktpm(ig)
 20   continue

c  Inductive MVA requirements, and stored energy

      delktim = tohs

c  PF system (including OH solenoid) inductive MVA requirements

      powpfi = 0.d0
      powpfr = 0.d0
      powpfr2 = 0.d0
      ensxpf = 0.d0

      do 30 i = 1,ncirt
         powpfii(i) = 0.d0
         vpfi(i) = 0.d0
 30   continue

      jpf = 0
      do 60 jjpf=1,ngrpt
         do 50 jjpf2 = 1,ncls(jjpf)
            jpf = jpf + 1
            engx = 0.d0
            do 40 ipf = 1,ncirt
               vpfij = sxlg(jpf,ipf) * (cpt(ipf,3)-cpt(ipf,2))/delktim
               vpfi(jpf) = vpfi(jpf) + vpfij
               powpfii(jpf) = powpfii(jpf) + vpfij*cpt(jpf,3)/1.d6
               engx = engx + sxlg(jpf,ipf)*cpt(ipf,5)
 40         continue

c  Compute inductive energy of each PF coil circuit at tim(5)
            engxpc  = 0.5d0 * engx * cpt(jpf,5)
            ensxpf = ensxpf + engxpc
            powpfr = powpfr + turns(jpf) * cpt(jpf,3) * cktr(jjpf)/1.d6
            powpfr2 = powpfr2 +turns(jpf)* cpt(jpf,5) * cktr(jjpf)/1.d6
            powpfi = powpfi + powpfii(jpf)

 50      continue
 60   continue

c  Compute the maximum stored energy and the maximum dissipative
c  energy in all the PF circuits over the entire cycle time, MJ

      ensxpfm = 1.d-6 * ensxpf

c  Check for peak MVA requirements
      peakmva =  max( (powpfr + powpfi), powpfr2)

      vpfskv = 20.d0
      pfckts = dble(ncirt-2) + 6.d0
      spfbusl = pfbusl*pfckts
      acptmax = 0.d0
      spsmva = 0.d0

      do 70 jpf=1,ncirt-1

c  Compute the power supply MVA for each PF circuit
         psmva(jpf) = 1.d-6 * abs (vpfi(jpf)*cptdin(jpf) )

c  Compute the sum of the power supply MVA of the PF circuits
         spsmva = spsmva + psmva(jpf)

c  Compute the average of the maximum currents in the PF circuits, kA
         acptmax = acptmax + 1.d-3 * abs(cptdin(jpf))/pfckts

 70   continue

      goto 1000

c  Output Section

 80   continue

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
