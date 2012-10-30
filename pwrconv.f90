!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfpwr(outfile,iprint)

  !+ad_name  tfpwr
  !+ad_summ  TF coil power supply requirements for resistive coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the power conversion requirements for
  !+ad_desc  resistive TF coils, or calls <CODE>tfpwcall</CODE> if the TF
  !+ad_desc  coils are superconducting.
  !+ad_prob  None
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  tfcoil_variables
  !+ad_call  times.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_call  tfpwcall
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use physics_variables
  use process_output
  use tfcoil_variables

  implicit none

  include 'times.h90'

  !  Arguments

  integer, intent(in) :: outfile,iprint

  !  Local variables

  real(kind(1.0D0)) :: abus,rhobus,ztot,tfbusmw,tfreacmw

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (itfsup == 0) then  !  Non-superconducting TF coils

     !  TF coil bus length (m)
     !  Assume power supplies are 5m away

     tfbusl = 300.0D0

     !  Cross-sectional area of bus
     !  cpttf  - current per TFC turn (A)
     !  jbus   - bus current density (A/m2)

     abus = cpttf/jbus

     !  Bus resistance (ohm)

     rhobus = 2.5D-8 * tfbusl/abus

     !  Bus mass (kg)

     tfbusmas = tfbusl * abus * 8000.0D0

     !  Total maximum impedance

     ztot = tfno*rhotfleg + rhocp + rhobus

     !  No reactive portion of the voltage is included here - assume 
     !  long ramp times

     !  Peak voltage (kV)

     vtfkv = 1.0D-3 * ztot * cpttf/tfno

     !  Resistive powers (MW):

     tfcpmw  = 1.0D-6 * prescp  !  inner legs
     tflegmw = 1.0D-6 * (ritfc/tfno)**2 * rhotfleg * tfno  !  outer legs
     tfbusmw = 1.0D-6 * cpttf**2 * rhobus  !  TF coil bus

     !  TF coil reactive power
     !  Set reactive power to 0, since ramp up can be long
     !     tfreacmw = 1.0D-6 * 1.0D9 * estotf/(tohs + tramp)

     tfreacmw = 0.0D0

     !  Total power consumption (MW)

     tfcmw = tfcpmw + tflegmw + tfbusmw + tfreacmw

  else  !  Superconducting TF coil option

     call tfpwcall(outfile,iprint)
     return

  end if

  !  Output section

  if ((iprint == 0).or.(sect13 == 0)) return

  call oheadr(outfile,'TF Coil Power Conversion')
  call ovarre(outfile,'Bus resistance (ohm)','(rhobus)',rhobus)
  call ovarre(outfile,'Bus current density (A/m2)','(jbus)',jbus)
  call ovarre(outfile,'Bus length - all coils (m)','(tfbusl)',tfbusl)
  call ovarre(outfile,'Bus mass (kg)','(tfbusmas)',tfbusmas)
  call ovarre(outfile,'Maximum impedance (ohm)','(ztot)',ztot)
  call ovarre(outfile,'Peak voltage per coil (kV)','(vtfkv)',vtfkv)
  call ovarre(outfile,'Peak power (MW)','(tfcmw)',tfcmw)
  call ovarre(outfile,'TF coil inner leg resistive power (MW)', &
       '(tfcpmw)',tfcpmw)
  call ovarre(outfile,'TF coil outer leg resistive power (MW)', &
       '(tflegmw)',tflegmw)
  call ovarre(outfile,'TF coil buswork resistive power','(tfbusmw)', &
       tfbusmw)
  call ovarre(outfile,'TF coil reactive power (MW)','(tfreacmw)', &
       tfreacmw)

end subroutine tfpwr

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfpwcall(outfile,iprint)

  !+ad_name  tfpwcall
  !+ad_summ  Calls the TF coil power conversion routine for superconducting coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calls routine <CODE>tfcpwr</CODE> to calculate the power
  !+ad_desc  conversion requirements for superconducting TF coils.
  !+ad_prob  None
  !+ad_call  heat_transport_variables
  !+ad_call  physics_variables
  !+ad_call  tfcoil_variables
  !+ad_call  bldgvol.h90
  !+ad_call  tfcpwr
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use heat_transport_variables
  use physics_variables
  use tfcoil_variables
 
  implicit none

  include 'bldgvol.h90'

  !  Arguments

  integer, intent(in) :: outfile,iprint

  !  Local variables

  real(kind(1.0D0)) :: drarea,ettfmj,itfka

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Stored energy (MJ)

  ettfmj = estotf * 1.0D3

  !  TF coil current (kA)

  itfka = 1.0D-3 * cpttf

  call tfcpwr(outfile,iprint,tfno,ettfmj,itfka,rhotfleg, &
       vtfskv,rmajor,tfckw,tfbusl,drarea,tfcbv,tfacpd)

end subroutine tfpwcall

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfcpwr(outfile,iprint,ntfc,ettfmj,itfka, &
     rptfc,vtfskv,rmajor,tfckw,tfbusl,drarea,tfcbv,tfacpd)

  !+ad_name  tfcpwr
  !+ad_summ  Calculates the TF coil power conversion system parameters
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_args  ntfc : input real : number of tf coils
  !+ad_args  ettfmj : input real : total stored energy of one TF coils, MJ
  !+ad_args  itfka : input real : design current for the TF coils, kA
  !+ad_args  rptfc : input real : resistance of a TF coil, ohms
  !+ad_args  vtfskv : input real : allowable voltage across a TF coil during quench, kV
  !+ad_args  rmajor : input real : plasma major radius, m
  !+ad_args  tfckw : output real : available DC power for charging the TF coils, kW
  !+ad_args  tfbusl : output real : total bus length of the TF coil system, m
  !+ad_args  drarea : output real : approx. area needed for the energy dump resistors, m2
  !+ad_args  tfcbv : output real : approx. vol needed for the TF coil power supplies
  !+ad_argc                        and DC circuit breakers, m3
  !+ad_args  tfacpd : output real : steady state TF coil AC power demand, MW
  !+ad_desc  This routine calculates the TF power conversion system
  !+ad_desc  parameters:  floor space, power supplies, bussing,
  !+ad_desc  coil protection equipment, and the associated controls
  !+ad_desc  and instrumentation. It was originally written by G. Gorker,
  !+ad_desc  FEDC/ORNL, April 1987, modified by J. Galambos in 1991 to
  !+ad_desc  run in TETRA, and included in PROCESS in 1992 by P. C. Shipe.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  process_output
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  16/10/12 PJK Added constants
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use process_output

  implicit none

  !  Arguments

  integer, intent(in) :: outfile, iprint
  real(kind(1.0D0)), intent(in) :: ntfc,ettfmj,itfka,rptfc,vtfskv,rmajor
  real(kind(1.0D0)), intent(out) :: tfckw,tfbusl,drarea,tfcbv,tfacpd

  !  Local variables

  real(kind(1.0D0)) :: albusa,albuswt,djmka,fspc1,fspc2,fspc3,ettfc, &
       ltfth,lptfcs,ncpbkr,ndumpr,nsptfc,ntfbkr,ntfpm,part1,part2, &
       part3,rcoils,rpower,rtfbus,rtfps,r1dump,r1emj,r1ppmw,tchghr, &
       tfackw,tfcfsp,tfcv,tfpmka,tfpmkw,tfpska,tfpsv,tfpmv,tftbv, &
       tftsp,ttfsec,vtfbus,xpower,xpwrmw,ztotal

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ncpbkr = 1.0D0   !  number of TF coils per circuit breaker
  djmka = 0.125D0  !  design current density of TF bus, kA/cm2
  rtfps = 1.05D0   !  rating factor for TF coil power supplies
  fspc1 = 0.15D0   !  floor space coefficient for power supplies
  fspc2 = 0.8D0    !  floor space coefficient for circuit breakers
  fspc3 = 0.4D0    !  floor space coefficient for load centres

  if (rptfc == 0.0D0) then
     tchghr = 4.0D0  !  charge time of the coils, hours
     nsptfc = 1.0D0  !  1.0 = superconducting, 0.0 = resistive
  else
     tchghr = 0.16667D0
     nsptfc = 0.0D0
  end if

  tfacpd = 0.0D0
  ettfc = ntfc*ettfmj  !  stored energy of all TF coils, MJ
  ltfth = 2.0D0*ettfc/itfka**2  !  inductance of all TF coils, Henries

  ntfbkr = ntfc/ncpbkr  !  number of circuit breakers
  lptfcs = ltfth/ntfc  !  inductance per TF coil, Henries
  albusa = itfka/djmka  !  aluminium bus section area, sq cm
  tfbusl = 8.0D0*pi*rmajor + &  !  total TF system bus length, m
       (1.0D0+ntfbkr)*(12.0D0*rmajor+80.0D0) + &
       0.2D0*itfka*sqrt(ntfc*rptfc*1000.0D0)
  albuswt = 2.7D0*albusa*tfbusl/1.0D4
  rtfbus = 2.62D-4*tfbusl/albusa  !  total resistance of TF bus, ohms
  vtfbus = 1000.0D0*itfka*rtfbus  !  total voltage drop across TF bus, volts
  rcoils = ntfc*rptfc  !  total resistance of the TF coils, ohms
  ztotal = rtfbus+rcoils+ltfth/(3600.0D0*tchghr)
  tfcv = 1000.0D0*itfka*ztotal  !  charging voltage for the TF coils, volts
  ntfpm = (itfka * (1.0D0 + nsptfc) )/5.0D0  !  number of TF power modules
  tfpmv = rtfps*tfcv/(1.0D0+nsptfc)  !  TF coil power module voltage, volts
  tfpsv = rtfps*tfcv
  tfpska = rtfps*itfka
  tfpmka = rtfps*itfka/(ntfpm/(1.0D0+nsptfc))  !  TF power module current, kA
  tfpmkw = tfpmv*tfpmka  !  TF power module power, kW
  tfckw = tfpmkw*ntfpm  !  peak AC power needed to charge coils, kW
  tfackw = tfckw/0.9D0
  r1dump = nsptfc*vtfskv*ncpbkr/itfka  !  resistance of dump resistor, ohms
  ttfsec = lptfcs*ncpbkr/(r1dump*nsptfc+rptfc*(1.0D0-nsptfc))  !  time constant, s
  ndumpr = ntfbkr*4.0D0  !  number of dump resistors
  r1ppmw = nsptfc*r1dump*(itfka/2.0D0)**2  !  peak power to a dump resistor
                                           !  during quench, MW
  r1emj = nsptfc*ettfc/(ndumpr+0.0001D0)  !  energy to a dump resistor during quench, MJ
  rpower = (ntfc*rptfc+rtfbus)*itfka**2
  xpower = ltfth/(3600.0D0*tchghr)*itfka**2  !  total TF coil mgf peak inductive
                                             !  power demand, MW

  part1 = fspc1*ntfpm*tfpmkw**0.667D0
  part2 = fspc2*ntfbkr*(vtfskv*itfka)**0.667D0
  part3 = fspc3*(tfackw/(2.4D0*nsptfc+13.8D0*(1.0D0-nsptfc)))**0.667D0
  tfcfsp = part1 + part2 + part3
  drarea = 0.5D0*ndumpr*(1.0D0+r1emj)**0.667D0
  tfcbv = 6.0D0*tfcfsp
  xpwrmw = xpower/0.9D0
  tfacpd = tfacpd+rpower/0.9D0

  tftsp = tfcfsp  !  total TF coil power conversion building floor area, m2
  tftbv = tfcbv   !  total TF coil power conversion building volume, m3

  !  Output section

  if ((iprint == 0).or.(sect13 == 0)) return

  call oheadr(outfile,'Superconducting TF Coil Power Conversion')

  call ovarre(outfile,'TF coil stored energy (MJ)','(ettfmj)',ettfmj)
  call ovarre(outfile,'TF coil current (kA)','(itfka)',itfka)
  call ovarre(outfile,'Number of TF coils','(ntfc)',ntfc)
  call ovarre(outfile,'Maximum voltage across TF coil (kV)','(vtfskv)', &
       vtfskv)
  call ovarre(outfile,'TF coil charge time (hours)','(tchghr)',tchghr)
  call ovarre(outfile,'Total inductance of TF coils (H)','(ltfth)', &
       ltfth)
  call ovarre(outfile,'Total resistance of TF coils (ohm)','(rcoils)', &
       rcoils)
  call ovarre(outfile,'Inductance per TF coil (H)','(lptfcs)',lptfcs)
  call ovarre(outfile,'TF coil charging voltage (V)','(tfcv)',tfcv)
  call ovarre(outfile,'Number of DC circuit breakers','(ntfbkr)', &
       ntfbkr)
  call ovarre(outfile,'Number of dump resistors','(ndumpr)',ndumpr)
  call ovarre(outfile,'Resistance per dump resistor (ohm)','(r1dump)', &
       r1dump)
  call ovarre(outfile,'Dump resistor peak power (MW)','(r1ppmw)', &
       r1ppmw)
  call ovarre(outfile,'Energy supplied per dump resistor (MJ)', &
       '(r1emj)',r1emj)
  call ovarre(outfile,'TF coil L/R time constant (s)','(ttfsec)', &
       ttfsec)
  call ovarre(outfile,'Power supply voltage (V)','(tfpsv)',tfpsv)
  call ovarre(outfile,'Power supply current (kA)','(tfpska)',tfpska)
  call ovarre(outfile,'DC power supply rating (kW)','(tfckw)',tfckw)
  call ovarre(outfile,'AC power for charging (kW)','(tfackw)',tfackw)
  call ovarre(outfile,'TF coil resistive power (MW)','(rpower)',rpower)
  call ovarre(outfile,'TF coil inductive power (MW)','(xpower)',xpower)
  call ovarre(outfile,'Aluminium bus current density (kA/cm2)', &
       '(djmka)',djmka)
  call ovarre(outfile,'Aluminium bus cross-sectional area (cm2)', &
       '(albusa)',albusa)
  call ovarre(outfile,'Total length of TF coil bussing (m)', &
       '(tfbusl)',tfbusl)
  call ovarre(outfile,'Aluminium bus weight (tonnes)','(albuswt)', &
       albuswt)
  call ovarre(outfile,'Total TF coil bus resistance (ohm)','(rtfbus)', &
       rtfbus)
  call ovarre(outfile,'TF coil bus voltage drop (V)','(vtfbus)',vtfbus)
  call ovarre(outfile,'Dump resistor floor area (m2)','(drarea)', &
       drarea)
  call ovarre(outfile,'TF coil power conversion floor space (m2)', &
       '(tfcfsp)',tfcfsp)
  call ovarre(outfile,'TF coil power conv. building volume (m3)', &
       '(tfcbv)',tfcbv)
  call ovarre(outfile,'TF coil AC inductive power demand (MW)', &
       '(xpwrmw)',xpwrmw)
  call ovarre(outfile,'Total steady state AC power demand (MW)', &
       '(tfacpd)',tfacpd)

end subroutine tfcpwr

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine pfpwr(outfile,iprint)

  !+ad_name  pfpwr
  !+ad_summ  PF coil power supply requirements for resistive coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the MVA, power and energy requirements
  !+ad_desc  for the PF coil systems.  Units are MW and MVA for power terms.
  !+ad_desc  The routine checks at the beginning of the flattop for the
  !+ad_desc  peak MVA, and at the end of flattop for the peak stored energy.
  !+ad_desc  The reactive (inductive) components use waves to calculate the
  !+ad_desc  <I>dI/dt</I> at the time periods.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  heat_transport_variables
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  process_output
  !+ad_call  build.h90
  !+ad_call  times.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_hist  20/09/11 PJK Removed dble calls
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added constants
  !+ad_hist  18/10/12 PJK Added pfcoil_variables
  !+ad_hist  29/10/12 PJK Added pf_power_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use heat_transport_variables
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use process_output

  implicit none

  include 'build.h90'
  include 'times.h90'

  !  Arguments

  integer, intent(in) :: outfile, iprint

  !  Local variables

  real(kind(1.0D0)), dimension(ngc2) :: albusa,pfbusr,cktr, &
       powpfii,vpfi,psmva,pfcr,rcktvm,rcktpm
  real(kind(1.0D0)) :: pfbusl,powpfr,cptburn,delktim,powpfi, &
       powpfr2,ensxpf,engx,vpfij,engxpc
  real(kind(1.0D0)), save :: pfbuspwr

  integer :: i,ic,ngrpt,ig,ipf,jjpf,jjpf2,jpf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  powpfii(:) = 0.0D0
  cktr(:) = 0.0D0
  pfcr(:) = 0.0D0

  if (iprint == 0) then

     !  Bus length

     pfbusl = 8.0D0 * rmajor + 140.0D0

     !  Find power requirements for PF coils at tim(ktim)

     !  PF coil resistive power requirements
     !  Bussing losses assume aluminium bussing with 100 A/cm**2

     ic = 0
     ngrpt = ngrp
     if (iohcl /= 0) ngrpt = ngrpt + 1

     srcktpm = 0.0D0
     pfbuspwr = 0.0D0

     do ig = 1,ngrpt
        ic = ic + ncls(ig)

        !  Section area of aluminium bussing for circuit (cm**2)
        !  cptdin : max current per turn of coil (A)

        albusa(ig) = abs(cptdin(ic)) / 100.0D0  

        !  Resistance of bussing for circuit (ohm)
        !  Include 50% enhancement for welds, joints etc, (G. Gorker, ORNL)
        !  pfbusl : bus length for each PF circuit (m)

        pfbusr(ig) = 1.5D0 * 2.62D-4 * pfbusl/albusa(ig)

        !  Total PF coil resistance (during burn)
        !  ric : maximum current in coil (A)

        pfcr(ig) = pfclres * 2.0D0 * pi * rpf(ic) * abs(rjconpf(ic) / &
             ( (1.0D0-vf(ic))*1.0D6*ric(ic)) ) * turns(ic)**2 * ncls(ig)

        cktr(ig) = pfcr(ig) + pfbusr(ig)  !  total resistance of circuit (ohms)
        cptburn = cptdin(ic) * curpfb(ic)/ric(ic)
        rcktvm(ig) = abs(cptburn)*cktr(ig)  !  peak resistive voltage (V)
        rcktpm(ig) = 1.0D-6*rcktvm(ig)*abs(cptburn)  !  peak resistive power (MW)

        !  Compute the sum of resistive power in the PF circuits, kW

        pfbuspwr = pfbuspwr + 1.0D-3 * pfbusr(ig) * cptburn**2
        srcktpm = srcktpm + 1.0D3*rcktpm(ig)
     end do

     !  Inductive MVA requirements, and stored energy

     delktim = tohs

     !  PF system (including OH solenoid) inductive MVA requirements
     !  cpt(i,j) : current per turn of coil i at (end) time period j (A)

     powpfi = 0.0D0
     powpfr = 0.0D0
     powpfr2 = 0.0D0
     ensxpf = 0.0D0

     !  ncirt : total number of PF coils (including OH coil and plasma)
     !          plasma is #ncirt, and OH coil is #(ncirt-1)
     !  sxlg(i,j) : mutual inductance between coil i and j

     do i = 1, ncirt
        powpfii(i) = 0.0D0
        vpfi(i) = 0.0D0
     end do

     jpf = 0
     do jjpf = 1,ngrpt
        do jjpf2 = 1,ncls(jjpf)
           jpf = jpf + 1
           engx = 0.0D0
           do ipf = 1,ncirt

              !  Voltage in circuit jpf due to change in current from
              !  circuit ipf

              vpfij = sxlg(jpf,ipf) * (cpt(ipf,3)-cpt(ipf,2))/delktim

              !  Voltage in circuit jpf at time, tim(3), due to changes
              !  in coil currents

              vpfi(jpf) = vpfi(jpf) + vpfij

              !  MVA in circuit jpf at time, tim(3) due to changes
              !  in current

              powpfii(jpf) = powpfii(jpf) + vpfij*cpt(jpf,3)/1.d6
              engx = engx + sxlg(jpf,ipf)*cpt(ipf,5)

           end do

           !  Compute inductive energy of each PF coil circuit at time
           !  tim(5)

           engxpc = 0.5D0 * engx * cpt(jpf,5)
           ensxpf = ensxpf + engxpc

           !  Resistive power in circuits at times tim(3) and tim(5)
           !  respectively (MW)

           powpfr = powpfr + turns(jpf) * cpt(jpf,3) * cktr(jjpf)/1.0D6
           powpfr2 = powpfr2 +turns(jpf)* cpt(jpf,5) * cktr(jjpf)/1.0D6
           powpfi = powpfi + powpfii(jpf)

        end do
     end do

     !  Compute the maximum stored energy and the maximum dissipative
     !  energy in all the PF circuits over the entire cycle time, MJ

     ensxpfm = 1.0D-6 * ensxpf

     !  Maximum total MVA requirements

     peakmva =  max( (powpfr + powpfi), powpfr2)

     vpfskv = 20.0D0
     pfckts = (ncirt-2) + 6.0D0
     spfbusl = pfbusl*pfckts
     acptmax = 0.0D0
     spsmva = 0.0D0

     do jpf = 1,ncirt-1

        !  Power supply MVA for each PF circuit

        psmva(jpf) = 1.0D-6 * abs (vpfi(jpf)*cptdin(jpf) )

        !  Sum of the power supply MVA of the PF circuits

        spsmva = spsmva + psmva(jpf)

        !  Average of the maximum currents in the PF circuits, kA

        acptmax = acptmax + 1.0D-3 * abs(cptdin(jpf))/pfckts

     end do

  else

     !  Output Section

     if (sect13 == 0) return

     call oheadr(outfile,'PF Coil Power Conversion')
     call ovarre(outfile,'Number of PF coil circuits','(pfckts)',pfckts)
     call ovarre(outfile,'Total power supply MVA for PF circuits', &
          '(spsmva)',spsmva)
     call ovarre(outfile,'Av. max curr/turn of PF coil circuits (kA)', &
          '(acptmax)',acptmax)
     call ovarre(outfile,'Total PF coil circuit bus length (m)', &
          '(spfbusl)',spfbusl)
     call ovarre(outfile,'Total PF coil bus resistive power (kW)', &
          '(pfbuspwr)',pfbuspwr)
     call ovarre(outfile,'Total PF coil resistive power (kW)', &
          '(srcktpm)',srcktpm)
     call ovarre(outfile,'Maximum PF coil voltage (kV)','(vpfskv)',vpfskv)
     call ovarre(outfile,'Max stored energy in PF coil circuits (MJ)', &
          '(ensxpfm)',ensxpfm)

  end if

end subroutine pfpwr
