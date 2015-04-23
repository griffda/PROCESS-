! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module power_module

  !+ad_name  power_module
  !+ad_summ  Module containing heat/power transport and power balance routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  tfpwr
  !+ad_cont  pfpwr
  !+ad_cont  acpow
  !+ad_cont  power1
  !+ad_cont  power2
  !+ad_cont  cryo
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  power supply requirements, heat transport system parameters
  !+ad_desc  and the power balance for a fusion power plant.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  buildings_variables
  !+ad_call  constants
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_module
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  pf_power_variables
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  structure_variables
  !+ad_call  times_variables
  !+ad_call  tfcoil_variables
  !+ad_hist  30/10/12 PJK Initial version of module
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added buildings_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Added cost_variables
  !+ad_hist  17/12/14 PJK Added error_handling
  !+ad_hist  23/04/15 MDK Removed fhole
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use constants
  use cost_variables
  use current_drive_variables
  use error_handling
  !use fwbs_module
  use fwbs_variables
  use heat_transport_variables
  use pf_power_variables
  use pfcoil_variables
  use physics_variables
  use process_output
  use structure_variables
  use tfcoil_variables
  use times_variables

  implicit none

  private
  public :: tfpwr, pfpwr, acpow, power1, power2

  !  Precision variable
  integer, parameter :: double = 8

  !  Local variables
  real(kind=double) :: htpmwe_fw, htpmwe_blkt, htpmwe_shld, htpmwe_div, htpmw_mech
  real(kind=double) :: pthermdiv, pthermfw, pthermblkt, pthermshld
  real(kind=double) :: ppumpmw, pcoresystems
  
  !  Primary power to divertor factor
  integer, private :: iprimdiv

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tfpwr(outfile,iprint)

    !+ad_name  tfpwr
    !+ad_summ  TF coil power supply requirements for resistive coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  tfpwcall
    !+ad_cont  tfcpwr
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output (1=yes)
    !+ad_desc  This routine calculates the power conversion requirements for
    !+ad_desc  resistive TF coils, or calls <CODE>tfpwcall</CODE> if the TF
    !+ad_desc  coils are superconducting.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_call  tfpwcall
    !+ad_hist  01/08/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

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

       !  No reactive portion of the voltage is included here - assume long ramp times
       !  Peak voltage (kV)
       vtfkv = 1.0D-3 * ztot * cpttf/tfno

       !  Resistive powers (MW):
       tfcpmw  = 1.0D-6 * prescp  !  inboard legs
       tflegmw = 1.0D-6 * (ritfc/tfno)**2 * rhotfleg * tfno  !  outboard legs
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
    if (iprint == 0) return

    call oheadr(outfile,'TF Coil Power Conversion')
    call ovarre(outfile,'Bus resistance (ohm)','(rhobus)',rhobus)
    call ovarre(outfile,'Bus current density (A/m2)','(jbus)',jbus)
    call ovarre(outfile,'Bus length - all coils (m)','(tfbusl)',tfbusl)
    call ovarre(outfile,'Bus mass (kg)','(tfbusmas)',tfbusmas)
    call ovarre(outfile,'Maximum impedance (ohm)','(ztot)',ztot)
    call ovarre(outfile,'Peak voltage per coil (kV)','(vtfkv)',vtfkv)
    call ovarre(outfile,'Peak power (MW)','(tfcmw..)',tfcmw)
    call ovarre(outfile,'TF coil inboard leg resistive power (MW)', &
         '(tfcpmw)',tfcpmw)
    call ovarre(outfile,'TF coil outboard leg resistive power (MW)', &
         '(tflegmw)',tflegmw)
    call ovarre(outfile,'TF coil buswork resistive power','(tfbusmw)', &
         tfbusmw)
    call ovarre(outfile,'TF coil reactive power (MW)','(tfreacmw)', &
         tfreacmw)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine tfpwcall(outfile,iprint)

      !+ad_name  tfpwcall
      !+ad_summ  Calls the TF coil power conversion routine for
      !+ad_summ  superconducting coils
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  P C Shipe, ORNL
      !+ad_cont  N/A
      !+ad_args  outfile : input integer : output file unit
      !+ad_args  iprint : input integer : switch for writing to output (1=yes)
      !+ad_desc  This routine calls routine <CODE>tfcpwr</CODE> to calculate
      !+ad_desc  the power conversion requirements for superconducting TF coils.
      !+ad_prob  None
      !+ad_call  tfcpwr
      !+ad_hist  01/08/11 PJK Initial F90 version
      !+ad_hist  15/10/12 PJK Added physics_variables
      !+ad_hist  18/10/12 PJK Added tfcoil_variables
      !+ad_hist  30/10/12 PJK Added heat_transport_variables
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

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
      !+ad_summ  for superconducting coils
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  P C Shipe, ORNL
      !+ad_cont  N/A
      !+ad_args  outfile : input integer : output file unit
      !+ad_args  iprint : input integer : switch for writing to output (1=yes)
      !+ad_args  ntfc : input real : number of TF coils
      !+ad_args  ettfmj : input real : total stored energy of one TF coils, MJ
      !+ad_args  itfka : input real : design current for the TF coils, kA
      !+ad_args  rptfc : input real : resistance of a TF coil, ohms
      !+ad_args  vtfskv : input real : allowable voltage across a TF coil
      !+ad_argc                        during quench, kV
      !+ad_args  rmajor : input real : plasma major radius, m
      !+ad_args  tfckw : output real : available DC power for charging the
      !+ad_argc                        TF coils, kW
      !+ad_args  tfbusl : output real : total bus length of the TF coil
      !+ad_argc                         system, m
      !+ad_args  drarea : output real : approx. area needed for the energy dump
      !+ad_argc                         resistors, m2
      !+ad_args  tfcbv : output real : approx. vol needed for the TF coil power
      !+ad_argc                        supplies and DC circuit breakers, m3
      !+ad_args  tfacpd : output real : steady state TF coil AC power demand, MW
      !+ad_desc  This routine calculates the TF power conversion system
      !+ad_desc  parameters:  floor space, power supplies, bussing,
      !+ad_desc  coil protection equipment, and the associated controls
      !+ad_desc  and instrumentation. It was originally written by G. Gorker,
      !+ad_desc  FEDC/ORNL, April 1987, modified by J. Galambos in 1991 to
      !+ad_desc  run in TETRA, and included in PROCESS in 1992 by P. C. Shipe.
      !+ad_prob  None
      !+ad_call  oheadr
      !+ad_call  ovarre
      !+ad_hist  01/08/11 PJK Initial F90 version
      !+ad_hist  09/10/12 PJK Modified to use new process_output module
      !+ad_hist  16/10/12 PJK Added constants
      !+ad_hist  08/04/13 PJK Comment changes; xpower units changed from MW to MVA
      !+ad_hist  15/04/13 PJK Comment changes
      !+ad_hist  08/05/14 PJK Tidied up comments
      !+ad_hist  19/06/14 PJK Removed sect?? flags
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
         nsptfc = 1.0D0  !  superconducting (1.0 = superconducting, 0.0 = resistive)
      else
         tchghr = 0.16667D0 !  charge time of the coils, hours
         nsptfc = 0.0D0  !  resistive (1.0 = superconducting, 0.0 = resistive)
      end if

      !  Total steady state TF coil AC power demand (summed later)

      tfacpd = 0.0D0

      !  Stored energy of all TF coils, MJ

      ettfc = ntfc*ettfmj

      !  Inductance of all TF coils, Henries

      ltfth = 2.0D0*ettfc/itfka**2

      !  Number of circuit breakers

      ntfbkr = ntfc/ncpbkr

      !  Inductance per TF coil, Henries

      lptfcs = ltfth/ntfc

      !  Aluminium bus section area, sq cm

      albusa = itfka/djmka

      !  Total TF system bus length, m

      tfbusl = 8.0D0*pi*rmajor + &
           (1.0D0+ntfbkr)*(12.0D0*rmajor+80.0D0) + &
           0.2D0*itfka*sqrt(ntfc*rptfc*1000.0D0)

      !  Aluminium bus weight, tonnes

      albuswt = 2.7D0*albusa*tfbusl/1.0D4

      !  Total resistance of TF bus, ohms

      rtfbus = 2.62D-4*tfbusl/albusa

      !  Total voltage drop across TF bus, volts

      vtfbus = 1000.0D0*itfka*rtfbus

      !  Total resistance of the TF coils, ohms

      rcoils = ntfc*rptfc

      !  Total impedance, ohms

      ztotal = rtfbus+rcoils+ltfth/(3600.0D0*tchghr)

      !  Charging voltage for the TF coils, volts

      tfcv = 1000.0D0*itfka*ztotal

      !  Number of TF power modules

      ntfpm = (itfka * (1.0D0 + nsptfc) )/5.0D0

      !  TF coil power module voltage, volts

      tfpmv = rtfps*tfcv/(1.0D0+nsptfc)

      !  TF coil power supply voltage, volts

      tfpsv = rtfps*tfcv

      !  Power supply current, kA

      tfpska = rtfps*itfka

      !  TF power module current, kA

      tfpmka = rtfps*itfka/(ntfpm/(1.0D0+nsptfc))

      !  TF power module power, kW

      tfpmkw = tfpmv*tfpmka

      !  Available DC power for charging the TF coils, kW

      tfckw = tfpmkw*ntfpm

      !  Peak AC power needed to charge coils, kW

      tfackw = tfckw/0.9D0

      !  Resistance of dump resistor, ohms

      r1dump = nsptfc*vtfskv*ncpbkr/itfka

      !  Time constant, s

      ttfsec = lptfcs*ncpbkr/(r1dump*nsptfc+rptfc*(1.0D0-nsptfc))

      !  Number of dump resistors

      ndumpr = ntfbkr*4.0D0

      !  Peak power to a dump resistor during quench, MW

      r1ppmw = nsptfc*r1dump*(itfka/2.0D0)**2

      !  Energy to dump resistor during quench, MJ

      r1emj = nsptfc*ettfc/(ndumpr+0.0001D0)

      !  Total TF coil peak resistive power demand, MVA

      rpower = (ntfc*rptfc+rtfbus)*itfka**2

      !  Total TF coil peak inductive power demand, MVA

      xpower = ltfth/(3600.0D0*tchghr)*itfka**2

      !  Building space:

      !  Power modules floor space, m2

      part1 = fspc1*ntfpm*tfpmkw**0.667D0

      !  Circuit breakers floor space, m2

      part2 = fspc2*ntfbkr*(vtfskv*itfka)**0.667D0

      !  Load centres floor space, m2

      part3 = fspc3*(tfackw/(2.4D0*nsptfc+13.8D0*(1.0D0-nsptfc)))**0.667D0

      !  Power conversion building floor area, m2

      tfcfsp = part1 + part2 + part3

      !  Dump resistor floor area, m2

      drarea = 0.5D0*ndumpr*(1.0D0+r1emj)**0.667D0

      !  Total TF coil power conversion building volume, m3

      tfcbv = 6.0D0*tfcfsp

      !  TF coil AC inductive power demand, MW

      xpwrmw = xpower/0.9D0

      !  Total steady state AC power demand, MW

      tfacpd = tfacpd + rpower/0.9D0

      !  Total TF coil power conversion building floor area, m2

      tftsp = tfcfsp

      !  Total TF coil power conversion building volume, m3

      tftbv = tfcbv

      !  Output section

      if (iprint == 0) return

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
      call ovarre(outfile,'TF coil inductive power (MVA)','(xpower)',xpower)
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

  end subroutine tfpwr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pfpwr(outfile,iprint)

    !+ad_name  pfpwr
    !+ad_summ  PF coil power supply requirements
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output (1=yes)
    !+ad_desc  This routine calculates the MVA, power and energy requirements
    !+ad_desc  for the PF coil systems.  Units are MW and MVA for power terms.
    !+ad_desc  The routine checks at the beginning of the flattop for the
    !+ad_desc  peak MVA, and at the end of flattop for the peak stored energy.
    !+ad_desc  The reactive (inductive) components use waves to calculate the
    !+ad_desc  <I>dI/dt</I> at the time periods.
    !+ad_prob  None
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
    !+ad_hist  04/02/13 PJK Comment change
    !+ad_hist  24/04/14 PJK Calculation always proceeds irrespective of iprint
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

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

             !  Voltage in circuit jpf due to change in current from circuit ipf
             vpfij = sxlg(jpf,ipf) * (cpt(ipf,3)-cpt(ipf,2))/delktim

             !  Voltage in circuit jpf at time, tim(3), due to changes in coil currents
             vpfi(jpf) = vpfi(jpf) + vpfij

             !  MVA in circuit jpf at time, tim(3) due to changes in current
             powpfii(jpf) = powpfii(jpf) + vpfij*cpt(jpf,3)/1.d6
             engx = engx + sxlg(jpf,ipf)*cpt(ipf,5)

          end do

          !  Compute inductive energy of each PF coil circuit at time tim(5)
          engxpc = 0.5D0 * engx * cpt(jpf,5)
          ensxpf = ensxpf + engxpc

          !  Resistive power in circuits at times tim(3) and tim(5) respectively (MW)
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
    
    !  PF Power requirements
    !  Wall plug power (MW)
    pfwp = pohmmw / etapsu
    
    !  Secondary waste heat (MW)
    pfsec = pfwp - pohmmw

    !  Output Section
    if (iprint == 0) return

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

  end subroutine pfpwr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acpow(outfile,iprint)

    !+ad_name  acpow
    !+ad_summ  AC power requirements
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  P C Shipe, ORNL
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output (1=yes)
    !+ad_desc  The routine was drastically shortened on 23/01/90 (ORNL) from the
    !+ad_desc  original TETRA routine to provide only the total power needs for
    !+ad_desc  the plant. Included in STORAC in January 1992 by P.C. Shipe.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  --/--/92 PJK Initial PROCESS version
    !+ad_hist  20/01/97 PJK Fixed error in pheatmw calculation, removed
    !+ad_hisc               assignment of htpmw, and tidied up coding
    !+ad_hist  22/01/97 PJK Subsumed heattr.h, heatrinp.h and pfelect.h into
    !+ad_hisc               htpwr.h
    !+ad_hist  27/07/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  29/10/12 PJK Added pf_power_variables
    !+ad_hist  30/10/12 PJK Added heat_transport_variables
    !+ad_hist  05/02/13 PJK Clarified MGF output
    !+ad_hist  27/03/13 PJK MGF power only included if iscenr /= 2
    !+ad_hist  17/04/13 PJK Removed 0.05*pacpmw contribution to fcsht
    !+ad_hist  21/05/14 PJK Added ignite comment
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: basemw,bdvmw,crymw,pheatingmw,pkwpm2,ppfmw,ptfmw

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Power to TF coil power supplies, MW
    ptfmw = tfacpd

    ! Power to PF coil power supplies, MW
    ppfmw = 1.0D-3 * srcktpm
    if (iscenr == 2) ppfmw = ppfmw + peakmva

    !  Power to plasma heating supplies, MW
    pheatingmw = pinjwp  !  Should be zero if ignite==1

    !  Power to cryogenic comp. motors, MW
    crymw = crypmw

    !  Facility base load, MW (loads not dependent on floor area)
    basemw = baseel * 1.0D-6

    !  Power needed per unit floor area, kW/m2
    pkwpm2 = pwpm2 * 1.0D-3

    !  Power to divertor coil supplies, MW
    bdvmw = 0.0D0

    !  Total pulsed power system load, MW
    pacpmw = ppfmw + bdvmw + ptfmw + crymw + vachtmw + &
         htpmw + trithtmw + pheatingmw + basemw + efloor*pkwpm2/1000.0D0

    !  Add contribution from motor-generator flywheels if these are part of
    !  the PF coil energy storage system
    if (iscenr /= 2) pacpmw = pacpmw + fmgdmw

    !  Total baseline power to facility loads, MW
    fcsht  = basemw + efloor*pkwpm2/1000.0D0

    !  Estimate of the total low voltage power, MW
    tlvpmw = fcsht + trithtmw + htpmw + vachtmw + 0.5D0*(crymw+ppfmw)

    if (iprint == 0) return

    !  Output section
    call oheadr(outfile,'AC Power')

    call ovarre(outfile,'Facility base load (MW)','(basemw)',basemw)
    call ovarre(outfile,'Divertor coil power supplies (MW)','(bdvmw)',bdvmw)
    call ovarre(outfile,'Cryogenic comp motors (MW)','(crymw)',crymw)
    call ovarre(outfile,'MGF (motor-generator flywheel) units (MW)', '(fmgdmw)',fmgdmw)
    call ovarin(outfile,'Primary coolant pumps (MW)', '(coolwh)',coolwh)
    call ovarre(outfile,'Primary coolant pumps (MW)', '(htpmw..)',htpmw)
    call ovarre(outfile,'PF coil power supplies (MW)','(ppfmw)',ppfmw)
    call ovarre(outfile,'Power/floor area (kW/m2)','(pkwpm2)',pkwpm2)
    call ovarre(outfile,'TF coil power supplies (MW)','(ptfmw)',ptfmw)
    call ovarre(outfile,'Plasma heating supplies (MW)','(pheatingmw)', &
         pheatingmw)
    call ovarre(outfile,'Tritium processing (MW)','(trithtmw..)',trithtmw)
    call ovarre(outfile,'Vacuum pumps  (MW)','(vachtmw..)',vachtmw)

    call oblnkl(outfile)

    call ovarre(outfile,'Total pulsed power (MW)','(pacpmw)',pacpmw)
    call ovarre(outfile,'Total base power reqd at all times (MW)', &
         '(fcsht)',fcsht)
    call ovarre(outfile,'Total low voltage power (MW)','(tlvpmw)',tlvpmw)

  end subroutine acpow

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine power1

    !+ad_name  power1
    !+ad_summ  Calculates the first part of the heat transport
    !+ad_summ  and plant power balance constituents
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the first part of the heat transport
    !+ad_desc  and plant power balance constituents.
    !+ad_prob  None
    !+ad_call  cryo
    !+ad_call  plant_thermal_efficiency
    !+ad_hist  01/08/11 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  29/10/12 PJK Added structure_variables
    !+ad_hist  29/10/12 PJK Added pf_power_variables
    !+ad_hist  30/10/12 PJK Added heat_transport_variables
    !+ad_hist  17/04/13 PJK Changed priheat to pthermmw in rnphx calculation
    !+ad_hist  17/04/13 PJK Added iprimnloss switch for pnucloss contribution
    !+ad_hisc               to primary heating
    !+ad_hist  21/05/14 PJK Added ignite clauses
    !+ad_hist  22/05/14 PJK Name changes to power quantities; added pohmmw
    !+ad_hisc               to pfwdiv
    !+ad_hist  04/06/14 PJK New power flow model added
    !+ad_hist  17/06/14 PJK Corrections to pfwdiv, priheat
    !+ad_hist  19/06/14 PJK Simplified pinjwp calculation
    !+ad_hist  21/08/14 PJK Revised new power flow model
    !+ad_hist  28/08/14 PJK Corrections to etath fitted formulae
    !+ad_hist  06/10/14 PJK Added orbit loss power to pfwdiv, pinjwp
    !+ad_hist  22/10/14 PJK Corrected orbit loss power usage
    !+ad_hist  04/11/14 PJK Corrected pnucblkt(*emult) usage
    !+ad_hist  17/11/14 PJK Added palpfwmw to first wall thermal power
    !+ad_hist  10/12/14 PJK Replaced real rnphx to integer nphx (with
    !+ad_hisc               different scaling)
    !+ad_hist  06/03/15 JM  Removed ipowerflow
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !------------------------------------------------------------------------------------
    !- Collate pumping powers
    !------------------------------------------------------------------------------------

    !  Account for pump electrical inefficiencies. The coolant pumps are not assumed to be
    !  100% efficient so the electric power to run them is greater than the power deposited 
    !  in the coolant.  The difference should be lost as secondary heat.
    htpmwe_fw = htpmw_fw / etahtp
    htpmwe_blkt = htpmw_blkt / etahtp
    htpmwe_shld = htpmw_shld / etahtp
    htpmwe_div = htpmw_div / etahtp
    
    ! Total mechanical pump power (deposited in coolant)
    htpmw_mech = htpmw_fw + htpmw_blkt + htpmw_shld + htpmw_div

    !  Total heat transport system input electrical power (MW)
    htpmw = htpmwe_fw + htpmwe_blkt + htpmwe_shld + htpmwe_div

    !  Heat lost through pump power inefficiencies (MW)
    htpsecmw = htpmw - htpmw_mech

    !  Total power deposited in first wall coolant (MW)
    pthermfw = pnucfw + pradfw + htpmw_fw + porbitlossmw + palpfwmw + nbshinemw

    !  Total power deposited in blanket coolant (MW) (energy multiplication in pnucblkt already)
    pthermblkt = pnucblkt + htpmw_blkt

    !  Total power deposited in shield coolant (MW)
    pthermshld = pnucshld + htpmw_shld

    !  Total thermal power deposited in divertor coolant (MW)
    !  = (conduction to divertor, less radiation) + (neutron and radiation power)
    !  using pdivt as calculated in physics.f90
    pthermdiv = pdivt + (pnucdiv + praddiv) + htpmw_div

    !  Heat removal from first wall and divertor (MW) (only used in costs.f90)
    pfwdiv = pthermfw + pthermdiv

    !  Thermal to electric efficiency
    call plant_thermal_efficiency(etath)

    !  Primary (high-grade) thermal power, available for electricity generation.  Switch iprimshld
    !  is 1 or 0, is user choice on whether the shield thermal power goes to primary or secondary heat
    if (secondary_cycle == 0) then
    
		!  Primary thermal power (MW)
		pthermmw = pthermfw + pthermblkt + iprimshld*pthermshld
		
		!  Secondary thermal power deposited in divertor (MW)
		psecdiv = pthermdiv
		
		! Divertor primary/secondary power switch value
		iprimdiv = 0
		
	else
		!  Primary thermal power (MW)
		pthermmw = pthermfw + pthermblkt + iprimshld*pthermshld + pthermdiv
		
		!  Secondary thermal power deposited in divertor (MW)
		psecdiv = 0.0D0
		
		! Divertor primary/secondary power switch value
		iprimdiv = 1
		
	end if

    !  Secondary thermal power deposited in shield
    psecshld = pthermshld * (1-iprimshld)

    !  Secondary thermal power lost to HCD apparatus and diagnostics
    psechcd = pnuchcd + pradhcd

    !  Number of primary heat exchangers
    nphx = ceiling(pthermmw/1000.0D0)

    !  Secondary heat (some of it... rest calculated in POWER2)
    !  Wall plug injection power
    if (ignite == 0) then
	   pinjwp = (pinjmw + porbitlossmw + palpfwmw)/etacd
	else
       pinjwp = 0.0D0
    end if

    !  Waste injection power 
    if (ignite == 0) then
       pinjht = pinjwp - pinjmw - porbitlossmw - palpfwmw
    else
       pinjht = 0.0D0
    end if

    !  Cryogenic power
    if ((itfsup /= 1).and.(ipfres == 1)) then  !  no superconducting coils
       helpow = 0.0D0
    else
       call cryo(itfsup, ipfres, tfsai, coldmass, ptfnuc, ensxpfm, tpulse, cpttf, tfno, helpow)
    end if

    !  Use 13% of ideal Carnot efficiency to fit J. Miller estimate
    crypmw = 1.0D-6 * (293.0D0 - tmpcry)/(0.13D0*tmpcry) * helpow

  end subroutine power1

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine power2(outfile,iprint)

    !+ad_name  power2
    !+ad_summ  Calculates the remainder of the heat transport
    !+ad_summ  and plant power balance constituents
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output (1=yes)
    !+ad_desc  This routine calculates the rest of the heat transport
    !+ad_desc  and plant power balance constituents, not already calculated in
    !+ad_desc  <A HREF="acpow.html">ACPOW</A> or <A HREF="power1.html">POWER1</A>.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_call  ovarrf
    !+ad_hist  23/01/97 PJK Initial version
    !+ad_hist  10/09/97 PJK Removed IF-statement that bypassed coding if iprint=1
    !+ad_hist  15/06/04 PJK Added use of IPRIMHTP, added HTPMW to PRECIR
    !+ad_hist  22/05/07 PJK Added hydrogen plant power requirements
    !+ad_hist  01/08/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added fwbs_module
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  30/10/12 PJK Added heat_transport_variables
    !+ad_hist  17/04/13 PJK Corrected precir, psecht, ctht
    !+ad_hist  17/04/13 PJK Added iprimnloss switch for pnucloss contribution
    !+ad_hisc               to secondary heating
    !+ad_hist  11/06/13 PJK Added output section on recirculating power
    !+ad_hist  04/06/14 PJK New power flow model added
    !+ad_hist  16/06/14 PJK Modified various labels to prevent duplicate outputs
    !+ad_hist  17/06/14 PJK Removed blktmodel from ipowerflow if-statement
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  27/08/14 PJK Modifications for new power flow model
    !+ad_hist  10/09/14 PJK Added power balance outputs
    !+ad_hist  22/10/14 PJK Minor mods to outputs
    !+ad_hist  04/11/14 PJK Corrected pnucblkt emult factor
    !+ad_hist  17/11/14 PJK Added palpfwmw to first wall power balance
    !+ad_hist  18/11/14 PJK Corrected input power when ignite=1
    !+ad_hist  10/12/14 PJK Replaced real rnphx with integer nphx;
    !+ad_hisc               deleted ctht, rnihx
    !+ad_hist  17/12/14 PJK Added tturb to output
    !+ad_hist  18/03/15 JM  Made changes in line with fwbs refactor
    !+ad_hist  02/04/15 JM  Pnetelmw doesn't scale on last run through
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)) :: cirpowfr, primsum, pinj, secsum, othermw, rejected_main
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Centrepost coolant pump power (ST)
    if (itart == 1) then
		ppumpmw = 1.0D-6 * ppump
	else
		ppumpmw = 0.0D0
	end if

    !  Facility heat removal (fcsht calculated in ACPOW)
    fachtmw = fcsht

    !  Hydrogen plant powers
    if (ihplant == 0) then
       helecmw = 0.0D0
       hthermmw = 0.0D0
       hpower = 0.0D0
    else if (ihplant == 1) then
       hthermmw = 0.0D0
       hpower = etahlte * helecmw
    else if (ihplant == 2) then
       hthermmw = 0.48D0 * helecmw
       hpower = etahhten * helecmw
    else if (ihplant == 3) then
       hthermmw = 0.19D0 * helecmw
       hpower = etahhtex * helecmw
    else
       helecmw = 0.0D0
       hpower = etahth * hthermmw
    end if

    !  Electrical power consumed by fusion power core systems
    !  (excluding heat transport pumps and auxiliary injection power system)
    !  Added pfwp (waste heat from PF coil ohmic heating)
    pcoresystems = crypmw + fachtmw + helecmw + ppumpmw + tfacpd + trithtmw + vachtmw + pfwp

    !  Total secondary heat
    !  (total low-grade heat rejected - does not contribute to power conversion cycle)
    !  Included ptfnuc
    psechtmw = pcoresystems + pinjht + htpsecmw + hthermmw + psecdiv + psecshld + psechcd + ptfnuc

    !  Calculate powers relevant to a power-producing plant
    if (ireactor == 1) then

       !  Gross electric power
       pgrossmw = (pthermmw-hthermmw) * etath

       !  Total recirculating power
       precircmw = pcoresystems + pinjwp + htpmw

       !  Net electric power
       pnetelmw = pgrossmw - precircmw

       !  Scaling to prevent negative pnetelmw
       ! Do NOT rescale if this is the last run through.
       if ( (pnetelmw < 1.0D0).and.(ipnet == 0).and.(iprint==0)) then
          pnetelmw = 1.0D0 / ( 1.0D0 + abs(pnetelmw-1.0D0))
       end if

       !  Recirculating power fraction
       cirpowfr = (pgrossmw - pnetelmw) / pgrossmw

    end if

    if (iprint == 0) return

    !  Output section

    call oheadr(outfile,'Plant Power / Heat Transport Balance')
    ! MDK Add warning for negative net electric
    if (pnetelmw < 0) then
        call ocmmnt(outfile, 'WARNING: Calculated net electric power is negative')
        call ocmmnt(outfile, '--------------------------------------------------')
    end if
    
    call osubhd(outfile,'Assumptions :')

    call ovarre(outfile,'Neutron power multiplication in blanket', '(emult)', emult)
    call ovarre(outfile, 'Divertor area fraction of whole toroid surface', '(fdiv)', fdiv)
    call ovarre(outfile,'H/CD apparatus + diagnostics area fraction', '(fhcd)', fhcd)
    call ovarre(outfile,'First wall area fraction ', '(1-fdiv-fhcd)', 1.0D0-fdiv-fhcd)
    
    if ((secondary_cycle == 0).or.(secondary_cycle == 1)) then
        call ovarre(outfile, 'Coolant pump power / non-pumping thermal power in first wall', '(fpumpfw)', fpumpfw)
        call ovarre(outfile, 'Coolant pump power / non-pumping thermal power in blanket', '(fpumpblkt)', fpumpblkt)
    end if
    
    call ovarre(outfile, 'Coolant pump power / non-pumping thermal power in shield', '(fpumpshld)', fpumpshld)
    call ovarre(outfile, 'Coolant pump power / non-pumping thermal power in divertor', '(fpumpdiv)',fpumpdiv)
    call ovarre(outfile, 'Electrical efficiency of heat transport coolant pumps', '(etahtp)', etahtp)
    call ovarin(outfile, 'Switch for destination of divertor thermal power (1 = power conv. cycle)', '(iprimdiv)', iprimdiv)
    call ovarin(outfile, 'Switch for destination of shield thermal power (1 = power conv. cycle)', '(iprimshld)', iprimshld)

    if (ireactor == 1) then
        if (secondary_cycle == 0) then
            call osubhd(outfile,'Power conversion cycle efficiency model: '// &
                'efficiency set according to blanket type (div power to secondary)')
        else if (secondary_cycle == 1) then
			call osubhd(outfile,'Power conversion cycle efficiency model: '// &
                'efficiency set according to blanket type (div power to primary)')
            call ovarrf(outfile, 'Thermal to electric conversion efficiency of the power conversion cycle', & 
			    '(etath)', etath)                
            call ovarrf(outfile, 'Thermal to electric conversion efficiency of the power conversion cycle', & 
			    '(etath)', etath)                
        else if (secondary_cycle == 2) then
            call osubhd(outfile,'Power conversion cycle efficiency model: user-defined efficiency')
            call ovarrf(outfile, 'Thermal to electric conversion efficiency of the power conversion cycle', & 
			    '(etath)', etath)            
        else if (secondary_cycle == 3) then
            call osubhd(outfile,'Power conversion cycle efficiency model: steam Rankine cycle')
        else
            call osubhd(outfile,'Power conversion cycle efficiency model: supercritical CO2 cycle')
        end if
   
        if (secondary_cycle > 2) then
            call ovarrf(outfile, 'Coolant temperature at turbine inlet (K)', '(tturb)', tturb)
        end if
   
!        call ovarrf(outfile, 'Thermal to electric conversion efficiency of the power conversion cycle', & 
!			'(etath)', etath)
!        call ovarre(outfile, '(Input) Balance of plant recirculating power fraction', '(fauxbop)', fauxbop)
    end if

    call oblnkl(outfile)
    call ocmmnt(outfile, 'Power Balance for Reactor (across vacuum vessel boundary) - Detail')
    call ocmmnt(outfile, '------------------------------------------------------------------')

!    call ovarre(outfile, 'Fusion power (MW)', '(powfmw.)',powfmw)

    if (ignite == 0) then
        pinj = pinjmw
!        call ovarre(outfile, 'Injected heating/current drive power (MW)', '(pinjmw.)', pinjmw)
    else
        pinj = 0.0D0
    end if

    !call ovarre(outfile, 'Power from energy multiplication in blanket (MW)','', pnucblkt*(1.0D0 - 1.0D0/emult))
    !call ovarre(outfile, 'Power deposited in coolant by pump (MW)','', htpmw_fw + htpmw_blkt + htpmw_shld + htpmw_div)
    !call ovarre(outfile, 'Power deposited in coolant by pump (MW)','(htpmw_mech)', htpmw_mech)
    !call ovarre(outfile, 'Total power entering fusion power core (MW)','', &
    !        powfmw + pinj + pnucblkt*(1.0D0 - 1.0D0/emult) + htpmw_fw + htpmw_blkt + htpmw_shld + htpmw_div)
    !call ovarre(outfile, 'Total power entering fusion power core (MW)','', &
    !         powfmw + pinj + pnucblkt*(1.0D0 - 1.0D0/emult) + htpmw_mech)

    primsum = 0.0D0 ; secsum = 0.0D0

    call oblnkl(outfile)
    write(outfile,'(t45,a)') 'High-grade             Low-grade              Total'
    write(outfile,'(t46,a)') 'thermal power (MW)     thermal power (MW)      (MW)'

    write(outfile,'(t10,a)') 'First wall:'
    write(outfile,10) pnucfw, 0.0D0, pnucfw
    write(outfile,20) palpfwmw, 0.0D0, palpfwmw
    write(outfile,30) pradfw, 0.0D0, pradfw
    write(outfile,40) htpmw_fw, 0.0D0, htpmw_fw

    primsum = primsum + pnucfw + palpfwmw + pradfw + htpmw_fw
    secsum = secsum

    call oblnkl(outfile)

    write(outfile,'(t10,a)') 'Blanket:'
    write(outfile,10) pnucblkt, 0.0D0, pnucblkt
    write(outfile,20) 0.0D0, 0.0D0, 0.0D0
    write(outfile,30) 0.0D0, 0.0D0, 0.0D0
    write(outfile,40) htpmw_blkt, 0.0D0, htpmw_blkt

    primsum = primsum + pnucblkt + htpmw_blkt
    secsum = secsum

    call oblnkl(outfile)

    write(outfile,'(t10,a)') 'Shield:'
    write(outfile,10) pnucshld*iprimshld, pnucshld*(1-iprimshld), pnucshld
    write(outfile,20) 0.0D0, 0.0D0, 0.0D0
    write(outfile,30) 0.0D0, 0.0D0, 0.0D0
    write(outfile,40) htpmw_shld*iprimshld, htpmw_shld*(1-iprimshld), htpmw_shld

    primsum = primsum + pnucshld*iprimshld + htpmw_shld*iprimshld
    secsum = secsum + pnucshld*(1-iprimshld) + htpmw_shld*(1-iprimshld)

    call oblnkl(outfile)

    write(outfile,'(t10,a)') 'Divertor:'
    write(outfile,10) pnucdiv*iprimdiv, pnucdiv*(1-iprimdiv), pnucdiv
    write(outfile,20) pdivt*iprimdiv, pdivt*(1-iprimdiv), pdivt
    write(outfile,30) praddiv*iprimdiv, praddiv*(1-iprimdiv), praddiv
    write(outfile,40) htpmw_div*iprimdiv, htpmw_div*(1-iprimdiv), htpmw_div

    primsum = primsum + pnucdiv*iprimdiv + pdivt*iprimdiv + praddiv*iprimdiv + htpmw_div*iprimdiv
    secsum = secsum + pnucdiv*(1-iprimdiv) + pdivt*(1-iprimdiv) + praddiv*(1-iprimdiv) + htpmw_div*(1-iprimdiv)

    if (itart == 1) then
        call oblnkl(outfile)
        write(outfile,'(t10,a)') 'TART centrepost:'
        write(outfile,10) 0.0D0, pnuccp, pnuccp
        write(outfile,20) 0.0D0, 0.0D0, 0.0D0
        write(outfile,30) 0.0D0, 0.0D0, 0.0D0
        write(outfile,40) 0.0D0, ppumpmw, ppumpmw  !  check
    end if

    primsum = primsum
    secsum = secsum + pnuccp + ppumpmw

    call oblnkl(outfile)
    write(outfile,'(t10,a)') 'TF coil:'
    write(outfile,10) 0.0D0, ptfnuc, ptfnuc
    write(outfile,20) 0.0D0, 0.0D0, 0.0D0
    write(outfile,30) 0.0D0, 0.0D0, 0.0D0
    write(outfile,40) 0.0D0, 0.0D0, 0.0D0

    primsum = primsum
    secsum = secsum + ptfnuc

    call oblnkl(outfile)
    write(outfile,'(t10,a)') 'Losses to H/CD apparatus + diagnostics:'
    write(outfile,10) 0.0D0, pnuchcd, pnuchcd
    write(outfile,20) 0.0D0, 0.0D0, 0.0D0
    write(outfile,30) 0.0D0, pradhcd, pradhcd
    write(outfile,40) 0.0D0, 0.0D0, 0.0D0

    primsum = primsum
    secsum = secsum + pnuchcd + pradhcd

    call oblnkl(outfile)
    write(outfile,'(t10,a)') repeat('-',88)
    write(outfile,50) primsum, secsum, primsum+secsum
10     format(t32,'neutrons',t50,f8.2,t70,f8.2,t90,f8.2)
20     format(t14,'charged particle transport',t50,f8.2,t70,f8.2,t90,f8.2)
30     format(t31,'radiation',t50,f8.2,t70,f8.2,t90,f8.2)
40     format(t25,'coolant pumping',t50,f8.2,t70,f8.2,t90,f8.2)
50     format(t34,'Totals',t50,f8.2,t70,f8.2,t90,f8.2)

    call oblnkl(outfile)
    call ovarrf(outfile, 'Total power leaving reactor (across vacuum vessel boundary) (MW)', '', primsum + secsum + ptfnuc)

    call osubhd(outfile, 'Other secondary thermal power constituents :')
    call ovarrf(outfile,'Heat removal from cryogenic plant (MW)', '(crypmw)', crypmw)
    call ovarrf(outfile,'Heat removal from facilities (MW)', '(fachtmw)', fachtmw)

    if (ihplant /= 0) then
        call ovarrf(outfile, 'Electrical power used for hydrogen production (MW)', '(helecmw)', helecmw)
        call ovarrf(outfile, 'Thermal power used for hydrogen production (MW)', '(hthermmw)', hthermmw)
    end if

    call ovarrf(outfile,'Coolant pumping efficiency losses (MW)', '(htpsecmw)', htpsecmw)
    call ovarrf(outfile,'Heat removal from injection power (MW)', '(pinjht)', pinjht)
    call ovarrf(outfile,'Heat removal from tritium plant (MW)', '(trithtmw)', trithtmw)
    call ovarrf(outfile,'Heat removal from vacuum pumps (MW)', '(vachtmw)', vachtmw)
    call ovarrf(outfile,'TF coil resistive power (MW)', '(tfcmw)', tfcmw)

    call oblnkl(outfile)
    call ovarrf(outfile, 'Total low-grade thermal power (MW)', '(psechtmw)', psechtmw)
    call ovarrf(outfile, 'Total High-grade thermal power (MW)', '(pthermmw)', pthermmw)

    call oblnkl(outfile)
    call ovarin(outfile,'Number of primary heat exchangers', '(nphx)', nphx)

    if (ihplant /= 0) then
        call oblnkl(outfile)
        call ovarrf(outfile,'Hydrogen production rate (MW)', '(hpower)', hpower)
        call ovarre(outfile,'Hydrogen production rate (Nm3/sec)', '(hpower/13)', hpower/13.0D0)
    end if

    if (ireactor /= 1) return
    
    ! MDK start
    call oblnkl(outfile)
    call oblnkl(outfile)
    call ocmmnt(outfile,'Power Balance across separatrix :')
    call ocmmnt(outfile,'-------------------------------')
    call ocmmnt(outfile,'Only energy deposited in the plasma is included here.')

    if (iradloss == 0) then
		call ocmmnt(outfile,'Total power loss is scaling power plus radiation (iradloss = 0)')
		call ovarrf(outfile,'Transport power from scaling law (MW)','(pscalingmw)',pscalingmw)
		call ovarrf(outfile,'Total net radiation power (MW)','(pradmw)',pradmw)
		call ovarrf(outfile,'Total (MW)','',pscalingmw+pradmw)
    else if (iradloss == 1) then
		call ocmmnt(outfile,'Total power loss is scaling power plus core radiation only (iradloss = 1)')
		call ovarrf(outfile,'Transport power from scaling law (MW)','(pscalingmw)',pscalingmw)
		call ovarrf(outfile,'Radiation power from inside "coreradius" (MW)','(pcoreradmw)',pcoreradmw)
		call ovarrf(outfile,'Total (MW)','',pscalingmw+pcoreradmw)
    else if (iradloss == 2) then
		call ocmmnt(outfile,'Total power loss is scaling power only (iradloss = 1).')
		call ocmmnt(outfile,'This is not recommended for power plant models.')
		call ovarrf(outfile,'Transport power from scaling law (MW)','(pscalingmw)',pscalingmw)
		call ovarrf(outfile,'Total (MW)','',pscalingmw)
    else
        write(*,*) 'The value of iradloss appears to be invalid.'
        call ocmmnt(outfile,'THE VALUE OF IRADLOSS APPEARS TO BE INVALID.') 
    end if
    
    call oblnkl(outfile)
	call ovarrf(outfile,'Alpha power deposited in plasma (MW)','(falpha*palpmw)',falpha*palpmw)
	call ovarrf(outfile,'Power from charged products of DD and/or D-He3 fusion (MW)','(pchargemw)',pchargemw)
	call ovarrf(outfile,'Ohmic heating (MW)','(pohmmw)',pohmmw)
	call ovarrf(outfile,'Injected power deposited in plasma (MW)','(pinjmw)',pinjmw)
	call ovarrf(outfile,'Total (MW)','',falpha*palpmw+pchargemw+pohmmw+pinjmw)		  
	call oblnkl(outfile)

    call ocmmnt(outfile,'Power Balance for Reactor - Summary :')
    call ocmmnt(outfile,'-------------------------------------')
	call ovarrf(outfile,'Fusion power (MW)','(powfmw)',powfmw)
	call ovarrf(outfile,'Power from energy multiplication in blanket and shield (MW)','(emultmw)',emultmw)
	call ovarrf(outfile,'Injected power (MW)','(pnbitot)',pnbitot)
	call ovarrf(outfile,'Ohmic power (MW)','(pohmmw)',pohmmw)
	call ovarrf(outfile,'Power deposited in primary coolant by pump (MW)','(htpmw_mech)',htpmw_mech)
	call ovarrf(outfile,'Total (MW)','',powfmw+emultmw+pnbitot+htpmw_mech+pohmmw)
	call oblnkl(outfile)
	call ovarrf(outfile,'Heat extracted from armour and first wall (MW)','(pthermfw)',pthermfw)
	call ovarrf(outfile,'Heat extracted from blanket (MW)','(pthermblkt)',pthermblkt)
	call ovarrf(outfile,'Heat extracted from shield  (MW)','(pthermshld)',pthermshld)
	call ovarrf(outfile,'Heat extracted from divertor (MW)','(pthermdiv)',pthermdiv)
	call ovarrf(outfile,'Nuclear and photon power lost to H/CD system (MW)','(psechcd)',psechcd)
	call ovarrf(outfile,'Total (MW)','',pthermfw+pthermblkt+pthermshld+pthermdiv+psechcd)
	call oblnkl(outfile)
	
	! Heat rejected by main power conversion circuit	
	rejected_main = pthermmw * (1 - etath)
	
	call ocmmnt(outfile,'Electrical Power Balance :')
    call ocmmnt(outfile,'--------------------------')
	call ovarrf(outfile,'Net electric power output(MW)','(pnetelmw)',pnetelmw)
	call ovarrf(outfile,'Electric power for heating and current drive (MW)','(pinjwp)',pinjwp)
	call ovarrf(outfile,'Electric power for primary coolant pumps (MW)','(htpmw)',htpmw)
	call ovarrf(outfile,'Electric power for vacuum pumps (MW)','(vachtmw)',vachtmw)
	call ovarrf(outfile,'Electric power for tritium plant (MW)','(trithtmw)',trithtmw)
	call ovarrf(outfile,'Electric power for cryoplant (MW)','(crypmw)',crypmw)
	call ovarrf(outfile,'Electric power for TF coils (MW)','(tfacpd)',tfacpd)	
	call ovarrf(outfile,'Electric power for PF coils (MW)','(pfwp)', pfwp)	
	!call ovarrf(outfile,'Electric power for secondary (power conversion)cycle (MW)','(bopmw)', bopmw)	
	call ovarrf(outfile,'All other internal electric power requirements (MW)','(fachtmw)', fachtmw)	
	call ovarrf(outfile,'Total (MW)','',pnetelmw+pinjwp+htpmw+vachtmw+trithtmw+crypmw+tfacpd+fachtmw+pfwp)
	call oblnkl(outfile)
	call ovarrf(outfile,'Gross electrical output* (MW)','(pgrossmw)',pgrossmw)
	call ocmmnt(outfile,'(*Power for pumps in secondary circuit already subtracted)')
	call oblnkl(outfile)
	
	call ocmmnt(outfile,'Power balance for power plant :')
    call ocmmnt(outfile,'-------------------------------')
	call ovarrf(outfile,'Fusion power (MW)','(powfmw)',powfmw)
	call ovarrf(outfile,'Power from energy multiplication in blanket and shield (MW)','(emultmw)',emultmw)
	call ovarrf(outfile,'Total (MW)','',powfmw + emultmw)
	call oblnkl(outfile)
	call ovarrf(outfile,'Net electrical output (MW)	','(pnetelmw)',pnetelmw)
	call ovarrf(outfile,'Heat rejected by main power conversion circuit (MW)','(rejected_main)',rejected_main)
	call ovarrf(outfile,'Heat rejected by other cooling circuits (MW)','(psechtmw)',psechtmw)
	call ovarrf(outfile,'Total (MW)','',pnetelmw + rejected_main + psechtmw)
    call oblnkl(outfile)    
    
    call osubhd(outfile,'Plant efficiency measures :')
    call ovarrf(outfile,'Net electric power / total nuclear power (%)', '(pnetelmw/(powfmw+emultmw)', 100.0D0*pnetelmw/(powfmw+emultmw))
    call ovarrf(outfile,'Net electric power / total fusion power (%)', '(pnetelmw/powfmw)', 100.0D0*pnetelmw/powfmw)
    call ovarrf(outfile,'Gross electric power* / high grade heat (%)', '(etath)', 100.0D0*etath)
    call ocmmnt(outfile,'(*Power for pumps in secondary circuit already subtracted)')
    call ovarrf(outfile,'Recirculating power fraction', '(cirpowfr)', cirpowfr)
    

  end subroutine power2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cryo(itfsup, ipfres, tfsai, coldmass, ptfnuc, ensxpfm, tpulse, cpttf, tfno, helpow)

    !+ad_name  cryo
    !+ad_summ  Calculates cryogenic loads
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  itfsup : input integer : Switch denoting whether TF coils are
    !+ad_argc                           superconducting
    !+ad_args  ipfres : input integer : Switch denoting whether PF coils are resistive
    !+ad_args  tfsai : input real : Inboard TF coil surface area (m2)
    !+ad_args  coldmass : input real : Mass of cold (cryogenic) components (kg),
    !+ad_argc                          including TF coils, PF coils, cryostat, and
    !+ad_argc                          intercoil structure
    !+ad_args  ptfnuc : input real : Nuclear heating in TF coils (MW)
    !+ad_args  ensxpfm : input real : Maximum PF coil stored energy (MJ)
    !+ad_args  tpulse : input real : Pulse length of cycle (s)
    !+ad_args  cpttf : input real : Current per turn in TF coils (A)
    !+ad_args  tfno : input real : Number of TF coils
    !+ad_args  helpow : output real : Helium heat removal at cryo temperatures (W)
    !+ad_desc  This routine calculates the cryogenic heat load.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  02/08/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  D. Slack memo SCMDG 88-5-1-059, LLNL ITER-88-054, Aug. 1988
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: itfsup,ipfres
    real(kind(1.0D0)), intent(in) :: coldmass,cpttf,ensxpfm,ptfnuc,tfno, &
         tfsai,tpulse
    real(kind(1.0D0)), intent(out) :: helpow

    !  Local variables

    real(kind(1.0D0)) :: qac,qcl,qnuc,qss

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Steady state loads (W)
    qss = 4.3D-4 * coldmass
    if (itfsup == 1) qss = qss + 2.0D0*tfsai

    !  Nuclear heating of TF coils (W) (zero if resistive)
    qnuc = 1.0D6 * ptfnuc

    !  AC losses
    qac = 1.0D3 * ensxpfm/tpulse

    !  Current leads
    if (itfsup == 1) then
       qcl = 13.6D-3 * tfno * cpttf
    else
       qcl = 0.0D0
    end if

    !  Total includes 45% extra miscellaneous, piping and reserves
    helpow = max(0.0D0, (1.45D0 * (qss + qnuc + qac + qcl)) )

  end subroutine cryo

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine plant_thermal_efficiency(etath)

    !+ad_name  plant_thermal_efficiency
    !+ad_summ  Calculates the thermal efficiency of the power conversion cycle
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  C Harrington, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  etath : input/output real : thermal to electric conversion efficiency
    !+ad_desc  This routine calculates the thermal efficiency of the power conversion cycle.
    !+ad_desc  This gives the gross power of the plant, i.e. the primary coolant pumping
    !+ad_desc  power is not subtracted at this point; however, the pumping of the
    !+ad_desc  secondary coolant is accounted for.
    !+ad_desc  <P>If secondary_cycle = 0, 1, a set efficiency for the chosen blanket design is used,
    !+ad_desc  taken from cycle modelling studies.
    !+ad_desc  <P>If secondary_cycle > 1, the outlet temperature from the first wall
    !+ad_desc  and breeder zone is used to calculate an efficiency, using a simple relationship
    !+ad_desc  between etath and outlet_temp again obtained from previous studies.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  23/10/14 PJK Initial version
    !+ad_hist  10/12/14 PJK Added tturb ranges of validity
    !+ad_hist  17/12/14 PJK Added warning messages if tturb out of range
    !+ad_hist  12/02/15 JM  Changed the thermal efficiency fits for detailed model
    !+ad_hist  11/03/15 JM  Changed the argument list to remove global variables.
    !+ad_stat  Okay
    !+ad_docs  C. Harrington, K:\Power Plant Physics and Technology \ PROCESS \ blanket_model
    !+ad_docc  \ New Power Module Harrington \ Cycle correlations \ Cycle correlations.xls
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(inout) :: etath

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    select case (secondary_cycle)

    !  Etath from reference. Div power not to primary
    case default  

	   !  CCFE HCPB model
	   if (iblanket == 1) then
		  !  HCPB, efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX Feedheat & reheat cycle assumed
          etath = 0.436D0
          
       !  KIT HCPB model
	   else if (iblanket == 2) then
		  !  HCPB, efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX Feedheat & reheat cycle assumed
          etath = 0.436D0
          
       end if

	!  Etath from reference. Div power to primary
    case (1) 
    
		!  CCFE HCPB model
		if (iblanket == 1) then
		  !  HCPB, efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX Feedheat & reheat cycle assumed
          etath = 0.397D0
        
        !  KIT HCPB model
        else if (iblanket == 2) then
			etath = 0.397D0
			
		end if

	!  User input used, etath not changed
    case (2)  

       return

    !  Steam Rankine cycle to be used
    case (3)

		!  CCFE HCPB Model
        if (iblanket == 1) then
          !  If coolant is helium, the steam cycle is assumed to be superheated
          !  and a different correlation is used. The turbine inlet temperature 
          !  is assumed to be 20 degrees below the primary coolant outlet 
          !  temperature, as was stated as practical in EFDA_D_2LLNBX.
          
          !  Superheated steam Rankine cycle correlation (C. Harrington)
          !  Range of validity: 657 K < tturb < 840 K
          tturb = outlet_temp - 20.0D0
          if ((tturb < 656.0D0).or.(tturb > 840.0D0)) then
             idiags(1) = 2 ; fdiags(1) = tturb
             call report_error(166)
          end if
          etath = 0.1802D0*log(tturb) - 0.8002D0
          
       !  KIT HCPB Model
       else if (iblanket == 2) then
		  !  Same as iblanket = 1
          tturb = outlet_temp - 20.0D0
          if ((tturb < 656.0D0).or.(tturb > 840.0D0)) then
             idiags(1) = 2 ; fdiags(1) = tturb
             call report_error(166)
          end if
          etath = 0.1802D0*log(tturb) - 0.8002D0
          
       end if

    !  Supercritical CO2 cycle to be used
    case (4)
       !  The same temperature/efficiency correlation is used regardless of 
       !  primary coolant choice.  The turbine inlet temperature is assumed to 
       !  be 20 degrees below the primary coolant outlet temperature.
       !  s-CO2 can in theory be used for both helium and water primary coolants
       !  so no differentiation is made, but for water the efficiency will be 
       !  very low and the correlation will reflect this.

       !  Supercritical CO2 cycle correlation (C. Harrington)
       !  Range of validity: 408 K < tturb < 1023 K
       tturb = outlet_temp - 20.0D0
       if ((tturb < 408.0D0).or.(tturb > 1023.0D0)) then
          idiags(1) = 3 ; fdiags(1) = tturb
          call report_error(166)
       end if
       etath = 0.4347D0*log(tturb) - 2.5043D0

    end select

  end subroutine plant_thermal_efficiency

end module power_module
