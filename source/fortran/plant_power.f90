! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module power_module

  !! Module containing heat/power transport and power balance routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! power supply requirements, heat transport system parameters
  !! and the power balance for a fusion power plant.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  private
  public :: tfpwr, pfpwr, acpow, power1, power2, power3, init_power_module

  !  Precision variable
  integer, parameter :: double = 8

  !  Local variables
  real(kind=double) :: htpmwe_shld, htpmwe_div, htpmw_mech, pthermfw_blkt
  real(kind=double) :: htpmwe_fw_blkt
  real(kind=double) :: pthermdiv, pthermfw, pthermblkt, pthermshld
  real(kind=double) :: ppumpmw, pcoresystems, pdivfraction, delta_eta, qss, qac, qcl, qmisc

  !  Primary power to divertor factor
  integer, private :: iprimdiv

  ! Var in subroutine power1 requiring re-initialisation on each new run
  real(dp) :: p_tf_cryoal_cryo

contains

  subroutine init_power_module
    !! Initialise module variables
    implicit none

    p_tf_cryoal_cryo = 0.0D0
    qmisc = 0.0D0
    qac = 0.0D0
    qcl = 0.0D0
    qss = 0.0D0
    htpmwe_shld = 0.0d0
    htpmwe_div = 0.0d0
    htpmw_mech = 0.0d0
    pthermfw_blkt = 0.0d0
    htpmwe_fw_blkt = 0.0d0
    pthermdiv = 0.0d0
    pthermfw = 0.0d0
    pthermblkt = 0.0d0
    pthermshld = 0.0d0
    ppumpmw = 0.0d0
    pcoresystems = 0.0d0
    pdivfraction = 0.0d0
    delta_eta = 0.0d0
    iprimdiv = 0.0d0
  end subroutine init_power_module

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tfpwr(outfile,iprint)

    !! TF coil power supply requirements for resistive coils
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output (1=yes)
    !! This routine calculates the power conversion requirements for
    !! resistive TF coils, or calls <CODE>tfpwcall</CODE> if the TF
    !! coils are superconducting.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use buildings_variables, only: tfcbv
    use heat_transport_variables, only: tfacpd, etatf
    use physics_variables, only: rmajor
    use process_output, only: oheadr, ovarre
    use tfcoil_variables, only: tflegmw, estotftgj, tfcpmw, rhotfleg, &
        tflegres, vtfskv, jbus, tfbusl, tfbusmas, tfcmw, vtfkv, i_tf_sup, &
        tfckw, presleg, ritfc, cpttf, prescp, n_tf, rhotfbus, tfjtsmw, &
        pres_joints
    use constants, only: pi, dcopper
    implicit none

    !  Arguments
    integer, intent(in) :: outfile,iprint

    !  Local variables
    real(dp) :: abus, tfbusres, ztot, tfbusmw, tfreacmw

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (i_tf_sup /= 1) then  !  Non-superconducting TF coils

       !  TF coil bus length (m)
       !  Assume power supplies are 5m away
       tfbusl = 300.0D0

       !  Cross-sectional area of bus
       !  cpttf  - current per TFC turn (A)
       !  jbus   - bus current density (A/m2)
       abus = cpttf/jbus

       ! Bus resistance [ohm]
       ! Bus resistivity (rhotfbus) default value : -1.0D0
       ! If this value is chosen, the bus resistivity is the same as the leg one
       if ( abs(rhotfbus + 1.0D0) < epsilon(rhotfbus) ) rhotfbus = rhotfleg  
       tfbusres = rhotfbus * tfbusl/abus

       !  Bus mass (kg)
       tfbusmas = tfbusl * abus * dcopper

       !  Total maximum impedance MDK actually just fixed resistance
       ztot = n_tf*tflegres + (prescp/ritfc**2) + tfbusres

       !  No reactive portion of the voltage is included here - assume long ramp times
       !  MDK This is steady state voltage, not "peak" voltage
       vtfkv = 1.0D-3 * ztot * cpttf/n_tf

       !  Resistive powers (MW):
       tfcpmw  = 1.0D-6 * prescp   !  inboard legs (called centrepost, CP for tart design)
       tflegmw = 1.0D-6 * presleg  !  outboard legs
       tfjtsmw =  1.0D-6 * pres_joints  ! Joints 
       tfbusmw = 1.0D-6 * cpttf**2 * tfbusres  !  TF coil bus => Dodgy !

       !  TF coil reactive power
       !  Set reactive power to 0, since ramp up can be long
       !  The TF coil can be ramped up as slowly as you like
       !  (although this will affect the time to recover from a magnet quench).
       !     tfreacmw = 1.0D-6 * 1.0D9 * estotf/(tohs + tramp)
       !                                 estotf(=estotftgj/n_tf) has been removed (#199 #847)
       tfreacmw = 0.0D0

       !  Total power consumption (MW)
       tfcmw = tfcpmw + tflegmw + tfbusmw + tfreacmw + tfjtsmw

       !  Total steady state AC power demand (MW)
       tfacpd = tfcmw / etatf

    else  !  Superconducting TF coil option

       call tfpwcall(outfile,iprint)
       return

    end if

    !  Output section
    if (iprint == 0) return
    ! Clarify that these outputs are for resistive coils only
    call oheadr(outfile,'Resistive TF Coil Power Conversion')
    call ovarre(outfile,'Bus resistance (ohm)','(tfbusres)',tfbusres, 'OP ')
    call ovarre(outfile,'Bus current density (A/m2)','(jbus)',jbus)
    call ovarre(outfile,'Bus length - all coils (m)','(tfbusl)',tfbusl)
    call ovarre(outfile,'Bus mass (kg)','(tfbusmas)',tfbusmas, 'OP ')
    !call ovarre(outfile,'Maximum impedance (ohm)','(ztot)',ztot)
    call ovarre(outfile,'Total resistance for TF coil set (ohm)','(ztot)',ztot, 'OP ')
    !call ovarre(outfile,'Peak voltage per coil (kV)','(vtfkv)',vtfkv)
    call ovarre(outfile,'Steady-state voltage per coil (kV)','(vtfkv)',vtfkv, 'OP ')
    !call ovarre(outfile,'Peak power (MW)','(tfcmw..)',tfcmw)
    call ovarre(outfile,'Total power dissipation in TF coil set (MW)','(tfcmw..)',tfcmw, 'OP ')
    call ovarre(outfile,'Power dissipation in TF coil set: inboard legs (MW)', '(tfcpmw)',tfcpmw, 'OP ')
    call ovarre(outfile,'Power dissipation in TF coil set: outboard legs (MW)', '(tflegmw)',tflegmw, 'OP ')
    call ovarre(outfile,'Power dissipation in TF coil set: buses','(tfbusmw)', tfbusmw, 'OP ')
    ! Reactive poower has been set to zero.
    !call ovarre(outfile,'TF coil reactive power (MW)','(tfreacmw)', tfreacmw)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine tfpwcall(outfile,iprint)

      !! Calls the TF coil power conversion routine for
      !! superconducting coils
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: P C Shipe, ORNL
      !! outfile : input integer : output file unit
      !! iprint : input integer : switch for writing to output (1=yes)
      !! This routine calls routine <CODE>tfcpwr</CODE> to calculate
      !! the power conversion requirements for superconducting TF coils.
      !! None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: outfile,iprint

      !  Local variables

      real(dp) :: drarea,ettfmj,itfka

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Stored energy (MJ) MDK changed to estotftgj/n_tf

      ettfmj = estotftgj / n_tf * 1.0D3

      !  TF coil current (kA)

      itfka = 1.0D-3 * cpttf

      call tfcpwr(outfile,iprint,n_tf,ettfmj,itfka,tflegres, &
           vtfskv,rmajor,tfckw,tfbusl,drarea,tfcbv,tfacpd)

    end subroutine tfpwcall

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine tfcpwr(outfile,iprint,ntfc,ettfmj,itfka, &
         rptfc,vtfskv,rmajor,tfckw,tfbusl,drarea,tfcbv,tfacpd)

      !! Calculates the TF coil power conversion system parameters
      !! for superconducting coils
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: P C Shipe, ORNL
      !! This routine calculates the TF power conversion system
      !! parameters:  floor space, power supplies, bussing,
      !! coil protection equipment, and the associated controls
      !! and instrumentation. It was originally written by G. Gorker,
      !! FEDC/ORNL, April 1987, modified by J. Galambos in 1991 to
      !! run in TETRA, and included in PROCESS in 1992 by P. C. Shipe.
      !! None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      ! Inputs
      ! ---
      integer, intent(in) :: outfile
      !! Output file unit

      integer, intent(in) :: iprint
      !! Switch for writing to output (1=yes)

      real(dp), intent(in) :: ntfc
      !! Number of TF coils
      
      real(dp), intent(in) :: ettfmj
      !! Total stored energy of one TF coils [MJ]

      real(dp), intent(in) :: itfka
      !! Design current for the TF coils, kA
      
      real(dp), intent(in) :: rptfc
      !! Resistance of a TF coil [ohm]

      real(dp), intent(in) :: vtfskv
      !! Allowable voltage across a TF coil during quench [kV]

      real(dp), intent(in) :: rmajor
      !! Plasma major radius [m]
      ! ---

      ! Outputs
      ! ---
      real(dp), intent(out) :: tfckw
      !! Available DC power for charging the TF coils [kW]
      
      real(dp), intent(out) :: tfbusl
      !! Total bus length of the TF coil system [m]

      real(dp), intent(out) :: drarea
      !! Approx. area needed for the energy dump resistors, [m2]

      real(dp), intent(out) :: tfcbv
      !! Approx. vol needed for the TF coil power supplies and DC circuit breakers [m3]

      real(dp), intent(out) :: tfacpd
      !! Steady state TF coil AC power demand, [MW]

      !  Local variables
      real(dp) :: albusa,albuswt,djmka,fspc1,fspc2,fspc3,ettfc, &
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
      tfacpd = tfacpd + rpower/etatf
      !  Total TF coil power conversion building floor area, m2

      tftsp = tfcfsp
      !  Total TF coil power conversion building volume, m3

      tftbv = tfcbv

      !  Output section
      if (iprint == 0) return

      call oheadr(outfile,'Superconducting TF Coil Power Conversion')
      call ovarre(outfile,'TF coil current (kA)','(itfka)',itfka, 'OP ')
      call ovarre(outfile,'Number of TF coils','(ntfc)',ntfc)
      call ovarre(outfile,'Voltage across a TF coil during quench (kV)','(vtfskv)', vtfskv, 'OP ')
      call ovarre(outfile,'TF coil charge time (hours)','(tchghr)',tchghr)
      call ovarre(outfile,'Total inductance of TF coils (H)','(ltfth)', ltfth, 'OP ')
      call ovarre(outfile,'Total resistance of TF coils (ohm)','(rcoils)', rcoils, 'OP ')
      ! MDK Remove this as it leads to confusion between (a) total inductance/n_tf, or (b)
      !     self-inductance of one single coil
      !call ovarre(outfile,'Inductance per TF coil (H)','(lptfcs)',lptfcs, 'OP ')
      call ovarre(outfile,'TF coil charging voltage (V)','(tfcv)',tfcv)
      call ovarre(outfile,'Number of DC circuit breakers','(ntfbkr)', &
           ntfbkr)
      call ovarre(outfile,'Number of dump resistors','(ndumpr)',ndumpr)
      call ovarre(outfile,'Resistance per dump resistor (ohm)','(r1dump)', r1dump, 'OP ')
      call ovarre(outfile,'Dump resistor peak power (MW)','(r1ppmw)', r1ppmw, 'OP ')
      call ovarre(outfile,'Energy supplied per dump resistor (MJ)', '(r1emj)',r1emj, 'OP ')
      call ovarre(outfile,'TF coil L/R time constant (s)','(ttfsec)', ttfsec, 'OP ')

      call ovarre(outfile,'Power supply voltage (V)','(tfpsv)',tfpsv, 'OP ')
      call ovarre(outfile,'Power supply current (kA)','(tfpska)',tfpska, 'OP ')
      call ovarre(outfile,'DC power supply rating (kW)','(tfckw)',tfckw, 'OP ')
      call ovarre(outfile,'AC power for charging (kW)','(tfackw)',tfackw, 'OP ')
      call ovarre(outfile,'TF coil resistive power (MW)','(rpower)',rpower, 'OP ')

      call ovarre(outfile,'TF coil inductive power (MVA)','(xpower)',xpower, 'OP ')
      call ovarre(outfile,'Aluminium bus current density (kA/cm2)', '(djmka)',djmka)
      call ovarre(outfile,'Aluminium bus cross-sectional area (cm2)', '(albusa)',albusa, 'OP ')
      call ovarre(outfile,'Total length of TF coil bussing (m)', '(tfbusl)',tfbusl, 'OP ')
      call ovarre(outfile,'Aluminium bus weight (tonnes)','(albuswt)', albuswt, 'OP ')

      call ovarre(outfile,'Total TF coil bus resistance (ohm)','(rtfbus)', rtfbus, 'OP ')
      call ovarre(outfile,'TF coil bus voltage drop (V)','(vtfbus)',vtfbus, 'OP ')
      call ovarre(outfile,'Dump resistor floor area (m2)','(drarea)', drarea, 'OP ')
      call ovarre(outfile,'TF coil power conversion floor space (m2)', '(tfcfsp)',tfcfsp, 'OP ')
      call ovarre(outfile,'TF coil power conv. building volume (m3)', '(tfcbv)',tfcbv, 'OP ')
      call ovarre(outfile,'TF coil AC inductive power demand (MW)', '(xpwrmw)',xpwrmw, 'OP ')
      call ovarre(outfile,'Total steady state AC power demand (MW)', '(tfacpd)',tfacpd, 'OP ')

    end subroutine tfcpwr

  end subroutine tfpwr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pfpwr(outfile,iprint)

    !! PF coil power supply requirements
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output (1=yes)
    !! This routine calculates the MVA, power and energy requirements
    !! for the PF coil systems.  Units are MW and MVA for power terms.
    !! The routine checks at the beginning of the flattop for the
    !! peak MVA, and at the end of flattop for the peak stored energy.
    !! The reactive (inductive) components use waves to calculate the
    !! <I>dI/dt</I> at the time periods.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: iohcl
    use heat_transport_variables, only: peakmva
    use pf_power_variables, only: pfckts, maxpoloidalpower, peakpoloidalpower, &
        spfbusl, poloidalpower, spsmva, vpfskv, ensxpfm, acptmax, srcktpm
    use pfcoil_variables, only: ngc2, ngrp, cpt, pfwpmw, pfclres, ncirt, ncls, &
        ric, etapsu, cptdin, curpfb, sxlg, turns, vf, rjconpf, rpf
    use physics_variables, only: pohmmw, rmajor
    use process_output, only: oheadr, ovarre, oblnkl, ocmmnt
    use numerics, only: active_constraints, ioptimz
    use times_variables, only: tim, intervallabel, timelabel, tohs
    use constants, only: twopi, pi
    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(dp), dimension(ngc2) :: albusa,pfbusr,cktr, &
         powpfii,vpfi,psmva,pfcr,rcktvm,rcktpm
    real(dp) :: pfbusl,powpfr,cptburn,delktim,powpfi, &
         powpfr2,ensxpf,engx,vpfij
        !  engxpc
    real(dp), save :: pfbuspwr
    real(dp), dimension(6) :: inductxcurrent,poloidalenergy
    real(dp), dimension(5) :: pfdissipation
    real(dp):: wall_plug_ohmicmw, pfpower, pfpowermw
    integer :: i,ic,ngrpt,ig,ipf,jjpf,jjpf2,jpf,time

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

    !  PF system (including Central Solenoid solenoid) inductive MVA requirements
    !  cpt(i,j) : current per turn of coil i at (end) time period j (A)
    powpfi = 0.0D0
    powpfr = 0.0D0
    powpfr2 = 0.0D0
    ensxpf = 0.0D0

    !  ncirt : total number of PF coils (including Central Solenoid and plasma)
    !          plasma is #ncirt, and Central Solenoid is #(ncirt-1)
    !  sxlg(i,j) : mutual inductance between coil i and j
    do i = 1, ncirt
       powpfii(i) = 0.0D0
       vpfi(i) = 0.0D0
    end do

    jpf = 0
    poloidalenergy(:) = 0.0d0
    do jjpf = 1,ngrpt           ! Loop over all groups of PF coils.
       do jjpf2 = 1,ncls(jjpf)  ! Loop over all coils in each group 
          jpf = jpf + 1
          engx = 0.0D0
          inductxcurrent(:) = 0.0d0
          do ipf = 1,ncirt

             !  Voltage in circuit jpf due to change in current from circuit ipf
             vpfij = sxlg(jpf,ipf) * (cpt(ipf,3)-cpt(ipf,2))/delktim

             !  Voltage in circuit jpf at time, tim(3), due to changes in coil currents
             vpfi(jpf) = vpfi(jpf) + vpfij

             !  MVA in circuit jpf at time, tim(3) due to changes in current
             powpfii(jpf) = powpfii(jpf) + vpfij*cpt(jpf,3)/1.d6

             ! Term used for calculating stored energy at each time
             do time = 1,6
                inductxcurrent(time) = inductxcurrent(time) + sxlg(jpf,ipf)*cpt(ipf,time)
             end do
             !engx = engx + sxlg(jpf,ipf)*cpt(ipf,5)

          end do

          !  Stored magnetic energy of the poloidal field at each time
          ! 'time' is the time INDEX.  'tim' is the time.
          do time = 1,6
            poloidalenergy(time) = poloidalenergy(time) + 0.5D0 * inductxcurrent(time) * cpt(jpf,time)
          end do
        !   do time = 1,5
        !     ! Mean rate of change of stored energy between time and time+1
        !     if(abs(tim(time+1)-tim(time)).gt.1.0d0) then
        !         poloidalpower(time) = (poloidalenergy(time+1)-poloidalenergy(time)) / (tim(time+1)-tim(time))
        !     else
        !         ! Flag when an interval is small or zero MDK 30/11/16
        !         poloidalpower(time) = 9.9d9
        !     end if

        !   end do
        !   !engxpc = 0.5D0 * engx * cpt(jpf,5)
        !   !ensxpf = ensxpf + engxpc

          !  Resistive power in circuits at times tim(3) and tim(5) respectively (MW)
          powpfr = powpfr + turns(jpf) * cpt(jpf,3) * cktr(jjpf)/1.0D6
          powpfr2 = powpfr2 +turns(jpf)* cpt(jpf,5) * cktr(jjpf)/1.0D6
          powpfi = powpfi + powpfii(jpf)

       end do
    end do

    do time = 1,5
        ! Stored magnetic energy of the poloidal field at each time
        ! 'time' is the time INDEX.  'tim' is the time.
        ! Mean rate of change of stored energy between time and time+1
        if(abs(tim(time+1)-tim(time)).gt.1.0d0) then
            poloidalpower(time) = (poloidalenergy(time+1)-poloidalenergy(time)) / (tim(time+1)-tim(time))
        else
            ! Flag when an interval is small or zero MDK 30/11/16
            poloidalpower(time) = 9.9d9
        end if
        ! Electrical energy dissipated in PFC power supplies as they increase or decrease the poloidal field energy
        ! This assumes that the energy storage in the PFC power supply is lossless and that currents
        ! in the coils can be varied without loss when there is no change in the energy in the poloidal field.
        ! Energy is dissipated only when energy moves into or out of the store in the power supply.
        ! Issue #713
        pfdissipation(time) = abs( poloidalenergy(time+1)-poloidalenergy(time) ) * (1.d0/etapsu - 1.d0)
    end do

    ! Mean power dissipated
    ! The flat top duration (time 4 to 5) is the denominator, as this is the time when electricity is generated.
    if(tim(5)-tim(4).gt.1.0d0) then
        pfpower = sum(pfdissipation(:)) / (tim(5) - tim(4))
    else
        ! Give up when an interval is small or zero.
        pfpower = 0.d0
    end if        
    pfpowermw = pfpower / 1.d6

    !  Compute the maximum stored energy and the maximum dissipative
    !  energy in all the PF circuits over the entire cycle time, MJ
    !ensxpfm = 1.0D-6 * ensxpf
    ensxpfm = 1.0D-6 * maxval(poloidalenergy)
    ! Peak absolute rate of change of stored energy in poloidal field (MW)
    peakpoloidalpower = maxval(abs(poloidalpower))/1.0d6

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

    !  PF wall plug power dissipated in power supply for ohmic heating (MW)
    !  This is additional to that required for moving stored energy around
    !pfwpmw = pohmmw / etapsu
    wall_plug_ohmicmw  = pohmmw * (1.d0 /etapsu - 1.d0) 
    ! Total mean wall plug power dissipated in PFC and CS power supplies.  Issue #713
    pfwpmw = wall_plug_ohmicmw + pfpowermw    

    !  Output Section
    if (iprint == 0) return

    call oheadr(outfile,'PF Coils and Central Solenoid: Power and Energy')
    call ovarre(outfile,'Number of PF coil circuits','(pfckts)',pfckts)
    call ovarre(outfile,'Sum of PF power supply ratings (MVA)', '(spsmva)',spsmva, 'OP ')
    call ovarre(outfile,'Total PF coil circuit bus length (m)', '(spfbusl)',spfbusl, 'OP ')
    call ovarre(outfile,'Total PF coil bus resistive power (kW)', '(pfbuspwr)',pfbuspwr, 'OP ')
    call ovarre(outfile,'Total PF coil resistive power (kW)', '(srcktpm)',srcktpm, 'OP ')
    call ovarre(outfile,'Maximum PF coil voltage (kV)','(vpfskv)',vpfskv)
    call ovarre(outfile,'Efficiency of transfer of PF stored energy into or out of storage','(etapsu)',etapsu)
    call ocmmnt(outfile,'(Energy is dissipated in PFC power supplies only when total PF energy increases or decreases.)')  

    call ovarre(outfile,'Maximum stored energy in poloidal field (MJ)', '(ensxpfm)',ensxpfm, 'OP ')
    call ovarre(outfile,'Peak absolute rate of change of stored energy in poloidal field (MW)',  &
                        'peakpoloidalpower',peakpoloidalpower, 'OP ')

    if ((ioptimz > 0).and.(active_constraints(66))) then
        call ovarre(outfile,'Max permitted abs rate of change of stored energy in poloidal field (MW)', &
                            'maxpoloidalpower',maxpoloidalpower)
    end if

    if (any(poloidalenergy<0.0d0))  then
        call oheadr(outfile,'ERROR Negative stored energy in poloidal field')
        write(*,*)          'ERROR Negative stored energy in poloidal field'
    end if

    call ocmmnt(outfile,'Energy stored in poloidal magnetic field :')
    call oblnkl(outfile)

    write(outfile,50)(tim(time),time=1,6)
50  format(t45,'time (sec)'//t15,6f11.2)
    write(outfile,55)(timelabel(time),time=1,6)
55  format(' Time point', t21,6a11)

    write(outfile,60) (poloidalenergy(time)/1.0d6,time=1,6)
60  format(' Energy (MJ)',t17,6(1pe11.3))
    call oblnkl(outfile)

    write(outfile,65)(intervallabel(time),time=1,5)
65  format(' Interval', t26,6a11)
    write(outfile,70) (poloidalpower(time)/1.0d6,time=1,5)
70  format(' dE/dt (MW)',t22,5(1pe11.3))
    call oblnkl(outfile)

  end subroutine pfpwr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine acpow(outfile,iprint)

    !! AC power requirements
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: P C Shipe, ORNL
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output (1=yes)
    !! The routine was drastically shortened on 23/01/90 (ORNL) from the
    !! original TETRA routine to provide only the total power needs for
    !! the plant. Included in STORAC in January 1992 by P.C. Shipe.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use buildings_variables, only: efloor
    use heat_transport_variables, only: baseel, crypmw, vachtmw, tfacpd, &
        trithtmw, pinjwp, tlvpmw, peakmva, fcsht, fmgdmw, pwpm2, htpmw, pacpmw
    use pf_power_variables, only: iscenr, srcktpm
    use process_output, only: oheadr, ovarre, oblnkl
    use constants, only: pi
    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(dp) :: basemw,bdvmw,crymw,pheatingmw,pkwpm2,ppfmw,ptfmw

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
    fcsht = basemw + efloor*pkwpm2/1000.0D0

    ! Estimate of the total low voltage power, MW
    ! MDK No idea what this is - especially the last term
    ! It is used in the old cost routine, so I will leave it in place.
    tlvpmw = fcsht + trithtmw + htpmw + vachtmw + 0.5D0*(crymw+ppfmw)

    if (iprint == 0) return

    !  Output section
    !call oheadr(outfile,'AC Power')
    call oheadr(outfile,'Electric Power Requirements')
    call ovarre(outfile,'Facility base load (MW)','(basemw)',basemw)
    call ovarre(outfile,'Divertor coil power supplies (MW)','(bdvmw)',bdvmw)
    call ovarre(outfile,'Cryoplant electric power (MW)','(crymw)',crymw, 'OP ')
    !call ovarre(outfile,'Heat removed from cryogenic coils (MWth)','(helpow/1.0D6)',helpow/1.0D6)
    !call ovarre(outfile,'MGF (motor-generator flywheel) units (MW)', '(fmgdmw)',fmgdmw)
    !call ovarin(outfile,'Primary coolant pumps (MW)', '(coolwh)',coolwh)
    call ovarre(outfile,'Primary coolant pumps (MW)', '(htpmw..)',htpmw, 'OP ')

    call ovarre(outfile,'PF coil power supplies (MW)','(ppfmw)',ppfmw, 'OP ')
    !call ovarre(outfile,'Power/floor area (kW/m2)','(pkwpm2)',pkwpm2)
    call ovarre(outfile,'TF coil power supplies (MW)','(ptfmw)',ptfmw, 'OP ')
    call ovarre(outfile,'Plasma heating supplies (MW)','(pheatingmw)', pheatingmw, 'OP ')
    call ovarre(outfile,'Tritium processing (MW)','(trithtmw..)',trithtmw)
    call ovarre(outfile,'Vacuum pumps  (MW)','(vachtmw..)',vachtmw)

    call oblnkl(outfile)

    call ovarre(outfile,'Total pulsed power (MW)','(pacpmw)',pacpmw, 'OP ')
    call ovarre(outfile,'Total base power required at all times (MW)', '(fcsht)',fcsht, 'OP ')
    ! MDK Remove this output: no idea what this is
    ! call ovarre(outfile,'Total low voltage power (MW)','(tlvpmw)',tlvpmw)

  end subroutine acpow

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine power1

    !! Calculates the first part of the heat transport
    !! and plant power balance constituents
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine calculates the first part of the heat transport
    !! and plant power balance constituents.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use current_drive_variables, only: pinjmw, porbitlossmw, nbshinemw
    use fwbs_variables, only: pnucblkt, pradfw, etahtp, praddiv, &
        secondary_cycle, pnuccp, primary_pumping, ptfnuc, pnucshld, pradhcd, &
        pnucdiv, pnucfw, pnuchcd
    use heat_transport_variables, only: htpmw_shld, htpsecmw, pfwdiv, &
        psecshld, crypmw, htpmw_min, nphx, htpmw_div, psechcd, helpow, &
        htpmw_fw, pinjwp, pthermmw, psecdiv, etath, pinjht, iprimshld, htpmw, &
        htpmw_blkt
    use pf_power_variables, only: ensxpfm
    use pfcoil_variables, only: ipfres
    use physics_variables, only: pdivt, palpfwmw, ignite
    use structure_variables, only: coldmass
    use tfcoil_variables, only: tfsai, tcoolin, tmpcry, i_tf_sup, presleg, &
        prescp, dtiocool, n_tf, cpttf, pres_joints, eff_tf_cryo
    use times_variables, only: tpulse
    use primary_pumping_variables, only: htpmw_fw_blkt
    use constants, only: rmu0, pi
    implicit none

    !! Cryo-aluminium cryoplant power consumption
    
    !------------------------------------------------------------------------------------
    !- Collate pumping powers
    !------------------------------------------------------------------------------------

    ! Combine fw and blanket for convenience
    ! Already combined if primary_pumping=3
    if(primary_pumping/=3) htpmw_fw_blkt = htpmw_fw + htpmw_blkt

    !  Account for pump electrical inefficiencies. The coolant pumps are not assumed to be
    !  100% efficient so the electric power to run them is greater than the power deposited
    !  in the coolant.  The difference should be lost as secondary heat.
    htpmwe_fw_blkt = htpmw_fw_blkt / etahtp
    htpmwe_shld = htpmw_shld / etahtp
    htpmwe_div = htpmw_div / etahtp

    ! Total mechanical pump power (deposited in coolant)
    htpmw_mech = htpmw_fw_blkt + htpmw_shld + htpmw_div

    ! Minimum total electrical power for primary coolant pumps  (MW) Issue #303
    ! Recommended to leave the minimum value at zero.
    ! Note that htpmw is an ELECTRICAL power
    htpmw = max(htpmw_min, htpmwe_fw_blkt + htpmwe_shld + htpmwe_div)

    !  Heat lost through pump power inefficiencies (MW)
    htpsecmw = htpmw - htpmw_mech

    if(primary_pumping/=3) then
        !  Total power deposited in first wall coolant (MW)
        pthermfw = pnucfw + pradfw + htpmw_fw + porbitlossmw + palpfwmw + nbshinemw
        !  Total power deposited in blanket coolant (MW) (energy multiplication in pnucblkt already)
        pthermblkt = pnucblkt + htpmw_blkt
        pthermfw_blkt = pthermfw + pthermblkt
    elseif(primary_pumping==3)then
        !  Total power deposited in first wall and blanket coolant combined (MW)
        ! (energy multiplication in pnucblkt already)
        pthermfw_blkt = pnucfw + pradfw + pnucblkt + htpmw_fw_blkt + porbitlossmw + palpfwmw + nbshinemw
    end if

    !  Total power deposited in shield coolant (MW)
    pthermshld = pnucshld + htpmw_shld

    !  Total thermal power deposited in divertor coolant (MW)
    !  = (conduction to divertor, less radiation) + (neutron and radiation power)
    !  using pdivt as calculated in physics.f90
    pthermdiv = pdivt + (pnucdiv + praddiv) + htpmw_div

    !  Heat removal from first wall and divertor (MW) (only used in costs.f90)
    if(primary_pumping/=3) pfwdiv = pthermfw + pthermdiv

    !  Thermal to electric efficiency
    call plant_thermal_efficiency(etath)

    !  Primary (high-grade) thermal power, available for electricity generation.  Switch iprimshld
    !  is 1 or 0, is user choice on whether the shield thermal power goes to primary or secondary heat
    if (secondary_cycle == 0) then
  		!  Primary thermal power (MW)
        pthermmw = pthermfw_blkt + iprimshld*pthermshld
  		!  Secondary thermal power deposited in divertor (MW)
        psecdiv = pthermdiv
  		! Divertor primary/secondary power switch: does NOT contribute to energy generation cycle
        iprimdiv = 0
    else
  		!  Primary thermal power (MW)
        pthermmw = pthermfw_blkt + iprimshld*pthermshld + pthermdiv
  		!  Secondary thermal power deposited in divertor (MW)
        psecdiv = 0.0D0
  		! Divertor primary/secondary power switch: contributes to energy generation cycle
        iprimdiv = 1
    end if

    if (abs(pthermmw) < 1.0D-4) write(*,*) 'ERROR Primary thermal power is zero or negative'

	! #284 Fraction of total high-grade thermal power to divertor
    pdivfraction = pthermdiv / pthermmw
    ! Loss in efficiency as this primary power is collecetd at very low temperature
    delta_eta = 0.339*pdivfraction

    !  Secondary thermal power deposited in shield
    psecshld = pthermshld * (1-iprimshld)

    !  Secondary thermal power lost to HCD apparatus and diagnostics
    psechcd = pnuchcd + pradhcd

    !  Number of primary heat exchangers
    nphx = ceiling(pthermmw/1000.0D0)

    !  Secondary heat (some of it... rest calculated in POWER2)
    !  Wall plug injection power
    ! MDK
    ! pinjwp = (pinjmw + porbitlossmw + palpfwmw)/etacd
	  ! pinjwp calculated in current_drive.f90

    !  Waste injection power
    if (ignite == 0) then
       ! MDK
       !pinjht = pinjwp - pinjmw - porbitlossmw - palpfwmw
       pinjht = pinjwp - pinjmw
    else
       pinjht = 0.0D0
    end if

    !  Cryogenic power
    ! ---
    ! Initialisation (unchanged if all coil resisitive)
    helpow = 0.0D0
    crypmw = 0.0D0
    
    ! Superconductors TF/PF cryogenic cooling
    if ( i_tf_sup == 1 .or. ipfres == 0 ) then
        ! helpow calculation
        call cryo(i_tf_sup, tfsai, coldmass, ptfnuc, ensxpfm, tpulse, cpttf, n_tf, helpow)

        ! Use 13% of ideal Carnot efficiency to fit J. Miller estimate
        ! Rem SK : This ITER efficiency is very low compare to the Strowbridge curve
        !          any reasons why? 
        crypmw = 1.0D-6 * (293.0D0 - tmpcry)/(eff_tf_cryo*tmpcry) * helpow   
    end if

    ! Cryogenic aluminium 
    ! Rem : The carnot efficiency is assumed at 40% as this is a conservative assumption since a 50%
    !       has been deduced from detailed studies
    ! Rem : Nuclear heating on the outer legs assumed to be negligible
    ! Rem : To be updated with 2 cooling loops for TART designs
    if ( i_tf_sup == 2 ) then
        p_tf_cryoal_cryo = (293.0D0 - tcoolin)/(eff_tf_cryo*tcoolin) * &
                           ( prescp + presleg + pres_joints + pnuccp * 1.0D6 )
        crypmw = crypmw + 1.0D-6 * p_tf_cryoal_cryo
    end if


  end subroutine power1

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine power2(outfile,iprint)

    !! Calculates the remainder of the heat transport
    !! and plant power balance constituents
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output (1=yes)
    !! This routine calculates the rest of the heat transport
    !! and plant power balance constituents, not already calculated in
    !! <A HREF="acpow.html">ACPOW</A> or <A HREF="power1.html">POWER1</A>.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constraint_variables, only: pnetelin
    use cost_variables, only: ipnet, ireactor
    use current_drive_variables, only: pinjmw
    use fwbs_variables, only: emultmw, inuclear, pnucblkt, pradfw, qnuc, &
        etahtp, emult, praddiv, fdiv, fhcd, secondary_cycle, pnuccp, pnucdiv, &
        primary_pumping, ptfnuc, pnuchcd, pnucshld, pradhcd, pnucfw
    use heat_transport_variables, only: htpmw_shld, htpmw_blkt, psecshld, &
        fpumpshld, tturb, pnetelmw, fpumpdiv, fpumpblkt, vachtmw, htpmw_div, &
        nphx, helpow, htpmw_fw, precircmw, pthermmw, fpumpfw, fcsht, &
        iprimshld, pinjwp, fachtmw, pgrossmw, psechtmw, trithtmw, psechcd, &
        tfacpd, htpmw, etath, crypmw, psecdiv, pinjht, htpsecmw
    use pfcoil_variables, only: pfwpmw
    use physics_variables, only: palpmw, ignite, pcoreradmw, pradmw, itart, &
        pdivt, palpfwmw, idivrt, pohmmw, iradloss, powfmw, pchargemw, &
        pscalingmw, falpha
    use process_output, only: ovarin, ocmmnt, ovarrf, oheadr, ovarre, oblnkl, &
        osubhd
    use tfcoil_variables, only: ppump, i_tf_sup, tfcmw, tmpcry
    use primary_pumping_variables, only: htpmw_fw_blkt
    use constants, only: rmu0, mfile, pi
    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(dp) :: cirpowfr, primsum, pinj, secsum, rejected_main, sum

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Centrepost coolant pump power (ST)
    if ( itart == 1 .and. i_tf_sup == 0 ) then
        ppumpmw = 1.0D-6 * ppump
    else
        ppumpmw = 0.0D0
    end if

    !  Facility heat removal (fcsht calculated in ACPOW)
    fachtmw = fcsht

    !  Electrical power consumed by fusion power core systems
    !  (excluding heat transport pumps and auxiliary injection power system)
    !  pfwpmw = Mean electrical energy dissipated in PFC power supplies as they 
    !  increase or decrease the poloidal field energy AND extra due to ohmic heating
    !  of the plasma.  Issue #713
    pcoresystems = crypmw + fachtmw + ppumpmw + tfacpd + trithtmw + vachtmw + pfwpmw

    !  Total secondary heat
    !  (total low-grade heat rejected - does not contribute to power conversion cycle)
    !  Included ptfnuc
    !psechtmw = pcoresystems + pinjht + htpsecmw + hthermmw + psecdiv + psecshld + psechcd + ptfnuc
    psechtmw = pcoresystems + pinjht + htpsecmw + psecdiv + psecshld + psechcd + ptfnuc

    !  Calculate powers relevant to a power-producing plant
    if (ireactor == 1) then

       !  Gross electric power
       !pgrossmw = (pthermmw-hthermmw) * etath
       pgrossmw = pthermmw * etath

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
    call oheadr(outfile,'Cryogenics')
    call ovarre(outfile,'Conduction and radiation heat loads on cryogenic components (MW)', '(qss/1.0D6)', qss/1.0D6, 'OP ')
    call ovarre(outfile,'Nuclear heating of cryogenic components (MW)', '(qnuc/1.0D6)', qnuc/1.0D6, 'OP ')
    if(inuclear==1)call ocmmnt(outfile,'Nuclear heating of cryogenic components is a user input.')
    call ovarre(outfile,'AC losses in cryogenic components (MW)', '(qac/1.0D6)', qac/1.0D6, 'OP ')
    call ovarre(outfile,'Resistive losses in current leads (MW)', '(qcl/1.0D6)', qcl/1.0D6, 'OP ')
    call ovarre(outfile,'45% allowance for heat loads in transfer lines, storage tanks etc (MW)', &
        '(qmisc/1.0D6)', qmisc/1.0D6, 'OP ')

    call ovarre(outfile,'Sum = Total heat removal at cryogenic temperatures (W)', &
        '(helpow/1.0D6)', helpow/1.0D6, 'OP ')
    call ovarre(outfile,'Temperature of cryogenic components (K)', '(tmpcry)', tmpcry)
    call ovarre(outfile,'Efficiency (figure of merit) of cryogenic plant is 13% of ideal Carnot value:', &
        '', (0.13D0*tmpcry)/(293.0D0 - tmpcry), 'OP ')
    call ovarre(outfile,'Electric power for cryogenic plant (MW)', '(crypmw)', crypmw, 'OP ')

    call oheadr(outfile,'Plant Power / Heat Transport Balance')
    if (pnetelmw < 0) then
        call ocmmnt(outfile, 'WARNING: Calculated net electric power is negative')
        call ocmmnt(outfile, '--------------------------------------------------')
    end if

    call osubhd(outfile,'Assumptions :')

    call ovarre(outfile,'Neutron power multiplication in blanket', '(emult)', emult)
    
    if (idivrt == 2) then
        ! Double null configuration
        call ovarre(outfile, 'Double Null Divertor area fraction of whole toroid surface', '(2*fdiv)', 2.0D0*fdiv)
    else
        ! Single null configuration
        call ovarre(outfile, 'Divertor area fraction of whole toroid surface', '(fdiv)', fdiv)
    end if 

    call ovarre(outfile,'H/CD apparatus + diagnostics area fraction', '(fhcd)', fhcd)
    
    if (idivrt == 2) then
        ! Double null configuration
        call ovarre(outfile,'First wall area fraction ', '(1-2*fdiv-fhcd)', 1.0D0-2.0D0*fdiv-fhcd)
    else
        ! Single null configuration
        call ovarre(outfile,'First wall area fraction ', '(1-fdiv-fhcd)', 1.0D0-fdiv-fhcd)
    end if

    call ovarin(outfile, 'Switch for pumping of primary coolant', '(primary_pumping)', primary_pumping)
    if (primary_pumping == 0) then
        call ocmmnt(outfile, 'User sets mechanical pumping power directly')
    else if (primary_pumping == 1) then
        call ocmmnt(outfile, 'User sets mechanical pumping power as a fraction of thermal power removed by coolant')
    else if (primary_pumping == 2) then
        call ocmmnt(outfile, 'Mechanical pumping power is calculated for FW and blanket')
    else if (primary_pumping == 3) then
        call ocmmnt(outfile, 'Mechanical pumping power for FW and blanket cooling loop')
        call ocmmnt(outfile, 'includes heat exchanger, using specified pressure drop')
    end if

    call ovarre(outfile, 'Mechanical pumping power for FW cooling loop including heat exchanger (MW)', &
                       '(htpmw_fw)', htpmw_fw, 'OP ')
    call ovarre(outfile, 'Mechanical pumping power for blanket cooling loop including heat exchanger (MW)', &
                       '(htpmw_blkt)', htpmw_blkt, 'OP ')
    call ovarre(outfile, 'Mechanical pumping power for FW and blanket cooling loop including heat exchanger (MW)', &
                       '(htpmw_fw_blkt)', htpmw_fw_blkt, 'OP ')

    if (primary_pumping /= 3) then
        call ovarre(outfile, 'Mechanical pumping power for FW (MW)', '(htpmw_fw)', htpmw_fw, 'OP ')
        call ovarre(outfile, 'Mechanical pumping power for blanket (MW)', '(htpmw_blkt)', htpmw_blkt, 'OP ')
    endif
    call ovarre(outfile, 'Mechanical pumping power for divertor (MW)', '(htpmw_div)', htpmw_div, 'OP ')
    call ovarre(outfile, 'Mechanical pumping power for shield and vacuum vessel (MW)', '(htpmw_shld)', htpmw_shld, 'OP ')

    call ovarre(outfile, 'Electrical pumping power for FW and blanket (MW)', '(htpmwe_fw_blkt)', htpmwe_fw_blkt, 'OP ')
    call ovarre(outfile, 'Electrical pumping power for shield (MW)', '(htpmwe_shld)', htpmwe_shld, 'OP ')
    call ovarre(outfile, 'Electrical pumping power for divertor (MW)', '(htpmwe_div)', htpmwe_div, 'OP ')
    call ovarre(outfile, 'Total electrical pumping power for primary coolant (MW)', '(htpmw)', htpmw, 'OP ')

    if (primary_pumping==1) then
        call ovarre(outfile, 'Coolant pump power / non-pumping thermal power in first wall', '(fpumpfw)', fpumpfw)
        call ovarre(outfile, 'Coolant pump power / non-pumping thermal power in blanket', '(fpumpblkt)', fpumpblkt)
    end if
    
    if (primary_pumping /= 0) then
        call ovarre(outfile, 'Coolant pump power / non-pumping thermal power in shield', '(fpumpshld)', fpumpshld)
        call ovarre(outfile, 'Coolant pump power / non-pumping thermal power in divertor', '(fpumpdiv)',fpumpdiv)
    end if
    
    call ovarre(outfile, 'Electrical efficiency of heat transport coolant pumps', '(etahtp)', etahtp)
    ! #284
    call osubhd(outfile,'Plant thermodynamics: options :')
    if (iprimdiv == 1) then
        call ocmmnt(outfile, 'Divertor thermal power is collected at only 150 C and is used to &
            &preheat the coolant in the power cycle')
    else if (iprimdiv == 0) then
        call ocmmnt(outfile, 'Divertor thermal power is not used, but rejected directly to the environment.')
    end if
    if (iprimshld == 1) then
        call ocmmnt(outfile, 'Shield thermal power is collected at only 150 C and is used to &
            &preheat the coolant in the power cycle')
    else if (iprimshld == 0) then
        call ocmmnt(outfile, 'Shield thermal power is not used, but rejected directly to the environment.')
    end if

    if (ireactor == 1) then
        if (secondary_cycle == 0) then
            call ocmmnt(outfile,'Power conversion cycle efficiency model: '// &
                'efficiency set according to blanket type (div power to secondary)')
        else if (secondary_cycle == 1) then
            call ocmmnt(outfile,'Power conversion cycle efficiency model: '// &
                'efficiency set according to blanket type (div power to primary)')
            call ovarrf(outfile, 'Thermal to electric conversion efficiency of the power conversion cycle', &
             '(etath)', etath)
        else if (secondary_cycle == 2) then
            call ocmmnt(outfile,'Power conversion cycle efficiency model: user-defined efficiency')
            call ovarrf(outfile, 'Thermal to electric conversion efficiency of the power conversion cycle', &
             '(etath)', etath)
        else if (secondary_cycle == 3) then
            call ocmmnt(outfile,'Power conversion cycle efficiency model: steam Rankine cycle')
        else
            call ocmmnt(outfile,'Power conversion cycle efficiency model: supercritical CO2 cycle')
        end if

        if (secondary_cycle > 2) then
            call ovarrf(outfile, 'Coolant temperature at turbine inlet (K)', '(tturb)', tturb)
        end if
        call ovarrf(outfile, 'Fraction of total high-grade thermal power to divertor', '(pdivfraction)', pdivfraction, 'OP ')

    end if

    call oblnkl(outfile)
    call ocmmnt(outfile, 'Power Balance for Reactor (across vacuum vessel boundary) - Detail')
    call ocmmnt(outfile, '------------------------------------------------------------------')

    if (ignite == 0) then
        pinj = pinjmw
    else
        pinj = 0.0D0
    end if

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
    call ovarrf(outfile, 'Total power leaving reactor (across vacuum vessel boundary) (MW)', '', primsum + secsum + ptfnuc, 'OP ')

    call osubhd(outfile, 'Other secondary thermal power constituents :')
    call ovarrf(outfile,'Heat removal from cryogenic plant (MW)', '(crypmw)', crypmw, 'OP ')
    call ovarrf(outfile,'Heat removal from facilities (MW)', '(fachtmw)', fachtmw, 'OP ')

    call ovarrf(outfile,'Coolant pumping efficiency losses (MW)', '(htpsecmw)', htpsecmw, 'OP ')
    call ovarrf(outfile,'Heat removal from injection power (MW)', '(pinjht)', pinjht, 'OP ')
    call ovarrf(outfile,'Heat removal from tritium plant (MW)', '(trithtmw)', trithtmw, 'OP ')
    call ovarrf(outfile,'Heat removal from vacuum pumps (MW)', '(vachtmw)', vachtmw, 'OP ')
    call ovarrf(outfile,'TF coil resistive power (MW)', '(tfcmw)', tfcmw, 'OP ')

    call oblnkl(outfile)
    call ovarrf(outfile, 'Total low-grade thermal power (MW)', '(psechtmw)', psechtmw, 'OP ')
    call ovarrf(outfile, 'Total High-grade thermal power (MW)', '(pthermmw)', pthermmw, 'OP ')

    call oblnkl(outfile)
    call ovarin(outfile,'Number of primary heat exchangers', '(nphx)', nphx, 'OP ')

    if (ireactor /= 1) return

    ! MDK start
    call oblnkl(outfile)
    call oblnkl(outfile)
    call ocmmnt(outfile,'Power Balance across separatrix :')
    call ocmmnt(outfile,'-------------------------------')
    call ocmmnt(outfile,'Only energy deposited in the plasma is included here.')

    if (iradloss == 0) then
        call ocmmnt(outfile,'Total power loss is scaling power plus radiation (iradloss = 0)')
        call ovarrf(outfile,'Transport power from scaling law (MW)','(pscalingmw)',pscalingmw, 'OP ')
        call ovarrf(outfile,'Total net radiation power (MW)','(pradmw)',pradmw, 'OP ')
        sum = pscalingmw+pradmw
        call ovarrf(outfile,'Total (MW)','',sum, 'OP ')
    else if (iradloss == 1) then
        call ocmmnt(outfile,'Total power loss is scaling power plus core radiation only (iradloss = 1)')
        call ovarrf(outfile,'Transport power from scaling law (MW)','(pscalingmw)',pscalingmw, 'OP ')
        call ovarrf(outfile,'Radiation power from inside "coreradius" (MW)','(pcoreradmw.)',pcoreradmw, 'OP ')
        call ovarrf(outfile,'Total (MW)','',pscalingmw+pcoreradmw, 'OP ')
        sum = pscalingmw+pcoreradmw
    else if (iradloss == 2) then
        call ocmmnt(outfile,'Total power loss is scaling power only (iradloss = 1).')
        call ocmmnt(outfile,'This is not recommended for power plant models.')
        call ovarrf(outfile,'Transport power from scaling law (MW)','(pscalingmw)',pscalingmw, 'OP ')
        call ovarrf(outfile,'Total (MW)','',pscalingmw, 'OP ')
        sum = pscalingmw
    else
        write(*,*) 'The value of iradloss appears to be invalid.'
        call ocmmnt(outfile,'ERROR: The value of iradloss appears to be invalid.')
    end if

    call oblnkl(outfile)
    call ovarrf(outfile,'Alpha power deposited in plasma (MW)','(falpha*palpmw)',falpha*palpmw, 'OP ')
    call ovarrf(outfile,'Power from charged products of DD and/or D-He3 fusion (MW)','(pchargemw.)',pchargemw, 'OP ')
    call ovarrf(outfile,'Ohmic heating (MW)','(pohmmw.)',pohmmw, 'OP ')
    !if (ignite == 1) then
    !    call ovarrf(outfile,'Total (MW)','',falpha*palpmw+pchargemw+pohmmw, 'OP ')
    !    call oblnkl(outfile)
    !    if (abs(sum - (falpha*palpmw+pchargemw+pohmmw)) > 5.0D0) then
    !        write(*,*) 'WARNING: Power balance across separatrix is in error by more than 5 MW.'
    !    call ocmmnt(outfile,'WARNING: Power balance across separatrix is in error by more than 5 MW.')
    !    end if
    !else
        call ovarrf(outfile,'Injected power deposited in plasma (MW)','(pinjmw)',pinj, 'OP ')
        call ovarrf(outfile,'Total (MW)','',falpha*palpmw+pchargemw+pohmmw+pinj, 'OP ')
        call oblnkl(outfile)
        if (abs(sum - (falpha*palpmw+pchargemw+pohmmw+pinj)) > 5.0D0) then
            write(*,*) 'WARNING: Power balance across separatrix is in error by more than 5 MW.'
        call ocmmnt(outfile,'WARNING: Power balance across separatrix is in error by more than 5 MW.')
        end if
    !end if 

    call ocmmnt(outfile,'Power Balance for Reactor - Summary :')
    call ocmmnt(outfile,'-------------------------------------')
    call ovarrf(outfile,'Fusion power (MW)','(powfmw.)',powfmw, 'OP ')
    call ovarrf(outfile,'Power from energy multiplication in blanket and shield (MW)','(emultmw)',emultmw, 'OP ')
    call ovarrf(outfile,'Injected power (MW)','(pinjmw.)',pinj, 'OP ')
    call ovarrf(outfile,'Ohmic power (MW)','(pohmmw.)',pohmmw, 'OP ')
    call ovarrf(outfile,'Power deposited in primary coolant by pump (MW)','(htpmw_mech)',htpmw_mech, 'OP ')
    sum = powfmw+emultmw+pinj+htpmw_mech+pohmmw
    call ovarrf(outfile,'Total (MW)','',sum, 'OP ')
    call oblnkl(outfile)
    !call ovarrf(outfile,'Heat extracted from armour and first wall (MW)','(pthermfw)',pthermfw, 'OP ')
    call ovarrf(outfile,'Heat extracted from first wall and blanket (MW)','(pthermfw_blkt)',pthermfw_blkt, 'OP ')
    call ovarrf(outfile,'Heat extracted from shield  (MW)','(pthermshld)',pthermshld, 'OP ')
    call ovarrf(outfile,'Heat extracted from divertor (MW)','(pthermdiv)',pthermdiv, 'OP ')
    call ovarrf(outfile,'Nuclear and photon power lost to H/CD system (MW)','(psechcd)',psechcd, 'OP ')
    call ovarrf(outfile,'Total (MW)','',pthermfw_blkt+pthermshld+pthermdiv+psechcd, 'OP ')
    call oblnkl(outfile)
    if (abs(sum - (pthermfw_blkt+pthermshld+pthermdiv+psechcd)) > 5.0D0) then
       write(*,*) 'WARNING: Power balance for reactor is in error by more than 5 MW.'
       call ocmmnt(outfile,'WARNING: Power balance for reactor is in error by more than 5 MW.')
    end if


    ! Heat rejected by main power conversion circuit
    rejected_main = pthermmw * (1 - etath)

    call ocmmnt(outfile,'Electrical Power Balance :')
    call ocmmnt(outfile,'--------------------------')
    call ovarrf(outfile,'Net electric power output(MW)','(pnetelmw.)',pnetelmw, 'OP ')
    call ovarrf(outfile,'Required Net electric power output(MW)','(pnetelin)',pnetelin)
    call ovarrf(outfile,'Electric power for heating and current drive (MW)','(pinjwp)',pinjwp, 'OP ')
    call ovarrf(outfile,'Electric power for primary coolant pumps (MW)','(htpmw)',htpmw, 'OP ')
    call ovarrf(outfile,'Electric power for vacuum pumps (MW)','(vachtmw)',vachtmw)
    call ovarrf(outfile,'Electric power for tritium plant (MW)','(trithtmw)',trithtmw)
    call ovarrf(outfile,'Electric power for cryoplant (MW)','(crypmw)',crypmw, 'OP ')
    call ovarrf(outfile,'Electric power for TF coils (MW)','(tfacpd)',tfacpd, 'OP ')
    call ovarrf(outfile,'Electric power for PF coils (MW)','(pfwpmw)', pfwpmw, 'OP ')
    call ovarrf(outfile,'All other internal electric power requirements (MW)','(fachtmw)', fachtmw, 'OP ')
    sum = pnetelmw+pinjwp+htpmw+vachtmw+trithtmw+crypmw+tfacpd+fachtmw+pfwpmw
    call ovarrf(outfile,'Total (MW)','',sum, 'OP ')
    call oblnkl(outfile)
    call ovarrf(outfile,'Gross electrical output* (MW)','(pgrossmw)',pgrossmw, 'OP ')
    call ocmmnt(outfile,'(*Power for pumps in secondary circuit already subtracted)')
    call oblnkl(outfile)
    if (abs(sum - pgrossmw) > 5.0D0) then
       write(*,*) 'WARNING: Electrical Power balance is in error by more than 5 MW.'
       call ocmmnt(outfile,'WARNING: Electrical Power balance is in error by more than 5 MW.')
    end if

    call ocmmnt(outfile,'Power balance for power plant :')
    call ocmmnt(outfile,'-------------------------------')
    call ovarrf(outfile,'Fusion power (MW)','(powfmw.)',powfmw, 'OP ')
    call ovarrf(outfile,'Power from energy multiplication in blanket and shield (MW)','(emultmw)',emultmw, 'OP ')
    sum = powfmw + emultmw
    call ovarrf(outfile,'Total (MW)','',sum, 'OP ')
    call oblnkl(outfile)
    call ovarrf(outfile,'Net electrical output (MW)	','(pnetelmw)',pnetelmw, 'OP ')
    call ovarrf(outfile,'Heat rejected by main power conversion circuit (MW)','(rejected_main)',rejected_main, 'OP ')
    call ovarrf(outfile,'Heat rejected by other cooling circuits (MW)','(psechtmw)',psechtmw, 'OP ')
    call ovarrf(outfile,'Total (MW)','',pnetelmw + rejected_main + psechtmw, 'OP ')
    call oblnkl(outfile)
    if (abs(sum - (pnetelmw + rejected_main + psechtmw)) > 5.0D0) then
       write(*,*) 'WARNING: Power balance for power plant is in error by more than 5 MW.'
       call ocmmnt(outfile,'WARNING: Power balance for power plant is in error by more than 5 MW.')
    end if

    call osubhd(outfile,'Plant efficiency measures :')
    call ovarrf(outfile,'Net electric power / total nuclear power (%)', '(pnetelmw/(powfmw+emultmw)', &
        100.0D0*pnetelmw/(powfmw+emultmw), 'OP ')
    call ovarrf(outfile,'Net electric power / total fusion power (%)', '(pnetelmw/powfmw)', &
        100.0D0*pnetelmw/powfmw, 'OP ')
    call ovarrf(outfile,'Gross electric power* / high grade heat (%)', '(etath)', 100.0D0*etath)
    call ocmmnt(outfile,'(*Power for pumps in secondary circuit already subtracted)')
    call ovarrf(outfile,'Recirculating power fraction', '(cirpowfr)', cirpowfr, 'OP ')


  end subroutine power2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine power3(outfile,iprint)

    !! Calculates the time-dependent power requirements
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output (1=yes)
    !! This routine calculates the time dependent power requirements
    !! and outputs them to the output file
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use current_drive_variables, only: etacd
    use heat_transport_variables, only: htpmw, pinjmax, crypmw, vachtmw, &
        tfacpd, trithtmw, pinjwp, fachtmw, pgrossmw
    use pf_power_variables, only: poloidalpower
    use process_output, only: osubhd, oblnkl
    use times_variables, only: tramp, tburn, theat, tdwell, tqnch, tohs
    use constants, only: mfile
    implicit none

    ! Arguments
    integer, intent(in) :: outfile, iprint

    ! Local variables
    real(dp) :: t_cs, t_ip_up, t_heat, t_flat_top, t_ip_down, t_extra

    ! Total continuous power
    real(dp), dimension(6) :: p_cont_tot

    ! Total intermittent power
    real(dp), dimension(6) :: p_int_tot

    ! Power arrays
    ! Primary cooling power array (MW)
    real(dp), dimension(6) :: p_cooling

    ! Vacuum system power array (MW)
    real(dp), dimension(6) :: p_vac

    ! Cyroplant system power array (MW)
    real(dp), dimension(6) :: p_cryo

    ! Heating and current drive power array (MW)
    real(dp), dimension(6) :: p_hcd

    ! Tritium system power array (MW)
    real(dp), dimension(6) :: p_tritium

    ! Facilities power array (MW)
    real(dp), dimension(6) :: p_fac

    ! TF coil system power array (MW)
    real(dp), dimension(6) :: p_tf

    ! PF coil system power array (MW)
    real(dp), dimension(6) :: p_pf

    ! Gross electric power array
    real(dp), dimension(6) :: p_gross

    ! Net electric power array
    real(dp), dimension(6) :: p_net

    ! Net electric power average
    real(dp) :: p_net_avg

    ! Times
    ! Central solenoid pre-magnetisation time (s)
    t_cs = tramp

    ! Plasma current ramp up time (s)
    t_ip_up = tohs

    ! Plasma heating phase (s)
    t_heat = theat

    ! Flat-top phase (s)
    t_flat_top = tburn

    ! Plasma current ramp down time (s)
    t_ip_down = tqnch

    ! Extra time between pulses (s)
    t_extra = tdwell

    ! Continuous power usage

    ! Primary pumping electrical power [MWe]
    p_cooling(1:6) = htpmw

    ! Cryoplant electrical power [MWe]
    p_cryo(1:6) = crypmw

    ! Vacuum electrical power [MWe]
    p_vac(1:6) = vachtmw

    ! Tritium system electrical power [MWe]
    p_tritium(1:6) = trithtmw

    ! Facilities electrical power [MWe]
    p_fac(1:6) = fachtmw

    ! TF coil electrical power [MWe]
    p_tf(1:6) = tfacpd

    ! Total continuous power [MWe]
    p_cont_tot = p_cooling + p_cryo + p_vac + p_tritium + p_fac + p_tf

    ! Intermittent power usage

    ! Heating and current drive electrical power [MWe]
    p_hcd(1) = 0.0D0
    p_hcd(2) = pinjmax/etacd
    p_hcd(3) = pinjmax/etacd
    p_hcd(4) = pinjwp
    p_hcd(5) = pinjmax/etacd
    p_hcd(6) = 0.0D0

    ! PF coils electrical power [MWe]
    p_pf(1) = poloidalpower(1)/1.0D6
    p_pf(2) = poloidalpower(2)/1.0D6
    p_pf(3) = poloidalpower(3)/1.0D6
    p_pf(4) = poloidalpower(4)/1.0D6
    p_pf(5) = poloidalpower(5)/1.0D6
    p_pf(6) = 0.0D0

    ! Total intermittent power [MWe]
    p_int_tot(1:6) = p_pf + p_hcd

    ! Gross power [MWe]
    p_gross(1:3) = 0.0D0
    p_gross(4) = pgrossmw
    p_gross(5:6) = 0.0D0

    ! Net electric power [MWe]
    p_net =  p_gross - (p_cooling + p_cryo + p_vac + p_fac + p_tritium + p_tf + p_pf + p_hcd)

    ! Net electric power average [MWe]
    p_net_avg = ((p_net(1)*t_cs) + (p_net(2)*t_ip_up) + (p_net(3)*t_heat) + &
        (p_net(4)*t_flat_top) + (p_net(5)*t_ip_down) + (p_net(6)*t_extra)) / &
        (t_cs + t_ip_up + t_heat + t_flat_top + t_ip_down + t_extra)

    ! Output
    if (iprint == 0) return

    call osubhd(outfile,'Time-dependent power usage')

    write(outfile,'(t10,a)') 'Pulse timings [s]:'
    call oblnkl(outfile)
    write(outfile,10) "", "tramp", "tohs", "theat", "tburn", "tqnch", "tdwell"
    write(outfile,10) "", "-----", "----", "-----", "-----", "-----", "------"
    write(outfile,20) "Duration", t_cs, t_ip_up, t_heat, t_flat_top, t_ip_down, t_extra
    write(outfile,10) "------", "-----", "----", "-----", "-----", "-----", "------"
    call oblnkl(outfile)

    write(outfile,'(t10,a)') 'Continous power usage [MWe]:'
    call oblnkl(outfile)
    write(outfile,10) "System", "tramp", "tohs", "theat", "tburn", "tqnch", "tdwell"
    write(outfile,10) "------", "-----", "----", "-----", "-----", "-----", "------"
    write(outfile,20) "Primary cooling", p_cooling(1), p_cooling(2), p_cooling(3), p_cooling(4), p_cooling(5), p_cooling(6)
    write(outfile,20) "Cyroplant", p_cryo(1), p_cryo(2), p_cryo(3), p_cryo(4), p_cryo(5), p_cryo(6)
    write(outfile,20) "Vacuum", p_vac(1), p_vac(2), p_vac(3), p_vac(4), p_vac(5), p_vac(6)
    write(outfile,20) "Tritium", p_tritium(1), p_tritium(2), p_tritium(3), p_tritium(4), p_tritium(5), p_tritium(6)
    write(outfile,20) "TF", p_tf(1), p_tf(2), p_tf(3), p_tf(4), p_tf(5), p_tf(6)
    write(outfile,20) "Facilities", p_fac(1), p_fac(2), p_fac(3), p_fac(4), p_fac(5), p_fac(6)
    write(outfile,10) "------", "-----", "----", "-----", "-----", "-----", "------"
    write(outfile,20) "Total", p_cont_tot(1), p_cont_tot(2), p_cont_tot(3), p_cont_tot(4), p_cont_tot(5), p_cont_tot(6)
    write(outfile,10) "------", "-----", "----", "-----", "-----", "-----", "------"
    call oblnkl(outfile)

    write(outfile,'(t10,a)') 'Intermittent power usage [MWe]:'
    call oblnkl(outfile)
    write(outfile,10) "System", "tramp", "tohs", "theat", "tburn", "tqnch", "tdwell"
    write(outfile,10) "------", "-----", "----", "-----", "-----", "-----", "------"
    write(outfile,20) "H & CD", p_hcd(1), p_hcd(2), p_hcd(3), p_hcd(4), p_hcd(5), p_hcd(6)
    write(outfile,20) "PF", p_pf(1), p_pf(2), p_pf(3), p_pf(4), p_pf(5), p_pf(6)
    write(outfile,10) "------", "-----", "----", "-----", "-----", "-----", "------"
    write(outfile,20) "Total", p_int_tot(1), p_int_tot(2), p_int_tot(3), p_int_tot(4), p_int_tot(5), p_int_tot(6)
    write(outfile,10) "------", "-----", "----", "-----", "-----", "-----", "------"

    call oblnkl(outfile)

    write(outfile,'(t10,a)') 'Power production [MWe]:'
    call oblnkl(outfile)
    write(outfile,30) "", "tramp", "tohs", "theat", "tburn", "tqnch", "tdwell", "avg"
    write(outfile,30) "", "-----", "----", "-----", "-----", "-----", "------", "---"
    write(outfile,20) "Gross power", p_gross(1), p_gross(2), p_gross(3), p_gross(4), p_gross(5), p_gross(6)
    write(outfile,40) "Net power", p_net(1), p_net(2), p_net(3), p_net(4), p_net(5), p_net(6), p_net_avg
    write(outfile,10) "------", "-----", "----", "-----", "-----", "-----", "------"

    call oblnkl(outfile)

    10     format(t20,a20,t40,a8,t50,a8,t60,a8,t70,a8,t80,a8,t90,a8)
    20     format(t20,a20,t40,f8.2,t50,f8.2,t60,f8.2,t70,f8.2,t80,f8.2,t90,f8.2,t100,f8.2)
    30     format(t20,a20,t40,a8,t50,a8,t60,a8,t70,a8,t80,a8,t90,a8,t100,a8)
    40     format(t20,a20,t40,f8.2,t50,f8.2,t60,f8.2,t70,f8.2,t80,f8.2,t90,f8.2,t100,f8.2,t110,f8.2)

  end subroutine power3

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cryo(i_tf_sup, tfsai, coldmass, ptfnuc, ensxpfm, tpulse, cpttf, n_tf, helpow)

    !! Calculates cryogenic loads
    !! author: P J Knight, CCFE, Culham Science Centre
    !! itfsup : input integer : Switch denoting whether TF coils are
    !! superconducting
    !! tfsai : input real : Inboard TF coil surface area (m2)
    !! coldmass : input real : Mass of cold (cryogenic) components (kg),
    !! including TF coils, PF coils, cryostat, and
    !! intercoil structure
    !! ptfnuc : input real : Nuclear heating in TF coils (MW)
    !! ensxpfm : input real : Maximum PF coil stored energy (MJ)
    !! tpulse : input real : Pulse length of cycle (s)
    !! cpttf : input real : Current per turn in TF coils (A)
    !! tfno : input real : Number of TF coils
    !! helpow : output real : Helium heat removal at cryo temperatures (W)
    !! This routine calculates the cryogenic heat load.
    !! D. Slack memo SCMDG 88-5-1-059, LLNL ITER-88-054, Aug. 1988
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fwbs_variables, only: qnuc, inuclear
    use constants, only: pi
    implicit none

    !  Arguments

    integer, intent(in) :: i_tf_sup
    real(dp), intent(in) :: coldmass,cpttf,ensxpfm,ptfnuc,n_tf, &
         tfsai,tpulse
    real(dp), intent(out) :: helpow

    !  Local variables

    !real(dp) :: qac,qcl,qnuc,qss

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Steady state loads (W)
    qss = 4.3D-4 * coldmass
    if ( i_tf_sup == 1 ) qss = qss + 2.0D0*tfsai

    !  Nuclear heating of TF coils (W) (zero if resistive)
    if( inuclear == 0 .and. i_tf_sup == 1) qnuc = 1.0D6 * ptfnuc
    ! Issue #511: if inuclear = 1 then qnuc is input.

    !  AC losses
    qac = 1.0D3 * ensxpfm/tpulse

    !  Current leads
    if (i_tf_sup == 1) then
       qcl = 13.6D-3 * n_tf * cpttf
    else
       qcl = 0.0D0
    end if

    !  45% extra miscellaneous, piping and reserves
    qmisc = 0.45D0 * (qss + qnuc + qac + qcl)
    helpow = max(0.0D0, qmisc + qss + qnuc + qac + qcl)

  end subroutine cryo

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine plant_thermal_efficiency(etath)

    !! Calculates the thermal efficiency of the power conversion cycle
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: C Harrington, CCFE, Culham Science Centre
    !! etath : input/output real : thermal to electric conversion efficiency
    !! This routine calculates the thermal efficiency of the power conversion cycle.
    !! This gives the gross power of the plant, i.e. the primary coolant pumping
    !! power is not subtracted at this point; however, the pumping of the
    !! secondary coolant is accounted for.
    !! <P>If secondary_cycle = 0, 1, a set efficiency for the chosen blanket design is used,
    !! taken from cycle modelling studies.
    !! <P>If secondary_cycle > 1, the outlet temperature from the first wall
    !! and breeder zone is used to calculate an efficiency, using a simple relationship
    !! between etath and outlet_temp again obtained from previous studies.
    !! C. Harrington, K:\Power Plant Physics and Technology \ PROCESS \ blanket_model
    !! \ New Power Module Harrington \ Cycle correlations \ Cycle correlations.xls
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use error_handling, only: fdiags, idiags, report_error
    use fwbs_variables, only: iblanket, secondary_cycle, outlet_temp
    use heat_transport_variables, only: tturb
    use constants, only: pi
    implicit none

    !  Arguments
    real(kind=double), intent(inout) :: etath

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Etath from reference. Div power not to primary
    if (secondary_cycle == 0) then

       !  CCFE HCPB Model (with or without TBR)
       if ((iblanket == 1).or.(iblanket == 3)) then
          !  HCPB, efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX Feedheat & reheat cycle assumed
          etath = 0.411D0

       !  KIT HCPB model
       else if (iblanket == 2) then
          !  HCPB, efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX Feedheat & reheat cycle assumed
          etath = 0.411D0
       else
          write(*,*) 'iblanket does not have a value in range 1-3.'
       end if

    !  Etath from reference. Div power to primary
    else if (secondary_cycle == 1) then

        !  CCFE HCPB Model (with or without TBR)
        if ((iblanket == 1).or.(iblanket == 3)) then
          !  HCPB, efficiency taken from WP12-DAS08-T01, EFDA_D_2LLNBX Feedheat & reheat cycle assumed
          etath = 0.411D0 - delta_eta

        !  KIT HCPB model
        else if (iblanket == 2) then
          etath = 0.411D0 - delta_eta
        else
          write(*,*) 'iblanket does not have a value in range 1-3.'
        end if

    !  User input used, etath not changed
    else if (secondary_cycle == 2) then
       ! Do nothing

    !  Steam Rankine cycle to be used
    else if (secondary_cycle == 3) then

        !  CCFE HCPB Model (with or without TBR)
        if ((iblanket == 1).or.(iblanket == 3)) then
          !  If coolant is helium, the steam cycle is assumed to be superheated
          !  and a different correlation is used. The turbine inlet temperature
          !  is assumed to be 20 degrees below the primary coolant outlet
          !  temperature, as was stated as practical in EFDA_D_2LLNBX.

          !  Superheated steam Rankine cycle correlation (C. Harrington)
          !  Range of validity: 657 K < tturb < 915 K
          tturb = outlet_temp - 20.0D0
          if ((tturb < 657.0D0).or.(tturb > 915.0D0)) then
             idiags(1) = 2 ; fdiags(1) = tturb
             call report_error(166)
          end if

          etath = 0.1802D0*log(tturb) - 0.7823 - delta_eta

       !  KIT HCPB Model
       else if (iblanket == 2) then
          !  Same as iblanket = 1
          tturb = outlet_temp - 20.0D0
          if ((tturb < 657.0D0).or.(tturb > 915.0D0)) then
             idiags(1) = 2 ; fdiags(1) = tturb
             call report_error(166)
          end if
          etath = 0.1802D0*log(tturb) - 0.7823 - delta_eta
       else
          write(*,*) 'iblanket does not have a value in range 1-3.'
       end if

    !  Supercritical CO2 cycle to be used
    else if (secondary_cycle == 4) then
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

    else
        write(*,*) 'secondary_cycle does not appear to have a value within its range (0-4)'
    end if

  end subroutine plant_thermal_efficiency

end module power_module
