!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module rfp_module

  !+ad_name  rfp_module
  !+ad_summ  Module containing Reversed Field Pinch device routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  rfptfc
  !+ad_cont  rfppfc
  !+ad_cont  rfppfp
  !+ad_cont  rfpphy
  !+ad_cont  ftheta
  !+ad_cont  efcurr
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of a Reversed Field Pinch fusion power plant.
  !+ad_prob  None
  !+ad_call  build_module
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  current_drive_module
  !+ad_call  current_drive_variables
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  pf_power_variables
  !+ad_call  pfcoil_module
  !+ad_call  pfcoil_variables
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  process_input
  !+ad_call  process_output
  !+ad_call  pulse_variables
  !+ad_call  rfp_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  05/11/12 PJK Initial version of module
  !+ad_hist  05/11/12 PJK Added pulse_variables
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_module
  use build_variables
  use constants
  use current_drive_module
  use current_drive_variables
  use fwbs_variables
  use heat_transport_variables
  use pf_power_variables
  use pfcoil_module
  use pfcoil_variables
  use physics_module
  use physics_variables
  use process_input, only : check_range_real
  use process_output
  use pulse_variables
  use rfp_variables
  use tfcoil_variables
  use times_variables

  implicit none

  private
  public :: rfppfc,rfppfp,rfpphy,rfptfc

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rfptfc(outfile,iprint)

    !+ad_name  rfptfc
    !+ad_summ  Routine to calculate TF coil information for the reversed field
    !+ad_summ  pinch model
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine calculates various parameters for the TF coil set
    !+ad_desc  of a reversed field pinch machine. The coils are assumed to be
    !+ad_desc  circular.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_call  portsz
    !+ad_hist  27/02/96 PJK Initial version
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  30/10/12 PJK Added build_module
    !+ad_hist  05/11/12 PJK Added rfp_variables
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: ltfleg,rin,rout,tfcind1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Radius of centre of inboard TF coil leg

    rtfcin = bore + ohcth + gapoh + bcylth + 0.5D0*tfcth

    !  Radius of outer edge of inboard TF coil leg

    rin = rtfcin + 0.5D0*tfcth

    !  Unlike in a tokamak, the inboard legs do not necessarily form
    !  a continuous ring. tftort is calculated ensuring that adjacent
    !  coils are not wider than their inboard edge centres are apart
    !  (constraint equation 47).

    !  Total area of inboard legs (coils have rectangular cross-section)

    tfareain = tfno * tfcth * tftort

    !  Total current in TF coils (A)

    ritfc = oacdcp * tfareain

    !  Peak toroidal field and radius of its occurrence

    rbmax = rin
    bmaxtf = 2.0D-7 * ritfc / rbmax

    !  Radius of inner edge of outboard TF coil leg

    rout = rtot - 0.5D0*tfcth

    !  Centering and vertical forces

    if (bore == 0.0D0) then
       cforce = 0.0D0
    else
       cforce = bmaxtf * ritfc/(2.0D0*tfno)
    end if
    vforce = 0.55D0 * bt * rmajor * 0.5D0*ritfc * log(rout/rin)/tfno

    !  Bore (gap between inner and outer TF coil legs)

    tfboreh = rtot - rbmax - 0.5D0*tfcth

    !  Number of turns per leg

    turnstf = ritfc / (tfno * cpttf)

    !  Outer leg information (per leg)

    !  Cross-sectional area

    arealeg = tfcth * tftort

    !  Length (circular coils, half assigned to 'outboard')

    ltfleg = 0.5D0 * pi * (tfboreh+tfcth)

    !  Volume

    voltfleg = ltfleg * arealeg

    !  Resistance

    rhotfleg = ltfleg * tflegres/arealeg

    !  Total weight (of all legs), assuming copper

    whttflgs = voltfleg * tfno * (1.0D0-vftf) * 8900.0D0

    !  Inner leg information (all legs)

    !  Volume

    volcp = voltfleg * tfno

    !  Weight of conductor

    whtcp = volcp * 8900.0D0 * (1.0D0-fcoolcp)

    !  Total weight of TF coils

    whttf = whtcp + whttflgs

    !  Resistance

    rhocp = 1.0D-8 * (1.72D0 + 0.0039D0*tcpav) / 0.92D0

    !  Resistive power losses

    prescp = rhocp * (ritfc / (tfareain*(1.0D0-fcoolcp)) )**2 &
         * volcp * (1.0D0-fcoolcp)

    !  Stress information (radial, tangential, vertical)

    sigrad = 1.0D-6 * bmaxtf**2 * (5.0D0 + 0.34D0) / &
         (8.0D0 * rmu0 * (1.0D0-fcoolcp))
    sigtan = sigrad
    sigver = 0.0D0

    !  Inductance (N.B. hmax may not be equal to the coil radius)

    tfcind1 = hmax * rmu0/pi * log(rout/rin)

    !  Stored energy per coil (GJ)

    estotf = 0.5D-9 * tfcind1 * ritfc**2/tfno

    !  Port size

    call portsz

    if ((iprint == 0).or.(sect07 == 0)) return

    !  Output section

    call oheadr(outfile,'TF Coils')
    call ovarre(outfile,'TF coil current (A)','(ritfc)',ritfc)
    call ovarre(outfile,'TF coil current density (A/m2)','(oacdcp)',oacdcp)
    call ovarre(outfile,'Peak field at the TF coils (T)','(bmaxtf)',bmaxtf)
    call ovarre(outfile,'Ripple at plasma edge (%)','(ripple)',ripple)
    call ovarre(outfile,'Allowable ripple (%)','(ripmax)',ripmax)
    call ovarre(outfile,'Number of TF coil legs','(tfno)',tfno)

    call osubhd(outfile,'Energy and Forces :')
    call ovarre(outfile,'Stored energy per coil (GJ)','(estotf)',estotf)
    call ovarre(outfile,'Vertical force on inner leg (N)','(vforce)',vforce)
    call ovarre(outfile,'Centering force on inner leg (N/m)','(cforce)',cforce)
    call ovarre(outfile,'Radial stress (Pa)','(sigrad)',sigrad)
    call ovarre(outfile,'Transverse stress (Pa)','(sigtan)',sigtan)
    call ovarre(outfile,'Vertical stress (Pa)','(sigver)',sigver)
    call ovarre(outfile,'Weight of inner legs (kg)','(whtcp)',whtcp)
    call ovarre(outfile,'Total TF coil weight (kg)','(whttf)',whttf)
    call ovarre(outfile,'Inner leg resistive power (W)','(prescp)',prescp)
    call ovarre(outfile,'Average conductor temperature (C)','(tcpav)',tcpav)

  end subroutine rfptfc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rfppfc(outfile,iprint)

    !+ad_name  rfppfc
    !+ad_summ  Routine to calculate PF coil information for the reversed field
    !+ad_summ  pinch model
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine calculates various parameters for the PF coil set
    !+ad_desc  of a reversed field pinch machine. The coils are scaled from the
    !+ad_desc  TITAN-I OH/EF coil set.
    !+ad_prob  bpf, bpf2, forcepf are never set...
    !+ad_call  bfield
    !+ad_call  efcurr
    !+ad_call  oblnkl
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  29/02/96 PJK Initial version
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_module
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    real(kind(1.0D0)), dimension(nrfppf) :: bpf2,cc,curdpf,curpf, &
         drr,dzz,rc,xc,zc

    integer :: i,j,jj,k,nc
    real(kind(1.0D0)) :: areaspf,br,bri,bro,bz,bzi,bzo,cmass,dens, &
         forcepf,psi,rl,rp,zp,sqrpi

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  The OH coils are the first 14 PF coils
    !  The superconducting EF coils are PF coils 15 and 16

    if (iprint == 0) then

       !  Number of turns in each coil

       nturns(1) = 10.0D0
       nturns(2) = 28.0D0
       nturns(3) = 10.0D0
       nturns(4) = 30.0D0
       nturns(5) = 30.0D0
       nturns(6) = 52.0D0
       nturns(7) = 26.0D0

       nturns(15) = 88.0D0
       nturns(16) = 88.0D0

       !  Coil cross-sectional dimensions

       drpf(1) = 0.20D0 ; dzpf(1) = 0.30D0
       drpf(2) = 0.40D0 ; dzpf(2) = 0.42D0
       drpf(3) = 0.30D0 ; dzpf(3) = 0.20D0
       drpf(4) = 0.30D0 ; dzpf(4) = 0.60D0
       drpf(5) = 0.60D0 ; dzpf(5) = 0.30D0
       drpf(6) = 0.40D0 ; dzpf(6) = 0.78D0
       drpf(7) = 0.40D0 ; dzpf(7) = 0.39D0

       drpf(15) = 0.70D0 ; dzpf(15) = 0.70D0
       drpf(16) = 0.70D0 ; dzpf(16) = 0.70D0

       !  Offset locations from plasma major radius normalised to the
       !  TF coil external radius (1.125m for TITAN-II)

       drr(1) =  1.8667D0 ; dzz(1) = 1.0667D0
       drr(2) =  0.1867D0 ; dzz(2) = 2.1422D0
       drr(3) = -0.5422D0 ; dzz(3) = 2.1333D0
       drr(4) = -1.0578D0 ; dzz(4) = 1.4667D0
       drr(5) = -1.4667D0 ; dzz(5) = 1.2711D0
       drr(6) = -1.5556D0 ; dzz(6) = 0.7200D0
       drr(7) = -1.5556D0 ; dzz(7) = 0.1867D0

       drr(15) = 2.3111D0 ; dzz(15) =  2.2133D0
       drr(16) = 2.3111D0 ; dzz(16) = -2.2133D0

       do i = 8,14
          drr(i) =  drr(i-7)
          dzz(i) = -dzz(i-7)
          nturns(i) = nturns(i-7)
          drpf(i) = drpf(i-7)
          dzpf(i) = dzpf(i-7)
       end do

       !  Scaled coil locations (largest coil radius is stored in pfrmax)

       pfrmax = 0.0D0
       do i = 1,nrfppf
          rrpf(i) = rmajor + (hmax+tfcth)*drr(i)
          zzpf(i) =          (hmax+tfcth)*dzz(i)
          pfrmax = max(pfrmax, (rrpf(i)+0.5D0*drpf(i)) )
       end do

       !  Current per turn in OH coils
       !  TITAN-I has 75 kA/turn for a plasma current of 17.8 MA

       do i = 1,14
          cptrfp(i) = 7.5D4/17.8D6 * plascur
       end do

       !  Current per turn in EF coils
       !  TITAN-I has -109.32 kA/turn for a plasma current of 17.8 MA

       cptrfp(15) = -1.0932D5/17.8D6 * plascur
       cptrfp(16) = -1.0932D5/17.8D6 * plascur

       !  Scale correctly to obtain correct vertical field at plasma

       call efcurr(plascur,rmajor,rminor,betap,rli,nrfppf,rrpf,zzpf, &
            nturns,cptrfp)

       !  Total currents and current densities (ignoring void fractions)

       do i = 1,nrfppf
          curpf(i) =  cptrfp(i) * nturns(i)
          curdpf(i) = curpf(i)/(drpf(i)*dzpf(i))
       end do

       !  Resistances and resistive power losses
       !  (EF coils 15 and 16 are superconducting)

       powpfres = 0.0D0
       do i = 1,14
          resrfp(i) = pfclres * 2.0D0 * pi * rrpf(i) * (nturns(i)**2) / &
               (drpf(i)*dzpf(i)*(1.0D0-vf(i)))
          powpfres = powpfres + (curpf(i)/nturns(i))**2 * resrfp(i)
       end do
       resrfp(15) = 0.0D0
       resrfp(16) = 0.0D0

       !  Masses (highest individual coil mass (tonnes) is stored in pfmmax)

       pfmmax = 0.0D0
       whtpf = 0.0D0
       do i = 1,nrfppf
          if (i <= 14) then
             dens = dcopper
          else
             dens = dcond(isumatpf)
          end if
          cmass = 2.0D0*pi*rrpf(i)*drpf(i)*dzpf(i)*dens*(1.0D0-vf(i))
          pfmmax = max(pfmmax, 1.0D-3*cmass)
          whtpf = whtpf + cmass
       end do

       !  Peak fields at coils; bri etc. never calculated...
       !  (Also, should be in an i loop!)
       !bpf(i) = sqrt(bri**2 + bzi**2)
       !bpf2(i) = sqrt(bro**2 + bzo**2)

       !  Assume 500 MPa stress limit, 2/3 of the force is supported
       !  in the outer (steel) case

       forcepf = 0.0D0  !  Never set otherwise; should be JxB force
       areaspf = 0.666D0 * forcepf / 5.0D8

       whtpfs = areaspf * 2.0D0*pi*rrpf(15) * denstl

       !  Assume that the additional steel shielding the superconductors
       !  has the same cross-sectional area as the coils themselves

       whtpfs = whtpfs + 2.0D0*pi*rrpf(15)*drpf(15)*dzpf(15)*denstl

       !  Double the steel mass, as there are two coils

       whtpfs = 2.0D0*whtpfs

       !  Mutual inductances

       sqrpi = sqrt(pi)
       nc = nrfppf-1
       do i = 1,nrfppf

          do j = 1,nrfppf-1
             if (j >= i) then
                jj = j+1
             else
                jj = j
             end if
             rc(j) = rrpf(jj)
             zc(j) = zzpf(jj)
          end do

          rp = rrpf(i)
          zp = zzpf(i)

          call bfield(nrfppf,nc,rc,zc,cc,xc,rp,zp,br,bz,psi)

          do k = 1,nrfppf
             if (k < i)  then
                sxlg(i,k) = xc(k) * nturns(k) * nturns(i)
             else if (k == i)  then
                rl = dzpf(k)/sqrpi
                sxlg(k,k) = rmu0 * nturns(k)**2 * rrpf(k) * &
                     (log(8.0D0*rrpf(k)/rl) - 1.75D0)
             else
                sxlg(i,k) = xc(k-1) * nturns(k) * nturns(i)
             end if
          end do

       end do

    end if

    if ((iprint == 0).or.(sect08 == 0)) return

    !  Output section

    call oheadr(outfile,'PF Coils')
    call ovarre(outfile,'Total mass of PF coils (kg)','(whtpf)',whtpf)
    call ovarre(outfile,'Mass of EF coil steel shield (kg)','(whtpfs)',whtpfs)
    call ovarre(outfile,'PF coil resistive power (W)','(powpfres)',powpfres)
    call oblnkl(outfile)

    write(outfile,140)
140 format(' coil',t12,'R(m)',t20,'Z(m)',t28,'dR(m)',t36,'dZ(m)', &
         t43,'turns',t51,'I(MA)',t58,'J(MA/m2)')

    call oblnkl(outfile)

    write(outfile,150) (i,rrpf(i),zzpf(i),drpf(i),dzpf(i),nturns(i), &
         curpf(i)/1.0D6,curdpf(i)/1.0D6, i=1,nrfppf)
150 format(i2,t8,7f8.2)

  end subroutine rfppfc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rfppfp(outfile,iprint)

    !+ad_name  rfppfp
    !+ad_summ  Routine to calculate the MVA, power and energy requirements
    !+ad_summ  for the RFP PF coil systems
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This subroutine calculates the MVA, power and energy requirements
    !+ad_desc  for the RFP PF coil systems.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  01/03/96 PJK Initial version
    !+ad_hist  22/01/97 PJK Subsumed pfelect.h into htpwr.h
    !+ad_hist  08/05/12 PJK Added necessary SAVE to pfbuspwr declaration
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  29/10/12 PJK Added pf_power_variables
    !+ad_hist  30/10/12 PJK Added heat_transport_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)), dimension(nrfppf) :: albusa,cktr,pfbusr, &
         powpfii,psmva,rcktpm,rcktvm,vpfi

    integer :: i,ic,ig,ipf,jjpf,jjpf2,jpf,ngrpt
    real(kind(1.0D0)) :: cptburn,delktim,di,engx,engxpc,ensxpf,pfbusl, &
         powpfi,powpfr,powpfr2,vpfij
    real(kind(1.0D0)), save :: pfbuspwr

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    powpfii(:) = 0.0D0
    cktr(:) = 0.0D0
    vpfi(:) = 0.0D0

    if (iprint == 0) then

       !  Bus length

       pfbusl = 8.0D0 * rmajor + 140.0D0

       !  PF coil resistive power requirements
       !  Bussing losses assume aluminium bussing with 100 A/cm**2

       srcktpm = 0.0D0
       pfbuspwr = 0.0D0

       do ic = 1,nrfppf

          !  Cross-sectional area of bus

          albusa(ic) = abs(cptrfp(ic)) / 100.0D0

          !  Include 50% enhancement for welds,joints etc, (G. Gorker)

          pfbusr(ic) = 1.5D0 * 2.62D-4 * pfbusl / albusa(ic)

          !  Total PF coil circuit resistance

          cktr(ic) = resrfp(ic) + pfbusr(ic)
          rcktvm(ic) = abs(cptrfp(ic))*cktr(ic)
          rcktpm(ic) = 1.0D-6*rcktvm(ic)*abs(cptrfp(ic))

          !  Compute the sum of resistive power in the PF circuits, kW

          pfbuspwr = pfbuspwr + 1.0D-3 * pfbusr(ic) * cptrfp(ic)**2
          srcktpm = srcktpm + 1.0D3*rcktpm(ic)
       end do

       !  Inductive MVA requirements, and stored energy

       delktim = tohs

       !  PF system inductive MVA requirements
       !  OH coils are assumed to swing symmetrically about zero in
       !  a period tohs.
       !  EF coils swing from zero to maximum current in the same time.

       powpfi = 0.0D0
       powpfr = 0.0D0
       powpfr2 = 0.0D0
       ensxpf = 0.0D0

       do jpf = 1,nrfppf
          engx = 0.0D0
          do ipf = 1,nrfppf
             if (ipf < 15) then
                di = 2.0D0 * cptrfp(ipf)
             else
                di = cptrfp(ipf)
             end if
             vpfij = sxlg(jpf,ipf) * abs(di)/delktim
             vpfi(jpf) = vpfi(jpf) + vpfij
             powpfii(jpf) = powpfii(jpf) + 1.0D-6*vpfij*abs(cptrfp(jpf))
             engx = engx + sxlg(jpf,ipf)*abs(cptrfp(ipf))
          end do

          !  Compute inductive energy of each PF coil circuit

          engxpc  = 0.5D0 * engx * abs(cptrfp(jpf))
          ensxpf = ensxpf + engxpc
          powpfr = powpfr + 1.0D-6*nturns(jpf)*abs(cptrfp(jpf))*cktr(jpf)
          powpfr2 = powpfr2 + 1.0D-6*nturns(jpf)*abs(cptrfp(jpf))*cktr(jpf)
          powpfi = powpfi + powpfii(jpf)

       end do

       !  Compute the maximum stored energy and the maximum dissipative
       !  energy in all the PF circuits over the entire cycle time, MJ

       ensxpfm = 1.0D-6 * ensxpf

       !  Check for peak MVA requirements

       peakmva = max( (powpfr + powpfi), powpfr2)

       vpfskv = 20.0D0
       pfckts = dble(nrfppf) + 6.0D0
       spfbusl = pfbusl*pfckts
       acptmax = 0.0D0
       spsmva = 0.0D0

       do jpf = 1,nrfppf

          !  Compute the power supply MVA for each PF circuit

          psmva(jpf) = 1.0D-6 * abs(vpfi(jpf)*cptrfp(jpf))

          !  Compute the sum of the power supply MVA of the PF circuits

          spsmva = spsmva + psmva(jpf)

          !  Compute the average of the maximum currents in the PF circuits, kA

          acptmax = acptmax + 1.0D-3 * abs(cptrfp(jpf))/pfckts

       end do

    end if

    !  Output Section

    if ((iprint == 0).or.(sect13 == 0)) return

    call oheadr(outfile,'PF Coil Power Conversion')
    call ovarre(outfile,'Number of PF coil circuits','(pfckts)',pfckts)
    call ovarre(outfile,'Total power supply MVA for PF circuits', &
         '(spsmva)',spsmva)
    call ovarre(outfile,'Av. max curr/turn of PF coil circuits (kA)', &
         '(acptmax)',acptmax)
    call ovarre(outfile,'Total PF coil circuit bus length (m)','(spfbusl)', &
         spfbusl)
    call ovarre(outfile,'Total PF coil bus resistive power (kW)', &
         '(pfbuspwr)',pfbuspwr)
    call ovarre(outfile,'Total PF coil resistive power (kW)','(srcktpm)', &
         srcktpm)
    call ovarre(outfile,'Maximum PF coil voltage (kV)','(vpfskv)',vpfskv)
    call ovarre(outfile,'Max stored energy in PF coil circuits (MJ)', &
         '(ensxpfm)',ensxpfm)

  end subroutine rfppfp

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rfpphy

    !+ad_name  rfpphy
    !+ad_summ  Routine to calculate the reversed field pinch
    !+ad_summ  plasma physics information
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine calculates the plasma physics of the
    !+ad_desc  reversed field pinch system.
    !+ad_prob  None
    !+ad_call  beamfus
    !+ad_call  betcom
    !+ad_call  cudriv
    !+ad_call  ftheta
    !+ad_call  palph
    !+ad_call  palph2
    !+ad_call  pcond
    !+ad_call  phyaux
    !+ad_call  pohm
    !+ad_call  radpwr
    !+ad_call  rether
    !+ad_call  05/03/96 PJK Initial version
    !+ad_call  01/04/98 PJK Modified calls to BETCOM and PCOND
    !+ad_call  01/04/98 PJK Modified call to BETCOM
    !+ad_call  24/04/98 PJK Modified call to BETCOM
    !+ad_call  30/06/98 PJK Modified call to PCOND
    !+ad_call  19/01/99 PJK Modified call to PCOND
    !+ad_call  16/07/01 PJK Modified call to PCOND
    !+ad_call  22/05/06 PJK Modified call to PALPH2
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added physics_module
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  17/10/12 PJK Added current_drive_module
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  17/12/12 PJK Added zfear to betcom, radpwr argument lists
    !+ad_stat  Okay
    !+ad_docs  UCLA-PPG-1100 TITAN RFP Fusion Reactor Study,
    !+ad_docc                Scoping Phase Report, January 1987
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: alphap,betat,bphi,fusrat,n0e,n0i,pht,pinj,p0, &
         sbar,sigvdt,taup,t0e,t0i,zimp,zion

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate plasma composition

    call betcom(alphan,alphat,cfe0,dene,fdeut,ftrit,fhe3,ftr,ftritbm, &
         idhe3,ignite,impc,impfe,impo,ralpne,rnbeam,te,zeff,abeam, &
         afuel,aion,deni,dlamee,dlamie,dnalp,dnbeam,dnitot,dnla, &
         dnprot,dnz,falpe,falpi,pcoef,rncne,rnone,rnfene,zeffai,zion,zfear)

    ten = te * pcoef
    tin = ti * pcoef

    !  Central values for temperature (keV), density (m**-3) and
    !  pressure (Pa). From ideal gas law : p = nkT

    t0e = te * (1.0D0+alphat)
    t0i = ti * (1.0D0+alphat)
    n0e = dene * (1.0D0+alphan)
    n0i = dnitot * (1.0D0+alphan)
    p0 = (n0e*t0e + n0i*t0i) * 1.6022D-16

    !  Pressure profile index
    !  From ideal gas law : p = nkT

    alphap = alphan + alphat

    !  Calculate reversal parameter F from F-theta curve

    call ftheta(rfpth,rfpf)

    !  Average value of toroidal field
    !  rfpf is constrained to be negative, but bt and TF coil
    !  current are positive in PROCESS

    bphi = abs(bt/rfpf)

    !  Poloidal field at plasma edge

    bp = bphi * rfpth

    !  Plasma current

    plascur = 5.0D6 * rminor * bp

    !  Cylindrical safety factor (for compatibility with tokamaks)

    qstar = rminor * bt / (rmajor * bp)
    q95 = q

    !  Total field (using average toroidal field)

    btot = sqrt(bphi**2 + bp**2)

    !  Poloidal beta

    betap = beta * ( btot/bp )**2

    !  Set OH coil ramp times

    if (lpulse /= 1) then
       if (tohsin == 0.0D0) then
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

    !  Pulse and down times : The reactor is assumed to be 'down'
    !  at all times outside of the plasma current flat-top period.
    !  The pulse length is the duration of non-zero plasma current

    tpulse = tohs + theat + tburn + tqnch
    tdown  = tramp + tohs + tqnch + tdwell

    !  Bootstrap current fraction

    bootipf = 0.0D0

    !  Fraction of plasma current produced by inductive means

    facoh = max( 1.0D-10, (1.0D0-fvsbrnni) )

    !  Fraction of plasma current produced by auxiliary current drive

    faccd = fvsbrnni - bootipf

    !  Do auxiliary current drive power calculations

    if (irfcd /= 0) call cudriv(nout,0)

    !  Calculate fusion power

    call palph(alphan,alphat,deni,ealpha,fdeut,fhe3,ftr,ftrit, &
         idhe3,iiter,pcoef,ti,palp,pcharge,pneut,sigvdt)

    !  Calculate neutral beam slowing down effects
    !  If ignited, then ignore beam fusion effects

    if ((pnbeam /= 0.0D0).and.(ignite == 0)) then
       call beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie, &
            ealpha,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol, &
            zeffai,betanb,dnbeam2,palpnb)
    end if

    call palph2(bt,bp,dene,deni,dnitot,ftr,falpe,falpi,palpnb, &
         ifalphap,pcharge,pcoef,pneut,te,ti,vol,alpmw,betaft, &
         palp,palpi,palpe,pfuscmw,powfmw)

    !  Neutron wall load

    wallmw = ffwal * (pneut*vol) / sarea

    !  Calculate ion/electron equilibration power

    call rether(alphan,alphat,dene,dlamie,te,ti,zeffai,pie)

    !  Calculate radiation power

    call radpwr(alphan,alphat,aspect,bt,dene,deni,fbfe,kappa95,rmajor, &
         rminor,ralpne,rncne,rnone,rnfene,ssync,ten,vol,pbrem,plrad, &
         prad,psync,zfear)

    !  Limit for minimum radiation power

    pht = 1.0D-6*(pinji + pinje) + alpmw + pcharge*vol
    pbrem = max( (fradmin*pht/vol), pbrem)
    prad = pbrem + psync

    !  Calculate ohmic power

    call pohm(facoh,ires,kappa95,plascur,rmajor,rminor,ten,vol,zeff, &
         pohmpv,rpfac,rplas)

    !  Density limit (arbitrary large number for now...)

    pinj = 1.0D-6 * (pinje + pinji)/vol
    pdivt = vol * (palp + pcharge + pinj + pohmpv - prad - plrad)
    pdivt = max(0.001D0, pdivt)

    dnelimt = 1.0D23

    !  Calculate transport losses and energy confinement time using the
    !  chosen scaling law

    call pcond(afuel,alpmw,aspect,bt,dnitot,dene,dnla,eps,hfact, &
         iinvqd,isc,ignite,kappa,kappa95,kappaa,pcharge,pinje,pinji, &
         plascur,pohmpv,prad,rmajor,rminor,te,ten,tin,q95,qstar,vol, &
         xarea,zeff,ptre,ptri,tauee,tauei,taueff,powerht)

    !  Calculate volt-second requirements (not done!)

    !call vscalc(csawth,eps,facoh,gamma,kappa,rmajor,rplas, &
    !      plascur,tburn,phiint,rli,rlp,vsbrn,vsind,vsres,vsstt)

    !  Calculate auxiliary physics related information
    !  for the rest of the code

    sbar = 1.0D0

    call phyaux(aspect,dene,deni,idhe3,plascur,powfmw,sbar,dnalp, &
         dnprot,taueff,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

    !  Poloidal beta limit is set by input parameter betpmx

  end subroutine rfpphy

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ftheta(theta,f)

    !+ad_name  ftheta
    !+ad_summ  Routine to calculate the RFP reversal parameter F,
    !+ad_summ  given the pinch parameter THETA
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  theta : input real : RFP pinch parameter
    !+ad_args  f     : output real : RFP reversal parameter
    !+ad_desc  This subroutine calculates the RFP reversal parameter F,
    !+ad_desc  given the pinch parameter THETA, using a polynomial fit.
    !+ad_prob  None
    !+ad_call  check_range_real
    !+ad_call  06/03/96 PJK Initial version
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  04/10/12 PJK Use new routine check_range_real instead of ranger
    !+ad_stat  Okay
    !+ad_docs  UCLA-PPG-1100 TITAN RFP Fusion Reactor Study,
    !+ad_docc           Scoping Phase Report, January 1987, Fig.4.2-2
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: theta
    real(kind(1.0D0)), intent(out) :: f

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check for sensible theta range

    call check_range_real('RPF theta',theta,0.0D0,1.8D0)

    !  Polynomial fit to experimental data

    f = 1.0D0 &
         + theta*(-2.45461D-02 &
         + theta*(-3.76376D-01 &
         + theta*(-1.89471D-01 &
         + theta*( 7.64083D-02 &
         + theta*(-1.31779D-02)))))

  end subroutine ftheta

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine efcurr(plascur,rmajor,rminor,betap,rli,nrfppf,rrpf, &
       zzpf,nturns,cptrfp)

    !+ad_name  efcurr
    !+ad_summ  Routine to calculate the calculate the current per turn
    !+ad_summ  in the RFP EF coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  plascur : input real : plasma current (A)
    !+ad_args  rmajor  : input real : plasma major radius (m)
    !+ad_args  rminor  : input real : plasma minor radius (m)
    !+ad_args  betap   : input real : plasma poloidal beta
    !+ad_args  rli     : input real : plasma internal inductance
    !+ad_args  nrfppf  : input integer : number of RFP PF coils
    !+ad_args  rrpf(nrfppf) : input real array : radial positions of PF coils (m)
    !+ad_args  zzpf(nrfppf) : input real array : vertical positions of PF coils (m)
    !+ad_args  nturns(nrfppf) : input real array : number of turns in each PF coil
    !+ad_args  cptrfp(nrfppf) : input/output real array : current per turn in
    !+ad_argc                                             each PF coil (A)
    !+ad_desc  This subroutine calculates the current per turn
    !+ad_desc  in the RFP equilibrium field coils required to provide
    !+ad_desc  the correct vertical field at the plasma.
    !+ad_prob  None
    !+ad_call  bfield
    !+ad_call  06/03/96 PJK Initial version
    !+ad_hist  09/05/12 PJK Initial F90 version
    !+ad_hist  18/10/12 PJK Added pfcoil_module
    !+ad_stat  Okay
    !+ad_docs  UCLA-PPG-1100 TITAN RFP Fusion Reactor Study,
    !+ad_docc           Scoping Phase Report, January 1987
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: nrfppf
    real(kind(1.0D0)), intent(in) :: plascur,rmajor,rminor,betap,rli
    real(kind(1.0D0)), dimension(nrfppf) :: rrpf,zzpf,nturns,cptrfp

    !  Local variables

    real(kind(1.0D0)), dimension(2) :: rc,zc,cc,xc
    real(kind(1.0D0)) :: br,bv,bz,psi

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Required vertical field at plasma centre

    bv = -1.0D-7 * plascur / rmajor * &
         (log(8.0D0*rmajor/rminor) + betap + 0.5D0*rli - 1.5D0)

    !  Actual vertical field at plasma centre, due to unscaled
    !  EF coil currents

    rc(1) = rrpf(15)
    rc(2) = rrpf(16)
    zc(1) = zzpf(15)
    zc(2) = zzpf(16)
    cc(1) = cptrfp(15)*nturns(15)
    cc(2) = cptrfp(16)*nturns(16)

    call bfield(2,2,rc,zc,cc,xc,rmajor,0.0D0,br,bz,psi)

    !  Scale EF coil current per turn (proportional to field)

    cptrfp(15) = cptrfp(15) * bv/bz
    cptrfp(16) = cptrfp(16) * bv/bz

  end subroutine efcurr

end module rfp_module
