!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfpwr(nout,iprint)

  !+ad_name  tfpwr
  !+ad_summ  TF coil power supply requirements for resistive coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the power conversion requirements for
  !+ad_desc  resistive TF coils, or calls <CODE>tfpwcall</CODE> if the TF
  !+ad_desc  coils are superconducting.
  !+ad_prob  None
  !+ad_call  times.h90
  !+ad_call  cost.h90
  !+ad_call  param.h90
  !+ad_call  phydat.h90
  !+ad_call  tfcoil.h90
  !+ad_call  osections.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_call  tfpwcall
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'tfcoil.h90'
  include 'times.h90'
  include 'cost.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

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

     call tfpwcall(nout,iprint)
     return

  end if

  !  Output section

  if ((iprint == 0).or.(sect13 == 0)) return

  call oheadr(nout,'TF Coil Power Conversion')
  call ovarre(nout,'Bus resistance (ohm)','(rhobus)',rhobus)
  call ovarre(nout,'Bus current density (A/m2)','(jbus)',jbus)
  call ovarre(nout,'Bus length - all coils (m)','(tfbusl)',tfbusl)
  call ovarre(nout,'Bus mass (kg)','(tfbusmas)',tfbusmas)
  call ovarre(nout,'Maximum impedance (ohm)','(ztot)',ztot)
  call ovarre(nout,'Peak voltage per coil (kV)','(vtfkv)',vtfkv)
  call ovarre(nout,'Peak power (MW)','(tfcmw)',tfcmw)
  call ovarre(nout,'TF coil inner leg resistive power (MW)', &
       '(tfcpmw)',tfcpmw)
  call ovarre(nout,'TF coil outer leg resistive power (MW)', &
       '(tflegmw)',tflegmw)
  call ovarre(nout,'TF coil buswork resistive power','(tfbusmw)', &
       tfbusmw)
  call ovarre(nout,'TF coil reactive power (MW)','(tfreacmw)', &
       tfreacmw)

end subroutine tfpwr

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine pfpwr(nout,iprint)

  !+ad_name  pfpwr
  !+ad_summ  PF coil power supply requirements for resistive coils
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine calculates the MVA, power and energy requirements
  !+ad_desc  for the PF coil systems.  Units are MW and MVA for power terms.
  !+ad_desc  The routine checks at the beginning of the flattop for the
  !+ad_desc  peak MVA, and at the end of flattop for the peak stored energy.
  !+ad_desc  The reactive (inductive) components use waves to calculate the
  !+ad_desc  <I>dI/dt</I> at the time periods.
  !+ad_prob  None
  !+ad_call  build.h90
  !+ad_call  htpwr.h90
  !+ad_call  osections.h90
  !+ad_call  param.h90
  !+ad_call  pfcoil.h90
  !+ad_call  phydat.h90
  !+ad_call  pwrcom.h90
  !+ad_call  times.h90
  !+ad_call  vltcom.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  01/08/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'pfcoil.h90'
  include 'vltcom.h90'
  include 'pwrcom.h90'
  include 'build.h90'
  include 'times.h90'
  include 'htpwr.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout, iprint

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
             ( (1.0D0-vf(ic))*1.0D6*ric(ic)) ) * turns(ic)**2 * &
             dble(ncls(ig))

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
     pfckts = dble(ncirt-2) + 6.0D0
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

     call oheadr(nout,'PF Coil Power Conversion')
     call ovarre(nout,'Number of PF coil circuits','(pfckts)',pfckts)
     call ovarre(nout,'Total power supply MVA for PF circuits', &
          '(spsmva)',spsmva)
     call ovarre(nout,'Av. max curr/turn of PF coil circuits (kA)', &
          '(acptmax)',acptmax)
     call ovarre(nout,'Total PF coil circuit bus length (m)', &
          '(spfbusl)',spfbusl)
     call ovarre(nout,'Total PF coil bus resistive power (kW)', &
          '(pfbuspwr)',pfbuspwr)
     call ovarre(nout,'Total PF coil resistive power (kW)', &
          '(srcktpm)',srcktpm)
     call ovarre(nout,'Maximum PF coil voltage (kV)','(vpfskv)',vpfskv)
     call ovarre(nout,'Max stored energy in PF coil circuits (MJ)', &
          '(ensxpfm)',ensxpfm)

  end if

end subroutine pfpwr
