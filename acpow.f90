!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine acpow(nout,iprint)

  !+ad_name  acpow
  !+ad_summ  AC power requirements
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  The routine was drastically shortened on 23/01/90 (ORNL) from the
  !+ad_desc  original TETRA routine to provide only the total power needs for
  !+ad_desc  the plant. Included in STORAC in January 1992 by P.C. Shipe.
  !+ad_prob  None
  !+ad_call  bldgvol.h90
  !+ad_call  estocom.h90
  !+ad_call  htpwr.h90
  !+ad_call  osections.h90
  !+ad_call  pwrcom.h90
  !+ad_call  oblnkl
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  --/--/92 PJK Initial PROCESS version
  !+ad_hist  20/01/97 PJK Fixed error in pheatmw calculation, removed
  !+ad_hisc               assignment of htpmw, and tidied up coding
  !+ad_hist  22/01/97 PJK Subsumed heattr.h, heatrinp.h and pfelect.h into
  !+ad_hisc               htpwr.h
  !+ad_hist  27/07/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'bldgvol.h90'
  include 'pwrcom.h90'
  include 'estocom.h90'
  include 'htpwr.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: iprint,nout

  !  Local variables

  real(kind(1.0D0)) :: basemw,bdvmw,crymw,pheatmw,pkwpm2,ppfmw,ptfmw

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Power to TF coil power supplies, MW

  ptfmw = tfacpd

  ! Power to PF coil power supplies, MW

  ppfmw = 1.0D-3 * srcktpm
  if (iscenr == 2) ppfmw = ppfmw + peakmva

  !  Power to plasma heating supplies, MW

  pheatmw = pinjwp

  !  Power to cryogenic comp. motors, MW

  crymw = crypmw

  !  Facility base load, MW (loads not dependent on floor area)

  basemw = baseel * 1.0D-6

  !  Power needed per unit floor area, kW/m2

  pkwpm2 = pwpm2 * 1.0D-3

  !  Power to divertor coil supplies, MW

  bdvmw = 0.0D0

  !  Total pulsed power system load, MW

  pacpmw = fmgdmw + ppfmw + bdvmw + ptfmw + crymw + vachtmw + &
       htpmw + trithtmw + pheatmw + basemw + efloor*pkwpm2/1000.0D0

  !  Total power to facility loads, MW

  fcsht  = basemw + efloor*pkwpm2/1000.0D0 + 0.05D0*pacpmw

  !  Estimate of the total low voltage power, MW

  tlvpmw = fcsht + trithtmw + htpmw + vachtmw + 0.5D0*(crymw+ppfmw)

  if ((iprint.eq.0).or.(sect17.eq.0)) return

  !  Output section

  call oheadr(nout,'AC Power')

  call ovarre(nout,'Facility base load (MW)','(basemw)',basemw)
  call ovarre(nout,'Divertor coil power supplies (MW)','(bdvmw)',bdvmw)
  call ovarre(nout,'Cryogenic comp motors (MW)','(crymw)',crymw)
  call ovarre(nout,'Total floor space (m2)','(efloor)',efloor)
  call ovarre(nout,'MGF units (MW)','(fmgdmw)',fmgdmw)
  call ovarre(nout,'Heat transport system pump motors (MW)', &
       '(htpmw)',htpmw)
  call ovarre(nout,'PF coil power supplies (MW)','(ppfmw)',ppfmw)
  call ovarre(nout,'Power/floor area (kW/m2)','(pkwpm2)',pkwpm2)
  call ovarre(nout,'TF coil power supplies (MW)','(ptfmw)',ptfmw)
  call ovarre(nout,'Plasma heating supplies (MW)','(pheatmw)', &
       pheatmw)
  call ovarre(nout,'Tritium processing (MW)','(trithtmw)',trithtmw)
  call ovarre(nout,'Vacuum pump motors (MW)','(vachtmw)',vachtmw)

  call oblnkl(nout)

  call ovarre(nout,'Total pulsed power (MW)','(pacpmw)',pacpmw)
  call ovarre(nout,'Total facility power (MW)','(fcsht)',fcsht)
  call ovarre(nout,'Total low voltage power (MW)','(tlvpmw)',tlvpmw)

end subroutine acpow
