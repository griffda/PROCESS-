!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine nbeam(nout,iprint)

  !+ad_name  nbeam
  !+ad_summ  Neutral Beam power requirements
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine was amputated on 28/03/89 to provide only a
  !+ad_desc  simplified dummy model of the wall plug power necessary
  !+ad_desc  for the neutral beams.  It was included in PROCESS in
  !+ad_desc  January 1992 by P. C. Shipe.
  !+ad_desc  <P>The output from the routine is <CODE>pwpnb</CODE>.
  !+ad_prob  None
  !+ad_call  cdriv.h90
  !+ad_call  osections.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'cdriv.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout,iprint

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  cnbeam,  neutral beam current, A
  !  enbeam,  neutral beam energy, keV
  !  etanbi,  neutral beam wall plug to injector efficiency

  !  pwpnb,   neutral beam wall plug power, W

  pwpnb = enbeam * cnbeam * 1.0D3/etanbi

  if ((iprint == 0).or.(sect18 == 0)) return

  call oheadr(nout,'Neutral Beams')

  call ovarre(nout,'Neutral beam current (A)','(cnbeam)',cnbeam)
  call ovarre(nout,'Neutral beam energy (keV)','(enbeam)',enbeam)
  call ovarre(nout,'Neutral beam wall plug efficiency','(etanbi)', &
       etanbi)
  call ovarre(nout,'Neutral beam wall plug power (W)','(pwpnb)', &
       pwpnb)

end subroutine nbeam
