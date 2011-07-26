! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine ech(nout,iprint)

  !+ad_name  ech
  !+ad_summ  Electron Cyclotron Heating
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Galambos, ORNL
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  nout : input integer : 
  !+ad_args  iprint : input integer : 
  !+ad_desc  This routine was added to TETRA on 8/9/88 by J Galambos
  !+ad_desc  to replace Wagner's model and provide a simplified dummy
  !+ad_desc  model of the wall plug power necessary for ech.  It was
  !+ad_desc  included in PROCESS in January 1992 by P. C. Shipe.
  !+ad_prob  None
  !+ad_call  cdriv.h90
  !+ad_call  osections.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  14/09/01 PJK Initial version
  !+ad_hist  23/03/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'cdriv.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout
  integer, intent(in) :: iprint

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  ECH heating power (Watts)

  echwpow = echpwr / etaech

  if ((iprint.eq.0).or.(sect19.eq.0)) return

  call oheadr(nout,'Electron Cyclotron Heating')
  call ovarre(nout,'ECH power (W)','(echpwr)',echpwr)
  call ovarre(nout,'ECH wall plug efficiency','(etaech)',etaech)
  call ovarre(nout,'ECH wall plug power (W)','(echwpow)',echwpow)

end subroutine ech
