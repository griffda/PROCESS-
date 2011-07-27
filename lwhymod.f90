!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine lwhymod(nout,iprint)

  !+ad_name  lwhymod
  !+ad_summ  Lower Hybrid module
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  nout : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine is a simplified dummy model of the wall
  !+ad_desc  plug power necessary for Lower Hybrid heating. It was included in
  !+ad_desc  PROCESS in January 1992 by P. C . Shipe.
  !+ad_prob  None
  !+ad_call  cdriv.h90
  !+ad_call  osections.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  27/07/11 PJK Initial F90 version
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

  !  etalh,   Lower Hybrid wall plug to plasma efficiency
  !  plhybd,  Lower Hybrid injection power, W 

  !  Lower Hybrid wall plug power, W

  pwplh = plhybd / etalh

  if ((iprint == 0).or.(sect20 == 0)) return

  call oheadr(nout,'Lower Hybrid Heating')
  call ovarre(nout,'Lower hybrid wall plug efficiency','(etalh)',etalh)
  call ovarre(nout,'Lower hybrid injection power (W)','(plhybd)',plhybd)
  call ovarre(nout,'Lower hybrid wall plug power (W)','(pwplh)',pwplh)

end subroutine lwhymod
