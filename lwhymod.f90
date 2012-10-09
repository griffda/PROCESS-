!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine lwhymod(outfile,iprint)

  !+ad_name  lwhymod
  !+ad_summ  Lower Hybrid module
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine is a simplified dummy model of the wall
  !+ad_desc  plug power necessary for Lower Hybrid heating. It was included in
  !+ad_desc  PROCESS in January 1992 by P. C . Shipe.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  cdriv.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  27/07/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  include 'cdriv.h90'

  !  Arguments

  integer, intent(in) :: outfile,iprint

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  etalh,   Lower Hybrid wall plug to plasma efficiency
  !  plhybd,  Lower Hybrid injection power, W 

  !  Lower Hybrid wall plug power, W

  pwplh = plhybd / etalh

  if ((iprint == 0).or.(sect20 == 0)) return

  call oheadr(outfile,'Lower Hybrid Heating')
  call ovarre(outfile,'Lower hybrid wall plug efficiency','(etalh)',etalh)
  call ovarre(outfile,'Lower hybrid injection power (W)','(plhybd)',plhybd)
  call ovarre(outfile,'Lower hybrid wall plug power (W)','(pwplh)',pwplh)

end subroutine lwhymod
