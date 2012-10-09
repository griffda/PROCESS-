!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine ech(outfile,iprint)

  !+ad_name  ech
  !+ad_summ  Electron Cyclotron Heating
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  J Galambos, ORNL
  !+ad_auth  P C Shipe, ORNL
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : output file unit
  !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
  !+ad_desc  This routine was added to TETRA on 8/9/88 by J Galambos
  !+ad_desc  to replace Wagner's model and provide a simplified dummy
  !+ad_desc  model of the wall plug power necessary for ech.  It was
  !+ad_desc  included in PROCESS in January 1992 by P. C. Shipe.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  cdriv.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_hist  14/09/01 PJK Initial version
  !+ad_hist  23/03/11 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  include 'cdriv.h90'

  !  Arguments

  integer, intent(in) :: outfile
  integer, intent(in) :: iprint

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  ECH heating power (Watts)

  echwpow = echpwr / etaech

  if ((iprint == 0).or.(sect19 == 0)) return

  call oheadr(outfile,'Electron Cyclotron Heating')
  call ovarre(outfile,'ECH power (W)','(echpwr)',echpwr)
  call ovarre(outfile,'ECH wall plug efficiency','(etaech)',etaech)
  call ovarre(outfile,'ECH wall plug power (W)','(echwpow)',echwpow)

end subroutine ech
