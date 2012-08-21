!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine inform(progid)

  !+ad_name  inform
  !+ad_summ  Routine to obtain information about the program being executed
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  progid(0:10) : output string array : Strings containing useful info
  !+ad_desc  This subroutine uses system calls to identify the user, date,
  !+ad_desc  machine etc. for the present run, and stores the information
  !+ad_desc  in a character string array.
  !+ad_prob  Non-standard Fortran, because of the unix system calls used.
  !+ad_call  None (except system calls)
  !+ad_hist  03/10/96 PJK Initial version
  !+ad_hist  07/10/96 PJK PROCESS 3001
  !+ad_hist  08/10/96 PJK PROCESS 3002
  !+ad_hist  22/10/96 PJK PROCESS 3003
  !+ad_hist  20/01/97 PJK PROCESS 3004
  !+ad_hist  24/01/97 PJK PROCESS 3005
  !+ad_hist  05/02/97 PJK PROCESS 3006
  !+ad_hist  19/02/97 PJK PROCESS 3007
  !+ad_hist  26/02/97 PJK PROCESS 3008
  !+ad_hist  21/03/97 PJK PROCESS 3009
  !+ad_hist  10/09/97 PJK PROCESS 3010
  !+ad_hist  17/09/97 PJK PROCESS 3011
  !+ad_hist  19/11/97 PJK PROCESS 3012
  !+ad_hist  01/04/98 PJK PROCESS 3013
  !+ad_hist  24/04/98 PJK PROCESS 3014
  !+ad_hist  23/06/98 PJK PROCESS 3015
  !+ad_hist  26/06/98 PJK PROCESS 3016
  !+ad_hist  17/07/98 PJK PROCESS 3017
  !+ad_hist  08/10/98 PJK PROCESS 3018
  !+ad_hist  19/01/99 PJK PROCESS 3019
  !+ad_hist  17/05/99 PJK PROCESS 3020
  !+ad_hist  06/07/99 PJK PROCESS 3021
  !+ad_hist  16/06/00 PJK PROCESS 3022: Modified 'whoami' call in this
  !+ad_hisc               routine, and made a single Makefile suitable
  !+ad_hisc               for both AIX and Linux
  !+ad_hist  04/05/01 PJK PROCESS 3023
  !+ad_hist  03/07/01 PJK PROCESS 3024
  !+ad_hist  16/07/01 PJK PROCESS 3025
  !+ad_hist  25/04/02 PJK PROCESS 3026
  !+ad_hist  16/06/04 PJK PROCESS 3027
  !+ad_hist  22/05/06 PJK PROCESS 3028
  !+ad_hist  22/05/07 PJK PROCESS 3029
  !+ad_hist  21/08/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  character(len=72), dimension(0:10) :: progid

  !  Local variables

  character(len=10) :: progname, progver
  character(len=72), dimension(10) :: id

  !  External routines

!  external system

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Program name and version number

  progname = 'PROCESS'
  progver = '4.000'

  !  Create temporary data file

  call system('/bin/rm -f PROGID.DAT' // char(0))
  call system('/bin/touch PROGID.DAT' // char(0))

  !  Write information to data file

  call system('/bin/date >> PROGID.DAT' // char(0))
  call system('/usr/bin/whoami >> PROGID.DAT' // char(0))
  call system('/bin/hostname >> PROGID.DAT' // char(0))
  call system('/bin/pwd >> PROGID.DAT' // char(0))

  !  Read back information into ID array

  open(unit=1,file='PROGID.DAT',status='old')
  read(1,'(A)') id(1)
  read(1,'(A)') id(2)
  read(1,'(A)') id(3)
  read(1,'(A)') id(4)
  close(unit=1)

  !  Delete temporary data file

  call system('/bin/rm -f PROGID.DAT' // char(0))

  !  Annotate information and store in PROGID character array
  !  for use in other program units via the routine argument

  progid(1) = '  Program : ' // progname
  progid(2) = '  Version : ' // progver
  progid(3) = 'Date/time : ' // id(1)
  progid(4) = '     User : ' // id(2)
  progid(5) = ' Computer : ' // id(3)
  progid(6) = 'Directory : ' // id(4)

  !  Summarise most useful data, and store in progid(0)

  progid(0) = trim(progname) // ' ' // trim(progver) // &
       ' : Run at ' // trim(id(1))

end subroutine inform
