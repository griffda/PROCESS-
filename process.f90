 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program process

  !+ad_name  process
  !+ad_summ  Power Reactor Optimisation Code for Environmental and Safety Studies
  !+ad_type  Main program
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  Power Reactor Optimisation Code for Environmental and Safety Studies
  !+ad_desc  <P>This is a systems code that evaluates various physics and
  !+ad_desc  engineering aspects of a fusion power plant subject to given
  !+ad_desc  constraints, and can optimise these parameters by minimising
  !+ad_desc  or maximising a function of them, such as the fusion power or
  !+ad_desc  cost of electricity.
  !+ad_desc  <P>This program is derived from the TETRA and STORAC codes produced by
  !+ad_desc  Oak Ridge National Laboratory, Tennessee, USA. The main authors in
  !+ad_desc  the USA were J.D.Galambos and P.C.Shipe.
  !+ad_desc  <P>The code was transferred to Culham Laboratory, Oxfordshire, UK, in
  !+ad_desc  April 1992, and the physics models were updated by P.J.Knight to
  !+ad_desc  include the findings of the Culham reactor studies documented in
  !+ad_desc  Culham Report AEA FUS 172 (1992). The standard of the Fortran has
  !+ad_desc  been thoroughly upgraded since that time, and a number of additional
  !+ad_desc  models have been added.
  !+ad_desc  <P>During 2012, PROCESS was upgraded from FORTRAN 77 to Fortran 95,
  !+ad_desc  to facilitate the restructuring of the code into proper modules
  !+ad_desc  (with all the benefits that modern software practices bring), and to
  !+ad_desc  aid the inclusion of more advanced physics and engineering models under
  !+ad_desc  development as part of a number of EFDA-sponsored collaborations.
  !+ad_prob  None
  !+ad_call  numerics
  !+ad_call  error_handling
  !+ad_call  global_variables
  !+ad_call  process_input
  !+ad_call  process_output
  !+ad_call  scan_module
  !+ad_call  eqslv
  !+ad_call  final
  !+ad_call  init
  !+ad_call  oheadr
  !+ad_call  scan
  !+ad_call  show_errors
  !+ad_hist  03/10/96 PJK Upgrade of main program unit
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  09/10/12 PJK Modified to use scan_module
  !+ad_hist  10/10/12 PJK Modified to use numerics module
  !+ad_hist  06/11/12 PJK Renamed this source file from aamain.f90 to process.f90.
  !+ad_hisc               Transferred routine inform from aachange.f90 
  !+ad_hist  13/02/14 PJK Added mfile close statement
  !+ad_hist  10/09/14 PJK Added vfile close statement
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !+ad_docs  Box file F/RS/CIRE5523/PWF (up to 15/01/96)
  !+ad_docs  Box file F/MI/PJK/PROCESS and F/PL/PJK/PROCESS (15/01/96 to 24/01/12)
  !+ad_docs  Box file T&amp;M/PKNIGHT/PROCESS (from 24/01/12)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use error_handling
  use global_variables
  use process_input
  use process_output
  use scan_module
  use numerics

  implicit none

  !  Arguments

  !  Local variables

  integer :: ifail

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise things

  call init

  !  Call equation solver (HYBRD)

  call eqslv(ifail)

  !  Call routine to do optimisation scans

  if (ioptimz >= 0) then
     call scan
  else
     call final(ifail)
  end if

  call show_errors

  call oheadr(nout,'End of PROCESS Output')
  call oheadr(iotty,'End of PROCESS Output')

  close(unit=nin)
  close(unit=nout)
  close(unit=nplot)
  close(unit=mfile)
  if (verbose == 1) close(unit=vfile)

end program process

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine init

  !+ad_name  init
  !+ad_summ  Routine that calls the initialisation routines
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine calls the main initialisation routines that set
  !+ad_desc  the default values for the global variables, reads in data from
  !+ad_desc  the input file, and checks the run parameters for consistency.
  !+ad_prob  None
  !+ad_call  error_handling
  !+ad_call  global_variables
  !+ad_call  impurity_radiation_module
  !+ad_call  numerics
  !+ad_call  process_input
  !+ad_call  process_output
  !+ad_call  check
  !+ad_call  initial
  !+ad_call  initialise_error_list
  !+ad_call  input
  !+ad_call  run_summary
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  17/11/97 PJK Changed file names to *.DAT
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  09/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added global_variables module
  !+ad_hist  13/02/14 PJK Added mfile open statement
  !+ad_hist  13/05/14 PJK Added impurity radiation model initialisation
  !+ad_hist  25/06/14 PJK Introduced call to initialise error handling
  !+ad_hist  22/07/14 PJK Rearranged calls to print output headers
  !+ad_hist  10/09/14 PJK Added vfile open statement
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use error_handling
  use global_variables, only: verbose
  use impurity_radiation_module
  use numerics
  use process_input
  use process_output

  implicit none

  !  Arguments

  !  Local variables

  integer :: i

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise error handling

  call initialise_error_list

  !  Initialise the program variables

  call initial

  !  Open the three input/output external files

  open(unit=nin,file='IN.DAT',status='old')
  open(unit=nout,file='OUT.DAT',status='unknown')
  open(unit=nplot,file='PLOT.DAT',status='unknown')
  open(unit=mfile,file='MFILE.DAT',status='unknown')

  !  Input any desired new initial values

  call input

  !  Initialise impurity radiation data

  if (imprad_model == 1) call initialise_imprad

  !  Check input data for errors/ambiguities

  call check

  !  Write to the output file certain relevant details about this run

  call run_summary

  !  Open verbose diagnostics file

  if (verbose == 1) then
     open(unit=vfile,file='VFILE.DAT',status='unknown')  
     write(vfile,'(a80)') 'nviter = number of VMCON iterations.'
     write(vfile,'(a80)') '(1-mod(ifail,7))=1 indicates that there has '// &
          'been an escape from a failed line search.'
     write(vfile,'(a80)') 'odd/even is a convenient plotting bit.'
     write(vfile,'(100a13)') 'nviter','escape', 'odd/even', 'te','coe','rmajor', &
          'powfmw','bt','tburn','sqsumsq', (lablxc(ixc(i)),i=1,nvar)
  end if

end subroutine init

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
  !+ad_hist  23/01/13 PJK Changed progver to update automatically with SVN
  !+ad_hist  06/05/14 PJK progver must now be changed manually (SVN --> git)
  !+ad_hist  23/07/14 PJK Modified system calls
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  character(len=72), dimension(0:10) :: progid

  !  Local variables

  character(len=*), parameter :: tempfile = 'SCRATCHFILE.DAT'
  character(len=10) :: progname
  character(len=*), parameter :: progver = &  !  Beware: keep exactly same format...
       '345    Release Date :: 2014-10-06'
  character(len=72), dimension(10) :: id
  integer :: unit
  logical :: unit_available

  !  External routines

!  external system

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Program name

  progname = 'PROCESS'

  !  Create temporary data file

  call system('/bin/rm -f '// tempfile // char(0))
  call system('/bin/touch '// tempfile // char(0))

  !  Write information to data file

  call system('/bin/date >> ' // tempfile // char(0))
  call system('/usr/bin/whoami >> ' // tempfile // char(0))
  call system("/usr/bin/finger `/usr/bin/whoami` " // &
       "| /usr/bin/head -1 | /usr/bin/cut -f 4 " // &
       "| /usr/bin/cut -f 2-3 -d ' ' >> " // tempfile // char(0))
  call system('/bin/hostname >> ' // tempfile // char(0))
  call system('/bin/pwd >> ' // tempfile // char(0))

  !  Read back information into ID array

  unit = 20
  unit_available = .false.
  do while (.not.unit_available)
     inquire(unit, exist=unit_available)
     unit = unit+1
  end do

  open(unit,file=tempfile,status='old')
  read(unit,'(A)') id(1)
  read(unit,'(A)') id(2)
  read(unit,'(A)') id(3)
  read(unit,'(A)') id(4)
  read(unit,'(A)') id(5)
  close(unit)

  !  Delete temporary data file

  call system('/bin/rm -f ' // tempfile // char(0))

  !  Annotate information and store in PROGID character array
  !  for use in other program units via the routine argument

  progid(1) = '  Program : ' // progname
  progid(2) = '  Version : ' // progver
  progid(3) = 'Date/time : ' // id(1)
  progid(4) = '     User : ' // trim(id(2)) // ' (' // trim(id(3)) // ')'
  progid(5) = ' Computer : ' // id(4)
  progid(6) = 'Directory : ' // id(5)

  !  Summarise most useful data, and store in progid(0)

  progid(0) = trim(progname) // ' ' // trim(progver(:7)) // &
       ' : Run on ' // trim(id(1)) // ' by ' // trim(id(3))

end subroutine inform

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine run_summary

  !+ad_name  run_summary
  !+ad_summ  Routine to print out a summary header
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine prints out a header summarising the program
  !+ad_desc  execution details, plus a list of the active iteration
  !+ad_desc  variables and constraint equations for the run.
  !+ad_prob  None
  !+ad_call  global_variables
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  inform
  !+ad_call  oblnkl
  !+ad_call  ocentr
  !+ad_call  ocmmnt
  !+ad_call  ostars
  !+ad_call  osubhd
  !+ad_call  ovarin
  !+ad_call  ovarst
  !+ad_hist  28/06/94 PJK Improved layout
  !+ad_hist  03/10/12 PJK Initial F90 version
  !+ad_hist  08/10/12 PJK Changed routine name from edit1 to run_summary
  !+ad_hist  28/11/13 PJK Modified format statement for longer lablxc
  !+ad_hist  27/02/14 PJK Introduced use of nineqns
  !+ad_hist  22/07/14 PJK Moved routine from input.f90, and rearranged layout,
  !+ad_hisc               incorporating old routine codever
  !+ad_stat  Okay
  !+ad_docs  A User's Guide to the PROCESS Systems Code, P. J. Knight,
  !+ad_docc    AEA Fusion Report AEA FUS 251, 1993
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use global_variables
  use numerics
  use process_output

  implicit none

  !  Arguments

  !  Local variables

  integer, parameter :: width = 110
  integer :: lap, ii, outfile
  character(len=72), dimension(0:10) :: progid
  character(len=5) :: vstring
  character(len=8) :: date
  character(len=10) :: time
  character(len=12) :: dstring
  character(len=7) :: tstring
  character(len=10) :: ustring
  integer :: version

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Obtain execution details for this run

  call inform(progid)

  !  Print code banner + run details to screen and output file
  
  do lap = 1,2
     if (lap == 1) then
        outfile = iotty
     else
        outfile = nout
     end if

     call oblnkl(outfile)

     call ostars(outfile, width)
     call ocentr(outfile,'PROCESS', width)
     call ocentr(outfile,'Power Reactor Optimisation Code', width)
     call ocentr(outfile,'for Environmental and Safety Studies', width)
     call ostars(outfile, width)

     call oblnkl(outfile)

     !  Run execution details

     call ocmmnt(outfile, progid(1))  !  program name
     call ocmmnt(outfile, progid(2))  !  version
     call ocmmnt(outfile, progid(3))  !  date/time
     call ocmmnt(outfile, progid(4))  !  user
     call ocmmnt(outfile, progid(5))  !  computer
     call ocmmnt(outfile, progid(6))  !  directory

     !  Print code version and run description

     call oblnkl(outfile)
     call ostars(outfile, width)
     call oblnkl(outfile)
     call ocmmnt(outfile, progid(0))
     call ocmmnt(outfile, 'Reactor concept design: '// trim(icase) // ', (c) CCFE')
     call osubhd(outfile, runtitle)
  end do

  call ocmmnt(nout,'(Please include this header in any models, ' // &
       'presentations and papers based on these results)')
  call oblnkl(nout)
  call ostars(nout, width)

  !  Beware of possible future changes to the progid(...) layouts

  !  The following should work up to version 99999
  !  Relies on an internal read statement

  vstring = progid(2)(13:17)
  read(vstring,'(i5)') version
  call ovarin(mfile,'PROCESS version number','(procver)',version)

  call date_and_time(date=date, time=time)

  !  Date output in the form "DD/MM/YYYY" (including quotes)

  dstring = '"'//date(7:8)//'/'//date(5:6)//'/'//date(1:4)//'"'
  call ovarst(mfile,'Date of run','(date)',dstring)

  !  Time output in the form "hh:mm" (including quotes)

  tstring = '"'//time(1:2)//':'//time(3:4)//'"'
  call ovarst(mfile,'Time of run','(time)',tstring)

  ustring = '"'//trim(progid(4)(13:20))//'"'
  call ovarst(mfile,'User','(username)',ustring)

#ifndef unit_test
  call oblnkl(nout)
  call ocmmnt(nout,'The following variables will be adjusted by')
  call ocmmnt(nout,'the code during the iteration process :')
  call oblnkl(nout)

  write(nout,10)
10 format(t10,'ixc',t18,'label')

  call oblnkl(nout)

  write(nout,20) (ii,ixc(ii),lablxc(ixc(ii)),ii=1,nvar)
20 format(t1,i3,t10,i3,t18,a9)

  call oblnkl(nout)
  call ocmmnt(nout, & 
       'The following constraint equations have been imposed,')
  if (ioptimz == -1) then
     call ocmmnt(nout, & 
          'but limits will not be enforced by the code :')
  else
     call ocmmnt(nout,'and will be enforced by the code :')
  end if
  call oblnkl(nout)

  write(nout,30)
30 format(t10,'icc',t25,'label')

  call oblnkl(nout)

  write(nout,40) (ii,icc(ii),lablcc(icc(ii)), ii=1,neqns+nineqns)
40 format(t1,i3,t10,i3,t18,a33)
#endif

end subroutine run_summary

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine eqslv(ifail)

  !+ad_name  eqslv
  !+ad_summ  Routine to call the non-optimising equation solver
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : output integer : error flag
  !+ad_desc  This routine calls the non-optimising equation solver.
  !+ad_prob  None
  !+ad_call  constraints
  !+ad_call  function_evaluator
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  constraint_eqns
  !+ad_call  eqsolv
  !+ad_call  fcnhyb
  !+ad_call  herror
  !+ad_call  int_to_string3
  !+ad_call  loadxc
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  osubhd
  !+ad_call  ovarin
  !+ad_call  ovarre
  !+ad_call  report_error
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics, function_evaluator
  !+ad_hisc               modules
  !+ad_hist  31/01/13 PJK Added warning about high residuals if the convergence
  !+ad_hisc               is suspicious
  !+ad_hist  28/11/13 PJK Modified format line 40 for longer lablxc length
  !+ad_hist  13/02/14 PJK Output ifail even if a feasible solution found
  !+ad_hist  13/03/14 PJK Added numerical state information to mfile
  !+ad_hist  09/07/14 PJK Turned on error reporting
  !+ad_hist  28/07/14 PJK Added constraint_eqns call to evaluate residues
  !+ad_hisc               in physical units
  !+ad_hist  19/08/14 PJK Added neqns, normalised residues to output
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraints
  use process_output
  use numerics
  use function_evaluator

  implicit none

  !  Arguments

  integer, intent(out) :: ifail

  !  Local variables

  integer :: inn,nprint,nx
  real(kind(1.0D0)) :: sumsq
  real(kind(1.0D0)), dimension(iptnt) :: wa
  real(kind(1.0D0)), dimension(ipeqns) :: con1, con2, err
  character(len=1), dimension(ipeqns) :: sym
  character(len=10), dimension(ipeqns) :: lab

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  If no HYBRD (non-optimisation) runs are required, exit routine

  if (ioptimz > 0) return

  ncalls = 0
  nfev1 = 0
  nfev2 = 0
  nprint = 0

  !  Use HYBRD to find a starting point

  call loadxc
  call eqsolv(fcnhyb,neqns,xcm,rcm,ftol,epsfcn,factor,nprint,ifail, &
       wa,iptnt,resdl,nfev1)

  !  Turn on error reporting

  errors_on = .true.

  !  Print out information on solution

  call oheadr(nout,'Numerics')
  call ocmmnt(nout, &
       'PROCESS has performed a HYBRD (non-optimisation) run,')

  if (ifail /= 1) then
     call ocmmnt(nout,'but could not find a feasible set of parameters.')
     call oblnkl(nout)
     call ovarin(nout,'Number of iteration variables and constraints','(neqns)',neqns)
     call ovarin(nout,'HYBRD error flag','(ifail)',ifail)

     call oheadr(iotty,'PROCESS COULD NOT FIND A FEASIBLE SOLUTION')
     call ovarin(iotty,'HYBRD error flag (ifail)','',ifail)
     call oblnkl(iotty)

     idiags(1) = ifail ; call report_error(131)

  else
     call ocmmnt(nout,'and found a feasible set of parameters.')
     call oblnkl(nout)
     call ovarin(nout,'HYBRD error flag','(ifail)',ifail)
     call oblnkl(nout)
     call oheadr(iotty,'PROCESS found a feasible solution')
  end if

  !  Sum the square of the residuals

  sumsq = 0.0D0
  do nx = 1,neqns
     sumsq = sumsq + rcm(nx)**2
  end do
  sqsumsq = sqrt(sumsq)

  call ovarre(nout,'Estimate of the constraints','(sqsumsq)',sqsumsq)

  !  If necessary, write out a relevant error message

  if (ifail /= 1) then
     call oblnkl(nout)
     call herror(ifail)
     call oblnkl(iotty)
  else
     !  Show a warning if the constraints appear high even if allegedly converged
     if (sqsumsq >= 1.0D-2) then
        call oblnkl(nout)
        call ocmmnt(nout,'WARNING: Constraint residues are HIGH; consider re-running')
        call ocmmnt(nout,'   with lower values of FTOL to confirm convergence...')
        call ocmmnt(nout,'   (should be able to get down to about 1.0E-8 okay)')

        call ocmmnt(iotty,'WARNING: Constraint residues are HIGH; consider re-running')
        call ocmmnt(iotty,'   with lower values of FTOL to confirm convergence...')
        call ocmmnt(iotty,'   (should be able to get down to about 1.0E-8 okay)')
        call oblnkl(iotty)

        fdiags(1) = sqsumsq ; call report_error(133)

     end if
  end if

  call osubhd(nout,'The solution vector is comprised as follows :')

  write(nout,10)
10 format(t5,'i',t23,'final',t33,'fractional',t46,'residue')

  write(nout,20)
20 format(t23,'value',t35,'change')

  call oblnkl(nout)

  do inn = 1,neqns
     xcs(inn) = xcm(inn)*scafc(inn)
     write(nout,30) inn,lablxc(ixc(inn)),xcs(inn),xcm(inn),resdl(inn)
     call ovarre(mfile,lablxc(ixc(inn)),'(itvar'//int_to_string3(inn)//')',xcs(inn))
  end do
30 format(t2,i4,t8,a9,t19,1pe12.4,1pe12.4,1pe12.4)

  call osubhd(nout, &
       'The following constraint residues should be close to zero :')

  call constraint_eqns(neqns,con1,-1,con2,err,sym,lab)
  write(nout,40)
40 format(t48,'physical',t73,'constraint',t100,'normalised')
  write(nout,50)
50 format(t47,'constraint',t74,'residue',t101,'residue')
  call oblnkl(nout)
  do inn = 1,neqns
     write(nout,60) inn,lablcc(icc(inn)),sym(inn),con2(inn), &
          lab(inn),err(inn),lab(inn),con1(inn)
     call ovarre(mfile,lablcc(icc(inn))//' normalised residue', &
          '(normres'//int_to_string3(inn)//')',con1(inn))
  end do
60 format(t2,i4,t8,a33,t46,a1,t47,1pe12.4,t60,a10,t71,1pe12.4,t84,a10,t98,1pe12.4)

end subroutine eqslv

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine herror(ifail)

  !+ad_name  herror
  !+ad_summ  Routine to print out relevant messages in the case of an
  !+ad_summ  unfeasible result from a HYBRD (non-optimisation) run
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail  : input integer : error flag
  !+ad_desc  This routine prints out relevant messages in the case of
  !+ad_desc  an unfeasible result from a HYBRD (non-optimisation) run.
  !+ad_desc  <P>The messages are written to units NOUT and IOTTY, which are
  !+ad_desc  by default the output file and screen, respectively.
  !+ad_desc  <P>If <CODE>IFAIL=1</CODE> then a feasible solution has been
  !+ad_desc  found and therefore no error message is required.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  31/01/13 PJK Reworded the ifail=4 error message
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  !  Arguments

  integer, intent(in) :: ifail

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  select case (ifail)

  case (:-1)
     call ocmmnt(nout, 'User-terminated execution of HYBRD.')
     call ocmmnt(iotty,'User-terminated execution of HYBRD.')

  case (0)
     call ocmmnt(nout, 'Improper input parameters to the HYBRD routine.')
     call ocmmnt(nout, 'PROCESS coding must be checked.')

     call ocmmnt(iotty,'Improper input parameters to the HYBRD routine.')
     call ocmmnt(iotty,'PROCESS coding must be checked.')

  case (1)
     continue

  case (2)
     call ocmmnt(nout, &
          'The maximum number of calls has been reached without')
     call ocmmnt(nout,'solution, suggesting that the iteration is not')
     call ocmmnt(nout,'making good progress.')
     call ocmmnt(nout,'Try changing the variables in IXC.')

     call ocmmnt(iotty, &
          'The maximum number of calls has been reached without')
     call ocmmnt(iotty,'solution, suggesting that the iteration is not')
     call ocmmnt(iotty,'making good progress.')
     call ocmmnt(iotty,'Try changing the variables in IXC.')

  case (3)
     call ocmmnt(nout, &
          'The tolerance is too small: No further improvement')
     call ocmmnt(nout,'in the approximate solution is possible.')
     call ocmmnt(nout,'Try raising the value of FTOL.')

     call ocmmnt(iotty, &
          'The tolerance is too small: No further improvement')
     call ocmmnt(iotty,'in the approximate solution is possible.')
     call ocmmnt(iotty,'Try raising the value of FTOL.')

  case (4)
     call ocmmnt(nout,'The iteration is not making good progress.')
     call ocmmnt(nout,'The code may be stuck in a minimum in the residual')
     call ocmmnt(nout,'space that is significantly above zero.')
     call oblnkl(nout)
     call ocmmnt(nout,'There is either no solution possible, or the code')
     call ocmmnt(nout,'is failing to escape from a deep local minimum.')
     call ocmmnt(nout,'Try changing the variables in IXC, or')
     call ocmmnt(nout,'modify their initial values.')

     call ocmmnt(iotty,'The iteration is not making good progress.')
     call ocmmnt(iotty,'The code may be stuck in a minimum in the residual')
     call ocmmnt(iotty,'space that is significantly above zero.')
     call oblnkl(iotty)
     call ocmmnt(iotty,'There is either no solution possible, or the code')
     call ocmmnt(iotty,'is failing to escape from a deep local minimum.')
     call ocmmnt(iotty,'Try changing the variables in IXC, or')
     call ocmmnt(iotty,'modify their initial values.')

  case default
     call ocmmnt(nout, 'This value of IFAIL should not be possible...')
     call ocmmnt(nout,'See source code for details.')

     call ocmmnt(iotty,'This value of IFAIL should not be possible...')
     call ocmmnt(iotty,'See source code for details.')

  end select

end subroutine herror

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine verror(ifail)

  !+ad_name  verror
  !+ad_summ  Routine to print out relevant messages in the case of an
  !+ad_summ  unfeasible result from a VMCON (optimisation) run
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail  : input integer : error flag
  !+ad_desc  This routine prints out relevant messages in the case of
  !+ad_desc  an unfeasible result from a VMCON (optimisation) run.
  !+ad_desc  <P>The messages are written to units NOUT and IOTTY, which are
  !+ad_desc  by default the output file and screen, respectively.
  !+ad_desc  <P>If <CODE>IFAIL=1</CODE> then a feasible solution has been
  !+ad_desc  found and therefore no error message is required.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  ocmmnt
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  31/01/13 PJK Reworded the ifail=2 error message
  !+ad_hist  04/07/13 PJK Reworded the ifail=5 error message
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  !  Arguments

  integer, intent(in) :: ifail

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  select case (ifail)

  case (:-1)
     call ocmmnt(nout, 'User-terminated execution of VMCON.')
     call ocmmnt(iotty,'User-terminated execution of VMCON.')

  case (0)
     call ocmmnt(nout, 'Improper input parameters to the VMCON routine.')
     call ocmmnt(nout, 'PROCESS coding must be checked.')

     call ocmmnt(iotty,'Improper input parameters to the VMCON routine.')
     call ocmmnt(iotty,'PROCESS coding must be checked.')

  case (1)
     continue

  case (2)
     call ocmmnt(nout, &
          'The maximum number of calls has been reached without')
     call ocmmnt(nout,'solution, suggesting that the iteration is not')
     call ocmmnt(nout,'making good progress.')
     call ocmmnt(nout,'The code may be stuck in a minimum in the residual')
     call ocmmnt(nout,'space that is significantly above zero.')
     call oblnkl(nout)
     call ocmmnt(nout,'There is either no solution possible, or the code')
     call ocmmnt(nout,'is failing to escape from a deep local minimum.')
     call ocmmnt(nout,'Try changing the variables in IXC, or')
     call ocmmnt(nout,'modify their initial values.')

     call ocmmnt(iotty, &
          'The maximum number of calls has been reached without')
     call ocmmnt(iotty,'solution, suggesting that the iteration is not')
     call ocmmnt(iotty,'making good progress.')
     call ocmmnt(iotty,'The code may be stuck in a minimum in the residual')
     call ocmmnt(iotty,'space that is significantly above zero.')
     call oblnkl(iotty)
     call ocmmnt(iotty,'There is either no solution possible, or the code')
     call ocmmnt(iotty,'is failing to escape from a deep local minimum.')
     call ocmmnt(iotty,'Try changing the variables in IXC, or')
     call ocmmnt(iotty,'modify their initial values.')

  case (3)
     call ocmmnt(nout, &
          'The line search required the maximum of 10 calls.')
     call ocmmnt(nout, &
          'A feasible solution may be difficult to achieve.')
     call ocmmnt(nout,'Try changing or adding variables to IXC.')

     call ocmmnt(iotty, &
          'The line search required the maximum of 10 calls.')
     call ocmmnt(iotty, &
          'A feasible solution may be difficult to achieve.')
     call ocmmnt(iotty,'Try changing or adding variables to IXC.')

  case (4)
     call ocmmnt(nout,'An uphill search direction was found.')
     call ocmmnt(nout,'Try changing the equations in ICC, or')
     call ocmmnt(nout,'adding new variables to IXC.')

     call ocmmnt(iotty,'An uphill search direction was found.')
     call ocmmnt(iotty,'Try changing the equations in ICC, or')
     call ocmmnt(iotty,'adding new variables to IXC.')

  case (5)
     call ocmmnt(nout, &
          'The quadratic programming technique was unable to')
     call ocmmnt(nout,'find a feasible point.')
     call oblnkl(nout)
     call ocmmnt(nout,'Try changing or adding variables to IXC, or modify')
     call ocmmnt(nout,'their initial values (especially if only 1 optimisation')
     call ocmmnt(nout,'iteration was performed).')

     call ocmmnt(iotty, &
          'The quadratic programming technique was unable to')
     call ocmmnt(iotty,'find a feasible point.')
     call oblnkl(iotty)
     call ocmmnt(iotty,'Try changing or adding variables to IXC, or modify')
     call ocmmnt(iotty,'their initial values (especially if only 1 optimisation')
     call ocmmnt(iotty,'iteration was performed).')

  case (6)
     call ocmmnt(nout, &
          'The quadratic programming technique was restricted')
     call ocmmnt(nout, &
          'by an artificial bound, or failed due to a singular')
     call ocmmnt(nout,'matrix.')
     call ocmmnt(nout,'Try changing the equations in ICC, or')
     call ocmmnt(nout,'adding new variables to IXC.')

     call ocmmnt(iotty, &
          'The quadratic programming technique was restricted')
     call ocmmnt(iotty, &
          'by an artificial bound, or failed due to a singular')
     call ocmmnt(iotty,'matrix.')
     call ocmmnt(iotty,'Try changing the equations in ICC, or')
     call ocmmnt(iotty,'adding new variables to IXC.')

  case default
     call ocmmnt(nout,'This value of IFAIL should not be possible...')
     call ocmmnt(nout,'See source code for details.')

     call ocmmnt(iotty,'This value of IFAIL should not be possible...')
     call ocmmnt(iotty,'See source code for details.')

  end select

end subroutine verror

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine doopt(ifail)

  !+ad_name  doopt
  !+ad_summ  Routine to call the optimising equation solver
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : output integer : error flag
  !+ad_desc  This routine calls the optimising equation solver.
  !+ad_prob  None
  !+ad_call  constraints
  !+ad_call  error_handling
  !+ad_call  function_evaluator
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  boundxc
  !+ad_call  constraint_eqns
  !+ad_call  int_to_string3
  !+ad_call  loadxc
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  optimiz
  !+ad_call  osubhd
  !+ad_call  ovarin
  !+ad_call  ovarre
  !+ad_call  report_error
  !+ad_call  verror
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics and function_evaluator
  !+ad_hisc               modules
  !+ad_hist  31/01/13 PJK Added warning about high residuals if the convergence
  !+ad_hisc               is suspicious
  !+ad_hist  04/07/13 PJK Modified wording for variables at/beyond their bounds
  !+ad_hist  28/11/13 PJK Modified format lines for longer lablxc length
  !+ad_hist  13/02/14 PJK Output ifail even if a feasible solution found
  !+ad_hist  27/02/14 PJK Added nineqns usage; minor output modifications
  !+ad_hist  13/03/14 PJK Added numerical state information to mfile
  !+ad_hist  09/07/14 PJK Added error reporting
  !+ad_hist  09/07/14 PJK Added range-normalised iteration variable values to mfile
  !+ad_hist  28/07/14 PJK Added constraint_eqns call to evaluate residues
  !+ad_hisc               in physical units
  !+ad_hist  19/08/14 PJK Added nvar, neqns to output, constraint residues to mfile
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraints
  use error_handling
  use function_evaluator
  use numerics
  use process_output

  implicit none

  !  Arguments

  integer, intent(out) :: ifail

  !  Local variables

  integer :: ii,inn,iflag
  real(kind(1.0D0)) :: summ,xcval,xmaxx,xminn,f,xnorm
  real(kind(1.0D0)), dimension(ipeqns) :: con1, con2, err
  character(len=1), dimension(ipeqns) :: sym
  character(len=10), dimension(ipeqns) :: lab

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  If no optimisation is required, leave the routine

  if (ioptimz < 0) return

  !  Set up variables to be iterated

  call loadxc
  call boundxc
  call optimiz(fcnvmc1,fcnvmc2,ifail,f)

  !  Check on accuracy of solution by summing the
  !  squares of the residuals of the equality constraints

  summ = 0.0D0
  do ii = 1,neqns
     summ = summ + rcm(ii)*rcm(ii)
  end do
  sqsumsq = sqrt(summ)

  !  Turn on error reporting

  errors_on = .true.

  !  Print out information on solution

  call oheadr(nout,'Numerics')
  call ocmmnt(nout,'PROCESS has performed a VMCON (optimisation) run,')
  if (ifail /= 1) then
     call ocmmnt(nout,'but could not find a feasible set of parameters.')

     call oheadr(iotty,'PROCESS COULD NOT FIND A FEASIBLE SOLUTION')
     call ovarin(iotty,'VMCON error flag (ifail)','',ifail)
     call oblnkl(iotty)

     idiags(1) = ifail ; call report_error(132)

  else
     call ocmmnt(nout,'and found a feasible set of parameters.')
     call oblnkl(nout)
     call ovarin(nout,'VMCON error flag','(ifail)',ifail)
     call oheadr(iotty,'PROCESS found a feasible solution')
  end if

  call oblnkl(nout)

  !  If necessary, write out a relevant error message

  if (ifail /= 1) then
     call verror(ifail)
     call oblnkl(nout)
     call oblnkl(iotty)
  else
     !  Show a warning if the constraints appear high even if allegedly converged
     if (sqsumsq >= 1.0D-2) then
        call oblnkl(nout)
        call ocmmnt(nout,'WARNING: Constraint residues are HIGH; consider re-running')
        call ocmmnt(nout,'   with lower values of EPSVMC to confirm convergence...')
        call ocmmnt(nout,'   (should be able to get down to about 1.0E-8 okay)')

        call ocmmnt(iotty,'WARNING: Constraint residues are HIGH; consider re-running')
        call ocmmnt(iotty,'   with lower values of EPSVMC to confirm convergence...')
        call ocmmnt(iotty,'   (should be able to get down to about 1.0E-8 okay)')
        call oblnkl(iotty)

        fdiags(1) = sqsumsq ; call report_error(134)

     end if
  end if

  call ovarin(nout,'Number of iteration variables','(nvar)',nvar)
  call ovarin(nout,'Number of constraints','(neqns)',neqns)
  call ovarin(nout,'Optimisation switch','(ioptimz)',ioptimz)
  call ovarin(nout,'Figure of merit switch','(minmax)',minmax)
  if (ifail /= 1) then
     call ovarin(nout,'VMCON error flag','(ifail)',ifail)
  end if
  call ovarre(nout,'Figure of merit objective function','(f)',f)
  call ovarre(nout,'Estimate of the constraints','(sqsumsq)',sqsumsq)
  call oblnkl(nout)

  if (ifail == 1) then
     call ocmmnt(nout, &
          'PROCESS has successfully optimised the program variables')
  else
     call ocmmnt(nout, &
          'PROCESS has tried to optimise the program variables')
  end if

  if (minmax > 0) then
     write(nout,10) lablmm(abs(minmax))
  else
     write(nout,20) lablmm(abs(minmax))
  end if
10 format(' to minimise the ',a22)
20 format(' to maximise the ',a22)

  call oblnkl(nout)

  !  Check which variables are at bounds

  iflag = 0
  do ii = 1,nvar
     xminn = 1.01D0*bondl(ii)
     xmaxx = 0.99D0*bondu(ii)

     if (xcm(ii) < xminn) then
        if (iflag == 0) then
           call ocmmnt(nout, &
                'Certain operating limits have been reached,')
           call ocmmnt(nout, &
                'as shown by the following iteration variables that are')
           call ocmmnt(nout, &
                'at the edge of their prescribed range :')
           call oblnkl(nout)
           iflag = 1
        end if
        xcval = xcm(ii)*scafc(ii)
        write(nout,30) ii,lablxc(ixc(ii)),xcval,bondl(ii)*scafc(ii)
     end if

     if (xcm(ii) > xmaxx) then
        if (iflag == 0) then
           call ocmmnt(nout, &
                'Certain operating limits have been reached,')
           call ocmmnt(nout, &
                'as shown by the following iteration variables that are')
           call ocmmnt(nout, &
                'at the edge of their prescribed range :')
           call oblnkl(nout)
           iflag = 1
        end if
        xcval = xcm(ii)*scafc(ii)
        write(nout,40) ii,lablxc(ixc(ii)),xcval,bondu(ii)*scafc(ii)
     end if
  end do

30 format(t4,'Variable ',i3,' (',a9, &
        ',',1pe12.4,') is at or below its lower bound:',1pe12.4)
40 format(t4,'Variable ',i3,' (',a9, &
        ',',1pe12.4,') is at or above its upper bound:',1pe12.4)

  !  Print out information on numerics

  call osubhd(nout,'The solution vector is comprised as follows :')
  write(nout,50)
50 format(t47,'lower',t59,'upper')

  write(nout,60)
60 format(t23,'final',t33,'fractional',t46,'Lagrange',t58,'Lagrange')

  write(nout,70)
70 format(t5,'i',t23,'value',t35,'change',t45,'multiplier', &
        t57,'multiplier')

  call oblnkl(nout)

  do inn = 1,nvar
     xcs(inn) = xcm(inn)*scafc(inn)
     write(nout,80) inn,lablxc(ixc(inn)),xcs(inn),xcm(inn), &
          vlam(neqns+nineqns+inn), vlam(neqns+nineqns+1+inn+nvar)
     call ovarre(mfile,lablxc(ixc(inn)),'(itvar'//int_to_string3(inn)//')',xcs(inn))

     !  'Range-normalised' iteration variable values:
     !  0.0 (at lower bound) to 1.0 (at upper bound)

     if (bondl(inn) == bondu(inn)) then
        xnorm = 1.0D0
     else
        xnorm = (xcm(inn) - bondl(inn)) / (bondu(inn) - bondl(inn))
        xnorm = max(xnorm, 0.0D0)
        xnorm = min(xnorm, 1.0D0)
     end if
     call ovarre(mfile,trim(lablxc(ixc(inn)))//' (range normalised)', &
          '(nitvar'//int_to_string3(inn)//')',xnorm)
  end do
80 format(t2,i4,t8,a9,t19,4(1pe12.4))

  call osubhd(nout, &
       'The following equality constraint residues should be close to zero :')

  call constraint_eqns(neqns,con1,-1,con2,err,sym,lab)
  write(nout,90)
90 format(t48,'physical',t73,'constraint',t100,'normalised')
  write(nout,100)
100 format(t47,'constraint',t74,'residue',t101,'residue')
  call oblnkl(nout)
  do inn = 1,neqns
     write(nout,110) inn,lablcc(icc(inn)),sym(inn),con2(inn), &
          lab(inn),err(inn),lab(inn),con1(inn)
     call ovarre(mfile,lablcc(icc(inn))//' normalised residue', &
          '(normres'//int_to_string3(inn)//')',con1(inn))
  end do
110 format(t2,i4,t8,a33,t46,a1,t47,1pe12.4,t60,a10,t71,1pe12.4,t84,a10,t98,1pe12.4)

  if (nineqns > 0) then
     call osubhd(nout, &
          'The following inequality constraint residues should be positive :')

     do inn = neqns+1,neqns+nineqns
        write(nout,120) inn,lablcc(icc(inn)),rcm(inn),vlam(inn)
        call ovarre(mfile,lablcc(icc(inn)),'(constr'//int_to_string3(inn)//')',rcm(inn))
     end do
  end if

120 format(t2,i4,t8,a33,t45,1pe12.4,1pe12.4)

end subroutine doopt

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine final(ifail)

  !+ad_name  final
  !+ad_summ  Routine to print out the final point in the scan
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : input integer : error flag
  !+ad_desc  This routine prints out the final point in the scan.
  !+ad_prob  None
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  oheadr
  !+ad_call  output
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  23/01/13 PJK Changed format for single iteration outputs
  !+ad_hist  10/09/14 PJK Removed output lines if a given solver is unused
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output
  use numerics

  implicit none

  !  Arguments

  integer, intent(in) :: ifail

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (ifail == 1) then
     call oheadr(nout,'Final Feasible Point')
  else
     call oheadr(nout,'Final UNFEASIBLE Point')
  end if

  call output(nout)

  if (nfev1 == 0) then  !  no HYBRD call
     if (nviter == 1) then
        write(iotty,10) nviter,ncalls
     else
        write(iotty,20) nviter,ncalls
     end if
  else if (nviter == 0) then  !  no VMCON call
     if (nfev1 == 1) then
        write(iotty,30) nfev1,ncalls
     else
        write(iotty,40) nfev1,ncalls
     end if
  else if (nfev1 == 1) then ! (unlikely that nviter is also 1...)
     write(iotty,50) nfev1,nviter,ncalls
  else if (nviter == 1) then ! (unlikely that nfev1 is also 1...)
     write(iotty,60) nfev1,nviter,ncalls
  else
     write(iotty,70) nfev1,nviter,ncalls
  end if

10 format( &
       t2,'The optimisation required ',i5,' iteration',/, &
       t2,'There were ',i6,' function calls')
20 format( &
       t2,'The optimisation required ',i5,' iterations',/, &
       t2,'There were ',i6,' function calls')
30 format( &
       t2,'The HYBRD point required ',i5,' iteration',/, &
       t2,'There were ',i6,' function calls')
40 format( &
       t2,'The HYBRD point required ',i5,' iterations',/, &
       t2,'There were ',i6,' function calls')
50 format( &
       t2,'The HYBRD point required ',i5,' iteration',/, &
       t2,'The optimisation required ',i5,' iterations',/, &
       t2,'There were ',i6,' function calls')
60 format( &
       t2,'The HYBRD point required ',i5,' iterations',/, &
       t2,'The optimisation required ',i5,' iteration',/, &
       t2,'There were ',i6,' function calls')
70 format( &
       t2,'The HYBRD point required ',i5,' iterations',/, &
       t2,'The optimisation required ',i5,' iterations',/, &
       t2,'There were ',i6,' function calls')

end subroutine final

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine output(outfile)

  !+ad_name  output
  !+ad_summ  Subroutine to write the results to the main output file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile : input integer : Fortran output unit identifier
  !+ad_desc  This routine writes the program results to a file,
  !+ad_desc  in a tidy format.
  !+ad_prob  None
  !+ad_call  availability_module
  !+ad_call  build_module
  !+ad_call  buildings_module
  !+ad_call  costs_module
  !+ad_call  current_drive_module
  !+ad_call  divertor_module
  !+ad_call  fwbs_module
  !+ad_call  ife_module
  !+ad_call  ife_variables
  !+ad_call  pfcoil_module
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  power_module
  !+ad_call  pulse_module
  !+ad_call  rfp_module
  !+ad_call  rfp_variables
  !+ad_call  sctfcoil_module
  !+ad_call  startup_module
  !+ad_call  stellarator_module
  !+ad_call  stellarator_variables
  !+ad_call  structure_module
  !+ad_call  tfcoil_module
  !+ad_call  vaccum_module
  !+ad_call  acpow
  !+ad_call  avail
  !+ad_call  bldgcall
  !+ad_call  cntrpst
  !+ad_call  costs
  !+ad_call  cudriv
  !+ad_call  divcall
  !+ad_call  fispac
  !+ad_call  fwbs
  !+ad_call  ifeout
  !+ad_call  igmarcal
  !+ad_call  induct
  !+ad_call  loca
  !+ad_call  outpf
  !+ad_call  outplas
  !+ad_call  outtim
  !+ad_call  outvolt
  !+ad_call  pfpwr
  !+ad_call  power2
  !+ad_call  pulse
  !+ad_call  radialb
  !+ad_call  rfppfc
  !+ad_call  rfppfp
  !+ad_call  rfptfc
  !+ad_call  startup
  !+ad_call  stout
  !+ad_call  strucall
  !+ad_call  tfcoil
  !+ad_call  tfpwr
  !+ad_call  tfspcall
  !+ad_call  vaccall
  !+ad_hist  23/01/97 PJK Initial upgraded version. Split routine POWER
  !+ad_hisc               into POWER1 and POWER2
  !+ad_hist  06/02/97 PJK Added routine LOCA
  !+ad_hist  21/03/97 PJK Added routine IFEOUT
  !+ad_hist  18/11/97 PJK Removed NOUT argument from FISPAC call
  !+ad_hist  19/05/99 PJK Added routine AVAIL
  !+ad_hist  20/09/11 PJK Initial F90 version
  !+ad_hist  24/09/12 PJK Swapped argument order of RADIALB, DIVCALL, INDUCT
  !+ad_hist  10/10/12 PJK Moved routine from output.f90 to aamain.f90
  !+ad_hist  15/10/12 PJK Added costs_module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added physics_module
  !+ad_hist  17/10/12 PJK Added current_drive_module
  !+ad_hist  17/10/12 PJK Added divertor_module
  !+ad_hist  18/10/12 PJK Added fwbs_module
  !+ad_hist  18/10/12 PJK Added pfcoil_module
  !+ad_hist  29/10/12 PJK Added tfcoil_module
  !+ad_hist  29/10/12 PJK Added sctfcoil_module
  !+ad_hist  29/10/12 PJK Added structure_module
  !+ad_hist  29/10/12 PJK Added vacuum_module
  !+ad_hist  30/10/12 PJK Added power_module
  !+ad_hist  30/10/12 PJK Added buildings_module
  !+ad_hist  30/10/12 PJK Added build_module
  !+ad_hist  31/10/12 PJK Added stellarator_variables
  !+ad_hist  31/10/12 PJK Added stellarator_module
  !+ad_hist  05/11/12 PJK Added rfp_variables
  !+ad_hist  05/11/12 PJK Added rfp_module
  !+ad_hist  05/11/12 PJK Added ife_variables
  !+ad_hist  05/11/12 PJK Added ife_module
  !+ad_hist  05/11/12 PJK Added pulse_module
  !+ad_hist  06/11/12 PJK Added startup_module
  !+ad_hist  06/11/12 PJK Added availability_module
  !+ad_hist  19/06/14 PJK Removed obsolete calls to nbeam, ech, lwhymod
  !+ad_hist  09/07/14 PJK Turned on error handling
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use availability_module
  use build_module
  use buildings_module
  use costs_module
  use current_drive_module
  use divertor_module
  use error_handling
  use fwbs_module
  use ife_module
  use ife_variables
  use pfcoil_module
  use physics_module
  use physics_variables
  use power_module
  use pulse_module
  use rfp_module
  use rfp_variables
  use sctfcoil_module
  use startup_module
  use stellarator_module
  use stellarator_variables
  use structure_module
  use tfcoil_module
  use vacuum_module

  implicit none

  !  Arguments

  integer, intent(in) :: outfile

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Turn on error reporting
  !  (warnings etc. encountered in previous iterations may have cleared themselves
  !  during the solution process)

  errors_on = .true.

  !  Call stellarator output routine instead if relevant

  if (istell /= 0) then
     call stout(outfile)
     return
  end if

  !  Call inertial fusion energy output routine instead if relevant

  if (ife /= 0) then
     call ifeout(outfile)
     return
  end if

  call costs(outfile,1)
  call avail(outfile,1)
  call outplas(outfile)
  !call startup(outfile,1)  !  commented-out for speed reasons
  call igmarcal(outfile)
  call cudriv(outfile,1)
  call pulse(outfile,1)
  call outtim(outfile)
  call divcall(outfile,1)
  call radialb(outfile,1)

  if (irfp == 0) then
     call tfcoil(outfile,1)
  else
     call rfptfc(outfile,1)
  end if

  call tfspcall(outfile,1)

  if (itart == 1) call cntrpst(outfile,1)

  if (irfp == 0) then
     call outpf(outfile)
  else
     call rfppfc(outfile,1)
  end if

  if (irfp == 0) call outvolt(outfile)

  call strucall(outfile,1)

  if (irfp == 0) call induct(outfile,1)

  call fwbs(outfile,1)

  if (ifispact == 1) then
     call fispac(0)
     call fispac(1)
     call loca(outfile,0)
     call loca(outfile,1)
  end if

  call tfpwr(outfile,1)

  if (irfp == 0) then
     call pfpwr(outfile,1)
  else
     call rfppfp(outfile,1)
  end if

  call vaccall(outfile,1)
  call bldgcall(outfile,1)
  call acpow(outfile,1)
  call power2(outfile,1)

end subroutine output

! SVN 145: New CICC plots for User Guide
! SVN 149: MGF power usage correction
! SVN 150: Machine parameters use Fortran intrinsics
! SVN 151: Minor comment changes relating to coils, powers, buildings
! SVN 152: Changes to vacuum vessel and cryostat dimensions
! SVN 153: Corrected maximum crane lift requirement
! SVN 154: Building volume multipliers now input parameters;
!          corrected maintenance building height calculation
! SVN 155: Corrected blanket and shield height calculations;
!          clarified 'inner' to 'inboard', and 'outer' to 'outboard' throughout
! SVN 156: Modified case and winding pack thicknesses for TF nuclear heating calcs.
! SVN 157: Removed some obsolete variables, changed some default switch settings;
!          changed beryllium density
! SVN 158: Removed energy storage building if lpulse=0
! SVN 159: Corrected in-code comments about tfckw
! SVN 160: Corrected use of fhole in pulse.f90 (improved first wall nuclear heating
!          calculation)
! SVN 161: PF coil case area modified for superconducting coils; new sigpfcf input
! SVN 162: sigpfcalw added, replacing sigpfalw in PF coil case area calculation
! SVN 163: Modified isumattf usage; implemented Richard Kemp's corrections to itersc
! SVN 164: Removed cohbof; fcohbof no longer an input parameter
! SVN 165: Corrected various power conversion values
! SVN 166: Added new switch iprimnloss to control destiny of pnucloss
! SVN 167: New switch fwbsshape to control first wall, blanket, shield and
!          vacuum vessel cross-sectional shape; volume and area calculations for
!          these fully updated
! SVN 168: Swapped build order of vacuum vessel and adjacent gaps
! SVN 169: Added new KIT blanket neutronics model (User Guide still to do)
! SVN 170: Updated User Guide to describe new blanket model
! SVN 171: Top/bottom shield thickness now calculated if new blanket model is in use
! SVN 172: Produced in-source comments for new blanket model; added a number of
!          requested outputs. New ISHAPE=2 option for elongation scaling with aspect ratio
! SVN 173: Corrected rlp, ipdot, tohsmn calculations; changed units for qfuel, rndfuel;
!          added several more requested outputs
! SVN 174: Corrected reactor building height and vacuum vessel mass calculations;
!          removed dign; added section to User Guide about requirements for new models;
!          modified/clarified various comments
! SVN 175: Removed obsolete TF coil current density equation 23
! SVN 176: Corrected NBI path length calculation
! SVN 177: Minor changes to allow compilation with gfortran (4.6.3 - won't compile with
!          gfortran 4.4.5...)
! SVN 178: Correction to cryostat radius used in buildings call; modified swing time
!          comments; removed 'Troyon' descriptor for tokamak beta limits
! SVN 179: Updated plotting utilities (requires python 2.7.3 or higher)
! SVN 180: Corrected long-standing niggle with zeffai formula; now matches description
! SVN 181: Modified numerics output hints for optimising runs
! SVN 182: New stellarator plasma geometry and divertor models incorporated
! SVN 183: Comment changes
! SVN 184: Fixed a number of discrepancies in the D-He3 model, although the fusion
!          power calculations use different fits for D-T fusion to Bosch-Hale
! SVN 185: Rationalised fusion power calculations to use Bosch-Hale parametrization
!          in all cases; no iiter or idhe3 switches; D-He3 reaction is now controlled
!          via fhe3 only
! SVN 186: Removed obsolete fusion power routines; fixed problem with betaft if fdeut=1
! SVN 187: New port size (beam tangency radius) calculation
! SVN 188: Modified output formatting for reporting of arrays set in input file
! SVN 189: Incorporated Fabrizio Franza's suggested revisions to the KIT blanket model
! SVN 190: Added Psep/R limit equation (no.56)
! SVN 191: Re-assigned isumattf=2 to new Bi-2212 high-temperature superconductor model
! SVN 192: Minor adjustment of multiplier in beta calculations for consistency
! SVN 193: Correction to fast neutron flux profiles in VV in KIT blanket model
! SVN 194: Added fwareaib, fwareaob to calculations in stfwbs
! SVN 195: Modified stellarator blanket thicknesses consistently with KIT blanket model
! SVN 196/7 : Modified TF coil case mass calculation
! SVN 198: Corrected thermal energy outputs by 3/2 factor
! SVN 199: Unified kappa95 definition to be kappa/1.12
! SVN 200: Removed obscure upper limit on divertor null-to-strike distance;
!          Changed 'breeding unit' to 'breeding zone' in KIT blanket model;
!          Raised tftort upper limit
! SVN 201: Changed boundl(25: fpnetel) to 0.001 from 1.0
! SVN 202: Changed some other unusual boundl, boundu values
! SVN 203: Fix in vacuum.f90 to remove runtime error problem
! SVN 204: Fix in induct routine to prevent problems if ncls(1)=1
! SVN 205: Improved OH coil self inductance calculation, and OH coil to
!          plasma mutual inductance
! SVN 206: New output taup/taueff;
!          Erroneous decimal points present within input lines for integer variables
!          are now discarded with a warning message;
!          Added information about scanning variable to output file
! SVN 207: Lowered minimum input value for ralpne to 1.0D-12;
!          Added theat effects to flux consumption calculations
! SVN 208: Clarified usage of in-line comments in input file
! SVN 209: Added ohmic power to bigq denominator;
!          Modified poloidal field calculation for conventional tokamaks;
!          Moved pfrmax, pfmmax calculations for tokamaks into PF coil module;
!          Added Psep/R to output variables in PLOT.DAT
! SVN 210: Modified TF outboard leg calculation for resistive coils
! SVN 211: New scanning variable 27: tbrmin
! SVN 212: New iteration variable 98: li6enrich
! SVN 213: Fusion power for each fuel ion pair now output separately
! SVN 214: Current profile consistency option (iprofile=1) introduced
! SVN 215: Modified LSA usage in first wall costs;
!          Added argument to constraints to give the option of evaluating only a single
!          chosen constraint equation rather than all of them
! SVN 216: Fixed error with previous version; nvrbl --> nvar
! SVN 217: Typo fix in manual
! SVN 218: Changed epsfcn description
! SVN 219: Improved initialisation by calling 'caller' twice at the start of a run
!          and modifying some initial estimates for quantities. Added unit testing
!          code for VMCON
! SVN 220: New figure of merit cfactr; new scanning variable bt
! SVN 221: Constraint (limit) equations made uniform in style
! SVN 222: Minor mods to prevent gfortran compilation errors. Also added tratio usage
!          to calculate ti from te for stellarators
! SVN 223: Added new output channel mfile (MFILE.DAT) to write out machine-readable
!          data. Also changed space characters in PLOT.DAT to underscores.
! SVN 224: VMCON ifail flag now written to output under all circumstances
! SVN 225: Updated rkemp's python utilities
! SVN 226: Added hlux's PROCESS_dicts.py utility;
!          Modified code output for costs and machine build, particularly to MFILE.DAT
! SVN 227: Draft implementation of pedestal profiles (use ipedestal=1)
! SVN 228: Fixed alphap problem in culbst
! SVN 229: Corrected misunderstanding about pressure profiles introduced at SVN 227
! SVN 230: Removed echoing of long lines in the input file to standard output
! SVN 231: Added use of general plasma profiles into current drive module.
!          Rationalised (simplified) argument lists for current drive routines.
!          Moved plasma profile routines into new source file and module to prevent
!          circular compilation issues.
! SVN 232: Fixed Id problem in plasma_profiles.f90
! SVN 233: Fixed error in tcore calculation
! SVN 234: Added verbose switch, plus diagnostic output in maths_library.f90;
!          VMCON line search now exits and restarts if conditions appear unfavourable
! SVN 235: Typo fix in maths_library.f90;
!          Added mkovari's write_constraints.py utility
! SVN 236: Minor additions to User Guide;
!          Trapped problem with port size calculation if coil spacing is too narrow;
!          Trapped probable negative square root argument if OH coil is very wide
! SVN 237: Used HYBRD throughout instead of HYBRID;
!          Added new argument niter to VMCON;
!          Added lists of figures and tables to User Guide
! SVN 238: Added constraints 57, 58, and iteration variables 99, 100
! SVN 239: Updated PROCESS_dicts.py
! SVN 240: Modified code to allow usage of inequality constraints in the future
! SVN 241: Minor output modifications
! SVN 242: Incorporated the new stellarator coil model;
!          Updated the stellarator description in the User Guide;
!          Moved a few maths utility routines into maths_library.f90;
!          Fixed problem with fusion reaction rate if temperature = zero;
!          Modified a few comments and added central profile values to output;
!          Corrected D-D reaction rates
! SVN 243: Minor comment changes; User Guide stellarator wording changes (F Warmer)
! SVN 244: Output of floats to mfile now always in scientific 'E' format;
!          Warning added if isumattf=2 range of validity is not upheld;
!          Warning about pdivt = 0.001 added;
!          Clarified ishape effects on kappa, triang
! SVN 245: Added references to STAR Code formulae
! SVN 246: Corrected tcore formula for pedestal profiles
! SVN 247: Added numerical state information to mfile
! SVN 248: Added Sauter et al bootstrap current fraction model
! SVN 249: Tidied up comments in Sauter et al model; added ibss=4 to User Guide;
!          Added run-time info, PF coil and TF coil geometry to mfile
! SVN 250: Tidied up string output to MFILE.DAT;
!          Added new description of optimisation algorithm to User Guide
!          (N.B. LaTeX not working properly due to problem with flow diagram)
! SVN 251: Corrected problem with User Guide
! SVN 252: Added isumattf to mfile
! SVN 253: Added full list of python utilities to repository, plus a new write-up in the
!          User Guide
! SVN 254: Minor changes to write_constraints.py
! SVN 255: Modified tfleng calculation to use tfthko on outboard side
! SVN 256: Added vertical field calculation
! SVN 257: Correction to ensure final solution vector is consistent with results
!          in the rest of the output
! SVN 258: Ensured that all quantities are re-calculated, regardless of the iprint
!          value at each call. This exposed a problem in availability.f90 in which
!          cfactr was not being taken into account in the lifetime values written
!          to the output file.
!          Also uploaded latest write_new_in_dat.py, process_funcs.py
! SVN 259: Updated write_new_in_dat.py, and added plot_mfile_sweep.py;
!          Corrected one line in scan.f90;
!          Added new two-layer TF coil stress model (stress_model=1) (draft only);
!          Small change in definition of rbmax for superconducting tokamak TF coils
! SVN 260: Brought process_dicts.py up to date with current code
! SVN 261: Changed ripmax default value to 1.0 percent
! SVN 262: Clarified logic for gtscale, iprofile interaction
! SVN 263: Clarified energy multiplication vs fusion gain wording
! GIT 263b: Changed SVN keywords to be updated manually
! GIT 264: Fixed progver format problem
! GIT 265: Removed wpvf usage
! GIT 266: New draft of stress model; replaced itfmod and stress_model with tfc_model
! GIT 267: Updated TF coil picture and description in User Guide
! GIT 268: Modified constraint 28 by adding new input parameter bigqmin
! GIT 269: Changed ripmax description; changed taup calculation to use alpharate
!          instead of fusionrate
! GIT 270: Tidied up comments in tfcpwr
! GIT 271: Added radial strain in insulator
! GIT 272: Initial draft of new impurity radiation model
! GIT 273: Modified python utility headers
! GIT 274: Made corrections to Sauter bootstrap fraction formulae as suggested by Fable
! GIT 275: Minor corrections to python utilities; added hyperlinks to User Guide
! GIT 276: Increased length of output lines
! GIT 277: Added vstot to output; removed ffwal from iwalld=2 calculation
! GIT 278: Removed tburn consistency equation, and replaced it with an internal loop;
!          ensured tburn is not negative (warning given if insufficient volt-seconds)
! GIT 279: Clarified core radiation usage; new radiation power constraint eqn;
!          introduced iradloss switch; corrected falpha usage (at least partially)
! GIT 280: Added warning if impurity temperature is below tabulated values
! GIT 281: Added several clauses for ignite switch to ensure injected power is
!          treated as zero for steady state power balance calculations. The usage
!          of ignite is now thought to be fully consistent throughout the code.
! GIT 282: Changed names (and in some cases, units) of several power-related variables
! GIT 283: Added iteration variable 102, fimpvar
! GIT 284: Changed a few more power-related variable names
! GIT 285: Added new scan variable coreradius
! GIT 286: Corrections to tfc_model=2
! GIT 287: New power flow model
! GIT 288: Correction to process_dicts.py
! GIT 289: Raised input upper limit on fimpvar; updated python library files
! GIT 290: Minor fix to mfile.py
! GIT 291: Removed duplicate outputs from mfile; 'make clean' now deletes all html files
!          and the User Guide pdf file
! GIT 292: Added fimpvar as scan variable 30
! GIT 293: Fixed small errors/inconsistencies in new power flow model
! GIT 294: Added impdir to allow impurity radiation datafile directory to be specified
! GIT 295: New confinement time scaling law DS03 (no.39)
! GIT 296: New ripple amplitude calculation
! GIT 297: Simplified current drive calculations
! GIT 298: Removed output section controlling flags sect?? (they were never used anyway,
!          and now it is important that all sections are output otherwise the mfile
!          will be incomplete)
! GIT 299: Corrected wallmw units in output files
! GIT 300: Removed references to bucking cylinder; updated in_dat.py library utility;
!          Blanket top/bottom thickness now always calculated rather than input
! GIT 301: Update mfile.py, plot_proc_func.py
! GIT 302: Corrected wallmw calculation to account for gaps in first wall
! GIT 303: Preliminary modifications to fispact.f90 for its possible resurrection;
!          Draft implementation of error handling module
! GIT 304: Added a tolerance level for the constraint residuals to the VMCON
!          convergence criteria;
!          Added a possible remedy to help with VMCON ifail=5 results
! GIT 305: Error handling now reports only during output steps, not during intermediate
!          iterations
! GIT 306: Range-normalised iteration variable values added to mfile
! GIT 307: Raised maximum number of scan points to 200
! GIT 308: Updated process_funcs.py
! GIT 309: Modified output banner and run description handling
! GIT 310: Constraint residues summary now output in physical units
! GIT 311: Updated in_dat.py
! GIT 312: Added fix for negative ion density occurrences at low electron density
! GIT 313: Corrected neutron power deposition in first wall for pulsed plants using
!          ipowerflow=1. Uncommented error trap in routine cycles.
! GIT 314: TF coil toroidal thickness tftort now calculated instead of input for
!          tokamaks
! GIT 315: Changed TF coil outboard radial thickness calculation
! GIT 316: tfthko now equal to tfcth for tokamaks; improved TF coil conductor mass calculations
! GIT 317: Updated run_process.py, process_config.py, process_funcs.py, process_dicts.py,
!          write_new_in_dat.py, in_dat.py
! GIT 318: Removed obsolete variables, other minor tidy-ups
! GIT 319: Removed casfact; added some variables to output files; trapped nvar < neqns
! GIT 320: Set fshine to zero if it is negligible; updated process_funcs.py;
!          trapped insufficient numbers of specified ixc, icc elements
! GIT 321: Added diagnose_process.py utility + funcs; minor wording changes elsewhere.
! GIT 322: Error list now read in from a JSON file
! GIT 323: New peak TF with ripple calculation; modified ripple calculation applicability range
! GIT 324: New scaling for PF coil to cryostat lid clearance
! GIT 325: Updated impurity radiation datafiles
! GIT 326: Added additional power balance outputs
! GIT 327: Added verbose output to VFILE.DAT
! GIT 328: Added error traps to pedestal profile routines
! GIT 329: Added tfcryoarea in advance of a change to the steady state cryogenic load calculation
! GIT 330: Removed Myall TF coil stress model; tfc_model switch usage changed
! GIT (new_defaults branch): Changed default values to approximate ITER-FDR (ITER98) design
! GIT (new_defaults branch): Updated/re-ordered variable descriptions
! GIT 331: Updated plot_proc_func.py
! GIT 332: Attempted to clarify zref usage
! GIT 333: Minor comment changes
! GIT 334: Merged new_defaults branch into develop branch
! GIT 335: Introduced a_to_b python utility
! GIT 336: Root directory is set via Makefile and new shell script setrootdir
! GIT 337: Corrections to create_dicts.py
! GIT 338: Minor corrections arising from gfortran warnings/error reports; added
!          impuritydata files to repository
! GIT 339: Merged process_gui branch into develop branch
! GIT 340: Rearranged GUI directory contents; reworded parts of User Guide
! GIT 341: September 2014 Master Release
! GIT 342: Added more ishape options
! GIT 343: Added L-H power threshold constraint
! GIT 344: Fixed problem with create_dicts.py (need to remove *.f90*~ files before running);
!          added conducting shell radius to rminor ratio constraint
! GIT 345: New NBI shine-through fraction constraint
