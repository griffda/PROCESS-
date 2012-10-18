!  $Id::                                                                $
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
  !+ad_desc  <P>PROCESS is being upgraded from FORTRAN 77 to Fortran 95, to
  !+ad_desc  facilitate the restructuring of the code into proper modules (with all
  !+ad_desc  the benefits that modern software practices bring), and to
  !+ad_desc  aid the inclusion of more advanced physics and engineering models under
  !+ad_desc  development as part of a number of EFDA-sponsored collaborations.
  !+ad_prob  None
  !+ad_call  numerics
  !+ad_call  process_input
  !+ad_call  process_output
  !+ad_call  scan_module
  !+ad_call  eqslv
  !+ad_call  final
  !+ad_call  init
  !+ad_call  oheadr
  !+ad_call  scan
  !+ad_hist  03/10/96 PJK Upgrade of main program unit
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  09/10/12 PJK Modified to use scan_module
  !+ad_hist  10/10/12 PJK Modified to use numerics module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !+ad_docs  Box file F/RS/CIRE5523/PWF (up to 15/01/96)
  !+ad_docs  Box file F/MI/PJK/PROCESS and F/PL/PJK/PROCESS (15/01/96 to 24/01/12)
  !+ad_docs  Box file T&amp;M/PKNIGHT/PROCESS (from 24/01/12)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

  call oheadr(nout,'End of PROCESS Output')
  call oheadr(iotty,'End of PROCESS Output')

  close(unit=nin)
  close(unit=nplot)
  close(unit=nout)

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
  !+ad_call  global_variables
  !+ad_call  numerics
  !+ad_call  process_input
  !+ad_call  process_output
  !+ad_call  check
  !+ad_call  codever
  !+ad_call  initial
  !+ad_call  input
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  run_summary
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  17/11/97 PJK Changed file names to *.DAT
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  09/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added global_variables module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use global_variables
  use process_input
  use process_output
  use numerics

  implicit none

  !  Arguments

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise the program variables

  call initial

  !  Open the three input/output external files

  open(unit=nin,file='IN.DAT',status='old')
  open(unit=nplot,file='PLOT.DAT',status='unknown')
  open(unit=nout,file='OUT.DAT',status='unknown')

  !  Print code banner + run details

  call codever(nout)
  call codever(iotty)

  !  Input any desired new initial values

  call input

  !  Check input data for errors/ambiguities

  call check

  !  Print code version

  call oblnkl(nout)
  call ocmmnt(nout,icase)
  call ocmmnt(iotty,icase)
  call oblnkl(iotty)

  !  Write to the output file certain relevant details about this run

  call run_summary

end subroutine init

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine codever(outfile)

  !+ad_name  codever
  !+ad_summ  Prints out the code version and other run-specific information
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  outfile   : input integer : output file unit
  !+ad_desc  This routine prints out the code version and various other
  !+ad_desc  run-specific details.
  !+ad_prob  None
  !+ad_call  process_output
  !+ad_call  inform
  !+ad_call  oblnkl
  !+ad_call  ocentr
  !+ad_call  ostars
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  17/11/97 PJK Changed file names to *.DAT
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output

  implicit none

  !  Arguments

  integer, intent(in) :: outfile

  !  Local variables

  integer, parameter :: width = 72
  character(len=width), dimension(0:10) :: progid

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Write out banner

  call oblnkl(outfile)
  call ostars(outfile,width)
  call ocentr(outfile,'PROCESS',width)
  call ocentr(outfile,'Power Reactor Optimisation Code',width)
  call ocentr(outfile,'for Environmental and Safety Studies',width)
  call ostars(outfile,width)
  call oblnkl(outfile)

  !  Obtain details of this run

  call inform(progid)

  !  Write out details

  write(outfile,*) progid(1)
  write(outfile,*) progid(2)
  write(outfile,*) progid(3)
  write(outfile,*) progid(4)
  write(outfile,*) progid(5)
  write(outfile,*) progid(6)

  call oblnkl(outfile)
  call ostars(outfile,width)
  call oblnkl(outfile)

end subroutine codever

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
  !+ad_call  function_evaluator
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  eqsolv
  !+ad_call  fcnhyb
  !+ad_call  herror
  !+ad_call  loadxc
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  osubhd
  !+ad_call  ovarin
  !+ad_call  ovarre
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics, function_evaluator
  !+ad_hisc               modules
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output
  use numerics
  use function_evaluator

  implicit none

  !  Arguments

  integer, intent(out) :: ifail

  !  Local variables

  real(kind(1.0D0)) :: sumsq
  real(kind(1.0D0)), dimension(iptnt) :: wa
  integer :: inn,nprint,nx

  !  External routines

!  external :: fcnhyb

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

  !  Print out information on solution

  call oheadr(nout,'Numerics')
  call ocmmnt(nout, &
       'PROCESS has performed a HYBRD (non-optimisation) run,')

  if (ifail /= 1) then
     call ocmmnt(nout,'but could not find a feasible set of parameters.')
     call oblnkl(nout)
     call ovarin(nout,'HYBRD error flag','(ifail)',ifail)

     call oheadr(iotty,'PROCESS COULD NOT FIND A FEASIBLE SOLUTION')
     call ovarin(iotty,'HYBRD error flag','(ifail)',ifail)
     call oblnkl(iotty)
  else
     call ocmmnt(nout,'and found a feasible set of parameters.')
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

  !  If necessary, write out the relevant error message

  if (ifail /= 1) then
     call oblnkl(nout)
     call herror(ifail)
     call oblnkl(iotty)
  end if

  call osubhd(nout,'The solution vector is comprised as follows :')

  write(nout,20)
20 format(t5,'i',t23,'final',t33,'fractional',t46,'residue')

  write(nout,30)
30 format(t23,'value',t35,'change')

  call oblnkl(nout)

  do inn = 1,neqns
     xcs(inn) = xcm(inn)*scafc(inn)
     write(nout,40) inn,lablxc(ixc(inn)),xcs(inn),xcm(inn),resdl(inn)
40   format(t2,i4,t8,a8,t19,1pe12.4,1pe12.4,1pe12.4)
  end do

  call osubhd(nout, &
       'The following constraint residues should be close to zero :')

  do inn = 1,neqns
     write(nout,60) inn,lablcc(icc(inn)),rcm(inn)
60   format(t2,i4,t8,a34,t45,1pe12.4)
  end do

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
  !+ad_call  ocmmnt
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
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
     call ocmmnt(nout,'Try changing the variables in IXC.')

     call ocmmnt(iotty,'The iteration is not making good progress.')
     call ocmmnt(iotty,'Try changing the variables in IXC.')

  case default
     call ocmmnt(nout, 'This value of IFAIL should not be possible...')
     call ocmmnt(nout,'See source code for details.')

     call ocmmnt(iotty,'This value of ifail should not be possible...')
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
     call ocmmnt(nout, 'solution, suggesting that the iteration is not')
     call ocmmnt(nout,'making good progress.')
     call ocmmnt(nout,'Try changing or adding variables to IXC.')

     call ocmmnt(iotty, &
          'The maximum number of calls has been reached without')
     call ocmmnt(iotty,'solution, suggesting that the iteration is not')
     call ocmmnt(iotty,'making good progress.')
     call ocmmnt(iotty,'Try changing or adding variables to IXC.')

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
     call ocmmnt(nout,'Try changing or adding variables to IXC.')

     call ocmmnt(iotty, &
          'The quadratic programming technique was unable to')
     call ocmmnt(iotty,'find a feasible point.')
     call ocmmnt(iotty,'Try changing or adding variables to IXC.')

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
     call ocmmnt(nout,'This value of ifail should not be possible...')
     call ocmmnt(nout,'See source code for details.')

     call ocmmnt(iotty,'This value of ifail should not be possible...')
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
  !+ad_call  function_evaluator
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  boundxc
  !+ad_call  loadxc
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  optimiz
  !+ad_call  osubhd
  !+ad_call  ovarin
  !+ad_call  ovarre
  !+ad_call  verror
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics and function_evaluator
  !+ad_hisc               modules
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_output
  use numerics
  use function_evaluator

  implicit none

  !  Arguments

  integer, intent(out) :: ifail

  !  Local variables

  real(kind(1.0D0)) :: summ,xcval,xmaxx,xminn,f
  integer :: ii,inn,iflag

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  If no optimisation is required, leave the routine

  if (ioptimz < 0) return

  !  Set up variables to be iterated

  call loadxc
  call boundxc
  call optimiz(fcnvmc1,fcnvmc2,ifail,f)

  !  Check on accuracy of solution by summing the
  !  squares of the residuals

  summ = 0.0D0
  do ii = 1,neqns
     summ = summ + rcm(ii)*rcm(ii)
  end do
  sqsumsq = sqrt(summ)

  !  Print out information on solution

  call oheadr(nout,'Numerics')
  call ocmmnt(nout,'PROCESS has performed a VMCON (optimisation) run,')
  if (ifail /= 1) then
     call ocmmnt(nout,'but could not find a feasible set of parameters.')

     call oheadr(iotty,'PROCESS COULD NOT FIND A FEASIBLE SOLUTION')
     call ovarin(iotty,'VMCON error flag','(ifail)',ifail)
     call oblnkl(iotty)
  else
     call ocmmnt(nout,'and found a feasible set of parameters.')
     call oheadr(iotty,'PROCESS found a feasible solution')
  end if

  call oblnkl(nout)

  !  If necessary, write the relevant error message

  if (ifail /= 1) then
     call verror(ifail)
     call oblnkl(nout)
     call oblnkl(iotty)
  end if

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
     write(nout,20) lablmm(abs(minmax))
  else
     write(nout,30) lablmm(abs(minmax))
  end if
20 format(' to minimise the ',a22)
30 format(' to maximise the ',a22)

  call oblnkl(nout)

  !  Check which variables are at bounds

  iflag = 0
  do ii = 1,nvrbl
     xminn = 1.01D0*bondl(ii)
     xmaxx = 0.99D0*bondu(ii)

     if (xcm(ii) < xminn) then
        if (iflag == 0) then
           call ocmmnt(nout, &
                'Certain operating limits have been reached,')
           call ocmmnt(nout, &
                'as shown by the following variables that are')
           call ocmmnt(nout, &
                'at the edge of their prescribed range :')
           call oblnkl(nout)
           iflag = 1
        end if
        xcval = xcm(ii)*scafc(ii)
        write(nout,40) ii,lablxc(ixc(ii)),xcval
     end if

     if (xcm(ii) > xmaxx) then
        if (iflag == 0) then
           call ocmmnt(nout, &
                'Certain operating limits have been reached,')
           call ocmmnt(nout, &
                'as shown by the following variables that are')
           call ocmmnt(nout, &
                'at the edge of their prescribed range :')
           call oblnkl(nout)
           iflag = 1
        end if
        xcval = xcm(ii)*scafc(ii)
        write(nout,50) ii,lablxc(ixc(ii)),xcval
     end if
  end do

40 format(t4,'Variable ',i3,'  (',a8, &
        ')  is at its lower bound of ',1pe12.4)
50 format(t4,'Variable ',i3,'  (',a8, &
        ')  is at its upper bound of ',1pe12.4)

  !  Print out information on numerics

  call osubhd(nout,'The solution vector is comprised as follows :')
  write(nout,70)
70 format(t47,'lower',t59,'upper')

  write(nout,80)
80 format(t23,'final',t33,'fractional',t46,'Lagrange',t58,'Lagrange')

  write(nout,90)
90 format(t5,'i',t23,'value',t35,'change',t45,'multiplier', &
        t57,'multiplier')

  call oblnkl(nout)

  do inn = 1,nvrbl
     xcs(inn) = xcm(inn)*scafc(inn)
     write(nout,100) inn,lablxc(ixc(inn)),xcs(inn),xcm(inn), &
          vlam(neqns+inn), vlam(neqns+1+inn+nvrbl)
  end do
100 format(t2,i4,t8,a8,t19,4(1pe12.4))

  call osubhd(nout, &
       'The following constraint residues should be close to zero :')

  do inn = 1,neqns
     write(nout,120) inn,lablcc(icc(inn)),rcm(inn),vlam(inn)
  end do
120 format(t2,i4,t8,a34,t45,1pe12.4,1pe12.4)

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

  write(iotty,10) nfev1,nfev2,ncalls
10 format( &
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
  !+ad_call  costs_module
  !+ad_call  current_drive_module
  !+ad_call  divertor_module
  !+ad_call  fwbs_module
  !+ad_call  pfcoil_module
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  stella.h90
  !+ad_call  rfp.h90
  !+ad_call  ife.h90
  !+ad_call  acpow
  !+ad_call  avail
  !+ad_call  bldgcall
  !+ad_call  cntrpst
  !+ad_call  costs
  !+ad_call  cudriv
  !+ad_call  divcall
  !+ad_call  ech
  !+ad_call  fispac
  !+ad_call  fwbs
  !+ad_call  ifeout
  !+ad_call  igmarcal
  !+ad_call  induct
  !+ad_call  loca
  !+ad_call  lwhymod
  !+ad_call  nbeam
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use costs_module
  use current_drive_module
  use divertor_module
  use fwbs_module
  use pfcoil_module
  use physics_module
  use physics_variables

  implicit none

  include 'stella.h90'
  include 'rfp.h90'
  include 'ife.h90'

  !  Arguments

  integer, intent(in) :: outfile

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
  call nbeam(outfile,1)
  call ech(outfile,1)
  call lwhymod(outfile,1)

end subroutine output
