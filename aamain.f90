!  $Id::                                                                $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
  !+ad_call  param.h90
  !+ad_call  numer.h90
  !+ad_call  eqslv
  !+ad_call  final
  !+ad_call  init
  !+ad_call  oheadr
  !+ad_call  scan
  !+ad_hist  03/10/96 PJK Upgrade of main program unit
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !+ad_docs  Box file F/RS/CIRE5523/PWF (up to 15/01/96)
  !+ad_docs  Box file F/MI/PJK/PROCESS and F/PL/PJK/PROCESS (15/01/96 to 24/01/12)
  !+ad_docs  Box file T&amp;M/PKNIGHT/PROCESS (from 24/01/12)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'

  !  Arguments

  !  Local variables

  integer :: ifail

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise things

  call init

  !  Call equation solver (HYBRD)

  call eqslv(ifail)

  !  Call routine to do optimisation sweeps

  if (ioptimz >= 0) then
     call scan
  else
     call final(ifail)
  end if

  call oheadr(nout,'End of PROCESS Output')
  call oheadr(iotty,'End of PROCESS Output')

  close(unit=nin)
  close(unit=11)  !  11=nplot
  close(unit=nout)

end program process

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine init

  !+ad_name  init
  !+ad_summ  Routine that calls the initialisation routines
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine calls the main initialisation routines that set
  !+ad_desc  the default values for the global variables, read in data from
  !+ad_desc  the input file, and check the run parameters for consistency.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  numer.h90
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use process_input

  implicit none

  include 'param.h90'
  include 'numer.h90'

  !  Arguments

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise the program variables

  call initial

  !  Open the three input/output external files

  open(unit=nin,file='IN.DAT',status='old')
  open(unit=11,file='PLOT.DAT',status='unknown')  !  nplot=11
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine codever(nout)

  !+ad_name  codever
  !+ad_summ  Prints out the code version and other run-specific information
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout   : input integer : output file unit
  !+ad_desc  This routine prints out the code version and various other
  !+ad_desc  run-specific details.
  !+ad_prob  None
  !+ad_call  inform
  !+ad_call  oblnkl
  !+ad_call  ocentr
  !+ad_call  ostars
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  17/11/97 PJK Changed file names to *.DAT
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: nout

  !  Local variables

  integer, parameter :: width = 72
  character(len=width), dimension(0:10) :: progid

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Write out banner

  call oblnkl(nout)
  call ostars(nout,width)
  call ocentr(nout,'PROCESS',width)
  call ocentr(nout,'Power Reactor Optimisation Code',width)
  call ocentr(nout,'for Environmental and Safety Studies',width)
  call ostars(nout,width)
  call oblnkl(nout)

  !  Obtain details of this run

  call inform(progid)

  !  Write out details

  write(nout,*) progid(1)
  write(nout,*) progid(2)
  write(nout,*) progid(3)
  write(nout,*) progid(4)
  write(nout,*) progid(5)
  write(nout,*) progid(6)

  call oblnkl(nout)
  call ostars(nout,width)
  call oblnkl(nout)

end subroutine codever

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine eqslv(ifail)

  !+ad_name  eqslv
  !+ad_summ  Routine to call the non-optimising equation solver
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : output integer : error flag
  !+ad_desc  This routine calls the non-optimising equation solver.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  numer.h90
  !+ad_call  labels.h90
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'
  include 'labels.h90'

  !  Arguments

  integer, intent(out) :: ifail

  !  Local variables

  real(kind(1.0D0)) :: sumsq
  real(kind(1.0D0)), dimension(iptnt) :: wa
  integer :: inn,nprint,nx

  !  External routines

  external :: fcnhyb

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
     call herror(nout,iotty,ifail)
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine herror(nout,iotty,ifail)

  !+ad_name  herror
  !+ad_summ  Routine to print out relevant messages in the case of an
  !+ad_summ  unfeasible result from a HYBRD (non-optimisation) run
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout   : input integer : output file unit
  !+ad_args  iotty  : input integer : terminal output file unit
  !+ad_args  ifail  : input integer : error flag
  !+ad_desc  This routine prints out relevant messages in the case of
  !+ad_desc  an unfeasible result from a HYBRD (non-optimisation) run.
  !+ad_desc  <P>The messages are written to units NOUT and IOTTY, which are
  !+ad_desc  by default the output file and screen, respectively.
  !+ad_desc  <P>If <CODE>IFAIL=1</CODE> then a feasible solution has been
  !+ad_desc  found and therefore no error message is required.
  !+ad_prob  None
  !+ad_call  ocmmnt
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: nout,iotty,ifail

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine verror(nout,iotty,ifail)

  !+ad_name  verror
  !+ad_summ  Routine to print out relevant messages in the case of an
  !+ad_summ  unfeasible result from a VMCON (optimisation) run
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout   : input integer : output file unit
  !+ad_args  iotty  : input integer : terminal output file unit
  !+ad_args  ifail  : input integer : error flag
  !+ad_desc  This routine prints out relevant messages in the case of
  !+ad_desc  an unfeasible result from a VMCON (optimisation) run.
  !+ad_desc  <P>The messages are written to units NOUT and IOTTY, which are
  !+ad_desc  by default the output file and screen, respectively.
  !+ad_desc  <P>If <CODE>IFAIL=1</CODE> then a feasible solution has been
  !+ad_desc  found and therefore no error message is required.
  !+ad_prob  None
  !+ad_call  ocmmnt
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  integer, intent(in) :: nout,iotty,ifail

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine scan

  !+ad_name  scan
  !+ad_summ  Routine to call the optimisation routine VMCON over
  !+ad_summ  a range of values of one of the variables
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine calls the optimisation routine VMCON
  !+ad_desc  a number of times, by performing a sweep over a range of
  !+ad_desc  values of a particular variable.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  sweep.h90
  !+ad_call  phydat.h90
  !+ad_call  cdriv.h90
  !+ad_call  pwrcom.h90
  !+ad_call  tfcoil.h90
  !+ad_call  pfcoil.h90
  !+ad_call  ineq.h90
  !+ad_call  cost.h90
  !+ad_call  htpwr.h90
  !+ad_call  divrt.h90
  !+ad_call  numer.h90
  !+ad_call  doopt
  !+ad_call  final
  !+ad_call  oblnkl
  !+ad_call  ostars
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  01/04/98 PJK Added POWFMAX to list of scanning variables
  !+ad_hist  23/06/98 PJK Added KAPPA and TRIANG to list of scanning vars
  !+ad_hist  19/05/99 PJK Added warning about trying to scan CFACTR with
  !+ad_hisc               new availability model
  !+ad_hist  25/05/06 PJK Added implied-DO loops for sweep outputs
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'sweep.h90'
  include 'phydat.h90'
  include 'cdriv.h90'
  include 'pwrcom.h90'
  include 'tfcoil.h90'
  include 'pfcoil.h90'
  include 'ineq.h90'
  include 'cost.h90'
  include 'htpwr.h90'
  include 'divrt.h90'
  include 'numer.h90'

  !  Arguments

  !  Local variables

  character(len=25) :: xlabel,tlabel

  real(kind(1.0D0)), dimension(ipnscns) :: asp,bet,betl,blim,bq,bs, &
       btor,btt,ccst,cdt,ce,cec,cef,ceo,crc,curd,d20,eb,epbp,fcl,hfio, &
       hfip,hld,ifa,ip,lq,ocd,pbf,pcp,pfp,pg,pht,pinj,piwp,pmp,pn,powf, &
       qlm,rcl,rec1,rmaj,sq,str,t10,tfp,tmx,vcl,wall,wpf,wtf

  integer :: ifail,i,nplot

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  tlabel = icase

  if (isweep == 0) then
     call doopt(ifail)
     call final(ifail)
     return
  end if

  if (isweep > ipnscns) then
     write(*,*) 'Error in routine SCAN:'
     write(*,*) 'Illegal value of isweep, = ',isweep
     write(*,*) 'Maximum = ',ipnscns
     write(*,*) 'PROCESS stopping.'
     stop
  end if

  do i = 1,isweep

     call oblnkl(nout)
     call ostars(nout,72)
     write(nout,10) i,isweep
10   format(' ***** Scan point ',i2,' of ',i2,' *****')
     call ostars(nout,72)

     select case (nsweep)

     case (1)
        aspect = sweep(i)
        xlabel = 'Aspect Ratio'
     case (2)
        hldivlim = sweep(i)
        xlabel = 'Divertor Ht Limit MW/m2'
     case (3)
        pnetelin = sweep(i)
        xlabel = 'Net Elec pwr MW'
     case (4)
        hfact = sweep(i)
        xlabel = 'Confinement hfact'
     case (5)
        oacdcp = sweep(i)
        xlabel = 'j TF inner leg MA/m2'
     case (6)
        walalw = sweep(i)
        xlabel = 'All. wall load MW/m2'
     case (7)
        beamfus0 = sweep(i)
        xlabel = 'Beam back. multiplier'
     case (8)
        fqval = sweep(i)
        xlabel = '1/Big Q'
     case (9)
        te = sweep(i)
        xlabel = 'Temperature'
     case (10)
        boundu(15) = sweep(i)
        xlabel = 'Volt-sec upper bound'
     case (11)
        dnbeta = sweep(i)
        xlabel = 'Troyon coefficient'
     case (12)
        bscfmax = sweep(i)
        xlabel = 'Bootstrap Fraction'
     case (13)
        boundu(10) = sweep(i)
        xlabel = 'H factor upper bound'
     case (14)
        fiooic = sweep(i)
        xlabel = 'Iop / Icrit f-value'
     case (15)
        fjprot = sweep(i)
        xlabel = 'TFC J limit f-value'
     case (16)
        rmajor = sweep(i)
        xlabel = 'Plasma major radius'
     case (17)
        bmxlim = sweep(i)
        xlabel = 'Max toroidal field'
     case (18)
        gammax = sweep(i)
        xlabel = 'Maximum CD gamma'
     case (19)
        boundl(16) = sweep(i)
        xlabel = 'OHC thickness lower bound'
     case (20)
        tbrnmn = sweep(i)
        xlabel = 'Minimum burn time'
     case (21)
        sigpfalw = sweep(i)
        xlabel = 'Allowable PF coil stress'
     case (22)
        if (iavail == 1) then
           write(*,*) 'Error in routine SCAN:'
           write(*,*) 'Do not scan CFACTR if IAVAIL=1'
           write(*,*) 'PROCESS stopping.'
           stop
        end if
        cfactr = sweep(i)
        xlabel = 'Plant availability factor'
     case (23)
        boundu(72) = sweep(i)
        xlabel = 'Ip/Irod upper bound'
     case (24)
        powfmax = sweep(i)
        xlabel = 'Fusion Power limit'
     case (25)
        kappa = sweep(i)
        xlabel = 'Elongation'
     case (26)
        triang = sweep(i)
        xlabel = 'Triangularity'

     case default
        write(*,*) 'Error in routine SCAN:'
        write(*,*) 'Illegal scan variable number, nsweep = ',nsweep
        write(*,*) 'PROCESS stopping.'
        stop

     end select

     call doopt(ifail)
     call final(ifail)

     !  Store values for output

     ifa(i)  = real(ifail)
     sq(i)   = sqsumsq
     ce(i)   = coe
     cec(i)  = coecap
     cef(i)  = coefuelt
     ceo(i)  = coeoam
     ccst(i) = capcost
     crc(i)  = c221 + c222
     cdt(i)  = cdirt / 1.0D3
     rmaj(i) = rmajor
     asp(i)  = aspect
     ip(i)   = 1.0D-6 * plascur
     btor(i) = bt
     btt(i)  = btot
     lq(i)   = q
     qlm(i)  = qlim
     bet(i)  = beta
     blim(i) = betalim
     epbp(i) = betap / aspect
     t10(i)  = te/10.0D0 * pcoef
     d20(i)  = dene/1.0D20
     hfip(i) = hfac(6)
     hfio(i) = hfac(7)
     powf(i) = powfmw
     pbf(i)  = palpnb * 5.0D0
     wall(i) = wallmw
     pinj(i) = 1.0D-6 * (pinji + pinje)
     piwp(i) = pinjwp
     pht(i)  = pheat * 1.0D-6
     curd(i) = 1.0D-6 * (pinji + pinje - pheat)
     bq(i)   = bigq
     bs(i)   = bootipf
     eb(i)   = enbeam/1.0D3
     hld(i)  = hldiv
     tfp(i)  = tfcmw
     wtf(i)  = whttf
     str(i)  = sigrad + sigtan
     ocd(i)  = oacdcp/1.0D6
     tmx(i)  = tcpmax
     pcp(i)  = tfcpmw
     fcl(i)  = fcoolcp
     rcl(i)  = rcool
     vcl(i)  = vcool
     pmp(i)  = ppump/1.0D6
     pfp(i)  = 1.0D-3 * srcktpm
     wpf(i)  = whtpf
     pg(i)   = pgrossmw
     pn(i)   = pnetelmw
     if (ireactor == 1) then
        rec1(i) = (pgrossmw-pnetelmw) / pgrossmw
     else
        rec1(i) = 0.0D0
     end if
  end do

  nplot = 11

  write(nplot,900) isweep
  write(nplot,901) tlabel

  !+**PJK 20/10/92 Why does this section supersede one above?

  if (nsweep == 8) then
     xlabel = 'Big Q'
     do i = 1,isweep
        sweep(i) = 1.0D0/sweep(i) 
     end do
  end if

  write(nplot,902) xlabel,(sweep(i),i=1,isweep)
  write(nplot,903) (ifa(i),i=1,isweep)
  write(nplot,904) (sq(i),i=1,isweep)
  write(nplot,905) (ce(i),i=1,isweep)
  write(nplot,906) (cec(i),i=1,isweep)
  write(nplot,907) (cef(i),i=1,isweep)
  write(nplot,908) (ceo(i),i=1,isweep)
  write(nplot,909) (ccst(i),i=1,isweep)
  write(nplot,910) (crc(i),i=1,isweep)
  write(nplot,911) (cdt(i),i=1,isweep)
  write(nplot,912) (rmaj(i),i=1,isweep)
  write(nplot,913) (asp(i),i=1,isweep)
  write(nplot,914) (ip(i),i=1,isweep)
  write(nplot,915) (btor(i),i=1,isweep)
  write(nplot,916) (btt(i),i=1,isweep)
  write(nplot,917) (lq(i),i=1,isweep)
  write(nplot,918) (qlm(i),i=1,isweep)
  write(nplot,919) (bet(i),i=1,isweep)
  write(nplot,920) (blim(i),i=1,isweep)
  write(nplot,921) (epbp(i),i=1,isweep)
  write(nplot,922) (t10(i),i=1,isweep)
  write(nplot,923) (d20(i),i=1,isweep)
  write(nplot,924) (hfip(i),i=1,isweep)
  write(nplot,925) (hfio(i),i=1,isweep)
  write(nplot,926) (powf(i),i=1,isweep)
  write(nplot,927) (pbf(i),i=1,isweep)
  write(nplot,928) (wall(i),i=1,isweep)
  write(nplot,929) (pinj(i),i=1,isweep)
  write(nplot,930) (piwp(i),i=1,isweep)
  write(nplot,931) (pht(i),i=1,isweep)
  write(nplot,932) (curd(i),i=1,isweep)
  write(nplot,933) (bq(i),i=1,isweep)
  write(nplot,934) (bs(i),i=1,isweep)
  write(nplot,935) (eb(i),i=1,isweep)
  write(nplot,936) (hld(i),i=1,isweep)
  write(nplot,937) (tfp(i),i=1,isweep)
  write(nplot,938) (wtf(i),i=1,isweep)
  write(nplot,939) (str(i),i=1,isweep)
  write(nplot,940) (ocd(i),i=1,isweep)
  write(nplot,941) (tmx(i),i=1,isweep)
  write(nplot,942) (pcp(i),i=1,isweep)
  write(nplot,943) (fcl(i),i=1,isweep)
  write(nplot,944) (rcl(i),i=1,isweep)
  write(nplot,945) (vcl(i),i=1,isweep)
  write(nplot,946) (pmp(i),i=1,isweep)
  write(nplot,947) (pfp(i),i=1,isweep)
  write(nplot,948) (wpf(i),i=1,isweep)
  write(nplot,949) (pg(i),i=1,isweep)
  write(nplot,950) (pn(i),i=1,isweep)
  write(nplot,951) (rec1(i),i=1,isweep)

900 format(i8)
901 format(a25)
902 format(a25, 20e10.4)
903 format('Ifail                    ', 20e10.4)
904 format('Sqsumsq                  ', 20e10.4)
905 format('Electric cost (mil/kwh)  ', 20e10.4)
906 format('Capital cost (mil/kwh)   ', 20e10.4)
907 format('Fuel cost (mil/kwh)      ', 20e10.4)
908 format('Operations cost (mil/kwh)', 20e10.4)
909 format('Capital cost (mil)       ', 20e10.4)
910 format('Core costs (millions)    ', 20e10.4)
911 format('Direct cost (billions)   ', 20e10.4)
912 format('Major Radius (m)         ', 20e10.4)
913 format('Aspect Ratio             ', 20e10.4)
914 format('Plasma Current (MA)      ', 20e10.4)
915 format('B Toroidal Axis (T)      ', 20e10.4)
916 format('B total on axis (T)      ', 20e10.4)
917 format('Safety Factor            ', 20e10.4)
918 format('Should be zero...........', 20e10.4)
919 format('Beta                     ', 20e10.4)
920 format('Beta Limit               ', 20e10.4)
921 format('Epsilon Beta Poloidal    ', 20e10.4)
922 format('Average Temp x10 (KeV)   ', 20e10.4)
923 format('Average Dens (10^20/m^3) ', 20e10.4)
924 format('H-fact Iter Power        ', 20e10.4)
925 format('H-fact Iter Offset       ', 20e10.4)
926 format('Fusion Power (MW)        ', 20e10.4)
927 format('nb Fusion Power (MW)     ', 20e10.4)
928 format('Wall Load (MW/m^2)       ', 20e10.4)
929 format('Injection Power (MW)     ', 20e10.4)
930 format('Inject Pwr Wall Plug (MW)', 20e10.4)
931 format('Heating Power (MW)       ', 20e10.4)
932 format('Current Drive (MW)       ', 20e10.4)
933 format('Big Q                    ', 20e10.4)
934 format('Bootstrap Fraction       ', 20e10.4)
935 format('Neutral Beam Energy (MeV)', 20e10.4)
936 format('Divertor Heat (MW/m^2)   ', 20e10.4)
937 format('TF coil Power (MW)       ', 20e10.4)
938 format('TF coil weight (kg)      ', 20e10.4)
939 format('TF stress (MPa)          ', 20e10.4)
940 format('J   TF inner leg (MA/m^2)', 20e10.4)
941 format('Should be zero...........', 20e10.4)
942 format('Res TF inner leg Pwr (MW)', 20e10.4)
943 format('Coolant Fraction Ctr.    ', 20e10.4)
944 format('Should be zero...........', 20e10.4)
945 format('Should be zero...........', 20e10.4)
946 format('Should be zero...........', 20e10.4)
947 format('PF coil Power (MW)       ', 20e10.4)
948 format('PF coil weight (kg)      ', 20e10.4)
949 format('Gross Elect Pwr (MW)     ', 20e10.4)
950 format('Net electric Pwr (MW)    ', 20e10.4)
951 format('Recirculating Fraction   ', 20e10.4)

end subroutine scan

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine doopt(ifail)

  !+ad_name  doopt
  !+ad_summ  Routine to call the optimising equation solver
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : output integer : error flag
  !+ad_desc  This routine calls the optimising equation solver.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  numer.h90
  !+ad_call  labels.h90
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'
  include 'labels.h90'

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
  call optimiz(ifail,f)

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
     call verror(nout,iotty,ifail)
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine final(ifail)

  !+ad_name  final
  !+ad_summ  Routine to print out the final point in the scan
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : input integer : error flag
  !+ad_desc  This routine prints out the final point in the scan.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  numer.h90
  !+ad_call  oheadr
  !+ad_call  output
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'numer.h90'

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
