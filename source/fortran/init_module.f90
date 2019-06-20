module init_module

contains
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
  !+ad_call  initialise_error_listfile:///home/mkumar/process/source/fortran/scan.f90file:///home/mkumar/process/source/fortran/divertor_ode.f90


  !+ad_call  input
  !+ad_call  run_summary
  !+ad_hist  19/05/15 PJK Added ability to use a file prefix obtained
  !+ad_hisc               from a command line argument
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Codefile:///home/mkumar/process/source/fortran/divertor_ode.f90

  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!file:///home/mkumar/process/source/fortran/input.f90


  use error_handling
  use global_variables, only: verbose, fileprefix, output_prefix
  use impurity_radiation_module
  use numerics
  use process_input
  use process_output
  use main_module, only: run_summary
  implicit none

  !  Arguments

  !  Local variables
  integer :: i

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Initialise error handling
  call initialise_error_list

  !  Initialise the program variables
  call initial

  !  Open the input/output external files
  if (trim(fileprefix) == "") then
    open(unit=nin,file="IN.DAT",status='old')
  else
    open(unit=nin,file=trim(fileprefix),status='old')
  end if
  ! open(unit=nin,file=trim(fileprefix)//'IN.DAT',status='old')

  open(unit=nout     ,file=trim(output_prefix)//'OUT.DAT'   ,status='unknown')
  open(unit=nplot    ,file=trim(output_prefix)//'PLOT.DAT'  ,status='unknown')
  open(unit=mfile    ,file=trim(output_prefix)//'MFILE.DAT' ,status='unknown')
  open(unit=opt_file ,file=trim(output_prefix)//'OPT.DAT'   ,status='unknown')

  !  Input any desired new initial values
  call input

  !  Initialise impurity radiation data
  call initialise_imprad

  !  Check input data for errors/ambiguities
  call check

  !  Write to the output file certain relevant details about this run
  call run_summary

  !  Open verbose diagnostics file
  if (verbose == 1) then
     open(unit=vfile,file=trim(output_prefix)//'VFILE.DAT',status='unknown')
     write(vfile,'(a80)') 'nviter = number of VMCON iterations.'
     write(vfile,'(a80)') '(1-mod(ifail,7))=1 indicates that there has '// &
          'been an escape from a failed line search.'
     write(vfile,'(a80)') 'odd/even is a convenient plotting bit.'
     write(vfile,'(100a13)') 'nviter','escape', 'odd/even', 'te','coe','rmajor', &
          'powfmw','bt','tburn','sqsumsq', (lablxc(ixc(i)),i=1,nvar)
  end if

end subroutine init

end module init_module
