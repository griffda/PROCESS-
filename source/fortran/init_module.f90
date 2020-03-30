module init_module

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

contains

subroutine init

  !! Routine that calls the initialisation routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! None
  !! This routine calls the main initialisation routines that set
  !! the default values for the global variables, reads in data from
  !! the input file, and checks the run parameters for consistency.


  !! AEA FUS 251: A User's Guide to the PROCESS Systems Codefile:///home/mkumar/process/source/fortran/divertor_ode.f90

  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!file:///home/mkumar/process/source/fortran/input.f90

  use global_variables, only: verbose, fileprefix, output_prefix
  use main_module, only: run_summary
  use constants, only: opt_file, vfile, nout, nplot, mfile, sig_file
  use error_handling, only: initialise_error_list 
  use impurity_radiation_module, only: initialise_imprad 
  use numerics, only: ixc , lablxc, nvar
  use process_input, only: nin, input
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
  open(unit=sig_file ,file=trim(output_prefix)//'SIG_TF.DAT',status='unknown')

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
