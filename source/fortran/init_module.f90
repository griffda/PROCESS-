module init_module

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

contains

subroutine init_all_module_vars
  !! Initialise all module variables
  !! This is vital to ensure a 'clean' state of Process before a new run starts,
  !! otherwise components of the previous run's state can persist into the new
  !! run. This matters ever since Process is used as a shared library, rather
  !! than a 'run-once' executable.
  use numerics, only: init_numerics
  use process_input, only: init_input
  use buildings_variables, only: init_buildings_variables
  use cost_variables, only: init_cost_variables
  use div_kal_vars, only: init_div_kal_vars
  use divertor_ode, only: init_divertor_ode
  use divertor_variables, only: init_divertor_variables
  use error_handling, only: init_error_handling
  use fson_library, only: init_fson_library
  use fwbs_variables, only: init_fwbs_variables
  use global_variables, only: init_global_variables
  use ccfe_hcpb_module, only: init_ccfe_hcpb_module

  call init_numerics
  call init_input
  call init_buildings_variables
  call init_cost_variables
  call init_div_kal_vars
  call init_divertor_ode
  call init_divertor_variables
  call init_error_handling
  call init_fson_library
  call init_fwbs_variables
  call init_global_variables
  call init_ccfe_hcpb_module
end subroutine init_all_module_vars

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
  use stellarator_module, only: stinit
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

  !  Initialise stellarator parameters if necessary
  !  This overrides some of the bounds of the tokamak parameters
  call stinit

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

subroutine finish
  ! Originally at the end of the "program", this subroutine writes some final 
  ! lines via the output module and then closes any open files. This is 
  ! currently called from Python, and will be removed once file handling is 
  ! completely dealt with in Python
  ! # TODO Move this output and file handling to Python

  use process_input, only: nin
  use constants, only: iotty, mfile, nout, nplot, opt_file, vfile
  use process_output, only: oheadr
  use global_variables, only: verbose
  implicit none

  call oheadr(nout,'End of PROCESS Output')
  call oheadr(iotty,'End of PROCESS Output')
  call oheadr(nout,'Copy of PROCESS Input Follows')

  close(unit = nin)
  close(unit = nout)
  close(unit = nplot)
  close(unit = mfile)
  close(unit = opt_file)
  if (verbose == 1) close(unit = vfile)
end subroutine finish

end module init_module
