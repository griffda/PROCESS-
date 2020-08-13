#ifndef INSTALLDIR
#error INSTALLDIR not defined!
#endif

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
  use kit_hcpb_module, only: init_kit_hcpb_module
  use heat_transport_variables, only: init_heat_transport_variables
  use ife_variables, only: init_ife_variables
  use impurity_radiation_module, only: init_impurity_radiation_module
  use function_evaluator, only: init_function_evaluator
  use build_module, only: init_build_module
  use maths_library, only: init_maths_library
  use testdata, only: init_testdata
  use pfcoil_module, only: init_pfcoil_module
  use physics_module, only: init_physics_module
  use physics_variables, only: init_physics_variables
  use power_module, only: init_power_module
  use read_and_get_atomic_data, only: init_read_and_get_atomic_data
  use read_radiation, only: init_read_radiation
  use scan_module, only: init_scan_module
  use sctfcoil_module, only: init_sctfcoil_module
  use stellarator_module, only: init_stellarator_module
  use kit_blanket_model, only: init_kit_blanket_model
  use tfcoil_variables, only: init_tfcoil_variables
  use times_variables, only: init_times_variables
  use torga_curgap_module, only: init_torga_curgap_module
  use constants, only: init_constants
  use plasmod_variables, only: init_plasmod_variables
  use current_drive_variables, only: init_current_drive_variables
  use divertor_kallenbach_variables, only: init_divertor_kallenbach_variables
  use primary_pumping_variables, only: init_primary_pumping_variables
  use pfcoil_variables, only: init_pfcoil_variables
  use structure_variables, only: init_structure_variables
  use vacuum_variables, only: init_vacuum_variables
  use pf_power_variables, only: init_pf_power_variables
  use build_variables, only: init_build_variables
  use constraint_variables, only: init_constraint_variables
  use stellarator_variables, only: init_stellarator_variables
  use pulse_variables, only: init_pulse_variables
  use startup_variables, only: init_startup_variables
  use fispact_variables, only: init_fispact_variables
  use rebco_variables, only: init_rebco_variables
  use reinke_variables, only: init_reinke_variables
  use costs_module, only: init_costs_module
  use costs_2015_module, only: init_costs_2015
  use costs_step_module, only: init_costs_step
  use divertor_ode_var, only: init_divertor_ode_var
  use green_func_ext, only: init_green_func_ext
  use kit_hcll_module, only: init_kit_hcll_module
  use define_iteration_variables, only: init_define_iteration_variables

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
  call init_kit_hcpb_module
  call init_heat_transport_variables
  call init_ife_variables
  call init_impurity_radiation_module
  call init_function_evaluator
  call init_build_module
  call init_maths_library
  call init_testdata
  call init_pfcoil_module
  call init_physics_module
  call init_physics_variables
  call init_power_module
  call init_read_and_get_atomic_data
  call init_read_radiation
  call init_scan_module
  call init_sctfcoil_module
  call init_stellarator_module
  call init_kit_blanket_model
  call init_tfcoil_variables
  call init_times_variables
  call init_torga_curgap_module
  call init_constants
  call init_plasmod_variables
  call init_current_drive_variables
  call init_divertor_kallenbach_variables
  call init_primary_pumping_variables
  call init_pfcoil_variables
  call init_structure_variables
  call init_vacuum_variables
  call init_pf_power_variables
  call init_build_variables
  call init_constraint_variables
  call init_stellarator_variables
  call init_pulse_variables
  call init_startup_variables
  call init_fispact_variables
  call init_rebco_variables
  call init_reinke_variables
  call init_costs_module
  call init_costs_2015
  call init_costs_step
  call init_divertor_ode_var
  call init_green_func_ext
  call init_kit_hcll_module
  call init_define_iteration_variables
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
