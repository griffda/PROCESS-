module global_variables
  !! author: J. Morris (UKAEA)
  !!
  !! This module contains miscellaneous global variables not well-suited to any 
  !! of the other 'variables' modules.
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none
  
  public

  character(len=48) :: icase = 'Steady-state tokamak model'
  !! power plant type

  character(len=180) :: runtitle = "Run Title (change this line using input variable 'runtitle')"
  !! short descriptive title for the run

  integer :: verbose = 0
  !! switch for turning on/off diagnostic messages
  !!
  !! - =0 turn off diagnostics
  !! - =1 turn on diagnostics

  integer :: run_tests = 0
  !! turns on built-in tests if set to 1

  integer :: maxcal = 200
  !! maximum number of VMCON iterations

  character(len=200) :: fileprefix = "" 
  !! input file prefix

  character(len=200) :: output_prefix = ""
  !! output file prefix

  character(len=25) :: xlabel
  !! scan parameter description label

  character(len=25) :: vlabel
  !! scan value name label
  
  character(len=25) :: xlabel_2
  !! scan parameter description label (2nd dimension)

  character(len=25) :: vlabel_2
  !! scan value name label (2nd dimension)

  integer :: iscan_global=0
  !! Makes iscan available globally.

  real(dp):: convergence_parameter
  !! VMCON convergence parameter "sum"

end module global_variables