module global_variables

    !! Module containing miscellaneous global variables
    !! This module contains miscellaneous global variables not
    !! well-suited to any of the other 'variables' modules.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  
    use, intrinsic :: iso_fortran_env, only: dp=>real64
  
    implicit none
    
    public
  
    character(len=48) :: icase = 'Steady-state tokamak model'
    !! icase : power plant type
    character(len=180) :: runtitle = &
         "Run Title (change this line using input variable 'runtitle')"
         !! runtitle /Run Title/ : short descriptive title for the run
  
    integer :: verbose = 0
    !! verbose /0/ : switch for turning on/off diagnostic messages:<UL>
    !!           <LI> = 0 turn off diagnostics
    !!           <LI> = 1 turn on diagnostics</UL>
    integer :: run_tests = 0
    !! run_tests /0/ : Turns on built-in tests if set to 1
  
    integer :: maxcal = 200
    !! maxcal /200/ : maximum number of VMCON iterations
  
    character(len=200) :: fileprefix = "" !'dummy_file_prefix'
    character(len=200) :: output_prefix = "" ! output file prefix
    character(len=25) :: xlabel, vlabel
    character(len=25) :: xlabel_2, vlabel_2
    integer :: iscan_global=0    ! Makes iscan available globally.
    real(dp):: convergence_parameter  ! VMCON convergence parameter "sum"
  
  end module global_variables