 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program process
  !! Power Reactor Optimisation Code for Environmental and Safety Studies
  !! author: P J Knight, CCFE, Culham Science Centre
  !! author: J Morris, CCFE, Culham Science Centre
  !! None
  !! Power Reactor Optimisation Code for Environmental and Safety Studies
  !! <P>This is a systems code that evaluates various physics and
  !! engineering aspects of a fusion power plant subject to given
  !! constraints, and can optimise these parameters by minimising
  !! or maximising a function of them, such as the fusion power or
  !! cost of electricity.
  !! <P>This program is derived from the TETRA and STORAC codes produced by
  !! Oak Ridge National Laboratory, Tennessee, USA. The main authors in
  !! the USA were J.D.Galambos and P.C.Shipe.
  !! <P>The code was transferred to Culham Laboratory, Oxfordshire, UK, in
  !! April 1992, and the physics models were updated by P.J.Knight to
  !! include the findings of the Culham reactor studies documented in
  !! Culham Report AEA FUS 172 (1992). The standard of the Fortran has
  !! been thoroughly upgraded since that time, and a number of additional
  !! models have been added.
  !! <P>During 2012, PROCESS was upgraded from FORTRAN 77 to Fortran 95,
  !! to facilitate the restructuring of the code into proper modules
  !! (with all the benefits that modern software practices bring), and to
  !! aid the inclusion of more advanced physics and engineering models under
  !! development as part of a number of EFDA-sponsored collaborations.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !! Box file F/RS/CIRE5523/PWF (up to 15/01/96)
  !! Box file F/MI/PJK/PROCESS and F/PL/PJK/PROCESS (15/01/96 to 24/01/12)
  !! Box file T&amp;M/PKNIGHT/PROCESS (from 24/01/12)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use error_handling, only: show_errors
  use process_input, only: nin
  use scan_module, only: scan
  use main_module, only: runtests, eqslv
  use init_module, only: init
  use final_module, only: final
  use kallenbach_module, only: kallenbach_testing, kallenbach_scan
  use divertor_kallenbach_variables, only: kallenbach_scan_switch, &
    kallenbach_tests
  use constants, only: iotty, mfile, nout, nplot, opt_file, vfile
  use process_output, only: oblnkl, ostars, ocentr, osubhd, ocmmnt, oheadr
  use numerics, only: ioptimz
  use global_variables, only: output_prefix, fileprefix, run_tests, verbose
  
  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  !  Arguments

  !  Local variables
  integer :: ifail
  character(len = 130) :: line
  character(len = 10)  :: fmtAppend
  character(len = 200) :: inFile
  character(len = 200) :: outFile
  integer :: iost
  logical :: inExist
  integer :: nargs
  integer :: file_name_length
  integer, parameter :: width = 110

  !  Obtain a file prefix from a command line argument
  !  (uses Fortran 2003 routines)
  nargs = command_argument_count()

  if (nargs == 0) then
     fileprefix = ''
  else
     call get_command_argument(1, fileprefix)
  end if

  if (trim(fileprefix) == "help") then
    call oblnkl(iotty)
    call ostars(iotty, width)
    call ocentr(iotty,'PROCESS', width)
    call ocentr(iotty,'Power Reactor Optimisation Code', width)
    call ostars(iotty, width)

    ! Usage help
    call osubhd(iotty,'# Usage')
    call ocmmnt(iotty, "Running code with IN.DAT        : ./<path_to_executable/process.exe")
    call ocmmnt(iotty, "Running code with named IN.DAT  : ./<path_to_executable/process.exe <path_to_input>/<file_prefix>IN.DAT")
    call oblnkl(iotty)
    call ocmmnt(iotty, "Help info                       : ./<path_to_executable/process.exe help")
    call oblnkl(iotty)
    call ocmmnt(iotty, "## Example Usage")
    call oblnkl(iotty)
    call ocmmnt(iotty, "Executable in current dir and input called IN.DAT in current dir  : ./process.exe")
    call ocmmnt(iotty, "Executable in current dir and named input in current dir          : ./process.exe tokamak_IN.DAT")
    call ocmmnt(iotty, "Executable in other dir and named input in other dir              : ./bin/process.exe ../../ITER_IN.DAT")
    call ocmmnt(iotty, "Executable in other dir and input called IN.DAT in current dir    : ./bin/process.exe")

    ! Input help
    call osubhd(iotty,'# Input')
    call ocmmnt(iotty, "Input file naming convention : <file_prefix>IN.DAT")
    call oblnkl(iotty)
    call ocmmnt(iotty, "## Input file syntax")
    call oblnkl(iotty)
    call ocmmnt(iotty, "Constraint equation             : icc = <constraint_number>")
    call ocmmnt(iotty, "Iteration variable              : ixc = <iteration_variable_number>")
    call ocmmnt(iotty, "Iteration variable lower bound  : boundl(<iteration_variable_number>) = <bound_value>")
    call ocmmnt(iotty, "Iteration variable upper bound  : boundu(<iteration_variable_number>) = <bound_value>")
    call ocmmnt(iotty, "Parameter                       : <parameter_name> = <parameter_value>")
    call ocmmnt(iotty, "Array                           : <array_name>(<array_index>) = <index_value>")

    ! Output help
    call osubhd(iotty,'# Output')
    call ocmmnt(iotty, "Output files naming convention : <file_prefix>OUT.DAT")
    call ocmmnt(iotty, "                               : <file_prefix>MFILE.DAT")
    call ocmmnt(iotty, "                               : <file_prefix>PLOT.DAT")

    ! Contact info
    call osubhd(iotty,'# Contact')
    call ocmmnt(iotty, "James Morris  : james.morris2@ukaea.uk")
    call ocmmnt(iotty, "Hanni Lux     : hanni.lux@ukaea.uk")
    call ocmmnt(iotty, "GitLab        : git.ccfe.ac.uk")
    call oblnkl(iotty)
    stop
  else if (trim(fileprefix) == "") then
    inFile = "IN.DAT"
  else
    file_name_length = LEN_TRIM(fileprefix)
    output_prefix = fileprefix(1:file_name_length-6)
    inFile = trim(fileprefix)
  end if
  outFile = trim(output_prefix)//"OUT.DAT"
  inquire(file = inFile, exist = inExist)

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  mainRun : if (inExist) then

    !  Initialise
    call init    

    ! Run built-in tests.
    ! These are distinct from the tests that are dependent on 'unit_test'.
    if (run_tests == 1) call runtests

    if(kallenbach_tests == 1) then
      call kallenbach_testing()
      call exit(0)
    endif

    if(kallenbach_scan_switch == 1) then
      call kallenbach_scan()
      call exit(0)
    endif

     ! Call equation solver (HYBRD)
    call eqslv(ifail)

     ! Call routine to do scans
    if (ioptimz >= 0) then
       call scan
    else
       call final(ifail)
    end if

    call show_errors

    call oheadr(nout,'End of PROCESS Output')
    call oheadr(iotty,'End of PROCESS Output')
    call oheadr(nout,'Copy of PROCESS Input Follows')

    close(unit = nin)
    close(unit = nout)
    close(unit = nplot)
    close(unit = mfile)
    close(unit = opt_file)
    if (verbose == 1) close(unit = vfile)

    open(unit = 100, FILE = inFile)
    open(unit = 101, FILE = outFile, ACCESS = "append")
    open(unit = 102, FILE=trim(output_prefix)//'MFILE.DAT', ACCESS = "append")
    fmtAppend = '(A)'
    write(102, fmtAppend) "***********************************************"

    DO
      read(100, fmtAppend, IOSTAT = iost) line
      if(iost < 0) exit                   ! exit if End of line is reached in IN.DAT
      write(101, fmtAppend) trim(line)
      write(102, fmtAppend) trim(line)
    END DO
    close(unit = 100)
    close(unit = 101)
    close(unit = 102)

  else mainRun

    write(*, *) "There is no input file named"//inFile//" in the analysis folder"

  end if mainRun

end program process
