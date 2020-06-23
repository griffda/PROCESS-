module process_module
  implicit none
contains
  subroutine process_subroutine
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
  
    use error_handling, only: show_errors
    use process_input, only: nin
    use kallenbach_module, only: kallenbach_testing, kallenbach_scan
    use constants, only: iotty, mfile, nout, nplot, opt_file, vfile
    use process_output, only: oblnkl, ostars, ocentr, osubhd, ocmmnt, oheadr
    use global_variables, only: output_prefix, verbose
    
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none
  
    !  Arguments
  
    !  Local variables
    character(len = 130) :: line
    character(len = 10)  :: fmtAppend
    character(len = 200) :: inFile
    character(len = 200) :: outFile
    integer :: iost
    integer, parameter :: width = 110
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
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

    ! Set output filename
    outFile = trim(output_prefix)//"OUT.DAT"

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
  end subroutine process_subroutine
end module process_module