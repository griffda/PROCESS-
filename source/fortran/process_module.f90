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
    use global_variables, only: verbose
    
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none
  
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
  end subroutine process_subroutine
end module process_module