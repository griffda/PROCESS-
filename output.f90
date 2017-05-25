! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module process_output

  !+ad_name  process_output
  !+ad_summ  Module containing routines to produce a uniform output style
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  int2char
  !+ad_cont  int_to_string2
  !+ad_cont  int_to_string3
  !+ad_cont  oblnkl
  !+ad_cont  obuild
  !+ad_cont  ocentr
  !+ad_cont  ocmmnt
  !+ad_cont  ocosts
  !+ad_cont  oheadr
  !+ad_cont  oshead
  !+ad_cont  ostars
  !+ad_cont  osubhd
  !+ad_cont  ovarin
  !+ad_cont  ovarre
  !+ad_cont  ovarrf
  !+ad_cont  ovarst
  !+ad_cont  underscore
  !+ad_args  N/A
  !+ad_desc  This module contains a number of routines that allow the
  !+ad_desc  program to write output to a file unit in a uniform style.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  09/10/12 PJK Initial version of module
  !+ad_hist  29/11/12 PJK Added shared variable autodoc comments
  !+ad_hist  13/02/14 PJK Added mfile for machine-readable output file unit
  !+ad_hist  13/03/14 PJK Added int2char, int_string2, int_to_string3
  !+ad_hist  19/06/14 PJK Removed all sect?? switches
  !+ad_hist  10/09/14 PJK Added vfile for diagnostic output
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use global_variables
  use numerics
  implicit none

  public



contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ocentr(file,string,width)

    !+ad_name  ocentr
    !+ad_summ  Routine to print a centred header within a line of asterisks
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  string : input character string : Character string to be used
    !+ad_args  width : input integer : Total width of header
    !+ad_desc  This routine writes out a centred header within a line of asterisks.
    !+ad_desc  It cannot cope with a zero-length string; routine
    !+ad_desc  <A HREF="ostars.html"><CODE>ostars</CODE></A> should be used instead.
    !+ad_prob  None
    !+ad_call  ostars
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  15/05/14 PJK Increased max output width to 110 characters
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file, width
    character(len=*), intent(in) :: string

    !  Local variables

    integer :: lh, nstars, nstars2
    integer, parameter :: maxwidth = 110
    character(len=maxwidth) :: stars

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    stars = repeat('*',maxwidth)

    lh = len(string)

    if (lh == 0) then
       call ostars(file,width)
       return
    end if

    if (width > maxwidth) then
       write(*,*) 'Error in routine OCENTR :'
       write(*,*) 'Maximum width = ',maxwidth
       write(*,*) 'Requested width = ',width
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    if (lh >= width) then
       write(*,*) 'Error in routine OCENTR :'
       write(*,*) string
       write(*,*) 'This is too long to fit into ',width,' columns.'
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    !  Number of stars to be printed on the left

    nstars = int( (width-lh)/2 ) - 1

    !  Number of stars to be printed on the right

    nstars2 = width - (nstars+lh+2)

    !  Write the whole line

    write(file,'(t2,a)') stars(1:nstars)//' '//string//' '//stars(1:nstars2)

    write(mfile,'(t2,a)') '#'//' '//string//' '//'#'

  end subroutine ocentr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ostars(file,width)

    !+ad_name  ostars
    !+ad_summ  Routine to print a line of asterisks
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  width : input integer : Total width of header
    !+ad_desc  This routine writes out a line of asterisks.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  15/05/14 PJK Increased max output width to 110 characters
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file, width

    !  Local variables

    integer, parameter :: maxwidth = 110
    character(len=maxwidth) :: stars

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    stars = repeat('*',maxwidth)

    write(file,'(1x,a)') stars(1:min(width,maxwidth))

  end subroutine ostars

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine oheadr(file,string)

    !+ad_name  oheadr
    !+ad_summ  Routine to print a centred header within a line of asterisks,
    !+ad_summ  and between two blank lines
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  string : input character string : Character string to be used
    !+ad_desc  This routine writes out a centred header within a line of
    !+ad_desc  asterisks, and between two blank lines.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocentr
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  15/05/14 PJK Increased output width to 110 characters
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: string

    !  Local variables

    integer, parameter :: width = 110

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oblnkl(file)
    call ocentr(file,string,width)
    call oblnkl(file)

  end subroutine oheadr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine oshead(file,string)

    !+ad_name  oshead
    !+ad_summ  Routine to print a short, centred header within a line of asterisks,
    !+ad_summ  and between two blank lines
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  string : input character string : Character string to be used
    !+ad_desc  This routine writes out a short, centred header within a line of
    !+ad_desc  asterisks, and between two blank lines.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocentr
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  15/05/14 PJK Increased output width to 80 characters
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: string

    !  Local variables

    integer, parameter :: width = 80

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oblnkl(file)
    call ocentr(file,string,width)
    call oblnkl(file)

  end subroutine oshead

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine oblnkl(file)

    !+ad_name  oblnkl
    !+ad_summ  Routine to print a blank line
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_desc  This routine writes out a simple blank line.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    write(file,10)
10  format(' ')

  end subroutine oblnkl

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine osubhd(file,string)

    !+ad_name  osubhd
    !+ad_summ  Routine to print a subheading between two blank lines
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  string : input character string : Character string to be used
    !+ad_desc  This routine writes out a subheading between two blank lines.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: string

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oblnkl(file)
    call ocmmnt(file,string)
    call oblnkl(file)

  end subroutine osubhd

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ocmmnt(file,string)

    !+ad_name  ocmmnt
    !+ad_summ  Routine to print a comment
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  string : input character string : Character string to be used
    !+ad_desc  This routine writes out a comment line.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  15/05/14 PJK Increased output width to 110 characters
    !+ad_hist  23/07/14 PJK Trimmed off trailing spaces
    !+ad_hist  05/08/15 MDK Remove "stop" command when the comment is too long.
    !+ad_hist  28/10/16 MK Modified previous output to reflect warning message.
    !+ad_hist              Removed variable (dummy) and use "string" to print.
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: string

    !  Local variables

    integer, parameter :: maxwidth = 110
    integer :: lh
!    character(len = maxwidth) :: dummy

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    lh = len(trim(string))

    if (lh == 0) then
       write(*,*) 'Error in routine OCMMNT :'
       write(*,*) 'A zero-length string is not permitted.'
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    if (lh >= maxwidth) then
       write(*, *) 'Warning in routine OCMMNT :'
       write(*, '(A)') string
!       write(*,*) 'This is too long to fit into ',maxwidth,' columns.'
       write(*, '(A,i3,A)') 'This is longer than ',maxwidth,' columns.'  ! MK 28/10/2016 Modified previous output to reflect warning message
       !write(*,*) 'PROCESS stopping.'
       !stop
    end if
!    dummy = trim(string)
    write(file,'(t2,a)') trim(string)
    !write(file,'(t2,a)') dummy
  end subroutine ocmmnt

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ovarrf(file,descr,varnam,value,output_flag)

    !+ad_name  ovarrf
    !+ad_summ  Routine to print out the details of a floating-point
    !+ad_summ  variable using 'F' format
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  descr : input character string : Description of the variable
    !+ad_args  varnam : input character string : Name of the variable
    !+ad_args  value : input real : Value of the variable
    !+ad_args  output_flag : optional character
    !+ad_desc  This routine writes out the description, name and value of a
    !+ad_desc  double precision variable in F format (e.g.
    !+ad_desc  <CODE>-12345.000</CODE>).
    !+ad_prob  None
    !+ad_call  ovarre
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  13/02/14 PJK Added output to mfile, with underscores replacing spaces
    !+ad_hist  17/02/14 PJK Ensured mfile output is not replicated if file=mfile
    !+ad_hist  06/03/14 PJK mfile output now sent to ovarre for 'E' format
    !+ad_hist  15/05/14 PJK Longer line length
    !+ad_hist  05/08/15 MDK Optional output flag
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: descr, varnam
    real(kind(1.0D0)), intent(in) :: value
    character(len=3), intent(in), optional :: output_flag

    !  Local variables

    character(len=72) :: dum72
    character(len=20) :: dum20
    character(len=20) :: stripped
    character(len=3) :: flag
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr and varnam with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum72 = descr
    dum20 = varnam
    stripped = varnam(2:len(varnam)-1)

    if (present(output_flag)) then
        flag = output_flag
    else
        flag = ''
    end if

    if (file /= mfile) then
       !MDK add label if it is an iteration variable
       ! The ITV flag overwrites the output_flag
       if (any(name_xc == stripped))  flag = 'ITV'
       if (verbose==1) then
            write(file,10) dum72, dum20, value, flag
       else
            write(file,20) dum72, dum20, value, flag
       end if
    end if

10  format(1x,a,t75,a,t100,f13.6, t115, a)
20  format(1x,a,t75,a,t100,f10.3, t112, a)

    call ovarre(mfile,descr,varnam,value)

  end subroutine ovarrf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ovarre(file,descr,varnam,value,output_flag)

    !+ad_name  ovarre
    !+ad_summ  Routine to print out the details of a floating-point
    !+ad_summ  variable using 'E' format
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  descr : input character string : Description of the variable
    !+ad_args  varnam : input character string : Name of the variable
    !+ad_args  value : input real : Value of the variable
    !+ad_args  output_flag : optional character
    !+ad_desc  This routine writes out the description, name and value of a
    !+ad_desc  double precision variable in E format (e.g.
    !+ad_desc  <CODE>-1.234E+04</CODE>).
    !+ad_prob  None
    !+ad_call  underscore
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  13/02/14 PJK Added output to mfile, with underscores replacing spaces
    !+ad_hist  17/02/14 PJK Ensured mfile output is not replicated if file=mfile
    !+ad_hist  15/05/14 PJK Longer line length
    !+ad_hist  31/07/15 MDK Add label if it is an iteration variable
    !+ad_hist  05/08/15 MDK Optional output flag
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: descr, varnam
    real(kind(1.0D0)), intent(in) :: value
    character(len=3), intent(in), optional :: output_flag

    !  Local variables

    character(len=72) :: dum72
    character(len=20) :: dum20
    character(len=20) :: stripped
    character(len=3) :: flag

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr and varnam with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum72 = descr
    dum20 = varnam
    ! Remove the "(" and ")" from the varnam
    stripped = varnam(2:len(varnam)-1)
    if (present(output_flag)) then
        flag = output_flag
    else
        flag = ''
    end if

    if (file /= mfile) then
       ! MDK add ITV label if it is an iteration variable
       ! The ITV flag overwrites the output_flag
       if (any(name_xc == stripped))  flag = 'ITV'
       write(file,20) dum72, dum20, value, flag
    end if

    call underscore(dum72)
    call underscore(dum20)
    write(mfile,10) dum72, dum20, value, flag

10  format(1x,a,t75,a,t100,1pe10.3," ",a,t10)
20  format(1x,a,t75,a,t100,1pe10.3, t112, a)

  end subroutine ovarre

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ovarin(file,descr,varnam,value,output_flag)

    !+ad_name  ovarin
    !+ad_summ  Routine to print out the details of an integer variable
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  descr : input character string : Description of the variable
    !+ad_args  varnam : input character string : Name of the variable
    !+ad_args  value : input integer : Value of the variable
    !+ad_args  output_flag : optional character
    !+ad_desc  This routine writes out the description, name and value of an
    !+ad_desc  integer variable.
    !+ad_prob  None
    !+ad_call  underscore
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  13/02/14 PJK Added output to mfile, with underscores replacing spaces
    !+ad_hist  17/02/14 PJK Ensured mfile output is not replicated if file=mfile
    !+ad_hist  15/05/14 PJK Longer line length
    !+ad_hist  05/08/15 MDK Optional output flag
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: descr, varnam
    integer, intent(in) :: value
    character(len=3), intent(in), optional :: output_flag

    !  Local variables

    character(len=72) :: dum72
    character(len=20) :: dum20
    ! character(len=15) :: dum15
    character(len=20) :: stripped
    character(len=3) :: flag

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr and varnam with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum72 = descr
    dum20 = varnam
    stripped = varnam(2:len(varnam)-1)
    if (present(output_flag)) then
        flag = output_flag
    else
        flag = ''
    end if

    if (file /= mfile) then
       ! MDK add ITV label if it is an iteration variable
       ! The ITV flag overwrites the output_flag
       if (any(name_xc == stripped))  flag = 'ITV'
       write(file,20) dum72, dum20, value, flag
    end if

    call underscore(dum72)
    call underscore(dum20)
    write(mfile,10) dum72, dum20, value, flag

10  format(1x,a,t75,a,t100,i10," ",a,t10)
20  format(1x,a,t75,a,t100,i10,t112, a)

  end subroutine ovarin

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ovarst(file,descr,varnam,value)

    !+ad_name  ovarst
    !+ad_summ  Routine to print out the details of a character variable
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  descr : input character string : Description of the variable
    !+ad_args  varnam : input character string : Name of the variable
    !+ad_args  value : input character string : Value of the variable
    !+ad_desc  This routine writes out the description, name and value of a
    !+ad_desc  character string variable.
    !+ad_prob  None
    !+ad_call  underscore
    !+ad_hist  03/04/14 PJK Initial version
    !+ad_hist  15/05/14 PJK Longer line length
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: descr, varnam, value

    !  Local variables

    character(len=72) :: dum72
    character(len=20) :: dum20

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr and varnam with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum72 = descr
    dum20 = varnam

    if (file /= mfile) then
       write(file,10) dum72, dum20, value
    end if

    call underscore(dum72)
    call underscore(dum20)
    write(mfile,10) dum72, dum20, value

10  format(1x,a,t75,a,t100,a)

  end subroutine ovarst

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ocosts(file,ccode,descr,value)

    !+ad_name  ocosts
    !+ad_summ  Routine to print out the code, description and value
    !+ad_summ  of a cost item
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  ccode : input character string : Code number/name of the cost item
    !+ad_args  descr : input character string : Description of the cost item
    !+ad_args  value : input real : Value of the cost item
    !+ad_desc  This routine writes out the cost code, description and value
    !+ad_desc  of a cost item.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  15/05/14 PJK Longer line length
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: ccode, descr
    real(kind(1.0D0)), intent(in) :: value

    !  Local variables

    character(len=20) :: dum20
    character(len=72) :: dum72

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace ccode and descr with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum20 = ccode
    dum72 = descr

    write(file,10) dum20, dum72, value
10  format(1x,a,t22,a,t100,f10.2)

    call ovarrf(mfile,descr,ccode,value)

  end subroutine ocosts

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine obuild(file,descr,thick,total,variable_name)

    !+ad_name  obuild
    !+ad_summ  Routine to print out a description, the thickness and
    !+ad_summ  summed build of a component of the radial or vertical build
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  descr : input character string : Description of the component
    !+ad_args  thick : input real : Thickness of the component (m)
    !+ad_args  total : input real : Total build, including this component (m)
    !+ad_desc  This routine writes out a description, the thickness and
    !+ad_desc  summed build of a component of the radial or vertical build.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: descr
    character(len=*), optional :: variable_name
    real(kind(1.0D0)), intent(in) :: thick, total

    !  Local variables

    character(len=30) :: dum30
    character(len=20) :: dum20

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr with dummy string of the correct length.
    !  This counters problems that would occur if the original string
    !  was the wrong length.

    dum30 = descr
    if (present(variable_name)) then
        dum20 = variable_name
    else
        dum20 = ''
    end if

    write(file,10) dum30, thick, total, dum20
10  format(1x,a,t42,f10.3,t58,f10.3,t71,a)

  end subroutine obuild

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine underscore(string)

    !+ad_name  underscore
    !+ad_summ  Routine that converts spaces in a string to underscores
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  string : input/output string : character string of interest
    !+ad_desc  This routine converts any space characters in the string
    !+ad_desc  to underscore characters.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  13/02/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(inout) :: string

    !  Local variables

    integer :: loop, i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    i = index(string, ' ')
    if (i > 0) then
       do loop = i, len(string)
          if (string(loop:loop) == ' ') string(loop:loop) = '_'
       end do
    end if

  end subroutine underscore

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function int2char(i)

    !+ad_name  int2char
    !+ad_summ  Converts a single-digit integer into a character string
    !+ad_type  Function returning a single character string
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  i : input integer : must be between 0 and 9
    !+ad_desc  This is a very simple routine that converts a single-digit
    !+ad_desc  integer into a character string. If the integer is outside
    !+ad_desc  the range 0 to 9 the program stops with an error.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  14/06/01 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    character(len=1) :: int2char

    !  Arguments

    integer, intent(in) :: i

    !  Local variables

    character(len=10) :: number = '0123456789'

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if ((i < 0).or.(i > 9)) then
       write(*,*) 'INT2CHAR: illegal argument'
       stop
    end if

    int2char = number(i+1:i+1)

  end function int2char

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function int_to_string2(i)

    !+ad_name  int_to_string2
    !+ad_summ  Converts a positive integer into a two-digit character string
    !+ad_type  Function returning a two-digit character string
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  i : input integer : must be between 0 and 99
    !+ad_desc  This routine converts a positive integer into a two-digit
    !+ad_desc  character string.
    !+ad_desc  If the integer is negative, the routine stops with an error.
    !+ad_desc  If the integer is greater than 99, the routine returns a
    !+ad_desc  string containing its last two digits.
    !+ad_prob  None
    !+ad_call  int2char
    !+ad_hist  13/03/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    character(len=2) :: int_to_string2

    !  Arguments

    integer, intent(in) :: i

    !  Local variables

    character(len=1) :: a0, a1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (i < 0) then
       write(*,*) 'INT_TO_STRING2: illegal argument'
       stop
    end if

    a0 = int2char(mod(i,10))
    a1 = int2char(mod(int(i/10),10))

    int_to_string2 = a1//a0

  end function int_to_string2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function int_to_string3(i)

    !+ad_name  int_to_string3
    !+ad_summ  Converts a positive integer into a 3-digit character string
    !+ad_type  Function returning a three-digit character string
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  i : input integer : must be between 0 and 99
    !+ad_desc  This routine converts a positive integer into a three-digit
    !+ad_desc  character string.
    !+ad_desc  If the integer is negative, the routine stops with an error.
    !+ad_desc  If the integer is greater than 999, the routine returns a
    !+ad_desc  string containing its last three digits.
    !+ad_prob  None
    !+ad_call  int2char
    !+ad_hist  13/03/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    character(len=3) :: int_to_string3

    !  Arguments

    integer, intent(in) :: i

    !  Local variables

    character(len=1) :: a0, a1, a2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (i < 0) then
       write(*,*) 'INT_TO_STRING3: illegal argument'
       stop
    end if

    a0 = int2char(mod(i,10))
    a1 = int2char(mod(int(i/10),10))
    a2 = int2char(mod(int(i/100),100))

    int_to_string3 = a2//a1//a0

  end function int_to_string3

end module process_output
