!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module process_output

  !+ad_name  process_output
  !+ad_summ  Module containing routines to produce a uniform output style
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
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
  !+ad_args  N/A
  !+ad_desc  This module contains a number of routines that allow the
  !+ad_desc  program to write output to a file unit in a uniform style.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  09/10/12 PJK Initial version of module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  integer, parameter :: nout = 12  !  Output file unit identifier
  integer, parameter :: nplot = 11 !  Plot data file unit identifier
  integer, parameter :: iotty = 6  !  Standard output unit identifier

  !  Switches for turning on/off output sections
  !  1 = on, 0 = off

  integer :: sect01 = 1  !  Power Reactor Costs
  integer :: sect02 = 1  !  Detailed Costings
  integer :: sect03 = 1  !  Plasma
  integer :: sect04 = 1  !  Current Drive System
  integer :: sect05 = 1  !  Divertor
  integer :: sect06 = 1  !  Machine Build
  integer :: sect07 = 1  !  TF Coils
  integer :: sect08 = 1  !  PF Coils
  integer :: sect09 = 1  !  Volt Second Consumption
  integer :: sect10 = 1  !  Support Structure
  integer :: sect11 = 1  !  PF Coil Inductances
  integer :: sect12 = 1  !  Shield / Blanket
  integer :: sect13 = 1  !  Power Conversion
  integer :: sect14 = 1  !  Power / Heat Transport
  integer :: sect15 = 1  !  Vacuum System
  integer :: sect16 = 1  !  Plant Buildings System
  integer :: sect17 = 1  !  AC Power
  integer :: sect18 = 1  !  Neutral Beams
  integer :: sect19 = 1  !  Electron Cyclotron Heating
  integer :: sect20 = 1  !  Lower Hybrid Heating
  integer :: sect21 = 1  !  Times

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
    !+ad_call  None
    !+ad_hist  20/09/11 PJK Initial F90 version
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
    integer, parameter :: maxwd = 100
    character(len=maxwd) :: stars

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    stars = repeat('*',maxwd)

    lh = len(string)

    if (width > maxwd) then
       write(*,*) 'Error in routine OCENTR :'
       write(*,*) 'Maximum width = ',maxwd
       write(*,*) 'Requested width = ',width
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    if (lh == 0) then
       write(*,*) 'Error in routine OCENTR :'
       write(*,*) 'A zero-length string is not permitted.'
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

    write(file,*) stars(1:nstars),' ',string,' ',stars(1:nstars2)

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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file, width

    !  Local variables

    integer, parameter :: maxwd = 100
    character(len=maxwd) :: stars

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    stars = repeat('*',maxwd)

    write(file,'(t2,a)') stars(1:min(width,maxwd))

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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: string

    !  Local variables

    integer, parameter :: width = 72

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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: string

    !  Local variables

    integer, parameter :: width = 50

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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: file
    character(len=*), intent(in) :: string

    !  Local variables

    integer, parameter :: width = 72
    integer :: lh

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    lh = len(string)

    if (lh == 0) then
       write(*,*) 'Error in routine OCMMNT :'
       write(*,*) 'A zero-length string is not permitted.'
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    if (lh >= width) then
       write(*,*) 'Error in routine OCMMNT :'
       write(*,*) string
       write(*,*) 'This is too long to fit into ',width,' columns.'
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    write(file,'(t2,a)') string

  end subroutine ocmmnt

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ovarrf(file,descr,varnam,value)

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
    !+ad_desc  This routine writes out the description, name and value of a
    !+ad_desc  double precision variable in F format (e.g.
    !+ad_desc  <CODE>-12345.000</CODE>).
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
    character(len=*), intent(in) :: descr, varnam
    real(kind(1.0D0)), intent(in) :: value

    !  Local variables

    character(len=42) :: dum42
    character(len=13) :: dum13

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr and varnam with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum42 = descr
    dum13 = varnam

    write(file,10) dum42, dum13, value
10  format(1x,a,t45,a,t60,f10.3)

  end subroutine ovarrf

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ovarre(file,descr,varnam,value)

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
    !+ad_desc  This routine writes out the description, name and value of a
    !+ad_desc  double precision variable in E format (e.g.
    !+ad_desc  <CODE>-1.234E+04</CODE>).
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
    character(len=*), intent(in) :: descr, varnam
    real(kind(1.0D0)), intent(in) :: value

    !  Local variables

    character(len=42) :: dum42
    character(len=13) :: dum13

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr and varnam with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum42 = descr
    dum13 = varnam

    write(file,10) dum42, dum13, value
10  format(1x,a,t45,a,t60,1pe10.3)

  end subroutine ovarre

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ovarin(file,descr,varnam,value)

    !+ad_name  ovarin
    !+ad_summ  Routine to print out the details of an integer variable
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  file : input integer : Fortran output unit identifier
    !+ad_args  descr : input character string : Description of the variable
    !+ad_args  varnam : input character string : Name of the variable
    !+ad_args  value : input integer : Value of the variable
    !+ad_desc  This routine writes out the description, name and value of an
    !+ad_desc  integer variable.
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
    character(len=*), intent(in) :: descr, varnam
    integer, intent(in) :: value

    !  Local variables

    character(len=42) :: dum42
    character(len=13) :: dum13

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr and varnam with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum42 = descr
    dum13 = varnam

    write(file,10) dum42, dum13, value
10  format(1x,a,t45,a,t60,i10)

  end subroutine ovarin

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

    character(len=8)  :: dum08
    character(len=42) :: dum42

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace ccode and descr with dummy strings of the correct length.
    !  This counters problems that would occur if the two original strings
    !  were the wrong length.

    dum08 = ccode
    dum42 = descr

    write(file,10) dum08, dum42, value
10  format(1x,a,t10,a,t60,f10.2)

  end subroutine ocosts

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine obuild(file,descr,thick,total)

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
    real(kind(1.0D0)), intent(in) :: thick, total

    !  Local variables

    character(len=30) :: dum30

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Replace descr with dummy string of the correct length.
    !  This counters problems that would occur if the original string
    !  was the wrong length.

    dum30 = descr

    write(file,10) dum30, thick, total
10  format(1x,a,t42,f10.3,t58,f10.3)

  end subroutine obuild

end module process_output
