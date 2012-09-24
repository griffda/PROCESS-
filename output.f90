!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine output(nout)

  !+ad_name  output
  !+ad_summ  Subroutine to write the results to the main output file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
  !+ad_desc  This routine writes the program results to a file,
  !+ad_desc  in a tidy format.
  !+ad_prob  None
  !+ad_call  param.h90
  !+ad_call  phydat.h90
  !+ad_call  stella.h90
  !+ad_call  rfp.h90
  !+ad_call  ife.h90
  !+ad_call  acpow
  !+ad_call  avail
  !+ad_call  bldgcall
  !+ad_call  cntrpst
  !+ad_call  costs
  !+ad_call  cudriv
  !+ad_call  divcall
  !+ad_call  ech
  !+ad_call  fispac
  !+ad_call  fwbs
  !+ad_call  ifeout
  !+ad_call  igmarcal
  !+ad_call  induct
  !+ad_call  loca
  !+ad_call  lwhymod
  !+ad_call  nbeam
  !+ad_call  outpf
  !+ad_call  outplas
  !+ad_call  outtim
  !+ad_call  outvolt
  !+ad_call  pfpwr
  !+ad_call  power2
  !+ad_call  pulse
  !+ad_call  radialb
  !+ad_call  rfppfc
  !+ad_call  rfppfp
  !+ad_call  rfptfc
  !+ad_call  stout
  !+ad_call  strucall
  !+ad_call  tfcoil
  !+ad_call  tfpwr
  !+ad_call  tfspcall
  !+ad_call  vaccall
  !+ad_hist  23/01/97 PJK Initial upgraded version. Split routine POWER
  !+ad_hisc               into POWER1 and POWER2
  !+ad_hist  06/02/97 PJK Added routine LOCA
  !+ad_hist  21/03/97 PJK Added routine IFEOUT
  !+ad_hist  18/11/97 PJK Removed NOUT argument from FISPAC call
  !+ad_hist  19/05/99 PJK Added routine AVAIL
  !+ad_hist  20/09/11 PJK Initial F90 version
  !+ad_hist  24/09/12 PJK Swapped argument order of RADIALB, DIVCALL, INDUCT
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'param.h90'
  include 'phydat.h90'
  include 'stella.h90'
  include 'rfp.h90'
  include 'ife.h90'

  !  Arguments

  integer, intent(in) :: nout

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Call stellarator output routine instead if relevant

  if (istell /= 0) then
     call stout(nout)
     return
  end if

  !  Call inertial fusion energy output routine instead if relevant

  if (ife /= 0) then
     call ifeout(nout)
     return
  end if

  call costs(nout,1)
  call avail(nout,1)
  call outplas(nout)
  call igmarcal(nout)
  call cudriv(nout,1)
  call pulse(nout,1)
  call outtim(nout)
  call divcall(nout,1)
  call radialb(nout,1)

  if (irfp == 0) then
     call tfcoil(nout,1)
  else
     call rfptfc(nout,1)
  end if

  call tfspcall(nout,1)

  if (itart == 1) call cntrpst(nout,1)

  if (irfp == 0) then
     call outpf(nout)
  else
     call rfppfc(nout,1)
  end if

  if (irfp == 0) call outvolt(nout)

  call strucall(nout,1)

  if (irfp == 0) call induct(nout,1)

  call fwbs(nout,1)

  if (ifispact == 1) then
     call fispac(0)
     call fispac(1)
     call loca(nout,0)
     call loca(nout,1)
  end if

  call tfpwr(nout,1)

  if (irfp == 0) then
     call pfpwr(nout,1)
  else
     call rfppfp(nout,1)
  end if

  call vaccall(nout,1)
  call bldgcall(nout,1)
  call acpow(nout,1)
  call power2(nout,1)
  call nbeam(nout,1)
  call ech(nout,1)
  call lwhymod(nout,1)

end subroutine output

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ocentr(nout,string,width)

  !+ad_name  ocentr
  !+ad_summ  Routine to print a centred header within a line of asterisks
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout, width
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

  write(nout,*) stars(1:nstars),' ',string,' ',stars(1:nstars2)

end subroutine ocentr

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ostars(nout,width)

  !+ad_name  ostars
  !+ad_summ  Routine to print a line of asterisks
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout, width

  !  Local variables

  integer, parameter :: maxwd = 100
  character(len=maxwd) :: stars

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  stars = repeat('*',maxwd)

  write(nout,'(t2,a)') stars(1:min(width,maxwd))

end subroutine ostars

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine oheadr(nout,string)

  !+ad_name  oheadr
  !+ad_summ  Routine to print a centred header within a line of asterisks,
  !+ad_summ  and between two blank lines
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
  character(len=*), intent(in) :: string

  !  Local variables

  integer, parameter :: width = 72

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call oblnkl(nout)
  call ocentr(nout,string,width)
  call oblnkl(nout)

end subroutine oheadr

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine oshead(nout,string)

  !+ad_name  oshead
  !+ad_summ  Routine to print a short, centred header within a line of asterisks,
  !+ad_summ  and between two blank lines
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
  character(len=*), intent(in) :: string

  !  Local variables

  integer, parameter :: width = 50

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call oblnkl(nout)
  call ocentr(nout,string,width)
  call oblnkl(nout)

end subroutine oshead

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine oblnkl(nout)

  !+ad_name  oblnkl
  !+ad_summ  Routine to print a blank line
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  write(nout,10)
10 format(' ')

end subroutine oblnkl

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine osubhd(nout,string)

  !+ad_name  osubhd
  !+ad_summ  Routine to print a subheading between two blank lines
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
  character(len=*), intent(in) :: string

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call oblnkl(nout)
  call ocmmnt(nout,string)
  call oblnkl(nout)

end subroutine osubhd

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ocmmnt(nout,string)

  !+ad_name  ocmmnt
  !+ad_summ  Routine to print a comment
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
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

  write(nout,'(t2,a)') string

end subroutine ocmmnt

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ovarrf(nout,descr,varnam,value)

  !+ad_name  ovarrf
  !+ad_summ  Routine to print out the details of a floating-point
  !+ad_summ  variable using 'F' format
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
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

  write(nout,10) dum42, dum13, value
10 format(1x,a,t45,a,t60,f10.3)

end subroutine ovarrf

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ovarre(nout,descr,varnam,value)

  !+ad_name  ovarre
  !+ad_summ  Routine to print out the details of a floating-point
  !+ad_summ  variable using 'E' format
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
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

  write(nout,10) dum42, dum13, value
10 format(1x,a,t45,a,t60,1pe10.3)

end subroutine ovarre

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ovarin(nout,descr,varnam,value)

  !+ad_name  ovarin
  !+ad_summ  Routine to print out the details of an integer variable
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
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

  write(nout,10) dum42, dum13, value
10 format(1x,a,t45,a,t60,i10)

end subroutine ovarin

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine ocosts(nout,ccode,descr,value)

  !+ad_name  ocosts
  !+ad_summ  Routine to print out the code, description and value
  !+ad_summ  of a cost item
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
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

  write(nout,10) dum08, dum42, value
10 format(1x,a,t10,a,t60,f10.2)

end subroutine ocosts

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine obuild(nout,descr,thick,total)

  !+ad_name  obuild
  !+ad_summ  Routine to print out a description, the thickness and
  !+ad_summ  summed build of a component of the radial or vertical build
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
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

  integer, intent(in) :: nout
  character(len=*), intent(in) :: descr
  real(kind(1.0D0)), intent(in) :: thick, total

  !  Local variables

  character(len=30) :: dum30

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Replace descr with dummy string of the correct length.
  !  This counters problems that would occur if the original string
  !  was the wrong length.

  dum30 = descr

  write(nout,10) dum30, thick, total
10 format(1x,a,t42,f10.3,t58,f10.3)

end subroutine obuild

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine outtim(nout)

  !+ad_name  outtim
  !+ad_summ  Routine to print out the times of the various stages
  !+ad_summ  during a single plant cycle
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  nout : input integer : Fortran output unit identifier
  !+ad_desc  This routine writes out the times of the various stages
  !+ad_desc  during a single plant cycle.
  !+ad_prob  None
  !+ad_call  times.h90
  !+ad_call  osections.h90
  !+ad_call  oblnkl
  !+ad_call  oheadr
  !+ad_call  ovarrf
  !+ad_hist  20/09/11 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'times.h90'
  include 'osections.h90'

  !  Arguments

  integer, intent(in) :: nout

  !  Local variables

  real(kind(1.0D0)) :: tcycle

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (sect21 == 0) return

  tcycle = tramp + tohs + theat + tburn + tqnch + tdwell

  call oheadr(nout,'Times')

  call ovarrf(nout,'Initial charge time for PF coils (s)','(tramp)', &
       tramp)
  call ovarrf(nout,'OH coil swing time (s)','(tohs)',tohs)
  call ovarrf(nout,'Heating time (s)','(theat)',theat)
  call ovarre(nout,'Burn time (s)','(tburn)',tburn)
  call ovarrf(nout,'Shutdown time for PF coils (s)','(tqnch)',tqnch)
  call ovarrf(nout,'Time between pulses (s)','(tdwell)',tdwell)
  call oblnkl(nout)
  call ovarre(nout,'Pulse time (s)','(tpulse)',tpulse)
  call ovarrf(nout,'Down time (s)','(tdown)',tdown)
  call ovarre(nout,'Total plant cycle time (s)','(tcycle)',tcycle)

end subroutine outtim
