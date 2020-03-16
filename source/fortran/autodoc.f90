! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module autodoc_data

  !! Module providing global storage for autodoc variables
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module provides a means of sharing globally-used variables
  !! between all the subprograms of autodoc. A number of widely-used
  !! constants and character strings are also defined.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  character(len=*), parameter :: autodoc_version = &
       '$Id:: autodoc.f90 124 2012-11-07 15:56:26Z pknight                     $'

  !  Flags object: indicates whether each section associated with a
  !  given command is active (or open) (1) or inactive (or closed) (0)

  type :: flags
     integer :: fileopen = 0
     integer :: name = 0
     integer :: summary = 0
     integer :: type = 0
     integer :: contents = 0
     integer :: arguments = 0
     integer :: details = 0
     integer :: problems = 0
     integer :: calls = 0
     integer :: history = 0
     integer :: status
     integer :: author = 0
     integer :: docs = 0
     integer :: variables = 0
     integer :: vdfileopen = 0
     integer :: vardes = 0
  end type flags

  integer, parameter :: iounit = 1, hfunit = 2, vdunit = 3
  integer, parameter :: lenmax = 120
  character(len=lenmax) :: hfile = 'documentation/adheader.src'
  character(len=lenmax) :: ffile = 'documentation/adfooter.src'
  character(len=lenmax) :: vdfile = 'vardes.html'
  character(len=lenmax) :: outfile

  !  Hypertext elements

  character(len=3) , parameter :: html_par = '<P>'
  character(len=4) , parameter :: html_h2_open = '<H2>'
  character(len=5) , parameter :: html_h2_close = '</H2>'
  character(len=4) , parameter :: html_h3_open = '<H3>'
  character(len=5) , parameter :: html_h3_close = '</H3>'
  character(len=5) , parameter :: html_dir_open = '<DIR>'
  character(len=6) , parameter :: html_dir_close = '</DIR>'
  character(len=6) , parameter :: html_code_open = '<CODE>'
  character(len=7) , parameter :: html_code_close = '</CODE>'
  character(len=9) , parameter :: html_link_open = '<A HREF="'
  character(len=2) , parameter :: html_link_mid = '">'
  character(len=4) , parameter :: html_link_close = '</A>'
  character(len=4) , parameter :: html_ulist_open = '<UL>'
  character(len=5) , parameter :: html_ulist_close = '</UL>'
  character(len=4) , parameter :: html_listitem = '<LI>'
  character(len=4) , parameter :: html_hrule = '<HR>'
  character(len=20), parameter :: html_summary_header =   '<P><H3>Summary:</H3>'
  character(len=17), parameter :: html_type_header =      '<P><H3>Type:</H3>'
  character(len=21), parameter :: html_contents_header =  '<P><H3>Contents:</H3>'
  character(len=22), parameter :: html_arguments_header = '<P><H3>Arguments:</H3>'
  character(len=20), parameter :: html_details_header =   '<P><H3>Details:</H3>'
  character(len=25), parameter :: html_problems_header =  '<P><H3>Known Issues:</H3>'
  character(len=36), parameter :: html_calls_header =     '<P><H3>Routines/Modules Called:</H3>'
  character(len=20), parameter :: html_history_header =   '<P><H3>History:</H3>'
  character(len=19), parameter :: html_status_header =    '<P><H3>Status:</H3>'
  character(len=19), parameter :: html_author_header =    '<P><H3>Author:</H3>'
  character(len=34), parameter :: html_doc_header =       '<P><H3>Further Documentation:</H3>'
  character(len=22), parameter :: html_var_header =       '<P><H3>Variables:</H3>'

end module autodoc_data

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module calltree_data

  !! Module providing global storage for calling tree variables
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module provides a means of sharing globally-used variables
  !! between all the subprograms of autodoc involved in tracking the
  !! calling tree.
  !! <P>The calling tree is built using linked lists as the information
  !! is gathered from the 'cont' and 'call' autodoc entries. A 'master'
  !! linked list of subprograms is created, and each entry in this list
  !! has two sub-lists, one containing the subprogram's parents (i.e.
  !! the routines/modules etc. that directly call or contain the
  !! subprogram), and the other containing the subprogram's children
  !! (i.e. the routines/modules etc. that it calls/uses).
  !! <P>
  !! An example follows for clarity. Consider a program in which there
  !! are five routines:
  !! <P><DIR>a calls b and c; b calls c and d; c calls d; d calls e</DIR>
  !! <P>Graphically, we then have the following calling tree:
  !! <PRE>
  !! depth: 0    1    2    3    4
  !! .      a -- b ------- d -- e
  !! .       \    \       /
  !! .        \----\- c -/
  !! </PRE>
  !! This will be stored as the following linked list of five items:
  !! <PRE>
  !! a - parents none ; children b, c
  !! b - parents a    ; children c, d
  !! c - parents a, b ; children d
  !! d - parents b, c ; children e
  !! e - parents d    ; children none
  !! </PRE>
  !! (Note that the order may well be different to that above, but the
  !! 'topology' will be unchanged)
  !! <P>
  !! The module includes global pointers to enable the calling tree
  !! routines to keep track of where they are in the various linked
  !! lists.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  type :: routine
     character(len=60) :: name
     integer :: tag = -1
     integer :: depth = 0
     integer :: parents = 0
     integer :: children = 0
     type(routine), pointer :: first_parent => null()
     type(routine), pointer :: latest_parent => null()
     type(routine), pointer :: first_child => null()
     type(routine), pointer :: latest_child => null()
     type(routine), pointer :: next => null()
     logical :: visited = .false.
  end type routine

  integer :: tagcount = 0  !  running count of entries in master list
  logical :: recursion = .false.

  type(routine), pointer :: current => null()

  type(routine), pointer :: first_routine => null()  ! first entry in master list
  type(routine), pointer :: latest_routine => null() ! latest entry in master list

  type(routine), pointer :: parent => null()  !  parent of current
  type(routine), pointer :: child => null()   !  child of current

  character(len=16) :: ctfile = 'callingtree.html'

end module calltree_data

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program autodoc

  !! Main program for autodoc
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This is the autodoc main program.
  !! The program simply reads a text file (typically a program source
  !! file) from standard input, line-by-line, and acts on any
  !! autodoc commands it finds, creating a series of html files as
  !! it goes. It is not limited to documenting Fortran source!
  !! <P>
  !! Autodoc commands consist of a marker string, <CODE>+ad_</CODE>,
  !! followed by a four-character command label, with the rest
  !! of the line acting as the 'argument' of the command.
  !! <P>
  !! The available command labels are:
  !! <UL>
  !! <LI> <CODE>name</CODE>: Name of the subprogram (once only per
  !! subprogram) - must come first
  !! <LI> <CODE>summ</CODE>: Summary text
  !! <LI> <CODE>type</CODE>: Module, Subroutine, Main Program, Function
  !! etc.
  !! <LI> <CODE>auth</CODE>: Author's name
  !! <LI> <CODE>cont</CODE>: Contents - names of subroutines within a
  !! module, for instance, or None, or N/A
  !! <LI> <CODE>args</CODE>: Arguments, or None, or N/A
  !! <LI> <CODE>argc</CODE>: Continuation of an argument description
  !! (to limit line lengths)
  !! <LI> <CODE>desc</CODE>: Wordy description
  !! <LI> <CODE>prob</CODE>: Known problems or issues
  !! <LI> <CODE>call</CODE>: Subprograms called, or None, or N/A
  !! <LI> <CODE>hist</CODE>: History, in chronological order
  !! <LI> <CODE>hisc</CODE>: Continuation of a history description
  !! (to limit line lengths)
  !! <LI> <CODE>stat</CODE>: Status
  !! <LI> <CODE>stac</CODE>: Continuation of a status description
  !! (to limit line lengths)
  !! <LI> <CODE>docs</CODE>: Further documentation
  !! <LI> <CODE>docc</CODE>: Continuation of a documentation description
  !! (to limit line lengths)
  !! <LI> <CODE>vars</CODE>: Variable description
  !! <LI> <CODE>varc</CODE>: Continuation of a variable description
  !! (to limit line lengths)
  !! </UL>
  !! <P>
  !! A new html file is created whenever a 'name' command is found
  !! (so it is important that this is the first command to be given
  !! within a particular subprogram). The other commands can occur
  !! in any order, but it is encouraged that a standard order
  !! (such as that demonstrated here!) is adhered to.
  !! <P>
  !! Most commands can be repeated consecutively; however, some cannot,
  !! as they would lead to strange formatting in the html. If, for
  !! example, the description of a subprogram's argument is going
  !! to extend beyond a single line, the description should be
  !! continued using the 'argc' command, not the 'args' command.
  !! <P>
  !! The 'name', 'cont', and 'call' commands must contain a single
  !! string without spaces, as these will be converted into file names
  !! in the hypertext.
  !! <P>
  !! The user can use extra hypertext features as they see fit.
  !! <P>
  !! As the program's autodoc commands are read in, its <I>calling</I>
  !! and <I>called-by</I> trees are built up automatically from the
  !! information provided in the 'cont' and 'call' entries. (See the
  !! <A HREF="calltree_data.html">calling tree data module</A> for
  !! more information.)
  !! These trees are written out after input has completed, to a file
  !! called <CODE>callingtree.html</CODE>.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  character(len=lenmax) :: line
  character(len=4)  :: command

  !  Control flags

  type(flags) :: flag

  !  Functions

  integer, external :: read_line

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call reset_flags(flag)

  do while (read_line(line, command) /= -1)

     select case (command)

     case ('name') ; call open_file(line,flag)
     case ('summ') ; call section_summary(line,flag)
     case ('type') ; call section_type(line,flag)
     case ('auth') ; call section_author(line,flag)
     case ('cont') ; call section_contents(line,flag)
     case ('args') ; call section_arguments(line,flag)
     case ('argc') ; call section_arguments_cont(line,flag)
     case ('desc') ; call section_details(line,flag)
     case ('prob') ; call section_problems(line,flag)
     case ('call') ; call section_calls(line,flag)
     case ('hist') ; call section_history(line,flag)
     case ('hisc') ; call section_history_cont(line,flag)
     case ('stat') ; call section_status(line,flag)
     case ('stac') ; call section_status_cont(line,flag)
     case ('docs') ; call section_documentation(line,flag)
     case ('docc') ; call section_documentation_cont(line,flag)
     case ('vars') ; call section_variables(line,flag)
     case ('varc') ; call section_variables_cont(line,flag)

     case ('null') ; continue
     case default ; continue

     end select

     command = 'null'

  end do

  call close_file(flag)
  call close_vardes(flag)

  !call check_calltree_depth
  call print_calltree(flag)

end program autodoc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine reset_flags(flag)

  !! Routine that resets all the command flags to indicate that
  !! the command sections are all 'closed'
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of flags to be reset
  !! This routine sets all the components of the given flag
  !! object to zero, indicating that all the command sections
  !! are now inactive, or closed.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  flag%fileopen  = 0
  flag%name      = 0
  flag%summary   = 0
  flag%type      = 0
  flag%contents  = 0
  flag%arguments = 0
  flag%details   = 0
  flag%problems  = 0
  flag%calls     = 0
  flag%history   = 0
  flag%status    = 0
  flag%author    = 0
  flag%docs      = 0
  flag%variables = 0
  flag%vardes    = 0

end subroutine reset_flags

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer function read_line(line,command)

  !! Routine that reads a line from standard input and parses it
  !! to find any autodoc commands and their associated arguments
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : output string : extracted autodoc command argument
  !! command : output string : extracted autodoc command
  !! This routine reads a line from the standard input channel
  !! and looks for the autodoc marker (the string of characters
  !! '+ad_') within it. If this marker is present, the routine
  !! extracts the specific autodoc command and its argument
  !! from the line, and returns them via the function's arguments.
  !! <P>The routine returns 0 if a line was read in correctly
  !! (irrespective of whether there was an autodoc command within it),
  !! or -1 if the end of the file has been reached.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=lenmax), intent(out) :: line
  character(len=4),  intent(out) :: command

  !  Local variables

  character(len=4), parameter :: marker = '+ad_'
  integer :: position, count

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  read_line = 0

  !  Read in a line of the file from standard input

  read(5,'(A)',end=99) line

  !  Locate an occurrence of the marker within the line

  position = index(line,marker)

  if (position == 0) then !  Marker not present
     goto 1000
  else
     command = line(position+4:position+8)
     line = line(position+9:len(line))
  end if

  !  If line is empty, return

  if (line == '') goto 1000

  !  Rotate leading blanks to the end of the line

  do
     if (line(1:1) == ' ') then
        line = adjustl(line)
     else
        exit
     end if
  end do

  goto 1000  !  Line and command found successfully

  !  Return -1 if we have reached the end of the file

99 continue
  read_line = -1

1000 continue
  return

end function read_line

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine open_file(line,flag)

  !! Routine that opens a new html file for autodoc to write to
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : root of the filename to be opened
  !! flag : input/output flags object : set of command flags
  !! This routine opens a new html file for autodoc to use for the
  !! current subprogram's documentation. The filename to be used
  !! is the string contained within the <CODE>line</CODE> argument
  !! followed by <CODE>.html</CODE>.
  !! If another html file is already open, this is closed first.
  !! <P>After opening the new file, the header for the html is
  !! written to it, followed by a title line.
  !! <P>Next, the title line is written to the variable descriptor file.
  !! <P>Finally, a new entry is added to the calling tree.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=lenmax), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  character(len=lenmax) :: string

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Close any existing open files first

  if (flag%fileopen == 1) then
     call close_file(flag)
  end if

  write(*,*) 'Writing '//trim(line)//'.html...'

  flag%fileopen = 1
  outfile = trim(line)//'.html'
  open(unit=iounit, file=outfile,status='unknown')

  call header(flag)

  call write_to_file(html_h2_open,flag)
  call write_to_file(trim(line),flag)
  call write_to_file(html_h2_close,flag)

  !  Write name to variable descriptor file

  if (flag%vdfileopen == 0) call open_vardes(flag)
  call write_to_vardes(html_h3_open,flag)
  string = html_link_open//trim(line)//'.html'//html_link_mid//trim(line) &
       //html_link_close
  call write_to_vardes(trim(string),flag)
  call write_to_vardes(html_h3_close,flag)

  !  Add a new 'parent' to the calling tree

  call new_parent(line)

end subroutine open_file

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine close_file(flag)

  !! Routine that completes and closes an html file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! This routine closes an html file being used by autodoc.
  !! First, any currently open command sections are closed.
  !! Then the html footer is written to the file.
  !! Finally, the file is closed and the command flags are reset.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 1) then

     call close_sections(flag)

     call footer(flag)

     close(unit=iounit)

     if (flag%vardes == 1) then
        call write_to_vardes(html_ulist_close,flag)
     end if

     call reset_flags(flag)
  end if

end subroutine close_file

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine write_to_file(line,flag)

  !! Routine that writes a line of text to the current html file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : text to be written
  !! flag : input flags object : set of command flags
  !! This routine simply writes a line of text to the current
  !! html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(in) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 1) then
     write(iounit,*) trim(line)
  end if

end subroutine write_to_file

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine close_sections(flag)

  !! Routine that closes any currently-open command sections
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! Some of the autodoc commands result in an un-numbered list
  !! of bullet points being created in the html file. When a new
  !! command is encountered, or when the file is to be closed,
  !! any un-numbered lists currently in use must be closed with the
  !! relevant html tag. This routine ensures that this happens, and
  !! resets the relevant flag accordingly.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%contents == 1) then
     flag%contents = 0
     call write_to_file(html_ulist_close,flag)
  end if

  if (flag%arguments == 1) then
     flag%arguments = 0
     call write_to_file(html_ulist_close,flag)
  end if

  if (flag%calls == 1) then
     flag%calls = 0
     call write_to_file(html_ulist_close,flag)
  end if

  if (flag%history == 1) then
     flag%history = 0
     call write_to_file(html_ulist_close,flag)
  end if

  if (flag%docs == 1) then
     flag%docs = 0
     call write_to_file(html_ulist_close,flag)
  end if

  if (flag%status == 1) then
     flag%status = 0
     call write_to_file(html_ulist_close,flag)
  end if

  if (flag%author == 1) then
     flag%author = 0
     call write_to_file(html_ulist_close,flag)
  end if

  if (flag%variables == 1) then
     flag%variables = 0
     call write_to_file(html_ulist_close,flag)
  end if

end subroutine close_sections

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_summary(line,flag)

  !! Routine that acts on a 'summ' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'summ' command is encountered.
  !! If one is not already open, a 'Summary' section is started
  !! by writing the relevant section header line to the html file.
  !! The given line of text is then written to the html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%summary == 0) then
     call close_sections(flag)
     flag%summary = 1
     call write_to_file(html_summary_header,flag)
  end if

  call write_to_file(line,flag)

end subroutine section_summary

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_type(line,flag)

  !! Routine that acts on a 'type' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'type' command is encountered.
  !! If one is not already open, a 'Type' section is started
  !! by writing the relevant section header line to the html file.
  !! The given line of text is then written to the html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%type == 0) then
     call close_sections(flag)
     flag%type = 1
     call write_to_file(html_type_header,flag)
  end if

  call write_to_file(line,flag)

end subroutine section_type

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_details(line,flag)

  !! Routine that acts on a 'desc' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'desc' command is encountered.
  !! If one is not already open, a 'Details' section is started
  !! by writing the relevant section header line to the html file.
  !! The given line of text is then written to the html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%details == 0) then
     call close_sections(flag)
     flag%details = 1
     call write_to_file(html_details_header,flag)
  end if

  call write_to_file(line,flag)

end subroutine section_details

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_problems(line,flag)

  !! Routine that acts on a 'prob' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'prob' command is encountered.
  !! If one is not already open, a 'Known Issues' section is started
  !! by writing the relevant section header line to the html file.
  !! The given line of text is then written to the html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%problems == 0) then
     call close_sections(flag)
     flag%problems = 1
     call write_to_file(html_problems_header,flag)
  end if

  call write_to_file(line,flag)

end subroutine section_problems

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_contents(line,flag)

  !! Routine that acts on a 'cont' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'cont' command is encountered.
  !! If one is not already open, a 'Contents' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument (which must contain a single word
  !! without spaces) is then written to the html file as a list item,
  !! becoming a hypertext link to a file of the same name as the
  !! <CODE>line</CODE> string itself followed by <CODE>.html</CODE>.
  !! However, if <CODE>line</CODE> contains 'None' or 'N/A', then
  !! this is written out instead, without becoming a link to a file.
  !! <P>Finally, a 'child' entry for the current subprogram is added
  !! to the calling tree.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  character(len=lenmax) :: string

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%contents == 0) then
     call close_sections(flag)
     flag%contents = 1
     call write_to_file(html_contents_header,flag)
     call write_to_file(html_ulist_open,flag)
  end if

  if ((trim(line) == 'N/A').or.(trim(line) == 'None')) then
     call write_to_file(html_listitem,flag)
     call write_to_file(line,flag)
  else
     call write_to_file(html_listitem,flag)
     string = html_link_open//trim(line)//'.html'//html_link_mid//trim(line) &
          //html_link_close
     call write_to_file(string,flag)

     !  Add a new 'child' to the calling tree

     call new_child(line)

  end if

end subroutine section_contents

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_arguments(line,flag)

  !! Routine that acts on an 'args' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if an 'args' command is encountered.
  !! If one is not already open, an 'Arguments' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a list item.
  !! <P>If a description of the argument extends over more than
  !! one line of the source file, an 'argc' command should be used
  !! instead for continuation lines, otherwise a new bullet will be
  !! written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%arguments == 0) then
     call close_sections(flag)
     flag%arguments = 1
     call write_to_file(html_arguments_header,flag)
     call write_to_file(html_ulist_open,flag)
  end if

  call write_to_file(html_listitem,flag)
  call write_to_file(line,flag)

end subroutine section_arguments

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_arguments_cont(line,flag)

  !! Routine that acts on an 'argc' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if an 'argc' command is encountered.
  !! If one is not already open, an 'Arguments' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a continuation of a list item.
  !! <P>The 'argc' command should be used to continue an 'args'
  !! command if the description of the argument extends over more than
  !! one line of the source file, to prevent extra bullets being
  !! written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%arguments == 0) then
     call close_sections(flag)
     flag%arguments = 1
     call write_to_file(html_arguments_header,flag)
     call write_to_file(html_ulist_open,flag)
     call write_to_file(html_listitem,flag)
  end if

  call write_to_file(line,flag)

end subroutine section_arguments_cont

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_calls(line,flag)

  !! Routine that acts on a 'call' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'call' command is encountered.
  !! If one is not already open, a 'Routines/Modules Called' section
  !! is started by writing the relevant section header line to the
  !! html file, and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument (which must contain a single word
  !! without spaces) is then written to the html file as a list item,
  !! becoming a hypertext link to a file of the same name as the
  !! <CODE>line</CODE> string itself followed by <CODE>.html</CODE>.
  !! However, if <CODE>line</CODE> contains 'None' or 'N/A', then
  !! this is written out instead, without becoming a link to a file.
  !! <P>Finally, a 'child' entry for the current subprogram is added
  !! to the calling tree.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  character(len=lenmax) :: string

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%calls == 0) then
     call close_sections(flag)
     flag%calls = 1
     call write_to_file(html_calls_header,flag)
     call write_to_file(html_ulist_open,flag)
  end if

  if ((trim(line) == 'N/A').or.(trim(line) == 'None')) then
     call write_to_file(html_listitem,flag)
     call write_to_file(line,flag)
  else
     call write_to_file(html_listitem,flag)
     string = html_link_open//trim(line)//'.html'//html_link_mid//trim(line) &
          //html_link_close
     call write_to_file(string,flag)

     !  Add a new 'child' to the calling tree

     call new_child(line)

  end if

end subroutine section_calls

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_history(line,flag)

  !! Routine that acts on a 'hist' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'hist' command is encountered.
  !! If one is not already open, a 'History' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a list item.
  !! <P>If a description of a given history item extends over more than
  !! one line of the source file, a 'hisc' command should be used
  !! instead for continuation lines, otherwise a new bullet will be
  !! written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%history == 0) then
     call close_sections(flag)
     flag%history = 1
     call write_to_file(html_history_header,flag)
     call write_to_file(html_ulist_open,flag)
  end if

  call write_to_file(html_listitem,flag)
  call write_to_file(line,flag)

end subroutine section_history

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_history_cont(line,flag)

  !! Routine that acts on a 'hisc' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'hisc' command is encountered.
  !! If one is not already open, a 'History' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a continuation of a list item.
  !! <P>The 'hisc' command should be used to continue a 'hist'
  !! command if the description of a given history item extends over
  !! more than one line of the source file, to prevent extra bullets
  !! being written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%history == 0) then
     call close_sections(flag)
     flag%history = 1
     call write_to_file(html_history_header,flag)
     call write_to_file(html_ulist_open,flag)
     call write_to_file(html_listitem,flag)
  end if

  call write_to_file(line,flag)

end subroutine section_history_cont

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_status(line,flag)

  !! Routine that acts on a 'stat' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'stat' command is encountered.
  !! If one is not already open, a 'Status' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a list item.
  !! <P>If a description of the status extends over more than
  !! one line of the source file, a 'stac' command should be used
  !! instead for continuation lines, otherwise a new bullet will be
  !! written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%status == 0) then
     call close_sections(flag)
     flag%status = 1
     call write_to_file(html_status_header,flag)
     call write_to_file(html_ulist_open,flag)
  end if

  call write_to_file(html_listitem,flag)
  call write_to_file(line,flag)

end subroutine section_status

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_status_cont(line,flag)

  !! Routine that acts on a 'stac' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'stac' command is encountered.
  !! If one is not already open, a 'Status' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a continuation of a list item.
  !! <P>The 'stac' command should be used to continue a 'stat'
  !! command if the description of the status extends over more than
  !! one line of the source file, to prevent extra bullets being
  !! written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%status == 0) then
     call close_sections(flag)
     flag%status = 1
     call write_to_file(html_status_header,flag)
     call write_to_file(html_ulist_open,flag)
     call write_to_file(html_listitem,flag)
  end if

  call write_to_file(line,flag)

end subroutine section_status_cont

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_variables(line,flag)

  !! Routine that acts on a 'vars' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'vars' command is encountered.
  !! If one is not already open, a 'Variables' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! and to the variable descriptor file as a list item.
  !! <P>If a description of the variable extends over more than
  !! one line of the source file, a 'varc' command should be used
  !! instead for continuation lines, otherwise a new bullet will be
  !! written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%variables == 0) then
     call close_sections(flag)
     flag%variables = 1
     call write_to_file(html_var_header,flag)
     call write_to_file(html_ulist_open,flag)
  end if

  call write_to_file(html_listitem,flag)
  call write_to_file(line,flag)

  !  Add variable to variable descriptor file

  if (flag%vardes == 0) then
     call write_to_vardes(html_ulist_open,flag)
     flag%vardes = 1
  end if
  call write_to_vardes(html_listitem,flag)
  call write_to_vardes(line,flag)

end subroutine section_variables

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_variables_cont(line,flag)

  !! Routine that acts on a 'varc' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'varc' command is encountered.
  !! If one is not already open, a 'Variables' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! and the variable descriptor file as a continuation of a list item.
  !! <P>The 'varc' command should be used to continue a 'vars'
  !! command if the description of the variable extends over more than
  !! one line of the source file, to prevent extra bullets being
  !! written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%variables == 0) then
     call close_sections(flag)
     flag%variables = 1
     call write_to_file(html_var_header,flag)
     call write_to_file(html_ulist_open,flag)
     call write_to_file(html_listitem,flag)
  end if

  call write_to_file(line,flag)
  call write_to_vardes(line,flag)

end subroutine section_variables_cont

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_author(line,flag)

  !! Routine that acts on an 'auth' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if an 'auth' command is encountered.
  !! If one is not already open, an 'Author' section is started
  !! by writing the relevant section header line to the html file,
  !! and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a list item.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%author == 0) then
     call close_sections(flag)
     flag%author = 1
     call write_to_file(html_author_header,flag)
     call write_to_file(html_ulist_open,flag)
  end if

  call write_to_file(html_listitem,flag)
  call write_to_file(line,flag)

end subroutine section_author

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_documentation(line,flag)

  !! Routine that acts on a 'docs' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'docs' command is encountered.
  !! If one is not already open, a 'Further Documentation' section
  !! is started by writing the relevant section header line to the
  !! html file, and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a list item.
  !! <P>If a description of a piece of documentation extends over
  !! more than one line of the source file, a 'docc' command should
  !! be used instead for continuation lines, otherwise a new bullet
  !! will be written to the html file, making the output look strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%docs == 0) then
     call close_sections(flag)
     flag%docs = 1
     call write_to_file(html_doc_header,flag)
     call write_to_file(html_ulist_open,flag)
  end if

  call write_to_file(html_listitem,flag)
  call write_to_file(line,flag)

end subroutine section_documentation

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_documentation_cont(line,flag)

  !! Routine that acts on a 'docc' command
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : line of text to be written
  !! flag : input/output flags object : set of command flags
  !! This routine is called if a 'docc' command is encountered.
  !! If one is not already open, a 'Further Documentation' section
  !! is started by writing the relevant section header line to the
  !! html file, and an un-numbered list (of bullet points) is started.
  !! The <CODE>line</CODE> argument is then written to the html file
  !! as a continuation of a list item.
  !! <P>The 'docc' command should be used to continue a 'docs'
  !! command if the description of a piece of documentation extends
  !! over more than one line of the source file, to prevent extra
  !! bullets being written to the html file, making the output look
  !! strange.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  if (flag%docs == 0) then
     call close_sections(flag)
     flag%docs = 1
     call write_to_file(html_doc_header,flag)
     call write_to_file(html_ulist_open,flag)
     call write_to_file(html_listitem,flag)
  end if

  call write_to_file(line,flag)

end subroutine section_documentation_cont

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine header(flag)

  !! Routine that writes the header of the html file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! This routine writes the header of the newly-opened html file.
  !! If file <CODE>adheader.src</CODE> exists in the current working
  !! directory it is used as the html source for the header; this
  !! can be modified as necessary to provide links to other files,
  !! or to customise the html to local conditions, etc.
  !! Otherwise, a plain header is written to the html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  character(len=lenmax) :: string
  logical :: file_exists

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  inquire(file=hfile,exist=file_exists)

  if (file_exists) then
     open(unit=hfunit,file=hfile,status='old')
     do
        read(hfunit,'(A)',end=99) string
        call write_to_file(string,flag)
     end do
  else
     string = '<HTML><HEAD><TITLE>Code Documentation</TITLE></HEAD><BODY>'
     call write_to_file(string,flag)
     return
  end if

99 continue
  if (file_exists) close(unit=hfunit)

end subroutine header

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine footer(flag)

  !! Routine that writes the footer of the html file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! This routine writes the footer of the currently-open html file.
  !! If file <CODE>adfooter.src</CODE> exists in the current working
  !! directory it is used as the html source for the footer; this
  !! can be modified as necessary to provide links to other files,
  !! or to customise the html to local conditions, etc.
  !! Otherwise a plain footer is written to the html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  character(len=lenmax) :: string
  logical :: file_exists

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%fileopen == 0) return

  inquire(file=ffile,exist=file_exists)

  if (file_exists) then
     open(unit=hfunit,file=ffile,status='old')
     do
        read(hfunit,'(A)',end=99) string
        call write_to_file(string,flag)
     end do
  else
     string = '</BODY></HTML>'
     call write_to_file(string,flag)
     return
  end if

99 continue
  if (file_exists) close(unit=hfunit)

end subroutine footer

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine open_vardes(flag)

  !! Routine that opens a new variable descriptor html file for
  !! autodoc to write to
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! This routine opens a new html file for autodoc to use for the
  !! variable descriptor file. The filename to be used
  !! is defined by variable <CODE>vdfile</CODE> in the
  !! <A HREF="autodoc_data.html"><CODE>autodoc_data</CODE></A> module.
  !! <P>After opening the new file, the header for the html is
  !! written to it, followed by a title line.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  character(len=8) :: date
  character(len=lenmax) :: string

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  If the file is already open, exit the routine

  if (flag%vdfileopen == 1) return

  write(*,*) 'Writing '//trim(vdfile)//'...'

  flag%vdfileopen = 1
  open(unit=vdunit,file=vdfile,status='replace')

  call header_vardes(flag)

  call write_to_vardes(html_h2_open,flag)

  call date_and_time(date=date)
  string = 'PROCESS Variable Descriptor File : dated '//date
  call write_to_vardes(trim(string),flag)

  call write_to_vardes(html_h2_close,flag)

  !string = 'Default values roughly correspond to the ITER-FDR (1998) design.'
  !call write_to_vardes(string,flag)
  !string = '<P><B>Reference:</B>'
  !call write_to_vardes(string,flag)
  !string = 'Table 1. ITER design features and parameters for reference'
  !call write_to_vardes(string,flag)
  !string = 'ignited ELMy H-mode operation,'
  !call write_to_vardes(string,flag)

  !call write_to_vardes(html_link_open,flag)
  !string = 'http://iopscience.iop.org/0029-5515/39/12/301/pdf/0029-5515_39_12_301.pdf'
  !call write_to_vardes(string,flag)
  !call write_to_vardes(html_link_mid,flag)
  !string = 'Nuclear Fusion <B>39</B> (1999) 2137'
  !call write_to_vardes(string,flag)
  !call write_to_vardes(html_link_close,flag)

  call write_to_vardes(html_hrule,flag)

  string = 'Variables labelled with FIX are initialised with the given'
  call write_to_vardes(string,flag)
  string = 'default value (shown between / / characters), but currently'
  call write_to_vardes(string,flag)
  string = 'are not available to be changed in the input file.'
  call write_to_vardes(string,flag)

  call write_to_vardes(html_par,flag)
  string = 'All other variables shown with a default value  '
  call write_to_vardes(string,flag)
  string = '(including arrays boundl, boundu and sweep) can be changed in the input file.'
  call write_to_vardes(string,flag)

  call write_to_vardes(html_par,flag)
  string = 'Variables not shown with a default value are calculated'
  call write_to_vardes(string,flag)
  string = 'within PROCESS, so need not be initialised.'
  call write_to_vardes(string,flag)

  call write_to_vardes(html_hrule,flag)

end subroutine open_vardes

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine close_vardes(flag)

  !! Routine that completes and closes a variable descriptor html file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! This routine closes the variable descriptor html file.
  !! Firstly, the html footer is written to the file.
  !! Then, the file is closed and the command flags are reset.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%vdfileopen == 1) then
!     call write_to_vardes(html_ulist_close,flag)
     call footer_vardes(flag)
     close(unit=vdunit)
     flag%vdfileopen = 0
  end if

end subroutine close_vardes

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine header_vardes(flag)

  !! Routine that writes the header of the variable descriptor html file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! This routine writes the header of the variable descriptor html file.
  !! If file <CODE>adheader.src</CODE> exists in the current working
  !! directory it is used as the html source for the header; this
  !! can be modified as necessary to provide links to other files,
  !! or to customise the html to local conditions, etc.
  !! Otherwise, a plain header is written to the html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  character(len=lenmax) :: string
  logical :: file_exists

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%vdfileopen == 0) return

  inquire(file=hfile,exist=file_exists)

  if (file_exists) then
     open(unit=hfunit,file=hfile,status='old')
     do
        read(hfunit,'(A)',end=99) string
        call write_to_vardes(string,flag)
     end do
  else
     string = '<HTML><HEAD><TITLE>Variable Descriptor</TITLE></HEAD><BODY>'
     call write_to_vardes(string,flag)
  end if

99 continue
  if (file_exists) close(unit=hfunit)

end subroutine header_vardes

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine footer_vardes(flag)

  !! Routine that writes the footer of the variable descriptor html file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! This routine writes the footer of the variable descriptor html file.
  !! If file <CODE>adfooter.src</CODE> exists in the current working
  !! directory it is used as the html source for the footer; this
  !! can be modified as necessary to provide links to other files,
  !! or to customise the html to local conditions, etc.
  !! Otherwise a plain footer is written to the html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  character(len=lenmax) :: string
  logical :: file_exists

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%vdfileopen == 0) return

  inquire(file=ffile,exist=file_exists)

  if (file_exists) then
     open(unit=hfunit,file=ffile,status='old')
     do
        read(hfunit,'(A)',end=99) string
        call write_to_vardes(string,flag)
     end do
  else
     string = '</BODY></HTML>'
     call write_to_vardes(string,flag)
  end if

99 continue
  if (file_exists) close(unit=hfunit)

end subroutine footer_vardes

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine write_to_vardes(line,flag)

  !! Routine that writes a line of text to the variable
  !! descriptor html file
  !! author: P J Knight, CCFE, Culham Science Centre
  !! line : input string : text to be written
  !! flag : input flags object : set of command flags
  !! This routine simply writes a line of text to the
  !! variable descriptor html file.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=*), intent(in) :: line
  type(flags), intent(in) :: flag

  !  Local variables

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (flag%vdfileopen == 1) then
     write(vdunit,*) trim(line)
  end if

end subroutine write_to_vardes

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine new_parent(name)

  !! Routine that adds a new entry to the calling tree list
  !! if it is not already present
  !! author: P J Knight, CCFE, Culham Science Centre
  !! name : input string : name of routine to be added to list
  !! This routine adds a new entry to the master list of routines,
  !! if a routine of the given name is not already present in the
  !! linked list.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data
  use calltree_data

  implicit none

  !  Arguments

  character(len=lenmax), intent(in) :: name

  !  Local variables

  logical :: exists
  type(routine), pointer :: ptr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Set up initial entry in the calling tree's list of routines,
  !  if not yet present

  if (.not.associated(first_routine)) then
     allocate(parent)
     first_routine => parent
     latest_routine => parent
     nullify(latest_routine%next)

     parent%name = name
     tagcount = tagcount + 1
     parent%tag = tagcount
  end if

  !  Check if routine already exists in the list

  exists = .false.
  ptr => first_routine
  check_if_routine_exists: do
     if (.not.associated(ptr)) exit
     if (ptr%name == name) then
        exists = .true.
        exit
     end if
     ptr => ptr%next
  end do check_if_routine_exists

  if (exists) then
     parent => ptr
  else
     !  Add to end of routine list, and populate fields
     allocate(latest_routine%next)
     parent => latest_routine%next
     latest_routine => latest_routine%next
     nullify(latest_routine%next)

     parent%name = name
     tagcount = tagcount + 1
     parent%tag = tagcount
  end if

end subroutine new_parent

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine new_child(name)

  !! Routine that adds a new child to the calling tree list
  !! if it is not already present
  !! author: P J Knight, CCFE, Culham Science Centre
  !! name : input string : name of routine to be added to list
  !! This routine adds a new entry to the current subprogram's list
  !! of children, and adds the current subprogram to the list of the
  !! child's parents.
  !! <P>If the child does not yet exist in the master linked list
  !! it is added to this list first.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data
  use calltree_data

  implicit none

  !  Arguments

  character(len=lenmax), intent(in) :: name

  !  Local variables

  logical :: exists
  type(routine), pointer :: ptr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Check the master linked list of subprograms to see if the child
  !  is already present in the list

  exists = .false.
  ptr => first_routine
  check_if_child_exists: do
     if (.not.associated(ptr)) exit
     if (ptr%name == name) then
        exists = .true.
        exit
     end if
     ptr => ptr%next
  end do check_if_child_exists

  if (.not.exists) then
     allocate(latest_routine%next)
     child => latest_routine%next
     latest_routine => latest_routine%next
     nullify(latest_routine%next)

     child%name = name
     tagcount = tagcount + 1
     child%tag = tagcount
     child%depth = parent%depth + 1
     child%parents = 0
     child%children = 0
  else
     child => ptr
     child%depth = max(child%depth, parent%depth + 1)
  end if

  !  Add this child to the current routine's list of children

  parent%children = parent%children + 1

  if (.not.associated(parent%first_child)) then
     allocate(parent%first_child)
     parent%latest_child => parent%first_child
     nullify(parent%latest_child%next)
  else
     allocate(parent%latest_child%next)
     parent%latest_child => parent%latest_child%next
     nullify(parent%latest_child%next)
  end if

  parent%latest_child%name = child%name

  !  Add the parent to the child's list of parents

  child%parents = child%parents + 1

  if (.not.associated(child%first_parent)) then
     allocate(child%first_parent)
     child%latest_parent => child%first_parent
     nullify(child%latest_parent%next)
  else
     allocate(child%latest_parent%next)
     child%latest_parent => child%latest_parent%next
     nullify(child%latest_parent%next)
  end if

  child%latest_parent%name = parent%name

end subroutine new_child

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine check_calltree_depth

  !! Routine that checks each routine's depth within the calling tree
  !! author: P J Knight, CCFE, Culham Science Centre
  !! None
  !! This routine loops through all the entries in the master calling
  !! tree linked list and checks that each entry is 'deeper' than all
  !! its parents. A routine's 'depth' is zero if nothing calls it, and
  !! is at least one more than the depth of any routine that directly
  !! calls it. This is evaluated by continually looping through the
  !! list of entries, checking their depth against those of their
  !! parents, and adjusting their depth if it is not consistent. The
  !! task is complete only when a loop does not make any depth
  !! adjustments.
  !! <P>In many programs, the calling tree is straightforward,
  !! as in the following example:
  !! <P>a calls b and c; b calls c and d; c calls d; d calls e
  !! <PRE>
  !! depth: 0    1    2    3    4
  !! .      a -- b ------- d -- e
  !! .       \    \       /
  !! .        \----\- c -/
  !! </PRE>
  !! However, in the case of programs which contain any
  !! recursive calls (a calls b calls c calls a, for instance)
  !! the depth is badly defined and this routine fails to deal with
  !! it correctly at present.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data
  use calltree_data

  implicit none

  !  Arguments

  !  Local variables

  logical :: finished
  type(routine), pointer :: ptr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do
     finished = .true.
     parent => first_routine
     do
        if (.not.associated(parent)) exit

        child => parent%first_child
        do  !  Loop through parent's children
           if (.not.associated(child)) exit

           !  Find the routine matching the child's name
           !  If necessary, modify its depth
           ptr => first_routine
           do
              if (.not.associated(ptr)) exit
              if (ptr%name == child%name) then
                 !if (ptr%depth <= parent%depth) then  !  segfaults if recursion
                 !  Following line catches most problems with recursion,
                 !  or at least prevents most segfaults,
                 !  although the depths will be wrong...
                 if ((ptr%depth <= parent%depth).and.(ptr%depth < tagcount)) then
                    finished = .false.
                    ptr%depth = parent%depth + 1
                    if (ptr%depth > tagcount) recursion = .true.
                 end if
                 exit
              end if
              ptr => ptr%next
           end do

           child => child%next
        end do

        parent => parent%next
     end do

     if (finished) exit  !  No depth changes were made on the latest loop
  end do

end subroutine check_calltree_depth

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine print_calltree(flag)

  !! Routine that prints the calling and called-by trees
  !! author: P J Knight, CCFE, Culham Science Centre
  !! flag : input/output flags object : set of command flags
  !! This routine runs through the master list of routines
  !! stored in the calling tree, and writes out an html file
  !! called <A HREF="callingtree.html"><CODE>callingtree.html</CODE></A>
  !! which lists each routine's parents and children.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data
  use calltree_data

  implicit none

  !  Arguments

  type(flags), intent(inout) :: flag

  !  Local variables

  integer :: i
  character(len=lenmax) :: string
  type(routine), pointer :: ptr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Close any existing open files first

  if (flag%fileopen == 1) then
     call close_file(flag)
  end if

  flag%fileopen = 1
  !  Print out results - text format

  write(*,*) ' '
  write(*,*) 'Writing '//trim(ctfile)

  open(unit=iounit, file=ctfile,status='unknown')

  call header(flag)

  call write_to_file(html_h2_open,flag)
  string = 'Calling Tree' ; call write_to_file(string,flag)
  call write_to_file(html_h2_close,flag)

  call write_to_file(html_ulist_open,flag)

  parent => first_routine
  do
     if (.not.associated(parent)) exit

     !  Print current routine name
     call write_to_file(html_listitem,flag)
     string = html_link_open//trim(parent%name)//'.html' &
          //html_link_mid//trim(parent%name)//html_link_close
     call write_to_file(string,flag)
     call write_to_file(html_ulist_open,flag)

     !  Print current routine's children, if present
     ptr => parent%first_child
     if (associated(ptr)) then
        call write_to_file(html_listitem,flag)
        string = 'calls/contains the following subprogram(s):'
        call write_to_file(string,flag)
        call write_to_file(html_ulist_open,flag)
        do
           if (.not.associated(ptr)) then  !  Last child has been printed
              call write_to_file(html_ulist_close,flag)
              exit
           end if
           call write_to_file(html_listitem,flag)
           string = html_link_open//trim(ptr%name) &
                //'.html'//html_link_mid//trim(ptr%name)//html_link_close
           call write_to_file(string,flag)
           ptr => ptr%next
        end do
     else
        call write_to_file(html_listitem,flag)
        string = 'does not call/contain any subprograms'
        call write_to_file(string,flag)
     end if

     !  Print current routine's parents, if present
     ptr => parent%first_parent
     if (associated(ptr)) then
        call write_to_file(html_listitem,flag)
        string = 'is called by the following subprogram(s):'
        call write_to_file(string,flag)
        call write_to_file(html_ulist_open,flag)
        do
           if (.not.associated(ptr)) then  !  Last parent has been printed
              call write_to_file(html_ulist_close,flag)
              exit
           end if
           call write_to_file(html_listitem,flag)
           string = html_link_open//trim(ptr%name) &
                //'.html'//html_link_mid//trim(ptr%name)//html_link_close
           call write_to_file(string,flag)
           ptr => ptr%next
        end do
     else
        call write_to_file(html_listitem,flag)
        string = 'is not called by any subprogram'
        call write_to_file(string,flag)
     end if
     call write_to_file(html_ulist_close,flag)

     parent => parent%next
  end do

!  If a program uses recursion, this section fails with a segmentation fault
!  - keep commented out until this is fixed.

!  !  Print out results - call tree format
!
!  if (recursion) then
!     write(*,*) ' '
!     write(*,*) 'Recursion has been detected; the following tree will be very misleading...'
!  end if
!
!  write(*,*) ' '
!  current => first_routine
!  do
!     if (.not.associated(current)) exit
!     if (.not.current%visited) then
!        do i = 1,7*current%depth
!           write(*,'(A1)',advance='no') ' '
!        end do
!        call descend_tree(current)
!     end if
!     current => current%next
!  end do

  call close_file(flag)

end subroutine print_calltree

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive subroutine descend_tree(subprogram)

  !! Routine that drills down the calling tree recursively to
  !! print its structure, correctly indented
  !! author: P J Knight, CCFE, Culham Science Centre
  !! subprogram : input/output routine type :
  !! This routine writes out the calling tree using correctly
  !! indented branches from each entry to show the tree structure.
  !! The routine is recursive so that each child's child is
  !! accounted for.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data
  use calltree_data

  implicit none

  !  Arguments

  type(routine), intent(inout) :: subprogram

  !  Local variables

  integer :: i
  type(routine), pointer :: ptr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if (subprogram%visited) then
     write(*,*) trim(subprogram%name),'...'
     return
  else
     write(*,*) trim(subprogram%name)
     subprogram%visited = .true.
  end if

  child => subprogram%first_child
  do  !  Loop through subprogram's children
     if (.not.associated(child)) exit

     !  Find the routine matching the child's name
     !  If necessary, modify its depth
     ptr => first_routine
     do
        if (.not.associated(ptr)) exit
        if (ptr%name == child%name) then
           do i = 1,7*subprogram%depth+3
              write(*,'(A1)',advance='no') ' '
           end do
           write(*,'(A1)',advance='no') '|'
           do i = 1,7*(ptr%depth-subprogram%depth)-4
              write(*,'(A1)',advance='no') '-'
           end do
           call descend_tree(ptr)
           exit
        end if
        ptr => ptr%next
     end do

     child => child%next
  end do

end subroutine descend_tree
