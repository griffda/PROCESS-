!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module autodoc_data

  !+ad_name  autodoc_data
  !+ad_summ  Module providing global storage for autodoc variables
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  N/A
  !+ad_desc  This module provides a means of sharing globally-used variables
  !+ad_desc  between all the subprograms of autodoc. A number of widely-used
  !+ad_desc  constants and character strings are also defined.
  !+ad_prob  None
  !+ad_call  N/A
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_hist  06/11/2006 PJK Changed lenmax from 80 to 85
  !+ad_hist  13/03/2009 PJK Changed lenmax from 85 to 100
  !+ad_hist  09/06/2011 PJK Added version number
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  public

  character(len=*), parameter :: autodoc_version = &
       '$Id::                                                                  $'

  !  Flags object: indicates whether each section associated with a
  !  given command is active (or open) (1) or inactive (or closed) (0)

  type :: flags
     integer :: fileopen
     integer :: name
     integer :: summary
     integer :: type
     integer :: contents
     integer :: arguments
     integer :: details
     integer :: problems
     integer :: calls
     integer :: history
     integer :: status
     integer :: author
     integer :: docs
  end type flags

  integer, parameter :: iounit = 1, hfunit = 2
  integer, parameter :: lenmax = 100
  character(len=lenmax) :: hfile = 'adheader.src'
  character(len=lenmax) :: ffile = 'adfooter.src'
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

end module autodoc_data

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module calltree_data

  !+ad_name  calltree_data
  !+ad_summ  Module providing global storage for calling tree variables
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  N/A
  !+ad_desc  This module provides a means of sharing globally-used variables
  !+ad_desc  between all the subprograms of autodoc involved in tracking the
  !+ad_desc  calling tree.
  !+ad_desc  <P>The calling tree is built using linked lists as the information
  !+ad_desc  is gathered from the 'cont' and 'call' autodoc entries. A 'master'
  !+ad_desc  linked list of subprograms is created, and each entry in this list
  !+ad_desc  has two sub-lists, one containing the subprogram's parents (i.e.
  !+ad_desc  the routines/modules etc. that directly call or contain the
  !+ad_desc  subprogram), and the other containing the subprogram's children
  !+ad_desc  (i.e. the routines/modules etc. that it calls/uses).
  !+ad_desc  <P>
  !+ad_desc  An example follows for clarity. Consider a program in which there
  !+ad_desc  are five routines:
  !+ad_desc  <P><DIR>a calls b and c; b calls c and d; c calls d; d calls e</DIR>
  !+ad_desc  <P>Graphically, we then have the following calling tree:
  !+ad_desc  <PRE>
  !+ad_desc  depth: 0    1    2    3    4
  !+ad_desc  .      a -- b ------- d -- e
  !+ad_desc  .       \    \       /       
  !+ad_desc  .        \----\- c -/        
  !+ad_desc  </PRE>
  !+ad_desc  This will be stored as the following linked list of five items:
  !+ad_desc  <PRE>
  !+ad_desc     a - parents none ; children b, c
  !+ad_desc     b - parents a    ; children c, d
  !+ad_desc     c - parents a, b ; children d
  !+ad_desc     d - parents b, c ; children e
  !+ad_desc     e - parents d    ; children none
  !+ad_desc  </PRE>
  !+ad_desc  (Note that the order may well be different to that above, but the
  !+ad_desc  'topology' will be unchanged)
  !+ad_desc  <P>
  !+ad_desc  The module includes global pointers to enable the calling tree
  !+ad_desc  routines to keep track of where they are in the various linked
  !+ad_desc  lists.
  !+ad_prob  None
  !+ad_call  N/A
  !+ad_hist  11/02/2009 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program autodoc

  !+ad_name  autodoc
  !+ad_summ  Main program for autodoc
  !+ad_type  Main program
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  N/A
  !+ad_desc  This is the autodoc main program.
  !+ad_desc  The program simply reads a text file (typically a program source
  !+ad_desc  file) from standard input, line-by-line, and acts on any
  !+ad_desc  autodoc commands it finds, creating a series of html files as
  !+ad_desc  it goes. It is not limited to documenting Fortran source!
  !+ad_desc  <P>
  !+ad_desc  Autodoc commands consist of a marker string, <CODE>+ad_</CODE>,
  !+ad_desc  followed by a four-character command label, with the rest
  !+ad_desc  of the line acting as the 'argument' of the command.
  !+ad_desc  <P>
  !+ad_desc  The available command labels are:
  !+ad_desc  <UL>
  !+ad_desc  <LI> <CODE>name</CODE>: Name of the subprogram (once only per
  !+ad_desc       subprogram) - must come first
  !+ad_desc  <LI> <CODE>summ</CODE>: Summary text
  !+ad_desc  <LI> <CODE>type</CODE>: Module, Subroutine, Main Program, Function
  !+ad_desc       etc.
  !+ad_desc  <LI> <CODE>auth</CODE>: Author's name
  !+ad_desc  <LI> <CODE>cont</CODE>: Contents - names of subroutines within a
  !+ad_desc       module, for instance, or None, or N/A
  !+ad_desc  <LI> <CODE>args</CODE>: Arguments, or None, or N/A
  !+ad_desc  <LI> <CODE>argc</CODE>: Continuation of an argument description
  !+ad_desc       (to limit line lengths)
  !+ad_desc  <LI> <CODE>desc</CODE>: Wordy description
  !+ad_desc  <LI> <CODE>prob</CODE>: Known problems or issues
  !+ad_desc  <LI> <CODE>call</CODE>: Subprograms called, or None, or N/A
  !+ad_desc  <LI> <CODE>hist</CODE>: History, in chronological order
  !+ad_desc  <LI> <CODE>hisc</CODE>: Continuation of a history description
  !+ad_desc       (to limit line lengths)
  !+ad_desc  <LI> <CODE>stat</CODE>: Status
  !+ad_desc  <LI> <CODE>stac</CODE>: Continuation of a status description
  !+ad_desc       (to limit line lengths)
  !+ad_desc  <LI> <CODE>docs</CODE>: Further documentation
  !+ad_desc  <LI> <CODE>docc</CODE>: Continuation of a documentation description
  !+ad_desc       (to limit line lengths)
  !+ad_desc  </UL>
  !+ad_desc  <P>
  !+ad_desc  A new html file is created whenever a 'name' command is found
  !+ad_desc  (so it is important that this is the first command to be given
  !+ad_desc  within a particular subprogram). The other commands can occur
  !+ad_desc  in any order, but it is encouraged that a standard order
  !+ad_desc  (such as that demonstrated here!) is adhered to.
  !+ad_desc  <P>
  !+ad_desc  Most commands can be repeated consecutively; however, some cannot,
  !+ad_desc  as they would lead to strange formatting in the html. If, for
  !+ad_desc  example, the description of a subprogram's argument is going
  !+ad_desc  to extend beyond a single line, the description should be
  !+ad_desc  continued using the 'argc' command, not the 'args' command.
  !+ad_desc  <P>
  !+ad_desc  The 'name', 'cont', and 'call' commands must contain a single
  !+ad_desc  string without spaces, as these will be converted into file names
  !+ad_desc  in the hypertext.
  !+ad_desc  <P>
  !+ad_desc  The user can use extra hypertext features as they see fit.
  !+ad_desc  <P>
  !+ad_desc  As the program's autodoc commands are read in, its <I>calling</I>
  !+ad_desc  and <I>called-by</I> trees are built up automatically from the
  !+ad_desc  information provided in the 'cont' and 'call' entries. (See the
  !+ad_desc  <A HREF="calltree_data.html">calling tree data module</A> for
  !+ad_desc  more information.)
  !+ad_desc  These trees are written out after input has completed, to a file
  !+ad_desc  called <CODE>callingtree.html</CODE>.
  !+ad_prob  Part of the calling tree code is commented out, as it currently fails
  !+ad_prob  to deal successfully with a program in which recursion is present.
  !+ad_call  autodoc_data
  !+ad_call  check_calltree_depth
  !+ad_call  print_calltree
  !+ad_call  close_file
  !+ad_call  open_file
  !+ad_call  read_line
  !+ad_call  reset_flags
  !+ad_call  section_arguments
  !+ad_call  section_arguments_cont
  !+ad_call  section_author
  !+ad_call  section_calls
  !+ad_call  section_contents
  !+ad_call  section_details
  !+ad_call  section_documentation
  !+ad_call  section_documentation_cont
  !+ad_call  section_history
  !+ad_call  section_history_cont
  !+ad_call  section_problems
  !+ad_call  section_status
  !+ad_call  section_status_cont
  !+ad_call  section_summary
  !+ad_call  section_type
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_hist  11/02/2009 PJK Added calling tree routines
  !+ad_stat  Okay
  !+ad_docs  None
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

     case ('null') ; continue
     case default ; continue

     end select

     command = 'null'

  end do

  call close_file(flag)

  !call check_calltree_depth
  call print_calltree(flag)

end program autodoc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine reset_flags(flag)

  !+ad_name  reset_flags
  !+ad_summ  Routine that resets all the command flags to indicate that
  !+ad_summ  the command sections are all 'closed'
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  flag : input/output flags object : set of flags to be reset
  !+ad_desc  This routine sets all the components of the given flag
  !+ad_desc  object to zero, indicating that all the command sections
  !+ad_desc  are now inactive, or closed.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

end subroutine reset_flags

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

integer function read_line(line,command)

  !+ad_name  read_line
  !+ad_summ  Routine that reads a line from standard input and parses it
  !+ad_summ  to find any autodoc commands and their associated arguments
  !+ad_type  Function returning integer
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : output string : extracted autodoc command argument
  !+ad_args  command : output string : extracted autodoc command
  !+ad_desc  This routine reads a line from the standard input channel
  !+ad_desc  and looks for the autodoc marker (the string of characters
  !+ad_desc  '+ad_') within it. If this marker is present, the routine
  !+ad_desc  extracts the specific autodoc command and its argument
  !+ad_desc  from the line, and returns them via the function's arguments.
  !+ad_desc  <P>The routine returns 0 if a line was read in correctly
  !+ad_desc  (irrespective of whether there was an autodoc command within it),
  !+ad_desc  or -1 if the end of the file has been reached.
  !+ad_prob  The fact that the function modifies its arguments is 
  !+ad_prob  deprecated in standard Fortran - this is a standard
  !+ad_prob  methodology by C programmers! Its use here merely keeps
  !+ad_prob  the main program tidy...
  !+ad_call  autodoc_data
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine open_file(line,flag)

  !+ad_name  open_file
  !+ad_summ  Routine that opens a new html file for autodoc to write to
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : root of the filename to be opened
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine opens a new html file for autodoc to use for the
  !+ad_desc  current subprogram's documentation. The filename to be used
  !+ad_desc  is the string contained within the <CODE>line</CODE> argument
  !+ad_desc  followed by <CODE>.html</CODE>.
  !+ad_desc  If another html file is already open, this is closed first.
  !+ad_desc  <P>After opening the new file, the header for the html is
  !+ad_desc  written to it, followed by a title line.
  !+ad_desc  <P>Finally, a new entry is added to the calling tree.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_file
  !+ad_call  header
  !+ad_call  new_parent
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_hist  11/02/2009 PJK Added call to new_parent
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use autodoc_data

  implicit none

  !  Arguments

  character(len=lenmax), intent(in) :: line
  type(flags), intent(inout) :: flag

  !  Local variables

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

  !  Add a new 'parent' to the calling tree

  call new_parent(line)

end subroutine open_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine close_file(flag)

  !+ad_name  close_file
  !+ad_summ  Routine that completes and closes an html file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine closes an html file being used by autodoc.
  !+ad_desc  First, any currently open command sections are closed.
  !+ad_desc  Then the html footer is written to the file.
  !+ad_desc  Finally, the file is closed and the command flags are reset.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  footer
  !+ad_call  reset_flags
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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
     call reset_flags(flag)
  end if

end subroutine close_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine write_to_file(line,flag)

  !+ad_name  write_to_file
  !+ad_summ  Routine that writes a line of text to the current html file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : text to be written
  !+ad_args  flag : input flags object : set of command flags
  !+ad_desc  This routine simply writes a line of text to the current
  !+ad_desc  html file.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine close_sections(flag)

  !+ad_name  close_sections
  !+ad_summ  Routine that closes any currently-open command sections
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  Some of the autodoc commands result in an un-numbered list
  !+ad_desc  of bullet points being created in the html file. When a new
  !+ad_desc  command is encountered, or when the file is to be closed,
  !+ad_desc  any un-numbered lists currently in use must be closed with the
  !+ad_desc  relevant html tag. This routine ensures that this happens, and
  !+ad_desc  resets the relevant flag accordingly.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

end subroutine close_sections

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_summary(line,flag)

  !+ad_name  section_summary
  !+ad_summ  Routine that acts on a 'summ' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'summ' command is encountered. 
  !+ad_desc  If one is not already open, a 'Summary' section is started
  !+ad_desc  by writing the relevant section header line to the html file.
  !+ad_desc  The given line of text is then written to the html file.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_type(line,flag)

  !+ad_name  section_type
  !+ad_summ  Routine that acts on a 'type' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'type' command is encountered. 
  !+ad_desc  If one is not already open, a 'Type' section is started
  !+ad_desc  by writing the relevant section header line to the html file.
  !+ad_desc  The given line of text is then written to the html file.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_details(line,flag)

  !+ad_name  section_details
  !+ad_summ  Routine that acts on a 'desc' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'desc' command is encountered. 
  !+ad_desc  If one is not already open, a 'Details' section is started
  !+ad_desc  by writing the relevant section header line to the html file.
  !+ad_desc  The given line of text is then written to the html file.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_problems(line,flag)

  !+ad_name  section_problems
  !+ad_summ  Routine that acts on a 'prob' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'prob' command is encountered. 
  !+ad_desc  If one is not already open, a 'Known Issues' section is started
  !+ad_desc  by writing the relevant section header line to the html file.
  !+ad_desc  The given line of text is then written to the html file.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_contents(line,flag)

  !+ad_name  section_contents
  !+ad_summ  Routine that acts on a 'cont' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'cont' command is encountered. 
  !+ad_desc  If one is not already open, a 'Contents' section is started
  !+ad_desc  by writing the relevant section header line to the html file,
  !+ad_desc  and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument (which must contain a single word
  !+ad_desc  without spaces) is then written to the html file as a list item,
  !+ad_desc  becoming a hypertext link to a file of the same name as the
  !+ad_desc  <CODE>line</CODE> string itself followed by <CODE>.html</CODE>.
  !+ad_desc  However, if <CODE>line</CODE> contains 'None' or 'N/A', then
  !+ad_desc  this is written out instead, without becoming a link to a file.
  !+ad_desc  <P>Finally, a 'child' entry for the current subprogram is added
  !+ad_desc  to the calling tree.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  new_child
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_hist  11/02/2009 PJK Added call to new_child
  !+ad_stat  Okay
  !+ad_docs  None
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
     string = html_link_open//trim(line)//'.html'//html_link_mid//trim(line)//html_link_close
     call write_to_file(string,flag)

     !  Add a new 'child' to the calling tree

     call new_child(line)

  end if

end subroutine section_contents

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_arguments(line,flag)

  !+ad_name  section_arguments
  !+ad_summ  Routine that acts on an 'args' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if an 'args' command is encountered. 
  !+ad_desc  If one is not already open, an 'Arguments' section is started
  !+ad_desc  by writing the relevant section header line to the html file,
  !+ad_desc  and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a list item.
  !+ad_desc  <P>If a description of the argument extends over more than
  !+ad_desc  one line of the source file, an 'argc' command should be used
  !+ad_desc  instead for continuation lines, otherwise a new bullet will be
  !+ad_desc  written to the html file, making the output look strange.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_arguments_cont(line,flag)

  !+ad_name  section_arguments_cont
  !+ad_summ  Routine that acts on an 'argc' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if an 'argc' command is encountered. 
  !+ad_desc  If one is not already open, an 'Arguments' section is started
  !+ad_desc  by writing the relevant section header line to the html file,
  !+ad_desc  and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a continuation of a list item.
  !+ad_desc  <P>The 'argc' command should be used to continue an 'args'
  !+ad_desc  command if the description of the argument extends over more than
  !+ad_desc  one line of the source file, to prevent extra bullets being
  !+ad_desc  written to the html file, making the output look strange.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_calls(line,flag)

  !+ad_name  section_calls
  !+ad_summ  Routine that acts on a 'call' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'call' command is encountered. 
  !+ad_desc  If one is not already open, a 'Routines/Modules Called' section
  !+ad_desc  is started by writing the relevant section header line to the
  !+ad_desc  html file, and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument (which must contain a single word
  !+ad_desc  without spaces) is then written to the html file as a list item,
  !+ad_desc  becoming a hypertext link to a file of the same name as the
  !+ad_desc  <CODE>line</CODE> string itself followed by <CODE>.html</CODE>.
  !+ad_desc  However, if <CODE>line</CODE> contains 'None' or 'N/A', then
  !+ad_desc  this is written out instead, without becoming a link to a file.
  !+ad_desc  <P>Finally, a 'child' entry for the current subprogram is added
  !+ad_desc  to the calling tree.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  new_child
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_hist  11/02/2009 PJK Added call to new_child
  !+ad_stat  Okay
  !+ad_docs  None
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
     string = html_link_open//trim(line)//'.html'//html_link_mid//trim(line)//html_link_close
     call write_to_file(string,flag)

     !  Add a new 'child' to the calling tree

     call new_child(line)

  end if

end subroutine section_calls

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_history(line,flag)

  !+ad_name  section_history
  !+ad_summ  Routine that acts on a 'hist' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'hist' command is encountered. 
  !+ad_desc  If one is not already open, a 'History' section is started
  !+ad_desc  by writing the relevant section header line to the html file,
  !+ad_desc  and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a list item.
  !+ad_desc  <P>If a description of a given history item extends over more than
  !+ad_desc  one line of the source file, a 'hisc' command should be used
  !+ad_desc  instead for continuation lines, otherwise a new bullet will be
  !+ad_desc  written to the html file, making the output look strange.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_history_cont(line,flag)

  !+ad_name  section_history_cont
  !+ad_summ  Routine that acts on a 'hisc' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'hisc' command is encountered. 
  !+ad_desc  If one is not already open, a 'History' section is started
  !+ad_desc  by writing the relevant section header line to the html file,
  !+ad_desc  and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a continuation of a list item.
  !+ad_desc  <P>The 'hisc' command should be used to continue a 'hist'
  !+ad_desc  command if the description of a given history item extends over
  !+ad_desc  more than one line of the source file, to prevent extra bullets
  !+ad_desc  being written to the html file, making the output look strange.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_status(line,flag)

  !+ad_name  section_status
  !+ad_summ  Routine that acts on a 'stat' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'stat' command is encountered. 
  !+ad_desc  If one is not already open, a 'Status' section is started
  !+ad_desc  by writing the relevant section header line to the html file,
  !+ad_desc  and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a list item.
  !+ad_desc  <P>If a description of the status extends over more than
  !+ad_desc  one line of the source file, a 'stac' command should be used
  !+ad_desc  instead for continuation lines, otherwise a new bullet will be
  !+ad_desc  written to the html file, making the output look strange.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_status_cont(line,flag)

  !+ad_name  section_status_cont
  !+ad_summ  Routine that acts on a 'stac' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'stac' command is encountered. 
  !+ad_desc  If one is not already open, a 'Status' section is started
  !+ad_desc  by writing the relevant section header line to the html file,
  !+ad_desc  and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a continuation of a list item.
  !+ad_desc  <P>The 'stac' command should be used to continue a 'stat'
  !+ad_desc  command if the description of the status extends over more than
  !+ad_desc  one line of the source file, to prevent extra bullets being
  !+ad_desc  written to the html file, making the output look strange.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_author(line,flag)

  !+ad_name  section_author
  !+ad_summ  Routine that acts on an 'auth' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if an 'auth' command is encountered. 
  !+ad_desc  If one is not already open, an 'Author' section is started
  !+ad_desc  by writing the relevant section header line to the html file,
  !+ad_desc  and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a list item.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_documentation(line,flag)

  !+ad_name  section_documentation
  !+ad_summ  Routine that acts on a 'docs' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'docs' command is encountered. 
  !+ad_desc  If one is not already open, a 'Further Documentation' section
  !+ad_desc  is started by writing the relevant section header line to the
  !+ad_desc  html file, and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a list item.
  !+ad_desc  <P>If a description of a piece of documentation extends over
  !+ad_desc  more than one line of the source file, a 'docc' command should
  !+ad_desc  be used instead for continuation lines, otherwise a new bullet
  !+ad_desc  will be written to the html file, making the output look strange.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine section_documentation_cont(line,flag)

  !+ad_name  section_documentation_cont
  !+ad_summ  Routine that acts on a 'docc' command
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  line : input string : line of text to be written
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine is called if a 'docc' command is encountered. 
  !+ad_desc  If one is not already open, a 'Further Documentation' section
  !+ad_desc  is started by writing the relevant section header line to the
  !+ad_desc  html file, and an un-numbered list (of bullet points) is started.
  !+ad_desc  The <CODE>line</CODE> argument is then written to the html file
  !+ad_desc  as a continuation of a list item.
  !+ad_desc  <P>The 'docc' command should be used to continue a 'docs'
  !+ad_desc  command if the description of a piece of documentation extends
  !+ad_desc  over more than one line of the source file, to prevent extra
  !+ad_desc  bullets being written to the html file, making the output look
  !+ad_desc  strange.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  close_sections
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine header(flag)

  !+ad_name  header
  !+ad_summ  Routine that writes the header of the html file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine writes the header of the newly-opened html file.
  !+ad_desc  If file <CODE>adheader.src</CODE> exists in the current working
  !+ad_desc  directory it is used as the html source for the header; this
  !+ad_desc  can be modified as necessary to provide links to other files,
  !+ad_desc  or to customise the html to local conditions, etc.
  !+ad_desc  Otherwise, a plain header is written to the html file.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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
  close(unit=hfunit)

end subroutine header

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine footer(flag)

  !+ad_name  footer
  !+ad_summ  Routine that writes the footer of the html file
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine writes the footer of the currently-open html file.
  !+ad_desc  If file <CODE>adfooter.src</CODE> exists in the current working
  !+ad_desc  directory it is used as the html source for the footer; this
  !+ad_desc  can be modified as necessary to provide links to other files,
  !+ad_desc  or to customise the html to local conditions, etc.
  !+ad_desc  Otherwise a plain footer is written to the html file.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  write_to_file
  !+ad_hist  20/04/2006 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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
  close(unit=hfunit)

end subroutine footer

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine new_parent(name)

  !+ad_name  new_parent
  !+ad_summ  Routine that adds a new entry to the calling tree list
  !+ad_summ  if it is not already present
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  name : input string : name of routine to be added to list
  !+ad_desc  This routine adds a new entry to the master list of routines,
  !+ad_desc  if a routine of the given name is not already present in the
  !+ad_desc  linked list. 
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  calltree_data
  !+ad_hist  11/02/2009 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine new_child(name)

  !+ad_name  new_child
  !+ad_summ  Routine that adds a new child to the calling tree list
  !+ad_summ  if it is not already present
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  name : input string : name of routine to be added to list
  !+ad_desc  This routine adds a new entry to the current subprogram's list
  !+ad_desc  of children, and adds the current subprogram to the list of the
  !+ad_desc  child's parents.
  !+ad_desc  <P>If the child does not yet exist in the master linked list
  !+ad_desc  it is added to this list first.
  !+ad_prob  None
  !+ad_call  autodoc_data
  !+ad_call  calltree_data
  !+ad_hist  11/02/2009 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine check_calltree_depth

  !+ad_name  check_calltree_depth
  !+ad_summ  Routine that checks each routine's depth within the calling tree
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  This routine loops through all the entries in the master calling
  !+ad_desc  tree linked list and checks that each entry is 'deeper' than all
  !+ad_desc  its parents. A routine's 'depth' is zero if nothing calls it, and
  !+ad_desc  is at least one more than the depth of any routine that directly
  !+ad_desc  calls it. This is evaluated by continually looping through the
  !+ad_desc  list of entries, checking their depth against those of their
  !+ad_desc  parents, and adjusting their depth if it is not consistent. The
  !+ad_desc  task is complete only when a loop does not make any depth
  !+ad_desc  adjustments.
  !+ad_desc  <P>In many programs, the calling tree is straightforward,
  !+ad_desc  as in the following example:
  !+ad_desc  <P>a calls b and c; b calls c and d; c calls d; d calls e
  !+ad_desc  <PRE>
  !+ad_desc  depth: 0    1    2    3    4
  !+ad_desc  .      a -- b ------- d -- e
  !+ad_desc  .       \    \       /       
  !+ad_desc  .        \----\- c -/        
  !+ad_desc  </PRE>
  !+ad_desc  However, in the case of programs which contain any
  !+ad_desc  recursive calls (a calls b calls c calls a, for instance)
  !+ad_desc  the depth is badly defined and this routine fails to deal with
  !+ad_desc  it correctly at present.
  !+ad_prob  Cases with recursion need to be sorted out before this routine
  !+ad_prob  is used in anger
  !+ad_call  autodoc_data
  !+ad_call  calltree_data
  !+ad_hist  11/02/2009 PJK Initial version
  !+ad_stat  Not currently used because of the problem with recursive programs
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine print_calltree(flag)

  !+ad_name  print_calltree
  !+ad_summ  Routine that prints the calling and called-by trees
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  flag : input/output flags object : set of command flags
  !+ad_desc  This routine runs through the master list of routines
  !+ad_desc  stored in the calling tree, and writes out an html file
  !+ad_desc  called <A HREF="callingtree.html"><CODE>callingtree.html</CODE></A>
  !+ad_desc  which lists each routine's parents and children.
  !+ad_prob  Output in a 'tree' form is not yet possible due to problems
  !+ad_prob  with the calculation of a routine's depth within the calling
  !+ad_prob  tree if a program contains any recursion - see
  !+ad_prob  <A HREF="check_calltree_depth.html">
  !+ad_prob  <CODE>check_calltree_depth</CODE</A>.
  !+ad_call  autodoc_data
  !+ad_call  calltree_data
  !+ad_call  close_file
  !+ad_call  descend_tree
  !+ad_call  header
  !+ad_call  write_to_file
  !+ad_hist  11/02/2009 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

recursive subroutine descend_tree(subprogram)

  !+ad_name  descend_tree
  !+ad_summ  Routine that drills down the calling tree recursively to
  !+ad_summ  print its structure, correctly indented
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  subprogram : input/output routine type : 
  !+ad_desc  This routine writes out the calling tree using correctly
  !+ad_desc  indented branches from each entry to show the tree structure.
  !+ad_desc  The routine is recursive so that each child's child is
  !+ad_desc  accounted for.
  !+ad_prob  The routine works, but the results are misleading if
  !+ad_prob  recursion is present within the program! See above...
  !+ad_prob  <P>The output will need to be converted to html before it
  !+ad_prob  is used in anger.
  !+ad_call  autodoc_data
  !+ad_call  calltree_data
  !+ad_call  descend_tree
  !+ad_hist  11/02/2009 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
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
