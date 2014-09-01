! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module error_handling

  !+ad_name  error_handling
  !+ad_summ  Error handling module for PROCESS
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  initialise_error_list
  !+ad_cont  report_error
  !+ad_cont  show_errors
  !+ad_args  N/A
  !+ad_desc  This module provides a centralised method for dealing with
  !+ad_desc  errors generated by PROCESS.
  !+ad_desc  <P>All possible informational/error messages are initialised
  !+ad_desc  via a call to <A HREF="initialise_error_list.html">
  !+ad_desc  <CODE>initialise_error_list</CODE></A>. Thereafter, any routine
  !+ad_desc  that needs to flag a message should call <A HREF="report_error.html">
  !+ad_desc  <CODE>report_error</CODE></A> with the relevant error identifier as
  !+ad_desc  the argument. Up to eight integer and eight floating-point diagnostic
  !+ad_desc  values may be saved by the user in arrays <CODE>idiags</CODE> and
  !+ad_desc  <CODE>fdiags</CODE>, respectively, for debugging purposes.
  !+ad_desc  <P>The list of messages reported during the course of a run
  !+ad_desc  may be displayed by calling routine
  !+ad_desc  <A HREF="show_errors.html"><CODE>show_errors</CODE></A>.
  !+ad_desc  <P>The <CODE>error_status</CODE> variable returns the highest severity
  !+ad_desc  level that has been encountered; if a severe error is flagged
  !+ad_desc  (level 3) the program is terminated immediately.
  !+ad_prob  None
  !+ad_call  fson_library
  !+ad_call  process_output
  !+ad_hist  25/06/14 PJK Initial version
  !+ad_hist  09/07/14 PJK Added errors_on switch
  !+ad_hist  01/09/14 PJK Added use of FSON library to read in error list
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use fson_library
  use process_output

  implicit none

  private
  public :: errors_on, error_status, idiags, fdiags
  public :: initialise_error_list, report_error, show_errors

  !  Switch to turn error handling on
  !  Error reporting is turned off, until either a severe error is found, or
  !  during an output step.  Warnings during intermediate iteration steps
  !  may be premature and might clear themselves at the final converged point.

  logical :: errors_on = .false.

  !  Levels of severity

  integer, parameter :: ERROR_OKAY = 0
  integer, parameter :: ERROR_INFO = 1
  integer, parameter :: ERROR_WARN = 2
  integer, parameter :: ERROR_SEVERE = 3

  !+ad_vars  error_id : identifier for final message encountered
  integer :: error_id = 0

  !  Overall status

  !+ad_vars  error_status : overall status flag for a run; on exit:<UL>
  !+ad_varc                 <LI> 0  all okay
  !+ad_varc                 <LI> 1  informational messages have been encountered
  !+ad_varc                 <LI> 2  warning (non-fatal) messages have been encountered
  !+ad_varc                 <LI> 3  severe (fatal) errors have occurred</UL>
  integer :: error_status = ERROR_OKAY

  integer, parameter :: INT_DEFAULT = -999999
  real(kind(1.0D0)), parameter :: FLT_DEFAULT = real(INT_DEFAULT, kind(1.0D0))

  !  Arrays for diagnostic output

  integer, dimension(8) :: idiags = INT_DEFAULT
  real(kind(1.0D0)), dimension(8) :: fdiags = FLT_DEFAULT

  !  Individual error item
  !  int and float arrays may be useful to provide diagnostic information

  type :: error
     integer           :: level    !  severity level
     character(len=80) :: message  !  information string
     integer, dimension(8) :: idiags = INT_DEFAULT
     real(kind(1.0D0)), dimension(8) :: fdiags = FLT_DEFAULT
  end type error

  !  Individual element in an error list

  type :: error_list_item
     integer                         :: id    !  identifier
     type (error)                    :: data  !  error details
     type (error_list_item), pointer :: ptr   !  linked list pointer
  end type error_list_item

  !  Pointers to head and tail of the error list

  type (error_list_item), pointer :: error_head => null()
  type (error_list_item), pointer :: error_tail => null()

  !  List of messages

  type(error), allocatable, dimension(:) :: error_type

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialise_error_list

    !+ad_name  initialise_error_list
    !+ad_summ  Initialises the informational/error message list
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine sets all the possible informational/error messages
    !+ad_desc  that may be used during the course of a run. Thus, it needs
    !+ad_desc  to be called during the initialisation phase.
    !+ad_desc  <P>The error messages are read in from a JSON-format file.
    !+ad_prob  None
    !+ad_call  fson_destroy
    !+ad_call  fson_get
    !+ad_call  fson_parse
    !+ad_hist  25/06/14 PJK Initial version
    !+ad_hist  09/07/14 PJK Added errors 131-135
    !+ad_hist  29/07/14 PJK Added error 136
    !+ad_hist  30/07/14 PJK Modified 51, 63, 103
    !+ad_hist  31/07/14 PJK Modified 103 (now obsolete)
    !+ad_hist  19/08/14 PJK Added errors 137, 138
    !+ad_hist  01/09/14 PJK Added errors 139, 140
    !+ad_hist  01/09/14 PJK Changed wording of 135
    !+ad_hist  01/09/14 PJK Now uses JSON format to read in error list
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: n_errortypes
    character(len=80) :: filename
    type(fson_value), pointer :: errorfile

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Parse the json file
    !  (Need to consider a better way to specify the file location...)

    filename = '/home/pknight/process/bin/utilities/errorlist.json'
    !filename = 'errorlist.json'  !  for testing purposes...

    errorfile => fson_parse(trim(filename))

    !  Allocate memory for error_type array contents

    call fson_get(errorfile, "n_errortypes", n_errortypes)
    allocate(error_type(n_errortypes))

    error_type(:)%level = ERROR_OKAY
    error_type(:)%message = ' '

    !  Extract information arrays from the file

    call fson_get(errorfile, "errors", "level", error_type%level)
    call fson_get(errorfile, "errors", "message", error_type%message)

    !  Clean up

    call fson_destroy(errorfile)

  end subroutine initialise_error_list

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine report_error(error_id)

    !+ad_name  report_error
    !+ad_summ  Adds the latest error message to the list already specified
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  error_id : input integer : identifier (error_type element number)
    !+ad_argc                             for the relevant error
    !+ad_desc  This routine should be called if a informational, warning
    !+ad_desc  or error message needs to be flagged.
    !+ad_desc  It uses a linked list (see references) to provide
    !+ad_desc  an audit trail for any such messages during the program
    !+ad_desc  execution.
    !+ad_desc  <P>Up to eight integer and eight floating-point diagnostic
    !+ad_desc  values may be saved by the user in arrays <CODE>idiags</CODE> and
    !+ad_desc  <CODE>fdiags</CODE>, respectively, for debugging; these arrays must
    !+ad_desc  be assigned with up to eight values each prior to calling this routine.
    !+ad_desc  <P>The <CODE>error_status</CODE> variable returns the highest severity
    !+ad_desc  level that has been encountered; if a severe error is flagged
    !+ad_desc  (level 3) the program is terminated immediately.
    !+ad_prob  None
    !+ad_call  show_errors
    !+ad_hist  24/06/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Introduction to Fortran 90/95, Stephen J, Chapman, pp.467-472,
    !+ad_docc    McGraw-Hill, ISBN 0-07-115896-0
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: error_id

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Turn on error handling if a severe error has been encountered

    if (error_type(error_id)%level == ERROR_SEVERE) errors_on = .true.

    !  Error handling is only turned on during an output step, not during
    !  intermediate iteration steps

    if (.not.errors_on) then
       idiags = INT_DEFAULT
       fdiags = FLT_DEFAULT
       return
    end if

    if (.not.associated(error_head)) then
       allocate(error_head)
       error_tail => error_head
    else
       allocate(error_tail%ptr)
       error_tail => error_tail%ptr
    end if

    error_tail%id           = error_id
    error_tail%data%level   = error_type(error_id)%level
    error_tail%data%message = error_type(error_id)%message
    error_tail%data%idiags = idiags ; idiags = INT_DEFAULT
    error_tail%data%fdiags = fdiags ; fdiags = FLT_DEFAULT

    nullify (error_tail%ptr)

    !  Update the overall error status (highest severity level encountered)
    !  and stop the program if a severe error has occurred

    error_status = max(error_status, error_type(error_id)%level)

    if (error_status == ERROR_SEVERE) then
       call show_errors
       write(*,*) 'PROCESS stopping.'
       stop
    end if

  end subroutine report_error

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine show_errors

    !+ad_name  show_errors
    !+ad_summ  Reports all informational/error messages encountered
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine provides a summary audit trail of all the errors
    !+ad_desc  encountered during the program's execution.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  oheadr
    !+ad_call  ovarin
    !+ad_hist  24/06/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Introduction to Fortran 90/95, Stephen J, Chapman, pp.467-472,
    !+ad_docc    McGraw-Hill, ISBN 0-07-115896-0
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    type (error_list_item), pointer :: ptr
    integer :: i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(iotty,'Program Error Report')
    write(iotty,'(a,i1)') 'PROCESS error status flag (error_status) = ',error_status
    call oblnkl(iotty)

    call oheadr(nout,'Program Error Report')
    call ovarin(nout,'PROCESS error status flag','(error_status)',error_status)

    ptr => error_head

    if (.not.associated(ptr)) then
       call ovarin(nout,'Final error identifier','(error_id)',error_id)
       return
    end if

    write(*,*) 'ID  LEVEL  MESSAGE'

    output: do
       if (.not.associated(ptr)) exit output

       error_id = ptr%id
       write(nout,'(i3,t7,i3,t13,a80)') ptr%id,ptr%data%level,ptr%data%message
       write(*,   '(i3,t7,i3,t13,a80)') ptr%id,ptr%data%level,ptr%data%message

       if (any(ptr%data%idiags /= INT_DEFAULT)) then
          write(*,*) 'Integer diagnostic values for this error:'
          do i = 1,8
             if (ptr%data%idiags(i) /= INT_DEFAULT) &
                  write(*,'(i4,a,i14)') i,') ',ptr%data%idiags(i)
          end do
       end if
       if (any(ptr%data%fdiags /= FLT_DEFAULT)) then
          write(*,*) 'Floating point diagnostic values for this error:'
          do i = 1,8
             if (ptr%data%fdiags(i) /= FLT_DEFAULT) &
                  write(*,'(i4,a,1pe14.5)') i,') ',ptr%data%fdiags(i)
          end do
       end if
       write(*,*) ' '

       ptr => ptr%ptr
    end do output

    call ovarin(nout,'Final error identifier','(error_id)',error_id)

  end subroutine show_errors

end module error_handling

