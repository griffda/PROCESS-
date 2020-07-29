! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module error_handling

  !! Error handling module for PROCESS
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module provides a centralised method for dealing with
  !! errors generated by PROCESS.
  !! <P>All possible informational/error messages are initialised
  !! via a call to <A HREF="initialise_error_list.html">
  !! <CODE>initialise_error_list</CODE></A>. Thereafter, any routine
  !! that needs to flag a message should call <A HREF="report_error.html">
  !! <CODE>report_error</CODE></A> with the relevant error identifier as
  !! the argument. Up to eight integer and eight floating-point diagnostic
  !! values may be saved by the user in arrays <CODE>idiags</CODE> and
  !! <CODE>fdiags</CODE>, respectively, for debugging purposes.
  !! <P>The list of messages reported during the course of a run
  !! may be displayed by calling routine
  !! <A HREF="show_errors.html"><CODE>show_errors</CODE></A>.
  !! <P>The <CODE>error_status</CODE> variable returns the highest severity
  !! level that has been encountered; if a severe error is flagged
  !! (level 3) the program is terminated immediately.
  !! None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  use, intrinsic :: iso_fortran_env, only: dp=>real64
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

  integer :: error_id = 0
  !! error_id : identifier for final message encountered

  !  Overall status

  integer :: error_status = ERROR_OKAY
  !! error_status : overall status flag for a run; on exit:<UL>
  !!                 <LI> 0  all okay
  !!                 <LI> 1  informational messages have been encountered
  !!                 <LI> 2  warning (non-fatal) messages have been encountered
  !!                 <LI> 3  severe (fatal) errors have occurred</UL>

  integer, parameter :: INT_DEFAULT = -999999
  real(dp), parameter :: FLT_DEFAULT = real(INT_DEFAULT, kind(1.0D0))

  !  Arrays for diagnostic output

  integer, dimension(8) :: idiags = INT_DEFAULT
  real(dp), dimension(8) :: fdiags = FLT_DEFAULT

  !  Individual error item
  !  int and float arrays may be useful to provide diagnostic information

  type :: error
     integer            :: level    !  severity level
     character(len=200) :: message  !  information string
     integer, dimension(8) :: idiags = INT_DEFAULT
     real(dp), dimension(8) :: fdiags = FLT_DEFAULT
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

    !! Initialises the informational/error message list
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine sets all the possible informational/error messages
    !! that may be used during the course of a run. Thus, it needs
    !! to be called during the initialisation phase.
    !! <P>The error messages are read in from a JSON-format file.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fson_library, only: fson_parse, fson_value, fson_get, fson_destroy 
    implicit none

    !  Arguments

    !  Local variables

    integer :: n_errortypes
    character(len=180) :: filename
    type(fson_value), pointer :: errorfile

    !  Obtain the root directory

#include "root.dir"

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Parse the json file

    filename = INSTALLDIR//'/utilities/errorlist.json'
    errorfile => fson_parse(trim(filename))

    !  Allocate memory for error_type array contents

    call fson_get(errorfile, "n_errortypes", n_errortypes)
    
    ! Guard against re-allocation
    if (allocated(error_type)) deallocate(error_type)
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

    !! Adds the latest error message to the list already specified
    !! author: P J Knight, CCFE, Culham Science Centre
    !! error_id : input integer : identifier (error_type element number)
    !! for the relevant error
    !! This routine should be called if a informational, warning
    !! or error message needs to be flagged.
    !! It uses a linked list (see references) to provide
    !! an audit trail for any such messages during the program
    !! execution.
    !! <P>Up to eight integer and eight floating-point diagnostic
    !! values may be saved by the user in arrays <CODE>idiags</CODE> and
    !! <CODE>fdiags</CODE>, respectively, for debugging; these arrays must
    !! be assigned with up to eight values each prior to calling this routine.
    !! <P>The <CODE>error_status</CODE> variable returns the highest severity
    !! level that has been encountered; if a severe error is flagged
    !! (level 3) the program is terminated immediately.
    !! Introduction to Fortran 90/95, Stephen J, Chapman, pp.467-472,
    !! McGraw-Hill, ISBN 0-07-115896-0
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

! SJP Issue #867
! Remove consecutive identical error messages

       if (error_tail%id == error_id) return

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

    !! Reports all informational/error messages encountered
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This routine provides a summary audit trail of all the errors
    !! encountered during the program's execution.
    !! Introduction to Fortran 90/95, Stephen J, Chapman, pp.467-472,
    !! McGraw-Hill, ISBN 0-07-115896-0
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use constants, only: iotty, nout
    use process_output, only: oblnkl, oheadr, ocmmnt, ovarin 
    implicit none

    !  Arguments

    !  Local variables

    type (error_list_item), pointer :: ptr
    integer :: i
    character(len=50) :: status_message

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(iotty,'Errors and Warnings')
    call oheadr(nout,'Errors and Warnings')
    call ocmmnt(nout,'(See top of file for solver errors and warnings.)')

    select case (error_status)
    case (0)
        status_message = 'No messages'
    case (1)
        status_message = 'Information messages only'
    case (2)
        status_message = 'Warning messages'
    case (3)
        status_message = 'Errors'
    case default
        status_message = 'Incorrect value of error_status'
    end select

    call ocmmnt(nout,'PROCESS status flag:   '//status_message)
    write(*,*)       'PROCESS status flag:   '//status_message
    call oblnkl(iotty)
    call ovarin(nout,'PROCESS error status flag','(error_status)',error_status)

    ptr => error_head

    if (.not.associated(ptr)) then
       call ovarin(nout,'Final error/warning identifier','(error_id)',error_id)
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
