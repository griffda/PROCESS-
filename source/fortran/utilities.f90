module utilities

  use iso_c_binding
contains
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real function process_value(x, y) result(ret_value)! bind(C, name="c_process_value_cpp")
   implicit none
   real, intent(in) :: x
   real, intent(in) :: y
   ret_value = x + y
end function process_value


real(c_double) function process_value_cpp(x, y) result(ret_value) bind(C, name="c_process_value_cpp")
   implicit none
   real(c_double), intent(in) :: x
   real(c_double), intent(in) :: y
   ret_value = x + y
end function process_value_cpp

  subroutine upper_case(string,start,finish) ! bind(C,name="utilities_upper_case")

    !+ad_name  upper_case
    !+ad_summ  Routine that converts a (sub-)string to uppercase
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  string : input string   : character string of interest
    !+ad_args  start  : optional input integer  : starting character for conversion
    !+ad_args  finish : optional input integer  : final character for conversion
    !+ad_desc  This routine converts the specified section of a string
    !+ad_desc  to uppercase. By default, the whole string will be converted.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_hist  12/04/11 PJK Made start,finish arguments optional
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(inout) :: string
    integer, optional, intent(in) :: start,finish

    !  Local variables

    character(len=1) :: letter
    character(len=27) :: upptab = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_'
    integer :: loop, i

    integer :: first, last

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (present(start)) then
       first = start
    else
       first = 1
    end if

    if (present(finish)) then
       last = finish
    else
       last = len(string)
    end if

    if (first <= last) then
       do loop = first,last
          letter = string(loop:loop)
          i = index('abcdefghijklmnopqrstuvwxyz_',letter)
          if (i > 0) string(loop:loop) = upptab(i:i)
       end do
    end if

  end subroutine upper_case

end module utilities
