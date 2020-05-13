program process
  ! Simplest possible program to call the Process subroutine
  ! This makes Python wrapping much easier, as can now just wrap process_module
  ! and call it from an equally simple Python script instead
  use process_module, only: process_subroutine
  
  call process_subroutine
end program process