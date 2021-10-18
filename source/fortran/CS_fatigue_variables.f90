module CS_fatigue_variables

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(8) :: residual_sig_hoop
  !! residual hoop stress in strucutal material (Pa) 
  
  real(8) :: N_cycle
  !! Number of cycles till CS fracture
  
  real(8) :: r_crack_size
  !! Inital radial crack size (m)

  real(8) :: z_crack_size
  !! Inital vertical crack size (m)

  contains

  subroutine init_watuse_variables
    !! Initialise module variables
    implicit none
    
    residual_sig_hoop = 2.4D8
    r_crack_size = 2.0D-3
    z_crack_size = 6.0D-3
    N_cycle = 0.0D0
      
  end subroutine init_watuse_variables

end module CS_fatigue_variables