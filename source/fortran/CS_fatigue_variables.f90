module CS_fatigue_variables

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(8) :: residual_sig_hoop
  !! residual hoop stress in strucutal material (Pa) 
  
  real(8) :: N_cycle
  !! Number of cycles till CS fracture
  
  real(8) :: t_crack_radial
  !! Inital radial crack size (m)

  real(8) :: t_crack_vertical
  !! Inital vertical crack size (m)

  real(8) :: t_structural_radial
  !! CS structural radial thickness (m)
  
  real(8) :: t_structural_vertical
  !! CS structural vertical thickness (m)

  contains

  subroutine init_watuse_variables
    !! Initialise module variables
    implicit none
    
    residual_sig_hoop = 2.4D8
    t_crack_radial = 6.0D-3
    t_crack_vertical = 2.0D-3
    N_cycle = 0.0D0
    t_structural_vertical = 0.022D0 
    t_structural_radial = 0.07D0 
      
  end subroutine init_watuse_variables

end module CS_fatigue_variables