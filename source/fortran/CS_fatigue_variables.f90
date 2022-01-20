module CS_fatigue_variables

#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif

  implicit none

  public

  real(dp) :: residual_sig_hoop
  !! residual hoop stress in strucutal material (Pa) 
  
  real(dp) :: N_cycle
  !! Allowable number of cycles for CS
  
  real(dp) :: t_crack_radial
  !! Initial depth of crack in thickness of conduit (m)

  real(dp) :: t_crack_vertical
  !! Inital vertical crack size (m)

  real(dp) :: t_structural_radial
  !! Thickness of CS conductor conduit (m)
  
  real(dp) :: t_structural_vertical
  !! Vertical thickness of CS conductor conduit (m)

  contains

  subroutine init_CS_fatigue_variables
    !! Initialise module variables
    implicit none
    
    residual_sig_hoop = 2.4D8
    t_crack_radial = 6.0D-3
    t_crack_vertical = 2.0D-3
    N_cycle = 0.0D0
    t_structural_vertical = 0.022D0 
    t_structural_radial = 0.07D0 
      
  end subroutine init_CS_fatigue_variables

end module CS_fatigue_variables