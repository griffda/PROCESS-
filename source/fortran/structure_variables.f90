module structure_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the support structure
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

#ifdef use_intrinsic
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif

  implicit none

  public

  real(8) :: aintmass
  !! intercoil structure mass (kg)

  real(8) :: clgsmass
  !! gravity support structure for TF coil, PF coil and intercoil support systems (kg)

  real(8) :: coldmass
  !! total mass of components at cryogenic temperatures (kg)

  real(8) :: fncmass
  !! PF coil outer support fence mass (kg)

  real(8) :: gsmass
  !! reactor core gravity support mass (kg)

  contains

  subroutine init_structure_variables
    !! Initialise module variables
    implicit none

    aintmass = 0.0D0
    clgsmass = 0.0D0
    coldmass = 0.0D0
    fncmass = 0.0D0
    gsmass = 0.0D0
  end subroutine init_structure_variables
end module structure_variables