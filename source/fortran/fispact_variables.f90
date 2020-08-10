module fispact_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the fispact routines
  !!
  !!### References
  !!
  !! -AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  !! Fispact arrays with 3 elements contain the results at the following times:
  !!
  !! - (1) - at end of component life
  !! - (2) - after 3 months cooling time
  !! - (3) - 100 years after end of plant life

  real(dp), dimension(3) :: bliact
  !! inboard blanket total activity (Bq)

  real(dp), dimension(3) :: bligdr
  !! inboard blanket total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: blihkw
  !! inboard blanket total heat output (kW)

  real(dp) :: bliizp
  !! inboard blanket integrated zone power / neutron

  real(dp) :: blimzp
  !! inboard blanket mean zone power density / neutron

  real(dp), dimension(3) :: bloact
  !! outboard blanket total activity (Bq)

  real(dp), dimension(3) :: blogdr
  !! outboard blanket total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: blohkw
  !! outboard blanket total heat output (kW)

  real(dp) :: bloizp
  !! outboard blanket integrated zone power / neutron

  real(dp) :: blomzp
  !! outboard blanket mean zone power density / neutron

  real(dp), dimension(3) :: fwiact
  !! inboard first wall total activity (Bq)

  real(dp), dimension(3) :: fwigdr
  !! inboard first wall total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: fwihkw
  !! inboard first wall total heat output (kW)

  real(dp) :: fwiizp
  !! inboard first wall integrated zone power / neutron

  real(dp) :: fwimzp
  !! inboard first wall mean zone power density/neutron

  real(dp), dimension(3) :: fwoact
  !! outboard first wall total activity (Bq)

  real(dp), dimension(3) :: fwogdr
  !! outboard first wall total gamma dose rate (Sv/hr)

  real(dp), dimension(3) :: fwohkw
  !! outboard first wall total heat output (kW)

  real(dp) :: fwoizp
  !! outboard first wall integrated zone power / neutron

  real(dp) :: fwomzp
  !! outboard first wall mean zone power density/neutron

  real(dp) :: fwtemp
  !! outboard first wall temperature after a LOCA (K)

  contains

  subroutine init_fispact_variables
    !! Initialise module variables
    implicit none

    bliact = 0.0D0
    bligdr = 0.0D0
    blihkw = 0.0D0
    bliizp = 0.0D0
    blimzp = 0.0D0
    bloact = 0.0D0
    blogdr = 0.0D0
    blohkw = 0.0D0
    bloizp = 0.0D0
    blomzp = 0.0D0
    fwiact = 0.0D0
    fwigdr = 0.0D0
    fwihkw = 0.0D0
    fwiizp = 0.0D0
    fwimzp = 0.0D0
    fwoact = 0.0D0
    fwogdr = 0.0D0
    fwohkw = 0.0D0
    fwoizp = 0.0D0
    fwomzp = 0.0D0
    fwtemp = 0.0D0
  end subroutine init_fispact_variables
end module fispact_variables