! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_ode
  !! Module containing divertor Kallenbach model
  !! author: M Kovari, CCFE, Culham Science Centre
  !! N/A
  !! This module contains the PROCESS Kallenbach divertor model
  !!

#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif

  use divertor_ode_var, only: nimp
  use constants, only: pi
  implicit none

  ! Module-level declarations !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!

  logical, save :: impurities_present(14)

  ! impurity element name - temporary

  ! relative ion mass
  ! Issue #501: change from 2 to 2.5
  real(dp) :: aplas

  ! ion mass [kg]
  real(dp) :: mi

  ! conversion from flux density [el/s] to Pascal [Molecules]
  ! see page 6 of paper.
  real(dp) :: fluxdens_to_pa

  ! Useful values (combinations of other constants/variables)
  real(dp) :: eightemi, eightemi48,  elEion
  real(dp), parameter :: degree=pi/180.0D0
  real(dp), parameter :: ln10=log(10.0D0)

  ! constant in thermal conductivity (equation 5) [J/(s m eV^7/2)]
  real(dp) :: kappa0

  ! neutral velocity along the flux bundle, groups 1 & 2 [m/s]
  real(dp) :: v01, v02

  ! Allowable absolute/relative error (UNUSED)
!   real(dp) :: abserr, relerr

  ! Circumference of plasma at outboard midplane and at target [m]
  real(dp) :: circumference_omp, circumference_target

  ! Circumference of normal to helical field line [m]
  real(dp) :: circumf_bu

  ! Flux bundle area perp. to B at target and at omp [m2]
  real(dp) :: area_target, area_omp

  ! Zeff for divertor region
  real(dp) :: zeff_div

  ! SOL radial thickness extrapolated to OMP [m]
  real(dp) :: sol_broadening

  ! Ratio: psep_kallenbach / Powerup
  real(dp), parameter :: seppowerratio=2.3D0

  real(dp) :: lengthofwidesol

end module divertor_ode
