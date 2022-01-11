! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_ode_var
  !! Module containing divertor Kallenbach model
  !! author: M Kovari, CCFE, Culham Science Centre
  !! N/A
  !! This module contains the PROCESS Kallenbach divertor model
  !! 

  ! NEW VERSION! 29/6/18
  ! Using n as variable rather than Ptotal
  ! Y(5) = electron density (n) [1e20 m-3]
  ! yp(5) = dn/dx [1e20 m-3 m-1]
#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
  use impurity_radiation_module, only: nimp, imp_label, impurity_arr
  implicit none

  ! Module-level declarations !

  ! impurity concentrations in divertor zone
  real(dp), public :: impurity_concs(nimp)
contains

  subroutine init_divertor_ode_var
    !! Initialise module variables
    implicit none

    impurity_concs = 0.0D0
  end subroutine init_divertor_ode_var

end module divertor_ode_var
