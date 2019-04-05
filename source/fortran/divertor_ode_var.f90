! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_ode_var
  !+ad_name  divertor_ode
  !+ad_summ  Module containing divertor Kallenbach model
  !+ad_type  Module
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_args  N/A
  !+ad_desc  This module contains the PROCESS Kallenbach divertor model
  !+ad_prob  None
  !+ad_hist  25/01/17 MDK  Initial version of module
  !+ad_stat  Okay
  !+ad_docs

  ! NEW VERSION! 29/6/18
  ! Using n as variable rather than Ptotal
  ! Y(5) = electron density (n) [1e20 m-3]
  ! yp(5) = dn/dx [1e20 m-3 m-1]

  use impurity_radiation_module, only: nimp, imp_label, impurity_arr
  implicit none

  ! Module-level declarations !

  ! impurity concentrations in divertor zone
  real(kind(1.0D0)), public :: impurity_concs(nimp)
contains

end module divertor_ode_var
