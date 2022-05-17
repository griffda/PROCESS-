! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module costs_2015_module
   !! Module containing fusion power plant costing algorithms
   !! author: J Morris, CCFE, Culham Science Centre
   !! N/A
   !! This module contains the PROCESS fusion power plant costing model,
   !! based on ITER costs and the PROCESS costs paper
   !! PROCESS Costs Paper (M. Kovari, J. Morris)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   ! Import modules
#ifndef dp
   use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
   implicit none

   ! Precision variable
   integer, parameter :: double = 8

   ! Output variables
   integer :: ip, ofile

   ! Costs structure for scaling laws (scl)
   character*80, save, dimension(100) :: s_label
   ! Description, appears in OUT.DAT
   real(dp), save, dimension(100) :: s_kref
   ! Reference value of scaling parameter
   real(dp), save, dimension(100) :: s_k
   ! Actual value of scaling parameter K
   real(dp), save, dimension(100) :: s_cref
   ! Reference cost $
   real(dp), save, dimension(100) :: s_cost
   ! Actual cost $
   real(dp), save, dimension(100) :: s_cost_factor
   ! Multiplier f


   ! Scaling law array (unused entries will be zeroes)
   real(dp) :: total_costs, mean_electric_output, annual_electric_output, &
      maintenance

   ! Private module variables
   public :: ip, ofile, double, total_costs, s_label, &
      s_kref, s_k, s_cref, s_cost, s_cost_factor

   ! Public variables/subroutines
   ! public :: costs_2015

contains

   subroutine init_costs_2015
      !! Initialise module variables
      implicit none

      ip = 0
      ofile = 0
      total_costs = 0.0D0
      mean_electric_output = 0.0D0
      annual_electric_output = 0.0D0
      maintenance = 0.0D0
      ip = 0.0D0
      ofile = 0.0D0
      total_costs = 0.0D0
      ! Re-initialise entire array
      s_label = 'not used'
      s_kref = 0.0D0
      s_k = 0.0D0
      s_cref = 0.0D0
      s_cost = 0.0D0
      s_cost_factor = 0.0D0
   end subroutine init_costs_2015

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module costs_2015_module
