! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module power_module

   !! Module containing heat/power transport and power balance routines
   !! author: P J Knight, CCFE, Culham Science Centre
   !! N/A
   !! This module contains routines for calculating the
   !! power supply requirements, heat transport system parameters
   !! and the power balance for a fusion power plant.
   !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifndef dp
   use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
   implicit none

   !  Precision variable
   integer, parameter :: double = 8

   !  Local variables
   real(kind=double) :: htpmwe_shld, htpmwe_div, htpmw_mech, pthermfw_blkt
   real(kind=double) :: htpmwe_fw_blkt
   real(kind=double) :: pthermdiv, pthermfw, pthermblkt, pthermshld
   real(kind=double) :: ppumpmw, pcoresystems, pdivfraction, delta_eta, qss, qac, qcl, qmisc

   !  Primary power to divertor factor
   integer, private :: iprimdiv

contains

   subroutine init_power_module
      !! Initialise module variables
      implicit none

      qmisc = 0.0D0
      qac = 0.0D0
      qcl = 0.0D0
      qss = 0.0D0
      htpmwe_shld = 0.0d0
      htpmwe_div = 0.0d0
      htpmw_mech = 0.0d0
      pthermfw_blkt = 0.0d0
      htpmwe_fw_blkt = 0.0d0
      pthermdiv = 0.0d0
      pthermfw = 0.0d0
      pthermblkt = 0.0d0
      pthermshld = 0.0d0
      ppumpmw = 0.0d0
      pcoresystems = 0.0d0
      pdivfraction = 0.0d0
      delta_eta = 0.0d0
      iprimdiv = 0.0d0
   end subroutine init_power_module

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module power_module
