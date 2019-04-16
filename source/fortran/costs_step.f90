! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module costs_step_module

  !+ad_name  costs_step_module
  !+ad_summ  Module containing STEP fusion power plant costing algorithms
  !+ad_type  Module
  !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
  !+ad_cont  costs_step
  !+ad_cont  step_a20
  !+ad_cont  step_a21
  !+ad_cont  step_a22
  !+ad_cont  step_a2201
  !+ad_cont  step_a2202
  !+ad_cont  step_a2203
  !+ad_cont  step_a2204
  !+ad_cont  step_a2205
  !+ad_cont  step_a2206
  !+ad_cont  step_a2207
  !+ad_cont  step_a23
  !+ad_cont  step_a24
  !+ad_cont  step_a25
  !+ad_cont  coelc_step
  !+ad_args  N/A
  !+ad_desc  This module contains the STEP fusion power plant costing model,
  !+ad_desc  developed by Nizar Ben Ayed, Stuart Muldrew and Tim Hender, based
  !+ad_desc  on the STARFIRE costing framework.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  buildings_variables
  !+ad_call  constants
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables

  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  process_output
  !+ad_call  pulse_variables

  !+ad_call  structure_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  vacuum_variables
  !+ad_hist  28/02/19 SIM Initial version of module
  !+ad_stat  Okay
  !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use constants
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables

  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use process_output
  use pulse_variables

  use structure_variables
  use tfcoil_variables
  use times_variables
  use vacuum_variables

  implicit none

  private
  public :: costs_step

  !  Various cost account values (M$)

  real(kind(1.0D0)) :: step20, step21, step22, step23, step24, step25, &
  step91, step92, step93, fwblkcost

  ! Scaling Properties

  real(kind(1.0D0)) :: vfi, vfi_star, ptherm_star, pinjmw_star, fwarea_star, &
  rmajor_star, rminor_star, pth

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine costs_step(outfile,iprint)

    !+ad_name  costs_step
    !+ad_summ  STEP cost accounting for a fusion power plant
    !+ad_type  Subroutine
    !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine performs the cost accounting for a fusion power plant.
    !+ad_desc  The direct costs are calculated based on parameters input
    !+ad_desc  from other sections of the code.
    !+ad_desc  <P>The code is arranged in the order of the standard accounts.
    !+ad_prob  None
    !+ad_call  step_a20
    !+ad_call  step_a21
    !+ad_call  step_a22
    !+ad_call  step_a2201
    !+ad_call  step_a2202
    !+ad_call  step_a2203
    !+ad_call  step_a2204
    !+ad_call  step_a2205
    !+ad_call  step_a2206
    !+ad_call  step_a2207
    !+ad_call  step_a23
    !+ad_call  step_a24
    !+ad_call  step_a25
    !+ad_call  coelc_step
    !+ad_call  oblnkl
    !+ad_call  ocosts
    !+ad_call  oheadr
    !+ad_call  oshead
    !+ad_hist  28/02/19 SIM Initial version
    !+ad_stat  Okay
    !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Fusion Island Volume as defined by Sheffield & Milora (2016)

    vfi = pi * (rtot + 0.5D0*tfthko)**2 * (hpfu + hmax + tfcth)
    pth = powfmw + emultmw + pinjwp

    !  STARFIRE Reference Value

!    vfi_star = 5.1D3                    ! Volume of Fusion Island (m3)
    vfi_star = 6.737D3                  ! Volume of Fusion Island (m3)
    ptherm_star = 4.15D3                ! Thermal Power (MW)
    pinjmw_star = 9.04D1                ! Auxiliary Power (MW)
!    fwarea_star = 7.8D2                 ! First Wall Area (m2)
    fwarea_star = 9.42D2                ! First Wall Area (m2)
    rmajor_star = 7.0D0                 ! Major Radius (m)
    rminor_star = rmajor_star / 3.6D0   ! Minor Radius (m)

    if ((iprint==1).and.(output_costs == 1)) then
      call oheadr(outfile,'STEP Costing Model (1980 US$)')
    end if
    
    !  Account 20 : Land and Rights

    call step_a20(outfile,iprint)

    !  Account 21 : Building and Site Service Infrastructure

    call step_a21(outfile,iprint)

    !  Account 22 : Reactor Plant Equipment

    call step_a22(outfile,iprint)

    !  Account 23 : Turbine Plant Equipment

    call step_a23(outfile,iprint)

    !  Account 24 : Electric Plant Equipment

    call step_a24(outfile,iprint)

    !  Account 25 : Miscellaneous Plant Equipment

    call step_a25(outfile,iprint)


    !  Total plant direct cost

    cdirt = step20 + step21 + step22 + step23 + step24 + step25

    ! Account 91 : Construction Facilities, Equipment and Services
    step91 = 1.0D-1 * cdirt

    ! Account 92 : Engineering and Costruction Management Services
    step92 = 8.0D-2 * cdirt

    ! Account 93 : Other Costs
    step93 = 5.0D-2 * cdirt

    !  Constructed cost

    concost = cdirt + step91 + step92 + step93

    if ((iprint==1).and.(output_costs == 1)) then
      call oshead(outfile,'Plant Direct Cost')
      call ocosts(outfile,'(cdirt)','Plant direct cost (M$)',cdirt)

      call oshead(outfile,'Indirect Cost')
      call ocosts(outfile,'(step91)','Construction Facilities, Equipment and Services (10%) (M$)',step91)
      call ocosts(outfile,'(step92)','Engineering and Costruction Management Services (8%) (M$)',step92)
      call ocosts(outfile,'(step93)','Other Costs (5%) (M$)',step93)

      call oshead(outfile,'Constructed Cost')
      call ocosts(outfile,'(concost)','Constructed Cost (M$)',concost)
    end if

    !  Cost of electricity

    if ((ireactor == 1).and.(ipnet == 0)) call coelc_step(outfile,iprint)

  end subroutine costs_step


  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine step_a20(outfile,iprint)

   !+ad_name  step_a20
   !+ad_summ  Account 20 : Land and Rights
   !+ad_type  Subroutine
   !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
   !+ad_cont  N/A
   !+ad_args  None
   !+ad_desc  This routine evaluates the Account 20 (Land and Rights)
   !+ad_desc  costs.
   !+ad_prob  None
   !+ad_call  oblnkl
   !+ad_call  ocosts
   !+ad_call  oshead
   !+ad_hist  28/02/19 SIM Initial version
   !+ad_stat  Okay
   !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   integer, intent(in) :: iprint,outfile

   !  Local variables

   real(kind(1.0D0)):: step2001, step2002

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   step20 = 0.0D0
   
   ! 21.01 Land
   ! Original STARFIRE value, no scaling
   step2001 = 3.0D0
   step20 = step20 + step2001

   ! 21.02 Site Preparation
   ! Original STARFIRE value, no scaling
   step2002 = 3.0D-1
   step20 = step20 + step2002

   if ((iprint==1).and.(output_costs == 1)) then
     call oshead(outfile,'20. Land and Rights')
     call ocosts(outfile,'(step2001)','Land (M$)', step2001)
     call ocosts(outfile,'(step2002)','Site Preparation (M$)', step2002)
     call oblnkl(outfile)
     call ocosts(outfile,'(step20)','Total Account 20 Cost (M$)', step20)
   end if

 end subroutine step_a20

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine step_a21(outfile,iprint)

   !+ad_name  step_a21
   !+ad_summ  Account 21 : Building and Site Service Infrastructure
   !+ad_type  Subroutine
   !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
   !+ad_cont  N/A
   !+ad_args  None
   !+ad_desc  This routine evaluates the Account 21 (Building and Site
   !+ad_desc  Service Infrastructure) costs.
   !+ad_prob  None
   !+ad_call  oblnkl
   !+ad_call  ocosts
   !+ad_call  oshead
   !+ad_hist  28/02/19 SIM Initial version
   !+ad_stat  Okay
   !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   integer, intent(in) :: iprint,outfile

   !  Local variables

   real(kind(1.0D0)):: &
   step2101, step2102, step2103, step2104, step2105, step2106, &
   step2107, step2108, step2109, step2110, step2111, step2112, &
   step2113, step2114, step2115, step2116, step2117, step2198, &
   step2199

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   step21 = 0.0D0
   
   ! 21.01 Site Improvements
   ! Original STARFIRE value, no scaling
   step2101 = 1.115D1
   step21 = step21 + step2101

   ! 21.02 Reactor Building
   ! Original STARFIRE value, scaling with fusion island volume
   step2102 = 1.5744D2 * (vfi / vfi_star)**(2.0D0 / 3.0D0)
   step21 = step21 + step2102

   ! 21.03 Turbine Building
   ! Original STARFIRE value, scaling with thermal power
   step2103 = 3.592D1 * (pth/ ptherm_star)**0.6D0
   step21 = step21 + step2103

   ! 21.04 Cooling System Structures
   ! Original STARFIRE value, scaling with thermal power
   step2104 = 7.96D0 * (pth/ ptherm_star)**0.6D0
   step21 = step21 + step2104

   ! 21.05 Electrical Equipment and Power Supply Building
   ! Original STARFIRE value, scaling with thermal power
   step2105 = 9.16D0 * (pth / ptherm_star)**0.6D0
   step21 = step21 + step2105

   ! 21.06 Auxiliary Services Building
   ! Original STARFIRE value, scaling with thermal power
   step2106 = 3.26D0 * (pth / ptherm_star)**0.6D0  
   step21 = step21 + step2106

   ! 21.07 Hot Cell
   ! Original STARFIRE value, scaling with fusion island volume
   step2107 = 5.369D1 * (vfi / vfi_star)**(2.0D0/3.0D0)
   step21 = step21 + step2107

   ! 21.08 Reactor Service Building
   ! Original STARFIRE value, scaling with fusion island volume
   step2108 = 1.88D0 * (vfi / vfi_star)**(2.0D0/3.0D0)
   step21 = step21 + step2108

   ! 21.09 Service Water Building
   ! Original STARFIRE value, scaling with thermal power
   step2109 = 6.6D-1 * (pth / ptherm_star)**0.6D0  
   step21 = step21 + step2109

   ! 21.10 Fuel Handling and Storage Building
   ! Original STARFIRE value, scaling with thermal power
   step2110 = 8.63D0 * (pth / ptherm_star)**0.6D0  
   step21 = step21 + step2110

   ! 21.11 Control Room
   ! Original STARFIRE value, no scaling
   step2111 = 3.1D0 
   step21 = step21 + step2111

   ! 21.12 AC Power Supply Building
   ! Original STARFIRE value, no scaling
   step2112 = 2.05D0 
   step21 = step21 + step2112

   ! 21.13 Admin Building
   ! Original STARFIRE value, no scaling
   step2113 = 8.7D-1
   step21 = step21 + step2113

   ! 21.14 Site Service
   ! Original STARFIRE value, scaling with thermal power
   step2114 = 8.7D-1 * (pth / ptherm_star)**0.6D0  
   step21 = step21 + step2114

   ! 21.15 Cryogenics and Inert Gas Storage Building
   ! Original STARFIRE value, no scaling
   step2115 = 9.1D-1
   step21 = step21 + step2115

   ! 21.16 Security Building
   ! Original STARFIRE value, no scaling
   step2116 = 3.1D-1
   step21 = step21 + step2116

   ! 21.17 Ventilation Stack
   ! Original STARFIRE value, scaling with thermal power
   step2117 = 1.81D0 * (pth / ptherm_star)**0.6D0  
   step21 = step21 + step2117

   ! 21.98 Spares
   ! Original STARFIRE value, no scaling
   step2198 = 1.96D0 
   step21 = step21 + step2198

   ! 21.99 Contingency
   ! STARFIRE 15%
   step2199 = 1.5D-1 * step21
   step21 = step21 + step2199

   if ((iprint==1).and.(output_costs == 1)) then
     call oshead(outfile,'21. Building and Site Service Infrastructure')
     call ocosts(outfile,'(step2101)','Site Improvements (M$)', step2101)
     call ocosts(outfile,'(step2102)','Reactor Building (M$)', step2102)
     call ocosts(outfile,'(step2103)','Turbine Building (M$)', step2103)
     call ocosts(outfile,'(step2104)','Cooling System Structures (M$)', step2104)
     call ocosts(outfile,'(step2105)','Electrical Equipment and Power Supply Building (M$)', step2105)
     call ocosts(outfile,'(step2106)','Auxiliary Services Building (M$)', step2106)
     call ocosts(outfile,'(step2107)','Hot Cell (M$)', step2107)
     call ocosts(outfile,'(step2108)','Reactor Service Building (M$)', step2108)
     call ocosts(outfile,'(step2109)','Service Water Building (M$)', step2109)
     call ocosts(outfile,'(step2110)','Fuel Handling and Storage Building (M$)', step2110)
     call ocosts(outfile,'(step2111)','Control Room (M$)', step2111)
     call ocosts(outfile,'(step2112)','AC Power Supply Building (M$)', step2112)
     call ocosts(outfile,'(step2113)','Admin Building (M$)', step2113)
     call ocosts(outfile,'(step2114)','Site Service (M$)', step2114)
     call ocosts(outfile,'(step2115)','Cryogenics and Inert Gas Storage Building (M$)', step2115)
     call ocosts(outfile,'(step2116)','Security Building (M$)', step2116)
     call ocosts(outfile,'(step2117)','Ventilation Stack (M$)', step2117)
     call ocosts(outfile,'(step2198)','Spares (M$)', step2198)
     call ocosts(outfile,'(step2199)','Contingency (M$)', step2199)
     call oblnkl(outfile)
     call ocosts(outfile,'(step21)','Total Account 21 Cost (M$)', step21)
   end if

 end subroutine step_a21

 subroutine step_a22(outfile,iprint)

     !+ad_name  step_a22
     !+ad_summ  Account 22 : Reactor Plant Equipment
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 22 (Reactor Plant Equipment)
     !+ad_desc  costs.
     !+ad_prob  None
     !+ad_call  step_a2201
     !+ad_call  step_a2202
     !+ad_call  step_a2203
     !+ad_call  step_a2204
     !+ad_call  step_a2205
     !+ad_call  step_a2206
     !+ad_call  step_a2207
     !+ad_call  oblnkl
     !+ad_call  ocosts
     !+ad_call  oshead
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
  
     !  Local variables

     real(kind(1.0D0)):: step2298, step2299
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step22 = 0.0D0
     step2298 = 0.0D0
  
     if ((iprint==1).and.(output_costs == 1)) then
       call oshead(outfile,'22. Reactor Plant Equipment')
     end if

     !  Account 22.01 : Reactor Equipment

     call step_a2201(step2298,outfile,iprint)

     !  Account 22.02 : Heat Transfer Systems

     call step_a2202(outfile,iprint)

     !  Account 22.03 : Cryogenic Cooling System

     call step_a2203(outfile,iprint)

     !  Account 22.04 : Waste Treatment and Disposal

     call step_a2204(outfile,iprint)

     !  Account 22.05 : Fuel Handling and Storage

     call step_a2205(step2298,outfile,iprint)

     !  Account 22.06 : Other Reactor Plant Equipment

     call step_a2206(step2298,outfile,iprint)

     !  Account 22.07 : Instrumentation and Control

     call step_a2207(outfile,iprint)

     ! 22.98 Spares
     ! Original STARFIRE value, no scaling
     !step2298 = 6.638D1 
     step22 = step22 + step2298
  
     ! 21.99 Contingency
     ! STARFIRE 15%
     step2299 = 1.5D-1 * step22
     step22 = step22 + step2299

     if ((iprint==1).and.(output_costs == 1)) then
       write(outfile,*) '******************* '
       call ocosts(outfile,'(step2298)','Spares (M$)', step2298)
       call ocosts(outfile,'(step2299)','Contingency (M$)', step2299)
       call oblnkl(outfile)
       call ocosts(outfile,'(step22)','Total Account 22 Cost (M$)', step22)
     end if
  
   end subroutine step_a22
  

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine step_a2201(step2298,outfile,iprint)

     !+ad_name  step_a2201
     !+ad_summ  Account 22.01 : Reactor Equipment
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 22.01 (Reactor Equipment)
     !+ad_desc  costs.
     !+ad_prob  None
     !+ad_call  oblnkl
     !+ad_call  ocosts
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
     real(kind(1.0D0)), intent(inout) :: step2298
  
     !  Local variables
  
     real(kind(1.0D0)):: &
     step220101, step220102, step220104, step220105, step220106, &
     step220107, step220108, step220109, step220110, step2201, &
     step22010301, step22010302, step22010303, step22010304
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step2201 = 0.0D0
     
     ! 22.01.01 Blanket and First Wall
     ! Original STARFIRE value, scaling with first wall area
     step220101 = 8.236D1 * (fwarea / fwarea_star)
     if (ifueltyp==1) then
       fwblkcost = step220101
       step220101 = 0.0D0
     end if
     step2201 = step2201 + step220101
  
     ! 22.01.02 Shield
     ! Original STARFIRE value, scaling with first wall area
     step220102 = 1.8607D2 * (fwarea / fwarea_star)
     step2201 = step2201 + step220102
     step2298 = step2298 + 9.985D-2 *  step220102
  
     ! 22.01.03.01 TF Coils
     ! Original STARFIRE value, scaling with fusion island volume
     step22010301 = 1.2572D2 * (vfi / vfi_star)
     step2201 = step2201 + step22010301

     ! 22.01.03.02 PF Coils
     ! Original STARFIRE value, scaling with fusion island volume
     step22010302 = 3.46D1 * (vfi / vfi_star)
     step2201 = step2201 + step22010302
     step2298 = step2298 + 3.269D-1 * step22010302

     ! 22.01.03.03 Central Solenoid
     ! Original STARFIRE value, scaling with fusion island volume
     step22010303 = 7.25D0 * (vfi / vfi_star)
     step2201 = step2201 + step22010303
     step2298 = step2298 + 6.124D-1 * step22010303

     ! 22.01.03.04 Control Coils
     ! Original STARFIRE value, scaling with fusion island volume
     step22010304 = 4.0D0 * (vfi / vfi_star)
     step2201 = step2201 + step22010304
     step2298 = step2298 + 1.075D-1 * step22010304
  
     ! 22.01.04 Auxiliary Heating and Current Drive
     ! Original STARFIRE value, scaling with auxiliary power
     step220104 = 3.349D1 * (pinjmw / pinjmw_star)
     if (ifueltyp==1) then
      step220104 = (1.0D0-fcdfuel) * step220104 
      cdcost = step220104
     end if
     step2201 = step2201 + step220104
     step2298 = step2298 + 2.335D-1 * step220104
  
     ! 22.01.05 Primary Structure and Support
     ! Original STARFIRE value, no scaling
     step220105 = 5.274D1
     step2201 = step2201 + step220105
     step2298 = step2298 + 6.824D-2 * step220105
  
     ! 22.01.06 Reactor Vacuum System
     ! Original STARFIRE value, no scaling
     step220106 = 4.86D0
     step2201 = step2201 + step220106
     step2298 = step2298 + 1.893D-1 * step220106
  
     ! 22.01.07 Power Supplies
     ! Original STARFIRE value, no scaling
     step220107 = 5.29D1
     step2201 = step2201 + step220107
  
     ! 22.01.08 Impurity Control
     ! Original STARFIRE value, no scaling
     step220108 = 2.45D0
     step2201 = step2201 + step220108
  
     ! 22.01.09 ECRH Plasma Breakdown
     ! Original STARFIRE value, no scaling
     step220109 = 2.82D0 
     step2201 = step2201 + step220109

     ! 22.01.10 Divertor
     ! Cost Model 0 cost for STARFIRE sized device
     ! 58.62% increase between 1980 and 1990 
     ! http://www.in2013dollars.com/1980-dollars-in-1990
     ! Scaling with product of rmajor and rminor
     step220110 = (2.658D1 /1.5862D0) * ((rmajor*rminor)/(rmajor_star*rminor_star)) 
     if (ifueltyp == 1) then
         divcst = step220110
         step220110 = 0.0D0
     else
         divcst = 0.0D0
     end if
     step2201 = step2201 + step220110
  
     step22 = step22 + step2201

     if ((iprint==1).and.(output_costs == 1)) then
       write(outfile,*) '******************* 22.01 Reactor Equipment'
       if (ifueltyp==0)then
         call ocosts(outfile,'(step220101)','Blanket and First Wall (M$)', step220101)
       else if (ifueltyp==1)then
         call ocosts(outfile,'(step220101)','Blanket and First Wall (Treated as Fuel) (M$)', step220101)
       end if
       call ocosts(outfile,'(step220102)','Shield (M$)', step220102)
       call ocosts(outfile,'(step22010301)','TF Coils (M$)', step22010301)
       call ocosts(outfile,'(step22010302)','PF Coils (M$)', step22010302)
       call ocosts(outfile,'(step22010303)','Central Solenoid (M$)', step22010303)
       call ocosts(outfile,'(step22010304)','Control Coils (M$)', step22010304)
       if (ifueltyp==0)then
         call ocosts(outfile,'(step220104)','Auxiliary Heating and Current Drive (M$)', step220104)
       else if (ifueltyp==1)then
        call ocosts(outfile,'(step220104)','Auxiliary Heating and Current Drive (Fraction as Fuel) (M$)', step220104)
       end if
       call ocosts(outfile,'(step220105)','Primary Structure and Support (M$)', step220105)
       call ocosts(outfile,'(step220106)','Reactor Vacuum System (M$)', step220106)
       call ocosts(outfile,'(step220107)','Power Supplies (M$)', step220107)
       call ocosts(outfile,'(step220108)','Impurity Control (M$)', step220108)
       call ocosts(outfile,'(step220109)','ECRH Plasma Breakdown (M$)', step220109)
       call ocosts(outfile,'(step220110)','Divertor (M$)', step220110)
       call oblnkl(outfile)
       call ocosts(outfile,'(step2201)','Total Account 22.01 Cost (M$)', step2201)
       call oblnkl(outfile)
     end if
  
   end subroutine step_a2201

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine step_a2202(outfile,iprint)

     !+ad_name  step_a2202
     !+ad_summ  Account 22.02 : Heat Transfer System
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 22.02 (Heat Transfer System)
     !+ad_desc  costs.
     !+ad_prob  None
     !+ad_call  oblnkl
     !+ad_call  ocosts
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
  
     !  Local variables
  
     real(kind(1.0D0)):: step2202
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step2202 = 0.0D0
     
     ! 22.02 Heat Transfer System
     ! Original STARFIRE value, scaling with first wall area
     step2202 = 6.984D1 * (pth / ptherm_star)**0.6D0  
  
     step22 = step22 + step2202

     if ((iprint==1).and.(output_costs == 1)) then
     write(outfile,*) '******************* 22.02 Heat Transfer System'
     call ocosts(outfile,'(step2202)','Heat Transfer System (M$)', step2202)
     call oblnkl(outfile)
     call ocosts(outfile,'(step2202)','Total Account 22.02 Cost (M$)', step2202)
     call oblnkl(outfile)
     end if
  
   end subroutine step_a2202

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine step_a2203(outfile,iprint)

     !+ad_name  step_a2203
     !+ad_summ  Account 22.03 : Cryogenic Cooling System
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 22.03 (Cryogenic Cooling
     !+ad_desc  System) costs.
     !+ad_prob  None
     !+ad_call  oblnkl
     !+ad_call  ocosts
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
  
     !  Local variables
  
     real(kind(1.0D0)):: &
     step220301, step220302, step220303, step220304, step2203
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step2203 = 0.0D0
     
     ! 22.03.01 Helium Refrigerator
     ! Original STARFIRE value, no scaling
     step220301 = 7.7D0
     step2203 = step2203 + step220301
  
     ! 22.03.02 Liquid Helium Transfer and Storage
     ! Original STARFIRE value, no scaling
     step220302 = 3.6D0
     step2203 = step2203 + step220302
  
     ! 22.03.03 Gas Helium Storage
     ! Original STARFIRE value, no scaling
     step220303 = 2.8D0
     step2203 = step2203 + step220303
  
     ! 22.03.04 Liquid Nitrogen Storage
     ! Original STARFIRE value, no scaling
     step220304 = 8.0D-1
     step2203 = step2203 + step220304
  
     step22 = step22 + step2203

     if ((iprint==1).and.(output_costs == 1)) then
       write(outfile,*) '******************* 22.03 Cryogenic Cooling System'
       call ocosts(outfile,'(step220301)','Helium Refrigerator (M$)', step220301)
       call ocosts(outfile,'(step220302)','Liquid Helium Transfer and Storage (M$)', step220302)
       call ocosts(outfile,'(step220303)','Gas Helium Storage (M$)', step220303)
       call ocosts(outfile,'(step220304)','Liquid Nitrogen Storage (M$)', step220304)
       call oblnkl(outfile)
       call ocosts(outfile,'(step2203)','Total Account 22.03 Cost (M$)', step2203)
       call oblnkl(outfile)
     end if
  
   end subroutine step_a2203

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine step_a2204(outfile,iprint)

     !+ad_name  step_a2204
     !+ad_summ  Account 22.04 : Waste Treatment and Disposal
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 22.04 (Waste Treatment
     !+ad_desc  and Disposal) costs.
     !+ad_prob  None
     !+ad_call  None
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
  
     !  Local variables
  
     real(kind(1.0D0)):: &
     step220401, step220402, step220403, step2204
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step2204 = 0.0D0
     
     ! 22.04.01 Liquid Waste
     ! Original STARFIRE value, no scaling
     step220401 = 1.7D0 * (pth / ptherm_star)**0.6D0 
     step2204 = step2204 + step220401
  
     ! 22.04.02 Gaseous Waste
     ! Original STARFIRE value, no scaling
     step220402 = 1.8D0 * (pth / ptherm_star)**0.6D0 
     step2204 = step2204 + step220402
  
     ! 22.04.03 Solid Waste
     ! Original STARFIRE value, no scaling
     step220403 = 1.3D0 * (pth / ptherm_star)**0.6D0 
     step2204 = step2204 + step220403
  
  
     step22 = step22 + step2204

     if ((iprint==1).and.(output_costs == 1)) then
       write(outfile,*) '******************* 22.04 Waste Treatment and Disposal'
       call ocosts(outfile,'(step220401)','Liquid Waste (M$)', step220401)
       call ocosts(outfile,'(step220402)','Gaseous Waste (M$)', step220402)
       call ocosts(outfile,'(step220403)','Solid Waste (M$)', step220403)
       call oblnkl(outfile)
       call ocosts(outfile,'(step2204)','Total Account 22.04 Cost (M$)', step2204)
       call oblnkl(outfile)
     end if
  
   end subroutine step_a2204

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine step_a2205(step2298,outfile,iprint)

     !+ad_name  step_a2205
     !+ad_summ  Account 22.05 : Fuel Handling and Storage
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 22.05 (Fuel Handling
     !+ad_desc  and Storage) costs.
     !+ad_prob  None
     !+ad_call  oblnkl
     !+ad_call  ocosts
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
     real(kind(1.0D0)), intent(inout) :: step2298
  
     !  Local variables
  
     real(kind(1.0D0)):: step2205
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step2205 = 0.0D0
     
     ! 22.05 Fuel Handling and Storage
     ! Original STARFIRE value, no scaling
     step2205 = 3.86D1 
  
     step22 = step22 + step2205
     step2298 = step2298 + 5.026D-2 * step2205

     if ((iprint==1).and.(output_costs == 1)) then
       write(outfile,*) '******************* 22.05 Fuel Handling and Storage'
       call ocosts(outfile,'(step2205)','Fuel Handling and Storage (M$)', step2205)
       call oblnkl(outfile)
       call ocosts(outfile,'(step2205)','Total Account 22.05 Cost (M$)', step2205)
       call oblnkl(outfile)
     end if
  
   end subroutine step_a2205

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine step_a2206(step2298,outfile,iprint)

     !+ad_name  step_a2206
     !+ad_summ  Account 22.06 : Other Reactor Plant Equipment
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 22.06 (Other Reactor
     !+ad_desc  Plant Equipment) costs.
     !+ad_prob  None
     !+ad_call  oblnkl
     !+ad_call  ocosts
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
     real(kind(1.0D0)), intent(inout) :: step2298
  
     !  Local variables
  
     real(kind(1.0D0)):: &
     step220601, step220602, step220603, step220604, step220605, &
     step220606, step220607, step220608, step2206
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step2206 = 0.0D0
     
     ! 22.06.01 Maintenance Equipment
     ! Original STARFIRE value, scaling with fusion island volume
     step220601 = 3.83D1 * (vfi / vfi_star)
     step2206 = step2206 + step220601
     step2298 = step2298 + 4.308D-1 * step220601
  
     ! 22.06.02 Special Heating Systems
     ! Original STARFIRE value, no scaling
     step220602 = 0.0D0
     step2206 = step2206 + step220602
  
     ! 22.06.03 Coolant Storage
     ! Original STARFIRE value, scaling with thermal power
     step220603 = 2.4D-1 * (pth / ptherm_star)**0.6D0
     step2206 = step2206 + step220603
  
     ! 22.06.04 Gas System
     ! Original STARFIRE value, scaling with fusion island volume
     step220604 = 8.0D-2 * (vfi / vfi_star)
     step2206 = step2206 + step220604
  
     ! 22.06.05 Inert Atmosphere System
     ! Original STARFIRE value, no scaling
     step220605 = 0.0D0
     step2206 = step2206 + step220605
  
     ! 22.06.06 Fluid Leak Detection
     ! Original STARFIRE value, no scaling
     step220606 = 2.0D0
     step2206 = step2206 + step220606
  
     ! 22.06.07 Closed Loop Coolant System
     ! Original STARFIRE value, scaling with thermal power
     step220607 = 1.97D0 * (pth / ptherm_star)**0.6D0
     step2206 = step2206 + step220607
     step2298 = step2298 + 8.3D-1 * (pth / ptherm_star)**0.6D0
  
     ! 22.06.08 Standby Cooling System
     ! Original STARFIRE value, no scaling
     step220608 = 1.16D0 * (pth / ptherm_star)**0.6D0
     step2206 = step2206 + step220608
  
     step22 = step22 + step2206

     if ((iprint==1).and.(output_costs == 1)) then
       write(outfile,*) '******************* 22.06 Other Reactor Plant Equipment'
       call ocosts(outfile,'(step220601)','Maintenance Equipment (M$)', step220601)
       call ocosts(outfile,'(step220602)','Special Heating Systems (M$)', step220602)
       call ocosts(outfile,'(step220603)','Coolant Storage (M$)', step220603)
       call ocosts(outfile,'(step220604)','Gas System (M$)', step220604)
       ! call ocosts(outfile,'(step220605)','Inert Atmosphere System (M$)', step220605)
       call ocosts(outfile,'(step220606)','Fluid Leak Detection (M$)', step220606)
       call ocosts(outfile,'(step220607)','Closed Loop Coolant System (M$)', step220607)
       call ocosts(outfile,'(step220608)','Standby Cooling System (M$)', step220608)
       call oblnkl(outfile)
       call ocosts(outfile,'(step2206)','Total Account 22.06 Cost (M$)', step2206)
       call oblnkl(outfile)
     end if
  
   end subroutine step_a2206
 
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine step_a2207(outfile,iprint)

     !+ad_name  step_a2207
     !+ad_summ  Account 22.07 : Instrumentation and Control
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 22.07 (Instrumentation
     !+ad_desc  and Control) costs.
     !+ad_prob  None
     !+ad_call  oblnkl
     !+ad_call  ocosts
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
     !  Local variables
  
     real(kind(1.0D0)):: step2207
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step2207 = 0.0D0
     
     ! 22.07 Instrumentation and Control
     ! Original STARFIRE value, no scaling
     step2207 = 2.341D1 
  
     step22 = step22 + step2207

     if ((iprint==1).and.(output_costs == 1)) then
     write(outfile,*) '******************* 22.07 Instrumentation and Control'
     call ocosts(outfile,'(step2207)','Instrumentation and Control (M$)', step2207)
     call oblnkl(outfile)
     call ocosts(outfile,'(step2207)','Total Account 22.07 Cost (M$)', step2207)
     call oblnkl(outfile)
     end if
  
   end subroutine step_a2207

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine step_a23(outfile,iprint)

   !+ad_name  step_a23
   !+ad_summ  Account 23 : Turbine Plant Equipment
   !+ad_type  Subroutine
   !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
   !+ad_cont  N/A
   !+ad_args  None
   !+ad_desc  This routine evaluates the Account 23 (Turbine Plant Equipment)
   !+ad_desc  costs.
   !+ad_prob  None
   !+ad_call  oblnkl
   !+ad_call  ocosts
   !+ad_call  oshead
   !+ad_hist  28/02/19 SIM Initial version
   !+ad_stat  Okay
   !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   integer, intent(in) :: iprint,outfile

   !  Local variables

   real(kind(1.0D0)):: &
   step2301, step2302, step2303, step2304, step2305, step2306, &
   step2307, step2398, step2399

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   step23 = 0.0D0
   
   ! 23.01 Turbine Generators
   ! Original STARFIRE value, scaling with thermal power
   step2301 = 7.733D1 * (pth / ptherm_star)**0.6D0
   step23 = step23 + step2301

   ! 23.02 Steam System
   ! Original STARFIRE value, scaling with thermal power
   step2302 = 4.37D0 * (pth / ptherm_star)**0.6D0
   step23 = step23 + step2302

   ! 23.03 Heat Rejection
   ! Original STARFIRE value, scaling with thermal power
   step2303 = 4.434D1 * (pth / ptherm_star)**0.6D0
   step23 = step23 + step2303

   ! 23.04 Condensing System
   ! Original STARFIRE value, scaling with thermal power
   step2304 = 1.918D1 * (pth / ptherm_star)**0.6D0
   step23 = step23 + step2304

   ! 23.05 Feedwater Heating System
   ! Original STARFIRE value, scaling with thermal power
   step2305 = 9.39D0 * (pth / ptherm_star)**0.6D0
   step23 = step23 + step2305

   ! 23.06 Other Turbine Equipment
   ! Original STARFIRE value, scaling with thermal power
   step2306 = 5.084D1 * (pth / ptherm_star)**0.6D0  
   step23 = step23 + step2306

   ! 23.07 Instrumentation and Control
   ! Original STARFIRE value, no scaling
   step2307 = 8.7D0
   step23 = step23 + step2307

   ! 23.98 Spares
   ! Original STARFIRE value, no scaling
   step2398 = 3.0D0 
   step23 = step23 + step2398

   ! 23.99 Contingency
   ! STARFIRE 15%
   step2399 = 1.5D-1 * step23
   step23 = step23 + step2399

   if ((iprint==1).and.(output_costs == 1)) then
     call oshead(outfile,'23. Turbine Plant Equipment')
     call ocosts(outfile,'(step2301)','Turbine Generators (M$)', step2301)
     call ocosts(outfile,'(step2302)','Steam System (M$)', step2302)
     call ocosts(outfile,'(step2303)','Heat Rejection (M$)', step2303)
     call ocosts(outfile,'(step2304)','Condensing System (M$)', step2304)
     call ocosts(outfile,'(step2305)','Feedwater Heating System (M$)', step2305)
     call ocosts(outfile,'(step2306)','Other Turbine Equipment (M$)', step2306)
     call ocosts(outfile,'(step2307)','Instrumentation and Control (M$)', step2307)
     call ocosts(outfile,'(step2398)','Spares (M$)', step2398)
     call ocosts(outfile,'(step2399)','Contingency (M$)', step2399)
     call oblnkl(outfile)
     call ocosts(outfile,'(step23)','Total Account 23 Cost (M$)', step23)
   end if

 end subroutine step_a23

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine step_a24(outfile,iprint)

   !+ad_name  step_a24
   !+ad_summ  Account 24 : Electric Plant Equipment
   !+ad_type  Subroutine
   !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
   !+ad_cont  N/A
   !+ad_args  None
   !+ad_desc  This routine evaluates the Account 24 (Electric Plant 
   !+ad_desc  Equipment) costs.
   !+ad_prob  None
   !+ad_call  oblnkl
   !+ad_call  ocosts
   !+ad_call  oshead
   !+ad_hist  28/02/19 SIM Initial version
   !+ad_stat  Okay
   !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
   !
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   implicit none

   !  Arguments

   integer, intent(in) :: iprint,outfile

   !  Local variables

   real(kind(1.0D0)):: &
   step2401, step2402, step2403, step2404, step2405, step2406, &
   step2407, step2498, step2499

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   step24 = 0.0D0
   
   ! 24.01 Switch Gear
   ! Original STARFIRE value, no scaling
   step2401 = 1.239D1
   step24 = step24 + step2401

   ! 24.02 Station Service Equipment
   ! Original STARFIRE value, no scaling
   step2402 = 1.704D1
   step24 = step24 + step2402

   ! 24.03 Switchboards
   ! Original STARFIRE value, no scaling
   step2403 = 7.8D0
   step24 = step24 + step2403

   ! 24.04 Protective Equipment
   ! Original STARFIRE value, no scaling
   step2404 = 2.11D0
   step24 = step24 + step2404

   ! 24.05 Electrical Structures
   ! Original STARFIRE value, no scaling
   step2405 = 1.74D1
   step24 = step24 + step2405

   ! 24.06 Power and Control Wiring
   ! Original STARFIRE value, no scaling
   step2406 = 3.599D1
   step24 = step24 + step2406

   ! 24.07 Electric Lighting
   ! Original STARFIRE value, no scaling
   step2407 = 8.2D0
   step24 = step24 + step2407

   ! 24.98 Spares
   ! Original STARFIRE value, no scaling
   step2498 = 1.05D0 
   step24 = step24 + step2498

   ! 24.99 Contingency
   ! STARFIRE 15%
   step2499 = 1.5D-1 * step24
   step24 = step24 + step2499

   if ((iprint==1).and.(output_costs == 1)) then
     call oshead(outfile,'24. Electric Plant Equipment')
     call ocosts(outfile,'(step2401)','Switch Gear (M$)', step2401)
     call ocosts(outfile,'(step2402)','Station Service Equipment (M$)', step2402)
     call ocosts(outfile,'(step2403)','Switchboards (M$)', step2403)
     call ocosts(outfile,'(step2404)','Protective Equipment (M$)', step2404)
     call ocosts(outfile,'(step2405)','Electrical Structures (M$)', step2405)
     call ocosts(outfile,'(step2406)','Power and Control Wiring (M$)', step2406)
     call ocosts(outfile,'(step2407)','Electric Lighting (M$)', step2407)
     call ocosts(outfile,'(step2498)','Spares (M$)', step2498)
     call ocosts(outfile,'(step2499)','Contingency (M$)', step2499)
     call oblnkl(outfile)
     call ocosts(outfile,'(step24)','Total Account 24 Cost (M$)', step24)
   end if

 end subroutine step_a24

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 subroutine step_a25(outfile,iprint)

     !+ad_name  step_a25
     !+ad_summ  Account 25 : Miscellaneous Plant Equipment
     !+ad_type  Subroutine
     !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
     !+ad_cont  N/A
     !+ad_args  None
     !+ad_desc  This routine evaluates the Account 25 (Miscellaneous Plant 
     !+ad_desc  Equipment) costs.
     !+ad_prob  None
     !+ad_call  oblnkl
     !+ad_call  ocosts
     !+ad_call  oshead
     !+ad_hist  01/03/19 SIM Initial version
     !+ad_stat  Okay
     !+ad_docs  STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     implicit none
  
     !  Arguments
  
     integer, intent(in) :: iprint,outfile
  
     !  Local variables
  
     real(kind(1.0D0)):: &
     step2501, step2502, step2503, step2504, step2598, step2599
  
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     step25 = 0.0D0
     
     ! 25.01 Transport and Lifting Equipment
     ! Original STARFIRE value, scaling with thermal power
     step2501 = 1.568D1 * (pth / ptherm_star)**0.6D0
     step25 = step25 + step2501
  
     ! 24.02 Air and Water Service System
     ! Original STARFIRE value, scaling with thermal power
     step2502 = 1.235D1 * (pth / ptherm_star)**0.6D0
     step25 = step25 + step2502
  
     ! 24.03 Communications Equipment
     ! Original STARFIRE value, no scaling
     step2503 = 6.22D0
     step25 = step25 + step2503
  
     ! 24.04 Furnishing and Fixtures
     ! Original STARFIRE value, no scaling
     step2504 = 7.5D-1
     step25 = step25 + step2504
  
     ! 24.98 Spares
     ! Original STARFIRE value, no scaling
     step2598 = 4.5D-1 
     step25 = step25 + step2598
  
     ! 24.99 Contingency
     ! STARFIRE 15%
     step2599 = 1.5D-1 * step25
     step25 = step25 + step2599
  
     if ((iprint==1).and.(output_costs == 1)) then
       call oshead(outfile,'25. Miscellaneous Plant Equipment')
       call ocosts(outfile,'(step2501)','Transport and Lifting Equipment (M$)', step2501)
       call ocosts(outfile,'(step2502)','Air and Water Service System (M$)', step2502)
       call ocosts(outfile,'(step2503)','Communications Equipment (M$)', step2503)
       call ocosts(outfile,'(step2504)','Furnishing and Fixtures (M$)', step2504)
       call ocosts(outfile,'(step2598)','Spares (M$)', step2598)
       call ocosts(outfile,'(step2599)','Contingency (M$)', step2599)
       call oblnkl(outfile)
       call ocosts(outfile,'(step25)','Total Account 25 Cost (M$)', step25)
     end if
  
   end subroutine step_a25

 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine coelc_step(outfile,iprint)

    !+ad_name  coelc
    !+ad_summ  Routine to calculate the cost of electricity for a fusion power plant
    !+ad_type  Subroutine
    !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine performs the calculation of the cost of electricity
    !+ad_desc  for a fusion power plant.
    !+ad_desc  <P>Annual costs are in megadollars/year, electricity costs are in
    !+ad_desc  millidollars/kWh, while other costs are in megadollars.
    !+ad_desc  All values are based on 1980 dollars.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  oshead
    !+ad_call  osubhd
    !+ad_call  ovarrf
    !+ad_hist  01/03/19 SIM Initial version copied from coelc
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: anncap,anncdr,anncp,anndecom,anndiv,annfuel, &
         annfuelt,annfwbl,annoam,anntot,annwst,coecdr, &
         coecp,coedecom,coediv,coefuel,coefwbl,coewst,crfcdr,crfcp, &
         crfdiv,crffwbl,fefcdr,fefcp,fefdiv,feffwbl,fwbllife,kwhpy
         ! annoam1,
         
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Number of kWh generated each year

    kwhpy = 1.0D3 * pnetelmw * (24.0D0*365.0D0) * cfactr * tburn/tcycle

    !  Costs due to reactor plant
    !  ==========================

    !  Interest on construction costs

    moneyint = concost * (fcap0 - 1.0D0)

    !  Capital costs

    capcost = concost + moneyint

    !  Annual cost of plant capital cost

    anncap = capcost * fcr0

    !  Cost of electricity due to plant capital cost

    coecap = 1.0D9 * anncap / kwhpy

    !  Costs due to first wall and blanket renewal
    !  ===========================================

    !  Operational life

    fwbllife = bktlife

    !  Compound interest factor

    feffwbl = (1.0D0 + ratecdol)**fwbllife

    !  Capital recovery factor

    crffwbl = (feffwbl*ratecdol) / (feffwbl-1.0D0)

    !  Annual cost of replacements

    annfwbl = fwblkcost * (1.0D0+cfind(lsa)) * fcap0cp * crffwbl



    !  Cost of electricity due to first wall/blanket replacements

    coefwbl = 1.0D9 * annfwbl / kwhpy

    !  Costs due to divertor renewal
    !  =============================

       !  Compound interest factor

       fefdiv = (1.0D0 + ratecdol)**divlife

       !  Capital recovery factor

       crfdiv = (fefdiv*ratecdol) / (fefdiv-1.0D0)

       !  Annual cost of replacements

       anndiv = divcst * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfdiv

       !  Cost of electricity due to divertor replacements

       coediv = 1.0D9 * anndiv / kwhpy

    !  Costs due to centrepost renewal
    !  ===============================

    if (itart == 1) then

       !  Compound interest factor

       fefcp = (1.0D0 + ratecdol)**cplife

       !  Capital recovery factor

       crfcp = (fefcp*ratecdol) / (fefcp-1.0D0)

       !  Annual cost of replacements

       anncp = cpstcst * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfcp

       !  Cost of electricity due to centrepost replacements

       coecp = 1.0D9 * anncp / kwhpy

    else
       anncp = 0.0D0
       coecp = 0.0D0
    end if

    !  Costs due to partial current drive system renewal
    !  =================================================

    !  Compound interest factor

    fefcdr = (1.0D0 + ratecdol)**cdrlife

    !  Capital recovery factor

    crfcdr = (fefcdr*ratecdol) / (fefcdr-1.0D0)

    !  Annual cost of replacements

    if (ifueltyp == 0) then
       anncdr = 0.0D0
    else
       anncdr = cdcost * fcdfuel/(1.0D0-fcdfuel) * &
            (1.0D0+cfind(lsa)) * fcap0cp * crfcdr
    end if

    !  Cost of electricity due to current drive system replacements

    coecdr = 1.0D9 * anncdr / kwhpy

    !  Costs due to operation and maintenance
    !  ======================================

    !  Annual cost of operation and maintenance

    annoam = ucoam(lsa) * sqrt(pnetelmw/1200.0D0)

    !  Additional cost due to pulsed reactor thermal storage
    !  See F/MPE/MOD/CAG/PROCESS/PULSE/0008
    !
    !      if (lpulse.eq.1) then
    !         if (istore.eq.1) then
    !            annoam1 = 51.0D0
    !         else if (istore.eq.2) then
    !            annoam1 = 22.2D0
    !         else
    !            continue
    !         end if
    !
    !  Scale with net electric power
    !
    !         annoam1 = annoam1 * pnetelmw/1200.0D0
    !
    !  It is necessary to convert from 1992 pounds to 1990 dollars
    !  Reasonable guess for the exchange rate + inflation factor
    !  inflation = 5% per annum; exchange rate = 1.5 dollars per pound
    !
    !         annoam1 = annoam1 * 1.36D0
    !
    !         annoam = annoam + annoam1
    !
    !      end if

    !  Cost of electricity due to operation and maintenance

    coeoam = 1.0D9 * annoam / kwhpy

    !  Costs due to reactor fuel
    !  =========================

    !  Annual cost of fuel

       !  Sum D-T fuel cost and He3 fuel cost
       annfuel = ucfuel * pnetelmw/1200.0D0 + &
            1.0D-6 * fhe3 * wtgpd * 1.0D-3 * uche3 * 365.0D0 * cfactr

    !  Cost of electricity due to reactor fuel

    coefuel = 1.0D9 * annfuel / kwhpy

    !  Costs due to waste disposal
    !  ===========================

    !  Annual cost of waste disposal

    annwst = ucwst(lsa) * sqrt(pnetelmw/1200.0D0)

    !  Cost of electricity due to waste disposal

    coewst = 1.0D9 * annwst / kwhpy

    !  Costs due to decommissioning fund
    !  =================================

    !  Annual contributions to fund for decommissioning
    !  A fraction decomf of the construction cost is set aside for
    !  this purpose at the start of the plant life.
    !  Final factor takes into account inflation over the plant lifetime
    !  (suggested by Tim Hender 07/03/96)
    !  Difference (dintrt) between borrowing and saving interest rates is
    !  included, along with the possibility of completing the fund dtlife
    !  years before the end of the plant's lifetime

    anndecom = decomf * concost * fcr0 / &
         (1.0D0+ratecdol-dintrt)**(tlife-dtlife)

    !  Cost of electricity due to decommissioning fund

    coedecom = 1.0D9 * anndecom / kwhpy

    !  Total costs
    !  ===========

    !  Annual costs due to 'fuel-like' components

    annfuelt = annfwbl + anndiv + anncdr + anncp + annfuel + annwst

    !  Total cost of electricity due to 'fuel-like' components

    coefuelt = coefwbl + coediv + coecdr + coecp + coefuel + coewst

    !  Total annual costs

    anntot = anncap + annfuelt + annoam + anndecom

    !  Total cost of electricity

    coe = coecap + coefuelt + coeoam + coedecom

    if ((iprint == 0).or.(output_costs == 0)) return

    !  Output section

     call oshead(outfile,'Interest during Construction')
     call ocosts(outfile,'(moneyint)','Interest during construction (M$)',moneyint)

     call oshead(outfile,'Total Capital Investment')
     call ocosts(outfile,'(capcost)','Total capital investment (M$)',capcost)


    call oheadr(outfile,'Cost of Electricity (1980 US$)')

    call ovarrf(outfile,'First wall / blanket life (years)','(fwbllife)', &
         fwbllife)


       call ovarrf(outfile,'Divertor life (years)','(divlife.)',divlife)
       if (itart == 1) then
          call ovarrf(outfile,'Centrepost life (years)','(cplife.)',cplife)
       end if


    call ovarrf(outfile,'Cost of electricity (m$/kWh)','(coe)',coe)

    call osubhd(outfile,'Power Generation Costs :')

    if ((annfwbl /= annfwbl).or.(annfwbl > 1.0D10).or.(annfwbl < 0.0D0)) then
        write(outfile,*)'Problem with annfwbl'
        write(outfile,*)'fwblkcost=', fwallcst
        write(outfile,*)'crffwbl=', crffwbl,   '  fcap0cp=', fcap0cp
        write(outfile,*)'feffwbl=', feffwbl,   '  fwbllife=', fwbllife
    end if

    write(outfile,200) &
         anncap,coecap, &
         annoam,coeoam, &
         anndecom,coedecom, &
         annfwbl,coefwbl, &
         anndiv,coediv, &
         anncp,coecp, &
         anncdr,coecdr, &
         annfuel,coefuel, &
         annwst,coewst, &
         annfuelt,coefuelt, &
         anntot,coe

200 format( &
         t76,'Annual Costs, M$       COE, m$/kWh'// &
         1x,'Capital Investment',t80,f10.2,10x,f10.2/ &
         1x,'Operation & Maintenance',t80,f10.2,10x,f10.2/ &
         1x,'Decommissioning Fund',t80,f10.2,10x,f10.2/ &
         1x,'Fuel Charge Breakdown'// &
         5x,'Blanket & first wall',t72,f10.2,10x,f10.2/ &
         5x,'Divertors',t72,f10.2,10x,f10.2/ &
         5x,'Centrepost (TART only)',t72,f10.2,10x,f10.2/ &
         5x,'Auxiliary Heating',t72,f10.2,10x,f10.2/ &
         5x,'Actual Fuel',t72,f10.2,10x,f10.2/ &
         5x,'Waste Disposal',t72,f10.2,10x,f10.2/ &
         1x,'Total Fuel Cost',t80,f10.2,10x,f10.2// &
         1x,'Total Cost',t80,f10.2,10x,f10.2 )

    if (ifueltyp == 1) then
       call oshead(outfile,'Replaceable Components Direct Capital Cost')
       call ovarrf(outfile,'First wall and Blanket direct capital cost (M$)', &
            '(fwblkcost)',fwblkcost)

          call ovarrf(outfile,'Divertor direct capital cost (M$)', &
               '(divcst)',divcst)
          if (itart == 1) then
             call ovarrf(outfile,'Centrepost direct capital cost (M$)', &
                  '(cpstcst)',cpstcst)
          end if
          call ovarrf(outfile,'Plasma heating/CD system cap cost (M$)', &
               '',cdcost*fcdfuel/(1.0D0-fcdfuel))
          call ovarrf(outfile,'Fraction of CD cost --> fuel cost', &
               '(fcdfuel)',fcdfuel)

    end if

  end subroutine coelc_step

end module costs_step_module
