! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module costs_step_module

  !! Module containing STEP fusion power plant costing algorithms
  !! author: S I Muldrew, CCFE, Culham Science Centre
  !! N/A
  !! This module contains the STEP fusion power plant costing model,
  !! developed by Nizar Ben Ayed, Tim Hender and Stuart Muldrew, based
  !! on the STARFIRE costing framework.
  !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
  !! Sheffield et al. (1986), Fusion Technology, 9, 199
  !! Sheffield & Milora (2016), Fusion Science and Technology, 70, 14
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  !  Various cost account values (M$)
  real(dp) :: step20, step21, step22, step23, step24, step25, &
              step27, step91, step92, step93, fwblkcost

  ! Scaling Properties
  real(dp) :: vfi, vfi_star, ptherm_star, pinjmw_star, fwarea_star, &
              rmajor_star, rminor_star, pth

contains

  subroutine init_costs_step
    !! Initialise module variables
    implicit none

    step20 = 0.0D0
    step21 = 0.0D0
    step22 = 0.0D0
    step23 = 0.0D0
    step24 = 0.0D0
    step25 = 0.0D0
    step27 = 0.0D0
    step91 = 0.0D0
    step92 = 0.0D0
    step93 = 0.0D0
    fwblkcost = 0.0D0
    vfi = 0.0D0
    vfi_star = 0.0D0
    ptherm_star = 0.0D0
    pinjmw_star = 0.0D0
    fwarea_star = 0.0D0
    rmajor_star = 0.0D0
    rminor_star = 0.0D0
    pth = 0.0D0
  end subroutine init_costs_step

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine costs_step(outfile,iprint)

    !! STEP cost accounting for a fusion power plant
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine performs the cost accounting for a fusion power plant.
    !! The direct costs are calculated based on parameters input
    !! from other sections of the code.
    !! <P>The code is arranged in the order of the standard accounts.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !! Sheffield et al. (1986), Fusion Technology, 9, 199
    !! Sheffield & Milora (2016), Fusion Science and Technology, 70, 14
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: r_tf_outboard_mid, tfthko, hpfu, hmax, tfcth
    use cost_variables, only: output_costs, cdirt, concost, ireactor, ipnet, &
             step_ref, step_currency
    use fwbs_variables, only: emultmw
    use heat_transport_variables, only: pinjwp
    use physics_variables, only: powfmw
    use process_output, only: oshead, ocosts, oheadr

    implicit none

    ! Arguments
    integer, intent(in) :: iprint,outfile
    character(len=80) :: title

    ! Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Fusion Island Volume as defined by Sheffield & Milora (2016)
    vfi = pi * (r_tf_outboard_mid + 0.5D0*tfthko)**2 * (hpfu + hmax + tfcth)
    pth = powfmw + emultmw + pinjwp

    !  STARFIRE Reference Values
    ! vfi_star = 5.1D3                    ! Volume of Fusion Island (m3)
    vfi_star = 6.737D3                  ! Volume of Fusion Island (m3)
    ptherm_star = 4.15D3                ! Thermal Power (MW)
    pinjmw_star = 9.04D1                ! Auxiliary Power (MW)
    ! fwarea_star = 7.8D2                 ! First Wall Area (m2)
    fwarea_star = 9.42D2                ! First Wall Area (m2)
    rmajor_star = 7.0D0                 ! Major Radius (m)
    rminor_star = rmajor_star / 3.6D0   ! Minor Radius (m)

    ! Output header
    if ((iprint==1).and.(output_costs == 1)) then
      title = 'STEP Costing Model ('// trim(step_currency) // ')'
      call oheadr(outfile,trim(title))
    end if
    
    ! Account 20 : Land and Rights
    call step_a20(outfile,iprint)

    ! Account 21 : Building and Site Service Infrastructure
    call step_a21(outfile,iprint)

    ! Account 22 : Reactor Plant Equipment
    call step_a22(outfile,iprint)

    ! Account 23 : Turbine Plant Equipment
    call step_a23(outfile,iprint)

    ! Account 24 : Electric Plant Equipment
    call step_a24(outfile,iprint)

    ! Account 25 : Miscellaneous Plant Equipment
    call step_a25(outfile,iprint)

    ! Total plant direct cost without remote handling
    cdirt = step20 + step21 + step22 + step23 + step24 + step25

    ! Account 27 : Remote Handling
    call step_a27(outfile,iprint) 

    ! Total plant direct cost with remote handling
    cdirt = cdirt + step27
    if ((iprint==1).and.(output_costs == 1)) then
      call oshead(outfile,'Plant Direct Cost')
      call ocosts(outfile,'(cdirt)','Plant direct cost (M$)',cdirt)
    endif

    ! Accounts 91-93: Indirect costs
    call step_indirect_costs(outfile, iprint)

    ! Constructed cost
    concost = cdirt + step91 + step92 + step93
    if ((iprint==1).and.(output_costs == 1)) then
      call oshead(outfile,'Constructed Cost')
      call ocosts(outfile,'(concost)','Constructed Cost (M$)',concost)
    end if

    !  Cost of electricity
    if ((ireactor == 1).and.(ipnet == 0)) then 
      call coelc_step(outfile,iprint) 
    end if 
  end subroutine costs_step

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine step_a20(outfile,iprint)

    !! Account 20 : Land and Rights
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 20 (Land and Rights)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cost_variables, only: output_costs, step_ref, sitecost
    use process_output, only: oshead, ocosts, oblnkl

    implicit none

    ! Arguments
    integer, intent(in) :: iprint,outfile

    ! Local variables
    real(dp):: step2001, step2002

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Initialise as zero
    step20 = 0.0D0

    ! 20.01 Land
    ! Fixed site cost (2017 M$); read from input, default = 100 M$
    step2001 = sitecost / 1.0D6
    step20 = step20 + step2001

    ! 20.02 Site Preparation
    ! Original STARFIRE value
    step2002 = step_ref(2)
    step20 = step20 + step2002

    ! Output costs
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

    !! Account 21 : Building and Site Service Infrastructure
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 21 (Building and Site
    !! Service Infrastructure) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! #TODO Add reference for STEP cost values
    use cost_variables, only: output_costs, step_con, step_ref, wfbuilding
    use process_output, only: oshead, ocosts, oblnkl
    use buildings_variables, only: a_reactor_bldg, a_ee_ps_bldg, &
      a_aux_services_bldg, a_hot_cell_bldg, a_reactor_service_bldg, &
      a_service_water_bldg, a_fuel_handling_bldg, a_control_room_bldg, &
      a_ac_ps_bldg, a_admin_bldg, a_site_service_bldg, a_cryo_inert_gas_bldg, &
      a_security_bldg
    ! Floor areas in m^2 for buildings
    use heat_transport_variables, only: pgrossmw
    ! pgrossmw is gross electric power of the plant in MW

    implicit none

    ! Arguments
    integer, intent(in) :: iprint,outfile

    ! Local variables
    real(dp):: &
    step2101, step2102, step2103, step2104, step2105, step2106, &
    step2107, step2108, step2109, step2110, step2111, step2112, &
    step2113, step2114, step2115, step2116, step2117, step2118, &
    step2198, step2199

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Initialise as zero
    step21 = 0.0D0
   
    ! 21.01 Site Improvements
    ! Original STARFIRE value
    step2101 = step_ref(3)
    step21 = step21 + step2101
    
    ! 21.02 Reactor Building
    step2102 = 8.665D3 * a_reactor_bldg**1.2132 * 1.0D-6
    ! * 1.0D-6 converts to M$
    step21 = step21 + step2102
    
    ! 21.03 Turbine Building
    step2103 = 3.14310D5 * pgrossmw * 1.0D-6
    step21 = step21 + step2103

    ! 21.04 Cooling System Structures
    step2104 = 1.08155D5 * pgrossmw * 1.0D-6
    step21 = step21 + step2104

    ! 21.05 Electrical Equipment and Power Supply Building
    step2105 = ((4.688D3 * a_ee_ps_bldg) + 3.185967D6) * 1.0D-6
    step21 = step21 + step2105

    ! 21.06 Auxiliary Services Building
    step2106 = ((3.107D3 * a_aux_services_bldg) + 1.206225D6) * 1.0D-6
    step21 = step21 + step2106

    ! 21.07 Hot Cell
    step2107 = ((1.9773D4 * a_hot_cell_bldg) + 5.975425D6) * 1.0D-6
    step21 = step21 + step2107

    ! 21.08 Reactor Service Building
    step2108 = ((8.563D3 * a_reactor_service_bldg) + 3.657324D6) * 1.0D-6
    step21 = step21 + step2108

    ! 21.09 Service Water Building
    step2109 = ((3.288D3 * a_service_water_bldg) + 3.19189D5) * 1.0D-6
    step21 = step21 + step2109

    ! 21.10 Fuel Handling and Storage Building
    step2110 = ((3.1528D4 * a_fuel_handling_bldg) + 9.181501D6) * 1.0D-6
    step21 = step21 + step2110

    ! 21.11 Control Room
    step2111 = ((1.2393D4 * a_control_room_bldg) + 1.924890D6) * 1.0D-6
    step21 = step21 + step2111

    ! 21.12 AC Power Supply Building
    step2112 = ((4.9755D4 * a_ac_ps_bldg) + 1.1591271D7) * 1.0D-6
    step21 = step21 + step2112

    ! 21.13 Admin Building
    step2113 = ((3.417D3 * a_admin_bldg) + 3.017077D6) * 1.0D-6
    step21 = step21 + step2113

    ! 21.14 Site Service
    step2114 = ((3.842D3 * a_site_service_bldg) + 1.193549D6) * 1.0D-6
    step21 = step21 + step2114

    ! 21.15 Cryogenics and Inert Gas Storage Building
    step2115 = ((7.031D3 * a_cryo_inert_gas_bldg) + 8.19004D5) * 1.0D-6
    step21 = step21 + step2115

    ! 21.16 Security Building
    step2116 = ((3.227D3 * a_security_bldg) + 2.06804D5) * 1.0D-6
    step21 = step21 + step2116

    ! 21.17 Ventilation Stack
    ! Original STARFIRE value, scaling with thermal power
    step2117 = step_ref(19) * (pth / ptherm_star)**0.6D0  
    step21 = step21 + step2117

    ! 21.18 Waste Facilities Buildings
    ! Fixed cost (2017 M$); read from input, default = 100 M$
    step2118 = wfbuilding / 1.0D6
    step21 = step21 + step2118

    ! 21.98 Spares
    ! STARFIRE percentage
    step2198 = 6.541D-3 * step21
    step21 = step21 + step2198

    ! 21.99 Contingency
    ! STARFIRE 15%
    step2199 = step_con * step21
    step21 = step21 + step2199

    ! Output costs
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
      call ocosts(outfile,'(step2118)','Waste Facilities Buildings (M$)', step2118)
      call ocosts(outfile,'(step2198)','Spares (M$)', step2198)
      call ocosts(outfile,'(step2199)','Contingency (M$)', step2199)
      call oblnkl(outfile)
      call ocosts(outfile,'(step21)','Total Account 21 Cost (M$)', step21)
    end if

  end subroutine step_a21

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine step_a22(outfile,iprint)

    !! Account 22 : Reactor Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22 (Reactor Plant Equipment)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    use process_output, only: oshead, ocosts, oblnkl
    use cost_variables, only: output_costs, step_con, step_rh_costfrac

    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile
  
    ! Local variables
    real(dp):: step2297, step2298, step2299
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! Initialise as zero
    step22 = 0.0D0
    step2298 = 0.0D0   ! Contingency
  
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
    ! STARFIRE percentage of components
    step22 = step22 + step2298
  
    ! 21.99 Contingency
    ! STARFIRE 15%
    step2299 = step_con * step22
    step22 = step22 + step2299

    ! Output costs
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

    !! Account 22.01 : Reactor Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.01 (Reactor Equipment)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cost_variables, only: output_costs, step_ref, ifueltyp, fcdfuel, &
      divcst, cdcost
    use current_drive_variables, only: pinjmw
    use physics_variables, only: rmajor, rminor
    use constants, only: pi
    use process_output, only: ocosts, oblnkl

    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile
    real(dp), intent(inout) :: step2298

    ! Local variables
    real(dp):: step2201, step220101, step22010101, step22010102, step2201010201, &
               step2201010202, step2201010203, step220102, step22010301, &
               step22010302, step22010303, step22010304, step220104, &
               step220105, step220106, step220107, step220108, step220109, &
               step220110
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! Initialise as zero
    step2201 = 0.0D0
     
    ! 22.01.01 Blanket and First Wall
    call step_a220101(step220101, step22010101, step22010102, step2201010201, &
                      step2201010202, step2201010203)
    step2201 = step2201 + step220101

    ! 22.01.02 Inboard Shield
    step220102 = step_a220102()
    ! Add shield  cost to total cost, step2201, in M$
    step2201 = step2201 + step220102
    ! STARFIRE percentage for spares
    step2298 = step2298 + 9.985D-2 *  step220102

    ! 22.01.03.01 TF Coils
    step22010301 = step_a22010301()
    ! Add TF coil cost to total cost, step2201, in M$
    step2201 = step2201 + step22010301

    ! 22.01.03.02 PF Coils
    step22010302 = step_a22010302()
    step2201 = step2201 + step22010302
    ! STARFIRE percentage for spares
    step2298 = step2298 + 3.269D-1 * step22010302

    ! 22.01.03.03 Central Solenoid
    ! Original STARFIRE value, scaling with fusion island volume
    step22010303 = step_ref(24) * (vfi / vfi_star)
    step2201 = step2201 + step22010303
    ! STARFIRE percentage for spares
    step2298 = step2298 + 6.124D-1 * step22010303

    ! 22.01.03.04 Control Coils
    ! Original STARFIRE value, scaling with fusion island volume
    step22010304 = step_ref(25) * (vfi / vfi_star)
    step2201 = step2201 + step22010304
    ! STARFIRE percentage for spares
    step2298 = step2298 + 1.075D-1 * step22010304
  
    ! 22.01.04 Auxiliary Heating and Current Drive
    ! Original STARFIRE value, scaling with auxiliary power
    step220104 = step_ref(26) * (pinjmw / pinjmw_star)
    if (ifueltyp==1) then
      step220104 = (1.0D0-fcdfuel) * step220104 
      cdcost = step220104
    end if
    step2201 = step2201 + step220104
    ! STARFIRE percentage for spares
    step2298 = step2298 + 2.335D-1 * step220104
  
    ! 22.01.05 Primary Structure and Support
    ! Original STARFIRE value, scaling with fusion island volume
    step220105 = step_ref(27) * (vfi / vfi_star)
    step2201 = step2201 + step220105
    ! STARFIRE percentage for spares
    step2298 = step2298 + 6.824D-2 * step220105
  
    ! 22.01.06 Reactor Vacuum System
    ! Original STARFIRE value, scaling with fusion island volume
    step220106 = step_ref(28) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2201 = step2201 + step220106
    ! STARFIRE percentage for spares
    step2298 = step2298 + 1.893D-1 * step220106
  
    ! 22.01.07 Power Supplies
    ! Original STARFIRE value, scaling with fusion island volume
    step220107 = step_ref(29) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2201 = step2201 + step220107
  
    ! 22.01.08 Impurity Control
    ! Original STARFIRE value, no scaling
    step220108 = step_ref(30)
    step2201 = step2201 + step220108
  
    ! 22.01.09 ECRH Plasma Breakdown
    ! Original STARFIRE value, no scaling
    step220109 = step_ref(31) 
    step2201 = step2201 + step220109

    ! 22.01.10 Divertor
    ! Cost Model 0 cost for STARFIRE sized device
    ! 58.62% increase between 1980 and 1990 
    ! http://www.in2013dollars.com/1980-dollars-in-1990
    ! Scaling with product of rmajor and rminor
    step220110 = step_ref(32) * ((rmajor*rminor)/(rmajor_star*rminor_star)) 
    if (ifueltyp == 1) then
      divcst = step220110
      step220110 = 0.0D0
    else
      divcst = 0.0D0
    end if
    step2201 = step2201 + step220110
  
    ! Add to Account 22 total
    step22 = step22 + step2201

    ! Output costs
    if ((iprint==1).and.(output_costs == 1)) then
      write(outfile,*) '******************* 22.01 Reactor Equipment'
      if (ifueltyp==0)then
        write(outfile,*) '******************* 22.01.01 Blanket and First Wall Equipment'
        call ocosts(outfile,'(step22010101)','Total First Wall Cost (M$)', step22010101)
        call oblnkl(outfile)
        call ocosts(outfile,'(step2201010201)','Blanket Multiplier Material (M$)', step2201010201)
        call ocosts(outfile,'(step2201010202)','Blanket Breeder Material (M$)', step2201010202)
        call ocosts(outfile,'(step2201010203)','Blanket Steel Costs (M$)', step2201010203)
        call ocosts(outfile,'(step22010102)','Total Blanket Cost (M$)', step22010102)
        call oblnkl(outfile)
        call ocosts(outfile,'(step220101)','Total Account 22.01.01 Cost (M$)', step220101)
        call oblnkl(outfile)
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

  subroutine step_a220101(step220101, step22010101, step22010102, step2201010201, &
                          step2201010202, step2201010203)

    !! Account 22.01.01 : Blanket and First Wall 
    !! author: A J Pearce, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.01.01 (BB+FW) costs.
    !! If ifueltyp = 0, the blanket cost is treated as capital cost
    !! If ifueltyp = 1, the blanket cost is treated as a fuel cost,
    !! rather than as a capital cost.
    !! If ifueltyp = 2, the initial blanket is included as a capital cost
    !! and the replacement blanket costs are treated as a fuel cost.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use cost_variables, only: step_ucblss, step_ucblbreed, step_ucblbe, ucblli, &
      step_ucblvd, ucblli2o, blkcst, ucbllipb, ifueltyp, lsa, step_ucfws, &
      fwallcst, step_ucfwps, step_ucfwa
		use fwbs_variables, only: blktmodel, whtblli, blkttype, wtblli2o, &
      whtblbreed, whtblvd, whtblbe, whtblss, wtbllipb, fw_armour_mass, fwmass 
    use build_variables, only: fwarea 
		use heat_transport_variables, only: ipowerflow 
    
    implicit none
    real(dp), intent(inout) :: step220101, step22010101, step22010102, step2201010201, &
                               step2201010202, step2201010203

    !  Local variables
    real(dp) :: step2201010204, step2201010205, step2201010206, step2201010207

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !! Account 22.01.01.01 : First wall
    step22010101 = 1.0D-6 * (fw_armour_mass * step_ucfwa + fwmass * step_ucfws) 

    if (ifueltyp == 1) then
       fwallcst = step22010101
       step22010101 = 0.0D0
    elseif (ifueltyp == 2) then
       fwallcst = step22010101
    else
       fwallcst = 0.0D0
    end if

    !! Account 22.01.01.02 : Breeder Blanket

    if (ipowerflow == 0) then
      
      !! Account 22.01.01.02.01 : Blanket Multiplier Material 
      step2201010201 = 1.0D-6 * whtblbe * step_ucblbe
          
      !! Account 22.01.01.02.02 : Blanket Breeder Material
      if (blktmodel == 0) then
             step2201010202 = 1.0D-6 * wtblli2o * step_ucblbreed
      else
             step2201010202 = 1.0D-6 * whtblbreed * step_ucblbreed
      end if
    else
      if ((blkttype == 1).or.(blkttype == 2)) then
         !  Liquid blanket (LiPb + Li)
         !! Account 22.01.01.02.01 : Blanket Multiplier Material 
        step2201010201 = 1.0D-6 * wtbllipb * ucbllipb * 2.99D0
         !! Account 22.01.01.02.02 : Blanket Breeder Material 
        step2201010202 = 1.0D-6 * whtblli * ucblli * 2.99D0
      else
         !  Solid blanket (Li2O + Be)
         !! Account 22.01.01.02.01 : Blanket Multiplier Material 
        step2201010201 = 1.0D-6 * whtblbe * step_ucblbe
         !! Account 22.01.01.02.02 : Blanket Breeder Material
        step2201010202 = 1.0D-6 * wtblli2o * step_ucblbreed
      end if
    end if

    !! Account 22.01.01.02.03 : Blanket Steel Costs
    step2201010203 = 1.0D-6 * whtblss * step_ucblss
    
    !! Account 22.01.01.02.04 : Blanket Vanadium Costs
    step2201010204 = 1.0D-6 * whtblvd * step_ucblvd
       
    !! Account 22.01.01.02.05 : Blanket Carbon Cloth Costs
    step2201010205 = 0.0D0
       
    !! Account 22.01.01.02.06 : Blanket Concrete Costs
    step2201010206 = 0.0D0

    !! Account 22.01.01.02.07 : Blanket FLiBe Costs
    step2201010207 = 0.0D0

    step22010102 = step2201010201 + step2201010202 + step2201010203 + step2201010204 &
     + step2201010205 + step2201010206 + step2201010207

    if (ifueltyp == 1) then
       blkcst = step22010102
       step22010102 = 0.0D0
    elseif (ifueltyp == 2) then
       blkcst = step22010102
    else
       blkcst = 0.0D0
    end if

    !! Total for Account 22.01.01
    step220101 = step22010101 + step22010102 

  end subroutine step_a220101

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function step_a220102() result(step220102)
    !! 22.01.02
    !! Returns cost of inboard shield

    use build_variables, only: rsldi, shldith, shldtth, vgap, &
      scrapli, scraplo, fwith, fwoth, blnktth
    use fwbs_variables, only: i_shield_mat, denw, denwc
    use divertor_variables, only: divfix
    use cost_variables, only: step_ucshw, step_ucshwc
    use physics_variables, only: rminor, kappa, idivrt
    use constants, only: pi
    implicit none

    ! Result
    real(dp) :: step220102
    !! Cost of inboard shield in M$

    ! Local variables
    real(dp):: inb_sh_v, r1, hbot, htop, hshld, &
      inb_sh_v_mtl, inb_sh_m, sh_mtl_d, sh_mtl_c    

    ! Volume of inboard shield found using same method as in CCFE HCPB blanket model:
    ! inboard shield is assumed to be a cylinder of uniform thickness

    ! Calculate shield internal half-height (m)
    hbot = rminor*kappa + vgap + divfix
    ! if a double null machine then symmetric otherwise asymmetric
    if ( idivrt == 2 ) then
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) + blnktth
    end if
    ! Average of top and bottom (m)
    hshld = 0.5D0*(htop + hbot)

    ! Radius to outer edge of inboard shield (m)
    r1 = rsldi + shldith
    ! Volume of inboard cylindrical shell (m3)
    inb_sh_v = 2.0D0*(hshld+shldtth) * pi*(r1**2 - (r1-shldith)**2)

    ! Scale shield material volume (allow for 10% volume coolant, 5% steel)
    inb_sh_v_mtl = 0.85D0 * inb_sh_v
    
    ! Define shield material density (sh_mtl_d [kg/m3]) and cost (sh_mtl_c [$/kg])
    if ( i_shield_mat == 1 ) then
      ! tungsten carbide
      sh_mtl_d = denwc
      sh_mtl_c = step_ucshwc
    else
      ! tungsten (default)
      sh_mtl_d = denw
      sh_mtl_c = step_ucshw
    end if

    ! Find inboard shield mass (kg) 
    inb_sh_m = inb_sh_v_mtl * sh_mtl_d

    ! Find inboard shield cost (converted to M$2017)
    step220102 = (inb_sh_m * sh_mtl_c) / 1.0D6*(229.0D0/264.71D0)

  end function step_a220102

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function step_a22010301() result(step22010301)
    !! 22.01.03.01 TF Coils
    !! Returns cost of TF coils
    use cost_variables, only: step_ref, cpstcst, ifueltyp, step_uc_cryo_al, &
      step_mc_cryo_al_per, uccpcl1, uccpclb
    use tfcoil_variables, only: i_tf_sup, whtconal, n_tf, whttflgs, whtcp
    use physics_variables, only: itart
    implicit none

    ! Result
    real(dp) :: step22010301
    !! Cost of TF coils in M$
    
    ! Declare local vars
    real(dp) :: c_tf_inboard_legs
    !! Cost of TF coil inboard legs in M$
    real(dp) :: c_tf_outboard_legs
    !! Cost of TF coil outboard legs in M$
    
    ! Initialise local vars
    c_tf_inboard_legs = 0.0D0
    c_tf_outboard_legs = 0.0D0
    
    ! Copper coils
    if (i_tf_sup == 0) then
      ! Calculation taken from cost model 0: simply the cost of copper conductor
      ! masses
      ! Inflating from 1990 $ to 2017 $ at nuclear rate equates to a factor of 
      ! 2.99
      ! Inboard TF coil legs
      c_tf_inboard_legs = 1.0D-6 * whtcp * uccpcl1 * 2.99D0
      
      ! Outboard TF coil legs
      c_tf_outboard_legs = 1.0D-6 * whttflgs * uccpclb * 2.99D0
      
      ! Total TF coil cost
      step22010301 = c_tf_inboard_legs + c_tf_outboard_legs
    endif
      
    ! Superconducting coils
    if (i_tf_sup == 1) then
      ! Original STARFIRE value in M$, scaling with fusion island volume
      step22010301 = step_ref(22) * (vfi / vfi_star)
    endif
    
    ! Cryogenic aluminium coils
    if (i_tf_sup == 2) then
      ! Cost approximated as the material cost of conducting Al * a 
      ! manufacturing cost factor
      ! Al conductor mass per coil * number of coils * cost per kilo *
      ! manufacturing cost factor, converted to M$
      ! step_mc_cryo_al_per = 0.2: 20% manufacturing cost
      step22010301 = (whtconal * n_tf * step_uc_cryo_al) * &
        (step_mc_cryo_al_per + 1.0D0) * 1.0D-6
    endif

    ! ifueltyp: consider centrepost cost as fuel, capital or both?
    ! cpstcst used later in coelc_step()
    cpstcst = 0.0D0  ! TART centrepost
    if (itart == 1) then
      if (ifueltyp == 1) then
        ! Treat centrepost cost as fuel cost
        cpstcst = c_tf_inboard_legs
        if (i_tf_sup == 0) then
          ! Subtract from capital cost
          step22010301 = step22010301 - c_tf_inboard_legs
        endif
      elseif (ifueltyp == 2) then
        ! Treat centrepost cost as capital and fuel cost
        cpstcst = c_tf_inboard_legs
      end if
    endif
  end function step_a22010301


  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function step_a22010302() result(step22010302)

    !! Account 22.01.03.02 PF Coils : PF magnet assemblies
    !! author: A J Pearce, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.01.03.02 (PF magnet) costs.
    !! Conductor costs previously used an algorithm devised by R. Hancox,
    !! January 1994, under contract to Culham, which took into
    !! account the fact that the superconductor/copper ratio in
    !! the conductor is proportional to the maximum field that
    !! each coil will experience. Now, the input copper fractions
    !! are used instead.
    !! Maximum values for current, current density and field
    !! are used. 
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use build_variables, only: iohcl 
		use constants, only: twopi, dcopper
		use cost_variables, only: step_uccase, step_uccu, step_cconshpf, step_ucfnc, &
      step_cconfix, step_ucsc, step_ucwindpf, lsa
		use pfcoil_variables, only: rjconpf, ipfres, vfohc, nohc, turns, isumatpf, &
      whtpfs, ric, rpf, isumatoh, fcupfsu, fcuohsu, vf, awpoh 
		use structure_variables, only: fncmass 
    use tfcoil_variables, only: dcond
    implicit none

    !  Result
    real(dp) :: step22010302
     
    !  Local variables
    real(dp) :: costpfcu,costpfsc,costpfsh,costwire,cpfconpm, &
         pfwndl, step2201030201, step2201030202, step2201030203, step2201030204
    integer :: i,npf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Total length of PF coil windings (m)

    pfwndl = 0.0D0
    do i = 1,nohc
       pfwndl = pfwndl + twopi*rpf(i)*turns(i)
    end do

    !  Account 22.01.03.02.01 : Conductor

    !  The following lines take care of resistive coils.
    !  costpfsh is the cost per metre of the steel conduit/sheath around
    !  each superconducting cable (so is zero for resistive coils)

    if (ipfres == 1) then
       costpfsh = 0.0D0
    else
       costpfsh = step_cconshpf
    end if

    !  Non-Central Solenoid coils

    if (iohcl == 1) then
       npf = nohc-1
    else
       npf = nohc
    end if
    
    step2201030201 = 0.0D0
    do i = 1,npf

       !  Superconductor ($/m)
       if (ipfres == 0) then
          costpfsc = step_ucsc(isumatpf) * (1.0D0-fcupfsu)*(1.0D0-vf(i)) * &
               abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcond(isumatpf)
       else
          costpfsc = 0.0D0
       end if

       !  Copper ($/m)
       if (ipfres == 0) then
          costpfcu = step_uccu * fcupfsu*(1.0D0-vf(i)) * &
               abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcopper
       else
          costpfcu = step_uccu * (1.0D0-vf(i)) * &
               abs(ric(i)/turns(i))*1.0D6 / rjconpf(i) * dcopper
       end if

       !  Total cost/metre of superconductor and copper wire
       costwire = costpfsc + costpfcu

       !  Total cost/metre of conductor (including sheath and fixed costs)
       cpfconpm = costwire + costpfsh + step_cconfix

       !  Total account 222.2.1 (PF coils excluding Central Solenoid)
       step2201030201 = step2201030201 + (1.0D-6 * twopi * rpf(i) * turns(i) * &
            cpfconpm)

    end do

    !  Account 22.01.03.02.02 : Winding
    step2201030202 = 1.0D-6 * step_ucwindpf * pfwndl
 
    !  Account 22.01.03.02.03 : Steel case - will be zero for resistive coils
    step2201030203 = 1.0D-6 * step_uccase * whtpfs
 
    !  Account 22.01.03.02.04 : Support structure
    step2201030204 = 1.0D-6 * step_ucfnc * fncmass

    !  Total account 22.01.03.02
    step22010302 = step2201030201 + step2201030202 + step2201030203 + step2201030204

    end function step_a22010302

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine step_a2202(outfile,iprint)

    !! Account 22.02 : Heat Transfer System
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.02 (Heat Transfer System)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    use cost_variables, only: output_costs
    use process_output, only: ocosts, oblnkl
    use heat_transport_variables, only: pgrossmw
    ! pgrossmw is gross electric power of the plant in MW

    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile
  
    ! Local variables
    real(dp):: step2202
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! Initialise as zero
    step2202 = 0.0D0
     
    ! 22.02 Heat Transfer System
    ! #TODO Needs reference for values
    step2202 = 9.2238D4 * pgrossmw * 1.0D-6
    ! Converted to M$
  
    ! Add to Account 22 total
    step22 = step22 + step2202

    ! Output costs
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

    !! Account 22.03 : Cryogenic Cooling System
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.03 (Cryogenic Cooling
    !! System) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    use cost_variables, only: output_costs, step_ref
    use process_output, only: ocosts, oblnkl

    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile
  
    ! Local variables
    real(dp):: &
    step220301, step220302, step220303, step220304, step2203
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! Initialise as zero
    step2203 = 0.0D0
     
    ! 22.03.01 Helium Refrigerator
    ! Original STARFIRE value, scaling with fusion island volume
    step220301 = step_ref(34) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2203 = step2203 + step220301
  
    ! 22.03.02 Liquid Helium Transfer and Storage
    ! Original STARFIRE value, scaling with fusion island volume
    step220302 = step_ref(35) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2203 = step2203 + step220302
  
    ! 22.03.03 Gas Helium Storage
    ! Original STARFIRE value, scaling with fusion island volume
    step220303 = step_ref(36) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2203 = step2203 + step220303
  
    ! 22.03.04 Liquid Nitrogen Storage
    ! Original STARFIRE value, scaling with fusion island volume
    step220304 = step_ref(37) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2203 = step2203 + step220304
  
    ! Add to Account 22 total
    step22 = step22 + step2203

    ! Output costs
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

    !! Account 22.04 : Waste Treatment and Disposal
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.04 (Waste Treatment
    !! and Disposal) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    use cost_variables, only: output_costs, step_ref
    use process_output, only: ocosts, oblnkl

    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile
  
    ! Local variables
    real(dp):: &
    step220401, step220402, step220403, step2204
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! Initialise as zero
    step2204 = 0.0D0
     
    ! 22.04.01 Liquid Waste
    ! Original STARFIRE value, scaling with thermal power
    step220401 = step_ref(38) * (pth / ptherm_star)**0.6D0 
    step2204 = step2204 + step220401
  
    ! 22.04.02 Gaseous Waste
    ! Original STARFIRE value, scaling with thermal power
    step220402 = step_ref(39) * (pth / ptherm_star)**0.6D0 
    step2204 = step2204 + step220402
  
    ! 22.04.03 Solid Waste
    ! Original STARFIRE value, scaling with thermal power
    step220403 = step_ref(40) * (pth / ptherm_star)**0.6D0 
    step2204 = step2204 + step220403
  
    ! Add to Account 22 total
    step22 = step22 + step2204

    ! Output costs
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

    !! Account 22.05 : Fuel Handling and Storage
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.05 (Fuel Handling
    !! and Storage) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    use cost_variables, only: output_costs, step_ref
    use process_output, only: ocosts, oblnkl

    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile
    real(dp), intent(inout) :: step2298
  
    ! Local variables
    real(dp):: step2205
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! Initialise as zero
    step2205 = 0.0D0
     
    ! 22.05 Fuel Handling and Storage
    ! Original STARFIRE value, scaling with thermal power
    step2205 = step_ref(41) * (pth / ptherm_star)**0.6D0 

    ! Add to Account 22 total
    step22 = step22 + step2205
    ! STARFIRE percentage for spares
    step2298 = step2298 + 5.026D-2 * step2205

    ! Output costs
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

    !! Account 22.06 : Other Reactor Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.06 (Other Reactor
    !! Plant Equipment) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    use cost_variables, only: output_costs, step_ref
    use process_output, only: ocosts, oblnkl

    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile
    real(dp), intent(inout) :: step2298
  
    ! Local variables
    real(dp):: &
    step220601, step220602, step220603, step220604, step220605, &
    step220606, step220607, step220608, step2206
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! Initialise as zero
    step2206 = 0.0D0
     
    ! 22.06.01 Maintenance Equipment
    ! Original STARFIRE value, scaling with fusion island volume
    ! Depreciated by the remote handling scaling in cost account 27. 
    step220601 = 0.0 ! step_ref(42) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2206 = step2206 + step220601
    ! STARFIRE percentage for spares
    step2298 = step2298 + 4.308D-1 * step220601
  
    ! 22.06.02 Special Heating Systems
    ! Original STARFIRE value, scaling with thermal power
    step220602 = step_ref(43) * (pth / ptherm_star)**0.6D0
    step2206 = step2206 + step220602
  
    ! 22.06.03 Coolant Storage
    ! Original STARFIRE value, scaling with thermal power
    step220603 = step_ref(44) * (pth / ptherm_star)**0.6D0
    step2206 = step2206 + step220603
  
    ! 22.06.04 Gas System
    ! Original STARFIRE value, scaling with fusion island volume
    step220604 = step_ref(45) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2206 = step2206 + step220604
  
    ! 22.06.05 Inert Atmosphere System
    ! Original STARFIRE value, scaling with thermal power
    step220605 = step_ref(46) * (pth / ptherm_star)**0.6D0
    step2206 = step2206 + step220605
  
    ! 22.06.06 Fluid Leak Detection
    ! Original STARFIRE value, scaling with thermal power
    step220606 = step_ref(47) * (pth / ptherm_star)**0.6D0
    step2206 = step2206 + step220606
  
    ! 22.06.07 Closed Loop Coolant System
    ! Original STARFIRE value, scaling with thermal power
    step220607 = step_ref(48) * (pth / ptherm_star)**0.6D0
    step2206 = step2206 + step220607
    ! STARFIRE percentage for spares
    step2298 = step2298 + 8.3D-1 * (pth / ptherm_star)**0.6D0
  
    ! 22.06.08 Standby Cooling System
    ! Original STARFIRE value, scaling with thermal power
    step220608 = step_ref(49) * (pth / ptherm_star)**0.6D0
    step2206 = step2206 + step220608
  
    ! Add to Account 22 total
    step22 = step22 + step2206

    ! Output costs
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

    !! Account 22.07 : Instrumentation and Control
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.07 (Instrumentation
    !! and Control) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    use cost_variables, only: output_costs,step_ref
    use process_output, only: ocosts, oblnkl

    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile

    ! Local variables
    real(dp):: step2207
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    ! Initialise as zero
    step2207 = 0.0D0
     
    ! 22.07 Instrumentation and Control
    ! Original STARFIRE value, scaling with thermal power
    step2207 = step_ref(50) * (pth / ptherm_star)**0.6D0
  
    ! Add to Account 22 total
    step22 = step22 + step2207

    ! Output costs
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

    !! Account 23 : Turbine Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 23 (Turbine Plant Equipment)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cost_variables, only: output_costs, step_con, step_ref
    use process_output, only: oshead, ocosts, oblnkl
    use heat_transport_variables, only: pgrossmw

    implicit none

    ! Arguments
    integer, intent(in) :: iprint,outfile

    ! Local variables
    real(dp):: step23a, step2303, step2398, step2399

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Initialise as zero
    step23 = 0.0D0
   
    ! 23.01 Turbine Generators
    ! 23.02 Steam System
    ! 23.04 Condensing System
    ! 23.05 Feedwater Heating System
    ! 23.06 Other Turbine Equipment
    ! 23.07 Instrumentation and Control
    ! step23a is the sum of the above accounts: total turbine system
    ! cost, not treating cooling towers as part of the turbine system
    step23a = 5.55440D5 * pgrossmw * 1.0D-6
    step23 = step23 + step23a

    ! 23.03 Heat Rejection
    step2303 = ((8.0437D4 * pgrossmw) + 2.2264895D7) * 1.0D-6
    step23 = step23 + step2303

    ! 23.98 Spares
    ! STARFIRE percentage
    step2398 = 1.401D-2 * step23
    step23 = step23 + step2398

    ! 23.99 Contingency
    ! STARFIRE 15%
    step2399 = step_con * step23
    step23 = step23 + step2399

    ! Output costs
    if ((iprint==1).and.(output_costs == 1)) then
      call oshead(outfile,'23. Turbine Plant Equipment')
      call ocosts(outfile,'(step23a)','Turbine System (M$)', step23a)
      call ocosts(outfile,'(step2303)','Heat Rejection (M$)', step2303)
      call ocosts(outfile,'(step2398)','Spares (M$)', step2398)
      call ocosts(outfile,'(step2399)','Contingency (M$)', step2399)
      call oblnkl(outfile)
      call ocosts(outfile,'(step23)','Total Account 23 Cost (M$)', step23)
    end if
  end subroutine step_a23

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine step_a24(outfile,iprint)

    !! Account 24 : Electric Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 24 (Electric Plant 
    !! Equipment) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cost_variables, only: output_costs, step_con, step_ref
    use process_output, only: oshead, ocosts, oblnkl
    use heat_transport_variables, only: pgrossmw

    implicit none

    ! Arguments
    integer, intent(in) :: iprint,outfile

    ! Local variables
    real(dp):: &
    step2401, step2402, step2403, step2404, step2405, step2406, &
    step2407, step2498, step2499

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Initialise as zero M$
    step24 = 0.0D0
    
    ! 24.01 Switch Gear
    step2401 = 1.8906D4 * pgrossmw * 1.0D-6
    step24 = step24 + step2401
    
    ! 24.02 Station Service Equipment
    step2402 = 5.1412D4 * pgrossmw * 1.0D-6
    step24 = step24 + step2402
    
    ! 24.03 Switchboards
    step2403 = 2.985D3 * pgrossmw * 1.0D-6
    step24 = step24 + step2403
    
    ! 24.04 Protective Equipment
    step2404 = ((3.05D4 * (pgrossmw / 1.2D3) * 18.0D0) + (4.0D6 * (pgrossmw / 1.2D3))) * 1.0D-6
    step24 = step24 + step2404
    
    ! 24.05 Electrical Structures
    step2405 = ((3.05D4 * (pgrossmw / 1.2D3) * 1.3D2) + (4.0D6 * 9.0D0 * (pgrossmw / 1.2D3))) * 1.0D-6
    step24 = step24 + step2405
    
    ! 24.06 Power and Control Wiring
    step2406 = 2.8989D4 * pgrossmw * 1.0D-6
    step24 = step24 + step2406
    
    ! 24.07 Electric Lighting
    step2407 = ((3.05D4 * (pgrossmw / 1.2D3) * 2.0D2) + (4.0D6 * 4.0D0 * (pgrossmw / 1.2D3))) * 1.0D-6
    step24 = step24 + step2407

    ! 24.98 Spares
    ! STARFIRE percentage
    step2498 = 1.0403D-2 * step24
    step24 = step24 + step2498

    ! 24.99 Contingency
    ! STARFIRE 15%
    step2499 = step_con * step24
    step24 = step24 + step2499

    ! Output costs
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

    !! Account 25 : Miscellaneous Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 25 (Miscellaneous Plant 
    !! Equipment) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! #TODO Need to add reference for cost calculations
    use cost_variables, only: output_costs, step_con, step_ref
    use process_output, only: oshead, ocosts, oblnkl
    use heat_transport_variables, only: pgrossmw
    use buildings_variables, only: wgt
    
    implicit none
  
    ! Arguments
    integer, intent(in) :: iprint,outfile
    
    ! Local variables
    real(dp):: &
    step2501, step2502, step2503, step2504, step2598, step2599
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ! Initialise as zero
    step25 = 0.0D0
    
    ! 25.01 Transport and Lifting Equipment
    step2501 = ((3.8005D4 * (wgt / 1.0D3)) + 1.529727D6) * 1.0D-6
    ! wgt is reactor building crane capacity (kg)
    ! #TODO Check that wgt is the correct variable to use here
    step25 = step25 + step2501
    
    ! 25.02 Air and Water Service System
    step2502 = 1.20689D5 * pgrossmw * 1.0D-6
    step25 = step25 + step2502
    
    ! 25.03 Communications Equipment
    step2503 = ((3.05D4 * (pgrossmw / 1.2D3) * 2.18D2) + (4.0D6 * 3.0D0 * (pgrossmw / 1.2D3))) * 1.0D-6
    step25 = step25 + step2503
    
    ! 25.04 Furnishing and Fixtures
    step2504 = 3.0D3 * pgrossmw * 1.0D-6
    step25 = step25 + step2504
  
    ! 25.98 Spares
    ! Original STARFIRE value, no scaling
    step2598 = 1.286D-2 * step25
    step25 = step25 + step2598
  
    ! 25.99 Contingency
    ! STARFIRE 15%
    step2599 = step_con * step25
    step25 = step25 + step2599
  
    ! Output costs
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

  subroutine step_a27(outfile,iprint)

    !! Account 27 : Remote Handling
    !! author: A J Pearce, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 27 (Remote Handling)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cost_variables, only: output_costs, cdirt, step_rh_costfrac
    use process_output, only: oshead, ocosts, oblnkl

    implicit none

    ! Arguments
    integer, intent(in) :: iprint,outfile

    ! Local variables
    real(dp):: step2701

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Initialise as zero
    step27 = 0.0D0
   
    ! 27.01 Remote Handling 
    ! From report by T. Hender CD-STEP-01030, scales with direct capital costs
    step2701 = step_rh_costfrac * cdirt 
    
    step27 = step2701

    ! Output costs
    if ((iprint==1).and.(output_costs == 1)) then
      call oshead(outfile,'27. Remote Handling')
      call ocosts(outfile,'(step2701)','Remote Handing (M$)', step2701)
      call oblnkl(outfile)
      call ocosts(outfile,'(step27)','Total Account 27 Cost (M$)', step27)
    end if

  end subroutine step_a27

  subroutine step_indirect_costs(outfile, iprint)
    !! Accounts 91-93: Indirect costs
    !! Calculate the indirect costs and print
    use cost_variables, only: cdirt, output_costs
    use process_output, only: oshead, ocosts
    use cost_variables, only: step91_per, step92_per, step93_per
    implicit none

    ! Arguments
    integer, intent(in) :: outfile, iprint

    ! Account 91 : Construction Facilities, Equipment and Services (default 30%)
    step91 = step91_per * cdirt

    ! Account 92 : Engineering and Costruction Management Services (default 32.5%)
    step92 = step92_per * cdirt

    ! Account 93 : Other Costs (default 5%)
    step93 = step93_per * cdirt

    if ((iprint==1).and.(output_costs == 1)) then
      call oshead(outfile,'Indirect Cost')
      call ocosts(outfile,'(step91)','Construction Facilities, Equipment and Services (M$)',step91)
      call ocosts(outfile,'(step92)','Engineering and Costruction Management Services (M$)',step92)
      call ocosts(outfile,'(step93)','Other Costs (M$)',step93)
    endif
  end subroutine step_indirect_costs

  subroutine coelc_step(outfile,iprint)

    !! Routine to calculate the cost of electricity for a fusion power plant
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine performs the calculation of the cost of electricity
    !! for a fusion power plant.
    !! <P>Annual costs are in megadollars/year, electricity costs are in
    !! millidollars/kWh, while other costs are in megadollars.
    !! All values are based on 1980 dollars.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use cost_variables, only: output_costs, discount_rate, tlife, ucfuel, uche3, cdcost, &
      divcst, fcdfuel, ifueltyp, moneyint, lsa, ucwst, ucoam, fwallcst, fcr0, fcap0cp, &
      cfind, fcap0, dtlife, divlife, dintrt, decomf, cpstcst, cplife, concost, coeoam, &
      coefuelt, coecap, coe, cfactr, cdrlife, capcost, step_ref, step_currency
    use fwbs_variables, only: bktlife
    use heat_transport_variables, only: pnetelmw
    use physics_variables, only: fhe3, itart, wtgpd
    use times_variables, only: tburn, tcycle
    use process_output, only: oshead, ocosts, oblnkl,  ovarrf, osubhd, oheadr
    use constants, only: n_day_year

    implicit none

    ! Arguments
    integer, intent(in) :: iprint,outfile

    ! Local variables
    real(dp) :: anncap,anncdr,anncp,anndecom,anndiv,annfuel, &
         annfuelt,annfwbl,annoam,anntot,annwst,coecdr, &
         coecp,coedecom,coediv,coefuel,coefwbl,coewst,crfcdr,crfcp, &
         crfdiv,crffwbl,fefcdr,fefcp,fefdiv,feffwbl,fwbllife,kwhpy
    character(len=80) :: title
         ! annoam1,
         
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Number of kWh generated each year
    kwhpy = 1.0D3 * pnetelmw * (24.0D0*n_day_year) * cfactr * tburn/tcycle

    ! Costs due to reactor plant
    ! ==========================

    ! Interest on construction costs
    moneyint = concost * (fcap0 - 1.0D0)

    ! Capital costs
    capcost = concost + moneyint

    ! Annual cost of plant capital cost
    anncap = capcost * fcr0

    ! Cost of electricity due to plant capital cost
    coecap = 1.0D9 * anncap / kwhpy

    ! Costs due to first wall and blanket renewal
    ! ===========================================

    ! Operational life
    fwbllife = bktlife

    ! Compound interest factor
    feffwbl = (1.0D0 + discount_rate)**fwbllife

    ! Capital recovery factor
    crffwbl = (feffwbl*discount_rate) / (feffwbl-1.0D0)

    ! Annual cost of replacements
    annfwbl = fwblkcost * (1.0D0+cfind(lsa)) * fcap0cp * crffwbl

    ! Cost of electricity due to first wall/blanket replacements
    coefwbl = 1.0D9 * annfwbl / kwhpy

    ! Costs due to divertor renewal
    ! =============================

    ! Compound interest factor
    fefdiv = (1.0D0 + discount_rate)**divlife

    ! Capital recovery factor
    crfdiv = (fefdiv*discount_rate) / (fefdiv-1.0D0)

    ! Annual cost of replacements
    anndiv = divcst * (1.0D0+cfind(lsa)) * fcap0cp * crfdiv

    ! Cost of electricity due to divertor replacements
    coediv = 1.0D9 * anndiv / kwhpy

    ! Costs due to centrepost renewal
    ! ===============================

    if (itart == 1) then
      ! Compound interest factor
      fefcp = (1.0D0 + discount_rate)**cplife

      ! Capital recovery factor
      crfcp = (fefcp*discount_rate) / (fefcp-1.0D0)

      ! Annual cost of replacements
      anncp = cpstcst * (1.0D0+cfind(lsa)) * fcap0cp * crfcp

      ! Cost of electricity due to centrepost replacements
      coecp = 1.0D9 * anncp / kwhpy
    else
      anncp = 0.0D0
      coecp = 0.0D0
    end if

    ! Costs due to partial current drive system renewal
    ! =================================================

    ! Compound interest factor
    fefcdr = (1.0D0 + discount_rate)**cdrlife

    ! Capital recovery factor
    crfcdr = (fefcdr*discount_rate) / (fefcdr-1.0D0)

    ! Annual cost of replacements
    if (ifueltyp == 0) then
      anncdr = 0.0D0
    else
      anncdr = cdcost * fcdfuel/(1.0D0-fcdfuel) * (1.0D0+cfind(lsa)) * fcap0cp * crfcdr
    end if

    ! Cost of electricity due to current drive system replacements
    coecdr = 1.0D9 * anncdr / kwhpy

    ! Costs due to operation and maintenance
    ! ======================================

    ! Annual cost of operation and maintenance
    annoam = ucoam(lsa) * sqrt(pnetelmw/1200.0D0)

  ! Additional cost due to pulsed reactor thermal storage
  ! See F/MPE/MOD/CAG/PROCESS/PULSE/0008
  !
  ! if (lpulse.eq.1) then
  !   if (istore.eq.1) then
  !     annoam1 = 51.0D0
  !   else if (istore.eq.2) then
  !     annoam1 = 22.2D0
  !   else
  !     continue
  !   end if
  !
  !   Scale with net electric power
  !   annoam1 = annoam1 * pnetelmw/1200.0D0
  !
  !   It is necessary to convert from 1992 pounds to 1990 dollars
  !   Reasonable guess for the exchange rate + inflation factor
  !   inflation = 5% per annum; exchange rate = 1.5 dollars per pound
  !
  !   annoam1 = annoam1 * 1.36D0
  !
  !   annoam = annoam + annoam1
  !
  !  end if

  !  Cost of electricity due to operation and maintenance
  coeoam = 1.0D9 * annoam / kwhpy

  !  Costs due to reactor fuel
  !  =========================

  !  Annual cost of fuel

  !  Sum D-T fuel cost and He3 fuel cost
  annfuel = ucfuel * pnetelmw/1200.0D0 + 1.0D-6 * fhe3 * wtgpd * 1.0D-3 * uche3 * n_day_year * cfactr

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
  anndecom = decomf * concost * fcr0 / (1.0D0+discount_rate-dintrt)**(tlife-dtlife)

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

  title = 'Cost of Electricity ('// trim(step_currency) // ')'
  call oheadr(outfile,trim(title))

  call ovarrf(outfile,'First wall / blanket life (years)','(fwbllife)',fwbllife)
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
    call ovarrf(outfile,'First wall and Blanket direct capital cost (M$)','(fwblkcost)',fwblkcost)

    call ovarrf(outfile,'Divertor direct capital cost (M$)','(divcst)',divcst)
    if (itart == 1) then
      call ovarrf(outfile,'Centrepost direct capital cost (M$)','(cpstcst)',cpstcst)
    end if
    call ovarrf(outfile,'Plasma heating/CD system cap cost (M$)','',cdcost*fcdfuel/(1.0D0-fcdfuel))
    call ovarrf(outfile,'Fraction of CD cost --> fuel cost','(fcdfuel)',fcdfuel)

  end if

  end subroutine coelc_step

end module costs_step_module
