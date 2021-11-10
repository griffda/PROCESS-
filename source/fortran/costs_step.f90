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

#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
  implicit none

  !  Various cost account values (M$)
  real(dp) :: step20, step21, step22, step23, step24, step25, &
              step27, step91, step92, step93, fwblkcost

  ! Scaling Properties
  real(dp) :: vfi, vfi_star, ptherm_star, &
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
    rmajor_star = 0.0D0
    rminor_star = 0.0D0
    pth = 0.0D0
  end subroutine init_costs_step

  subroutine step_a20(step_ref, sitecost, step2001, step2002, step20)
    !! Account 20 : Land and Rights
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! This routine evaluates the Account 20 (Land and Rights)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none

    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: sitecost
    real(dp), intent(out) :: step2001, step2002, step20

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
  end subroutine step_a20

  subroutine step_a21(step_ref, step_con, wfbuilding, a_reactor_bldg, &
    a_ee_ps_bldg, a_aux_services_bldg, a_hot_cell_bldg, &
    a_reactor_service_bldg, a_service_water_bldg, a_fuel_handling_bldg, &
    a_control_room_bldg, a_ac_ps_bldg, a_admin_bldg, a_site_service_bldg, &
    a_cryo_inert_gas_bldg, a_security_bldg, pgrossmw, &
    pth, ptherm_star, &
    step2101, step2102, step2103, step2104, step2105, &
    step2106, step2107, step2108,  step2109, step2110, step2111, step2112, &
    step2113, step2114, step2115, step2116, step2117, step2118, step2198, &
    step2199, step21)
    !! Account 21 : Building and Site Service Infrastructure
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! This routine evaluates the Account 21 (Building and Site
    !! Service Infrastructure) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    ! #TODO Add reference for STEP cost values
    ! Floor areas in m^2 for buildings
    ! pgrossmw is gross electric power of the plant in MW
    implicit none

    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: step_con, wfbuilding, a_reactor_bldg, &
      a_ee_ps_bldg, a_aux_services_bldg, a_hot_cell_bldg, &
      a_reactor_service_bldg, a_service_water_bldg, a_fuel_handling_bldg, &
      a_control_room_bldg, a_ac_ps_bldg, a_admin_bldg, a_site_service_bldg, &
      a_cryo_inert_gas_bldg, a_security_bldg, pgrossmw, pth, ptherm_star
    real(dp), intent(out) :: step2101, step2102, step2103, step2104, step2105, &
      step2106, step2107, step2108, step2109, step2110, step2111, step2112, &
      step2113, step2114, step2115, step2116, step2117, step2118, step2198, &
      step2199, step21

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
  end subroutine step_a21

  subroutine step_a2201(step_ref, ifueltyp, fcdfuel, &
    rmajor, rminor, step220101, step220102, step22010301, &
    step22010302, step220104, step2201, spares, divcst, cdcost, step22010303, &
    step22010304, step220105, step220106, step220107, step220108, &
    step220109, step220110)
    !! Account 22.01 : Reactor Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.01 (Reactor Equipment)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none
  
    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: ifueltyp, fcdfuel, &
      rmajor, rminor, step220101, step220102, step22010301, step22010302, &
      step220104
    real(dp), intent(out) :: step2201, spares, divcst, cdcost, &
      step22010303, step22010304, step220105, step220106, &
      step220107, step220108, step220109, step220110
  
    ! Initialise as zero
    spares = 0.0D0

    ! 22.01.01 Blanket and First Wall
    step2201 = step220101

    ! 22.01.02 Shield
    ! Inboard shield costs:
    ! Note: outboard shield costs currently set to zero.
    ! Add shield cost to total cost, step2201, in M$
    step2201 = step2201 + step220102
    ! STARFIRE percentage for spares
    spares = 9.985D-2 *  step220102
  
    ! 22.01.03.01 TF Coils
    ! Add TF coil cost to total cost, step2201, in M$
    step2201 = step2201 + step22010301

    ! 22.01.03.02 PF Coils
    step2201 = step2201 + step22010302
    ! STARFIRE percentage for spares
    spares = spares + 3.269D-1 * step22010302

    ! 22.01.03.03 Central Solenoid
    ! Original STARFIRE value, scaling with fusion island volume
    step22010303 = step_ref(24) * (vfi / vfi_star)
    step2201 = step2201 + step22010303
    ! STARFIRE percentage for spares
    spares = spares + 6.124D-1 * step22010303

    ! 22.01.03.04 Control Coils
    ! Original STARFIRE value, scaling with fusion island volume
    step22010304 = step_ref(25) * (vfi / vfi_star)
    step2201 = step2201 + step22010304
    ! STARFIRE percentage for spares
    spares = spares + 1.075D-1 * step22010304
  
    ! 22.01.04 Auxiliary Heating and Current Drive
    ! HCD cost = cost per injected Watt of power * injected Watts
    step2201 = step2201 + step220104
    ! STARFIRE percentage for spares
    spares = spares + 2.335D-1 * step220104
  
    ! 22.01.05 Primary Structure and Support
    ! Original STARFIRE value, scaling with fusion island volume
    step220105 = step_ref(27) * (vfi / vfi_star)
    step2201 = step2201 + step220105
    ! STARFIRE percentage for spares
    spares = spares + 6.824D-2 * step220105
  
    ! 22.01.06 Reactor Vacuum System
    ! Original STARFIRE value, scaling with fusion island volume
    step220106 = step_ref(28) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2201 = step2201 + step220106
    ! STARFIRE percentage for spares
    spares = spares + 1.893D-1 * step220106
  
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
  end subroutine step_a2201

  subroutine step_a220101(step_ucblss, step_ucblbreed, step_ucblbe, ucblli, &
    step_ucblvd, ucblli2o, ucbllipb, ifueltyp, step_ucfws, step_ucfwps, &
    step_ucfwa, blktmodel, whtblli, blkttype, wtblli2o, whtblbreed, whtblvd, &
    whtblbe, whtblss, wtbllipb, fw_armour_mass, fwmass, fwarea, ipowerflow, &
    step220101, step22010101, step22010102, step2201010201, step2201010202, &
    step2201010203, fwallcst, blkcst)
    !! Account 22.01.01 : Blanket and First Wall 
    !! author: A J Pearce, CCFE, Culham Science Centre
    !! This routine evaluates the Account 22.01.01 (BB+FW) costs.
    !! If ifueltyp = 0, the blanket cost is treated as capital cost
    !! If ifueltyp = 1, the blanket cost is treated as a fuel cost,
    !! rather than as a capital cost.
    !! If ifueltyp = 2, the initial blanket is included as a capital cost
    !! and the replacement blanket costs are treated as a fuel cost.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    implicit none

    real(dp), intent(in) :: step_ucblss, step_ucblbreed, step_ucblbe, ucblli, &
      step_ucblvd, ucblli2o, ucbllipb, ifueltyp, step_ucfws, step_ucfwps, &
      step_ucfwa, blktmodel, whtblli, blkttype, wtblli2o, whtblbreed, whtblvd, &
      whtblbe, whtblss, wtbllipb, fw_armour_mass, fwmass, fwarea, ipowerflow 
    real(dp), intent(out) :: step22010101, step22010102, step2201010201, &
      step2201010202, step2201010203, step220101, fwallcst, blkcst

    !  Local variables
    real(dp) :: step2201010204, step2201010205, step2201010206, step2201010207

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

  subroutine step_a220102(rsldi, shldith, shldtth, vgap, scrapli, scraplo, &
    fwith, fwoth, blnktth, d_vv_in, i_shield_mat, denw, denwc, divfix, &
    step_ucshw, step_ucshwc, rminor, kappa, idivrt, pi, step220102)
    !! 22.01.02
    !! Returns cost of inboard shield
    !! Note: outboard shield costs currently set to zero
    implicit none

    ! Result
    real(dp), intent(in) :: rsldi, shldith, shldtth, vgap, scrapli, scraplo, &
    fwith, fwoth, blnktth, d_vv_in, i_shield_mat, denw, denwc, divfix, &
    step_ucshw, step_ucshwc, rminor, kappa, idivrt, pi
    real(dp), intent(out) :: step220102
    !! Cost of shield in M$

    ! Local variables
    real(dp):: inb_sh_v, r1, hbot, htop, hshld, &
      inb_sh_v_mtl, inb_sh_m, sh_mtl_d, sh_mtl_c, shldith_corr    

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

    ! Corrected shield thickness: allows for 300mm vacuum vessel
    ! Justification: requirement from K. Taylor, neutronics
    ! # TODO: replace this correction when valid (VV + shield) is used
    shldith_corr = (d_vv_in + shldith) - 0.3D0
    ! Volume of inboard cylindrical shell (m3)
    inb_sh_v = 2.0D0*(hshld+shldtth) * pi*(r1**2 - (r1-shldith_corr)**2)

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

    ! Note: outboard shield costs currently set to zero

  end subroutine step_a220102

  subroutine step_a22010301(step_ref, ifueltyp, step_uc_cryo_al, &
      step_mc_cryo_al_per, uccpcl1, uccpclb, &
      i_tf_sup, whtconal, n_tf, whttflgs, whtcp, itart, step22010301, cpstcst)    
    !! 22.01.03.01 TF Coils
    implicit none
    
    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: ifueltyp, step_uc_cryo_al, &
      step_mc_cryo_al_per, uccpcl1, uccpclb, &
      i_tf_sup, whtconal, n_tf, whttflgs, whtcp, itart
    real(dp), intent(out) :: step22010301, cpstcst
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
  end subroutine step_a22010301

  subroutine step_a22010302(iohcl, twopi, dcopper, step_uccase, step_uccu, &
    step_cconshpf, step_ucfnc, step_cconfix, step_ucsc, step_ucwindpf, &
		rjconpf, ipfres, vfohc, nohc, turns, isumatpf, whtpfs, ric, rpf, isumatoh, &
    fcupfsu, fcuohsu, vf, awpoh, fncmass, dcond, step22010302)
    !! Account 22.01.03.02 PF Coils : PF magnet assemblies
    !! author: A J Pearce, CCFE, Culham Science Centre
    !! This routine evaluates the Account 22.01.03.02 (PF magnet) costs.
    !! Conductor costs previously used an algorithm devised by R. Hancox,
    !! January 1994, under contract to Culham, which took into
    !! account the fact that the superconductor/copper ratio in
    !! the conductor is proportional to the maximum field that
    !! each coil will experience. Now, the input copper fractions
    !! are used instead.
    !! Maximum values for current, current density and field
    !! are used. 
    implicit none

    ! Arguments
    real(dp), intent(in) :: iohcl, twopi, dcopper, step_uccase, step_uccu, &
      step_cconshpf, step_ucfnc, step_cconfix, step_ucwindpf, &
      ipfres, vfohc, whtpfs, fcupfsu, fcuohsu, awpoh, fncmass
    integer, intent(in) :: nohc, isumatoh, isumatpf
    real(dp), dimension(:), intent(in) :: dcond, ric, rpf, rjconpf, step_ucsc, &
      turns, vf
    real(dp), intent(out) :: step22010302
     
    !  Local variables
    real(dp) :: costpfcu,costpfsc,costpfsh,costwire,cpfconpm, &
         pfwndl, step2201030201, step2201030202, step2201030203, step2201030204
    integer :: i,npf

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

  end subroutine step_a22010302

  subroutine step_a220104(step_ref, fcdfuel, ucich, uclh, ifueltyp, &
    iefrf, iefrffix, echpwr, pnbitot, plhybd, step220104, cdcost)
    !! 22.01.04 Auxiliary Heating and Current Drive
    !! Returns cost of auxiliary HCD
    !! HCD cost = cost per injected Watt of power * injected Watts
    implicit none

    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: fcdfuel, ucich, uclh, ifueltyp, iefrf, &
      iefrffix, echpwr, pnbitot, plhybd
    real(dp), intent(out) :: step220104, cdcost
    !! Cost of HCD in M$

    ! Cost per Watt depends on technology/hardware used;
    ! inflation adjustment applied as appropriate to source for costs
    ! (tech adjusted from 1990 $ is costed as per Cost Model 0)

    ! NBI cost per injected Watt (adjusted from 2020 $):
    step220104 = step220104 + &
               ( pnbitot * step_ref(69) * (229.0D0/258.84D0) )

    ! EC or EBW cost per injected Watt (adjusted from 2020 $):
    step220104 = step220104 + & 
               ( echpwr * step_ref(70) * (229.0D0/258.84D0) )

    if ( (iefrf == 2) .or. (iefrffix == 2) ) then
      ! Ion Cyclotron current drive (adjusted from 1990 $):
      step220104 = step220104 + &
                ( 1.0D-6 * ucich * (1.0D6*plhybd) * (229.0D0/76.7D0) )
    end if

    if ( (iefrf == 1) .or. (iefrffix == 1) .or. &
              (iefrf == 4) .or. (iefrffix == 4) .or. &
              (iefrf == 6) .or. (iefrffix == 6) ) then
      ! Lower Hybrid system (adjusted from 1990 $):
      step220104 = step220104 + &
                ( 1.0D-6 * uclh * (1.0D6*plhybd) * (229.0D0/76.7D0) )    
    end if

    if ( ifueltyp == 1 ) then
      ! fraction `fcdfuel` of HCD cost treated as fuel cost
      step220104 = (1.0D0-fcdfuel) * step220104 
      cdcost = step220104
    end if
  end subroutine step_a220104

  subroutine step_a2202(pgrossmw, step2202)
    !! Account 22.02 : Heat Transfer System
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! This routine evaluates the Account 22.02 (Heat Transfer System)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    ! pgrossmw is gross electric power of the plant in MW
    implicit none
  
    ! Arguments
    real(dp), intent(in) :: pgrossmw
    real(dp), intent(out) :: step2202
  
    ! 22.02 Heat Transfer System
    ! #TODO Needs reference for values
    step2202 = 9.2238D4 * pgrossmw * 1.0D-6
    ! Converted to M$
  end subroutine step_a2202

  subroutine step_a2203(step_ref, vfi, vfi_star, step220301, step220302, &
    step220303, step220304, step2203)
    !! Account 22.03 : Cryogenic Cooling System
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 22.03 (Cryogenic Cooling
    !! System) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none
  
    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: vfi, vfi_star
    real(dp), intent(out) :: step220301, step220302, step220303, step220304, &
      step2203
  
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
  end subroutine step_a2203

  subroutine step_a2204(step_ref, pth, ptherm_star, step2204, step220401, &
    step220402, step220403)
    !! Account 22.04 : Waste Treatment and Disposal
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! This routine evaluates the Account 22.04 (Waste Treatment
    !! and Disposal) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none
  
    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: pth, ptherm_star
    real(dp), intent(out) :: step2204, step220401, step220402, step220403
  
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
  end subroutine step_a2204

  subroutine step_a2205(step_ref, pth, ptherm_star, step2205, spares)
    !! Account 22.05 : Fuel Handling and Storage
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! This routine evaluates the Account 22.05 (Fuel Handling
    !! and Storage) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none
  
    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: pth, ptherm_star
    real(dp), intent(out) :: step2205, spares
  
    ! Initialise as zero
    step2205 = 0.0D0
    spares = 0.0D0
     
    ! 22.05 Fuel Handling and Storage
    ! Original STARFIRE value, scaling with thermal power
    step2205 = step_ref(41) * (pth / ptherm_star)**0.6D0 

    ! STARFIRE percentage for spares
    spares = spares + 5.026D-2 * step2205
  end subroutine step_a2205

  subroutine step_a2206(step_ref, pth, ptherm_star, step2206, spares, &
    step220601, step220602, step220603, step220604, step220605, step220606, &
    step220607, step220608)
    !! Account 22.06 : Other Reactor Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! This routine evaluates the Account 22.06 (Other Reactor
    !! Plant Equipment) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none
  
    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: pth, ptherm_star
    real(dp), intent(out) :: step2206, spares, step220601, step220602, &
      step220603, step220604, step220605, step220606, step220607, step220608
  
    ! Initialise as zero
    step2206 = 0.0D0
    spares = 0.0D0
     
    ! 22.06.01 Maintenance Equipment
    ! Original STARFIRE value, scaling with fusion island volume
    ! Depreciated by the remote handling scaling in cost account 27. 
    step220601 = 0.0 ! step_ref(42) * (vfi / vfi_star)**(2.0D0/3.0D0)
    step2206 = step2206 + step220601
    ! STARFIRE percentage for spares
    spares = spares + 4.308D-1 * step220601
  
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
    spares = spares + 8.3D-1 * (pth / ptherm_star)**0.6D0
  
    ! 22.06.08 Standby Cooling System
    ! Original STARFIRE value, scaling with thermal power
    step220608 = step_ref(49) * (pth / ptherm_star)**0.6D0
    step2206 = step2206 + step220608
  end subroutine step_a2206
 
  subroutine step_a2207(step_ref, pth, ptherm_star, step2207)
    !! Account 22.07 : Instrumentation and Control
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! This routine evaluates the Account 22.07 (Instrumentation
    !! and Control) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none
  
    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: pth, ptherm_star
    real(dp), intent(out) :: step2207

    ! Initialise as zero
    step2207 = 0.0D0

    ! 22.07 Instrumentation and Control
    ! Original STARFIRE value, scaling with thermal power
    step2207 = step_ref(50) * (pth / ptherm_star)**0.6D0
  end subroutine step_a2207

  subroutine step_a23(step_ref, step_con, pgrossmw, step23a, step2303, &
    step2398, step2399, step23)
    !! Account 23 : Turbine Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 23 (Turbine Plant Equipment)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none

    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: step_con, pgrossmw
    real(dp), intent(out) :: step23a, step2303, step2398, step2399, step23

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
  end subroutine step_a23

  subroutine step_a24(step_ref, step_con, pgrossmw, step2401, step2402, &
    step2403, step2404, step2405, step2406, step2407, step2498, step2499, &
    step24)
    !! Account 24 : Electric Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! This routine evaluates the Account 24 (Electric Plant 
    !! Equipment) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none

    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: step_con, pgrossmw
    real(dp), intent(out) :: step2401, step2402, step2403, step2404, step2405, &
    step2406, step2407, step2498, step2499, step24

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
  end subroutine step_a24

  subroutine step_a25(step_ref, step_con, pgrossmw, wgt, step2501, step2502, &
    step2503, step2504, step2598, step2599, step25)
    !! Account 25 : Miscellaneous Plant Equipment
    !! author: S I Muldrew, CCFE, Culham Science Centre
    !! None
    !! This routine evaluates the Account 25 (Miscellaneous Plant 
    !! Equipment) costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    ! #TODO Need to add reference for cost calculations
    implicit none
  
    ! Arguments
    real(dp), dimension(:), intent(in) :: step_ref
    real(dp), intent(in) :: step_con, pgrossmw, wgt
    real(dp), intent(out) :: step2501, step2502, step2503, step2504, step2598, &
      step2599, step25

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
  end subroutine step_a25

  subroutine step_a27(cdirt, step_rh_costfrac, step2701, step27)
    !! Account 27 : Remote Handling
    !! author: A J Pearce, CCFE, Culham Science Centre
    !! This routine evaluates the Account 27 (Remote Handling)
    !! costs.
    !! STARFIRE - A Commercial Tokamak Fusion Power Plant Study (1980)
    implicit none

    ! Arguments
    real(dp), intent(in) :: cdirt, step_rh_costfrac
    real(dp), intent(out) :: step2701, step27

    ! Initialise as zero
    step27 = 0.0D0
   
    ! 27.01 Remote Handling 
    ! From report by T. Hender CD-STEP-01030, scales with direct capital costs
    step2701 = step_rh_costfrac * cdirt 
    
    step27 = step2701
  end subroutine step_a27

  subroutine step_indirect_costs(cdirt, step91_per, step92_per, step93_per, &
    step91, step92, step93)
    !! Accounts 91-93: Indirect costs
    !! Calculate the indirect costs and print
    implicit none
    
    ! Arguments
    real(dp), intent(in) :: cdirt, step91_per, step92_per, step93_per
    real(dp), intent(out) :: step91, step92, step93

    ! Account 91 : Construction Facilities, Equipment and Services (default 30%)
    step91 = step91_per * cdirt

    ! Account 92 : Engineering and Costruction Management Services (default 32.5%)
    step92 = step92_per * cdirt

    ! Account 93 : Other Costs (default 5%)
    step93 = step93_per * cdirt
  end subroutine step_indirect_costs

  subroutine coelc_step(discount_rate, tlife, ucfuel, uche3, cdcost, &
    divcst, fcdfuel, ifueltyp, fwallcst, fcr0, fcap0cp, &
    fcap0, dtlife, divlife, dintrt, decomf, cpstcst, cplife, concost, &
    cfactr, cdrlife, step_ref, step_currency, &
    step_ucoam, step_ucwst, bktlife, pnetelmw, fhe3, itart, wtgpd, tburn, &
    tcycle, n_day_year, &
    anncap,anncdr,anncp,anndecom,anndiv,annfuel, &
    annfuelt, annfwbl, annoam, anntot, annwst, coecdr,  &
    coecp, coedecom, coediv, coefuel, coefwbl, coewst, &
    crffwbl, feffwbl, fwbllife, &
    moneyint, capcost, coecap, coeoam, coefuelt, coe, title)
    !! Routine to calculate the cost of electricity for a fusion power plant
    !! author: S I Muldrew,  CCFE, Culham Science Centre
    !! This routine performs the calculation of the cost of electricity
    !! for a fusion power plant.
    !! Annual costs are in megadollars/year, electricity costs are in
    !! millidollars/kWh, while other costs are in megadollars.
    !! All values are based on 1980 dollars.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    implicit none

    ! Arguments
    real(dp), intent(in) :: discount_rate, tlife, ucfuel, uche3, cdcost, &
      divcst, fcdfuel, ifueltyp, fwallcst, fcr0, fcap0cp, &
      fcap0, dtlife, divlife, dintrt, decomf, cpstcst, cplife, concost, &
      cfactr, cdrlife, step_ref, &
      step_ucoam, step_ucwst, bktlife, pnetelmw, fhe3, itart, wtgpd, tburn, &
      tcycle, n_day_year
    character(len=50), intent(in) :: step_currency
    real(dp), intent(out) :: anncap,anncdr,anncp,anndecom,anndiv,annfuel, &
      annfuelt,annfwbl,annoam,anntot,annwst,coecdr, &
      coecp,coedecom,coediv,coefuel,coefwbl,coewst, &
      crffwbl,feffwbl,fwbllife,moneyint, &
      capcost, coecap, coeoam, coefuelt, coe
    character(len=80), intent(out) :: title
         
    ! Variables
    real(dp) :: crfcdr, crfcp, crfdiv, fefcdr, fefcp, fefdiv, kwhpy

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
    annfwbl = fwblkcost * fcap0cp * crffwbl

    ! Cost of electricity due to first wall/blanket replacements
    coefwbl = 1.0D9 * annfwbl / kwhpy

    ! Costs due to divertor renewal
    ! =============================

    ! Compound interest factor
    fefdiv = (1.0D0 + discount_rate)**divlife

    ! Capital recovery factor
    crfdiv = (fefdiv*discount_rate) / (fefdiv-1.0D0)

    ! Annual cost of replacements
    anndiv = divcst * fcap0cp * crfdiv

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
      anncp = cpstcst * fcap0cp * crfcp

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
      anncdr = cdcost * fcdfuel/(1.0D0-fcdfuel) * fcap0cp * crfcdr
    end if

    ! Cost of electricity due to current drive system replacements
    coecdr = 1.0D9 * anncdr / kwhpy

    ! Costs due to operation and maintenance
    ! ======================================

    ! Annual cost of operation and maintenance
    annoam = step_ucoam * sqrt(pnetelmw/1200.0D0)

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
  annwst = step_ucwst * sqrt(pnetelmw/1200.0D0)

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
  end subroutine coelc_step
end module costs_step_module
