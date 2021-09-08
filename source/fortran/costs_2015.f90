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
  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  ! Precision variable
  integer, parameter :: double = 8

  ! Output variables
  integer :: ip, ofile

  ! Costs structure for scaling laws (scl)
  character(len=80), save, dimension(100) :: s_label = 'not used' ! Description, appears in OUT.DAT
  real(kind=double), save, dimension(100) :: s_kref = 0.0D0 ! Reference value of scaling parameter
  real(kind=double), save, dimension(100) :: s_k = 0.0D0 ! Actual value of scaling parameter K
  real(kind=double), save, dimension(100) :: s_cref = 0.0D0 ! Reference cost $
  real(kind=double), save, dimension(100) :: s_cost = 0.0D0 ! Actual cost $
  real(kind=double), save, dimension(100) :: s_cost_factor = 0.0D0 ! Multiplier f


  ! Scaling law array (unused entries will be zeroes)
  real(kind=double) :: total_costs, mean_electric_output, annual_electric_output, &
                       maintenance

  ! Private module variables
  private :: ip, ofile, double, total_costs, s_label, &
            s_kref, s_k, s_cref, s_cost, s_cost_factor

  ! Public variables/subroutines
  public :: costs_2015

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

  subroutine costs_2015(outfile,iprint)
    !! Cost accounting for a fusion power plant
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine performs the cost accounting for a fusion power plant.
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use heat_transport_variables, only: pnetelmw
    use cost_variables, only: concost, cpfact, maintenance_fwbs, maintenance_gen, &
      amortization, output_costs, coe

    implicit none

    ! Arguments
    integer, intent(in) :: iprint, outfile
    integer :: i

    ! Assign module private variables to iprint and outfile
    ip = iprint
    ofile = outfile

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate building costs
    call calc_building_costs

    ! Calculate land costs
    call calc_land_costs

    ! Calculate tf coil costs
    call calc_tf_coil_costs

    ! Calculate fwbs costs
    call calc_fwbs_costs

    ! Calculate remote handling costs
    call calc_remote_handling_costs

    ! Calculate N plant and vacuum vessel costs
    call calc_n_plant_and_vv_costs

    ! Calculate energy conversion system costs
    call calc_energy_conversion_system

    ! Calculate remaining subsystems costs
    call calc_remaining_subsystems

    ! Calculate total capital cost
    total_costs = s_cost(9) + s_cost(13) + s_cost(21) + &
         s_cost(27) + s_cost(31) + s_cost(34) + &
         s_cost(35) + s_cost(61)

    ! Save as concost, the variable used as a Figure of Merit (M$)
    concost = total_costs/1.0D6

    ! Electrical output (given availability) for a whole year
    mean_electric_output = pnetelmw * cpfact
    annual_electric_output = mean_electric_output * 24.0D0*265.25D0

    ! Annual maintenance cost.
    maintenance =  (s_cost(27) + s_cost(38) ) * maintenance_fwbs + &
                   (s_cost(9) + s_cost(31) + s_cost(34) + s_cost(35) + &
                   s_cost(41) + s_cost(43) + s_cost(45) + s_cost(47) + &
                   s_cost(48) + s_cost(49) + s_cost(50) + s_cost(51) + &
                   s_cost(52) + s_cost(53) + s_cost(54) + s_cost(58))* &
                   maintenance_gen

    ! Levelized cost of electricity (LCOE) ($/MWh)
    if(annual_electric_output.gt.0.00001) then
        coe = (1.0D0/annual_electric_output)*(total_costs/amortization + maintenance)
    endif

    ! Switch on output if there is a NaN error
    if ((abs(concost) > 9.99D99).or.(concost /= concost)) then
        call write_costs_to_output
        do i=1,100
          write(*,*) s_label(i), s_kref(i), s_k(i), s_cref(i), s_cost(i), s_cost_factor(i)
        end do
        return
    end if

    ! Output costs !
    ! !!!!!!!!!!!!!!!

    if ((ip == 0).or.(output_costs == 0)) return

    call write_costs_to_output

  end subroutine costs_2015

  subroutine calc_building_costs

    !! Function to calculate the cost of all buildings.
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine calculates the building costs for a fusion power plant
    !! based on the costings in the PROCESS costs Paper.
    !! Buildings have a different scaling law, with fixed cost per unit volume.
    !! Cref is therefore now f.Viter.unit_cost
    !! The costs for individual buildings must not be output,
    !! as the same mean cost per unit volume has been used both for light
    !! and for shielded buildings
    !! The exponent =1
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use current_drive_variables, only: pwpnb
    use pfcoil_variables, only: pfrmax
    use heat_transport_variables, only: pthermmw, psechtmw, helpow
    use tfcoil_variables, only: ritfc, n_tf, estotftgj
    use fwbs_variables, only: rdewex, zdewex
    use cost_variables, only: cost_factor_buildings, light_build_cost_per_vol, &
      tok_build_cost_per_vol

    implicit none

    ! Local Variables
    integer :: i, j

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Set cost factor for buildings
    do i=1, 9
       s_cost_factor(i) = cost_factor_buildings
    end do

    ! Power plant admin buildings cost ($)
    s_label(1) = "Admin Buildings"
    s_cref(1) = 129000.0D0 * light_build_cost_per_vol
    s_cost(1) = s_cost_factor(1) * s_cref(1)

    ! Tokamak complex excluding hot cell cost ($)
    s_label(2) = "Tokamak Complex (excluding hot cell)"
    s_cref(2) = 1100000.0D0 * tok_build_cost_per_vol
    ! ITER cryostat volume (m^3)
    s_k(2) = (pi*rdewex**2) * 2.0D0 * zdewex
    s_kref(2) = 18712.0D0
    s_cost(2) = s_cost_factor(2) * s_cref(2) * (s_k(2) / s_kref(2))

    ! Neutral beam buildings cost ($)
    s_label(3)="Neutral beam buildings"
    s_cref(3) = 28000.0D0 * light_build_cost_per_vol
    ! Scale with neutral beam wall plug power (MW)
    s_k(3) = pwpnb
    s_kref(3) = 120.0D0
    s_cost(3) = s_cost_factor(3) * s_cref(3) * (s_k(3) / s_kref(3))

    ! Cryoplant buildings cost ($)
    s_label(4) = "Cryoplant buildings"
    s_cref(4) = 130000.0D0 * light_build_cost_per_vol
    ! Scale with the total heat load on the cryoplant at ~4.5K (kW)
    s_k(4) = helpow/1.0D3
    s_kref(4) = 61.0D0
    s_cost(4) = s_cost_factor(4) * s_cref(4) * (s_k(4) / s_kref(4))

    ! PF Coil winding building cost ($)
    s_label(5) = "PF Coil winding building"
    s_cref(5) = 190000.0D0 * light_build_cost_per_vol
    ! Scale with the radius of the largest PF coil squared (m^2)
    s_k(5) = pfrmax**2
    s_kref(5) = 12.4D0**2
    s_cost(5) = s_cost_factor(5) * s_cref(5) * (s_k(5) / s_kref(5))

    ! Magnet power supplies and related buildings cost ($)
    s_label(6) = "Magnet power supplies and related buildings"
    s_cref(6) = 110000.0D0 * light_build_cost_per_vol
    ! Scale with TF current per coil (MA)
    s_k(6) = (ritfc/n_tf)/1.0D6
    s_kref(6) = 9.1D0
    s_cost(6) = s_cost_factor(6) * s_cref(6) * (s_k(6) / s_kref(6))

    ! Magnet discharge buildings cost ($)
    s_label(7) = "Magnet discharge buildings"
    s_cref(7) = 35000.0D0 * light_build_cost_per_vol
    ! Scale with total stored energy in TF coils (GJ)
    s_k(7) = estotftgj
    s_kref(7) = 41.0D0
    s_cost(7)  = s_cost_factor(7) * s_cref(7) * (s_k(7) / s_kref(7))

    ! Heat removal system buildings cost ($)
    s_label(8) = "Heat removal system buildings"
    ! ITER volume of cooling water buildings (m^3)
    s_cref(8) = 51000.0D0 * light_build_cost_per_vol
    ! Scale with total thermal power removed from the core (MW)
    s_k(8) = pthermmw + psechtmw
    s_kref(8) = 880.0D0
    s_cost(8) = s_cost_factor(8) * s_cref(8) * (s_k(8) / s_kref(8))

    ! Total cost of buildings ($)
    s_label(9) = "Total cost of buildings"
    s_cost(9) = 0.0D0
    do j=1, 8
       s_cost(9) = s_cost(9) + s_cost(j)
    end do

  end subroutine calc_building_costs

  subroutine calc_land_costs

    !! Function to calculate the cost of land for the power plant
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! Land also uses a unit cost, but area is scaled.
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: dr_tf_inner_bore, dh_tf_inner_bore, tfcth
    use fwbs_variables, only: rdewex
    use cost_variables, only: cost_factor_land, costexp

    implicit none

    ! Local Variables
    integer :: i, j
    real(kind=double) :: ITER_total_land_area
    real(kind=double) :: ITER_key_buildings_land_area
    real(kind=double) :: ITER_buffer_land_area

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Set cost factor for land
    do i=10, 13
       s_cost_factor(i) = cost_factor_land
    end do

    ! Land purchasing cost ($)
    s_label(10) = "Land purchasing"
    ! ITER Land area (hectares)
    ITER_total_land_area = 180.0D0
    ! ITER Land area for key buildings (hectares)
    ITER_key_buildings_land_area = 42.0D0
    ! ITER buffer land (hectares)
    ITER_buffer_land_area = ITER_total_land_area - ITER_key_buildings_land_area

    ! Scale with area of cryostat (m)
    s_k(10) = pi * rdewex**2
    s_kref(10) = 638.0D0
    ! Cost of land per hectare (2014 $ / ha)
    s_cref(10) = 318000.0D0
    ! Cost of power plant land (2014 $)
    s_cost(10) = s_cost_factor(10) * s_cref(10) * &
    (ITER_key_buildings_land_area *(s_k(10) / s_kref(10))**costexp + ITER_buffer_land_area)

    ! Land improvement costs ($)
    s_label(11) = "Land improvement"
    ! Cost of clearing ITER land
    s_cref(11) = 214.0D6
    ! Scale with area of cryostat (m)
    s_k(11) = pi * rdewex**2
    s_kref(11) = 638.0D0
    s_cost(11) = s_cost_factor(11) * (s_k(11) / s_kref(11) )**costexp * s_cref(11)

    ! Road improvements cost ($)
    s_label(12) = "Road improvements"
    ! Cost of ITER road improvements
    s_cref(12) = 150.0D6
    ! Scale with TF coil longest dimension
    s_k(12) = max(dh_tf_inner_bore, dr_tf_inner_bore) + 2.0D0*tfcth
    s_kref(12) = 14.0D0
    s_cost(12) = s_cost_factor(12) * s_cref(12) * (s_k(12) / s_kref(12))**costexp

    ! Total land costs ($)
    s_label(13) = "Total land costs"
    s_cost(13) = 0.0D0
    do j=10, 12
       s_cost(13) = s_cost(13) + s_cost(j)
    end do

  end subroutine calc_land_costs

  subroutine calc_tf_coil_costs

    !! Function to calculate the cost of the TF coils for the power plant
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine calculates the cost of the TF coils for a fusion power
    !! plant based on the costings in the PROCESS costs Paper.
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use tfcoil_variables, only: n_tf, tfleng, n_tf_turn, whtconcu, whtconsc
    use cost_variables, only: cost_factor_tf_coils, costexp

    implicit none

    ! Local Variables
    integer :: i, j

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Set cost factor for tf coils
    do i=14, 20
       s_cost_factor(i) = cost_factor_tf_coils
    end do

    ! TF coil insertion and welding costs ($)
    s_label(14) = "TF Coil insertion and welding"
    ! ITER coil insertion and welding cost (2014 $)
    s_cref(14) = 258.0D6
    ! Scale with total TF coil length (m)
    s_k(14) = n_tf * tfleng
    s_kref(14) = 18.0D0*34.1D0
    s_cost(14) = s_cost_factor(14) * s_cref(14) * (s_k(14) / s_kref(14))**costexp    

    ! TF coil winding costs ($)
    s_label(16) = "TF coil winding"
    ! ITER winding cost (2014 $)
    s_cref(16) = 414.0D6
    ! Scale with the total turn length (m)
    s_k(16) = n_tf * tfleng * n_tf_turn
    s_kref(16) = 82249.0D0
    s_cost(16) = s_cost_factor(16) * s_cref(16) * (s_k(16) / s_kref(16))**costexp

    ! Copper stand cost for TF coil ($)
    s_label(17) = "Copper strand for TF coil"
    ! ITER Chromium plated Cu strand for TF SC cost (2014 $)
    s_cref(17) = 21.0D6
    ! Scale with total copper mass (kg)
    s_k(17) = whtconcu * n_tf
    s_kref(17) = 244.0D3
    s_cost(17) = s_cost_factor(17) * s_cref(17) * (s_k(17) / s_kref(17))**costexp

    ! superconductor strand cost ($)
    s_label(18) = "Strands with Nb3Sn superconductor and copper stabiliser"
    ! ITER Nb3Sn SC strands cost (2014 $)
    s_cref(18) = 526.0D6
    ! Scale with the total mass of Nb3Sn (kg)
    s_k(18) = whtconsc * n_tf
    s_kref(18) = 210.0D3
    s_cost(18) = s_cost_factor(18) * s_cref(18) * (s_k(18) / s_kref(18))**costexp

    ! Superconductor testing cost ($)
    s_label(19) = "Testing of superconducting strands"
    ! ITER Nb3Sn strand test costs (2014 $)
    s_cref(19) = 4.0D6
    s_cost(19) = s_cost_factor(19) * s_cref(19)

    ! Superconductor cabling and jacketing cost ($)
    s_label(20) = "Cabling and jacketing"
    ! ITER cabling and jacketing costs (2014 $)
    s_cref(20) = 81.0D6
    ! Scale with total turn length.
    s_k(20) = n_tf * tfleng * n_tf_turn
    s_kref(20) = 82249.0D0
    s_cost(20) = s_cost_factor(20) * s_cref(20) * (s_k(20) / s_kref(20))**costexp

    ! Total TF coil costs ($)
    s_label(21) = "Total TF coil costs"
    s_cost(21) = 0.0D0
    do j=14, 20
       s_cost(21) = s_cost(21) + s_cost(j)
    end do

  end subroutine calc_tf_coil_costs

  subroutine calc_fwbs_costs

    !! Function to calculate the cost of the first wall, blanket and shield
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine calculates the cost of the first wall, blanket and shield
    !! coils for a fusion power plant based on the costings in the PROCESS costs paper.
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwarea
    use global_variables, only: run_tests
    use process_output, only: ocmmnt
    use fwbs_variables, only: li6enrich, whtblli4sio4, whtbltibe12, fw_armour_thickness, &
      whtblss, whtshld
    use cost_variables, only: cost_factor_fwbs, costexp, costexp_pebbles

    implicit none

    ! Local Variables
    integer :: i, j

    ! Enrichment variables
    real(dp) :: product_li6
    real(dp) :: feed_li6
    real(dp) :: tail_li6
    real(dp) :: feed_to_product_mass_ratio
    real(dp) :: tail_to_product_mass_ratio
    real(dp) :: p_v, f_v, t_v
    real(dp) :: swu, total_swu
    real(dp) :: mass_li

    ! First wall W coating variables
    real(dp) :: W_density

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Set cost factor for fwbs
    do i=22, 27
       s_cost_factor(i) = cost_factor_fwbs
    end do

    ! Enrichment
    ! Costs based on the number of separative work units (SWU) required
    !
    ! SWU = P V(x_p) + T V(x_t) - F V(x_f)
    !
    ! where V(x) is the value function
    !
    ! V(x) = (1 - 2x)ln((1-x)/x)

    ! Percentage of lithium 6 in the feed (natural abundance)
    feed_li6 = 0.0742D0
    ! Percentage of lithium 6 in the tail (waste) (75% natural abundance)
    tail_li6 = feed_li6 * 0.75D0

    ! Built-in test
    if ((ip == 1).and.(run_tests == 1)) then
      product_li6 = 0.3
      feed_to_product_mass_ratio = (product_li6 - tail_li6) / (feed_li6 - tail_li6)
      tail_to_product_mass_ratio = (product_li6 - feed_li6) / (feed_li6 - tail_li6)
      call value_function(product_li6, p_v)
      call value_function(tail_li6, t_v)
      call value_function(feed_li6, f_v)
      swu = p_v + tail_to_product_mass_ratio * t_v - feed_to_product_mass_ratio * f_v
      if (abs(swu-2.66D0) < 2.0D-2) then
        call ocmmnt(ofile, "SWU for default 30% enrichment.  Should = 2.66. CORRECT")
      else
        call ocmmnt(ofile, "SWU for default 30% enrichment.  Should = 2.66. ERROR")
      end if
      ! Reference cost
      s_label(22) = "Lithium enrichment"
      s_cref(22) = 0.1D6
      s_k(22) =  64.7D0
      s_kref(22) = 64.7D0
      s_cost(22) = s_cost_factor(22) * s_cref(22) * (s_k(22) / s_kref(22))**costexp
      if (abs(s_cost(22)-0.1D6)/0.1D6 < 1.0D-3) then
        call ocmmnt(ofile, "Reference cost for enrichment CORRECT")
      else
        call ocmmnt(ofile, "Reference cost for enrichment ERROR")
      end if
    end if

    ! Lithium 6 enrichment cost ($)
    s_label(22) = "Lithium enrichment"

    ! Zero cost for natural enrichment
    if (li6enrich <= 7.42D0) then
      s_cost(22) = 0.0D0
    else
      ! Percentage of lithium 6 in the product
      product_li6 = min(li6enrich,99.99D0) / 100.0D0
      ! SWU will be calculated for a unit mass of product (P=1)

      ! Feed to product mass ratio
      feed_to_product_mass_ratio = (product_li6 - tail_li6) / (feed_li6 - tail_li6)

      ! Tail to product mass ratio
      tail_to_product_mass_ratio = (product_li6 - feed_li6) / (feed_li6 - tail_li6)

      ! Calculate value functions
      call value_function(product_li6, p_v)
      call value_function(tail_li6, t_v)
      call value_function(feed_li6, f_v)

      ! Calculate separative work units per kg
      swu = p_v + tail_to_product_mass_ratio * t_v - feed_to_product_mass_ratio * f_v

      ! Mass of lithium (kg).  Lithium orthosilicate is 22% lithium by mass.
      mass_li = whtblli4sio4 * 0.22

      ! Total swu for lithium in blanket
      total_swu = swu * mass_li

      ! Reference cost for lithium enrichment (2014 $)
      s_cref(22) = 0.1D6
      ! Reference case of lithium SWU
      s_k(22) =  total_swu
      s_kref(22) = 64.7D0
      s_cost(22) = s_cost_factor(22) * s_cref(22) * (s_k(22) / s_kref(22))**costexp
    end if

    s_label(23) = "Lithium orthosilicate pebble manufacturing"
    ! Reference cost of lithium pebble manufacture (2014 $)
    s_cref(23) = 6.5D4
    ! Scale with mass of pebbles (kg)
    s_k(23) = whtblli4sio4
    s_kref(23) = 10.0D0
    s_cost(23) = s_cost_factor(23) * s_cref(23) * (s_k(23) / s_kref(23))**costexp_pebbles

    s_label(24) = "Titanium beryllide pebble manufacturing"
    !  Reference cost of titanium beryllide pebble manufacture (2014 $)
    s_cref(24) = 450.0D6
    !  Scale with mass of titanium beryllide pebbles (kg)
    s_k(24) = whtbltibe12
    s_kref(24) = 1.0D5
    s_cost(24) = s_cost_factor(24) * s_cref(24) * (s_k(24) / s_kref(24))**costexp_pebbles

    s_label(25) = "First wall W coating manufacturing"
    !  Reference (PPCS A) first wall W coating cost (2014 $)
    s_cref(25) = 25.0D6
    !  W density (kg/m^3)
    W_density = 19250.0D0
    !  First wall W coating mass (kg)
    s_k(25) = fwarea * fw_armour_thickness * W_density
    s_kref(25) = 29000.0D0
    s_cost(25) = s_cost_factor(25) * s_cref(25) * (s_k(25) / s_kref(25))**costexp

    s_label(26) = "Blanket and shield materials and manufacturing"
    ! The cost of making the blanket was estimated for PPCS A.
    ! This cost includes only manufacturing – not R&D, transport, or assembly in the reactor.
    ! It includes the first wall, blanket and shield, but excludes the breeder and multiplier materials.
    s_cref(26) = 317.0D6
    !  Scale with steel mass in blanket + shield mass
    s_k(26) = whtblss + whtshld
    s_kref(26) = 4.07D6
    s_cost(26) = s_cost_factor(26) * s_cref(26) * (s_k(26) / s_kref(26))**costexp

    s_label(27) = "Total first wall and blanket cost"
    s_cost(27) = 0.0D0
    do j=22, 26
       s_cost(27) = s_cost(27) + s_cost(j)
    end do

  end subroutine calc_fwbs_costs

  subroutine calc_remote_handling_costs

    !! Function to calculate the cost of the remote handling facilities
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fwbs_variables, only: armour_fw_bl_mass
    use cost_variables, only: cost_factor_rh, costexp, num_rh_systems

    implicit none

    !  Local Variables
    integer :: i

    !  Divertor RH system variables
    !real(dp) :: ITER_num_div_rh_systems
    !real(dp) :: div_num_rh_systems_ratio

    !  First wall and blanket RH system variables
    !real(dp) :: ITER_num_blanket_rh_systems
    !real(dp) :: blanket_num_rh_systems_ratio

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set cost factor for remote handling
    do i=28, 31
       s_cost_factor(i) = cost_factor_rh
    end do

    ! K:\Power Plant Physics and Technology\Costs\Remote handling
    ! From Sam Ha.

    s_label(28) = "Moveable equipment"
    s_cref(28) = 1.0D6 * (139.0D0*num_rh_systems + 410.0D0)
    !  Scale with total mass of armour, first wall and blanket (kg)
    s_kref(28) = 4.35D6
    s_k(28) = armour_fw_bl_mass
    s_cost(28) = s_cost_factor(28) * s_cref(28) * (s_k(28) / s_kref(28))**costexp

    s_label(29) = "Active maintenance facility with fixed equipment"
    s_cref(29) = 1.0D6 * (95.0D0*num_rh_systems + 2562.0D0)
    !  Scale with total mass of armour, first wall and blanket (kg)
    s_kref(29) = 4.35D6
    s_k(29) = armour_fw_bl_mass
    s_cost(29) = s_cost_factor(29) * s_cref(29) * (s_k(29) / s_kref(29))**costexp

    ! s(30) is not in use

    s_label(31) = "Total remote handling costs"
    s_cost(31) = s_cost(28) + s_cost(29)

  end subroutine calc_remote_handling_costs

  subroutine calc_n_plant_and_vv_costs

    !! Function to calculate the cost of the nitrogen plant and vacuum vessel
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine calculates the cost of the nitrogen plant and vacuum vessel
    !! for a fusion power plant based on the costings in the PROCESS costs paper.
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: rsldo, d_vv_out
    use heat_transport_variables, only: helpow
    use cost_variables, only: cost_factor_vv, costexp

    implicit none

    !  Local Variables
    integer :: i, j

    !  Set cost factor for vacuum vessel and N plant
    do i=32, 34
       s_cost_factor(i) = cost_factor_vv
    end do

    !  Vacuum vessel
    s_label(32) = "Vacuum vessel"
    !  ITER reference vacuum vessel cost (2014 $)
    s_cref(32) = 537.0D6
    !  Scale with outermost midplane radius of vacuum vessel squared (m2)
    s_k(32) = (rsldo + d_vv_out)**2
    s_kref(32) = 94.09D0
    s_cost(32) = s_cost_factor(32) * s_cref(32) * (s_k(32) / s_kref(32))**costexp

    !  Nitrogen plant
    s_label(33) = "Liquid nitrogen plant"
    !  ITER reference cost (2014 $)
    s_cref(33) = 86.0D6
    !  Scale with 4.5K cryopower (W)
    s_k(33) = helpow
    s_kref(33) = 50.0D3
    s_cost(33) = s_cost_factor(33) * s_cref(33) * (s_k(33) / s_kref(33))**costexp

    s_label(34) = "Total liquid nitrogen plant and vacuum vessel"
    s_cost(34) = 0.0D0
    do j=32,33
       s_cost(34) = s_cost(34) + s_cost(j)
    end do

  end subroutine calc_n_plant_and_vv_costs

  subroutine calc_energy_conversion_system

    !! Function to calculate the cost of the energy conversion system
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine calculates the cost of the energy conversion system
    !! for a fusion power plant based on the costings in the PROCESS costs paper.
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use heat_transport_variables, only: pgrossmw
    use cost_variables, only: cost_factor_bop, costexp

    implicit none

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    s_label(35) = "Energy conversion system"
    !  Set cost factor for energy conversion system
    s_cost_factor(35) = cost_factor_bop
    !  Cost of reference energy conversion system (Rolls Royce)
    s_cref(35) = 511.0D6
    !  Scale with gross electric power (MWe)
    s_k(35) = pgrossmw
    s_kref(35) = 692.0D0
    s_cost(35) = s_cost_factor(35) * s_cref(35) * (s_k(35) / s_kref(35))**costexp

  end subroutine calc_energy_conversion_system

  subroutine calc_remaining_subsystems

    !! Function to calculate the cost of the remaining subsystems
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine calculates the cost of the remaining subsystems
    !! for a fusion power plant based on the costings in the PROCESS costs paper.
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use current_drive_variables, only: pinjmw
    use physics_variables, only: pdivt, powfmw, res_time
    use pfcoil_variables, only: itr_sum
    use pf_power_variables, only: ensxpfm
    use heat_transport_variables, only: pthermmw, psechtmw, helpow
    use fwbs_variables, only: vvmass, rdewex, zdewex
    use cost_variables, only: cost_factor_misc, costexp

    implicit none

    !  Local Variables
    integer :: i, j

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set cost factor for remaining subsystems
    do i=36, 60
       s_cost_factor(i) = cost_factor_misc
    end do

    s_label(36) = "CS and PF coils"
    ! !  Cost of ITER CS and PF magnets
    s_cref(36) = 1538.0D6
    !  Scale with sum of (A x turns x radius) of CS and all PF coils
    s_k(36) = itr_sum
    s_kref(36) = 7.4D8
    s_cost(36) = s_cost_factor(36) * s_cref(36) * (s_k(36) / s_kref(36))**costexp

    s_label(37) = "Vacuum vessel in-wall shielding, ports and in-vessel coils"
    !  Cost of ITER VV in-wall shielding, ports and in-vessel coils
    s_cref(37) = 211.0D6
    !  Scale with vacuum vessel mass (kg)
    s_k(37) = vvmass
    s_kref(37) = 5.2360D6
    s_cost(37) = s_cost_factor(37) * s_cref(37) * (s_k(37) / s_kref(37))**costexp

    s_label(38) = "Divertor"
    !  Cost of ITER divertor
    s_cref(38) = 381.0D6
    !  Scale with max power to SOL (MW)
    s_k(38) = pdivt
    s_kref(38) = 140.0D0
    s_cost(38) = s_cost_factor(38) * s_cref(38) * (s_k(38) / s_kref(38))**costexp

    s_label(39) = 'not used'
    s_label(40) = 'not used'

    s_label(41) = "Ex-vessel neutral beam remote handling equipment"
    !  Cost of ITER Ex-vessel NBI RH equipment
    ! Increased to 90 Mdollar because of press release
    s_cref(41) = 90.0D6
    !  Scale with total aux injected power (MW)
    s_k(41) = pinjmw
    s_kref(41) = 50.0D0
    s_cost(41) = s_cost_factor(41) * s_cref(41) *(s_k(41) / s_kref(41))**costexp

    s_label(42) = 'not used'

    s_label(43) = "Vacuum vessel pressure suppression system"
    !  Cost of ITER Vacuum vessel pressure suppression system
    s_cref(43) = 40.0D6
    !  Scale with total thermal power removed from fusion core (MW)
    s_k(43) = pthermmw + psechtmw
    s_kref(43) = 550.0D0
    s_cost(43) = s_cost_factor(43) * s_cref(43) * (s_k(43) / s_kref(43))**costexp

    s_label(44) = "Cryostat"
    !  Cost of ITER cryostat
    s_cref(44) = 351.0D6
    !  Scale with cryostat external volume (m3)
    s_k(44) = (pi*rdewex**2.0D0) * 2.0D0 * zdewex
    s_kref(44) = 18700.0D0
    s_cost(44) = s_cost_factor(44) * s_cref(44) * (s_k(44) / s_kref(44))**costexp

    s_label(45) = "Heat removal system"
    !  Cost of ITER cooling water system
    s_cref(45) = 724.0D6
    !  Scale with total thermal power removed from fusion core (MW)
    s_k(45) = pthermmw + psechtmw
    s_kref(45) = 550.0D0
    s_cost(45) = s_cost_factor(45) * s_cref(45) * (s_k(45) / s_kref(45))**costexp

    s_label(46) = "Thermal shields"
    !  Cost of ITER thermal shields
    s_cref(46) = 126.0D6
    !  Scale with cryostat surface area (m2)
    s_k(46) = 2.0D0*pi*rdewex * 2.0D0*zdewex + 2*(pi*rdewex**2)
    s_kref(46) = 3902.0D0
    s_cost(46) = s_cost_factor(46) * s_cref(46) * (s_k(46) / s_kref(46))**costexp

    s_label(47) = "Pellet injection system"
    !  Cost of ITER pellet injector and pellet injection system
    s_cref(47) = 25.0D6
    !  Scale with fusion power (MW)
    s_k(47) = powfmw
    s_kref(47) = 500.0D0
    s_cost(47) = s_cost_factor(47) * s_cref(47) * (s_k(47) / s_kref(47))**costexp

    s_label(48) = "Gas injection and wall conditioning system"
    ! !  Cost of ITER gas injection system, GDC, Gi valve boxes
    s_cref(48) = 32.0D6
    !  Scale with fusion power (MW)
    s_k(48) = powfmw
    s_kref(48) = 500.0D0
    s_cost(48) = s_cost_factor(48) * s_cref(48) * (s_k(48) / s_kref(48))**costexp

    s_label(49) = "Vacuum pumping"
    !  Cost of ITER vacuum pumping
    s_cref(49) = 201.0D6
    !  Scale with fusion power (MW)
    s_k(49) = powfmw
    s_kref(49) = 500.0D0
    s_cost(49) = s_cost_factor(49) * s_cref(49) * (s_k(49) / s_kref(49))**costexp

    s_label(50) = "Tritium plant"
    !  Cost of ITER tritium plant
    s_cref(50) = 226.0D6
    !  Scale with fusion power (MW)
    s_k(50) = powfmw
    s_kref(50) = 500.0D0
    s_cost(50) = s_cost_factor(50) * s_cref(50) * (s_k(50) / s_kref(50))**costexp

    s_label(51) = "Cryoplant and distribution"
    !  Cost of ITER Cryoplant and distribution
    s_cref(51) = 397.0D6
    !  Scale with heat removal at 4.5 K approx (W)
    s_k(51) = helpow
    s_kref(51) = 50000.0D0
    s_cost(51) = s_cost_factor(51) * s_cref(51) * (s_k(51) / s_kref(51))**costexp

    s_label(52) = "Electrical power supply and distribution"
    !  Cost of ITER electrical power supply and distribution
    s_cref(52) = 1188.0D6
    !  Scale with total magnetic energy in the poloidal field / resistive diffusion time (W)
    !  For ITER value see
    !  K:\Power Plant Physics and Technology\PROCESS\PROCESS documentation papers\resistive diffusion time.xmcd or pdf
    s_k(52) = ensxpfm * 1.0E6 / res_time
    s_kref(52) = 8.0D9 / 953.0D0
    s_cost(52) = s_cost_factor(52) * s_cref(52) * (s_k(52) / s_kref(52))**costexp

    s_label(53) = "Neutral beam heating and current drive system"
    !  Cost of ITER NB H & CD
    s_cref(53) = 814.0D6
    !  Scale with total auxiliary injected power (MW)
    s_k(53) = pinjmw
    s_kref(53) = 50.0D0
    s_cost(53) = s_cost_factor(53) * s_cref(53) * (s_k(53) / s_kref(53))**costexp

    s_label(54) = "Diagnostics systems"
    !  Cost of ITER diagnostic systems
    s_cref(54) = 640.0D6
    ! No scaling
    s_cost(54) = s_cost_factor(54) * s_cref(54)

    s_label(55) = "Radiological protection"
    !  Cost of ITER radiological protection
    s_cref(55) = 19.0D6
    !  Scale with fusion power (MW)
    s_k(55) = powfmw
    s_kref(55) = 500.0D0
    s_cost(55) = s_cost_factor(55) * s_cref(55) * (s_k(55) / s_kref(55))**costexp

    s_label(56) = "Access control and security systems"
    !  Cost of ITER access control and security systems
    !  Scale with area of cryostat (m2)
    s_k(56) = pi * rdewex**2
    s_kref(56) = 640.0D0
    s_cref(56) = 42.0D6
    s_cost(56) = s_cost_factor(56) * s_cref(56) * (s_k(56) / s_kref(56))**costexp

    s_label(57) = "Assembly"
    !  Cost of ITER assembly
    s_cref(57) = 732.0D6
    !  Scale with total cost of reactor items (cryostat and everything inside it)
    s_k(57) = s_cost(21) + s_cost(27) + s_cost(32) + s_cost(36) + s_cost(37) + &
         s_cost(38) + s_cost(44) + s_cost(46) + s_cost(49)
    s_kref(57) = s_cref(21) + s_cref(27) + s_cref(32) + s_cref(36) + s_cref(37) + &
         s_cref(38) + s_cref(44) + s_cref(46) + s_cref(49)
    s_cost(57) = s_cost_factor(57) * s_cref(57) * (s_k(57) / s_kref(57))

    s_label(58) = "Control and communication"
    !  Cost of ITER control and data access and communication
    s_cref(58) = 219.0D6
    !  Scale with total cost of reactor items (cryostat and everythign inside it)
    s_k(58) = s_cost(21) + s_cost(27) + s_cost(32) + s_cost(36) + s_cost(37) + &
         s_cost(38) + s_cost(44) + s_cost(46) + s_cost(49)
    s_kref(58) = s_cref(21) + s_cref(27) + s_cref(32) + s_cref(36) + s_cref(37) + &
         s_cref(38) + s_cref(44) + s_cref(46) + s_cref(49)
    s_cost(58) = s_cost_factor(58) * s_cref(58) * (s_k(58) / s_kref(58))**costexp

    s_label(59) = "Additional project expenditure"
    !  Cost of ITER additional ITER IO expenditure
    s_cref(59) = 1624.0D6
    s_cost(59) = s_cost_factor(59) * s_cref(59)

    ! Calculate miscellaneous costs
    s_label(60) = "Logistics"
    s_cref(60) = 129.0D6
    !  Scale with cryostat external volume (m)
    s_k(60) = pi * rdewex**2 * 2.0D0 * zdewex
    s_kref(60) = 18700.0D0
    s_cost(60) = s_cost_factor(60) * s_cref(60) * (s_k(60) / s_kref(60))**costexp

    s_label(61) = "Total remaining subsystem costs"
    s_cost(61) = 0.0D0
    do j=36, 60
       s_cost(61) = s_cost(61) + s_cost(j)
    end do

  end subroutine calc_remaining_subsystems


  subroutine write_costs_to_output


    !! Function to output the costs calculations
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! This routine outputs the costs to output file
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use heat_transport_variables, only: pnetelmw
    use process_output, only: oheadr, oshead, ocosts, oblnkl, ovarrf
    use cost_variables, only: coe, cpfact
		use constants, only: mfile
    implicit none

    !  Local Variables
    integer :: i, j, k, l, n, q

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(ofile,'Estimate of "overnight" capital cost for a first of kind power plant (2014 M$)')

    call oshead(ofile,'Buildings (M$)')
    do i=1, 9
       call ocost(ofile, s_label(i), i, s_cost(i)/1.0D6)
    end do

    call oshead(ofile,'Land (M$)')
    do j=10, 13
       call ocost(ofile, s_label(j), j, s_cost(j)/1.0D6)
    end do

    call oshead(ofile,'TF Coils (M$)')
    do k=14, 21
       call ocost(ofile, s_label(k), k, s_cost(k)/1.0D6)
    end do

    call oshead(ofile,'First wall and blanket (M$)')
    do l=22, 27
       call ocost(ofile, s_label(l), l, s_cost(l)/1.0D6)
    end do

    call oshead(ofile,'Active maintenance and remote handling (M$)')
    call ocost(ofile, s_label(28), 28, s_cost(28)/1.0D6)
    call ocost(ofile, s_label(29), 29, s_cost(29)/1.0D6)
    call ocost(ofile, s_label(31), 31, s_cost(31)/1.0D6)

    call oshead(ofile,'Vacuum vessel and liquid nitrogen plant (M$)')
    do n=32, 34
       call ocost(ofile, s_label(n), n, s_cost(n)/1.0D6)
    end do

    call oshead(ofile,'System for converting heat to electricity (M$)')
    call ocost(ofile, s_label(35), 35, s_cost(35)/1.0D6)

    call oshead(ofile,'Remaining subsystems (M$)')
    do q=36, 61
       call ocost(ofile, s_label(q), q, s_cost(q)/1.0D6)
    end do

    call oblnkl(ofile)
    call ocost_vname(ofile, "TOTAL OVERNIGHT CAPITAL COST (M$)", "(total_costs)", total_costs/1.0D6)
    call ocost_vname(ofile, "Annual maintenance cost (M$)", "(maintenance)", maintenance/1.0D6)
    call oblnkl(ofile)
    call ovarrf(ofile,"Net electric output (MW)", '(pnetelmw)', pnetelmw, 'OP ')
    call ovarrf(ofile,"Capacity factor", '(cpfact)', cpfact, 'OP ')
    call ovarrf(ofile,"Mean electric output (MW)", '(mean_electric_output)', mean_electric_output, 'OP ')
    call ovarrf(ofile,"Capital cost / mean electric output ($/W)", '', total_costs/mean_electric_output/1.0D6, 'OP ')
    call ovarrf(ofile, "Levelized cost of electricity ($/MWh)", '(coe)', coe, 'OP ')

  end subroutine write_costs_to_output

  subroutine value_function(x, v)
    !! Value function
    !! author: J Morris, CCFE, Culham Science Centre
    !! None
    !! Function for separative work unit calculation for enrichment cost
    !! PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(dp), intent(in) :: x
    real(dp), intent(out) :: v

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    v = (1.0D0 - 2.0D0*x) * log((1.0D0 - x)/x)

  end subroutine value_function

  subroutine ocost(file,descr,n,value)
    !! Routine to print out the code, description and value
    !! of a cost item from array s in costs_2015

    use process_output, only: ovarrf
		use constants, only: mfile
    implicit none

    !  Arguments
    integer, intent(in) :: file
    integer :: n
    character(len=*), intent(in) :: descr
    character(len=5) :: vname
    real(dp), intent(in) :: value
    !  Local variables
    character(len=70) :: dum70

    if (descr == 'not used') return

    !  Replace descr with dummy string of the correct length.
    dum70 = descr
    write(file,10) dum70, value, ' '
    10  format(1x,a,t73,f10.0, tl1, a)
    ! Create variable name of format s + array entry
    write(vname,"(A2,I2.2,A1)") '(s', n, ')'

    call ovarrf(mfile,descr,vname,value)

  end subroutine ocost

  subroutine ocost_vname(file,descr,vname,value)
    !! Routine to print out the code, description and value
    !! of a cost item not in the array s in costs_2015

    use process_output, only: ovarrf
		use constants, only: mfile
    implicit none

    !  Arguments
    integer, intent(in) :: file
    character(len=*), intent(in) :: descr
    character(len=*), intent(in) :: vname
    real(dp), intent(in) :: value
    !  Local variables
    character(len=70) :: dum70

    if (descr == 'not used') return

    !  Replace descr with dummy string of the correct length.
    dum70 = descr
    write(file,10) dum70, value, ' '
    10  format(1x,a,t73,f10.0, tl1, a)

    call ovarrf(mfile,descr,vname,value)

  end subroutine ocost_vname

end module costs_2015_module
