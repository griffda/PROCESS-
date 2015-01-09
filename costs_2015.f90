! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module costs_2015_module

  !+ad_name  costs_module
  !+ad_summ  Module containing fusion power plant costing algorithms
  !+ad_type  Module
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_args  N/A
  !+ad_desc  This module contains the PROCESS fusion power plant costing model,
  !+ad_desc  based on ITER costs and the PROCESS costs paper
  !+ad_prob  None
  !+ad_call  some function
  !+ad_hist  05/01/15 JM  Initial version of module
  !+ad_stat  Okay
  !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P. Knight)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use cost_variables
  use current_drive_variables

  implicit none

  private
  public :: costs_2015

  !  Various cost account values (M$)

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine costs_2015(outfile,iprint)

    !+ad_name  costs
    !+ad_summ  Cost accounting for a fusion power plant
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine performs the cost accounting for a fusion power plant.
    !+ad_desc  Costs are based on PROCESS costs paper (M. Kovari, J. Morris, P.knight)
    !+ad_desc  The direct costs are calculated based on parameters input
    !+ad_desc  from other sections of the code.
    !+ad_desc  <P>Costs are in 2014 $, and assume first-of-a-kind components
    !+ad_desc  unless otherwise stated.
    !+ad_prob  None
    !+ad_call  calc_building_costs
    !+ad_hist  05/01/15 JM Initial version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P Knight)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint, outfile

    !  Local Variables
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !  Calculate building costs
    call calc_building_costs


  end subroutine costs_2015

  
  subroutine calc_building_costs

    !+ad_name  calc_building_costs
    !+ad_summ  Function to calculate the cost of all buildings.
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the building costs for a fusion power plant 
    !+ad_desc  based on the costings in the PROCESS costs Paper.
    !+ad_prob  None
    !+ad_call  some function? (OUTPUT functions)
    !+ad_hist  05/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P. Knight)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local Variables

    !  Total cost
    real(kind(1.0D0)) :: cost_buildings_total
    
    !  Admin building variables
    real(kind(1.0D0)) :: cost_admin_buildings
    real(kind(1.0D0)) :: ITER_admin_building_vol
    
    !  Tokamak complex variables
    real(kind(1.0D0)) :: cost_tokamak_buildings
    real(kind(1.0D0)) :: ITER_tokamak_building_vol
    real(kind(1.0D0)) :: ITER_cryo_vol
    real(kind(1.0D0)) :: cryo_size_ratio

    !  Neutral beam building variables
    real(kind(1.0D0)) :: cost_NBI_buildings
    real(kind(1.0D0)) :: ITER_NBI_buildings_vol
    real(kind(1.0D0)) :: ITER_NBI_wp_power
    real(kind(1.0D0)) :: NBI_wp_power_ratio

    !  Cryoplant building variables
    real(kind(1.0D0)) :: cost_cryoplant_buildings
    real(kind(1.0D0)) :: ITER_cryoplant_buildings_vol
    real(kind(1.0D0)) :: ITER_cryoplant_heat_load
    real(kind(1.0D0)) :: cryoplant_heat_load_ratio

    !  PF coil winding building variables
    real(kind(1.0D0)) :: cost_PF_wind_buildings
    real(kind(1.0D0)) :: ITER_PF_wind_buildings_vol
    real(kind(1.0D0)) :: ITER_largest_PF_radius_sq
    real(kind(1.0D0)) :: PF_largest_radius_sq_ratio

    !  Magnet power supplies and related building variables
    real(kind(1.0D0)) :: cost_mag_pow_sup_buildings
    real(kind(1.0D0)) :: ITER_mag_pow_sup_buildings_vol
    real(kind(1.0D0)) :: ITER_TF_current_per_coil
    real(kind(1.0D0)) :: TF_current_per_coil_ratio

    !  Magnet discharge buildings
    real(kind(1.0D0)) :: cost_mag_discharge_buildings
    real(kind(1.0D0)) :: ITER_mag_discharge_buildings_vol
    real(kind(1.0D0)) :: ITER_tot_stored_energy_TF
    real(kind(1.0D0)) :: TF_tot_stored_energy_ratio

    !  Cooling water buildings
    real(kind(1.0D0)) :: cost_cooling_water_buildings
    real(kind(1.0D0)) :: ITER_cooling_water_buildings_vol
    real(kind(1.0D0)) :: ITER_tot_thermal_power_removed
    real(kind(1.0D0)) :: tot_thermal_power_removed_ratio


    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !  Administration buildings
     
    !  ITER volume of admin buildings (m^3)
    ITER_admin_building_vol = 129000.0D0

    !  Cost of power plant admin buildings (2014 $)
    cost_admin_buildings = cost_factor_buildings * ITER_admin_building_vol * &
         ITER_build_cost_per_vol

    
    !  Tokamak complex (excluding Hot Cell)

    !  ITER volume of tokamak complex (m^3)
    ITER_tokamak_building_vol = 1100000.0D0
    
    !  Scale with the overall size of the cryostat    
    !  ITER cryostat volume (m^3)
    ITER_cryo_vol = 18712.0D0

    !  cryostat size ratio
    cryo_size_ratio = vdewex / ITER_cryo_vol

    !  Cost of power plant tokamak complex (2014 $)
    cost_tokamak_buildings = cost_factor_buildings * ITER_tokamak_building_vol * &
         (cryo_size_ratio**costexp) * ITER_build_cost_per_vol
    
    
    !  Neutral beam buildings

    !  ITER volume of neutral beam buildings (m^3)
    ITER_NBI_buildings_vol = 28000.0D0

    !  Scale with neutral beam wall plug power (MW)
    ITER_NBI_wp_power = 120.0D0
    
    !  NBI power ratio
    NBI_wp_power_ratio = pwpnb / ITER_NBI_wp_power
    
    !  Cost of power plant neutral beam buildings (2014 $)
    cost_NBI_buildings = cost_factor_buildings * ITER_NBI_buildings_vol * &
         (NBI_wp_power_ratio**costexp) * ITER_build_cost_per_vol
    
    
    !  Cryoplant buildings
    
    !  ITER volume of cryoplant buildings (m^3)
    ITER_cryoplant_buildings_vol = 130000.0D0
    
    !  Scale with the total heat load on the cryoplant at ~4.5K (kW)
    ITER_cryoplant_heat_load = 61.0D0

    !  Cryoplant heat load ratio
    cryoplant_heat_load_ratio = (helpow / 1.0D3) / ITER_cryoplant_heat_load

    !  Cost of power plant cryoplant buildings (2014 $)
    cost_cryoplant_buildings = cost_factor_buildings * ITER_cryoplant_buildings_vol * &
         (cryoplant_heat_load_ratio**costexp) * ITER_build_cost_per_vol

    
    !  PF Coil winding building
    
    !  ITER volume of PF winding buildings (m^3)
    ITER_PF_wind_buildings_vol = 190000.0D0
    
    !  Scale with the outer radius of the largest PF coil squared (m^2)
    ITER_largest_PF_radius_sq = 12.4D0**2
    
    !  Outer radius of the largest PF coil squared ratio
    PF_largest_radius_sq_ratio =  / ITER_largest_PF_radius_sq

    !  Cost of power plant PF coil winding buildings (2014 $)
    cost_PF_wind_buildings = cost_factor_buildings * ITER_PF_wind_buildings_vol * &
         (PF_largest_radius_sq_ratio**costexp) * ITER_build_cost_per_vol

    
    !  Magnet power supplies and related buildings

    !  ITER volume of magnet power supplies and related buildings (m^3)
    ITER_mag_pow_sup_buildings_vol = 110000.0D0
    
    !  Scale with TF current per coil (MA)
    ITER_TF_current_per_coil = 9.1D0
    
    !  TF current per coil ratio
    TF_current_per_coil_ratio = (ritfc/tfno) / ITER_TF_current_per_coil
    
    !  Cost of power plant magnet power supplies and related buildings (2014 $)
    cost_mag_pow_sup_buildings = cost_factor_buildings * ITER_mag_pow_sup_buildings_vol * &
         (TF_current_per_coil_ratio**costexp) * ITER_build_cost_per_vol

    
    !  Magnet discharge buildings
    
    !  ITER volume of magnet discharge buildings
    ITER_mag_discharge_buildings_vol = 35000.0D0
    
    !  Scale with total stored energy in TF coils (GJ)
    ITER_tot_stored_energy_TF = 41.0D0
    
    !  Total power plant stored energy in TF coils ratio
    TF_tot_stored_energy_ratio = (estotf*tfno) / ITER_tot_stored_energy_TF
    
    !  Cost of magnet discharge buildings (2014 $)
    cost_mag_discharge_buildings = cost_factor_buildings * ITER_mag_discharge_buildings_vol * &
         (TF_tot_stored_energy_ratio**costexp) * ITER_build_cost_per_vol

    
    !  Cooling water buildings

    !  ITER volume of cooling water buildings (m^3)
    ITER_cooling_water_buildings_vol = 51000.0D0

    !  Scale with total thermal power removed from the core (MW)
    ITER_tot_thermal_power_removed = 880.0D0

    !  Total thermal power remove from the core ratio
    tot_thermal_power_removed_ratio = priheat / ITER_tot_thermal_power_removed

    !  Cost of cooling water buildings (2014 $)
    cost_cooling_water_buildings = cost_factor_buildings * ITER_cooling_water_buildings_vol * &
         (tot_thermal_power_removed_ratio**costexp) * ITER_build_cost_per_vol


    !  Total cost of buildings (2014 $)
    cost_buildings_total = cost_admin_buildings + cost_tokamak_buildings + cost_NBI_buildings + &
         cost_PF_wind_buildings + cost_mag_pow_sup_buildings + cost_mag_discharge_buildings + &
         cost_cooling_water_buildings
    
    
    !  Output to file if option chosen

    if ((iprint == 0).or.(output_costs == 0)) return

    !  Output section for building costs
    
    !  TODO

  end subroutine calc_building_costs


  subroutine calc_land_costs

    !+ad_name  calc_land_costs
    !+ad_summ  Function to calculate the cost of land for the power plant
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the land cost for a fusion power plant 
    !+ad_desc  based on the costings in the PROCESS costs Paper.
    !+ad_prob  None
    !+ad_call  some function? (OUTPUT functions)
    !+ad_hist  05/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P. Knight)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local Variables

    !  Total cost of land and clearing
    real(kind(1.0D0)) :: cost_land_total

    !  Land Costs
    real(kind(1.0D0)) :: cost_land
    real(kind(1.0D0)) :: ITER_total_land_area
    real(kind(1.0D0)) :: ITER_key_buildings_land_area
    real(kind(1.0D0)) :: ITER_buffer_land_area
    real(kind(1.0D0)) :: ITER_cryostat_diameter
    real(kind(1.0D0)) :: cost_of_land_per_hectare

    !  Clearing Costs
    real(kind(1.0D0)) :: cost_clearing
    real(kind(1.0D0)) :: cost_of_clearing_per_hectare

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Land Costs

    !  ITER Land area (hectares)
    ITER_total_land_area = 180.0D0

    !  ITER Land area for key buildings (hectares)
    ITER_key_buildings_land_area = 42.0D0

    !  ITER buffer land (hectares)
    ITER_buffer_land_area = ITER_total_land_area - ITER_key_buildings_land_area

    !  Scale with diameter of cryostat (m)
    ITER_cryostat_diameter = 28.5D0

    !  Diameter of cryostat ratio
    diameter_cryostat_ratio = (2.0D0 * rdewex) / ITER_cryostat_diameter

    !  Cost of land per hectare (2014 $ / ha)
    cost_of_land_per_hectare = 260000.0D0

    !  Cost of power plant land (2014 $)
    cost_land = cost_factor_land * (ITER_key_buildings_land_area * &
         (diameter_cryostat_ratio**costexp) + ITER_buffer_land_area) * &
         cost_of_land_per_hectare

    
    !  Clearing costs
    
    !  Cost of clearing land per hectare (2014 $ / ha)
    cost_of_clearing_per_hectare = 460000.0D0
    
    !  Cost of clearing the key buildings area for power plant
    cost_clearing = cost_factor_land * ITER_key_buildings_land_area * &
         (diameter_cryostat_ratio**costexp) * cost_of_clearing_per_hectare

    !  Total land and clearing costs (2014 $)
    cost_land_total = cost_land + cost_clearing

    
    !  Road works
    !  TODO

   
    !  Output to file if option chosen

    if ((iprint == 0).or.(output_costs == 0)) return

    !  Output section for land costs
    
    !  TODO

  end subroutine calc_land_costs

  subroutine calc_tf_coil_costs

    !+ad_name  calc_tf_coil_costs
    !+ad_summ  Function to calculate the cost of the TF coils for the power plant
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the cost of the TF coils for a fusion power
    !+ad_desc  plant based on the costings in the PROCESS costs Paper.
    !+ad_prob  None
    !+ad_call  some function? (OUTPUT functions)
    !+ad_hist  06/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P. Knight)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local Variables
    real(kind(1.0D0)) :: cost_tf_coils_total
    real(kind(1.0D0)) :: ITER_TF_coil_height
    real(kind(1.0D0)) :: TF_coil_height_ratio

    !  Coil insertion and welding variables
    real(kind(1.0D0)) :: cost_coil_insertion_welding
    real(kind(1.0D0)) :: ITER_coil_insertion_welding_cost
    real(kind(1.0D0)) :: ITER_total_TF_coil_length
    real(kind(1.0D0)) :: total_TF_coil_length_ratio

    !  Radial plates
    real(kind(1.0D0)) :: cost_radial_plates
    real(kind(1.0D0)) :: ITER_radial_plates_cost
    real(kind(1.0D0)) :: ITER_total_TF_coil_volume
    real(kind(1.0D0)) :: total_TF_coil_volume_ratio

    !  Winding
    real(kind(1.0D0)) :: cost_winding
    real(kind(1.0D0)) :: ITER_winding_cost
    real(kind(1.0D0)) :: ITER_total_turn_length
    real(kind(1.0D0)) :: total_turn_length_ratio

    !  Chromium plated Cu strand for TF superconductor (quench protection copper)
    real(kind(1.0D0)) :: cost_crcu_strands
    real(kind(1.0D0)) :: ITER_crcu_strands_cost
    real(kind(1.0D0)) :: ITER_crcu_strands_mass
    real(kind(1.0D0)) :: crcu_strands_mass_ratio

    !  Radial plate prototypes
    real(kind(1.0D0)) :: cost_radial_plate_proto
    real(kind(1.0D0)) :: ITER_radial_plate_proto_cost
    real(kind(1.0D0)) :: ITER_radial_plate_s_area
    real(kind(1.0D0)) :: radial_plate_s_area_ratio
        
    !  Nb3Sn conductor strands
    real(kind(1.0D0)) :: cost_nb3sn_strands
    real(kind(1.0D0)) :: ITER_nb3sn_strands_cost
    real(kind(1.0D0)) :: ITER_total_nb3sn_mass
    real(kind(1.0D0)) :: total_nb3sn_mass_ratio

    !  Testing of TF Nb3Sn strands
    real(kind(1.0D0)) :: cost_nb3sn_strands_testing
    real(kind(1.0D0)) :: ITER_nb3sn_strands_testing_cost

    !  Cabling and jacketing (uses total turn length and total turn length ratio)
    real(kind(1.0D0)) :: cost_cabling_jacketing
    real(kind(1.0D0)) :: ITER_cabling_jacketing_cost

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Coil insertion and welding

    !  ITER coil insertion and welding cost (2014 $)
    ITER_coil_insertion_welding_cost = 258000000.0

    !  Scale with total TF coil length (m)
    ITER_total_TF_coil_length = 18.0D0*34.1D0

    !  Total TF coil length ratio
    total_TF_coil_length_ratio = (tfno * tfleng) / ITER_total_TF_coil_length
    
    !  Cost of power plant coil insertion and welding (2014 $)
    cost_coil_insertion_welding = cost_factor_tf_coils * ITER_coil_insertion_welding_cost * &
         (total_TF_coil_length_ratio**costexp)
    

    !  Radial plates

    !  if radial plate option is turned on? TODO

    !  ITER radial plates cost (2014 $)
    ITER_radial_plates_cost = 395000000.0D0

    !  Scale with the total TF coil volume (m^3)
    ITER_total_TF_coil_volume = 317.3D0
    
    !  Total TF coil volume ratio
    total_TF_coil_volume_ratio = ((tficrn + tfocrn)*tfcth*tfleng) / ITER_total_TF_coil_volume

    !  Cost of power plant radial plates (2014 $)
    cost_radial_plates = cost_factor_tf_coils * ITER_radial_plates_cost * &
         total_TF_coil_volume_ratio**costexp


    !  Winding
    
    !  ITER winding cost (2014 $)
    ITER_winding_cost = 414000000.0D0
    
    !  Scale with the total turn length (m)
    ITER_total_turn_length = 82249.0D0
    
    !  Total turn length ratio
    total_turn_length_ratio = (tfno * tfleng * turnstf) / ITER_total_turn_length
    
    !  Cost of power plant winding (2014 $)
    cost_winding = cost_factor_tf_coils * ITER_winding_cost * & 
         total_turn_length_ratio**costexp

    
    !  Chromium plated Cu strand for TF superconductor (quench protection copper)
    
    !  ITER Chromium plated Cu strand for TF SC cost (2014 $)
    ITER_crcu_strands_cost = 225000000.0D0
    
    !  Scale with total copper mass (excluding mass of copper in Nb3Sn strands) (kg)
    ITER_crcu_strands_mass = 220000.0D0
    
    !  Total copper mass ratio
    crcu_strands_mass_ratio = (whtconcu*tfno) / ITER_crcu_strands_mass
    
    !  Cost of power plant chromium plated Cu strand for TF SC (2014 $)
    cost_crcu_strands = cost_factor_tf_coils * ITER_crcu_strands_cost * &
         crcu_strands_mass_ratio**costexp
    
    
    !  Radial plate prototypes
    
    !  ITER radial plate prototypes cost (2014 $)
    ITER_radial_plate_proto_cost = 15000000.0D0
    
    !  Scale with surface are of radial plate (m^2)
    ITER_radial_plate_s_area = 21.1D0
    
    !  Radial plate surface area ratio
    radial_plate_s_area_ratio = (tfleng*thkwp) / ITER_radial_plate_s_area

    !  Cost of power plant radial plate prototypes (2014 $)
    cost_radial_plate_proto = cost_factor_tf_coils * ITER_radial_plate_proto_cost * &
         radial_plate_s_area_ratio**costexp

    
    !  Nb3Sn superconductor strands
    
    !  ITER Nb3Sn SC strands cost (2014 $)
    ITER_nb3sn_strands_cost = 526000000.0D0
    
    !  Scale with the total mass of nb3sn strands (kg)
    ITER_total_nb3sn_mass = 420000.0D0

    !  Total mass of Nb3Sn strands ratio
    total_nb3sn_mass_ratio = (whtconsc * tfno) / ITER_total_nb3sn_mass

    !  Cost of power plant Nb3Sn strands (2014 $)
    cost_nb3sn_strands = cost_factor_tf_coils * ITER_nb3sn_strands_cost * &
         total_nb3sn_mass_ratio**costexp

    
    !  Testing of nb3sn strands
    
    !  ITER Nb3Sn strand test costs (2014 $)
    ITER_nb3sn_strands_testing_cost = 4200000.0D0
    
    !  No scaling currently (could scale with current density per strand). Cost is (2014 $)
    cost_nb3sn_strands_testing = cost_factor_tf_coils * ITER_nb3sn_strands_testing_cost


    !  Cabling and jacketing
    
    !  ITER cabling and jacketing costs (2014 $)
    ITER_cabling_jacketing_cost = 81000000.0D0
    
    !  Scale with total turn length.
    !  Use value from "winding" cost calculation

    !  Cost for power plant cabling and jacketing (2014 $)
    cost_cabling_jacketing = cost_factor_tf_coils * ITER_cabling_jacketing_cost * &
         total_turn_length_ratio**costexp


    !  Output to file if option chosen

    if ((iprint == 0).or.(output_costs == 0)) return

    !  Output section for TF coil costs
    
    !  TODO

  end subroutine calc_tf_coil_costs

  subroutine calc_fwbs_costs

    !+ad_name  calc_fwbs_costs
    !+ad_summ  Function to calculate the cost of the first wall, blanket and shield
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the cost of the first wall, blanket and shield
    !+ad_desc  coils for a fusion power plant based on the costings in the PROCESS costs paper.
    !+ad_prob  None
    !+ad_call  some function? (OUTPUT functions)
    !+ad_hist  07/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P. Knight)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local Variables
    real(kind(1.0D0)) :: cost_total_fwbs

    !  Enrichment variables
    real(kind(1.0D0)) :: product_li6_percentage
    real(kind(1.0D0)) :: feed_li6_percentage
    real(kind(1.0D0)) :: tail_li6_percentage
    real(kind(1.0D0)) :: feed_to_product_mass_ratio
    real(kind(1.0D0)) :: tail_to_product_mass_ratio
    real(kind(1.0D0)) :: p_v, f_v, t_v
    real(kind(1.0D0)) :: swu, total_swu
    real(kind(1.0D0)) :: boron_swu
    real(kind(1.0D0)) :: boron_reference_cost
    real(kind(1.0D0)) :: pebble_packing_fraction
    real(kind(1.0D0)) :: mass_li_pebbles
    real(kind(1.0D0)) :: cost_li6_enrichment

    !  Li pebble manufacture variables
    real(kind(1.0D0)) :: reference_mass_li_pebbles
    real(kind(1.0D0)) :: li_pebble_mass_ratio
    real(kind(1.0D0)) :: reference_cost_li_pebble_manufacture
    real(kind(1.0D0)) :: cost_li_pebble_manufacture

    !  Be pebble manufacture variables
    real(kind(1.0D0)) :: reference_mass_be_pebbles
    real(kind(1.0D0)) :: be_pebble_mass_ratio
    real(kind(1.0D0)) :: reference_cost_be_pebble_manufacture
    real(kind(1.0D0)) :: cost_be_pebble_manufacture

    !  First wall W coating variables
    real(kind(1.0D0)) :: cost_first_wall_W_coating_manufacture
    real(kind(1.0D0)) :: first_wall_W_coating_thickness
    real(kind(1.0D0)) :: W_density
    real(kind(1.0D0)) :: first_wall_W_coating_mass
    real(kind(1.0D0)) :: reference_first_wall_W_coating_mass
    real(kind(1.0D0)) :: reference_first_wall_W_coating_cost

    !  Blanket manufacture variables
    real(kind(1.0D0)) :: cost_blanket_steel_manufacture
    real(kind(1.0D0)) :: reference_blanket_steel_mass
    real(kind(1.0D0)) :: reference_blanket_steel_cost
    real(kind(1.0D0)) :: blanket_steel_mass_ratio

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !  Enrichment
    !  Costs based on the number of separative work units (SWU) required
    !
    !  SWU = P V(x_p) + T V(x_t) - F V(x_f)
    !
    !  where V(x) is the value function
    !
    !  V(x) = (1 - 2x)ln((1-x)/x)
    
    !  Percentage of lithium 6 in the procduct
    product_li6_percentage = li6enrich

    !  Percentage of lithium 6 in the feed (natural abundance)
    feed_li6_percentage = 7.5D0

    !  Percentage of lithium 6 in the tail (waste) (75% natural abundance)
    tail_li6_percentage = feed_li6_percentage * 0.75D0
    
    !  SWU will be calculated for a unit mass of product (P=1)

    !  Feed to product mass ratio
    feed_to_product_mass_ratio = (product_li6_percentage - tail_li6_percentage) / &
         (feed_li6_percentage - tail_li6_percentage)
    
    !  Tail to product mass ratio
    tail_to_product_mass_ratio = (product_li6_percentage - feed_li6_percentage) / &
         (feed_li6_percentage - tail_li6_percentage)

    !  Calculate value functions
    p_v = call value_function(product_li6_percentage)
    f_v = call value_function(feed_li6_percentage)
    t_v = call value_function(tail_li6_percentage)

    !  Calculate separative work units per kg
    swu = p_v + tail_to_product_mass_ratio * t_v - feed_to_product_mass_ratio * f_v

    !  Pebbles packing fraction (%)
    pebble_packing_fraction = 0.63

    !  Mass of li4sio4 pebbles (kg)
    mass_li_pebbles = whtblli4sio4 * pebble_packing_fraction

    !  Total swu for lithium in blanket
    total_swu = swu * mass_li_pebbles

    !  Reference case of Boron SWU
    boron_swu = 6900

    !  Reference cost for Boron enrichment (2014 $)
    boron_reference_cost = 23000000.0D0

    !  Ratio of swu
    ratio_swu = total_swu / boron_swu

    !  Cost of li6 enrichment (2014 $)
    cost_li6_enrichment = cost_factor_fwbs * boron_reference_cost * &
         ratio_swu**costexp

    
    !  Lithium pebble manufacture
    
    !  Reference cost of lithium pebble manufacture (2014 $)
    reference_cost_li_pebble_manufacture = 650000.0D0

    !  Scale with mass of lithium pebbles (kg)
    reference_mass_li_pebbles = 10.0D0
    
    !  Ratio of lithium pebble masses
    li_pebble_mass_ratio = mass_li_pebbles / reference_mass_li_pebbles
    
    !  Cost of Li4SiO4 pebbles for power plant (2014 $)
    cost_li_pebble_manufacture = cost_factor_fwbs * reference_cost_li_pebble_manufacture * &
         li_pebble_mass_ratio**costexp
    
    
    ! Beryllium pebble manufacture
    
    !  Reference cost of beryllium pebble manufacture (2014 $)
    reference_cost_be_pebble_manufacture = 450000000.0D0

    !  Scale with mass of beryllium pebbles (kg)
    reference_mass_be_pebbles = 100000.0D0

    !  Mass of beryllium pebbles
    mass_be_pebbles = whtbltibe12 * pebble_packing_fraction
    
    !  Ratio of beryllium pebble masses
    be_pebble_mass_ratio = mass_be_pebbles / reference_mass_be_pebbles
    
    !  Cost of TiBe12 pebbles for power plant (2014 $)
    cost_be_pebble_manufacture = cost_factor_fwbs * reference_cost_be_pebble_manufacture * &
         be_pebble_mass_ratio**costexp


    !  First wall W coating manufacture
    
    !  First wall W coating thickness (m)
    first_wall_W_coating_thickness = fw_w_thickness

    !  First wall W coating volume (m^3)
    first_wall_W_coating_volume = fwarea * first_wall_W_coating_thickness
    
    !  W density (kg/m^3)
    W_density = 19250.0D0

    !  First wall W coating mass (kg)
    first_wall_W_coating_mass = W_density * first_wall_W_coating_volume
    
    !  Reference (PPCS A) first wall W coating mass (kg)
    reference_first_wall_W_coating_mass = 29000.0D0
    
    !  Reference (PPCS A) first wall W coating cost (2014 $)
    reference_first_wall_W_coating_cost = 25000000.0D0
    
    !  Cost of W first wall coating manufacture (2014 $)
    cost_first_wall_W_coating_manufacture = cost_factor_fwbs * first_wall_W_coating_mass * &
         unit_cost_fw_manufacture
    
    
    !  Blanket manufacture (EUROFER)

    !  Reference case (PPCS A) blanket steel mass (kg)
    reference_blanket_steel_mass = 4070000.0D0
    
    !  Reference case (PPCS A) blanket steel cost (2014 $)
    reference_blanket_steel_cost = 317300000.0D0
    
    !  Blanket steel mass ratio
    blanket_steel_mass_ratio = whtblss / reference_blanket_steel_mass

    !  Cost of power plant blanket steel (2014 $)
    cost_blanket_steel_manufacture = cost_factor_fwbs * reference_blanket_steel_cost * &
         blanket_steel_mass_ratio**costexp


    !  Total cost of fwbs
    cost_total_fwbs = cost_li6_enrichment + cost_li_pebble_manufacture + &
         cost_be_pebble_manufacture + cost_first_wall_W_coating_manufacture + &
         cost_blanket_steel_manufacture


    !  Output to file if option chosen

    if ((iprint == 0).or.(output_costs == 0)) return

    !  Output section for TF coil costs
    
    !  TODO    
    
  end subroutine calc_fwbs_costs

  subroutine calc_remote_handling_costs

    !+ad_name  calc_remote_handling_costs
    !+ad_summ  Function to calculate the cost of the remote handling facilities
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the cost of the remote handling facilities
    !+ad_desc  for a fusion power plant based on the costings in the PROCESS costs paper.
    !+ad_prob  None
    !+ad_call  some function? (OUTPUT functions)
    !+ad_hist  09/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P. Knight)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local Variables
    real(kind(1.0D0)) :: cost_total_rh

    !  Divertor RH system variables
    real(kind(1.0D0)) :: ITER_div_rh_costs
    real(kind(1.0D0)) :: ITER_div_cassette_mass
    real(kind(1.0D0)) :: ITER_num_div_rh_systems
    real(kind(1.0D0)) :: div_num_rh_systems_ratio
    real(kind(1.0D0)) :: div_mass_estimate
    real(kind(1.0D0)) :: div_cassette_mass_ratio
    real(kind(1.0D0)) :: cost_divertor_rh
    
    !  First wall and blanket RH system variables
    real(kind(1.0D0)) :: cost_blanket_rh
    real(kind(1.0D0)) :: ITER_blanket_rh_costs
    real(kind(1.0D0)) :: ITER_num_blanket_rh_systems
    real(kind(1.0D0)) :: blanket_num_rh_systems_ratio
    real(kind(1.0D0)) :: ITER_blanket_segment_mass
    real(kind(1.0D0)) :: blanket_mass_estimate
    real(kind(1.0D0)) :: blanket_module_mass_ratio

    !  Active maintenance facility variables
    real(kind(1.0D0)) :: cost_amf
    real(kind(1.0D0)) :: reference_amf_volume
    real(kind(1.0D0)) :: reference_major_radius
    real(kind(1.0D0)) :: major_radius_ratio

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Divertor remote handling system

    !  Reference ITER costs (2014 $)
    ITER_div_rh_costs = 40000000.0D0

    !  Scale with the number of remote handling systems and 
    !  mass of a divertor module

    !  ITER Number of RH systems
    ITER_num_div_rh_systems = 1.0

    !  Ratio of number of divertor RH systems
    div_num_rh_systems_ratio = num_rh_systems / ITER_num_rh_systems
    
    !  ITER divertor cassette mass (kg)
    ITER_div_cassette_mass = 4700.0D0

    !  Estimated divertor mass
    if (ipowerflow == 0) then
       div_mass_estimate = (whtblkt/(1 - fhole))*fhole
    else
       div_mass_estimate = (whtblkt/(1 - fhole - fdiv - fhcd))*(fdiv)
    end if

    !  Ratio of divertor cassette masses
    div_cassette_mass_ratio = div_mass_estimate / ITER_div_cassette_mass
    
    !  Cost of power plant divertor remote handling equipment  (2014 $)
    cost_divertor_rh = cost_factor_rh * ITER_div_rh_costs * &
         div_num_rh_systems_ratio**costexp * div_cassette_mass_ratio**costexp
    
    
    !  First wall and blanket RH system
    
    !  Reference ITER cost (2014 $)
    ITER_blanket_rh_costs = 80000000.0D0
    
    !  Scale with the number of remote handling systems and 
    !  mass of a blanket segment
    
    !  ITER Number of RH systems
    ITER_num_blanket_rh_systems = 1.0

    !  Ratio of number of divertor RH systems
    blanket_num_rh_systems_ratio = num_rh_systems / ITER_num_rh_systems
    
    !  ITER Total blanket module mass (kg)
    ITER_blanket_segment_mass = (3500.0D0*440.0D0) / (18.0D0 * 5.0D0)

    !  Blanket mass (kg)
    blanket_mass_estimate = whtblkt / (tfno * 5.0D0)
    
    !  Ratio of divertor cassette masses
    blanket_module_mass_ratio = blanket_mass_estimate / ITER_blanket_module_mass
    
    !  Cost of power plant blanket remote handling equipment  (2014 $)
    cost_blanket_rh = cost_factor_rh * ITER_blanket_rh_costs * &
         blanket_num_rh_systems_ratio**costexp * blanket_module_mass_ratio**costexp    


    !  Active maintenance facility
    
    !  Scales with plasma major radiu compared to EFDA AMF report

    !  reference volume (m^3)
    reference_amf_volume = 737000.0D0

    !  reference plasma major radius (m)
    reference_major_radius = 9.0D0
    
    !  Major radius ratio
    major_radius_ratio = rmajor / reference_major_radius
    
    !  Cost of power plant amf (2014 $)
    cost_amf = cost_factor_rh * major_radius_ratio**costexp * &
         ITER_build_cost_per_vol


    !  Total fwbs remote handling cost (2014 $)
    cost_total_rh = cost_divertor_rh + cost_blanket_rh + cost_amf
    

    !  Output to file if option chosen

    if ((iprint == 0).or.(output_costs == 0)) return

    !  Output section for TF coil costs
    
    !  TODO        
    
  end subroutine calc_remote_handling_costs

  subroutine calc_n_plant_and_vv_costs
    
    !+ad_name  calc_n_plant_and_vv
    !+ad_summ  Function to calculate the cost of the nitrogen plant and vacuum vessel
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the cost of the nitrogen plant and vacuum vessel
    !+ad_desc  for a fusion power plant based on the costings in the PROCESS costs paper.
    !+ad_prob  None
    !+ad_call  some function? (OUTPUT functions)
    !+ad_hist  09/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P. Knight)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local Variables
    
    !  Vacuum vessel variables
    real(kind(1.0D0)) :: ITER_vv_cost_per_sector
    real(kind(1.0D0)) :: ITER_vv_cost
    real(kind(1.0D0)) :: ITER_max_midplane_radius

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Vacuum vessel
    
    !  ITER reference vacuum vessel cost (2014 $) per sector
    ITER_vv_cost_per_sector = 323000000.0D0 / 7.0D0

    !  ITER reference vacuum vessel cost (2014 $)
    ITER_vv_cost = 9.0D0 * ITER_vv_cost_per_sector

    !  Scale with outermost midplane radius of vacuum vessel

    !  ITER vacuum vessel width midplane radius (m)
    ITER_max_midplane_radius = 6.472D0

    !  Power plant vacuum vessel width
    vacuum_vessel_width = (rad build up to vv outer) - (rad build up to vv inner)

    !  Nitrogen plant
  
  end subroutine calc_n_plant_and_vv_costs

  subroutine value_function(x, v)

    !+ad_name  value_function
    !+ad_summ  Value function
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  Value function for separative work unit calculation
    !+ad_prob  None
    !+ad_hist  07/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris, P. Knight)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind(1.0D0)) intent(in) :: x
    real(kind(1.0D0)) intent(out) :: v

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    v = (1.0D0 - 2.0D0*x) * log((1.0D0 - x)/x)

  end subroutine value_function

end module costs_2015_module
