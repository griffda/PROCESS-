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
  !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use cost_variables
  use current_drive_variables
  use build_variables
  use fwbs_variables
  use tfcoil_variables
  use physics_variables
  use divertor_variables
  use pfcoil_variables
  use process_output
  use structure_variables
  use vacuum_variables
  use pf_power_variables
  use heat_transport_variables
  use times_variables
  use buildings_variables
  use pulse_variables

  implicit none

  !  Precision variable
  integer, parameter :: double = 8
  
  !  Output variables
  integer :: ip, ofile

  !  Costs structure for scaling laws (scl)
  type :: scl
     character(len=50) :: label
     real(kind=double) :: kref
     real(kind=double) :: k
     real(kind=double) :: cref
     real(kind=double) :: cost
     real(kind=double) :: cost_factor
  end type scl

  !  Scaling law array (unused entries will be zeroes)
  type(scl), dimension(100) :: s
  
  !  Private module variables
  private :: ip, ofile, double, s

  !  Public variables/subroutines
  public :: costs_2015

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
    !+ad_desc  Costs are based on PROCESS costs paper (M. Kovari, J. Morris)
    !+ad_desc  The direct costs are calculated based on parameters input
    !+ad_desc  from other sections of the code.
    !+ad_desc  <P>Costs are in 2014 $, and assume first-of-a-kind components
    !+ad_desc  unless otherwise stated.
    !+ad_prob  None
    !+ad_call  calc_building_costs
    !+ad_hist  05/01/15 JM Initial version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint, outfile

    !  Assign module private variables to iprint and outfile
    ip = iprint
    ofile = outfile

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !  Calculate building costs
    call calc_building_costs

    !  Calculate land costs
    call calc_land_costs

    !  Calculate tf coil costs
    call calc_tf_coil_costs

    !  Calculate fwbs costs
    call calc_fwbs_costs
    
    !  Calculate remote handling costs
    call calc_remote_handling_costs
    
    !  Calculate N plant and vacuum vessel costs
    call calc_n_plant_and_vv_costs

    !  Calculate energy conversion system costs
    call calc_energy_conversion_system
    
    !  Calculate remaining subsystems costs
    call calc_remaining_subsystems

    if ((ip == 0).or.(output_costs == 0)) return    

    !  Output costs
    call write_costs_to_output

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
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !  Set cost factor for buildings
    do i=1, 9
       s(i)%cost_factor = cost_factor_buildings
    end do

    s(1)%label = "Admin Buildings cost (M$)"

    !  ITER volume of admin buildings (m^3)
    s(1)%cref = 129000.0D0 * ITER_build_cost_per_vol
    s(1)%cost = s(1)%cost_factor * s(1)%cref
    

    s(2)%label = "Tokamak Complex (excluding hot cell) cost (M$)"
    
    !  Cost of ITER of tokamak complex (volume * cost per vol)
    s(2)%cref = 1100000.0D0 * ITER_build_cost_per_vol
   
    !  ITER cryostat volume (m^3)
    s(2)%k = (pi*rdewex**2) * 2.0D0 * zdewex
    s(2)%kref = 18712.0D0
    s(2)%cost = s(2)%cost_factor * s(2)%cref * (s(2)%k / s(2)%kref)**costexp
    
    
    s(3)%label="Neutral beam buildings cost (M$)"

    !  Cost of ITER neutral beam buildings (volume * cost per vol)
    s(3)%cref = 28000.0D0 * ITER_build_cost_per_vol

    !  Scale with neutral beam wall plug power (MW)
    s(3)%k = pwpnb
    s(3)%kref = 120.0D0
    s(3)%cost = s(3)%cost_factor * s(3)%cref * (s(3)%k / s(3)%kref)**costexp
    
    
    s(4)%label = "Cryoplant buildings cost (M$)"
    
    !  Cost of ITER cryoplant buildings (volume * cost per vol)
    s(4)%cref = 130000.0D0 * ITER_build_cost_per_vol
    
    !  Scale with the total heat load on the cryoplant at ~4.5K (kW)
    s(4)%k = helpow/1.0D3
    s(4)%kref = 61.0D0
    s(4)%cost = s(4)%cost_factor * s(4)%cref * (s(4)%k / s(4)%kref)**costexp

    
    s(5)%label = "PF Coil winding building cost (M$)"
    
    !  Cost of ITER PF winding buildings (volume * cost per vol)
    s(5)%cref = 190000.0D0 * ITER_build_cost_per_vol
    
    !  Scale with the outer radius of the largest PF coil squared (m^2)
    s(5)%k = pfrmax**2
    s(5)%kref = 12.4D0**2
    s(5)%cost = s(5)%cost_factor * s(5)%cref * (s(5)%k / s(5)%kref)**costexp

    
    s(6)%label = "Magnet power supplies and related buildings cost (M$)"

    !  Cost of ITER magnet power supplies and related buildings (volume * cost per vol)
    s(6)%cref = 110000.0D0 * ITER_build_cost_per_vol
    
    !  Scale with TF current per coil (MA)
    s(6)%k = (ritfc/tfno)/1.0D6
    s(6)%kref = 9.1D0
    s(6)%cost = s(6)%cost_factor * s(6)%cref * (s(6)%k / s(6)%kref)**costexp

     
    s(7)%label = "Magnet discharge buildings cost (M$)"
    
    !  Cost of ITER magnet discharge buildings (volume * cost per vol)
    s(7)%cref = 35000.0D0 * ITER_build_cost_per_vol
    
    !  Scale with total stored energy in TF coils (GJ)
    s(7)%k = estotf*tfno
    s(7)%kref = 41.0D0
    s(7)%cost  = s(7)%cost_factor * s(7)%cref * (s(7)%k / s(7)%kref)**costexp

    
    s(8)%label = "Cooling water buildings cost (M$)"

    !  ITER volume of cooling water buildings (m^3)
    s(8)%cref = 51000.0D0 * ITER_build_cost_per_vol

    !  Scale with total thermal power removed from the core (MW)
    s(8)%k = priheat
    s(8)%kref = 880.0D0
    s(8)%cost = s(8)%cost_factor * s(8)%cref * (s(8)%k / s(8)%kref)**costexp


    s(9)%label = "Total cost of buildings (M$)"
    s(9)%cost = 0.0D0
    do j=1, 8
       s(9)%cost = s(9)%cost + s(j)%cost
    end do

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
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j
    real(kind=double) :: ITER_total_land_area
    real(kind=double) :: ITER_key_buildings_land_area
    real(kind=double) :: ITER_buffer_land_area

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set cost factor for land
    do i=10, 13
       s(i)%cost_factor = cost_factor_land
    end do

    s(10)%label = "Land purchasing costs (M$)"

    !  ITER Land area (hectares)
    ITER_total_land_area = 180.0D0

    !  ITER Land area for key buildings (hectares)
    ITER_key_buildings_land_area = 42.0D0

    !  ITER buffer land (hectares)
    ITER_buffer_land_area = ITER_total_land_area - ITER_key_buildings_land_area

    !  Scale with diameter of cryostat (m)
    s(10)%k = 2.0D0 * rdewex
    s(10)%kref = 28.5D0

    !  Cost of land per hectare (2014 $ / ha)
    s(10)%cref = 260000.0D0

    !  Cost of power plant land (2014 $)
    s(10)%cost = s(10)%cost_factor * (ITER_key_buildings_land_area * & 
         (s(10)%k / s(10)%kref)**costexp + ITER_buffer_land_area) * &
         s(10)%cref

    
    s(11)%label = "Land clearing costs (M$)"
    
    !  Cost of clearing ITER land (area (ha) * cost per ha)
    s(11)%cref = 460000.0D0 * ITER_key_buildings_land_area
    
    !  Scale with diameter of cryostat (m)
    s(11)%k = 2.0D0 *rdewex
    s(11)%kref = 28.5D0
    s(11)%cost = s(11)%cost_factor * (s(11)%k / s(11)%kref )**costexp * s(11)%cref

    
    s(12)%label = "Road works cost (M$)"
    
    !  Cost of ITER road works
    s(12)%cref = 150000000.0D0

    !  Scale with TF coil longest dimension
    s(12)%k = max(tfborev, tfboreh) + 2.0D0*tfcth
    s(12)%kref = 14.0D0
    s(12)%cost = s(12)%cost_factor * s(12)%cref * (s(12)%k / s(12)%kref)**costexp


    s(13)%label = "Total land costs (M$)"
    s(13)%cost = 0.0D0
    do j=10, 12
       s(13)%cost = s(13)%cost + s(j)%cost
    end do

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
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set cost factor for tf coils
    do i=14, 20
       s(i)%cost_factor = cost_factor_tf_coils
    end do
    

    s(14)%label = "TF Coil insertion and welding cost (M$)"
    
    !  ITER coil insertion and welding cost (2014 $)
    s(14)%cref = 258000000.0

    !  Scale with total TF coil length (m)
    s(14)%k = tfno * tfleng
    s(14)%kref = 18.0D0*34.1D0
    s(14)%cost = s(14)%cost_factor * s(14)%cref * (s(14)%k / s(14)%kref)**costexp
    

    s(15)%label = "TF coil radial plates cost (M$)"
    !  if radial plate option is turned on? TODO
    !  ITER radial plates cost (2014 $)
    s(15)%cref = 395000000.0D0

    !  Scale with the total TF coil volume (m^3)
    s(15)%k = (tficrn + tfocrn)*tfcth*tfleng
    s(15)%kref = 317.3D0
    s(15)%cost = s(15)%cost_factor * s(15)%cref * (s(15)%k / s(15)%kref)**costexp


    s(16)%label = "TF coil winding cost (M$)"
    
    !  ITER winding cost (2014 $)
    s(16)%cref = 414000000.0D0
    
    !  Scale with the total turn length (m)
    s(16)%k = tfno * tfleng * turnstf
    s(16)%kref = 82249.0D0
    s(16)%cost = s(16)%cost_factor * s(16)%cref * (s(16)%k / s(16)%kref)**costexp

    
    s(17)%label = "CrCu strand for TF SC (quench protection Cu) cost (M$)"
    
    !  ITER Chromium plated Cu strand for TF SC cost (2014 $)
    s(17)%cref = 225000000.0D0
    
    !  Scale with total copper mass (excluding mass of copper in Nb3Sn strands) (kg)
    s(17)%k = whtconcu * tfno
    s(17)%kref = 220000.0D0
    s(17)%cost = s(17)%cost_factor * s(17)%cref * (s(17)%k / s(17)%kref)**costexp
    
    
    s(18)%label = "Radial plate prototypes cost (M$)"
    
    !  ITER radial plate prototypes cost (2014 $)
    s(18)%cref = 15000000.0D0
    
    !  Scale with surface are of radial plate (m^2)
    s(18)%k = tfleng * thkwp
    s(18)%kref = 21.1D0
    s(18)%cost = s(18)%cost_factor * s(18)%cref * (s(18)%k / s(18)%kref)**costexp

    
    s(19)%label = "Nb3Sn superconductor strands cost (M$)"
    
    !  ITER Nb3Sn SC strands cost (2014 $)
    s(19)%cref = 526000000.0D0
    
    !  Scale with the total mass of nb3sn strands (kg)
    s(19)%k = whtconsc * tfno
    s(19)%kref = 420000.0D0
    s(19)%cost = s(19)%cost_factor * s(19)%cref * (s(19)%k / s(19)%kref)**costexp

    
    s(20)%label = "Testing of nb3sn strands cost (M$)"
    
    !  ITER Nb3Sn strand test costs (2014 $)
    s(20)%cref = 4200000.0D0
    s(20)%cost = s(20)%cost_factor * s(20)%cref


    s(21)%label = "Cabling and jacketing cost (M$)"
    
    !  ITER cabling and jacketing costs (2014 $)
    s(21)%cref = 81000000.0D0
    
    !  Scale with total turn length.
    s(21)%k = tfno * tfleng * turnstf
    s(21)%kref = 82249.0D0
    s(21)%cost = s(21)%cost_factor * s(21)%cref * (s(21)%k / s(21)%kref)**costexp

    
    s(22)%label = "Total TF coil costs (M$)"
    s(22)%cost = 0.0D0
    do j=14, 21
       s(22)%cost = s(22)%cost + s(j)%cost
    end do

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
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j
    real(kind(1.0D0)) :: cost_total_fwbs

    !  Enrichment variables
    real(kind(1.0D0)) :: product_li6_percentage
    real(kind(1.0D0)) :: feed_li6_percentage
    real(kind(1.0D0)) :: tail_li6_percentage
    real(kind(1.0D0)) :: feed_to_product_mass_ratio
    real(kind(1.0D0)) :: tail_to_product_mass_ratio
    real(kind(1.0D0)) :: p_v, f_v, t_v
    real(kind(1.0D0)) :: swu, total_swu
    real(kind(1.0D0)) :: pebble_packing_fraction
    real(kind(1.0D0)) :: mass_li_pebbles

    !  First wall W coating variables
    real(kind(1.0D0)) :: W_density

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set cost factor for tf coils
    do i=23, 28
       s(i)%cost_factor = cost_factor_fwbs
    end do
    
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
    call value_function(product_li6_percentage/100.0, p_v)
    call value_function(feed_li6_percentage/100.0, f_v)
    call value_function(tail_li6_percentage/100.0, t_v)

    !  Calculate separative work units per kg
    swu = p_v + tail_to_product_mass_ratio * t_v - feed_to_product_mass_ratio * f_v

    !  Pebbles packing fraction (%)
    pebble_packing_fraction = 0.63

    !  Mass of li4sio4 pebbles (kg)
    mass_li_pebbles = whtblli4sio4 * pebble_packing_fraction

    !  Total swu for lithium in blanket
    total_swu = swu * mass_li_pebbles

    s(23)%label = "Lithium enrichment cost (M$)"

    !  Reference cost for Boron enrichment (2014 $)
    s(23)%cref = 23000000.0D0

    !  Reference case of Boron SWU
    s(23)%k =  total_swu
    s(23)%kref = 6900
    s(23)%cost = s(23)%cost_factor * s(23)%cref * (s(23)%k / s(23)%kref)**costexp
    
    
    s(24)%label = "Lithium pebble manufacturing cost (M$)"
    
    !  Reference cost of lithium pebble manufacture (2014 $)
    s(24)%cref = 650000.0D0

    !  Scale with mass of lithium pebbles (kg)
    s(24)%k = mass_li_pebbles
    s(24)%kref = 10.0D0
    s(24)%cost = s(24)%cost_factor * s(24)%cref * (s(24)%k / s(24)%kref)**costexp
    
    
    s(25)%label = "Beryllium pebble manufacturing cost (M$)"
    
    !  Reference cost of beryllium pebble manufacture (2014 $)
    s(25)%cref = 450000000.0D0

    !  Scale with mass of beryllium pebbles (kg)
    s(25)%k = whtbltibe12*pebble_packing_fraction
    s(25)%kref = 100000.0D0
    s(25)%cost = s(25)%cost_factor * s(25)%cref * (s(25)%k / s(25)%kref)**costexp


    s(26)%label = "First wall W coating manufacturing cost (M$)"
    
    !  Reference (PPCS A) first wall W coating cost (2014 $)
    s(26)%cref = 25000000.0D0
    
    !  W density (kg/m^3)
    W_density = 19250.0D0

    !  First wall W coating mass (kg)
    s(26)%k = fwarea * fw_w_thickness * W_density
    s(26)%kref = 29000.0D0
    s(26)%cost = s(26)%cost_factor * s(26)%cref * (s(26)%k / s(26)%kref)**costexp
    
    
    s(27)%label = "Blanket manufacturing (EUROFER) cost (M$)"

    !  Reference case (PPCS A) blanket steel cost (2014 $)
    s(27)%cref = 317300000.0D0
    
    !  Scale with steel mass
    s(27)%k = whtblss
    s(27)%kref = 4070000.0D0
    s(27)%cost = s(27)%cost_factor * s(27)%cref * (s(27)%k / s(27)%kref)**costexp


    s(28)%label = "Total fwbs cost (M$)"
    s(28)%cost = 0.0D0
    do j=23, 27
       s(28)%cost = s(28)%cost + s(j)%cost
    end do
    
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
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j

    !  Divertor RH system variables
    real(kind(1.0D0)) :: ITER_num_div_rh_systems
    real(kind(1.0D0)) :: div_num_rh_systems_ratio
    
    !  First wall and blanket RH system variables
    real(kind(1.0D0)) :: ITER_num_blanket_rh_systems
    real(kind(1.0D0)) :: blanket_num_rh_systems_ratio

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set cost factor for remote handling
    do i=29, 32
       s(i)%cost_factor = cost_factor_rh
    end do

    s(29)%label = "Divertor remote handling system cost (M$)"

    !  Reference ITER costs (2014 $)
    s(29)%cref = 40000000.0D0

    !  Scale with the number of remote handling systems and  mass of a divertor module
    ITER_num_div_rh_systems = 1.0
    div_num_rh_systems_ratio = num_rh_systems / ITER_num_div_rh_systems
    
    !  ITER divertor cassette mass (kg)
    s(29)%kref = 4700.0D0

    if (ipowerflow == 0) then
       s(29)%k = (whtblkt/(1 - fhole))*fhole
    else
       s(29)%k = (whtblkt/(1 - fhole - fdiv - fhcd))*(fdiv)
    end if
    
    !  Cost of power plant divertor remote handling equipment  (2014 $)
    s(29)%cost = s(29)%cost_factor * s(29)%cref * &
         div_num_rh_systems_ratio**costexp * (s(29)%k / s(29)%kref)**costexp
    
    
    s(30)%label = "First wall and blanket RH system cost (M$)"
    
    !  Reference ITER cost (2014 $)
    s(30)%cref = 80000000.0D0
    
    !  Scale with the number of remote handling systems and mass of a blanket segment
    ITER_num_blanket_rh_systems = 1.0
    blanket_num_rh_systems_ratio = num_rh_systems / ITER_num_blanket_rh_systems
    
    !  Scale with blanket mass (kg)
    s(30)%k = whtblkt / (tfno * 5.0D0)
    s(30)%kref = 3500.0D0
    s(30)%cost = s(30)%cost_factor * s(30)%cref * &
         blanket_num_rh_systems_ratio**costexp * (s(30)%k / s(30)%kref)**costexp    


    s(31)%label = "Active maintenance facility cost (M$)"
    
    !  Cost of reference AMF (volume * cost per vol)
    s(31)%cref = 737000.0D0 * ITER_build_cost_per_vol

    !  Scales with plasma major radius compared to EFDA AMF report
    s(31)%k = rmajor
    s(31)%kref = 9.0D0
    s(31)%cost = s(31)%cost_factor * (s(31)%k / s(31)%kref)**costexp


    s(32)%label = "Total remote handling cost (M$)"
    s(32)%cost = 0.0D0
    do j=29, 31
       s(32)%cost = s(32)%cost + s(j)%cost
    end do    
    
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
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j
    
    !  Vacuum vessel variables
    real(kind(1.0D0)) :: ITER_max_midplane_radius

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set cost factor for vacuum vessel and N plant
    do i=33, 35
       s(i)%cost_factor = cost_factor_vv
    end do

    !  Vacuum vessel
    s(33)%label = "Vacuum vessel cost (M$)"
    
    !  ITER reference vacuum vessel cost (2014 $)
    s(33)%cref = (323000000.0D0 / 7.0D0) * 9.0D0

    !  Scale with outermost midplane radius of vacuum vessel
    s(33)%k = bore + ohcth + gapoh + tfcth + gapds + ddwi + &
         shldith + blnkith + fwith + scrapli + 2.0D0*rminor + &
         scraplo + blnkoth + shldoth + ddwi
    s(33)%kref = 6.472D0
    s(33)%cost = s(33)%cost_factor * s(33)%cref * (s(33)%k / s(33)%kref)**costexp


    !  Nitrogen plant
    s(34)%label = "Nitrogen plant cost (M$)"
    
    !  ITER reference cost (2014 $)
    s(34)%cref = 86000000.0D0

    !  Scale with 4.5K cryopower
    s(34)%k = helpow/1.0D3
    s(34)%kref = 50.0D0
    s(34)%cost = s(34)%cost_factor * s(34)%cref * (s(34)%k / s(34)%kref)**costexp


    s(35)%label = "Total N plant and VV costs (M$)"
    s(35)%cost = 0.0D0
    do j=33,34
       s(35)%cost = s(35)%cost + s(j)%cost
    end do
  
  end subroutine calc_n_plant_and_vv_costs

  subroutine calc_energy_conversion_system
    
    !+ad_name  calc_energy_conversion_system
    !+ad_summ  Function to calculate the cost of the energy conversion system
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the cost of the energy conversion system
    !+ad_desc  for a fusion power plant based on the costings in the PROCESS costs paper.
    !+ad_prob  None
    !+ad_call  some function? (OUTPUT functions)
    !+ad_hist  15/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    s(36)%label = "Energy conversion system cost (M$)"

    !  Set cost factor for energy conversion system
    s(36)%cost_factor = cost_factor_bop
    
    !  Cost of reference energy conversion system (Rolls Royce)
    s(36)%cref = 511000000.0D0

    !  Scale with gross electric power (MWe)
    s(36)%k = pgrossmw
    s(36)%kref = 692.0D0
    s(36)%cost = s(36)%cost_factor * s(36)%cref * (s(36)%k / s(36)%kref)**costexp

  end subroutine calc_energy_conversion_system

  subroutine calc_remaining_subsystems
    
    !+ad_name  calc_remaining_subsystems
    !+ad_summ  Function to calculate the cost of the remaining subsystems
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the cost of the remaining subsystems
    !+ad_desc  for a fusion power plant based on the costings in the PROCESS costs paper.
    !+ad_prob  None
    !+ad_call  some function? (OUTPUT functions)
    !+ad_hist  15/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j, k
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !  Set cost factor for remaining subsystems
    do i=37, 60
       s(i)%cost_factor = cost_factor_misc
    end do

    s(37)%label = "CS and PF magnets cost (M$)"

    ! !  Cost of ITER CS and PF magnets
    s(37)%cref = 1538000000.0D0
    
    !  Scale with sum of (kA x turns x radius) of CS and all PF coils
    s(37)%k = itr_sum/1.0D3
    s(37)%kref = 7.4D5
    s(37)%cost = s(37)%cost_factor * s(37)%cref * (s(37)%k / s(37)%kref)**costexp

    
    s(38)%label = "VV in-wall shielding, ports and in-vessel coils cost (M$)"

    !  Cost of ITER VV in-wall shielding, ports and in-vessel coils
    s(38)%cref = 211000000.0D0
    
    !  Scale with vacuum vessel mass (t)
    s(38)%k = dewmkg/1.0D3
    s(38)%kref = 5236.0D0
    s(38)%cost = s(38)%cost_factor * s(38)%cref * (s(38)%k / s(38)%kref)**costexp


    s(39)%label = "Divertor cost (M$)"

    !  Cost of ITER divertor
    s(39)%cref = 381000000.0D0
    
    !  Scale with max power to SOL (MW)
    s(39)%k = pdivt
    s(39)%kref = 140.0D0
    s(39)%cost = s(39)%cost_factor * s(39)%cref * (s(39)%k / s(39)%kref)**costexp
    
    
    s(40)%label = "In-vessel blanket RH system cost (M$)"
    
    !  Cost of ITER In-vessel blanket RH system
    s(40)%cref = 187000000.0D0

    !  Scale with mass of blanket module (kg)
    s(40)%k = whtblkt / (tfno * 5.0D0)
    s(40)%kref = 3500.0
    s(40)%cost = s(40)%cost_factor * s(40)%cref * (s(40)%k / s(40)%kref)**costexp


    s(41)%label = "Viewing and metrology for RH system cost (M$)"
    
    !  Cost of ITER Viewing and metrology for RH system
    s(41)%cref = 31000000.0D0
    s(41)%cost = s(41)%cost_factor * s(41)%cref


    s(42)%label = "Ex-vessel NBI RH equipment cost (M$)"

    !  Cost of ITER Ex-vessel NBI RH equipment
    s(42)%cref = 27000000.0D0
    
    !  Scale with total aux injected power (MW)
    s(42)%k = pinjmw
    s(42)%kref = 50.0D0
    s(42)%cost = s(42)%cost_factor * s(42)%cref *(s(42)%k / s(42)%kref)**costexp


    s(43)%label = "Hot Cell Maintenance equipment cost (M$)"
    
    !  Cost of Hot Cell Maintenance equipment
    s(43)%cref = 62000000.0D0
    
    !  Scale with Blanket mass (tonne) x no. of parallel RH systems
    s(43)%k = (whtblkt / (tfno * 5.0D0))*num_rh_systems
    s(43)%kref = 1530.0D0
    s(43)%cost = s(43)%cost_factor * s(43)%cref * (s(43)%k / s(43)%kref)**costexp
    

    s(44)%label = "Vacuum vessel pressure suppression system cost (M$)"
    
    !  Cost of ITER Vacuum vessel pressure suppression system
    s(44)%cref = 40000000.0D0
    
    !  Scale with total thermal power removed from fusion core (MW)
    s(44)%k = priheat
    s(44)%kref = 550.0D0
    s(44)%cost = s(44)%cost_factor * s(44)%cref * (s(44)%k / s(44)%kref)**costexp
    

    s(45)%label = "Cyrostat cost (M$)"
    
    !  Cost of ITER cryostat
    s(45)%cref = 351000000.0D0
    
    !  Scale with (cryostat shell radius)^2 x height (external dims) (m3)
    s(45)%k = (pi*rdewex**2.0D0) * 2.0D0 * zdewex
    s(45)%kref = 18700.0D0
    s(45)%cost = s(45)%cost_factor * s(45)%cref * (s(45)%k / s(45)%kref)**costexp    

    s(46)%label = "Cooling water cost (M$)"
    
    !  Cost of ITER cooling water 
    s(46)%cref = 724000000.0D0
    
    !  Scale with total thermal power removed from fusion core (MW)
    s(46)%k = priheat
    s(46)%kref = 550.0D0
    s(46)%cost = s(46)%cost_factor * s(46)%cref * (s(46)%k / s(46)%kref)**costexp

    
    s(47)%label = "Thermal shield cost (M$)"

    !  Cost of ITER thermal shield
    s(47)%cref = 126000000.0D0
    
    !  Scale with (cryostat shell radius) x height (external dims) (m2)
    s(47)%k = rdewex
    s(47)%kref = 400.0D0
    s(47)%cost = s(47)%cost_factor * s(47)%cref * (s(47)%k / s(47)%kref)**costexp


    s(48)%label = "Pellet injector and pellet injection system cost (M$)"
    
    !  Cost of ITER pellet injector and pellet injection system
    s(48)%cref = 25000000.0D0
    
    !  Scale with core fuelling rate (atoms/s)
    s(48)%k = qfuel
    s(48)%kref = 9.7D22
    s(48)%cost = s(48)%cost_factor * s(48)%cref * (s(48)%k / s(48)%kref)**costexp


    s(49)%label = "Gas injection system, GDC, Gi valve boxes cost (M$)"

    ! !  Cost of ITER gas injection system, GDC, Gi valve boxes
    s(49)%cref = 32000000.0D0

    !  Scale with core fuelling rate (atoms/s)
    s(49)%k = qfuel
    s(49)%kref = 9.7D22
    s(49)%cost = s(49)%cost_factor * s(49)%cref * (s(49)%k / s(49)%kref)**costexp
    

    s(50)%label = "Vacuum pumping cost (M$)"
    
    !  Cost of ITER vacuum pumping
    s(50)%cref = 201000000.0D0
    
    !  Scale with fusion power (MW)
    s(50)%k = powfmw
    s(50)%kref = 500.0D0
    s(50)%cost = s(50)%cost_factor * s(50)%cref * (s(50)%k / s(50)%kref)**costexp


    s(51)%label = "Tritium plant cost (M$)"
    
    !  Cost of ITER tritium plant
    s(51)%cref = 226000000.0D0

    !  Scale with fusion power (MW)
    s(51)%k = powfmw
    s(51)%kref = 500.0D0
    s(51)%cost = s(51)%cost_factor * s(51)%cref * (s(51)%k / s(51)%kref)**costexp


    s(52)%label = "Cryoplant and distribution cost (M$)"

    !  Cost of ITER Cryoplant and distribution
    s(52)%cref = 397000000.0D0
    
    !  Scale with heat removal at 4.5 K approx (W)
    s(52)%k = helpow
    s(52)%kref = 50000.0D0
    s(52)%cost = s(52)%cost_factor * s(52)%cref * (s(52)%k / s(52)%kref)**costexp


    s(53)%label = "Electrical power supply and distribution cost (M$)"
    
    !  Cost of ITER electrical power supply and distribution
    s(53)%cref = 1188000000.0D0
    
    !  Scale with total magnetic energy stored by CS and PF coils (GJ)
    s(53)%k = ensxpfm/1.0D3
    s(53)%kref = 15.56
    s(53)%cost = s(53)%cost_factor * s(53)%cref * (s(53)%k / s(53)%kref)**costexp


    s(54)%label = "NB H & CD cost (M$)"
    
    !  Cost of ITER NB H & CD
    s(54)%cref = 814000000.0D0
    
    !  Scale with Total auxiliary injected power (MW)
    s(54)%k = pinjmw
    s(54)%kref = 50.0D0
    s(54)%cost = s(54)%cost_factor * s(54)%cref * (s(54)%k / s(54)%kref)**costexp
    

    s(55)%label = "Diagnostics systems cost (M$)"
    
    !  Cost of ITER diagnostic systems
    s(55)%cref = 640000000.0D0
    s(55)%cost = s(55)%cost_factor * s(55)%cref
    

    s(56)%label = "Radiological protection cost (M$)"
    
    !  Cost of ITER radiological protection
    s(56)%cref = 19000000.0D0

    !  Scale with fusion power (MW)
    s(56)%k = powfmw
    s(56)%kref = 500.0D0
    s(56)%cost = s(56)%cost_factor * s(56)%cref * (s(56)%k / s(56)%kref)**costexp


    s(57)%label = "Access control and security systems cost (M$)"
    
    !  Cost of ITER access control and security systems
    s(57)%cref = 42000000.0D0

    !  Scale with inner area (equivalent to ITER platform) (hectare)
    s(57)%k = efloor/1.0D4
    s(57)%kref = 42.0
    s(57)%cost = s(57)%cost_factor * s(57)%cref * (s(57)%k / s(57)%kref)**costexp
    

    s(58)%label = "Assembly cost (M$)"
    
    !  Cost of ITER assembly
    s(58)%cref = 732000000.0D0
    
    !  Scale with total cost of reactor items (cryostat and everything inside it)
    s(58)%k = s(22)%cost + s(28)%cost + s(33)%cost + s(36)%cost + s(38)%cost + &
         s(39)%cost + s(45)%cost + s(47)%cost + s(48)%cost + s(49)%cost + &
         s(50)%cost + s(54)%cost + s(55)%cost
    s(58)%kref = 2996000000.0D0
    s(58)%cost = s(58)%cost_factor * s(58)%cref * (s(58)%k / s(58)%kref)**costexp

    
    s(59)%label = "Control and data access and communication cost (M$)"

    !  Cost of ITER control and data access and communication
    s(59)%cref = 219000000.0D0
    
    !  Scale with total cost of reactor items (cryostat and everythign inside it)
    s(59)%k = s(22)%cost + s(28)%cost + s(33)%cost + s(36)%cost + s(38)%cost + &
         s(39)%cost + s(45)%cost + s(47)%cost + s(48)%cost + s(49)%cost + &
         s(50)%cost + s(54)%cost + s(55)%cost 
    s(59)%kref = 2996000000.0D0
    s(59)%cost = s(59)%cost_factor * s(59)%cref * (s(59)%k / s(59)%kref)**costexp

    
    s(60)%label = "Additional ITER IO expenditure cost (M$)"
    
    !  Cost of ITER additional ITER IO expenditure
    s(60)%cref = 1624000000.0D0
    s(60)%cost = s(60)%cost_factor * s(60)%cref


    s(61)%label = "Total remaining subsystem costs (M$)"
    s(61)%cost = 0.0D0
    do j=37, 60
       s(61)%cost = s(61)%cost + s(j)%cost
    end do

  end subroutine calc_remaining_subsystems

  subroutine write_costs_to_output

    
    !+ad_name  write_costs_to_output
    !+ad_summ  Function to output the costs calculations
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine outputs the costs to output file
    !+ad_prob  None
    !+ad_hist  20/01/15 JM  Initial Version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Local Variables
    integer :: i, j, k, l, m, n, p, q
    real(kind=double) :: total_costs
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

    call oheadr(ofile,'Costings (2015 model)')
    call oblnkl(ofile)

    call oshead(ofile,'Buildings')
    do i=1, 9
       call ocosts(ofile,'', s(i)%label, s(i)%cost/1.0D6)
    end do
    call oblnkl(ofile)

    call oshead(ofile,'Land')
    do j=10, 13
       call ocosts(ofile,'', s(j)%label, s(j)%cost/1.0D6)
    end do
    call oblnkl(ofile)

    call oshead(ofile,'TF Coils')
    do k=14, 22
       call ocosts(ofile,'', s(k)%label, s(k)%cost/1.0D6)
    end do
    call oblnkl(ofile)

    call oshead(ofile,'First wall and blanket')
    do l=23, 28
       call ocosts(ofile,'', s(l)%label, s(l)%cost/1.0D6)
    end do
    call oblnkl(ofile)

    call oshead(ofile,'Remote handling')
    do m=29, 32
       call ocosts(ofile,'', s(m)%label, s(m)%cost/1.0D6)
    end do
    call oblnkl(ofile)

    call oshead(ofile,'N plant and vacuum vessel')
    do n=33, 35
       call ocosts(ofile,'', s(n)%label, s(n)%cost/1.0D6)
    end do
    call oblnkl(ofile)

    call oshead(ofile,'Energy conversion systems')
    call ocosts(ofile,'', s(36)%label, s(36)%cost/1.0D6)
    call oblnkl(ofile)

    call oshead(ofile,'Remaining subsystems')
    do q=37, 61
       call ocosts(ofile,'', s(q)%label, s(q)%cost/1.0D6)
    end do
    call oblnkl(ofile)

    !  Calculate total costs
    total_costs = s(9)%cost + s(13)%cost + s(22)%cost + &
         s(28)%cost + s(32)%cost + s(35)%cost + &
         s(36)%cost + s(61)%cost

    call oblnkl(ofile)
    call ocosts(ofile,'', "Total Costs (M$)", total_costs/1.0D6)
    
  end subroutine write_costs_to_output
  

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
    !+ad_docs  PROCESS Costs Paper (M. Kovari, J. Morris)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(in) :: x
    real(kind(1.0D0)), intent(out) :: v

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    v = (1.0D0 - 2.0D0*x) * log((1.0D0 - x)/x)

  end subroutine value_function

end module costs_2015_module
