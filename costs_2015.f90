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
  !+ad_hist  13/05/15 MDK
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
    
  use availability_module
  use buildings_module
  use process_output
  use ccfe_hcpb_module

  implicit none

  !  Precision variable
  integer, parameter :: double = 8
  
  !  Output variables
  integer :: ip, ofile

  !  Costs structure for scaling laws (scl)
  type :: scl
     character(len=80) :: label         ! Description, appears in OUT.DAT
     real(kind=double) :: kref          ! Reference value of scaling parameter
     real(kind=double) :: k             ! Actual value of scaling parameter K        
     real(kind=double) :: cref          ! Reference cost $
     real(kind=double) :: cost          ! Actual cost $
     real(kind=double) :: cost_factor   ! Multiplier f
  end type scl

  !  Scaling law array (unused entries will be zeroes)
  type(scl), dimension(100) :: s
  real(kind=double) :: total_costs, mean_electric_output

  !  Private module variables
  private :: ip, ofile, double, s, total_costs
  
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

    !  Calculate total costs
    total_costs = s(9)%cost + s(13)%cost + s(21)%cost + &
         s(27)%cost + s(31)%cost + s(34)%cost + &
         s(35)%cost + s(61)%cost

    mean_electric_output = pnetelmw * cfactr             
         
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
    !+ad_desc  Buildings have a different scaling law, with fixed cost per unit volume.
    !+ad_desc  Cref is therefore now f.Viter.unit_cost
    !+ad_desc  The costs for individual buildings must not be output,
    !+ad_desc  as the same mean cost per unit volume has been used both for light 
    !+ad_desc  and for shielded buildings
    !+ad_desc  The exponent =1
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

    s(1)%label = "Admin Buildings ($)"
    s(1)%cref = 129000.0D0 * light_build_cost_per_vol
    s(1)%cost = s(1)%cost_factor * s(1)%cref    

    s(2)%label = "Tokamak Complex (excluding hot cell) ($)"        
    s(2)%cref = 1100000.0D0 * tok_build_cost_per_vol   
    !  ITER cryostat volume (m^3)
    s(2)%k = (pi*rdewex**2) * 2.0D0 * zdewex
    s(2)%kref = 18712.0D0
    s(2)%cost = s(2)%cost_factor * s(2)%cref * (s(2)%k / s(2)%kref)    
    
    s(3)%label="Neutral beam buildings ($)"
    s(3)%cref = 28000.0D0 * light_build_cost_per_vol
    !  Scale with neutral beam wall plug power (MW)
    s(3)%k = pwpnb
    s(3)%kref = 120.0D0
    s(3)%cost = s(3)%cost_factor * s(3)%cref * (s(3)%k / s(3)%kref)    
    
    s(4)%label = "Cryoplant buildings ($)"    
    s(4)%cref = 130000.0D0 * light_build_cost_per_vol    
    !  Scale with the total heat load on the cryoplant at ~4.5K (kW)
    s(4)%k = helpow/1.0D3
    s(4)%kref = 61.0D0
    s(4)%cost = s(4)%cost_factor * s(4)%cref * (s(4)%k / s(4)%kref)

    
    s(5)%label = "PF Coil winding building ($)"    
    s(5)%cref = 190000.0D0 * light_build_cost_per_vol    
    !  Scale with the radius of the largest PF coil squared (m^2)
    s(5)%k = pfrmax**2
    s(5)%kref = 12.4D0**2
    s(5)%cost = s(5)%cost_factor * s(5)%cref * (s(5)%k / s(5)%kref)
    
    s(6)%label = "Magnet power supplies and related buildings ($)"
    s(6)%cref = 110000.0D0 * light_build_cost_per_vol    
    !  Scale with TF current per coil (MA)
    s(6)%k = (ritfc/tfno)/1.0D6
    s(6)%kref = 9.1D0
    s(6)%cost = s(6)%cost_factor * s(6)%cref * (s(6)%k / s(6)%kref)

     
    s(7)%label = "Magnet discharge buildings ($)"
    s(7)%cref = 35000.0D0 * light_build_cost_per_vol    
    !  Scale with total stored energy in TF coils (GJ)
    s(7)%k = estotft
    s(7)%kref = 41.0D0
    s(7)%cost  = s(7)%cost_factor * s(7)%cref * (s(7)%k / s(7)%kref)
    
    s(8)%label = "Heat removal system buildings ($)"
    !  ITER volume of cooling water buildings (m^3)
    s(8)%cref = 51000.0D0 * light_build_cost_per_vol
    !  Scale with total thermal power removed from the core (MW)
    s(8)%k = pthermmw + psechtmw
    s(8)%kref = 880.0D0
    s(8)%cost = s(8)%cost_factor * s(8)%cref * (s(8)%k / s(8)%kref)

    s(9)%label = "Total cost of buildings ($)"
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
    !+ad_desc  Land also uses a unit cost, but area is scaled.
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

    s(10)%label = "Land purchasing ($)"

    !  ITER Land area (hectares)
    ITER_total_land_area = 180.0D0
    !  ITER Land area for key buildings (hectares)
    ITER_key_buildings_land_area = 42.0D0
    !  ITER buffer land (hectares)
    ITER_buffer_land_area = ITER_total_land_area - ITER_key_buildings_land_area

    !  Scale with area of cryostat (m)
    s(10)%k = pi * rdewex**2
    s(10)%kref = 638.0D0
    !  Cost of land per hectare (2014 $ / ha)
    s(10)%cref = 318000.0D0
    !  Cost of power plant land (2014 $)
    s(10)%cost = s(10)%cost_factor * s(10)%cref * &
    (ITER_key_buildings_land_area *(s(10)%k / s(10)%kref)**costexp + ITER_buffer_land_area)    
    
    s(11)%label = "Land improvement ($)"    
    !  Cost of clearing ITER land
    s(11)%cref = 214.0D6    
    !  Scale with area of cryostat (m)
    s(11)%k = pi * rdewex**2
    s(11)%kref = 638.0D0
    s(11)%cost = s(11)%cost_factor * (s(11)%k / s(11)%kref )**costexp * s(11)%cref
    
    s(12)%label = "Road improvements ($)"    
    !  Cost of ITER road improvements
    s(12)%cref = 150.0D6
    !  Scale with TF coil longest dimension
    s(12)%k = max(tfborev, tfboreh) + 2.0D0*tfcth
    s(12)%kref = 14.0D0
    s(12)%cost = s(12)%cost_factor * s(12)%cref * (s(12)%k / s(12)%kref)**costexp

    s(13)%label = "Total land costs ($)"
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

    s(14)%label = "TF Coil insertion and welding ($)"    
    !  ITER coil insertion and welding cost (2014 $)
    s(14)%cref = 258.0D6
    !  Scale with total TF coil length (m)
    s(14)%k = tfno * tfleng
    s(14)%kref = 18.0D0*34.1D0
    s(14)%cost = s(14)%cost_factor * s(14)%cref * (s(14)%k / s(14)%kref)**costexp
    

    s(15)%label = "TF coil radial plates ($)"
    !  This is included even if radial plate option is not turned on in TF stress calc
    !  ITER radial plates cost (2014 $)
    s(15)%cref = 395.0D6
    !  Scale with the total TF coil winding pack volume (m^3)
    s(15)%k = 0.5D0*thkwp*(wwp1+wwp2) * tfleng * tfno
    s(15)%kref = 317.3D0
    s(15)%cost = s(15)%cost_factor * s(15)%cref * (s(15)%k / s(15)%kref)**costexp

    s(16)%label = "TF coil winding ($)"    
    !  ITER winding cost (2014 $)
    s(16)%cref = 414.0D6    
    !  Scale with the total turn length (m)
    s(16)%k = tfno * tfleng * turnstf
    s(16)%kref = 82249.0D0
    s(16)%cost = s(16)%cost_factor * s(16)%cref * (s(16)%k / s(16)%kref)**costexp
    
    s(17)%label = "Copper strand for TF coil ($)"    
    !  ITER Chromium plated Cu strand for TF SC cost (2014 $)
    s(17)%cref = 21.0D6    
    !  Scale with total copper mass (kg)
    s(17)%k = whtconcu * tfno
    s(17)%kref = 244.0D3
    s(17)%cost = s(17)%cost_factor * s(17)%cref * (s(17)%k / s(17)%kref)**costexp    
        
    s(18)%label = "Strands with Nb3Sn superconductor and copper stabiliser ($)"    
    !  ITER Nb3Sn SC strands cost (2014 $)
    s(18)%cref = 526.0D6    
    !  Scale with the total mass of Nb3Sn (kg)
    s(18)%k = whtconsc * tfno
    s(18)%kref = 210.0D3
    s(18)%cost = s(18)%cost_factor * s(18)%cref * (s(18)%k / s(18)%kref)**costexp
    
    s(19)%label = "Testing of superconducting strands ($)"    
    !  ITER Nb3Sn strand test costs (2014 $)
    s(19)%cref = 4.0D6
    s(19)%cost = s(19)%cost_factor * s(19)%cref

    s(20)%label = "Cabling and jacketing ($)"    
    !  ITER cabling and jacketing costs (2014 $)
    s(20)%cref = 81.0D6    
    !  Scale with total turn length.
    s(20)%k = tfno * tfleng * turnstf
    s(20)%kref = 82249.0D0
    s(20)%cost = s(20)%cost_factor * s(20)%cref * (s(20)%k / s(20)%kref)**costexp
    
    s(21)%label = "Total TF coil costs ($)"
    s(21)%cost = 0.0D0
    do j=14, 20
       s(21)%cost = s(21)%cost + s(j)%cost
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
    real(kind(1.0D0)) :: product_li6
    real(kind(1.0D0)) :: feed_li6
    real(kind(1.0D0)) :: tail_li6
    real(kind(1.0D0)) :: feed_to_product_mass_ratio
    real(kind(1.0D0)) :: tail_to_product_mass_ratio
    real(kind(1.0D0)) :: p_v, f_v, t_v
    real(kind(1.0D0)) :: swu, total_swu
    real(kind(1.0D0)) :: pebble_packing_fraction
    real(kind(1.0D0)) :: mass_li

    !  First wall W coating variables
    real(kind(1.0D0)) :: W_density

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Set cost factor for tf coils
    do i=22, 27
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
    
    !  Percentage of lithium 6 in the product
    product_li6 = li6enrich / 100.0D0

    !  Percentage of lithium 6 in the feed (natural abundance)
    feed_li6 = 0.075D0

    !  Percentage of lithium 6 in the tail (waste) (75% natural abundance)
    tail_li6 = feed_li6 * 0.75D0
    
    !  SWU will be calculated for a unit mass of product (P=1)

    !  Feed to product mass ratio
    feed_to_product_mass_ratio = (product_li6 - tail_li6) / (feed_li6 - tail_li6)
    
    !  Tail to product mass ratio
    tail_to_product_mass_ratio = (product_li6 - feed_li6) / (feed_li6 - tail_li6)

    !  Calculate value functions
    call value_function(product_li6, p_v)
    call value_function(tail_li6, t_v)
    call value_function(feed_li6, f_v)    

    !  Calculate separative work units per kg
    swu = p_v + tail_to_product_mass_ratio * t_v - feed_to_product_mass_ratio * f_v

    !  Mass of lithium (kg).  Lithium orthosilicate is 22% lithium by mass.
    mass_li = whtblli4sio4 * 0.22

    !  Total swu for lithium in blanket
    total_swu = swu * mass_li

    s(22)%label = "Lithium enrichment ($)"
    !  Reference cost for lithium enrichment (2014 $)
    s(22)%cref = 0.1D6
    !  Reference case of lithium SWU
    s(22)%k =  total_swu
    s(22)%kref = 64.7D0
    s(22)%cost = s(22)%cost_factor * s(22)%cref * (s(22)%k / s(22)%kref)**costexp
    
    
    s(23)%label = "Lithium orthosilicate pebble manufacturing ($)"    
    !  Reference cost of lithium pebble manufacture (2014 $)
    s(23)%cref = 0.65D6
    !  Scale with mass of pebbles (kg)
    s(23)%k = whtblli4sio4
    s(23)%kref = 10.0D0
    s(23)%cost = s(23)%cost_factor * s(23)%cref * (s(23)%k / s(23)%kref)**costexp    
    
    s(24)%label = "Titanium beryllide pebble manufacturing ($)"    
    !  Reference cost of titanium beryllide pebble manufacture (2014 $)
    s(24)%cref = 450.0D6
    !  Scale with mass of titanium beryllide pebbles (kg)
    s(24)%k = whtbltibe12
    s(24)%kref = 1.0D5
    s(24)%cost = s(24)%cost_factor * s(24)%cref * (s(24)%k / s(24)%kref)**costexp

    s(25)%label = "First wall W coating manufacturing ($)"    
    !  Reference (PPCS A) first wall W coating cost (2014 $)
    s(25)%cref = 25.0D6    
    !  W density (kg/m^3)
    W_density = 19250.0D0
    !  First wall W coating mass (kg)
    s(25)%k = fwarea * fw_armour_thickness * W_density
    s(25)%kref = 29000.0D0
    s(25)%cost = s(25)%cost_factor * s(25)%cref * (s(25)%k / s(25)%kref)**costexp    
    
    s(26)%label = "Blanket manufacturing ($)"
    !  Reference case (PPCS A) blanket steel cost (2014 $)
    s(26)%cref = 317.0D6    
    !  Scale with steel mass
    s(26)%k = whtblss
    s(26)%kref = 4070.0D3
    s(26)%cost = s(26)%cost_factor * s(26)%cref * (s(26)%k / s(26)%kref)**costexp

    s(27)%label = "Total first wall and blanket cost ($)"
    s(27)%cost = 0.0D0
    do j=22, 26
       s(27)%cost = s(27)%cost + s(j)%cost
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
    do i=28, 31
       s(i)%cost_factor = cost_factor_rh
    end do

    s(28)%label = "Remote handling equipment ($)"
    !  Reference costs from Sam Ha (2014 $)
    s(28)%cref = 1.0D6 * (128.0D0*num_rh_systems + 377.0D0)
    !  total mass of the blanket (kg)
    s(28)%kref = 0.0D0    !TODO
    s(28)%k = whtblkt    
    s(28)%cost = 0.0D0
    !s(28)%cost = s(28)%cost_factor * s(28)%cref * (s(28)%k / s(28)%kref)**costexp            
    
    s(29)%label = "Active maintenance facility ($)"    
    !  Reference costs from Sam Ha (2014 $)
    s(29)%cref = 1.0D6 * (77.0D0*num_rh_systems + 2868.0D0)       
    !  Scale with blanket mass (kg)
    s(28)%kref = 0.0D0    !TODO
    s(28)%k = whtblkt
    s(29)%cost = 0.0D0
    !s(29)%cost = s(29)%cost_factor * s(29)%cref * (s(29)%k / s(29)%kref)**costexp   
    
    ! s(30) is not in use       

    s(31)%label = "Total remote handling costs ($)"
    s(31)%cost = 0.0D0
    do j=28, 30
       s(31)%cost = s(31)%cost + s(j)%cost
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
    
    !  Set cost factor for vacuum vessel and N plant
    do i=32, 34
       s(i)%cost_factor = cost_factor_vv
    end do

    !  Vacuum vessel
    s(32)%label = "Vacuum vessel ($)"    
    !  ITER reference vacuum vessel cost (2014 $)
    s(32)%cref = 537.0D6
    !  Scale with outermost midplane radius of vacuum vessel squared (m2)
    s(32)%k = (rsldo + ddwi)**2
    s(32)%kref = 94.09D0
    s(32)%cost = s(32)%cost_factor * s(32)%cref * (s(32)%k / s(32)%kref)**costexp

    !  Nitrogen plant
    s(33)%label = "Liquid nitrogen plant ($)"    
    !  ITER reference cost (2014 $)
    s(33)%cref = 86.0D6
    !  Scale with 4.5K cryopower (W)
    s(33)%k = helpow
    s(33)%kref = 50.0D3
    s(33)%cost = s(33)%cost_factor * s(33)%cref * (s(33)%k / s(33)%kref)**costexp

    s(34)%label = "Total liquid nitrogen plant and vacuum vessel ($)"
    s(34)%cost = 0.0D0
    do j=32,33
       s(34)%cost = s(34)%cost + s(j)%cost
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

    s(35)%label = "Energy conversion system ($)"
    !  Set cost factor for energy conversion system
    s(35)%cost_factor = cost_factor_bop    
    !  Cost of reference energy conversion system (Rolls Royce)
    s(35)%cref = 511.0D6
    !  Scale with gross electric power (MWe)
    s(35)%k = pgrossmw
    s(35)%kref = 692.0D0
    s(35)%cost = s(35)%cost_factor * s(35)%cref * (s(35)%k / s(35)%kref)**costexp

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
    do i=36, 60
       s(i)%cost_factor = cost_factor_misc
    end do

    s(36)%label = "CS and PF coils ($)"
    ! !  Cost of ITER CS and PF magnets
    s(36)%cref = 1538.0D6    
    !  Scale with sum of (A x turns x radius) of CS and all PF coils
    s(36)%k = itr_sum
    s(36)%kref = 7.4D8
    s(36)%cost = s(36)%cost_factor * s(36)%cref * (s(36)%k / s(36)%kref)**costexp
    
    s(37)%label = "Vacuum vessel in-wall shielding, ports and in-vessel coils ($)"
    !  Cost of ITER VV in-wall shielding, ports and in-vessel coils
    s(37)%cref = 211.0D6    
    !  Scale with vacuum vessel mass (kg)
    s(37)%k = cryomass
    s(37)%kref = 5.2360D6
    s(37)%cost = s(37)%cost_factor * s(37)%cref * (s(37)%k / s(37)%kref)**costexp

    s(38)%label = "Divertor ($)"
    !  Cost of ITER divertor
    s(38)%cref = 381.0D6    
    !  Scale with max power to SOL (MW)
    s(38)%k = pdivt
    s(38)%kref = 140.0D0
    s(38)%cost = s(38)%cost_factor * s(38)%cref * (s(38)%k / s(38)%kref)**costexp    
    
    s(39)%label = "In-vessel blanket RH system ($)"    
    !  Cost of ITER In-vessel blanket RH system
    s(39)%cref = 187.0D6
    !  Scale with mass of blanket module (kg)
    s(39)%k = whtblkt / (tfno * 5.0D0)
    s(39)%kref = 3500.0
    !s(39)%cost = s(39)%cost_factor * s(39)%cref * (s(39)%k / s(39)%kref)**costexp
    s(39)%cost = 0.0D0  ! TODO  

    s(40)%label = "Viewing and metrology for RH system ($)"    
    !  Cost of ITER Viewing and metrology for RH system
    s(40)%cref = 31.0D6
    s(40)%cost = s(41)%cost_factor * s(40)%cref

    s(41)%label = "Ex-vessel neutral beam remote handling equipment ($)"
    !  Cost of ITER Ex-vessel NBI RH equipment
    s(41)%cref = 27.0D6    
    !  Scale with total aux injected power (MW)
    s(41)%k = pinjmw
    s(41)%kref = 50.0D0
    s(41)%cost = s(41)%cost_factor * s(41)%cref *(s(41)%k / s(41)%kref)**costexp

    ! s(42) not used   
    s(42)%label = 'not used'
    s(42)%cost = 0.0D0

    s(43)%label = "Vacuum vessel pressure suppression system ($)"    
    !  Cost of ITER Vacuum vessel pressure suppression system
    s(43)%cref = 40.0D6    
    !  Scale with total thermal power removed from fusion core (MW)
    s(43)%k = pthermmw + psechtmw
    s(43)%kref = 550.0D0
    s(43)%cost = s(43)%cost_factor * s(43)%cref * (s(43)%k / s(43)%kref)**costexp    

    s(44)%label = "Cryostat ($)"    
    !  Cost of ITER cryostat
    s(44)%cref = 351.0D6    
    !  Scale with cryostat external volume (m3)
    s(44)%k = (pi*rdewex**2.0D0) * 2.0D0 * zdewex
    s(44)%kref = 18700.0D0
    s(44)%cost = s(44)%cost_factor * s(44)%cref * (s(44)%k / s(44)%kref)**costexp    
    
    s(45)%label = "Heat removal system ($)"    
    !  Cost of ITER cooling water system
    s(45)%cref = 724.0D6    
    !  Scale with total thermal power removed from fusion core (MW)
    s(45)%k = pthermmw + psechtmw
    s(45)%kref = 550.0D0
    s(45)%cost = s(45)%cost_factor * s(45)%cref * (s(45)%k / s(45)%kref)**costexp
    
    s(46)%label = "Thermal shields ($)"
    !  Cost of ITER thermal shields
    s(46)%cref = 126.0D6  
    !  Scale with cryostat surface area (m2)
    s(46)%k = 2.0D0*pi*rdewex * 2.0D0*zdewex + 2*(pi*rdewex**2)
    s(46)%kref = 3902.0D0
    s(46)%cost = s(46)%cost_factor * s(46)%cref * (s(46)%k / s(46)%kref)**costexp

    s(47)%label = "Pellet injection system ($)"    
    !  Cost of ITER pellet injector and pellet injection system
    s(47)%cref = 25.0D6    
    !  Scale with fusion power (MW)
    s(47)%k = powfmw
    s(47)%kref = 500.0D0
    s(47)%cost = s(47)%cost_factor * s(47)%cref * (s(47)%k / s(47)%kref)**costexp

    s(48)%label = "Gas injection and wall conditioning system ($)"
    ! !  Cost of ITER gas injection system, GDC, Gi valve boxes
    s(48)%cref = 32.0D6
    !  Scale with fusion power (MW)
    s(48)%k = powfmw
    s(48)%kref = 500.0D0
    s(48)%cost = s(48)%cost_factor * s(48)%cref * (s(48)%k / s(48)%kref)**costexp    

    s(49)%label = "Vacuum pumping ($)"    
    !  Cost of ITER vacuum pumping
    s(49)%cref = 201.0D6    
    !  Scale with fusion power (MW)
    s(49)%k = powfmw
    s(49)%kref = 500.0D0
    s(49)%cost = s(49)%cost_factor * s(49)%cref * (s(49)%k / s(49)%kref)**costexp

    s(50)%label = "Tritium plant ($)"    
    !  Cost of ITER tritium plant
    s(50)%cref = 226.0D6
    !  Scale with fusion power (MW)
    s(50)%k = powfmw
    s(50)%kref = 500.0D0
    s(50)%cost = s(50)%cost_factor * s(50)%cref * (s(50)%k / s(50)%kref)**costexp

    s(51)%label = "Cryoplant and distribution ($)"
    !  Cost of ITER Cryoplant and distribution
    s(51)%cref = 397.0D6    
    !  Scale with heat removal at 4.5 K approx (W)
    s(51)%k = helpow
    s(51)%kref = 50000.0D0
    s(51)%cost = s(51)%cost_factor * s(51)%cref * (s(51)%k / s(51)%kref)**costexp

    s(52)%label = "Electrical power supply and distribution ($)"    
    !  Cost of ITER electrical power supply and distribution
    s(52)%cref = 1188.0D6    
    !  Scale with total magnetic energy in the poloidal field (J)
    s(52)%k = ensxpfm * 1.0E6
    s(52)%kref = 8.0D9
    s(52)%cost = s(52)%cost_factor * s(52)%cref * (s(52)%k / s(52)%kref)**costexp

    s(53)%label = "Neutral beam heating and current drive system ($)"    
    !  Cost of ITER NB H & CD
    s(53)%cref = 814.0D6    
    !  Scale with total auxiliary injected power (MW)
    s(53)%k = pinjmw
    s(53)%kref = 50.0D0
    s(53)%cost = s(53)%cost_factor * s(53)%cref * (s(53)%k / s(53)%kref)**costexp    

    s(54)%label = "Diagnostics systems ($)"    
    !  Cost of ITER diagnostic systems
    s(54)%cref = 640.0D6
    ! No scaling
    s(54)%cost = s(54)%cost_factor * s(54)%cref    

    s(55)%label = "Radiological protection ($)"    
    !  Cost of ITER radiological protection
    s(55)%cref = 19.0D6
    !  Scale with fusion power (MW)
    s(55)%k = powfmw
    s(55)%kref = 500.0D0
    s(55)%cost = s(55)%cost_factor * s(55)%cref * (s(55)%k / s(55)%kref)**costexp

    s(56)%label = "Access control and security systems ($)"    
    !  Cost of ITER access control and security systems
    !  Scale with area of cryostat (m2)
    s(56)%k = pi * rdewex**2
    s(56)%kref = 640.0D0
    s(56)%cref = 42.0D6
    s(56)%cost = s(56)%cost_factor * s(56)%cref * (s(56)%k / s(56)%kref)**costexp    

    s(57)%label = "Assembly ($)"    
    !  Cost of ITER assembly
    s(57)%cref = 732.0D6    
    !  Scale with total cost of reactor items (cryostat and everything inside it)
    s(57)%k = s(21)%cost + s(27)%cost + s(32)%cost + s(36)%cost + s(37)%cost + &
         s(38)%cost + s(44)%cost + s(46)%cost + s(49)%cost 
    s(57)%kref = s(21)%cref + s(27)%cref + s(32)%cref + s(36)%cref + s(37)%cref + &
         s(38)%cref + s(44)%cref + s(46)%cref + s(49)%cref 
    s(57)%cost = s(57)%cost_factor * s(57)%cref * (s(57)%k / s(57)%kref)
    
    s(58)%label = "Control and communication ($)"
    !  Cost of ITER control and data access and communication
    s(58)%cref = 219.0D6    
    !  Scale with total cost of reactor items (cryostat and everythign inside it)
    s(58)%k = s(21)%cost + s(27)%cost + s(32)%cost + s(36)%cost + s(37)%cost + &
         s(38)%cost + s(44)%cost + s(46)%cost + s(49)%cost 
    s(58)%kref = s(21)%cref + s(27)%cref + s(32)%cref + s(36)%cref + s(37)%cref + &
         s(38)%cref + s(44)%cref + s(46)%cref + s(49)%cref 
    s(58)%cost = s(58)%cost_factor * s(58)%cref * (s(58)%k / s(58)%kref)**costexp
    
    s(59)%label = "Additional project expenditure ($)"    
    !  Cost of ITER additional ITER IO expenditure
    s(59)%cref = 1624.0D6
    s(59)%cost = s(59)%cost_factor * s(59)%cref

    ! Calculate miscellaneous costs
    s(60)%label = "Logistics ($)"    
    s(60)%cref = 129.0D6
    !  Scale with cryostat external volume (m)
    s(60)%k = pi * rdewex**2 * 2.0D0 * zdewex
    s(60)%kref = 18700.0D0
    s(60)%cost = s(60)%cost_factor * s(60)%cref * (s(60)%k / s(60)%kref)**costexp    
    
    s(61)%label = "Total remaining subsystem costs ($)"
    s(61)%cost = 0.0D0
    do j=36, 60
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
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    

    call oheadr(ofile,'Estimate of "overnight" capital cost for a first of kind power plant (2014 M$)')

    call oshead(ofile,'Buildings (M$)')
    do i=1, 9
       call ocost(ofile, s(i)%label, s(i)%cost/1.0D6)
    end do

    call oshead(ofile,'Land (M$)')
    do j=10, 13
       call ocost(ofile, s(j)%label, s(j)%cost/1.0D6)
    end do

    call oshead(ofile,'TF Coils (M$)')
    do k=14, 21
       call ocost(ofile, s(k)%label, s(k)%cost/1.0D6)
    end do

    call oshead(ofile,'First wall and blanket (M$)')
    do l=22, 27
       call ocost(ofile, s(l)%label, s(l)%cost/1.0D6)
    end do

    call oshead(ofile,'Active maintenance and remote handling (M$)')
    call ocost(ofile, s(28)%label, s(28)%cost/1.0D6)
    call ocost(ofile, s(29)%label, s(29)%cost/1.0D6)
    call ocost(ofile, s(31)%label, s(31)%cost/1.0D6)
    
    call oshead(ofile,'Vacuum vessel and liquid nitrogen plant (M$)')
    do n=32, 34
       call ocost(ofile, s(n)%label, s(n)%cost/1.0D6)
    end do

    call oshead(ofile,'System for converting heat to electricity (M$)')
    call ocost(ofile, s(35)%label, s(35)%cost/1.0D6)

    call oshead(ofile,'Remaining subsystems (M$)')
    do q=36, 61
       call ocost(ofile, s(q)%label, s(q)%cost/1.0D6)
    end do

    call oblnkl(ofile)
    call ocost(ofile, "TOTAL OVERNIGHT CAPITAL COST (M$)", total_costs/1.0D6)
    call oblnkl(ofile)
    call ovarrf(ofile, "Net electric output (MW)", '(pnetelmw)', pnetelmw)    
    call ovarrf(ofile,"Capacity factor", '(cfactr)', cfactr)
    call ovarrf(ofile,"Mean electric output (MW)", '(mean_electric_output)', mean_electric_output)
    call ovarrf(ofile,"Capital cost / mean electric output ($/W)", &
      '(total_costs/mean_electric_output/1.0D6)', total_costs/mean_electric_output/1.0D6)
    
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
  

subroutine ocost(file,descr,value)

    !+ad_name  ocost
    !+ad_summ  Routine to print out the code, description and value
    !+ad_summ  of a cost item in costs_2015
    !+ad_type  Subroutine

    implicit none

    !  Arguments
    integer, intent(in) :: file
    character(len=*), intent(in) :: descr
    real(kind(1.0D0)), intent(in) :: value
    !  Local variables
    character(len=70) :: dum70
    
    if (descr == 'not used') return
    
    !  Replace descr with dummy string of the correct length.
    dum70 = descr
        write(file,10) dum70, value, ' '
10  format(1x,a,t73,f10.0, tl1, a)
    call ovarrf(mfile,descr,'',value)

  end subroutine ocost

end module costs_2015_module
