! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module buildings_module
  !! author: J. Morris, P. Knight, R. Chapman (UKAEA)
  !!
  !! This module contains routines for calculating the
  !! parameters of the fusion power plant buildings.
  !!

#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif

  implicit none

contains

  subroutine bldgcall(outfile, iprint)
    !! author: J. Morris, P. Knight, R. Chapman (UKAEA)
    !!
    !! This routine calls the buildings calculations.

  !!!!!!!!!!! ************ !!!!!!!!!!!!!!!!!!!
    ! use build_variables, only: r_tf_inboard_mid, r_tf_outboard_mid, tfthko, tfcth, &
    !   hmax, rsldo, d_vv_top, d_vv_bot, vgap2, rsldi
    ! use fwbs_variables, only: whtshld, rdewex
    ! use buildings_variables, only: cryvol, volrci, rbvol, rmbvol, wsvol, elevol
    ! use heat_transport_variables, only: helpow
    ! use pfcoil_variables, only: pfrmax, pfmmax
    ! use tfcoil_variables, only: whttf, n_tf

    use build_variables, only: r_tf_inboard_mid, r_tf_outboard_mid, tfcth, tfthko, &
      hmax
    use pfcoil_variables, only: pfrmax
    use fwbs_variables, only: rdewex
    !use buildings_variables, only: cryvol, volrci, rbvol, rmbvol, wsvol, elevol
    !use heat_transport_variables, only: helpow    
    !use tfcoil_variables, only: whttf, n_tf

    implicit none

    !  Arguments 

    integer, intent(in) :: iprint
    !! switch for writing to output file (1=yes)

    integer, intent(in) :: outfile
    !! output file unit

    !  Local variables

    real(dp) :: tf_radial_dim
    !! Radial dimension of TF coil (m)

    real(dp) :: tf_vertical_dim
    !! Vertical dimension of TF coil (m)

    ! real(dp) :: tfh
    ! real(dp) :: tfmtn
    ! real(dp) :: tfri
    ! real(dp) :: tfro


    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! Find width, in radial dimension, of TF coil (m)
    !  = (outboard mid-leg radial position + half-thickness of outboard leg) - 
    !    (inboard mid-leg radial position - half-thickness of inboard leg)
    tf_radial_dim = r_tf_outboard_mid + ( tfthko * 0.5D0 ) - &
                    r_tf_inboard_mid - ( tfcth * 0.5D0 )

    ! Find full height of TF coil (m)
    !  = 2 * (mid-plane to TF coil inside edge + thickness of coil)
    tf_vertical_dim = 2.0D0 * ( hmax + tfthko )


    ! tfh = (hmax + tfcth)*2.0D0
    ! !! TF coil vertical height (m)
    ! ! Rem : SK not valid for single null

  !   ! Reactor vault wall and roof thicknesses are hardwired
  !   call bldgs(pfrmax,pfmmax,tfro,tfri,tfh,tfmtn,n_tf,rsldo, &
  !     rsldi,2.0D0*(hmax-vgap2)-d_vv_top-d_vv_bot,whtshld, &
  !     rdewex,helpow,iprint,outfile,cryvol,volrci,rbvol,rmbvol, &
  !     wsvol,elevol)
  ! !!!!!!!!!!! ************ !!!!!!!!!!!!!!!!!!!

    ! Calculate building areas and volumes
    call bldgs_sizes(pfrmax, rdewex, tf_radial_dim, tf_vertical_dim, &
    outfile, iprint)

  end subroutine bldgcall

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine bldgs_sizes(pfrmax, rdewex, tf_radial_dim, tf_vertical_dim, &
    outfile, iprint)

    !! Subroutine that estimates the sizes (footprints and volumes) of 
    !! buildings within a fusion power plant.
    !! Some estimates are scaled with parameters of the fusion plant,
    !! some are based on engineering/specialist assumptions,
    !! some are derived from footprints/volumes based on 
    !! assessment of other power plants and/or similar facilities.
    !!
    !! author: R Chapman, UKAEA
    !!
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use buildings_variables, only: i_v_bldgs, &
      reactor_wall_thk, reactor_roof_thk, reactor_fndtn_thk, &
      reactor_clrnc, transp_clrnc, cryostat_clrnc, ground_clrnc, &
      crane_clrnc_h, crane_clrnc_v, crane_arm_h, &
      reactor_hall_l, reactor_hall_w, reactor_hall_h, &
      warm_shop_l, warm_shop_w, warm_shop_h, &
      workshop_l, workshop_w, workshop_h, &
      robotics_l, robotics_w, robotics_h, &
      maint_cont_l, maint_cont_w, maint_cont_h, &
      turbine_hall_l, turbine_hall_w, turbine_hall_h, &
      gas_buildings_l, gas_buildings_w, gas_buildings_h, &
      water_buildings_l, water_buildings_w, water_buildings_h, &
      sec_buildings_l, sec_buildings_w, sec_buildings_h, &
      staff_buildings_area, staff_buildings_h, &
      hcd_building_l, hcd_building_w, hcd_building_h, &
      magnet_pulse_l, magnet_pulse_w, magnet_pulse_h, &
      magnet_trains_l, magnet_trains_w, magnet_trains_h, &
      control_buildings_l, control_buildings_w, control_buildings_h, &
      ilw_smelter_l, ilw_smelter_w, ilw_smelter_h, &
      ilw_storage_l, ilw_storage_w, ilw_storage_h, &
      llw_storage_l, llw_storage_w, llw_storage_h, &
      hw_storage_l, hw_storage_w, hw_storage_h, &
      tw_storage_l, tw_storage_w, tw_storage_h, &
      auxcool_l, auxcool_w, auxcool_h, &
      cryomag_l, cryomag_w, cryomag_h, &
      cryostore_l, cryostore_w, cryostore_h, &
      elecdist_l, elecdist_w, elecdist_h, & 
      elecstore_l, elecstore_w, elecstore_h, & 
      elecload_l, elecload_w, elecload_h, & 
      chemlab_l, chemlab_w, chemlab_h, &
      heat_sink_l, heat_sink_w, heat_sink_h, &
      aux_build_l, aux_build_w, aux_build_h, &
      qnty_sfty_fac, hot_cell_facility_h, hot_sepdist
    use current_drive_variables, only: iefrf
    use tfcoil_variables, only: n_tf
    use cost_variables, only: tlife
    use fwbs_variables, only: bktlife
    use constants, only: pi
    use process_output, only: oheadr, ovarre

    implicit none

    !  Arguments 
    
    real(dp), intent(in) :: pfrmax
    !! radius of largest PF coil (m)
    real(dp), intent(in) :: rdewex
    !! cryostat radius (m)
    real(dp), intent(in) :: tf_radial_dim
    !! Radial dimension of TF coil (m)
    real(dp), intent(in) :: tf_vertical_dim
    !! Vertical dimension of TF coil (m)

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(dp) :: width_reactor_piece
    !! radial width of largest reactor component (m)
    real(dp) :: key_width
    !! half-width of reactor building (m)
    real(dp) :: height_clrnc
    !! vertical clearance required in reactor building (m)

    real(dp) :: reactor_hall_area, reactor_hall_vol
    !! reactor hall footprint (m2), volume (m3)
    real(dp) :: reactor_hall_area_ext
    !! footprint of reactor hall, including walls (m2)
    real(dp) :: reactor_hall_vol_ext
    !! volume of reactor hall, including walls, roof, foundation (m3)
    real(dp) :: reactor_basement_l, reactor_basement_w, reactor_basement_h
    !! reactor length, width, height (m)
    real(dp) :: reactor_basement_area, reactor_basement_vol
    !! reactor basement footprint (m2), volume (m3)
    real(dp) :: reactor_building_vol
    !! volume of reactor hall + basement (m3)

    real(dp) :: hotcell_facility_area, hotcell_facility_vol
    !! reactor hall footprint (m2), volume (m3)

    real(dp) :: chemlab_area, chemlab_vol
    !! chemistry labs footprint (m2), volume (m3)
    real(dp) :: heat_sink_area, heat_sink_vol
    !! heat sink footprint (m2), volume (m3)
    real(dp) :: aux_build_area, aux_build_vol
    !! auxiliary building supporting reactor systems footprint (m2), volume (m3)
    real(dp) :: reactor_aux_area, reactor_aux_vol
    !! total aux buildings supporting reactor systems footprint (m2), volume (m3)

    real(dp) :: hcd_building_area, hcd_building_vol
    !! HCD building footprint (m2), volume (m3)
    real(dp) :: magnet_trains_area, magnet_trains_vol
    !! steady state magnet power trains building footprint (m2), volume (m3)
    real(dp) :: magnet_pulse_area, magnet_pulse_vol
    !! pulsed magnet power building footprint (m2), volume (m3)
    real(dp) :: power_buildings_area, power_buildings_vol
    !! power buildings footprint (m2), volume (m3)

    real(dp) :: control_buildings_area, control_buildings_vol 
    !! control buildings footprint (m2), volume (m3)

    real(dp) :: warm_shop_area, warm_shop_vol
    !! warm shop footprint (m2), volume (m3)

    real(dp) :: workshop_area, workshop_vol
    !! [cold] workshop footprint (m2), volume (m3)
    real(dp) :: robotics_area, robotics_vol
    !!robotics building footprint (m2), volume (m3)
    real(dp) :: maint_cont_area, maint_cont_vol
    !! maintenance control buildings footprint (m2), volume (m3)
    real(dp) :: maintenance_area, maintenance_vol
    !! maintenance buildings footprint (m2), volume (m3)

    real(dp) :: cryomag_area, cryomag_vol
    !! Cryogenic Buildings for Magnet and Fuel Cycle footprint (m2), volume (m3)
    real(dp) :: cryostore_area, cryostore_vol
    !!  Magnet Cryo Storage Tanks footprint (m2), volume (m3)
    real(dp) :: auxcool_area, auxcool_vol    
    !! Auxiliary Cooling Water facility footprint (m2), volume (m3)
    real(dp) :: cryocool_area, cryocool_vol
    !! Cryogenic & cooling buildings footprint (m2), volume (m3)

    real(dp) :: elecdist_area, elecdist_vol
    !! Transformers and electrical distribution footprint (m2), volume (m3)
    real(dp) :: elecload_area, elecload_vol
    !! Electric (eesential and non-essential) load centres footprint (m2), volume (m3)
    real(dp) :: elecstore_area, elecstore_vol
    !! Energy Storage facilities footprint (m2), volume (m3)
    real(dp) :: elec_buildings_area, elec_buildings_vol
    !! Electrical buildings footprint (m2), volume (m3)
    
    real(dp) :: turbine_hall_area, turbine_hall_vol
    !! turbine hall footprint (m2), volume (m3)

    real(dp) :: ilw_smelter_area, ilw_smelter_vol
    !! radioactive waste smelting facility footprint (m2), volume (m3)
    real(dp) :: ilw_storage_area, ilw_storage_vol
    !! ILW storage building footprint (m2), volume (m3)       
    real(dp) :: llw_storage_area, llw_storage_vol
    !! LLW storage building footprint (m2), volume (m3)
    real(dp) :: hw_storage_area, hw_storage_vol
    !! hazardous waste building footprint (m2), volume (m3)
    real(dp) :: tw_storage_area, tw_storage_vol
    !! tritiated building footprint (m2), volume (m3)
    real(dp) :: waste_buildings_area, waste_buildings_vol
    !! waste buildings (amalgamated) footprint (m2), volume (m3)

    real(dp) :: gas_buildings_area, gas_buildings_vol
    !! air & gas supply buildings footprint (m2), volume (m3)
    real(dp) :: water_buildings_area, water_buildings_vol
    !! water, laundry & drainage buildings footprint (m2), volume (m3)
    real(dp) :: sec_buildings_area, sec_buildings_vol
    !! security & safety buildings footprint (m2), volume (m3)

    real(dp) :: staff_buildings_vol
    !! staff buildings volume (m3)


    !!  building footprint (m2), volume (m3)
    !! footprint of  buildings (m2)
    !! volume of  buildings (m3)

    !real(8) :: fwbllife !! rmc fix this!

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    i_v_bldgs = 1 ! rmc debug
    !fwbllife = 9.0D0 !! rmc fix this!


    ! Reactor building    

    ! Lateral size driven by radial width of largest component, from:
    !  PF coil max radius, cryostat radius, TF coil outer radius
    width_reactor_piece = max(pfrmax, rdewex, tf_radial_dim)

    ! Calculate key-width of building (m)
    ! include radial width of largest component *twice*, to allow for construction;
    ! include clearance around reactor, transportation clearance between components,
    ! clearance to building wall for crane operation
    key_width = (2.0D0 * width_reactor_piece) + reactor_clrnc + transp_clrnc + crane_clrnc_h

    ! Width of reactor building
    ! allows for laydown of large components during construction
    reactor_hall_w = 3.0D0 * key_width
    
    ! Length of reactor building    
    ! includes space for Fuel Cycle Outer Loop Facility
    reactor_hall_l = 3.0D0 * key_width
    
    ! Calculate vertical clearance required (above and below reactor):
    ! include clearance around reactor, transportation clearance between components,
    ! clearance from TF coil to cryostat, clearance beneath TF coil,
    ! clearance to ceiling for crane operation, crane arm height
    height_clrnc = reactor_clrnc + transp_clrnc &
                  + cryostat_clrnc + ground_clrnc &
                  + crane_clrnc_h + crane_arm_h

    ! Height of reactor building
    ! include height of TF coil *twice*, to allow for construction/maintenance
    reactor_hall_h = (2.0D0 * tf_vertical_dim) + height_clrnc             
   
    ! Heating and Current Drive facility
    ! Dimensions based upon estimates from M. Henderson, HCD Development Group
    ! iefrf = switch for current drive model
    if ( (iefrf == 5) .or. (iefrf == 8) ) then
      ! NBI technology will be situated within the reactor building
      reactor_hall_l = reactor_hall_l + 225.0D0 + reactor_clrnc + transp_clrnc
      reactor_hall_w = reactor_hall_w + 185.0D0 + reactor_clrnc + transp_clrnc
      ! Yes, it's really big. 
      hcd_building_area = 0.0D0     
      hcd_building_vol = 0.0D0
    else 
      ! Assume external building designed for EC or EBW is appropriate
      hcd_building_area = hcd_building_l * hcd_building_w
      hcd_building_vol = hcd_building_area * hcd_building_h
    end if

    ! Reactor building internal footprint and volume
    reactor_hall_area = reactor_hall_l * reactor_hall_w
    reactor_hall_vol = reactor_hall_area * reactor_hall_h

    ! Reactor building external footprint and volume
    reactor_hall_area_ext = (reactor_hall_l + 2.0D0*reactor_wall_thk) &
                           * (reactor_hall_w + 2.0D0*reactor_wall_thk)

    reactor_hall_vol_ext = reactor_hall_area_ext * &
                          (reactor_hall_h + reactor_roof_thk + reactor_fndtn_thk) 
    
    ! Reactor maintenance basement and tunnel
    ! Architecture proposed here is a basement directly beneath the reactor enabling the
    ! downwards extraction of hot components. The footprint estimated here is oversized to 
    ! include allowance for a tunnel to the hot cell storage/maintenance building.
    reactor_basement_l = reactor_hall_w
    reactor_basement_w = reactor_hall_w
    reactor_basement_area = reactor_basement_l * reactor_basement_w

    ! basement height still includes some clearances
    reactor_basement_h = tf_vertical_dim + transp_clrnc + crane_clrnc_h + crane_arm_h

    reactor_basement_vol = reactor_basement_area * reactor_basement_h
    
    ! Calculate external volume of reactor hall + maintenance basement and tunnel
    reactor_building_vol = reactor_hall_vol_ext + reactor_basement_vol

   
    ! Hot Cell Facility
    ! Provides hot cell facilities to maintain or dismantle highly radioactive components.
    ! These are simplifications of R. Gowland's estimates of Operational Active Waste Storage,
    ! which assumes all IVC components used through the life of the plant will need storage.
    ! The storage area required is derived from the sizes and number of components, allowing
    ! for a margin in component numbers as set by the quantity safety factor (qnty_sfty_fac).
    ! Footprints and volumes required for storage include hot separation distance (hot_sepdist).
    !
    ! assumptions: 
    ! tokomak is toroidally segmented based on number of TF coils (n_tf);
    ! component will be stored with the largest dimension oriented horizontally;
    ! height is the largest dimension    
    !
    ! Inboard 'component': shield, blanket, first wall: 
    ! find height, maximum radial dimension, maximum toroidal dimension
    comp_height = 2 * ( hmax - (tfcth + tftsgap + thshield + vgap2) )
    comp_rad_thk = shldith + blnkith + fwith
    comp_tor_thk = ( 2 * pi * (rmajor - (rminor + scrapli + fwith + blnkith + shldith)) ) &
                     / n_tf
    ! find footprint and volume for storing component
    comp_footprint = ( comp_height + hot_sepdist ) &
                     * ( max(comp_rad_thk,comp_tor_thk) + hot_sepdist )
    comp_vol = comp_footprint * ( min(comp_rad_thk,comp_tor_thk) + hot_sepdist )
    ! required lifetime supply of components = 
    !   ( number in build / (plant lifetime / component lifetime) ) * quantity safety factor
    comp_req_supply = ( n_tf / (tlife / bktlife) ) * qnty_sfty_fac
    ! total storage space for required supply of inboard shield-blanket-wall
    ib_hotcell_vol = comp_req_supply * comp_vol
    !
    ! Outboard 'component': first wall, blanket, shield
    comp_height = 2 * ( hmax - (tfcth + tftsgap + thshield + vgap2) )
    comp_rad_thk = fwoth + blnkoth + shldoth
    comp_tor_thk = ( 2 * pi * ( rmajor + rminor + scraplo + fwoth + blnkoth + shldoth ) ) &
                     / n_tf
    comp_footprint = ( comp_height + hot_sepdist ) &
                     * ( max(comp_rad_thk,comp_tor_thk) + hot_sepdist )
    comp_vol = comp_footprint * ( min(comp_rad_thk,comp_tor_thk) + hot_sepdist )
    comp_req_supply = ( n_tf / (tlife / bktlife) ) * qnty_sfty_fac
    ! total storage space for required supply of outboard wall-blanket-shield
    ob_hotcell_vol = comp_req_supply * comp_vol
    !
    ! Divertor
    ! Notes: 
    !  i) this estimation developed before the divertor design has been finalised
    !  ii) a 'corrected' divertor lifetime is used here (divlife_corr)
    comp_height = divfix
    comp_rad_thk = 2 * rminor
    comp_tor_thk = rmajor + rminor
    comp_footprint = ( comp_height + hot_sepdist ) &
                     * ( max(comp_rad_thk,comp_tor_thk) + hot_sepdist )
    comp_vol = comp_footprint * ( min(comp_rad_thk,comp_tor_thk) + hot_sepdist )
    divlife_corr = 2.0D0  ! based on DEMO studies
    comp_req_supply = ( n_tf / (tlife / divlife_corr) ) * qnty_sfty_fac
    ! total storage space for required supply of divertor segments
    div_hotcell_vol = comp_req_supply * comp_vol
    !
    ! Centre post
    if ( i_tf_sup /= 1 ) then
      comp_height = 2 * hmax
      comp_rad_thk = r_cp_top
      comp_footprint = ( comp_height + hot_sepdist ) * ( comp_rad_thk + hot_sepdist )
      comp_vol = comp_footprint * ( comp_rad_thk + hot_sepdist )
      comp_req_supply = ( n_tf / (tlife / cplife) ) * qnty_sfty_fac
      ! total storage space for required supply of centre posts
      cp_hotcell_vol = comp_req_supply * comp_vol
    else
      cp_hotcell_vol = 0.0D0
    end if
    !
    ! hot cell building footprint
    hotcell_facility_vol = ib_hotcell_vol + ob_hotcell_vol &
                           + div_hotcell_vol + cp_hotcell_vol
    ! building footprint derived from assumed building height
    hotcell_facility_area = hotcell_building_vol / hot_cell_facility_h
    

    hotcell_facility_area, hotcell_facility_vol

    


        ! number of hot cells, their size and functions can only sensibly be determined once the failure modes and recovery process (if any) of each IVC has been identified.
    



    ! hot_store_building_area = 0.0D0

        
    ! ! outboard first wall + shield + blanket
    
    ! ! number of components = num of modules in poloidal direction * num of modules in toroidal direction
    ! components_in_build = nblktmodpo * nblktmodto

    ! ! number of components required through lifetime of power plant, given an 
    ! ! estimated lifetime of the component; rounded up to an integer
    ! components_thru_life = ceiling((tlife / fwbllife) * components_in_build * qnty_sfty_fac)
    
    ! ! radial dimension of component = thickness
    ! dim1 = fwoth + blnkoth + shldoth
    ! ! vertical dimension of component = (2 * plasma half-height) / (num poloidal components)
    ! dim2 = 2.0D0 * (rminor * kappa) / nblktmodpo
    ! ! toroidal dimension of component = (circumference at radius of component) / (num toroidal components)
    ! dim3 = 2*pi*(rminor+rmajor+scraplo+fwoth+blnkoth+shldoth) / nblktmodto

    ! ! for storage, allow separation distance between parts
    ! dim1 = dim1 + sepdist
    ! dim2 = dim2 + sepdist
    ! dim3 = dim3 + sepdist

    ! ! find storage volume required per component, including separation distance
    ! compt_store_vol = dim1 * dim2 * dim3
    ! ! find storage volume required for total components through lifetime of plant
    ! compt_store_vol = comp_store_vol * components_thru_life

    
    ! hot_store_building_h = 12.0D0
    ! ! find footprint required, given set height of building (from RG's estimates)
    ! compt_store_area = compt_store_vol / hot_store_building_h

    ! ! if area was square, length and width would be:
    ! sq_len = sqrt(compt_store_area)

    ! ! length including separation to walls and hot cell walls; set width to match
    ! compt_store_l = sq_len + (2.0D0 * sepdist) + (2.0D0 * hotcell_wall_thk)
    ! compt_store_w = compt_store_l

    ! compt_store_area = compt_store_l * compt_store_w
    ! compt_store_vol = compt_store_area * hot_store_building_h
    
 






 
    
    ! Reactor Auxiliary Buildings
    ! Derived from W. Smith's estimates of necessary facilities and their sizes;
    ! these values amalgamate multiple individual buildings.
    !
    ! Chemistry labs: includes RA, non-RA and environmental labs, 
    ! and chemical treatment facilities for coolant circuits
    chemlab_area = chemlab_l * chemlab_w
    chemlab_vol = chemlab_area * chemlab_h
    !
    ! Heat sink facilities, includes aux heat sink at heat energy island,
    ! low temp and emergency heat sink facilities, ultimate heat sink facility 
    ! to sea/river/cooling towers, including pumping, chemical dosing and heat exchangers
    heat_sink_area = heat_sink_l * heat_sink_w
    heat_sink_vol = heat_sink_area * heat_sink_h
    !
    ! auxiliary buildings supporting tokamak processes & systems, includes non-RA
    ! interfacing services such as, hydraulics, compressed air, chilled water...
    aux_build_area = aux_build_l * aux_build_w
    aux_build_vol = aux_build_area * aux_build_h
    !
    ! Total auxiliary buildings supporting reactor processes & systems
    reactor_aux_area = chemlab_area + heat_sink_area + aux_build_area
    reactor_aux_vol = chemlab_vol + heat_sink_vol + aux_build_vol    
 

    ! Magnet power facilities
    ! Providing specific electrical supplies for reactor magnets;
    ! based upon dimensions of comparable equipment at ITER site.
    ! Steady state power trains:
    magnet_trains_area = magnet_trains_l * magnet_trains_w
    magnet_trains_vol = magnet_trains_area * magnet_trains_h
    ! Pulsed power for central solenoid
    magnet_pulse_area = magnet_pulse_l * magnet_pulse_w
    magnet_pulse_vol = magnet_pulse_area * magnet_pulse_h
    !
    ! Total power buildings areas and volumes
    power_buildings_area = hcd_building_area + magnet_trains_area + magnet_pulse_area
    power_buildings_vol = hcd_building_vol + magnet_trains_vol + magnet_pulse_vol


    ! Control    
    ! Derived from W. Smith's estimates of necessary facilities and their sizes:
    ! includes Main Control Room, Back-up Control Room, 
    ! Signal Processing and Distribution Centres [Safety Train A, Safety Train B], 
    ! HP offices & Data Logging centre, Data Storage centre;
    ! these values amalgamate multiple individual buildings.
    control_buildings_area = control_buildings_l * control_buildings_w
    control_buildings_vol = control_buildings_area * control_buildings_h


    ! Warm Shop
    ! Values taken from W. Smith's estimates of necessary facility size:
    ! 'hands on maintenance workshops for low RA dose equipment'
    warm_shop_area = warm_shop_l * warm_shop_w
    warm_shop_vol = warm_shop_area * warm_shop_h


    ! Maintenance
    ! Derived from W. Smith's estimates of necessary facilities and their sizes;
    ! these values amalgamate multiple individual buildings.
    !
    ! Maintenance workshops and clean rooms for components with *no* radiation 
    ! inventory; should include allowance for overhead gantry and crane access
    workshop_area = workshop_l * workshop_w
    workshop_vol = workshop_area * workshop_h
    !
    ! Robot construction, testing, mock-up facilities
    ! To allow robots to be fully assembled, commissioned and tested 
    ! in mock-ups of the real environment. Height should allow for mock-up of
    ! central column, but building also houses offices and classrooms. 
    robotics_area = robotics_l * robotics_w
    robotics_vol = robotics_area * robotics_h
    !
    ! Maintenance control and inspection facilities: includes operations centre,
    ! inbound inspection and QA storage facilities.
    maint_cont_area = maint_cont_l * maint_cont_w
    maint_cont_vol = maint_cont_area * maint_cont_h
    !
    maintenance_area = workshop_area + robotics_area + maint_cont_area
    maintenance_vol = workshop_vol + robotics_vol + maint_cont_vol


    ! Cryogenic & cooling facilities
    ! Derived from W. Smith's estimates of necessary facilities and their sizes.
    !
    ! Cryogenic Buildings for Magnet and Fuel Cycle
    cryomag_area = cryomag_l * cryomag_w
    cryomag_vol = cryomag_area * cryomag_h
    !
    ! Magnet Cryo Storage Tanks
    cryostore_area = cryostore_l * cryostore_w
    cryostore_vol = cryostore_area * cryostore_h
    !
    ! Site-Wide Auxiliary Cooling Water facility, including pumping, 
    ! chemical dosing, filtration and heat exchangers.
    auxcool_area = auxcool_l * auxcool_w
    auxcool_vol = auxcool_area * auxcool_h
    !
    cryocool_area = cryomag_area + cryostore_area + auxcool_area
    cryocool_vol = cryomag_vol + cryostore_vol + auxcool_vol

    
    ! Electrical
    ! Derived from W. Smith's estimates of necessary facilities and their sizes;
    ! these values amalgamate multiple individual buildings.
    !
    ! Transformers and electrical distribution facilities; includes
    ! main step down & step up transformers and substation, reactive power buildings,
    elecdist_area = elecdist_l * elecdist_w
    elecdist_vol = elecdist_area * elecdist_h
    !
    ! Load centres (essential and non-essential supplies)
    elecload_area = elecload_l * elecload_w
    elecload_vol = elecload_area * elecload_h
    !
    ! Energy Storage Systems (batteries & flywheels) and back-up generators
    elecstore_area = elecstore_l * elecstore_w
    elecstore_vol = elecstore_area * elecstore_h
    !
    elec_buildings_area = elecdist_area + elecload_area + elecstore_area
    elec_buildings_vol = elecdist_vol + elecload_vol + elecstore_vol

    
    ! Turbine Hall
    ! As proposed by R. Gowland, based on assessment of 18 existing fission power plants: 
    ! turbine hall size is largely independent of plant output power. 
    ! The default footprint used here represents a weighted mean of those plants 
    ! and the design of a Steam Rankine cycle turbine building suitable for STEP, 
    ! produced by Morsons as part of the Year 1 work.
    turbine_hall_area = turbine_hall_l * turbine_hall_w
    turbine_hall_vol = turbine_hall_area * turbine_hall_h


    ! Waste
    ! Derived from W. Smith's estimates of necessary facilities and their sizes.
    !
    ! Intermediate Level Waste
    ! Radioactive waste melt, separation and size reduction facility
    ilw_smelter_area = ilw_smelter_l * ilw_smelter_w
    ilw_smelter_vol = ilw_smelter_area * ilw_smelter_h
    ! ILW process and storage, amalgamated buildings
    ilw_storage_area = ilw_storage_l * ilw_storage_w
    ilw_storage_vol = ilw_storage_area * ilw_storage_h
    !
    ! Low Level Waste process and storage, amalgamated buildings
    llw_storage_area = llw_storage_l * llw_storage_w
    llw_storage_vol = llw_storage_area * llw_storage_h
    !
    ! Hazardous Waste process and storage, amalgamated buildings
    hw_storage_area = hw_storage_l * hw_storage_w
    hw_storage_vol = hw_storage_area * hw_storage_h
    !
    ! Tritiated Waste Store
    tw_storage_area = tw_storage_l * tw_storage_w
    tw_storage_vol = tw_storage_area * tw_storage_h
    !
    ! Total waste buildings areas and volumes
    waste_buildings_area = ilw_smelter_area + ilw_storage_area + &
      llw_storage_area + hw_storage_area + tw_storage_area
    waste_buildings_vol = ilw_smelter_vol + ilw_storage_vol + &
      llw_storage_vol + hw_storage_vol + tw_storage_vol


    ! Site Services    
    ! Derived from W. Smith's estimates of necessary facilities and their sizes; 
    ! buildings grouped by function.
    !
    ! Air & Gas supplies
    ! Includes compressed air facility, common gas systems facility, bottled gas 
    ! storage compounds; these values amalgamate multiple individual buildings.
    gas_buildings_area = gas_buildings_l * gas_buildings_w
    gas_buildings_vol = gas_buildings_area * gas_buildings_h
    !
    ! Water, Laundry & Drainage
    ! Includes facilities for potable water, firewater, chilled water; PPE laundry & 
    ! Respiratory Protective Equipment cleaning; industrial drains & sewage 
    ! process and discharge; these values amalgamate multiple individual buildings.
    water_buildings_area = water_buildings_l * water_buildings_w
    water_buildings_vol = water_buildings_area * water_buildings_h
    !
    ! Site Security & Safety
    ! Includes Security Control Centre and Fire and Ambulance Garages; 
    ! these values amalgamate multiple individual buildings.
    sec_buildings_area = sec_buildings_l * sec_buildings_w
    sec_buildings_vol = sec_buildings_area * sec_buildings_h


    ! Staff Services
    ! Derived from W. Smith's estimates of necessary facilities and their sizes; 
    ! includes main office buildings, contractor offices, staff restaurant and cafe,
    ! staff induction and training facilities, main gate and reception, access control
    ! and site pass office, occupational health centre.
    ! Amalgamates estimates of floor area for all individual buildings, uses average height.
    staff_buildings_vol = staff_buildings_area * staff_buildings_h
    

    ! ****************************** rmc rmc rmc 
!   ! Calculate effective floor area for ac power module
!   efloor = (rbv+rmbv+wsv+triv+elev+conv+cryv+admv+shov)/6.0D0
!   admvol = admv
!   shovol = shov
!   convol = conv

!   ! Total volume of nuclear buildings
!   volnucb = ( vrci + rmbv + wsv + triv + cryv )
    ! ******************************




    ! Output    
    if (iprint == 0) return
    call oheadr(outfile,'Plant Buildings System - RMC') ! rmc
    call ovarre(outfile,'reactor_hall_l (m)', '(reactor_hall_l)', reactor_hall_l)
    call ovarre(outfile,'reactor_hall_w (m)', '(reactor_hall_w)', reactor_hall_w)
    call ovarre(outfile,'reactor_hall_h (m)', '(reactor_hall_h)', reactor_hall_h)
    call ovarre(outfile,'Internal footprint of Reactor Hall (m2)', '(reactor_hall_area)', reactor_hall_area)
    call ovarre(outfile,'Internal volume of Reactor Hall (m3)', '(reactor_hall_vol)', reactor_hall_vol)
    call ovarre(outfile,'External footprint of Reactor Hall (m2)', '(reactor_hall_area_ext)', reactor_hall_area_ext)
    call ovarre(outfile,'External volume of Reactor Hall (m3)', '(reactor_hall_vol_ext)', reactor_hall_vol_ext)
    call ovarre(outfile,'Footprint of Reactor Basement (m2)', '(reactor_basement_area)', reactor_basement_area) ! RMC check
    call ovarre(outfile,'Volume of Reactor Basement (m3)', '(reactor_basement_vol)', reactor_basement_vol)
    call ovarre(outfile,'Volume of Reactor Hall + Basement (m3)', '(reactor_building_vol)', reactor_building_vol)
    if ( (iefrf == 5) .or. (iefrf == 8) ) then
      call ocmmnt(outfile,'NBI HCD facility included within reactor building')
    end if

    if (i_v_bldgs == 1) then
      ! verbose output of building sizes, areas and volumes
      call ovarre(outfile,'chemlab_l (m)', '(chemlab_l)', chemlab_l)
      call ovarre(outfile,'chemlab_w (m)', '(chemlab_w)', chemlab_w)
      call ovarre(outfile,'chemlab_h (m)', '(chemlab_h)', chemlab_h)
      call ovarre(outfile,'Footprint of chemistry labs and facilities (m2)', '(chemlab_area)', chemlab_area)
      call ovarre(outfile,'Volume of chemistry labs and facilities (m3)', '(chemlab_vol)', chemlab_vol)
      call ovarre(outfile,'aux_build_l (m)', '(aux_build_l)', aux_build_l)
      call ovarre(outfile,'aux_build_w (m)', '(aux_build_w)', aux_build_w)
      call ovarre(outfile,'aux_build_h (m)', '(aux_build_h)', aux_build_h)
      call ovarre(outfile,'Footprint of reactor-supporting buildings (m2)', '(aux_build_area)', aux_build_area)
      call ovarre(outfile,'Volume of reactor-supporting buildings (m3)', '(aux_build_vol)', aux_build_vol)
      call ovarre(outfile,'heat_sink_l (m)', '(heat_sink_l)', heat_sink_l)
      call ovarre(outfile,'heat_sink_w (m)', '(heat_sink_w)', heat_sink_w)
      call ovarre(outfile,'heat_sink_h (m)', '(heat_sink_h)', heat_sink_h)
      call ovarre(outfile,'Footprint of Heat Sinks (m2)', '(heat_sink_area)', heat_sink_area)
      call ovarre(outfile,'Volume of Heat Sinks (m2)', '(heat_sink_vol)', heat_sink_vol)
      call ovarre(outfile,'Footprint of reactor auxiliary buildings (m2)', '(reactor_aux_area)', reactor_aux_area)
      call ovarre(outfile,'Volume of reactor auxiliary buildings (m3)', '(reactor_aux_vol)', reactor_aux_vol)
      if ( (iefrf == 5) .or. (iefrf == 8) ) then
        call ocmmnt(outfile,'NBI HCD facility included within reactor building')
      else 
        call ovarre(outfile,'hcd_building_area (m2)', '(hcd_building_area)', hcd_building_area)
        call ovarre(outfile,'hcd_building_vol (m3)', '(hcd_building_vol)', hcd_building_vol)
      end if
      call ovarre(outfile,'magnet_trains_area (m2)', '(magnet_trains_area)', magnet_trains_area)
      call ovarre(outfile,'magnet_trains_vol (m3)', '(magnet_trains_vol)', magnet_trains_vol)
      call ovarre(outfile,'magnet_pulse_area (m2)', '(magnet_pulse_area)', magnet_pulse_area)
      call ovarre(outfile,'magnet_pulse_vol (m3)', '(magnet_pulse_vol)', magnet_pulse_vol)
      call ovarre(outfile,'power_buildings_area (m2)', '(power_buildings_area)', power_buildings_area)
      call ovarre(outfile,'power_buildings_vol (m3)', '(power_buildings_vol)', power_buildings_vol)
      
      call ovarre(outfile,'control_buildings_area (m2)', '(control_buildings_area)', control_buildings_area)
      call ovarre(outfile,'control_buildings_vol (m3)', '(control_buildings_vol)', control_buildings_vol)

      call ovarre(outfile,'warm_shop_l (m)', '(warm_shop_l)', warm_shop_l)
      call ovarre(outfile,'warm_shop_w (m)', '(warm_shop_w)', warm_shop_w)
      call ovarre(outfile,'warm_shop_h (m)', '(warm_shop_h)', warm_shop_h)
      call ovarre(outfile,'Footprint of Warm Shop (m2)', '(warm_shop_area)', warm_shop_area)
      call ovarre(outfile,'Volume of Warm Shop (m3)', '(warm_shop_vol)', warm_shop_vol)
      call ovarre(outfile,'workshop_l (m)', '(workshop_l)', workshop_l)
      call ovarre(outfile,'workshop_w (m)', '(workshop_w)', workshop_w)
      call ovarre(outfile,'workshop_h (m)', '(workshop_h)', workshop_h)
      call ovarre(outfile,'Footprint of Workshop (m2)', '(workshop_area)', workshop_area)
      call ovarre(outfile,'Volume of Workshop (m3)', '(workshop_vol)', workshop_vol)
      call ovarre(outfile,'robotics_l (m)', '(robotics_l)', robotics_l)
      call ovarre(outfile,'robotics_w (m)', '(robotics_w)', robotics_w)
      call ovarre(outfile,'robotics_h (m)', '(robotics_h)', robotics_h)
      call ovarre(outfile,'Footprint of Robotics building (m2)', '(robotics_area)', robotics_area)
      call ovarre(outfile,'Volume of Robotics building (m3)', '(robotics_vol)', robotics_vol)
      call ovarre(outfile,'maint_cont_l (m)', '(maint_cont_l)', maint_cont_l)
      call ovarre(outfile,'maint_cont_w (m)', '(maint_cont_w)', maint_cont_w)
      call ovarre(outfile,'maint_cont_h (m)', '(maint_cont_h)', maint_cont_h)
      call ovarre(outfile,'Footprint of Maint. Cont. buildings (m2)', '(maint_cont_area)', maint_cont_area)
      call ovarre(outfile,'Volume of Maint. Cont. buildings (m3)', '(maint_cont_vol)', maint_cont_vol)
      call ovarre(outfile,'Footprint of Maintenance buildings (m2)', '(maintenance_area)', maintenance_area)
      call ovarre(outfile,'Volume of Maintenance buildings (m3)', '(maintenance_vol)', maintenance_vol)

      call ovarre(outfile,'cryomag_l (m)', '(cryomag_l)', cryomag_l)
      call ovarre(outfile,'cryomag_w (m)', '(cryomag_w)', cryomag_w)
      call ovarre(outfile,'cryomag_h (m)', '(cryomag_h)', cryomag_h)
      call ovarre(outfile,'Footprint of Cryogenic Buildings (m2)', '(cryomag_area)', cryomag_area)
      call ovarre(outfile,'Volume of Cryogenic Buildings (m3)', '(cryomag_vol)', cryomag_vol)
      call ovarre(outfile,'cryostore_l (m)', '(cryostore_l)', cryostore_l)
      call ovarre(outfile,'cryostore_w (m)', '(cryostore_w)', cryostore_w)
      call ovarre(outfile,'cryostore_h (m)', '(cryostore_h)', cryostore_h)  
      call ovarre(outfile,'Footprint of Magnet Cryo Storage Tanks (m2)', '(cryostore_area)', cryostore_area)
      call ovarre(outfile,'Volume of Magnet Cryo Storage Tanks (m3)', '(cryostore_vol)', cryostore_vol)
      call ovarre(outfile,'auxcool_l (m)', '(auxcool_l)', auxcool_l)
      call ovarre(outfile,'auxcool_w (m)', '(auxcool_w)', auxcool_w)
      call ovarre(outfile,'auxcool_h (m)', '(auxcool_h)', auxcool_h)
      call ovarre(outfile,'Footprint of Auxiliary Cooling buildings (m2)', '(auxcool_area)', auxcool_area)
      call ovarre(outfile,'Volume of Auxiliary Cooling buildings (m3)', '(auxcool_vol)', auxcool_vol)
      call ovarre(outfile,'Footprint of Cryogenic & cooling buildings (m2)', '(cryocool_area)', cryocool_area)
      call ovarre(outfile,'Volume of Cryogenic & cooling building (m3)', '(cryocool_vol)', cryocool_vol)

      call ovarre(outfile,'elecdist_l (m)', '(elecdist_l)', elecdist_l)
      call ovarre(outfile,'elecdist_w (m)', '(elecdist_w)', elecdist_w)
      call ovarre(outfile,'elecdist_h (m)', '(elecdist_h)', elecdist_h)
      call ovarre(outfile,'Footprint of transformers (m2)', '(elecdist_area)', elecdist_area)
      call ovarre(outfile,'Volume of transformers (m3)', '(elecdist_vol)', elecdist_vol)
      call ovarre(outfile,'elecload_l (m)', '(elecload_l)', elecload_l)
      call ovarre(outfile,'elecload_w (m)', '(elecload_w)', elecload_w)
      call ovarre(outfile,'elecload_h (m)', '(elecload_h)', elecload_h)
      call ovarre(outfile,'Footprint of electrical load centres (m2)', '(elecload_area)', elecload_area)
      call ovarre(outfile,'Volume of electrical load centres (m3)', '(elecload_vol)', elecload_vol)     
      call ovarre(outfile,'elecstore_l (m)', '(elecstore_l)', elecstore_l)
      call ovarre(outfile,'elecstore_w (m)', '(elecstore_w)', elecstore_w)
      call ovarre(outfile,'elecstore_h (m)', '(elecstore_h)', elecstore_h)
      call ovarre(outfile,'Footprint of Energy Storage Systems (m2)', '(elecstore_area)', elecstore_area)
      call ovarre(outfile,'Volume of Energy Storage Systems (m3)', '(elecstore_vol)', elecstore_vol)
      call ovarre(outfile,'Footprint of electrical buildings (m2)', '(elec_buildings_area)', elec_buildings_area)
      call ovarre(outfile,'Volume of electrical buildings (m3)', '(elec_buildings_vol)', elec_buildings_vol)

      call ovarre(outfile,'turbine_hall_l (m)', '(turbine_hall_l)', turbine_hall_l)
      call ovarre(outfile,'turbine_hall_w (m)', '(turbine_hall_w)', turbine_hall_w)
      call ovarre(outfile,'turbine_hall_h (m)', '(turbine_hall_h)', turbine_hall_h)
      call ovarre(outfile,'Footprint of Turbine Hall (m2)', '(turbine_hall_area)', turbine_hall_area)
      call ovarre(outfile,'Volume of Turbine Hall (m3)', '(turbine_hall_vol)', turbine_hall_vol)

      call ovarre(outfile,'Footprint of waste buildings (m2)', '(waste_buildings_area)', waste_buildings_area)
      call ovarre(outfile,'Volume of waste buildings (m3)', '(waste_buildings_vol)', waste_buildings_vol)

      call ovarre(outfile,'Footprint of air & gas supply buildings (m2)', '(gas_buildings_area)', gas_buildings_area)
      call ovarre(outfile,'Volume of air & gas supply buildings (m3)', '(gas_buildings_vol)', gas_buildings_vol)
      call ovarre(outfile,'Footprint of water supply buildings (m2)', '(water_buildings_area)', water_buildings_area)
      call ovarre(outfile,'Volume of water supply buildings (m3)', '(water_buildings_vol)', water_buildings_vol)
      call ovarre(outfile,'Footprint of Security & Safety buildings (m2)', '(sec_buildings_area)', sec_buildings_area)
      call ovarre(outfile,'Volume of Security & Safety buildings (m3)', '(sec_buildings_vol)', sec_buildings_vol)
      call ovarre(outfile,'height of staff buildings (m)', '(staff_buildings_h)', staff_buildings_h)
      call ovarre(outfile,'Footprint of staff buildings (m2)', '(staff_buildings_area)', staff_buildings_area)
      call ovarre(outfile,'Volume of staff buildings (m3)', '(staff_buildings_vol)', staff_buildings_vol)

    end if 

    !call ovarre(outfile,' (m)', '()', )
    !call ovarre(outfile,'Footprint of  buildings (m2)', '(_buildings_area)', _buildings_area)
    !call ovarre(outfile,'Volume of  buildings (m3)', '(_buildings_vol)', _buildings_vol)

  end subroutine bldgs_sizes

end module buildings_module
