module buildings_variables
    !! author: J. Morris (UKAEA)
    !!
    !! Module containing global variables relating to the plant buildings
    !!
    !!### References
    !!
    !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code
  
    use, intrinsic :: iso_fortran_env, only: dp=>real64
  
    implicit none
  
    public
  
    real(8), parameter :: a_default_bldg = 1.0D3
    !! Default building floor area in m^2

    real(8) :: admv
    !! administration building volume (m3)
  
    real(8) :: admvol
    !! volume of administration buildings (m3)

    real(8) :: auxcool_l, auxcool_w, auxcool_h
    !! Site-Wide Auxiliary Cooling Water facility length, width, height (m)
    
    real(8) :: clh1
    !! vertical clearance from TF coil to cryostat (m) (calculated for tokamaks)
  
    real(8) :: clh2
    !! clearance beneath TF coil to foundation (including basement) (m)
  
    real(8) :: conv
    !! control building volume (m3)
  
    real(8) :: convol
    !! volume of control, protection and i&c building (m3)

    real(8) :: control_buildings_l, control_buildings_w, control_buildings_h
    !! control building length, width, height (m)

    real(8) :: crane_arm_h
    !! vertical dimension of crane arm, operating over reactor (m)
  
    real(8) :: cryvol
    !! volume of cryoplant building (m3)

    real(8) :: cryostat_clrnc
    !! vertical clearance from TF coil to cryostat (m)
  
    real(8) :: cryomag_l, cryomag_w, cryomag_h
    !! Cryogenic Buildings for Magnet and Fuel Cycle length, width, height (m)

    real(8) :: cryostore_l, cryostore_w, cryostore_h
    !! Magnet Cryo Storage Tanks length, width, height (m)  

    real(8) :: efloor
    !! effective total floor space (m2)
  
    real(8) :: elecdist_l, elecdist_w, elecdist_h
    !! Transformers and electrical distribution facilities length, width, height (m)

    real(8) :: elecload_l, elecload_w, elecload_h
    !! Electric (eesential and non-essential) load centres length, width, height (m)

    real(8) :: elecstore_l, elecstore_w, elecstore_h
    !! Energy Storage facilities length, width, height (m)

    real(8) :: elevol
    !! volume of electrical equipment building (m3)
  
    real(8) :: esbldgm3
    !! volume of energy storage equipment building (m3) (not used if `lpulse=0`)
  
    real(8) :: fndt
    !! foundation thickness (m) - RMC remove?!

    real(8) :: gas_buildings_l, gas_buildings_w, gas_buildings_h
    !! air & gas supply (amalgamated) buildings length, width, height (m)

    real(8) :: ground_clrnc
    !! clearance beneath TF coil (m)

    real(8) :: hw_storage_l, hw_storage_w, hw_storage_h
    !! hazardous waste storage building length, width, height (m)
  
    real(8) :: hcd_building_l, hcd_building_w, hcd_building_h
    !! HCD building length, width, height (m)

    real(8) :: hccl
    !! clearance around components in hot cell (m)
  
    real(8) :: hcwt
    !! hot cell wall thickness (m)

    real(8) :: ilw_smelter_l, ilw_smelter_w, ilw_smelter_h
    !! radioactive waste smelting facility length, width, height (m)
    
    real(8) :: ilw_storage_l, ilw_storage_w, ilw_storage_h
    !! ILW waste storage building length, width, height (m)

    real(8) :: llw_storage_l, llw_storage_w, llw_storage_h
    !! LLW waste storage building length, width, height (m)
  
    integer :: i_v_bldgs
    !! switch to turn verbose output for buildings

    real(8) :: mbvfac
    !! maintenance building volume multiplication factor

    real(8) :: magnet_pulse_l, magnet_pulse_w, magnet_pulse_h
    !! pulsed magnet power building length, width, height (m)

    real(8) :: magnet_trains_l, magnet_trains_w, magnet_trains_h
    !! steady state magnet power trains building length, width, height (m)

    real(8) :: maint_cont_l, maint_cont_w, maint_cont_h
    !! maintenance control building length, width, height (m)

    real(8) :: pfbldgm3
    !! volume of PF coil power supply building (m3)
  
    real(8) :: pibv
    !! power injection building volume (m3)
  
    real(8) :: rbrt
    !! reactor building roof thickness (m) - RMC remove?!
  
    real(8) :: rbvfac
    !! reactor building volume multiplication factor
  
    real(8) :: rbvol
    !! reactor building volume (m3)
  
    real(8) :: rbwt
    !! reactor building wall thickness (m) - RMC remove?!

    real(8) :: reactor_hall_l, reactor_hall_w, reactor_hall_h
    !! reactor building length, width, height (m)
  
    real(8) :: reactor_wall_thk 
    !! reactor building wall thickness (m)

    real(8) :: reactor_roof_thk
    !! reactor building roof thickness (m)

    real(8) :: reactor_fndtn_thk
    !! reactor building foundation thickness (m)

    real(8) :: rmbvol
    !! volume of maintenance and assembly building (m3)
  
    real(8) :: row
    !! clearance to building wall for crane operation (m) - RMC remove?!
  
    real(8) :: rxcl
    !! clearance around reactor (m) - RMC remove?!

    real(8) :: reactor_clrnc
    !! clearance around reactor (m)

    real(8) :: robotics_l, robotics_w, robotics_h
    !! robotics buildings length, width, height (m)

    real(8) :: transp_clrnc
    !! transportation clearance between components (m)

    real(8) :: crane_clrnc_h
    !! horizontal clearance to building wall for crane operation (m)

    real(8) :: crane_clrnc_v
    !! vertical clearance for crane operation (m)

    real(8) :: sec_buildings_l, sec_buildings_w, sec_buildings_h
    !! security & safety buildings length, width, height (m)

    real(8) :: staff_buildings_h
    !! staff buildings height (m)

    real(8) :: staff_buildings_area 
    !! footprint of staff buildings (m2)
  
    real(8) :: shmf
    !! fraction of shield mass per TF coil to be moved in the maximum shield lift
  
    real(8) :: shov
    !! shops and warehouse volume (m3)
  
    real(8) :: shovol
    !! volume of shops and buildings for plant auxiliaries (m3)
  
    real(8) :: stcl
    !! clearance above crane to roof (m)  - RMC remove?!
  
    real(8) :: tfcbv
    !! volume of TF coil power supply building (m3) (calculated if TF coils are superconducting)
  
    real(8) :: trcl
    !! transportation clearance between components (m) - RMC remove?!
  
    real(8) :: triv
    !! volume of tritium, fuel handling and health physics buildings (m3)
  
    real(8) :: turbine_hall_l, turbine_hall_w, turbine_hall_h
    !! turbine hall length, width, height (m)

    real(8) :: tw_storage_l, tw_storage_w, tw_storage_h
    !! tritiated waste storage building length, width, height (m)

    real(8) :: volnucb
    !! sum of nuclear buildings volumes (m3)
  
    real(8) :: volrci
    !! internal volume of reactor building (m3)

    real(8) :: warm_shop_l, warm_shop_w, warm_shop_h 
    !! warm shop length, width, height (m)

    real(8) :: water_buildings_l, water_buildings_w, water_buildings_h
    !! water, laundry & drainage buildings length, width, height (m)

    real(8) :: workshop_l, workshop_w, workshop_h
    !! [cold] workshop buildings length, width, height (m)
  
    real(8) :: wgt
    !! reactor building crane capacity (kg) (calculated if 0 is input)
  
    real(8) :: wgt2
    !! hot cell crane capacity (kg) (calculated if 0 is input)
  
    real(8) :: wrbi
    !! distance from centre of machine to building wall (m), i.e. reactor building half-width  - RMC remove?!
  
    real(8) :: wsvfac
    !! warm shop building volume multiplication factor
  
    real(8) :: wsvol
    !! volume of warm shop building (m3)

    real(8) :: a_reactor_bldg
    !! Floor area of reactor building in m^2 - RMC remove?!

    real(8) :: a_ee_ps_bldg
    !! Floor area of electrical equipment and power supply building in m^2 - RMC remove?!

    real(8) :: a_aux_services_bldg
    !! Floor area of auxiliary services building in m^2 - RMC remove?!

    real(8) :: a_hot_cell_bldg
    !! Floor area of hot cell building in m^2 - RMC remove?!

    real(8) :: a_reactor_service_bldg
    !! Floor area of reactor service building in m^2 - RMC remove?!

    real(8) :: a_service_water_bldg
    !! Floor area of service water building in m^2 - RMC remove?!

    real(8) :: a_fuel_handling_bldg
    !! Floor area of fuel handling and storage building in m^2 - RMC remove?!

    real(8) :: a_control_room_bldg
    !! Floor area of controlroom building in m^2 - RMC remove?!

    real(8) :: a_ac_ps_bldg
    !! Floor area of AC power supply building in m^2 - RMC remove?!

    real(8) :: a_admin_bldg
    !! Floor area of admin building in m^2 - RMC remove?!

    real(8) :: a_site_service_bldg
    !! Floor area of site service building in m^2 - RMC remove?!

    real(8) :: a_cryo_inert_gas_bldg
    !! Floor area of cryogenics and inert gas storage building in m^2 - RMC remove?!

    real(8) :: a_security_bldg
    !! Floor area of security building in m^2 - RMC remove?!

    contains

    subroutine init_buildings_variables
      !! Initialise buildings variables
      implicit none

      admv = 1.0D5
      admvol = 0.0D0     
      auxcool_l = 20.0D0
      auxcool_w = 10.0D0
      auxcool_h = 5.0D0
      clh1 = 2.5D0
      clh2 = 15.0D0
      conv = 6.0D4
      convol = 0.0D0
      control_buildings_l = 80.0D0
      control_buildings_w = 60.0D0
      control_buildings_h = 6.0D0
      crane_arm_h = 10.0D0
      cryomag_l = 120.0D0
      cryomag_w = 90.0D0
      cryomag_h = 5.0D0
      cryostore_l = 160.0D0
      cryostore_w = 25.0D0
      cryostore_h = 20.0D0
      cryostat_clrnc = 2.5D0
      cryvol = 0.0D0
      efloor = 0.0D0
      elecdist_l = 400.0D0
      elecdist_w = 350.0D0
      elecdist_h = 5.0D0
      elecload_l = 100.0D0
      elecload_w = 90.0D0
      elecload_h = 3.0D0
      elecstore_l = 100.0D0
      elecstore_w = 60.0D0
      elecstore_h = 12.0D0
      elevol = 0.0D0
      esbldgm3 = 1.0D3
      fndt = 2.0D0
      gas_buildings_l = 25.0D0
      gas_buildings_w = 15.0D0
      gas_buildings_h = 5.0D0
      hcd_building_l = 70.0D0
      hcd_building_w = 40.0D0
      hcd_building_h = 25.0D0
      hw_storage_l = 20.0D0
      hw_storage_w = 10.0D0
      hw_storage_h = 5.0D0
      ground_clrnc = 5.0D0
      hccl = 5.0D0
      hcwt = 1.5D0
      ilw_smelter_l = 50.0D0
      ilw_smelter_w = 30.0D0
      ilw_smelter_h = 30.0D0
      ilw_storage_l = 60.0D0
      ilw_storage_w = 50.0D0
      ilw_storage_h = 5.0D0
      llw_storage_l = 45.0D0
      llw_storage_w = 20.0D0
      llw_storage_h = 5.0D0
      i_v_bldgs = 0
      mbvfac = 2.8D0
      magnet_pulse_l = 105.0D0
      magnet_pulse_w = 40.0D0
      magnet_pulse_h = 5.0D0
      magnet_trains_l = 120.0D0
      magnet_trains_w = 90.0D0
      magnet_trains_h = 5.0D0
      maint_cont_l = 100.0D0
      maint_cont_w = 100.0D0
      maint_cont_h = 6.0D0
      pfbldgm3 = 2.0D4
      pibv = 2.0D4
      rbrt = 1.0D0
      rbvfac = 1.6D0
      rbvol = 0.0D0
      rbwt = 2.0D0
      reactor_hall_l = 0.0D0
      reactor_hall_w = 0.0D0
      reactor_hall_h = 0.0D0
      reactor_wall_thk = 2.0D0
      reactor_roof_thk = 1.0D0
      reactor_fndtn_thk = 2.0D0
      rmbvol = 0.0D0
      robotics_l = 50.0D0
      robotics_w = 30.0D0
      robotics_h = 30.0D0
      row = 4.0D0
      rxcl = 4.0D0
      sec_buildings_l = 30.0D0
      sec_buildings_w = 25.0D0
      sec_buildings_h = 6.0D0
      staff_buildings_h = 5.0D0
      staff_buildings_area = 5.0D5
      shmf = 0.5D0
      shov = 1.0D5
      shovol = 0.0D0
      stcl = 3.0D0
      tfcbv = 2.0D4
      trcl = 1.0D0
      triv = 4.0D4
      turbine_hall_l = 109.0D0
      turbine_hall_w = 62.0D0
      turbine_hall_h = 15.0D0
      tw_storage_l = 90.0D0
      tw_storage_w = 30.0D0
      tw_storage_h = 5.0D0
      volnucb = 0.0D0
      volrci = 0.0D0
      warm_shop_l = 100.0D0
      warm_shop_w = 50.0D0
      warm_shop_h = 10.0D0
      water_buildings_l = 110.0D0
      water_buildings_w = 10.0D0
      water_buildings_h = 5.0D0
      workshop_l = 150.0D0
      workshop_w = 120.0D0
      workshop_h = 10.0D0
      wgt = 5.0D5
      wgt2 = 1.0D5
      wrbi = 0.0D0
      wsvfac = 1.9D0
      wsvol = 0.0D0
      a_reactor_bldg = 8.32D3
      a_ee_ps_bldg = 2.133D4
      a_aux_services_bldg = a_default_bldg
      a_hot_cell_bldg = 8.43D3
      a_reactor_service_bldg = 2.44D3
      a_service_water_bldg = 1.567D3
      a_fuel_handling_bldg = 1.67D3
      a_control_room_bldg = 2.88D3
      a_ac_ps_bldg = 6.423D3
      a_admin_bldg = 2.5674D4
      a_site_service_bldg = 8.3D3
      a_cryo_inert_gas_bldg = 1.838D4
      a_security_bldg = 4.552D3
      
      reactor_clrnc = 4.0D0
      transp_clrnc = 1.0D0
      crane_clrnc_h = 4.0D0
      crane_clrnc_v = 3.0D0
      


    end subroutine init_buildings_variables
  end module buildings_variables