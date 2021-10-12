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
  
    real(8) :: clh1
    !! vertical clearance from TF coil to cryostat (m) (calculated for tokamaks)
  
    real(8) :: clh2
    !! clearance beneath TF coil to foundation (including basement) (m)
  
    real(8) :: conv
    !! control building volume (m3)
  
    real(8) :: convol
    !! volume of control, protection and i&c building (m3)
  
    real(8) :: cryvol
    !! volume of cryoplant building (m3)
  
    real(8) :: efloor
    !! effective total floor space (m2)
  
    real(8) :: elevol
    !! volume of electrical equipment building (m3)
  
    real(8) :: esbldgm3
    !! volume of energy storage equipment building (m3) (not used if `lpulse=0`)
  
    real(8) :: fndt
    !! foundation thickness (m)
  
    real(8) :: hccl
    !! clearance around components in hot cell (m)
  
    real(8) :: hcwt
    !! hot cell wall thickness (m)
  
    real(8) :: mbvfac
    !! maintenance building volume multiplication factor
  
    real(8) :: pfbldgm3
    !! volume of PF coil power supply building (m3)
  
    real(8) :: pibv
    !! power injection building volume (m3)
  
    real(8) :: rbrt
    !! reactor building roof thickness (m)
  
    real(8) :: rbvfac
    !! reactor building volume multiplication factor
  
    real(8) :: rbvol
    !! reactor building volume (m3)
  
    real(8) :: rbwt
    !! reactor building wall thickness (m)
  
    real(8) :: rmbvol
    !! volume of maintenance and assembly building (m3)
  
    real(8) :: row
    !! clearance to building wall for crane operation (m)
  
    real(8) :: rxcl
    !! clearance around reactor (m)
  
    real(8) :: shmf
    !! fraction of shield mass per TF coil to be moved in the maximum shield lift
  
    real(8) :: shov
    !! shops and warehouse volume (m3)
  
    real(8) :: shovol
    !! volume of shops and buildings for plant auxiliaries (m3)
  
    real(8) :: stcl
    !! clearance above crane to roof (m)
  
    real(8) :: tfcbv
    !! volume of TF coil power supply building (m3) (calculated if TF coils are superconducting)
  
    real(8) :: trcl
    !! transportation clearance between components (m)
  
    real(8) :: triv
    !! volume of tritium, fuel handling and health physics buildings (m3)
  
    real(8) :: volnucb
    !! sum of nuclear buildings volumes (m3)
  
    real(8) :: volrci
    !! internal volume of reactor building (m3)
  
    real(8) :: wgt
    !! reactor building crane capacity (kg) (calculated if 0 is input)
  
    real(8) :: wgt2
    !! hot cell crane capacity (kg) (calculated if 0 is input)
  
    real(8) :: wrbi
    !! distance from centre of machine to building wall (m), i.e. reactor building half-width
  
    real(8) :: wsvfac
    !! warm shop building volume multiplication factor
  
    real(8) :: wsvol
    !! volume of warm shop building (m3)

    real(8) :: a_reactor_bldg
    !! Floor area of reactor building in m^2

    real(8) :: a_ee_ps_bldg
    !! Floor area of electrical equipment and power supply building in m^2

    real(8) :: a_aux_services_bldg
    !! Floor area of auxiliary services building in m^2

    real(8) :: a_hot_cell_bldg
    !! Floor area of hot cell building in m^2

    real(8) :: a_reactor_service_bldg
    !! Floor area of reactor service building in m^2

    real(8) :: a_service_water_bldg
    !! Floor area of service water building in m^2

    real(8) :: a_fuel_handling_bldg
    !! Floor area of fuel handling and storage building in m^2

    real(8) :: a_control_room_bldg
    !! Floor area of controlroom building in m^2

    real(8) :: a_ac_ps_bldg
    !! Floor area of AC power supply building in m^2

    real(8) :: a_admin_bldg
    !! Floor area of admin building in m^2

    real(8) :: a_site_service_bldg
    !! Floor area of site service building in m^2

    real(8) :: a_cryo_inert_gas_bldg
    !! Floor area of cryogenics and inert gas storage building in m^2

    real(8) :: a_security_bldg
    !! Floor area of security building in m^2

    contains

    subroutine init_buildings_variables
      !! Initialise buildings variables
      implicit none

      admv = 1.0D5
      admvol = 0.0D0
      clh1 = 2.5D0
      clh2 = 15.0D0
      conv = 6.0D4
      convol = 0.0D0
      cryvol = 0.0D0
      efloor = 0.0D0
      elevol = 0.0D0
      esbldgm3 = 1.0D3
      fndt = 2.0D0
      hccl = 5.0D0
      hcwt = 1.5D0
      mbvfac = 2.8D0
      pfbldgm3 = 2.0D4
      pibv = 2.0D4
      rbrt = 1.0D0
      rbvfac = 1.6D0
      rbvol = 0.0D0
      rbwt = 2.0D0
      rmbvol = 0.0D0
      row = 4.0D0
      rxcl = 4.0D0
      shmf = 0.5D0
      shov = 1.0D5
      shovol = 0.0D0
      stcl = 3.0D0
      tfcbv = 2.0D4
      trcl = 1.0D0
      triv = 4.0D4
      volnucb = 0.0D0
      volrci = 0.0D0
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
    end subroutine init_buildings_variables
  end module buildings_variables