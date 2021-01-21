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
  
    real(dp) :: admv
    !! administration building volume (m3)
  
    real(dp) :: admvol
    !! volume of administration buildings (m3)
  
    real(dp) :: clh1
    !! vertical clearance from TF coil to cryostat (m) (calculated for tokamaks)
  
    real(dp) :: clh2
    !! clearance beneath TF coil to foundation (including basement) (m)
  
    real(dp) :: conv
    !! control building volume (m3)
  
    real(dp) :: convol
    !! volume of control, protection and i&c building (m3)
  
    real(dp) :: cryvol
    !! volume of cryoplant building (m3)
  
    real(dp) :: efloor
    !! effective total floor space (m2)
  
    real(dp) :: elevol
    !! volume of electrical equipment building (m3)
  
    real(dp) :: esbldgm3
    !! volume of energy storage equipment building (m3) (not used if `lpulse=0`)
  
    real(dp) :: fndt
    !! foundation thickness (m)
  
    real(dp) :: hccl
    !! clearance around components in hot cell (m)
  
    real(dp) :: hcwt
    !! hot cell wall thickness (m)
  
    real(dp) :: mbvfac
    !! maintenance building volume multiplication factor
  
    real(dp) :: pfbldgm3
    !! volume of PF coil power supply building (m3)
  
    real(dp) :: pibv
    !! power injection building volume (m3)
  
    real(dp) :: rbrt
    !! reactor building roof thickness (m)
  
    real(dp) :: rbvfac
    !! reactor building volume multiplication factor
  
    real(dp) :: rbvol
    !! reactor building volume (m3)
  
    real(dp) :: rbwt
    !! reactor building wall thickness (m)
  
    real(dp) :: rmbvol
    !! volume of maintenance and assembly building (m3)
  
    real(dp) :: row
    !! clearance to building wall for crane operation (m)
  
    real(dp) :: rxcl
    !! clearance around reactor (m)
  
    real(dp) :: shmf
    !! fraction of shield mass per TF coil to be moved in the maximum shield lift
  
    real(dp) :: shov
    !! shops and warehouse volume (m3)
  
    real(dp) :: shovol
    !! volume of shops and buildings for plant auxiliaries (m3)
  
    real(dp) :: stcl
    !! clearance above crane to roof (m)
  
    real(dp) :: tfcbv
    !! volume of TF coil power supply building (m3) (calculated if TF coils are superconducting)
  
    real(dp) :: trcl
    !! transportation clearance between components (m)
  
    real(dp) :: triv
    !! volume of tritium, fuel handling and health physics buildings (m3)
  
    real(dp) :: volnucb
    !! sum of nuclear buildings volumes (m3)
  
    real(dp) :: volrci
    !! internal volume of reactor building (m3)
  
    real(dp) :: wgt
    !! reactor building crane capacity (kg) (calculated if 0 is input)
  
    real(dp) :: wgt2
    !! hot cell crane capacity (kg) (calculated if 0 is input)
  
    real(dp) :: wrbi
    !! distance from centre of machine to building wall (m), i.e. reactor building half-width
  
    real(dp) :: wsvfac
    !! warm shop building volume multiplication factor
  
    real(dp) :: wsvol
    !! volume of warm shop building (m3)
  
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
    end subroutine init_buildings_variables

  end module buildings_variables