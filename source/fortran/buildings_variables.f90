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
  
    real(dp) :: admv = 1.0D5
    !! administration building volume (m3)
  
    real(dp) :: admvol = 0.0D0
    !! volume of administration buildings (m3)
  
    real(dp) :: clh1 = 2.5D0
    !! vertical clearance from TF coil to cryostat (m) (calculated for tokamaks)
  
    real(dp) :: clh2 = 15.0D0
    !! clearance beneath TF coil to foundation (including basement) (m)
  
    real(dp) :: conv = 6.0D4
    !! control building volume (m3)
  
    real(dp) :: convol = 0.0D0
    !! volume of control, protection and i&c building (m3)
  
    real(dp) :: cryvol = 0.0D0
    !! volume of cryoplant building (m3)
  
    real(dp) :: efloor = 0.0D0
    !! effective total floor space (m2)
  
    real(dp) :: elevol = 0.0D0
    !! volume of electrical equipment building (m3)
  
    real(dp) :: esbldgm3 = 1.0D3
    !! volume of energy storage equipment building (m3) (not used if `lpulse=0`)
  
    real(dp) :: fndt = 2.0D0
    !! foundation thickness (m)
  
    real(dp) :: hccl = 5.0D0
    !! clearance around components in hot cell (m)
  
    real(dp) :: hcwt = 1.5D0
    !! hot cell wall thickness (m)
  
    real(dp) :: mbvfac = 2.8D0
    !! maintenance building volume multiplication factor
  
    real(dp) :: pfbldgm3 = 2.0D4
    !! volume of PF coil power supply building (m3)
  
    real(dp) :: pibv = 2.0D4
    !! power injection building volume (m3)
  
    real(dp) :: rbrt = 1.0D0
    !! reactor building roof thickness (m)
  
    real(dp) :: rbvfac = 1.6D0
    !! reactor building volume multiplication factor
  
    real(dp) :: rbvol = 0.0D0
    !! reactor building volume (m3)
  
    real(dp) :: rbwt = 2.0D0
    !! reactor building wall thickness (m)
  
    real(dp) :: rmbvol = 0.0D0
    !! volume of maintenance and assembly building (m3)
  
    real(dp) :: row = 4.0D0
    !! clearance to building wall for crane operation (m)
  
    real(dp) :: rxcl = 4.0D0
    !! clearance around reactor (m)
  
    real(dp) :: shmf = 0.5D0
    !! fraction of shield mass per TF coil to be moved in the maximum shield lift
  
    real(dp) :: shov = 1.0D5
    !! shops and warehouse volume (m3)
  
    real(dp) :: shovol = 0.0D0
    !! volume of shops and buildings for plant auxiliaries (m3)
  
    real(dp) :: stcl = 3.0D0
    !! clearance above crane to roof (m)
  
    real(dp) :: tfcbv = 2.0D4
    !! volume of TF coil power supply building (m3) (calculated if TF coils are superconducting)
  
    real(dp) :: trcl = 1.0D0
    !! transportation clearance between components (m)
  
    real(dp) :: triv = 4.0D4
    !! volume of tritium, fuel handling and health physics buildings (m3)
  
    real(dp) :: volnucb = 0.0D0
    !! sum of nuclear buildings volumes (m3)
  
    real(dp) :: volrci = 0.0D0
    !! internal volume of reactor building (m3)
  
    real(dp) :: wgt = 5.0D5
    !! reactor building crane capacity (kg) (calculated if 0 is input)
  
    real(dp) :: wgt2 = 1.0D5
    !! hot cell crane capacity (kg) (calculated if 0 is input)
  
    real(dp) :: wrbi = 0.0D0
    !! distance from centre of machine to building wall (m), i.e. reactor building half-width
  
    real(dp) :: wsvfac = 1.9D0
    !! warm shop building volume multiplication factor
  
    real(dp) :: wsvol = 0.0D0
    !! volume of warm shop building (m3)
  
  end module buildings_variables