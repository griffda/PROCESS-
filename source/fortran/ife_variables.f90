module ife_variables
    !! author: S. Muldrew (UKAEA)
    !!
    !! Module containing global variables relating to the inertial fusion energy model
    !!
    !!### References
    !!
    !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code
     
    use, intrinsic :: iso_fortran_env, only: dp=>real64 
  
    implicit none
  
    public
     
     
    !! Default IFE builds and material volumes are those for the SOMBRERO device.
    !! The 2-dimensional arrays have indices (region, material), where 'region'
    !! is the region and maxmat is the 'material':
    !!
    !! - 'region' = 1 radially outside chamber
    !! - 'region' = 2 above chamber
    !! - 'region' = 3 below chamber
    
    integer, parameter ::  maxmat = 8
    !! Total number of materials in IFE device. Material numbers are as follows:
    !!
    !! - =0 void
    !! - =1 steel
    !! - =2 carbon cloth
    !! - =3 FLiBe
    !! - =4 lithium oxide Li2O
    !! - =5 concrete
    !! - =6 helium
    !! - =7 xenon
    !! - =8 lithium
     
    real(dp) :: bldr   = 1.0D0
    !! radial thickness of IFE blanket (m; calculated `if ifetyp=4`)
  
    real(dp) :: bldrc   = 1.0D0
    !! radial thickness of IFE curtain (m; `ifetyp=4`)
  
    real(dp) :: bldzl  = 4.0D0
    !! vertical thickness of IFE blanket below chamber (m)
  
    real(dp) :: bldzu  = 4.0D0
    !! vertical thickness of IFE blanket above chamber (m)
  
    real(dp), dimension(3,0:maxmat) :: blmatf = reshape( (/ &
      0.05D0,0.05D0,0.05D0, &
      0.0D0, 0.0D0, 0.0D0,  &
      0.45D0,0.45D0,0.45D0, &
      0.0D0, 0.0D0, 0.0D0,  &
      0.20D0,0.20D0,0.20D0, &
      0.0D0, 0.0D0, 0.0D0,  &
      0.30D0,0.30D0,0.30D0, &
      0.0D0,0.0D0,0.0D0,    &
      0.0D0, 0.0D0, 0.0D0  /), shape(blmatf))
    !! IFE blanket material fractions
  
    real(dp), dimension(3,0:maxmat) :: blmatm = 0.0D0
    !! IFE blanket material masses (kg)
  
    real(dp), dimension(3,0:maxmat) :: blmatv = 0.0D0
    !! IFE blanket material volumes (m3)
  
    real(dp), dimension(3) :: blvol = 0.0D0
    !! IFE blanket volume (m3)
  
    real(dp) :: cdriv0 = 154.3D0
    !! IFE generic/laser driver cost at edrive=0 (M$)
  
    real(dp) :: cdriv1 = 163.2D0
    !! IFE low energy heavy ion beam driver cost extrapolated to `edrive=0` (M$)
  
    real(dp) :: cdriv2 = 244.9D0
    !! IFE high energy heavy ion beam driver cost extrapolated to `edrive=0` (M$)
  
    real(dp) :: cdriv3 = 1.463D0
    !! IFE driver cost ($/J wall plug) (`ifedrv==3`)
  
    real(dp) :: chdzl = 9.0D0
    !! vertical thickness of IFE chamber below centre (m)
  
    real(dp) :: chdzu = 9.0D0
    !! vertical thickness of IFE chamber above centre (m)
  
    real(dp), dimension(0:maxmat) :: chmatf = &
      (/1.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/)
    !! IFE chamber material fractions
  
    real(dp), dimension(0:maxmat) :: chmatm = 0.0D0
    !! IFE chamber material masses (kg)
  
    real(dp), dimension(0:maxmat) :: chmatv = 0.0D0
    !! IFE chamber material volumes (m3)
  
    real(dp) :: chrad = 6.5D0
    !! radius of IFE chamber (m) (`iteration variable 84`)
  
    real(dp) :: chvol = 0.0D0
    !! IFE chamber volume (m3)
  
    real(dp) :: dcdrv0 = 111.4D0
    !! IFE generic/laser driver cost gradient (M$/MJ)
  
    real(dp) :: dcdrv1 = 78.0D0
    !! HIB driver cost gradient at low energy (M$/MJ)
  
    real(dp) :: dcdrv2 = 59.9D0
    !! HIB driver cost gradient at high energy (M$/MJ)
  
    real(dp) :: drveff = 0.28D0
    !! IFE driver wall plug to target efficiency (`ifedrv=0,3`) (`iteration variable 82`)
  
    real(dp) :: edrive = 5.0D6
    !! IFE driver energy (J) (`iteration variable 81`)
  
    real(dp) :: etadrv = 0.0D0
    !! IFE driver wall plug to target efficiency
  
    real(dp) :: etali = 0.4D0
    !! IFE lithium pump wall plug efficiency (`ifetyp=4`)
  
    real(dp), dimension(10) :: etave = (/ &
      0.082D0,0.079D0,0.076D0,0.073D0,0.069D0, &
      0.066D0,0.062D0,0.059D0,0.055D0,0.051D0 /)
    !! IFE driver efficiency vs driver energy (`ifedrv=-1`)
  
    real(dp) :: fauxbop = 0.06D0
    !! fraction of gross electric power to balance-of-plant (IFE)
  
    real(dp) :: fbreed = 0.51D0
    !! fraction of breeder external to device core
  
    real(dp) :: fburn  = 0.3333D0
    !! IFE burn fraction (fraction of tritium fused/target)
  
    real(dp) :: flirad = 0.78D0
    !! radius of FLiBe/lithium inlet (m) (`ifetyp=3,4`)
  
    real(dp) :: frrmax = 1.0D0
    !! f-value for maximum IFE repetition rate (`constraint equation 50`, `iteration variable 86`)
  
    real(dp) :: fwdr = 0.01D0
    !! radial thickness of IFE first wall (m)
  
    real(dp) :: fwdzl = 0.01D0
    !! vertical thickness of IFE first wall below chamber (m)
  
    real(dp) :: fwdzu = 0.01D0
    !! vertical thickness of IFE first wall above chamber (m)
  
    real(dp), dimension(3,0:maxmat) :: fwmatf = reshape( (/ &
      0.05D0,0.05D0,0.05D0, &
      0.0D0, 0.0D0, 0.0D0,  &
      0.95D0,0.95D0,0.95D0, &
      0.0D0, 0.0D0, 0.0D0,  &
      0.0D0, 0.0D0, 0.0D0,  &
      0.0D0, 0.0D0, 0.0D0,  &
      0.0D0, 0.0D0, 0.0D0,  &
      0.0D0, 0.0D0, 0.0D0,  &
      0.0D0, 0.0D0, 0.0D0  /), shape(fwmatf))
    !! IFE first wall material fractions
  
    real(dp), dimension(3,0:maxmat) :: fwmatm = 0.0D0
    !! IFE first wall material masses (kg)
  
    real(dp), dimension(3,0:maxmat) :: fwmatv = 0.0D0
    !! IFE first wall material volumes (kg)
  
    real(dp), dimension(3) :: fwvol = 0.0D0
    !! IFE first wall volume (m3)
  
    real(dp) :: gain = 0.0D0
    !! IFE target gain
  
    real(dp), dimension(10) :: gainve = (/ &
      60.0D0, 95.0D0,115.0D0,125.0D0,133.0D0, &
      141.0D0,152.0D0,160.0D0,165.0D0,170.0D0 /)
    !! IFE target gain vs driver energy (`ifedrv=-1`)
  
    real(dp) :: htpmw_ife = 0.0D0         
    !! IFE heat transport system electrical pump power (MW)
  
    integer :: ife = 0
    !! Switch for IFE option (set via `device.dat`):
    !!
    !! - =0 use tokamak, RFP or stellarator model
    !! - =1 use IFE model
  
    integer :: ifedrv = 2
    !! Switch for type of IFE driver:
    !!
    !! - =-1 use gainve, etave for gain and driver efficiency
    !! - =0 use tgain, drveff for gain and driver efficiency
    !! - =1 use laser driver based on SOMBRERO design
    !! - =2 use heavy ion beam driver based on OSIRIS
    !! - =3 Input pfusife, rrin and drveff
  
    integer :: ifetyp = 0
    !! Switch for type of IFE device build:
    !!
    !! - =0 generic (cylindrical) build
    !! - =1 OSIRIS-like build
    !! - =2 SOMBRERO-like build
    !! - =3 HYLIFE-II-like build
    !! - =4 2019 build
  
    real(dp) :: lipmw = 0.0D0
    !! IFE lithium pump power (MW; `ifetyp=4`)
  
    real(dp) :: mcdriv = 1.0D0
    !! IFE driver cost multiplier
  
    real(dp) :: mflibe = 0.0D0
    !! total mass of FLiBe (kg)
  
    real(dp) :: pdrive = 23.0D6
    !! IFE driver power reaching target (W) (`iteration variable 85`)
    
    real(dp) :: pfusife = 1000.0D0
    !! IFE input fusion power (MW) (`ifedrv=3 only`; `itv 155`)
  
    real(dp) :: pifecr = 10.0D0
    !! IFE cryogenic power requirements (MW)
  
    real(dp) :: ptargf = 2.0D0
    !! IFE target factory power at 6 Hz repetition rate (MW)
  
    real(dp) :: r1 = 0.0D0
    !! IFE device radial build (m)
  
    real(dp) :: r2 = 0.0D0
    !! IFE device radial build (m)
  
    real(dp) :: r3 = 0.0D0
    !! IFE device radial build (m)
  
    real(dp) :: r4 = 0.0D0
    !! IFE device radial build (m)
  
    real(dp) :: r5 = 0.0D0
    !! IFE device radial build (m)
  
    real(dp) :: r6 = 0.0D0
    !! IFE device radial build (m)
  
    real(dp) :: r7 = 0.0D0
    !! IFE device radial build (m)
  
    real(dp) :: reprat = 0.0D0
    !! IFE driver repetition rate (Hz)
  
    real(dp) :: rrin = 6.0D0
    !! Input IFE repetition rate (Hz) (`ifedrv=3 only`; `itv 156`)
  
    real(dp) :: rrmax = 20.0D0
    !! maximum IFE repetition rate (Hz)
  
    real(dp) :: shdr = 1.7D0
    !! radial thickness of IFE shield (m)
  
    real(dp) :: shdzl = 5.0D0
    !! vertical thickness of IFE shield below chamber (m)
  
    real(dp) :: shdzu  = 5.0D0
    !! vertical thickness of IFE shield above chamber (m)
  
    real(dp), dimension(3,0:maxmat) :: shmatf = reshape( (/ &
      0.05D0,0.05D0,0.05D0, &
      0.19D0,0.19D0,0.19D0, &
      0.0D0, 0.0D0, 0.0D0,  &
      0.0D0, 0.0D0, 0.0D0,  &
      0.0D0, 0.0D0, 0.0D0,  &
      0.665D0,0.665D0,0.665D0, &
      0.095D0,0.095D0,0.095D0, &
      0.0D0, 0.0D0, 0.0D0,  &
      0.0D0, 0.0D0, 0.0D0  /), shape(shmatf))
    !! IFE shield material fractions
  
    real(dp), dimension(3,0:maxmat) :: shmatm = 0.0D0
    !! IFE shield material masses (kg)
  
    real(dp), dimension(3,0:maxmat) :: shmatv = 0.0D0
    !! IFE shield material volumes (kg)
  
    real(dp), dimension(3) :: shvol = 0.0D0
    !! IFE shield volume (m3)
  
    real(dp) :: sombdr = 2.7D0
    !! radius of cylindrical blanket section below chamber (`ifetyp=2`)
  
    real(dp) :: somtdr = 2.7D0
    !! radius of cylindrical blanket section above chamber (`ifetyp=2`)
  
    real(dp) :: taufall = 0.0D0
    !! Lithium Fall Time (s)
  
    real(dp) :: tdspmw = 0.01D0
    !! IFE target delivery system power (MW)
  
    real(dp) :: tfacmw = 0.0D0
    !! IFE target factory power (MW)
  
    real(dp) :: tgain = 85.0D0
    !! IFE target gain (if `ifedrv = 0`) (`iteration variable 83`)
  
    real(dp) :: uccarb = 50.0D0
    !! cost of carbon cloth ($/kg)
  
    real(dp) :: ucconc = 0.1D0
    !! cost of concrete ($/kg)
  
    real(dp) :: ucflib = 84.0D0
    !! cost of FLiBe ($/kg)
  
    real(dp) :: uctarg = 0.3D0
    !! cost of IFE target ($/target)
  
    real(dp) :: v1dr = 0.0D0
    !! radial thickness of IFE void between first wall and blanket (m)
  
    real(dp) :: v1dzl = 0.0D0
    !! vertical thickness of IFE void 1 below chamber (m)
  
    real(dp) :: v1dzu = 0.0D0
    !! vertical thickness of IFE void 1 above chamber (m)
  
    real(dp), dimension(3,0:maxmat) :: v1matf = reshape( (/ &
      1.0D0, 1.0D0, 1.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0  /), shape(v1matf))
    !! IFE void 1 material fractions
  
    real(dp), dimension(3,0:maxmat) :: v1matm = 0.0D0
    !! IFE void 1 material masses (kg)
  
    real(dp), dimension(3,0:maxmat) :: v1matv = 0.0D0
    !! IFE void 1 material volumes (kg)
  
    real(dp), dimension(3) :: v1vol = 0.0D0
    !! IFE void 1 volume (m3)
  
    real(dp) :: v2dr = 2.0D0
    !! radial thickness of IFE void between blanket and shield (m)
  
    real(dp) :: v2dzl = 7.0D0
    !! vertical thickness of IFE void 2 below chamber (m)
  
    real(dp) :: v2dzu = 7.0D0
    !! vertical thickness of IFE void 2 above chamber (m)
  
    real(dp), dimension(3,0:maxmat) :: v2matf = reshape( (/ &
      1.0D0, 1.0D0, 1.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0  /), shape(v2matf))
    !! IFE void 2 material fractions
  
    real(dp), dimension(3,0:maxmat) :: v2matm = 0.0D0
    !! IFE void 2 material masses (kg)
  
    real(dp), dimension(3,0:maxmat) :: v2matv = 0.0D0
    !! IFE void 2 material volumes (kg)
  
    real(dp), dimension(3) :: v2vol = 0.0D0
    !! IFE void 2 volume (m3)
  
    real(dp) :: v3dr   = 43.3D0
    !! radial thickness of IFE void outside shield (m)
  
    real(dp) :: v3dzl  = 30.0D0
    !! vertical thickness of IFE void 3 below chamber (m)
  
    real(dp) :: v3dzu  = 20.0D0
    !! vertical thickness of IFE void 3 above chamber (m)
  
    real(dp), dimension(3,0:maxmat) :: v3matf = reshape( (/ &
      1.0D0, 1.0D0, 1.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0, &
      0.0D0, 0.0D0, 0.0D0  /), shape(v3matf))
    !! IFE void 3 material fractions
  
    real(dp), dimension(3,0:maxmat) :: v3matm = 0.0D0
    !! IFE void 3 material masses (kg)
  
    real(dp), dimension(3,0:maxmat) :: v3matv = 0.0D0
    !! IFE void 3 material volumes (kg)
  
    real(dp), dimension(3) :: v3vol = 0.0D0
    !! IFE void 3 volume (m3)
  
    real(dp) :: zl1 = 0.0D0
    !! IFE vertical build below centre (m)
  
    real(dp) :: zl2 = 0.0D0
    !! IFE vertical build below centre (m)
  
    real(dp) :: zl3 = 0.0D0
    !! IFE vertical build below centre (m)
  
    real(dp) :: zl4 = 0.0D0
    !! IFE vertical build below centre (m)
  
    real(dp) :: zl5 = 0.0D0
    !! IFE vertical build below centre (m)
  
    real(dp) :: zl6 = 0.0D0
    !! IFE vertical build below centre (m)
  
    real(dp) :: zl7 = 0.0D0
    !! IFE vertical build below centre (m)
  
    real(dp) :: zu1 = 0.0D0
    !! IFE vertical build above centre (m)
  
    real(dp) :: zu2 = 0.0D0
    !! IFE vertical build above centre (m)
  
    real(dp) :: zu3 = 0.0D0
    !! IFE vertical build above centre (m)
  
    real(dp) :: zu4 = 0.0D0
    !! IFE vertical build above centre (m)
  
    real(dp) :: zu5 = 0.0D0
    !! IFE vertical build above centre (m)
  
    real(dp) :: zu6 = 0.0D0
    !! IFE vertical build above centre (m)
  
    real(dp) :: zu7 = 0.0D0
    !! IFE vertical build above centre (m)
     
  end module ife_variables