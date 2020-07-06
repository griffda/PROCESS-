module tfcoil_variables
  !! author: J. Morris, M. Kovari, S. Kahn (UKAEA)
  !!
  !! Module containing global variables relating to the toroidal field coil systems
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !! - ITER Magnets design description document DDD11-2 v2 2 (2009)

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(dp) :: acasetf = 0.0D0
  !! external case area per coil (inboard leg) (m2)

  real(dp) :: acasetfo = 0.0D0
  !! external case area per coil (outboard leg) (m2)

  real(dp) :: acndttf = 0.0D0
  !! area of the cable conduit (m2)

  real(dp) :: acond = 0.0D0
  !! conductor area (winding pack) (m2)

  real(dp) :: acstf = 0.0D0
  !! internal area of the cable space (m2)

  real(dp) :: insulation_area = 0.0D0
  !! single turn insulation area (m2)

  real(dp) :: aiwp = 0.0D0
  !! winding pack insulation area (m2)

  real(dp) :: alstrtf = 6.0D8
  !! Allowable Tresca stress in TF coil structural material (Pa)

  real(dp) :: arealeg = 0.0D0
  !! outboard TF leg area (m2)

  real(dp) :: aswp = 0.0D0
  !! winding pack structure area (m2)

  real(dp) :: avwp = 0.0D0
  !! winding pack void (He coolant) area (m2)

  real(dp) :: awphec = 0.0D0
  !! winding pack He coil area (m2)

  real(dp) :: bcritsc = 24.0D0
  !! upper critical field (T) for Nb3Sn superconductor at zero temperature and 
  !! strain (`i_tf_sc_mat=4, =bc20m`)

  real(dp) :: bmaxtf = 0.0D0
  !! mean peak field at TF coil (T)

  real(dp) :: bmaxtfrp = 0.0D0
  !! peak field at TF conductor with ripple (T)

  real(dp) :: casestr = 0.0D0
  !! case strain

  real(dp) :: casthi = 0.0D0
  !! inboard TF coil case plasma side thickness (m) (calculated for stellarators)

  real(dp) :: casthi_fraction = 0.05D0
  !! inboard TF coil case plasma side thickness as a fraction of tfcth

  logical :: casthi_is_fraction
  !! logical switch to make casthi a fraction of TF coil thickness (`casthi_fraction`)

  real(dp) :: casths = 0.0D0
  !! inboard TF coil sidewall case thickness (m) (calculated for stellarators)

  real(dp) :: casths_fraction = 0.03D0
  !! inboard TF coil sidewall case thickness as a fraction of tftort

  logical :: tfc_sidewall_is_fraction
  !! logical switch to make casths a fraction of TF coil thickness (`casths_fraction`)

  real(dp) :: t_conductor = 0.0D0
  !! Conductor (cable + steel conduit) area averaged dimension [m]
  
  real(dp) :: t_turn = 0.0D0
  !! WP turn squared dimensions [m]

  real(dp) :: acs = 0.0D0
  !! Area of space inside conductor (m2)

  real(dp) :: cdtfleg = 0.0D0
  !! TF outboard leg current density (A/m2) (resistive coils only)
  
  real(dp) :: cforce = 0.0D0
  !! centering force on inboard leg (per coil) (N/m)

  real(dp) :: cpttf = 7.0e4
  !! TF coil current per turn (A). (calculated for stellarators) (calculated for 
  !! integer-turn TF coils `i_tf_turns_integer=1`) (`iteration variable 60`)

  real(dp) :: cpttf_max = 9.0e4
  !! Max TF coil current per turn [A]. (for stellarators and `i_tf_turns_integer=1`) 
  !! (`constraint equation 77`)

  real(dp) :: dcase = 8000.0D0
  !! density of coil case (kg/m3)

  real(dp), dimension(7) :: dcond = 9000.0D0
  !! density of superconductor type given by i_tf_sc_mat/isumatoh/isumatpf (kg/m3)
  
  real(dp) :: dcondins = 1800.0D0
  !! density of conduit + ground-wall insulation (kg/m3)

  real(dp) :: dhecoil = 0.005D0
  !! diameter of He coil in TF winding (m)

  real(dp) :: estotftgj = 0.0D0
  !! total stored energy in the toroidal field (GJ)

  real(dp) :: farc4tf = 0.7D0
  !! farc4tf /0.7/ : factor to size height of point 4 on TF coil
  real(kind(1.0D0)) :: b_crit_upper_nbti = 14.86D0
  !! b_crit_upper_nbti /14.86/ : upper critical field of GL_nbti
  real(kind(1.0D0)) :: t_crit_nbti = 9.04D0
  !! t_crit_nbti /9.04/ : critical temperature of GL_nbti
  real(kind(1.0D0)) :: max_force_density = 0.0D0
  !! max_force_density :  Maximal (WP averaged) force density in TF coils at 1 point. (MN/m3)
  real(kind(1.0D0)) :: fcutfsu = 0.69D0
  !! fcutfsu /0.69/ : copper fraction of cable conductor (TF coils)
  !!                  (iteration variable 59)
  real(dp) :: fhts = 0.5D0
  !! technology adjustment factor for critical current density fit for isumat..=2 
  !! Bi-2212 superconductor, to describe the level of technology assumed (i.e. to 
  !! account for stress, fatigue, radiation, AC losses, joints or manufacturing 
  !! variations; 1.0 would be very optimistic)
  
  real(dp) :: insstrain = 0.0D0
  !! Radial strain in insulator

  integer :: i_tf_plane_stress = 1
  !! Switch for the TF coil stress model
  !!   0 : New generalized plane strain formulation 
  !!   1 : Old plane stress model (only for SC)

  integer :: i_tf_tresca = 0
  !! Switch for TF coil conduit Tresca stress criterion:
  !!   0 : Tresca (no adjustment);
  !!   1 : Tresca with CEA adjustment factors (radial+2%, vertical+60%) </UL>
  
  integer :: i_tf_wp_geom = -1
  !! Switch for TF WP geometry selection
  !!   0 : Rectangular geometry 
  !!   1 : Double rectangular geometry 
  !!   2 : Trapezoidal geometry (constant lateral casing thickness)
  !! Default setting for backward compatibility 
  !!   if i_tf_turns_integer = 0 : Double rectangular
  !!   if i_tf_turns_integer = 1 : Rectangular 

  integer :: i_tf_turns_integer = 0
  !! Switch for TF coil integer/non-integer turns:
  !!   0 : non-integer turns
  !!   1 : integer turns

  integer :: i_tf_sc_mat = 1
  !! Switch for superconductor material in TF coils:
  !!
  !! - =1 ITER Nb3Sn critical surface model with standard
  !!   ITER parameters
  !! - =2 Bi-2212 high temperature superconductor (range of
  !!   validity T < 20K, adjusted field b < 104 T, B > 6 T)
  !! - =3 NbTi
  !! - =4 ITER Nb3Sn model with user-specified parameters
  !! - =5 WST Nb3Sn parameterisation
  !! - =6 REBCO HTS tape in CroCo strand

  integer :: i_tf_sup = 1
  !! Switch for TF coil conductor model:
  !!
  !! - =0 copper
  !! - =1 superconductor
  !! - =2 Cryogenic aluminium

  integer :: i_tf_shape = 0
  !! Switch for TF coil toroidal shape:
  !!
  !! - =0  Default value : Picture frame coil for TART / PROCESS D-shape for non itart
  !! - =1  PROCESS D-shape : parametrise with 2 arcs 
  !! - =2  Picture frame coils 

  integer :: n_pancake = 10
  !! Number of pancakes in TF coil. Only used if `i_tf_turns_integer=1`

  integer :: n_layer = 20
  !! Number of layers in TF coil. Only used if `i_tf_turns_integer=1`
  
  integer :: n_rad_per_layer = 50
  !! Size of the arrays per layers storing the radial dependent stress 
  !! quantities (stresses, strain displacement etc..)

  integer :: i_tf_bucking = -1
  !! Switch for TF inboard suport structure design:
  !! 
  !! Default setting for backward compatibility
  !!     - if copper resistive TF (i_tf_sup = 0) : Free standing TF without bucking structure 
  !!     - if Superconducting TF  (i_tf_sup = 1) : Free standing TF with a steel casing  
  !!     - if aluminium  TF       (i_tf_sup = 2) : Free standing TF with a bucking structure
  !!     Rem : the case is a bucking structure
  !! - =0 : Free standing TF without case/bucking cyliner (only a conductor layer)
  !! - =1 : Free standing TF with a case/bucking cylinder made of 
  !!     - if copper resistive     TF (i_tf_sup = 0) : used defined bucking cylinder
  !!     - if Superconducting      TF (i_tf_sup = 1) : Steel casing
  !!     - if aluminium resisitive TF (i_tf_sup = 2) : used defined bucking cylinder
  !! - =2 : The TF is in contact with the CS : "bucked and weged design"
  !!       Fast version : thin TF-CS interface neglected in the stress calculations (3 layers)
  !! - =3 : The TF is in contact with the CS : "bucked and weged design"
  !!       Full version : thin TF-CS Kapton interface introduced in the stress calculations (4 layers)

  integer :: n_tf_graded_layers = 1
  !! Number of layers of different stress properties in the WP. If `n_tf_graded_layers > 1`, 
  !! a graded coil is condidered

  integer :: n_tf_stress_layers = 0
  !! Number of layers considered for the inboard TF stress calculations
  !! set in initial.f90 from i_tf_bucking and n_tf_graded_layers

  real(dp) :: jbus = 1.25D6
  !! bussing current density (A/m2)
  
  real(dp) :: jwdgcrt = 0.0D0
  !! critical current density for winding pack (A/m2)

  real(dp) :: jwdgpro = 0.0D0
  !! allowable TF coil winding pack current density, for dump temperature rise protection (A/m2)

  real(dp) :: jwptf = 0.0D0
  !! winding pack engineering current density (A/m2)

  real(dp) :: oacdcp = 0.0D0
  !! Overall current density in TF coil inboard legs midplane (A/m2)
  !! Rem SK : Not used in tfcoil to set the current any more. Should not be used as
  !! iteration variable 12 any more. It is now calculated.

  real(dp) :: eyzwp = 0.0D0
  !! Winding pack vertical Young's modulus (Pa)

  real(dp) :: eyoung_ins = 1.0D8
  !! Insulator Young's modulus [Pa]. Default value (1.0D8) setup the following values
  !!  - SC TF, eyoung_ins = 20 Gpa (default value from DDD11-2 v2 2 (2009))
  !!  - Al TF, eyoung_ins = 2.5 GPa (Kapton polymer)

  real(dp) :: eyoung_steel = 2.05D11
  !! Steel case Young's modulus (Pa) (default value from DDD11-2 v2 2 (2009))

  real(dp) :: eyoung_winding = 6.6D8
  !! SC TF coil winding Young's modulus (Pa)
  
  real(dp) :: eyoung_res_tf_buck = 150.0D9 
  !! Resistive TF magnets bucking cylinder young modulus (Pa)

  real(dp) :: eyoung_copper = 117.0D9
  !! Copper young modulus. Default value taken from wikipedia

  real(dp) :: eyoung_al = 69.0D9 
  !! Aluminium young modulus.  Default value taken from wikipedia
  
  real(dp) :: poisson_steel = 0.3D0
  !! Steel Poisson's ratio 
  
  real(dp):: poisson_copper = 0.35D0
  !! Copper Poisson's ratio. Source : https://www.engineeringtoolbox.com/poissons-ratio-d_1224.html

  real(dp):: poisson_al = 0.35D0
  !! Aluminium Poisson's ratio. 
  !! Source : https://www.engineeringtoolbox.com/poissons-ratio-d_1224.html

  real(dp) :: rbmax = 0.0D0
  !! Radius of maximum TF B-field (m)

  real(dp) :: tflegres = 0.0D0
  !! TF coil leg resistance (ohm)

  real(dp) :: toroidalgap = 1.0D0 ![m]
  !! Minimal distance between two toroidal coils. (m)

  real(dp) :: ftoroidalgap = 1.0D0
  !! F-value for minimum tftort (`constraint equation 82`)

  real(dp) :: ripmax = 1.0D0
  !! aximum allowable toroidal field ripple amplitude at plasma edge (%)

  real(dp) :: ripple = 0.0D0
  !! peak/average toroidal field ripple at plasma edge (%)

  real(dp) :: ritfc = 0.0D0
  !! total (summed) current in TF coils (A)
  
  integer, parameter :: n_radial_array = 50
  !! Size of the radial distribution arrays per layers
  !! used for stress, strain and displacement distibution

  real(dp), dimension(2*n_radial_array) :: radial_array = 0.0D0
  !! Array refining the radii of the stress calculations arrays

  real(dp), dimension(2*n_radial_array) :: sig_tf_r = 0.0D0
  !! TF Inboard leg radial stress in steel r distribution at mid-plane [Pa]
  
  real(dp), dimension(2*n_radial_array) :: sig_tf_t = 0.0D0
  !! TF Inboard leg tangential stress in steel r distribution at mid-plane [Pa]
  
  real(dp), dimension(2*n_radial_array) :: deflect = 0.0D0
  !! TF coil radial deflection (displacement) radial distribution [m]

  real(dp) :: sig_tf_z = 0.0D0
  !! TF Inboard leg vertical tensile stress in steel at mid-plane [Pa]
    
  real(dp), dimension(2*n_radial_array) :: sig_tf_vmises = 0.0D0
  !! TF Inboard leg Von-Mises stress in steel r distribution at mid-plane [Pa]
      
  real(dp), dimension(2*n_radial_array) :: sig_tf_tresca = 0.0D0 
  !! TF Inboard leg TRESCA stress in steel r distribution at mid-plane [Pa]

  real(dp) :: strtf0 = 0.0D0
  !! Maximum TRESCA stress in CS structures at CS flux swing [Pa]:
  !!
  !!  - If superconducting CS (ipfres = 0): turn steel conduits TRESCA stress
  !!  - If resistive       CS (ipfres = 1): copper conductor TRESCA stress 
  !!
  !! Quantity only computed for bucked and wedged design (`i_tf_bucking >= 2`)
  !! Def : CS Flux swing, instant when the current changes sign in CS (null current) 

  real(dp) :: strtf1 = 0.0D0
  !! Maximum TRESCA stress in TF casing steel structures (Pa)
  
  real(dp) :: strtf2 = 0.0D0
  !! Maximum TRESCA stress in TF WP conduit steel structures (Pa)
  
  real(dp) :: sigvvall = 9.3D7
  !! allowable stress from TF quench in vacuum vessel (Pa)

  real(dp) :: strncon_cs = -0.005D0
  !! strain in CS superconductor material (used in Nb3Sn critical surface model `isumatoh=1,4,5`)

  real(dp) :: strncon_pf = -0.005D0
  !! strain in PF superconductor material (used in Nb3Sn critical surface model `isumatph=1,4,5`)

  real(dp) :: strncon_tf = -0.005D0
  !! strain in TF superconductor material (used in Nb3Sn critical surface model `i_tf_sc_mat=1,4,5`)

  character(len=12) :: quench_model = 'exponential'
  !! switch for TF coil quench model (Only applies to REBCO magnet at present, issue #522):
  !!
  !! - ='exponential' exponential quench with constant discharge resistor
  !! - ='linear' quench with constant voltage

  real(dp) :: quench_detection_ef = 0D0
  !! Electric field at which TF quench is detected and discharge begins (V/m)

  real(dp) :: time1 = 0D0
  !! Time at which TF quench is detected (s)

  real(dp) :: taucq = 30.0D0
  !! allowable TF quench time (s)

  real(dp) :: tcritsc = 16.0D0
  !! critical temperature (K) for superconductor at zero field and strain (`i_tf_sc_mat=4, =tc0m`)

  real(dp) :: tdmptf = 10.0D0
  !! fast discharge time for TF coil in event of quench (s) (`iteration variable 56`)
  !!
  !! For REBCO model, meaning depends on quench_model:
  !!
  !! - exponential quench : e-folding time (s)`
  !! - linear quench : discharge time (s)

  real(dp) :: tfareain = 0.0D0
  !! Area of inboard midplane TF legs (m2)

  real(dp) :: tfbusl = 0.0D0
  !! TF coil bus length (m)

  real(dp) :: tfbusmas = 0.0D0
  !! TF coil bus mass (kg)

  real(dp) :: tfckw = 0.0D0
  !! available DC power for charging the TF coils (kW)

  !#TODO: issue #781
  ! integer :: tfc_model = 1
  ! !! tfc_model /1/ : switch for TF coil magnet stress model:<UL>
  ! !!                 <LI> = 0 simple model (solid copper coil)
  ! !!                 <LI> = 1 CCFE two-layer stress model; superconductor</UL>

  real(dp) :: tfcmw = 0.0D0
  !! Peak power per TF power supply (MW)
  
  real(dp) :: tfcpmw = 0.0D0
  !! Peak resistive TF coil inboard leg power (MW)

  real(dp) :: tfjtsmw = 0.0D0
  !! TF joints resistive power losses (MW)

  real(dp) :: tfcryoarea = 0.0D0
  !! surface area of toroidal shells covering TF coils (m2)

  real(dp) :: tficrn = 0.0D0
  !! TF coil half-width - inner bore (m)

  real(dp) :: tfind = 0.0D0
  !! TF coil inductance (H)

  real(dp) :: tfinsgap = 0.010D0
  !! TF coil WP insertion gap (m)
  
  real(dp) :: tflegmw = 0.0D0
  !! TF coil outboard leg resistive power (MW)

  real(dp) :: rhocp = 0.0D0
  !! TF coil inboard leg resistivity [Ohm-m]. If `itart=0`, this variable is the 
  !! average resistivity over the whole magnet

  real(dp) :: rhotfleg = 0.0D0
  !! Resistivity of a TF coil leg (Ohm-m)

  real(dp) :: rhotfbus = -1.0D0 ! 2.5D-8
  !! Resistivity of a TF coil bus (Ohm-m). Default value takes the same res as the leg one
  
  real(dp) :: frhocp = 1.0D0
  !! Centrepost resistivity enhancement factor. For `itart=0`, this factor 
  !! is used for the whole magnet 
  
  real(dp) :: frholeg = 1.0D0
  !! Ouboard legs resistivity enhancement factor. Only used for `itart=1`.
  
  real(dp) :: rho_tf_joints = 2.5D-10
  !! TF joints surfacic resistivity [ohm.m^2]. Feldmetal joints assumed.

  integer :: n_tf_joints_contact = 6
  !! Number of contact per sliding joint

  integer :: n_tf_joints = 4
  !! Number of joint per turn

  real(dp) :: th_joint_contact = 0.03D0
  !! TF sliding joints contact pad width [m]

  real(dp) :: pres_joints = 0.0D0
  !! Calculated TF joints resistive power losses [W]

  real(dp) :: tfleng = 0.0D0
  !! TF coil circumference (m)

  real(dp) :: eff_tf_cryo = -1.0D0
  !! TF cryoplant efficiency (compared to pefect Carnot cycle).
  !! Using -1 set the default value depending on magnet technology:
  !!
  !!  - i_tf_sup = 1 : SC magnet, eff_tf_cryo = 0.13 (ITER design)
  !!  - i_tf_sup = 2 : Cryo-aluminium, eff_tf_cryo = 0.4

  real(dp) :: n_tf = 16.0D0
  !! Number of TF coils (default = 50 for stellarators). Number of TF coils outer legs for ST

  real(dp) :: tfocrn = 0.0D0
  !! TF coil half-width - outer bore (m)

  real(dp) :: tfsai = 0.0D0
  !! area of the inboard TF coil legs (m2)

  real(dp) :: tfsao = 0.0D0
  !! area of the outboard TF coil legs (m2)

  real(dp) :: tftmp = 4.5D0
  !! peak helium coolant temperature in TF coils and PF coils (K)

  real(dp) :: tftort = 1.0D0
  !! TF coil toroidal thickness (m)

  real(dp) :: thicndut = 8.0D-4
  !! conduit insulation thickness (m)

  real(dp) :: layer_ins = 0.0D0
  !! Additional insulation thickness between layers (m)

  real(dp) :: thkcas = 0.3D0
  !! inboard TF coil case outer (non-plasma side) thickness (m) (`iteration variable 57`)
  !! (calculated for stellarators)

  real(dp) :: dr_tf_wp = 0.0D0
  !! radial thickness of winding pack (m) (`iteration variable 140`) (issue #514)

  real(dp) :: thwcndut = 8.0D-3
  !! TF coil conduit case thickness (m) (`iteration variable 58`)
  
  real(dp) :: tinstf = 0.018D0
  !! Thickness of the ground insulation layer surrounding (m) 
  !! 
  !!   - Superconductor TF (`i_tf_sup == 1`) : The TF Winding packs
  !!   - Resistive magnets (`i_tf_sup /= 1`) : The TF turns
  !!
  !! Rem : The default value includes allowance for 10 mm insertion gap.
  !! Rem : Thickness calculated for stellarators.

  real(dp) :: tmargmin_tf = 0D0
  !! minimum allowable temperature margin : TF coils (K)

  real(dp) :: tmargmin_cs = 0D0
  !! minimum allowable temperature margin : CS (K)

  real(dp) :: tmargmin = 0D0
  !! minimum allowable temperature margin : TFC AND CS (K)

  real(dp) :: temp_margin = 0.00D0
  !! temperature margin (K)

  real(dp) :: tmargtf = 0.0D0
  !! TF coil temperature margin (K)

  real(dp) :: tmaxpro = 150.0D0
  !! maximum temp rise during a quench for protection (K)

  real(dp) :: tmax_croco = 200.0D0
  !! CroCo strand: maximum permitted temp during a quench (K)

  real(dp) :: croco_quench_temperature = 0D0
  !! CroCo strand: Actual temp reached during a quench (K)

  real(dp) :: tmpcry = 4.5D0
  !! coil temperature for cryogenic plant power calculation (K)

  real(dp) :: turnstf = 0.0D0
  !! number of turns per TF coil

  real(dp) :: vdalw = 20.0D0
  !! max voltage across TF coil during quench (kV) (`iteration variable 52`)

  real(dp) :: vforce = 0.0D0
  !! vertical separating force on inboard leg/coil (N)
  
  real(dp) :: f_vforce_inboard = 0.5D0
  !! Fraction of the total vertical force taken by the TF inboard leg
  !! Not used for resistive `itart=1` (sliding joints)

  real(dp) :: vforce_outboard = 0.0D0
  !! Vertical separating force on out board leg/coil (N)

  real(dp) :: vftf = 0.4D0
  !! coolant fraction of TFC 'cable' (`i_tf_sup=1`), or of TFC leg (`i_tf_ssup=0`)

  real(dp) :: voltfleg = 0.0D0
  !! volume of each TF coil outboard leg (m3)

  real(dp) :: vtfkv = 0.0D0
  !! TF coil voltage for resistive coil including bus (kV)

  real(dp) :: vtfskv = 0.0D0
  !! voltage across a TF coil during quench (kV)

  real(dp) :: whtcas = 0.0D0
  !! mass per coil of external case (kg)

  real(dp) :: whtcon = 0.0D0
  !! TF coil conductor mass per coil (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf

  real(dp) :: whtconcu = 0.0D0
  !! copper mass in TF coil conductor (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf
  
  real(dp) :: whtconal = 0.0D0
  !! Aluminium mass in TF coil conductor (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf
  
  real(dp) :: whtconin = 0.0D0
  !! conduit insulation mass in TF coil conductor (kg/coil)

  real(dp) :: whtconsc = 0.0D0
  !! superconductor mass in TF coil cable (kg/coil)

  real(dp) :: whtconsh = 0.0D0
  !! steel conduit mass in TF coil conductor (kg/coil)

  real(dp) :: whtgw = 0.0D0
  !! mass of ground-wall insulation layer per coil (kg/coil)

  real(dp) :: whttf = 0.0D0
  !! total mass of the TF coils (kg)

  real(dp) :: windstrain = 0.0D0
  !! longitudinal strain in winding pack

  real(dp) :: wwp1 = 0.0D0
  !! width of first step of winding pack (m)

  real(dp) :: wwp2 = 0.0D0
  !! width of second step of winding pack (m)

  ! Superconducting TF coil shape parameters</B> (see also farc4tf);
  ! the TF inner surface top half is approximated by four circular arcs.
  ! Arc 1 goes through points 1 and 2 on the inner surface. Arc 2
  ! goes through points 2 and 3, etc.
  real(dp), dimension(4) :: dthet = 0.0D0
  !! angle of arc i (rad)

  real(dp), dimension(4) :: radctf = 0.0D0
  !! radius of arc i (m)

  real(dp), dimension(5) :: xarc = 0.0D0
  !! x location of arc point i on surface (m)

  real(dp), dimension(4) :: xctfc = 0.0D0
  !! x location of arc centre i (m)

  real(dp), dimension(5) :: yarc = 0.0D0
  !! y location of arc point i on surface (m)

  real(dp), dimension(4) :: yctfc = 0.0D0
  !! y location of arc centre i (m)

  ! New TF shape:  Horizontal and vertical radii of inside edge of TF coil
  ! Arcs are numbered clockwise:
  ! 1=upper inboard, 2=upper outboard, 3=lower ouboard, 4=lower inboard

  real(dp), dimension(4) :: tfa = 0.0D0
  !! Horizontal radius of inside edge of TF coil (m)

  real(dp), dimension(4) :: tfb = 0.0D0
  !! Vertical radius of inside edge of TF coil (m)
  ! Quantities relating to the spherical tokamak model (itart=1)
  ! (and in some cases, also to resistive TF coils, i_tf_sup=0):

  real(dp) :: drtop = 0.0D0
  !! centrepost taper maximum radius adjustment (m)

  real(dp) :: dztop = 0.0D0
  !! centrepost taper height adjustment (m)

  real(dp) :: etapump = 0.8D0
  !! centrepost coolant pump efficiency

  real(dp) :: fcoolcp = 0.3D0
  !! coolant fraction of TF coil inboard legs (`iteration variable 23`)

  real(dp) :: fcoolleg = 0.2D0
  !! coolant fraction of TF coil inboard legs
  
  real(dp) :: a_cp_cool = 0.0D0
  !! Centrepost cooling area toroidal cross-section (constant over the whole CP)

  real(dp) :: ncool = 0.0D0
  !! number of centrepost coolant tubes

  real(dp) :: ppump = 0.0D0
  !! centrepost coolant pump power (W)

  real(dp) :: prescp = 0.0D0
  !! resistive power in the centrepost (itart=1) [W].
  !! If `itart=0`, this variable is the ressitive power on the whole magnet

  real(dp) :: presleg = 0.0D0
  !! Summed resistive power in the TF coil legs [W]. Remain 0 if `itart=0`.
    
  real(dp) :: ptempalw = 473.15D0   ! 200 C
  !! maximum peak centrepost temperature (K) (`constraint equation 44`)

  real(dp) :: rcool = 0.005D0
  !! average radius of coolant channel (m) (`iteration variable 69`)

  real(dp) :: tcoolin = 313.15D0   ! 40 C
  !! centrepost coolant inlet temperature (K)

  real(dp) :: dtiocool = 0.0D0
  !! inlet / outlet TF coil coolant temperature rise (K)  

  real(dp) :: tcpav = 373.15D0     ! 100 C
  !! Average temperature of centrepost called CP (K). Only used for resistive coils 
  !! to compute the resisitive heating. Must be an iteration variable for 
  !! ST (`itart=1`) (`iteration variable 20`)

  real(dp) :: tcpav2 = 0.0D0
  !! Computed centrepost average temperature (K) (for consistency)

  real(dp) :: tlegav = -1.0D0 
  !! Average temperature of the TF outboard legs [K]. If `tlegav=-1.0`, the ouboard 
  !! legs and CP temperatures are the same. Fixed for now, should use a contraints eq like tcpav 

  real(dp) :: tcpmax = 0.0D0
  !! peak centrepost temperature (K)
  
  real(dp) :: vcool = 20.0D0
  !! max centrepost coolant flow speed at midplane (m/s) (`iteration variable 70`)

  real(dp) :: vol_cond_cp = 0.0D0
  !! Exact conductor volume in the centrepost (m3)
  
  real(dp) :: whtcp = 0.0D0
  !! mass of TF coil inboard legs (kg)

  real(dp) :: whttflgs = 0.0D0
  !! mass of the TF coil legs (kg)

end module tfcoil_variables