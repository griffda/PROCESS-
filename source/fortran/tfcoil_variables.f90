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

  real(8) :: acasetf
  !! external case area per coil (inboard leg) (m2)

  real(8) :: acasetfo
  !! external case area per coil (outboard leg) (m2)

  real(8) :: acndttf
  !! area of the cable conduit (m2)

  real(8) :: acond
  !! conductor area (winding pack) (m2)

  real(8) :: acstf
  !! internal area of the cable space (m2)

  real(8) :: insulation_area
  !! single turn insulation area (m2)

  real(8) :: aiwp
  !! winding pack turn insulation area per coil (m2)

  real(8) :: sig_tf_case_max
  !! Allowable maximum shear stress in TF coil case (Tresca criterion) (Pa)

  real(8) :: sig_tf_wp_max
  !! Allowable maximum shear stress in TF coil conduit (Tresca criterion) (Pa)

  ! TODO remove below IF not needed 
  ! real(8) :: alstrtf
  !! Allowable Tresca stress in TF coil structural material (Pa)

  real(8) :: arealeg
  !! outboard TF leg area (m2)

  real(8) :: aswp
  !! winding pack structure area (m2)

  real(8) :: avwp
  !! winding pack void (He coolant) area (m2)

  real(8) :: awphec
  !! winding pack He coil area (m2)

  real(8) :: bcritsc
  !! upper critical field (T) for Nb3Sn superconductor at zero temperature and 
  !! strain (`i_tf_sc_mat=4, =bc20m`)

  real(8) :: bmaxtf
  !! mean peak field at TF coil (T)

  real(8) :: bmaxtfrp
  !! peak field at TF conductor with ripple (T)

  real(8) :: casestr
  !! case strain

  real(8) :: casthi
  !! inboard TF coil case plasma side thickness (m) (calculated for stellarators)

  real(8) :: casthi_fraction
  !! inboard TF coil case plasma side thickness as a fraction of tfcth

  logical :: casthi_is_fraction
  !! logical switch to make casthi a fraction of TF coil thickness (`casthi_fraction`)

  real(8) :: casths
  !! inboard TF coil sidewall case thickness (m) (calculated for stellarators)

  real(8) :: casths_fraction
  !! inboard TF coil sidewall case thickness as a fraction of tftort

  logical :: tfc_sidewall_is_fraction
  !! logical switch to make casths a fraction of TF coil thickness (`casths_fraction`)

  real(8) :: t_conductor
  !! Conductor (cable + steel conduit) area averaged dimension [m]
  
  real(8) :: t_turn_tf
  !! TF coil turn edge length including turn insulation [m]
  !!   If the turn is not a square (i_tf_turns_integer = 1) a squared turn of 
  !!   equivelent size is use to calculated this quantity
  !!   If the t_turn_tf is non zero, cpttf is calculated

  logical :: t_turn_tf_is_input
  !! Boolean switch to activated when the user set the TF coil turn dimensions
  !! Not an input

  real(8) :: f_t_turn_tf
  !! f-value for TF turn edge length constraint 
  !!  If the turn is not a square (i_tf_turns_integer = 1) a squared turn of 
  !!  equivelent size is use for this constraint
  !!  iteration variable ixc = 175
  !!  constraint equation icc = 86

  real(8) :: t_turn_tf_max
  !! TF turn edge length including turn insulation upper limit [m] 
  !! If the turn is not a square (i_tf_turns_integer = 1) a squared turn of 
  !! equivelent size is use for this constraint
  !! constraint equation icc = 86

  real(8) :: t_cable_tf
  !! TF coil superconducting cable squared/rounded dimensions [m]
  !!   If the turn is not a square (i_tf_turns_integer = 1) a squared cable of 
  !!   equivelent size is use to calculated this quantity
  !!   If the t_cable_tf is non zero, cpttf is calculated

  logical :: t_cable_tf_is_input
  !! Boolean switch to activated when the user set the TF coil cable dimensions
  !! Not an input

  real(8) :: acs
  !! Area of space inside conductor (m2)

  real(8) :: cdtfleg
  !! TF outboard leg current density (A/m2) (resistive coils only)
  
  real(8) :: cforce
  !! centering force on inboard leg (per coil) (N/m)

  real(8) :: cpttf
  !! TF coil current per turn (A). (calculated for stellarators) (calculated for 
  !! integer-turn TF coils `i_tf_turns_integer=1`) (`iteration variable 60`)

  real(8) :: cpttf_max
  !! Max TF coil current per turn [A]. (for stellarators and `i_tf_turns_integer=1`) 
  !! (`constraint equation 77`)

  real(8) :: dcase
  !! density of coil case (kg/m3)

  real(8), dimension(9) :: dcond
  !! density of superconductor type given by i_tf_sc_mat/isumatoh/isumatpf (kg/m3)
  
  real(8) :: dcondins
  !! density of conduit + ground-wall insulation (kg/m3)

  real(8) :: dhecoil
  !! diameter of He coil in TF winding (m)

  real(8) :: estotftgj
  !! total stored energy in the toroidal field (GJ)

  real(8) :: farc4tf
  !! factor to size height of point 4 on TF coil
  real(kind(1.0D0)) :: b_crit_upper_nbti
  !! upper critical field of GL_nbti
  real(kind(1.0D0)) :: t_crit_nbti
  !! critical temperature of GL_nbti
  real(kind(1.0D0)) :: max_force_density
  !! Maximal (WP averaged) force density in TF coils at 1 point. (MN/m3)
  real(kind(1.0D0)) :: fcutfsu
  !! copper fraction of cable conductor (TF coils)
  !! (iteration variable 59)
  real(8) :: fhts
  !! technology adjustment factor for critical current density fit for isumat..=2 
  !! Bi-2212 superconductor, to describe the level of technology assumed (i.e. to 
  !! account for stress, fatigue, radiation, AC losses, joints or manufacturing 
  !! variations; 1.0 would be very optimistic)
  
  real(8) :: hts_tape_width
  !! Width of HTS tape [m] (if i_tf_sc_mat = 9)

  real(8) :: hts_tape_thickness
  !! Thickness of HTS tape layer [m] (if i_tf_sc_mat = 9)

  real(8) :: insstrain
  !! Radial strain in insulator

  integer :: i_tf_plane_stress
  !! Switch for the TF coil stress model
  !!   0 : New generalized plane strain formulation 
  !!   1 : Old plane stress model (only for SC)

  integer :: i_tf_tresca
  !! Switch for TF coil conduit Tresca stress criterion:
  !!   0 : Tresca (no adjustment);
  !!   1 : Tresca with CEA adjustment factors (radial+2%, vertical+60%) </UL>
  
  integer :: i_tf_wp_geom
  !! Switch for TF WP geometry selection
  !!   0 : Rectangular geometry 
  !!   1 : Double rectangular geometry 
  !!   2 : Trapezoidal geometry (constant lateral casing thickness)
  !! Default setting for backward compatibility 
  !!   if i_tf_turns_integer = 0 : Double rectangular
  !!   if i_tf_turns_integer = 1 : Rectangular 

  integer :: i_tf_case_geom
  !! Switch for TF case geometry selection
  !!   0 : Circular front case (ITER design)
  !!   1 : Straight front case

  integer :: i_tf_turns_integer
  !! Switch for TF coil integer/non-integer turns:
  !!   0 : non-integer turns
  !!   1 : integer turns

  integer :: i_tf_sc_mat
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
  !! - =7 Durham Ginzburg-Landau critical surface model for Nb-Ti
  !! - =8 Durham Ginzburg-Landau critical surface model for REBCO
  !! - =9 Hazelton experimental data + Zhai conceptual model for REBCO

  integer :: i_tf_sup
  !! Switch for TF coil conductor model:
  !!
  !! - =0 copper
  !! - =1 superconductor
  !! - =2 Cryogenic aluminium

  integer :: i_tf_shape
  !! Switch for TF coil toroidal shape:
  !!
  !! - =0  Default value : Picture frame coil for TART / PROCESS D-shape for non itart
  !! - =1  PROCESS D-shape : parametrise with 2 arcs 
  !! - =2  Picture frame coils 

  integer :: n_pancake
  !! Number of pancakes in TF coil. Only used if `i_tf_turns_integer=1`

  integer :: n_layer
  !! Number of layers in TF coil. Only used if `i_tf_turns_integer=1`
  
  integer :: n_rad_per_layer
  !! Size of the arrays per layers storing the radial dependent stress 
  !! quantities (stresses, strain displacement etc..)

  integer :: i_tf_bucking
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

  integer :: n_tf_graded_layers
  !! Number of layers of different stress properties in the WP. If `n_tf_graded_layers > 1`, 
  !! a graded coil is condidered

  integer :: n_tf_stress_layers
  !! Number of layers considered for the inboard TF stress calculations
  !! set in initial.f90 from i_tf_bucking and n_tf_graded_layers

  real(8) :: jbus
  !! bussing current density (A/m2)
  
  real(8) :: jwdgcrt
  !! critical current density for winding pack (A/m2)

  real(8) :: jwdgpro
  !! allowable TF coil winding pack current density, for dump temperature rise protection (A/m2)

  real(8) :: jwptf
  !! winding pack engineering current density (A/m2)

  real(8) :: oacdcp
  !! Overall current density in TF coil inboard legs midplane (A/m2)
  !! Rem SK : Not used in tfcoil to set the current any more. Should not be used as
  !! iteration variable 12 any more. It is now calculated.

  real(8) :: eyzwp
  !! Winding pack vertical Young's modulus (Pa)

  real(8) :: eyoung_ins
  !! Insulator Young's modulus [Pa]. Default value (1.0D8) setup the following values
  !!  - SC TF, eyoung_ins = 20 Gpa (default value from DDD11-2 v2 2 (2009))
  !!  - Al TF, eyoung_ins = 2.5 GPa (Kapton polymer)

  real(8) :: eyoung_steel
  !! Steel case Young's modulus (Pa) (default value from DDD11-2 v2 2 (2009))

  real(8) :: eyoung_winding
  !! SC TF coil winding Young's modulus (Pa)
  
  real(8) :: eyoung_res_tf_buck
  !! Resistive TF magnets bucking cylinder young modulus (Pa)

  real(8) :: eyoung_copper
  !! Copper young modulus. Default value taken from wikipedia

  real(8) :: eyoung_al
  !! Aluminium young modulus.  Default value taken from wikipedia
  
  real(8) :: poisson_steel
  !! Steel Poisson's ratio 
  
  real(8):: poisson_copper
  !! Copper Poisson's ratio. Source : https://www.engineeringtoolbox.com/poissons-ratio-d_1224.html

  real(8):: poisson_al
  !! Aluminium Poisson's ratio. 
  !! Source : https://www.engineeringtoolbox.com/poissons-ratio-d_1224.html

  real(8) :: rbmax
  !! Radius of maximum TF B-field (m)

  real(8) :: tflegres
  !! TF coil leg resistance (ohm)

  real(8) :: toroidalgap
  !! Minimal distance between two toroidal coils. (m)

  real(8) :: ftoroidalgap
  !! F-value for minimum tftort (`constraint equation 82`)

  real(8) :: ripmax
  !! aximum allowable toroidal field ripple amplitude at plasma edge (%)

  real(8) :: ripple
  !! peak/average toroidal field ripple at plasma edge (%)

  real(8) :: ritfc
  !! total (summed) current in TF coils (A)
  
  integer, parameter :: n_radial_array = 50
  !! Size of the radial distribution arrays per layers
  !! used for stress, strain and displacement distibution

  real(8), dimension(2*n_radial_array) :: radial_array
  !! Array refining the radii of the stress calculations arrays

  real(8), dimension(2*n_radial_array) :: sig_tf_r
  !! TF Inboard leg radial stress in steel r distribution at mid-plane [Pa]
  
  real(8), dimension(2*n_radial_array) :: sig_tf_t
  !! TF Inboard leg tangential stress in steel r distribution at mid-plane [Pa]
  
  real(8), dimension(2*n_radial_array) :: deflect
  !! TF coil radial deflection (displacement) radial distribution [m]

  real(8) :: sig_tf_z
  !! TF Inboard leg vertical tensile stress in steel at mid-plane [Pa]
    
  real(8), dimension(2*n_radial_array) :: sig_tf_vmises
  !! TF Inboard leg Von-Mises stress in steel r distribution at mid-plane [Pa]
      
  real(8), dimension(2*n_radial_array) :: sig_tf_tresca
  !! TF Inboard leg TRESCA stress in steel r distribution at mid-plane [Pa]

  real(8) :: sig_tf_cs_bucked

  ! TODO is this needed?
  ! real(8) :: strtf0
  !! Maximum TRESCA stress in CS structures at CS flux swing [Pa]:
  !!
  !!  - If superconducting CS (ipfres = 0): turn steel conduits TRESCA stress
  !!  - If resistive       CS (ipfres = 1): copper conductor TRESCA stress 
  !!
  !! Quantity only computed for bucked and wedged design (`i_tf_bucking >= 2`)
  !! Def : CS Flux swing, instant when the current changes sign in CS (null current) 

  real(8) :: sig_tf_case
  !! Maximum TRESCA stress in TF casing steel structures (Pa)
  
  real(8) :: sig_tf_wp

  ! TODO is this needed?
  ! real(8) :: strtf1
  ! !! Maximum TRESCA stress in TF casing steel structures (Pa)
  
  ! real(8) :: strtf2
  ! !! Maximum TRESCA stress in TF WP conduit steel structures (Pa)
  ! !! This is the TF stress condition used in the case of stellarators
  
  real(8) :: sigvvall
  !! allowable stress from TF quench in vacuum vessel (Pa)

  real(8) :: strncon_cs
  !! strain in CS superconductor material (used in Nb3Sn critical surface model `isumatoh=1,4,5`)

  real(8) :: strncon_pf
  !! strain in PF superconductor material (used in Nb3Sn critical surface model `isumatph=1,4,5`)

  real(8) :: strncon_tf
  !! strain in TF superconductor material (used in Nb3Sn critical surface model `i_tf_sc_mat=1,4,5`)

  character(len=12) :: quench_model
  !! switch for TF coil quench model (Only applies to REBCO magnet at present, issue #522):
  !!
  !! - ='exponential' exponential quench with constant discharge resistor
  !! - ='linear' quench with constant voltage

  real(8) :: quench_detection_ef
  !! Electric field at which TF quench is detected and discharge begins (V/m)

  real(8) :: time1
  !! Time at which TF quench is detected (s)

  real(8) :: taucq
  !! allowable TF quench time (s)

  real(8) :: tcritsc
  !! critical temperature (K) for superconductor at zero field and strain (`i_tf_sc_mat=4, =tc0m`)

  real(8) :: tdmptf
  !! fast discharge time for TF coil in event of quench (s) (`iteration variable 56`)
  !!
  !! For REBCO model, meaning depends on quench_model:
  !!
  !! - exponential quench : e-folding time (s)`
  !! - linear quench : discharge time (s)

  real(8) :: tfareain
  !! Area of inboard midplane TF legs (m2)

  real(8) :: tfbusl
  !! TF coil bus length (m)

  real(8) :: tfbusmas
  !! TF coil bus mass (kg)

  real(8) :: tfckw
  !! available DC power for charging the TF coils (kW)

  !#TODO: issue #781
  ! integer :: tfc_model
  ! !! tfc_model /1/ : switch for TF coil magnet stress model:<UL>
  ! !!                 <LI> = 0 simple model (solid copper coil)
  ! !!                 <LI> = 1 CCFE two-layer stress model; superconductor</UL>

  real(8) :: tfcmw
  !! Peak power per TF power supply (MW)
  
  real(8) :: tfcpmw
  !! Peak resistive TF coil inboard leg power (MW)

  real(8) :: tfjtsmw
  !! TF joints resistive power losses (MW)

  real(8) :: tfcryoarea
  !! surface area of toroidal shells covering TF coils (m2)

  real(8) :: tficrn
  !! TF coil half-width - inner bore (m)

  real(8) :: tfind
  !! TF coil inductance (H)

  real(8) :: tfinsgap
  !! TF coil WP insertion gap (m)
  
  real(8) :: tflegmw
  !! TF coil outboard leg resistive power (MW)

  real(8) :: rhocp
  !! TF coil inboard leg resistivity [Ohm-m]. If `itart=0`, this variable is the 
  !! average resistivity over the whole magnet

  real(8) :: rhotfleg
  !! Resistivity of a TF coil leg (Ohm-m)

  real(8) :: rhotfbus
  !! Resistivity of a TF coil bus (Ohm-m). Default value takes the same res as the leg one

  real(8) :: frhocp
  !! Centrepost resistivity enhancement factor. For `itart=0`, this factor 
  !! is used for the whole magnet 
  
  real(8) :: frholeg
  !! Ouboard legs resistivity enhancement factor. Only used for `itart=1`.
  
  integer :: i_cp_joints
  !! Switch for CP demoutable joints type
  !!  -= 0 : Clampled joints
  !!  -= 1 : Sliding joints
  !! Default value (-1) choses : 
  !!   Sliding joints for resistive magnets (i_tf_sup = 0, 2)  
  !!   Clampled joints for superconducting magents (i_tf_sup = 1)

  real(8) :: rho_tf_joints
  !! TF joints surfacic resistivity [ohm.m]. Feldmetal joints assumed.

  integer :: n_tf_joints_contact
  !! Number of contact per turn

  integer :: n_tf_joints
  !! Number of joints
  !! Ex: n_tf_joints = 2 for top and bottom CP joints

  real(8) :: th_joint_contact
  !! TF sliding joints contact pad width [m]

  real(8) :: pres_joints
  !! Calculated TF joints resistive power losses [W]

  real(8) :: tfleng
  !! TF coil circumference (m)

  real(8) :: eff_tf_cryo
  !! TF cryoplant efficiency (compared to pefect Carnot cycle).
  !! Using -1 set the default value depending on magnet technology:
  !!
  !!  - i_tf_sup = 1 : SC magnet, eff_tf_cryo = 0.13 (ITER design)
  !!  - i_tf_sup = 2 : Cryo-aluminium, eff_tf_cryo = 0.4

  real(8) :: n_tf
  !! Number of TF coils (default = 50 for stellarators). Number of TF coils outer legs for ST

  real(8) :: tfocrn
  !! TF coil half-width - outer bore (m)

  real(8) :: tfsai
  !! area of the inboard TF coil legs (m2)

  real(8) :: tfsao
  !! area of the outboard TF coil legs (m2)

  real(8) :: tftmp
  !! peak helium coolant temperature in TF coils and PF coils (K)

  real(8) :: tftort
  !! TF coil toroidal thickness (m)

  real(8) :: thicndut
  !! conduit insulation thickness (m)

  real(8) :: layer_ins
  !! Additional insulation thickness between layers (m)

  real(8) :: thkcas
  !! inboard TF coil case outer (non-plasma side) thickness (m) (`iteration variable 57`)
  !! (calculated for stellarators)

  real(8) :: dr_tf_wp
  !! radial thickness of winding pack (m) (`iteration variable 140`) (issue #514)

  real(8) :: thwcndut
  !! TF coil conduit case thickness (m) (`iteration variable 58`)
  
  real(8) :: tinstf
  !! Thickness of the ground insulation layer surrounding (m) 
  !!   - Superconductor TF (`i_tf_sup == 1`) : The TF coil Winding packs
  !!   - Resistive magnets (`i_tf_sup /= 1`) : The TF coil wedges
  !! Rem : Thickness calculated for stellarators.

  real(8) :: tmargmin_tf
  !! minimum allowable temperature margin : TF coils (K)

  real(8) :: tmargmin_cs
  !! minimum allowable temperature margin : CS (K)

  real(8) :: tmargmin
  !! minimum allowable temperature margin : TFC AND CS (K)

  real(8) :: temp_margin
  !! temperature margin (K)

  real(8) :: tmargtf
  !! TF coil temperature margin (K)

  real(8) :: tmaxpro
  !! maximum temp rise during a quench for protection (K)

  real(8) :: tmax_croco
  !! CroCo strand: maximum permitted temp during a quench (K)

  real(8) :: croco_quench_temperature
  !! CroCo strand: Actual temp reached during a quench (K)

  real(8) :: tmpcry
  !! coil temperature for cryogenic plant power calculation (K)

  real(8) :: n_tf_turn
  !! number of turns per TF coil

  real(8) :: vdalw
  !! max voltage across TF coil during quench (kV) (`iteration variable 52`)

  real(8) :: vforce
  !! vertical tension on inboard leg/coil (N)
  
  real(8) :: f_vforce_inboard
  !! Fraction of the total vertical force taken by the TF inboard leg tension
  !! Not used for resistive `itart=1` (sliding joints)

  real(8) :: vforce_outboard
  !! Vertical tension on outboard leg/coil (N)

  real(8) :: vftf
  !! coolant fraction of TFC 'cable' (`i_tf_sup=1`), or of TFC leg (`i_tf_ssup=0`)

  real(8) :: voltfleg
  !! volume of each TF coil outboard leg (m3)

  real(8) :: vtfkv
  !! TF coil voltage for resistive coil including bus (kV)

  real(8) :: vtfskv
  !! voltage across a TF coil during quench (kV)

  real(8) :: whtcas
  !! mass per coil of external case (kg)

  real(8) :: whtcon
  !! TF coil conductor mass per coil (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf

  real(8) :: whtconcu
  !! copper mass in TF coil conductor (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf
  
  real(8) :: whtconal
  !! Aluminium mass in TF coil conductor (kg/coil).
  !! For `itart=1`, coil is return limb plus centrepost/n_tf
  
  real(8) :: whtconin
  !! conduit insulation mass in TF coil conductor (kg/coil)

  real(8) :: whtconsc
  !! superconductor mass in TF coil cable (kg/coil)

  real(8) :: whtconsh
  !! steel conduit mass in TF coil conductor (kg/coil)

  real(8) :: whtgw
  !! mass of ground-wall insulation layer per coil (kg/coil)

  real(8) :: whttf
  !! total mass of the TF coils (kg)

  real(8) :: windstrain
  !! longitudinal strain in winding pack

  real(8) :: wwp1
  !! width of first step of winding pack (m)

  real(8) :: wwp2
  !! width of second step of winding pack (m)

  ! Superconducting TF coil shape parameters</B> (see also farc4tf);
  ! the TF inner surface top half is approximated by four circular arcs.
  ! Arc 1 goes through points 1 and 2 on the inner surface. Arc 2
  ! goes through points 2 and 3, etc.
  real(8), dimension(4) :: dthet
  !! angle of arc i (rad)

  real(8), dimension(4) :: radctf
  !! radius of arc i (m)

  real(8), dimension(5) :: xarc
  !! x location of arc point i on surface (m)

  real(8), dimension(4) :: xctfc
  !! x location of arc centre i (m)

  real(8), dimension(5) :: yarc
  !! y location of arc point i on surface (m)

  real(8), dimension(4) :: yctfc
  !! y location of arc centre i (m)

  ! New TF shape:  Horizontal and vertical radii of inside edge of TF coil
  ! Arcs are numbered clockwise:
  ! 1=upper inboard, 2=upper outboard, 3=lower ouboard, 4=lower inboard

  real(8), dimension(4) :: tfa
  !! Horizontal radius of inside edge of TF coil (m)

  real(8), dimension(4) :: tfb
  !! Vertical radius of inside edge of TF coil (m)
  ! Quantities relating to the spherical tokamak model (itart=1)
  ! (and in some cases, also to resistive TF coils, i_tf_sup=0):

  real(8) :: drtop
  !! centrepost taper maximum radius adjustment (m)

  real(8) :: dztop
  !! centrepost taper height adjustment (m)

  real(8) :: etapump
  !! centrepost coolant pump efficiency

  real(8) :: fcoolcp
  !! coolant fraction of TF coil inboard legs (`iteration variable 23`)

  real(8) :: fcoolleg
  !! coolant fraction of TF coil outboard legs
  
  real(8) :: a_cp_cool
  !! Centrepost cooling area toroidal cross-section (constant over the whole CP)

  real(8) :: ncool
  !! number of centrepost coolant tubes

  real(8) :: ppump
  !! centrepost coolant pump power (W)

  real(8) :: prescp
  !! resistive power in the centrepost (itart=1) [W].
  !! If `itart=0`, this variable is the ressitive power on the whole magnet

  real(8) :: presleg
  !! Summed resistive power in the TF coil legs [W]. Remain 0 if `itart=0`.
  
  real(8) :: ptempalw
  !! maximum peak centrepost temperature (K) (`constraint equation 44`)

  real(8) :: rcool
  !! average radius of coolant channel (m) (`iteration variable 69`)

  real(8) :: tcoolin
  !! centrepost coolant inlet temperature (K)

  real(8) :: dtiocool
  !! inlet / outlet TF coil coolant temperature rise (K)  

  real(8) :: tcpav
  !! Average temperature of centrepost called CP (K). Only used for resistive coils 
  !! to compute the resisitive heating. Must be an iteration variable for 
  !! ST (`itart=1`) (`iteration variable 20`)

  real(8) :: tcpav2
  !! Computed centrepost average temperature (K) (for consistency)

  real(8) :: tlegav
  !! Average temperature of the TF outboard legs [K]. If `tlegav=-1.0`, the ouboard 
  !! legs and CP temperatures are the same. Fixed for now, should use a contraints eq like tcpav 

  real(8) :: tcpmax
  !! peak centrepost temperature (K)
  
  real(8) :: vcool
  !! inlet centrepost coolant flow speed at midplane (m/s) (`iteration variable 70`)

  real(8) :: vol_cond_cp
  !! Exact conductor volume in the centrepost (m3)
  
  real(8) :: whtcp
  !! mass of TF coil inboard legs (kg)

  real(8) :: whttflgs
  !! mass of the TF coil legs (kg)

  contains

  subroutine init_tfcoil_variables
    !! Initialise module variables
    implicit none

    acasetf = 0.0D0
    acasetfo = 0.0D0
    acndttf = 0.0D0
    acond = 0.0D0
    acstf = 0.0D0
    insulation_area = 0.0D0
    aiwp = 0.0D0
    sig_tf_case_max = 6.0D8
    sig_tf_wp_max = 6.0D8
    arealeg = 0.0D0
    aswp = 0.0D0
    avwp = 0.0D0
    awphec = 0.0D0
    bcritsc = 24.0D0
    bmaxtf = 0.0D0
    bmaxtfrp = 0.0D0
    casestr = 0.0D0
    casthi = 0.0D0
    casthi_fraction = 0.05D0
    casthi_is_fraction = .false.
    casths = 0.0D0
    casths_fraction = 0.06D0
    t_conductor = 0.0D0
    t_cable_tf = 0.0D0
    t_cable_tf_is_input = .false.
    t_turn_tf = 0.0D0
    t_turn_tf_is_input = .false.
    f_t_turn_tf = 1.0D0
    t_turn_tf_max = 0.05
    acs = 0.0D0
    cdtfleg = 0.0D0
    cforce = 0.0D0
    cpttf = 7.0e4
    cpttf_max = 9.0e4
    dcase = 8000.0D0
    dcond = (/6080.0D0, 6080.0D0, 6070.0D0, 6080.0D0, 6080.0D0, 8500.0D0, &
      6070.0D0, 8500.0D0/)
    dcondins = 1800.0D0
    dhecoil = 0.005D0
    estotftgj = 0.0D0
    farc4tf = 0.7D0
    b_crit_upper_nbti = 14.86D0
    t_crit_nbti = 9.04D0
    max_force_density = 0.0D0
    fcutfsu = 0.69D0
    fhts = 0.5D0
    hts_tape_width = 4.0D-3
    hts_tape_thickness = 1.0D-6
    insstrain = 0.0D0
    i_tf_plane_stress = 1
    i_tf_tresca = 0
    i_tf_wp_geom = -1
    i_tf_case_geom = 0
    i_tf_turns_integer = 0
    i_tf_sc_mat = 1
    i_tf_sup = 1
    i_tf_shape = 0
    n_pancake = 10
    n_layer = 20
    n_rad_per_layer = 100
    i_tf_bucking = -1
    n_tf_graded_layers = 1
    n_tf_stress_layers = 0
    jbus = 1.25D6
    jwdgcrt = 0.0D0
    jwdgpro = 0.0D0
    jwptf = 0.0D0
    oacdcp = 0.0D0
    eyzwp = 0.0D0
    eyoung_ins = 1.0D8
    eyoung_steel = 2.05D11
    eyoung_winding = 6.6D8
    eyoung_res_tf_buck = 150.0D9 
    eyoung_copper = 117.0D9
    eyoung_al = 69.0D9 
    poisson_steel = 0.3D0
    poisson_copper = 0.35D0
    poisson_al = 0.35D0
    rbmax = 0.0D0
    tflegres = 0.0D0
    toroidalgap = 1.0D0 ![m]
    ftoroidalgap = 1.0D0
    ripmax = 1.0D0
    ripple = 0.0D0
    ritfc = 0.0D0
    radial_array = 0.0D0
    sig_tf_r = 0.0D0
    sig_tf_t = 0.0D0
    deflect = 0.0D0
    sig_tf_z = 0.0D0
    sig_tf_vmises = 0.0D0
    sig_tf_tresca = 0.0D0 
    sig_tf_cs_bucked = 0.0D0
    sig_tf_case = 0.0D0
    sig_tf_wp = 0.0D0
    sigvvall = 9.3D7
    strncon_cs = -0.005D0
    strncon_pf = -0.005D0
    strncon_tf = -0.005D0
    quench_model = 'exponential'
    quench_detection_ef = 0D0
    time1 = 0D0
    taucq = 30.0D0
    tcritsc = 16.0D0
    tdmptf = 10.0D0
    tfareain = 0.0D0
    tfbusl = 0.0D0
    tfbusmas = 0.0D0
    tfckw = 0.0D0
    tfcmw = 0.0D0
    tfcpmw = 0.0D0
    tfjtsmw = 0.0D0
    tfcryoarea = 0.0D0
    tficrn = 0.0D0
    tfind = 0.0D0
    tfinsgap = 0.010D0
    tflegmw = 0.0D0
    rhocp = 0.0D0
    rhotfleg = 0.0D0
    rhotfbus = -1.0D0 ! 2.5D-8
    frhocp = 1.0D0
    frholeg = 1.0D0
    rho_tf_joints = 2.5D-10
    n_tf_joints_contact = 6
    n_tf_joints = 4
    th_joint_contact = 0.03D0
    pres_joints = 0.0D0
    tfleng = 0.0D0
    eff_tf_cryo = -1.0D0
    n_tf = 16.0D0
    tfocrn = 0.0D0
    tfsai = 0.0D0
    tfsao = 0.0D0
    tftmp = 4.5D0
    tftort = 1.0D0
    thicndut = 8.0D-4
    layer_ins = 0.0D0
    thkcas = 0.3D0
    dr_tf_wp = 0.0D0
    thwcndut = 8.0D-3
    tinstf = 0.018D0
    tmargmin_tf = 0D0
    tmargmin_cs = 0D0
    tmargmin = 0D0
    temp_margin = 0.00D0
    tmargtf = 0.0D0
    tmaxpro = 150.0D0
    tmax_croco = 200.0D0
    croco_quench_temperature = 0D0
    tmpcry = 4.5D0
    n_tf_turn = 0.0D0
    vdalw = 20.0D0
    vforce = 0.0D0
    f_vforce_inboard = 0.5D0
    vforce_outboard = 0.0D0
    vftf = 0.4D0
    voltfleg = 0.0D0
    vtfkv = 0.0D0
    vtfskv = 0.0D0
    whtcas = 0.0D0
    whtcon = 0.0D0
    whtconcu = 0.0D0
    whtconal = 0.0D0
    whtconin = 0.0D0
    whtconsc = 0.0D0
    whtconsh = 0.0D0
    whtgw = 0.0D0
    whttf = 0.0D0
    windstrain = 0.0D0
    wwp1 = 0.0D0
    wwp2 = 0.0D0
    dthet = 0.0D0
    radctf = 0.0D0
    xarc = 0.0D0
    xctfc = 0.0D0
    yarc = 0.0D0
    yctfc = 0.0D0
    tfa = 0.0D0
    tfb = 0.0D0
    drtop = 0.0D0
    dztop = 0.0D0
    etapump = 0.8D0
    fcoolcp = 0.3D0
    fcoolleg = 0.2D0
    a_cp_cool = 0.0D0
    ncool = 0.0D0
    ppump = 0.0D0
    prescp = 0.0D0
    presleg = 0.0D0
    ptempalw = 473.15D0   ! 200 C
    rcool = 0.005D0
    tcoolin = 313.15D0   ! 40 C
    dtiocool = 0.0D0
    tcpav = 373.15D0     ! 100 C
    tcpav2 = 0.0D0
    tlegav = -1.0D0 
    tcpmax = 0.0D0
    vcool = 20.0D0
    vol_cond_cp = 0.0D0
    whtcp = 0.0D0
    whttflgs = 0.0D0
    tfc_sidewall_is_fraction = .false.
    i_cp_joints = -1
  end subroutine init_tfcoil_variables
end module tfcoil_variables