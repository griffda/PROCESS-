module build_variables
  !! author: J. Morris (UKAEA)
  !!
  !! Module containing global variables relating to the machine's radial and vertical build
  !!
  !!### References
  !!
  !! - AEA FUS 251: A User's Guide to the PROCESS Systems Code

  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  public

  real(8) :: aplasmin
  !! minimum minor radius (m)

  real(8) :: available_radial_space
  !! Minimal radial space between plasma and coils (m)

  real(8) :: blarea
  !! blanket total surface area (m2)

  real(8) :: blareaib
  !! inboard blanket surface area (m2)

  real(8) :: blareaob
  !! outboard blanket surface area (m2)

  real(8) :: blbmith
  !! inboard blanket box manifold thickness (m) (`blktmodel>0`)
  !#TODO: remove blktmodel and similar below

  real(8) :: blbmoth
  !! outboard blanket box manifold thickness (m) (`blktmodel>0`)

  real(8) :: blbpith
  !! inboard blanket base plate thickness (m) (`blktmodel>0`)

  real(8) :: blbpoth
  !! outboard blanket base plate thickness (m) (`blktmodel>0`)

  real(8) :: blbuith
  !! inboard blanket breeding zone thickness (m) (`blktmodel>0`) (`iteration variable 90`)

  real(8) :: blbuoth
  !! outboard blanket breeding zone thickness (m) (`blktmodel>0`) (`iteration variable 91`)

  real(8) :: blnkith
  !! inboard blanket thickness (m); (calculated if `blktmodel>0`) (=0.0 if `iblnkith=0`)

  real(8) :: blnkoth
  !! outboard blanket thickness (m); calculated if `blktmodel>0`

  real(8) :: blnktth
  !! top blanket thickness (m), = mean of inboard and outboard blanket thicknesses

  real(8) :: bore
  !! central solenoid inboard radius (m) (`iteration variable 29`)

  real(8) :: clhsf
  !! cryostat lid height scaling factor (tokamaks)

  real(8) :: ddwex
  !! cryostat thickness (m)

  real(8) :: d_vv_in
  !! vacuum vessel inboard thickness (TF coil / shield) (m)

  real(8) :: d_vv_out
  !! vacuum vessel outboard thickness (TF coil / shield) (m)

  real(8) :: d_vv_top
  !! vacuum vessel topside thickness (TF coil / shield) (m) (= d_vv_bot if double-null)

  real(8) :: d_vv_bot
  !! vacuum vessel underside thickness (TF coil / shield) (m)

  real(8) :: f_avspace
  !! F-value for stellarator radial space check (`constraint equation 83`)

  real(8) :: fcspc
  !! Fraction of space occupied by CS pre-compression structure

  real(8) :: fmsbc
  !! Martensitic fraction of steel in (non-existent!) bucking cylinder

  real(8) :: fmsbl
  !! Martensitic fraction of steel in blanket

  real(8) :: fmsdwe
  !! Martensitic fraction of steel in cryostat

  real(8) :: fmsdwi
  !! Martensitic fraction of steel in vacuum vessel

  real(8) :: fmsfw
  !! Martensitic fraction of steel in first wall

  real(8) :: fmsoh
  !! Martensitic fraction of steel in central solenoid

  real(8) :: fmssh
  !! Martensitic fraction of steel in shield

  real(8) :: fmstf
  !! Martensitic fraction of steel in TF coil

  real(8) :: fseppc
  !! Separation force in CS coil pre-compression structure

  real(8) :: fwarea
  !! first wall total surface area (m2)

  real(8) :: fwareaib
  !! inboard first wall surface area (m2)

  real(8) :: fwareaob
  !! outboard first wall surface area (m2)

  real(8) :: fwith
  !! inboard first wall thickness, initial estimate as calculated (m)

  real(8) :: fwoth
  !! outboard first wall thickness, initial estimate as calculated (m)

  real(8) :: gapds
  !! gap between inboard vacuum vessel and thermal shield (m) (`iteration variable 61`)

  real(8) :: gapoh
  !! gap between central solenoid and TF coil (m) (`iteration variable 42`)

  real(8) :: gapomin
  !! minimum gap between outboard vacuum vessel and TF coil (m) (`iteration variable 31`)

  real(8) :: gapsto
  !! gap between outboard vacuum vessel and TF coil (m)

  real(8) :: hmax
  !! maximum (half-)height of TF coil (inside edge) (m)

  real(8) :: hpfdif
  !! difference in distance from midplane of upper and lower portions of TF
  !! legs (non-zero for single-null devices) (m)

  real(8) :: hpfu
  !! height to top of (upper) TF coil leg (m)

  real(8) :: hr1
  !! half-height of TF coil inboard leg straight section (m)

  integer :: iohcl
  !! Switch for existence of central solenoid:
  !!
  !! - =0 central solenoid not present
  !! - =1 central solenoid exists

  integer :: iprecomp
  !! Switch for existence of central solenoid pre-compression structure:
  !!
  !! - =0 no pre-compression structure
  !! - =1 calculated pre-compression structure

  real(8) :: ohcth
  !! Central solenoid thickness (m) (`iteration variable 16`)

  real(8) :: precomp
  !! CS coil precompression structure thickness (m)

  real(8) :: rbld
  !! sum of thicknesses to the major radius (m)

  real(8) :: required_radial_space
  !! Required space between coil and plasma for blanket shield wall etc (m)

  real(8) :: rinboard
  !! plasma inboard radius (m) (`consistency equation 29`)

  real(8) :: rsldi
  !! radius to inboard shield (inside point) (m)

  real(8) :: rsldo
  !! radius to outboard shield (outside point) (m)

  real(8) :: r_vv_inboard_out
  !! Radial plasma facing side position of inboard vacuum vessel [m]

  real(8) :: r_sh_inboard_in
  !! Radial inner side position of inboard neutronic shield [m]

  real(8) :: r_sh_inboard_out
  !! Radial plasma facing side position of inboard neutronic shield [m]

  real(8) :: r_tf_inboard_in
 	!! Mid-plane inboard TF coil leg radius at the centre-machine side [m]

  real(8) :: r_tf_inboard_mid
  !! Mid-plane inboard TF coil leg radius at middle of the coil [m]

  real(8) :: r_tf_inboard_out
  !! Mid-plane inboard TF coil leg radius at the plasma side [m]

  real(8) :: r_tf_outboard_mid
  !! Mid-plane outboard TF coil leg radius at the middle of the coil [m]

  integer :: i_r_cp_top
  !! Switch selecting the he parametrization of the outer radius of the top of the CP part of the TF coil
  !!  0 : `r_cp_top` is set by the plasma shape
  !!  1 : `r_cp_top` is a user input
  !!  2 : `r_cp_top` is set using the CP top and midplane CP radius ratio 

  real(8) :: r_cp_top
  !! Top outer radius of the centropost (ST only) (m)

  real(8) :: f_r_cp
  !! Ratio between the top and the midplane TF CP outer radius [-] 
  !! Not used by default (-1) must be larger than 1 otherwise

  real(8) :: dr_tf_inner_bore
  !! TF coil horizontal inner bore (m)

  real(8) :: dh_tf_inner_bore
  !! TF coil vertical inner bore (m)

  real(8) :: scrapli
  !! Gap between plasma and first wall, inboard side (m) (if `iscrp=1`) 
  !! Iteration variable: ixc = 73
  !! Scan variable: nsweep = 58

  real(8) :: scraplo
  !! Gap between plasma and first wall, outboard side (m) (if `iscrp=1`)
  !! Iteration variable: ixc = 74
  !! Scan variable: nsweep = 59

  real(8) :: sharea
  !! shield total surface area (m2)

  real(8) :: shareaib
  !! inboard shield surface area (m2)

  real(8) :: shareaob
  !! outboard shield surface area (m2)

  real(8) :: shldith
  !! inboard shield thickness (m) (`iteration variable 93`)

  real(8) :: shldlth
  !! lower (under divertor) shield thickness (m)

  real(8) :: shldoth
  !! outboard shield thickness (m) (`iteration variable 94`)

  real(8) :: shldtth
  !! upper/lower shield thickness (m); calculated if `blktmodel > 0` (= shldlth if double-null)

  real(8) :: sigallpc
  !! allowable stress in CSpre-compression structure (Pa)

  !#TODO: Issue #514 Make tfcth an output not an iteration variable
  real(8) :: tfcth
  !! inboard TF coil thickness, (centrepost for ST) (m)
  !! (input, calculated or `iteration variable 13`)

  real(8) :: tfoffset
  !! vertical distance between centre of TF coils and centre of plasma (m)

  real(8) :: tfootfi
  !! TF coil outboard leg / inboard leg radial thickness
  !! ratio (`i_tf_sup=0` only) (`iteration variable 75`)

  real(8) :: tfthko
  !! Outboard TF coil thickness (m)

  real(8) :: tftsgap
  !! Minimum metal-to-metal gap between TF coil and thermal shield (m)

  real(8) :: thshield
  !! TF-VV thermal shield thickness (m)

  real(8) :: vgap2
  !! vertical gap between vacuum vessel and thermal shields (m)

  real(8) :: vgap
  !! vertical gap between x-point and divertor (m) (if = 0, it is calculated)

  real(8) :: vgaptop
  !! vertical gap between top of plasma and first wall (m) (= vgap if double-null)

  real(8) :: vvblgap
  !! gap between vacuum vessel and blanket (m)

  real(8) :: plleni
  !! length of inboard divertor plate (m)

  real(8) :: plleno
  !! length of outboard divertor plate (m)

  real(8) :: plsepi
  !! poloidal length, x-point to inboard strike point (m)

  real(8) :: plsepo
  !! poloidal length, x-point to outboard strike point (m)

  real(8) :: rspo
  !! outboard strike point radius (m)

  contains

  subroutine init_build_variables
    !! Initialise module variables
    implicit none

    aplasmin = 0.25D0
    available_radial_space = 0.0D0
    blarea = 0.0D0
    blareaib = 0.0D0
    blareaob = 0.0D0
    blbmith = 0.17D0
    blbmoth = 0.27D0
    blbpith = 0.30D0
    blbpoth = 0.35D0
    blbuith = 0.365D0
    blbuoth = 0.465D0
    blnkith = 0.115D0
    blnkoth = 0.235D0
    blnktth = 0.0D0
    bore = 1.42D0
    clhsf = 4.268D0
    ddwex = 0.07D0
    d_vv_in = 0.07D0
    d_vv_out = 0.07D0
    d_vv_top = 0.07D0
    d_vv_bot = 0.07D0
    f_avspace = 1.0D0
    fcspc = 0.6D0
    fmsbc = 0.0D0
    fmsbl = 0.0D0
    fmsdwe = 0.0D0
    fmsdwi = 0.0D0
    fmsfw = 0.0D0
    fmsoh = 0.0D0
    fmssh = 0.0D0
    fmstf = 0.0D0
    fseppc = 3.5D8
    fwarea = 0.0D0
    fwareaib = 0.0D0
    fwareaob = 0.0D0
    fwith = 0.0D0
    fwoth = 0.0D0
    gapds = 0.155D0
    gapoh = 0.08D0
    gapomin = 0.234D0
    gapsto = 0.0D0
    hmax = 0.0D0
    hpfdif = 0.0D0
    hpfu = 0.0D0
    hr1 = 0.0D0
    iohcl = 1
    iprecomp = 1
    ohcth = 0.811D0
    precomp = 0.0D0
    rbld = 0.0D0
    required_radial_space = 0.0D0
    rinboard = 0.651D0
    rsldi = 0.0D0
    rsldo = 0.0D0
    r_vv_inboard_out = 0.0D0
    r_sh_inboard_out = 0.0D0
    r_tf_inboard_in = 0.0D0
    r_tf_inboard_mid = 0.0D0
    r_tf_inboard_out = 0.0D0
    r_tf_outboard_mid = 0.0D0
    i_r_cp_top = 0
    r_cp_top = 0.0D0
    f_r_cp = 1.4D0
    dr_tf_inner_bore = 0.0D0
    dh_tf_inner_bore = 0.0D0
    scrapli = 0.14D0
    scraplo = 0.15D0
    sharea = 0.0D0
    shareaib = 0.0D0
    shareaob = 0.0D0
    shldith = 0.69D0
    shldlth = 0.7D0
    shldoth = 1.05D0
    shldtth = 0.6D0
    sigallpc = 3.0D8
    tfcth = 0.0D0
    tfoffset = 0.0D0
    tfootfi = 1.19D0
    tfthko = 0.0D0
    tftsgap = 0.05D0
    thshield = 0.05D0
    vgap2 = 0.163D0
    vgap= 0.0D0
    vgaptop = 0.60D0
    vvblgap = 0.05D0
    plleni = 1.0D0
    plleno = 1.0D0
    plsepi = 1.0D0
    plsepo = 1.5D0
    rspo = 0.0D0
    r_sh_inboard_in = 0.0D0
  end subroutine init_build_variables
end module build_variables