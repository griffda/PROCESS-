module kit_hcll_module
  !! Module containing KIT HCLL blanket model
  !! author: J Morris, CCFE, Culham Science Centre
  !! N/A
  !! This module contains the PROCESS KIT HCLL blanket model
  !! based on KIT HCLL model developed by F. Franza
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Modules to import
  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  !  Subroutine declarations
  private
  public :: kit_hcll, init_kit_hcll_module

  !  Precision variable
  integer, parameter :: double = 8

  ! TODO - blanket thickness includes the first wall in Fabrizio's model
  ! real(dp) :: blnkith = 0.025D0 + 0.375D0 + 0.21D0
  ! real(dp) :: blnkoth = 0.025D0 + 0.715D0 + 0.21D0

  ! TODO - need checking of the bounds of validity

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Variables for output to file
  integer, private :: ip, ofile

  ! Blanket inlet/outlet He temperatures
  real(dp), private :: T_he_in, T_He_out

  ! Density of lead lithium (kg/m3)
  real(dp), private :: denpbli

  ! Density of helium (kg/m3 at 8MPa, 400C)
  real(dp), private :: denhe

  ! Coverage factor (%)
  real(dp), private :: cf

  ! Energy multiplication in blanket, VV and divertor
  real(dp), private :: emult_all

  ! TBR formula correction Factors
  real(dp), private :: ff_ib, ff_ob

  ! Fraction of neutronic current going towards inboard/outboard blankets (%)
  real(dp), private :: j_plus_ib, j_plus_ob

  ! Specific heat at constant pressure for He at 8 MPa and 400C (kJ/kg/K)
  real(dp),private :: cp_he

  ! Reference thermal blanket power, DEMO 2007 (MW)
  real(dp), private :: P_th_0

  ! Reference He pumping power, DEMO 2007 (MW)
  real(dp),private :: P_pump_0

  ! Radial position of first wall inboard/outboard side (m)
  real(dp),private :: rad_ib, rad_ob

  ! Breeder zone and back plate size percentage of blanket inboard/outboard (%)
  real(dp),private :: bp_ratio_ib, bp_ratio_ob, bz_ratio_ib, bz_ratio_ob

  ! Breeder blanket breeder zone thickness (m)
  real(dp), private :: thick_bz_ib, thick_bz_ob

  ! Breeder blanket back plate thickness (m)
  real(dp), private :: thick_bp_ib, thick_bp_ob

  ! First wall thicknesses inboard/outboard (m)
  real(dp),private :: thick_fw_ib, thick_fw_ob

  ! Breeder blanket thicknesses inboard/outboard (m)
  real(dp),private :: dr_bb_ib, dr_bb_ob

  ! TODO Why is this comment here?
  ! Breeder blanket breeder zone thickness (m)

  ! Inboard ellipse 1 minor and major radii (m)
  real(dp),private :: r_ib, z_ib

  ! Outboard ellipse 2 minor and major radii (m)
  real(dp),private :: r_ob, z_ob

  ! h parameter for ellipse's length formula inboard/outboard
  real(dp), private :: h_ib, h_ob

  ! ellipse length (m) inboard/outboard
  real(dp), private :: len_ib, len_ob

  ! Fraction of ideal blanket ellipse length for divertor inboard/outboard
  real(dp), private :: frac_div_ib, frac_div_ob

  ! ellipse length (accounting for divertor gap) (m) inboard/outboard
  real(dp), private :: len_act_ib, len_act_ob

  ! Number of inboard/outboard modules in the poloidal direction
  real(dp), private :: nb_pol_ib, nb_pol_ob

  ! Max allowed (from Maintainance) Poloidal thickness of inboard blanket module (m)
  real(dp), private :: dp_bb_ib_max, dp_bb_ob_max

  ! Number of inboard/outboard  modules in the toroidal direction (blanket segment)
  real(dp), private :: nb_tor_ib, nb_tor_ob

  ! Max allowed (from RH) toroidal thickness of inboard/outboard blanket module (m)
  real(dp), private :: dt_bb_ib_max, dt_bb_ob_max

  ! Poloidal length of inboard/outboard blanket module
  real(dp), private :: dp_bb_ib, dp_bb_ob

  ! Toroidal length of inboard/outboard blanket module (m)
  real(dp), private :: dt_bb_ib, dt_bb_ob

  ! Inboard/outboard and total FW area (m2)
  real(dp), private :: area_fw_ib, area_fw_ob, area_fw

  ! Inboard/outboard blanket module side wall thickness
  real(dp), private :: thick_sw_ib, thick_sw_ob

  ! Inboard/outboard blanket module cap thickness (m)
  real(dp), private :: thick_cap_ib, thick_cap_ob

  ! Radial thickness of inboard/outboard breeding zone (m)
  real(dp), private :: dr_bz_ib, dr_bz_ob

  ! Toroidal thickness of inboard/outboard breeding zone (m)
  real(dp), private :: dt_bz_ib, dt_bz_ob

  ! Poloidal thickness of inboard/outboard breeding zone (m)
  real(dp), private :: dp_bz_ib, dp_bz_ob

  ! Number of BU in a inboard/outboard module along the toroidal direction
  real(dp), private :: nb_bu_tor_ib, nb_bu_tor_ob

  ! Number of BU in a inboard/outboard module along the poloidal direction
  real(dp), private :: nb_bu_pol_ib, nb_bu_pol_ob

  ! Toroidal thickness of a inboard/outboard BU (m)
  real(dp), private :: dt_bu_ib, dt_bu_ob

  ! Poloidal thickness of a inboard/outboard BU (m)
  real(dp), private :: dp_bu_ib, dp_bu_ob

  ! Max allowed toroidal/poloidal dimension of BU (m)
  real(dp), private :: dt_bu_max, dp_bu_max

  ! Inboard/outboard back supporting structure radial thickness (m)
  real(dp), private :: thick_bss_ib, thick_bss_ob

  ! Poloidal thickness of inboard/outboard manifold region (m)
  real(dp), private :: dp_mf_ib, dp_mf_ob

  ! Toroidal thickness of inboard/outboard manifold region (m)
  real(dp), private :: dt_mf_ib, dt_mf_ob

  ! Radial thickness of inboard/outboard manifold region (m)
  real(dp), private :: dr_mf_ib, dr_mf_ob

  ! volume fraction of helium in the FW (calculated in the CEA version) (%)
  real(dp), private :: frac_vol_he_fw

  ! Percentage of steel in the FW (%)
  real(dp), private :: frac_vol_steel_fw

  ! FW volume in an inboard/outboard blanket module (m3)
  real(dp), private :: vol_fw_ib, vol_fw_ob

  ! Total FW volume (reactor segment) (m3)
  real(dp), private :: vol_fw

  ! He volume in the FW for in inboard/outboard blanket module (m3)
  real(dp), private :: vol_he_fw_ib, vol_he_fw_ob

  ! Total He volume in FW (reactor segment) (m3)
  real(dp), private :: vol_he_fw

  ! Steel volume in the FW for an inboard outboard blanket module (m3)
  real(dp), private :: vol_steel_fw_ib, vol_steel_fw_ob

  ! Total steel volume in the FW (reactor segment) (m3)
  real(dp), private :: vol_steel_fw

  ! Tungsten volume in the FW for an inboard/outboard blanket module (m3)
  real(dp), private :: vol_w_fw_ib, vol_w_fw_ob

  ! Volume fraction of tungsten in the FW (%)
  real(dp), private :: frac_vol_w_fw

  ! Volume fraction of He in BZ (%)
  real(dp), private :: frac_vol_he_bz

  ! Volume fraction of steel in BZ (%)
  real(dp), private :: frac_vol_steel_bz

  ! Volume fraction of PbLi in manifold region (%)
  real(dp), private :: frac_vol_pbli_mf

  ! Volume fraction of steel in manifold region (%)
  real(dp), private :: frac_vol_steel_mf

  ! Percentage of PbLi in the BZ (%)
  real(dp), private :: frac_vol_pbli_bz

  ! BZ volume in an inboard/outboard blanket module
  real(dp), private :: vol_bz_ib, vol_bz_ob

  ! Total He volume in inboard/outboard BZ (m3)
  real(dp), private :: vol_he_bz_ib, vol_he_bz_ob

  ! Total steel volume in the inboard/outboard blanket module (m3)
  real(dp), private :: vol_steel_bz_ib, vol_steel_bz_ob

  ! Total PbLi volume in the inboard/outboard blanket module BZ
  real(dp), private :: vol_pbli_bz_ib, vol_pbli_bz_ob

  ! Total He volume in the BZ for a reactor sector (m3)
  real(dp), private :: vol_he_bz

  ! Total steel volume in the BZ for a reactor sector (m3)
  real(dp), private :: vol_steel_bz

  ! Total volume in the BZ for a reactor sector (m3)
  real(dp), private :: vol_bz

  ! Total Pb-Li volume in the BZ for a reactor sector (m3)
  real(dp), private :: vol_pbli_bz

  ! Percentage of He in the manifold region (%)
  real(dp), private :: frac_vol_he_mf

  ! Total volume in the inboard/outboard manifold region (m3)
  real(dp), private :: vol_mf_ib, vol_mf_ob

  ! Total PbLi volume in the inboard/outboard manifold region (m3)
  real(dp), private :: vol_pbli_mf_ib, vol_pbli_mf_ob

  ! Total He volume in the inboard/outboard manifold region (m3)
  real(dp), private :: vol_he_mf_ib, vol_he_mf_ob

  ! Total steel volume in the inboard/outboard manifold region (m3)
  real(dp), private :: vol_steel_mf_ib, vol_steel_mf_ob

  ! Total PbLi volume in the manifold region for a reactor sector (m3)
  real(dp), private :: vol_pbli_mf

  ! Total steel volume in the manifold region for a reactor sector (m3)
  real(dp), private :: vol_steel_mf

  ! Total volume in the manifold region for a reactor sector (m3)
  real(dp), private :: vol_mf

  ! Total steel volume in the manifold region for a reactor sector (m3)
  real(dp), private :: vol_he_mf

  ! Helium mass for an inboard/outboard reactor segment (kg)
  real(dp), private :: mass_he_segm_ib, mass_he_segm_ob

  ! Steel mass for an inboard/outboard reactor segment (kg)
  real(dp), private :: mass_steel_segm_ib, mass_steel_segm_ob

  ! PbLi mass for an inboard/outboard reactor segment (kg)
  real(dp), private :: mass_pbli_segm_ib, mass_pbli_segm_ob

  ! Tungsten mass for an inboard/outboard reactor segment (kg)
  real(dp), private :: mass_w_segm_ib, mass_w_segm_ob

  ! Total mass for an inboard/outboard reactor segment (kg)
  real(dp), private :: mass_segm_ib, mass_segm_ob

  ! Total mass for a blanket sector (kg)
  real(dp), private :: mass_sector

  ! Total He mass for the whole blanket (kg)
  real(dp), private :: mass_he_blanket

  ! Total PbLi mass for the whole blanket (kg)
  real(dp), private :: mass_pbli_blanket

  ! Total steel mass for the whole blanket (kg)
  real(dp), private :: mass_steel_blanket

  ! Total tungsten mass for the whole breeding blanket (kg)
  real(dp), private :: mass_w_blanket

  ! Total blanket mass (kg)
  real(dp), private :: mass_blanket

  ! Dimensionless inboard/outboard Tritium Production Rate
  real(dp), private :: TPR_ib, TPR_ob

  ! TBR from the inboard/outboard blankets
  real(dp), private :: TBR_ib, TBR_ob

  ! Fast neutron flux on the inboard/outboard leg (cm-2 s-1)
  real(dp), private :: phi_tfc_ib, phi_tfc_ob

  ! He mass flow rate (kg/s)
  real(dp), private :: w_he

  ! Blanket He pumping power (MW)
  ! real(dp), private :: p_pump

  ! Surface heat flux on first wall (MW) (sum = pradfw)
  real(dp), private :: psurffwi, psurffwo

  ! Ratio of FW/BKT nuclear power as fraction of total
  real(dp), private :: pnuc_fw_ratio, pnuc_bkt_ratio

  ! Powerflow calculation variables

  ! Inboard/outboard blanket coolant channel length (radial direction) (m)
  real(dp), private :: bldepti, bldepto

  ! Inboard/outboard blanket flow lengths (m)
  real(dp), private :: bzfllengi, bzfllengo

  ! Inboard/outboard coolant velocity in blanket (m/s)
  real(dp), private :: velblkti, velblkto

  ! Inboard/outboard first wall peak temperature (K)
  real(dp), private :: tpeakfwi, tpeakfwo

  ! Inboard/outboard first wall nuclear heating (MW)
  real(dp), private :: pnucfwi, pnucfwo

  ! Neutron power deposited inboard/outboard blanket blanket (MW)
  real(dp), private :: pnucblkti, pnucblkto

  ! Inboard/utboard total number of pipes
  real(dp), private :: npfwi, npfwo

  ! Inboard/outboard total num of pipes
  real(dp), private :: npblkti, npblkto

  ! Total mass flow rate for coolant (kg/s)
  real(dp), private :: mftotal

  ! Inboard/outboard mass flow rate per coolant pipe (kg/s)
  real(dp), private :: mffwpi, mffwpo

  ! Inboard/outboard total mass flow rate to remove inboard FW power (kg/s)
  real(dp), private :: mffwi, mffwo, mffw

  ! Inboard/outboard blanket mass flow rate for coolant (kg/s)
  real(dp), private :: mfblkti, mfblkto, mfblkt

  ! Inboard/outboard mass flow rate per coolant pipe (kg/s)
  real(dp), private :: mfblktpi, mfblktpo

  ! Blanket internal half-height (m)
  real(dp), private :: hblnkt

  ! Inboard/outboard blanket segment poloidal length (m)
  real(dp), private :: bllengi, bllengo

  ! Inboard/outboard blanket mid-plan toroidal circumference for segment (m)
  real(dp), private :: blwidti, blwidto

  ! Inboard/outboard first wall pumping power (MW)
  real(dp), private :: htpmw_fwi, htpmw_fwo

  ! Inboard/outboard blanket pumping power (MW)
  real(dp), private :: htpmw_blkti, htpmw_blkto

contains

  subroutine init_kit_hcll_module
    !! Initialise module variables
    implicit none

    ip = 0
    ofile = 0
    T_he_in = 0.0D0
    T_He_out = 0.0D0
    denpbli = 0.0D0
    denhe = 0.0D0
    cf = 0.0D0
    emult_all = 0.0D0
    ff_ib = 0.0D0
    ff_ob = 0.0D0
    j_plus_ib = 0.0D0
    j_plus_ob = 0.0D0
    cp_he = 0.0D0
    P_th_0 = 0.0D0
    P_pump_0 = 0.0D0
    rad_ib = 0.0D0
    rad_ob = 0.0D0
    bp_ratio_ib = 0.0D0
    bp_ratio_ob = 0.0D0
    bz_ratio_ib = 0.0D0
    bz_ratio_ob = 0.0D0
    thick_bz_ib = 0.0D0
    thick_bz_ob = 0.0D0
    thick_bp_ib = 0.0D0
    thick_bp_ob = 0.0D0
    thick_fw_ib = 0.0D0
    thick_fw_ob = 0.0D0
    dr_bb_ib = 0.0D0
    dr_bb_ob = 0.0D0
    r_ib = 0.0D0
    z_ib = 0.0D0
    r_ob = 0.0D0
    z_ob = 0.0D0
    h_ib = 0.0D0
    h_ob = 0.0D0
    len_ib = 0.0D0
    len_ob = 0.0D0
    frac_div_ib = 0.0D0
    frac_div_ob = 0.0D0
    len_act_ib = 0.0D0
    len_act_ob = 0.0D0
    nb_pol_ib = 0.0D0
    nb_pol_ob = 0.0D0
    dp_bb_ib_max = 0.0D0
    dp_bb_ob_max = 0.0D0
    nb_tor_ib = 0.0D0
    nb_tor_ob = 0.0D0
    dt_bb_ib_max = 0.0D0
    dt_bb_ob_max = 0.0D0
    dp_bb_ib = 0.0D0
    dp_bb_ob = 0.0D0
    dt_bb_ib = 0.0D0
    dt_bb_ob = 0.0D0
    area_fw_ib = 0.0D0
    area_fw_ob = 0.0D0
    area_fw = 0.0D0
    thick_sw_ib = 0.0D0
    thick_sw_ob = 0.0D0
    thick_cap_ib = 0.0D0
    thick_cap_ob = 0.0D0
    dr_bz_ib = 0.0D0
    dr_bz_ob = 0.0D0
    dt_bz_ib = 0.0D0
    dt_bz_ob = 0.0D0
    dp_bz_ib = 0.0D0
    dp_bz_ob = 0.0D0
    nb_bu_tor_ib = 0.0D0
    nb_bu_tor_ob = 0.0D0
    nb_bu_pol_ib = 0.0D0
    nb_bu_pol_ob = 0.0D0
    dt_bu_ib = 0.0D0
    dt_bu_ob = 0.0D0
    dp_bu_ib = 0.0D0
    dp_bu_ob = 0.0D0
    dt_bu_max = 0.0D0
    dp_bu_max = 0.0D0
    thick_bss_ib = 0.0D0
    thick_bss_ob = 0.0D0
    dp_mf_ib = 0.0D0
    dp_mf_ob = 0.0D0
    dt_mf_ib = 0.0D0
    dt_mf_ob = 0.0D0
    dr_mf_ib = 0.0D0
    dr_mf_ob = 0.0D0
    frac_vol_he_fw = 0.0D0
    frac_vol_steel_fw = 0.0D0
    vol_fw_ib = 0.0D0
    vol_fw_ob = 0.0D0
    vol_fw = 0.0D0
    vol_he_fw_ib = 0.0D0
    vol_he_fw_ob = 0.0D0
    vol_he_fw = 0.0D0
    vol_steel_fw_ib = 0.0D0
    vol_steel_fw_ob = 0.0D0
    vol_steel_fw = 0.0D0
    vol_w_fw_ib = 0.0D0
    vol_w_fw_ob = 0.0D0
    frac_vol_w_fw = 0.0D0
    frac_vol_he_bz = 0.0D0
    frac_vol_steel_bz = 0.0D0
    frac_vol_pbli_mf = 0.0D0
    frac_vol_steel_mf = 0.0D0
    frac_vol_pbli_bz = 0.0D0
    vol_bz_ib = 0.0D0
    vol_bz_ob = 0.0D0
    vol_he_bz_ib = 0.0D0
    vol_he_bz_ob = 0.0D0
    vol_steel_bz_ib = 0.0D0
    vol_steel_bz_ob = 0.0D0
    vol_pbli_bz_ib = 0.0D0
    vol_pbli_bz_ob = 0.0D0
    vol_he_bz = 0.0D0
    vol_steel_bz = 0.0D0
    vol_bz = 0.0D0
    vol_pbli_bz = 0.0D0
    frac_vol_he_mf = 0.0D0
    vol_mf_ib = 0.0D0
    vol_mf_ob = 0.0D0
    vol_pbli_mf_ib = 0.0D0
    vol_pbli_mf_ob = 0.0D0
    vol_he_mf_ib = 0.0D0
    vol_he_mf_ob = 0.0D0
    vol_steel_mf_ib = 0.0D0
    vol_steel_mf_ob = 0.0D0
    vol_pbli_mf = 0.0D0
    vol_steel_mf = 0.0D0
    vol_mf = 0.0D0
    vol_he_mf = 0.0D0
    mass_he_segm_ib = 0.0D0
    mass_he_segm_ob = 0.0D0
    mass_steel_segm_ib = 0.0D0
    mass_steel_segm_ob = 0.0D0
    mass_pbli_segm_ib = 0.0D0
    mass_pbli_segm_ob = 0.0D0
    mass_w_segm_ib = 0.0D0
    mass_w_segm_ob = 0.0D0
    mass_segm_ib = 0.0D0
    mass_segm_ob = 0.0D0
    mass_sector = 0.0D0
    mass_he_blanket = 0.0D0
    mass_pbli_blanket = 0.0D0
    mass_steel_blanket = 0.0D0
    mass_w_blanket = 0.0D0
    mass_blanket = 0.0D0
    TPR_ib = 0.0D0
    TPR_ob = 0.0D0
    TBR_ib = 0.0D0
    TBR_ob = 0.0D0
    phi_tfc_ib = 0.0D0
    phi_tfc_ob = 0.0D0
    w_he = 0.0D0
    psurffwi = 0.0D0
    psurffwo = 0.0D0
    pnuc_fw_ratio = 0.0D0
    pnuc_bkt_ratio = 0.0D0
    bldepti = 0.0D0
    bldepto = 0.0D0
    bzfllengi = 0.0D0
    bzfllengo = 0.0D0
    velblkti = 0.0D0
    velblkto = 0.0D0
    tpeakfwi = 0.0D0
    tpeakfwo = 0.0D0
    pnucfwi = 0.0D0
    pnucfwo = 0.0D0
    pnucblkti = 0.0D0
    pnucblkto = 0.0D0
    npfwi = 0.0D0
    npfwo = 0.0D0
    npblkti = 0.0D0
    npblkto = 0.0D0
    mftotal = 0.0D0
    mffwpi = 0.0D0
    mffwpo = 0.0D0
    mffwi = 0.0D0
    mffwo = 0.0D0
    mffw = 0.0D0
    mfblkti = 0.0D0
    mfblkto = 0.0D0
    mfblkt = 0.0D0
    mfblktpi = 0.0D0
    mfblktpo = 0.0D0
    hblnkt = 0.0D0
    bllengi = 0.0D0
    bllengo = 0.0D0
    blwidti = 0.0D0
    blwidto = 0.0D0
    htpmw_fwi = 0.0D0
    htpmw_fwo = 0.0D0
    htpmw_blkti = 0.0D0
    htpmw_blkto = 0.0D0
  end subroutine init_kit_hcll_module

  subroutine kit_hcll(outfile, iprint)
    !! KIT HCLL blanket model
    !! author: J Morris, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates nuclear heating for the KIT HCLL
    !! blanket model.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    integer, intent(in) :: iprint, outfile

    !  Assign module private variables to iprint and outfile
    ip = iprint
    ofile = outfile

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! initalise some module level parameters for the model
    call initialise_parameters

    ! setup blanket composition
    call blanket_composition

    ! Neutronics
    call neutronics

    ! Thermal hyraulics
    call thermal_hydraulics

    ! Display output if final iteration
    if (ip /= 1) then
      return
    else
      call display_output
    end if

  end subroutine kit_hcll

  subroutine initialise_parameters
    !! KIT HCLL blanket model parameter initialisation
    !! author: J Morris, CCFE, Culham Science Centre
    !! This routine initialises parameters for the KIT HCLL blanket model
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwith, fwoth
    use fwbs_variables, only: emult, fdiv, fhcd, afw, fw_wall, coolwh

    implicit none

    ! Set the blanket inlet and outlet temperatures (K)
    T_he_in = 300.0D0
    T_he_out = 500.0D0

    ! Energy multiplication in blanket + vv + divertor
    ! (emult = 1.17 for blanket + vv + divertor)
    emult = 1.12D0
    emult_all = 1.17D0

    ! Density of lead lithium (kg/m3)
    denpbli = 9839.0D0

    ! Density of Helium (kg/m3 at 8MPa, 400 C)
    denhe = 5.4D0

    ! Coverage factor (%)
    cf = 1.0D0-fdiv-fhcd

    ! TBR Formula correction Factors
    ff_ib = 0.8572D0
    ff_ob = 0.8026D0

    ! Fraction of neutronic current going towards inboard/outboard blankts (%)
    j_plus_ib = 0.237D0
    j_plus_ob = 0.686D0

    ! Specific heat at constant pressure for He at 8 MPa and 400C (kJ/kg/K)
    cp_he = 5.190D0

    ! Reference thermal blanket power, DEMO 2007 (MW)
    P_th_0 = 2394.0D0

    ! Reference He pumping power, DEMO 2007 (MW)
    P_pump_0 = 245.0D0

    ! Max allowed (from Maintainance) Poloidal thickness of inboard blanket module (m)
    dp_bb_ib_max = 2.0D0
    dp_bb_ob_max = 2.0D0

    ! Max allowed (from RH) toroidal thickness of inboard/outboard blanket module (m)
    dt_bb_ib_max = 1.5D0
    dt_bb_ob_max = 1.5D0

    ! Inboard/outboard blanket module side wall thickness
    thick_sw_ib = 0.025D0
    thick_sw_ob = 0.025D0

    ! Inboard/outboard blanket module cap thickness (m)
    thick_cap_ib = 0.025D0
    thick_cap_ob = 0.025D0

    ! Max allowed toroidal/poloidal dimension of BU (m)
    dt_bu_max = 0.2D0
    dp_bu_max = 0.2D0

    ! Inboard/outboard back supporting structure radial thickness (m)
    thick_bss_ib = 0.111D0
    thick_bss_ob = 0.322D0

    ! volume fraction of helium in the FW (calculated in the CEA version) (%)
    frac_vol_he_fw = 26.3D0

    ! Volume fraction of tungsten in the FW (%)
    frac_vol_w_fw = 4.8D0

    ! Volume fraction of He in BZ (%)
    frac_vol_he_bz = 7.0D0

    ! Volume fraction of steel in BZ (%)
    frac_vol_steel_bz = 14.7D0

    ! Volume fraction of PbLi in manifold region (%)
    frac_vol_pbli_mf = 6.8D0

    ! Volume of steel in the manifold region (%)
    frac_vol_steel_mf = 42.0D0

    ! Set first wall inboard/outboard thickness (m)
    fwith = 2*afw + 2*fw_wall
    fwoth = fwith

    ! Set coolant type
    coolwh = 1

  end subroutine initialise_parameters

  subroutine blanket_composition
    !! KIT HCLL blanket model blanket composition
    !! author: J Morris, CCFE, Culham Science Centre
    !! This routine calculates blanket composition for the KIT HCLL blanket model.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: scrapli, scraplo, blnkith, fwith, blnkoth, fwoth
    use fwbs_variables, only: volblkti, volblkto, volblkt, denstl, denw
    use tfcoil_variables, only: n_tf
    use physics_variables, only: rmajor, rminor, triang, kappa

    implicit none

    ! Radial position of first wall inboard/outboard side (m)
    rad_ib = rmajor - rminor - scrapli
    rad_ob = rmajor + rminor + scraplo

    ! Inboard blanket module and first wall thickness (m)
    dr_bb_ib = blnkith
    thick_fw_ib = fwith

    ! Outboard blanket module and first wall thickness (m)
    dr_bb_ob = blnkoth
    thick_fw_ob = fwoth

    ! Ratio of BZ to BP in blanket thickness in original data (inboard)
    bz_ratio_ib = 0.641D0
    bp_ratio_ib = 0.359D0

    ! Calculate thicknesses of breeding zone and back plate (inboard) (m)
    thick_bz_ib = bz_ratio_ib*blnkith
    thick_bp_ib = bp_ratio_ib*blnkith

    ! Ratio of BZ to BP in blanket thickness in original data (outboard)
    bz_ratio_ob = 0.773D0
    bp_ratio_ob = 0.227D0

    ! Fabrizio's model includes FW in blanket thickness => different ratios
    ! bz_ratio_ib = 0.615D0
    ! bp_ratio_ib = 0.344D0
    ! bz_ratio_ob = 0.753D0
    ! bp_ratio_ob = 0.221D0

    ! Calculate thicknesses of breeding zone and back plate (outboard) (m)
    thick_bz_ob = bz_ratio_ob*blnkoth
    thick_bp_ob = bp_ratio_ob*blnkoth

    ! Inboard ellipse !
    ! !!!!!!!!!!!!!!!!!!

    ! Inboard ellipse minor radius (m)
    r_ib = rminor*(1.0D0 - 0.0D0*triang) + scrapli + dr_bb_ib/2.0D0

    ! Inboard ellipse major radius (m)
    z_ib = rminor*kappa + scrapli + dr_bb_ib/2.0D0

    ! Inboard h parameter for ellipse's length formula
    h_ib = ((z_ib - r_ib)/(z_ib + r_ib))**2.0D0

    ! Inboard ellipse length (m)
    len_ib = pi*(z_ib + r_ib)*(1.0D0 + 3.0D0*h_ib/(10.0D0 + (4.0D0 - 3.0D0*h_ib)**0.5D0))/2.0D0

    ! Fraction of ideal inboard blanket ellipse length for divertor
    frac_div_ib = 1.0D0 - CF

    ! Inboard ellipse length (accounting for divertor gap) (m)
    len_act_ib = len_ib*(1.0D0 - frac_div_ib)

    ! Outboard ellipse !
    ! !!!!!!!!!!!!!!!!!!!

    ! Outboard ellipse minor radius (m)
    r_ob = rminor*(1.0D0 - 0.0D0*triang) + scraplo + dr_bb_ob/2.0D0

    ! Outboard ellipse major radius (m)
    z_ob = rminor*kappa + scraplo + dr_bb_ob/2.0D0

    ! Outboard h parameter for ellipse's length formula
    h_ob = ((z_ob - r_ob)/(z_ob + r_ob))**2.0D0

    ! Outboard ellipse length (m)
    len_ob = pi*(z_ob + r_ob)*(1.0D0 + 3.0D0*h_ob/(10.0D0 + (4.0D0 - 3.0D0*h_ob)**0.5D0))/2.0D0

    ! Fraction of ideal outboard blanket ellipse length for divertor
    frac_div_ob = 1.0D0 - CF

    ! Outboard ellipse length (accounting for divertor gap) (m)
    len_act_ob = len_ob*(1.0D0 - frac_div_ob)

    ! Number of inboard  modules in the poloidal direction
    nb_pol_ib = ceiling(len_act_ib/dp_bb_ib_max)

    ! Number of outboard modules in the poloidal direction
    nb_pol_ob = ceiling(len_act_ob/dp_bb_ib_max)

    ! Number of inboard  modules in the toroidal direction (blanket segment)
    nb_tor_ib = ceiling((2.0D0*pi*rad_ib)/(n_tf*dt_bb_ib_max))

    ! Number of outboard  modules in the toroidal direction (blanket segment)
    nb_tor_ob = ceiling((2.0D0*pi*rad_ob)/(n_tf*dt_bb_ob_max))

    ! Poloidal length of inboard/outboard blanket module
    dp_bb_ib = len_act_ib/nb_pol_ib
    dp_bb_ob = len_act_ob/nb_pol_ob

    ! Toroidal length of inboard/outboard blanket module (m)
    dt_bb_ib = 2.0D0*pi*rad_ib/(n_tf*nb_tor_ib)
    dt_bb_ob = 2.0D0*pi*rad_ob/(n_tf*nb_tor_ob)

    ! Inboard/outboard and total FW area (m2)
    ! TODO make with work with first wall area from regular calculation
    area_fw_ib = dp_bb_ib*dt_bb_ib*nb_pol_ib*nb_tor_ib*n_tf
    area_fw_ob = dp_bb_ob*dt_bb_ob*nb_pol_ob*nb_tor_ob*n_tf
    area_fw = area_fw_ib + area_fw_ob

    ! Breeding Zone (BZ) !
    ! !!!!!!!!!!!!!!!!!!!!!

    ! Radial thickness of inboard/outboard breeding zone (m)
    dr_bz_ib = thick_bz_ib
    dr_bz_ob = thick_bz_ob

    ! Toroidal thickness of inboard/outboard breeding zone (m)
    dt_bz_ib = dt_bb_ib - 2.0D0*thick_sw_ib
    dt_bz_ob = dt_bb_ob - 2.0D0*thick_sw_ob

    ! Poloidal thickness of inboard/outboard breeding zone (m)
    dp_bz_ib = dp_bb_ib - 2.0D0*thick_cap_ib
    dp_bz_ob = dp_bb_ob - 2.0D0*thick_cap_ob

    ! Breeding Unit (BU) !
    ! !!!!!!!!!!!!!!!!!!!!!

    ! Number of BU in a inboard/outboard module along the toroidal direction
    nb_bu_tor_ib = ceiling(dt_bz_ib/dt_bu_max)
    nb_bu_tor_ob = ceiling(dt_bz_ob/dt_bu_max)

    ! Number of BU in a inboard/outboard module along the poloidal direction
    nb_bu_pol_ib = ceiling(dp_bz_ib/dp_bu_max)
    nb_bu_pol_ob = ceiling(dp_bz_ob/dp_bu_max)

    ! Toroidal thickness of a inboard/outboard BU (m)
    dt_bu_ib = dt_bz_ib/nb_bu_tor_ib
    dt_bu_ob = dt_bz_ob/nb_bu_tor_ob

    ! Poloidal thickness of a inboard/outboard BU (m)
    dp_bu_ib = dp_bz_ib/nb_bu_pol_ib
    dp_bu_ob = dp_bz_ob/nb_bu_pol_ob

    ! Manifold Region (Back Plates + Back Supporting Structure) !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Radial thickness of inboard/outboard manifold region (m)
    dr_mf_ib = thick_bp_ib + thick_bss_ib
    dr_mf_ob = thick_bp_ob + thick_bss_ob

    ! Poloidal thickness of inboard/outboard manifold region (m)
    dp_mf_ib = dp_bb_ib
    dp_mf_ob = dp_bb_ob

    ! Toroidal thickness of inboard/outboard manifold region (m)
    dt_mf_ib = dt_bb_ib
    dt_mf_ob = dt_bb_ob

    ! First wall (FW) material composition !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Percentage of steel in the FW (%)
    frac_vol_steel_fw = 100.0D0 - frac_vol_he_fw

    ! FW volume in an inboard/outboard blanket module (m3)
    vol_fw_ib = thick_fw_ib*dt_bb_ib*dp_bb_ib
    vol_fw_ob = thick_fw_ob*dt_bb_ob*dp_bb_ob

    ! Total FW volume (reactor segment) (m3)
    vol_fw = vol_fw_ib*(nb_pol_ib*nb_tor_ib) + vol_fw_ob*(nb_pol_ob*nb_tor_ob)

    ! He volume in the FW for in inboard/outboard blanket module (m3)
    vol_he_fw_ib = frac_vol_he_fw/100.0D0*vol_fw_ib
    vol_he_fw_ob = frac_vol_he_fw/100.0D0*vol_fw_ob

    ! Total He volume in FW (reactor segment) (m3)
    vol_he_fw = vol_he_fw_ib*(nb_pol_ib*nb_tor_ib) + vol_he_fw_ob*(nb_pol_ob*nb_tor_ob)

    ! Steel volume in the FW for an inboard outboard blanket module (m3)
    vol_steel_fw_ib = frac_vol_steel_fw/100.0D0*vol_fw_ib
    vol_steel_fw_ob = frac_vol_steel_fw/100.0D0*vol_fw_ob

    ! Total steel volume in the FW (reactor segment) (m3)
    vol_steel_fw = vol_fw - vol_he_fw

    ! Tungsten volume in the FW for an inboard/outboard blanket module (m3)
    vol_w_fw_ib = frac_vol_w_fw/100.0D0*vol_fw_ib
    vol_w_fw_ob = frac_vol_w_fw/100.0D0*vol_fw_ob

    ! Breeding Zone (BZ) !
    ! !!!!!!!!!!!!!!!!!!!!!

    ! Percentage of PbLi in the BZ
    frac_vol_pbli_bz = 100.0D0 - frac_vol_steel_bz - frac_vol_he_bz

    ! BZ volume in an inboard/outboard blanket module (m3)
    vol_bz_ib = dr_bz_ib*dt_bz_ib*dp_bz_ib
    vol_bz_ob = dr_bz_ob*dt_bz_ob*dp_bz_ob

    ! Total He volume in inboard/outboard blanket module BZ (m3)
    vol_he_bz_ib = vol_bz_ib*frac_vol_he_bz/100.0D0
    vol_he_bz_ob = vol_bz_ob*frac_vol_he_bz/100.0D0

    ! Total He volume in the BZ for a reactor sector (m3)
    vol_he_bz = vol_he_bz_ib*nb_pol_ib*nb_tor_ib + vol_he_bz_ob*nb_pol_ob*nb_tor_ob

    ! Total steel volume in the inboard/outboard blanket module BZ (m3)
    vol_steel_bz_ib = vol_bz_ib*frac_vol_steel_bz/100.0D0
    vol_steel_bz_ob = vol_bz_ob*frac_vol_steel_bz/100.0D0

    ! Total steel volume in the BZ for a reactor sector (m3)
    vol_steel_bz = vol_steel_bz_ib*nb_pol_ib*nb_tor_ib + vol_steel_bz_ob*nb_pol_ob*nb_tor_ob

    ! Total PbLi volume in the inboard/outboard blanket module BZ (m3)
    vol_pbli_bz_ib = vol_bz_ib*frac_vol_pbli_bz/100.0D0
    vol_pbli_bz_ob = vol_bz_ob*frac_vol_pbli_bz/100.0D0

    ! Total volume in the BZ for a reactor sector (m3)
    vol_bz = vol_bz_ib*nb_pol_ib*nb_tor_ib + vol_bz_ob*nb_pol_ob*nb_tor_ob

    ! Total Pb-Li volume in the BZ for a reactor sector (m3)
    vol_pbli_bz = vol_bz -vol_steel_bz - vol_he_bz

    ! Manifold region (Back Plate + Back Supporting Structure) !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Percentage of He in the manifold region (%)
    frac_vol_he_mf = 100.0D0 - frac_vol_pbli_mf - frac_vol_steel_mf

    ! Total volume in the inboard/outboard manifold region (m3)
    vol_mf_ib = dr_mf_ib*dt_mf_ib*dp_mf_ib
    vol_mf_ob = dr_mf_ob*dt_mf_ob*dp_mf_ob

    ! Total PbLi volume in the inboard/outboard manifold region (m3)
    vol_pbli_mf_ib = frac_vol_pbli_mf/100.0D0*vol_mf_ib
    vol_pbli_mf_ob = frac_vol_pbli_mf/100.0D0*vol_mf_ob

    ! Total PbLi volume in the manifold region for a reactor sector (m3)
    vol_pbli_mf = vol_pbli_mf_ib*nb_pol_ib*nb_tor_ib + vol_pbli_mf_ob*nb_pol_ob*nb_tor_ob

    ! Total steel volume in the inboard/outboard manifold (m3)
    vol_steel_mf_ib = frac_vol_steel_mf/100.0D0*vol_mf_ib
    vol_steel_mf_ob = frac_vol_steel_mf/100.0D0*vol_mf_ob

    ! Total steel volume in the manifold region for a reactor sector (m3)
    vol_steel_mf = vol_steel_mf_ib*nb_pol_ib*nb_tor_ib + vol_steel_mf_ob*nb_pol_ob*nb_tor_ob

    ! Total He volume in the inboard/outboard manifold region (m3)
    vol_he_mf_ib = frac_vol_he_mf/100.0D0*vol_mf_ib
    vol_he_mf_ob = frac_vol_he_mf/100.0D0*vol_mf_ob

    ! Total volume in the manifold region for a reactor sector (m3)
    vol_mf = vol_mf_ib*nb_pol_ib*nb_tor_ib + vol_mf_ob*nb_pol_ob*nb_tor_ob

    ! Total steel volume in the manifold region for a reactor sector (m3)
    vol_he_mf = vol_mf -vol_steel_mf - vol_pbli_mf

    ! Total volumes !
    ! !!!!!!!!!!!!!!!!

    ! Volume of inboard/outboard and total blanket (m3)
    volblkti = (vol_mf_ib*nb_pol_ib*nb_tor_ib + vol_bz_ib*nb_pol_ib*nb_tor_ib + &
      vol_fw_ib*(nb_pol_ib*nb_tor_ib))*n_tf
    volblkto = (vol_mf_ob*nb_pol_ob*nb_tor_ob + vol_bz_ob*nb_pol_ob*nb_tor_ob + &
      vol_fw_ob*(nb_pol_ob*nb_tor_ob))*n_tf
    volblkt = (vol_fw + vol_bz + vol_mf)*n_tf

    ! Modules and segments mass !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Helium mass for an inboard/outboard reactor segment (kg)
    mass_he_segm_ib = denhe*nb_pol_ib*(vol_he_fw_ib + vol_he_bz_ib + vol_he_mf_ib)
    mass_he_segm_ob = denhe*nb_pol_ob*(vol_he_fw_ob + vol_he_bz_ob + vol_he_mf_ob)

    ! Steel mass for an inboard/outboard reactor segment (kg)
    mass_steel_segm_ib = denstl*nb_pol_ib*(vol_steel_fw_ib + vol_steel_bz_ib + vol_steel_mf_ib)
    mass_steel_segm_ob = denstl*nb_pol_ob*(vol_steel_fw_ob + vol_steel_bz_ob + vol_steel_mf_ob)

    ! PbLi mass for an inboard/outboard reactor segment (kg)
    mass_pbli_segm_ib = denpbli*nb_pol_ib*(vol_pbli_bz_ib + vol_pbli_mf_ib)
    mass_pbli_segm_ob = denpbli*nb_pol_ob*(vol_pbli_bz_ob + vol_pbli_mf_ob)

    ! Tungsten mass for an inboard/outboard reactor segment (kg)
    mass_w_segm_ib = denw*nb_pol_ib*(vol_w_fw_ib)
    mass_w_segm_ob = denw*nb_pol_ob*(vol_w_fw_ob)

    ! Total mass for an inboard/outboard reactor segment (kg)
    mass_segm_ib = mass_he_segm_ib + mass_steel_segm_ib + mass_pbli_segm_ib + mass_w_segm_ib
    mass_segm_ob = mass_he_segm_ob + mass_steel_segm_ob + mass_pbli_segm_ob + mass_w_segm_ob

    ! Total mass for a blanket sector (kg)
    mass_sector = mass_segm_ib*nb_tor_ib + mass_segm_ob*nb_tor_ob

    ! Total He mass for the whole blanket (kg)
    mass_he_blanket = (mass_he_segm_ib*nb_tor_ib + mass_he_segm_ob*nb_tor_ob)*n_tf

    ! Total PbLi mass for the whole blanket (kg)
    mass_pbli_blanket = (mass_pbli_segm_ib*nb_tor_ib + mass_pbli_segm_ob*nb_tor_ob)*n_tf

    ! Total steel mass for the whole blanket (kg)
    mass_steel_blanket = (mass_steel_segm_ib*nb_tor_ib + mass_steel_segm_ob*nb_tor_ob)*n_tf

    ! Total tungsten mass for the whole breeding blanket (kg)
    mass_w_blanket = (mass_w_segm_ib*nb_tor_ib + mass_w_segm_ob*nb_tor_ob)*n_tf

    ! Total blanket mass (kg)
    mass_blanket = mass_sector*n_tf

  end subroutine blanket_composition

  subroutine neutronics
    !! KIT HCLL blanket model neutronics
    !! author: J Morris, CCFE, Culham Science Centre
    !! This routine calculates the neutronics for the KIT HCLL blanket model.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwith, shldith, d_vv_in, d_vv_out, fwoth, shldoth
    use fwbs_variables, only: tbr

		use global_variables, only: output_prefix, fileprefix
    implicit none

    ! Local variables

    ! Dimensions in cm for TBR
    real(dp) :: bz_ri, bz_ro
    real(dp) :: f_steel_bz, f_pbli_bz

    bz_ri = dr_bz_ib*100.0D0
    bz_ro = dr_bz_ob*100.0D0
    f_steel_bz = frac_vol_steel_bz/100.0D0
    f_pbli_bz = frac_vol_pbli_bz/100.0D0


    ! TBR formula from CEA study !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Dimensionless Inboard Tritium Production Rate
    TPR_ib = (bz_ri**3.0D0)*(8.686E-0*f_steel_bz**2.0D0 + 1.168E-0*f_steel_bz + 1.006E-0)* &
                        (-1.033E-6*f_pbli_bz**2.0D0 + 4.835E-6*f_pbli_bz  - 2.138E-6) + &
            (bz_ri**2.0D0)*(2.603E-0*f_steel_bz**2.0D0 + 8.698E-1*f_steel_bz + 1.001E-0)* &
                        (4.075E-4*f_pbli_bz**2.0D0 - 1.316E-3*f_pbli_bz  + 4.319E-4) + &
            (bz_ri**1.0D0)*(9.107E-1*f_steel_bz**2.0D0 + 4.253E-2*f_steel_bz + 1.000E-0)* &
                        (-4.675E-2*f_pbli_bz**2.0D0 + 1.129E-1*f_pbli_bz  - 1.904E-2) + &
            (bz_ri**0.0D0)*( 5.950E-1*f_pbli_bz**2.0D0 - 6.856E-1*f_pbli_bz  + 1.934E-1) + &
                        (-8.365E-1*f_steel_bz**2.0D0 + 5.460E-1*f_steel_bz + 1.949E-4);

    ! Dimensionless Outboard Tritium Production Rate
    TPR_ob = (bz_ro**3.0D0)*( 8.686E-0*f_steel_bz**2.0D0 + 1.168E-0*f_steel_bz + 1.006E-0)* &
                        (-1.033E-6*f_pbli_bz **2.0D0 + 4.835E-6*f_pbli_bz  - 2.138E-6) + &
            (bz_ro**2.0D0)*( 2.603E-0*f_steel_bz**2.0D0 + 8.698E-1*f_steel_bz + 1.001E-0)* &
                        ( 4.075E-4*f_pbli_bz **2.0D0 - 1.316E-3*f_pbli_bz  + 4.319E-4) + &
            (bz_ro**1.0D0)*( 9.107E-1*f_steel_bz**2.0D0 + 4.253E-2*f_steel_bz + 1.000E-0)* &
                        (-4.675E-2*f_pbli_bz **2.0D0 + 1.129E-1*f_pbli_bz  - 1.904E-2) + &
            (bz_ro**0.0D0)*( 5.950E-1*f_pbli_bz **2.0D0 - 6.856E-1*f_pbli_bz  + 1.934E-1) + &
                        (-8.365E-1*f_steel_bz**2.0D0 + 5.460E-1*f_steel_bz + 1.949E-4)

    ! TBR from the inboard blanket
    TBR_ib = j_plus_ib*TPR_ib*ff_ib

    ! TBR from the outboard blanket
    TBR_ob = j_plus_ob*TPR_ob*ff_ob

    ! Total tritium breeding ratio
    TBR = TBR_ib + TBR_ob

    ! Peak fast neutron fluence on inboard TFC leg !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Fast neutron flux on the inboard leg (cm-2 s-1)
    call fast_neutron_flux(fwith*100.0D0, dr_bz_ib*100.0D0, dr_mf_ib*100.0D0, &
      shldith*100.0D0, d_vv_in*100.0D0, frac_vol_steel_bz/100.0D0, &
        frac_vol_pbli_bz/100.0D0, phi_tfc_ib)

    ! Fast neutron flux on the outboard leg (cm-2 s-1)
    call fast_neutron_flux(fwoth*100.0D0, dr_bz_ob*100.0D0, dr_mf_ob*100.0D0, &
      shldoth*100.0D0, d_vv_out*100.0D0, frac_vol_steel_bz/100.0D0, &
        frac_vol_pbli_bz/100.0D0, phi_tfc_ob)

  end subroutine neutronics

  subroutine thermal_hydraulics
    !! KIT HCLL blanket model thermal_hydraulics
    !! author: J Morris, CCFE, Culham Science Centre
    !! This routine calculates the thermal_hydraulics for the KIT HCLL blanket model.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwareaob, fwarea
    use fwbs_variables, only: pnucfw, pnucblkt, emult, emultmw, pnucdiv, fdiv, &
      praddiv, fhcd, pradfw, pradhcd, primary_pumping
    use heat_transport_variables, only: htpmw_fw, fpumpfw, htpmw_blkt, fpumpblkt, &
      htpmw_shld, fpumpshld, htpmw_div, fpumpdiv
    use current_drive_variables, only: porbitlossmw
    use physics_variables, only: pneutmw, pradmw, palpfwmw, pdivt

    implicit none

    ! Nuclear heating in Fabrizio's model doesn't separate the FW and blanket
    ! Use values given in Jordonova et al. 2005 separate the FW armour, FW and blanket.
    !
    ! For P_th = 2753.7 MW
    ! P_FW_W = 114.7 MW
    ! P_FW = 281.3 MW
    ! P_BKT = 2357.6 MW
    !
    ! Use these ratios for separating the power.
    pnuc_fw_ratio = (114.7D0 + 281.3D0)/2753.6D0
    pnuc_bkt_ratio = 2357.6D0/2753.6D0

    ! Nuclear heating in the first wall (MW)
    !pnucfw = powfmw*0.8D0*pnuc_fw_ratio
    pnucfw = pneutmw*pnuc_fw_ratio

    ! Nuclear heating in the blanket with energy multiplication (MW)
    !pnucblkt = (powfmw*0.8D0*pnuc_bkt_ratio)*emult*CF
    pnucblkt = (pneutmw*pnuc_bkt_ratio)*emult*CF

    ! Energy multiplication energy (MW)
    !emultmw = (powfmw*0.8D0*pnuc_bkt_ratio)*(emult - 1.0D0)
    emultmw = (pneutmw*pnuc_bkt_ratio)*(emult - 1.0D0)

    ! Nuclear heating in the divertor
    !pnucdiv = powfmw*0.8D0*fdiv
    pnucdiv = pneutmw*fdiv

    ! Radiation power incident on divertor (MW)
    praddiv = pradmw * fdiv

    ! Radiation power incident on HCD apparatus (MW)
    pradhcd = pradmw * fhcd

    ! Radiation power incident on first wall (MW)
    pradfw = pradmw - praddiv - pradhcd

    ! Surface heat flux on first wall (outboard and inboard) (MW)
    ! All of the fast particle losses go to the outer wall.
    psurffwo = pradfw * fwareaob/fwarea + porbitlossmw + palpfwmw
    psurffwi = pradfw * (1.0D0 - fwareaob/fwarea)

    if (primary_pumping == 0) then

      ! User sets mechanical pumping power directly (primary_pumping_power)
      ! Values of htpmw_blkt, htpmw_div, htpmw_fw, htpmw_shld set in input file

    else if (primary_pumping == 1) then

      ! User sets mechanical pumping power as a fraction of thermal power removed by coolant
      htpmw_fw = fpumpfw*(pnucfw + psurffwi + psurffwo)
      htpmw_blkt = fpumpblkt*pnucblkt
      htpmw_shld = fpumpshld*0.0D0 
      ! pnucshld is not available and is very small compared to other powers so set to zero.
      htpmw_div = fpumpdiv*(pdivt + pnucdiv + praddiv)

    else if (primary_pumping == 2) then

      ! Mechanical pumping power is calculated for first wall and blanket
      call thermo_hydraulic_model

      ! For divertor and shield, mechanical pumping power is a fraction of 
      ! thermal power removed by coolant
      htpmw_div = fpumpdiv * (pdivt + pnucdiv + praddiv)

      ! Shield power is negligible and this model doesn't have nuclear heating to the shield
      htpmw_shld = fpumpshld * 0.0D0

      ! He mass flow rate (kg/s)
      w_he = pnucblkt*1.0E6/(cp_he*1.0E3*(T_he_out - T_he_in))

      ! Blanket He pumping power (MW)
      ! TODO - keep this instead of more detailed calculation?
      ! p_pump= P_pump_0*(pnucblkt/P_th_0)

    end if

  end subroutine thermal_hydraulics

  subroutine thermo_hydraulic_model
    !! Thermo-hydraulic model for first wall and blanket
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculations for detailed powerflow model secondary_cycle > 0
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    use constants, only: pi
    use build_variables, only: blnkith, blnkoth, scraplo, scrapli, fwareaib, &
      fwareaob, fwarea, fwith, fwoth
    use fwbs_variables, only: nblktmodti, nblktmodto, afw, fwoutlet, fwinlet, &
      fw_channel_length, pitch, pnucblkt, volblkti, volblkt, outlet_temp, &
      inlet_temp, vfblkt, fwcoolant, fwpressure, etaiso, blpressure, coolwh, &
      pnucfw, volblkto, tpeak, fw_wall, roughness, primary_pumping, tfwmatmax
    use process_output, only: ovarin, oheadr, ovarre, ovarrf, ovarst, ocmmnt
    use heat_transport_variables, only: htpmw_fw, htpmw_blkt, htpmw_div, & 
      htpmw_shld, htpmw
    use fw_module, only: fw_temp
    use physics_variables, only: rmajor, rminor

    implicit none

    ! Local variables

    ! coolant specific heat capacity at constant pressure (J/kg/K)
    real(dp) :: cf

    ! coolant density (kg/m3)
    real(dp) :: rhof

    ! Number of 90 degree angle turns in FW and blanket flow channels
    integer :: no90fw, no90bz

    ! Number of 180 degree angle turns in FW and blanket flow channels
    integer :: no180fw, no180bz

    ! Coolant type (He or H20)
    integer :: coolant

    ! String for formatting coolant type output
    character(len=8) :: cstring

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Determine size of blanket modules
    ! Radial length of coolant pipes is assumed to be 80% of total radial space
    ! available for blanket, allowing for connections, manifolds etc.
    bldepti = 0.8D0 * blnkith
    bldepto = 0.8D0 * blnkoth

    ! Using the total perimeter of the machine, segment the outboard
    ! blanket into nblktmodp*nblktmodt modules, all assumed to be the same size

    ! Calculate mid-plane toroidal circumference and segment
    blwidti = (2.0D0*pi*(rmajor - rminor - scrapli)) / nblktmodti
    blwidto = (2.0D0*pi*(rmajor + rminor + scraplo)) / nblktmodto

    ! Calculate poloidal height of blanket modules
    call blanket_mod_pol_height

    ! Calculate total flow lengths, used for pressure drop calculation
    ! Blanket flow is assumed to follow a rad-pol-rad-rad-pol-rad path
    bzfllengi = 4.0D0*bldepti + 2.0D0*bllengi
    bzfllengo = 4.0D0*bldepto + 2.0D0*bllengo

    ! Number of angle turns in FW and blanket flow channels, consistent 
    ! with flow lengths defined
    no90fw = 2
    no180fw = 0
    no90bz = 4
    no180bz = 1

    ! Nuclear power deposited in fw inner and outer (MW)
    pnucfwi = pnucfw * fwareaib/fwarea
    pnucfwo = pnucfw * fwareaob/fwarea
    if ((pnucfw<0.0d0).or.(pnucfw/=pnucfw)) write(*,*)'pnucfw =', pnucfw
    if ((pnucfwi<0.0d0).or.(pnucfwi/=pnucfwi)) write(*,*)'pnucfwi =', pnucfwi
    if ((pnucfwo<0.0d0).or.(pnucfwo/=pnucfwo)) write(*,*)'pnucfwo =', pnucfwo

    !  Thermohydraulic calculations
    ! -------------------------------

    ! INBOARD !
    ! !!!!!!!!!!

    ! Maximum FW temperature. (27/11/2015) Issue #348
    ! First wall flow is just along the first wall, with no allowance for radial
    ! pipes, manifolds etc.
    ! The outputs are mean quantities of inlet and outlet
    call fw_temp(ip, ofile, afw, fwith, fwareaib, psurffwi, pnucfwi, tpeakfwi, & 
      cf, rhof, mffwpi, 'Inboard first wall')
    ! Recalculate the mass flow rates here using the heat capacity output by fw_temp

    ! Total mass flow rate to remove inboard first wall power (kg/s)
    mffwi = 1.0D6*(pnucfwi + psurffwi) / (cf*(fwoutlet - fwinlet))

    ! Total number of first wall pipes from channel length and pitch (02/12/2015)
    npfwi = fwareaib / (fw_channel_length*pitch)

    ! Mass flow rate per coolant pipe (kg/s):
    mffwpi = mffwi / npfwi

    ! Neutron power deposited in inboard blanket (MW)
    pnucblkti = pnucblkt * volblkti / volblkt

    ! Mass flow rate for inboard blanket coolant (kg/s)
    mfblkti = 1.0D6*(pnucblkti) / (cf*(outlet_temp-inlet_temp))

    ! Calc total num of pipes (in all inboard modules) from coolant frac and channel dimensions
    ! Assumes up/down flow, two 90 deg bends per length
    npblkti = (vfblkt * volblkti) / (pi * afw * afw * bzfllengi)

    ! Mass flow rate per coolant pipe (kg/s)
    if ((npblkti<1.0d1).or.(npblkti/=npblkti).or.(mfblkti<1.0d0).or.(mfblkti/=mfblkti)) then
        write(*,*) 'npblkti = ', npblkti, '   vfblkt =', vfblkt
        write(*,*) 'afw = ', afw, '   bzfllengi =', bzfllengi
        write(*,*) 'mfblkti =',mfblkti,   'pnucblkti =', pnucblkti
        write(*,*) 'pnucblkt =',pnucblkt,   'volblkti =', volblkti
        stop 1
    end if

    mfblktpi = mfblkti / npblkti

    ! Coolant velocity in blanket (m/s)
    velblkti = mfblktpi / (pi * afw * afw * rhof)

    ! Pumping powers for blanket and first wall (MW)
    if (fwcoolant == 'helium') coolant=1
    if (fwcoolant == 'water') coolant=2
    htpmw_fwi = pumppower(fwinlet, fwoutlet, fwpressure, fw_channel_length, afw, mffwi, &
        mffwpi, no90fw, no180fw, etaiso, coolant, 'Inboard first wall')
    htpmw_blkti = pumppower(inlet_temp, outlet_temp,blpressure, bzfllengi, afw, mfblkti, &
        mfblktpi, no90bz, no180bz, etaiso, coolwh, 'Inboard blanket')

    ! OUTBOARD !
    ! !!!!!!!!!!!

    ! Maximum FW temperature. (27/11/2015) Issue #348.
    call fw_temp(ip, ofile, afw, fwoth, fwareaob, psurffwo, pnucfwo, tpeakfwo, & 
      cf, rhof, mffwo, 'Outboard first wall')

    ! Recalculate the mass flow rates here using the heat capacity output by fw_temp
    ! Total mass flow rate to remove outboard first wall power (kg/s)
    mffwo = 1.0D6*(pnucfwo + psurffwo) / (cf*(fwoutlet-fwinlet))

    ! Total number of first wall pipes from channel length and pitch (02/12/2015)
    npfwo = fwareaob / (fw_channel_length*pitch)

    ! Mass flow rate per coolant pipe (kg/s)
    mffwpo = mffwo/npfwo

    ! Neutron power deposited in outboard blanket (MW)
    pnucblkto = pnucblkt * volblkto / volblkt

    ! Mass flow rate for outboard blanket coolant (kg/s)
    mfblkto = 1.0D6*(pnucblkto) / (cf*(outlet_temp-inlet_temp))  !  kg/s

    ! Calculate total number of pipes (in all outboard modules) from coolant fraction and
    ! channel dimensions (assumes up/down flow, two 90 deg bends per length)
    npblkto = (vfblkt*volblkto)/(pi*afw*afw*bzfllengo)

    ! mass flow rate per coolant pipe (kg/s)
    mfblktpo = mfblkto / npblkto

    ! Coolant velocity in breeder zone (m/s)
    velblkto = mfblktpo/(pi*afw*afw*rhof)

    ! Pumping powers for blanket and first wall
    if (fwcoolant == 'helium') coolant=1
    if (fwcoolant == 'water') coolant=2

    htpmw_fwo = pumppower(fwinlet, fwoutlet, fwpressure, fw_channel_length, afw, mffwo, &
        mffwpo, no90fw, no180fw, etaiso, coolant, 'Outboard first wall')
    htpmw_blkto = pumppower(inlet_temp, outlet_temp, blpressure, bzfllengo, afw, mfblkto, &
        mfblktpo, no90bz, no180bz, etaiso, coolwh, 'Outboard blanket')

    ! Total inboard & outboard FW and blanket pumping powers (MW)
    htpmw_fw = htpmw_fwi + htpmw_fwo
    htpmw_blkt = htpmw_blkti + htpmw_blkto

    ! Peak first wall temperature (K)
    tpeak = max(tpeakfwi, tpeakfwo)

    ! Totals
    ! Total coolant mass flow rate in the first wall (kg/s)
    mffw = mffwi + mffwo

    ! Total coolant mass flow rate in the blanket (kg/s)
    mfblkt = mfblkti + mfblkto

    ! output !
    ! !!!!!!!!!

    if (ip == 0) return
    call oheadr(ofile, 'First wall and blanket thermohydraulics: Summary')
    call ovarin(ofile, 'Blanket coolant type (1=He, 2=H20)', '(coolwh)',coolwh)
    cstring = '"'//fwcoolant//'"'
    call ovarst(ofile, 'First wall coolant type', '(fwcoolant)',cstring)
    call ovarre(ofile, 'Wall thickness of first wall cooling channels (m)', '(fw_wall)',fw_wall)
    call ovarre(ofile, 'Radius of first wall cooling channels (m)', '(afw)', afw)
    call ovarre(ofile, 'Roughness of first wall cooling channels (m)', '(roughness)', roughness)

    call ovarrf(ofile, 'Inlet temperature of first wall coolant (K)', '(fwinlet)', fwinlet)
    call ovarrf(ofile, 'Outlet temperature of first wall coolant (K)', '(fwoutlet)', fwoutlet)
    call ovarrf(ofile, 'Inlet temperature of blanket coolant (K)', '(inlet_temp)', inlet_temp)
    call ovarrf(ofile, 'Outlet temperature of blanket coolant (K)', '(outlet_temp)', outlet_temp)

    call ovarre(ofile, 'First wall coolant mass flow rate (kg/s)', '(mffw)', mffw, 'OP ')
    call ovarre(ofile, 'Blanket coolant mass flow rate (kg/s)', '(mfblkt)', mfblkt, 'OP ')
    ! Total coolant flow rate (if they are the same coolant)
    if (((fwcoolant=='helium').and.(coolwh==1)).or.((fwcoolant=='water').and.(coolwh==2))) then
        mftotal = mffw + mfblkt
        call ovarre(ofile, 'Total coolant mass flow rate(kg/s)', '(mftotal)', mftotal, 'OP ')
    end if
    call ovarre(ofile, 'First wall coolant pressure (Pa)', '(fwpressure)', fwpressure)
    call ovarre(ofile, 'Blanket coolant pressure (Pa)', '(blpressure)', blpressure)

    call ovarrf(ofile, 'Allowable temperature of first wall material, excluding armour (K)', & 
      '(tfwmatmax)', tfwmatmax)
    call ovarrf(ofile, 'Actual peak temperature of first wall material (K)', '(tpeak)', &
      tpeak, 'OP ')

    call ovarin(ofile, 'Switch for pumping of primary coolant', '(primary_pumping)', &
      primary_pumping)
    if (primary_pumping == 0) then
      call ocmmnt(ofile, 'User sets mechanical pumping power directly')
    else if (primary_pumping == 1) then
      call ocmmnt(ofile, 'User sets mechanical pumping power as a fraction of thermal &
        &power removed by coolant')
    else if (primary_pumping == 2) then
      call ocmmnt(ofile, 'Mechanical pumping power is calculated for first wall and blanket')
    end if

     call ovarre(ofile, 'Pumping power for first wall (MW)', '(htpmw_fw)', htpmw_fw, 'OP ')
     call ovarre(ofile, 'Pumping power for blanket (MW)', '(htpmw_blkt)', htpmw_blkt, 'OP ')
     call ovarre(ofile, 'Pumping power for divertor (MW)', '(htpmw_div)', htpmw_div, 'OP ')
     call ovarre(ofile, 'Pumping power for shield and vacuum vessel (MW)', '(htpmw_shld)', htpmw_shld, 'OP ')
     call ovarre(ofile, 'Total coolant pumping power: first wall, blanket, shield and divertor (MW)', '(htpmw)', htpmw, 'OP ')

   end subroutine thermo_hydraulic_model

   function pumppower(temp_in, temp_out, pressure, flleng, rad, mf, mfp, no90, no180, etaiso, coolant, label)
     !! Routine to calculate the coolant pumping power in MW in the first
     !! wall and breeding zone
     !! author: P J Knight, CCFE, Culham Science Centre
     !! temp_in     : input real : inlet temperature (K)
     !! temp_out    : input real : outlet temperature (K)
     !! pressure    : input real : outlet coolant pressure (Pa)
     !! flleng      : input real : total flow length along pipe (m)
     !! rad         : input real : pipe inner radius (m)
     !! mf          : input real : total coolant mass flow rate in (kg/s)
     !! mfp         : input real : coolant mass flow rate per pipe (kg/s)
     !! no90        : input integer : number of 90 degree bends in pipe
     !! no180       : input integer : number of 180 degree bends in pipe
     !! etaiso      : input real : isentropic efficiency of coolant pumps
     !! coolant     : input integer: coolant fluid (1=helium, 2=water)
     !! label       : input string: description of this calculation
     !! This routine calculates the power required (MW) to pump the coolant in the
     !! first wall and breeding zone.
     !! <P>Pressure drops are calculated for a pipe with a number of 90
     !! and 180 degree bends.  The pressure drop due to frictional forces along
     !! the total straight length of the pipe is calculated, then the pressure
     !! drop due to the bends is calculated.  The total pressure drop is the sum
     !! of all contributions.
     !! The pumping power is be calculated in the most general way,
     !! using enthalpies before and after the pump.
     !! WCLL DDD, WP12-DAS02-T03, J. Aubert et al, EFDA_D_2JNFUP
     !! A Textbook on Heat Transfer, S.P. Sukhatme, 2005
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use constants, only: pi
      use fwbs_variables, only: coolwh
      use process_output, only: oheadr, ovarre, ocmmnt, oblnkl
      use fw_module, only: friction
      use refprop_interface, only: fluid_properties, enthalpy_ps
      use global_variables, only: verbose
     implicit none

     ! Function return parameter: Calculated pumping power (MW)
     real(dp) :: pumppower

     ! Arguments !
     ! !!!!!!!!!!!!

     real(dp), intent(in) :: flleng, rad, mf, mfp, etaiso
     real(dp), intent(in) :: temp_in, temp_out, pressure
     integer, intent(in) :: no90, no180, coolant
     character(len=*), intent(in) :: label

     ! Local variables !
     ! !!!!!!!!!!!!!!!!!!

     ! Inlet pressure (Pa)
     real(dp) :: coolpin

     ! Coolant pressure drop (Pa)
     real(dp) :: deltap

     ! Hydraulic diameter (circular channels assumed) (m)
     real(dp) :: dh

     ! Fluid specific enthalpy from refprop (J/kg)
     real(dp) :: h1

     ! enthalpy
     !real(dp) ::

     real(dp) :: h2, kelbwn, kelbwt, kstrght, &
          lambda, reyn, rhof, s1, s2, viscf, xifn, xift, ximn, ximt, vv, &
          temp_mean,pdropstraight, pdrop90, pdrop180

     ! TODO Variables that appear not to be used below. Check again before removing
     !real(dp) :: cf

     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     if ((temp_in<100.0d0).or.(temp_in>1500.0d0).or.(temp_in/=temp_in)) call write_output

     if ((temp_out<100.0d0).or.(temp_out>1500.0d0).or.(temp_out/=temp_out)) call write_output

     if ((pressure<1.0d5).or.(pressure>1.0d9).or.(pressure/=pressure)) call write_output

     if ((mfp<1.0d-8).or.(mfp>1.0d0).or.(mfp/=mfp)) call write_output

     ! Mean properties
     temp_mean = (temp_in + temp_out)/2.0d0

     ! Calculate coolant fluid properties
     call fluid_properties(temp_mean, pressure, coolwh, density=rhof, viscosity=viscf, label='2001')

     ! Check that coolant density is within bounds and not a NaN/Inf
     if ((rhof>1.0d9).or.(rhof<=0.0d0).or.(rhof/=rhof)) then
         write(*,*)'Error in pumppower.  rhof = ', rhof
         stop 1
     end if

     ! Hydraulic diameter (circular channels assumed) (m)
     dh = 2.0D0*rad

     ! Flow velocity (m.s)
     vv = mfp / (rhof*pi*rad*rad)

     ! Reynolds number
     reyn = rhof * vv * dh / viscf

     ! Calculate Darcy friction factor
     call friction(reyn,lambda)

     ! Straight section pressure drop coefficient
     kstrght = lambda * flleng/dh

     ! 90 degree elbow pressure drop coefficient
     ! Elbow radius assumed = 0.018m, from WCLL
     ximn = 0.21D0 / sqrt(0.018D0/dh)  !  singularity coefficient
     xifn = 0.0175D0*lambda*0.018D0*90.0D0/dh  !  friction coefficient
     kelbwn = ximn + xifn

     ! 180 degree elbow pressure drop coefficient
     ! Elbow radius assumed half that of 90 deg case
     ximt = (0.7D0 + 0.35D0*180.0D0/90.0D0) * 0.21D0 / sqrt(0.009D0/dh)
     xift = 0.0175D0*lambda*0.018D0*90.0D0/dh  !+PJK... 90 or 180?
     kelbwt = ximt + xift

     ! Total pressure drop (Pa)
     pdropstraight = kstrght * 0.5D0*rhof*vv*vv
     pdrop90 = no90*kelbwn * 0.5D0*rhof*vv*vv
     pdrop180 = no180*kelbwt * 0.5D0*rhof*vv*vv
     deltap = pdropstraight + pdrop90 + pdrop180

     ! Pumping power

     ! Inlet pressure (Pa)
     ! Here we are approximating the outlet pressure as 'pressure'.
     coolpin = pressure + deltap

     ! Obtain inlet enthalpy and entropy from inlet pressure and temperature
     if ((coolpin>1.0d9).or.(coolpin<=0.0d0).or.(coolpin/=coolpin)) call write_output

     !
     call fluid_properties(temp_in, coolpin, coolant, enthalpy=h2, entropy=s2, label='2049')

     ! Assume isentropic pump so that s1 = s2
     s1 = s2

     ! Get specific enthalpy at the outlet (J/kg) before pump using pressure and entropy s1
     call enthalpy_ps(pressure, s1, coolant, h1)

     ! Pumping power (MW) is given by enthalpy change, with a correction for
     ! the isentropic efficiency of the pump.
     pumppower = 1.0D-6 * mf * (h2-h1) / etaiso

     ! Output !
     ! !!!!!!!!!

     if (ip  == 1) call write_output

     contains
       subroutine write_output

         call oheadr(ofile, 'Pumping power for ' // label)
         call ovarre(ofile, 'Viscosity (Pa.s)', '(viscf)', viscf, 'OP ')
         call ovarre(ofile, 'Density (kg/m3)', '(rhof)', rhof, 'OP ')
         call ovarre(ofile, 'Velocity (m/s)', '(vv)', vv, 'OP ')
         call ovarre(ofile, 'Reynolds number', '(reyn)', reyn, 'OP ')
         call ovarre(ofile, 'Darcy friction factor', '(lambda)', lambda, 'OP ')
         call ovarre(ofile, 'Channel length', '(flleng)', flleng, 'OP ')
         call ovarre(ofile, 'Pressure drop (Pa)', '(deltap)', deltap, 'OP ')
         call ocmmnt(ofile, 'This is the sum of the following:')
         call ovarre(ofile, '            Straight sections (Pa)', '(pdropstraight)', pdropstraight, 'OP ')
         call ovarre(ofile, '            90 degree bends (Pa)', '(pdrop90)', pdrop90, 'OP ')
         call ovarre(ofile, '            180 degree bends (Pa)', '(pdrop180)', pdrop180, 'OP ')
         call ovarre(ofile, 'Inlet pressure (Pa)', '(coolpin)', coolpin, 'OP ')
         call ovarre(ofile, 'Total coolant mass flow rate in (kg/s)', '(mf)', mf, 'OP ')
         call ovarre(ofile, 'Coolant mass flow rate in one channel (kg/s)', '(mfp)', mfp, 'OP ')
         call ovarre(ofile, 'Pumping power (MW)', '(pumppower)', pumppower, 'OP ')
         call ocmmnt(ofile, 'Additional information is printed when verbose = 1')
         if (verbose==1) then
             call oblnkl(ofile)
             call ovarre(ofile, 'Straight section pressure drop coefficient', '(kstrght)', kstrght, 'OP ')
             call ovarre(ofile, '90 degree elbow singularity coefficient', '(ximn)', ximn, 'OP ')
             call ovarre(ofile, '90 degree elbow friction coefficient', '(xifn)', xifn, 'OP ')
             call ovarre(ofile, '180 degree elbow singularity coefficient', '(ximt)', ximt, 'OP ')
             call ovarre(ofile, '180 degree elbow friction coefficient', '(xift)', xift, 'OP ')
             call ovarre(ofile, 'Inlet specific enthalpy (J/kg)', '(h2)', h2, 'OP ')
             call ovarre(ofile, 'Specific enthalpy before pump (J/kg)', '(h1)', h1, 'OP ')
             call ovarre(ofile, 'Specific enthalpy added by pump (J/kg)', '(h2-h1)', h2-h1, 'OP ')
         end if

     end subroutine

   end function pumppower

  subroutine blanket_mod_pol_height
    !! Calculations for blanket module poloidal height
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculations for blanket module poloidal height for D shaped and elliptical machines
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: scrapli, scraplo
    use fwbs_variables, only: fwbsshape, nblktmodpi, fdiv, nblktmodpo
    use physics_variables, only: itart, rminor, rmajor, triang

    implicit none

    ! Local variables

    ! Mid-plane distance from inboard to outboard side (m)
    real(dp) :: a

    ! Internal half-height of blanket (m)
    real(dp) :: b

    ! Calculate ellipse circumference using Ramanujan approximation (m)
    real(dp) :: ptor

    ! Major radius where half-ellipses 'meet' (m)
    real(dp) :: r1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if ((itart == 1).or.(fwbsshape == 1)) then  ! D-shaped machine

      ! Segment vertical inboard surface (m)
      bllengi = (2.0D0*hblnkt) / nblktmodpi

      ! Calculate perimeter of ellipse that defines the internal
         ! surface of the outboard first wall / blanket

      ! Mid-plane distance from inboard to outboard side (m)
      a = scrapli + 2.0D0*rminor + scraplo

      ! Internal half-height of blanket (m)
      b = hblnkt

      ! Calculate ellipse circumference using Ramanujan approximation (m)
      ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

      ! Calculate blanket poloidal length and segment, subtracting divertor length (m)
      bllengo = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpo

    ! shape defined by two half-ellipses
    else

      ! Major radius where half-ellipses 'meet' (m)
      r1 = rmajor - rminor*triang

      ! Internal half-height of blanket (m)
      b = hblnkt

      ! Distance between r1 and nearest edge of inboard first wall / blanket (m)
      a = r1 - (rmajor - rminor - scrapli)

      ! Calculate ellipse circumference using Ramanujan approximation (m)
      ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

      ! Calculate inboard blanket poloidal length and segment, subtracting divertor length (m)
      ! Assume divertor lies between the two ellipses, so fraction fdiv still applies
      bllengi = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpi

      ! Distance between r1 and inner edge of outboard first wall / blanket (m)
      a = rmajor + rminor + scraplo - r1

      ! Calculate ellipse circumference using Ramanujan approximation (m)
      ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

      ! Calculate outboard blanket poloidal length and segment, subtracting divertor length (m)
      bllengo = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpo
      
    end if
    
  end subroutine blanket_mod_pol_height
     
  subroutine fast_neutron_flux(dr_fw, dr_bz, dr_mf, dr_sh, dr_vv, f_vol_steel_bz, &
    f_vol_pbli_bz, fnflux)
    !! KIT HCLL blanket model fast_neutron_flux
    !! author: J Morris, CCFE, Culham Science Centre
    !! This routine calculates the fast neutron flux on the TF coil for
    !! the KIT HCLL blanket model.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    use physics_variables, only: powfmw

    implicit none

    ! Arguments !
    ! !!!!!!!!!!!!
    
    ! Radial thicknesses of FW, BZ, MF, shield, VV for inboard or outboard (m)
    real(dp), intent(in) :: dr_fw, dr_bz, dr_mf, dr_sh, dr_vv
  
    ! Volume fraction of steel and PbLi in BZ (%)
    real(dp), intent(in) :: f_vol_steel_bz, f_vol_pbli_bz
  
    ! Fast Neutron Flux (cm-2 sec-1)
    real(dp), intent(out) :: fnflux
    ! Local variables

    ! Exponential factor
    real(dp) :: f

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Fast Neutron Flux formula (from CEA study)

    f = -3.916E-2*dr_fw + 5.091E-3*dr_fw**2.0D0 - &
      9.603E-1*dr_bz*(1.859D0*f_vol_steel_bz**2.0D0 + 2.086D0*f_vol_steel_bz + 1.0D0)* &
      (2.033E-2*f_vol_pbli_bz - 1.093E-3) - &
      1.725E-2*dr_mf - 1.195E-1*dr_sh - 5.659E-2*dr_vv + 14.805

    ! Fast Neutron Flux (cm-2 sec-1)
    fnflux = powfmw/2385.0D0*(10.0D0**f)

  end subroutine fast_neutron_flux

  subroutine display_output
    !! KIT HCLL blanket model output
    !! author: J Morris, CCFE, Culham Science Centre
    !! This routine outputs the results for the KIT HCLL blanket model.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwith, fwoth
    use fwbs_variables, only: pradfw, pnucfw, pnucblkt, tbr, primary_pumping
    use process_output, only: oheadr, osubhd, ovarrf, ovarre, ocmmnt, ovarin
    use heat_transport_variables, only: htpmw_fw, htpmw_blkt, htpmw_div, &
      htpmw_shld, htpmw

    implicit none

    call oheadr(ofile, 'First wall and blanket : KIT HCLL model')

    call osubhd(ofile, 'Blanket segmentation :')

    call ovarrf(ofile, 'Toroidal length of inboard  blanket module (m)', &
      '(dt_bb_ib)', dt_bb_ib, 'OP ')
    call ovarrf(ofile, 'Poloidal length of inboard  blanket module (m)', &
      '(dp_bb_ib)', dp_bb_ib, 'OP ')
    call ovarrf(ofile, 'Radial length of inboard  blanket module (m)', &
      '(dr_bb_ib)', dr_bb_ib, 'OP ')
    call ovarrf(ofile, 'Toroidal length of outboard  blanket module (m)', & 
      '(dt_bb_ob)', dt_bb_ob, 'OP ')
    call ovarrf(ofile, 'Poloidal length of outboard  blanket module (m)', &
      '(dp_bb_ob)', dp_bb_ob, 'OP ')
    call ovarrf(ofile, 'Radial length of outboard  blanket module (m)', &
      '(dr_bb_ob)', dr_bb_ob, 'OP ')
    call ovarrf(ofile, 'Average blanket poloidal perimeter (m)', &
      '', len_act_ib+len_act_ob, 'OP ')

    call ocmmnt(ofile, 'Inboard/outboard total first wall area :')

    call ovarrf(ofile, 'Number of inboard poloidal modules for a blanket sector', &
      '(nb_pol_ib)', nb_pol_ib, 'OP ')
    call ovarrf(ofile, 'Number of inboard toroidal modules for a blanket sector', &
      '(nb_tor_ib)', nb_tor_ib, 'OP ')
    call ovarrf(ofile, 'Number of outboard poloidal modules for a blanket sector', &
      '(nb_pol_ob)', nb_pol_ob, 'OP ')
    call ovarrf(ofile, 'Number of outboard toroidal modules for a blanket sector', &
      '(nb_tor_ob)', nb_tor_ob, 'OP ')

    call osubhd(ofile, 'Material composition :')

    call ocmmnt(ofile, 'First Wall :')

    call ovarrf(ofile, 'Inboard radial first wall thickness (m)', '(fwith)', fwith)
    call ovarrf(ofile, 'Outboard radial first wall thickness (m)', '(fwoth)', fwoth)
    call ovarrf(ofile, 'He volume in first wall (m3)', '(vol_he_fw)', vol_he_fw)
    call ovarrf(ofile, 'Steel volume in first wall (m3)', '(vol_steel_fw)', vol_steel_fw)
    call ovarrf(ofile, 'Total first wall volume (m3)', '(vol_fw)', vol_fw, 'OP ')
    call ovarrf(ofile, 'Volume fraction of He in first wall', &
      '(frac_vol_he_fw)', frac_vol_he_fw, 'OP ')
    call ovarrf(ofile, 'Volume fraction of steel in first wall', &
      '(frac_vol_steel_fw)', frac_vol_steel_fw, 'OP ')

    call ocmmnt(ofile, 'Breeding zone :\n')

    call ovarrf(ofile, 'Inboard radial breeder zone thickness (m)', '(dr_bz_ib)', dr_bz_ib)
    call ovarrf(ofile, 'Outboard radial breeder zone thickness (m)', '(dr_bz_ob)', dr_bz_ob)
    call ovarrf(ofile, 'Inboard toroidal breeder zone thickness (m)', '(dt_bz_ib)', dt_bz_ib)
    call ovarrf(ofile, 'Outboard toroidal breeder zone thickness (m)', '(dt_bz_ob)', dt_bz_ob)
    call ovarrf(ofile, 'Inboard poloidal breeder zone thickness (m)', '(dp_bz_ib)', dp_bz_ib)
    call ovarrf(ofile, 'Outboard poloidal breeder zone thickness (m)', '(dp_bz_ob)', dp_bz_ob)
    call ovarrf(ofile, 'He volume in breeder zone (m3)', '(vol_he_bz)', vol_he_bz)
    call ovarrf(ofile, 'Steel volume in the breeder zone (m3)', '(vol_steel_bz)', vol_steel_bz)
    call ovarrf(ofile, 'PbLi volume in the breeder zone (m3)', '(vol_pbli_bz)', vol_pbli_bz)
    call ovarrf(ofile, 'Total breeder zone volume (m3)', '(vol_bz)', vol_bz)
    call ovarrf(ofile, 'Volume fraction of He in breeder zone', &
      '(frac_vol_he_bz)', frac_vol_he_bz)
    call ovarrf(ofile, 'Volume fraction of steel in the breeder zone', &
      '(frac_vol_steel_bz)', frac_vol_steel_bz)
    call ovarrf(ofile, 'Volume fraction of PbLi in the breeder zone', &
      '(frac_vol_pbli_bz)', frac_vol_pbli_bz)

    call ocmmnt(ofile, 'Manifold region (back plates + back supporting structure) :\n')

    call ovarrf(ofile, 'Inboard radial manifold thickness (m)', '(dr_mf_ib)', dr_mf_ib)
    call ovarrf(ofile, 'Outboard radial manifold thickness (m)', '(dr_mf_ob)', dr_mf_ob)
    call ovarrf(ofile, 'Inboard toroidal manifold thickness (m)', '(dt_mf_ib)', dt_mf_ib)
    call ovarrf(ofile, 'Outboard toroidal manifold thickness (m)', '(dt_mf_ob)', dt_mf_ob)
    call ovarrf(ofile, 'Inboard poloidal manifold thickness (m)', '(dp_mf_ib)', dp_mf_ib)
    call ovarrf(ofile, 'Outboard poloidal manifold thickness (m)', '(dp_mf_ob)', dp_mf_ob)
    call ovarrf(ofile, 'He volume in the manifold (m3)', '(vol_he_mf)', vol_he_mf)
    call ovarrf(ofile, 'Steel volume in the manifold (m3)', '(vol_steel_mf)', vol_steel_mf)
    call ovarrf(ofile, 'PbLi volume in the manifold (m3)', '(vol_pbli_mf)', vol_pbli_mf)
    call ovarrf(ofile, 'Total manifold volume (m3)', '(vol_mf)', vol_mf)
    call ovarrf(ofile, 'Volume fraction of He in the manifold', &
      '(frac_vol_he_mf)', frac_vol_he_fw)
    call ovarrf(ofile, 'Volume fraction of the steel in the manifold', &
      '(frac_vol_steel_mf)', frac_vol_steel_mf)
    call ovarrf(ofile, 'Volume fraction of PbLi in the manifold', &
      '(frac_vol_pbli_mf)', frac_vol_pbli_mf)

    call ocmmnt(ofile, 'Blanket masses :\n')

    call ovarre(ofile, 'Total mass for an inboard blanket segment (kg)', &
      '(mass_segm_ib)', mass_segm_ib, 'OP ')
    call ovarre(ofile, 'Total mass for an outboard blanket segment (kg)', &
      '(mass_segm_ob)', mass_segm_ob, 'OP ')
    call ovarre(ofile, 'Total mass for a blanket sector (kg)', &
      '(mass_sector)', mass_sector, 'OP ')
    call ovarre(ofile, 'Total He mass (whole blanket) (kg)', &
      '(mass_he_blanket)', mass_he_blanket, 'OP ')
    call ovarre(ofile, 'Total PbLi mass (whole blanket) (kg)', &
      '(mass_pbli_blanket)', mass_pbli_blanket, 'OP ')
    call ovarre(ofile, 'Total steel mass (whole blanket) (kg)', &
      '(mass_steel_blanket)', mass_steel_blanket, 'OP ')
    call ovarre(ofile, 'Total W mass (whole blanket) (kg)', &
      '(mass_w_blanket)', mass_w_blanket, 'OP ')
    call ovarre(ofile, 'Total blanket mass (kg)', '(mass_blanket)', mass_blanket, 'OP ')

    call osubhd(ofile, 'Neutronics :')

    call ovarrf(ofile, 'Radiation heating power into the first wall (MW)', &
      '(pradfw)', pradfw, 'OP ')
    call ovarrf(ofile, 'Nuclear heating power into the first wall (MW)', &
      '(pnucfw)', pnucfw, 'OP ')
    call ovarrf(ofile, 'Nuclear heating power into the blanket (MW)', &
      '(pnucblkt)', pnucblkt, 'OP ')
    call ovarrf(ofile, 'Tritium breeding ration', '(TBR)', TBR, 'OP ')
    call ovarre(ofile, 'Inboard TFC fast neutron flux (cm-2 s-1)', &
      '(phi_tfc_ib)', phi_tfc_ib, 'OP ')
    call ovarre(ofile, 'Outboard TFC fast neutron flux (cm-2 s-1)', &
      '(phi_tfc_ob)', phi_tfc_ob, 'OP ')

    call osubhd(ofile, 'Thermal-hydraulics :')

    call ovarin(ofile, 'Switch for pumping of primary coolant', &
      '(primary_pumping)', primary_pumping)
    if (primary_pumping == 0) then
      call ocmmnt(ofile, 'User sets mechanical pumping power directly')
    else if (primary_pumping == 1) then
      call ocmmnt(ofile, 'User sets mechanical pumping power as a fraction of &
        &thermal power removed by coolant')
    else if (primary_pumping == 2) then
      call ocmmnt(ofile, 'Mechanical pumping power is calculated for first wall and blanket')
      ! TODO - should this still be here?
      call ovarrf(ofile, 'Total He mass flow rate (kg/s)', '(w_he)', w_he, 'OP ')
    end if

    call ovarre(ofile, 'Pumping power for first wall (MW)', '(htpmw_fw)', htpmw_fw, 'OP ')
    call ovarre(ofile, 'Pumping power for blanket (MW)', '(htpmw_blkt)', htpmw_blkt, 'OP ')
    call ovarre(ofile, 'Pumping power for divertor (MW)', '(htpmw_div)', htpmw_div, 'OP ')
    call ovarre(ofile, 'Pumping power for shield and vacuum vessel (MW)', &
      '(htpmw_shld)', htpmw_shld, 'OP ')
    call ovarre(ofile, 'Total coolant pumping power: first wall, blanket, shield &
      &and divertor (MW)', '(htpmw)', htpmw, 'OP ')

  end subroutine display_output

end module kit_hcll_module
