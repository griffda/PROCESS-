! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module ccfe_hcpb_module

  !+ad_name  ccfe_hcpb_module
  !+ad_summ  Module containing CCFE HCPB blanket model
  !+ad_type  Module
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_args  N/A
  !+ad_desc  This module contains the PROCESS CCFE HCPB blanket model
  !+ad_desc  based on CCFE HCPB model from the PROCESS engineering paper
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  fwbs_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  tfcoil_variables
  !+ad_hist  10/02/15 JM  Initial version of module
  !+ad_stat  Okay
  !+ad_docs  PROCESS Engineering paper (M. Kovari et al.)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  !  Modules to import
  use build_variables
  use buildings_variables
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables
  use maths_library
  use rfp_variables
  use pfcoil_variables
  use physics_variables
  use process_output
  use refprop_interface
  use tfcoil_variables
  use constants
  
  implicit none

  !  Subroutine declarations
  private
  public :: ccfe_hcpb
  
  !  Precision variable
  integer, parameter :: double = 8

  !  Variables for output to file
  integer, private :: ip, ofile
   
  !  Module variables
  !  Tungsten density (kg/m3)
  real(kind=double), private :: W_density = 19.25D0 * 1000.0D0
  
  !  Smeared densities of build sections
  !  FW armour density
  real(kind=double), private :: armour_density
  
  !  FW density
  real(kind=double), private :: fw_density
  
  !  Blanket density
  real(kind=double), private :: blanket_density
  
  !  Shield density
  real(kind=double), private ::	shield_density
  
  !  Vacuum vessel density
  real(kind=double), private :: vv_density
  
  !  First wall volume
  real(kind=double), private :: volfw
  
  !  Blanket exponent (tonne/m2)
  real(kind=double), private :: x_blanket
  
  !  Shield exponent (tonne/m2)
  real(kind=double), private :: x_shield
  
  !  Unit nuclear heating in TF coil (W per W of fusion power)
  real(kind=double), private :: tfc_nuc_heating
  
  !  Unit heating of FW and armour in FW armour (W/kg per W of fusion power)
  real(kind=double), private :: fw_armour_u_nuc_heating
  
  !  Unit nuclear heating in shield (W per W of fusion power)
  real(kind=double), private :: shld_u_nuc_heating
  
  !  Blanket internal half-height (m)
  real(kind=double), private :: hblnkt
  
  !  Shield internal half-height (m)
  real(kind=double), private :: hshld
  
  !  Clearance between uppermost PF coil and cryostat lid (m)
  real(kind=double), private :: hcryopf
  
  !  Vacuum vessel internal half-height (m)
  real(kind=double), private :: hvv
  
  !  Volume of inboard and outboard shield (m3)
  real(kind=double), private :: volshldi, volshldo
  
  !  Internal half-height of external cryostat (m)
  real(kind=double), private :: zdewex
  
  !  Inboard/outboard FW half thicknesses (m)
  real(kind=double), private :: bfwi, bfwo
  
  !  Inboard/outboard FW coolant void fraction
  real(kind=double), private :: vffwi, vffwo

  !  Surface heat flux on first wall (MW) (sum = pradfw)
  real(kind=double), private :: psurffwi, psurffwo

  !  Inboard/outboard blanket coolant channel length (radial direction) (m)
  real(kind=double), private :: bldepti, bldepto
    
  !  Inboard/outboard blanket mid-plan toroidal circumference for segment (m)
  real(kind=double), private :: blwidti, blwidto

  !  Inboard/outboard blanket segment poloidal length (m)
  real(kind=double), private :: bllengi, bllengo
  
  !  Inboard/outboard first wall flow lengths (m)
  real(kind=double), private :: fwfllengi, fwfllengo
  
  !  Inboard/outboard blanket flow lengths (m)
  real(kind=double), private :: bzfllengi, bzfllengo
  
  !  Inboard/outboard first wall nuclear heating (MW)
  real(kind=double), private :: pnucfwi, pnucfwo

  !  Inboard/outboard first wall peak temperature (K)
  real(kind=double), private :: tpeakfwi, tpeakfwo
  
  !  Inboard/outboard first wall coolant flow rate (m/s)
  real(kind=double), private :: velfwi, velfwo
  
  !  Inboard/outboard total mass flow rate to remove inboard FW power (kg/s)
  real(kind=double), private :: mffwi, mffwo
  
  !  Inboard/utboard total number of pipes
  real(kind=double), private :: npfwi, npfwo
  
  !  Inboard/outboard mass flow rate per coolant pipe (kg/s)
  real(kind=double), private :: mffwpi, mffwpo
  
  !  Neutron power deposited inboard/outboard blanket blanket (MW)
  real(kind=double), private :: pnucblkti, pnucblkto
  
  !  Inboard/outboard blanket mass flow rate for coolant (kg/s)
  real(kind=double), private :: mfblkti, mfblkto
  
  !  Inboard/outboard total num of pipes
  real(kind=double), private :: npblkti, npblkto
  
  !  Inboard/outboard mass flow rate per coolant pipe (kg/s)
  real(kind=double), private :: mfblktpi, mfblktpo
  
  !  Inboard/outboard coolant velocity in blanket (m/s)
  real(kind=double), private :: velblkti, velblkto
  
  !  Inboard/outboard first wall pumping power (MW)
  real(kind=double), private :: htpmw_fwi, htpmw_fwo
  
  !  Inboard/outboard blanket pumping power (MW)
  real(kind=double), private :: htpmw_blkti, htpmw_blkto
  
  !  Total nuclear power deposited in FW, BLKT, SHLD, DIV, TF (MW)
  real(kind=double), private :: nuc_pow_dep_tot
  
  !  Exponential factors in nuclear heating calcs
  real(kind=double), private :: exp_blanket, exp_shield1, exp_shield2
  
  !  Fraction of neutron energy lost by main wall
  real(kind=double), private :: fdep
  
contains
  
  subroutine ccfe_hcpb(outfile, iprint)
    !+ad_name  ccfe_hcpb
    !+ad_summ  CCFE HCPB blanket model
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates nuclear heating for the CCFE HCPB
    !+ad_desc  blanket model.
    !+ad_prob  None
    !+ad_call  component_volumes
    !+ad_call  nuclear_heating_magnets
    !+ad_call  nuclear_heating_fw
    !+ad_call  nuclear_heating_blanket
    !+ad_call  nuclear_heating_shield
    !+ad_call  nuclear_heating_divertor
    !+ad_call  powerflow_calc
    !+ad_call  component_masses
    !+ad_call  write_ccfe_hcpb_output
    !+ad_hist  10/02/15 JM Initial version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Engineering paper (M. Kovari et al.)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint, outfile

    !  Assign module private variables to iprint and outfile
    ip = iprint
    ofile = outfile

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
	!  Calculate FW/Blanket lifetime
	fwlife = min(abktflnc/wallmw, tlife)
	
	!  Coolant type
	coolwh = 1
	
	!  Energy multiplication
	emult = 1.269
	
	!  Calculate blanket, shield, vacuum vessel and external cryostat volumes
	call component_volumes
	
	!  Centrepost heating for a ST machine
	if (itart == 1) then
        pnuccp = st_centrepost_nuclear_heating(pneutmw,hmax,tfcth,rmajor)
    else
        pnuccp = 0.0D0
    end if
           
	!  Calculate the nuclear heating in the magnets
    call nuclear_heating_magnets
    
    !  Calculate the nuclear heating in the fW
    call nuclear_heating_fw
    
    !  Calculate the nuclear heating in the blanket
    call nuclear_heating_blanket
    
    !  Calculate the nuclear heating in the shield
    call nuclear_heating_shield
    
    !  Calculate the nuclear heating in the divertor
    call nuclear_heating_divertor
    
    !  Neutron power to divertor: 0.8D0 * fdiv * powfmw (assume this is all absorbed, no multiplication)
    !  Neutron power to main wall: 0.8D0 * (1-fdiv) * powfmw (assume that all are absorbed)
    !  Total energy deposited in main wall: emult * 0.8D0 * (1-fdiv) * powfmw
    !  Assume that all the neutrons are absorbed. (Not applicable for very thin blankets)   
    
    !  Split neutron power to main wall between fw, bkt, shld and TF with same fractions as before.
    !  Total nuclear power deposited (MW)
	nuc_pow_dep_tot = pnucfw + pnucblkt + pnucshld + ptfnuc
	
	!  Power to the first wall (MW)
	pnucfw = (pnucfw / nuc_pow_dep_tot) * emult * 0.8D0 * (1-fdiv) * powfmw
	
	!  Power to the blanket (MW)
	pnucblkt = (pnucblkt / nuc_pow_dep_tot) * emult * 0.8D0 * (1-fdiv) * powfmw
	
	!  Power to the shield(MW)
	pnucshld = (pnucshld / nuc_pow_dep_tot) * emult * 0.8D0 * (1-fdiv) * powfmw
	
	!  Power to the TF coils (MW)
	ptfnuc = (ptfnuc / nuc_pow_dep_tot) * emult * 0.8D0 * (1-fdiv) * powfmw
	
	!  pnucdiv is not changed.
	!  The energy due to multiplication, by subtraction:
	emultmw = pnucfw + pnucblkt + pnucshld + ptfnuc + pnucdiv - 0.8D0 * powfmw

	!  Calculate the powerflow
    call powerflow_calc
    
    !  Calculate component masses
    call component_masses
    
	if (ip == 0) return
	
	!  Write output to file if chosen
	call write_ccfe_hcpb_output
    
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine component_volumes
	!+ad_name  component_volumes
    !+ad_summ  Calculate the blanket, shield, vacuum vessel and external cryostat volumes
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the blanket, shield, vacuum vessel and external cryostat volumes
    !+ad_prob  None
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
  
    !  Local variables
    real(kind=double) :: hcryopf, r1, r2, r3, v1, v2
           
    !  Calculate blanket half-height
    call blanket_half_height
    
    !  Calculate shield half-height
    call shield_half_height
    
    !  Calculate vacuum vessel half-height
    call vv_half_height
    
    !  D-shaped blanket and shield
    if ((itart == 1).or.(fwbsshape == 1)) then

       call dshaped_blanket
       
       call dshaped_shield
       
       call dshaped_vv

	!  Elliptical blanket and shield
    else
    
	   call elliptical_blanket
	   
	   call elliptical_shield
	   
	   call elliptical_vv

    end if
    
	!  Apply coverage factors to volumes and surface areas
    call apply_coverage_factors
    
    !  Calculate external cryostat geometry
    call external_cryo_geometry
      
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine blanket_half_height
    !+ad_name  blanket_half_height
    !+ad_summ  Calculate the blanket half-height
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the blanket half-height
    !+ad_prob  None
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
	!  Local variables
	real(kind=double) :: hbot, htop
  
	!  Calculate blanket internal lower half-height (m)
    hbot = rminor*kappa + vgap + divfix - blnktth
    
    !  If a double null machine then symmetric
    !  Calculate blanket internal upper half-height (m)  
    if (idivrt == 2) then  
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth)
    end if
    
    !  Average of top and bottom (m)
    hblnkt = 0.5D0*(htop + hbot)
  
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine shield_half_height
    !+ad_name  shield_half_height
    !+ad_summ  Calculate the shield half-height
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the shield half-height
    !+ad_prob  None
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !  Local variables
	real(kind=double) :: hbot, htop
  
	!  Calculate shield internal lower half-height (m)
    hbot = rminor*kappa + vgap + divfix
    
    !  If a double null machine then symmetric
    !  Calculate shield internal upper half-height (m)
    if (idivrt == 2) then  
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) + blnktth
    end if
    
    !  Average of top and bottom (m)
    hshld = 0.5D0*(htop + hbot)
  
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine vv_half_height
    !+ad_name  vv_half_height
    !+ad_summ  Calculate the vacuum vessel half-height
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the vacuum vessel half-height
    !+ad_prob  None
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    !  Local variables
	real(kind=double) :: hbot, htop
  
	!  Calculate vacuum vessel internal lower half-height (m)
    hbot = hmax - vgap2 - ddwi
    
    !  If a double null machine then symmetric
    !  Calculate vacuum vessel internal upper half-height (m)
    if (idivrt == 2) then
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) &
            + blnktth + shldtth
    end if
    
    !  Average of top and bottom (m)
    hvv = 0.5D0*(htop + hbot)
  
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine dshaped_blanket
	!+ad_name  dshaped_blanket
    !+ad_summ  Calculate the blanket surface area and volume using dshaped scheme
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the blanket surface area and volume using dshaped scheme
    !+ad_prob  None
    !+ad_call  dhshellarea
    !+ad_call  dhshellvol
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none
  
    !  Local variables
    real(kind=double) :: r1, r2
  
	!  Major radius to outer edge of inboard blanket (m)
    r1 = rsldi + shldith + blnkith

    !  Horizontal distance between inside edges of blanket (m)
    !  i.e. outer radius of inboard part to inner radius of outboard part
    r2 = fwith + scrapli + 2.0D0*rminor + scraplo + fwoth

    !  Calculate blanket surface area, assuming 100% coverage
    call dshellarea(r1, r2, hblnkt, blareaib, blareaob, blarea)

    !  Calculate blanket volumes, assuming 100% coverage
    call dshellvol(r1, r2, hblnkt, blnkith, blnkoth, blnktth, volblkti, volblkto, volblkt)
   
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine dshaped_shield
	!+ad_name  dshaped_shield
    !+ad_summ  Calculate the shield surface area and volume using dshaped scheme
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the shield surface area and volume using dshaped scheme
    !+ad_prob  None
    !+ad_call  dhshellarea
    !+ad_call  dhshellvol
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
	implicit none
  
    !  Local variables
    real(kind=double) :: r1, r2
  
    !  Major radius to outer edge of inboard shield (m)
    r1 = rsldi + shldith

    !  Horizontal distance between inside edges of shield (m)
    !  i.e. outer radius of inboard part to inner radius of outboard part
    r2 = blnkith + fwith + scrapli + 2.0D0*rminor + scraplo + fwoth + blnkoth

    !  Calculate shield surface area, assuming 100% coverage
    call dshellarea(r1, r2, hshld, shareaib, shareaob, sharea)

    !  Calculate shield volumes, assuming 100% coverage
    call dshellvol(r1, r2, hshld, shldith, shldoth, shldtth, volshldi, volshldo, volshld)
        
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine dshaped_vv
	!+ad_name  dshaped_vv
    !+ad_summ  Calculate the vacuum vessel volume using dshaped scheme
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the vacuum vessel volume using dshaped scheme
    !+ad_prob  None
    !+ad_call  dhshellvol
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
	implicit none
  
    !  Local variables
    real(kind=double) :: r1, r2, v1, v2
    
    !  Major radius to outer edge of inboard section (m)
    r1 = rsldi

    !  Horizontal distance between inside edges (m)
    !  i.e. outer radius of inboard part to inner radius of outboard part
    r2 = rsldo - r1

    !  Calculate volume, assuming 100% coverage
    call dshellvol(r1, r2, hvv, ddwi, ddwi, ddwi, v1, v2, vdewin)

    !  Apply area coverage factor
    vdewin = fvoldw*vdewin
          
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine elliptical_blanket
	!+ad_name  elliptical_blanket
    !+ad_summ  Calculate the blanket surface area and volume using elliptical scheme
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the blanket surface area and volume using elliptical scheme
    !+ad_prob  None
    !+ad_call  ehshellarea
    !+ad_call  eshellvol
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none
  
    !  Local variables
    real(kind=double) :: r1, r2, r3
  
	!  Major radius to centre of inboard and outboard ellipses (m)
    !  (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang

    !  Distance between r1 and outer edge of inboard blanket (m)
    r2 = r1 - (rsldi + shldith + blnkith)

    !  Distance between r1 and inner edge of outboard blanket (m)
    r3 = (rsldo - shldoth - blnkoth) - r1

    !  Calculate blanket surface area, assuming 100% coverage
    call eshellarea(r1, r2, r3, hblnkt, blareaib, blareaob, blarea)

    !  Calculate blanket volumes, assuming 100% coverage
    call eshellvol(r1, r2, r3, hblnkt, blnkith, blnkoth, blnktth, volblkti, volblkto, volblkt)
  
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine elliptical_shield
	!+ad_name  elliptical_shield
    !+ad_summ  Calculate the shield surface area and volume using elliptical scheme
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the shield surface area and volume using elliptical scheme
    !+ad_prob  None
    !+ad_call  ehshellarea
    !+ad_call  ehshellvol
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none
  
    !  Local variables
    real(kind=double) :: r1, r2, r3
  
	!  Major radius to centre of inboard and outboard ellipses (m)
    !  (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang
  
  	!  Distance between r1 and outer edge of inboard shield (m)
	r2 = r1 - (rsldi + shldith)

    !  Distance between r1 and inner edge of outboard shield (m)
    r3 = (rsldo - shldoth) - r1

    !  Calculate shield surface area, assuming 100% coverage
    call eshellarea(r1, r2, r3, hshld, shareaib, shareaob, sharea)

    !  Calculate shield volumes, assuming 100% coverage
    call eshellvol(r1, r2, r3, hshld, shldith, shldoth, shldtth, volshldi, volshldo, volshld)
  
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine elliptical_vv
    !+ad_name  elliptical_vv
    !+ad_summ  Calculate the vacuum vessel volume using elliptical scheme
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate the vacuum vessel volume using elliptical scheme
    !+ad_prob  None
    !+ad_call  ehshellvol
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none
  
    !  Local variables
    real(kind=double) :: r1, r2, r3, v1, v2
  
    !  Major radius to centre of inboard and outboard ellipses (m)
    !  (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang

    !  Distance between r1 and outer edge of inboard section (m)
    r2 = r1 - rsldi

    !  Distance between r1 and inner edge of outboard section (m)
    r3 = rsldo - r1

    !  Calculate volume, assuming 100% coverage
    call eshellvol(r1, r2, r3, hvv, ddwi, ddwi, ddwi, v1, v2, vdewin)

    !  Apply area coverage factor
    vdewin = fvoldw*vdewin
  
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine apply_coverage_factors
    !+ad_name  apply_coverage_factors
    !+ad_summ  Apply coverage factors to volumes
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Apply coverage factors to volumes
    !+ad_prob  None
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none
    
    !  Apply blanket coverage factors
    blareaob = blarea*(1.0D0-fhole-fdiv-fhcd) - blareaib
    blarea = blareaib + blareaob

    volblkto = volblkt*(1.0D0-fhole-fdiv-fhcd) - volblkti
    volblkt = volblkti + volblkto

	!  Apply shield coverage factors
    shareaib = fvolsi*shareaib
    shareaob = fvolso*shareaob
    sharea = shareaib + shareaob

    volshldi = fvolsi*volshldi
    volshldo = fvolso*volshldo
    volshld = volshldi + volshldo
  
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine external_cryo_geometry
    !+ad_name  external_cryo_geometry
    !+ad_summ  Calculate external cryostat geometry
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate external cryostat geometry
    !+ad_prob  None
    !+ad_hist  16/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    implicit none  
  
    !  External cryostat radius (m)
    !  For rfp machines 
    if (irfp == 1) then
    
	    !  rrpf(i) = radius of RFP coil i (RFPs)
        rdewex = maxval(rrpf + 0.5D0*drpf) + rpf2dewar
    
    !  Tokamaks 
    else
        
        !  rb(i) = outer radius of PF coil i (tokamaks)    
        rdewex = maxval(rb) + rpf2dewar
       
    end if

    !  Clearance between uppermost PF coil and cryostat lid (m). 
    !  Scaling from ITER by M. Kovari
    hcryopf = clhsf * (2.0D0*rdewex)/28.440D0

    !  Half-height of cryostat (m)
    !  Tokamak
    if (irfp /= 1) then
    
       zdewex = maxval(zh) + hcryopf
       
    !  rfp machine
    else
    
       zdewex = maxval(zzpf + 0.5D0*dzpf) + hcryopf
       
    end if

    !  Vertical clearance between TF coil and cryostat (m)
    clh1 = zdewex - (hmax + tfcth)

    !  External cryostat volume (m3)
    vdewex = ( (2.0D0*pi*rdewex) * 2.0D0*zdewex + (2.0D0*pi*rdewex**2) ) * ddwex

    !  Vacuum vessel mass (kg)
    cryomass = vdewin * denstl

    !  Sum of internal vacuum vessel and external cryostat masses (kg)
    dewmkg = (vdewin + vdewex) * denstl
  
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_heating_magnets
    !+ad_name  nuclear_heating_magnets
    !+ad_summ  Nuclear heating in the magnets for CCFE HCPB model
    !+ad_type  Subroutine
    !+ad_auth  Michael Kovari, CCFE, Culham Science Centre
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  This subroutine calculates the nuclear heating in the 
    !+ad_desc  coils.
    !+ad_prob  None
    !+ad_hist  10/02/15 MDK Initial version
    !+ad_hist  10/02/15 JM  First complete version
    !+ad_stat  Okay
    !+ad_docs  PROCESS Engineering paper (M. Kovari et al.)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Local variables
    real(kind=double) :: a, b, e
    real(kind=double) :: vffwi, vffwo, vffwm
    
    !  Model factors and coefficients
    a = 2.830D0       	! Exponential factor (m2/tonne)
    b = 0.583D0       	! Exponential factor (m2/tonne)
    e = 18.5	       	! Pre-factor (1/kg)
    
    !  Half-widths of inboard and outboard first wall
    bfwi = 0.5D0*fwith
    bfwo = 0.5D0*fwoth
      
	!  First wall void fractions
    vffwi = afwi*afwi/(bfwi*bfwi)  !  inboard FW coolant void fraction
    vffwo = afwo*afwo/(bfwo*bfwo)  !  outboard FW coolant void fraction
    vffwm = (vffwi + vffwo) / 2    !  mean FW coolant void fraction
    
	!  Calculate smeared densities of blanket sections
	!  gaseous He coolant in armour, FW & blanket: He mass is neglected
	armour_density = W_density*(1-vffwm)
	fw_density = denstl*(1-vffwm)
	blanket_density = whtblkt / volblkt
	shield_density = whtshld / volshld
	vv_density = cryomass / vdewin
	
	!  Exponents (tonne/m2)
	!  Blanket exponent (/1000 for kg -> tonnes)
    x_blanket = (armour_density * fw_armour_thickness + &
				fw_density * (fwith+fwoth)/2.0D0 + &
				blanket_density * (blnkith+blnkoth)/2.0D0) / 1000.0D0
				
    !  Shield exponent(/1000 for kg -> tonnes)
    x_shield = (shield_density * (shldith+shldoth)/2.0D0 + &
				vv_density * ddwi) / 1000.D0
    
	!  Nuclear heating in TF coil
	!  Unit heating (W/kg/GW of fusion power) x mass (kg)
    tfc_nuc_heating = e*exp(-a*x_blanket)*exp(-b*x_shield) * whttf
    
    !  Total heating (MW)
    ptfnuc = tfc_nuc_heating * (powfmw / 1000.0D0) / 1.0D6
    
    if (ip == 0) return
    
    call oheadr(ofile, 'Nuclear Heating Magnets') 
    !call ovarin(ofile, '', '()',)
    call ovarre(ofile, 'Shield exponent (tonne/m2)', '(x_shield)', x_shield)
    call ovarre(ofile, 'Blanket exponent (tonne/m2)', '(x_blanket)', x_blanket)
    call ovarre(ofile, 'Unit nuclear heating in TF coil (W/GW)', '(tfc_nuc_heating)', tfc_nuc_heating)
    call ovarre(ofile, 'Total nuclear heating in TF coil (MW)', '(ptfnuc)', ptfnuc)
  
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_heating_fw
	!+ad_name  nuclear_heating_fw
    !+ad_summ  Nuclear heating in the FW for CCFE HCPB model
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  This subroutine calculates the nuclear heating in the FW
    !+ad_prob  None
    !+ad_hist  11/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
  
    !  Unit heating of FW and armour (W/kg per W of fusion power)
    fw_armour_u_nuc_heating = 6.25D-7
    
    !  Total nuclear heating in FW (MW)
    pnucfw = fwmass * fw_armour_u_nuc_heating * powfmw
  
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_heating_blanket
	!+ad_name  nuclear_heating_blanket
    !+ad_summ  Nuclear heating in the blanket for CCFE HCPB model
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  This subroutine calculates the nuclear heating in the blanket
    !+ad_prob  None
    !+ad_hist  11/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Local variables
    real(kind=double) :: a, b, mass
    
    !  Blanket nuclear heating coefficient and exponent
    a = 0.764D0
    b = 2.476D-3  ! 1/tonne
    
    !  Mass of the blanket in tonnes
    mass = whtblkt / 1000.0D0
    
    !  Total blanket nuclear heating (MW)
    exp_blanket = 1-exp(-b*mass)
    pnucblkt = powfmw * a * exp_blanket
  
  end subroutine
 
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
  subroutine nuclear_heating_shield
  	!+ad_name  nuclear_heating_shield
    !+ad_summ  Nuclear heating in the shield for CCFE HCPB model
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  This subroutine calculates the nuclear heating in the shield
    !+ad_prob  None
    !+ad_hist  11/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Local variables
    real(kind=double) :: f, g, h, y
    
    ! Shield nuclear heating coefficients and exponents
    f = 6.88D2  ! W/kg/W of fusion power
    g = 2.723D0  ! m2/tonne
    h = 0.798D0  ! m2/tonne
    y = (shield_density/1000.D0) * (shldith+shldoth)/2.0D0
        
    !  Unit nuclear heating of shield (W/kg/GW of fusion power) x mass
    exp_shield1 = exp(-g * x_blanket)
    exp_shield2 = exp(-h * y)
    shld_u_nuc_heating = whtshld * f * exp_shield1 * exp_shield2
    
    !  Total nuclear heating in shield (MW)
    pnucshld = shld_u_nuc_heating * (powfmw / 1000.D0) / 1.0D6
    
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine nuclear_heating_divertor
    !+ad_name  nuclear_heating_divertor
    !+ad_summ  Nuclear heating in the divertor for CCFE HCPB model
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  This subroutine calculates the nuclear heating in the divertor
    !+ad_prob  None
    !+ad_hist  11/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Unfortunately the divertor heating was not tallied in the neutronics calcs
    !  Assume that all the neutron energy + energy multiplication is absorbed in the reactor + 
    !  coils. It turns out that emult is also approx constant, but this is not used. No energy 
    !  multiplication in the divertor
    
    !  Overwrite global variable for fdiv
    fdiv = 0.115D0
    
    !  Nuclear heating in the divertor just the neutron power times fdiv
    pnucdiv = 0.8D0 * powfmw * fdiv
    
    !  No heating of the H & CD
    pnuchcd = 0.0D0
  
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine powerflow_calc
    !+ad_name  powerflow_calc
    !+ad_summ  Calculations for powerflow
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculations for powerflow
    !+ad_prob  None
    !+ad_hist  11/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Local Variables

    !  Radiation power incident on divertor (MW)
    praddiv = pradmw * fdiv

    !  Radiation power incident on HCD apparatus (MW)
    pradhcd = pradmw * fhcd

    !  Radiation power lost through holes (eventually hits shield) (MW)
    pradloss = pradmw * fhole

    !  Radiation power incident on first wall (MW)
    pradfw = pradmw - praddiv - pradloss - pradhcd

    !  If we have chosen pressurised water as the coolant, set the
    !  coolant outlet temperature as 20 deg C below the boiling point
    !if (coolwh == 2) then
    !    outlet_temp = tsat_refprop(coolp*1.0D6, coolwh) - 20.0D0  !  in K
    !end if

    !  Surface heat flux on first wall (MW) (sum = pradfw)
    psurffwi = (pradfw + porbitlossmw + palpfwmw) * fwareaib/fwarea
    psurffwo = (pradfw + porbitlossmw + palpfwmw) * fwareaob/fwarea
	
	!  Simple model
	if ((secondary_cycle == 0).or.(secondary_cycle == 1)) then
	
		!  First wall pumping power (MW)
		htpmw_fw = fpumpfw * (pnucfw + psurffwi + psurffwo)

		!  Blanket pumping power (MW)
		htpmw_blkt = fpumpblkt * pnucblkt
	
	!  Detailed model
	else
		
		call thermo_hydraulic_model		
		
	end if
	
    !  Calculate coolant pumping powers from input fraction.  
    !  The pumping power is assumed to be a fraction, fpump, of the incident
    !  thermal power to each component so that,
    !     htpmw_i = fpump_i*C
    !  where C is the non-pumping thermal power deposited in the coolant

    !  Shield pumping power (MW)
    htpmw_shld = fpumpshld*(pnucshld)

    !  Divertor pumping power (MW)
    htpmw_div = fpumpdiv*(pdivt + pnucdiv + praddiv)

  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine thermo_hydraulic_model
	!+ad_name  thermo_hydraulic_model
    !+ad_summ  Calculations for detailed thermo-hydraulic model for fw
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculations for detailed powerflow model secondary_cycle > 0
    !+ad_prob  None
    !+ad_hist  23/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Local variables
    real(kind=double) :: cf, rhof
    integer :: no90fw, no180fw, no90bz, no180bz

    !  Determine size of blanket modules
    !  Length of coolant pipes is assumed to be 80% of total radial space
    !  available for blanket, allowing for connections, manifolds etc.
    bldepti = 0.8D0 * blnkith
    bldepto = 0.8D0 * blnkoth

    !  Using the total perimeter of the machine, segment the outboard
    !  blanket into nblktmodp*nblktmodt modules, all assumed to be the same size

    !  Calculate mid-plane toroidal circumference and segment
    blwidti = (2.0D0*pi*(rmajor - rminor - scrapli)) / nblktmodti
    blwidto = (2.0D0*pi*(rmajor + rminor + scraplo)) / nblktmodto
        
    !  Calculate poloidal height of blanket modules
    call blanket_mod_pol_height
            
    !  Calculate total flow lengths
    !  First wall flow is assumed to follow a radial-poloidal-radial path
    fwfllengi = 2.0D0*bldepti + bllengi
    fwfllengo = 2.0D0*bldepto + bllengo

    !  Blanket flow is assumed to follow a rad-pol-rad-rad-pol-rad path
    bzfllengi = 4.0D0*bldepti + 2.0D0*bllengi
    bzfllengo = 4.0D0*bldepto + 2.0D0*bllengo

    !  Number of angle turns in FW and blanket flow channels, consistent with flow lengths defined
    no90fw = 2
    no180fw = 0
    no90bz = 4
    no180bz = 1
    
    !  Nuclear power deposited in fw inner and outer
    !pnucfwi = pnucfw * fwareaib/(fwareaib + fwareaob)
    !pnucfwo = 

    !  Start thermal hydraulic calculations with inboard side. Calc of max FW temperature.
    !  Includes fraction of escaped alpha power as a component of the inboard wall surface power.
    call iterate_fw(afwi, bfwi, fwareaib, (psurffwi+fwareaib/fwarea*palpfwmw), bllengi, &
	    pnucfwi, tpeakfwi, cf, rhof, velfwi)

    !  Adjust first wall thickness if bfwi has been changed
    fwith = 2.0D0*bfwi
    !vffwi = (afwi/bfwi)**2
    vffwi = (pi*afwi*afwi)/((2.0D0*bfwi)**2.0D0)

    !  Total mass flow rate to remove inboard first wall power (kg/s)
    mffwi = 1.0D6*(pnucfwi + psurffwi) / (cf*(outlet_temp-inlet_temp))

    !  Calculate total number of pipes from coolant fraction and channel dimensions
    !npfwi = (vffwi*fwith*fwareaib)/(pi*afwi*afwi*fwfllengi)
    npfwi = (vffwi*fwith*fwareaib)/(pi*afwi*afwi*bllengi)

    !  Mass flow rate per coolant pipe (kg/s)
    mffwpi = mffwi/npfwi

    !  Neutron power deposited in inboard blanket (MW)
    pnucblkti = pnucblkt * volblkti / volblkt
	
    !  Assume inlet and outlet temps for blanket are the same as those for the FW. Channel inner 
    !  diameter is also the same. Calculate mass flow rate for inboard blanket coolant (kg/s)
    mfblkti = 1.0D6*(pnucblkti) / (cf*(outlet_temp-inlet_temp))

    !  Calc total num of pipes (in all inboard modules) from coolant frac and channel dimensions
    !  Assumes up/down flow, two 90 deg bends per length
    npblkti = (vfblkt * volblkti) / (pi * afwi * afwi * bzfllengi)

    !  Mass flow rate per coolant pipe (kg/s)
    mfblktpi = mfblkti / npblkti

    !  Coolant velocity in blanket (m/s)
    velblkti = mfblktpi / (pi * afwi * afwi * rhof)

    !  Should check max temp in the blanket is below necessary limits; this is not straightforward
    !  Calculate pumping powers for blanket and first wall (MW)
    htpmw_fwi = pumppower(fwfllengi, afwi, mffwi, mffwpi, no90fw, no180fw, velfwi, etaiso, coolwh)
    htpmw_blkti = pumppower(bzfllengi, afwi, mfblkti, mfblktpi, no90bz, no180bz, velblkti, etaiso, coolwh)

    !  Repeat thermal hydraulic calculations for outboard side

    !  Calculation of max FW temp. Include NBI orbit loss power (assume to be only on outboard side)
    ! and a fraction of the escaped alpha power as components of the outboard wall surface power
    call iterate_fw(afwo, bfwo, fwareaob, (psurffwo + porbitlossmw + fwareaob/fwarea*palpfwmw), &
        bllengo, pnucfwo, tpeakfwo, cf, rhof, velfwo)

    !  Adjust first wall thickness if bfwo has been changed
    fwoth = 2.0D0*bfwo
    !vffwo = (afwo/bfwo)**2
    vffwo = (pi*afwo*afwo)/((2.0D0*bfwo)**2.0D0)

    !  Total mass flow rate to remove outboard first wall power (kg/s)
    mffwo = 1.0D6*(pnucfwo + psurffwo + porbitlossmw) / (cf*(outlet_temp-inlet_temp))

    !  Calculate total number of pipes from coolant fraction and channel dimensions
    !npfwo = (vffwo*fwoth*fwareaob)/(pi*afwo*afwo*fwfllengo)
    npfwo = (vffwo*fwoth*fwareaob)/(pi*afwo*afwo*bllengo)

    !  Mass flow rate per coolant pipe (kg/s)
    mffwpo = mffwo/npfwo

    !  Neutron power deposited in outboard blanket (MW)
    pnucblkto = pnucblkt * volblkto / volblkt

    !  Assume inlet and outlet temperatures for breeder zone are the same as those of
    !  for the first wall. Channel inner diameter is also the same. This does not
    !  have to be so but is the case for WCLL, and allows the pumping power to be calculated.
    mfblkto = 1.0D6*(pnucblkto) / (cf*(outlet_temp-inlet_temp))  !  kg/s

    !  Calculate total number of pipes (in all outboard modules) from coolant fraction and 
    !  channel dimensions (assumes up/down flow, two 90 deg bends per length)
    npblkto = (vfblkt*volblkto)/(pi*afwo*afwo*bzfllengo)

    !  mass flow rate per coolant pipe (kg/s)
    mfblktpo = mfblkto / npblkto

    !  Coolant velocity in breeder zone (m/s)
    velblkto = mfblktpo/(pi*afwo*afwo*rhof)

    !  Ideally we should check the maximum temperature in the breeder zone is below
    !  necessary limits, but this is not straightforward

    !  Calculate pumping powers for blanket and first wall
    htpmw_fwo = pumppower(fwfllengo, afwo, mffwo, mffwpo, no90fw, no180fw, velfwo, etaiso, coolwh)
    htpmw_blkto = pumppower(bzfllengo, afwo, mfblkto, mfblktpo, no90bz, no180bz, velblkto, etaiso, coolwh)

    !  Total inboard & outboard FW and blanket pumping powers (MW)
    htpmw_fw = htpmw_fwi + htpmw_fwo
    htpmw_blkt = htpmw_blkti + htpmw_blkto
    
	!  Peak temperature (deg C)
    tpeak = max(tpeakfwi, tpeakfwo) - 273.15D0
    
    if (ip == 0) return
    
    call oheadr(ofile, 'Thermohydraulics') 
    call ovarin(ofile, 'Coolant type (1=He, 2=H20)', '(coolwh)',coolwh)
    call ovarre(ofile, 'Outboard coolant flow rate (m/s)', '(velfwo)',velfwo)
    call ovarre(ofile, 'Radial thickness available for pipes inboard (80%)', '(bldepti)', bldepti)
    call ovarre(ofile, 'Radial thickness available for pipes outboard (80%)', '(bldepti)', bldepto)
    call ovarre(ofile, 'mid-plane toroidal circumference and segment', '(blwidti)', blwidti)
    call ovarre(ofile, 'mid-plane toroidal circumference and segment', '(blwidti)', blwidto)
    call ovarre(ofile, 'FW Inboard flow length', '(fwfllengi)', fwfllengi)
    call ovarre(ofile, 'FW Outboard flow length', '(fwfllengo)', fwfllengo)
    call ovarre(ofile, 'Blanket Inboard flow length', '(bzfllengi)', bzfllengi)
    call ovarre(ofile, 'Blanket Outboard flow length', '(bzfllengo)', bzfllengo)
    call ovarre(ofile, 'coolant specific heat capacity at constant pressure (J/kg/K)', '(cf)',cf)
    call ovarre(ofile, 'coolant density (kg/m3)', '(rhof)',rhof)
    call ovarre(ofile, 'Inboard coolant flow rate (m/s)', '(velfwi)',velfwi)
    call ovarre(ofile, 'Outboard coolant flow rate (m/s)', '(velfwo)',velfwo)
    call ovarre(ofile, 'Inboard coolant flow rate (m/s)', '(fwareaib)',fwareaib)
    call ovarre(ofile, 'Outboard coolant flow rate (m/s)', '(fwareaob)',fwareaob)
    call ovarre(ofile, 'Surface heating FW inner', '(psurffwi)',psurffwi)
    call ovarre(ofile, 'Surface heating FW outer', '(psurffwo)',psurffwo)
    call ovarre(ofile, 'Nuclear heating FW inner', '(pnucfwi)',pnucfwi)
    call ovarre(ofile, 'Nuclear heating FW outer', '(pnucfwo)',pnucfwo)
    call oblnkl(ofile)
    call ovarre(ofile, 'void fraction inboard side', '(vffwi)', vffwi)
    call ovarre(ofile, 'Total mass flow rate to remove inboard first wall power (kg/s)', '(mffwi)', mffwi)
    call ovarre(ofile, 'Calculate total number of pipes from coolant fraction and channel dimensions', '(npfwi)', npfwi)
    call ovarre(ofile, 'Mass flow rate per coolant pipe (kg/s)', '(mffwpi)', mffwpi)
    call ovarre(ofile, 'Neutron power deposited in inboard blanket (MW)', '(pnucblkti)', pnucblkti)
    call ovarre(ofile, 'mass flow rate for inboard blanket coolant (kg/s)', '(mfblkti)', mfblkti)
    call ovarre(ofile, 'total num of pipes (in all inboard modules)', '(npblkti)', npblkti)
    call ovarre(ofile, 'Mass flow rate per coolant pipe (kg/s)', '(mfblktpi)', mfblktpi)
    call ovarre(ofile, 'Coolant velocity in blanket (m/s)', '(velblkti)', velblkti)
    call ovarre(ofile, 'pumping powers for first wall (MW)', '(htpmw_fwi)', htpmw_fwi)
    call ovarre(ofile, 'pumping powers for blanket (MW)', '(htpmw_blkti)', htpmw_blkti)
    call oblnkl(ofile)
    call ovarre(ofile, 'void fraction outboard side', '(vffwo)', vffwo)
    call ovarre(ofile, 'Total mass flow rate to remove outboard first wall power (kg/s)', '(mffwo)', mffwo)
    call ovarre(ofile, 'Calculate total number of pipes from coolant fraction and channel dimensions', '(npfwo)', npfwo)
    call ovarre(ofile, 'Mass flow rate per coolant pipe (kg/s)', '(mffwpo)', mffwpo)
    call ovarre(ofile, 'Neutron power deposited in outboard blanket (MW)', '(pnucblkto)', pnucblkto)
    call ovarre(ofile, 'mass flow rate for outboard blanket coolant (kg/s)', '(mfblkto)', mfblkto)
    call ovarre(ofile, 'total num of pipes (in all outboard modules)', '(npblkto)', npblkto)
    call ovarre(ofile, 'Mass flow rate per coolant pipe (kg/s)', '(mfblktpo)', mfblktpo)
    call ovarre(ofile, 'Coolant velocity in blanket (m/s)', '(velblkto)', velblkto)
    call ovarre(ofile, 'pumping powers for first wall (MW)', '(htpmw_fwo)', htpmw_fwo)
    call ovarre(ofile, 'pumping powers for blanket (MW)', '(htpmw_blkto)', htpmw_blkto)
  
  end subroutine
   
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine component_masses
    !+ad_name  component_masses
    !+ad_summ  Calculations for component masses
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculations for component masses
    !+ad_prob  None
    !+ad_hist  23/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Local variables
    real(kind=double) :: coolvol
    
    !  Start adding components of the coolant mass:
    !  Divertor coolant volume (m3)
    coolvol = divsur * divclfr * divplt

	!  Blanket coolant volume (m3)
    coolvol = coolvol + volblkt*vfblkt
    
    !  Shield coolant volume (m3)
    coolvol = coolvol + volshld*vfshld

	!  First wall coolant volume (m3)
	coolvol = coolvol + fwareaib*fwith*vffwi + fwareaob*fwoth*vffwo

    !  Mass of He coolant = volume * density at typical coolant temperatures and pressures (kg)
    coolmass = coolvol*1.517D0
    
    !  Average first wall coolant fraction, only used by old routines in fispact.f90, safety.f90
    fwclfr = (fwareaib*fwith*vffwi + fwareaob*fwoth*vffwo) / (fwarea*0.5D0*(fwith+fwoth))

	!  Component masses
	!  Divertor mass (kg)
    divsur = fdiva * 2.0D0 * pi * rmajor * rminor
    if (idivrt == 2) divsur = divsur * 2.0D0
    divmas = divsur * divdens * (1.0D0 - divclfr) * divplt

    !  Blanket mass, excluding coolant (kg)
    !  Blanket Titanium beryllide mass (kg)
    whtbltibe12 = volblkt * fbltibe12 * 2260.0D0
    
    !  Blanket Lithium orthosilicate mass (kg)
    !  Ref: www.rockwoodlithium.com...
    whtblli2sio4 = volblkt * fblli2sio4 * 2400.0D0
    
    !  Blanket Steel mass (kg)
    whtblss = volblkt * fblss * denstl
    
    !  Total blanket mass (kg)
    whtblkt = whtbltibe12 + whtblli2sio4 + whtblss  

    !  Shield mass (kg)
    whtshld = volshld * denstl * (1.0D0 - vfshld)

    !  Penetration shield mass (set = internal shield) (kg)
    wpenshld = whtshld

	!  First wall volume (m^3)
	volfw = (fwareaib*fwith*(1.0D0-vffwi) + fwareaob*fwoth*(1.0D0-vffwo))

	!  First wall mass (kg)
    fwmass = denstl * volfw
    
    !  First wall armour volume (m^3)
    fw_armour_vol = sarea*fw_armour_thickness
    
    !  First wall armour mass (kg)
    fw_armour_mass = fw_armour_vol*W_density
  
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blanket_mod_pol_height
    !+ad_name  blanket_mod_pol_height
    !+ad_summ  Calculations for blanket module poloidal height
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculations for blanket module poloidal height for D shaped and elliptical machines
    !+ad_prob  None
    !+ad_hist  23/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Local variables
    real(kind=double) :: a, b, ptor, r1
    
    if ((itart == 1).or.(fwbsshape == 1)) then  !  D-shaped machine

        !  Segment vertical inboard surface (m)
        bllengi = (2.0D0*hblnkt) / nblktmodpi
                
        !  Calculate perimeter of ellipse that defines the internal
        !  surface of the outboard first wall / blanket

        !  Mid-plane distance from inboard to outboard side (m)
        a = scrapli + 2.0D0*rminor + scraplo

        !  Internal half-height of blanket (m)
        b = hblnkt

        !  Calculate ellipse circumference using Ramanujan approximation (m)
        ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

        !  Calculate blanket poloidal length and segment, subtracting divertor length (m)
        bllengo = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpo

    !  shape defined by two half-ellipses
    else

        !  Major radius where half-ellipses 'meet' (m)
        r1 = rmajor - rminor*triang

        !  Internal half-height of blanket (m)
        b = hblnkt

        !  Distance between r1 and nearest edge of inboard first wall / blanket (m)
        a = r1 - (rmajor - rminor - scrapli)

        !  Calculate ellipse circumference using Ramanujan approximation (m)
        ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

        !  Calculate inboard blanket poloidal length and segment, subtracting divertor length (m) 
        !  Assume divertor lies between the two ellipses, so fraction fdiv still applies
        bllengi = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpi

        !  Distance between r1 and inner edge of outboard first wall / blanket (m)
        a = rmajor + rminor + scraplo - r1

        ! Calculate ellipse circumference using Ramanujan approximation (m)
        ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

        !  Calculate outboard blanket poloidal length and segment, subtracting divertor length (m)
        bllengo = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpo

    end if
  
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
   subroutine iterate_fw(afw, bfw, area, prad_incident, blleng, pnuc_deposited, tpeakfw, &
       cf, rhof, velfw)
    !+ad_name  iterate_fw
    !+ad_summ  Routine to perform detailed thermal hydraulic calculations
    !+ad_summ  for the first wall
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  afw : input real : first wall pipe inner radius (m)
    !+ad_args  bfw : input/output real : first wall pipe outer radius (m)
    !+ad_args  area : input real : area of first wall section under consideration (m2)
    !+ad_argc                      (i.e. area of inboard wall or outboard wall)
    !+ad_args  prad_incident : input real : incident radiation power (MW)
    !+ad_args  blleng : input real : poloidal length of pipe per segment (m)
    !+ad_args  pnuc_deposited : output real : neutron power deposited in FW side (IB or OB) (MW)
    !+ad_args  tpeakfw : output real : peak first wall temperature (K)
    !+ad_args  cf : output real : coolant specific heat capacity at constant
    !+ad_argc                     pressure (J/kg/K)
    !+ad_args  rhof : output real : coolant density (kg/m3)
    !+ad_args  velfw : output real : coolant flow rate (m/s)
    !+ad_desc  Detailed thermal hydraulic model for the blanket (first wall +
    !+ad_desc  breeding zone).
    !+ad_desc  <P>Given the heating incident on the first wall, and the coolant
    !+ad_desc  outlet temperature, the maximum temperature of the first wall is
    !+ad_desc  calculated to check it is below material limits (given by
    !+ad_desc  tfwmatmax). The first wall is assumed to consist of a set of
    !+ad_desc  parallel pipes of outer diameter bfw.  If the FW temperature is
    !+ad_desc  too high, bfw is decreased. A lower limit is applied such that
    !+ad_desc  the thickness of the pipe wall is sufficient to withstand the
    !+ad_desc  internal coolant pressure.
    !+ad_desc  <P>This routine was extracted from pulse.f90, with some
    !+ad_desc  modifications.  The routine is called separately for the inboard
    !+ad_desc  and outboard sides.
    !+ad_prob  None
    !+ad_call  cprops
    !+ad_call  smt
    !+ad_hist  21/08/14 PJK Initial version
    !+ad_hist  05/11/14 PJK Corrected position of fwlifs evaluation
    !+ad_stat  Okay
    !+ad_docs  The calculation of the maximum temperature is described by Gardner:
    !+ad_docc  "Temperature distribution in the first wall", K:\Power Plant Physics and
    !+ad_docc  Technology\ PROCESS\PROCESS References & Systems Codes\Pulsed option -
    !+ad_docc  Gardner.
    !+ad_docs  This is in turn taken from "Methods of First Wall Structural
    !+ad_docc  Analysis with Application to the Long Pulse Commercial Tokamak Reactor
    !+ad_docc  Design", R.J. LeClaire, MIT, PFC/RR-84-9
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: afw, area, prad_incident, blleng
    real(kind=double), intent(inout) :: bfw
    real(kind=double), intent(out) :: pnuc_deposited, tpeakfw, cf, rhof, velfw

    !  Local variables
    integer, parameter :: nk = 51
    integer :: it, k
    real(kind=double) :: boa, fboa, flnce, fwlifs, fwvol, hcoeff, kf, masflx, maxstress, &
        mindif, qpp, qppp, sgpthn, tav, temp_c, temp_k, tfwav, tmpdif, tmprop_c, tmprop_k, &
        tmprse, tmthet, tpeakfw_c, viscf, viscfs

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	!  Ratio of outer / inner radius of each structural FW tube
    boa = bfw/afw
    
    !  Temperature difference (outlet - inlet) (K)
    tmprse = outlet_temp - inlet_temp

    it = 0
    iteration: do ; it = it+1

	   !  Iterate for no more than 100 loops
       if (it > 100) then
          call report_error(88)
          exit iteration
       end if

       !  Check to see if inner radius is greater than outer radius
       if (afw >= bfw) then
          fdiags(1) = afw ; fdiags(2) = bfw
          call report_error(89)
       end if

	   !  Assume FW volume is equal to its surface area x external diameter of the hollow 
       !  cylindrical tubes that make up the first wall
       !  FW volume of FW side (inboard or outboard depending on arguments) (m3)
       fwvol = area*(2.0D0*bfw)

       !  Neutron power deposited in FW side (inboard or outboard depending on arguments) (MW)
       if (area == fwareaib) then
	     pnuc_deposited = pnucfw * fwareaib/fwarea
	   end if
	   
	   if (area == fwareaob) then
	     pnuc_deposited = pnucfw * fwareaob/fwarea
	   end if
       !fwvol / (fwvol + ((fwarea - area) * (bfwi + bfwo - bfw )))       

       !  Heat generation in the first wall due to neutron flux deposited in the material (W/m3)
       qppp = 1.0D6 * pnuc_deposited / fwvol
       
       !  Heat flux incident on the first wall surface from electromagnetic radiation flux (W/m2)      
       qpp = 1.0D6 * prad_incident / area

       !  Coolant properties at average coolant temperature (tb)
       !  tb is not known at this point, but since we only need the cf output (which does not
       !  depend on tb) we simply use outlet_temp as the input for tb
       call cprops(outlet_temp, cf, rhof, viscf, viscfs, kf)

       !  Coolant flow rate (kg/m2/s)
       !masflx = blleng*(qppp*(bfw**2) + 2.0D0*qpp*bfw) / afw**2 / cf / tmprse
       masflx = blleng*(qppp*((2.0D0*bfw)**2.0D0) + 2.0D0*qpp*bfw) / (pi*afw**2.0D0 )/ cf / tmprse
       velfw = masflx/rhof  !  m/s

       !  Average first wall temperature
       !  There is a problem here because the expression for the average
       !  temperature in the first wall contains a term which involves the
       !  thermal conductivity which is in itself temperature dependent. How
       !  do we resolve this problem?  Firstly we define a temperature range
       !  where the lower bound is defined to be the temperature on the inner
       !  wall (in contact with the coolant) and therefore equal to the bulk
       !  coolant temperature, and the upper bound is taken to be 800 degrees
       !  Celsius.  Next we iterate over this range so that at each step the
       !  average temperature can be calculated together with the difference
       !  between the average temperature and the iterated temperature. The
       !  average temperature at which this difference is minimised is taken
       !  as the correct average temperature in the first wall.

       !  Calculate the first wall average temperature using the conductivity
       !  taken at temperatures from outlet_temp to 1073 K.  The iteration loop 
       !  stores the calculated value of the average wall temperature when 
       !  it is closest to the temperature used for the conductivity.
       mindif = 1.0D30

       do k = 1, nk
		
		  !  Temperature in Kelvin and deg C
          temp_k = outlet_temp + (1073.15D0-outlet_temp) * dble(k-1)/(nk-1)
          temp_c = temp_k - 273.15D0

          !  hcoeff also depends on FW temp so the calc of coolant properties & hcoeff is repeated
          call cprops(temp_k, cf, rhof, viscf, viscfs, kf)

          !  Heat transfer coefficient calculated using Sieder-Tate correlation, 
          !  valid for Re > 1.0e3, 0.7 < Pr < 16700, L/D > 10
          hcoeff = 0.027D0*(kf/2.0D0/afw)*(masflx*2.0D0*afw &
               /viscf)**0.8D0 * (viscf*cf/kf)**0.33D0 &
               *(viscf/viscfs)**0.14D0

          tav = bfw/tk(temp_c)*(qpp/pi + qppp*bfw/2.0D0)*(bfw**2/ &
               (bfw**2-afw**2)*log(bfw/afw)-0.5D0) &
               - qppp/4.0D0/tk(temp_c)*((bfw**2-afw**2)/2.0D0) &
               + (pi*(bfw**2-afw**2)*qppp + 2.0D0*bfw*qpp)/ &
               (2.0D0*pi*afw*hcoeff) + outlet_temp - 273.15D0
          tmpdif = abs(tav-temp_c)

          if (tmpdif <= mindif) then
             mindif = tmpdif
             tfwav = tav
             tmprop_k = temp_k
          end if

       end do

       tmprop_c = tmprop_k - 273.15D0

       !  Recalculate hcoeff with the found average first wall temperature
       call cprops(tmprop_k, cf, rhof, viscf, viscfs, kf)

       !  Heat transfer coefficient calculated using Sieder-Tate
       !  correlation, valid for Re > 1.0e3, 0.7 < Pr < 16700, L/D > 10
       hcoeff = 0.027D0*(kf/2.0D0/afw)*(masflx*2.0D0*afw &
            /viscf)**0.8D0 * (viscf*cf/kf)**0.33D0 &
            *(viscf/viscfs)**0.14D0

       !  Limits on the first wall thickness
       !  The upper limit on the first wall thickness is derived from the
       !  maximum temperature of the FW material.  There is also a fluence
       !  limit; if this is exceeded the FW lifetime is reduced. The fluence
       !  is the product of the neutron wall loading (qppp*fwvol/fwarea) and
       !  the wall lifetime. This fluence limit is a conservative one, with
       !  the upper bound on the fluence set by the value abktflnc (MW-yr/m2)

       !  Fluence (MW-yr/m^2)
       flnce = 1.0D-6*qppp * fwvol/area * fwlife

       !  Calculate peak temperature - occurs at (r,theta) = (bfw,0)
       call cosine_term(afw, bfw, 0.0D0, bfw, qpp, hcoeff, tmprop_c, tmthet)

       tpeakfw = bfw/tk(tmprop_c) * (qpp/pi + qppp*bfw/2.0D0) &
            * log(bfw/afw) - qppp/4.0D0/tk(tmprop_c)*(bfw**2-afw**2) &
            + (pi*(bfw**2-afw**2)*qppp + 2.0D0*bfw*qpp) / &
            (2.0D0*pi*afw*hcoeff) + outlet_temp + tmthet  !  in K
       tpeakfw_c = tpeakfw - 273.15D0

       if ((tpeakfw > tfwmatmax).or.(flnce > abktflnc)) then

          !  Temperature or fluence limit exceeded; reduce first wall lifetime
          fwlife = abktflnc * area/ fwvol / (1.0D-6*qppp)

          !  fboa is chosen such that fboa**100 * (bfw/afw) = 1.001,
          !  i.e. after 100 iterations bfw is still just larger than afw
          !  N.B. bfw may also have been modified via the stress test below...
          fboa = (1.001D0/boa)**0.01D0

          bfw = bfw*fboa
          if ((bfw/afw) <= 1.001D0) then
             write(*,*) 'Warning in routine ITERATE_FW: Swelling limit exceeded, and optimisation'// &
             ' is failing to find a suitable first wall thickness...PROCESS continuing.'
             exit iteration
          else
             cycle iteration
          end if

       end if

       fwlifs = 3.1536D7*fwlife

       !  The lower limit on the first wall thickness is derived from the
       !  constraint that the first wall must possess the ability to withstand
       !  the internal coolant pressure. The limit is written as,
       !  (bfw-afw)	 > p*(afw+bfw)/2/maxstress
       sgpthn = (coolp*(afw+bfw)/2.0D0) / (bfw-afw)

       maxstress = 1.0D9 !smt(tpeakfw_c, fwlifs)

       if (sgpthn <= maxstress) then
          exit iteration
       else
          !  First wall too thin
          !  Keep afw fixed and alter bfw so that the lower limit
          !  is satisfied.
          bfw = ( afw*(maxstress + coolp/2.0D0)) / (maxstress - coolp/2.0D0)
          write(*,*) 'FW too thin...',sgpthn, maxstress, tpeakfw_c
          write(*,*) 'FW too thin...',afw, bfw
          write(*,*) 'FW too thin...',coolp
       end if

    end do iteration

  end subroutine iterate_fw

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine cosine_term(afw, bfw, angle, rad, qpp, hcoeff, tmprop, tmthet)
    !+ad_name  cosine_term
    !+ad_summ  Calculates cosine terms in temperature distribution
    !+ad_type  Subroutine
    !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  afw    : input real : first wall pipe inner radius (m)
    !+ad_args  bfw    : input real : first wall pipe outer radius (m)
    !+ad_args  angle  : input real : azimuthal angle (radians)
    !+ad_args  rad    : input real : radial position within first wall tube (m)
    !+ad_args  qpp    : input real : surface heat flux incident on first wall (W/m**2)
    !+ad_args  hcoeff : input real : heat transfer coefficient (W/m**2/K)
    !+ad_args  tmprop : input real : property temperature (C)
    !+ad_args  tmthet : output real : azimuthal temperature term (C)
    !+ad_desc  This routine calculates the cosine terms in the temperature
    !+ad_desc  distribution formula. These terms are calculated with the material
    !+ad_desc  properties measured at the property temperature.
    !+ad_prob  None
    !+ad_call  tk
    !+ad_hist  25/11/93 PJK Incorporation into PROCESS
    !+ad_hist  01/10/12 PJK Initial F90 version
    !+ad_hist  04/09/14 PJK Modified for use with new thermal hydraulic model
    !+ad_stat  Okay
    !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: afw,bfw,angle,rad,qpp,hcoeff,tmprop
    real(kind=double), intent(out) :: tmthet

    !  Local variables
    integer :: i,k
    real(kind=double) :: cc,dd

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Lowest order terms
    cc = qpp*afw**2*bfw**2/2.0D0/tk(tmprop) * &
         (tk(tmprop) - hcoeff*afw) / &
         ( tk(tmprop)*(bfw**2 - afw**2) + hcoeff*afw*(afw**2 + bfw**2) )
    dd = qpp*bfw**2/2.0D0/tk(tmprop) * &
         (tk(tmprop) + hcoeff*afw) / &
         ( tk(tmprop)*(bfw**2 - afw**2) + hcoeff*afw*(afw**2 + bfw**2) )

    tmthet = (cc/rad + dd*rad)*cos(angle)

    !  Higher order even terms
    do i = 2,8,2
       k = i/2
       cc = qpp/pi/tk(tmprop)/dble(k) * ( dble((-1)**(k+1)) / &
            ((2.0D0*dble(k))**2 - 1.0D0) ) &
            * ( bfw**(2*k+1)*(2.0D0*dble(k)*tk(tmprop) - hcoeff*afw) ) &
            / ( 2.0D0*dble(k)*tk(tmprop)*((bfw/afw)**(4*k)-1.0D0) &
            + hcoeff*afw*((bfw/afw)**(4*k)+1.0D0) )
       dd = 1.0D0 / ((afw*10.0D0)**(4*k)) * (10.0D0)**(4*k) &
            * (2.0D0*dble(k)*tk(tmprop) + hcoeff*afw) / &
            (2.0D0*dble(k)*tk(tmprop) - hcoeff*afw)*cc

       tmthet = tmthet + (cc/rad**i + dd*rad**i)*cos(dble(i)*angle)
    end do

  end subroutine cosine_term

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine cprops(tb, cf, rhof, viscf, viscfs, kf)
    !+ad_name  cprops
    !+ad_summ  Calculates various temperature dependent coolant properties
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  tb     : input real : bulk temperature (K)
    !+ad_args  cf     : output real : coolant specific heat capacity at
    !+ad_argc                         constant pressure (J/kg/K)
    !+ad_args  rhof   : output real : coolant mass density (kg/m3)
    !+ad_args  viscf  : output real : coolant dynamic viscosity at the
    !+ad_argc                         bulk temperature (Pa-s = kg/m/s)
    !+ad_args  viscfs : output real : coolant dynamic viscosity at the
    !+ad_argc                         average wall temperature (Pa-s = kg/m/s)
    !+ad_args  kf     : output real : coolant thermal conductivity at
    !+ad_argc                         average coolant temperature (W/m/K)
    !+ad_desc  This routine calculates various temperature dependent properties
    !+ad_desc  of the liquid coolant in the first wall / blanket.
    !+ad_desc  <P>The properties required to calculate the heat transfer
    !+ad_desc  coefficient are found. This is done at the average coolant
    !+ad_desc  temperature and the average temperature at the material surface.
    !+ad_desc  The latter is found by averaging the difference between the material
    !+ad desc  temperature and the inlet temperature and the difference between the
    !+ad_desc  material temperature and the outlet temperature.
    !+ad_desc  <P>The fluid properties are obtained from REFPROP, or using formulae
    !+ad_desc  from Panos Karditsas's original subroutine 'props'.
    !+ad_prob  None
    !+ad_call  fluid_properties
    !+ad_hist  04/09/14 PJK Initial version
    !+ad_hist  17/12/14 PJK Added calls to REFPROP interface
    !+ad_stat  Okay
    !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
    !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
    !+ad_docc  Dept., Culham Laboratory, Abingdon
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: tb
    real(kind=double), intent(out) :: cf, rhof, viscf, viscfs, kf

    !  Local variables
    real(kind=double) :: x, y, gascf, kfs, cfs, pran, prans, tbc

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  x: average coolant temperature (K)
    !  y: average coolant temperature at the material surface (K)
    x = 0.5D0*(outlet_temp + inlet_temp)
    
    !  y = tb?!
    y = x + 0.5D0*( (tb - outlet_temp) + (tb - inlet_temp) )

    call fluid_properties(x, coolp, coolwh, density=rhof, thermal_conductivity=kf, &
      viscosity=viscf, specific_heat_const_p=cf)

    call fluid_properties(y, coolp, coolwh, viscosity=viscfs)

  end subroutine cprops

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   
  subroutine write_ccfe_hcpb_output
	!+ad_name  write_ccfe_hcpb_output
    !+ad_summ  Write output to file for CCFE HCPB model
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  This subroutine outputs the CCFE HCPB model results to 
    !+ad_desc  an output file
    !+ad_prob  None
    !+ad_hist  10/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    call oheadr(ofile, 'Blanket model output - CCFE HCPB model')   
    call osubhd(ofile, 'Blanket Composition :')
 
    call ovarrf(ofile, 'Titanium beryllide fraction', '(fbltibe12)', fbltibe12)
    call ovarrf(ofile, 'Lithium orthosilicate fraction', '(fblli2sio4)', fblli2sio4)
    call ovarrf(ofile, 'Steel fraction', '(fblss)', fblss)
    call ovarrf(ofile, 'Void fraction coolant', '(vfcblkt)', vfcblkt)
    call ovarrf(ofile, 'Void fraction purge gas', '(vfpblkt)', vfpblkt)
    
    call osubhd(ofile, 'Component Volumes :')
    
    call ovarrf(ofile, 'First Wall Armour Volume (m3)', '(fw_armour_vol)', fw_armour_vol)
    call ovarrf(ofile, 'First Wall Volume (m3)', '(volfw)', volfw)
    call ovarrf(ofile, 'Blanket Volume (m3)', '(volblkt)', volblkt)
    call ovarrf(ofile, 'Shield Volume (m3)', '(volshld)', volshld)
    call ovarre(ofile, 'Vacuum vessel volume (m3)', '(vdewin)', vdewin)
        
    call osubhd(ofile, 'Component Masses :')
    
    call ovarre(ofile, 'First Wall Armour Mass (kg)', '(fw_armour_mass)', fw_armour_mass)
    call ovarre(ofile, 'First Wall Mass (kg)', '(fwmass)', fwmass)
    call ovarre(ofile, 'Blanket Mass - Total(kg)', '(whtblkt)', whtblkt)
    call ovarre(ofile, 'Blanket Mass - TiBe12 (kg)', '(whtbltibe12)', whtbltibe12)
    call ovarre(ofile, 'Blanket Mass - Li2SiO4 (kg)', '(whtblli2sio4)', whtblli2sio4)
    call ovarre(ofile, 'Blanket Mass - Steel (kg)', '(whtblss)', whtblss)
    call ovarre(ofile, 'Shield Mass (kg)', '(whtshld)', whtshld)
    call ovarre(ofile, 'Vacuum vessel mass (kg)', '(cryomass)', cryomass)
    
    !  Nuclear heting section
    call osubhd(ofile, 'Nuclear heating :')
    
    call ovarre(ofile, 'Average neutron wall load (MW/m2)','(wallmw)', wallmw)
    call ovarre(ofile, 'First wall full-power lifetime (years)', '(fwlife)', fwlife)
    call oblnkl(ofile)
    
    !  ST centre post
    if (itart == 1) then
       call osubhd(ofile,'(Copper centrepost used)')
       call ovarre(ofile,'ST centrepost heating (MW)','(pnuccp)',pnuccp)
    end if
    
    call ovarre(ofile, 'Total nuclear heating in TF+PF coils (CS is negligible) (MW)', '(ptfnuc)', ptfnuc) 
    call ovarre(ofile, 'Total nuclear heating in FW (MW)', '(pnucfw)', pnucfw) 
	call ovarre(ofile, 'Total nuclear heating in the blanket (including emult) (MW)', '(pnucblkt)', pnucblkt) 
	call ovarre(ofile, 'Total nuclear heating in the shield (MW)', '(pnucshld)', pnucshld)
	call ovarre(ofile, 'Total nuclear heating in the divertor (MW)', '(pnucdiv)', pnucdiv)
    call osubhd(ofile,'Diagostic output for nuclear heating :')    
    call ovarre(ofile, 'Blanket exponential factor', '(exp_blanket)', exp_blanket)    
    call ovarre(ofile, 'Shield: first exponential', '(exp_shield1)', exp_shield1)
    call ovarre(ofile, 'Shield: second exponential', '(exp_shield2)', exp_shield2)   
    
    call osubhd(ofile,'Thermodynamic Model Output :')
    
    call ovarin(ofile, 'Switch for plant secondary cycle ', '(secondary_cycle)', secondary_cycle) 
    call ovarre(ofile, 'First wall coolant pressure (Pa)', '(coolp)', coolp)
    call ovarre(ofile, 'Inner radius of inboard first wall coolant channels (m)', '(afwi)', afwi)
    call ovarre(ofile, 'Outer radius of inboard first wall coolant channels (m)', '(bfwi)', bfwi)
    call ovarre(ofile, 'Inner radius of outboard first wall coolant channels (m)', '(afwo)', afwo)
    call ovarre(ofile, 'Outer radius of outboard first wall coolant channels (m)', '(bfwo)', bfwo)
    call ovarrf(ofile, 'Inlet temperature of coolant (K)', '(inlet_temp)', inlet_temp)
    call ovarrf(ofile, 'Outlet temperature of coolant (K)', '(outlet_temp)', outlet_temp)
    call ovarre(ofile, 'Maximum temperature of first wall material (K)', '(tfwmatmax)', tfwmatmax)
    call ovarin(ofile, 'No of inboard blanket modules poloidally', '(nblktmodpi)', nblktmodpi)
    call ovarin(ofile, 'No of inboard blanket modules toroidally', '(nblktmodti)', nblktmodti)
    call ovarin(ofile, 'No of outboard blanket modules poloidally', '(nblktmodpo)', nblktmodpo)
    call ovarin(ofile,'No of outboard blanket modules toroidally', '(nblktmodto)', nblktmodto)
    call ovarre(ofile, 'Isentropic efficiency of first wall / blanket coolant pumps', '(etaiso)', etaiso)
    
    call osubhd(ofile, 'Other volumes, masses and areas :')
    call ovarre(ofile, 'First wall area (m2)', '(fwarea)', fwarea)
    call ovarre(ofile, 'External cryostat radius (m)', '(rdewex)', rdewex)
    call ovarre(ofile, 'External cryostat half-height (m)', '(zdewex)', zdewex)
    call ovarre(ofile, 'External cryostat volume (m3)', '(vdewex)', vdewex)
    call ovarre(ofile, 'Total cryostat + vacuum vessel mass (kg)', '(dewmkg)', dewmkg)
    call ovarre(ofile, 'Divertor area (m2)', '(divsur)', divsur)
    call ovarre(ofile, 'Divertor mass (kg)', '(divmas)', divmas)
    
  end subroutine

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
  function st_centrepost_nuclear_heating(pneut, cphalflen, cpradius, rmajor) &
       result(pnuccp)

    !+ad_name  st_centrepost_nuclear_heating
    !+ad_summ  Estimates the nuclear power absorbed by the ST centrepost
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  pneut : input real : total neutron power (MW)
    !+ad_args  cphalflen : input real : half-length of centrepost (m)
    !+ad_args  cpradius : input real : centrepost radius (m)
    !+ad_args  rmajor : input real : plasma major radius (m)
    !+ad_desc  This routine calculates the neutron power absorbed by a
    !+ad_desc  copper spherical tokamak centrepost.
    !+ad_desc  The calculation estimates the fraction of neutrons hitting
    !+ad_desc  the centrepost from a point source at the plasma centre,
    !+ad_desc  and assumes an average path length of 2*cpradius, and an
    !+ad_desc  e-folding decay length of 0.08m (copper-water mixture).
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  05/11/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  J D Galambos, STAR Code : Spherical Tokamak Analysis and Reactor Code,
    !+ad_docc  unpublished internal Oak Ridge document
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: pneut,cphalflen,cpradius,rmajor
    real(kind=double) :: pnuccp

    !  Local variables
    real(kind=double) :: frachit

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    frachit = cphalflen / sqrt(cphalflen**2 + (rmajor-cpradius)**2 ) * &
         atan(cpradius/(rmajor-cpradius) )/pi

    pnuccp = pneut * frachit * (1.0D0 - exp(-2.0D0*cpradius/0.08D0))

  end function st_centrepost_nuclear_heating

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
  function tk(t)
    !+ad_name  tk
    !+ad_summ  Calculates the thermal conductivity of the first wall
    !+ad_type  Function returning real
    !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  t : input real : property temperature (C)
    !+ad_desc  This routine calculates the thermal conductivity of the
    !+ad_desc  first wall (W/m/K). This gives a reasonable fit to 316 stainless
    !+ad_desc  steel for temperatures between 300 and 800 degrees Celsius.
    !+ad_desc  An additional option for using Eurofer steel as the first wall
    !+ad_desc  material has been added for future use (currently commented out).
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  25/11/93 PJK Incorporation into PROCESS
    !+ad_hist  01/10/12 PJK Initial F90 version
    !+ad_hist  04/09/14 PJK Added Eurofer steel fit
    !+ad_stat  Okay
    !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind=double) :: tk, temp

    !  Arguments
    real(kind=double), intent(in) :: t

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	! Convert to Kelvin
    temp = t + 273.15

    ! Eurofer correlation, from "Fusion Demo Interim Structural Design Criteria - 
    ! Appendix A Material Design Limit Data", F. Tavassoli, TW4-TTMS-005-D01, 2004
    tk = 5.4308D0 + 0.13565D0*temp - 0.00023862D0*temp*temp + 1.3393D-7*temp*temp*temp

  end function tk

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function smt(t, fwlifs)
    !+ad_name  smt
    !+ad_summ  Calculates the maximum stress intensity for the first wall
    !+ad_type  Function returning real
    !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  t      : input real : property temperature (C)
    !+ad_args  fwlifs : input real : first wall lifetime (s)
    !+ad_desc  This routine calculates the maximum stress intensity (Pa)
    !+ad_desc  in the first wall, from fits via the ASME code.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  25/11/93 PJK Incorporation into PROCESS
    !+ad_hist  01/10/12 PJK Initial F90 version
    !+ad_hist  04/09/14 PJK Copied from pulse.f90
    !+ad_stat  Okay
    !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind=double) :: smt

    !  Arguments
    real(kind=double), intent(in) :: t, fwlifs

    !  Local variables
    real(kind=double) :: smt400, smt500, smt600, lnpwr

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    smt400 = 109.0D0  !  smt at <= 400 degrees Celsius
    smt500 = 107.0D0  !  smt at 500 degrees Celsius
    smt600 = 102.0D0  !  smt at 600 degrees Celsius

    if (fwlifs >= 3000.0D0) then
       lnpwr = log(fwlifs)
       smt600 = -0.8139765D0*lnpwr**2 + 6.2849D0*lnpwr + 99.147D0
    end if

    !  Linear interpolation used within range 400 deg C to 600 deg C
    if (t <= 400.0D0) then
       smt = smt400
    else if (t <= 500.0D0) then
       smt = smt400 + (t-400.0D0)/100.0D0*(smt500-smt400)
    else if (t <= 600.0D0) then
       smt = smt500 + (t-500.0D0)/100.0D0*(smt600-smt500)
    else
       fdiags(1) = smt ; call report_error(91)
    end if

    !  Stress intensity in Pascals
    smt = smt*1.0D6

  end function smt

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function pumppower(flleng, rad, mf, mfp, no90, no180, vel, etaiso, coolwh)

    !+ad_name  pumppower
    !+ad_summ  Routine to calculate the coolant pumping power in MW in the first
    !+ad_summ  wall and breeding zone
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  flleng      : input real : total flow length along pipe (m)
    !+ad_args  rad         : input real : pipe inner radius (m)
    !+ad_args  mf          : input real : total coolant mass flow rate in (kg/s)
    !+ad_args  mfp         : input real : coolant mass flow rate per pipe (kg/s)
    !+ad_args  no90        : input integer : number of 90 degree bends in pipe
    !+ad_args  no180       : input integer : number of 180 degree bends in pipe
    !+ad_args  vel         : input real : coolant flow speed (m/s)
    !+ad_args  etaiso      : input real : isentropic efficiency of coolant pumps
    !+ad_args  coolwh      : input integer: coolant fluid (1=helium, 2=water)
    !+ad_desc  This routine calculates the power required (MW) to pump the coolant in the
    !+ad_desc  first wall and breeding zone.
    !+ad_desc  <P>Pressure drops are calculated for a pipe with a number of 90
    !+ad_desc  and 180 degree bends.  The pressure drop due to frictional forces along
    !+ad_desc  the total straight length of the pipe is calculated, then the pressure
    !+ad_desc  drop due to the bends is calculated.  The total pressure drop is the sum
    !+ad_desc  of all contributions.  For a water coolant the pumping power is
    !+ad_desc  calculated as the volume flow rate multiplied by the total pressure
    !+ad_desc  drop, with a correction for non-isentropic pump behaviour applied.
    !+ad_desc  Since helium is a compressible fluid the pumping power is be calculated
    !+ad_desc  using enthalpies before and after the pump.
    !+ad_prob  None
    !+ad_call  cprops
    !+ad_call  enthalpy_ps
    !+ad_call  fluid_properties
    !+ad_call  report_error
    !+ad_call  smt
    !+ad_hist  04/09/14 PJK Initial version
    !+ad_hist  17/12/14 PJK Added calls to REFPROP interface
    !+ad_stat  Okay
    !+ad_docs  WCLL DDD, WP12-DAS02-T03, J. Aubert et al, EFDA_D_2JNFUP
    !+ad_docs  A Textbook on Heat Transfer, S.P. Sukhatme, 2005
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind=double) :: pumppower  !  MW

    !  Arguments
    real(kind=double), intent(in) :: flleng, rad, mf, mfp, vel, etaiso
    integer, intent(in) :: no90, no180, coolwh

    !  Local variables
    real(kind=double) :: cf, coolpin, deltap, dh, h1, h2, kelbwn, kelbwt, kf, kstrght, &
         lambda, ppump, reyn, rhof, s1, s2, viscf, viscfs, xifn, xift, ximn, ximt, vv

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Get fluid properties (only viscf and rhof are needed)
    call cprops(outlet_temp, cf, rhof, viscf, viscfs, kf)

    !  Hydraulic diameter (circular channels assumed) (m)
    dh = 2.0D0*rad

    !  Reynolds number
    reyn = 4.0D0*mfp / (pi*dh*viscf)

    !  Darcy friction factor, using Filonenko equation
    !  valid for 1.0e4 < Re < 1.0e7 (Sukhatme)
    if ((reyn < 1.0D4).or.(reyn > 1.0D7)) then
       fdiags(1) = reyn ; fdiags(2) = mfp
       fdiags(3) = dh ; fdiags(4) = viscf
       call report_error(167)
    end if
    lambda = 1.0D0 / (1.8D0*log10(reyn) - 1.64D0)**2

    !  Straight section pressure drop coefficient
    kstrght = lambda * flleng/dh

    !  90 degree elbow pressure drop coefficient
    !  Elbow radius assumed = 0.018m, from WCLL
    ximn = 0.21D0 / sqrt(0.018D0/dh)  !  singularity coefficient
    xifn = 0.0175D0*lambda*0.018D0*90.0D0/dh  !  friction coefficient
    kelbwn = ximn + xifn

    !  180 degree elbow pressure drop coefficient
    !  Elbow radius assumed half that of 90 deg case
    ximt = (0.7D0 + 0.35D0*180.0D0/90.0D0) * 0.21D0 / sqrt(0.009D0/dh)
    xift = 0.0175D0*lambda*0.018D0*90.0D0/dh  !+PJK... 90 or 180?
    kelbwt = ximt + xift
    
    !  TODO: remove vel from arguments and from where pumppower is called!

    !  Total pressure drop, dividing by 1.0e6 to get MPa
    vv = (mfp/(rhof*Pi*rad*rad))
    deltap = 1.0D-6 * (kstrght + no90*kelbwn + no180*kelbwt) * 0.5D0*rhof*vv*vv

    !  Pumping power
    
    !  Inlet pressure (Pa)
    coolpin = coolp + 1.0D6*deltap

    !  Obtain inlet enthalpy and entropy from inlet pressure and temperature
    call fluid_properties(inlet_temp, coolpin, coolwh, enthalpy=h2, entropy=s2)

    !  Assume isentropic pump so that s1 = s2
    s1 = s2

    !  Get enthalpy (J/kg) before pump using coolp and s1
    call enthalpy_ps(coolp, s1, coolwh, h1)

    !  Pumping power (MW)
    ppump = 1.0D-6 * mf * (h2-h1) / etaiso

    pumppower = ppump
    
    if (ip  == 0) return
    
    call oheadr(ofile, 'Pumppower') 
    call ovarre(ofile, 'Viscosity', '(viscf)', viscf)
    call ovarre(ofile, 'Density', '(rhof)', rhof)
    call ovarre(ofile, 'Reynolds number', '(reyn)', reyn)
    call ovarre(ofile, 'lambda', '(lambda)', lambda)
    call ovarre(ofile, 'Straight section pressure drop coefficient', '(kstrght)', kstrght)
    call ovarre(ofile, '90 degree elbow singularity coefficient', '(ximn)', ximn)
    call ovarre(ofile, '90 degree elbow friction coefficient', '(xifn)', xifn)
    call ovarre(ofile, '180 degree elbow singularity coefficient', '(ximt)', ximt)
    call ovarre(ofile, '180 degree elbow friction coefficient', '(xift)', xift)
    call ovarre(ofile, 'Pressure drop', '(deltap)', deltap)
    call ovarre(ofile, 'Inlet pressure (Pa)', '(coolpin)', coolpin)
    call ovarre(ofile, 'Inley enthalpy', '(h2)', h2)
    call ovarre(ofile, 'Inley entropy', '(s2)', s2)
    call ovarre(ofile, 'Enthalpy before pump', '(h1)', h1)
    call ovarre(ofile, 'Pumping power (MW)', '(ppump)', ppump)

  end function pumppower

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module
    
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
module kit_hcpb_module
  !+ad_name  kit_hcpb_module
  !+ad_summ  Module containing the KIT HCPB blanket model based on the HCPB concept design
  !+ad_type  Module
  !+ad_auth  J Morris, CCFE, Culham Science Centre
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  F Franza, KIT (original MATLAB implementation)
  !+ad_cont  blanket_lifetime
  !+ad_cont  f_alpha
  !+ad_cont  fast_neutron_fluence
  !+ad_cont  he_production_vacuum_vessel
  !+ad_cont  kit_blanket
  !+ad_cont  nuclear_power_production
  !+ad_cont  power_density
  !+ad_cont  radial_coordinates
  !+ad_cont  tritium_breeding_ratio
  !+ad_args  N/A
  !+ad_desc  This module contains the blanket neutronics model developed
  !+ad_desc  by Fabrizio Franza et al. from Karlsruhe Institute of Technology (KIT)
  !+ad_desc  based on the EUROfusion Helium-Cooled Pebble Bed (HCPB) blanket concept.
  !+ad_prob  None
  !+ad_hist  12/02/15 JM  Initial version of refactor
  !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
  !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
  !+ad_docc  Karlsruhe Institute of Technology, January 2013;
  !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
  !+ad_stat  Okay
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Terminology
  !  Radial coordinate arrays for the blanket sub-assemblies:
  !  BZ = Breeding Zone
  !  BM = Box Manifold
  !  BP = Back Plates
  !  VV = Vacuum Vessel (includes low-temperature shield)
  !  Element 1 = 'inner' edge, element np(=2) = 'outer' edge
  !  IB = inboard, OB = outboard

  !  Modules to import
  use build_variables
  use cost_variables
  use error_handling
  use fwbs_variables
  use physics_variables
  use process_output
  use tfcoil_variables
  use times_variables

  implicit none

  !  Subroutine declarations
  private
  public :: kit_hcpb
  
  !  Precision variable
  integer, parameter :: double = 8
 
  !  Variables for output to file
  integer, private :: ip, ofile
  
  !  Array length
  integer, parameter :: np = 2

  real(kind=double), dimension(np) :: x_BZ_IB, x_BM_IB, x_BP_IB, x_VV_IB
  real(kind=double), dimension(np) :: x_BZ_OB, x_BM_OB, x_BP_OB, x_VV_OB

  !  Values shared between subroutines in this module
  real(kind=double) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
  real(kind=double) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
  real(kind=double) :: phi_n_vv_IB_start,phi_n_vv_OB_start

  !  Universal constants
  real(kind=double), parameter :: E_n = 14.1D0    ! [MeV] Average neutron energy
  real(kind=double), parameter :: PA_T = 3.0D0    ! [g/mol] Tritium atomic weight
  real(kind=double), parameter :: N_Av = 6.02D23  ! [at/mol] Avogadro number

  !  Constants and fixed coefficients used in the model
  !  Based on Helium-Cooled Pebble Beds (HCPB) configuration
  !  of the PPCS Model B design
  real(kind=double) :: A_cov_PPCS = 1365.0D0   ! [m^2] Total blanket coverage area
  real(kind=double) :: A_FW_PPCS = 1253.0D0    ! [m^2] First wall area
  real(kind=double) :: A_FW_IB_PPCS = 348.2D0  ! [m^2] IB first wall area
  real(kind=double) :: A_FW_OB_PPCS = 905.6D0  ! [m^2] OB first wall area
  real(kind=double) :: NWL_av_PPCS = 1.94D0    ! [MW/m^2] Average neutron wall load
  real(kind=double) :: NWL_av_IB_PPCS = 1.73D0 ! [MW/m^2] Average IB wall load
  real(kind=double) :: NWL_av_OB_PPCS = 1.92D0 ! [MW/m^2] Average OB wall load
  real(kind=double) :: NWL_max_IB_PPCS = 1.99D0 ! [MW/m^2] Maximum IB wall load
  real(kind=double) :: NWL_max_OB_PPCS = 2.41D0 ! [MW/m^2] Maximum OB wall load
  real(kind=double) :: f_peak_PPCS = 1.21      ! [--] Neutron wall load peaking factor
  real(kind=double) :: CF_bl_PPCS              ! [%] Blanket coverage factor (calculated)
  real(kind=double) :: e_Li_PPCS = 30.0D0      ! [%] Li6 enrichment
  character(len=13) :: breeder_PPCS = 'Orthosilicate' ! Breeder type

  real(kind=double) :: t_BZ_IB_PPCS = 36.5D0   ! [cm] IB Breeding Zone thickness
  real(kind=double) :: t_BZ_OB_PPCS = 46.5D0   ! [cm] OB Breeding Zone thickness
  real(kind=double) :: TBR_PPCS = 1.12D0       ! [--] Tritium Breeding Ratio
  real(kind=double) :: M_E_PPCS = 1.38D0       ! [--] Energy multiplication factor
  
  !  Power density pre-exponential terms and decay lengths
  real(kind=double) :: q_0_BZ_breed_IB = 31.348D0 ! [W/cm^3] Pre-exp term in IB BZ breeder
  real(kind=double) :: q_0_BZ_breed_OB = 37.144D0 ! [W/cm^3] Pre-exp term in OB BZ breeder
  real(kind=double) :: lambda_q_BZ_breed_IB = 29.42D0 ! [cm] Decay length in IB BZ breeder
  real(kind=double) :: lambda_q_BZ_breed_OB = 27.03D0 ! [cm] Decay length in OB BZ breeder

  real(kind=double) :: q_0_BZ_Be_IB = 9.532D0 ! [W/cm^3] Pre-exp term in IB BZ Beryllium
  real(kind=double) :: q_0_BZ_Be_OB = 11.809D0 ! [W/cm^3] Pre-exp term in OB BZ Beryllium
  real(kind=double) :: lambda_q_BZ_Be_IB = 16.39D0 ! [cm] Decay length in IB BZ Beryllium
  real(kind=double) :: lambda_q_BZ_Be_OB = 16.39D0 ! [cm] Decay length in OB BZ Beryllium

  real(kind=double) :: q_0_BZ_steels_IB = 16.067D0 ! [W/cm^3] Pre-exp term in IB BZ steels
  real(kind=double) :: q_0_BZ_steels_OB = 18.788D0 ! [W/cm^3] Pre-exp term in OB BZ steels
  real(kind=double) :: lambda_q_BZ_steels_IB = 21.27D0 ! [cm] Decay length in IB BZ steels
  real(kind=double) :: lambda_q_BZ_steels_OB = 21.27D0 ! [cm] Decay length in OB BZ steels

  real(kind=double) :: lambda_EU = 11.57D0  ! [cm] Decay length in EUROFER
  real(kind=double) :: lambda_q_BM_IB       ! [cm] Decay length in IB BM (calculated)
  real(kind=double) :: lambda_q_BM_OB       ! [cm] Decay length in OB BM (calculated)
  real(kind=double) :: lambda_q_BP_IB       ! [cm] Decay length in IB BP (calculated)
  real(kind=double) :: lambda_q_BP_OB       ! [cm] Decay length in OB BP (calculated)
  real(kind=double) :: lambda_q_VV = 6.92D0 ! [cm] Decay length in Vacuum Vessel

  !  Fast neutron flux pre-exponential terms and decay lengths
  real(kind=double) :: phi_0_n_BZ_IB = 5.12D14  ! [n/cm^2/sec] Pre-exp term in IB BZ
  real(kind=double) :: phi_0_n_BZ_OB = 5.655D14 ! [n/cm^2/sec] Pre-exp term in OB BZ
  real(kind=double) :: lambda_n_BZ_IB = 18.79D0 ! [cm] Decay length in IB BZ
  real(kind=double) :: lambda_n_BZ_OB = 19.19D0 ! [cm] Decay length in OB BZ
  real(kind=double) :: lambda_n_VV = 8.153D0    ! [cm] Decay length in VV

  !  [n/cm^2/sec] Reference fast neutron flux on VV inner side [Fish09]
  real(kind=double) :: phi_n_0_VV_ref = 2.0D10  

  !  Vacuum vessel helium production pre-exponential terms and decay lengths
  real(kind=double) :: Gamma_He_0_ref = 1.8D-3  ! [appm/yr] Pre-exp term
  real(kind=double) :: lambda_He_VV = 7.6002D0  ! [cm] Decay length

  !  [dpa] Allowable neutron damage to the FW EUROFER
  real(kind=double) :: D_EU_max = 60.0D0  

  !  Variables used in this module, ultimately to be set via the calling routine
  !  to values given by PROCESS variables
  real(kind=double), public :: P_n = 2720.0D0    ! [MW] Fusion neutron power
  real(kind=double), public :: NWL_av = 1.94D0   ! [MW/m^2] Average neutron wall load
  real(kind=double), public :: f_peak = 1.21D0   ! [--] NWL peaking factor
  real(kind=double), public :: t_FW_IB = 2.3D0   ! [cm] IB first wall thickness
  real(kind=double), public :: t_FW_OB = 2.3D0   ! [cm] OB first wall thickness
  real(kind=double), public :: A_FW_IB = 3.5196D6 ! [cm^2] IB first wall area
  real(kind=double), public :: A_FW_OB = 9.0504D6 ! [cm^2] OB first wall area
  real(kind=double), public :: A_bl_IB = 3.4844D6 ! [cm^2] IB blanket area
  real(kind=double), public :: A_bl_OB = 8.9599D6 ! [cm^2] OB blanket area
  real(kind=double), public :: A_VV_IB = 3.8220D6 ! [cm^2] IB shield/VV area
  real(kind=double), public :: A_VV_OB = 9.8280D6 ! [cm^2] OB shield/VV area
  real(kind=double), public :: CF_bl = 91.7949D0 ! [%] Blanket coverage factor
  integer, public :: n_ports_div = 2             ! [ports] Number of divertor ports
  integer, public :: n_ports_H_CD_IB = 2         ! [ports] Number of IB H&CD ports
  integer, public :: n_ports_H_CD_OB = 2         ! [ports] Number of OB H&CD ports
  character(len=5), public :: H_CD_ports = 'small' ! Type of H&CD ports (small or large)
  real(kind=double), public :: e_Li = 30.0D0     ! [%] Lithium 6 enrichment
  real(kind=double), public :: t_plant = 40.0D0  ! [FPY] Plant lifetime
  real(kind=double), public :: alpha_m = 0.75D0  ! [--] Availability factor
  real(kind=double), public :: alpha_puls = 1.0D0 ! [--] Pulsed regime fraction

  !  Breeder type (allowed values are Orthosilicate, Metatitanate or Zirconate)
  character(len=20), public :: breeder = 'Orthosilicate'

  !  Inboard parameters
  real(kind=double), public :: t_BZ_IB = 36.5D0     ! [cm] BZ thickness
  real(kind=double), public :: t_BM_IB = 17.0D0     ! [cm] BM thickness
  real(kind=double), public :: t_BP_IB = 30.0D0     ! [cm] BP thickness
  real(kind=double), public :: t_VV_IB = 35.0D0     ! [cm] VV thickness
  real(kind=double), public :: alpha_BM_IB = 40.0D0  ! [%] Helium fraction in the IB BM
  real(kind=double), public :: alpha_BP_IB = 65.95D0 ! [%] Helium fraction in the IB BP
  real(kind=double), public :: chi_Be_BZ_IB = 69.2D0 ! [%] Beryllium vol. frac. in IB BZ
  real(kind=double), public :: chi_breed_BZ_IB = 15.4D0 ! [%] Breeder vol. frac. in IB BZ
  real(kind=double), public :: chi_steels_BZ_IB = 9.8D0 ! [%] Steels vol. frac. in IB BZ

  !  Outboard parameters
  real(kind=double), public :: t_BZ_OB = 46.5D0     ! [cm] BZ thickness
  real(kind=double), public :: t_BM_OB = 27.0D0     ! [cm] BM thickness
  real(kind=double), public :: t_BP_OB = 35.0D0     ! [cm] BP thickness
  real(kind=double), public :: t_VV_OB = 65.0D0     ! [cm] VV thickness
  real(kind=double), public :: alpha_BM_OB = 40.0D0  ! [%] Helium fraction in the OB BM
  real(kind=double), public :: alpha_BP_OB = 67.13D0 ! [%] Helium fraction in the OB BP
  real(kind=double), public :: chi_Be_BZ_OB = 69.2D0 ! [%] Beryllium vol. frac. in OB BZ
  real(kind=double), public :: chi_breed_BZ_OB = 15.4D0 ! [%] Breeder vol. frac. in OB BZ
  real(kind=double), public :: chi_steels_BZ_OB = 9.8D0 ! [%] Steels vol. frac. in OB BZ

  !  Model outputs
  real(kind=double), public :: pnuctfi  ! [MW/m3] Nuclear heating on IB TF coil
  real(kind=double), public :: pnuctfo  ! [MW/m3] Nuclear heating on OB TF coil
  real(kind=double), public :: P_th_tot ! [MW] Nuclear power generated in blanket
  real(kind=double), public :: pnucsh   ! [MW] Nuclear power generated in shield/VV
  real(kind=double), public :: M_E      ! [--] Energy multiplication factor
  real(kind=double), public :: tbratio  ! [--] Tritium breeding ratio
  real(kind=double), public :: G_tot    ! [g/day] Tritium production rate
  real(kind=double), public :: nflutfi  ! [n/cm2] Fast neutron fluence on IB TF coil
  real(kind=double), public :: nflutfo  ! [n/cm2] Fast neutron fluence on OB TF coil
  real(kind=double), public :: vvhemini ! [appm] minimum final He. conc in IB VV
  real(kind=double), public :: vvhemino ! [appm] minimum final He. conc in OB VV
  real(kind=double), public :: vvhemaxi ! [appm] maximum final He. conc in IB VV
  real(kind=double), public :: vvhemaxo ! [appm] maximum final He. conc in OB VV
  real(kind=double), public :: t_bl_fpy ! [y] blanket lifetime in full power years
  real(kind=double), public :: t_bl_y   ! [y] blanket lifetime in calendar years

  !  Inboard/outboard void fraction of blanket
  real(kind=double), private :: vfblkti, vfblkto

contains

  ! TODO : global check
  ! TODO : Output section for model!

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function f_alpha(alpha)

    !+ad_name  f_alpha
    !+ad_summ  Calculates the power density decay length multiplier
    !+ad_summ  in a blanket region given the helium fraction
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  alpha : input real : helium fraction (%)
    !+ad_desc  This routine calculates the power density decay length
    !+ad_desc  multiplier in a blanket region comprising EUROFER steel and
    !+ad_desc  helium coolant, given the helium volume fraction within the
    !+ad_desc  region.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: alpha

    !  Local variables  
    real(kind=double) :: f_alpha

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    f_alpha = 1.0D0 + 0.019D0*alpha

  end function f_alpha
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine kit_hcpb(outfile, iprint)

    !+ad_name  kit_hcpb
    !+ad_summ  Main routine for the KIT HCPB blanket model
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calls the main work routines for the KIT HCPB
    !+ad_desc  blanket model.
    !+ad_prob  None
    !+ad_call  radial_coordinates
    !+ad_call  power_density
    !+ad_call  nuclear_power_production
    !+ad_call  tritium_breeding_ratio
    !+ad_call  fast_neutron_fluence
    !+ad_call  he_production_vacuum_vessel
    !+ad_call  blanket_lifetime
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Arguments

    integer, intent(in) :: iprint, outfile

    !  Assign module private variables to iprint and outfile
    ip = iprint
    ofile = outfile
    
    !  Calculate FW/Blanket lifetime
	fwlife = min(abktflnc/wallmw, tlife)
	
	!  Coolant type
	coolwh = 1

	!  Convert global variables into KIT blanket inputs
    A_FW_IB = fwareaib * 1.0D4  ! [cm^2] IB first wall area
    A_FW_OB = fwareaob * 1.0D4  ! [cm^2] OB first wall area
    A_bl_IB = blareaib * 1.0D4  ! [cm^2] IB blanket area
    A_bl_OB = blareaob * 1.0D4  ! [cm^2] OB blanket area
    A_VV_IB = shareaib * 1.0D4  ! [cm^2] IB shield/VV area
    A_VV_OB = shareaob * 1.0D4  ! [cm^2] OB shield/VV area
    P_n = pneutmw               ! [MW] Fusion neutron power
    NWL_av = wallmw             ! [MW/m^2] Average neutron wall load
    f_peak = wallpf             ! [--] NWL peaking factor
    t_FW_IB = fwith * 100.0D0   ! [cm] IB first wall thickness
    t_FW_OB = fwoth * 100.0D0   ! [cm] OB first wall thickness
    !  f_FW = 0.99D0            ! [--] Frac. FW area for junctions, etc.
    CF_bl = (1.0D0-fhole-fhcd-fdiv) * 100.0D0 ! [%] Blanket coverage factor
    n_ports_div = npdiv         ! [ports] Number of divertor ports
    n_ports_H_CD_IB = nphcdin   ! [ports] Number of IB H&CD ports
    n_ports_H_CD_OB = nphcdout  ! [ports] Number of OB H&CD ports

    if (hcdportsize == 1) then
       H_CD_ports = 'small'
    else
       H_CD_ports = 'large'
    end if

    e_Li = li6enrich            ! [%] Lithium 6 enrichment
    t_plant = tlife/cfactr      ! [FPY] Plant lifetime
    alpha_m = cfactr            ! [--] Availability factor
    alpha_puls = tpulse/(tramp+tpulse+tdwell) ! [--] Pulsed regime fraction

    !  Breeder type (allowed values are Orthosilicate, Metatitanate or Zirconate)
    !
    !  Mass densities supplied by F. Franza, taken from Seventh International
    !  Workshop on Ceramic Breeder Blanket Interactions, September 14-16, 1998,
    !  Petten, Netherlands:
    !                              Li4Si04      Li2TiO3      Li2ZrO3
    !  Theory density [g/cm^3]      2.40         3.45         4.19
    !  Material density             98 %         94 %         89 %
    !  Packing factor               64 %         55 %         57 %
    !  Pebble bed density [g/cm^3]  1.50         1.78         2.12
    if (breedmat == 1) then
       breeder = 'Orthosilicate'
       densbreed = 1.50D3
    else if (breedmat == 2) then
       breeder = 'Metatitanate'
       densbreed = 1.78D3
    else
       breeder = 'Zirconate'  !  (In reality, rarely used - activation problems)
       densbreed = 2.12D3
    end if

    !  Inboard parameters
    t_BZ_IB = blbuith * 100.0D0          ! [cm] BZ thickness
    t_BM_IB = blbmith * 100.0D0          ! [cm] BM thickness
    t_BP_IB = blbpith * 100.0D0          ! [cm] BP thickness
    t_VV_IB = (shldith+ddwi) * 100.0D0   ! [cm] VV thickness
    alpha_BM_IB = fblhebmi * 100.0D0     ! [%] Helium fraction in the IB BM
    alpha_BP_IB = fblhebpi * 100.0D0     ! [%] Helium fraction in the IB BP
    chi_Be_BZ_IB = fblbe * 100.0D0       ! [%] Beryllium vol. frac. in IB BZ
    chi_breed_BZ_IB = fblbreed * 100.0D0 ! [%] Breeder vol. frac. in IB BZ
    chi_steels_BZ_IB = fblss * 100.0D0   ! [%] Steels vol. frac. in IB BZ

    !  Outboard parameters
    t_BZ_OB = blbuoth * 100.0D0          ! [cm] BZ thickness
    t_BM_OB = blbmoth * 100.0D0          ! [cm] BM thickness
    t_BP_OB = blbpoth * 100.0D0          ! [cm] BP thickness
    t_VV_OB = (shldoth+ddwi) * 100.0D0   ! [cm] VV thickness
    alpha_BM_OB = fblhebmo * 100.0D0     ! [%] Helium fraction in the OB BM
    alpha_BP_OB = fblhebpo * 100.0D0     ! [%] Helium fraction in the OB BP
    chi_Be_BZ_OB = fblbe * 100.0D0       ! [%] Beryllium vol. frac. in OB BZ
    chi_breed_BZ_OB = fblbreed * 100.0D0 ! [%] Breeder vol. frac. in OB BZ
    chi_steels_BZ_OB = fblss * 100.0D0   ! [%] Steels vol. frac. in OB BZ

    !  Perform preliminary calculations for the PPCS Model B configuration
    !  Blanket coverage factor (%)
    CF_bl_PPCS = A_FW_PPCS/A_cov_PPCS * 100.0D0

    !  Power density decay lengths (cm) in the BM and BP regions,
    !  given the helium fractions
    lambda_q_BM_IB = lambda_EU * f_alpha(alpha_BM_IB)
    lambda_q_BM_OB = lambda_EU * f_alpha(alpha_BM_OB)
    lambda_q_BP_IB = lambda_EU * f_alpha(alpha_BP_IB)
    lambda_q_BP_OB = lambda_EU * f_alpha(alpha_BP_OB)

    !  Initialise the radial coordinate arrays, defining the blanket
    !  sub-assembly thicknesses
    call radial_coordinates

    !  Perform the main calculations
    call power_density(q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end, &
         q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end,pnuctfi,pnuctfo)

    call nuclear_power_production(q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end, &
         q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end,P_th_tot,M_E,pnucsh)

    call tritium_breeding_ratio(tbratio,G_tot)

    call fast_neutron_fluence(phi_n_vv_IB_start,phi_n_vv_OB_start, &
         nflutfi,nflutfo)

    call he_production_vacuum_vessel(phi_n_vv_IB_start,phi_n_vv_OB_start, &
         vvhemini,vvhemino,vvhemaxi,vvhemaxo)

    call blanket_lifetime(t_bl_FPY,t_bl_Y)
    
    call component_masses
    
    !  Transfer output values from model to global variables
    pnucblkt = P_th_tot
    pnucshld = pnucsh
    emult = M_E
    tbr = tbratio
    tritprate = G_tot
    bktlife = t_bl_fpy  !  This is later adjusted for availability in routine AVAIL

    !  Peak fast neutron fluence on TF coils (neutrons/m2)
    nflutf = max(nflutfi,nflutfo) * 1.0D4

    !  Peak nuclear heating in TF coil (MW/m3)
    ptfnucpm3 = max(pnuctfi,pnuctfo)

    !  Total nuclear heating in TF coil (MW)
    !  Rough estimate of TF coil volume used, assuming 25% of the total
    !  TF coil perimeter is inboard, 75% outboard
    ptfnuc = 0.25D0*tfleng*tfareain * pnuctfi &
         + 0.75D0*tfleng*arealeg*tfno * pnuctfo

    !  Maximum helium concentration in vacuum vessel at
    !  end of plant lifetime (appm)
    vvhemax = max(vvhemaxi,vvhemaxo)
    
    if (ip == 0) return
    
    call write_kit_hcpb_output

  end subroutine kit_hcpb

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine radial_coordinates

    !+ad_name  radial_coordinates
    !+ad_summ  Sets up the radial build within the KIT blanket
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine sets up the arrays containing the radial
    !+ad_desc  build within each blanket sub-assembly.
    !+ad_desc  <P>At present, the arrays contain only NP=2 elements, i.e. contain the
    !+ad_desc  values at the inner and outer radial locations; however, if required,
    !+ad_desc  they may be changed easily to provide several points for plotting
    !+ad_desc  purposes, for example.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Radial coordinates in each inboard sub-assembly (cm)
    !  Element 1 is 'inner' edge (nearer the plasma), element np (=2) is 'outer' edge
    x_BZ_IB(1) = 0.0D0 ; x_BZ_IB(np) = t_FW_IB + t_BZ_IB
    x_BM_IB(1) = 0.0D0 ; x_BM_IB(np) = t_BM_IB
    x_BP_IB(1) = 0.0D0 ; x_BP_IB(np) = t_BP_IB
    x_VV_IB(1) = 0.0D0 ; x_VV_IB(np) = t_VV_IB

    !  Radial coordinates in each outboard sub-assembly (cm)
    x_BZ_OB(1) = 0.0D0 ; x_BZ_OB(np) = t_FW_OB + t_BZ_OB
    x_BM_OB(1) = 0.0D0 ; x_BM_OB(np) = t_BM_OB
    x_BP_OB(1) = 0.0D0 ; x_BP_OB(np) = t_BP_OB
    x_VV_OB(1) = 0.0D0 ; x_VV_OB(np) = t_VV_OB

  end subroutine radial_coordinates

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine power_density(q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end, &
    q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end,pnuctfi,pnuctfo)

    !+ad_name  power_density
    !+ad_summ  Calculates the nuclear power density profiles
    !+ad_dumm  within the KIT blanket sub-assemblies
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  q_BZ_IB_end : output real : power density at outer edge of IB BZ (MW/m3)
    !+ad_args  q_BM_IB_end : output real : power density at outer edge of IB BM (MW/m3)
    !+ad_args  q_BP_IB_end : output real : power density at outer edge of IB BP (MW/m3)
    !+ad_args  q_BZ_OB_end : output real : power density at outer edge of OB BZ (MW/m3)
    !+ad_args  q_BM_OB_end : output real : power density at outer edge of OB BM (MW/m3)
    !+ad_args  q_BP_OB_end : output real : power density at outer edge of OB BP (MW/m3)
    !+ad_args  pnuctfi     : output real : power density at outer edge of IB VV (MW/m3)
    !+ad_argc                              = that on inner TF coil winding pack
    !+ad_args  pnuctfo     : output real : power density at outer edge of OB VV (MW/m3)
    !+ad_argc                              = that on outer TF coil winding pack
    !+ad_desc  This routine calculates the nuclear power density profiles within each
    !+ad_desc  blanket sub-assembly, assuming an exponential decay with distance through
    !+ad_desc  each region, with the decay indices dependent on the material fractions.
    !+ad_desc  <P>At present, the arrays contain only NP=2 elements, i.e. contain the
    !+ad_desc  values at the inner and outer radial locations; however, if required,
    !+ad_desc  they may be changed easily to provide several points for plotting
    !+ad_desc  purposes, for example.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(out) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
    real(kind=double), intent(out) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
    real(kind=double), intent(out) :: pnuctfi, pnuctfo

    !  Local variables
    real(kind=double), dimension(np) :: q_steels_BZ_IB, q_steels_BZ_OB
    real(kind=double), dimension(np) :: q_BM_IB, q_BP_IB, q_VV_IB
    real(kind=double), dimension(np) :: q_BM_OB, q_BP_OB, q_VV_OB

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  N.B. Power density is in W/cm3 = MW/m3
    !  Inboard profiles
    !  Power density profile in IB BZ steels
    q_steels_BZ_IB(:) = NWL_av/NWL_av_PPCS * q_0_BZ_steels_IB * &
         exp(-x_BZ_IB(:)/lambda_q_BZ_steels_IB)
    q_BZ_IB_end = q_steels_BZ_IB(np)

    ! Power density profile in IB BM
    q_BM_IB(:) = q_steels_BZ_IB(np) * exp(-x_BM_IB(:)/lambda_q_BM_IB)
    q_BM_IB_end = q_BM_IB(np)

    !  Power density profile in IB BP
    q_BP_IB(:) = q_BM_IB(np) * exp(-x_BP_IB(:)/lambda_q_BP_IB)
    q_BP_IB_end = q_BP_IB(np)

    !  Power density profile in IB VV
    q_VV_IB(:) = q_BP_IB(np) * exp(-x_VV_IB(:)/lambda_q_VV)

    !  Outboard profiles
    !  Power density profile in OB BZ steels
    q_steels_BZ_OB(:) = NWL_av/NWL_av_PPCS * q_0_BZ_steels_OB * &
         exp(-x_BZ_OB(:)/lambda_q_BZ_steels_OB)
    q_BZ_OB_end = q_steels_BZ_OB(np)

    !  Power density profile in OB BM
    q_BM_OB(:) = q_steels_BZ_OB(np) * exp(-x_BM_OB(:)/lambda_q_BM_OB)
    q_BM_OB_end = q_BM_OB(np)

    !  Power density profile in OB BP
    q_BP_OB(:) = q_BM_OB(np) * exp(-x_BP_OB(:)/lambda_q_BP_OB)
    q_BP_OB_end = q_BP_OB(np)

    !  Power density profile in OB VV
    q_VV_OB(:) = q_BP_OB(np) * exp(-x_VV_OB(:)/lambda_q_VV)

    !  Nuclear heating on TF coil winding pack is assumed to be equal to
    !  the value at the outer edge of the VV (neglecting the steel TF coil case
    pnuctfi = q_VV_IB(np)
    pnuctfo = q_VV_OB(np)

  end subroutine power_density

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_power_production(q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end, &
       q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end,P_th_tot,M_E,pnucsh)

    !+ad_name  nuclear_power_production
    !+ad_summ  Calculates nuclear power production and energy multiplication factor
    !+ad_dumm  within the KIT blanket sub-assemblies
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  q_BZ_IB_end : input real : power density at outer edge of IB BZ (MW/m3)
    !+ad_args  q_BM_IB_end : input real : power density at outer edge of IB BM (MW/m3)
    !+ad_args  q_BP_IB_end : input real : power density at outer edge of IB BP (MW/m3)
    !+ad_args  q_BZ_OB_end : input real : power density at outer edge of OB BZ (MW/m3)
    !+ad_args  q_BM_OB_end : input real : power density at outer edge of OB BM (MW/m3)
    !+ad_args  q_BP_OB_end : input real : power density at outer edge of OB BP (MW/m3)
    !+ad_args  p_th_tot    : output real : total nuclear power in the blanket (MW)
    !+ad_args  m_e         : output real : energy multiplication factor in the blanket
    !+ad_args  pnucsh      : output real : total nuclear power in the shield (MW)
    !+ad_desc  This routine calculates the nuclear power production within each
    !+ad_desc  blanket sub-assembly, assuming an exponential decay with distance through
    !+ad_desc  each region, with the decay indices dependent on the material fractions.
    !+ad_desc  These are summed to give the total nuclear power produced in the 'blanket'
    !+ad_desc  (BZ+BM+BP) and 'shield' regions, and the energy multiplication factor
    !+ad_desc  in the blanket is calculated.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_hist  26/09/13 PJK/FF Refined model to take into account average/peak PPCS
    !+ad_hisc               wall load scaling in inboard and outboard regions
    !+ad_stat  Okay
    !+ad_docs  WP13-SYS01-A-T02 Interim Review Meeting, 10.07.2013, F. Franza
    !+ad_docc  (describes 26/09/2013 model refinement)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
    real(kind=double), intent(in) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
    real(kind=double), intent(out) :: P_th_tot, M_E, pnucsh

    !  Local variables
    real(kind=double) :: A_BZ_breed_IB, A_BZ_breed_OB, A_BZ_Be_IB, A_BZ_Be_OB
    real(kind=double) :: A_BZ_steels_IB, A_BZ_steels_OB
    real(kind=double) :: P_BZ_breed_IB, P_BZ_Be_IB, P_BZ_steels_IB
    real(kind=double) :: P_BZ_IB, P_BM_IB, P_BP_IB, P_VV_IB
    real(kind=double) :: P_BZ_breed_OB, P_BZ_Be_OB, P_BZ_steels_OB
    real(kind=double) :: P_BZ_OB, P_BM_OB, P_BP_OB, P_VV_OB
    real(kind=double) :: P_tot_IB, P_tot_OB, P_n_FW

    real(kind=double) :: nwl_ratio, nwl_IB_ratio_PPCS, nwl_OB_ratio_PPCS

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nwl_ratio = NWL_av/NWL_av_PPCS
    nwl_IB_ratio_PPCS = NWL_av_IB_PPCS / NWL_max_IB_PPCS
    nwl_OB_ratio_PPCS = NWL_av_OB_PPCS / NWL_max_OB_PPCS

    !  Cross-sectional areas in the breeder zone (cm2)
    !  Breeder (chi... = volumetric fraction as a percentage)
    A_BZ_breed_IB = A_bl_IB * 0.01D0*chi_breed_BZ_IB
    A_BZ_breed_OB = A_bl_OB * 0.01D0*chi_breed_BZ_OB

    !  Beryllium pebbles
    A_BZ_Be_IB = A_bl_IB * 0.01D0*chi_Be_BZ_IB
    A_BZ_Be_OB = A_bl_OB * 0.01D0*chi_Be_BZ_OB

    !  Breeder Zone steels
    A_BZ_steels_IB = A_bl_IB * 0.01D0*chi_steels_BZ_IB
    A_BZ_steels_OB = A_bl_OB * 0.01D0*chi_steels_BZ_OB

    !  Inboard power terms (MW)
    !  Nuclear power in IB breeder pebbles
    P_BZ_breed_IB = 1.0D-6 * nwl_ratio * nwl_IB_ratio_PPCS * A_BZ_breed_IB * &
         lambda_q_BZ_breed_IB * q_0_BZ_breed_IB * &
         ( exp(-t_FW_IB/lambda_q_BZ_breed_IB) - &
         exp(-(t_FW_IB+t_BZ_IB)/lambda_q_BZ_breed_IB) )

    !  Nuclear power in IB Be pebbles     
    P_BZ_Be_IB = 1.0D-6 * nwl_ratio * nwl_IB_ratio_PPCS * A_BZ_Be_IB * &
         lambda_q_BZ_Be_IB * q_0_BZ_Be_IB * &
         ( exp(-t_FW_IB/lambda_q_BZ_Be_IB) - &
         exp(-(t_FW_IB+t_BZ_IB)/lambda_q_BZ_Be_IB) )

    !  Nuclear power in IB BZ steels
    P_BZ_steels_IB = 1.0D-6 * nwl_ratio * nwl_IB_ratio_PPCS * A_BZ_steels_IB * &
         lambda_q_BZ_steels_IB * q_0_BZ_steels_IB * &
         (1.0D0-exp(-(t_FW_IB+t_BZ_IB)/lambda_q_BZ_steels_IB))

    !  Total nuclear power in IB BZ
    P_BZ_IB = P_BZ_breed_IB + P_BZ_Be_IB + P_BZ_steels_IB

    !  Nuclear power in IB BM
    P_BM_IB = 1.0D-6 * nwl_IB_ratio_PPCS * A_bl_IB * &
         lambda_q_BM_IB * q_BZ_IB_end * &
         (1.0D0-exp(-t_BM_IB/lambda_q_BM_IB))

    !  Nuclear power in IB BP
    P_BP_IB = 1.0D-6 * nwl_IB_ratio_PPCS * A_bl_IB * &
         lambda_q_BP_IB * q_BM_IB_end * &
         (1.0D0-exp(-t_BP_IB/lambda_q_BP_IB))

    !  Nuclear power in IB VV
    P_VV_IB = 1.0D-6 * nwl_IB_ratio_PPCS * A_VV_IB * &
         lambda_q_VV * q_BP_IB_end * &
         (1.0D0-exp(-t_VV_IB/lambda_q_VV))

    !  Outboard power terms (MW)
    !  Nuclear power in OB BZ breeder pebbles
    P_BZ_breed_OB = 1.0D-6 * nwl_ratio * nwl_OB_ratio_PPCS * A_BZ_breed_OB * &
         lambda_q_BZ_breed_OB * q_0_BZ_breed_OB * &
         ( exp(-t_FW_OB/lambda_q_BZ_breed_OB) - &
         exp(-(t_FW_OB+t_BZ_OB)/lambda_q_BZ_breed_OB) )

    !  Nuclear power in OB BZ Be pebbles
    P_BZ_Be_OB = 1.0D-6 * nwl_ratio * nwl_OB_ratio_PPCS * A_BZ_Be_OB * &
         lambda_q_BZ_Be_OB * q_0_BZ_Be_OB * &
         ( exp(-t_FW_OB/lambda_q_BZ_Be_OB) - &
         exp(-(t_FW_OB+t_BZ_OB)/lambda_q_BZ_Be_OB) )

    !  Nuclear power in OB BZ steels
    P_BZ_steels_OB = 1.0D-6 * nwl_ratio * nwl_OB_ratio_PPCS * A_BZ_steels_OB * &
         lambda_q_BZ_steels_OB * q_0_BZ_steels_OB * &
         (1.0D0-exp(-(t_FW_OB+t_BZ_OB)/lambda_q_BZ_steels_OB))

    !  Total nuclear power in OB BZ
    P_BZ_OB = P_BZ_breed_OB + P_BZ_Be_OB + P_BZ_steels_OB

    !  Nuclear power in OB BM
    P_BM_OB = 1.0D-6 * nwl_OB_ratio_PPCS * A_bl_OB * &
         lambda_q_BM_OB * q_BZ_OB_end * &
         (1.0D0-exp(-t_BM_OB/lambda_q_BM_OB))

    !  Nuclear power in OB BP
    P_BP_OB = 1.0D-6 * nwl_OB_ratio_PPCS * A_bl_OB * &
         lambda_q_BP_OB * q_BM_OB_end * &
         (1.0D0-exp(-t_BP_OB/lambda_q_BP_OB))

    !  Nuclear power in OB VV
    P_VV_OB = 1.0D-6 * nwl_OB_ratio_PPCS * A_VV_OB * &
         lambda_q_VV * q_BP_OB_end * &
         (1.0D0-exp(-t_VV_OB/lambda_q_VV))

    !  Total nuclear power in IB and OB regions (MW)
    !  Excludes contribution from shield/vacuum vessel
    P_tot_IB = P_BZ_IB + P_BM_IB + P_BP_IB
    P_tot_OB = P_BZ_OB + P_BM_OB + P_BP_OB

    !  Total nuclear power in the 'blanket' (MW)
    P_th_tot = P_tot_IB + P_tot_OB

    !  Total nuclear power in shield/VV (MW)
    pnucsh = P_VV_IB + P_VV_OB

    !  Fusion neutron power impinging first wall (MW)
    P_n_FW = P_n * 0.01D0*CF_bl

    !  Energy multiplication factor
    M_E = P_th_tot/P_n_FW

  end subroutine nuclear_power_production
  
   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tritium_breeding_ratio(tbr,g_tot)

    !+ad_name  nuclear_power_production
    !+ad_summ  Calculates the tritium breeding ratio for the KIT blanket
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  tbr_breed
    !+ad_cont  tbr_ports
    !+ad_args  tbr   : output real : tritium breeding ratio
    !+ad_args  g_tot : output real : tritium production rate (g/day)
    !+ad_desc  This routine calculates the tritium breeding ratio and the rate
    !+ad_desc  of production of tritium in the KIT blanket design, taking into
    !+ad_desc  account the breeding material and the number and size of ports
    !+ad_desc  in the blanket.
    !+ad_prob  None
    !+ad_call  tbr_breed
    !+ad_call  tbr_ports
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_hist  26/09/13 PJK/FF Refinement to take into account IB/OB contributions
    !+ad_stat  Okay
    !+ad_docs  WP13-SYS01-A-T02 Interim Review Meeting, 10.07.2013, F. Franza
    !+ad_docc  (describes 26/09/2013 model refinement)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind=double), intent(out) :: tbr, g_tot

    !  Local variables

    real(kind=double) :: wib, wob
    real(kind=double), parameter :: wib_PPCS = 0.28D0, wob_PPCS = 0.72D0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    wib = A_FW_IB / (A_FW_IB + A_FW_OB)
    wob = A_FW_OB / (A_FW_IB + A_FW_OB)

    tbr = TBR_PPCS * CF_bl/CF_bl_PPCS * &
         TBR_breed(e_Li, breeder)/TBR_breed(e_Li_PPCS, breeder) * &
         ( 1.0D0 - exp( -(wib*t_BZ_IB + wob*t_BZ_OB) / &
         (wib*lambda_q_BZ_breed_IB + wob*lambda_q_BZ_breed_OB)) ) / &
         ( 1.0D0 - exp( -(wib_PPCS*t_BZ_IB_PPCS + wob_PPCS*t_BZ_OB_PPCS) / &
         (wib_PPCS*lambda_q_BZ_breed_IB + wob_PPCS*lambda_q_BZ_breed_OB)) ) * &
         TBR_ports(n_ports_div, n_ports_H_CD_IB, n_ports_H_CD_OB, H_CD_ports)

    !  Total tritium production rate (grammes/day)
    g_tot = tbr * P_n/(E_n*1.602D-19)/N_Av * PA_T*3600*24

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function TBR_breed(e_Li, breeder)

      !+ad_name  tbr_breed
      !+ad_summ  Returns a fit to the tritium breeding ratio for different breeder
      !+ad_summ  materials
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  F Franza, KIT (original MATLAB implementation)
      !+ad_cont  None
      !+ad_args  e_li   : input real : Lithium-6 enrichment (%)
      !+ad_args  breeder : input character string : breeder material; either
      !+ad_argc          <UL><LI>'Orthosilicate' or
      !+ad_argc              <LI>'Metatitanate' or
      !+ad_argc              <LI>'Zirconate'</UL>
      !+ad_desc  This routine provides the dependence of the tritium breeding
      !+ad_desc  ratio on the ceramic breeder in use and the lithium-6 enrichment of
      !+ad_desc  the breeder.
      !+ad_prob  None
      !+ad_call  report_error
      !+ad_hist  06/06/13 PJK Initial release
      !+ad_hist  30/06/14 PJK Added error handling
      !+ad_stat  Okay
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments
      real(kind=double), intent(in) :: e_Li
      character(len=*), intent(in) :: breeder

      !  Local variables
      real(kind=double) :: TBR_breed

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (trim(breeder) == 'Orthosilicate') then

         TBR_breed = 0.1361D0*log(e_Li) + 0.6331D0

      else if (trim(breeder) == 'Metatitanate') then

         TBR_breed = 0.1564D0*log(e_Li) + 0.9140D0

      else if (trim(breeder) == 'Zirconate') then

         TBR_breed = 0.1640D0*log(e_Li) + 0.4325D0

      else
         call report_error(128)
      end if

    end function TBR_breed

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function TBR_ports(n_ports_div, n_ports_H_CD_IB, n_ports_H_CD_OB, H_CD_ports)

      !+ad_name  tbr_ports
      !+ad_summ  Returns a fit to the tritium breeding ratio with different
      !+ad_summ  machine port types
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  F Franza, KIT (original MATLAB implementation)
      !+ad_cont  None
      !+ad_args  n_ports_div : input integer : number of divertor ports
      !+ad_args  n_ports_h_cd_ib : input integer : number of inboard H/CD ports
      !+ad_args  n_ports_h_cd_ob : input integer : number of outboard H/CD ports
      !+ad_args  h_cd_ports : input character string : H/CD port size;
      !+ad_argc          <UL><LI>'small' or <LI>'large'</UL>
      !+ad_desc  This routine provides the dependence of the tritium breeding
      !+ad_desc  ratio on the number and size of machine ports.
      !+ad_desc  The equatorial heating/current drive ports may be specified as
      !+ad_desc  being either <CODE>'small'</CODE> (1.27 x 1.5 m2) or
      !+ad_desc  <CODE>'large'</CODE> (3 x 3 m2).
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  06/06/13 PJK Initial release
      !+ad_stat  Okay
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments
      integer, intent(in) :: n_ports_div, n_ports_H_CD_IB, n_ports_H_CD_OB
      character(len=*), intent(in) :: H_CD_ports

      !  Local variables
      real(kind=double) :: TBR_ports

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (trim(H_CD_ports) == 'small') then

         TBR_ports = (1.0D0 - 0.0055D0*n_ports_div) * &
              (1.0D0 - 0.0031D0*n_ports_H_CD_IB) * &
              (1.0D0 - 0.0031D0*n_ports_H_CD_OB)

      else  !  if (trim(H_CD_ports) == 'large') then

         TBR_ports = (1.0D0-0.0055D0*n_ports_div) * &
              (1.0D0 - 0.0107D0*n_ports_H_CD_IB) * &
              (1.0D0 - 0.0107D0*n_ports_H_CD_OB)

      end if

    end function TBR_ports

  end subroutine tritium_breeding_ratio

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fast_neutron_fluence(phi_n_vv_IB_start,phi_n_vv_OB_start, &
       phi_n_IB_TFC,phi_n_OB_TFC)

    !+ad_name  fast_neutron_fluence
    !+ad_summ  Calculates fast neutron fluence within the KIT blanket sub-assemblies
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  phi_n_vv_IB_start : output real : flux at inner edge of IB VV (n/cm2/s)
    !+ad_args  phi_n_vv_OB_start : output real : flux at inner edge of OB VV (n/cm2/s)
    !+ad_args  phi_n_IB_TFC      : output real : lifetime fluence at IB TF coil (n/cm2)
    !+ad_args  phi_n_OB_TFC      : output real : lifetime fluence at OB TF coil (n/cm2)
    !+ad_desc  This routine calculates the fast neutron flux profiles within each
    !+ad_desc  blanket sub-assembly, assuming an exponential decay with distance through
    !+ad_desc  each region, with the decay indices dependent on the material fractions.
    !+ad_desc  <P>At present, the arrays contain only NP=2 elements, i.e. contain the
    !+ad_desc  values at the inner and outer radial locations; however, if required,
    !+ad_desc  they may be changed easily to provide several points for plotting
    !+ad_desc  purposes, for example.
    !+ad_desc  <P>The total neutron fluence over the plant lifetime reaching the
    !+ad_desc  TF coils is also calculated.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_hist  05/11/13 PJK Corrected lambda_q_VV to lambda_n_VV in two places
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(out) :: phi_n_VV_IB_start,phi_n_VV_OB_start
    real(kind=double), intent(out) :: phi_n_IB_TFC, phi_n_OB_TFC

    !  Local variables
    integer, parameter :: K_tau = 31536000  ! [sec/yr] Number of seconds per year
    real(kind=double), dimension(np) :: phi_n_BZ_IB, phi_n_BM_IB
    real(kind=double), dimension(np) :: phi_n_BP_IB, phi_n_VV_IB
    real(kind=double), dimension(np) :: phi_n_BZ_OB, phi_n_BM_OB
    real(kind=double), dimension(np) :: phi_n_BP_OB, phi_n_VV_OB
    real(kind=double) :: nwl_ratio

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nwl_ratio = NWL_av/NWL_av_PPCS

    !  Inboard fast neutron flux profiles (n/cm2/second)
    !  Fast neutron flux profile in IB BZ
    phi_n_BZ_IB(:) = nwl_ratio * phi_0_n_BZ_IB * &
         exp(-x_BZ_IB(:)/lambda_n_BZ_IB)

    !  Fast neutron flux profile in IB BM
    phi_n_BM_IB(:) = phi_n_BZ_IB(np) * exp(-x_BM_IB(:)/lambda_q_BM_IB)

    !  Fast neutron flux profile in IB BP
    phi_n_BP_IB(:) = phi_n_BM_IB(np) * exp(-x_BP_IB(:)/lambda_q_BP_IB)

    !  Fast neutron flux profile in IB VV
    phi_n_VV_IB(:) = phi_n_BP_IB(np) * exp(-x_VV_IB(:)/lambda_n_VV)
    phi_n_vv_IB_start = phi_n_VV_IB(1)

    !  Fast neutron lifetime fluence at IB TF coil (n/cm2)
    phi_n_IB_TFC = phi_n_VV_IB(np) * t_plant * K_tau

    !  Outboard fast neutron flux profiles (n/cm2/second)
    !  Fast neutron flux profile in OB BZ
    phi_n_BZ_OB(:) = nwl_ratio * phi_0_n_BZ_OB * &
         exp(-x_BZ_OB(:)/lambda_n_BZ_OB)

    !  Fast neutron flux profile in OB BM
    phi_n_BM_OB(:) = phi_n_BZ_OB(np) * exp(-x_BM_OB(:)/lambda_q_BM_OB)

    !  Fast neutron flux profile in OB BP
    phi_n_BP_OB(:) = phi_n_BM_OB(np) * exp(-x_BP_OB(:)/lambda_q_BP_OB)

    !  Fast neutron flux profile in OB VV
    phi_n_VV_OB(:) = phi_n_BP_OB(np) * exp(-x_VV_OB(:)/lambda_n_VV)
    phi_n_vv_OB_start = phi_n_VV_OB(1)

    !  Fast neutron lifetime fluence at OB TF coil (n/cm2)
    phi_n_OB_TFC = phi_n_VV_OB(np) * t_plant * K_tau

  end subroutine fast_neutron_fluence

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine He_production_vacuum_vessel(phi_n_VV_IB_start,phi_n_VV_OB_start, &
       vvhemini,vvhemino,vvhemaxi,vvhemaxo)

    !+ad_name  he_production_vacuum_vessel
    !+ad_summ  Calculates helium concentrations in the vacuum vessel at the end
    !+ad_summ  of the plant lifetime
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  phi_n_vv_IB_start : input real : n flux at inner edge of IB VV (n/cm2/s)
    !+ad_args  phi_n_vv_OB_start : input real : n flux at inner edge of OB VV (n/cm2/s)
    !+ad_args  vvhemini : output real : final He concentr. at outer edge of IB VV (appm)
    !+ad_args  vvhemino : output real : final He concentr. at outer edge of OB VV (appm)
    !+ad_args  vvhemaxi : output real : final He concentr. at inner edge of IB VV (appm)
    !+ad_args  vvhemaxo : output real : final He concentr. at inner edge of OB VV (appm)
    !+ad_desc  This routine calculates the helium production profiles, and the
    !+ad_desc  minimum and maximum helium concentrations in the vacuum vessel
    !+ad_desc  at the end of the plant lifetime.
    !+ad_desc  <P>At present, the arrays contain only NP=2 elements, i.e. contain the
    !+ad_desc  values at the inner and outer radial locations; however, if required,
    !+ad_desc  they may be changed easily to provide several points for plotting
    !+ad_desc  purposes, for example.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(in) :: phi_n_VV_IB_start,phi_n_VV_OB_start
    real(kind=double), intent(out) :: vvhemini,vvhemino,vvhemaxi,vvhemaxo

    !  Local variables
    real(kind=double), dimension(np) :: Gamma_He_IB, Gamma_He_OB
    real(kind=double), dimension(np) :: C_He_IB, C_He_OB

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Helium production rate (appm/year)
    Gamma_He_IB(:) = phi_n_VV_IB_start / phi_n_0_VV_ref * &
         Gamma_He_0_ref * exp(-x_VV_IB(:)/lambda_He_VV)

    Gamma_He_OB(:) = phi_n_VV_OB_start / phi_n_0_VV_ref * &
         Gamma_He_0_ref * exp(-x_VV_OB(:)/lambda_He_VV)

    !  Helium concentrations at end of plant lifetime (appm)
    C_He_IB(:) = Gamma_He_IB(:) * t_plant
    C_He_OB(:) = Gamma_He_OB(:) * t_plant

    !  Minimum concentrations occur furthest from the plasma
    vvhemini = C_He_IB(np)
    vvhemino = C_He_OB(np)

    !  Maximum concentrations occur nearest the plasma
    vvhemaxi = C_He_IB(1)
    vvhemaxo = C_He_OB(1)

  end subroutine He_production_vacuum_vessel

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blanket_lifetime(t_bl_FPY,t_bl_Y)

    !+ad_name  blanket_lifetime
    !+ad_summ  Calculates the blanket lifetime
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  t_bl_fpy : output real : blanket lifetime (full power years)
    !+ad_args  t_bl_y   : output real : blanket lifetime (calendar years)
    !+ad_desc  This routine calculates the blanket lifetime, assuming that the
    !+ad_desc  maximum allowed neutron damage to the EUROFER steel is 60 dpa.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind=double), intent(out) :: t_bl_FPY, t_bl_Y

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Lifetime in full-power years
    !  10 dpa equates to 1 MW-yr/m2 (steel)
    t_bl_FPY = D_EU_max / (10.0D0*NWL_av*f_peak)

    !  Lifetime in calendar years, given availability and pulsed factors
    t_bl_Y = t_bl_FPY / (alpha_m*alpha_puls)

  end subroutine blanket_lifetime

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine component_masses
    !+ad_name  component_masses
    !+ad_summ  Calculations for component masses
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculations for component masses
    !+ad_prob  None
    !+ad_hist  23/02/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Mass of steel in blanket (kg)
    whtblss = denstl * ( volblkti/blnkith * ( blbuith * fblss + blbmith * (1.0D0-fblhebmi) + &
		blbpith * (1.0D0-fblhebpi) ) + volblkto/blnkoth * ( blbuoth * fblss + &
		blbmoth * (1.0D0-fblhebmo) + blbpoth * (1.0D0-fblhebpo) ) )
            
    !  Mass of beryllium in blanket (kg)
    whtblbe = 1850.0D0 * fblbe * ( (volblkti * blbuith/blnkith) + (volblkto * blbuoth/blnkoth) )
       
    !  Mass of breeder material in blanket (kg)
    whtblbreed = densbreed * fblbreed * ( (volblkti * blbuith/blnkith) + (volblkto * blbuoth/blnkoth) )
    
    !  Mass of blanket (kg)
    whtblkt = whtblss + whtblbe + whtblbreed

	!  Void fraction of blanket inboard portion
    vfblkti = volblkti/volblkt * ( (blbuith/blnkith) * (1.0D0 - fblbe - fblbreed - fblss) &
       + (blbmith/blnkith) * fblhebmi + (blbpith/blnkith) * fblhebpi )
       
    !  Void fraction of blanket outboard portion
    vfblkto = volblkto/volblkt * ( (blbuoth/blnkoth) * (1.0D0 - fblbe - fblbreed - fblss) &
       + (blbmoth/blnkoth) * fblhebmo + (blbpoth/blnkoth) * fblhebpo )
       
    !  Void fraction of blanket
    vfblkt = vfblkti + vfblkto
  
  end subroutine
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine write_kit_hcpb_output
	!+ad_name  write_kit_hcpb_output
    !+ad_summ  Write output to file for KIT HCPB model
    !+ad_type  Subroutine
    !+ad_auth  J. Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  This subroutine outputs the CCFE HCPB model results to 
    !+ad_desc  an output file
    !+ad_prob  None
    !+ad_hist  12/03/15 JM  Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    call oheadr(ofile, 'Blanket model output - KIT HCPB model')   
    call osubhd(ofile, 'Blanket Composition :')
 
    !call ovarrf(ofile, 'Titanium beryllide fraction', '(fbltibe12)', fbltibe12)
    !call ovarrf(ofile, 'Lithium orthosilicate fraction', '(fblli2sio4)', fblli2sio4)
    !call ovarrf(ofile, 'Steel fraction', '(fblss)', fblss)
    !call ovarrf(ofile, 'Void fraction coolant', '(vfcblkt)', vfcblkt)
    !call ovarrf(ofile, 'Void fraction purge gas', '(vfpblkt)', vfpblkt)
    
    !call osubhd(ofile, 'Component Volumes :')
    
    ! TODO: Volume of FW ARMOUR
    !call ovarrf(ofile, 'First Wall Volume (m3)', '(volfw)', volfw)
    !call ovarrf(ofile, 'Blanket Volume (m3)', '(volblkt)', volblkt)
    !call ovarrf(ofile, 'Shield Volume (m3)', '(volshld)', volshld)
    !call ovarre(ofile, 'Vacuum vessel volume (m3)', '(vdewin)', vdewin)
        
    !call osubhd(ofile, 'Component Masses :')
    
    ! TODO: Mass of FW ARMOUR
    !call ovarre(ofile, 'First Wall Mass (kg)', '(fwmass)', fwmass)
    !call ovarre(ofile, 'Blanket Mass - Total(kg)', '(whtblkt)', whtblkt)
    !call ovarre(ofile, 'Blanket Mass - TiBe12 (kg)', '(whtbltibe12)', whtbltibe12)
    !call ovarre(ofile, 'Blanket Mass - Li2SiO4 (kg)', '(whtblli2sio4)', whtblli2sio4)
    !call ovarre(ofile, 'Blanket Mass - Steel (kg)', '(whtblss)', whtblss)
    !call ovarre(ofile, 'Shield Mass (kg)', '(whtshld)', whtshld)
    !call ovarre(ofile, 'Vacuum vessel mass (kg)', '(cryomass)', cryomass)
    
    !  Nuclear heting section
    !call osubhd(ofile, 'Nuclear heating :')
    
    !call ovarre(ofile, 'Average neutron wall load (MW/m2)','(wallmw)', wallmw)
    !call ovarre(ofile, 'First wall full-power lifetime (years)', '(fwlife)', fwlife)
    !call oblnkl(ofile)
    
    !  ST centre post
    !if (itart == 1) then
    !   call osubhd(ofile,'(Copper centrepost used)')
    !   call ovarre(ofile,'ST centrepost heating (MW)','(pnuccp)',pnuccp)
    !end if
    
    !call ovarre(ofile, 'Total nuclear heating in TF coil (MW)', '(ptfnuc)', ptfnuc) 
    !call ovarre(ofile, 'Total nuclear heating in FW (MW)', '(pnucfw)', pnucfw) 
	!call ovarre(ofile, 'Total nuclear heating in the blanket (including emult) (MW)', '(pnucblkt)', pnucblkt) 
	!call ovarre(ofile, 'Total nuclear heating in the shield (MW)', '(pnucshld)', pnucshld)
	!call ovarre(ofile, 'Total nuclear heating in the divertor (MW)', '(pnucdiv)', pnucdiv)
    !call oblnkl(ofile)
    
    !call osubhd(ofile,'Thermodynamic Model Output :')
    
    !call ovarin(ofile, 'Switch for plant secondary cycle ', '(secondary_cycle)', secondary_cycle) 
    !call ovarre(ofile, 'First wall coolant pressure (Pa)', '(coolp)', coolp)
    !call ovarre(ofile, 'Inner radius of inboard first wall coolant channels (m)', '(afwi)', afwi)
    !call ovarre(ofile, 'Outer radius of inboard first wall coolant channels (m)', '(bfwi)', bfwi)
    !call ovarre(ofile, 'Inner radius of outboard first wall coolant channels (m)', '(afwo)', afwo)
    !call ovarre(ofile, 'Outer radius of outboard first wall coolant channels (m)', '(bfwo)', bfwo)
    !call ovarrf(ofile, 'Inlet temperature of coolant (K)', '(inlet_temp)', inlet_temp)
    !call ovarrf(ofile, 'Outlet temperature of coolant (K)', '(outlet_temp)', outlet_temp)
    !call ovarre(ofile, 'Maximum temperature of first wall material (K)', '(tfwmatmax)', tfwmatmax)
    !call ovarin(ofile, 'No of inboard blanket modules poloidally', '(nblktmodpi)', nblktmodpi)
    !call ovarin(ofile, 'No of inboard blanket modules toroidally', '(nblktmodti)', nblktmodti)
    !call ovarin(ofile, 'No of outboard blanket modules poloidally', '(nblktmodpo)', nblktmodpo)
    !call ovarin(ofile,'No of outboard blanket modules toroidally', '(nblktmodto)', nblktmodto)
    !call ovarre(ofile, 'Isentropic efficiency of first wall / blanket coolant pumps', '(etaiso)', etaiso)
    
    !call osubhd(ofile, 'Other volumes, masses and areas :')
    !call ovarre(ofile, 'First wall area (m2)', '(fwarea)', fwarea)
    !call ovarre(ofile, 'External cryostat radius (m)', '(rdewex)', rdewex)
    !call ovarre(ofile, 'External cryostat half-height (m)', '(zdewex)', zdewex)
    !call ovarre(ofile, 'External cryostat volume (m3)', '(vdewex)', vdewex)
    !call ovarre(ofile, 'Total cryostat + vacuum vessel mass (kg)', '(dewmkg)', dewmkg)
    !call ovarre(ofile, 'Divertor area (m2)', '(divsur)', divsur)
    !call ovarre(ofile, 'Divertor mass (kg)', '(divmas)', divmas)
    
  end subroutine

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
