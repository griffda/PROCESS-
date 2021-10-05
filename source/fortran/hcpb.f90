
module ccfe_hcpb_module
  !! author: J Morris (UKAEA)
  !!
  !! This module contains the PROCESS CCFE HCPB blanket model
  !! based on CCFE HCPB model from the PROCESS engineering paper
  !! PROCESS Engineering paper (M. Kovari et al.)
  !!
  !!### References
  !!
  !! - Kovari et al., Fusion Engineering and Design 104 (2016) 9-20  
  
  use, intrinsic :: iso_fortran_env, only: dp=>real64

  implicit none

  ! Variables for output to file
  integer, private :: ip, ofile

  real(8), private :: W_density
  !! Tungsten density [kg/m3]

  ! Smeared densities of build sections
  real(8), private :: armour_density
  !! FW armour density [kg/m3]

  real(8), private :: fw_density
  !! FW density [kg/m3]

  real(8), private :: blanket_density
  !! Blanket density [kg/m3]

  real(8), private :: shield_density
  !! Shield density [kg/m3]

  real(8), private :: vv_density
  !! Vacuum vessel density [kg/m3]

  real(8), private :: volfw
  !! First wall volume [m3]

  real(8), private :: x_blanket
  !! Blanket exponent (tonne/m2)

  real(8), private :: x_shield
  !! Shield exponent (tonne/m2)

  real(8), private :: tfc_nuc_heating
  !! Unit nuclear heating in TF coil (W per W of fusion power)

  real(8), private :: fw_armour_u_nuc_heating
  !! Unit heating of FW and armour in FW armour (W/kg per W of fusion power)

  real(8), private :: shld_u_nuc_heating
  !! Unit nuclear heating in shield (W per W of fusion power)

  real(8), private :: hblnkt
  !! Blanket internal half-height (m)

  real(8), private :: hshld
  !! Shield internal half-height (m)

  real(8), private :: hcryopf
  !! Clearance between uppermost PF coil and cryostat lid (m)

  real(8), private :: hvv
  !! Vacuum vessel internal half-height (m)

  real(8), private :: volshldi, volshldo
  !! Volume of inboard and outboard shield (m3)

  real(8), private :: vffwi, vffwo
  !! Inboard/outboard FW coolant void fraction

  real(8), private :: psurffwi, psurffwo
  !! Surface heat flux on first wall (MW) (sum = pradfw)

  real(8), private :: bldepti, bldepto
  !! Inboard/outboard blanket coolant channel length (radial direction) (m)

  real(8), private :: blwidti, blwidto
  !! Inboard/outboard blanket mid-plan toroidal circumference for segment (m)

  real(8), private :: bllengi, bllengo
  !! Inboard/outboard blanket segment poloidal length (m)

  real(8), private :: bzfllengi, bzfllengo
  !! Inboard/outboard blanket flow lengths (m)

  real(8), private :: pnucfwi, pnucfwo
  !! Inboard/outboard first wall nuclear heating (MW)

  real(8), private :: tpeakfwi, tpeakfwo
  !! Inboard/outboard first wall peak temperature (K)

  real(8), private :: mffwi, mffwo, mffw
  !! Inboard/outboard total mass flow rate to remove inboard FW power (kg/s)

  real(8), private :: npfwi, npfwo
  !! Inboard/utboard total number of pipes

  real(8), private :: mffwpi, mffwpo
  !! Inboard/outboard mass flow rate per coolant pipe (kg/s)

  real(8), private :: pnucblkti, pnucblkto
  !! Neutron power deposited inboard/outboard blanket blanket (MW)

  real(8), private :: mfblkti, mfblkto, mfblkt
  !! Inboard/outboard blanket mass flow rate for coolant (kg/s)

  real(8), private :: mftotal
  !! Total mass flow rate for coolant (kg/s)

  real(8), private :: npblkti, npblkto
  !! Inboard/outboard total num of pipes

  real(8), private :: mfblktpi, mfblktpo
  !! Inboard/outboard mass flow rate per coolant pipe (kg/s)

  real(8), private :: velblkti, velblkto
  !! Inboard/outboard coolant velocity in blanket (m/s)

  real(8), private :: htpmw_fwi, htpmw_fwo
  !! Inboard/outboard first wall pumping power (MW)

  real(8), private :: htpmw_blkti, htpmw_blkto
  !! Inboard/outboard blanket pumping power (MW)

  real(8), private :: pnuc_tot_blk_sector
  !! Total nuclear power deposited in blanket covered sector (FW, BLKT, SHLD, TF) (MW)

  real(8), private :: exp_blanket, exp_shield1, exp_shield2
  !! Exponential factors in nuclear heating calcs

  real(8), private :: fblss_ccfe, fblli2sio4, fbltibe12
  !! Fractions of blanket by volume: steel, lithium orthosilicate, titanium beryllide

contains

  subroutine init_ccfe_hcpb_module
    !! Initialise module variables
    implicit none

    W_density = 19.25D0 * 1000.0D0
    ip = 0
    ofile = 0
    armour_density = 0.0D0
    fw_density = 0.0D0
    blanket_density = 0.0D0
    shield_density = 0.0D0
    vv_density = 0.0D0
    volfw = 0.0D0
    x_blanket = 0.0D0
    x_shield = 0.0D0
    tfc_nuc_heating = 0.0D0
    fw_armour_u_nuc_heating = 0.0D0
    shld_u_nuc_heating = 0.0D0
    hblnkt = 0.0D0
    hshld = 0.0D0
    hcryopf = 0.0D0
    hvv = 0.0D0
    volshldi = 0.0D0
    volshldo = 0.0D0
    vffwi = 0.0D0
    vffwo = 0.0D0
    psurffwi = 0.0D0
    psurffwo = 0.0D0
    bldepti = 0.0D0
    bldepto = 0.0D0
    blwidti = 0.0D0
    blwidto = 0.0D0
    bllengi = 0.0D0
    bllengo = 0.0D0
    bzfllengi = 0.0D0
    bzfllengo = 0.0D0
    pnucfwi = 0.0D0
    pnucfwo = 0.0D0
    tpeakfwi = 0.0D0
    tpeakfwo = 0.0D0
    mffwi = 0.0D0
    mffwo = 0.0D0
    mffw = 0.0D0
    npfwi = 0.0D0
    npfwo = 0.0D0
    mffwpi = 0.0D0
    mffwpo = 0.0D0
    pnucblkti = 0.0D0
    pnucblkto = 0.0D0
    mfblkti = 0.0D0
    mfblkto = 0.0D0
    mfblkt = 0.0D0
    mftotal = 0.0D0
    npblkti = 0.0D0
    npblkto = 0.0D0
    mfblktpi = 0.0D0
    mfblktpo = 0.0D0
    velblkti = 0.0D0
    velblkto = 0.0D0
    htpmw_fwi = 0.0D0
    htpmw_fwo = 0.0D0
    htpmw_blkti = 0.0D0
    htpmw_blkto = 0.0D0
    exp_blanket = 0.0D0
    exp_shield1 = 0.0D0
    exp_shield2 = 0.0D0
    fblss_ccfe = 0.0D0
    fblli2sio4 = 0.0D0
    fbltibe12 = 0.0D0
  end subroutine init_ccfe_hcpb_module

  subroutine ccfe_hcpb(outfile, iprint)
    !! author: J Morris (UKAEA)
    !!
    !! This routine calculates nuclear heating for the CCFE HCPB blanket model.
    !!
    !!### References
    !!
    !! - Kovari et al., Fusion Engineering and Design 104 (2016) 9-20   

    use build_variables, only: fwith, fwoth, r_tf_inboard_mid, tfcth, &
      gapds, shldith, hmax, r_tf_outboard_mid, r_sh_inboard_out, &
      r_sh_inboard_in, scrapli
    use fwbs_variables, only: afw, coolwh, emult, pnuc_cp_tf, pnuc_cp_sh, &
      pnuc_cp, fw_wall, pnucfw, pnucblkt, pnucshld, ptfnuc, fdiv, emultmw, &
      pnucdiv, neut_flux_cp
    use physics_variables, only: itart, pneutmw, idivrt, rmajor, kappa, & 
      triang, rminor

    implicit none

    integer, intent(in) :: iprint
    !!switch for writing to output file (1=yes)

    integer, intent(in) :: outfile
    !! output file unit

    real(8) :: f_geom_blanket
    !! Solid angle fraction covered by the breeding blankets

    real(8) :: f_geom_cp
    !! Solid angle fraction of neutrons that hit the centrepost shield [-]

    real(8) :: r_sh_inboard_out_top
    !! CP radius at the point of maximum sield radius [m]
    !! The maximum shield radius is assumed to be at the X-point

    real(8) :: h_sh_max_r
    !! Half height of the CP at the largest shield radius [m]

    ip = iprint
    ofile = outfile

    ! MDK (27/11/2015)
    fwith = 2.0D0*afw + 2.0D0*fw_wall
    fwoth = fwith

    ! Coolant type
    coolwh = 1
    ! Note that the first wall coolant is now input separately.

    ! Energy multiplication
    emult = 1.269

    ! Calculate blanket, shield, vacuum vessel and cryostat volumes
    call component_volumes

    ! Centrepost neutronics
    if ( itart == 1 ) then

      ! CP radius at the point of maximum sield radius [m]
      r_sh_inboard_out_top = rmajor - rminor * triang - 3.0D0*scrapli

      ! Half height of the CP at the largest shield radius [m]
      h_sh_max_r = rminor * kappa

      ! Calculating the CP solid angle coverage fraction
      ! Rem : This calculation considered the shield flaring
      !       while the MCNP based neutronincs considers a 
      !       cylindre
      call st_cp_angle_fraction( h_sh_max_r, r_sh_inboard_out, & ! Inputs
                                 r_sh_inboard_out_top, rmajor, & ! Inputs
                                 f_geom_cp )

      ! TF fast neutron flux (E > 0.1 MeV) [m^{-2}.s^{-1}]
      call st_tf_centrepost_fast_neut_flux( pneutmw, shldith, rmajor, & ! Inputs
                                            neut_flux_cp )              ! Outputs

      ! TF, shield and total CP nuclear heating [MW]
      call st_centrepost_nuclear_heating( pneutmw, shldith,               & ! Inputs
                                          pnuc_cp_tf, pnuc_cp_sh, pnuc_cp ) ! Outputs
  
    else ! No CP
      f_geom_cp = 0.0D0
      pnuc_cp_tf = 0.0D0
      pnuc_cp_sh = 0.0D0
      pnuc_cp = 0.0D0
      neut_flux_cp = 0.0D0
    end if

    call component_masses

    ! Calculate the nuclear heating
    ! Rem : The heating power will be normalized to the neutron power using
    !       the divertor and the centrepost (for itart == 1), 
    call nuclear_heating_magnets
    call nuclear_heating_fw
    call nuclear_heating_blanket
    call nuclear_heating_shield
    call nuclear_heating_divertor

    ! Normalisation of the nuclear heating
    ! The nuclear heating are noramalized assuming no energy multiplication
    ! in the divertor and the centrepost
    ! Assume that all the neutrons are absorbed. (Not applicable for very thin blankets)
    ! Rem SK : This calculation effectively only uses the angular fractions to get 
    !          the energy multiplication and hence the power balance ...

    ! Split neutron power to main wall between fw, bkt, shld and TF with same 
    ! fractions as before.
    ! Total nuclear power deposited in the blancket sector (MW)
    pnuc_tot_blk_sector = pnucfw + pnucblkt + pnucshld + ptfnuc

    ! Total nuclear power deposited in the 
    if ( pnuc_tot_blk_sector < 1.0d0 .or. pnuc_tot_blk_sector /= pnuc_tot_blk_sector ) then
        write(*,*)'pnucfw =', pnucfw, ' and ', 'pnucblkt =', pnucblkt
        write(*,*)'pnucshld =', pnucshld, ' ptfnuc =', ptfnuc
    end if


    ! Solid angle fraction taken by the breeding blankets/shields
    f_geom_blanket = 1.0D0 - dble(idivrt) * fdiv - f_geom_cp

    ! Power to the first wall (MW)
    pnucfw = ( pnucfw / pnuc_tot_blk_sector ) * emult * f_geom_blanket * pneutmw

    ! Power to the blanket (MW)
    pnucblkt = ( pnucblkt / pnuc_tot_blk_sector ) * emult  * f_geom_blanket * pneutmw
    
    ! Power to the shield(MW)
    ! The power deposited in the CP shield is added back 
    pnucshld = ( pnucshld / pnuc_tot_blk_sector ) * emult * f_geom_blanket * pneutmw

    ! Power to the TF coils (MW)
    ! The power deposited in the CP shield is added back 
    ptfnuc = ( ptfnuc / pnuc_tot_blk_sector ) * emult * f_geom_blanket * pneutmw

    ! Power deposited in the CP
    pnuc_cp_sh = f_geom_cp * pneutmw - pnuc_cp_tf

    
    ! Old code kept for backward compatibility
    ! ---
    ! pnucdiv is not changed.
    ! The energy due to multiplication, by subtraction:
    !emultmw = pnucfw + pnucblkt + pnucshld + ptfnuc + pnucdiv - pneutmw
    ! ---

    ! New code, a bit simpler
    emultmw = ( emult - 1.0D0 ) * f_geom_blanket * pneutmw

    ! powerflow calculation for pumping power
    call powerflow_calc

    ! output
    if (ip == 0) return
    call write_ccfe_hcpb_output

  end subroutine ccfe_hcpb

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine component_volumes
    !! Calculate the blanket, shield, vacuum vessel and cryostat volumes
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the blanket, shield, vacuum vessel and cryostat volumes
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fwbs_variables, only: fwbsshape
    use physics_variables, only: itart

    implicit none

    call blanket_half_height
    call shield_half_height
    call vv_half_height

    ! D-shaped blanket and shield
    if ((itart == 1).or.(fwbsshape == 1)) then

       call dshaped_blanket
       call dshaped_shield
       call dshaped_vv

    ! Elliptical blanket and shield
    else

     call elliptical_blanket
     call elliptical_shield
     call elliptical_vv

    end if

    ! Apply coverage factors to volumes and surface areas
    call apply_coverage_factors

    ! Calculate cryostat geometry
    call external_cryo_geometry

  end subroutine component_volumes

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blanket_half_height
    !! Calculate the blanket half-height
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the blanket half-height
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: vgap, blnktth, scrapli, scraplo, fwith, fwoth
    use divertor_variables, only: divfix
    use physics_variables, only: rminor, kappa, idivrt

    implicit none

    ! Blanket bottom/top half-height (m)
    real(8) :: hbot, htop

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate blanket internal lower half-height (m)
    hbot = rminor*kappa + vgap + divfix - blnktth

    ! Calculate blanket internal upper half-height (m)
    ! if a double null machine then symmetric otherwise asymmetric
    if (idivrt == 2) then
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth)
    end if

    ! Average of top and bottom (m)
    hblnkt = 0.5D0*(htop + hbot)

  end subroutine blanket_half_height

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine shield_half_height
    !! Calculate the shield half-height
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the shield half-height
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: vgap, scrapli, scraplo, fwith, fwoth, blnktth
    use divertor_variables, only: divfix
    use physics_variables, only: rminor, kappa, idivrt

    implicit none

    ! Shield bottom/top half-height (m)
    real(8) :: hbot, htop

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate shield internal lower half-height (m)
    hbot = rminor*kappa + vgap + divfix

    ! Calculate shield internal upper half-height (m)
    ! if a double null machine then symmetric otherwise asymmetric
    if (idivrt == 2) then
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) + blnktth
    end if

    ! Average of top and bottom (m)
    hshld = 0.5D0*(htop + hbot)

  end subroutine shield_half_height

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vv_half_height
    !! Calculate the vacuum vessel half-height
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the vacuum vessel half-height
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: hmax, vgap2, d_vv_bot, d_vv_top, &
      scrapli, scraplo, fwith, fwoth, blnktth, shldtth
    use physics_variables, only: idivrt, rminor, kappa

    implicit none

    ! Vacuum vessel bottom/top internal half-height (m)
    real(8) :: hbot, htop

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Calculate vacuum vessel internal lower half-height (m)
    hbot = hmax - vgap2 - d_vv_bot

    ! Calculate vacuum vessel internal upper half-height (m)
    ! if a double null machine then symmetric otherwise asymmetric
    if (idivrt == 2) then
       htop = hmax - vgap2 - d_vv_top
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) &
            + blnktth + shldtth
    end if

    ! Average of top and bottom (m)
    hvv = 0.5D0*(htop + hbot)

  end subroutine vv_half_height

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshaped_blanket
    !! Calculate the blanket surface area and volume using dshaped scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the blanket surface area and volume using dshaped scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: rsldi, shldith, blnkith, fwith, scrapli, scraplo, &
      fwoth, blareaib, blareaob, blarea, blnkoth, blnktth
    use fwbs_variables, only: volblkti, volblkto, volblkt
    use physics_variables, only: rminor
    use maths_library, only: dshellarea, dshellvol

    implicit none

    ! Major radius to outer edge of inboard blanket (m)
    real(8) :: r1

    ! Horizontal distance between inside edges of blanket (m)
    ! i.e. outer radius of inboard part to inner radius of outboard part
    real(8) :: r2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Major radius to outer edge of inboard blanket (m)
    r1 = rsldi + shldith + blnkith

    ! Horizontal distance between inside edges of blanket (m)
    r2 = fwith + scrapli + 2.0D0*rminor + scraplo + fwoth

    ! Calculate blanket surface area, assuming 100% coverage
    call dshellarea(r1, r2, hblnkt, blareaib, blareaob, blarea)

    ! Calculate blanket volumes, assuming 100% coverage
    call dshellvol(r1, r2, hblnkt, blnkith, blnkoth, blnktth, volblkti, volblkto, volblkt)

  end subroutine dshaped_blanket

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshaped_shield
    !! Calculate the shield surface area and volume using dshaped scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the shield surface area and volume using dshaped scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: rsldi, shldith, blnkith, fwith, scrapli, scraplo, &
      fwoth, blnkoth, shareaib, shareaob, sharea, shldoth, shldtth
    use fwbs_variables, only: volshld
    use physics_variables, only: rminor
    use maths_library, only: dshellarea, dshellvol

    implicit none

    ! Major radius to outer edge of inboard shield (m)
    real(8) :: r1

    ! Horizontal distance between inside edges of shield (m)
    ! i.e. outer radius of inboard part to inner radius of outboard part
    real(8) :: r2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Major radius to outer edge of inboard shield (m)
    r1 = rsldi + shldith

    ! Horizontal distance between inside edges of shield (m)
    r2 = blnkith + fwith + scrapli + 2.0D0*rminor + scraplo + fwoth + blnkoth

    ! Calculate shield surface area, assuming 100% coverage
    call dshellarea(r1, r2, hshld, shareaib, shareaob, sharea)

    ! Calculate shield volumes, assuming 100% coverage
    call dshellvol(r1, r2, hshld, shldith, shldoth, shldtth, volshldi, volshldo, volshld)

  end subroutine dshaped_shield

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshaped_vv
    !! Calculate the vacuum vessel volume using dshaped scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the vacuum vessel volume using dshaped scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: rsldi, rsldo, d_vv_in, d_vv_out, &
                               d_vv_top, d_vv_bot
    use fwbs_variables, only: vdewin, fvoldw
    use maths_library, only: dshellvol

    implicit none

    ! Major radius to outer edge of inboard section (m)
    real(8) :: r1

    ! Horizontal distance between inside edges (m)
    real(8) :: r2

    ! Unused outputs from dshellvol
    real(8) :: v1, v2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Major radius to outer edge of inboard section (m)
    r1 = rsldi

    ! Horizontal distance between inside edges (m)
    ! i.e. outer radius of inboard part to inner radius of outboard part
    r2 = rsldo - r1

    ! Calculate volume, assuming 100% coverage
    call dshellvol(r1, r2, hvv, d_vv_in, d_vv_out, &
                  (d_vv_top+d_vv_bot)/2, v1, v2, vdewin)

    ! Apply area coverage factor
    vdewin = fvoldw*vdewin

  end subroutine dshaped_vv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine elliptical_blanket
    !! Calculate the blanket surface area and volume using elliptical scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the blanket surface area and volume using elliptical scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: rsldi, shldith, blnkith, rsldo, shldoth, blnkoth, &  
      blareaib, blareaob, blarea, blnktth
    use fwbs_variables, only: volblkti, volblkto, volblkt
    use physics_variables, only: rmajor, rminor, triang
    use maths_library, only: eshellarea, eshellvol

    implicit none

    ! Major radius to centre of inboard and outboard ellipses (m)
    real(8) :: r1

    ! Distance between r1 and outer edge of inboard blanket (m)
    real(8) :: r2

    ! Distance between r1 and inner edge of outboard blanket (m)
    real(8) :: r3

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Major radius to centre of inboard and outboard ellipses (m)
    ! (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang

    ! Distance between r1 and outer edge of inboard blanket (m)
    r2 = r1 - (rsldi + shldith + blnkith)

    ! Distance between r1 and inner edge of outboard blanket (m)
    r3 = (rsldo - shldoth - blnkoth) - r1

    ! Calculate blanket surface area, assuming 100% coverage
    call eshellarea(r1, r2, r3, hblnkt, blareaib, blareaob, blarea)

    ! Calculate blanket volumes, assuming 100% coverage
    call eshellvol(r1, r2, r3, hblnkt, blnkith, blnkoth, blnktth, volblkti, &
      volblkto, volblkt)

  end subroutine elliptical_blanket

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine elliptical_shield
    !! Calculate the shield surface area and volume using elliptical scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the shield surface area and volume using elliptical scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: rsldi, shldith, rsldo, shldoth, shareaib, &
      shareaob, sharea, shldtth
    use fwbs_variables, only: volshld
    use physics_variables, only: rmajor, rminor, triang
    use maths_library, only: eshellarea, eshellvol

    implicit none

    ! Major radius to centre of inboard and outboard ellipses (m)
    real(8) :: r1

    ! Distance between r1 and outer edge of inboard shield (m)
    real(8) :: r2

    ! Distance between r1 and inner edge of outboard shield (m)
    real(8) :: r3

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Major radius to centre of inboard and outboard ellipses (m)
    ! (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang

    ! Distance between r1 and outer edge of inboard shield (m)
    r2 = r1 - (rsldi + shldith)

    ! Distance between r1 and inner edge of outboard shield (m)
    r3 = (rsldo - shldoth) - r1

    ! Calculate shield surface area, assuming 100% coverage
    call eshellarea(r1, r2, r3, hshld, shareaib, shareaob, sharea)

    ! Calculate shield volumes, assuming 100% coverage
    call eshellvol(r1, r2, r3, hshld, shldith, shldoth, shldtth, volshldi, &
      volshldo, volshld)

  end subroutine elliptical_shield

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine elliptical_vv
    !! Calculate the vacuum vessel volume using elliptical scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the vacuum vessel volume using elliptical scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: rsldi, rsldo, d_vv_in, d_vv_out, &
                               d_vv_top, d_vv_bot
    use fwbs_variables, only: vdewin, fvoldw
    use physics_variables, only: rmajor, rminor, triang
    use maths_library, only: eshellarea, eshellvol

    implicit none

    ! Major radius to centre of inboard and outboard ellipses (m)
    real(8) :: r1

    ! Distance between r1 and outer edge of inboard section (m)
    real(8) :: r2

    ! Distance between r1 and inner edge of outboard section (m)
    real(8) :: r3

    ! Unused output from eshellvol
    real(8) :: v1, v2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Major radius to centre of inboard and outboard ellipses (m)
    ! (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang

    ! Distance between r1 and outer edge of inboard section (m)
    r2 = r1 - rsldi

    ! Distance between r1 and inner edge of outboard section (m)
    r3 = rsldo - r1

    ! Calculate volume, assuming 100% coverage
    call eshellvol(r1, r2, r3, hvv, d_vv_in, d_vv_out, &
                  (d_vv_top+d_vv_bot)/2, v1, v2, vdewin)

    ! Apply area coverage factor
    vdewin = fvoldw*vdewin

  end subroutine elliptical_vv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine apply_coverage_factors
    !! Apply coverage factors to volumes
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Apply coverage factors to volumes
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: blareaob, blarea, blareaib, shareaib, shareaob, &
      sharea
    use fwbs_variables, only: fdiv, fhcd, volblkto, volblkti, volblkt, fvolsi, &
      fvolso, volshld
    use physics_variables, only: idivrt

    implicit none

    ! Apply blanket coverage factors
    if (idivrt == 2) then
      ! double null configuration
      blareaob = blarea*(1.0D0-2.0D0*fdiv-fhcd) - blareaib
    else
      ! single null configuration
      blareaob = blarea*(1.0D0-fdiv-fhcd) - blareaib
    end if
    
    blarea = blareaib + blareaob

    volblkto = volblkt*(1.0D0-fdiv-fhcd) - volblkti
    volblkt = volblkti + volblkto

    ! Apply shield coverage factors
    shareaib = fvolsi*shareaib
    shareaob = fvolso*shareaob
    sharea = shareaib + shareaob

    volshldi = fvolsi*volshldi
    volshldo = fvolso*volshldo
    volshld = volshldi + volshldo

  end subroutine apply_coverage_factors

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine external_cryo_geometry
    !! Calculate cryostat geometry
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate cryostat geometry
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: clhsf, hmax, tfcth, ddwex
    use buildings_variables, only: clh1
    use fwbs_variables, only: rdewex, rpf2dewar, zdewex, vdewex, vdewin, vvmass, &
      denstl, dewmkg
    use pfcoil_variables, only: rb, zh

    implicit none

    ! cryostat radius (m)
    ! ISSUE #508 Remove RFP option
    ! rb(i) = outer radius of PF coil i (tokamaks)
    rdewex = maxval(rb) + rpf2dewar

    ! Clearance between uppermost PF coil and cryostat lid (m).
    ! Scaling from ITER by M. Kovari
    hcryopf = clhsf * (2.0D0*rdewex)/28.440D0

    ! Half-height of cryostat (m)
    ! ISSUE #508 Remove RFP option
    zdewex = maxval(zh) + hcryopf


    ! Vertical clearance between TF coil and cryostat (m)
    clh1 = zdewex - (hmax + tfcth)

    ! cryostat volume (m3)
    vdewex = ( (2.0D0*pi*rdewex) * 2.0D0*zdewex + (2.0D0*pi*rdewex**2) ) * ddwex

    ! Vacuum vessel mass (kg)
    vvmass = vdewin * denstl

    ! Sum of internal vacuum vessel and cryostat masses (kg)
    dewmkg = (vdewin + vdewex) * denstl

  end subroutine external_cryo_geometry

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_heating_magnets
    !! Nuclear heating in the magnets for CCFE HCPB model
    !! author: Michael Kovari, CCFE, Culham Science Centre
    !! author: J. Morris, CCFE, Culham Science Centre
    !! This subroutine calculates the nuclear heating in the
    !! coils.
    !! PROCESS Engineering paper (M. Kovari et al.)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: fwith, d_vv_in, d_vv_out, d_vv_top, d_vv_bot, &
      fwoth, blnkith, blnkoth, shldith, shldoth
    use fwbs_variables, only: afw, pitch, denstl, whtblkt, volblkt, whtshld, &
      volshld, vvmass, vdewin, fw_armour_thickness, ptfnuc
    use physics_variables, only: powfmw, itart
    use process_output, only: oheadr, ovarre
    use tfcoil_variables, only: whttf
    use global_variables, only: verbose

    implicit none

    ! Model factors and coefficients
    real(8) :: a ! Exponential factor (m2/tonne)
    real(8) :: b ! Exponential factor (m2/tonne)
    real(8) :: e ! Pre-factor (1/kg). Corrected see issue #272

    real(8) :: vffwm
    !! mean FW coolant void fraction
    
    real(8) :: th_blanket_av
    !! Average blanket thickness

    real(8) :: th_shield_av
    !! Average blanket thickness


    ! largest vaccum vessel thickness
    real(8) :: d_vv_all

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Model factors and coefficients
    a = 2.830D0     ! Exponential factor (m2/tonne)
    b = 0.583D0     ! Exponential factor (m2/tonne)
    e = 9.062D0     ! Pre-factor (1/kg). Corrected see issue #272

    ! First wall void fractions

    ! inboard FW coolant void fraction
    vffwi = pi*afw**2/(pitch*fwith)

    ! outboard FW coolant void fraction
    vffwo = vffwi

    ! mean FW coolant void fraction
    vffwm = vffwi

    ! Calculate smeared densities of blanket sections
    ! gaseous He coolant in armour, FW & blanket: He mass is neglected
    armour_density = W_density*(1.0D0-vffwm)
    fw_density = denstl*(1.0D0-vffwm)
    blanket_density = whtblkt / volblkt
    shield_density = whtshld / volshld
    ! Picking the largest value for VV thickness
    d_vv_all = d_vv_in
    if (d_vv_out > d_vv_all) then
        d_vv_all = d_vv_out
    end if
    if (d_vv_all>1.0D-6) then
        vv_density = vvmass / vdewin
    else
        vv_density = 0.0D0
    end if

    ! Calculation of average blanket/shield thickness [m]
    if ( itart == 1 ) then
      
      ! There is no inner blanket for TART design [m]
      th_blanket_av = blnkoth

      ! The CP shield in considered in a separate calcualtion [m]
      th_shield_av = shldoth

    else
      ! Average breeding blanket thickness [m]
      th_blanket_av = 0.5D0 * ( blnkoth + blnkith )

      ! Average neutronic shield thickness [m]
      th_shield_av = 0.5D0 * ( shldoth + shldith )
    end if

    ! Exponents (tonne/m2)
    ! Blanket exponent (/1000 for kg -> tonnes)
    x_blanket = (armour_density * fw_armour_thickness &
              + fw_density * (fwith+fwoth)/2.0D0 &
              + blanket_density * th_blanket_av ) / 1000.0D0

    ! Shield exponent(/1000 for kg -> tonnes)
    x_shield = (shield_density * th_shield_av + &
              vv_density * (d_vv_in+d_vv_out)/2.0D0) / 1000.D0

    ! Nuclear heating in TF coil
    ! Unit heating (W/kg/GW of fusion power) x mass (kg)
    tfc_nuc_heating = e*exp(-a*x_blanket)*exp(-b*x_shield) * whttf

    ! Total heating (MW)
    ptfnuc = tfc_nuc_heating * (powfmw / 1000.0D0) / 1.0D6

    ! Output
    if (ip == 0) return
    if (verbose == 1) then
        call oheadr(ofile, 'Nuclear Heating Magnets Before Renormalisation')
        call ovarre(ofile, 'Shield line density (tonne/m2)', '(x_shield)', x_shield)
        call ovarre(ofile, 'Blanket line density (tonne/m2)', '(x_blanket)', x_blanket)
        call ovarre(ofile, 'Unit nuclear heating in TF coil (W/GW)', &
          '(tfc_nuc_heating)', tfc_nuc_heating)
        call ovarre(ofile, 'Total nuclear heating in TF coil (MW)', '(ptfnuc)', ptfnuc)
        call ovarre(ofile, 'powfmw', '(powfmw.)', powfmw)
        call ovarre(ofile, 'total mass of the TF coils (kg)', '(whttf)', whttf)
    end if

  end subroutine nuclear_heating_magnets

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_heating_fw
    !! Nuclear heating in the FW for CCFE HCPB model
    !! author: J. Morris, CCFE, Culham Science Centre
    !! This subroutine calculates the nuclear heating in the FW
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fwbs_variables, only: pnucfw, fwmass
    use physics_variables, only: powfmw

    implicit none

    ! Unit heating of FW and armour (W/kg per W of fusion power)
    fw_armour_u_nuc_heating = 6.25D-7

    ! Total nuclear heating in FW (MW)
    pnucfw = fwmass * fw_armour_u_nuc_heating * powfmw

    ! Errors handling (should be a lvl 3 error no ?)
    if ((pnucfw<0.0d0).or.(pnucfw /= pnucfw)) then
        write(*,*)'Error in nuclear_heating_fw.  pnucfw = ', pnucfw, 'powfmw = ',&
          powfmw, 'fwmass = ', fwmass
        stop 1
    end if

  end subroutine nuclear_heating_fw

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_heating_blanket
    !! Nuclear heating in the blanket for CCFE HCPB model
    !! author: J. Morris, CCFE, Culham Science Centre
    !! This subroutine calculates the nuclear heating in the blanket
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fwbs_variables, only: whtblkt, pnucblkt
    use physics_variables, only: powfmw

    implicit none

    ! Blanket nuclear heating coefficient
    real(8) :: a

    ! Blanket nuclear heating exponent (1/tonne)
    real(8) :: b

    ! Mass of the blanket (tonnes)
    real(8) :: mass

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Blanket nuclear heating coefficient and exponent
    a = 0.764D0
    b = 2.476D-3  ! 1/tonne

    ! Mass of the blanket in tonnes
    mass = whtblkt / 1000.0D0

    ! Total blanket nuclear heating (MW)
    exp_blanket = 1-exp(-b*mass)
    pnucblkt = powfmw * a * exp_blanket

    ! error handling (should ba a lvl 3 error no?)
    if ((pnucblkt<1.0d0).or.(pnucblkt /= pnucblkt)) then
        write(*,*)'Error in nuclear_heating_blanket. '
        write(*,*)'pnucblkt =', pnucblkt, ' exp_blanket =', exp_blanket
        write(*,*)'powfmw =', powfmw, ' mass =', mass
        stop 1
    end if

  end subroutine nuclear_heating_blanket

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_heating_shield
    !! Nuclear heating in the shield for CCFE HCPB model
    !! author: J. Morris, CCFE, Culham Science Centre
    !! This subroutine calculates the nuclear heating in the shield
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: shldith, shldoth
    use fwbs_variables, only: whtshld, pnucshld
    use physics_variables, only: powfmw, itart

    implicit none

    ! Local variables

    real(8) :: f
    !! Shield nuclear heating coefficient (W/kg/W)

    real(8) :: g, h
    !! Shield nuclear heating exponent m2/tonne
    
    real(8) :: y
    !! Decay "length" (kg/m2)
    
    real(8) :: th_shield_av
    !! Neutron shield average thickness [m]
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Shield nuclear heating coefficients and exponents
    f = 6.88D2  ! W/kg/W of fusion power
    g = 2.723D0  ! m2/tonne
    h = 0.798D0  ! m2/tonne

    ! Calculation of average blanket/shield thickness [m]
    if ( itart == 1 ) then
      ! The CP shield in considered in a separate calcualtion
      th_shield_av = shldoth
    else 
      ! Average neutronic shield thickness [m]
      th_shield_av = 0.5D0 * ( shldoth + shldith )
    end if      

    ! Decay length [m-2]
    y = (shield_density/1000.D0) * th_shield_av

    ! Unit nuclear heating of shield (W/kg/GW of fusion power) x mass
    exp_shield1 = exp(-g * x_blanket)
    exp_shield2 = exp(-h * y)
    shld_u_nuc_heating = whtshld * f * exp_shield1 * exp_shield2

    ! Total nuclear heating in shield (MW)
    pnucshld = shld_u_nuc_heating * (powfmw / 1000.D0) / 1.0D6

  end subroutine nuclear_heating_shield

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_heating_divertor
    !! Nuclear heating in the divertor for CCFE HCPB model
    !! author: J. Morris, CCFE, Culham Science Centre
    !! This subroutine calculates the nuclear heating in the divertor
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fwbs_variables, only: fdiv, pnucdiv, pnuchcd
    use physics_variables, only: idivrt, powfmw

    implicit none

    ! Unfortunately the divertor heating was not tallied in the neutronics calcs
    ! Assume that all the neutron energy + energy multiplication is absorbed in the reactor +
    ! coils. It turns out that emult is also approx constant, but this is not used. No energy
    ! multiplication in the divertor

    ! Overwrite global variable for fdiv 07/11/18 SIM: Removed having spoken to JM
    ! fdiv = 0.115D0

    ! Nuclear heating in the divertor just the neutron power times fdiv
    if (idivrt == 2) then
      ! Double null configuration
      pnucdiv = 0.8D0 * powfmw * 2.0D0*fdiv
    else
      ! single null configuration
      pnucdiv = 0.8D0 * powfmw * fdiv
    end if 
    ! No heating of the H & CD
    pnuchcd = 0.0D0

  end subroutine nuclear_heating_divertor

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine powerflow_calc
    !! Calculations for powerflow
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculations for powerflow
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwareaob, fwarea
    use current_drive_variables, only: porbitlossmw
    use fwbs_variables, only: fdiv, praddiv, pradhcd, fhcd, pradfw, coolwh, &
      outlet_temp, blpressure, primary_pumping, pnucfw, pnucblkt, pnucdiv, &
      pnucshld, etaiso, pnuc_cp_sh
    use heat_transport_variables, only: htpmw_fw, fpumpfw, htpmw_blkt, fpumpblkt, &
      htpmw_shld, fpumpshld, htpmw_div, fpumpdiv
    use physics_variables, only: idivrt, pradmw, palpfwmw, pdivt
    use process_output, only: oheadr, ovarre
    use primary_pumping_variables, only: p_he, dp_he, gamma_he, t_in_bb, t_out_bb, &
      htpmw_fw_blkt
    use refprop_interface, only: tsat_refprop

    implicit none
    real(8):: t_in_compressor, dt_he, fpump, pfactor, p_plasma

    ! Radiation power incident on divertor (MW)
    if (idivrt == 2) then
      ! Double null configuration
      praddiv = pradmw * 2.0D0 *fdiv
    else
      ! single null configuration
      praddiv = pradmw * fdiv
    end if 

    ! Radiation power incident on HCD apparatus (MW)
    pradhcd = pradmw * fhcd

    ! Radiation power incident on first wall (MW)
    pradfw = pradmw - praddiv - pradhcd

    ! If we have chosen pressurised water as the blanket coolant, set the
    ! coolant outlet temperature as 20 deg C below the boiling point
    if (coolwh == 2) then
      outlet_temp = tsat_refprop(blpressure*1.0D6, coolwh) - 20.0D0  !  in K
    end if

    ! Surface heat flux on first wall (outboard and inboard) (MW)
    ! All of the fast particle losses go to the outer wall.
    psurffwo = pradfw * fwareaob/fwarea + porbitlossmw + palpfwmw
    psurffwi = pradfw * (1.0D0 - fwareaob/fwarea)

    if (primary_pumping == 0) then

      ! User sets mechanical pumping power directly (primary_pumping_power)
      ! Values of htpmw_blkt, htpmw_div, htpmw_fw, htpmw_shld set in input file

    else if ( primary_pumping == 1 ) then
      ! User sets mechanical pumping power as a fraction of thermal power 
      ! removed by coolant
      htpmw_fw = fpumpfw * (pnucfw + psurffwi + psurffwo)
      htpmw_blkt = fpumpblkt * pnucblkt
      htpmw_shld = fpumpshld * ( pnucshld + pnuc_cp_sh )
      htpmw_div = fpumpdiv * (pdivt + pnucdiv + praddiv)

    else if (primary_pumping == 2) then
      ! Mechanical pumping power is calculated for first wall and blanket
      call thermo_hydraulic_model
      ! For divertor and shield, mechanical pumping power is a fraction of thermal 
      ! power removed by coolant
      htpmw_shld = fpumpshld * ( pnucshld + pnuc_cp_sh )
      htpmw_div = fpumpdiv * (pdivt + pnucdiv + praddiv)

    else if (primary_pumping == 3) then
      ! Issue #503
      ! Mechanical pumping power is calculated using specified pressure drop for 
      ! first wall and blanket circuit, including heat exchanger and pipes
      pfactor = (p_he/(p_he-dp_he))**((gamma_he-1.0d0)/gamma_he)
      t_in_compressor = t_in_bb / pfactor
      dt_he = t_out_bb - t_in_bb
      fpump = t_in_compressor/(etaiso*dt_he) * (pfactor - 1.0d0)
      p_plasma = pnucfw + psurffwi + psurffwo + pnucblkt
      htpmw_fw_blkt = fpump/(1-fpump) * p_plasma

      ! For divertor and shield, mechanical pumping power is a fraction of thermal 
      ! power removed by coolant
      htpmw_shld = fpumpshld * ( pnucshld + pnuc_cp_sh )
      htpmw_div = fpumpdiv * (pdivt + pnucdiv + praddiv)
      if (ip /= 0) then
        call oheadr(ofile, 'Pumping for primary coolant (helium)')
        call ovarre(ofile, 'Pressure drop in FW and blanket coolant incl. hx and &
          &pipes (Pa)', '(dp_he)', dp_he)
        call ovarre(ofile, 'Fraction of FW and blanket thermal power required for pumping', &
          '(fpump)', fpump, 'OP ')
        call ovarre(ofile, 'Total power absorbed by FW & blanket (MW)', &
          '(p_plasma)', p_plasma, 'OP ')
        call ovarre(ofile, 'Inlet temperature of FW & blanket coolant pump (K)', &
          '(t_in_compressor)', t_in_compressor, 'OP ')
        call ovarre(ofile, 'Outlet temperature of FW & blanket coolant pump (K)', &
          '(t_in_bb)', t_in_bb)
        call ovarre(ofile, 'Mechanical pumping power for FW and blanket cooling loop &
          &including heat exchanger (MW)', '(htpmw_fw_blkt)', htpmw_fw_blkt, 'OP ')
        call ovarre(ofile, 'Mechanical pumping power for divertor (MW)', &
          '(htpmw_div)', htpmw_div, 'OP ')
        call ovarre(ofile, 'Mechanical pumping power for shield and vacuum vessel (MW)', &
          '(htpmw_shld)', htpmw_shld, 'OP ')
      endif
   end if

  end subroutine powerflow_calc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine thermo_hydraulic_model
    !! Thermo-hydraulic model for first wall and blanket
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculations for detailed powerflow model secondary_cycle > 0
    ! ONLY CALLED if primary_pumping = 2
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: blnkith, blnkoth, scrapli, scraplo, fwarea, &
      fwareaib, fwareaob, fwith, fwoth
    use fwbs_variables, only: nblktmodti, nblktmodto, afw, fwoutlet, fwinlet, &
      fw_channel_length, pitch, iblnkith, pnucblkt, volblkti, volblkt, &
      outlet_temp, inlet_temp, vfblkt, fwcoolant, fwpressure, etaiso, coolwh, &
      blpressure, volblkto, tpeak, fw_wall, roughness, tfwmatmax, pnucfw
    use heat_transport_variables, only: htpmw_fw, htpmw_blkt, htpmw_div, &
      htpmw_shld, htpmw
    use physics_variables, only: rmajor, rminor
    use process_output, only: oheadr, ovarin, ovarre, ovarst, ovarrf
    use fw_module, only: fw_temp

    implicit none

    ! coolant specific heat capacity at constant pressure (J/kg/K)
    real(8) :: cf

    ! coolant density (kg/m3)
    real(8) :: rhof

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

    ! Number of angle turns in FW and blanket flow channels, consistent with flow lengths defined
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

    if (iblnkith==1) then
        ! Neutron power deposited in inboard blanket (MW)
        pnucblkti = pnucblkt * volblkti / volblkt

        ! Mass flow rate for inboard blanket coolant (kg/s)
        mfblkti = 1.0D6*(pnucblkti) / (cf*(outlet_temp-inlet_temp))

        ! Calc total num of pipes (in all inboard modules) from coolant frac 
        ! and channel dimensions
        ! Assumes up/down flow, two 90 deg bends per length
        npblkti = (vfblkt * volblkti) / (pi * afw * afw * bzfllengi)

        ! Mass flow rate per coolant pipe (kg/s)
        if ((npblkti<1.0d1).or.(npblkti/=npblkti).or.(mfblkti<1.0d0).or.(mfblkti/=mfblkti)) then
            write(*,*) 'npblkti = ', npblkti, '   vfblkt =', vfblkt
            write(*,*) 'mfblkti =',mfblkti,   'pnucblkti =', pnucblkti
            write(*,*) 'pnucblkt =',pnucblkt,   'volblkti =', volblkti
            stop 1
        end if

        mfblktpi = mfblkti / npblkti

        ! Coolant velocity in blanket (m/s)
        velblkti = mfblktpi / (pi * afw * afw * rhof)

    end if

    ! Pumping powers for blanket and first wall (MW)
    if (fwcoolant == 'helium') coolant=1
    if (fwcoolant == 'water') coolant=2
    htpmw_fwi = pumppower(fwinlet, fwoutlet, fwpressure, fw_channel_length, afw, mffwi, &
        mffwpi, no90fw, no180fw, etaiso, coolant, 'Inboard first wall')
    if (iblnkith==1) then
        htpmw_blkti = pumppower(inlet_temp, outlet_temp,blpressure, bzfllengi, afw, mfblkti, &
            mfblktpi, no90bz, no180bz, etaiso, coolwh, 'Inboard blanket')
    end if

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
    call ovarrf(ofile, 'Actual peak temperature of first wall material (K)', &
      '(tpeak)', tpeak, 'OP ')

    call ovarre(ofile, 'Mechanical pumping power for FW (MW)', '(htpmw_fw)', htpmw_fw, 'OP ')
    call ovarre(ofile, 'Mechanical pumping power for blanket (MW)', '(htpmw_blkt)', &
      htpmw_blkt, 'OP ')
    call ovarre(ofile, 'Pumping power for divertor (MW)', '(htpmw_div)', htpmw_div, 'OP ')
    call ovarre(ofile, 'Pumping power for shield and vacuum vessel (MW)', &
      '(htpmw_shld)', htpmw_shld, 'OP ')
    call ovarre(ofile, 'Total primary coolant pumping power (MW)', '(htpmw)', htpmw, 'OP ')

  end subroutine thermo_hydraulic_model

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine component_masses
    !! Calculations for component masses
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculations for component masses
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: fwareaib, fwith, fwareaob, fwoth, fwarea
    use divertor_variables, only: divsur, divclfr, divplt, fdiva, divmas, divdens
    use fwbs_variables, only: volblkt, vfblkt, volshld, vfshld, coolmass, fwclfr, &
      breeder_f, breeder_multiplier, whtbltibe12, whtblli4sio4, whtblbe, wtblli2o, &
      vfcblkt, vfpblkt, whtblss, denstl, whtblkt, whtshld, wpenshld, fwmass, &
      fw_armour_vol, fw_armour_thickness, fw_armour_mass, armour_fw_bl_mass
    use physics_variables, only: rminor, rmajor, idivrt, sarea

    implicit none

    ! Local variables

    ! Coolant volume (m3)
    real(8) :: coolvol

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Start adding components of the coolant mass:
    ! Divertor coolant volume (m3)
    coolvol = divsur * divclfr * divplt

    ! Blanket coolant volume (m3)
    coolvol = coolvol + volblkt*vfblkt

    ! Shield coolant volume (m3)
    coolvol = coolvol + volshld*vfshld

    ! First wall coolant volume (m3)
    coolvol = coolvol + fwareaib*fwith*vffwi + fwareaob*fwoth*vffwo

    ! Mass of He coolant = volume * density at typical coolant temperatures and pressures (kg)
    coolmass = coolvol*1.517D0

    ! Average first wall coolant fraction, only used by old routines in fispact.f90, safety.f90
    fwclfr = (fwareaib*fwith*vffwi + fwareaob*fwoth*vffwo) / (fwarea*0.5D0*(fwith+fwoth))

    ! Component masses
    ! Divertor mass (kg)
    divsur = fdiva * 2.0D0 * pi * rmajor * rminor
    if (idivrt == 2) divsur = divsur * 2.0D0
    divmas = divsur * divdens * (1.0D0 - divclfr) * divplt

    if (breeder_f < 1.0D-10) breeder_f = 1.0D-10
    if (breeder_f > 1.0D0  ) breeder_f = 1.0D0

    ! fbltibe12 = fblli2sio4 * (1 - breeder_f)/breeder_f
    ! New combined variable breeder_multiplier
    ! Lithium orthosilicate fraction:
    fblli2sio4 = breeder_f * breeder_multiplier

    ! Titanium beryllide fraction, and mass (kg):
    fbltibe12  = breeder_multiplier - fblli2sio4
    whtbltibe12 = volblkt * fbltibe12 * 2260.0D0

    ! Blanket Lithium orthosilicate mass (kg)
    ! Ref: www.rockwoodlithium.com...
    whtblli4sio4 = volblkt * fblli2sio4 * 2400.0D0

    ! TODO sort this out so that costs model uses new variables.
    ! #327 For backwards compatibility, set the old blanket masses the same:
    whtblbe = whtbltibe12
    wtblli2o = whtblli4sio4

    ! Steel fraction by volume is the remainder:
    fblss_ccfe = 1.0D0 - fblli2sio4 - fbltibe12 - vfcblkt - vfpblkt

    ! Steel mass (kg)
    whtblss = volblkt * fblss_ccfe * denstl

    ! Total blanket mass (kg)
    whtblkt = whtbltibe12 + whtblli4sio4 + whtblss

    ! Shield mass (kg)
    whtshld = volshld * denstl * (1.0D0 - vfshld)

    ! Penetration shield mass (set = internal shield) (kg)
    wpenshld = whtshld

    ! First wall volume (m^3)
    volfw = (fwareaib*fwith*(1.0D0-vffwi) + fwareaob*fwoth*(1.0D0-vffwo))

    ! First wall mass, excluding armour (kg)
    fwmass = denstl * volfw

    ! First wall armour volume (m^3)
    fw_armour_vol = sarea*fw_armour_thickness

    ! First wall armour mass (kg)
    fw_armour_mass = fw_armour_vol*W_density

    ! Total mass of first wall and blanket
    armour_fw_bl_mass = fw_armour_mass + fwmass + whtblkt

  end subroutine component_masses

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blanket_mod_pol_height
    !! Calculations for blanket module poloidal height
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculations for blanket module poloidal height for D shaped and elliptical machines
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: scrapli, scraplo
    use fwbs_variables, only: fwbsshape, nblktmodpi, fdiv, nblktmodpo
    use physics_variables, only: itart, rminor, idivrt, rmajor, triang

    implicit none

    ! Local variables

    ! Mid-plane distance from inboard to outboard side (m)
    real(8) :: a

    ! Internal half-height of blanket (m)
    real(8) :: b

    ! Calculate ellipse circumference using Ramanujan approximation (m)
    real(8) :: ptor

    ! Major radius where half-ellipses 'meet' (m)
    real(8) :: r1

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
      if (idivrt == 2) then
        ! Double null configuration
        bllengo = 0.5D0*ptor * (1.0D0 - 2.0D0*fdiv) / nblktmodpo
      else
        ! single null configuration
        bllengo = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpo
      end if 

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
      if (idivrt == 2) then
        ! Double null configuration
        bllengi = 0.5D0*ptor * (1.0D0 - 2.0D0*fdiv) / nblktmodpi
      else
        ! single null configuration
        bllengi = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpi
      end if

      ! Distance between r1 and inner edge of outboard first wall / blanket (m)
      a = rmajor + rminor + scraplo - r1

      ! Calculate ellipse circumference using Ramanujan approximation (m)
      ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

      ! Calculate outboard blanket poloidal length and segment, subtracting divertor length (m)
      if (idivrt == 2) then
        ! Double null configuration
        bllengo = 0.5D0*ptor * (1.0D0 - 2.0D0*fdiv) / nblktmodpo
      else
        ! single null configuration
        bllengo = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpo
      end if

    end if

  end subroutine blanket_mod_pol_height

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine write_ccfe_hcpb_output
    !! Write output to file for CCFE HCPB model
    !! author: J. Morris, CCFE, Culham Science Centre
    !! This subroutine outputs the CCFE HCPB model results to
    !! an output file
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwarea
    use buildings_variables, only: clh1
    use cost_variables, only: abktflnc
    use divertor_variables, only: divsur, divmas
    use fwbs_variables, only: vfcblkt, vfpblkt, fw_armour_vol, volblkt, volshld, &
      vdewin, fw_armour_mass, fwmass, whtblkt, whtbltibe12, whtblli4sio4, whtblss, &
      armour_fw_bl_mass, whtshld, vvmass, pnuc_cp, pnuc_cp_tf, pnuc_cp_sh, pnucfw, &
      ptfnuc, pnucblkt, pnucshld, pnucdiv, secondary_cycle, fwpressure, blpressure, &
      primary_pumping, nblktmodpi, nblktmodti, nblktmodpo, nblktmodto, etaiso, &
      rdewex, zdewex, neut_flux_cp, fdiv
    use heat_transport_variables, only: htpmw_fw, htpmw_blkt, htpmw_div, &
      htpmw_shld, htpmw
    use tfcoil_variables, only: i_tf_sup
    use physics_variables, only: itart
    use process_output, only: oheadr, osubhd, ovarrf, ovarre, ocmmnt, ovarin

    implicit none

    call oheadr(ofile, 'First wall and blanket : CCFE HCPB model')
    call osubhd(ofile, 'Blanket Composition by volume :')

    call ovarrf(ofile, 'Titanium beryllide fraction', '(fbltibe12)', fbltibe12, 'OP ')
    call ovarrf(ofile, 'Lithium orthosilicate fraction', '(fblli2sio4)', fblli2sio4, 'OP ')
    call ovarrf(ofile, 'Steel fraction', '(fblss_ccfe)', fblss_ccfe, 'OP ')
    call ovarrf(ofile, 'Coolant fraction', '(vfcblkt)', vfcblkt)
    call ovarrf(ofile, 'Purge gas fraction', '(vfpblkt)', vfpblkt)

    call osubhd(ofile, 'Component Volumes :')

    call ovarrf(ofile, 'First Wall Armour Volume (m3)', '(fw_armour_vol)', &
      fw_armour_vol, 'OP ')
    call ovarrf(ofile, 'First Wall Volume (m3)', '(volfw)', volfw, 'OP ')
    call ovarrf(ofile, 'Blanket Volume (m3)', '(volblkt)', volblkt, 'OP ')
    call ovarrf(ofile, 'Shield Volume (m3)', '(volshld)', volshld, 'OP ')
    call ovarrf(ofile, 'Vacuum vessel volume (m3)', '(vdewin)', vdewin, 'OP ')

    call osubhd(ofile, 'Component Masses :')

    call ovarre(ofile, 'First Wall Armour Mass (kg)', '(fw_armour_mass)', fw_armour_mass, 'OP ')
    call ovarre(ofile, 'First Wall Mass, excluding armour (kg)', '(fwmass)', fwmass, 'OP ')
    call ovarre(ofile, 'Blanket Mass - Total(kg)', '(whtblkt)', whtblkt, 'OP ')
    call ovarre(ofile, '    Blanket Mass - TiBe12 (kg)', '(whtbltibe12)', whtbltibe12, 'OP ')
    call ovarre(ofile, '    Blanket Mass - Li2SiO4 (kg)', '(whtblli4sio4)', whtblli4sio4, 'OP ')
    call ovarre(ofile, '    Blanket Mass - Steel (kg)', '(whtblss)', whtblss, 'OP ')
    call ovarre(ofile, 'Total mass of armour, first wall and blanket (kg)', &
      '(armour_fw_bl_mass)', armour_fw_bl_mass, 'OP ')
    call ovarre(ofile, 'Shield Mass (kg)', '(whtshld)', whtshld, 'OP ')
    call ovarre(ofile, 'Vacuum vessel mass (kg)', '(vvmass)', vvmass, 'OP ')

    !  Nuclear heating section
    call osubhd(ofile, 'Nuclear heating :')

    !  ST centre post
    if (itart == 1) then
       
      if ( i_tf_sup == 0 ) then
        call osubhd(ofile,'(Copper resistive centrepost used)')
      else if ( i_tf_sup == 1 ) then
        call osubhd(ofile,'(Superdonducting magnet centrepost used)')
        call ovarre(ofile,'ST centrepost TF fast neutron fllux (E > 0.1 MeV) (m^(-2).s^(-1))',&
                    '(neut_flux_cp)',neut_flux_cp, 'OP ')
      else if ( i_tf_sup == 2 ) then
        call osubhd(ofile,'(Aluminium magnet centrepost used)')
      end if

      call ovarre(ofile,'ST centrepost TF heating (MW)','(pnuc_cp_tf)',pnuc_cp_tf, 'OP ')
      call ovarre(ofile,'ST centrepost shield heating (MW)','(pnuc_cp_sh)',pnuc_cp_sh, 'OP ')
      call ovarre(ofile,'ST centrepost total heating (MW)','(pnuc_cp)',pnuc_cp, 'OP ')
  
    end if

    call ovarre(ofile, 'Total nuclear heating in TF+PF coils (CS is negligible) (MW)', &
      '(ptfnuc)', ptfnuc, 'OP ')
    call ovarre(ofile, 'Total nuclear heating in FW (MW)', '(pnucfw)', pnucfw, 'OP ')
    call ovarre(ofile, 'Total nuclear heating in the blanket (including emult) (MW)', &
      '(pnucblkt)', pnucblkt, 'OP ')
    call ocmmnt(ofile, '(Note: emult is fixed for this model inside the code)')
    call ovarre(ofile, 'Total nuclear heating in the shield (MW)', '(pnucshld)', pnucshld, 'OP ')
    call ovarre(ofile, 'Total nuclear heating in the divertor (MW)', '(pnucdiv)', pnucdiv, 'OP ')
    call osubhd(ofile,' Diagostic output for nuclear heating :')
    call ovarre(ofile, 'Blanket exponential factor', '(exp_blanket)', exp_blanket, 'OP ')
    call ovarre(ofile, 'Shield: first exponential', '(exp_shield1)', exp_shield1, 'OP ')
    call ovarre(ofile, 'Shield: second exponential', '(exp_shield2)', exp_shield2, 'OP ')
    call ovarre(ofile, 'Solid angle fraction taken by on divertor', '(fdiv)', fdiv)

    call ovarin(ofile, 'Switch for plant secondary cycle ', '(secondary_cycle)', secondary_cycle)
    call ovarre(ofile, 'First wall coolant pressure (Pa)', '(fwpressure)', fwpressure)
    call ovarre(ofile, 'Blanket coolant pressure (Pa)', '(blpressure)', blpressure)

    if(primary_pumping/=3)then
        call ovarre(ofile, 'Mechanical pumping power for first wall (MW)', &
          '(htpmw_fw)', htpmw_fw, 'OP ')
        call ovarre(ofile, 'Mechanical pumping power for blanket (MW)', &
          '(htpmw_blkt)', htpmw_blkt, 'OP ')
        call ovarre(ofile, 'Mechanical pumping power for divertor (MW)', &
          '(htpmw_div)', htpmw_div, 'OP ')
        call ovarre(ofile, 'Mechanical pumping power for shield and vacuum vessel (MW)', &
          '(htpmw_shld)', htpmw_shld, 'OP ')
        call ovarre(ofile, 'Total electrical coolant pumping power: first wall, &
          &blanket, shield and divertor (MW)', '(htpmw)', htpmw, 'OP ')
    end if

    call ovarre(ofile, 'Allowable nominal neutron fluence at first wall (MW.year/m2)', &
      '(abktflnc)', abktflnc)
    call ovarin(ofile, 'No of inboard blanket modules poloidally', '(nblktmodpi)', nblktmodpi)
    call ovarin(ofile, 'No of inboard blanket modules toroidally', '(nblktmodti)', nblktmodti)
    call ovarin(ofile, 'No of outboard blanket modules poloidally', '(nblktmodpo)', nblktmodpo)
    call ovarin(ofile,'No of outboard blanket modules toroidally', '(nblktmodto)', nblktmodto)
    call ovarre(ofile, 'Isentropic efficiency of first wall / blanket coolant pumps', &
      '(etaiso)', etaiso)

    call osubhd(ofile, 'Other volumes, masses and areas :')
    call ovarre(ofile, 'First wall area (m2)', '(fwarea)', fwarea, 'OP ')
    call ovarre(ofile, 'Cryostat internal radius (m)', '(rdewex)', rdewex, 'OP ')
    call ovarre(ofile, 'Cryostat internal half-height (m)', '(zdewex)', zdewex, 'OP ')
    call ovarre(ofile, 'Vertical clearance from TF coil to cryostat (m)', '(clh1)', clh1, 'OP ')
    call ovarre(ofile, 'Divertor area (m2)', '(divsur)', divsur, 'OP ')
    call ovarre(ofile, 'Divertor mass (kg)', '(divmas)', divmas, 'OP ')

  end subroutine write_ccfe_hcpb_output

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine st_centrepost_nuclear_heating( pneut, sh_width,                & ! Inputs
                                            pnuc_cp_tf, pnuc_cp_sh, pnuc_cp ) ! Outputs 

    !! Author : P J Knight, CCFE, Culham Science Centre
    !! Author : S Kahn, CCFE, Culham Science Centre
    !! Estimates the nuclear power absorbed by the ST centrepost magnet
    !! This routine calculates the neutron power absorbed by a
    !! copper spherical tokamak centrepost.
    !! The calculation estimates the fraction of neutrons hitting
    !! the centrepost from a point source at the plasma centre,
    !! and assumes an average path length of 2*r_cp_outer, and an
    !! e-folding decay length of 0.08m (copper-water mixture).
    !! J D Galambos, STAR Code : Spherical Tokamak Analysis and Reactor Code,
    !! unpublished internal Oak Ridge document
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: tfcth
    use fwbs_variables, only: f_neut_shield
    use physics_variables, only: rmajor
    use tfcoil_variables, only: i_tf_sup
    use constants, only: pi

    implicit none

    ! Inputs
    ! ------
    real(8), intent(in) :: pneut
    !! 14 MeV plasma neutron power generated by the plasma [MW]
  
    real(8), intent(in) :: sh_width
    !! Thickeness of the centrepost neutron shield [m]
    ! ------


    ! Outputs
    ! -------

    real(8), intent(out) :: pnuc_cp_tf
    !! Nuclear nuclear heat deposited in the centrepost TF coil [MW]

    real(8), intent(out) :: pnuc_cp_sh
    !! Nuclear nuclear heat deposited in the centrepost shield [MW]

    real(8), intent(out) :: pnuc_cp
    !! Total nuclear heat deposited in the centrepost shield [MW]
    ! -------


    ! Local variables
    ! ---------------
    real(8) :: pnuc_cp_wp_gam
    !! Nuclear power deposited in the CP winding pack by gammas [MW]

    real(8) :: pnuc_cp_wp_n
    !! Nuclear power deposited in the CP winding pack by neutrons [MW]

    real(8) :: pnuc_cp_case_gam
    !! Nuclear power deposited in the CP steel case by gammas [MW]

    real(8) :: pnuc_cp_case_n
    !! Nuclear power deposited in the CP steel case by neutrons [MW]
    
    real(8) :: pnuc_cp_sh_gam
    !! Nuclear power deposited in the CP steel case by gammas [MW]

    real(8) :: pnuc_cp_sh_n
    !! Nuclear power deposited in the CP steel case by neutrons [MW]

    real(8) :: f_pnuc_cp_tf = 1.0D0
    !! Outer wall reflection TF nuclear heating enhancement factor [-] 

    real(8) :: f_pnuc_cp_sh = 1.7D0
    !! Outer wall reflection shield nuclear heating enhancement factor [-]

    real(8) :: f_wc_density = 2.0D0
    !! Tungsten density may vary with different manufacturing processes.
    !! Here is a factor to take this into account

    real(8) :: f_steel_struct = 0.1
    !! Fraction of steel structures

    real(8) :: sh_width_eff
    !! Effecting shield width, removing steel structures  
    ! ---------------
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! Former nuclear heating calculations for Copper magnets
    ! Commented out as no nuclear shielding was included 
    ! ---
    ! ! Fraction of the nuclear power absorbed by the copper centrepost 
    ! ! (0.08 m e-folding decay length)
    ! f_neut_absorb_cp = 1.0D0 - exp( -2.0D0*tfcth / 0.08D0) 
    ! 
    ! ! Nuclear power
    ! pnuc_cp = pneut * f_geom_cp * f_neut_absorb_cp
    ! ---

    ! Steel support structure effective WC shield thickness reduction
    sh_width_eff = sh_width / ( 1.0D0 - f_steel_struct ) 

    ! Aluminium CP
    ! ------------
    ! From Pfus = 1 GW ST MCNP neutronic calculations assuming
    ! Tungsten carbyde with 13% water cooling fraction
    if ( i_tf_sup == 2 ) then
      pnuc_cp_tf = ( pneut / 800.0D0 ) * exp( 3.882D0 - 16.69D0*sh_width_eff )

      ! WARINING, this is an extraoilation from TF heat ...
      ! DO NOT TRUST THIS VALUE !!
      pnuc_cp_sh = ( pneut / 800.0D0 ) * exp(3.882D0) - pnuc_cp_tf
    ! ------------


    ! Superconducting / copper CP
    ! ---------------------------
    ! MCNP calculations made with a TF magnet model with very large WP
    ! so the TF is mostly copper, making the calculation also valid for 
    ! Copper TF centrepost 
    else 
      
      ! Nuclear powers fits for a 800 MW plasma neutron source
      ! ***
      ! Nuclear power deposited in the CP winding pack by gammas [MW]
      pnuc_cp_wp_gam = 16.3D0 * exp( -14.63D0 * sh_width_eff ) + 143.08D0 &
                     * sh_width_eff * ( sh_width / rmajor ) * exp( -21.747D0 * sh_width_eff )
  
      ! Nuclear power deposited in the CP winding pack by neutrons [MW]
      pnuc_cp_wp_n = 1.403D0  * exp( -16.535D0 * sh_width_eff ) + 3.812D0 &
                   * sh_width_eff * ( sh_width / rmajor ) * exp( -23.631D0 * sh_width_eff )
      
      ! Nuclear power deposited in the CP steel case by gammas [MW]
      pnuc_cp_case_gam = 1.802D0 * exp( -13.993D0 * sh_width_eff ) + 38.592D0 &
                       * sh_width * ( sh_width_eff / rmajor ) * exp( -27.051D0 * sh_width_eff )
  
      ! Nuclear power deposited in the CP steel case by neutrons [MW]
      pnuc_cp_case_n = 0.158D0 * exp( -55.046D0 * sh_width_eff ) + 2.0742D0 &
                     * sh_width_eff * ( sh_width / rmajor ) * exp( -24.401D0 * sh_width_eff )
             
      ! Nuclear power density deposited in the tungsten carbyde shield by photons [MW]
      pnuc_cp_sh_gam = sh_width_eff * ( 596.00D0 * exp( -4.130D0 * sh_width_eff ) &
                                      + 90.586D0 * exp( 0.6837D0 * sh_width_eff ) )
      
      ! Nuclear power density deposited in the tungsten carbyde shield by neutrons [MW]
      pnuc_cp_sh_n = sh_width_eff * ( 202.10D0 * exp( -10.533D0 * sh_width_eff ) &
                                    + 80.510D0 * exp( -0.9801D0 * sh_width_eff ) )
      ! ***


      ! Fit generalisation
      ! ***
      ! Correction for the actual 14 MeV plasma neutron power
      pnuc_cp_wp_gam =   ( pneut / 800.0D0 ) * pnuc_cp_wp_gam   
      pnuc_cp_wp_n =     ( pneut / 800.0D0 ) * pnuc_cp_wp_n    
      pnuc_cp_case_gam = ( pneut / 800.0D0 ) * pnuc_cp_case_gam
      pnuc_cp_case_n =   ( pneut / 800.0D0 ) * pnuc_cp_case_n  
      pnuc_cp_sh_gam =   ( pneut / 800.0D0 ) * pnuc_cp_sh_gam  
      pnuc_cp_sh_n =     ( pneut / 800.0D0 ) * pnuc_cp_sh_n     

      ! Correction for neutron reflected by the outer wall hitting the CP
      pnuc_cp_wp_gam =   f_pnuc_cp_tf * pnuc_cp_wp_gam
      pnuc_cp_wp_n =     f_pnuc_cp_tf * pnuc_cp_wp_n
      pnuc_cp_case_gam = f_pnuc_cp_tf * pnuc_cp_case_gam
      pnuc_cp_case_n =   f_pnuc_cp_tf * pnuc_cp_case_n
      pnuc_cp_sh_gam =   f_pnuc_cp_sh * pnuc_cp_sh_gam
      pnuc_cp_sh_n =     f_pnuc_cp_sh * pnuc_cp_sh_n               

      ! TF nuclear heat [MW]
      pnuc_cp_tf = pnuc_cp_wp_gam + pnuc_cp_wp_n + pnuc_cp_case_gam + pnuc_cp_case_n  

      ! Tungsten density correction
      pnuc_cp_tf = pnuc_cp_tf * f_wc_density

      ! Shield nuclear heat [MW]
      pnuc_cp_sh = pnuc_cp_sh_gam + pnuc_cp_sh_n
      ! ***
    end if

    ! Total CP nuclear heat [MW]
    pnuc_cp = pnuc_cp_tf + pnuc_cp_sh 
    ! ---------------------------

  end subroutine st_centrepost_nuclear_heating

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine st_tf_centrepost_fast_neut_flux( pneutmw, sh_width, rmajor, & ! Inputs
                                              neut_flux_cp )               ! Outputs
    !! Author S Kahn
    !! Routine calculating the fast neutron (E > 0.1 MeV) flux reaching the TF
    !! at the centerpost. These calcualtion are made from a CP only MCNP fit
    !! with a variable tungsten carbyde shield with 13% water cooling. The 
    !! TF size is kept constant in the MCNP runs in such a way tha it increases
    !! size.  

    use tfcoil_variables, only: i_tf_sup

    implicit none
    
    real(8), intent(in) :: pneutmw
    !! neutron fusion power [MW]

    real(8), intent(in) :: sh_width
    !! Neutron shield width [m]

    real(8), intent(in) :: rmajor
    !! Plasma major radius [m]

    real(8), intent(out) :: neut_flux_cp
    !! CP fast neutron flux (E > 0.1 MeV) [m^{-2}.s^}{-1}]

    ! Internal variables
    real(8) :: f_neut_flux_out_wall = 1.0D0
    !! Fraction of fast neutrons originating from the outer wall reflection [-]

    real(8) :: f_wc_density = 2.0D0
    !! Tungsten density may vary with different manufacturing processes.
    !! Here is a factor to take this into account

    real(8) :: f_steel_struct = 0.1
    !! Fraction of steel structures

    real(8) :: sh_width_eff
    !! Effecting shield width, removing steel structures    

    if ( i_tf_sup == 1 ) then 
      
      sh_width_eff = sh_width / ( 1.0D0 - f_steel_struct ) 

      ! Fit [10^{-13}.cm^{-2}]
      neut_flux_cp = 5.835D0 * exp( -15.392D0 * sh_width_eff ) &
                   + 39.70D0 * ( sh_width_eff / rmajor ) * exp( -24.722D0 * sh_width_eff )

      ! Units conversion [10^{-13}.cm^{-2}] -> [m^{-2}]
      neut_flux_cp = neut_flux_cp * 1.0D17

      ! Scaling to the actual plasma neutron power
      neut_flux_cp = f_wc_density * f_neut_flux_out_wall &
                   * neut_flux_cp * ( pneutmw / 800.0D0 )

    end if    

  end subroutine st_tf_centrepost_fast_neut_flux

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine st_cp_angle_fraction( h_cp_top, r_cp_mid, r_cp_top, rmajor, & ! Inputs
                                   f_geom_cp )                             ! Output
    !! Author : S. Kahn, CCFE, Culham science centre
    !! Estimates the CP angular solid angle coverage fration
    !! Equation (1-3) from 
    !! ref : P. Guest THE-REVIEW OF SCIENTIFIC INSTRUMENTS, vol 32, n 2 (1960)
    !! Initial, but undocumented calculation kept as commented section
    !! without any talor expansion approximation

    use constants, only: pi

    implicit none

    ! Inputs
    ! ------
    real(8), intent(in) :: h_cp_top
    !! Centrepost shield half height [m] 

    real(8), intent(in) :: r_cp_top
    !! Centrepost top radius [m]

    real(8), intent(in) :: r_cp_mid
    !! Centrepost mid-plane radius [m]

    real(8), intent(in) :: rmajor
    !! Plasma major radius [m]
    ! ------

    ! Output
    ! ------
    real(8), intent(out) :: f_geom_cp
    !! Solid angle fraction covered by the CP [-]
    ! ------
    
    ! Local variables
    ! ---------------
    real(8) :: rho_maj
    !! Major radius normalized to the CP average radius [-]

    real(8) :: phy_cp
    !! Average CP extent in the toroidal plane [rad]

    real(8) :: d_phy_cp
    !! Toroidal plane infinitesimal angle used in the integral [rad]

    real(8) :: phy_cp_calc
    !! Toroidal plane angles used in the integral [rad]
    
    real(8) :: cp_sol_angle
    !! Centrepost solid angle coverage from plasma centre source [sterad]

    real(8) :: int_calc_1, int_calc_2, int_calc_3
    !! Variables used in the trapezoidal integral

    integer :: n_integral = 10
    !! Number of integral steps 

    integer :: ii
    !! Do loop integer 
    ! ---------------


    ! Initial calculation
    ! -------------------
    ! Kept as commented section for documentation 
    ! ! Fraction of neutrons that hit the centre post neutronic shield
    ! f_geom_cp = cphalflen / sqrt(cphalflen**2 + (rmajor-r_cp_outer)**2 ) &
    !           * atan(r_cp_outer/(rmajor-r_cp_outer) )/pi
    ! -------------------

    ! Major radius normalized to the CP average radius [-]
    rho_maj = 2.0D0 * rmajor / ( r_cp_mid + r_cp_top )

    !! Average CP extent in the toroidal plane [rad]
    phy_cp = asin( 1.0D0 / rho_maj )

    ! toroidal plane infinitesimal angle used in the integral [rad]
    d_phy_cp = phy_cp / dble(n_integral)

    ! CP solid angle integral using trapezoidal method
    phy_cp_calc = 0.0D0
    cp_sol_angle = 0.0D0

    do ii = 1, n_integral

      ! Little tricks to avoild NaNs due to rounding
      int_calc_3 = 1.0D0 - rho_maj**2 * sin(phy_cp_calc)**2 
      if ( int_calc_3 < 0.0D0 ) int_calc_3 = 0.0D0  

      int_calc_1 = 1.0D0 / sqrt( h_cp_top**2 + (rho_maj*cos(phy_cp_calc) &
                               - sqrt(int_calc_3) )**2 )


      phy_cp_calc = phy_cp_calc + d_phy_cp

      ! Little tricks to avoild NaNs due to rounding
      int_calc_3 = 1.0D0 - rho_maj**2 * sin(phy_cp_calc)**2 
      if ( int_calc_3 < 0.0D0 ) int_calc_3 = 0.0D0

      int_calc_2 = 1.0D0 / sqrt( h_cp_top**2 + (rho_maj*cos(phy_cp_calc) &
                               - sqrt(int_calc_3) )**2 )

      cp_sol_angle = cp_sol_angle + d_phy_cp * 0.5D0 * ( int_calc_1 + int_calc_2 )  

    end do
    cp_sol_angle = cp_sol_angle * 4.0D0 * h_cp_top

    ! Solid angle fraction covered by the CP (OUTPUT) [-] 
    f_geom_cp = 0.25D0 * cp_sol_angle / pi

  end subroutine st_cp_angle_fraction

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function pumppower(temp_in, temp_out, pressure, flleng, rad, mf, mfp, no90, &
    no180, etaiso, coolant, label)
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
    use global_variables, only: verbose
    use fwbs_variables, only: coolwh
    use process_output, only: oheadr, ovarre, ocmmnt, oblnkl
    use fw_module, only: friction
    use refprop_interface, only: fluid_properties, enthalpy_ps

    implicit none

    ! Function return parameter: Calculated pumping power (MW)
    real(8) :: pumppower

    ! Arguments !
    ! !!!!!!!!!!!!

    real(8), intent(in) :: flleng, rad, mf, mfp, etaiso
    real(8), intent(in) :: temp_in, temp_out, pressure
    integer, intent(in) :: no90, no180, coolant
    character(len=*), intent(in) :: label

    ! Local variables

    ! Inlet pressure (Pa)
    real(8) :: coolpin

    ! Coolant pressure drop (Pa)
    real(8) :: deltap

    ! Hydraulic diameter (circular channels assumed) (m)
    real(8) :: dh

    ! Fluid specific enthalpy from refprop (J/kg)
    real(8) :: h1

    ! enthalpy
    !real(8) ::

    real(8) :: h2, kelbwn, kelbwt, kstrght, &
         lambda, reyn, rhof, s1, s2, viscf, xifn, xift, ximn, ximt, vv, &
         temp_mean,pdropstraight, pdrop90, pdrop180

    ! TODO Variables that appear not to be used below. Check again before removing
    !real(8) :: cf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! if ((temp_in<100.0d0).or.(temp_in>1500.0d0).or.(temp_in/=temp_in)) call write_output

    ! if ((temp_out<100.0d0).or.(temp_out>1500.0d0).or.(temp_out/=temp_out)) call write_output

    ! if ((pressure<1.0d5).or.(pressure>1.0d9).or.(pressure/=pressure)) call write_output

    ! if ((mfp<1.0d-8).or.(mfp>1.0d0).or.(mfp/=mfp)) call write_output

    ! Mean properties
    temp_mean = (temp_in + temp_out)/2.0d0

    ! Calculate coolant fluid properties
    call fluid_properties(temp_mean, pressure, coolwh, density=rhof, &
      viscosity=viscf, label='1819')

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
    ! if ((coolpin>1.0d9).or.(coolpin<=0.0d0).or.(coolpin/=coolpin)) call write_output

    !
    call fluid_properties(temp_in, coolpin, coolant, enthalpy=h2, entropy=s2, label='1870')

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
        call ovarre(ofile, '            Straight sections (Pa)', '(pdropstraight)', &
          pdropstraight, 'OP ')
        call ovarre(ofile, '            90 degree bends (Pa)', '(pdrop90)', pdrop90, 'OP ')
        call ovarre(ofile, '            180 degree bends (Pa)', '(pdrop180)', pdrop180, 'OP ')
        call ovarre(ofile, 'Inlet pressure (Pa)', '(coolpin)', coolpin, 'OP ')
        call ovarre(ofile, 'Total coolant mass flow rate in (kg/s)', '(mf)', mf, 'OP ')
        call ovarre(ofile, 'Coolant mass flow rate in one channel (kg/s)', '(mfp)', mfp, 'OP ')
        call ovarre(ofile, 'Pumping power (MW)', '(pumppower)', pumppower, 'OP ')
        call ocmmnt(ofile, 'Additional information is printed when verbose = 1')
        if (verbose==1) then
            call oblnkl(ofile)
            call ovarre(ofile, 'Straight section pressure drop coefficient', &
              '(kstrght)', kstrght, 'OP ')
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tbr_shimwell(outfile, iprint, breeder_f, li6enrich, iblanket_thickness, tbr)
    !! Calculates TBR
    !! author: Michael Kovari
    !! breeder_f   : input real : Volume of Li4SiO4 / (Volume of Be12Ti + Li4SiO4)
    !! li6enrich   : input real : lithium-6 enrichment (%)
    !! iblanket_thickness   : input integer : blanket thickness switch
    !! tbr         : output real : 5-year time-averaged tritium breeding ratio
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwith, fwoth
    use constraint_variables, only: tbrmin
    use fwbs_variables, only: fw_armour_thickness
    use process_output, only: ovarrf, ovarin, ocmmnt, ovarre

    implicit none

    ! Inputs
    integer, intent(in) :: iprint, outfile, iblanket_thickness
    real(8), intent(in) :: breeder_f, li6enrich

    ! outputs
    real(8), intent(out) :: tbr

    ! Local variables

    ! Fit expansion terms
    real(8), dimension(3) :: v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, &
                                       v11, v12, v13, v14, v15, v16, v17, v18, v19
    real(8) :: x, y
    integer ::  i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Thin blanket:
    v1(1)= 1.93920586301
    v2(1)= -0.948494854004
    v3(1)= -0.0186700302911
    v4(1)= 0.483417432982
    v5(1)= 0.785901227724
    v6(1)= -0.0120169189644
    v7(1)= -3.45723121388
    v8(1)= -2.05212472576
    v9(1)= 6.45375263346
    v10(1)= -0.436421277881
    v11(1)= 0.0129809166177
    v12(1)= 2.26116309299
    v13(1)= -3.87538808736
    v14(1)= 1.05778783291
    v15(1)= -3.12644013943
    v16(1)= 1.86242247177
    v17(1)= 0.253324925437
    v18(1)= 0.18795823903
    v19(1)= -0.0256707269253

    ! Medium blanket
    v1(2)= 1.96122608615
    v2(2)= -0.860855681012
    v3(2)= 0.0193393390622
    v4(2)= 0.279977226537
    v5(2)= 0.659918133027
    v6(2)= 0.013070435947
    v7(2)= -3.48450356973
    v8(2)= -2.3360647329
    v9(2)= 7.38314099334
    v10(2)= -0.365511595682
    v11(2)= -0.0181287662329
    v12(2)= 2.30397890094
    v13(2)= -4.37481611533
    v14(2)= 1.30804004777
    v15(2)= -3.71450110227
    v16(2)= 2.1588023402
    v17(2)= 0.263823845354
    v18(2)= 0.198976219881
    v19(2)= -0.0192924115968

    ! Thick blanket
    v1(3)= 1.95893103797
    v2(3)= -0.809792727863
    v3(3)= 0.016958778333
    v4(3)= -0.120230857418
    v5(3)= 0.461211316443
    v6(3)= -0.0478789050674
    v7(3)= -2.1978304461
    v8(3)= -1.38785787744
    v9(3)= 4.93883798388
    v10(3)= -0.223668963335
    v11(3)= 0.0178181886132
    v12(3)= 1.42583418972
    v13(3)= -2.80720698559
    v14(3)= 0.814691647096
    v15(3)= -2.48568193656
    v16(3)= 1.37932384899
    v17(3)= 0.253355839249
    v18(3)= 0.190845918447
    v19(3)= -0.0257699008284

    ! 6Li atom fraction
    y = li6enrich/100.0D0
    i = iblanket_thickness
    x = breeder_f
    tbr = v1(i) + v2(i)*x + v3(i)*y + v4(i)*y*x + v5(i)*x**2 + v6(i)*y**2 + v7(i)*x**2*y + &
          v8(i)*x*y**2 + v9(i)*x**2*y**2 + v10(i)*x**3 + v11(i)*y**3 + v12(i)*y*x**3 + &
          v13(i)*y**2*x**3 + v14(i)*y**3*x + v15(i)*y**3*x**2 + v16(i)*y**3*x**3 + &
          v17(i)*log(x) + v18(i)*log(y) + v19(i)*log(x)*log(y)

    ! Output
    if (iprint == 1) then
        call ovarrf(outfile, 'Lithium-6 enrichment (%)', '(li6enrich)', li6enrich)
        call ovarrf(outfile, 'Breeder fraction by volume: Li4SiO4/(Be12Ti+Li4SiO4)', &
          '(breeder_f)', breeder_f)
        if (i == 1) call ovarin(outfile, 'Blanket thickness choice: THIN (0.53 m &
          &inboard, 0.91 m outboard)', '(iblanket_thickness)', iblanket_thickness)
        if (i == 2) call ovarin(outfile, 'Blanket thickness choice: MEDIUM (0.64 m &
          &inboard, 1.11 m outboard)', '(iblanket_thickness)', iblanket_thickness)
        if (i == 3) call ovarin(outfile, 'Blanket thickness choice: THICK (0.75 m &
          &inboard, 1.30 m outboard)', '(iblanket_thickness)', iblanket_thickness)
        call ovarrf(outfile, 'Tritium breeding ratio (5-year time-averaged)','(tbr)',tbr, 'OP ')
        call ovarrf(outfile, 'Minimum Tritium breeding ratio','(tbrmin)',tbrmin)

        call ocmmnt(outfile,'(See "A parameter study of time-varying tritium production &
          &in solid-type breeder blankets,')
        call ocmmnt(outfile, 'J. Shimwell et al, Fusion Engineering and Design')
        call ovarre(outfile, 'For consistency, inboard first wall thicknesses &
          &should be 0.03 (m)', '(fwith)', fwith)
        call ovarre(outfile, 'For consistency, outboard first wall thicknesses &
          &should be 0.03 (m)', '(fwoth)', fwoth)
        call ovarre(outfile, 'For consistency, first wall armour thickness &
          &should be 0.003 (m)', '(fw_armour_thickness)', fw_armour_thickness)
    end if

  end subroutine tbr_shimwell

end module ccfe_hcpb_module

module kit_hcpb_module
  !! Module containing the KIT HCPB blanket model based on the HCPB concept design
  !! author: J Morris, CCFE, Culham Science Centre
  !! author: P J Knight, CCFE, Culham Science Centre
  !! author: F Franza, KIT (original MATLAB implementation)
  !! N/A
  !! This module contains the blanket neutronics model developed
  !! by Fabrizio Franza et al. from Karlsruhe Institute of Technology (KIT)
  !! based on the EUROfusion Helium-Cooled Pebble Bed (HCPB) blanket concept.
  !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
  !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
  !! Karlsruhe Institute of Technology, January 2013;
  !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Terminology
  ! Radial coordinate arrays for the blanket sub-assemblies:
  ! BZ = Breeding Zone
  ! BM = Box Manifold
  ! BP = Back Plates
  ! VV = Vacuum Vessel (includes low-temperature shield)
  ! Element 1 = 'inner' edge, element np(=2) = 'outer' edge
  ! IB = inboard, OB = outboard

  ! Modules to import !
  ! !!!!!!!!!!!!!!!!!!!!
  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  ! Variables for output to file
  integer, private :: ip, ofile

  integer, parameter :: np = 2
  !! Array length

  real(8), dimension(np) :: x_BZ_IB, x_BM_IB, x_BP_IB, x_VV_IB
  real(8), dimension(np) :: x_BZ_OB, x_BM_OB, x_BP_OB, x_VV_OB

  ! Values shared between subroutines in this module
  real(8) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
  real(8) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
  real(8) :: phi_n_vv_IB_start,phi_n_vv_OB_start

  ! Universal constants
  real(8), parameter :: E_n = 14.1D0
  !! [MeV] Average neutron energy

  real(8), parameter :: PA_T = 3.0D0
  !! [g/mol] Tritium atomic weight

  real(8), parameter :: N_Av = 6.02D23  
  !! [at/mol] Avogadro number
  !#TODO: move to constants

  ! Constants and fixed coefficients used in the model
  ! Based on Helium-Cooled Pebble Beds (HCPB) configuration
  ! of the PPCS Model B design
  real(8) :: A_cov_PPCS
  !! Total blanket coverage area [m^2]

  real(8) :: A_FW_PPCS
  !! First wall area [m^2]

  real(8) :: NWL_av_PPCS
  !! Average neutron wall load [MW/m^2]

  real(8) :: NWL_av_IB_PPCS
  !! Average IB wall load [MW/m^2]

  real(8) :: NWL_av_OB_PPCS
  !! Average OB wall load [MW/m^2]

  real(8) :: NWL_max_IB_PPCS
  !! Maximum IB wall load [MW/m^2]

  real(8) :: NWL_max_OB_PPCS
  !! Maximum OB wall load [MW/m^2]

  real(8) :: CF_bl_PPCS
  !! Blanket coverage factor (calculated) [%]

  ! Power density pre-exponential terms and decay lengths
  real(8) :: q_0_BZ_breed_IB
  !! Pre-exp term in IB BZ breeder [W/cm^3]

  real(8) :: q_0_BZ_breed_OB
  !! Pre-exp term in OB BZ breeder [W/cm^3]

  real(8) :: lambda_q_BZ_breed_IB
  !! Decay length in IB BZ breeder [cm]

  real(8) :: lambda_q_BZ_breed_OB
  !! Decay length in OB BZ breeder [cm]

  real(8) :: q_0_BZ_Be_IB
  !! Pre-exp term in IB BZ Beryllium [W/cm^3]

  real(8) :: q_0_BZ_Be_OB
  !! Pre-exp term in OB BZ Beryllium [W/cm^3]

  real(8) :: lambda_q_BZ_Be_IB
  !! Decay length in IB BZ Beryllium [cm]

  real(8) :: lambda_q_BZ_Be_OB
  !! Decay length in OB BZ Beryllium [cm]

  real(8) :: q_0_BZ_steels_IB
  !! Pre-exp term in IB BZ steels [W/cm^3]

  real(8) :: q_0_BZ_steels_OB
  !! Pre-exp term in OB BZ steels [W/cm^3]

  real(8) :: lambda_q_BZ_steels_IB
  !! Decay length in IB BZ steels [cm]

  real(8) :: lambda_q_BZ_steels_OB
  !! Decay length in OB BZ steels [cm]

  real(8) :: lambda_EU
  !! Decay length in EUROFER [cm]

  real(8) :: lambda_q_BM_IB
  !! Decay length in IB BM (calculated) [cm]

  real(8) :: lambda_q_BM_OB
  !! Decay length in OB BM (calculated) [cm]

  real(8) :: lambda_q_BP_IB
  !! Decay length in IB BP (calculated) [cm]

  real(8) :: lambda_q_BP_OB
  !! Decay length in OB BP (calculated) [cm]

  real(8) :: lambda_q_VV
  !! Decay length in Vacuum Vessel [cm]

  ! Fast neutron flux pre-exponential terms and decay lengths
  real(8) :: phi_0_n_BZ_IB
  !! Pre-exp term in IB BZ [n/cm^2/sec]

  real(8) :: phi_0_n_BZ_OB
  !! Pre-exp term in OB BZ [n/cm^2/sec]

  real(8) :: lambda_n_BZ_IB
  !! Decay length in IB BZ [cm]

  real(8) :: lambda_n_BZ_OB
  !! Decay length in OB BZ [cm]

  real(8) :: lambda_n_VV
  !! Decay length in VV [cm]

  real(8) :: phi_n_0_VV_ref
  !! Reference fast neutron flux on VV inner side [Fish09] [n/cm^2/sec]

  ! Vacuum vessel helium production pre-exponential terms and decay lengths
  real(8) :: Gamma_He_0_ref
  !! Pre-exp term [appm/yr]

  real(8) :: lambda_He_VV
  !! Decay length [cm]

  real(8) :: D_EU_max
  !! Allowable neutron damage to the FW EUROFER [dpa]

  ! Variables used in this module, ultimately to be set via the calling routine
  ! to values given by PROCESS variables
  real(8), public :: P_n
  !! Fusion neutron power [MW]

  real(8), public :: NWL_av
  !! Average neutron wall load [MW/m^2]

  real(8), public :: f_peak
  !! NWL peaking factor [--]

  real(8), public :: t_FW_IB
  !! IB first wall thickness [cm]

  real(8), public :: t_FW_OB
  !! OB first wall thickness [cm]

  real(8), public :: A_FW_IB
  !! IB first wall area [cm^2]

  real(8), public :: A_FW_OB
  !! OB first wall area [cm^2]

  real(8), public :: A_bl_IB
  !! IB blanket area [cm^2]

  real(8), public :: A_bl_OB
  !! OB blanket area [cm^2]

  real(8), public :: A_VV_IB
  !! IB shield/VV area [cm^2]

  real(8), public :: A_VV_OB
  !! OB shield/VV area [cm^2]

  real(8), public :: CF_bl
  !! Blanket coverage factor [%]

  integer, public :: n_ports_div
  !! Number of divertor ports [ports]

  integer, public :: n_ports_H_CD_IB
  !! Number of IB H&CD ports [ports]

  integer, public :: n_ports_H_CD_OB
  !! Number of OB H&CD ports [ports]

  character(len=5), public :: H_CD_ports
  !! Type of H&CD ports (small or large)

  real(8), public :: e_Li
  !! Lithium 6 enrichment [%]
  
  real(8), public :: t_plant
  !! Plant lifetime [FPY]

  real(8), public :: alpha_m
  !! Availability factor [--]

  real(8), public :: alpha_puls
  !! Pulsed regime fraction [--]

  character(len=20), public :: breeder
  !! Breeder type (allowed values are Orthosilicate, Metatitanate or Zirconate)

  ! Inboard parameters
  real(8), public :: t_BZ_IB
  !! BZ thickness [cm]

  real(8), public :: t_BM_IB
  !! BM thickness [cm]

  real(8), public :: t_BP_IB
  !! BP thickness [cm]

  real(8), public :: t_VV_IB
  !! VV thickness [cm]

  real(8), public :: alpha_BM_IB
  !! Helium fraction in the IB BM [%]

  real(8), public :: alpha_BP_IB
  !! Helium fraction in the IB BP [%]

  real(8), public :: chi_Be_BZ_IB
  !! Beryllium vol. frac. in IB BZ [%]

  real(8), public :: chi_breed_BZ_IB
  !! Breeder vol. frac. in IB BZ [%]

  real(8), public :: chi_steels_BZ_IB
  !! Steels vol. frac. in IB BZ [%]

  ! Outboard parameters
  real(8), public :: t_BZ_OB
  !! BZ thickness [cm]

  real(8), public :: t_BM_OB
  !! BM thickness [cm]

  real(8), public :: t_BP_OB
  !! BP thickness [cm]

  real(8), public :: t_VV_OB
  !! VV thickness [cm]

  real(8), public :: alpha_BM_OB
  !! Helium fraction in the OB BM [%]

  real(8), public :: alpha_BP_OB
  !! Helium fraction in the OB BP [%]

  real(8), public :: chi_Be_BZ_OB
  !! Beryllium vol. frac. in OB BZ [%]

  real(8), public :: chi_breed_BZ_OB
  !! Breeder vol. frac. in OB BZ [%]

  real(8), public :: chi_steels_BZ_OB
  !! Steels vol. frac. in OB BZ [%]

  ! Model outputs
  real(8), public :: pnuctfi 
  !! Nuclear heating on IB TF coil [MW/m3]

  real(8), public :: pnuctfo 
  !! Nuclear heating on OB TF coil [MW/m3]

  real(8), public :: P_th_tot
  !! Nuclear power generated in blanket [MW]

  real(8), public :: pnucsh  
  !! Nuclear power generated in shield/VV [MW]

  real(8), public :: M_E     
  !! Energy multiplication factor [--]

  real(8), public :: tbratio 
  !! Tritium breeding ratio [--]

  real(8), public :: G_tot   
  !! Tritium production rate [g/day]

  real(8), public :: nflutfi 
  !! Fast neutron fluence on IB TF coil [n/cm2]

  real(8), public :: nflutfo 
  !! Fast neutron fluence on OB TF coil [n/cm2]

  real(8), public :: vvhemini
  !! minimum final He. conc in IB VV [appm]

  real(8), public :: vvhemino
  !! minimum final He. conc in OB VV [appm]

  real(8), public :: vvhemaxi
  !! maximum final He. conc in IB VV [appm]

  real(8), public :: vvhemaxo
  !! maximum final He. conc in OB VV [appm]

  real(8), public :: t_bl_fpy
  !! blanket lifetime in full power years [y]

  real(8), public :: t_bl_y  
  !! blanket lifetime in calendar years [y]

  real(8), private :: vfblkti, vfblkto
  !! Inboard/outboard void fraction of blanket

  ! Component volume info
  real(8), private :: hblnkt
  !! Blanket internal half-height (m)

  real(8), private :: hshld
  !! Shield internal half-height (m)

  real(8), private :: hcryopf
  !! Clearance between uppermost PF coil and cryostat lid (m)

  real(8), private :: hvv
  !! Vacuum vessel internal half-height (m)

  real(8), private :: volshldi, volshldo
  !! Volume of inboard and outboard shield (m3)

contains

  ! #TODO : global check
  ! #TODO : Output section for model!

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_kit_hcpb_module
    !! Initialise module variables
    implicit none

    A_cov_PPCS = 1365.0D0
    A_FW_PPCS = 1253.0D0
    NWL_av_PPCS = 1.94D0
    NWL_av_IB_PPCS = 1.73D0
    NWL_av_OB_PPCS = 1.92D0
    NWL_max_IB_PPCS = 1.99D0
    NWL_max_OB_PPCS = 2.41D0
    q_0_BZ_breed_IB = 23.41D0
    q_0_BZ_breed_OB = 28.16D0
    lambda_q_BZ_breed_IB = 44.56D0
    lambda_q_BZ_breed_OB = 28.37D0
    q_0_BZ_Be_IB = 7.5D0
    q_0_BZ_Be_OB = 8.85D0
    lambda_q_BZ_Be_IB = 21.19D0
    lambda_q_BZ_Be_OB = 19.33D0
    q_0_BZ_steels_IB = 9.04D0
    q_0_BZ_steels_OB = 9.93D0
    lambda_q_BZ_steels_IB = 21.59D0
    lambda_q_BZ_steels_OB = 20.61D0
    lambda_EU = 11.57D0
    lambda_q_VV = 6.92D0
    phi_0_n_BZ_IB = 5.12D14  
    phi_0_n_BZ_OB = 5.655D14 
    lambda_n_BZ_IB = 18.79D0 
    lambda_n_BZ_OB = 19.19D0 
    lambda_n_VV = 8.153D0    
    phi_n_0_VV_ref = 2.0D10
    Gamma_He_0_ref = 1.8D-3
    lambda_He_VV = 7.6002D0 
    D_EU_max = 60.0D0
    P_n = 2720.0D0
    NWL_av = 1.94D0 
    f_peak = 1.21D0 
    t_FW_IB = 2.3D0 
    t_FW_OB = 2.3D0
    A_FW_IB = 3.5196D6
    A_FW_OB = 9.0504D6
    A_bl_IB = 3.4844D6
    A_bl_OB = 8.9599D6
    A_VV_IB = 3.8220D6
    A_VV_OB = 9.8280D6
    CF_bl = 91.7949D0
    n_ports_div = 2    
    n_ports_H_CD_IB = 2
    n_ports_H_CD_OB = 2
    H_CD_ports = 'small'
    e_Li = 60.0D0     
    t_plant = 40.0D0
    alpha_m = 0.75D0
    alpha_puls = 1.0D0
    breeder = 'Orthosilicate'
    t_BZ_IB = 36.5D0
    t_BM_IB = 17.0D0
    t_BP_IB = 30.0D0
    t_VV_IB = 35.0D0
    alpha_BM_IB = 40.0D0 
    alpha_BP_IB = 65.95D0
    chi_Be_BZ_IB = 69.2D0
    chi_breed_BZ_IB = 15.4D0
    chi_steels_BZ_IB = 9.8D0
    t_BZ_OB = 46.5D0
    t_BM_OB = 27.0D0
    t_BP_OB = 35.0D0
    t_VV_OB = 65.0D0
    alpha_BM_OB = 40.0D0 
    alpha_BP_OB = 67.13D0
    chi_Be_BZ_OB = 69.2D0
    chi_breed_BZ_OB = 15.4D0
    chi_steels_BZ_OB = 9.8D0
  end subroutine init_kit_hcpb_module

  function f_alpha(alpha)

    !! Calculates the power density decay length multiplier
    !! in a blanket region given the helium fraction
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! alpha : input real : helium fraction (%)
    !! This routine calculates the power density decay length
    !! multiplier in a blanket region comprising EUROFER steel and
    !! helium coolant, given the helium volume fraction within the
    !! region.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    real(8), intent(in) :: alpha

    ! Local variables
    real(8) :: f_alpha

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    f_alpha = 1.0D0 + 0.019D0*alpha

  end function f_alpha

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine kit_hcpb(outfile, iprint)
    !! Main routine for the KIT HCPB blanket model
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calls the main work routines for the KIT HCPB
    !! blanket model.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: fwith, fwoth, fwareaib, fwareaob, blareaib, &
      blareaob, shareaib, shareaob, blbuith, blbmith, blbpith, shldith, d_vv_in, &
      d_vv_out, blbuoth, blbmoth, blbpoth, shldoth
    use cost_variables, only: abktflnc, tlife, cfactr
    use fwbs_variables, only: afw, fw_wall, fwlife, coolwh, wallpf, fhcd, fdiv, &
      npdiv, nphcdin, nphcdout, hcdportsize, li6enrich, breedmat, densbreed, &
      fblhebmi, fblhebmo, fblhebpi, fblhebpo, fblbe, fblbreed, fblss, pnucblkt, &
      pnucshld, emult, tbr, tritprate, bktlife, nflutf, ptfnucpm3, ptfnuc, &
      vvhemax
    use physics_variables, only: wallmw, pneutmw
    use tfcoil_variables, only: tfleng, tfareain, arealeg, n_tf
    use times_variables, only: tpulse, tramp, tdwell

    implicit none

    ! Arguments
    integer, intent(in) :: iprint, outfile

    ! Assign module private variables to iprint and outfile
    ip = iprint
    ofile = outfile

    ! SIM (Issue #669)
    fwith = 2.0D0*afw + 2.0D0*fw_wall
    fwoth = fwith

    ! Calculate FW/Blanket lifetime
    fwlife = min(abktflnc/wallmw, tlife)

    ! Coolant type
    coolwh = 1

    ! Calculate component volumes
    call component_volumes

    ! Convert global variables into KIT blanket inputs
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
    CF_bl = (1.0D0-fhcd-fdiv) * 100.0D0 ! [%] Blanket coverage factor
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

    ! Breeder type (allowed values are Orthosilicate, Metatitanate or Zirconate)
    !
    ! Mass densities supplied by F. Franza, taken from Seventh International
    ! Workshop on Ceramic Breeder Blanket Interactions, September 14-16, 1998,
    ! Petten, Netherlands:
    !                              Li4Si04      Li2TiO3      Li2ZrO3
    ! Theory density [g/cm^3]      2.40         3.45         4.19
    ! Material density             98 %         94 %         89 %
    ! Packing factor               64 %         55 %         57 %
    ! Pebble bed density [g/cm^3]  1.50         1.78         2.12
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

    ! Inboard parameters
    t_BZ_IB = blbuith * 100.0D0          ! [cm] BZ thickness
    t_BM_IB = blbmith * 100.0D0          ! [cm] BM thickness
    t_BP_IB = blbpith * 100.0D0          ! [cm] BP thickness
    t_VV_IB = (shldith+d_vv_in) * 100.0D0 ! [cm] VV thickness
    alpha_BM_IB = fblhebmi * 100.0D0     ! [%] Helium fraction in the IB BM
    alpha_BP_IB = fblhebpi * 100.0D0     ! [%] Helium fraction in the IB BP
    chi_Be_BZ_IB = fblbe * 100.0D0       ! [%] Beryllium vol. frac. in IB BZ
    chi_breed_BZ_IB = fblbreed * 100.0D0 ! [%] Breeder vol. frac. in IB BZ
    chi_steels_BZ_IB = fblss * 100.0D0   ! [%] Steels vol. frac. in IB BZ

    ! Outboard parameters
    t_BZ_OB = blbuoth * 100.0D0          ! [cm] BZ thickness
    t_BM_OB = blbmoth * 100.0D0          ! [cm] BM thickness
    t_BP_OB = blbpoth * 100.0D0          ! [cm] BP thickness
    t_VV_OB = (shldoth+d_vv_out) * 100.0D0 ! [cm] VV thickness
    alpha_BM_OB = fblhebmo * 100.0D0     ! [%] Helium fraction in the OB BM
    alpha_BP_OB = fblhebpo * 100.0D0     ! [%] Helium fraction in the OB BP
    chi_Be_BZ_OB = fblbe * 100.0D0       ! [%] Beryllium vol. frac. in OB BZ
    chi_breed_BZ_OB = fblbreed * 100.0D0 ! [%] Breeder vol. frac. in OB BZ
    chi_steels_BZ_OB = fblss * 100.0D0   ! [%] Steels vol. frac. in OB BZ

    ! Perform preliminary calculations for the PPCS Model B configuration
    ! Blanket coverage factor (%)
    CF_bl_PPCS = A_FW_PPCS/A_cov_PPCS * 100.0D0

    ! Power density decay lengths (cm) in the BM and BP regions,
    ! given the helium fractions
    lambda_q_BM_IB = lambda_EU * f_alpha(alpha_BM_IB)
    lambda_q_BM_OB = lambda_EU * f_alpha(alpha_BM_OB)
    lambda_q_BP_IB = lambda_EU * f_alpha(alpha_BP_IB)
    lambda_q_BP_OB = lambda_EU * f_alpha(alpha_BP_OB)

    ! Initialise the radial coordinate arrays, defining the blanket
    ! sub-assembly thicknesses
    call radial_coordinates

    ! Perform the main calculations
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

    ! Transfer output values from model to global variables
    pnucblkt = P_th_tot
    pnucshld = pnucsh
    emult = M_E
    tbr = tbratio
    tritprate = G_tot
    bktlife = t_bl_fpy  !  This is later adjusted for availability in routine AVAIL

    ! Peak fast neutron fluence on TF coils (neutrons/m2)
    nflutf = max(nflutfi,nflutfo) * 1.0D4

    ! Peak nuclear heating in TF coil (MW/m3)
    ptfnucpm3 = max(pnuctfi,pnuctfo)

    ! Total nuclear heating in TF coil (MW)
    ! Rough estimate of TF coil volume used, assuming 25% of the total
    ! TF coil perimeter is inboard, 75% outboard
    ptfnuc = 0.25D0*tfleng*tfareain * pnuctfi &
         + 0.75D0*tfleng*arealeg*n_tf * pnuctfo

    ! Maximum helium concentration in vacuum vessel at
    ! end of plant lifetime (appm)
    vvhemax = max(vvhemaxi,vvhemaxo)

    if (ip == 0) return

    call write_kit_hcpb_output

  end subroutine kit_hcpb

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine radial_coordinates
    !! Sets up the radial build within the KIT blanket
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! None
    !! This routine sets up the arrays containing the radial
    !! build within each blanket sub-assembly.
    !! <P>At present, the arrays contain only NP=2 elements, i.e. contain the
    !! values at the inner and outer radial locations; however, if required,
    !! they may be changed easily to provide several points for plotting
    !! purposes, for example.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Radial coordinates in each inboard sub-assembly (cm)
    ! Element 1 is 'inner' edge (nearer the plasma), element np (=2) is 'outer' edge
    x_BZ_IB(1) = 0.0D0 ; x_BZ_IB(np) = t_FW_IB + t_BZ_IB
    x_BM_IB(1) = 0.0D0 ; x_BM_IB(np) = t_BM_IB
    x_BP_IB(1) = 0.0D0 ; x_BP_IB(np) = t_BP_IB
    x_VV_IB(1) = 0.0D0 ; x_VV_IB(np) = t_VV_IB

    ! Radial coordinates in each outboard sub-assembly (cm)
    x_BZ_OB(1) = 0.0D0 ; x_BZ_OB(np) = t_FW_OB + t_BZ_OB
    x_BM_OB(1) = 0.0D0 ; x_BM_OB(np) = t_BM_OB
    x_BP_OB(1) = 0.0D0 ; x_BP_OB(np) = t_BP_OB
    x_VV_OB(1) = 0.0D0 ; x_VV_OB(np) = t_VV_OB

  end subroutine radial_coordinates

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine power_density(q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end, &
    q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end,pnuctfi,pnuctfo)
    !! Calculates the nuclear power density profiles
    !!  within the KIT blanket sub-assemblies
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! q_BZ_IB_end : output real : power density at outer edge of IB BZ (MW/m3)
    !! q_BM_IB_end : output real : power density at outer edge of IB BM (MW/m3)
    !! q_BP_IB_end : output real : power density at outer edge of IB BP (MW/m3)
    !! q_BZ_OB_end : output real : power density at outer edge of OB BZ (MW/m3)
    !! q_BM_OB_end : output real : power density at outer edge of OB BM (MW/m3)
    !! q_BP_OB_end : output real : power density at outer edge of OB BP (MW/m3)
    !! pnuctfi     : output real : power density at outer edge of IB VV (MW/m3)
    !! = that on inner TF coil winding pack
    !! pnuctfo     : output real : power density at outer edge of OB VV (MW/m3)
    !! = that on outer TF coil winding pack
    !! This routine calculates the nuclear power density profiles within each
    !! blanket sub-assembly, assuming an exponential decay with distance through
    !! each region, with the decay indices dependent on the material fractions.
    !! <P>At present, the arrays contain only NP=2 elements, i.e. contain the
    !! values at the inner and outer radial locations; however, if required,
    !! they may be changed easily to provide several points for plotting
    !! purposes, for example.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    real(8), intent(out) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
    real(8), intent(out) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
    real(8), intent(out) :: pnuctfi, pnuctfo

    ! Local variables
    real(8), dimension(np) :: q_steels_BZ_IB, q_steels_BZ_OB
    real(8), dimension(np) :: q_BM_IB, q_BP_IB, q_VV_IB
    real(8), dimension(np) :: q_BM_OB, q_BP_OB, q_VV_OB

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! N.B. Power density is in W/cm3 = MW/m3
    ! Inboard profiles
    ! Power density profile in IB BZ steels
    q_steels_BZ_IB(:) = NWL_av/NWL_av_PPCS * q_0_BZ_steels_IB * &
         exp(-x_BZ_IB(:)/lambda_q_BZ_steels_IB)
    q_BZ_IB_end = q_steels_BZ_IB(np)

    ! Power density profile in IB BM
    q_BM_IB(:) = q_steels_BZ_IB(np) * exp(-x_BM_IB(:)/lambda_q_BM_IB)
    q_BM_IB_end = q_BM_IB(np)

    ! Power density profile in IB BP
    q_BP_IB(:) = q_BM_IB(np) * exp(-x_BP_IB(:)/lambda_q_BP_IB)
    q_BP_IB_end = q_BP_IB(np)

    ! Power density profile in IB VV
    q_VV_IB(:) = q_BP_IB(np) * exp(-x_VV_IB(:)/lambda_q_VV)

    ! Outboard profiles
    ! Power density profile in OB BZ steels
    q_steels_BZ_OB(:) = NWL_av/NWL_av_PPCS * q_0_BZ_steels_OB * &
         exp(-x_BZ_OB(:)/lambda_q_BZ_steels_OB)
    q_BZ_OB_end = q_steels_BZ_OB(np)

    ! Power density profile in OB BM
    q_BM_OB(:) = q_steels_BZ_OB(np) * exp(-x_BM_OB(:)/lambda_q_BM_OB)
    q_BM_OB_end = q_BM_OB(np)

    ! Power density profile in OB BP
    q_BP_OB(:) = q_BM_OB(np) * exp(-x_BP_OB(:)/lambda_q_BP_OB)
    q_BP_OB_end = q_BP_OB(np)

    ! Power density profile in OB VV
    q_VV_OB(:) = q_BP_OB(np) * exp(-x_VV_OB(:)/lambda_q_VV)

    ! Nuclear heating on TF coil winding pack is assumed to be equal to
    ! the value at the outer edge of the VV (neglecting the steel TF coil case
    pnuctfi = q_VV_IB(np)
    pnuctfo = q_VV_OB(np)

  end subroutine power_density

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine nuclear_power_production(q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end, &
       q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end,P_th_tot,M_E,pnucsh)
    !! Calculates nuclear power production and energy multiplication factor
    !! within the KIT blanket sub-assemblies
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! q_BZ_IB_end : input real : power density at outer edge of IB BZ (MW/m3)
    !! q_BM_IB_end : input real : power density at outer edge of IB BM (MW/m3)
    !! q_BP_IB_end : input real : power density at outer edge of IB BP (MW/m3)
    !! q_BZ_OB_end : input real : power density at outer edge of OB BZ (MW/m3)
    !! q_BM_OB_end : input real : power density at outer edge of OB BM (MW/m3)
    !! q_BP_OB_end : input real : power density at outer edge of OB BP (MW/m3)
    !! p_th_tot    : output real : total nuclear power in the blanket (MW)
    !! m_e         : output real : energy multiplication factor in the blanket
    !! pnucsh      : output real : total nuclear power in the shield (MW)
    !! This routine calculates the nuclear power production within each
    !! blanket sub-assembly, assuming an exponential decay with distance through
    !! each region, with the decay indices dependent on the material fractions.
    !! These are summed to give the total nuclear power produced in the 'blanket'
    !! (BZ+BM+BP) and 'shield' regions, and the energy multiplication factor
    !! in the blanket is calculated.
    !! WP13-SYS01-A-T02 Interim Review Meeting, 10.07.2013, F. Franza
    !! (describes 26/09/2013 model refinement)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    real(8), intent(in) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
    real(8), intent(in) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
    real(8), intent(out) :: P_th_tot, M_E, pnucsh

    ! Local variables
    real(8) :: A_BZ_breed_IB, A_BZ_breed_OB, A_BZ_Be_IB, A_BZ_Be_OB
    real(8) :: A_BZ_steels_IB, A_BZ_steels_OB
    real(8) :: P_BZ_breed_IB, P_BZ_Be_IB, P_BZ_steels_IB
    real(8) :: P_BZ_IB, P_BM_IB, P_BP_IB, P_VV_IB
    real(8) :: P_BZ_breed_OB, P_BZ_Be_OB, P_BZ_steels_OB
    real(8) :: P_BZ_OB, P_BM_OB, P_BP_OB, P_VV_OB
    real(8) :: P_tot_IB, P_tot_OB, P_n_FW

    real(8) :: nwl_ratio, nwl_IB_ratio_PPCS, nwl_OB_ratio_PPCS

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nwl_ratio = NWL_av/NWL_av_PPCS
    nwl_IB_ratio_PPCS = NWL_av_IB_PPCS / NWL_max_IB_PPCS
    nwl_OB_ratio_PPCS = NWL_av_OB_PPCS / NWL_max_OB_PPCS

    ! Cross-sectional areas in the breeder zone (cm2)
    ! Breeder (chi... = volumetric fraction as a percentage)
    A_BZ_breed_IB = A_bl_IB * 0.01D0*chi_breed_BZ_IB
    A_BZ_breed_OB = A_bl_OB * 0.01D0*chi_breed_BZ_OB

    ! Beryllium pebbles
    A_BZ_Be_IB = A_bl_IB * 0.01D0*chi_Be_BZ_IB
    A_BZ_Be_OB = A_bl_OB * 0.01D0*chi_Be_BZ_OB

    ! Breeder Zone steels
    A_BZ_steels_IB = A_bl_IB * 0.01D0*chi_steels_BZ_IB
    A_BZ_steels_OB = A_bl_OB * 0.01D0*chi_steels_BZ_OB

    ! Inboard power terms (MW)
    ! Nuclear power in IB breeder pebbles
    P_BZ_breed_IB = 1.0D-6 * nwl_ratio * nwl_IB_ratio_PPCS * A_BZ_breed_IB * &
         lambda_q_BZ_breed_IB * q_0_BZ_breed_IB * &
         ( exp(-t_FW_IB/lambda_q_BZ_breed_IB) - &
         exp(-(t_FW_IB+t_BZ_IB)/lambda_q_BZ_breed_IB) )

    ! Nuclear power in IB Be pebbles
    P_BZ_Be_IB = 1.0D-6 * nwl_ratio * nwl_IB_ratio_PPCS * A_BZ_Be_IB * &
         lambda_q_BZ_Be_IB * q_0_BZ_Be_IB * &
         ( exp(-t_FW_IB/lambda_q_BZ_Be_IB) - &
         exp(-(t_FW_IB+t_BZ_IB)/lambda_q_BZ_Be_IB) )

    ! Nuclear power in IB BZ steels
    P_BZ_steels_IB = 1.0D-6 * nwl_ratio * nwl_IB_ratio_PPCS * A_BZ_steels_IB * &
         lambda_q_BZ_steels_IB * q_0_BZ_steels_IB * &
         (1.0D0-exp(-(t_FW_IB+t_BZ_IB)/lambda_q_BZ_steels_IB))

    ! Total nuclear power in IB BZ
    P_BZ_IB = P_BZ_breed_IB + P_BZ_Be_IB + P_BZ_steels_IB

    ! Nuclear power in IB BM
    P_BM_IB = 1.0D-6 * nwl_IB_ratio_PPCS * A_bl_IB * &
         lambda_q_BM_IB * q_BZ_IB_end * &
         (1.0D0-exp(-t_BM_IB/lambda_q_BM_IB))

    ! Nuclear power in IB BP
    P_BP_IB = 1.0D-6 * nwl_IB_ratio_PPCS * A_bl_IB * &
         lambda_q_BP_IB * q_BM_IB_end * &
         (1.0D0-exp(-t_BP_IB/lambda_q_BP_IB))

    ! Nuclear power in IB VV
    P_VV_IB = 1.0D-6 * nwl_IB_ratio_PPCS * A_VV_IB * &
         lambda_q_VV * q_BP_IB_end * &
         (1.0D0-exp(-t_VV_IB/lambda_q_VV))

    ! Outboard power terms (MW)
    ! Nuclear power in OB BZ breeder pebbles
    P_BZ_breed_OB = 1.0D-6 * nwl_ratio * nwl_OB_ratio_PPCS * A_BZ_breed_OB * &
         lambda_q_BZ_breed_OB * q_0_BZ_breed_OB * &
         ( exp(-t_FW_OB/lambda_q_BZ_breed_OB) - &
         exp(-(t_FW_OB+t_BZ_OB)/lambda_q_BZ_breed_OB) )

    ! Nuclear power in OB BZ Be pebbles
    P_BZ_Be_OB = 1.0D-6 * nwl_ratio * nwl_OB_ratio_PPCS * A_BZ_Be_OB * &
         lambda_q_BZ_Be_OB * q_0_BZ_Be_OB * &
         ( exp(-t_FW_OB/lambda_q_BZ_Be_OB) - &
         exp(-(t_FW_OB+t_BZ_OB)/lambda_q_BZ_Be_OB) )

    ! Nuclear power in OB BZ steels
    P_BZ_steels_OB = 1.0D-6 * nwl_ratio * nwl_OB_ratio_PPCS * A_BZ_steels_OB * &
         lambda_q_BZ_steels_OB * q_0_BZ_steels_OB * &
         (1.0D0-exp(-(t_FW_OB+t_BZ_OB)/lambda_q_BZ_steels_OB))

    ! Total nuclear power in OB BZ
    P_BZ_OB = P_BZ_breed_OB + P_BZ_Be_OB + P_BZ_steels_OB

    ! Nuclear power in OB BM
    P_BM_OB = 1.0D-6 * nwl_OB_ratio_PPCS * A_bl_OB * &
         lambda_q_BM_OB * q_BZ_OB_end * &
         (1.0D0-exp(-t_BM_OB/lambda_q_BM_OB))

    ! Nuclear power in OB BP
    P_BP_OB = 1.0D-6 * nwl_OB_ratio_PPCS * A_bl_OB * &
         lambda_q_BP_OB * q_BM_OB_end * &
         (1.0D0-exp(-t_BP_OB/lambda_q_BP_OB))

    ! Nuclear power in OB VV
    P_VV_OB = 1.0D-6 * nwl_OB_ratio_PPCS * A_VV_OB * &
         lambda_q_VV * q_BP_OB_end * &
         (1.0D0-exp(-t_VV_OB/lambda_q_VV))

    ! Total nuclear power in IB and OB regions (MW)
    ! Excludes contribution from shield/vacuum vessel
    P_tot_IB = P_BZ_IB + P_BM_IB + P_BP_IB
    P_tot_OB = P_BZ_OB + P_BM_OB + P_BP_OB

    ! Total nuclear power in the 'blanket' (MW)
    P_th_tot = P_tot_IB + P_tot_OB

    ! Total nuclear power in shield/VV (MW)
    pnucsh = P_VV_IB + P_VV_OB

    ! Fusion neutron power impinging first wall (MW)
    P_n_FW = P_n * 0.01D0*CF_bl

    ! Energy multiplication factor
    M_E = P_th_tot/P_n_FW

  end subroutine nuclear_power_production

   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine tritium_breeding_ratio(tbr,g_tot)
    !! Calculates the tritium breeding ratio for the KIT blanket
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! tbr   : output real : tritium breeding ratio
    !! g_tot : output real : tritium production rate (g/day)
    !! This routine calculates the tritium breeding ratio and the rate
    !! of production of tritium in the KIT blanket design, taking into
    !! account the breeding material and the number and size of ports
    !! in the blanket.
    !! WP13-SYS01-A-T02 Interim Review Meeting, 10.07.2013, F. Franza
    !! (describes 26/09/2013 model refinement)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments

    real(8), intent(out) :: tbr, g_tot

    ! Local variables

    real(8) :: wib, wob, eu_frac
    real(8), parameter :: wib_PPCS = 0.28D0, wob_PPCS = 0.72D0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Changed to % units by multiplying by 100.0
    wib = (A_FW_IB / (A_FW_IB + A_FW_OB))*100.0D0
    wob = (A_FW_OB / (A_FW_IB + A_FW_OB))*100.0D0

    eu_frac = (chi_steels_BZ_IB + chi_steels_BZ_OB)/2.0D0
    tbr = 0.9157 * CF_bl * &
       (1 - exp(-wib*(t_BZ_IB/38.35))) * &
    (1 - exp(-wob*(t_BZ_OB/39.95))) * &
    (1.17126 - 0.01231*eu_frac + 7.9431D-5*eu_frac**2) * &
    (0.1362*log(e_Li) + 0.6331)

    ! Total tritium production rate (grammes/day)
    g_tot = tbr * P_n/(E_n*1.602D-19)/N_Av * PA_T*3600*24

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function TBR_breed(e_Li, breeder)
      !! Returns a fit to the tritium breeding ratio for different breeder
      !! materials
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: F Franza, KIT (original MATLAB implementation)
      !! e_li   : input real : Lithium-6 enrichment (%)
      !! breeder : input character string : breeder material; either
      !! <UL><LI>'Orthosilicate' or
      !! <LI>'Metatitanate' or
      !! <LI>'Zirconate'</UL>
      !! This routine provides the dependence of the tritium breeding
      !! ratio on the ceramic breeder in use and the lithium-6 enrichment of
      !! the breeder.
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use error_handling, only: report_error

      implicit none

      ! Arguments
      real(8), intent(in) :: e_Li
      character(len=*), intent(in) :: breeder

      ! Local variables
      real(8) :: TBR_breed

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
      !! Returns a fit to the tritium breeding ratio with different
      !! machine port types
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: F Franza, KIT (original MATLAB implementation)
      !! n_ports_div : input integer : number of divertor ports
      !! n_ports_h_cd_ib : input integer : number of inboard H/CD ports
      !! n_ports_h_cd_ob : input integer : number of outboard H/CD ports
      !! h_cd_ports : input character string : H/CD port size;
      !! <UL><LI>'small' or <LI>'large'</UL>
      !! This routine provides the dependence of the tritium breeding
      !! ratio on the number and size of machine ports.
      !! The equatorial heating/current drive ports may be specified as
      !! being either <CODE>'small'</CODE> (1.27 x 1.5 m2) or
      !! <CODE>'large'</CODE> (3 x 3 m2).
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      ! Arguments
      integer, intent(in) :: n_ports_div, n_ports_H_CD_IB, n_ports_H_CD_OB
      character(len=*), intent(in) :: H_CD_ports

      ! Local variables
      real(8) :: TBR_ports

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
    !! Calculates fast neutron fluence within the KIT blanket sub-assemblies
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! phi_n_vv_IB_start : output real : flux at inner edge of IB VV (n/cm2/s)
    !! phi_n_vv_OB_start : output real : flux at inner edge of OB VV (n/cm2/s)
    !! phi_n_IB_TFC      : output real : lifetime fluence at IB TF coil (n/cm2)
    !! phi_n_OB_TFC      : output real : lifetime fluence at OB TF coil (n/cm2)
    !! This routine calculates the fast neutron flux profiles within each
    !! blanket sub-assembly, assuming an exponential decay with distance through
    !! each region, with the decay indices dependent on the material fractions.
    !! <P>At present, the arrays contain only NP=2 elements, i.e. contain the
    !! values at the inner and outer radial locations; however, if required,
    !! they may be changed easily to provide several points for plotting
    !! purposes, for example.
    !! <P>The total neutron fluence over the plant lifetime reaching the
    !! TF coils is also calculated.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    real(8), intent(out) :: phi_n_VV_IB_start,phi_n_VV_OB_start
    real(8), intent(out) :: phi_n_IB_TFC, phi_n_OB_TFC

    ! Local variables
    integer, parameter :: K_tau = 31536000  ! [sec/yr] Number of seconds per year
    real(8), dimension(np) :: phi_n_BZ_IB, phi_n_BM_IB
    real(8), dimension(np) :: phi_n_BP_IB, phi_n_VV_IB
    real(8), dimension(np) :: phi_n_BZ_OB, phi_n_BM_OB
    real(8), dimension(np) :: phi_n_BP_OB, phi_n_VV_OB
    real(8) :: nwl_ratio

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    nwl_ratio = NWL_av/NWL_av_PPCS

    ! Inboard fast neutron flux profiles (n/cm2/second)
    ! Fast neutron flux profile in IB BZ
    phi_n_BZ_IB(:) = nwl_ratio * phi_0_n_BZ_IB * &
         exp(-x_BZ_IB(:)/lambda_n_BZ_IB)

    ! Fast neutron flux profile in IB BM
    phi_n_BM_IB(:) = phi_n_BZ_IB(np) * exp(-x_BM_IB(:)/lambda_q_BM_IB)

    ! Fast neutron flux profile in IB BP
    phi_n_BP_IB(:) = phi_n_BM_IB(np) * exp(-x_BP_IB(:)/lambda_q_BP_IB)

    ! Fast neutron flux profile in IB VV
    phi_n_VV_IB(:) = phi_n_BP_IB(np) * exp(-x_VV_IB(:)/lambda_n_VV)
    phi_n_vv_IB_start = phi_n_VV_IB(1)

    ! Fast neutron lifetime fluence at IB TF coil (n/cm2)
    phi_n_IB_TFC = phi_n_VV_IB(np) * t_plant * K_tau

    ! Outboard fast neutron flux profiles (n/cm2/second)
    ! Fast neutron flux profile in OB BZ
    phi_n_BZ_OB(:) = nwl_ratio * phi_0_n_BZ_OB * &
         exp(-x_BZ_OB(:)/lambda_n_BZ_OB)

    ! Fast neutron flux profile in OB BM
    phi_n_BM_OB(:) = phi_n_BZ_OB(np) * exp(-x_BM_OB(:)/lambda_q_BM_OB)

    ! Fast neutron flux profile in OB BP
    phi_n_BP_OB(:) = phi_n_BM_OB(np) * exp(-x_BP_OB(:)/lambda_q_BP_OB)

    ! Fast neutron flux profile in OB VV
    phi_n_VV_OB(:) = phi_n_BP_OB(np) * exp(-x_VV_OB(:)/lambda_n_VV)
    phi_n_vv_OB_start = phi_n_VV_OB(1)

    ! Fast neutron lifetime fluence at OB TF coil (n/cm2)
    phi_n_OB_TFC = phi_n_VV_OB(np) * t_plant * K_tau

  end subroutine fast_neutron_fluence

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine He_production_vacuum_vessel(phi_n_VV_IB_start,phi_n_VV_OB_start, &
       vvhemini,vvhemino,vvhemaxi,vvhemaxo)
    !! Calculates helium concentrations in the vacuum vessel at the end
    !! of the plant lifetime
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! phi_n_vv_IB_start : input real : n flux at inner edge of IB VV (n/cm2/s)
    !! phi_n_vv_OB_start : input real : n flux at inner edge of OB VV (n/cm2/s)
    !! vvhemini : output real : final He concentr. at outer edge of IB VV (appm)
    !! vvhemino : output real : final He concentr. at outer edge of OB VV (appm)
    !! vvhemaxi : output real : final He concentr. at inner edge of IB VV (appm)
    !! vvhemaxo : output real : final He concentr. at inner edge of OB VV (appm)
    !! This routine calculates the helium production profiles, and the
    !! minimum and maximum helium concentrations in the vacuum vessel
    !! at the end of the plant lifetime.
    !! <P>At present, the arrays contain only NP=2 elements, i.e. contain the
    !! values at the inner and outer radial locations; however, if required,
    !! they may be changed easily to provide several points for plotting
    !! purposes, for example.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    real(8), intent(in) :: phi_n_VV_IB_start,phi_n_VV_OB_start
    real(8), intent(out) :: vvhemini,vvhemino,vvhemaxi,vvhemaxo

    ! Local variables
    real(8), dimension(np) :: Gamma_He_IB, Gamma_He_OB
    real(8), dimension(np) :: C_He_IB, C_He_OB

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Helium production rate (appm/year)
    Gamma_He_IB(:) = phi_n_VV_IB_start / phi_n_0_VV_ref * &
         Gamma_He_0_ref * exp(-x_VV_IB(:)/lambda_He_VV)

    Gamma_He_OB(:) = phi_n_VV_OB_start / phi_n_0_VV_ref * &
         Gamma_He_0_ref * exp(-x_VV_OB(:)/lambda_He_VV)

    ! Helium concentrations at end of plant lifetime (appm)
    C_He_IB(:) = Gamma_He_IB(:) * t_plant
    C_He_OB(:) = Gamma_He_OB(:) * t_plant

    ! Minimum concentrations occur furthest from the plasma
    vvhemini = C_He_IB(np)
    vvhemino = C_He_OB(np)

    ! Maximum concentrations occur nearest the plasma
    vvhemaxi = C_He_IB(1)
    vvhemaxo = C_He_OB(1)

  end subroutine He_production_vacuum_vessel

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blanket_lifetime(t_bl_FPY,t_bl_Y)
    !! Calculates the blanket lifetime
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! t_bl_fpy : output real : blanket lifetime (full power years)
    !! t_bl_y   : output real : blanket lifetime (calendar years)
    !! This routine calculates the blanket lifetime, assuming that the
    !! maximum allowed neutron damage to the EUROFER steel is 60 dpa.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    real(8), intent(out) :: t_bl_FPY, t_bl_Y

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Lifetime in full-power years
    ! 10 dpa equates to 1 MW-yr/m2 (steel)
    t_bl_FPY = D_EU_max / (10.0D0*NWL_av*f_peak)

    ! Lifetime in calendar years, given availability and pulsed factors
    t_bl_Y = t_bl_FPY / (alpha_m*alpha_puls)

  end subroutine blanket_lifetime

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine component_masses
    !! Calculations for component masses
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculations for component masses
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: blnkith, blbuith, blbmith, blbpith, blnkoth, &
      blbuoth, blbmoth, blbpoth
    use fwbs_variables, only: iblnkith, whtblss, denstl, volblkti, &
      fblhebmi, fblhebpi, volblkto, fblhebmo, fblhebpo, fblss, whtblbe, fblbe, &
      whtblbreed, densbreed, fblbreed, volblkt, vfblkt, whtblkt

    implicit none

    ! Mass of steel in blanket (kg)
    if (iblnkith==1) then
        whtblss = denstl * ( volblkti/blnkith * ( blbuith * fblss + blbmith * (1.0D0-fblhebmi) + &
         blbpith * (1.0D0-fblhebpi) ) + volblkto/blnkoth * ( blbuoth * fblss + &
         blbmoth * (1.0D0-fblhebmo) + blbpoth * (1.0D0-fblhebpo) ) )
    else
        whtblss = denstl * ( volblkto/blnkoth * ( blbuoth * fblss + &
         blbmoth * (1.0D0-fblhebmo) + blbpoth * (1.0D0-fblhebpo) ) )
    end if

    ! Mass of beryllium in blanket (kg)
    if (iblnkith==1) then
        whtblbe = 1850.0D0 * fblbe * ( (volblkti * blbuith/blnkith) + &
          (volblkto * blbuoth/blnkoth) )
    else
        whtblbe = 1850.0D0 * fblbe * (volblkto * blbuoth/blnkoth)
    end if

    ! Mass of breeder material in blanket (kg)
    if (iblnkith==1) then
        whtblbreed = densbreed * fblbreed * ( (volblkti * blbuith/blnkith) + &
          (volblkto * blbuoth/blnkoth) )
    else
        whtblbreed = densbreed * fblbreed * (volblkto * blbuoth/blnkoth)
    end if

    ! Mass of blanket (kg)
    whtblkt = whtblss + whtblbe + whtblbreed

    ! Void fraction of blanket inboard portion
    if (iblnkith==1) then
        vfblkti = volblkti/volblkt * ( (blbuith/blnkith) * (1.0D0 - fblbe - fblbreed - fblss) &
           + (blbmith/blnkith) * fblhebmi + (blbpith/blnkith) * fblhebpi )
    else
        vfblkti = 0.0D0
    end if

    ! Void fraction of blanket outboard portion
    vfblkto = volblkto/volblkt * ( (blbuoth/blnkoth) * (1.0D0 - fblbe - fblbreed - fblss) &
       + (blbmoth/blnkoth) * fblhebmo + (blbpoth/blnkoth) * fblhebpo )

    ! Void fraction of blanket
    vfblkt = vfblkti + vfblkto

  end subroutine component_masses

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine component_volumes
    !! Calculate the blanket, shield, vacuum vessel and cryostat volumes
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the blanket, shield, vacuum vessel and cryostat volumes
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fwbs_variables, only: fwbsshape
    use physics_variables, only: itart

    implicit none

    ! Calculate blanket half-height
    call blanket_half_height

    ! Calculate shield half-height
    call shield_half_height

    ! Calculate vacuum vessel half-height
    call vv_half_height

    ! D-shaped blanket and shield
    if ((itart == 1).or.(fwbsshape == 1)) then

       call dshaped_blanket

       call dshaped_shield

       call dshaped_vv

    ! Elliptical blanket and shield
    else

     call elliptical_blanket

     call elliptical_shield

     call elliptical_vv

    end if

    ! Apply coverage factors to volumes and surface areas
    call apply_coverage_factors

    ! Calculate cryostat geometry
    call external_cryo_geometry

  end subroutine component_volumes

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blanket_half_height
    !! Calculate the blanket half-height
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the blanket half-height
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: vgap, blnktth, scrapli, scraplo, fwith, fwoth
    use physics_variables, only: rminor, kappa, idivrt
    use divertor_variables, only: divfix

    implicit none

    ! Local variables
    real(8) :: hbot, htop

    ! Calculate blanket internal lower half-height (m)
    hbot = rminor*kappa + vgap + divfix - blnktth

    ! If a double null machine then symmetric
    ! Calculate blanket internal upper half-height (m)
    if (idivrt == 2) then
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth)
    end if

    ! Average of top and bottom (m)
    hblnkt = 0.5D0*(htop + hbot)

  end subroutine blanket_half_height

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine shield_half_height
    !! Calculate the shield half-height
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the shield half-height
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: vgap, scrapli, scraplo, fwith, fwoth, blnktth
    use physics_variables, only: rminor, kappa, idivrt
    use divertor_variables, only: divfix

    implicit none

    ! Local variables
    real(8) :: hbot, htop

    ! Calculate shield internal lower half-height (m)
    hbot = rminor*kappa + vgap + divfix

    ! If a double null machine then symmetric
    ! Calculate shield internal upper half-height (m)
    if (idivrt == 2) then
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) + blnktth
    end if

    ! Average of top and bottom (m)
    hshld = 0.5D0*(htop + hbot)

  end subroutine shield_half_height

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vv_half_height
    !! Calculate the vacuum vessel half-height
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the vacuum vessel half-height
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: hmax, vgap2, d_vv_bot, d_vv_top, &
      scrapli, scraplo, fwith, fwoth, blnktth, shldtth
    use physics_variables, only: idivrt, rminor, kappa

    implicit none

    ! Local variables
    real(8) :: hbot, htop

    ! Calculate vacuum vessel internal lower half-height (m)
    hbot = hmax - vgap2 - d_vv_bot

    ! If a double null machine then symmetric
    ! Calculate vacuum vessel internal upper half-height (m)
    if (idivrt == 2) then
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) &
            + blnktth + shldtth
    end if

    ! Average of top and bottom (m)
    hvv = 0.5D0*(htop + hbot)

  end subroutine vv_half_height

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshaped_blanket
    !! Calculate the blanket surface area and volume using dshaped scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the blanket surface area and volume using dshaped scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use maths_library, only: dshellarea, dshellvol
    use build_variables, only: rsldi, shldith, blnkith, fwith, scrapli, scraplo, &
      fwoth, blareaib, blareaob, blarea, blnkoth, blnktth
    use fwbs_variables, only: volblkti, volblkto, volblkt
    use physics_variables, only: rminor

    implicit none

    ! Local variables
    real(8) :: r1, r2

    ! Major radius to outer edge of inboard blanket (m)
    r1 = rsldi + shldith + blnkith

    ! Horizontal distance between inside edges of blanket (m)
    ! i.e. outer radius of inboard part to inner radius of outboard part
    r2 = fwith + scrapli + 2.0D0*rminor + scraplo + fwoth

    ! Calculate blanket surface area, assuming 100% coverage
    call dshellarea(r1, r2, hblnkt, blareaib, blareaob, blarea)

    ! Calculate blanket volumes, assuming 100% coverage
    call dshellvol(r1, r2, hblnkt, blnkith, blnkoth, blnktth, volblkti, volblkto, volblkt)

  end subroutine dshaped_blanket

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshaped_shield
    !! Calculate the shield surface area and volume using dshaped scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the shield surface area and volume using dshaped scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use maths_library, only: dshellarea, dshellvol
    use build_variables, only: rsldi, shldith, blnkith, fwith, scrapli, scraplo, &
      fwoth, blnkoth, shareaib, shareaob, sharea, shldoth, shldtth
    use fwbs_variables, only: volshld
    use physics_variables, only: rminor

    implicit none

    ! Local variables
    real(8) :: r1, r2

    ! Major radius to outer edge of inboard shield (m)
    r1 = rsldi + shldith

    ! Horizontal distance between inside edges of shield (m)
    ! i.e. outer radius of inboard part to inner radius of outboard part
    r2 = blnkith + fwith + scrapli + 2.0D0*rminor + scraplo + fwoth + blnkoth

    ! Calculate shield surface area, assuming 100% coverage
    call dshellarea(r1, r2, hshld, shareaib, shareaob, sharea)

    ! Calculate shield volumes, assuming 100% coverage
    call dshellvol(r1, r2, hshld, shldith, shldoth, shldtth, volshldi, volshldo, volshld)

  end subroutine dshaped_shield

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshaped_vv
    !! Calculate the vacuum vessel volume using dshaped scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the vacuum vessel volume using dshaped scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use maths_library, only: dshellvol
    use build_variables, only: rsldi, rsldo, d_vv_in, d_vv_out, &
                               d_vv_top, d_vv_bot
    use fwbs_variables, only: vdewin, fvoldw

    implicit none

    ! Local variables
    real(8) :: r1, r2, v1, v2

    ! Major radius to outer edge of inboard section (m)
    r1 = rsldi

    ! Horizontal distance between inside edges (m)
    ! i.e. outer radius of inboard part to inner radius of outboard part
    r2 = rsldo - r1

    ! Calculate volume, assuming 100% coverage
    call dshellvol(r1, r2, hvv, d_vv_in, d_vv_out, &
                  (d_vv_top+d_vv_bot)/2, v1, v2, vdewin)

    ! Apply area coverage factor
    vdewin = fvoldw*vdewin

  end subroutine dshaped_vv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine elliptical_blanket
    !! Calculate the blanket surface area and volume using elliptical scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the blanket surface area and volume using elliptical scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use maths_library, only: eshellarea, eshellvol
    use build_variables, only: rsldi, shldith, blnkith, rsldo, shldoth, blnkoth, &
      blareaib, blareaob, blarea, blnktth
    use fwbs_variables, only: volblkti, volblkto, volblkt
    use physics_variables, only: rmajor, rminor, triang

    implicit none

    ! Local variables
    real(8) :: r1, r2, r3

    ! Major radius to centre of inboard and outboard ellipses (m)
    ! (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang

    ! Distance between r1 and outer edge of inboard blanket (m)
    r2 = r1 - (rsldi + shldith + blnkith)

    ! Distance between r1 and inner edge of outboard blanket (m)
    r3 = (rsldo - shldoth - blnkoth) - r1

    ! Calculate blanket surface area, assuming 100% coverage
    call eshellarea(r1, r2, r3, hblnkt, blareaib, blareaob, blarea)

    ! Calculate blanket volumes, assuming 100% coverage
    call eshellvol(r1, r2, r3, hblnkt, blnkith, blnkoth, blnktth, volblkti, volblkto, volblkt)

  end subroutine elliptical_blanket

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine elliptical_shield
    !! Calculate the shield surface area and volume using elliptical scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the shield surface area and volume using elliptical scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use maths_library, only: eshellarea, eshellvol
    use build_variables, only: rsldi, shldith, rsldo, shldoth, shareaib, &
      shareaob, sharea, shldtth
    use fwbs_variables, only: volshld
    use physics_variables, only: rmajor, rminor, triang
    
    implicit none

    ! Local variables
    real(8) :: r1, r2, r3

    ! Major radius to centre of inboard and outboard ellipses (m)
    ! (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang

    ! Distance between r1 and outer edge of inboard shield (m)
    r2 = r1 - (rsldi + shldith)

    ! Distance between r1 and inner edge of outboard shield (m)
    r3 = (rsldo - shldoth) - r1

    ! Calculate shield surface area, assuming 100% coverage
    call eshellarea(r1, r2, r3, hshld, shareaib, shareaob, sharea)

    ! Calculate shield volumes, assuming 100% coverage
    call eshellvol(r1, r2, r3, hshld, shldith, shldoth, shldtth, volshldi, volshldo, volshld)

  end subroutine elliptical_shield

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine elliptical_vv
    !! Calculate the vacuum vessel volume using elliptical scheme
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate the vacuum vessel volume using elliptical scheme
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use maths_library, only: eshellvol
    use build_variables, only: rsldi, rsldo, d_vv_in, d_vv_out, &
                               d_vv_top, d_vv_bot
    use fwbs_variables, only: vdewin, fvoldw
    use physics_variables, only: rmajor, rminor, triang

    implicit none

    ! Local variables
    real(8) :: r1, r2, r3, v1, v2

    ! Major radius to centre of inboard and outboard ellipses (m)
    ! (coincident in radius with top of plasma)
    r1 = rmajor - rminor*triang

    ! Distance between r1 and outer edge of inboard section (m)
    r2 = r1 - rsldi

    ! Distance between r1 and inner edge of outboard section (m)
    r3 = rsldo - r1

    ! Calculate volume, assuming 100% coverage
    call eshellvol(r1, r2, r3, hvv, d_vv_in, d_vv_out, &
                  (d_vv_top+d_vv_bot)/2, v1, v2, vdewin)

    ! Apply area coverage factor
    vdewin = fvoldw*vdewin

  end subroutine elliptical_vv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine apply_coverage_factors
    !! Apply coverage factors to volumes
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Apply coverage factors to volumes
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: blareaob, blarea, blareaib, shareaib, shareaob, &
      sharea
    use fwbs_variables, only: fdiv, fhcd, volblkto, volblkti, volblkt, fvolsi, &
      fvolso, volshld
    use physics_variables, only: idivrt

    implicit none

    ! Apply blanket coverage factors
    if (idivrt == 2) then
      ! double null configuration
      blareaob = blarea*(1.0D0-2.0D0*fdiv-fhcd) - blareaib
    else 
      ! single null configuration
      blareaob = blarea*(1.0D0-fdiv-fhcd) - blareaib
    end if
    
    blarea = blareaib + blareaob

    volblkto = volblkt*(1.0D0-fdiv-fhcd) - volblkti
    volblkt = volblkti + volblkto

    ! Apply shield coverage factors
    shareaib = fvolsi*shareaib
    shareaob = fvolso*shareaob
    sharea = shareaib + shareaob

    volshldi = fvolsi*volshldi
    volshldo = fvolso*volshldo
    volshld = volshldi + volshldo

  end subroutine apply_coverage_factors

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine external_cryo_geometry
    !! Calculate cryostat geometry
    !! author: J. Morris, CCFE, Culham Science Centre
    !! Calculate cryostat geometry
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    use build_variables, only: clhsf, hmax, tfcth, ddwex
    use fwbs_variables, only: rdewex, rpf2dewar, zdewex, vdewex, vvmass, vdewin, &
      denstl, dewmkg
    use pfcoil_variables, only: rb, zh
    use buildings_variables, only: clh1

    implicit none

    ! cryostat radius (m)
    ! ISSUE #508 Remove RFP option
    ! rb(i) = outer radius of PF coil i (tokamaks)
    rdewex = maxval(rb) + rpf2dewar

    ! Clearance between uppermost PF coil and cryostat lid (m).
    ! Scaling from ITER by M. Kovari
    hcryopf = clhsf * (2.0D0*rdewex)/28.440D0

    ! Half-height of cryostat (m)
    ! ISSUE #508 Remove RFP option
    zdewex = maxval(zh) + hcryopf

    ! Vertical clearance between TF coil and cryostat (m)
    clh1 = zdewex - (hmax + tfcth)

    ! cryostat volume (m3)
    vdewex = ( (2.0D0*pi*rdewex) * 2.0D0*zdewex + (2.0D0*pi*rdewex**2) ) * ddwex

    ! Vacuum vessel mass (kg)
    vvmass = vdewin * denstl

    ! Sum of internal vacuum vessel and cryostat masses (kg)
    dewmkg = (vdewin + vdewex) * denstl

  end subroutine external_cryo_geometry

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine write_kit_hcpb_output
    !! Write output to file for KIT HCPB model
    !! author: J. Morris, CCFE, Culham Science Centre
    !! This subroutine outputs the CCFE HCPB model results to
    !! an output file
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use fwbs_variables, only: pnucblkt, pnucshld, emult, npdiv, nphcdin, &
      nphcdout, hcdportsize, breedmat, li6enrich, tbr, tritprate, ptfnuc, bktlife
    use process_output, only: oheadr, osubhd, ovarre, ovarin, ocmmnt

    implicit none

    call oheadr(ofile, 'Blanket model output - KIT HCPB model')
    call osubhd(ofile, 'Blanket Composition :')
    call osubhd(ofile,'Blanket neutronics :')
    call ovarre(ofile,'Blanket heating (prior to energy multiplication) (MW)', &
      '(pnucblkt)', pnucblkt)
    call ovarre(ofile,'Shield heating (MW)','(pnucshld)',pnucshld)
    call ovarre(ofile,'Energy multiplication in blanket','(emult)',emult)
    call ovarin(ofile,'Number of divertor ports assumed','(npdiv)',npdiv)
    call ovarin(ofile,'Number of inboard H/CD ports assumed', '(nphcdin)', nphcdin)
    call ovarin(ofile,'Number of outboard H/CD ports assumed', '(nphcdout)', nphcdout)
    select case (hcdportsize)
    case (1)
       call ocmmnt(ofile,'     (small heating/current drive ports assumed)')
    case default
        call ocmmnt(ofile,'     (large heating/current drive ports assumed)')
    end select
    select case (breedmat)
       case (1)
          call ocmmnt(ofile,'Breeder material: Lithium orthosilicate (Li4Si04)')
       case (2)
          call ocmmnt(ofile,'Breeder material: Lithium methatitanate (Li2TiO3)')
       case (3)
          call ocmmnt(ofile,'Breeder material: Lithium zirconate (Li2ZrO3)')
       case default  !  shouldn't get here...
          call ocmmnt(ofile,'Unknown breeder material...')
    end select

    call ovarre(ofile,'Lithium-6 enrichment (%)','(li6enrich)',li6enrich)
    call ovarre(ofile,'Tritium breeding ratio','(tbr)',tbr)
    call ovarre(ofile,'Tritium production rate (g/day)','(tritprate)',tritprate)
    call ovarre(ofile,'Nuclear heating on i/b TF coil (MW/m3)','(pnuctfi)',pnuctfi)
    call ovarre(ofile,'Nuclear heating on o/b TF coil (MW/m3)','(pnuctfo)',pnuctfo)
    call ovarre(ofile,'Total nuclear heating on TF coil (MW)','(ptfnuc)',ptfnuc)
    call ovarre(ofile,'Fast neut. fluence on i/b TF coil (n/m2)', '(nflutfi)',nflutfi*1.0D4)
    call ovarre(ofile,'Fast neut. fluence on o/b TF coil (n/m2)', '(nflutfo)',nflutfo*1.0D4)
    call ovarre(ofile,'Minimum final He conc. in IB VV (appm)','(vvhemini)',vvhemini)
    call ovarre(ofile,'Minimum final He conc. in OB VV (appm)','(vvhemino)',vvhemino)
    call ovarre(ofile,'Maximum final He conc. in IB VV (appm)','(vvhemaxi)',vvhemaxi)
    call ovarre(ofile,'Maximum final He conc. in OB VV (appm)','(vvhemaxo)',vvhemaxo)
    call ovarre(ofile,'Blanket lifetime (full power years)','(t_bl_fpy)',bktlife)
    call ovarre(ofile,'Blanket lifetime (calendar years)','(t_bl_y)',t_bl_y)

  end subroutine write_kit_hcpb_output

end module kit_hcpb_module
