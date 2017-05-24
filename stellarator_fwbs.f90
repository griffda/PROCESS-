! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module kit_blanket_model

  !+ad_name  kit_blanket_model
  !+ad_summ  Module containing the KIT blanket model
  !+ad_summ  based on the HCPB concept
  !+ad_type  Module
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
  !+ad_desc  based on the Helium-Cooled Pebble Bed (HCPB) blanket concept
  !+ad_desc  of the PPCS Model B design.
  !+ad_prob  None
  !+ad_call  error_handling
  !+ad_hist  06/06/13 PJK Initial release for comments
  !+ad_hist  14/11/13 PJK Global replacement of BU by BZ (Unit --> Zone)
  !+ad_hist  17/02/14 PJK Used np instead of 2 for array dimensions
  !+ad_hist  30/06/14 PJK Added error_handling
  !+ad_stat  Okay
  !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
  !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
  !+ad_docc  Karlsruhe Institute of Technology, January 2013;
  !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use error_handling

  implicit none

  private
  public :: kit_blanket

  !  Local variables

  !  Radial coordinate arrays for the blanket sub-assemblies:
  !  BZ = Breeding Zone
  !  BM = Box Manifold
  !  BP = Back Plates
  !  VV = Vacuum Vessel (includes low-temperature shield)
  !  Element 1 = 'inner' edge, element np(=2) = 'outer' edge
  !
  !  IB = inboard, OB = outboard

  integer, parameter :: np = 2

  real(kind(1.0D0)), dimension(np) :: x_BZ_IB, x_BM_IB, x_BP_IB, x_VV_IB
  real(kind(1.0D0)), dimension(np) :: x_BZ_OB, x_BM_OB, x_BP_OB, x_VV_OB

  !  Values shared between subroutines in this module

  real(kind(1.0D0)) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
  real(kind(1.0D0)) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
  real(kind(1.0D0)) :: phi_n_vv_IB_start,phi_n_vv_OB_start

  !  Universal constants

  real(kind(1.0D0)), parameter :: E_n = 14.1D0    ! [MeV] Average neutron energy
  real(kind(1.0D0)), parameter :: PA_T = 3.0D0    ! [g/mol] Tritium atomic weight
  real(kind(1.0D0)), parameter :: N_Av = 6.02D23  ! [at/mol] Avogadro number

  !  Constants and fixed coefficients used in the model
  !  Based on Helium-Cooled Pebble Beds (HCPB) configuration
  !  of the PPCS Model B design

  real(kind(1.0D0)) :: A_cov_PPCS = 1365.0D0   ! [m^2] Total blanket coverage area
  real(kind(1.0D0)) :: A_FW_PPCS = 1253.0D0    ! [m^2] First wall area
  real(kind(1.0D0)) :: NWL_av_PPCS = 1.94D0    ! [MW/m^2] Average neutron wall load
  real(kind(1.0D0)) :: NWL_av_IB_PPCS = 1.73D0 ! [MW/m^2] Average IB wall load
  real(kind(1.0D0)) :: NWL_av_OB_PPCS = 1.92D0 ! [MW/m^2] Average OB wall load
  real(kind(1.0D0)) :: NWL_max_IB_PPCS = 1.99D0 ! [MW/m^2] Maximum IB wall load
  real(kind(1.0D0)) :: NWL_max_OB_PPCS = 2.41D0 ! [MW/m^2] Maximum OB wall load
  real(kind(1.0D0)) :: CF_bl_PPCS              ! [%] Blanket coverage factor (calculated)
  real(kind(1.0D0)) :: e_Li_PPCS = 30.0D0      ! [%] Li6 enrichment
  real(kind(1.0D0)) :: t_BZ_IB_PPCS = 36.5D0   ! [cm] IB Breeding Zone thickness
  real(kind(1.0D0)) :: t_BZ_OB_PPCS = 46.5D0   ! [cm] OB Breeding Zone thickness
  real(kind(1.0D0)) :: TBR_PPCS = 1.12D0       ! [--] Tritium Breeding Ratio
  
  ! not used...
  ! real(kind(1.0D0)) :: A_FW_IB_PPCS = 348.2D0  ! [m^2] IB first wall area
  ! real(kind(1.0D0)) :: A_FW_OB_PPCS = 905.6D0  ! [m^2] OB first wall area
  ! character(len=13) :: breeder_PPCS = 'Orthosilicate' ! Breeder type
  ! real(kind(1.0D0)) :: f_peak_PPCS = 1.21      ! [--] Neutron wall load peaking factor
  ! real(kind(1.0D0)) :: M_E_PPCS = 1.38D0       ! [--] Energy multiplication factor
  !real(kind(1.0D0)) :: t_HTS_IB_PPCS = 17.0D0  ! [cm] IB high temp. shield thickness
  !real(kind(1.0D0)) :: alpha_HTS_IB_PPCS = 40.0D0 ! [%] IB HTS helium fraction
  !real(kind(1.0D0)) :: t_HTS_OB_PPCS = 27.0D0  ! [cm] OB high temp. shield thickness
  !real(kind(1.0D0)) :: alpha_HTS_OB_PPCS = 40.0D0 ! [%] OB HTS helium fraction

  !  Power density pre-exponential terms and decay lengths

  real(kind(1.0D0)) :: q_0_BZ_breed_IB = 31.348D0 ! [W/cm^3] Pre-exp term in IB BZ breeder
  real(kind(1.0D0)) :: q_0_BZ_breed_OB = 37.144D0 ! [W/cm^3] Pre-exp term in OB BZ breeder
  real(kind(1.0D0)) :: lambda_q_BZ_breed_IB = 29.42D0 ! [cm] Decay length in IB BZ breeder
  real(kind(1.0D0)) :: lambda_q_BZ_breed_OB = 27.03D0 ! [cm] Decay length in OB BZ breeder

  real(kind(1.0D0)) :: q_0_BZ_Be_IB = 9.532D0 ! [W/cm^3] Pre-exp term in IB BZ Beryllium
  real(kind(1.0D0)) :: q_0_BZ_Be_OB = 11.809D0 ! [W/cm^3] Pre-exp term in OB BZ Beryllium
  real(kind(1.0D0)) :: lambda_q_BZ_Be_IB = 16.39D0 ! [cm] Decay length in IB BZ Beryllium
  real(kind(1.0D0)) :: lambda_q_BZ_Be_OB = 16.39D0 ! [cm] Decay length in OB BZ Beryllium

  real(kind(1.0D0)) :: q_0_BZ_steels_IB = 16.067D0 ! [W/cm^3] Pre-exp term in IB BZ steels
  real(kind(1.0D0)) :: q_0_BZ_steels_OB = 18.788D0 ! [W/cm^3] Pre-exp term in OB BZ steels
  real(kind(1.0D0)) :: lambda_q_BZ_steels_IB = 21.27D0 ! [cm] Decay length in IB BZ steels
  real(kind(1.0D0)) :: lambda_q_BZ_steels_OB = 21.27D0 ! [cm] Decay length in OB BZ steels

  real(kind(1.0D0)) :: lambda_EU = 11.57D0  ! [cm] Decay length in EUROFER
  real(kind(1.0D0)) :: lambda_q_BM_IB       ! [cm] Decay length in IB BM (calculated)
  real(kind(1.0D0)) :: lambda_q_BM_OB       ! [cm] Decay length in OB BM (calculated)
  real(kind(1.0D0)) :: lambda_q_BP_IB       ! [cm] Decay length in IB BP (calculated)
  real(kind(1.0D0)) :: lambda_q_BP_OB       ! [cm] Decay length in OB BP (calculated)
  real(kind(1.0D0)) :: lambda_q_VV = 6.92D0 ! [cm] Decay length in Vacuum Vessel

  !  Fast neutron flux pre-exponential terms and decay lengths

  real(kind(1.0D0)) :: phi_0_n_BZ_IB = 5.12D14  ! [n/cm^2/sec] Pre-exp term in IB BZ
  real(kind(1.0D0)) :: phi_0_n_BZ_OB = 5.655D14 ! [n/cm^2/sec] Pre-exp term in OB BZ
  real(kind(1.0D0)) :: lambda_n_BZ_IB = 18.79D0 ! [cm] Decay length in IB BZ
  real(kind(1.0D0)) :: lambda_n_BZ_OB = 19.19D0 ! [cm] Decay length in OB BZ
  real(kind(1.0D0)) :: lambda_n_VV = 8.153D0    ! [cm] Decay length in VV

  !  [n/cm^2/sec] Reference fast neutron flux on VV inner side [Fish09]

  real(kind(1.0D0)) :: phi_n_0_VV_ref = 2.0D10

  !  Vacuum vessel helium production pre-exponential terms and decay lengths

  real(kind(1.0D0)) :: Gamma_He_0_ref = 1.8D-3  ! [appm/yr] Pre-exp term
  real(kind(1.0D0)) :: lambda_He_VV = 7.6002D0  ! [cm] Decay length

  !  [dpa] Allowable neutron damage to the FW EUROFER

  real(kind(1.0D0)) :: D_EU_max = 60.0D0

  !  Variables used in this module, ultimately to be set via the calling routine
  !  to values given by PROCESS variables

  real(kind(1.0D0)), public :: P_n = 2720.0D0    ! [MW] Fusion neutron power
  real(kind(1.0D0)), public :: NWL_av = 1.94D0   ! [MW/m^2] Average neutron wall load
  real(kind(1.0D0)), public :: f_peak = 1.21D0   ! [--] NWL peaking factor
  real(kind(1.0D0)), public :: t_FW_IB = 2.3D0   ! [cm] IB first wall thickness
  real(kind(1.0D0)), public :: t_FW_OB = 2.3D0   ! [cm] OB first wall thickness
  real(kind(1.0D0)), public :: A_FW_IB = 3.5196D6 ! [cm^2] IB first wall area
  real(kind(1.0D0)), public :: A_FW_OB = 9.0504D6 ! [cm^2] OB first wall area
  real(kind(1.0D0)), public :: A_bl_IB = 3.4844D6 ! [cm^2] IB blanket area
  real(kind(1.0D0)), public :: A_bl_OB = 8.9599D6 ! [cm^2] OB blanket area
  real(kind(1.0D0)), public :: A_VV_IB = 3.8220D6 ! [cm^2] IB shield/VV area
  real(kind(1.0D0)), public :: A_VV_OB = 9.8280D6 ! [cm^2] OB shield/VV area
  real(kind(1.0D0)), public :: CF_bl = 91.7949D0 ! [%] Blanket coverage factor
  integer, public :: n_ports_div = 2             ! [ports] Number of divertor ports
  integer, public :: n_ports_H_CD_IB = 2         ! [ports] Number of IB H&CD ports
  integer, public :: n_ports_H_CD_OB = 2         ! [ports] Number of OB H&CD ports
  character(len=5), public :: H_CD_ports = 'small' ! Type of H&CD ports (small or large)
  real(kind(1.0D0)), public :: e_Li = 30.0D0     ! [%] Lithium 6 enrichment
  real(kind(1.0D0)), public :: t_plant = 40.0D0  ! [FPY] Plant lifetime
  real(kind(1.0D0)), public :: alpha_m = 0.75D0  ! [--] Availability factor
  real(kind(1.0D0)), public :: alpha_puls = 1.0D0 ! [--] Pulsed regime fraction

  !  Breeder type (allowed values are Orthosilicate, Metatitanate or Zirconate)

  character(len=20), public :: breeder = 'Orthosilicate'

  !  Inboard parameters

  real(kind(1.0D0)), public :: t_BZ_IB = 36.5D0     ! [cm] BZ thickness
  real(kind(1.0D0)), public :: t_BM_IB = 17.0D0     ! [cm] BM thickness
  real(kind(1.0D0)), public :: t_BP_IB = 30.0D0     ! [cm] BP thickness
  real(kind(1.0D0)), public :: t_VV_IB = 35.0D0     ! [cm] VV thickness
  real(kind(1.0D0)), public :: alpha_BM_IB = 40.0D0  ! [%] Helium fraction in the IB BM
  real(kind(1.0D0)), public :: alpha_BP_IB = 65.95D0 ! [%] Helium fraction in the IB BP
  real(kind(1.0D0)), public :: chi_Be_BZ_IB = 69.2D0 ! [%] Beryllium vol. frac. in IB BZ
  real(kind(1.0D0)), public :: chi_breed_BZ_IB = 15.4D0 ! [%] Breeder vol. frac. in IB BZ
  real(kind(1.0D0)), public :: chi_steels_BZ_IB = 9.8D0 ! [%] Steels vol. frac. in IB BZ

  !  Outboard parameters

  real(kind(1.0D0)), public :: t_BZ_OB = 46.5D0     ! [cm] BZ thickness
  real(kind(1.0D0)), public :: t_BM_OB = 27.0D0     ! [cm] BM thickness
  real(kind(1.0D0)), public :: t_BP_OB = 35.0D0     ! [cm] BP thickness
  real(kind(1.0D0)), public :: t_VV_OB = 65.0D0     ! [cm] VV thickness
  real(kind(1.0D0)), public :: alpha_BM_OB = 40.0D0  ! [%] Helium fraction in the OB BM
  real(kind(1.0D0)), public :: alpha_BP_OB = 67.13D0 ! [%] Helium fraction in the OB BP
  real(kind(1.0D0)), public :: chi_Be_BZ_OB = 69.2D0 ! [%] Beryllium vol. frac. in OB BZ
  real(kind(1.0D0)), public :: chi_breed_BZ_OB = 15.4D0 ! [%] Breeder vol. frac. in OB BZ
  real(kind(1.0D0)), public :: chi_steels_BZ_OB = 9.8D0 ! [%] Steels vol. frac. in OB BZ

  !  Model outputs

  real(kind(1.0D0)), public :: pnuctfi  ! [MW/m3] Nuclear heating on IB TF coil
  real(kind(1.0D0)), public :: pnuctfo  ! [MW/m3] Nuclear heating on OB TF coil
  real(kind(1.0D0)), public :: P_th_tot ! [MW] Nuclear power generated in blanket
  real(kind(1.0D0)), public :: pnucsh   ! [MW] Nuclear power generated in shield/VV
  real(kind(1.0D0)), public :: M_E      ! [--] Energy multiplication factor
  real(kind(1.0D0)), public :: tbratio  ! [--] Tritium breeding ratio
  real(kind(1.0D0)), public :: G_tot    ! [g/day] Tritium production rate
  real(kind(1.0D0)), public :: nflutfi  ! [n/cm2] Fast neutron fluence on IB TF coil
  real(kind(1.0D0)), public :: nflutfo  ! [n/cm2] Fast neutron fluence on OB TF coil
  real(kind(1.0D0)), public :: vvhemini ! [appm] minimum final He. conc in IB VV
  real(kind(1.0D0)), public :: vvhemino ! [appm] minimum final He. conc in OB VV
  real(kind(1.0D0)), public :: vvhemaxi ! [appm] maximum final He. conc in IB VV
  real(kind(1.0D0)), public :: vvhemaxo ! [appm] maximum final He. conc in OB VV
  real(kind(1.0D0)), public :: t_bl_fpy ! [y] blanket lifetime in full power years
  real(kind(1.0D0)), public :: t_bl_y   ! [y] blanket lifetime in calendar years

contains

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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: f_alpha

    !  Arguments

    real(kind(1.0D0)), intent(in) :: alpha

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    f_alpha = 1.0D0 + 0.019D0*alpha

  end function f_alpha

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine kit_blanket

    !+ad_name  kit_blanket
    !+ad_summ  Main routine for the KIT HCPB blanket model
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Franza, KIT (original MATLAB implementation)
    !+ad_cont  None
    !+ad_args  None
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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

  end subroutine kit_blanket

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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
    real(kind(1.0D0)), intent(out) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
    real(kind(1.0D0)), intent(out) :: pnuctfi, pnuctfo

    !  Local variables

    real(kind(1.0D0)), dimension(np) :: q_steels_BZ_IB, q_steels_BZ_OB
    real(kind(1.0D0)), dimension(np) :: q_BM_IB, q_BP_IB, q_VV_IB
    real(kind(1.0D0)), dimension(np) :: q_BM_OB, q_BP_OB, q_VV_OB

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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !+ad_docs  WP13-SYS01-A-T02 Interim Review Meeting, 10.07.2013, F. Franza
    !+ad_docc  (describes 26/09/2013 model refinement)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
    real(kind(1.0D0)), intent(in) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
    real(kind(1.0D0)), intent(out) :: P_th_tot, M_E, pnucsh

    !  Local variables

    real(kind(1.0D0)) :: A_BZ_breed_IB, A_BZ_breed_OB, A_BZ_Be_IB, A_BZ_Be_OB
    real(kind(1.0D0)) :: A_BZ_steels_IB, A_BZ_steels_OB
    real(kind(1.0D0)) :: P_BZ_breed_IB, P_BZ_Be_IB, P_BZ_steels_IB
    real(kind(1.0D0)) :: P_BZ_IB, P_BM_IB, P_BP_IB, P_VV_IB
    real(kind(1.0D0)) :: P_BZ_breed_OB, P_BZ_Be_OB, P_BZ_steels_OB
    real(kind(1.0D0)) :: P_BZ_OB, P_BM_OB, P_BP_OB, P_VV_OB
    real(kind(1.0D0)) :: P_tot_IB, P_tot_OB, P_n_FW

    real(kind(1.0D0)) :: nwl_ratio, nwl_IB_ratio_PPCS, nwl_OB_ratio_PPCS

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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !+ad_docs  WP13-SYS01-A-T02 Interim Review Meeting, 10.07.2013, F. Franza
    !+ad_docc  (describes 26/09/2013 model refinement)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: tbr, g_tot

    !  Local variables

    real(kind(1.0D0)) :: wib, wob
    real(kind(1.0D0)), parameter :: wib_PPCS = 0.28D0, wob_PPCS = 0.72D0

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
      !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !+ad_docc  Karlsruhe Institute of Technology, January 2013;
      !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: TBR_breed

      !  Arguments

      real(kind(1.0D0)), intent(in) :: e_Li
      character(len=*), intent(in) :: breeder

      !  Local variables

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
      !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !+ad_docc  Karlsruhe Institute of Technology, January 2013;
      !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: TBR_ports

      !  Arguments

      integer, intent(in) :: n_ports_div, n_ports_H_CD_IB, n_ports_H_CD_OB
      character(len=*), intent(in) :: H_CD_ports

      !  Local variables

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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: phi_n_VV_IB_start,phi_n_VV_OB_start
    real(kind(1.0D0)), intent(out) :: phi_n_IB_TFC, phi_n_OB_TFC

    !  Local variables

    integer, parameter :: K_tau = 31536000  ! [sec/yr] Number of seconds per year
    real(kind(1.0D0)), dimension(np) :: phi_n_BZ_IB, phi_n_BM_IB
    real(kind(1.0D0)), dimension(np) :: phi_n_BP_IB, phi_n_VV_IB
    real(kind(1.0D0)), dimension(np) :: phi_n_BZ_OB, phi_n_BM_OB
    real(kind(1.0D0)), dimension(np) :: phi_n_BP_OB, phi_n_VV_OB
    real(kind(1.0D0)) :: nwl_ratio

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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: phi_n_VV_IB_start,phi_n_VV_OB_start
    real(kind(1.0D0)), intent(out) :: vvhemini,vvhemino,vvhemaxi,vvhemaxo

    !  Local variables

    real(kind(1.0D0)), dimension(np) :: Gamma_He_IB, Gamma_He_OB
    real(kind(1.0D0)), dimension(np) :: C_He_IB, C_He_OB

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
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: t_bl_FPY, t_bl_Y

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Lifetime in full-power years
    !  10 dpa equates to 1 MW-yr/m2 (steel)

    t_bl_FPY = D_EU_max / (10.0D0*NWL_av*f_peak)

    !  Lifetime in calendar years, given availability and pulsed factors

    t_bl_Y = t_bl_FPY / (alpha_m*alpha_puls)

  end subroutine blanket_lifetime

end module kit_blanket_model

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module fwbs_module

  !+ad_name  fwbs_module
  !+ad_summ  Module containing first wall, blanket and shield routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  fwbs
  !+ad_cont  dshellvol
  !+ad_cont  eshellvol
  !+ad_cont  blanket
  !+ad_cont  blanket_neutronics
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of the first wall, blanket and shield components
  !+ad_desc  of a fusion power plant.
  !+ad_prob  None
  !+ad_call  build_module
  !+ad_call  build_variables
  !+ad_call  buildings_variables
  !+ad_call  constants
  !+ad_call  cost_variables
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  kit_blanket_model
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  plasma_geometry_module
  !+ad_call  process_output

  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  18/10/12 PJK Initial version of module
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Added cost_variables
  !+ad_hist  31/10/12 PJK Moved local common variables into module header
  !+ad_hist  06/11/12 PJK Added plasma_geometry_module
  !+ad_hist  09/04/13 PJK Added buildings_variables, pfcoil_variables,
  !+ad_hisc               stellarator_variables
  !+ad_hist  08/05/13 PJK Added dshellvol, eshellvol
  !+ad_hist  22/05/13 PJK Added kit_blanket_model, build_module, times_variables
  !+ad_hist  14/08/13 PJK Made blanket_neutronics public
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_module
  use build_variables
  use buildings_variables
  use constants
  use cost_variables
  use divertor_variables
  use fwbs_variables
  use heat_transport_variables
  use kit_blanket_model
  use maths_library
  use pfcoil_variables
  use physics_variables
  use plasma_geometry_module
  use process_output
  use refprop_interface

  use stellarator_variables
  use tfcoil_variables
  use times_variables

  implicit none

  private
  public :: fwbs, blanket_neutronics, tsat, sctfcoil_nuclear_heating_iter90

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fwbs(outfile,iprint)

    !+ad_name  fwbs
    !+ad_summ  First wall, blanket and shield module
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  C A Gardner, UKAEA Fusion
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_args  iprint : input integer : Switch to write output to file (1=yes)
    !+ad_desc  This subroutine calculates the nuclear heating in the blanket /
    !+ad_desc  shield, and estimates the volume and masses of the first wall,
    !+ad_desc  blanket and shield.
    !+ad_desc  <P>The arrays <CODE>coef(i,j)</CODE> and <CODE>decay(i,j)</CODE>
    !+ad_desc  are used for exponential decay approximations of the
    !+ad_desc  (superconducting) TF coil nuclear parameters.
    !+ad_desc  <UL><P><LI><CODE>j = 1</CODE> : stainless steel shield (assumed)
    !+ad_desc      <P><LI><CODE>j = 2</CODE> : tungsten shield (not used)</UL>
    !+ad_desc  Note: Costing and mass calculations elsewhere assume
    !+ad_desc  stainless steel only.
    !+ad_prob  None
    !+ad_call  blanket
    !+ad_call  blanket_neutronics
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_call  dshellarea
    !+ad_call  dshellvol
    !+ad_call  eshellarea
    !+ad_call  eshellvol
    !+ad_hist  14/11/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  09/04/13 PJK Replaced hardwired 5.0 by 2*clh1 in vdewex calculation;
    !+ad_hisc               rdewex transferred to fwbs_variables, and now uses
    !+ad_hisc               routr + rpf2dewar instead of hardwired value.
    !+ad_hisc               New cryomass calculation
    !+ad_hist  10/04/13 PJK Removed irrelevant vgap2 from ht1 calculation
    !+ad_hist  11/04/13 PJK Modified definition of hecan and wpthk;
    !+ad_hisc               modified beryllium density
    !+ad_hist  09/05/13 PJK Redefined blanket, shield and vacuum vessel volumes
    !+ad_hist  15/05/13 PJK Swapped build order of vacuum vessel and gap
    !+ad_hist  21/05/13 PJK Added blanket, shield area calculations
    !+ad_hist  18/06/13 PJK Corrected cryomass (= vacuum vessel mass, not cryostat mass)
    !+ad_hist  25/06/13 PJK Removed hecan output if blktmodel > 0
    !+ad_hist  16/08/13 PJK Removed obsolete stellarator clause (this routine
    !+ad_hisc               is no longer used for stellarators)
    !+ad_hist  24/04/14 PJK Changed bktlife output statement to avoid confusion
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  03/06/14 PJK Modified fhole etc. usage
    !+ad_hist  16/06/14 PJK Reworded pnucblkt output; removed duplicate outputs
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  23/06/14 PJK Corrected wallmw units
    !+ad_hist  03/09/14 PJK Changed PF coil to cryostat top vertical clearance
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(kind(1.0D0)), dimension(5) :: fact
    real(kind(1.0D0)), dimension(5,2) :: coef
    real(kind(1.0D0)), dimension(7,2) :: decay

    integer, parameter :: ishmat = 1  !  stainless steel coil casing is assumed

    real(kind(1.0D0)) :: coilhtmx,decaybl,dpacop,dshieq,dshoeq, &
         fpsdt,frachit,hbot,hblnkt,hcryopf,hecan,hshld,htop,htheci,hvv, &
         pheci,pheco, fpydt, pneut2,ptfi,ptfiwp,ptfo,ptfowp,r1,r2,r3, &
         raddose,v1,v2,volshldi,volshldo,wpthk,zdewex,coolvol

    real(kind(1.0D0)) :: pnucfwbs,pnucbs,pnucs
    real(kind(1.0D0)) :: fwthick,decayfw,decaysh

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Blanket and shield volumes and masses

    !  N.B. The blanket is not well-defined at top and bottom of the device,
    !  i.e. whether or not it actually extends into the divertor region
    !  is not clear (it is assumed to do so for the volume calculation
    !  if ipowerflow=0, but not if ipowerflow=1)...

    !  Internal half-height of blanket
    !  (average of above- and below-midplane parts)

    hbot = rminor*kappa + vgap + divfix - blnktth
    if (idivrt == 2) then  !  (i.e. snull=0)
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth)
    end if
    hblnkt = 0.5D0*(htop + hbot)

    !  Internal half-height of shield
    !  (average of above- and below-midplane parts)

    hbot = rminor*kappa + vgap + divfix
    if (idivrt == 2) then  !  (i.e. snull=0)
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) + blnktth
    end if
    hshld = 0.5D0*(htop + hbot)

    if ((itart == 1).or.(fwbsshape == 1)) then  !  D-shaped

       !  Major radius to outer edge of inboard blanket

       r1 = rsldi + shldith + blnkith

       !  Horizontal distance between inside edges of blanket,
       !  i.e. outer radius of inboard part to inner radius of outboard part

       r2 = fwith + scrapli + 2.0D0*rminor + scraplo + fwoth

       !  Calculate blanket surface area, assuming 100% coverage

       call dshellarea(r1,r2,hblnkt,blareaib,blareaob,blarea)

       !  Calculate blanket volumes, assuming 100% coverage

       call dshellvol(r1,r2,hblnkt,blnkith,blnkoth,blnktth, &
            volblkti,volblkto,volblkt)

       !  Major radius to outer edge of inboard shield

       r1 = rsldi + shldith

       !  Horizontal distance between inside edges of shield,
       !  i.e. outer radius of inboard part to inner radius of outboard part

       r2 = blnkith + fwith + scrapli + 2.0D0*rminor + scraplo + fwoth + blnkoth

       !  Calculate shield surface area, assuming 100% coverage

       call dshellarea(r1,r2,hshld,shareaib,shareaob,sharea)

       !  Calculate shield volumes, assuming 100% coverage

       call dshellvol(r1,r2,hshld,shldith,shldoth,shldtth, &
            volshldi,volshldo,volshld)

    else  !  Cross-sections are assumed to be defined by two ellipses

       !  Major radius to centre of inboard and outboard ellipses
       !  (coincident in radius with top of plasma)

       r1 = rmajor - rminor*triang

       !  Distance between r1 and outer edge of inboard blanket

       r2 = r1 - (rsldi + shldith + blnkith)

       !  Distance between r1 and inner edge of outboard blanket

       r3 = (rsldo - shldoth - blnkoth) - r1

       !  Calculate blanket surface area, assuming 100% coverage

       call eshellarea(r1,r2,r3,hblnkt,blareaib,blareaob,blarea)

        !  Calculate blanket volumes, assuming 100% coverage

       call eshellvol(r1,r2,r3,hblnkt,blnkith,blnkoth,blnktth, &
            volblkti,volblkto,volblkt)

       !  Distance between r1 and outer edge of inboard shield

       r2 = r1 - (rsldi + shldith)

       !  Distance between r1 and inner edge of outboard shield

       r3 = (rsldo - shldoth) - r1

       !  Calculate shield surface area, assuming 100% coverage

       call eshellarea(r1,r2,r3,hshld,shareaib,shareaob,sharea)

       !  Calculate shield volumes, assuming 100% coverage

       call eshellvol(r1,r2,r3,hshld,shldith,shldoth,shldtth, &
            volshldi,volshldo,volshld)

    end if

    !  Apply area (and volume) coverage factors

    if (ipowerflow == 0) then

       blareaib = (1.0D0-fhole) * blareaib
       blareaob = (1.0D0-fhole) * blareaob
       blarea = blareaib + blareaob

       volblkti = (1.0D0-fhole) * volblkti
       volblkto = (1.0D0-fhole) * volblkto
       volblkt = volblkti + volblkto

    else
       !  New power flow method uses different area fraction assumptions
       !  for the blanket

       blareaob = blarea*(1.0D0-fhole-fdiv-fhcd) - blareaib
       blarea = blareaib + blareaob

       volblkto = volblkt*(1.0D0-fhole-fdiv-fhcd) - volblkti
       volblkt = volblkti + volblkto

    end if

    shareaib = fvolsi*shareaib
    shareaob = fvolso*shareaob
    sharea = shareaib + shareaob

    volshldi = fvolsi*volshldi
    volshldo = fvolso*volshldo
    volshld = volshldi + volshldo

    !  Neutron power lost through holes in first wall

    pnucloss = pneutmw * fhole

    !  Blanket neutronics calculations

    if (blktmodel == 1) then

       call blanket_neutronics

       fpydt = cfactr * tlife

    else if (ipowerflow == 0) then

       !  TF coil nuclear heating parameters

       fact(1) = 8.0D0
       fact(2) = 8.0D0
       fact(3) = 6.0D0
       fact(4) = 4.0D0
       fact(5) = 4.0D0

       coef(1,1) = 10.3D0
       coef(2,1) = 11.6D0
       coef(3,1) = 7.08D5
       coef(4,1) = 2.19D18
       coef(5,1) = 3.33D-7
       coef(1,2) = 8.32D0
       coef(2,2) = 10.6D0
       coef(3,2) = 7.16D5
       coef(4,2) = 2.39D18
       coef(5,2) = 3.84D-7

       decay(1,1) = 10.05D0
       decay(2,1) = 17.61D0
       decay(3,1) = 13.82D0
       decay(4,1) = 13.24D0
       decay(5,1) = 14.31D0
       decay(6,1) = 13.26D0
       decay(7,1) = 13.25D0
       decay(1,2) = 10.02D0
       decay(2,2) = 3.33D0
       decay(3,2) = 15.45D0
       decay(4,2) = 14.47D0
       decay(5,2) = 15.87D0
       decay(6,2) = 15.25D0
       decay(7,2) = 17.25D0

       !  TART centrepost nuclear heating. Estimate fraction hitting from a
       !  point source at the plasma centre, and assume average path length
       !  of 2*tfcth, and e-fold decay length of 0.08m (copper water mixture).

       if (itart == 1) then
          frachit = hmax / sqrt(hmax**2 + (rmajor-tfcth)**2 ) * &
               atan(tfcth/(rmajor-tfcth) )/pi
          pnuccp = pneutmw * frachit * (1.0D0 - exp(-2.0D0*tfcth/0.08D0))
       else
          pnuccp = 0.0D0
       end if

       !  Energy-multiplied neutron power

       pneut2 = (pneutmw - pnucloss - pnuccp) * emult

       !  Nuclear heating in the blanket
       decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)

       pnucblkt = pneut2 * (1.0D0 - exp(-blnkoth/decaybl) )

       !  Nuclear heating in the shield

       pnucshld = pneut2 - pnucblkt

       !  Full power DT operation years for replacement of TF Coil
       !  (or Plant Life)

       fpydt = cfactr * tlife
       fpsdt = fpydt * 3.154D7

       !  Superconducting TF coil shielding calculations
       !  The 'He can' previously referred to is actually the steel case on the
       !  plasma-facing side of the TF coil.

       if (itfsup == 1) then

          !  N.B. The vacuum vessel appears to be ignored

          dshieq = shldith + fwith + blnkith
          dshoeq = shldoth + fwoth + blnkoth

          !  Case thickness on plasma-facing side of TF coil

          !hecan = 0.5D0*thkcas  !  Old calculation, assumes 1/2 average thickness
          hecan = casthi

          !  Winding pack radial thickness, including groundwall insulation

          !wpthk = tfcth - 1.5D0 * thkcas  !  Old calculation
          wpthk = thkwp + 2.0D0*tinstf

          !  Nuclear heating rate in inboard TF coil (MW/m**3)

          coilhtmx = fact(1) * wallmw * coef(1,ishmat) * &
               exp(-decay(6,ishmat) * (dshieq + hecan))

          !  Total nuclear heating (MW)

          ptfiwp = coilhtmx * tfsai * &
               (1.0D0-exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)
          ptfowp = fact(1) * wallmw * coef(1,ishmat) * &
               exp(-decay(6,ishmat) * (dshoeq + hecan)) * tfsao * &
               (1.0D0 - exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)

          !  Nuclear heating in plasma-side TF coil case (MW)

          htheci = fact(2) * wallmw * coef(2,ishmat) * &
               exp(-decay(7,ishmat) * dshieq)
          pheci = htheci * tfsai * (1.0D0-exp(-decay(2,ishmat)*hecan))/ &
               decay(2,ishmat)
          pheco = fact(2) * wallmw * coef(2,ishmat) * &
               exp(-decay(7,ishmat) * dshoeq) * tfsao * &
               (1.0D0-exp(-decay(2,ishmat)*hecan))/decay(2,ishmat)
          ptfi = ptfiwp + pheci
          ptfo = ptfowp + pheco
          ptfnuc = ptfi + ptfo

          !  Insulator dose (rad)

          raddose = coef(3,ishmat) * fpsdt * fact(3) * wallmw * &
               exp(-decay(3,ishmat) * (dshieq+hecan))

          !  Maximum neutron fluence in superconductor (n/m**2)

          nflutf = fpsdt * fact(4) * wallmw * coef(4,ishmat) * &
               exp(-decay(4,ishmat) * (dshieq+hecan))

          !  Atomic displacement in copper stabilizer

          dpacop = fpsdt * fact(5) * wallmw * coef(5,ishmat) * &
               exp(-decay(5,ishmat) * (dshieq + hecan) )

       else  !  Resistive TF coils
          dshieq = 0.0D0
          dshoeq = 0.0D0
          hecan = 0.0D0
          wpthk = 0.0D0
          coilhtmx = 0.0D0
          ptfiwp = 0.0D0
          ptfowp = 0.0D0
          htheci = 0.0D0
          pheci = 0.0D0
          pheco = 0.0D0
          ptfi = 0.0D0
          ptfo = 0.0D0
          ptfnuc = 0.0D0
          raddose = 0.0D0
          nflutf = 0.0D0
          dpacop = 0.0D0
       end if

    else  !  ipowerflow == 1

       !  TART centrepost nuclear heating. Estimate fraction hitting from a
       !  point source at the plasma centre, and assume average path length
       !  of 2*tfcth, and e-fold decay length of 0.08m (copper water mixture).

       if (itart == 1) then
          frachit = hmax / sqrt(hmax**2 + (rmajor-tfcth)**2 ) * &
               atan(tfcth/(rmajor-tfcth) )/pi
          pnuccp = pneutmw * frachit * (1.0D0 - exp(-2.0D0*tfcth/0.08D0))
       else
          pnuccp = 0.0D0
       end if

       !  Neutron power incident on divertor

       pnucdiv = pneutmw * fdiv

       !  Neutron power incident on HCD apparatus

       pnuchcd = pneutmw * fhcd

       !  Neutron power deposited in first wall, blanket and shield

       pnucfwbs = pneutmw - pnucdiv - pnucloss - pnuccp - pnuchcd

       !  Neutron power deposited in first wall

       fwthick = 0.5D0*(fwith+fwoth)
       decayfw = declfw / (1.0D0-fwclfr)
       pnucfw = pnucfwbs * (1.0D0 - exp(-fwthick/decayfw))

       !  Neutron power reaching blanket and shield

       pnucbs = pnucfwbs - pnucfw

       !  Neutron power deposited in the blanket
       decaybl = declblkt / (1.0D0 - vfblkt - fblli2o - fblbe)

       pnucblkt = pnucbs * (1.0D0 - exp(-blnkoth/decaybl))

       !  Neutron power reaching shield

       pnucs = pnucbs - pnucblkt

       !  Neutron power deposited in the shield

       decaysh = declshld / (1.0D0 - vfshld)
       pnucshld = pnucs * (1.0D0 - exp(-shldoth/decaysh))

       !  Full power DT operation years for replacement of TF Coil
       !  (or Plant Life)

       fpydt = cfactr * tlife
       fpsdt = fpydt * 3.154D7

       !  Superconducting TF coil shielding calculations
       !  The 'He can' previously referred to is actually the steel case on the
       !  plasma-facing side of the TF coil.

       if (itfsup == 1) then

          !  N.B. The vacuum vessel appears to be ignored

          !+PJK  First draft: replace all previous code with the following...

          dshieq = 0.0D0
          dshoeq = 0.0D0
          hecan = 0.0D0
          wpthk = 0.0D0
          coilhtmx = 0.0D0
          ptfiwp = 0.0D0
          ptfowp = 0.0D0
          htheci = 0.0D0
          pheci = 0.0D0
          pheco = 0.0D0
          ptfi = 0.0D0
          ptfo = 0.0D0
          raddose = 0.0D0
          nflutf = 0.0D0
          dpacop = 0.0D0

          ptfnuc = pnucs - pnucshld

          ! !  old coding follows...
          ! fact(1) = 8.0D0
          ! fact(2) = 8.0D0
          ! fact(3) = 6.0D0
          ! fact(4) = 4.0D0
          ! fact(5) = 4.0D0

          ! coef(1,1) = 10.3D0
          ! coef(2,1) = 11.6D0
          ! coef(3,1) = 7.08D5
          ! coef(4,1) = 2.19D18
          ! coef(5,1) = 3.33D-7
          ! coef(1,2) = 8.32D0
          ! coef(2,2) = 10.6D0
          ! coef(3,2) = 7.16D5
          ! coef(4,2) = 2.39D18
          ! coef(5,2) = 3.84D-7

          ! decay(1,1) = 10.05D0
          ! decay(2,1) = 17.61D0
          ! decay(3,1) = 13.82D0
          ! decay(4,1) = 13.24D0
          ! decay(5,1) = 14.31D0
          ! decay(6,1) = 13.26D0
          ! decay(7,1) = 13.25D0
          ! decay(1,2) = 10.02D0
          ! decay(2,2) = 3.33D0
          ! decay(3,2) = 15.45D0
          ! decay(4,2) = 14.47D0
          ! decay(5,2) = 15.87D0
          ! decay(6,2) = 15.25D0
          ! decay(7,2) = 17.25D0

          ! dshieq = shldith + fwith + blnkith
          ! dshoeq = shldoth + fwoth + blnkoth

          ! !  Case thickness on plasma-facing side of TF coil

          ! !hecan = 0.5D0*thkcas  !  Old calculation, assumes 1/2 average thickness
          ! hecan = casthi

          ! !  Winding pack radial thickness, including groundwall insulation

          ! !wpthk = tfcth - 1.5D0 * thkcas  !  Old calculation
          ! wpthk = thkwp + 2.0D0*tinstf

          ! !  Nuclear heating rate in inboard TF coil (MW/m**3)

          ! coilhtmx = fact(1) * wallmw * coef(1,ishmat) * &
          !      exp(-decay(6,ishmat) * (dshieq + hecan))

          ! !  Total nuclear heating (MW)

          ! ptfiwp = coilhtmx * tfsai * &
          !      (1.0D0-exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)
          ! ptfowp = fact(1) * wallmw * coef(1,ishmat) * &
          !      exp(-decay(6,ishmat) * (dshoeq + hecan)) * tfsao * &
          !      (1.0D0 - exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)

          ! !  Nuclear heating in plasma-side TF coil case (MW)

          ! htheci = fact(2) * wallmw * coef(2,ishmat) * &
          !      exp(-decay(7,ishmat) * dshieq)
          ! pheci = htheci * tfsai * (1.0D0-exp(-decay(2,ishmat)*hecan))/ &
          !      decay(2,ishmat)
          ! pheco = fact(2) * wallmw * coef(2,ishmat) * &
          !      exp(-decay(7,ishmat) * dshoeq) * tfsao * &
          !      (1.0D0-exp(-decay(2,ishmat)*hecan))/decay(2,ishmat)
          ! ptfi = ptfiwp + pheci
          ! ptfo = ptfowp + pheco
          ! ptfnuc = ptfi + ptfo

          ! !  Insulator dose (rad)

          ! raddose = coef(3,ishmat) * fpsdt * fact(3) * wallmw * &
          !      exp(-decay(3,ishmat) * (dshieq+hecan))

          ! !  Maximum neutron fluence in superconductor (n/m**2)

          ! nflutf = fpsdt * fact(4) * wallmw * coef(4,ishmat) * &
          !      exp(-decay(4,ishmat) * (dshieq+hecan))

          ! !  Atomic displacement in copper stabilizer

          ! dpacop = fpsdt * fact(5) * wallmw * coef(5,ishmat) * &
          !      exp(-decay(5,ishmat) * (dshieq + hecan) )

       else  !  Resistive TF coils
          dshieq = 0.0D0
          dshoeq = 0.0D0
          hecan = 0.0D0
          wpthk = 0.0D0
          coilhtmx = 0.0D0
          ptfiwp = 0.0D0
          ptfowp = 0.0D0
          htheci = 0.0D0
          pheci = 0.0D0
          pheco = 0.0D0
          ptfi = 0.0D0
          ptfo = 0.0D0
          ptfnuc = 0.0D0
          raddose = 0.0D0
          nflutf = 0.0D0
          dpacop = 0.0D0
       end if

    end if ! blktmodel = 0

    !  Divertor mass

    divsur = fdiva * 2.0D0 * pi * rmajor * rminor
    if (idivrt == 2) divsur = divsur * 2.0D0
    divmas = divsur * divdens * (1.0D0 - divclfr) * divplt

    !  Start adding components of the coolant mass:
    !  Divertor coolant volume (m3)

    coolvol = divsur * divclfr * divplt

    !  Blanket mass, excluding coolant

    if (blktmodel == 0) then
       whtblss = volblkt * denstl * fblss
       whtblbe = volblkt * 1850.0D0  * fblbe  !  density modified from 1900 kg/m3
       whtblvd = volblkt * 5870.0D0  * fblvd
       wtblli2o = volblkt * 2010.0D0  * fblli2o
       whtblkt = whtblss + whtblvd + wtblli2o + whtblbe
    else  !  volume fractions proportional to sub-assembly thicknesses
       whtblss = denstl * ( &
            volblkti/blnkith * ( &
            blbuith * fblss + &
            blbmith * (1.0D0-fblhebmi) + &
            blbpith * (1.0D0-fblhebpi) ) &
            + volblkto/blnkoth * ( &
            blbuoth * fblss + &
            blbmoth * (1.0D0-fblhebmo) + &
            blbpoth * (1.0D0-fblhebpo) ) )
       whtblbe = 1850.0D0 * fblbe * ( &
            (volblkti * blbuith/blnkith) + (volblkto * blbuoth/blnkoth) )
       whtblbreed = densbreed * fblbreed * ( &
            (volblkti * blbuith/blnkith) + (volblkto * blbuoth/blnkoth) )
       whtblkt = whtblss + whtblbe + whtblbreed

       vfblkt = volblkti/volblkt * ( &  !  inboard portion
            (blbuith/blnkith) * (1.0D0 - fblbe - fblbreed - fblss) &
            + (blbmith/blnkith) * fblhebmi &
            + (blbpith/blnkith) * fblhebpi )
       vfblkt = vfblkt + volblkto/volblkt * ( &  !  outboard portion
            (blbuoth/blnkoth) * (1.0D0 - fblbe - fblbreed - fblss) &
            + (blbmoth/blnkoth) * fblhebmo &
            + (blbpoth/blnkoth) * fblhebpo )

    end if

    whtshld = volshld * denstl * (1.0D0 - vfshld)

    !  Blanket coolant is assumed to be helium for the models used
    !  when blktmodel > 0

    if (blktmodel == 0) then
       coolvol = coolvol + volblkt*vfblkt
    end if

    !  Penetration shield (set = internal shield)

    wpenshld = whtshld
    coolvol = coolvol + volshld*vfshld

    !  First wall mass
    !  (first wall area is calculated elsewhere)

    fwmass = fwarea * (fwith+fwoth)/2.0D0 * denstl * (1.0D0-fwclfr)

    !  Surface areas adjacent to plasma

    coolvol = coolvol + fwarea * (fwith+fwoth)/2.0D0 * fwclfr

    !  Mass of coolant = volume * density at typical coolant
    !  temperatures and pressures
    !  N.B. for blktmodel > 0, mass of helium coolant in blanket is ignored...
    if (blktmodel > 0) then  !  pressurised water coolant
       coolmass = coolvol*806.719D0
    else  !  gaseous helium coolant
       coolmass = coolvol*1.517D0
    end if

    !  External cryostat radius (m)
    !  rb(i) = outer radius of PF coil i (tokamaks)
    rdewex = maxval(rb) + rpf2dewar
    
    !  Clearance between uppermost PF coil and cryostat lid
    !  Scaling from ITER by M. Kovari

    hcryopf = clhsf * (2.0D0*rdewex)/28.440D0

    !  Half-height of cryostat
    zdewex = maxval(zh) + hcryopf

    !  Vertical clearance between TF coil and cryostat

    clh1 = zdewex - (hmax + tfcth)

    !  External cryostat volume

    vdewex = ( (2.0D0*pi*rdewex) * 2.0D0*zdewex + &
         (2.0D0*pi*rdewex**2) ) * ddwex

    !  Internal vacuum vessel volume

    !  Original method:
    !  Factor of 2 to account for outside part of TF coil
    !  fvoldw accounts for ports, support, etc. additions
    !
    !vdewin = (2.0D0*(2.0D0*hmax) + 2.0D0 * (rsldo-rsldi)) * &
    !     2.0D0 * pi * rmajor * ddwi * 2.0D0 * fvoldw

    hbot = hmax - vgap2 - ddwi
    if (idivrt == 2) then  !  (i.e. snull=0)
       htop = hbot
    else
       htop = rminor*kappa + 0.5D0*(scrapli+scraplo + fwith+fwoth) &
            + blnktth + shldtth
    end if
    hvv = 0.5D0*(htop + hbot)

    if ((itart == 1).or.(fwbsshape == 1)) then  !  D-shaped

       !  Major radius to outer edge of inboard section

       r1 = rsldi

       !  Horizontal distance between inside edges,
       !  i.e. outer radius of inboard part to inner radius of outboard part

       r2 = rsldo - r1

       !  Calculate volume, assuming 100% coverage

       call dshellvol(r1,r2,hvv,ddwi,ddwi,ddwi,v1,v2,vdewin)

       !  Apply area coverage factor

       vdewin = fvoldw*vdewin

    else  !  Cross-section is assumed to be defined by two ellipses

       !  Major radius to centre of inboard and outboard ellipses
       !  (coincident in radius with top of plasma)

       r1 = rmajor - rminor*triang

       !  Distance between r1 and outer edge of inboard section

       r2 = r1 - rsldi

       !  Distance between r1 and inner edge of outboard section

       r3 = rsldo - r1

       !  Calculate volume, assuming 100% coverage

       call eshellvol(r1,r2,r3,hvv,ddwi,ddwi,ddwi,v1,v2,vdewin)

       !  Apply area coverage factor

       vdewin = fvoldw*vdewin

    end if

    !  Vacuum vessel mass - original obscure calculation replaced

    !cryomass = fvolcry * 4.0D0 * (2.0D0*(rtot-rsldi) + 2.0D0*hmax) * &
    !     2.0D0 * pi * rmajor * ddwi * denstl

    vvmass = vdewin * denstl

    !  Sum of internal vacuum vessel and external cryostat masses

    dewmkg = (vdewin + vdewex) * denstl

    if (iprint == 0) return

    !  Output section

    call oheadr(outfile,'Shield / Blanket')
    call ovarre(outfile,'Average neutron wall load (MW/m2)','(wallmw)', wallmw)
    if (blktmodel > 0) then
       call ovarre(outfile,'Neutron wall load peaking factor','(wallpf)', wallpf)
    end if
    call ovarre(outfile,'DT full power TF coil operation (yrs)', &
         '(fpydt)',fpydt)
    if (blktmodel > 0) then
       call ovarre(outfile,'Inboard breeding zone thickness (m)','(blbuith)', blbuith)
       call ovarre(outfile,'Inboard box manifold thickness (m)','(blbmith)', blbmith)
       call ovarre(outfile,'Inboard back plate thickness (m)','(blbpith)', blbpith)
    end if
    if (blktmodel > 0) then
       call ovarre(outfile,'Outboard breeding zone thickness (m)','(blbuoth)', blbuoth)
       call ovarre(outfile,'Outboard box manifold thickness (m)','(blbmoth)', blbmoth)
       call ovarre(outfile,'Outboard back plate thickness (m)','(blbpoth)', blbpoth)
    end if
    if (blktmodel == 0) &
         call ovarre(outfile,'Inboard side TF coil case thickness (m)', &
         '(hecan)',hecan)

    if (itart == 1) then
       call osubhd(outfile,'(Copper centrepost used)')
       call ovarre(outfile,'Centrepost heating (MW)','(pnuccp)',pnuccp)
    else if (blktmodel == 0) then
       call osubhd(outfile,'TF coil nuclear parameters :')
       call ovarre(outfile,'Peak magnet heating (MW/m3)','(coilhtmx)', &
            coilhtmx)
       call ovarre(outfile,'Inboard TF coil winding pack heating (MW)', &
            '(ptfiwp)',ptfiwp)
       call ovarre(outfile,'Outboard TF coil winding pack heating (MW)', &
            '(ptfowp)',ptfowp)
       call ovarre(outfile,'Peak TF coil case heating (MW/m3)','(htheci)', &
            htheci)
       call ovarre(outfile,'Inboard coil case heating (MW)','(pheci)',pheci)
       call ovarre(outfile,'Outboard coil case heating (MW)','(pheco)',pheco)
       call ovarre(outfile,'Insulator dose (rad)','(raddose)',raddose)
       call ovarre(outfile,'Maximum neutron fluence (n/m2)','(nflutf)', &
            nflutf)
       call ovarre(outfile,'Copper stabiliser displacements/atom', &
            '(dpacop)',dpacop)
    end if

    if (blktmodel == 0) then
       call osubhd(outfile,'Nuclear heating :')
       call ovarre(outfile,'Blanket heating (prior to energy multiplication) (MW)', &
            '(pnucblkt)',pnucblkt)
       call ovarre(outfile,'Shield heating (MW)','(pnucshld)',pnucshld)
    else
       call osubhd(outfile,'Blanket neutronics :')
       call ovarre(outfile,'Blanket heating (prior to energy multiplication) (MW)', &
            '(pnucblkt)',pnucblkt)
       call ovarre(outfile,'Shield heating (MW)','(pnucshld)',pnucshld)
       call ovarre(outfile,'Energy multiplication in blanket','(emult)',emult)
       call ovarin(outfile,'Number of divertor ports assumed','(npdiv)',npdiv)
       call ovarin(outfile,'Number of inboard H/CD ports assumed', &
            '(nphcdin)',nphcdin)
       call ovarin(outfile,'Number of outboard H/CD ports assumed', &
            '(nphcdout)',nphcdout)
       select case (hcdportsize)
       case (1)
          call ocmmnt(outfile,'     (small heating/current drive ports assumed)')
       case default
          call ocmmnt(outfile,'     (large heating/current drive ports assumed)')
       end select
       select case (breedmat)
       case (1)
          call ocmmnt(outfile,'Breeder material: Lithium orthosilicate (Li4Si04)')
       case (2)
          call ocmmnt(outfile,'Breeder material: Lithium methatitanate (Li2TiO3)')
       case (3)
          call ocmmnt(outfile,'Breeder material: Lithium zirconate (Li2ZrO3)')
       case default  !  shouldn't get here...
          call ocmmnt(outfile,'Unknown breeder material...')
       end select
       call ovarre(outfile,'Lithium-6 enrichment (%)','(li6enrich)',li6enrich)
       call ovarre(outfile,'Tritium breeding ratio','(tbr)',tbr)
       call ovarre(outfile,'Tritium production rate (g/day)','(tritprate)',tritprate)
       call ovarre(outfile,'Nuclear heating on i/b TF coil (MW/m3)','(pnuctfi)',pnuctfi)
       call ovarre(outfile,'Nuclear heating on o/b TF coil (MW/m3)','(pnuctfo)',pnuctfo)
       call ovarre(outfile,'Total nuclear heating on TF coil (MW)','(ptfnuc)',ptfnuc)
       call ovarre(outfile,'Fast neut. fluence on i/b TF coil (n/m2)', &
            '(nflutfi)',nflutfi*1.0D4)
       call ovarre(outfile,'Fast neut. fluence on o/b TF coil (n/m2)', &
            '(nflutfo)',nflutfo*1.0D4)
       call ovarre(outfile,'Minimum final He conc. in IB VV (appm)','(vvhemini)',vvhemini)
       call ovarre(outfile,'Minimum final He conc. in OB VV (appm)','(vvhemino)',vvhemino)
       call ovarre(outfile,'Maximum final He conc. in IB VV (appm)','(vvhemaxi)',vvhemaxi)
       call ovarre(outfile,'Maximum final He conc. in OB VV (appm)','(vvhemaxo)',vvhemaxo)
       call ovarre(outfile,'Blanket lifetime (full power years)','(t_bl_fpy)',bktlife)
       call ovarre(outfile,'Blanket lifetime (calendar years)','(t_bl_y)',t_bl_y)
    end if

    call osubhd(outfile,'Blanket / shield volumes and weights :')

    if (blktmodel == 0) then
       write(outfile,600) volblkti, volblkto, volblkt, whtblkt, vfblkt, &
            fblbe, whtblbe, fblli2o, wtblli2o, fblss, whtblss, fblvd, &
            whtblvd, volshldi, volshldo, volshld, whtshld, vfshld, &
            wpenshld
    else
       write(outfile,602) volblkti, volblkto, volblkt, whtblkt, vfblkt, &
            (volblkti/volblkt * blbuith/blnkith + &
            volblkto/volblkt * blbuoth/blnkoth) * fblbe, whtblbe, &
            (volblkti/volblkt * blbuith/blnkith + &
            volblkto/volblkt * blbuoth/blnkoth) * fblbreed, whtblbreed, &
            volblkti/volblkt/blnkith * (blbuith * fblss &
            + blbmith * (1.0D0-fblhebmi) + blbpith * (1.0D0-fblhebpi)) + &
            volblkto/volblkt/blnkoth * (blbuoth * fblss &
            + blbmoth * (1.0D0-fblhebmo) + blbpoth * (1.0D0-fblhebpo)), &
            whtblss, &
            volshldi, volshldo, volshld, whtshld, vfshld, wpenshld
    end if

600 format( &
         t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
         t32,'-----------',t45,'------------',t62,'-----------'/ &
         '    Inboard blanket' ,t32,1pe10.3,/ &
         '    Outboard blanket' ,t32,1pe10.3,/ &
         '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '       Blanket Be   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket Li2O ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/ &
         '    Inboard shield'  ,t32,1pe10.3,/ &
         '    Outboard shield'  ,t32,1pe10.3,/ &
         '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '    Penetration shield'        ,t62,1pe10.3)

! 601 format( &
!          t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
!          t32,'-----------',t45,'------------',t62,'-----------'/ &
!          '    Inboard blanket' ,t32,1pe10.3,/ &
!          '    Outboard blanket' ,t32,1pe10.3,/ &
!          '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
!          '       Void fraction' ,t45,1pe10.3,/ &
!          '       Blanket LiPb ',t45,1pe10.3,t62,1pe10.3/ &
!          '       Blanket Li   ',t45,1pe10.3,t62,1pe10.3/ &
!          '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/ &
!          '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/ &
!          '    Inboard shield'  ,t32,1pe10.3,/ &
!          '    Outboard shield'  ,t32,1pe10.3,/ &
!          '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
!          '       Void fraction' ,t45,1pe10.3,/ &
!          '    Penetration shield'        ,t62,1pe10.3)

602 format( &
         t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
         t32,'-----------',t45,'------------',t62,'-----------'/ &
         '    Inboard blanket' ,t32,1pe10.3,/ &
         '    Outboard blanket' ,t32,1pe10.3,/ &
         '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '       Blanket Be   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket breeder',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket steel',t45,1pe10.3,t62,1pe10.3/ &
         '    Inboard shield'  ,t32,1pe10.3,/ &
         '    Outboard shield'  ,t32,1pe10.3,/ &
         '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '    Penetration shield'        ,t62,1pe10.3)

    call osubhd(outfile,'Other volumes, masses and areas :')
    call ovarre(outfile,'First wall area (m2)','(fwarea)',fwarea)
    call ovarre(outfile,'First wall mass (kg)','(fwmass)',fwmass)
    call ovarre(outfile,'External cryostat radius (m)','(rdewex)',rdewex)
    call ovarre(outfile,'External cryostat half-height (m)','(zdewex)',zdewex)
    call ovarre(outfile,'External cryostat volume (m3)','(vdewex)',vdewex)
    call ovarre(outfile,'Total cryostat + vacuum vessel mass (kg)','(dewmkg)',dewmkg)
    call ovarre(outfile,'Internal vacuum vessel volume (m3)','(vdewin)',vdewin)
    call ovarre(outfile,'Vacuum vessel mass (kg)','(vvmass)',vvmass)
    call ovarre(outfile,'Divertor area (m2)','(divsur)',divsur)
    call ovarre(outfile,'Divertor mass (kg)','(divmas)',divmas)

  end subroutine fwbs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blanket_neutronics

    !+ad_name  blanket_neutronics
    !+ad_summ  Interface between PROCESS and the KIT HCPB blanket model
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This routine provides the interface between the KIT HCPB
    !+ad_desc  blanket neutronics model and the rest of the code.
    !+ad_prob  None
    !+ad_call  kit_blanket
    !+ad_hist  06/06/13 PJK Initial release
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  03/06/14 PJK Changed fhole usage for ipowerflow=1
    !+ad_stat  Okay
    !+ad_docs  FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !+ad_docc  for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !+ad_docc  Karlsruhe Institute of Technology, January 2013;
    !+ad_docc  EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    if (ipowerflow == 0) then
       CF_bl = (1.0D0-fhole) * 100.0D0 ! [%] Blanket coverage factor
    else
       CF_bl = (1.0D0-fhole-fhcd-fdiv) * 100.0D0 ! [%] Blanket coverage factor
    end if
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

    call kit_blanket

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

  end subroutine blanket_neutronics

  function tsat(p)

    !+ad_name  tsat
    !+ad_summ  Saturation temperature of water as a function of pressure
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  P Karditsas, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  p  : input real : saturated liquid/steam pressure (MPa)
    !+ad_desc  This routine calculates the saturation temperature (K) of
    !+ad_desc  water given the pressure. The calculation is performed
    !+ad_desc  either by calling a REFPROP routine, or by using an
    !+ad_desc  algorithm taken from Panos's satliq routine.
    !+ad_prob  None
    !+ad_call  tsat_refprop
    !+ad_hist  04/09/14 PJK Initial version
    !+ad_hist  17/12/14 PJK Added call to REFPROP interface
    !+ad_stat  Okay
    !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
    !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
    !+ad_docc  Dept., Culham Laboratory, Abingdon
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: tsat

    !  Arguments

    real(kind(1.0D0)), intent(in) :: p

    !  Local variables

    real(kind(1.0D0)) :: ta1,ta2,ta3,ta4,ta5,ta6,ta7

    !  Global shared variables

    !  Inputs: coolwh, irefprop

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (irefprop == 1) then

       tsat = tsat_refprop(p*1.0D6, coolwh)

    else

       !  Method taken from Panos Karditsas's satliq routine

       ta1 = 168.396D0
       ta2 =   0.314653D0
       ta3 =  -0.000728D0
       ta4 =  31.588979D0
       ta5 =  11.473141D0
       ta6 =  -0.575335D0
       ta7 =   0.013165D0

       tsat = 273.15D0 + ta1 + ta2/p + ta3/p**2 + ta4*log(p) &
            + ta5*p + ta6*p*p + ta7*p*p*p

    end if

  end function tsat

  subroutine sctfcoil_nuclear_heating_iter90(coilhtmx,dpacop,htheci,nflutf, &
       pheci,pheco,ptfiwp,ptfowp,raddose,ptfnuc)

    !+ad_name  sctfcoil_nuclear_heating_iter90
    !+ad_summ  Superconducting TF coil nuclear heating estimate
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  coilhtmx : output real : peak magnet heating (MW/m3)
    !+ad_args  dpacop : output real : copper stabiliser displacements/atom
    !+ad_args  htheci : output real : peak TF coil case heating (MW/m3)
    !+ad_args  nflutf : output real : maximum neutron fluence (n/m2)
    !+ad_args  pheci : output real : inboard coil case heating (MW)
    !+ad_args  pheco : output real : outboard coil case heating (MW)
    !+ad_args  ptfiwp : output real : inboard TF coil winding pack heating (MW)
    !+ad_args  ptfowp : output real : outboard TF coil winding pack heating (MW)
    !+ad_args  raddose : output real : insulator dose (rad)
    !+ad_args  ptfnuc : output real : TF coil nuclear heating (MW)
    !+ad_desc  This subroutine calculates the nuclear heating in the
    !+ad_desc  superconducting TF coils, assuming an exponential neutron
    !+ad_desc  attenuation through the blanket and shield materials.
    !+ad_desc  The estimates are based on 1990 ITER data.
    !+ad_desc  <P>The arrays <CODE>coef(i,j)</CODE> and <CODE>decay(i,j)</CODE>
    !+ad_desc  are used for exponential decay approximations of the
    !+ad_desc  (superconducting) TF coil nuclear parameters.
    !+ad_desc  <UL><P><LI><CODE>j = 1</CODE> : stainless steel shield (assumed)
    !+ad_desc      <P><LI><CODE>j = 2</CODE> : tungsten shield (not used)</UL>
    !+ad_desc  Note: Costing and mass calculations elsewhere assume
    !+ad_desc  stainless steel only.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  04/11/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: coilhtmx,dpacop,htheci,nflutf, &
         pheci,pheco,ptfiwp,ptfowp,raddose,ptfnuc

    !  Local variables

    integer, parameter :: ishmat = 1  !  stainless steel coil casing is assumed

    real(kind(1.0D0)), dimension(5) :: fact
    real(kind(1.0D0)), dimension(5,2) :: coef
    real(kind(1.0D0)), dimension(7,2) :: decay
    real(kind(1.0D0)) :: dshieq,dshoeq,fpsdt,fpydt,ptfi,ptfo,wpthk

    !  Global shared variables

    !  Input: blnkith,blnkoth,casthi,cfactr,fwith,fwoth,itfsup,shldith
    !  Input: shldoth,tfsai,tfsao,thkwp,tinstf,tlife,wallmw

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (itfsup == 0) then  !  Resistive coils
       coilhtmx = 0.0D0
       ptfiwp = 0.0D0
       ptfowp = 0.0D0
       htheci = 0.0D0
       pheci = 0.0D0
       pheco = 0.0D0
       raddose = 0.0D0
       nflutf = 0.0D0
       dpacop = 0.0D0

       ptfnuc = 0.0D0

    else

       !  TF coil nuclear heating coefficients in region i (first element),
       !  assuming shield material j (second element where present)

       fact(1) = 8.0D0
       fact(2) = 8.0D0
       fact(3) = 6.0D0
       fact(4) = 4.0D0
       fact(5) = 4.0D0

       coef(1,1) = 10.3D0
       coef(2,1) = 11.6D0
       coef(3,1) = 7.08D5
       coef(4,1) = 2.19D18
       coef(5,1) = 3.33D-7
       coef(1,2) = 8.32D0
       coef(2,2) = 10.6D0
       coef(3,2) = 7.16D5
       coef(4,2) = 2.39D18
       coef(5,2) = 3.84D-7

       decay(1,1) = 10.05D0
       decay(2,1) = 17.61D0
       decay(3,1) = 13.82D0
       decay(4,1) = 13.24D0
       decay(5,1) = 14.31D0
       decay(6,1) = 13.26D0
       decay(7,1) = 13.25D0
       decay(1,2) = 10.02D0
       decay(2,2) = 3.33D0
       decay(3,2) = 15.45D0
       decay(4,2) = 14.47D0
       decay(5,2) = 15.87D0
       decay(6,2) = 15.25D0
       decay(7,2) = 17.25D0

       !  N.B. The vacuum vessel appears to be ignored

       dshieq = shldith + fwith + blnkith
       dshoeq = shldoth + fwoth + blnkoth

       !  Winding pack radial thickness, including groundwall insulation

       wpthk = thkwp + 2.0D0*tinstf

       !  Nuclear heating rate in inboard TF coil (MW/m**3)

       coilhtmx = fact(1) * wallmw * coef(1,ishmat) * &
            exp(-decay(6,ishmat) * (dshieq + casthi))

       !  Total nuclear heating (MW)

       ptfiwp = coilhtmx * tfsai * &
            (1.0D0-exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)
       ptfowp = fact(1) * wallmw * coef(1,ishmat) * &
            exp(-decay(6,ishmat) * (dshoeq + casthi)) * tfsao * &
            (1.0D0 - exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)

       !  Nuclear heating in plasma-side TF coil case (MW)

       htheci = fact(2) * wallmw * coef(2,ishmat) * &
            exp(-decay(7,ishmat) * dshieq)
       pheci = htheci * tfsai * (1.0D0-exp(-decay(2,ishmat)*casthi))/ &
            decay(2,ishmat)
       pheco = fact(2) * wallmw * coef(2,ishmat) * &
            exp(-decay(7,ishmat) * dshoeq) * tfsao * &
            (1.0D0-exp(-decay(2,ishmat)*casthi))/decay(2,ishmat)
       ptfi = ptfiwp + pheci
       ptfo = ptfowp + pheco

       ptfnuc = ptfi + ptfo

       !  Full power DT operation years for replacement of TF Coil
       !  (or plant life)

       fpydt = cfactr * tlife
       fpsdt = fpydt * 3.154D7  !  seconds

       !  Insulator dose (rad)

       raddose = coef(3,ishmat) * fpsdt * fact(3) * wallmw * &
            exp(-decay(3,ishmat) * (dshieq+casthi))

       !  Maximum neutron fluence in superconductor (n/m**2)

       nflutf = fpsdt * fact(4) * wallmw * coef(4,ishmat) * &
            exp(-decay(4,ishmat) * (dshieq+casthi))

       !  Atomic displacement in copper stabilizer

       dpacop = fpsdt * fact(5) * wallmw * coef(5,ishmat) * &
            exp(-decay(5,ishmat) * (dshieq + casthi) )

    end if

  end subroutine sctfcoil_nuclear_heating_iter90


end module fwbs_module
