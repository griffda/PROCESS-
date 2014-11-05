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
  real(kind(1.0D0)) :: A_FW_IB_PPCS = 348.2D0  ! [m^2] IB first wall area
  real(kind(1.0D0)) :: A_FW_OB_PPCS = 905.6D0  ! [m^2] OB first wall area
  real(kind(1.0D0)) :: NWL_av_PPCS = 1.94D0    ! [MW/m^2] Average neutron wall load
  real(kind(1.0D0)) :: NWL_av_IB_PPCS = 1.73D0 ! [MW/m^2] Average IB wall load
  real(kind(1.0D0)) :: NWL_av_OB_PPCS = 1.92D0 ! [MW/m^2] Average OB wall load
  real(kind(1.0D0)) :: NWL_max_IB_PPCS = 1.99D0 ! [MW/m^2] Maximum IB wall load
  real(kind(1.0D0)) :: NWL_max_OB_PPCS = 2.41D0 ! [MW/m^2] Maximum OB wall load
  real(kind(1.0D0)) :: f_peak_PPCS = 1.21      ! [--] Neutron wall load peaking factor
  real(kind(1.0D0)) :: CF_bl_PPCS              ! [%] Blanket coverage factor (calculated)
  real(kind(1.0D0)) :: e_Li_PPCS = 30.0D0      ! [%] Li6 enrichment
  character(len=13) :: breeder_PPCS = 'Orthosilicate' ! Breeder type

  real(kind(1.0D0)) :: t_BZ_IB_PPCS = 36.5D0   ! [cm] IB Breeding Zone thickness
  real(kind(1.0D0)) :: t_BZ_OB_PPCS = 46.5D0   ! [cm] OB Breeding Zone thickness
  real(kind(1.0D0)) :: TBR_PPCS = 1.12D0       ! [--] Tritium Breeding Ratio
  real(kind(1.0D0)) :: M_E_PPCS = 1.38D0       ! [--] Energy multiplication factor
  ! not used...
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
  !+ad_cont  blanket_neutronics_hcpb_kit
  !+ad_cont  blshvv_volume
  !+ad_cont  dshellvol
  !+ad_cont  eshellvol
  !+ad_cont  fwbs
  !+ad_cont  sctfcoil_nuclear_heating_iter90
  !+ad_cont  st_centrepost_nuclear_heating
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
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  kit_blanket_model
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  plasma_geometry_module
  !+ad_call  process_output
  !+ad_call  rfp_variables
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
  !+ad_hisc               rfp_variables, stellarator_variables
  !+ad_hist  08/05/13 PJK Added dshellvol, eshellvol
  !+ad_hist  22/05/13 PJK Added kit_blanket_model, build_module, times_variables
  !+ad_hist  14/08/13 PJK Made blanket_neutronics public
  !+ad_hist  04/09/14 PJK Added error_handling
  !+ad_hist  22/10/14 PJK Added current_drive_variables
  !+ad_hist  04/11/14 PJK Split TF coil nuclear heating calculations
  !+ad_hisc               into new routine
  !+ad_hist  05/11/14 PJK Split ST centrepost nuclear heating, and
  !+ad_hisc               volume calculations into new routines
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_module
  use build_variables
  use buildings_variables
  use constants
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables
  use kit_blanket_model
  use pfcoil_variables
  use physics_variables
  use plasma_geometry_module
  use process_output
  use rfp_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables

  implicit none

  private
  public :: fwbs, blanket_neutronics_hcpb_kit, sctfcoil_nuclear_heating_iter90

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
    !+ad_desc  blanket, shield, vacuum vessel and external cryostat.
    !+ad_prob  None
    !+ad_call  blanket_neutronics_hcpb_kit
    !+ad_call  blshvv_volume
    !+ad_call  iterate_fw
    !+ad_call  oblnkl
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_call  pumppower
    !+ad_call  sctfcoil_nuclear_heating_iter90
    !+ad_call  st_centrepost_nuclear_heating
    !+ad_call  tsat
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
    !+ad_hist  21/08/14 PJK Initial draft incorporation of new thermodynamic model
    !+ad_hist  03/09/14 PJK Changed PF coil to cryostat top vertical clearance
    !+ad_hist  22/10/14 PJK Added porbitlossmw to htpmw_fw calculation
    !+ad_hist  03/11/14 PJK Clarified ipowerflow vs blkttype logic
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  C5.M15 Milestone Report: Development and Implementation of Improved
    !+ad_docc  Balance of Plant Models for PROCESS, C. Harrington, August 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    integer :: no90fw,no180fw,no90bz,no180bz

    real(kind(1.0D0)) :: a,b,bfwi,bfwo,bldepti,bldepto,bllengi,bllengo, &
         blwidti,blwidto,bzfllengi,bzfllengo,cf,coilhtmx,coolvol, &
         decaybl,decaybzi,decaybzo,decayfwi,decayfwo,decayshldi, &
         decayshldo,dpacop,frachit,fwfllengi,fwfllengo,hblnkt,htheci, &
         htpmw_bzi,htpmw_bzo,htpmw_fwi,htpmw_fwo,hvv,mfbzi,mfbzo, &
         mfbzpi,mfbzpo,mffwi,mffwo,mffwpi,mffwpo,npbzi,npbzo,npfwi, &
         npfwo,pheci,pheco,pneut2,pnucbsi,pnucbso,pnucbzi,pnucbzo, &
         pnucfwbs,pnucfwbsi,pnucfwbso,pnucfwi,pnucfwo,pnucshldi, &
         pnucshldo,pnucsi,pnucso,psurffwi,psurffwo,ptfiwp, &
         ptfowp,ptor,r1,raddose,rhof,tpeakfwi,tpeakfwo, &
         velbzi,velbzo,velfwi,velfwo,vffwi,vffwo,volshldi,volshldo,zdewex

    !  Global shared variables

    !  Input: abktflnc,blbmith,blbmoth,blbpith,blbpoth,blbuith,blbuoth
    !  Input: blktcycle,blktmodel,blkttype,blnkith,blnkoth,coolp,coolwh
    !  Input: declblkt,declfw,declshld,denstl,divclfr,divplt,emult
    !  Input: etaiso,fblbe,fblbreed,fblhebmi,fblhebmo
    !  Input: fblhebpi,fblhebpo,fblli,fblli2o,fblli2o,fbllipb
    !  Input: fblss,fblvd,fdiv,fhcd,fhole,fpumpblkt,fpumpdiv,fpumpfw
    !  Input: fpumpshld,fwarea,fwareaib,fwareaob,fwbsshape,fwclfr
    !  Input: fwerlim,fwith,fwoth,hmax,idivrt,inlet_temp,ipowerflow,itart
    !  Input: nblktmodpi,nblktmodpo,nblktmodti,nblktmodto,outlet_temp,pi
    !  Input: pneutmw,porbitlossmw,pradmw,rmajor,rminor
    !  Input: scrapli,scraplo,tfcth,tlife,triang,vfblkt,vfshld,wallmw

    !  Output: afwi,afwo,coolmass,densbreed,divmas,divsur,fwclfr,fwith
    !  Output: fwlife,fwmass,fwoth,htpmw_blkt,htpmw_div,htpmw_fw
    !  Output: htpmw_shld,outlet_temp,pnucblkt,pnuccp,pnucdiv
    !  Output: pnucfw,pnuchcd,pnuchcd,pnucloss,pnucshld,praddiv
    !  Output: pradfw,pradhcd,pradloss,ptfnuc
    !  Output: tpeak,vfblkt,volblkti,volblkto,whtblbe,whtblbreed,whtblkt
    !  Output: whtblli,whtblss,whtblvd,whtshld,wpenshld,wtblli2o,wtbllipb

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  First wall full-power lifetime (years)
    !  May be recalculated below if ipowerflow=1 and blktcycle>0,
    !  and also by the availability model

    fwlife = min(abktflnc/wallmw, tlife)

    !  Blanket, shield, vacuum vessel and external cryostat volumes

    call blshvv_volume(hblnkt,volshldi,volshldo,zdewex)

    !  Power deposition in the first wall, blanket, shield and divertor

    !  Neutron power lost through holes in first wall (eventually absorbed by
    !  shield)

    pnucloss = pneutmw * fhole

    if (blktmodel == 1) then

       call blanket_neutronics_hcpb_kit

       if (ipowerflow == 1) then
          pnucdiv = pneutmw * fdiv
          pnuchcd = pneutmw * fhcd
          pnucfw = pneutmw - pnucdiv - pnucloss - pnuchcd

          pradloss = pradmw * fhole
          praddiv = pradmw * fdiv
          pradhcd = pradmw * fhcd
          pradfw = pradmw - praddiv - pradloss - pradhcd

          htpmw_fw = fpumpfw * (pnucfw + pradfw + porbitlossmw)
          htpmw_blkt = fpumpblkt * pnucblkt
          htpmw_shld = fpumpshld * pnucshld
          htpmw_div = fpumpdiv * (pdivt + pnucdiv + praddiv)

          !  Void fraction in first wall / breeding zone,
          !  for use in fwmass and coolvol calculation below

          vffwi = 1.0D0 - fblbe - fblbreed - fblss
          vffwo = vffwi
       end if

    else

       if (itart == 1) then
          pnuccp = st_centrepost_nuclear_heating(pneutmw,hmax,tfcth,rmajor)
       else
          pnuccp = 0.0D0
       end if

       if (ipowerflow == 0) then

          !  Energy-multiplied neutron power

          pneut2 = (pneutmw - pnucloss - pnuccp) * emult

          !  Nuclear heating in the blanket

          decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)

          pnucblkt = pneut2 * (1.0D0 - exp(-blnkoth/decaybl))

          !  Nuclear heating in the shield

          pnucshld = pneut2 - pnucblkt

          !  Superconducting TF coil shielding calculations

          call sctfcoil_nuclear_heating_iter90(coilhtmx,dpacop,htheci,nflutf, &
               pheci,pheco,ptfiwp,ptfowp,raddose,ptfnuc)

       else  !  ipowerflow == 1

          !  Neutron power incident on divertor (MW)

          pnucdiv = pneutmw * fdiv

          !  Neutron power incident on HCD apparatus (MW)

          pnuchcd = pneutmw * fhcd

          !  Neutron power deposited in first wall, blanket and shield (MW)

          pnucfwbs = pneutmw - pnucdiv - pnucloss - pnuccp - pnuchcd

          !  Split between inboard and outboard by first wall area fractions

          pnucfwbsi = pnucfwbs * fwareaib/fwarea
          pnucfwbso = pnucfwbs * fwareaob/fwarea

          !  Radiation power incident on divertor (MW)

          praddiv = pradmw * fdiv

          !  Radiation power incident on HCD apparatus (MW)

          pradhcd = pradmw * fhcd

          !  Radiation power lost through holes (eventually hits shield) (MW)

          pradloss = pradmw * fhole

          !  Radiation power incident on first wall (MW)

          pradfw = pradmw - praddiv - pradloss - pradhcd

          !  Calculate the power deposited in the first wall, blanket and shield,
          !  and the required coolant pumping power

          !  If we have chosen pressurised water as the coolant, set the
          !  coolant outlet temperature as 20 deg C below the boiling point

          if (coolwh == 2) then
             outlet_temp = tsat(1.0D-6*coolp) - 20.0D0  !  in K
          end if

          bfwi = 0.5D0*fwith
          bfwo = 0.5D0*fwoth

          vffwi = afwi*afwi/(bfwi*bfwi)  !  inboard FW coolant void fraction
          vffwo = afwo*afwo/(bfwo*bfwo)  !  outboard FW coolant void fraction

          !  First wall decay length (m) - improved calculation required

          decayfwi = declfw
          decayfwo = declfw

          !  Surface heat flux on first wall (MW) (sum = pradfw)

          psurffwi = pradfw * fwareaib/fwarea
          psurffwo = pradfw * fwareaob/fwarea

          if (blktcycle == 0) then

             !  Simple blanket model
             !  The power deposited in the first wall, breeder zone and shield is
             !  calculated according to their dimensions and materials assuming
             !  an exponential attenuation of nuclear heating with increasing
             !  radial distance.  The pumping power for the coolant is calculated
             !  as a fraction of the total thermal power deposited in the
             !  coolant.

             pnucfwi = pnucfwbsi * (1.0D0 - exp(-2.0D0*bfwi/decayfwi))
             pnucfwo = pnucfwbso * (1.0D0 - exp(-2.0D0*bfwo/decayfwo))

             !  Neutron power reaching blanket and shield (MW)

             pnucbsi = pnucfwbsi - pnucfwi
             pnucbso = pnucfwbso - pnucfwo

             !  Blanket decay length (m) - improved calculation required

             decaybzi = declblkt
             decaybzo = declblkt

             !  Neutron power deposited in breeder zone (MW)

             pnucbzi = pnucbsi * (1.0D0 - exp(-blnkith/decaybzi))
             pnucbzo = pnucbso * (1.0D0 - exp(-blnkoth/decaybzo))

             !  Calculate coolant pumping powers from input fraction.  
             !  The pumping power is assumed to be a fraction, fpump, of the
             !  incident thermal power to each component so that
             !  htpmw_i = fpump_i*C, where C is the non-pumping thermal power
             !  deposited in the coolant

             !  First wall pumping power (MW)

             htpmw_fw = fpumpfw * (pnucfwi + pnucfwo + psurffwi + psurffwo + porbitlossmw)

             !  Blanket pumping power (MW)

             htpmw_blkt = fpumpblkt * (pnucbzi*emult + pnucbzo*emult)

          else
             !  blktcycle > 0; detailed thermal hydraulic model for first wall and breeding zone

             !  Determine size of blanket modules
             !  Length of coolant pipes is assumed to be 80% of total radial space
             !  available for blanket, allowing for connections, manifolds etc.

             bldepti = 0.8D0 * blnkith
             bldepto = 0.8D0 * blnkoth

             !  Using the total perimeter of the machine, segment the outboard
             !  blanket into nblktmodp*nblktmodt modules, all assumed to be the same
             !  size

             !  Calculate mid-plane toroidal circumference and segment

             blwidti = (2.0D0*pi*(rmajor - rminor - scrapli)) / nblktmodti
             blwidto = (2.0D0*pi*(rmajor + rminor + scraplo)) / nblktmodto

             !  Calculate poloidal height of blanket modules

             if ((itart == 1).or.(fwbsshape == 1)) then  !  D-shaped machine

                !  Segment vertical inboard surface

                bllengi = (2.0D0*hblnkt) / nblktmodpi

                !  Calculate perimeter of ellipse that defines the internal
                !  surface of the outboard first wall / blanket

                !  Mid-plane distance from inboard to outboard side

                a = scrapli + 2.0D0*rminor + scraplo

                !  Internal half-height of blanket

                b = hblnkt

                !  Calculate ellipse circumference using Ramanujan approximation

                ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

                !  Calculate blanket poloidal length and segment, subtracting
                !  divertor length

                bllengo = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpo

             else  !  shape defined by two half-ellipses

                !  Major radius where half-ellipses 'meet'

                r1 = rmajor - rminor*triang

                !  Internal half-height of blanket

                b = hblnkt

                !  Distance between r1 and nearest edge of inboard first wall /
                !  blanket

                a = r1 - (rmajor - rminor - scrapli)

                !  Calculate ellipse circumference using Ramanujan approximation

                ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

                !  Calculate inboard blanket poloidal length and segment,
                !  subtracting divertor length.
                !  Assume divertor lies between the two ellipses, so fraction fdiv still applies

                bllengi = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpi

                !  Distance between r1 and inner edge of outboard first wall /
                !  blanket

                a = rmajor + rminor + scraplo - r1

                ! Calculate ellipse circumference using Ramanujan approximation

                ptor = pi * ( 3.0D0*(a+b) - sqrt( (3.0D0*a + b)*(a + 3.0D0*b) ) )

                !  Calculate outboard blanket poloidal length and segment,
                !  subtracting divertor length

                bllengo = 0.5D0*ptor * (1.0D0 - fdiv) / nblktmodpo

             end if

             !  Calculate total flow lengths
             !  First wall flow is assumed to follow a radial-poloidal-radial path
             !  (as WCLL)

             fwfllengi = 2.0D0*bldepti + bllengi
             fwfllengo = 2.0D0*bldepto + bllengo

             !  Blanket flow is assumed to follow a rad-pol-rad-rad-pol-rad path (as
             !  WCLL)

             bzfllengi = 4.0D0*bldepti + 2.0D0*bllengi
             bzfllengo = 4.0D0*bldepto + 2.0D0*bllengo

             !  Number of angle turns in FW and blanket flow channels,
             !  consistent with flow lengths defined

             no90fw = 2
             no180fw = 0
             no90bz = 4
             no180bz = 1

             !  Start thermal hydraulic calculations with inboard side

             !  Calculation of maximum first wall temperature

             call iterate_fw(afwi,bfwi,fwareaib,fwarea,decayfwi,abktflnc,fwlife, &
                  pnucfwbsi,psurffwi,inlet_temp,outlet_temp,bllengi,coolp, &
                  fwerlim,pnucfwi,tpeakfwi,cf,rhof,velfwi)

             !  Adjust first wall thickness if bfwi has been changed

             fwith = 2.0D0*bfwi
             vffwi = (afwi/bfwi)**2

             !  Total mass flow rate to remove inboard first wall power (kg/s)

             mffwi = 1.0D6*(pnucfwi + psurffwi) / (cf*(outlet_temp-inlet_temp))

             !  Calculate total number of pipes from coolant fraction and 
             !  channel dimensions

             npfwi = (vffwi*fwith*fwareaib)/(pi*afwi*afwi*fwfllengi)

             !  Mass flow rate per coolant pipe (kg/s)

             mffwpi = mffwi/npfwi

             !  Neutron power reaching inboard breeder zone and shield (MW)

             pnucbsi = pnucfwbsi - pnucfwi

             !  Breeder zone decay length (m) - improved calculation required

             decaybzi = declblkt

             !  Neutron power deposited in inboard breeder zone (MW)

             pnucbzi = pnucbsi * (1.0D0 - exp(-blnkith/decaybzi))

             !  Assume inlet and outlet temperatures for breeder zone are the same as those of
             !  for the first wall. Channel inner diameter is also the same. This does not
             !  have to be so but is the case for WCLL, and allows the pumping power to be
             !  calculated.

             mfbzi = 1.0D6*(pnucbzi*emult) / (cf*(outlet_temp-inlet_temp))  !  kg/s

             !  Calculate total number of pipes (in all inboard modules) from coolant fraction and 
             !  channel dimensions
             !  Assumes up/down flow, two 90 deg bends per length

             npbzi = (vfblkt*volblkti)/(pi*afwi*afwi*bzfllengi)

             !  Mass flow rate per coolant pipe (kg/s)

             mfbzpi = mfbzi/npbzi

             !  Coolant velocity in breeder zone (m/s)

             velbzi = mfbzpi/(pi*afwi*afwi*rhof)

             !  Ideally we should check the maximum temperature in the breeder zone is below
             !  necessary limits, but this is not straightforward

             !  Calculate pumping powers for blanket and first wall (MW)

             htpmw_fwi = pumppower(fwfllengi,afwi,mffwi,mffwpi,no90fw,no180fw,velfwi, &
                  inlet_temp,outlet_temp,etaiso,coolwh,coolp)
             htpmw_bzi = pumppower(bzfllengi,afwi,mfbzi,mfbzpi,no90bz,no180bz,velbzi, &
                  inlet_temp,outlet_temp,etaiso,coolwh,coolp)

             !  Repeat thermal hydraulic calculations for outboard side

             !  Calculation of maximum first wall temperature
             !  Include NBI orbit loss power as a component of the  outboard wall surface power

             call iterate_fw(afwo,bfwo,fwareaob,fwarea,decayfwo,abktflnc,fwlife, &
                  pnucfwbso,psurffwo+porbitlossmw,inlet_temp,outlet_temp,bllengo,coolp, &
                  fwerlim,pnucfwo,tpeakfwo,cf,rhof,velfwo)

             !  Adjust first wall thickness if bfwo has been changed

             fwoth = 2.0D0*bfwo
             vffwo = (afwo/bfwo)**2

             !  Total mass flow rate to remove outboard first wall power (kg/s)

             mffwo = 1.0D6*(pnucfwo + psurffwo + porbitlossmw) / (cf*(outlet_temp-inlet_temp))

             !  Calculate total number of pipes from coolant fraction and 
             !  channel dimensions

             npfwo = (vffwo*fwoth*fwareaob)/(pi*afwo*afwo*fwfllengo)

             !  Mass flow rate per coolant pipe (kg/s)

             mffwpo = mffwo/npfwo

             !  Neutron power reaching outboard breeder zone and shield (MW)

             pnucbso = pnucfwbso - pnucfwo

             !  Breeder zone decay length (m) - improved calculation required

             decaybzo = declblkt

             !  Neutron power deposited in outboard breeder zone (MW)

             pnucbzo = pnucbso * (1.0D0 - exp(-blnkoth/decaybzo))

             !  Assume inlet and outlet temperatures for breeder zone are the same as those of
             !  for the first wall. Channel inner diameter is also the same. This does not
             !  have to be so but is the case for WCLL, and allows the pumping power to be
             !  calculated.

             mfbzo = 1.0D6*(pnucbzo*emult) / (cf*(outlet_temp-inlet_temp))  !  kg/s

             !  Calculate total number of pipes (in all outboard modules) from coolant fraction and 
             !  channel dimensions
             !  Assumes up/down flow, two 90 deg bends per length

             npbzo = (vfblkt*volblkto)/(pi*afwo*afwo*bzfllengo)

             !  mass flow rate per coolant pipe (kg/s)

             mfbzpo = mfbzo/npbzo

             !  Coolant velocity in breeder zone (m/s)

             velbzo = mfbzpo/(pi*afwo*afwo*rhof)

             !  Ideally we should check the maximum temperature in the breeder zone is below
             !  necessary limits, but this is not straightforward

             !  Calculate pumping powers for blanket and first wall

             htpmw_fwo = pumppower(fwfllengo,afwo,mffwo,mffwpo,no90fw,no180fw,velfwo, &
                  inlet_temp,outlet_temp,etaiso,coolwh,coolp)
             htpmw_bzo = pumppower(bzfllengo,afwo,mfbzo,mfbzpo,no90bz,no180bz,velbzo, &
                  inlet_temp,outlet_temp,etaiso,coolwh,coolp)

             !  Total inboard & outboard FW and blanket pumping powers (MW)

             htpmw_fw = htpmw_fwi + htpmw_fwo

             htpmw_blkt = htpmw_bzi + htpmw_bzo

             tpeak = max(tpeakfwi, tpeakfwo) - 273.15D0  !  deg C

          end if  !  blktcycle

          pnucfw = pnucfwi + pnucfwo
          pnucblkt = (pnucbzi + pnucbzo)*emult

          !  Calculation of shield and divertor powers
          !  Shield and divertor powers and pumping powers are calculated using the same 
          !  simplified method as the first wall and breeder zone when blktcycle = 0. 
          !  i.e. the pumping power is a fraction of the total thermal power deposited in the
          !  coolant.

          !  Neutron power reaching the shield (MW)
          !  The power lost from the fhole area fraction is assumed to be incident upon the shield

          pnucsi = pnucbsi - pnucbzi + (pnucloss + pradloss) * fwareaib/fwarea
          pnucso = pnucbso - pnucbzo + (pnucloss + pradloss) * fwareaob/fwarea

          !  Improved calculation of shield power decay lengths required

          decayshldi = declshld
          decayshldo = declshld

          !  Neutron power deposited in the shield (MW)

          pnucshldi = pnucsi * (1.0D0 - exp(-shldith/decayshldi))
          pnucshldo = pnucso * (1.0D0 - exp(-shldoth/decayshldo))

          pnucshld = pnucshldi + pnucshldo

          !  Calculate coolant pumping powers from input fraction.  
          !  The pumping power is assumed to be a fraction, fpump, of the incident
          !  thermal power to each component so that,
          !     htpmw_i = fpump_i*C
          !  where C is the non-pumping thermal power deposited in the coolant

          !  Shield pumping power (MW)

          htpmw_shld = fpumpshld*(pnucshldi + pnucshldo)

          !  Divertor pumping power (MW)

          htpmw_div = fpumpdiv*(pdivt + pnucdiv + praddiv)

          !  Remaining neutron power to TF coils and elsewhere. This is assumed
          !  (for superconducting TF coils at least) to be absorbed by the TF
          !  coils, and so contributes to the cryogenic load

          if (itfsup == 1) then
             ptfnuc = pnucsi + pnucso - pnucshldi - pnucshldo
          else  !  resistive TF coils
             ptfnuc = 0.0D0
          end if

       end if  !  ipowerflow

    end if  !  blktmodel

    !  Divertor mass

    divsur = fdiva * 2.0D0 * pi * rmajor * rminor
    if (idivrt == 2) divsur = divsur * 2.0D0
    divmas = divsur * divdens * (1.0D0 - divclfr) * divplt

    !  Start adding components of the coolant mass:
    !  Divertor coolant volume (m3)

    coolvol = divsur * divclfr * divplt

    !  Blanket mass, excluding coolant

    if (blktmodel == 0) then
       if ((blkttype == 1).or.(blkttype == 2)) then  !  liquid breeder (WCLL or HCLL)
          wtbllipb = volblkt * fbllipb * 9400.0D0
          whtblli = volblkt * fblli * 534.0D0
          whtblkt = wtbllipb + whtblli
       else  !  solid breeder (HCPB); always for ipowerflow=0
          wtblli2o = volblkt * fblli2o * 2010.0D0
          whtblbe = volblkt * fblbe * 1850.0D0
          whtblkt = wtblli2o + whtblbe
       end if
       whtblss = volblkt * denstl * fblss
       whtblvd = volblkt * 5870.0D0  * fblvd

       whtblkt = whtblkt + whtblss + whtblvd

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

    !  When blktmodel > 0, although the blanket is by definition helium-cooled
    !  in this case, the shield etc. are assumed to be water-cooled, and since
    !  water is heavier the calculation for coolmass is better done with
    !  coolwh=2 if blktmodel > 0; thus we can ignore the helium coolant mass
    !  in the blanket.

    if (blktmodel == 0) then
       coolvol = coolvol + volblkt*vfblkt
    end if

    !  Shield mass

    whtshld = volshld * denstl * (1.0D0 - vfshld)
    coolvol = coolvol + volshld*vfshld

    !  Penetration shield (set = internal shield)

    wpenshld = whtshld

    if (ipowerflow == 0) then

       !  First wall mass
       !  (first wall area is calculated elsewhere)

       fwmass = fwarea * (fwith+fwoth)/2.0D0 * denstl * (1.0D0-fwclfr)

       !  Surface areas adjacent to plasma

       coolvol = coolvol + fwarea * (fwith+fwoth)/2.0D0 * fwclfr

    else

       fwmass = denstl * &
            (fwareaib*fwith*(1.0D0-vffwi) + fwareaob*fwoth*(1.0D0-vffwo))
       coolvol = coolvol + fwareaib*fwith*vffwi + fwareaob*fwoth*vffwo

       !  Average first wall coolant fraction, only used by old routines
       !  in fispact.f90, safety.f90

       fwclfr = (fwareaib*fwith*vffwi + fwareaob*fwoth*vffwo) / &
            (fwarea*0.5D0*(fwith+fwoth))

    end if

    !  Mass of coolant = volume * density at typical coolant
    !  temperatures and pressures
    !  N.B. for blktmodel > 0, mass of *water* coolant in the non-blanket
    !  structures is used (see comment above)

    if ((blktmodel > 0).or.(coolwh == 2)) then  !  pressurised water coolant
       coolmass = coolvol*806.719D0
    else  !  gaseous helium coolant
       coolmass = coolvol*1.517D0
    end if

    if (iprint == 0) return

    !  Output section

    call oheadr(outfile,'First Wall / Blanket / Shield')
    call ovarre(outfile,'Average neutron wall load (MW/m2)','(wallmw)', wallmw)
    if (blktmodel > 0) then
       call ovarre(outfile,'Neutron wall load peaking factor','(wallpf)', wallpf)
    end if
    call ovarre(outfile,'First wall full-power lifetime (years)', &
         '(fwlife)',fwlife)

    if (blktmodel > 0) then
       call ovarre(outfile,'Inboard breeding zone thickness (m)', &
            '(blbuith)', blbuith)
       call ovarre(outfile,'Inboard box manifold thickness (m)', &
            '(blbmith)', blbmith)
       call ovarre(outfile,'Inboard back plate thickness (m)', &
            '(blbpith)', blbpith)
       call ovarre(outfile,'Outboard breeding zone thickness (m)', &
            '(blbuoth)', blbuoth)
       call ovarre(outfile,'Outboard box manifold thickness (m)', &
            '(blbmoth)', blbmoth)
       call ovarre(outfile,'Outboard back plate thickness (m)', &
            '(blbpoth)', blbpoth)
    end if

    if (itart == 1) then
       call osubhd(outfile,'(Copper centrepost used)')
       call ovarre(outfile,'ST centrepost heating (MW)','(pnuccp)',pnuccp)

    else if ((ipowerflow == 0).and.(blktmodel == 0)) then
       call osubhd(outfile,'TF coil nuclear parameters :')
       call ovarre(outfile,'Peak magnet heating (MW/m3)','(coilhtmx)', &
            coilhtmx)
       call ovarre(outfile,'Inboard TF coil winding pack heating (MW)', &
            '(ptfiwp)',ptfiwp)
       call ovarre(outfile,'Outboard TF coil winding pack heating (MW)', &
            '(ptfowp)',ptfowp)
       call ovarre(outfile,'Peak TF coil case heating (MW/m3)', &
            '(htheci)',htheci)
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
       call ovarre(outfile, &
            'Blanket heating (including energy multiplication) (MW)', &
            '(pnucblkt)',pnucblkt)
       call ovarre(outfile,'Shield nuclear heating (MW)', &
            '(pnucshld)',pnucshld)
       call ovarre(outfile,'TF coil nuclear heating (MW)', &
            '(ptfnuc)',ptfnuc)
    else
       call osubhd(outfile,'Blanket neutronics :')
       call ovarre(outfile, &
            'Blanket heating (including energy multiplication) (MW)', &
            '(pnucblkt)',pnucblkt)
       call ovarre(outfile,'Shield heating (MW)','(pnucshld)',pnucshld)
       call ovarre(outfile,'Energy multiplication in blanket','(emult.)',emult)
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

    if ((ipowerflow == 1).and.(blktmodel == 0)) then
       call oblnkl(outfile)
       call ovarin(outfile, &
            'First wall / blanket thermodynamic model','(blktcycle)',blktcycle)
       if (blktcycle == 0) then
          call ocmmnt(outfile,'   (Simple calculation)')
       else
          call ocmmnt(outfile, &
               '   (Detailed thermal hydraulic calculation)')
          call oblnkl(outfile)
          call ovarin(outfile,'Blanket type','(blkttype)',blkttype)
          if (blkttype == 1) then
             call ocmmnt(outfile, &
                  '   (Water-cooled liquid lithium: WCLL)')
          else if (blkttype == 2) then
             call ocmmnt(outfile, &
                  '   (Helium-cooled liquid lithium: HCLL)')
          else
             call ocmmnt(outfile,'   (Helium-cooled pebble bed: HCPB)')
          end if
          call ovarre(outfile,'First wall coolant pressure (Pa)', &
               '(coolp)',coolp)
          call ovarin(outfile,'Coolant fluid (1=helium, 2=water)', &
               '(coolwh)',coolwh)
          call ovarre(outfile, &
               'Inner radius of inboard coolant channels (m)', &
               '(afwi)',afwi)
          call ovarre(outfile, &
               'Outer radius of inboard coolant channels (m)', &
               '(fwith/2)',bfwi)
          call ovarre(outfile, &
               'Inner radius of outboard coolant channels (m)', &
               '(afwo)',afwo)
          call ovarre(outfile, &
               'Outer radius of outboard coolant channels (m)', &
               '(fwoth/2)',bfwo)
          call ovarre(outfile,'Inlet temperature of coolant (K)', &
               '(inlet_temp)',inlet_temp)
          call ovarre(outfile,'Outlet temperature of coolant (K)', &
               '(outlet_temp)',outlet_temp)
          call ovarre(outfile, &
               'Erosion thickness allowance for first wall (m)', &
               '(fwerlim)',fwerlim)
          call ovarre(outfile, &
               'Maximum temperature of first wall material (K)', &
               '(tfwmatmax)',tfwmatmax)
          call ovarin(outfile,'No of inboard blanket modules poloidally', &
               '(nblktmodpi)',nblktmodpi)
          call ovarin(outfile,'No of inboard blanket modules toroidally', &
               '(nblktmodti)',nblktmodti)
          call ovarin(outfile,'No of outboard blanket modules poloidally', &
               '(nblktmodpo)',nblktmodpo)
          call ovarin(outfile,'No of outboard blanket modules toroidally', &
               '(nblktmodto)',nblktmodto)
          call ovarre(outfile, &
               'Isentropic efficiency of first wall / blanket coolant pumps', &
               '(etaiso)',etaiso) !'
       end if
    end if

    call osubhd(outfile,'Blanket / shield volumes and weights :')

    if (blktmodel == 0) then
       if ((blkttype == 1).or.(blkttype == 2)) then
          write(outfile,601) volblkti, volblkto, volblkt,  &
               whtblkt, vfblkt, fbllipb, wtbllipb, fblli, whtblli,  &
               fblss, whtblss, fblvd, whtblvd, volshldi, volshldo,  &
               volshld, whtshld, vfshld, wpenshld
       else  !  (also if ipowerflow=0)
          write(outfile,600) volblkti, volblkto, volblkt,  &
               whtblkt, vfblkt, fblbe, whtblbe, fblli2o, wtblli2o,  &
               fblss, whtblss, fblvd, whtblvd, volshldi, volshldo,  &
               volshld, whtshld, vfshld, wpenshld
       end if
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

601 format( &
         t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
         t32,'-----------',t45,'------------',t62,'-----------'/ &
         '    Inboard blanket' ,t32,1pe10.3,/ &
         '    Outboard blanket' ,t32,1pe10.3,/ &
         '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '       Blanket LiPb ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket Li   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/ &
         '    Inboard shield'  ,t32,1pe10.3,/ &
         '    Outboard shield'  ,t32,1pe10.3,/ &
         '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '    Penetration shield'        ,t62,1pe10.3)

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
    call ovarre(outfile,'Total cryostat + vacuum vessel mass (kg)',&
         '(dewmkg)',dewmkg)
    call ovarre(outfile,'Internal vacuum vessel volume (m3)','(vdewin)',vdewin)
    call ovarre(outfile,'Vacuum vessel mass (kg)','(cryomass)',cryomass)
    call ovarre(outfile,'Divertor area (m2)','(divsur)',divsur)
    call ovarre(outfile,'Divertor mass (kg)','(divmas)',divmas)

  end subroutine fwbs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function st_centrepost_nuclear_heating(pneut,cphalflen,cpradius,rmajor) &
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

    real(kind(1.0D0)) :: pnuccp

    !  Arguments

    real(kind(1.0D0)), intent(in) :: pneut,cphalflen,cpradius,rmajor

    !  Local variables

    real(kind(1.0D0)) :: frachit

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    frachit = cphalflen / sqrt(cphalflen**2 + (rmajor-cpradius)**2 ) * &
         atan(cpradius/(rmajor-cpradius) )/pi

    pnuccp = pneut * frachit * (1.0D0 - exp(-2.0D0*cpradius/0.08D0))

  end function st_centrepost_nuclear_heating

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blshvv_volume(hblnkt,volshldi,volshldo,zdewex)

    !+ad_name  blshvv_volume
    !+ad_summ  Blanket, shield, vacuum vessel and external cryostat volumes
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  hblnkt : output real : internal half-height of blanket (m)
    !+ad_args  volshldi : output real : volume of inboard shield (m3)
    !+ad_args  volshldo : output real : volume of outboard shield (m3)
    !+ad_args  zdewex : output real : internal half-height of external cryostat (m)
    !+ad_desc  This routine calculates the volumes of (most of) the main fusion power
    !+ad_desc  core components.
    !+ad_prob  None
    !+ad_call  dshellarea
    !+ad_call  dshellvol
    !+ad_call  eshellarea
    !+ad_call  eshellvol
    !+ad_hist  05/11/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: hblnkt,volshldi,volshldo,zdewex

    !  Local variables

    real(kind(1.0D0)) :: hcryopf,hbot,htop,hshld,hvv,r1,r2,r3,v1,v2

    !  Global shared variables

    !  Input: blnkith,blnkoth,blnktth,clhsf,ddewex,ddwi,denstl,divfix
    !  Input: drpf,dzpf,fdiv,fhcd,fhole,fvoldw,fvolsi,fvolso,fwbsshape
    !  Input: fwith,fwoth,hmax,idivrt,ipowerflow,irfp,itart,kappa,pi,rb
    !  Input: rmajor,rminor,rpf2dewar,rrpf,rsldi,rsldo,scrapli,scraplo
    !  Input: shldith,shldoth,shldtth,tfcth,triang,vgap,vgap2,zh,zzpf

    !  Output: blarea,blareaib,blareaob,clh1,cryomass,dewmkg,rdewex
    !  Output: sharea,shareaib,shareaob,vdewex,vdewin,volblkt,volblkti
    !  Output: volblkto,volshld

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    !  External cryostat radius (m)
    !  rb(i) = outer radius of PF coil i (tokamaks)
    !  rrpf(i) = radius of RFP coil i (RFPs)

    if (irfp == 1) then
       rdewex = maxval(rrpf + 0.5D0*drpf) + rpf2dewar
    else  !  tokamaks
       rdewex = maxval(rb) + rpf2dewar
    end if

    !  Clearance between uppermost PF coil and cryostat lid
    !  Scaling from ITER by M. Kovari

    hcryopf = clhsf * (2.0D0*rdewex)/28.440D0

    !  Half-height of cryostat

    if (irfp /= 1) then
       zdewex = maxval(zh) + hcryopf
    else
       zdewex = maxval(zzpf + 0.5D0*dzpf) + hcryopf
    end if

    !  Vertical clearance between TF coil and cryostat

    clh1 = zdewex - (hmax + tfcth)

    !  External cryostat volume

    vdewex = ( (2.0D0*pi*rdewex) * 2.0D0*zdewex + &
         (2.0D0*pi*rdewex**2) ) * ddwex

    !  Internal vacuum vessel volume

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

    !  Vacuum vessel mass

    cryomass = vdewin * denstl

    !  Sum of internal vacuum vessel and external cryostat masses

    dewmkg = (vdewin + vdewex) * denstl

  end subroutine blshvv_volume

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dshellvol(rmajor,rminor,zminor,drin,drout,dz,vin,vout,vtot)

    !+ad_name  dshellvol
    !+ad_summ  Routine to calculate the inboard, outboard and total volumes
    !+ad_summ  of a D-shaped toroidal shell
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rmajor : input real : major radius to outer point of inboard
    !+ad_argc                        straight section of shell (m)
    !+ad_args  rminor : input real : horizontal internal width of shell (m)
    !+ad_args  zminor : input real : vertical internal half-height of shell (m)
    !+ad_args  drin   : input real : horiz. thickness of inboard shell at midplane (m)
    !+ad_args  drout  : input real : horiz. thickness of outboard shell at midplane (m)
    !+ad_args  dz     : input real : vertical thickness of shell at top/bottom (m)
    !+ad_args  vin    : output real : volume of inboard straight section (m3)
    !+ad_args  vout   : output real : volume of outboard curved section (m3)
    !+ad_args  vtot   : output real : total volume of shell (m3)
    !+ad_desc  This routine calculates the volume of the inboard and outboard sections
    !+ad_desc  of a D-shaped toroidal shell defined by the above input parameters.
    !+ad_desc  The inboard section is assumed to be a cylinder of uniform thickness.
    !+ad_desc  The outboard section's internal and external surfaces are defined
    !+ad_desc  by two semi-ellipses, centred on the outer edge of the inboard section;
    !+ad_desc  its volume is calculated as the difference in those of the volumes of
    !+ad_desc  revolution enclosed by the two surfaces.
    !+ad_desc  <P>See also <A HREF="dshellarea.html"><CODE>dshellarea</CODE></A>
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/13 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Internal CCFE note T&amp;M/PKNIGHT/PROCESS/009, P J Knight:
    !+ad_docc  Surface Area and Volume Calculations for Toroidal Shells
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rmajor,rminor,zminor,drin,drout,dz
    real(kind(1.0D0)), intent(out) :: vin,vout,vtot

    !  Local variables

    real(kind(1.0D0)) :: a,b,elong,v1,v2

    !  Global shared variables

    !  Input: pi,twopi

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Volume of inboard cylindrical shell

    vin = 2.0D0*(zminor+dz) * pi*(rmajor**2 - (rmajor-drin)**2)

    !  Volume enclosed by inner surface of elliptical outboard section
    !  and the vertical straight line joining its ends

    a = rminor ; b = zminor ; elong = b/a
    v1 = twopi * elong * (0.5D0*pi*rmajor*a*a + 2.0D0/3.0D0*a*a*a)

    !  Volume enclosed by outer surface of elliptical outboard section
    !  and the vertical straight line joining its ends

    a = rminor+drout ; b = zminor+dz ; elong = b/a
    v2 = twopi * elong * (0.5D0*pi*rmajor*a*a + 2.0D0/3.0D0*a*a*a)

    !  Volume of elliptical outboard shell

    vout = v2 - v1

    !  Total shell volume

    vtot = vin + vout

  end subroutine dshellvol

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine eshellvol(rshell,rmini,rmino,zminor,drin,drout,dz,vin,vout,vtot)

    !+ad_name  eshellvol
    !+ad_summ  Routine to calculate the inboard, outboard and total volumes
    !+ad_summ  of a toroidal shell comprising two elliptical sections
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rshell : input real : major radius of centre of both ellipses (m)
    !+ad_args  rmini  : input real : horizontal distance from rshell to outer edge
    !+ad_argc                        of inboard elliptical shell (m)
    !+ad_args  rmino  : input real : horizontal distance from rshell to inner edge
    !+ad_argc                        of outboard elliptical shell (m)
    !+ad_args  zminor : input real : vertical internal half-height of shell (m)
    !+ad_args  drin   : input real : horiz. thickness of inboard shell at midplane (m)
    !+ad_args  drout  : input real : horiz. thickness of outboard shell at midplane (m)
    !+ad_args  dz     : input real : vertical thickness of shell at top/bottom (m)
    !+ad_args  vin    : output real : volume of inboard section (m3)
    !+ad_args  vout   : output real : volume of outboard section (m3)
    !+ad_args  vtot   : output real : total volume of shell (m3)
    !+ad_desc  This routine calculates the volume of the inboard and outboard sections
    !+ad_desc  of a toroidal shell defined by two co-centred semi-ellipses.
    !+ad_desc  Each section's internal and external surfaces are in turn defined
    !+ad_desc  by two semi-ellipses. The volumes of each section are calculated as
    !+ad_desc  the difference in those of the volumes of revolution enclosed by their
    !+ad_desc  inner and outer surfaces.
    !+ad_desc  <P>See also <A HREF="eshellarea.html"><CODE>eshellarea</CODE></A>
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  09/05/13 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Internal CCFE note T&amp;M/PKNIGHT/PROCESS/009, P J Knight:
    !+ad_docc  Surface Area and Volume Calculations for Toroidal Shells
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rshell,rmini,rmino,zminor,drin,drout,dz
    real(kind(1.0D0)), intent(out) :: vin,vout,vtot

    !  Local variables

    real(kind(1.0D0)) :: a,b,elong,v1,v2

    !  Global shared variables

    !  Input: pi,twopi

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Inboard section

    !  Volume enclosed by outer (higher R) surface of elliptical section
    !  and the vertical straight line joining its ends

    a = rmini ; b = zminor ; elong = b/a
    v1 = twopi * elong * (0.5D0*pi*rshell*a*a - 2.0D0/3.0D0*a*a*a)

    !  Volume enclosed by inner (lower R) surface of elliptical section
    !  and the vertical straight line joining its ends

    a = rmini+drin ; b = zminor+dz ; elong = b/a
    v2 = twopi * elong * (0.5D0*pi*rshell*a*a - 2.0D0/3.0D0*a*a*a)

    !  Volume of inboard section of shell

    vin = v2 - v1

    !  Outboard section

    !  Volume enclosed by inner (lower R) surface of elliptical section
    !  and the vertical straight line joining its ends

    a = rmino ; b = zminor ; elong = b/a
    v1 = twopi * elong * (0.5D0*pi*rshell*a*a + 2.0D0/3.0D0*a*a*a)

    !  Volume enclosed by outer (higher R) surface of elliptical section
    !  and the vertical straight line joining its ends

    a = rmino+drout ; b = zminor+dz ; elong = b/a
    v2 = twopi * elong * (0.5D0*pi*rshell*a*a + 2.0D0/3.0D0*a*a*a)

    !  Volume of outboard section of shell

    vout = v2 - v1

    !  Total shell volume

    vtot = vin + vout

  end subroutine eshellvol

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine iterate_fw(afw,bfw,area,fwarea,decayfw,abktflnc,fwlife, &
       pnuc_incident,prad_incident,inlet_temp,outlet_temp,blleng,coolp, &
       fwerlim,pnuc_deposited,tpeakfw,cf,rhof,velfw)

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
    !+ad_args  fwarea : input real : total first wall area (m2)
    !+ad_args  decayfw : input real : decay length for neutron power deposition (m)
    !+ad_args  abktflnc : input real : allowable first wall neutron fluence (MW-yr/m2)
    !+ad_args  fwlife : input/output real : first wall lifetime (full-power years)
    !+ad_args  pnuc_incident : input real : incident neutron power (MW)
    !+ad_args  prad_incident : input real : incident radiation power (MW)
    !+ad_args  inlet_temp : input real : coolant inlet temperature (K)
    !+ad_args  outlet_temp : input real : coolant outlet temperature (K)
    !+ad_args  blleng : input real : poloidal length of pipe per segment (m)
    !+ad_args  coolp : input real : coolant pressure (Pa)
    !+ad_args  fwerlim : input real : maximum allowable erosion thickness loss (m)
    !+ad_args  pnuc_deposited : output real : neutron power deposited in FW (MW)
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

    real(kind(1.0D0)), intent(in) :: afw,decayfw,pnuc_incident,area,prad_incident, &
         inlet_temp,outlet_temp,blleng,coolp,fwerlim,fwarea,abktflnc
    real(kind(1.0D0)), intent(inout) :: bfw,fwlife
    real(kind(1.0D0)), intent(out) :: pnuc_deposited,tpeakfw,cf,rhof,velfw

    !  Local variables

    integer, parameter :: nk = 51
    integer :: it,k
    real(kind(1.0D0)) :: boa,fboa,flnce,fwlifs,fwvol,hcoeff,kf,masflx,maxstress, &
         mindif,qpp,qppp,sgpthn,tav,temp_c,temp_k,tfwav,tmpdif,tmprop_c,tmprop_k, &
         tmprse,tmthet,tpeakfw_c,viscf,viscfs

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    boa = bfw/afw
    tmprse = outlet_temp - inlet_temp

    it = 0
    iteration: do ; it = it+1

       if (it > 100) then
          call report_error(88)
          exit iteration
       end if

       !  Check to see if inner radius is greater than outer radius

       if (afw >= bfw) then
          fdiags(1) = afw ; fdiags(2) = bfw
          call report_error(89)
       end if

       !  Neutron power deposited in first wall
       !  The neutron power deposited in the first wall, breeder zone and shield is found
       !  by assuming an exponential decay of the net power transmitted radially through 
       !  the structure, i.e. P(x) = P(0)*exp(x/decayi), where decayi is an appropriate
       !  decay length for each structure. Further consideration of how to determine 
       !  this decay length to correctly account for different material compositions and
       !  properties is required.

       pnuc_deposited = pnuc_incident * (1.0D0 - exp(-2.0D0*bfw/decayfw))

       !  Assume that the first wall volume is equal to its surface area
       !  multiplied by the external diameter of the hollow cylindrical
       !  tubes that make up the first wall

       fwvol = area*(2.0D0*bfw)

       !  Heat fluxes
       !  qppp represents the heat generation in the first wall due to
       !  the neutron flux deposited in the material (W/m3)
       !  qpp represents the heat flux incident on the first wall
       !  surface from electromagnetic radiation flux (W/m2)

       qppp = 1.0D6 * pnuc_deposited / fwvol
       qpp = 1.0D6 * prad_incident / area

       !  Coolant properties at average coolant temperature
       !  tb is not known at this point, but since we only need the cf output (which does not
       !  depend on tb) we simply use outlet_temp as the input for tb

       call cprops(outlet_temp,inlet_temp,outlet_temp,coolp,cf,rhof,viscf,viscfs,kf)

       !  Coolant flow rate (kg/m2/s)

       masflx = blleng*(qppp*(bfw**2) + 2.0D0*qpp*bfw) &
            /afw**2 /cf /tmprse
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

       do k = 1,nk

          temp_k = outlet_temp + (1073.15D0-outlet_temp) * dble(k-1)/(nk-1)  !  in K
          temp_c = temp_k - 273.15D0

          !  hcoeff also depends on first wall temperature so the
          !  calculation of coolant properties and hcoeff is repeated

          call cprops(outlet_temp,inlet_temp,temp_k,coolp,cf,rhof,viscf,viscfs,kf)

          !  Heat transfer coefficient calculated using Sieder-Tate
          !  correlation, valid for Re > 1.0e3, 0.7 < Pr < 16700, L/D > 10

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

       call cprops(outlet_temp,inlet_temp,tmprop_k,coolp,cf,rhof,viscf,viscfs,kf)

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

       !  Fluence

       flnce = 1.0D-6*qppp * fwvol/area * fwlife

       !  Calculate peak temperature - occurs at (r,theta) = (bfw,0)

       call cosine_term(afw,bfw,0.0D0,bfw,qpp,hcoeff,tmprop_c,tmthet)

       tpeakfw = bfw/tk(tmprop_c) * (qpp/pi + qppp*bfw/2.0D0) &
            * log(bfw/afw) - qppp/4.0D0/tk(tmprop_c)*(bfw**2-afw**2) &
            + (pi*(bfw**2-afw**2)*qppp + 2.0D0*bfw*qpp) / &
            (2.0D0*pi*afw*hcoeff) + outlet_temp + tmthet  !  in K
       tpeakfw_c = tpeakfw - 273.15D0

       if ((tpeakfw > tfwmatmax).or.(flnce > abktflnc)) then
          !  Temperature or fluence limit exceeded; reduce first wall lifetime

          fwlife = abktflnc * area/fwvol / (1.0D-6*qppp)

          !  fboa is chosen such that fboa**100 * (bfw/afw) = 1.001,
          !  i.e. after 100 iterations bfw is still just larger than afw
          !  N.B. bfw may also have been modified via the stress test below...

          fboa = (1.001D0/boa)**0.01D0

          bfw = bfw*fboa
          if ((bfw/afw) <= 1.001D0) then
             write(*,*) 'Warning in routine ITERATE_FW:'
             write(*,*) 'Swelling limit exceeded, and'
             write(*,*) 'optimisation is failing to find a'
             write(*,*) 'suitable first wall thickness...'
             write(*,*) 'PROCESS continuing.'
             exit iteration
          else
             cycle iteration
          end if

       end if

       fwlifs = 3.1536D7*fwlife

       !  The lower limit on the first wall thickness is derived from the
       !  constraint that the first wall must possess the ability to withstand
       !  the internal coolant pressure. An additional thickness defined by
       !  user input "fwerlim" is required to account for erosion of the first
       !  wall surface over its lifetime. The limit is written as,
       !  (bfw-afw) - fwerlim > p*(afw+bfw)/2/maxstress

       sgpthn = (coolp*(afw+bfw)/2.0D0) / (bfw-afw-fwerlim)

       maxstress = smt(tpeakfw_c, fwlifs)

       if (sgpthn <= maxstress) then
          exit iteration
       else
          !  First wall too thin
          !  Keep afw fixed and alter bfw so that the lower limit
          !  is satisfied.

          bfw = ( afw*(maxstress + coolp/2.0D0) + fwerlim*maxstress ) / &
               (maxstress - coolp/2.0D0)
          write(*,*) 'FW too thin...',sgpthn,maxstress,tpeakfw_c
          write(*,*) 'FW too thin...',afw,bfw
          write(*,*) 'FW too thin...',coolp,fwerlim
       end if

    end do iteration

  end subroutine iterate_fw

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function pumppower(flleng,rad,mf,mfp,no90,no180,vel,inlet_temp, &
       outlet_temp,etaiso,coolwh,coolp)

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
    !+ad_args  inlet_temp  : input real : coolant inlet temperature (K)
    !+ad_args  outlet_temp : input real : coolant outlet temperature (K)
    !+ad_args  etaiso      : input real : isentropic efficiency of coolant pumps
    !+ad_args  coolwh      : input integer: coolant fluid (1=helium, 2=water)
    !+ad_args  coolp       : input real : coolant pressure (Pa)
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
    !+ad_prob  The functions to find enthalpies should come from REFPROP
    !+ad_call  cprops
    !+ad_call  smt
    !+ad_hist  04/09/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  WCLL DDD, WP12-DAS02-T03, J. Aubert et al, EFDA_D_2JNFUP
    !+ad_docs  A Textbook on Heat Transfer, S.P. Sukhatme, 2005
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: pumppower  !  MW

    !  Arguments

    real(kind(1.0D0)), intent(in) :: flleng,rad,mf,mfp,vel,inlet_temp,outlet_temp, &
         etaiso,coolp
    integer, intent(in) :: no90,no180,coolwh

    !  Local variables

    real(kind(1.0D0)) :: cf,coolpin,deltap,dh,kelbwn,kelbwt,kf,kstrght,lambda, &
         ppump,reyn,rhof,viscf,viscfs,xifn,xift,ximn,ximt

    !  Global shared variables

    !  Inputs: pi

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Get fluid properties (only viscf and rhof are needed)

    call cprops(outlet_temp,inlet_temp,outlet_temp,coolp,cf,rhof,viscf,viscfs,kf)

    !  Hydraulic diameter (circular channels assumed) (m)

    dh = 2.0D0*rad

    !  Reynolds number

    reyn = 4.0D0*mfp / (pi*dh*viscf)

    !  Darcy friction factor, using Filonenko equation
    !  valid for 1.0e4 < Re < 1.0e7 (Sukhatme)

    if ((reyn < 1.0D4).or.(reyn > 1.0D7)) then
       !+PJK Add proper error message
       !  Print warning that pressure drop calculation is being performed 
       !  outside the range of validity of friction factor correlation
       write(*,*) 'Warning... pressure drop calculation is being performed'
       write(*,*) 'outside the range of validity of the friction factor correlation.'
       write(*,*) 'Reyn = ',reyn,mfp,dh,viscf
    end if
    lambda = 1.0D0 / (1.8D0*log(reyn) - 1.64D0)**2

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

    !  Total pressure drop, dividing by 1.0e6 to get MPa

    deltap = 1.0D-6 * (kstrght + no90*kelbwn + no180*kelbwt) * 0.5D0*rhof*vel*vel

    !  Pumping power

    if (coolwh == 2) then  !  water coolant

       !  Incompressible fluid pumping power (MW)

       ppump = deltap*mf / (rhof*etaiso)

    else  !  helium coolant

       !+PJK Treating as for water for now; awaiting helpropps,helproppt routines
       ppump = deltap*mf / (rhof*etaiso)

       !  Compressible fluid pumping power (MW)

       !  Inlet pressure

       !coolpin = 1.0D-6*coolp + deltap

       !  Obtain inlet enthalpy and entropy from inlet pressure and temperature

       !call helproppt(coolpin,inlet_temp,h2,s2)

       !  Assume isentropic pump so that s1 = s2

       !s1 = s2

       !  Get enthalpy before pump using coolp and s1

       !call helpropps(coolp,s1,h1)

       !  Pumping power, dividing by 1000(?) to get MW

       !ppump = 1.0D-3 * mf*(h2-h1) / etaiso

    end if

    pumppower = ppump

  contains
    !+PJK to do...
    subroutine helproppt()
      !- helproppt - function to find enthalpy and entropy of helium, given pressure
      !              and temperature
      ! This should be taken from REFPROP routines
      !Inputs: p,t
      !Outputs: h,s

      !  Calculate enthalpy from pressure and temperature (kJ/kg)

      !h = f(p,t)

      !  Calculate entropy from pressure and temperature (kJ/kg/K)

      !s = f(p,t)

    end subroutine helproppt

    subroutine helpropps()
      !- helpropps - function to find enthalpy of helium, given pressure and entropy
      ! This should be taken from REFPROP routines
      !Inputs: p,s
      !Outputs: h

      !  Calculate enthalpy from pressure and entropy (kJ/kg)

      !h = f(p,s)

    end subroutine helpropps
    !-PJK
  end function pumppower

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cprops(tfo,tfi,tb,pf,cf,rhof,viscf,viscfs,kf)

    !+ad_name  cprops
    !+ad_summ  Calculates various temperature dependent coolant properties
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  tfo    : input real : coolant outlet temperature (K)
    !+ad_args  tfi    : input real : coolant inlet temperature (K)
    !+ad_args  tb     : input real : bulk temperature (K)
    !+ad_args  pf     : input real : coolant outlet pressure (Pa)
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
    !+ad_prob  Fluid properties should be taken from REFPROP (those currently given
    !+ad_prob  here are taken from Panos Karditsas's original subroutine 'props')
    !+ad_call  None
    !+ad_hist  04/09/14 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
    !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
    !+ad_docc  Dept., Culham Laboratory, Abingdon
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: tfo,tfi,tb,pf
    real(kind(1.0D0)), intent(out) :: cf,rhof,viscf,viscfs,kf

    !  Local variables

    real(kind(1.0D0)) :: x,y,gascf,kfs,cfs,pran,prans,tbc

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  x: average coolant temperature (K)
    !  y: average coolant temperature at the material surface (K)

    x = 0.5D0*(tfo + tfi)
    y = x + 0.5D0*( (tb-tfo) + (tb-tfi) )

    if (coolwh == 1) then  !  Helium coolant

       !  Helium properties, to come from REFPROP routines
       !  Panos's correlations currently provided as placeholders

       !  Specific heat capacity of gaseous coolant (J/kg/K)

       gascf = 8314.3D0/4.0026D0

       !  Density (kg/m3)

       rhof = pf / (gascf*x)

       !  Thermal conductivity (W/m/K)
       !  N.B. REFPROP gives kf in milliWatts/m/K

       kf = 0.033378D0 + 4.2674D-04*x - 1.0807D-07*x*x

       !  Dynamic viscosity of the fluid at the bulk temperature (Pa-s)
       !  N.B. REFPROP gives viscosity in uPa-s

       viscf  = 4.7744D-07*x**0.6567D0

       !  Dynamic viscosity of the fluid at the wall temperature (Pa-s)

       viscfs = 4.7744D-07*y**0.6567D0

       !  Specific heat capacity at constant pressure (J/kg/K)

       cf = 5193.0D0

    else  !  Pressurized water coolant

       !  Water properties, to come from REFPROP routines
       !  Panos's correlations currently provided as placeholders

       !  Thermal conductivity (W/m/K)

       kf  = 8.9372D0 - 0.048702D0*x + 9.6994D-05*x*x - 6.5541D-08*x*x*x
       kfs = 8.9372D0 - 0.048702D0*y + 9.6994D-05*y*y - 6.5541D-08*y*y*y

       !  Specific heat capacity at constant pressure (J/kg/K)

       cf  = -371240.0D0 + 2188.7D0*x - 4.2565D0*x*x + 0.0027658D0*x*x*x
       cfs = -371240.0D0 + 2188.7D0*y - 4.2565D0*y*y + 0.0027658D0*y*y*y

       !  Prandtl number

       pran  = -68.469D0 + 0.41786D0*x - 8.3547D-04*x*x + 5.5443D-07*x*x*x
       prans = -68.469D0 + 0.41786D0*y - 8.3547D-04*y*y + 5.5443D-07*y*y*y

       !  Density (kg/m3)

       rhof = 15307.0D0 - 81.04D0*x + 0.15318D0*x*x - 9.8061D-05*x*x*x

       !  Dynamic viscosity (Pa-s)

       viscf = kf*pran/cf
       viscfs = kfs*prans/cfs

    end if

  end subroutine cprops

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cosine_term(afw,bfw,angle,rad,qpp,hcoeff,tmprop,tmthet)

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

    real(kind(1.0D0)), intent(in) :: afw,bfw,angle,rad,qpp,hcoeff,tmprop
    real(kind(1.0D0)), intent(out) :: tmthet

    !  Local variables

    integer :: i,k
    real(kind(1.0D0)) :: cc,dd

    !  Global shared variables

    !  Inputs: pi

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

    real(kind(1.0D0)) :: tk

    !  Arguments

    real(kind(1.0D0)), intent(in) :: t

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !if (first wall material is Eurofer) then
    !
    !   ! Eurofer correlation, from "Fusion Demo Interim Structural Design Criteria - 
    !   ! Appendix A Material Design Limit Data", F. Tavassoli, TW4-TTMS-005-D01, 2004
    !
    !   tk = 5.4308D0 + 0.13565D0*t - 0.00023863D0*t*t + 1.3393D-7*t*t*t
    !
    !else if (first wall material is 316 stainless steel)
    !
    !   ! 316 stainless steel correlation, from pulse.f90 module
    !
    tk = 3.78D0 * t**0.28D0

  end function tk

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function smt(t,fwlifs)

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

    real(kind(1.0D0)) :: smt

    !  Arguments

    real(kind(1.0D0)), intent(in) :: t,fwlifs

    !  Local variables

    real(kind(1.0D0)) :: smt400,smt500,smt600,lnpwr

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

  function tsat(p)

    !+ad_name  satliq
    !+ad_summ  Saturation temperature of water as a function of pressure
    !+ad_type  Function returning real
    !+ad_auth  P Karditsas, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  p  : input real : saturated liquid/steam pressure (MPa)
    !+ad_desc  This routine calculates the saturation temperature (K) of
    !+ad_desc  water given the pressure. Currently, the method is taken
    !+ad_desc  from Panos's satliq routine.
    !+ad_prob  This should be updated with the REFPROP calculation.
    !+ad_call  None
    !+ad_hist  04/09/14 PJK Initial version
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

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Saturated liquid properties

    ta1 = 168.396D0
    ta2 =   0.314653D0
    ta3 =  -0.000728D0
    ta4 =  31.588979D0
    ta5 =  11.473141D0
    ta6 =  -0.575335D0
    ta7 =   0.013165D0

    tsat = 273.15D0 + ta1 + ta2/p + ta3/p**2 + ta4*log(p) &
         + ta5*p + ta6*p*p + ta7*p*p*p

  end function tsat

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blanket_neutronics_hcpb_kit

    !+ad_name  blanket_neutronics_hcpb_kit
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

    !  Global shared variables

    !  Input: blareaib,blareaob,blbmith,blbmoth,blbpith,blbpoth,blbuith
    !  Input: blbuoth,breedmat,cfactr,ddwi,fblbe,fblbreed,fblhebmi,fblhebmo
    !  Input: fblhebpi,fblhebpo,fblss,fdiv,fhcd,fhole,fwareaib,fwareaob
    !  Input: fwith,fwoth,hcdportsize,ipowerflow,li6enrich,npdiv,nphcdin
    !  Input: nphcdout,pneutmw,shareaib,shareaob,shldith,shldoth,tdwell
    !  Input: tlife,tpulse,tramp,wallmw,wallpf

    !  Output: bktlife,emult,nflutf,pnucblkt,pnucshld,ptfnuc,ptfnucpm3
    !  Output: tbr,tritprate,vvhemax

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

  end subroutine blanket_neutronics_hcpb_kit

end module fwbs_module
