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
  !+ad_cont  fwbs
  !+ad_cont  dshellvol
  !+ad_cont  eshellvol
  !+ad_cont  blanket
  !+ad_cont  blnkt
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
  public :: fwbs, blanket, blanket_neutronics

  !  Local variables

  real(kind(1.0D0)) :: a,g0,pf,reyd
  real(kind(1.0D0)) :: tc,tol,tso
  real(kind(1.0D0)) :: hle,hli,hre,hri,sle,sli,sre,sri,tre
  integer :: ncc

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
    !+ad_hist  08/01/15 JM  Changed default blanket model to have ss, TiBe12 and Li4SiO4
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

    real(kind(1.0D0)) :: coilhtmx,decaybl,dpacop,dshieq,dshoeq,elong, &
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

       if (lblnkt == 1) then
          if (smstr == 1) then  !  solid blanket
             decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
          else  !  liquid blanket
             decaybl = 0.075D0 / (1.0D0 - vfblkt - fbllipb - fblli)
          end if
       else  !  original blanket model - solid blanket
          decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
       end if

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

       if (lblnkt == 1) then
          if (smstr == 1) then  !  solid blanket
             decaybl = declblkt / (1.0D0 - vfblkt - fblli2o - fblbe)
          else  !  liquid blanket
             decaybl = declblkt / (1.0D0 - vfblkt - fbllipb - fblli)
          end if
       else  !  original blanket model - solid blanket
          decaybl = declblkt / (1.0D0 - vfblkt - fblli2o - fblbe)
       end if

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

    !  Default blanket model now changed to ss, TiBe12 and Li4SiO4
    if (blktmodel == 0) then
       whtblss = volblkt * denstl * fblss
       whtbltibe12 = volblkt * 2260.0D0  * fbltibe12  ! 2.26 g cm^-3
       whtblli4sio4 = volblkt * 2400.0D0  * fblli4sio4 ! 2.4 g cm^-3
       whtblkt = whtblss + whtblli4sio4 + whtbltibe12
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

    !  Thermodynamic blanket model
    !  (supersedes above calculations of blanket mass and volume)

    if (lblnkt == 1) then
       call blanket(1,outfile,iprint)

       !  Different (!) approximation for inboard/outboard
       !  blanket volumes: assume cylinders of equal heights

       r1 = rsldi + shldith + 0.5D0*blnkith
       r2 = rsldo - shldoth - 0.5D0*blnkoth
       volblkti = volblkt * (r1*blnkith)/((r1*blnkith)+(r2*blnkoth))
       volblkto = volblkt * (r2*blnkoth)/((r1*blnkith)+(r2*blnkoth))

    end if

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

    if ((blktmodel > 0).or.(costr == 2)) then  !  pressurised water coolant
       coolmass = coolvol*806.719D0
    else  !  gaseous helium coolant
       coolmass = coolvol*1.517D0
    end if

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

    cryomass = vdewin * denstl

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

    if (lblnkt == 1) then
       if (smstr == 1) then
          write(outfile,600) volblkti, volblkto, volblkt,  &
               whtblkt, vfblkt, fblbe, whtblbe, fblli2o, wtblli2o,  &
               fblss, whtblss, volshldi, volshldo,  &
               volshld, whtshld, vfshld, wpenshld
       else
          write(outfile,601) volblkti, volblkto, volblkt,  &
               whtblkt, vfblkt, fbllipb, wtbllipb, fblli, whtblli,  &
               fblss, whtblss, volshldi, volshldo,  &
               volshld, whtshld, vfshld, wpenshld
       end if
    else if (blktmodel == 0) then
       write(outfile,600) volblkti, volblkto, volblkt, whtblkt, vfblkt, &
            fblbe, whtblbe, fblli2o, wtblli2o, fblss, whtblss, &
            volshldi, volshldo, volshld, whtshld, vfshld, &
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
    call ovarre(outfile,'Total cryostat + vacuum vessel mass (kg)','(dewmkg)',dewmkg)
    call ovarre(outfile,'Internal vacuum vessel volume (m3)','(vdewin)',vdewin)
    call ovarre(outfile,'Vacuum vessel mass (kg)','(cryomass)',cryomass)
    call ovarre(outfile,'Divertor area (m2)','(divsur)',divsur)
    call ovarre(outfile,'Divertor mass (kg)','(divmas)',divmas)

  end subroutine fwbs

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

  subroutine blanket(icalc,outfile,iprint)

    !+ad_name  blanket
    !+ad_summ  Caller for the detailed blanket thermodynamic model
    !+ad_type  Subroutine
    !+ad_auth  P Karditsas, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  icalc : input integer : 1 = calculate volume, 2 = calculate electric power
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  Synopsis of model: 
    !+ad_desc  <P><B>(a) Blanket structure</B>
    !+ad_desc  <P>The blanket is assumed to consist of NR*NP*NT blocks of
    !+ad_desc  dimension LRxLPxW, with the coolant travelling through
    !+ad_desc  channels of: hydraulic diameter DH1; cross-sectional
    !+ad_desc  area AC; perimeter S; mass flow rate MC; and specific
    !+ad_desc  heat capacity CPC. The channel orientation can be either
    !+ad_desc  poloidal or radial; also a circular or annular channel
    !+ad_desc  can be chosen.
    !+ad_desc  <P>Energy balance on a elemental volume dV=ACdX (dX
    !+ad_desc  is either the radial or poloidal distance) leads to
    !+ad_desc  an equation of the form shown below.
    !+ad_desc  <PRE>
    !+ad_desc            d TC                        2           
    !+ad_desc    MC CPC  ----     =     QT(X,THETA) W  ( 1 - YC )
    !+ad_desc            d X
    !+ad_desc  </PRE>
    !+ad_desc  QT(X,THETA) is the volumetric heat deposition as a function
    !+ad_desc  of both X and the toroidal variable THETA.
    !+ad_desc  <P>The equation above can be integrated over the range
    !+ad_desc  (0--->LR or LP), this results in an expression for the
    !+ad_desc  outlet coolant temperature in terms of the inlet coolant
    !+ad_desc  temperature.
    !+ad_desc  <P><B>(b) Coolant pumping power</B>
    !+ad_desc  <P>Only the losses incurred in the cooling channel are considered, 
    !+ad_desc  with the pressure drop along the length LK (K=P or R) of a
    !+ad_desc  single channel given by:
    !+ad_desc  <PRE>
    !+ad_desc                              2
    !+ad_desc    dP = fric 2 LK/DH1 (MC/AC)  1/rho
    !+ad_desc  </PRE>
    !+ad_desc  fric: friction factor (dependent on the flow Reynolds number)
    !+ad_desc  <BR>rho: coolant mass density (kg/m**3)
    !+ad_desc  <BR>Therefore the total pumping power for the NR*NP*NT cooling 
    !+ad_desc  channels is:
    !+ad_desc  <PRE>
    !+ad_desc    P = NR*NP*NT*MC*dP/rho
    !+ad_desc  </PRE>
    !+ad_desc  <P><B>(c) Thermal energy conversion</B>
    !+ad_desc  <P>A Rankine cycle with reheat and regeneration is employed
    !+ad_desc  with water/steam as the working fluid. The cycle is 
    !+ad_desc  summarised below:
    !+ad_desc  <OL>
    !+ad_desc  <P><LI> Water (condensate) is passed through the steam 
    !+ad_desc         generator (SG) (TB1,HB1,PH)---->(TB2=TH1,HB2=HH1,PH)
    !+ad_desc  <P><LI> The steam then enters a high pressure (HP) turbine where
    !+ad_desc         it is partially expanded ----->(TH2,HH2,PR=PI)
    !+ad_desc  <P><LI> It is then reheated and enters a intermediate pressure (IP)
    !+ad_desc         turbine (TI1,HI1,PI)---->(TI2,HI2=HL1,PL)
    !+ad_desc  <P><LI> It then enters a low pressure turbine (LP) 
    !+ad_desc         (TL1,HL1,PL)---->(TL2=TC,HL2=HC,PC) to the condenser.
    !+ad_desc  </OL>
    !+ad_desc  Syntax: '1' - inlet; '2' - outlet; 'H' - high; 'L' - low;
    !+ad_desc          'I' - intermediate; 'R' - reheat; 'B' - boiler;
    !+ad_desc          and 'C' - condenser.
    !+ad_desc  <P>Regeneration is achieved by extracting a small fraction of the
    !+ad_desc  superheated steam from the IP and LP turbines directing it to
    !+ad_desc  the feed water heaters, where the excess energy is given off.
    !+ad_desc  The type of feed water heaters used in the analysis are
    !+ad_desc  termed `open' and the condensate is compressed to the
    !+ad_desc  pressure of the extracted steam.
    !+ad_desc  <P>The enthalpy rise due to the feed pump compression is:
    !+ad_desc  <PRE>
    !+ad_desc                       P  - P 
    !+ad_desc                        k    k+1
    !+ad_desc       DH      =  v    ---------
    !+ad_desc         k+1       k+1   eta                 
    !+ad_desc                            k+1
    !+ad_desc  v   : the specific volume (m**3/kg)
    !+ad_desc   k+1
    !+ad_desc  eta   : isentropic efficiency 
    !+ad_desc     k+1
    !+ad_desc  </PRE>
    !+ad_desc  <P><B>(d) Cycle efficiency</B>
    !+ad_desc  <P>The cycle efficiency is the net power output over the heat input.
    !+ad_desc  To calculate this quantity the enthalpies and mass flow rates
    !+ad_desc  must be known at all points in the cycle. 
    !+ad_desc  <P>A more detailed description of the physical models
    !+ad_desc  used in the analysis can be found in the main reference.
    !+ad_prob  None
    !+ad_call  blnkt
    !+ad_call  perim
    !+ad_hist  --/--/-- PK  Initial version
    !+ad_hist  25/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  11/04/13 PJK Modified beryllium density
    !+ad_stat  Okay
    !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
    !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
    !+ad_docc  Dept., Culham Laboratory, Abingdon
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile,icalc

    !  Local variables

    real(kind(1.0D0)) :: rm,ap,yc,xlr,xlp,xlf,volbl,wnet1
    real(kind(1.0D0)), dimension(3) :: qfus

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    xlr = 0.5D0 * (blnkith+blnkoth)
    xlf = 0.5D0 * (fwith+fwoth)
    xlp = perim(rminor+xlf+xlr, fkblkt*kappa, triang)
    xlr = xlr*100.0D0
    xlp = xlp/4.0D0*100.0D0
    rm = rmajor
    ap = rminor
    yc = vfblkt
    qfus(1) = pnucblkt
    qfus(2) = pnucshld
    qfus(3) = pfwdiv

    !  Assume that the cooling channels in both the first wall/divertor
    !  and shield can be changed in such a fashion so that the
    !  temperature difference between the coolant inlet and outlet is
    !  identical to the difference calculated for the blanket. If one
    !  accepts this assumption then the shield and first wall/divertor
    !  can easily be incorporated into the Rankine cycle. 

    call blnkt(rm,ap,qfus,yc,xlr,xlp,volbl,wnet1,icalc,outfile,iprint)

    !  Volume and mass of blanket

    volblkt = volbl

    if (smstr == 1) then
       wtblli2o = volblkt * fblli2o * 2010.0D0
       whtblbe = volblkt * fblbe * 1850.0D0  !  density changed from 1900 kg/m3
       whtblkt = wtblli2o + whtblbe
    else if (smstr == 2) then
       wtbllipb = volblkt * fbllipb * 9400.0D0
       whtblli = volblkt * fblli * 534.0D0
       whtblkt = wtbllipb + whtblli
    end if

    whtblkt = whtblkt + volblkt*(5870.0D0*fblvd + denstl*fblss)

    if (icalc == 2) pgrossmw = wnet1

  end subroutine blanket

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine blnkt(rm,ap,xqfus,yc,xlr,xlp,vol,wnet1,icalc,outfile,iprint)

    !+ad_name  blnkt
    !+ad_summ  Detailed thermodynamic model for the blanket
    !+ad_type  Subroutine
    !+ad_auth  P Karditsas, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  flow
    !+ad_cont  props
    !+ad_cont  satliq
    !+ad_cont  supvap
    !+ad_cont  supvap1
    !+ad_cont  supvap2
    !+ad_cont  supvap3
    !+ad_cont  twophas1
    !+ad_cont  twophas2
    !+ad_cont  twophase
    !+ad_args  rm       : input real : plasma major radius (m)
    !+ad_args  ap       : input real : plasma minor radius (m)
    !+ad_args  xqfus(3) : input real array : nuclear heating (MW) in (1) blanket,
    !+ad_argc                                (2) shield, (3) first wall/divertor
    !+ad_args  yc       : input/output real : coolant fraction 
    !+ad_args  xlr      : input real : blanket thickness (cm)
    !+ad_args  xlp      : input real : quarter poloidal extent (cm)
    !+ad_args  vol      : output real : total blanket volume (m**3)
    !+ad_args  wnet1    : output real : net electric power (MW)
    !+ad_args  icalc    : input integer : 1 = calculate volume, 2 = calculate electric power
    !+ad_args  outfile     : input integer : output file unit
    !+ad_args  iprint   : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine provides a detailed thermodynamic model for the blanket.
    !+ad_desc  For a synopsis of the model, see the description in routine
    !+ad_desc  <A HREF="blanket.html">blanket</A>.
    !+ad_prob  None
    !+ad_call  flow
    !+ad_call  oblnkl
    !+ad_call  ocentr
    !+ad_call  ocmmnt
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarrf
    !+ad_call  props
    !+ad_call  satliq
    !+ad_call  supvap
    !+ad_call  supvap1
    !+ad_call  supvap2
    !+ad_call  supvap3
    !+ad_call  twophas1
    !+ad_call  twophas2
    !+ad_call  twophase
    !+ad_hist  --/--/-- PK  Initial version
    !+ad_hist  27/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  15/05/14 PJK Increased output width to 110 characters
    !+ad_stat  Okay
    !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
    !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
    !+ad_docc  Dept., Culham Laboratory, Abingdon
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rm,ap,xlr,xlp
    real(kind(1.0D0)), intent(inout) :: yc
    real(kind(1.0D0)), intent(in), dimension(3) :: xqfus
    integer, intent(in) :: icalc,outfile,iprint
    real(kind(1.0D0)), intent(out) :: vol,wnet1

    !  Local variables

    real(kind(1.0D0)) :: ac,asurf,asurface,atotal,c1,c2,cf,d0,df0, &
         dg,dh1,dhip,dhlp,di,dpf,eta,etacycle,etaplant,f0,fb0,ff,ff0, &
         fi,fo,g,ha1,ha2,ha3,ha4,ha5,ha6,ha7,hc,hg1,hgc,hgh,hgin, &
         hgr,hhe,hhes,hhi,hhsat,hi,hinsat,hles,hres,hrsat,hsat1, &
         hsi,hso,htc,kf,kfs,ksolid,lc,lp,lr,mf,mf1,mf2,mp,ms,mtb,p1, &
         per,peri,pero,pran,prans,px,qav,qc,qfusion,qin,qpump,qr, &
         qsg,rhof,rhohi,rhori,sg1,sgc,sgh,sgin,sgr,shi,si,sum1,sum2, &
         sum3,sum4,tb,tfi,tfo,thes,thsat,ti,tinsat,tle,tles,tres, &
         trsat,ts,tsat1,tsi,vc,vhi,vhsat,vinsat,viscf,viscfs,vri,vrsat, &
         vsat1,w,wcp,whpt,winpfp,winpt,wlpfp,wlpt,wnet,wp1,wst,xexit, &
         xmf,xo,xqav,xtso
    real(kind(1.0D0)), dimension(0:10) :: dh,h,hg,hsat,m,mb,p,s,sg,t, &
         tsat,vsat,wp,wt
    integer :: i,np,npi,npo,nr,nri,nro,nt,nti,nto

    integer, parameter :: width = 110

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    tol = 0.0001D0

    !  Important local variables
    !  -------------------------
    ! 
    !  nt     (INT): number of cooling channels in toroidal direction
    !  per     (DP): perimeter of cooling channel (m)
    !  ac      (DP): cooling channel cross-sectional area (m**2)
    !  asurf   (DP): cooling channel surface area (m**2)
    !  w       (DP): cooling channel dimension (m)
    !  xo      (DP): thickness of 1-D slab (m)
    !  g0      (DP): geometrical factor
    !  nr     (INT): number of cooling channels in radial direction
    !  np     (INT): number of cooling channels in poloidal direction
    !  ncc    (INT): total  number of cooling channels 
    !  atotal  (DP): total blanket cross-sectional area (m**2)
    !  asurface (DP): total blanket surface area (m**2)
    !  vol     (DP): total blanket volume (m**3)
    !  qav     (DP): average volumetric heating (MW/m**3)
    !  mf      (DP): coolant mass flow rate (kg/s)
    !  ms      (DP): flow rate of steam (kg/s)
    !  m(i)    (DP): condensate flow rate into i'th FWH (kg/s)
    !  mb(i)   (DP): mass flow rate at i'th extraction point (kg/s)
    !  tso     (DP): SG output temperature (K)
    !  tsi     (DP): SG input temperature (K)
    !  hso     (DP): SG output enthalpy (kJ/kg)
    !  hsi     (DP): SG input enthalpy (kJ/kg)
    !  hhe     (DP): HP outlet enthalpy (kJ/kg)
    !  hhi     (DP): HP inlet enthalpy (kJ/kg)
    !  shi     (DP): HP inlet entropy (kJ/kg)
    !  hhes    (DP): HP outlet isentropic enthalpy (kJ/kg)
    !  hri     (DP): IP inlet enthalpy (kJ/kg)
    !  hre     (DP): IP outlet enthalpy (kJ/kg)
    !  sri     (DP): IP inlet entropy (kJ/kg)
    !  sre     (DP): IP outlet entropy (kJ/kg)
    !  hres    (DP): IP outlet isentropic enthalpy (kJ/kg)
    !  hli     (DP): LP inlet enthalpy (kJ/kg)
    !  hle     (DP): LP outlet enthalpy (kJ/kg)
    !  sli     (DP): LP inlet entropy (kJ/kg)
    !  sle     (DP): LP outlet entropy (kJ/kg)
    !  hles    (DP): LP outlet isentropic enthalpy (kJ/kg)
    !  hc      (DP): Condenser outlet enthalpy (kJ/kg)
    !  p(i)    (DP): saturated liquid pressure at FWHPi inlet (Pa)
    !  hsat(i) (DP): saturated liquid enthalpy at FWHPi inlet (kJ/kg)
    !  tsat(i) (DP): saturated liquid temperature at FWHPi inlet (K)
    !  vsat(i) (DP): saturated liquid spec. vol. at FWHPi inlet (m**3/kg)
    !  sg(i)   (DP): saturated vapour entropy at FWHPi inlet (kJ/kg)
    !  hg(i)   (DP): saturated vapour enthalpy at FWHPi inlet (kJ/kg)
    !  h(i)    (DP): enthalpy at i'th extraction point (kJ/kg)
    !  s(i)    (DP): entropy at i'th extraction point (kJ/kg)
    !  t(i)    (DP): temperature at i'th extraction point (K)
    !  dh(i)   (DP): enthalpy rise due to i'th FWHP (kJ/kg)
    !  wt(*)   (DP): power output from turbines (MW)
    !  wp(*)   (DP): power input to FWHPs (MW)

    !  Ensure that W >= d0*1.25 for both circular and annular
    !  geometry.

    if ( yc > pi/4.0D0/(1.25D0)**2 ) &
         yc = 0.99D0*pi/4.0D0/(1.25D0)**2
    if (astr == 2) then
       if ( xdi/xdo > sqrt(1.0D0 - 4.0D0*(1.25D0)**2 * yc/pi) ) &
            xdo = 1.01D0*xdi/( sqrt(1.0D0 - 4.0D0*(1.25D0)**2 * yc/pi) )
    end if

    qfusion = xqfus(1)*1.0D6
    tfi = xtfi + 273.15D0
    pf = xpf * 1.0D6

    lr = xlr/100.0D0
    lp = xlp/100.0D0

    if (estr == 1) then
       lc = lr
    else
       lc = lp
    end if

    !  Calculate the number of toroidal blocks together with
    !  various geometric features of the cooling channels

    if (astr == 1) then  !  Circular cooling channels

       d0 = xdo/100.0D0
       dh1 = d0
       per = pi*d0
       ac = pi*d0**2 / 4.0D0
       asurf = per*lc
       w = sqrt(ac/yc)
       nto = int(2.0D0*pi*(Rm+ap)/w)
       nti = int(2.0D0*pi*(Rm-ap)/w)
       nt = nto + nti
       g0 = (1.0D0/16.0D0)*pi*d0*(1.0D0 - d0/w)**2 / (1.0D0-yc)
       xo = 0.5D0*(w - d0)

    else  !  Annular cooling channels

       d0 = xdo/100.0D0
       di = xdi/100.0D0
       dh1 = d0 - di
       per = pi*(d0 + di)
       ac = pi*(d0**2 - di**2)/4.0D0
       asurf = per*lc
       pero = pi*d0
       peri = pi*di
       w = sqrt(ac/yc)
       fo = 1.0D0 - 0.25D0*pi*(d0/w)**2
       fi = 0.25D0*pi*(di/w)**2
       nto = int(2.0D0*pi*(rm + ap)/w)
       nti = int(2.0D0*pi*(rm - ap)/w)
       nt = nto + nti
       g0 = (1.0D0/16.0D0) * ( fo*pi*d0*(1.0D0 - d0/w)**2 / (1.0D0-yc) &
            + fi*pi*Di*(di/w)**2 / (1.0D0-yc) )
       xo = sqrt(fo*0.25D0*(pero/per)*(w-d0)**2 &
            + 0.25D0*fi*(peri/per)*di**2)

    end if

    !  Calculate the total number of cooling channels (this is
    !  equivalent to the total number of blocks). The reactor
    !  vessel is not completely surrounded by the blanket;
    !  there are gaps between different sections of the 
    !  blanket (80 % coverage is assumed).

    if (estr == 1) then  !  Cooling channels are orientated in the radial direction
       nr = int(lp/w)
       np = 0
       ncc = int(2.0D0*0.8D0*dble(nt*nr))
    else  !  Cooling channels are orientated in the poloidal direction
       np = int(lr/w)
       nr = 0
       ncc = int(2.0D0*0.8D0*dble(nt*np))
    end if

    !  Calculate the total blanket area, volume and surface area

    atotal = dble(ncc)*w*w
    vol = atotal*lc
    asurface = 4.0D0*pi*rm*(lp*2.0D0)

    if (icalc == 1) return

    !  Boundary condition

    if (bstr == 1) then  !  fixed coolant outlet temperature
       tfo = xtfo + 273.15D0
    else  !  fixed maximum blanket temperature
       tb = xtb + 273.15D0
    end if

    if (iprint == 1) then

       call oblnkl(outfile)
       call ocentr(outfile,'Blanket Input Data',width)
       call oblnkl(outfile)

       call ovarrf(outfile,'fusion power (MW)','(xqfusion)',xqfus(1))
       call ovarrf(outfile,'blanket coolant inlet temp (C)','(xtfi)', &
            xtfi)
       call ovarrf(outfile,'blanket coolant inlet pressure (MPa)', &
            '(xpf)',xpf)
       call ovarrf(outfile,'Radial blanket length (cm)','(xlr)',xlr)
       call ovarrf(outfile,'Poloidal (1/4) blanket length (cm)','(xlp)' &
            ,xlp)
       call ovarrf(outfile,'Reactor major radius (m)','(Rm)',Rm)
       call ovarrf(outfile,'Reactor minor radius (m)','(ap)',ap)
       call ovarrf(outfile,'Coolant fraction','(yc)',yc)

       if (astr == 1) then
          call ovarrf(outfile,'outside diameter (cm)','(xdo)',xdo)
       else
          call ovarrf(outfile,'outside diameter (cm)','(xdo)',xdo)
          call ovarrf(outfile,'inside diameter (cm)','(xdi)',xdi)
       end if

       if (bstr == 1) then
          call osubhd(outfile,'Output coolant temperature is fixed')
          call ovarrf(outfile,'blanket coolant outlet temp (C)','(xtfo)' &
               ,xtfo)
       else
          call osubhd(outfile,'Maximum blanket temperature is fixed')
          call ovarrf(outfile,'blanket max allowable temp (C)','(xtb)', &
               xtb)
       end if

       if (estr == 1) then
          call osubhd(outfile,'Cooling channels radially orientated')
       else
          call osubhd(outfile,'Cooling channels poloidally orientated')
       end if

       if (astr == 1) then
          call osubhd(outfile,'Circular cooling channels')
       else
          call osubhd(outfile,'Annular cooling channels')
       end if

       if (costr == 1) then
          call ocmmnt(outfile,'Gaseous helium chosen to be coolant')
       else
          call ocmmnt(outfile,'Pressurized water chosen to be coolant')
       end if

       if (smstr == 1) then
          call ocmmnt(outfile,'Solid blanket chosen (Li2O/Be)')
       else
          call ocmmnt(outfile,'Liquid blanket chosen (LiPb/Li)')
       end if

    end if

    !  Blanket parameter calculation 
    !  -----------------------------
    !  Assuming one dimensional heat conduction in a slab of thickness
    !  XO with volumetric heat source QFUSION, an approximate expression
    !  relating the maximum block temperature to the surface temperature
    !  can be derived.

    if (bstr == 1) then  !  Fixed outlet coolant temperature

       tb = tfo ; mtb = tfo
       do
          call props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans, &
               kf,kfs,ksolid)

          !  Calculate the bulk coolant mass flow rate. This is found
          !  simply from steady state (E = mcdT) considerations.

          mf = qfusion / (cf*(tfo-tfi))

          call flow(mf,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff, &
               dpf,qpump)

          !  Maximum blanket temperature

          tb = tfi + qfusion*( 1.0D0 + (1.0D0+c2*mf**a)/ &
               (c1*mf**(a-1.0D0)) ) / (mf*cf)

          !  The calculation must be repeated until a converged solution
          !  has been obtained because the expression for the maximum blanket
          !  temperature involves material properties which are themselves
          !  functions of the blanket temperature.

          if (abs(tb-mtb) < tol) exit

          mtb = tb
       end do

    else
       !  Calculate the material properties of the coolant
       !  at a fixed coolant outlet temperature of 850 K.

       tfo = 850.0D0
       call props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans, &
            kf,kfs,ksolid)

       !  Coolant mass flow rate at TFO = 850 K

       mf = qfusion / (cf*(tfo-tfi))

       call flow(mf,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff, &
            dpf,qpump)

       do
          !  Mass Flow Rate difference at coolant mass flow rate MF

          f0 = mf/( 1.0D0 + (1.0D0+c2*mf**a) / (c1*mf**(a-1.0D0)) ) &
               - qfusion / (cf*(tb-tfi))
          mf1 = mf + 10.0D0
          call flow(mf1,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff, &
               dpf,qpump)

          !  Mass Flow Rate difference at coolant mass flow rate MF1

          ff0 = mf1/( 1.0D0 + (1.0D0+c2*mf1**a) / (c1*mf1**(a-1.0D0)) ) &
               - qfusion / (cf*(tb-tfi))
          mf2 = mf - 10.0D0
          call flow(mf2,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff, &
               dpf,qpump)

          !  Mass Flow Rate difference at coolant mass flow rate MF2

          fb0 = mf2/( 1.0D0 + (1.0D0+c2*mf2**a) / (c1*mf2**(a-1.0D0)) ) &
               - qfusion / (cf*(tb-tfi))

          !  Employ Newton Raphson method to find the flow rate MF
          !  at which XMF-MF = 0.

          df0 = (ff0-fb0)/20.0D0
          xmf = mf - f0/df0

          !  The calculation must be repeated until a converged solution
          !  has been obtained because the expression for the coolant outlet
          !  temperature involves material properties which are themselves
          !  functions of the coolant temperature.

          if (abs(xmf-mf) < tol) exit

          mf = xmf
       end do

       mf = xmf

       !  Coolant outlet temperature at converged coolant mass flow rate.

       tfo = qfusion/(mf*cf) + tfi
       call props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans, &
            kf,kfs,ksolid)
       call flow(mf,tfo,tfi,tb,dh1,lc,ac,asurf,per,c1,c2,htc,ff, &
            dpf,qpump)

    end if

    !  Volumetric heating power

    xqav = xqfus(1)/vol
    qav = qfusion/vol

    !  Surface temperature

    ts = tb - 0.25D0*qav*xo*xo / ksolid

    !  Output of blanket variables

    if (iprint == 1) then

       call oblnkl(outfile)
       call ocentr(outfile,'Blanket Output Data',width)
       call oblnkl(outfile)
       call ovarrf(outfile,'Max Blanket temp (C)','(tb)',tb-273.15D0)
       call ovarrf(outfile,'Coolant outlet temp (C)','(tfo)',tfo-273.15D0)
       call ovarrf(outfile,'Surface temp (C)','(ts)',ts-273.15D0)
       call ovarrf(outfile,'Pump power (MW)','(qpump)',qpump/1.0D6)
       call ovarrf(outfile,'Qfusion  (MW)','(xqfusion)',xqfus(1))
       call ovarrf(outfile,'Coolant fraction (%)','(yc)',yc*100.0D0)
       call ovarrf(outfile,'Coolant mass flow rate (kg/s)','(mf)',mf)
       call ovarrf(outfile,'Heat transfer coeff (W/m**2/K)','(htc)',htc)
       call ovarrf(outfile,'Reynolds number','(reyd)',reyd)
       call ovarrf(outfile,'Dh (cm)','(dh1)',dh1*100.0D0)
       call ovarrf(outfile,'Inlet press (MPa)','(xpf)',xpf)
       call ovarrf(outfile,'Pressure drop (kPa)','(dpf)',dpf/1.0D3)
       call ovarrf(outfile,'Exit press (MPa)','(pf-dpf)',(pf-dpf)/1.0D6)
       call ovarin(outfile,'Total no. of cooling channels','(ncc)',ncc)
       call ovarin(outfile,'Radial cooling channels','(4*nr)',4*nr)
       call ovarin(outfile,'Poloidal cooling channels','(np)',np)
       call ovarin(outfile,'Toroidal cooling channels','(nt)',nt)
       call ovarrf(outfile,'Block side length (cm)','(w)',w*100.0D0)
       call ovarrf(outfile,'Total cross sect. area (m**2)','(atotal)' &
            ,atotal)
       call ovarrf(outfile,'Blanket volume (m**3)','(vol)',vol)
       call ovarrf(outfile,'Total surface area (m**2)','(asurface)' &
            ,asurface)
       call ovarrf(outfile,'Average heat (MW/m**3)','(xqav)',xqav)

       call oblnkl(outfile)
       call ocentr(outfile,'Steam Input Data',width)
       call oblnkl(outfile)

       call ovarrf(outfile,'High inlet pressure (MPa)','(ph)',ph)
       call ovarrf(outfile,'Reheat intermediate pressure (MPa)','(pr)', &
            pr)
       call ovarrf(outfile,'Low inlet pressure (MPa)','(pin)',pin)
       call ovarrf(outfile,'Condenser pressure (MPa)','(pc)',pc)
       call ovarrf(outfile,'HP turbine isentropic efficiency','(etahp)', &
            etahp)
       call ovarrf(outfile,'IP turbine isentropic efficiency','(etainp)', &
            etainp)
       call ovarrf(outfile,'LP turbine isentropic efficiency','(etalp)', &
            etalp)
       call ovarrf(outfile,'Feed pump isentropic efficiency','(etafp)', &
            etafp)
       call ovarrf(outfile,'Condensate pump isentropic efficiency', &
            '(etacp)',etacp)
       call ovarin(outfile,'Number of IP feedwater heaters','(nipfwh)', &
            nipfwh)
       call ovarin(outfile,'Number of LP feedwater heaters','(nlpfwh)', &
            nlpfwh)
       call ovarrf(outfile,'Steam generator effectiveness','(sgeff)', &
            sgeff)
       call oblnkl(outfile)

    end if

    !  General steam cycle parameter calculation:
    !
    !  Given - isentropic efficiency (eta)
    !        - inlet pressure and temperature (p1,t1)
    !        - exit pressure (p2)
    !  Determine - inlet enthalpy h1 = f(p1,t1) and entropy s1 = f(p1,t1)
    !              from subroutine SUPVAP
    !            - isentropic exit enthalpy h2s=f1(p1,s1) from subroutine
    !              SUPVAP1 or TWOPHASE depending on whether the exit
    !              conditions are in the single or two-phase region.
    !            - exit enthalpy h2 = h1-eta*(h1-h2s)
    !            - exit entropy and temperature from SUPVAP2 or 
    !              TWOPHASE.
    !            - for both the intermediate and low pressure
    !              turbines evaluate the extraction point enthalpies
    !              to the feedwater heaters.     

    !  Steam generator/High pressure phase
    !  -----------------------------------
    !
    !  Water passes through the steam generator at (tb1,hb1,ph)
    !  and exits at (tb2 = th1,hb2 = hh1,ph). The water/steam mixture
    !  then enters the HP turbine where is expands to (th2,hh2,pr).

    !  Calculate saturated liquid/vapour properties at ph and pr

    call satliq(ph,thsat,hhsat,vhsat,hgh,sgh)
    call satliq(pr,trsat,hrsat,vrsat,hgr,sgr)

    !  Saturation temperature at SG inlet (a guess)

    tsi = trsat

    !  If the steam generation is completely effective 
    !  the saturation temperature at the SG outlet will 
    !  equal the outlet coolant temperature.

    tso = sgeff*tfo+(1.0D0-sgeff)*tsi

    steam_cycle_loop: do

       xtso = tso

       !  Intermediate/low pressure turbines phase
       !  ----------------------------------------
       ! 
       !  The mixture is then reheated and enters an intermediate
       !  pressure turbine at (ti1,hi1,pi=pr) and exits at (ti2,hi2=hl2,pl)
       !  and then enters a low pressure turbine at (tl1,hl1,pl) and 
       !  exits at (tl2 = tc,hl2 = hc,pc).

       !  Calculate saturated liquid/vapour properties at pl = pin and pc

       call satliq(pin,tinsat,hinsat,vinsat,hgin,sgin)
       call satliq(pc,tc,hc,vc,hgc,sgc)

       !  Evaluate inlet enthalpies/entropies
       !  -----------------------------------
       ! 
       !  SUPVAP calculates the enthalpy and entropy
       ! 
       !  At the HP inlet (ph,tso)

       call supvap(ph,tso,hhi,shi,rhohi,vhi)
       hso = hhi

       !  At the IP inlet (after reheat) (pr,tso)

       call supvap(pr,tso,hri,sri,rhori,vri)

       !  Calculation of enthalpies for IP/LP feed water pump heaters
       !  -----------------------------------------------------------
       ! 
       !  Array assignments
       ! 
       !  Zeroth element: intermediate pressure turbine inlet value (fixed)
       !  first--->nipfwh element: intermediate pressure feed water pump 
       !                           value (determined)
       !  nipfwh+1 element: intermediate pressure turbine outlet value
       !                           (fixed)
       !  nipfwh+2--->nipfwh+nlpfwh+1 element: low pressure feed water pump 
       !                           value (determined)
       !  nipfwh+nlpfwh+2 element: low pressure inlet value (fixed)
       ! 
       !  These assigments are carried out for the saturated liquid 
       !  temperature, specific mass, super heated vapour enthalpy, 
       !  saturated liquid enthalpy and super heated vapour entropy. 

       !  Saturated liquid variables

       tsat(0) = trsat
       tsat(nipfwh+nlpfwh+2) = tc
       tsat(nipfwh+1) = tinsat

       vsat(0) = vrsat
       vsat(nipfwh+nlpfwh+2) = vc
       vsat(nipfwh+1) = vinsat

       hsat(0) = hrsat
       hsat(nipfwh+nlpfwh+2) = hc
       hsat(nipfwh+1) = hinsat

       !  Superheated vapour variables

       sg(0) = sgr
       sg(nipfwh+nlpfwh+2) = sgc
       sg(nipfwh+1) = sgin

       hg(0) = hgr
       hg(nipfwh+nlpfwh+2) = hgc
       hg(nipfwh+1) = hgin

       !  Average enthalpy change across the feed water pump heaters
       !  between the intermediate inlet and intermediate outlet

       dhip = (hrsat-hinsat)/dble(nipfwh+1)

       !  Average enthalpy change across the feed water pump heaters
       !  between the low pressure inlet and the condenser inlet

       dhlp = (hinsat-hc)/dble(nlpfwh+1)

       !  Intermediate pressure feed water pump heaters

       do i = 1,nipfwh
          !  hsat(0) = hrsat
          hsat(i) = hsat(0)-dble(i)*dhip
       end do

       !  Low pressure feed water pump heaters.

       do i = nipfwh+2,nipfwh+nlpfwh+1
          !  hsat(nipfwh+nlpfwh+2) = hc
          hsat(i) = hsat(nipfwh+nlpfwh+2)+ &
               dble(nipfwh+nlpfwh+2-i)*dhlp
       end do

       !  Calculation of pressures for IP/LP feed water pump heaters

       ha1 = 702.603552D0
       ha2 = 1.289508D0
       ha3 = -0.002973D0
       ha4 = 131.378047D0
       ha5 = 59.892944D0
       ha6 = -2.739215D0
       ha7 = 0.079064D0

       !  Assignment of pressures at three stages of the Rankine cycle

       p1 = pr
       p(0) = pr
       p(nipfwh+1) = pin
       p(nipfwh+nlpfwh+2) = pc

       !  From the enthalpies at each stage in the chain of
       !  intermediate pressure feed water pump heaters calculate
       !  the pressures

       do i = 1,nipfwh

          do
             !  Difference between the saturated liquid enthalpy at feed point
             !  'i' and the saturated liquid enthalpy at p1

             g = -hsat(i) + (ha1 + ha2/p1 + ha3/p1**2 + ha4*log(p1) + ha5*p1 &
                  + ha6*p1*p1 + ha7*p1*p1*p1)

             !  Derivative of this difference with pressure

             dg = -ha2/p1**2 - 2.0D0*ha3/p1**3 + ha4/p1 + ha5 &
                  + 2.0D0*ha6*p1 + 3.0D0*ha7*p1*p1

             !  Newton Raphson method employed to determine the pressure at
             !  the point where the difference in the enthalpies is within
             !  the tolerance

             mp = p1 - 0.005D0*g/dg

             if (abs(mp-p1) < tol) exit

             p1 = mp
          end do

          p(i) = mp

          !  Evaluate various saturated liquid/vapour quantities at
          !  this pressure

          call satliq(mp,tsat1,hsat1,vsat1,hg1,sg1)
          tsat(i) = tsat1
          vsat(i) = vsat1
          sg(i) = sg1
          hg(i) = hg1

       end do

       p1 = pin

       !  From the enthalpies at each stage in the chain of
       !  low pressure feed water pump heaters calculate
       !  the pressures

       do i = nipfwh+2,nipfwh+nlpfwh+1

          do
             !  Difference between the saturated liquid enthalpy at feed point
             !  'i' and the saturated liquid enthalpy at p1.

             g = -hsat(i) + (ha1 + ha2/p1 + ha3/p1**2 + ha4*log(p1) &
                  + ha5*p1 + ha6*p1*p1 + ha7*p1*p1*p1)

             !  Derivative of this difference with pressure

             dg = -ha2/p1**2 - 2.0D0*ha3/p1**3 + ha4/p1 + ha5 &
                  + 2.0D0*ha6*p1 + 3.0D0*ha7*p1*p1

             !  Newton Raphson method employed to determine the pressure at
             !  the point where the difference in the enthalpies is within
             !  the tolerance

             mp = p1 - 0.005D0*g/dg

             if (abs(mp-p1) < tol) exit

             p1 = mp
          end do

          p(i) = mp

          !  Evaluate various saturated liquid/vapour quantities at
          !  this pressure

          call satliq(mp,tsat1,hsat1,vsat1,hg1,sg1)
          tsat(i) = tsat1
          vsat(i) = vsat1
          sg(i) = sg1
          hg(i) = hg1

       end do

       if (sgeff /= 1.0D0) then

          tsi = tsat(1)
          tso = sgeff*tfo + (1.0D0-sgeff)*tsi

          !  Repeat above calculations for the feedwater pump saturated
          !  liquid variables to obtain a revised estimate of the
          !  saturated liquid entry temperature to the steam generator

          if (abs(xtso-tso) <= 0.1D0) exit steam_cycle_loop

       else
          exit steam_cycle_loop
       end if

    end do steam_cycle_loop

    !  Evaluation of isentropic exit enthalpies
    !  ----------------------------------------
    ! 
    !  We have to check to see if the exit conditions are within
    !  the superheated vapour region or the two-phase region.

    !  Each turbine effects the process (t1,s1,p1)---->(t2,s2,p2).
    !  This process is modelled in two parts: firstly an isentropic
    !  part (t1,s1,h1,p1)---->(t1s,s1,h1s,p1s); followed by a
    !  non-isentropic part (t1s,s1,h1s,p1s)---->(t2,s2,h2,p2).
    !  Subroutines SUPVAP1 or TWOPHASE simulates the first part
    !  while SUPVAP2 or TWOPHAS1 evaluate the entropy at the
    !  system point (p2,h2).

    !  High pressure turbine
    !  ---------------------

    !  The isentropic enthalpy at the high pressure turbine 
    !  outlet (pr,shi).

    if (shi > sgr) then
       !  Superheated steam
       call supvap1(pr,shi,hhes,thes) 
    else
       !  Two-phase state
       call twophase(pr,thes,shi,hhes)
    end if

    !  High pressure outlet enthalpy (etahp is the
    !  isentropic efficiency of the high pressure turbine)

    hhe = hhi - etahp*(hhi-hhes)

    !  Intermediate pressure turbine (post reheat)
    !  -------------------------------------------

    !  Evaluate the isentropic enthalpy at the intermediate
    !  pressure outlet (pin,sri)

    if (sri > sgin) then
       !  Superheated steam
       call supvap1(pin,sri,hres,tres)
    else 
       !  Two-phase state
       call twophase(pin,tres,sri,hres)
    end if

    !  Intermediate pressure outlet enthalpy (etainp is the
    !  isentropic efficiency of the intermediate pressure turbine)

    hre = hri - etainp*(hri-hres)

    !  Low pressure inlet enthalpy equals the intermediate pressure
    !  outlet enthalpy

    hli = hre

    !  Evaluate the isentropic entropy at intermediate pressure
    !  outlet (pin,hre)

    if (hre > hgin) then
       !  Superheated steam
       call supvap2(pin,hre,sre,tre)
    else 
       !  Steam/liquid mixture
       call twophas1(pin,tre,hre,sre)
    end if

    !  Low pressure inlet entropy equals the intermediate outlet entropy

    sli = sre

    !  Low pressure turbine
    !  --------------------

    !  Evaluate the isentropic enthalpy at the low pressure turbine
    !  outlet (pc,sli)

    if (sli < sgc) then
       !  Superheated steam
       call twophase(pc,tles,sli,hles)
    else 
       !  Steam/liquid mixture
       call supvap1(pc,sli,hles,tles)
    end if

    !  Low pressure outlet enthalpy (etalp is the isentropic 
    !  pressure efficiency for the low pressure turbine)

    hle = hli - etalp*(hli-hles)

    !  Evaluate the entropy at the low pressure turbine
    !  outlet (pc,hle)

    if (hle < hgc) then
       !  Vapour/liquid mixture
       call twophas1(pc,tle,hle,sle)
    else
       !  Superheated steam
       call supvap2(pc,hle,sle,tle)
    end if

    !  Calculate extraction enthalpies/entropies to the IP/LP FWHs
    !  -----------------------------------------------------------

    !  Given the enthalpies and entropies at the IP inlet;
    !  the IP outlet; and the LP outlet find the enthalpies
    !  and entropies at each extraction point to the
    !  feed water pump heaters.
    !    Once again what region the conditions are in determines
    !  whether SUPVAP3 or TWOPHAS2 are called.

    h(0) = hri
    h(nipfwh+nlpfwh+2) = hle
    h(nipfwh+1) = hre

    s(0) = sri
    s(nipfwh+nlpfwh+2) = sle
    s(nipfwh+1) = sre

    t(0) = tso
    t(nipfwh+nlpfwh+2) = tle
    t(nipfwh+1) = tre

    !  IP feed water pump heaters

    do i = 1,nipfwh

       px = p(i)
       ti = 0.5D0*(tso + tre)

       if (etainp == 1) then

          !  If the pump is 100 % efficient then the enthalpy does
          !  not change (equivalent to vertical trajectory on s-h diagram)

          if (sri > sg(i)) then
             !  Superheated vapour
             call supvap1(px,sri,hi,ti)
          else
             !  Two-phase region
             call twophase(px,ti,sri,hi)
          end if

       else
          !  Superheated vapour

          call supvap3(px,ti,hi,si)

          if (si > sg(i)) then
             h(i) = hi
             t(i) = ti
             s(i) = si
          else
             !  Two-phase region
             call twophas2(px,ti,hi,si)
             h(i) = hi
             t(i) = ti
             s(i) = si
          end if
       end if

    end do

    !  IP feed water pump heaters

    do i = nipfwh+2,nipfwh+nlpfwh+1

       px = p(i)
       ti = 0.5D0*(tre + tle)

       if (etalp == 1) then

          !  If the pump is 100 % efficient then the enthalpy does
          !  not change

          if (sri > sg(i)) then
             !  Superheated vapour
             call supvap1(px,sri,hi,ti) 
          else
             !  Two-phase region
             call twophase(px,ti,sri,hi)
          end if

       else
          !  Superheated vapour

          call supvap3(px,ti,hi,si)

          if (si > sg(i)) then
             h(i) = hi
             t(i) = ti
             s(i) = si
          else
             !  Two-phase region
             call twophas2(px,ti,hi,si)
             h(i) = hi
             t(i) = ti
             s(i) = si
          end if
       end if

    end do

    dh(1) = vsat(1) * (ph - p(1))*1000.0D0 / etafp
    hsi = hsat(1) + dh(1)

    !  Steam mass flow rate: determined by consideration of 
    !  steam generator heat balance. The steam mass flow
    !  rate is altered by the presence of heated coolant coming
    !  from the shield and first wall/divertor.

    ms = ( (mf*cf/1000.0D0)*(tfo-tfi) + xqfus(2)*1.0D3 &
         + xqfus(3)*1.0D3 ) / (hso-hsi+hri-hhe)

    !  Heat change (MW) in passing through the steam generator
    !  and during the reheat stage

    qsg = ms*(hso-hsi)/1000.0D0
    qr = ms*(hri-hhe)/1000.0D0

    !  Boundary conditions of the mass flow rate

    mb(0) = 0.0D0
    mb(nipfwh+nlpfwh+2) = 0.0D0
    m(0) = ms
    m(1) = m(0)-mb(0)

    do i = 1,nipfwh+nlpfwh+1

       if (i == (nipfwh+nlpfwh+1)) then
          eta = etacp 
       else
          eta = etafp
       end if

       !  Evaluate the enthalpy rise due to each feed pump and
       !  the corresponding mass flow rates

       dh(i+1) = vsat(i+1) * (p(i) - p(i+1)) * 1000.0D0 / eta
       mb(i) = m(i) * (hsat(i)-hsat(i+1)-dh(i+1)) &
            / (h(i)-hsat(i+1)-dh(i+1))
       m(i+1) = m(i) - mb(i)

    end do

    !  Power calculation
    !  -----------------

    do i = 1,nipfwh+nlpfwh+2
       wt(i) = m(i) * (h(i-1)-h(i)) / 1000.0D0
       wp(i) = m(i) * dh(i)/1000.0D0
    end do

    wcp = wp(nipfwh+nlpfwh+2)
    whpt = ms * (hhi-hhe)/1000.0D0

    !  Sum powers (MW)

    sum1 = 0.0D0 ; sum2 = 0.0D0 ; sum3 = 0.0D0 ; sum4 = 0.0D0

    do i = 1,nipfwh+1
       sum1 = sum1 + wt(i)
       sum3 = sum3 + wp(i)
    end do

    do i = nipfwh+2,nipfwh+nlpfwh+2
       sum2 = sum2 + wt(i)
       sum4 = sum4 + wp(i)
    end do

    winpt = sum1
    wlpt = sum2
    wst = whpt + sum1 + sum2
    winpfp = sum3
    wcp = wp(nipfwh+nlpfwh+2)
    wlpfp = sum4 - wcp
    wp1 = sum3 + sum4
    qc = m(nipfwh+nlpfwh+2) * (h(nipfwh+nlpfwh+2)-hc)/1000.0D0
    wnet = wst - wp1

    wnet1 = wst - wp1 - 1.0D-6*qpump
    qin = ms*(hso-hsi+hri-hhe)/1000.0D0

    !  Plant and cycle efficiencies

    etacycle = wnet/qin
    etaplant = wnet1/qin
    xexit = (hle-hc)/(hgc-hc)

    if (iprint == 1) then

       call oblnkl(outfile)
       call ocentr(outfile,'Thermal Cycle Data',width)
       call oblnkl(outfile)

       do i = 0,nipfwh+nlpfwh+2
          call ovarrf(outfile,'Pressure (MPa)','(p)',p(i))
          call ovarin(outfile,'Extraction point','(i)',i)
          call ovarrf(outfile,'Enthalpy (kJ/kg)','(h)',h(i))
          call ovarrf(outfile,'Entropy (kJ/kg/K)','(s)',s(i))
          call ovarrf(outfile,'Temperature (C)','(t)',t(i)-273.15D0)
          call ovarrf(outfile,'Bled mass flow rate (kg/s)','(mb)',mb(i))
          call ovarin(outfile,'Feedwater heater train point','(i)',i)
          call ovarrf(outfile,'Saturated enthalpy (kJ/kg)','(hsat)', &
               hsat(i))
          call ovarrf(outfile,'Saturated temperature (C)','(tsat)', &
               tsat(i))
          call ovarrf(outfile,'Saturated specific volume (m**3/kg)', &
               '(vsat)',vsat(i))
          call ovarrf(outfile,'Mass flow rate (kg/s)','(m)',m(i))
          call oblnkl(outfile)
       end do

       call ocentr(outfile,'Steam Output Data',width)
       call oblnkl(outfile)

       call ovarrf(outfile,'SG inlet enthalpy (kJ/kg)','(hsi)',hsi)
       call ovarrf(outfile,'SG exit enthalpy (kJ/kg)','(hso)',hso)
       call ovarrf(outfile,'SG inlet temp (C)','(tsi)',tsi-273.15D0)
       call ovarrf(outfile,'SG exit temp (C)','(tso)',tso-273.15D0)
       call ovarrf(outfile,'HP turbine inlet  h (kJ/kg)','(hhi)',hhi)
       call ovarrf(outfile,'HP turbine outlet  h (kJ/kg)','(hhe)',hhe)
       call ovarrf(outfile,'Reheat inlet (IP inlet) h (kJ/kg)','(hri)', &
            hri)
       call ovarrf(outfile,'IP outlet (LP turb inlet) h (kJ/kg)','(hre)', &
            hre)
       call ovarrf(outfile,'LP turb outlet h (kJ/kg)','(hle)',hle)
       call ovarrf(outfile,'Condenser enthalpy (kJ/kg)','(hc)',hc)
       call ovarrf(outfile,'Coolant mass flow rate (kg/s)','(mf)',mf)
       call ovarrf(outfile,'Steam mass flow rate (kg/s)','(ms)',ms)
       call ovarrf(outfile,'LP turbine exit dryness','(xexit)',xexit)
       call ovarrf(outfile,'HP turb. output (MW)','(whpt)',whpt)
       call ovarrf(outfile,'IP turb. output (MW)','(winpt)',winpt)
       call ovarrf(outfile,'LP turb. output (MW)','(wlpt)',wlpt)
       call ovarrf(outfile,'IP-FP power (MW)','(winpfp)',winpfp)
       call ovarrf(outfile,'LP-FP power (MW)','(wlpfp)',wlpfp)
       call ovarrf(outfile,'CP power (MW)','(wcp)',wcp)
       call ovarrf(outfile,'Qin (MW)','(qin)',qin)
       call ovarrf(outfile,'Qs (MW)','(qsg)',qsg)
       call ovarrf(outfile,'Qr (MW)','(qr)',qr)
       call ovarrf(outfile,'Qcond (MW)','(qc)',qc)
       call ovarrf(outfile,'Wnet plant (MW)','(wnet1)',wnet1)
       call ovarrf(outfile,'Wnet cycle (MW)','(wnet)',wnet)
       call ovarrf(outfile,'Plant Efficiency','(etaplant)',etaplant)
       call ovarrf(outfile,'Cycle Efficiency','(etacycle)',etacycle)

    end if

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine flow(mf,tfo,tfi,tb,dh,lc,ac,asurf,per,c1,c2,htc,ff, &
         dpf,qpump) 

      !+ad_name  flow
      !+ad_summ  Calculate fluid properties of the liquid coolant
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  mf    : input real : coolant mass flow rate (kg/s)
      !+ad_args  tfo   : input real : coolant outlet temperature (K) 
      !+ad_args  tfi   : input real : coolant inlet temperature (K) 
      !+ad_args  tb    : input real : maximum blanket temperature (K)
      !+ad_args  dh    : input real : hydraulic diameter (m)
      !+ad_args  lc    : input real : radial or half the poloidal extent of blanket (m)
      !+ad_args  ac    : input real : cooling channel cross sectional area (m**2)
      !+ad_args  asurf : input real : coolant channel surface area (m**2)
      !+ad_args  per   : input real : coolant channel circumference (m)
      !+ad_args  c1    : output real : constant
      !+ad_args  c2    : output real : constant
      !+ad_args  htc   : output real : heat transfer coefficient (W/m**2/K)
      !+ad_args  ff    : output real : coefficient of friction
      !+ad_args  dpf   : output real : pressure drop along coolant channel (Pa)
      !+ad_args  qpump : output real : pumping power (W)
      !+ad_desc  This routine calculates the fluid properties of the liquid
      !+ad_desc  coolant in the blanket.
      !+ad_prob  None
      !+ad_call  props
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: mf,tfo,tfi,tb,dh,lc,ac,asurf,per
      real(kind(1.0D0)), intent(out) :: c1,c2,htc,ff,dpf,qpump

      !  Local variables

      real(kind(1.0D0)) :: a0,b,b0,c,c0,cf,has,kf,kfs,ksolid,mf0, &
           mult,pran,prans,rhof,viscf,viscfs

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans, &
           kf,kfs,ksolid)

      !  Single coolant channel mass flow rate

      mf0 = mf/dble(ncc)

      !  Coolant flow Reynolds number

      reyd = mf0*dh/(ac*viscf)

      !  The 1.5 factor in the expression for the coolant pumping
      !  power is equivalent to assigning the pump efficiency to 2/3
      !  (efficiency = power required to pump coolant/pumping power)

      if (reyd > 2300.0D0) then  !  Turbulent flow
         a0 = 0.023D0
         a = 0.8D0
         c = 0.4D0
         b0 = 0.046D0
         b = -0.2D0
      else  !  Laminar flow
         a0 = 3.66D0
         a = 0.0D0
         c = 0.0D0
         b0 = 16.0D0
         b = -1.0D0
      end if

      c0 = a0*kf*pran**c * (4.0D0/viscf)**a
      htc = c0*(mf0/per)**a / dh
      has = htc*asurf*dble(ncc)
      c1 = c0*asurf*dble(ncc)**(1.0D0-a) / (dh*per**a*cf)
      c2 = g0*c0 / (ksolid*dh*(per*dble(ncc))**a)
      ff = b0*reyd**b
      dpf = 2.0D0*ff*(lc/dh)*(mf0/ac)**2 / rhof

      if (estr == 1) then
         mult = 2.0D0
      else
         mult = 1.0D0
      end if

      qpump = 1.50D0*mult*dble(ncc)*mf0*dpf / rhof

    end subroutine flow

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine props(tfo,tfi,tb,cf,rhof,viscf,viscfs,pran,prans, &
         kf,kfs,ksolid) 

      !+ad_name  props
      !+ad_summ  Calculate various temperature dependent coolant properties
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  tfo    : input real : coolant outlet temperature (K)
      !+ad_args  tfi    : input real : coolant inlet temperature (K)
      !+ad_args  tb     : input real : maximum blanket temperature (K)
      !+ad_args  cf     : output real : specific heat of coolant (J/kg/K)
      !+ad_args  rhof   : output real : coolant mass density (kg/m**3)
      !+ad_args  viscf  : output real : viscosity at T=x (kg/s/m)
      !+ad_args  viscfs : output real : viscosity at T=y (kg/s/m)
      !+ad_args  pran   : output real : viscf*cf/kf 
      !+ad_args  prans  : output real : viscfs*cfs/kfs
      !+ad_args  kf     : output real : thermal conductivity of coolant at T=x (W/m/K)
      !+ad_args  kfs    : output real : thermal conductivity of coolant at T=y (W/m/K)
      !+ad_args  ksolid : output real : thermal conductivity of blanket (W/m/K)
      !+ad_desc  This routine calculates various temperature dependent properties
      !+ad_desc  of the liquid coolant in the blanket.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: tfo,tfi,tb
      real(kind(1.0D0)), intent(out) :: cf,rhof,viscf,viscfs,pran,prans, &
           kf,kfs,ksolid

      !  Local variables

      real(kind(1.0D0)) :: x,y,gascf,cfs,ks1,ks2

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  x: average coolant temperature (K)
      !  y: average temperature difference between coolant and 
      !     blanket (K)
      !  gascf: specific heat capacity of gaseous coolant (J/kg/K)

      x = 0.5D0*(tfo + tfi)
      y = 0.5D0*( (tb-tfo) + (tb-tfi) )

      !  The blanket is a composite of two materials,
      !  the smallest thermal conductivity is used

      if (smstr == 1) then
         ks1 = 100.0D0 / (1.4D0 + 0.01828D0*y)
         ks2 = 233.54D0 - 0.28168D0*y + 1.8566D-04*y*y &
              - 4.4946D-08*y*y*y
      else
         if (y < 508.0D0) then
            ks1 = 17.7D0 + 0.0294D0*y 
         else 
            ks1 = 1.95D0 + 0.0196D0*y
         end if
         ks2 = 34.9246D0 + 0.019037D0*y
      end if

      ksolid = min(ks1,ks2)

      !  Calculate the specific heat of the blanket

      if (costr == 1) then  !  Helium coolant

         gascf = 8314.3D0/4.0026D0
         kf  = 0.033378D0 + 4.2674D-04*x - 1.0807D-07*x*x
         kfs = 0.033378D0 + 4.2674D-04*y - 1.0807D-07*y*y
         viscf  = 4.7744D-07*x**0.6567D0
         viscfs = 4.7744D-07*y**0.6567D0
         cf = 5193.0D0
         pran = viscf*cf/kf
         prans = viscfs*cf/kfs
         rhof = pf/(gascf*x)

      else  !  Pressurized water coolant

         kf  = 8.9372D0 - 0.048702D0*x + 9.6994D-05*x*x - 6.5541D-08*x*x*x
         kfs = 8.9372D0 - 0.048702D0*y + 9.6994D-05*y*y - 6.5541D-08*y*y*y
         cf  = -371240.0D0 + 2188.7D0*x - 4.2565D0*x*x + 0.0027658D0*x*x*x
         cfs = -371240.0D0 + 2188.7D0*y - 4.2565D0*y*y + 0.0027658D0*y*y*y
         pran  = -68.469D0 + 0.41786D0*x - 8.3547D-04*x*x + 5.5443D-07*x*x*x
         prans = -68.469D0 + 0.41786D0*y - 8.3547D-04*y*y + 5.5443D-07*y*y*y
         rhof = 15307.0D0 - 81.04D0*x + 0.15318D0*x*x - 9.8061D-05*x*x*x
         viscf = kf*pran/cf
         viscfs = kfs*prans/cfs

      end if

    end subroutine props

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine satliq(p,t,hf,vf,hg,sg) 

      !+ad_name  satliq
      !+ad_summ  Calculate properties of saturated liquid and vapour
      !+ad_summ  at a given liquid/vapour pressure
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  p  : input real : saturated liquid/steam pressure (MPa)
      !+ad_args  t  : output real : saturated temperature (K)
      !+ad_args  hf : output real : saturated liquid enthalpy (kJ/kg)
      !+ad_args  vf : output real : saturated liquid specific volume (m**3/kg)
      !+ad_args  hg : output real : saturated vapour enthalpy (kJ/kg)
      !+ad_args  sg : output real : saturated vapour entropy (kJ/kg)
      !+ad_desc  This routine calculates the properties of saturated liquid
      !+ad_desc  and vapour at a given liquid/vapour pressure.
      !+ad_prob  The hg, sg results only reliable for 0.004 < p < 20 MPa
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: p
      real(kind(1.0D0)), intent(out) :: t,hf,vf,hg,sg

      !  Local variables

      real(kind(1.0D0)) :: pp,ta1,ta2,ta3,ta4,ta5,ta6,ta7,ha1,ha2,ha3, &
           ha4,ha5,ha6,ha7,va1,va2,va3,va4,va5,va6,va7

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Saturated liquid properties

      ta1 = 168.396D0
      ta2 = 0.314653D0
      ta3 = -0.000728D0
      ta4 = 31.588979D0
      ta5 = 11.473141D0
      ta6 = -0.575335D0
      ta7 = 0.013165D0
      t = 273.15D0 + ta1 + ta2/p + ta3/p**2 + ta4*log(p) &
           + ta5*p + ta6*p*p + ta7*p*p*p

      ha1 = 702.603552D0
      ha2 = 1.289508D0
      ha3 = -0.002973D0
      ha4 = 131.378047D0
      ha5 = 59.892944D0
      ha6 = -2.739215D0
      ha7 = 0.079064D0
      hf = ha1 + ha2/p + ha3/p**2 + ha4*log(p) + ha5*p + ha6*p*p + ha7*p*p*p

      va1 = 1084.060198D0
      va2 = 0.217242D0
      va3 = -0.000394D0
      va4 = 19.993979D0
      va5 = 43.156082D0
      va6 = -2.293468D0
      va7 = 0.115701D0
      vf = 1.0D-6 * (va1 + va2/p + va3/p**2 + va4*log(p) + va5*p &
           + va6*p*p + va7*p*p*p)

      !  Saturated vapour properties

      !  PJK 27/09/12
      !  Outside range 0.004 <= p < 20, values take the extrema
      !  Therefore, results are unreliable outside of the p range given

      if (p < 0.004D0) then
         pp = 0.004D0
         hg = 2765.4675D0*pp**0.0147D0
         sg = 6.6542D0*pp**(-0.0441D0)
      else if (p < 0.1D0) then
         hg = 2765.4675D0*p**0.0147D0
         sg = 6.6542D0*p**(-0.0441D0)
      else if (p < 1.0D0) then
         hg = 2777.9404D0*p**0.0162D0
         sg = 6.5914D0*p**(-0.0484D0)
      else if (p < 20.0D0) then
         hg = 2741.1D0 + 48.918D0*p - 13.46D0*p**2 + 1.6412D0*p**3 &
              - 0.11537D0*p**4 + 0.0041868D0*p**5 - 6.226D-05*p**6
         sg = 6.9035D0 - 0.40221D0*p + 0.077209D0*p**2 - 0.009548D0*p**3 &
              + 6.5518D-04*p**4 - 2.3121D-05*p**5 + 3.2419D-06*p**6
      else
         pp = 20.0D0
         hg = 2741.1D0 + 48.918D0*pp - 13.46D0*pp**2 + 1.6412D0*pp**3 &
              - 0.11537D0*pp**4 + 0.0041868D0*pp**5 - 6.226D-05*pp**6
         sg = 6.9035D0 - 0.40221D0*pp + 0.077209D0*pp**2 - 0.009548D0*pp**3 &
              + 6.5518D-04*pp**4 - 2.3121D-05*pp**5 + 3.2419D-06*pp**6
      end if

    end subroutine satliq

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine supvap(p,t,h,s,rho,v) 

      !+ad_name  supvap
      !+ad_summ  Calculate superheated vapour enthalpy and entropy
      !+ad_summ  at a given pressure and temperature
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  p   : input real : superheated vapour pressure (MPa)
      !+ad_args  t   : input real : saturated vapour temperature (K)
      !+ad_args  h   : output real : superheated vapour enthalpy (kJ/kg)
      !+ad_args  s   : output real : superheated vapour entropy (kJ/kg)
      !+ad_args  rho : output real : superheated vapour mass density (kg/m**3)
      !+ad_args  v   : output real : superheated vapour specific volume (m**3/kg)
      !+ad_desc  This routine calculates the enthalpy and entropy
      !+ad_desc  of a superheated vapour at a given pressure and temperature.
      !+ad_desc  Analytical approximations to the following expressions are used:
      !+ad_desc  <PRE>
      !+ad_desc                      ,t              ,p
      !+ad_desc                      |               |
      !+ad_desc  h(p,t) = h(pr,tr) + |  cp(pr,t)dt + |  ( v - T [dv/dT] )dp
      !+ad_desc                      |               |                 p
      !+ad_desc                      'tr             'pr
      !+ad_desc 
      !+ad_desc                      ,t              ,p
      !+ad_desc                      |               |
      !+ad_desc  s(p,t) = s(pr,tr) + |  cp(pr,t)dt + |  ( [dv/dT] )dp
      !+ad_desc                      |          --   |           p
      !+ad_desc                      'tr        t    'pr
      !+ad_desc  </PRE>
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: p,t
      real(kind(1.0D0)), intent(out) :: h,s,rho,v

      !  Local variables

      real(kind(1.0D0)) :: a,a1,a2,a3,a4,aa,ab,ac,ac1,ac2,ac3,acap, &
           ad,ad2,ad3,ad4,ad5,as0,as1,as2,as3,b,bb,bcap,beta,ccap, &
           delta,gamma,gasc,hr,mw,pref,saa,sbb,sr,theta,thetar,tr,x,xp,xtr

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      hr = 2501.6D0  !  reference enthalpy (kJ/kg)
      sr = 9.157D0   !  reference entropy (kJ/kg)
      xtr = 0.1D0    !  reference temperature (deg C)
      tr = xtr + 273.15D0  !  reference temperature (K)
      pref = 611.2D0 !  reference pressure (Pa)
      xp = p*1.0D6   !  superheated vapour pressure (Pa)

      !  Constants

      mw = 18.015D0
      gasc = 8314.3D0/mw
      acap = 0.769185D0
      bcap = 0.00126D0

      if (p < 1.0D0) then
         ccap = 1220000.0D0
      else if (p <= 10.0D0) then
         ccap = 1300000.0D0
      else if (p <= 40.0D0) then
         ccap = 1325000.0D0
      else if (p <= 60.0D0) then
         ccap = 1330000.0D0
      else if (p < 100.0D0) then
         ccap = 1335000.0D0
      else if (p < 180.0D0) then
         ccap = 1365000.0D0
      else
         ccap = 1380000.0D0
      end if

      a = 0.005318D0
      b = 0.002482D0
      a1 = 143.05D0/mw
      a2 = -183.54D0/mw
      a3 = 82.751D0/mw
      a4 = -3.6989D0/mw
      beta = gasc*t*bcap - acap - gasc*ccap/(t*t)
      gamma = acap*a - gasc*b*bcap*t - gasc*bcap*ccap/(t*t)
      delta = gasc*bcap*b*ccap/(t*t)

      !  Expressions for the superheated vapour entropy and enthalpy
      !  as a function of pressure and temperature (Beattie-Bridgeman
      !  equations)

      theta = t/100.0D0
      thetar = tr/100.0D0

      !  Constant pressure term

      aa = a1*(theta-thetar) + a2*(theta**1.25D0 - thetar**1.25D0)/1.25D0 &
           + a3*(theta**1.5D0 - thetar**1.5D0)/1.5D0 &
           + a4*(theta**2 - thetar**2)/2.0D0
      x = gasc*t
      ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t**3
      ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t**6 &
           + 5.0D0*bcap*ccap/t**3
      ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t**3
      ac3 = -4.0D0*acap**2
      ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
      ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
           + 12.0D0*b*bcap*ccap)/t**3 + 27.0D0*bcap*ccap**2/t**6 &
           - 24.0D0*ccap**3/t**9
      ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
           + (63.0D0*acap*bcap*ccap + 21.0D0*acap*ccap*a)/t**3 &
           - 60.0D0*acap*ccap**2/t**6
      ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 &
           - 48.0D0*ccap*acap**2/t**3
      ad5 = -12.0D0*acap**3
      ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0

      !  Constant temperature term

      bb = (ab*(xp-pref) + ac*(xp*xp-pref*pref) &
           + ad*(xp*xp*xp-pref*pref*pref))/1.0D3

      !  Enthalpy

      h = hr + aa*100.0D0 + bb

      !  Specific volume and mass density

      v = gasc*t/xp + beta/(gasc*t) + (gamma-beta*beta/(gasc*t)) * &
           xp / (gasc**2*t*t) + (delta-3.0D0*beta*gamma/(gasc*t) + &
           2.0D0*beta**3/(gasc*t)**2)*xp*xp/(gasc**3*t*t*t)
      rho = 1.0D0/v

      !  Constant pressure term

      saa = a1*log(theta/thetar) + a4*(theta-thetar) + 4.0D0*a2 &
           *(theta**0.25D0-thetar**0.25D0) &
           + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t + 3.0D0*gasc*ccap/t**3)/x
      as2 = (0.5D0*bcap*b/t + 2.0D0*bcap*ccap/t**4)/x - (acap*a/t)/x**2
      as3 = -5.0D0*(bcap*b*ccap/t**4)/(3.0D0*x**2)

      !  Constant temperature term

      sbb = (as0 - as1*(xp-pref) - as2*(xp*xp-pref*pref) &
           - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

      !  Entropy

      s = sr + saa + sbb

    end subroutine supvap

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine supvap1(p,s,h,t) 

      !+ad_name  supvap1
      !+ad_summ  Calculate the enthalpy at (p,s)
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  p   : input real : superheated vapour pressure (MPa)
      !+ad_args  s   : input real : superheated vapour entropy (kJ/kg)
      !+ad_args  h   : output real : superheated vapour enthalpy (kJ/kg)
      !+ad_args  t   : output real : saturated vapour temperature (K)
      !+ad_desc  This routine calculates the enthalpy of a superheated vapour
      !+ad_desc  at a given pressure and entropy.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: p,s
      real(kind(1.0D0)), intent(out) :: h,t

      !  Local variables

      real(kind(1.0D0)) :: a,a1,a2,a3,a4,aa,ab,ac,ac1,ac2,ac3,acap, &
           ad,ad2,ad3,ad4,ad5,as0,as1,as2,as3,b,bb,bcap,beta,ccap, &
           delta,dgg,gamma,gasc,gg,ggb,ggf,hr,mt,mw,pref,saa,sbb,sr, &
           t1,t2,theta,thetar,tr,x,xp,xtr

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Reference values

      hr = 2501.6D0
      sr = 9.157D0
      xtr = 0.1D0
      tr = xtr + 273.15D0
      pref = 611.2D0
      xp = p*1.0D6

      !  Constants

      mw = 18.015D0
      gasc = 8314.3D0/mw
      acap = 0.769185D0
      bcap = 0.00126D0

      if (p < 1.0D0) then
         ccap = 1220000.0D0
      else if (p <= 10.0D0) then
         ccap = 1300000.0D0
      else if (p <= 40.0D0) then
         ccap = 1325000.0D0
      else if (p <= 60.0D0) then
         ccap = 1330000.0D0
      else if (p < 100.0D0) then
         ccap = 1335000.0D0
      else if (p < 180.0D0) then
         ccap = 1365000.0D0
      else
         ccap = 1380000.0D0
      end if

      a = 0.005318D0
      b = 0.002482D0
      a1 = 143.05D0/mw
      a2 = -183.54D0/mw
      a3 = 82.751D0/mw
      a4 = -3.6989D0/mw

      !  Fix temperature to the outlet coolant temperature minus 100 K

      t = tso - 100.0D0

      do
         !  Given the entropy and pressure find the temperature at
         !  which the Beattie-Bridgeman equations result in the
         !  same entropy

         x = gasc*t
         theta = t/100.0D0
         thetar = tr/100.0D0

         !  Entropy term at constant pressure

         saa = a1*log(theta/thetar) + a4*(theta-thetar) &
              + 4.0D0*a2*(theta**0.25D0-thetar**0.25D0) &
              + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
         as0 = gasc*log(pref/xp)
         as1 = (acap/t + 3.0D0*gasc*ccap/t**3) / x
         as2 = (0.5D0*bcap*b/t + 2.0D0*bcap*ccap/t**4) /x - (acap*a/t)/x**2
         as3 = -5.0D0*(bcap*b*ccap/t**4)/(3.0D0*x**2)

         !  Entropy term at constant temperature

         sbb = (as0 - as1*(xp-pref) - as2*(xp*xp-pref*pref) &
              - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

         !  Difference between calculated entropy and entropy
         !  carried as an argument to this subroutine

         gg = sr + saa + sbb - s

         ! Increment the temperature

         t1 = t + 1.0D0
         x = gasc*t1
         theta = t1/100.0D0
         thetar = tr/100.0D0

         !  Entropy term at constant pressure

         saa = a1*log(theta/thetar) + a4*(theta-thetar) &
              + 4.0D0*a2*(theta**0.25D0-thetar**0.25D0) &
              + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
         as0 = gasc*log(pref/xp)
         as1 = (acap/t1 + 3.0D0*gasc*ccap/t1**3) / x
         as2 = (0.5D0*bcap*b/t1 + 2.0D0*bcap*ccap/t1**4) /x - (acap*a/t1)/x**2
         as3 = -5.0D0*(bcap*b*ccap/t1**4)/(3.0D0*x**2)

         !  Entropy term at constant temperature

         sbb = (as0 - as1*(xp-pref) - as2*(xp*xp-pref*pref) &
              - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

         ggf = sr + saa + sbb - s

         !  Decrement the temperature

         t2 = t - 1.0D0
         x = gasc*t2
         theta = t2/100.0D0
         thetar = tr/100.0D0

         !  Entropy term at constant pressure

         saa = a1*log(theta/thetar) + a4*(theta-thetar) &
              + 4.0D0*a2*(theta**0.25D0-thetar**0.25D0) &
              + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
         as0 = gasc*log(pref/xp)
         as1 = (acap/t2 + 3.0D0*gasc*ccap/t2**3) / x
         as2 = (0.5D0*bcap*b/t2 + 2.0D0*bcap*ccap/t2**4) /x - (acap*a/t2)/x**2
         as3 = -5.0D0*(bcap*b*ccap/t2**4)/(3.0D0*x**2)

         !  Entropy term at constant temperature

         sbb = (as0 - as1*(xp-pref) - as2*(xp*xp-pref*pref) &
              - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

         ggb = sr + saa + sbb - s

         !  Derivative of the entropy change with temperature

         dgg = (ggf-ggb)/2.0D0

         !  Employ Newton Raphson to find the temperature where
         !  the difference between the calculated and read in
         !  entropies is zero

         mt = t - 0.05D0*gg/dgg

         !  Has the solution converged?

         if (abs(mt-t) < tol) exit

         t = mt

      end do

      t = mt

      !  Calculate the enthalpy at (p,t)

      beta = gasc*t*bcap - acap - gasc*ccap/(t*t)
      gamma = acap*a - gasc*b*bcap*t - gasc*bcap*ccap/(t*t)
      delta = gasc*bcap*b*ccap/(t*t)

      theta = t/100.0D0
      thetar = tr/100.0D0

      !  Constant pressure term

      aa = a1*(theta-thetar) + a2*(theta**1.25D0-thetar**1.25D0)/1.25D0 &
           + a3*(theta**1.5D0-thetar**1.5D0)/1.5D0 &
           + a4*(theta**2-thetar**2)/2.0D0
      x = gasc*t
      ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t**3
      ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t**6 &
           + 5.0D0*bcap*ccap/t**3
      ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t**3
      ac3 = -4.0D0*acap**2
      ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
      ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
           + 12.0D0*b*bcap*ccap)/t**3 + 27.0D0*bcap*ccap**2/t**6 &
           - 24.0D0*ccap**3/t**9
      ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
           + (63.0D0*acap*bcap*ccap + 21.0D0*acap*ccap*a)/t**3 &
           - 60.0D0*acap*ccap**2/t**6
      ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 &
           - 48.0D0*ccap*acap**2/t**3
      ad5 = -12.0D0*acap**3
      ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0

      !  Constant temperature term

      bb = ( ab*(xp-pref) + ac*(xp*xp-pref*pref) &
           + ad*(xp*xp*xp-pref*pref*pref) )/1000.0D0
      h = hr + aa*100.0D0 + bb

    end subroutine supvap1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine supvap2(p,h,s,t)

      !+ad_name  supvap2
      !+ad_summ  Calculate the entropy at (p,h)
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  p   : input real : superheated vapour pressure (MPa)
      !+ad_args  h   : input real : superheated vapour enthalpy (kJ/kg)
      !+ad_args  s   : output real : superheated vapour entropy (kJ/kg)
      !+ad_args  t   : output real : saturated vapour temperature (K)
      !+ad_desc  This routine calculates the entropy of a superheated vapour
      !+ad_desc  at a given pressure and enthalpy.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: p,h
      real(kind(1.0D0)), intent(out) :: s,t

      !  Local variables

      real(kind(1.0D0)) :: a,a1,a2,a3,a4,aa,ab,ac,ac1,ac2,ac3,acap, &
           ad,ad2,ad3,ad4,ad5,as0,as1,as2,as3,b,bb,bcap,ccap,dgh, &
           gasc,gh,ghb,ghf,hr,mt,mw,pref,saa,sbb,sr,t1,t2,theta, &
           thetar,tr,x,xp,xtr

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  tc: saturated temperature (K)

      !  Reference values

      hr = 2501.6D0
      sr = 9.157D0
      xtr = 0.1D0
      tr = xtr + 273.15D0
      pref = 611.2D0
      xp = p*1.0D6

      !  Constants

      mw = 18.015D0
      gasc = 8314.3D0/mw
      acap = 0.769185D0
      bcap = 0.00126D0

      if (p < 1.0D0) then
         ccap = 1220000.0D0
      else if (p <= 10.0D0) then
         ccap = 1300000.0D0
      else if (p <= 40.0D0) then
         ccap = 1325000.0D0
      else if (p <= 60.0D0) then
         ccap = 1330000.0D0
      else if (p < 100.0D0) then
         ccap = 1335000.0D0
      else if (p < 180.0D0) then
         ccap = 1365000.0D0
      else
         ccap = 1380000.0D0
      end if

      a = 0.005318D0
      b = 0.002482D0
      a1 = 143.05D0/mw
      a2 = -183.54D0/mw
      a3 = 82.751D0/mw
      a4 = -3.6989D0/mw

      t = 0.5D0*(tso + tc)

      !  Calculate the temperature at which the Beattie-Bridgeman
      !  equations give an enthalpy indentical to H (within the
      !  tolerance)

      do
         !  Calculate the enthalpy at (p,t)

         theta = t/100.0D0
         thetar = tr/100.0D0

         aa = a1*(theta-thetar) + a2*(theta**1.25D0-thetar**1.25D0)/1.25D0 &
              + a3*(theta**1.5D0-thetar**1.5D0)/1.5D0 &
              + a4*(theta**2-thetar**2)/2.0D0
         x = gasc*t
         ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t**3
         ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t**6 &
              + 5.0D0*bcap*ccap/t**3
         ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t**3
         ac3 = -4.0D0*acap**2
         ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
         ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
              + 12.0D0*b*bcap*ccap)/t**3 + 27.0D0*bcap*ccap**2/t**6 &
              - 24.0D0*ccap**3/t**9
         ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
              + (63.0D0*acap*bcap*ccap+21.0D0*acap*ccap*a)/t**3 &
              - 60.0D0*acap*ccap**2/t**6
         ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 &
              - 48.0D0*ccap*acap**2/t**3
         ad5 = -12.0D0*acap**3
         ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0
         bb = (ab*(xp-pref) + ac*(xp*xp-pref*pref) &
              + ad*(xp*xp*xp-pref*pref*pref))/1.0D3

         gh = h - (hr + aa*100.0D0 + bb)

         !  Calculate the enthalpy at (p,t+1)

         t1 = t + 1.0D0
         theta = t1/100.0D0
         thetar = tr/100.0D0

         aa = a1*(theta-thetar) + a2*(theta**1.25D0-thetar**1.25D0)/1.25D0 &
              + a3*(theta**1.5D0-thetar**1.5D0)/1.5D0 &
              + a4*(theta**2-thetar**2)/2.0D0
         x = gasc*t1
         ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t1**3
         ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t1**6 &
              + 5.0D0*bcap*ccap/t1**3
         ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t1**3
         ac3 = -4.0D0*acap**2
         ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
         ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
              + 12.0D0*b*bcap*ccap)/t1**3 + 27.0D0*bcap*ccap**2/t1**6 &
              - 24.0D0*ccap**3/t1**9
         ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
              + (63.0D0*acap*bcap*ccap+21.0D0*acap*ccap*a)/t1**3 &
              - 60.0D0*acap*ccap**2/t1**6
         ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 &
              - 48.0D0*ccap*acap**2/t1**3
         ad5 = -12.0D0*acap**3
         ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0
         bb = (ab*(xp-pref) + ac*(xp*xp-pref*pref) &
              + ad*(xp*xp*xp-pref*pref*pref))/1.0D3

         ghf = h - (hr + aa*100.0D0 + bb)

         !  Calculate the enthalpy at (p,t-1)

         t2 = t-1.0D0
         theta = t2/100.0D0
         thetar = tr/100.0D0

         aa = a1*(theta-thetar) + a2*(theta**1.25D0-thetar**1.25D0)/1.25D0 &
              + a3*(theta**1.5D0-thetar**1.5D0)/1.5D0 &
              + a4*(theta**2-thetar**2)/2.0D0
         x = gasc*t2
         ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t2**3
         ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t2**6 &
              + 5.0D0*bcap*ccap/t2**3
         ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t2**3
         ac3 = -4.0D0*acap**2
         ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
         ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
              + 12.0D0*b*bcap*ccap)/t2**3 + 27.0D0*bcap*ccap**2/t2**6 &
              - 24.0D0*ccap**3/t2**9
         ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
              + (63.0D0*acap*bcap*ccap+21.0D0*acap*ccap*a)/t2**3 &
              - 60.0D0*acap*ccap**2/t2**6
         ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 &
              - 48.0D0*ccap*acap**2/t2**3
         ad5 = -12.0D0*acap**3
         ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0
         bb = (ab*(xp-pref) + ac*(xp*xp-pref*pref) &
              + ad*(xp*xp*xp-pref*pref*pref))/1.0D3

         ghb = h - (hr + aa*100.0D0 + bb)

         !  Derivative of the enthalpy difference with temperature

         dgh = (ghf-ghb)/2.0D0

         !  Employ Newton Raphson to find the temperature at which
         !  this difference between the calculated and read in
         !  entropies is zero

         mt = t - 0.05D0*gh/dgh

         if (abs(mt-t) < tol) exit

         t = mt
      end do

      !  Calculate the entropy at (p,t)

      t = mt
      x = gasc*t
      theta = t/100.0D0
      thetar = tr/100.0D0

      !  Constant pressure term

      saa = a1*log(theta/thetar) + a4*(theta-thetar) &
           + 4.0D0*a2*(theta**0.25D0-thetar**0.25D0) &
           + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t + 3.0D0*gasc*ccap/t**3)/x
      as2 = (0.5D0*bcap*b/t + 2.0D0*bcap*ccap/t**4)/x - (acap*a/t)/x**2
      as3 = -5.0D0*(bcap*b*ccap/t**4)/(3.0D0*x**2)

      !  Constant temperature term

      sbb = (as0 - as1*(xp-pref) - as2*(xp*xp-pref*pref) &
           - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

      s = sr + saa + sbb

    end subroutine supvap2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine supvap3(p,t,h,s) 

      !+ad_name  supvap3
      !+ad_summ  Calculate the entropy and enthalpy at extraction points
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  p   : input real : superheated vapour pressure (MPa)
      !+ad_args  t   : input/output real : saturated vapour temperature (K)
      !+ad_args  h   : output real : superheated vapour enthalpy (kJ/kg)
      !+ad_args  s   : output real : superheated vapour entropy (kJ/kg)
      !+ad_desc  This routine calculates the temperature, entropy and enthalpy
      !+ad_desc  of a superheated vapour at a given pressure.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: p
      real(kind(1.0D0)), intent(inout) :: t
      real(kind(1.0D0)), intent(out) :: h,s

      !  Local variables

      real(kind(1.0D0)) :: a,a1,a2,a3,a4,aa,ab,ac,ac1,ac2,ac3, &
           acap,ad,ad2,ad3,ad4,ad5,as0,as1,as2,as3,b,bb,bcap,ccap, &
           dgt,gasc,gt,gtb,gtf,h1,h2,hr,mt,mw,pref,s1,s2,saa,sbb, &
           sr,t1,t2,theta,thetar,tr,x,xp,xtr

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Reference variables

      hr = 2501.6D0
      sr = 9.157D0
      xtr = 0.1D0
      tr = xtr + 273.15D0
      pref = 611.2D0
      xp = p*1.0D6

      !  Constants

      mw = 18.015D0
      gasc = 8314.3D0/mw
      acap = 0.769185D0
      bcap = 0.00126D0

      if (p < 1.0D0) then
         ccap = 1220000.0D0
      else if (p <= 10.0D0) then
         ccap = 1300000.0D0
      else if (p <= 40.0D0) then
         ccap = 1325000.0D0
      else if (p <= 60.0D0) then
         ccap = 1330000.0D0
      else if (p < 100.0D0) then
         ccap = 1335000.0D0
      else if (p < 180.0D0) then
         ccap = 1365000.0D0
      else
         ccap = 1380000.0D0
      end if

      a = 0.005318D0
      b = 0.002482D0
      a1 = 143.05D0/mw
      a2 = -183.54D0/mw
      a3 = 82.751D0/mw
      a4 = -3.6989D0/mw

      !  Calculate the temperature by employing the identity:
      ! 
      !  (h-hri)    (hri-hre)
      !  -------  = ---------
      !  (s-sri)    (sre-sri)
      ! 
      !  sre: outlet entropy (kJ/kg) 
      !  sri: inlet entropy (kJ/kg)
      !  hre: outlet enthalpy (kJ/kg)
      !  hri: inlet enthalpy (kJ/kg)
      ! 
      !  This is done by using a Newton Raphson scheme to
      !  calculate (s,h) which satisfies the above equation

      do
         !  Enthalpy at (p,t)

         theta = t/100.0D0
         thetar = tr/100.0D0

         aa = a1*(theta-thetar) + a2*(theta**1.25D0-thetar**1.25D0)/1.25D0 &
              + a3*(theta**1.5D0-thetar**1.5D0)/1.5D0 &
              + a4*(theta**2-thetar**2)/2.0D0
         x = gasc*t
         ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t**3
         ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t**6 &
              + 5.0D0*bcap*ccap/t**3
         ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t**3
         ac3 = -4.0D0*acap**2
         ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
         ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
              + 12.0D0*b*bcap*ccap)/t**3 + 27.0D0*bcap*ccap**2/t**6 &
              - 24.0D0*ccap**3/t**9
         ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
              + (63.0D0*acap*bcap*ccap + 21.0D0*acap*ccap*a)/t**3 &
              - 60.0D0*acap*ccap**2/t**6
         ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 - 48.0D0*ccap*acap**2/t**3
         ad5 = -12.0D0*acap**3
         ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0
         bb = (ab*(xp-pref) + ac*(xp*xp-pref*pref) &
              + ad*(xp*xp*xp-pref*pref*pref))/1.0D3

         h = hr + aa*100.0D0 + bb

         !  Entropy at (p,t)

         saa = a1*log(theta/thetar) + a4*(theta-thetar) &
              + 4.0D0*a2*(theta**0.25D0-thetar**0.25D0) &
              + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
         as0 = gasc*log(pref/xp)
         as1 = (acap/t + 3.0D0*gasc*ccap/t**3)/x
         as2 = (0.5D0*bcap*b/t + 2.0D0*bcap*ccap/t**4)/x - (acap*a/t)/x**2
         as3 = -5.0D0*(bcap*b*ccap/t**4)/(3.0D0*x**2)
         sbb = (as0-as1*(xp-pref) - as2*(xp*xp-pref*pref) &
              - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

         s = sr + saa + sbb
         gt = hri - h - (s-sri)*(hri-hre)/(sre-sri)

         !  Enthalpy at (p,t+1)

         t1 = t + 1.0D0
         theta = t1/100.0D0
         thetar = tr/100.0D0

         aa = a1*(theta-thetar) + a2*(theta**1.25D0-thetar**1.25D0)/1.25D0 &
              + a3*(theta**1.5D0-thetar**1.5D0)/1.5D0 &
              + a4*(theta**2-thetar**2)/2.0D0
         x = gasc*t1
         ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t1**3
         ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t1**6 &
              + 5.0D0*bcap*ccap/t1**3
         ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t1**3
         ac3 = -4.0D0*acap**2
         ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
         ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
              + 12.0D0*b*bcap*ccap)/t1**3 + 27.0D0*bcap*ccap**2/t1**6 &
              - 24.0D0*ccap**3/t1**9
         ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
              + (63.0D0*acap*bcap*ccap + 21.0D0*acap*ccap*a)/t1**3 &
              - 60.0D0*acap*ccap**2/t1**6
         ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 - 48.0D0*ccap*acap**2/t1**3
         ad5 = -12.0D0*acap**3
         ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0
         bb = (ab*(xp-pref) + ac*(xp*xp-pref*pref) &
              + ad*(xp*xp*xp-pref*pref*pref))/1.0D3

         h1 = hr + aa*100.0D0 + bb

         !  Entropy at (p,t+1)

         saa = a1*log(theta/thetar) + a4*(theta-thetar) &
              + 4.0D0*a2*(theta**0.25D0-thetar**0.25D0) &
              + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
         as0 = gasc*log(pref/xp)
         as1 = (acap/t1 + 3.0D0*gasc*ccap/t1**3)/x
         as2 = (0.5D0*bcap*b/t1 + 2.0D0*bcap*ccap/t1**4)/x - (acap*a/t1)/x**2
         as3 = -5.0D0*(bcap*b*ccap/t1**4)/(3.0D0*x**2)
         sbb = (as0-as1*(xp-pref) - as2*(xp*xp-pref*pref) &
              - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

         s1 = sr + saa + sbb
         gtf = hri - h1 - (s1-sri)*(hri-hre)/(sre-sri)

         !  Enthalpy at (p,t-1)

         t2 = t - 1.0D0
         theta = t2/100.0D0
         thetar = tr/100.0D0

         aa = a1*(theta-thetar) + a2*(theta**1.25D0-thetar**1.25D0)/1.25D0 &
              + a3*(theta**1.5D0-thetar**1.5D0)/1.5D0 &
              + a4*(theta**2-thetar**2)/2.0D0
         x = gasc*t2
         ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t2**3
         ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t2**6 &
              + 5.0D0*bcap*ccap/t2**3
         ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t2**3
         ac3 = -4.0D0*acap**2
         ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
         ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
              + 12.0D0*b*bcap*ccap)/t2**3 + 27.0D0*bcap*ccap**2/t2**6 &
              - 24.0D0*ccap**3/t2**9
         ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
              + (63.0D0*acap*bcap*ccap + 21.0D0*acap*ccap*a)/t2**3 &
              - 60.0D0*acap*ccap**2/t2**6
         ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 - 48.0D0*ccap*acap**2/t2**3
         ad5 = -12.0D0*acap**3
         ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0
         bb = (ab*(xp-pref) + ac*(xp*xp-pref*pref) &
              + ad*(xp*xp*xp-pref*pref*pref))/1.0D3

         h2 = hr + aa*100.0D0 + bb

         !  Entropy at (p,t-1)

         saa = a1*log(theta/thetar) + a4*(theta-thetar) &
              + 4.0D0*a2*(theta**0.25D0-thetar**0.25D0) &
              + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
         as0 = gasc*log(pref/xp)
         as1 = (acap/t2 + 3.0D0*gasc*ccap/t2**3)/x
         as2 = (0.5D0*bcap*b/t2 + 2.0D0*bcap*ccap/t2**4)/x - (acap*a/t2)/x**2
         as3 = -5.0D0*(bcap*b*ccap/t2**4)/(3.0D0*x**2)
         sbb = (as0-as1*(xp-pref) - as2*(xp*xp-pref*pref) &
              - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

         s2 = sr + saa + sbb
         gtb = hri - h2 - (s2-sri)*(hri-hre)/(sre-sri)

         dgt = (gtf-gtb)/2.0D0
         mt = t - 0.05D0*gt/dgt

         if (abs(mt-t) < tol) exit

         t = mt
      end do

      !  Calculate the entropy and enthalpy at (p,t) from 
      !  the Beattie-Bridgeman equations

      t = mt
      x = gasc*t
      theta = t/100.0D0
      thetar = tr/100.0D0

      saa = a1*log(theta/thetar) + a4*(theta-thetar) &
           + 4.0D0*a2*(theta**0.25D0-thetar**0.25D0) &
           + 2.0D0*a3*(theta**0.5D0-thetar**0.5D0)
      as0 = gasc*log(pref/xp)
      as1 = (acap/t + 3.0D0*gasc*ccap/t**3)/x
      as2 = (0.5D0*bcap*b/t + 2.0D0*bcap*ccap/t**4)/x - (acap*a/t)/x**2
      as3 = -5.0D0*(bcap*b*ccap/t**4)/(3.0D0*x**2)
      sbb = (as0-as1*(xp-pref) - as2*(xp*xp-pref*pref) &
           - as3*(xp*xp*xp-pref*pref*pref))/1.0D3

      s = sr + saa + sbb

      aa = a1*(theta-thetar) + a2*(theta**1.25D0-thetar**1.25D0)/1.25D0 &
           + a3*(theta**1.5D0-thetar**1.5D0)/1.5D0 &
           + a4*(theta**2-thetar**2)/2.0D0
      ab = bcap - 2.0D0*acap/x - 4.0D0*ccap/t**3
      ac1 = -2.0D0*b*bcap - 2.0D0*bcap**2 - 8.0D0*ccap**2/t**6 &
           + 5.0D0*bcap*ccap/t**3
      ac2 = 3.0D0*a*acap + 6.0D0*acap*bcap - 12.0D0*acap*ccap/t**3
      ac3 = -4.0D0*acap**2
      ac = 0.5D0*(ac1/x + ac2/x**2 + ac3/x**3)
      ad2 = 9.0D0*b*bcap**2 + 6.0D0*bcap**3 - (18.0D0*ccap*bcap**2 &
           + 12.0D0*b*bcap*ccap)/t**3 + 27.0D0*bcap*ccap**2/t**6 &
           - 24.0D0*ccap**3/t**9
      ad3 = -24.0D0*acap*bcap**2 - 12.0D0*acap*bcap*(a+b) &
           + (63.0D0*acap*bcap*ccap + 21.0D0*acap*ccap*a)/t**3 &
           - 60.0D0*acap*ccap**2/t**6
      ad4 = 30.0D0*bcap*acap**2 + 15.0D0*a*acap**2 - 48.0D0*ccap*acap**2/t**3
      ad5 = -12.0D0*acap**3
      ad = (ad2/x**2 + ad3/x**3 + ad4/x**4 + ad5/x**5)/3.0D0
      bb = (ab*(xp-pref) + ac*(xp*xp-pref*pref) &
           + ad*(xp*xp*xp-pref*pref*pref))/1.0D3

      h = hr + aa*100.0D0 + bb

    end subroutine supvap3

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine twophase(p,t,s,h) 

      !+ad_name  twophase
      !+ad_summ  Calculate the two-phase temperature and enthalpy
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  p   : input real : saturated vapour pressure (MPa)
      !+ad_args  t   : output real : saturated vapour temperature (K)
      !+ad_args  s   : input real : saturated vapour entropy (kJ/kg)
      !+ad_args  h   : output real : saturated vapour enthalpy (kJ/kg)
      !+ad_desc  This routine calculates the temperature and enthalpy
      !+ad_desc  of a two-phase (liquid/gas) state.
      !+ad_prob  The enthalpy result is only reliable for 0.004 < p < 2 MPa
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: p,s
      real(kind(1.0D0)), intent(out) :: t,h

      !  Local variables

      real(kind(1.0D0)) :: a,b,c,d,pp,ta1,ta2,ta3,ta4,ta5,ta6,ta7

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      pp = p

      !  Enthalpy from the entropy

      if (p < 0.02D0) then
         pp = max(p, 0.004D0)
         a = 2443.2013D0
         b = 0.0544D0
         c = 431.6173D0
         d = 0.0659D0
      else if (p < 0.2D0) then
         a = 2490.1152D0
         b = 0.0592D0
         c = 437.2028D0
         d = 0.0711D0
      else
         pp = min(p, 2.0D0)
         a = 2515.6971D0
         b = 0.066D0
         c = 452.6615D0
         d = 0.092D0
      end if

      h = a*pp**b + c*pp**d * (s-6.0D0)

      !  Saturated liquid temperature from the pressure

      ta1 = 168.396D0
      ta2 = 0.314653D0
      ta3 = -0.000728D0
      ta4 = 31.588979D0
      ta5 = 11.473141D0
      ta6 = -0.575335D0
      ta7 = 0.013165D0
      t = 273.15D0 + ta1 + ta2/p + ta3/p**2 + ta4*log(p) &
           + ta5*p + ta6*p*p + ta7*p*p*p

    end subroutine twophase

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine twophas1(p,t,h,s) 

      !+ad_name  twophas1
      !+ad_summ  Calculate the two-phase temperature and entropy
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  p   : input real : saturated vapour pressure (MPa)
      !+ad_args  t   : output real : saturated vapour temperature (K)
      !+ad_args  h   : input real : saturated vapour enthalpy (kJ/kg)
      !+ad_args  s   : output real : saturated vapour entropy (kJ/kg)
      !+ad_desc  This routine calculates the temperature and entropy
      !+ad_desc  of a two-phase (liquid/gas) state.
      !+ad_prob  The entropy result is only reliable for 0.004 < p < 2 MPa
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: p,h
      real(kind(1.0D0)), intent(out) :: t,s

      !  Local variables

      real(kind(1.0D0)) :: a,b,c,d,pp,ta1,ta2,ta3,ta4,ta5,ta6,ta7

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      pp = p

      !  Calculate the entropy from the enthalpy

      if (p < 0.02D0) then
         pp = max(p, 0.004D0)
         a = 2443.2013D0
         b = 0.0544D0
         c = 431.6173D0
         d = 0.0659D0
      else if (p < 0.2D0) then
         a = 2490.1152D0
         b = 0.0592D0
         c = 437.2028D0
         d = 0.0711D0
      else
         pp = min(p, 2.0D0)
         a = 2515.6971D0
         b = 0.066D0
         c = 452.6615D0
         d = 0.092D0
      end if

      s = 6.0D0 + (h - a*pp**b)/(c*pp**d)

      !  Saturated liquid temperature from the pressure

      ta1 = 168.396D0
      ta2 = 0.314653D0
      ta3 = -0.000728D0
      ta4 = 31.588979D0
      ta5 = 11.473141D0
      ta6 = -0.575335D0
      ta7 = 0.013165D0
      t = 273.15D0 + ta1 + ta2/p + ta3/p**2 + ta4*log(p) &
           + ta5*p + ta6*p*p + ta7*p*p*p

    end subroutine twophas1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine twophas2(p,t,h,s) 

      !+ad_name  twophas2
      !+ad_summ  Calculate the entropy, enthalpy and the temperature given
      !+ad_summ  the pressure
      !+ad_type  Subroutine
      !+ad_auth  P Karditsas, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  p   : input real : saturated vapour pressure (MPa)
      !+ad_args  t   : output real : saturated vapour temperature (K)
      !+ad_args  h   : output real : saturated vapour enthalpy (kJ/kg)
      !+ad_args  s   : output real : saturated vapour entropy (kJ/kg)
      !+ad_desc  This routine calculates the temperature, enthalpy
      !+ad_desc  and entropy of a two-phase (liquid/gas) state.
      !+ad_prob  The result is only reliable for 0.004 < p < 2 MPa
      !+ad_call  None
      !+ad_hist  --/--/-- PK  Initial version
      !+ad_hist  27/09/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Blanket and Energy Conversion Model for Fusion Reactors,
      !+ad_docc  Dr. P.J. Karditsas, AEA Technology, Theoretical and Strategic Studies
      !+ad_docc  Dept., Culham Laboratory, Abingdon
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: p
      real(kind(1.0D0)), intent(out) :: t,h,s

      !  Local variables

      real(kind(1.0D0)) :: a,b,c,d,a1,a2,m,pp,ta1,ta2,ta3,ta4,ta5,ta6,ta7

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Calculate both the entropy and enthalpy by using the
      !  expression:
      ! 
      !  (h-hli)   (hli-hle)
      !  ------- = ---------
      !  (s-sli)   (sli-sle)

      pp = p

      if (p < 0.02D0) then
         pp = max(p, 0.004D0)
         a = 2443.2013D0
         b = 0.0544D0
         c = 431.6173D0
         d = 0.0659D0
      else if (p < 0.2D0) then
         a = 2490.1152D0
         b = 0.0592D0
         c = 437.2028D0
         d = 0.0711D0
      else
         pp = min(p, 2.0D0)
         a = 2515.6971D0
         b = 0.066D0
         c = 452.6615D0
         d = 0.092D0
      end if

      !  h = a1+a2*(s-6)
      !  m (s-sli)  =  (h-hli)

      a1 = a*pp**b
      a2 = c*pp**d
      m = (hle-hli)/(sle-sli)
      s = (hli + 6.0D0*a2 - a1 - m*sli) / (a2-m)
      h = a*pp**b + c*pp**d * (s-6.0D0)

      !  Calculate the saturation temperature given the pressure

      ta1 = 168.396D0
      ta2 = 0.314653D0
      ta3 = -0.000728D0
      ta4 = 31.588979D0
      ta5 = 11.473141D0
      ta6 = -0.575335D0
      ta7 = 0.013165D0
      t = 273.15D0 + ta1 + ta2/p + ta3/p**2 + ta4*log(p) &
           + ta5*p + ta6*p*p + ta7*p*p*p

    end subroutine twophas2

  end subroutine blnkt

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

end module fwbs_module
