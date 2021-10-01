module kit_blanket_model

    !! Module containing the KIT blanket model
    !! based on the HCPB concept
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Franza, KIT (original MATLAB implementation)
    !! N/A
    !! This module contains the blanket neutronics model developed
    !! by Fabrizio Franza et al. from Karlsruhe Institute of Technology (KIT)
    !! based on the Helium-Cooled Pebble Bed (HCPB) blanket concept
    !! of the PPCS Model B design.
    !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
    !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
    !! Karlsruhe Institute of Technology, January 2013;
    !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef use_intrinsic
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
    implicit none
  
    private
    public :: kit_blanket, init_kit_blanket_model
  
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
  
    real(dp), dimension(np) :: x_BZ_IB, x_BM_IB, x_BP_IB, x_VV_IB
    real(dp), dimension(np) :: x_BZ_OB, x_BM_OB, x_BP_OB, x_VV_OB
  
    !  Values shared between subroutines in this module
  
    real(dp) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
    real(dp) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
    real(dp) :: phi_n_vv_IB_start,phi_n_vv_OB_start
  
    !  Universal constants
  
    real(dp), parameter :: E_n = 14.1D0    ! [MeV] Average neutron energy
    real(dp), parameter :: PA_T = 3.0D0    ! [g/mol] Tritium atomic weight
    real(dp), parameter :: N_Av = 6.02D23  ! [at/mol] Avogadro number
  
    !  Constants and fixed coefficients used in the model
    !  Based on Helium-Cooled Pebble Beds (HCPB) configuration
    !  of the PPCS Model B design
  
    real(dp) :: A_cov_PPCS      ! [m^2] Total blanket coverage area
    real(dp) :: A_FW_PPCS       ! [m^2] First wall area
    real(dp) :: NWL_av_PPCS     ! [MW/m^2] Average neutron wall load
    real(dp) :: NWL_av_IB_PPCS  ! [MW/m^2] Average IB wall load
    real(dp) :: NWL_av_OB_PPCS  ! [MW/m^2] Average OB wall load
    real(dp) :: NWL_max_IB_PPCS ! [MW/m^2] Maximum IB wall load
    real(dp) :: NWL_max_OB_PPCS ! [MW/m^2] Maximum OB wall load
    real(dp) :: CF_bl_PPCS      ! [%] Blanket coverage factor (calculated)
    real(dp) :: e_Li_PPCS       ! [%] Li6 enrichment
    real(dp) :: t_BZ_IB_PPCS    ! [cm] IB Breeding Zone thickness
    real(dp) :: t_BZ_OB_PPCS    ! [cm] OB Breeding Zone thickness
    real(dp) :: TBR_PPCS        ! [--] Tritium Breeding Ratio
    
    ! not used...
    ! real(dp) :: A_FW_IB_PPCS = 348.2D0  ! [m^2] IB first wall area
    ! real(dp) :: A_FW_OB_PPCS = 905.6D0  ! [m^2] OB first wall area
    ! character(len=13) :: breeder_PPCS = 'Orthosilicate' ! Breeder type
    ! real(dp) :: f_peak_PPCS = 1.21      ! [--] Neutron wall load peaking factor
    ! real(dp) :: M_E_PPCS = 1.38D0       ! [--] Energy multiplication factor
    !real(dp) :: t_HTS_IB_PPCS = 17.0D0  ! [cm] IB high temp. shield thickness
    !real(dp) :: alpha_HTS_IB_PPCS = 40.0D0 ! [%] IB HTS helium fraction
    !real(dp) :: t_HTS_OB_PPCS = 27.0D0  ! [cm] OB high temp. shield thickness
    !real(dp) :: alpha_HTS_OB_PPCS = 40.0D0 ! [%] OB HTS helium fraction
  
    !  Power density pre-exponential terms and decay lengths
  
    real(dp) :: q_0_BZ_breed_IB ! [W/cm^3] Pre-exp term in IB BZ breeder
    real(dp) :: q_0_BZ_breed_OB ! [W/cm^3] Pre-exp term in OB BZ breeder
    real(dp) :: lambda_q_BZ_breed_IB ! [cm] Decay length in IB BZ breeder
    real(dp) :: lambda_q_BZ_breed_OB ! [cm] Decay length in OB BZ breeder
  
    real(dp) :: q_0_BZ_Be_IB ! [W/cm^3] Pre-exp term in IB BZ Beryllium
    real(dp) :: q_0_BZ_Be_OB ! [W/cm^3] Pre-exp term in OB BZ Beryllium
    real(dp) :: lambda_q_BZ_Be_IB ! [cm] Decay length in IB BZ Beryllium
    real(dp) :: lambda_q_BZ_Be_OB ! [cm] Decay length in OB BZ Beryllium
  
    real(dp) :: q_0_BZ_steels_IB ! [W/cm^3] Pre-exp term in IB BZ steels
    real(dp) :: q_0_BZ_steels_OB ! [W/cm^3] Pre-exp term in OB BZ steels
    real(dp) :: lambda_q_BZ_steels_IB ! [cm] Decay length in IB BZ steels
    real(dp) :: lambda_q_BZ_steels_OB ! [cm] Decay length in OB BZ steels
  
    real(dp) :: lambda_EU  ! [cm] Decay length in EUROFER
    real(dp) :: lambda_q_BM_IB       ! [cm] Decay length in IB BM (calculated)
    real(dp) :: lambda_q_BM_OB       ! [cm] Decay length in OB BM (calculated)
    real(dp) :: lambda_q_BP_IB       ! [cm] Decay length in IB BP (calculated)
    real(dp) :: lambda_q_BP_OB       ! [cm] Decay length in OB BP (calculated)
    real(dp) :: lambda_q_VV ! [cm] Decay length in Vacuum Vessel
  
    !  Fast neutron flux pre-exponential terms and decay lengths
  
    real(dp) :: phi_0_n_BZ_IB  ! [n/cm^2/sec] Pre-exp term in IB BZ
    real(dp) :: phi_0_n_BZ_OB ! [n/cm^2/sec] Pre-exp term in OB BZ
    real(dp) :: lambda_n_BZ_IB ! [cm] Decay length in IB BZ
    real(dp) :: lambda_n_BZ_OB ! [cm] Decay length in OB BZ
    real(dp) :: lambda_n_VV    ! [cm] Decay length in VV
  
    !  [n/cm^2/sec] Reference fast neutron flux on VV inner side [Fish09]
  
    real(dp) :: phi_n_0_VV_ref 
    !  Vacuum vessel helium production pre-exponential terms and decay lengths
  
    real(dp) :: Gamma_He_0_ref  ! [appm/yr] Pre-exp term
    real(dp) :: lambda_He_VV  ! [cm] Decay length
  
    !  [dpa] Allowable neutron damage to the FW EUROFER
  
    real(dp) :: D_EU_max 
    !  Variables used in this module, ultimately to be set via the calling routine
    !  to values given by PROCESS variables
  
    real(dp), public :: P_n     ! [MW] Fusion neutron power
    real(dp), public :: NWL_av  ! [MW/m^2] Average neutron wall load
    real(dp), public :: f_peak  ! [--] NWL peaking factor
    real(dp), public :: t_FW_IB ! [cm] IB first wall thickness
    real(dp), public :: t_FW_OB ! [cm] OB first wall thickness
    real(dp), public :: A_FW_IB ! [cm^2] IB first wall area
    real(dp), public :: A_FW_OB ! [cm^2] OB first wall area
    real(dp), public :: A_bl_IB ! [cm^2] IB blanket area
    real(dp), public :: A_bl_OB ! [cm^2] OB blanket area
    real(dp), public :: A_VV_IB ! [cm^2] IB shield/VV area
    real(dp), public :: A_VV_OB ! [cm^2] OB shield/VV area
    real(dp), public :: CF_bl   ! [%] Blanket coverage factor
    integer, public :: n_ports_div      ! [ports] Number of divertor ports
    integer, public :: n_ports_H_CD_IB  ! [ports] Number of IB H&CD ports
    integer, public :: n_ports_H_CD_OB  ! [ports] Number of OB H&CD ports
    character(len=5), public :: H_CD_ports  ! Type of H&CD ports (small or large)
    real(dp), public :: e_Li     ! [%] Lithium 6 enrichment
    real(dp), public :: t_plant  ! [FPY] Plant lifetime
    real(dp), public :: alpha_m  ! [--] Availability factor
    real(dp), public :: alpha_puls ! [--] Pulsed regime fraction
  
    !  Breeder type (allowed values are Orthosilicate, Metatitanate or Zirconate)
  
    character(len=20), public :: breeder 
  
    !  Inboard parameters
  
    real(dp), public :: t_BZ_IB     ! [cm] BZ thickness
    real(dp), public :: t_BM_IB     ! [cm] BM thickness
    real(dp), public :: t_BP_IB     ! [cm] BP thickness
    real(dp), public :: t_VV_IB     ! [cm] VV thickness
    real(dp), public :: alpha_BM_IB  ! [%] Helium fraction in the IB BM
    real(dp), public :: alpha_BP_IB ! [%] Helium fraction in the IB BP
    real(dp), public :: chi_Be_BZ_IB ! [%] Beryllium vol. frac. in IB BZ
    real(dp), public :: chi_breed_BZ_IB ! [%] Breeder vol. frac. in IB BZ
    real(dp), public :: chi_steels_BZ_IB ! [%] Steels vol. frac. in IB BZ
  
    !  Outboard parameters
  
    real(dp), public :: t_BZ_OB     ! [cm] BZ thickness
    real(dp), public :: t_BM_OB     ! [cm] BM thickness
    real(dp), public :: t_BP_OB     ! [cm] BP thickness
    real(dp), public :: t_VV_OB     ! [cm] VV thickness
    real(dp), public :: alpha_BM_OB  ! [%] Helium fraction in the OB BM
    real(dp), public :: alpha_BP_OB ! [%] Helium fraction in the OB BP
    real(dp), public :: chi_Be_BZ_OB ! [%] Beryllium vol. frac. in OB BZ
    real(dp), public :: chi_breed_BZ_OB ! [%] Breeder vol. frac. in OB BZ
    real(dp), public :: chi_steels_BZ_OB ! [%] Steels vol. frac. in OB BZ
  
    !  Model outputs
  
    real(dp), public :: pnuctfi  ! [MW/m3] Nuclear heating on IB TF coil
    real(dp), public :: pnuctfo  ! [MW/m3] Nuclear heating on OB TF coil
    real(dp), public :: P_th_tot ! [MW] Nuclear power generated in blanket
    real(dp), public :: pnucsh   ! [MW] Nuclear power generated in shield/VV
    real(dp), public :: M_E      ! [--] Energy multiplication factor
    real(dp), public :: tbratio  ! [--] Tritium breeding ratio
    real(dp), public :: G_tot    ! [g/day] Tritium production rate
    real(dp), public :: nflutfi  ! [n/cm2] Fast neutron fluence on IB TF coil
    real(dp), public :: nflutfo  ! [n/cm2] Fast neutron fluence on OB TF coil
    real(dp), public :: vvhemini ! [appm] minimum final He. conc in IB VV
    real(dp), public :: vvhemino ! [appm] minimum final He. conc in OB VV
    real(dp), public :: vvhemaxi ! [appm] maximum final He. conc in IB VV
    real(dp), public :: vvhemaxo ! [appm] maximum final He. conc in OB VV
    real(dp), public :: t_bl_fpy ! [y] blanket lifetime in full power years
    real(dp), public :: t_bl_y   ! [y] blanket lifetime in calendar years
  
  contains
  
    subroutine init_kit_blanket_model
      !! Initialise module variables
      implicit none
  
      x_BZ_IB = 0.0D0
      x_BM_IB = 0.0D0
      x_BP_IB = 0.0D0
      x_VV_IB = 0.0D0
      x_BZ_OB = 0.0D0
      x_BM_OB = 0.0D0
      x_BP_OB = 0.0D0
      x_VV_OB = 0.0D0
      q_BZ_IB_end = 0.0D0
      q_BM_IB_end = 0.0D0
      q_BP_IB_end = 0.0D0
      q_BZ_OB_end = 0.0D0
      q_BM_OB_end = 0.0D0
      q_BP_OB_end = 0.0D0
      phi_n_vv_IB_start = 0.0D0
      phi_n_vv_OB_start = 0.0D0
      CF_bl_PPCS = 0.0D0
      lambda_q_BM_IB = 0.0D0
      lambda_q_BM_OB = 0.0D0
      lambda_q_BP_IB = 0.0D0
      lambda_q_BP_OB = 0.0D0
      pnuctfi = 0.0D0
      pnuctfo = 0.0D0
      P_th_tot = 0.0D0
      pnucsh = 0.0D0
      M_E = 0.0D0
      tbratio = 0.0D0
      G_tot = 0.0D0
      nflutfi = 0.0D0
      nflutfo = 0.0D0
      vvhemini = 0.0D0
      vvhemino = 0.0D0
      vvhemaxi = 0.0D0
      vvhemaxo = 0.0D0
      t_bl_fpy = 0.0D0
      t_bl_y = 0.0D0
      A_cov_PPCS = 1365.0D0
      A_FW_PPCS = 1253.0D0
      NWL_av_PPCS = 1.94D0
      NWL_av_IB_PPCS = 1.73D0
      NWL_av_OB_PPCS = 1.92D0
      NWL_max_IB_PPCS = 1.99D0
      NWL_max_OB_PPCS = 2.41D0
      e_Li_PPCS = 30.0D0
      t_BZ_IB_PPCS = 36.5D0
      t_BZ_OB_PPCS = 46.5D0
      TBR_PPCS = 1.12D0
      q_0_BZ_breed_IB = 31.348D0
      q_0_BZ_breed_OB = 37.144D0
      lambda_q_BZ_breed_IB = 29.42D0
      lambda_q_BZ_breed_OB = 27.03D0
      q_0_BZ_Be_IB = 9.532D0
      q_0_BZ_Be_OB = 11.809D0
      lambda_q_BZ_Be_IB = 16.39D0
      lambda_q_BZ_Be_OB = 16.39D0
      q_0_BZ_steels_IB = 16.067D0
      q_0_BZ_steels_OB = 18.788D0
      lambda_q_BZ_steels_IB = 21.27D0
      lambda_q_BZ_steels_OB = 21.27D0
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
      e_Li = 30.0D0
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
    end subroutine init_kit_blanket_model
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
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
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
      implicit none
  
      real(dp) :: f_alpha
  
      !  Arguments
  
      real(dp), intent(in) :: alpha
  
      !  Local variables
  
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
      f_alpha = 1.0D0 + 0.019D0*alpha
  
    end function f_alpha
  
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
    subroutine kit_blanket
  
      !! Main routine for the KIT HCPB blanket model
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: F Franza, KIT (original MATLAB implementation)
      !! None
      !! This routine calls the main work routines for the KIT HCPB
      !! blanket model.
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
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
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
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
  
      !! Calculates the nuclear power density profiles
      !! within the KIT blanket sub-assemblies
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
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use constants, only: pi
      implicit none
  
      !  Arguments
  
      real(dp), intent(out) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
      real(dp), intent(out) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
      real(dp), intent(out) :: pnuctfi, pnuctfo
  
      !  Local variables
  
      real(dp), dimension(np) :: q_steels_BZ_IB, q_steels_BZ_OB
      real(dp), dimension(np) :: q_BM_IB, q_BP_IB, q_VV_IB
      real(dp), dimension(np) :: q_BM_OB, q_BP_OB, q_VV_OB
  
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
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !! WP13-SYS01-A-T02 Interim Review Meeting, 10.07.2013, F. Franza
      !! (describes 26/09/2013 model refinement)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
      implicit none
  
      !  Arguments
  
      real(dp), intent(in) :: q_BZ_IB_end,q_BM_IB_end,q_BP_IB_end
      real(dp), intent(in) :: q_BZ_OB_end,q_BM_OB_end,q_BP_OB_end
      real(dp), intent(out) :: P_th_tot, M_E, pnucsh
  
      !  Local variables
  
      real(dp) :: A_BZ_breed_IB, A_BZ_breed_OB, A_BZ_Be_IB, A_BZ_Be_OB
      real(dp) :: A_BZ_steels_IB, A_BZ_steels_OB
      real(dp) :: P_BZ_breed_IB, P_BZ_Be_IB, P_BZ_steels_IB
      real(dp) :: P_BZ_IB, P_BM_IB, P_BP_IB, P_VV_IB
      real(dp) :: P_BZ_breed_OB, P_BZ_Be_OB, P_BZ_steels_OB
      real(dp) :: P_BZ_OB, P_BM_OB, P_BP_OB, P_VV_OB
      real(dp) :: P_tot_IB, P_tot_OB, P_n_FW
  
      real(dp) :: nwl_ratio, nwl_IB_ratio_PPCS, nwl_OB_ratio_PPCS
  
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
  
      !! Calculates the tritium breeding ratio for the KIT blanket
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: F Franza, KIT (original MATLAB implementation)
      !! tbr   : output real : tritium breeding ratio
      !! g_tot : output real : tritium production rate (g/day)
      !! This routine calculates the tritium breeding ratio and the rate
      !! of production of tritium in the KIT blanket design, taking into
      !! account the breeding material and the number and size of ports
      !! in the blanket.
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !! WP13-SYS01-A-T02 Interim Review Meeting, 10.07.2013, F. Franza
      !! (describes 26/09/2013 model refinement)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use error_handling, only: report_error
      
      implicit none
  
      !  Arguments
  
      real(dp), intent(out) :: tbr, g_tot
  
      !  Local variables
  
      real(dp) :: wib, wob
      real(dp), parameter :: wib_PPCS = 0.28D0, wob_PPCS = 0.72D0
  
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
        !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
        !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
        !! Karlsruhe Institute of Technology, January 2013;
        !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
        implicit none
  
        real(dp) :: TBR_breed
  
        !  Arguments
  
        real(dp), intent(in) :: e_Li
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
        !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
        !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
        !! Karlsruhe Institute of Technology, January 2013;
        !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
        implicit none
  
        real(dp) :: TBR_ports
  
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
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
      implicit none
  
      !  Arguments
  
      real(dp), intent(out) :: phi_n_VV_IB_start,phi_n_VV_OB_start
      real(dp), intent(out) :: phi_n_IB_TFC, phi_n_OB_TFC
  
      !  Local variables
  
      integer, parameter :: K_tau = 31536000  ! [sec/yr] Number of seconds per year
      real(dp), dimension(np) :: phi_n_BZ_IB, phi_n_BM_IB
      real(dp), dimension(np) :: phi_n_BP_IB, phi_n_VV_IB
      real(dp), dimension(np) :: phi_n_BZ_OB, phi_n_BM_OB
      real(dp), dimension(np) :: phi_n_BP_OB, phi_n_VV_OB
      real(dp) :: nwl_ratio
  
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
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
      implicit none
  
      !  Arguments
  
      real(dp), intent(in) :: phi_n_VV_IB_start,phi_n_VV_OB_start
      real(dp), intent(out) :: vvhemini,vvhemino,vvhemaxi,vvhemaxo
  
      !  Local variables
  
      real(dp), dimension(np) :: Gamma_He_IB, Gamma_He_OB
      real(dp), dimension(np) :: C_He_IB, C_He_OB
  
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
  
      !! Calculates the blanket lifetime
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: F Franza, KIT (original MATLAB implementation)
      !! t_bl_fpy : output real : blanket lifetime (full power years)
      !! t_bl_y   : output real : blanket lifetime (calendar years)
      !! This routine calculates the blanket lifetime, assuming that the
      !! maximum allowed neutron damage to the EUROFER steel is 60 dpa.
      !! FU-TF1.1-12/003/01, Development of a new HCPB Blanket Model
      !! for Fusion Reactor System Codes, F. Franza and L. V. Boccaccini,
      !! Karlsruhe Institute of Technology, January 2013;
      !! EFDA IDM reference EFDA_D_2LKMCT, v1.0 (Appendix 2)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          use maths_library, only: linesolv
      implicit none
  
      !  Arguments
  
      real(dp), intent(out) :: t_bl_FPY, t_bl_Y
  
      !  Local variables
  
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
      !  Lifetime in full-power years
      !  10 dpa equates to 1 MW-yr/m2 (steel)
  
      t_bl_FPY = D_EU_max / (10.0D0*NWL_av*f_peak)
  
      !  Lifetime in calendar years, given availability and pulsed factors
  
      t_bl_Y = t_bl_FPY / (alpha_m*alpha_puls)
  
    end subroutine blanket_lifetime
  
end module kit_blanket_model