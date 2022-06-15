module sctfcoil_module

!! Module containing superconducting TF coil routines
!! author: P J Knight, CCFE, Culham Science Centre
!! author: J Morris, CCFE, Culham Science Centre
!! author: S Kahn, CCFE, Culham Science Centre
!! N/A
!! This module contains routines for calculating the
!! parameters of a superconducting TF coil system for a
!! fusion power plant.
!! PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifndef dp
   use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
   use resistive_materials, only: resistive_material, volume_fractions, &
      supercon_strand
   implicit none

! Module variables
!-----------------

   real(dp) :: tf_fit_t
!! Dimensionless winding pack width

   real(dp) :: tf_fit_z
!! Dimensionless winding pack radial thickness

   real(dp) :: tf_fit_y
!! Ratio of peak field with ripple to nominal axisymmetric peak field

   real(dp) :: tfc_current
!! Current in each TF coil

   real(dp) :: awpc
!! Total cross-sectional area of winding pack including
!! GW insulation and insertion gap [m2]

   real(dp) :: awptf
!! Total cross-sectional area of winding pack without
!! ground insulation and insertion gap [m2]

   real(dp) :: a_tf_steel
!! Inboard coil steel coil cross-sectional area [m2]

   real(dp) :: a_tf_ins
!! Inboard coil insulation cross-section per coil [m2]

   real(dp) :: f_tf_steel
!! Inboard coil steel fraction [-]

   real(dp) :: f_tf_ins
!! Inboard coil insulation fraction [-]

   real(dp) :: h_cp_top
!! Vertical distance from the midplane to the top of the tapered section [m]

   real(dp) :: r_tf_outboard_in
!! Radial position of plasma-facing edge of TF coil outboard leg [m]

   real(dp) :: r_tf_outboard_out
!! Radial position of outer edge of TF coil inboard leg [m]

   real(dp) :: r_wp_inner
!! Radial position of inner edge and centre of winding pack [m]

   real(dp) :: r_wp_outer
!! Radial position of outer edge and centre of winding pack [m]

   real(dp) :: r_wp_centre
!! Radial position of centre and centre of winding pack [m]

   real(dp) :: dr_tf_wp_top
!! Conductor layer radial thickness at centercollumn top [m]
!! Ground insulation layer included, only defined for itart = 1

   real(dp) :: vol_ins_cp
!! CP turn insulation volume [m3]

   real(dp) :: vol_gr_ins_cp
!! CP ground insulation volume [m3]

   real(dp) :: vol_case_cp
!! Volume of the CP outer casing cylinder

   real(dp) :: t_wp_toroidal
!! Minimal toroidal thickness of of winding pack [m]

   real(dp) :: t_wp_toroidal_av
!! Averaged toroidal thickness of of winding pack [m]

   real(dp) :: t_lat_case_av
!! Average lateral casing thickness [m]

   real(dp) :: a_case_front
!! Front casing area [m2]

   real(dp) :: a_case_nose
!! Nose casing area [m2]

   real(dp) :: a_ground_ins
!! Inboard mid-plane cross-section area of the WP ground insulation [m2]

   real(dp) :: a_leg_ins
!! TF ouboard leg turn insulation area per coil [m2]

   real(dp) :: a_leg_gr_ins
!! TF outboard leg ground insulation area per coil [m2]

   real(dp) :: a_leg_cond
!! Exact TF ouboard leg conductor area [m2]

   real(dp) :: theta_coil
!! Half toroidal angular extent of a single TF coil inboard leg

   real(dp) :: tan_theta_coil
!! Tan half toroidal angular extent of a single TF coil inboard leg

   real(dp) :: t_conductor_radial, t_conductor_toroidal
!! Conductor area radial and toroidal dimension (integer turn only) [m]

   real(dp) :: t_cable_radial, t_cable_toroidal
!! Cable area radial and toroidal dimension (integer turn only) [m]

   real(dp) :: t_turn_radial, t_turn_toroidal
!! Turn radial and toroidal dimension (integer turn only) [m]

   real(dp) :: t_cable
!! Cable area averaged dimension (square shape) [m]

   real(dp) :: vforce_inboard_tot
!! Total inboard vertical tension (all coils) [N]

   type(resistive_material), private :: copper
   type(resistive_material), private :: hastelloy
   type(resistive_material), private :: solder
   type(resistive_material), private :: jacket
   type(resistive_material), private :: helium

! croco_strand
   real(dp) :: croco_strand_area
   real(dp) :: croco_strand_critical_current

! conductor
   real(dp) :: conductor_copper_area,  conductor_copper_fraction
   real(dp) :: conductor_copper_bar_area
   real(dp) :: conductor_hastelloy_area, conductor_hastelloy_fraction
   real(dp) :: conductor_helium_area, conductor_helium_fraction
   real(dp) :: conductor_solder_area, conductor_solder_fraction
   real(dp) :: conductor_jacket_area, conductor_jacket_fraction
   real(dp) :: conductor_rebco_area,  conductor_rebco_fraction
   real(dp) :: conductor_critical_current
   real(dp) :: conductor_acs
   real(dp) :: conductor_area
!! Area of cable space inside jacket

   real(dp):: T1, time2, tau2, estotft
! (OBSOLETE, but leave for moment)
! real (kind(1.0D0)) ::croco_quench_factor
! real(dp):: jwdgpro_1, jwdgpro_2,  etamax

! Var in tf_res_heating requiring re-initialisation on each new run
! Not sure what is really doing --> to be checked
   integer :: is_leg_cp_temp_same

contains

   subroutine init_sctfcoil_module
      !! Initialise module variables
      implicit none

      is_leg_cp_temp_same = 0
      tf_fit_t = 0.0D0
      tf_fit_z = 0.0D0
      tf_fit_y = 0.0D0
      tfc_current = 0.0D0
      awpc = 0.0D0
      awptf = 0.0D0
      a_tf_steel = 0.0D0
      a_tf_ins = 0.0D0
      f_tf_steel = 0.0D0
      f_tf_ins = 0.0D0
      h_cp_top = 0.0D0
      r_tf_outboard_in = 0.0D0
      r_tf_outboard_out = 0.0D0
      r_wp_inner = 0.0D0
      r_wp_outer = 0.0D0
      r_wp_centre = 0.0D0
      dr_tf_wp_top = 0.0D0
      vol_ins_cp = 0.0d0
      vol_gr_ins_cp = 0.0D0
      vol_case_cp = 0.0D0
      t_wp_toroidal = 0.0D0
      t_wp_toroidal_av = 0.0D0
      t_lat_case_av = 0.0D0
      a_case_front = 0.0D0
      a_case_nose = 0.0D0
      a_ground_ins = 0.0D0
      a_leg_ins = 0.0D0
      a_leg_gr_ins = 0.0D0
      a_leg_cond = 0.0D0
      theta_coil = 0.0D0
      tan_theta_coil = 0.0D0
      t_conductor_radial = 0.0D0
      t_conductor_toroidal = 0.0D0
      t_cable_radial = 0.0D0
      t_cable_toroidal = 0.0D0
      t_turn_radial = 0.0D0
      t_turn_toroidal = 0.0D0
      t_cable = 0.0D0
      vforce_inboard_tot = 0.0D0
      T1 = 0.0D0
      time2 = 0.0D0
      tau2 = 0.0D0
      estotft = 0.0D0
   end subroutine init_sctfcoil_module

! --------------------------------------------------------------------------
   subroutine initialise_cables()
      use rebco_variables, only: copper_rrr

      implicit none

      copper%rrr = copper_rrr
      copper%density = 8960.0d0
      hastelloy%density = 8890.0d0
      ! Solder: 60EN ie 60%Sn + 40%Pb solder (approx the same as eutectic 63/37)
      solder%density = 8400.0d0
      jacket%density = 8000.0d0       ! 304 stainless
   end subroutine initialise_cables
! --------------------------------------------------------------------------

   subroutine generalized_plane_strain(nu_p, nu_z, ey_p, ey_z, rad, d_curr, v_force,   &
      nlayers, n_radial_array, i_tf_bucking,          &
      rradius, sigr, sigt, sigz,              &
      str_r, str_t, str_z, r_deflect )
      !! TN: This subroutine won't be wrapped and will likely be replaced pending
      !! discussion in #1670
      !! Author : S. Kahn, CCFE
      !! Jan 2020
      !! This subroutine estimates the normal stresses/strains and radial displacement
      !! radial distributions of a multilayer cylinder with forces at its ends,
      !! assuming the generalized plain strain formulation. This formlation relies
      !! on the fact that the vertical forces are applied far enough at the ends
      !! so that vertical strain can be approximated radially constant.
      !! The form of the radial displacement is calculated in the reference (issue #991)
      !! up to two integration constants c1 and c2 per layers. As the geometry is
      !! cylindrical it is enough to deduce all the normal stress/strains distributions.
      !! The c1 and c2 constants are then estimated from the inter-layers boundary
      !! conditions, assuming radial displacement and normal stress continuity,
      !! and null radial stress at the TF(-CS) system, using a marix formulation.
      !! A more recent possiblity consider designs where the CS as a support
      !! for the TF centering pressure (B&W). For this design, the TF coil and the
      !! CS vertical strains must be insulated as the TF gets taller with the
      !! current and the CS shrinks with current. Hence, two vertical boundary
      !! conditions must be applied separately on the strain generalization
      !! parameter calculation and removing the inter TF-CS layers matrix cross
      !!  terms.
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use constants, only: rmu0, pi
      use maths_library, only: linesolv
      implicit none

      ! Inputs
      ! ---
      integer, intent(in) :: n_radial_array
      !! Number of elements per layers used in stress analysis
      !! quantities arrays (stress, strain, displacement)

      integer, intent(in) :: nlayers
      !! Total number of layers

      integer, intent(in) :: i_tf_bucking
      !! Switch for bucking cylinder (case)
      !!   0 : Free standing TF without case/bucking cyliner (only a conductor layer)
      !!   1 : Free standing TF with a case/bucking cylinder (material depends on i_tf_sup)
      !!   2 : Bucked and wedged design, CS frictionally decoupled from TF (no interlayer)
      !!   3 : Bucked and wedged design, CS and Kapton interlayer decoupled from TF

      real(dp), dimension(nlayers), intent(in) :: nu_p
      !! Toroidal plan's Poisson's ratios

      real(dp), dimension(nlayers), intent(in) :: nu_z
      !! Toroidal plan to vertical direction's Poisson's ratios

      real(dp), dimension(nlayers), intent(in) :: ey_p
      !! Toroidal plan's Young modulae

      real(dp), dimension(nlayers), intent(in) :: ey_z
      !! Vertical direction's Young modulae

      real(dp), dimension(nlayers), intent(in) :: d_curr
      !! Layers current densities [A.m-2]

      real(dp), dimension(nlayers+1), intent(in) :: rad
      !! Radii of the layers boundaries [m], starting from the innermost
      !! i.e. the blking/casing cylinder

      real(dp), intent(in) :: v_force
      !! Electromecanical vertical forces
      ! ---


      ! Outputs
      ! ---
      real(dp), dimension(n_radial_array*nlayers), intent(out) :: sigr
      !! Stress distribution in the radial direction (r) [Pa]

      real(dp), dimension(n_radial_array*nlayers), intent(out) :: sigt
      !! Stress distribution in the toroidal direction (t) [Pa]

      real(dp), dimension(n_radial_array*nlayers), intent(out) :: sigz
      !! Stress distribution in the vertical direction (z)

      real(dp), dimension(n_radial_array*nlayers), intent(out) :: str_r
      !! Strain distribution in the radial direction (r)

      real(dp), dimension(n_radial_array*nlayers), intent(out) :: str_t
      !! Strain distribution in the toroidal direction (t)

      real(dp), dimension(n_radial_array*nlayers), intent(out) :: str_z
      !! Uniform strain in the vertical direction (z)

      real(dp), dimension(n_radial_array*nlayers), intent(out) :: r_deflect
      !! Radial displacement radial distribution [m]

      real(dp), dimension(nlayers*n_radial_array), intent(out) :: rradius
      !! Radius array [m]
      ! ---


      ! Local variables
      ! ---
      real(dp), dimension(nlayers) :: alpha
      real(dp), dimension(nlayers) :: beta
      !! Lorentz body force parametres

      real(dp), dimension(nlayers) :: kk_p
      real(dp), dimension(nlayers) :: kk_z
      !! Toroidal plane / vertical direction hooke's law coeficient

      real(dp), dimension(nlayers) :: nu_z_eff2
      !! Toroidal plan to vertical direction poisson's squared coefficient

      real(dp), dimension(nlayers) :: fr_par
      !! Body force parameter in displacement differential equation

      real(dp), dimension(nlayers) :: cc_par_sig
      real(dp), dimension(nlayers) :: alpha_par_sigr
      real(dp), dimension(nlayers) :: alpha_par_sigt
      real(dp), dimension(nlayers) :: beta_par_sigr
      real(dp), dimension(nlayers) :: beta_par_sigt
      !! Radial/toroidal stress constant parameters

      real(dp), dimension(nlayers) :: area
      !! Layer area

      real(dp) :: sum_1
      real(dp) :: sum_2
      real(dp), dimension(nlayers) :: aleph
      real(dp), dimension(nlayers) :: beth
      real(dp), dimension(nlayers) :: par_1
      real(dp), dimension(nlayers) :: par_2
      !! Vertical strain parameters

      real(dp), dimension(2*nlayers, 2*nlayers) :: aa
      !! Matrix encoding the integration constant cc coeficients

      real(dp), dimension(2*nlayers) :: bb
      !! Vector encoding the alpha/beta (lorentz forces) contribution

      real(dp), dimension(2*nlayers) :: cc
      real(dp), dimension(nlayers) :: c1, c2
      !! Integration constants vector (solution)

      real(dp) :: dradius
      real(dp) :: inner_layer_curr
      !! Variables used for radial stress distribution

      real(dp), dimension(2) :: str_z_calc
      !! Constraint strains for calculation (on for TF and CS systems)

      integer :: ii  ! Line in the aa matrix
      integer :: jj  ! Collumn in the aa matrix
      !! Indices
      ! ---

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! The stress calcualtion differential equations is analytically sloved
      ! The final solution is given by the layer boundary conditions on
      ! radial stress and displacement between layers solved
      ! The problem is set as aa.cc = bb, cc being the constant we search
      ! ------

      ! Layer parameterisation
      ! ***
      ! Vertical poisson's squared coefficient (array equation)
      nu_z_eff2 = nu_z**2 * ey_p / ey_z

      ! Stress to strain coeficients (array equation)
      kk_p = ey_p / ( 1.0D0 - nu_p - 2.0D0*nu_z_eff2 )
      kk_z = ey_z / ( 1.0D0 - nu_p - 2.0D0*nu_z_eff2 )

      ! Body force parameter in displacement differential equation (array equation)
      fr_par = ( 1.0D0 + nu_p ) / ( kk_p * ( 1.0D0 - nu_z_eff2 ) )

      ! Lorentz forces parametrisation coeficients (array equation)
      alpha = 0.5D0*rmu0 * d_curr**2 * fr_par

      inner_layer_curr = 0.0D0
      do ii = 1, nlayers

         beta(ii) = 0.5D0*rmu0 * d_curr(ii) * fr_par(ii) &
            * ( inner_layer_curr - pi*d_curr(ii)*rad(ii)**2 ) / pi

         ! Layer area
         area(ii) = pi * (rad(ii+1)**2 - rad(ii)**2)

         ! Total current carried by the inners layers
         inner_layer_curr = inner_layer_curr + area(ii)*d_curr(ii)
      end do

      ! Constant radial/toroidal stress parameters associated cc, alpha/8 and beta/2
      !-!
      ! array equations
      cc_par_sig = ( nu_p - 1.0D0 + 2.0D0*nu_z_eff2 ) / ( 1.0D0 + nu_p )
      alpha_par_sigr = ( 3.0D0 + 1.0D0*nu_p - 2.0D0*nu_z_eff2 ) / ( 1.0D0 + nu_p )
      alpha_par_sigt = ( 1.0D0 + 3.0D0*nu_p + 2.0D0*nu_z_eff2 ) / ( 1.0D0 + nu_p )
      beta_par_sigr = ( 1.0D0 - nu_z_eff2 ) / ( 1.0D0 + nu_p )
      beta_par_sigt = ( nu_p  + nu_z_eff2 ) / ( 1.0D0 + nu_p )
      !-!


      ! Plain strain generalisation parameters
      ! Rem : if i_tf_bucking >= 2, the CS is used as a TF support structure
      !       with vertical strain insulation. If so, two vertical boundary
      !       conditions (strain generalization) must be considered.
      !-!
      ! Cylindrical integrals parameters
      do ii = 1, nlayers
         par_1(ii) = pi * (rad(ii+1)**4 - rad(ii)**4)
         par_2(ii) = pi * (log(rad(ii+1)) * rad(ii+1)**2 - log(rad(ii)) * rad(ii)**2)
      end do

      ! CS layer parameter
      ! Rem : no CS vertical tension (uncoupled & CS flux swing)
      sum_1 = 0.0D0
      sum_2 = 0.0D0
      aleph(:) = 0.0D0
      beth(:) = 0.0D0
      if ( i_tf_bucking >= 2 ) then
         do ii = 1, i_tf_bucking - 1
            sum_1 = sum_1 + kk_z(ii) * ( 1.0D0 - nu_p(ii) ) * area(ii)
         end do
         do ii = 1, i_tf_bucking - 1
            beth(ii) = - ( 2.0D0 * area(ii) * kk_z(ii) * nu_z_eff2(ii) / nu_z(ii) ) / sum_1
         end do
      end if

      ! TF coil layers parameters
      sum_1 = 0.0D0
      sum_2 = 0.0D0
      do ii = max( 1, i_tf_bucking ), nlayers
         sum_1 = sum_1 + kk_z(ii) * ( 1.0D0 - nu_p(ii) ) * area(ii)
         sum_2 = sum_2 + kk_z(ii) * ( nu_z_eff2(ii)/nu_z(ii) )  &
            * ( 0.25D0 * alpha(ii) * par_1(ii) + beta(ii) * par_2(ii) )
      end do

      ! TF bucking/nose casing layer
      do ii = max( 1, i_tf_bucking ), nlayers
         aleph(ii) = (v_force - sum_2) / sum_1
         beth(ii) = - ( 2.0D0 * area(ii) * kk_z(ii) * nu_z_eff2(ii) / nu_z(ii) ) / sum_1
      end do
      !-!
      ! ***


      ! Left hand side matrix aa
      ! ***
      aa(:,:) = 0.0D0

      ! Null radial stress at R(1)
      aa(1,1) = kk_p(1)
      aa(1,2) = kk_p(1) * cc_par_sig(1) / rad(1)**2

      ! Free standing TF system plain strain generalisation
      if ( i_tf_bucking <= 1 ) then ! Free standing TF case
         do jj = 1, nlayers
            aa(1, 2*jj-1) = aa(1, 2*jj-1) + beth(jj)*kk_p(1)*nu_z(1)
         end do

         ! CS system plane strain generalization
      else if ( i_tf_bucking >= 2 ) then ! TF case bucked on CS
         do jj = 1, i_tf_bucking - 1
            aa(1, 2*jj-1) = aa(1, 2*jj-1) + beth(jj)*kk_p(1)*nu_z(1)
         end do
      end if

      ! Inter-layer boundary conditions
      if ( nlayers >= 2 ) then

         ! Plane strain
         do ii = 1, nlayers - 1

            ! Continuous radial normal stress at R(ii+1)
            aa(2*ii, 2*ii-1) = kk_p(ii)
            aa(2*ii, 2*ii  ) = kk_p(ii) * cc_par_sig(ii) / rad(ii+1)**2
            aa(2*ii, 2*ii+1) = -kk_p(ii+1)
            aa(2*ii, 2*ii+2) = -kk_p(ii+1) * cc_par_sig(ii+1) / rad(ii+1)**2

            ! Continuous displacement at R(ii+1)
            aa(2*ii+1, 2*ii-1) = rad(ii+1)
            aa(2*ii+1, 2*ii  ) = 1.0D0 / rad(ii+1)
            aa(2*ii+1, 2*ii+1) = -rad(ii+1)
            aa(2*ii+1, 2*ii+2) = -1.0D0 / rad(ii+1)
         end do

         ! Free standing TF plain strain generalisation
         if ( i_tf_bucking <= 1 ) then !
            do ii = 1, nlayers - 1
               do jj = 1, nlayers
                  aa(2*ii, 2*jj-1) = aa(2*ii, 2*jj-1) &
                     + beth(jj)*(kk_p(ii)*nu_z(ii) - kk_p(ii+1)*nu_z(ii+1))
               end do
            end do

            ! TF case bucked on CS
         else if ( i_tf_bucking == 2 ) then

            ! Layer 1-2 interface (vertical strain decoupled)
            aa(2,1) = aa(2,1) + beth(1) * kk_p(1) * nu_z(1)
            do jj = 2, nlayers
               aa(2, 2*jj-1) = aa(2, 2*jj-1) - beth(jj) * kk_p(2)*nu_z(2)
            end do

            ! Remaining TF interfaces
            do ii = 2, nlayers - 1
               do jj = 2, nlayers
                  aa(2*ii, 2*jj-1) = aa(2*ii, 2*jj-1) &
                     + beth(jj)*(kk_p(ii)*nu_z(ii) - kk_p(ii+1)*nu_z(ii+1))
               end do
            end do

            ! TF case bucked on CS with TF-CS interlayer
         else if ( i_tf_bucking == 3 ) then

            ! Layer 1-2 interface
            do jj = 1, 2
               aa(2, 2*jj-1) = aa(2, 2*jj-1) + beth(jj) * ( kk_p(1)*nu_z(1) - kk_p(2)*nu_z(2) )
            end do

            ! Layer 2-3 interface (vertical strain decoupled)
            do jj = 1, 2
               aa(4, 2*jj-1) = aa(4, 2*jj-1) + beth(jj) * kk_p(2) * nu_z(2)
            end do
            do jj = 3, nlayers
               aa(4, 2*jj-1) = aa(4, 2*jj-1) - beth(jj) * kk_p(3) * nu_z(3)
            end do

            ! Remaining TF interfaces
            do ii = 3, nlayers - 1
               do jj = 3, nlayers
                  aa(2*ii, 2*jj-1) = aa(2*ii, 2*jj-1) &
                     + beth(jj)*(kk_p(ii)*nu_z(ii) - kk_p(ii+1)*nu_z(ii+1))
               end do
            end do
         end if
      end if

      ! Null radial stress at outermost radius at R(nlayers+1)
      aa(2*nlayers, 2*nlayers - 1) = kk_p(nlayers)
      aa(2*nlayers, 2*nlayers    ) = kk_p(nlayers) * cc_par_sig(nlayers) / rad(nlayers+1)**2

      ! Plain strain generalisation
      do jj = max(i_tf_bucking, 1), nlayers
         aa(2*nlayers, 2*jj-1) = aa(2*nlayers, 2*jj-1) + beth(jj)*kk_p(nlayers)*nu_z(nlayers)
      end do
      ! ***

      ! Right hand side vector bb
      ! ***
      ! Null radial stress at R(1)
      bb(1) = -kk_p(1) * ( 0.125D0*alpha(1) * rad(1)**2 * alpha_par_sigr(1)     &
         + 0.5D0*beta(1) * ( beta_par_sigr(1) + log(rad(1)) )   &
         + nu_z(1)*aleph(1) ) ! Plain strain generalisation

      ! Inter-layer boundary conditions
      if ( nlayers /= 1 ) then
         do ii = 1, nlayers - 1

            ! Continuous radial normal stress at R(ii+1)
            bb(2*ii) = - kk_p(ii) * ( 0.125D0*alpha(ii) * rad(ii+1)**2 * alpha_par_sigr(ii)   &
               + 0.5D0*beta(ii) * ( beta_par_sigr(ii) + log(rad(ii+1)) ) &
               + aleph(ii) * nu_z(ii) )        & ! Plain strain generalisation line
               + kk_p(ii+1) * ( 0.125D0*alpha(ii+1) * rad(ii+1)**2 * alpha_par_sigr(ii+1)  &
               + 0.5D0*beta(ii+1) * ( beta_par_sigr(ii+1) + log(rad(ii+1))) &
               + aleph(ii+1) * nu_z(ii+1) )    ! Plain strain generalisation line

            ! Continuous displacement at R(ii+1)
            bb(2*ii+1) = - 0.125D0*alpha(ii) * rad(ii+1)**3 - 0.5D0*beta(ii) * rad(ii+1)*log(rad(ii+1))  &
               + 0.125D0*alpha(ii+1)* rad(ii+1)**3 + 0.5D0*beta(ii+1)*rad(ii+1)*log(rad(ii+1))

         end do
      end if

      ! Null radial stress at R(nlayers+1)
      bb(2*nlayers) = -kk_p(nlayers) * ( 0.125D0*alpha(nlayers)*rad(nlayers+1)**2 * alpha_par_sigr(nlayers)    &
         + 0.5D0*beta(nlayers) * (beta_par_sigr(nlayers) + log(rad(nlayers+1)) ) &
         + nu_z(nlayers)*aleph(nlayers) )   ! Plain strain generalisation
      ! ***

      !  Find solution vector cc
      ! ***
      cc(:) = 0.0D0
      call linesolv(aa, 2*nlayers, bb, cc)

      do ii = 1, nlayers
         c1(ii) = cc(2*ii-1)
         c2(ii) = cc(2*ii)
      end do
      ! ***
      ! ------


      ! Radial/toroidal/vertical stress radial distribution
      ! ------
      rradius(:) = 0.0D0
      sigr(:) = 0.0D0
      sigt(:) = 0.0D0
      sigz(:) = 0.0D0
      str_r(:) = 0.0D0
      str_t(:) = 0.0D0
      str_z(:) = 0.0D0
      r_deflect(:) = 0.0D0

      ! CS system vertical strain
      if ( i_tf_bucking >= 2 ) then
         str_z_calc(1) = aleph(1)
         do ii = 1, i_tf_bucking - 1
            str_z_calc(1) = str_z_calc(1) + c1(ii) * beth(ii)
         end do
      end if

      ! TF system vertical normal strain (constant) WRONG IF GRADED COIL
      str_z_calc(2) = aleph(nlayers)
      do ii = max( 1, i_tf_bucking ), nlayers
         str_z_calc(2) = str_z_calc(2) + c1(ii) * beth(ii)
      end do


      ! Radial displacement, stress and strain distributions
      do ii = 1, nlayers

         dradius = (rad(ii+1) - rad(ii)) / dble(n_radial_array - 1 )
         do jj = (ii-1)*n_radial_array + 1, ii*n_radial_array

            rradius(jj) = rad(ii) + dradius*dble(jj - n_radial_array*(ii-1) - 1)

            ! Radial normal stress
            sigr(jj) = kk_p(ii)*( c1(ii) + cc_par_sig(ii)*c2(ii)/rradius(jj)**2       &
               + 0.125D0*alpha(ii)*alpha_par_sigr(ii)*rradius(jj)**2 &
               + 0.5D0*beta(ii)*(beta_par_sigr(ii) + log(rradius(jj)) ) )

            ! Toroidal normal stress
            sigt(jj) = kk_p(ii)*( c1(ii) - cc_par_sig(ii)*c2(ii)/rradius(jj)**2       &
               + 0.125D0*alpha(ii)*alpha_par_sigt(ii)*rradius(jj)**2 &
               + 0.5D0*beta(ii)*(beta_par_sigt(ii) + log(rradius(jj)) ) )

            ! Vertical normal stress
            sigz(jj) = kk_z(ii) * nu_z_eff2(ii) / nu_z(ii)                   &
               * ( 2.0D0*c1(ii) + 0.5D0*alpha(ii) * rradius(jj)**2     &
               + 0.5D0*beta(ii) * ( 1.0D0 + 2.0D0*log(rradius(jj)) ) )

            ! No vertical strain effect on CS / TF-CS inter layer
            if ( ii >= i_tf_bucking ) then ! TF system
               sigr(jj) = sigr(jj) + kk_p(ii) * str_z_calc(2) * nu_z(ii)
               sigt(jj) = sigt(jj) + kk_p(ii) * str_z_calc(2) * nu_z(ii)
               sigz(jj) = sigz(jj) + kk_z(ii) * str_z_calc(2) * (1.0D0 - nu_p(ii))
            else ! CS system
               sigr(jj) = sigr(jj) + kk_p(ii) * str_z_calc(1) * nu_z(ii)
               sigt(jj) = sigt(jj) + kk_p(ii) * str_z_calc(1) * nu_z(ii)
               sigz(jj) = sigz(jj) + kk_z(ii) * str_z_calc(1) * (1.0D0 - nu_p(ii))
            end if

            ! Radial normal strain
            str_r(jj) = c1(ii) - c2(ii) / rradius(jj)**2                      &
               + 0.375D0*alpha(ii) * rradius(jj)**2 + 0.5D0*beta(ii) &
               * (1.0D0 + log(rradius(jj)))

            ! Toroidal normal strain
            str_t(jj) = c1(ii) + c2(ii) / rradius(jj)**2                       &
               + 0.125D0*alpha(ii) * rradius(jj)**2 + 0.5D0*beta(ii) &
               * log(rradius(jj))

            ! Vertical normal strain
            if ( ii >= i_tf_bucking ) then
               str_z(jj) = str_z_calc(2)
            else
               str_z(jj) = str_z_calc(1)
            end if

            ! Radial displacement
            r_deflect(jj) = c1(ii)*rradius(jj) + c2(ii)/rradius(jj) &
               + 0.125D0*alpha(ii) * rradius(jj)**3    &
               + 0.5D0*beta(ii) * rradius(jj)*log(rradius(jj))

         end do ! layer array loop
      end do ! Layer loop
      ! ------

   end subroutine generalized_plane_strain


end module sctfcoil_module
