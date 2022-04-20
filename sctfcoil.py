from process.fortran import physics_variables
from process.fortran import maths_library
from process.fortran import build_variables
from process.fortran import constants
from process.fortran import tfcoil_variables
from process.fortran import rebco_variables
from process.fortran import fwbs_variables
from process.fortran import numerics


class Sctfcoil:
    def __init__(self):
        self.outfile = constants.nout

    
    def extended_plane_strain(self):
        """
        
        """
            nonslip_layer = i_tf_bucking
    if ( nonslip_layer < 1) :
        nonslip_layer = 1


    # Stiffness tensor factors
    # Section 3 in the writeup
    # With Section 12 anisotropic materials properties
    # ***
    # Dependent Poisson's ratio: nu-transverse-axial
    # from nu-axial-transverse and the Young's moduli
    nu_tz = nu_zt * ey_t / ey_z

    # Effective Young's Moduli and Poisson's ratios
    # holding strain, not stress, cross-terms constant
    ey_bar_z  = ey_z * (1-nu_t) / (1-nu_t-2*nu_tz*nu_zt)
    ey_bar_t  = ey_t * (1-nu_tz*nu_zt) / (1-nu_t-2*nu_tz*nu_zt) / (1+nu_t)
    nu_bar_t  = (nu_t+nu_tz*nu_zt) / (1-nu_tz*nu_zt)
    nu_bar_tz = nu_tz / (1-nu_t)
    nu_bar_zt = nu_zt * (1+nu_t) / (1-nu_tz*nu_zt)

    # Lorentz force parameters
    # Section 13 in the writeup
    # ***
    # Currents in each layer [A]
    currents = pi * d_curr * (rad(2:nlayers+1)**2 - rad(1:nlayers)**2)
    # Currents enclosed by inner radius of each layer [A]
    currents_enclosed(1) = 0.0e0
    do ii = 2, nlayers
        currents_enclosed(ii) = currents_enclosed(ii-1) + currents(ii-1)
    end do
    # Factor that multiplies r linearly in the force density
    f_lin_fac = rmu0/2.0e0 * d_curr**2
    # Factor that multiplies r reciprocally in the force density
    f_rec_fac = rmu0/2.0e0 * (d_curr * currents_enclosed / pi - d_curr**2 * rad(1:nlayers)**2)
    # Force density integral that adds to Lame parameter A
    f_int_A   = 0.5e0*f_lin_fac * (rad(2:nlayers+1)**2-rad(1:nlayers)**2) + f_rec_fac * log(rad(2:nlayers+1)/(rad(1:nlayers)))
    if ( f_rec_fac(1) == 0e0) :
        f_int_A(1) = 0.5e0*f_lin_fac(1) * (rad(2)**2-rad(1)**2)


    # Force density integral that adds to Lame parameter B
    f_int_B   = 0.25e0*f_lin_fac * (rad(2:nlayers+1)**4-rad(1:nlayers)**4)         + 0.5e0*f_rec_fac * (rad(2:nlayers+1)**2-rad(1:nlayers)**2)

    # Transformation matrix from outer to inner Lame parameters
    # Section 5 in the writeup
    # With Section 12 anisotropic materials properties
    # ***
    # M_int(kk) multiplies Lame parameter vector of layer kk (A,B,eps_z,1.0,eps_z_slip)
    # and transforms the values at the outer radius to the values at the inner radius
    M_int = 0.0e0
    do kk = 1, nlayers
        M_int(1,1,kk) = 1.0e0
        M_int(2,2,kk) = 1.0e0
        M_int(3,3,kk) = 1.0e0
        M_int(4,4,kk) = 1.0e0
        M_int(5,5,kk) = 1.0e0

        M_int(1,4,kk) = -0.5e0 / ey_bar_t(kk) * f_int_A(kk);
        M_int(2,4,kk) =  0.5e0 / ey_bar_t(kk) * f_int_B(kk);
    end do

    # Transformation matrix between layers
    # Section 6 in the writeup
    # With Section 12 anisotropic materials properties
    # With Section 15 inner slip-decoupled layers
    # ***
    # M_ext(kk) multiplies Lame parameter vector of layer kk (A,B,eps_z,1.0,eps_z_slip)
    # and transforms the values at the inner radius to the values at the outer radius
    # of layer kk-1
    M_ext = 0.0e0
    do kk = 2, nonslip_layer-1
        ey_fac = ey_bar_t(kk)/ey_bar_t(kk-1)
        M_ext(1,3,kk) = 0.0e0
        M_ext(1,5,kk) = 0.5e0 * (ey_fac*nu_bar_zt(kk) - nu_bar_zt(kk-1))
    end do
    if ( nonslip_layer > 1) :
        kk = nonslip_layer
        ey_fac = ey_bar_t(kk)/ey_bar_t(kk-1)
        M_ext(1,3,kk) = 0.5e0 * ey_fac * nu_bar_zt(kk)
        M_ext(1,5,kk) = 0.5e0 * (-nu_bar_zt(kk-1))

    do kk = nonslip_layer+1, nlayers
        ey_fac = ey_bar_t(kk)/ey_bar_t(kk-1)
        M_ext(1,3,kk) = 0.5e0 * (ey_fac*nu_bar_zt(kk) - nu_bar_zt(kk-1))
        M_ext(1,5,kk) = 0.0e0
    end do
    do kk = 2, nlayers
        ey_fac = ey_bar_t(kk)/ey_bar_t(kk-1);
        M_ext(1,1,kk) = 0.5e0*(ey_fac*(1+nu_bar_t(kk))+1-nu_bar_t(kk-1));
        if (rad(kk) > 0e0) :
            M_ext(1,2,kk) = 0.5e0/rad(kk)**2*(1-nu_bar_t(kk-1)-ey_fac*(1-nu_bar_t(kk)));

        M_ext(2,1,kk) = rad(kk)**2*(1-M_ext(1,1,kk));
        M_ext(2,2,kk) = (1-rad(kk)**2*M_ext(1,2,kk));
        M_ext(2,3,kk) = -rad(kk)**2*M_ext(1,3,kk);
        M_ext(2,5,kk) = -rad(kk)**2*M_ext(1,5,kk);
        M_ext(3,3,kk) = 1.0e0;
        M_ext(4,4,kk) = 1.0e0;
        M_ext(5,5,kk) = 1.0e0;
    end do

    # Total transformation matrix, from Lame parmeters at outside to
    # Lame parameters at inside of each layer
    # Section 7 in the writeup
    # ***
    M_tot(:,:,nlayers) = M_int(:,:,nlayers)
    do kk = nlayers-1, 1, -1
        M_tot(:,:,kk) = matmul(M_int(:,:,kk),matmul(M_ext(:,:,kk+1),M_tot(:,:,kk+1)));
    end do

    # Axial force inner product. Dot-product this with the
    # outermost solution vector, (A,B,eps_z,1.0,eps_z_slip),
    # to obtain the axial force.
    # Section 8 in the writeup
    # ***
    # Axial stiffness products
    ey_bar_z_area      = pi*sum(ey_bar_z(nonslip_layer:nlayers)*(rad(nonslip_layer+1:nlayers+1)**2 - rad(nonslip_layer:nlayers)**2))
    ey_bar_z_area_slip = pi*sum(ey_bar_z(1:nonslip_layer-1)*(rad(2:nonslip_layer)**2 - rad(1:nonslip_layer-1)**2))

    # Axial stiffness inner product, for layers which carry axial force
    rad_row_helper(1,:) = (/rad(nlayers+1)**2, 1e0, 0e0, 0e0, 0e0/)
    v_force_row = 2e0*pi*ey_bar_z(nlayers)*nu_bar_tz(nlayers)*rad_row_helper
    rad_row_helper(1,:) = (/rad(nonslip_layer)**2, 1e0, 0e0, 0e0, 0e0/)
    v_force_row = v_force_row - 2e0*pi*ey_bar_z(nonslip_layer)*nu_bar_tz(nonslip_layer)         * matmul(rad_row_helper,M_tot(:,:,nonslip_layer))
    do kk = (nonslip_layer+1), nlayers
        rad_row_helper(1,:) = (/rad(kk)**2, 1e0, 0e0, 0e0, 0e0/)
        v_force_row = v_force_row + 2e0*pi*(ey_bar_z(kk-1)*nu_bar_tz(kk-1)             - ey_bar_z(kk)*nu_bar_tz(kk))*matmul(rad_row_helper,M_tot(:,:,kk))
    end do
    # Include the effect of axial stiffness
    v_force_row(1,3) = v_force_row(1,3) + ey_bar_z_area

    # Axial stiffness inner product, for layers which DON'T carry force
    if ( nonslip_layer > 1) :
        rad_row_helper(1,:) = (/rad(nonslip_layer)**2, 1e0, 0e0, 0e0, 0e0/)
        v_force_row_slip = 2e0*pi*ey_bar_z(nonslip_layer-1)*nu_bar_tz(nonslip_layer-1)             * matmul(rad_row_helper,M_tot(:,:,nonslip_layer))
        rad_row_helper(1,:) = (/rad(1)**2, 1e0, 0e0, 0e0, 0e0/)
        v_force_row_slip = v_force_row_slip - 2e0*pi*ey_bar_z(1)*nu_bar_tz(1)*matmul(rad_row_helper,M_tot(:,:,1))
        do kk = 2, (nonslip_layer-1)
            rad_row_helper(1,:) = (/rad(kk)**2, 1e0, 0e0, 0e0, 0e0/)
            v_force_row_slip = v_force_row_slip + 2e0*pi*(ey_bar_z(kk-1)*nu_bar_tz(kk-1)                 - ey_bar_z(kk)*nu_bar_tz(kk))*matmul(rad_row_helper,M_tot(:,:,kk))
        end do
        # Include the effect of axial stiffness
        v_force_row_slip(1,5) = v_force_row_slip(1,5) + ey_bar_z_area_slip
    else:
        # If there's no inner slip layer, still need a finite 5th
        # element to ensure no singular matrix
        v_force_row_slip(1,:) = (/ 0e0, 0e0, 0e0, 0e0, 1e0 /)


    # Boundary condition matrix. Multiply this with the
    # outermost solution vector, (A,B,eps_z,1.0,eps_z_slip),
    # to obtain a zero vector.
    # Solved to get the Lame parameters.
    # Section 9 in the writeup
    # ***
    # Outer boundary condition row, zero radial stress
    M_bc(1,:) = (/ (1e0+nu_bar_t(nlayers))*rad(nlayers+1)**2, -1e0+nu_bar_t(nlayers),         nu_bar_zt(nlayers)*rad(nlayers+1)**2, 0e0, 0e0 /)
    # Inner boundary condition row, zero radial stress
    # or zero displacement if rad(1)=0
    if ( nonslip_layer > 1) :
        M_bc(2,:) = (/ (1e0+nu_bar_t(1))*rad(1)**2, -1e0+nu_bar_t(1), 0e0, 0e0, nu_bar_zt(1)*rad(1)**2 /)
    else:
        M_bc(2,:) = (/ (1e0+nu_bar_t(1))*rad(1)**2, -1e0+nu_bar_t(1), nu_bar_zt(1)*rad(1)**2, 0e0, 0e0 /)

    M_bc(2,:) = matmul(M_bc(2,:),M_tot(:,:,1))
    # Axial force boundary condition
    M_bc(3,:) = v_force_row(1,:)
    M_bc(3,4) = M_bc(3,4) - v_force
    # Axial force boundary condition of slip layers
    M_bc(4,:) = v_force_row_slip(1,:)

    # The solution, the outermost Lame parameters A,B
    # and the axial strains of the force-carrying and
    # slip layers eps_z and eps_z_slip.
    # Section 10 in the writeup
    # ***
    M_toinv = M_bc(:,(/1,2,3,5/))
    RHS_vec = -M_bc(:,4)
    call maths_library.linesolv(M_toinv, 4, RHS_vec, A_vec_solution)
    A_vec_solution(5) = A_vec_solution(4)
    A_vec_solution(4) = 1e0

    # Radial/toroidal/vertical stress radial distribution
    # ------
    rradius(:) = 0.0e0
    sigr(:) = 0.0e0
    sigt(:) = 0.0e0
    sigz(:) = 0.0e0
    str_r(:) = 0.0e0
    str_t(:) = 0.0e0
    str_z(:) = 0.0e0
    r_deflect(:) = 0.0e0

    # Radial displacement, stress and strain distributions
    A_vec_layer = A_vec_solution
    do ii = nlayers, 1, -1
        A_layer = A_vec_layer(1)
        B_layer = A_vec_layer(2)

        dradius = (rad(ii+1) - rad(ii)) / dble(n_radial_array - 1 )
        do jj = (ii-1)*n_radial_array + 1, ii*n_radial_array

            rradius(jj) = rad(ii) + dradius*dble(jj - n_radial_array*(ii-1) - 1)

            f_int_A_plot = 0.5e0*f_lin_fac(ii) * (rad(ii+1)**2-rradius(jj)**2)                 + f_rec_fac(ii) * log(rad(ii+1)/(rradius(jj)))
            f_int_B_plot = 0.25e0*f_lin_fac(ii) * (rad(ii+1)**4-rradius(jj)**4)                 + 0.5e0*f_rec_fac(ii) * (rad(ii+1)**2-rradius(jj)**2)
            A_plot       = A_layer - 0.5e0 / ey_bar_t(ii) * f_int_A_plot;
            B_plot       = B_layer + 0.5e0 / ey_bar_t(ii) * f_int_B_plot;

            # Radial displacement
            r_deflect(jj) = A_plot*rradius(jj) + B_plot/rradius(jj)

            # Radial strain
            str_r(jj)  = A_plot - B_plot/rradius(jj)**2
            # Azimuthal strain
            str_t(jj)  = A_plot + B_plot/rradius(jj)**2
            # Axial strain
            if ( ii < nonslip_layer) :
                str_z(jj) = A_vec_solution(5)
            else:
                str_z(jj) = A_vec_solution(3)


            # Radial stress
            sigr(jj) = ey_bar_t(ii)*(str_r(jj) + nu_bar_t(ii)*str_t(jj) + nu_bar_zt(ii)*str_z(jj))
            # Aximuthal stress
            sigt(jj) = ey_bar_t(ii)*(str_t(jj) + nu_bar_t(ii)*str_r(jj) + nu_bar_zt(ii)*str_z(jj))
            # Axial stress
            sigz(jj) = ey_bar_z(ii)*(str_z(jj) + nu_bar_tz(ii)*(str_r(jj) + str_t(jj)))


        end do # layer array loop

        # Move to the outer vector of the next-inner layer
        A_vec_layer = matmul(M_tot(:,:,ii),A_vec_solution)
        A_vec_layer = matmul(M_ext(:,:,ii),A_vec_layer)
    end do # Layer loop
    # ------

    #-# end break


    
    def tf_field_and_force(self):
        """
         Calculate the TF coil field, force and VV quench consideration, and the resistive magnets resistance/volume
        """
            if ( tfcoil_variables.i_tf_sup == 1 ) tfcoil_variables.taucq = (physics_variables.bt * tfcoil_variables.ritfc * physics_variables.rminor * rminor) / (build_variables.r_vv_inboard_out * tfcoil_variables.sigvvall)

    # Outer/inner WP radius removing the ground insulation layer and the insertion gap [m]
    if ( tfcoil_variables.i_tf_sup == 1 ) :
        r_out_wp = r_wp_outer - tfcoil_variables.tinstf - tfcoil_variables.tfinsgap
        r_in_wp = r_wp_inner + tfcoil_variables.tinstf + tfcoil_variables.tfinsgap
    else:
        r_out_wp = r_wp_outer - tfcoil_variables.tinstf
        r_in_wp = r_wp_inner + tfcoil_variables.tinstf


    # Associated WP thickness
    dr_wp = r_out_wp - r_in_wp


    # In plane forces
    # ---
    # Centering force = net inwards radial force per meters per TF coil [N/m]
    tfcoil_variables.cforce = 0.5e0 * tfcoil_variables.bmaxtf*tfcoil_variables.ritfc/tfcoil_variables.n_tf


    # Vertical force per coil [N]
    # ***
    # Rem : this force does not depends on the TF shape or the presence of
    #        sliding joints, the in/outboard vertical tension repartition is
    #-#
    # Ouboard leg WP plasma side radius without ground insulation/insertion gat [m]
    if ( tfcoil_variables.i_tf_sup == 1 ) :
        r_in_outwp = r_tf_outboard_in + tfcoil_variables.casthi + tfcoil_variables.tinstf + tfcoil_variables.tfinsgap
    else:
        r_in_outwp = r_tf_outboard_in + tfcoil_variables.tinstf


    # If the TF coil has no bore it would induce division by 0.
    # In this situation, the bore radius is set to a very small value : 1.0e-9 m
    if ( abs(r_in_wp) < epsilon(r_in_wp) ) r_in_wp = 1.0e-9

    # May the force be with you
    tfcoil_variables.vforce_tot = 0.5e0 * ( physics_variables.bt * physics_variables.rmajor * tfcoil_variables.ritfc ) / ( tfcoil_variables.n_tf * dr_wp**2 )                * ( r_out_wp**2 * log( r_out_wp / r_in_wp )                  + r_in_outwp**2 * log( (r_in_outwp + dr_wp) / r_in_outwp )                  + dr_wp**2         * log( (r_in_outwp + dr_wp) / r_in_wp )                  - dr_wp            * ( r_out_wp + r_in_outwp )                  + 2.0e0 * dr_wp * ( r_out_wp     * log(r_in_wp / r_out_wp)                                    + r_in_outwp * log((r_in_outwp + dr_wp)                                    / r_in_outwp)))

    # Case of a centrepost (physics_variables.itart == 1) with sliding joints (the CP vertical are separated from the leg ones)
    # Rem SK : casing/insulation thickness not subtracted as part of the CP is genuinely connected to the legs..
    if ( physics_variables.itart == 1 .and. tfcoil_variables.i_cp_joints == 1 ) :

        # CP vertical tension [N]
        tfcoil_variables.vforce = 0.25e0 * (physics_variables.bt * physics_variables.rmajor * tfcoil_variables.ritfc) / (tfcoil_variables.n_tf * dr_wp**2)                * ( 2.0e0 * r_out_wp**2 * log(r_out_wp / r_in_wp )                  + 2.0e0 * dr_wp**2 * log( build_variables.r_cp_top / r_in_wp )                  + 3.0e0 * dr_wp**2                  - 2.0e0 * dr_wp * r_out_wp                  + 4.0e0 * dr_wp * r_out_wp *log( r_in_wp / r_out_wp ) )

        # Vertical tension applied on the outer leg [N]
        tfcoil_variables.vforce_outboard = tfcoil_variables.vforce_tot - tfcoil_variables.vforce

        # Inboard vertical tension fraction
        tfcoil_variables.f_vforce_inboard = tfcoil_variables.vforce / vforce_tot

    # Case of TF without joints or with clamped joints vertical tension
    else:

        # Inboard vertical tension [N]
        tfcoil_variables.vforce = tfcoil_variables.f_vforce_inboard * tfcoil_variables.vforce_tot

        # Ouboard vertical tension [N]
        tfcoil_variables.vforce_outboard = tfcoil_variables.vforce * ( ( 1.0e0 / tfcoil_variables.f_vforce_inboard ) - 1.0e0 )

    # ***

    # Total vertical force
    tfcoil_variables.vforce_inboard_tot = tfcoil_variables.vforce * tfcoil_variables.n_tf

    #-# end break


    
    def tf_averaged_turn_geom(self):
        """
        
        """
                if ( t_turn_tf_is_input ) :

            # Turn area [m2]
            a_turn = t_turn_tf**2

            # Current per turn [A]
            cpttf = a_turn * jwptf

        # Turn cable dimension is an input
        elif  ( t_cable_tf_is_input ) :

            # Turn squared dimension [m]
            t_turn_tf = t_cable_tf + 2.0e0 * ( thicndut + thwcndut )

            # Turn area [m2]
            a_turn = t_turn_tf**2

            # Current per turn [A]
            cpttf = a_turn * jwptf

        # Current per turn is an input
        else:
            # Turn area [m2]
            # Allow for additional inter-layer insulation MDK 13/11/18
            # Area of turn including conduit and inter-layer insulation
            a_turn = cpttf / jwptf

            # Dimension of square cross-section of each turn including inter-turn insulation [m]
            t_turn_tf = sqrt(a_turn)



        # Square turn assumption
        t_turn_radial = t_turn_tf
        t_turn_toroidal = t_turn_tf

        # See derivation in the following document
        # k:\power plant physics and technology\process\hts\hts coil module for process.docx
        t_conductor = (-layer_ins + sqrt(layer_ins**2 + 4.0e00*a_turn))/2.e0                     - 2.0e0*thicndut

        # Total number of turns per TF coil (not required to be an integer)
        n_tf_turn = awptf / a_turn

        # Area of inter-turn insulation: single turn [m2]
        insulation_area = a_turn - t_conductor**2

        # ITER like turn structure
        if ( i_tf_sc_mat /= 6 ) :

            # Radius of rounded corners of cable space inside conduit [m]
            rbcndut = thwcndut * 0.75e0

            # Dimension of square cable space inside conduit [m]
            t_cable = t_conductor - 2.0e0*thwcndut

            # Cross-sectional area of cable space per turn
            # taking account of rounded inside corners [m2]
            acstf = t_cable**2 - (4.0e0-pi)*rbcndut**2

            if (acstf <= 0.0e0) :
                if ( t_conductor < 0.0e0 ) :
                    fdiags(1) = acstf
                    fdiags(2) = t_cable
                    call report_error(101)
                else:
                    fdiags(1) = acstf
                    fdiags(2) = t_cable
                    call report_error(102)
                    rbcndut = 0.0e0
                    acstf = t_cable**2



            # Cross-sectional area of conduit jacket per turn [m2]
            acndttf = t_conductor**2 - acstf

        # REBCO turn structure
        elif  ( i_tf_sc_mat == 6 ) :

            # Diameter of circular cable space inside conduit [m]
            t_cable = t_conductor - 2.0e0*thwcndut

            # Cross-sectional area of conduit jacket per turn [m2]
            acndttf = t_conductor**2 - acstf



        ##- end break

    
    
    def peak_tf_with_ripple(self):
        """
         Peak toroidal field on the conductor
 author: P J Knight, CCFE, Culham Science Centre
 tfno : input real : number of TF coils
 wwp1 : input real : width of plasma-facing face of winding pack (m)
 dr_tf_wp : input real : radial thickness of winding pack (m)
 tfin : input real : major radius of centre of winding pack (m)
 bmaxtf : input real : nominal (axisymmetric) peak toroidal field (T)
 bmaxtfrp : output real : peak toroidal field including ripple (T)
 flag : output integer : flag warning of applicability problems
 This subroutine calculates the peak toroidal field at the
 outboard edge of the inboard TF coil winding pack, including
 the effects of ripple.
 <P>For 16, 18 or 20 coils, the calculation uses fitting formulae
 derived by M. Kovari using MAGINT calculations on coil sets based
 on a DEMO1 case.
 <P>For other numbers of coils, the original estimate using a 9%
 increase due to ripple from the axisymmetric calculation is used.
 M. Kovari, Toroidal Field Coils - Maximum Field and Ripple -
 Parametric Calculation, July 2014
        """
            flag = 0

    #  Set fitting coefficients for different numbers of TF coils

    select case (nint(n_tf))

    case (16)
        a(1) =  0.28101e0
        a(2) =  1.8481e0
        a(3) = -0.88159e0
        a(4) =  0.93834e0

    case (18)
        a(1) =  0.29153e0
        a(2) =  1.81600e0
        a(3) = -0.84178e0
        a(4) =  0.90426e0

    case (20)
        a(1) =  0.29853e0
        a(2) =  1.82130e0
        a(3) = -0.85031e0
        a(4) =  0.89808e0

    case default

        #  Original calculation - no fits were performed
        bmaxtfrp = 1.09e0 * bmaxtf
        return

    end select

    #  Maximum winding pack width before adjacent packs touch
    #  (ignoring the external case and ground wall thicknesses)

    wmax = (2.0e0 * tfin + dr_tf_wp) * tan(pi/n_tf)

    #  Dimensionless winding pack width

    tf_fit_t = wwp1/wmax
    if ((tf_fit_t < 0.3e0).or.(tf_fit_t > 1.1e0)) :
        #write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; t = ',t
        flag = 1


    #  Dimensionless winding pack radial thickness

    tf_fit_z = dr_tf_wp/wmax
    if ((tf_fit_z < 0.26e0).or.(tf_fit_z > 0.7e0)) :
        #write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; z = ',z
        flag = 2


    #  Ratio of peak field with ripple to nominal axisymmetric peak field

    tf_fit_y = a(1) + a(2)*exp(-tf_fit_t) + a(3)*tf_fit_z + a(4)*tf_fit_z*tf_fit_t

    bmaxtfrp = tf_fit_y * bmaxtf

    #-# end break


    
    def cpost(self):
        """
        
        """
            rtop = r_cp_top - cas_out_th - gr_ins_th

    # Conductor outer radius at CP mid-plane [m]
    rmid = r_tf_inboard_out - cas_out_th - gr_ins_th

    # Conductor inner radius [m]
    r_tfin_inleg = r_tf_inboard_in + cas_in_th + gr_ins_th
    #-#


    #  Error traps
    # ------------
    if (rtop <= 0.0e0) :
        fdiags(1) = rtop ; call report_error(115)


    if (ztop <= 0.0e0) :
        fdiags(1) = ztop ; call report_error(116)


    if (rmid <= 0.0e0) :
        fdiags(1) = rmid ; call report_error(117)


    if (build_variables.hmax <= 0.0e0) :
        fdiags(1) = build_variables.hmax ; call report_error(118)


    if ((fcool < 0.0e0).or.(fcool > 1.0e0)) :
        fdiags(1) = fcool ; call report_error(119)


    if (rtop < rmid) :
        fdiags(1) = rtop ; fdiags(2) = rmid
        call report_error(120)


    if (build_variables.hmax < ztop) :
        fdiags(1) = build_variables.hmax ; fdiags(2) = ztop
        call report_error(121)

    # ------------


    # Mid-plane area calculations
    # ---------------------------
    # Total number of CP turns
    n_turns_tot = tfcoil_variables.n_tf * n_tf_turn

    # Area of the innner TF central hole [m2]
    a_tfin_hole = pi * r_tfin_inleg**2

    # Mid-plane outer casing cross-section area [m2]
    a_casout = pi * ( ( rmid + gr_ins_th + cas_out_th )**2                     - ( rmid + gr_ins_th )**2 )

    # Mid-plane outter ground insulation thickness [m2]
    a_cp_gr_ins = pi * ( ( rmid + gr_ins_th )**2 - rmid**2 )                 + 2.0e0 * gr_ins_th * ( rmid - r_tfin_inleg ) * tfcoil_variables.n_tf

    # Mid-plane turn layer cross-section area [m2]
    a_cp_ins = pi * ( ( r_tfin_inleg + ins_th )**2 - r_tfin_inleg**2 )  & # Inner layer volume
             + pi * ( rmid**2 - ( rmid - ins_th )**2 )                  & # Outter layer volume
             + 2.0e0 * n_turns_tot * ins_th * ( rmid - r_tfin_inleg - 2.0e0*ins_th ) # inter turn separtion

    # Cooling pipes cross-section per coil [m2]
    a_cp_cool = fcool * ( ( pi*rmid**2 - a_tfin_hole - a_cp_ins ) / tfcoil_variables.n_tf                         - 2.0e0 * gr_ins_th * ( rmid - r_tfin_inleg ) ) # Wedge ground insulation
    # ---------------------------


    #  Trivial solutions
    # ------------------
    if ( abs(fcool) < epsilon(fcool) ) :
        vol_cond_cp = 0.0e0
        respow = 0.0e0
        call report_error(122)
        return


    if ( abs(rmid - rtop) < epsilon(rtop) ) :

        # Exact conductor cross-section
        a_cond_midplane = pi*rmid**2 - a_tfin_hole - tfcoil_variables.n_tf * a_cp_cool - a_cp_ins

        # Volumes and resisitive losses calculations
        vol_cond_cp = 2.0e0 * build_variables.hmaxi * a_cond_midplane
        vol_ins_cp = 2.0e0 * build_variables.hmaxi * a_cp_ins
        vol_gr_ins_cp = 2.0e0 * build_variables.hmaxi * a_cp_gr_ins
        respow = 2.0e0 * build_variables.hmaxi * curr**2 * rho / a_cond_midplane
        vol_case_cp = 2.0e0 * build_variables.hmaxi * a_casout

        return

    # ------------------


    # Find centre of circle (RC,0) defining the taper's arc
    # (r1,z1) is midpoint of line joining (rmid,0) and (rtop,ztop)
    # Rem : The taper arc is defined using the outer radius of the
    #       conductor including turn unsulation
    # -------------------------------------------------------------
    r1 = 0.5e0*(rmid + rtop)
    z1 = 0.5e0*ztop

    x = (r1-rmid)**2 + z1**2
    y = ztop**2 / ( (rtop-rmid)**2 + ztop**2 )

    rc = rmid + sqrt( x / (1.0e0-y) )
    # -------------------------------------------------------------


    #  Find volume of tapered section of centrepost, and the resistive
    #  power losses, by integrating along the centrepost from the midplane
    # --------------------------------------------------------------------
    #  Calculate centrepost radius and cross-sectional areas at each Z
    dz = 0.01e0*ztop

    do ii = 0,100
        z = dble(ii) * dz
        z = min(z,ztop)

        r = rc - sqrt( (rc-rmid)**2 - z*z )

        if (r <= 0.0e0) :
            fdiags(1) = r ; fdiags(2) = rc
            fdiags(3) = rmid ; fdiags(4) = z
            call report_error(123)


        # Insulation cross-sectional area at z
        yy_ins(ii) = pi * ( (r_tfin_inleg + ins_th)**2 - r_tfin_inleg**2 )            + & # Inner layer volume
                     pi * ( r**2 - ( r - ins_th )**2 )                                + & # Outter layer volume
                     2.0e0 * ins_th * (r - r_tfin_inleg - 2.0e0*ins_th) * n_turns_tot     # inter turn layers

        #  Conductor cross-sectional area at z
        yy_cond(ii) = pi*r**2 - a_tfin_hole - tfcoil_variables.n_tf*a_cp_cool - yy_ins(ii)                     - 2.0e0 * tfcoil_variables.n_tf * gr_ins_th * ( r - r_tfin_inleg )   # Wedge ground insulation

        #  Outer ground insulation area at z
        yy_gr_ins(ii) = pi * ( ( r + gr_ins_th )**2 - r**2 )                       + 2.0e0 * tfcoil_variables.n_tf * gr_ins_th * ( r - r_tfin_inleg )

        #  Outer casing Cross-sectional area at z
        yy_casout(ii) = pi * ( ( r + gr_ins_th + cas_out_th )**2                              - ( r + gr_ins_th )**2 )

    end do

    #  Perform integrals using trapezium rule
    sum1 = 0.0e0
    sum2 = 0.0e0
    sum3 = 0.0e0
    sum4 = 0.0e0
    sum5 = 0.0e0
    do ii = 1,99
        sum1 = sum1 + yy_cond(ii)
        sum2 = sum2 + 1.0e0/yy_cond(ii)
        sum3 = sum3 + yy_ins(ii)
        sum4 = sum4 + yy_casout(ii)
        sum5 = sum5 + yy_gr_ins(ii)
    end do

    sum1 = 0.5e0*dz * ( yy_cond(0) + yy_cond(100) + 2.0e0*sum1 )
    sum2 = 0.5e0*dz * ( 1.0e0/yy_cond(0) + 1.0e0/yy_cond(100) + 2.0e0*sum2 )
    sum3 = 0.5e0*dz * ( yy_ins(0) + yy_ins(100) + 2.0e0*sum3 )
    sum4 = 0.5e0*dz * ( yy_casout(0) + yy_casout(100) + 2.0e0*sum4 )
    sum5 = 0.5e0*dz * ( yy_gr_ins(0) + yy_gr_ins(100) + 2.0e0*sum5 )

    # Turn insulation layer cross section at CP top  [m2]
    a_cp_ins = pi * ( (r_tfin_inleg + ins_th)**2 - r_tfin_inleg**2 )           + & # Inner layer volume
               pi * ( rtop**2 - ( rtop - ins_th )**2 )                         + & # Outter layer volume
               2.0e0 * ins_th * (rtop - r_tfin_inleg - 2.0e0*ins_th) * n_turns_tot # turn separtion layers

    # Ground insulation layer cross-section at CP top [m2]
    a_cp_gr_ins = pi * ( ( rtop + gr_ins_th )**2 - rtop**2 )                 + 2.0e0 * gr_ins_th * ( rtop - r_tfin_inleg ) * tfcoil_variables.n_tf

    # Outer casing cross-section area at CP top [m2]
    a_casout = pi * ( ( rmid + gr_ins_th + cas_out_th )**2                     - ( rmid + gr_ins_th )**2 )

    # Centrepost volume (ignoring coolant fraction) [m3]
    vol_cond_cp = 2.0e0 * sum1 &             # Tapered section
                + 2.0e0 * ( build_variables.hmaxi - ztop ) & # Straight section vertical height
                * ( pi*rtop**2 - a_tfin_hole - a_cp_ins - tfcoil_variables.n_tf*a_cp_cool                   - 2.0e0*tfcoil_variables.n_tf * gr_ins_th * ( rtop - r_tfin_inleg ) ) # subtracting ground insulation wedge separation

    # Resistive power losses in taped section (variable radius section) [W]
    res_taped = rho * curr**2 * sum2

    # Centrepost insulator volume [m3]
    vol_ins_cp = 2.0e0 * ( sum3 + ( build_variables.hmaxi - ztop ) * a_cp_ins )

    # Ground insulation volume [m3]
    vol_gr_ins_cp = 2.0e0*( sum5 + ( build_variables.hmaxi - ztop ) * a_cp_gr_ins                           + build_variables.hmaxi * pi * ( r_tfin_inleg**2                                        - (r_tfin_inleg - gr_ins_th)**2 ) )

    # CP casing volume [m3]
    vol_case_cp = 2.0e0*( sum4 + (build_variables.hmaxi - ztop) * a_casout                         + build_variables.hmaxi * pi * ( ( r_tfin_inleg - gr_ins_th )**2                                        - ( r_tfin_inleg - gr_ins_th - cas_in_th )**2 ) )

    # Resistive power losses in cylindrical section (constant radius) [W]
    res_cyl = rho * curr**2 * ( ( build_variables.hmaxi - ztop )                 / ( pi * rtop**2 - a_tfin_hole  - a_cp_ins - tfcoil_variables.n_tf*a_cp_cool                   - 2.0e0*tfcoil_variables.n_tf * gr_ins_th * ( rtop - r_tfin_inleg ) ) ) # ground insulation separation

    # Total CP resistive power [W]
    respow = 2.0e0 * ( res_cyl + res_taped )
    # --------------------------------------------------------------------

    #-# end break

    
    def tf_wp_geom(self):
        """
         Author : S. Kahn, CCFE
 Seting the WP geometry and area for SC magnets
        """
                r_wp_inner = build_variables.r_tf_inboard_in + tfcoil_variables.thkcas

        # Radial position of outer edge of winding pack [m]
        r_wp_outer = r_wp_inner + tfcoil_variables.dr_tf_wp

        # Radius of geometrical centre of winding pack [m]
        r_wp_centre = 0.5e0 * ( r_wp_inner + r_wp_outer )

        # TF toroidal thickness at the WP inner radius [m]
        t_tf_at_wp = 2.0e0 * r_wp_inner * tan_theta_coil

        # Minimal toroidal thickness of winding pack [m]
        t_wp_toroidal = t_tf_at_wp - 2.0e0 * tfcoil_variables.casths

        # Rectangular WP
        # --------------
        if ( i_tf_wp_geom == 0 ) :

            # Outer WP layer toroidal thickness [m]
            tfcoil_variables.wwp1 = t_wp_toroidal

            # Averaged toroidal thickness of of winding pack [m]
            t_wp_toroidal_av = t_wp_toroidal

            # Total cross-sectional area of winding pack [m2]
            awpc = tfcoil_variables.dr_tf_wp * t_wp_toroidal

            # WP cross-section without insertion gap and ground insulation [m2]
            awptf = ( tfcoil_variables.dr_tf_wp - 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.tfinsgap ) )                   * ( t_wp_toroidal - 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.tfinsgap ) )

            # Cross-section area of the WP ground insulation [m2]
            a_ground_ins = ( tfcoil_variables.dr_tf_wp - 2.0e0 * tfcoil_variables.tfinsgap )                          * ( t_wp_toroidal - 2.0e0 *  tfcoil_variables.tfinsgap ) - awptf


        # Double rectangular WP
        # ---------------------
        elif  ( i_tf_wp_geom == 1 ) :

            # Thickness of winding pack section at R > r_wp_centre [m]
            tfcoil_variables.wwp1 = 2.0e0 * ( r_wp_centre * tan_theta_coil - tfcoil_variables.casths )

            # Thickness of winding pack section at R < r_wp_centre [m]
            tfcoil_variables.wwp2 = 2.0e0 * ( r_wp_inner * tan_theta_coil - tfcoil_variables.casths )

            # Averaged toroidal thickness of of winding pack [m]
            t_wp_toroidal_av = 0.5e0 * ( tfcoil_variables.wwp1 + tfcoil_variables.wwp2 )

            # Total cross-sectional area of winding pack [m2]
            # Including ground insulation and insertion gap
            awpc = tfcoil_variables.dr_tf_wp * t_wp_toroidal_av

            # WP cross-section without insertion gap and ground insulation [m2]
            awptf = 0.5e0 * ( tfcoil_variables.dr_tf_wp - 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.tfinsgap ) )                           * ( tfcoil_variables.wwp1 + tfcoil_variables.wwp2 - 4.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.tfinsgap ) )

            # Cross-section area of the WP ground insulation [m2]
            a_ground_ins = 0.5e0 * ( tfcoil_variables.dr_tf_wp - 2.0e0 * tfcoil_variables.tfinsgap )                                  * ( tfcoil_variables.wwp1 + tfcoil_variables.wwp2 - 4.0e0 * tfcoil_variables.tfinsgap ) - awptf


        # Trapezoidal WP
        # --------------
        else:

            # Thickness of winding pack section at r_wp_outer [m]
            tfcoil_variables.wwp1 = 2.0e0 * ( r_wp_outer * tan_theta_coil - tfcoil_variables.casths )

            # Thickness of winding pack section at r_wp_inner [m]
            tfcoil_variables.wwp2 = 2.0e0 * ( r_wp_inner * tan_theta_coil - tfcoil_variables.casths )

            # Averaged toroidal thickness of of winding pack [m]
            t_wp_toroidal_av = 0.5e0 * ( tfcoil_variables.wwp1 + tfcoil_variables.wwp2 )

            # Total cross-sectional area of winding pack [m2]
            # Including ground insulation and insertion gap
            awpc = tfcoil_variables.dr_tf_wp * ( tfcoil_variables.wwp2 + 0.5e0 * ( tfcoil_variables.wwp1 - tfcoil_variables.wwp2 ) )

            # WP cross-section without insertion gap and ground insulation [m2]
            awptf = ( tfcoil_variables.dr_tf_wp - 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.tfinsgap ) )                   * ( tfcoil_variables.wwp2 - 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.tfinsgap ) + 0.5e0 * ( tfcoil_variables.wwp1 - tfcoil_variables.wwp2 ) )

            # Cross-section area of the WP ground insulation [m2]
            a_ground_ins = ( tfcoil_variables.dr_tf_wp - 2.0e0 * tfcoil_variables.tfinsgap )                          * ( tfcoil_variables.wwp2 - 2.0e0 * tfcoil_variables.tfinsgap  + 0.5e0 * ( tfcoil_variables.wwp1 - tfcoil_variables.wwp2 ) ) - awptf


        # --------------


        # Negative WP area error reporting
        if ( awptf <= 0.0e0 .or. awpc <= 0.0e0 ) :
            fdiags(1) = awptf
            fdiags(2) = awpc
            call report_error(99)


        #-# end break

    
    
    def initialise_cables(self):
        """
        
        """
            copper%rrr = rebco_variables.copper_rrr
    copper%density = 8960.0e0
    hastelloy%density = 8890.0e0
    # Solder: 60EN ie 60%Sn + 40%Pb solder (approx the same as eutectic 63/37)
    solder%density = 8400.0e0
    jacket%density = 8000.0e0       # 304 stainless

    
    def outtf(self):
        """
         Writes superconducting TF coil output to file
 author: P J Knight, CCFE, Culham Science Centre
 outfile : input integer : output file unit
 peaktfflag : input integer : warning flag from peak TF calculation
 This routine writes the superconducting TF coil results
 to the output file.
 PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
        """
            character(len=1) :: intstring

    # ###############################################


    # General coil parameters
    call osubhd(outfile,'TF design')
    call ovarin(outfile,'Conductor technology','(i_tf_sup)',i_tf_sup)
    select case (tfcoil_variables.i_tf_sup)
        case (0)
            call ocmmnt(outfile,'  -> Resitive coil : Water cooled copper (GLIDCOP AL-15)')
        case (1)
            call ocmmnt(outfile,'  -> Superconducting coil (SC)')
        case (2)
            call ocmmnt(outfile,'  -> Reisitive coil : Helium cooled aluminium')
    end select

    # SC material scaling
    if ( tfcoil_variables.i_tf_sup == 1 ) :
        call ovarin(outfile,'Superconductor material','(i_tf_sc_mat)',i_tf_sc_mat)
        select case (tfcoil_variables.i_tf_sc_mat)
            case (1)
                call ocmmnt(outfile,'  -> ITER Nb3Sn critical surface model')
            case (2)
                call ocmmnt(outfile,'  -> Bi-2212 high temperature superconductor')
            case (3)
                call ocmmnt(outfile,'  -> NbTi')
            case (4)
                call ocmmnt(outfile,                 '  -> ITER Nb3Sn critical surface model, user-defined parameters')
            case (5)
                call ocmmnt(outfile, '  -> WST Nb3Sn')
            case (6)
                call ocmmnt(outfile,                     '  -> High temperature superconductor: REBCO HTS tape in CroCo strand')
            case (7)
                call ocmmnt(outfile,                     '  ->  Durham Ginzburg-Landau critical surface model for Nb-Ti')
            case (8)
                call ocmmnt(outfile,                     '  ->  Durham Ginzburg-Landau critical surface model for REBCO')
            case (9)
                call ocmmnt(outfile,                     '  ->  Hazelton experimental data + Zhai conceptual model for REBCO')
        end select


    # Joints strategy
    call ovarin(outfile,'Presence of TF demountable joints','(itart)',itart)
    if ( physics_variables.itart == 1 ) :
        call ocmmnt(outfile,'  -> TF coil made of a Centerpost (CP) and outer legs')
        call ocmmnt(outfile,'     interfaced with demountable joints')
    else:
        call ocmmnt(outfile,'  -> Coils without demountable joints')


    # Centring forces support strategy
    call ovarin(outfile,'TF inboard leg support strategy','(i_tf_bucking)', tfcoil_variables.i_tf_bucking)
    select case ( tfcoil_variables.i_tf_bucking )
        case (0)
            call ocmmnt(outfile,'  -> No support structure')
        case (1)
            if ( tfcoil_variables.i_tf_sup == 1 ) :
                call ocmmnt(outfile,'  -> Steel casing')
            elif  ( abs( tfcoil_variables.eyoung_res_tf_buck - 205.0e9 ) < epsilon(tfcoil_variables.eyoung_res_tf_buck) ) :
                call ocmmnt(outfile,'  -> Steel bucking cylinder')
            else:
                call ocmmnt(outfile,'  -> Bucking cylinder')

        case (2,3)
            call ocmmnt(outfile,'  -> TF in contact with CS (bucked and weged design)')
    end select

    # TF coil geometry
    call osubhd(outfile,'TF coil Geometry :')
    call ovarin(outfile,'Number of TF coils','(n_tf)', int(tfcoil_variables.n_tf))
    call ovarre(outfile,'Inboard leg centre radius (m)','(r_tf_inboard_mid)',r_tf_inboard_mid, 'OP ')
    call ovarre(outfile,'Outboard leg centre radius (m)','(r_tf_outboard_mid)',r_tf_outboard_mid, 'OP ')
    call ovarre(outfile,'Total inboard leg radial thickness (m)','(tfcth)',tfcth)
    call ovarre(outfile,'Total outboard leg radial thickness (m)','(tfthko)',tfthko)
    call ovarre(outfile,'Outboard leg toroidal thickness (m)','(tftort)',tftort, 'OP ')
    call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax, 'OP ')
    if ( physics_variables.itart == 1 ) :
        call ovarre(outfile,'Mean coil circumference (inboard leg not included) (m)','(tfleng)',tfleng, 'OP ')
        call ovarre(outfile,'Length of the inboard segment (m)','(cplen)',cplen, 'OP ')
    else:
        call ovarre(outfile,'Mean coil circumference (including inboard leg length) (m)','(tfleng)',tfleng, 'OP ')


    # Vertical shape
    call ovarin(outfile,'Vertical TF shape','(i_tf_shape)',i_tf_shape)
    if ( tfcoil_variables.i_tf_shape == 1 ) :
        call oblnkl(outfile)
        call ocmmnt(outfile,'D-shape coil, inner surface shape approximated by')
        call ocmmnt(outfile,'by a straight segment and elliptical arcs between the following points:')
        call oblnkl(outfile)
    elif  ( tfcoil_variables.i_tf_shape == 2 ) :
        call oblnkl(outfile)
        call ocmmnt(outfile,'Picture frame coil, inner surface approximated by')
        call ocmmnt(outfile,'by a straight segment between the following points:')
        call oblnkl(outfile)


    write(outfile,10)
    10  format(t2,'point',t16,'x(m)',t31,'y(m)')
    do ii = 1,5
        write(outfile,20) ii,xarc(ii),yarc(ii)
        intstring = int2char(ii)
        call ovarre(mfile,'TF coil arc point '//intstring//' R (m)', '(tfcoil_variables.xarc('//intstring//'))',xarc(ii))
        call ovarre(mfile,'TF coil arc point '//intstring//' Z (m)', '(tfcoil_variables.yarc('//intstring//'))',yarc(ii))
    end do
    20 format(i4,t10,f10.3,t25,f10.3)

    # CP tapering geometry
    if ( physics_variables.itart == 1 .and. tfcoil_variables.i_tf_sup /= 1 ) :
        call osubhd(outfile,'Tapered Centrepost TF coil Dimensions:')
        call ovarre(outfile,'TF coil centrepost outer radius at midplane (m)','(r_tf_inboard_out)',r_tf_inboard_out)
        call ovarre(outfile,'TF coil centrepost outer radius at its top (m)','(r_cp_top)',r_cp_top)
        call ovarre(outfile,'Top/miplane TF CP radius ratio (-)','(f_r_cp)', build_variables.f_r_cp)
        call ovarre(outfile,'Distance from the midplane to the top of the tapered section (m)','(h_cp_top)',h_cp_top)
        call ovarre(outfile,'Distance from the midplane to the top of the centrepost (m)','(build_variables.hmax + build_variables.tfthko)',hmax + build_variables.tfthko)


    # Turn/WP gemoetry
    if ( tfcoil_variables.i_tf_sup == 1 ) :

        # Total material fraction
        call osubhd(outfile,'Global material area/fractions:')
        call ovarre(outfile,'TF cross-section (total) (m2)','(tfareain)', tfcoil_variables.tfareain)
        call ovarre(outfile,'Total steel cross-section (m2)','(a_tf_steel*tfcoil_variables.n_tf)',a_tf_steel*tfcoil_variables.n_tf)
        call ovarre(outfile,'Total steel TF fraction','(f_tf_steel)',f_tf_steel)
        call ovarre(outfile,'Total Insulation cross-section (total) (m2)','(a_tf_ins*tfcoil_variables.n_tf)',a_tf_ins*tfcoil_variables.n_tf)
        call ovarre(outfile,'Total Insulation fraction','(f_tf_ins)',f_tf_ins)

        # External casing
        call osubhd(outfile,'External steel Case Information :')
        call ovarre(outfile,'Casing cross section area (per leg) (m2)','(acasetf)',acasetf)
        call ovarre(outfile,'Inboard leg case plasma side wall thickness (m)','(casthi)',casthi)
        call ovarre(outfile,'Inboard leg case inboard "nose" thickness (m)','(thkcas)',thkcas)
        call ovarre(outfile,'Inboard leg case sidewall thickness at its narrowest point (m)','(casths)',casths)
        call ovarre(outfile,'External case mass per coil (kg)','(whtcas)',whtcas, 'OP ')

        # Winding pack structure
        call osubhd(outfile,'TF winding pack (WP) geometry:')
        call ovarre(outfile,'WP cross section area with insulation and insertion (per coil) (m2)','(awpc)',awpc)
        call ovarre(outfile,'WP cross section area (per coil) (m2)','(aswp)', awptf)
        call ovarre(outfile,'Winding pack radial thickness (m)','(dr_tf_wp)',dr_tf_wp, 'OP ')
        if (  tfcoil_variables.i_tf_turns_integer == 1 ) :
            call ovarre(outfile, 'Winding pack toroidal width (m)', '(wwp1)', tfcoil_variables.wwp1, 'OP ')
        else:
            call ovarre(outfile,'Winding pack toroidal width 1 (m)','(wwp1)',wwp1, 'OP ')
            call ovarre(outfile,'Winding pack toroidal width 2 (m)','(wwp2)',wwp2, 'OP ')

        call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)',tinstf)
        call ovarre(outfile,'Winding pack insertion gap (m)','(tfinsgap)',tfinsgap)

        # WP material fraction
        call osubhd(outfile,'TF winding pack (WP) material area/fractions:')
        call ovarre(outfile,'Steel WP cross-section (total) (m2)','(tfcoil_variables.aswp*tfcoil_variables.n_tf)',aswp*tfcoil_variables.n_tf)
        call ovarre(outfile,'Steel WP fraction','(tfcoil_variables.aswp/awpc)',aswp/awpc)
        call ovarre(outfile,'Insulation WP fraction','(tfcoil_variables.aiwp/awpc)',aiwp/awpc)
        call ovarre(outfile,'Cable WP fraction','((awpc-tfcoil_variables.aswp-tfcoil_variables.aiwp)/awpc)',(awpc-tfcoil_variables.aswp-tfcoil_variables.aiwp)/awpc)

        # Number of turns
        call osubhd(outfile,'WP turn information:')
        call ovarin(outfile,'Turn parametrisation', '(i_tf_turns_integer)', tfcoil_variables.i_tf_turns_integer)
        if ( tfcoil_variables.i_tf_turns_integer == 0 ) :
            call ocmmnt(outfile,'  Non-integer number of turns')
        else:
            call ocmmnt(outfile,'  Integer number of turns')

        call ovarre(outfile,'Number of turns per TF coil','(tfcoil_variables.n_tf_turn)',n_tf_turn, 'OP ')
        if ( tfcoil_variables.i_tf_turns_integer == 1 ) :
            call ovarin(outfile, 'Number of TF pancakes', '(n_pancake)', tfcoil_variables.n_pancake)
            call ovarin(outfile, 'Number of TF layers', '(n_layer)', tfcoil_variables.n_layer)

        call oblnkl(outfile)

        if ( tfcoil_variables.i_tf_turns_integer == 1 ) :
            call ovarre(outfile, 'Radial width of turn (m)', '(t_turn_radial)', t_turn_radial)
            call ovarre(outfile, 'Toroidal width of turn (m)', '(t_turn_toroidal)', t_turn_toroidal)
            call ovarre(outfile, 'Radial width of conductor (m)', '(elonductor_radial)', tfcoil_variables.t_conductor_radial, 'OP ')
            call ovarre(outfile, 'Toroidal width of conductor (m)', '(tfcoil_variables.t_conductor_toroidal)', tfcoil_variables.t_conductor_toroidal, 'OP ')
            call ovarre(outfile, 'Radial width of cable space', '(t_cable_radial)', t_cable_radial)
            call ovarre(outfile, 'Toroidal width of cable space', '(t_cable_toroidal)', t_cable_toroidal)
       else:
            call ovarre(outfile,'Width of turn including inter-turn insulation (m)','(t_turn_tf)',t_turn_tf, 'OP ')
            call ovarre(outfile,'Width of conductor (square) (m)','(t_conductor)',t_conductor, 'OP ')
            call ovarre(outfile,'Width of space inside conductor (m)','(t_cable)',t_cable, 'OP ')

        call ovarre(outfile,'Steel conduit thickness (m)','(thwcndut)',thwcndut)
        call ovarre(outfile,'Inter-turn insulation thickness (m)','(thicndut)',thicndut)

        select case (tfcoil_variables.i_tf_sc_mat)
        case (1,2,3,4,5,7,8,9)
            call osubhd(outfile,'Conductor information:')
            call ovarre(outfile,'Diameter of central helium channel in cable','(dhecoil)',dhecoil)
            call ocmmnt(outfile,'Fractions by area')
            call ovarre(outfile, 'internal area of the cable space', '(acstf)', tfcoil_variables.acstf)
            call ovarre(outfile,'Coolant fraction in conductor excluding central channel','(vftf)',vftf)
            call ovarre(outfile,'Copper fraction of conductor','(fcutfsu)',fcutfsu)
            call ovarre(outfile,'Superconductor fraction of conductor','(1-tfcoil_variables.fcutfsu)',1-tfcoil_variables.fcutfsu)
            # TODO
            #call ovarre(outfile,'Conductor fraction of winding pack','(tfcoil_variables.acond/ap)',acond/ap, 'OP ')
            #call ovarre(outfile,'Conduit fraction of winding pack','(tfcoil_variables.n_tf_turn*tfcoil_variables.acndttf/ap)',n_tf_turn*tfcoil_variables.acndttf/ap, 'OP ')
            #call ovarre(outfile,'Insulator fraction of winding pack','(tfcoil_variables.aiwp/ap)',aiwp/ap, 'OP ')
            #call ovarre(outfile,'Helium area fraction of winding pack excluding central channel','(tfcoil_variables.avwp/ap)',avwp/ap, 'OP ')
            #call ovarre(outfile,'Central helium channel area as fraction of winding pack','(tfcoil_variables.awphec/ap)',awphec/ap, 'OP ')
            ap = tfcoil_variables.acond + tfcoil_variables.n_tf_turn*tfcoil_variables.acndttf + tfcoil_variables.aiwp + tfcoil_variables.avwp + tfcoil_variables.awphec
            call ovarrf(outfile,'Check total area fractions in winding pack = 1','',             (tfcoil_variables.acond + tfcoil_variables.n_tf_turn*tfcoil_variables.acndttf + tfcoil_variables.aiwp + tfcoil_variables.avwp + tfcoil_variables.awphec)/ap)
            call ovarrf(outfile,'minimum TF conductor temperature margin  (K)','(tmargmin_tf)',tmargmin_tf)
            call ovarrf(outfile,'TF conductor temperature margin (K)','(tmargtf)',tmargtf)

            call ovarin(outfile,'Elastic properties behavior', '(i_tf_cond_eyoung_axial)', tfcoil_variables.i_tf_cond_eyoung_axial)
            if ( tfcoil_variables.i_tf_cond_eyoung_axial == 0 ) :
              call ocmmnt(outfile,'  Conductor stiffness neglected')
            elif  ( tfcoil_variables.i_tf_cond_eyoung_axial == 1 ) :
              call ocmmnt(outfile,'  Conductor stiffness is user-input')
            elif  ( tfcoil_variables.i_tf_cond_eyoung_axial == 2 ) :
              call ocmmnt(outfile,'  Conductor stiffness is set by material-specific default')

            call ovarre(outfile, 'Conductor axial Young''s modulus', '(eyoung_cond_axial)', tfcoil_variables.eyoung_cond_axial)
            call ovarre(outfile, 'Conductor transverse Young''s modulus', '(eyoung_cond_trans)', tfcoil_variables.eyoung_cond_trans)
        end select
    else:

        # External casing
        call osubhd(outfile,'Bucking cylinder information:')
        call ovarre(outfile,'Casing cross section area (per leg) (m2)','(acasetf)',acasetf)
        call ovarre(outfile,'Inboard leg case plasma side wall thickness (m)','(casthi)',casthi)
        call ovarre(outfile,'Inboard leg bucking cylinder thickness (m)','(thkcas)',thkcas)

        # Conductor layer geometry
        call osubhd(outfile,'Inboard TFC conductor sector geometry:')
        call ovarre(outfile,'Inboard TFC conductor sector area with gr insulation (per leg) (m2)'             ,'(awpc))',awpc)
        call ovarre(outfile,'Inboard TFC conductor sector area, NO ground & gap (per leg) (m2)','(awptf)',awptf )
        call ovarre(outfile,'Inboard conductor sector radial thickness (m)','(dr_tf_wp)',dr_tf_wp )
        if ( physics_variables.itart == 1 ) :
            call ovarre(outfile,'Central collumn top conductor sector radial thickness (m)',                 '(tfcoil_variables.dr_tf_wp_top)',dr_tf_wp_top )

        call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)', tfcoil_variables.tinstf )

        # Turn info
        call osubhd(outfile,'Coil turn information:')
        call ovarre(outfile,'Number of turns per TF leg','(tfcoil_variables.n_tf_turn)',n_tf_turn)
        call ovarre(outfile,'Turn insulation thickness','(thicndut)',thicndut)
        call ovarre(outfile,'Mid-plane CP cooling fraction','(fcoolcp)',fcoolcp)
        call ovarre(outfile,'Outboard leg current per turn (A)','(cpttf)',cpttf)
        call ovarre(outfile,'Inboard leg conductor volume (m3)','(vol_cond_cp)',vol_cond_cp)
        call ovarre(outfile,'Outboard leg volume per coil (m3)','(voltfleg)',voltfleg)


    # Coil masses
    call osubhd(outfile,'TF coil mass:')
    call ovarre(outfile,'Superconductor mass per coil (kg)','(tfcoil_variables.whtconsc)',whtconsc, 'OP ')
    call ovarre(outfile,'Copper mass per coil (kg)','(tfcoil_variables.whtconcu)',whtconcu, 'OP ')
    call ovarre(outfile,'Steel conduit mass per coil (kg)','(tfcoil_variables.whtconsh)',whtconsh, 'OP ')
    call ovarre(outfile,'Conduit insulation mass per coil (kg)','(tfcoil_variables.whtconin)',whtconin, 'OP ')
    if ( tfcoil_variables.i_tf_sup == 1 ) :
        call ovarre(outfile,'Total conduit mass per coil (kg)','(whtcon)',whtcon, 'OP ')

    if ( physics_variables.itart ==  1 ) :
        call ovarre(outfile,'Mass of inboard legs (kg)','(whtcp)',whtcp, 'OP ')
        call ovarre(outfile,'Mass of outboard legs (kg)','(whttflgs)',whttflgs, 'OP ')

    call ovarre(outfile,'Mass of each TF coil (kg)','(tfcoil_variables.whttf/tfcoil_variables.n_tf)',whttf/tfcoil_variables.n_tf, 'OP ')
    call ovarre(outfile,'Total TF coil mass (kg)','(whttf)',whttf, 'OP ')

    # TF current and field
    call osubhd(outfile,'Maximum B field and currents:')
    call ovarre(outfile,'Nominal peak field assuming toroidal symmetry (T)','(bmaxtf)',bmaxtf, 'OP ')
    call ovarre(outfile,'Total current in all TF coils (MA)','(tfcoil_variables.ritfc/1.D6)',1.0e-6*tfcoil_variables.ritfc, 'OP ')
    call ovarre(outfile,'TF coil current (summed over all coils) (A)','(ritfc)',ritfc)
    if ( tfcoil_variables.i_tf_sup == 1 ) :
        call ovarre(outfile,'Actual peak field at discrete conductor (T)','(tfcoil_variables.bmaxtfrp)',bmaxtfrp, 'OP ')
        call ovarre(outfile,'Winding pack current density (A/m2)','(jwptf)',jwptf, 'OP ')

    call ovarre(outfile,'Inboard leg mid-plane conductor current density (A/m2)','(oacdcp)',oacdcp)
    if ( physics_variables.itart == 1 ) :
        call ovarre(outfile,'Outboard leg conductor current density (A/m2)','(cdtfleg)',cdtfleg)

    call ovarre(outfile,'Total stored energy in TF coils (GJ)','(estotftgj)',estotftgj, 'OP ')


    # TF forces
    call osubhd(outfile,'TF Forces:')
    call ovarre(outfile,'Inboard vertical tension per coil (N)','(vforce)',vforce, 'OP ')
    call ovarre(outfile,'Outboard vertical tension per coil (N)','(vforce_outboard)', tfcoil_variables.vforce_outboard, 'OP ')
    call ovarre(outfile,'inboard vertical tension fraction (-)','(f_vforce_inboard)', tfcoil_variables.f_vforce_inboard, 'OP ')
    call ovarre(outfile,'Centring force per coil (N/m)','(cforce)',cforce, 'OP ')

    # Resistive coil parameters
    if ( tfcoil_variables.i_tf_sup /= 1 ) :
        call osubhd(outfile,'Resitive loss parameters:')
        if ( tfcoil_variables.i_tf_sup == 0 ) :
            call ocmmnt(outfile,'Resistive Material : GLIDCOP AL-15 - Dispersion Strengthened Copper')
        elif  ( tfcoil_variables.i_tf_sup == 2 ) :
            call ocmmnt(outfile,'Resistive Material : Pure Aluminium (99.999+ %)')


        if ( physics_variables.itart == 1 ) :
            call ovarre(outfile,'CP resistivity (ohm.m)','(rhocp)',rhocp)
            call ovarre(outfile,'Leg resistivity (ohm.m)','(rhotfleg)',rhotfleg)
            call ovarre(outfile,'CP resistive power loss (W)','(prescp)',prescp)
            call ovarre(outfile,'Leg resitive power loss, (per leg) (W)','(presleg)',presleg)
            call ovarre(outfile,'joints resistive power loss (W)','(pres_joints)',pres_joints)
            call ovarre(outfile,'Outboard leg resistance per coil (ohm)','(tflegres)',tflegres)
            call ovarre(outfile,'Average CP temperature (K)','(tcpav)',tcpav)
            call ovarre(outfile,'Average leg temperature (K)','(tlegav)',tlegav)

        else:
            call ovarre(outfile,'TF resistivity (ohm.m)','(prescp)',rhocp)
            call ovarre(outfile,'TF coil resistive power less (total) (ohm.m)','(prescp)',prescp)
            call ovarre(outfile,'Average coil temperature (K)','(tcpav)',tcpav)



    # Ripple calculations
    call osubhd(outfile,'Ripple information:')
    if ( tfcoil_variables.i_tf_shape == 1 ) :

        if (peaktfflag == 1) :
            call report_error(144)
        elif  (peaktfflag == 2) :
            call report_error(145)
        else:
            continue


        call ovarre(outfile,'Max allowed tfcoil_variables.ripple amplitude at plasma outboard midplane (%)','(ripmax)',ripmax)
        call ovarre(outfile,'Ripple amplitude at plasma outboard midplane (%)','(ripple)',ripple, 'OP ')
    else:
        call ovarre(outfile,'Max allowed tfcoil_variables.ripple amplitude at plasma (%)','(ripmax)',ripmax)
        call ovarre(outfile,'Ripple at plasma edge (%)','(ripple)',ripple)
        call ocmmnt(outfile,'  Ripple calculation to be re-defined for picure frame coils')


    # Quench information
    if ( tfcoil_variables.i_tf_sup == 1 ) :
        call osubhd(outfile,'Quench information :')
        call ovarre(outfile,'Allowable stress in vacuum vessel (VV) due to quench (Pa)','(sigvvall)',sigvvall)
        call ovarre(outfile,'Minimum allowed quench time due to stress in VV (s)','(taucq)',taucq, 'OP ')
        call ovarre(outfile,'Actual quench time (or time constant) (s)','(tdmptf)',tdmptf)
        call ovarre(outfile,'Maximum allowed voltage during quench due to insulation (kV)', '(vdalw)', tfcoil_variables.vdalw)
        call ovarre(outfile,'Actual quench voltage (kV)','(vtfskv)',vtfskv, 'OP ')

        select case (tfcoil_variables.i_tf_sc_mat)
        case (1,2,3,4,5)
            call ovarre(outfile,'Maximum allowed temp rise during a quench (K)','(tmaxpro)', tfcoil_variables.tmaxpro)
        case(6)
            call ocmmnt(outfile,'CroCo cable with jacket: ')

            if (any(numerics.icc == 75) ) :
                call ovarre(outfile,'Maximum permitted TF coil current / copper area (A/m2)',                 '(rebco_variables.copperA_m2_max)', rebco_variables.copperA_m2_max)

            call ovarre(outfile,'Actual TF coil current / copper area (A/m2)',                                 '(copperA_m2)', rebco_variables.copperA_m2)

        end select


    # TF coil radial build
    call osubhd(outfile,'Radial build of TF coil centre-line :')
    write(outfile,5)
    5   format(t43,'Thickness (m)',t60,'Outer radius (m)')

    radius = build_variables.r_tf_inboard_in
    call obuild(outfile,'Innermost edge of TF coil',radius,radius)

    # Radial build for SC TF coils
    if ( tfcoil_variables.i_tf_sup == 1 ) :

        radius = radius + tfcoil_variables.thkcas
        call obuild(outfile,'Coil case ("nose")',thkcas,radius,'(thkcas)')

        radius = radius + tfcoil_variables.tfinsgap
        call obuild(outfile,'Insertion gap for winding pack',tfinsgap,radius,'(tfinsgap)')

        radius = radius + tfcoil_variables.tinstf
        call obuild(outfile,'Winding pack ground insulation',tinstf,radius,'(tinstf)')

        radius = radius + 0.5e0*tfcoil_variables.dr_tf_wp - tfcoil_variables.tinstf - tfcoil_variables.tfinsgap
        call obuild(outfile,'Winding - first half', tfcoil_variables.dr_tf_wp/2e0-tfcoil_variables.tinstf-tfcoil_variables.tfinsgap,             radius, '(tfcoil_variables.dr_tf_wp/2-tfcoil_variables.tinstf-tfcoil_variables.tfinsgap)')

        radius = radius + 0.5e0*tfcoil_variables.dr_tf_wp - tfcoil_variables.tinstf - tfcoil_variables.tfinsgap
        call obuild(outfile,'Winding - second half',dr_tf_wp/2e0-tfcoil_variables.tinstf-tfcoil_variables.tfinsgap,             radius,'(tfcoil_variables.dr_tf_wp/2-tfcoil_variables.tinstf-tfcoil_variables.tfinsgap)')

        radius = radius + tfcoil_variables.tinstf
        call obuild(outfile,'Winding pack insulation',tinstf,radius,'(tinstf)')

        radius = radius + tfcoil_variables.tfinsgap
        call obuild(outfile,'Insertion gap for winding pack',tfinsgap,radius,'(tfinsgap)')

        radius = radius + tfcoil_variables.casthi
        call obuild(outfile,'Plasma side case min radius',casthi,radius,'(casthi)')

        radius = radius / cos(pi/tfcoil_variables.n_tf)
        call obuild(outfile,'Plasma side case max radius', build_variables.r_tf_inboard_out, radius,'(r_tf_inboard_out)')


    # Radial build for restive coil
    else:
        radius = radius + tfcoil_variables.thkcas
        call obuild(outfile,'Coil bucking cylindre',thkcas,radius,'(thkcas)')

        radius = radius + tfcoil_variables.tinstf
        call obuild(outfile,'Conductor ground insulation',tinstf,radius,'(tinstf)')

        radius = radius + 0.5e0*tfcoil_variables.dr_tf_wp - tfcoil_variables.tinstf
        call obuild(outfile,'Conductor - first half', tfcoil_variables.dr_tf_wp/2e0 - tfcoil_variables.tinstf, radius,             '(tfcoil_variables.dr_tf_wp/2-tfcoil_variables.tinstf)')

        radius = radius + 0.5e0*tfcoil_variables.dr_tf_wp - tfcoil_variables.tinstf
        call obuild(outfile,'Conductor - second half',dr_tf_wp/2e0-tfcoil_variables.tinstf,             radius,'(tfcoil_variables.dr_tf_wp/2-tfcoil_variables.tinstf)')

        radius = radius + tfcoil_variables.tinstf
        call obuild(outfile,'Conductor ground insulation',tinstf,radius,'(tinstf)')

        radius = radius + tfcoil_variables.casthi
        call obuild(outfile,'Plasma side TF coil support',casthi,radius,'(casthi)')



    # Radial build consistency check
    if ( abs( radius - build_variables.r_tf_inboard_in - build_variables.tfcth ) < 10.0e0 * epsilon(radius) ) :
        call ocmmnt(outfile,'TF coil dimensions are consistent')
    else:
        call ocmmnt(outfile,'ERROR: TF coil dimensions are NOT consistent:')
        call ovarre(outfile,'Radius of plasma-facing side of inner leg SHOULD BE [m]','',r_tf_inboard_in + build_variables.tfcth)
        call ovarre(outfile,'Inboard TF coil radial thickness [m]','(tfcth)',tfcth)
        call oblnkl(outfile)


    # Top section TF coil radial build (physics_variables.itart = 1 only)
    if ( physics_variables.itart == 1 .and. tfcoil_variables.i_tf_sup /= 1 ) :

        call osubhd(outfile,'Radial build of TF coil at central collumn top :')
        write(outfile,5)

        # Restart the radial build at bucking cylindre inner radius
        radius = build_variables.r_tf_inboard_in
        call obuild(outfile,'Innermost edge of TF coil',radius,radius)

        radius = radius + tfcoil_variables.thkcas
        call obuild(outfile,'Coil bucking cylindre',thkcas,radius,'(thkcas)')

        radius = radius + tfcoil_variables.tinstf
        call obuild(outfile,'Conductor ground insulation',tinstf,radius,'(tinstf)')

        radius = radius + 0.5e0*tfcoil_variables.dr_tf_wp_top - tfcoil_variables.tinstf
        call obuild(outfile,'Conductor - first half', 0.5e0*tfcoil_variables.dr_tf_wp_top - tfcoil_variables.tinstf,              radius, '(tfcoil_variables.dr_tf_wp_top/2-tfcoil_variables.tinstf)')

        radius = radius + 0.5e0*tfcoil_variables.dr_tf_wp_top - tfcoil_variables.tinstf
        call obuild(outfile,'Conductor - second half', 0.5e0*tfcoil_variables.dr_tf_wp_top - tfcoil_variables.tinstf,             radius,'(tfcoil_variables.dr_tf_wp_top/2-tfcoil_variables.tinstf)')

        radius = radius + tfcoil_variables.tinstf
        call obuild(outfile,'Conductor ground insulation',tinstf,radius,'(tinstf)')

        radius = radius + tfcoil_variables.casthi
        call obuild(outfile,'Plasma side TF coil support',casthi,radius,'(casthi)')

        # Consistency check
        if ( abs( radius - build_variables.r_cp_top ) < epsilon(radius) ) :
            call ocmmnt(outfile,'Top TF coil dimensions are consistent')
        else:
            call ocmmnt(outfile,'ERROR: TF coil dimensions are NOT consistent:')
            call ovarre(outfile,'Radius of plasma-facing side of inner leg SHOULD BE [m]','',r_cp_top)
            call oblnkl(outfile)




    
    def eyoung_parallel(self):
        """
        
        """
            poisson_j_perp_3 = (poisson_j_perp_1 * a_1 + poisson_j_perp_2 * a_2) / (a_1 + a_2)
    eyoung_j_3 = (eyoung_j_1 * a_1 + eyoung_j_2 * a_2) / (a_1 + a_2)
    a_3 = a_1 + a_2

    #-# end break

end subroutine eyoung_parallel

# ##################################################################

subroutine eyoung_series(eyoung_j_1, l_1, poisson_j_perp_1, & # Inputs
                      eyoung_j_2, l_2, poisson_j_perp_2, & # Inputs
                      eyoung_j_3, l_3, poisson_j_perp_3)   # Outputs

    ## Author : C. Swanson, PPPL
    ## January 2022
    ## See Issue #1205 for derivation PDF
    ## This subroutine gives the smeared elastic properties of two
    ## members that are carrying a force in series with each other.
    ## The force goes in direction j.
    ## The assumption is that the stresses in j are equal.
    ## The smeared Young's modulus is the inverse of the average of
    ## the inverse of the Young's moduli, weighted by the length
    ## of the members in j.
    ## Members 1 and 2 are the individual members to be smeared.
    ## Member 3 is the effective smeared member (output).
    ## The smeared Poisson's ratio is the averaged of the Poisson's
    ## ratios, weighted by the quantity (Young's modulus / length of
    ## the members in j).
    ##
    ## If you're dealing with anisotropy, please pay attention to the
    ## fact that the specific Young's Modulus used here is that in
    ## the j direction, and the specific Poisson's ratio used here is
    ## that between the j and transverse directions in that order.
    ## (transverse strain / j strain, under j stress)
    ## The smeared Poisson's ratio is computed assuming the transverse
    ## dynamics are isotropic, and that the two members are free to
    ## shrink/expand under Poisson effects without interference from
    ## each other. This may not be true of your case.
    ##
    ## To build up a composite smeared member of any number of
    ## individual members, you can pass the same properties for
    ## members 2 and 3, and call it successively, using the properties
    ## of each member as the first triplet of arguments. This way, the
    ## last triplet acts as a "working sum":
    ## call eyoung_series(triplet1, triplet2, tripletOUT)
    ## call eyoung_series(triplet3, tripletOUT, tripletOUT)
    ## call eyoung_series(triplet4, tripletOUT, tripletOUT)
    ## ... etc.
    ## So that tripletOUT would eventually have the smeared properties
    ## of the total composite member.
    # ###############################################

    implicit none

    # Inputs
    # ---
    real(dp), intent(in) :: eyoung_j_1,eyoung_j_2
    ## Young's modulus of members 1,2 in the j direction [Pa]

    real(dp), intent(in) :: l_1,l_2
    ## Length of members 1,2 in the j direction
    ## [m, or consistent units]

    real(dp), intent(in) :: poisson_j_perp_1,poisson_j_perp_2
    ## Poisson's ratio between the j and transverse directions,
    ## in that order, of members 1,2
    ## (transverse strain / j strain, under j stress)

    # Outputs
    # ---
    real(dp), intent(in out) :: eyoung_j_3
    ## Young's modulus of composite member in the j direction [Pa]

    real(dp), intent(in out) :: l_3
    ## Length of composite member in the j direction
    ## [m, or consistent units]

    real(dp), intent(in out) :: poisson_j_perp_3
    ## Poisson's ratio between the j and transverse directions,
    ## in that order, of composite member
    ## (transverse strain / j strain, under j stress)

    if ( eyoung_j_1*eyoung_j_2 == 0 ) :
      #poisson_j_perp_3 = 0
      if ( eyoung_j_1 == 0) :
        poisson_j_perp_3 = poisson_j_perp_1
      else:
        poisson_j_perp_3 = poisson_j_perp_2 #

      eyoung_j_3 = 0
      l_3 = l_1 + l_2
    else:
      poisson_j_perp_3 = (poisson_j_perp_1*l_1/eyoung_j_1 + poisson_j_perp_2*l_2/eyoung_j_2) / (l_1/eyoung_j_1 + l_2/eyoung_j_2)
      eyoung_j_3 = (l_1 + l_2) / (l_1/eyoung_j_1 + l_2/eyoung_j_2)
      l_3 = l_1 + l_2



    #-# end break

end subroutine eyoung_series

# ##################################################################

subroutine eyoung_t_nested_squares(n,eyoung_j_in, l_in, poisson_j_perp_in, & # Inputs
                      eyoung_j_out, l_out, poisson_j_perp_out, eyoung_stiffest)   # Outputs

    ## Author : C. Swanson, PPPL
    ## January 2022
    ## This subroutine gives the smeared transverse elastic
    ## properties of n members whose cross sectional areas are
    ## nested squares. It uses the subroutines eyoung_series and
    ## eyoung_parallel, above, so please be aware of the assumptions
    ## inherent in those subroutines.
    ##
    ## It assumes that each "leg" of the square cross section
    ## (vertical slice, as described in Figure 10 of the TF coil
    ## documentation) is composed of several layers under stress in
    ## series, and each leg is under stress in parallel with every
    ## other leg.
    # ###############################################

    # Inputs
    # ---
    integer :: n
    ## Number of nested-square-cross-section members

    real(dp), dimension(n), intent(in) :: eyoung_j_in
    ## Young's modulus of members in the j direction [Pa]

    real(dp), dimension(n), intent(in) :: l_in
    ## Length of members in the j direction
    ## [m, or consistent units]

    real(dp), dimension(n), intent(in) :: poisson_j_perp_in
    ## Poisson's ratio between the j and transverse directions,
    ## in that order, of members
    ## (transverse strain / j strain, under j stress)

    # Outputs
    # ---
    real(dp), intent(out) :: eyoung_j_out
    ## Young's modulus of composite member in the j direction [Pa]

    real(dp), intent(out) :: l_out
    ## Length of composite member in the j direction
    ## [m, or consistent units]

    real(dp), intent(out) :: poisson_j_perp_out
    ## Poisson's ratio between the j and transverse directions,
    ## in that order, of composite member
    ## (transverse strain / j strain, under j stress)

    real(dp), intent(out) :: eyoung_stiffest
    ## Young's modulus of "leg" with the highest Young's modulus [Pa]

    # Local
    # ---
    integer :: ii,jj ## Indices

    real(dp), dimension(n) :: eyoung_j_working
    ## Working array of the Young's moduli of the legs [Pa]

    real(dp), dimension(n) :: l_working
    ## Working array of the linear dimension of the legs [m]

    real(dp), dimension(n) :: poisson_j_perp_working
    ## Working array of the Poissin's ratios of the legs

    # Initialize
    eyoung_j_working = 0
    l_working = 0
    poisson_j_perp_working = 0
    eyoung_j_out = 0
    l_out = 0
    poisson_j_perp_out = 0
    eyoung_stiffest = 0

    # First member
    eyoung_j_working(1) = eyoung_j_in(1)
    l_working(1) = l_in(1)
    poisson_j_perp_working(1) = poisson_j_perp_in(1)

    do ii = 2,n
      # Initialize the leg of which this is the new member
      eyoung_j_working(ii) = eyoung_j_in(ii)
      l_working(ii) = l_working(ii-1) + l_in(ii)
      poisson_j_perp_working(ii) = poisson_j_perp_in(ii)

      # Serial-composite the new layer of this member into the previous legs
      do jj = 1,(ii-1)
        call eyoung_series(eyoung_j_working(ii),l_in(ii),poisson_j_perp_working(ii),                 eyoung_j_working(jj),l_working(jj),poisson_j_perp_working(jj),                 eyoung_j_working(jj),l_working(jj),poisson_j_perp_working(jj))
      end do
    end do

    # Find stiffest leg
    do ii = 1,n
      if (eyoung_stiffest < eyoung_j_working(ii)) :
        eyoung_stiffest = eyoung_j_working(ii)

    end do

    # Parallel-composite them all together
    do ii = 1,n
      call eyoung_parallel(eyoung_j_working(ii),l_in(ii),poisson_j_perp_working(ii),                 eyoung_j_out,l_out,poisson_j_perp_out,                 eyoung_j_out,l_out,poisson_j_perp_out)
    end do

    #-# end break

end subroutine eyoung_t_nested_squares

# ##################################################################

subroutine eyoung_parallel_array(n,eyoung_j_in, a_in, poisson_j_perp_in, & # Inputs
                      eyoung_j_out, a_out, poisson_j_perp_out)   # Outputs

    ## Author : C. Swanson, PPPL
    ## January 2022
    ## This subroutine gives the smeared axial elastic
    ## properties of n members in parallel. It uses the subroutines
    ## eyoung_parallel, above, so please be aware of the assumptions
    ## inherent in that subroutine.
    # ###############################################

    # Inputs
    # ---
    integer :: n
    ## Number of parallel members

    real(dp), dimension(n), intent(in) :: eyoung_j_in
    ## Young's modulus of members 1,2 in the j direction [Pa]

    real(dp), dimension(n), intent(in) :: a_in
    ## Length of members 1,2 in the j direction
    ## [m2, or consistent units]

    real(dp), dimension(n), intent(in) :: poisson_j_perp_in
    ## Poisson's ratio between the j and transverse directions,
    ## in that order, of members 1,2
    ## (transverse strain / j strain, under j stress)

    # Outputs
    # ---
    real(dp), intent(out) :: eyoung_j_out
    ## Young's modulus of composite member in the j direction [Pa]

    real(dp), intent(out) :: a_out
    ## Length of composite member in the j direction
    ## [m2, or consistent units]

    real(dp), intent(out) :: poisson_j_perp_out
    ## Poisson's ratio between the j and transverse directions,
    ## in that order, of composite member
    ## (transverse strain / j strain, under j stress)

    # Local
    # ---
    integer :: ii
    ## Indices

    # Initialize
    eyoung_j_out = 0
    a_out = 0
    poisson_j_perp_out = 0

    # Parallel-composite them all together
    do ii = 1,n
      call eyoung_parallel(eyoung_j_in(ii),a_in(ii),poisson_j_perp_in(ii),                 eyoung_j_out,a_out,poisson_j_perp_out,                 eyoung_j_out,a_out,poisson_j_perp_out)
    end do

    #-# end break


    
    def stresscl(self):
        """
         TF coil stress routine
 author: P J Knight, CCFE, Culham Science Centre
 author: J Morris, CCFE, Culham Science Centre
 author: S Kahn, CCFE, Culham Science Centre
 author: J Galambos, FEDC/ORNL
 This subroutine sets up the stress calculations for the
 TF coil set.
 PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
        """
            if ( abs(build_variables.r_tf_inboard_in) < epsilon(build_variables.r_tf_inboard_in) ) :
        # New extended plane strain model can handle it
        if ( tfcoil_variables.i_tf_stress_model /= 2 ) :
            call report_error(245)
            tfcoil_variables.sig_tf_case = 0.0e0
            tfcoil_variables.sig_tf_wp = 0.0e0
            return




    if (tfcoil_variables.acstf >= 0.0e0) :
        tcbs = sqrt(tfcoil_variables.acstf)
    else:
        tcbs = 0.0e0




    # LAYER ELASTIC PROPERTIES
    # ------------------------
    # Number of bucking layers
    tfcoil_variables.n_tf_bucking = tfcoil_variables.i_tf_bucking

    # CS properties (bucked and wedged)
    # ---
    if ( tfcoil_variables.i_tf_bucking >= 2 ) :

        # Calculation performed at CS flux swing (no current on CS)
        jeff(1) = 0.0e0

        # Inner radius of the CS
        radtf(1) = build_variables.bore

        # Superconducting CS
        if ( ipfres == 0 ) :

            # Getting the turn dimention from scratch
            # as the TF is called before CS in caller.f90
            #-#

            # CS vertical cross-section area [m2]
            a_oh = 2.0e0 * build_variables.hmax * ohhghf * build_variables.ohcth

            # Maximum current in Central Solenoid, at either BOP or EOF [MA-turns]
            # Absolute value
            curr_oh_max = 1.0e-6*max(coheof,cohbop)*a_oh

            #  Number of turns
            n_oh_turns = 1.0e6 * curr_oh_max / cptdin(sum(ncls))

            # CS Turn vertical cross-sectionnal area
            a_oh_turn = a_oh / n_oh_turns

            # Central Solenoid (OH) turn dimension [m]
            t_turn_oh = sqrt( a_oh_turn )

            # OH/CS conduit thickness calculated assuming square conduit [m]
            # The CS insulation layer is assumed to the same as the TF one
            t_cond_oh = 0.5e0*(t_turn_oh - 2.0e0*tfcoil_variables.thicndut -                                sqrt( (2.0e0*tfcoil_variables.thicndut - t_turn_oh)**2 -                                oh_steel_frac * t_turn_oh**2) )

            # CS turn cable space thickness
            t_cable_oh = t_turn_oh - 2.0e0 * ( t_cond_oh + tfcoil_variables.thicndut )
            #-#

            # Smeared elastic properties of the CS
            # These smearing functions were written assuming transverse-
            # isotropic materials; that is not true of the CS, where the
            # stiffest dimension is toroidal and the radial and vertical
            # dimension are less stiff. Nevertheless this attempts to
            # hit the mark.
            # [EDIT: eyoung_cond is for the TF coil, not the CS coil]

            # Get transverse properties
            call eyoung_parallel(tfcoil_variables.eyoung_steel,                                  oh_steel_frac,                                  tfcoil_variables.poisson_steel,                                  tfcoil_variables.eyoung_cond_axial,                                  1e0-oh_steel_frac,                                  tfcoil_variables.poisson_cond_axial,                                  eyoung_trans(1),                                  a_working,                                  poisson_trans(1))

            # Get vertical properties
            # Split up into "members", concentric squares in cross section
            # (described in Figure 10 of the TF coil documentation)
            # Conductor
            eyoung_member_array(1)  = tfcoil_variables.eyoung_cond_trans
            poisson_member_array(1) = tfcoil_variables.poisson_cond_trans
            l_member_array(1)       = t_cable_oh
            # Steel conduit
            eyoung_member_array(2)  = tfcoil_variables.eyoung_steel
            poisson_member_array(2) = tfcoil_variables.poisson_steel
            l_member_array(2)       = 2*t_cond_oh
            # Insulation
            eyoung_member_array(3)  = tfcoil_variables.eyoung_ins
            poisson_member_array(3) = tfcoil_variables.poisson_ins
            l_member_array(3)       = 2*tfcoil_variables.thicndut
            # [EDIT: Add central cooling channel? Would be new member #1]

            # Compute the composited (smeared) properties
            call eyoung_t_nested_squares(3,                                          eyoung_member_array,                                          l_member_array,                                          poisson_member_array,                                          eyoung_axial(1),                                          a_working,poisson_axial(1),                                          eyoung_cs_stiffest_leg)

        # resistive CS (copper)
        else:
            # Here is a rough approximation
            eyoung_trans(1) = tfcoil_variables.eyoung_copper
            eyoung_axial(1) = tfcoil_variables.eyoung_copper
            poisson_trans(1) = tfcoil_variables.poisson_copper
            poisson_axial(1) = tfcoil_variables.poisson_copper


    # ---


    # CS-TF interlayer properties
    # ---
    if ( tfcoil_variables.i_tf_bucking == 3 ) :

        # No current in this layer
        jeff(2) = 0.0e0

        # Outer radius of the CS
        radtf(2) = build_variables.bore + build_variables.ohcth

        # Assumed to be Kapton for the moment
        # Ref : https://www.dupont.com/content/dam/dupont/products-and-services/membranes-and-films/polyimde-films/documents/DEC-Kapton-summary-of-properties.pdf
        eyoung_trans(2) = 2.5e9
        eyoung_axial(2) = 2.5e9
        poisson_trans(2) = 0.34e0  # Default value for young modulus
        poisson_axial(2) = 0.34e0  # Default value for young modulus

    # ---


    # bucking cylinder/casing properties
    # ---
    if ( tfcoil_variables.i_tf_bucking >= 1 ) :

        # No current in bucking cylinder/casing
        jeff(tfcoil_variables.n_tf_bucking) = 0.0e0

        if ( tfcoil_variables.i_tf_sup == 1 ) :
            eyoung_trans(tfcoil_variables.n_tf_bucking) = tfcoil_variables.eyoung_steel
            eyoung_axial(tfcoil_variables.n_tf_bucking) = tfcoil_variables.eyoung_steel
            poisson_trans(tfcoil_variables.n_tf_bucking) = tfcoil_variables.poisson_steel
            poisson_axial(tfcoil_variables.n_tf_bucking) = tfcoil_variables.poisson_steel

        # Bucking cylinder properties
        else:
            eyoung_trans(tfcoil_variables.n_tf_bucking) = tfcoil_variables.eyoung_res_tf_buck
            eyoung_axial(tfcoil_variables.n_tf_bucking) = tfcoil_variables.eyoung_res_tf_buck
            poisson_trans(tfcoil_variables.n_tf_bucking) = tfcoil_variables.poisson_steel # Seek better value #
            poisson_axial(tfcoil_variables.n_tf_bucking) = tfcoil_variables.poisson_steel # Seek better value #


        # Innernost TF casing radius
        radtf(tfcoil_variables.n_tf_bucking) = build_variables.r_tf_inboard_in

    #---


    # (Super)conductor layer properties
    # ---
    # SC coil
    if ( tfcoil_variables.i_tf_sup == 1 ) :

        # Inner/outer radii of the layer representing the WP in stress calculations [m]
        # These radii are chosen to preserve the true WP area; see Issue #1048
        r_wp_inner_eff = r_wp_inner * sqrt( tan_theta_coil / theta_coil )
        r_wp_outer_eff = r_wp_outer * sqrt( tan_theta_coil / theta_coil )

        # Area of the cylinder representing the WP in stress calculations [m2]
        a_wp_eff = ( r_wp_outer_eff**2 - r_wp_inner_eff**2 ) * theta_coil

        # Steel cross-section under the area representing the WP in stress calculations [m2]
        a_wp_steel_eff = a_tf_steel - a_case_front - a_case_nose

        # WP effective insulation thickness (SC only) [m]
        # include groundwall insulation + insertion gap in tfcoil_variables.thicndut
        # inertion gap is tfcoil_variables.tfinsgap on 4 sides
        t_ins_eff = tfcoil_variables.thicndut + ( tfcoil_variables.tfinsgap + tfcoil_variables.tinstf ) / tfcoil_variables.n_tf_turn

        # Effective WP young modulus in the toroidal direction [Pa]
        # The toroidal property drives the stress calculation (J. Last report no 4)
        # Hence, the radial direction is relevant for the property smearing
        # Rem : This assumption might be re-defined for bucked and wedged design
        if ( tfcoil_variables.i_tf_turns_integer == 0 ) :
            # Non-integer number of turns
            t_cable_eyng = t_cable
        else:
            # Integer number of turns
            t_cable_eyng = t_cable_radial


        # Average WP Young's modulus in the transverse
        # (radial and toroidal) direction
        # Split up into "members", concentric squares in cross section
        # (described in Figure 10 of the TF coil documentation)
        # Helium
        eyoung_member_array(1)  = 0e0
        poisson_member_array(1) = tfcoil_variables.poisson_steel
        l_member_array(1)       = tfcoil_variables.dhecoil
        # Conductor and co-wound copper
        call eyoung_series(tfcoil_variables.eyoung_cond_trans,                            (t_cable_eyng-tfcoil_variables.dhecoil)*(1.0e0-tfcoil_variables.fcutfsu),                            tfcoil_variables.poisson_cond_trans,                            tfcoil_variables.eyoung_copper,                            (t_cable_eyng-tfcoil_variables.dhecoil)*tfcoil_variables.fcutfsu,                            tfcoil_variables.poisson_copper,                            eyoung_member_array(2),                            l_member_array(2),                            poisson_member_array(2))
        # Steel conduit
        eyoung_member_array(3)  = tfcoil_variables.eyoung_steel
        poisson_member_array(3) = tfcoil_variables.poisson_steel
        l_member_array(3)       = 2*tfcoil_variables.thwcndut
        # Insulation
        eyoung_member_array(4)  = tfcoil_variables.eyoung_ins
        poisson_member_array(4) = tfcoil_variables.poisson_ins
        l_member_array(4)       = 2*t_ins_eff

        # Compute the composited (smeared) properties
        call eyoung_t_nested_squares(4,                                      eyoung_member_array,                                      l_member_array,                                      poisson_member_array,                                      eyoung_wp_trans,                                      a_working,                                      poisson_wp_trans,                                      eyoung_wp_stiffest_leg)

        # Lateral casing correction (series-composition)
        call eyoung_series(eyoung_wp_trans,                            t_wp_toroidal_av,                            poisson_wp_trans,                            tfcoil_variables.eyoung_steel,                            2.0e0*t_lat_case_av,                            tfcoil_variables.poisson_steel,                            eyoung_wp_trans_eff,                            a_working,                            poisson_wp_trans_eff)

        # Average WP Young's modulus in the vertical direction
        # Split up into "members", concentric squares in cross section
        # (described in Figure 10 of the TF coil documentation)
        # Steel conduit
        eyoung_member_array(1)  = tfcoil_variables.eyoung_steel
        poisson_member_array(1) = tfcoil_variables.poisson_steel
        l_member_array(1)       = tfcoil_variables.aswp
        # Insulation
        eyoung_member_array(2)  = tfcoil_variables.eyoung_ins
        poisson_member_array(2) = tfcoil_variables.poisson_ins
        l_member_array(2)       = a_tf_ins
        # Copper
        eyoung_member_array(3)  = tfcoil_variables.eyoung_copper
        poisson_member_array(3) = tfcoil_variables.poisson_copper
        l_member_array(3)       = tfcoil_variables.acond*tfcoil_variables.fcutfsu
        # Conductor
        eyoung_member_array(4)  = tfcoil_variables.eyoung_cond_axial
        poisson_member_array(4) = tfcoil_variables.poisson_cond_axial
        l_member_array(4)       = tfcoil_variables.acond*(1.0e0-tfcoil_variables.fcutfsu)
        # Helium and void
        eyoung_member_array(5)  = 0e0
        poisson_member_array(5) = tfcoil_variables.poisson_steel
        l_member_array(5)       = awpc - tfcoil_variables.acond - a_tf_ins - tfcoil_variables.aswp
        # Compute the composite / smeared properties:
        call eyoung_parallel_array(5,                                    eyoung_member_array,                                    l_member_array,                                    poisson_member_array,                                    eyoung_wp_axial,                                    a_working,                                    poisson_wp_axial)

        # Average WP Young's modulus in the vertical direction, now including the lateral case
        # Parallel-composite the steel and insulation, now including the lateral case (sidewalls)
        call eyoung_parallel(tfcoil_variables.eyoung_steel,                              a_wp_steel_eff - tfcoil_variables.aswp,poisson_steel,                              eyoung_wp_axial,                              a_working,                              poisson_wp_axial,                              eyoung_wp_axial_eff,                              a_working,                              poisson_wp_axial_eff)

    # Resistive coil
    else:

        # Picking the conductor material Young's modulus
        if ( tfcoil_variables.i_tf_sup == 0 ) :
            eyoung_cond = tfcoil_variables.eyoung_copper
            poisson_cond = tfcoil_variables.poisson_copper
        elif  ( tfcoil_variables.i_tf_sup == 2 ) :
            eyoung_cond = tfcoil_variables.eyoung_al
            poisson_cond = tfcoil_variables.poisson_al


        # Effective WP young modulus in the toroidal direction [Pa]
        # Rem : effect of cooling pipes and insulation not taken into account
        #       for now as it needs a radially dependent Young modulus
        eyoung_wp_trans_eff = eyoung_cond
        eyoung_wp_trans = eyoung_cond
        poisson_wp_trans_eff = poisson_cond
        poisson_wp_trans = poisson_cond

        # WP area using the stress model circular geometry (per coil) [m2]
        a_wp_eff = (r_wp_outer**2 - r_wp_inner**2) * theta_coil

        # Effective conductor region young modulus in the vertical direction [Pa]
        # Parallel-composite conductor and insulator
        call eyoung_parallel(eyoung_cond,                              (a_wp_eff-a_tf_ins)*(1.0e0-tfcoil_variables.fcoolcp),                              poisson_cond,                              tfcoil_variables.eyoung_ins,                              a_tf_ins,                              tfcoil_variables.poisson_ins,                              eyoung_wp_axial,                              a_working,                              poisson_wp_axial)
        # Parallel-composite cooling pipes into that
        call eyoung_parallel(0e0,                              (a_wp_eff-a_tf_ins)*tfcoil_variables.fcoolcp,poisson_cond,                              eyoung_wp_axial,                              a_working,                              poisson_wp_axial,                              eyoung_wp_axial,                              a_working,                              poisson_wp_axial)

        # Effective young modulus used in stress calculations
        eyoung_wp_axial_eff = eyoung_wp_axial
        poisson_wp_axial_eff = poisson_wp_axial

        # Effect conductor layer inner/outer radius
        r_wp_inner_eff = r_wp_inner
        r_wp_outer_eff = r_wp_outer



    # Thickness of the layer representing the WP in stress calcualtions [m]
    tfcoil_variables.dr_tf_wp_eff = r_wp_outer_eff - r_wp_outer_eff

    # Thickness of WP with homogeneous stress property [m]
    dr_wp_layer = tfcoil_variables.dr_tf_wp_eff / dble(tfcoil_variables.n_tf_graded_layers)

    # Loop on layers
    do ii = 1, tfcoil_variables.n_tf_graded_layers

        # Homogeneous current in (super)conductor
        jeff(tfcoil_variables.n_tf_bucking + ii) = tfcoil_variables.ritfc / (pi * (r_wp_outer_eff**2 - r_wp_inner_eff**2))

        # Same thickness for all WP layers in stress calculation
        radtf(tfcoil_variables.n_tf_bucking + ii) = r_wp_inner_eff + dble(ii-1)*dr_wp_layer

        # Young modulus
        eyoung_trans(tfcoil_variables.n_tf_bucking + ii) = eyoung_wp_trans_eff
        eyoung_axial(tfcoil_variables.n_tf_bucking + ii) = eyoung_wp_axial_eff

        # Poisson's ratio
        poisson_trans(tfcoil_variables.n_tf_bucking + ii) = poisson_wp_trans_eff
        poisson_axial(tfcoil_variables.n_tf_bucking + ii) = poisson_wp_axial_eff

    end do

    # Steel case on the plasma side of the inboard TF coil
    # As per Issue #1509
    jeff(tfcoil_variables.n_tf_layer) = 0.0e0
    radtf(tfcoil_variables.n_tf_layer) = r_wp_outer_eff
    eyoung_trans(tfcoil_variables.n_tf_layer) = tfcoil_variables.eyoung_steel
    eyoung_axial(tfcoil_variables.n_tf_layer) = tfcoil_variables.eyoung_steel
    poisson_trans(tfcoil_variables.n_tf_layer) = tfcoil_variables.poisson_steel
    poisson_axial(tfcoil_variables.n_tf_layer) = tfcoil_variables.poisson_steel

    # last layer radius
    radtf(tfcoil_variables.n_tf_layer + 1) = r_wp_outer_eff + tfcoil_variables.casthi

    # The ratio between the true cross sectional area of the
    # front case, and that considered by the plane strain solver
    f_tf_stress_front_case = a_case_front / theta_coil / (radtf(tfcoil_variables.n_tf_layer+1)**2 - radtf(tfcoil_variables.n_tf_layer)**2)

    # Correct for the missing axial stiffness from the missing
    # outer case steel as per the updated description of
    # Issue #1509
    eyoung_axial(tfcoil_variables.n_tf_layer) = eyoung_axial(tfcoil_variables.n_tf_layer) * f_tf_stress_front_case

    # ---
    # ------------------------



    # RADIAL STRESS SUBROUTINES CALL
    # ------------------------------
    # Stress model not valid the TF does not contain any hole
    # (Except if i_tf_plane_stress == 2; see Issue 1414)
    # Current action : trigger and error and add a little hole
    #                  to allow stress calculations
    # Rem SK : Can be easily ameneded playing around the boundary conditions
    if ( abs(radtf(1)) < epsilon(radtf(1)) ) :
        # New extended plane strain model can handle it
        if ( tfcoil_variables.i_tf_stress_model /= 2 ) :
            call report_error(245)
            radtf(1) = 1.0e-9
        elif  ( abs(radtf(2)) < epsilon(radtf(1)) ) :
            write(*,*)'ERROR: First TF layer has zero thickness.'
            write(*,*)'       Perhaps you meant to have thkcas nonzero or tfcoil_variables.i_tf_bucking = 0?'


    # ---


    # Old generalized plane stress model
    # ---
    if ( tfcoil_variables.i_tf_stress_model == 1 ) :

        # Plane stress calculation (SC) [Pa]
        call plane_stress( poisson_trans, radtf, eyoung_trans, jeff, & # Inputs
                           tfcoil_variables.n_tf_layer, n_radial_array,       & # Inputs
                           sig_tf_r, sig_tf_t, deflect, radial_array ) # Outputs

        # Vertical stress [Pa]
        sig_tf_z = tfcoil_variables.vforce / (tfcoil_variables.acasetf + tfcoil_variables.acndttf*tfcoil_variables.n_tf_turn) # Array equation [EDIT: Are you sure? It doesn't look like one to me]

        # Strain in vertical direction on WP
        tfcoil_variables.str_wp = sig_tf_z(tfcoil_variables.n_tf_bucking+1) / eyoung_wp_axial_eff

        # Case strain
        tfcoil_variables.casestr = sig_tf_z(tfcoil_variables.n_tf_bucking) / tfcoil_variables.eyoung_steel

        # Radial strain in insulator
        tfcoil_variables.insstrain = sig_tf_r(n_radial_array) * eyoung_wp_stiffest_leg / eyoung_wp_trans_eff / tfcoil_variables.eyoung_ins
    # ---


    # New generalized plane strain formulation
    # ---
    elif  ( tfcoil_variables.i_tf_stress_model == 0) :
        # Generalized plane strain calculation [Pa]
        # Issues #977 and #991
        # build_variables.bore > 0, O(n^3) in layers
        call generalized_plane_strain( poisson_trans, poisson_axial, eyoung_trans, eyoung_axial,  & # Inputs
                                       radtf, jeff, tfcoil_variables.vforce_inboard_tot,          & # Inputs
                                       tfcoil_variables.n_tf_layer, n_radial_array, tfcoil_variables.n_tf_bucking,  & # Inputs
                                       radial_array, sig_tf_r, sig_tf_t, sig_tf_z,    & # Outputs
                                       str_tf_r, str_tf_t, str_tf_z, deflect ) # Outputs

        # Strain in TF conductor material
        tfcoil_variables.str_wp = str_tf_z(tfcoil_variables.n_tf_bucking*n_radial_array+1);

    elif  ( tfcoil_variables.i_tf_stress_model == 2) :
        # Extended plane strain calculation [Pa]
        # Issues #1414 and #998
        # Permits build_variables.bore >= 0, O(n) in layers
        # If build_variables.bore > 0, same result as generalized plane strain calculation
        call extended_plane_strain( poisson_trans, poisson_axial, eyoung_trans, eyoung_axial,  & # Inputs
                                       radtf, jeff, tfcoil_variables.vforce_inboard_tot,          & # Inputs
                                       tfcoil_variables.n_tf_layer, n_radial_array, tfcoil_variables.n_tf_bucking,  & # Inputs
                                       radial_array, sig_tf_r, sig_tf_t, sig_tf_z,    & # Outputs
                                       str_tf_r, str_tf_t, str_tf_z, deflect ) # Outputs

        # Strain in TF conductor material
        tfcoil_variables.str_wp = str_tf_z(tfcoil_variables.n_tf_bucking*n_radial_array+1);


    # ---

    # Storing the smeared properties for output
    if ( iprint == 1 ) :
        sig_tf_smeared_r = sig_tf_r   # Array equation
        sig_tf_smeared_t = sig_tf_t   # Array equation
        sig_tf_smeared_z = sig_tf_z   # Array equation

    # ------------------------------

    # STRESS DISTRIBUTIONS CORRECTIONS
    # --------------------------------
    # SC central solenoid coil stress unsmearing (bucked and wedged only)
    # ---
    if ( tfcoil_variables.i_tf_bucking >= 2 .and. ipfres == 0 ) :

        # Central Solenoid (OH) steel conduit stress unsmearing factors

        do ii = 1, n_radial_array

            # CS (OH) superconducting case stress unsmearing
            sig_tf_r(ii) = sig_tf_r(ii) * eyoung_cs_stiffest_leg/eyoung_axial(1)
            sig_tf_t(ii) = sig_tf_t(ii) * tfcoil_variables.eyoung_steel/eyoung_trans(1)
            sig_tf_z(ii) = sig_tf_z(ii) * eyoung_cs_stiffest_leg/eyoung_axial(1)
        end do

    # ---


    # No TF vertical forces on CS and CS-TF layer (bucked and wedged only)
    # ---
    # This correction is only applied if the plane stress model is used
    # as the generalized plane strain calculates the vertical stress properly
    if ( tfcoil_variables.i_tf_bucking >= 2 .and. tfcoil_variables.i_tf_stress_model == 1 ) :
        do ii = 1, (tfcoil_variables.n_tf_bucking-1)*n_radial_array
            sig_tf_z(ii) = 0.0e0
        end do

    # ---


    # Toroidal coil unsmearing
    # ---
    # Copper magnets
    if ( tfcoil_variables.i_tf_sup == 0 ) :

        # Vertical force unsmearing factor
        fac_sig_z = tfcoil_variables.eyoung_copper / eyoung_wp_axial_eff

        # Toroidal WP steel stress unsmearing factor
        fac_sig_t = 1.0e0
        fac_sig_r = 1.0e0

    elif  ( tfcoil_variables.i_tf_sup == 1 ) :

        # Vertical WP steel stress unsmearing factor
        if ( tfcoil_variables.i_tf_stress_model /= 1 ) :
            fac_sig_z = tfcoil_variables.eyoung_steel / eyoung_wp_axial_eff
            fac_sig_z_wp_av = eyoung_wp_axial / eyoung_wp_axial_eff
        else:
            fac_sig_z = 1.0e0


        # Toroidal WP steel conduit stress unsmearing factor
        fac_sig_t = eyoung_wp_stiffest_leg / eyoung_wp_trans_eff

        # Radial WP steel conduit stress unsmearing factor
        fac_sig_r = eyoung_wp_stiffest_leg / eyoung_wp_trans_eff

    elif  ( tfcoil_variables.i_tf_sup == 2 ) :

        # Vertical WP steel stress unsmearing factor
        fac_sig_z = tfcoil_variables.eyoung_al / eyoung_wp_axial_eff

        # Toroidal WP steel stress unsmearing factor
        # NO CALCULTED FOR THE MOMENT (to be done later)
        fac_sig_t = 1.0e0
        fac_sig_r = 1.0e0



    # Application of the unsmearing to the WP layers
    # For each point within the winding pack / conductor, unsmear the
    # stress. This is n_radial_array test points within tfcoil_variables.n_tf_graded_layers
    # layers starting at tfcoil_variables.n_tf_bucking + 1
    # GRADED MODIF : add another do loop to allow the graded properties
    #                to be taken into account
    do ii = tfcoil_variables.n_tf_bucking * n_radial_array + 1, (tfcoil_variables.n_tf_bucking + tfcoil_variables.n_tf_graded_layers)*n_radial_array
        tfcoil_variables.sig_tf_wp_av_z(ii - tfcoil_variables.n_tf_bucking * n_radial_array) = sig_tf_z(ii) * fac_sig_z_wp_av
        sig_tf_r(ii) = sig_tf_r(ii) * fac_sig_r
        sig_tf_t(ii) = sig_tf_t(ii) * fac_sig_t
        sig_tf_z(ii) = sig_tf_z(ii) * fac_sig_z
    end do

    # For each point within the front case,
    # remove the correction for the missing axial
    # stiffness as per the updated description of
    # Issue #1509
    do ii = (tfcoil_variables.n_tf_bucking + tfcoil_variables.n_tf_graded_layers)*n_radial_array + 1, tfcoil_variables.n_tf_layer*n_radial_array
        sig_tf_z(ii) = sig_tf_z(ii) / f_tf_stress_front_case
    end do
    # ---


    # Tresca / Von Mises yield criteria calculations
    # -----------------------------
    # Array equation
    sig_tf_tresca = max( abs(sig_tf_r - sig_tf_t),                          abs(sig_tf_r - sig_tf_z),                          abs(sig_tf_z - sig_tf_t) )

    # Array equation
    if ( iprint == 1 ) :
        sig_tf_vmises = sqrt( 0.5e0*(  (sig_tf_r - sig_tf_t)**2                                      + (sig_tf_r - sig_tf_z)**2                                      + (sig_tf_z - sig_tf_t)**2 ) )


    # Array equation
    s_tresca_cond_cea = sig_tf_tresca


    # SC conducting layer stress distribution corrections
    # ---
    if ( tfcoil_variables.i_tf_sup == 1 ) :

        # GRADED MODIF : add another do loop to allow the graded properties
        #                to be taken into account
        do ii = tfcoil_variables.n_tf_bucking * n_radial_array + 1, tfcoil_variables.n_tf_layer*n_radial_array

            # Addaped Von-mises stress calculation to WP strucure [Pa]
            if ( iprint == 1 ) :
                svmxz = sigvm( 0.0e0, sig_tf_t(ii), sig_tf_z(ii), 0.0e0,0.0e0,0.0e0)
                svmyz = sigvm( sig_tf_r(ii), 0.0e0, sig_tf_z(ii), 0.0e0,0.0e0,0.0e0)
                sig_tf_vmises(ii) = max(svmxz, svmyz)


            # Maximum shear stress for the Tresca yield criterion using CEA calculation [Pa]
            s_tresca_cond_cea(ii) = 1.02e0*abs(sig_tf_r(ii)) + 1.6e0*sig_tf_z(ii)
        end do

    # ---
    # -----------------------------



    # Output formating : Maximum shear stress of each layer for the Tresca yield criterion
    # ----------------
    do ii = 1, tfcoil_variables.n_tf_layer
        sig_max = 0.0e0
        ii_max = 1

        do jj = (ii-1)*n_radial_array + 1, ii*n_radial_array

            # CEA out of plane approximation
            if ( tfcoil_variables.i_tf_tresca == 1 .and. tfcoil_variables.i_tf_sup == 1 .and. ii >= tfcoil_variables.i_tf_bucking + 1 ) :
                if ( sig_max < s_tresca_cond_cea(jj) ) :
                    sig_max = s_tresca_cond_cea(jj)
                    ii_max = jj


            # Conventional Tresca
            else:
                if ( sig_max < sig_tf_tresca(jj) ) :
                    sig_max = sig_tf_tresca(jj)
                    ii_max = jj


        end do

        # OUT.DAT output
        if ( iprint == 1 ) :
            sig_tf_r_max(ii) = sig_tf_r(ii_max)
            sig_tf_t_max(ii) = sig_tf_t(ii_max)
            sig_tf_z_max(ii) = sig_tf_z(ii_max)
            sig_tf_vmises_max(ii) = sig_tf_vmises(ii_max)


        # Maximum shear stress for the Tresca yield criterion (or CEA OOP correction)
        if ( tfcoil_variables.i_tf_tresca == 1 .and. tfcoil_variables.i_tf_sup == 1 .and. ii >= tfcoil_variables.i_tf_bucking + 1 ) :
            sig_tf_tresca_max(ii) = s_tresca_cond_cea(ii_max)
        else:
            sig_tf_tresca_max(ii) = sig_tf_tresca(ii_max)

    end do

    # Constraint equation for the Tresca yield criterion
    tfcoil_variables.sig_tf_wp = sig_tf_tresca_max(tfcoil_variables.n_tf_bucking + 1) # Maximum assumed in the first graded layer
    if ( tfcoil_variables.i_tf_bucking >= 1 ) tfcoil_variables.sig_tf_case = sig_tf_tresca_max(tfcoil_variables.n_tf_bucking)
    if ( tfcoil_variables.i_tf_bucking >= 2 ) tfcoil_variables.sig_tf_cs_bucked = sig_tf_tresca_max(1)
    # ----------------

    if ( iprint == 1 ) call out_stress(sig_tf_r_max, sig_tf_t_max, sig_tf_z_max,     sig_tf_vmises_max, sig_tf_tresca_max, deflect, eyoung_axial, eyoung_trans,     eyoung_wp_axial, eyoung_wp_trans, poisson_wp_trans, radial_array,     s_tresca_cond_cea, poisson_wp_axial,     sig_tf_r, sig_tf_smeared_r, sig_tf_smeared_t, sig_tf_smeared_z,     sig_tf_t, sig_tf_tresca, sig_tf_vmises, sig_tf_z, str_tf_r, str_tf_t, str_tf_z,     n_radial_array, tfcoil_variables.n_tf_bucking, outfile, sig_file)


    #-# end break


    
    def tf_res_heating(self):
        """
         Resitive magnet resitive heating calculations
 Rem SK : Clamped joined superconductors might have resistive power losses on the joints
 Rem SK : Sliding joints might have a region of high resistivity
        """
            if ( tfcoil_variables.i_tf_sup == 0 ) tfcoil_variables.rhocp = (tfcoil_variables.frhocp/0.92e0) * ( 1.72e0 + 0.0039e0*(tfcoil_variables.tcpav-273.15e0) ) * 1.0e-8

    # Aluminium
    if ( tfcoil_variables.i_tf_sup == 2 ) tfcoil_variables.rhocp = tfcoil_variables.frhocp * ( 2.00016e-14*tfcoil_variables.tcpav**3 - 6.75384e-13*tfcoil_variables.tcpav**2 + 8.89159e-12*tfcoil_variables.tcpav )

    # Calculations dedicated for configurations with CP
    if ( physics_variables.itart == 1 ) :

        # Tricky trick to make the leg / CP tempearture the same
        if ( abs(tfcoil_variables.tlegav + 1.0e0) < epsilon(tfcoil_variables.tlegav) ) :
            is_leg_cp_temp_same = 1
            tfcoil_variables.tlegav = tfcoil_variables.tcpav


        # Leg resistivity (different leg temperature as separate cooling channels)
        if ( tfcoil_variables.i_tf_sup == 0 ) tfcoil_variables.rhotfleg = (tfcoil_variables.frholeg/0.92e0) * ( 1.72e0 + 0.0039e0*(tfcoil_variables.tlegav-273.15e0) ) * 1.0e-8
        if ( tfcoil_variables.i_tf_sup == 2 ) tfcoil_variables.rhotfleg =  tfcoil_variables.frholeg * ( 2.00016e-14*tfcoil_variables.tlegav**3 - 6.75384e-13*tfcoil_variables.tlegav**2 + 8.89159e-12*tfcoil_variables.tlegav )

        # Tricky trick to make the leg / CP tempearture the same
        if ( is_leg_cp_temp_same == 1 ) tfcoil_variables.tlegav = -1.0e0

        # Centrepost resisitivity and conductor/insulation volume
        call cpost( build_variables.r_tf_inboard_in, build_variables.r_tf_inboard_out, build_variables.r_cp_top, h_cp_top,    & # Inputs
                    build_variables.hmax+build_variables.tfthko, tfcoil_variables.thkcas, tfcoil_variables.casthi, tfcoil_variables.tinstf, tfcoil_variables.thicndut, tfcoil_variables.n_tf_turn, & # Inputs
                    tfcoil_variables.ritfc, tfcoil_variables.rhocp, tfcoil_variables.fcoolcp,                                    & # Inputs
                    tfcoil_variables.a_cp_cool, tfcoil_variables.vol_cond_cp, tfcoil_variables.prescp,        & # Outputs
                    vol_ins_cp, vol_case_cp, vol_gr_ins_cp ) # Outputs


    # Leg cross-section areas
    # Rem : For physics_variables.itart = 1, these quantitire corresponds to the outer leg only
    # ---
    # Leg ground insulation area per coil [m2]
    a_leg_gr_ins = tfcoil_variables.arealeg - ( tfcoil_variables.tftort - 2.0e0 * tfcoil_variables.tinstf )                            * ( build_variables.tfthko - 2.0e0 * tfcoil_variables.tinstf )

    # Outboard leg turns insulation area per coil [m2]
    a_leg_ins = 2.0e0 * tfcoil_variables.thicndut * ( tfcoil_variables.tftort - 2.0e0 * tfcoil_variables.tinstf )     &                    # toroidal direction
              + 2.0e0 * tfcoil_variables.thicndut * tfcoil_variables.n_tf_turn * ( build_variables.tfthko - 2.0e0 * ( tfcoil_variables.thicndut + tfcoil_variables.tinstf ) ) # radial direction

    # Exact TF outboard leg conductor area per coil [m2]
    a_leg_cond = ( 1.0e0 - tfcoil_variables.fcoolleg ) * ( tfcoil_variables.arealeg - a_leg_gr_ins - a_leg_ins )
    # ---


    if ( physics_variables.itart == 1 ) :

        # Outer leg resistive power loss
        # ---
        # TF outboard leg's resistance calculation (per leg) [ohm]
        tfcoil_variables.tflegres = tfcoil_variables.rhotfleg * tfcoil_variables.tfleng / a_leg_cond

        # TF outer leg resistive power (TOTAL) [W]
        tfcoil_variables.presleg = tfcoil_variables.tflegres * tfcoil_variables.ritfc**2 / tfcoil_variables.n_tf
        # ---


        # Sliding joints resistive heating
        # ---
        if ( tfcoil_variables.i_cp_joints /= 0 ) :

            # Number of contact area per joint (all legs)
            n_contact_tot = tfcoil_variables.n_tf_joints_contact * nint(tfcoil_variables.n_tf_turn) * nint(tfcoil_variables.n_tf)

            # Area of joint contact (all legs)
            a_joints = build_variables.tfthko * tfcoil_variables.th_joint_contact * dble(n_contact_tot)

            # Total joints resistive power losses
            tfcoil_variables.pres_joints = dble(tfcoil_variables.n_tf_joints) * tfcoil_variables.rho_tf_joints * tfcoil_variables.ritfc**2 / a_joints
        else:
            # Joints resistance to be evaluated for SC
            tfcoil_variables.pres_joints = 0.0e0

        # ---


    # Case of a resistive magnet without joints
    # ***
    else:

        # TF resistive powers
        tfcoil_variables.prescp = tfcoil_variables.rhocp * tfcoil_variables.ritfc**2 * tfcoil_variables.tfleng / ( a_leg_cond * tfcoil_variables.n_tf )

        # tfcoil_variables.prescp containts the the total resistive power losses
        tfcoil_variables.presleg = 0.0e0

        # No joints if physics_variables.itart = 0
        tfcoil_variables.pres_joints = 0.0e0



    #-# end break


    
    def tfcind(self):
        """
         Calculates the self inductance of a TF coil
 tfthk        : input real : TF coil thickness (m)
 This routine calculates the self inductance of a TF coil
 approximated by a straight inboard section and two elliptical arcs.
 The inductance of the TFC (considered as a single axisymmetric turn)
 is calculated by numerical integration over the cross-sectional area.
 The contribution from the cross-sectional area of the
 coil itself is calculated by taking the field as B(r)/2.
 The field in the bore is calculated for unit current.
 Top/bottom symmetry is assumed.
        """
            tfcoil_variables.tfind = 0.0e0
    # Integrate over the whole TF area, including the coil thickness.
    x0 = tfcoil_variables.xarc(2)
    y0 = tfcoil_variables.yarc(2)

    # Minor and major radii of the inside and outside perimeters of the the
    # Inboard leg and arc.
    # Average the upper and lower halves, which are different in the
    # single null case
    ai = tfcoil_variables.xarc(2) - tfcoil_variables.xarc(1)
    bi = (tfcoil_variables.yarc(2)-tfcoil_variables.yarc(4))/2.0e0 - tfcoil_variables.yarc(1)
    ao = ai + tfthk
    bo = bi + tfthk
    # Interval used for integration
    dr = ao / dble(nintervals)
    # Start both integrals from the centre-point where the arcs join.
    # Initialise major radius
    r = x0 - dr/2.0e0
    do i = 1,nintervals
        # Field in the bore for unit current
        b = rmu0/(2.0e0*pi*r)
        # Find out if there is a bore
        if (x0-r < ai) :
            h_bore = y0 + bi * sqrt(1 - ((r-x0)/ai)**2)
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) - h_bore
        else:
            h_bore = 0.0e0
            # Include the contribution from the straight section
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) + tfcoil_variables.yarc(1)

        # Assume B in TF coil = 1/2  B in bore
        # Multiply by 2 for upper and lower halves of coil
        tfcoil_variables.tfind = tfcoil_variables.tfind + b*dr*(2.0e0*h_bore + h_thick)
        r = r - dr
    end do

    # Outboard arc
    ai = tfcoil_variables.xarc(3) - tfcoil_variables.xarc(2)
    bi = (tfcoil_variables.yarc(2) - tfcoil_variables.yarc(4))/2.0e0
    ao = ai + tfthk
    bo = bi + tfthk
    dr = ao / dble(nintervals)
    # Initialise major radius
    r = x0 + dr/2.0e0
    do i = 1,nintervals
        # Field in the bore for unit current
        b = rmu0/(2.0e0*pi*r)
        # Find out if there is a bore
        if (r-x0 < ai) :
            h_bore = y0 + bi * sqrt(1 - ((r-x0)/ai)**2)
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) - h_bore
        else:
            h_bore = 0.0e0
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2)

        # Assume B in TF coil = 1/2  B in bore
        # Multiply by 2 for upper and lower halves of coil
        tfcoil_variables.tfind = tfcoil_variables.tfind + b*dr*(2.0e0*h_bore + h_thick)
        r=r+dr
    end do

    #-# end break


    
    def tf_coil_area_and_masses(self):
        """
         Subroutine to calculate the TF coil areas and masses
        """
            vol_case = 0.0e0
    vol_ins = 0.0e0
    vol_gr_ins = 0.0e0
    vol_cond = 0.0e0
    vol_ins_leg = 0.0e0
    vol_gr_ins_leg = 0.0e0
    vol_cond_leg = 0.0e0
    # ---

    # Surface areas (for cryo system) [m2]
    # tfcoil_variables.tfsai, tfcoil_variables.tfsao are retained for the (obsolescent) TF coil nuclear heating calculation
    wbtf = build_variables.r_tf_inboard_out*sin(theta_coil) - build_variables.r_tf_inboard_in*tan_theta_coil
    tfcoil_variables.tfocrn = build_variables.r_tf_inboard_in * tan_theta_coil
    tfcoil_variables.tficrn = tfcoil_variables.tfocrn + wbtf
    tfcoil_variables.tfsai = 4.0e0 * tfcoil_variables.n_tf * tfcoil_variables.tficrn * build_variables.hr1
    tfcoil_variables.tfsao = 2.0e0 * tfcoil_variables.n_tf * tfcoil_variables.tficrn * (tfcoil_variables.tfleng - 2.0e0*build_variables.hr1)

    # Total surface area of two toroidal shells covering the TF coils [m2]
    # (inside and outside surfaces)
    # = 2 * centroid coil length * 2 pi R, where R is average of i/b and o/b centres
    # (This will possibly be used to replace 2*tfcoil_variables.tfsai in the calculation of qss
    # in subroutine cryo - not done at present.)
    tfcoil_variables.tfcryoarea = 2.0e0 * tfcoil_variables.tfleng * twopi*0.5e0*(build_variables.r_tf_inboard_mid+build_variables.r_tf_outboard_mid)


    # Superconductor coil design specific calculation
    # ---
    if ( tfcoil_variables.i_tf_sup == 1 ) :

        # Mass of case [kg]
        # ***

        # Mass of ground-wall insulation [kg]
        # (assumed to be same density/material as turn insulation)
        tfcoil_variables.whtgw = tfcoil_variables.tfleng * (awpc-awptf) * tfcoil_variables.dcondins

        # The length of the vertical section is that of the first (inboard) segment
        # = height of TF coil inner edge + (2 * coil thickness)
        tfcoil_variables.cplen = (2.0e0 * build_variables.hmax) + (2.0e0 * build_variables.tfcth)

        # The 2.2 factor is used as a scaling factor to fit
        # to the ITER-FDR value of 450 tonnes; see CCFE note T&M/PKNIGHT/PROCESS/026
        if ( physics_variables.itart == 1 ) :
            # tfcoil_variables.tfleng does not include inboard leg ('centrepost') length in TART
            tfcoil_variables.whtcas = 2.2e0 * tfcoil_variables.dcase * (tfcoil_variables.cplen * tfcoil_variables.acasetf + tfcoil_variables.tfleng * tfcoil_variables.acasetfo)
        else:
            tfcoil_variables.whtcas = 2.2e0 * tfcoil_variables.dcase * (tfcoil_variables.cplen * tfcoil_variables.acasetf + (tfcoil_variables.tfleng-tfcoil_variables.cplen) * tfcoil_variables.acasetfo)

        # ***


        # Masses of conductor constituents
        #---------------------------------
        # Superconductor mass [kg]
        # Includes space allowance for central helium channel, area tfcoil_variables.awphec
        tfcoil_variables.whtconsc = (tfcoil_variables.tfleng * tfcoil_variables.n_tf_turn * tfcoil_variables.acstf*(1.0e0-tfcoil_variables.vftf) *                    (1.0e0-tfcoil_variables.fcutfsu) - tfcoil_variables.tfleng*tfcoil_variables.awphec) * tfcoil_variables.dcond(tfcoil_variables.i_tf_sc_mat)

        # Copper mass [kg]
        tfcoil_variables.whtconcu = (tfcoil_variables.tfleng * tfcoil_variables.n_tf_turn * tfcoil_variables.acstf*(1.0e0-tfcoil_variables.vftf) * tfcoil_variables.fcutfsu - tfcoil_variables.tfleng*tfcoil_variables.awphec) * dcopper
        if ( tfcoil_variables.whtconcu <= 0.0e0 ) tfcoil_variables.whtconcu = 0.0e0

        # Steel conduit (sheath) mass [kg]
        tfcoil_variables.whtconsh = tfcoil_variables.tfleng * tfcoil_variables.n_tf_turn * tfcoil_variables.acndttf * fwbs_variables.denstl

        # Conduit insulation mass [kg]
        # (tfcoil_variables.aiwp already contains tfcoil_variables.n_tf_turn)
        tfcoil_variables.whtconin = tfcoil_variables.tfleng * tfcoil_variables.aiwp * tfcoil_variables.dcondins

        # Total conductor mass [kg]
        tfcoil_variables.whtcon = tfcoil_variables.whtconsc + tfcoil_variables.whtconcu + tfcoil_variables.whtconsh + tfcoil_variables.whtconin
        #---------------------------------

        # Total TF coil mass [kg] (all coils)
        tfcoil_variables.whttf = (tfcoil_variables.whtcas + tfcoil_variables.whtcon + tfcoil_variables.whtgw) * tfcoil_variables.n_tf

        # If spherical tokamak, distribute between centrepost and outboard legs
        # (in this case, total TF coil length = inboard `cplen` + outboard `tfleng`)
        if ( physics_variables.itart == 1 ) :
            tfcoil_variables.tfleng_sph = tfcoil_variables.cplen + tfcoil_variables.tfleng
            tfcoil_variables.whtcp = tfcoil_variables.whttf * (tfcoil_variables.cplen / tfcoil_variables.tfleng_sph)
            tfcoil_variables.whttflgs = tfcoil_variables.whttf * (tfcoil_variables.tfleng / tfleng_sph)


    # Resitivive magnets weights
    # ---
    # Rem SK : No casing for the outboard leg is considered for now #
    else:

        # Volumes
        # -------
        # CP with joints
        # ---
        if ( physics_variables.itart == 1 ) :

            # Total volume of one outerleg [m3]
            tfcoil_variables.voltfleg = tfcoil_variables.tfleng * tfcoil_variables.arealeg

            # Outboard leg TF conductor volume [m3]
            vol_cond_leg = tfcoil_variables.tfleng * a_leg_cond

            # Total TF conductor volume [m3]
            vol_cond = tfcoil_variables.vol_cond_cp + tfcoil_variables.n_tf * vol_cond_leg

            # Outboard leg TF turn insulation layer volume (per leg) [m3]
            vol_ins_leg = tfcoil_variables.tfleng * a_leg_ins

            # Total turn insulation layer volume [m3]
            vol_ins = vol_ins_cp + tfcoil_variables.n_tf * vol_ins_leg

            # Ouboard leg TF ground insulation layer volume (per leg) [m3]
            vol_gr_ins_leg = tfcoil_variables.tfleng * a_leg_gr_ins

            # Total ground insulation layer volume [m3]
            vol_gr_ins = vol_gr_ins_cp + tfcoil_variables.n_tf * vol_gr_ins_leg

            # Total volume of the CP casing [m3]
            # Rem : no outer leg case
            vol_case = vol_case_cp

        # No joints
        # ---
        else:
            # Total TF outer leg conductor volume [m3]
            vol_cond = tfcoil_variables.tfleng * a_leg_cond * tfcoil_variables.n_tf

            # Total turn insulation layer volume [m3]
            vol_ins = tfcoil_variables.tfleng * a_leg_ins * tfcoil_variables.n_tf

            # Total ground insulation volume [m3]
            vol_gr_ins = tfcoil_variables.tfleng * a_leg_gr_ins * tfcoil_variables.n_tf

            # Total case volume [m3]
            vol_case = tfcoil_variables.tfleng * tfcoil_variables.acasetf * tfcoil_variables.n_tf

        # ---
        # -------


        # Copper magnets casing/conductor weights per coil [kg]
        if ( tfcoil_variables.i_tf_sup == 0 ) :

            tfcoil_variables.whtcas = fwbs_variables.denstl * vol_case / tfcoil_variables.n_tf  # Per TF leg, no casing for outer leg
            tfcoil_variables.whtconcu = dcopper * vol_cond / tfcoil_variables.n_tf
            tfcoil_variables.whtconal = 0.0e0

            # Outer legs/CP weights
            if ( physics_variables.itart == 1 ) :

                # Weight of all the TF legs
                tfcoil_variables.whttflgs = tfcoil_variables.n_tf * ( dcopper * vol_cond_leg                                   + tfcoil_variables.dcondins * ( vol_ins_leg + vol_gr_ins_leg ) )

                # CP weight
                tfcoil_variables.whtcp = dcopper * tfcoil_variables.vol_cond_cp                       + tfcoil_variables.dcondins * ( vol_ins_cp + vol_gr_ins_cp )                       + vol_case_cp * fwbs_variables.denstl


        # Cryo-aluminium conductor weights
        # Casing made of re-inforced aluminium alloy
        elif  ( tfcoil_variables.i_tf_sup == 2 ) :

            # Casing weight (CP only if physics_variables.itart = 1)bper leg/coil
            tfcoil_variables.whtcas = dalu * vol_case / tfcoil_variables.n_tf
            tfcoil_variables.whtconcu = 0.0e0
            tfcoil_variables.whtconal = dalu * vol_cond / tfcoil_variables.n_tf

            # Outer legs/CP weights
            if ( physics_variables.itart == 1 ) :

                # Weight of all the TF legs
                tfcoil_variables.whttflgs = tfcoil_variables.n_tf * ( dalu * vol_cond_leg                                   + tfcoil_variables.dcondins * ( vol_ins_leg + vol_gr_ins_leg ) )

                # CP weight
                tfcoil_variables.whtcp = dalu * tfcoil_variables.vol_cond_cp                       + tfcoil_variables.dcondins * ( vol_ins_cp + vol_gr_ins_cp )                       + vol_case_cp * fwbs_variables.denstl



        # Turn insulation mass [kg]
        tfcoil_variables.whtconin = tfcoil_variables.dcondins * vol_ins / tfcoil_variables.n_tf

        # Ground wall insulation layer weight
        tfcoil_variables.whtgw = tfcoil_variables.dcondins * vol_gr_ins / tfcoil_variables.n_tf

        # Total weight
        tfcoil_variables.whttf = (tfcoil_variables.whtcas + tfcoil_variables.whtconcu + tfcoil_variables.whtconal + tfcoil_variables.whtconin + tfcoil_variables.whtgw ) * tfcoil_variables.n_tf


    # ---

    #-# end break


    
    def tf_case_geom(self):
        """
         Author : S. Kahn, CCFE
 Seting the case geometry and area for SC magnets
        """
                tfcoil_variables.acasetf = (tfcoil_variables.tfareain / tfcoil_variables.n_tf) - awpc

        # Outboard leg cross-sectional area of surrounding case [m2]
        tfcoil_variables.acasetfo = tfcoil_variables.arealeg - awpc

        # Front casing area [m2]
        if ( i_tf_case_geom == 0 ) :

            # Circular front case
            a_case_front = theta_coil * build_variables.r_tf_inboard_out**2                          - tan_theta_coil * r_wp_outer**2
        else:

            # Straight front case
            a_case_front = ( (r_wp_outer + tfcoil_variables.casthi)**2 - r_wp_outer**2 ) * tan_theta_coil



        # Nose casing area [m2]
        a_case_nose = tan_theta_coil * r_wp_inner**2                     - theta_coil * build_variables.r_tf_inboard_in**2

        # Report error if the casing area is negative
        if ( tfcoil_variables.acasetf <= 0.0e0 .or. tfcoil_variables.acasetfo <= 0.0e0 ) :
            fdiags(1) = tfcoil_variables.acasetf
            fdiags(2) = tfcoil_variables.acasetfo
            call report_error(99)


        # Average lateral casing thickness
        # --------------
        # Rectangular casing
        if ( i_tf_wp_geom == 0 ) :
            t_lat_case_av = tfcoil_variables.casths + 0.5e0*tan_theta_coil * tfcoil_variables.dr_tf_wp

        # Double rectangular WP
        elif  ( i_tf_wp_geom == 1 ) :
            t_lat_case_av = tfcoil_variables.casths + 0.25e0*tan_theta_coil * tfcoil_variables.dr_tf_wp

        # Trapezoidal WP
        else:
            t_lat_case_av = tfcoil_variables.casths

        # --------------

        #-# end break
    
    
    def init_sctfcoil_module(self):
        """
         Initialise module variables
        """
            is_leg_cp_temp_same = 0
    tf_fit_t = 0.0e0
    tf_fit_z = 0.0e0
    tf_fit_y = 0.0e0
    tfc_current = 0.0e0
    awpc = 0.0e0
    awptf = 0.0e0
    a_tf_steel = 0.0e0
    a_tf_ins = 0.0e0
    f_tf_steel = 0.0e0
    f_tf_ins = 0.0e0
    h_cp_top = 0.0e0
    r_tf_outboard_in = 0.0e0
    r_tf_outboard_out = 0.0e0
    r_wp_inner = 0.0e0
    r_wp_outer = 0.0e0
    r_wp_centre = 0.0e0
    dr_tf_wp_top = 0.0e0
    vol_ins_cp = 0.0e0
    vol_gr_ins_cp = 0.0e0
    vol_case_cp = 0.0e0
    t_wp_toroidal = 0.0e0
    t_wp_toroidal_av = 0.0e0
    t_lat_case_av = 0.0e0
    a_case_front = 0.0e0
    a_case_nose = 0.0e0
    a_ground_ins = 0.0e0
    a_leg_ins = 0.0e0
    a_leg_gr_ins = 0.0e0
    a_leg_cond = 0.0e0
    theta_coil = 0.0e0
    tan_theta_coil = 0.0e0
    t_conductor_radial = 0.0e0
    t_conductor_toroidal = 0.0e0
    t_cable_radial = 0.0e0
    t_cable_toroidal = 0.0e0
    t_turn_radial = 0.0e0
    t_turn_toroidal = 0.0e0
    t_cable = 0.0e0
    vforce_inboard_tot = 0.0e0
    T1 = 0.0e0
    time2 = 0.0e0
    tau2 = 0.0e0
    estotft = 0.0e0
  
    
    def tf_wp_currents(self):
        """
         Author : S. Kahn, CCFE
 Turn engineering turn currents/densities
        """
                tfcoil_variables.jwptf = max(1.0e0, tfcoil_variables.ritfc/(tfcoil_variables.n_tf*awptf))

        #-# end break

    
    
    def generalized_plane_strain(self):
        """
        
        """
            nu_z_eff2 = nu_z**2 * ey_p / ey_z

    # Stress to strain coeficients (array equation)
    kk_p = ey_p / ( 1.0e0 - nu_p - 2.0e0*nu_z_eff2 )
    kk_z = ey_z / ( 1.0e0 - nu_p - 2.0e0*nu_z_eff2 )

    # Body force parameter in displacement differential equation (array equation)
    fr_par = ( 1.0e0 + nu_p ) / ( kk_p * ( 1.0e0 - nu_z_eff2 ) )

    # Lorentz forces parametrisation coeficients (array equation)
    alpha = 0.5e0*rmu0 * d_curr**2 * fr_par

    inner_layer_curr = 0.0e0
    do ii = 1, nlayers

        beta(ii) = 0.5e0*rmu0 * d_curr(ii) * fr_par(ii)                  * ( inner_layer_curr - pi*d_curr(ii)*rad(ii)**2 ) / pi

        # Layer area
        area(ii) = pi * (rad(ii+1)**2 - rad(ii)**2)

        # Total current carried by the inners layers
        inner_layer_curr = inner_layer_curr + area(ii)*d_curr(ii)
    end do

    # Constant radial/toroidal stress parameters associated cc, alpha/8 and beta/2
    #-#
    # array equations
    cc_par_sig = ( nu_p - 1.0e0 + 2.0e0*nu_z_eff2 ) / ( 1.0e0 + nu_p )
    alpha_par_sigr = ( 3.0e0 + 1.0e0*nu_p - 2.0e0*nu_z_eff2 ) / ( 1.0e0 + nu_p )
    alpha_par_sigt = ( 1.0e0 + 3.0e0*nu_p + 2.0e0*nu_z_eff2 ) / ( 1.0e0 + nu_p )
    beta_par_sigr = ( 1.0e0 - nu_z_eff2 ) / ( 1.0e0 + nu_p )
    beta_par_sigt = ( nu_p  + nu_z_eff2 ) / ( 1.0e0 + nu_p )
    #-#


    # Plain strain generalisation parameters
    # Rem : if i_tf_bucking >= 2, the CS is used as a TF support structure
    #       with vertical strain insulation. If so, two vertical boundary
    #       conditions (strain generalization) must be considered.
    #-#
    # Cylindrical integrals parameters
    do ii = 1, nlayers
        par_1(ii) = pi * (rad(ii+1)**4 - rad(ii)**4)
        par_2(ii) = pi * (log(rad(ii+1)) * rad(ii+1)**2 - log(rad(ii)) * rad(ii)**2)
    end do

    # CS layer parameter
    # Rem : no CS vertical tension (uncoupled & CS flux swing)
    sum_1 = 0.0e0
    sum_2 = 0.0e0
    aleph(:) = 0.0e0
    beth(:) = 0.0e0
    if ( i_tf_bucking >= 2 ) :
        do ii = 1, i_tf_bucking - 1
            sum_1 = sum_1 + kk_z(ii) * ( 1.0e0 - nu_p(ii) ) * area(ii)
        end do
        do ii = 1, i_tf_bucking - 1
            beth(ii) = - ( 2.0e0 * area(ii) * kk_z(ii) * nu_z_eff2(ii) / nu_z(ii) ) / sum_1
        end do


    # TF coil layers parameters
    sum_1 = 0.0e0
    sum_2 = 0.0e0
    do ii = max( 1, i_tf_bucking ), nlayers
        sum_1 = sum_1 + kk_z(ii) * ( 1.0e0 - nu_p(ii) ) * area(ii)
        sum_2 = sum_2 + kk_z(ii) * ( nu_z_eff2(ii)/nu_z(ii) )                                  * ( 0.25e0 * alpha(ii) * par_1(ii) + beta(ii) * par_2(ii) )
    end do

    # TF bucking/nose casing layer
    do ii = max( 1, i_tf_bucking ), nlayers
        aleph(ii) = (v_force - sum_2) / sum_1
        beth(ii) = - ( 2.0e0 * area(ii) * kk_z(ii) * nu_z_eff2(ii) / nu_z(ii) ) / sum_1
    end do
    #-#
    # ***


    # Left hand side matrix aa
    # ***
    aa(:,:) = 0.0e0

    # Null radial stress at R(1)
    aa(1,1) = kk_p(1)
    aa(1,2) = kk_p(1) * cc_par_sig(1) / rad(1)**2

    # Free standing TF system plain strain generalisation
    if ( i_tf_bucking <= 1 ) : # Free standing TF case
        do jj = 1, nlayers
            aa(1, 2*jj-1) = aa(1, 2*jj-1) + beth(jj)*kk_p(1)*nu_z(1)
        end do

    # CS system plane strain generalization
    elif  ( i_tf_bucking >= 2 ) : # TF case bucked on CS
        do jj = 1, i_tf_bucking - 1
            aa(1, 2*jj-1) = aa(1, 2*jj-1) + beth(jj)*kk_p(1)*nu_z(1)
        end do


    # Inter-layer boundary conditions
    if ( nlayers >= 2 ) :

        # Plane strain
        do ii = 1, nlayers - 1

            # Continuous radial normal stress at R(ii+1)
            aa(2*ii, 2*ii-1) = kk_p(ii)
            aa(2*ii, 2*ii  ) = kk_p(ii) * cc_par_sig(ii) / rad(ii+1)**2
            aa(2*ii, 2*ii+1) = -kk_p(ii+1)
            aa(2*ii, 2*ii+2) = -kk_p(ii+1) * cc_par_sig(ii+1) / rad(ii+1)**2

            # Continuous displacement at R(ii+1)
            aa(2*ii+1, 2*ii-1) = rad(ii+1)
            aa(2*ii+1, 2*ii  ) = 1.0e0 / rad(ii+1)
            aa(2*ii+1, 2*ii+1) = -rad(ii+1)
            aa(2*ii+1, 2*ii+2) = -1.0e0 / rad(ii+1)
        end do

        # Free standing TF plain strain generalisation
        if ( i_tf_bucking <= 1 ) : #
            do ii = 1, nlayers - 1
                do jj = 1, nlayers
                    aa(2*ii, 2*jj-1) = aa(2*ii, 2*jj-1)                                      + beth(jj)*(kk_p(ii)*nu_z(ii) - kk_p(ii+1)*nu_z(ii+1))
                end do
            end do

        # TF case bucked on CS
        elif  ( i_tf_bucking == 2 ) :

            # Layer 1-2 interface (vertical strain decoupled)
            aa(2,1) = aa(2,1) + beth(1) * kk_p(1) * nu_z(1)
            do jj = 2, nlayers
                aa(2, 2*jj-1) = aa(2, 2*jj-1) - beth(jj) * kk_p(2)*nu_z(2)
            end do

            # Remaining TF interfaces
            do ii = 2, nlayers - 1
                do jj = 2, nlayers
                    aa(2*ii, 2*jj-1) = aa(2*ii, 2*jj-1)                                      + beth(jj)*(kk_p(ii)*nu_z(ii) - kk_p(ii+1)*nu_z(ii+1))
                end do
            end do

        # TF case bucked on CS with TF-CS interlayer
        elif  ( i_tf_bucking == 3 ) :

            # Layer 1-2 interface
            do jj = 1, 2
                aa(2, 2*jj-1) = aa(2, 2*jj-1) + beth(jj) * ( kk_p(1)*nu_z(1) - kk_p(2)*nu_z(2) )
            end do

            # Layer 2-3 interface (vertical strain decoupled)
            do jj = 1, 2
                aa(4, 2*jj-1) = aa(4, 2*jj-1) + beth(jj) * kk_p(2) * nu_z(2)
            end do
            do jj = 3, nlayers
                aa(4, 2*jj-1) = aa(4, 2*jj-1) - beth(jj) * kk_p(3) * nu_z(3)
            end do

            # Remaining TF interfaces
            do ii = 3, nlayers - 1
                do jj = 3, nlayers
                    aa(2*ii, 2*jj-1) = aa(2*ii, 2*jj-1)                                      + beth(jj)*(kk_p(ii)*nu_z(ii) - kk_p(ii+1)*nu_z(ii+1))
                end do
            end do



    # Null radial stress at outermost radius at R(nlayers+1)
    aa(2*nlayers, 2*nlayers - 1) = kk_p(nlayers)
    aa(2*nlayers, 2*nlayers    ) = kk_p(nlayers) * cc_par_sig(nlayers) / rad(nlayers+1)**2

    # Plain strain generalisation
    do jj = max(i_tf_bucking, 1), nlayers
        aa(2*nlayers, 2*jj-1) = aa(2*nlayers, 2*jj-1) + beth(jj)*kk_p(nlayers)*nu_z(nlayers)
    end do
    # ***

    # Right hand side vector bb
    # ***
    # Null radial stress at R(1)
    bb(1) = -kk_p(1) * ( 0.125e0*alpha(1) * rad(1)**2 * alpha_par_sigr(1)                        + 0.5e0*beta(1) * ( beta_par_sigr(1) + log(rad(1)) )                        + nu_z(1)*aleph(1) ) # Plain strain generalisation

    # Inter-layer boundary conditions
    if ( nlayers /= 1 ) :
        do ii = 1, nlayers - 1

            # Continuous radial normal stress at R(ii+1)
            bb(2*ii) = - kk_p(ii) * ( 0.125e0*alpha(ii) * rad(ii+1)**2 * alpha_par_sigr(ii)                                     + 0.5e0*beta(ii) * ( beta_par_sigr(ii) + log(rad(ii+1)) )                                     + aleph(ii) * nu_z(ii) )        & # Plain strain generalisation line
                       + kk_p(ii+1) * ( 0.125e0*alpha(ii+1) * rad(ii+1)**2 * alpha_par_sigr(ii+1)                                       + 0.5e0*beta(ii+1) * ( beta_par_sigr(ii+1) + log(rad(ii+1)))                                       + aleph(ii+1) * nu_z(ii+1) )    # Plain strain generalisation line

            # Continuous displacement at R(ii+1)
            bb(2*ii+1) = - 0.125e0*alpha(ii) * rad(ii+1)**3 - 0.5e0*beta(ii) * rad(ii+1)*log(rad(ii+1))                          + 0.125e0*alpha(ii+1)* rad(ii+1)**3 + 0.5e0*beta(ii+1)*rad(ii+1)*log(rad(ii+1))

        end do


    # Null radial stress at R(nlayers+1)
    bb(2*nlayers) = -kk_p(nlayers) * ( 0.125e0*alpha(nlayers)*rad(nlayers+1)**2 * alpha_par_sigr(nlayers)                                      + 0.5e0*beta(nlayers) * (beta_par_sigr(nlayers) + log(rad(nlayers+1)) )                                      + nu_z(nlayers)*aleph(nlayers) )   # Plain strain generalisation
    # ***

    #  Find solution vector cc
    # ***
    cc(:) = 0.0e0
    call maths_library.linesolv(aa, 2*nlayers, bb, cc)

    do ii = 1, nlayers
        c1(ii) = cc(2*ii-1)
        c2(ii) = cc(2*ii)
    end do
    # ***
    # ------


    # Radial/toroidal/vertical stress radial distribution
    # ------
    rradius(:) = 0.0e0
    sigr(:) = 0.0e0
    sigt(:) = 0.0e0
    sigz(:) = 0.0e0
    str_r(:) = 0.0e0
    str_t(:) = 0.0e0
    str_z(:) = 0.0e0
    r_deflect(:) = 0.0e0

    # CS system vertical strain
    if ( i_tf_bucking >= 2 ) :
        str_z_calc(1) = aleph(1)
        do ii = 1, i_tf_bucking - 1
            str_z_calc(1) = str_z_calc(1) + c1(ii) * beth(ii)
        end do


    # TF system vertical normal strain (constant) WRONG IF GRADED COIL
    str_z_calc(2) = aleph(nlayers)
    do ii = max( 1, i_tf_bucking ), nlayers
        str_z_calc(2) = str_z_calc(2) + c1(ii) * beth(ii)
    end do


    # Radial displacement, stress and strain distributions
    do ii = 1, nlayers

        dradius = (rad(ii+1) - rad(ii)) / dble(n_radial_array - 1 )
        do jj = (ii-1)*n_radial_array + 1, ii*n_radial_array

            rradius(jj) = rad(ii) + dradius*dble(jj - n_radial_array*(ii-1) - 1)

            # Radial normal stress
            sigr(jj) = kk_p(ii)*( c1(ii) + cc_par_sig(ii)*c2(ii)/rradius(jj)**2                                 + 0.125e0*alpha(ii)*alpha_par_sigr(ii)*rradius(jj)**2                                 + 0.5e0*beta(ii)*(beta_par_sigr(ii) + log(rradius(jj)) ) )

            # Toroidal normal stress
            sigt(jj) = kk_p(ii)*( c1(ii) - cc_par_sig(ii)*c2(ii)/rradius(jj)**2                                 + 0.125e0*alpha(ii)*alpha_par_sigt(ii)*rradius(jj)**2                                 + 0.5e0*beta(ii)*(beta_par_sigt(ii) + log(rradius(jj)) ) )

            # Vertical normal stress
            sigz(jj) = kk_z(ii) * nu_z_eff2(ii) / nu_z(ii)                      * ( 2.0e0*c1(ii) + 0.5e0*alpha(ii) * rradius(jj)**2                        + 0.5e0*beta(ii) * ( 1.0e0 + 2.0e0*log(rradius(jj)) ) )

            # No vertical strain effect on CS / TF-CS inter layer
            if ( ii >= i_tf_bucking ) : # TF system
                sigr(jj) = sigr(jj) + kk_p(ii) * str_z_calc(2) * nu_z(ii)
                sigt(jj) = sigt(jj) + kk_p(ii) * str_z_calc(2) * nu_z(ii)
                sigz(jj) = sigz(jj) + kk_z(ii) * str_z_calc(2) * (1.0e0 - nu_p(ii))
            else: # CS system
                sigr(jj) = sigr(jj) + kk_p(ii) * str_z_calc(1) * nu_z(ii)
                sigt(jj) = sigt(jj) + kk_p(ii) * str_z_calc(1) * nu_z(ii)
                sigz(jj) = sigz(jj) + kk_z(ii) * str_z_calc(1) * (1.0e0 - nu_p(ii))


            # Radial normal strain
            str_r(jj) = c1(ii) - c2(ii) / rradius(jj)**2                            + 0.375e0*alpha(ii) * rradius(jj)**2 + 0.5e0*beta(ii)                            * (1.0e0 + log(rradius(jj)))

            # Toroidal normal strain
            str_t(jj) = c1(ii) + c2(ii) / rradius(jj)**2                             + 0.125e0*alpha(ii) * rradius(jj)**2 + 0.5e0*beta(ii)                             * log(rradius(jj))

            # Vertical normal strain
            if ( ii >= i_tf_bucking ) :
                str_z(jj) = str_z_calc(2)
            else:
                str_z(jj) = str_z_calc(1)


            # Radial displacement
            r_deflect(jj) = c1(ii)*rradius(jj) + c2(ii)/rradius(jj)                             + 0.125e0*alpha(ii) * rradius(jj)**3                             + 0.5e0*beta(ii) * rradius(jj)*log(rradius(jj))

        end do # layer array loop
    end do # Layer loop
    # ------

    #-# end break


    
    def eyoung_t_nested_squares(self):
        """
        
        """
            eyoung_j_working = 0
    l_working = 0
    poisson_j_perp_working = 0
    eyoung_j_out = 0
    l_out = 0
    poisson_j_perp_out = 0
    eyoung_stiffest = 0

    # First member
    eyoung_j_working(1) = eyoung_j_in(1)
    l_working(1) = l_in(1)
    poisson_j_perp_working(1) = poisson_j_perp_in(1)

    do ii = 2,n
      # Initialize the leg of which this is the new member
      eyoung_j_working(ii) = eyoung_j_in(ii)
      l_working(ii) = l_working(ii-1) + l_in(ii)
      poisson_j_perp_working(ii) = poisson_j_perp_in(ii)

      # Serial-composite the new layer of this member into the previous legs
      do jj = 1,(ii-1)
        call eyoung_series(eyoung_j_working(ii),l_in(ii),poisson_j_perp_working(ii),                 eyoung_j_working(jj),l_working(jj),poisson_j_perp_working(jj),                 eyoung_j_working(jj),l_working(jj),poisson_j_perp_working(jj))
      end do
    end do

    # Find stiffest leg
    do ii = 1,n
      if (eyoung_stiffest < eyoung_j_working(ii)) :
        eyoung_stiffest = eyoung_j_working(ii)

    end do

    # Parallel-composite them all together
    do ii = 1,n
      call eyoung_parallel(eyoung_j_working(ii),l_in(ii),poisson_j_perp_working(ii),                 eyoung_j_out,l_out,poisson_j_perp_out,                 eyoung_j_out,l_out,poisson_j_perp_out)
    end do

    #-# end break


    
    def eyoung_parallel_array(self):
        """
        
        """
            eyoung_j_out = 0
    a_out = 0
    poisson_j_perp_out = 0

    # Parallel-composite them all together
    do ii = 1,n
      call eyoung_parallel(eyoung_j_in(ii),a_in(ii),poisson_j_perp_in(ii),                 eyoung_j_out,a_out,poisson_j_perp_out,                 eyoung_j_out,a_out,poisson_j_perp_out)
    end do

    #-# end break


    
    def eyoung_series(self):
        """
        
        """
            if ( eyoung_j_1*eyoung_j_2 == 0 ) :
      #poisson_j_perp_3 = 0
      if ( eyoung_j_1 == 0) :
        poisson_j_perp_3 = poisson_j_perp_1
      else:
        poisson_j_perp_3 = poisson_j_perp_2 #

      eyoung_j_3 = 0
      l_3 = l_1 + l_2
    else:
      poisson_j_perp_3 = (poisson_j_perp_1*l_1/eyoung_j_1 + poisson_j_perp_2*l_2/eyoung_j_2) / (l_1/eyoung_j_1 + l_2/eyoung_j_2)
      eyoung_j_3 = (l_1 + l_2) / (l_1/eyoung_j_1 + l_2/eyoung_j_2)
      l_3 = l_1 + l_2



    #-# end break


    
    def sc_tf_internal_geom(self):
        """
         Author : S. Kahn, CCFE
 Seting the WP, case and tunrs geometry for SC magnets
        """
            call tf_wp_geom(i_tf_wp_geom)

    # Calculating the TF steel casing areas
    call tf_case_geom(i_tf_wp_geom, i_tf_case_geom)

    # WP/trun currents
    call tf_wp_currents

    # Setting the WP turn geometry / areas
    if ( i_tf_turns_integer == 0 ) :
        # Non-ingeger number of turns
        call tf_averaged_turn_geom( tfcoil_variables.jwptf, tfcoil_variables.thwcndut, tfcoil_variables.thicndut, tfcoil_variables.i_tf_sc_mat, &     # Inputs
                                    tfcoil_variables.acstf, tfcoil_variables.acndttf, tfcoil_variables.insulation_area, tfcoil_variables.n_tf_turn )  # Outputs
    else:
        # Integer number of turns
        call tf_integer_turn_geom( tfcoil_variables.n_layer, tfcoil_variables.n_pancake, tfcoil_variables.thwcndut, tfcoil_variables.thicndut, & # Inputs
                                   tfcoil_variables.acstf, tfcoil_variables.acndttf, tfcoil_variables.insulation_area, &        # Outputs
                                   tfcoil_variables.cpttf, tfcoil_variables.n_tf_turn )                        # Outputs



    # Areas and fractions
    # -------------------
    # Central helium channel down the conductor core [m2]
    tfcoil_variables.awphec = 0.25e0 * tfcoil_variables.n_tf_turn * pi*tfcoil_variables.dhecoil**2

    # Total conductor cross-sectional area, taking account of void area
    # and central helium channel [m2]
    tfcoil_variables.acond = tfcoil_variables.acstf * tfcoil_variables.n_tf_turn * (1.0e0-tfcoil_variables.vftf) - tfcoil_variables.awphec

    # Void area in conductor for He, not including central channel [m2]
    tfcoil_variables.avwp = tfcoil_variables.acstf * tfcoil_variables.n_tf_turn * tfcoil_variables.vftf

    # Area of inter-turn insulation: total [m2]
    tfcoil_variables.aiwp = tfcoil_variables.n_tf_turn * tfcoil_variables.insulation_area

    # Area of steel structure in winding pack [m2]
    tfcoil_variables.aswp = tfcoil_variables.n_tf_turn * tfcoil_variables.acndttf

    # Inboard coil steel area [m2]
    a_tf_steel = tfcoil_variables.acasetf + tfcoil_variables.aswp

    # Inboard coil steel fraction [-]
    f_tf_steel = tfcoil_variables.n_tf * a_tf_steel / tfcoil_variables.tfareain

    # Inboard coil insulation cross-section [m2]
    a_tf_ins = tfcoil_variables.aiwp + a_ground_ins

    #  Inboard coil insulation fraction [-]
    f_tf_ins = tfcoil_variables.n_tf * a_tf_ins / tfcoil_variables.tfareain

    # Negative areas or fractions error reporting
    if ( tfcoil_variables.acond <= 0.0e0 .or. tfcoil_variables.avwp <= 0.0e0 .or. tfcoil_variables.aiwp <= 0.0e0 .or.          tfcoil_variables.aswp <= 0.0e0 .or. a_tf_steel <= 0.0e0 .or. f_tf_steel <= 0.0e0 .or.          a_tf_ins <= 0.0e0 .or. f_tf_ins <= 0.0e0 ) :
        fdiags(1) = tfcoil_variables.acond
        fdiags(2) = tfcoil_variables.avwp
        fdiags(3) = tfcoil_variables.aiwp
        fdiags(4) = tfcoil_variables.aswp
        fdiags(5) = a_tf_steel
        fdiags(6) = f_tf_steel
        fdiags(7) = a_tf_ins
        fdiags(8) = f_tf_ins
        call report_error(276)

    # -------------------


    
    def res_tf_internal_geom(self):
        """
         Author : S. Kahn
 Resisitve TF turn geometry, equivalent to winding_pack subroutines
        """
            r_wp_inner = build_variables.r_tf_inboard_in  + tfcoil_variables.thkcas
    r_wp_outer = build_variables.r_tf_inboard_out - tfcoil_variables.casthi

    # Conductor layer radial thickness at centercollumn top [m]
    if ( physics_variables.itart == 1 ) :
        tfcoil_variables.dr_tf_wp_top = build_variables.r_cp_top - tfcoil_variables.casthi - tfcoil_variables.thkcas - build_variables.r_tf_inboard_in


    # Number of turns
    # Set by user (no turn structure by default, i.e. tfcoil_variables.n_tf_turn = 1 )
    if ( abs(tfcoil_variables.n_tf_turn) < epsilon(tfcoil_variables.n_tf_turn) ) tfcoil_variables.n_tf_turn = 1.0e0

    # Total mid-plane cross-sectional area of winding pack, [m2]
    # including the surrounding ground-wall insulation layer
    awpc = pi * ( r_wp_outer**2 - r_wp_inner**2 ) / tfcoil_variables.n_tf

    # Area of the front case, the plasma-facing case of the inner TF coil [m2]
    a_case_front = pi * ( (r_wp_outer + tfcoil_variables.casthi)**2 - r_wp_outer**2 ) / tfcoil_variables.n_tf

    # WP mid-plane cross-section excluding ground insulation per coil [m2]
    awptf = pi * ( ( r_wp_outer - tfcoil_variables.tinstf )**2 - ( r_wp_inner + tfcoil_variables.tinstf )**2 ) / tfcoil_variables.n_tf           - 2.0e0 * tfcoil_variables.tinstf * ( tfcoil_variables.dr_tf_wp - 2.0e0 * tfcoil_variables.tinstf )

    # Ground insulation cross-section area per coil [m2]
    a_ground_ins = awpc - awptf

    # Exact mid-plane cross-section area of the conductor per TF coil [m2]
    a_tf_cond = pi * ( ( r_wp_outer - tfcoil_variables.tinstf - tfcoil_variables.thicndut )**2                      - ( r_wp_inner + tfcoil_variables.tinstf + tfcoil_variables.thicndut )**2 ) / tfcoil_variables.n_tf               - ( tfcoil_variables.dr_tf_wp - 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.thicndut ) )               * 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.thicndut * tfcoil_variables.n_tf_turn )
    a_tf_cond = a_tf_cond * ( 1.0e0 - tfcoil_variables.fcoolcp )

    # Inter turn insulation area per coil [m2]
    tfcoil_variables.aiwp = awptf - a_tf_cond / ( 1.0e0 - tfcoil_variables.fcoolcp )

    # Total insulation cross-section per coil [m2]
    a_tf_ins = tfcoil_variables.aiwp + a_ground_ins

    # Insulation fraction [-]
    f_tf_ins = tfcoil_variables.n_tf * a_tf_ins / tfcoil_variables.tfareain

    # Total cross-sectional area of the bucking cylindre and the outer support
    # support structure per coil [m2]
    # physics_variables.itart = 1 : Only valid at mid-plane
    tfcoil_variables.acasetf = ( tfcoil_variables.tfareain / tfcoil_variables.n_tf ) - awpc

    # Current per turn
    tfcoil_variables.cpttf = tfcoil_variables.ritfc / ( tfcoil_variables.n_tf_turn * tfcoil_variables.n_tf )

    # Exact current density on TF oubard legs
    tfcoil_variables.cdtfleg = tfcoil_variables.ritfc / ( ( 1.0e0 - tfcoil_variables.fcoolcp )                       * ( tfcoil_variables.tftort - 2.0e0 * ( tfcoil_variables.n_tf_turn * tfcoil_variables.thicndut + tfcoil_variables.tinstf ) )                       * ( build_variables.tfthko - 2.0e0 * ( tfcoil_variables.thicndut + tfcoil_variables.tinstf ) ) )

    # Reporting negative WP areas issues
    if ( awpc < 0.0e0 ) :
        fdiags(1) = awpc
        fdiags(1) = tfcoil_variables.dr_tf_wp
        call report_error(99)

    elif  ( awptf < 0.0e0 ) :
        fdiags(1) = awptf
        call report_error(101)


    ### end break



    
    def out_stress(self):
        """
         Subroutine showing the writing the TF midplane stress analysis
 in the output file and the stress distribution in the SIG_TF.DAT
 file used to plot stress distributions
 Author : S. Kahn
        """
                character(len=1) :: intstring
        integer :: ii
        ## Char used for integer convertion to string

        # Stress output section
        call oheadr(outfile,'TF coils ')
        call osubhd(outfile,'TF Coil Stresses (CCFE model) :')

        if ( tfcoil_variables.i_tf_stress_model == 1 ) :
            call ocmmnt(outfile, 'Plane stress model with smeared properties')
        else:
            call ocmmnt(outfile, 'Generalized plane strain model')


        call ovarre(outfile, 'Allowable maximum shear stress in TF coil case (Tresca criterion) (Pa)',         '(sig_tf_case_max)',sig_tf_case_max)

        call ovarre(outfile, 'Allowable maximum shear stress in TF coil conduit (Tresca criterion) (Pa)',         '(sig_tf_wp_max)',sig_tf_wp_max)
        if ( tfcoil_variables.i_tf_tresca == 1  .and. tfcoil_variables.i_tf_sup == 1) :
            call ocmmnt(outfile, 'WP conduit Tresca criterion corrected using CEA formula (tfcoil_variables.i_tf_tresca = 1)')


        if ( tfcoil_variables.i_tf_bucking >= 3) :
            call ocmmnt(outfile, 'No stress limit imposed on the TF-CS interface layer')
            call ocmmnt(outfile, '  -> Too much unknow on it material choice/properties')


        # OUT.DAT data on maximum shear stress values for the Tresca criterion
        call ocmmnt(outfile, 'Materal stress of the point of maximum shear stress (Tresca criterion) for each layer')
        call ocmmnt(outfile, 'Please use utilities/plot_stress_tf.py for radial plots plots summary')

        select case (tfcoil_variables.i_tf_bucking)
            case (0)
                if (tfcoil_variables.i_tf_sup == 1 ) :
                    write(outfile,'(t2, "Layers", t36, *(a11,3x) )') "WP", "Outer case"
                else:
                    write(outfile,'(t2, "Layers", t36, *(a11,3x) )') "conductor", "Outer case"

            case (1)
                if (tfcoil_variables.i_tf_sup == 1 ) :
                    write(outfile,'(t2, "Layers", t36, *(a11,3x) )') "Steel case", "WP", "Outer case"
                else:
                    write(outfile,'(t2, "Layers", t36, *(a11,3x) )') "bucking", "conductor", "Outer case"

            case (2)
                if (tfcoil_variables.i_tf_sup == 1 ) :
                    write(outfile,'(t2, "Layers", t36, *(a11,3x) )') "CS", "Steel case", "WP", "Outer case"
                else:
                    write(outfile,'(t2, "Layers", t36, *(a11,3x) )') "CS", "bucking", "conductor", "Outer case"

            case (3)
                if (tfcoil_variables.i_tf_sup == 1 ) :
                    write(outfile,'(t2, "Layers", t36, *(a11,3x) )') "CS", "interface", "Steel case", "WP", "Outer case"
                else:
                    write(outfile,'(t2, "Layers", t36, *(a11,3x) )') "CS", "interface", "bucking", "conductor", "Outer case"

        end select

        write(outfile,'(t2, "Radial"    ," stress", t30, "(MPa)",t36, *(F11.3,3x))')               sig_tf_r_max*1.0e-6
        write(outfile,'(t2, "toroidal"  ," stress", t30, "(MPa)",t36, *(F11.3,3x))')               sig_tf_t_max*1.0e-6
        write(outfile,'(t2, "Vertical"  ," stress", t30, "(MPa)",t36, *(F11.3,3x))')               sig_tf_z_max*1.0e-6
        write(outfile,'(t2, "Von-Mises" ," stress", t30, "(MPa)",t36, *(F11.3,3x))')               sig_tf_vmises_max*1.0e-6
        if ( tfcoil_variables.i_tf_tresca == 1 .and. tfcoil_variables.i_tf_sup == 1 ) :
            write(outfile,'(t2, "Shear (CEA Tresca)"    ," stress", t30, "(MPa)",t36, *(F11.3,3x))') sig_tf_tresca_max*1.0e-6
        else:
            write(outfile,'(t2, "Shear (Tresca)"    ," stress", t30, "(MPa)",t36, *(F11.3,3x))') sig_tf_tresca_max*1.0e-6

        write(outfile, *) ''
        write(outfile,'(t2, "Toroidal"    ," modulus", t30, "(GPa)",t36, *(F11.3,3x))') eyoung_trans * 1.0e-9
        write(outfile,'(t2, "Vertical"    ," modulus", t30, "(GPa)",t36, *(F11.3,3x))') eyoung_axial * 1.0e-9
        write(outfile,* ) ''
        call ovarre(outfile,'WP transverse modulus (GPa)','(eyoung_wp_trans*1.0e-9)', eyoung_wp_trans*1.0e-9, 'OP ')
        call ovarre(outfile,'WP vertical modulus (GPa)','(eyoung_wp_axial*1.0e-9)', eyoung_wp_axial*1.0e-9, 'OP ')
        call ovarre(outfile,'WP transverse Poisson''s ratio','(poisson_wp_trans)', poisson_wp_trans, 'OP ')
        call ovarre(outfile,'WP vertical-transverse Pois. rat.','(poisson_wp_axial)', poisson_wp_axial, 'OP ')

        # MFILE.DAT data
        do ii = 1, tfcoil_variables.n_tf_bucking + 2
            intstring = int2char(ii)
            call ovarre(mfile,'Radial    stress at maximum shear of layer '//intstring//                         ' (Pa)', '(sig_tf_r_max('//intstring//'))', sig_tf_r_max(ii) )
            call ovarre(mfile,'toroidal  stress at maximum shear of layer '//intstring//                         ' (Pa)', '(sig_tf_t_max('//intstring//'))', sig_tf_t_max(ii) )
            call ovarre(mfile,'Vertical  stress at maximum shear of layer '//intstring//                         ' (Pa)', '(sig_tf_z_max('//intstring//'))', sig_tf_z_max(ii) )
            call ovarre(mfile,'Von-Mises stress at maximum shear of layer '//intstring//                         ' (Pa)', '(sig_tf_vmises_max('//intstring//'))', sig_tf_vmises_max(ii) )
            if ( tfcoil_variables.i_tf_tresca == 1 .and. tfcoil_variables.i_tf_sup == 1 ) :
                call ovarre(mfile,'Maximum shear stress for CEA Tresca yield criterion '//intstring//                            ' (Pa)', '(sig_tf_tresca_max('//intstring//'))', sig_tf_tresca_max(ii) )
            else:
                call ovarre(mfile,'Maximum shear stress for the Tresca yield criterion '//intstring//                             ' (Pa)', '(sig_tf_tresca_max('//intstring//'))', sig_tf_tresca_max(ii) )

        end do

        # SIG_TF.DAT storage
        write(sig_file,'(t2, "Points per layers"                 ,t26, *(I11,3x))') n_radial_array
        write(sig_file,*)
        write(sig_file,'(t2, "radius"              , t20, "(m)"  ,t26, *(F11.3,3x))') radial_array
        write(sig_file,'(t2, "Radial"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_r*1.0e-6
        write(sig_file,'(t2, "toroidal"  ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_t*1.0e-6
        write(sig_file,'(t2, "Vertical"  ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_z*1.0e-6
        write(sig_file,'(t2, "Radial"    ," smeared stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_smeared_r*1.0e-6
        write(sig_file,'(t2, "toroidal"  ," smeared stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_smeared_t*1.0e-6
        write(sig_file,'(t2, "vertical"  ," smeared stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_smeared_z*1.0e-6
        write(sig_file,'(t2, "Von-Mises" ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_vmises*1.0e-6
        write(sig_file,'(t2, "Tresca"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_tresca*1.0e-6
        if ( tfcoil_variables.i_tf_sup == 1 ) :
            write(sig_file,'(t2, "CEA Tresca"," stress", t20, "(MPa)",t26, *(F11.3,3x))') s_tresca_cond_cea*1.0e-6
        else:
            write(sig_file,'(t2, "Tresca"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_tresca*1.0e-6

        write(sig_file,*)
        write(sig_file,*) 'Displacement'
        write(sig_file,'(t2, "rad. displacement", t20, "(mm)",t26, *(F11.5,5x))') deflect*1.0e3
        if ( tfcoil_variables.i_tf_stress_model /= 1 ) :
            write(sig_file,*)
            write(sig_file,*) 'Strain'
            write(sig_file,'(t2, "radial strain"   ,t26, *(F11.8,3x))') str_tf_r
            write(sig_file,'(t2, "toroidal strain" ,t26, *(F11.8,3x))') str_tf_t
            write(sig_file,'(t2, "vertical strain" ,t26, *(F11.8,3x))') str_tf_z


        # TODO tfcoil_variables.sig_tf_wp_av_z is always undefined here. This needs correcting or removing
        # if ( tfcoil_variables.i_tf_sup == 1 ) :
            # write(sig_file,'(t2, "WP"    ," smeared stress", t20, "(MPa)",t26, *(F11.3,3x))') tfcoil_variables.sig_tf_wp_av_z*1.0e-6
        #

        # Quantities from the plane stress stress formulation (no resitive coil output)
        if ( tfcoil_variables.i_tf_stress_model == 1 .and. tfcoil_variables.i_tf_sup == 1 ) :
            # Other quantities (displacement strain, etc..)
            call ovarre(outfile,'Maximum radial deflection at midplane (m)','(deflect)',                                 deflect(n_radial_array), 'OP ')
            call ovarre(outfile,'Vertical strain on casing','(casestr)', tfcoil_variables.casestr, 'OP ')
            call ovarre(outfile,'Radial strain on insulator','(insstrain)', tfcoil_variables.insstrain, 'OP ')


    
    
    def plane_stress(self):
        """
        
        """
            kk = ey/(1.0e0 - nu**2)

    # Lorentz forces parametrisation coeficients (array equation)
    alpha = 0.5e0*rmu0 * j**2 / kk

    inner_layer_curr = 0.0e0
    do ii = 1, nlayers

        beta(ii) = 0.5e0*rmu0 * j(ii) * ( inner_layer_curr - pi*j(ii)*rad(ii)**2 ) / (pi*kk(ii))

        # Layer area
        area(ii) = pi * (rad(ii+1)**2 - rad(ii)**2)

        # Total current carried by the inners layers
        inner_layer_curr = inner_layer_curr + area(ii)*j(ii)
    end do
    # ***


    # Left hand side matrix aa
    # ***
    aa(:,:) = 0.0e0

    # Null radial stress at R(1)
    aa(1,1) = kk(1) * (1.0e0+nu(1))
    aa(1,2) = -kk(1) * (1.0e0-nu(1))/(rad(1)**2)


    # Inter-layer boundary conditions
    if ( nlayers /= 1 ) :
        do ii = 1, nlayers - 1

            # Continuous radial normal stress at R(ii+1)
            aa(2*ii, 2*ii-1) = kk(ii) * ( 1.0e0 + nu(ii) )
            aa(2*ii, 2*ii  ) = -kk(ii) * ( 1.0e0 - nu(ii) ) / rad(ii+1)**2
            aa(2*ii, 2*ii+1) = -kk(ii+1) * ( 1.0e0 + nu(ii+1) )
            aa(2*ii, 2*ii+2) = kk(ii+1) * ( 1.0e0 - nu(ii+1) ) / rad(ii+1)**2

            # Continuous displacement at R(ii+1)
            aa(2*ii+1, 2*ii-1) = rad(ii+1)
            aa(2*ii+1, 2*ii  ) = 1.0e0 / rad(ii+1)
            aa(2*ii+1, 2*ii+1) = -rad(ii+1)
            aa(2*ii+1, 2*ii+2) = -1.0e0 / rad(ii+1)

        end do


    # Radial stress = 0
    aa(2*nlayers, 2*nlayers - 1) =  kk(nlayers) * ( 1.0e0 + nu(nlayers) )
    aa(2*nlayers, 2*nlayers    ) = -kk(nlayers) * ( 1.0e0 - nu(nlayers) ) / rad(nlayers+1)**2
    # ***

    # Right hand side vector bb
    # ***
    # Null radial stress at R(1)
    bb(1) = -kk(1) * ( 0.125e0*alpha(1)*(3.0e0+nu(1))*rad(1)**2                      + 0.5e0*beta(1)*(1.0e0 + (1.0e0+nu(1))*log(rad(1))) )

    # Inter-layer boundary conditions
    if ( nlayers /= 1 ) :
        do ii = 1, nlayers - 1

            # Continuous radial normal stress at R(ii+1)
            bb(2*ii) = -kk(ii) * ( 0.125e0*alpha(ii)*(3.0e0+nu(ii))*rad(ii+1)**2                                   + 0.5e0*beta(ii)*(1.0e0 + (1.0e0+nu(ii))*log(rad(ii+1))) )                        +kk(ii+1) * ( 0.125e0*alpha(ii+1)*(3.0e0+nu(ii+1))*rad(ii+1)**2                                   + 0.5e0*beta(ii+1)*(1.0e0 + (1.0e0+nu(ii+1))*log(rad(ii+1))) )

            # Continuous displacement at R(ii+1)
            bb(2*ii+1) = - 0.125e0*alpha(ii)  * rad(ii+1)**3 - 0.5e0*beta(ii)  *rad(ii+1)*log(rad(ii+1))                          + 0.125e0*alpha(ii+1)* rad(ii+1)**3 + 0.5e0*beta(ii+1)*rad(ii+1)*log(rad(ii+1))
        end do


    # Null radial stress at R(nlayers+1)
    bb(2*nlayers) = -kk(nlayers) * ( 0.125e0*alpha(nlayers)*(3.0e0+nu(nlayers))*rad(nlayers+1)**2                                    + 0.5e0*beta(nlayers)*(1.0e0 + (1.0e0+nu(nlayers))*log(rad(nlayers+1))) )
    # ***

    #  Find solution vector cc
    # ***
    cc(:) = 0.0e0
    call maths_library.linesolv(aa, 2*nlayers, bb, cc)

    #  Multiply c by (-1) (John Last, internal CCFE memorandum, 21/05/2013)
    do ii = 1, nlayers
        c1(ii) = cc(2*ii-1)
        c2(ii) = cc(2*ii)
    end do
    # ***
    # ------


    # Radial/toroidal/vertical stress radial distribution
    # ------
    rradius(:) = 0.0e0
    sigr(:) = 0.0e0
    sigt(:) = 0.0e0
    r_deflect(:) = 0.0e0

    do ii = 1, nlayers

        dradius = (rad(ii+1) - rad(ii)) / dble(n_radial_array)
        do jj = (ii-1)*n_radial_array + 1, ii*n_radial_array

            rad_c = rad(ii) + dradius*dble(jj - n_radial_array*(ii-1) - 1)
            rradius(jj) = rad_c


            # Radial stress radial distribution [Pa]
            sigr(jj) = kk(ii) * ( (1.0e0+nu(ii))*c1(ii) - ((1.0e0-nu(ii))*c2(ii))/ rad_c**2                                   + 0.125e0*(3.0e0 + nu(ii))*alpha(ii)* rad_c**2                                   + 0.5e0*beta(ii)*(1.0e0 + (1.0e0+nu(ii))*log(rad_c)) )

            # Radial stress radial distribution [Pa]
            sigt(jj) = kk(ii) * ( (1.0e0+nu(ii))*c1(ii) + (1.0e0-nu(ii))*c2(ii)/ rad_c**2                                   + 0.125e0*(1.0e0+3.0e0*nu(ii))*alpha(ii)*rad_c**2                                   + 0.5e0*beta(ii)*(nu(ii) + (1.0e0+nu(ii))*log(rad_c)) )

            #  Deflection [m]
            r_deflect(jj) = c1(ii)*rad_c + c2(ii)/rad_c                               + 0.125e0*alpha(ii) * rad_c**3                               + 0.5e0*beta(ii) * rad_c*log(rad_c)

        end do
    end do
   # ---

   #-# end break


    
    def tf_integer_turn_geom(self):
        """
        
        """
                rbcndut = thwcndut * 0.75e0

        # Radial turn dimension [m]
        t_turn_radial = ( tfcoil_variables.dr_tf_wp - 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.tfinsgap ) ) / n_layer

        if (t_turn_radial <= (2.0e0*thicndut + 2.0e0*thwcndut) ) :
            fdiags(1) = t_turn_radial
            fdiags(2) = thicndut
            fdiags(3) = thwcndut
            call report_error(100)


        # Toroidal turn dimension [m]
        t_turn_toroidal = ( t_wp_toroidal - 2.0e0 * ( tfcoil_variables.tinstf + tfcoil_variables.tfinsgap ) ) / n_pancake

        if ( t_turn_toroidal <= (2.0e0*thicndut + 2.0e0*thwcndut) ) :
            fdiags(1) = t_turn_toroidal
            fdiags(2) = thicndut
            fdiags(3) = thwcndut
            call report_error(100)


        tfcoil_variables.t_turn_tf = sqrt(t_turn_radial*t_turn_toroidal)

        # Number of TF turns
        n_tf_turn = dble( n_layer * n_pancake )

        # Current per turn [A/turn]
        cpttf = tfc_current/n_tf_turn

        # Radial and toroidal dimension of conductor [m]
        tfcoil_variables.t_conductor_radial = t_turn_radial - 2.0e0*thicndut
        tfcoil_variables.t_conductor_toroidal = t_turn_toroidal - 2.0e0*thicndut
        tfcoil_variables.t_conductor = sqrt(tfcoil_variables.t_conductor_radial*tfcoil_variables.t_conductor_toroidal)

        # Dimension of square cable space inside conduit [m]
        t_cable_radial = tfcoil_variables.t_conductor_radial - 2.0e0*thwcndut
        t_cable_toroidal = tfcoil_variables.t_conductor_toroidal - 2.0e0*thwcndut
        t_cable = sqrt(t_cable_radial*t_cable_toroidal)

        # Cross-sectional area of cable space per turn
        # taking account of rounded inside corners [m2]
        acstf = (t_cable_radial*t_cable_toroidal) - (4.0e0-pi)*rbcndut**2

        if (acstf <= 0.0e0) :
            if ((t_cable_radial < 0.0e0).or.(t_cable_toroidal < 0.0e0)) :
                fdiags(1) = acstf
                fdiags(2) = t_cable_radial
                fdiags(3) = t_cable_toroidal
                call report_error(101)
            else:
                fdiags(1) = acstf
                fdiags(2) = t_cable_radial
                fdiags(2) = t_cable_toroidal
                call report_error(102)
                rbcndut = 0.0e0
                acstf = t_cable_radial * t_cable_toroidal



        # Cross-sectional area of conduit jacket per turn [m2]
        acndttf = tfcoil_variables.t_conductor_radial*tfcoil_variables.t_conductor_toroidal - acstf

        # Area of inter-turn insulation: single turn [m2]
        insulation_area = t_turn_radial*t_turn_toroidal - acndttf - acstf
        # -------------


        #-# end break

    
    