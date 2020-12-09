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

use, intrinsic :: iso_fortran_env, only: dp=>real64
use resistive_materials, only: resistive_material, volume_fractions, &
    supercon_strand
implicit none

private
public :: outtf, sctfcoil, stresscl, tfcind, tfspcall, initialise_cables

! Module variables
!-----------------

real(dp), private :: tf_fit_t
!! Dimensionless winding pack width

real(dp), private :: tf_fit_z
!! Dimensionless winding pack radial thickness

real(dp), private :: tf_fit_y
!! Ratio of peak field with ripple to nominal axisymmetric peak field

real(dp) :: tfc_current
!! Current in each TF coil

real(dp), private :: awpc
!! Total cross-sectional area of winding pack including
!! GW insulation and insertion gap [m2]

real(dp), private :: awptf
!! Total cross-sectional area of winding pack [m2]

real(dp), private :: a_tf_steel
!! Inboard coil steel coil cross-sectional area [m2]

real(dp), private :: a_tf_ins
!! Inboard coil insulation cross-section per coil [m2]

real(dp), private :: f_tf_steel
!! Inboard coil steel fraction [-]

real(dp), private :: f_tf_ins
!! Inboard coil insulation fraction [-]

real(dp), private :: h_cp_top
!! Vertical distance from the midplane to the top of the tapered section [m]

real(dp), private :: r_tf_outboard_in
!! Radial position of plasma-facing edge of TF coil outboard leg [m]

real(dp), private :: r_tf_outboard_out
!! Radial position of outer edge of TF coil inboard leg [m]

real(dp), private :: r_wp_inner
!! Radial position of inner edge and centre of winding pack [m]

real(dp), private :: r_wp_outer
!! Radial position of outer edge and centre of winding pack [m]

real(dp), private :: r_wp_centre
!! Radial position of centre and centre of winding pack [m]

real(dp), private :: vol_ins_cp
!! CP turn insulation volume [m3]

real(dp), private :: vol_gr_ins_cp
!! CP ground insulation volume [m3]

real(dp), private :: vol_case_cp
!! Volume of the CP outer casing cylinder

real(dp), private :: t_wp_toroidal
!! Minimal toroidal thickness of of winding pack [m]

real(dp), private :: t_wp_toroidal_av
!! Averaged toroidal thickness of of winding pack [m]

real(dp), private :: t_lat_case_av
!! Average lateral casing thickness [m]

real(dp), private :: a_case_front
!! Front casing area [m2]

real(dp), private :: a_case_nose
!! Nose casing area [m2]

real(dp), private :: a_ground_ins
!! Inboard mid-plane cross-section area of the WP ground insulation [m2]

real(dp), private :: a_leg_ins
!! TF ouboard leg turn insulation area per coil [m2]

real(dp), private :: a_leg_gr_ins
!! TF outboard leg ground insulation area per coil [m2]

real(dp), private :: a_leg_cond
!! Exact TF ouboard leg conductor area [m2] 

real(dp), private :: theta_coil
!! Half toroidal angular extent of a single TF coil inboard leg

real(dp), private :: tan_theta_coil
!! Tan half toroidal angular extent of a single TF coil inboard leg

real(dp), private :: t_conductor_radial, t_conductor_toroidal
!! Conductor area radial and toroidal dimension (integer turn only) [m]

real(dp), private :: t_cable_radial, t_cable_toroidal
!! Cable area radial and toroidal dimension (integer turn only) [m]

real(dp), private :: t_turn_radial, t_turn_toroidal
!! Turn radial and toroidal dimension (integer turn only) [m]

real(dp), private :: t_cable
!! Cable area averaged dimension (square shape) [m]

real(dp), private :: vforce_inboard_tot
!! Total inboard vertical tension (all coils) [N] 

type(resistive_material):: copper
type(resistive_material):: hastelloy
type(resistive_material):: solder
type(resistive_material):: jacket
type(resistive_material):: helium
type(volume_fractions):: conductor
type(supercon_strand)::croco_strand

real(dp):: T1, time2, tau2, estotft
! (OBSOLETE, but leave for moment)
! real (kind(1.0D0)) ::croco_quench_factor
! real(dp):: jwdgpro_1, jwdgpro_2,  etamax
contains

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

subroutine sctfcoil(outfile,iprint)

    !! Superconducting TF coil module
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Galambos, FEDC/ORNL
    !! author: R Kemp, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! author: S Kahn, CCFE, Culham Science Centre
    !! This subroutine calculates various parameters for a superconducting
    !! TF coil set. The primary outputs are coil size, shape, stress,
    !! and fields.
    !! It is a variant from the original FEDC/Tokamak systems code.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use build_variables, only: tfcth, tfthko, r_tf_outboard_mid, r_tf_inboard_mid, &
        hmax
    use tfcoil_variables, only: i_tf_turns_integer, wwp1, estotftgj, tfind, &
        ritfc, dr_tf_wp, n_tf, bmaxtfrp, bmaxtf, n_tf_stress_layers, n_rad_per_layer, &
        i_tf_sup, i_tf_shape, i_tf_wp_geom, i_tf_case_geom, tinstf, tfinsgap, &
        b_crit_upper_nbti, t_crit_nbti
    use constants, only: rmu0, pi
    use physics_variables, only: itart

    implicit none

    !  Arguments
    integer, intent(in) :: iprint
    !! switch for writing to output file (1=yes)

    integer, intent(in) :: outfile
    !! output file unit

    !  Local variables
    integer :: peaktfflag

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Global radial geometry
    call tf_global_geometry

    ! Calculation of the TF current from bt
    call tf_current

    ! Conductor section internal geometry
    ! ---
    ! Superconducting magnets
    if ( i_tf_sup == 1 ) then 
        call sc_tf_internal_geom(i_tf_wp_geom, i_tf_case_geom, i_tf_turns_integer)

    ! Resitive magnets
    else
        call res_tf_internal_geom
    end if
    ! ---

    ! Coil vertical geometry
    call coilshap

    ! TF resistive heating (res TF only)
    if ( i_tf_sup /= 1 ) call tf_res_heating

    ! Vertical force
    call tf_field_and_force


    ! TF coil inductance
    ! ---
    if ( itart == 0 .and. i_tf_shape == 1 ) then 
        call tfcind(tfcth)
    else 
       tfind = ( hmax + tfthko ) * rmu0/pi * log(r_tf_outboard_mid/r_tf_inboard_mid)
    end if 

    ! Total TF coil stored magnetic energy [J]
    estotft = 0.5D0*tfind * ritfc**2

    ! Total TF coil stored magnetic energy [Gigajoule]
    estotftgj = 1.0D-9 * estotft
    ! ---


    ! Calculate TF coil areas and masses
    call tf_coil_area_and_masses


    ! Peak field including ripple
    ! Rem : as resistive magnets are axisymmetric, no inboard ripple is present 
    if ( i_tf_sup == 1 ) then 
       call peak_tf_with_ripple(n_tf, wwp1, dr_tf_wp - 2.0D0*(tinstf+tfinsgap), &
                                 r_wp_centre, bmaxtf, bmaxtfrp, peaktfflag)
    else
        bmaxtfrp = bmaxtf
    end if

    ! Do stress calculations (writes the stress output)
    if ( iprint == 1 ) n_rad_per_layer = 500
    call stresscl(n_tf_stress_layers, n_rad_per_layer, iprint, outfile)

    if ( iprint == 1 ) call outtf(outfile, peaktfflag)

end subroutine sctfcoil

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_global_geometry()
    !! Subroutine for calculating the TF coil geometry
    !! This includes:
    !!   - Overall geometry of coil (radii and toroidal planes area)
    !!   - Winding Pack NOT included
    use physics_variables, only: rmajor, bt, kappa, itart, rminor
    use build_variables, only: tfcth, tfthko, r_tf_outboard_mid, r_cp_top, &
        r_tf_inboard_in, r_tf_inboard_mid, r_tf_inboard_out
    use tfcoil_variables, only: tinstf, tfc_sidewall_is_fraction, tfareain, &
        ritfc, tftort, n_tf, casthi_is_fraction, bmaxtf, arealeg, &
        casthi_fraction, casths_fraction, tfinsgap, rbmax, casthi, casths, i_tf_sup, &
        dztop, tinstf, tftort, tfinsgap, i_tf_case_geom
    use constants, only: pi
    implicit none
    


    ! Inner leg geometry
    ! ---
    ! Half toroidal angular extent of a single TF coil inboard leg
    theta_coil = pi / n_tf 
    tan_theta_coil = tan(theta_coil)

    ! TF coil inboard legs mid-plane cross-section area (WP + casing ) [m2]
    if ( i_tf_case_geom == 0 ) then
        ! Circular front case
        tfareain = pi * ( r_tf_inboard_out**2 - r_tf_inboard_in**2 )
    else
        ! Straight front case
        tfareain = n_tf * sin(theta_coil)*cos(theta_coil) * r_tf_inboard_out**2  &
                 - pi * r_tf_inboard_in**2 
    end if

    ! Vertical distance from the midplane to the top of the tapered section [m]
    if ( itart ==  1 ) h_cp_top = rminor * kappa + dztop 
    ! ---


    ! Outer leg geometry
    ! ---    
    ! Mid-plane inner/out radial position of the TF coil outer leg [m] 
    r_tf_outboard_in =  r_tf_outboard_mid - tfthko * 0.5D0 
    r_tf_outboard_out = r_tf_outboard_mid + tfthko * 0.5D0 


    ! TF coil width in toroidal direction at inboard leg outer edge [m]
    ! *** 
    ! Sliding joints geometry
    if ( itart == 1 .and. i_tf_sup /= 1 ) then 
        tftort = 2.0D0 * r_cp_top * sin(theta_coil) 

    ! Default thickness, initially written for DEMO SC magnets
    else if ( itart == 1 .and. i_tf_sup ==  1 ) then 
        tftort = 2.0D0 * r_tf_inboard_out * sin(theta_coil)
    else 
        tftort = 2.0D0 * r_tf_inboard_out * sin(theta_coil)    
    end if

    ! Area of rectangular cross-section TF outboard leg [m2]
    arealeg = tftort * tfthko
    ! ---

end subroutine tf_global_geometry

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_current()
    !! Calculation of the maximum B field and the corresponding TF current
    use tfcoil_variables, only: casthi, ritfc, rbmax, i_tf_sup, casths_fraction, &
        tinstf, tftort, bmaxtf, tfinsgap, tfc_sidewall_is_fraction, casths, &
        casthi_is_fraction, casthi_fraction, n_tf, thicndut, thkcas
    use build_variables, only: r_tf_inboard_out, r_tf_inboard_in, tfcth
    use physics_variables, only: bt, rmajor
    use constants, only: pi
    implicit none


    ! Plasma-facing wall thickness if fraction option selected [m]
    if (casthi_is_fraction) casthi = casthi_fraction * tfcth

    ! Case thickness of side wall [m]
    if ( tfc_sidewall_is_fraction ) then
        casths = casths_fraction * ( r_tf_inboard_in + thkcas ) * tan(pi/n_tf)
    end if

    ! Radial position of peak toroidal field [m]
    if ( i_tf_sup == 1 ) then
        ! SC : conservative assumption as the radius is calculated with the
        ! WP radial distances defined at the TF middle (cos)
        rbmax = r_tf_inboard_out * cos(theta_coil) - casthi - tinstf - tfinsgap
    else 
        ! Resistive coils : No approx necessary as the symmetry is cylindrical 
        ! The turn insulation th (thicndut) is also subtracted too here 
        rbmax = r_tf_inboard_out - casthi - thicndut - tinstf
    end if

    ! Calculation of the maximum B field on the magnet [T]
    bmaxtf = bt * rmajor / rbmax  
    
    ! Total current in TF coils [A]
    ! rem SK : ritcf is no longer an input
    ritfc = bmaxtf * rbmax * 5.0D6 

    ! Current per TF coil [A]
    tfc_current = ritfc/n_tf

end subroutine tf_current

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine sc_tf_internal_geom(i_tf_wp_geom, i_tf_case_geom, i_tf_turns_integer)
    !! Author : S. Kahn, CCFE
    !! Seting the WP, case and tunrs geometry for SC magnets
    
    use tfcoil_variables, only: acndttf, awphec, cpttf, insulation_area,     &
        n_layer, n_pancake, n_tf_turn, i_tf_sc_mat, jwptf, thicndut, thwcndut, &
        acasetf, acstf, acond, aiwp, avwp, dhecoil, n_tf, aswp, vftf, tfareain
    use constants, only: pi
       
    implicit none
    

    ! Inputs
    ! ------
    integer, intent(in) :: i_tf_case_geom
    !! Switch for TF case geometry selection

    integer, intent(in) :: i_tf_wp_geom
    !! Switch for TF WP geometry selection

    integer, intent(in) :: i_tf_turns_integer
    !! Switch for TF coil integer/non-integer turns
    ! ------


    ! Calculating the WP / ground insulation areas
    call tf_wp_geom(i_tf_wp_geom)

    ! Calculating the TF steel casing areas 
    call tf_case_geom(i_tf_wp_geom, i_tf_case_geom)

    ! WP/trun currents
    call tf_wp_currents
    
    ! Setting the WP turn geometry / areas 
    if ( i_tf_turns_integer == 0 ) then 
        ! Non-ingeger number of turns
        call tf_averaged_turn_geom( jwptf, thwcndut, thicndut, i_tf_sc_mat, &     ! Inputs
                                    acstf, acndttf, insulation_area, n_tf_turn )  ! Outputs
    else 
        ! Integer number of turns
        call tf_integer_turn_geom( n_layer, n_pancake, thwcndut, thicndut, & ! Inputs
                                   acstf, acndttf, insulation_area, &        ! Outputs
                                   cpttf, n_tf_turn )                        ! Outputs
    end if 
    
    
    ! Areas and fractions
    ! -------------------
    ! Central helium channel down the conductor core [m2]
    awphec = 0.25D0 * n_tf_turn * pi*dhecoil**2

    ! Total conductor cross-sectional area, taking account of void area
    ! and central helium channel [m2]
    acond = acstf * n_tf_turn * (1.0D0-vftf) - awphec

    ! Void area in conductor for He, not including central channel [m2]
    avwp = acstf * n_tf_turn * vftf

    ! Area of inter-turn insulation: total [m2]
    aiwp = n_tf_turn * insulation_area

    ! Area of steel structure in winding pack [m2]
    aswp = n_tf_turn * acndttf

    ! Inboard coil steel area [m2]
    a_tf_steel = acasetf + aswp

    ! Inboard coil steel fraction [-]
    f_tf_steel = n_tf * a_tf_steel / tfareain

    ! Inboard coil insulation cross-section [m2]
    a_tf_ins = aiwp + a_ground_ins

    !  Inboard coil insulation fraction [-]
    f_tf_ins = n_tf * a_tf_ins / tfareain 
    ! -------------------


    contains
    subroutine tf_wp_geom(i_tf_wp_geom)
        !! Author : S. Kahn, CCFE
        !! Seting the WP geometry and area for SC magnets

        use error_handling, only: fdiags, report_error
        use build_variables, only: tfcth, r_tf_inboard_in, r_tf_inboard_out
        use tfcoil_variables, only: dr_tf_wp, casthi, thkcas, casths, &
            wwp1, wwp2, tinstf, tfinsgap
        use numerics, only: nvar, ixc
            
        implicit none

        ! Inputs
        ! ------
        integer, intent(in) :: i_tf_wp_geom
        !! Switch for TF WP geometry selection
        !!   0 : Rectangular geometry 
        !!   1 : Double rectangular geometry 
        !!   2 : Trapezoidal geometry (constant lateral casing thickness)
        ! ------


        ! Local variables
        ! ------
        real(dp) :: t_tf_at_wp
        !! TF coil width at inner egde of winding pack toroidal direction [m]
        ! ------
    

        ! Radial position of inner edge of winding pack [m]
        r_wp_inner = r_tf_inboard_in + thkcas
  
        ! Radial position of outer edge of winding pack [m]
        r_wp_outer = r_wp_inner + dr_tf_wp
    
        ! Radius of geometrical centre of winding pack [m]
        r_wp_centre = 0.5D0 * ( r_wp_inner + r_wp_outer )

        ! TF toroidal thickness at the WP inner radius [m]
        t_tf_at_wp = 2.0D0 * r_wp_inner * tan_theta_coil    

        ! Minimal toroidal thickness of winding pack [m]
        t_wp_toroidal = t_tf_at_wp - 2.0D0 * casths

        ! Rectangular WP
        ! --------------
        if ( i_tf_wp_geom == 0 ) then
        
            ! Outer WP layer toroidal thickness [m]
            wwp1 = t_wp_toroidal

            ! Averaged toroidal thickness of of winding pack [m]
            t_wp_toroidal_av = t_wp_toroidal

            ! Total cross-sectional area of winding pack [m2]
            awpc = dr_tf_wp * t_wp_toroidal
            
            ! WP cross-section without insertion gap and ground insulation [m2]
            awptf = ( dr_tf_wp - 2.0D0 * ( tinstf + tfinsgap ) )  &
                  * ( t_wp_toroidal - 2.0D0 * ( tinstf + tfinsgap ) )
                    
            ! Cross-section area of the WP ground insulation [m2]
            a_ground_ins = ( dr_tf_wp - 2.0D0 * tfinsgap )  &
                         * ( t_wp_toroidal - 2.0D0 *  tfinsgap ) - awptf
         
        
        ! Double rectangular WP
        ! ---------------------
        else if ( i_tf_wp_geom == 1 ) then 

            ! Thickness of winding pack section at R > r_wp_centre [m]
            wwp1 = 2.0D0 * ( r_wp_centre * tan_theta_coil - casths )

            ! Thickness of winding pack section at R < r_wp_centre [m]
            wwp2 = 2.0D0 * ( r_wp_inner * tan_theta_coil - casths )

            ! Averaged toroidal thickness of of winding pack [m]
            t_wp_toroidal_av = 0.5D0 * ( wwp1 + wwp2 )

            ! Total cross-sectional area of winding pack [m2]
            ! Including ground insulation and insertion gap
            awpc = dr_tf_wp * t_wp_toroidal_av

            ! WP cross-section without insertion gap and ground insulation [m2]
            awptf = 0.5D0 * ( dr_tf_wp - 2.0D0 * ( tinstf + tfinsgap ) )  &
                          * ( wwp1 + wwp2 - 4.0D0 * ( tinstf + tfinsgap ) )

            ! Cross-section area of the WP ground insulation [m2]
            a_ground_ins = 0.5D0 * ( dr_tf_wp - 2.0D0 * tfinsgap )  &
                                 * ( wwp1 + wwp2 - 4.0D0 * tfinsgap ) - awptf

        
        ! Trapezoidal WP
        ! --------------
        else 

            ! Thickness of winding pack section at r_wp_outer [m]
            wwp1 = 2.0D0 * ( r_wp_outer * tan_theta_coil - casths )

            ! Thickness of winding pack section at r_wp_inner [m]
            wwp2 = 2.0D0 * ( r_wp_inner * tan_theta_coil - casths )
            
            ! Averaged toroidal thickness of of winding pack [m]
            t_wp_toroidal_av = 0.5D0 * ( wwp1 + wwp2 )

            ! Total cross-sectional area of winding pack [m2]
            ! Including ground insulation and insertion gap
            awpc = dr_tf_wp * ( wwp2 + 0.5D0 * ( wwp1 - wwp2 ) )
            
            ! WP cross-section without insertion gap and ground insulation [m2]
            awptf = ( dr_tf_wp - 2.0D0 * ( tinstf + tfinsgap ) ) &
                  * ( wwp2 - 2.0D0 * ( tinstf + tfinsgap ) + 0.5D0 * ( wwp1 - wwp2 ) )
            
            ! Cross-section area of the WP ground insulation [m2]
            a_ground_ins = ( dr_tf_wp - 2.0D0 * tfinsgap ) &
                         * ( wwp2 - 2.0D0 * tfinsgap  + 0.5D0 * ( wwp1 - wwp2 ) ) - awptf

        end if 
        ! --------------


        ! Negative WP area error reporting
        if ( awptf <= 0.0D0 .or. awpc <= 0.0D0 ) then
            fdiags(1) = awptf 
            fdiags(2) = awpc 
            call report_error(99)
        end if
        
    end subroutine tf_wp_geom

    subroutine tf_case_geom(i_tf_wp_geom, i_tf_case_geom)
        !! Author : S. Kahn, CCFE
        !! Seting the case geometry and area for SC magnets

        use error_handling, only: fdiags, report_error
        use tfcoil_variables, only: acasetf, acasetfo, arealeg, tfareain, n_tf, &
            casths, casthi, dr_tf_wp
        use build_variables, only: r_tf_inboard_in, r_tf_inboard_out
        implicit none

        ! Inputs
        ! ------
        integer, intent(in) :: i_tf_wp_geom
        !! Switch for TF WP geometry selection
        !!   0 : Rectangular geometry 
        !!   1 : Double rectangular geometry 
        !!   2 : Trapezoidal geometry (constant lateral casing thickness)

        integer, intent(in) :: i_tf_case_geom
        !! Switch for TF case geometry selection
        !!   0 : Circular front case (ITER design)
        !!   1 : Straight front case
        ! ------

        ! Inboard leg cross-sectional area of surrounding case [m2]
        acasetf = (tfareain / n_tf) - awpc
    
        ! Outboard leg cross-sectional area of surrounding case [m2]
        acasetfo = arealeg - awpc

        ! Front casing area [m2]
        if ( i_tf_case_geom == 0 ) then

            ! Circular front case
            a_case_front = theta_coil * r_tf_inboard_out**2  &
                         - tan_theta_coil * r_wp_outer**2 
        else
            
            ! Straight front case
            a_case_front = ( (r_wp_outer + casthi)**2 - r_wp_outer**2 ) * tan_theta_coil
        end if


        ! Nose casing area [m2]
        a_case_nose = tan_theta_coil * r_wp_inner**2  &
                    - theta_coil * r_tf_inboard_in**2

        ! Report error if the casing area is negative
        if ( acasetf <= 0.0D0 .or. acasetfo <= 0.0D0 ) then
            fdiags(1) = acasetf
            fdiags(2) = acasetfo
            call report_error(99)
        end if
        
        ! Average lateral casing thickness
        ! --------------
        ! Rectangular casing
        if ( i_tf_wp_geom == 0 ) then
            t_lat_case_av = casths + 0.5D0*tan_theta_coil * dr_tf_wp
    
        ! Double rectangular WP
        else if ( i_tf_wp_geom == 1 ) then 
            t_lat_case_av = casths + 0.25D0*tan_theta_coil * dr_tf_wp
             
        ! Trapezoidal WP
        else 
            t_lat_case_av = casths 
        end if 
        ! --------------
    end subroutine tf_case_geom

    subroutine tf_averaged_turn_geom( jwptf, thwcndut, thicndut, i_tf_sc_mat,    & ! Inputs
                                      acstf, acndttf, insulation_area, n_tf_turn ) ! Outputs

        !! Authors : J. Morris, CCFE
        !! Authors : S. Kahn, CCFE
        !! Setting the TF WP turn geometry for SC magnets from the number
        !! the current per turn.
        !! This calculation has two purposes, first to check if a turn can exist
        !! (positive cable space) and the second to provide its dimensions,
        !! areas and the (float) number of turns
        
        use error_handling, only: fdiags, report_error
        use constants, only: pi
        use tfcoil_variables, only : layer_ins, t_conductor, t_turn_tf, &
            t_turn_tf_is_input, cpttf
        
        implicit none

        ! Inputs
        ! ------
        integer, intent(in) :: i_tf_sc_mat
        !! Switch for superconductor material in TF coils

        real(dp), intent(in) :: jwptf
        !! Winding pack engineering current density [A/m2]

        real(dp), intent(in) :: thwcndut
        !! Steel conduit thickness [m]

        real(dp), intent(in) :: thicndut
        !! Turn insulation thickness [m]
        ! ------


        ! Outputs
        ! -------
        real(dp), intent(out) :: acstf
        !! Cable space area (per turn)  [m2]

        real(dp), intent(out) :: acndttf
        !! Steel conduit area (per turn) [m2]
        
        real(dp), intent(out) :: insulation_area
        !! Turn insulation area (per turn) [m2]

        real(dp), intent(out) :: n_tf_turn
        !! Number of turns per WP (float)
        ! -------


        ! Local variables
        !----------------
        real(dp) :: a_turn
        !! Turn squared dimension [m2]

        real(dp) :: rbcndut
        !! Radius of rounded corners of cable space inside conduit [m]
        !----------------
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        ! Turn dimension is a an input
        if ( t_turn_tf_is_input ) then 
            ! Turn dimension [m2]
            a_turn = t_turn_tf**2

            ! Current per turn [A]
            cpttf = a_turn * jwptf

        ! Current per turn is an input
        else                         
            ! Turn dimension [m2]
            ! Allow for additional inter-layer insulation MDK 13/11/18
            ! Area of turn including conduit and inter-layer insulation
            a_turn = cpttf / jwptf

            ! Dimension of square cross-section of each turn including inter-turn insulation [m]
            t_turn_tf = sqrt(a_turn)

        end if 
        
        ! Square turn assumption
        t_turn_radial = t_turn_tf
        t_turn_toroidal = t_turn_tf
            
        ! See derivation in the following document
        ! k:\power plant physics and technology\process\hts\hts coil module for process.docx
        t_conductor = (-layer_ins + sqrt(layer_ins**2 + 4.0D00*a_turn))/2.d0 &
                    - 2.0D0*thicndut
            
        ! Total number of turns per TF coil (not required to be an integer)
        n_tf_turn = awptf / a_turn

        ! Area of inter-turn insulation: single turn [m2]
        insulation_area = a_turn - t_conductor**2
        
        ! ITER like turn structure 
        if ( i_tf_sc_mat /= 6 ) then 

            ! Radius of rounded corners of cable space inside conduit [m]
            rbcndut = thwcndut * 0.75D0     
        
            ! Dimension of square cable space inside conduit [m]
            t_cable = t_conductor - 2.0D0*thwcndut
        
            ! Cross-sectional area of cable space per turn
            ! taking account of rounded inside corners [m2]
            acstf = t_cable**2 - (4.0D0-pi)*rbcndut**2
        
            if (acstf <= 0.0D0) then
                if ( t_conductor < 0.0D0 ) then
                    fdiags(1) = acstf
                    fdiags(2) = t_cable
                    call report_error(101)
                else
                    fdiags(1) = acstf
                    fdiags(2) = t_cable
                    call report_error(102)
                    rbcndut = 0.0D0
                    acstf = t_cable**2
                end if
            end if
        
            ! Cross-sectional area of conduit jacket per turn [m2]
            acndttf = t_conductor**2 - acstf

        ! REBCO turn structure
        else if (i_tf_sc_mat == 6 ) then  

            ! Diameter of circular cable space inside conduit [m]
            t_cable = t_conductor - 2.0D0*thwcndut
        
            ! Cross-sectional area of conduit jacket per turn [m2]
            acndttf = t_conductor**2 - acstf
        
        end if

    end subroutine tf_averaged_turn_geom

    subroutine tf_integer_turn_geom( n_layer, n_pancake, thwcndut, thicndut, & ! Inputs
                                     acstf, acndttf, insulation_area, & ! Outputs
                                     cpttf, n_tf_turn )                   ! Outputs

        !! Authors : J. Morris
        !! Authors : S. Kahn
        !! Setting the TF WP turn geometry for SC magnets from the number
        !! of turns rows in the radial direction. The turns can have any 
        !! rectangular shapes.
        !! This calculation has two purposes, first to check if a turn can exist
        !! (positive cable space) and the second to provide its dimenesions,
        !! areas and the its associated current

        use error_handling, only: fdiags, report_error
        use tfcoil_variables, only: dr_tf_wp, tinstf, tfinsgap, t_conductor, &
            t_turn_tf
        use constants, only: pi
        implicit none

        ! Inputs
        ! ------
        integer, intent(in) :: n_layer
        !! Number of turns in the radial direction
        
        integer, intent(in) :: n_pancake
        !! Number of turns in the toroidal direction

        real(dp), intent(in) :: thwcndut
        !! Steel conduit thickness [m]

        real(dp), intent(in) :: thicndut
        !! Turn insulation thickness [m]
        ! ------

        ! Outputs
        ! -------
        real(dp), intent(out) :: acstf
        !! Cable space area (per turn)  [m2]

        real(dp), intent(out) :: acndttf
        !! Steel conduit area (per turn) [m2]
        
        real(dp), intent(out) :: insulation_area
        !! Turn insulation area (per turn) [m2]

        real(dp), intent(out) :: cpttf
        !! TF turns current [A]

        real(dp), intent(out) :: n_tf_turn
        !! Number of turns
        ! -------

        ! Local variables
        ! ------
        real(dp) :: rbcndut
        !! Radius of rounded corners of cable space inside conduit [m]
        ! ------

        ! ************************************
        ! TODO: turn  compatibility with croco
        ! ************************************

        ! Radius of rounded corners of cable space inside conduit [m]
        rbcndut = thwcndut * 0.75D0

        ! Radial turn dimension [m]
        t_turn_radial = ( dr_tf_wp - 2.0D0 * ( tinstf + tfinsgap ) ) / n_layer

        if (t_turn_radial <= (2.0D0*thicndut + 2.0D0*thwcndut) ) then
            fdiags(1) = t_turn_radial
            fdiags(2) = thicndut
            fdiags(3) = thwcndut
            call report_error(100)
        end if
    
        ! Toroidal turn dimension [m]
        t_turn_toroidal = ( t_wp_toroidal - 2.0D0 * ( tinstf + tfinsgap ) ) / n_pancake
    
        if ( t_turn_toroidal <= (2.0D0*thicndut + 2.0D0*thwcndut) ) then
            fdiags(1) = t_turn_toroidal
            fdiags(2) = thicndut
            fdiags(3) = thwcndut
            call report_error(100)
        end if

        t_turn_tf = sqrt(t_turn_radial*t_turn_toroidal)
    
        ! Number of TF turns
        n_tf_turn = dble( n_layer * n_pancake )

        ! Current per turn [A/turn]
        cpttf = tfc_current/n_tf_turn
    
        ! Radial and toroidal dimension of conductor [m]
        t_conductor_radial = t_turn_radial - 2.0D0*thicndut
        t_conductor_toroidal = t_turn_toroidal - 2.0D0*thicndut
        t_conductor = sqrt(t_conductor_radial*t_conductor_toroidal)
    
        ! Dimension of square cable space inside conduit [m]
        t_cable_radial = t_conductor_radial - 2.0D0*thwcndut
        t_cable_toroidal = t_conductor_toroidal - 2.0D0*thwcndut
        t_cable = sqrt(t_cable_radial*t_cable_toroidal)

        ! Cross-sectional area of cable space per turn
        ! taking account of rounded inside corners [m2]
        acstf = (t_cable_radial*t_cable_toroidal) - (4.0D0-pi)*rbcndut**2
    
        if (acstf <= 0.0D0) then
            if ((t_cable_radial < 0.0D0).or.(t_cable_toroidal < 0.0D0)) then
                fdiags(1) = acstf
                fdiags(2) = t_cable_radial
                fdiags(3) = t_cable_toroidal
                call report_error(101)
            else
                fdiags(1) = acstf 
                fdiags(2) = t_cable_radial
                fdiags(2) = t_cable_toroidal
                call report_error(102)
                rbcndut = 0.0D0
                acstf = t_cable_radial * t_cable_toroidal
            end if
        end if
    
        ! Cross-sectional area of conduit jacket per turn [m2]
        acndttf = t_conductor_radial*t_conductor_toroidal - acstf
    
        ! Area of inter-turn insulation: single turn [m2]
        insulation_area = t_turn_radial*t_turn_toroidal - acndttf - acstf
        ! -------------
    
    end subroutine tf_integer_turn_geom

    subroutine tf_wp_currents()
        !! Author : S. Kahn, CCFE
        !! Turn engineering turn currents/densities

        use tfcoil_variables, only: ritfc, tfareain, n_tf, oacdcp
        implicit none

        ! Global inboard leg average current in TF coils [A/m2]
        oacdcp = ritfc / tfareain
    
        ! Winding pack current density (forced to be positive) [A/m2]
        jwptf = max(1.0D0, ritfc/(n_tf*awptf))
    
    end subroutine tf_wp_currents


end subroutine sc_tf_internal_geom

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine res_tf_internal_geom()
    !! Author : S. Kahn
    !! Resisitve TF turn geometry, equivalent to winding_pack subroutines
    use error_handling, only: fdiags, report_error 
    use numerics, only: nvar, ixc
    use tfcoil_variables, only: n_tf_turn, thicndut, thkcas, dr_tf_wp, tftort,   &
        tfareain, ritfc, oacdcp, fcoolcp, cpttf, cdtfleg, casthi, aiwp, acasetf, &
        tinstf, n_tf
    use build_variables, only: tfthko, r_tf_inboard_in, r_tf_inboard_out
    use constants, only: pi

    implicit none
            
    ! Inernal variables
    ! -----------------
    real(dp) :: a_tf_cond
    !! Exact mid-plane conductor cross-section area [m2]
    ! -----------------

            
    ! Radial position of inner/outer edge of winding pack [m]
    r_wp_inner = r_tf_inboard_in  + thkcas 
    r_wp_outer = r_tf_inboard_out - casthi 

    ! Number of turns
    ! Set by user (no turn structure by default, i.e. n_tf_turn = 1 ) 
    if ( abs(n_tf_turn) < epsilon(n_tf_turn) ) n_tf_turn = 1.0D0

    ! Total mid-plane cross-sectional area of winding pack, [m2]
    ! including the surrounding ground-wall insulation layer 
    awpc = pi * ( r_wp_outer**2 - r_wp_inner**2 ) / n_tf

    ! WP mid-plane cross-section excluding ground insulation per coil [m2]
    awptf = pi * ( ( r_wp_outer - tinstf )**2 - ( r_wp_inner + tinstf )**2 ) / n_tf &
          - 2.0D0 * tinstf * ( dr_tf_wp - 2.0D0 * tinstf )

    ! Ground insulation cross-section area per coil [m2]
    a_ground_ins = awpc - awptf

    ! Exact mid-plane cross-section area of the conductor per TF coil [m2]
    a_tf_cond = pi * ( ( r_wp_outer - tinstf - thicndut )**2          &
                     - ( r_wp_inner + tinstf + thicndut )**2 ) / n_tf &
              - ( dr_tf_wp - 2.0D0 * ( tinstf + thicndut ) )          &
              * 2.0D0 * ( tinstf + thicndut * n_tf_turn )
    a_tf_cond = a_tf_cond * ( 1.0D0 - fcoolcp )

    ! Inter turn insulation area per coil [m2]                    
    aiwp = awptf - a_tf_cond / ( 1.0D0 - fcoolcp )  

    ! Total insulation cross-section per coil [m2]
    a_tf_ins = aiwp + a_ground_ins

    ! Insulation fraction [-]
    f_tf_ins = n_tf * a_tf_ins / tfareain 

    ! Total cross-sectional area of surrounding case per coil [m2]
    ! itart = 1 : Only valid at mid-plane
    acasetf = ( tfareain / n_tf ) - awpc 

    ! Current per turn 
    cpttf = ritfc / ( n_tf_turn * n_tf )

    ! Exact current density on the mid-plane conductors  
    oacdcp = ritfc / ( awptf * n_tf * n_tf_turn ) 

    ! Exact current density on TF oubard legs
    cdtfleg = ritfc / ( ( 1.0D0 - fcoolcp )  &
                      * ( tftort - 2.0D0 * ( n_tf_turn * thicndut + tinstf ) ) &
                      * ( tfthko - 2.0D0 * ( thicndut + tinstf ) ) ) 

    ! Reporting negative WP areas issues
    if ( awpc < 0.0D0 ) then
        fdiags(1) = awpc
        fdiags(1) = dr_tf_wp
        call report_error(99)

    else if ( awptf < 0.0D0 ) then
        fdiags(1) = awptf
        call report_error(101)
    end if
       

end subroutine res_tf_internal_geom

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_res_heating()
    !! Resitive magnet resitive heating calculations
    !! Rem SK : Clamped joined superconductors might have resistive power losses on the joints
    !! Rem SK : Sliding joints might have a region of high resistivity
    use tfcoil_variables, only: rhocp, tlegav, thicndut, th_joint_contact, rhotfleg, &
        vol_cond_cp, n_tf_turn, thkcas, tftort, tfleng, tflegres, tcpav, arealeg, &
        ritfc, rho_tf_joints, presleg, prescp, pres_joints, n_tf_joints_contact, &
        n_tf_joints, n_tf, i_tf_sup, frholeg, frhocp, fcoolcp, casthi, &
        a_cp_cool, fcoolleg, i_cp_joints, tinstf
    use build_variables, only: tfthko, tfcth, r_cp_top, hmax, &
        r_tf_inboard_in, r_tf_inboard_out
    use physics_variables, only: itart
    use constants, only: pi
    implicit none

    ! Internal variable
    ! ---
    real(dp) :: a_joints
    !! Total area of joint contact [m2]

    integer :: n_contact_tot
    !! Total number of contact area (4 joints section per legs)

    integer :: is_leg_cp_temp_same = 0
    ! Not sure what it actually does --> to be understood !!
    ! ---

        
    ! Copper : Copper resistivity degraded by 1/0.92 for the used of GLIDCOP A-15 
    !          Better structural properties at high temperature and radiation damage resilience
    if ( i_tf_sup == 0 ) rhocp = (frhocp/0.92D0) * ( 1.72D0 + 0.0039D0*(tcpav-273.15D0) ) * 1.0D-8

    ! Aluminium
    if ( i_tf_sup == 2 ) rhocp = frhocp * ( 2.00016D-14*tcpav**3 - 6.75384D-13*tcpav**2 + 8.89159D-12*tcpav )

    ! Calculations dedicated for configurations with CP
    if ( itart == 1 ) then 

        ! Tricky trick to make the leg / CP tempearture the same
        if ( abs(tlegav + 1.0D0) < epsilon(tlegav) ) then 
            is_leg_cp_temp_same = 1
            tlegav = tcpav
        end if

        ! Leg resistivity (different leg temperature as separate cooling channels) 
        if ( i_tf_sup == 0 ) rhotfleg = (frholeg/0.92D0) * ( 1.72D0 + 0.0039D0*(tlegav-273.15D0) ) * 1.0D-8              
        if ( i_tf_sup == 2 ) rhotfleg =  frholeg * ( 2.00016D-14*tlegav**3 - 6.75384D-13*tlegav**2 + 8.89159D-12*tlegav )

        ! Tricky trick to make the leg / CP tempearture the same
        if ( is_leg_cp_temp_same == 1 ) tlegav = -1.0D0  

        ! Centrepost resisitivity and conductor/insulation volume
        call cpost( r_cp_top-casthi-tinstf, h_cp_top,                     & ! Inputs
                    r_tf_inboard_out-casthi-tinstf, hmax+tfthko,          & ! Inputs
                    ritfc, rhocp, fcoolcp, r_tf_inboard_in+thkcas+tinstf, & ! Inputs
                    thicndut, tinstf, thkcas, casthi, n_tf*n_tf_turn,     & ! Inputs
                    a_cp_cool, vol_cond_cp, prescp, &        ! Outputs
                    vol_ins_cp, vol_case_cp, vol_gr_ins_cp ) ! Outputs
    end if

    ! Leg cross-section areas
    ! Rem : For itart = 1, these quantitire corresponds to the outer leg only
    ! ---
    ! Leg ground insulation area per coil [m2]
    a_leg_gr_ins = arealeg - ( tftort - 2.0D0 * tinstf ) &
                           * ( tfthko - 2.0D0 * tinstf )

    ! Outboard leg turns insulation area per coil [m2]
    a_leg_ins = 2.0D0 * thicndut * ( tftort - 2.0D0 * tinstf )     &                    ! toroidal direction 
              + 2.0D0 * thicndut * n_tf_turn * ( tfthko - 2.0D0 * ( thicndut + tinstf ) ) ! radial direction

    ! Exact TF outboard leg conductor area per coil [m2]
    a_leg_cond = ( 1.0D0 - fcoolleg ) * ( arealeg - a_leg_gr_ins - a_leg_ins )  
    ! ---


    if ( itart == 1 ) then

        ! Outer leg resistive power loss
        ! ---
        ! TF outboard leg's resistance calculation (per leg) [ohm]
        tflegres = rhotfleg * tfleng / a_leg_cond

        ! TF outer leg resistive power (TOTAL) [W]   
        presleg = tflegres * ritfc**2 / n_tf 
        ! ---


        ! Sliding joints resistive heating
        ! ---
        if ( i_cp_joints /= 0 ) then

            ! Total number of contact area (4 joints section per legs)
            n_contact_tot = 4 * n_tf_joints_contact* n_tf_joints * nint(n_tf_turn) * nint(n_tf)
            
            ! Total area of joint contact
            a_joints = tfthko * th_joint_contact * dble(n_contact_tot)

            ! joints resistive power losses
            pres_joints = rho_tf_joints * ritfc**2 / a_joints
        else 
            ! Joints resistance to be evaluated for SC
            pres_joints = 0.0D0
        end if
        ! ---


    ! Case of a resistive magnet without joints
    ! ***
    else          

        ! TF resistive powers
        prescp = rhocp * ritfc**2 * tfleng / ( a_leg_cond * n_tf )

        ! prescp containts the the total resistive power losses
        presleg = 0.0D0
                           
        ! No joints if itart = 0
        pres_joints = 0.0D0

    end if

end subroutine tf_res_heating

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_field_and_force()
    !! Calculate the TF coil field, force and VV quench consideration, and the resistive magnets resistance/volume

    use physics_variables, only: rminor, rmajor, bt, itart
    use build_variables, only: r_tf_outboard_mid, r_vv_inboard_out, &
        r_tf_inboard_mid, r_cp_top
    use tfcoil_variables, only: vforce, n_tf, taucq, sigvvall, cforce, &
        ritfc, bmaxtf, rbmax, i_tf_sup, f_vforce_inboard, vforce_outboard, &
        tinstf, thicndut, dr_tf_wp, tfinsgap, i_cp_joints

    implicit none

    ! Local variables
    ! ---------------
    real(dp) :: r_in_wp
    !! Inner WP radius removing the insulation layer and the insertion gap [m]

    real(dp) :: r_out_wp
    !! Outer WP radius removing the insulation layer and the insertion gap [m]

    real(dp) :: dr_wp
    !! WP radial thickness removing the insulation layer and the insertion gap [m]

    real(dp) :: vforce_tot
    !! Total vertical force : inboard + outbord [N] 

    real(dp) :: r_in_outwp
    !! Plasma side radius of the outboard leg winding pack (at-midplane) [m]
    ! ---------------


    ! Quench time [s]
    ! Determine quench time (based on IDM: 2MBSE3)
    ! Resistive magnets : calculation of the resistive power losses added
    ! Issue #337: Force on the vessel wall due to TF coil quench
    if ( i_tf_sup == 1 ) taucq = (bt * ritfc * rminor * rminor) / (r_vv_inboard_out * sigvvall)
    
    ! Outer/inner WP radius removing the ground insulation layer and the insertion gap [m]
    if ( i_tf_sup == 1 ) then
        r_out_wp = r_wp_outer - tinstf - tfinsgap
        r_in_wp = r_wp_inner + tinstf + tfinsgap
    else
        r_out_wp = r_wp_outer - tinstf
        r_in_wp = r_wp_inner + tinstf
    end if

    ! Associated WP thickness
    dr_wp = r_out_wp - r_in_wp


    ! In plane forces 
    ! ---
    ! Centering force = net inwards radial force per meters per TF coil [N/m]
    cforce = 0.5D0 * bmaxtf*ritfc/n_tf 


    ! Vertical force per coil [N]
    ! ***
    ! Rem : this force does not depends on the TF shape or the presence of
    !        sliding joints, the in/outboard vertical tension repartition is
    !-!
    ! Ouboard leg WP plasma side radius without ground insulation/insertion gat [m]
    if ( i_tf_sup == 1 ) then
        r_in_outwp = r_tf_outboard_in + tinstf + tfinsgap    
    else
        r_in_outwp = r_tf_outboard_in + tinstf
    end if
    
    ! May the force be with you
    vforce_tot = 0.5D0 * ( bt * rmajor * ritfc ) / ( n_tf * dr_wp**2 ) &
               * ( r_out_wp**2 * log( r_out_wp / r_in_wp )  &
                 + r_in_outwp**2 * log( (r_in_outwp + dr_wp) / r_in_outwp ) &
                 + dr_wp**2         * log( (r_in_outwp + dr_wp) / r_in_wp )             &
                 - dr_wp            * ( r_out_wp + r_in_outwp )                         &
                 + 2.0D0 * dr_wp * ( r_out_wp     * log(r_in_wp / r_out_wp)                   &
                                   + r_in_outwp * log((r_in_outwp + dr_wp)        &
                                   / r_in_outwp))) 

    ! Case of a centrepost (itart == 1) with sliding joints (the CP vertical are separated from the leg ones)
    ! Rem SK : casing/insulation thickness not subtracted as part of the CP is genuinely connected to the legs..
    if ( itart == 1 .and. i_cp_joints == 1 ) then
        
        ! Tricky trick to avoid dividing by 0 if the TF has no hole in it
        if ( abs(r_in_wp) < epsilon(r_in_wp) ) r_in_wp = 1.0D-9

        ! CP vertical tension [N]
        vforce = 0.25D0 * (bt * rmajor * ritfc) / (n_tf * dr_wp**2) & 
               * ( 2.0D0 * r_out_wp**2 * log(r_out_wp / r_in_wp )   &
                 + 2.0D0 * dr_wp**2 * log( r_cp_top / r_in_wp )     &
                 + 3.0D0 * dr_wp**2                                 &
                 - 2.0D0 * dr_wp * r_out_wp                         &
                 + 4.0D0 * dr_wp * r_out_wp *log( r_in_wp / r_out_wp ) )

        ! Vertical tension applied on the outer leg [N]
        vforce_outboard = vforce_tot - vforce

        ! Inboard vertical tension fraction
        f_vforce_inboard = vforce / vforce_tot

    ! Case of TF without joints or with clamped joints vertical tension
    else 

        ! Inboard vertical tension [N]
        vforce = f_vforce_inboard * vforce_tot

        ! Ouboard vertical tension [N]
        vforce_outboard = vforce * ( ( 1.0D0 / f_vforce_inboard ) - 1.0D0 )  
    end if
    ! ***

    ! Total vertical force
    vforce_inboard_tot = vforce * n_tf

end subroutine tf_field_and_force

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_coil_area_and_masses()
    !! Subroutine to calculate the TF coil areas and masses
    use build_variables, only: hr1, r_tf_outboard_mid, tfcth, r_tf_inboard_mid, &
        r_tf_inboard_in, r_tf_inboard_out
    use fwbs_variables, only: denstl
    use tfcoil_variables, only: whtconsh, whttf, whtcas, tficrn, tfcryoarea, &
        tfsao, whtgw, tfocrn, whtconsc, whtconcu, whtcon, whtconin, &
        tfsai, vftf, whtconin, tfsai, dcond, dcondins, whtcon, &
        tfleng, dthet, dcase, acndttf, n_tf_turn, n_tf, aiwp, radctf, acasetfo, &
        acasetf, fcutfsu, awphec, acstf, whttflgs, whtcp, whtconal, vol_cond_cp, &
        i_tf_sup, i_tf_sc_mat, arealeg, thkcas, voltfleg
    use constants, only: twopi, dcopper, dalu, pi
    use physics_variables, only: itart
    implicit none

    ! Local Variables
    ! ---------------
    real(dp) :: cplen, wbtf

    real(dp) :: vol_case
    !! Total TF case volume [m3]

    real(dp) :: vol_ins
    !! Total leg turn insulation volume [m3]
    
    real(dp) :: vol_gr_ins
    !! Total leg turn insulation volume [m3]
    
    real(dp) :: vol_cond
    !! Total conductor insulator volume [m3]

    real(dp) :: vol_ins_leg
    !! Outboard leg turn isulation volume [m3]
    
    real(dp) :: vol_gr_ins_leg
    !! Outboard leg turn isulation volume [m3]

    real(dp) :: vol_cond_leg
    !! Outboard leg conductor insulator volume [m3]
    ! ---------------


    ! Surface areas (for cryo system) [m2]
    ! tfsai, tfsao are retained for the (obsolescent) TF coil nuclear heating calculation
    wbtf = r_tf_inboard_out*sin(theta_coil) - r_tf_inboard_in*tan_theta_coil
    tfocrn = r_tf_inboard_in * tan_theta_coil
    tficrn = tfocrn + wbtf
    tfsai = 4.0D0 * n_tf * tficrn * hr1
    tfsao = 2.0D0 * n_tf * tficrn * (tfleng - 2.0D0*hr1)

    ! Total surface area of two toroidal shells covering the TF coils [m2]
    ! (inside and outside surfaces)
    ! = 2 * centroid coil length * 2 pi R, where R is average of i/b and o/b centres
    ! (This will possibly be used to replace 2*tfsai in the calculation of qss
    ! in subroutine cryo - not done at present.)
    tfcryoarea = 2.0D0 * tfleng * twopi*0.5D0*(r_tf_inboard_mid+r_tf_outboard_mid)

    ! Mass of ground-wall insulation [kg]
    ! (assumed to be same density/material as turn insulation)
    whtgw = tfleng * (awpc-awptf) * dcondins

    ! Superconductor coil design specific calculation
    ! ---
    if ( i_tf_sup == 1 ) then

        ! Mass of case [kg]
        ! ***
        ! The length of the vertical section is that of the first (inboard) segment
        cplen = 2.0D0*(radctf(1) + 0.5D0*tfcth) * dthet(1)
        
        ! The 2.2 factor is used as a scaling factor to fit
        ! to the ITER-FDR value of 450 tonnes; see CCFE note T&M/PKNIGHT/PROCESS/026
        whtcas = 2.2D0 * dcase * (cplen * acasetf + (tfleng-cplen) * acasetfo)
        ! ***
        

        ! Masses of conductor constituents
        !---------------------------------    
        ! Superconductor mass [kg]
        ! Includes space allowance for central helium channel, area awphec
        whtconsc = (tfleng * n_tf_turn * acstf*(1.0D0-vftf) * &
                   (1.0D0-fcutfsu) - tfleng*awphec) * dcond(i_tf_sc_mat)

        ! Copper mass [kg]
        whtconcu = (tfleng * n_tf_turn * acstf*(1.0D0-vftf) * fcutfsu - tfleng*awphec) * dcopper
        if ( whtconcu <= 0.0D0 ) whtconcu = 0.0D0

        ! Steel conduit (sheath) mass [kg]
        whtconsh = tfleng * n_tf_turn * acndttf * denstl

        ! Conduit insulation mass [kg]
        ! (aiwp already contains n_tf_turn)
        whtconin = tfleng * aiwp * dcondins

        ! Total conductor mass [kg]
        whtcon = whtconsc + whtconcu + whtconsh + whtconin
        !---------------------------------
    
        ! Total TF coil mass [kg] (all coils)
        whttf = (whtcas + whtcon + whtgw) * n_tf

    ! Resitivive magnets weights
    ! ---
    ! Rem SK : No casing for the outboard leg is considered for now !
    else 
        
        ! Volumes
        ! -------
        ! CP with joints
        ! ---    
        if ( itart == 1 ) then

            ! Total volume of one outerleg [m3]
            voltfleg = tfleng * arealeg

            ! Outboard leg TF conductor volume [m3]
            vol_cond_leg = tfleng * a_leg_cond

            ! Total TF conductor volume [m3]
            vol_cond = vol_cond_cp + n_tf * vol_cond_leg

            ! Outboard leg TF turn insulation layer volume (per leg) [m3]
            vol_ins_leg = tfleng * a_leg_ins

            ! Total turn insulation layer volume [m3]
            vol_ins = vol_ins_cp + n_tf * vol_ins_leg

            ! Ouboard leg TF ground insulation layer volume (per leg) [m3]
            vol_gr_ins_leg = tfleng * a_leg_gr_ins

            ! Total ground insulation layer volume [m3]
            vol_gr_ins = vol_gr_ins_cp + n_tf * vol_gr_ins_leg

            ! Total volume of the CP casing [m3]
            ! Rem : no outer leg case
            vol_case = vol_case_cp
        
        ! No joints
        ! ---    
        else 
            ! Total TF outer leg conductor volume [m3]
            vol_cond = tfleng * a_leg_cond * n_tf 
    
            ! Total turn insulation layer volume [m3]
            vol_ins = tfleng * a_leg_ins * n_tf

            ! Total ground insulation volume [m3]
            vol_gr_ins = tfleng * a_leg_gr_ins * n_tf

            ! Total case volume [m3]
            vol_case = tfleng * acasetf * n_tf
        end if
        ! ---    
        ! -------


        ! Copper magnets casing/conductor weights per coil [kg]
        if ( i_tf_sup == 0 ) then 

            whtcas = denstl * vol_case / n_tf  ! Per TF leg, no casing for outer leg
            whtconcu = dcopper * vol_cond / n_tf
            whtconal = 0.0D0         

            ! Outer legs/CP weights
            if ( itart == 1 ) then

                ! Weight of all the TF legs
                whttflgs = n_tf * ( dcopper * vol_cond_leg &
                                  + dcondins * ( vol_ins_leg + vol_gr_ins_leg ) )

                ! CP weight 
                whtcp = dcopper * vol_cond_cp &
                      + dcondins * ( vol_ins_cp + vol_gr_ins_cp ) &
                      + vol_case_cp * denstl 
            end if

        ! Cryo-aluminium conductor weights
        ! Casing made of re-inforced aluminium alloy
        else if ( i_tf_sup == 2 ) then
            
            ! Casing weight (CP only if itart = 1)bper leg/coil
            whtcas = dalu * vol_case / n_tf
            whtconcu = 0.0D0            
            whtconal = dalu * vol_cond / n_tf

            ! Outer legs/CP weights
            if ( itart == 1 ) then

                ! Weight of all the TF legs
                whttflgs = n_tf * ( dalu * vol_cond_leg &
                                  + dcondins * ( vol_ins_leg + vol_gr_ins_leg ) )

                ! CP weight 
                whtcp = dalu * vol_cond_cp &
                      + dcondins * ( vol_ins_cp + vol_gr_ins_cp ) &
                      + vol_case_cp * denstl 
            end if
        end if

        ! Turn insulation mass [kg]
        whtconin = dcondins * vol_ins / n_tf
            
        ! Ground wall insulation layer weight
        whtgw = dcondins * vol_gr_ins / n_tf

        ! Total weight
        whttf = (whtcas + whtconcu + whtconal + whtconin + whtgw ) * n_tf

    end if 
    ! ---

end subroutine tf_coil_area_and_masses

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine peak_tf_with_ripple(n_tf,wwp1,dr_tf_wp,tfin,bmaxtf,bmaxtfrp,flag)

    !! Peak toroidal field on the conductor
    !! author: P J Knight, CCFE, Culham Science Centre
    !! tfno : input real : number of TF coils
    !! wwp1 : input real : width of plasma-facing face of winding pack (m)
    !! dr_tf_wp : input real : radial thickness of winding pack (m)
    !! tfin : input real : major radius of centre of winding pack (m)
    !! bmaxtf : input real : nominal (axisymmetric) peak toroidal field (T)
    !! bmaxtfrp : output real : peak toroidal field including ripple (T)
    !! flag : output integer : flag warning of applicability problems
    !! This subroutine calculates the peak toroidal field at the
    !! outboard edge of the inboard TF coil winding pack, including
    !! the effects of ripple.
    !! <P>For 16, 18 or 20 coils, the calculation uses fitting formulae
    !! derived by M. Kovari using MAGINT calculations on coil sets based
    !! on a DEMO1 case.
    !! <P>For other numbers of coils, the original estimate using a 9%
    !! increase due to ripple from the axisymmetric calculation is used.
    !! M. Kovari, Toroidal Field Coils - Maximum Field and Ripple -
    !! Parametric Calculation, July 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: pi
    implicit none

    !  Arguments

    real(dp), intent(in) :: n_tf,wwp1,dr_tf_wp,tfin,bmaxtf
    real(dp), intent(out) :: bmaxtfrp
    integer, intent(out) :: flag

    !  Local variables

    real(dp) :: wmax
    real(dp), dimension(4) :: a

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    flag = 0

    !  Set fitting coefficients for different numbers of TF coils

    select case (nint(n_tf))

    case (16)
        a(1) =  0.28101D0
        a(2) =  1.8481D0
        a(3) = -0.88159D0
        a(4) =  0.93834D0

    case (18)
        a(1) =  0.29153D0
        a(2) =  1.81600D0
        a(3) = -0.84178D0
        a(4) =  0.90426D0

    case (20)
        a(1) =  0.29853D0
        a(2) =  1.82130D0
        a(3) = -0.85031D0
        a(4) =  0.89808D0

    case default

        !  Original calculation - no fits were performed
        bmaxtfrp = 1.09D0 * bmaxtf
        return

    end select

    !  Maximum winding pack width before adjacent packs touch
    !  (ignoring the external case and ground wall thicknesses)

    wmax = (2.0D0 * tfin + dr_tf_wp) * tan(pi/n_tf)

    !  Dimensionless winding pack width

    tf_fit_t = wwp1/wmax
    if ((tf_fit_t < 0.3D0).or.(tf_fit_t > 1.1D0)) then
        !write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; t = ',t
        flag = 1
    end if

    !  Dimensionless winding pack radial thickness

    tf_fit_z = dr_tf_wp/wmax
    if ((tf_fit_z < 0.26D0).or.(tf_fit_z > 0.7D0)) then
        !write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; z = ',z
        flag = 2
    end if

    !  Ratio of peak field with ripple to nominal axisymmetric peak field

    tf_fit_y = a(1) + a(2)*exp(-tf_fit_t) + a(3)*tf_fit_z + a(4)*tf_fit_z*tf_fit_t

    bmaxtfrp = tf_fit_y * bmaxtf

end subroutine peak_tf_with_ripple

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine stresscl( n_tf_layer, n_radial_array, iprint, outfile )

    !! TF coil stress routine
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! author: S Kahn, CCFE, Culham Science Centre
    !! author: J Galambos, FEDC/ORNL
    !! This subroutine sets up the stress calculations for the
    !! TF coil set.
    !! PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use build_variables, only: tfcth, r_tf_inboard_mid, bore, ohcth, hmax, &
        r_tf_inboard_in
    use tfcoil_variables, only: eyzwp, casestr, windstrain, n_tf_turn, &
        dr_tf_wp, i_tf_tresca, acstf, vforce, &
        ritfc, jwptf, strtf0, strtf1, strtf2, &
        thwcndut, insstrain, strtf2, vforce, tinstf, &
        acstf, jwptf, insstrain, &
        strtf1, rbmax, thicndut, acndttf, tfinsgap, &
        acasetf, alstrtf, poisson_steel, poisson_copper, poisson_al, &
        n_tf_graded_layers, i_tf_sup, i_tf_bucking, fcoolcp, eyoung_winding, &
        eyoung_steel, eyoung_res_tf_buck, eyoung_ins, eyoung_al, eyoung_copper, &
        aiwp, aswp, cpttf, n_tf, i_tf_plane_stress
    use pfcoil_variables, only : ipfres, oh_steel_frac, ohhghf, coheof, &
        cohbop, ncls, cptdin
    use constants, only: pi, sig_file
    use error_handling, only: report_error
    implicit none

    !  Arguments

    ! Inputs
    ! ------
    integer, intent(in) :: iprint
    !! Print option (if 1, output quantities calculated)
    
    integer, intent(in) :: outfile
    !! output file unit

    integer, intent(in) :: n_radial_array
    !! Size of the arrays per layers storing the radial dependent 
    !! stress quantities (stresses, strain displacement etc..)
    
    integer, intent(in) :: n_tf_layer
    !! Number of layers considered for the inboard TF stress calculations
    ! ------


    ! Internal parameters
    ! ---
    real(dp), dimension(n_tf_layer*n_radial_array) :: radial_array
    !! Array refining the radii of the stress calculations arrays
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: sig_tf_smeared_r
    !! TF Inboard leg radial smeared stress r distribution at mid-plane [Pa]
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: sig_tf_smeared_t
    !! TF Inboard leg tangential smeared stress r distribution at mid-plane [Pa]

    real(dp), dimension(n_tf_layer*n_radial_array) :: sig_tf_smeared_z
    !! TF Inboard leg vertical smeared stress r distribution at mid-plane [Pa]
    
    real(dp), dimension((n_tf_layer-i_tf_bucking)*n_radial_array) :: sig_tf_wp_av_z
    !! TF Inboard leg WP smeared vertical stress r distribution at mid-plane [Pa]

    real(dp), dimension(n_tf_layer*n_radial_array) :: sig_tf_r
    !! TF Inboard leg radial stress r distribution at mid-plane [Pa]
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: sig_tf_t
    !! TF Inboard leg tangential stress r distribution at mid-plane [Pa]
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: sig_tf_z
    !! TF Inboard leg vertical tensile stress at mid-plane [Pa]
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: deflect
    !! TF coil radial deflection (displacement) radial distribution [m]
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: sig_tf_vmises
    !! TF Inboard leg Von-Mises stress r distribution at mid-plane [Pa]
        
    real(dp), dimension(n_tf_layer*n_radial_array) :: sig_tf_tresca 
    !! TF Inboard leg TRESCA stress r distribution at mid-plane [Pa]
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: s_tresca_cond_cea
    !! Conduit Tresca stress with CEA adjustment factors [Pa]
    
    real(dp), dimension(n_tf_layer) :: sig_tf_r_max
    !! Radial stress of the point of maximum TRESCA stress (for each layers) [Pa]
    
    real(dp), dimension(n_tf_layer) :: sig_tf_t_max 
    !! Toroidal stress of the point of maximum TRESCA stress (for each layers) [Pa]
    
    real(dp), dimension(n_tf_layer) :: sig_tf_z_max
    !! Vertical stress of the point of maximum TRESCA stress (for each layers) [Pa]
    !! Rem : Currently constant but will be r dependent in the future
    
    real(dp), dimension(n_tf_layer) :: sig_tf_vmises_max 
    !! Von-Mises stress of the point of maximum TRESCA stress (for each layers) [Pa]
    
    real(dp), dimension(n_tf_layer) :: sig_tf_tresca_max
    !! Maximum TRESCA stress (for each layers) [Pa]
    !! If the CEA correction is addopted, the CEA corrected value is used
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: strain_tf_r
    !! Radial normal strain radial distribution
    
    real(dp), dimension(n_tf_layer*n_radial_array) :: strain_tf_t
    !! Toroidal normal strain radial distribution
     
    real(dp), dimension(n_tf_layer*n_radial_array) :: strain_tf_z
    !! Vertical normal strain radial distribution

    real(dp) :: eyoung_wp_t
    !! Effective WP young modulus in the toroidal direction

    real(dp) :: eyoung_wp_z
    !! Smeared WP young modulus in the vertical direction [Pa]

    real(dp) :: eyoung_wp_z_eff
    !! Effective WP young modulus used in the stress calculations [Pa]

    real(dp), dimension(n_tf_layer+1) :: radtf
    !! Radii used to define the layers used in the stress models [m]
    !! Layers are labelled from inboard to outbard
    
    real(dp), dimension(n_tf_layer) :: eyoung_p
    !! Toroidal plan's Young modulae (one per layer) used in the stress models [Pa]
    
    real(dp), dimension(n_tf_layer) :: eyoung_z
    !! Vertical direction's Young modulae (one per layer) used in the stress models [Pa]
    
    real(dp), dimension(n_tf_layer) :: poisson_p
    !! Toroidal plan's Poisson's ratio (one per layer) used in the stress models
    
    real(dp), dimension(n_tf_layer) :: poisson_z
    !! Toroidal plan's Poisson's ratio (one per layer) used in the stress models

    real(dp), dimension(n_tf_layer) :: jeff
    !! Effective current density [A/m2]
  
    integer :: n_tf_bucking
    !! Number of layers without currents in stress calculation

    integer :: ii, jj
    !! do loop indexes

    integer :: ii_max
    !! Index of the maximum TRESCA stress

    real(dp) :: sig_max
    !! Working float to find maximum TRESCA stress index [Pa]

    real(dp) :: tcbs
    !! Radial cable dimension [m]

    real(dp) :: t_ins_eff
    !! Effective insulation thickness (turn + ground insulation per turn) [m]

    real(dp) :: a_oh
    !! CS vertical cross-section area [m2]

    real(dp) :: curr_oh_max
    !! Maximum CS current (absolute value) [A]

    real(dp) :: n_oh_turns
    !! Number of CS turn (float ...)

    real(dp) :: a_oh_turn
    !! CS turn vertica cross section area [m]

    real(dp) :: t_cond_oh
    !! Central Solenoid (OH) conduit thickness assuming square conduit [m]
    !! Used only for bucked and wedged design
    
    real(dp) :: t_cable_oh
    !! Central Solenoid (OH) turn cable thickness assming square conduit [m]
    !! Used only for bucked and wedged design

    real(dp) :: t_turn_oh
    !! Central Solenoid (OH) turn dimension [m]

    real(dp) :: fac_sig_t
    !! Toroidal WP steel conduit stress unsmearing factor

    real(dp) :: fac_sig_r
    !! Radial WP steel conduit stress unsmearing factor

    real(dp) :: fac_sig_z
    !! Vertical WP steel conduit stress unsmearing factor

    real(dp) :: fac_sig_z_wp_av
    !! WP averaged vertical stress unsmearing factor

    real(dp) :: fac_oh
    !! Central Solenoid (OH) steel conduit stress unsmearing factor

    real(dp) :: svmxz
    !! Von-mises stress in steel setting the radial stress to 0

    real(dp) :: svmyz
    !! Von-mises stress in stell setting the toroidal stress to 0

    real(dp) :: dr_wp_layer
    !! Size of WP layer with homogeneous smeared property 

    real(dp) :: a_steel_eff
    !! Effective coil steel area used in stress calculations [m2]
    !! defined as the total steel area - the front casing 

    real(dp) :: a_wp_steel_eff
    !! Winding pack stress layer effective steel area [m2] 
    !! WP steel + latera casing area

    real(dp) :: a_wp_eff
    !! WP area using the stress model circular geometry [m2]
    !! WP + lateral casing area

    real(dp) :: f_vforce_case
    !! Correction factor for plasma side case vertical stress contribution

    real(dp) :: vforce_eff
    !! Effective vertical tension used in stess calculation [N]

    real(dp) :: eyoung_wp_t_eff
    !! WP young modulus in toroidal direction with lateral casing effect [Pa]

    real(dp) :: r_wp_inner_eff
    !! Inner radius of the stress model effective WP layer [m]
    
    real(dp) :: r_wp_outer_eff
    !! Inner radius of the stress model effective WP layer [m]

    real(dp) :: dr_tf_wp_eff
    !! Width of the effective WP layer used in the stress calculations [m] 
    ! ---
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Stress model not valid the TF does not contain any hole
    ! Rem SK : Can be easily ameneded playing around the boundary conditions
    if ( abs(r_tf_inboard_in) < epsilon(r_tf_inboard_in) ) then
        call report_error(245)
        strtf1 = 0.0D0
        strtf2 = 0.0D0
        return
    end if


    if (acstf >= 0.0D0) then
        tcbs = sqrt(acstf)
    else
        tcbs = 0.0D0
    end if



    ! LAYER ELASTIC PROPERTIES
    ! ------------------------
    ! Number of bucking layers
    n_tf_bucking = i_tf_bucking

    ! CS properties (bucked and wedged)
    ! ---
    if ( i_tf_bucking >= 2 ) then

        ! Calculation performed at CS flux swing (no current on CS)
        jeff(1) = 0.0D0

        ! Inner radius of the CS
        radtf(1) = bore

        ! Superconducting CS
        if ( ipfres == 0 ) then

            ! Getting the turn dimention from scratch 
            ! as the TF is called before CS in caller.f90
            !-!

            ! CS vertical cross-section area [m2]
            a_oh = 2.0D0 * hmax * ohhghf * ohcth

            ! Maximum current in Central Solenoid, at either BOP or EOF [MA-turns]
            ! Absolute value
            curr_oh_max = 1.0D-6*max(coheof,cohbop)*a_oh
    
            !  Number of turns
            n_oh_turns = 1.0D6 * curr_oh_max / cptdin(sum(ncls))
    
            ! CS Turn vertical cross-sectionnal area    
            a_oh_turn = a_oh / n_oh_turns
    
            ! Central Solenoid (OH) turn dimension [m]
            t_turn_oh = sqrt( a_oh_turn )
    
            ! OH/CS conduit thickness calculated assuming square conduit [m]  
            ! The CS insulation layer is assumed to the same as the TF one
            t_cond_oh = 0.5D0*(t_turn_oh - 2.0D0*thicndut - &
                               sqrt( (2.0D0*thicndut - t_turn_oh)**2 - &
                               oh_steel_frac * t_turn_oh**2) )

            ! CS turn cable space thickness
            t_cable_oh = t_turn_oh - 2.0D0 * ( t_cond_oh + thicndut )
            !-!

            ! Effective young modulus assuming the parallel case
            ! Rem the oh_steel_fraction is potentially a volumic one ... To be checked 
            eyoung_p(1) = oh_steel_frac * eyoung_steel + (1.0D0 - oh_steel_frac) * eyoung_winding
            eyoung_z(1) = eyngeff( eyoung_steel, eyoung_ins, thicndut, t_cond_oh, t_cable_oh )
            poisson_p(1) = poisson_steel
            poisson_z(1) = poisson_steel

        ! resistive CS (copper)
        else
            ! Here is a rough approximation
            eyoung_p(1) = eyoung_copper
            eyoung_z(1) = eyoung_copper
            poisson_p(1) = poisson_copper
            poisson_z(1) = poisson_copper
        end if
    end if
    ! ---


    ! CS-TF interlayer properties
    ! ---
    if ( i_tf_bucking == 3 ) then

        ! No current in this layer
        jeff(2) = 0.0D0

        ! Outer radius of the CS
        radtf(2) = bore + ohcth

        ! Assumed to be Kapton for the moment
        ! Ref : https://www.dupont.com/content/dam/dupont/products-and-services/membranes-and-films/polyimde-films/documents/DEC-Kapton-summary-of-properties.pdf
        eyoung_p(2) = 2.5D9
        eyoung_z(2) = 2.5D9
        poisson_p(2) = 0.34D0  ! Default value for young modulus
        poisson_z(2) = 0.34D0  ! Default value for young modulus
    end if 
    ! ---

    
    ! bucking cylinder/casing properties
    ! ---
    if ( i_tf_bucking >= 1 ) then
        
        ! No current in bucking cylinder/casing
        jeff(n_tf_bucking) = 0.0D0

        if ( i_tf_sup == 1 ) then 
            eyoung_p(n_tf_bucking) = eyoung_steel
            eyoung_z(n_tf_bucking) = eyoung_steel
            poisson_p(n_tf_bucking) = poisson_steel
            poisson_z(n_tf_bucking) = poisson_steel

        ! Bucking cylinder properties
        else 
            eyoung_p(n_tf_bucking) = eyoung_res_tf_buck
            eyoung_z(n_tf_bucking) = eyoung_res_tf_buck
            poisson_p(n_tf_bucking) = poisson_steel ! Seek better value !
            poisson_z(n_tf_bucking) = poisson_steel ! Seek better value !
        end if
        
        ! Innernost TF casing radius
        radtf(n_tf_bucking) = r_tf_inboard_in
    end if
    !---

 
    ! (Super)conductor layer properties
    ! ---
    ! Rem : They are only unique for isotropic materials, hence
    !       the underlying assumption of our models is the anisotropy
    !       of the material. A good assumption for resistive magnets
    !       but a doggy one for SC


    ! Resistive coil
    if ( i_tf_sup == 0 ) then

        ! No vertical force is assumed to be on the plasma side TF tube 
        f_vforce_case = 1.0D0

        ! Effective WP young modulus in the toroidal direction [Pa]
        ! Rem : effect of cooling pipes not taken into account for now
        eyoung_wp_t_eff = eyoung_copper
        eyoung_wp_t = eyoung_copper

        ! Effective conductor region young modulus in the vertical direction [Pa]
        eyoung_wp_z_eff = eyoung_copper * (1.0D0 - fcoolcp)

        ! Effect conductor layer inner/outer radius
        r_wp_inner_eff = r_wp_inner
        r_wp_outer_eff = r_wp_outer

    ! SC coil
    else if ( i_tf_sup == 1 ) then

        ! Inner/outer radii of the layer representing the WP in stress calculations [m2]
        r_wp_inner_eff = r_wp_inner * sqrt( tan_theta_coil / theta_coil )
        r_wp_outer_eff = r_wp_outer * sqrt( tan_theta_coil / theta_coil )
        
        ! Area of the cylinder representing the WP in stress calculations [m2]
        a_wp_eff = ( r_wp_outer_eff**2 - r_wp_inner_eff**2 ) * theta_coil
       
        ! Steel cross-section under the area considered in stress calculations [m2] 
        a_steel_eff = a_tf_steel - a_case_front

        ! Steel cross-section under the area representing the WP in stress calculations [m2]
        a_wp_steel_eff = a_steel_eff - a_case_nose
        
        ! Fraction of the vertical force to be considered in the stress calculation 
        f_vforce_case = (eyoung_steel*a_steel_eff + eyoung_ins*a_tf_ins) &
                      / (eyoung_steel*a_tf_steel  + eyoung_ins*a_tf_ins)

        ! WP effective insulation thickness (SC only) [m]
        ! include groundwall insulation + insertion gap in thicndut
        ! inertion gap is tfinsgap on 4 sides
        t_ins_eff = thicndut + ( tfinsgap + tinstf ) / n_tf_turn

        ! Effective WP young modulus in the toroidal direction [Pa] 
        ! The toroidal property drives the stress calculation (J. Last report no 4)
        ! Hence, the radial direction is relevant for the property smearing 
        ! Rem : This assumption might be re-defined for bucked and wedged design
        eyoung_wp_t = eyngeff( eyoung_steel, eyoung_ins, &
                               t_ins_eff, thwcndut, t_cable_radial )
        
        ! Lateral casing correction (serie)
        eyoung_wp_t_eff = ( 2.0D0 * t_lat_case_av + t_wp_toroidal_av ) &
                        / ( 2.0D0 * t_lat_case_av / eyoung_steel  &
                          + t_wp_toroidal_av / eyoung_wp_t )

        ! Average young WP modulus [Pa]
        eyoung_wp_z = ( eyoung_steel * aswp + eyoung_ins * a_tf_ins ) / awpc

        ! Average young modulus used in the WP layer stress calculation [Pa]
        eyoung_wp_z_eff = ( eyoung_steel * a_wp_steel_eff + eyoung_ins * a_tf_ins ) &
                        / a_wp_eff 

    ! Aluminium coil
    else if ( i_tf_sup == 2 ) then

        ! No vertical force is assumed to be on the plasma side TF tube 
        f_vforce_case = 1.0D0

        ! Effective WP young modulus in the toroidal direction [Pa]
        ! Rem : effect of cooling pipes and insulation not taken into account 
        !       for now as it needs a radially dependent Young modulus
        eyoung_wp_t_eff = eyoung_al
        eyoung_wp_t = eyoung_al

        ! WP area using the stress model circular geometry (per coil) [m2]
        a_wp_eff = (r_wp_outer**2 - r_wp_inner**2) * theta_coil

        ! Effective conductor region young modulus in the vertical direction [Pa]
        eyoung_wp_z_eff = eyoung_ins * a_tf_ins / a_wp_eff &
                        + eyoung_al * (1.0D0 - a_tf_ins / a_wp_eff) * (1.0D0 - fcoolcp)

        ! Effect conductor layer inner/outer radius
        r_wp_inner_eff = r_wp_inner
        r_wp_outer_eff = r_wp_outer

    end if 

    ! Effective total inboard vertical tension for stress calculation [N]
    vforce_eff = vforce_inboard_tot * f_vforce_case

    ! Thickness of the layer representing the WP in stress calcualtions [m]
    dr_tf_wp_eff = r_wp_outer_eff - r_wp_outer_eff

    ! Thickness of WP with homogeneous stress property [m]
    dr_wp_layer = dr_tf_wp_eff / dble(n_tf_graded_layers)

    ! Loop on layers
    do ii = 1, n_tf_graded_layers
        
        ! Homogeneous current in (super)conductor
        jeff(n_tf_bucking + ii) = ritfc / (pi * (r_wp_outer_eff**2 - r_wp_inner_eff**2))

        ! Same thickness for all WP layers in stress calculation
        radtf(n_tf_bucking + ii) = r_wp_inner_eff + dble(ii-1)*dr_wp_layer

        ! Young modulus
        eyoung_p(n_tf_bucking + ii) = eyoung_wp_t_eff
        eyoung_z(n_tf_bucking + ii) = eyoung_wp_z_eff

        ! Poisson's ratio
        if ( i_tf_sup == 0 ) then
            poisson_p(n_tf_bucking + ii) = poisson_copper
            poisson_z(n_tf_bucking + ii) = poisson_copper
            
        ! SC magnets smeared properties
        else if ( i_tf_sup == 1 ) then 
            poisson_p(n_tf_bucking + ii) = poisson_steel
            poisson_z(n_tf_bucking + ii) = poisson_steel
        
        ! Aluminium properties
        else 
            poisson_p(n_tf_bucking + ii) = poisson_al
            poisson_z(n_tf_bucking + ii) = poisson_al
        end if 
    end do

    ! last layer radius
    radtf(n_tf_layer + 1) = r_wp_outer_eff
    ! ---  
    ! ------------------------



    ! RADIAL STRESS SUBROUTINES CALL 
    ! ------------------------------
    ! Stress model not valid the TF does not contain any hole
    ! Current action : trigger and error and add a little hole
    !                  to allow stress calculations 
    ! Rem SK : Can be easily ameneded playing around the boundary conditions
    if ( abs(radtf(1)) < epsilon(radtf(1)) ) then
        call report_error(245)
        radtf(1) = 1.0D-9
    end if
    ! ---


    ! Old generalized plane stress model
    ! ---
    if ( i_tf_plane_stress == 1 ) then

        ! Plane stress calculation (SC) [Pa]
        call plane_stress( poisson_p, radtf, eyoung_p, jeff, & ! Inputs
                           n_tf_layer, n_radial_array,       & ! Inputs
                           sig_tf_r, sig_tf_t, deflect, radial_array ) ! Outputs
    
        ! Vertical stress [Pa]  
        sig_tf_z = vforce / (acasetf + acndttf*n_tf_turn) ! Array equation

        ! Case strain
        casestr = sig_tf_z(n_tf_bucking) / eyoung_steel

        ! Young's modulus in vertical direction on WP
        eyzwp = eyngzwp(eyoung_steel,eyoung_ins,eyoung_winding,t_ins_eff,thwcndut,tcbs)
    
        ! Strain in vertical direction on WP
        windstrain = sig_tf_z(n_tf_bucking+1) / eyzwp
    
        ! Radial strain in insulator
        insstrain = sig_tf_r(n_radial_array) / eyoung_ins * &
                    edoeeff(eyoung_steel, eyoung_ins, t_ins_eff, thwcndut, tcbs)
    ! ---


    ! New generalized plane strain formulation
    ! ---
    else 
        ! Generalized plane strain calculation [Pa]
        call generalized_plane_strain( poisson_p, poisson_z, eyoung_p, eyoung_z,  & ! Inputs
                                       radtf, jeff, vforce_eff,                   & ! Inputs
                                       n_tf_layer, n_radial_array, n_tf_bucking,  & ! Inputs
                                       radial_array, sig_tf_r, sig_tf_t, sig_tf_z,    & ! Outputs
                                       strain_tf_r, strain_tf_t, strain_tf_z, deflect ) ! Outputs
    end if
    ! ---

    ! Storing the smeared properties for output
    if ( iprint == 1 ) then
        sig_tf_smeared_r = sig_tf_r   ! Array equation
        sig_tf_smeared_t = sig_tf_t   ! Array equation
        sig_tf_smeared_z = sig_tf_z   ! Array equation
    end if
    ! ------------------------------



    ! STRESS DISTRIBUTIONS CORRECTIONS
    ! --------------------------------
    ! SC central solenoid coil stress unsmeating (bucked and wedged only)
    ! --- 
    if ( i_tf_bucking >= 2 .and. ipfres == 0 ) then

        ! Central Solenoid (OH) steel conduit stress unsmearing factor
        fac_oh = 0.5D0 * t_turn_oh / t_cond_oh 

        do ii = 1, n_radial_array

            ! CS (OH) superconducting case stress unsmearing
            sig_tf_r(ii) = sig_tf_r(ii) * fac_oh
            sig_tf_t(ii) = sig_tf_t(ii) / oh_steel_frac
            sig_tf_z(ii) = sig_tf_z(ii) * fac_oh
        end do
    end if
    ! ---


    ! No TF vertical forces on CS and CS-TF layer (bucked and wedged only)
    ! ---
    ! This correction is only applied if the plane stress model is used
    ! as the generalized plane strain calculates the vertical stress properly
    if ( i_tf_bucking >= 2 .and. i_tf_plane_stress == 1 ) then
        do ii = 1, (n_tf_bucking-1)*n_radial_array
            sig_tf_z(ii) = 0.0D0
        end do
    end if
    ! ---


    ! Toroidal coil unsmearing
    ! ---
    ! Copper magnets
    if ( i_tf_sup == 0 ) then

        ! Vertical force unsmearing factor
        fac_sig_z = eyoung_copper / eyoung_wp_z_eff

        ! Toroidal WP steel stress unsmearing factor
        fac_sig_t = 1.0D0
        fac_sig_r = 1.0D0

    else if ( i_tf_sup == 1 ) then

        ! Vertical WP steel stress unsmearing factor
        if ( i_tf_plane_stress == 0 ) then
            fac_sig_z = eyoung_steel / eyoung_wp_z_eff
            fac_sig_z_wp_av = eyoung_wp_z / eyoung_wp_z_eff
        else
            fac_sig_z = 1.0D0
        end if  

        ! Toroidal WP steel conduit stress unsmearing factor
        ! Rem : These correction factors are calculated with the genuine WP geom
        fac_sig_t = ( dr_tf_wp / ( dr_tf_wp - 2.0D0 * ( tinstf + tfinsgap ) ) ) & 
                  * ( 0.5D0 * t_turn_radial / thwcndut )

        ! Radial WP steel conduit stress unsmearing factor
        fac_sig_r = ( t_wp_toroidal_av / ( t_wp_toroidal_av - 2.0D0 * ( tinstf + tfinsgap ) ) ) & 
                  * ( 0.5D0 * t_turn_toroidal / thwcndut )

    else if ( i_tf_sup == 2 ) then
        
        ! Vertical WP steel stress unsmearing factor
        fac_sig_z = eyoung_al / eyoung_wp_z_eff

        ! Toroidal WP steel stress unsmearing factor
        ! NO CALCULTED FOR THE MOMENT (to be done later)
        fac_sig_t = 1.0D0
        fac_sig_r = 1.0D0
    
    end if
    
    ! Application of the corrections on the WP layers
    ! GRADED MODIF : add another do loop to allow the graded properties
    !                to be taken into account
    do ii = n_tf_bucking * n_radial_array + 1, n_tf_layer*n_radial_array  
        sig_tf_wp_av_z(ii - n_tf_bucking * n_radial_array) = sig_tf_z(ii) * fac_sig_z_wp_av
        sig_tf_r(ii) = sig_tf_r(ii) * fac_sig_r
        sig_tf_t(ii) = sig_tf_t(ii) * fac_sig_t
        sig_tf_z(ii) = sig_tf_z(ii) * fac_sig_z  
    end do
    ! ---


    ! TRESCA/VM stress calculations
    ! -----------------------------
    ! Array equation
    sig_tf_tresca = max( abs(sig_tf_r - sig_tf_t), &
                         abs(sig_tf_r - sig_tf_z), &
                         abs(sig_tf_z - sig_tf_t) )

    ! Array equation
    if ( iprint == 1 ) then
        sig_tf_vmises = sqrt( 0.5D0*(  (sig_tf_r - sig_tf_t)**2  &
                                     + (sig_tf_r - sig_tf_z)**2  &
                                     + (sig_tf_z - sig_tf_t)**2 ) )
    end if

    ! Array equation
    s_tresca_cond_cea = sig_tf_tresca


    ! SC condcuting layer stress distribution corrections
    ! ---
    if ( i_tf_sup == 1 ) then

        ! GRADED MODIF : add another do loop to allow the graded properties
        !                to be taken into account
        do ii = n_tf_bucking * n_radial_array + 1, n_tf_layer*n_radial_array
       
            ! Addaped Von-mises stress calculation to WP strucure [Pa]
            if ( iprint == 1 ) then
                svmxz = sigvm( 0.0D0, sig_tf_t(ii), sig_tf_z(ii), 0.0D0,0.0D0,0.0D0)
                svmyz = sigvm( sig_tf_r(ii), 0.0D0, sig_tf_z(ii), 0.0D0,0.0D0,0.0D0)
                sig_tf_vmises(ii) = max(svmxz, svmyz)
            end if

            ! TRESCA stress using CEA calculation [Pa]
            s_tresca_cond_cea(ii) = 1.02D0*abs(sig_tf_r(ii)) + 1.6D0*sig_tf_z(ii)
        end do
    end if
    ! ---
    ! -----------------------------



    ! Output formating : Maximum TRESCA per layers
    ! ----------------
    do ii = 1, n_tf_layer
        sig_max = 0.0D0
        ii_max = 0

        do jj = (ii-1)*n_radial_array + 1, ii*n_radial_array

            ! CEA out of plane approximation
            if ( i_tf_tresca == 1 .and. i_tf_sup == 1 .and. ii >= i_tf_bucking + 1 ) then
                if ( sig_max < s_tresca_cond_cea(jj) ) then
                    sig_max = s_tresca_cond_cea(jj)
                    ii_max = jj
                end if

            ! Conventionnal TRESCA
            else 
                if ( sig_max < sig_tf_tresca(jj) ) then
                    sig_max = sig_tf_tresca(jj)
                    ii_max = jj
                end if
            end if
        end do

        ! OUT.DAT output
        if ( iprint == 1 ) then
            sig_tf_r_max(ii) = sig_tf_r(ii_max)
            sig_tf_t_max(ii) = sig_tf_t(ii_max)
            sig_tf_z_max(ii) = sig_tf_z(ii_max)
            sig_tf_vmises_max(ii) = sig_tf_vmises(ii_max)
        end if

        ! Maximum TRESCA (or CEA OOP correction)
        if ( i_tf_tresca == 1 .and. i_tf_sup == 1 .and. ii >= i_tf_bucking + 1 ) then
            sig_tf_tresca_max(ii) = s_tresca_cond_cea(ii_max)
        else
            sig_tf_tresca_max(ii) = sig_tf_tresca(ii_max)
        end if
    end do

    ! Constrains equation TRESCA stress values
    strtf2 = sig_tf_tresca_max(n_tf_bucking + 1) ! Maximum assumed in the first graded layer
    if ( i_tf_bucking >= 1 ) strtf1 = sig_tf_tresca_max(n_tf_bucking)
    if ( i_tf_bucking >= 2 ) strtf0 = sig_tf_tresca_max(1)
    ! ----------------

    if ( iprint == 1 ) call out_stress

    contains
    subroutine out_stress
        !! Subroutine showing the writing the TF midplane stress analysis
        !! in the output file and the stress distribution in the SIG_TF.DAT
        !! file used to plot stress distributions
        !! Author : S. Kahn

        use process_output, only: osubhd, ocmmnt, oheadr, ovarre, int2char
        use constants, only: mfile
        implicit none

        character(len=1) :: intstring
        !! Char used for integer convertion to string
        
        ! Stress output section
        call oheadr(outfile,'TF coils ')
        call osubhd(outfile,'TF Coil Stresses (CCFE model) :')
        
        if ( i_tf_plane_stress == 1 ) then
            call ocmmnt(outfile, 'Plane stress model with smeared properties')
        else 
            call ocmmnt(outfile, 'Generalized plane strain model')
        end if

        call ovarre(outfile, 'Allowable Tresca stress limit (Pa)','(alstrtf)',alstrtf)
        if ( i_tf_tresca == 1  .and. i_tf_sup == 1) then
            call ocmmnt(outfile, 'WP conduit TRESCA stress corrected using CEA formula (i_tf_tresca = 1)')
        end if

        if ( i_tf_bucking >= 2) then
            call ocmmnt(outfile, 'No stress limit imposed on the TF-CS interface layer')
            call ocmmnt(outfile, '  -> Too much unknow on it material choice/properties')
        end if 

        ! OUT.DAT data on maximum TRESCA stress values
        call ocmmnt(outfile, 'Structural materal stress of the point of maximum TRESCA stress per layer')
        call ocmmnt(outfile, 'Please use utility/plot_TF_stress.py for radial plots plots summary')

        select case (i_tf_bucking)
            case (0)
                if (i_tf_sup == 1 ) then
                    write(outfile,'(t2, "Layers", t26, *(a11) )') "WP"
                else 
                    write(outfile,'(t2, "Layers", t26, *(a11) )') "conductor"
                end if
            case (1)
                if (i_tf_sup == 1 ) then
                    write(outfile,'(t2, "Layers", t26, *(a11) )') "Steel case", "WP"
                else 
                    write(outfile,'(t2, "Layers", t26, *(a11) )') "bucking", "conductor"
                end if
            case (2)
                if (i_tf_sup == 1 ) then
                    write(outfile,'(t2, "Layers", t26, *(a12) )') "CS", "Steel case", "WP"
                else 
                    write(outfile,'(t2, "Layers", t26, *(a12) )') "CS", "bucking", "conductor"
                end if
            case (3)
                if (i_tf_sup == 1 ) then

                    write(outfile,*) "Layers                         CS        interface    Steel case        WP"
                else 
                    write(outfile,*) "Layers                         CS        interface      bucking      conductor"
                end if
        end select
        
        write(outfile,'(t2, "Radial"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') &
              sig_tf_r_max*1.0D-6
        write(outfile,'(t2, "toroidal"  ," stress", t20, "(MPa)",t26, *(F11.3,3x))') &
              sig_tf_t_max*1.0D-6
        write(outfile,'(t2, "Vertical"  ," stress", t20, "(MPa)",t26, *(F11.3,3x))') &
              sig_tf_z_max*1.0D-6
        write(outfile,'(t2, "Von-Mises" ," stress", t20, "(MPa)",t26, *(F11.3,3x))') &
              sig_tf_vmises_max*1.0D-6
        if ( i_tf_tresca == 1 .and. i_tf_sup == 1 ) then
            write(outfile,'(t2, "CEA TRESCA"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_tresca_max*1.0D-6
        else 
            write(outfile,'(t2, "TRESCA"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_tresca_max*1.0D-6
        end if
        write(outfile, *) ''
        write(outfile,'(t2, "Toroidal"    ," modulus", t20, "(GPa)",t26, *(F11.3,3x))') eyoung_p * 1.0D-9
        write(outfile,'(t2, "Vertical"    ," modulus", t20, "(GPa)",t26, *(F11.3,3x))') eyoung_z * 1.0D-9
        write(outfile,* ) ''
        call ovarre(outfile,'WP toroidal modulus (GPa)','(eyoung_wp_t*1.0D-9)', eyoung_wp_t*1.0D-9, 'OP ')
        call ovarre(outfile,'WP vertical modulus (GPa)','(eyoung_wp_z*1.0D-9)', eyoung_wp_z*1.0D-9, 'OP ')

        ! MFILE.DAT data
        do ii = 1, n_tf_bucking + 1
            intstring = int2char(ii)    
            call ovarre(mfile,'Radial    stress at maximum TRESCA of layer '//intstring// &
                        ' (Pa)', '(sig_tf_r_max('//intstring//'))', sig_tf_r_max(ii) )
            call ovarre(mfile,'toroidal  stress at maximum TRESCA of layer '//intstring// &
                        ' (Pa)', '(sig_tf_t_max('//intstring//'))', sig_tf_t_max(ii) )
            call ovarre(mfile,'Vertical  stress at maximum TRESCA of layer '//intstring// &
                        ' (Pa)', '(sig_tf_z_max('//intstring//'))', sig_tf_z_max(ii) )
            call ovarre(mfile,'Von-Mises stress at maximum TRESCA of layer '//intstring// &
                        ' (Pa)', '(sig_tf_vmises_max('//intstring//'))', sig_tf_vmises_max(ii) )
            if ( i_tf_tresca == 1 .and. i_tf_sup == 1 ) then
                call ovarre(mfile,'Maximum CEA TRESCA stress '//intstring// &
                           ' (Pa)', '(sig_tf_tresca_max('//intstring//'))', sig_tf_tresca_max(ii) )
            else           
                call ovarre(mfile,'Maximum TRESCA stress '//intstring// &
                            ' (Pa)', '(sig_tf_tresca_max('//intstring//'))', sig_tf_tresca_max(ii) )
            end if
        end do

        ! SIG_TF.DAT storage  
        write(sig_file,'(t2, "Points per layers"                 ,t26, *(I11,3x))') n_radial_array          
        write(sig_file,*) 
        write(sig_file,'(t2, "radius"              , t20, "(m)"  ,t26, *(F11.3,3x))') radial_array
        write(sig_file,'(t2, "Radial"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_r*1.0D-6
        write(sig_file,'(t2, "toroidal"  ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_t*1.0D-6
        write(sig_file,'(t2, "Vertical"  ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_z*1.0D-6
        write(sig_file,'(t2, "Radial"    ," smeared stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_smeared_r*1.0D-6
        write(sig_file,'(t2, "toroidal"  ," smeared stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_smeared_t*1.0D-6
        write(sig_file,'(t2, "vertical"  ," smeared stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_smeared_z*1.0D-6
        write(sig_file,'(t2, "Von-Mises" ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_vmises*1.0D-6
        write(sig_file,'(t2, "TRESCA"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_tresca*1.0D-6
        if ( i_tf_sup == 1 ) then
            write(sig_file,'(t2, "CEA TRESCA"," stress", t20, "(MPa)",t26, *(F11.3,3x))') s_tresca_cond_cea*1.0D-6
        else 
            write(sig_file,'(t2, "TRESCA"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_tresca*1.0D-6
        end if 
        write(sig_file,*) 
        write(sig_file,*) 'Displacement'         
        write(sig_file,'(t2, "rad. displacement", t20, "(mm)",t26, *(F11.5,5x))') deflect*1.0D3
        if ( i_tf_plane_stress /= 1 ) then
            write(sig_file,*)
            write(sig_file,*) 'Strain'    
            write(sig_file,'(t2, "radial strain"   ,t26, *(F11.8,3x))') strain_tf_r
            write(sig_file,'(t2, "toroidal strain" ,t26, *(F11.8,3x))') strain_tf_t
            write(sig_file,'(t2, "vertical strain" ,t26, *(F11.8,3x))') strain_tf_z
        end if

        if ( i_tf_sup == 1 ) then
            write(sig_file,'(t2, "WP"    ," smeared stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_wp_av_z*1.0D-6
        end if 

        ! Other quantities (displacement strain, etc..)
        call ovarre(outfile,'Maximum radial deflection at midplane (m)','(deflect)',&
                            deflect(n_radial_array), 'OP ')
        call ovarre(outfile,'Vertical strain on casing','(casestr)', casestr, 'OP ')
        call ovarre(outfile,'Vertical strain on winding pack','(windstrain)', windstrain, 'OP ')
        call ovarre(outfile,'Radial strain on insulator','(insstrain)', insstrain, 'OP ')

    end subroutine out_stress

end subroutine stresscl

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine plane_stress( nu, rad, ey, j,          & ! Inputs
                         nlayers, n_radial_array, & ! Inputs
                         sigr, sigt, r_deflect, rradius ) ! Outputs

    !! Calculates the stresses in a superconductor TF coil
    !! inboard leg at the midplane using the plain stress approximation 
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! author: S Kahn, CCFE, Culham Science Centre
    !! This routine calculates the stresses in a superconductor TF coil
    !! inboard leg at midplane.
    !! <P>A 2 layer plane stress model developed by CCFE is used. The first layer
    !! is the steel case inboard of the winding pack, and the second
    !! layer is the winding pack itself.
    !! PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use constants, only: pi, rmu0
    use maths_library, only: linesolv
    implicit none

    !  Arguments

    integer, intent(in) :: n_radial_array
    !! Number of elements per layers used in stress analysis 
    !! quantities arrays (stress, strain, displacement) 

    integer, intent(in) :: nlayers
    !! Number of layers considered in the stress model

    real(dp), dimension(nlayers), intent(in) :: nu
    !! Poisson's ratio

    real(dp), dimension(nlayers+1), intent(in) :: rad
    !! Layers delimitation radii [m]
    
    real(dp), dimension(nlayers), intent(in) :: ey
    !! Young modulae [Pa]
    
    real(dp), dimension(nlayers), intent(in) :: j
    !! Layers effective current density [A/m2]

    real(dp), dimension(nlayers*n_radial_array), intent(out) :: sigr
    !! Radial stress radial distribution [Pa]
    
    real(dp), dimension(nlayers*n_radial_array), intent(out) :: sigt
    !! Toroidal stress radial distribution [Pa]

    real(dp), dimension(nlayers*n_radial_array), intent(out) :: r_deflect
    !! Radial deflection (displacement) radial distribution [m]

    real(dp), dimension(nlayers*n_radial_array), intent(out) :: rradius
    !! Radius array [m]


    ! Local variables
    ! ---
    ! Lorentz body force parametres
    real(dp), dimension(nlayers) :: alpha
    real(dp), dimension(nlayers) :: beta

    ! Strain to stress hooke's law coeficient
    real(dp), dimension(nlayers) :: kk

    ! Layer area
    real(dp), dimension(nlayers) :: area
    
    ! Matrix encoding the integration constant cc coeficients 
    real(dp), dimension(2*nlayers, 2*nlayers) :: aa
    
    ! Vector encoding the alpha/beta (lorentz forces) contribution
    real(dp), dimension(2*nlayers) :: bb

    ! Integration constants vector (solution)
    real(dp), dimension(2*nlayers) :: cc
    real(dp), dimension(nlayers) :: c1, c2

    ! Variables used for radial stress distribution  
    real(dp) :: dradius
    real(dp) :: inner_layer_curr
    real(dp) :: rad_c

    integer :: ii = 0
    integer :: jj = 0
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Layer parameterisation
    ! ***
    ! Array equation
    kk = ey/(1.0D0 - nu**2)

    ! Lorentz forces parametrisation coeficients (array equation)
    alpha = 0.5D0*rmu0 * j**2 / kk
    
    inner_layer_curr = 0.0D0
    do ii = 1, nlayers

        beta(ii) = 0.5D0*rmu0 * j(ii) * ( inner_layer_curr - pi*j(ii)*rad(ii)**2 ) / (pi*kk(ii))

        ! Layer area
        area(ii) = pi * (rad(ii+1)**2 - rad(ii)**2)

        ! Total current carried by the inners layers 
        inner_layer_curr = inner_layer_curr + area(ii)*j(ii)
    end do
    ! ***


    ! Left hand side matrix aa
    ! ***
    aa(:,:) = 0.0D0
    
    ! Null radial stress at R(1)
    aa(1,1) = kk(1) * (1.0D0+nu(1))
    aa(1,2) = -kk(1) * (1.0D0-nu(1))/(rad(1)**2)


    ! Inter-layer boundary conditions
    if ( nlayers /= 1 ) then 
        do ii = 1, nlayers - 1

            ! Continuous radial normal stress at R(ii+1)
            aa(2*ii, 2*ii-1) = kk(ii) * ( 1.0D0 + nu(ii) )
            aa(2*ii, 2*ii  ) = -kk(ii) * ( 1.0D0 - nu(ii) ) / rad(ii+1)**2 
            aa(2*ii, 2*ii+1) = -kk(ii+1) * ( 1.0D0 + nu(ii+1) )
            aa(2*ii, 2*ii+2) = kk(ii+1) * ( 1.0D0 - nu(ii+1) ) / rad(ii+1)**2 

            ! Continuous displacement at R(ii+1)
            aa(2*ii+1, 2*ii-1) = rad(ii+1)
            aa(2*ii+1, 2*ii  ) = 1.0D0 / rad(ii+1)
            aa(2*ii+1, 2*ii+1) = -rad(ii+1)
            aa(2*ii+1, 2*ii+2) = -1.0D0 / rad(ii+1)

        end do
    end if

    ! Radial stress = 0
    aa(2*nlayers, 2*nlayers - 1) =  kk(nlayers) * ( 1.0D0 + nu(nlayers) )
    aa(2*nlayers, 2*nlayers    ) = -kk(nlayers) * ( 1.0D0 - nu(nlayers) ) / rad(nlayers+1)**2
    ! ***

    ! Right hand side vector bb
    ! ***
    ! Null radial stress at R(1)
    bb(1) = -kk(1) * ( 0.125D0*alpha(1)*(3.0D0+nu(1))*rad(1)**2   &
                     + 0.5D0*beta(1)*(1.0D0 + (1.0D0+nu(1))*log(rad(1))) )

    ! Inter-layer boundary conditions
    if ( nlayers /= 1 ) then 
        do ii = 1, nlayers - 1

            ! Continuous radial normal stress at R(ii+1)
            bb(2*ii) = -kk(ii) * ( 0.125D0*alpha(ii)*(3.0D0+nu(ii))*rad(ii+1)**2   &
                                  + 0.5D0*beta(ii)*(1.0D0 + (1.0D0+nu(ii))*log(rad(ii+1))) ) &
                       +kk(ii+1) * ( 0.125D0*alpha(ii+1)*(3.0D0+nu(ii+1))*rad(ii+1)**2   &
                                  + 0.5D0*beta(ii+1)*(1.0D0 + (1.0D0+nu(ii+1))*log(rad(ii+1))) )

            ! Continuous displacement at R(ii+1)
            bb(2*ii+1) = - 0.125D0*alpha(ii)  * rad(ii+1)**3 - 0.5D0*beta(ii)  *rad(ii+1)*log(rad(ii+1))  &
                         + 0.125D0*alpha(ii+1)* rad(ii+1)**3 + 0.5D0*beta(ii+1)*rad(ii+1)*log(rad(ii+1))
        end do
    end if

    ! Null radial stress at R(nlayers+1)
    bb(2*nlayers) = -kk(nlayers) * ( 0.125D0*alpha(nlayers)*(3.0D0+nu(nlayers))*rad(nlayers+1)**2  &
                                   + 0.5D0*beta(nlayers)*(1.0D0 + (1.0D0+nu(nlayers))*log(rad(nlayers+1))) )
    ! ***

    !  Find solution vector cc
    ! ***
    cc(:) = 0.0D0
    call linesolv(aa, 2*nlayers, bb, cc)

    !  Multiply c by (-1) (John Last, internal CCFE memorandum, 21/05/2013)
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
    r_deflect(:) = 0.0D0

    do ii = 1, nlayers

        dradius = (rad(ii+1) - rad(ii)) / dble(n_radial_array)
        do jj = (ii-1)*n_radial_array + 1, ii*n_radial_array

            rad_c = rad(ii) + dradius*dble(jj - n_radial_array*(ii-1) - 1)
            rradius(jj) = rad_c


            ! Radial stress radial distribution [Pa]
            sigr(jj) = kk(ii) * ( (1.0D0+nu(ii))*c1(ii) - ((1.0D0-nu(ii))*c2(ii))/ rad_c**2 &
                                  + 0.125D0*(3.0D0 + nu(ii))*alpha(ii)* rad_c**2            &
                                  + 0.5D0*beta(ii)*(1.0D0 + (1.0D0+nu(ii))*log(rad_c)) )

            ! Radial stress radial distribution [Pa]
            sigt(jj) = kk(ii) * ( (1.0D0+nu(ii))*c1(ii) + (1.0D0-nu(ii))*c2(ii)/ rad_c**2 &
                                  + 0.125D0*(1.0D0+3.0D0*nu(ii))*alpha(ii)*rad_c**2       &
                                  + 0.5D0*beta(ii)*(nu(ii) + (1.0D0+nu(ii))*log(rad_c)) )

            !  Deflection [m]
            r_deflect(jj) = c1(ii)*rad_c + c2(ii)/rad_c      &
                              + 0.125D0*alpha(ii) * rad_c**3 &
                              + 0.5D0*beta(ii) * rad_c*log(rad_c)

        end do
    end do
   ! ---
 
end subroutine plane_stress

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine generalized_plane_strain( nu_p, nu_z, ey_p, ey_z, rad, d_curr, v_force,   & ! Inputs
                                     nlayers, n_radial_array, i_tf_bucking,          & ! Inputs
                                     rradius, sigr, sigt, sigz,              & ! Outputs
                                     strain_r, strain_t, strain_z, r_deflect ) ! Outputs
      
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
    !!   0 : No casing/bucking cylinder
    !!   1 : casing/buling cylinder
    !!   2 : Bucked and wedged design
    !!   3 : Bucked and wedged design with CS-TF interlayer

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

    real(dp), dimension(n_radial_array*nlayers), intent(out) :: strain_r
    !! Strain distribution in the radial direction (r)

    real(dp), dimension(n_radial_array*nlayers), intent(out) :: strain_t
    !! Strain distribution in the toroidal direction (t)
          
    real(dp), dimension(n_radial_array*nlayers), intent(out) :: strain_z
    !! Uniform strain in the vertical direction (z)

    real(dp), dimension(n_radial_array*nlayers), intent(out) :: r_deflect
    !! Radial displacement radial distribution [m]

    real(dp), dimension(nlayers*n_radial_array), intent(out) :: rradius
    !! Radius array [m]
    ! ---


    ! Local variables
    ! ---
    ! Lorentz body force parametres
    real(dp), dimension(nlayers) :: alpha
    real(dp), dimension(nlayers) :: beta
      
    ! Toroidal plane / vertical direction hooke's law coeficient
    real(dp), dimension(nlayers) :: kk_p
    real(dp), dimension(nlayers) :: kk_z

    ! Toroidal plan to vertical direction poisson's squared coefficient
    real(dp), dimension(nlayers) :: nu_z_eff2
    
    ! Body force parameter in displacement differential equation
    real(dp), dimension(nlayers) :: fr_par

    ! Radial/toroidal stress constant parameters
    real(dp), dimension(nlayers) :: cc_par_sig
    real(dp), dimension(nlayers) :: alpha_par_sigr
    real(dp), dimension(nlayers) :: alpha_par_sigt
    real(dp), dimension(nlayers) :: beta_par_sigr
    real(dp), dimension(nlayers) :: beta_par_sigt

    ! Layer area
    real(dp), dimension(nlayers) :: area

    ! Vertical strain parameters
    real(dp) :: sum_1
    real(dp) :: sum_2
    real(dp), dimension(nlayers) :: aleph
    real(dp), dimension(nlayers) :: beth
    real(dp), dimension(nlayers) :: par_1
    real(dp), dimension(nlayers) :: par_2

    ! Matrix encoding the integration constant cc coeficients 
    real(dp), dimension(2*nlayers, 2*nlayers) :: aa

    ! Vector encoding the alpha/beta (lorentz forces) contribution
    real(dp), dimension(2*nlayers) :: bb

    ! Integration constants vector (solution)
    real(dp), dimension(2*nlayers) :: cc
    real(dp), dimension(nlayers) :: c1, c2

    ! Variables used for radial stress distribution     
    real(dp) :: dradius  
    real(dp) :: inner_layer_curr
      
    ! Constraint strains for calculation (on for TF and CS systems)
    real(dp), dimension(2) :: strain_z_calc
      
    ! Indexes
    integer :: ii = 0  ! Line in the aa matrix
    integer :: jj = 0  ! Collumn in the aa matrix 
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
    strain_r(:) = 0.0D0
    strain_t(:) = 0.0D0
    strain_z(:) = 0.0D0
    r_deflect(:) = 0.0D0

    ! CS system vertical strain
    if ( i_tf_bucking >= 2 ) then
        strain_z_calc(1) = aleph(1)
        do ii = 1, i_tf_bucking - 1
            strain_z_calc(1) = strain_z_calc(1) + c1(ii) * beth(ii)
        end do
    end if
    
    ! TF system vertical normal strain (constant) WRONG IF GRADED COIL
    strain_z_calc(2) = aleph(nlayers) 
    do ii = max( 1, i_tf_bucking ), nlayers
        strain_z_calc(2) = strain_z_calc(2) + c1(ii) * beth(ii)
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
                sigr(jj) = sigr(jj) + kk_p(ii) * strain_z_calc(2) * nu_z(ii) 
                sigt(jj) = sigt(jj) + kk_p(ii) * strain_z_calc(2) * nu_z(ii)
                sigz(jj) = sigz(jj) + kk_z(ii) * strain_z_calc(2) * (1.0D0 - nu_p(ii))
            else ! CS system
                sigr(jj) = sigr(jj) + kk_p(ii) * strain_z_calc(1) * nu_z(ii) 
                sigt(jj) = sigt(jj) + kk_p(ii) * strain_z_calc(1) * nu_z(ii)
                sigz(jj) = sigz(jj) + kk_z(ii) * strain_z_calc(1) * (1.0D0 - nu_p(ii))
            end if
                    
            ! Radial normal strain
            strain_r(jj) = c1(ii) - c2(ii) / rradius(jj)**2                      &
                           + 0.375D0*alpha(ii) * rradius(jj)**2 + 0.5D0*beta(ii) &
                           * (1.0D0 + log(rradius(jj)))
      
            ! Toroidal normal strain
            strain_t(jj) = c1(ii) + c2(ii) / rradius(jj)**2                       &
                            + 0.125D0*alpha(ii) * rradius(jj)**2 + 0.5D0*beta(ii) & 
                            * log(rradius(jj))
                    
            ! Vertical normal strain
            if ( ii >= i_tf_bucking ) then
                strain_z(jj) = strain_z_calc(2)
            else 
                strain_z(jj) = strain_z_calc(1)
            end if

            ! Radial displacement
            r_deflect(jj) = c1(ii)*rradius(jj) + c2(ii)/rradius(jj) &
                            + 0.125D0*alpha(ii) * rradius(jj)**3    &
                            + 0.5D0*beta(ii) * rradius(jj)*log(rradius(jj))

        end do ! layer array loop
    end do ! Layer loop
    ! ------     

end subroutine generalized_plane_strain     

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function eyngeff(estl,eins,tins,tstl,tcs)

    !! Finds the effective Young's modulus of the TF coil winding pack
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! author: J Galambos, FEDC/ORNL
    !! This routine calculates the effective Young's modulus (Pa)
    !! of the TF coil in the winding pack section.
    !! PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(dp) :: eyngeff

    !  Arguments
    ! ---
    real(dp), intent(in) :: estl
    !! Young's modulus of steel (Pa)

    real(dp), intent(in) :: eins
    !! Young's modulus of insulator (Pa)

    real(dp), intent(in) :: tins
    !! Insulator wrap thickness (m)

    real(dp), intent(in) :: tstl
    !! Thickness of steel conduit (m)

    real(dp), intent(in) :: tcs
    !! Dimension of cable space area inside conduit (m)

    !  Local variables
    ! ---
    real(dp) :: ed

    real(dp) :: ttot
    !!  Total turn thickness
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Total thickness of a turn
    ttot = tcs + 2.0D0*(tins + tstl)

    !  See Figure 8 and Section III.4, Morris
    ed = ttot / (2.0D0*tins/eins + (tcs+2.0D0*tstl)/estl)

    eyngeff = 1.0D0/ttot * 2.0D0*tstl*ed

end function eyngeff

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function edoeeff(estl,eins,tins,tstl,tcs)

    !! Returns ratio of E_d to E_eff in Morris
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! This routine calculates the ratio of E_d to the effective Young's
    !! modulus, given in Morris, Section III.4. This is used to calculate
    !! the strain in the insulator.
    !! PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use maths_library, only: eshellvol
    implicit none

    real(dp) :: edoeeff

    !  Arguments
    real(dp), intent(in) :: estl
    !! Young's modulus of steel (Pa)

    real(dp), intent(in) :: eins
    !! Young's modulus of insulator (Pa)
    
    real(dp), intent(in) :: tins
    !! Insulator wrap thickness (m)
    
    real(dp), intent(in) :: tstl
    !! Thickness of steel conduit (m)
    
    real(dp), intent(in) :: tcs
    !! Dimension of cable space area inside conduit (m)

    !  Local variables
    real(dp) :: ed,ttot,eeff

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Total thickness of a turn
    ttot = tcs + 2.0D0*(tins + tstl)

    !  Code copied from eyngeff routine
    !  See Figure 8 and Section III.4, Morris
    ed = ttot / (2.0D0*tins/eins + (tcs+2.0D0*tstl)/estl)
    eeff = 1.0D0/ttot * 2.0D0*tstl*ed
    edoeeff = ed/eeff

end function edoeeff

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function eyngzwp(estl,eins,ewp,tins,tstl,tcs)

    !! Finds the vertical Young's modulus of the TF coil winding pack
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: J Morris, CCFE, Culham Science Centre
    !! This routine calculates the vertical Young's modulus (Pa)
    !! of the TF coil in the winding pack section.
    !! PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(dp) :: eyngzwp

    !  Arguments

    real(dp), intent(in) :: estl
    !! Young's modulus of steel (Pa)

    real(dp), intent(in) :: eins
    !! Young's modulus of insulator (Pa)
    
    real(dp), intent(in) :: ewp
    !! Young's modulus of windings (Pa)
    
    real(dp), intent(in) :: tins
    !! insulator wrap thickness (m)

    real(dp), intent(in) :: tstl
    !! thickness of steel conduit (m)

    real(dp), intent(in) :: tcs
    !! dimension of cable space area inside conduit (m)
 
    !  Local variables
    real(dp) :: ttot

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ttot = tcs + 2.0D0*(tins + tstl)

    eyngzwp = ewp*tcs*tcs &
              + estl*( (tcs + 2.0D0*tstl)**2 - tcs*tcs ) &
              + eins*( (tcs + 2.0D0*(tstl + tins))**2 - (tcs + 2.0D0*tstl)**2 )

    eyngzwp = eyngzwp / (ttot*ttot)

end function eyngzwp

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function sig_tresca(sx,sy,sz)
    !! Calculates TRESCA stress in a TF coil
    !! author: S Kahn
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! OUTPUT
    real(dp) :: sig_tresca

    !  Arguments
    real(dp), intent(in) :: sx
    !! In-plane stress in X direction [Pa]

    real(dp), intent(in) :: sy
    !! In-plane stress in Y direction [Pa]
    
    real(dp), intent(in) :: sz
    !! In-plane stress in Z direction [Pa]
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    sig_tresca = max(ABS(sx-sy), ABS(sx-sz), ABS(sy-sz))

end function sig_tresca

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function sigvm(sx,sy,sz,txy,txz,tyz)

    !! Calculates Von Mises stress in a TF coil
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: B Reimer, FEDC
    !! This routine calculates the Von Mises combination of
    !! stresses (Pa) in a TF coil.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! OUTPUT
    real(dp) :: sigvm

    !  Arguments
    real(dp), intent(in) :: sx
    !! In-plane stress in X direction [Pa]

    real(dp), intent(in) :: sy
    !! In-plane stress in Y direction [Pa]
    
    real(dp), intent(in) :: sz
    !! In-plane stress in Z direction [Pa]
    
    real(dp), intent(in) :: txy
    !! Out of plane stress in X-Y plane [Pa]
    
    real(dp), intent(in) :: txz
    !! Out of plane stress in X-Z plane [Pa]
    
    real(dp), intent(in) :: tyz
    !! Out of plane stress in Y-Z plane [Pa]

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    sigvm = sqrt( 0.5D0 * ( (sx-sy)**2 + (sx-sz)**2 + (sz-sy)**2 &
                + 6.0D0*(txy**2 + txz**2 + tyz**2) ) )

end function sigvm

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine coilshap

    !! Calculates the TF coil shape
    !! Calculates the shape of the INSIDE of the TF coil. The coil is
    !! approximated by a straight inboard section and four elliptical arcs
    !! This is a totally ad hoc model, with no physics or engineering basis.
    use physics_variables, only: i_single_null, rminor, rmajor, itart
    use build_variables, only: hmax, hpfu, tfcth, r_tf_outboard_mid, &
        r_tf_inboard_mid, tfthko, r_cp_top, r_tf_inboard_out
    use tfcoil_variables, only: yarc, xarc, tfleng, tfa, tfb, i_tf_shape
    use constants, only: pi
    implicit none

    !  Arguments
    !  Local variables
    real(dp), parameter :: fstraight = 0.6D0
    real(dp) :: aa, bb
    integer :: ii
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    if ( i_tf_shape == 1 .and. itart == 0 ) then
    ! PROCESS D-shape parametrisation
        
        ! X position of the arcs, eq(15)
        ! The xarc/yarc are defined in the INSIDE part of the TF
        xarc(1) = r_tf_inboard_out
        xarc(2) = rmajor - 0.2D0*rminor
        xarc(3) = r_tf_outboard_in
        xarc(4) = xarc(2)
        xarc(5) = xarc(1)

        ! Height of straight section as a fraction of the coil inner height
        if ( i_single_null == 0 ) then
            ! Double null
            yarc(1) = fstraight * hmax
            yarc(2) = hmax
            yarc(3) = 0
            yarc(4) = -hmax
            yarc(5) = -fstraight * hmax
        else
            ! Single null
            yarc(1) = fstraight * (hpfu - tfcth)
            yarc(2) = hpfu - tfcth
            yarc(3) = 0
            yarc(4) = -hmax
            yarc(5) = -fstraight * hmax
        end if

        ! Horizontal and vertical radii of inside edge of TF coil
        ! Arcs are numbered clockwise:
        ! 1=upper inboard, 2=upper outboard, 3=lower ouboard, 4=lower inboard
        ! 'tfleng' is the length of the coil midline.
        tfleng = yarc(1) - yarc(5)
        do ii = 1, 4
            tfa(ii) = abs(xarc(ii+1) - xarc(ii))
            tfb(ii) = abs(yarc(ii+1) - yarc(ii))
            ! Radii and length of midline of coil segments
            aa = tfa(ii) + 0.5D0*tfcth
            bb = tfb(ii) + 0.5D0*tfcth
            tfleng = tfleng + 0.25d0 * circumference(aa,bb)
        end do

    ! Centrepost with D-shaped
    ! ---
    else if ( i_tf_shape == 1 .and. itart == 1  ) then
        
        ! X position of the arcs, eq(17) and text before it
        xarc(1) = r_cp_top
        xarc(2) = rmajor - 0.2D0*rminor
        xarc(3) = r_tf_outboard_in
        xarc(4) = xarc(2)
        xarc(5) = xarc(1)

        ! Double null, eq(17) and text before it
        yarc(1) = hpfu - tfcth
        yarc(2) = hpfu - tfcth
        yarc(3) = 0
        yarc(4) = -hmax
        yarc(5) = -hmax

        ! TF middle circumference, eq(18)
        tfleng = 2*(xarc(2) - xarc(1))

        do ii = 2, 3 
           tfa(ii) = abs(xarc(ii+1) - xarc(ii))
           tfb(ii) = abs(yarc(ii+1) - yarc(ii))
   
           ! Radii and length of midline of coil segments
           aa = tfa(ii) + 0.5D0 * tfthko
           bb = tfb(ii) + 0.5D0 * tfthko
           tfleng = tfleng + 0.25d0 * circumference(aa,bb)
        end do
    ! ---

    
    ! Picture frame coil
    ! ---
    else if ( i_tf_shape == 2 ) then
 
        ! X position of the arcs
        if ( itart == 0 ) xarc(1) = r_tf_inboard_out  
        if ( itart == 1 ) xarc(1) = r_cp_top        
        xarc(2) = r_tf_outboard_in 
        xarc(3) = xarc(2)
        xarc(4) = xarc(2)
        xarc(5) = xarc(1)

        ! Y position of the arcs
        yarc(1) = hpfu - tfcth
        yarc(2) = hpfu - tfcth
        yarc(3) = 0
        yarc(4) = -hmax
        yarc(5) = -hmax

        ! TF middle circumference
        ! IMPORTANT : THE CENTREPOST LENGTH IS NOT INCLUDED FOR TART 
        if ( itart == 0 ) tfleng = 2.0D0 * ( 2.0D0*hmax + tfcth  + r_tf_outboard_mid - r_tf_inboard_mid )    ! eq(19)
        if ( itart == 1 ) tfleng = hmax + hpfu + 2.0D0 * ( r_tf_outboard_mid - r_cp_top ) ! eq(20)
    end if
    ! ---

    contains
    function circumference(aaa,bbb)
        !! Calculate ellipse arc circumference using Ramanujan approximation (m)
        !!  See https://www.johndcook.com/blog/2013/05/05/ramanujan-circumference-ellipse/
        !!  for a discussion of the precision of the formula 

        real(dp) :: circumference
        real(dp), intent(in) :: aaa, bbb
        real(dp) :: hh
        hh = ( aaa - bbb )**2 / ( aaa + bbb )**2
        circumference = pi* ( aaa + bbb ) * ( 1.0D0 + (3.0D0*hh)/(10.0D0 + sqrt(4.0D0 - 3.0D0*hh)) )

    end function

end subroutine coilshap

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfcind(tfthk)

    !! Calculates the self inductance of a TF coil
    !! tfthk        : input real : TF coil thickness (m)
    !! This routine calculates the self inductance of a TF coil
    !! approximated by a straight inboard section and two elliptical arcs.
    !! The inductance of the TFC (considered as a single axisymmetric turn)
    !! is calculated by numerical integration over the cross-sectional area.
    !! The contribution from the cross-sectional area of the
    !! coil itself is calculated by taking the field as B(r)/2.
    !! The field in the bore is calculated for unit current.
    !! Top/bottom symmetry is assumed.
    use tfcoil_variables, only: yarc, xarc, tfind
    use constants, only: pi, rmu0
    implicit none
    !  Arguments
    real(dp), intent(in) :: tfthk

    !  Local variables
    integer, parameter :: nintervals = 100
    integer :: i
    real(dp) :: ai, ao, bi, bo, x0, y0, h_bore, h_thick, dr, r, b

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Initialise inductance
    tfind = 0.0D0
    ! Integrate over the whole TF area, including the coil thickness.
    x0 = xarc(2)
    y0 = yarc(2)

    ! Minor and major radii of the inside and outside perimeters of the the
    ! Inboard leg and arc.
    ! Average the upper and lower halves, which are different in the
    ! single null case
    ai = xarc(2) - xarc(1)
    bi = (yarc(2)-yarc(4))/2.0d0 - yarc(1)
    ao = ai + tfthk
    bo = bi + tfthk
    ! Interval used for integration
    dr = ao / dble(nintervals)
    ! Start both integrals from the centre-point where the arcs join.
    ! Initialise major radius
    r = x0 - dr/2.0d0
    do i = 1,nintervals
        ! Field in the bore for unit current
        b = rmu0/(2.0D0*pi*r)
        ! Find out if there is a bore
        if (x0-r < ai) then
            h_bore = y0 + bi * sqrt(1 - ((r-x0)/ai)**2)
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) - h_bore
        else
            h_bore = 0.0d0
            ! Include the contribution from the straight section
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) + yarc(1)
        end if
        ! Assume B in TF coil = 1/2  B in bore
        ! Multiply by 2 for upper and lower halves of coil
        tfind = tfind + b*dr*(2.0D0*h_bore + h_thick)
        r = r - dr
    end do

    ! Outboard arc
    ai = xarc(3) - xarc(2)
    bi = (yarc(2) - yarc(4))/2.0d0
    ao = ai + tfthk
    bo = bi + tfthk
    dr = ao / dble(nintervals)
    ! Initialise major radius
    r = x0 + dr/2.0d0
    do i = 1,nintervals
        ! Field in the bore for unit current
        b = rmu0/(2.0D0*pi*r)
        ! Find out if there is a bore
        if (r-x0 < ai) then
            h_bore = y0 + bi * sqrt(1 - ((r-x0)/ai)**2)
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2) - h_bore
        else
            h_bore = 0.0d0
            h_thick = bo * sqrt(1 - ((r-x0)/ao)**2)
        end if
        ! Assume B in TF coil = 1/2  B in bore
        ! Multiply by 2 for upper and lower halves of coil
        tfind = tfind + b*dr*(2.0D0*h_bore + h_thick)
        r=r+dr
    end do

end subroutine tfcind

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine outtf(outfile, peaktfflag)

    !! Writes superconducting TF coil output to file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! peaktfflag : input integer : warning flag from peak TF calculation
    !! This routine writes the superconducting TF coil results
    !! to the output file.
    !! PROCESS Superconducting TF Coil Model, J. Morris, CCFE, 1st May 2014
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use rebco_variables, only: solder_area, copperA_m2, coppera_m2_max, &
        copper_rrr
    use error_handling, only: report_error
    use build_variables, only: hmax, r_tf_inboard_mid, r_tf_outboard_mid, &
        tfcth, tfthko, r_cp_top, r_tf_inboard_in, r_tf_inboard_out, f_r_cp
    use process_output, only: int2char, ovarre, ocmmnt, oblnkl, ovarin, osubhd, &
        ovarrf, obuild
    use numerics, only: icc
    use tfcoil_variables, only: wwp1, whttf, yarc, xarc, &
        windstrain, wwp2, whtconsh, tftort, whtconcu, ritfc, &
        tfinsgap, deflect, vtfskv, tmaxpro, fcutfsu, t_conductor, &
        tinstf, n_tf_turn, cforce, i_tf_turns_integer, tdmptf, &
        oacdcp, estotftgj, n_tf, whtconin, jwptf, tfa, &
        tficrn, n_layer, tfleng, thwcndut, casthi, sigvvall, &
        thkcas, casths, vforce, n_pancake, aswp, aiwp, tfareain, acasetf, &
        vftf, eyzwp, thicndut, dhecoil, insstrain, taucq, ripmax, &
        whtconsc, alstrtf, bmaxtfrp, vdalw, dr_tf_wp, whtcas, whtcon, &
        ripple, i_tf_tresca, bmaxtf, awphec, avwp, aiwp, acond, acndttf, &
        i_tf_sc_mat, voltfleg, vol_cond_cp, tflegres, tcpav, prescp, i_tf_sup, &
        cpttf, cdtfleg, whttflgs, whtcp, i_tf_bucking, tlegav, rhotfleg, rhocp, &
        presleg, i_tf_shape, fcoolcp, pres_joints, tmargtf, tmargmin_tf, &
        f_vforce_inboard, vforce_outboard, acstf, t_turn_tf
    use physics_variables, only: itart
    use constants, only: mfile, pi
    implicit none

    !  Arguments

    integer, intent(in) :: outfile, peaktfflag

    !  Local variables

    integer :: ii
    real(dp) :: ap, radius
    character(len=1) :: intstring

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     
    ! General coil parameters
    call osubhd(outfile,'TF design')
    call ovarin(outfile,'Conductor technology','(i_tf_sup)',i_tf_sup)
    select case (i_tf_sup)
        case (0)
            call ocmmnt(outfile,'  -> Resitive coil : Water cooled copper (GLIDCOP AL-15)')
        case (1)
            call ocmmnt(outfile,'  -> Superconducting coil (SC)')
        case (2)
            call ocmmnt(outfile,'  -> Reisitive coil : Helium cooled aluminium')
    end select

    ! SC material scaling 
    if ( i_tf_sup == 1 ) then
        call ovarin(outfile,'Superconductor material','(i_tf_sc_mat)',i_tf_sc_mat)
        select case (i_tf_sc_mat)
            case (1)
                call ocmmnt(outfile,'  -> ITER Nb3Sn critical surface model')
            case (2)
                call ocmmnt(outfile,'  -> Bi-2212 high temperature superconductor')
            case (3)
                call ocmmnt(outfile,'  -> NbTi')
            case (4)
                call ocmmnt(outfile, &
                '  -> ITER Nb3Sn critical surface model, user-defined parameters')
            case (5)
                call ocmmnt(outfile, '  -> WST Nb3Sn')
            case (6)
                call ocmmnt(outfile, &
                    '  -> High temperature superconductor: REBCO HTS tape in CroCo strand')
            case (7)
                call ocmmnt(outfile, & 
                    '  ->  Durham Ginzburg-Landau critical surface model for Nb-Ti')
            case (8)
                call ocmmnt(outfile, & 
                    '  ->  Durham Ginzburg-Landau critical surface model for REBCO')
        end select
    end if

    ! Joints strategy
    call ovarin(outfile,'Presence of TF demountable joints','(itart)',itart)
    if ( itart == 1 ) then
        call ocmmnt(outfile,'  -> TF coil made of a Centerpost (CP) and outer legs')
        call ocmmnt(outfile,'     interfaced with demountable joints')
    else 
        call ocmmnt(outfile,'  -> Coils without demountable joints')
    end if

    ! Centring forces support strategy
    call ovarin(outfile,'TF inboard leg support strategy','(i_tf_bucking)', i_tf_bucking)
    select case ( i_tf_bucking )
        case (0)
            call ocmmnt(outfile,'  -> No support structure')
        case (1)
            if ( i_tf_sup == 0 ) then
                call ocmmnt(outfile,'  -> Steel bucking cylinder')
            else if ( i_tf_sup == 1 ) then 
                call ocmmnt(outfile,'  -> Steel casing')
            else 
                call ocmmnt(outfile,'  -> Nibron special bucking cylinder')
            end if
        case (2,3)
            call ocmmnt(outfile,'  -> TF in contact with CS (bucked and weged design)')
    end select

    ! TF coil geometry
    call osubhd(outfile,'TF coil Geometry :')
    call ovarin(outfile,'Number of TF coils','(n_tf)', int(n_tf))
    call ovarre(outfile,'Inboard leg centre radius (m)','(r_tf_inboard_mid)',r_tf_inboard_mid, 'OP ')
    call ovarre(outfile,'Outboard leg centre radius (m)','(r_tf_outboard_mid)',r_tf_outboard_mid, 'OP ')
    call ovarre(outfile,'Total inboard leg radial thickness (m)','(tfcth)',tfcth)
    call ovarre(outfile,'Total outboard leg radial thickness (m)','(tfthko)',tfthko)
    call ovarre(outfile,'Outboard leg toroidal thickness (m)','(tftort)',tftort, 'OP ')
    call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax, 'OP ')
    call ovarre(outfile,'Mean coil circumference (m)','(tfleng)',tfleng, 'OP ')
    
    ! Vertical shape
    call ovarin(outfile,'Vertical TF shape','(i_tf_shape)',i_tf_shape)
    if ( i_tf_shape == 1 ) then
        call oblnkl(outfile)
        call ocmmnt(outfile,'D-shape coil, inner surface shape approximated by')
        call ocmmnt(outfile,'by a straight segment and elliptical arcs between the following points:')
        call oblnkl(outfile)
    else if ( i_tf_shape == 2 ) then 
        call oblnkl(outfile)
        call ocmmnt(outfile,'Picture frame coil, inner surface approximated by')
        call ocmmnt(outfile,'by a straight segment between the following points:')
        call oblnkl(outfile)
    end if

    write(outfile,10)
    10  format(t2,'point',t16,'x(m)',t31,'y(m)')
    do ii = 1,5
        write(outfile,20) ii,xarc(ii),yarc(ii)
        intstring = int2char(ii)
        call ovarre(mfile,'TF coil arc point '//intstring//' R (m)', '(xarc('//intstring//'))',xarc(ii))
        call ovarre(mfile,'TF coil arc point '//intstring//' Z (m)', '(yarc('//intstring//'))',yarc(ii))
    end do
    20 format(i4,t10,f10.3,t25,f10.3)      

    ! CP tapering geometry
    if ( itart == 1 .and. i_tf_sup /= 1 ) then
        call osubhd(outfile,'Tapered Centrepost Dimensions:')
        call ovarre(outfile,'Radius of the centrepost at the midplane (m)','(r_tf_inboard_out)',r_tf_inboard_out)
        call ovarre(outfile,'Radius of the ends of the centrepost (m)','(r_cp_top)',r_cp_top)
        call ovarre(outfile,'Top/miplane TF CP radius ratio (-)','(f_r_cp)', f_r_cp)
        call ovarre(outfile,'Distance from the midplane to the top of the tapered section (m)','(h_cp_top)',h_cp_top)
        call ovarre(outfile,'Distance from the midplane to the top of the centrepost (m)','(hmax + tfthko)',hmax + tfthko)
    end if

    ! Turn/WP gemoetry
    if ( i_tf_sup == 1 ) then

        ! Total material fraction
        call osubhd(outfile,'Global material area/fractions:')
        call ovarre(outfile,'TF cross-section (total) (m2)','(tfareain)', tfareain)
        call ovarre(outfile,'Total steel cross-section (m2)','(a_tf_steel*n_tf)',a_tf_steel*n_tf)
        call ovarre(outfile,'Total steel TF fraction','(f_tf_steel)',f_tf_steel)
        call ovarre(outfile,'Total Insulation cross-section (total) (m2)','(a_tf_ins*n_tf)',a_tf_steel*n_tf)
        call ovarre(outfile,'Total Insulation fraction','(f_tf_ins)',f_tf_ins)
        
        ! External casing
        call osubhd(outfile,'External steel Case Information :')
        call ovarre(outfile,'Casing cross section area (per leg) (m2)','(acasetf)',acasetf)
        call ovarre(outfile,'Inboard leg case plasma side wall thickness (m)','(casthi)',casthi)
        call ovarre(outfile,'Inboard leg case inboard "nose" thickness (m)','(thkcas)',thkcas)
        call ovarre(outfile,'Inboard leg case sidewall thickness at its narrowest point (m)','(casths)',casths)
        call ovarre(outfile,'External case mass per coil (kg)','(whtcas)',whtcas, 'OP ')

        ! Winding pack structure
        call osubhd(outfile,'TF winding pack (WP) geometry:')
        call ovarre(outfile,'WP cross section area with insulation and insertion (per coil) (m2)','(awpc))',awpc)
        call ovarre(outfile,'WP cross section area (per coil) (m2)','(aswp)', awptf)
        call ovarre(outfile,'Winding pack radial thickness (m)','(dr_tf_wp)',dr_tf_wp, 'OP ')
        if (  i_tf_turns_integer == 1 ) then
            call ovarre(outfile, 'Winding pack toroidal width (m)', '(wwp1)', wwp1, 'OP ')
        else
            call ovarre(outfile,'Winding pack toroidal width 1 (m)','(wwp1)',wwp1, 'OP ')
            call ovarre(outfile,'Winding pack toroidal width 2 (m)','(wwp2)',wwp2, 'OP ')
        end if
        call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)',tinstf)
        call ovarre(outfile,'Winding pack insertion gap (m)','(tfinsgap)',tfinsgap)
       
        ! WP material fraction        
        call osubhd(outfile,'TF winding pack (WP) material area/fractions:')
        call ovarre(outfile,'Steel WP cross-section (total) (m2)','(aswp*n_tf)',aswp*n_tf)
        call ovarre(outfile,'Steel WP fraction','(aswp/awpc)',aswp/awpc)
        call ovarre(outfile,'Insulation WP fraction','(aiwp/awpc)',aiwp/awpc)
        call ovarre(outfile,'Cable WP fraction','((awpc-aswp-aiwp)/awpc)',(awpc-aswp-aiwp)/awpc)

        ! Number of turns
        call osubhd(outfile,'WP turn information:')    
        call ovarin(outfile,'Turn parametrisation', '(i_tf_turns_integer)', i_tf_turns_integer)
        if ( i_tf_turns_integer == 0 ) then 
            call ocmmnt(outfile,'  Non-integer number of turns')
        else 
            call ocmmnt(outfile,'  Integer number of turns')
        end if
        call ovarre(outfile,'Number of turns per TF coil','(n_tf_turn)',n_tf_turn, 'OP ')
        if ( i_tf_turns_integer == 1 ) then
            call ovarin(outfile, 'Number of TF pancakes', '(n_pancake)', n_pancake)
            call ovarin(outfile, 'Number of TF layers', '(n_layer)', n_layer)
        end if    
        call oblnkl(outfile)

        if ( i_tf_turns_integer == 1 ) then
            call ovarre(outfile, 'Radial width of turn (m)', '(t_turn_radial)', t_turn_radial)
            call ovarre(outfile, 'Toroidal width of turn (m)', '(t_turn_toroidal)', t_turn_toroidal)
            call ovarre(outfile, 'Radial width of conductor (m)', '(elonductor_radial)', t_conductor_radial, 'OP ')
            call ovarre(outfile, 'Toroidal width of conductor (m)', '(t_conductor_toroidal)', t_conductor_toroidal, 'OP ')
            call ovarre(outfile, 'Radial width of cable space', '(t_cable_radial)', t_cable_radial)
            call ovarre(outfile, 'Toroidal width of cable space', '(t_cable_toroidal)', t_cable_toroidal)
       else
            call ovarre(outfile,'Width of turn including inter-turn insulation (m)','(t_turn_tf)',t_turn_tf, 'OP ')
            call ovarre(outfile,'Width of conductor (square) (m)','(t_conductor)',t_conductor, 'OP ')
            call ovarre(outfile,'Width of space inside conductor (m)','(t_cable)',t_cable, 'OP ')
        end if
        call ovarre(outfile,'Steel conduit thickness (m)','(thwcndut)',thwcndut)
        call ovarre(outfile,'Inter-turn insulation thickness (m)','(thicndut)',thicndut)

        select case (i_tf_sc_mat)
        case (1,2,3,4,5,7,8)
            call osubhd(outfile,'Conductor information:')
            call ovarre(outfile,'Diameter of central helium channel in cable','(dhecoil)',dhecoil)
            call ocmmnt(outfile,'Fractions by area')
            call ovarre(outfile, 'internal area of the cable space', '(acstf)', acstf)
            call ovarre(outfile,'Coolant fraction in conductor excluding central channel','(vftf)',vftf)
            call ovarre(outfile,'Copper fraction of conductor','(fcutfsu)',fcutfsu)
            call ovarre(outfile,'Superconductor fraction of conductor','(1-fcutfsu)',1-fcutfsu)
            ! TODO
            !call ovarre(outfile,'Conductor fraction of winding pack','(acond/ap)',acond/ap, 'OP ')
            !call ovarre(outfile,'Conduit fraction of winding pack','(n_tf_turn*acndttf/ap)',n_tf_turn*acndttf/ap, 'OP ')
            !call ovarre(outfile,'Insulator fraction of winding pack','(aiwp/ap)',aiwp/ap, 'OP ')
            !call ovarre(outfile,'Helium area fraction of winding pack excluding central channel','(avwp/ap)',avwp/ap, 'OP ')
            !call ovarre(outfile,'Central helium channel area as fraction of winding pack','(awphec/ap)',awphec/ap, 'OP ')
            ap = acond + n_tf_turn*acndttf + aiwp + avwp + awphec
            call ovarrf(outfile,'Check total area fractions in winding pack = 1','', &
            (acond + n_tf_turn*acndttf + aiwp + avwp + awphec)/ap)
            call ovarrf(outfile,'minimum TF conductor temperature margin  (K)','(tmargmin_tf)',tmargmin_tf)
            call ovarrf(outfile,'TF conductor temperature margin (K)','(tmargtf)',tmargtf)

        end select
    else

        ! External casing
        call osubhd(outfile,'Bucking cylinder information:')
        call ovarre(outfile,'Casing cross section area (per leg) (m2)','(acasetf)',acasetf)
        call ovarre(outfile,'Inboard leg case plasma side wall thickness (m)','(casthi)',casthi)
        call ovarre(outfile,'Inboard leg bucking cylinder thickness (m)','(thkcas)',thkcas)

        ! Conductor layer geometry
        call osubhd(outfile,'Inboard TFC conductor sector geometry:')
        call ovarre(outfile,'Inboard TFC conductor sector area with gr insulation (per leg) (m2)' &
            ,'(awpc))',awpc)
        call ovarre(outfile,'Inboard TFC conductor sector area (per leg) (m2)','(aswp)',awptf )
        call ovarre(outfile,'Inboard conductor sector radial thickness (m)','(dr_tf_wp)',dr_tf_wp )
        call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)', tinstf )
       
        ! Turn info
        call osubhd(outfile,'Coil turn information:')
        call ovarre(outfile,'Number of turns per TF leg','(n_tf_turn)',n_tf_turn)
        call ovarre(outfile,'Turn insulation thickness','(tinstf)',tinstf)
        call ovarre(outfile,'Mid-plane CP cooling fraction','(fcoolcp)',fcoolcp)
        call ovarre(outfile,'Outboard leg current per turn (A)','(cpttf)',cpttf)
        call ovarre(outfile,'Inboard leg conductor volume (m3)','(vol_cond_cp)',vol_cond_cp)
        call ovarre(outfile,'Outboard leg volume per coil (m3)','(voltfleg)',voltfleg)
    end if 

    ! Coil masses
    call osubhd(outfile,'TF coil mass:')    
    call ovarre(outfile,'Superconductor mass per coil (kg)','(whtconsc)',whtconsc, 'OP ')
    call ovarre(outfile,'Copper mass per coil (kg)','(whtconcu)',whtconcu, 'OP ')
    call ovarre(outfile,'Steel conduit mass per coil (kg)','(whtconsh)',whtconsh, 'OP ')
    call ovarre(outfile,'Conduit insulation mass per coil (kg)','(whtconin)',whtconin, 'OP ')
    call ovarre(outfile,'Total conductor mass per coil (kg)','(whtcon)',whtcon, 'OP ')
    if ( itart ==  1 ) then
        call ovarre(outfile,'Mass of inboard legs (kg)','(whtcp)',whtcp)
        call ovarre(outfile,'Mass of outboard legs (kg)','(whttflgs)',whttflgs)
    end if 
    call ovarre(outfile,'Mass of each TF coil (kg)','(whttf/n_tf)',whttf/n_tf, 'OP ')
    call ovarre(outfile,'Total TF coil mass (kg)','(whttf)',whttf)

    ! TF current and field
    call osubhd(outfile,'Maximum B field and currents:')
    call ovarre(outfile,'Nominal peak field assuming toroidal symmetry (T)','(bmaxtf)',bmaxtf, 'OP ')
    call ovarre(outfile,'Total current in all TF coils (MA)','(ritfc/1.D6)',1.0D-6*ritfc, 'OP ')
    call ovarre(outfile,'TF coil current (summed over all coils) (A)','(ritfc)',ritfc)
    if ( i_tf_sup == 1 ) then
        call ovarre(outfile,'Actual peak field at discrete conductor (T)','(bmaxtfrp)',bmaxtfrp, 'OP ')
        call ovarre(outfile,'Winding pack current density (A/m2)','(jwptf)',jwptf, 'OP ')
    end if
    call ovarre(outfile,'Inboard leg mid-plane conductor current density (A/m2)','(oacdcp)',oacdcp)
    if ( itart == 1 ) then
        call ovarre(outfile,'Outboard leg conductor current density (A/m2)','(cdtfleg)',cdtfleg)    
    end if 
    call ovarre(outfile,'Total stored energy in TF coils (GJ)','(estotftgj)',estotftgj, 'OP ')


    ! TF forces
    call osubhd(outfile,'TF Forces:')
    call ovarre(outfile,'Inboard vertical tension per coil (N)','(vforce)',vforce, 'OP ')
    call ovarre(outfile,'Outboard vertical tension per coil (N)','(vforce_outboard)', vforce_outboard, 'OP ')
    call ovarre(outfile,'inboard vertical tension fraction (-)','(f_vforce_inboard)', f_vforce_inboard, 'OP ')
    call ovarre(outfile,'Centring force per coil (N/m)','(cforce)',cforce, 'OP ')

    ! Resistive coil parameters
    if ( i_tf_sup /= 1 ) then 
        call osubhd(outfile,'Resitive loss parameters:')
        if ( i_tf_sup == 0 ) then
            call ocmmnt(outfile,'Resistive Material : GLIDCOP AL-15 - Dispersion Strengthened Copper') 
        else if ( i_tf_sup == 2 ) then
            call ocmmnt(outfile,'Resistive Material : Pure Aluminium (99.999+ %)')
        end if

        if ( itart == 1 ) then
            call ovarre(outfile,'CP resistivity (ohm.m)','(rhocp)',rhocp)
            call ovarre(outfile,'Leg resistivity (ohm.m)','(rhotfleg)',rhotfleg)
            call ovarre(outfile,'CP resistive power loss (W)','(prescp)',prescp)
            call ovarre(outfile,'Leg resitive power loss, (per leg) (W)','(presleg)',presleg)
            call ovarre(outfile,'joints resistive power loss (W)','(pres_joints)',pres_joints)
            call ovarre(outfile,'Outboard leg resistance per coil (ohm)','(tflegres)',tflegres)
            call ovarre(outfile,'Average CP temperature (K)','(tcpav)',tcpav)
            call ovarre(outfile,'Average leg temperature (K)','(tlegav)',tlegav)
            
        else    
            call ovarre(outfile,'TF resistivity (ohm.m)','(prescp)',rhocp)
            call ovarre(outfile,'TF coil resistive power less (total) (ohm.m)','(prescp)',prescp)        
            call ovarre(outfile,'Average coil temperature (K)','(tcpav)',tcpav)
        end if 
    end if

    ! Ripple calculations
    call osubhd(outfile,'Ripple information:') 
    if ( i_tf_shape == 1 ) then

        if (peaktfflag == 1) then
            call report_error(144)
        else if (peaktfflag == 2) then
            call report_error(145)
        else
            continue
        end if
 
        call ovarre(outfile,'Max allowed ripple amplitude at plasma outboard midplane (%)','(ripmax)',ripmax)
        call ovarre(outfile,'Ripple amplitude at plasma outboard midplane (%)','(ripple)',ripple, 'OP ')
    else 
        call ovarre(outfile,'Max allowed ripple amplitude at plasma (%)','(ripmax)',ripmax)
        call ovarre(outfile,'Ripple at plasma edge (%)','(ripple)',ripple)
        call ocmmnt(outfile,'  Ripple calculation to be re-defined for picure frame coils') 
    end if 

    ! Quench information
    if ( i_tf_sup == 1 ) then
        call osubhd(outfile,'Quench information :')
        call ovarre(outfile,'Allowable stress in vacuum vessel (VV) due to quench (Pa)','(sigvvall)',sigvvall)
        call ovarre(outfile,'Minimum allowed quench time due to stress in VV (s)','(taucq)',taucq, 'OP ')
        call ovarre(outfile,'Actual quench time (or time constant) (s)','(tdmptf)',tdmptf)
        call ovarre(outfile,'Maximum allowed voltage during quench due to insulation (kV)', '(vdalw)', vdalw)
        call ovarre(outfile,'Actual quench voltage (kV)','(vtfskv)',vtfskv, 'OP ')

        select case (i_tf_sc_mat)
        case (1,2,3,4,5)
            call ovarre(outfile,'Maximum allowed temp rise during a quench (K)','(tmaxpro)', tmaxpro)
        case(6)
            call ocmmnt(outfile,'CroCo cable with jacket: ')

            if (any(icc == 75) ) then
                call ovarre(outfile,'Maximum permitted TF coil current / copper area (A/m2)', &
                '(copperA_m2_max)', copperA_m2_max)
            endif
            call ovarre(outfile,'Actual TF coil current / copper area (A/m2)', &
                                '(copperA_m2)', copperA_m2)

        end select        
    end if 

    ! TF coil radial build
    call osubhd(outfile,'Radial build of TF coil centre-line :')
    write(outfile,5)
    5   format(t43,'Thickness (m)',t60,'Outer radius (m)')

    radius = r_tf_inboard_in
    call obuild(outfile,'Innermost edge of TF coil',radius,radius)

    radius = radius + thkcas
    call obuild(outfile,'Coil case ("nose")',thkcas,radius,'(thkcas)')
        
    if ( i_tf_sup == 1 ) then
        radius = radius + tfinsgap
        call obuild(outfile,'Insertion gap for winding pack',tfinsgap,radius,'(tfinsgap)')
    end if

    radius = radius + tinstf
    if ( i_tf_sup == 1 ) then
        call obuild(outfile,'Winding pack ground insulation',tinstf,radius,'(tinstf)')
    else
        call obuild(outfile,'Conductor ground insulation',tinstf,radius,'(tinstf)')
    end if

    if ( i_tf_sup == 1 ) then
        radius = radius + 0.5D0*dr_tf_wp - tinstf - tfinsgap
        call obuild(outfile,'Winding - first half', dr_tf_wp/2d0-tinstf-tfinsgap, & 
            radius, '(dr_tf_wp/2-tinstf)')
    else
        radius = radius + 0.5D0*dr_tf_wp - tinstf
        call obuild(outfile,'Conductor - first half', dr_tf_wp/2d0 - tinstf, radius, &
            '(dr_tf_wp/2-tinstf)')
    end if
        
    if ( i_tf_sup == 1 ) then
        radius = radius + 0.5D0*dr_tf_wp - tinstf - tfinsgap
        call obuild(outfile,'Winding - second half',dr_tf_wp/2d0-tinstf-tfinsgap, &
            radius,'(dr_tf_wp/2-tinstf)')
    else
        radius = radius + 0.5D0*dr_tf_wp - tinstf
        call obuild(outfile,'Conductor - second half',dr_tf_wp/2d0-tinstf, &
            radius,'(dr_tf_wp/2-tinstf)')
    end if
        
    radius = radius + tinstf
    if ( i_tf_sup == 1 ) then
        call obuild(outfile,'Winding pack insulation',tinstf,radius,'(tinstf)')
    else 
        call obuild(outfile,'Conductor ground insulation',tinstf,radius,'(tinstf)')
    end if
        
    if ( i_tf_sup == 1 ) then
        radius = radius + tfinsgap
        call obuild(outfile,'Insertion gap for winding pack',tfinsgap,radius,'(tfinsgap)')
    end if

    radius = radius + casthi
    call obuild(outfile,'Plasma side case min radius',casthi,radius,'(casthi)')

    if ( i_tf_sup == 1 ) then
        radius = radius / cos(pi/n_tf)
        call obuild(outfile,'Plasma side case max radius', r_tf_inboard_out, radius,'(r_tf_inboard_out)')
    end if

    ! Radial build consistency check
    if ( abs( radius - r_tf_inboard_in - tfcth ) < 10.0D0 * epsilon(radius) ) then
        call ocmmnt(outfile,'TF coil dimensions are consistent')
    else
        call ocmmnt(outfile,'ERROR: TF coil dimensions are NOT consistent:')
        call ovarre(outfile,'Radius of plasma-facing side of inner leg SHOULD BE [m]','',r_tf_inboard_in + tfcth)
        call ovarre(outfile,'Inboard TF coil radial thickness [m]','(tfcth)',tfcth)
        call oblnkl(outfile)
    end if

end subroutine outtf

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tfspcall(outfile,iprint)

    !! Routine to call the superconductor module for the TF coils
    !! outfile : input integer : Fortran output unit identifier
    !! iprint : input integer : Switch to write output to file (1=yes)
    use rebco_variables, only: copper_area, copper_thick, copperA_m2, &
        croco_id, croco_od, croco_od, croco_thick, hastelloy_area, &
        hastelloy_thickness, rebco_area, stack_thickness, tape_thickness, &
        tape_thickness, tape_width, tapes, rebco_thickness, solder_area
    use error_handling, only: idiags, fdiags, report_error
    use process_output, only: ovarre, ocmmnt, oheadr, oblnkl, ovarin
    use tfcoil_variables, only: tmargmin_tf, n_tf_turn, n_tf, vftf, &
        temp_margin, jwdgpro, tftmp, vtfskv, acndttf, dhecoil, tmaxpro, &
        tmargtf, thwcndut, t_conductor, fcutfsu, jwdgcrt, tdmptf, cpttf, &
        ritfc, jwptf, bmaxtfrp, tcritsc, acstf, strncon_tf, fhts, bcritsc, &
        i_tf_sc_mat, b_crit_upper_nbti, t_crit_nbti
    use superconductors, only: wstsc, current_sharing_rebco, itersc, jcrit_rebco, jcrit_nbti, croco, bi2212,&
    GL_nbti, GL_REBCO
    use global_variables, only: run_tests
    use constants, only: pi
    implicit none
    integer, intent(in) :: outfile, iprint

    !  Local variables
    real(dp) :: aturn, tfes, vdump

    ! Simple model REMOVED Issue #781
    ! if (tfc_model == 0) then
    !     vtfskv = 20.0D0
    !     return
    ! end if

    ! Stored energy (J) per coil (NOT a physical meaningful quantity)
    tfes = estotft / n_tf
    ! Cross-sectional area per turn
    aturn = ritfc/(jwptf*n_tf*n_tf_turn)    

    if(i_tf_sc_mat==6)then
        call supercon_croco(aturn,bmaxtfrp,cpttf,tftmp, &
        iprint, outfile,  &
        jwdgcrt,tmargtf)

        vtfskv = croco_voltage()/1.0D3  !  TFC Quench voltage in kV
        
    else
        call supercon(acstf,aturn,bmaxtfrp,vftf,fcutfsu,cpttf,jwptf,i_tf_sc_mat, &
        fhts,strncon_tf,tdmptf,tfes,tftmp,tmaxpro,bcritsc,tcritsc,iprint, &
        outfile,jwdgcrt,vdump,tmargtf)
        
        vtfskv = vdump/1.0D3            !  TFC Quench voltage in kV
    end if    

contains    

    subroutine supercon(acs,aturn,bmax,fhe,fcu,iop,jwp,isumat,fhts, &
        strain,tdmptf,tfes,thelium,tmax,bcritsc,tcritsc,iprint,outfile, &
        jwdgcrt,vd,tmarg)

        !! Routine to calculate the TF superconducting conductor  properties
        !! author: P J Knight, CCFE, Culham Science Centre
        !! author: J Galambos, ORNL
        !! author: R Kemp, CCFE, Culham Science Centre
        !! author: M Kovari, CCFE, Culham Science Centre
        !! author: J Miller, ORNL
        !! acs : input real : Cable space - inside area (m2)
        !! aturn : input real : Area per turn (i.e. entire jacketed conductor) (m2)
        !! bmax : input real : Peak field at conductor (T)
        !! fhe : input real : Fraction of cable space that is for He cooling
        !! fcu : input real : Fraction of conductor that is copper
        !! iop : input real : Operating current per turn (A)
        !! jwp : input real : Actual winding pack current density (A/m2)
        !! isumat : input integer : Switch for conductor type:
        !! 1 = ITER Nb3Sn, standard parameters,
        !! 2 = Bi-2212 High Temperature Superconductor,
        !! 3 = NbTi,
        !! 4 = ITER Nb3Sn, user-defined parameters
        !! 5 = WST Nb3Sn parameterisation
        !! 7 = Durham Ginzburg-Landau Nb-Ti parameterisation
        !! fhts    : input real : Adjustment factor (<= 1) to account for strain,
        !! radiation damage, fatigue or AC losses
        !! strain : input real : Strain on superconductor at operation conditions
        !! tdmptf : input real : Dump time (sec)
        !! tfes : input real : Energy stored in one TF coil (J)
        !! thelium : input real : He temperature at peak field point (K)
        !! tmax : input real : Max conductor temperature during quench (K)
        !! bcritsc : input real : Critical field at zero temperature and strain (T) (isumat=4 only)
        !! tcritsc : input real : Critical temperature at zero field and strain (K) (isumat=4 only)
        !! iprint : input integer : Switch for printing (1 = yes, 0 = no)
        !! outfile : input integer : Fortran output unit identifier
        !! jwdgpro : output real : Winding pack current density from temperature
        !! rise protection (A/m2)
        !! jwdgcrt : output real : Critical winding pack current density (A/m2)
        !! vd : output real : Discharge voltage imposed on a TF coil (V)
        !! tmarg : output real : Temperature margin (K)
        !! This routine calculates the superconductor properties for the TF coils.
        !! It was originally programmed by J. Galambos 1991, from algorithms provided
        !! by J. Miller.
        !! <P>The routine calculates the critical current density (winding pack)
        !! and also the protection information (for a quench).
        !! NOT used for the Croco conductor
        implicit none

        integer, intent(in) :: isumat, iprint, outfile
        real(dp), intent(in) :: acs, aturn, bmax, fcu, fhe, fhts
        real(dp), intent(in) :: iop, jwp, strain, tdmptf, tfes, thelium, tmax, bcritsc, tcritsc
        real(dp), intent(out) :: jwdgcrt, vd, tmarg

        !  Local variables

        integer :: lap
        real(dp) :: b,bc20m,bcrit,c0,delt,fcond,icrit,iooic, &
        jcritsc,jcrit0,jcritm,jcritp,jcritstr,jsc,jstrand,jtol,jwdgop, &
        t,tc0m,tcrit,ttest,ttestm,ttestp, tdump, fhetot

        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Rename tdmptf as it is called tdump in this routine and those called from here.
        tdump = tdmptf

        ! Helium channel
        fhetot = fhe + (pi/4.0d0)*dhecoil*dhecoil/acs
        !  Conductor fraction (including central helium channel)
        fcond = 1.0D0 - fhetot

        !  Find critical current density in superconducting strand, jcritstr
        select case (isumat)

        case (1)  !  ITER Nb3Sn critical surface parameterization
            bc20m = 32.97D0
            tc0m = 16.06D0

            !  jcritsc returned by itersc is the critical current density in the
            !  superconductor - not the whole strand, which contains copper
            call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (2)  !  Bi-2212 high temperature superconductor parameterization

            !  Current density in a strand of Bi-2212 conductor
            !  N.B. jcrit returned by bi2212 is the critical current density
            !  in the strand, not just the superconducting portion.
            !  The parameterization for jcritstr assumes a particular strand
            !  composition that does not require a user-defined copper fraction,
            !  so this is irrelevant in this model
            jstrand = jwp * aturn / (acs*fcond)

            call bi2212(bmax,jstrand,thelium,fhts,jcritstr,tmarg)
            jcritsc = jcritstr / (1.0D0-fcu)
            tcrit = thelium + tmarg
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (3)  !  NbTi data
            bc20m = 15.0D0
            tc0m = 9.3D0
            c0 = 1.0D10
            call jcrit_nbti(thelium,bmax,c0,bc20m,tc0m,jcritsc,tcrit)
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (4)  !  ITER Nb3Sn parameterization, but user-defined parameters
            bc20m = bcritsc
            tc0m = tcritsc
            call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (5) ! WST Nb3Sn parameterisation
            bc20m = 32.97D0
            tc0m = 16.06D0
            !  jcritsc returned by itersc is the critical current density in the
            !  superconductor - not the whole strand, which contains copper
            call wstsc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (6) ! "REBCO" 2nd generation HTS superconductor in CrCo strand
            write(*,*)'ERROR: subroutine supercon has been called but i_tf_sc_mat=6'
            stop
        case default  !  Error condition
            idiags(1) = isumat ; call report_error(105)

        case (7) ! Durham Ginzburg-Landau Nb-Ti parameterisation
            bc20m = b_crit_upper_nbti
            tc0m = t_crit_nbti 
            call GL_nbti(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            jcritstr = jcritsc  * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

        case (8) ! Branch YCBO model fit to Tallahassee data
            bc20m = 430
            tc0m = 185
            call GL_REBCO(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            ! A0 calculated for tape cross section already
            jcritstr = jcritsc * (1.0D0-fcu)
            !  Critical current in cable (copper added at this stage in HTS cables)
            icrit = jcritstr * acs * fcond 
            
            !REBCO fractures in strains above ~+/- 0.7%
            if (strncon_tf > 0.7D-2 .or. strncon_tf < -0.7D-2) then
                fdiags(1) = strncon_tf ; call report_error(261)
            end if
            

        end select

        ! Critical current density in winding pack
        ! aturn : Area per turn (i.e. entire jacketed conductor with insulation) (m2)
        jwdgcrt = icrit / aturn
        !  Ratio of operating / critical current
        iooic = iop / icrit
        !  Operating current density
        jwdgop = iop / aturn
        !  Actual current density in superconductor, which should be equal to jcrit(thelium+tmarg)
        !  when we have found the desired value of tmarg
        jsc = iooic * jcritsc

        if(iooic<=0d0) then
            write(*,*) 'ERROR:Negative Iop/Icrit for TF coil'
            write(*,*) 'jsc', jsc, '  iooic', iooic, '  jcritsc', jcritsc
            write(*,*) 'Check conductor dimensions. fcond likely gone negative. fcond =', fcond
        end if

        ! REBCO measurements from 2 T to 14 T, extrapolating outside this
        if((isumat == 8) .and. (bmaxtfrp >= 14)) then
        call report_error(266)
        end if

        !  Temperature margin (already calculated in bi2212 for isumat=2)
        if ((isumat == 1).or.(isumat == 4).or.(isumat == 3).or.(isumat == 5).or.(isumat == 7)) then

            !  Newton-Raphson method; start approx at requested minimum temperature margin
            ttest = thelium + tmargmin_tf + 0.001d0
            delt = 0.01D0
            jtol = 1.0D4

            lap = 0
            solve_for_tmarg: do ; lap = lap+1
                if ((ttest <= 0.0D0).or.(lap > 100)) then
                    idiags(1) = lap ; fdiags(1) = ttest ; call report_error(157)
                    exit solve_for_tmarg
                end if
                ! Calculate derivative numerically
                ttestm = ttest - delt
                ttestp = ttest + delt

                select case (isumat)
                    ! Issue #483 to be on the safe side, check the fractional as well as the absolute error
                case (1,4)
                    call itersc(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
                    if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                    call itersc(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
                    call itersc(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
                case (3)
                    call jcrit_nbti(ttest ,bmax,c0,bc20m,tc0m,jcrit0,t)
                    if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                    call jcrit_nbti(ttestm,bmax,c0,bc20m,tc0m,jcritm,t)
                    call jcrit_nbti(ttestp,bmax,c0,bc20m,tc0m,jcritp,t)
                case (5)
                    call wstsc(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
                    if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                    call wstsc(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
                    call wstsc(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
                case (7)
                    call GL_nbti(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
                    if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                    call GL_nbti(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
                    call GL_nbti(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
                case (8)
                    call GL_REBCO(ttest ,bmax,strain,bc20m,tc0m,jcrit0,b,t)
                    if ((abs(jsc-jcrit0) <= jtol).and.(abs((jsc-jcrit0)/jsc) <= 0.01)) exit solve_for_tmarg
                    call GL_REBCO(ttestm,bmax,strain,bc20m,tc0m,jcritm,b,t)
                    call GL_REBCO(ttestp,bmax,strain,bc20m,tc0m,jcritp,b,t)
                end select
                ttest = ttest - 2.0D0*delt*(jcrit0-jsc)/(jcritp-jcritm)
            end do solve_for_tmarg
            tmarg = ttest - thelium
            temp_margin = tmarg

        end if

        !  Find the current density limited by the protection limit
        !  (N.B. Unclear of this routine's relevance for Bi-2212 (isumat=2), due
        !  to presence of fcu argument, which is not used for this model above)

        call protect(iop,tfes,acs,aturn,tdump,fcond,fcu,thelium,tmax,jwdgpro,vd)


        if (iprint == 0) return       ! Output --------------------------

        if (ttest <= 0.0D0) then
            write(*,*)'ERROR: Negative TFC temperature margin'
            write(*,*)'ttest  ', ttest, 'bmax   ', bmax
            write(*,*)'jcrit0 ', jcrit0,'jsc    ', jsc
            write(*,*)'ttestp ', ttestp,'ttestm ', ttestm
            write(*,*)'jcritp ', jcritp,'jcritm ', jcritm
        endif

        call oheadr(outfile,'Superconducting TF Coils')
        call ovarin(outfile,'Superconductor switch', '(isumat)',isumat)

        select case (isumat)
        case (1)
            call ocmmnt(outfile,'Superconductor used: Nb3Sn')
            call ocmmnt(outfile,'  (ITER Jcrit model, standard parameters)')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (2)
            call ocmmnt(outfile,'Superconductor used: Bi-2212 HTS')
        case (3)
            call ocmmnt(outfile,'Superconductor used: NbTi')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (4)
            call ocmmnt(outfile,'Superconductor used: Nb3Sn')
            call ocmmnt(outfile,'  (ITER Jcrit model, user-defined parameters)')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (5)
            call ocmmnt(outfile,'Superconductor used: Nb3Sn')
            call ocmmnt(outfile, ' (WST Nb3Sn critical surface model)')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (7)
            call ocmmnt(outfile,'Superconductor used: Nb-Ti')
            call ocmmnt(outfile, ' (Durham Ginzburg-Landau critical surface model)')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        case (8)
            call ocmmnt(outfile,'Superconductor used: REBCO')
            call ocmmnt(outfile, ' (Durham Ginzburg-Landau critical surface model)')
            call ovarre(outfile,'Critical field at zero temperature and strain (T)','(bc20m)',bc20m)
            call ovarre(outfile,'Critical temperature at zero field and strain (K)', '(tc0m)',tc0m)
        end select ! isumat


        if (run_tests==1) then
            call oblnkl(outfile)
            call ocmmnt(outfile, "PROCESS TF Coil peak field fit. Values for t, z and y:")
            call oblnkl(outfile)
            call ovarre(outfile,'Dimensionless winding pack width','(tf_fit_t)', tf_fit_t, 'OP ')
            call ovarre(outfile,'Dimensionless winding pack radial thickness','(tf_fit_z)', tf_fit_z, 'OP ')
            call ovarre(outfile,'Ratio of peak field with ripple to nominal axisymmetric peak field','(tf_fit_y)', tf_fit_y, 'OP ')
        end if

        call oblnkl(outfile)
        call ovarre(outfile,'Helium temperature at peak field (= superconductor temperature) (K)','(thelium)',thelium)
        call ovarre(outfile,'Total helium fraction inside cable space','(fhetot)',fhetot, 'OP ')
        call ovarre(outfile,'Copper fraction of conductor','(fcutfsu)',fcu)
        call ovarre(outfile,'Strain on superconductor','(strncon_tf)',strncon_tf)
        call ovarre(outfile,'Critical current density in superconductor (A/m2)','(jcritsc)',jcritsc, 'OP ')
        call ovarre(outfile,'Critical current density in strand (A/m2)','(jcritstr)',jcritstr, 'OP ')
        call ovarre(outfile,'Critical current density in winding pack (A/m2)', '(jwdgcrt)',jwdgcrt, 'OP ')
        call ovarre(outfile,'Actual current density in winding pack (A/m2)','(jwdgop)',jwdgop, 'OP ')

        call ovarre(outfile,'Minimum allowed temperature margin in superconductor (K)','(tmargmin_tf)',tmargmin_tf)
        call ovarre(outfile,'Actual temperature margin in superconductor (K)','(tmarg)',tmarg, 'OP ')
        call ovarre(outfile,'Critical current (A)','(icrit)',icrit, 'OP ')
        call ovarre(outfile,'Actual current (A)','(cpttf)',cpttf, 'OP ')
        call ovarre(outfile,'Actual current / critical current','(iooic)', iooic, 'OP ')

    end subroutine supercon
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine supercon_croco(aturn,bmax,iop,thelium,     &
        iprint,outfile, &
        jwdgcrt,tmarg)

        !! TF superconducting CroCo conductor using REBCO tape
        !! author: M Kovari, CCFE, Culham Science Centre
        !! bmax : input real : Peak field at conductor (T)
        !! iop : input real : Operating current per turn (A)
        !! thelium : input real : He temperature at peak field point (K)
        !! iprint : input integer : Switch for printing (1 = yes, 0 = no)
        !! outfile : input integer : Fortran output unit identifier
        !! jwdgcrt : output real : Critical winding pack current density (A/m2)
        !! tmarg : output real : Temperature margin (K)
        
        implicit none
        
        real(dp), intent(in) :: aturn, bmax, iop, thelium
        integer, intent(in) :: iprint, outfile
        real(dp), intent(out) :: jwdgcrt, tmarg

        !  Local variables
        real(dp) :: icrit,iooic, jcritsc,jcritstr,jsc,jwdgop, total
        real(dp) :: current_sharing_t
        logical:: validity

        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !  Find critical current density in superconducting strand, jcritstr
        call jcrit_rebco(thelium,bmax,jcritsc,validity,iprint)
        ! acstf : Cable space - inside area (m2)
        ! Set new croco_od - allowing for scaling of croco_od
        croco_od = t_conductor / 3.0d0 - thwcndut * ( 2.0d0 / 3.0d0 )
        conductor%acs =  9.d0/4.d0 * pi * croco_od**2
        acstf = conductor%acs
        conductor%area =  t_conductor**2 ! does this not assume it's a sqaure???

        conductor%jacket_area = conductor%area - conductor%acs
        acndttf = conductor%jacket_area
        
        conductor%jacket_fraction = conductor%jacket_area / conductor%area
        call croco(jcritsc,croco_strand,conductor,croco_od,croco_thick)
        copperA_m2 = iop / conductor%copper_area
        icrit = conductor%critical_current
        jcritstr = croco_strand%critical_current / croco_strand%area

        ! Critical current density in winding pack
        ! aturn : Area per turn (i.e. entire jacketed conductor with insulation) (m2)
        jwdgcrt = icrit / aturn
        !  Ratio of operating / critical current
        iooic = iop / icrit
        !  Operating current density
        jwdgop = iop / aturn
        !  Actual current density in superconductor, which should be equal to jcrit(thelium+tmarg)
        !  when we have found the desired value of tmarg
        jsc = iooic * jcritsc

        ! Temperature margin using secant solver
        call current_sharing_rebco(current_sharing_t, bmax, jsc)
        tmarg = current_sharing_t - thelium
        temp_margin = tmarg         ! Only used in the availabilty routine - see comment to Issue #526

        ! Quench thermal model not in use
        ! call croco_quench(conductor)

        if (iprint == 0) return     ! Output ----------------------------------

        total = conductor%copper_area+conductor%hastelloy_area+conductor%solder_area+ &
        conductor%jacket_area+conductor%helium_area+conductor%rebco_area

        if (temp_margin <= 0.0D0) then
            write(*,*)'ERROR: Negative TFC temperature margin'
            write(*,*)'temp_margin  ', temp_margin, '  bmax   ', bmax
        endif

        call oheadr(outfile,'Superconducting TF Coils')
        call ovarin(outfile,'Superconductor switch', '(isumat)',6)
        call ocmmnt(outfile,'Superconductor used: REBCO HTS tape in CroCo strand')

        call ovarre(outfile,'Thickness of REBCO layer in tape (m)','(rebco_thickness)',rebco_thickness)
        call ovarre(outfile,'Thickness of copper layer in tape (m)','(copper_thick  )', copper_thick)
        call ovarre(outfile,'Thickness of Hastelloy layer in tape (m) ','(hastelloy_thickness)', hastelloy_thickness)

        call ovarre(outfile,'Mean width of tape (m)','(tape_width)', tape_width , 'OP ')
        call ovarre(outfile,'Outer diameter of CroCo copper tube (m) ','(croco_od)', croco_od , 'OP ')
        call ovarre(outfile,'Inner diameter of CroCo copper tube (m) ','(croco_id)',croco_id , 'OP ')
        call ovarre(outfile,'Thickness of CroCo copper tube (m) ','(croco_thick)',croco_thick)

        call ovarre(outfile,'Thickness of each HTS tape ','(tape_thickness)',tape_thickness , 'OP ')
        call ovarre(outfile,'Thickness of stack of tapes (m) ','(stack_thickness)',stack_thickness , 'OP ')
        call ovarre(outfile,'Number of tapes in strand','(tapes)',tapes , 'OP ')
        call oblnkl(outfile)
        call ovarre(outfile,'Area of REBCO in strand (m2)','(rebco_area)',rebco_area , 'OP ')
        call ovarre(outfile,'Area of copper in strand (m2)','(copper_area)',copper_area , 'OP ')
        call ovarre(outfile,'Area of hastelloy substrate in strand (m2) ','(hastelloy_area)',hastelloy_area , 'OP ')
        call ovarre(outfile,'Area of solder in strand (m2)  ','(solder_area)',solder_area , 'OP ')
        call ovarre(outfile,'Total: area of CroCo strand (m2)  ','(croco_strand%area)',croco_strand%area , 'OP ')
        if(abs(croco_strand%area-(rebco_area+copper_area+hastelloy_area+solder_area))>1d-6)then
            call ocmmnt(outfile, "ERROR: Areas in CroCo strand do not add up")
            write(*,*)'ERROR: Areas in CroCo strand do not add up - see OUT.DAT'
        endif

        call oblnkl(outfile)
        call ocmmnt(outfile,'Cable information')
        call ovarin(outfile,'Number of CroCo strands in the cable (fixed) ','',6 , 'OP ')
        call ovarre(outfile,'Total area of cable space (m2)','(acstf)',acstf , 'OP ')

        call oblnkl(outfile)
        call ocmmnt(outfile,'Conductor information (includes jacket, not including insulation)')
        call ovarre(outfile,'Width of square conductor (cable + steel jacket) (m)', &
            '(t_conductor)', t_conductor , 'OP ')
        call ovarre(outfile,'Area of conductor (m2)','(area)', conductor%area , 'OP ')
        call ovarre(outfile,'REBCO area of conductor (mm2)','(rebco_area)',conductor%rebco_area , 'OP ')
        call ovarre(outfile,'Area of central copper bar (mm2)', '(copper_bar_area)', conductor%copper_bar_area, 'OP ')
        call ovarre(outfile,'Total copper area of conductor, total (mm2)','(copper_area)',conductor%copper_area, 'OP ')
        call ovarre(outfile,'Hastelloy area of conductor (mm2)','(hastelloy_area)',conductor%hastelloy_area, 'OP ')
        call ovarre(outfile,'Solder area of conductor (mm2)','(solder_area)',conductor%solder_area, 'OP ')
        call ovarre(outfile,'Jacket area of conductor (mm2)','(jacket_area)',conductor%jacket_area, 'OP ')
        call ovarre(outfile,'Helium area of conductor (mm2)','(helium_area)',conductor%helium_area, 'OP ')
        if(abs(total-conductor%area)>1d-8) then
            call ovarre(outfile, "ERROR: conductor areas do not add up:",'(total)',total , 'OP ')
        endif
        call ovarre(outfile,'Critical current of CroCo strand (A)','(croco_strand%critical_current)', &
        croco_strand%critical_current , 'OP ')
        call ovarre(outfile,'Critical current of conductor (A) ','(conductor%critical_current)', &
        conductor%critical_current , 'OP ')

        if (run_tests==1) then
            call oblnkl(outfile)
            call ocmmnt(outfile, "PROCESS TF Coil peak field fit. Values for t, z and y:")
            call oblnkl(outfile)
            call ovarre(outfile,'Dimensionless winding pack width','(tf_fit_t)', tf_fit_t, 'OP ')
            call ovarre(outfile,'Dimensionless winding pack radial thickness','(tf_fit_z)', tf_fit_z, 'OP ')
            call ovarre(outfile,'Ratio of actual peak field to nominal axisymmetric peak field','(tf_fit_y)', tf_fit_y, 'OP ')
        end if

        call oblnkl(outfile)
        call ovarre(outfile,'Helium temperature at peak field (= superconductor temperature) (K)','(thelium)',thelium)
        call ovarre(outfile,'Critical current density in superconductor (A/m2)','(jcritsc)',jcritsc, 'OP ')
        call ovarre(outfile,'Critical current density in strand (A/m2)','(jcritstr)',jcritstr, 'OP ')
        call ovarre(outfile,'Critical current density in winding pack (A/m2)', '(jwdgcrt)',jwdgcrt, 'OP ')
        call ovarre(outfile,'Actual current density in winding pack (A/m2)','(jwdgop)',jwdgop, 'OP ')

        call ovarre(outfile,'Minimum allowed temperature margin in superconductor (K)','(tmargmin_tf)',tmargmin_tf)
        call ovarre(outfile,'Actual temperature margin in superconductor (K)','(tmarg)',tmarg, 'OP ')
        call ovarre(outfile,'Current sharing temperature (K)','(current_sharing_t)',current_sharing_t, 'OP ')
        call ovarre(outfile,'Critical current (A)','(icrit)',icrit, 'OP ')
        call ovarre(outfile,'Actual current (A)','(cpttf)',cpttf, 'OP ')
        call ovarre(outfile,'Actual current / critical current','(iooic)', iooic, 'OP ')

    end subroutine supercon_croco


    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine protect(aio,tfes,acs,aturn,tdump,fcond,fcu,tba,tmax,ajwpro,vd)

        !! Finds the current density limited by the protection limit
        !! author: P J Knight, CCFE, Culham Science Centre
        !! author: J Miller, ORNL
        !! aio : input real : Operating current (A)
        !! tfes : input real : Energy stored in one TF coil (J)
        !! acs : input real : Cable space - inside area (m2)
        !! aturn : input real : Area per turn (i.e.  entire cable) (m2)
        !! tdump : input real : Dump time (sec)
        !! fcond : input real : Fraction of cable space containing conductor
        !! fcu : input real : Fraction of conductor that is copper
        !! tba : input real : He temperature at peak field point (K)
        !! tmax : input real : Max conductor temperature during quench (K)
        !! ajwpro : output real :  Winding pack current density from temperature
        !! rise protection (A/m2)
        !! vd : output real :  Discharge voltage imposed on a TF coil (V)
        !! This routine calculates maximum conductor current density which
        !! limits the peak temperature in the winding to a given limit (tmax).
        !! It also finds the dump voltage.
        !! <P>These calculations are based on Miller's formulations.
        !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        implicit none

        !  Arguments

        real(dp), intent(in) :: aio, tfes, acs, aturn, tdump, fcond, &
        fcu,tba,tmax
        real(dp), intent(out) :: ajwpro, vd

        !  Local variables

        integer :: no,np
        real(dp) :: aa,ai1,ai2,ai3,ajcp,bb,cc,dd,tav
        real(dp), dimension(11) :: p1, p2, p3

        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !  Integration coefficients p1,p2,p3

        p1(1) = 0.0D0
        p1(2) = 0.8D0
        p1(3) = 1.75D0
        p1(4) = 2.4D0
        p1(5) = 2.7D0
        p1(6) = 2.95D0
        p1(7) = 3.1D0
        p1(8) = 3.2D0
        p1(9) = 3.3D0
        p1(10) = 3.4D0
        p1(11) = 3.5D0

        p2(1) = 0.0D0
        p2(2) = 0.05D0
        p2(3) = 0.5D0
        p2(4) = 1.4D0
        p2(5) = 2.6D0
        p2(6) = 3.7D0
        p2(7) = 4.6D0
        p2(8) = 5.3D0
        p2(9) = 5.95D0
        p2(10) = 6.55D0
        p2(11) = 7.1D0

        p3(1) = 0.0D0
        p3(2) = 0.05D0
        p3(3) = 0.5D0
        p3(4) = 1.4D0
        p3(5) = 2.6D0
        p3(6) = 3.7D0
        p3(7) = 4.6D0
        p3(8) = 5.4D0
        p3(9) = 6.05D0
        p3(10) = 6.8D0
        p3(11) = 7.2D0

        !  Dump voltage

        vd = 2.0D0 * tfes/(tdump*aio)

        !  Current density limited by temperature rise during quench

        tav = 1.0D0 + (tmax-tba)/20.0D0
        no = int(tav)
        np = no+1
        np = min(np,11)

        ai1 = 1.0D16 * ( p1(no)+(p1(np)-p1(no)) * (tav - no) )
        ai2 = 1.0D16 * ( p2(no)+(p2(np)-p2(no)) * (tav - no) )
        ai3 = 1.0D16 * ( p3(no)+(p3(np)-p3(no)) * (tav - no) )

        aa = vd * aio/tfes
        bb = (1.0D0-fcond)*fcond*fcu*ai1
        cc = (fcu*fcond)**2 * ai2
        dd = (1.0D0-fcu)*fcu * fcond**2 * ai3
        ajcp = sqrt( aa* (bb+cc+dd) )
        ajwpro = ajcp*(acs/aturn)

    end subroutine protect

end subroutine tfspcall

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function croco_voltage()

    !! Finds the coil voltage during a quench
    
    ! croco_voltage : voltage across a TF coil during quench (V)
    ! tdmptf /10.0/ : fast discharge time for TF coil in event of quench (s) (time-dump-TF)
    ! For clarity I have copied this into 'time2' or 'tau2' depending on the model.
    use tfcoil_variables, only: n_tf, quench_model, tdmptf, cpttf

    implicit none

    real(dp):: croco_voltage

    if(quench_model=='linear')then
        time2 = tdmptf
        croco_voltage = 2.0D0/time2 * (estotft/n_tf) / cpttf
    elseif(quench_model=='exponential')then
        tau2 = tdmptf
        croco_voltage = 2.0D0/tau2 * (estotft/n_tf) / cpttf
    endif

end function croco_voltage

! --------------------------------------------------------------------
subroutine croco_quench(conductor)

    !! Finds the current density limited by the maximum temperatures in quench
    !! It also finds the dump voltage.
    use tfcoil_variables, only: tmax_croco, bmaxtf, quench_detection_ef, &
        tftmp, croco_quench_temperature, jwptf, t_conductor
    use superconductors, only: copper_properties2, jcrit_rebco
    use ode_mod, only: ode
    use maths_library, only: secant_solve
    implicit none

    type(volume_fractions), intent(in)::conductor
    real(dp):: current_density_in_conductor


    real(dp)::tout     !for the phase 2
    real(dp)::relerr= 0.01d0, abserr= 0.01d0

    integer(kind=4), parameter :: neqn = 1
    integer(kind=4) :: iflag
    integer(kind=4) :: iwork(5)

    real(dp) :: work(100+21*neqn)
    real(dp) :: y(neqn)

    real(dp)::residual, t
    logical::error

    if(quench_detection_ef>1d-10)then
        ! Two-phase quench model is used.
        ! Phase 1
        ! Issue #548, or see K:\Power Plant Physics and Technology\PROCESS\HTS\
        ! Solve for the temperature at which the quench detection field is reached.
        ! secant_solve(f,x1,x2,solution,error,residual,opt_tol)
        current_density_in_conductor = jwptf *  (t_cable / t_conductor)**2
        call secant_solve(detection_field_error,5d0, 70d0,T1,error,residual)
        ! T1 = Peak temperature of normal zone before quench is detected

        ! Obsolete but leave here for the moment
        ! croco_quench_factor = conductor%copper_fraction / jwptf**2

        if(T1>tmax_croco)write(*,*)'Phase 1 of quench is too hot: T1 = ',T1
    else
        ! Quench is detected instantly - no phase 1.
        T1 = tftmp
    endif

    ! vtfskv : voltage across a TF coil during quench (kV)
    ! tdmptf /10.0/ : fast discharge time for TF coil in event of quench (s) (time-dump-TF)
    ! For clarity I have copied this into 'time2' or 'tau2' depending on the model.

    ! if(quench_model=='linear')then
    !     time2 = tdmptf
    !     vtfskv = 2.0D0/time2 * (estotft/n_tf) / cpttf
    ! elseif(quench_model=='exponential')then
    !     tau2 = tdmptf
    !     vtfskv = 2.0D0/tau2 * (estotft/n_tf) / cpttf
    ! endif

    ! PHASE 2 OF QUENCH: fast discharge into resistor
    ! The field declines in proportion to the current.
    ! The operating current is iop.
    ! The peak field at the operating current is bmaxtfrp
    ! This is declared in global_variable.f90, so is in scope.
    ! Solve the set of differential equations
    ! subroutine ode ( f, neqn, y, t, tout, relerr, abserr, iflag, work, iwork )
    ! See ode.f90 for details.
    !    declare F in an external statement, supply the double precision
    !      SUBROUTINE F ( T, Y, YP )
    y(1) = T1
    tout = 2.0d0 * tau2
    iflag = 1
    ! Starting time
    t = 0d0
    ! Remember that t will be set to the finish time by the ode solver!
    ! ODE SOLVER
    call ode(dtempbydtime, neqn, y, t, tout, relerr, abserr, iflag, work, iwork)
    if(iflag /= 2)write(*,*)'ODE in subroutine croco_quench failed: iflag =', iflag

    croco_quench_temperature = y(1)


contains
    function detection_field_error(t1)
        ! Issue #548.
        ! The difference beteween the actual voltage developed during the first
        ! phase of the quench and the specified detection voltage

        implicit none

        real(dp)::detection_field_error, deltaj,jcritsc

        real(dp), intent(in) :: t1
        real(dp):: jc
        logical :: validity
        integer :: iprint

        call copper_properties2(t1,bmaxtf,copper)
        call jcrit_rebco(t1,bmaxtf,jcritsc,validity,iprint)

        ! Critical current density at specified temperature t1, operating maximum field bmaxtf
        jc = jcritsc * conductor%rebco_fraction

        ! By definition jc=0 below the critical temperature at operating field
        ! All the current flows in the copper
        ! Note that the copper  resisitivity is a function of temperature, so it should still
        ! be possible to solve for the correct detection voltage.
        if(jc<0) jc = 0d0

        deltaj = (current_density_in_conductor - jc)
        detection_field_error = deltaj * copper%resistivity / conductor%copper_fraction &
        - quench_detection_ef
    end function

end subroutine croco_quench

!-------------------------------------------------------------------
subroutine dtempbydtime ( qtime, qtemperature, derivative )
    !! Supplies the right hand side of the ODE for the croco quench phase 2 subroutine
    !! author: M Kovari, CCFE, Culham Science Centre
    !! qtime : input real : time, the independent variable
    !! qtemperature : input real : temperature, the dependent variable
    !! derivative : output real : the value of dtempbydtime

    ! Time-dependent quantities during the fast discharge local to this subroutine:
    use tfcoil_variables, only: quench_model, bmaxtfrp, cpttf
    use superconductors, only: jcrit_rebco

    implicit none

    ! time, the independent variable
    real(dp),intent(in) :: qtime

    ! Y(), the dependent variable
    real(dp),intent(in) :: qtemperature(1)

    ! YP(), the value of the derivative
    real(dp),intent(out) :: derivative(1)

    real(dp)::qj  ! Current density in superconductor during fast discharge
    real(dp)::qcurrent  ! Total current in cable during fast discharge
    real(dp)::qbfield  ! Peak magnetic field in cable during fast discharge
    real(dp)::q_crit_current ! Critical current during fast discharge
    logical :: validity
    real(dp)::qratio,qtemp

    !write(*,*)'subroutine dtempbydtime ( qtime, qtemperature, derivative )'
    !write(*,*)'qtime = ',qtime,' qtemperature = ',qtemperature

    ! For convenience
    qtemp = qtemperature(1)

    ! The current is a known function of time
    if(quench_model=='linear')then
        qcurrent = cpttf * (1 - qtime / time2)
    elseif(quench_model=='exponential')then
        qcurrent = cpttf * exp(- qtime / tau2)
    endif

    ! Field is proportional to current
    qbfield = bmaxtfrp * qcurrent / cpttf

    ! Critical current 'qj' given field and temperature
    call jcrit_rebco(qtemp,qbfield,qj,validity,0)
    q_crit_current = conductor%rebco_area * qj

    ! The jacket is now included in the argument list
    qratio = resistivity_over_heat_capacity(qtemp,qbfield,copper,hastelloy,solder,helium,jacket)

    ! Derivatives

    derivative(1) = (qcurrent - q_crit_current)**2 * qratio / &
    (conductor%copper_fraction * conductor%area**2)


    !write(*,*)'subroutine dtempbydtime: derivative =',derivative(1)
    return
end subroutine dtempbydtime

!-----------------------------------------------------------------------
subroutine cpost( rtop, ztop, rmid, hmaxi, curr, rho, fcool, r_tfin_inleg, &  ! Inputs
                  ins_th, gr_ins_th, cas_in_th, cas_out_th, n_turns_tot,   &  ! Inputs
                  a_cp_cool, vol_cond_cp, respow,        & ! Outputs
                  vol_ins_cp, vol_case_cp, vol_gr_ins_cp ) ! Outputs
    !!  author: P J Knight, CCFE, Culham Science Centre
    !!  Calculates the volume and resistive power losses of a TART centrepost
    !!  This routine calculates the volume and resistive power losses
    !!  of a TART centrepost. It is assumed to be tapered - narrowest at
    !!  the midplane and reaching maximum thickness at the height of the
    !!  plasma. Above/below the plasma, the centrepost is cylindrical.
    !!  The shape of the taper is assumed to be an arc of a circle.
    !!  P J Knight, CCFE, Culham Science Centre
    !!  21/10/96 PJK Initial version
    !!  08/05/12 PJK Initial F90 version
    !!  16/10/12 PJK Added constants; removed argument pi
    !!  26/06/14 PJK Added error handling
    !!  12/11/19 SK Using fixed cooling cross-section area along the CP 
    !!  26/11/19 SK added the coolant area, the conuctor/isulator/outer casing volume 
    !!  30/11/20 SK added the ground outer ground insulation volume 
    !!  F/MI/PJK/LOGBOOK12, pp.33,34
    !!  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use error_handling, only: fdiags, report_error
    use constants, only: pi
    use tfcoil_variables, only: n_tf
    use build_variables, only: hmax
    implicit none


    !  Arguments
    ! ----------
    ! Inputs 
    ! ------
    real(dp), intent(in) :: rtop
    !! Conductor outer radius at CP top [m]
    !! This includes the trun insulation by the exclude the ground one
        
    real(dp), intent(in) :: rmid
    !! Conductor outer radius at CP mid-plane [m]
    !! This includes the trun insulation by the exclude the ground one

    real(dp), intent(in) :: r_tfin_inleg
    !! Conductor `WP` inner radius [m]
    !! This includes the trun insulation by the exclude the ground one

    real(dp), intent(in) :: ztop
    !! Top vertical position (z) of the CP curved section [m]

    real(dp), intent(in) :: hmaxi
    !! Total CP half vertical length [m]

    real(dp), intent(in) :: curr
    !! CP total current [A]
    
    real(dp), intent(in) :: rho
    !! CP conductor resistivity
    
    real(dp), intent(in) :: fcool
    !! Mid-plane CP conductor cooling fraction    

    real(dp), intent(in) :: ins_th
    !! Turn insulation thickness [m]

    real(dp), intent(in) :: gr_ins_th
    !! Ground insulation thickness [m]

    real(dp), intent(in) :: cas_in_th
    !! Inner casing (bucking cylinder) thickness [m]

    real(dp), intent(in) :: cas_out_th
    !! Outer casing (plasma side) thickness [m]

    real(dp), intent(in) :: n_turns_tot
    !! Total number of turns in CP 
    ! ------


    ! Outputs
    ! -------
    real(dp), intent(out) :: respow
    !! CP resistive power losses [W]
    
    real(dp), intent(out) :: vol_ins_cp
    !! Total CP turn insulation volume [m3]

    real(dp), intent(out) :: vol_gr_ins_cp
    !! Total CP ground insulation volume [m3] 
    
    real(dp), intent(out) :: vol_case_cp
    !! Total CP outer layer case volume [m3]

    real(dp), intent(out) :: vol_cond_cp
    !! Total conductor volume in the centrepost [m3]
    !! Rem : The cooling space is removed

    real(dp), intent(out) :: a_cp_cool
    !! Centrepost cooling area toroidal cross-section [m2]
    !! Rem : constant over the whole CP
    ! -------


    ! Internal variables
    ! ------------------
    real(dp) :: r1,z1,x,y,rc,dz,r,z, a_tfin_hole, res_cyl, res_taped

    real(dp) :: a_cond_midplane
    !! Mid-plane conductor area [m2]
    
    real(dp) :: a_casout
    !! Straight section (top) outter case area [m2] 

    real(dp) :: a_cp_ins
    !! Straight section (top) turn insulation area [m2] 

    real(dp) :: a_cp_gr_ins
    !! Straight section (top) outter ground insulation area [m2] 

    real(dp) :: sum1
    !! Exact conductor volume integration sum [m3]

    real(dp) :: sum2
    !! Resistive heating integration sum [1/m2]

    real(dp) :: sum3
    !! Turn insulation volume integration sum [m3]
    
    real(dp) :: sum4
    !! Outter case volume integration sum [m3]

    real(dp) :: sum5
    !! Outer ground insulation volume integration sum [m3]

    real(dp), dimension(0:100) :: yy_cond
    !! Exact conductor area (to be integrated)

    real(dp), dimension(0:100) :: yy_ins
    !! Turn insulation area (to be integrated)

    real(dp), dimension(0:100) :: yy_gr_ins
    !! Outter ground insulation area (to be integrated)

    real(dp), dimension(0:100) :: yy_casout
    !! Outter case area (to be integrated)

    integer :: ii
    !! Loop iterator
    ! ------------------
    ! ---


    !  Error traps
    ! ------------
    if (rtop <= 0.0D0) then
        fdiags(1) = rtop ; call report_error(115)
    end if

    if (ztop <= 0.0D0) then
        fdiags(1) = ztop ; call report_error(116)
    end if

    if (rmid <= 0.0D0) then
        fdiags(1) = rmid ; call report_error(117)
    end if

    if (hmax <= 0.0D0) then
        fdiags(1) = hmax ; call report_error(118)
    end if

    if ((fcool < 0.0D0).or.(fcool > 1.0D0)) then
        fdiags(1) = fcool ; call report_error(119)
    end if

    if (rtop < rmid) then
        fdiags(1) = rtop ; fdiags(2) = rmid
        call report_error(120)
    end if

    if (hmax < ztop) then
        fdiags(1) = hmax ; fdiags(2) = ztop
        call report_error(121)
    end if
    ! ------------


    ! Mid-plane area calculations
    ! ---------------------------
    ! Area of the innner TF central hole [m2]
    a_tfin_hole = pi * r_tfin_inleg**2

    ! Mid-plane outer casing cross-section area [m2]
    a_casout = pi * ( ( rmid + gr_ins_th + cas_out_th )**2   &
                    - ( rmid + gr_ins_th )**2 )

    ! Mid-plane outter ground insulation thickness [m2]
    a_cp_gr_ins = pi * ( ( rmid + gr_ins_th )**2 - rmid**2 )  &
                + 2.0D0 * gr_ins_th * ( rmid - r_tfin_inleg ) * n_tf

    ! Mid-plane turn layer cross-section area [m2] 
    a_cp_ins = pi * ( ( r_tfin_inleg + ins_th )**2 - r_tfin_inleg**2 )  & ! Inner layer volume
             + pi * ( rmid**2 - ( rmid - ins_th )**2 )                  & ! Outter layer volume
             + 2.0D0 * n_turns_tot * ins_th * ( rmid - r_tfin_inleg - 2.0D0*ins_th ) ! inter turn separtion

    ! Cooling pipes cross-section per coil [m2]
    a_cp_cool = fcool * ( ( pi*rmid**2 - a_tfin_hole - a_cp_ins ) / n_tf  &
                        - 2.0D0 * gr_ins_th * ( rmid - r_tfin_inleg ) ) ! Wedge ground insulation
    ! ---------------------------


    !  Trivial solutions
    ! ------------------
    if ( abs(fcool) < epsilon(fcool) ) then
        vol_cond_cp = 0.0D0
        respow = 0.0D0
        call report_error(122)
        return
    end if

    if ( abs(rmid - rtop) < epsilon(rtop) ) then

        ! Exact conductor cross-section
        a_cond_midplane = pi*rmid**2 - a_tfin_hole - n_tf * a_cp_cool - a_cp_ins

        ! Volumes and resisitive losses calculations
        vol_cond_cp = 2.0D0 * hmaxi * a_cond_midplane
        vol_ins_cp = 2.0D0 * hmaxi * a_cp_ins
        vol_gr_ins_cp = 2.0D0 * hmaxi * a_cp_gr_ins
        respow = 2.0D0 * hmaxi * curr**2 * rho / a_cond_midplane
        vol_case_cp = 2.0D0 * hmaxi * a_casout

        return
    end if
    ! ------------------


    ! Find centre of circle (RC,0) defining the taper's arc
    ! (r1,z1) is midpoint of line joining (rmid,0) and (rtop,ztop)
    ! Rem : The taper arc is defined using the outer radius of the 
    !       conductor including turn unsulation
    ! -------------------------------------------------------------
    r1 = 0.5D0*(rmid + rtop)
    z1 = 0.5D0*ztop

    x = (r1-rmid)**2 + z1**2
    y = ztop**2 / ( (rtop-rmid)**2 + ztop**2 )

    rc = rmid + sqrt( x / (1.0D0-y) )
    ! -------------------------------------------------------------


    !  Find volume of tapered section of centrepost, and the resistive
    !  power losses, by integrating along the centrepost from the midplane
    ! --------------------------------------------------------------------
    !  Calculate centrepost radius and cross-sectional areas at each Z
    dz = 0.01D0*ztop

    do ii = 0,100
        z = dble(ii) * dz
        z = min(z,ztop)

        r = rc - sqrt( (rc-rmid)**2 - z*z )

        if (r <= 0.0D0) then
            fdiags(1) = r ; fdiags(2) = rc
            fdiags(3) = rmid ; fdiags(4) = z
            call report_error(123)
        end if

        ! Insulation cross-sectional area at z
        yy_ins(ii) = pi * ( (r_tfin_inleg + ins_th)**2 - r_tfin_inleg**2 )            + & ! Inner layer volume
                     pi * ( r**2 - ( r - ins_th )**2 )                                + & ! Outter layer volume
                     2.0D0 * ins_th * (r - r_tfin_inleg - 2.0D0*ins_th) * n_turns_tot     ! inter turn layers

        !  Conductor cross-sectional area at z
        yy_cond(ii) = pi*r**2 - a_tfin_hole - n_tf*a_cp_cool - yy_ins(ii)  &
                    - 2.0D0 * n_tf * gr_ins_th * ( r - r_tfin_inleg )   ! Wedge ground insulation

        !  Outer ground insulation area at z
        yy_gr_ins(ii) = pi * ( ( r + gr_ins_th )**2 - r**2 ) &
                      + 2.0D0 * n_tf * gr_ins_th * ( r - r_tfin_inleg )

        !  Outer casing Cross-sectional area at z 
        yy_casout(ii) = pi * ( ( r + gr_ins_th + cas_out_th )**2 &
                             - ( r + gr_ins_th )**2 )

    end do

    !  Perform integrals using trapezium rule
    sum1 = 0.0D0
    sum2 = 0.0D0
    sum3 = 0.0D0
    sum4 = 0.0D0
    sum5 = 0.0D0
    do ii = 1,99
        sum1 = sum1 + yy_cond(ii)
        sum2 = sum2 + 1.0D0/yy_cond(ii)
        sum3 = sum3 + yy_ins(ii)
        sum4 = sum4 + yy_casout(ii)
        sum5 = sum5 + yy_gr_ins(ii)
    end do

    sum1 = 0.5D0*dz * ( yy_cond(0) + yy_cond(100) + 2.0D0*sum1 )
    sum2 = 0.5D0*dz * ( 1.0D0/yy_cond(0) + 1.0D0/yy_cond(100) + 2.0D0*sum2 )
    sum3 = 0.5D0*dz * ( yy_ins(0) + yy_ins(100) + 2.0D0*sum3 )
    sum4 = 0.5D0*dz * ( yy_casout(0) + yy_casout(100) + 2.0D0*sum4 )
    sum5 = 0.5D0*dz * ( yy_gr_ins(0) + yy_gr_ins(100) + 2.0D0*sum5 )

    ! Turn insulation layer cross section at CP top  [m2]
    a_cp_ins = pi * ( (r_tfin_inleg + ins_th)**2 - r_tfin_inleg**2 )           + & ! Inner layer volume
               pi * ( rtop**2 - ( rtop - ins_th )**2 )                         + & ! Outter layer volume
               2.0D0 * ins_th * (rtop - r_tfin_inleg - 2.0D0*ins_th) * n_turns_tot ! turn separtion layers      

    ! Ground insulation layer cross-section at CP top [m2]
    a_cp_gr_ins = pi * ( ( rtop + gr_ins_th )**2 - rtop**2 ) & 
                + 2.0D0 * gr_ins_th * ( rtop - r_tfin_inleg ) * n_tf 

    ! Outer casing cross-section area at CP top [m2]
    a_casout = pi * ( ( rmid + gr_ins_th + cas_out_th )**2  &
                    - ( rmid + gr_ins_th )**2 )

    ! Centrepost volume (ignoring coolant fraction) [m3]
    vol_cond_cp = 2.0D0*( sum1 + ( hmaxi - ztop ) &
                               * ( pi*rtop**2 - a_tfin_hole - a_cp_ins - n_tf*a_cp_cool &
                                 - 2.0D0*n_tf * gr_ins_th * ( rtop - r_tfin_inleg ) ) ) ! ground insulation separation

    ! Resistive power losses in taped section (variable radius section) [W]
    res_taped = rho * curr**2 * sum2

    ! Centrepost insulator volume [m3]
    vol_ins_cp = 2.0D0 * ( sum3 + ( hmaxi - ztop ) * a_cp_ins )

    ! Ground insulation volume [m3]
    vol_gr_ins_cp = 2.0D0*( sum5 + ( hmaxi - ztop ) * a_cp_gr_ins   &
                          + hmaxi * pi * ( r_tfin_inleg**2          &
                                       - (r_tfin_inleg - gr_ins_th)**2 ) )

    ! CP casing volume [m3]
    vol_case_cp = 2.0D0*( sum4 + (hmaxi - ztop) * a_casout  &
                        + hmaxi * pi * ( ( r_tfin_inleg - gr_ins_th )**2 &
                                       - ( r_tfin_inleg - gr_ins_th - cas_in_th )**2 ) )

    ! Resistive power losses in cylindrical section (constant radius) [W]
    res_cyl = rho * curr**2 * ( ( hmaxi - ztop )   &
                / ( pi * rtop**2 - a_tfin_hole  - a_cp_ins - n_tf*a_cp_cool &
                  - 2.0D0*n_tf * gr_ins_th * ( rtop - r_tfin_inleg ) ) ) ! ground insulation separation

    ! Total CP resistive power [W]
    respow = 2.0D0 * ( res_cyl + res_taped )
    ! --------------------------------------------------------------------

end subroutine cpost

!-----------------------------------------------------------------------
function resistivity_over_heat_capacity(qtemp,qbfield,copper,hastelloy,solder,helium,jacket)
    use superconductors, only: hastelloy_properties, solder_properties, &
        helium_properties, jacket_properties, copper_properties2
    implicit none
    
    real(dp),intent(in):: qtemp,qbfield
    ! Only those materials that are actually supplied in the arguments are used.
    type(resistive_material),intent(in),optional::copper,hastelloy,solder,helium,jacket
    real(dp)::sum,resistivity_over_heat_capacity

    sum = 0d0
    call copper_properties2(qtemp,qbfield, copper)
    if(present(copper))then
        sum = sum + conductor%copper_fraction * copper%density * copper%cp
    end if
    if(present(hastelloy))then
        call hastelloy_properties(qtemp,hastelloy)
        sum = sum + conductor%hastelloy_fraction * hastelloy%density * hastelloy%cp
    end if
    if(present(solder))then
        call solder_properties(qtemp,solder)
        sum = sum + conductor%solder_fraction    * solder%density * solder%cp
    end if
    if(present(helium))then
        call helium_properties(qtemp,helium)
        sum = sum + conductor%helium_fraction    * helium%cp_density
    end if
    if(present(jacket))then
        call jacket_properties(qtemp,jacket)
        sum = sum + conductor%jacket_fraction    * jacket%density * jacket%cp
    end if

    resistivity_over_heat_capacity = copper%resistivity / sum

    ! write(*,'(10(1pe10.3), 1x)')qtemp, copper%resistivity, sum,resistivity_over_heat_capacity
    ! write(*,'(10(1pe10.3), 1x)')conductor%copper_fraction    , copper%density ,copper%cp
    ! write(*,'(10(1pe10.3), 1x)')conductor%hastelloy_fraction , hastelloy%density , hastelloy%cp
    ! write(*,'(10(1pe10.3), 1x)')conductor%solder_fraction    , solder%density , solder%cp
    ! write(*,'(10(1pe10.3), 1x)')conductor%helium_fraction    , helium%cp_density

end function resistivity_over_heat_capacity
!--------------------------------------------------------------


end module sctfcoil_module