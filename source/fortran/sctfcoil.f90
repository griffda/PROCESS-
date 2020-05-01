module sctfcoil_module

!! Module containing superconducting TF coil routines
!! author: P J Knight, CCFE, Culham Science Centre
!! author: J Morris, CCFE, Culham Science Centre
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
public :: outtf, sctfcoil, stresscl, &
tfcind, tfspcall, initialise_cables

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

real(dp), private :: h_cp_top
!! Vertical distance from the midplane to the top of the tapered section [m]

real(dp), private :: r_tf_inboard_in
!! Radial position of inner edge of TF coil inboard leg [m]

real(dp), private :: r_tf_inboard_out
!! Radial position of plasma-facing edge of TF coil inboard leg [m]

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

real(dp), private :: vol_ins
!! Total/CP insulator insulator volume [m3]

real(dp), private :: vol_ins_cp
!! CP insulator insulator volume [m3]

real(dp), private :: vol_ins_leg
!! Outboard leg insulator volume [m3]

real(dp), private :: vol_cond
!! Total conductor insulator volume [m3]

real(dp), private :: vol_cond_leg
!! Outboard leg conductor insulator volume [m3]

real(dp), private :: vol_case_cp
!! Volume of the CP outer casing cylinder

real(dp), private :: t_wp_toroidal
!! Toroidal thickness of of winding pack [m]

real(dp), private :: theta_coil
!! Half toroidal angular extent of a single TF coil inboard leg

real(dp), private :: tan_theta_coil
!! Tan half toroidal angular extent of a single TF coil inboard leg

real(dp), private :: t_conductor_radial, t_conductor_toroidal
!! Conductor area radial and toroidal dimension [m]

real(dp), private :: t_cable_radial, t_cable_toroidal
!! Cable area radial and toroidal dimension [m]

real(dp), private :: t_turn_radial, t_turn_toroidal
!! Turn radial and toroidal dimension [m]

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
        ritfc, thkwp, n_tf, bmaxtfrp, bmaxtf, n_tf_stress_layers, n_rad_per_layer, &
        i_tf_sup, i_tf_shape, b_crit_upper_nbti
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

    call tf_coil_geometry

    call tf_current

    ! Conductor section internal geometry
    ! ---
    ! Resitive magnets
    if ( i_tf_sup /= 1 ) then   
        call tf_turn_geom
    
    ! SC using an integer number of turns per WP
    else if ( i_tf_turns_integer == 1 ) then  
        call tf_integer_winding_pack
    
    ! SC using a float numnber of turns per WP
    else   
        call tf_winding_pack
    end if
    ! ---

    call coilshap

    if ( i_tf_sup /= 1 ) call tf_res_heating

    call tf_field_and_force


    ! Calculation of TF coil inductance
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
    call peak_tf_with_ripple(n_tf, wwp1, thkwp, r_wp_centre, bmaxtf, bmaxtfrp, peaktfflag)

    ! Do stress calculations (writes the stress output)
    if ( iprint == 1 ) n_rad_per_layer = 500
    call stresscl(n_tf_stress_layers, n_rad_per_layer, iprint, outfile)

    if (iprint == 1) call outtf(outfile, peaktfflag)

end subroutine sctfcoil

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_coil_geometry()
    !! Subroutine for calculating the TF coil geometry
    !! This includes:
    !!   - Overall geometry of coil (radii and toroidal planes area)
    !!   - Winding Pack NOT included
    use physics_variables, only: rmajor, bt, kappa, itart, rminor
    use build_variables, only: tfcth, tfthko, r_tf_inboard_mid, &
        r_tf_outboard_mid, r_cp_top
    use tfcoil_variables, only: tinstf, tfc_sidewall_is_fraction, tfareain, &
        ritfc, tftort, n_tf, casthi_is_fraction, bmaxtf, arealeg, &
        casthi_fraction, casths_fraction, tfinsgap, rbmax, casthi, casths, i_tf_sup, &
        dztop, tinstf, tftort, tfinsgap
    use constants, only: pi
    implicit none
    

    ! Inner leg geometry
    ! ---
    ! Radial position of inner/outer edge of inboard TF coil leg [m]
    r_tf_inboard_in =  r_tf_inboard_mid - 0.5D0 * tfcth
    r_tf_inboard_out = r_tf_inboard_mid + 0.5D0 * tfcth

    ! Annular area of midplane containing TF coil inboard legs ( WP + casing ) [m2]
    tfareain = pi * (r_tf_inboard_out**2 - r_tf_inboard_in**2)

    ! Vertical distance from the midplane to the top of the tapered section [m]
    if ( itart ==  1 ) h_cp_top = rminor * kappa + dztop 
    ! ---


    ! Outer leg geometry
    ! ---    
    ! Mid-plane inner/out radial position of the TF coil outer leg [m] 
    r_tf_outboard_in =  r_tf_outboard_mid - tfthko * 0.5D0 
    r_tf_outboard_out = r_tf_outboard_mid + tfthko * 0.5D0 

    ! Half toroidal angular extent of a single TF coil inboard leg
    theta_coil = pi/n_tf              ! eq(9)
    tan_theta_coil = tan(theta_coil)

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

end subroutine tf_coil_geometry

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_current()
    !! Calculation of the maximum B field and the corresponding TF current
    use tfcoil_variables, only: casthi, ritfc, rbmax, i_tf_sup, casths_fraction, &
        tinstf, tftort, bmaxtf, tfinsgap, tfc_sidewall_is_fraction, casths, &
        casthi_is_fraction, casthi_fraction
    use physics_variables, only: bt, rmajor
    use build_variables, only: tfcth
    implicit none


    ! Plasma-facing wall thickness if fraction option selected [m]
    if (casthi_is_fraction) casthi = casthi_fraction * tfcth

    ! Case thickness of side wall [m]
    if (tfc_sidewall_is_fraction) casths = casths_fraction * tftort  
    
    ! Radial position of peak toroidal field (assuming axisymmetry) [m]
    ! (assumed to be at the outer edge of the winding pack)    
    if ( i_tf_sup == 1 ) then
        rbmax = r_tf_inboard_out - casthi - tinstf - tfinsgap
    else 
        rbmax = r_tf_inboard_out - casthi - tinstf   ! eq(11)
    end if

    ! Calculation of the maximum B field on the magnet [T]
    bmaxtf = bt * rmajor / rbmax  
    
    ! Total current in TF coils [A]
    ! rem SK : ritcf is no longer an input
    ritfc = bmaxtf * rbmax * 5.0D6 

end subroutine tf_current

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_turn_geom()
    !! Resisitve TF turn geometry, equivalent to winding_pack subroutines
    use tfcoil_variables, only: turnstf, tinstf, thkcas, thkwp, tftort, n_tf, &
        tfareain, ritfc, oacdcp, fcoolcp, cpttf, cdtfleg, casthi, aiwp, acasetf
    use build_variables, only: tfthko
    use constants, only: pi
    implicit none
            
    ! Radial position of inner/outer edge of winding pack [m]
    r_wp_inner = r_tf_inboard_in  + thkcas + tinstf 
    r_wp_outer = r_tf_inboard_out - casthi - tinstf 

    ! Mid-plane Radial thickness of conductor layer [m]
    thkwp = r_wp_outer - r_wp_inner

    ! Number of turns
    ! Set by user (no turn structure by default, i.e. turnstf = 1 ) 
    if ( abs(turnstf) < epsilon(turnstf) ) turnstf = 1.0D0

    ! Total mid-plane cross-sectional area of winding pack, [m2]
    ! including the surrounding ground-wall insulation layer 
    awpc = pi * ( (r_wp_outer + tinstf)**2 - (r_wp_inner - tinstf)**2 ) / n_tf

    ! Exact mid-plane cross-section area of the conductor per TF turn [m2]
    awptf = ( 1.0D0 - fcoolcp ) * ( pi*(r_wp_outer**2 - r_wp_inner**2)/(n_tf*turnstf) - &
                                  2.0D0 * tinstf * thkwp )

    ! Inter turn insulation area per turn [m2]                    
    aiwp = (awpc / turnstf) - awptf / ( 1.0D0 - fcoolcp )  

    ! Total cross-sectional area of surrounding case [m2]
    ! Only valid at mid-plane for resistive itart design
    acasetf = ( tfareain / n_tf ) - awpc 

    ! Current per turn 
    cpttf = ritfc / ( turnstf * n_tf )

    ! Exact current density on the mid-plane conductors  
    oacdcp = ritfc / ( awptf * n_tf * turnstf ) 

    ! Exact current density on TF oubard legs
    cdtfleg = ritfc / ( ( 1.0D0 - fcoolcp ) * &
                        ( tftort - 2.0D0 * turnstf * tinstf) * &
                        ( tfthko - 2.0D0 * tinstf ) ) 


end subroutine tf_turn_geom

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_winding_pack()
    !! Subroutine for calculating winding pack quantities
    !!   - Overall dimensions of winding pack
    !!   - Turn dimensions
    !!   - Current, field, etc.
    !!   - Turns geometry
    use error_handling, only: fdiags, report_error
    use build_variables, only: tfcth, tfthko
    use tfcoil_variables, only: dhecoil, thicndut, cpttf, aswp, aiwp, tftort, &
        leni, turnstf, tfareain, n_tf, tinstf, leno, acstf, wwp1, &
        vftf, avwp, jwptf, acasetfo, acasetf, wwp2, thwcndut, insulation_area, &
        tftort, aswp, tinstf, turnstf, leno, acasetf, n_tf, jwptf, &
        thwcndut, thicndut, wwp1, &
        dhecoil, tfareain, leni, insulation_area, cpttf, ritfc, thkwp, &
        arealeg, casths, awphec, acndttf, acond, layer_ins, thkcas, &
        conductor_width, oacdcp, tfinsgap, casthi, i_tf_sc_mat
    use global_variables, only: icase
    use constants, only: pi
    use numerics, only: nvar, ixc
    implicit none

    ! Local variables
    !----------------
    ! Rounded corner radius
    real(dp) :: rbcndut
    real(dp) :: A
    !----------------


    ! Radial position of inner edge of winding pack [m]
    ! Rem SK : added the insulation thickness/insertion gap
    r_wp_inner = r_tf_inboard_in + thkcas + tinstf + tfinsgap  
        
    if (any(ixc(1:nvar) == 140) ) then
      ! Radial thickness of TF coil inboard leg [m]
      tfcth = thkwp + casthi + thkcas + 2.0D0*tinstf + 2.0d0*tfinsgap
    else
      ! Radial thickness of winding pack [m]
      thkwp = tfcth - casthi - thkcas - 2.0D0*tinstf - 2.0d0*tfinsgap
    end if
    
    ! Radial position of outer edge of winding pack [m]
    r_wp_outer = r_wp_inner + thkwp

    ! Global inboard leg average current in TF coils [A/m2]
    oacdcp = ritfc / tfareain

    ! Current per TF coil [A]
    tfc_current = ritfc/n_tf

    ! Radius of geometrical centre of winding pack [m]
    r_wp_centre = 0.5D0 * ( r_wp_inner + r_wp_outer )

    ! Thickness of winding pack section at R > r_wp_centre [m]
    wwp1 = 2.0D0 * (r_wp_centre*tan_theta_coil - casths - tinstf - tfinsgap)

    ! Thickness of winding pack section at R < r_wp_centre [m]
    wwp2 = 2.0D0 * ( r_wp_inner*tan_theta_coil - casths - tinstf - tfinsgap )

    ! Total cross-sectional area of winding pack [m2]
    awptf = (0.5D0*thkwp)*(wwp1 + wwp2)

    ! Total cross-sectional area of winding pack, [m2]
    ! including the surrounding ground-wall insulation layer
    ! and insertion gap [m2]
    awpc = 0.5D0*thkwp*(wwp2 + 2.0D0*tinstf + 2.0d0*tfinsgap) + &
        (0.5D0*thkwp + 2.0D0*tinstf + 2.0d0*tfinsgap)*(wwp1 + 2.0D0*tinstf+ 2.0d0*tfinsgap)

    ! Total cross-sectional area of surrounding case [m2]
    acasetf = ( tfareain / n_tf ) - awpc   ! eq(14)

    if ((awptf <= 0.0D0).or.(awpc <= 0.0D0).or.(acasetf <= 0.0D0)) then
        fdiags(1) = awptf ; fdiags(2) = awpc ; fdiags(3) = acasetf
        call report_error(99)
        write(*,*) 'Error in routine SCTFCOIL: Winding pack cross-section problem'
        write(*,*) 'awptf = ',awptf, '  awpc = ',awpc, '  acasetf = ',acasetf
        write(*,*) 'KE thkwp, wwp1, wwp2 = ', thkwp, ', ', wwp1, ', ', wwp2
        !negative awptf comes from neg. thkwp
        write(*,*) 'tfcth, casthi, thkcas, tinstf, tfinsgap = ', tfcth, ', ', &
            casthi, ', ', thkcas, ', ', tinstf, ', ', tfinsgap
        write(*,*) ' '
    end if

    ! Cross-sectional area of surrounding case, outboard leg [m2]
    acasetfo = arealeg - awpc

    ! Winding pack current density (forced to be positive) [A/m2]
    jwptf = max(1.0D0, ritfc/(n_tf*awptf))    

    ! Dimension of square conductor [m]
    ! Allow for additional inter-layer insulation MDK 13/11/18
    ! Area of turn including conduit and inter-layer insulation
    A = cpttf / jwptf

    ! Dimension of square cross-section of each turn including inter-turn insulation [m]
    leno = sqrt(cpttf / jwptf)

    ! See derivation in k:\power plant physics and technology\process\hts\hts coil module for process.docx
    conductor_width = (-layer_ins + sqrt(layer_ins**2 + 4.d0*A))/2.d0 - 2.0D0*thicndut

    ! Total number of turns per TF coil (not required to be an integer)
    turnstf = awptf / A

    ! Area of inter-turn insulation: single turn [m2]
    insulation_area = A - conductor_width**2

    ! Area of inter-turn insulation: total [m2]
    aiwp = turnstf * insulation_area

    ! Area of steel structure in winding pack [m2]
    aswp = turnstf*acndttf

    if ( i_tf_sc_mat .ne. 6) then  ! NOT REBCO
        ! Radius of rounded corners of cable space inside conduit [m]
        rbcndut = thwcndut * 0.75D0     

        ! Dimension of square cable space inside conduit [m]
        leni = conductor_width - 2.0D0*thwcndut

        ! Cross-sectional area of cable space per turn
        ! taking account of rounded inside corners [m2]
        acstf = leni**2 - (4.0D0-pi)*rbcndut**2

        if (acstf <= 0.0D0) then
            if (leni < 0.0D0) then
                fdiags(1) = acstf ; fdiags(2) = leni
                call report_error(101)
                write(*,*) 'Warning in routine SCTFCOIL:'
                write(*,*) 'Cable space area, acstf = ',acstf, 'Cable space dimension, leni = ',leni
                write(*,*) ' '
            else
                fdiags(1) = acstf ; fdiags(2) = leni
                call report_error(102)
                write(*,*) 'Warning in routine SCTFCOIL:'
                write(*,*) 'Cable space area, acstf = ',acstf,&
                     'Cable space dimension, leni = ',leni
                write(*,*) 'Reduce the upper limit for thwcndut (TF coil conduitcase thickness, iteration variable 58),'
                write(*,*) 'or remove it from the list of iteration variables.'
                write(*,*) 'Artificially set rounded corner radius to zero'
                write(*,*)
                rbcndut = 0.0D0
                acstf = leni**2
            end if
        end if

        ! Cross-sectional area of conduit jacket per turn [m2]
        acndttf = conductor_width**2 - acstf
        ! Central helium channel down the conductor core [m2]
        awphec = turnstf * ((pi/4.0d0)*dhecoil**2)
        ! Total conductor cross-sectional area, taking account of void area
        ! and central helium channel [m2]
        acond = acstf * turnstf * (1.0D0-vftf) - awphec
        ! Void area in conductor for He, not including central channel [m2]
        avwp = acstf * turnstf * vftf
        
    else if (i_tf_sc_mat == 6 ) then  ! REBCO
        ! Diameter of circular cable space inside conduit [m]
        leni = conductor_width - 2.0D0*thwcndut
        ! Cross-sectional area of conduit jacket per turn [m2]
        acndttf = conductor_width**2 - acstf

    end if  

end subroutine tf_winding_pack

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_integer_winding_pack()
    !! Subroutine to calculate integer winding pack   
    use error_handling, only: fdiags, report_error
    use build_variables, only: tfcth, tfthko
    use tfcoil_variables, only: dhecoil, thicndut, cpttf, aswp, aiwp, tftort, &
        leni, turnstf, tfareain, casths, n_tf, tinstf, acstf, wwp1, &
        acndttf, vftf, avwp, jwptf, acasetfo, acasetf, thwcndut, &
        insulation_area, avwp, arealeg, acasetf, ritfc, vftf, n_pancake, &
        acstf, jwptf, acasetfo, wwp1, insulation_area, thwcndut, awphec, &
        tinstf, acndttf, acond, oacdcp, n_layer, thkwp, thkcas, tfinsgap, &
        casthi, i_tf_sc_mat
    use constants, only: pi
    use maths_library, only: hybrd
    use numerics, only: nvar, ixc
    implicit none
    
    ! Local variables
    !----------------
    ! Radius of rounded corners of cable space inside conduit [m]
    real(dp) :: rbcndut

    ! TF coil width at inner egde of winding pack toroidal direction [m]
    real(dp) :: t_tf_at_wp
    !----------------


    if(i_tf_sc_mat==6)then
        write(*,*)'Integer turns in TF coil not yet available for CROCO model (i_tf_turns_integer == 1)'
        stop
    end if

    ! Total number of turns
    turnstf = n_pancake*n_layer

    if (any(ixc(1:nvar) == 140)) then
      ! Radial thickness of TF coil inboard leg [m]
      tfcth = thkwp + casthi + thkcas + 2.0D0*tinstf + 2.0d0*tfinsgap
    else
      ! Radial thickness of winding pack [m]
      thkwp = tfcth - casthi - thkcas - 2.0D0*tinstf - 2.0d0*tfinsgap
    endif

    ! Radial position of inner edge of winding pack [m]
    r_wp_inner = r_tf_inboard_in + thkcas + tinstf + tfinsgap

    ! Radial position of outner edge of winding pack [m]
    r_wp_outer = r_wp_inner + thkwp

    ! Global inboard leg average current in TF coils [A/m2]
    oacdcp = ritfc / tfareain
    
    ! Current per TF coil [A]
    tfc_current = ritfc/n_tf

    ! Radius of geometrical centre of winding pack [m]
    r_wp_centre = 0.5D0 * ( r_wp_inner + r_wp_outer )

    ! TF coil width at inner egde of winding pack toroidal direction [m]
    t_tf_at_wp = 2.0D0 * r_wp_inner*sin(theta_coil)

    ! Toroidal thickness of winding pack [m]
    t_wp_toroidal = t_tf_at_wp - casths - 2.0D0*tinstf - 2.0D0*tfinsgap
    wwp1 = t_wp_toroidal

    ! Total cross-sectional area of winding pack [m2]
    awptf = thkwp*t_wp_toroidal

    ! Total cross-sectional area of winding pack,
    ! including the surrounding ground-wall insulation layer
    ! and insertion gap [m2]
    awpc = (thkwp + 2.0D0*tinstf + 2.0D0*tfinsgap)* &
        (t_wp_toroidal + 2.0D0*tinstf + 2.0D0*tfinsgap)

    ! Total cross-sectional area of surrounding case [m2]
    acasetf = (tfareain/n_tf) - awpc

    if ((awptf <= 0.0D0).or.(awpc <= 0.0D0).or.(acasetf <= 0.0D0)) then
        fdiags(1) = awptf ; fdiags(2) = awpc ; fdiags(3) = acasetf
        call report_error(99)
        write(*,*) 'Error in routine SCTFCOIL:'
        write(*,*) 'Winding pack cross-section problem'
        write(*,*) 'awptf = ',awptf
        write(*,*) 'awpc = ',awpc
        write(*,*) 'acasetf = ',acasetf
        write(*,*) ' '
    end if

    ! Area of rectangular cross-section outboard leg [m2]
    arealeg = tftort * tfthko

    ! Cross-sectional area of surrounding case, outboard leg [m2]
    acasetfo = arealeg - awpc

    ! Winding pack current density (forced to be positive) [A/m2]
    jwptf = max(1.0D0, ritfc/(n_tf*awptf))

    ! Radius of rounded corners of cable space inside conduit [m]
    rbcndut = thwcndut * 0.75D0

    ! TODO: leno compatibility with croco

    ! Radial turn dimension [m]
    t_turn_radial = thkwp/n_layer

    if (t_turn_radial <= (2.0D0*thicndut + 2.0D0*thwcndut)) then
        write(*,*) 'Error in routine SCTFCOIL:'
        write(*,*) 'Turn radial dimension too small. No cable space'
        write(*,*) 'Turn radial dimension [m]', t_turn_radial
        write(*,*) '2*thicndut + 2*thwcndut', 2.0D0*thicndut + 2.0D0*thwcndut
        write(*,*) ' '
    end if

    ! Toroidal turn dimension [m]
    t_turn_toroidal = t_wp_toroidal/n_pancake

    if (t_turn_toroidal <= (2.0D0*thicndut + 2.0D0*thwcndut)) then
        write(*,*) 'Error in routine SCTFCOIL:'
        write(*,*) 'Turn toroidal dimension too small. No cable space'
        write(*,*) 'Turn toroidal dimension [m]', t_turn_toroidal
        write(*,*) '2*thicndut + 2*thwcndut', 2.0D0*thicndut + 2.0D0*thwcndut
        write(*,*) ' '
    end if

    ! Current per turn [A/turn]
    cpttf = tfc_current/turnstf

    ! Radial and toroidal dimension of conductor [m]
    t_conductor_radial = t_turn_radial - 2.0D0*thicndut
    t_conductor_toroidal = t_turn_toroidal - 2.0D0*thicndut

    ! Dimension of square cable space inside conduit [m]
    t_cable_radial = t_conductor_radial - 2.0D0*thwcndut
    t_cable_toroidal = t_conductor_toroidal - 2.0D0*thwcndut

    ! Cross-sectional area of cable space per turn
    ! taking account of rounded inside corners [m2]
    acstf = (t_cable_radial*t_cable_toroidal) - (4.0D0-pi)*rbcndut**2

    if (acstf <= 0.0D0) then
        if ((t_cable_radial < 0.0D0).or.(t_cable_toroidal < 0.0D0)) then
            fdiags(1) = acstf; fdiags(2) = t_cable_radial; fdiags(3) = t_cable_toroidal
            call report_error(101)
            write(*,*) 'Warning in routine SCTFCOIL:'
            write(*,*) 'Cable space area, acstf = ',acstf
            write(*,*) 'Cable space radial dimension, t_cable_radial = ', t_cable_radial
            write(*,*) 'Cable space radial dimension, t_cable_toroidal = ', t_cable_toroidal
            write(*,*) ' '
        else
            fdiags(1) = acstf ; fdiags(2) = t_cable_radial
            call report_error(102)
            write(*,*) 'Warning in routine SCTFCOIL:'
            write(*,*) 'Cable space area, acstf = ',acstf, 'Cable space dimension, leni = ',leni
            write(*,*) 'Reduce the upper limit for thwcndut (TF coil conduitcase thickness, iteration variable 58),'
            write(*,*) 'or remove it from the list of iteration variables.'
            write(*,*) 'Artificially set rounded corner radius to zero'
            write(*,*)
            rbcndut = 0.0D0
            acstf = leni**2
        end if
    end if

    ! Cross-sectional area of conduit jacket per turn [m2]
    acndttf = t_conductor_radial*t_conductor_toroidal - acstf

    ! Central helium channel down the conductor core [m2]
    awphec = turnstf * ((pi/4.0d0)*dhecoil**2)

    ! Total conductor cross-sectional area, taking account of void area
    ! and central helium channel [m2]
    acond = acstf * turnstf * (1.0D0-vftf) - awphec

    ! Void area in conductor for He, not including central channel [m2]
    avwp = acstf * turnstf * vftf

    ! Area of inter-turn insulation: single turn [m2]
    insulation_area = t_turn_radial*t_turn_toroidal - acndttf - acstf

    ! Area of inter-turn insulation: total [m2]
    aiwp = turnstf * insulation_area

    ! Area of steel structure in winding pack [m2]
    aswp = turnstf * acndttf

end subroutine tf_integer_winding_pack

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_res_heating()
    !! Resitive magnet resitive heating calculations
    !! Rem SK : Clamped joined superconductors might have resistive power losses on the joints
    !! Rem SK : Sliding joints might have a region of high resistivity
    use tfcoil_variables, only: rhocp, tlegav, tinstf, th_joint_contact, rhotfleg, &
        voltfleg, vol_cond_cp, turnstf, thkcas, tftort, tfleng, tflegres, tcpav, &
        ritfc, rho_tf_joints, presleg, prescp, pres_joints, n_tf_joints_contact, &
        n_tf_joints, n_tf, i_tf_sup, frholeg, frhocp, fcoolcp, casthi, arealeg, &
        a_cp_cool, fcoolleg
    use build_variables, only: tfthko, tfcth, r_cp_top, hmax
    use physics_variables, only: itart
    use constants, only: pi
    implicit none

    ! Internal variable
    ! ---
    real(dp) :: a_wp_ins_turn
    !! TF ouboard leg insulation area  

    real(dp) :: a_wp_cond_leg
    !! Exact TF ouboard leg conductor area 
    
    real(dp) :: a_joints
    !! Total area of joint contact

    integer ::  n_contact_tot
    !! Total number of contact area (4 joints section per legs)

    integer :: is_leg_cp_temp_same = 0
    ! ---

        
    ! Copper : Copper resistivity degraded by 1/0.92 for the used of GLIDCOP A-15 
    !          Better structural properties at high temperature and radiation damage resilience
    if ( i_tf_sup == 0 ) rhocp = (frhocp/0.92D0) * ( 1.72D0 + 0.0039D0*(tcpav-273.15D0) ) * 1.0D-8

    ! Cryogenic aluminium
    if ( i_tf_sup == 2 ) rhocp = frhocp * ( 2.00016D-14*tcpav**3 - 6.75384D-13*tcpav**2 + 8.89159D-12*tcpav )

    ! Calculations dedicated for configurations with CP
    ! ***
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
        call cpost( r_cp_top-casthi, h_cp_top, r_tf_inboard_out-casthi, hmax+tfthko, & ! Inputs
                    ritfc, rhocp, fcoolcp, r_tf_inboard_in+thkcas, tinstf, casthi,   & ! Inputs
                    n_tf*turnstf,                                                    & ! Inputs
                    a_cp_cool, vol_cond_cp, prescp, vol_ins_cp, vol_case_cp )          ! Outputs


        ! Outer leg cross-section areas
        ! ---
        ! Area taken by one outboard leg's turns insulation [m2]
        a_wp_ins_turn = 2.0D0 * tinstf * ( (tftort/turnstf) + tfthko - 2.0D0*tinstf ) 

        ! Exact TF outboard leg conductor area (per leg) [m2]
        a_wp_cond_leg = ( 1.0D0 - fcoolleg ) * ( arealeg - a_wp_ins_turn * turnstf )  
        ! ---


        ! Outer leg resistive power loss
        ! ---
        ! TF outboard leg's resistance calculation (per leg) [ohm]
        tflegres = rhotfleg * tfleng / a_wp_cond_leg  

        ! TF outer leg resistive power (TOTAL) [W]   
        presleg = tflegres * ritfc**2 / n_tf 
        ! ---


        ! Sliding joints resistive heating
        ! ---
        if ( i_tf_sup /= 1 ) then

            ! Total number of contact area (4 joints section per legs)
            n_contact_tot = 4 * n_tf_joints_contact* n_tf_joints * nint(turnstf) * nint(n_tf)
            
            ! Total area of joint contact
            a_joints = tfthko * th_joint_contact * dble(n_contact_tot)

            ! joints resistive power losses
            pres_joints = rho_tf_joints * ritfc**2 / a_joints
        else 
            ! Joints resistance to be evaluated for SC
            pres_joints = 0.0D0
        end if
        ! ---


        ! Total volume of one outerleg [m3]
        voltfleg = tfleng * arealeg
            
        ! Outboard leg TF conductor volume [m3]
        vol_cond_leg = tfleng * a_wp_cond_leg 

        ! Total TF conductor volume [m3]
        vol_cond = vol_cond_cp + n_tf * vol_cond_leg

        ! Outboard leg TF insulation layer volume (per leg) [m3]
        vol_ins_leg = tfleng * a_wp_ins_turn * turnstf

        ! Total insulation layer volume [m3]
        vol_ins = vol_ins_cp + n_tf * vol_ins_leg

        ! Total volume of the CP casing [m3]
        vol_case_cp = vol_case_cp + pi*((r_tf_inboard_in + thkcas)**2 - r_tf_inboard_in**2 )

    ! Case of a continuous resistive magnet 
    ! ***
    else          
                        
        ! Conductor / insulation cross-section areas
        ! ---
        ! Rem SK : These quantities assume a square shape of the turn, this is a
        !          crude approximation if the inboard legs are vaulted
            
        ! Area taken by the inter turn ground insulation
        a_wp_ins_turn = 2.0D0 * tinstf * ( (tfcth/turnstf) + tfcth - 2.0D0*tinstf )

        ! Exact TF outboard leg conductor area
        a_wp_cond_leg = ( 1.0D0 - fcoolleg ) * ( arealeg - a_wp_ins_turn * turnstf ) 
        ! ---

        ! TF resistive powers
        prescp = rhocp * ritfc**2 * tfleng / ( a_wp_cond_leg * n_tf ) 
        presleg = 0.0D0

        ! Total TF outer leg conductor volume (not per leg)
        vol_cond = tfleng * a_wp_cond_leg * n_tf 

        ! Total insulation layer volume
        vol_ins = tfleng * a_wp_ins_turn * n_tf * turnstf
                        
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
        tinstf, thkwp

    implicit none

    ! Determine quench time (based on IDM: 2MBSE3)
    ! Resistive magnets : calculation of the resistive power losses added
    ! Issue #337: Force on the vessel wall due to TF coil quench

    ! Quench time [s]
    if ( i_tf_sup == 1 ) taucq = (bt * ritfc * rminor * rminor) / (r_vv_inboard_out * sigvvall)
    

    ! In plane forces 
    ! ---
    ! Centering force = net inwards radial force per meters per TF coil [N/m]
    cforce = 0.5D0 * bmaxtf*ritfc/n_tf 


    ! Vertical force per leg [N]
    ! ***
    ! Case of a centrepost (itart == 1) with sliding joints (the CP vertical are separated from the leg ones)
    ! Rem SK : casing/insulation thickness not subtracted as part of the CP is genuinely connected to the legs..
    if ( itart == 1 .and. i_tf_sup /= 1 ) then
        
        ! Tricky trick to avoid dividing by 0 if the TF has no hole in it
        if ( abs(r_wp_inner) < epsilon(r_wp_inner) ) r_wp_inner = 1.0D-9

        vforce = 0.25D0 * (bt * rmajor * ritfc) / (n_tf * thkwp**2) * (       & 
                      2.0D0 * r_wp_outer**2 * log(r_wp_outer / r_wp_inner ) + &
                      2.0D0 * thkwp**2 * log( r_cp_top     / r_wp_inner )   + &
                      3.0D0 * thkwp**2                                      - &
                      2.0D0 * thkwp * r_wp_outer                            + &
                      4.0D0 * thkwp * r_wp_outer *log( r_wp_inner / r_wp_outer ) )

        r_tf_outboard_in = r_tf_outboard_in + tinstf    ! Tricky trick t avoid writting tinstg all the time           
        vforce_outboard = 0.5D0 * (bt * rmajor * ritfc) / (n_tf * thkwp**2) * ( &
                      r_wp_outer**2       * log( r_wp_outer       / r_wp_inner                 ) + &
                      r_tf_outboard_in**2 * log( (r_tf_outboard_in + thkwp) / r_tf_outboard_in ) + &
                      thkwp**2         * log( (r_tf_outboard_in + thkwp) / r_wp_inner          ) - &
                      thkwp            * ( r_wp_outer + r_tf_outboard_in                       ) + &
                      2.0D0 * thkwp * ( r_wp_outer     * log(r_wp_inner / r_wp_outer)            + &
                                           r_tf_outboard_in * log((r_tf_outboard_in + thkwp)     / &
                                           r_tf_outboard_in))) - vforce 
        r_tf_outboard_in = r_tf_outboard_in - tinstf    ! Tricky trick to avoid writting tinstf all the time
        
        ! End of tricky trick
        if ( abs( r_wp_inner - 1.0D-9 ) < epsilon(r_wp_inner) ) r_wp_inner = 0.0D0

    ! Case of TF without joints or with clamped joints total
    ! Rem SK : f_vforce_inboard might be calculated analytically (see M. Kovari comment in #848)
    else 

        ! Inboard leg vertical force (per coil) [N]
        r_tf_outboard_in = r_tf_outboard_in + tinstf ! Tricky trick to avoid writting tinstg all the time    
        
        vforce = 0.5D0 * f_vforce_inboard * (bmaxtf * rbmax * ritfc) / (n_tf * thkwp**2) * ( &
                         r_wp_outer**2       * log( r_wp_outer                 / r_wp_inner       ) + &
                         r_tf_outboard_in**2 * log( (r_tf_outboard_in + thkwp) / r_tf_outboard_in ) + &
                         thkwp**2            * log( (r_tf_outboard_in + thkwp) / r_wp_inner       ) - &
                         thkwp * ( r_wp_outer + r_tf_outboard_in                                  ) + &
                         2.0D0 * thkwp * ( r_wp_outer       * log(r_wp_inner                 / r_wp_outer ) + &
                                           r_tf_outboard_in * log((r_tf_outboard_in + thkwp) / r_tf_outboard_in ) ))      
        
        r_tf_outboard_in = r_tf_outboard_in - tinstf  ! Tricky trick to avoid writting tinstf all the time

        vforce_outboard = vforce * ( ( 1.0D0 / f_vforce_inboard ) - 1.0D0 )  
    end if
    ! ***

end subroutine tf_field_and_force

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine tf_coil_area_and_masses()
    !! Subroutine to calculate the TF coil areas and masses
    use build_variables, only: hr1, r_tf_outboard_mid, tfcth, r_tf_inboard_mid
    use fwbs_variables, only: denstl
    use tfcoil_variables, only: whtconsh, whttf, whtcas, tficrn, tfcryoarea, &
        tfsao, whtgw, tfocrn, whtconsc, whtconcu, whtcon, whtconin, &
        tfsai, dcopper, vftf, whtconin, tfsai, dcond, dcondins, whtcon, &
        tfleng, dthet, dcase, acndttf, turnstf, n_tf, aiwp, radctf, acasetfo, &
        acasetf, fcutfsu, awphec, acstf, whttflgs, whtcp, whtconal, vol_cond_cp, &
        i_tf_sup, i_tf_sc_mat, dalu
    use constants, only: twopi
    use physics_variables, only: itart
    implicit none

    ! Local Variables
    real(dp) :: cplen, wbtf

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
        
        ! Mass of ground-wall insulation [kg]
        ! (assumed to be same density/material as conduit insulation)
        whtgw = tfleng * (awpc-awptf) * dcondins

        ! Masses of conductor constituents
        !---------------------------------    

        ! Superconductor mass [kg]
        ! Includes space allowance for central helium channel, area awphec
        whtconsc = (tfleng * turnstf * acstf*(1.0D0-vftf) * (1.0D0-fcutfsu) - tfleng*awphec) &
        *dcond(i_tf_sc_mat)

        ! Copper mass [kg]
        whtconcu = (tfleng * turnstf * acstf*(1.0D0-vftf) * fcutfsu - tfleng*awphec) * dcopper

        ! Steel conduit (sheath) mass [kg]
        whtconsh = tfleng * turnstf * acndttf * denstl

        ! Conduit insulation mass [kg]
        ! (aiwp already contains turnstf)
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
        
        ! Copper magnets casing/conductor weights per coil [kg]
        if ( i_tf_sup == 0 ) then 

            whtcas = denstl * vol_case_cp / n_tf  ! Per TF leg, no casing for outer leg
            whtconcu = dcopper * vol_cond / n_tf
            whtconal = 0.0D0         

            ! Outer legs/CP weights
            if ( itart == 1 ) then
                whttflgs = n_tf * ( dcopper * vol_cond_leg + dcondins * vol_ins_leg ) 
                whtcp =  dcopper * vol_cond_cp + dcondins * vol_ins_cp + vol_case_cp * denstl 
            end if

        ! Cryo-aluminium conductor weights
        ! Casing made of re-inforced aluminium alloy
        else  
            whtcas = dalu * vol_case_cp / n_tf
            whtconcu = 0.0D0            
            whtconal = dalu * vol_cond / n_tf

            ! Outer legs/CP weights
            if ( itart == 1 ) then
                whttflgs = n_tf * ( dalu * vol_cond_leg + dcondins * vol_ins_leg ) 
                whtcp =  dalu * vol_cond_cp + dcondins * vol_ins_cp + vol_case_cp * dalu 
            end if
        end if
            
        ! Ground wall insulation layer weight
        whtgw = dcondins * vol_ins / n_tf

        ! Total weight
        whttf = (whtcas + whtconcu + whtconal + whtgw) * n_tf

    end if 
    ! ---

end subroutine tf_coil_area_and_masses

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine peak_tf_with_ripple(n_tf,wwp1,thkwp,tfin,bmaxtf,bmaxtfrp,flag)

    !! Peak toroidal field on the conductor
    !! author: P J Knight, CCFE, Culham Science Centre
    !! tfno : input real : number of TF coils
    !! wwp1 : input real : width of plasma-facing face of winding pack (m)
    !! thkwp : input real : radial thickness of winding pack (m)
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

    real(dp), intent(in) :: n_tf,wwp1,thkwp,tfin,bmaxtf
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

    wmax = (2.0D0 * tfin + thkwp) * tan(pi/n_tf)

    !  Dimensionless winding pack width

    tf_fit_t = wwp1/wmax
    if ((tf_fit_t < 0.3D0).or.(tf_fit_t > 1.1D0)) then
        !write(*,*) 'PEAK_TF_WITH_RIPPLE: fitting problem; t = ',t
        flag = 1
    end if

    !  Dimensionless winding pack radial thickness

    tf_fit_z = thkwp/wmax
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
    use build_variables, only: tfcth, r_tf_inboard_mid, bore, ohcth, hmax
    use tfcoil_variables, only: eyzwp, casestr, windstrain, turnstf, &
        thkwp, i_tf_tresca, acstf, vforce, &
        ritfc, jwptf, strtf0, strtf1, strtf2, &
        thwcndut, insstrain, strtf2, vforce, tinstf, &
        acstf, jwptf, insstrain, layer_ins,&
        strtf1, rbmax, thicndut, acndttf, tfinsgap, &
        acasetf, alstrtf, poisson_steel, poisson_copper, poisson_al, &
        n_tf_graded_layers, i_tf_sup, i_tf_bucking, fcoolcp, eyoung_winding, &
        eyoung_steel, eyoung_nibron, eyoung_ins, eyoung_al, eyoung_copper, &
        aiwp, cpttf
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
     
    real(dp) :: strain_tf_z
    !! Vertical normal strain (constant as layer assumed to be bonded)
    
    real(dp), dimension(n_tf_layer+1) :: radtf
    !! Radii used to define the layers used in the stress models [m]
    !! Layers are labelled from inboard to outbard
    
    real(dp), dimension(n_tf_layer) :: eyoung
    !! Young modulae (one per layer) used in the stress models [Pa]
    
    real(dp), dimension(n_tf_layer) :: poisson
    !! Poisson's ratio (one per layer) used in the stress models
    
    real(dp), dimension(n_tf_layer) :: jeff
    !! Effective current density [A/m2]
  
    integer :: ii, jj
    !! do loop indexes

    integer :: ii_max
    !! Index of the maximum TRESCA stress

    real(dp) :: sig_max
    !! Working float to find maximum TRESCA stress index [Pa]

    real(dp) :: seff
    !! Turn dimenstion [m] 

    real(dp) :: tcbs
    !! Square cable dimension [m]
    
    real(dp) :: t_ins_eff
    !! Effective insulation thickness (turn + ground insulation per turn) [m]

    real(dp) :: fac
    !! TF steel conduit stress unsmearing factor

    real(dp) :: a_oh
    !! CS vertical cross-section area [m2]

    real(dp) :: curr_oh_max
    !! Maximum CS current (absolute value) [A]

    real(dp) :: n_oh_turns
    !! Number of CS turn (float ...)

    real(dp) :: a_oh_turn
    !! CS turn vertica cross section area [m]

    real(dp) :: l_cond_oh
    !! Central Solenoid (OH) conduit thickness assuming square conduit [m]
    !! Used only to get effective radial stress in bucked and wedged
    
    real(dp) :: l_turn_oh
    !! Central Solenoid (OH) turn dimension [m]

    real(dp) :: fac_oh
    !! Central Solenoid (OH) steel conduit stress unsmearing factor

    real(dp) :: sig_z_fac
    !! Vertical stress correction factor for inter turn structure

    real(dp) :: svmxz
    !! Von-mises stress in steel setting the radial stress to 0

    real(dp) :: svmyz
    !! Von-mises stress in stell setting the toroidal stress to 0

    real(dp) :: dr_wp_layer
    !! Size of WP layer with homogeneous smeared property 
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


    seff = sqrt(cpttf/jwptf)
    if (acstf >= 0.0D0) then
        tcbs = sqrt(acstf)
    else
        tcbs = 0.0D0
    end if



    ! LAYER ELASTIC PROPERTIES
    ! ------------------------
    ! CS properties (bucked and wedged)
    ! ---
    if ( i_tf_bucking >= 2 ) then

        ! Calculation performed at CS flux swing (no current on CS)
        jeff(1) = 0.0D0

        ! Inner radius of the CS
        radtf(1) = bore

        ! Superconducting CS
        if ( ipfres == 0 ) then

            ! Effective young modulus assuming the parallel case
            ! Rem the oh_steel_fraction is potentially a volumic one ... To be checked 
            eyoung(1) = oh_steel_frac * eyoung_steel + (1.0D0 - oh_steel_frac) * eyoung_winding
            poisson(1) = poisson_steel

        ! resistive CS (assumed to copper)
        else
            ! Here is a rough approximation
            eyoung(1) = eyoung_copper
            poisson(1) = poisson_copper
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
        eyoung(2) = 2.5D9
        poisson(2) = 0.34D0  ! Default value for young modulus
    end if 
    ! ---

    
    ! bucking cylinder/casing properties
    ! ---
    if ( i_tf_bucking >= 1 ) then
        
        ! No current in bucking cylinder/casing
        jeff(i_tf_bucking) = 0.0D0

        ! Steel bucking cylinder (copper and SC design)
        if ( i_tf_sup /= 2 ) then 
            eyoung(i_tf_bucking) = eyoung_steel
            poisson(i_tf_bucking) = poisson_steel
        
        ! Re-inforced aluminium 
        else 
            eyoung(i_tf_bucking) = eyoung_nibron
            poisson(i_tf_bucking) = poisson_al
        end if
        
        ! Innernost TF casing radius
        radtf(i_tf_bucking) = r_tf_inboard_in
    end if
    !---

 
    ! (Super)conductor layer properties
    ! ---
    ! Rem : They are only unique for isotropic materials, hence
    !       the underlying assumption of our models is the anisotropy
    !       of the material. A good assumption for resistive magnets
    !       but a doggy one for SC

    ! Thickness of a homognenous WP stress property layer
    dr_wp_layer = thkwp / dble(n_tf_graded_layers)

    ! WP effective insulation thickness (SC only)
    ! include groundwall insulation + insertion gap in thicndut
    ! inertion gap is tfinsgap on 4 sides
    if ( i_tf_sup == 1 ) t_ins_eff = thicndut + ((tfinsgap+tinstf)/turnstf)

    ! Loop on layers
    do ii = 1, n_tf_graded_layers
        
        ! Homogeneous current in (super)conductor
        jeff(i_tf_bucking + ii) = ritfc / (pi * (r_wp_outer**2 - r_wp_inner**2))

        ! Same thickness for all WP layers in stress calculation
        radtf(i_tf_bucking + ii) = r_wp_inner + dble(ii-1)*dr_wp_layer

        ! Copper magent
        if ( i_tf_sup == 0 ) then
            eyoung(i_tf_bucking + ii) = eyoung_copper
            poisson(i_tf_bucking + ii) = poisson_copper
            
        ! SC magnets smeared properties
        else if ( i_tf_sup == 1 ) then
            eyoung(i_tf_bucking  + ii) = eyngeff( eyoung_steel, eyoung_ins, &
                                                 t_ins_eff, thwcndut, tcbs )
            poisson(i_tf_bucking + ii) = poisson_steel
        
        ! Cryogenic aluminium properties
        else 
            eyoung(i_tf_bucking  + ii) = eyoung_al
            poisson(i_tf_bucking + ii) = poisson_al
        end if 
    end do

    ! last layer radius
    radtf(n_tf_layer + 1) = r_wp_outer
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


    ! Superconductor magnet stress/strain calculation
    ! ---
    if ( i_tf_sup == 1 ) then 

        ! Plane stress calculation (SC) [Pa]
        call plane_stress( poisson, radtf, eyoung, jeff, & ! Inputs
                           n_tf_layer, n_radial_array,   & ! Inputs
                           sig_tf_r, sig_tf_t, deflect, radial_array ) ! Outputs
    
        ! Vertical stress [Pa]  
        sig_tf_z = vforce / (acasetf + acndttf*turnstf) ! Array equation

        ! Case strain
        casestr = sig_tf_z(i_tf_bucking) / eyoung_steel

        ! Young's modulus in vertical direction on WP
        eyzwp = eyngzwp(eyoung_steel,eyoung_ins,eyoung_winding,t_ins_eff,thwcndut,tcbs)
    
        ! Strain in vertical direction on WP
        windstrain = sig_tf_z(i_tf_bucking+1) / eyzwp
    
        ! Radial strain in insulator
        insstrain = sig_tf_r(n_radial_array) / eyoung_ins * &
                    edoeeff(eyoung_steel, eyoung_ins, t_ins_eff, thwcndut, tcbs)
    end if 
    ! ---


    ! Resisitve magnet stress/strain calculations
    ! ---
    if ( i_tf_sup /= 1 ) then
        ! Generalized plane strain calculation [Pa]
        call generalized_plane_strain( poisson, radtf, eyoung, jeff, vforce,     & ! Inputs
                                       n_tf_layer, n_radial_array, i_tf_bucking, & ! Inputs
                                       radial_array, sig_tf_r, sig_tf_t, sig_tf_z,    & ! Outputs
                                       strain_tf_r, strain_tf_t, strain_tf_z, deflect ) ! Outputs
    end if
    ! ---

    ! Storing the smeared properties for output
    if ( iprint == 1 ) then
        sig_tf_smeared_r = sig_tf_r   ! Array equation
        sig_tf_smeared_t = sig_tf_t   ! Array equation
    end if
    ! ------------------------------



    ! STRESS DISTRIBUTIONS CORRECTIONS
    ! --------------------------------
    ! SC central solenoid coil stress unsmeating (bucked and wedged only)
    ! --- 
    if ( i_tf_bucking >= 2 .and. ipfres == 0 ) then

        ! Getting the turn dimention from scratch 
        ! as the TF is called before CS in caller.f90
        !-!

        ! CS area [m2]
        a_oh = 2.0D0 * hmax * ohhghf * ohcth

        ! Maximum current in Central Solenoid, at either BOP or EOF [MA-turns]
        ! Absolute value
        curr_oh_max = 1.0D-6*max(coheof,cohbop)*a_oh

        !  Number of turns
        n_oh_turns = 1.0D6 * curr_oh_max / cptdin(sum(ncls))

        ! CS Turn vertical cross-sectionnal area    
        a_oh_turn = a_oh / n_oh_turns

        ! Central Solenoid (OH) turn dimension [m]
        l_turn_oh = sqrt( a_oh_turn )

        ! OH/CS conduit thickness calculated assuming square conduit [m]  
        ! The insulation layer is set to 0 in the calculation
        ! Used only to get effective radial stress
        l_cond_oh = 0.5D0*(l_turn_oh - 2.0D0*layer_ins - &
                        sqrt( (2.0D0*layer_ins - l_turn_oh)**2 - &
                            oh_steel_frac * l_turn_oh**2) )
        !-!

        ! Central Solenoid (OH) steel conduit stress unsmearing factor
        fac_oh = 0.5D0 * l_turn_oh / l_cond_oh 

        do ii = 1, n_radial_array

            ! CS (OH) superconducting case stress unsmearing
            sig_tf_r(ii) = sig_tf_r(ii) * fac_oh
            sig_tf_t(ii) = sig_tf_t(ii) / oh_steel_frac
        end do
    end if
    ! ---


    ! No TF vertical forces on CS and CS-TF layer (bucked and wedged only)
    ! ---
    if ( i_tf_bucking >= 2 ) then
        do ii = 1, (i_tf_bucking-1)*n_radial_array
            sig_tf_z(ii) = 0.0D0
        end do
    end if
    ! ---


    ! SC Toroidal coil unsmearing
    ! ---
    if ( i_tf_sup == 1 ) then

        ! WP conduit stress unsmearing factor
        fac = eyoung_steel*eyoung_ins*seff / &
              (eyoung_ins*(seff-2.0D0*t_ins_eff) + 2.0D0*t_ins_eff*eyoung_steel)


        ! GRADED MODIF : add another do loop to allow the graded properties
        !                to be taken into account
        do ii = i_tf_bucking * n_radial_array + 1, n_tf_layer*n_radial_array  
            ! GRADED MODIF: eyoung of the given layer to be used 
            sig_tf_r(ii) = sig_tf_r(ii)/eyoung(i_tf_bucking+1) * fac
            sig_tf_t(ii) = sig_tf_t(ii)/eyoung(i_tf_bucking+1) * fac
        end do

    end if
    ! ---


    ! Resisve TF coil vertical forces correction (for cooling pipes) 
    ! ---
    if ( i_tf_sup /= 1 ) then

        ! Copper coil correction factor 
        if ( i_tf_sup == 0 ) then
            sig_z_fac = 1.0D0 / ( 1 - fcoolcp ) 
        
        ! Cryo-aluminium correction factor
        else if (i_tf_sup == 2 ) then
            sig_z_fac = eyoung_al / ( awptf * eyoung_al + aiwp * eyoung_ins ) &
                                  * ( awpc / turnstf ) 
        end if
        
        ! Applying vertical stress correction factor
        do ii = i_tf_bucking*n_radial_array + 1, n_tf_layer*n_radial_array
            sig_tf_z(ii) = sig_z_fac * sig_tf_z(ii)
        end do
    end if
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
        do ii = i_tf_bucking * n_radial_array + 1, n_tf_layer*n_radial_array
       
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
    strtf2 = sig_tf_tresca_max(i_tf_bucking + 1) ! Maximum assumed in the first graded layer
    if ( i_tf_bucking >= 1 ) strtf1 = sig_tf_tresca_max(i_tf_bucking)
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
        
        if ( i_tf_sup == 1 ) then
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
        write(outfile,* ) ''

        ! MFILE.DAT data
        do ii = 1, i_tf_bucking + 1
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
        write(sig_file,'(t2, "Von-Mises" ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_vmises*1.0D-6
        write(sig_file,'(t2, "TRESCA"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_tresca*1.0D-6
        if ( i_tf_sup == 1 ) then
            write(sig_file,'(t2, "CEA TRESCA"," stress", t20, "(MPa)",t26, *(F11.3,3x))') s_tresca_cond_cea*1.0D-6
        else 
            write(sig_file,'(t2, "TRESCA"    ," stress", t20, "(MPa)",t26, *(F11.3,3x))') sig_tf_tresca*1.0D-6
        end if 
        write(sig_file,*) 
        write(sig_file,*) 'Displacement'         
        write(sig_file,'(t2, "raidal displacement", t20, "(mm)",t26, *(F11.3,3x))') deflect*1.0D3
        if ( i_tf_sup /= 1 ) then
            write(sig_file,*)
            write(sig_file,*) 'Strain'    
            write(sig_file,'(t2, "radial strain"   ,t26, *(F11.8,3x))') strain_tf_r
            write(sig_file,'(t2, "toroidal strain" ,t26, *(F11.8,3x))') strain_tf_t
            write(sig_file,'(t2, "vertical strain" ,t26, *(F11.8,3x))') strain_tf_z
        end if

             ! Other quantities (displacement strain, etc..)
        call ovarre(outfile,'Maximum radial deflection at midplane (m)','(deflect)',&
                            deflect(n_radial_array), 'OP ')
        call ovarre(outfile,"Winding pack vertical Young's Modulus (Pa)",'(eyzwp)', eyzwp, 'OP ')
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

subroutine generalized_plane_strain( nu, rad, ey, d_curr, v_force,          & ! Inputs
                                     nlayers, n_radial_array, i_tf_bucking, & ! Inputs
                                     rradius, sigr, sigt, sigz,              & ! Outputs
                                     strain_r, strain_t, strain_z, r_deflect ) ! Outputs
      
    !! This subroutine numerically find the constant (2 per layer) of the
    !! analytical resolution of the mid-plane stress calculations using the
    !! generalized plain strain formulation, from the radial stress and 
    !! displacement boundary conditions. This conditions sets 2*nlayer
    !! linear equation of the integrals constants cc, find with using matrix inversion 
    !! S. Kahn, Jan 2020
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

    real(dp), dimension(nlayers), intent(in) :: nu
    !! Poisson's ratios

    real(dp), dimension(nlayers), intent(in) :: ey
    !! Young modulae
        
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
          
    real(dp), intent(out) :: strain_z
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
      
    ! Strain to stress hooke's law coeficient
    real(dp), dimension(nlayers) :: kk

    ! Layer area
    real(dp), dimension(nlayers) :: area

    ! Vertical strain parameters
    real(dp) :: aleph
    real(dp) :: sum_1 
    real(dp) :: sum_2 
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
    ! Stress to strain coeficients (array equation)
    kk = ey / ( 1.0D0 - nu - nu**2 )

    ! Lorentz forces parametrisation coeficients (array equation)
    alpha = 0.5D0*rmu0 * d_curr**2 / (kk * ( 1.0D0 - nu ))

    inner_layer_curr = 0.0D0
    do ii = 1, nlayers

        beta(ii) = 0.5D0*rmu0 * d_curr(ii) * ( inner_layer_curr - pi*d_curr(ii)*rad(ii)**2 ) / &
                                             ( pi*kk(ii)*(1.0D0 - nu(ii)) )
         
        ! Layer area
        area(ii) = pi * (rad(ii+1)**2 - rad(ii)**2)

        ! Total current carried by the inners layers 
        inner_layer_curr = inner_layer_curr + area(ii)*d_curr(ii)
    end do
      
      
    ! Plain strain generalisation parameters
    !-!
    do ii = 1, nlayers
        par_1(ii) = pi * (rad(ii+1)**4 - rad(ii)**4)
        par_2(ii) = pi * (log(rad(ii+1)) * rad(ii+1)**2 - log(rad(ii)) * rad(ii)**2)
    end do
 
    ! The first layer (CS) is not considered on the bucked and wedged option
    if ( i_tf_bucking == 2 ) then

        sum_1 = 0.0D0
        sum_2 = 0.0D0
        do jj = 2, nlayers
            sum_1 = sum_1 + kk(jj) * area(jj) * (1.0D0 - nu(jj))
            sum_2 = sum_2 + nu(jj) * kk(jj) * (0.25D0 * alpha(jj) * par_1(jj) + &
                                               beta(jj)*par_2(jj))
        end do        
    
    ! Free standing TF, the vertical force is distributed to all layers
    else 
        sum_1 = sum( kk * area * (1.0D0 - nu) )
        sum_2 = sum( nu * kk * (0.25D0*alpha*par_1 + beta*par_2) )
    end if

    aleph = (v_force - sum_2) / sum_1      
    beth = - (2.0D0*nu*kk*area) / sum_1   ! Vector equation

    ! Bucked and wegde
    ! CS (first layer) - TF (all remaining layer) sliding 
    !  -> No TF vertical force on the CS layer (null coef)
    if ( i_tf_bucking == 2 ) beth(1) = 0.0D0
    !-!
    ! ***
      

    ! Left hand side matrix aa
    ! ***
    aa(:,:) = 0.0D0

    ! Null radial stress at R(1)
    aa(1,1) = kk(1)
    aa(1,2) = kk(1) * (2.0D0*nu(1) - 1.0D0) / rad(1)**2 

    do jj = 1, nlayers ! Plain strain generalisation on C1 coeficients
        aa(1, 2*jj-1) = aa(1, 2*jj-1) + beth(jj)*kk(1)*nu(1)
    end do

    ! Inter-layer boundary conditions
    if ( nlayers /= 1 ) then 
        do ii = 1, nlayers - 1

            ! Continuous radial normal stress at R(ii+1)
            aa(2*ii, 2*ii-1) = kk(ii)
            aa(2*ii, 2*ii  ) = kk(ii) * ( 2.0D0*nu(ii) - 1.0D0 ) / rad(ii+1)**2 
            aa(2*ii, 2*ii+1) = -kk(ii+1)
            aa(2*ii, 2*ii+2) = -kk(ii+1) * ( 2.0D0*nu(ii+1) - 1.0D0 ) / rad(ii+1)**2

            do jj = 1, nlayers ! Plain strain generalisation
                aa(2*ii, 2*jj-1) = aa(2*ii, 2*jj-1) + beth(jj)*(kk(ii)*nu(ii) - kk(ii+1)*nu(ii+1)) 
            end do

            ! Continuous displacement at R(ii+1)
            aa(2*ii+1, 2*ii-1) = rad(ii+1)
            aa(2*ii+1, 2*ii  ) = 1.0D0 / rad(ii+1)
            aa(2*ii+1, 2*ii+1) = -rad(ii+1)
            aa(2*ii+1, 2*ii+2) = -1.0D0 / rad(ii+1)

        end do
    end if

    ! Null radial stress at outermost radius at R(nlayers+1)
    aa(2*nlayers, 2*nlayers - 1) = kk(nlayers)
    aa(2*nlayers, 2*nlayers    ) = kk(nlayers) * (2.0D0*nu(nlayers) - 1.0D0) / rad(nlayers+1)**2

    do jj = 1, nlayers ! Plain strain generalisation
        aa(2*nlayers, 2*jj-1) = aa(2*nlayers, 2*jj-1) + beth(jj)*kk(nlayers)*nu(nlayers)
    end do
    ! ***

    ! Right hand side vector bb
    ! ***
    ! Null radial stress at R(1)
    bb(1) = -kk(1) * ( 0.125D0*alpha(1) * rad(1)**2 * ( 3.0D0 - 2.0D0*nu(1) ) &
                      + 0.5D0*beta(1) * ( 1.0D0 - nu(1)   + log(rad(1)) )     &
                      + nu(1)*aleph ) ! Plain strain generalisation

    ! Inter-layer boundary conditions
    if ( nlayers /= 1 ) then 
        do ii = 1, nlayers - 1

            ! Continuous radial normal stress at R(ii+1)
            bb(2*ii) = - kk(ii) * ( 0.125D0*alpha(ii) * rad(ii+1)**2 * ( 3.0D0 - 2.0D0*nu(ii) )       &
                                   + 0.5D0*beta(ii) * ( 1.0D0 - nu(ii)   + log(rad(ii+1)) ) )         &
                       + kk(ii+1) * ( 0.125D0*alpha(ii+1) * rad(ii+1)**2 * ( 3.0D0 - 2.0D0*nu(ii+1) ) &
                                     + 0.5D0*beta(ii+1) * ( 1.0D0 - nu(ii+1) + log(rad(ii+1)) ) )     &
                       - aleph * ( kk(ii)*nu(ii) - kk(ii+1)*nu(ii+1) ) ! Plain strain generalisation line

            ! Continuous displacement at R(ii+1)
            bb(2*ii+1) = - 0.125D0*alpha(ii)  * rad(ii+1)**3 - 0.5D0*beta(ii)  *rad(ii+1)*log(rad(ii+1))  &
                         + 0.125D0*alpha(ii+1)* rad(ii+1)**3 + 0.5D0*beta(ii+1)*rad(ii+1)*log(rad(ii+1))

        end do
    end if

    ! Null radial stress at R(nlayers+1)
    bb(2*nlayers) = -kk(nlayers) * ( 0.125D0*alpha(nlayers)*rad(nlayers+1)**2 * (3.0D0 - 2.0D0*nu(nlayers)) &
                                    + 0.5D0*beta(nlayers) * (1.0D0 - nu(nlayers) + log(rad(nlayers+1)))      & 
                                    + nu(nlayers)*aleph )   ! Plain strain generalisation
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
    r_deflect(:) = 0.0D0

    ! Vertical normal strain
    strain_z = aleph
    do ii = 1, nlayers
        strain_z = strain_z + beth(ii)*c1(ii)
    end do 

    do ii = 1, nlayers
         
        dradius = (rad(ii+1) - rad(ii)) / dble(n_radial_array)
        do jj = (ii-1)*n_radial_array + 1, ii*n_radial_array

            rradius(jj) = rad(ii) + dradius*dble(jj - n_radial_array*(ii-1) - 1)

            sigr(jj) = kk(ii) * ( c1(ii) + (2.0D0*nu(ii) - 1.0D0)*c2(ii)/rradius(jj)**2 + &
                                  0.125D0*alpha(ii)*( 3.0D0 - 2.0D0*nu(ii) )*rradius(jj)**2 + &
                                  0.5D0*beta(ii)*(1.0D0 - nu(ii) + log(rradius(jj)) ) + &
                                  nu(ii)*strain_z )

            sigt(jj) = kk(ii) * ( c1(ii) - (2.0D0*nu(ii) - 1.0D0)*c2(ii)/rradius(jj)**2 + &
                                  0.125D0*alpha(ii)*( 1.0D0 + 2.0D0*nu(ii) )*rradius(jj)**2 + &
                                  0.5D0*beta(ii)*(nu(ii) + log(rradius(jj))) + &
                                  nu(ii)*strain_z )
                                     
            sigz(jj) = kk(ii) * ( nu(ii) * ( 2.0D0*c1(ii) + &
                                  0.5D0*alpha(ii) * rradius(jj)**2 + &
                                  0.5D0*beta(ii) * (1.0D0 + 2.0D0*log(rradius(jj))) ) + &
                                  (1.0D0 - nu(ii)) * strain_z )

            ! Radisal strain
            strain_r(jj) = c1(ii) - c2(ii) / rradius(jj)**2 + &
                           0.375D0*alpha(ii) * rradius(jj)**2 + 0.5D0*beta(ii) * (1 + log(rradius(jj)))
      
            ! Toroidal strain
            strain_t(jj) = c1(ii) + c2(ii) / rradius(jj)**2 + &
                           0.125D0*alpha(ii) * rradius(jj)**2 + 0.5D0*beta(ii) * log(rradius(jj))
                              
            ! Radial displacement
            r_deflect(jj) = c1(ii)*rradius(jj) + c2(ii)/rradius(jj)   &
                            + 0.125D0*alpha(ii) * rradius(jj)**3     &
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
        r_tf_inboard_mid, tfthko, r_cp_top
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
        circumference = pi* ( aaa + bbb ) * ( 1.0D0 + (3.0D0*hh)/(10.0D0 + sqrt(4.0D0 - 3.0D0*hh)) )  ! eq(14)

        ! Initial formula slightly less accurate
        ! circumference = pi * ( 3.0D0*(aaa+bbb) - sqrt( (3.0D0*aaa + bbb)*(aaa + 3.0D0*bbb) ) ) 
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
        tfcth, tfthko, r_cp_top
    use process_output, only: int2char, ovarre, ocmmnt, oblnkl, ovarin, osubhd, &
        ovarrf, obuild
    use numerics, only: icc
    use tfcoil_variables, only: wwp1, whttf, yarc, xarc, &
        windstrain, wwp2, whtconsh, tftort, whtconcu, ritfc, &
        conductor_width, tfinsgap, deflect, vtfskv, tmaxpro, fcutfsu, &
        tinstf, turnstf, cforce, i_tf_turns_integer, tdmptf, &
        leno, oacdcp, estotftgj, n_tf, whtconin, jwptf, tfa, &
        tficrn, n_layer, tfleng, thwcndut, casthi, sigvvall, &
        thkcas, casths, vforce, n_pancake, aswp, aiwp, tfareain, acasetf, &
        vftf, eyzwp, thicndut, dhecoil, insstrain, taucq, ripmax, &
        whtconsc, alstrtf, bmaxtfrp, vdalw, leni, thkwp, whtcas, whtcon, &
        ripple, i_tf_tresca, bmaxtf, awphec, avwp, aiwp, acond, acndttf, &
        i_tf_sc_mat, voltfleg, vol_cond_cp, tflegres, tcpav, prescp, i_tf_sup, &
        cpttf, cdtfleg, whttflgs, whtcp, i_tf_bucking, tlegav, rhotfleg, rhocp, &
        presleg, i_tf_shape, fcoolcp, pres_joints, tmargtf, tmargmin_tf
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
            call ocmmnt(outfile,'  -> Resitive coil : Water cooled copper (GLIDCOP)')
        case (1)
            call ocmmnt(outfile,'  -> Superconducting coil (SC)')
        case (2)
            call ocmmnt(outfile,'  -> Reisitive coil : Helium cooled cryogenic aluminium')
    end select

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
        end select
    end if

    call ovarin(outfile,'Presence of TF demountable joints','(itart)',itart)
    if ( itart == 1 ) then
        call ocmmnt(outfile,'  -> TF coil made of a Centerpost (CP) and outer legs')
        call ocmmnt(outfile,'     interfaced with demountable joints')
    else 
        call ocmmnt(outfile,'  -> Coils without demountable joints')
    end if

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
    call ovarre(outfile,'Number of TF coils','(n_tf)',n_tf)
    call ovarre(outfile,'Inboard leg centre radius (m)','(r_tf_inboard_mid)',r_tf_inboard_mid, 'OP ')
    call ovarre(outfile,'Outboard leg centre radius (m)','(r_tf_outboard_mid)',r_tf_outboard_mid, 'OP ')
    call ovarre(outfile,'Total inboard leg radial thickness (m)','(tfcth)',tfcth)
    call ovarre(outfile,'Total outboard leg radial thickness (m)','(tfthko)',tfthko)
    call ovarre(outfile,'Outboard leg toroidal thickness (m)','(tftort)',tftort, 'OP ')
    call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax, 'OP ')
    call ovarre(outfile,'Mean coil circumference (m)','(tfleng)',tfleng, 'OP ')
    
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

    if ( itart == 1 ) then
        call osubhd(outfile,'Tapered Centrepost Dimensions:')
        call ovarre(outfile,'Radius of the centrepost at the midplane (m)','(r_tf_inboard_out)',r_tf_inboard_out)
        call ovarre(outfile,'Radius of the ends of the centrepost (m)','(r_cp_top)',r_cp_top)
        call ovarre(outfile,'Distance from the midplane to the top of the tapered section (m)','(h_cp_top)',h_cp_top)
        call ovarre(outfile,'Distance from the midplane to the top of the centrepost (m)','(hmax)',hmax + tfthko)
    end if

    ! Turn/WP gemoetry
    if ( i_tf_sup == 1 ) then


        call osubhd(outfile,'Global material area/fractions:')
        call ovarre(outfile,'TF cross-section (total) (m2)','(tfareain)', tfareain)
        call ovarre(outfile,'Steel cross-section (total) (m2)','((acasetf+aswp)*n_tf)',(acasetf+aswp)*n_tf)
        call ovarre(outfile,'Steel TF fraction','(f_tf_steel)',(acasetf+aswp)/(tfareain/n_tf))
        call ovarre(outfile,'Steel WP cross-section (total) (m2)','(aswp*n_tf)',aswp*n_tf)
        call ovarre(outfile,'Steel WP fraction','(aswp/awpc)',aswp/awpc)
        call ovarre(outfile,'Insulation WP fraction','(aiwp/awpc)',aiwp/awpc)
        call ovarre(outfile,'Cable WP fraction','((awpc-aswp-aiwp)/awpc)',(awpc-aswp-aiwp)/awpc)
        
        ! External casing
        call osubhd(outfile,'External steel Case Information :')
        call ovarre(outfile,'Casing cross section area (total) (m2)','(acasetf*n_tf))',acasetf*n_tf)
        call ovarre(outfile,'Inboard leg case plasma side wall thickness (m)','(casthi)',casthi)
        call ovarre(outfile,'Inboard leg case inboard "nose" thickness (m)','(thkcas)',thkcas)
        call ovarre(outfile,'Inboard leg case sidewall thickness at its narrowest point (m)','(casths)',casths)
        call ovarre(outfile,'External case mass per coil (kg)','(whtcas)',whtcas, 'OP ')

        ! Winding pack structure
        call osubhd(outfile,'TF winding pack (WP) geometry:')
        call ovarre(outfile,'WP cross section area with insulation and insertion (total) (m2)','(awpc*n_tf))',awpc*n_tf)
        call ovarre(outfile,'WP cross section area (total) (m2)','(aswp*n_tf))',awptf*n_tf)
        call ovarre(outfile,'Winding pack radial thickness (m)','(thkwp)',thkwp, 'OP ')
        if (  i_tf_turns_integer == 1 ) then
            call ovarre(outfile, 'Winding pack toroidal width (m)', '(wwp1)', wwp1, 'OP ')
        else
            call ovarre(outfile,'Winding pack toroidal width 1 (m)','(wwp1)',wwp1, 'OP ')
            call ovarre(outfile,'Winding pack toroidal width 2 (m)','(wwp2)',wwp2, 'OP ')
        end if
        call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)',tinstf)
        call ovarre(outfile,'Winding pack insertion gap (m)','(tfinsgap)',tfinsgap)
       
        ! Number of turns
        call osubhd(outfile,'WP turn information:')    
        call ovarin(outfile,'Turn parametrisation', '(i_tf_turns_integer)', i_tf_turns_integer)
        if ( i_tf_turns_integer == 0 ) then 
            call ocmmnt(outfile,'  Non-integer number of turns')
        else 
            call ocmmnt(outfile,'  Integer number of turns')
        end if
        call ovarre(outfile,'Number of turns per TF coil','(turnstf)',turnstf, 'OP ')
        if ( i_tf_turns_integer == 1 ) then
            call ovarin(outfile, 'Number of TF pancakes', '(n_pancake)', n_pancake)
            call ovarin(outfile, 'Number of TF layers', '(n_layer)', n_layer)
        end if    
        call oblnkl(outfile)

        if ( i_tf_turns_integer == 1 ) then
            call ovarre(outfile, 'Radial width of turn (m)', '(t_turn_radial)', t_turn_radial)
            call ovarre(outfile, 'Toroidal width of turn (m)', '(t_turn_toroidal)', t_turn_toroidal)
            call ovarre(outfile, 'Radial width of cable space', '(t_cable_radial)', t_cable_radial)
            call ovarre(outfile, 'Toroidal width of cable space', '(t_cable_toroidal)', t_cable_toroidal)
            call ovarre(outfile, 'Radial width of conductor (m)', '(t_conductor_radial)', t_conductor_radial, 'OP ')
            call ovarre(outfile, 'Toroidal width of conductor (m)', '(t_conductor_toroidal)', t_conductor_toroidal, 'OP ')
        else
            call ovarre(outfile,'Width of conductor (square) (m)','(conductor_width)',conductor_width, 'OP ')
            call ovarre(outfile,'Width of turn including inter-turn insulation (m)','(leno)',leno, 'OP ')
            call ovarre(outfile,'Width of space inside conductor (m)','(leni)',leni, 'OP ')
        end if
        call ovarre(outfile,'Steel conduit thickness (m)','(thwcndut)',thwcndut)
        call ovarre(outfile,'Inter-turn insulation thickness (m)','(thicndut)',thicndut)

        select case (i_tf_sc_mat)
        case (1,2,3,4,5)
            call osubhd(outfile,'Conductor information:')    
            call ovarre(outfile,'Diameter of central helium channel in cable','(dhecoil)',dhecoil)
            call ocmmnt(outfile,'Fractions by area')
            call ovarre(outfile,'Coolant fraction in conductor excluding central channel','(vftf)',vftf)
            call ovarre(outfile,'Copper fraction of conductor','(fcutfsu)',fcutfsu)
            call ovarre(outfile,'Superconductor fraction of conductor','(1-fcutfsu)',1-fcutfsu)
            ! TODO
            !call ovarre(outfile,'Conductor fraction of winding pack','(acond/ap)',acond/ap, 'OP ')
            !call ovarre(outfile,'Conduit fraction of winding pack','(turnstf*acndttf/ap)',turnstf*acndttf/ap, 'OP ')
            !call ovarre(outfile,'Insulator fraction of winding pack','(aiwp/ap)',aiwp/ap, 'OP ')
            !call ovarre(outfile,'Helium area fraction of winding pack excluding central channel','(avwp/ap)',avwp/ap, 'OP ')
            !call ovarre(outfile,'Central helium channel area as fraction of winding pack','(awphec/ap)',awphec/ap, 'OP ')
            ap = acond + turnstf*acndttf + aiwp + avwp + awphec
            call ovarrf(outfile,'Check total area fractions in winding pack = 1','', &
            (acond + turnstf*acndttf + aiwp + avwp + awphec)/ap)
            call ovarrf(outfile,'minimum TF conductor temperature margin  (K)','(tmargmin_tf)',tmargmin_tf)
            call ovarrf(outfile,'TF conductor temperature margin (K)','(tmargtf)',tmargtf)

        end select
    else

        ! External casing
        call osubhd(outfile,'Bucking cylinder information:')
        call ovarre(outfile,'Inboard leg case plasma side wall thickness (m)','(casthi)',casthi)
        call ovarre(outfile,'Inboard leg bucking cylinder thickness (m)','(thkcas)',thkcas)

        call osubhd(outfile,'Coil turn information:')
        call ovarre(outfile,'Number of turns per TF leg','(turnstf)',turnstf)
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
    call ovarre(outfile,'Inboard vertical separating force per leg (N)','(vforce)',vforce, 'OP ')
    call ovarre(outfile,'Centring force per coil (N/m)','(cforce)',cforce, 'OP ')

    ! Resistive coil parameters
    if ( i_tf_sup /= 1 ) then 
        call osubhd(outfile,'Resitive loss parameters:')
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
    if ( i_tf_sup == 1 ) then

        call osubhd(outfile,'Radial build of TF coil centre-line :')
        write(outfile,5)
        5   format(t43,'Thickness (m)',t60,'Outer radius (m)')
        radius = r_tf_inboard_mid - 0.5D0*tfcth
        call obuild(outfile,'Innermost edge of TF coil',radius,radius)
        radius = radius + thkcas
        call obuild(outfile,'Coil case ("nose")',thkcas,radius,'(thkcas)')
        radius = radius + tfinsgap
        call obuild(outfile,'Insertion gap for winding pack',tfinsgap,radius,'(tfinsgap)')
        radius = radius + tinstf
        call obuild(outfile,'Winding pack insulation',tinstf,radius,'(tinstf)')
        radius = radius + thkwp/2d0
        call obuild(outfile,'Winding - first half',thkwp/2d0,radius,'(thkwp/2 - tinstf)')
        radius = radius + thkwp/2d0
        call obuild(outfile,'Winding - second half',thkwp/2d0,radius,'(thkwp/2 - tinstf)')
        radius = radius + tinstf
        call obuild(outfile,'Winding pack insulation',tinstf,radius,'(tinstf)')
        radius = radius + tfinsgap
        call obuild(outfile,'Insertion gap for winding pack',tfinsgap,radius,'(tfinsgap)')
        radius = radius + casthi
        call obuild(outfile,'Coil case (plasma side)',casthi,radius,'(casthi)')
      
        if(abs((radius - r_tf_inboard_mid - 0.5D0*tfcth)) < 1d-6)then
            call ocmmnt(outfile,'TF coil dimensions are consistent')
        else
            call ocmmnt(outfile,'ERROR: TF coil dimensions are NOT consistent:')
            call ovarre(outfile,'Radius of plasma-facing side of inner leg SHOULD BE [m]','',r_tf_inboard_mid + 0.5D0*tfcth)
            call ovarre(outfile,'Inboard TF coil radial thickness [m]','(tfcth)',tfcth)
            !thkwp = tfcth - casthi - thkcas - 2.0D0*tinstf - 2.0d0*tfinsgap
            call oblnkl(outfile)
        end if

    else

        call osubhd(outfile,'Energy and Forces :')
        call oblnkl(outfile)
        call ocmmnt(outfile,'TF coil inner surface shape is given by a rectangle with the')
        call ocmmnt(outfile,'following inner points (Note that this does not account')
        call ocmmnt(outfile,'for the ST tapered centrepost):')
        call oblnkl(outfile)
 
        write(outfile,10)
        ! 10  format(t2,'point',t16,'x(m)',t31,'y(m)')
        do ii = 1,5
           write(outfile,20) ii,xarc(ii),yarc(ii)
           intstring = int2char(ii)
           call ovarre(mfile,'TF coil arc point '//intstring//' R (m)', '(xarc('//intstring//'))',xarc(ii))
           call ovarre(mfile,'TF coil arc point '//intstring//' Z (m)', '(yarc('//intstring//'))',yarc(ii))
        end do
        ! 20  format(i4,t10,f10.3,t25,f10.3)

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
    use tfcoil_variables, only: tmargmin_tf, turnstf, n_tf, vftf, &
        temp_margin, jwdgpro, tftmp, vtfskv, acndttf, dhecoil, tmaxpro, &
        tmargtf, thwcndut, conductor_width, fcutfsu, jwdgcrt, tdmptf, cpttf, &
        ritfc, jwptf, bmaxtfrp, tcritsc, acstf, strncon_tf, fhts, bcritsc, &
        i_tf_sc_mat, b_crit_upper_nbti
    use superconductors, only: wstsc, current_sharing_rebco, itersc, jcrit_rebco, jcrit_nbti, croco, bi2212,&
    GL_nbti
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
    aturn = ritfc/(jwptf*n_tf*turnstf)    

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
            tc0m = 9.04D0
            call GL_nbti(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
            jcritstr = jcritsc  * (1.0D0-fcu)
            !  Critical current in cable
            icrit = jcritstr * acs * fcond

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

        !  Temperature margin (already calculated in bi2212 for isumat=2)
        if ((isumat == 1).or.(isumat == 4).or.(isumat == 3).or.(isumat == 5)) then

            !  Newton-Raphson method; start approx at requested minimum temperature margin
            ttest = thelium + 0.00001!+ tmargmin_tf + 0.001d0
            delt = 0.000001D0
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
        croco_od = conductor_width / 3.0d0 - thwcndut * ( 2.0d0 / 3.0d0 )
        conductor%acs =  9.d0/4.d0 * pi * croco_od**2
        acstf = conductor%acs
        conductor%area =  conductor_width**2 ! does this not assume it's a sqaure???

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
        call ovarre(outfile,'Width of square conductor (m)','(conductor_width)', conductor_width , 'OP ')
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
    use tfcoil_variables, only: leno, tmax_croco, bmaxtf, quench_detection_ef, &
        tftmp, croco_quench_temperature, jwptf, conductor_width
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
        current_density_in_conductor = jwptf *  (leno / conductor_width)**2
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
                     ins_th, cas_out_th, n_turns_tot,                         &  ! Inputs
                     acpcool, volume, respow, volins, volcasout )                ! Outputs
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
    !!  F/MI/PJK/LOGBOOK12, pp.33,34
    !!  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use error_handling, only: fdiags, report_error
    use constants, only: pi
    use tfcoil_variables, only: n_tf
    use build_variables, only: hmax
    implicit none

    !  Arguments
    ! ---
    ! Inputs
    real(dp), intent(in) :: rtop,ztop,rmid,hmaxi,curr,rho,fcool,&
                          r_tfin_inleg, ins_th, cas_out_th, n_turns_tot

    ! Outputs
    real(dp), intent(out) :: volume, respow, acpcool, volins, volcasout

    ! Internal
    real(dp) :: r1,z1,x,y,rc,dz,r,z, a_tfin_hole, res_cyl, res_taped
    real(dp) :: sum1, sum2, sum3, sum4
    real(dp) :: a_cond_midplane, a_cp_ins, a_casout
    real(dp), dimension(0:100) :: yy, yy_ins, yy_casout

    integer :: ii
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
    a_tfin_hole = pi*r_tfin_inleg**2  ! eq(32)

    ! Mid-plane outer casing cross-section area [m2]
    a_casout = pi * ( (rmid + cas_out_th)**2 - rmid**2 )

    ! Mid-plane insualtion layer cross-section [m2] 
    ! eq(27)
    a_cp_ins = pi * ( (r_tfin_inleg + ins_th)**2 - r_tfin_inleg**2 ) + & ! Inner layer volume
               pi * ( rmid**2 - ( rmid - ins_th )**2 )               + & ! Outter layer volume
               2.0D0 * ins_th * (rmid - r_tfin_inleg - 2.0D0*ins_th) * n_turns_tot ! inter turn separtion layers

    ! Cooling pipes cross-section [m2]
    acpcool = fcool * ( pi*rmid**2 - a_tfin_hole - a_cp_ins ) / n_tf
    ! ---------------------------


    !  Trivial solutions
    ! ------------------
    if ( abs(fcool) < epsilon(fcool) ) then
        volume = 0.0D0
        respow = 0.0D0
        call report_error(122)
        return
    end if

    if ( abs(rmid - rtop) < epsilon(rtop) ) then

        ! Exact conductor cross-section
        a_cond_midplane = pi*rmid**2 - a_tfin_hole - n_tf * acpcool - a_cp_ins

        ! Volumes and resisitive losses calculations
        volume = 2.0D0 * hmaxi * a_cond_midplane
        volins = 2.0D0 * hmaxi * a_cp_ins
        respow = 2.0D0 * hmaxi * curr**2 * rho / a_cond_midplane
        volcasout = 2.0D0 * hmaxi * a_casout

        return
    end if
    ! ------------------


    !  Find centre of circle (RC,0) defining the taper's arc
    !  (r1,z1) is midpoint of line joining (rmid,0) and (rtop,ztop)
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

        ! Insulation cross-sectional area at Z
        yy_ins(ii) = pi * ( (r_tfin_inleg + ins_th)**2 - r_tfin_inleg**2 )            + & ! Inner layer volume
                     pi * ( r**2 - ( r - ins_th )**2 )                                + & ! Outter layer volume
                     2.0D0 * ins_th * (r - r_tfin_inleg - 2.0D0*ins_th) * n_turns_tot     ! inter turn separtion layers

        !  Cross-sectional area at Z
        yy(ii) = pi*r**2 - a_tfin_hole - n_tf*acpcool - yy_ins(ii)  ! eq(32)

        !  Outer casing Cross-sectional area at z 
        yy_casout(ii) = pi * ( (r+cas_out_th)**2 - r**2 )

    end do

    !  Perform integrals using trapezium rule
    sum1 = 0.0D0
    sum2 = 0.0D0
    sum3 = 0.0D0
    sum4 = 0.0D0
    do ii = 1,99
        sum1 = sum1 + yy(ii)
        sum2 = sum2 + 1.0D0/yy(ii)
        sum3 = sum3 + yy_ins(ii)
        sum4 = sum4 + yy_casout(ii)
    end do

    sum1 = 0.5D0*dz * ( yy(0) + yy(100) + 2.0D0*sum1 )
    sum2 = 0.5D0*dz * ( 1.0D0/yy(0) + 1.0D0/yy(100) + 2.0D0*sum2 )
    sum3 = 0.5D0*dz * ( yy_ins(0) + yy_ins(100) + 2.0D0*sum3 )
    sum4 = 0.5D0*dz * ( yy_casout(0) + yy_casout(100) + 2.0D0*sum4 )

    ! Insulation layer cross section at CP top 
    a_cp_ins = pi * ( (r_tfin_inleg + ins_th)**2 - r_tfin_inleg**2 )           + & ! Inner layer volume
               pi * ( rtop**2 - ( rtop - ins_th )**2 )                         + & ! Outter layer volume
               2.0D0 * ins_th * (rtop - r_tfin_inleg - 2.0D0*ins_th) * n_turns_tot ! inter turn separtion layers      

    ! Outer casing cross-section area at CP top [m2]
    a_casout = pi * ( (rtop + cas_out_th)**2 - rtop**2 )

    ! Centrepost volume (ignoring coolant fraction) [m3]
    volume = 2.0D0 * ( sum1 + ( hmaxi - ztop ) * &
                     ( pi*rtop**2 - a_tfin_hole - a_cp_ins - n_tf*acpcool ) )

    ! Resistive power losses in taped section (variable radius section) [W]
    res_taped = rho * curr**2 * sum2                    ! eq(31)

    ! Centrepost insulator volume [m3]
    volins = 2.0D0 * ( sum3 + (hmaxi - ztop) * a_cp_ins )

    ! Outer casing volume [m3]
    volcasout = 2.0D0 * ( sum4 + (hmaxi - ztop) * a_casout ) 

    ! Resistive power losses in cylindrical section (constant radius) [W]
    res_cyl = rho * curr**2 * ( ( hmaxi - ztop ) / &    ! eq(30)
              ( pi * rtop**2 - a_tfin_hole  - a_cp_ins - n_tf*acpcool ) )  

    ! Total CP resistive power [W]
    respow = 2.0D0 * ( res_cyl + res_taped )   ! eq(36)   
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