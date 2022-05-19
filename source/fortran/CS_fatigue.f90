!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module CS_fatigue

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Module containing subroutines and functions for CS coil fatigue modelling
    !
    !

#ifndef dp
    use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif
    implicit none

contains

subroutine Ncycle(n_cycle, max_hoop_stress,residual_stress,t_crack_vertical,t_crack_radial, &
    t_structural_vertical,t_structural_radial)

    use constants, only: pi
    implicit none

    ! Arguments
    real(dp), intent(in) :: max_hoop_stress, residual_stress
    real(dp), intent(in) :: t_crack_vertical, t_structural_vertical, t_structural_radial
    real(dp), intent(inout) :: n_cycle, t_crack_radial

    ! local variables
    real(dp) :: C0, m, R, delta, deltaN, mw, n, CR
    real(dp) :: Kmax, Ka, Kc, a, c, N_pulse
    real(dp) :: max_hoop_stress_MPa, residual_stress_MPa, hoop_stress_MPa

    ! Set material parameters
    ! Magnet Structural Design Criteria, Part I, C. Bachmann c, p.67
    ! https://idm.euro-fusion.org/?uid=2MFLQE
    ! Steel type: 316LN Steel
    C0 = 0.39D-12
    m = 3.5D0
    ! Walker Coefficient
    mw = 0.436D0
    n= -m*(mw-1)
  
    ! Set units to MPa
    max_hoop_stress_MPa = max_hoop_stress / 1.0D6
    residual_stress_MPa = residual_stress / 1.0D6

    ! Set intial crack size
    t_crack_radial = 3.0D0 * t_crack_vertical
    a = t_crack_vertical
    c = t_crack_radial

    ! Cyclic element of stress
    hoop_stress_MPa = max_hoop_stress_MPa
    
    ! Mean stress ratio
    ! Fatigue Stress Assessment in Fusion Magnet Components
    ! J. Lorenzo, X. Sarasola, M. Mantsinen
    R = residual_stress_MPa / (max_hoop_stress_MPa + residual_stress_MPa)
    
    ! Calculated constant for a given stress ratio
    ! https://idm.euro-fusion.org/?uid=2MFLQE
    CR = C0/(1.0D0-R)**n

    ! select given increase in crack (area or length?)
    delta = 1.0D-4

    !Initialise number of cycles
    n_cycle = 0.0
    N_pulse = 0.0
    Kmax = 0.0

    ! factor 2 taken as saftey factors in the crack sizes
    ! CS steel undergoes fast fracture when SIF > 200 MPa, under a saftey factor 2 we use 100MPa
    do while ((a.le.t_structural_vertical/2.0D0).and.(c.le.t_structural_radial/2.0D0).and.(Kmax.le.1.0D2))
        ! find SIF max from SIF_a and SIF_c
        Ka = surface_stress_intensity_factor(hoop_stress_MPa, t_structural_vertical, t_structural_radial, a, c, pi/2.0D0)
        Kc = surface_stress_intensity_factor(hoop_stress_MPa, t_structural_vertical, t_structural_radial, a, c, 0.0D0)
        Kmax = max(Ka,Kc)
    
        ! run euler_method and find number of cycles needed to give crack increase
        deltaN = delta / (CR * (Kmax**m))

        ! update a and c, N
        a = a + delta * (Ka / Kmax) ** m
        c = c + delta * (Kc / Kmax) ** m
        N_pulse = N_pulse + deltaN

    ! two pulses - ramp to Vsmax and ramp down per cycle
    n_cycle = N_pulse / 2.0D0
    end do

end subroutine Ncycle

function embedded_stress_intensity_factor(hoop_stress, t, w, a, c, phi) result(K)
    ! Assumes an embedded elliptical efect geometry
    ! geometric quantities
    ! hoop_stress - change in hoop stress over cycle
    ! t - plate thickness
    ! w - plate width
    ! a - crack depth (t -direction)
    ! c - crack length (w - direction)
    ! Ref: J. Lorenzo, X. Sarasola, M. Mantsinen, Fatigue Stress Assessment
    ! in Fusion Components Nov 13 2020 https://idm.euro-fusion.org/?uid=2PBDRG
    ! Ref: C. Jong, Magnet Structural Design
    ! Criteria Part 1: Main Structural Components and Welds 2012

    use constants, only: pi
    implicit none

    ! Arguments
    real(dp), intent(in) :: hoop_stress, t, w, a, c, phi
    real(dp) :: K

    ! local variables
    real(dp) :: Q, m1, m2, m3, g, f_phi, f_w, F


    if (a<=c) then
        Q = 1.0D0 + 1.464D0 * (a/c)**1.65D0
        m1 = 1.0D0
        m2 = 0.05D0 / (0.11D0 + (a/c)**(3.0D0/2.0D0) )
        m3 = 0.29D0 / (0.23D0 + (a/c)**(3.0D0/2.0D0) )
        g = 1.0D0 - ( (a/t)**4.0D0*sqrt(2.6D0-(2.0D0*a/t)) / (1.0D0+4.0D0*(a/c)) ) * abs(cos(phi))
        f_phi = ((a/c)**2.0D0*(cos(phi))**2.0D0+(sin(phi)**2.0))**(1.0D0/4.0D0)
        f_w = sqrt(1.0D0/cos(sqrt(a/t)*pi*c/(2.0D0*w)))
    else if (a>c) then
        Q = 1.0D0 + 1.464D0 * (c/a)**1.65D0
        m1 = sqrt(c/a)
        m2 = 0.05D0 / (0.11D0 + (a/c)**(3.0D0/2.0D0) )
        m3 = 0.29D0 / (0.23D0 + (a/c)**(3.0D0/2.0D0) )
        g = 1.0D0 - ( (a/t)**4.0D0*sqrt(2.6D0-(2.0D0*a/t)) / (1.0D0+4.0D0*(a/c)) )  * abs(cos(phi))
        f_phi = ((c/a)**2.0D0*(sin(phi))**2.0D0+(cos(phi)**2.0D0))**(1.0D0/4.0D0)
        f_w = sqrt(1.0D0/cos(sqrt(a/t)*pi*c/(2.0D0*w)))
    end if

    ! compute the unitless geometric correction
    F = (m1 + m2 * (a/t)**2.0D0 + m3 * (a/t)**4.0D0 ) * g * f_phi * f_w

    ! compute the stress intensity factor
    K = hoop_stress * F * sqrt(pi * a / Q )

    end function embedded_stress_intensity_factor

function surface_stress_intensity_factor(hoop_stress, t, w, a, c, phi) result(K)
    ! Assumes an surface semi elliptical defect geometry
    ! geometric quantities
    ! hoop_stress - change in hoop stress over cycle
    ! t - plate thickness
    ! w - plate width
    ! a - crack depth (t -direction)
    ! c - crack length (w - direction)
    ! Ref: J. Lorenzo, X. Sarasola, M. Mantsinen, Fatigue Stress Assessment
    ! in Fusion Components Nov 13 2020 https://idm.euro-fusion.org/?uid=2PBDRG
    ! Ref: C. Jong, Magnet Structural Design
    ! Criteria Part 1: Main Structural Components and Welds 2012

    use constants, only: pi
    implicit none

    ! Arguments
    real(dp), intent(in) :: hoop_stress, t, w, a, c, phi
    real(dp) :: K

    ! local variables
    real(dp) :: Q, m1, m2, m3, g, f_phi, f_w, bending_stress
    real(dp) :: p, Hs, H1, H2, G11, G12, G21, G22, F

    bending_stress = 0.0D0! * 3.0 * M / (w*d**2.0)

    if (a<=c) then
        Q = 1.0D0 + 1.464D0 * (a/c)**1.65D0
        m1 = 1.13D0 - 0.09D0 * a / c
        m2 = -0.54D0 + 0.89D0 / (0.2D0 + a/c)
        m3 = 0.5D0 - 1.0D0 / (0.65D0 + a/c) + 14.0D0 *(1-a/c)**24.0D0
        g = 1.0D0 + (0.1D0 + 0.35D0*(a/c)**2.0D0)*(1.0D0-sin(phi))**2.0D0
        f_phi = ((a/c)**2.0D0*(cos(phi))**2.0D0+(sin(phi))**2.0D0)**(1.0D0/4.0D0)
        f_w = sqrt(1.0D0/cos(sqrt(a/t)*pi*c/(2.0D0*w)))
        p = 0.2D0 + a/c + 0.6D0 * a/t
        G21 = - 1.22D0 - 0.12D0 * a/ c
        G22 = 0.55D0 - 1.05D0 *(a/c)**0.75D0 + 0.47D0 * (a/c)**1.5D0
        H1 = 1.0D0 - 0.34D0 * a/t - 0.11D0*a*a /(c*t)
        H2 = 1.0D0 + G21 * a/t + G22 *(a/t)**2.0D0
    else if (a>c) then
        Q = 1.0D0 + 1.464D0 * (c/a)**1.65D0
        m1 = sqrt(c/a) * (1.0D0 + 0.04D0 * c /a )
        m2 = 0.2D0 * (c/a)**4.0D0
        m3 = -0.11D0* (c/a)**4.0D0
        g = 1.0D0 + (0.1D0 + 0.35D0*(c/a)*(a/t)**2.0D0)*(1.0D0-sin(phi))**2.0D0
        f_phi = ((c/a)**2.0D0*(sin(phi))**2.0D0+(cos(phi))**2.0D0)**(1.0D0/4.0D0)
        f_w = sqrt(1.0D0/cos(sqrt(a/t)*pi*c/(2.0D0*w)))
        p = 0.2D0 + c/a + 0.6D0 * a / t
        G11 = -0.04D0 - 0.41D0 * c/a
        G12 = 0.55D0 - 1.93D0 * (c/a)**0.75D0 + 1.38D0*(c/a)**1.5D0
        G21 = -2.11D0 + 0.77D0 * c/a
        G22 = 0.55D0 - 0.72D0 * (c/a)*0.75D0 + 0.14D0*(c/a)*1.5D0
        H1 = 1.0D0 + G11 * a/t + G12 *(a/t)**2.0D0
        H2 = 1.0D0 + G21 * a/t + G22 *(a/t)**2.0D0
    end if

    ! compute the unitless geometric correction
    Hs = H1 + (H2 - H1) * (sin(phi))**p
    F = (m1 + m2 * (a/t)**2.0D0 + m3 * (a/t)**4.0D0 ) * g * f_phi * f_w

    ! compute the stress intensity factor
    K = (hoop_stress + Hs * bending_stress) * F * sqrt(pi * a / Q )

    end function surface_stress_intensity_factor

end module CS_fatigue
