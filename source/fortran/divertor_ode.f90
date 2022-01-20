! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_ode
  !! Module containing divertor Kallenbach model
  !! author: M Kovari, CCFE, Culham Science Centre
  !! N/A
  !! This module contains the PROCESS Kallenbach divertor model
  !! 
  
#ifndef dp
  use, intrinsic :: iso_fortran_env, only: dp=>real64
#endif

  use divertor_ode_var, only: nimp
  use constants, only: pi
  implicit none

  ! Module-level declarations !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!

  logical, public, save :: impurities_present(14)

  ! impurity element name - temporary

  ! relative ion mass
  ! Issue #501: change from 2 to 2.5
  real(dp), private :: aplas

  ! ion mass [kg]
  real(dp), private :: mi

  ! conversion from flux density [el/s] to Pascal [Molecules]
  ! see page 6 of paper.
  real(dp), private :: fluxdens_to_pa

  ! Useful values (combinations of other constants/variables)
  real(dp), private :: eightemi, eightemi48,  elEion
  real(dp), private, parameter :: degree=pi/180.0D0
  real(dp), parameter :: ln10=log(10.0D0)

  ! constant in thermal conductivity (equation 5) [J/(s m eV^7/2)]
  real(dp), private :: kappa0

  ! neutral velocity along the flux bundle, groups 1 & 2 [m/s]
  real(dp), private :: v01, v02

  ! Allowable absolute/relative error (UNUSED)
!   real(dp), private :: abserr, relerr

  ! Circumference of plasma at outboard midplane and at target [m]
  real(dp), private :: circumference_omp, circumference_target

  ! Circumference of normal to helical field line [m]
  real(dp), private :: circumf_bu

  ! Flux bundle area perp. to B at target and at omp [m2]
  real(dp), private :: area_target, area_omp

  ! Zeff for divertor region
  real(dp) :: zeff_div

  ! SOL radial thickness extrapolated to OMP [m]
  real(dp), private :: sol_broadening

  ! Ratio: psep_kallenbach / Powerup
  real(dp), private, parameter :: seppowerratio=2.3D0

  real(dp), private :: lengthofwidesol

contains

  subroutine init_divertor_ode
    !! Initialise divertor_ode
    implicit none

    impurities_present = .false.
    ! Set entire array to false
    aplas=2.5D0
    fluxdens_to_pa= 1.0D0/1.55e23
    kappa0=2390.0D0
    mi = 0.0D0
    eightemi = 0.0D0
    eightemi48 = 0.0D0
    elEion = 0.0D0
    v01 = 0.0D0
    v02 = 0.0D0
    circumference_omp = 0.0D0
    circumference_target = 0.0D0
    circumf_bu = 0.0D0
    area_target = 0.0D0
    area_omp = 0.0D0
    zeff_div = 0.0D0
    sol_broadening = 0.0D0
    lengthofwidesol = 0.0D0
  end subroutine init_divertor_ode

  subroutine divertor_Kallenbach(rmajor,rminor,bt,plascur,q,verboseset,     &
             ttarget,qtargettotal,targetangle, &
             unit_test,  &
             bp, &
             psep_kallenbach, teomp, neomp,  &
             outfile,iprint)

    !! calculate radiative loss and plasma profiles along a flux tube including momentum losses
    !! author: M Kovari, CCFE, Culham Science Centre
    !! Calculate radiative loss and plasma profiles along a flux tube including momentum losses.
    !! Description in A. Kallenbach et al., PPCF 58 (2016) 045013, this version based on that
    !! sent by Arne 17.5.2016.
    !! Note this solver is not suitable for stiff equations - which these are. Instead I have
    !! set the neutral density derivatives to zero when the neutral density is small.
    !! https://people.sc.fsu.edu/~jburkardt/f_src/ode/ode.html
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use ode_mod , only :            ode
    use numerics, only :            active_constraints
    use physics_variables, only :   nesep, pdivt
    use read_radiation, only: read_lz
    use global_variables, only: iscan_global, fileprefix
    use maths_library, only: integer2string
    use read_and_get_atomic_data, only: get_h_rates, unit_test_read
    use process_output, only: oheadr, ocmmnt, ovarre, osubhd, ovarin
    use constants, only: echarge, rmu0, umass
		use div_kal_vars, only: netau_sol, neratio, &
      exchangepowerlost, fractionwidesol, target_spread, fmom, totalpowerlost, &
      relerr_sol, hydrogenicpowerlost, ionisationpowerlost, impuritypowerlost, &
      mach0, impurity_enrichment, lcon_factor, pressure0, abserr_sol, &
      lambda_q_omp, kallenbach_switch 
		use build_variables, only: rspo 
		use physics_variables, only: tesep_keV => tesep
		use divertor_ode_var, only: nimp, imp_label, &
      impurity_concs, impurity_arr
		use divertor_variables, only: hldiv 
    implicit none

    logical::verbose
    logical,save :: firstcall
    logical, intent(in) :: verboseset
    logical, optional, intent(in) :: unit_test

    ! Output file unit identifier
    integer, intent(in) :: outfile

    ! Print flag
    integer, intent(in) :: iprint

    ! Plasma major/minor radius [m]
    real(dp), intent(in) :: rmajor, rminor

    ! Toroidal field on-axis [T]
    real(dp), intent(in) :: bt

    ! Plasma current [A]
    real(dp), intent(in) :: plascur

    ! Vertical field at plasma [T] UNUSED
    ! real(dp), intent(in) :: bvert

    ! Plasma safety factor near edge
    real(dp), intent(in) :: q

    ! Target temperature input [eV]
    real(dp), intent(in) :: ttarget

    ! qtargettotal : Power density on target including surface recombination [W/m2]
    real(dp),intent(in) :: qtargettotal

    real(dp) :: qtargetcomplete
    !! qtargetcomplete : Total power density on target [W/m2]

    ! target angle [deg]
    real(dp),intent(in) :: targetangle

    ! connection length from omp to target [m]
    real(dp) :: lcon

    ! Absolute error input
    ! real(dp), optional, intent(in) :: abserrset

    ! Average poloidal field (T)
    real(dp), intent(in) :: bp

    ! Power conducted through the separatrix, calculated by divertor model [W]
    real(dp), intent(out) :: psep_kallenbach

    ! Temperature (eV) and density (1/m3) at outboard midplane
    real(dp), intent(out) :: teomp, neomp

    ! Btheta, Bphi and Btotal at OMP (equation 1 of Kallenbach)
    real(dp) :: Bp_omp, Bt_omp, Btotal_omp

    ! Field due to plasma current [T]
    real(dp) :: BpPlasmaOmp

    ! Kallenbach equation 1 terms
    ! real(dp) :: bu  ! 2018/4/16

    ! ion flux density into target [m-2.s-1]
    real(dp) :: partfluxtar

    ! electron energy loss due to ionization [eV]
    real(dp) :: Eion

    ! kinetic energy for the first group of neutrals [eV]
    real(dp) :: Eneutrals

    ! neutral velocity along the flux bundle, group 1 [m/s]
    real(dp) :: v0

    ! Electron sheath energy transmission coefficient
    real(dp), parameter :: gammae = 5.5d0
    ! Ion energy reflection coefficient
    real(dp), parameter :: energyreflection = 0.5d0
    ! Ion sheath energy transmission coefficient
    ! Issue #500 item 3. Adjust sheath heat transmission coefficient for ion reflection.
    real(dp), parameter :: gammai = 2.5d0 * (1d0 - energyreflection)
    ! Sheath energy transmission coefficient (kallenbach paper pg. 4)
    real(dp), parameter :: gammasheath = gammae + gammai

    ! Recombination energy [eV]
    real(dp) :: Erecomb

    ! Volume recombination energy (assumed to be lost as radiation) [eV]
    real(dp) :: Evolrec

    ! Ion sound speed near target, and a value slightly less [m/s]
    real(dp) :: cs0, cs0minus

    ! Nominal neutral pressure at target [Pa]
    real(dp) :: p0partflux

    ! Target area to target wetted area factor
    real(dp) :: sinfact

    ! Relative distribution between 2 neutral particle classes
    real(dp) :: neutfrac1, neutfrac2

    ! factor by which neutral penetration is enhanced
    real(dp) :: neutpenfact2

    ! neutral density in group 1, 2
    real(dp) :: n010, n020

    ! electron temperature [eV]
    real(dp) :: te0

    ! n*v [1/m2.s]
    real(dp) :: nv0

    ! Total power running along SOL [W]
    real(dp) :: Power0

    ! impurity element name - temporary
    character(len=2) :: element

    ! Poloidal, toroidal and total field at target
    real(dp) :: Bp_target, Bt_target, Btotal_target, pitch_angle, sin_pitch_angle
    real(dp) :: poloidal_flux_expansion

    real(dp) ::ptarget_conv
    !! ptarget_conv : power deposited on target by convection [W]
    real(dp) ::ptarget_cond
    !! ptarget_cond : power deposited on target by conduction [W]
    real(dp) ::ptarget_recomb
    !! ptarget_recomb : power deposited on target by recombination of hydrogenic ions [W]
    real(dp) ::ptarget_isotropic
    real(dp) ::ptarget_total
    real(dp) ::ptarget_complete
    real(dp) ::neutral_target


    ! ODE solver parameters
    ! !!!!!!!!!!!!!!!!!!!!!!!

    ! Number of steps along the 1D line
    integer(kind=4), parameter :: step_num = 200

    ! NEQN, the number of coupled  differential equations to be solved
    integer(kind=4), parameter :: neqn = 10

    integer(kind=4) :: i, step
    integer(kind=4) :: iflag
    integer(kind=4) :: iwork(5)

    ! First step [m]
    real(dp) :: step0

    ! Ratio between successive step sizes
    real(dp) :: factor

    real(dp) :: x
    real(dp) :: xout
    real(dp) :: work(100+21*neqn)
    real(dp) :: y(neqn)

    ! These are local variables for recalculation at each data point - not used for integration:
    real(dp) :: v, n0, nelsquared
    real(dp) :: s, al, Rcx, plt, prb
    real(dp) :: cxrate, plossdenscx, ionrate1, ionrate2, recrate
    real(dp) :: plossion, lz, raddens, radHdens, qperp_total, qperp_conv, qperp_conducted
    real(dp) :: n, n01, n02, te, nv, pressure, Power, nv24, nel20, Pthermal, n0e20, bracket
    real(dp) :: cs, mach

    real(dp) :: A_cross
    !! A_cross : Area of flux bundle perpendicular to B

    real(dp) :: lambda_q_target
    !! lambda_q_target : SOL radial thickness at the target, mapped to OMP [m]

    real(dp) :: Powerup
    !! Powerup : upstream power [W]

    real(dp) :: balance
    !! balance : Power balance error - should be zero [W]

    ! Plasma thermal pressure
    real(dp) :: nete

    real(dp) :: nete0
    !! nete0 : Plasma thermal pressure near target

    real(dp) :: nel0
    !! nel0 : Electron density near target [m-3]

    real(dp) :: qtarget
    !! qtarget : Power density on target not including surface recombination [W/m2]

    real(dp) :: qtarget_isotropic
    !! qtarget_isotropic : Power density on target due to isotropic losses from SOL [W/m2]

    real(dp) :: receiving_area
    !! receiving_area : Area on which the power due to isotropic losses from SOL energy is incident [m2]

    ! Mean charge and quadratic mean charge from ADAS
    real(dp) :: z, qz

    ! Power density on target due to surface recombination [W/m2]
    real(dp) :: qtargetrecomb

    ! Total power on target [W]
    real(dp) :: powertargettotal



    real(dp) :: WettedArea, WettedAreaComparison
    !! WettedArea : Wetted area of target [m2]

    real(dp) :: WettedLength, WettedLengthComparison
    !! WettedLength : Wetted length on target measured in poloidal plane [m]

    real(dp) :: sab
    !! sab : connection length used for calculating the "near zone" [m]
    real(dp) :: romp
    !! romp : Major radius at outer midplane  [m]
    real(dp) :: ionfluxtarget
    !! ionfluxtarget : Ion flux density on target [m-2s-1]
    ! Typical SOL temperature, used only for estimating zeffective in the SOL [eV]
    real(dp) :: ttypical
    ! Impurity radiation by species
    real(dp) :: raddensspecies(nimp)

    ! Chodura sheath width [m]
    real(dp) :: lchodura
    real(dp),parameter :: squareroot6 = sqrt(6d0)

    character(len=200) :: filename
    ! Angle between B and the surface normal (deg)
    real(dp) :: psi
    ! Poloidal flux through flux tube at OMP, target 2018/6/15
    real(dp) :: psi_p_omp, psi_p_target
    ! Power loss integrals corrected by subtracting  1
    real(dp) :: y7, y8, y9, y10

    !! Initialise local variables
    firstcall = .true.
    element='**'
    step0=0.0002
    raddensspecies(nimp)=0.0d0

    ! Major radius at outer midplane [m]
    romp = rmajor + rminor

    ! Typical SOL temperature, used only for estimating zeffective in the SOL [eV]
    ttypical=1000.0d0*tesep_keV/2.0d0
    ! B theta at OMP
    Bt_omp = - bt*rmajor/romp      ! OK
    ! B theta at target
    ! On the first iteration rspo may still have its initial value of zero:
    if(rspo.lt.1.0d0) rspo = rmajor
    Bt_target = - bt*rmajor/rspo   ! OK

    ! Cylindrical approximation for field at OMP due to plasma current
    BpPlasmaOmp = rmu0 * plascur / (2.0D0*pi*rminor)

    ! At the OMP the vertical field generated by the equilibrium field coils
    ! (assumed approximately constant), adds to the field generated by the plasma.
    ! This formula not used as it doesn't seem to agree with the eqdsk data.
    ! Bp_omp = BpPlasmaOmp + abs(bvert)
    ! Bp_target = 0.35d0 * bp / 0.956d0     Obsolete

    ! At the target the poloidal field is much smaller, as it is near the X-point.
    ! Issue #500 item 6, comparing the poloidal field at the target given in a
    ! DEMO eqdsk file, with the nominal Bpoloidal from PROCESS.  2018/6/14
    Bp_target = 0.5d0 * bp / 0.921d0
    Bp_omp =   1.35d0 * bp / 0.921d0

    Btotal_omp = sqrt(Bp_omp**2 + Bt_omp**2)  ! OK
    !   Btotal_target = sqrt(Bp_omp**2 + Bt_target**2)  NOT OK
    Btotal_target = sqrt(Bp_target**2 + Bt_target**2)  ! 2018/06/14

    ! Sine of pitch angle
    sin_pitch_angle = abs(Bp_target / Btotal_target)
    ! Ratio: target area perp to B / target wetted area
    sinfact = 1.0D0 / (sin(targetangle*degree) * sin_pitch_angle)
    ! Angle between B and the surface normal (deg)
    ! psi = 90.0d0 - acos(1.0d0/sinfact)/degree  NOT OK
    ! Equation 19.
    psi = acos(sin(targetangle*degree) * abs(Bp_target / Btotal_target)) /degree  ! 2018/06/14

    ! Connection length from OMP to target
    ! MDK Issues #494, #497
    ! lambda_q_omp is taken to be the relevant radial distance from the separatrix at the OMP.
    ! `lcon_factor` is still available but not recommended.
    lcon = lcon_factor * (pi*q*rmajor/93.2) * (21.25*log(1/lambda_q_omp)-8.7)

    lengthofwidesol = fractionwidesol * lcon
    ! lambda_q_target is no longer an input.
    lambda_q_target = lambda_q_omp + target_spread
    sol_broadening = lambda_q_target / lambda_q_omp

    ! Populate array that says which impurities are present
    if(firstcall) then

        ! Helium is always present (even if ralpne is initially zero)
        impurities_present(2) = .true.

        ! Loop over the remaining PROCESS impurities
        do i = 3, nimp
            if (impurity_arr(i)%frac .gt. 1.d-10) then
                impurities_present(i) = .true.
            end if
        end do
        firstcall = .false.

    endif

    ! Get impurity concentrations every time, as they can change
    do i = 2, nimp
        if(impurities_present(i)) then
           impurity_concs(i)= impurity_arr(i)%frac * impurity_enrichment(i)
        else
            impurity_concs(i)=0.0d0
        endif
    enddo

    ! IFLAG.  (ODE.F90) Normal input is +1.  The user should set IFLAG = -1 only
    ! if it is impossible to continue the integration beyond TOUT.
    iflag = 1

    ! set verbose level
    verbose = verboseset

    if (verbose) then
        write(*,*) 'subroutine divertor_Kallenbach'
    end if

    ! ion mass [kg]
    mi = umass*aplas

    ! RELERR, ABSERR, the relative and absolute local error tolerances
    ! At each step, the code requires
    !      abs ( local error ) <= abs ( y ) * relerr + abserr
    ! for each component of the local error and solution vectors.
    ! abserr is also used to cut off reactions involving neutrals
    ! abserr = 1.0d-3
    ! relerr = 1.0D-3
    ! ! Set absolute error level if input
    ! if (present(abserrset)) then
    !     abserr = abserrset
    ! end if

    ! Dissociation energy, T2 to 2T = 4.59 eV
    ! Ionization energy, T to T+ e- = 13.55 eV
    ! http://energy.gov/sites/prod/files/2013/07/f2/hdbk1129.pdf
    ! D2: Experimental Ionization Energy is 13.603 eV
    ! Dissociation energy, D2 to 2D = 4.52 eV
    ! Use approx mean values: dissociation: 4.6 eV per molecule, ionization 13.6 eV.
    ! Energy released at the target from surface recombination of ions and association (per ion)
    ! = 13.6 +  4.6/2 = 15.9 eV
    ! Note Kallenbach figure was 15.0 eV.

    ! See Figure 7, which uses target heat load, "not counting recombination energy".
    Erecomb = 15.9D0

    ! volume recombination energy (assumed to be lost as radiation) [eV]
    Evolrec = 13.6D0

    ! I think this term is the influence of recombination on the energy balance,
    ! which is neglected in the paper

    ! electron energy loss due to ionization [eV]
    Eion = 15.0D0

    ! includes some dissociation. H radiation treated explicitly  [eV].  eq 4.  Ref?
    ! kinetic energy [eV] for the first group of neutrals (p. 4)
    Eneutrals = 5.0D0

    ! set up relative distribution between 2 neutral particle classes, p. 4.
    neutfrac1 = 0.5D0
    neutfrac2 = 1.0D0 - neutfrac1

    ! factor by which neutral penetration is enhanced to simulate 1/20 fieldline
    ! pitch at target this is just the the velocity ratio on page 4
    neutpenfact2 = 10.0D0

    ! useful parameter combinations
    eightemi = 8.0D0*echarge*mi
    eightemi48 = eightemi * 1.0D48
    elEion = echarge * Eion

    ! (Stangeby NF 51 (2011), equation 1 of Kallenbach)
    ! bu = sin(atan(abs(Bp_omp/Bt_omp)))  Simplify this:
    ! bu = Bp_omp / Btotal_omp   ! 2018/06/14

    ! Actual circumference at OMP and target [m]
    circumference_omp = 2.0D0*pi*romp    ! OK
    circumference_target = 2.0D0*pi*rspo ! OK

    ! Poloidal flux through flux tube at OMP 2018/6/15
    psi_p_omp = Bp_omp * circumference_omp * lambda_q_omp
    ! Poloidal flux through flux tube at target 2018/6/15
    ! Remember that 'lambda_q_target' (so-called) is mapped to the OMP.
    psi_p_target = Bp_omp * circumference_omp * lambda_q_target

    ! Circumference measured normal to B at OMP [m]
    ! circumf_bu = circumference_omp*bu  Simplify:
    circumf_bu = circumference_omp * Bp_omp / Btotal_omp  ! 2018/4/16
    ! Flux bundle area perp. to B at omp [m2]
    area_omp = circumf_bu * lambda_q_omp  ! OK

    ! neutral velocity along the flux bundle, group 1 [m/s]
    v0 = 0.25D0 * sqrt(8.0D0/pi * echarge*Eneutrals / mi)
    ! Rename this for consistency
    v01 = v0
    ! neutral velocity along the flux bundle, group 2 [m/s]
    v02 = v0*neutpenfact2

    ! Sound speed [m/s]
    cs0 = sqrt(2.0D0*echarge*ttarget/mi)
    ! To prevent division by zero at the target, insert a factor just less than 1.
    cs0minus = cs0 * mach0

    ! Chodura sheath width (m).  Sin psi taken = 1.
    ! Stangeby Equation 2.112
    lchodura = squareroot6 * ttarget / (Btotal_target*cs0)

    ! Flux bundle area perp. to B at target [m2]
    ! Include total flux expansion here (but not consistently) 2018/6/15
    area_target = area_omp * sol_broadening * Btotal_omp / Btotal_target
    ! Wetted area on target [m2]
    WettedArea= area_target*sinfact
    ! Wetted length on target measured in poloidal plane [m]
    WettedLength = WettedArea/circumference_target
    ! Simplify, using poloidal flux
    WettedAreaComparison = psi_p_target / Bp_target / sin(targetangle*degree)  ! 2018/6/16
    WettedLengthComparison = WettedAreaComparison / circumference_target       ! 2018/6/16

    ! Area on which the power due to isotropic losses from SOL energy is incident [m2]
    ! Issue #497 Change romp to rspo
    receiving_area = circumference_target * 2.0D0 * WettedLength

    ! The connection length used for calculating the "near zone":
    sab = 2.0D0*WettedLength/cos(targetangle*degree) * Btotal_target/Bp_target

    ! Calculate the power density carried by the plasma from the total power density
    ! on the target which includes surface recombination and association [W/m2]
    qtarget = qtargettotal / (1.0 + Erecomb/gammasheath/ttarget)

    ! Mach=1 is assumed as boundary condition at the target
    ! This equation not stated explicitly in the paper.
    ! Plasma density near target [m-3]
    nel0 = qtarget*sinfact/(gammasheath*echarge*ttarget*cs0minus)

    ! Ion flux density perp to B at target [m-2.s-1]
    partfluxtar = nel0*cs0minus

    ! Estimate of corresponding neutral pressure, assuming conservation of flux densities the
    ! factor converts the flux density at the target into a molecular pressure at room temperature.
    ! This is a conventional quantity only.
    ! Nominal neutral pressure at target [p0partflux] [Pa]
    p0partflux = partfluxtar*fluxdens_to_pa/sinfact

    ! Find mean Z and mean Z^2 at a typical temperature 'ttypical' for each impurity
    ! Sum + 1 to get Zeffective
    zeff_div = 1.0D0
do i = 2, nimp
        if(impurities_present(i)) then
            element = imp_label(i)
            z = read_lz(element,ttypical,netau_sol, mean_z=.true., mean_qz=.false., verbose=.false.)
            qz = read_lz(element,ttypical,netau_sol, mean_z=.false., mean_qz=.true., verbose=.false.)
            zeff_div = zeff_div + impurity_concs(i)*(qz-z)
        endif
    enddo

    ! Initialise independent variables for differential equations
    ! neutral density in group 1 [m-3]
    n010 = partfluxtar*neutfrac1/v01

    ! neutral density in group 2 [m-3]
    n020 = partfluxtar*neutfrac2/v02

    ! electron temperature [eV]
    te0 =  ttarget

    ! n*v [m-2.s-1]
    nv0 = -nel0*cs0minus

    ! Pressure  (Kallenbach paper equation 6) [Pa]
    pressure0 = mi*nv0**2.0D0 / nel0 + nel0*2.0D0*echarge*te0

    ! n*T [eV.m-3]
    nete0 = nel0*te0

    ! Total power running along SOL [W]
    Power0 = qtarget*WettedArea

    ! Power deposited on target by recombination of hydrogenic ions [W]
    ptarget_recomb = Erecomb*echarge*nel0*cs0minus*area_target

    ! Power density on target due to surface recombination [W/m2]
    qtargetrecomb = ptarget_recomb / WettedArea

    ! Total power on target [W]
    powertargettotal = Power0 + ptarget_recomb

    if ((present(unit_test)).and.unit_test) then
        call unit_test_read()
        return
    endif

    ! Define an initial X value:
    x = 0.0D+00                     ! (ODE.F90: T)

    ! Define initial Y values:
    y(1) = n010*1.d-20   ! y(1) = neutral density (group 1) [1e20 m-3]
    y(2) = n020*1.d-20   ! y(2) = neutral density (group 2) [1e20 m-3]
    y(3) = te0           ! y(3) = temperature [eV]
    y(4) = nv0*1.d-24    ! y(4) = ion flux [1e24 m-3 s-1]
    y(5) = pressure0  
    y(6) = Power0/1.d6   ! y(6) = power in SOL [MW]
    y(7) =1.0D0          ! Y(7) = 1 + integral of impurity radiation loss [MW]
    y(8) =1.0D0          ! Y(8) = 1 + integral of radiation loss from hydrogenic species [MW]
    y(9) =1.0D0          ! Y(9) = 1 + integral of power loss due to charge exchange [MW]
    y(10)=1.0d0          ! Y(10)= 1 + integral of power loss due to electron impact ionisation [MW]

    ! Use logarithmic spacing for the x values along the SOL
    factor = 10.0D0**(log10(lcon/step0)/real(step_num))

    ! Set initial x (ODE, T) value to step
    xout = step0

    if(iprint.eq.1) then
        if(iscan_global/=0)then
            filename = trim(fileprefix)//'_divertor' // integer2string(iscan_global) // '.txt'
        else
            filename = trim(fileprefix)//'_divertor.txt'
        end if
        open(unit=9, file=filename, status='replace')
        write(9,*) 'Y7_=_integral_of_radiation_loss_from_impurities_[MW]'
        write(9,*) 'Y8_=_integral_of_radiation_loss_from_hydrogenic_species_[MW]'
        write(9,*) 'Y9_=_integral_of_power_loss_due_to_charge_exchange_[MW]'
        write(9,*) 'Y10=_integral_of_power_loss_due_to_electron_impact_ionisation_[MW]'
        write(9,*) 'qconv=_Convected_power_density_through_a_surface_perpendicular_to_B_[W/m2]'
        write(9,*) 'qcond=_Conducted_power_density_through_a_surface_perpendicular_to_B_[W/m2]'
        write(9,*) 'im_rad=_Impurity_radiation_[W/m2]'
        write(9,'(a5, 2x, a9, 47a12)')  &
              'step', 'x//B_[m]', 'te_[eV]', 'ne/1e20/m3', 'Pth_[Pa]', 'Ptotal_[Pa]', &
              'v_[m/s]', 'mach ',                                                     &
              'n0/1e20/m3', 'Power_[W]', 'perp_area', 'qtot_W/m2', 'qconv_W/m2', 'qcond_W/m2',         &
              'CX_W/m3', 'Ion_W/m3' , 'H_rad_W/m3', 'im_rad_W/m3', 'Y7', 'Y8', 'Y9', 'Y10',    &
              (impurity_arr(i)%Label, i=2,nimp),'n01/1e20/m3','n02/1e20m-3','nv24','v/ms-1'
        !open(unit=10, file='divertor_diagnostics.txt', status='replace')
    endif   ! (iprint.eq.1)

    do step = 0, step_num+1
        if(step.ne.0) then
            ! Solve the set of differential equations
            call ode(differential, neqn, y, x, xout, relerr_sol, abserr_sol, iflag, work, iwork )

            ! Logarithmic step along SOL (x)
            xout = xout*factor

            if ( iflag /= 2 ) then
                write(*, '(a)' ) ' '
                write(*, '(a)' ) 'Differential equation solver in divertor model has failed:'
                write(*,'(a,i8)') 'ODE.F90, called by subroutine sol_ode in divertor_ode.f90 returned IFLAG = ', iflag
                write(*,*) 'Step number = ',step, '  x = ', x
                write(*,*) 'relerr_sol = ',relerr_sol, '  abserr_sol = ', abserr_sol

                select case (iflag)
                case(3) ; write(*,*) 'Integration did not reach TOUT because the error tolerances were too small.'
                case(4) ; write(*,*) 'Integration did not reach TOUT because more than 500 steps were taken.'
                case(5) ; write(*,*) 'Integration did not reach TOUT because the equations appear to be stiff.'
                case(6) ; write(*,*) 'Invalid input parameters.'
                end select

                write(*,*)'Kallenbach SOL model: vector of dependent variables Y'
                write(*,'(10es12.3)') y
                stop 1
            endif
        endif  ! (step.ne.0)

        ! Derived quantities need to be recalculated at each data point
        ! and converted to SI units
        n01 = Y(1)*1.d20
        n02 = Y(2)*1.d20
        te  = Y(3)
        nv24 = Y(4)
        nv  = nv24*1.d24
        pressure = Y(5) 
        Power   = Y(6)*1.d6

        ! The area of the flux tube, measured perpendicular to B
        ! This is set to a step function as in Kallenbach
        if(x.lt.lengthofwidesol) then
            A_cross = area_target
        else
            A_cross = area_omp
        end if

        ! Calculate density [m-3] 
        bracket = max( (pressure**2.0D0 - eightemi48*te*nv24**2.0D0), 0.0D0)
        n = (pressure + sqrt(bracket))/(4.0D0*echarge*te)
        nel20 = n/1.d20
        nelsquared = n**2.0D0
        
        Pthermal = 2.0D0*n*te*echarge               ! Thermal pressure [Pa]
        
        v = nv/n
        ! Neutral density = sum of the two velocity groups [1e20.m-3]
        n0e20 = Y(1) + Y(2)
        ! Heat flux perpendicular to flux tube cross section [W/m2]
        qperp_total = Power/A_cross

        cs = sqrt(2.0D0*echarge*Te/mi)
        mach = -v/cs
        nete = n*te

        ! Convective heat flux is positive [W/m2]
        qperp_conv= -(5.0D0*echarge*Te + 0.5D0*mi*v**2)*nv

        ! Conducted heat flux [W/m2]
        qperp_conducted = qperp_total - qperp_conv

        ! Neutral density = sum of the two velocity groups [m-3]
        n0 = n01 + n02

        ! If the total neutral density is small, set the atomic rates to zero.
        ! This adjustment is used in subroutine differential to make it behave better (less stiff)
        ! Include it here for consistency
        if(n0.gt.abserr_sol*1.d20) then
            call get_h_rates(n, te, s, al, Rcx, plt, prb, aplas, verbose=.false.)

            ! charge exchange rate for equation 7 [s-1]
            cxrate = Rcx*n*n0
            ! ionisation rate of neutrals: velocity group 1 [s-1]
            ionrate1 = s * n * n01
            ! ionisation rate of neutrals: velocity group 2 [s-1]
            ionrate2 = s * n * n02
            ! recombination rate of ions and electrons [s-1]
            recrate = al*nelsquared
            ! energy conservation: equation 4, charge exchange term
            plossdenscx = echarge*te*cxrate
            ! energy conservation: equation 4, ionisation term
            plossion = (ionrate1 + ionrate2)*elEion
            ! radiation loss density for neutral hydrogenic species
            radHdens = (plt + prb)*n0*n

         else
            cxrate =0.0D0
            ionrate1 = 0.0D0
            ionrate2 = 0.0D0
            recrate = 0.0D0
            plossdenscx = 0.0D0
            plossion = 0.0D0
            radHdens = 0.0D0
        endif

        ! Get radiative loss for all impurities present
        raddens = 0.0D0
       do i = 2, nimp
            if(impurities_present(i)) then
                lz = read_lz(imp_label(i), te, netau_sol, mean_z=.false., mean_qz=.false., verbose=.false.)
                ! Store species-specific radiation loss density
                raddensspecies(i) = lz*impurity_concs(i)*nelsquared
                ! Total impurity radiation loss density
                raddens = raddens + raddensspecies(i)
            endif
        enddo

        ! Subtract 1 to restore physical quantities
        y7 = y(7)-1.d0
        y8 = y(8)-1.d0
        y9 = y(9)-1.d0
        y10 = y(10)-1.d0

        if(iprint.eq.1) then
            ! Use the 'es12.3e3' format code to ensure fixed three-digit exponent
            write(9,'(i5, 2x, f9.5, 46es12.3e3)')  &
                step, x, te, nel20, Pthermal, pressure, v, mach, n0e20, Power, A_cross, qperp_total,  &
                qperp_conv, qperp_conducted,plossdenscx, plossion, radHdens, raddens,                 &
                y7, y8, y9, y10, (raddensspecies(i), i=2,nimp),y(1),y(2),nv24,v
        end if

        ! When we reach the edge of the "near zone" (connection length = sab),
        ! we use split the integrated emission: half towards the target.
        if((x.lt.sab).and.(xout.gt.sab)) then
            qtarget_isotropic = 0.5D0 * 1e6 * (y7 + y8 + y9*(1d0-energyreflection)) / (2.0D0*WettedArea)
        endif

        ! Store some target parameters
        if(step==0)then
            ptarget_conv = qperp_conv * A_cross
            ptarget_cond = qperp_conducted * A_cross
            ! Neutral density at targetangle [m-3]
            neutral_target = n0e20 * 1d20
        endif
    end do     ! step
    if(verbose) write(*,*)'Differential equations complete'

    ! Midplane (upstream) and integrated quantities
    ! -----------------------------------------
    ptarget_isotropic = qtarget_isotropic * WettedArea
    ! This total is for checking only
    ptarget_total = ptarget_conv + ptarget_cond + ptarget_recomb + ptarget_isotropic

    ! Upstream power [W]
    Powerup = Power
    ! Power conducted through the separatrix, calculated by divertor model [W]
    psep_kallenbach = seppowerratio*Powerup

    ! Total power lost due to radiation, ionisation and recombination [W]
    totalpowerlost =  (y7+y8+y9+y10)*1.0d6
    ! Power balance - should be zero [W]
    balance = Y(6)*1.d6 - totalpowerlost - Power0

    ! momentum factor [-]
    fmom = 2.0D0*nete0/nete

    ! Ion flux density on target [m-2s-1]
    ionfluxtarget = partfluxtar/sinfact

    teomp = te         ! Plasma temperature at outer midplane [eV]
    neomp = n        ! Plasma density at outer midplane [m-3]
    ! -----------------------------------------------------

    ! Output ----------------------------------------------

! SJP Issue #834
! Calculate hldiv from qtargetcomplete with the correct units

    hldiv = (qtargettotal + qtarget_isotropic) * 1.0d-6

    if(iprint.eq.0) return

    ! Close divertor output and diagnostic .txt files
    close(unit=9)
    close(unit=10)

    impuritypowerlost = y7*1.d6
    hydrogenicpowerlost = y8*1.d6
    exchangepowerlost = y9*1.d6
    ionisationpowerlost = y10*1.d6

    pitch_angle = asin(sin_pitch_angle)/degree
    poloidal_flux_expansion = Bp_omp / Bp_target
    qtargetcomplete = qtargettotal + qtarget_isotropic
    ! Just to check
    ptarget_complete = qtargetcomplete * WettedArea

    call oheadr(outfile, 'Divertor: Kallenbach 1D Model')
    call ocmmnt(outfile, 'For graphical output use kallenbach_plotting.py')

    call osubhd(outfile, 'Global SOL properties and geometry:')
    call ovarre(outfile, 'Connection length:  [m]','(lcon)', lcon, 'OP ')
    call ovarre(outfile, 'Parameter for approach to local equilibrium  [ms.1e20/m3]','(netau_sol)', netau_sol)
    call ovarre(outfile, 'Typical SOL temperature, used only for estimating zeff_div [eV] ','(ttypical)', ttypical, 'OP ')
    call ocmmnt(outfile, 'The zeff_div is used only for estimating thermal conductivity of the SOL plasma.')
    call ovarre(outfile, 'Z effective [W] ','(zeff_div)', zeff_div, 'OP ')

    call ovarre(outfile, 'Plasma major radius [m]  ','(rmajor)', rmajor, 'OP ')
    call ovarre(outfile, 'Outboard midplane radius [m]  ','(romp)', romp, 'OP ')
    call ovarre(outfile, 'Outboard strike point radius [m]  ','(rspo)', rspo, 'OP ')

    call ovarre(outfile, 'Toroidal field at outboard midplane [T] ','(Bt_omp)', Bt_omp, 'OP ')
    call ovarre(outfile, 'Poloidal field at outboard midplane [T] ','(Bp_omp)', Bp_omp, 'OP ')
    call ovarre(outfile, 'Total field at outboard midplane [T]  ','(Btotal_omp)', Btotal_omp, 'OP ')
    call ovarre(outfile, 'Toroidal field at strike point [T]   ','(Bt_target)', Bt_target, 'OP ')
    call ovarre(outfile, 'Poloidal field at strike point [T]   ','(Bp_target)', Bp_target, 'OP ')
    call ovarre(outfile, 'Poloidal flux expansion: Bp_omp/Bp_target','(poloidal_flux_expansion)', poloidal_flux_expansion, 'OP ')

    call osubhd(outfile, 'Properties of SOL plasma :')
    call ovarre(outfile, 'SOL power fall-off length at the outer midplane [m]','(lambda_q_omp)', lambda_q_omp)
    call ovarre(outfile, 'SOL radial thickness at the target, mapped to OMP [m]','(lambda_q_target)', lambda_q_target)
    call ovarre(outfile, 'SOL area (normal to B) at outer midplane [m2]','(area_omp)', area_omp, 'OP ')
    call ovarre(outfile, 'SOL area (normal to B) at target [m2]','(area_target)', area_target, 'OP ')
    call ovarre(outfile, 'Plasma temperature at outer midplane [eV]','(teomp)', teomp, 'OP ')
    call ovarre(outfile, 'Plasma density at outer midplane [m-3]','(neomp)', neomp, 'OP ')
    if(active_constraints(71) .eqv. .true.)then
        call ocmmnt(outfile, 'Constraint 71 is applied as follows.')
        call ovarre(outfile, '. Ratio: SOL density at OMP / separatrix density','(neratio)', neratio)
    else
             call ocmmnt(outfile, 'Separatrix density consistency constraint 71 is NOT applied')
    end if
    call ovarre(outfile, '. COMPARISON: Plasma density at separatrix [m-3]','(nesep.)', nesep, 'OP ')

    call ovarre(outfile, 'Poloidal field at outer midplane [T]','(Bp_omp)', Bp_omp, 'OP ')
    call ovarre(outfile, 'Power at outer midplane [W] ','(Powerup)', Powerup, 'OP ')
    call ovarre(outfile, 'Power conducted through the separatrix, calculated by divertor model [W] ',&
                         '(psep_kallenbach)', psep_kallenbach, 'OP ')
    if(active_constraints(69) .eqv. .true.)then
        call ocmmnt(outfile, 'Separatrix power consistency constraint 69 is applied')
    else
         call ocmmnt(outfile, 'Separatrix power consistency constraint 69 is NOT applied')
    end if
    call ovarre(outfile, '. COMPARISON: Separatrix power from main plasma model [MW]','(pdivt.)', pdivt, 'OP ')

    call ovarre(outfile, 'Ratio: psep_kallenbach / Powerup ','(seppowerratio)', seppowerratio)

    call osubhd(outfile, 'Properties of SOL plasma adjacent to divertor sheath :')
    call ovarre(outfile, 'Ion sound speed near target [m/s] ','(cs0)', cs0, 'OP ')
    call ovarre(outfile, 'Plasma density near target [m-3] ','(nel0)', nel0, 'OP ')
    call ovarre(outfile, 'Ion flux density perp to B at target m-2s-1 ','(partfluxtar)', partfluxtar, 'OP ')
    call ovarre(outfile, 'Ion flux density on target [partfluxtar/sinfact]  m-2s-1 ','(ionfluxtarget)', ionfluxtarget, 'OP ')
    call ovarre(outfile, 'Neutral density at target [m-3] ','(neutral_target)', neutral_target, 'OP ')
    call ovarre(outfile, 'Nominal neutral pressure at target [Pa] ','(p0partflux)', p0partflux, 'OP ')
    call ovarre(outfile, 'Plasma temperature near target [eV] ','(ttarget)', ttarget)

    call ovarre(outfile, 'Total plasma pressure near target (thermal+dynamic) [Pa] ','(pressure0)', pressure0, 'OP ')
    call ovarre(outfile, 'momentum factor [-] ','(fmom)', fmom, 'OP ')
    call ovarre(outfile, 'Nominal Chodura sheath width [m] ','(lchodura)', lchodura, 'OP ')

    call osubhd(outfile, 'Divertor target parameters :')
    call ovarre(outfile, 'Angle between flux surface and normal to divertor target [deg]', '(targetangle)', targetangle)
    call ovarre(outfile, 'Pitch angle of field line at target [deg]','(pitch_angle)', pitch_angle, 'OP ')
    call ovarre(outfile, 'Angle between B and the normal to divertor target [deg]','(psi)', psi, 'OP ')

    call ovarre(outfile, 'Ratio: area of flux tube perpendicular to B / target wetted area  ','(sinfact)', sinfact, 'OP ')

    call ovarre(outfile, 'Total power on target [W]','(ptarget_total)', ptarget_total, 'OP ')
    call ovarre(outfile, 'Total power on target [W]','(ptarget_complete)', ptarget_complete, 'OP ')
    call ocmmnt(outfile, 'These should be equal.')

    call ovarre(outfile, 'Power on target due to convection [W]','(ptarget_conv)', ptarget_conv, 'OP ')
    call ovarre(outfile, 'Power on target due to conduction [W]','(ptarget_cond)', ptarget_cond, 'OP ')
    call ovarre(outfile, 'Power on target due to surface recombination [W]','(ptarget_recomb)', ptarget_recomb, 'OP ')
    call ovarre(outfile, 'Power on target due to isotropic losses [W]','(ptarget_isotropic)', ptarget_isotropic, 'OP ')

    call ovarre(outfile, '"Wetted area" of target [m2]','(WettedArea)', WettedArea, 'OP ')
    call ovarre(outfile, '"Wetted length" of target measured in poloidal plane [m]','(WettedLength)', WettedLength, 'OP ')
    call ocmmnt(outfile, 'Alternative calculation:')

    call ovarre(outfile, '"Wetted area" of target [m2]','(WettedAreaComparison)', WettedAreaComparison, 'OP ')
    call ovarre(outfile, '"Wetted length" of target measured in poloidal plane [m]','(WettedLengthComparison)', &
                                                                                      WettedLengthComparison, 'OP ')

    call ovarre(outfile, 'Total power density on target [W/m2]','(qtargetcomplete)', qtargetcomplete)
    call ovarre(outfile, 'Power density on target due to conduction and convection [W/m2]','(qtarget)', qtarget, 'OP ')
    call ovarre(outfile, 'Power density on target due to surface recombination [W/m2]','(qtargetrecomb)', qtargetrecomb, 'OP ')
    call ovarre(outfile, 'Power density on target due to isotropic losses from SOL [W/m2]', &
                         '(qtarget_isotropic)', qtarget_isotropic, 'OP ')
    call ocmmnt(outfile, '(Based on 1/2 x (radiation + CX) in first "sab" of flux line.)')
    call ovarre(outfile, 'Connection length used for "near zone" (m)','(sab)', sab, 'OP ')
    call ovarre(outfile, 'Length of broadened downstream part of SOL [m]','(lengthofwidesol)', lengthofwidesol, 'OP ')

    call osubhd(outfile, 'Integrated powers :')
    call ovarre(outfile, 'Power lost due to impurity radiation [W] ','(impuritypowerlost)', impuritypowerlost, 'OP ')
    call ovarre(outfile, 'Power lost due to hydrogenic radiation [W] ','(hydrogenicpowerlost)', hydrogenicpowerlost, 'OP ')
    call ovarre(outfile, 'Power lost due to charge exchange  [W] ','(exchangepowerlost)', exchangepowerlost, 'OP ')
    call ovarre(outfile, 'Power lost due to electron impact ionisation [W] ','(ionisationpowerlost)', ionisationpowerlost, 'OP ')
    call ovarre(outfile, 'Total power lost due to radiation, ionisation and recombination [W] ',&
                         '(totalpowerlost)', totalpowerlost, 'OP ')
    call ovarre(outfile, 'Power balance error [W] ','(balance)', balance, 'OP ')

    call ocmmnt(outfile, 'The following impurities are used in the divertor model:')
    write(outfile, '(a17, 13a9)')'',(imp_label(i), i=2,14)
    write(outfile, '(a17, 13es9.1)')'Fraction in SOL', (impurity_concs(i), i=2,14)
    write(outfile, '(a17, 13es9.1)')'Enrichment', (impurity_enrichment(i), i=2,14)

  end subroutine divertor_Kallenbach

  ! ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine differential ( t, y, yp )
    !! differential supplies the right hand side of the ODE
    !! author: M Kovari, CCFE, Culham Science Centre
    !! t : input real : T, the independent variable
    !! y : input real : Y(), the dependent variable
    !! yp : output real : YP(), the value of the derivative
    !! differential supplies the right hand side of the ODE
    !! Note that t is only used here because the area is a function of x.
    !! Y(7-10) are the power loss integrals
    !! 

    use read_radiation, only: read_lz
    use read_and_get_atomic_data, only: get_h_rates
    use constants, only: echarge
		use div_kal_vars, only: abserr_sol, netau_sol 
		use divertor_ode_var, only: impurity_concs, imp_label, nimp
    implicit none

    real(dp),intent(in) :: t       ! T, the independent variable
    real(dp),intent(in) :: y(10)   ! Y(), the dependent variable
    real(dp),intent(out) :: yp(10) ! YP(), the value of the derivative

    real(dp):: n01, n02, te, nv, pressure, Power, nv24, bracket
    real(dp):: n, v, n0
    real(dp):: s, al, Rcx, plt, prb
    real(dp):: cxrate,plossdenscx,ionrate1,ionrate2,recrate
    real(dp):: plossion,lz,raddens,radHdens,qperp_total,qperp_conv,qperp_conducted
    real(dp):: A_cross, dpdx, dnvdx, dtdx
    real(dp):: numerator, denominator
    real(dp):: LzTotal      ! Combined weighted radiative loss function
    integer :: i

    ! Rescale to SI units
    n01 = Y(1)*1.d20
    n02 = Y(2)*1.d20
    te  = Y(3)
    nv  = Y(4)*1.d24
    pressure = Y(5) 
    Power = Y(6)*1.d6                             ! Q
    nv24 = Y(4)               ! Ion flux

    bracket = pressure**2 - eightemi48*te*nv24**2 

    if ((bracket .lt. 0.0)) then

        if((bracket .ge. -0.1*pressure**2).and.(t<0.001D0)) then
            ! Continue calculation
            bracket=0.d0
        else
            write(*,*) 'Square root of a negative number in divertor model'
            write(*,'(10a14)') 't', 'pressure', 'te', 'nv24', 'pressure**2', 'eightemi48*te*nv24**2', 'bracket'
            write(*,'(10es14.6)') t, pressure, te, nv24, pressure**2, eightemi48*te*nv24**2, bracket
            stop 1
        endif

    endif

    n = (pressure + sqrt(bracket))/(4.0d0*echarge*te) 

    v = nv/n                ! Plasma velocity
    n0 = n01 + n02            ! neutral density = sum of the two velocity groups

    ! Get total radiative loss function for all impurities present
    LzTotal = 0.0D0
    do i = 2, nimp
        if(impurities_present(i)) then
            lz = read_lz(imp_label(i), te, netau_sol, mean_z=.false., mean_qz=.false., verbose=.false.)
            LzTotal = LzTotal + lz*impurity_concs(i)
        endif
    enddo
    ! impurity radiation loss density
    raddens = LzTotal*n**2

    ! The area of the flux tube, measured perpendicular to B
    ! This is set to a step function as in Kallenbach
    if(t.lt.lengthofwidesol) then
        A_cross = area_target
    else
        A_cross = area_omp
    end if

    qperp_total = Power/A_cross
    ! Convective heat flux is positive
    qperp_conv= -(5.0d0*echarge*Te + 0.5d0*mi*v**2)*nv
    ! conducted heat flux
    qperp_conducted = qperp_total - qperp_conv

    ! Set the atomic rates to zero when the total neutral density is small
    ! Set derivatives to zero when they depend only on rates that are zero.
    if(n0.gt.abserr_sol*1.d18)  then
        call get_h_rates(n, te, s, al, Rcx, plt, prb, aplas, verbose=.false.)
        ! charge exchange rate
        cxrate = Rcx*n*n0
        ! ionisation of neutrals: velocity group 1
        ionrate1 = s * n * n01
        ! ionisation of neutrals: velocity group 2
        ionrate2 = s * n * n02
        ! volume recombination rate
        recrate = al*n**2.0D0
        ! energy conservation: equation 4, charge exchange term
        plossdenscx = echarge*te*cxrate
        ! energy conservation: equation 4, ionisation term
        plossion = (ionrate1 + ionrate2)*elEion
        ! radiation loss density for neutral hydrogenic species
        radHdens = (plt + prb)*n0*n

        ! dn01dx - neutral continuity
        yp(1)=1.d-20*(-ionrate1 + recrate)/v01
        ! dn02dx - neutral continuity
        yp(2)=1.d-20*(-ionrate2)/v02
        ! dnvdx - ion continuity
        dnvdx = ionrate1 + ionrate2 - recrate
        yp(4) =1.d-24*dnvdx
        ! dpressuredx - momentum conservation
        ! Use cxrate instead of Rcx as it may have been set to zero.
        ! Use recrate instead of a1 as it may have been set to zero.
        dpdx = -(cxrate/n + recrate/n)*nv*mi
    else
       ! Fudge to make differential equation behave better
        cxrate=0.0D0
        ionrate1 = 0.0D0
        ionrate2 = 0.0D0
        recrate = 0.0D0
        plossdenscx= 0.0D0
        plossion = 0.0D0
        radHdens = 0.0D0
        ! These derivatives are consequently zero
        dnvdx = 0.0d0
        dpdx = 0.0d0
        yp(1) = 0.0d0
        yp(2) = 0.0d0
        yp(4) = 0.0d0
    endif

    ! Parallel thermal conductivity! Issue #497
    ! Revised formula from Huber and Chankin.
    kappa0 = (8788/zeff_div) * (zeff_div+0.21)/(zeff_div+4.2)
    ! dtedx Equation 5 - thermal conduction equation
    dtdx = qperp_conducted / te**2.5 / kappa0
    yp(3) =dtdx

    ! See K:\Power Plant Physics and Technology\PROCESS\SOL & Divertor\Revised equations for Kallenbach model.docx
    numerator = dpdx - 2.0d0*mi*v*dnvdx - 2.0d0*n*echarge*dtdx
    denominator = 2.0d0*echarge*te - mi*v**2
    !write(*,*) 'denom, product = ', denominator, 2.0d0*echarge*te
    !if(t < 1.0d-4)  then
    !     write(*,*)
    !     write(*,'(10(a18,es12.3))')'t=', t, '  te=',te, '  denominator=',denominator,&
    !              '  numerator', numerator
    !     write(*,'(10(a18,es12.3))') 'dpdx=', dpdx, ' 2.0d0*mi*v*dnvdx', &
    !              2.0d0*mi*v*dnvdx, '2.0d0*n*echarge*dtdx', 2.0d0*n*echarge*dtdx
    !     write(*,'(10(a18,es12.3))') '2.0d0*echarge*te=', 2.0d0*echarge*te, ' mi*v**2', &
    !                       mi*v**2, 'v', v, 'nv',nv
    !end if

    yp(5) =-(cxrate/n + al*n)*nv*mi 

    ! dPowerdx - energy conservation
    yp(6) =1.d-6*(raddens + radHdens + plossdenscx + plossion)*A_cross

    ! Derivatives of the power loss integrals - these are for information only.
    ! They don't affect the results Y(1-6)

    ! Y(7) = integral of impurity radiation loss [MW]
    yp(7) =1.d-6*raddens*A_cross
    ! Y(8) = integral of radiation loss from hydrogenic species [MW]
    yp(8) =1.d-6*radHdens*A_cross
    ! Y(9) = integral of power loss due to charge exchange [MW]
    yp(9) =1.d-6*plossdenscx*A_cross
    ! Y(10)= integral of power loss due to electron impact ionisation [MW]
    yp(10)=1.d-6*plossion*A_cross

    ! The effect of volume recombination on the power balance is not taken into account
    !write(*,'(10(i12))') 1,2,3,4,5,6,7,8,9,10
    !write(*,'(10(es12.3))') y
    !write(*,'(10(es12.3))') yp
    !write(*,'(10(es12.3))') y/yp

    return

  end subroutine differential

  !*****************************************************************************
  !
  !! ODE is the user interface to an ordinary differential equation solver.
  !
  !  Discussion:
  !
  !    ODE integrates a system of NEQN first order ordinary differential
  !    equations of the form:
  !      dY(i)/dT = F(T,Y(1),Y(2),...,Y(NEQN))
  !      Y(i) given at T.
  !    The subroutine integrates from T to TOUT.  On return, the
  !    parameters in the call list are set for continuing the integration.
  !    The user has only to define a new value TOUT and call ODE again.
  !
  !    The differential equations are actually solved by a suite of codes
  !    DE, STEP, and INTRP.  ODE allocates virtual storage in the
  !    arrays WORK and IWORK and calls DE.  DE is a supervisor which
  !    directs the solution.  It calls the routines STEP and INTRP
  !    to advance the integration and to interpolate at output points.
  !
  !    STEP uses a modified divided difference form of the Adams PECE
  !    formulas and local extrapolation.  It adjusts the order and step
  !    size to control the local error per unit step in a generalized
  !    sense.  Normally each call to STEP advances the solution one step
  !    in the direction of TOUT.  For reasons of efficiency, DE integrates
  !    beyond TOUT internally, though never beyond T+10*(TOUT-T), and
  !    calls INTRP to interpolate the solution at TOUT.  An option is
  !    provided to stop the integration at TOUT but it should be used
  !    only if it is impossible to continue the integration beyond TOUT.
  !
  !    On the first call to ODE, the user must provide storage in the calling
  !    program for the arrays in the call list,
  !      Y(NEQN), WORK(100+21*NEQN), IWORK(5),
  !    declare F in an external statement, supply the double precision
  !      SUBROUTINE F ( T, Y, YP )
  !    to evaluate dy(i)/dt = yp(i) = f(t,y(1),y(2),...,y(neqn))
  !    and initialize the parameters:
  !    * NEQN, the number of equations to be integrated!
  !    * Y(1:NEQN), the vector of initial conditions!
  !    * T, the starting point of integration!
  !    * TOUT, the point at which a solution is desired!
  !    * RELERR, ABSERR, the relative and absolute local error tolerances!
  !    * IFLAG, an indicator to initialize the code.  Normal input
  !      is +1.  The user should set IFLAG = -1 only if it is
  !      impossible to continue the integration beyond TOUT.
  !    All parameters except F, NEQN and TOUT may be altered by the
  !    code on output, and so must be variables in the calling program.
  !
  !    On normal return from ODE, IFLAG is 2, indicating that T has been
  !    set to TOUT, and Y has been set to the approximate solution at TOUT.
  !
  !    If IFLAG is 3, then the program noticed that RELERR or ABSERR was
  !    too small! the output values of these variables are more appropriate,
  !    and integration can be resumed by setting IFLAG to 1.
  !
  !    IFLAG is -2 if the user input IFLAG = -1, and the code was able to
  !    reach TOUT exactly.  In that case, the output value of T is TOUT,
  !    and the output value of Y is the solution at TOUT, which was computed
  !    directly, and not by interpolation.
  !
  !    Other values of IFLAG generally indicate an error.
  !
  !    Normally, it is desirable to compute many points along the solution
  !    curve.  After the first successful step, more steps may be taken
  !    simply by updating the value of TOUT and calling ODE again.
  !
  !  Licensing:
  !
  !    This code is distributed under the GNU LGPL license.
  !
  !  Modified:
  !
  !    02 August 2009
  !
  !  Author:
  !
  !    Original FORTRAN77 version by Lawrence Shampine, Marilyn Gordon.
  !    FORTRAN90 version by John Burkardt.
  !
  !  Reference:
  !
  !    Lawrence Shampine, Marilyn Gordon,
  !    Computer Solution of Ordinary Differential Equations:
  !    The Initial Value Problem,
  !    Freeman, 1975,
  !    ISBN: 0716704617,
  !    LC: QA372.S416.
  !
  !  Parameters:
  !
  !    Input, external F, the name of a user-supplied routine of the form
  !      subroutine f ( t, y, yp )
  !      real ( kind = 8 ) t
  !      real ( kind = 8 ) y(neqn)
  !      real ( kind = 8 ) yp(neqn)
  !    which accepts input values T and Y(1:NEQN), evaluates the right hand
  !    sides of the ODE, and stores the result in YP(1:NEQN).
  !
  !    Input, integer ( kind = 4 ) NEQN, the number of equations.
  !
  !    Input/output, real ( kind = 8 ) Y(NEQN), the current solution.
  !
  !    Input/output, real ( kind = 8 ) T, the current value of the independent
  !    variable.
  !
  !    Input, real ( kind = 8 ) TOUT, the desired value of T on output.
  !
  !    Input, real ( kind = 8 ) RELERR, ABSERR, the relative and absolute error
  !    tolerances.  At each step, the code requires
  !      abs ( local error ) <= abs ( y ) * relerr + abserr
  !    for each component of the local error and solution vectors.
  !
  !    Input/output, integer ( kind = 4 ) IFLAG, indicates the status of
  !    integration.  On input, IFLAG is normally 1 (or -1 in the special case
  !    where TOUT is not to be exceeded.)  On normal output, IFLAG is 2.  Other
  !    output values are:
  !    * 3, integration did not reach TOUT because the error tolerances
  !      were too small.  But RELERR and ABSERR were increased appropriately
  !      for continuing!
  !    * 4, integration did not reach TOUT because more than 500 steps were taken!
  !    * 5, integration did not reach TOUT because the equations appear to
  !      be stiff!
  !    * 6, invalid input parameters (fatal error).
  !    The value of IFLAG is returned negative when the input value is
  !    negative and the integration does not reach TOUT.
  !
  !    Input/output, real ( kind = 8 ) WORK(100+21*NEQN), workspace.
  !
  !    Input/output, integer ( kind = 4 ) IWORK(5), workspace.
  !
  !---------------------------------------------------------------------------

end module divertor_ode
