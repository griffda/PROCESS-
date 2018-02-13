! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module divertor_ode
  !+ad_name  divertor_ode
  !+ad_summ  Module containing divertor Kallenbach model
  !+ad_type  Module
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_args  N/A
  !+ad_desc  This module contains the PROCESS Kallenbach divertor model
  !+ad_prob  None
  !+ad_hist  25/01/17 MDK  Initial version of module
  !+ad_stat  Okay
  !+ad_docs

  use global_variables
  use maths_library
  use read_and_get_atomic_data
  use impurity_radiation_module, only: nimp, imp_label, impurity_arr
  use process_output, only: oblnkl,obuild, ocentr, ocmmnt, oheadr, osubhd, ovarin, ovarre, ovarrf, ovarst
  use constants
  use process_input, only: lower_case
  use divertor_kallenbach_variables, only: neratio, pressure0, fractionwidesol, fmom, totalpowerlost, impurity_enrichment, &
             lambda_q_omp, target_spread, hydrogenicpowerlost, impuritypowerlost, exchangepowerlost, ionisationpowerlost
  use build_variables, only: rspo
  use physics_variables, only:  tesep_keV => tesep

  implicit none

  ! Module-level declarations !
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  logical, public, save :: impurities_present(nimp)=.false.

  ! impurity element name - temporary

  ! impurity concentrations in divertor zone
  real(kind(1.0D0)), public :: impurity_concs(nimp)

  ! relative ion mass
  ! Issue #501: change from 2 to 2.5
  real(kind(1.0D0)), private :: aplas=2.5D0

  ! ion mass [kg]
  real(kind(1.0D0)), private :: mi

  ! conversion from flux density [el/s] to Pascal [Molecules]
  ! see page 6 of paper.
  real(kind(1.0D0)), private :: fluxdens_to_pa= 1.0D0/1.55e23

  ! Useful values (combinations of other constants/variables)
  real(kind(1.0D0)), private :: eightemi, eightemi48,  elEion
  real(kind(1.0D0)), private, parameter :: degree=pi/180.0D0
  real(kind(1.0D0)), parameter :: ln10=log(10.0D0)

  ! "non-coronal parameter" for radiative loss function [ms.1e20/m3]
  real(kind(1.0D0)), private :: netau

  ! constant in thermal conductivity (equation 5) [J/(s m eV^7/2)]
  real(kind(1.0D0)), private :: kappa0=2390.0D0

  ! neutral velocity along the flux bundle, groups 1 & 2 [m/s]
  real(kind(1.0D0)), private :: v01, v02

  ! Allowable absolute/relative error
  real(kind(1.0D0)), private :: abserr, relerr

  ! Circumference of plasma at outboard midplane and at target [m]
  real(kind(1.0D0)), private :: circumference_omp, circumference_target

  ! Circumference of normal to helical field line [m]
  real(kind(1.0D0)), private :: circumf_bu

  ! Flux bundle area perp. to B at target and at omp [m2]
  real(kind(1.0D0)), private :: area_target, area_omp

  ! Zeff for divertor region
  real(kind(1.0D0)) :: zeff_div

  ! SOL radial thickness extrapolated to OMP [m]
  real(kind(1.0D0)), private :: sol_broadening

  ! Ratio: psep_kallenbach / Powerup
  real(kind(1.0D0)), private, parameter :: seppowerratio=2.3D0

  real(kind(1.0D0)), private :: lengthofwidesol

contains

  subroutine divertor_Kallenbach(rmajor,rminor,bt,plascur,bvert,q,verboseset,     &
             ttarget,qtargettotal,targetangle,lcon_factor,netau_in,&
             unit_test,abserrset,  &
             bp, &
             psep_kallenbach, teomp, neomp,  &
             outfile,iprint)

    !+ad_name  divertor_Kallenbach
    !+ad_summ  calculate radiative loss and plasma profiles along a flux tube including momentum losses
    !+ad_type  subroutine
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Calculate radiative loss and plasma profiles along a flux tube including momentum losses.
    !+ad_desc  Description in A. Kallenbach et al., PPCF 58 (2016) 045013, this version based on that
    !+ad_desc  sent by Arne 17.5.2016.
    !+ad_desc  Note this solver is not suitable for stiff equations - which these are. Instead I have
    !+ad_desc  set the neutral density derivatives to zero when the neutral density is small.
    !+ad_stat  Okay
    !+ad_docs  https://people.sc.fsu.edu/~jburkardt/f_src/ode/ode.html
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use ode_mod , only :            ode
    use numerics, only :            active_constraints
    use physics_variables, only :   nesep, pdivt

    implicit none

    logical::verbose
    logical,save::firstcall=.true.
    logical, intent(in) :: verboseset
    logical, optional, intent(in) :: unit_test

    ! Output file unit identifier
    integer, intent(in) :: outfile

    ! Print flag
    integer, intent(in) :: iprint

    ! Plasma major/minor radius [m]
    real(kind(1.0D0)), intent(in) :: rmajor, rminor

    ! Toroidal field on-axis [T]
    real(kind(1.0D0)), intent(in) :: bt

    ! Plasma current [A]
    real(kind(1.0D0)), intent(in) :: plascur

    ! Vertical field at plasma [T]
    real(kind(1.0D0)), intent(in) :: bvert

    ! Plasma safety factor near edge
    real(kind(1.0D0)), intent(in) :: q

    ! Target temperature input [eV]
    real(kind(1.0D0)), intent(in) :: ttarget

    ! qtargettotal : Power density on target including surface recombination [W/m2]
    real(kind(1.0D0)),intent(in) :: qtargettotal

    !+ad_vars  qtargetcomplete : Total power density on target [W/m2]
    real(kind(1.0D0)) :: qtargetcomplete

    ! target angle [deg]
    real(kind(1.0D0)),intent(in) :: targetangle

    ! target angle [deg]
    real(kind(1.0D0)),intent(in) :: lcon_factor

    ! connection length from omp to target [m]
    real(kind(1.0D0)) :: lcon

    ! "non-coronal parameter" for radiative loss function [ms.1e20/m3]
    real(kind(1.0D0)), intent(in) :: netau_in

    ! Absolute error input
    real(kind(1.0D0)), optional, intent(in) :: abserrset

    ! Average poloidal field (T)
    real(kind(1.0D0)), optional, intent(in) :: bp

    ! Power conducted through the separatrix, calculated by divertor model [W]
    real(kind(1.0D0)), optional, intent(out) :: psep_kallenbach

    ! Temperature (eV) and density (1/m3) at outboard midplane
    real(kind(1.0D0)), optional, intent(out) :: teomp, neomp

    ! Btheta, Bphi and Btotal at OMP (equation 1 of Kallenbach)
    real(kind(1.0D0)) :: Bp_omp, Bt_omp, Btotal_omp

    ! Field due to plasma current [T]
    real(kind(1.0D0)) :: BpPlasmaOmp

    ! Kallenbach equation 1 terms
    real(kind(1.0D0)) :: bu

    ! ion flux density into target [m-2.s-1]
    real(kind(1.0D0)) :: partfluxtar

    ! electron energy loss due to ionization [eV]
    real(kind(1.0D0)) :: Eion

    ! kinetic energy for the first group of neutrals [eV]
    real(kind(1.0D0)) :: Eneutrals

    ! neutral velocity along the flux bundle, group 1 [m/s]
    real(kind(1.0D0)) :: v0

    ! Electron sheath energy transmission coefficient
    real(kind(1.0D0)), parameter :: gammae = 5.5d0
    ! Ion energy reflection coefficient
    real(kind(1.0D0)), parameter :: energyreflection = 0.5d0
    ! Ion sheath energy transmission coefficient
    ! Issue #500 item 3. Adjust sheath heat transmission coefficient for ion reflection.
    real(kind(1.0D0)), parameter :: gammai = 2.5d0 * (1d0 - energyreflection)
    ! Sheath energy transmission coefficient (kallenbach paper pg. 4)
    real(kind(1.0D0)), parameter :: gammasheath = gammae + gammai

    ! Recombination energy [eV]
    real(kind(1.0D0)) :: Erecomb

    ! Volume recombination energy (assumed to be lost as radiation) [eV]
    real(kind(1.0D0)) :: Evolrec

    ! Ion sound speed near target [m/s]
    real(kind(1.0D0)) :: cs0

    ! Nominal neutral pressure at target [Pa]
    real(kind(1.0D0)) :: p0partflux

    ! Target area to target wetted area factor
    real(kind(1.0D0)) :: sinfact

    ! Relative distribution between 2 neutral particle classes
    real(kind(1.0D0)) :: neutfrac1, neutfrac2

    ! factor by which neutral penetration is enhanced
    real(kind(1.0D0)) :: neutpenfact2

    ! neutral density in group 1, 2
    real(kind(1.0D0)) :: n010, n020

    ! electron temperature [eV]
    real(kind(1.0D0)) :: te0

    ! n*v [1/m2.s]
    real(kind(1.0D0)) :: nv0

    ! Total power running along SOL [W]
    real(kind(1.0D0)) :: Power0

    ! impurity element name - temporary
    character(len=2) :: element='**'

    ! Poloidal, toroidal and total field at target
    real(kind(1.0D0)) :: Bp_target, Bt_target, Btotal_target, pitch_angle, sin_pitch_angle
    real(kind(1.0D0)) :: poloidal_flux_expansion

    !+ad_vars  ptarget_conv : power deposited on target by convection [W]
    real(kind(1.0D0)) ::ptarget_conv
    !+ad_vars  ptarget_cond : power deposited on target by conduction [W]
    real(kind(1.0D0)) ::ptarget_cond
    !+ad_vars  ptarget_recomb : power deposited on target by recombination of hydrogenic ions [W]
    real(kind(1.0D0)) ::ptarget_recomb
    real(kind(1.0D0)) ::ptarget_isotropic
    real(kind(1.0D0)) ::ptarget_total
    real(kind(1.0D0)) ::ptarget_complete
    real(kind(1.0D0)) ::neutral_target


    ! ODE solver parameters
    !!!!!!!!!!!!!!!!!!!!!!!!

    ! Number of steps along the 1D line
    integer(kind=4), parameter :: step_num = 200

    ! NEQN, the number of coupled  differential equations to be solved
    integer(kind=4), parameter :: neqn = 10

    integer(kind=4) :: i, step
    integer(kind=4) :: iflag
    integer(kind=4) :: iwork(5)

    ! First step [m]
    real(kind(1.0D0)) :: step0=0.0001

    ! Ratio between successive step sizes
    real(kind(1.0D0)) :: factor

    real(kind(1.0D0)) :: x
    real(kind(1.0D0)) :: xout
    real(kind(1.0D0)) :: work(100+21*neqn)
    real(kind(1.0D0)) :: y(neqn)

    ! These are local variables for recalculation at each data point - not used for integration:
    real(kind(1.0D0)) :: nel, v, n0, nelsquared
    real(kind(1.0D0)) :: s, al, cx, plt, prb
    real(kind(1.0D0)) :: cxrate, plossdenscx, ionrate1, ionrate2, recrate
    real(kind(1.0D0)) :: plossion, lz, raddens, radHdens, qperp_total, qperp_conv, qperp_conducted
    real(kind(1.0D0)) :: n01, n02, te, nv, pressure, Power, nv24, bracket, nel20, Pthermal, n0e20
    real(kind(1.0D0)) :: cs, mach

    !+ad_vars  A_cross : Area of flux bundle perpendicular to B
    real(kind(1.0D0)) :: A_cross

    !+ad_vars  lambda_q_target : SOL radial thickness at the target, mapped to OMP [m]
    real(kind(1.0D0)) :: lambda_q_target

    !+ad_vars  Powerup : upstream power [W]
    real(kind(1.0D0)) :: Powerup

    !+ad_vars  balance : Power balance error - should be zero [W]
    real(kind(1.0D0)) :: balance

    ! Plasma thermal pressure
    real(kind(1.0D0)) :: nete

    !+ad_vars  nete0 : Plasma thermal pressure near target
    real(kind(1.0D0)) :: nete0

    !+ad_vars  nel0 : Electron density near target [m-3]
    real(kind(1.0D0)) :: nel0

    !+ad_vars  qtarget : Power density on target not including surface recombination [W/m2]
    real(kind(1.0D0)) :: qtarget

    !+ad_vars  qtarget_isotropic : Power density on target due to isotropic losses from SOL [W/m2]
    real(kind(1.0D0)) :: qtarget_isotropic

    !+ad_vars  receiving_area : Area on which the power due to isotropic losses from SOL energy is incident [m2]
    real(kind(1.0D0)) :: receiving_area

    ! Mean charge and quadratic mean charge from ADAS
    real(kind(1.0D0)) :: z, qz

    ! Power density on target due to surface recombination [W/m2]
    real(kind(1.0D0)) :: qtargetrecomb

    ! Total power on target [W]
    real(kind(1.0D0)) :: powertargettotal



    !+ad_vars  WettedArea : Wetted area of target [m2]
    real(kind(1.0D0)) :: WettedArea

    !+ad_vars  WettedLength : Wetted length on target measured in poloidal plane [m]
    real(kind(1.0D0)) :: WettedLength

    !+ad_vars  sab : connection length used for calculating the "near zone" [m]
    real(kind(1.0D0)) :: sab
    !+ad_vars  romp : Major radius at outer midplane  [m]
    real(kind(1.0D0)) :: romp
    !+ad_vars  IonFluxTarget : Ion flux density on target [m-2s-1]
    real(kind(1.0D0)) :: IonFluxTarget
    ! Typical SOL temperature, used only for estimating zeffective in the SOL [eV]
    real(kind(1.0D0)) :: ttypical
    ! Impurity radiation by species
    real(kind(1.0D0)) :: raddensspecies(nimp)=0.0d0

    ! Chodura sheath width [m]
    real(kind(1.0D0)) :: lchodura
    real(kind(1.0D0)),parameter :: squareroot6 = sqrt(6d0)

    character(len=100) :: filename
    ! Angle between B and the surface normal (deg)
    real(kind(1.0D0)) :: psi

    ! Major radius at outer midplane [m]
    romp = rmajor + rminor

    ! Typical SOL temperature, used only for estimating zeffective in the SOL [eV]
    ttypical=1000.0d0*tesep_keV/2.0d0
    ! B theta at OMP
    Bt_omp = - bt*rmajor/romp
    ! B theta at target
    ! On the first iteration rspo may still have its initial value of zero:
    if(rspo.lt.1.0d0) rspo = rmajor
    Bt_target = - bt*rmajor/rspo

    ! Cylindrical approximation for field due to plasma current
    BpPlasmaOmp = rmu0 * plascur / (2.0D0*pi*rminor)

    ! At the OMP the vertical field generated by the equilibrium field coils
    ! (assumed approximately constant), adds to the field generated by the plasma.
    Bp_omp = BpPlasmaOmp + abs(bvert)
    ! At the target the poloidal field is much smaller, as it is near the X-point.
    ! Issue #500 item 6:
    Bp_target = 0.35d0 * bp / 0.956d0

    Btotal_omp = sqrt(Bp_omp**2 + Bt_omp**2)
    Btotal_target = sqrt(Bp_omp**2 + Bt_target**2)

    ! Sine of pitch angle
    sin_pitch_angle = abs(Bp_target / Btotal_target)
    ! Ratio: target area perp to B / target wetted area
    sinfact = 1.0D0 / (sin(targetangle*degree) * sin_pitch_angle)
    ! Angle between B and the surface normal (deg)
    psi = 90.0d0 - acos(1.0d0/sinfact)/degree

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

    ! Set netau to initial input value
    netau = netau_in

    if (verbose) then
        write(*,*) 'subroutine divertor_Kallenbach'
    end if

    ! ion mass [kg]
    mi = umass*aplas

    ! RELERR, ABSERR, the relative and absolute local error tolerances
    ! At each step, the code requires
    !      abs ( local error ) <= abs ( y ) * relerr + abserr
    ! for each component of the local error and solution vectors.
    ! abserr is also used to cut off reactions involving neutrals at n0=abserr*1e20 m-3
    abserr = 1.0D-5
    relerr = 0.0D0

    ! Set absolute error level if input
    if (present(abserrset)) then
        abserr = abserrset
    end if

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
    bu = sin(atan(abs(Bp_omp/Bt_omp)))

    ! Actual plasma circumference at OMP and target [m]
    circumference_omp = 2.0D0*pi*romp
    circumference_target = 2.0D0*pi*rspo

    ! Circumference measured normal to B at OMP [m]
    circumf_bu = circumference_omp*bu
    ! Flux bundle area perp. to B at omp [m2]
    area_omp = circumf_bu * lambda_q_omp

    ! neutral velocity along the flux bundle, group 1 [m/s]
    v0 = 0.25D0 * sqrt(8.0D0/pi * echarge*Eneutrals / mi)
    ! Rename this for consistency
    v01 = v0
    ! neutral velocity along the flux bundle, group 2 [m/s]
    v02 = v0*neutpenfact2

    ! Sound speed [m/s]
    cs0 = sqrt(2.0D0*echarge*ttarget/mi)

    ! Chodura sheath width (m).  Sin psi taken = 1.
    ! Stangeby Equation 2.112
    lchodura = squareroot6 * ttarget / (Btotal_target*cs0)

    ! Flux bundle area perp. to B at target [m2]
    area_target = area_omp * sol_broadening
    ! Wetted area on target [m2]
    WettedArea= area_target*sinfact
    ! Wetted length on target measured in poloidal plane [m]
    WettedLength = WettedArea/circumference_target

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
    nel0 = qtarget*sinfact/(gammasheath*echarge*ttarget*cs0)

    ! Ion flux density perp to B at target [m-2.s-1]
    partfluxtar = nel0*cs0

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
            z = read_lz(element,ttypical,netau, mean_z=.true., mean_qz=.false., verbose=.false.)
            qz = read_lz(element,ttypical,netau, mean_z=.false., mean_qz=.true., verbose=.false.)
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
    nv0 = -nel0*cs0

    ! Pressure  (Kallenbach paper equation 6) [Pa]
    pressure0 = mi*nv0**2.0D0 / nel0 + nel0*2.0D0*echarge*te0

    ! n*T [eV.m-3]
    nete0 = nel0*te0

    ! Total power running along SOL [W]
    Power0 = qtarget*WettedArea

    ! Power deposited on target by recombination of hydrogenic ions [W]
    ptarget_recomb = Erecomb*echarge*nel0*cs0*area_target

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
    y(1) = n010*1.d-20
    y(2) = n020*1.d-20
    y(3) = te0
    y(4) = nv0*1.d-24
    y(5) = pressure0
    y(6) = Power0/1.d6
    y(7) =0.0D0           ! Y(7) = integral of impurity radiation loss [MW]
    y(8) =0.0D0           ! Y(8) = integral of radiation loss from hydrogenic species [MW]
    y(9) =0.0D0           ! Y(9) = integral of power loss due to charge exchange [MW]
    y(10)=0.0d0           ! Y(10)= integral of power loss due to electron impact ionisation [MW]

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
        write(9,*) 'Y(7)_=_integral_of_radiation_loss_from_impurities_[MW]'
        write(9,*) 'Y(8)_=_integral_of_radiation_loss_from_hydrogenic_species_[MW]'
        write(9,*) 'Y(9)_=_integral_of_power_loss_due_to_charge_exchange_[MW]'
        write(9,*) 'Y(10)=_integral_of_power_loss_due_to_electron_impact_ionisation_[MW]'
        write(9,*) 'qconv=_Convected_power_density_through_a_surface_perpendicular_to_B_[W/m2]'
        write(9,*) 'qcond=_Conducted_power_density_through_a_surface_perpendicular_to_B_[W/m2]'
        write(9,*) 'im_rad=_Impurity_radiation_[W/m2]'
        write(9,'(a5, 2x, a9, 47a12)')  &
              'step', 'x//B_[m]', 'te_[eV]', 'ne/1e20/m3', 'Pth_[Pa]', 'Ptotal_[Pa]', &
              'v_[m/s]', 'mach ',                                                     &
              'n0/1e20/m3', 'Power_[W]', 'perp_area', 'qtot_W/m2', 'qconv_W/m2', 'qcond_W/m2',         &
              'CX_W/m3', 'Ion_W/m3' , 'H_rad_W/m3', 'im_rad_W/m3', 'Y(7)', 'Y(8)', 'Y(9)', 'Y(10)',    &
              (impurity_arr(i)%Label, i=2,nimp),'n01/1e20/m3','n02/1e20m-3','nv24','v/ms-1'
        !open(unit=10, file='divertor_diagnostics.txt', status='replace')
     endif

    do step = 0, step_num+1
        if(step.ne.0) then

            ! Solve the set of differential equations
            call ode(differential, neqn, y, x, xout, relerr, abserr, iflag, work, iwork )

            ! Logarithmic step along SOL (x)
            xout = xout*factor

            if ( iflag /= 2 ) then
                write ( *, '(a)' ) ' '
                write ( *, '(a)' ) 'Differential equation solver in divertor model has failed:'
                write(*,*) 'ODE.F90, called by subroutine sol_ode in divertor_ode.f90'
                write ( *, '(a,i8)' ) '  ODE returned IFLAG = ', iflag
                write(*,*) 'If iflag = 3, integration did not reach TOUT because the error tolerances were too small.'
                write(*,*) 'If iflag = 4, integration did not reach TOUT because more than 500 steps were taken.'
                write(*,*) 'If iflag = 5, integration did not reach TOUT because the equations appear to be stiff.'
                write(*,*) 'If iflag = 6, invalid input parameters.'
                write(*,*) 'Step number = ',step, '  x = ', x
            endif
        endif

        ! Derived quantities need to be recalculated at each data point
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
        nel = (pressure + sqrt(bracket))/(4.0D0*echarge*te)
        nel20 = nel/1.d20
        nelsquared = nel**2.0D0

        ! Thermal power [W]
        Pthermal = 2.0D0*nel*Te*echarge

        v = nv/nel

        ! Neutral density = sum of the two velocity groups [1e20.m-3]
        n0e20 = Y(1) + Y(2)

        ! Heat flux perpendicular to flux tube cross section [W/m2]
        qperp_total = Power/A_cross

        cs = sqrt(2.0D0*echarge*Te/mi)
        mach = -v/cs
        nete = nel*te

        ! Convective heat flux is positive [W/m2]
        qperp_conv= -(5.0D0*echarge*Te + 0.5D0*mi*v**2.0D0)*nv

        ! Conducted heat flux [W/m2]
        qperp_conducted = qperp_total - qperp_conv

        ! Neutral density = sum of the two velocity groups [m-3]
        n0 = n01 + n02

        ! If the neutral density is small, set the atomic rates to zero.
        ! This adjustment is used in subroutine differential to make it behave better (less stiff)
        if(n0.gt.abserr*1.d18) then
            call get_h_rates(nel, te, s, al, cx, plt, prb, aplas, verbose=.false.)

            ! charge exchange rate for equation 7 [s-1]
            cxrate = cx*nel*n0

            ! ionisation rate of neutrals: velocity group 1 [s-1]
            ionrate1 = s * nel * n01

            ! ionisation rate of neutrals: velocity group 2 [s-1]
            ionrate2 = s * nel * n02

            ! recombination rate of ions and electrons [s-1]
            recrate = al*nelsquared

            ! energy conservation: equation 4, charge exchange term
            plossdenscx = echarge*te*cxrate

            ! energy conservation: equation 4, ionisation term
            plossion = (ionrate1 + ionrate2)*elEion

            ! radiation loss density for neutral hydrogenic species
            radHdens = (plt + prb)*n0*nel

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

       do i = 1, nimp
            if(impurities_present(i)) then
                lz = read_lz(imp_label(i), te, netau, mean_z=.false., mean_qz=.false., verbose=.false.)
                ! MDK Store species-specific radiation data
                raddensspecies(i) = lz*impurity_concs(i)*nelsquared
                ! impurity radiation loss density
                raddens = raddens + raddensspecies(i)
            endif
        enddo

        if(iprint.eq.1) then
            ! do i=2,nimp
            !     if(raddensspecies(i)<1.0d-99)raddensspecies(i)=0.0d0
            ! end do

            write(9,'(i5, 2x, f9.5, 46es12.3)')  &
                step, x, te, nel20, Pthermal, pressure, v, mach, n0e20, Power, A_cross, qperp_total,  &
                qperp_conv, qperp_conducted,plossdenscx, plossion, radHdens, raddens,                 &
                y(7), y(8), y(9), y(10), (raddensspecies(i), i=2,nimp),y(1),y(2),nv24,v
        end if

        ! Note when we reach the edge of the "near zone" (connection length = sab):
        ! At this point we use split the integrated emission: half towards the target
        ! Y(9)_=_integral_of_power_loss_due_to_charge_exchange_[MW]
        if((x.lt.sab).and.(xout.gt.sab)) then
            qtarget_isotropic = 0.5D0 * 1e6 * (y(7) + y(8) + y(9)*(1d0-energyreflection)) / (2.0D0*WettedArea)
        endif

        ! Store some target parameters
        if(step==0)then
            ptarget_conv = qperp_conv * A_cross
            ptarget_cond = qperp_conducted * A_cross
            ! Neutral density at targetangle [m-3]
            neutral_target = n0e20 * 1d20
        endif
    end do

    ptarget_isotropic = qtarget_isotropic * WettedArea
    ! This total is for checking only
    ptarget_total = ptarget_conv + ptarget_cond + ptarget_recomb + ptarget_isotropic

    if(verbose) then
        write(*,*)'Differential equations complete'
    end if


    ! Upstream power [W]
    Powerup = Power

    ! Power conducted through the separatrix, calculated by divertor model [W]
    psep_kallenbach = seppowerratio*Powerup

    ! Power balance - should be zero [W]
    balance = (Y(6) - Y(7)-Y(8)-Y(9)-Y(10))*1.d6 - Power0

    ! momentum factor [-]
    fmom = 2.0D0*nete0/nete

    ! Total power lost due to radiation, ionisation and recombination [W]
    totalpowerlost =  (Y(7)+Y(8)+Y(9)+Y(10))*1.0e6

    ! Ion flux density on target [m-2s-1]
    IonFluxTarget = partfluxtar/sinfact

    ! Plasma temperature at outer midplane [eV]
    teomp = te

    ! Plasma density at outer midplane [m-3]
    neomp = nel

    ! Output !
    !!!!!!!!!!

    if(iprint.eq.0) then
        return
    end if

    ! Close divertor output and diagnostic .txt files
    close(unit=9)
    close(unit=10)

    ! Output to OUT.DAT !
    !!!!!!!!!!!!!!!!!!!!!

    pitch_angle = asin(sin_pitch_angle)/degree
    poloidal_flux_expansion = Bp_omp / Bp_target
    qtargetcomplete = qtargettotal + qtarget_isotropic
    ! Just to check
    ptarget_complete = qtargetcomplete * WettedArea

    call oheadr(outfile, 'Divertor: Kallenbach 1D Model')
    call ocmmnt(outfile, 'For graphical output use kallenbach_plotting.py')

    call osubhd(outfile, 'Global SOL properties :')
    call ovarre(outfile, 'Connection length:  [m]','(lcon)', lcon, 'OP ')
    call ovarre(outfile, 'Parameter for approach to local equilibrium  [ms.1e20/m3]','(netau)', netau)
    call ovarre(outfile, 'Typical SOL temperature, used only for estimating zeff_div [eV] ','(ttypical)', ttypical, 'OP ')
    call ocmmnt(outfile, 'The zeff_div is used only for estimating thermal conductivity of the SOL plasma.')
    call ovarre(outfile, 'Z effective [W] ','(zeff_div)', zeff_div, 'OP ')

    call osubhd(outfile, 'Properties of SOL plasma :')
    call ovarre(outfile, 'SOL power fall-off length at the outer midplane [m]','(lambda_q_omp)', lambda_q_omp)
    call ovarre(outfile, 'SOL radial thickness at the target, mapped to OMP [m]','(lambda_q_target)', lambda_q_target)
    call ovarre(outfile, 'SOL area (normal to B) at outer midplane [m2]','(area_omp)', area_omp, 'OP ')
    call ovarre(outfile, 'SOL area (normal to B) at target [m2]','(area_target)', area_target, 'OP ')
    call ovarre(outfile, 'Poloidal flux expansion: Bp(OMP)/Bp(target)','(poloidal_flux_expansion)', poloidal_flux_expansion, 'OP ')
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
    call ovarre(outfile, 'Ion flux density on target [partfluxtar/sinfact]  m-2s-1 ','(IonFluxTarget)', IonFluxTarget, 'OP ')
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

    call ovarre(outfile, 'Total power density on target [W/m2]','(qtargetcomplete)', qtargetcomplete)
    call ovarre(outfile, 'Power density on target due to conduction and convection [W/m2]','(qtarget)', qtarget, 'OP ')
    call ovarre(outfile, 'Power density on target due to surface recombination [W/m2]','(qtargetrecomb)', qtargetrecomb, 'OP ')
    call ovarre(outfile, 'Power density on target due to isotropic losses from SOL [W/m2]', &
                         '(qtarget_isotropic)', qtarget_isotropic, 'OP ')
    call ocmmnt(outfile, '(Based on 1/2 x (radiation + CX) in first "sab" of flux line.)')
    call ovarre(outfile, 'Connection length used for "near zone" (m)','(sab)', sab, 'OP ')
    call ovarre(outfile, 'Length of broadened downstream part of SOL [m]','(lengthofwidesol)', lengthofwidesol, 'OP ')

    call osubhd(outfile, 'Integrated powers :')
    impuritypowerlost = Y(7)*1.d6
    hydrogenicpowerlost = Y(8)*1.d6
    exchangepowerlost = Y(9)*1.d6
    ionisationpowerlost = Y(10)*1.d6
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
    !+ad_name  differential
    !+ad_summ  differential supplies the right hand side of the ODE
    !+ad_type  subroutine
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  t : input real : T, the independent variable
    !+ad_args  y : input real : Y(), the dependent variable
    !+ad_args  yp : output real : YP(), the value of the derivative

    !+ad_desc  differential supplies the right hand side of the ODE
    !+ad_desc  Note that t is only used here because the area is a function of x.
    !+ad_desc  Y(7-10) are the power loss integrals
    !+ad_prob  None
    !+ad_hist  01/02/17 MDK  Initial version
    !+ad_stat  Okay
    !+ad_docs
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer :: i

    ! T, the independent variable
    real(kind(1.0D0)),intent(in) :: t

    ! Y(), the dependent variable
    real(kind(1.0D0)),intent(in) :: y(10)

    ! YP(), the value of the derivative
    real(kind(1.0D0)),intent(out) :: yp(10)

    !
    real(kind(1.0D0)):: n01, n02, te, nv, pressure, Power, nv24, bracket
    real(kind(1.0D0)):: nel, v, n0
    real(kind(1.0D0)):: s, al, cx, plt, prb
    real(kind(1.0D0)):: cxrate,plossdenscx,ionrate1,ionrate2,recrate
    real(kind(1.0D0)):: plossion,lz,raddens,radHdens,qperp_total,qperp_conv,qperp_conducted
    real(kind(1.0D0)):: A_cross

    ! Combined weighted radiative loss function
    real(kind(1.0D0))::LzTotal

    ! Set initial values
    n01 = Y(1)*1.d20
    n02 = Y(2)*1.d20
    te  = Y(3)
    nv  = Y(4)*1.d24
    pressure = Y(5)
    Power   = Y(6)*1.d6

    ! Derived quantities
    nv24 = Y(4)
    bracket = pressure**2 - eightemi48*te*nv24**2

    if ((bracket .lt. 0.0)) then

        if((bracket .ge. -0.1*pressure**2).and.(t<0.001D0)) then
            ! Continue calculation
            bracket=0.d0
        else
            write(*,*) 'Square root of a negative number in divertor model'
            write(*,'(10a14)') 't', 'pressure', 'te', 'nv24', 'pressure**2', 'eightemi48*te*nv24**2', 'bracket'
            write(*,'(10es14.6)') t, pressure, te, nv24, pressure**2, eightemi48*te*nv24**2, bracket
            stop
        endif

    endif

    nel = (pressure + sqrt(bracket))/(4.0d0*echarge*te)
    v = nv/nel
    n0 = n01 + n02                         ! neutral density = sum of the two velocity groups

    if(n0.gt.abserr*1.d18)  then
        call get_h_rates(nel, te, s, al, cx, plt, prb, aplas, verbose=.false.)

        ! charge exchange for equation 7
        cxrate = cx*nel*n0
        ! ionisation of neutrals: velocity group 1
        ionrate1 = s * nel * n01
        ! ionisation of neutrals: velocity group 2
        ionrate2 = s * nel * n02
        ! recombination of ions and electrons
        recrate = al*nel**2.0D0
        ! energy conservation: equation 4, charge exchange term
        plossdenscx = echarge*te*cxrate
        ! energy conservation: equation 4, ionisation term
        plossion = (ionrate1 + ionrate2)*elEion
        ! radiation loss density for neutral hydrogenic species
        radHdens = (plt + prb)*n0*nel
    else
        ! Fudge to make it behave better (less stiff)
        cxrate=0.0D0
        ionrate1 = 0.0D0
        ionrate2 = 0.0D0
        recrate = 0.0D0
        plossdenscx= 0.0D0
        plossion = 0.0D0
        radHdens = 0.0D0
    endif

    ! Get radiative loss function for all impurities present
    LzTotal = 0.0D0

    do i = 2, nimp
        if(impurities_present(i)) then
            lz = read_lz(imp_label(i), te, netau, mean_z=.false., mean_qz=.false., verbose=.false.)
            LzTotal = LzTotal + lz*impurity_concs(i)
        endif
    enddo

    ! impurity radiation loss density
    raddens = LzTotal*nel**2.0D0

    ! The area of the flux tube, measured perpendicular to B
    ! This is set to a step function as in Kallenbach
    if(t.lt.lengthofwidesol) then
        A_cross = area_target
    else
        A_cross = area_omp
    end if

    qperp_total = Power/A_cross

    ! Convective heat flux is positive
    qperp_conv= -(5.0d0*echarge*Te + 0.5*mi*v**2)*nv

    ! conducted heat flux
    qperp_conducted = qperp_total - qperp_conv

    ! Derivatives

    ! dn01dx Equation 2 - neutral continuity
    yp(1)=1.d-20*(-ionrate1 + recrate)/v01

    ! dn02dx Equation 2 - neutral continuity
    yp(2)=1.d-20*(-ionrate2)/v02

    ! Parallel thermal conductivity! Issue #497
    ! Revised formula from Huber and Chankin.
    kappa0 = (8788/zeff_div) * (zeff_div+0.21)/(zeff_div+4.2)

    ! dtedx Equation 5 - thermal conduction equation
    !yp(3) =qperp_conducted * zeffpoint3 /te**2.5 /kappa0
    yp(3) =qperp_conducted / te**2.5 / kappa0

    ! dnvdx Equation 3 - ion continuity
    yp(4) =1.d-24*(ionrate1 + ionrate2 - recrate)

    ! dpressuredx Equation 6 - momentum conservation
    yp(5) =-(cxrate/nel + al*nel)*nv*mi

    ! dPowerdx Equation 4 - energy conservation
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

    ! The effect of recombination on the power balance is not taken into account

    return

  end subroutine differential

  !*****************************************************************************80
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

!---------------------------------------------------------------------------
! This routine is outside the module, so it functions as test for the module.

subroutine kallenbach_test()
  !+ad_name  kallenbach_test
  !+ad_summ  Test for divertor kallenbach model
  !+ad_type  subroutine
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_desc  Test of Kallenbach divertor model
  !+ad_prob  None
  !+ad_hist  01/02/17 MDK  Initial version
  !+ad_stat  Okay
  !+ad_docs
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use divertor_ode
  use read_and_get_atomic_data
  use read_radiation
  use constants
  use process_output, only: oblnkl, obuild, ocentr, ocmmnt, oheadr, osubhd, ovarin, ovarre, ovarrf, ovarst

  implicit none!

  integer :: i
  real(kind(1.0D0))::rmajor, rminor, bt, plascur, lcon_factor,dummy, dummy2, dummy3

  ! This section just for reproducing the original numbers
  rmajor = 8.0D0
  rminor = 2.75D0
  bt = 4.00972D0*(rmajor + rminor)/rmajor
  plascur = 1.33542D0*(2.0D0*pi*rminor)/rmu0

  ! MDK Issue #494.
  ! Invert the equation for lcon to ensure lcon=100 for this test.
  ! lcon = lcon_factor * 0.395d0*pi*q*rmajor/lambda_omp**0.196
  lcon_factor = 100.0D0 / (0.395d0*pi*3.0d0*8.0D0/0.002D0**0.196)

  call oheadr(nout, 'Divertor: Kallenbach 1D Model - TESTS - ')
  call osubhd(nout, 'Inputs :')

  call ovarre(nout, 'Major radius [m]','(rmajor)', rmajor)
  call ovarre(nout, 'Minor radius [m]','(rminor)', rminor)
  call ovarre(nout, 'Toroidal field [T]','(bt)', bt, 'OP ')
  call ovarre(nout, 'Vertical (equilibrium) field [T]','(bvert)', 0.0D0)
  call ovarre(nout, 'Plasma current [A]','(plascur)', plascur)
  call ovarre(nout, 'q95 [A]','(q)', 3.0D0)

  ! Set the impurity fractions to the test values
  do i = 2, nimp
    impurity_arr(i)%frac = 0.0D0
  enddo

  impurity_arr(5)%frac = 0.04D0

  call divertor_Kallenbach(rmajor=rmajor, rminor=rminor,     &
                           bt=bt, plascur=plascur,      &
                           bvert=0.0D0, q=3.0D0,            &
                           verboseset=.false.,          &
                           ttarget=2.3D0,qtargettotal=4.175D6,                  &
                           targetangle=30.0D0,lcon_factor=lcon_factor,            &
                           netau_in=0.5D0,unit_test=.false.,abserrset=1.0D-6,     &
                           bp = 0.956d0,   &
                           psep_kallenbach=dummy, teomp=dummy2, neomp=dummy3, &
                           outfile=nout,iprint=1 )

  call ocmmnt(nout, 'Testing the reading of atomic rates and impurity radiative functions.')
  call ocmmnt(nout, 'Use "output_divertor.xlsx" in K:\Power Plant Physics and Technology\PROCESS\SOL & Divertor')

  call plot_rates()
  call ocmmnt(nout, 'Rate coefficients for deuterium - saved in "rate_coefficients.txt"')
  call ocmmnt(nout, 'Compare to Figure 2 in Kallenbach 2016.')
  call plot_Lz()
  call ocmmnt(nout, 'Radiative loss functions - saved in "radiative_loss_functions.txt"')
  call ocmmnt(nout, 'Compare to Figure 3 in Kallenbach 2016.')
  call plot_z()
  call ocmmnt(nout, 'Reads mean Z and mean Z^2 - saved in "mean_Z.tx"')
  call ocmmnt(nout, 'Compare to plots such as He_z.ps etc in /home/mkovari/sol/kallenbach/divertor_ode/LZ_NON_CORONA.')

end subroutine Kallenbach_test

!-------------------------------------------------------------------------
subroutine kallenbach_scan()
  !+ad_name  kallenbach_scan
  !+ad_summ  Scans for divertor kallenbach model: No optimisation
  !+ad_type  subroutine
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_hist  13/11/17 MDK  Initial version
  !
  ! Modify this code to run whatever scans are required (or none)
  ! The values of divertor_kallenbach_variables are taken from the input file,
  ! but can be overwritten here.
  use divertor_ode
  use divertor_kallenbach_variables
  use read_and_get_atomic_data
  use read_radiation
  use constants
  use physics_variables, only:rmajor, rminor, plascur, bvert, bp, bt, q, ralpne
  use process_output, only: oblnkl, obuild, ocentr, ocmmnt, oheadr, osubhd, ovarin, ovarre, ovarrf, ovarst

  implicit none

  integer :: i
  real(kind(1.0D0)):: dummy, dummy2, dummy3
  integer::isweep

  ! MDK Issue #494.
  ! Invert the equation for lcon to ensure lcon=100 for this test.
  ! lcon = lcon_factor * 0.395d0*pi*q*rmajor/lambda_omp**0.196
  lcon_factor = 100.0D0 / (0.395d0*pi*3.0d0*8.0D0/0.002D0**0.196)

  ! Set the impurity fractions to the test values
  do i = 2, nimp
    impurity_arr(i)%frac = 0.0D0
  enddo

  rmajor = 8.9384d0
  rminor = 2.883d0
  plascur = 19.075d6
  bvert = -0.725d0
  bp = 0.921d0
  q = 3.000d0
  ttarget = 10.0d0

  ! Set the impurity fractions to the test values
  do i = 2, nimp
    impurity_arr(i)%frac = 0.0D0
  enddo
  impurity_arr(9)%frac  = 1.0d-3   ! argon
  impurity_arr(13)%frac = 2.0d-4   ! Xenon
  impurity_arr(14)%frac = 5d-05    ! Tungsten

  isweep = 21
  call oheadr(nout, 'Divertor: Kallenbach 1D Model - SCANS - ')
  call ovarin(mfile,'Number of scan points','(isweep)',isweep)
  call ovarin(mfile,'Scanning variable number','(nsweep)',33)

  do i = 1, isweep

      if(i==1) then
          ttarget = 1.0d0
      else
          ttarget = 5.0d0*(i - 1.0d0)
      endif
      !impurity_arr(9)%frac = 1.0d-3 + (i-1.0d0)/(isweep-1.0d0) *1.0d-3
      !write(*,*)'Running kallenbach model for argon = ',impurity_arr(9)%frac
      write(*,*)'Running kallenbach model for ttarget = ',ttarget
      call oblnkl(mfile)
      call ovarin(mfile,'Scan point number','(iscan)',i)
      call ovarre(nout, 'Major radius [m]','(rmajor)', rmajor)
      call ovarre(nout, 'Minor radius [m]','(rminor)', rminor)
      call ovarre(nout, 'Toroidal field [T]','(bt)', bt, 'OP ')
      call ovarre(nout, 'Vertical (equilibrium) field [T]','(bvert)', bvert)
      call ovarre(nout, 'Average poloidal field [T]','(bp)', bp)
      call ovarre(nout, 'Plasma current [A]','(plascur)', plascur)
      call ovarre(nout, 'q95 [A]','(q)', q)
      call ovarre(nout, 'Helium ion density (thermalised ions only) / electron density','(ralpne)',ralpne)
      call ovarre(nout, 'Argon ion density / electron density','(fimp(9)',impurity_arr(9)%frac)
      call ovarre(nout, 'Xenon ion density / electron  density','(fimp(13)',impurity_arr(13)%frac)
      call ovarre(nout, 'Tungsten ion density / electron density','(fimp(14)',impurity_arr(14)%frac)

      call divertor_Kallenbach(rmajor=rmajor, rminor=rminor,     &
                           bt=bt, plascur=plascur,      &
                           bvert=bvert, q=q,            &
                           verboseset=.false.,          &
                           ttarget=ttarget, qtargettotal=qtargettotal,         &
                           targetangle=targetangle,lcon_factor=lcon_factor,    &
                           netau_in=netau,unit_test=.false.,abserrset=1.0D-6,  &
                           bp = bp,   &
                           psep_kallenbach=dummy, teomp=dummy2, neomp=dummy3,  &
                           outfile=nout,iprint=1 )
   end do


end subroutine Kallenbach_scan
