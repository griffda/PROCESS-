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

  logical, save :: impurities_present(14)

  ! impurity element name - temporary

  ! relative ion mass
  ! Issue #501: change from 2 to 2.5
  real(dp) :: aplas

  ! ion mass [kg]
  real(dp) :: mi

  ! conversion from flux density [el/s] to Pascal [Molecules]
  ! see page 6 of paper.
  real(dp) :: fluxdens_to_pa

  ! Useful values (combinations of other constants/variables)
  real(dp) :: eightemi, eightemi48,  elEion
  real(dp), parameter :: degree=pi/180.0D0
  real(dp), parameter :: ln10=log(10.0D0)

  ! constant in thermal conductivity (equation 5) [J/(s m eV^7/2)]
  real(dp) :: kappa0

  ! neutral velocity along the flux bundle, groups 1 & 2 [m/s]
  real(dp) :: v01, v02

  ! Allowable absolute/relative error (UNUSED)
!   real(dp) :: abserr, relerr

  ! Circumference of plasma at outboard midplane and at target [m]
  real(dp) :: circumference_omp, circumference_target

  ! Circumference of normal to helical field line [m]
  real(dp) :: circumf_bu

  ! Flux bundle area perp. to B at target and at omp [m2]
  real(dp) :: area_target, area_omp

  ! Zeff for divertor region
  real(dp) :: zeff_div

  ! SOL radial thickness extrapolated to OMP [m]
  real(dp) :: sol_broadening

  ! Ratio: psep_kallenbach / Powerup
  real(dp), parameter :: seppowerratio=2.3D0

  real(dp) :: lengthofwidesol

  contains

    subroutine solve_ode(y0, time_steps, initial_step, eightemi48, netau_sol, lengthofwidesol, area_target, &
        area_omp, mi, aplas, eleion, v01, v02, zeff_div, impurity_concs, nimp, imp_labels, ntimesteps, yout)

      use ode_mod, only: ode
      use div_kal_vars, only: abserr_sol, abserr_sol

      real(dp), dimension(10), intent(in) :: y0
      real(dp), dimension(ntimesteps), intent(in) :: time_steps
      real(dp), intent(in) :: eightemi48, netau_sol, lengthofwidesol, area_target, area_omp, &
        mi, aplas, eleion, v01, v02, zeff_div, initial_step!, factor
      real(dp), dimension(14), intent(in) :: impurity_concs
      integer, intent(in) :: nimp, ntimesteps
      character*2, dimension(14), intent(in) :: imp_labels

      real(dp), dimension(10, ntimesteps), intent(out) :: Yout

      integer :: i, iflag
      integer, dimension(5) :: iwork
      real(dp), dimension(310) :: work
      real(dp) :: x, xout
      real(dp), dimension(10) :: y


      iflag = 1
      y = y0

      do i = 1, ntimesteps
        xout = time_steps(i)
        call ode(differential, 10, y, x, xout, 1.d-4, 1.d-4, iflag, work, iwork)
        Yout(:, i) = Y
      enddo

    contains
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
      use div_kal_vars, only: abserr_sol
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
              lz = read_lz(imp_labels(i), te, netau_sol, mean_z=.false., mean_qz=.false., verbose=.false.)
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

    end subroutine solve_ode


end module divertor_ode
