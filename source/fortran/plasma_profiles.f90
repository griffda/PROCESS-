! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module profiles_module

  !! Density and temperature profiles
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines that give the density and temperature
  !! profile quantities
  !! T&amp;M/PKNIGHT/LOGBOOK24, pp.4-7
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  private
  public :: plasma_profiles, ncore, nprofile, tcore, tprofile

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine plasma_profiles

    !! Calculates density and temperature profile quantities
    !! author: P J Knight, CCFE, Culham Science Centre
    !! None
    !! This subroutine initialises the density and temperature
    !! profile averages and peak values, given the main
    !! parameters describing these profiles.
    !! T&amp;M/PKNIGHT/LOGBOOK24, pp.4-7
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constants, only: echarge
    use divertor_variables, only: prn1
    use maths_library, only: gamfun, sumup3
    use physics_variables, only: rhopedt, ten, tin, alphap, tbeta, te0, p0, &
      nesep, tesep, pcoef, ipedestal, ni0, ne0, ti0, tratio, dnla, alphat, &
      dnitot, neped, ti, rhopedn, dene, teped, alphan, te, dndrho_max, rho_max_dn, &
      rho_max_dt, dtdrho_max
    implicit none

    !  Arguments

    !  Local variables

    integer, parameter :: nrho = 501
    integer :: irho
    real(8) :: drho, rho, integ1, integ2, dens, temp
    real(8), dimension(nrho) :: arg1, arg2, arg3

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Volume-averaged ion temperature
    !  (input value used directly if tratio=0.0)

    if (tratio > 0.0D0) ti = tratio * te

    if (ipedestal == 0) then

       !  Reset pedestal values to agree with original parabolic profiles

       rhopedt = 1.0D0 ; rhopedn = 1.0D0
       teped = 0.0D0 ; tesep = 0.0D0
       neped = 0.0D0 ; nesep = 0.0D0
       tbeta = 2.0D0

       !  Profile factor; ratio of density-weighted to volume-averaged
       !  temperature

       pcoef = (1.0D0 + alphan)*(1.0D0 + alphat)/(1.0D0+alphan+alphat)

       !  Line averaged electron density (IPDG89)
       !  0.5*gamfun(0.5) = 0.5*sqrt(pi) = 0.886227

       dnla = dene*(1.0D0+alphan) * 0.886227D0 * gamfun(alphan+1.0D0) / &
            gamfun(alphan+1.5D0)

       !  Density-weighted temperatures

       ten = te * pcoef
       tin = ti * pcoef

       !  Central values for temperature (keV) and density (m**-3)

       te0 = te * (1.0D0+alphat)
       ti0 = ti * (1.0D0+alphat)

       ne0 =   dene * (1.0D0+alphan)
       ni0 = dnitot * (1.0D0+alphan)

    else

       !  The following reproduces the above results within sensible
       !  tolerances if rhopedt = rhopedn = 1.0, teped = tesep = neped
       !  = nesep = 0.0, and tbeta = 2.0

       !  Central values for temperature (keV) and density (m**-3)

       te0 = tcore(rhopedt,teped,tesep,te,alphat,tbeta)
       ti0 = ti/te * te0

       ne0 = ncore(rhopedn,neped,nesep,dene,alphan)
       ni0 = dnitot/dene * ne0

       !  Perform integrations to calculate ratio of density-weighted
       !  to volume-averaged temperature, etc.
       !  Density-weighted temperature = integral(n.T dV) / integral(n dV)
       !  which is approximately equal to the ratio
       !  integral(rho.n(rho).T(rho) drho) / integral(rho.n(rho) drho)

       drho = 1.0D0/(nrho-1)
       do irho = 1,nrho
          rho = (irho-1.0D0)/(nrho-1)
          dens = nprofile(rho,rhopedn,ne0,neped,nesep,alphan)
          temp = tprofile(rho,rhopedt,te0,teped,tesep,alphat,tbeta)
          arg1(irho) = rho*dens*temp
          arg2(irho) = rho*dens
          arg3(irho) = dens
       end do
       call sumup3(drho,arg1,integ1,nrho)
       call sumup3(drho,arg2,integ2,nrho)

       !  Density-weighted temperatures

       ten = integ1/integ2
       tin = ti/te * ten

       !  Profile factor; ratio of density-weighted to volume-averaged
       !  temperature

       pcoef = ten / te

       !  Line-averaged electron density
       !  = integral(n(rho).drho)

       call sumup3(drho,arg3,dnla,nrho)

       !  Scrape-off density / volume averaged density
       !  (Input value is used if ipedestal = 0)

       prn1 = max(0.01D0, nesep/dene)  !  preventing division by zero later

    end if

    !  Central pressure (Pa), from ideal gas law : p = nkT

    p0 = (ne0*te0 + ni0*ti0) * 1.0D3 * echarge

    !  Pressure profile index (N.B. no pedestal effects included here)
    !  N.B. p0 is NOT equal to <p> * (1 + alphap), but p(rho) = n(rho)*T(rho)
    !  and <p> = <n>.T_n where <...> denotes volume-averages and T_n is the
    !  density-weighted temperature

    alphap = alphan + alphat


    ! The gradient information for ipedestal = 0:
    if (ipedestal == 0) then
      if(alphat > 1.0) then
         ! Rho (normalized radius), where temperature derivative is largest
         rho_max_dt = 1.0d0/sqrt(-1.0d0 +2.0d0 * alphat)
         dtdrho_max = -2.0**alphat*(-1.0 + alphat)**(-1.0 + alphat)*alphat*(-1.0 + &
                     2.0 * alphat)**(0.5 - alphat)

      elseif (alphat .le. 1.0 .and. alphaT > 0.0) then
         ! This makes the profiles very 'boxy'
         ! The gradient diverges here at the edge so define some 'wrong' value of 0.99
         ! to approximate the gradient
         rho_max_dt = 0.99
         dtdrho_max = -100.0*0.02**alphat*alphat

      else
         print *, "ERROR: alphat is negative!"
         call exit(1)
      end if

      ! Same for density
      if(alphan > 1.0) then
         rho_max_dn = 1.0d0/sqrt(-1.0d0 +2.0d0 * alphan)
         dndrho_max = -2.0**alphan*(-1.0 + alphan)**(-1.0 + alphan)*alphan*(-1.0 + &
                     2.0 * alphan)**(0.5 - alphan)

      elseif (alphan .le. 1.0 .and. alphan > 0.0) then
         ! This makes the profiles very 'boxy'
         ! The gradient diverges here at the edge so define some 'wrong' value of 0.99
         ! to approximate the gradient
         rho_max_dn = 0.99
         dndrho_max = -100.0*0.02**alphan*alphan

      else
         print *, "ERROR: alphan is negative!"
         call exit(1)
      end if

    end if



  end subroutine plasma_profiles

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function tcore(rhopedt, tped, tsep, tav, alphat, tbeta)

    !! Central temperature for a pedestal profile
    !! author: R Kemp, CCFE, Culham Science Centre
    !! author: H Lux, CCFE, Culham Science Centre
    !! author: P J Knight, CCFE, Culham Science Centre
    !! rhopedt : input real : normalised minor radius pedestal position
    !! tped : input real : pedestal temperature (keV)
    !! tsep : input real : separatrix temperature (keV)
    !! tav : input real : volume average temperature (keV)
    !! alphat : input real : temperature peaking parameter
    !! tbeta : input real : second temperature exponent
    !! This routine calculates the core temperature (keV)
    !! of a pedestalised profile.
    !! J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use constants, only: pi
		use maths_library, only: gamfun
    implicit none

    real(8) :: tcore

    !  Arguments

    real(8), intent(in) :: rhopedt, tped, tsep, tav, alphat, tbeta

    !  Local variables

    real(8), parameter :: numacc = 1.0D-7
    real(8) :: gamfac

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  For integer values of alphat, the limit of
    !  gamfun(-alphat)*sin(pi*alphat) needs to be calculated directly

    gamfac = gamfun(1.0D0 + alphat + 2.0D0/tbeta) / &
         gamfun((2.0D0 + tbeta)/tbeta) / rhopedT**2

    if (abs(alphat-nint(alphat)) <= numacc) then
       gamfac = -gamfac / gamfun(1.0D0 + alphat)
    else
       gamfac = gamfac * gamfun(-alphat)*sin(pi*alphat)/pi
    end if

    !  Calculate core temperature

    tcore = tped + gamfac * ( tped*rhopedT**2 - tav + &
         (1.0D0 - rhopedT)/3.0D0 * &
         ( (1.0D0 + 2.0D0*rhopedT)*tped + (2.0D0 + rhopedT)*tsep ) )

  end function tcore

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function tprofile(rho, rhopedt, t0, tped, tsep, alphat, tbeta)

    !! Implementation of HELIOS-type temperature pedestal profile
    !! author: R Kemp, CCFE, Culham Science Centre
    !! author: H Lux, CCFE, Culham Science Centre
    !! author: P J Knight, CCFE, Culham Science Centre
    !! rho     : input real : normalised minor radius
    !! rhopedt : input real : normalised minor radius pedestal position
    !! t0      : input real : central temperature (keV)
    !! tped    : input real : pedestal temperature (keV)
    !! tsep    : input real : separatrix temperature (keV)
    !! alphat  : input real : temperature peaking parameter
    !! tbeta   : input real : second temperature exponent
    !! trho    : output real : T(rho) (keV)
    !! This routine calculates the temperature at a normalised minor 
    !! radius position rho for a pedestalised profile.
    !! <P>If <CODE>ipedestal = 0</CODE> the original parabolic
    !! profile form is used instead.
    !! J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use error_handling, only: fdiags, report_error
		use physics_variables, only: ipedestal
    implicit none

    real(8) :: tprofile

    !  Arguments

    real(8), intent(in) :: rho, rhopedt, t0, tped, tsep, alphat, tbeta

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ipedestal == 0) then
       tprofile = t0 * (1.0D0 - rho**2)**alphat
       return
    end if

    !  Error trap; shouldn't happen unless volume-averaged temperature has
    !  been allowed to drop below tped. This may happen during a HYBRD case,
    !  but should have been prevented for optimisation runs.

    if (t0 < tped) then
       fdiags(1) = tped ; fdiags(2) = t0
       call report_error(148)
    end if

    if (rho <= rhopedt) then
       tprofile = tped + (t0 - tped) * (1.0D0 - (rho/rhopedT)**tbeta)**alphat
    else
       tprofile = tsep + (tped - tsep) * (1.0D0 - rho)/(1.0D0 - rhopedt)
    end if

  end function tprofile
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function ncore(rhopedn, nped, nsep, nav, alphan)
    
    !! Central density of a pedestal profile
    !! author: R Kemp, CCFE, Culham Science Centre
    !! author: H Lux, CCFE, Culham Science Centre
    !! author: P J Knight, CCFE, Culham Science Centre
    !! rhopedn : input real : normalised minor radius pedestal position
    !! nped : input real : pedestal density (/m3)
    !! nsep : input real : separatrix density (/m3)
    !! nav : input real : volume average density (/m3)
    !! alphan : input real : density peaking parameter
    !! This routine calculates the central density
    !! of a pedestalised profile.
    !! J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use error_handling, only: report_error
    implicit none

    real(8) :: ncore

    !  Arguments

    real(8), intent(in) :: rhopedn, nped, nsep, nav, alphan
 
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ncore = 1.0D0 / (3.0D0*rhopedn**2) * ( &
         3.0D0*nav*(1.0D0 + alphan) &
         + nsep*(1.0D0 + alphan)*(-2.0D0 + rhopedn + rhopedn**2) &
         - nped*( (1.0D0 + alphan)*(1.0D0 + rhopedn) + &
         (alphan - 2.0D0)*rhopedn**2 ) )

    if (ncore<0.0D0) then
      write(*,*) 'Error in ncore: negative central density'
      write(*,*) 'nped =', nped, ' nsep =', nsep
      write(*,*) 'nav =', nav, ' ncore =', ncore
      call report_error(212)
    end if

  end function ncore

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function nprofile(rho, rhopedn, n0, nped, nsep, alphan)
    
    !! Implementation of HELIOS-type density pedestal profile
    !! author: R Kemp, CCFE, Culham Science Centre
    !! author: H Lux, CCFE, Culham Science Centre
    !! author: P J Knight, CCFE, Culham Science Centre
    !! rho     : input real : normalised minor radius
    !! rhopedn : input real : normalised minor radius pedestal position
    !! n0      : input real : central density (/m3)
    !! nped    : input real : pedestal density (/m3)
    !! nsep    : input real : separatrix density (/m3)
    !! alphan  : input real : density peaking parameter
    !! This routine calculates the density at a normalised minor
    !! radius position rho for a pedestalised profile.
    !! <P>If <CODE>ipedestal = 0</CODE> the original parabolic
    !! profile form is used instead.
    !! J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use error_handling, only: fdiags, report_error
		use physics_variables, only: ipedestal
    implicit none

    real(8) :: nprofile

    !  Arguments

    real(8), intent(in) :: rho, rhopedn, n0,  nped, nsep, alphan

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ipedestal == 0) then
       nprofile = n0 * (1.0D0 - rho**2)**alphan
       return
    end if

    !  Error trap; shouldn't happen unless volume-averaged density has
    !  been allowed to drop below nped. This may happen during a HYBRD case,
    !  but should have been prevented for optimisation runs.

    !  Input checks

    if (n0 < nped) then
       fdiags(1) = nped ; fdiags(2) = n0
       call report_error(153)
    end if

    if (rho <= rhopedn) then
       nprofile = nped + (n0 - nped) * (1.0D0 - (rho/rhopedn)**2)**alphan
    else
       nprofile = nsep + (nped - nsep) * (1.0D0 - rho)/(1.0D0 - rhopedn)
    end if

  end function nprofile

end module profiles_module
