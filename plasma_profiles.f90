!  $Id:: global_variables.f90 227 2014-02-20 16:19:03Z pknight          $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module profiles_module

  !+ad_name  profiles_module
  !+ad_summ  Density and temperature profiles
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  plasma_profiles
  !+ad_cont  sumup3
  !+ad_cont  tcore
  !+ad_cont  tprofile
  !+ad_cont  ncore
  !+ad_cont  nprofile
  !+ad_args  N/A
  !+ad_desc  This module contains routines that give the density and temperature
  !+ad_desc  profile quantities
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  divertor_variables
  !+ad_call  physics_variables
  !+ad_call  maths_library
  !+ad_hist  24/02/14 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  T&amp;M/PKNIGHT/LOGBOOK24, pp.4-7
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use divertor_variables
  use maths_library
  use physics_variables

  private :: sumup3
  public :: plasma_profiles, ncore, nprofile, tcore, tprofile

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine plasma_profiles

    !+ad_name  plasma_profiles
    !+ad_summ  Calculates density and temperature profile quantities
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  None
    !+ad_desc  This subroutine initialises the density and temperature
    !+ad_desc  profile averages and peak values, given the main
    !+ad_desc  parameters describing these profiles.
    !+ad_prob  None
    !+ad_call  gamfun
    !+ad_call  ncore
    !+ad_call  nprofile
    !+ad_call  sumup3
    !+ad_call  tcore
    !+ad_call  tprofile
    !+ad_hist  19/02/14 PJK Initial version
    !+ad_hist  24/02/14 PJK Corrected alphap
    !+ad_stat  Okay
    !+ad_docs  T&amp;M/PKNIGHT/LOGBOOK24, pp.4-7
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer, parameter :: nrho = 501  !  N.B. sumup3 routine assumes odd nrho
    integer :: irho
    real(kind(1.0D0)) :: drho, rho, integ1, integ2, dens, temp
    real(kind(1.0D0)), dimension(nrho) :: arg1, arg2, arg3

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

  end subroutine plasma_profiles

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine sumup3(dx,y,integral,n)

    !+ad_name  sumup3
    !+ad_summ  Routine to integrate a 1-D array of y values using the
    !+ad_summ  Extended Simpson's Rule, assuming equally-spaced x values
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  dx : input real : (constant) spacing between adjacent x values
    !+ad_args  y(1:n) : input real array : y values to be integrated
    !+ad_args  integral : output real : calculated integral
    !+ad_args  n : input integer : length of array y
    !+ad_desc  This routine uses Simpson's Rule to integrate an array y.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  28/06/06 PJK Initial version
    !+ad_hist  19/02/14 PJK (Possibly temporary) Inclusion into PROCESS,
    !+ad_hisc               cut-down to only allow odd n
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: n
    real(kind(1.0D0)), intent(in) :: dx
    real(kind(1.0D0)), intent(in), dimension(n) :: y
    real(kind(1.0D0)), intent(out) :: integral

    !  Local variables

    integer :: ix
    real(kind(1.0D0)) :: sum1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (mod(n,2) == 0) then
       write(*,*) 'Error in routine SUMUP3:'
       write(*,*) 'This version assumes an odd number of tabulated points'
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    sum1 = y(1)
    do ix = 2,n-3,2
       sum1 = sum1 + 4.0D0*y(ix) + 2.0D0*y(ix+1)
    end do
    integral = dx/3.0D0*(sum1 + 4.0D0*y(n-1) + y(n))

  end subroutine sumup3

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function tcore(rhopedt, tped, tsep, tav, alphat, tbeta)

    !+ad_name  tcore
    !+ad_summ  Central temperature for a pedestal profile
    !+ad_type  Function returning real
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rhopedt : input real : normalised minor radius pedestal position
    !+ad_args  tped : input real : pedestal temperature (keV)
    !+ad_args  tsep : input real : separatrix temperature (keV)
    !+ad_args  tav : input real : volume average temperature (keV)
    !+ad_args  alphat : input real : temperature peaking parameter
    !+ad_args  tbeta : input real : second temperature exponent
    !+ad_desc  This routine calculates the core temperature (keV)
    !+ad_desc  of a pedestalised profile.
    !+ad_prob  None
    !+ad_call  gamfun
    !+ad_hist  07/10/13 RK  First draft of routine
    !+ad_hist  19/12/13 HL  Separate function
    !+ad_hist  19/02/14 PJK Minor modifications to use gamfun
    !+ad_stat  Okay
    !+ad_docs  J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: tcore

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rhopedt, tped, tsep, tav, alphat, tbeta

    !  Local variables

    real(kind(1.0D0)), parameter :: numacc = 1.0D-7
    real(kind(1.0D0)) :: a,gamfac

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  For integer values of alphat, the limit of
    !  gamfun(-alphat)*sin(pi*alphat) needs to be calculated directly

    if (abs(alphat-nint(alphat)) <= numacc) then
       a = real(nint(alphat), kind(1.0D0))
       gamfac = 3.0D0/gamfun(a) * pi * gamfun(1.0D0 + alphat + 2.0D0/tbeta)
    else
       gamfac = ( (rhopedt - 1.0D0) * &
            (tped + 2.0D0*tped*rhopedt + tsep*(2.0D0+rhopedt)) &
            - 3.0D0*gamfun(-alphat)*gamfun(1.0D0 + alphat + 2.0D0/tbeta) ) &
            * sin(pi*alphat)
    end if

    !  Calculate core temperature

    tcore = tped + 1.0D0/(3.0D0*pi*gamfun(1.0D0 + 2.0D0/tbeta))   &
         * (1.0D0/rhopedt**2) * (tav - tped*rhopedt**2) * gamfac

  end function tcore

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function tprofile(rho, rhopedt, t0, tped, tsep, alphat, tbeta)

    !+ad_name  tprofile
    !+ad_summ  Implementation of HELIOS-type temperature pedestal profile
    !+ad_type  Function returning real
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rho     : input real : normalised minor radius
    !+ad_args  rhopedt : input real : normalised minor radius pedestal position
    !+ad_args  t0      : input real : central temperature (keV)
    !+ad_args  tped    : input real : pedestal temperature (keV)
    !+ad_args  tsep    : input real : separatrix temperature (keV)
    !+ad_args  alphat  : input real : temperature peaking parameter
    !+ad_args  tbeta   : input real : second temperature exponent
    !+ad_args  trho    : output real : T(rho) (keV)
    !+ad_desc  This routine calculates the temperature at a normalised minor 
    !+ad_desc  radius position rho for a pedestalised profile.
    !+ad_desc  <P>If <CODE>ipedestal = 0</CODE> the original parabolic
    !+ad_desc  profile form is used instead.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  07/10/13 RK  First draft of routine
    !+ad_hist  12/12/13 HL  Separate n and T profiles, minor changes
    !+ad_hist  19/02/14 PJK Transferred into PROCESS as a function
    !+ad_stat  Okay
    !+ad_docs  J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: tprofile

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rho, rhopedt, t0, tped, tsep, alphat, tbeta

    !  Local variables

    real(kind(1.0D0)), parameter :: numacc = 1.0D-7

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ipedestal == 0) then
       tprofile = t0 * (1.0D0 - rho**2)**alphat
       return
    end if

    !  Input checks

    if ((abs(rhopedt-1.0D0) <=  numacc) .and. ((tped-tsep) >= numacc)) then
       write(*,*) 'Warning in TPROFILE:'
       write(*,*) 'tped sets the value of the temperature profile'
       write(*,*) 'at the separatrix!'
    end if

    if (tped < tsep) then
       write(*,*) 'Warning in TPROFILE:'
       write(*,*) 'The temperature at the separatrix is higher than at'
       write(*,*) 'the pedestal!'
    end if

    if (t0 < tped) then
       write(*,*) 'Warning in TPROFILE:'
       write(*,*) 'The temperature at the pedestal is higher than at the core!'
    end if

    if (rho <= rhopedt) then
       tprofile = tped + (t0 - tped) * (1.0D0 - (rho/rhopedT)**tbeta)**alphat
    else
       tprofile = tsep + (tped - tsep) * (1.0D0 - rho)/(1.0D0 - rhopedt)
    end if

  end function tprofile
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function ncore(rhopedn, nped, nsep, nav, alphan)
    
    !+ad_name  ncore
    !+ad_summ  Central density of a pedestal profile
    !+ad_type  Function returning real
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rhopedn : input real : normalised minor radius pedestal position
    !+ad_args  nped : input real : pedestal density (/m3)
    !+ad_args  nsep : input real : separatrix density (/m3)
    !+ad_args  nav : input real : volume average density (/m3)
    !+ad_args  alphan : input real : density peaking parameter
    !+ad_desc  This routine calculates the central density
    !+ad_desc  of a pedestalised profile.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  07/10/13 RK  First draft of routine
    !+ad_hist  19/12/13 HL  Separate function
    !+ad_hist  19/02/14 PJK First version within PROCESS
    !+ad_stat  Okay
    !+ad_docs  J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: ncore

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rhopedn, nped, nsep, nav, alphan
 
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ncore = 1.0D0 / (3.0D0*rhopedn**2) * ( &
         3.0D0*nav*(1.0D0 + alphan) &
         + nsep*(1.0D0 + alphan)*(-2.0D0 + rhopedn + rhopedn**2) &
         - nped*( (1.0D0 + alphan)*(1.0D0 + rhopedn) + &
         (alphan - 2.0D0)*rhopedn**2 ) )

  end function ncore

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  function nprofile(rho, rhopedn, n0, nped, nsep, alphan)
    
    !+ad_name  nprofile
    !+ad_summ  Implementation of HELIOS-type density pedestal profile
    !+ad_type  Function returning real
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rho     : input real : normalised minor radius
    !+ad_args  rhopedn : input real : normalised minor radius pedestal position
    !+ad_args  n0      : input real : central density (/m3)
    !+ad_args  nped    : input real : pedestal density (/m3)
    !+ad_args  nsep    : input real : separatrix density (/m3)
    !+ad_args  alphan  : input real : density peaking parameter
    !+ad_desc  This routine calculates the density at a normalised minor
    !+ad_desc  radius position rho for a pedestalised profile.
    !+ad_desc  <P>If <CODE>ipedestal = 0</CODE> the original parabolic
    !+ad_desc  profile form is used instead.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  07/10/13 RK  First draft of routine
    !+ad_hist  12/12/13 HL  Separate n and T profiles, minor changes
    !+ad_hist  20/02/14 PJK Transferred into PROCESS as a function
    !+ad_stat  Okay
    !+ad_docs  J.Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: nprofile

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rho, rhopedn, n0,  nped, nsep, alphan

    !  Local variables

    real(kind(1.0D0)), parameter :: numacc = 1.0D-7

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (ipedestal == 0) then
       nprofile = n0 * (1.0D0 - rho**2)**alphan
       return
    end if

    !  Input checks

    if ((abs(rhopedn-1.0D0) <=  numacc) .and. ((nped-nsep) >= numacc)) then
       write(*,*) 'Warning in NPROFILE:'
       write(*,*) 'nped sets the value of the density profile'
       write(*,*) 'at the separatrix!'
    end if

    if (nped < nsep) then
       write(*,*) 'Warning in NPROFILE:'
       write(*,*) 'Warning: The density at the separatrix is higher than at'
       write(*,*) 'the pedestal!'
    end if
 
   if (n0 < nped) then
       write(*,*) 'Warning in NPROFILE:'
       write(*,*) 'The density at the pedestal is higher than at the core!'
    end if

    if (rho <= rhopedn) then
       nprofile = nped + (n0 - nped) * (1.0D0 - (rho/rhopedn)**2)**alphan
    else
       nprofile = nsep + (nped - nsep) * (1.0D0 - rho)/(1.0D0 - rhopedn)
    end if

  end function nprofile

end module profiles_module
