module physics_functions_module

  !+ad_name  physics_functions_module
  !+ad_summ  Module containing physics subfunctions
  !+ad_type  Module
  !+ad_auth  K Ellis, CCFE, Culham Science Centre
  !+ad_cont  physics_functions
  !+ad_cont  beamcalc
  !+ad_cont  beamfus
  !+ad_cont  bosch_hale
  !+ad_cont  fsv
  !+ad_cont  imprad
  !+ad_cont  palph
  !+ad_cont  palph2
  !+ad_cont  p_eped_scaling
  !+ad_cont  prad_ipdg89
  !+ad_cont  psync_albajar_fidone
  !+ad_cont  pthresh
  !+ad_cont  radpwr
  !+ad_cont  t_eped_scaling
  !+ad_args  N/A
  !+ad_desc  This module contains physics routines which can be called by physics or
  !+ad_desc  other modules (e.g. PLASMOD).
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  error_handling
  !+ad_call  maths_library
  !+ad_call  physics_variables
  !+ad_call  profiles_module
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
!  use divertor_ode, only: impurity_concs
!  use divertor_kallenbach_variables, only: impurity_enrichment
  use error_handling
  use impurity_radiation_module
  use maths_library
  use physics_variables
  use profiles_module
  use read_and_get_atomic_data
!  use reinke_variables

  implicit none

  !private
  public :: beamfus,palph,palph2

  !  Module-level variables

  !integer ::
  real(kind(1.0D0)) :: vcritx

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pthresh(dene,dnla,bt,rmajor,kappa,sarea,aion,pthrmw)

    !+ad_name  pthresh
    !+ad_summ  L-mode to H-mode power threshold calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  S I Muldrew, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  dene   : input real :  volume-averaged electron density (/m3)
    !+ad_args  dnla   : input real :  line-averaged electron density (/m3)
    !+ad_args  bt     : input real :  toroidal field on axis (T)
    !+ad_args  rmajor : input real :  plasma major radius (m)
    !+ad_args  kappa  : input real :  plasma elongation
    !+ad_args  sarea  : input real :  plasma surface area (m**2)
    !+ad_args  aion   : input real :  average mass of all ions (amu)
    !+ad_args  pthrmw(17) : output real array : power threshold (different scalings)
    !+ad_desc  This routine calculates the power threshold for the L-mode to
    !+ad_desc  H-mode transition.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  17/07/98 PJK New routine
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_hist  18/12/12 PJK Added scalings 6-8
    !+ad_hist  16/04/18 KVE Cut from physics and pasted into physics_functions
    !+ad_hist  02/05/18 SIM Added scaling 9-14
    !+ad_stat  Okay
    !+ad_docs  ITER Physics Design Description Document, p.2-2
    !+ad_docs  ITER-FDR Plasma Performance Assessments, p.III-9
    !+ad_docs  Snipes, 24th EPS Conference, Berchtesgaden 1997, p.961
    !+ad_docs  Martin et al, 11th IAEA Tech. Meeting on H-mode Physics and
    !+ad_docc  Transport Barriers, Journal of Physics: Conference Series
    !+ad_docc  123 (2008) 012033
    !+ad_docs  J A Snipes and the International H-mode Threshold Database
    !+ad+docc  Working Group, 2000, Plasma Phys. Control. Fusion, 42, A299
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: dene,dnla,bt,rmajor,kappa,sarea,aion
    real(kind(1.0D0)), dimension(18), intent(out) :: pthrmw

    !  Local variables

    real(kind(1.0D0)) :: dene20,dnla20,marterr

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    dene20 = 1.0D-20*dene
    dnla20 = 1.0D-20*dnla

    !  ITER-DDD, D.Boucher
    !  Fit to 1996 H-mode power threshold database: nominal

    pthrmw(1) = 0.45D0 * dene20**0.75D0 * bt * rmajor**2

    !  Fit to 1996 H-mode power threshold database: upper bound

    pthrmw(2) = 0.37D0 * dene20 * bt * rmajor**2.5D0

    !  Fit to 1996 H-mode power threshold database: lower bound

    pthrmw(3) = 0.54D0 * dene20**0.5D0 * bt * rmajor**1.5D0

    !  J. A. Snipes, ITER H-mode Threshold Database Working Group,
    !  Controlled Fusion and Plasma Physics, 24th EPS Conference,
    !  Berchtesgaden, June 1997, vol.21A, part III, p.961

    pthrmw(4) = 0.65D0 * dnla20**0.93D0 * bt**0.86D0 * rmajor**2.15D0

    pthrmw(5) = 0.42D0 * dnla20**0.80D0 * bt**0.90D0 * rmajor**1.99D0 &
         * kappa**0.76D0

    !  Martin et al (2008) for recent ITER scaling, with mass correction
    !  and 95% confidence limits

    pthrmw(6) = 0.0488D0 * dnla20**0.717D0 * bt**0.803D0 &
         * sarea**0.941D0 * (2.0D0/aion)

    marterr = 0.057D0**2 + (0.035D0 * log(dnla20))**2 &
         + (0.032D0 * log(bt))**2 + (0.019D0 * log(sarea))**2
    marterr = sqrt(marterr) * pthrmw(6)

    pthrmw(7) = pthrmw(6) + 2.0D0*marterr
    pthrmw(8) = pthrmw(6) - 2.0D0*marterr

    ! Snipes et al (2000) scaling with mass correction
    ! Nominal, upper and lower

    pthrmw(9) = 1.42D0 * dnla20**0.58D0 * bt**0.82D0 * rmajor &
               * rminor**0.81D0 * (2.0D0/aion)

    pthrmw(10) = 1.547D0 * dnla20**0.615D0 * bt**0.851D0 &
              * rmajor**1.089D0 * rminor**0.876D0 * (2.0D0/aion)

    pthrmw(11) = 1.293D0 * dnla20**0.545D0 * bt**0.789D0 &
              * rmajor**0.911D0 * rminor**0.744D0 * (2.0D0/aion)

    ! Snipes et al (2000) scaling (closed divertor) with mass correction
    ! Nominal, upper and lower

    pthrmw(12) = 0.8D0 * dnla20**0.5D0 * bt**0.53D0 * rmajor**1.51D0 &
               * (2.0D0/aion)

    pthrmw(13) = 0.867D0 * dnla20**0.561D0 * bt**0.588D0 * rmajor**1.587D0 &
               * (2.0D0/aion)

    pthrmw(14) = 0.733D0 * dnla20**0.439D0 * bt**0.472D0 * rmajor**1.433D0 &
               * (2.0D0/aion)

    ! Hubbard et al. 2012 L-I threshold scaling

    ! Nominal
    pthrmw(15) = 2.11 * (plascur/1.0D6)**0.94 * dnla20**0.65

    ! Lower bound
    pthrmw(16) = 2.11 * (plascur/1.0D6)**0.70 * dnla20**0.47

    ! Upper bound
    pthrmw(17) = 2.11 * (plascur/1.0D6)**1.18 * dnla20**0.83

    ! Hubbard et al. 2017 L-I threshold scaling
    pthrmw(18) = 0.2 * dnla20 * sarea * (bt/2.0)**0.25

  end subroutine pthresh

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine palph(alphan,alphat,deni,fdeut,fhe3,ftrit,ti, &
       palppv,pchargepv,pneutpv,sigvdt,fusionrate,alpharate,protonrate, &
       pdtpv,pdhe3pv,pddpv)

    !+ad_name  palph
    !+ad_summ  (Initial part of) fusion power and fast alpha pressure calculations
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  fint
    !+ad_args  alphan     : input real :  density profile index
    !+ad_args  alphat     : input real :  temperature profile index
    !+ad_args  deni       : input real :  fuel ion density (/m3)
    !+ad_args  fdeut      : input real :  deuterium fuel fraction
    !+ad_args  fhe3       : input real :  helium-3 fuel fraction
    !+ad_args  ftrit      : input real :  tritium fuel fraction
    !+ad_args  ti         : input real :  ion temperature (keV)
    !+ad_args  palppv     : output real : alpha particle fusion power per volume (MW/m3)
    !+ad_args  pchargepv  : output real : other charged particle fusion power/volume (MW/m3)
    !+ad_args  pneutpv    : output real : neutron fusion power per volume (MW/m3)
    !+ad_args  sigvdt     : output real : profile averaged <sigma v DT> (m3/s)
    !+ad_args  fusionrate : output real : fusion reaction rate (reactions/m3/s)
    !+ad_args  alpharate  : output real : alpha particle production rate (/m3/s)
    !+ad_args  protonrate : output real : proton production rate (/m3/s)
    !+ad_args  pdtpv      : output real : D-T fusion power (MW/m3)
    !+ad_args  pdhe3pv    : output real : D-He3 fusion power (MW/m3)
    !+ad_args  pddpv      : output real : D-D fusion power (MW/m3)
    !+ad_desc  This subroutine numerically integrates over plasma cross-section to
    !+ad_desc  find the core plasma fusion power.
    !+ad_prob  None
    !+ad_call  fint
    !+ad_call  quanc8
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  06/12/95 PJK Added D-He3 calculations
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Removed pi from argument list
    !+ad_hist  10/09/13 PJK Added fusion, alpha and proton rate calculations
    !+ad_hist  11/09/13 PJK Removed idhe3, ftr, ealpha, iiter usage
    !+ad_hist  28/11/13 PJK Added powers for each fuel-pair to output
    !+ad_hist  20/02/14 PJK Modified calculation to deal with pedestal profiles
    !+ad_hist  04/03/14 PJK Changed fusion power upper integration bound to 1.0;
    !+ad_hisc               corrected D-D reaction rates (MK/TNT)
    !+ad_stat  Okay
    !+ad_docs  T&amp;M/PKNIGHT/LOGBOOK24, p.6
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: alphan, alphat, deni, fdeut, &
         fhe3, ftrit, ti
    real(kind(1.0D0)), intent(out) :: palppv, pchargepv, pneutpv, sigvdt, &
         fusionrate, alpharate, protonrate, pdtpv, pdhe3pv, pddpv

    !  Local variables

    integer, parameter :: DT=1, DHE3=2, DD1=3, DD2=4
    integer :: ireaction,nofun
    real(kind(1.0D0)) :: alow,arate,bhigh,epsq8,errest,etot,flag, &
         fpow,frate,pa,pc,pn,prate,sigmav

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Initialise local quantities

    alow = 0.0D0
    bhigh = 1.0D0
    epsq8 = 1.0D-9

    !  Find fusion power
    !  Integrate over plasma profiles to obtain fusion reaction rate

    palppv = 0.0D0
    pchargepv = 0.0D0
    pneutpv = 0.0D0
    fusionrate = 0.0D0
    alpharate = 0.0D0
    protonrate = 0.0D0
    pddpv = 0.0D0

    do ireaction = 1,4

       !  Fusion reaction rate (m3/s) is calculated in fint for each ireaction
       !  sigmav is the volume-averaged fusion reaction rate (m3/s)
       !  = integral(2 rho sigv(rho).ni(rho)^2 drho) / (deni**2)

       call quanc8(fint,alow,bhigh,epsq8,epsq8,sigmav,errest,nofun,flag)
       if (ireaction == DT) sigvdt = sigmav

       select case (ireaction)

       case (DT)  !  D + T --> 4He + n reaction

          etot = 17.59D0 * echarge  !  MJ
          fpow = 1.0D0 * sigmav * etot * fdeut*ftrit * deni*deni  !  MW/m3
          pa = 0.2D0 * fpow
          pc = 0.0D0
          pn = 0.8D0 * fpow
          frate = fpow/etot  !  reactions/m3/second
          arate = frate
          prate = 0.0D0
          pdtpv = fpow

       case (DHE3)  !  D + 3He --> 4He + p reaction

          etot = 18.35D0 * echarge  !  MJ
          fpow = 1.0D0 * sigmav * etot * fdeut*fhe3 * deni*deni  !  MW/m3
          pa = 0.2D0 * fpow
          pc = 0.8D0 * fpow
          pn = 0.0D0
          frate = fpow/etot  !  reactions/m3/second
          arate = frate
          prate = frate      !  proton production /m3/second
          pdhe3pv = fpow

       case (DD1)  !  D + D --> 3He + n reaction
          !  The 0.5 branching ratio is assumed to be included in sigmav

          etot = 3.27D0 * echarge  !  MJ
          fpow = 1.0D0 * sigmav * etot * 0.5D0*fdeut*fdeut * deni*deni  !  MW/m3
          pa = 0.0D0
          pc = 0.25D0 * fpow
          pn = 0.75D0 * fpow
          frate = fpow/etot  !  reactions/m3/second
          arate = 0.0D0
          prate = 0.0D0      !  Issue #557: No proton production
          pddpv = pddpv + fpow

       case (DD2)  !  D + D --> T + p reaction
          !  The 0.5 branching ratio is assumed to be included in sigmav

          etot = 4.03D0 * echarge  !  MJ
          fpow = 1.0D0 * sigmav * etot * 0.5D0*fdeut*fdeut * deni*deni  !  MW/m3
          pa = 0.0D0
          pc = fpow
          pn = 0.0D0
          frate = fpow/etot  !  reactions/m3/second
          arate = 0.0D0
          prate = frate      !  proton production /m3/second
          pddpv = pddpv + fpow

       end select

       palppv = palppv + pa
       pchargepv = pchargepv + pc
       pneutpv = pneutpv + pn
       fusionrate = fusionrate + frate
       alpharate = alpharate + arate
       protonrate = protonrate + prate

    end do

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function fint(rho)

      !+ad_name  fint
      !+ad_summ  Integrand for fusion power integration
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  rho : input real :  Abscissa of the integration, = normalised
      !+ad_argc                      plasma minor radius (0.0 <= rho < 1.0)
      !+ad_desc  This function evaluates the integrand for the fusion power
      !+ad_desc  integration, performed using routine
      !+ad_desc  <A HREF="quanc8.html">QUANC8</A>
      !+ad_desc  in routine <A HREF="palph.html">PALPH</A>.
      !+ad_desc  The fusion reaction assumed is controlled by flag
      !+ad_desc  <CODE>ireaction</CODE> set in <CODE>PALPH</CODE>.
      !+ad_prob  None
      !+ad_call  bosch_hale
      !+ad_call  nprofile
      !+ad_call  tprofile
      !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
      !+ad_hist  09/11/11 PJK Initial F90 version
      !+ad_hist  11/09/13 PJK Used bosch_hale instead of svfdt
      !+ad_hist  20/02/14 PJK Modified to deal with generalised profiles
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: fint

      !  Arguments

      real(kind(1.0D0)), intent(in) :: rho

      !  Local variables

      real(kind(1.0D0)) :: nprof, nprofsq, sigv, tiofr

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Local ion temperature (keV) at r/a = rho

      tiofr = ti/te * tprofile(rho,rhopedt,te0,teped,tesep,alphat,tbeta)

      !  Fusion reaction rate (m3/s)

      sigv = bosch_hale(tiofr,ireaction)

      !  Integrand for the volume averaged fusion reaction rate sigmav:
      !  sigmav = integral(2 rho (sigv(rho) ni(rho)^2) drho),
      !  divided by the square of the volume-averaged ion density
      !  to retain the dimensions m3/s (this is multiplied back in later)

      nprof = 1.0D0/dene * nprofile(rho,rhopedn,ne0,neped,nesep,alphan)
      nprofsq = nprof*nprof

      fint = 2.0D0 * rho * sigv * nprofsq

    end function fint

  end subroutine palph

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine palph2(bt,bp,dene,deni,dnitot,falpe,falpi,palpnb, &
       ifalphap,pchargepv,pneutpv,ten,tin,vol,palpmw,pneutmw,pchargemw, &
       betaft,palppv,palpipv,palpepv,pfuscmw,powfmw)

    !+ad_name  palph2
    !+ad_summ  (Concluding part of) fusion power and fast alpha pressure
    !+ad_summ  calculations
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  bp       : input real :  poloidal field (T)
    !+ad_args  bt       : input real :  toroidal field on axis (T)
    !+ad_args  dene     : input real :  electron density (/m3)
    !+ad_args  deni     : input real :  fuel ion density (/m3)
    !+ad_args  dnitot   : input real :  total ion density (/m3)
    !+ad_args  falpe    : input real :  fraction of alpha energy to electrons
    !+ad_args  falpi    : input real :  fraction of alpha energy to ions
    !+ad_args  ifalphap : input integer :  switch for fast alpha pressure method
    !+ad_args  palpnb   : input real :  alpha power from hot neutral beam ions (MW)
    !+ad_args  pchargepv : input real : other charged particle fusion power/volume (MW/m3)
    !+ad_args  pneutpv  : input/output real : neutron fusion power per volume (MW/m3)
    !+ad_args  ten      : input real :  density-weighted electron temperature (keV)
    !+ad_args  tin      : input real :  density-weighted ion temperature (keV)
    !+ad_args  vol      : input real :  plasma volume (m3)
    !+ad_args  palpmw   : output real : alpha power (MW)
    !+ad_args  pneutmw  : output real : neutron fusion power (MW)
    !+ad_args  pchargemw : output real : other charged particle fusion power (MW)
    !+ad_args  betaft   : output real : fast alpha beta component
    !+ad_args  palppv   : input/output real : alpha power per volume (MW/m3)
    !+ad_args  palpepv  : output real : alpha power per volume to electrons (MW/m3)
    !+ad_args  palpipv  : output real : alpha power per volume to ions (MW/m3)
    !+ad_args  pfuscmw  : output real : charged particle fusion power (MW)
    !+ad_args  powfmw   : output real : fusion power (MW)
    !+ad_desc  This subroutine completes the calculation of the fusion power
    !+ad_desc  fast alpha pressure, and determines other alpha particle quantities.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  06/12/95 PJK Added D-He3 calculations
    !+ad_hist  22/05/06 PJK Added modified fit to fast alpha pressure
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  11/09/13 PJK Removed obsolete argument ftr
    !+ad_hist  12/09/13 PJK Fixed betaft calculation when fdeut=1
    !+ad_hist  10/10/13 PJK Made multiplier in betath equation explicit
    !+ad_hist  19/02/14 PJK Removed obsolete argument pcoef;
    !+ad_hisc               changed te,ti to ten,tin
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  03/06/14 PJK Added pchargemw output
    !+ad_hist  17/11/14 PJK Added falpha dependencies
    !+ad_stat  Okay
    !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
    !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
    !+ad_docs  D J Ward, UKAEA Fusion: F/PL/PJK/PROCESS/CODE/050
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: ifalphap
    real(kind(1.0D0)), intent(in) :: bp, bt, dene, deni, dnitot, falpe, &
         falpi, palpnb, pchargepv, ten, tin, vol
    real(kind(1.0D0)), intent(inout) :: palppv, pneutpv
    real(kind(1.0D0)), intent(out) :: palpmw, pneutmw, pchargemw, betaft, palpepv, &
         palpipv, pfuscmw, powfmw

    !  Local variables

    real(kind(1.0D0)) :: betath, fact, fact2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Add neutral beam alpha power / volume

    palppv = palppv + palpnb/vol

    !  Add extra neutron power

    pneutpv = pneutpv + 4.0D0*palpnb/vol

    !  Total alpha power

    palpmw = palppv*vol

    !  Total non-alpha charged particle power

    pchargemw = pchargepv*vol

    !  Total neutron power

    pneutmw = pneutpv*vol

    !  Total fusion power

    powfmw = palpmw + pneutmw + pchargemw

    !  Charged particle fusion power

    pfuscmw = palpmw + pchargemw

    !  Alpha power to electrons and ions (used with electron
    !  and ion power balance equations only)
    !  No consideration of pchargepv here...

    palpipv = falpha * palppv*falpi
    palpepv = falpha * palppv*falpe

    !  Determine average fast alpha density

    if (fdeut < 1.0D0) then

       betath = 2.0D3*rmu0*echarge * (dene*ten + dnitot*tin)/(bt**2 + bp**2)

       if (ifalphap == 0) then
          !  IPDG89 fast alpha scaling
          fact = min( 0.30D0, &
               0.29D0*(deni/dene)**2 * ( (ten+tin)/20.0D0 - 0.37D0) )
       else
          !  Modified scaling, D J Ward
          fact = min( 0.30D0, &
               0.26D0*(deni/dene)**2 * &
               sqrt( max(0.0D0, ((ten+tin)/20.0D0 - 0.65D0)) ) )
       end if

       fact = max(fact,0.0D0)
       fact2 = palppv/(palppv-(palpnb/vol))
       betaft = betath * fact*fact2

    else  !  negligible alpha production, palppv = palpnb = 0
       betaft = 0.0D0
    end if

  end subroutine palph2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function bosch_hale(t,reaction)

    !+ad_name  bosch_hale
    !+ad_summ  Routine to calculate the fusion reaction rate
    !+ad_type  Function returning real
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  t : input real : Maxwellian density-weighted ion temperature (keV)
    !+ad_args  reaction : input integer : flag for fusion reaction to use:
    !+ad_argc                            1 : D-T reaction
    !+ad_argc                            2 : D-3He reaction
    !+ad_argc                            3 : D-D 1st reaction (50% probability)
    !+ad_argc                            4 : D-D 2nd reaction (50% probability)
    !+ad_desc  This routine calculates the volumetric fusion reaction rate
    !+ad_desc  <I>&lt;sigma v&gt;</I> in m3/s for one of four nuclear reactions,
    !+ad_desc  using the Bosch-Hale parametrization.
    !+ad_desc  <P>The valid range of the fit is 0.2 keV < t < 100 keV
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  11/09/13 PJK Initial version
    !+ad_hist  04/03/14 PJK Prevent division by zero problem if t=0
    !+ad_stat  Okay
    !+ad_docs  Bosch and Hale, Nuclear Fusion 32 (1992) 611-631
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: bosch_hale

    !  Arguments

    real(kind(1.0D0)), intent(in) :: t
    integer, intent(in) :: reaction

    !  Local variables

    integer, parameter :: DT=1, DHE3=2, DD1=3, DD2=4
    real(kind(1.0D0)) :: theta1, theta, xi
    real(kind(1.0D0)), dimension(4) :: bg, mrc2
    real(kind(1.0D0)), dimension(4,7) :: cc

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if  (t == 0.0D0) then
       bosch_hale = 0.0D0
       return
    end if

    !  Gamov constant, BG

    bg(DT)   = 34.3827D0  !  D + T --> 4He + n reaction
    bg(DHE3) = 68.7508D0  !  D + 3He --> 4He + p reaction
    bg(DD1)  = 31.3970D0  !  D + D --> 3He + n reaction
    bg(DD2)  = 31.3970D0  !  D + D --> T + p reaction

    !  Reduced mass of the particles, keV

    mrc2(DT)   = 1.124656D6
    mrc2(DHE3) = 1.124572D6
    mrc2(DD1)  = 0.937814D6
    mrc2(DD2)  = 0.937814D6

    !  Parametrization coefficients

    cc(DT,1) =  1.17302D-9
    cc(DT,2) =  1.51361D-2
    cc(DT,3) =  7.51886D-2
    cc(DT,4) =  4.60643D-3
    cc(DT,5) =  1.35000D-2
    cc(DT,6) = -1.06750D-4
    cc(DT,7) =  1.36600D-5

    cc(DHE3,1) =  5.51036D-10
    cc(DHE3,2) =  6.41918D-3
    cc(DHE3,3) = -2.02896D-3
    cc(DHE3,4) = -1.91080D-5
    cc(DHE3,5) =  1.35776D-4
    cc(DHE3,6) =  0.00000D0
    cc(DHE3,7) =  0.00000D0

    cc(DD1,1) =  5.43360D-12
    cc(DD1,2) =  5.85778D-3
    cc(DD1,3) =  7.68222D-3
    cc(DD1,4) =  0.00000D0
    cc(DD1,5) = -2.96400D-6
    cc(DD1,6) =  0.00000D0
    cc(DD1,7) =  0.00000D0

    cc(DD2,1) =  5.65718D-12
    cc(DD2,2) =  3.41267D-3
    cc(DD2,3) =  1.99167D-3
    cc(DD2,4) =  0.00000D0
    cc(DD2,5) =  1.05060D-5
    cc(DD2,6) =  0.00000D0
    cc(DD2,7) =  0.00000D0

    theta1 = t*(cc(reaction,2) + t*(cc(reaction,4) + t*cc(reaction,6))) / &
         (1.0D0 + t*(cc(reaction,3) + t*(cc(reaction,5) + t*cc(reaction,7))))
    theta = t/(1.0D0 - theta1)

    xi = ((bg(reaction)**2)/(4.0D0*theta))**0.3333333333D0

    !  Volumetric reaction rate <sigma v> (m3/s)

    bosch_hale = 1.0D-6 * cc(reaction,1) * theta * &
         sqrt( xi/(mrc2(reaction)*t**3) ) * exp(-3.0D0*xi)

  end function bosch_hale

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie, &
       ealphadt,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol,zeffai, &
       betanb,dnbeam2,palpnb)

    !+ad_name  beamfus
    !+ad_summ  Routine to calculate beam slowing down properties
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  beamfus0: input real : multiplier for beam-background fusion calculation
    !+ad_args  betbm0 : input real :  leading coefficient for neutral beam beta fraction
    !+ad_args  bp     : input real :  poloidal field (T)
    !+ad_args  bt     : input real :  toroidal field on axis (T)
    !+ad_args  cnbeam : input real :  neutral beam current (A)
    !+ad_args  dene   : input real :  electron density (/m3)
    !+ad_args  deni   : input real :  fuel ion density (/m3)
    !+ad_args  dlamie : input real :  ion-electron coulomb logarithm
    !+ad_args  ealphadt : input real :  alpha particle birth energy (D-T) (keV)
    !+ad_args  enbeam : input real :  neutral beam energy (keV)
    !+ad_args  fdeut  : input real :  deuterium fraction of main plasma
    !+ad_args  ftrit  : input real :  tritium fraction of main plasma
    !+ad_args  ftritbm: input real :  tritium fraction of neutral beam
    !+ad_args  sigvdt : input real :  profile averaged <sigma v> for D-T (m3/s)
    !+ad_args  ten    : input real :  density weighted average electron temperature (keV)
    !+ad_args  tin    : input real :  density weighted average ion temperature (keV)
    !+ad_args  vol    : input real :  plasma volume (m3)
    !+ad_args  zeffai : input real :  mass weighted plasma effective charge
    !+ad_args  betanb : output real : neutral beam beta component
    !+ad_args  dnbeam2: output real : hot beam ion density (/m3)
    !+ad_args  palpnb : output real : alpha power from hot neutral beam ions (MW)
    !+ad_desc  This routine calculates the beam slowing down properties.
    !+ad_prob  None
    !+ad_call  beamcalc
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  05/12/95 PJK Added ealpha to argument list
    !+ad_hist  11/12/95 PJK Added fdeut to argument list
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_hist  03/07/13 PJK Changed zeffai description
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: beamfus0, betbm0, bp, bt, cnbeam, &
         dene, deni, dlamie, ealphadt, enbeam, fdeut, ftrit, ftritbm, &
         sigvdt, ten, tin, vol, zeffai
    real(kind(1.0D0)), intent(out) :: betanb, dnbeam2, palpnb

    !  Local variables

    real(kind(1.0D0)) :: denid,denit,ecritd,ecritt,ehotnb,palpdb, &
         palptb,tausl

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Velocity slowing down time

    tausl = 1.99D19 * (2.0D0*(1.0D0-ftritbm) + 3.0D0*ftritbm) * &
         ten**1.5D0 / dene / dlamie

    !  Critical energy for electron/ion slowing down of the beam ion
    !  (deuterium and tritium neutral beams, respectively) (keV)

    ecritd = 14.8D0 * ten * 2.0D0 * zeffai**0.6666D0 * &
         (dlamie+4.0D0)/dlamie
    ecritt = ecritd * 1.5D0

    !  Deuterium and tritium ion densities

    denid = deni * fdeut
    denit = deni * ftrit

    !  Perform beam calculations

    call beamcalc(denid,denit,ealphadt,enbeam,ecritd,ecritt,tausl, &
         ftritbm,cnbeam,tin,vol,sigvdt,palpdb,palptb,dnbeam2,ehotnb)

    !  Neutral beam alpha power

    palpnb = beamfus0 * (palpdb + palptb)

    !  Neutral beam beta

    betanb = betbm0 * 4.03D-22 * 0.66666D0 * dnbeam2 * ehotnb / &
         (bt**2 + bp**2)

  end subroutine beamfus

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine beamcalc(nd,nt,ealphadt,ebeam,ecritd,ecritt,tausbme, &
       ftritbm,ibeam,ti,vol,svdt,palfdb,palftb,nhot,ehot)

    !+ad_name  beamcalc
    !+ad_summ  Neutral beam alpha power and ion energy
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  palphabm
    !+ad_cont  sgvhot
    !+ad_cont  xbrak
    !+ad_args  ealphadt : input real :  alpha particle birth energy (D-T) (keV)
    !+ad_args  ebeam  : input real :  beam energy (keV)
    !+ad_args  ecritd : input real :  critical energy for electron/ion slowing down of
    !+ad_argc                         the beam ion (deuterium neutral beam) (keV)
    !+ad_args  ecritt : input real :  critical energy for beam slowing down
    !+ad_argc                         (tritium neutral beam) (keV)
    !+ad_args  ftritbm: input real :  beam tritium fraction (0.0 = deuterium beam)
    !+ad_args  ibeam  : input real :  beam current (A)
    !+ad_args  nd     : input real :  thermal deuterium density (/m3)
    !+ad_args  nt     : input real :  thermal tritium density   (/m3)
    !+ad_args  svdt   : input real :  profile averaged <sigma v> for D-T (m3/s)
    !+ad_args  tausbme: input real :  beam ion slowing down time on electrons (s)
    !+ad_args  ti     : input real :  thermal ion temperature (keV)
    !+ad_args  vol    : input real :  plasma volume (m3) (95% flux surface)
    !+ad_args  ehot   : output real : average hot beam ion energy (keV)
    !+ad_args  nhot   : output real : hot beam ion density (/m3)
    !+ad_args  palfdb : output real : alpha power from deut. beam-background fusion (MW)
    !+ad_args  palftb : output real : alpha power from trit. beam-background fusion (MW)
    !+ad_desc  This routine calculates the neutral beam alpha power and ion energy.
    !+ad_prob  None
    !+ad_call  palphabm
    !+ad_call  sgvhot
    !+ad_call  xbrak
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  05/12/95 PJK Added ealpha to argument list
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: ealphadt, ebeam, ecritd, ecritt, &
         ftritbm, ibeam, nd, nt, svdt, tausbme, ti, vol
    real(kind(1.0D0)), intent(out) :: ehot, nhot, palfdb, palftb

    !  Local variables

    integer :: iabm
    real(kind(1.0D0)) :: ebmratd,ebmratt,ehotd,ehott,ifbmd,ifbmt, &
         ndhot,nhotmsd,nhotmst,nthot,presd,prest,s0d,s0t,svdhotn, &
         svthotn,tauseffd,tausefft,vcds,vcritd,vcritt,vcts,xcoefd, &
         xcoeft
    real(kind(1.0D0)) :: atmd,atmt,epsabs,epsrel

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Initialise shared variables

    atmd = 2.0D0   !  atomic mass of deuterium
    atmt = 3.0D0   !  atomic mass of tritium
    epsabs = 1.0D-7  !  absolute error
    epsrel = 1.0D-7  !  relative error

    !  D and T beam current fractions

    ifbmd = ibeam * (1.0D0 - ftritbm)
    ifbmt = ibeam * ftritbm

    ebmratd = ebeam/ecritd
    vcritd = sqrt(2.0D0*echarge*1000.0D0*ecritd/(mproton*atmd))
    tauseffd = tausbme/3.0D0 * log(1.0D0+(ebmratd)**1.5D0)
    nhotmsd = (1.0D0-ftritbm) * ibeam * tauseffd/(echarge * vol)

    ebmratt = ebeam/ecritt
    vcritt = sqrt(2.0D0*echarge*1000.0D0*ecritt/(mproton*atmt))
    tausefft = tausbme/3.0D0 * log(1.0D0+(ebmratt)**1.5D0)
    nhotmst = ftritbm * ibeam * tausefft/(echarge * vol)

    nhot = nhotmsd + nhotmst
    ndhot = nhotmsd
    nthot = nhotmst

    !  Average hot ion energy from Deng & Emmert, UWFDM-718, Jan 87

    vcds = 2.0D0 * ecritd * echarge * 1000.0D0/(2.0D0 * mproton)
    vcts = 2.0D0 * ecritt * echarge * 1000.0D0/(3.0D0 * mproton)

    s0d = ifbmd/(echarge * vol)
    s0t = ifbmt/(echarge * vol)

    xcoefd = atmd * mproton * tausbme * vcds * s0d / &
         (echarge * 1000.0D0 * 3.0D0)
    xcoeft = atmt * mproton * tausbme * vcts * s0t / &
         (echarge * 1000.0D0 * 3.0D0)

    presd = xcoefd * xbrak(ebeam,ecritd)
    prest = xcoeft * xbrak(ebeam,ecritt)

    ehotd = 1.5D0 * presd/ndhot
    ehott = 1.5D0 * prest/nthot
    ehot = (ndhot*ehotd + nthot*ehott)/nhot

    iabm = 2 ; svdhotn = 1.0D-4 * sgvhot(iabm,vcritd,ebeam)
    iabm = 3 ; svthotn = 1.0D-4 * sgvhot(iabm,vcritt,ebeam)

    palfdb = palphabm(ealphadt,ndhot,nt,svdhotn,vol,ti,svdt)
    palftb = palphabm(ealphadt,nthot,nd,svthotn,vol,ti,svdt)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function xbrak(e0,ec)

      !+ad_name  xbrak
      !+ad_summ  Hot ion energy parameter
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  e0 : input real :  neutral beam energy (keV)
      !+ad_args  ec : input real :  critical energy for electron/ion slowing down of
      !+ad_argc                     the beam ion (keV)
      !+ad_desc  This routine calculates something to do with the hot ion energy...
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
      !+ad_hist  10/11/11 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: xbrak

      !  Arguments

      real(kind(1.0D0)), intent(in) :: e0, ec

      !  Local variables

      real(kind(1.0D0)) :: ans,t1,t2,t3,t4,xarg,xc,xcs

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      xcs = e0/ec
      xc = sqrt(xcs)

      t1 = xcs/2.0D0
      t2 = (log((xcs + 2.0D0*xc + 1.0D0)/(xcs - xc + 1.0D0)))/6.0D0

      xarg = (2.0D0*xc -1.0D0)/sqrt(3.0D0)
      t3 = (atan(xarg))/sqrt(3.0D0)
      t4 = 0.3022999D0

      ans = t1 + t2 - t3 - t4
      xbrak = ans

    end function xbrak

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function palphabm(ealphadt,nbm,nblk,sigv,vol,ti,svdt)

      !+ad_name  palphabm
      !+ad_summ  Alpha power from beam-background fusion
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  ealphadt : input real :  alpha particle birth energy (D-T) (keV)
      !+ad_args  nblk   : input real :  thermal ion density (/m3)
      !+ad_args  nbm    : input real :  hot beam ion density (/m3)
      !+ad_args  sigv   : input real :  hot beam fusion reaction rate (m3/s)
      !+ad_args  svdt   : input real :  profile averaged <sigma v> for D-T (m3/s)
      !+ad_args  ti     : input real :  thermal ion temperature (keV)
      !+ad_args  vol    : input real :  plasma volume (m3)
      !+ad_desc  This routine calculates the alpha power from
      !+ad_desc  beam-background fusion.
      !+ad_prob  None
      !+ad_call  bosch_hale
      !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
      !+ad_hist  05/12/95 PJK Moved ealpha to argument list
      !+ad_hist  10/11/11 PJK Initial F90 version
      !+ad_hist  12/09/13 PJK Replaced svfdt usage with bosch_hale
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: palphabm

      !  Arguments

      real(kind(1.0D0)), intent(in) :: ealphadt,nblk,nbm,sigv,svdt,ti,vol

      !  Local variables

      real(kind(1.0D0)) :: ratio

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ratio = svdt / bosch_hale(ti,1)

      palphabm = echarge/1000.0D0 * nbm * nblk * sigv * ealphadt * vol * ratio

    end function palphabm

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function sgvhot(iabm,vcrx,ebeam)

      !+ad_name  sgvhot
      !+ad_summ  Hot beam fusion reaction rate
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  ebeam  : input real :  neutral beam energy (keV)
      !+ad_args  iabm   : input integer : switch denoting type of ion (2=D,3=T)
      !+ad_args  vcrx   : input real :  critical velocity for electron/ion slowing down of
      !+ad_argc                         the beam ion (m/s)
      !+ad_desc  This routine calculates the hot beam fusion reaction rate in m3/s.
      !+ad_prob  None
      !+ad_call  fsv
      !+ad_call  quanc8
      !+ad_call  report_error
      !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
      !+ad_hist  10/11/11 PJK Initial F90 version
      !+ad_hist  26/06/14 PJK Added error handling
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: sgvhot

      !  Arguments

      integer, intent(in) :: iabm
      real(kind(1.0D0)), intent(in) :: ebeam, vcrx

      !  Local variables

      integer :: nofun
      real(kind(1.0D0)) :: abm,abserr,epsabs1,flag,svint,t1,t2, &
           vbeam,vbeams,xv

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      epsabs1 = 1.0D-33

      if (iabm == 2) then
         abm = atmd
      else if (iabm == 3) then
         abm = atmt
      else
         idiags(1) = iabm ; call report_error(84)
      end if

      !  Initialise global variables

      vcritx = vcrx

      !  Beam velocity

      vbeams = ebeam * echarge * 1000.0D0 * 2.0D0/(abm * mproton)
      vbeam = sqrt(vbeams)

      xv = vbeam/vcrx
      t1 = 3.0D0 * vcrx/log(1.0D0+(xv**3))

      call quanc8(fsv,0.0D0,xv,epsabs1,epsrel,svint,abserr,nofun,flag)
      t2 = svint

      sgvhot = t1 * t2

    end function sgvhot

  end subroutine beamcalc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function fsv(u)

    !+ad_name  fsv
    !+ad_summ  Integrand function for the hot beam fusion reaction rate
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  sigbmfus
    !+ad_args  u : input real : abscissa of integration, = ratio of beam velocity
    !+ad_argc                   to the critical velocity
    !+ad_desc  This is the integrand function for the hot beam fusion reaction rate.
    !+ad_prob  None
    !+ad_call  sigbmfus
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: fsv

    !  Arguments

    real(kind(1.0D0)), intent(in) :: u

    !  Local variables

    real(kind(1.0D0)) :: t1,t2,xvc,xvcs

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    t1 = (u**3)/(1.0D0+u**3)

    !  vcritx : critical velocity for electron/ion slowing down of beam ion (m/s)

    xvc = vcritx*u
    xvcs = xvc * xvc * mproton/(echarge * 1000.0D0)
    t2 = sigbmfus(xvcs)

    fsv = t1 * t2

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function sigbmfus(vrelsq)

      !+ad_name  sigbmfus
      !+ad_summ  Fusion reaction cross-section
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  vrelsq : input real :  square of the speed of the beam ion (keV/amu)
      !+ad_desc  This function evaluates the fusion reaction cross-section as a
      !+ad_desc  function of beam ion velocity (squared).
      !+ad_desc  The functional form of the cross-section is in terms of the equivalent
      !+ad_desc  deuterium energy, i.e. for a tritium beam at 500 keV the energy
      !+ad_desc  used in the cross-section function is 333 keV.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
      !+ad_hist  10/11/11 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: sigbmfus

      !  Arguments

      real(kind(1.0D0)), intent(in) :: vrelsq

      !  Local variables

      real(kind(1.0D0)) :: a1,a2,a3,a4,a5,atmd,ebm,t1,t2

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      a1 = 45.95D0
      a2 = 5.02D4
      a3 = 1.368D-2
      a4 = 1.076D0
      a5 = 4.09D2

      !  Deuterium atomic mass

      atmd = 2.0D0

      !  Beam kinetic energy

      ebm = 0.5D0 * atmd * vrelsq

      !  Set limits on cross-section at low and high beam energies

      if (ebm < 10.0D0) then
         sigbmfus = 1.0D-27
      else if (ebm > 1.0D4) then
         sigbmfus = 8.0D-26
      else
         t1 = a2/(1.0D0 + (a3 * ebm - a4)**2) + a5
         t2 = ebm * (exp (a1/sqrt(ebm)) - 1.0D0)
         sigbmfus = 1.0D-24 * t1/t2
      end if

    end function sigbmfus

  end function fsv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function t_eped_scaling()
    !+ad_name  t_eped_scaling
    !+ad_summ  Scaling function for calculation of pedestal temperature
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This function calculates pedestal temperature using a scaling formula
    !+ad_desc  Issue #413.  See also comment dated 7/8/17
    !+ad_desc  Predictive pedestal modelling for DEMO,  Samuli Saarelma.
    !+ad_prob  None
    !+ad_call  None
    !+ad_stat  Okay
    !+ad_docs  https://idm.euro-fusion.org/?uid=2MSZ4T
    !

    real(kind(1.0D0)) :: t_eped_scaling
    ! Scaling constant and exponents
    real(kind(1.0D0)) :: c0, a_delta, a_ip, a_r, a_beta, a_kappa, a_a

    c0 = 2.16d0
    a_delta = 0.82D0
    a_ip = 0.26D0
    a_r = -0.39D0
    a_beta = 0.43D0
    a_kappa = 0.50d0
    a_a = 0.88D0

    !corrected_n_tot_beta = normalised_total_beta * 1.2566
    ! KE, 19/06/18 Reverting correction noted below. Samuli decided this
    ! was not an error afterall.
    ! KE, 25/04/18 Correction to normalised_total _beta applied as specified in the
    ! email from Samuli which is reproduced in issue #413

    ! Correction for single null and for ELMs = 0.65
    ! Elongation and triangularity are defined at the plasma boundary.
    ! Total normalised plasma beta is used.

    t_eped_scaling =  0.65d0 * c0 * triang**a_delta * (plascur/1.0d6)**a_ip * rmajor**a_r * &
         kappa**a_kappa  * normalised_total_beta**a_beta  * rminor**a_a
    !Issue #730 - add scaling factor to eped model
    t_eped_scaling = eped_sf * t_eped_scaling
  end function t_eped_scaling

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function p_eped_scaling(betan_pl,kappa_pl,delta_pl,ip_pl)
    !+ad_name  p_eped_scaling
    !+ad_summ  Scaling function for calculation of pedestal pressure
    !+ad_type  Function returning real
    !+ad_auth  E Fable, Max Planck Institute of Plasma Physics Garching
    !+ad_cont  N/A
    !+ad_args  This currently has PLASMOD inputs - could they come from global variables?
    !+ad_desc  This function calculates pedestal pressure using a scaling formula
    !+ad_desc  Issue #413.  See also comment dated 7/8/17
    !+ad_desc  Predictive pedestal modelling for DEMO,  Samuli Saarelma.
    !+ad_prob  None
    !+ad_call  None
    !+ad_stat  Okay
    !+ad_docs  https://idm.euro-fusion.org/?uid=2MSZ4T

    real(kind(1.0D0)) :: p_eped_scaling !pressure in kev*10¹9*m¯3
    ! Scaling constant and exponents
    real(kind(1.0D0)) :: c0, a_delta, a_ip, a_r, a_beta, a_kappa, a_a
    real(kind(1.0D0)) :: betan_pl,kappa_pl,delta_pl,ip_pl

    c0 = 9.4d0
    a_delta = 0.82D0
    a_ip = 1.25D0
    a_r = -0.39D0
    a_beta = 0.43D0
    a_kappa = 0.50d0
    a_a = -1.11d0

    !corrected_n_tot_beta = betan_pl * 1.2566
    ! KE, 19/06/18 Reverting correction noted below. Samuli decided this
    ! was not an error afterall.
    ! KE, 25/04/18 Correction to normalised_total _beta applied as specified in the
    ! email from Samuli which is reproduced in issue #413

    ! Correction for single null and for ELMs = 0.65
    ! Elongation and triangularity are defined at the plasma boundary.
    ! Total normalised plasma beta is used.

    p_eped_scaling =  0.65d0 * c0 * delta_pl**a_delta * ip_pl**a_ip * rmajor**a_r * &
         kappa_pl**a_kappa  * betan_pl**a_beta * rminor**a_a
    !Issue #730 - add scaling factor to eped model
    p_eped_scaling = eped_sf * p_eped_scaling
  end function p_eped_scaling

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine radpwr(imprad_model,pbrempv,plinepv,psyncpv,pcoreradpv,pedgeradpv,pradpv)

    !+ad_name  radpwr
    !+ad_summ  Radiation power interface routine
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  imprad_model : input integer : switch to choose model
    !+ad_args  pbrempv    : output real : bremsstrahlung radiation power/volume (MW/m3)
    !+ad_args  plinepv    : output real : line radiation power/volume (MW/m3)
    !+ad_args  psyncpv    : output real : synchrotron radiation power/volume (MW/m3)
    !+ad_args  pcoreradpv : output real : total core radiation power/volume (MW/m3)
    !+ad_args  pedgeradpv : output real : edge (non-core) radiation power/volume (MW/m3)
    !+ad_args  pradpv     : output real : total radiation power/volume (MW/m3)
    !+ad_desc  This routine finds the radiation powers in MW/m3 by calling
    !+ad_desc  relevant routines.
    !+ad_call  prad_ipdg89
    !+ad_call  psync_albajar_fidone
    !+ad_call  imprad
    !+ad_call  report_error
    !+ad_hist  14/05/14 PJK Redefined routine as a caller to the actual calculations
    !+ad_hist  20/05/14 PJK Clarified core radiation vs bremsstrahlung
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: imprad_model
    real(kind(1.0D0)), intent(out) :: pbrempv,plinepv,psyncpv,pcoreradpv, &
         pedgeradpv,pradpv

    !  Local variables

    real(kind(1.0D0)) :: pimpcore, pimptot

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Bremsstrahlung and line radiation

    if (imprad_model == 0) then
       call prad_ipdg89(pimpcore, pedgeradpv)
       pimptot = pimpcore + pedgeradpv
       pbrempv = 0.0D0 ; plinepv = 0.0D0  !  therefore, not useful...
    else if (imprad_model == 1) then
       call imprad(pbrempv, plinepv, pimpcore, pimptot)
       pedgeradpv = pimptot - pimpcore
    else
       idiags(1) = imprad_model ; call report_error(82)
    end if

    !  Synchrotron radiation power/volume; assumed to be from core only

    call psync_albajar_fidone(psyncpv)

    !  Total core radiation power/volume

    pcoreradpv = pimpcore + psyncpv

    !  Total radiation power/volume

    pradpv = pimptot + psyncpv  !  = pcoreradpv + pedgeradpv

  end subroutine radpwr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine prad_ipdg89(pcoreradpv,pedgeradpv)

    !+ad_name  prad_ipdg89
    !+ad_summ  Bremsstrahlung and line radiation power calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  pcoreradpv : output real : core radiation power/volume (MW/m3)
    !+ad_args  pedgeradpv : output real : edge line radiation power/volume (MW/m3)
    !+ad_desc  This routine finds the D-T and impurity bremsstrahlung and line
    !+ad_desc  radiation powers in MW/m3, using the IPDG89 formulation.
    !+ad_prob  No account is taken of pedestal profiles.
    !+ad_call  None
    !+ad_hist  14/05/14 PJK Moved bremsstrahlung calculation here from original
    !+ad_hisc               <CODE>radpwr</CODE> routine
    !+ad_hist  19/05/14 PJK Renamed arguments
    !+ad_hist  20/05/14 PJK Renamed routine from pbrems_ipdg89
    !+ad_stat  Okay
    !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
    !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: pcoreradpv, pedgeradpv

    !  Local variables

    real(kind(1.0D0)) :: den20,fbc,fbhe,fbo,pbremdt,pbremz,pc,phe, &
         phighz,po,radexp,t10,vr

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    fbhe = 0.9D0
    fbc = 0.52D0
    fbo = 0.52D0

    den20 = dene/1.0D20
    t10 = ten/10.0D0

    !  D-T bremsstrahlung (IPDG89)
    !  Coefficient 0.016*radexp is C_B in IPDG89, with Zeff set to 1 for D-T
    !  Note that the formula in IPDG89 should use ni/1.0E20 * ne/1.0E20,
    !  not just (n20)^2 (the code below is correct)

    radexp = (1.0D0 + alphan)**1.5D0 * sqrt(1.0D0 + alphan + alphat) / &
         (1.0D0 + 2.0D0*alphan + 0.5D0*alphat)

    pbremdt = 1.6D-2 * radexp * den20**2 * (deni/dene) * sqrt(t10)

    !  High Z bremsstrahlung

    vr = rmajor * (rminor*(1.0D0 + kappa95)/2.0D0)**2 / (58.652D0*vol)
    phe = 65.8D0 * ralpne * (dene/7.0D19)**1.5D0 * vr
    pc  = 1120.0D0 * rncne * (dene/7.0D19)**1.5D0 * vr
    po  = 2240.0D0 * rnone * (dene/7.0D19)**1.5D0 * vr
    if (zfear == 1) then  !  high-Z impurity is argon
       phighz = 16000.0D0 * rnfene * (dene/7.0D19)**1.5D0 * vr
    else  !  iron
       phighz = 44800.0D0 * rnfene * (dene/7.0D19)**2.5D0 * vr
    end if
    pbremz = fbhe*phe + fbc*pc + fbo*po + fbfe*phighz

    !  Total core radiation power (this is a more accurate description than
    !  simply the bremsstrahlung power)

    pcoreradpv = pbremz + pbremdt

    !  Edge line radiation

    pedgeradpv = (1.0D0-fbhe)*phe + (1.0D0-fbc)*pc + (1.0D0-fbo)*po + &
         (1.0D0-fbfe)*phighz

  end subroutine prad_ipdg89

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine psync_albajar_fidone(psyncpv)

    !+ad_name  psync_albajar_fidone
    !+ad_summ  Synchrotron radiation power calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  psyncpv  : output real : synchrotron radiation power/volume (MW/m3)
    !+ad_desc  This routine finds the synchrotron radiation power in MW/m3,
    !+ad_desc  using the method of Albajar and Fidone.
    !+ad_prob  No account is taken of pedestal profiles.
    !+ad_call  None
    !+ad_hist  14/05/14 PJK Moved synchrotron calculation here from original
    !+ad_hisc               <CODE>radpwr</CODE> routine
    !+ad_stat  Okay
    !+ad_docs  Albajar, Nuclear Fusion 41 (2001) 665
    !+ad_docs  Fidone, Giruzzi, Granata, Nuclear Fusion 41 (2001) 1755
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: psyncpv

    !  Local variables

    real(kind(1.0D0)) :: de2o,dum,gfun,kap,kfun,pao,psync,rpow,tbet

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  tbet is betaT in Albajar, not to be confused with plasma beta

    tbet = 2.0D0

    !  rpow is the (1-Rsyn) power dependence based on plasma shape
    !  (see Fidone)

    rpow = 0.62D0

    kap = vol / (2.0D0 * pi**2 * rmajor * rminor**2)

    !  No account is taken of pedestal profiles here, other than use of
    !  the correct ne0 and te0...

    de2o = 1.0D-20*ne0
    pao = 6.04D3 * (rminor*de2o)/bt
    gfun = 0.93D0 * ( 1.0D0 + 0.85D0*exp(-0.82D0 * rmajor/rminor) )
    kfun = (alphan + 3.87D0*alphat + 1.46D0)**(-0.79D0)
    kfun = kfun * (1.98D0+alphat)**1.36D0 * tbet**2.14D0
    kfun = kfun*(tbet**1.53D0 + 1.87D0*alphat - 0.16D0)**(-1.33D0)
    dum = (1.0D0+0.12D0*(te0/(pao**0.41D0))*(1.0D0-ssync)**0.41D0)

    !  Very high T modification, from Fidone

    dum = dum**(-1.51D0)

    psync = 3.84D-8 * (1.0D0-ssync)**rpow * rmajor * rminor**1.38D0
    psync = psync * kap**0.79D0 * bt**2.62D0 * de2o**0.38D0
    psync = psync * te0 *(16.0D0+te0)**2.61D0 * dum * gfun * kfun

    !  psyncpv should be per unit volume; Albajar gives it as total

    psyncpv = psync/vol

  end subroutine psync_albajar_fidone

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine imprad(radb, radl, radcore, radtot)

    !+ad_name  imprad
    !+ad_summ  Total impurity line radiation and bremsstrahlung
    !+ad_type  Subroutine
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  radb    : output real : bremsstrahlung only (MW/m3)
    !+ad_args  radl    : output real : line radiation only (MW/m3)
    !+ad_args  radcore : output real : total impurity radiation from core (MW/m3)
    !+ad_args  radtot  : output real : total impurity radiation (MW/m3)
    !+ad_desc  This routine calculates the total radiation losses from
    !+ad_desc  impurity line radiation and bremsstrahlung for all elements
    !+ad_desc  for a given temperature and density profile.
    !+ad_desc  <P>Bremsstrahlung equation from Johner
    !+ad_desc  <P>L(z) data (coronal equilibrium) from Marco Sertoli, ASDEX-U,
    !+ad_desc  ref. Kallenbach et al.
    !+ad_prob  None
    !+ad_call  Tprofile
    !+ad_call  nprofile
    !+ad_call  impradprofile
    !+ad_call  fradcore
    !+ad_hist  17/12/13 HL  First draft of routine, based on code by R Kemp
    !+ad_hist  09/05/14 HL  Using new data structure
    !+ad_hist  14/05/14 PJK First PROCESS implementation
    !+ad_hist  19/05/14 PJK Added call to fradcore; radtot now an output arg
    !+ad_stat  Okay
    !+ad_docs  Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !+ad_docs  Sertoli, private communication
    !+ad_docs  Kallenbach et al., Plasma Phys. Control. Fus. 55 (2013) 124041
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Arguments

    real(kind(1.0D0)), intent(out) :: radb, radl, radcore, radtot

    !  Local variables

    real(kind(1.0D0)) :: rho, drho, trho,  nrho
    real(kind(1.0D0)) :: pimp, pbrem, pline
    integer :: i, imp, npts

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    npts = 200  !  originally 1000; no significant difference found
    drho = 1.0D0/real(npts,kind(1.0D0))

    radtot = 0.0D0
    radcore = 0.0D0
    radb = 0.0D0
    radl = 0.0D0

    !  Numerical integration using the midpoint rule
    !  Consider using the maths_library integrator in the future...
    !    quanc8(fun,0.0D0,1.0D0,abserr,relerr,result,errest,nofun,flag)

    do i = 0, npts-1

       rho = (0.5D0 + i)/npts
       trho = tprofile(rho, rhopedt, te0, teped, tesep, alphat, tbeta)
       nrho = nprofile(rho, rhopedn, ne0, neped, nesep, alphan)

       do imp = 1, size(impurity_arr)

          if (impurity_arr(imp)%frac > 1.0D-30) then

             call impradprofile(impurity_arr(imp), nrho, trho, pimp, pbrem, pline)

             radtot  = radtot  + pimp*rho
             radcore = radcore + pimp*rho * fradcore(rho,coreradius,coreradiationfraction)
             radb = radb + pbrem*rho
             radl = radl + pline*rho
          end if

       end do
    end do

    !  Radiation powers in MW/m3

    radtot  = 2.0D-6 * drho * radtot
    radcore = 2.0D-6 * drho * radcore
    radb    = 2.0D-6 * drho * radb
    radl    = 2.0D-6 * drho * radl

  end subroutine imprad

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end module physics_functions_module
