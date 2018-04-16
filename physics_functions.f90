module physics_functions_module

  !+ad_name  physics_functions_module
  !+ad_summ  Module containing physics subfunctions
  !+ad_type  Module
  !+ad_auth  K Ellis, CCFE, Culham Science Centre
  !+ad_cont  physics_functions
  !+ad_cont  pthresh
  !+ad_args  N/A
  !+ad_desc  This module contains physics routines which can be called by physics or 
  !+ad_desc  other modules (e.g. PLASMOD).
  !+ad_prob  None
  !!!!+ad_call  build_variables

  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !use global_variables

  implicit none

  !private
  public

  !  Module-level variables

  !integer :: 
  !real(kind(1.0D0)) :: 

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  subroutine pthresh(dene,dnla,bt,rmajor,kappa,sarea,aion,pthrmw)
    
    !+ad_name  pthresh
    !+ad_summ  L-mode to H-mode power threshold calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  dene   : input real :  volume-averaged electron density (/m3)
    !+ad_args  dnla   : input real :  line-averaged electron density (/m3)
    !+ad_args  bt     : input real :  toroidal field on axis (T)
    !+ad_args  rmajor : input real :  plasma major radius (m)
    !+ad_args  kappa  : input real :  plasma elongation
    !+ad_args  sarea  : input real :  plasma surface area (m**2)
    !+ad_args  aion   : input real :  average mass of all ions (amu)
    !+ad_args  pthrmw(8) : output real array : power threshold (different scalings)
    !+ad_desc  This routine calculates the power threshold for the L-mode to
    !+ad_desc  H-mode transition.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  17/07/98 PJK New routine
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_hist  18/12/12 PJK Added scalings 6-8
    !ad_hist   16/04/18 KVE Cut from physics and pasted into physics_functions
    !+ad_stat  Okay
    !+ad_docs  ITER Physics Design Description Document, p.2-2
    !+ad_docs  ITER-FDR Plasma Performance Assessments, p.III-9
    !+ad_docs  Snipes, 24th EPS Conference, Berchtesgaden 1997, p.961
    !+ad_docs  Martin et al, 11th IAEA Tech. Meeting on H-mode Physics and
    !+ad_docc  Transport Barriers, Journal of Physics: Conference Series
    !+ad_docc  123 (2008) 012033
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: dene,dnla,bt,rmajor,kappa,sarea,aion
    real(kind(1.0D0)), dimension(8), intent(out) :: pthrmw

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

  end subroutine pthresh

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  end module physics_functions_module
