module superconductors
  !+ad_name  superconductors
  !+ad_summ  Module containing superconducter critical surfaces and cable data
  !+ad_type  Module

  use process_output
  use error_handling
  use rebco_variables
  use resistive_materials



contains

! -------------------------------------------------------------------

subroutine jcrit_rebco(temperature, b, jcrit, validity, iprint)

    !+ad_name  jcrit_rebco
    !+ad_summ  Critical surface for "REBCO" 2nd generation HTS superconductor
    !+ad_type  Subroutine
    !+ad_args  temperature : input real : superconductor temperature (K)
    !+ad_args  bnormal : input real : Magnetic field at superconductor (T)
    !+ad_args  jcrit : output real : Critical current density in superconductor (A/m2)
    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(in) :: temperature, b
    real(kind(1.0D0)), intent(out) :: jcrit
    logical, intent(out) :: validity
    integer, intent(in) :: iprint

    !  Local variables
    real(kind(1.0D0)) :: birr, factor

    !  Parameters
    real(kind(1.0D0)), parameter :: tc0 = 90.0d0        !  (K)
    real(kind(1.0D0)), parameter :: birr0 = 132.5d0     !  (T)
    real(kind(1.0D0)), parameter :: a = 1.82962d8       ! scaling constant
    real(kind(1.0D0)), parameter :: p = 0.5875d0        ! exponent
    real(kind(1.0D0)), parameter :: q = 1.7d0           ! exponent
    real(kind(1.0D0)), parameter :: alpha =1.54121d0    ! exponent
    real(kind(1.0D0)), parameter :: beta = 1.96679d0    ! exponent

    validity = .true.
    if((temperature<4.2d0).or.(temperature>72.0d0) )validity = .false.
    if(temperature<65)then
        if((b<0.0d0).or.(b>15.0d0))validity = .false.
    else
        if((b<0.0d0).or.(b>11.5d0))validity = .false.
    endif

    if((iprint==1).and.(.not.validity))then
        write(*,*)'ERROR in subroutine jcrit_rebco: input out of range'
        write(*,*)'temperature = ', temperature, '   Field = ', b
    endif

    birr = birr0 * (1 - temperature/tc0)**alpha
    factor = (b/birr)**p * (1-b/birr)**q
    jcrit = (a/b) * birr**beta *factor

end subroutine jcrit_rebco
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine test_rebco()
    real(kind(1.0D0)) :: jcrit
    logical :: validity

    call jcrit_rebco(14.0d0, 15.0d0, jcrit, validity, 0)
    write(*,*)'TEST of subroutine jcrit_rebco:'
    write(*,*)'temperature = 14 K,   Field = 15 T, validity = ', validity
    write(*,10)'jcrit =  ', jcrit
10  format(1x,a,1pe10.3)
    write(*,*)'Critical current from Superpower plot (approx) =  2.5e10 A/m2'
end subroutine test_rebco
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine itersc(thelium,bmax,strain,bc20max,tc0max,jcrit,bcrit,tcrit)

  !+ad_name  itersc
  !+ad_summ  Implementation of ITER Nb3Sn critical surface implementation
  !+ad_type  Subroutine
  !+ad_auth  R Kemp, CCFE, Culham Science Centre
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  thelium : input real : Coolant/SC temperature (K)
  !+ad_args  bmax : input real : Magnetic field at conductor (T)
  !+ad_args  strain : input real : Strain in superconductor
  !+ad_args  bc20max : input real : Upper critical field (T) for superconductor
  !+ad_argc                      at zero temperature and strain
  !+ad_args  tc0max : input real : Critical temperature (K) at zero field and strain
  !+ad_args  jcrit : output real : Critical current density in superconductor (A/m2)
  !+ad_args  bcrit : output real : Critical field (T)
  !+ad_args  tcrit : output real : Critical temperature (K)
  !+ad_desc  This routine calculates the critical current density and
  !+ad_desc  temperature in the superconducting TF coils using the
  !+ad_desc  ITER Nb3Sn critical surface model.
  !+ad_prob  None
  !+ad_hist  18/11/15 RK  Updated to latest WPMAG coefficients (personal communication from Louis Zani describing TFEU4)
  !+ad_stat  Okay
  !+ad_docs  $J_C(B,T,\epsilon)$ Parameterization for ITER Nb3Sn production,
  !+ad_docc    L. Bottura, CERN-ITER Collaboration Report, Version 2, April 2nd 2008
  !+ad_docc    (distributed by Arnaud Devred, ITER, 10th April 2008)
  !+ad_docs  ITER Nb3Sn critical surface parameterization (2MMF7J) (2008),
  !+ad_docc    https://user.iter.org/?uid=2MMF7J&action=get_document
  !+ad_docs  ITER DDD 11-7: Magnets - conductors (2NBKXY) (2009),
  !+ad_docc    https://user.iter.org/?uid=2NBKXY&action=get_document
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  !  Arguments

  real(kind(1.0D0)), intent(in) :: thelium, bmax, strain, bc20max, tc0max
  real(kind(1.0D0)), intent(out) :: jcrit, bcrit, tcrit

  !  Local variables

  !  Parameters named in Bottura

  !real(kind(1.0D0)), parameter :: csc = 16500.0D6 !  scaling constant C
  real(kind(1.0D0)), parameter :: csc = 19922.0D6 !  scaling constant C
  real(kind(1.0D0)), parameter :: p = 0.63D0      !  low field exponent p
  real(kind(1.0D0)), parameter :: q = 2.1D0       !  high field exponent q
  !real(kind(1.0D0)), parameter :: ca1 = 44.0D0    !  strain fitting constant C_{a1}
  real(kind(1.0D0)), parameter :: ca1 = 44.48D0    !  strain fitting constant C_{a1}
  !real(kind(1.0D0)), parameter :: ca2 = 4.0D0     !  strain fitting constant C_{a2}
  real(kind(1.0D0)), parameter :: ca2 = 0.0D0     !  strain fitting constant C_{a2}
  real(kind(1.0D0)), parameter :: eps0a = 0.00256D0  !  epsilon_{0,a}
  !real(kind(1.0D0)), parameter :: epsmax = -0.003253075D0  !  epsilon_{max} (not used)
  !real(kind(1.0D0)), parameter :: epsmax = -1.1D-3  !  epsilon_{max} (not used)

  real(kind(1.0D0)), parameter :: diter = 0.82D0  !  ITER strand diameter (mm)
  real(kind(1.0D0)), parameter :: cuiter = 0.5D0  !  ITER strand copper fraction

  real(kind(1.0D0)) :: bred, epssh, t, bc20eps, &
       tc0eps, bzero, strfun, jc1, jc2, jc3, scalefac

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  $\epsilon_{sh}$
  epssh = (ca2*eps0a)/(sqrt(ca1**2 - ca2**2))

  !  Strain function $s(\epsilon)$
  !  0.83 < s < 1.0, for -0.005 < strain < 0.005
  strfun = sqrt(epssh**2 + eps0a**2) - sqrt((strain-epssh)**2 + eps0a**2)
  strfun = strfun*ca1 - ca2*strain
  strfun = 1.0D0 + (1.0D0/(1.0D0 - ca1*eps0a))*strfun

  !  $B^*_{C2} (0,\epsilon)$
  bc20eps = bc20max*strfun

  !  $T^*_C (0,\epsilon)$
  tc0eps = tc0max * strfun**(1.0D0/3.0D0)

  !  Reduced temperature, restricted to be < 1
  !  Should remain < 1 for thelium < 0.94*tc0max (i.e. 15 kelvin for isumattf=1)

  if (thelium/tc0eps >= 1.0D0) then
     fdiags(1) = thelium ; fdiags(2) = tc0eps
     call report_error(159)
  end if
  t = min(thelium/tc0eps, 0.9999D0)

  !  Reduced magnetic field at zero temperature
  !  Should remain < 1 for bmax < 0.83*bc20max (i.e. 27 tesla for isumattf=1)

  if (bmax/bc20eps >= 1.0D0) then
     fdiags(1) = bmax ; fdiags(2) = bc20eps
     call report_error(160)
  end if
  bzero = min(bmax/bc20eps, 0.9999D0)

  !  Critical temperature (K)
  tcrit = tc0eps * (1.0D0 - bzero)**(1.0D0/1.52D0)  !  bzero must be < 1 to avoid NaNs

  !  Critical field (T)
  bcrit = bc20eps * (1.0D0 - t**1.52D0)

  !  Reduced magnetic field, restricted to be < 1
  if (bmax/bcrit >= 1.0D0) then
     fdiags(1) = bmax ; fdiags(2) = bcrit
     call report_error(161)
  end if
  bred = min(bmax/bcrit, 0.9999D0)

  !  Critical current density in superconductor (A/m2)
  !  ITER parameterization is for the current in a single strand,
  !  not per unit area, so scalefac converts to current density

  scalefac = pi * (0.5D0*diter)**2 * (1.0D0-cuiter)

  jc1 = (csc/bmax)*strfun
  jc2 = (1.0D0-t**1.52D0) * (1.0D0-t**2)  !  t must be < 1 to avoid NaNs
  jc3 = bred**p * (1.0D0-bred)**q  !  bred must be < 1 to avoid NaNs

  jcrit = jc1 * jc2 * jc3 / scalefac

end subroutine itersc

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine bi2212(bmax,jstrand,tsc,fhts,jcrit,tmarg)

    !+ad_name  bi2212
    !+ad_summ  Fitted parameterization to Bi-2212 superconductor properties
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  bmax    : input real : Magnetic field at conductor (T)
    !+ad_args  jstrand : input real : Current density in strand (A/m2)
    !+ad_args  tsc     : input real : Superconductor temperature (K)
    !+ad_args  fhts    : input real : Adjustment factor (<= 1) to account for strain,
    !+ad_argc                         radiation damage, fatigue or AC losses
    !+ad_args  jcrit : output real : Critical current density in strand (A/m2)
    !+ad_args  tmarg : output real : Temperature margin (K)
    !+ad_desc  This routine calculates the critical current density and
    !+ad_desc  the temperature margin for Bi-2212 superconductor in the TF coils
    !+ad_desc  using a fit by M. Kovari to measurements described in the reference,
    !+ad_desc  specifically from the points shown in Figure 6.
    !+ad_desc  <P>Bi-2212 (Bi<SUB>2</SUB>Sr<SUB>2</SUB>CaCu<SUB>2</SUB>O<SUB>8-x</SUB>)
    !+ad_desc  is a first-generation high temperature superconductor; it still needs
    !+ad_desc  to be operated below about 10K, but remains superconducting at much
    !+ad_desc  higher fields at that temperature than Nb3Sn etc.
    !+ad_desc  The model's range of validity is T &lt; 20K, adjusted field
    !+ad_desc  b &lt; 104 T, B &gt; 6 T.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  08/10/13 PJK Initial version
    !+ad_hist  05/03/14 PJK Added comment about range of validity
    !+ad_hist  06/03/14 PJK Added warning if range of validity is violated
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  A transformative superconducting magnet technology for fields well
    !+ad_docc  above 30 T using isotropic round wire multifilament
    !+ad_docc  Bi2Sr2CaCu2O8-x conductor, D. C. Larbalestier et al., preprint,
    !+ad_docc  9th April 2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: bmax, jstrand, tsc, fhts
    real(kind(1.0D0)), intent(out) :: jcrit, tmarg

    !  Local variables

    real(kind(1.0D0)) :: b

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Adjusted field (T)

    b = bmax / exp(-0.168D0*(tsc-4.2D0))

    !  Engineering (i.e. strand) critical current density (A/m2)

    jcrit = fhts * (1.175D9*exp(-0.02115D0*b) - 1.288D8)

    !  Temperature margin (K)
    !  Simple inversion of above calculation, using actual current density
    !  in strand instead of jcrit

    tmarg = 1.0D0/0.168D0 * &
    log( log(1.175D9/(jstrand/fhts + 1.288D8)) / (0.02115*bmax) ) &
    + 4.2D0 - tsc

    !  Check if ranges of validity have been violated

    if ((tsc > 20.0D0).or.(bmax < 6.0D0).or.(b > 104.0D0)) then
        fdiags(1) = tsc ; fdiags(2) = bmax ; fdiags(3) = b
        call report_error(106)
        write(*,*) 'Warning in routine BI2212:'
        write(*,*) 'Range of validity of the HTS Bi-2212 model has been violated:'
        write(*,*) '   S/C temperature (K) = ',tsc, ' (should be < 20 K)'
        write(*,*) 'Field at conductor (T) = ',bmax, ' (should be > 6 T)'
        write(*,*) '    Adjusted field (T) = ',b, ' (should be < 104 T)'
        write(*,*) ' '
    end if

end subroutine bi2212
!------------------------------------------------------------------
subroutine jcrit_nbti(thelium,bmax,c0,bc20max,tc0max,jcrit,tcrit)

    !+ad_name  jcrit_nbti
    !+ad_summ  Critical current density in a NbTi superconductor strand
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  thelium : input real : Coolant/SC temperature (K)
    !+ad_args  bmax : input real : Magnetic field at conductor (T)
    !+ad_args  c0   : input real : Scaling constant (A/m2)
    !+ad_args  bc20max : input real : Upper critical field (T) for superconductor
    !+ad_argc                      at zero temperature and strain
    !+ad_args  tc0max : input real : Critical temperature (K) at zero field and strain
    !+ad_args  jcrit : output real : Critical current density in superconductor (A/m2)
    !+ad_args  tcrit : output real : Critical temperature (K)
    !+ad_desc  This routine calculates the critical current density and
    !+ad_desc  temperature in superconducting TF coils using NbTi
    !+ad_desc  as the superconductor.
    !+ad_prob  Results will be misleading unless bmax < bc20max, and
    !+ad_prob  thelium is sufficiently low.
    !+ad_call  None
    !+ad_hist  09/10/14 PJK Initial version, taken from inline code in supercon
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    real(kind(1.0D0)), intent(in) :: thelium, bmax, c0, bc20max, tc0max
    real(kind(1.0D0)), intent(out) :: jcrit, tcrit

    !  Local variables
    real(kind(1.0D0)) :: bratio, tbar
    ! ------------------

    bratio = min(bmax/bc20max, 0.9999D0)  !  avoids NaNs below

    !  Critical temperature (K)

    tcrit = tc0max * (1.0D0 - bratio)**0.59D0

    tbar = max((1.0D0 - thelium/tcrit), 0.001D0)

    !  Critical current density (A/m2)

    jcrit = c0 * (1.0D0 - bratio) * tbar

end subroutine jcrit_nbti
!--------------------------------------------------------------------

subroutine wstsc(thelium,bmax,strain,bc20max,tc0max,jcrit,bcrit,tcrit)

    !+ad_name  wstsc
    !+ad_summ  Implementation of WST Nb3Sn critical surface implementation
    !+ad_type  Subroutine
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  thelium : input real : Coolant/SC temperature (K)
    !+ad_args  bmax : input real : Magnetic field at conductor (T)
    !+ad_args  strain : input real : Strain in superconductor
    !+ad_args  bc20max : input real : Upper critical field (T) for superconductor
    !+ad_argc                      at zero temperature and strain
    !+ad_args  tc0max : input real : Critical temperature (K) at zero field and strain
    !+ad_args  jcrit : output real : Critical current density in superconductor (A/m2)
    !+ad_args  bcrit : output real : Critical field (T)
    !+ad_args  tcrit : output real : Critical temperature (K)
    !+ad_desc  This routine calculates the critical current density and
    !+ad_desc  temperature in the superconducting TF coils using the
    !+ad_desc  WST Nb3Sn critical surface model.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  22/02/17 JM  Initial version
    !+ad_stat  Okay
    !+ad_docs  https://idm.euro-fusion.org/?uid=2MMDTG
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    real(kind(1.0D0)), intent(in) :: thelium, bmax, strain, bc20max, tc0max
    real(kind(1.0D0)), intent(out) :: jcrit, bcrit, tcrit

    ! Local variables

    ! Scaling constant C [AT/mm2]
    real(kind(1.0D0)), parameter :: csc = 83075.0D0

    ! Low field exponent p
    real(kind(1.0D0)), parameter :: p = 0.593D0

    ! High field exponent q
    real(kind(1.0D0)), parameter :: q = 2.156D0

    ! Strain fitting constant C_{a1}
    real(kind(1.0D0)), parameter :: ca1 = 50.06D0

    ! Strain fitting constant C_{a2}
    real(kind(1.0D0)), parameter :: ca2 = 0.0D0

    ! epsilon_{0,a}
    real(kind(1.0D0)), parameter :: eps0a = 0.00312D0

    !real(kind(1.0D0)), parameter :: epsmax = -1.1D-3  !  epsilon_{max} (not used)

    real(kind(1.0D0)) :: bred, epssh, t, bc20eps, &
    tc0eps, bzero, strfun, jc1, jc2, jc3, scalefac

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  $\epsilon_{sh}$
    epssh = (ca2*eps0a)/(sqrt(ca1**2 - ca2**2))

    !  Strain function $s(\epsilon)$
    !  0.83 < s < 1.0, for -0.005 < strain < 0.005
    strfun = sqrt(epssh**2 + eps0a**2) - sqrt((strain-epssh)**2 + eps0a**2)
    strfun = strfun*ca1 - ca2*strain
    strfun = 1.0D0 + (1.0D0/(1.0D0 - ca1*eps0a))*strfun

    !  $B^*_{C2} (0,\epsilon)$
    bc20eps = bc20max*strfun

    !  $T^*_C (0,\epsilon)$
    tc0eps = tc0max * strfun**(1.0D0/3.0D0)

    !  Reduced temperature, restricted to be < 1
    !  Should remain < 1 for thelium < 0.94*tc0max (i.e. 15 kelvin for isumattf=1)

    if (thelium/tc0eps >= 1.0D0) then
        fdiags(1) = thelium ; fdiags(2) = tc0eps
        call report_error(159)
    end if
    t = min(thelium/tc0eps, 0.9999D0)

    !  Reduced magnetic field at zero temperature
    !  Should remain < 1 for bmax < 0.83*bc20max (i.e. 27 tesla for isumattf=1)

    if (bmax/bc20eps >= 1.0D0) then
        fdiags(1) = bmax ; fdiags(2) = bc20eps
        call report_error(160)
    end if
    bzero = min(bmax/bc20eps, 0.9999D0)

    !  Critical temperature (K)
    tcrit = tc0eps * (1.0D0 - bzero)**(1.0D0/1.52D0)  !  bzero must be < 1 to avoid NaNs

    !  Critical field (T)
    bcrit = bc20eps * (1.0D0 - t**1.52D0)

    !  Reduced magnetic field, restricted to be < 1
    if (bmax/bcrit >= 1.0D0) then
        fdiags(1) = bmax ; fdiags(2) = bcrit
        call report_error(161)
    end if
    bred = min(bmax/bcrit, 0.9999D0)

    !  Critical current density in superconductor (A/m2)
    jc1 = (csc/bmax)*strfun
    jc2 = (1.0D0-t**1.52D0) * (1.0D0-t**2)  !  t must be < 1 to avoid NaNs
    jc3 = bred**p * (1.0D0-bred)**q  !  bred must be < 1 to avoid NaNs

    ! scale from mm2 to m2
    scalefac = 1.0D6

    jcrit = jc1 * jc2 * jc3*scalefac

end subroutine wstsc
!--------------------------------------------------------------------------

subroutine croco(jcritsc,croco_strand)

    !+ad_name  croco
    !+ad_summ  "CroCo" (cross-conductor) strand and cable design for
    !+ad_summ  "REBCO" 2nd generation HTS superconductor
    !+ad_type  Subroutine
    implicit none
    real(kind(1.0D0)), intent(in) ::jcritsc
    type(volume_fractions)::croco_strand

    tape_thickness = rebco_thickness + copper_thickness + hastelloy_thickness
    stack_thickness = sqrt(croco_id**2 - (tape_width/2.0d0)**2)
    tapes = stack_thickness / tape_thickness
    rebco_area = rebco_thickness * tape_width * tapes
    copper_area = pi / 4.0d0 * (croco_od**2 - croco_id**2) + copper_thickness*tape_width*tapes
    hastelloy_area = hastelloy_thickness * tape_width * tapes
    solder_area = pi / 4.0d0 * croco_id**2 - stack_thickness * tape_width

    rebco_area = rebco_thickness * tape_width * tapes
    croco_area =  pi * croco_od**2

    croco_strand%copper_fraction = copper_area / croco_area
    croco_strand%hastelloy_fraction = hastelloy_area / croco_area
    croco_strand%helium_fraction = 0.0d0
    croco_strand%solder_fraction = solder_area / croco_area

    croco_strand%critical_current = jcritsc * rebco_area

    cable_crit_current = croco_strand%critical_current * number_croco
    cable_helium_area =  0.25d0 * croco_od**2 * number_croco

end subroutine croco
!--------------------------------------------------------------------------
subroutine copper_properties(T,copper)
    ! RRR=100, B=12 T
    ! Cryodata Software Package, CRYOCOMP, v 3.0, Florence, SC, 1997
    ! Data are available up to 1000 K.

    type(resistive_material)::copper
    real(kind(1.0D0)), intent(in) :: T   ! temperature

    copper%label = 'Copper with RRR=100 at B=12 T'

    if(T<40.0d0)then
        copper%cp = -1.6113+0.60915*T-0.07152*T**2+0.00398*T**3-4.07673E-5*T**4
    else if((40.0d0<T).and.(T<300.0d0))then
        copper%cp = -196.51325+7.84244*T-0.04493*T**2+1.24263E-4*T**3-1.33845E-7*T**4
    else if((300.0d0<T).and.(T<1000.0d0))then
        copper%cp = 274.99376+0.70709*T-0.0015*T**2+1.35186E-6*T**3-4.34609E-10*T**4
    endif

    if(T<70.0d0)then
        copper%resistivity = 6.19659E-10+7.13979E-12*T-5.36171E-13*T**2+1.47182E-14*T**3-6.68583E-17*T**4
    else if((70.0d0<T).and.(T<1000.0d0))then
        copper%resistivity = -3.3449E-9 +7.6628E-11*T-3.94931E-14*T**2+6.36545E-17*T**3-2.87882E-20*T**4
    endif

end subroutine copper_properties
!--------------------------------------------------------------------------
! subroutine copper_properties_nist(T,B,copper)
!     ! Properties of copper and copper alloys,
!     ! N. J. Simon, E. S. Drexler, and R. P. Reed
!     ! NIST Monograph 177 (1992)
!     ! Note that some papers that copy this formula have confused the units.
!     ! They also seem to have misread the sign in the exponent term "P2+P4".
!     ! Unfortunately the NIST website doesn't seem to work, so it is hard to verify.
!
!     type(resistive_material)::copper
!     real(kind(1.0D0)), intent(in) :: T, B   ! temperature, field
!     ! Fitting constants
!     real(kind(1.0D0)), parameter::P1 = 1.171d-17
!     real(kind(1.0D0)), parameter::P2 = 4.49
!     real(kind(1.0D0)), parameter::P3 = 3.841d10
!     real(kind(1.0D0)), parameter::P4 = 1.14
!     real(kind(1.0D0)), parameter::P5 = 50
!     real(kind(1.0D0)), parameter::P6 = 6.428
!     real(kind(1.0D0)), parameter::P7 = 0.4531
!
!     real(kind(1.0D0)), parameter::a0 = -0.2662d0
!     real(kind(1.0D0)), parameter::a1 = 0.3168d0
!     real(kind(1.0D0)), parameter::a2 = 0.6229d0
!     real(kind(1.0D0)), parameter::a3 = -0.1839d0
!     real(kind(1.0D0)), parameter::a4 = 0.01827
!
!     real(kind(1.0D0))::rho0, rhoi, rhoi0    ! Resistivity terms (nano-ohm.m)
!     real(kind(1.0D0))::denominator, x
!
!     copper%label = 'Copper with specified RRR and field'
!
!         rho0 = 15.53 / copper%rrr
!     denominator = 1 + P1*P3*T**(P2+P4)*exp(-(P5/T)**P6)
!     rhoi = P1*T**P2 / denominator
!     rhoi0 = P7*rhoi*rho0 / (rhoi+rho0)
!     copper%resistivity = 1.0d-9*rhoi0
!
!     ! Effect of magnetic field
!     x = 15.53 / copper%rrr
! end subroutine copper_properties_nist
! -------------------------------------------------------------------------

subroutine copper_properties2(T,B, copper)
    ! Review of ROXIE's Material Properties Database for Quench Simulation,
    ! Author: Giulio Manfreda, December 2011
    ! https://espace.cern.ch/roxie/Documentation/Materials.pdf
    ! Different models use different definitions for residual resisitivity ratio RRR.
    ! CUDI: resistivity at 290 K / 4 K.
    ! The range of validity of this t is between 4 K and 300 K.
    implicit none

    type(resistive_material)::copper
    real(kind(1.0D0)), intent(in) :: T, B   ! temperature, field
    real(kind(1.0D0)):: bracket, logt, sum
    ! Fitting constants: resistivity
    real(kind(1.0D0)), parameter:: t5 = 2.32547d9
    real(kind(1.0D0)), parameter:: t3 = 9.57137d5
    real(kind(1.0D0)), parameter:: t1 = 1.62735d2
    real(kind(1.0D0)), parameter:: mr = 5.0d-11     ! ohm.m/T
    real(kind(1.0D0)), parameter:: a = 1.7          ! ohm.m
    ! Fitting constants: specific heat (p.13)
    ! Checked against
    ! http://cryogenics.nist.gov/MPropsMAY/OFHC%20Copper/OFHC_Copper_rev1.htm
    real(kind(1.0D0)), parameter::a0 = -1.91844d0
    real(kind(1.0D0)), parameter::a1 = -0.15973d0
    real(kind(1.0D0)), parameter::a2 =  8.61013d0
    real(kind(1.0D0)), parameter::a3 = -18.996d0
    real(kind(1.0D0)), parameter::a4 =  21.9661d0
    real(kind(1.0D0)), parameter::a5 = -12.7328d0
    real(kind(1.0D0)), parameter::a6 =  3.54322d0
    real(kind(1.0D0)), parameter::a7 = -0.3797d0

    copper%label = 'Copper with specified RRR, field'

    ! page 5: Copper resistivity is computed in CUDI with the t function similar
    ! to the one of McAshan [McA88]
    ! Note this formula is much quicker to evaluate than the NIST formula.
    bracket = 1 / (t5/T**5 + t3/T**3 + t1/T)
    copper%resistivity = 1.d-8 * (a/copper%rrr + bracket) + mr*B

    ! NIST typical polynomial interpolation, equation 4 page 3
    logt = log10(T)
    sum = a0 + a1*logt + a2*logt**2 + a3*logt**3 + a4*logt**4 + a5*logt**5 + a6*logt**6 +a7*logt**7
    copper%cp = 10**sum

end subroutine copper_properties2
! -------------------------------------------------------------------------
subroutine hastelloy_properties(temperature,hastelloy)
    implicit none

    type(resistive_material)::hastelloy
    real(kind(1.0D0)), intent(in) :: temperature   ! temperature
    real(kind(1.0D0)) :: T

    T = temperature
    if(temperature>300d0) T=300d0

    ! Reinhard Heller: obtained by fitting data published in :
    ! J. Lu, E. S. Choi, and H. D. Zhou,
    ! "Physical properties of Hastelloy C-276 at cryogenic temperatures,"
    ! J. Appl. Phys. 103(6) 2008 064908

    hastelloy%label = 'Hastelloy with specified T'
    hastelloy%density = 8890.0d0    ! kg/m3

    if(T<42.2571d0)then
        hastelloy%cp = 0.60796d0 + 0.15309d0*T - 0.00237d0*T**2 + 6.76732d-4*T**3
    else if(T.ge.42.2571d0)then
        hastelloy%cp = -147.06251d0 + 5.43432d0*T - 0.01937d0*T**2 + 2.71669d-5*T**3   &
                       -8.12438d-9*T**4
    endif

    ! if(T<48.7135d0)then
    !     hastelloy%resistivity = 1.23386d-6 - 1.40462d-9*T + 1.09943d-10*T**2 -  &
    !                             3.81875d-12*T**3 + 6.3866d-14*T**4 - 4.10322d-16*T**5
    ! else if(T.ge.48.7135d0)then
    !     hastelloy%resistivity = 1.22641d-6 + 1.19188d-10*T
    ! endif
end subroutine hastelloy_properties
! --------------------------------------------------------------------------
subroutine solder_properties(T,solder)
    implicit none

    type(resistive_material)::solder
    real(kind(1.0D0)), intent(in) :: T   ! temperature

    ! Reinhard Heller: obtained by fitting data
    ! Material Database from Cryodata Software Package, CRYOCOMP, version 3.0, Florence, SC

    solder%label = 'Solder with specified T'
    ! solder%density = TODO    ! kg/m3

    if(T<20.0d0)then
        solder%cp = 4.88028d0 - 2.92865d0*T + 0.52736d0*T**2 - 0.01861d0*T**3 + 2.36019d-4*T**4
    else if((T.ge.20.0d0).and.(T<300.0d0))then
        solder%cp = 4.88028d0 - 2.92865d0*T + 0.52736d0*T**2 - 0.01861d0*T**3 + 2.36019d-4*T**4
    else
        solder%cp = 181.29d0
    endif

    ! if(T<31.0d0)then
    !     solder%resistivity = 3.40409d-9 + 1.98993d-12*T + 6.56824d-12*T**2 - 1.32602d-13*T**3  &
    !                         +1.33974d-15*T**4
    ! else if((T.ge.31.0d0).and.(T<300.0d0))then
    !     solder%resistivity = -9.01684d-9 + 5.11669d-10*T + 1.40751d-13*T**2 - 5.6729d-16*T**3 +  &
    !                           7.41049d-19*T**4
    ! else
    !     solder%resistivity = 1.4782d-7
    ! endif

end subroutine solder_properties
! -------------------------------------------------------------------------
subroutine jacket_properties(T, jacket)

    implicit none

    type(resistive_material)::jacket
    real(kind(1.0D0)), intent(in) :: T   ! temperature
    real(kind(1.0D0)):: logt, sum
    ! Fitting constants: specific heat (p.13)
    ! http://cryogenics.nist.gov/MPropsMAY/304Stainless/304Stainless_rev.htm
    real(kind(1.0D0)), parameter::a0 = 22.0061
    real(kind(1.0D0)), parameter::a1 = -127.5528
    real(kind(1.0D0)), parameter::a2 = 303.647
    real(kind(1.0D0)), parameter::a3 = -381.0098
    real(kind(1.0D0)), parameter::a4 = 274.0328
    real(kind(1.0D0)), parameter::a5 = -112.9212
    real(kind(1.0D0)), parameter::a6 = 24.7593
    real(kind(1.0D0)), parameter::a7 = -2.239153

    jacket%label = 'Jacket (stainless steel 316)'

    logt = log10(T)
    if(T>300) logt = log10(300d0)
    sum = a0 + a1*logt + a2*logt**2 + a3*logt**3 + a4*logt**4 + a5*logt**5 + &
                         a6*logt**6 + a7*logt**7
    jacket%cp = 10**sum

end subroutine jacket_properties


end module superconductors
