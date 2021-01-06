!     **********************************************************************
subroutine currn(om,zacosarg,xk0,xnpa,enpep,angnb,op,oc,ocmax,ocmin, &
    te,zeff,cdeff,lh,cefldx,cefldy,cefldz,cdharm)
    !     **********************************************************************
    !
    !     zelchg is the electron charge in statcoulombs.
    !     zconst is the ln of Boltzmann's constant (1.3807e-16) times the
    !          factor to convert temperature from eV to degrees Kelvin
    !          (1.1605e4) divided by the product of c_light times Planck's
    !          constant (1.0546e-27).
    !     coverw is c_light divided by omega (2*Pi*f).
    !     zconvr is the constant that converts from (statamps/cm**2) divided
    !          by (ergs/sec) to (amps/m**2) divided by watts.
    !     zconv1 is the constant that converts from (statamps*cm) divided
    !          by (ergs/sec) to (amps*cm) divided by watts, i.e., zconvr/e4

    use abs_cd, only: cdroutine
    use param, only: pi
    use bsquar, only: conf, conf2
    use mode, only: imod
    use mod_f90_kind, only: rkind
    implicit none

    ! Parameters
    ! It is unknown what the intents of these parameters are; guess inout
    real(kind(1.0D0)), intent(inout) :: om
    real(kind(1.0D0)), intent(inout) :: zacosarg
    real(kind(1.0D0)), intent(inout) :: xk0
    real(kind(1.0D0)), intent(inout) :: xnpa
    real(kind(1.0D0)), intent(inout) :: enpep
    real(kind(1.0D0)), intent(inout) :: angnb
    real(kind(1.0D0)), intent(inout) :: op
    real(kind(1.0D0)), intent(inout) :: oc
    real(kind(1.0D0)), intent(inout) :: ocmax
    real(kind(1.0D0)), intent(inout) :: ocmin
    real(kind(1.0D0)), intent(inout) :: te
    real(kind(1.0D0)), intent(inout) :: zeff
    real(kind(1.0D0)), intent(inout) :: cdeff
    integer, intent(inout) :: lh
    complex, intent(inout) :: cefldx
    complex, intent(inout) :: cefldy
    complex, intent(inout) :: cefldz
    real(kind(1.0D0)), intent(inout) :: cdharm

    ! Local variables
    real(kind(1.0D0)) :: aspct
    real(kind(1.0D0)) :: coverw
    real(kind(1.0D0)) :: denom
    real(kind(1.0D0)) :: elomom
    real(kind(1.0D0)) :: eltmp
    real(kind(1.0D0)) :: enpa
    real(kind(1.0D0)) :: ezeff
    integer :: ig
    integer :: modelc
    integer :: ngauss
    real(kind(1.0D0)) :: ratjpd
    real(kind(1.0D0)) :: rbeta
    real(kind(1.0D0)) :: rjpd
    real(kind(1.0D0)) :: rjpd0
    real(kind(1.0D0)) :: rjpd0h
    real(kind(1.0D0)) :: rjpdh
    real(kind(1.0D0)) :: theta
    real(kind(1.0D0)) :: thprnt
    real(kind(1.0D0)) :: thtc
    real(kind(1.0D0)) :: tolcur
    real(kind(1.0D0)) :: xnperp
    real(kind(1.0D0)) :: zb
    real(kind(1.0D0)) :: zbmax
    real(kind(1.0D0)) :: zbmin
    real(kind(1.0D0)) :: zconst
    real(kind(1.0D0)) :: zconv1
    real(kind(1.0D0)) :: zconvr
    real(kind(1.0D0)) :: zelchg
    real(kind(1.0D0)) :: zfac
    real(kind(1.0D0)) :: zfac1
    real(kind(1.0D0)) :: zfac2
    real(kind(1.0D0)) :: zfac3
    real(kind(1.0D0)) :: zralfa

    DATA &
    zelchg /4.8032d-10/, &
    zconst /10.83259d0/, &
    zconvr /33.33333d0/, &
    zconv1 /33.33333d-4/

    conf = 2.e9_rkind*pi*27.9924e0_rkind/om
    conf2 = conf*conf
    ! #ifdef PERF
    !   call profstart('currn')  ! time profiling
    ! #endif
    IF (te.le.1.d-10) RETURN
    !
    enpa = sqrt(xnpa)*cos(angnb)/abs(cos(angnb))
    !      enpa = sqrt(xnpa)
    !
    zbmax = sqrt(ocmax/conf2)
    zbmin = sqrt(ocmin/conf2)
    aspct = (zbmax-zbmin)/(zbmax+zbmin)
    !     print*,'bmax,bmin,aspct',zbmax,zbmin,aspct
    !
    zb = sqrt(oc/conf2)

    ! ...  Prevent crash due to interpolation errors in bmaxo0 or bmino0.
    IF (ABS(zacosarg) .gt. 1.) zacosarg = SIGN(1.d0, zacosarg)
    theta = ACOS(zacosarg)
    thprnt = theta*1.8d2/pi
    !      print*,'b,theta',zb,thprnt
    !      T_e in eV
    eltmp = te*1.d3
    !      omega_p/omega
    zralfa = SQRT(op)
    !      omega_c/omega
    rbeta = SQRT(oc)
    !
    coverw = 1.d0/xk0
    zfac1 = zconst+log(coverw)
    zfac2 = (coverw**2/(zelchg*op))*2.d0*eltmp/511.d3
    zfac3 = zfac1+LOG(eltmp/zralfa)
    !      print*,'zfac1,2,3',zfac1,zfac2,zfac3
    zfac = zconv1*zfac2/zfac3
    !
    ! ... Compute current-drive efficiency for each harmonic,
    !     then weight the efficiencies according to the fraction
    !     of the power deposited at each harmonic.
    !
    rjpd = 0.d0
    rjpd0 = 0.d0
    !
    !      The harmonic is computed as the lowest possible
    lh = int(sqrt(1.d0-xnpa)/rbeta)+1
    !      print*,a,te,sqrt(xnpa),lh
    elomom = lh*rbeta
    !      Other arguments for TorGA_curgap...
    thtc = 1.d0
    ezeff = zeff
    tolcur = 1.d-5

    cdroutine = 2

    modelc = +5
    xnperp = enpep

    lh = abs(lh)

    ngauss = 64

    CALL TorGA_curgap(rjpdh,rjpd0h,ratjpd,denom,aspct,enpa,xnperp, &
    imod,cefldx,cefldy,cefldz,te,thtc,theta,elomom,lh,ezeff,modelc, &
    tolcur,ngauss,ig,cdharm)
    
    rjpd = rjpd + rjpdh
    rjpd0 = rjpd0 + rjpd0h

    rjpd = rjpd*zfac
    cdeff = rjpd

    rjpd0 = rjpd0 * zfac


    ! #ifdef PERF
    !   call profend('currn')  ! time profiling
    ! #endif
    RETURN
END
