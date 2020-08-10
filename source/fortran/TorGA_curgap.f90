module torga_curgap_module
  implicit none

  ! First run flag for subroutine green_func_emp, which requires re-initialising
  ! on each new run
  logical :: green_func_emp_first

  contains

  subroutine init_torga_curgap_module
    !! Initialise module variables
    implicit none

    green_func_emp_first = .true.
  end subroutine init_torga_curgap_module
end module torga_curgap_module

!-----------------------------------------------------------------------&
      subroutine TorGA_curgap(rjpd,rjpd0,ratjpd,denom,eps,npara,nperp   &
     &,omode,cefldx,cefldy,cefldz,tebulk,thtc,thetap,yy,lh,zeffin,model &
     &,tol,ngauss,ig,cdharm)
      use precision_mod, only: p_
      implicit none

      REAL(p_) rjpd,rjpd0,ratjpd,denom
      REAL(p_) eps,npara,nperp,omode,tebulk,thtc,thetap,yy,tol,zeffin
      COMPLEX(p_) cefldx,cefldy,cefldz
      INTEGER lh,model,ngauss,ig
      INCLUDE 'globcd.h'
      ! 'globcd1.h'
      REAL(p_) enperp, omodev
      COMPLEX(p_) cez, ceplus, ceminus
      COMMON /wavccd1/cez,ceplus,ceminus,omodev, enperp
      ! 'globcd2.h'
      REAL(p_) zrtmp, fctmp
      common /dumm/zrtmp, fctmp

      REAL(p_) TorGa_zgauleg
      EXTERNAL TorGa_zgauleg
      EXTERNAL TorGa_funxjs,TorGa_funxjz,TorGa_alpha
!------------------------------------------------------------------------
!     CURGAP(version 1.0) Y.R. Lin-Liu, 08/16/04
!     This routine is an improved version and the replacement of CURGAC.
!     The code calculates a normalized ECCD efficiency using the Green's
!     function formulation[1,2]. The improvement is the use of the exact
!     polarization-dependent rf quasi-linear diffusion operator[3]in
!     evaluating the current drive efficiency.
!
!     References:
!     1. Y.R. Lin-Liu, V.S. Chan, and R. Prater,"Electron cyclotron
!        current drive efficiency in general tokamak gemetry,"
!        Phys. Plasmas 10, 4064 (2003).
!     2. R.H. Cohen, "Effect of trapped electrons on current drive,"
!        Phys. Fluids 30, 2442 (1987); 31, 421 (1988).
!     3. R.W. Harvey and M.G. McCoy, "The CQL3D Fokker-Plank code,"
!        GA report GA-A20978 (1992).
!
!     OUTPUTS:
!     rjpd = Cohen's normalized current drive efficiency (j/Pd),
!            where j is the flux-surface-average of the parallel driven
!            current density (in units of e(*n_e)*Vt) and Pd is the
!            absorbed power density ( in units of nu*n_e*m*Vt^2).
!            Here n_e = electron density
!                 Vt  = SQRT(2*T/m)
!                 nu  = e^4*n_e*Log(Lambda)/(4*pi*eps0^2*m^2*Vt^3)
!     rjpd0 = the equivalent rjpd in the absence of trapped electrons
!     ratjpd = rjpd/rjpd0
!     denom  = the denomintor in evaluation of rjpd
!
!     INPUTS:
!     eps = inverse aspect ratio of the flux surface of interest
!     npara = parallel index of refraction
!     nperp = Real part of perpendicular index of refraction
!     omode = +1.0 for O-mode and -1.0 for x-mode
!     cefldx = the x-component of the wave electric field (COMPLEX)
!     cefldy = the y-component                            (COMPLEX)
!     cefldy = the z-component                            (COMPLEX)
!     tebulk = Electron temperature in keV's
!     thtc   = an obsolete variable
!     thetap = poloidal angle at which power is absorbed in radians
!              0: outborad; pi: inboard
!     lh = the cyclotron harmonic number
!     yy = lh*omega_c/omega (y in Refs [1] and [2])
!     zeffin = effective ion charge
!     model < 5  gives rjpd in CURGAC
!           = 5  gives rjpd using the exact polarization-dependent
!                rf diffusion operator
!           > 5  gives rjpd using the polarization-dependent rf diffusion
!                operator with but small gyro-radius expansion
!     tol = The relative error tolerence in numerical integration is set
!           to be MAX (tol, 1.0E-6).
!     ngauss = number of points in the Gaussian quadrature (ngauss = 64
!              is recommended.)
!     ig     = 1 : the relativistic Green's function
!            = 0 : the non-relativistic approximation
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!     Comments from older versions of CURGAC
!     revisions (02/13/99):
!     improve numerical integeration of ft and hcap
!     introduce ismall for flaging small eps
!     introduce new routine "getftrap"
!     combine interation and integrant routines
!     revisions (02/10/99):
!     introduce nghalf and use TorGa_zgauleg
!     remove the bug in cxi2
!
!     revisions (02/01/99):
!     set ng=min(ngauss,64)
!     remove ptfctr option
!     if model .eq. 4 will calculate the collsionality correction of CD
!     add new variables: rjpdc, nustar, ccf
!     add a new global variable: eta
!
!------------------------------------------------------------------------
      REAL(p_) rteps,ss
      REAL(p_) etmax,epst1,epst2
      REAL(p_) expt,expb,exp1,exp2,rint,rint0
      INTEGER ng,nghalf,ifail
      INTEGER jromb
      INTEGER ismall
!emp
      REAL(p_) rintsum,rint0sum,denomsum
      INTEGER iharm,ihigher,cdharm
!
      pi=acos(-1._p_)
!
      if (abs(npara) .lt. small)then   ! don't bother
         rjpd=0._p_
         rjpd0=0._p_
         ratjpd=1._p_
         denom=1._p_
         return
      endif
!
!     set up the global variables for the current-drive package
!     numerical control parameters:
      tolval=max(tol,1.e-6_p_)
      nghalf=MIN(ngauss,64)/2
      ng=nghalf*2
      modelv=model
      igv=ig
!
!     variables of wave characteristics:
!emp (yval and nharm moved to loop over harmonics)
!      yval=yy
!      nharm=lh
      omodev=omode
      enz=npara
      enzsq=enz*enz
      sgnnz=sign(1.0_p_,npara)
      enperp=nperp
      cez=cefldz
      ceplus=cefldx+(0._p_, 1._p_)*cefldy
      ceminus=cefldx-(0._p_, 1._p_)*cefldy
!
!     geometric variables:
      call TorGa_ceqmdl(eps,thetap)
      rteps=sqrt(abs(1._p_-hav))
      if (rteps .lt. small)then
         ismall=1
         cxi2=-0.125_p_
         cxi4=pi**2/8._p_-1._p_
         ft=1.46_p_*rteps
         fc=1._p_-ft
      else
         ismall=0
         cxi2=-0.25_p_*(hsqav-hav**2)/(1._p_-hav)**2
         cxi4=chrtav**2/(1._p_-hav)-1._p_
         call TorGa_getftrap
      endif
!
!     kinetic variables:
      tau=tebulk/tmass     ! tmass given in globcd.h
      zeff=zeffin
      zrat=(zeff+1._p_)/fc
      etcutoff=20._p_    ! upper bound for (gamma-1)/tau integration
!emp
!initialize
      rintsum=0._p_; rint0sum=0._p_; denomsum=0._p_; ihigher=0
!start loop over neighbouring harmonics
   if (cdharm == 1) ihigher=1
   do iharm=lh,lh+ihigher
!
      nharm=iharm
      yval=real(nharm)*yy/real(lh)
!     setup integration limits
      call TorGa_getlims(etmax,epst1,epst2)
      if (ismall .eq. 1)then          ! deal with small eps case
         if (etmax .gt. 0._p_)then
            ifail=0
            expb=EXP(-etmax)
            expb=1._p_
            denom=tau*TorGa_zgauleg(TorGa_alpha,expb,expt,nghalf,2,ifail)
            rint0=TorGa_zgauleg(TorGa_funxjz,expb,expt,nghalf,2,ifail)
!emp
            rint=rint0
!            rjpd0=-0.5_p_*hav*rint0/denom
!            rjpd=rjpd0
!            ratjpd=1._p_
         else
            denom=0._p_
            rjpd0=0._p_
            rjpd=0._p_
            ratjpd=1._p_
         endif
!         return
!emp
         go to 10
      endif
!
      if (etmax .gt. 0._p_)then
!        evaluate the normalized absorbed power
!        call pdnorm(denom,tau,yval,enzsq,nharm,gammin,gammax)
         expt=1._p_
         expb=exp(-etmax)
         denom=tau*TorGa_zgauleg(TorGa_alpha,expb,expt,nghalf,2,ifail)
         rint0=TorGa_zgauleg(TorGa_funxjz,expb,expt,nghalf,2,ifail)
         if (epst1 .gt. 0._p_ .and. epst2 .lt. etmax)then
            expb=exp(-etmax)
            exp1=exp(-epst1)
            exp2=exp(-epst2)
            ifail=0
            rint=TorGa_zgauleg(TorGa_funxjs,expb,exp2,nghalf,1,ifail)
            rint=rint+TorGa_zgauleg(TorGa_funxjs,exp1,expt,nghalf,1,ifail)
         else
            ifail=0
            if(epst2.le.0._p_)expb=exp(-etmax)
            if(epst2.gt.0._p_.and.epst1.le.0._p_)expb=exp(-MIN(epst2,etmax))
            if(epst2.gt.0._p_.and.epst1.gt.0._p_)expb=exp(-MIN(epst1,etmax))
            rint=TorGa_zgauleg(TorGa_funxjs,expb,expt,nghalf,2,ifail)
!%PR         write (6,*)rint0,rint
         endif
!emp (first build the sum over harmonics for numerator and denominator separately,
!    then divide)
!         rjpd=-0.5_p_*hav*rint/denom
!         rjpd0=-0.5_p_*hav*rint0/denom
!         ratjpd=rint/rint0
      else
         denom=0._p_
         rjpd=0._p_
         rjpd0=0._p_
         ratjpd=1._p_
      endif
!emp !!! in the integrals, gamma=-log(x)*tau+gammin implying that the result
!    !!! is multiplied by exp((gammin-1)/tau) -> divide by this factor
10    continue
      rintsum =rintsum +rint *exp((1._p_-gammin)/tau)
      rint0sum=rint0sum+rint0*exp((1._p_-gammin)/tau)
      denomsum=denomsum+denom*exp((1._p_-gammin)/tau)
   end do
   if (denomsum > 0._p_) then
      rjpd  =-0.5_p_*hav*rintsum/denomsum
      rjpd0 =-0.5_p_*hav*rint0sum/denomsum
      ratjpd=rintsum/rint0sum
      denom =denomsum
   end if
!
      return
!
      end
      FUNCTION TorGa_alpha(x)
!
      USE precision_mod, ONLY: p_
      IMPLICIT NONE
      REAL(p_) TorGa_alpha, x
      REAL(p_) TorGa_bessj
      INCLUDE 'globcd.h'
      ! 'globcd1.h'
      REAL(p_) enperp, omodev
      COMPLEX(p_) cez, ceplus, ceminus
      COMMON /wavccd1/cez,ceplus,ceminus,omodev, enperp
!-------------------------------------------------------------------------
      REAL(p_) gamma,usq,u,uz,uxsq,uperp
      REAL(p_) aperp,xjz,xjp,xjm
      COMPLEX(p_) cuel
      INTEGER np,nm
!
      gamma=-log(x)*tau+gammin
      usq=ABS(gamma**2-1._p_)
      u=SQRT(usq)
      uz=(gamma-yval)/enz
      uxsq=usq-uz**2
      uperp=SQRT(ABS(uxsq))
!
      aperp=nharm*enperp*uperp/yval
!
      IF (modelv == 5) THEN
         xjz=TorGa_bessj(nharm,aperp)
         np=nharm+1
         xjp=TorGa_bessj(np,aperp)
         nm=nharm-1
         xjm=TorGa_bessj(nm,aperp)
         cuel=cez*uz*xjz+0.5_p_*uperp*(ceminus*xjm+ceplus*xjp)
         TorGa_alpha=ABS(cuel)**2
      ELSE IF (modelv < 5)THEN
         TorGa_alpha=uxsq**nharm
      ELSE
         xjm=1._p_-0.25_p_*aperp**2/nharm
         xjz=0.5_p_*aperp/nharm
         xjp=0.25_p_*aperp**2/(nharm*(nharm+1))
         cuel=cez*uz*xjz+0.5_p_*uperp*(ceminus*xjm+ceplus*xjp)
         TorGa_alpha=uxsq**(nharm-1)*ABS(cuel)**2
      ENDIF
!
      RETURN
      END FUNCTION TorGa_alpha

      function TorGa_funxjz(x)

      use precision_mod, only: p_
      use abs_cd, only: cdroutine !EXTERNAL
      implicit none
      REAL(p_) TorGa_funxjz, x
      REAL(p_) TorGa_alpha
      include 'globcd.h'
      ! 'globcd2.h'
      REAL(p_) zrtmp, fctmp
      common /dumm/zrtmp, fctmp

!-------------------------------------------------------------------------
      REAL(p_) alpha
      REAL(p_) gamma,usq,u,uz,uxsq
      REAL(p_) fcap,fprime
      REAL(p_) hksi,hcap
!emp
      REAL(p_) tebulk
!
      alpha=TorGa_alpha(x)
!
      gamma=-log(x)*tau+gammin
      usq=abs(gamma**2-1._p_)
      u=sqrt(usq)
      uz=(gamma-yval)/enz
      uxsq=usq-uz**2
!
      hksi=hloc/hsqav
      hcap=uz/u*hksi
!
      zrtmp=zeff+1._p_
      fctmp=1._p_
!emp
      select case (cdroutine)
      case(1,11)
         call TorGa_getfcap(u,fcap,fprime)
      case(2,12)
         tebulk = tau*tmass
         call green_func_emp(igv, tebulk, zeff, fc, u, gamma, fcap, fprime)
      case default
         print*,'WARNING! No CD model chosen'
         TorGa_funxjz = 0._p_
         return
      end select
!
      TorGa_funxjz=alpha*(fprime*hcap+(enz-gamma*uz/usq)*fcap*hksi/u)
!
      return
      end

      function TorGa_funxjs(x)

      use precision_mod, only: p_
      use abs_cd, only: cdroutine  !EXTERNAL
      implicit none

      REAL(p_) TorGa_funxjs,x
      REAL(p_) TorGa_alpha
      include 'globcd.h'
!-------------------------------------------------------------------------
      REAL(p_) alpha
      REAL(p_) gamma,usq,u,uz,uxsq,lambda
      REAL(p_) sgnup,betayr,hcap,hprime,hksi,fcap,fprime
!emp
      REAL(p_) tebulk
!
      alpha=TorGa_alpha(x)
!
      gamma=-log(x)*tau+gammin
      usq=abs(gamma**2-1._p_)
      u=sqrt(usq)
      uz=(gamma-yval)/enz
      uxsq=usq-uz**2
      lambda=uxsq/usq/hloc
!
      sgnup=sign(1.0_p_,uz)
!%LL      beta=2./hloc*(gamma-yval)/usq*(gamma*(gamma-yval)/(usq*enzsq)-1.)
      betayr=(enz-gamma*uz/usq)
      call TorGa_gethcap(lambda,hcap,hprime)
      hksi=-2._p_/hloc*(uz/u)*hprime
!emp
      select case (cdroutine)
      case(1,11)
         call TorGa_getfcap(u,fcap,fprime)
      case(2,12)
         tebulk = tau*tmass
         call green_func_emp(igv, tebulk, zeff, fc, u, gamma, fcap, fprime)
      case default
         print*,'WARNING! No CD model chosen'
         TorGa_funxjs = 0._p_
         return
      end select
!
!%LL      TorGa_funcds=sgnup*alpha*(fprime*hcap+beta*fcap*hprime)
      TorGa_funxjs=sgnup*alpha*(fprime*hcap+betayr*fcap*hksi/u)
!
      return
      end

      FUNCTION TorGA_bessj0(x)
      USE precision_mod, ONLY: p_
      IMPLICIT NONE
!
      REAL(p_) TorGA_bessj0,x
      REAL(p_) ax,xx,z
      REAL(p_) p1,p2,p3,p4,p5
      REAL(p_) q1,q2,q3,q4,q5
      REAL(p_) r1,r2,r3,r4,r5,r6
      REAL(p_) s1,s2,s3,s4,s5,s6
      REAL(p_) y
      SAVE p1,p2,p3,p4,p5
      SAVE q1,q2,q3,q4,q5
      SAVE r1,r2,r3,r4,r5,r6
      SAVE s1,s2,s3,s4,s5,s6
!
!234567890123456789012345678901234567890123456789012345678901234567890123
!-----------------------------------------------------------------------&
      DATA p1,p2,p3,p4,p5/1.E0_p_,-.1098628627E-2_p_                    &
     &,.2734510407E-4_p_,-.2073370639E-5_p_,.2093887211E-6_p_/
      DATA q1,q2,q3,q4,q5/-.1562499995E-1_p_,.1430488765E-3_p_          &
     &,-.6911147651E-5_p_,.7621095161E-6_p_,-.934945152E-7_p_/
      DATA r1,r2,r3,r4,r5,r6/57568490574.E0_p_,-13362590354.E0_p_       &
     &,651619640.7E0_p_,-11214424.18E0_p_,77392.33017E0_p_              &
     &,-184.9052456E0_p_/
      DATA s1,s2,s3,s4,s5,s6/57568490411.E0_p_,1029532985.E0_p_         &
     &,9494680.718E0_p_,59272.64853E0_p_,267.8532712E0_p_,1.E0_p_/
!-----------------------------------------------------------------------&
!
      IF (ABS(x).lt. 8._p_)THEN
        y=x**2
        TorGA_bessj0=(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6)))))              &
     &               /(s1+y*(s2+y*(s3+y*(s4+y*(s5+y*s6)))))
      ELSE
        ax=ABS(x)
        z=8._p_/ax
        y=z**2
        xx=ax-.785398164_p_
        TorGA_bessj0=sqrt(.636619772_p_/ax)*(cos(xx)*                   &
     &              (p1+y*(p2+y*(p3+y*(p4+y*p5))))                      &
     &               -z*sin(xx)*(q1+y*(q2+y*(q3+y*(q4+y*q5)))))
      ENDIF
!
      RETURN
      END FUNCTION TorGA_bessj0
      FUNCTION TorGA_bessj1(x)
      USE precision_mod, ONLY: p_
      IMPLICIT NONE
!
      REAL(p_) TorGA_bessj1,x
      REAL(p_) ax,xx,z
      REAL(p_) p1,p2,p3,p4,p5
      REAL(p_) q1,q2,q3,q4,q5
      REAL(p_) r1,r2,r3,r4,r5,r6
      REAL(p_) s1,s2,s3,s4,s5,s6
      REAL(p_) y
      SAVE p1,p2,p3,p4,p5
      SAVE q1,q2,q3,q4,q5
      SAVE r1,r2,r3,r4,r5,r6
      SAVE s1,s2,s3,s4,s5,s6
!
!234567890123456789012345678901234567890123456789012345678901234567890123
!-----------------------------------------------------------------------&
      DATA r1,r2,r3,r4,r5,r6/72362614232.E0_p_,-7895059235.E0_p_        &
     &,242396853.1E0_p_,-2972611.439E0_p_,15704.48260E0_p_              &
     &,-30.16036606E0_p_/
      DATA s1,s2,s3,s4,s5,s6/144725228442.E0_p_,2300535178.E0_p_        &
     &,18583304.74E0_p_,99447.43394E0_p_,376.9991397E0_p_,1.E0_p_/
      DATA p1,p2,p3,p4,p5/1.E0_p_,.183105E-2_p_,-.3516396496E-4_p_      &
     &,.2457520174E-5_p_,-.240337019E-6_p_/
      DATA q1,q2,q3,q4,q5/.04687499995E0_p_,-.2002690873E-3_p_          &
     &,.8449199096E-5_p_,-.88228987E-6_p_,.105787412E-6_p_/
!-----------------------------------------------------------------------&
!
      IF (ABS(x).lt.8._p_) THEN
        y=x**2
        TorGA_bessj1=x*(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6)))))/           &
     &           (s1+y*(s2+y*(s3+y*(s4+y*(s5+y*s6)))))
      ELSE
        ax=ABS(x)
        z=8._p_/ax
        y=z**2
        xx=ax-2.356194491_p_
        TorGA_bessj1=sqrt(.636619772_p_/ax)*                            &
     &        (cos(xx)*(p1+y*(p2+y*(p3+y*(p4+y*p5))))                   &
     &        -z*sin(xx)*(q1+y*(q2+y*(q3+y*(q4+y*q5)))))*sign(1._p_,x)
      ENDIF
!
      RETURN
      END FUNCTION TorGA_bessj1
      FUNCTION TorGA_bessj(n,x)
      USE precision_mod, ONLY: p_
      IMPLICIT NONE
!
      INTEGER n,IACC
      REAL(p_) TorGA_bessj,x,BIGNO,BIGNI
      PARAMETER (IACC=40,BIGNO=1.E10_p_,BIGNI=1.E-10_p_)
!     USES bessj0,bessj1
      REAL(P_) TorGA_bessj0,TorGA_bessj1
      EXTERNAL TorGA_bessj0,TorGA_bessj1
!-----------------------------------------------------------------------&
!234567890123456789012345678901234567890123456789012345678901234567890123
!-----------------------------------------------------------------------&
      INTEGER j,jsum,m
      REAL(p_) ax,bj,bjm,bjp,sum,tox
!
      IF (n.lt.0) THEN
         WRITE(6,"(A)")'bad argument n in bessj'
         STOP
      ENDIF
!
      IF (n.eq.0) THEN
         TorGA_bessj=TorGA_bessj0(x)
         RETURN
      ENDIF
!
      IF (n.eq.1) THEN
         TorGA_bessj=TorGA_bessj1(x)
         RETURN
      ENDIF
!
      ax=ABS(x)
      IF (ax.eq.0._p_) THEN
        TorGA_bessj=0._p_
      ELSE IF(ax.gt.FLOAT(n)) THEN
        tox=2._p_/ax
        bjm=TorGA_bessj0(ax)
        bj=TorGA_bessj1(ax)
        DO j=1,n-1
          bjp=j*tox*bj-bjm
          bjm=bj
          bj=bjp
        ENDDO
        TorGA_bessj=bj
      ELSE
        tox=2._p_/ax
        m=2*((n+INT(SQRT(FLOAT(IACC*n))))/2)
        TorGA_bessj=0._p_
        jsum=0
        sum=0._p_
        bjp=0._p_
        bj=1._p_
        DO j=m,1,-1
          bjm=j*tox*bj-bjp
          bjp=bj
          bj=bjm
          IF (ABS(bj).gt.BIGNO) THEN
            bj=bj*BIGNI
            bjp=bjp*BIGNI
            torGA_bessj=TorGA_bessj*BIGNI
            sum=sum*BIGNI
          ENDIF
          IF (jsum.ne.0)sum=sum+bj
          jsum=1-jsum
          IF (j.eq.n)TorGA_bessj=bjp
        ENDDO
!
        sum=2._p_*sum-bj
        TorGA_bessj=TorGA_bessj/sum
      ENDIF

      IF (x.lt.0._p_.and.mod(n,2).eq.1)TorGA_bessj=-TorGA_bessj
!
      RETURN
      END FUNCTION TorGA_bessj


!--------------------------------------------------------------------------
!     Gauss Legendre package
!--------------------------------------------------------------------------
      subroutine TorGa_mgauleg (x, w, n)
      use precision_mod, only: p_
      implicit none
      integer n
      REAL(p_) x(n),w(n)
!     returns the abscissas and weights for n-point
!     gauss-legendre integration
! ----------------------------------------------------------------------
      REAL(p_) eps
      parameter (eps=3.e-14_p_)
      integer i,j,m
      REAL(p_) pi,z,p1,p2,p3,pp,z1
!
      pi=acos(-1._p_)
!     pi = 3.1415926536E0_p_

      m=(n+1)/2
      do i=1,m
         z=cos(pi*(i-0.25_p_)/(n+0.5_p_))
 1       continue
         p1=1._p_
         p2=0._p_
         do j=1,n
            p3=p2
            p2=p1
            p1=((2._p_*j-1._p_)*z*p2-(j-1._p_)*p3)/j
         enddo
         pp=n*(z*p1-p2)/(z*z-1._p_)
         z1=z
         z=z1-p1/pp
         if (abs(z-z1) .gt. eps)  go to 1
         x(i)=-z
         x(n+1-i)=z
         w(i)=2._p_/((1.-z*z)*pp*pp)
         w(n+1-i)=w(i)
      enddo
!
      return
      end
!
      function TorGa_zgauleg (func, a, b, n, multi, ifail)

      use precision_mod, only: p_
      implicit none

      REAL(p_) TorGa_zgauleg,func,a,b
      integer n,multi,ifail
      external func
!
!     returns (n*multi)-point gauss-legendre quadrature of 'func'
!     from a to b.  (n*multi) should be less than nmax =64
!     multi = 1 or 2
! ----------------------------------------------------------------------
!
      integer nmax,nmaxh
      parameter (nmax=64,nmaxh=nmax/2)
      integer stderr
      parameter (stderr=6)
      integer nh,nn,ns,j
      REAL(p_) x(nmax),w(nmax),xx(nmax),y(nmax),ss,xm,xr
      REAL(p_) xh(nmaxh),wh(nmaxh)
      save ns,x,w,xh,wh
      data ns/-1/
!
      nh=n
      nn=2*nh
      if (nh .gt. nmaxh) then
         nh=nmaxh
         nn=nmax
         ifail=1
         write (stderr,'(a)')'WARNING TorGa_zgauleg: (n) set to 32 '
      endif
      if (ns .ne. nh) then
         ns=nh
         call TorGa_mgauleg(xh,wh,nh)
         call TorGa_mgauleg(x,w,nn)
      endif
!
      xm=0.5_p_*(b+a)
      xr=0.5_p_*(b-a)
      if (multi .eq. 1)then
         do j=1,nh
            xx(j)=xm+xr*xh(j)
            y(j)=func(xx(j))
         enddo
         ss=0._p_
         do j=1,nh
            ss=ss+wh(j)*y(j)
         enddo
         TorGa_zgauleg=ss*xr
      else if (multi .eq. 2)then
         do j=1,nn
            xx(j)=xm+xr*x(j)
            y(j)=func(xx(j))
         enddo
         ss=0._p_
         do j=1,nn
            ss=ss+w(j)*y(j)
         enddo
         TorGa_zgauleg=ss*xr
      else
         write (stderr,'(a)')'TorGa_zgauleg:  dimensional error'
!         temporarily commented next statement E.Westerhof 2009
!         call TGaLib_stop('TorGa_zgauleg', 0)
      endif
      return
!
      end
      subroutine TorGa_ceqmdl(eps,thetap)

      use precision_mod, only: p_
      implicit none

      REAL(p_) eps,thetap
      include 'globcd.h'
!     return geometrical quantities of circular model equilibrium with
!     given eps and thetap
!--------------------------------------------------------------------------
!     real pi
!
      pi=acos(-1._p_)
!     pi = 3.1415926536E0_p_

      hloc=(1._p_-eps)/(1._p_+eps*cos(thetap))
      href=(1._p_-eps)/(1._p_+eps)
      hav=1._p_-eps
      hsqav=(1._p_-eps)**2/sqrt(1._p_-eps**2)
      chrtav=(sqrt(2._p_*eps*(1._p_-eps))+(1._p_+eps)*   &
     &        asin(sqrt(2._p_*eps/(1._p_+eps))))/pi
!
!     cxi2=-0.25*(hsqav-hav**2)/(1.-hav)**2
!      cxi2=-0.25*(1.-eps)**2/(sqrt(1.-eps**2)+1.-eps**2)
!      cxi4=chrtav**2/(1.-hav)-1.
!
      return
      end

      subroutine TorGa_getftrap

      use precision_mod, only: p_
      implicit none

      include 'globcd.h'
      ! 'globcd2.h'
      REAL(p_) zrtmp, fctmp
      common /dumm/zrtmp, fctmp

      REAL(p_) hbar,c2,c4
      common /cmbftrap/hbar,c2,c4
!---------------------------------------------------------------------------
      REAL(p_) tolft
      parameter (tolft=1.e-6_p_)
      REAL(p_) ap,ss
      integer jromb
      external TorGa_qftint
!
      hbar=hav
      c2=cxi2
      c4=cxi4
!
      ap=0.5_p_*sqrt(1._p_-hav)*hsqav/hav**2*     &
     &    (2._p_+hav)-hsqav/hav**2+1._p_
!
      call TorGa_mqromb1(TorGa_qftint,0._p_,1._p_,ss,tolft,jromb)
      ft=ap-0.75_p_*sqrt(1._p_-hav)*hsqav*ss
      fc=1._p_-ft
!
      return
      end
      subroutine TorGa_gethcap(z_in,hcap,hprime)

      use precision_mod, only: p_
      implicit none

      REAL(p_) z_in,hcap,hprime
      include 'globcd.h'
!----------------------------------------------------------------------------
      REAL(p_) z,s,ss,ap
      integer jromb
      external TorGa_hcapint
! DMC bugfix: prevent the first arg from reaching or exceeding 1.0
      z=min(0.9999999_p_,z_in)
      s=z*(1._p_-hav)/(1._p_-z*hav)
      hprime=-0.5_p_/sqrt(1._p_-z*hav)/sqrt(1._p_+s**2* &
     &     (cxi2*(1._p_-s**2)+cxi4*s**2) )
!
      ap=sqrt(1._p_-hav)/hav*(1._p_/sqrt(1._p_-(1._p_-s)*hav)-1._p_)
      call TorGa_mqromb1(TorGa_hcapint,s,1._p_,ss,tolval,jromb)
!%PR      write (6,'(a,e12.4,i5)')'gethcap:   /z/jromb = ',z,jromb
      hcap=0.5_p_*sqrt(1._p_-hav)*ss+ap
!
      return
      end
!---------------------------------------------------------------------------
      function TorGa_hcapint(s)

      use precision_mod, only: p_
      implicit none

      REAL(p_) TorGa_hcapint,s
      include 'globcd.h'
!     return the integrand for evaluating hcap
!-------------------------------------------------------------------------
      REAL(p_) qq
      qq=s**2*(cxi2*(1._p_-s**2)+cxi4*s**2)
      TorGa_hcapint=-qq/sqrt(1._p_-hav+s*hav)**3  &
     &     /(sqrt(1._p_+qq)*(1._p_+sqrt(1._p_+qq)) )
!
      return
      end

      subroutine TorGa_getfcap(u,fcap,fprime)

      use precision_mod, only: p_
      implicit none

      REAL(p_) u,tol
      REAL(p_) fcap,fprime
       include 'globcd.h'
       ! 'globcd2.h'
       REAL(p_) zrtmp, fctmp
       common /dumm/zrtmp, fctmp

!----------------------------------------------------------------------------
      REAL(p_) uv,rhocap,gamma,ss
      integer jromb
      common /cmbqfc/uv,rhocap,gamma
      external TorGa_fcapint
!
      if (u .le. 0._p_)then
         fcap=0._p_
         fprime=0._p_
         return
      endif
!
      if (igv .eq. 0)then
         fcap=1._p_/(fc*(zrat+4._p_))*u**4
         fprime=1._p_/(fc*(zrat+4.))*(4._p_*sqrt(1._p_+u**2)*u**2)
      else
         uv=u
         rhocap=zrat
         gamma=sqrt(1._p_+u*u)
         tol=tolval
         call TorGa_mqromb1(TorGa_fcapint,0._p_,1._p_,ss,tol,jromb)
!%PR         write (6,'(a,e12.4,i5)')'getfcap:  /u/jromb = ',u,jromb
         fcap=u**4/fc*ss
         fprime=(u/gamma)**2/fc-rhocap*fcap/u**2
      endif
!
      return
      end
!-------------------------------------------------------------------------
      function TorGa_fcapint(y)

      use precision_mod, only: p_
      implicit none

      REAL(p_) TorGa_fcapint,y
      REAL(p_) uv,rhocap,gamma
      common /cmbqfc/uv,rhocap,gamma
!-------------------------------------------------------------------------
      REAL(p_) gamy
!
!     gamma=sqrt(1._p_+uv**2)
      gamy=sqrt(1._p_+(uv*y)**2)
      TorGa_fcapint=y**(rhocap+3._p_)*((1._p_+gamma)/(1._p_+gamy))**rhocap/gamy**3
!
      return
      end
      subroutine TorGa_getlims(etmax,epst1,epst2)

      use precision_mod, only: p_
      implicit none

      REAL(p_) etmax,epst1,epst2
!  real gammin,gammax,etmax,epst1,epst2
!  real etcutoff
!  real tau,yval,enzsq,hloc
      include 'globcd.h'
!------------------------------------------------------------------------
      REAL(p_) gam1,gam2,gam3,gam4,ettmp
      REAL(p_) xisqc
!
!     find the intersections of resonance curve with u_perp=0
      call TorGa_gamsrc(gam1,gam2,1._p_)
!
      if (gam2 .le. 1._p_)then     ! no resonance set etmax negative
         gammin=1._p_
         gammax=1._p_
         etmax=-1._p_
         epst1=-1._p_
         epst2=-1._p_
         return
      endif
!
!     in the case of gam2 > 1.0
      if (gam1 .le. 1._p_)then
         gammin=gam2
         etmax=etcutoff
         gammax=etmax*tau+gammin
      else
         gammin=gam1
         ettmp=(gam2-gam1)/tau
         etmax=MIN(etcutoff,ettmp)
         gammax=etmax*tau+gammin
      endif
!
!     find the intersections of resonance curve with passing-
!     trapped separatrix
      xisqc=1._p_-hloc
      call TorGa_gamsrc(gam3,gam4,xisqc)
      epst1=(gam3-gammin)/tau
      epst2=(gam4-gammin)/tau
      return
!
      end

      subroutine TorGa_gamsrc(gam1,gam2,xisq)

      use precision_mod, only: p_
      implicit none

      REAL(p_) gam1,gam2
      REAL(p_) xisq
      include 'globcd.h'
!     return the gamma values on the resonace curve for given yval,
!     enzsq, and xisq
!     yval=nharm*(omega_c)/omega
!     enzsq=(n_parallel)^2
!     xisq=(u||/u)^2
!-----------------------------------------------------------------------
      REAL(p_) rutsq,dd,rut,gtmp
!
      rutsq=xisq*enzsq*(yval**2-1._p_+enzsq*xisq)
!
!     if no physical roots, returns gam1=gam2=-1.e10
      if (rutsq .lt. 0._p_)then  ! no physcal roots
         gam1=-1.e10_p_
         gam2=-1.e10_p_

         return
      endif
!
      dd=1._p_/(1._p_-xisq*enzsq)
      rut=sqrt(rutsq)
      gam1=dd*(yval-rut)
      gam2=dd*(yval+rut)
!     arrange roots in increasing order
      if (gam2 .lt. gam1)then
         gtmp=gam2
         gam2=gam1
         gam1=gtmp
      endif
!
      return
      end
      SUBROUTINE TorGa_mqromb1(func,a,b,ss,eps,jt)
      use precision_mod, only: p_
      implicit none

      INTEGER JMAX,JMAXP,K,KM
      REAL(p_) a,b,ss, eps
      integer jt
      EXTERNAL func
      PARAMETER (JMAX=200, JMAXP=JMAX+1, K=5, KM=K-1)
!CU    USES TorGa_polint,trapzd
      INTEGER j
      REAL(p_) dss,h(JMAXP),s(JMAXP)
      if (a .eq. b)then
         ss=0._p_
         return
      endif
      h(1)=1._p_
      do 11 j=1,JMAX
         jt=j
        call TorGa_trapzd(func,a,b,s(j),j)
!        write (6,*)j
        if (j.ge.K) then
          call TorGa_polint(h(j-KM),s(j-KM),K,0.0_p_,ss,dss)
          if (abs(dss).le.eps*abs(ss)) return
        endif
        s(j+1)=s(j)
        h(j+1)=0.25_p_*h(j)
11    continue
      write (6,'(a)') 'warning:   too many steps in mqromb'
      END
!------------------------------------------------------------------------
      SUBROUTINE TorGa_trapzd(func,a,b,s,n)
      use precision_mod, only: p_
      implicit none

      INTEGER n
      REAL(p_) a,b,s
      REAL(p_) func
      EXTERNAL func
      INTEGER it,j
      REAL(p_) del,sum,tnm,x
      if (n.eq.1) then
        s=0.5_p_*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5_p_*del
        sum=0._p_
        do 11 j=1,it
          sum=sum+func(x)
          x=x+del
11      continue
        s=0.5_p_*(s+(b-a)*sum/tnm)
      endif
      return
      END
      function TorGa_qftint(s)

      use precision_mod, only: p_
      implicit none

      REAL(p_) TorGa_qftint,s
      REAL(p_) hbar,c2,c4
      common /cmbftrap/hbar,c2,c4
!----------------------------------------------------------------------
      REAL(p_) dd
      dd=c2*(1._p_-s**2)+c4*s**2
      TorGa_qftint=-dd*s**3/sqrt(1._p_-hbar+s*hbar)**5/  &
     &     (sqrt(1._p_+dd*s**2)*(1._p_+sqrt(1._p_+dd*s**2)))
!
      return
      end
      SUBROUTINE TorGa_polint(xa,ya,n,x,y,dy)
      use precision_mod, only: p_
      implicit none

      INTEGER n,NMAX
      REAL(p_) dy,x,y,xa(n),ya(n)
      PARAMETER (NMAX=10)
      INTEGER i,m,ns
      REAL(p_) den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=abs(x-xa(1))
      do 11 i=1,n
        dift=abs(x-xa(i))
        if (dift.lt.dif) then
          ns=i
          dif=dift
        endif
        c(i)=ya(i)
        d(i)=ya(i)
11    continue
      y=ya(ns)
      ns=ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den.eq.0._p_)then
             write (6,'(a)') 'warning:  failure in TorGa_polint'
             return
          endif
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
12      continue
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
13    continue
      return
      END

!#######################################################################
 !INCLUDE 'const_and_precisions.f90'
 !INCLUDE 'green_func_ext.f90'
!#######################################################################

 SUBROUTINE green_func_emp(igv,Te,Zeff,fc,u,gam, K,dKdu)
!=======================================================================
! Author:  N.B.Marushchenko
! March 2010: prepared for TORBEAM
!
! Returns the 'velocity part' of the Green's function
! Actually, is only the shell for recalling the original subroutines.
!=======================================================================
! INPUTS:
!  igv      - switcher for the models (emp definitions)
!  adj_appr - switcher for the models (nm definitions)
!  Te       - temperature [keV]
!  Zeff     - effective charge
!  fc       - fraction of circulating particles
!  u        - p/sqrt(2mT)
!  gam      - relativistic factor
!
! OUTPUTS:
!   K   - Spitzer's function
!  dKdu = dK/du, i.e. its derivative over normalized momentum
!=======================================================================
! USE precision_mod         ! emp
! USE real_mod              ! emp
!---
! NM
 USE green_func_ext, ONLY: wp_, mc2_, Setup_SpitzFunc, SpitzFuncCoeff, &
  GenSpitzFunc
  use torga_curgap_module, only: green_func_emp_first
!---
 IMPLICIT NONE
!--- remove it later! ---
 INTEGER, PARAMETER :: p_ = 8
!---
 INTEGER,          INTENT(in)  :: igv
 REAL(p_),         INTENT(in)  :: Te,Zeff,fc,u,gam
 REAL(p_),         INTENT(out) :: K,dKdu
!--- internal variables
 REAL(wp_) :: SS1,ne1,Te1,Zeff1,fc1,u1,q1,gam1
 REAL(wp_) :: K1,dKdu1
 CHARACTER(Len=1) :: adj_appr(6)
!=======================================================================
!--- Spitzer function definitions ---
 adj_appr(1) = 'l'         ! collisionless limit
! adj_appr(1) = 'c'         ! collisional (classical) limit, w/o trap. part.
 adj_appr(2) = 'm'         ! momentum conservation
! adj_appr(2) = 'h'         ! high-speed limit
!--- Green's function (pitch-angle part) ---
 adj_appr(3) = 'l'         ! DO NOT CHANGE!
!--- Spitzer function approach: relativistic or non-relat. ---
 adj_appr(4) = 'r'         ! relativistic formulation
! adj_appr(4) = 'n'         ! non-relativistic formulation
!--- Spitzer function (method) ---
 adj_appr(5) = 'v'         ! DO NOT CHANGE!
!---
 adj_appr(6) = 'i'         ! DO NOT CHANGE!
!=======================================================================
 Te1   = Te
 Zeff1 = Zeff
 fc1   = fc
 q1    = u
 u1    = u/sqrt(2*Te/mc2_)
 gam1  = gam
!---
 IF (green_func_emp_first) THEN
   CALL Setup_SpitzFunc(adj_appr)
   green_func_emp_first =.false.
 ENDIF
!---
 CALL SpitzFuncCoeff(Te1,Zeff1,fc1)
 CALL GenSpitzFunc  (Te1,Zeff1,fc1,u1,q1,gam1, K1,dKdu1)
! CALL GreenFunction (SS,Te1,Zeff1,b,bav,b2av,ft, &
!                            gam,qq,qpar,lamb, X,dXdw,dXdqpar)
!---
  K   =  K1
 dKdu = dKdu1
!emp: added for consistency with curgap by Lin-Liu (where u_LL=p/mc):
!     getfcap in curgap returns (vth/c)^4*K/fc and
!     (vth/c)^4*gamma/u_LL*d(K/fc)/du_LL
 K = K/fc*(2*Te/mc2_)**2
 dKdu = dKdu/fc/u1*gam*(2*Te/mc2_)
!=======================================================================
 RETURN
 END SUBROUTINE green_func_emp

!#######################################################################
