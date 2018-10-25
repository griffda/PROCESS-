module hare

contains
    subroutine hare_calc(dens,bfield,R0,amin,rho,te0,zeff, nout)
        use mode
        use param
        use freq
        use process_output, only:ocmmnt, ovarre, ovarin, ovarrf
        implicit none
        real(rkind), intent(in) :: dens,bfield,R0,amin,rho,te0,zeff
        integer(ikind), intent(in) :: nout
        real(rkind) :: acosarg,xnpa,enpep,angnb,op,oc
        real(rkind) :: ocmax,ocmin,cdeff
        real(rkind) :: delsq,xnpe,xf,enpa
        real(rkind) :: facop,conf,bmin,bmax
        real(rkind) :: uparm,gammam,mcsq,ftherm,fpp,sgnnpa,nharm
        real(rkind) :: Ra,Rpp,fshift,fapp,Teomc2,nucycloc,fabs,extrafac
        real(rkind) :: deltarho
        real(rkind) :: bigG
        real(rkind) :: perc, kAperMW
        real(rkind) :: te
        complex(rkind) :: ex,ey,ez

        integer(ikind) :: lh1
        integer(ikind) :: cdharm = 1

        external currn

        imod = 1

        if (imod == 1) nharm=1.e0_rkind
        if (imod ==-1) nharm=2.e0_rkind

        ! Cap temperature at 30 keV (mimick ECCD saturation)
        if (te0 > 3.e1_rkind) then
            te = 3.e1_rkind
            write(*,*)'Temperature capped at 30 keV (mimick ECCD saturation)'
        else
            te = te0
        end if

        ! guess position on poloidal surface (here 60 deg)
        acosarg = cos(pi*6.e1_rkind/1.8e2_rkind)
        ! determination of E/kT (ratio between energy of resonant electrons and
        ! temperature) from Bornatici et al. NF 83, Eq. 3.1.67
        mcsq = 5.11e2_rkind
        Teomc2 = te/mcsq
        nucycloc = 2.8e0_rkind*bfield/3.e0_rkind
        facop = 4.e0_rkind*pi*2.53616e8_rkind/1.e6_rkind
        conf = 2.e9_rkind*pi*27.9924e0_rkind
        op = dens*facop
        oc = (conf*bfield)**2
        !fabs=1/5 means that the absorption length is (1/5) of the minor radius
        fabs = 2.e-1_rkind;
        !Extra factor from Bornatici's formula, to be fixed to match simulations
        extrafac = 1.;
        ftherm = log(sqrt(2.*pi*Teomc2)*fabs*amin*nucycloc*op/oc/extrafac)
        !      ftherm=4.e0_rkind
        ! determination of optimum wave frequency and npar
        gammam = ftherm*te/mcsq+1.e0_rkind
        uparm = sqrt(gammam**2-1.e0_rkind)
        !         far=amin/(R0+rho*amin)
        deltarho = fabs
        Ra = R0+acosarg*amin*rho
        Rpp= R0+acosarg*amin*(rho+deltarho)
        fapp = Ra/Rpp
        bigG=sqrt(1.e0_rkind-fapp**2)
        fshift=sqrt(uparm**2+1.e0_rkind)+uparm*bigG
        xf = 2.8e10_rkind*nharm*bfield*fshift

        xnpa=1.e0_rkind-(fapp/fshift)**2
        sgnnpa = 1.e0_rkind; enpa = sgnnpa*sqrt(xnpa)
        fpp=(fshift/fapp-1.e0_rkind)/Teomc2

        !      xf = 2.25e11_rkind
        om = 2.e0_rkind*pi*xf
        xk0 = om/c
        facop = facop/om**2
        conf = conf/om
        conf = conf**2
        op = dens*facop
        oc=conf*bfield**2
        !      rho=0.1;amin=2.92e2
        bmax = bfield*R0/(R0-rho*amin)
        bmin = bfield*R0/(R0+rho*amin)
        ocmax = conf*bmax**2
        ocmin = conf*bmin**2

        ! Appleton-Hartree
        delsq = oc*(1.e0_rkind-xnpa)**2 +4.e0_rkind*xnpa*(1.e0_rkind-op)
        if (imod.eq.1) then
            !  O-mode
            xnpe = 1.e0_rkind-xnpa-op-op*(oc*(1.e0_rkind+xnpa)-sqrt(oc*delsq))/ &
            2.e0_rkind/(1.e0_rkind-oc-op)
        else if (imod.eq.-1) then
            !  X-mode
            xnpe = 1.e0_rkind-xnpa-op-op*(oc*(1.e0_rkind+xnpa)+sqrt(oc*delsq))/ &
            2.e0_rkind/(1.e0_rkind-oc-op)
        endif
        !
        !      enpa  = sqrt(xnpa)*cos(angnb)/abs(cos(angnb))
        angnb = acos(enpa/sqrt(xnpa+xnpe))

        ! Wave polarization, cold plasma
        call polar(oc,op,enpa,sqrt(xnpe),imod,ex,ey,ez)

        enpep=sqrt(xnpe)

        ! >>>
        !Only O-mode implemented!!!!
        !      nharm = nharm+1.e0_rkind
        !      te2 = te*6.7e-1_rkind
        !      bf2 = bfield*R0/(R0+5.e-1_rkind*amin*acosarg)
        !      drho2 = 1.e0_rkind-rho-5.e-1_rkind*deltarho
        !      write(*,210)dens,bf2,te2,xf,enpa,drho2,angnb,amin,nharm,perc
        !210   format(10(1x,1pe11.4))
        !      call parasitic(dens,bf2,te2,xf,enpa,drho2,angnb,amin,nharm,perc)
        !      print*,'Power fraction absorbed by 2nd harmonic',1.e0_rkind-perc

        !do i=1,11
        !   acosarg = real(i-1)*pi/1.e1_rkind
        !   acosarg = cos(acosarg)

        call currn(om,acosarg,xk0,xnpa,enpep,angnb,op,oc,ocmax,ocmin, &
        te,zeff,cdeff,lh1,ex,ey,ez,cdharm)

        perc = 1.e0_rkind
        kAperMW = perc*1.e3_rkind*cdeff/2.e0_rkind/pi/R0

        print*,'Frequency = Optimum wave frequency for ECCD (GHz)',xf/1.e9_rkind
        print*,'Nparallel ',enpa
        print*,'f_T = ratio of energy of resonant electrons to kTe at point of maximum &
                & absorption ',ftherm
        print*,'f_pp = ratio of energy of resonant electrons to kTe at pinch &
                        & point',fpp
        print*,'cdeff = Normalised current drive efficiency (eq 34: eta &
                & = Icd.2.pi.R0 / P )  (Am/W)', cdeff
        print*,'curr = Driven current per unit absorbed power (kA/MW) ', &
                perc*1.e3_rkind*cdeff/2.e0_rkind/pi/R0

        write(*,*)nout

        call ocmmnt(nout,'ECCD results from "HARE" ')
        call ocmmnt(nout,'("Fast evaluation of the current driven by electron &
                          &cyclotron waves for reactor studies", E. Poli)')
        call ocmmnt(nout,'Frequency shift = wave frequency / cold cyclotron resonance frequency on plasma axis')
        call ovarrf(nout,'Frequency shift','(fshift)', fshift, 'OP ')
        call ovarrf(nout,'Optimum wave frequency for ECCD (GHz)','(xf/1e9)', xf/1.e9_rkind, 'OP ')
        call ovarrf(nout,'Nparallel','(enpa)', enpa, 'OP ')
        call ovarrf(nout,'fT = Energy of resonant electrons / kTe at point of max absorption','(ftherm)', ftherm, 'OP ')
        call ovarrf(nout,'Ratio of energy of resonant electrons to kTe at pinch &
                         & point','(fpp)', fpp, 'OP ')
        call ovarrf(nout,'Normalised CD efficiency: eta = Icd.2.pi.R0 / P (Am/W)','(cdeff)', cdeff, 'OP ')
        call ovarrf(nout,'Driven current per unit absorbed power (kA/MW) ','(kAperMW)', kAperMW, 'OP ')


        !enddo

    end subroutine hare_calc

    subroutine polar(oc,op,npar,nperp,imode,exnorm,eynorm,eznorm)
        use param
        implicit none

        integer(ikind), intent(in) :: imode
        real(rkind), intent(in) :: op,oc,npar,nperp
        real(rkind) :: Sbig,Dbig,Pbig
        real(rkind) :: omod,xmod
        complex(rkind) :: ex,ey,ez,lamxx,eps12,lamxz,lamyy,lamyz,lamzz
        complex(rkind) :: exnorm,eynorm,eznorm

        !Elements of the dispersion tensor

        !----------------------------------------------------------------------------
        !Calcualation of the polarization vector by N. Bertelli copied from DAMPBQ
        !
        !----------------------------------------------------------------------------
        !   COMPONENT OF DIELETRIC TENSOR
        !   (CORRESPONDING TO EQ.(5) OF Physics Fluids 21 (1978) 645
        !   or from Eq.(57) OF TORAY MANUAL (1989))
        !emp: cold plasma
        Sbig = 1.e0_rkind - op/(1.e0_rkind - oc)
        Dbig = -op*sqrt(oc)/(1.e0_rkind-oc)
        Pbig = 1.e0_rkind - op
        lamxx = -npar**2 + Sbig
        eps12 = -(0.e0_rkind,1.e0_rkind)*Dbig
        lamxz = npar*nperp
        lamyy = -npar**2 - nperp**2 + Sbig
        lamyz = 0.e0_rkind
        lamzz = -nperp**2 + Pbig

        !----------------------------------------------------------------------------
        !CL    O-MODE POLARIZATION VECTOR (NORMALIZED TO EZ)
        !      (SEE EQ.(54) OF TORAY MANUAL (1989))

        if (imode == 1) then
            ex=(eps12*lamyz-lamxz*lamyy)/&
            (lamxx*lamyy+eps12**2)
            ey=(lamxx*lamyz+eps12*lamxz)/&
            (-1.e0_rkind*eps12**2-lamxx*lamyy)
            !     VECTOR NORMALIZATION
            omod = sqrt(abs(ex)**2+abs(ey)**2+1.e0_rkind)
            exnorm = ex/omod
            eynorm = ey/omod
            eznorm = 1.e0_rkind/omod

            !----------------------------------------------------------------------------
            !CL    X-MODE POLARIZATION VECTOR (NORMALIZED TO EY)
            !      (SEE EQ.(54) OF TORAY MANUAL (1989))

        else if (imode == -1) then
            ex = - ( lamyz*lamxz / lamzz + eps12 ) /&
            ( lamxx - lamxz**2 / lamzz )
            ez = - ( lamxz * ex - lamyz ) / lamzz
            !     VECTOR NORMALIZATION
            xmod = sqrt(abs(ex)**2+1.e0_rkind+abs(ez)**2)
            exnorm = ex/xmod
            eynorm = 1.e0_rkind/xmod
            eznorm = ez/xmod
        end if

        !     END POLARIZATION BY BERTELLI
        !----------------------------------------------------------------------------
    end subroutine polar

    subroutine parasitic(ne,bmod,temp,freq,npar,drho,theta,am,nh,percent)

        use param
        implicit none

        real(rkind) :: ne,bmod,temp,freq,npar,drho,theta,am,nh
        !real(rkind) :: angfac,muO
        !real(rkind) :: gammalow
        real(rkind) :: alpha2,percent

        alpha2 = pi*sqrt(2.e0_rkind*pi)*angfac(nh,theta)*muO(nh,theta) &
        *8.1e1_rkind*ne/c/2.8e10_rkind/bmod &
        *sqrt(temp/5.11e2_rkind) &
        *exp(-5.e-1_rkind*5.11e2_rkind*(1.e0_rkind-nh*2.8e10_rkind &
        *bmod/freq)**2/npar**2/temp)/npar
        !             *bmod/gammalow(nh,npar,bmod,freq)/freq)**2/npar**2/temp)/npar
        print*,'B, theta, alpha_2 ',bmod,theta,alpha2*1.e2_rkind
        percent = exp(-alpha2*drho*am)

    end subroutine parasitic

    function angfac(nh,th)
        use param
        implicit none
        real(rkind) :: nh, th, angfac

        angfac = sin(th)**(2.e0_rkind*(nh-1.e0_rkind))
        angfac = angfac*(1.e0_rkind+cos(th)**2)

    end function angfac

    function muO(nh,th)
        use param
        implicit none
        real(rkind) :: nh, th, sin4o4nh, muO

        sin4o4nh = sin(th)**4/4.e0_rkind/nh

        muO = sin4o4nh+cos(th)**2
        muO = 5.e-1_rkind-muO/(sqrt(sin4o4nh/nh+cos(th)**2)* &
        (1.e0_rkind+cos(th)**2))

    end function muO

    function gammalow(nh,npa,bf,xf)
        use param
        implicit none
        real(rkind) :: nh, npa, bf, xf, Ombar, npa2, gammalow

        Ombar = 2.8e10_rkind*bf/xf
        npa2 = npa*npa
        gammalow = (nh*Ombar-npa*sqrt((nh*Ombar)**2-1.e0_rkind+npa2))/ &
        (1.e0_rkind-npa2)

    end function gammalow
end module hare
