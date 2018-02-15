subroutine cubsfml(cc,cubb, &
  jrad,x,q,tepr,nx,fuelmix,rmajor,btor)

  use grad_func
        
!x(nx)                                     : local real array : radial linspace normalized to one
!tepr(nx)                                  : local real array : tepr=interp1([0.0d0, xtrt], [T_e0(1), T_e0], xr) interpolated in Te profile known rho
!tipr(nx)                                  : local real array : tipr=interp1([0.0d0, xtrt], [T_i0(1), T_i0], xr)
!nepr(nx)                                  : local real array : nepr=interp1([0.0d0, xtrt], [N_e0(1), N_e0], xr)
!xr(nx)                                    : local real array : xr = x*amin  --> rho

    real(kind(1.0d0)), dimension(nx) :: x, tepr, tipr, nepr, xr

!nions(nx)                                 : local real array : nions = ndeut + ntrit + nNe + nXe + nHe ion density
!zeff(nx)                                  : local real array : zeff = (1.0d0/nepr) * (ndeut+ntrit+4.0d0*nHe+zavne**2*nNe+zavxe**2*nXe)

    real(kind(1.0d0)), dimension(nx) :: nions, zeff!  conflict with process



    ! others

!btor                                      : local real : btor = 1.8d0 * rmajor**0.6d0...qedge = 12.0d0 * rminor**2 * btor / (rmajor * Ip)...betpl = 4.0d0*1.6d-4*pi*(nel*tel + nii*tii)*(rtor/(btor*rho*(mux+0.000001)))**2...betple = 4.0d0*1.6d-4*pi*(nel*tel)*(rtor/(btor*rho*(mux+0.000001)))**2
!rmajor                                    : local real : rmajor = 8.0d0...rminor = rmajor/aspect...Ip = (rmajor - 1.35d0)*3.0d0...btor = 1.8d0 * rmajor**0.6d0...qedge = ...gn_e(1:nxt) = ...gT_i0(1:nxt) = ...gyhat = ...tau_scal =...Vn = ...b(j,1)=...rtor = ...yd = ...chat = ...

    real(kind(1.0d0)) :: btor, rmajor!, conflict with process

!fuelmix                                   : local real : deuterium number fraction in hydrogen ions - fuelmix = 0.5d0...amain = 2*fuelmix+3*(1.-fuelmix) !D+T...amain = (1.0d0-fuelmix)*2.0d0 + fuelmix*3.0d0...ndeut = fuelmix*(nepr - nNe*zavne - nXe*zavxe - 2.0d0*nHe)...ntrit = (1.0d0-fuelmix)*(nepr - nNe*zavne - nXe*zavxe - 2.0d0*nHe)
!zmain                                     : local real : H's Z - zmain = 1.0d0 -- nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))
!amain                                     : local real : H's A - amain = 2*fuelmix+3*(1.-fuelmix) !D+T...nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))...nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrt(tii/amain))

    real(kind(1.0d0)) :: fuelmix, zmain, amain!, conflict with process

!rtor                                      : local real : rtor = rmajor...sqeps = ...betpl = ...betple = ...nues = ...nuis ...
!yd                                        : local real : ?? - yd = -0.8d0 * pi**2 * rmajor...ya = 2.0d0/(hro**2 * yd)

    real(kind(1.0d0)) :: rtor, yd

 
!shif(nx)                                  : local real array : shift, calculated in compute_equil, sqeps = sqrt(ametr/(rtor+shif))...call trmodel(...SHIF,...,0.*shif,0.*shif,0.*shif,...)
!q(nx)                                     : local real array : q, calculated in compute_equil, output variable - shear = gradient(log(q),log(x))...q_tr = interp1(x, q, xtr/xr(nx))...q0 = q...mux = 1.0d0/q
!rho(nx)                                   : local real array : rho, calculated in compute_equil - xtr = linspace(x0, xb, nxt) * amin  ! rho normalized for transport...hro = gradient1(rho)...betpl = ...betple = ...
!psi(nx)                                   : local real array : psi, calculated in compute_equil, dpsi = gradient1(psi)

    real(kind(1.0d0)), dimension(nx) :: shif, rho, psi, q!   conflict with process

!jrad                                      : local integer : do jrad = 1, size(x)

    integer :: jrad

!mux(nx)                                   : local real array : rotational transform - mux = 1.0d0/q...betpl =...betple =...nues =...nuis =...
!hro(nx)                                   : local real array : minor radius differential - hro = gradient1(rho) - ya = 2.0d0/(hro**2 * yd)
!ametr(nx)                                 : local real array : ?? - ametr = xr...sqeps = sqrt(ametr/(rtor+shif))
!!!!!!! used in cubsfml.inc
!nel(nx)                                   : local real array : nel = nepr...betpl = ...betple = ...coulg = ...nuee = ...zavg = ...
!tel(nx)                                   : local real array : tel = tepr...betpl = ...betple = ...coulg = ...nuee = ...nues = ...
!tii(nx)                                   : local real array : tii = tipr...betpl = ...nui = ...nuis = ...
!nii(nx)                                   : local real array : nii = nions...betpl = ...zavg = ...nui = ...
!ya(nx)                                    : local real array : ya = 2.0d0/(hro**2 * yd)...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)

    real(kind(1.0d0)), dimension(nx) :: mux, hro, ametr, nel, tel, tii, nii, ya

!sqeps(nx)                                 : local real array : sqeps = sqrt(ametr/(rtor+shif))...nues = ...nuis = ...tpf = ...
!dlogte(nx)                                : local real array : dlogte = gradient1(log(tepr))/2.0d0...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!dlogti(nx)                                : local real array : dlogti = gradient1(log(tipr))/2.0d0...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!dlogne(nx)                                : local real array : dlogne = gradient1(log(nepr))/2.0d0...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!dpsi(nx)                                  : local real array : dpsi = gradient1(psi)...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!betpl(nx)                                 : local real array : poloidal beta? - betpl = 4.0d0*1.6d-4*pi*(nel*tel + nii*tii)*(rtor/(btor*rho*(mux+0.000001)))**2...dcsa = ...hcsa = ...xcsa = ...
!betple(nx)                                : local real array : poloidal beta for electrons? - betple = 4.0d0*1.6d-4*pi*(nel*tel)*(rtor/(btor*rho*(mux+0.000001)))**2...hcsa = ...xcsa = ...

    real(kind(1.0d0)), dimension(nx) :: sqeps, dlogte, dlogti, dlogne, dpsi, betpl, betple

!nuee(nx)                                  : local real array : nu_ee?? - nuee = 670.0d0*coulg*nel/tel**1.5d0...nues = nuee*1.4d0*zeff*rtor/(mux*sqeps**3 * sqrt(tel)*1.875d7)
!nues(nx)                                  : local real array : nu_es?? - nues = nuee*1.4d0*zeff*rtor/(mux*sqeps**3 * sqrt(tel)*1.875d7)...zdf = ... (various occurrences)
!zavg(nx)                                  : local real array : average Z - zavg = nel/nii...nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))
!nui(nx)                                   : local real array : nu_i?? - nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))...nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrt(tii/amain))
!nuis(nx)                                  : local real array : nu_is?? - nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrt(tii/amain))...alp = ...a1 = ...
!coulg(nx)                                 : local real array : ?? - coulg = 15.9d0 - 0.5d0*log(nel) + log(tel)...nuee = 670.0d0*coulg*nel/tel**1.5d0
!cc(nx)                                    : local real array : ?? , calculated in compute_equil, also here- cc = 0.0*tepr+1.0d0...compute_equil...cc(jrad) = 1041d0/20d0 * tepr(jrad)**(1.5d0)
!tpf(nx)                                   : local real array : ?? - tpf = 1.0d0 - (1.0d0-sqeps**2) * sqrt(1.0d0 - sqeps**2)/(1.0d0+1.46d0*sqeps)...zft = tpf
!cubb(nx)                                  : local real array : ?? , calculated in compute_equil, also here- cubb =0.d0*nepr...compute_equil...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)

    real(kind(1.0d0)), dimension(nx) :: nuee, nues, zavg, nui, nuis, coulg, cc, tpf, cubb

!zz(nx)                                    : local real array : zz = zeff...zdf = ...dcsa = ...hcee = ...hcei = ...xcsa = ...
!zft(nx)                                   : local real array : ?? - zft = tpf...zdf = ...zft = zft/zdf...dcsa = ...zft = tpf...zdf = ...zfte = zft/zdf...zfti = ...a0 = ...alp = ...a1 = ...
!zdf(nx)                                   : local real array : ?? - zdf = ...(several occurrences)...zfte = zft/zdf...zfti = zft/zdf
!dcsa(nx)                                  : local real array : ?? - dcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zft - 1.9d0/(zz+1.0d0)*zft*zft...dcsa = dcsa + (0.3d0*zft*zft + 0.2d0*zft*zft*zft) * zft/(zz+1.0d0)...dcsa = dcsa * betpl...hcsa = ...xcsa = ...cubb = ...
!hcee(nx)                                  : local real array : ?? - hcee = (0.05d0 + 0.62d0*zz)/zz/(1.0d0+0.44d0*zz)*(zfte-zfte4)...hcee = hcee + (zfte2 - zfte4 - 1.2d0*(zfte3-zfte4))/(1.0d0+0.22*zz)...hcee = hcee + 1.2d0/(1.0d0+0.5d0*zz)*zfte4...hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)
!hcei(nx)                                  : local real array : ?? - hcei = -1.0d0*(0.56d0 + 1.93d0*zz)/zz/(1.0d0 + 0.44d0*zz)*(zfti-zfti4)...hcei = hcei + 4.95d0/(1.0d0 + 2.48d0*zz)*(zfti2 - zfti4 - 0.55d0*(zfti3-zfti4))...hcei = hcei - 1.2d0/(1.0d0 + 0.5d0*zz)*zfti4...hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)
!hcsa(nx)                                  : local real array : ?? - hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!a0(nx)                                    : local real array : ?? - a0 = -1.17d0*(1.0d0-zft)...a0 = a0/(1.0d0 - 0.22d0*zft - 0.19d0*zft*zft)...alp = (a0 + 0.25d0*(1.0d0 - zft**2)*sqrt(nuis))/(1.0d0+0.5d0*sqrt(nuis))
!alp(nx)                                   : local real array : ?? - alp = (a0 + 0.25d0*(1.0d0 - zft**2)*sqrt(nuis))/(1.0d0+0.5d0*sqrt(nuis))...alp = (alp + 0.315d0*a1)/(1.0d0+0.15*a1)...xcsa = betpl*(1.0d0 - betple/betpl)*(xcsa*alp)
!a1(nx)                                    : local real array : ?? - a1 = nuis**2 * zft**6...alp = (alp + 0.315d0*a1)/(1.0d0+0.15*a1)
!xcsa(nx)                                  : local real array : ?? - xcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zfte - 1.9d0/(zz+1.0d0)*zfte**2...xcsa = xcsa + (0.3d0*zfte**2 + 0.2d0*zfte**3)*zfte/(zz+1.0d0)...xcsa = betpl*(1.0d0 - betple/betpl)*(xcsa*alp)...xcsa = xcsa + (1.0d0-betple/betpl)*dcsa...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)

    real(kind(1.0d0)), dimension(nx) :: zz, zft, zdf, dcsa, hcee, hcei, hcsa, a0, alp, a1, xcsa

!zfte(nx)                                  : local real array : numerical algorithm?? - 
!zfte2(nx)                                 : local real array :
!zfte3(nx)                                 : local real array :
!zfte4(nx)                                 : local real array :

    real(kind(1.0d0)), dimension(nx) :: zfte, zfte2, zfte3, zfte4

!zfti(nx)                                  : local real array :
!zfti2(nx)                                 : local real array :
!zfti3(nx)                                 : local real array :
!zfti4(nx)                                 : local real array :

    real(kind(1.0d0)), dimension(nx) :: zfti, zfti2, zfti3, zfti4

    
    ! set physics constants
    
    real(kind(1.0d0)), parameter :: clight = 2.9979d+18, e_charge = 1.602d-19, eps_vacuum = 8.854d-12
    real(kind(1.0d0)), parameter :: m_electron = 9.109d-31, m_proton = 1.673d-27, mu_vacuum = 1.2566d-6
    real(kind(1.0d0)), parameter :: pe_mratio = 3438.0d0, planckh = 6.63d-34 , pi = 4.0d0*datan(1.0d0) !3.1415926535d0 !conflict with process

    real(kind(1.0d0)), dimension(nx) :: sqrtnuis,sqrtnues,sqrttiiamain

!--------------not here

  !ccfml
  !do jrad = 1, size(x)
  !   cc(jrad) = 1041d0/20d0 * tepr(jrad)**(1.5d0)
  !end do
  cc = 52.05d0 * tepr**(1.5d0)
  !cubsfml
  mux = 1.0d0/q!
  zmain = 1.0d0
  !amain = (1.0d0-fuelmix)*2.0d0 + fuelmix*3.0d0
  amain = (1.0d0-fuelmix)*3.0d0 + fuelmix*2.0d0
  hro = gradient1(rho)!
  ametr = xr!
  i = 1
  rtor = rmajor!
  nel = nepr!
  tel = tepr!
  tii = tipr!
  nii = nions!
  yd = -0.8d0 * pi**2 * rmajor!
  ya = 2.0d0/(hro**2 * yd)!
  sqeps = sqrt(ametr/(rtor+shif))
  dpsi = gradient1(psi)!
!--------------not here

  dlogte = gradient1(log(tepr))/2.0d0!
  dlogti = gradient1(log(tipr))/2.0d0!
  dlogne = gradient1(log(nepr))/2.0d0!
  betpl = 4.0d0*1.6d-4*pi*(nel*tel + nii*tii)*(rtor/(btor*rho*(mux+0.000001)))**2!
  betple = 4.0d0*1.6d-4*pi*(nel*tel)*(rtor/(btor*rho*(mux+0.000001)))**2!
  
  coulg = 15.9d0 - 0.5d0*log(nel) + log(tel)
  
  nuee = 670.0d0*coulg*nel/tel**1.5d0
  nues = nuee*1.4d0*zeff*rtor/(mux*sqeps**3 * sqrt(tel)*1.875d7)
  sqrttiiamain=sqrt(tii/amain)
  zavg = nel/nii
  nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**2.0d0 /sqrttiiamain)
  nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrttiiamain)
  sqrtnuis=sqrt(nuis)
  sqrtnues=sqrt(nues)
  
  tpf = 1.0d0 - (1.0d0-sqeps**2) * sqrt(1.0d0 - sqeps**2)/(1.0d0+1.46d0*sqeps)
  
  zz = zeff
  
  zft = tpf
  zdf = 1.0d0 + (1.0d0 - 0.1d0*zft) * sqrtnues
  zdf = zdf + 0.5d0*(1.0d0 - zft) * nues/zz
  zft = zft/zdf
  dcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zft - 1.9d0/(zz+1.0d0)*zft*zft
  dcsa = dcsa + (0.3d0*zft*zft + 0.2d0*zft*zft*zft) * zft/(zz+1.0d0)
  dcsa = dcsa * betpl!
  
  zft = tpf
  zdf = 1.0d0 + 0.26d0*(1.0d0-zft)*sqrtnues
  zdf = zdf + 0.18d0 * (1.0d0 - 0.37d0*zft)*nues/sqrt(zz)
  zfte = zft/zdf
  zfte2 = zfte*zfte
  zfte3 = zfte*zfte2
  zfte4 = zfte*zfte3
  
  zdf = 1.0d0 + (1.0d0+0.6d0*zft) * sqrtnues!
  zdf = zdf + 0.85d0*(1.0d0 - 0.37d0*zft)*nues*(1.0d0+zz)!
  zfti = zft/zdf!
  zfti2 = zfti*zfti!
  zfti3 = zfti*zfti2!
  zfti4 = zfti*zfti3!
  
  hcee = (0.05d0 + 0.62d0*zz)/zz/(1.0d0+0.44d0*zz)*(zfte-zfte4)
  hcee = hcee + (zfte2 - zfte4 - 1.2d0*(zfte3-zfte4))/(1.0d0+0.22*zz)
  hcee = hcee + 1.2d0/(1.0d0+0.5d0*zz)*zfte4!
  
  hcei = -1.0d0*(0.56d0 + 1.93d0*zz)/zz/(1.0d0 + 0.44d0*zz)*(zfti-zfti4)
  hcei = hcei + 4.95d0/(1.0d0 + 2.48d0*zz)*(zfti2 - zfti4 - 0.55d0*(zfti3-zfti4))
  hcei = hcei - 1.2d0/(1.0d0 + 0.5d0*zz)*zfti4!
  
  hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)!
  
  zdf = 1.0d0 + (1.0d0 - 0.1d0*zft)*sqrtnues!
  zdf = zdf + 0.5*(1.0d0 - 0.5d0*zft)*nues/zz
  zfte = zft/zdf!
  
  xcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zfte - 1.9d0/(zz+1.0d0)*zfte**2
  xcsa = xcsa + (0.3d0*zfte**2 + 0.2d0*zfte**3)*zfte/(zz+1.0d0)
  
  a0 = -1.17d0*(1.0d0-zft)
  a0 = a0/(1.0d0 - 0.22d0*zft - 0.19d0*zft*zft)!
  
  alp = (a0 + 0.25d0*(1.0d0 - zft**2)*sqrtnuis)/(1.0d0+0.5d0*sqrtnuis)
  a1 = nuis**2 * zft**6!
  alp = (alp + 0.315d0*a1)/(1.0d0+0.15*a1)!
  
  xcsa = betpl*(1.0d0 - betple/betpl)*(xcsa*alp)
  xcsa = xcsa + (1.0d0-betple/betpl)*dcsa!
  
  cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
  ! end of cubsfml
  

end subroutine cubsfml

subroutine ex_cubsfml(cc,cubb, &
  jrad,x,q,tepr,nx,fuelmix,rmajor,btor)

  use grad_func
        
!x(nx)                                     : local real array : radial linspace normalized to one
!tepr(nx)                                  : local real array : tepr=interp1([0.0d0, xtrt], [T_e0(1), T_e0], xr) interpolated in Te profile known rho
!tipr(nx)                                  : local real array : tipr=interp1([0.0d0, xtrt], [T_i0(1), T_i0], xr)
!nepr(nx)                                  : local real array : nepr=interp1([0.0d0, xtrt], [N_e0(1), N_e0], xr)
!xr(nx)                                    : local real array : xr = x*amin  --> rho

    real(kind(1.0d0)), dimension(nx) :: x, tepr, tipr, nepr, xr

!nions(nx)                                 : local real array : nions = ndeut + ntrit + nNe + nXe + nHe ion density
!zeff(nx)                                  : local real array : zeff = (1.0d0/nepr) * (ndeut+ntrit+4.0d0*nHe+zavne**2*nNe+zavxe**2*nXe)

    real(kind(1.0d0)), dimension(nx) :: nions, zeff!  conflict with process



    ! others

!btor                                      : local real : btor = 1.8d0 * rmajor**0.6d0...qedge = 12.0d0 * rminor**2 * btor / (rmajor * Ip)...betpl = 4.0d0*1.6d-4*pi*(nel*tel + nii*tii)*(rtor/(btor*rho*(mux+0.000001)))**2...betple = 4.0d0*1.6d-4*pi*(nel*tel)*(rtor/(btor*rho*(mux+0.000001)))**2
!rmajor                                    : local real : rmajor = 8.0d0...rminor = rmajor/aspect...Ip = (rmajor - 1.35d0)*3.0d0...btor = 1.8d0 * rmajor**0.6d0...qedge = ...gn_e(1:nxt) = ...gT_i0(1:nxt) = ...gyhat = ...tau_scal =...Vn = ...b(j,1)=...rtor = ...yd = ...chat = ...

    real(kind(1.0d0)) :: btor, rmajor!, conflict with process

!fuelmix                                   : local real : deuterium number fraction in hydrogen ions - fuelmix = 0.5d0...amain = 2*fuelmix+3*(1.-fuelmix) !D+T...amain = (1.0d0-fuelmix)*2.0d0 + fuelmix*3.0d0...ndeut = fuelmix*(nepr - nNe*zavne - nXe*zavxe - 2.0d0*nHe)...ntrit = (1.0d0-fuelmix)*(nepr - nNe*zavne - nXe*zavxe - 2.0d0*nHe)
!zmain                                     : local real : H's Z - zmain = 1.0d0 -- nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))
!amain                                     : local real : H's A - amain = 2*fuelmix+3*(1.-fuelmix) !D+T...nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))...nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrt(tii/amain))

    real(kind(1.0d0)) :: fuelmix, zmain, amain!, conflict with process

!rtor                                      : local real : rtor = rmajor...sqeps = ...betpl = ...betple = ...nues = ...nuis ...
!yd                                        : local real : ?? - yd = -0.8d0 * pi**2 * rmajor...ya = 2.0d0/(hro**2 * yd)

    real(kind(1.0d0)) :: rtor, yd

 
!shif(nx)                                  : local real array : shift, calculated in compute_equil, sqeps = sqrt(ametr/(rtor+shif))...call trmodel(...SHIF,...,0.*shif,0.*shif,0.*shif,...)
!q(nx)                                     : local real array : q, calculated in compute_equil, output variable - shear = gradient(log(q),log(x))...q_tr = interp1(x, q, xtr/xr(nx))...q0 = q...mux = 1.0d0/q
!rho(nx)                                   : local real array : rho, calculated in compute_equil - xtr = linspace(x0, xb, nxt) * amin  ! rho normalized for transport...hro = gradient1(rho)...betpl = ...betple = ...
!psi(nx)                                   : local real array : psi, calculated in compute_equil, dpsi = gradient1(psi)

    real(kind(1.0d0)), dimension(nx) :: shif, rho, psi, q!   conflict with process

!jrad                                      : local integer : do jrad = 1, size(x)

    integer :: jrad

!mux(nx)                                   : local real array : rotational transform - mux = 1.0d0/q...betpl =...betple =...nues =...nuis =...
!hro(nx)                                   : local real array : minor radius differential - hro = gradient1(rho) - ya = 2.0d0/(hro**2 * yd)
!ametr(nx)                                 : local real array : ?? - ametr = xr...sqeps = sqrt(ametr/(rtor+shif))
!!!!!!! used in cubsfml.inc
!nel(nx)                                   : local real array : nel = nepr...betpl = ...betple = ...coulg = ...nuee = ...zavg = ...
!tel(nx)                                   : local real array : tel = tepr...betpl = ...betple = ...coulg = ...nuee = ...nues = ...
!tii(nx)                                   : local real array : tii = tipr...betpl = ...nui = ...nuis = ...
!nii(nx)                                   : local real array : nii = nions...betpl = ...zavg = ...nui = ...
!ya(nx)                                    : local real array : ya = 2.0d0/(hro**2 * yd)...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)

    real(kind(1.0d0)), dimension(nx) :: mux, hro, ametr, nel, tel, tii, nii, ya

!sqeps(nx)                                 : local real array : sqeps = sqrt(ametr/(rtor+shif))...nues = ...nuis = ...tpf = ...
!dlogte(nx)                                : local real array : dlogte = gradient1(log(tepr))/2.0d0...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!dlogti(nx)                                : local real array : dlogti = gradient1(log(tipr))/2.0d0...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!dlogne(nx)                                : local real array : dlogne = gradient1(log(nepr))/2.0d0...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!dpsi(nx)                                  : local real array : dpsi = gradient1(psi)...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!betpl(nx)                                 : local real array : poloidal beta? - betpl = 4.0d0*1.6d-4*pi*(nel*tel + nii*tii)*(rtor/(btor*rho*(mux+0.000001)))**2...dcsa = ...hcsa = ...xcsa = ...
!betple(nx)                                : local real array : poloidal beta for electrons? - betple = 4.0d0*1.6d-4*pi*(nel*tel)*(rtor/(btor*rho*(mux+0.000001)))**2...hcsa = ...xcsa = ...

    real(kind(1.0d0)), dimension(nx) :: sqeps, dlogte, dlogti, dlogne, dpsi, betpl, betple

!nuee(nx)                                  : local real array : nu_ee?? - nuee = 670.0d0*coulg*nel/tel**1.5d0...nues = nuee*1.4d0*zeff*rtor/(mux*sqeps**3 * sqrt(tel)*1.875d7)
!nues(nx)                                  : local real array : nu_es?? - nues = nuee*1.4d0*zeff*rtor/(mux*sqeps**3 * sqrt(tel)*1.875d7)...zdf = ... (various occurrences)
!zavg(nx)                                  : local real array : average Z - zavg = nel/nii...nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))
!nui(nx)                                   : local real array : nu_i?? - nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))...nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrt(tii/amain))
!nuis(nx)                                  : local real array : nu_is?? - nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrt(tii/amain))...alp = ...a1 = ...
!coulg(nx)                                 : local real array : ?? - coulg = 15.9d0 - 0.5d0*log(nel) + log(tel)...nuee = 670.0d0*coulg*nel/tel**1.5d0
!cc(nx)                                    : local real array : ?? , calculated in compute_equil, also here- cc = 0.0*tepr+1.0d0...compute_equil...cc(jrad) = 1041d0/20d0 * tepr(jrad)**(1.5d0)
!tpf(nx)                                   : local real array : ?? - tpf = 1.0d0 - (1.0d0-sqeps**2) * sqrt(1.0d0 - sqeps**2)/(1.0d0+1.46d0*sqeps)...zft = tpf
!cubb(nx)                                  : local real array : ?? , calculated in compute_equil, also here- cubb =0.d0*nepr...compute_equil...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)

    real(kind(1.0d0)), dimension(nx) :: nuee, nues, zavg, nui, nuis, coulg, cc, tpf, cubb

!zz(nx)                                    : local real array : zz = zeff...zdf = ...dcsa = ...hcee = ...hcei = ...xcsa = ...
!zft(nx)                                   : local real array : ?? - zft = tpf...zdf = ...zft = zft/zdf...dcsa = ...zft = tpf...zdf = ...zfte = zft/zdf...zfti = ...a0 = ...alp = ...a1 = ...
!zdf(nx)                                   : local real array : ?? - zdf = ...(several occurrences)...zfte = zft/zdf...zfti = zft/zdf
!dcsa(nx)                                  : local real array : ?? - dcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zft - 1.9d0/(zz+1.0d0)*zft*zft...dcsa = dcsa + (0.3d0*zft*zft + 0.2d0*zft*zft*zft) * zft/(zz+1.0d0)...dcsa = dcsa * betpl...hcsa = ...xcsa = ...cubb = ...
!hcee(nx)                                  : local real array : ?? - hcee = (0.05d0 + 0.62d0*zz)/zz/(1.0d0+0.44d0*zz)*(zfte-zfte4)...hcee = hcee + (zfte2 - zfte4 - 1.2d0*(zfte3-zfte4))/(1.0d0+0.22*zz)...hcee = hcee + 1.2d0/(1.0d0+0.5d0*zz)*zfte4...hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)
!hcei(nx)                                  : local real array : ?? - hcei = -1.0d0*(0.56d0 + 1.93d0*zz)/zz/(1.0d0 + 0.44d0*zz)*(zfti-zfti4)...hcei = hcei + 4.95d0/(1.0d0 + 2.48d0*zz)*(zfti2 - zfti4 - 0.55d0*(zfti3-zfti4))...hcei = hcei - 1.2d0/(1.0d0 + 0.5d0*zz)*zfti4...hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)
!hcsa(nx)                                  : local real array : ?? - hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
!a0(nx)                                    : local real array : ?? - a0 = -1.17d0*(1.0d0-zft)...a0 = a0/(1.0d0 - 0.22d0*zft - 0.19d0*zft*zft)...alp = (a0 + 0.25d0*(1.0d0 - zft**2)*sqrt(nuis))/(1.0d0+0.5d0*sqrt(nuis))
!alp(nx)                                   : local real array : ?? - alp = (a0 + 0.25d0*(1.0d0 - zft**2)*sqrt(nuis))/(1.0d0+0.5d0*sqrt(nuis))...alp = (alp + 0.315d0*a1)/(1.0d0+0.15*a1)...xcsa = betpl*(1.0d0 - betple/betpl)*(xcsa*alp)
!a1(nx)                                    : local real array : ?? - a1 = nuis**2 * zft**6...alp = (alp + 0.315d0*a1)/(1.0d0+0.15*a1)
!xcsa(nx)                                  : local real array : ?? - xcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zfte - 1.9d0/(zz+1.0d0)*zfte**2...xcsa = xcsa + (0.3d0*zfte**2 + 0.2d0*zfte**3)*zfte/(zz+1.0d0)...xcsa = betpl*(1.0d0 - betple/betpl)*(xcsa*alp)...xcsa = xcsa + (1.0d0-betple/betpl)*dcsa...cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)

    real(kind(1.0d0)), dimension(nx) :: zz, zft, zdf, dcsa, hcee, hcei, hcsa, a0, alp, a1, xcsa

!zfte(nx)                                  : local real array : numerical algorithm?? - 
!zfte2(nx)                                 : local real array :
!zfte3(nx)                                 : local real array :
!zfte4(nx)                                 : local real array :

    real(kind(1.0d0)), dimension(nx) :: zfte, zfte2, zfte3, zfte4

!zfti(nx)                                  : local real array :
!zfti2(nx)                                 : local real array :
!zfti3(nx)                                 : local real array :
!zfti4(nx)                                 : local real array :

    real(kind(1.0d0)), dimension(nx) :: zfti, zfti2, zfti3, zfti4

    
    ! set physics constants
    
    real(kind(1.0d0)), parameter :: clight = 2.9979d+18, e_charge = 1.602d-19, eps_vacuum = 8.854d-12
    real(kind(1.0d0)), parameter :: m_electron = 9.109d-31, m_proton = 1.673d-27, mu_vacuum = 1.2566d-6
    real(kind(1.0d0)), parameter :: pe_mratio = 3438.0d0, planckh = 6.63d-34 , pi = 4.0d0*datan(1.0d0) !3.1415926535d0 !conflict with process

    real(kind(1.0d0)), dimension(nx) :: sqrtnuis,sqrtnues,sqrttiiamain

!--------------not here

  !ccfml
  !do jrad = 1, size(x)
  !   cc(jrad) = 1041d0/20d0 * tepr(jrad)**(1.5d0)
  !end do
  cc = 52.05d0 * tepr**(1.5d0)
  !cubsfml
  mux = 1.0d0/q!
  zmain = 1.0d0
  !amain = (1.0d0-fuelmix)*2.0d0 + fuelmix*3.0d0
  amain = (1.0d0-fuelmix)*3.0d0 + fuelmix*2.0d0
  hro = gradient1(rho)!
  ametr = xr!
  i = 1
  rtor = rmajor!
  nel = nepr!
  tel = tepr!
  tii = tipr!
  nii = nions!
  yd = -0.8d0 * pi**2 * rmajor!
  ya = 2.0d0/(hro**2 * yd)!
  sqeps = sqrt(ametr/(rtor+shif))
  dpsi = gradient1(psi)!
!--------------not here

  dlogte = gradient1(log(tepr))/2.0d0!
  dlogti = gradient1(log(tipr))/2.0d0!
  dlogne = gradient1(log(nepr))/2.0d0!
  betpl = 4.0d0*1.6d-4*pi*(nel*tel + nii*tii)*(rtor/(btor*rho*(mux+0.000001)))**2!
  betple = 4.0d0*1.6d-4*pi*(nel*tel)*(rtor/(btor*rho*(mux+0.000001)))**2!
  
  coulg = 15.9d0 - 0.5d0*log(nel) + log(tel)
  
  nuee = 670.0d0*coulg*nel/tel**1.5d0
  nues = nuee*1.4d0*zeff*rtor/(mux*sqeps**3 * sqrt(tel)*1.875d7)
  sqrttiiamain=sqrt(tii/amain)
  zavg = nel/nii
  nui = (zeff*zmain**2*zavg)*nii*322.0d0/(tii**2.0d0 /sqrttiiamain)
  nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrttiiamain)
  sqrtnuis=sqrt(nuis)
  sqrtnues=sqrt(nues)
  
  tpf = 1.0d0 - (1.0d0-sqeps**2) * sqrt(1.0d0 - sqeps**2)/(1.0d0+1.46d0*sqeps)
  
  zz = zeff
  
  zft = tpf
  zdf = 1.0d0 + (1.0d0 - 0.1d0*zft) * sqrtnues
  zdf = zdf + 0.5d0*(1.0d0 - zft) * nues/zz
  zft = zft/zdf
  dcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zft - 1.9d0/(zz+1.0d0)*zft*zft
  dcsa = dcsa + (0.3d0*zft*zft + 0.2d0*zft*zft*zft) * zft/(zz+1.0d0)
  dcsa = dcsa * betpl!
  
  zft = tpf
  zdf = 1.0d0 + 0.26d0*(1.0d0-zft)*sqrtnues
  zdf = zdf + 0.18d0 * (1.0d0 - 0.37d0*zft)*nues/sqrt(zz)
  zfte = zft/zdf
  zfte2 = zfte*zfte
  zfte3 = zfte*zfte2
  zfte4 = zfte*zfte3
  
  zdf = 1.0d0 + (1.0d0+0.6d0*zft) * sqrtnues!
  zdf = zdf + 0.85d0*(1.0d0 - 0.37d0*zft)*nues*(1.0d0+zz)!
  zfti = zft/zdf!
  zfti2 = zfti*zfti!
  zfti3 = zfti*zfti2!
  zfti4 = zfti*zfti3!
  
  hcee = (0.05d0 + 0.62d0*zz)/zz/(1.0d0+0.44d0*zz)*(zfte-zfte4)
  hcee = hcee + (zfte2 - zfte4 - 1.2d0*(zfte3-zfte4))/(1.0d0+0.22*zz)
  hcee = hcee + 1.2d0/(1.0d0+0.5d0*zz)*zfte4!
  
  hcei = -1.0d0*(0.56d0 + 1.93d0*zz)/zz/(1.0d0 + 0.44d0*zz)*(zfti-zfti4)
  hcei = hcei + 4.95d0/(1.0d0 + 2.48d0*zz)*(zfti2 - zfti4 - 0.55d0*(zfti3-zfti4))
  hcei = hcei - 1.2d0/(1.0d0 + 0.5d0*zz)*zfti4!
  
  hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)!
  
  zdf = 1.0d0 + (1.0d0 - 0.1d0*zft)*sqrtnues!
  zdf = zdf + 0.5*(1.0d0 - 0.5d0*zft)*nues/zz
  zfte = zft/zdf!
  
  xcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zfte - 1.9d0/(zz+1.0d0)*zfte**2
  xcsa = xcsa + (0.3d0*zfte**2 + 0.2d0*zfte**3)*zfte/(zz+1.0d0)
  
  a0 = -1.17d0*(1.0d0-zft)
  a0 = a0/(1.0d0 - 0.22d0*zft - 0.19d0*zft*zft)!
  
  alp = (a0 + 0.25d0*(1.0d0 - zft**2)*sqrtnuis)/(1.0d0+0.5d0*sqrtnuis)
  a1 = nuis**2 * zft**6!
  alp = (alp + 0.315d0*a1)/(1.0d0+0.15*a1)!
  
  xcsa = betpl*(1.0d0 - betple/betpl)*(xcsa*alp)
  xcsa = xcsa + (1.0d0-betple/betpl)*dcsa!
  
  cubb = ya*dpsi*(hcsa*dlogte+xcsa*dlogti+dcsa*dlogne)
  ! end of cubsfml
  


end subroutine ex_cubsfml


