!Bootstrap current calculation

              !cubsfml!!
              zz = zeff

              zft = tpf
              zdf = 1.0d0 + (1.0d0 - 0.1d0*zft) * sqrt(nues)
              zdf = zdf + 0.5d0*(1.0d0 - zft) * nues/zz
              zft = zft/zdf
              dcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zft - 1.9d0/(zz+1.0d0)*zft*zft
              dcsa = dcsa + (0.3d0*zft*zft + 0.2d0*zft*zft*zft) * zft/(zz+1.0d0)
              dcsa = dcsa * betpl
!write(*,*) 'zef',zeff,tpf,betpl,dcsa
              zft = tpf
              zdf = 1.0d0 + 0.26d0*(1.0d0-zft)*sqrt(nues)
              zdf = zdf + 0.18d0 * (1.0d0 - 0.37d0*zft)*nues/sqrt(zz)
              zfte = zft/zdf
              zfte2 = zfte*zfte
              zfte3 = zfte*zfte2
              zfte4 = zfte*zfte3

              zdf = 1.0d0 + (1.0d0+0.6d0*zft) * sqrt(nues)
              zdf = zdf + 0.85d0*(1.0d0 - 0.37d0*zft)*nues*(1.0d0+zz)
              zfti = zft/zdf
              zfti2 = zfti*zfti
              zfti3 = zfti*zfti2
              zfti4 = zfti*zfti3

              hcee = (0.05d0 + 0.62d0*zz)/zz/(1.0d0+0.44d0*zz)*(zfte-zfte4)
              hcee = hcee + (zfte2 - zfte4 - 1.2d0*(zfte3-zfte4))/(1.0d0+0.22*zz)
              hcee = hcee + 1.2d0/(1.0d0+0.5d0*zz)*zfte4

              hcei = -1.0d0*(0.56d0 + 1.93d0*zz)/zz/(1.0d0 + 0.44d0*zz)*(zfti-zfti4)
              hcei = hcei + 4.95d0/(1.0d0 + 2.48d0*zz)*(zfti2 - zfti4 - 0.55d0*(zfti3-zfti4))
              hcei = hcei - 1.2d0/(1.0d0 + 0.5d0*zz)*zfti4

              hcsa = (betple*(hcee + hcei) + dcsa/betpl*betple)
!write(*,*) hcsa

              zdf = 1.0d0 + (1.0d0 - 0.1d0*zft)*sqrt(nues)
              zdf = zdf + 0.5*(1.0d0 - 0.5d0*zft)*nues/zz
              zfte = zft/zdf

              xcsa = (1.0d0 + 1.4d0/(zz+1.0d0))*zfte - 1.9d0/(zz+1.0d0)*zfte**2
              xcsa = xcsa + (0.3d0*zfte**2 + 0.2d0*zfte**3)*zfte/(zz+1.0d0)

              a0 = -1.17d0*(1.0d0-zft)
              a0 = a0/(1.0d0 - 0.22d0*zft - 0.19d0*zft*zft)

              alp = (a0 + 0.25d0*(1.0d0 - zft**2)*sqrt(nuis))/(1.0d0+0.5d0*sqrt(nuis))
              a1 = nuis**2 * zft**6
              alp = (alp + 0.315d0*a1)/(1.0d0+0.15*a1)

              xcsa = betpl*(1.0d0 - betple/betpl)*(xcsa*alp)
              xcsa = xcsa + (1.0d0-betple/betpl)*dcsa

              ya = 2.0d0/(yd)
              dlogte = gradient1(log(tepr))/hro
              dlogti = gradient1(log(tipr))/hro
              dlogne = gradient1(log(nepr))/hro
              dpsi = gradient1(psi)/hro

               cubb=0.5d0*(dcsa*dlogne+hcsa*dlogte+xcsa*dlogti)
 cubb=-btor/(0.2d0*pi*rtor)*rho*mux*cubb !cubb is Jbs in MA/m^2

              ! end of cubsfml





