
subroutine compute_equil( &
  !input 
  nx, jiter,i_equiltype, &
  x, te, ti, ne, ni, palph, cc, G30,qinit,G20,vp0, &
  R,rmin,elon,tria,Ip,btor,betaz,lint,ipol0,e_charge,mu_vacuum, &
  !inout
  qedge, &
  !output
  roc, Vloop, fbs,fcd, toleq, &
  k, d, shif, cubb, jcdr, V, G1, G2, G3, dV, phi, q, rho, psi, jpar,&
  ipol, Vprime,droda,eqpf,eqff,gradro,q_edge_in,f_ind_in,q_95,elong95,triang95 &
  ,pres_fac)
  
  use grad_func
  implicit none


  
  !  Arguments
  
  integer, intent(inout) :: nx, jiter,i_equiltype
  integer :: j
  real(kind(1.0d0)), dimension(nx), intent(in) :: x, te, ti, ne, ni, palph, cc, G30,qinit,G20,vp0
  real(kind(1.0D0)), intent(in) :: q_edge_in,f_ind_in,R,rmin,btor,betaz,lint,ipol0,e_charge,mu_vacuum
  
  real(kind(1.0d0)), intent(inout) :: pres_fac,qedge,ip,q_95,elon,tria,elong95,triang95
  
  real(kind(1.0D0)), intent(inout) :: roc, Vloop, fbs,fcd, toleq
  real(kind(1.0d0)), dimension(nx), intent(inout) :: k, d, shif, cubb, jcdr, V, G1, G2, G3, dV, phi, q, rho, psi, jpar
  real(kind(1.0d0)), dimension(nx), intent(inout) :: ipol, Vprime,droda,eqpf,eqff,gradro
  
  !  Local variables
! Other local variables of interest
!rhoint                : local real : rhoint = x * rmin !minor radius, linspace ---
!smallk                : local real : smallk = (elon-1.0d0) * x**2 - se usa para calcular la k en x ---
!diagz                 : local integer : flag para activar el output, se usa al final
!na                    : local integer : na = nx-1 -- no se usa!!
!gp2                   : local real : gp2 = 2.*pi
!gpp4                  : local real : gpp4 = gp2*gp2
!yro                   : local real : 
!hro                   : local real : 
!yda                   : local real : 
!TIME                  : local real :   
  real(kind(1.0D0)), dimension(nx) :: smallk, dvdr, rhoint, f, jpol, kerncur, pressure, A, B, C, bbb, ccc, dum1, dum2, dum3
  real(kind(1.0D0)), dimension(nx) :: chat,betahat,y, FF, fp, kpk, dpk,  pprime,ffprime,ba,bb
  real(kind(1.0D0)) :: dpsidt, Epar, Ibs, fb, yb, C1
  real(kind(1.0d0)), parameter :: pi = 4.0d0*datan(1.0d0),ACEQLB=1.0d-6 !pi = 3.1415926536d0,ACEQLB=1.e-6
  real(kind(1.0d0)), dimension(nx) :: gr,GBD,GL,GSD, & 
  &  BD,BC,B2B0EQ,B0B2EQ,BMAXEQ,BMINEQ,BMODEQ,FOFBEQ,GRDAEQ, &
  &  btooo,rooo,g11,g22,g33,slat,vr
		real(kind(1.0d0)), dimension(nx) ::  &
  & bdb0,bmint,bmaxt, b0db2,ya,ybb,BDB02,FOFB,qg3s,q0,vvvv
  real(kind(1.0d0)), dimension(nx) :: gra,sqgra,grar,avr2,ai0,dgrda,avsqg !gradient,sqrgradient,radial gradient,
  
  integer :: na,diagz,nxtemp
  real(kind(1.0d0)) :: gpp4,gp2,yro,hro,yda,TIME,alfa
! Quiet NAN, double precision.
REAL(8), PARAMETER :: D_QNAN = &
TRANSFER((/ Z'00000000', Z'7FF80000' /),1.0_8)  
  diagz=1
  
	pres_fac=1.d0
  na = nx-1
  gp2 = 2.*pi
  gpp4 = gp2*gp2
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  cubb(1) = 0.0d0
  rhoint = x * rmin !minor radius
  
  
!  write(*,*) 'jiter',jiter
!  pres_fac=1.d0
  if (jiter .eq. 0) then 

     !geometry, initial guess  151116
     call INITEQUIL( &
          nx,x,elon,tria,R,rmin, &
          k,d,shif,V,G1,G2,G3&
          )
     
  else    ! next iterations
     
     
     !	write(*,*) pressure,te,ne,ti,ni,palph
     !	write(*,*) '*',ne,'('



     !precalculations
111	continue
     pressure = 1.d3 * e_charge * 1.d19 * (te*ne + ti*ni + palph)*pres_fac ! in J/m^3
     pprime = derivcc(nx,psi,pressure,2)

     q0 = qinit

     qg3s = q0/G30

     A=G20/qg3s**2.+(gpp4**2.)*G30

     C=-gpp4*mu_vacuum*derivcc(nx,x,pressure,1)/A

     dum3=G20/qg3s
     dum2=derivcc(nx,x,dum3,1)

     B = -dum2/qg3s/A	

     fb = R*btor/gp2
     yb = 0.5*fb**2.d0
     

     dum2 = integrcc(nx,x,B)
     dum1=exp(2.d0*dum2)
					dum1 = dum1/dum1(nx)


     dum3 = integrcc(nx,x,C/dum1)
     dum3=dum3-dum3(nx)
     C1 = yb 

     y = dum1*(dum3+C1)

						dum2=G20/qg3s
	dum3=derivcc(nx,psi,dum2,2)						
	betahat=dum3/qg3s/A
	Chat=-gpp4*mu_vacuum*pprime/A

     FF = sqrt(2.*y)
	
					ffprime=gpp4*(chat-betahat*FF**2.d0)     

!	write(1551,*) ' ' 
!	write(*,*) 'A ',A,' B',B,'C ',C,'y ', & 
!	& ' ',y,'F',FF,' ','pp ',pprime,'ff ',ffprime

!	write(1552,'(911E25.11)') pressure,psi,A,B,C,y,FF,pprime,ffprime

     !EMEQ!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     if (i_equiltype.eq.1.or.i_equiltype.eq.2) then
        
        !        call EMEQ 
        !     &	(BA,BB		! j_zeta = BA*(R00/r) + BB*(r/R00-R00/r)
        !     &	,RTOR+SHIFT	! R00 = R_0+\Delta_edge
        !     &	,ABC		! a_edge
        !     &	,ELONG		! \lambda_edge
        !     &	,TRIABC		! \delta_edge
        !     &	,N3EQL		! radial grid point No.
        !     &	,ACEQLB		! relative accuracy
        !     &	,BTOR*RTOR/(RTOR+SHIFT)		! B_tor_vac at R00
        !     &	,IPL		! Total plasma current
        !     .	,GR,GBD,GL,GSD,A,BD,B,BA,BB,BC,C,D
        !     .  ,B2B0EQ,B0B2EQ,BMAXEQ,BMINEQ,BMODEQ,FOFBEQ,GRDAEQ
        !     .	,TIME)
        
        
	BB = -2.*pi*R*pprime*1.e-6 !Zakharov (17)
	BA = -2.*pi/mu_vacuum/R*ffprime*1.e-6+BB !Zakharov (16)

	eqpf=BB
	eqff=-2.*pi/mu_vacuum/R*ffprime*1.e-6


!	write(*,*) eqpf,eqff

!write(*,*) 'g',eqpf(nx-4:nx),eqff(nx-4:nx)

        
        !	do j=1,nx
        !		write(88,'(7E25.11)') x(j),pprime(j),ffprime(j),BA(j),BB(j),FF(j),pressure(j)
        !	enddo
        
	TIME=0.
        
!	pause
!	write(*,*) 'data2m',r,elon,tria,nx,btor,shif(nx),ip

!     .	,GR,GBD,GL,GSD,A,BD,B,BA,BB,BC,C,D
!     .  ,B2B0EQ,B0B2EQ,BMAXEQ,BMINEQ,BMODEQ,FOFBEQ,GRDAEQ
nxtemp=nx
	call EMEQ(BA,BB,R+SHIF(nx),rmin,ELON,TRIA*rmin,nx, &		! radial grid point No.
             &	 ACEQLB,BTOR*R/(R+SHIF(nx)),IP,GR,GBD,GL,GSD,gra, & 
        &  sqgra,grar,avr2,ai0,dgrda,avsqg,vvvv,B2B0EQ,B0B2EQ,BMAXEQ,BMINEQ,BMODEQ,FOFBEQ,GRDAEQ, &
        &  TIME)
!	write(888,*) BA,BB,R+SHIF(nx),rmin,ELON,TRIA,nx, &		! radial grid point !No.
!             &	 ACEQLB,BTOR*R/(R+SHIF(nx)),IP,GR,GBD,GL,GSD,gra, & 
!        &  sqgra,grar,avr2,ai0,dgrda,avsqg,v,B2B0EQ,B0B2EQ,BMAXEQ,BMINEQ,BMODEQ,FOFBEQ,GRDAEQ, &
!        &  TIME
        
        

	if (nx.lt.1) then
	nx=nxtemp
	pres_fac=pres_fac*0.9
!	write(*,*) 'not converged pressure too high',pres_fac
!stop
!		qedge=d_qnan
		goto 111
	endif


	if (isnan(sqgra(2))) then
	pres_fac=pres_fac*0.9
!	write(*,*) 'not converged pressure too high'
!	stop
	goto 111
	endif


	BTOOO=BTOR*R/(R+SHIF(nx)) ! toroidal field at plasma axis
	ROOO=(R+SHIF(nx))! plasma axis
        
        
        
        ! Define a new RHO-grid:
	ALFA = .00001
	call SMAP(ALFA,nx,x,nx,x,gr)
	call SMAP(ALFA,nx,x,nx,x,gbd)
	call SMAP(ALFA,nx,x,nx,x,gl)
	call SMAP(ALFA,nx,x,nx,x,gsd)
	call SMAP(ALFA,nx,x,nx,x,gra)
	call SMAP(ALFA,nx,x,nx,x,sqgra)
	call SMAP(ALFA,nx,x,nx,x,grar)
	call SMAP(ALFA,nx,x,nx,x,avr2)
	call SMAP(ALFA,nx,x,nx,x,ai0)
	call SMAP(ALFA,nx,x,nx,x,dgrda)
	call SMAP(ALFA,nx,x,nx,x,avsqg)
	call SMAP(ALFA,nx,x,nx,x,vvvv)
	call SMAP(ALFA,nx,x,nx,x,ai0)
	call SMAP(ALFA,nx,x,nx,x,gr)
	IPOL=ai0


	ROC = GR(nx)		! Define a new RHO_edge
	rho = GR !rho toroidal
	phi=btor*3.141592*rho**2.d0



 !----------------------------------------------------------------------|
 ! Map quantities attributed to the auxiliary grid:
 ! A -> <[nabla(a)]^2>		B -> <[nabla(a)/r]^2>
 ! C -> dV/d(a)			BD -> <|nabla(a)|>
 ! BA -> G33			BB -> IPOL
 ! BC -> DRODA	!!  d(rho_A)/da=sqrt(RTOR/(RTOR+SHIFT))*d(rho_Z)/da
	GPP4 = GP2*GP2
        
	hro = (x(2)-x(1))*rmin !minor radius differential
	
!	do	J=1,nx
!	   DRODA(J) = sqgra(J)
!	   G1(J) = gra(J)
!	   G3(J) = avr2(J)
!	   VR(J) = gpp4*avsqg(J)
!	   G2(J) = grar(J)*VR(j)**2.
!	   GRADRO(J) = dgrda(J)
!	   Vprime(J) = VR(j)
!	   YA(j) = G3(j)
!	   YBb(j) = IPOL(j)
  !         shif(j)=gbd(j)
 !          k(j)=gl(j)
!	enddo



	   DRODA = dgrda
	   G1 = gra
	   G3 = avr2
	   VR = gpp4*avsqg
	   G2 = grar*VR**2.
	   GRADRO = sqgra
	   Vprime = VR
	   YA = G3
	   YBb = IPOL
           shif=gbd
           k=gl
	v=vvvv

!	write(*,*) shif(1),shif(nx)
!	write(*,*) v(nx),trapz(vr)*hro

!	write(*,*) 'p',vprime(1:10)
!	write(*,*) 'p',vr(nx-4:nx)


 ! Multiply above quantities by linear in rho factors (i.e. f(0)=0)
!	do	J=1,nx
!	   SLAT(J) = GRADRO(J)*VR(J)
!	enddo

        SLAT = GRADRO*VR
        
        ! GL -> a
        ! GSD -> \delta^{ASTRA} (dimensionless)
	YDA = rmin/(nx-1.)
	do	j =1,nx
	   A(j) = YDA*(J-1.) !ametr
	   if (j.ne.1) B(J) = GSD(J)/A(J)
	enddo
	B(1)=0. !tria
	d=B
        
        !MR additional quantities
        ! Note!	   BDB02(j) = G33(j)*IPOL(j)**2+(RHO(j)*MU(j)/RTOR)**2
        !   BDB02 - <B**2/B0**2>
        !   B0DB2 - <B0**2/B**2>
        !   BMAXT - BMAXT
        !   BMINT - BMINT
        !   BDB0  - <B/BTOR>
        !   FOFB  - <(BTOR/B)**2*(1.-SQRT(1-B/Bmax)*(1+.5B/Bmax))>
        
        BDB02=B2B0EQ
        B0DB2=B0B2EQ
        BMAXT=BMAXEQ
        BMINT= BMINEQ
        BDB0 =BMODEQ
        FOFB = FOFBEQ
	BDB02=BDB02*BTOOO**2./BTOR**2.
	BDB0=BDB0*BTOOO/BTOR
	B0DB2=B0DB2/BTOOO**2.*BTOR**2.
 !not used!!!
	
        
!	endif
     endif
     !EMEQ!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     
     
     
     
     
     !...!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !...!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     
  end if
  !end next iterations
  
  
  !valid for all:

	if (isnan(sqgra(2))) then
	
		else
  call ADDITIONAL_CALCS( &
       i_equiltype,jiter,nx,V,btor,Ip,R,rmin,x,cc,cubb, jcdr, FF,G2,G3,q0,mu_vacuum, &
       qedge, &
       dV,phi,rho,roc,ipol,jpol,kerncur,Ibs,Epar,jpar,Vloop,Vprime,q,psi,toleq, dum1, fbs,fcd &
       ,q_edge_in,q_95,elon,tria,elong95,triang95,k,d)
	endif

  ! output
  
  


	return
end subroutine compute_equil



! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+ad_name INITEQUIL
!+ad_summ "compute initial guess for geometry"
! definitions are initial guesses  041116, --- means confirmed
!
!+ad_args nx                    : input integer : dimension of radial vector
!+ad_args x(nx)                 : input real array : radial position in poloidal section ---
!+ad_args R                     : input real : major radius
!+ad_args rmin                  : input real : minor radius 
!+ad_args elon                  : input real : elongation k
!+ad_args tria                  : input real : triangularity d
!
!+ad_args k(nx)                 : output real array : kappa at each flux surface - k = smallk + 1.0d0 - effective k at position x --
!+ad_args d(nx)                 : output real array : delta at each flux surface - d = tria * x**2 - effective triangularity at x --
!+ad_args shif(nx)              : output real array : shift at each flux surface - shif = 0.0d0 * d - effective shift at x --
!+ad_args V(nx)                 : output real array : cumint1(dvdr * gradient1(rhoint)) - toroidal volume at rhoint --
!+ad_args G1(nx)                : output real array : abs(grad(V)^2) (fable) ---
!+ad_args G2(nx)                : output real array : (g1/R^2)_flux surface (fable) ---
!+ad_args G3(nx)                : output real array : (1/R^2)_flux surface (fable) ---
!
!+ad_desc Other local variables of interest
!+ad_args rhoint                : local real : rhoint = x * rmin !minor radius, linspace ---
!+ad_args smallk                : local real : smallk = (elon-1.0d0) * x**2 - se usa para calcular la k en x ---
!+ad_args dvdr                  : local real : 
!+ad_args pi                    : local real : parameter 
!+ad_desc=======================================================================
!+ad_desc======================================================================|
!+ad_prob None
!+ad_call None
!+ad_hist 15/11/16  programmed the initial version as a refactoring of previously existent code
!+ad_stat Okay
!+ad_docs None


subroutine INITEQUIL( &
     nx,x,elon,tria,R,rmin, &
     k,d,shif,V,G1,G2,G3&
     )

  use grad_func
  implicit none
  ! first iteration
  
  ! initial guess, parabolic elongation profile
  !input
  integer, intent(in) :: nx
  real(kind(1.0d0)), intent(in) :: elon,tria,R,rmin
  real(kind(1.0d0)), dimension(nx), intent(in) :: x
  !output
  real(kind(1.0D0)), dimension(nx), intent(out) :: k,d,shif,V,G1,G2,G3
  !local
  real(kind(1.0D0)), dimension(nx) :: smallk,dvdr,rhoint
  real(kind(1.0d0)), parameter :: pi = 3.141592 !3.1415926535d0



  smallk = (elon-1.0d0) * x**2
  k = smallk + 1.0d0
  d = tria * x**2
  shif = 0.0d0 * d*(1.-x)

  rhoint = x * rmin !minor radius
  !volume
  dvdr = 4.0d0 * pi**2 * rhoint * R * (1.0d0 + k + rhoint * gradient(k, rhoint)/2.0d0 + shif/R &
  - 3.0/8.0*rhoint*d/R + 1.0/2.0*rhoint*gradient(shif,rhoint)/R - 1.0/8.0*rhoint**2 * gradient(d,rhoint)/R)
  V = cumint1(dvdr * gradient1(rhoint))

  !metric characteristics
  G1 = 1.0+(-k-rhoint*gradient(k, rhoint)) + 1./(16.*R) * &
  (R*rhoint**2*gradient(d, rhoint)**2 + 14.*R*rhoint**2*gradient(k,rhoint)**2 &
  - 4.0*rhoint*R*gradient(shif, rhoint)*gradient(d, rhoint) + 2.&
  *R*rhoint*d*gradient(d,rhoint) + 36.*k*R*rhoint*gradient(k,rhoint) &
  + 8.*R*gradient(shif,rhoint)**2 - 4.*R*d*gradient(shif,rhoint) + 5.&
  *R*d**2 + 4.*rhoint**2*gradient(d,rhoint) &
  + 24.*R*k**2 - 16.*rhoint*gradient(shif,rhoint) + 4.*rhoint*d)
  
  G2 = 16.*pi**4*rhoint**2 * (1.+k+1./(16.*R**2)* (R**2 * rhoint**2 *&
  gradient(d, rhoint)**2 + 2.*R**2 * rhoint**2 * gradient(k, rhoint)**2 &
  - 4.0*rhoint*R**2*gradient(shif, rhoint)*gradient(d, rhoint) + 2.&
  *R**2*rhoint*d*gradient(d,rhoint) + 4.*k*R**2*rhoint*gradient(k,rhoint) &
  + 8.*R**2*gradient(shif,rhoint)**2 - 4.*R**2*d*gradient(shif,rhoint) + 5.&
  *R**2*d**2 - 4.*rhoint**2*R*gradient(d,rhoint) &
  + 8.*R**2*k**2 + 16.*rhoint*R*gradient(shif,rhoint) - 4.*R*rhoint*d + 8.*rhoint**2))
  
  G3=1./R**2-1./(4.*R**4)*(-2.*rhoint**2 + 8.*shif*R-3.*d*R*rhoint + 4.&
  *gradient(shif,rhoint)*R*rhoint-rhoint**2*R*gradient(d,rhoint))

!write(*,*) 'volum',elon,tria,R,rmin,v(nx)

	return
end subroutine INITEQUIL



! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!+ad_name ADDITIONAL_CALCS
!+ad_summ "compute results from MHD equilibrium calculations"
! definitions are initial guesses  041116, --- means confirmed
!
!+ad_args nx                    : input integer : dimension of radial vector
!+ad_args jiter                 : input integer : iteration number
!+ad_args R                     : input real : major radius
!+ad_args rmin                  : input real : minor radius 
!+ad_args Ip                    : input real : poloidal current
!+ad_args btor                  : input real : toroidal magnetic field
!+ad_args mu_vacuum             : input real : vacuum permitivity (constant)
!+ad_args V(nx)                 : input real array : cumint1(dvdr * gradient1(rhoint)) - toroidal volume at rhoint --
!+ad_args x(nx)                 : input real array : radial position in poloidal section ---
!+ad_args cc(nx)                : input real array : cc = 0.0*tepr+1.0d0 - algun tipo de corriente, es el jpar cuando jiter no es inicial
!+ad_args cubb(nx)              : input real array : cubb =0.d0*nepr -- esta desactivado, es un vector nulo todo el tiempo
!+ad_args FF(nx)                : input real array : 
!+ad_args G2(nx)                : input real array : (g1/R^2)_flux surface (fable) ---
!+ad_args G3(nx)                : input real array : (1/R^2)_flux surface (fable) ---
!+ad_args q0(nx)                : input real array :

!+ad_args qedge                 : input output real : quality factor at edge

!+ad_args dV(nx)                : output real array : volume differential at each flux surface (?)
!+ad_args phi(nx)               : output real array : poloidal angle coordinate at each flux surface (?)
!+ad_args rho(nx)               : output real array : rho=sqrt(phi/(pi*btor)) !rho toroidal ---
!+ad_args ipol(nx)              : output real array : poloidal current density at each flux surface (?)
!+ad_args jpol(nx)              : output real array : *
!+ad_args kerncur(nx)           : output real array : 
!+ad_args jpar(nx)              : output real array : parallel current density at each flux surface (?)
!+ad_args Vprime(nx)            : output real array : radial derivative of toroidal volume at radial position (fable) ---
!+ad_args q(nx)                 : output real array : quality factor at each flux surface
!+ad_args psi(nx)               : output real array : toroidal angle coordinate at each flux surface (?)
!+ad_args dum1(nx)              : output real array : shift at each flux surface - shif = 0.0d0 * d - effective shift at x --
!+ad_args roc                   : output real : 
!+ad_args Vloop                 : output real : loop voltage
!+ad_args toleq                 : output real : 
!+ad_args Ibs                   : output real : 
!+ad_args Epar                  : output real : 
!
!+ad_desc Other local variables of interest
!+ad_args rhoint(nx)            : local real array : rhoint = x * rmin !minor radius, linspace ---
!+ad_args f(nx)                 : local real array :
!+ad_args dvdr(nx)              : local real array : 
!+ad_args pi                    : local real : parameter 
!+ad_args gp2                   : local real :  
!+ad_args dpsidt                : local real :  
!+ad_args j                     : local integer :  
!+ad_desc=======================================================================
!+ad_desc======================================================================|
!+ad_prob None
!+ad_call None
!+ad_hist 15/11/16  programmed the initial version as a refactoring of previously existent code
!+ad_stat Okay
!+ad_docs None


subroutine ADDITIONAL_CALCS( &
     i_equiltype,jiter,nx,V,btor,Ip,R,rmin,x,cc,cubb,jcdr,FF,G2,G3,q0,mu_vacuum, &
     qedge, &
     dV,phi,rho,roc,ipol,jpol,kerncur,Ibs,Epar,jpar,Vloop,Vprime,q,psi,toleq, dum1, fbs,fcd &
     ,q_edge_in,q_95,elon,tria,elong95,triang95,k,d)

  use grad_func
  implicit none	
  !input
  integer, intent(in) :: nx,jiter,i_equiltype
  real(kind(1.0d0)), intent(in) :: R,rmin,btor,mu_vacuum,q_edge_in
  real(kind(1.0d0)), intent(inout) :: ip,q_95
  real(kind(1.0d0)), dimension(nx), intent(in) :: V,x,cc,cubb,jcdr,FF,G2,G3,q0
  !inout
  real(kind(1.0d0)), intent(inout) :: qedge,elon,tria,elong95,triang95
  real(kind(1.0d0)), intent(inout),dimension(nx) :: Vprime,k,d
  !output
  real(kind(1.0D0)), dimension(nx), intent(inout) :: dV,phi,rho,ipol,jpol,kerncur,jpar,q,psi, dum1 
  real(kind(1.0D0)), intent(inout) :: roc,Vloop,toleq,Ibs,Epar,fbs,fcd
  !local
  real(kind(1.0D0)), dimension(nx) :: dvdr,rhoint,f,dum2
  real(kind(1.0D0)) :: icd
  real(kind(1.0d0)), parameter :: pi = 4.0d0*datan(1.0d0) !3.1415926535d0 !, mu_vacuum = 1.2566d-6
  real(kind(1.0D0)) :: dpsidt,gp2,qcyl,alfa,kcyl,dcyl
  integer :: j,DEBUG_FLAG,j_9
  gp2 = 2.*pi
  rhoint = x * rmin !minor radius


	qcyl=rmin**2.d0*btor/R/ip/0.091

  if (jiter .ne. 0) then 
     !additional calcs	
     dV=vprime*(x(2)-x(1))*rmin
	!write(*,*) roc
!     ipol = gp2*FF
     jpol = ipol/(R*btor)
!     phi = cumint1(ipol*G3/(2.*pi)*dV)
!     rho = sqrt(phi/(pi*btor))
!     roc = rho(nx)
!write(*,*) ipol,phi,rho,roc,dv
!pause

     kerncur = cc/ipol**2.*dV
     Ibs = sum(cubb/ipol**2.*dV)*ipol(nx)*btor/(2.*pi)
     Icd = sum(jcdr/ipol**2.*dV)*ipol(nx)*btor/(2.*pi)
     Epar = (Ip - Ibs-Icd)/(ipol(nx)*btor/(2.*pi)*trapz(kerncur))
     
     jpar = cc*Epar + cubb+jcdr
     Vloop = Epar * 2. * pi * btor/(ipol(nx) * G3(nx))
     
!     Vprime = gradient(V, rho)
     kerncur = jpar/ipol**2.*dV
     
     dum1=ipol*btor/gp2*cumint1(dV*jpar/ipol**2.)
     q=ipol*g2*g3/(mu_vacuum*8*pi**3.*dum1*1.e6)

!	write(*,*) 'j9',j_9
!write(*,*) 	'q',q
!pause
	j_9=1
	do j=1,nx
	if (q(j).le.1.d0) then
		j_9=j
	endif
	enddo

	if (j_9.gt.1) then
	    q(1:j_9) = 1.d0
	goto 10
	endif

	j_9=1
	do j=1,nx
	if (q(nx-j+1).le.q(1)) then
		j_9=nx-j+1
		goto 11
	endif
	enddo
11	continue
	q(1)=sum(q(1:j_9))/(j_9-1.d0)
	q(1:j_9)=q(1)+(q(j_9)-q(1))*v(1:j_9)/v(j_9)	

10	continue



	ALFA = .00001
	call SMAP(ALFA,nx,x,nx,x,q)




     qedge=q(nx)
     
     psi = integrcc(nx,phi,1.d0/q)
!	write(*,*) q,phi,psi
     
     toleq = maxval(abs(q-q0)/q0)

	dum2=(psi-psi(1))/(psi(nx)-psi(1))	     
	j_9=1
	do j=1,nx
	if (dum2(j).le.0.95) j_9=j
	enddo
	qcyl=(q(j_9+1)-q(j_9))/(dum2(j_9+1)-dum2(j_9))*(0.95-dum2(j_9))+q(j_9)
	kcyl=(k(j_9+1)-k(j_9))/(dum2(j_9+1)-dum2(j_9))*(0.95-dum2(j_9))+k(j_9)
	dcyl=(d(j_9+1)-d(j_9))/(dum2(j_9+1)-dum2(j_9))*(0.95-dum2(j_9))+d(j_9)

	if (i_equiltype.eq.1) ip=ip*qcyl/q_95
	if (i_equiltype.eq.2) q_95=qcyl

	k=k/kcyl*elong95
	d=d/dcyl*triang95
	elon=k(nx)
	tria=d(nx)


!	write(*,*) 'equil' ,j_9,x(j_9),q(j_9),q(j_9+1),dum2(j_9+1),dum2(j_9)
	
!	open(32,file='qprof.out')
!	write(32,'(911E25.11)') q
!	close(32)

  else

     !volume gradient
     dV=gradient1(V)
     !toroidal flux, internal variable
     phi=btor*V/(2.*pi*R)
     !quality factor at edge, internal variable
     q=1.+(qedge-1.)*x**4.
     !toroidal rho, internal variable
     rho=sqrt(phi/(pi*btor)) !rho toroidal
     roc=rho(nx)
     !poloidal flux, internal variable		
     psi=cumint1(gradient1(phi)/q)
     !f function, internal variable
     f=gradient(psi,rhoint)
     !poloidal current, internal variable
     ipol=R*btor*(1.+0.*x)
     !poloidal current density, internal variable
     jpol=ipol/(R*btor)
     !??, internal variable
     kerncur=dV*cc/ipol**2
     !=0, internal variable
     Ibs=sum(dV*cubb/ipol**2)*ipol(nx)*btor/(2.*pi)
     Icd=sum(dV*jcdr/ipol**2)*ipol(nx)*btor/(2.*pi)
     !=0, internal variable
     dpsidt=0.
     !=0, internal variable
     Epar=dpsidt
     !=0, internal variable
     jpar=cc*Epar+cubb+jcdr
     !loop voltage, internal variable
     Vloop=Epar*2.*pi*btor/(ipol(nx)/G3(nx))
     !derivada del volumen, internal variable
     Vprime=gradient(V,rhoint)
     !toleq, lo calcula al principio y al final, el valor inicial para la iteracion cero (initial guess), cota de estabilidad del plasma
     toleq = maxval(abs(q-1.0d0))

  endif

  !bootstrap fraction      
  fbs=Ibs/Ip
  fcd=Icd/Ip



	
end subroutine ADDITIONAL_CALCS





!C----------------------------------------------------------------------|
	subroutine	SMOOTH(ALFA,NO,FO,XO,N,FN,XN)
!C----------------------------------------------------------------------|
	implicit none
!	include	'for/parameter.inc'
	integer	NO,N,J,I
	double precision	ALFA,XO(NO),FO(NO),XN(N),FN(N),P(N)
	double precision	YF,YX,YP,YQ,YD,FJ
!	if (N .gt. NRD .or. NO .le. 0)	then
!		write(*,*)' >>> SMOOTH: array is out of limits'
!		call	a_stop
!	endif
	if (NO .eq. 1)	then
	   do	j=1,N
		FN(j) = FO(1)
	   enddo
	   return
	endif
	if (NO .eq. 2)	then
	  do	j=1,N
	   FN(j)=(FO(2)*(XN(j)-XO(1))-FO(1)*(XN(j)-XO(2)))/(XO(2)-XO(1))
	  enddo
	  return
	endif
	if (N .lt. 2)	then
!		write(*,*)' >>> SMOOTH: no output grid is provided'
!		call	a_stop
	endif
	if (abs(XO(NO)-XN(N)) .gt. XN(N)/N)	then
!	    write(*,*)'>>> SMOOTH: grids are not aligned'
!	    write(*,'(1A23,I4,F8.4)')'     Old grid size/edge',NO,XO(NO)
!	    write(*,'(1A23,I4,F8.4)')'     New grid size/edge',N,XN(N)
!	    call	a_stop
	endif
	do	1	j=2,N
	   YP = (XN(j)-XN(j-1))
	   if (YP .le. 0.d0)	then
!	write(*,*)'>>> SMOOTH: new grid is not increasing monotonically'
!	      write(*,'(A,I4,A,F8.4)')'Node ',j-1,'   Value',XN(j-1)
!	      write(*,'(A,I4,A,F8.4)')'Node ',j,  '   Value',XN(j)
!	      call	a_stop
	   endif
	   P(j)	=ALFA/YP/XO(NO)**2
 1	continue
	P(1)	=0.
	FN(1)	=0.
	I	=1
	YF	=(FO(2)-FO(1))/(XO(2)-XO(1))
	YX	=2./(XN(2)+XN(1))
	YP	=0.
	YQ	=0.
	do	5	j=1,N-1
		if(XO(I) .gt. XN(j))	GO TO 4
 3		I	=I+1
		if(I .gt. NO)	I=NO
		if(I .ne. NO .and. XO(I) .lt. XN(j))	GOTO	3
		YF	=(FO(I)-FO(I-1))/(XO(I)-XO(I-1))
 4		FJ	=FO(I)+YF*(XN(j)-XO(I))
		YD=1.+YX*(YP+P(j+1))
		P(j)	=YX*P(j+1)/YD
		FN(j)	=(FJ+YX*YQ)/YD
		if (j .eq. N-1)	goto	5
		YX	=2./(XN(j+2)-XN(j))
		YP	=(1.-P(j))*P(j+1)
		YQ	=FN(j)*P(j+1)
 5	continue
	FN(N)	=FO(NO)
	do	6	j=N-1,1,-1
		FN(j)	=P(j)*FN(j+1)+FN(j)
 6	continue
	end
!CC======================================================================|
	subroutine	SMAP(ALFA,NO,XO,N,XN,F)
!C----------------------------------------------------------------------|
!C Smooth mapping from grid 
!C Similar to SMOOTH but the same array, F, is used for input and output
!C----------------------------------------------------------------------|
	implicit none
!	include	'for/parameter.inc'
	integer	NO,N,J
	double precision	ALFA,XO(NO),XN(N),F(N),P(N)
	call	SMOOTH(ALFA,NO,F,XO,N,P,XN)
	do	j=1,N
	   F(j) = P(j)
	enddo
	end	subroutine smap ! End subroutine SMAP
!C======================================================================|
