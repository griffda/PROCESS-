! this subroutine computes the 2D equilibrium + the current diffusion equation at steady-state

subroutine compute_equil( &
  nx, jiter,i_equiltype, &
  x, te, ti, ne, ni, palph, cc, G30,qinit,G20,vp0, &
  R,rmin,elon,tria,Ip,btor,betaz,lint,ipol0,e_charge,mu_vacuum, &
  qedge, &
  roc, Vloop, fbs,fcd, toleq, &
  k, d, shif, cubb, jcdr, V, G1, G2, G3, dV, phi, q, rho, psi, jpar,&
  ipol, Vprime,droda,eqpf,eqff,gradro,q_edge_in,f_ind_in,q_95,elong95,triang95 &
  ,pres_fac,areat,isawt,j_qeq1)

  use grad_func
  implicit none


!variable declaration!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer, intent(inout) :: nx, jiter,i_equiltype,j_qeq1
  integer :: j,isawt
! SJP Issue #859
  real(kind(1.0D0)), intent(in) :: q_edge_in,f_ind_in,R,rmin,btor,betaz,lint,e_charge,mu_vacuum
  real(kind(1.0D0)), dimension(nx), intent(in) :: ipol0

  real(kind(1.0d0)), intent(inout) :: pres_fac,qedge,ip,q_95,elon,tria,elong95,triang95
  real(kind(1.0D0)), intent(inout) :: roc, Vloop, fbs,fcd, toleq
  real(kind(1.0d0)), dimension(nx), intent(in) :: x, te, ne, ni, cc, G30,qinit,G20,vp0
  real(kind(1.0d0)), dimension(nx), intent(inout) :: k, d, shif, cubb, jcdr, V, G1, G2, G3, dV, phi, q, rho, psi, jpar
  real(kind(1.0d0)), dimension(nx), intent(inout) :: ipol, Vprime,droda,eqpf,eqff,gradro,palph, ti
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !  Local variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  integer :: na,diagz,nxtemp,redo
  real(kind(1.0D0)) :: dpsidt, Epar, Ibs, fb, yb, C1
  real(kind(1.0d0)) :: gpp4,gp2,yro,hro,yda,TIME,alfa,areat
		real(kind(1.0d0)), dimension(nx) ::  &
  & bdb0,bmint,bmaxt, b0db2,ya,ybb,BDB02,FOFB,qg3s,q0,vvvv
  real(kind(1.0d0)), dimension(nx) :: gra,sqgra,grar,avr2,ai0,dgrda,avsqg !gradient,sqrgradient,radial gradient,
  real(kind(1.0D0)), dimension(nx) :: smallk, dvdr, rhoint, f, jpol, kerncur, pressure, A, B, C, bbb, ccc, dum1, dum2, dum3
  real(kind(1.0D0)), dimension(nx) :: chat,betahat,y,  fp, kpk, dpk,  pprime,FF,ffprime,ba,bb
  real(kind(1.0d0)), dimension(nx) :: gr,GBD,GL,GSD, & 
  &  BD,BC,B2B0EQ,B0B2EQ,BMAXEQ,BMINEQ,BMODEQ,FOFBEQ,GRDAEQ, &
  &  btooo,rooo,g11,g22,g33,slat,vr
! Quiet NAN, double precision.
  real(kind(1.0d0)), parameter :: pi = 3.141592,ACEQLB=1.0d-6 !pi = 3.1415926536d0,ACEQLB=1.e-6
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!some initialization
  diagz=1
! SJP Issue #859
! Correct the formula as ipol0 is now an array
! Don't use the equation as pres_fac is overwritten in the next line
! pres_fac=betaz*f_ind_in*ipol0(1)*lint*vp0(1)
pres_fac=1.d0
  na = nx-1
  gp2 = 2.*pi
  gpp4 = gp2*gp2
  cubb(1) = 0.0d0
  rhoint = x * rmin !minor radiu



  if (jiter .eq. 0) then  !at the very first iteration, computes a very rough analytical equilibrium
     call INITEQUIL( &
          nx,x,elon,tria,R,rmin, &
          k,d,shif,V,G1,G2,G3&
          )
  else    ! for every other iteration after 0

		redo=0 !check if eq has crashed

111	continue
     pressure = 1.d3 * e_charge * 1.d19 * (te*ne + ti*ni + palph)*pres_fac ! plasma local total pressure in J/m^3

	if (pres_fac.lt.0.001) then !stop if pressure goes to 0
		write(*,*) 'equilibrium not possible',pres_fac
		stop 1
		endif
	if (isnan(palph(1))) pressure=0.d0
	if (isnan(te(1))) pressure=0.d0
	if (isnan(ti(1))) pressure=0.d0

!this below is E. Fable scheme for stable equilibrium calculations, NF 2012!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!computes FFprime and PPrime
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!EMEQ- 3 Moment equilibrium caller!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     if (i_equiltype.eq.1.or.i_equiltype.eq.2) then

	BB = -2.*pi*R*pprime*1.e-6 !Zakharov (17)
	BA = -2.*pi/mu_vacuum/R*ffprime*1.e-6+BB !Zakharov (16)
	eqpf=BB
	eqff=-2.*pi/mu_vacuum/R*ffprime*1.e-6

	TIME=0.
        

	nxtemp=nx
	!call emeq with inputs Ba, BB which are pprime and ffprime recasted
	call EMEQ(redo,BA,BB,R+SHIF(nx),rmin,ELON,TRIA*rmin,nx, &		! radial grid point No.
             &	 ACEQLB,BTOR*R/(R+SHIF(nx)),IP,GR,GBD,GL,GSD,gra, & 
        &  sqgra,grar,avr2,ai0,dgrda,avsqg,vvvv,B2B0EQ,B0B2EQ,BMAXEQ,BMINEQ,BMODEQ,FOFBEQ,GRDAEQ)

	if (nx.lt.1.or.isnan(sqgra(2))) then ! if crashed, redo at lower pressure
	nx=nxtemp
	pres_fac=pres_fac*0.9
	redo=1
		goto 111
	endif

!below, assign quantities and smooth profiles
	BTOOO=BTOR*R/(R+SHIF(nx)) ! toroidal field at plasma axis
	ROOO=(R+SHIF(nx))! plasma axis


!smoothing below
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


!some assignments
	IPOL=ai0
	ROC = GR(nx)		! Define a new RHO_edge
	rho = GR !rho toroidal
	phi=btor*3.141592*rho**2.d0
	GPP4 = GP2*GP2
	hro = (x(2)-x(1))*rmin !minor radius differential
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
  SLAT = GRADRO*VR
	YDA = rmin/(nx-1.)
	do	j =1,nx
	   A(j) = YDA*(J-1.) !ametr
	   if (j.ne.1) B(J) = GSD(J)/A(J)
	enddo
	B(1)=0. !tria
	d=B
        BDB02=B2B0EQ
        B0DB2=B0B2EQ
        BMAXT=BMAXEQ
        BMINT= BMINEQ
        BDB0 =BMODEQ
        FOFB = FOFBEQ
	BDB02=BDB02*BTOOO**2./BTOR**2.
	BDB0=BDB0*BTOOO/BTOR
	B0DB2=B0DB2/BTOOO**2.*BTOR**2.
 areat=2.*pi*trapz((x*rmin*k+x**2.*rmin**2./2.*derivcc(nx,x*rmin,k,1)))*(x(2)-x(1))*rmin
	endif
!EMEQ!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end if

	if (isnan(sqgra(2))) then
!if crashed, dont do anything	
		else !call current diffusion equation solver below
  call ADDITIONAL_CALCS( &
       i_equiltype,jiter,nx,V,btor,Ip,R,rmin,x,cc,cubb, jcdr, FF,G2,G3,q0,mu_vacuum, &
       qedge, &
       dV,phi,rho,roc,ipol,jpol,kerncur,Ibs,Epar,jpar,Vloop,Vprime,q,psi,toleq, dum1, fbs,fcd &
       ,q_edge_in,q_95,elon,tria,elong95,triang95,k,d,isawt,j_qeq1)
	endif


!end of equil
	return
end subroutine compute_equil



! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this subroutine provides a rough analytical equilibrium
subroutine INITEQUIL( &
     nx,x,elon,tria,R,rmin, &
     k,d,shif,V,G1,G2,G3&
     )

  use grad_func
  implicit none

  integer, intent(in) :: nx
  real(kind(1.0d0)), intent(in) :: elon,tria,R,rmin
  real(kind(1.0d0)), dimension(nx), intent(in) :: x
  real(kind(1.0D0)), dimension(nx), intent(out) :: k,d,shif,V,G1,G2,G3
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

	return
end subroutine INITEQUIL



! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!the routine below computes current diffusion, ip, loop voltage, q profile, etc.
subroutine ADDITIONAL_CALCS( &
     i_equiltype,jiter,nx,V,btor,Ip,R,rmin,x,cc,cubb,jcdr,FF,G2,G3,q0,mu_vacuum, &
     qedge, &
     dV,phi,rho,roc,ipol,jpol,kerncur,Ibs,Epar,jpar,Vloop,Vprime,q,psi,toleq, dum1, fbs,fcd &
     ,q_edge_in,q_95,elon,tria,elong95,triang95,k,d,isawt,j_qeq1)

  use grad_func
  implicit none	

!input/output exchange variables
  integer, intent(in) :: nx,jiter,i_equiltype,isawt
		integer, intent(out) :: j_qeq1
  real(kind(1.0d0)), intent(in) :: R,rmin,btor,mu_vacuum,q_edge_in
  real(kind(1.0d0)), intent(inout) :: ip,q_95
  real(kind(1.0d0)), intent(inout) :: qedge,elon,tria,elong95,triang95
  real(kind(1.0D0)), intent(inout) :: roc,Vloop,toleq,Ibs,Epar,fbs,fcd
  real(kind(1.0d0)), dimension(nx), intent(inout) :: Vprime,k,d
  real(kind(1.0d0)), dimension(nx), intent(in) :: V,x,cc,cubb,jcdr,FF,G2,G3,q0
  real(kind(1.0D0)), dimension(nx), intent(inout) :: dV,phi,rho,ipol,jpol,kerncur,jpar,q,psi, dum1 

!local variables
  integer :: j,DEBUG_FLAG,j_9
  real(kind(1.0D0)) :: icd
  real(kind(1.0D0)) :: dpsidt,gp2,qcyl,alfa,kcyl,dcyl
  real(kind(1.0D0)), dimension(nx) :: dvdr,rhoint,f,dum2
  real(kind(1.0d0)), parameter :: pi = 4.0d0*datan(1.0d0) !3.1415926535d0 !, mu_vacuum = 1.2566d-6

!some definitions
  gp2 = q_edge_in*FF(1)
  gp2 = 2.*pi
  rhoint = x * rmin !minor radius
		qcyl=rmin**2.d0*btor/R/ip/0.091

!choose
  if (jiter .ne. 0) then  !if iterations already ongoing, use real calculation
     dV=vprime*(x(2)-x(1))*rmin ! dV/dr
     jpol = ipol/(R*btor)
 ! R*Bphi normalized to reference B*R

!calculation of currents
     kerncur = cc/ipol**2.*dV
 
     Ibs = sum(cubb/ipol**2.*dV)*ipol(nx)*btor/(2.*pi)
 !integrated bootstrap in MA
     Icd = sum(jcdr/ipol**2.*dV)*ipol(nx)*btor/(2.*pi)
 !integrated CD

     Epar = (Ip - Ibs-Icd)/(ipol(nx)*btor/(2.*pi)*trapz(kerncur))
 !electric field

     jpar = cc*Epar + cubb+jcdr
 !parallel current density of the plasma
					
     Vloop = Epar * 2. * pi * btor/(ipol(nx) * G3(nx))
 !loop voltage
					
!calculation of safety factor
     kerncur = jpar/ipol**2.*dV
     dum1=ipol*btor/gp2*cumint1(dV*jpar/ipol**2.)
     q=ipol*g2*g3/(mu_vacuum*8*pi**3.*dum1*1.e6)
	
	j_qeq1=0
	
 q(1)=q(2)
	j_9=1
	do j=1,nx
	if (q(j).le.1.d0) then !find q = 1 surface
		j_9=j
	endif
	enddo
	if (j_9.gt.1) then !fix q = 1 if below 1
	if (isawt.eq.1) q(1:j_9) = 1.d0
	if (isawt.eq.1) j_qeq1=j_9
	goto 10
	endif

!find reversed q surfaces
	j_9=1
	do j=1,nx
	if (q(nx-j+1).le.q(1)) then
		j_9=nx-j+1
		goto 11
	endif
	enddo
11	continue ! if q is reversed, fix it to some arbitrary profile which is monotonic
	q(1)=sum(q(1:j_9))/(j_9-1.d0)
	q(1:j_9)=q(1)+(q(j_9)-q(1))*v(1:j_9)/v(j_9)	
 q(1)=q(2)
10	continue
!smooth q
	ALFA = .00001
	call SMAP(ALFA,nx,x,nx,x,q)

     qedge=q(nx)
 !edge q
					
     psi = integrcc(nx,phi,1.d0/q)
 !compute Psi in Wb
					
     toleq = maxval(abs(q-q0)/q0) !tolerance of q
					
!find 95% position
	dum2=(psi-psi(1))/(psi(nx)-psi(1))	     
	j_9=1
	do j=1,nx
	if (dum2(j).le.0.95) j_9=j
	enddo
!calculate q, k, and d at 95 position
	qcyl=(q(j_9+1)-q(j_9))/(dum2(j_9+1)-dum2(j_9))*(0.95-dum2(j_9))+q(j_9)
	kcyl=(k(j_9+1)-k(j_9))/(dum2(j_9+1)-dum2(j_9))*(0.95-dum2(j_9))+k(j_9)
	dcyl=(d(j_9+1)-d(j_9))/(dum2(j_9+1)-dum2(j_9))*(0.95-dum2(j_9))+d(j_9)

	if (i_equiltype.eq.1) ip=ip*qcyl/q_95 !if q95 given, recalculate ip
	if (i_equiltype.eq.2) q_95=qcyl !if ip given, recalculate q95

!recalculate k, d fixing 95 value from input
	k=k/kcyl*elong95
	d=d/dcyl*triang95
	elon=k(nx)
	tria=d(nx)

  else !just for very first iteration, use rough estimates below
     dV=gradient1(V)
     phi=btor*V/(2.*pi*R)
     q=1.+(qedge-1.)*x**4.
     rho=sqrt(phi/(pi*btor)) !rho toroidal
     roc=rho(nx)
     psi=cumint1(gradient1(phi)/q)
     f=gradient(psi,rhoint)
     ipol=R*btor*(1.+0.*x)
     jpol=ipol/(R*btor)
     kerncur=dV*cc/ipol**2
     Ibs=sum(dV*cubb/ipol**2)*ipol(nx)*btor/(2.*pi)
     Icd=sum(dV*jcdr/ipol**2)*ipol(nx)*btor/(2.*pi)
     dpsidt=0.
     Epar=dpsidt
     jpar=cc*Epar+cubb+jcdr
     Vloop=Epar*2.*pi*btor/(ipol(nx)/G3(nx))
     Vprime=gradient(V,rhoint)
     toleq = maxval(abs(q-1.0d0))

  endif

  !bootstrap and current drive fractions
		fbs=Ibs/Ip
  fcd=Icd/Ip


!end of current diffusion calculations

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
