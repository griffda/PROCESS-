	call update_profiles(dx, nxt, nchannels, gy0, y0, rmajor, y, N_e(1:nxt), T_e(1:nxt), T_i(1:nxt))
	
!	nepr=interp1([0.0d0, xtrt,amin], [N_e0(1), N_e0,nsep], xr)
	
	
!		write(*,*) 'xepr ',[0.0d0, xtrt, amin],[N_e0(1), N_e0, nsep],xr
!		write(*,*) 'nepr ',nsep,N_e0,nepr , '***'
!		write(*,*) 'tepr ',tsep,t_e0,tepr , '***'
		
!	tepr=interp1([0.0d0, xtrt,amin], [T_e0(1), T_e0,tsep], xr)
!	tipr=interp1([0.0d0, xtrt,amin], [T_i0(1), T_i0,tsep], xr)
	nepr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [N_e0(1), N_e0],xr)
	tepr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [T_e0(1), T_e0],xr)
	tipr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [T_i0(1), T_i0],xr)


	
	! Powers section -- to be coupled otherwise to HandCD ultimately
	! Auxiliary
	Peaux = exp(-(x-inp0%x_heat(2))**2/inp0%dx_heat(2)**2.d0)
	Pech=0.*Peaux/trapz(Peaux*dV);


!sum over powers
	Piaux = exp(-(x-inp0%x_heat(1))**2/inp0%dx_heat(1)**2.d0)
 Pnbi=q_heat*Piaux/trapz(Piaux*dV);
	Piaux = exp(-(x-inp0%x_cd(1))**2/inp0%dx_cd(1)**2.d0)
 Pnbi=Pnbi+q_cd*Piaux/trapz(Piaux*dV);
	Piaux = exp(-(x-inp0%x_fus(1))**2/inp0%dx_fus(1)**2.d0)
 Pnbi=Pnbi+q_fus*Piaux/trapz(Piaux*dV);
	Piaux = exp(-(x-inp0%x_control(1))**2/inp0%dx_control(1)**2.d0)
 Pnbi=Pnbi+inp0%q_control*Piaux/trapz(Piaux*dV);


!write(*,*) 'fds ',q_heat,q_cd,q_fus,inp0%q_control


!split between electrons and ions of NBI
	include 'calculate_beam_split.f90'

	Peaux=Pech+nbi_split*Pnbi;
 Piaux=(1.d0-nbi_split)*Pnbi;
	! Radiation -- to couple to PROCESS properly
	! Impurity densities -- also to couple to PROCESS where necessary
	nHe = cHe * nepr

! impurity model 0
	if (num%i_impmodel.eq.0) then
		nNe = car * nepr
		nXe = cXe * nepr
	endif
	if (num%i_impmodel.eq.1) then
		nNe = car * nepr
		nXe = cXe * nepr
	nne = nne(jped)
	nxe = nxe(jped)
	endif
		
	call prxe_func(nx, tepr, prxe, zavxe)
	call prar_func(nx, tepr, prne, zavne)
	prad = nXe*nepr*prxe + nne*nepr*prne
	ndeut = fuelmix*(nepr - nNe*zavne - nXe*zavxe - 2.0d0*nHe-comp%protium*nepr)
	ntrit = (1.0d0-fuelmix)*(nepr - nNe*zavne - nXe*zavxe - 2.0d0*nHe-comp%protium*nepr)
	nions = ndeut + ntrit + nNe + nXe + nHe+comp%protium*nepr
	zeff = (1.0d0/nepr) * (ndeut+ntrit+4.0d0*nHe+zavne**2*nNe+zavxe**2*nXe+comp%protium*nepr)
    coulg = 15.9d0 - 0.5d0*log(nepr) + log(tepr)

  PBRAD=5.06E-5*zeff*NEpr**2.d0*SQRT(TEpr)
  totse(nx)=trapz(tepr*dV)/V(nx)
  totsi(nx)=trapz(nepr*dV)/V(nx)
	
	! Calculate fusion power -- also in PROCESS...
	do jrad = 1, size(x)
	  svdt = tipr(jrad)**(-0.333333333d0)
	  svdt = 8.972d0*exp(-19.9826d0*svdt)*svdt*svdt*((tipr(jrad)+1.0134d0) & 
			/(1.0d0+6.386d-3*(tipr(jrad)+1.0134d0)**2)     +1.877d0*exp(-0.16176d0*tipr(jrad)*sqrt(tipr(jrad))))
	  pdt = 5.632d0*ndeut(jrad)*ntrit(jrad)*svdt
 ts_alf = 2.*SQRT(tepr(jrad))*tepr(jrad)/nepr(jrad)/COULG(jrad)
 NALPH = NDEUT(jrad)*NTRIT(jrad)*SVDT*ts_alf
YVALP=1.2960e+07
YLLAME=23.9+LOG(1.e3*tepr(jrad)/SQRT(1.e19*nepr(jrad)))
YY6=SQRT(1.e3*tepr(jrad)/1.e19*nepr(jrad))
YY6=YY6*(4.*AMAIN*YVALP)/(4.+AMAIN)	 
  	  if(YY6.lt..1)YY6=0.1
 YLLAMI=14.2+LOG(YY6)
 YY6=SQRT(1.0e3*tepr(jrad)/1.0e19*nepr(jrad))*2.0*YVALP
  	  if(YY6.lt..01)YY6=.01
YLLAMA=14.2+LOG(YY6)
  YY6=YLLAMI*NIons(jrad)/(AMAIN*nepr(jrad))
  YY6=7.3e-4/YLLAME*(YY6+YLLAMA*NALPH/nepr(jrad))

  	  if(YY6.lt.0.0001)YY6=.0001
  YVC=YY6**0.33*SQRT(2.0*tepr(jrad)*1.7564e+14)
YEPS=YVALP/(YVC+0.0001)
YY6=atan(0.577*(2.*YEPS-1.))	   
YY7=log((1.+YEPS)**2/(1.-YEPS+YEPS**2))
PAION=2./YEPS**2*(0.577*YY6-0.167*YY7+0.3)

!electron and ion fusion power
      pedt(jrad) = (1.-paion)*pdt
      pidt(jrad) = paion*pdt
      peicl(jrad)=0.00246*COULG(jrad)*NEpr(jrad)*NIons(jrad)*ZMAIN**2.d0* &
     & (TEpr(jrad)-TIpr(jrad))/(AMAIN*TEpr(jrad)*SQRT(TEpr(jrad)))
yllama=1.d0
  yv6=totse(nx)
  yv7=totsi(nx)

	PSYNC(jrad)=1.32E-7*(yv6*BTOR)**2.5 &
     & *SQRT(yv7/rminor*(1.+18.*rminor/(rmajor*SQRT(yv6))))*yllama

!alpha particle content (not used for particle balance)
	nalf(jrad)=nalph
	end do


	pradtot=prad+psync+pbrad
	jrad=min(nx,max(1,nint(comp%pradpos*nx)))
	pradedge=pradtot
	pradtot(jrad:nx)=0.d0
	if (jrad.gt.1)	pradedge(1:jrad-1)=0.d0

	pradedge=pradedge+pradtot*(1.-comp%pradfrac)
	pradtot=pradtot*comp%pradfrac





	Qrad = trapz((pradtot)*dV)
	Qradedge = trapz((pradedge)*dV)

	pfus = 5.0d0*cumint1((pedt+pidt)*dV)


	sfus_he=pfus(nx)/5./5.632
	! net powers
	Powe = Peaux - peicl + pedt - prad- psync - pbrad +q_oh
	Powi = Piaux + peicl + pidt


	totSe = cumint1(Powe*dV)
	totSi = cumint1(Powi*dV)
    DdnVne = 625.d0*interp1_ef(nx,nxt,xr, totSe/G1/vprime/nepr, xtr)
    DdnVni = 625.d0*interp1_ef(nx,nxt,xr, totSi/G1/vprime/nions, xtr)
	
	! Fuelling
	
	Sn = exp(-(x-0.85d0)**2 / 0.05d0)  ! No fuelling now, but this is pellet
	totS = cumint1(Sn*dV)
	totS = totS/totS(nx) 
	DdnVn = interp1(xr, inp0%spellet*inp0%fpellet*totS/Vprime/G1, xtr)
	
	c(:,1) = DdnVn
	c(:,2) = DdnVne
	c(:,3) = DdnVni
	
	! define transport coefficients
	
	call trmodel(i_modeltype,nx,nxt, nchannels,vprime,2.5d0,1.d0,1.+0.*amin,1.+0.*amin,1.+0.*amin, & 
			& nepr,tepr,nepr,0.5*nepr,0.*nepr,tipr,1.+0.*nepr,0.*nepr,2.5+0.*nepr,1./q,RHO,x*rminor, &
			& SHIF,k,d,0.*shif,0.*shif,0.*shif,0.*shif,IPOL,g1*vprime,vprime,1.+0.*vprime, & 
			& SHEAR,0.*nepr,0.*nepr,palph,0.*nepr,0.*nepr,0.*nepr,y0, gy0, xtr,x, amin, rmajor, btor,num%capA, q_tr, sh_tr, a, b, &
			& Hfactor,chi00,chipow,Hnow,chifac0)

	chat = a * gy0 * y0/rmajor + b * y0  ! To compare to c

	F = chat-c  ! New residual



