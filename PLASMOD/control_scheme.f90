!constraints: psep and plh
	Psep = trapz((powe+powi)*dV) !net psep, including radiation

	if (inp0%PLH.eq.0) then
	PLH=1.67*(trapz(nepr*dv)/trapz(dv)/10.)**0.61*(geom%bt)**0.78 &
     & *rminor**0.89*geom%r**0.94 !Martin scaling
	else
	PLH=inp0%PLH
	endif


	if (i_diagz.eq.1) 	write(*,*) 'plh',plh

	if (comp%psepplh_inf.gt.0.) then
		q_heat=max(0.,min(inp0%pheatmax-q_cd-q_fus-inp0%q_control,q_heat+ & 
		& inp0%qnbi_psepfac*(comp%psepplh_inf-Psep/PLH)*num%dt/(1.+num%dt)))
	endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Psep mitigation
!	write(*,*) 'psep' ,Psep/PLH,Psep/rmajor,Psep*btor/q_95/geom%A/rmajor,q_heat,cxe

dum2=1.d6
	if (comp%psepplh_sup.gt.0.d0) then
dum2=min(comp%psepplh_sup*PLH,dum2)
	endif
	if (comp%psepb_q95AR.gt.0.d0) then
dum2=min(dum2,comp%psepb_q95AR*(btor/q_95/geom%A/rmajor)**(-1.))
	endif
	if (comp%psep_r.gt.0.d0) then
dum2=min(dum2,comp%psep_r*rmajor)
	endif
	
	if (dum2.lt.1.d6) then
		cxe=max(0.,cxe+inp0%cxe_psepfac*(Psep-dum2)/dum2*num%dt/(1.+num%dt))
	comp%cxe=cxe
	endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!constraint: current drive fni or loop voltage!
	if (inp0%f_ni.gt.0.) then
		q_cd=max(0.,min(inp0%pheatmax-q_heat-q_fus-inp0%q_control,q_cd+& 
		& inp0%qnbi_psepfac*(inp0%f_ni-(fbs+fcd))*num%dt/(1.+num%dt)))
	endif
	if (inp0%V_loop.gt.-1.e3) then
		q_cd=max(0.,min(inp0%pheatmax-q_heat-q_fus-inp0%q_control,q_cd+& 
		& inp0%qnbi_psepfac*(vloop-inp0%V_loop)*num%dt/(1.+num%dt)))
	endif


!constraint: pfusion target
	if (inp0%pfus.gt.0.) then
		q_fus=max(0.,min(inp0%pheatmax-q_heat-q_cd-inp0%q_control,q_fus+& 
		& inp0%qnbi_psepfac*(1.d0-pfus(nx)/inp0%pfus)*num%dt/(1.+num%dt)))
		if (i_diagz.eq.1) 	write(*,*) q_heat,q_cd,q_fus,pfus(nx)
	endif

!sum up all powers
 loss%pnbi=min(inp0%maxpauxor*geom%r,min(inp0%pheatmax,q_heat+q_cd+q_fus+inp0%q_control))


!write(*,*) comp%psepb_q95AR,Psep*btor/q_95/geom%A/rmajor,cxe

!constraint: divertor temperature
	include 'solmodel.f90'
	if (i_diagz.eq.1) 	write(*,*) 'lambda',lambda_q,lparsep,ldiv,qpar,t_plate,qdivt,nsep
	if (comp%qdivt.gt.0.) then
	include 'solmodel.f90'
!	qdivt=0.d0 !put here sol model for qdivt
	 car = max(0.,car+inp0%car_qdivt*(qdivt-comp%qdivt)*num%dt/(1.+num%dt))
	comp%car=car
	if (i_diagz.eq.1) 	write(556,*) car,qdivt
	if (i_diagz.eq.1) 	write(*,*) 'ccar',car,qdivt,qpar
	endif


!Helium concentration 
	if (comp%globtau(3).gt.0.) then
	 che = 1.8d0*comp%globtau(3)*max(0.001,taue)*Sfus_he/integr_cde(v,nepr,nx)
	comp%che=che
	endif
