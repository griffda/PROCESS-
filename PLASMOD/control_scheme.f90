! this code control Xe, powers, He, Ar to fullfill requirements and constraints

	Psep = trapz((powe+powi)*dV) !net psep

!assign q_control if give povs
if (inp0%contrpovs.gt.0.) inp0%q_control=inp0%contrpovs*radp%vp(nx)*radp%gradro(nx)
if (inp0%contrpovr.gt.0.) inp0%q_control=inp0%contrpovr*geom%r



!LH threshold !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!PROCESS function
	ne_av = trapz(nepr*dv)/v(nx)*1.d19
	nela=sum(nepr)/nx*1.d19
	call pthresh(ne_av,nela,btor,rpmajor,elong,vprime(nx)*gradro(nx),2.5d0,PLH_th) !PROCESS function
!!!

!PLASMOD function
!	PLH_th(6)=1.67*(trapz(nepr*dv)/trapz(dv)/10.)**0.61*(geom%bt)**0.78 &    !PLASMOD function
!    & *rpminor**0.89*geom%r**0.94 !Martin scaling
!!!!

!assignment
	PLH = PLH_th(inp0%PLH)

	if (i_diagz.eq.1) 	write(*,*) 'plh',plh
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!this below is: if Psep < PLH*psepplhinf, applies NBI to not go into L mode
	if (comp%psepplh_inf.gt.0.) then
		q_heat=max(0.,min(inp0%pheatmax-q_cd-q_fus-inp0%q_control,q_heat+ & 
		& inp0%qnbi_psepfac*(comp%psepplh_inf-Psep/PLH)*num%dt/(1.+num%dt)))
	endif

! these below are: apply Xe if one of the criteria on Psep < psep_crit is to be satisfeid
dum2=1.d6
	if (comp%psepplh_sup.gt.0.d0) then !use psep/Plh sup as constraint
dum2=min(comp%psepplh_sup*PLH,dum2)
	endif 
	if (comp%psepb_q95AR.gt.0.d0) then !use psepbqar as constraint
dum2=min(dum2,comp%psepb_q95AR*(btor/q_95/geom%A/rpmajor)**(-1.))

	endif
	if (comp%psep_r.gt.0.d0) then !use psep/R as constraint
dum2=min(dum2,comp%psep_r*rpmajor)
	endif


if (dum2.lt.1.d6.and.comp%fcoreraditv.lt.0.d0) then !do the calculation
		cxe=max(0.,cxe+inp0%cxe_psepfac*(Psep-dum2)/dum2*num%dt/(1.+num%dt))
 if (q_heat.gt.0.) cxe=0.d0
endif

if (comp%fcoreraditv.ge.0.d0) then !if fcoreraditv is given , replace the above with this one
 		cxe=max(0.,cxe+inp0%cxe_psepfac*(comp%fcoreraditv*(psepxe-dum2)/dum2-plinexe/dum2)*num%dt/(1.+num%dt))
 if (q_heat.gt.0.) cxe=0.d0
endif





!constraint: current drive fni or loop voltage!
	if (inp0%f_ni.gt.0.) then !use f_ni as constraint
		q_cd=max(0.,min(inp0%pheatmax-q_heat-q_fus-inp0%q_control,q_cd+& 
		& inp0%qnbi_psepfac*(inp0%f_ni-(fbs+fcd))*num%dt/(1.+num%dt)))
	endif
	if (inp0%fcdp.ge.0.) then ! use fcdp as constraint
		q_cd=inp0%fcdp*(inp0%pheatmax-q_heat-q_fus-inp0%q_control)
	endif
	if (inp0%V_loop.gt.-1.e3) then !use loop voltage as constraint
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
 loss%pnbi=min(inp0%maxpauxor*geom%r, & 
 & min(inp0%pheatmax,q_heat+q_cd+q_fus+inp0%q_control))

!this below limits the total power to the actual one if overridden
if(q_heat.gt.0.) q_heat=q_heat*loss%pnbi/(q_heat+q_cd+q_fus+inp0%q_control)
if(q_cd.gt.0.) q_cd=q_cd*loss%pnbi/(q_heat+q_cd+q_fus+inp0%q_control)
if(q_fus.gt.0.) q_fus=q_fus*loss%pnbi/(q_heat+q_cd+q_fus+inp0%q_control)


!SOL MODEL below
!constraint: divertor temperature --> gives Ar in output
	include 'solmodel.f90'
	if (i_diagz.eq.1) 	write(*,*) 'lambda',lambda_q,lparsep,ldiv,qpar,t_plate,qdivt,nsep
	if (comp%qdivt.gt.0.) then
	include 'solmodel.f90'
!	qdivt=0.d0 !put here sol model for qdivt
	 car = max(0.,car+inp0%car_qdivt*(qdivt-comp%qdivt)*num%dt/(1.+num%dt))
	if (i_diagz.eq.1) 	write(556,*) car,qdivt
	if (i_diagz.eq.1) 	write(*,*) 'ccar',car,qdivt,qpar
	endif


!Helium concentration calculation
	if (comp%globtau(3).gt.0.) then
	 che = comp%globtau(3)*max(0.001,taue)*Sfus_he/integr_cde(v,nepr,nx)
	endif



!END OF MODULE
