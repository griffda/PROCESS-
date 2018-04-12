!initialize
	qdivt=0.d0

!T. Eich scaling
	lambda_q=0.73e-3*btor**(-0.78)* &     
		&   (rminor**2.*btor/(0.2*rmajor*ip))**1.02* &
 	&   (qtot-qrad)**0.1* &
 	&   (rmajor)**0.02* &
 	&    3.*3.

!geometry of field line
	lparsep=q(nx)*2.d0*pi*rmajor/2.d0
	ldiv=0.55*lparsep
	
!upstream qpar
	qpar=(qtot-qradedge)*1.d6/(2.d0*pi*(rmajor+rminor+shif(nx)) & 
	 & *lambda_q*rminor/rmajor/q(nx)/2.d0)

!some numerical values for Msicci model
	fx=10.d0
	tau_relax=0.1
	ds1=nne(nx)/nepr(nx)*comp%c_car
	dd1=comp%c_car*ds1
	ds2=nxe(nx)/nepr(nx)*comp%c_car
	dd2=ds2
	ds3=0.d0
	dd3=0.d0






if (num%isiccir.eq.0) then
!SOL model based on Eich scaling + a tanh fit of results of Mattia's model for qpar_divertor as a function of argon concentration

!this is a fit to MS model
!results
	t_plate=max(0.001,qpar/fx)/1.e7*atan(1./(comp%c_car*car*nepr(1)/ &
     & (max(0.001,qpar/fx)/1.e6*0.00011*(4./nsep)))**16.)
	qdivt = t_plate*0.8
endif






if (num%isiccir.eq.1) then
! M. Siccinio SOL model
	if (isnan(Tup_0d)) then
		tshguess=100.
		tupguess=1000.
		fmguess=0.
		dqoqguess=0.
		tdivguess=100.
	endif
	
!	call MS_SOL_model_sub(Lparsep,qpar,fx,0.d0, & 
!	 & 0.d0,0.d0,nsep*1.d19,qdivt,Tup_0d,t_plate,fm_SE,Lr,6.d0,8.d0,7.d0 & 
!		& ,1.e3*tsep,tau_relax,taue,tshguess,tupguess, & 
!		& fmguess,dqoqguess,Ldiv,tdivguess,rmajor,Qtot-Qradedge,ds1,dd1,ds2,dd2, &
!		& ds3,dd3)
	qdivt=qdivt/1e6

endif





