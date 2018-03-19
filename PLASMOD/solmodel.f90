	qdivt=0.d0

!eich scaling
	lambda_q=0.73e-3*btor**(-0.78)* &     
		&   (rminor**2.*btor/(0.2*rmajor*ip))**1.02* &
 	&   (qtot-qrad)**0.1* &
 	&   (rmajor)**0.02* &
 	&    3.*3.

	lparsep=q(nx)*2.d0*pi*rmajor/2.d0
	ldiv=0.55*lparsep
	
	qpar=(qtot-qrad)*1.d6/(2.d0*pi*(rmajor+rminor+shif(nx)) & 
	 & *lambda_q*rminor/rmajor/q(nx)/2.d0)

	fx=10.d0
!0.1 is car, core Ar concentration
!this is a fit to MS model
	t_plate=max(0.001,qpar/fx)/1.e7*atan(1./(comp%c_car*car*nepr(1)/ &
     & (max(0.001,qpar/fx)/1.e6*0.00011*(4./nsep)))**16.)
	qdivt = t_plate*0.8



!write(*,*) 'qpar',qpar/1e6,qdivt,qpar/1.d6/qdivt
