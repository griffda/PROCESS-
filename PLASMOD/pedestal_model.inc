!define the following quantities
!    teb = ped%teped
!    tib = teb
!    neb = ped%nped*1.0d-19
!    nsep = ped%nsep*1.0d-19




!EPED scaling for DEMO

!compute betan
	pressure=(nepr*tepr+tipr*nions+palph)
	betan=100.*rminor*btor/Ip*0.004/btor**2.*trapz(pressure*dV)/V(nx)
	P_pedtop = 1.3*rmajor**(-0.38)*triang**(0.83)*elong**(0.62)*Ip**1.25*betan**0.43

!SOL and pedestal stuff

	lambda_sol = 0.01 ! 1 cm
	V_sol = 2.*3.141592*rmajor*qedge*lambda_sol
	tau_sol = 0.1 ! s

	D_ped = 0.1 ! m^2/s
	V_ped = ped%rho_n*rminor*2.*3.141592*rmajor*2.*3.141592*rminor
	
!!compute pedestal top density and separatrix density
!	nsep = (spellet+sfuelling)*tau_sol/V_sol
!	neb = nsep+spellet*ped%rho_n*2.*rminor**2./(D_ped*V_ped)


!compute pedestal top temperature
	teb = P_pedtop/neb	
	
