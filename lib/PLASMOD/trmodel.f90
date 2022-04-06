!transport model: calculation of transport coefficients on the reduced grid

subroutine trmodel(i_modeltype,nx,nxt, nchan, & !input
  VR,AMJ,ZMJ,AIM1,AIM2,AIM3, & ! input
  NE,TE,NI,NDEUT,NIZ1,TI,ZEF,ZIM1,AMAIN,MU,RHO,AMETR, & ! input
  SHIF,ELON,TRIA,VPOL,VTOR,ER,NIBM,IPOL,G11,VRS,GRADRO, &  ! input
  SHEAR,PBLON,PBPER,PFAST,NIZ3,ZIM2,ZIM3, & ! input
  y0, gy0, xtr,x, amin, rmajor, btor,capA, q_tr, sh_tr, &! input
  a, b, &
  Hfactor,chi00,chipow,Hnow,chifac,npikpalmod,xihepalmod)

  use grad_func
  implicit none

!exchange variables
  integer, intent(in) :: nxt, nchan, i_modeltype,nx
  real(kind(1.0d0)), intent(in) :: amin, rmajor, capA,btor
  real(kind(1.0d0)), intent(in) :: AMJ,ZMJ,AIM1,AIM2,AIM3
  real(kind(1.0d0)), intent(in) :: Hfactor,chi00,chipow,Hnow,chifac
  real(kind(1.0d0)), intent(in) :: npikpalmod,xihepalmod
  real(kind(1.0d0)), dimension(nxt), intent(in) :: xtr, q_tr, sh_tr
  real(kind(1.0d0)), dimension(nxt, nchan), intent(out) :: a, b
  real(kind(1.0d0)), dimension(nx), intent(in) :: VR, &
  & NE,TE,NI,NDEUT,NIZ1,TI,ZEF,ZIM1,AMAIN,MU,RHO,AMETR, &
  & SHIF,ELON,TRIA,VPOL,VTOR,ER,NIBM,IPOL,G11,VRS,GRADRO, &
  & SHEAR,PBLON,PBPER,PFAST,NIZ3,ZIM2,ZIM3,x
  real(kind(1.0d0)), dimension(nxt, nchan), intent(in) :: gy0, y0

  !  Local variables
  integer :: IS,IE,NA1,NA1N,NA1E,NA1I,j,jmin
  real(kind(1.0d0)) :: TIME, &
  &		HRO,HROA,ROC,RTOR
  real(kind(1.0d0)), dimension(nxt) :: cgbohm,rltecrit, rlticrit, rlnecrit, drlne, drlte, drlti, j1, j2
  real(kind(1.0d0)), dimension(nxt) :: chie, chii, Dn, Vn
  real(kind(1.0d0)), dimension(nx) :: CHI,CHE,DIF,VIN, &
  & DPH,DPL,DPR,XTB,EGM,GAM,GM1,GM2,OM1,OM2,FR1
  real(kind(1.0d0)), dimension(nxt, nchan) :: rlx

! a definition
		rlx = gy0





!below: choice of model


  if (i_modeltype.eq.1.or.i_modeltype.eq.555) then
 !this model assumes that H factor is give. Shape of coefficients is similar to TGLF, however can be improved a bit

	cgbohm=3.236*y0(:,2)**(1.5d0)*sqrt(AMJ)/(btor**2.*rmajor) !gB factor

	chie = 0.01d0 + 1.05d0*cgbohm*(3.13+(xtr/amin)**0.3d0)*(max(0.,rlx(:,2)-5.))**1.d0*q_tr**2.  !electron chi

	if (i_modeltype.eq.1) then
		chii = 2.*chie
 !ion chi
	else
		chii = xihepalmod*chie
 !ion chi
	endif

	Dn = 0.5d0 * (chii+chie) * 0.8d0
 !particle D

	if (i_modeltype.eq.1) then
		Vn = -Dn/rmajor * (0.25d0*rlx(:,2)+0.5) * sqrt(xtr/amin)
 !particle V
	else
		Vn = -Dn/rmajor * npikpalmod * &
		& sqrt(xtr/amin)/sqrt(xtr(nint((0.0001+nxt)/2.))/amin)
	endif

!transfer
     a(:,1)=chifac*Dn
     a(:,2)=chifac*chie
     a(:,3)=chifac*chii
     b(:,1)=chifac*Vn
     b(:,2)=0.0d0*Dn
     b(:,3)=0.0d0*Dn

  endif









  if (i_modeltype.eq.111) then
 !this model is a trial on gB transport
!write(*,*) y0(:,2)**1.5d0,(0.03+(xtr/amin)**3.d0)
!stop 1
	cgbohm=3.236*y0(:,2)**(1.5d0)*sqrt(AMJ)/(btor**2.*rmajor)

     chie = 0.01d0 + 0.25d0*cgbohm*(3.13+(xtr/amin)**0.3d0)*(rlx(:,2)/10.d0)**1.d0*q_tr**1.
!	write(*,*) chie
     chii = 2.*chie
     Dn = 0.5d0 * (chii+chie) * 0.8d0
     Vn = -Dn/rmajor * (0.25d0*rlx(:,2)+0.5) * sqrt(xtr/amin)

     a(:,1)=1.*Dn
     a(:,2)=1.*chie
     a(:,3)=1.*chii

     b(:,1)=1.*Vn
     b(:,2)=0.0d0*Dn
     b(:,3)=0.0d0*Dn

  endif








  if (i_modeltype.eq.2) then
 !another crappy model
!write(*,*) y0(:,2)**1.5d0,(0.03+(xtr/amin)**3.d0)
!stop 1
	cgbohm=3.236*y0(:,2)**(1.5d0)*sqrt(AMJ)/(btor**2.*rmajor)

     chie = 0.01d0 + 1.05d0*cgbohm*(0.03+(xtr/amin)**0.3d0)*(max(0.d0,rlx(:,2)-7.d0))**4.d0*q_tr**2.
!	write(*,*) chie
     chii = 2.*chie
     Dn = 0.5d0 * (chii+chie) * 0.8d0
     Vn = -Dn/rmajor * (0.25d0*rlx(:,2)+0.5) * sqrt(xtr/amin)

     a(:,1)=Dn
     a(:,2)=chie
     a(:,3)=chii

     b(:,1)=Vn
     b(:,2)=0.0d0*Dn
     b(:,3)=0.0d0*Dn

  endif







!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



!end transport model
end subroutine trmodel
