!transport model

subroutine trmodel(i_modeltype,nx,nxt, nchan, & !input
  VR,AMJ,ZMJ,AIM1,AIM2,AIM3, & ! input
  NE,TE,NI,NDEUT,NIZ1,TI,ZEF,ZIM1,AMAIN,MU,RHO,AMETR, & ! input
  SHIF,ELON,TRIA,VPOL,VTOR,ER,NIBM,IPOL,G11,VRS,GRADRO, &  ! input
  SHEAR,PBLON,PBPER,PFAST,NIZ3,ZIM2,ZIM3, & ! input
  y0, gy0, xtr,x, amin, rmajor, btor,capA, q_tr, sh_tr, &! input
  a, b, & !output
  Hfactor,chi00,chipow,Hnow,chifac) !input
  
  use grad_func
  implicit none
  
  !  Arguments
  
  integer, intent(in) :: nxt, nchan, i_modeltype,nx
  real(kind(1.0d0)), dimension(nxt, nchan), intent(in) :: gy0, y0
  real(kind(1.0d0)), dimension(nxt), intent(in) :: xtr, q_tr, sh_tr
  real(kind(1.0d0)), intent(in) :: amin, rmajor, capA,btor
  
  real(kind(1.0d0)), dimension(nxt, nchan), intent(out) :: a, b
  

  real(kind(1.0d0)), dimension(nx), intent(in) :: VR, & 
  & NE,TE,NI,NDEUT,NIZ1,TI,ZEF,ZIM1,AMAIN,MU,RHO,AMETR, &
  & SHIF,ELON,TRIA,VPOL,VTOR,ER,NIBM,IPOL,G11,VRS,GRADRO, & 
  & SHEAR,PBLON,PBPER,PFAST,NIZ3,ZIM2,ZIM3,x
  
  real(kind(1.0d0)), intent(in) :: AMJ,ZMJ,AIM1,AIM2,AIM3
  
  real(kind(1.0d0)), intent(in) :: Hfactor,chi00,chipow,Hnow,chifac
  
  !  Local variables
  
  real(kind(1.0d0)), dimension(nxt) :: cgbohm,rltecrit, rlticrit, rlnecrit, drlne, drlte, drlti, j1, j2
  real(kind(1.0d0)), dimension(nxt) :: chie, chii, Dn, Vn
  real(kind(1.0d0)), dimension(nxt, nchan) :: rlx
  
  integer :: IS,IE,NA1,NA1N,NA1E,NA1I,j,jmin
  real(kind(1.0d0)) :: TIME, &
  &		HRO,HROA,ROC,RTOR
    
  real(kind(1.0d0)), dimension(nx) :: CHI,CHE,DIF,VIN, &
  & DPH,DPL,DPR,XTB,EGM,GAM,GM1,GM2,OM1,OM2,FR1
  
  rlx = gy0
  
  
  !test model for testing purposes, very crappy physics (but very nonlinear!)


  if (i_modeltype.eq.1) then
!write(*,*) y0(:,2)**1.5d0,(0.03+(xtr/amin)**3.d0)
!stop
	cgbohm=3.236*y0(:,2)**(1.5d0)*sqrt(AMJ)/(btor**2.*rmajor)

     chie = 0.01d0 + 1.05d0*cgbohm*(0.03+(xtr/amin)**0.3d0)*(rlx(:,2)/10.d0)**4.d0*q_tr**2. 
!	write(*,*) chie
     chii = 2.*chie
     Dn = 0.5d0 * (chii+chie) * 0.8d0
     Vn = -Dn/rmajor * (0.25d0*rlx(:,2)+0.5) * sqrt(xtr/amin)

     a(:,1)=chifac*Dn
     a(:,2)=chifac*chie
     a(:,3)=chifac*chii

     b(:,1)=chifac*Vn
     b(:,2)=0.0d0*Dn
     b(:,3)=0.0d0*Dn

  endif



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  
  !...
  if (i_modeltype.eq.2) then
!write(*,*) y0(:,2)**1.5d0,(0.03+(xtr/amin)**3.d0)
!stop
	cgbohm=3.236*y0(:,2)**(1.5d0)*sqrt(AMJ)/(btor**2.*rmajor)

     chie = 0.01d0 + 1.05d0*cgbohm*(0.03+(xtr/amin)**0.3d0)*(max(0.d0,rlx(:,2)-7.d0))**4.d0*q_tr**2. 
!	write(*,*) chie
     chii = 2.*chie
     Dn = 0.5d0 * (chii+chie) * 0.8d0
     Vn = -Dn/rmajor * (0.25d0*rlx(:,2)+0.5) * sqrt(xtr/amin)

     a(:,1)=chifac*Dn
     a(:,2)=chifac*chie
     a(:,3)=chifac*chii

     b(:,1)=chifac*Vn
     b(:,2)=0.0d0*Dn
     b(:,3)=0.0d0*Dn

  endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





  
  
  !...
  if (i_modeltype.eq.3) then
     
     do j=1,nxt
        
        !	write(*,*) xtr(j),xtr(j)/amin
	jmin=minloc(abs(x-xtr(j)/amin),1)		
 !	write(*,*) jmin,x(jmin)
        
	IS=jmin-2
	IE=jmin+2
	
	NA1=nx
	NA1N=NA1
	NA1E=NA1
	NA1I=NA1
	TIME=0.
	HRO = (x(2)-x(1))*amin
	HROA = (x(2)-x(1))*amin
	ROC=amin
	
        !write(333,*) IS,IE,NA1,NA1N,NA1E,NA1I,TIME, &
        !& HRO,HROA,ROC,BTOR,rmajor,VR,AMJ,ZMJ,AIM1,AIM2,AIM3, & 
        !& NE,TE,NI,NDEUT,NIZ1,TI,ZEF,ZIM1,AMAIN,MU,RHO,AMETR, &
        !& SHIF,ELON,TRIA,VPOL,VTOR,ER,NIBM,IPOL,G11,VRS,GRADRO, & 
        !& SHEAR,PBLON,PBPER,PFAST,NIZ3,ZIM2,ZIM3,CHI,CHE,DIF,VIN, &
        !& DPH,DPL,DPR,XTB,EGM,GAM,GM1,GM2,OM1,OM2,FR1	
        
        !		call ATGLFP(IS,IE,NA1,NA1N,NA1E,NA1I,TIME, &
        !   &		HRO,HROA,ROC,BTOR,rmajor,VR,AMJ,ZMJ,AIM1,AIM2,AIM3, & 
        !			& NE,TE,NI,NDEUT,NIZ1,TI,ZEF,ZIM1,AMAIN,MU,RHO,AMETR, &
        !			& SHIF,ELON,TRIA,VPOL,VTOR,ER,NIBM,IPOL,G11,VRS,GRADRO, & 
        !			& SHEAR,PBLON,PBPER,PFAST,NIZ3,ZIM2,ZIM3,CHI,CHE,DIF,VIN, &
        !			& DPH,DPL,DPR,XTB,EGM,GAM,GM1,GM2,OM1,OM2,FR1	)
        
        
        a(j,1)=abs(DIF(jmin))+0.02
        a(j,2)=abs(CHE(jmin))+0.01
        a(j,3)=abs(CHI(jmin))+0.05
        
        b(j,1)=VIN(jmin)
        b(j,2)=0.0d0*a(j,1)
        b(j,3)=0.0d0*a(j,1)
        
	
        
     enddo
     
  endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
31  continue
    goto 10
    open(99,file='./trmodel.chartst',STATUS='UNKNOWN',Access = 'append')
    write(99,*)   nxt, nchan, i_modeltype,nx
    write(99,*)   gy0(nxt, nchan), y0(nxt, nchan)
    write(99,*)   xtr(nxt), q_tr(nxt), sh_tr(nxt)
    write(99,*)   amin, rmajor, capA,btor
  
    write(99,*)   a(nxt, nchan), b(nxt, nchan)
  
  !  Local variables
  
    write(99,*)   rltecrit(nxt), rlticrit(nxt), rlnecrit(nxt), drlne(nxt), drlte(nxt), drlti(nxt), j1(nxt), j2(nxt)
    write(99,*)   chie(nxt), chii(nxt), Dn(nxt), Vn(nxt)
    write(99,*)   rlx(nxt, nchan)
  
    write(99,*)   IS,IE,NA1,NA1N,NA1E,NA1I,j,jmin
    write(99,*)   TIME, &
  &		HRO,HROA,ROC,RTOR
  
    write(99,*)   VR(nx), & 
  & NE(nx),TE(nx),NI(nx),NDEUT(nx),NIZ1(nx),TI(nx),ZEF(nx),ZIM1(nx),AMAIN(nx),MU(nx),RHO(nx),AMETR(nx), &
  & SHIF(nx),ELON(nx),TRIA(nx),VPOL(nx),VTOR(nx),ER(nx),NIBM(nx),IPOL(nx),G11(nx),VRS(nx),GRADRO(nx), & 
  & SHEAR(nx),PBLON(nx),PBPER(nx),PFAST(nx),NIZ3(nx),ZIM2(nx),ZIM3(nx),x(nx)
  
    write(99,*)   AMJ,ZMJ,AIM1,AIM2,AIM3
  
    write(99,*)   Hfactor,chi00,chipow,Hnow,chifac
  
  
    write(99,*)   CHI(nx),CHE(nx),DIF(nx),VIN(nx), &
  & DPH(nx),DPL(nx),DPR(nx),XTB(nx),EGM(nx),GAM(nx),GM1(nx),GM2(nx),OM1(nx),OM2(nx),FR1(nx)
    close(99)  
10  continue  
end subroutine trmodel
