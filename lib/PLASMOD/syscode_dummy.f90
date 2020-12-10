

  program main
  
    use grad_func
    use structs
    implicit none
      
    ! solver controls  
    
    real(kind(1.0D0)) :: tol, dtmin, dtminmin, dtminmax, dtinc, tolopt, etol, eopt, dtmin0, dt0
    real(kind(1.0d0)) :: dtmax, dtmaxmin, dtmaxmax, tolmax, tolmin, dgy, etol0, etolm, dtmax0
    integer :: jiter, nitermax, Pf0, jipperdo, jipper, redo, jnit, redo0,i_modeltype,i_equiltype
    real(kind(1.0d0)) :: rminor,ng,pi
    real(kind(1.0d0)),dimension(8) :: rscan
				real(kind(1.0d0)),dimension(7) :: btscan
    real(kind(1.0d0)),dimension(8,7) :: fusscan,vlopscan,volscan,hscan,nbiscan
    
    ! grid
    
 !   integer, parameter :: nx = 41, nxt = 5, nchannels = 3
    
    ! others
    
    ! set physics constants
    
    real(kind(1.0d0)) :: Hfactor,chi00,chipow,Hnow,tau_scal,chifac,chifac0
    
				integer :: i_flag,jloop,j1scan,j2scan
				
    type (geometry) :: geom
    type (composition) :: comp
    type (pedestal) :: ped
    type (inputs) :: inp0
    type (radial_profiles) :: radp
    type (MHD_EQ) :: mhd
    type (power_losses) :: loss
    type (numerics_transp) :: num



	rscan=(/8.7,8.8,8.9,9.,9.1,9.2,9.3,9.4/)
	btscan=(/5.5,5.6,5.7,5.8,5.9,6.,6.1/)

    
    
    ! Initialise plasma stuff -- to be obtained from rest of PROCESS!
				pi=3.141592
	include 'defaults_inputs.f90'				
	mhd%equilcheck =0.d0    
				
				!	pause
    ! set up grid

!	open(32,file='inpuz.dat')
!	read(32,*) geom%R,geom%bt
!	close(32)



	do j2scan=4,4
	do j1scan=4,4
	call system('rm tglfou*')
	write(*,*) j1scan,j2scan
!    ped%teped=5.5  !pedestal top temperature
!    ped%tesep=0.3  !separatrix temperature
! geom%R=rscan(j1scan)
!	geom%bt=btscan(j2scan)
  	num%dt=num%dtmin
!					inp0%qnbi_psepfac=50. !dqnbi/d(1-Psep/PLH)
!				inp0%cxe_psepfac=1.e-4 !dcxe/d(1-Psep/PLH)
!				inp0%car_qdivt=1.e-6 !dcar/d(qdivt)

!    geom%R =rscan(j1scan)  ! major radius in m
!    geom%bt = btscan(j1scan) !magnetic field

!				inp0%f_ni=0. !required fraction of non inductive current, if 0, dont use CD
!				inp0%Hfac_inp=1. !input H factor, if 0., this is not used. This is radiation corrected H factor
!				geom%q95 = 3.5 !safety factor. 
								
!write(*,*) 'first iteration'
	include 'DEMOlike.dat'				
	include 'ITERlike.dat'				
	include 'DEMO1.dat'				
	include 'FPALSCANS.dat'				
	include 'DEMOlike.dat'				
!pause
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
!stop 1
!write(*,*) 'num',num
!write(*,*) 'geom',geom
write(*,*) 'comp',comp
write(*,*) 'ped',ped
!write(*,*) 'inp0',inp0
!write(*,*) 'radp',radp
write(*,*) 'mhd',mhd
write(*,*) 'loss',loss
write(*,*) ' '
write(*,*) 'Pfus/V '
write(*,*) loss%Pfus/mhd%vp,radp%zeff,mhd%vloop

fusscan(j1scan,j2scan)=loss%Pfus
vlopscan(j1scan,j2scan)=mhd%vloop
volscan(j1scan,j2scan)=mhd%vp
hscan(j1scan,j2scan)=loss%H
nbiscan(j1scan,j2scan)=loss%pnbi

	open(32,file='fort.9911')
	write(32,'(722E25.11)') rscan,btscan,fusscan,& 
	& vlopscan,volscan,hscan,nbiscan
	close(32)

enddo
enddo




stop 1








stop 1
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
write(*,*) 'num',num
write(*,*) 'geom',geom
write(*,*) 'comp',comp
write(*,*) 'ped',ped
write(*,*) 'inp0',inp0
!write(*,*) 'radp',radp
write(*,*) 'mhd',mhd
write(*,*) 'loss',loss
write(*,*) ' '
write(*,*) 'Pfus/V '
write(*,*) loss%Pfus/mhd%vp
stop 1
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)

stop 1
	! i_flag = 1 (OK) , 0 (crashed) , -1 (not converged, max iterations reached) , -2 (equilibrium not converged)
write(*,*) 'second iteration',i_flag

	if (i_flag.ne.1) then
	write(*,*) j1scan,'not converged, redo'
	pause
	include 'deallocate_all.f90'
	include 'defaults_inputs.f90'
!	num%dt=0.0001
	loss%pnbi=0.d0
    geom%R =rscan(j1scan)  ! major radius in m
    geom%bt = btscan(j1scan) !magnetic field
	mhd%equilcheck=0.d0



	call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)

	
	endif


write(*,*) 'num',num
write(*,*) 'geom',geom
write(*,*) 'comp',comp
write(*,*) 'ped',ped
write(*,*) 'inp0',inp0
!write(*,*) 'radp',radp
write(*,*) 'mhd',mhd
write(*,*) 'loss',loss

write(1555,*) 'num',num
write(1555,*) 'geom',geom
write(1555,*) 'comp',comp
write(1555,*) 'ped',ped
write(1555,*) 'inp0',inp0
!write(*,*) 'radp',radp
write(1555,*) 'mhd',mhd
write(1555,*) 'loss',loss





	!write outputs
! geom%ip --> if i_equiltype = 1, ip is an output

! radp%x --> normalized minor radius 
! radp%ne --> electron density profiule in 10^19 m^-3 
! radp%te --> electron temperature profile in kev
! radp%ti --> ion temperature profile in kev

! ped%teped --> temperature pedestal top if ipedestal = 2
! ped%nped --> density pedestal top

! mhd%betan --> beta_N

! mhd%vp --> plasma volume in m^3
! mhd%sp --> plasma lateral surface in m^2
! mhd%q --> q_95
! mhd%ip_out --> plasma current in MA
! mhd%vloop --> loop voltage in Volts
! mhd%fbs --> bootstrap cfraction
! mhd%f_ni --> non inductive fraction (bs + cd)

! inp0%qheat --> MW for heating
! inp0%qcd --> MW for current drive
! inp0%qfus --> MW for fusion power control

! loss%psep --> net separatrix power: Paux+Pfus-Prad
! loss%prad --> net radiated power: line + brad + sync
! loss%wth --> plasma thermal energy in MJ
! loss%taueff --> plasma confinement time in s

! loss%pdiv --> divertor power in MW/m^2
! loss%Hcorr --> H factor with radiation correction	

! loss%pradcore --> core radiation in MW
! loss%pradedge --> edge radiation in MW	
! loss%psepe --> electron separatrix power
! loss%psync --> synchrotron radiation in MW
! loss%pbrehms --> brehm radiation in MW
! loss%pline --> line radiation in MW
! loss%Dfuelreq --> required influx of D in p/s
! loss%Tfuelreq --> required influx of T in p/s

write(*,*) ' geom%ip   ped%teped ped%nped   mhd%betan  mhd%vp'
write(*,'(911E25.11)')  geom%ip ,  ped%teped ,ped%nped  , mhd%betan , mhd%vp

write(*,*) 'mhd%sp , mhd%q 	,mhd%ip_out,	mhd%vloop ,	mhd%fbs, 	mhd%f_ni	'			 
write(*,'(911E25.11)')  mhd%sp , mhd%q 	,mhd%ip_out,	mhd%vloop ,	mhd%fbs, 	mhd%f_ni				 

write(*,*) 'inp0%qheat  ,inp0%qcd ,  inp0%qfus   '
write(*,'(911E25.11)')  inp0%qheat  ,inp0%qcd ,  inp0%qfus   

write(*,*) ' loss%psep  ,  loss%prad   ,loss%wth ,loss%taueff , loss%pdiv   , loss%Hcorr , loss%pfus  , loss%chifac0' 
write(*,'(911E25.11)')   loss%psep  ,  loss%prad   ,loss%wth ,loss%taueff , loss%pdiv   , loss%Hcorr, loss%pfus  , loss%chifac0


write(774,'(911E25.11)') geom%R,geom%bt,loss%pfus,loss%qheat+loss%qfus+loss%qcd,comp%cxe,comp%che
write(*,*) "geom%R,geom%bt,loss%pfus,loss%qheat+loss%qfus+loss%qcd,comp%cxe,comp%che"
write(*,'(911E25.11)') geom%R,geom%bt,loss%pfus,loss%qheat+loss%qfus+loss%qcd,comp%cxe,comp%che


write(*,*) (mhd%f_ni-mhd%fbs)*geom%ip/loss%pnbi*1e3

!pause

!enddo



    
  end

