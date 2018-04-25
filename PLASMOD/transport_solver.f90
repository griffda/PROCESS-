! This code is called PLASMOD, copyright E Fable 2018
!
! Disclaimer: results obtained with this code should be referenced as:
! E. Fable et al., Fusion Eng. and Design, to be published (2018)


! MAIN CALL!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)

    use grad_func
    use structs
    use physics_functions_module

    implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!









!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!explanation of i_flag and what to do in case:
!
!
! i_flag = 1 --> converged, no problems
!
! i_flag = -1 --> max iterations reached. 
! 				Reasons: * max iterations too low, increase them
!													 * equilibrium oscillating between solutions --> pressure too high, reduce H factor
!              * solution oscillating, reduce time step dtmax and/or dtmin
!
! i_flag = 0 --> transport solver crashed. 
!					Reasons: * strange paramaters. Check the inputs if something is abnormal
!              * too large time step, reduce dtmax (and/or dtmin)
!
! i_flag = -2 --> equilibrium crashes. 
!					Reasons: * too strange q profile --> advanced scenarios not feasible for now
!              * too high pressure, reduce H factor
!              * negative temperatures or densities or some other strange parameters, reduce time step
!
! Basically, first solution is reduce time step. If this doesnt work, try changing the parameters, above all H factor 
!              
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!










! DECLARATION OF VARIABLES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type (geometry), intent(inout) :: geom
  type (composition), intent(inout) :: comp
  type (pedestal), intent(inout) :: ped
  type (inputs), intent(inout) :: inp0
  type (radial_profiles), intent(inout) :: radp
  type (MHD_EQ), intent(inout) :: mhd 
  type (power_losses), intent(inout) :: loss 

  ! num                                       :  input  type (numerics_transp) :: solver controls 
  type (numerics_transp), intent(inout) :: num   !, intent(in) ::   num !to numerics.f90


  integer :: i_flag,jiter, nitermax, jipperdo, jipper, redo, jnit,i_modeltype,i_equiltype

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

















! INTERNAL VARIABLES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		logical :: filetglf
		character*80 :: fnamez(num%nxt)
  integer :: jipperdo2,jipper2,j
  integer :: i, gippa, g_info, g_count
  integer, dimension(num%nxt,num%nchannels) :: indxx
  integer :: jiterext, jiterextmax, jrad,jrad1,jrad2
  integer :: j1,j2    , irho
  integer :: j_aux
  integer :: jped,nx,nxt,nchannels, iped_model,jdum1,nxequil,i_qsaw, i_diagz
  real(kind(1.0d0)) :: x0, dx, dxn,psep,plh
  real(kind(1.0d0)) :: Qeaux, Qiaux, Qrad ,qradedge, betan,P_pedtop, qecrh, qnbi, spellet, spuffing, f_gw !sfuelling
  real(kind(1.0d0)) :: nsep, Ip, btor, cHe,che3,cprot, & 
		 & cxe,car,nG, qedge, elong, trianpg, amin ,tsep, rpmajor,  rpminor, asppect, nlineavg 
  real(kind(1.0d0)) :: tau_sol, V_sol, D_ped, V_ped, lambda_sol,pdtp,sv_dd,svdt
  real(kind(1.0d0)) :: rtor, yd, betaz, lint,taue,Qtot
  real(kind(1.0d0)) :: Hfactor,chi00,chipow,Hnow,tau_scal,chifac,chifac0
  real(kind(1.0d0)) :: paion, NALPH,YVALP,YLLAME,yllami,yllama,YY6,YEPS,YVC, YY7,yv7 ,yv6 !fraction of D-T power deposited to ions, plus dummies
  real(kind(1.0d0)) :: ts_alf,chepck,dum1,dum2,roc0,vloop0,fbs0,toleq0,pow_eq   !fraction of D-T power deposited to ions, plus dummies
  real(kind(1.0d0)) :: aim1,aim2,aim3   !fraction of D-T power deposited to ions, plus dummies
  real(kind(1.0d0)) :: q_edge_in,f_ind_in,ip0 ,tepp0,tipp0,nepp0,fq  !fraction of D-T power deposited to ions, plus dummies
  real(kind(1.0d0)) :: elong95,trianpg95  !fraction of D-T power deposited to ions, plus dummies
  real(kind(1.0d0)) :: xb,teb,tib,neb,zmain,amain,toleq,fuelmix
  real(kind(1.0d0)) :: roc,vloop,fbs,qf,qf0,sfus_he,sfus_he3,sfus_p,fcd,qdivt,q_heat,q_cd,q_fus,q_95,qtote,qtoti,w_e,w_i
  real(kind(1.0d0)) :: lambda_q,lparsep,ldiv,qpar,fx, t_plate,pres_fac,areat,plinexe,psepxe
  real(kind(1.0d0)) :: ne_av,nela,PLH_th(8)
  real(kind(1.0d0)), dimension(num%nx) :: theta_perim,dtheta,f_perim,p_dd
  real(kind(1.0d0)), dimension(num%nx) :: x, tepr, tipr, nepr, qinit, xr, Peaux, Piaux, nHe,nprot,nhe3, nXe, nNe, prxe, prne
  real(kind(1.0d0)), dimension(num%nx) :: pech,pnbi,psync,pbrad
  real(kind(1.0d0)), dimension(num%nx) :: zavxe, zavne, prad,pradtot,pradedge, ndeut, ntrit, nions, pedt, pidt, peicl, zepff!  conflict with   
  real(kind(1.0d0)), dimension(num%nxt+1) :: T_e, T_i, N_e, gT_e, gT_i, gn_e
	 real(kind(1.0d0)), dimension(num%nxt+1) :: T_e0, T_i0, N_e0, xtrt, Fn0, Fe0, Fi0
  real(kind(1.0d0)), dimension(num%nxt+1) :: Fn, Fe, Fi
  real(kind(1.0d0)), dimension(num%nxt) :: xtr
  real(kind(1.0d0)), dimension(num%ntglf,num%nchannels) :: atglf,btglf
  real(kind(1.0d0)), dimension(num%nxt) :: q_tr, sh_tr
  real(kind(1.0d0)), dimension(num%nxt) :: gng, geg, gig, gng0, geg0, gig0, DdnVne, DdnVni, DdnVn
  real(kind(1.0d0)), dimension(num%nxt+1) :: gn_e0, gT_e0, gT_i0
  real(kind(1.0d0)), dimension(num%nxt,num%nchannels) :: y, gy, ay, by, cy, a, b, aa,bb,c, chat, gyhat
  real(kind(1.0d0)), dimension(num%nxt,num%nchannels) :: a_neo,b_neo
  real(kind(1.0d0)), dimension(num%nxt,num%nchannels) :: qy, qy0, gy0, qym, gym
  real(kind(1.0d0)), dimension(num%nxt,num%nchannels) :: cym, cy0, y0, ym, F, F0, Fm
  real(kind(1.0d0)), dimension(num%nxt*num%nchannels,num%nxt*num%nchannels) :: jacob, ijacob
  real(kind(1.0d0)), dimension(num%nxt*num%nchannels) :: Fvec, g0vec, gyvec
  real(kind(1.0d0)), dimension(num%nx) :: pressure,k, d, jcdr, shif, G1, G2, G3, dV, phi, rho, V, psi, ipol, &
		&  Vprime, jpar, qprf, qprf0
  real(kind(1.0d0)), dimension(num%nx) :: pressure0,k0, d0, shif0, G10, dV0, phi0, rho0, V0, psi0, ipol00, Vprime0, jpar0,gradro0 !   conflict with process
  real(kind(1.0d0)), dimension(num%nx) :: shear, W, mux, hro, ametr, nel, tel, tii, nii, ya
  real(kind(1.0d0)), dimension(num%nx) :: q_oh,sqeps, dlogte, dlogti, dlogne, dpsi, betpl, betple, nalf, droda
  real(kind(1.0d0)), dimension(num%nx) :: nuee, nues, zavg, nui, nuis, coulg, cc, tpf, cubb
  real(kind(1.0d0)), dimension(num%nx) :: zz, zft, zdf, dcsa, hcee, hcei, hcsa, a0, alp, a1, xcsa !
  real(kind(1.0d0)), dimension(num%nx) :: zfte, zfte2, zfte3, zfte4,powe,powi,sfuel
  real(kind(1.0d0)), dimension(num%nx) :: zfti, zfti2, zfti3, zfti4,totse,totsi !
  real(kind(1.0d0)), dimension(num%nx) :: palpph, G30, G20, ipol0, vp0, pfus,sn,tots
  real(kind(1.0d0)), dimension(num%nx) :: chie,chii,dnn,vvn,eqpf,eqff,gradro,jpol,nbi_split
  real(kind(1.0d0)), dimension(num%nxt,num%nchannels) :: gytemp,ytemp
  real(kind(1.0d0)), dimension(num%nxt*num%nchannels) :: F1vec,F2vec
  real(kind(1.0d0)) :: tup_0d,fm_SE,Lr,tguess,tau_relax,tshguess,tupguess,fmguess
  real(kind(1.0d0)) :: dqoqguess,tdivguess,tau,p_sep,ds2,dd2,ds3,dd3,ds1,dd1
  ! set physics constants
  real(kind(1.0d0)), parameter :: clight = 2.9979d+18, e_charge = 1.602d-19, eps_vacuum = 8.854d-12
  real(kind(1.0d0)), parameter :: m_electron = 9.109d-31, m_proton = 1.673d-27, mu_vacuum = 1.2566d-6
  real(kind(1.0d0)), parameter :: pe_mratio = 3438.0d0, planckh = 6.63d-34 , pi_g = 3.141592

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


















! PRE-INITIALIZATION!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
pres_fac=1.d0 !pressure scaling coefficient to avoid emeq crashing, see inside equil.f90


!diagnostic switch
 i_diagz=nint(num%maxa)
!succes flag
	i_flag=1 !success

  ! engineering data from input structures to local variables 
  rpmajor = geom%R
  asppect = geom%A
  rpminor = rpmajor/asppect
  elong = geom%k
  trianpg = geom%d
  elong95 = geom%k95
  trianpg95 = geom%d95
  Ip = geom%Ip !MA
  btor = geom%Bt
 	fuelmix=comp%fuelmix
		q_edge_in=geom%q95*1.1
  q_95 = geom%q95
  amin = rpminor
  Hfactor = inp0%hfac_inp
  Hnow = Hfactor

  !this counter checks if transport is called the first time or not (to improve speed reusing previous iterations results)
  geom%counter=min(geom%counter,3.d0)	

  jiterext=2  !when 2, this means it has already been run before

  if (.not.allocated(radp%ne).or..not.allocated(radp%g2))    jiterext=1 !if nothing allocated, must be first call ever

  if (geom%counter.eq.0.d0)   jiterext=1 !if geom%counter 0 --> first call ever

  !check if the machine has changed, major radius, current, anything
  chepck=0.d0
  if (geom%counter.ge.1.d0) then
     chepck = abs(rpmajor-geom%Rold)/rpmajor+ & 
          & abs(asppect-geom%aold)/rpmajor+ & 
          & abs(elong-geom%kold)/elong+ & 
          & abs(trianpg-geom%dold)/0.2+ & 
          & abs(geom%q95-geom%q95old)/geom%q95+ & 
          & abs(btor-geom%btold)/btor
  endif
!	write(*,*) chepck,geom%counter,jiterext

  if (geom%counter.ge.1..and.chepck.gt.0.1) then
   ! if machine has changed, restart from scratch guess
   write(*,*) 'machine has changed'
   jiterext=1
  endif

!write(*,*) 'PLASMOD RUNS'

  if (mhd%equilcheck.eq.0.d0) jiterext=1 !if equil crashed before, restarts. Dont know if to keep this in the future

  !assign some numerics
  nx = num%nx
  nxt=num%nxt
  nchannels=num%nchannels
  nxequil=nx
  iped_model=   (num%ipedestal)
  i_modeltype = (num%i_modeltype)  
  i_equiltype = (num%i_equiltype)   
  nitermax = nint(num%test)
  pow_eq = num%etol0
  i_qsaw=nint(num%maxA)
  jiter = 0
  redo = 0
  jnit = 0
  jiterextmax = 1
		num%dt=num%dtmin

  !impurities mass ( to be substituted later by PROCESS functions)
  aim1=4.d0 !helium
  aim2=48.d0 !Ar
  aim3=131.d0 !Xe
  zmain = 1.0d0  !to change this to comp%zmain
  amain = 2*fuelmix+3*(1.-fuelmix) !D+T
		!impurity concentrations
		che=comp%comparray(2)
  cxe=comp%comparray(13)
		car=comp%comparray(9)

  ! set transport boundary conditions
  xb = ped%rho_T !rho_n : normalized minor radius
  teb = ped%teped
  tib = teb
  tsep = ped%tesep

  !Setup plasma current from some formula, but really this will be changed by the code anyway, but keep this
	if (jiterext.eq.1) then
  fq = 0.5D0 * (1.17D0-0.65D0*1.d0/asppect)/((1.0D0-1.d0/asppect*1.d0/asppect)**2) * &
       (1.0D0 + elong95**2 * &
       (1.0D0 + 2.0D0*trianpg95**2 - 1.2D0*trianpg95**3) )
  if (i_equiltype.eq.1) then
   ip =5.d0*rpminor**2.d0/(rpmajor*q_95)*fq*btor !need to put an actual scaling here...
  endif
  nG = 10.0d0 * Ip/(pi_g * rpminor**2)  ! 10^19
  neb = inp0%f_gw*nG
  nsep = inp0%f_gws*nG
		ped%nped=neb
		ped%nsep=nsep
	endif	

  ! set up transport and reduced grids
  dx = 1.0d0/(nx-1.d0)
  dxn = dx
  x = (/ (dx*(irho-1.), irho = 1, nx) /)
  x0=max(num%capa,x(2)) !this maybe revisited ... EFable
 	jped=nint(xb/dx) !pedestal top position
  xtr = linspace(x0, xb, nxt) * amin  ! normalized minor radius for transport
  xtrt(1:nxt) = xtr/amin
  xtrt(nxt+1) = 1.0d0
  xtrt = xtrt*amin
  xr = x*amin
  dx = xtr(2)-xtr(1)

  !initial conditions from scratch
  T_e(nxt) = teb
  T_i(nxt) = tib
  N_e(nxt) = neb
  T_e(nxt+1) = tsep
  T_i(nxt+1) = tsep
  N_e(nxt+1) = nsep
  T_e0 = teb
  T_i0 = teb
  N_e0 = neb
  T_e0(nxt+1) = tsep
  T_i0(nxt+1) = tsep
  N_e0(nxt+1) = nsep
  gT_e = 0.0d0
  gT_i = 0.0d0
  gn_e = 0.0d0
		qedge=q_95*1.1
  qinit = 1.0d0 + (qedge-1.0d0) * x**4
  qprf0 = qinit
  Qrad=0.0d0    
  palpph = 0.0d0 * nions
  betaz = 0.01d0
  lint = 0.7d0
  G30 = 1.d0*nepr
  G20 = 0.d0*nepr
  ipol0 = 0.d0*nepr
  vp0 = 0.d0*nepr
  cc = 0.0*tepr+1.0d0
  cubb =0.d0*nepr
  jcdr =0.d0*nepr
  toleq0=100.
  toleq=100.0d0    
		chifac0=1.d0 !for h factor normalization

  ! Initial conditions
  if (jiterext.eq.1) then !this is generic guess.
   T_e0(1:nxt) = 5.d0*teb*(1.0d0-xtr/xtr(nxt))+teb
   T_i0 = T_e0 
   gn_e(1:nxt) = -rpmajor * gradient(log(N_e0(1:nxt)),xtr)
   gT_e(1:nxt) = -rpmajor * gradient(log(T_e0(1:nxt)),xtr)
   gT_i(1:nxt) = -rpmajor * gradient(log(T_i0(1:nxt)),xtr)
   gn_e0 = gn_e
   gT_e0(1:nxt) = -rpmajor * gradient(log(T_e0(1:nxt)),xtr)
   gT_i0(1:nxt) = -rpmajor * gradient(log(T_i0(1:nxt)),xtr)
 	 cxe=0.d0
			q_heat=inp0%qheat
			q_cd=inp0%qcd
			q_fus=inp0%qfus
  else !this below is previous result as new guess, speed up computation
   N_e0(1:nxt+1)=radp%Nepg(1:nxt+1)
   T_e0(1:nxt+1)=radp%Tepg(1:nxt+1)
   T_i0(1:nxt+1)=radp%Tipg(1:nxt+1)
   neb=N_e0(nxt)
   teb=T_e0(nxt)
   nsep=N_e0(nxt+1)
   tsep=T_e0(nxt+1)
   gT_e(1:nxt) = radp%gte(1:nxt)
   gT_i(1:nxt) = radp%gti(1:nxt)
   gN_e(1:nxt) = radp%gne(1:nxt)
   gn_e0 = gn_e
   gT_e0(1:nxt) = gt_e(1:nxt)
   gT_i0(1:nxt) = gt_i(1:nxt)
   chifac0 = loss%chifac0
			q_heat=loss%qheat
			q_cd=loss%qcd
			q_fus=loss%qfus
			qinit = radp%qprof
  end if

!update b.c. due to choice of guess
  teb = ped%teped
  tib = teb
  neb = ped%nped
  tsep = ped%tesep
  nsep = ped%nsep
  N_e0(nxt)=neb
  T_e0(nxt)=teb
  N_e0(nxt+1)=nsep
  T_e0(nxt+1)=tsep
  tib=teb

!set up initial data for simulation
  N_e = N_e0
  T_e = T_e0
  T_i = T_i0
  qprf0 = qinit
  qprf = qinit
  q_tr = interp1_ef(nx,nxt,x, qprf, xtr/xr(nx))
  sh_tr = interp1_ef(nx,nxt,x, shear, xtr/xr(nx))
  Fn0 = 0.0d0 * gn_e + 100.0d0
  Fe0 = 0.0d0 * gn_e + 100.0d0
  Fi0 = 0.0d0 * gn_e + 100.0d0
  Fn = 0.0d0
  Fe = 0.0d0
  Fi = 0.0d0
  gng = 0.0d0
  geg = 0.0d0
  gig = 0.0d0
  gng0 = 0.0d0
  geg0 = 0.0d0
  gig0 = 0.0d0
  y = 0.0d0
  gy = 0.0d0
  ay = 0.0d0
  by = 0.0d0
  cy = 0.0d0
  qy = 0.0d0
  qy0 = 0.0d0
  gy0 = 0.0d0
  qym = 0.0d0
  gym = 0.0d0
  y(:,1) = N_e0(1:nxt)
  y(:,2) = T_e0(1:nxt)
  y(:,3) = T_i0(1:nxt)
  gy(:,1) = gn_e0(1:nxt)
  gy(:,2) = gT_e0(1:nxt)
  gy(:,3) = gT_i0(1:nxt)
  gy0 = gy
  gym = gy
  cym = gy
  cy0 = gy
  y0 = y
  ym = y
  F = 0.0d0
  F0 = F
  Fm = F
  jacob = eye2(nxt*nchannels)
  ijacob = jacob
		q_oh=0.d0
 	shif(nx)=0.d0

!call update_profiles to build up actual profiles from gradients
  call update_profiles(dx, nxt, nchannels, gy0, y0, rpmajor, y, N_e(1:nxt), T_e(1:nxt), T_i(1:nxt))
  y0 = y
  ym = y
  N_e0(1:nxt)=y0(:,1)
  T_e0(1:nxt)=y0(:,2)
  T_i0(1:nxt)=y0(:,3)
  N_e(1:nxt)=y0(:,1)
  T_e(1:nxt)=y0(:,2)
  T_i(1:nxt)=y0(:,3)
	
!equilibrium initialization
  if (jiterext.eq.1) then !first equilibrium from guess
    !first call ever
    call compute_equil( &
                                !input 
          nx,jiterext-1,i_equiltype, &
          x, tepr, tipr, nepr, nions, palpph, cc, G30,qprf0,G20,vp0, &
          rpmajor,rpminor,elong,trianpg,Ip,btor,betaz,lint,ipol0,e_charge,mu_vacuum, &
                                !inout
          qedge,&
                                !output
          roc, Vloop, fbs,fcd, toleq, &
          k, d, shif, cubb, jcdr, V, G1, G2, G3, dV, phi, qprf, rho, psi, jpar,&
          ipol, Vprime,droda,eqpf,eqff,gradro,q_edge_in,f_ind_in,q_95,elong95,trianpg95 &
          ,pres_fac,areat)
  else !equilibrium updated using previous result as new guess, faster
    call compute_equil( &
                                !input 
          nx, jiterext-jiterext+1,i_equiltype, &
          x, radp%te, radp%ti, radp%ne, radp%nions, radp%palph, radp%cc, radp%g3,radp%qprof,radp%g2,radp%vp, &
          rpmajor,rpminor,elong,trianpg,Ip,btor,betaz,lint,radp%ipol(nx),e_charge,mu_vacuum, &
                                !inout
          qedge,&
                                !output
          roc, Vloop, fbs,fcd, toleq, &
          k, d, shif, radp%jbs, radp%jcd, radp%Volum, G1, G2, G3, dV, phi, qprf, rho, radp%psi, jpar,&
          radp%ipol, Vprime,droda,eqpf,eqff,gradro,q_edge_in,f_ind_in,q_95,elong95,trianpg95 &
          ,pres_fac,areat)
    psi = radp%psi
    ipol=radp%ipol
    jcdr=radp%jcd
    palpph=radp%palph
    cc=radp%cc
    V=radp%volum
    q_oh=mhd%qoh
  endif

!update Greenwald density and profiles
  nG = 10.0d0 * Ip/(pi_g * rpminor**2)  ! 10^19
  nepr=interp1_ef(nxt+2,nx,[0.0d0, xtrt],[N_e(1), N_e],xr)
  tepr=interp1_ef(nxt+2,nx,[0.0d0, xtrt],[T_e(1), T_e],xr)
  tipr=interp1_ef(nxt+2,nx,[0.0d0, xtrt],[T_i(1), T_i],xr) !
  teb = ped%teped
  tib = teb
  nsep = inp0%f_gws*nG

  !initialization of counters & tolerance
  jiter=1
  jnit=0
  jipperdo = 100
  jipper = 1
  jipperdo2 = 100
  jipper2 = 1
  toleq = 100.0d0
  num%etol=100.d0
  num%etol0=100.d0
		num%etolm=100.d0

  if (i_diagz.eq.1) write(3301,*) ' nmew run ' 

!allocate radial profiles if not allocated
  if (.not.allocated(radp%ne))    ALLOCATE ( radp%ne(nx) )
  if (.not.allocated(radp%shif))    ALLOCATE ( radp%shif(nx) )
  if (.not.allocated(radp%k))    ALLOCATE ( radp%k(nx) )
  if (.not.allocated(radp%d))    ALLOCATE ( radp%d(nx) )
  if (.not.allocated(radp%Ti))       ALLOCATE ( radp%Ti(nx) )
  if (.not.allocated(radp%Te))       ALLOCATE ( radp%Te(nx) )
  if (.not.allocated(radp%x))       ALLOCATE ( radp%x(nx) )
  if (.not.allocated(radp%Volum))       ALLOCATE ( radp%Volum(nx) )
  if (.not.allocated(radp%jbs))       ALLOCATE ( radp%jbs(nx) )
  if (.not.allocated(radp%jcd))       ALLOCATE ( radp%jcd(nx) )
  if (.not.allocated(radp%jpar))       ALLOCATE ( radp%jpar(nx) )
  if (.not.allocated(radp%ipol))       ALLOCATE ( radp%ipol(nx) )
  if (.not.allocated(radp%qprof))       ALLOCATE ( radp%qprof(nx) )
  if (.not.allocated(radp%g2))       ALLOCATE ( radp%g2(nx) )
  if (.not.allocated(radp%g3))       ALLOCATE ( radp%g3(nx) )
  if (.not.allocated(radp%vp))       ALLOCATE ( radp%vp(nx) )
  if (.not.allocated(radp%palph))       ALLOCATE ( radp%palph(nx) )
  if (.not.allocated(radp%cc))       ALLOCATE ( radp%cc(nx) )
  if (.not.allocated(radp%nions))       ALLOCATE ( radp%nions(nx) )
  if (.not.allocated(radp%psi))       ALLOCATE ( radp%psi(nx) )
  if (.not.allocated(radp%gte))       ALLOCATE ( radp%gte(nx) )
  if (.not.allocated(radp%gti))       ALLOCATE ( radp%gti(nx) )
  if (.not.allocated(radp%gne))       ALLOCATE ( radp%gne(nx) )
  if (.not.allocated(radp%nepg))    ALLOCATE ( radp%nepg(nx) )
  if (.not.allocated(radp%Tepg))       ALLOCATE ( radp%Tepg(nx) )
  if (.not.allocated(radp%Tipg))       ALLOCATE ( radp%Tipg(nx) )
  if (.not.allocated(radp%bpol))       ALLOCATE ( radp%bpol(nx) )
  if (.not.allocated(radp%gradro))       ALLOCATE ( radp%gradro(nx) )
  if (.not.allocated(radp%ndeut))       ALLOCATE ( radp%ndeut(nx) )
  if (.not.allocated(radp%ntrit))       ALLOCATE ( radp%ntrit(nx) )

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!













! MAIN LOOP, do until convergence is reached!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! num%etol : tolerance on kinetic profiles. toleq: tolerance on q profile. num%tol : overall tolerance.
  do while (((num%etol+toleq/num%dt .ge. num%tol).and.(jiter.le.nitermax)).or.(redo.eq.1))

     if (i_diagz.eq.1) write(3301,*) num%etol,toleq,num%tol,jiter,nitermax
     if (i_diagz.eq.1)     write(*,*) " "
     if (i_diagz.eq.1)     write(*,*) " "
     if (i_diagz.eq.1)     write(*,*) " "
     if (i_diagz.eq.1)     write(*,*) "init stuff num%etol,toleq,num%tol,jiter,nitermax"
     if (i_diagz.eq.1)     write(*,*) num%etol,toleq,num%tol,jiter,nitermax

    !update counters
     jnit=jnit+1
     jipperdo = nint(max(1.0d0, 1.0d0/(num%etol*num%dt/100.d0)**num%eopt))
     jipperdo2 = nint(max(1.0d0, 1.0d0/(toleq/100.d0)**(num%dtmaxmin)))
    !!!!!

  !update profiles and tolerance
  y=y0
  gy=gy0
  qy=qy0
  cy=cy0
  Qf=Qf0
  F=F0
  num%etol=num%etol0

! the call below produces sources and transport coefficients to solve the equations
	include 'produce_transport.inc'
 if (i_diagz.eq.1)	write(*,*) 'matrix obtained'
	if (i_diagz.eq.1) then
		write(1441,'(4111E25.11)') xtrt,a(:,1),a(:,2),a(:,3),b(:,1),b(:,2),b(:,3), &
		& c(:,1),c(:,2),c(:,3),y(:,1),y(:,2),y(:,3),chat(:,1),chat(:,2),chat(:,3),gy(:,1),gy(:,2),gy(:,3)
		write(1447,'(4111E25.11)') x,tepr,tipr,nepr, & 
		& interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,2),a(:,2),0.1d0],xr), & 
		& interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,3),a(:,3),0.1d0],xr), & 
		& interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,1),a(:,1),0.1d0],xr), & 
		& interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[b(1,1),b(:,1),0.d0],xr)
		write(1448,'(4111E25.11)') tepr(1),tipr(1),nepr(1),teb,neb,che,cxe,car
			endif

! Transport solver
    Fvec = reshape(F,(/nxt*nchannels/))
    gippa = 0  ! this routine probably not required if imethod is always 1...!
    gyhat = (c-bb*y0)/aa/y0*rpmajor  ! this is the transport solution for the normalized gradients
    gy = (gy0+num%dt*gyhat)/(1.0d0+num%dt)  ! this is the actual solution, weighted with the time step

!	write(*,*) gy0,'*',gy

!	write(1441,'(911E25.11)') x,nepr,tepr,tipr, & 
!	& interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,2),a(:,2),0.1d0],xr), & 
!	& interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[b(1,1),b(:,1),0.d0],xr)


!if (jiter.eq.20) stop

!after transport computation, update plasma profiles, ne te ti
  call update_profiles(dx, nxt, nchannels, gy, y0, rpmajor, y, N_e(1:nxt), T_e(1:nxt), T_i(1:nxt))

!Additional quantities after transport
     Qf = trapz((pedt+pidt)*dV) !alpha power
     Qtot = trapz((Powe+Powi+pradedge)*dV) !power for scaling
     Qtote = trapz((Powe+pradedge)*dV) !electron power
     Qtoti = trapz(Powi*dV) !ion power
     W = (1.5d0)*(tepr*nepr + tipr*nions) * e_charge * 1.0d3 * 1.0d19/1.0d6 !plasma energy
     W_e = trapz((1.5d0)*(tepr*nepr ) * e_charge * 1.0d3 * 1.0d19/1.0d6*dv) !electron energy
     W_i = trapz((1.5d0)*( tipr*nions) * e_charge * 1.0d3 * 1.0d19/1.0d6*dv) !ion energy
     taue = trapz(W*dV)/Qtot !confinement time (only thermal particles)

!scaling confinement time, iter ipb98 y,2
     tau_scal =0.0562*ip**.93*(rpmajor*BTOR)**.15* &
          &          amain**.19*amin**.58*elong**.78* &
          &      (sum(nepr)/nx)**.41*rpmajor**1.24/Qtot**0.69
     Hnow=taue/tau_scal !H factor definition

	if (i_diagz.eq.1) 	write(*,*) 'Qtot,taue,Hnow,ip'
	if (i_diagz.eq.1) 	write(*,*) Qtot,taue,Hnow,ip,y0,y

	if (isnan(Qtot)) then !crashes if Qtot is nan for some reasons
		write(*,*) 'problems!!!!!!!!!!!!!!!'
	i_flag=0
	return
	endif
!these chies are used only by transport model 0

	if (i_modeltype.ne.1) then 
	 chifac0=1.d0  ! transport model gives H in output
	else
  chifac0=max(0.01,chifac0+num%dt*(Hnow-Hfactor)/(1.+num%dt)) !imodeltype = 1: rescale transport to match H factor in input
	endif

!initialize check on time stepping
  redo = 0
	if (i_diagz.eq.1) 	write(*,*) 'etol',num%etol
  num%etol = (maxval(abs(y-y0)/(abs(y)+abs(y0))))/num%dt*100 !this is the new tolerance
	if (i_diagz.eq.1) 	write(*,*) 'etol new',num%etol
 if (num%etol.eq.0.d0) num%etol=100.d0
  num%etolm=min(num%etol,num%etolm)

!time step control
if (num%etol.lt.num%etolm*num%tolmin) then
num%dt=min(num%dtmax,num%dt*num%Ainc)
endif
if (num%etol.ge.num%etolm*num%tolmin) then
num%dt=max(num%dtmin,num%dt/num%dtinc)
endif
	if (i_diagz.eq.1) 	write(444,'(6E25.11)') num%etol0,num%etol,num%dt,pfus(nx),loss%pnbi,comp%cxe

     if (jnit.ge.nitermax) then !if max iterations achieved, exit with an error message
        write(*,*) 'max iterations acheived transport'
        redo=0
        num%etol=0.d0
        jiter=nitermax+1
     endif
	if (i_diagz.eq.1)    write(*,*) 'redo',redo

! redo stands for repeating calculation of transport before going back to MHD equilibrium, obsolete, not used
     if (redo.eq.0) then

        !update counters and tolerances
        jiter = jiter+1
        jipper = jipper+1
        jipper2 = jipper2+1
        num%etol0=num%etol

!update profiles on the transport grid
	nepr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [N_e(1), N_e], xr)
	tepr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [T_e(1), T_e], xr)
	tipr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [T_i(1), T_i], xr)

!update density pedestal & separatrix
              N_e = (N_e+num%dt*N_e/neb*inp0%f_gw*nG)/(1.+num%dt)    !when f_gw is pedestal top greenwald fraction
              neb=N_e(nxt)
														nsep=inp0%f_gws*nG
              N_e(nxt+1)=nsep
              y(:,1) = N_e(1:nxt)
   
!update pedestal temperature if needed
              pressure=nepr*tepr+nions*tipr+palpph !total pressure
              betan=trapz(pressure*dV)/V(nx)*1.e3*e_charge*1.e19*2.*mu_vacuum/btor**2. 
              betan=100.*betan*rpminor*btor/Ip !beta_n

              P_pedtop = ped%pedscal*2.*rpmajor**(-0.38)*trianpg**(0.83)*elong**(0.62)*Ip**(1.25)*betan**(0.43) !Samuli's DEMO scaling

	if (i_diagz.eq.1) 	write(*,*) 'betan,tpedtop',betan,P_pedtop/neb,V(nx),palpph(1),pressure(1)

           if (iped_model.eq.2) then !update temperature if pedmodel = 2
		T_e = T_e/teb*P_pedtop/neb
		T_i = T_i/tib*P_pedtop/neb
             teb = T_e(nxt)
              tib = teb
              T_e(nxt+1)=tsep
              T_i(nxt+1)=tsep

              ! update Te and Ti profiles
              y(:,2) = T_e(1:nxt)
              y(:,3) = T_i(1:nxt)
           endif

        !Finally, update ALL profiles 
        T_e0(1:nxt+1)=T_e(1:nxt+1)
        N_e0(1:nxt+1)=N_e(1:nxt+1)
        T_i0(1:nxt+1)=T_i(1:nxt+1)
        gT_e0(1:nxt)=gT_e(1:nxt)
        gn_e0(1:nxt)=gn_e(1:nxt)
        gT_i0(1:nxt)=gT_i(1:nxt)
        ym=y0
        gym=gy0
        qym=qy0
        cym=cy0
        Fm=F0
        F0=F
        y0=y
        gy0=gy
        qy0=qy
        cy0=cy
        Qf0=Qf
        ! Update profiles after pedestal
	nepr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [N_e(1), N_e], xr)
	tepr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [T_e(1), T_e],xr)
	tipr=interp1_ef(nxt+2,nx,[0.0d0, xtrt], [T_i(1), T_i],xr)

!compute new alpha pressure palph with slowdn_ef
	do jrad=1,nx
           call slowdn_ef(tepr(jrad),zmain,2.d0,zavne(jrad), & 
                & zavxe(jrad),nepr(jrad),amain,aim1,aim2,aim3,palpph(jrad), & 
                & nhe(jrad),nne(jrad),nxe(jrad),& 
                & nalf(jrad),ndeut(jrad)+ntrit(jrad))
	enddo

!EQUILIBRIUM BLOCK
!initialize some quantities for equilibrium calculation
        betaz = 0.0d0
        lint = 0.0d0
        qprf0 = qprf
        G30 = G3
        G20 = G2
        ipol0 = ipol
        vp0 = Vprime !
	if (i_diagz.eq.1) then
         write(*,'(111E25.11)') num%etol
        write(*,*) ' ' 
        write(*,*) trapz((powe+powi)*dV)
        write(*,*) trapz((peaux+piaux)*dV)
        write(*,*) trapz((powe)*dV)
        write(*,*) trapz((powi)*dV)
        write(*,*) trapz((prad)*dV)
        write(*,*) trapz((psync)*dV)
        write(*,*) trapz((pbrad)*dV)
        write(*,*) trapz((pedt)*dV)
        write(*,*) trapz((pidt)*dV)
        write(*,*) qprf(1),qprf(nx)
        write(*,*) ' end stuff'
        write(1321,'(911E25.11)') num%etol,toleq,trapz((peaux+piaux)*dV),trapz((pidt+pedt)*dV),& 
								& tepr(1),nepr(1),neb,teb,tipr(1),&
								& trapz((powe+powi)*dV),trapz((prad)*dV),cxe,che,psep/plh,ip
	endif
        if (jipper2.ge.jipperdo2) then !call equilibrium only if jipper2>jipperdo2

!all the below is computation of jbs and conductivity cc
								           cubb=0.d0 !initialize Jbs to 0
              mux = 1.0d0/qprf
              hro = gradient1(rho)
              ametr = xr
              i = 1
              rtor = rpmajor
              nel = nepr
              tel = tepr
              tii = tipr
              nii = nions
              yd = -0.8d0 * pi_g**2 * rpmajor
              sqeps = sqrt(ametr/rtor)
              betpl = 4.0d0*1.6d-4*pi_g*(nel*tel + nii*tii)*(rtor/(btor*rho*(mux+0.000001)))**2
             	betpl(1)=betpl(2)
														betple = 4.0d0*1.6d-4*pi_g*(nel*tel)*(rtor/(btor*rho*(mux+0.000001)))**2
														betple(1)=betple(2)
              nuee = 670.0d0*coulg*nel/tel**1.5d0
              nues = 6.921e-5*rpmajor*nepr*zepff*coulg/ & 
                   & abs(mux*tepr**2.d0*sqeps**3.d0)
              zavg = nel/nii
              nui = (zepff*zmain**2*zavg)*nii*322.0d0/(tii**1.5d0 * sqrt(amain))
              nuis = 3.2d-6*nui*rtor/(mux*sqeps**3 * sqrt(tii/amain))
              tpf = 1.0d0 - (1.0d0-sqeps**2) * sqrt(1.0d0 - sqeps**2)/(1.0d0+1.46d0*sqeps)
              zz = zepff
              zft = tpf
              ZDF = 1.+(0.55-0.1*ZFT)*SQRT(nues)
              ZDF = ZDF + 0.45*(1.-ZFT)*nues/ZZ/SQRT(ZZ)
              ZFT = ZFT/ZDF
              dcsa=1.-(1.+0.36/ZZ)*ZFT
              dcsa=dcsa+0.59/ZZ*ZFT*ZFT-0.23/ZZ*ZFT*ZFT*ZFT
!conductivity cc
              do jrad = 1, size(x)
                 cc(jrad) = 601.2d0 * tepr(jrad)**(1.5d0)*(0.76+zepff(jrad))/ &
                      & zepff(jrad)/(1.18+0.58*zepff(jrad))/coulg(jrad)
              end do
              cc=cc*dcsa

	if (i_diagz.eq.1) 	write(*,*) 'cc',cc(1)

           !compute current drive from ecrh and nbi
           jcdr=0.d0
											jcdr=inp0%nbcdeff*pnbi*6.2832/trapz(nepr*dv)*v(nx)*10.d0 !PROCESS definition of gamcd
											
 	include 'cubsfml.inc' !jbs computation according to Sauter et al.
	 cubb(1)=0.d0 !on axis is 0
		cubb=max(0.d0,cubb) !do not allow negative bootstrap

           !assign present state to old, so that one can reverse if there are problems in equil
           dv0=dv
           g10=g1
           ipol00=ipol
           phi0=phi
           jpar0=jpar
           roc0=roc
           vloop0=vloop
           fbs0=fbs
           k0=k
           d0=d
           v0=v
           Vprime0=Vprime
           psi0=psi
           rho0=rho
           shif0=shif
           ip0=ip
           gradro0=gradro
           !equilibrium solver equil.f90
           call compute_equil(&
                                !input 
                nx, jiter,i_equiltype, &
                x, tepr, tipr, nepr, nions, palpph, cc, G30,qprf0,G20,vp0, &
                rpmajor,rpminor,elong,trianpg,Ip,btor,betaz,lint,ipol0,e_charge,mu_vacuum, &
                                !inout
                qedge, &
                                !output
                roc, Vloop, fbs,fcd, dum1, &
                k, d, shif, cubb, jcdr, V, G1, G2, G3, dV, phi, qprf, rho, psi, jpar,&
                ipol, Vprime,droda,eqpf,eqff,gradro,q_edge_in,f_ind_in,q_95,elong95,trianpg95 &
                ,pres_fac,areat)
           !check if isnan
           if (isnan(qedge).or.isnan(vloop).or.ip.eq.0.d0) then
              write(*,*) 'equilibrium not converged',vloop,qprf,ip
              qprf=qprf0
              ip=ip0
              dum1=qedge
              qedge=qprf(nx)
              q_95=qprf(nx-1)
              roc=roc0
              vloop=vloop0
              g1=g10
              g2=g20
              g3=g30
              v=v0
              dv=dv0
              ipol=ipol00
              jpar=jpar0
              phi=phi0
              fbs=fbs0
              k=k0
              d=d0
              Vprime=Vprime0
              psi=psi0
              rho=rho0
              cubb=0.d0
              jcdr=0.d0
              shif=shif0
              gradro=gradro0
              toleq=0.d0
              mhd%equilcheck=0.d0
														i_flag=-2 
													return
           else !if converged, update stuff
              toleq=dum1
												  q_oh=ip*(1.-fbs-fcd)*vloop/v(nx) !Ohmic power
              mhd%equilcheck=1.d0
              radp%Volum  = V
              radp%jbs  = cubb
              radp%jcd  = jcdr
              radp%jpar  = jpar
              radp%ipol  = ipol
              radp%qprof = qprf
              radp%psi=psi
              radp%g2 = G2
              radp%g3=G3
              radp%vp=Vprime
              radp%gradro=gradro
	if (num%i_equiltype.eq.1) geom%ip=ip
	if (num%i_equiltype.eq.2) geom%q95=q_95
	geom%k=elong
	geom%d=trianpg
           endif
           !update and exit
           jipper2=0
        endif

!some additional stuff
        shear = gradient(log(qprf),log(x))
        q_tr = interp1_ef(nx,nxt,x, qprf, xtr/xr(nx))
        sh_tr = interp1_ef(nx,nxt,x, shear, xtr/xr(nx))
!current fractions
	fbs=sum(cubb/ipol**2.*dV)*ipol(nx)*btor/(2.*pi_g)/ip
	fcd=sum(jcdr/ipol**2.*dV)*ipol(nx)*btor/(2.*pi_g)/ip
 endif ! jipperdo2, end of equilibrium call

!update greenwald density
  nG = 10.0d0 * Ip/(pi_g * rpminor**2)  ! 10^19

! this below is the control_scheme routine for Psep, Pfus, f_ni, He content, and divertor protection
	include 'control_scheme.f90'

	if (i_diagz.eq.1) 	write(*,*) 'qnbi, cxe',tepr(1),sfus_he,integr_cde(v,nepr,nx),comp%globtau(3),taue,loss%pnbi,cxe,che,car

  end do  ! end of main iteration loop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
















! POST-PROCESSING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 if (jiter.ge.nitermax) then
     write(*,*) ' '
     write(*,*) 'not converged ', num%etol,toleq,trapz(powe*dv),trapz(powi*dv),trapz(prad*dv),trapz((pedt+pidt)*dv)
     write(*,*) ' '
    i_flag=-1
  endif

  ! post-processing: assign stuff to output that will be used by PROCESS

  !profiles
  radp%x = xr/amin
  radp%ne = nepr
  radp%Te = tepr
  radp%Ti = tipr
  radp%ndeut= ndeut
  radp%ntrit = ntrit
  if (mhd%equilcheck.eq.1.d0) then
     radp%Volum  = V
     radp%jbs  = cubb
     radp%jcd  = jcdr
     radp%jpar  = jpar
     radp%ipol  = ipol
     radp%qprof = qprf
     radp%psi=psi
     radp%g2 = G2
     radp%g3=G3
     radp%vp=Vprime
     radp%gradro=gradro
  endif
  radp%cc=cc
  radp%palph=palpph
  radp%nions=nions
  !gradients (only on transport grid)
  radp%gne(1:nxt) = gy(1:nxt,1)
  radp%gTe(1:nxt) = gy(1:nxt,2)
  radp%gTi(1:nxt) = gy(1:nxt,3)
  radp%nepg(1:nxt+1) = N_e(1:nxt+1)
  radp%Tepg(1:nxt+1) = T_e(1:nxt+1)
  radp%Tipg(1:nxt+1) = T_i(1:nxt+1)
		radp%shif=shif
		radp%k=k
		radp%d=d
  !averages
  radp%av_ne = trapz(nepr*dV)/V(nx)
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
 	nHe = cHe * nepr
 	nHe3 = cHe3 * nepr
 	nprot = (cprot+comp%protium)*nepr
	call prxe_func(nx, tepr, prxe, zavxe)
	call prar_func(nx, tepr, prne, zavne)
	ndeut = fuelmix*(nepr - nNe*zavne - nXe*zavxe - 2.0d0*(nHe+nhe3)-nprot)
	ntrit = (1.0d0-fuelmix)*(nepr - nNe*zavne - nXe*zavxe - 2.0d0*(nHe+nhe3)-nprot)
	nions = ndeut + ntrit + nNe + nXe + nHe + nhe3 + nprot
	zepff = (1.0d0/nepr) * (ndeut+ntrit+4.0d0*(nHe+nhe3)+zavne**2*nNe+zavxe**2*nXe+nprot)
  coulg = 15.9d0 - 0.5d0*log(nepr) + log(tepr)
  radp%av_ni = trapz(nions*dV)/V(nx)
  radp%av_nd = trapz((ndeut+ntrit)*dV)/V(nx)
  radp%av_nz = trapz((nne+nxe)*dv)/V(nx)
  radp%av_nhe = trapz((nhe+nhe3)*dV)/V(nx)
  radp%av_Ti = trapz(tipr*dV)/V(nx)
  radp%av_Te = trapz(tepr*dV)/V(nx)
  radp%av_Ten = trapz(tepr*nepr*dV)/trapz(nepr*dV)
  radp%zeff  = trapz(zepff*dV)/V(nx)

!b.c.
	ped%teped=teb
	ped%nped=neb
	ped%tesep=tsep
	ped%nsep=nsep

!composition
 	comp%comparray(1)=comp%protium+cprot+radp%av_nd/radp%av_ne
		comp%comparray(2)=che+che3
  comp%comparray(13)=cxe
		comp%comparray(9)=car

!global geometry
  geom%Rold=rpmajor
  geom%Aold=asppect
  geom%kold=elong
  geom%dold=trianpg
  geom%Ipold=geom%ip
  geom%Btold=btor
  geom%q95old=q_95
  geom%counter=geom%counter+1.d0

!MHD
  pressure=nepr*tepr+nions*tipr+palpph
  radp%bpol=1.d0/(2.d0*pi_g*rpmajor)*gradient(psi,xr)
  mhd%Bpolavg=trapz(radp%bpol*dv)/v(nx)
  mhd%betator=2.d0*mu_vacuum*1.d3*1.d19*e_charge*trapz(pressure*dv)/(v(nx)*btor**2.d0)
  mhd%betapol=2.d0*mu_vacuum*1.d3*1.d19*e_charge*trapz(pressure*dv)/trapz(radp%bpol**2.d0*dv)
  mhd%torsurf=areat
  mhd%rli=2.d0*trapz(radp%bpol**2.d0*dv)/ & 
  & ((1.d6*geom%ip)**2.d0)/(4.*3.141592*1.d-7)**2.d0/rpmajor
 	mhd%qoh=q_oh(1)
 	mhd%betan = betan
  mhd%f_gwpedtop=neb/(1./(3.141592*rpminor**2.)*Ip*10)
  mhd%Vp = V(nx)
  mhd%Sp = radp%vp(nx)*radp%gradro(nx)
  mhd%q = q_95
  mhd%q_sep = qprf(nx)
  mhd%ip_out = ip
  jpol=ipol/(rpmajor*btor)
  mhd%qstar = 5*btor*rpminor**2./rpmajor/ip* & 
  & (1+elong95**2.*(1+2*trianpg95**2.-1.2*trianpg95**3.))/2.
  mhd%bp=mhd%bpolavg
  if (mhd%equilcheck.eq.1.d0) then
     write(2901,*) 'change vloop'
     mhd%Vloop = Vloop
     mhd%fbs = fbs
     mhd%f_ni = fcd+fbs
  endif

!Loss powers, confinement, and additionals
	loss%peaux=trapz(peaux*dv)
	loss%piaux=trapz(piaux*dv)
	loss%qheat=q_heat
	loss%qcd=q_cd
	loss%qfus=q_fus
	loss%pfusdd=trapz(p_dd*dv)*4.d0
  !loss powers and confinement characteristics
  loss%Psep = Qtot-Qradedge
  loss%PLH = PLH
  loss%Prad = trapz((pradtot+pradedge)*dv)
  loss%Wth = trapz(W*dV) ! MJoules check units  051216
  loss%TAUeff = taue
  loss%H = loss%TAUeff/tau_scal
  loss%Pfus = 5.0d0*trapz((pidt+pedt)*dV)
  loss%chifac0 = chifac0
  loss%pohm=ip*(1.-fbs-fcd)*vloop
  loss%rplas=vloop/(ip*(1.-fbs-fcd))/1.d6
  loss%psync=trapz(psync*dv)
  loss%pbrehms=trapz(pbrad*dv)
  loss%pline=trapz(prad*dv)
  loss%psepi=trapz(powi*dv)
  loss%piepv=-trapz(peicl*dv)/v(nx)
		loss%pdiv=qdivt
	 loss%palpe=trapz(pedt*dv)
	 loss%palpi=trapz(pidt*dv)
 if (i_modeltype.eq.1) loss%H=inp0%hfac_inp
 loss%fusionrate=trapz((pedt+pidt)*dv)/v(nx)/5.632*1.e19	
 loss%alpharate=loss%fusionrate
 loss%betaft=trapz(palpph*dV)/V(nx)*1.e3*e_charge*1.e19*2.*mu_vacuum/btor**2.
  loss%Hcorr=Hnow            ! H factor with radiation correction
  loss%pradedge=qradedge
  loss%pradcore=qrad
  loss%psepe=trapz(powe*dv)
!fueling
	loss%dfuelreq = trapz(ndeut*dv)/(comp%globtau(1)*taue)
	loss%tfuelreq = trapz(ntrit*dv)/(comp%globtau(2)*taue)
	loss%hepumpreq = trapz(nHe*dv)/(comp%globtau(3)*taue)
!confinement
 loss%tauee=w_e/qtote
 loss%tauei=w_i/qtoti
 loss%qtot=qtot

	!perimeter
	do i=1,num%nx
	theta_perim(i)=6.28318530717959*(i-1.)/num%nx
	enddo
	dtheta=theta_perim(2)-theta_perim(1)
	f_perim=sqrt(cos(theta_perim)**2.+elong**2.*sin(theta_perim)**2.+ & 
	 & trianpg**2.*sin(theta_perim)**4.-2.*trianpg*cos(theta_perim)*sin(theta_perim)**2.)
	geom%perim = rpminor*trapz(f_perim*dtheta)


!diags for ASTRA
  if (geom%counter.gt.1.) then
     open(99,file='./CHARTST/PROCESSOUTPUT.chartst',STATUS='UNKNOWN',Access = 'append')
  else
     open(99,file='./CHARTST/PROCESSOUTPUT.chartst')
  endif
  write(99,'(911E25.11)')   x
  write(99,'(911E25.11)')   nepr
  write(99,'(911E25.11)')   tepr
  write(99,'(911E25.11)')   tipr
  write(99,'(911E25.11)')   qprf
  write(99,'(911E25.11)')   jpar
  write(99,'(911E25.11)')   cubb
  write(99,'(911E25.11)')   jcdr
  write(99,'(911E25.11)')   peaux
  write(99,'(911E25.11)')   piaux
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,2),a(:,2),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,3),a(:,3),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,1),a(:,1),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[b(1,1),b(:,1),0.d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[c(1,2),c(:,2),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[c(1,3),c(:,3),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[c(1,1),c(:,1),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[c(1,1),c(:,1),0.d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[chat(1,2),chat(:,2),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[chat(1,3),chat(:,3),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[chat(1,1),chat(:,1),0.1d0],xr)
  write(99,'(911E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[chat(1,1),chat(:,1),0.d0],xr)
  write(99,'(911E25.11)')   pradtot+pradedge
  close(99)
  open(99,file='./CHARTST/forastra1.txt')
  write(99,'(111E25.11)')   rpmajor,asppect,elong,trianpg,ip,btor, &
       & che,cxe,car,fuelmix,xb,hfactor,Hnow,loss%H,loss%Pfus,loss%pnbi
  close(99)
  open(99,file='./CHARTST/forastra11.txt')
  write(99,*)   'rpmajor,asppect,elong,trianpg,ip,btor,che,cxe,cne,fuelmix,xb,hfactor,Hnow,loss%H'
  write(99,'(111E25.11)')   rpmajor,asppect,elong,trianpg,ip,btor, &
       & che,cxe,car,fuelmix,xb,hfactor,Hnow,loss%H
  close(99)
  open(99,file='./CHARTST/forastra2.txt')
  write(99,'(999E25.11)')   x
  write(99,'(999E25.11)')   nepr
  write(99,'(999E25.11)')   tepr
  write(99,'(999E25.11)')   tipr
  write(99,'(999E25.11)')   qprf
  write(99,'(999E25.11)')   jpar
  write(99,'(999E25.11)')   cubb
  write(99,'(999E25.11)')   jcdr
  write(99,'(911E25.11)')   peaux
  write(99,'(911E25.11)')   piaux
  write(99,'(999E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,2)+a_neo(1,2),a(:,2)+a_neo(:,2),0.1d0],xr)
  write(99,'(999E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,3)+a_neo(1,3),a(:,3)+a_neo(:,3),0.1d0],xr)
  write(99,'(999E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[a(1,1)+a_neo(1,1),a(:,1)+a_neo(:,1),0.1d0],xr)
  write(99,'(999E25.11)')   interp1_ef(nxt+2,nx,[0.d0,xtr,rpminor],[b(1,1)+b_neo(1,1),b(:,1)+b_neo(:,1),0.d0],xr)
  write(99,'(999E25.11)')   vprime !15
  write(99,'(999E25.11)')   g1 !16
  write(99,'(999E25.11)')   droda !17
  write(99,'(999E25.11)')   g2
  write(99,'(999E25.11)')   g3
  write(99,'(999E25.11)')   shif
  write(99,'(999E25.11)')   eqpf
  write(99,'(999E25.11)')   eqff
  write(99,'(999E25.11)')   (psi-psi(1))/(psi(nx)-psi(1))
  write(99,'(999E25.11)')   ndeut+ntrit
  write(99,'(999E25.11)')   nhe
  write(99,'(999E25.11)')   nne
  write(99,'(999E25.11)')   nxe
		write(99,'(999E25.11)')   nxe*zavxe+nne*zavne+2*nhe
  close(99)
  write(2901,*) 'vloop,q,ip : ',mhd%vloop,mhd%q,mhd%ip_out 
  write(2901,*) 'Fusion power : ',loss%pfus 
  write(2901,*) 'converged in iterations : ',jiter,num%etol,toleq,redo,loss%Pfus,mhd%vloop,T_e(1),tepr(1),& 
       & mhd%equilcheck,mhd%f_ni,loss%H,loss%Hcorr,inp0%hfac_inp,Hfactor
!	if (jiter.gt.3) write(*,*) "plasmod end ",jiter,mhd%vloop,loss%pfus
	write(*,*) "plasmod end ",jiter,mhd%vloop,loss%pfus,toleq,num%etol
!	write(*,*) nx,nxequil,ip,q(nx),q_edge_in,q_95,qedge


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


end subroutine plasmod_EF




!ADDITIONAL SUBROUTINES BELOW!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





  subroutine update_profiles(dx, nxt, nchan, gy, y0, R, y, N, Te, Ti)

    implicit none
    
    !  Arguments
    
    integer, intent(in) :: nxt, nchan
    real(kind(1.0D0)), intent(in) :: dx, R
    real(kind(1.0d0)), dimension(nxt, nchan), intent(in) :: gy, y0
    
    real(kind(1.0d0)), dimension(nxt), intent(out) :: N, Te, Ti
    real(kind(1.0d0)), dimension(nxt, nchan), intent(out) :: y
    
    !  Local variables
    
    real(kind(1.0d0)), dimension(nxt, nchan) :: smallr
    real(kind(1.0d0)), dimension(nxt) :: f
    integer :: s, j, k
    
    y = y0
    N = 0.0d0
    Te = 0.0d0
    Ti = 0.0d0
    
    smallr = -1.0d0 * gy/R
    f = 0.0d0

    !recovers y from gy
    do s = 1, nchan
       do j = 1, nxt-1
          k = nxt-j
          f(k) = f(k+1) - dx * (smallr(k, s) + smallr(k+1, s))/2.0d0
          y(k, s) = y(nxt, s)*exp(f(k))
       end do
    end do
    
    N(1:nxt) = y(:,1)
    Te(1:nxt) = y(:,2)
    Ti(1:nxt) = y(:,3)


  return
    
  end subroutine update_profiles
  















      subroutine SLOWDN_EF(te,zmain,zim1,zim2,zim3,ne,amain,aim1,aim2, & 
       & aim3,pfast,niz1,niz2,niz3,nalf,nmain)

!EFable 2016, compute t_fast and p_fast assuming slowing down. R. Bilato, http://pubman.mpdl.mpg.de/pubman/item/escidoc:2039793/component/escidoc:2183257/Bilato_On.pdf, page 4-5
			
			implicit none

	integer J,i,ishot
				

	double precision 	te,zmain,zim1,zim2,zim3,ne,amain,aim1,aim2,aim3,pfast
	double precision  e_birth,xx,gg,t_fast,nalf
	double precision e_crit,gp,niz1,niz2,niz3,nmain
	
	gp=3.141592
	

		e_birth=3500.

		e_crit = 4.*TE*(  &
     & 3./4.*sqrt(GP)*42.8561* &
     & ZMAIN**2.*NMAIN/NE/AMAIN+ &
     & ZIM1**2.*NIZ1/NE/AIM1+ &
     & ZIM2**2.*NIZ2/NE/AIM2+ &
     & ZIM3**2.*NIZ3/NE/AIM3 &
     & )**(2./3.)

	xx=sqrt(e_birth/e_crit) 
	gg=1./sqrt(3.)*atan((2.*xx-1.)/sqrt(3.))+ &
     & 1./6.*log((xx**2.-xx+1)/(xx+1)**2.)+ &
     & sqrt(3.*GP)/18.
	
	t_fast=e_crit* &
     & (log(1+(e_birth/e_crit)**(1.5)))**(-1.)* &
     & (e_birth/e_crit-2*gg)

	PFAST=t_fast*NALF !output
						
 end















  
  subroutine prxe_func(nx, tepr, prxe, zavxe)

    implicit none
    
    ! Arguments
    real(kind(1.0d0)), intent(in) :: tepr(nx)
    real(kind(1.0d0)), intent(out) :: prxe(nx), zavxe(nx)
    integer, intent(in) :: nx
    
    ! Local variables
    real(kind(1.0d0)) :: T, Z, Y
    integer :: j
    
    do j = 1, size(tepr)
       T = tepr(j)*1000.0d0
       Z = log10(tepr(j))
       
       if ((T.le.200.0d0).and.(T.gt.0.0d0)) then
          Y=-2.027267d1 - Z*1.530175d1 - Z**2*3.074557d1 - Z**3*3.155124d1 - Z**4*1.600739d1 - Z**5*3.091098
       else if ((T.le.2000.0d0).and.(T.gt.200.0d0)) then
          Y=-1.778249d1 + Z*2.776326d-1 + Z**2*1.901048 - Z**3*5.727093 - Z**4*6.456918 + Z**5*2.998205
       else if ((T.le.20000.0d0).and.(T.gt.2000.0d0)) then
          Y=-2.445709d1 + Z*5.504901d1 - Z**2*1.625266d2 + Z**3*2.174682d2 - Z**4*1.363026d2 + Z**5*3.242958d1
       else if (T.gt.20000.0d0) then
          Y=-3.693018d1 + Z*6.802254d1 - Z**2*9.562685d1 + Z**3*6.445815d1 - Z**4*2.107990d1 + Z**5*2.700386
       end if
       prxe(j) = 10.0d0**Y * 1.0d19
       if (T.le.80.0d0) then
          prxe(j) = 0.0d0
       end if
       
       if ((T.le.200.0d0).and.(T.gt.0.0d0)) then
          Y=-1.612164d2 - Z*1.091205d3 - Z**2*2.598436d3 - Z**3*3.017742d3 - Z**4*1.738551d3 - Z**5*3.985735d2
       else if ((T.le.2000.0d0).and.(T.gt.200.0d0)) then
          Y=2.556534d1 + Z*1.9454143 + Z**2*7.525661 + Z**3*1.189704d2 + Z**4*7.082394d1 - Z**5*5.952649d1
       else if ((T.le.20000.0d0).and.(T.gt.2000.0d0)) then
          Y=9.150919d1 - Z*5.787112d2 + Z**2*1.935449d3 - Z**3*2.807403d3 + Z**4*1.874038d3 - Z**5*4.698364d2
       else if (T.gt.20000.0d0) then
          Y=-6.2323d2 + Z*2.021506d3 - Z**2*2.433083d3 + Z**3*1.466899d3 - Z**4*4.421467d2 + Z**5*5.32444d1
       end if
       zavxe(j) = Y
       if (T.le.80.0d0) then
          zavxe(j) = 0.0d0
       end if
       
    end do

return     
  end subroutine prxe_func


















  
  subroutine prne_func(nx, tepr, prne, zavne)

    implicit none
    
    ! Arguments
    real(kind(1.0d0)), intent(in) :: tepr(nx)
    real(kind(1.0d0)), intent(out) :: prne(nx), zavne(nx)
    integer, intent(in) :: nx
    
    ! Local variables
    real(kind(1.0d0)) :: T, Z, Y
    integer :: j
    
    zavne = 1.0d0
    
    do j = 1, size(tepr)
       T = tepr(j)*1000.0d0
       Z = log10(T)
       
       if (T.le.20.0d0) then
          Y=-18.09 + 9.8979*Z - 3.6044*Z*Z
       else if ((T.le.35.0d0).and.(T.gt.20.0d0)) then
          Y=-10.073 - 3.3149*Z + 1.7445*Z*Z
       else if ((T.le.70.0d0).and.(T.gt.35.0d0)) then
          Y=-40.082 + 36.658*Z - 11.554*Z*Z
       else if ((T.le.200.0d0).and.(T.gt.70.0d0)) then
          Y=30.049 - 38.298*Z + 8.4829*Z*Z
       else if ((T.le.500.0d0).and.(T.gt.200.0d0)) then
          Y=-51.92 + 30.584*Z - 5.9674*Z*Z
       else if ((T.le.2000.0d0).and.(T.gt.500.0d0)) then
          Y=-2.6116 - 5.9817*Z + 0.81513*Z*Z
       else if (T.gt.2000.0d0) then
          Y=-5.8745 - 4.1134*Z + 0.54821*Z*Z
       end if
       prne(j) = 10.0d0**(Y+12.0d0)
       
       T = T/1000.0
       Z = log10(T)
       
       if (T.lt.0.09d0) then
          zavne(j)=max(1.0d0 , 3.0d0+5.714d0*(Z+1.92d0))
       else if ((T.lt.0.3d0).and.(T.ge.0.09d0)) then
          zavne(j) = 8.0d0
       else if ((T.lt.0.7d0).and.(T.ge.0.3d0)) then
          zavne(j) = 8.0d0 + 5.435*(Z+0.5229)
       else if (T.ge.0.7d0) then
          zavne(j) = 10.0d0
       end if
       
    end do

return
    
  end subroutine prne_func
























  
  subroutine prar_func(nx, tepr, prne, zavne)

    implicit none
    
    ! Arguments
    real(kind(1.0d0)), intent(in) :: tepr(nx)
    real(kind(1.0d0)), intent(out) :: prne(nx), zavne(nx)
    integer, intent(in) :: nx
    
    ! Local variables
    real(kind(1.0d0)) :: T, Z, Y
    integer :: j
    
    zavne = 1.0d0
    
    do j = 1, size(tepr)
        T=TEpr(J)
        Z=1/(TEpr(J)+.0001)
        IF(T.LE..02)                 Y=1000*t
        IF(T.LE..043.AND.T.GT..02)   Y=0.7*Z-0.55
        IF(T.LE..07.AND.T.GT..043)   Y=0.65
        IF(T.LE..25.AND.T.GT..07)    Y=30.*T-1.3
        IF(T.LE..53.AND.T.GT..25)    Y=2.5*Z-4
        IF(T.LE..86.AND.T.GT..53)    Y=0.7*Z-0.55
        IF(T.LE.1.9.AND.T.GT..86)    Y=0.09*T+0.19
        IF(T.LE.2.53.AND.T.GT.1.9)   Y=0.35
        IF(T.LE.10..AND.T.GT.2.53)   Y=0.7/(T+.01)+0.09
        IF(T.LE.23..AND.T.GT.10.)    Y=1/(T-3.4)**2+0.138
        IF(T.LE.100..AND.T.GT.23.)   Y=9.*10**(-4.01)*T+0.12
        IF(T.GT.100.) 		     Y=9.*10**(-4.01)*T+0.12
        prne(j)	=Y
       
       T = T/1000.0
       Z = log10(T)
       
					zavne(j)=1.
       IF(T.LT..045.AND.T.GE..0055) zavne(j)=3.+7.6545*(Z+2.)
       IF(T.LT..12.AND.T.GE..045) zavne(j)=8.
       IF(T.LT..55.AND.T.GE..12) zavne(j)=8.+12.1*(Z+0.92)
       IF(T.LT.1.8.and.T.GE..55)      zavne(j)=16.
	IF(T.LT.4.0.and.T.ge.1.8)	zavne(j)=16+5.767*(Z-.2553)
	if(T.gt.4.)		zavne(j)=18.
       
    end do

return
    
  end subroutine prar_func


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
