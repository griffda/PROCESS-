module structs



  !use grid_dimensions
  implicit none

!  integer, parameter :: nx = 41    !these should go into inputs together with numeric_transp
!  integer, parameter :: nxt = 5, nchannels = 3
    type numerics_transp
      SEQUENCE
      real(kind(1.0D0)) :: tol, dtmin, dtminmin, dtminmax, dtmax, dtmaxmin, dtmaxmax, dtinc, tolopt, eopt, &
        etol, etol0, etolm,tolmax, tolmin, dgy !, dtmin0, dt0
      real(kind(1.0d0)) :: dt, capA, Ainc, maxA, test, biggest
      integer :: i_modeltype, i_equiltype
      integer :: nx != 11!31!41    !these should go into inputs together with numeric_transp
      integer :: nxt, nchannels,i_impmodel != 5, nchannels = 3
      integer :: ipedestal
    end type numerics_transp  



!numerics structure
!+ad_name  numerics
!+ad_summ  solver controls
!+ad_type  Structure
!+ad_cont tol                                       : local real : tol = 1.0d-9 used as condition in transport loop
!+ad_cont dtmin                                     : local real : dtmin = 0.001 minimum time step, used in bigchecks
!+ad_cont dtminmin                                  : local real : dtminmin = 1.0d-9 (dtmin^2), minimum bound for dtmin, used in bigchecks
!+ad_cont dtminmax                                  : local real : dtminmax = 0.001, maximum bound for dtmin, used in bigchecks
!+ad_cont dtmax                                     : local real : upper bound used to calculate time step
!+ad_cont dtmaxmin                                  : local real : lower bound of dtmax, not used
!+ad_cont dtmaxmax                                  : local real : upper bound of dtmax, not used
!+ad_cont dtinc                                     : local real : dtinc = 2.0, increase in time step?
!+ad_cont tolopt                                    : local real : tolopt = 0.0001 not used
!+ad_cont eopt                                      : local real : eopt = 100.0d0 not used
!+ad_cont etol                                      : local real : etol: error, do while (((etol+toleq .ge. tol)... - etol = (2.0d0/3.0d0)*(maxval(abs(y-y0)/(abs(y)+abs(y0)))+maxval(abs(y0-ym)/(abs(y0)+abs(ym)))+maxval...
!+ad_cont etol0                                     : local real : previous iteration value for etol, used also in bigchecks
!+ad_cont etolm                                     : local real : previous value to etol0, 2nd order difference?
!+ad_cont tolmax                                    : local real : upper bound for etol, used in bigchecks to determine time step
!+ad_cont tolmin                                    : local real : lower bound for etol, used in bigchecks to determine time step
!+ad_cont dgy                                       : local real : not used
!+ad_cont dt                                        : local real : time step
!+ad_cont capA                                      : local real : used for an error control procedure that still don't understand ???
!+ad_cont Ainc                                      : local real : used to determine the value of capA
!+ad_cont maxA                                      : local real : used to determine the value of capA
!+ad_cont test                                      : local real : test = (sum(abs(ijacob))) if ((test.ne.test).or.(test.gt.biggest)) then gippa = 1 - a part of the same procedure
!+ad_cont biggest                                   : local real : biggest = huge(kind(1.0d0)) -- if ((test.ne.test).or.(test.gt.biggest)) then ...
!+ad_desc  This structure contains variables used to control the numerical algorithms
!+ad_prob  None
!+ad_hist  5/12/16 Initial version
!+ad_stat  Okay
!+ad_docs  N/A



!input structures

!+ad_name  geometry
!+ad_summ  plasma geometrical parameters + Ip, Bt
!+ad_type  Structure
!+ad_cont R                                     : local real : major radius
!+ad_cont A                                     : local real : aspect ratio
!+ad_cont k                                     : local real : elongation
!+ad_cont d                                     : local real : triangularity
!+ad_cont Ip                                    : local real : poloidal current
!+ad_cont Bt                                    : local real : toroidal field
!+ad_desc  This structure contains variables describing plasma geometry, plus poloidal current and toroidal field, used in MHD equilibrium calculations
!+ad_prob  None
!+ad_hist  5/12/16 Initial version
!+ad_stat  Okay
!+ad_docs  N/A

    type geometry
      SEQUENCE
      real(kind(1.0D0)) :: R,A,k,d,Ip,Bt,q95,Rold,Aold,kold,dold,Ipold,Btold,counter=0.d0
      real(kind(1.0D0)) :: f_ind !the fraction of the plasma current produced by non-inductive means.
      real(kind(1.0D0)) :: q_edge 
      real(kind(1.0D0)) :: alphaj,alphan,alphat,ipedestal ,te,dene,tbeta
      real(kind(1.0D0)) :: k95,d95,q95old
    end type geometry 

!+ad_name  composition
!+ad_summ  plasma composition parameters 
!+ad_type  Structure
!+ad_cont cHe                                     : local real : average helium fraction in plasma core
!+ad_cont cXe                                     : local real : average xenon fraction in plasma core
!+ad_cont cNe                                     : local real : average neon fraction in plasma core
!+ad_cont fuelmix                                 : local real : average tritium fraction in plasma core
!+ad_desc  This structure contains variables describing plasma composition
!+ad_prob  None
!+ad_hist  5/12/16 Initial version
!+ad_stat  Okay
!+ad_docs  N/A

    type composition
      SEQUENCE
      real(kind(1.0D0)) :: globtau(5),psepplh_inf,psepplh_sup,qdivt,fuelmix,typ1,typ2,typ3,che,cxe,car,c_car, &
						& pradpos,pradfrac,psepb_q95AR,psep_r
    end type composition 

!+ad_name  pedestal
!+ad_summ  plasma pedestal parameters 
!+ad_type  Structure
!+ad_cont nped                                     : local real : plasma density at pedestal border with plasma core (e-/m3)
!+ad_cont teped                                    : local real : plasma temperature at pedestal border with plasma core (keV)
!+ad_cont nsep                                     : local real : plasma density at separatrix (e-/m3)
!+ad_cont tesep                                    : local real : plasma temperature at separatrix (keV)
!+ad_cont rho_T                                    : local real : r/a radial position (normalized to fraction of 1) of temperature pedestal
!+ad_cont rho_n                                    : local real : r/a radial position (normalized to fraction of 1) of density pedestal
!+ad_cont typ1                                    : local real : first type of impurity (identification number) 1 He, 2 Ne,  3 Xe
!+ad_cont typ2                                    : local real : second type of impurity (identification number)
!+ad_cont typ3                                    : local real : third type of impurity (identification number)
!+ad_desc  This structure contains variables describing plasma pedestal
!+ad_prob  None
!+ad_hist  5/12/16 Initial version
!+ad_stat  Okay
!+ad_docs  N/A


    type pedestal
      SEQUENCE
      real(kind(1.0D0)) :: nped,teped,nsep,tesep,rho_T,rho_n,pedscal   !should go into the solver
    end type pedestal 


!+ad_name  inputs
!+ad_summ  plasma particle and energy input parameters 
!+ad_type  Structure
!+ad_cont qecrh                                     : local real : ecrh power in MW
!+ad_cont qnbi                                     : local real : nbi power in MW
!+ad_cont spellet                                     : local real : pellet ion fuelling (paticle/s)
!+ad_cont spuffing                                     : local real : gas puff (particle/s)
!+ad_cont nbcdeff                                     : local real : nbi cd efficiency [MA/(MW*m^2)]
!+ad_cont eccdeff                                     : local real : eccd efficiency  [MA/(MW*m^2*keV)]
!+ad_desc  This structure contains variables describing plasma particle and energy input parameters 
!+ad_prob  None
!+ad_hist  5/12/16 Initial version
!+ad_stat  Okay
!+ad_docs  N/A
! x's : position where gaussians have to be deposited for heating, cd, fus, control (1) is NBI, (2) is ECH
! dx's : widht of gaussians have to be deposited for heating, cd, fus, control (1) is NBI, (2) is ECH
! exp(-((x-x0)/dx)^2)


    type inputs
      SEQUENCE
      real(kind(1.0D0)) :: f_ni,pfus,nbcdeff,eccdeff,f_gw,Hfac_inp,pheatmax,& 
						& v_loop,f_gws,qnbi_psepfac,cxe_psepfac,pech,pnbi,car_qdivt, & 
						& qheat,qcd,qfus,spellet,fpellet,q_control, & 
						& x_heat(2),x_cd(2),x_fus(2),x_control(2), &
						& dx_heat(2),dx_cd(2),dx_fus(2),dx_control(2),nbi_energy, &
						& PLH,maxpauxor
!gamma_ecrh,gamma_nbi (efficiencies)
    end type inputs 

! output structures

!+ad_name  radial_profiles
!+ad_summ  plasma radial profiles, and average values 
!+ad_type  Structure
!+ad_cont ne                                     : local real : radial profile for plasma density (e-/m3)
!+ad_cont Ti                                     : local real : radial profile for ion temperature (keV)
!+ad_cont Te                                     : local real : radial profile for electron temperature (keV)
!+ad_cont x                                      : local real : radial profile positions (fraction of 1), defined as r/a
!+ad_cont ndeut                                      : local real : deuterium density profile
!+ad_cont ntrit                                      : local real : tritium density profile
!+ad_cont av_ne                                  : local real : volume averaged electron density (e-/m3)
!+ad_cont av_Ti                                  : local real : density averaged ion temperature (keV)
!+ad_cont av_Te                                  : local real : density averaged electron temperature (keV)
!+ad_cont zeff                                   : local real : zeff
!+ad_cont jbs                                      : local real : bootstrap current profile in MA/m^2
!+ad_cont jcd                                      : local real : current drive current profile in MA/m^2
!+ad_cont jpar                                      : local real : total current profile in MA/m^2
!+ad_cont ipol                                      : local real : R*Bphi profile T*m
!+ad_cont qprof                                      : local real : safety factor profile
!+ad_cont Volum                                      : local real : volume profile m^3
!+ad_desc  This structure contains variables describing radial profiles and averaged values
!+ad_prob  None
!+ad_hist  5/12/16 Initial version
!+ad_stat  Okay
!+ad_docs  N/A

    type radial_profiles
      SEQUENCE
      real(kind(1.0D0)), DIMENSION(:), ALLOCATABLE :: ne,Ti,Te,x,ndeut,ntrit, & 
						& jbs, jcd, jpar, & 
						&  ipol,qprof,Volum,g2,g3,vp,cc,& 
						& palph,nions,psi,gte,gti,gne,Nepg,Tepg,Tipg,bpol,gradro
      real(kind(1.0D0)) :: av_ne,av_Ti,av_Te, zeff,av_ni
    end type radial_profiles 

!+ad_name  MHD_EQ
!+ad_summ  plasma MHD equilibrium results
!+ad_type  Structure
!+ad_cont Vp                                     : local real : plasma volume
!+ad_cont Sp                                     : local real : plasma surface
!+ad_cont Vloop                                  : local real : loop voltage
!+ad_cont fbs                                    : local real : bootstrap fraction
!+ad_cont q                                      : local real : safety factor edge
!+ad_desc  This structure contains variables describing results from MHD equilibrium calculations
!+ad_prob  None
!+ad_hist  5/12/16 Initial version
!+ad_stat  Okay
!+ad_docs  N/A

    type MHD_EQ
      SEQUENCE
      real(kind(1.0D0)) :: Vp, Sp, Vloop, fbs, q , f_gwpedtop !, Hfac_out
      real(kind(1.0D0)) :: Bpolavg,betator,betapol,torsurf,ip_out,f_ni,equilcheck,betan,q_sep
      real(kind(1.0D0)) :: qstar,bp,rli
    end type MHD_EQ 

!+ad_name  power_losses
!+ad_summ  plasma power_losses results
!+ad_type  Structure
!+ad_cont Psep                                      : local real : power crossing separatrix, conductive losses (MW)
!+ad_cont Prad                                      : local real : radiative losses in plasma core (MW)
!+ad_cont Wth                                       : local real : Plasma thermal energy (MJ)
!+ad_cont TAUeff                                    : local real : effective energy confinement time (s)
!+ad_cont H                                         : local real : H factor
!+ad_cont Pfus                                      : local real : fusion power
!+ad_desc  This structure contains variables describing results from power balance calculations
!+ad_prob  None
!+ad_hist  5/12/16 Initial version
!+ad_stat  Okay
!+ad_docs  N/A

    type power_losses
      SEQUENCE
      real(kind(1.0D0)) :: Psep, Prad, Wth, TAUeff, & 
						& H, Pfus, Pdiv, Tdiv, Pradcore, Pradsol, chifac0, PLH
      real(kind(1.0D0)) :: pradedge,psync,pbrehms,pline,& 
  & psepe,psepi,Hcorr,piepv,pohm,rplas,dfuelreq,tfuelreq,hepumpreq, &
	& qheat,qcd,qfus,pnbi,pech,peaux,piaux, & 
	& fusionrate,alpharate,betaft,palpe,palpi,tauee,tauei,qtot
	! power units in MW   
 end type power_losses 


end module structs




