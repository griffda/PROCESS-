!these are the structure used in PLASMOD. Inputs and outputs are defined in the documentation.
module structs

  implicit none


!numerics
    type numerics_transp
      SEQUENCE
      real(kind(1.0D0)) :: tol, dtmin, dtminmin, dtminmax, dtmax, dtmaxmin, dtmaxmax=1.d0, dtinc, tolopt, eopt, &
        etol, etol0, etolm,tolmax, tolmin, dgy !, dtmin0, dt0
      real(kind(1.0d0)) :: dt, capA, Ainc, maxA, test, biggest
      integer :: i_modeltype, i_equiltype
      integer :: nx != 11!31!41    !these should go into inputs together with numeric_transp
      integer :: nxt, nchannels,i_impmodel != 5, nchannels = 3
      integer :: ipedestal,ntglf
      real(kind(1.0d0)) :: chvars(100)
      real(kind(1.0d0)) :: xtglf(100)
      integer :: isiccir=0
						integer :: isawt=1
						integer :: iprocess=1
    end type numerics_transp  



!geometry
    type geometry
      SEQUENCE
      real(kind(1.0D0)) :: R,A,k,d,Ip,Bt,q95,Rold,Aold,kold,dold,Ipold,Btold,counter=0.d0
      real(kind(1.0D0)) :: f_ind !the fraction of the plasma current produced by non-inductive means.
      real(kind(1.0D0)) :: q_edge 
      real(kind(1.0D0)) :: alphaj,alphan,alphat,ipedestal ,te,dene,tbeta
      real(kind(1.0D0)) :: k95,d95,q95old,perim
    end type geometry 



!composition
    type composition
      SEQUENCE
      real(kind(1.0D0)) :: globtau(5),psepplh_inf,psepplh_sup,qdivt,fuelmix,typ1,typ2,typ3,che,cxe,car,c_car, &
						& pradpos,pradfrac,psepb_q95AR,psep_r,comparray(14),protium,fcoreraditv=-1.d0
						integer :: imptype(3)=(/14,13,9/) ! 1 for intrinsic, 2 for psep control, 3 for SOL control or SOL intrinsic
	     real(kind(1.0D0)) :: fuelhe3=0.d0,che3,protfus=0.d0,he3fus=0.d0
    end type composition 




!pedestal
    type pedestal
      SEQUENCE
      real(kind(1.0D0)) :: nped,teped,nsep,tesep,rho_T,rho_n,pedscal   !should go into the solver
    end type pedestal 



!inputs
    type inputs
      SEQUENCE
      real(kind(1.0D0)) :: f_ni,pfus,nbcdeff,eccdeff,f_gw,Hfac_inp,pheatmax,& 
						& v_loop,f_gws,qnbi_psepfac,cxe_psepfac,pech,pnbi,car_qdivt, & 
						& qheat,qcd,qfus,spellet,fpellet,q_control, & 
						& x_heat(2),x_cd(2),x_fus(2),x_control(2), &
						& dx_heat(2),dx_cd(2),dx_fus(2),dx_control(2),nbi_energy, &
						& maxpauxor,contrpovs=0.d0,contrpovr=0.d0,betalim=0.d0,fpion=0.5d0, & 
						& fcdp=-1.d0,chisaw=0.d0,chisawpos=-1.d0,gamcdothers=1.d0, & 
						& sawpertau=1.d-6 !sawtooth period over confinement time
	integer :: PLH=6
    end type inputs 




!radial profiles and averages
    type radial_profiles
      SEQUENCE
      real(kind(1.0D0)), DIMENSION(:), ALLOCATABLE :: ne,Ti,Te,x,ndeut,ntrit, & 
						& jbs, jcd, jpar, & 
						&  ipol,qprof,Volum,g2,g3,vp,cc,& 
						& palph,nions,psi,gte,gti,gne,Nepg,Tepg,Tipg,bpol,gradro,shif,k,d
      real(kind(1.0D0)) :: av_ne,av_Ti,av_Te, zeff,av_ni,av_nd,av_nz,av_nhe
      real(kind(1.0D0)) :: av_Ten
      real(kind(1.0D0)), DIMENSION(:), ALLOCATABLE :: nprot,nhe3,nalf,nwol
    end type radial_profiles 



!MHD
    type MHD_EQ
      SEQUENCE
      real(kind(1.0D0)) :: Vp, Sp, Vloop, fbs, q , f_gwpedtop !, Hfac_out
      real(kind(1.0D0)) :: Bpolavg,betator,betapol,torsurf,ip_out,f_ni,equilcheck,betan,q_sep
      real(kind(1.0D0)) :: qstar,bp,rli,qoh
    end type MHD_EQ 



!loss power
    type power_losses
      SEQUENCE
      real(kind(1.0D0)) :: Psep, Prad, Wth, TAUeff, & 
						& H, Pfus, Pdiv, Tdiv, Pradcore, Pradsol, chifac0, PLH
      real(kind(1.0D0)) :: pradedge,psync,pbrehms,pline,& 
  & psepe,psepi,Hcorr,piepv,pohm,rplas,dfuelreq,tfuelreq,hepumpreq, &
	& qheat,qcd,qfus,pnbi,pech,peaux,piaux, & 
	& fusionrate,alpharate,betaft,palpe,palpi,tauee,tauei,qtot,pfusdd
 end type power_losses 



!end sutrcture
end module structs




