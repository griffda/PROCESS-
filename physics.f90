! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module physics_module

  !+ad_name  physics_module
  !+ad_summ  Module containing tokamak plasma physics routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  physics					
  !+ad_cont  betcom
  !+ad_cont  bootstrap_fraction_iter89
  !+ad_cont  bootstrap_fraction_nevins
  !+ad_cont  bootstrap_fraction_sauter
  !+ad_cont  bootstrap_fraction_wilson					   
  !+ad_cont  bpol
  !+ad_cont  culblm
  !+ad_cont  culcur
  !+ad_cont  culdlm
  !+ad_cont  fhfac
  !+ad_cont  fhz				
  !+ad_cont  igmarcal
  !+ad_cont  outplas
  !+ad_cont  outtim								   
  !+ad_cont  pcond
  !+ad_cont  phyaux
  !+ad_cont  plasma_composition
  !+ad_cont  pohm					
  !+ad_cont  radpwr
  !+ad_cont  rether
  !+ad_cont  vscalc
  !+ad_args  N/A
  !+ad_desc  This module contains all the primary plasma physics routines
  !+ad_desc  for a tokamak device.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  constraint_variables
  !+ad_call  current_drive_module
  !+ad_call  current_drive_variables
  !+ad_call  divertor_kallenbach_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  grad_func
  !+ad_call  heat_transport_variables
  !+ad_call  impurity_radiation_module
  !+ad_call  maths_library
  !+ad_call  numerics
  !+ad_call  physics_variables
  !+ad_call  plasmod_module
  !+ad_call  plasmod_variables
  !+ad_call  profiles_module
  !+ad_call  process_output
  !+ad_call  pulse_variables
  !+ad_call  startup_variables
  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  16/10/12 PJK Initial version of module
  !+ad_hist  16/10/12 PJK Added constants
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  17/10/12 PJK Added current_drive_module
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Changed private/public lists
  !+ad_hist  31/10/12 PJK Moved local common variables into module header
  !+ad_hist  05/11/12 PJK Added pulse_variables
  !+ad_hist  05/11/12 PJK Added startup_variables
  !+ad_hist  06/11/12 PJK Inserted routines outplas, outtim from outplas.f90
  !+ad_hist  03/01/13 PJK Removed denlim routine
  !+ad_hist  23/01/13 PJK Added stellarator_variables
  !+ad_hist  10/06/13 PJK Added tfcoil_variables
  !+ad_hist  12/09/13 PJK Removed svfdt,svfdt_orig,fpower,ffus; added bosch_hale
  !+ad_hist  19/02/14 PJK Added plasma_profiles
  !+ad_hist  24/02/14 PJK Moved plasma_profiles etc into new profiles_module
  !+ad_hist  26/03/14 PJK Renamed bootstrap fraction routines; added Sauter model
  !+ad_hist  13/05/14 PJK Added plasma_composition routine, impurity_radiation_module
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  01/10/14 PJK Added numerics
  !+ad_hist  20/05/15 RK  Added iscdens, fgwped for pedestal density scaling
  !+ad_hist  08/02/17 JM  Added Kallenbach model parameters
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use constraint_variables
  use current_drive_module
  use current_drive_variables
  use divertor_kallenbach_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use grad_func
  use heat_transport_variables
  use impurity_radiation_module
  use maths_library
  use numerics
  use physics_functions_module
  use physics_variables
  use plasmod_module
  use plasmod_variables
  use profiles_module
  use process_output
  use pulse_variables
  use startup_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables  					   

  implicit none

  private
  public :: betcom,bpol,fhfac,igmarcal,outplas,outtim,pcond,phyaux, &
       physics,plasma_composition,pohm,radpwr,rether
       
  !  Module-level variables
						   
  integer :: iscz
  real(kind(1.0D0)) :: photon_wall, rad_fraction
  real(kind(1.0D0)) :: total_plasma_internal_energy  ! [J]
  real(kind(1.0D0)) :: total_loss_power        ! [W]
  real(kind(1.0D0)) :: total_energy_conf_time  ! [s]


contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine physics

    !+ad_name  physics
    !+ad_summ  Routine to calculate tokamak plasma physics information
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates all the primary plasma physics
    !+ad_desc  characteristics for a tokamak device.
    !+ad_prob  None
    !+ad_call  beamfus
    !+ad_call  betcom
    !+ad_call  bootstrap_fraction_iter89
    !+ad_call  bootstrap_fraction_nevins
    !+ad_call  bootstrap_fraction_sauter
    !+ad_call  bootstrap_fraction_wilson
    !+ad_call  convert_Plasmod2PROCESS
    !+ad_call  cudriv
    !+ad_call  culblm
    !+ad_call  culcur
    !+ad_call  culdlm
    !+ad_call  palph
    !+ad_call  palph2
    !+ad_call  pcond
    !+ad_call  phyaux
    !+ad_call  plasma_composition
    !+ad_call  plasma_profiles
    !+ad_call  plasmod_EF
    !+ad_call  pohm
    !+ad_call  pthresh
    !+ad_call  radpwr
    !+ad_call  report_error
    !+ad_call  rether
    !+ad_call  setupPlasmod
    !+ad_call  vscalc
    !+ad_hist  20/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  04/12/95 PJK Added D-He3 relevant coding
    !+ad_hist  14/05/96 PJK Modified poloidal beta used in bootstrap formula
    !+ad_hisc               and added diamagnetic contribution
    !+ad_hist  10/06/96 PJK Added use of IWALLD in wall load calculation
    !+ad_hist  07/10/96 PJK Added new ICULBL=2 option
    !+ad_hist  17/09/97 PJK Added Greenwald density limit (added arguments
    !+ad_hisc               to CULDLM)
    !+ad_hist  01/04/98 PJK Changed PBREM to PRAD in argument list of PCOND,
    !+ad_hisc               added DNLA and IGNITE to arguments of BETCOM
    !+ad_hisc               and PCOND, and added other effects of IGNITE
    !+ad_hist  24/04/98 PJK Added IMPC, IMPFE, IMPO to arguments of BETCOM
    !+ad_hist  30/06/98 PJK Added XAREA to arguments of PCOND
    !+ad_hist  17/07/98 PJK Added call to PTHRESH
    !+ad_hist  19/01/99 PJK Added POWERHT to argument list of PCOND
    !+ad_hist  16/07/01 PJK Added KAPPAA to argument list of PCOND
    !+ad_hist  22/05/06 PJK Added IFALPHAP to argument list of PALPH2
    !+ad_hist  10/11/11 PJK Initial F90 version; retired routine CURREN
    !+ad_hisc               and switch ICULCR
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  17/12/12 PJK Added ZFEAR to argument lists of BETCOM, RADPWR
    !+ad_hist  18/12/12 PJK Added SAREA,AION to argument list of PTHRESH
    !+ad_hist  03/01/13 PJK Removed switch ICULDL and call to DENLIM
    !+ad_hist  11/04/13 PJK Removed switch IRES from POHM call
    !+ad_hist  12/06/13 PJK TAUP now global
    !+ad_hist  10/09/13 PJK Added FUSIONRATE,ALPHARATE,PROTONRATE to PALPH arguments
    !+ad_hist  11/09/13 PJK Removed idhe3, ftr, iiter usage
    !+ad_hist  27/11/13 PJK Added THEAT to VSCALC arguments
    !+ad_hist  27/11/13 PJK Added PPERIM to CULCUR arguments
    !+ad_hist  28/11/13 PJK Added PDTPV, PDHE3PV, PDDPV to PALPH arguments
    !+ad_hist  28/11/13 PJK Added current profile consistency option;
    !+ad_hist               Added IPROFILE, Q0, RLI to CULCUR arguments
    !+ad_hist  19/02/14 PJK Added pedestal profile model
    !+ad_hist  24/02/14 PJK Modified CULBST arguments
    !+ad_hist  26/03/14 PJK Converted BOOTST to a function;
    !+ad_hisc               introduced Sauter et al bootstrap model
    !+ad_hist  08/05/14 PJK Modified PHYAUX arguments
    !+ad_hist  14/05/14 PJK Added call to plasma_composition and new
    !+ad_hisc               impurity radiation calculations
    !+ad_hist  15/05/14 PJK Removed ffwal from iwalld=2 calculation
    !+ad_hist  19/05/14 PJK Clarified pcorerad vs pbrem; plrad --> pedgerad
    !+ad_hist  21/05/14 PJK Added ignite clause to pinj calculation
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  03/06/14 PJK Modifications for new power flow model
    !+ad_hist  24/06/14 PJK Corrected neutron wall load to account for gaps
    !+ad_hisc               in first wall
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  19/08/14 PJK Removed impfe usage
    !+ad_hist  01/10/14 PJK Added plhthresh
    !+ad_hist  01/04/15 JM  Added total transport power from scaling law
    !+ad_hist  11/09/15 MDK Resistive diffusion time
    !+ad_hist  10/11/16 HL  Added peakradwallload calculation
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  T. Hartmann and H. Zohm: Towards a 'Physics Design Guidelines for a
    !+ad_docc  DEMO Tokamak' Document, March 2012, EFDA Report
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: betat,betpth,fusrat,pddpv,pdtpv,pdhe3pv, &
         pinj,sbar,sigvdt,zion

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (icurr == 2) then
       q95 = q * 1.3D0 * (1.0D0 - eps)**0.6D0
    else
       q95 = q  !  i.e. input (or iteration variable) value
    end if

    if (ipedestal .ne. 3) then

       !  Calculate plasma composition
       if (imprad_model == 0) then
          call betcom(cfe0,dene,fdeut,ftrit,fhe3,ftritbm,ignite,impc,impo, &
               ralpne,rnbeam,te,zeff,abeam,afuel,aion,deni,dlamee,dlamie,dnalp, &
               dnbeam,dnitot,dnprot,dnz,falpe,falpi,rncne,rnone,rnfene,zeffai, &
               zion,zfear)
       else
          call plasma_composition
       end if
              
       !  Calculate plasma current
       call culcur(alphaj,alphap,bt,eps,icurr,iprofile,kappa,kappa95,p0, &
            pperim,q0,q,rli,rmajor,rminor,sf,triang,triang95,bp,qstar,plascur)

       !  Calculate density and temperature profile quantities
       !  If ipedestal = 1 and iscdens = 1 then set pedestal density to
       !    fgwped * Greenwald density limit
       !  Note: this used to be done before plasma current
       ! Issue #589 remove iscdens
       if (((ipedestal == 1).or.(ipedestal==2)).and.(fgwped >=0d0)) then
          neped = fgwped * 1.0D14 * plascur/(pi*rminor*rminor)
       endif
       if (((ipedestal == 1).or.(ipedestal==2)).and.(fgwsep >=0d0)) then
          nesep = fgwsep * 1.0D14 * plascur/(pi*rminor*rminor)
       end if
														
    else if (geom%counter.eq.0.d0) then
       !if plasmod_i_equiltype = 2 plascur is an input
       !This is not yet consistently implemented though and contradicts
       !usual PROCESS workflows where q is an input/interation variable
      
       
       !Note that alphap is 0 here!
       !alphap is only used for icurr=7 (Connor-Hastie model)
       call culcur(alphaj,alphap,bt,eps,icurr,iprofile,kappa,kappa95,p0, &
            pperim,q0,q,rli,rmajor,rminor,sf,triang,triang95,bp,qstar,plascur)
       
    endif
    
    ! Issue #413 Dependence of Pedestal Properties on Plasma Parameters
    ! ieped : switch for scaling pedestal-top temperature with plasma parameters
    if ((ipedestal >= 1) .and. (ieped == 1)) teped = t_eped_scaling()
    
    if (ipedestal .ne. 3)then
       
       call plasma_profiles
       
    else  ! Run PLASMOD 
         												 
       call setupPlasmod(num,geom,comp,ped,inp0,i_flag)

       if(verbose == 1) then

          open(32,file='plasmodsolprima.dat')
          write(32,*) 'num ',num
          write(32,*) 'geom ',geom
          write(32,*) 'comp ',comp
          write(32,*) 'ped ',ped
          write(32,*) 'inp0 ',inp0
          write(32,*) 'mhd ',mhd
          write(32,*) 'loss ',loss
          close(32)	   

       endif

       call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)
       
       if (verbose == 1) then
          open(32,file='plasmodsoldopo.dat')
          write(32,*) 'num ',num
          write(32,*) 'geom ',geom
          write(32,*) 'comp ',comp
          write(32,*) 'ped ',ped
          write(32,*) 'inp0 ',inp0
          write(32,*) 'mhd ',mhd
          write(32,*) 'loss ',loss
          close(32)
       endif

       call convert_Plasmod2PROCESS(geom,comp,ped,radp,mhd,loss,theat,tburn,&
							& fusrat)

    endif

    btot = sqrt(bt**2 + bp**2)
    betap = beta * ( btot/bp )**2

    !  Set PF coil ramp times

    if (lpulse /= 1) then

       if (tohsin == 0.0D0) then
          tohs = plascur/5.0D5
          tramp = tohs
          tqnch = tohs
       else
          tohs = tohsin
       end if

    else

       if (pulsetimings == 0.0D0) then
         ! tramp is input
         tohs = plascur/1.0D5
         tqnch = tohs

       else
         !  tohs is set either in INITIAL or INPUT, or by being
         !  iterated using limit equation 41.
         tramp = max(tramp,tohs)
         !tqnch = max(tqnch,tohs)
         tqnch = tohs
       end if

    end if

    !  Reset second tburn value (tburn0).
    !  This is used to ensure that the burn time is used consistently;
    !  see convergence loop in fcnvmc1, evaluators.f90

    tburn0 = tburn

    !  Pulse and down times : The reactor is assumed to be 'down'
    !  at all times outside of the plasma current flat-top period.
    !  The pulse length is the duration of non-zero plasma current

    tpulse = tohs + theat + tburn + tqnch
    tdown  = tramp + tohs + tqnch + tdwell

    !  Total cycle time

    tcycle = tramp + tohs + theat + tburn + tqnch + tdwell

    !  Calculate bootstrap current fraction using various models

    bscf_iter89 = bootstrap_fraction_iter89(aspect,beta,btot,cboot,plascur, &
         q95,q0,rmajor,vol)											   

    !Profile parameters are meaningless with ipedestal=3
    if (ipedestal.ne.3) then
       betat = beta * btot**2 / bt**2
       bscf_nevins = cboot * bootstrap_fraction_nevins(alphan,alphat,betat,bt,dene, &
            plascur,q95,q0,rmajor,rminor,ten,zeff)
       
       !  Wilson scaling uses thermal poloidal beta, not total
       betpth = (beta-betaft-betanb) * ( btot/bp )**2
       bscf_wilson = cboot * bootstrap_fraction_wilson(alphaj,alphap,alphat,beta,betpth, &
            q0,q95,rmajor,rminor,itart)
    endif
    
    bscf_sauter = cboot * bootstrap_fraction_sauter()

    if (ipedestal .ne. 3) then
       if (bscfmax < 0.0D0) then							   
          bootipf = abs(bscfmax)							   
       else
          if (ibss == 1) then
             bootipf = bscf_iter89
          else if (ibss == 2) then
             bootipf = bscf_nevins
          else if (ibss == 3) then
             bootipf = bscf_wilson
          else if (ibss == 4) then
             bootipf = bscf_sauter
          else
             idiags(1) = ibss ; call report_error(75)
          end if
          
          bootipf = min(bootipf,bscfmax)

       end if
       
       !  Bootstrap current fraction constrained to be less than
       !  or equal to the total fraction of the plasma current
       !  produced by non-inductive means (which also includes
       !  the current drive proportion)
       
       bootipf = min(bootipf,fvsbrnni)
       
    endif
       
    !  Fraction of plasma current produced by inductive means
    if (ipedestal .ne. 3) then
      facoh = max( 1.0D-10, (1.0D0 - fvsbrnni) )
    !   Fraction of plasma current produced by auxiliary current drive
      faccd = fvsbrnni - bootipf
    endif

    !  Auxiliary current drive power calculations
    
    if (irfcd /= 0) call cudriv(nout,0)
 
    if (ipedestal .ne. 3) then  ! otherwise replaced by PLASMOD variables

       !  Calculate fusion power + components
       call palph(alphan,alphat,deni,fdeut,fhe3,ftrit,ti,palppv,pchargepv,pneutpv, &
            sigvdt,fusionrate,alpharate,protonrate,pdtpv,pdhe3pv,pddpv)

       pdt = pdtpv * vol
       pdhe3 = pdhe3pv * vol
       pdd = pddpv * vol


       !  Calculate neutral beam slowing down effects
       !  If ignited, then ignore beam fusion effects

       if ((cnbeam /= 0.0D0).and.(ignite == 0)) then
          call beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie, &
               ealphadt,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol, &
               zeffai,betanb,dnbeam2,palpnb)
          fusionrate = fusionrate + 1.0D6*palpnb / (1.0D3*ealphadt*echarge) / vol
          alpharate = alpharate + 1.0D6*palpnb / (1.0D3*ealphadt*echarge) / vol
       end if

       pdt = pdt + 5.0D0*palpnb

       ! Create some derived values and add beam contribution to fusion power
       call palph2(bt,bp,dene,deni,dnitot,falpe,falpi,palpnb, &
            ifalphap,pchargepv,pneutpv,ten,tin,vol,palpmw,pneutmw,pchargemw,betaft, &
            palppv,palpipv,palpepv,pfuscmw,powfmw)

    endif
     
    !  Nominal mean neutron wall load on entire first wall area including divertor and beam holes
    !  Note that 'fwarea' excludes these, so they have been added back in.
    if (iwalld == 1) then
       wallmw = ffwal * pneutmw / sarea
    else
       wallmw = (1.0D0-fhcd-fdiv)*pneutmw / fwarea
    end if

    if (ipedestal .ne. 3) then ! otherwise replaced by PLASMOD variables
       
        !  Calculate ion/electron equilibration power

        call rether(alphan,alphat,dene,dlamie,te,ti,zeffai,piepv)

							  
       !  Calculate radiation power

       call radpwr(imprad_model,pbrempv,plinepv,psyncpv, &
            pcoreradpv,pedgeradpv,pradpv)
       
       pcoreradmw = pcoreradpv*vol
       pedgeradmw = pedgeradpv*vol
       pradmw = pradpv*vol

    endif
       
    ! MDK
    !  Nominal mean photon wall load on entire first wall area including divertor and beam holes
    !  Note that 'fwarea' excludes these, so they have been added back in.
    if (iwalld == 1) then
       photon_wall = ffwal * pradmw / sarea
    else
       photon_wall = (1.0D0-fhcd-fdiv)*pradmw / fwarea
    end if

    peakradwallload = photon_wall * peakfactrad

    if (ipedestal .ne. 3) then
       !  Calculate ohmic power
       call pohm(facoh,kappa95,plascur,rmajor,rminor,ten,vol,zeff, &
            pohmpv,pohmmw,rpfac,rplas)
       
       !  Calculate L- to H-mode power threshold for different scalings

       call pthresh(dene,dnla,bt,rmajor,kappa,sarea,aion,pthrmw)

       !  Enforced L-H power threshold value (if constraint 15 is turned on)

       plhthresh = pthrmw(ilhthresh)
        
       !  Power transported to the divertor by charged particles,
       !  i.e. excludes neutrons and radiation, and also NBI orbit loss power,
       !  which is assumed to be absorbed by the first wall												   
       if (ignite == 0) then
          pinj = pinjmw
       else
          pinj = 0.0D0
       end if
       pdivt = falpha*palpmw + pchargemw + pinj + pohmmw - pradmw

       !  The following line is unphysical, but prevents -ve sqrt argument
       !  Should be obsolete if constraint eqn 17 is turned on
       pdivt = max(0.001D0, pdivt)
       
    endif
 
    ! Resistive diffusion time = current penetration time ~ mu0.a^2/resistivity
    res_time = 2.0D0*rmu0*rmajor / (rplas*kappa95)							   

    !  Power transported to the first wall by escaped alpha particles

    palpfwmw = palpmw * (1.0D0-falpha)

    !  Density limit

    call culdlm(bt,idensl,pdivt,plascur,prn1,qstar,q95, &
         rmajor,rminor,sarea,zeff,dlimit,dnelimt)

    if (ipedestal .ne. 3) then
    
       !  Calculate transport losses and energy confinement time using the
       !  chosen scaling law
       call pcond(afuel,palpmw,aspect,bt,dnitot,dene,dnla,eps,hfact, &
            iinvqd,isc,ignite,kappa,kappa95,kappaa,pchargemw,pinjmw, &
            plascur,pcoreradpv,rmajor,rminor,te,ten,tin,q95,qstar,vol, &
            xarea,zeff,ptrepv,ptripv,tauee,tauei,taueff,powerht)
       
       ptremw = ptrepv*vol
       ptrimw = ptripv*vol
       !  Total transport power from scaling law (MW)
       !pscalingmw = ptremw + ptrimw !KE - why is this commented?

       ! Calculate Volt-second requirements
       call vscalc(csawth,eps,facoh,gamma,kappa,rmajor,rplas, &
            plascur,theat,tburn,phiint,rli,rlp,vsbrn,vsind,vsres,vsstt)

       !  Calculate auxiliary physics related information
       sbar = 1.0D0
       call phyaux(aspect,dene,deni,fusionrate,alpharate,plascur,sbar,dnalp, &
            taueff,vol,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

    endif

    !ptremw = ptrepv*vol
    !ptrimw = ptripv*vol
    !  Total transport power from scaling law (MW)
    pscalingmw = ptremw + ptrimw
       
    !!!!vscal and phyaux should be replaced by PLASMOD output ipedestal 3 - is this done?															  

    !  Calculate beta limit

    if (iprofile == 0) then

       if (gtscale == 1) then
          dnbeta = 2.7D0 * (1.0D0 + 5.0D0*eps**3.5D0)
          !... otherwise use input value for dnbeta
       end if

    else
       !  Relation between beta limit and plasma internal inductance
       !  Hartmann and Zohm

       dnbeta = 4.0D0 * rli
    end if

    call culblm(bt,dnbeta,plascur,rminor,betalim)

    ! Calculate some derived quantities that may not have been defined earlier
    total_loss_power = 1d6 * (falpha*palpmw+pchargemw+pohmmw+pinjmw)
    rad_fraction = 1.0D6*pradmw / total_loss_power
    total_plasma_internal_energy = 1.5D0*beta*btot*btot/(2.0D0*rmu0)*vol
    total_energy_conf_time = total_plasma_internal_energy / total_loss_power											  

    if (verbose == 1) then
       !write some PROCESS outputs that have been generated by PLASMOD
       open(32,file='processOutput_plasmod.dat')
       write(32,*) 'te0 ',te0, ' ne0 ',ne0
       write(32,*) 'teped ',teped, ' neped ',neped
       write(32,*) 'nesep ',nesep, ' vol ',vol
       write(32,*) 'plascur ',plascur, ' bootipf ',bootipf
       write(32,*) 'q95 ',q95, ' qstar ',qstar
       write(32,*) 'kappaa ',kappaa
       write(32,*) 'FIELDS -----'
       write(32,*) 'bp ',bp, ' bt ',bt
       write(32,*) 'btot ', btot
       write(32,*) 'BETA -----'
       write(32,*) 'betap ', betap, ' betaft ',betaft
       write(32,*) 'normalised_total_beta ',normalised_total_beta
       write(32,*) 'RATES -----'
       write(32,*) 'fusionrate ',fusionrate, ' alpharate ',alpharate
       write(32,*) 'POWERS -----'
       write(32,*) 'palpmw ',palpmw, ' pchargemw ',pchargemw, ' pneutmw ',pneutmw
       write(32,*) 'palppv ',palppv, ' pchargepv ',pchargepv, ' pneutpv ',pneutpv
       write(32,*) 'pdt ',pdt, ' pdhe3 ',pdhe3, ' pdd ',pdd
       write(32,*) 'palpepv ',palpepv, ' palpipv ',palpipv
       write(32,*) 'powfmw ',powfmw, ' pfuscmw ',pfuscmw, ' hfact ',hfact
       write(32,*) 'ptrepv ',ptrepv, ' ptripv ',ptripv
       write(32,*) 'powerht ',powerht
       write(32,*) 'CONFINEMENT -----'
       write(32,*) 'tauee ',tauee, ' tauei ',tauei
       write(32,*) 'taueff ',taueff, ' taup ',taup, ' dntau ',dntau
       write(32,*) 'NEUTRAL BEAM -----'
       write(32,*) 'betanb ',betanb, ' dnbeam2 ',dnbeam2, ' palpnb ',palpnb
       write(32,*) 'IMPURITIES -----'
       write(32,*) 'ralpne ',ralpne, ' fimp_13 ',fimp(13)
       write(32,*) 'RADIATION -----'
       write(32,*) 'rad_fraction ', rad_fraction, ' pradmw ',pradmw
       write(32,*) 'pcoreradmw ', pcoreradmw, ' pedgeradmw ',pedgeradmw
       write(32,*) 'psyncpv ', psyncpv, ' pbrempv ',pbrempv
       write(32,*) 'plinepv ', plinepv, ' piepv ',piepv
       write(32,*) 'pinjemw ', pinjemw, ' pinjimw ',pinjimw
       write(32,*) 'FUELLING -----'
       write(32,*) 'qfuel ',qfuel, ' burnup ',burnup, ' rndfuel ',rndfuel
       write(32,*) 'PLASMA INDUCTANCE -----'
       write(32,*) 'phiint ', phiint, ' rlp ',rlp
       write(32,*) 'vsbrn ', vsbrn, ' vsind ',vsind
       write(32,*) 'vsres ', vsres, ' vsstt ',vsstt
       write(32,*) 'PLASMA RESISTANCE -----'
       write(32,*) 'pohmpv ', pohmpv, ' pohmmw ',pohmmw
       write(32,*) 'rpfac ', rpfac, ' rplas ',rplas
       close(32)
    endif
    
  end subroutine physics

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 ! function eped_warning()
 !     ! Issue #413.
 !     logical :: eped_warning
 !     eped_warning=.false.
 !     if((triang<0.399d0).or.(triang>0.601d0)) eped_warning=.true.
 !     if((kappa<1.499d0).or.(kappa>2.001d0)) eped_warning=.true.
 !     if((plascur<9.99d6).or.(plascur>20.01d6)) eped_warning=.true.
 !     if((rmajor<6.99d0).or.(rmajor>11.01d0)) eped_warning=.true.
 !     if((rminor<1.99d0).or.(rminor>3.501d0))eped_warning=.true.
 !     if((normalised_total_beta<1.99d0).or.(normalised_total_beta>3.01d0))eped_warning=.true.
 !     if(tesep>0.5)eped_warning=.true.
 ! end function eped_warning

     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  function eped_warning()
      ! Issue #413.  MDK 26/2/18: improved output
      character(len=100) :: eped_warning, info_string
      eped_warning=''
      info_string = ''

      if((triang<0.399d0).or.(triang>0.601d0)) then
          write(info_string , '(1pe13.4)') triang
          eped_warning='triang = '//trim(info_string)
      endif
      if((kappa<1.499d0).or.(kappa>2.001d0)) then
          write(info_string , '(1pe13.4)') kappa
          eped_warning=trim(eped_warning)//'  kappa = '//trim(info_string)
      endif
      if((plascur<9.99d6).or.(plascur>20.01d6)) then
          write(info_string , '(1pe13.4)') plascur
          eped_warning=trim(eped_warning)//'  plascur = '//trim(info_string)
      endif
      if((rmajor<6.99d0).or.(rmajor>11.01d0)) then
          write(info_string , '(1pe13.4)') rmajor
          eped_warning=trim(eped_warning)//'  rmajor = '//trim(info_string)
      endif
      if((rminor<1.99d0).or.(rminor>3.501d0))then
          write(info_string , '(1pe13.4)') rminor
          eped_warning=trim(eped_warning)//'  rminor = '//trim(info_string)
      endif
      if((normalised_total_beta<1.99d0).or.(normalised_total_beta>3.01d0))then
          write(info_string , '(1pe13.4)') normalised_total_beta
          eped_warning=trim(eped_warning)//'  normalised_total_beta = '//trim(info_string)
      endif
      if(tesep>0.5)then
          write(info_string , '(1pe13.4)') tesep
          eped_warning=trim(eped_warning)//'  tesep = '//trim(info_string)
      endif
  end function eped_warning

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function bootstrap_fraction_iter89(aspect,beta,bt,cboot,plascur,q95,q0,rmajor,vol)

    !+ad_name  bootstrap_fraction_iter89
    !+ad_summ  Original ITER calculation of bootstrap-driven fraction
    !+ad_summ  of the plasma current.
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  aspect  : input real : plasma aspect ratio
    !+ad_args  beta    : input real : plasma total beta
    !+ad_args  bt      : input real : toroidal field on axis (T)
    !+ad_args  cboot   : input real : bootstrap current fraction multiplier
    !+ad_args  plascur : input real : plasma current (A)
    !+ad_args  q95     : input real : safety factor at 95% surface
    !+ad_args  q0      : input real : central safety factor
    !+ad_args  rmajor  : input real : plasma major radius (m)
    !+ad_args  vol     : input real : plasma volume (m3)
    !+ad_desc  This routine performs the original ITER calculation of the
    !+ad_desc  plasma current bootstrap fraction.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  20/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  23/05/06 PJK Prevented negative square roots from being attempted
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Removed pi from argument list
    !+ad_hist  26/03/14 PJK Converted to a function; renamed from BOOTST
    !+ad_hist  01/10/14 PJK Renamed argument q to q95
    !+ad_stat  Okay
    !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
    !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: bootstrap_fraction_iter89

    !  Arguments

    real(kind(1.0D0)), intent(in) :: aspect, beta, bt, cboot, &
         plascur, q95, q0, rmajor, vol

    !  Local variables

    real(kind(1.0D0)) :: betapbs, bpbs, cbs, xbs, bootipf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    xbs = min(10.0D0, q95/q0)
    cbs = cboot * (1.32D0 - 0.235D0*xbs + 0.0185D0*xbs**2)
    bpbs = rmu0*plascur/(2.0D0*pi*sqrt(vol/(2.0D0* pi**2 *rmajor)) )
    betapbs = beta*bt**2 / bpbs**2

    if (betapbs <= 0.0D0) then  !  only possible if beta <= 0.0
       bootipf = 0.0D0
    else
       bootipf = cbs * ( betapbs/sqrt(aspect) )**1.3D0
    end if

    bootstrap_fraction_iter89 = bootipf

  end function bootstrap_fraction_iter89

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function bootstrap_fraction_nevins(alphan,alphat,betat,bt,dene,plascur, &
       q95,q0,rmajor,rminor,ten,zeff)

    !+ad_name  bootstrap_fraction_nevins
    !+ad_summ  Bootstrap current fraction from Nevins et al scaling
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  bsinteg
    !+ad_args  alphan : input real :  density profile index
    !+ad_args  alphat : input real :  temperature profile index
    !+ad_args  betat  : input real :  total plasma beta (with respect to the toroidal
    !+ad_argc                         field)
    !+ad_args  bt     : input real :  toroidal field on axis (T)
    !+ad_args  dene   : input real :  electron density (/m3)
    !+ad_args  plascur: input real :  plasma current (A)
    !+ad_args  q0     : input real :  central safety factor
    !+ad_args  q95    : input real :  safety factor at 95% surface
    !+ad_args  rmajor : input real :  plasma major radius (m)
    !+ad_args  rminor : input real :  plasma minor radius (m)
    !+ad_args  ten    : input real :  density weighted average plasma temperature (keV)
    !+ad_args  zeff   : input real :  plasma effective charge
    !+ad_desc  This function calculates the bootstrap current fraction,
    !+ad_desc  using the Nevins et al method, 4/11/90.
    !+ad_prob  None
    !+ad_call  bsinteg
    !+ad_call  quanc8
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_hist  20/02/14 PJK Removed unnecessary use of shared variables;
    !+ad_hist               Corrected error in peak electron beta
    !+ad_hist  24/02/14 PJK Re-corrected peak electron beta (version prior to
    !+ad_hisc               previous change was correct after all!)
    !+ad_hist  26/03/14 PJK Renamed from FNEWBS
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: bootstrap_fraction_nevins

    !  Arguments

    real(kind(1.0D0)), intent(in) :: alphan,alphat,betat,bt,dene,plascur, &
         q0,q95,rmajor,rminor,ten,zeff

    !  Local variables

    integer :: nofun
    real(kind(1.0D0)) :: aibs,ainteg,betae0,dum1,fibs,flag

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate peak electron beta

    betae0 = ne0 * te0 * 1.0D3*echarge / ( bt**2 /(2.0D0*rmu0) )

    !  Call integration routine

    call quanc8(bsinteg,0.0D0,0.999D0,0.001D0,0.001D0,ainteg,dum1, &
         nofun,flag)

    !  Calculate bootstrap current and fraction

    aibs = 2.5D0 * betae0 * rmajor * bt * q95 * ainteg
    fibs = 1.0D6 * aibs / plascur

    bootstrap_fraction_nevins = fibs

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function bsinteg(y)

      !+ad_name  bsinteg
      !+ad_summ  Integrand function for Nevins et al bootstrap current scaling
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  y : input real : abscissa of integration, = normalised minor radius
      !+ad_desc  This function calculates the integrand function for the
      !+ad_desc  Nevins et al bootstrap current scaling, 4/11/90.
      !+ad_prob  No account is taken of pedestal profiles.
      !+ad_call  None
      !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
      !+ad_hist  10/11/11 PJK Initial F90 version
      !+ad_hist  20/02/14 PJK Removed unnecessary use of shared variables
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: bsinteg

      !  Arguments

      real(kind(1.0D0)), intent(in) :: y

      !  Local variables

      real(kind(1.0D0)) :: alphai,al1,al2,a1,a2,betae,c1,c2,c3, &
           d,del,pratio,q,x,z

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Constants for fit to q-profile

      c1 = 1.0D0
      c2 = 1.0D0
      c3 = 1.0D0

      !  Compute average electron beta

      betae = dene*ten*1.0D3*echarge/(bt**2/(2.0D0*rmu0))

      del = rminor*sqrt(y)/rmajor
      x = (1.46D0*sqrt(del) + 2.4D0*del)/(1.0D0 - del)**1.5D0
      z = zeff
      d = 1.414D0*z + z*z + x*(0.754D0 + 2.657D0*z + 2.0D0*z*z) &
           + x*x*(0.348D0 + 1.243D0*z + z*z)
      al2 = -x*(0.884D0 + 2.074D0*z)/d
      a2 = alphat*(1.0D0-y)**(alphan+alphat-1.0D0)
      alphai = -1.172D0/(1.0D0 + 0.462D0*x)
      a1 = (alphan+alphat)*(1.0D0-y)**(alphan+alphat-1.0D0)
      al1 = x*(0.754D0+2.21D0*z+z*z+x*(0.348D0+1.243D0*z+z*z))/d

      !  q-profile

      q = q0 + (q95-q0)*(c1*y + c2*y*y + c3*y**3)/(c1+c2+c3)

      pratio = (betat - betae) / betae

      bsinteg = (q/q95)*(al1*(a1 + pratio*(a1+alphai*a2) ) + al2*a2 )

    end function bsinteg

  end function bootstrap_fraction_nevins

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function bootstrap_fraction_wilson(alphaj,alphap,alphat,beta,betpth, &
       q0,qpsi,rmajor,rminor,itart)

    !+ad_name  bootstrap_fraction_wilson
    !+ad_summ  Bootstrap current fraction from Wilson et al scaling
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  alphaj  : input real :  current profile index
    !+ad_args  alphap  : input real :  pressure profile index
    !+ad_args  alphat  : input real :  temperature profile index
    !+ad_args  beta    : input real :  total beta
    !+ad_args  betpth  : input real :  thermal component of poloidal beta
    !+ad_args  q0      : input real :  safety factor on axis
    !+ad_args  qpsi    : input real :  edge safety factor
    !+ad_args  rmajor  : input real :  major radius (m)
    !+ad_args  rminor  : input real :  minor radius (m)
    !+ad_args  itart   : input integer :  switch denoting tight aspect ratio option
    !+ad_desc  This function calculates the bootstrap current fraction
    !+ad_desc  using the numerically fitted algorithm written by Howard Wilson.
    !+ad_prob  No account is taken of pedestal profiles.
    !+ad_call  report_error
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  14/05/96 PJK Modified to use THERMAL poloidal beta, and
    !+ad_hisc               added diamagnetic term at tight aspect ratio
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_hist  20/02/14 PJK alphap now calculated elsewhere
    !+ad_hist  24/02/14 PJK Swapped alphan for alphap in argument list
    !+ad_hist  26/03/14 PJK Renamed from CULBST
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !+ad_docs  H. R. Wilson, Nuclear Fusion <B>32</B> (1992) 257
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: bootstrap_fraction_wilson

    !  Arguments

    integer, intent(in) :: itart
    real(kind(1.0D0)), intent(in) :: alphaj,alphap,alphat,beta,betpth, &
         q0,qpsi,rmajor,rminor

    !  Local variables

    integer :: i
    real(kind(1.0D0)), dimension(12) :: a, b
    real(kind(1.0D0)) :: aj,alfpnw,alftnw,eps1,r1,r2, &
         saj,seps1,sss,termj,termp,termt,term1,term2,z

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  alphap, alphat and alphaj are indices relevant to profiles of
    !  the form
    !             p = p0.(1-(r/a)**2)**alphap, etc.
    !
    !  Convert these indices to those relevant to profiles of the form
    !             p = p0.psi**alfpnw, etc.

    term1 = log(0.5D0)
    term2 = log(q0/qpsi)

    termp = 1.0D0-0.5D0**(1.0D0/alphap)
    termt = 1.0D0-0.5D0**(1.0D0/alphat)
    termj = 1.0D0-0.5D0**(1.0D0/alphaj)

    alfpnw = term1/log( log( (q0+(qpsi-q0)*termp)/qpsi )/term2)
    alftnw = term1/log( log( (q0+(qpsi-q0)*termt)/qpsi )/term2)
    aj     = term1/log( log( (q0+(qpsi-q0)*termj)/qpsi )/term2)

    !  Crude check for NaN errors or other illegal values...

    if ((aj /= aj).or.(alfpnw /= alfpnw).or.(alftnw /= alftnw).or.(aj <= 0.0D0)) then
       fdiags(1) = aj ; fdiags(2) = alfpnw ; fdiags(3) = alftnw ; fdiags(4) = aj
       call report_error(76)
    end if

    !  Ratio of ionic charge to electron charge

    z = 1.0D0

    !  Inverse aspect ratio: r2 = maximum plasma radius, r1 = minimum

    r2 = rmajor+rminor
    r1 = rmajor-rminor
    eps1 = (r2-r1)/(r2+r1)

    !  Coefficients fitted using least squares techniques

    saj = sqrt(aj)

    a(1)  =    1.41D0*(1.0D0-0.28D0*saj)*(1.0D0+0.12D0/z)
    a(2)  =    0.36D0*(1.0D0-0.59D0*saj)*(1.0D0+0.8D0/z)
    a(3)  =   -0.27D0*(1.0D0-0.47D0*saj)*(1.0D0+3.0D0/z)
    a(4)  =  0.0053D0*(1.0D0+5.0D0/z)
    a(5)  =   -0.93D0*(1.0D0-0.34D0*saj)*(1.0D0+0.15D0/z)
    a(6)  =   -0.26D0*(1.0D0-0.57D0*saj)*(1.0D0-0.27D0*z)
    a(7)  =   0.064D0*(1.0D0-0.6D0*aj+0.15D0*aj*aj)*(1.0D0+7.6D0/z)
    a(8)  = -0.0011D0*(1.0D0+9.0D0/z)
    a(9)  =   -0.33D0*(1.0D0-aj+0.33D0*aj*aj)
    a(10) =   -0.26D0*(1.0D0-0.87D0/saj-0.16D0*aj)
    a(11) =   -0.14D0*(1.0D0-1.14D0/saj-0.45D0*saj)
    a(12) = -0.0069D0

    seps1 = sqrt(eps1)

    b(1)  = 1.0D0
    b(2)  = alfpnw
    b(3)  = alftnw
    b(4)  = alfpnw*alftnw
    b(5)  = seps1
    b(6)  = alfpnw*seps1
    b(7)  = alftnw*seps1
    b(8)  = alfpnw*alftnw*seps1
    b(9)  = eps1
    b(10) = alfpnw*eps1
    b(11) = alftnw*eps1
    b(12) = alfpnw*alftnw*eps1

    sss = 0.0D0
    do i = 1,12
       sss = sss + a(i)*b(i)
    end do

    !  Empirical bootstrap current fraction

    bootstrap_fraction_wilson = seps1 * betpth * sss

    !  Diamagnetic contribution to the bootstrap fraction
    !  at tight aspect ratio.
    !  Tim Hender fit

    if (itart == 1) then
       bootstrap_fraction_wilson = bootstrap_fraction_wilson + beta/2.8D0
    end if

  end function bootstrap_fraction_wilson

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function bootstrap_fraction_sauter()

    !+ad_name  bootstrap_fraction_sauter
    !+ad_summ  Bootstrap current fraction from Sauter et al scaling
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  beta_poloidal_local
    !+ad_cont  beta_poloidal_local_total
    !+ad_cont  nues
    !+ad_cont  nuee
    !+ad_cont  coulg
    !+ad_cont  nuis
    !+ad_cont  nui
    !+ad_cont  dcsa
    !+ad_cont  hcsa
    !+ad_cont  xcsa
    !+ad_cont  tpf
    !+ad_args  None
    !+ad_desc  This function calculates the bootstrap current fraction
    !+ad_desc  using the Sauter, Angioni and Lin-Liu scaling.
    !+ad_desc  <P>The code was extracted from the ASTRA code, and was
    !+ad_desc  supplied by Emiliano Fable, IPP Garching
    !+ad_desc  (private communication).
    !+ad_prob  None
    !+ad_call  nprofile
    !+ad_call  tprofile
    !+ad_call  dcsa
    !+ad_call  hcsa
    !+ad_call  xcsa
    !+ad_hist  26/03/14 PJK Initial version
    !+ad_hist  15/05/14 PJK Corrections made as per Fable's e-mail, 15/05/2014
    !+ad_stat  Okay
    !+ad_docs  O. Sauter, C. Angioni and Y. R. Lin-Liu,
    !+ad_docc    Physics of Plasmas <B>6</B> (1999) 2834
    !+ad_docs  O. Sauter, C. Angioni and Y. R. Lin-Liu, (ERRATA)
    !+ad_docc    Physics of Plasmas <B>9</B> (2002) 5140
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: bootstrap_fraction_sauter

    !  Arguments

    !  Local variables

    integer, parameter :: nr = 200
    integer :: ir
    real(kind(1.0D0)) :: da,drho,iboot,jboot,roa
    real(kind(1.0D0)) :: dlogne_drho,dlogte_drho,dlogti_drho
    real(kind(1.0D0)), dimension(nr) :: amain,mu,ne,ni,rho,sqeps,tempe,tempi,zef,zmain

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Populate profile arrays

    do ir = 1,nr
       roa = dble(ir)/nr
       rho(ir) = sqrt(xarea/pi) * roa !  local circularised minor radius (m)
       sqeps(ir) = sqrt(roa * rminor/rmajor)

       ne(ir) = 1.0D-19 * nprofile(roa,rhopedn,ne0,neped,nesep,alphan)
       ni(ir) = dnitot/dene * ne(ir)
       tempe(ir) = tprofile(roa,rhopedt,te0,teped,tesep,alphat,tbeta)
       tempi(ir) = ti/te * tempe(ir)

       zef(ir) = zeff  !  Flat Zeff profile assumed

       !  mu = 1/safety factor
       !  Parabolic q profile assumed

       mu(ir) = 1.0D0 / (q0 + (q-q0)*roa**2)
       amain(ir) = afuel  !  fuel ion mass
       zmain(ir) = 1.0D0 + fhe3  !  sum(Zi.ni)/sum(ni) over fuel ions i
    end do

    !  Ensure that density and temperature values are not zero at edge

    if (ne(nr) == 0.0D0) then
       ne(nr) = 1.0D-4*ne(nr-1)
       ni(nr) = 1.0D-4*ni(nr-1)
    end if

    if (tempe(nr) == 0.0D0) then
       tempe(nr) = 1.0D-4*tempe(nr-1)
       tempi(nr) = 1.0D-4*tempi(nr-1)
    end if

    !  Calculate total bootstrap current (MA) by summing along profiles

    iboot = 0.0D0
    do ir = 1,nr

       if (ir == nr) then
          jboot = 0.0D0
          da = 0.0D0
       else
          drho = rho(ir+1) - rho(ir)
          da = 2.0D0*pi*rho(ir)*drho  !  area of annulus

          dlogte_drho = (log(tempe(ir+1)) - log(tempe(ir))) / drho
          dlogti_drho = (log(tempi(ir+1)) - log(tempi(ir))) / drho
          dlogne_drho = (log(ne(ir+1)) - log(ne(ir))) / drho

          !  The factor of 0.5 below arises because in ASTRA the logarithms
          !  are coded as (e.g.):  (Te(j+1)-Te(j))/(Te(j+1)+Te(j)), which
          !  actually corresponds to grad(log(Te))/2. So the factors dcsa etc.
          !  are a factor two larger than one might otherwise expect.

          jboot = 0.5D0 * ( dcsa(ir,nr) * dlogne_drho &
               + hcsa(ir,nr) * dlogte_drho &
               + xcsa(ir,nr) * dlogti_drho )
          jboot = -bt/(0.2D0*pi*rmajor) * rho(ir)*mu(ir) * jboot  !  MA/m2
       end if

       iboot = iboot + da*jboot  !  MA

    end do

    bootstrap_fraction_sauter = 1.0D6 * iboot/plascur

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function beta_poloidal_local(j,nr)

      !+ad_name  beta_poloidal_local
      !+ad_summ  Local beta poloidal calculation
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_args  nr : input integer : maximum value of j
      !+ad_desc  This function calculates the local beta poloidal.
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_desc  <P>beta poloidal = 4*pi*ne*Te/Bpo**2
      !+ad_prob  PJK: I do not understand why it should be 4*pi*... instead
      !+ad_prob  of 8*pi*... Presumably it is because of a strange ASTRA
      !+ad_prob  method similar to that noted above in the calculation of jboot.
      !+ad_call  None
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_stat  Okay
      !+ad_docs  Pereverzev, 25th April 1989 (?)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: beta_poloidal_local

      !  Arguments

      integer, intent(in) :: j, nr

      !  Local variables

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (j /= nr)  then
         beta_poloidal_local = 1.6D-4*pi * (ne(j+1)+ne(j)) * (tempe(j+1)+tempe(j))
      else
         beta_poloidal_local = 6.4D-4*pi * ne(j)*tempe(j)
      end if

      beta_poloidal_local = beta_poloidal_local * &
           ( rmajor/(bt*rho(j)*abs(mu(j)+1.0D-4)) )**2

    end function beta_poloidal_local

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function beta_poloidal_local_total(j,nr)

      !+ad_name  beta_poloidal_local_total
      !+ad_summ  Local beta poloidal calculation, including ion pressure
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_args  nr : input integer : maximum value of j
      !+ad_desc  This function calculates the local total beta poloidal.
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_desc  <P>beta poloidal = 4*pi*(ne*Te+ni*Ti)/Bpo**2
      !+ad_desc  where ni is the sum of all ion densities (thermal)
      !+ad_prob  PJK: I do not understand why it should be 4*pi*... instead
      !+ad_prob  of 8*pi*... Presumably it is because of a strange ASTRA
      !+ad_prob  method similar to that noted above in the calculation of jboot.
      !+ad_call  None
      !+ad_hist  15/05/14 PJK New routine, which includes the ion pressure contribution
      !+ad_stat  Okay
      !+ad_docs  Pereverzev, 25th April 1989 (?)
      !+ad_docs  E Fable, private communication, 15th May 2014
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: beta_poloidal_local_total

      !  Arguments

      integer, intent(in) :: j, nr

      !  Local variables

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (j /= nr)  then
         beta_poloidal_local_total = 1.6D-4*pi * ( &
              ( (ne(j+1)+ne(j)) * (tempe(j+1)+tempe(j)) ) + &
              ( (ni(j+1)+ni(j)) * (tempi(j+1)+tempi(j)) ) )
      else
         beta_poloidal_local_total = 6.4D-4*pi * (ne(j)*tempe(j) + ni(j)*tempi(j))
      end if

      beta_poloidal_local_total = beta_poloidal_local_total * &
           ( rmajor/(bt*rho(j)*abs(mu(j)+1.0D-4)) )**2

    end function beta_poloidal_local_total

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function nues(j)

      !+ad_name  nues
      !+ad_summ  Relative frequency of electron collisions
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_desc  This function calculates the relative frequency of electron
      !+ad_desc  collisions: <I>NU* = Nuei*q*Rt/eps**1.5/Vte</I>
      !+ad_desc  The electron-ion collision frequency NUEI=NUEE*1.4*ZEF is
      !+ad_desc  used.
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  None
      !+ad_call  nuee
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_stat  Okay
      !+ad_docs  Yushmanov, 30th April 1987 (?)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: nues

      !  Arguments

      integer, intent(in) :: j

      !  Local variables

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      nues = nuee(j) * 1.4D0*zef(j)*rmajor / &
           abs(mu(j)*(sqeps(j)**3)*sqrt(tempe(j))*1.875D7)

    end function nues

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function nuee(j)

      !+ad_name  nuee
      !+ad_summ  Frequency of electron-electron collisions
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_desc  This function calculates the frequency of electron-electron
      !+ad_desc  collisions (Hz): <I>NUEE = 4*SQRT(pi)/3*Ne*e**4*lambd/
      !+ad_desc  SQRT(Me)/Te**1.5</I>
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  None
      !+ad_call  coulg
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_stat  Okay
      !+ad_docs  Yushmanov, 25th April 1987 (?),
      !+ad_docc  updated by Pereverzev, 9th November 1994 (?)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: nuee

      !  Arguments

      integer, intent(in) :: j

      !  Local variables

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      nuee = 670.0D0 * coulg(j) * ne(j) / ( tempe(j)*sqrt(tempe(j)) )

    end function nuee

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function coulg(j)

      !+ad_name  coulg
      !+ad_summ  Coulomb logarithm
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_desc  This function calculates the Coulomb logarithm, valid
      !+ad_desc  for e-e collisions (T_e > 0.01 keV), and for
      !+ad_desc  e-i collisions (T_e > 0.01*Zeff^2) (Alexander, 9/5/1994).
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  None
      !+ad_call  coulg
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_stat  Okay
      !+ad_docs  C. A. Ordonez and M. I. Molina, Phys. Plasmas <B>1</B> (1994) 2515
      !+ad_docs  Rev. Mod. Phys., V.48, Part 1 (1976) 275
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: coulg

      !  Arguments

      integer, intent(in) :: j

      !  Local variables

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      coulg = 15.9D0 - 0.5D0*log(ne(j)) + log(tempe(j))

    end function coulg

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function nuis(j)

      !+ad_name  nuis
      !+ad_summ  Relative frequency of ion collisions
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_desc  This function calculates the relative frequency of ion
      !+ad_desc  collisions: <I>NU* = Nui*q*Rt/eps**1.5/Vti</I>
      !+ad_desc  The full ion collision frequency NUI is used.
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  None
      !+ad_call  nui
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_stat  Okay
      !+ad_docs  Yushmanov, 30th April 1987 (?)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: nuis

      !  Arguments

      integer, intent(in) :: j

      !  Local variables

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      nuis = 3.2D-6 * nui(j)*rmajor / ( abs(mu(j)+1.0D-4) * &
           sqeps(j)**3 * sqrt(tempi(j)/amain(j)) )

    end function nuis

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function nui(j)

      !+ad_name  nui
      !+ad_summ  Full frequency of ion collisions
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_desc  This function calculates the full frequency of ion
      !+ad_desc  collisions (Hz).
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      real(kind(1.0D0)) :: nui

      !  Arguments

      integer, intent(in) :: j

      !  Local variables

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !	Coulomb logarithm = 15 is used

      nui = zmain(j)**4 * ni(j) * 322.0D0 / ( tempi(j)*sqrt(tempi(j)*amain(j)) )

    end function nui

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function dcsa(j,nr)

      !+ad_name  dcsa
      !+ad_summ  Grad(ln(ne)) coefficient in the Sauter bootstrap scaling
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_args  nr : input integer : maximum value of j
      !+ad_desc  This function calculates the coefficient scaling grad(ln(ne))
      !+ad_desc  in the Sauter bootstrap current scaling.
      !+ad_desc  Code by Angioni, 29th May 2002.
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  None
      !+ad_call  beta_poloidal_local_total
      !+ad_call  nues
      !+ad_call  tpf
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_hist  15/05/14 PJK Corrections made as per Fable's e-mail
      !+ad_stat  Okay
      !+ad_docs  O. Sauter, C. Angioni and Y. R. Lin-Liu,
      !+ad_docc    Physics of Plasmas <B>6</B> (1999) 2834
      !+ad_docs  O. Sauter, C. Angioni and Y. R. Lin-Liu, (ERRATA)
      !+ad_docc    Physics of Plasmas <B>9</B> (2002) 5140
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  DCSA $\equiv \mathcal{L}_{31}$, Eq.14a, Sauter et al, 1999

      implicit none

      real(kind(1.0D0)) :: dcsa

      !  Arguments

      integer, intent(in) :: j,nr

      !  Local variables

      real(kind(1.0D0)) :: zz,zft,zdf

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (j == 1) then
         dcsa = 0.0D0
      else
         zz = zef(j)
         zft = tpf(j)
         zdf = 1.0D0 + (1.0D0 - 0.1D0*zft)*sqrt(nues(j))
         zdf = zdf + 0.5D0*(1.0D0-zft)*nues(j)/zz
         zft = zft/zdf  !  $f^{31}_{teff}(\nu_{e*})$, Eq.14b
         dcsa = (1.0D0 + 1.4D0/(zz+1.0D0))*zft - 1.9D0/(zz+1.0D0)*zft*zft
         dcsa = dcsa + (0.3D0*zft*zft + 0.2D0*zft*zft*zft)*zft / (zz+1.0D0)

         !  Corrections suggested by Fable, 15/05/2015
         !dcsa = dcsa*beta_poloidal_local(j,nr) * (1.0D0+tempi(j)/(zz*tempe(j)))
         dcsa = dcsa*beta_poloidal_local_total(j,nr)
      end if

    end function dcsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function hcsa(j,nr)

      !+ad_name  hcsa
      !+ad_summ  Grad(ln(Te)) coefficient in the Sauter bootstrap scaling
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_args  nr : input integer : maximum value of j
      !+ad_desc  This function calculates the coefficient scaling grad(ln(Te))
      !+ad_desc  in the Sauter bootstrap current scaling.
      !+ad_desc  Code by Angioni, 29th May 2002.
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  None
      !+ad_call  beta_poloidal_local
      !+ad_call  beta_poloidal_local_total
      !+ad_call  dcsa
      !+ad_call  nues
      !+ad_call  tpf
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_hist  15/05/14 PJK Corrections made as per Fable's e-mail
      !+ad_stat  Okay
      !+ad_docs  O. Sauter, C. Angioni and Y. R. Lin-Liu,
      !+ad_docc    Physics of Plasmas <B>6</B> (1999) 2834
      !+ad_docs  O. Sauter, C. Angioni and Y. R. Lin-Liu, (ERRATA)
      !+ad_docc    Physics of Plasmas <B>9</B> (2002) 5140
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  HCSA $\equiv ?$, Sauter et al, 1999

      implicit none

      real(kind(1.0D0)) :: hcsa

      !  Arguments

      integer, intent(in) :: j,nr

      !  Local variables

      real(kind(1.0D0)) :: zz,zft,zdf,zfte,zfte2,zfte3,zfte4
      real(kind(1.0D0)) :: zfti,zfti2,zfti3,zfti4,hcee,hcei

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (j == 1) then
         hcsa = 0.0D0
      else
         zz = zef(j)
         zft = tpf(j)
         zdf = 1.0D0 + 0.26D0*(1.0D0-zft)*sqrt(nues(j))
         zdf = zdf + 0.18D0*(1.0D0-0.37D0*zft)*nues(j)/sqrt(zz)
         zfte = zft/zdf  !  $f^{32\_ee}_{teff}(\nu_{e*})$, Eq.15d
         zfte2 = zfte*zfte
         zfte3 = zfte*zfte2
         zfte4 = zfte2*zfte2

         zdf = 1.0D0 + (1.0D0 + 0.6D0*zft)*sqrt(nues(j))
         zdf = zdf + 0.85D0*(1.0D0 - 0.37D0*zft)*nues(j)*(1.0D0+zz)
         zfti = zft/zdf  !  $f^{32\_ei}_{teff}(\nu_{e*})$, Eq.15e
         zfti2 = zfti*zfti
         zfti3 = zfti*zfti2
         zfti4 = zfti2*zfti2

         hcee = (0.05D0 + 0.62D0*zz) / zz / (1.0D0 + 0.44D0*zz) * (zfte-zfte4)
         hcee = hcee + (zfte2 - zfte4 - 1.2D0*(zfte3-zfte4)) / (1.0D0 + 0.22D0*zz)
         hcee = hcee + 1.2D0/(1.0D0 + 0.5D0*zz)*zfte4  !  $F_{32\_ee}(X)$, Eq.15b

         hcei = -(0.56D0 + 1.93D0*zz) / zz / (1.0D0 + 0.44*zz) * (zfti-zfti4)
         hcei = hcei + 4.95D0/(1.0D0 + 2.48D0*zz) * &
              (zfti2 - zfti4 - 0.55D0*(zfti3-zfti4))
         hcei = hcei - 1.2D0/(1.0D0 + 0.5D0*zz)*zfti4  !  $F_{32\_ei}(Y)$, Eq.15c

         !  Corrections suggested by Fable, 15/05/2015
         !hcsa = beta_poloidal_local(j,nr)*(hcee + hcei) + dcsa(j,nr) &
         !     / (1.0D0 + tempi(j)/(zz*tempe(j)))
         hcsa = beta_poloidal_local(j,nr)*(hcee + hcei) + dcsa(j,nr) &
              * beta_poloidal_local(j,nr)/beta_poloidal_local_total(j,nr)
      end if

    end function hcsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function xcsa(j,nr)

      !+ad_name  xcsa
      !+ad_summ  Grad(ln(Ti)) coefficient in the Sauter bootstrap scaling
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_args  nr : input integer : maximum value of j
      !+ad_desc  This function calculates the coefficient scaling grad(ln(Ti))
      !+ad_desc  in the Sauter bootstrap current scaling.
      !+ad_desc  Code by Angioni, 29th May 2002.
      !+ad_desc  <P>The code was extracted from the ASTRA code, and was
      !+ad_desc  supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  None
      !+ad_call  beta_poloidal_local
      !+ad_call  dcsa
      !+ad_call  nues
      !+ad_call  nuis
      !+ad_call  tpf
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_hist  15/05/14 PJK Corrections made as per Fable's e-mail
      !+ad_stat  Okay
      !+ad_docs  O. Sauter, C. Angioni and Y. R. Lin-Liu,
      !+ad_docc    Physics of Plasmas <B>6</B> (1999) 2834
      !+ad_docs  O. Sauter, C. Angioni and Y. R. Lin-Liu, (ERRATA)
      !+ad_docc    Physics of Plasmas <B>9</B> (2002) 5140
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: xcsa

      !  Arguments

      integer, intent(in) :: j,nr

      !  Local variables

      real(kind(1.0D0)) :: zz,zft,zdf,a0,alp,a1,zfte

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if (j == 1) then
         xcsa = 0.0D0
      else
         zz = zef(j)
         zft = tpf(j)
         zdf = 1.0D0 + (1.0D0 - 0.1D0*zft)*sqrt(nues(j))
         zdf = zdf + 0.5D0*(1.0D0 - 0.5D0*zft)*nues(j)/zz
         zfte = zft/zdf  !  $f^{34}_{teff}(\nu_{e*})$, Eq.16b

         xcsa = (1.0D0 + 1.4D0/(zz+1.0D0))*zfte - 1.9D0/(zz+1.0D0)*zfte*zfte
         xcsa = xcsa + (0.3D0*zfte*zfte + 0.2D0*zfte*zfte*zfte)*zfte &
              / (zz+1.0D0)  !  Eq.16a

         a0 = -1.17D0*(1.0D0-zft)
         a0 = a0 / (1.0D0 - 0.22D0*zft - 0.19D0*zft*zft)  !  $\alpha_0$, Eq.17a

         alp = (a0 + 0.25D0*(1.0D0 - zft*zft)*sqrt(nuis(j))) / &
              (1.0D0 + 0.5*sqrt(nuis(j)))
         a1 = nuis(j)*nuis(j) * zft**6
         alp = (alp + 0.315D0*a1) / (1.0D0 + 0.15D0*a1)  !  $\alpha(\nu_{i*})$, Eq.17b

         !  Corrections suggested by Fable, 15/05/2015
         !xcsa = beta_poloidal_local(j,nr) * (xcsa*alp)*tempi(j)/zz/tempe(j)
         !xcsa = xcsa + dcsa(j,nr) / (1.0D0 + zz*tempe(j)/tempi(j))

         xcsa = (beta_poloidal_local_total(j,nr)-beta_poloidal_local(j,nr)) &
              * (xcsa*alp)
         xcsa = xcsa + dcsa(j,nr) * &
              (1.0D0 - beta_poloidal_local(j,nr)/beta_poloidal_local_total(j,nr))
      end if

    end function xcsa

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function tpf(j)

      !+ad_name  tpf
      !+ad_summ  Trapped particle fraction
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  j  : input integer : radial element index in range 1 to nr
      !+ad_desc  This function calculates the trapped particle fraction at
      !+ad_desc  a given radius.
      !+ad_desc  <P>A number of different fits are provided, but the one
      !+ad_desc  to be used is hardwired prior to run-time.
      !+ad_desc  <P>The ASTRA fit was supplied by Emiliano Fable, IPP Garching
      !+ad_desc  (private communication).
      !+ad_prob  The ASTRA and Sauter 2002 fits are almost identical, and it
      !+ad_prob  is unclear which (if either) is better.
      !+ad_call  None
      !+ad_hist  26/03/14 PJK Initial version
      !+ad_stat  Okay
      !+ad_docs  O. Sauter et al, Plasma Phys. Contr. Fusion <B>44</B> (2002) 1999
      !+ad_docs  O. Sauter, 2013:
      !+ad_docc    http://infoscience.epfl.ch/record/187521/files/lrp_012013.pdf
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: tpf

      !  Arguments

      integer, intent(in) :: j

      !  Local variables

      integer, parameter :: ASTRA=1, SAUTER2002=2, SAUTER2013=3

      real(kind(1.0D0)) :: eps,epseff,g,s,zz

      integer :: fit = ASTRA

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      s = sqeps(j)
      eps = s*s

      select case (fit)

      case (ASTRA)

         !  ASTRA method, from Emiliano Fable, private communication
         !  (Excluding h term which dominates for inverse aspect ratios < 0.5,
         !  and tends to take the trapped particle fraction to 1)

         zz = 1.0D0 - eps

         g = 1.0D0 - zz*sqrt(zz) / (1.0D0 + 1.46D0*s)

         !  Advised by Emiliano to ignore ASTRA's h below
         !
         !h = 0.209D0 * (sqrt(tempi(j)*amain(j))/zmain(j)*mu(j)*rmajor*bt)**0.3333D0
         !tpf = min(1.0D0, max(g, h))

         tpf = g

      case (SAUTER2002)

         !  Equation 4 of Sauter 2002
         !  Similar to, but not quite identical to g above

         tpf = 1.0D0 - (1.0D0-eps)**2 / (1.0D0 + 1.46D0*s) / sqrt(1.0D0 - eps*eps)

      case (SAUTER2013)

         !  Includes correction for triangularity

         epseff = 0.67D0*(1.0D0 - 1.4D0*triang*abs(triang)) * eps

         tpf = 1.0D0 - sqrt( (1.0D0-eps)/(1.0D0+eps) ) * &
              (1.0D0 - epseff) / (1.0D0 + 2.0D0*sqrt(epseff))

      end select

    end function tpf

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine fast_alpha_bs()

      !  BSALP (local per index J) is in MA/m^2

      !  Required... before we can use this routine:
      !  fast alpha pressure profile
      !  poloidal flux profile vs local minor radius  (and grad(psi))
      !  Shafranov shift vs local minor radius

      !  all lengths in meters,
      !  temperatures in keV where j is the radial index,
      !  IPOL is R*Bphi / (R0*Bphi0)  (i.e. the normalized poloidal current integral)
      !  PFAST is the alpha pressure
      !  TE is the electron temperature
      !  SHIF is the Shafranov shift (defined with respect to the geom. major radius)
      !  AMETR is the minor radius
      !  RTOR = R0
      !  ZEF = Z effective
      !  FP = PSI (magnetic flux, poloidal) defined such that
      !    B_pol = grad(PSI) / (2*PI*R)

      ! ZBIRTH = 1.

      ! !MeV already included in PFAST ,convert PFAST from keV*1e19 to J
      ! ZDPDPSI = 3./2.*1.60218*1.e3* &
      !      (PFAST(J)-PFAST(J-1))/((FP(J)-FP(J-1))/GP2)
      ! ZSB=(0.027*ZEF(J-1)*(TE(J-1)/20.)**(3./2.))**(1./3.)
      ! ZSB1=(0.027*ZEF(J)*(TE(J)/20.)**(3./2.))**(1./3.)
      ! ZSC=(5./3.)**(1./3.)*ZSB
      ! ZSC1=(5./3.)**(1./3.)*ZSB1
      ! ZDSC3DPSI = 3./2.*1.60218*1.e3*PFAST(J)* &
      !      (ZSC1**3.-ZSC**3.)/((FP(J)-FP(J-1))/GP2)
      ! ZEPS=AMETR(J)/RTOR
      ! ZFP=1.-1.46*(1.+0.67/ZEF(J))*ZEPS**0.5+ 0.46*(1.+2.1/ZEF(J))*ZEPS

      ! ZDR0DR=(SHIF(J)-SHIF(J-1))/(AMETR(J)-AMETR(J-1))

      ! ZY=(1.-ZDR0DR/ZEPS*(1.-(1.-ZEPS**2.)**0.5)) &
      !      /(1.+ZEPS*ZDR0DR/2.)/(1.-ZEPS**2.)**.5

      ! ZA11=-ZSB**(3./2.)*(0.12+2.24*ZSB**(3./2.)-0.9*ZSB**3.) &
      !      /(0.7+18.4*ZSB**(3./2.)+ &
      !      23.5*ZSB**3.+101.*ZSB**(9./2.))*ZY

      ! ZA12=-2./3.*(0.5+0.8*ZSC**(3./2.)+0.4*ZSC**3.) &
      !      /(1.+2.3*ZSC**(3./2.)+4.*ZSC**3.)

      ! ZA21=(7.e-4+0.02*ZSB**(3./2.)+0.4*ZSB**3.)/ &
      !      (0.01-0.61*ZSB**(3./2.)+ &
      !      24.8*ZSB**3.-53.4*ZSB**(9./2.)+118.*ZSB**6.)*ZY

      ! ZA22=2./3.*(0.1+3.25*ZSC**(3./2.)-1.1*ZSC**3.)/ &
      !      (1.e-3+0.6*ZSC**(3./2.)+ &
      !      8.6*ZSC**3.+3.1*ZSC**(9./2.)+15.1*ZSC**6.)

      ! ZB1=(0.155+3.9*ZSB**(3./2.)-3.1*ZSB**3.+0.3*ZSB**6.) &
      !      /(0.1+3.*ZSB**(3./2.)-2.1*ZSB**3.)*ZY

      ! ZB2=(1.3-0.5*ZSB**(3./2.)+5.9*ZSB**3.)/ &
      !      (1.-0.34*ZSB**(3./2.)+4.9*ZSB**3.)*ZY

      ! ZA1 = -ZA11+(2.*ZA11+(2.*ZB1-3.)*ZA12)*ZEPS**.5 &
      !      -(ZA11+2.*(ZB1-1.)*ZA12)*ZEPS

      ! ZA2 = -ZA21+(2.*ZA21+(2.*ZB2-3.)*ZA22)*ZEPS**.5 &
      !      -(ZA21+2.*(ZB2-1.)*ZA22)*ZEPS

      ! !bootstrap current by alphas
      ! BSALP=-ZEPS**.5*(1.-2./ZEF(J)*ZFP)*IPOL(J)* &
      !      RTOR*ZBIRTH*(ZA1*ZDPDPSI+ZA2*ZDSC3DPSI)
      ! BSALP=BSALP/1.e6

    end subroutine fast_alpha_bs

  end function bootstrap_fraction_sauter

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine culcur(alphaj,alphap,bt,eps,icurr,iprofile,kappa,kappa95, &
       p0,pperim,q0,qpsi,rli,rmajor,rminor,sf,triang,triang95,bp,qstar,plascur)

    !+ad_name  culcur
    !+ad_summ  Routine to calculate the plasma current
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  conhas
    !+ad_cont  plasc
    !+ad_args  alphaj   : input/output real : current profile index
    !+ad_args  alphap   : input real :  pressure profile index
    !+ad_args  bt       : input real :  toroidal field on axis (T)
    !+ad_args  eps      : input real :  inverse aspect ratio
    !+ad_args  icurr    : input integer : current scaling model to use
    !+ad_argc                           1 = Peng analytic fit
    !+ad_argc                           2 = Peng divertor scaling (TART)
    !+ad_argc                           3 = simple ITER scaling
    !+ad_argc                           4 = revised ITER scaling
    !+ad_argc                           5 = Todd empirical scaling I
    !+ad_argc                           6 = Todd empirical scaling II
    !+ad_argc                           7 = Connor-Hastie model
    !+ad_args  iprofile : input integer : switch for current profile consistency
    !+ad_argc                           0 = use input alphaj, rli
    !+ad_argc                           1 = make these consistent with q, q0
    !+ad_args  kappa    : input real :  plasma elongation
    !+ad_args  kappa95  : input real :  plasma elongation at 95% surface
    !+ad_args  p0       : input real :  central plasma pressure (Pa)
    !+ad_args  pperim   : input real :  plasma perimeter length (m)
    !+ad_args  q0       : input real :  plasma safety factor on axis
    !+ad_args  qpsi     : input real :  plasma edge safety factor (= q-bar for icurr=2)
    !+ad_args  rli      : input/output real : plasma normalised internal inductance
    !+ad_args  rmajor   : input real :  major radius (m)
    !+ad_args  rminor   : input real :  minor radius (m)
    !+ad_args  sf       : input real :  shape factor for icurr=1 (=A/pi in documentation)
    !+ad_args  triang   : input real :  plasma triangularity
    !+ad_args  triang95 : input real :  plasma triangularity at 95% surface
    !+ad_args  bp       : output real : poloidal field (T)
    !+ad_args  qstar    : output real : equivalent cylindrical safety factor (shaped)
    !+ad_args  plascur  : output real : plasma current (A)
    !+ad_desc  This routine performs the calculation of the
    !+ad_desc  plasma current, with a choice of formula for the edge
    !+ad_desc  safety factor. It will also make the current profile parameters
    !+ad_desc  consistent with the q-profile if required.
    !+ad_prob  None
    !+ad_call  bpol
    !+ad_call  conhas
    !+ad_call  plasc
    !+ad_call  report_error
    !+ad_hist  20/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  29/01/96 PJK Added icurr=2 TART option
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  22/11/12 PJK Added stop statement in error block
    !+ad_hist  27/11/13 PJK Added new arguments to bpol
    !+ad_hist  28/11/13 PJK Added current profile consistency if iprofile=1
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  02/06/16 RK  Added Sauter scaling for negative triangularity
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  J D Galambos, STAR Code : Spherical Tokamak Analysis and Reactor Code,
    !+ad_docc  unpublished internal Oak Ridge document
    !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
    !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
    !+ad_docs  T. Hartmann and H. Zohm: Towards a 'Physics Design Guidelines for a
    !+ad_docc  DEMO Tokamak' Document, March 2012, EFDA Report
    !+ad_docc  Sauter, Geometric formulas for systems codes..., FED 2016
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: icurr, iprofile
    real(kind(1.0D0)), intent(inout) :: alphaj, rli
    real(kind(1.0D0)), intent(in) :: alphap, bt, eps, kappa, &
         kappa95, p0, pperim, q0, qpsi, rmajor, rminor, sf, triang, triang95
    real(kind(1.0D0)), intent(out) :: bp, qstar, plascur

    !  Local variables

    real(kind(1.0D0)) :: asp, curhat, fq, w07

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Aspect ratio

    asp = 1.0D0/eps

    !  Calculate the function Fq that scales the edge q from the
    !  circular cross-section cylindrical case

    !  First check for negative triangularity using unsuitable current scaling

    if ((icurr.ne.8).and.(triang.lt.0.0)) then
     write(*,*) 'Triangularity is negative without icurr = 8.'
     write(*,*) 'Please check and try again.'
     write(*,*) 'PROCESS stopping'
     stop
    end if

    select case (icurr)

    case (1)  !  Peng analytical fit
       fq = (1.22D0-0.68D0*eps)/((1.0D0-eps*eps)**2) * sf**2

    case (2)  !  Peng scaling for double null divertor; TARTs [STAR Code]
       curhat = 1.0D6 * plasc(qpsi,asp,rminor,bt,kappa,triang)/bt

    case (3)  !  Simple ITER scaling (simply the cylindrical case)
       fq = 1.0D0

    case (4)  !  ITER formula (IPDG89)
       fq = 0.5D0 * (1.17D0-0.65D0*eps)/((1.0D0-eps*eps)**2) * &
            (1.0D0 + kappa95**2 * &
            (1.0D0 + 2.0D0*triang95**2 - 1.2D0*triang95**3) )

    case (5, 6) !  Todd empirical scalings

       fq = (1.0D0+2.0D0*eps*eps) * 0.5D0*(1.0D0+kappa95**2) * &
            (1.24D0-0.54D0*kappa95+0.3D0*(kappa95**2 + triang95**2) + &
            0.125D0*triang95)

       if (icurr == 6) fq = fq * (1.0D0 + ( abs(kappa95-1.2D0) )**3)

    case (7)  !  Connor-Hastie asymptotically-correct expression

       !  N.B. If iprofile=1, alphaj will be wrong during the first call (only)

       call conhas(alphaj,alphap,bt,triang,eps,kappa,p0,fq)

    case (8)  !  Sauter scaling allowing negative triangularity [FED May 2016]

        ! Assumes zero squareness, note takes kappa, delta at separatrix not _95

        w07 = 1.0d0    ! zero squareness - can be modified later if required

        fq = (1.0d0 + 1.2d0*(kappa - 1.0d0) + 0.56d0*(kappa-1.0d0)**2) * &
             (1.0d0 + 0.09d0 * triang + 0.16d0 * triang**2) * &
       (1.0d0 + 0.45d0 * triang * eps)/(1.0d0 - 0.74d0 * eps) * &
       (1.0d0 + 0.55d0 * (w07 - 1.0d0))

    case default
       idiags(1) = icurr ; call report_error(77)

    end select

    !  Calculate the ratio of plasma current to toroidal field

    if (icurr /= 2) then
       curhat = 5.0D6 * rminor**2 / (rmajor*qpsi) * fq
    end if
    if (icurr == 8) then
       curhat = 4.1d6 * rminor**2 / (rmajor*qpsi) * fq
    end if

    !  Calculate the equivalent edge safety factor (= qcyl)

    qstar = 5.0D6 * rminor**2 / (rmajor*curhat) * 0.5D0 * &
         (1.0D0 + kappa95**2 * &
         (1.0D0 + 2.0D0*triang95**2 - 1.2D0*triang95**3) )

    !  Calculate plasma current

    plascur = curhat * bt

    normalised_total_beta = 1.0D8*beta*rminor*bt/plascur

    !  Calculate the poloidal field

    bp = bpol(itart,plascur,qpsi,asp,bt,kappa,triang,pperim)

    !  Ensure current profile consistency, if required
    !  This is as described in Hartmann and Zohm only if icurr = 4 as well...

    if (iprofile == 1) then
       alphaj = qstar/q0 - 1.0D0
       rli = log(1.65D0 + 0.89D0*alphaj)  !  Tokamaks 4th Edition, Wesson, page 116
    end if

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function plasc(qbar,aspect,rminor,bt,kappa,delta)

      !+ad_name  plasc
      !+ad_summ  Function to calculate plasma current (Peng scaling)
      !+ad_type  Function returning real
      !+ad_auth  J Galambos, FEDC/ORNL
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  aspect : input real :  plasma aspect ratio
      !+ad_args  bt     : input real :  toroidal field on axis (T)
      !+ad_args  delta  : input real :  plasma triangularity
      !+ad_args  kappa  : input real :  plasma elongation
      !+ad_args  qbar   : input real :  edge q-bar
      !+ad_args  rminor : input real :  plasma minor radius (m)
      !+ad_desc  This function calculates the plasma current in MA,
      !+ad_desc  using a scaling from M Peng's notes, 24 February 1989.
      !+ad_desc  It is primarily used for Tight Aspect Ratio Tokamaks and is
      !+ad_desc  selected via <CODE>icurr=2</CODE>.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
      !+ad_hist  10/11/11 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  J D Galambos, STAR Code : Spherical Tokamak Analysis and Reactor Code,
      !+ad_docc  unpublished internal Oak Ridge document
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: plasc

      !  Arguments

      real(kind(1.0D0)), intent(in) :: aspect,bt,delta,kappa,qbar,rminor

      !  Local variables

      real(kind(1.0D0)) :: c1,c2,d1,d2,eps,e1,e2,f1,f2,ff1,ff2,g,h1,h2,y1,y2

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      eps = 1.0D0/aspect

      c1 = kappa**2/(1.0D0+delta) + delta
      c2 = kappa**2/(1.0D0-delta) - delta

      d1 = (kappa/(1.0D0+delta))**2 + 1.0D0
      d2 = (kappa/(1.0D0-delta))**2 + 1.0D0

      if (aspect < c1) then
         y1 = sqrt( (c1*eps - 1.0D0)/(1.0D0+eps) ) * (1.0D0 + delta)/kappa
      else
         y1 = sqrt( (1.0D0 - c1*eps)/(1.0D0+eps) ) * (1.0D0 + delta)/kappa
      end if
      y2 = sqrt( (c2*eps+1.0D0)/(1.0D0-eps) ) * (1.0D0-delta)/kappa

      e1 = 2.0D0*kappa/(d1*(1.0D0+delta))
      e2 = 2.0D0*kappa/(d2*(1.0D0-delta))

      h2 = (1.0D0 + (c2-1.0D0)*eps/2.0D0) / &
           sqrt( (1.0D0-eps)*(c2*eps+1.0D0) )
      f2 = (d2*(1.0D0-delta)*eps) / ( (1.0D0-eps)*(c2*eps+1.0D0) )
      g = eps*kappa / (1.0D0 - eps*delta)
      ff2 = f2 * (g + 2.0D0*h2*atan(y2) )

      if (aspect < c1) then
         h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / &
              sqrt( (1.0D0+eps)*(c1*eps-1.0D0) )
         f1 = (d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
         ff1 = f1*(g - h1*log( (1.0D0+y1)/(1.0D0-y1) ) )
      else
         h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / &
              sqrt( (1.0D0+eps)*(1.0D0-c1*eps) )
         f1 = -(d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
         ff1 = f1*( -g + 2.0D0*h1*atan(y1) )
      end if

      plasc = rminor*bt/qbar * 5.0D0*kappa/(2.0D0*pi**2) * &
           ( asin(e1)/e1 + asin(e2)/e2 ) * (ff1 + ff2)

    end function plasc

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine conhas(alphaj,alphap,bt,delta,eps,kappa,p0,fq)

      !+ad_name  conhas
      !+ad_summ  Routine to calculate the F coefficient used for scaling the
      !+ad_summ  plasma current
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  alphaj   : input real :  current profile index
      !+ad_args  alphap   : input real :  pressure profile index
      !+ad_args  bt       : input real :  toroidal field on axis (T)
      !+ad_args  delta    : input real :  plasma triangularity
      !+ad_args  eps      : input real :  inverse aspect ratio
      !+ad_args  kappa    : input real :  plasma elongation
      !+ad_args  p0       : input real :  central plasma pressure (Pa)
      !+ad_args  fq       : output real : scaling for edge q from circular
      !+ad_argc                           cross-section cylindrical case
      !+ad_desc  This routine calculates the F coefficient used for scaling the
      !+ad_desc  plasma current, using the Connor-Hastie scaling given in
      !+ad_desc  AEA FUS 172.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
      !+ad_hist  09/11/11 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: alphaj,alphap,bt,delta,eps,kappa,p0
      real(kind(1.0D0)), intent(out) :: fq

      !  Local variables

      real(kind(1.0D0)) :: beta0, deltap, deltar, eprime, er, kap1, &
           lambda, lamp1, li, nu, tprime, tr

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Exponent in Connor-Hastie current profile - matching total
      !  current gives the following trivial relation

      lambda = alphaj

      !  Exponent in Connor-Hastie pressure profile

      nu = alphap

      !  Central plasma beta

      beta0 = 2.0D0 * rmu0 * p0 / (bt**2)

      !  Plasma internal inductance

      lamp1 = 1.0D0 + lambda
      li = lamp1/lambda * (lamp1/lambda * log(lamp1) - 1.0D0)

      !  T/r in AEA FUS 172

      kap1 = kappa + 1.0D0
      tr = kappa * delta / kap1**2

      !  E/r in AEA FUS 172

      er = (kappa-1.0D0)/kap1

      !  T primed in AEA FUS 172

      tprime = 2.0D0 * tr * lamp1/(1.0D0 + 0.5D0*lambda)

      !  E primed in AEA FUS 172

      eprime = er * lamp1/(1.0D0 + lambda/3.0D0)

      !  Delta primed in AEA FUS 172

      deltap = 0.5D0*kap1 * eps * 0.5D0*li + &
           beta0/(0.5D0*kap1*eps) * lamp1**2 / (1.0D0+nu)

      !  Delta/R0 in AEA FUS 172

      deltar = beta0/6.0D0 * (1.0D0 + 5.0D0*lambda/6.0D0 + 0.25D0*lambda**2) &
           + (0.5D0*kap1*eps)**2 * 0.125D0*(1.0D0-(lambda**2)/3.0D0)

      !  F coefficient

      fq = (0.5D0*kap1)**2 * &
           ( 1.0D0 + eps**2 * (0.5D0*kap1)**2 + 0.5D0*deltap**2 + &
           2.0D0*deltar + 0.5D0*(eprime**2 + er**2) + &
           0.5D0*(tprime**2 + 4.0D0*tr**2) )

    end subroutine conhas

  end subroutine culcur

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function bpol(itart,ip,qbar,aspect,bt,kappa,delta,perim)

    !+ad_name  bpol
    !+ad_summ  Function to calculate poloidal field
    !+ad_type  Function returning real
    !+ad_auth  J Galambos, FEDC/ORNL
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  itart  : input integer : Switch for tight aspect ratio tokamaks
    !+ad_args  ip     : input real :  plasma current (A)
    !+ad_args  qbar   : input real :  edge q-bar
    !+ad_args  aspect : input real :  plasma aspect ratio
    !+ad_args  bt     : input real :  toroidal field on axis (T)
    !+ad_args  kappa  : input real :  plasma elongation
    !+ad_args  delta  : input real :  plasma triangularity
    !+ad_args  perim  : input real :  plasma perimeter (m)
    !+ad_desc  This function calculates the poloidal field in Tesla,
    !+ad_desc  using a simple calculation using Stoke's Law for conventional
    !+ad_desc  tokamaks, or for TARTs, a scaling from M Peng's notes,
    !+ad_desc  24 February 1989.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  22/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_hist  27/11/13 PJK Added conventional aspect ratio coding
    !+ad_stat  Okay
    !+ad_docs  J D Galambos, STAR Code : Spherical Tokamak Analysis and Reactor Code,
    !+ad_docc  unpublished internal Oak Ridge document
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: bpol

    !  Arguments

    integer, intent(in) :: itart
    real(kind(1.0D0)), intent(in) :: aspect,bt,delta,ip,kappa,perim,qbar

    !  Local variables

    real(kind(1.0D0)) :: c1,c2,d1,d2,eps,f1,f2,ff1,ff2,g,h1,h2,y1,y2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (itart == 0) then

       !  Stoke's Law

       bpol = rmu0 * ip / perim

    else  !  Original coding, only suitable for TARTs [STAR Code]

       eps = 1.0D0/aspect

       c1 = kappa**2/(1.0D0+delta) + delta
       c2 = kappa**2/(1.0D0-delta) - delta

       d1 = (kappa/(1.0D0+delta))**2 + 1.0D0
       d2 = (kappa/(1.0D0-delta))**2 + 1.0D0

       if (aspect < c1) then
          y1 = sqrt( (c1*eps - 1.0D0)/(1.0D0+eps) ) * (1.0D0 + delta)/kappa
       else
          y1 = sqrt( (1.0D0 - c1*eps)/(1.0D0+eps) ) * (1.0D0 + delta)/kappa
       end if
       y2 = sqrt( (c2*eps+1.0D0)/(1.0D0-eps) ) * (1.0D0-delta)/kappa

       h2 = (1.0D0 + (c2-1.0D0)*eps/2.0D0) / &
            sqrt( (1.0D0-eps)*(c2*eps+1.0D0) )
       f2 = (d2*(1.0D0-delta)*eps) / ( (1.0D0-eps)*(c2*eps+1.0D0) )
       g = eps*kappa / (1.0D0 - eps*delta)
       ff2 = f2 * (g + 2.0D0*h2*atan(y2) )

       if (aspect < c1) then
          h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / &
               sqrt( (1.0D0+eps)*(c1*eps-1.0D0) )
          f1 = (d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
          ff1 = f1*(g - h1*log( (1.0D0+y1)/(1.0D0-y1) ) )
       else
          h1 = (1.0D0 + (1.0D0-c1)*eps/2.0D0) / &
               sqrt( (1.0D0+eps)*(1.0D0-c1*eps) )
          f1 = -(d1*(1.0D0+delta)*eps) / ( (1.0D0+eps)*(c1*eps-1.0D0) )
          ff1 = f1*( -g + 2.0D0*h1*atan(y1) )
       end if

       bpol = bt * (ff1 + ff2) / (2.0D0 * pi * qbar)

    end if

  end function bpol

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine culblm(bt,dnbeta,plascur,rminor,betalim)

    !+ad_name  culblm
    !+ad_summ  Beta scaling limit
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  bt      : input real :  toroidal B-field on plasma axis (T)
    !+ad_args  dnbeta  : input real :  Troyon-like g coefficient
    !+ad_args  plascur : input real :  plasma current (A)
    !+ad_args  rminor  : input real :  plasma minor axis (m)
    !+ad_args  betalim : output real : beta limit as defined below
    !+ad_desc  This subroutine calculates the beta limit, using
    !+ad_desc  the algorithm documented in AEA FUS 172.
    !+ad_desc  <P>The limit applies to beta defined with respect to the total B-field.
    !+ad_desc  Switch ICULBL determines which components of beta to include (see
    !+ad_desc  routine <A HREF="constraints.html">constraints</A> for coding):
    !+ad_desc  <UL>
    !+ad_desc  <P><LI>If ICULBL = 0, then the limit is applied to the total beta
    !+ad_desc  <P><LI>If ICULBL = 1, then the limit is applied to the thermal beta only
    !+ad_desc  <P><LI>If ICULBL = 2, then the limit is applied to the thermal +
    !+ad_desc                        neutral beam beta components
    !+ad_desc  </UL>
    !+ad_desc  The default value for the g coefficient is DNBETA = 3.5
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  27/06/13 PJK Modified header comments
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: bt, dnbeta, plascur, rminor
    real(kind(1.0D0)), intent(out) :: betalim

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    betalim = 0.01D0 * dnbeta * (plascur/1.0D6) / (rminor*bt)

  end subroutine culblm

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine betcom(cfe0,dene,fdeut,ftrit,fhe3,ftritbm,ignite,impc, &
       impo,ralpne,rnbeam,te,zeff,abeam,afuel,aion,deni,dlamee, &
       dlamie,dnalp,dnbeam,dnitot,dnprot,dnz,falpe,falpi,rncne,rnone, &
       rnfene,zeffai,zion,zfear)

    !+ad_name  betcom
    !+ad_summ  Calculates various plasma component fractional makeups
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  cfe0   : input real :  additional iron impurity fraction
    !+ad_args  dene   : input real :  electron density (/m3)
    !+ad_args  fdeut  : input real :  deuterium fraction of fuel
    !+ad_args  ftrit  : input real :  tritium fraction of fuel
    !+ad_args  fhe3   : input real :  helium-3 fraction of fuel
    !+ad_args  ftritbm: input real :  tritium fraction of beam
    !+ad_args  ignite : input integer :  switch for ignited calculation
    !+ad_args  impc   : input real :  carbon impurity multiplier
    !+ad_args  impo   : input real :  oxygen impurity multiplier
    !+ad_args  ralpne : input real :  thermal alpha density / electron density
    !+ad_args  rnbeam : input real :  hot beam density / electron density
    !+ad_args  te     : input real :  electron temperature (keV)
    !+ad_args  zfear  : input integer :  high-Z impurity switch; 0=iron, 1=argon
    !+ad_args  abeam  : output real : beam ion mass (amu)
    !+ad_args  afuel  : output real : average mass of fuel portion of ions (amu)
    !+ad_args  aion   : output real : average mass of all ions (amu)
    !+ad_args  deni   : output real : fuel ion density (/m3)
    !+ad_args  dlamee : output real : electron-electron coulomb logarithm
    !+ad_args  dlamie : output real : ion-electron coulomb logarithm
    !+ad_args  dnalp  : output real : alpha ash density (/m3)
    !+ad_args  dnbeam : output real : hot beam ion density (/m3)
    !+ad_args  dnitot : output real : total ion density (/m3)
    !+ad_args  dnprot : output real : proton ash density (/m3)
    !+ad_args  dnz    : output real : high Z ion density (/m3)
    !+ad_args  falpe  : output real : fraction of alpha energy to electrons
    !+ad_args  falpi  : output real : fraction of alpha energy to ions
    !+ad_args  rncne  : output real : carbon density / electron density
    !+ad_args  rnfene : output real : iron density / electron density
    !+ad_args  rnone  : output real : oxygen density / electron density
    !+ad_args  zeff   : output real : plasma effective charge
    !+ad_args  zeffai : output real : mass weighted plasma effective charge
    !+ad_args  zion   : output real : density weighted charge
    !+ad_desc  This subroutine determines the various plasma component
    !+ad_desc  fractional makeups.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  06/12/95 PJK Added D-He3 calculations
    !+ad_hist  01/04/98 PJK Added calculation of line-averaged density
    !+ad_hisc               and effects of IGNITE
    !+ad_hist  24/04/98 PJK Added IMPC, IMPFE, IMPO impurity multipliers
    !+ad_hist  23/05/06 PJK Ensured that deni is positive
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  17/12/12 PJK Added ZFEAR coding, and updated AION and other
    !+ad_hisc               high-Z impurity terms
    !+ad_hist  03/07/13 PJK Amended ZEFFAI coding as per long-standing comment
    !+ad_hist  24/07/13 PJK Clarified DNLA comment
    !+ad_hist  10/09/13 PJK Clarified DENI calculation for D-He3;
    !+ad_hist               modified dnprot calculation
    !+ad_hist  11/09/13 PJK Removed idhe3, ftr usage
    !+ad_hist  12/02/14 PJK Modified initial dnprot approximation
    !+ad_hist  19/02/14 PJK Moved PCOEF and DNLA calculations elsewhere
    !+ad_hist  28/07/14 PJK Added fix for problems due to carbon impurity
    !+ad_hisc               scaling at low electron density
    !+ad_hist  19/08/14 PJK Removed IMPFE argument
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  F/MI/PJK/LOGBOOK11, p.38 for D-He3 deni calculation
    !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
    !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: ignite, zfear
    real(kind(1.0D0)), intent(in) :: cfe0, dene, fdeut, ftrit, fhe3, &
         ftritbm, impc, impo, ralpne, rnbeam, te
    real(kind(1.0D0)), intent(out) :: abeam, afuel, aion, deni, dlamee, &
         dlamie, dnalp, dnbeam, dnitot, dnprot, dnz, falpe, falpi, &
         rncne, rnfene, rnone, zeff, zeffai, zion

    !  Local variables

    real(kind(1.0D0)) :: fc, f_highz, fo, m_highz, pc, znfuel, z_highz
    integer :: first_call = 1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Ion density components
    !  ======================

    !  Alpha ash portion

    dnalp = dene * ralpne

    !  Protons
    !  This calculation will be wrong on the first call as the particle
    !  production rates are evaluated later in the calling sequence
    !  Issue #557 Allow protium impurity to be specified: 'protium'
    !  This will override the calculated value which is a minimum.

    if (alpharate < 1.0D-6) then  !  not calculated yet...
       dnprot = max(protium*dene, dnalp * (fhe3 + 1.0D-3)) !  rough estimate
    else
       dnprot = max(protium*dene, dnalp * protonrate/alpharate)
    end if


    !  Beam hot ion component
    !  If ignited, prevent beam fusion effects

    if (ignite == 0) then
       dnbeam = dene * rnbeam
    else
       dnbeam = 0.0D0
    end if

    !  Carbon portion (IPDG89)

    fc = impc * (0.009D0 + 0.006D0 * (7.0D19/dene)**2.6D0)

    !  The following should prevent problems at low electron density
    !  dene with ion density deni becoming negative

    if (fc > 0.05D0) then
       fc = 0.05D0
       call report_error(136)
    end if

    rncne = fc

    !  Oxygen portion (IPDG89)

    fo = impo * 0.001D0
    rnone = fo

    !  High-Z portion (formerly assumed to be iron)

    f_highz = cfe0
    rnfene = f_highz

    if (zfear == 1) then  !  High-Z impurity is argon
       z_highz = 18.0D0
       m_highz = 40.0D0
    else  !  Iron
       z_highz = 26.0D0
       m_highz = 56.0D0
    end if

    !  Fuel portion - conserve charge neutrality
    !  znfuel is the sum of Zi.ni for the three fuel ions

    znfuel = dene - 2.0D0*dnalp - dnprot - dnbeam - &
         dene*(6.0D0*fc + 8.0D0*fo + z_highz*f_highz)

    !  Fuel ion density, deni
    !  For D-T-He3 mix, deni = nD + nT + nHe3, while znfuel = nD + nT + 2*nHe3
    !  So deni = znfuel - nHe3 = znfuel - fhe3*deni

    deni = znfuel/(1.0D0+fhe3)

    !  Ensure that deni is never negative or zero

    deni = max(deni,1.0D0)

    !  Total ion density

    dnz = dene * (fc + fo + f_highz)
    dnitot = deni + dnz + dnalp + dnprot + dnbeam

    !  Effective charge
    !  True calculation should be sum(ni.Zi^2) / sum(ni.Zi),
    !  but ne = sum(ni.Zi) through quasineutrality

    zeff = (fdeut + ftrit)*deni/dene + 4.0D0*fhe3*deni/dene + &
         dnbeam/dene + 4.0D0*ralpne + dnprot/dene + 36.0D0*fc + &
         64.0D0*fo + z_highz*z_highz*f_highz

    !  Define coulomb logarithm
    !  (collisions: ion-electron, electron-electron)

    dlamee = 31.0D0 - (log(dene)/2.0D0) + log(te*1000.0D0)
    dlamie = 31.3D0 - (log(dene)/2.0D0) + log(te*1000.0D0)

    !  Fraction of alpha energy to ions and electrons
    !  From Max Fenstermacher
    !  (used with electron and ion power balance equations only)
    !  No consideration of pchargepv here...

    !  pcoef now calculated in plasma_profiles, after the very first
    !  call of betcom; use old parabolic profile estimate in this case

    if (first_call == 1) then
       pc = (1.0D0 + alphan)*(1.0D0 + alphat)/(1.0D0+alphan+alphat)
       first_call = 0
    else
       pc = pcoef
    end if

    falpe = 0.88155D0 * exp(-te*pc/67.4036D0)
    falpi = 1.0D0 - falpe

    !  Average atomic masses

    afuel = 2.0D0*fdeut + 3.0D0*ftrit + 3.0D0*fhe3
    abeam = 2.0D0*(1.0D0-ftritbm) + 3.0D0*ftritbm

    !  Density weighted masses and charges

    aion = ( afuel*deni + 4.0D0*dnalp + dnprot + abeam*dnbeam + &
         dene*(12.0D0*fc + 16.0D0*fo + m_highz*f_highz) )/ dnitot

    zion = ( fdeut*deni + ftrit*deni + 2.0D0*fhe3*deni + &
         2.0D0*dnalp + dnprot + dnbeam + dene * &
         (6.0D0*fc + 8.0D0*fo + z_highz*f_highz) )/dnitot

    zeffai = ( &
         fdeut*deni/2.0D0 + ftrit*deni/3.0D0 + 4.0D0*fhe3*deni/3.0D0 + &
         dnalp + dnprot + &
         (1.0D0-ftritbm)*dnbeam/2.0D0 + ftritbm*dnbeam/3.0D0 + &
         dene*(3.0D0*fc + 4.0D0*fo + (z_highz*z_highz/m_highz)*f_highz) &
         ) / dene

  end subroutine betcom

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine plasma_composition

    !+ad_name  plasma_composition
    !+ad_summ  Calculates various plasma component fractional makeups
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This subroutine determines the various plasma component
    !+ad_desc  fractional makeups. It is the replacement for the original
    !+ad_desc  routine <CODE>betcom</CODE>, and is used in conjunction with
    !+ad_desc  the new impurity radiation model
    !+ad_prob  None
    !+ad_call  element2index
    !+ad_call  report_error
    !+ad_hist  13/05/14 PJK Initial version
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  23/02/16 HL impurity Z now dependent on te
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: znimp, pc, znfuel
    integer :: imp
    integer :: first_call = 1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Ion density components
    !  ======================

    !  Alpha ash portion

    dnalp = dene * ralpne

    !  Protons
    !  This calculation will be wrong on the first call as the particle
    !  production rates are evaluated later in the calling sequence
    !  Issue #557 Allow protium impurity to be specified: 'protium'
    !  This will override the calculated value which is a minimum.

    if (alpharate < 1.0D-6) then  !  not calculated yet...
       dnprot = max(protium*dene, dnalp * (fhe3 + 1.0D-3)) !  rough estimate
    else
       dnprot = max(protium*dene, dnalp * protonrate/alpharate)
    end if

    !  Beam hot ion component
    !  If ignited, prevent beam fusion effects

    if (ignite == 0) then
       dnbeam = dene * rnbeam
    else
       dnbeam = 0.0D0
    end if

    !  Sum of Zi.ni for all impurity ions (those with charge > helium)

    znimp = 0.0D0
    do imp = 1,nimp
       if (impurity_arr(imp)%Z > 2) then
         ! znimp = znimp + impurity_arr(imp)%Z*(impurity_arr(imp)%frac * dene)
          znimp = znimp + Zav_of_te(impurity_arr(imp),te)*(impurity_arr(imp)%frac * dene)
       end if
    end do

    !  Fuel portion - conserve charge neutrality
    !  znfuel is the sum of Zi.ni for the three fuel ions

    znfuel = dene - 2.0D0*dnalp - dnprot - dnbeam - znimp

    !  Fuel ion density, deni
    !  For D-T-He3 mix, deni = nD + nT + nHe3, while znfuel = nD + nT + 2*nHe3
    !  So deni = znfuel - nHe3 = znfuel - fhe3*deni

    deni = znfuel/(1.0D0+fhe3)

    !  Ensure that deni is never negative or zero

    if (deni < 0.0D0) then
       fdiags(1) = deni ; call report_error(78)
       deni = max(deni,1.0D0)
    end if

    !  Set hydrogen and helium impurity fractions for
    !  radiation calculations

    impurity_arr(element2index('H_'))%frac = &
         (dnprot + (fdeut+ftrit)*deni + dnbeam)/dene

    impurity_arr(element2index('He'))%frac = fhe3*deni/dene + ralpne

    !  Total impurity density

    dnz = 0.0D0
    do imp = 1,nimp
       if (impurity_arr(imp)%Z > 2) then
          dnz = dnz + impurity_arr(imp)%frac*dene
       end if
    end do

    !  Total ion density

    dnitot = deni + dnalp + dnprot + dnbeam + dnz

    !  Set some (obsolescent) impurity fraction variables
    !  for the benefit of other routines

    rncne = impurity_arr(element2index('C_'))%frac
    rnone = impurity_arr(element2index('O_'))%frac
    if (zfear == 0) then
       rnfene = impurity_arr(element2index('Fe'))%frac
    else
       rnfene = impurity_arr(element2index('Ar'))%frac
    end if

    !  Effective charge
    !  Calculation should be sum(ni.Zi^2) / sum(ni.Zi),
    !  but ne = sum(ni.Zi) through quasineutrality

    zeff = 0.0D0
    do imp = 1,nimp
       !zeff = zeff + impurity_arr(imp)%frac * (impurity_arr(imp)%Z)**2
       zeff = zeff + impurity_arr(imp)%frac * Zav_of_te(impurity_arr(imp),te)**2
    end do

    !  Define coulomb logarithm
    !  (collisions: ion-electron, electron-electron)

    dlamee = 31.0D0 - (log(dene)/2.0D0) + log(te*1000.0D0)
    dlamie = 31.3D0 - (log(dene)/2.0D0) + log(te*1000.0D0)

    !  Fraction of alpha energy to ions and electrons
    !  From Max Fenstermacher
    !  (used with electron and ion power balance equations only)
    !  No consideration of pchargepv here...

    !  pcoef now calculated in plasma_profiles, after the very first
    !  call of plasma_composition; use old parabolic profile estimate
    !  in this case

    if (first_call == 1) then
       pc = (1.0D0 + alphan)*(1.0D0 + alphat)/(1.0D0+alphan+alphat)
       first_call = 0
    else
       pc = pcoef
    end if

    falpe = 0.88155D0 * exp(-te*pc/67.4036D0)
    falpi = 1.0D0 - falpe

    !  Average atomic masses

    afuel = 2.0D0*fdeut + 3.0D0*ftrit + 3.0D0*fhe3
    abeam = 2.0D0*(1.0D0-ftritbm) + 3.0D0*ftritbm

    !  Density weighted mass

    aion = afuel*deni + 4.0D0*dnalp + dnprot + abeam*dnbeam
    do imp = 1,nimp
       if (impurity_arr(imp)%Z > 2) then
          aion = aion + dene*impurity_arr(imp)%frac*impurity_arr(imp)%amass
       end if
    end do
    aion = aion/dnitot

    !  Mass weighted plasma effective charge

    zeffai = ( fdeut*deni/2.0D0 + ftrit*deni/3.0D0 + 4.0D0*fhe3*deni/3.0D0 + &
         dnalp + dnprot + (1.0D0-ftritbm)*dnbeam/2.0D0 + ftritbm*dnbeam/3.0D0 &
         ) / dene
    do imp = 1,nimp
       if (impurity_arr(imp)%Z > 2) then
          zeffai = zeffai + impurity_arr(imp)%frac &
          !     * (impurity_arr(imp)%Z)**2 / impurity_arr(imp)%amass
               * Zav_of_te(impurity_arr(imp),te)**2 / impurity_arr(imp)%amass
       end if
    end do

  end subroutine plasma_composition

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine culdlm(bt,idensl,pdivt,plascur,prn1,qcyl,q95, &
       rmajor,rminor,sarea,zeff,dlimit,dnelimt)

    !+ad_name  culdlm
    !+ad_summ  Density limit calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  bt       : input real :  toroidal field on axis (T)
    !+ad_args  idensl   : input/output integer : switch denoting which formula to enforce
    !+ad_args  pdivt    : input real :  power flowing to the edge plasma via
    !+ad_argc                           charged particles (MW)
    !+ad_args  plascur  : input real :  plasma current (A)
    !+ad_args  prn1     : input real :  edge density / average plasma density
    !+ad_args  qcyl     : input real :  equivalent cylindrical safety factor (qstar)
    !+ad_args  q95      : input real :  safety factor at 95% surface
    !+ad_args  rmajor   : input real :  plasma major radius (m)
    !+ad_args  rminor   : input real :  plasma minor radius (m)
    !+ad_args  sarea    : input real :  plasma surface area (m**2)
    !+ad_args  zeff     : input real :  plasma effective charge
    !+ad_args  dlimit(7): output real array : average plasma density limit using
    !+ad_argc                                 seven different models (m**-3)
    !+ad_args  dnelimt  : output real : enforced average plasma density limit (m**-3)
    !+ad_desc  This routine calculates several different formulae for the
    !+ad_desc  density limit, and enforces the one chosen by the user.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  17/09/97 PJK Added Greenwald limit (idensl=7)
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Removed pi from argument list
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(inout) :: idensl
    real(kind(1.0D0)), intent(in) :: bt, pdivt, plascur, prn1, q95, &
         qcyl, rmajor, rminor, sarea, zeff
    real(kind(1.0D0)), intent(out) :: dnelimt
    real(kind(1.0D0)), dimension(7), intent(out) :: dlimit

    !  Local variables

    real(kind(1.0D0)) :: denom, dlim, qperp

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check for illegal values of IDENSL

    if ((idensl < 1).or.(idensl > 7)) then
       idiags(1) = idensl ; call report_error(79)
    end if

    dlimit(:) = 0.0D0

    !  Power per unit area crossing the plasma edge
    !  (excludes radiation and neutrons)

    qperp = pdivt/sarea

    !  Old ASDEX density limit formula
    !  This applies to the density at the plasma edge, so must be scaled
    !  to give the density limit applying to the average plasma density.

    dlim = 1.54D20 * qperp**0.43D0 * bt**0.31D0 /(q95*rmajor)**0.45D0
    dlimit(1) = dlim/prn1

    !  Borrass density limit model for ITER (I)
    !  This applies to the density at the plasma edge, so must be scaled
    !  to give the density limit applying to the average plasma density.
    !  Borrass et al, ITER-TN-PH-9-6 (1989)

    dlim = 1.8D20 * qperp**0.53D0 * bt**0.31D0 /(q95*rmajor)**0.22D0
    dlimit(2) = dlim/prn1

    !  Borrass density limit model for ITER (II)
    !  This applies to the density at the plasma edge, so must be scaled
    !  to give the density limit applying to the average plasma density.
    !  This formula is (almost) identical to that in the original routine
    !  denlim (now deleted).

    dlim = 0.5D20 * qperp**0.57D0 * bt**0.31D0 /(q95*rmajor)**0.09D0
    dlimit(3) = dlim/prn1

    !  JET edge radiation density limit model
    !  This applies to the density at the plasma edge, so must be scaled
    !  to give the density limit applying to the average plasma density.
    !  qcyl=qstar here, but literature is not clear.

    denom = (zeff-1.0D0)*( 1.0D0-4.0D0/(3.0D0*qcyl) )
    if (denom <= 0.0D0) then
       if (idensl == 4) then
          fdiags(1) = denom ; fdiags(2) = qcyl
          call report_error(80)
          idensl = 5
       end if
       dlimit(4) = 0.0D0
    else
       dlim = 1.0D20 * sqrt(pdivt/denom)
       dlimit(4) = dlim/prn1
    end if

    !  JET simplified density limit model
    !  This applies to the density at the plasma edge, so must be scaled
    !  to give the density limit applying to the average plasma density.

    dlim = 0.237D20 * bt * sqrt(pdivt)/rmajor
    dlimit(5) = dlim/prn1

    !  Hugill-Murakami M.q limit
    !  qcyl=qstar here, which is okay according to the literature

    dlimit(6) = 3.0D20 * bt / (rmajor*qcyl)

    !  Greenwald limit

    dlimit(7) = 1.0D14 * plascur/(pi*rminor*rminor)

    !  Enforce the chosen density limit

    dnelimt = dlimit(idensl)

  end subroutine culdlm					 

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pcond(afuel,palpmw,aspect,bt,dnitot,dene,dnla,eps,hfact, &
       iinvqd,isc,ignite,kappa,kappa95,kappaa,pchargemw,pinjmw,&
       plascur,pcoreradpv,rmajor,rminor,te,ten,tin,q,qstar,vol, &
       xarea,zeff,ptrepv,ptripv,tauee,tauei,taueff,powerht)

    !+ad_name  pcond
    !+ad_summ  Routine to calculate the confinement times and
    !+ad_summ  the transport power loss terms.
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  afuel     : input real :  average mass of fuel (amu)
    !+ad_args  palpmw    : input real :  alpha particle power (MW)
    !+ad_args  aspect    : input real :  aspect ratio
    !+ad_args  bt        : input real :  toroidal field on axis (T)
    !+ad_args  dene      : input real :  volume averaged electron density (/m3)
    !+ad_args  dnitot    : input real :  total ion density (/m3)
    !+ad_args  dnla      : input real :  line-averaged electron density (/m3)
    !+ad_args  eps       : input real :  inverse aspect ratio
    !+ad_args  hfact     : input real :  H factor on energy confinement scalings
    !+ad_args  iinvqd    : input integer :  switch for inverse quadrature
    !+ad_args  isc       : input integer :  switch for energy confinement scaling to use
    !+ad_args  ignite    : input integer :  switch for ignited calculation
    !+ad_args  kappa     : input real :  plasma elongation
    !+ad_args  kappa95   : input real :  plasma elongation at 95% surface
    !+ad_args  kappaa    : output real : plasma elongation calculated using area ratio
    !+ad_args  pchargemw : input real :  non-alpha charged particle fusion power (MW)
    !+ad_args  pinjmw    : input real :  auxiliary power to ions and electrons (MW)
    !+ad_args  plascur   : input real :  plasma current (A)
    !+ad_args  pcoreradpv: input real :  total core radiation power (MW/m3)
    !+ad_args  q         : input real :  edge safety factor (tokamaks), or
    !+ad_argc                            rotational transform iotabar (stellarators)
    !+ad_args  qstar     : input real :  equivalent cylindrical edge safety factor
    !+ad_args  rmajor    : input real :  plasma major radius (m)
    !+ad_args  rminor    : input real :  plasma minor radius (m)
    !+ad_args  te        : input real :  average electron temperature (keV)
    !+ad_args  ten       : input real :  density weighted average electron temp. (keV)
    !+ad_args  tin       : input real :  density weighted average ion temperature (keV)
    !+ad_args  vol       : input real :  plasma volume (m3)
    !+ad_args  xarea     : input real :  plasma cross-sectional area (m2)
    !+ad_args  zeff      : input real :  plasma effective charge
    !+ad_args  ptrepv    : output real : electron transport power (MW/m3)
    !+ad_args  ptripv    : output real : ion transport power (MW/m3)
    !+ad_args  tauee     : output real : electron energy confinement time (s)
    !+ad_args  taueff    : output real : global energy confinement time (s)
    !+ad_args  tauei     : output real : ion energy confinement time (s)
    !+ad_args  powerht   : output real : heating power (MW) assumed in calculation
    !+ad_desc  This subroutine calculates the energy confinement time
    !+ad_desc  using one of a large number of scaling laws, and the
    !+ad_desc  transport power loss terms.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  30/06/94 PJK Added stellarator scaling laws 20-23
    !+ad_hist  07/12/95 PJK Added pcharge to plasma input power
    !+ad_hist  14/11/97 PJK Added ITER-97 scaling laws (26,27)
    !+ad_hist  01/04/98 PJK Added ITER-96P scaling law (28) and moved
    !+ad_hisc               calculation of dnla into BETCOM instead
    !+ad_hist  26/06/98 PJK Added scaling laws 29,30,31
    !+ad_hist  08/10/98 PJK Added scaling laws 32,33,34,35,36
    !+ad_hist  16/07/01 PJK Added KAPPAA to argument list
    !+ad_hist  23/05/06 PJK Ensured that powerht is always positive
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  23/01/13 PJK Added stellarator scaling laws 37,38
    !+ad_hist  07/11/13 PJK Modified prad description
    !+ad_hist  20/05/14 PJK Changed prad argument to pcorerad;
    !+ad_hisc               introduced iradloss switch;
    !+ad_hisc               added falpha multiplier to alpmw term
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  03/06/14 PJK Changed pchargepv usage to pchargemw
    !+ad_hist  17/06/14 PJK Added scaling law 39
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  13/11/14 PJK Modified iradloss usage
    !+ad_hist  17/06/15 MDK Added Murari scaling (40)
    !+ad_hist  02/11/16 HL  Added Petty, Lang scalings (41,42)
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  N. A. Uckan and ITER Physics Group,
    !+ad_docc    "ITER Physics Design Guidelines: 1989",
    !+ad_docc    ITER Documentation Series, No. 10, IAEA/ITER/DS/10 (1990)
    !+ad_docc  A. Murari et al 2015 Nucl. Fusion, 55, 073009
    !+ad_docc  C.C. Petty 2008 Phys. Plasmas, 15, 080501
    !+ad_docc  P.T. Lang et al. 2012 IAEA conference proceeding EX/P4-01
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iinvqd, isc, ignite
    real(kind(1.0D0)), intent(in) :: afuel, palpmw, aspect, bt, dene, &
         dnitot, dnla, eps, hfact, kappa, kappa95, pchargemw, pinjmw, &
         plascur, pcoreradpv, q, qstar, rmajor, rminor, te, &
         ten, tin, vol, xarea, zeff
    real(kind(1.0D0)), intent(out) :: kappaa, powerht, ptrepv, ptripv, &
         tauee, taueff, tauei

    !  Local variables

    real(kind(1.0D0)) :: chii,ck2,denfac,dnla19,dnla20,eps2,gjaeri,iotabar, &
         n20,pcur,qhat,ratio,rll,str2,str5,taueena,tauit1,tauit2, &
         term1,term2, h, qratio, nratio, nGW

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Neoclassical ion transport loss
    !  Calculate ion energy confinement time
    !
    !  N.B. This calculation is superseded later in the routine

    eps2 = eps/2.0D0
    str5 = (2.0D0/(1.0D0+(kappa**2)))
    ck2 = (0.66D0+(1.88D0*(sqrt(eps2)))-(1.54D0*eps2))* &
         (1.0D0+(1.5D0*(eps2**2)))
    chii = (6.5D-22)*ck2*zeff*(aspect**1.5D0)*dene*(q**2)*str5/ &
         ((sqrt(tin))*(bt**2))
    str2 = (2.0D0*(kappa**2)/(1.0D0+(kappa**2)))
    tauei = 0.375D0*rminor**2/chii*str2

    !  Calculate heating power (MW)

    powerht = falpha*palpmw + pchargemw + pohmmw

    !  If the device is not ignited, add the injected auxiliary power

    if (ignite == 0) powerht = powerht + pinjmw

    !  Include the radiation as a loss term if requested

    if (iradloss == 0) then
       powerht = powerht - pradpv*vol
    else if (iradloss == 1) then
       powerht = powerht - pcoreradpv*vol
    else
       continue  !  do not adjust powerht for radiation
    end if

    !  Ensure heating power is positive (shouldn't be necessary)

    powerht = max(powerht,1.0D-3)

    !  Line averaged electron density in scaled units

    dnla20 = dnla * 1.0D-20
    dnla19 = dnla * 1.0D-19

    !  Volume averaged electron density in units of 10**20 m**-3

    n20 = dene / 1.0D20

    !  Plasma current in MA

    pcur = plascur / 1.0D6

    !  kappaa = plasma X-sectional area/(pi*rminor*rminor) by definition

    kappaa = xarea/(pi*rminor*rminor)

    !  Calculate Neo-Alcator confinement time (used in several scalings)

    taueena = 0.07D0 * n20 * rminor * rmajor*rmajor * qstar

    !  For reference (see startup.f90):
    !  gtaue = offset term in tauee scaling
    !  ptaue = exponent for density term in tauee scaling
    !  qtaue = exponent for temperature term in tauee scaling
    !  rtaue = exponent for power term in tauee scaling

    !  Electron energy confinement times

    select case (isc)

    case (1)  !  Neo-Alcator scaling (ohmic)
       !tauee = taueena
       tauee = hfact * taueena
       gtaue = 0.0D0
       ptaue = 1.0D0
       qtaue = 0.0D0
       rtaue = 0.0D0

    case (2)  !  Mirnov scaling (H-mode)
       tauee = hfact * 0.2D0 * rminor * sqrt(kappa95) * pcur
       gtaue = 0.0D0
       ptaue = 0.0D0
       qtaue = 0.0D0
       rtaue = 0.0D0

    case (3)  !  Merezhkin-Muhkovatov scaling (L-mode)
       tauee = hfact * 3.5D-3 * rmajor**2.75D0 * rminor**0.25D0 * &
            kappa95**0.125D0 * qstar * dnla20 * sqrt(afuel) / &
            sqrt(ten/10.0D0)
       gtaue = 0.0D0
       ptaue = 1.0D0
       qtaue = -0.5D0
       rtaue = 0.0D0

    case (4)  !  Shimomura scaling (H-mode)
       tauee = hfact * 0.045D0 * rmajor * rminor * bt * sqrt(kappa95) &
            * sqrt(afuel)
       gtaue = 0.0D0
       ptaue = 0.0D0
       qtaue = 0.0D0
       rtaue = 0.0D0

    case (5)  !  Kaye-Goldston scaling (L-mode)
       tauee = hfact * 0.055D0 * kappa95**0.28D0 * pcur**1.24D0 * &
            n20**0.26D0 * rmajor**1.65D0 * sqrt(afuel/1.5D0) / &
            ( bt**0.09D0 * rminor**0.49D0 * powerht**0.58D0 )
       gtaue = 0.0D0
       ptaue = 0.26D0
       qtaue = 0.0D0
       rtaue = -0.58D0
       if (iinvqd /= 0) tauee = 1.0D0 / &
            sqrt(1.0D0/taueena**2 + 1.0D0/tauee**2)

    case (6)  !  ITER Power scaling - ITER 89-P (L-mode)
       tauee = hfact * 0.048D0 * pcur**0.85D0 * rmajor**1.2D0 * &
            rminor**0.3D0 * sqrt(kappa) * dnla20**0.1D0 * bt**0.2D0 * &
            sqrt(afuel) / sqrt(powerht)
       gtaue = 0.0D0
       ptaue = 0.1D0
       qtaue = 0.0D0
       rtaue = -0.5D0

    case (7)  !  ITER Offset linear scaling - ITER 89-O (L-mode)

       term1 = 0.04D0 * pcur**0.5D0 * rmajor**0.3D0 * &
            rminor**0.8D0 * kappa**0.6D0 * afuel**0.5D0
       term2 = 0.064D0 * pcur**0.8D0 * rmajor**1.6D0 * &
            rminor**0.6D0 * kappa**0.5D0 * dnla20**0.6D0 * &
            bt**0.35D0 * afuel**0.2D0 / powerht
       tauee = hfact * (term1 + term2)
       gtaue = hfact*term1
       ptaue = 0.6D0
       qtaue = 0.0D0
       rtaue = -1.0D0

    case (8)  !  Rebut-Lallia offset linear scaling (L-mode)
       rll = (rminor**2 * rmajor * kappa95)**0.333D0
       tauee = hfact * 1.65D0 * sqrt(afuel/2.0D0) * &
            ( 1.2D-2 * pcur * rll**1.5D0 / sqrt(zeff) + &
            0.146D0 * dnla20**0.75D0 * sqrt(pcur) * sqrt(bt) * &
            rll**2.75D0 * zeff**0.25D0 /powerht )
       gtaue = hfact * 1.65D0 * sqrt(afuel/2.0D0) * &
            (1.2D-2 * pcur * rll**1.5D0 / sqrt(zeff))
       ptaue = 0.75D0
       qtaue = 0.0D0
       rtaue = -1.0D0

    case (9)  !  Goldston scaling (L-mode)
       tauee = hfact * 0.037D0 * pcur * rmajor**1.75D0 * &
            rminor**(-0.37D0) * sqrt(kappa95) * sqrt(afuel/1.5D0) / &
            sqrt(powerht)
       gtaue = 0.0D0
       ptaue = 0.0D0
       qtaue = 0.0D0
       rtaue = -0.5D0
       if (iinvqd /= 0) tauee = 1.0D0 / &
            sqrt(1.0D0/taueena**2 + 1.0D0/tauee**2)

    case (10)  !  T10 scaling
       denfac = dnla20 * rmajor * qstar / (1.3D0*bt)
       denfac = min(1.0D0,denfac)
       tauee = hfact * 0.095D0 * rmajor * rminor * bt * &
            sqrt(kappa95) * denfac / powerht**0.4D0 * &
            ( zeff**2 * pcur**4 / &
            (rmajor * rminor * qstar**3 * kappa95**1.5D0) )**0.08D0
       gtaue = 0.0D0
       ptaue = 1.0D0
       qtaue = 0.0D0
       rtaue = -0.4D0

    case (11)  !  JAERI scaling
       gjaeri = zeff**0.4D0 * ((15.0D0-zeff)/20.0D0)**0.6D0 * &
            (3.0D0 * qstar * (qstar+5.0D0) / ((qstar+2.0D0) * &
            (qstar+7.0D0)))**0.6D0
       tauee = hfact * (0.085D0 * kappa95 * rminor**2 * sqrt(afuel) + &
            0.069D0 * n20**0.6D0 * pcur * bt**0.2D0 * rminor**0.4D0 * &
            rmajor**1.6D0 * sqrt(afuel) * gjaeri * kappa95**0.2D0 / &
            powerht)
       gtaue = hfact * 0.085D0 * kappa95 * rminor**2 * sqrt(afuel)
       ptaue = 0.6D0
       qtaue = 0.0D0
       rtaue = -1.0D0

    case (12)  !  Kaye-Big scaling
       tauee = hfact * 0.105D0 * sqrt(rmajor) * rminor**0.8D0 * &
            bt**0.3D0 * kappa95**0.25D0 * pcur**0.85D0 * &
            n20**0.1D0 * afuel**0.5D0 / powerht**0.5D0
       gtaue = 0.0D0
       ptaue = 0.1D0
       qtaue = 0.0D0
       rtaue = -0.5D0

    case (13)  !  ITER H-mode scaling - ITER H90-P
       tauee = hfact * 0.064D0 * pcur**0.87D0 * rmajor**1.82D0 * &
            rminor**(-0.12D0) * kappa**0.35D0 * dnla20**0.09D0 * &
            bt**0.15D0 * sqrt(afuel) / sqrt(powerht)
       gtaue = 0.0D0
       ptaue = 0.09D0
       qtaue = 0.0D0
       rtaue = -0.5D0

    case (14)  !  Minimum of ITER 89-P (isc=6) and ITER 89-O (isc=7)
       tauit1 = hfact * 0.048D0 * pcur**0.85D0 * rmajor**1.2D0 * &
            rminor**0.3D0 * sqrt(kappa) * dnla20**0.1D0 * bt**0.2D0 * &
            sqrt(afuel) / sqrt(powerht)
       term1 = 0.04D0 * pcur**0.5D0 * rmajor**0.3D0 * &
            rminor**0.8D0 * kappa**0.6D0 * afuel**0.5D0
       term2 = 0.064D0 * pcur**0.8D0 * rmajor**1.6D0 * &
            rminor**0.6D0 * kappa**0.5D0 * dnla20**0.6D0 * &
            bt**0.35D0 * afuel**0.2D0 / powerht
       tauit2 = hfact * (term1 + term2)
       tauee = min(tauit1,tauit2)

       if (tauit1 < tauit2) then
          gtaue = 0.0D0
          ptaue = 0.1D0
          qtaue = 0.0D0
          rtaue = -0.5D0
       else
          gtaue = hfact*term1
          ptaue = 0.6D0
          qtaue = 0.0D0
          rtaue = -1.0D0
       end if

    case (15)  !  Riedel scaling (L-mode)
       tauee = hfact * 0.044D0 * pcur**0.93D0 * rmajor**1.37D0 * &
            rminor**(-0.049D0) * kappa95**0.588D0 * dnla20**0.078D0 * &
            bt**0.152D0 / powerht**0.537D0
       gtaue = 0.0D0
       ptaue = 0.078D0
       qtaue = 0.0D0
       rtaue = -0.537D0

    case (16)  !  Christiansen et al scaling (L-mode)
       tauee = hfact * 0.24D0 * pcur**0.79D0 * rmajor**0.56D0 * &
            rminor**1.46D0 * kappa95**0.73D0 * dnla20**0.41D0 * &
            bt**0.29D0 / (powerht**0.79D0 * afuel**0.02D0)
       gtaue = 0.0D0
       ptaue = 0.41D0
       qtaue = 0.0D0
       rtaue = -0.79D0

    case (17)  !  Lackner-Gottardi scaling (L-mode)
       qhat = (1.0D0+kappa95**2) * rminor**2 * bt /(0.4D0 * pcur * rmajor)
       tauee = hfact * 0.12D0 * pcur**0.8D0 * rmajor**1.8D0 * &
            rminor**0.4D0 * kappa95 * (1.0D0+kappa95)**(-0.8D0) * &
            dnla20**0.6D0 * qhat**0.4D0 / powerht**0.6D0
       gtaue = 0.0D0
       ptaue = 0.6D0
       qtaue = 0.0D0
       rtaue = -0.6D0

    case (18)  !  Neo-Kaye scaling (L-mode)
       tauee = hfact * 0.063D0 * pcur**1.12D0 * rmajor**1.3D0 * &
            rminor**(-0.04D0) * kappa95**0.28D0 * dnla20**0.14D0 * &
            bt**0.04D0 * sqrt(afuel) / powerht**0.59D0
       gtaue = 0.0D0
       ptaue = 0.14D0
       qtaue = 0.0D0
       rtaue = -0.59D0

    case (19)  !  Riedel scaling (H-mode)
       tauee = hfact * 0.1D0 * sqrt(afuel) * pcur**0.884D0 * &
            rmajor**1.24D0 * rminor**(-0.23D0) * kappa95**0.317D0 * &
            bt**0.207D0 * dnla20**0.105D0 / powerht**0.486D0
       gtaue = 0.0D0
       ptaue = 0.105D0
       qtaue = 0.0D0
       rtaue = -0.486D0

    case (20)  !  Amended version of ITER H90-P law
       !  Nuclear Fusion 32 (1992) 318
       tauee = hfact * 0.082D0 * pcur**1.02D0 * &
            bt**0.15D0 * sqrt(afuel) * rmajor**1.60D0 / &
            (powerht**0.47D0 * kappa**0.19D0)
       gtaue = 0.0D0
       ptaue = 0.0D0
       qtaue = 0.0D0
       rtaue = -0.47D0

    case (21)  !  Large Helical Device scaling (stellarators)
       !  S.Sudo, Y.Takeiri, H.Zushi et al., Nuclear Fusion 30 (1990) 11
       tauee = hfact * 0.17D0 * rmajor**0.75D0 * rminor**2 * &
            dnla20**0.69D0 * bt**0.84D0 * powerht**(-0.58D0)
       gtaue = 0.0D0
       ptaue = 0.69D0
       qtaue = 0.0D0
       rtaue = 0.58D0

    case (22)  !  Gyro-reduced Bohm scaling
       !  R.J.Goldston, H.Biglari, G.W.Hammett et al., Bull.Am.Phys.Society,
       !  volume 34, 1964 (1989)
       tauee = hfact * 0.25D0 * bt**0.8D0 * dnla20**0.6D0 * &
            powerht**(-0.6D0) * rminor**2.4D0 * rmajor**0.6D0
       gtaue = 0.0D0
       ptaue = 0.6D0
       qtaue = 0.0D0
       rtaue = -0.6D0

    case (23)  !  Lackner-Gottardi stellarator scaling
       !  K.Lackner and N.A.O.Gottardi, Nuclear Fusion, 30, p.767 (1990)
       iotabar = q  !  dummy argument q is actual argument iotabar for stellarators
       tauee = hfact * 0.17D0 * rmajor * rminor**2 * dnla20**0.6D0 * &
            bt**0.8D0 * powerht**(-0.6D0) * iotabar**0.4D0
       gtaue = 0.0D0
       ptaue = 0.6D0
       qtaue = 0.0D0
       rtaue = -0.6D0

    case (24)  !  ITER-93H scaling (ELM-free; multiply by 0.85 for ELMy version)
       !  S.Kaye and the ITER Joint Central Team and Home Teams, in Plasma
       !  Physics and Controlled Nuclear Fusion Research (Proc. 15th
       !  Int. Conf., Seville, 1994) IAEA-CN-60/E-P-3
       tauee = hfact * 0.053D0 * pcur**1.06D0 * bt**0.32D0 * &
            powerht**(-0.67D0) * afuel**0.41D0 * rmajor**1.79D0 * &
            dnla20**0.17D0 * aspect**0.11D0 * kappa**0.66D0
       gtaue = 0.0D0
       ptaue = 0.17D0
       qtaue = 0.0D0
       rtaue = -0.67D0

   case (25)  !  Issue #508 Remove RFP option.

       !  Next two are ITER-97 H-mode scalings
       !  J. G. Cordey et al., EPS Berchtesgaden, 1997

    case (26)  !  ELM-free: ITERH-97P
       tauee = hfact * 0.031D0 * pcur**0.95D0 * bt**0.25D0 * &
            powerht**(-0.67D0) * dnla19**0.35D0 * &
            rmajor**1.92D0 * aspect**(-0.08D0) * kappa**0.63D0 * &
            afuel**0.42D0
       gtaue = 0.0D0
       ptaue = 0.35D0
       qtaue = 0.0D0
       rtaue = -0.67D0

    case (27)  !  ELMy: ITERH-97P(y)
       tauee = hfact * 0.029D0 * pcur**0.90D0 * bt**0.20D0 * &
            powerht**(-0.66D0) * dnla19**0.40D0 * &
            rmajor**2.03D0 * aspect**(-0.19D0) * kappa**0.92D0 * &
            afuel**0.2D0
       gtaue = 0.0D0
       ptaue = 0.4D0
       qtaue = 0.0D0
       rtaue = -0.66D0

    case (28)  !  ITER-96P (= ITER-97L) L-mode scaling
       !  S.M.Kaye and the ITER Confinement Database Working Group,
       !  Nuclear Fusion 37 (1997) 1303
       !  N.B. tau_th formula used
       tauee = hfact * 0.023D0 * pcur**0.96D0 * bt**0.03D0 * &
            kappa95**0.64D0 * rmajor**1.83D0 * aspect**0.06D0 * &
            dnla19**0.40D0 * afuel**0.20D0 * powerht**(-0.73D0)
       gtaue = 0.0D0
       ptaue = 0.4D0
       qtaue = 0.0D0
       rtaue = -0.73D0

    case (29)  !  Valovic modified ELMy-H mode scaling
       tauee = hfact * 0.067D0 * pcur**0.9D0 * bt**0.17D0 * &
            dnla19**0.45D0 * afuel**0.05D0 * rmajor**1.316D0 * &
            rminor**0.79D0 * kappa**0.56D0 * powerht**(-0.68D0)
       gtaue = 0.0D0
       ptaue = 0.45D0
       qtaue = 0.0D0
       rtaue = -0.68D0

    case (30)  !  Kaye PPPL Workshop April 1998 L-mode scaling
       tauee = hfact * 0.021D0 * pcur**0.81D0 * bt**0.14D0 * &
            kappa**0.7D0 * rmajor**2.01D0 * aspect**(-0.18D0) * &
            dnla19**0.47D0 * afuel**0.25D0 * powerht**(-0.73D0)
       gtaue = 0.0D0
       ptaue = 0.47D0
       qtaue = 0.0D0
       rtaue = -0.73D0

    case (31)  !  ITERH-PB98P(y), ELMy H-mode scaling
       tauee = hfact * 0.0615D0 * pcur**0.9D0 * bt**0.1D0 * &
            dnla19**0.4D0 * powerht**(-0.66D0) * rmajor**2 * &
            kappaa**0.75D0 * aspect**(-0.66D0) * afuel**0.2D0
       gtaue = 0.0D0
       ptaue = 0.4D0
       qtaue = 0.0D0
       rtaue = -0.66D0

    case (32)  !  IPB98(y), ELMy H-mode scaling
       !  Nuclear Fusion 39 (1999) 2175
       tauee = hfact * 0.0365D0 * pcur**0.97D0 * bt**0.08D0 * &
            dnla19**0.41D0 * powerht**(-0.63D0) * rmajor**1.93D0 * &
            kappa**0.67D0 * aspect**(-0.23D0) * afuel**0.2D0
       gtaue = 0.0D0
       ptaue = 0.41D0
       qtaue = 0.0D0
       rtaue = -0.63D0

    case (33)  !  IPB98(y,1), ELMy H-mode scaling
       !  Nuclear Fusion 39 (1999) 2175
       tauee = hfact * 0.0503D0 * pcur**0.91D0 * bt**0.15D0 * &
            dnla19**0.44D0 * powerht**(-0.65D0) * rmajor**2.05D0 * &
            kappaa**0.72D0 * aspect**(-0.57D0) * afuel**0.13D0
       gtaue = 0.0D0
       ptaue = 0.44D0
       qtaue = 0.0D0
       rtaue = -0.65D0

    case (34)  !  IPB98(y,2), ELMy H-mode scaling
       !  Nuclear Fusion 39 (1999) 2175
       tauee = hfact * 0.0562D0 * pcur**0.93D0 * bt**0.15D0 * &
            dnla19**0.41D0 * powerht**(-0.69D0) * rmajor**1.97D0 * &
            kappaa**0.78D0 * aspect**(-0.58D0) * afuel**0.19D0
       gtaue = 0.0D0
       ptaue = 0.41D0
       qtaue = 0.0D0
       rtaue = -0.69D0

    case (35)  !  IPB98(y,3), ELMy H-mode scaling
       !  Nuclear Fusion 39 (1999) 2175
       tauee = hfact * 0.0564D0 * pcur**0.88D0 * bt**0.07D0 * &
            dnla19**0.40D0 * powerht**(-0.69D0) * rmajor**2.15D0 * &
            kappaa**0.78D0 * aspect**(-0.64D0) * afuel**0.20D0
       gtaue = 0.0D0
       ptaue = 0.4D0
       qtaue = 0.0D0
       rtaue = -0.69D0

    case (36)  !  IPB98(y,4), ELMy H-mode scaling
       !  Nuclear Fusion 39 (1999) 2175
       tauee = hfact * 0.0587D0 * pcur**0.85D0 * bt**0.29D0 * &
            dnla19**0.39D0 * powerht**(-0.70D0) * rmajor**2.08D0 * &
            kappaa**0.76D0 * aspect**(-0.69D0) * afuel**0.17D0
       gtaue = 0.0D0
       ptaue = 0.39D0
       qtaue = 0.0D0
       rtaue = -0.70D0

    case (37)  !  ISS95 stellarator scaling
       !  U. Stroth et al., Nuclear Fusion, 36, p.1063 (1996)
       !  Assumes kappa = 1.0, triang = 0.0
       iotabar = q  !  dummy argument q is actual argument iotabar for stellarators
       tauee = hfact * 0.079D0 * rminor**2.21D0 * rmajor**0.65D0 * dnla19**0.51D0 * &
            bt**0.83D0 * powerht**(-0.59D0) * iotabar**0.4D0
       gtaue = 0.0D0
       ptaue = 0.51D0
       qtaue = 0.0D0
       rtaue = -0.59D0

    case (38)  !  ISS04 stellarator scaling
       !  H. Yamada et al., Nuclear Fusion, 45, p.1684 (2005)
       !  Assumes kappa = 1.0, triang = 0.0
       iotabar = q  !  dummy argument q is actual argument iotabar for stellarators
       tauee = hfact * 0.134D0 * rminor**2.28D0 * rmajor**0.64D0 * dnla19**0.54D0 * &
            bt**0.84D0 * powerht**(-0.61D0) * iotabar**0.41D0
       gtaue = 0.0D0
       ptaue = 0.54D0
       qtaue = 0.0D0
       rtaue = -0.61D0

    case (39)  !  DS03 beta-independent H-mode scaling
       !  T. C. Luce, C. C. Petty and J. G. Cordey,
       !  Plasma Phys. Control. Fusion 50 (2008) 043001, eqn.4.13, p.67
       tauee = hfact * 0.028D0 * pcur**0.83D0 * bt**0.07D0 * &
            dnla19**0.49D0 * powerht**(-0.55D0) * rmajor**2.11D0 * &
            kappa95**0.75D0 * aspect**(-0.3D0) * afuel**0.14D0
       gtaue = 0.0D0
       ptaue = 0.49D0
       qtaue = 0.0D0
       rtaue = -0.55D0

    case (40)  !  "Non-power law" (NPL) Murari energy confinement scaling
       !   Based on the ITPA database of H-mode discharges
       !   A new approach to the formulation and validation of scaling expressions for plasma confinement in tokamaks
       !   A. Murari et al 2015 Nucl. Fusion 55 073009, doi:10.1088/0029-5515/55/7/073009
       !   Table 4.  (Issue #311)
       !  Note that aspect ratio and M (afuel) do not appear, and B (bt) only
       !  appears in the "saturation factor" h.
       h = dnla19**0.448D0 / (1.0D0 + exp(-9.403D0*(bt/dnla19)**1.365D0))
       tauee = hfact * 0.0367D0 * pcur**1.006D0 * rmajor**1.731D0 * kappaa**1.450D0 * &
               powerht**(-0.735D0) * h

       gtaue = 0.0D0
       ptaue = 0.448D0
       qtaue = 0.0D0
       rtaue = -0.735D0

    case (41) ! Beta independent dimensionless confinement scaling
       ! C.C. Petty 2008 Phys. Plasmas 15, 080501, equation 36
       ! Note that there is no dependence on the average fuel mass 'afuel'
       tauee = hfact * 0.052D0 * pcur**0.75D0 * bt**0.3D0 * &
            dnla19**0.32D0 * powerht**(-0.47D0) * rmajor**2.09D0 * &
            kappaa**0.88D0 * aspect**(-0.84D0)

       gtaue = 0.0D0
       ptaue = 0.32D0
       qtaue = 0.0D0
       rtaue = -0.47D0

    case (42) ! High density relevant confinement scaling
       ! P.T. Lang et al. 2012, IAEA conference proceeding EX/P4-01
       ! Note that in the paper kappaa is defined as V/(2pi^2Ra^2)
       ! which should be equivalent to our local definition assuming
       ! V = 2piR * (X-sectional area)
       ! q should be q95: incorrect if icurr = 2 (ST current scaling)
       qratio = q/qstar
       ! Greenwald density in m^-3
       nGW = 1.0D14 * plascur/(pi*rminor*rminor)
       nratio = dnla/nGW
       tauee = hfact * 6.94D-7 * pcur**1.3678D0 * bt**0.12D0 * &
            dnla19**0.032236D0 * powerht**(-0.74D0) * rmajor**1.2345D0 * &
            kappaa**0.37D0 * aspect**2.48205D0 * afuel**0.2D0 * &
            qratio**0.77D0 * aspect**(-0.9D0*log(aspect)) * &
            nratio**(-0.22D0*log(nratio))

       gtaue = 0.0D0
       ptaue = 0.032236D0 -0.22D0*log(nratio)
       qtaue = 0.0D0
       rtaue = -0.74D0

    case default
       idiags(1) = isc ; call report_error(81)

    end select

    !  Ion energy confinement time
    !  N.B. Overwrites earlier calculation above

    tauei = tauee

    !  Calculation of the transport power loss terms
    !  Transport losses in Watts/m3 are 3/2 * n.e.T / tau , with T in eV
    !  (here, tin and ten are in keV, and ptrepv and ptripv are in MW/m3)

    ptripv = 2.403D-22 * dnitot*tin/tauei
    ptrepv = 2.403D-22 * dene*ten/tauee

    ratio = dnitot/dene * tin/ten

    !  Global energy confinement time

    taueff = ((ratio + 1.0D0)/(ratio/tauei + 1.0D0/tauee))

    ! This is used only in subroutine startup, which is currently (r400)
    ! not used.
    ftaue = (tauee-gtaue) / &
         (n20**ptaue * (te/10.0D0)**qtaue * powerht**rtaue)

  end subroutine pcond

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine vscalc(csawth,eps,facoh,gamma,kappa,rmajor,rplas, &
       plascur,theat,tburn,phiint,rli,rlp,vsbrn,vsind,vsres,vsstt)

    !+ad_name  vscalc
    !+ad_summ  Volt-second requirements
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  csawth : input real :  coefficient for sawteeth effects
    !+ad_args  eps    : input real :  inverse aspect ratio
    !+ad_args  facoh  : input real :  fraction of plasma current produced inductively
    !+ad_args  gamma  : input real :  Ejima coeff for resistive start-up V-s component
    !+ad_args  kappa  : input real :  plasma elongation
    !+ad_args  plascur: input real :  plasma current (A)
    !+ad_args  rli    : input real :  plasma normalised inductivity
    !+ad_args  rmajor : input real :  plasma major radius (m)
    !+ad_args  rplas  : input real :  plasma resistance (ohm)
    !+ad_args  theat  : input real :  heating time (s)
    !+ad_args  tburn  : input real :  burn time (s)
    !+ad_args  phiint : output real : internal plasma volt-seconds (Wb)
    !+ad_args  rlp    : output real : plasma inductance (H)
    !+ad_args  vsbrn  : output real : volt-seconds needed during flat-top (heat+burn) (Wb)
    !+ad_args  vsind  : output real : internal and external plasma inductance V-s (Wb)
    !+ad_args  vsres  : output real : resistive losses in start-up volt-seconds (Wb)
    !+ad_args  vsstt  : output real : total volt-seconds needed (Wb)
    !+ad_desc  This subroutine calculates the volt-second requirements and some
    !+ad_desc  other related items.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Removed rmu0 from argument list
    !+ad_hist  11/06/13 PJK Removed 1.25 enhancement in rlp formula
    !+ad_hist  27/11/13 PJK Added theat to tburn in vsbrn calculation
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: csawth, eps, facoh, gamma, kappa, &
         plascur, rli, rmajor, rplas, tburn, theat
    real(kind(1.0D0)), intent(out) :: phiint, rlp, vsbrn, vsind, vsres, vsstt

    !  Local variables

    real(kind(1.0D0)) :: aeps,beps,rlpext,rlpint,vburn

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Internal inductance

    rlpint = rmu0 * rmajor * rli/2.0D0
    phiint = rlpint*plascur

    !  Start-up resistive component
    !  Uses ITER formula without the 10 V-s add-on

    vsres = gamma * rmu0*plascur*rmajor

    !  Hirshman, Neilson: Physics of Fluids, 29 (1986) p790
    !  fit for external inductance

    aeps = (1.0D0 + 1.81D0*sqrt(eps)+2.05D0*eps)*log(8.0D0/eps) &
         - (2.0D0 + 9.25D0*sqrt(eps)-1.21D0*eps)
    beps = 0.73D0 * sqrt(eps) *(1.0D0 + 2.0D0*eps**4-6.0D0*eps**5 &
         + 3.7D0*eps**6)
    rlpext = rmajor*rmu0 * aeps*(1.0D0-eps)/(1.0D0-eps+beps*kappa)

    rlp = rlpext + rlpint

    !  Inductive V-s component

    vsind = rlp * plascur
    vsstt = vsres + vsind

    !  Loop voltage during flat-top
    !  Include enhancement factor in flattop V-s requirement
    !  to account for MHD sawtooth effects.

    vburn = plascur * rplas * facoh * csawth

    !  N.B. tburn on first iteration will not be correct
    !  if the pulsed reactor option is used, but the value
    !  will be correct on subsequent calls.

    vsbrn = vburn*(theat + tburn)
    vsstt = vsstt + vsbrn

  end subroutine vscalc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine phyaux(aspect,dene,deni,fusionrate,alpharate,plascur,sbar,dnalp, &
       taueff,vol,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

    !+ad_name  phyaux
    !+ad_summ  Auxiliary physics quantities
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  aspect : input real :  plasma aspect ratio
    !+ad_args  dene   : input real :  electron density (/m3)
    !+ad_args  deni   : input real :  fuel ion density (/m3)
    !+ad_args  dnalp  : input real :  alpha ash density (/m3)
    !+ad_args  fusionrate : input real :  fusion reaction rate (/m3/s)
    !+ad_args  alpharate  : input real :  alpha particle production rate (/m3/s)
    !+ad_args  plascur: input real :  plasma current (A)
    !+ad_args  sbar   : input real :  exponent for aspect ratio (normally 1)
    !+ad_args  taueff : input real :  global energy confinement time (s)
    !+ad_args  vol    : input real :  plasma volume (m3)
    !+ad_args  burnup : output real : fractional plasma burnup
    !+ad_args  dntau  : output real : plasma average n-tau (s/m3)
    !+ad_args  figmer : output real : physics figure of merit
    !+ad_args  fusrat : output real : number of fusion reactions per second
    !+ad_args  qfuel  : output real : fuelling rate for D-T (nucleus-pairs/sec)
    !+ad_args  rndfuel: output real : fuel burnup rate (reactions/s)
    !+ad_args  taup   : output real : (alpha) particle confinement time (s)
    !+ad_desc  This subroutine calculates extra physics related items
    !+ad_desc  needed by other parts of the code
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  07/12/95 PJK Added D-He3 calculations
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  12/06/13 PJK Changed rndfuel, qfuel units from Amps
    !+ad_hist  10/09/13 PJK Modified fusion reaction rate calculation
    !+ad_hist  11/09/13 PJK Modified burnup calculation + comments
    !+ad_hist  08/05/14 PJK Modified taup calculation
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: aspect, dene, deni, dnalp, &
         fusionrate, alpharate, plascur, sbar, taueff, vol
    real(kind(1.0D0)), intent(out) :: burnup, dntau, figmer, fusrat, &
         qfuel, rndfuel, taup

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    figmer = 1.0D-6 * plascur * aspect**sbar

    dntau = taueff*dene

    !  Fusion reactions per second

    fusrat = fusionrate*vol

    !  Alpha particle confinement time (s)
    !  Number of alphas / alpha production rate

    if (alpharate /= 0.0D0) then
      taup = dnalp / alpharate
    else  !  only likely if DD is only active fusion reaction
      taup = 0.0D0
    end if

    !  Fractional burnup

    !  (Consider detailed model in: G. L. Jackson, V. S. Chan, R. D. Stambaugh,
    !  Fusion Science and Technology, vol.64, no.1, July 2013, pp.8-12)

    !  The ratio of ash to fuel particle confinement times is given by
    !  tauratio
    !  Possible logic...
    !  burnup = fuel ion-pairs burned/m3 / initial fuel ion-pairs/m3;
    !  fuel ion-pairs burned/m3 = alpha particles/m3 (for both D-T and D-He3 reactions)
    !  initial fuel ion-pairs/m3 = burnt fuel ion-pairs/m3 + unburnt fuel-ion pairs/m3
    !  Remember that unburnt fuel-ion pairs/m3 = 0.5 * unburnt fuel-ions/m3

    burnup = dnalp / (dnalp + 0.5D0*deni) / tauratio

    !  Fuel burnup rate (reactions/second) (previously Amps)

    rndfuel = fusrat

    !  Required fuelling rate (fuel ion pairs/second) (previously Amps)

    qfuel = rndfuel/burnup

  end subroutine phyaux

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine rether(alphan,alphat,dene,dlamie,te,ti,zeffai,piepv)

    !+ad_name  rether
    !+ad_summ  Routine to find the equilibration power between the
    !+ad_summ  ions and electrons
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  alphan : input real :  density profile index
    !+ad_args  alphat : input real :  temperature profile index
    !+ad_args  dene   : input real :  electron density (/m3)
    !+ad_args  dlamie : input real :  ion-electron coulomb logarithm
    !+ad_args  te     : input real :  electron temperature (keV)
    !+ad_args  ti     : input real :  ion temperature (keV)
    !+ad_args  zeffai : input real :  mass weighted plasma effective charge
    !+ad_args  piepv  : output real : ion/electron equilibration power (MW/m3)
    !+ad_desc  This routine calculates the equilibration power between the
    !+ad_desc  ions and electrons.
    !+ad_prob  No account is taken of pedestal profiles.
    !+ad_call  None
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  03/07/13 PJK Changed zeffai description
    !+ad_stat  Okay
    !+ad_docs  Unknown origin
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: alphan, alphat, dene, dlamie, &
         te, ti, zeffai
    real(kind(1.0D0)), intent(out) :: piepv

    !  Local variables

    real(kind(1.0D0)) :: conie, profie

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    profie = (1.0D0+alphan)**2 / &
         ( (2.0D0*alphan - 0.5D0*alphat + 1.0D0) * sqrt(1.0D0+alphat) )

    conie = 2.42165D-41 * dlamie * dene**2 * zeffai * profie

    piepv = conie*(ti-te)/(te**1.5D0)

  end subroutine rether

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine radpwr(imprad_model,pbrempv,plinepv,psyncpv,pcoreradpv,pedgeradpv,pradpv)

    !+ad_name  radpwr
    !+ad_summ  Radiation power interface routine
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  imprad_model : input integer : switch to choose model
    !+ad_args  pbrempv    : output real : bremsstrahlung radiation power/volume (MW/m3)
    !+ad_args  plinepv    : output real : line radiation power/volume (MW/m3)
    !+ad_args  psyncpv    : output real : synchrotron radiation power/volume (MW/m3)
    !+ad_args  pcoreradpv : output real : total core radiation power/volume (MW/m3)
    !+ad_args  pedgeradpv : output real : edge (non-core) radiation power/volume (MW/m3)
    !+ad_args  pradpv     : output real : total radiation power/volume (MW/m3)
    !+ad_desc  This routine finds the radiation powers in MW/m3 by calling
    !+ad_desc  relevant routines.
    !+ad_call  prad_ipdg89
    !+ad_call  psync_albajar_fidone
    !+ad_call  imprad
    !+ad_call  report_error
    !+ad_hist  14/05/14 PJK Redefined routine as a caller to the actual calculations
    !+ad_hist  20/05/14 PJK Clarified core radiation vs bremsstrahlung
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: imprad_model
    real(kind(1.0D0)), intent(out) :: pbrempv,plinepv,psyncpv,pcoreradpv, &
         pedgeradpv,pradpv

    !  Local variables

    real(kind(1.0D0)) :: pimpcore, pimptot

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Bremsstrahlung and line radiation

    if (imprad_model == 0) then
       call prad_ipdg89(pimpcore, pedgeradpv)
       pimptot = pimpcore + pedgeradpv
       pbrempv = 0.0D0 ; plinepv = 0.0D0  !  therefore, not useful...
    else if (imprad_model == 1) then
       call imprad(pbrempv, plinepv, pimpcore, pimptot)
       pedgeradpv = pimptot - pimpcore
    else
       idiags(1) = imprad_model ; call report_error(82)
    end if

    !  Synchrotron radiation power/volume; assumed to be from core only

    call psync_albajar_fidone(psyncpv)

    !  Total core radiation power/volume

    pcoreradpv = pimpcore + psyncpv

    !  Total radiation power/volume

    pradpv = pimptot + psyncpv  !  = pcoreradpv + pedgeradpv

  end subroutine radpwr

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine prad_ipdg89(pcoreradpv,pedgeradpv)

    !+ad_name  prad_ipdg89
    !+ad_summ  Bremsstrahlung and line radiation power calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  pcoreradpv : output real : core radiation power/volume (MW/m3)
    !+ad_args  pedgeradpv : output real : edge line radiation power/volume (MW/m3)
    !+ad_desc  This routine finds the D-T and impurity bremsstrahlung and line
    !+ad_desc  radiation powers in MW/m3, using the IPDG89 formulation.
    !+ad_prob  No account is taken of pedestal profiles.
    !+ad_call  None
    !+ad_hist  14/05/14 PJK Moved bremsstrahlung calculation here from original
    !+ad_hisc               <CODE>radpwr</CODE> routine
    !+ad_hist  19/05/14 PJK Renamed arguments
    !+ad_hist  20/05/14 PJK Renamed routine from pbrems_ipdg89
    !+ad_stat  Okay
    !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
    !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: pcoreradpv, pedgeradpv

    !  Local variables

    real(kind(1.0D0)) :: den20,fbc,fbhe,fbo,pbremdt,pbremz,pc,phe, &
         phighz,po,radexp,t10,vr

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    fbhe = 0.9D0
    fbc = 0.52D0
    fbo = 0.52D0

    den20 = dene/1.0D20
    t10 = ten/10.0D0

    !  D-T bremsstrahlung (IPDG89)
    !  Coefficient 0.016*radexp is C_B in IPDG89, with Zeff set to 1 for D-T
    !  Note that the formula in IPDG89 should use ni/1.0E20 * ne/1.0E20,
    !  not just (n20)^2 (the code below is correct)

    radexp = (1.0D0 + alphan)**1.5D0 * sqrt(1.0D0 + alphan + alphat) / &
         (1.0D0 + 2.0D0*alphan + 0.5D0*alphat)

    pbremdt = 1.6D-2 * radexp * den20**2 * (deni/dene) * sqrt(t10)

    !  High Z bremsstrahlung

    vr = rmajor * (rminor*(1.0D0 + kappa95)/2.0D0)**2 / (58.652D0*vol)
    phe = 65.8D0 * ralpne * (dene/7.0D19)**1.5D0 * vr
    pc  = 1120.0D0 * rncne * (dene/7.0D19)**1.5D0 * vr
    po  = 2240.0D0 * rnone * (dene/7.0D19)**1.5D0 * vr
    if (zfear == 1) then  !  high-Z impurity is argon
       phighz = 16000.0D0 * rnfene * (dene/7.0D19)**1.5D0 * vr
    else  !  iron
       phighz = 44800.0D0 * rnfene * (dene/7.0D19)**2.5D0 * vr
    end if
    pbremz = fbhe*phe + fbc*pc + fbo*po + fbfe*phighz

    !  Total core radiation power (this is a more accurate description than
    !  simply the bremsstrahlung power)

    pcoreradpv = pbremz + pbremdt

    !  Edge line radiation

    pedgeradpv = (1.0D0-fbhe)*phe + (1.0D0-fbc)*pc + (1.0D0-fbo)*po + &
         (1.0D0-fbfe)*phighz

  end subroutine prad_ipdg89

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine psync_albajar_fidone(psyncpv)

    !+ad_name  psync_albajar_fidone
    !+ad_summ  Synchrotron radiation power calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  psyncpv  : output real : synchrotron radiation power/volume (MW/m3)
    !+ad_desc  This routine finds the synchrotron radiation power in MW/m3,
    !+ad_desc  using the method of Albajar and Fidone.
    !+ad_prob  No account is taken of pedestal profiles.
    !+ad_call  None
    !+ad_hist  14/05/14 PJK Moved synchrotron calculation here from original
    !+ad_hisc               <CODE>radpwr</CODE> routine
    !+ad_stat  Okay
    !+ad_docs  Albajar, Nuclear Fusion 41 (2001) 665
    !+ad_docs  Fidone, Giruzzi, Granata, Nuclear Fusion 41 (2001) 1755
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: psyncpv

    !  Local variables

    real(kind(1.0D0)) :: de2o,dum,gfun,kap,kfun,pao,psync,rpow,tbet

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  tbet is betaT in Albajar, not to be confused with plasma beta

    tbet = 2.0D0

    !  rpow is the (1-Rsyn) power dependence based on plasma shape
    !  (see Fidone)

    rpow = 0.62D0

    kap = vol / (2.0D0 * pi**2 * rmajor * rminor**2)

    !  No account is taken of pedestal profiles here, other than use of
    !  the correct ne0 and te0...

    de2o = 1.0D-20*ne0
    pao = 6.04D3 * (rminor*de2o)/bt
    gfun = 0.93D0 * ( 1.0D0 + 0.85D0*exp(-0.82D0 * rmajor/rminor) )
    kfun = (alphan + 3.87D0*alphat + 1.46D0)**(-0.79D0)
    kfun = kfun * (1.98D0+alphat)**1.36D0 * tbet**2.14D0
    kfun = kfun*(tbet**1.53D0 + 1.87D0*alphat - 0.16D0)**(-1.33D0)
    dum = (1.0D0+0.12D0*(te0/(pao**0.41D0))*(1.0D0-ssync)**0.41D0)

    !  Very high T modification, from Fidone

    dum = dum**(-1.51D0)

    psync = 3.84D-8 * (1.0D0-ssync)**rpow * rmajor * rminor**1.38D0
    psync = psync * kap**0.79D0 * bt**2.62D0 * de2o**0.38D0
    psync = psync * te0 *(16.0D0+te0)**2.61D0 * dum * gfun * kfun

    !  psyncpv should be per unit volume; Albajar gives it as total

    psyncpv = psync/vol

  end subroutine psync_albajar_fidone

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine imprad(radb, radl, radcore, radtot)

    !+ad_name  imprad
    !+ad_summ  Total impurity line radiation and bremsstrahlung
    !+ad_type  Subroutine
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  radb    : output real : bremsstrahlung only (MW/m3)
    !+ad_args  radl    : output real : line radiation only (MW/m3)
    !+ad_args  radcore : output real : total impurity radiation from core (MW/m3)
    !+ad_args  radtot  : output real : total impurity radiation (MW/m3)
    !+ad_desc  This routine calculates the total radiation losses from
    !+ad_desc  impurity line radiation and bremsstrahlung for all elements
    !+ad_desc  for a given temperature and density profile.
    !+ad_desc  <P>Bremsstrahlung equation from Johner
    !+ad_desc  <P>L(z) data (coronal equilibrium) from Marco Sertoli, ASDEX-U,
    !+ad_desc  ref. Kallenbach et al.
    !+ad_prob  None
    !+ad_call  Tprofile
    !+ad_call  nprofile
    !+ad_call  impradprofile
    !+ad_call  fradcore
    !+ad_hist  17/12/13 HL  First draft of routine, based on code by R Kemp
    !+ad_hist  09/05/14 HL  Using new data structure
    !+ad_hist  14/05/14 PJK First PROCESS implementation
    !+ad_hist  19/05/14 PJK Added call to fradcore; radtot now an output arg
    !+ad_stat  Okay
    !+ad_docs  Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !+ad_docs  Sertoli, private communication
    !+ad_docs  Kallenbach et al., Plasma Phys. Control. Fus. 55 (2013) 124041
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Arguments

    real(kind(1.0D0)), intent(out) :: radb, radl, radcore, radtot

    !  Local variables

    real(kind(1.0D0)) :: rho, drho, trho,  nrho
    real(kind(1.0D0)) :: pimp, pbrem, pline
    integer :: i, imp, npts

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    npts = 200  !  originally 1000; no significant difference found
    drho = 1.0D0/real(npts,kind(1.0D0))

    radtot = 0.0D0
    radcore = 0.0D0
    radb = 0.0D0
    radl = 0.0D0

    !  Numerical integration using the midpoint rule
    !  Consider using the maths_library integrator in the future...
    !    quanc8(fun,0.0D0,1.0D0,abserr,relerr,result,errest,nofun,flag)

    do i = 0, npts-1

       rho = (0.5D0 + i)/npts
       trho = tprofile(rho, rhopedt, te0, teped, tesep, alphat, tbeta)
       nrho = nprofile(rho, rhopedn, ne0, neped, nesep, alphan)

       do imp = 1, size(impurity_arr)

          if (impurity_arr(imp)%frac > 1.0D-30) then

             call impradprofile(impurity_arr(imp), nrho, trho, pimp, pbrem, pline)

             radtot  = radtot  + pimp*rho
             radcore = radcore + pimp*rho * fradcore(rho,coreradius,coreradiationfraction)
             radb = radb + pbrem*rho
             radl = radl + pline*rho
          end if

       end do
    end do

    !  Radiation powers in MW/m3

    radtot  = 2.0D-6 * drho * radtot
    radcore = 2.0D-6 * drho * radcore
    radb    = 2.0D-6 * drho * radb
    radl    = 2.0D-6 * drho * radl

  end subroutine imprad

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine pohm(facoh,kappa95,plascur,rmajor,rminor,ten,vol, &
       zeff,pohmpv,pohmmw,rpfac,rplas)

    !+ad_name  pohm
    !+ad_summ  Ohmic power calculation
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  facoh  : input real :  fraction of plasma current produced inductively
    !+ad_args  kappa95: input real :  plasma elongation at 95% flux
    !+ad_args  plascur: input real :  plasma current (A)
    !+ad_args  rmajor : input real :  plasma major radius (m)
    !+ad_args  rminor : input real :  plasma minor radius (m)
    !+ad_args  ten    : input real :  density weighted average electron temperature (keV)
    !+ad_args  vol    : input real :  plasma volume (m3)
    !+ad_args  zeff   : input real :  plasma effective charge
    !+ad_args  pohmpv : output real : ohmic heating power per unit volume (MW/m3)
    !+ad_args  pohmmw : output real : ohmic heating power (MW)
    !+ad_args  rpfac  : output real : neoclassical resistivity enhancement factor
    !+ad_args  rplas  : output real : plasma resistance (ohm)
    !+ad_desc  This routine finds the ohmic heating power per unit volume.
    !+ad_desc  The expression is a good fit for alphan = 0.5, alphat = 1.0,
    !+ad_desc  alphaj = 1.5, aspect = 2.5 -- 4.
    !+ad_prob  Therefore, no account is taken of pedestal profiles.
    !+ad_call  report_error
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  25/07/11 PJK Correction to facoh coding
    !+ad_hist  09/11/11 PJK Initial F90 version
    !+ad_hist  11/04/13 PJK Removed ires argument
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
    !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: facoh, kappa95, plascur, rmajor, &
         rminor, ten, vol, zeff
    real(kind(1.0D0)), intent(out) :: pohmpv, pohmmw, rpfac, rplas

    !  Local variables

    real(kind(1.0D0)) :: t10

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Density weighted electron temperature in 10 keV units

    t10 = ten/10.0D0

    !  Plasma resistance, from loop voltage calculation in IPDG89

    rplas = 2.15D-9 * zeff*rmajor / (kappa95*rminor**2 * t10**1.5D0)

    !  Neo-classical resistivity enhancement factor
    !  Taken from  N. A. Uckan et al, Fusion Technology 13 (1988) p.411.
    !  The expression is valid for aspect ratios in the range 2.5--4.

    rpfac = 4.3D0 - 0.6D0*rmajor/rminor
    rplas = rplas * rpfac

    !  Check to see if plasma resistance is negative
    !  (possible if aspect ratio is too high)

    if (rplas <= 0.0D0) then
       fdiags(1) = rplas ; fdiags(2) = aspect
       call report_error(83)
    end if

    !  Ohmic heating power per unit volume
    !  Corrected from: pohmpv = (facoh*plascur)**2 * ...

    pohmpv = facoh * plascur**2 * rplas * 1.0D-6/vol

    !  Total ohmic heating power

    pohmmw = pohmpv*vol

  end subroutine pohm

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!																	  

  subroutine igmarcal(outfile)

    !+ad_name  igmarcal
    !+ad_summ  Routine to calculate ignition margin
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile   : input integer : Fortran output unit identifier
    !+ad_desc  This routine calculates the ignition margin at the final point
    !+ad_desc  with different scalings.
    !+ad_prob  None
    !+ad_call  fhfac
    !+ad_call  oblnkl
    !+ad_call  osubhd
    !+ad_call  pcond
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  01/04/98 PJK Modified PCOND arguments
    !+ad_hist  30/06/98 PJK Modified PCOND arguments
    !+ad_hist  19/01/99 PJK Modified PCOND arguments
    !+ad_hist  17/07/01 PJK Modified PCOND arguments
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  20/05/14 PJK Changed prad to pcorerad
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  20/10/14 PJK Output power balances for H=1 instead of H=2
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    integer :: iisc
    real(kind(1.0D0)), parameter :: d1 = 1.0D0
    real(kind(1.0D0)) :: powerhtz, ptrez, ptriz, &
         taueez, taueffz, taueiz

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'Energy confinement times, and required H-factors :')

    write(outfile,10)
10  format(t5,'scaling law', t30,'confinement time (s)', &
         t55,'H-factor for')

    write(outfile,20)
20  format(t34,'for H = 1',t54,'power balance')

    call oblnkl(outfile)

    !  Calculate power balances for all scaling laws assuming H = 1

    do iisc = 32,ipnlaws
       call pcond(afuel,palpmw,aspect,bt,dnitot,dene,dnla,eps,d1, &
            iinvqd,iisc,ignite,kappa,kappa95,kappaa,pchargemw,pinjmw, &
            plascur,pcoreradpv,rmajor,rminor,te,ten,tin,q,qstar,vol, &
            xarea,zeff,ptrez,ptriz,taueez,taueiz,taueffz,powerhtz)
       hfac(iisc) = fhfac(iisc)

       write(outfile,30) tauscl(iisc),taueez,hfac(iisc)
    end do
30  format(t2,a24,t34,f7.3,t58,f7.3)

  end subroutine igmarcal

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function fhfac(is)

    !+ad_name  fhfac
    !+ad_summ  Function to find H-factor for power balance
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  is : input integer : confinement time scaling law of interest
    !+ad_desc  This function calculates the H-factor required for power balance,
    !+ad_desc  using the given energy confinement scaling law.
    !+ad_prob  None
    !+ad_call  fhz
    !+ad_call  zeroin
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: fhfac

    !  Arguments

    integer, intent(in) :: is

    !  Local variables

    real(kind(1.0D0)) :: abserr = 0.003D0  !  numerical tolerance
    real(kind(1.0D0)) :: xlow = 0.01D0     !  minimum bound on H-factor
    real(kind(1.0D0)) :: xhigh = 100.0D0   !  maximum bound on H-factor

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    iscz = is

    !  Find value of H-factor for which function FHZ is zero
    !  (this occurs at power balance)

    fhfac = zeroin(xlow,xhigh,fhz,abserr)

  end function fhfac

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function fhz(hhh)

    !+ad_name  fhz
    !+ad_summ  Function used to find power balance
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  hhh : input real : test value for confinement time H-factor
    !+ad_desc  This function is used to find power balance.
    !+ad_desc  <CODE>FHZ</CODE> is zero at power balance, which is achieved
    !+ad_desc  using routine <A HREF="zeroin.html">ZEROIN</A> to adjust the
    !+ad_desc  value of <CODE>hhh</CODE>, the confinement time H-factor.
    !+ad_prob  None
    !+ad_call  pcond
    !+ad_hist  21/06/94 PJK Upgrade to higher standard of coding
    !+ad_hist  01/04/98 PJK Modified PCOND arguments, and adding coding for
    !+ad_hisc               use of IGNITE
    !+ad_hist  30/06/98 PJK Modified PCOND arguments
    !+ad_hist  19/01/99 PJK Modified PCOND arguments
    !+ad_hist  16/07/01 PJK Modified PCOND arguments
    !+ad_hist  10/11/11 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  20/05/14 PJK Changed prad to pcorerad; introduced iradloss;
    !+ad_hisc               Added falpha multiplier to palp term
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  13/11/14 PJK Modified iradloss usage
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: fhz

    !  Arguments

    real(kind(1.0D0)), intent(in) :: hhh

    !  Local variables

    real(kind(1.0D0)) :: powerhtz,ptrez,ptriz,taueezz,taueiz,taueffz

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call pcond(afuel,palpmw,aspect,bt,dnitot,dene,dnla,eps,hhh, &
         iinvqd,iscz,ignite,kappa,kappa95,kappaa,pchargemw,pinjmw, &
         plascur,pcoreradpv,rmajor,rminor,te,ten,tin,q,qstar,vol, &
         xarea,zeff,ptrez,ptriz,taueezz,taueiz,taueffz,powerhtz)

    ! MDK All the scaling laws now contain hfact, so this code no longer required.
    !if (iscz < 3) then  !  only laws 1 and 2 are affected???
    !   ptrez = ptrez/hhh
    !   ptriz = ptriz/hhh
    !end if

    !  At power balance, fhz is zero.

    fhz = ptrez + ptriz - falpha*palppv - pchargepv - pohmpv

    !  Take into account whether injected power is included in tau_e
    !  calculation (i.e. whether device is ignited)

    if (ignite == 0) fhz = fhz - pinjmw/vol

    !  Include the radiation power if requested

    if (iradloss == 0) then
       fhz = fhz + pradpv
    else if (iradloss == 1) then
       fhz = fhz + pcoreradpv
    else
       continue
    end if

  end function fhz

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!																  

  subroutine outplas(outfile)

    !+ad_name  outplas
    !+ad_summ  Subroutine to output the plasma physics information
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_desc  This routine writes the plasma physics information
    !+ad_desc  to a file, in a tidy format.
    !+ad_prob  None
    !+ad_call  int_to_string2
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_call  ovarrf
    !+ad_call  ovarst
    !+ad_call  report_error
    !+ad_hist  17/09/97 PJK Upgrade to higher standard of coding. Added
    !+ad_hisc               Greenwald density limit
    !+ad_hist  17/11/97 PJK Added additional beta diagnostics
    !+ad_hist  01/04/98 PJK Added dnla to output, and comment about ignition
    !+ad_hist  17/07/98 PJK Added power threshold scalings
    !+ad_hist  19/01/99 PJK Added powerht and minor word changes
    !+ad_hist  16/07/01 PJK Added kappaa
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  17/12/12 PJK Added ZFEAR lines
    !+ad_hist  18/12/12 PJK Added PTHRMW(6 to 8)
    !+ad_hist  03/01/13 PJK Removed ICULDL if-statement
    !+ad_hist  23/01/13 PJK Modified logic for stellarators and ignite=1
    !+ad_hist  10/06/13 PJK Added ISHAPE=2 and other outputs
    !+ad_hist  12/06/13 PJK Added plasma energy and other outputs
    !+ad_hist  12/08/13 PJK Removed some stellarator-irrelevant outputs
    !+ad_hist  30/09/13 PJK Added Psep/R output line
    !+ad_hist  02/10/13 PJK Changed pcharge output description
    !+ad_hist  14/11/13 PJK Corrected thermal energy outputs by 3/2
    !+ad_hist  14/11/13 PJK Changed kappa95 output description
    !+ad_hist  26/11/13 PJK Added taup/taueff ratio to output
    !+ad_hist  28/11/13 PJK Added fuel-ion pair fusion power contributions to output
    !+ad_hist  28/11/13 PJK Added icurr, iprofile information to output
    !+ad_hist  20/02/14 PJK Added pedestal profile quantities
    !+ad_hist  05/03/14 PJK Added on-axis values
    !+ad_hist  06/03/14 PJK Added warning if pdivt=0.001;
    !+ad_hisc               clarified ishape effects on kappa, triang
    !+ad_hist  26/03/14 PJK Added all bootstrap current estimations
    !+ad_hist  02/04/14 PJK Added confinement scaling law name to mfile
    !+ad_hist  03/04/14 PJK Used ovarst to write out confinement scaling law name
    !+ad_hist  23/04/14 PJK Added bvert
    !+ad_hist  14/05/14 PJK Added impurity concentration info
    !+ad_hist  21/05/14 PJK Added ignite
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  02/06/14 PJK Added fimpvar
    !+ad_hist  05/06/14 PJK Rearranged power balance output
    !+ad_hist  16/06/14 PJK Removed duplicate outputs
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  19/08/14 PJK Added dnla / Greenwald ratio
    !+ad_hist  01/10/14 PJK Modified safety factor output statements
    !+ad_hist  01/10/14 PJK Added plhthresh output
    !+ad_hist  06/10/14 PJK Modified plhthresh output
    !+ad_hist  11/11/14 PJK Added aion output
    !+ad_hist  13/11/14 PJK Modified elong, triang outputs with ishape
    !+ad_hist  13/11/14 PJK Modified iradloss usage
    !+ad_hist  17/11/14 PJK Modified output to account for falpha, palpfwmw
    !+ad_hist  18/11/14 PJK Corrected power balance output if ignite=1
    !+ad_hist  01/04/15 JM  Core plasma power balance removed
    !+ad_hist  05/08/15 MDK Output to say which impurity (if any) is an iteration variable.
    !+ad_hist  02/05/18 SIM Added pthrmw(9-14) and associated error warnings
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    real(kind(1.0D0)) :: betath
    ! pinj
    integer :: imp
    character(len=30) :: tauelaw
    character(len=30) :: str1,str2
    real(kind(1.0D0)) :: fgwped_out ! neped/dlimit(7)
    real(kind(1.0D0)) :: fgwsep_out ! nesep/dlimit(7)

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'Plasma')

    if (istell == 0) then
       select case (idivrt)
       case (0)
          call ocmmnt(outfile,'Plasma configuration = limiter')
       case (1)
          call ocmmnt(outfile,'Plasma configuration = single null divertor')
       case (2)
          call ocmmnt(outfile,'Plasma configuration = double null divertor')
       case default
          idiags(1) = idivrt ; call report_error(85)
       end select
    else
       call ocmmnt(outfile,'Plasma configuration = stellarator')
    end if

    call osubhd(outfile,'Plasma Geometry :')
    call ovarrf(outfile,'Major radius (m)','(rmajor)',rmajor)
    call ovarrf(outfile,'Minor radius (m)','(rminor)',rminor, 'OP ')
    call ovarrf(outfile,'Aspect ratio','(aspect)',aspect)

    if (istell == 0) then

       select case (ishape)
       case (0)
          call ovarrf(outfile,'Elongation, X-point (input value used)', '(kappa)',kappa)
       case (1)
          call ovarrf(outfile,'Elongation, X-point (TART scaling)', '(kappa)',kappa, 'OP ')
       case (2,3)
          call ovarrf(outfile,'Elongation, X-point (Zohm scaling)', '(kappa)',kappa, 'OP ')
          call ovarrf(outfile,'Zohm scaling adjustment factor', '(fkzohm)',fkzohm)
       case (4)
          call ovarrf(outfile,'Elongation, X-point (calculated from kappa95)', '(kappa)',kappa, 'OP ')
       case default
          idiags(1) = ishape ; call report_error(86)
       end select

       select case (ishape)
       case (4)
          call ovarrf(outfile,'Elongation, 95% surface (input value used)', &
               '(kappa95)',kappa95)
       case default
          call ovarrf(outfile,'Elongation, 95% surface (kappa/1.12)', &
               '(kappa95)',kappa95, 'OP ')
       end select

       call ovarrf(outfile,'Elongation, area ratio calc.','(kappaa)',kappaa, 'OP ')

       select case (ishape)
       case (0,2)
          call ovarrf(outfile,'Triangularity, X-point (input value used)', &
               '(triang)',triang)
       case (1)
          call ovarrf(outfile,'Triangularity, X-point (TART scaling)', &
               '(triang)',triang, 'OP ')
       case (3,4)
          call ovarrf(outfile,'Triangularity, X-point (calculated from triang95)', &
               '(triang)',triang, 'OP ')
       end select

       select case (ishape)
       case (3,4)
          call ovarrf(outfile,'Triangularity, 95% surface (input value used)', &
               '(triang95)',triang95)
       case default
          call ovarrf(outfile,'Triangularity, 95% surface (triang/1.5)', &
               '(triang95)',triang95, 'OP ')
       end select

       call ovarrf(outfile,'Plasma poloidal perimeter (m)','(pperim)',pperim, 'OP ')

    end if

    call ovarrf(outfile,'Plasma cross-sectional area (m2)','(xarea)',xarea, 'OP ')
    call ovarre(outfile,'Plasma surface area (m2)','(sarea)',sarea, 'OP ')
    call ovarre(outfile,'Plasma volume (m3)','(vol)',vol, 'OP ')

    call osubhd(outfile,'Current and Field :')

    if (istell == 0) then
       if (iprofile == 0) then
          call ocmmnt(outfile, &
               'Consistency between q0,q,alphaj,rli,dnbeta is not enforced')
       else
          call ocmmnt(outfile, &
               'Consistency between q0,q,alphaj,rli,dnbeta is enforced')
       end if
       call oblnkl(outfile)
       call ovarin(outfile,'Plasma current scaling law used','(icurr)',icurr)
    end if

    if (istell == 0) then
       call ovarrf(outfile,'Plasma current (MA)','(plascur/1D6)',plascur/1.0D6, 'OP ')
       if (iprofile == 1) then
            call ovarrf(outfile,'Current density profile factor','(alphaj)',alphaj, 'OP ')
       else
            call ovarrf(outfile,'Current density profile factor','(alphaj)',alphaj)
       end if

       call ovarrf(outfile,'Plasma internal inductance, li','(rli)',rli, 'OP ')
       call ovarrf(outfile,'Vertical field at plasma (T)','(bvert)',bvert, 'OP ')
    end if

    call ovarrf(outfile,'Vacuum toroidal field at R (T)','(bt)',bt)
    call ovarrf(outfile,'Average poloidal field (T)','(bp)',bp, 'OP ')

    call ovarrf(outfile,'Total field (sqrt(bp^2 + bt^2)) (T)','(btot)',btot, 'OP ')

    if (istell == 0) then
       call ovarrf(outfile,'Safety factor on axis','(q0)',q0)

       if (icurr == 2) then
          call ovarrf(outfile,'Mean edge safety factor','(q)',q)
       end if

       call ovarrf(outfile,'Safety factor at 95% flux surface','(q95)',q95)

       call ovarrf(outfile,'Cylindrical safety factor (qcyl)','(qstar)',qstar, 'OP ')

       if (ishape == 1) then
          call ovarrf(outfile,'Lower limit for edge safety factor q', '(qlim)',qlim, 'OP ')
       end if
    else
       call ovarrf(outfile,'Rotational transform','(iotabar)',iotabar)
    end if

    call osubhd(outfile,'Beta Information :')

    betath = beta-betaft-betanb
    gammaft = (betaft + betanb)/betath
    if (ipedestal == 3) then
       call ovarre(outfile,'Total plasma beta','(beta)',beta, 'OP ')
    else
       call ovarre(outfile,'Total plasma beta','(beta)',beta)     
    endif
    call ovarre(outfile,'Total poloidal beta','(betap)',betap, 'OP ')
    call ovarre(outfile,'Total toroidal beta',' ',beta*(btot/bt)**2, 'OP ')
    call ovarre(outfile,'Fast alpha beta','(betaft)',betaft, 'OP ')
    call ovarre(outfile,'Beam ion beta','(betanb)',betanb, 'OP ')
    call ovarre(outfile,'(Fast alpha + beam beta)/(thermal beta)','(gammaft)',gammaft, 'OP ')

    call ovarre(outfile,'Thermal beta',' ',betath, 'OP ')
    call ovarre(outfile,'Thermal poloidal beta',' ',betath*(btot/bp)**2, 'OP ')
    call ovarre(outfile,'Thermal toroidal beta (= beta-exp)',' ', betath*(btot/bt)**2, 'OP ')

    call ovarrf(outfile,'2nd stability beta : beta_p / (R/a)', '(eps*betap)',eps*betap, 'OP ')
    call ovarrf(outfile,'2nd stability beta upper limit','(epbetmax)', epbetmax)

    if (istell == 0) then
       if (iprofile == 1) then
            call ovarrf(outfile,'Beta g coefficient','(dnbeta)',dnbeta, 'OP ')
       else
            call ovarrf(outfile,'Beta g coefficient','(dnbeta)',dnbeta)
       end if

       call ovarrf(outfile,'Normalised thermal beta',' ',1.0D8*betath*rminor*bt/plascur, 'OP ')
       !call ovarrf(outfile,'Normalised total beta',' ',1.0D8*beta*rminor*bt/plascur, 'OP ')
       call ovarrf(outfile,'Normalised total beta',' ',normalised_total_beta, 'OP ')
    end if

    if (itart == 1) then
       call ovarrf(outfile,'Normalised thermal toroidal beta', ' ',fbetatry*dnbeta*btot**2/bt**2, 'OP ')
    end if

    if (iculbl == 0) then
       call ovarrf(outfile,'Limit on total beta','(betalim)',betalim, 'OP ')
    else if (iculbl == 1) then
       call ovarrf(outfile,'Limit on thermal beta','(betalim)',betalim, 'OP ')
    else
       call ovarrf(outfile,'Limit on thermal + NB beta','(betalim)', betalim, 'OP ')
    end if

    call ovarre(outfile,'Plasma thermal energy (J)',' ', 1.5D0*betath*btot*btot/(2.0D0*rmu0)*vol, 'OP ')					 

	call ovarre(outfile,'Total plasma internal energy (J)','(total_plasma_internal_energy)', total_plasma_internal_energy, 'OP ')
	
    call osubhd(outfile,'Temperature and Density (volume averaged) :')
    call ovarrf(outfile,'Electron temperature (keV)','(te)',te)
    call ovarrf(outfile,'Electron temperature on axis (keV)','(te0)',te0, 'OP ')
    call ovarrf(outfile,'Ion temperature (keV)','(ti)',ti)
    call ovarrf(outfile,'Ion temperature on axis (keV)','(ti0)',ti0, 'OP ')
    call ovarrf(outfile,'Electron temp., density weighted (keV)','(ten)',ten, 'OP ')
    call ovarre(outfile,'Electron density (/m3)','(dene)',dene)
    call ovarre(outfile,'Electron density on axis (/m3)','(ne0)',ne0, 'OP ')
    call ovarre(outfile,'Line-averaged electron density (/m3)','(dnla)',dnla, 'OP ')
    if (istell == 0) then
       call ovarre(outfile,'Line-averaged electron density / Greenwald density', &
            '(dnla_gw)',dnla/dlimit(7), 'OP ')
    end if

    call ovarre(outfile,'Ion density (/m3)','(dnitot)',dnitot, 'OP ')
    call ovarre(outfile,'Fuel density (/m3)','(deni)',deni, 'OP ')
    call ovarre(outfile,'High Z impurity density (/m3)','(dnz)',dnz, 'OP ')
    call ovarre(outfile,'Helium ion density (thermalised ions only) (/m3)','(dnalp)',dnalp, 'OP ')
    call ovarre(outfile,'Proton density (/m3)','(dnprot)',dnprot, 'OP ')
    if(protium > 1.0d-10)then
        call ovarre(outfile,'Seeded protium density / electron density','(protium)',protium)
    end if

    call ovarre(outfile,'Hot beam density (/m3)','(dnbeam)',dnbeam, 'OP ')
    call ovarre(outfile,'Density limit from scaling (/m3)','(dnelimt)',dnelimt, 'OP ')
    if ((ioptimz > 0).and.(active_constraints(5))) then
        call ovarre(outfile,'Density limit (enforced) (/m3)','(boundu(9)*dnelimt)',boundu(9)*dnelimt, 'OP ')
    end if
    call ovarre(outfile,'Helium ion density (thermalised ions only) / electron density','(ralpne)',ralpne)
    call oblnkl(outfile)

    call ovarin(outfile,'Plasma impurity model','(imprad_model)',imprad_model)
    if (imprad_model == 0) then
       call ocmmnt(outfile,'Original model; ITER 1989 Bremsstrahlung calculation')
       call ovarre(outfile,'Carbon impurity concentration (%)','(rncne*100)',rncne*100)
       call ovarre(outfile,'Oxygen impurity concentration (%)','(rnone*100)',rnone*100)

       if (zfear == 1) then
          call ovarre(outfile,'Argon impurity concentration (%)','(cfe0*100)',cfe0*100)
       else
          call ovarre(outfile,'Iron impurity concentration (%)','(cfe0*100)',cfe0*100)
       end if
    else
       call ocmmnt(outfile,'New generalised impurity model')
       call oblnkl(outfile)
       call ocmmnt(outfile,'Plasma ion densities / electron density:')
       do imp = 1,nimp
          ! MDK Update fimp, as this will make the ITV output work correctly.
          fimp(imp)=impurity_arr(imp)%frac
          str1 = impurity_arr(imp)%label // ' concentration'
          str2 = '(fimp('//int_to_string2(imp)//')'
          ! MDK Add output flag for H which is calculated
          if (imp==1) then
            !call ovarre(outfile,str1,str2,impurity_arr(imp)%frac, 'OP ')
            call ovarre(outfile,str1,str2,fimp(imp), 'OP ')
          else
            call ovarre(outfile,str1,str2,fimp(imp))
          end if
       end do
    end if
    call ovarre(outfile,'Average mass of all ions (amu)','(aion)',aion, 'OP ')
    ! MDK Say which impurity is varied, if iteration variable fimpvar (102) is turned on
    !if (any(ixc == 102)) then
    !    call ovarst(outfile,'Impurity used as an iteration variable' , '', '"' // impurity_arr(impvar)%label // '"')
    !    call ovarre(outfile,'Fractional density of variable impurity (ion / electron density)','(fimpvar)',fimpvar)
    !end if
    call oblnkl(outfile)
    if (ipedestal==3) then
       call ocmmnt(outfile, 'PLASMOD does not calculate a temperature dependent Zeff and zeffai!')
    endif
    call ovarrf(outfile,'Effective charge','(zeff)',zeff, 'OP ')

    ! Issue #487.  No idea what zeffai is.
    ! I haven't removed it as it is used in subroutine rether,
    !   (routine to find the equilibration power between the ions and electrons)
    ! call ovarrf(outfile,'Mass weighted effective charge','(zeffai)',zeffai, 'OP ')

    call ovarrf(outfile,'Density profile factor','(alphan)',alphan)
    call ovarin(outfile,'Plasma profile model','(ipedestal)',ipedestal)

    if(ipedestal.ge.1)then
        call ocmmnt(outfile,'Pedestal profiles are used.')
        call ovarrf(outfile,'Density pedestal r/a location','(rhopedn)',rhopedn)
        if(fgwped >= 0d0)then
            call ovarre(outfile,'Electron density pedestal height (/m3)','(neped)',neped, 'OP ')
        else
            call ovarre(outfile,'Electron density pedestal height (/m3)','(neped)',neped)
        end if

        ! This code is ODD! Don't change it! No explanation why fgwped and fgwsep
        ! must be assigned to their exisiting values!
        fgwped_out = neped/dlimit(7)
        fgwsep_out = nesep/dlimit(7)
        if(fgwped >= 0d0) fgwped = neped/dlimit(7)
        if(fgwsep >= 0d0) fgwsep = nesep/dlimit(7)

        call ovarre(outfile,'Electron density at pedestal / nGW','(fgwped_out)',fgwped_out)
        call ovarrf(outfile,'Temperature pedestal r/a location','(rhopedt)',rhopedt)
        ! Issue #413 Pedestal scaling
        call ovarin(outfile,'Pedestal scaling switch','(ieped)',ieped)
        if(ieped==1)then
            call ocmmnt(outfile,'Saarelma 6-parameter pedestal temperature scaling is ON')

            if(eped_warning() /= '')then
                call ocmmnt(outfile,'WARNING: Pedestal parameters are outside the range of applicability of the scaling:')
                call ocmmnt(outfile,'triang: 0.4 - 0.6; kappa: 1.5 - 2.0;   plascur: 10 - 20 MA, rmajor: 7 - 11 m;')
                call ocmmnt(outfile,'rminor: 2 - 3.5 m; tesep: 0 - 0.5 keV; normalised_total_beta: 2 - 3; ')
                write(*,*)'WARNING: Pedestal parameters are outside the range of applicability of the scaling:'
                write(*,*)'triang: 0.4 - 0.6; kappa: 1.5 - 2.0;   plascur: 10 - 20 MA, rmajor: 7 - 11 m;'
                write(*,*)'rminor: 2 - 3.5 m; tesep: 0 - 0.5 keV; normalised_total_beta: 2 - 3'
                write(*,*)trim(eped_warning())
            endif
        endif
        call ovarrf(outfile,'Electron temp. pedestal height (keV)','(teped)',teped)
        call ovarrf(outfile,'Electron temp. at separatrix (keV)','(tesep)',tesep)
        call ovarre(outfile,'Electron density at separatrix (/m3)','(nesep)',nesep)
        call ovarre(outfile,'Electron density at separatrix / nGW','(fgwsep_out)',fgwsep_out)																		   
																							 
    endif

    ! Issue 558 - addition of constraint 76 to limit the value of nesep, in proportion with the ballooning parameter and Greenwald density
    if(any(icc==76))then
       call ovarre(outfile,'Critical ballooning parameter value','(alpha_crit)',alpha_crit)
       call ovarre(outfile,'Critical electron density at separatrix (/m3)','(nesep_crit)',nesep_crit)
    endif

    call ovarrf(outfile,'Temperature profile index','(alphat)',alphat)
    call ovarrf(outfile,'Temperature profile index beta','(tbeta)',tbeta)

    if (istell == 0) then
       call osubhd(outfile,'Density Limit using different models :')
       call ovarre(outfile,'Old ASDEX model','(dlimit(1))',dlimit(1), 'OP ')
       call ovarre(outfile,'Borrass ITER model I','(dlimit(2))',dlimit(2), 'OP ')
       call ovarre(outfile,'Borrass ITER model II','(dlimit(3))',dlimit(3), 'OP ')
       call ovarre(outfile,'JET edge radiation model','(dlimit(4))',dlimit(4), 'OP ')
       call ovarre(outfile,'JET simplified model','(dlimit(5))',dlimit(5), 'OP ')
       call ovarre(outfile,'Hugill-Murakami Mq model','(dlimit(6))',dlimit(6), 'OP ')
       call ovarre(outfile,'Greenwald model','(dlimit(7))',dlimit(7), 'OP ')
    end if

    call osubhd(outfile,'Fuel Constituents :')
    call ovarrf(outfile,'Deuterium fuel fraction','(fdeut)',fdeut)
    call ovarrf(outfile,'Tritium fuel fraction','(ftrit)',ftrit)
    if (fhe3 > 1.0D-3) call ovarrf(outfile,'3-Helium fuel fraction','(fhe3)',fhe3)

    call osubhd(outfile,'Fusion Power :')
    call ovarre(outfile,'Total fusion power (MW)','(powfmw.)',powfmw, 'OP ')
    call ovarre(outfile,' =    D-T fusion power (MW)','(pdt)',pdt, 'OP ')
    call ovarre(outfile,'  +   D-D fusion power (MW)','(pdd)',pdd, 'OP ')
    call ovarre(outfile,'  + D-He3 fusion power (MW)','(pdhe3)',pdhe3, 'OP ')
    call ovarre(outfile,'Alpha power: total (MW)','(palpmw)',palpmw, 'OP ')
    call ovarre(outfile,'Alpha power: beam-plasma (MW)','(palpnb)',palpnb, 'OP ')
    call ovarre(outfile,'Neutron power (MW)','(pneutmw)',pneutmw, 'OP ')
    call ovarre(outfile,'Charged particle power (excluding alphas) (MW)', '(pchargemw)',pchargemw, 'OP ')
    call ovarre(outfile,'Total power deposited in plasma (MW)','()',falpha*palpmw+pchargemw+pohmmw+pinjmw, 'OP ')

    call osubhd(outfile,'Radiation Power (excluding SOL):')
    if (imprad_model == 1) then
       call ovarre(outfile,'Bremsstrahlung radiation power (MW)','(pbrempv*vol)', pbrempv*vol, 'OP ')
       call ovarre(outfile,'Line radiation power (MW)','(plinepv*vol)', plinepv*vol, 'OP ')
    end if
    call ovarre(outfile,'Synchrotron radiation power (MW)','(psyncpv*vol)', psyncpv*vol, 'OP ')
    call ovarrf(outfile,'synchrotron wall reflectivity factor','(ssync)',ssync)
    if (imprad_model == 1) then
       call ovarre(outfile,"Normalised minor radius defining 'core'", '(coreradius)',coreradius)
       call ovarre(outfile,"Fraction of core radiation subtracted from P_L", &
            '(coreradiationfraction)',coreradiationfraction)
    end if
    call ovarre(outfile,'Total core radiation power (MW)', '(pcoreradmw)',pcoreradmw, 'OP ')
    call ovarre(outfile,'Edge radiation power (MW)','(pedgeradmw)', pedgeradmw, 'OP ')
    call ovarre(outfile,'Total radiation power (MW)','(pradmw)',pradmw, 'OP ')
    call ovarre(outfile,'Radiation fraction = total radiation / total power deposited in plasma', &
        '(rad_fraction)', rad_fraction, 'OP ')
    call ovarre(outfile,'Nominal mean radiation load on inside surface of reactor (MW/m2)', &
        '(photon_wall)', photon_wall, 'OP ')
    call ovarre(outfile,'Peaking factor for radiation wall load', &
        '(peakfactrad)', peakfactrad, 'IP ')
    call ovarre(outfile,'Maximum permitted radiation wall load (MW/m^2)', &
        '(maxradwallload)', maxradwallload, 'IP ')
    call ovarre(outfile,'Peak radiation wall load (MW/m^2)', &
        '(peakradwallload)', peakradwallload, 'OP ')
    call ovarre(outfile,'Nominal mean neutron load on inside surface of reactor (MW/m2)', &
        '(wallmw)', wallmw, 'OP ')

    call oblnkl(outfile)
    call ovarre(outfile,'Ohmic heating power (MW)','(pohmmw)',pohmmw, 'OP ')
    call ovarrf(outfile,'Fraction of alpha power deposited in plasma','(falpha)',falpha, 'OP ')
    call ovarrf(outfile,'Fraction of alpha power to electrons','(falpe)',falpe, 'OP ')
    call ovarrf(outfile,'Fraction of alpha power to ions','(falpi)',falpi, 'OP ')
    call ovarre(outfile,'Ion transport (MW)','(ptrimw)',ptrimw, 'OP ')
    call ovarre(outfile,'Electron transport (MW)','(ptremw)',ptremw, 'OP ')
    call ovarre(outfile,'Injection power to ions (MW)','(pinjimw)',pinjimw, 'OP ')
    call ovarre(outfile,'Injection power to electrons (MW)','(pinjemw)',pinjemw, 'OP ')
    if (ignite == 1) then
       call ocmmnt(outfile,'  (Injected power only used for start-up phase)')
    end if
    call ovarin(outfile,'Ignited plasma switch (0=not ignited, 1=ignited)', '(ignite)',ignite)

    call oblnkl(outfile)
    call ovarre(outfile,'Power into divertor zone via charged particles (MW)','(pdivt)',pdivt, 'OP ')

    if (pdivt <= 0.001D0) then
       fdiags(1) = pdivt ; call report_error(87)
       call oblnkl(outfile)
       call ocmmnt(outfile,'  BEWARE: possible problem with high radiation power')
       call ocmmnt(outfile,'          Power into divertor zone is unrealistic;')
       call ocmmnt(outfile,'          divertor calculations will be nonsense!')
       call ocmmnt(outfile,'  Set constraint 17 (Radiation fraction upper limit).')
       call oblnkl(outfile)
    end if

    call ovarre(outfile,'Psep / R ratio (MW/m)','(pdivt/rmajor)',pdivt/rmajor, 'OP ')
    call ovarre(outfile,'Psep Bt / qAR ratio (MWT/m)','(pdivtbt/qar)', ((pdivt*bt)/(q95*aspect*rmajor)), 'OP ')

    if (istell == 0) then
       call osubhd(outfile,'H-mode Power Threshold Scalings :')

       call ovarin(outfile,'L-H threshold used:', "(ilhthresh)", ilhthresh)
       call ovarre(outfile,'ITER 1996 scaling: nominal (MW)','(pthrmw(1))', pthrmw(1), 'OP ')
       call ovarre(outfile,'ITER 1996 scaling: upper bound (MW)','(pthrmw(2))', pthrmw(2), 'OP ')
       call ovarre(outfile,'ITER 1996 scaling: lower bound (MW)','(pthrmw(3))', pthrmw(3), 'OP ')
       call ovarre(outfile,'ITER 1997 scaling (1) (MW)','(pthrmw(4))',pthrmw(4), 'OP ')
       call ovarre(outfile,'ITER 1997 scaling (2) (MW)','(pthrmw(5))',pthrmw(5), 'OP ')
       call ovarre(outfile,'Martin 2008 scaling: nominal (MW)', '(pthrmw(6))',pthrmw(6), 'OP ')
       call ovarre(outfile,'Martin 2008 scaling: 95% upper bound (MW)', '(pthrmw(7))',pthrmw(7), 'OP ')
       call ovarre(outfile,'Martin 2008 scaling: 95% lower bound (MW)', '(pthrmw(8))',pthrmw(8), 'OP ')
       call ovarre(outfile,'Snipes 2000 scaling: nominal (MW)', '(pthrmw(9))',pthrmw(9), 'OP ')
       call ovarre(outfile,'Snipes 2000 scaling: upper bound (MW)', '(pthrmw(10))',pthrmw(10), 'OP ')
       call ovarre(outfile,'Snipes 2000 scaling: lower bound (MW)', '(pthrmw(11))',pthrmw(11), 'OP ')
       call ovarre(outfile,'Snipes 2000 scaling (closed divertor): nominal (MW)', '(pthrmw(12))',pthrmw(12), 'OP ')
       call ovarre(outfile,'Snipes 2000 scaling (closed divertor): upper bound (MW)', '(pthrmw(13))',pthrmw(13), 'OP ')
       call ovarre(outfile,'Snipes 2000 scaling (closed divertor): lower bound (MW)', '(pthrmw(14))',pthrmw(14), 'OP ')
       call oblnkl(outfile)
       if ((ilhthresh.eq.9).or.(ilhthresh.eq.10).or.(ilhthresh.eq.11)) then
           if ((bt < 0.78D0).or.(bt > 7.94D0)) then
               call ocmmnt(outfile,'(bt outside Snipes 2000 fitted range)')
               call report_error(201)
           end if
           if ((rminor < 0.15D0).or.(rminor > 1.15D0)) then
               call ocmmnt(outfile,'(rminor outside Snipes 2000 fitted range)')
               call report_error(202)
           end if
           if ((rmajor < 0.55D0).or.(rmajor > 3.37D0)) then
               call ocmmnt(outfile,'(rmajor outside Snipes 2000 fitted range)')
               call report_error(203)
           end if
           if ((dnla < 0.09D20).or.(dnla > 3.16D20)) then
               call ocmmnt(outfile,'(dnla outside Snipes 2000 fitted range)')
               call report_error(204)
           end if
           if ((kappa < 1.0D0).or.(kappa > 2.04D0)) then
               call ocmmnt(outfile,'(kappa outside Snipes 2000 fitted range)')
               call report_error(205)
           end if
           if ((triang < 0.07D0).or.(triang > 0.74D0)) then
               call ocmmnt(outfile,'(triang outside Snipes 2000 fitted range)')
               call report_error(206)
           end if
       call oblnkl(outfile)
       end if
       if ((ilhthresh.eq.12).or.(ilhthresh.eq.13).or.(ilhthresh.eq.14)) then
           call ocmmnt(outfile,'(L-H threshold for closed divertor only. Limited data used in Snipes fit)')
           call oblnkl(outfile)
           call report_error(207)
       end if
       if ((ioptimz > 0).and.(active_constraints(15))) then
          call ovarre(outfile,'L-H threshold power (enforced) (MW)', '(boundl(103)*plhthresh)',boundl(103)*plhthresh, 'OP ')
       else
          call ovarre(outfile,'L-H threshold power (NOT enforced) (MW)', '(plhthresh)',plhthresh, 'OP ')
       end if
    end if

    call osubhd(outfile,'Confinement :')

    if (ignite == 1) then
       call ocmmnt(outfile, &
            'Device is assumed to be ignited for the calculation of confinement time')
       call oblnkl(outfile)
    end if

    write(outfile,200) tauscl(isc)
200 format(' Confinement scaling law',T45,A24)

    if (index(tauscl(isc),'(') /= 0) then
       tauelaw = '"'//trim(tauscl(isc)(1:index(tauscl(isc),'(',.true.)-1))//'"'
    else
       tauelaw = '"'//trim(tauscl(isc))//'"'
    end if
    call ovarst(mfile,'Confinement scaling law','(tauelaw)',trim(tauelaw))

    call ovarrf(outfile,'Confinement H factor','(hfact)',hfact)
    call ovarrf(outfile,'Global energy confinement time (s)','(taueff)',taueff, 'OP ')
    call ovarrf(outfile,'Ion energy confinement time (s)','(tauei)',tauei, 'OP ')
    call ovarrf(outfile,'Electron energy confinement time (s)','(tauee)',tauee, 'OP ')
    call ovarre(outfile,'n.tau = Volume-average electron density x Energy confinement time (s/m3)', &
        '(dntau)', dntau, 'OP ')
    call ocmmnt(outfile,'Triple product = Vol-average electron density x Vol-average&
        & electron temperature x Energy confinement time:')
    call ovarre(outfile,'Triple product  (keV s/m3)','(dntau*te)',dntau*te, 'OP ')
    call ovarre(outfile,'Transport loss power assumed in scaling law (MW)', '(powerht)',powerht, 'OP ')
    call ovarin(outfile,'Switch for radiation loss term usage in power balance', '(iradloss)',iradloss)
    if (iradloss == 0) then
       call ovarre(outfile,'Radiation power subtracted from plasma power balance (MW)', '',pradmw, 'OP ')
       call ocmmnt(outfile,'  (Radiation correction is total radiation power)')
    else if (iradloss == 1) then
       call ovarre(outfile,'Radiation power subtracted from plasma power balance (MW)', '',pcoreradmw, 'OP ')
       call ocmmnt(outfile,'  (Radiation correction is core radiation power)')
    else
       call ovarre(outfile,'Radiation power subtracted from plasma power balance (MW)', '',0.0D0)
       call ocmmnt(outfile,'  (No radiation correction applied)')
    end if
    call ovarrf(outfile,'Alpha particle confinement time (s)','(taup)',taup, 'OP ')
    ! Note alpha confinement time is no longer equal to fuel particle confinement time.
    call ovarrf(outfile,'Alpha particle/energy confinement time ratio','(taup/taueff)',taup/taueff, 'OP ')
    call ovarrf(outfile,'Lower limit on taup/taueff','(taulimit)',taulimit)

    call ovarrf(outfile,'Total energy confinement time including radiation loss (s)', &
                    '(total_energy_conf_time)', total_energy_conf_time, 'OP ')																							 

    if (istell == 0) then
       call osubhd(outfile,'Plasma Volt-second Requirements :')
       call ovarre(outfile,'Total volt-second requirement (Wb)','(vsstt)',vsstt, 'OP ')
       call ovarre(outfile,'Inductive volt-seconds (Wb)','(vsind)',vsind, 'OP ')
       call ovarrf(outfile,'Ejima coefficient','(gamma)',gamma)
       call ovarre(outfile,'Start-up resistive (Wb)','(vsres)',vsres, 'OP ')
       call ovarre(outfile,'Flat-top resistive (Wb)','(vsbrn)',vsbrn, 'OP ')

       call ovarrf(outfile,'bootstrap current fraction multiplier', '(cboot)',cboot)
       call ovarrf(outfile,'Bootstrap fraction (ITER 1989)', '(bscf_iter89)',bscf_iter89, 'OP ')
																								   
																								   
       call ovarrf(outfile,'Bootstrap fraction (Sauter et al)', '(bscf_sauter)',bscf_sauter, 'OP ')
       
       if (ipedestal==3) then
          call ocmmnt(outfile,'if ipedestal==3, bscf_nevins and bscf_wilson are meaningless')
          call ocmmnt(outfile,'(PLASMOD bootstrap current fraction used)')
       else
          call ovarrf(outfile,'Bootstrap fraction (Nevins et al)', '(bscf_nevins)',bscf_nevins, 'OP ')
          call ovarrf(outfile,'Bootstrap fraction (Wilson et al)', '(bscf_wilson)',bscf_wilson, 'OP ')
          if (bscfmax < 0.0D0) then
             call ocmmnt(outfile,'  (User-specified bootstrap current fraction used)')
          else if (ibss == 1) then
             call ocmmnt(outfile,'  (ITER 1989 bootstrap current fraction model used)')
          else if (ibss == 2) then
             call ocmmnt(outfile,'  (Nevins et al bootstrap current fraction model used)')
          else if (ibss == 3) then
             call ocmmnt(outfile,'  (Wilson et al bootstrap current fraction model used)')
          else if (ibss == 4) then
             call ocmmnt(outfile,'  (Sauter et al bootstrap current fraction model used)')
          end if
       endif																					   
			 
       call ovarrf(outfile,'Bootstrap fraction (enforced)','(bootipf.)',bootipf, 'OP ')

       call ovarre(outfile,'Loop voltage during burn (V)','(vburn)', plascur*rplas*facoh, 'OP ')
       call ovarre(outfile,'Plasma resistance (ohm)','(rplas)',rplas, 'OP ')

       call ovarre(outfile,'Resistive diffusion time (s)','(res_time)',res_time, 'OP ')
       call ovarre(outfile,'Plasma inductance (H)','(rlp)',rlp, 'OP ')
       call ovarrf(outfile,'Coefficient for sawtooth effects on burn V-s requirement','(csawth)',csawth)
    end if

    call osubhd(outfile,'Fuelling :')
    call ovarre(outfile,'Ratio of He and pellet particle confinement times','(tauratio)',tauratio)
    call ovarre(outfile,'Fuelling rate (nucleus-pairs/s)','(qfuel)',qfuel, 'OP ')
    call ovarre(outfile,'Fuel burn-up rate (reactions/s)','(rndfuel)',rndfuel, 'OP ')
    call ovarrf(outfile,'Burn-up fraction','(burnup)',burnup, 'OP ')

  end subroutine outplas

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine outtim(outfile)

    !+ad_name  outtim
    !+ad_summ  Routine to print out the times of the various stages
    !+ad_summ  during a single plant cycle
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_desc  This routine writes out the times of the various stages
    !+ad_desc  during a single plant cycle.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  oheadr
    !+ad_call  ovarrf
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  27/06/13 PJK Relabelled tohs
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  12/11/14 PJK tcycle now a global variable
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'Times')

    call ovarrf(outfile,'Initial charge time for CS from zero current (s)','(tramp)', tramp)
    call ovarrf(outfile,'Plasma current ramp-up time (s)','(tohs)',tohs)
    call ovarrf(outfile,'Heating time (s)','(theat)',theat)
    call ovarre(outfile,'Burn time (s)','(tburn)',tburn, 'OP ')
    call ovarrf(outfile,'Reset time to zero current for CS (s)','(tqnch)',tqnch)
    call ovarrf(outfile,'Time between pulses (s)','(tdwell)',tdwell)
    call oblnkl(outfile)
    !call ovarre(outfile,'Pulse time (s)','(tpulse)',tpulse, 'OP ')
    !call ovarrf(outfile,'Down time (s)','(tdown)',tdown, 'OP ')
    call ovarre(outfile,'Total plant cycle time (s)','(tcycle)',tcycle, 'OP ')

  end subroutine outtim

end module physics_module
