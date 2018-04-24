!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module plasmod_module

  
  !+ad_name  plasmod
  !+ad_summ  Module containing plasmod interface
  !+ad_type  Module
  !+ad_auth  K Ellis, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains all the interface
  !+ad_desc  functions between the 1D transport and 
  !+ad_desc  equilibrium code PLASMOD and the rest
  !+ad_desc  of PROCESS.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_kallenbach_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  global_variables
  !+ad_call  impurity_radiation_module
  !+ad_call  numerics
  !+ad_call  physics_variables
  !+ad_call  plasmod_variables
  !+ad_call  process_output
  !+ad_hist  26/02/18 KE Initial version of module
  !+ad_stat  Okay
  !+ad_docs  E Fable et al. Fus. Eng. & Des. (2018)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use constraint_variables
  use current_drive_variables
  use divertor_kallenbach_variables   !for impurity_enrichment
  use divertor_variables
  use error_handling
  use global_variables
  use impurity_radiation_module
  use numerics                        !for boundl
  use physics_variables
  use plasmod_variables
  use process_output
  
  
  implicit none

  public
  
contains

  subroutine setupPlasmod(num,geom,comp,ped,inp0,i_flag)

    !+ad_name  setupPlasmod
    !+ad_summ  Routine to set up the PLASMOD input params
    !+ad_type  Subroutine
    !+ad_auth  K Ellis, UKAEA, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  num  : derived type : numerics information
    !+ad_args  geom : derived type : geometry information 
    !+ad_args  comp : derived type : composition information
    !+ad_args  ped  : derived type :  pedestal information
    !+ad_args  inp0 : derived type : miscellaneous input information
    !+ad_args  i_flag : integer    : PLASMOD error flag 
    !+ad_desc  This routine sets up the input parameters for
    !+ad_desc  PLASMOD from PROCESS variables.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_call  element2index
    !+ad_hist  26/02/18 KE Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  E Fable et al. Fus. Eng. & Des. (2018)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
 
    type (geometry), intent(inout) :: geom
    type (composition), intent(inout) :: comp
    type (pedestal), intent(inout) :: ped
    type (inputs), intent(inout) :: inp0
    type (numerics_transp), intent(inout) :: num
    integer, intent(inout) :: i_flag


    ! Only variables that can be iteration variables or those that
    ! are calculated inside PROCESS need to be put here:

    i_flag = 1

    geom%r   = rmajor
    geom%a   = aspect
    geom%q95 = q95
    geom%bt  = bt

    inp0%f_gw      = fgwped !pedestal top greenwald fraction
    inp0%Hfac_inp  = hfact !input H factor (radiation corrected), if 0., this is not used.
    inp0%pheatmax  = pinjalw !max allowed power for heating+CD+fusion control
    inp0%q_control = pheat !minimal power required for control 

    !fvsbrnni can be an iteration variable!
    inp0%f_ni   = fvsbrnni !required fraction of non inductive current, if 0 dont use CD

    !Note that this is only a correct input on the second iteration!
    inp0%fpion = fpion ! Fraction of neutral beam energy to ions

    if (comp%qdivt.eq.0.d0) then
       comp%comparray(9) = impurity_arr(element2index('Ar'))%frac !argon concentration, uses Kallenbach model if qdivt = 0. from PLASMOD inputs
       !else
       !@EF: What should happen, if this is not assigned?
    endif
    
    !uses PROCES defined LH threshold, if this is > 0
    inp0%PLH = 0d0 !plhthresh ! This won't work as this can only be calculated after.
    !KE - should inp0%PLH have the possibility to be an input variable?
   
							
    inp0%nbcdeff = gamcd ! normalised current drive efficiency (1.0e20 A/W-m2) 

    ! all fixed input variables that cannot change within a PROCESS
    ! iteration go here!
    ! They only need to be initialised once.
    if (geom%counter.eq.0.d0) then


       !HL: This is a temporary set up for the moment!
       comp%psep_r      = pseprmax !Psep/R max value
       comp%psepb_q95AR = psepbqarmax !Psep B/qAR max value

       
       ! To selfconsistently compute the He concentration inside PLASMOD
       ! its intial fraction has to be 0.d0. Then globtau is used!
       ! The Xe fraction is used as an iteration variable inside PLASMOD
       ! it adjusts to fulfil psepqbarmax, pseprmax or psepplh_sup.
       comp%comparray = 0.d0 !array of impurities !HL: This overwrites Argon setting!!!
       comp%protium   = protium !protium is treated separately
       
       comp%psepplh_inf = boundl(103) !Psep/PLH if below this, use nbi      
       comp%psepplh_sup = plasmod_psepplh_sup !Psep/PLH if above this, use Xe
       inp0%maxpauxor   = plasmod_maxpauxor ! maximum Paux/R allowed

       num%tol    = plasmod_tol !tolerance to be reached, in % variation at each time step
       num%dtmin  = plasmod_dtmin !min time step
       num%dtmax  = plasmod_dtmax !max time step
       num%dt     = plasmod_dt !time step
       num%dtinc  = plasmod_dtinc !decrease of dt
       num%Ainc   = plasmod_ainc !increase of dt
       num%test   = plasmod_test !max iteration number
       num%tolmin = plasmod_tolmin ! multiplier of etolm that should not be overcome

       num%eopt     = plasmod_eopt !exponent of jipperdo
       num%dtmaxmin = plasmod_dtmaxmin !exponent of jipperdo2
       num%capA     = plasmod_capa !first radial grid point
       num%maxA     = plasmod_maxa !diagz 0 or 1
       num%dgy      = plasmod_dgy !Newton differential
       num%i_modeltype = plasmod_i_modeltype !1 - simple gyrobohm scaling with imposed H factor > 1, other models with H in output
       num%i_equiltype = plasmod_i_equiltype !1 - EMEQ, solve equilibrium
       !with given q95, with sawteeth. 2- EMEQ, solve with given Ip, with sawteeth.
       num%nx  = plasmod_nx  !number of interpolated grid points
       num%nxt = plasmod_nxt !number of reduced grid points
       num%nchannels = plasmod_nchannels  !leave this at 3

       if(ieped == 0) then
          num%ipedestal= 1  !fixed temperature pedestal
       else if (ieped == 1) then
          num%ipedestal= 2  !Sareelma scaling
       else
          call report_error(175) !option not possible
       endif

       num%i_impmodel = plasmod_i_impmodel !impurity model: 0 - fixed
       !concentration, 1 - concentration fixed at pedestal top, then fixed density. 
       comp%globtau(1) = plasmod_globtau(1) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(2) = plasmod_globtau(2) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(3) = plasmod_globtau(3) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(4) = plasmod_globtau(4) !tauparticle/tauE for D, T, He, Xe, Ar Not used for Xe!
       comp%globtau(5) = plasmod_globtau(5) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%fuelmix = fdeut !fuel mix
       
       !compression factor between div and
       !core: e.g. 10 means there is 10 more Argon concentration in the
       !divertor than in the core
       comp%c_car = impurity_enrichment(element2index('Ar')) 


       !derivatives
       inp0%qnbi_psepfac = plasmod_qnbi_psepfac !dqnbi/d(1-Psep/PLH)
       inp0%cxe_psepfac  = plasmod_cxe_psepfac !dcxe/d(1-Psep/PLH)
       inp0%car_qdivt    = plasmod_car_qdivt !dcar/d(qdivt)

       !deposition locations
       inp0%x_heat(1)     = plasmod_x_heat(1) !nbi
       inp0%x_heat(2)     = plasmod_x_heat(2) !ech
       inp0%x_cd(1)       = plasmod_x_cd(1) !nbi
       inp0%x_cd(2)       = plasmod_x_cd(2) !ech
       inp0%x_fus(1)      = plasmod_x_fus(1) !nbi
       inp0%x_fus(2)      = plasmod_x_fus(2) !ech
       inp0%x_control(1)  = plasmod_x_control(1) !nbi
       inp0%x_control(2)  = plasmod_x_control(2) !ech
       inp0%dx_heat(1)    = plasmod_dx_heat(1) !nbi
       inp0%dx_heat(2)    = plasmod_dx_heat(2) !ech
       inp0%dx_cd(1)      = plasmod_dx_cd(1) !nbi
       inp0%dx_cd(2)      = plasmod_dx_cd(2) !ech
       inp0%dx_fus(1)     = plasmod_dx_fus(1) !nbi
       inp0%dx_fus(2)     = plasmod_dx_fus(2) !ech
       inp0%dx_control(1) = plasmod_dx_control(1) !nbi
       inp0%dx_control(2) = plasmod_dx_control(2) !ech
       inp0%nbi_energy    = plasmod_nbi_energy !in keV

       inp0%eccdeff = 0.3  !CD = this * PCD * TE/NE !not used for now
       inp0%pech    = 0.d0 !ech power !not used for now
       inp0%pnbi    = 0.d0 !nbi power
       inp0%qheat   = 0.d0 !
       inp0%qcd     = 0.d0 !
       inp0%qfus    = 0.d0 !
       inp0%spellet = 0.d0 !pellet mass in particles of D in 10^19
       inp0%fpellet = 0.5d0 !pellet frequency in Hz

       ! implemented global variables ready for use when required
       !inp0%eccdeff = plasmod_eccdeff  !CD = this * PCD * TE/NE !not used for now
       !inp0%pech    = plasmod_pech !ech power !not used for now
       !inp0%spellet = plasmod_spellet !pellet mass in particles of D in 10^19
       !inp0%fpellet = plasmod_fpellet !pellet frequency in Hz


       inp0%V_loop = plasmod_v_loop !target loop voltage. If lower than  -1.e5 dont use
       inp0%pfus   = plasmod_pfus !if 0., not used (otherwise it would be controlled with Pauxheat)
       !Only one of the two below should be specified
       inp0%contrpovs = plasmod_contrpovs !control power in Paux/lateral_area (MW/m2)
       inp0%contrpovr = plasmod_contrpovr !control power in Paux/R (MW/m)

       ! qdivt should be equal to qtargettotal /5.0e6/ if using the
       ! Kallenbach model at the same time
       ! This needs to be implemented when coupling to the Kallenbach model!
       comp%qdivt       = plasmod_qdivt !divertor heat flux in MW/m^2, if 0, dont use SOL model
       comp%pradfrac    = coreradiationfraction !fraction of radiation from 'core' region that is subtracted from the loss power
       comp%pradpos     = coreradius ! position after which radiation is
       !counted 0. for tau and other global quantities, i.e. position after
       !which radiation is "edge"

       
       ped%tesep   = tesep  !separatrix temperature
       ped%rho_t   = rhopedt !pedestal top position T
       ped%rho_n   = rhopedn !pedestal top position n
       ped%pedscal = plasmod_pedscal !multiplies the pedestal scaling in PLASMOD
       ped%teped   = teped !pedestal top temperature
       inp0%f_gws  = fgwsep !separatrix greenwald fraction
       
       geom%k  = kappa !edge elongation
       geom%d  = triang !edge triangularity
       geom%ip = plascur/1.d6
       geom%k95 = kappa95 !edge elongation
       geom%d95 = triang95 !edge triangularity
        
    endif
    
  end subroutine setupPlasmod

  subroutine convert_Plasmod2PROCESS(geom,comp,ped,radp,mhd,loss,theat,&
		& tburn,fusrat)

    !+ad_name  convert_Plasmod2PROCESS
    !+ad_summ  Routine to set up the PLASMOD input params
    !+ad_type  Subroutine
    !+ad_auth  K Ellis, UKAEA, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  num : derived type :
    !+ad_args  geom : derived type : 
    !+ad_args  comp : derived type :
    !+ad_args  ped : derived type :
    !+ad_desc  This routine writes out the times of the various stages
    !+ad_desc  during a single plant cycle.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  28/02/18 HL Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  E Fable et al. Fus. Eng. & Des. (2018)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
    type (geometry), intent(inout) :: geom
    type (composition), intent(inout) :: comp
    type (pedestal), intent(inout) :: ped
    type (radial_profiles), intent(inout) :: radp
    type (MHD_EQ), intent(inout) :: mhd 
    type (power_losses), intent(inout) :: loss 

    real(kind(1.0D0)) :: theat,tburn,aeps,beps,rlpext,rlpint,vburn,fusrat
    real(kind(1.0D0)) :: znimp, znfuel
    real(kind(1.0D0)) :: pdtpv, betath
    integer :: imp


    if (i_flag==1)then
!       write(*,*) 'PLASMOD has converged!!!'
    elseif (i_flag==0)then
       write(*,*) 'The PLASMOD transport model has crashed'
       write(*,*) 'Possible reasons: * strange parameters - check the inputs'
       write(*,*) '* time step too large, reduce plasmod_dtmax (and/or plasmod_dtmin)'
    elseif (i_flag==-1)then
       write(*,*) 'The PLASMOD transport model has not converged after itermax'
       write(*,*) 'Possible reasons: * max iterations too low, try increasing plasmod_test'
       write(*,*) '* equilibrium oscillating between solutions --> pressure too high, reduce H factor'
       write(*,*) '* solution oscillating, reduce time step size plasmod_dtmax and/or plasmod_dtmin'
    elseif (i_flag==-2)then
       write(*,*) 'The PLASMOD equilibrium has crashed'
       write(*,*) 'Possible reasons: * overly complex q profile --> advanced scenarios not yet feasible'
       write(*,*) '* pressure too high, reduce H factor'
       write(*,*) '* negative temperatures or densities or some other strange parameters, reduce PLASMOD time step'
       write(*,*) 'N.B. If reducing time step size doesnt work, try changing the parameters, particularly H factor'
    endif

    ! mhd%equilcheck will likely be depricated in the future, as it is covered
    ! by i_flag
    if (i_flag.ne.1 .or. mhd%equilcheck.ne.1)then
       idiags(1) = i_flag
       call report_error(174)
    endif
        
    !------------------------------------------------
    !Quantities previously calculated by geomty in plasma_geometry
    !kappa95 and triang95 are inputs and not recalculated! (#646)
    kappa    = geom%k
    triang   = geom%d 
    xarea    = mhd%torsurf !Plasma cross-sectional area (m2)
    sarea    = mhd%sp
    !sareao   =  !outboard plasma surface area
    kappaa   = xarea/(3.141592*rminor**2)
    pperim = geom%perim !Plasma poloidal perimeter (m)
    sf = pperim / (2.0D0*pi*rminor)    
    vol = mhd%vp ! plasma volume (m^3)
    
    !------------------------------------------------
    !Temperature outputs otherwise input or
    !calculated in plasma_profiles
    te0   = radp%te(1) 
    ti0   = radp%ti(1) !ion temperature on axis
    teped = ped%teped !only computed, if ieped = 2
    te    = radp%av_te
    ten   = radp%av_Ten
    ti    = radp%av_ti
    tin   = ti/te * ten

    !------------------------------------------------
    !Density outputs otherwise inputs or
    !calculated in plasma_profiles
    dene  = radp%av_ne*1.d19
    ne0   = radp%ne(1)*1.d19
    neped = ped%nped*1.d19
    nesep = ped%nsep*1.d19
    dnitot = radp%av_ni * 1.0d19 !Ion density (/m3)
    ni0   = dnitot/dene * ne0
    dnla  = sum(radp%ne)/size(radp%ne)*1.d19

    !  Central pressure (Pa), from ideal gas law : p = nkT
    p0 = (ne0*te0 + ni0*ti0) * 1.0D3 * echarge

    !------------------------------------------------
    !Plasma Composition outputs otherwise inputs or
    !calculated in plasma_compostion/betcom 
    !in PROCESS fimp is used for input
    !ralpne, fprot, ftrit etc. are used throughout instead impurity_arr()
    !impurity_arr is used for the composition calculations and
    !the radiation model
    
    ralpne = comp%comparray(2)
    
    dnalp = radp%av_nhe*1.d19 ! Helium ion density (thermalised ions only) (/m3)
    if ((dnalp - dene*ralpne)/dnalp > 1e-6) then
       fdiags(1) = dnalp; fdiags(2) = dene; fdiags(3) = ralpne;
       call report_error(192)
    endif
    dnprot = protium*dene ! Proton density (/m3) from seeding only, not from DD-fusion!
    
    ! Hot beam density (/m3)
    if (ignite == 0) then
       dnbeam = dene * rnbeam
    else
       dnbeam = 0.0D0
    end if

    do imp=1,nimp
       impurity_arr(imp)%frac=comp%comparray(imp)
    enddo

    if (.false.) then !This cannot be used as PLASMOD cannot vary Z with Te yet
    
       !  Sum of Zi.ni for all impurity ions (those with charge > helium)
       znimp = 0.0D0
       do imp = 1,nimp
          if (impurity_arr(imp)%Z > 2) then
             znimp = znimp + Zav_of_te(impurity_arr(imp),te)*(impurity_arr(imp)%frac * dene)
          end if
       end do

       !  Fuel portion - conserve charge neutrality
       !  znfuel is the sum of Zi.ni for the three fuel ions
       znfuel = dene - 2.0D0*dnalp - dnprot - dnbeam - znimp
       deni   = znfuel/(1.0D0+fhe3)! Fuel density (/m3)
    
       if ((deni - radp%av_nd*1.d19)/deni > 1e-6) then
          fdiags(1) = deni; fdiags(2) = radp%av_nd*1.d19; fdiags(3) = znfuel; fdiags(4) = fhe3
          call report_error(193)
       endif

    endif
    
    deni  = radp%av_nd*1.d19 ! Fuel density (/m3)
    
    !  Ensure that deni is never negative or zero - KE: deni could be zero with this inequality
    if (deni < 0.0D0) then
       fdiags(1) = deni ; call report_error(78)
       deni = max(deni,1.0D0)
    end if

    
    !  Set hydrogen and helium impurity fractions for
    !  radiation calculations
    impurity_arr(element2index('H_'))%frac = &
         (dnprot + (fdeut+ftrit)*deni + dnbeam)/dene

    impurity_arr(element2index('He'))%frac = fhe3*deni/dene + ralpne

    if (plasmod_i_impmodel == 0 ) then
       !  Total impurity density (/m3)
       dnz = 0.0D0
       do imp = 1,nimp
          if (impurity_arr(imp)%Z > 2) then
             dnz = dnz + impurity_arr(imp)%frac*dene
          end if
       end do
       
       if ( (dnz - radp%av_nz*1.d19)/dnz > 1e-6) then
          fdiags(1) = dnz; fdiags(2) = radp%av_nz*1.d19
          call report_error(194)
       endif
    else
       dnz = radp%av_nz*1.d19
    endif

    !  Total ion density
    if ((dnitot - deni - dnalp - dnprot - dnbeam - dnz) > 1e-6) then
       fdiags(1) = dnitot; fdiags(2) = deni; fdiags(3) = dnalp
       fdiags(4) = dnprot; fdiags(5) = dnbeam; fdiags(6)= dnz
       call report_error(200)
    endif

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
    if (.false.) then !This cannot be used as PLASMOD cannot vary Z with Te yet
       zeff = 0.0D0
       do imp = 1,nimp
          zeff = zeff + impurity_arr(imp)%frac * Zav_of_te(impurity_arr(imp),te)**2
       end do
       
       if ((zeff - radp%zeff)/zeff > 1e-6) then
          fdiags(1) = zeff; fdiags(2) = radp%zeff
          call report_error(195)
       endif
    endif
    zeff = radp%zeff
    
    !  Define coulomb logarithm
    !  (collisions: ion-electron, electron-electron)

    dlamee = 31.0D0 - (log(dene)/2.0D0) + log(te*1000.0D0)
    dlamie = 31.3D0 - (log(dene)/2.0D0) + log(te*1000.0D0)
    
    falpe = loss%palpe/(loss%palpe+loss%palpi)   
    falpi = loss%palpi/(loss%palpe+loss%palpi)   
    

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
               * Zav_of_te(impurity_arr(imp),te)**2 / impurity_arr(imp)%amass
       end if
    end do

    !------------------------------------------------
    !Plasma Current outputs otherwise inputs or
    !calculated in culcur
    !if plasmod_i_equiltype = 1 q95 is an input and plascur an output
    !if plasmod_i_equiltype = 2 plascur is an input and q95 an output
    !Reassign both for simplicity
    plascur = geom%ip * 1.0D6 !Plasma current in Ampere
    !Edge safety factor
    q95 = geom%q95
    !duplicate variables that might get deprecated!
    !plascur = mhd%ip_out  * 1.0D6
    !q95 = mhd%q

    qstar = mhd%qstar ! equivalent cylindrical safety factor (shaped)
    bp    = mhd%bp ! poloidal field in (T)
    rli   = mhd%rli !plasma inductance internal (H)
    
    !------------------------------------------------
    !beta is now an output, is an input with (ipedestal .ne. 3)
    beta  = mhd%betan * geom%ip / (rminor * bt)/100.
    normalised_total_beta = mhd%betan
    
    !------------------------------------------------
    !replacing parametrised bootstrap models
    bootipf= mhd%fbs
    
    !See #645 for discussion on fvsbrnni
    fvsbrnni = mhd%f_ni
    facoh = max(1.0D-10, (1.-mhd%f_ni))
    faccd = max(0., mhd%f_ni - bootipf )

    !mhd%q_sep !q at separatrix
    !mhd%vloop !loop voltage in V ! Check this is consistent with our volt-seconds requirements routine in physics.f90

    ! This should match that input value and therefore should not need to be reassigned
    ! One could implement a reference check?

    !-----------------------------------------------
    !Fusion power and fast alpha pressure calculations
    !previously calculated by palph
    palppv     = loss%Pfus/(5.0*vol) !alpha particle fusion power per volume (MW/m3)
    pchargepv  = 0d0 !other charged particle fusion power/volume (MW/m3)
    !pneutpv calculated in palph2 section below
    !missing e.g. beam power as calculated in beamfus
    !sigvdt = 0.0d0
    fusionrate = loss%fusionrate !fusion reaction rate (reactions/m3/s)
    alpharate  = loss%alpharate !alpha particle production rate (/m3/s)
    protonrate = 0.0d0
    pdt        = loss%Pfus !D-T fusion power (MW)
    pdtpv      = pdt / vol

    !Eventually PLASMOD will generate these other interactions more completely.
    !For now, set values to zero.
    pdhe3      = 0d0 !KE !D-He3 fusion power (MW) !PLASMOD does not calc.
    pdd        = loss%pfusdd !D-D fusion power (MW)
    !pddpv      = pdd / vol !KE - not a global variable, is this required?
    
    !---------------------------------------------
    ! beam fusion components previously calculated in beamfus
    ! PLASMOD does not currently calculate beam fusion properties
    ! Todo: Add in PLASMOD or call beamfus instead!
    betanb  = 0D0 !neutral beam beta component
    dnbeam2 = 0D0 !hot beam ion density (/m3)
    palpnb  = 0D0 !alpha power from hot neutral beam ions (MW)

    !---------------------------------------------
    !previously calculated in palph2 - recalculates quantities to changes units
    !e.g. per volume to MW, and to include beam fusion, e.g:
     !neutrons
    pneutpv   = loss%Pfus/(5.0 * vol) * 4.0  !neutron fusion power (MW/m3), updated below
    pneutpv   = pneutpv + 4.0D0*palpnb/vol !updating with neutral beam power (currently zero)
    pneutmw   = pneutpv * vol !neutron fusion power including beam fusion (currently zero) (MW)
     !alphas
    falpha    =1.0
    palpepv   = loss%palpe/vol !alpha power per volume to electrons (MW/m3) 
    palpipv   = loss%palpi/vol !alpha power per volume to ions (MW/m3)
    palppv    = palppv + palpnb/vol !updating with neutral beam power (currently zero) (MW/m3)
    palpmw    = loss%Pfus/5.0 !alpha power (MW)
    betaft    = loss%betaft !fast alpha beta component
     !non-alpha charged particles
    pchargemw = 0d0 !other charged particle fusion power, excluding alphas (MW) !This comes from reactions other than DT
    pfuscmw   = loss%Pfus/5.0 !charged particle fusion power (MW) !
    !KE - not sure above is correct? Definition in PROCESS: pfuscmw = palpmw + pchargemw
    !KE - in PROCESS, below is, powfmw = palpmw + pneutmw + pchargemw
    powfmw    = pdt + pdd + pdhe3 !Same calculation as in ASTRA, complete formula with cross section, should be equivalent to PROCESS  !Total power deposited in plasma (MW)

    !---------------------------------------
    betath = beta-betaft-betanb
    gammaft = (betaft+betanb)/betath !(Fast alpha + beam beta)/(thermal beta)
    hfact  = loss%H
    pradmw     = loss%prad ! fradpwr is total radiation fraction 
    pedgeradmw = loss%pradedge    
    pcoreradmw = loss%pradcore
    psyncpv    = loss%psync/vol 
    pbrempv    = loss%pbrehms/vol
    plinepv    = loss%pline/vol

    piepv      = loss%piepv
    pinjemw    = loss%peaux
    pinjimw    = loss%piaux
    pdivt      = loss%Psep
    plhthresh  = loss%PLH

    !-----------------------------------------
    !previously calculated by pcond
    ptrepv  =  loss%psepe/vol !electron transport power (MW/m3)
    ptripv  =  loss%psepi/vol !ion transport power (MW/m3)
    tauee   =  loss%tauee !electron energy confinement time (s) !Emiliano todo
    tauei   =  loss%tauei !ion energy confinement time (s)
    powerht =  loss%qtot !heating power (MW) assumed in calculation of confinement scaling
    taueff  =  loss%taueff   !global energy confinement time (s)

    !-----------------------------------------
    !previously calculated by pohm
    pohmpv = loss%pohm/vol !ohmic heating power per unit volume (MW/m3)
    pohmmw = loss%pohm !ohmic heating power (MW)
    rpfac = 1.0d0 !neoclassical resistivity enhancement factor
    rplas=loss%rplas
    !-----------------------------------------
    
    rlpint = rmu0 * rmajor * rli/2.0D0

    aeps = (1.0D0 + 1.81D0*sqrt(eps)+2.05D0*eps)*log(8.0D0/eps) &
         - (2.0D0 + 9.25D0*sqrt(eps)-1.21D0*eps)
    beps = 0.73D0 * sqrt(eps) *(1.0D0 + 2.0D0*eps**4-6.0D0*eps**5 &
         + 3.7D0*eps**6)
    rlpext = rmajor*rmu0 * aeps*(1.0D0-eps)/(1.0D0-eps+beps*kappa)

    !-----------------------------------------
    ! previously calculated by vscalc
    phiint = radp%psi(size(radp%psi)) !internal plasma volt-seconds (Wb)
    rlp = rlpext + rlpint
    vburn = plascur * rplas * facoh * csawth !volt-seconds needed during flat-top (heat+burn) (Wb)
    vsbrn = vburn*(theat + tburn)
    vsind = rlp * geom%ip*1.d6 !internal and external plasma inductance V-s (Wb)
    vsres = gamma * rmu0*geom%ip*1.d6*rmajor !resistive losses in start-up volt-seconds (Wb)
    vsstt = vsres + vsind + vsbrn !total volt-seconds needed (Wb) 

    !----------------------------------------
    ! previously calculated by phyaux:
    qfuel = loss%dfuelreq * 2.0*1.d19 !qfuel is for nucleus pairs
    dntau = radp%av_ne*taueff*1.d19 !plasma average n-tau (s/m3)
    rndfuel = fusionrate*vol !fuel burnup rate (reactions/s)
    taup = loss%taueff*comp%globtau(3) !(alpha) particle confinement time (s)
    burnup = rndfuel/qfuel*2.d0 !fractional plasma burnup
    fusrat=fusionrate*vol
    figmer=plascur*1.d-6*aspect
    !---------------------------------------

    pinjmw =loss%pnbi
    pradpv = loss%Prad/vol !Total radiation power (MW) 
    ptrimw = loss%psepi !Ion transport (MW)  
    ptremw = loss%psepe !Electron transport (MW)
 
  end subroutine convert_Plasmod2PROCESS

  subroutine outputPlasmod(outfile)


    !+ad_name  outputPlasmod
    !+ad_summ  Routine to print out the output from PLASMOD
    !+ad_type  Subroutine
    !+ad_auth  K Ellis, UKAEA, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_desc  This routine writes out the results from the PLASMOD code
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarrf
    !+ad_hist  27/02/18 KE Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Arguments
    integer, intent(in) :: outfile
    
    call oheadr(outfile,'PLASMOD')

    call osubhd(outfile,'Geometry')

    !ovarrf is floating point variable using F format
    !ovarre is floating point variable using E format
    !ovarin is floating point variable

    call ovarin(outfile, 'PLASMOD error flag', '(i_flag)', i_flag, 'OP ')

    !if plasmod_i_equiltype = 1 q95 is an input and plascur an output
    !if plasmod_i_equiltype = 2 plascur is an input and q95 an output

    if(plasmod_i_equiltype == 1)then
       call ovarrf(outfile,'Plasma current (MA)','(geom%ip)', geom%ip, 'OP ')
       call ovarrf(outfile,'q95','(geom%q95)', geom%q95)
    else
       call ovarrf(outfile,'Plasma current (MA)','(geom%ip)', geom%ip)
       call ovarrf(outfile,'q95','(geom%q95)', geom%q95, 'OP ')
    endif
     
    call ovarrf(outfile,'Edge elongation','(geom%k)', geom%k, 'OP ')
    call ovarrf(outfile,'Edge triangularity','(geom%d)', geom%d, 'OP ')

    call osubhd(outfile,'Composition')
    call ovarrf(outfile,'Xenon concentration at the pedestal top','(comp%cxe)', comp%cxe)
    call ovarrf(outfile,'Helium concentration','(comp%che)', comp%che)
    call ovarrf(outfile,'Argon concentration at the pedestal top','(comp%car)', comp%car)

    call osubhd(outfile,'Pedestal')
    call ovarrf(outfile,'Pedestal top electron temperature (keV)','(ped%teped)', ped%teped, 'OP ')
    call ovarrf(outfile,'Pedestal top density (10^19 m^-3)','(ped%nped)', ped%nped, 'OP ')
    call ovarrf(outfile,'Separatrix density (10^19 m^-3)','(ped%nsep)', ped%nsep, 'OP ')
    call ovarrf(outfile,'Separatrix temperature (keV?)','(ped%tesep)', ped%tesep)

    call osubhd(outfile,'Power losses')
    call ovarrf(outfile,'Total required power for all kind of controls (MW)','(loss%pnbi)', loss%pnbi, 'OP ')
    call ovarrf(outfile,'Total auxiliary power to electrons (MW)','(loss%peaux)', loss%peaux, 'OP ')
    call ovarrf(outfile,'Total auxiliary power to ions (MW)','(loss%piaux)', loss%piaux, 'OP ')
    call ovarrf(outfile,'Power used to control Psep (MW)','(loss%qheat)', loss%qheat, 'OP ') !KE - not being calculated
    call ovarrf(outfile,'Power used for CD (MW)','(loss%qcd)', loss%qcd, 'OP ') !KE - not being calculated
    call ovarrf(outfile,'Power used to control Pfus (MW)','(loss%qfus)', loss%qfus, 'OP ') !KE - not being calculated
    call ocmmnt(outfile,'Please note that PLASMOD uses both pseprmax and psepbqarmax simultaneously')
    call ovarrf(outfile,'Net separatrix power = Paux+Pfus-Prad (MW)','(loss%Psep)', loss%Psep, 'OP ')
    call ovarrf(outfile,'maximum ratio of Psep*Bt/qAR (MWT/m)','(psepbqarmax)', psepbqarmax)
    call ovarrf(outfile,'Actual Psep*Bt/qAR (MW/m)', '(Psepbt/q95AR)', loss%Psep*bt/rmajor/aspect/geom%q95, 'OP ')
    call ovarrf(outfile,'maximum ratio of power crossing the separatrix to plasma major radius (Psep/R) (MW/m)', &
         '(pseprmax)', pseprmax)
    call ovarrf(outfile,'Actual Psep/R (MW/m)', '(loss%Psep/R)', loss%Psep/rmajor, 'OP ')

    if(inp0%PLH.eq.0.0) then
       call ovarrf(outfile,'LH transition power (MW)','(loss%PLH)', loss%PLH, 'OP ')
    else !user-defined value (see PLASMOD/control_scheme.f90)
       call ovarrf(outfile,'LH transition power (MW)','(loss%PLH)', loss%PLH)
    endif
    
    call ovarrf(outfile,'Total radiated power (MW)','(loss%Prad)', loss%Prad, 'OP ')
    call ovarrf(outfile,'Plasma energy (MJ)','(loss%Wth)', loss%Wth, 'OP ')
    call ovarrf(outfile,'Confinement time (s)','(loss%taueff)', loss%taueff, 'OP ')

    if(num%i_modeltype.eq.1)then !H-factor is an input
       call ovarrf(outfile,'H factor set by user','(loss%H)', loss%H)
    else
       call ovarrf(outfile,'Computed H factor according to ITER 98 y2 elmy H mode','(loss%H)', loss%H, 'OP ')
    endif
    
    call ovarrf(outfile,'Total fusion power (MW) i.e. 5*alpha power','(loss%Pfus)', loss%Pfus, 'OP ')
    call ovarrf(outfile,'Ohmic power (MW)','(loss%Pohm)', loss%Pohm, 'OP ')
    call ovarrf(outfile,'Plasma resistivity (V/A)','(loss%rplas)', loss%rplas, 'OP ')
    call ovarrf(outfile,'Total synchrotron power (MW)','(loss%psync)', loss%psync, 'OP ')
    call ovarrf(outfile,'Total Brehmstrahlung power (MW)','(loss%pbrehms)', loss%pbrehms, 'OP ')
    call ovarrf(outfile,'Total line radiation (MW)','(loss%pline)', loss%pline, 'OP ')
    call ovarrf(outfile,'Ion power through separatrix (MW)','(loss%psepi)', loss%psepi, 'OP ')
    call ovarrf(outfile,'Electron power through separatrix (MW)','(loss%psepe)', loss%psepe, 'OP ')
    call ovarrf(outfile,'Volume averaged equipartition power (MW/m^3),electrons lose and ions gain)','(loss%piepv)',&
         loss%piepv, 'OP ')
    call ovarrf(outfile,'Power onto divertor (MW/m^2)','(loss%pdiv)', loss%pdiv, 'OP ')
    call ovarrf(outfile,'Edge radiation (MW)','(loss%pradedge)', loss%pradedge, 'OP ')
    call ovarrf(outfile,'Core radiation (MW)','(loss%pradcore)', loss%pradcore, 'OP ')
    
    call osubhd(outfile,'Magneto Hydro Dynamics')
    call ovarrf(outfile,'Plasma volume (m^3)','(mhd%vp)', mhd%vp, 'OP ')
    call ovarrf(outfile,'Plasma lateral surface (m^2)','(mhd%Sp)', mhd%Sp, 'OP ')
    call ovarrf(outfile,'Plasma surface of a toroidal section (m^2)','(mhd%torsurf)', mhd%torsurf, 'OP ')  
    call ovarrf(outfile,'Edge safety factor','(mhd%q_sep)', mhd%q_sep, 'OP ')
    call ovarrf(outfile,'Loop voltage (V)','(mhd%vloop)', mhd%vloop, 'OP ')
    call ovarrf(outfile,'Bootstrap current fraction','(mhd%fbs)', mhd%fbs, 'OP ')
    call ovarrf(outfile,'Total non-inductive current fraction','(mhd%f_ni)', mhd%f_ni, 'OP ')

    call osubhd(outfile,'Radial profile averages')
    call ovarrf(outfile,'Volume averaged electron density (10^19 m^-3)','(radp%av_ne)', radp%av_ne, 'OP ')
    call ovarrf(outfile,'Volume averaged ion density (10^19 m^-3)','(radp%av_ni)', radp%av_ni, 'OP ')   
    call ovarrf(outfile,'Volume averaged electron temperature (keV)','(radp%av_te)', radp%av_te, 'OP ')
    call ovarrf(outfile,'Volume averaged ion temperature (keV)','(radp%av_ti)', radp%av_ti, 'OP ')
    call ocmmnt(outfile, 'PLASMOD does not calculate a temperature dependent Zeff!')
    call ovarrf(outfile,'Volume averaged effective charge','(radp%zeff)', radp%zeff, 'OP ')

    call outputRadialProf
    
  end subroutine outputPlasmod

  subroutine outputRadialProf


    !+ad_name  outputRadialProf
    !+ad_summ  Routine to print out the radial profiles from PLASMOD
    !+ad_type  Subroutine
    !+ad_auth  K Ellis, UKAEA, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine writes out the radial profiles as created by the PLASMOD code
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  13/03/18 KE Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none
    
    !  Arguments

    integer, parameter :: radp_file = 15  !  Radial profiles file unit identifier
    character(len = 50) :: outfile_radp
    integer :: j
    outfile_radp = trim(fileprefix)//"_RADP.DAT"

    open(unit = radp_file, file = outfile_radp, action = 'write')
    
    write(radp_file,*) '# **********PLASMOD Radial Profiles***********#'
    write(radp_file,*) '#                                            '
    write(radp_file,*) '# Radial position    ||     Electron density  ||    &
         &Electron temperature   ||   Ion temperature   || &
         &Deuterium density   ||    Tritium density   &
         &BS current density(MA/m^2)||&
 &CD current dens(MA/m^2)||Total current dens(MA/m^2)||&
 &Poloidal current(R*Bp)(T.m)|| Safety factor q    ||     Volume (m^3)    &
 &||      dVolume/dr (m^2)  || Plasma conductivity(MA/(V.m)||&
 &Alpha press(keV*10^10 m^-3)||Ion dens(10^19 m^-3) || &
 &Poloidal flux (Wb)'

    do j=1,size(radp%x)
       write(radp_file,*) radp%x(j),radp%ne(j),radp%Te(j),radp%Ti(j),&
            &radp%ndeut(j),radp%ntrit(j),&
            &radp%jbs(j),radp%jcd(j),radp%jpar(j),&
            &radp%ipol(j),radp%qprof(j),radp%Volum(j),radp%vp(j),radp%cc(j),&
            &radp%palph(j),radp%nions(j),radp%psi(j)
    end do

    close(unit = radp_file)

  end subroutine outputRadialProf
      
end module plasmod_module
