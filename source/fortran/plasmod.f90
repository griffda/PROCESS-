! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module plasmod_module


  !! Module containing plasmod interface
  !! author: K Ellis, CCFE, Culham Science Centre
  !! N/A
  !! This module contains all the interface
  !! functions between the 1D transport and
  !! equilibrium code PLASMOD and the rest
  !! of PROCESS.
  !! E Fable et al. Fus. Eng. & Des. (2018)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none

  public

contains

  subroutine setupPlasmod(i_flag, &
    geom_k, geom_d, geom_ip, geom_k95, geom_d95, &
    geom_r, geom_a, geom_q95, geom_bt, geom_counter, &
    comp_fcoreraditv, comp_qdivt, comp_pradfrac, &
    comp_pradpos, comp_psep_r, comp_psepb_q95AR, comp_protium, &
    comp_psepplh_inf, comp_psepplh_sup, comp_c_car, comp_fuelmix, &
    comp_comparray, comp_globtau, comp_imptype, &
    inp0_car_qdivt, inp0_chisaw, inp0_chisawpos, inp0_contrpovr, &
    inp0_contrpovs, inp0_cxe_psepfac, &
    inp0_eccdeff, inp0_f_gw, inp0_f_gws, &
    inp0_f_ni, inp0_fcdp, inp0_fpellet, inp0_fpion, inp0_gamcdothers, &
    inp0_Hfac_inp, inp0_maxpauxor, inp0_nbcdeff, inp0_nbi_energy, &
    inp0_pech, inp0_pfus, inp0_pheatmax, inp0_PLH, inp0_pnbi, &
    inp0_q_control, inp0_qcd, inp0_qfus, inp0_qheat, inp0_qnbi_psepfac, &
    inp0_sawpertau, inp0_spellet, inp0_V_loop, &
    inp0_x_heat, inp0_x_cd, inp0_x_fus, inp0_x_control, &
    inp0_dx_heat, inp0_dx_cd, inp0_dx_fus, inp0_dx_control, &
    num_Ainc, num_capA, num_dgy, num_dt, num_dtinc, num_dtmax, num_dtmaxmax, &
    num_dtmaxmin, num_dtmin, num_eopt, num_i_equiltype, num_i_impmodel, &
    num_i_modeltype, num_ipedestal, num_iprocess, num_isawt, num_maxA, &
    num_nchannels, num_nx, num_nxt, num_test, num_tol, num_tolmin, &
    ped_tesep, ped_rho_t, ped_rho_n, ped_pedscal, ped_teped &
    )

    !! Routine to set up the PLASMOD input params
    !! author: K Ellis, UKAEA, Culham Science Centre
    !! i_flag : integer    : PLASMOD error flag
    !! num_*  : decomposed contents of numerics derived type
    !! geom_* : decomposed contents of geometry derived type
    !! comp_* : decomposed contents of composition derived type
    !! ped_*  : decomposed contents of pedestal derived type
    !! inp0_* : decomposed contents of miscellaneous input derived type
    !! Arguments are contents of the derived types found in the structs module
    !! This routine sets up the input parameters for
    !! PLASMOD from PROCESS variables.
    !! E Fable et al. Fus. Eng. & Des. (2018)
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use constraint_variables, only: psepbqarmax, pseprmax
    use current_drive_variables, only: fpion, pinjalw, pheat, gamcd
    use div_kal_vars, only: impurity_enrichment
    use error_handling, only: report_error
    use impurity_radiation_module, only: coreradiationfraction, &
        impurity_arr, coreradius
    use physics_variables, only: hfact, tesep, bt, protium, teped, rhopedn, &
        triang95, triang, plascur, ieped, fgwped, aspect, kappa95, q95, &
        kappa, ilhthresh, fdeut, fvsbrnni, rhopedt, fgwsep, rmajor
    use global_variables, only: verbose
    use constants, only: pi, echarge, rmu0
    use structs, only: geometry, composition, pedestal, inputs, &
      numerics_transp
    use numerics, only: boundl
    use plasmod_variables, only: plasmod_nx, plasmod_dx_control, plasmod_dt, &
      plasmod_sawpertau, plasmod_dtmin, plasmod_contrpovs, &
      plasmod_car_qdivt, plasmod_gamcdothers, plasmod_dtmaxmax, &
      plasmod_maxa, plasmod_pfus, plasmod_cxe_psepfac, plasmod_test, &
      plasmod_chisawpos, plasmod_dtmaxmin, plasmod_maxpauxor, &
      plasmod_fradc, plasmod_globtau, plasmod_i_equiltype, plasmod_capa, &
      plasmod_qdivt, plasmod_qnbi_psepfac, plasmod_psepplh_sup, &
      plasmod_x_heat, plasmod_tol, plasmod_contrpovr, plasmod_ainc, &
      plasmod_dx_cd, plasmod_x_cd, plasmod_dtmax, plasmod_v_loop, &
      plasmod_x_fus, plasmod_pedscal, plasmod_i_impmodel, &
      plasmod_x_control, plasmod_nbi_energy, plasmod_dx_fus, &
      plasmod_dx_heat, plasmod_eopt, plasmod_nxt, plasmod_chisaw, &
      plasmod_dgy, plasmod_fcdp, plasmod_nchannels, plasmod_tolmin, &
      plasmod_dtinc, plasmod_iprocess, plasmod_imptype, &
      plasmod_i_modeltype, plasmod_isawt
    implicit none

    !  Arguments
    integer, intent(out) :: i_flag
    
    ! geometry type
    real(8), intent(out) :: geom_k, geom_d, geom_ip, geom_k95, geom_d95, &
      geom_r, geom_a, geom_q95, geom_bt, geom_counter
    
    ! composition type
    real(8), intent(out) :: comp_fcoreraditv, comp_qdivt, comp_pradfrac, &
      comp_pradpos, comp_psep_r, comp_psepb_q95AR, comp_protium, &
      comp_psepplh_inf, comp_psepplh_sup, comp_c_car, comp_fuelmix
    real(8), dimension(:), intent(out) :: comp_comparray, comp_globtau
    integer, dimension(:), intent(out) :: comp_imptype
    
    ! inputs type
    real(8), intent(out) :: inp0_car_qdivt, inp0_chisaw, inp0_chisawpos, inp0_contrpovr, &
      inp0_contrpovs, inp0_cxe_psepfac, &
      inp0_eccdeff, inp0_f_gw, inp0_f_gws, &
      inp0_f_ni, inp0_fcdp, inp0_fpellet, inp0_fpion, inp0_gamcdothers, &
      inp0_Hfac_inp, inp0_maxpauxor, inp0_nbcdeff, inp0_nbi_energy, &
      inp0_pech, inp0_pfus, inp0_pheatmax, inp0_pnbi, &
      inp0_q_control, inp0_qcd, inp0_qfus, inp0_qheat, inp0_qnbi_psepfac, &
      inp0_sawpertau, inp0_spellet, inp0_V_loop
    integer, intent(out) :: inp0_PLH
    real(8), dimension(:), intent(out) :: inp0_x_heat, inp0_x_cd, inp0_x_fus, inp0_x_control, &
      inp0_dx_heat, inp0_dx_cd, inp0_dx_fus, inp0_dx_control
    
    ! numerics_transp type
    real(8), intent(out) :: num_Ainc, num_capA, num_dgy, num_dt, num_dtinc, &
      num_dtmax, num_dtmaxmax, &
      num_dtmaxmin, num_dtmin, num_eopt, &
      num_maxA, &
      num_test, num_tol, num_tolmin
    integer, intent(out) :: num_i_equiltype, num_i_impmodel, num_isawt, &
      num_i_modeltype, num_ipedestal, num_iprocess, num_nchannels, num_nx, &
      num_nxt
    
    ! pedestal type
    real(8), intent(out) :: ped_tesep, ped_rho_t, ped_rho_n, ped_pedscal, &
      ped_teped

    ! all fixed input variables that cannot change within a PROCESS iteration go here!
    ! They only need to be initialised once.

    if (geom_counter.eq.0.d0) then

       num_nx        = plasmod_nx  !number of interpolated grid points
       num_nxt       = plasmod_nxt !number of reduced grid points
       num_nchannels = plasmod_nchannels  !leave this at 3

       !HL: This is a temporary set up for the moment!
       comp_psep_r      = pseprmax !Psep/R max value
       comp_psepb_q95AR = psepbqarmax !Psep B/qAR max value

       ! To selfconsistently compute the He concentration inside PLASMOD
       ! its intial fraction has to be 0.d0. Then globtau is used!
       ! The Xe fraction is used as an iteration variable inside PLASMOD
       ! it adjusts to fulfil psepqbarmax, pseprmax or psepplh_sup.
       comp_comparray = 0.d0 !array of impurity concentrations
       comp_comparray(comp_imptype(3)) = impurity_arr(comp_imptype(3))%frac !argon concentration, uses Kallenbach model if qdivt = 0. from PLASMOD inputs
       comp_comparray(comp_imptype(1)) = impurity_arr(comp_imptype(1))%frac
       comp_protium   = protium !protium is treated separately

       ! Impurities to be used for (1)intrinsic (2)Psep control (3)SOL seeding
       comp_imptype(1) = plasmod_imptype(1)
       comp_imptype(2) = plasmod_imptype(2)
       comp_imptype(3) = plasmod_imptype(3)


       comp_psepplh_inf = boundl(103) !Psep/PLH if below this, use nbi
       comp_psepplh_sup = plasmod_psepplh_sup !Psep/PLH if above this, use Xe
       inp0_maxpauxor   = plasmod_maxpauxor ! maximum Paux/R allowed

       num_tol    = plasmod_tol !tolerance to be reached, in % variation at each time step
       num_dtmin  = plasmod_dtmin !min time step
       num_dtmax  = plasmod_dtmax !max time step
       num_dt     = plasmod_dt !time step
       num_dtinc  = plasmod_dtinc !decrease of dt
       num_Ainc   = plasmod_ainc !increase of dt
       num_test   = plasmod_test !max iteration number
       num_tolmin = plasmod_tolmin ! multiplier of etolm that should not be overcome

       num_eopt     = plasmod_eopt !exponent of jipperdo
       num_dtmaxmin = plasmod_dtmaxmin !exponent of jipperdo2
       num_dtmaxmax = plasmod_dtmaxmax !stabilizing coefficient
       num_capA     = plasmod_capa !first radial grid point
       num_maxA     = plasmod_maxa !diagz 0 or 1
       num_dgy      = plasmod_dgy !Newton differential

       num_iprocess = plasmod_iprocess !0 - use PLASMOD functions, 1 - use PROCESS functions
       num_i_modeltype = plasmod_i_modeltype !1 - simple gyrobohm scaling with imposed H factor > 1, other models with H in output
       num_i_equiltype = plasmod_i_equiltype !1 - EMEQ, solve equilibrium
       !with given q95, with sawteeth. 2- EMEQ, solve with given Ip, with sawteeth.
       !sawtooth inputs
       num_isawt     = plasmod_isawt !0 - no sawteeth, 1 - solve with sawteeth
       inp0_chisawpos= plasmod_chisawpos !position where artificial sawtooth diffusivity is added, -1 - uses q=1 position
       inp0_chisaw   = plasmod_chisaw !artificial diffusivity in m^2/s
       inp0_sawpertau= plasmod_sawpertau !ratio between sawtooth period and confinement time

       if(ieped == 0) then
          num_ipedestal= 1  !fixed temperature pedestal
       else if (ieped == 1) then
          num_ipedestal= 2  !Sareelma scaling
       else
          call report_error(175) !option not possible
       endif

       num_i_impmodel = plasmod_i_impmodel !impurity model: 0 - fixed
       !concentration, 1 - concentration fixed at pedestal top, then fixed density.
       comp_globtau(1) = plasmod_globtau(1) !tauparticle/tauE for D, T, He, Xe, Ar
       comp_globtau(2) = plasmod_globtau(2) !tauparticle/tauE for D, T, He, Xe, Ar
       comp_globtau(3) = plasmod_globtau(3) !tauparticle/tauE for D, T, He, Xe, Ar
       comp_globtau(4) = plasmod_globtau(4) !tauparticle/tauE for D, T, He, Xe, Ar Not used for Xe!
       comp_globtau(5) = plasmod_globtau(5) !tauparticle/tauE for D, T, He, Xe, Ar
       comp_fuelmix = fdeut !fuel mix

       !compression factor between div and
       !core: e.g. 10 means there is 10 more Argon concentration in the
       !divertor than in the core
       comp_c_car = impurity_enrichment(comp_imptype(3))


       !derivatives
       inp0_qnbi_psepfac = plasmod_qnbi_psepfac !dqnbi/d(1-Psep/PLH)
       inp0_cxe_psepfac  = plasmod_cxe_psepfac !dcxe/d(1-Psep/PLH)
       inp0_car_qdivt    = plasmod_car_qdivt !dcar/d(qdivt)

       !deposition locations
       inp0_x_heat(1)     = plasmod_x_heat(1) !nbi
       inp0_x_heat(2)     = plasmod_x_heat(2) !ech
       inp0_x_cd(1)       = plasmod_x_cd(1) !nbi
       inp0_x_cd(2)       = plasmod_x_cd(2) !ech
       inp0_x_fus(1)      = plasmod_x_fus(1) !nbi
       inp0_x_fus(2)      = plasmod_x_fus(2) !ech
       inp0_x_control(1)  = plasmod_x_control(1) !nbi
       inp0_x_control(2)  = plasmod_x_control(2) !ech
       inp0_dx_heat(1)    = plasmod_dx_heat(1) !nbi
       inp0_dx_heat(2)    = plasmod_dx_heat(2) !ech
       inp0_dx_cd(1)      = plasmod_dx_cd(1) !nbi
       inp0_dx_cd(2)      = plasmod_dx_cd(2) !ech
       inp0_dx_fus(1)     = plasmod_dx_fus(1) !nbi
       inp0_dx_fus(2)     = plasmod_dx_fus(2) !ech
       inp0_dx_control(1) = plasmod_dx_control(1) !nbi
       inp0_dx_control(2) = plasmod_dx_control(2) !ech
       inp0_nbi_energy    = plasmod_nbi_energy !in keV

       inp0_eccdeff = 0.3  !CD = this * PCD * TE/NE !not used for now
       inp0_pech    = 0.d0 !ech power !not used for now
       inp0_pnbi    = 0.d0 !nbi power
       inp0_qheat   = 0.d0 !
       inp0_qcd     = 0.d0 !
       inp0_qfus    = 0.d0 !
       inp0_gamcdothers = plasmod_gamcdothers

       inp0_spellet = 0.d0 !pellet mass in particles of D in 10^19
       inp0_fpellet = 0.5d0 !pellet frequency in Hz

       ! implemented global variables ready for use when required
       !inp0_eccdeff = plasmod_eccdeff  !CD = this * PCD * TE/NE !not used for now
       !inp0_pech    = plasmod_pech !ech power !not used for now
       !inp0_spellet = plasmod_spellet !pellet mass in particles of D in 10^19
       !inp0_fpellet = plasmod_fpellet !pellet frequency in Hz


       inp0_V_loop = plasmod_v_loop !target loop voltage. If lower than  -1.e5 dont use
       inp0_pfus   = plasmod_pfus !if 0., not used (otherwise it would be controlled with Pauxheat)
       !Only one of the two below should be specified
       inp0_contrpovs = plasmod_contrpovs !control power in Paux/lateral_area (MW/m2)
       inp0_contrpovr = plasmod_contrpovr !control power in Paux/R (MW/m)

       ! qdivt should be equal to qtargettotal /5.0e6/ if using the
       ! Kallenbach model at the same time
       ! This needs to be implemented when coupling to the Kallenbach model!
       comp_qdivt       = plasmod_qdivt !divertor heat flux in MW/m^2, if 0, dont use SOL model
       comp_pradfrac    = coreradiationfraction !fraction of radiation from 'core' region that is subtracted from the loss power
       comp_pradpos     = coreradius ! position after which radiation is
       !counted 0. for tau and other global quantities, i.e. position after
       !which radiation is "edge"


       ped_tesep   = tesep  !separatrix temperature
       ped_rho_t   = rhopedt !pedestal top position T
       ped_rho_n   = rhopedn !pedestal top position n
       ped_pedscal = plasmod_pedscal !multiplies the pedestal scaling in PLASMOD
       ped_teped   = teped !pedestal top temperature
       inp0_f_gws  = fgwsep !separatrix greenwald fraction

       geom_k  = kappa !edge elongation
       geom_d  = triang !edge triangularity
       geom_ip = plascur/1.d6
       geom_k95 = kappa95 !edge elongation
       geom_d95 = triang95 !edge triangularity

    endif

    ! Variables that can be iteration variables or those that
    ! are calculated inside PROCESS need to be put here:

    i_flag = 1

    geom_r   = rmajor
    geom_a   = aspect
    geom_q95 = q95
    geom_bt  = bt

    inp0_f_gw      = fgwped !pedestal top greenwald fraction
    inp0_Hfac_inp  = hfact !input H factor (radiation corrected), if 0., this is not used.
    inp0_pheatmax  = pinjalw !max allowed power for heating+CD+fusion control
    inp0_q_control = pheat !minimal power required for control

    !fvsbrnni can be an iteration variable!
    inp0_f_ni   = fvsbrnni !required fraction of non inductive current, if 0 dont use CD

    !Iterations variables for PLASMOD - see issue #658
    inp0_fcdp        = plasmod_fcdp !(P_CD - Pheat)/(Pmax-Pheat),i.e. ratio of CD power over available power (PROCESS iteration variable 147)
    comp_fcoreraditv = plasmod_fradc !Pline_Xe / (Palpha + Paux - PlineAr - Psync - Pbrad) (PROCESS iteration variable 148)

    !Note that this is only a correct input on the second iteration!
    inp0_fpion = fpion ! Fraction of neutral beam energy to ions

    if (comp_qdivt.eq.0.d0) then
       comp_comparray(comp_imptype(3)) = impurity_arr(comp_imptype(3))%frac !argon concentration, uses Kallenbach model if qdivt = 0. from PLASMOD inputs
       !else
       !@EF: What should happen, if this is not assigned?
    endif

    !uses PROCESS defined LH threshold, if this is > 0
    inp0_PLH = ilhthresh

    inp0_nbcdeff = gamcd ! normalised current drive efficiency (1.0e20 A/W-m2)


  end subroutine setupPlasmod

  subroutine convert_Plasmod2PROCESS(geom,comp,ped,radp,mhd,loss,theat,&
		& tburn,fusrat)

    !! Routine to set up the PLASMOD input params
    !! author: K Ellis, UKAEA, Culham Science Centre
    !! num : derived type :
    !! geom : derived type :
    !! comp : derived type :
    !! ped : derived type :
    !! This routine writes out the times of the various stages
    !! during a single plant cycle.
    !! E. Fable et al., Fusion Engineering and Design, Volume 130, May 2018, Pages 131-136

    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use current_drive_variables, only: pinjimw, bootipf, pinjemw, pinjmw, &
        ftritbm, plasipf
		use div_kal_vars, only: netau_sol
		use error_handling, only: idiags, fdiags, report_error
    use impurity_radiation_module, only: impurity_arr, nimp, element2index, &
        zav_of_te
    use physics_variables, only: rplas, tin, pohmmw, sf, facoh, eps, &
        pneutmw, pdt, p0, powerht, faccd, gammaft, dnla, bp, ptripv, &
        plhthresh, plascur, psyncpv, dlamee, q95, pradpv, dnalp, aion, &
        ignite, dnitot, pdd, normalised_total_beta, afuel, vsbrn, zeffai, &
        abeam, protonrate, alpharate, ti, phiint, dnbeam2, qstar, falpha, &
        ptremw, taueff, ne0, pperim, plinepv, sarea, rnbeam, ftrit, &
        fusionrate, xarea, pdhe3, rminor, te0, fhe3, fvsbrnni, burnup, ten, &
        piepv, powfmw, neped, bt, vsind, vol, taup, teped, palpipv, csawth, &
        falpe, pradmw, rncne, palpepv, qfuel, palpmw, te, betanb, dene, &
        triang, rnone, ptrepv, palpnb, tauei, tauee, pneutpv, dntau, &
        pcoreradmw, ti0, rli, pchargemw, pfuscmw, vsstt, rlp, ralpne, &
        pchargepv, hfact, figmer, protium, pohmpv, pdivt, rndfuel, rpfac, &
        betaft, ptrimw, ni0, zeff, vsres, nesep, dnz, pedgeradmw, dlamie, &
        falpi, kappa, rnfene, pbrempv, rmajor, dnbeam, gamma, kappaa, deni, &
        dnprot, beta, fdeut, palppv, aspect
    
		use constants, only: rmu0, echarge, pi, nout
    use structs, only: geometry, composition, pedestal, radial_profiles, &
      MHD_EQ, power_losses
    use plasmod_variables, only: power_losses, plasmod_i_impmodel, geometry, &
      i_flag
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
    ! Issue 879 remove the line in Plasmod that overwrites 'vol'
    ! vol = mhd%vp ! plasma volume (m^3)
    ! write(*,*)'convert_Plasmod2PROCESS: Plasma volume  ', vol

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
    !calculated in plasma_compostion
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

    ! Issue #261 Remove zfear.  Use the sum of Fe and Ar concentrations
    ! if (zfear == 0) then
    !    rnfene = impurity_arr(element2index('Fe'))%frac
    ! else
    !    rnfene = impurity_arr(element2index('Ar'))%frac
    ! end if
    rnfene = impurity_arr(element2index('Fe'))%frac + impurity_arr(element2index('Ar'))%frac

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
    plasipf = bootipf

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


    !! Routine to print out the output from PLASMOD
    !! author: K Ellis, UKAEA, Culham Science Centre
    !! outfile : input integer : Fortran output unit identifier
    !! This routine writes out the results from the PLASMOD code
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use constraint_variables, only: psepbqarmax, pseprmax
		use physics_variables, only: rmajor, bt, aspect
    use process_output, only: ovarin, ocmmnt, oheadr, ovarrf, osubhd
    
		use constants, only: echarge, rmu0
    use plasmod_variables, only: radp, ped, geom, plasmod_i_equiltype, loss, &
      inp0, mhd, num, i_flag, comp
    implicit none

    !  Arguments
    integer, intent(in) :: outfile
    ! integer :: imp (NOT USED)
    !character*10 :: nImpurity

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

    call osubhd(outfile,'Composition (impurity concentrations)')
    !do imp=1,nimp
    !   nImpurity = 'Impurity' //char(imp)
       !call ovarrf(outfile,'Impurity '//char(imp),'(comp%comparray)', comp%comparray(imp))
    !   call ovarrf(outfile,nImpurity,'(comp%comparray)', comp%comparray(imp))
    !enddo
    call ovarrf(outfile,'Hydrogen','(comp%comparray [1])', comp%comparray(1))
    call ovarrf(outfile,'Helium','(comp%comparray [2])', comp%comparray(2))
    call ovarrf(outfile,'Berylium','(comp%comparray [3])', comp%comparray(3))
    call ovarrf(outfile,'Carbon','(comp%comparray [4])', comp%comparray(4))
    call ovarrf(outfile,'Nitrogen','(comp%comparray [5])', comp%comparray(5))
    call ovarrf(outfile,'Oxygen','(comp%comparray [6])', comp%comparray(6))
    call ovarrf(outfile,'Neon','(comp%comparray [7])', comp%comparray(7))
    call ovarrf(outfile,'Silicon','(comp%comparray [8])', comp%comparray(8))
    call ovarrf(outfile,'Argon','(comp%comparray [9])', comp%comparray(9))
    call ovarrf(outfile,'Iron','(comp%comparray[10])', comp%comparray(10))
    call ovarrf(outfile,'Nickel','(comp%comparray[11])', comp%comparray(11))
    call ovarrf(outfile,'Krypton','(comp%comparray[12])', comp%comparray(12))
    call ovarrf(outfile,'Xenon','(comp%comparray[13])', comp%comparray(13))
    call ovarrf(outfile,'Tungsten','(comp%comparray[14])', comp%comparray(14))

    !call ovarrf(outfile,'Xenon concentration at the pedestal top','(comp%cxe)', comp%cxe)
    !call ovarrf(outfile,'Helium concentration','(comp%che)', comp%che)
    !call ovarrf(outfile,'Argon concentration at the pedestal top','(comp%car)', comp%car)

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


    !! Routine to print out the radial profiles from PLASMOD
    !! author: K Ellis, UKAEA, Culham Science Centre
    !! None
    !! This routine writes out the radial profiles as created by the PLASMOD code
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		use global_variables, only: fileprefix, output_prefix
		use plasmod_variables, only: radp, i_flag
    implicit none

    !  Arguments

    integer, parameter :: radp_file = 15  !  Radial profiles file unit identifier
    integer :: file_name_length
    character(len = 50) :: outfile_radp
    integer :: j

    file_name_length = LEN_TRIM(fileprefix)
    output_prefix = fileprefix(1:file_name_length-6)
    outfile_radp = trim(output_prefix)//"RADP.DAT"

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
