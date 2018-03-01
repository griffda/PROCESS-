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
  !+ad_call  None
  !+ad_hist  26/02/18 KE Initial version of module
  !+ad_stat  Okay
  !+ad_docs  E Fable et al. Fus. Eng. & Des. (2018)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use plasmod_variables
  use global_variables
  use physics_variables
  use impurity_radiation_module
  use current_drive_variables
  use constants
  use constraint_variables
  use process_output
  use error_handling
  use divertor_variables
  use numerics !for boundl

  
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
       
    inp0%Hfac_inp  = hfact !input H factor (radiation corrected), if 0., this is not used. 
    inp0%pheatmax  = pinjalw !max allowed power for heating+CD+fusion control
    inp0%q_control = pheat !minimal power required for control
    

    ! all fixed input variables that cannot change within a PROCESS iteration go here!
    ! They only need to be initialised once.
    if (geom%counter.eq.0.d0) then

       geom%k95 = kappa95 !edge elongation
       geom%d95 = triang95 !edge triangularity
         
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
       num%i_modeltype = plasmod_i_modeltype !1 - simple gyrobohm scaling
       num%i_equiltype = plasmod_i_equiltype !1 - EMEQ, solve equilibrium with given q95, with sawteeth. 2- EMEQ, solve with given Ip, with sawteeth.
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
       
       num%i_impmodel = plasmod_i_impmodel !impurity model: 0 - fixed concentration, 1 - concentration fixed at pedestal top, then fixed density.
       ! HL Todo: We need to make sure that our impurity fractions/mixes match with Emilianos confinement time relations!
       comp%globtau(1) = plasmod_globtau(1) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(2) = plasmod_globtau(2) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(3) = plasmod_globtau(3) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(4) = plasmod_globtau(4) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(5) = plasmod_globtau(5) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%fuelmix = 0.5d0 !fuel mix Could be fdeut or ftrit! CHECK!
       comp%c_car = plasmod_c_car !compression factor between div and core: e.g. 10 means there is 10 more Argon concentration in the divertor than in the core
       !HL Todo: We need to make sure the impurity concentrations match with the PROCESS definitions for ralpne and fimp
       comp%car = fimp(9) !argon concentration, used if qdivt=0.
       comp%cxe = fimp(13) !xenon concentration, if negative uses Psepplh as criterion
       comp%che = ralpne !helium concentration, used if globtau(3)=0.
       
       !derivatives
       inp0%qnbi_psepfac=plasmod_qnbi_psepfac !dqnbi/d(1-Psep/PLH)
       inp0%cxe_psepfac=plasmod_cxe_psepfac !dcxe/d(1-Psep/PLH)
       inp0%car_qdivt=plasmod_car_qdivt !dcar/d(qdivt)
       
       !deposition locations
       inp0%x_heat(1)=plasmod_x_heat(1) !nbi
       inp0%x_heat(2)=plasmod_x_heat(2) !ech
       inp0%x_cd(1)=plasmod_x_cd(1) !nbi
       inp0%x_cd(2)=plasmod_x_cd(2) !ech
       inp0%x_fus(1)=plasmod_x_fus(1) !nbi
       inp0%x_fus(2)=plasmod_x_fus(2) !ech
       inp0%x_control(1)=plasmod_x_control(1) !nbi
       inp0%x_control(2)=plasmod_x_control(2) !ech
       inp0%dx_heat(1)=plasmod_dx_heat(1) !nbi
       inp0%dx_heat(2)=plasmod_dx_heat(2) !ech
       inp0%dx_cd(1)=plasmod_dx_cd(1) !nbi
       inp0%dx_cd(2)=plasmod_dx_cd(2) !ech
       inp0%dx_fus(1)=plasmod_dx_fus(1) !nbi
       inp0%dx_fus(2)=plasmod_dx_fus(2) !ech
       inp0%dx_control(1)=plasmod_dx_control(1) !nbi
       inp0%dx_control(2)=plasmod_dx_control(2) !ech
       inp0%nbi_energy=plasmod_nbi_energy !in keV

       !HL To do: I guess, these still need to be made input variables?
       inp0%eccdeff=0.3 !CD = this * PCD * TE/NE !not used for now
       inp0%pech=0.d0 !ech power !not used for now
       inp0%pnbi=0.d0 !nbi power
       inp0%qheat=0.d0 !nbi power
       inp0%qcd=0.d0 !nbi power
       inp0%qfus=0.d0 !nbi power
       inp0%spellet=0.d0 !pellet mass in particles of D in 10^19
       inp0%fpellet=0.5d0 !pellet frequency in Hz
       
       inp0%f_gw   = fgwped !pedestal top greenwald fraction
       inp0%f_gws  = fgwsep !separatrix greenwald fraction
       inp0%V_loop = plasmod_v_loop !target loop voltage. If lower than -1.e5 dont use
       inp0%f_ni   = plasmod_f_ni !required fraction of non inductive current, if 0 dont use CD
       inp0%pfus   = plasmod_pfus !if 0., not used (otherwise it would be controlled with Pauxheat)
       
       comp%psepplh_inf = boundl(103) !Psep/PLH if below this, use nbi
       comp%psepplh_sup = plasmod_psepplh_sup !Psep/PLH if above this, use Xe

       comp%qdivt       = plasmod_qdivt !divertor heat flux in MW/m^2, if 0, dont use SOL model
       comp%pradpos     = coreradius ! position after which radiation is counted 0. for tau and other global quantities, i.e. position after which radiation is "edge"

       
       ! These values should only be set, if the respective constraints
       ! are being used. They should be set to large values otherwise.
       ! pseprmax and psepbqarmax cannot be used at the same time!
       if (any(icc == 56)) then
          comp%psep_r      = pseprmax !Psep/R max value
          comp%psepb_q95AR = 1.0e3 !large number to have no effect
       else if (any(icc == 68)) then
          comp%psep_r      = 1.0e3 !large number to have no effect
          comp%psepb_q95AR = psepbqarmax !Psep B/qaR max value
       else
          comp%psep_r      = 1.0e3 !large number to have no effect
          comp%psepb_q95AR = 1.0e3 !large number to have no effect
       endif
       
       ped%rho_t = rhopedt !pedestal top position T
       ped%rho_n = rhopedt !pedestal top position n
       ped%tesep = tesep  !separatrix temperature
       ped%teped = teped !pedestal top temperature
       
       geom%k  = kappa !edge elongation
       geom%d  = triang !edge triangularity
       geom%ip = plascur/1.d6	
       
       write(*,*) 'gamcd = ', gamcd
       inp0%nbcdeff = gamcd !CD = this * PCD   units: m*MA/MW (MA/m^2 * m^3/MW)
       

    endif
    
  end subroutine setupPlasmod

  subroutine convert_Plasmod2PROCESS(geom,comp,ped,inp0,radp,mhd,loss)

    !+ad_name  convert_Plasmod2PROCESS
    !+ad_summ  Routine to set up the PLASMOD input params
    !+ad_type  Subroutine
    !+ad_auth  K Ellis, UKAEA, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  num : derived type :
    !+ad_args  geom : derived type : 
    !+ad_args  comp : derived type :
    !+ad_args  ped : derived type :
    !+ad_args  inp0 : derived type : 
    !+ad_desc  This routine writes out the times of the various stages
    !+ad_desc  during a single plant cycle.
    !+ad_prob  None
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
    type (inputs), intent(inout) :: inp0
    type (radial_profiles), intent(inout) :: radp
    type (MHD_EQ), intent(inout) :: mhd 
    type (power_losses), intent(inout) :: loss 


    if (i_flag==1)then
       write(*,*) 'PLASMOD has converged!!!'
    elseif (i_flag==0)then
       write(*,*) 'The PLASMOD transport model has crashed'
       call report_error(174)
    elseif (i_flag==-1)then
       write(*,*) 'The PLASMOD transport model has not converged after itermax'
       call report_error(174)
    elseif (i_flag==-2)then
       write(*,*) 'The PLASMOD equilibrium has crashed'
       call report_error(174)
    endif

    
    if (mhd%equilcheck .ne. 1) then
       write(*,*) 'The PLASMOD equilibrium has crashed'
       call report_error(174) 
    endif
    
    
    te0 = radp%te(1) 
    teped = ped%teped !only computed, if ieped = 2
    ne0   = radp%ne(1)
    neped = ped%nped
    nesep = ped%nsep

    
    !If plascur was an input, q95 is an output and vice versa
    !Reassign both for simplicity
    !Plasma current in Ampere
    !plascur = geom%ip * 1.0D6 
    plascur = mhd%ip_out  * 1.0D6
    !Edge safety factor
    !q95 = geom%q95  
    q95 = mhd%q
    
    ralpne   = comp%che
    fimp(13) = comp%cxe 
    fimp(9)  = comp%car
    
    normalised_total_beta = mhd%betan ! Todo Assure other beta's are calculated consistently.
    
    vol = mhd%vp ! plasma volume (m^3)
    !mhd%q_sep !q at separatrix
    !mhd%vloop !loop voltage in V ! Check this is consistent with our volt-seconds requirements routine in physics.f90
    bootipf= mhd%fbs
    !If this is an output cannot be an iteration variable! Add input check!
    fvsbrnni = mhd%f_ni !non-inductive current fraction 
    
    powfmw = loss%pfus ! Check this is consistent with PROCESS calculation! Does PLASMOD only use an approximation?
    taueff = loss%taueff 
    hfact = loss%H
        
    != loss%Wth 
    pradmw     = loss%prad ! fradpwr is total radiation fraction 
    pedgeradmw = loss%pradedge    
    pcoreradmw = loss%pradcore
    psyncpv    = loss%psync/vol 
    pbrempv    = loss%pbrehms/vol
    plinepv    = loss%pline/vol
    != loss%psepi / 1.0D6 !psep_kallenbach is Power conducted through the separatrix, as calculated by the divertor model [W] ion/electron??
    piepv      = loss%piepv
    pinjemw    = loss%peaux
    pinjimw    = loss%piaux
    hldiv      = loss%pdiv
    pdivt      = loss%Psep
    plhthresh  = loss%PLH 
    
    qfuel = loss%dfuelreq * 2.0 !qfuel is for nucleus pairs
    != loss%tfuelreq ! think this is assumed in PROCESS to be the same as above
    != loss%hepumpreq

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

    call ovarin(outfile, 'PLASMOD error flag', '(i_flag)', i_flag)
    
    call ovarrf(outfile,'Plasma current (MA)','(geom%ip)', geom%ip)
    call ovarrf(outfile,'q95','(geom%q95)', geom%q95)
    call ovarrf(outfile,'Edge elongation','(geom%k)', geom%k)
    call ovarrf(outfile,'Edge triangularity','(geom%d)', geom%d)

    call osubhd(outfile,'Composition')
    call ovarrf(outfile,'Xenon concentration at the pedestal top','(comp%cxe)', comp%cxe)
    call ovarrf(outfile,'Helium concentration','(comp%che)', comp%che)
    call ovarrf(outfile,'Argon concentration at the pedestal top','(comp%car)', comp%car)

    call osubhd(outfile,'Pedestal')
    call ovarrf(outfile,'Pedestal top electron temperature (keV)','(ped%teped)', ped%teped)
    call ovarrf(outfile,'Pedestal top density (10^19 m^-3)','(ped%nped)', ped%nped)
    call ovarrf(outfile,'Separatrix density (10^19 m^-3)','(ped%nsep)', ped%nsep)
    call ovarrf(outfile,'Separatrix temperature (keV?)','(ped%tesep)', ped%tesep)

    call osubhd(outfile,'Power losses')
    call ovarrf(outfile,'Total required power for all kind of controls (MW)','(loss%pnbi)', loss%pnbi)
    call ovarrf(outfile,'Total auxiliary power to electrons (MW)','(loss%peaux)', loss%peaux)
    call ovarrf(outfile,'Total auxiliary power to ions (MW)','(loss%piaux)', loss%piaux)
    call ovarrf(outfile,'Power used to control Psep (MW)','(loss%qheat)', loss%qheat)
    call ovarrf(outfile,'Power used for CD (MW)','(loss%qcd)', loss%qcd)
    call ovarrf(outfile,'Power used to control Pfus (MW)','(loss%qfus)', loss%qfus)
    call ovarrf(outfile,'Net separatrix power = Paux+Pfus-Prad (MW)','(loss%Psep)', loss%Psep)
    call ovarrf(outfile,'LH transition power (MW)','(loss%PLH)', loss%PLH)
    call ovarrf(outfile,'Total radiated power (MW)','(loss%Prad)', loss%Prad)
    call ovarrf(outfile,'Plasma energy (MJ)','(loss%Wth)', loss%Wth)
    call ovarrf(outfile,'Confinement time (s)','(loss%taueff)', loss%taueff)
    call ovarrf(outfile,'Computed H factor according to ITER 98 y2 elmy H mode','(loss%H)', loss%H)
    call ovarrf(outfile,'Total fusion power (MW) i.e. 5*alpha power','(loss%Pfus)', loss%Pfus)
    call ovarrf(outfile,'Ohmic power (MW)','(loss%Pohm)', loss%Pohm)
    call ovarrf(outfile,'Plasma resistivity (V/A)','(loss%rplas)', loss%rplas)
    call ovarrf(outfile,'Total synchrotron power (MW)','(loss%psync)', loss%psync)
    call ovarrf(outfile,'Total Brehmstrahlung power (MW)','(loss%pbrehms)', loss%pbrehms)
    call ovarrf(outfile,'Total line radiation (MW)','(loss%pline)', loss%pline)
    call ovarrf(outfile,'Ion power through separatrix (MW)','(loss%psepi)', loss%psepi)
    call ovarrf(outfile,'Electron power through separatrix (MW)','(loss%psepe)', loss%psepe)
    call ovarrf(outfile,'Volume averaged equipartition power (MW/m^3),electrons lose and ions gain)','(loss%piepv)', loss%piepv)
    call ovarrf(outfile,'Power onto divertor (MW/m^2)','(loss%pdiv)', loss%pdiv)
    call ovarrf(outfile,'Edge radiation (MW)','(loss%pradedge)', loss%pradedge)
    call ovarrf(outfile,'Core radiation (MW)','(loss%pradcore)', loss%pradcore)  
    
    call osubhd(outfile,'Magneto Hydro Dynamics')
    call ovarrf(outfile,'Plasma volume (m^3)','(mhd%vp)', mhd%vp)
    call ovarrf(outfile,'Plasma lateral surface (m^2)','(mhd%Sp)', mhd%Sp)
    call ovarrf(outfile,'Plasma surface of a toroidal section (m^2)','(mhd%torsurf)', mhd%torsurf)  
    call ovarrf(outfile,'Edge safety factor','(mhd%q_sep)', mhd%q_sep)
    call ovarrf(outfile,'Loop voltage (V)','(mhd%vloop)', mhd%vloop)
    call ovarrf(outfile,'Bootstrap current fraction','(mhd%fbs)', mhd%fbs)
    call ovarrf(outfile,'Total non-inductive current fraction','(mhd%f_ni)', mhd%f_ni)

    call osubhd(outfile,'Radial profile averages')
    call ovarrf(outfile,'Volume averaged electron density (10^19 m^-3)','(radp%av_ne)', radp%av_ne)
    call ovarrf(outfile,'Volume averaged ion density (10^19 m^-3)','(radp%av_ni)', radp%av_ni)   
    call ovarrf(outfile,'Volume averaged electron temperature (keV)','(radp%av_te)', radp%av_te)
    call ovarrf(outfile,'Volume averaged ion temperature (keV)','(radp%av_ti)', radp%av_ti)
    call ovarrf(outfile,'Volume averaged effective charge','(radp%zeff)', radp%zeff)
    
  end subroutine outputPlasmod
  
end module plasmod_module
