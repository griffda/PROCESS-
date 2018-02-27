!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module plasmod_module


  !+ad_name  plasmod
  !+ad_summ  Module containing plasmod interface
  !+ad_type  Module
  !+ad_auth  K Ellis, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  N/A
  !+ad_desc  This module contains variables relating to the PLASMOD
  !+ad_desc  1-dimensional transport code.
  !+ad_prob  None
  !+ad_call  None
  !+ad_hist  26/02/18 KE Initial version of module
  !+ad_stat  Okay
  !+ad_docs  E Fable et al. Fus. Eng. & Des. (2018)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! This module sets up the plasmod variables, calls the function and writes the output
  use plasmod_variables
  use global_variables
  use physics_variables
  use impurity_radiation_module
  use current_drive_variables
  use constants
  use constraint_variables
  use process_output
  use error_handling

  
  implicit none

  public
  
contains

  subroutine setupPlasmod(num,geom,comp,ped,inp0)

    !+ad_name  setupPlasmod
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
    !+ad_call  oblnkl
    !+ad_call  oheadr
    !+ad_call  ovarrf
    !+ad_hist  26/02/18 KE Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments
 
    type (geometry), intent(inout) :: geom
    type (composition), intent(inout) :: comp
    type (pedestal), intent(inout) :: ped
    type (inputs), intent(inout) :: inp0
    type (numerics_transp), intent(inout) :: num
    
    alpha_crit = (kappa ** 1.2) * (1 + 1.5 * triang)
    nesep_crit = 5.9D0 * alpha_crit * (aspect ** (-2.0D0/7.0D0)) * (((1.0D0 + (kappa ** 2.0D0)) / 2.0D0) ** (-6.0D0/7.0D0)) &
    * ((pdivt * 1.0D6) ** (-11.0D0/70.0D0)) * dlimit(7)
    !teped = t_eped_scaling()
    
    !Here are the PROCESS links for input
    geom%r=rmajor
    geom%a=aspect
    geom%q95=q95
    geom%bt=bt
    geom%k95 = kappa95 !edge elongation
    geom%d95 = triang95 !edge triangularity
    
    ped%rho_t=rhopedt !pedestal top position T
    ped%rho_n=rhopedt !pedestal top position n
    ped%tesep=tesep  !separatrix temperature
    
    inp0%Hfac_inp=hfact !input H factor, if 0., this is not used. This is radiation corrected H factor
    inp0%pheatmax=pinjalw !max allowed power for heating+CD+fusion control
    inp0%q_control=pheat !minimal power required for control
    

    inp0%f_gw=fgwped !pedestal top greenwald fraction
    inp0%f_gws=fgwsep !separatrix greenwald fraction
       
    write(*,*) 'gamcd = ', gamcd
    inp0%nbcdeff=gamcd !CD = this * PCD   units: m*MA/MW (MA/m^2 * m^3/MW)
    
    comp%pradpos = coreradius ! position after which radiation is counted 0. for tau and other global quantities, i.e. position after which radiation is "edge"
    comp%psepb_q95AR = psepbqarmax !Psep B/qaR max value

    if (geom%counter.eq.0.d0) then     
       num%tol=plasmod_tol !tolerance to be reached, in % variation at each time step
       num%dtmin=plasmod_dtmin !min time step
       num%dtmax=plasmod_dtmax !max time step
       num%dt=plasmod_dt !time step
       num%dtinc=plasmod_dtinc !decrease of dt
       num%Ainc=plasmod_ainc !increase of dt
       num%test=plasmod_test !max iteration number
       num%tolmin=plasmod_tolmin ! multiplier of etolm that should not be overcome
       
       num%eopt=plasmod_eopt !exponent of jipperdo
       num%dtmaxmin=plasmod_dtmaxmin !exponent of jipperdo2
       num%capA=plasmod_capa !first radial grid point
       num%maxA=plasmod_maxa !diagz 0 or 1
       num%dgy=plasmod_dgy !Newton differential
       num%i_modeltype=plasmod_i_modeltype !1 - simple gyrobohm scaling
       num%i_equiltype=plasmod_i_equiltype !1 - EMEQ, solve equilibrium with given q95, with sawteeth. 2- EMEQ, solve with given Ip, with sawteeth.
       num%nx=plasmod_nx	 !number of interpolated grid points
       num%nxt=plasmod_nxt !number of reduced grid points
       num%nchannels=plasmod_nchannels  !leave this at 3
       
       if(ieped == 0) then
          num%ipedestal= 1  !fixed temperature pedestal
       else if (ieped == 1) then
          num%ipedestal= 2  !Sareelma scaling
       else
          call report_error(175) !option not possible
       endif
       
       num%i_impmodel=plasmod_i_impmodel !impurity model: 0 - fixed concentration, 1 - concentration fixed at pedestal top, then fixed density.
       comp%globtau(1) = plasmod_globtau(1) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(2) = plasmod_globtau(2) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(3) = plasmod_globtau(3) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(4) = plasmod_globtau(4) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%globtau(5) = plasmod_globtau(5) !tauparticle/tauE for D, T, He, Xe, Ar
       comp%fuelmix = 0.5d0 !fuel mix
       comp%c_car = plasmod_c_car !compression factor between div and core: e.g. 10 means there is 10 more Argon concentration in the divertor than in the core
       comp%car = 0. !argon concentration, used if qdivt=0.
       comp%cxe = 0. !xenon concentration, if negative uses Psepplh as criterion
       comp%che = 0. !helium concentration, used if globtau(3)=0.
       
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
       
       inp0%eccdeff=0.3 !CD = this * PCD * TE/NE !not used for now
       inp0%pech=0.d0 !ech power !not used for now
       inp0%pnbi=0.d0 !nbi power
       inp0%qheat=0.d0 !nbi power
       inp0%qcd=0.d0 !nbi power
       inp0%qfus=0.d0 !nbi power
       inp0%spellet=0.d0 !pellet mass in particles of D in 10^19
       inp0%fpellet=0.5d0 !pellet frequency in Hz
       
       comp%psepplh_inf = plasmod_psepplh_inf !Psep/PLH if below this, use nbi
       comp%psepplh_sup = plasmod_psepplh_sup !Psep/PLH if above this, use Xe
       comp%psep_r = plasmod_psep_r !Psep/R max value
       comp%qdivt = plasmod_qdivt !divertor heat flux in MW/m^2, if 0, dont use SOL model
       inp0%V_loop=plasmod_v_loop !target loop voltage. If lower than -1.e5 dont use
       inp0%f_ni=plasmod_f_ni !required fraction of non inductive current, if 0 dont use CD
       inp0%pfus=plasmod_pfus !if 0., not used (otherwise it would be controlled with Pauxheat)
       
       geom%k = kappa !edge elongation
       geom%d = triang !edge triangularity
       geom%ip=plascur/1.d6	
       
       ped%teped=teped !pedestal top temperature
    endif
    
  end subroutine setupPlasmod

  subroutine runPlasmod


  end subroutine runPlasmod

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
    !call ovarrf(outfile,'Pedestal top density (10^19 m^-3)','(ped%neped)', ped%neped)
    call ovarrf(outfile,'Separatrix density (10^19 m^-3)','(ped%nsep)', ped%nsep)
    !call ovarrf(outfile,'Separatrix temperature (keV?)','(ped%tsep)', ped%tsep)

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
