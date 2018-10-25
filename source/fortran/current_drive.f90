! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module current_drive_module

  !+ad_name  current_drive_module
  !+ad_summ  Module containing current drive system routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  cudriv
  !+ad_cont  iternb
  !+ad_cont  cfnbi
  !+ad_cont  sigbeam
  !+ad_cont  cullhy
  !+ad_cont  culecd
  !+ad_cont  culnbi
  !+ad_args  N/A
  !+ad_desc  This module contains routines relevant for calculating the
  !+ad_desc  current drive parameters for a fusion power plant.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  current_drive_variables
  !+ad_call  error_handling
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  profiles_module
  !+ad_hist  17/10/12 PJK Initial version of module
  !+ad_hist  31/10/12 PJK Changed public/private lists
  !+ad_hist  19/06/14 PJK Removed obsolete routines nbeam, ech, lwhymod
  !+ad_hist  26/06/14 PJK Added error handling
  !+ad_hist  25/01/17 JM  Added case 10 for iefrf for user input ECRH
  !+ad_hist  24/10/18 MDK Added case 11 for iefrf for ECRH using Poli model "HARE"
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Import modules !
  !!!!!!!!!!!!!!!!!!
  use iso_c_binding

  use constraint_variables
  use constants
  use current_drive_variables
  use error_handling
  use profiles_module
  use physics_variables
  use process_output
  use heat_transport_variables
  use hare, only:hare_calc

  implicit none

  private
  public :: cudriv, culnbi

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cudriv(outfile,iprint)

    !+ad_name  cudriv
    !+ad_summ  Routine to calculate the current drive power requirements
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the power requirements of the current
    !+ad_desc  drive system, using a choice of models for the current drive
    !+ad_desc  efficiency.
    !+ad_prob  None
    !+ad_call  culecd
    !+ad_call  cullhy
    !+ad_call  culnbi
    !+ad_call  iternb
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_call  ovarrf
    !+ad_call  report_error
    !+ad_hist  22/08/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  23/01/13 PJK Added comment about ignited plasma
    !+ad_hist  11/09/13 PJK Corrected error in NBI calls; ftr replaced by ftritbm
    !+ad_hist  25/09/13 PJK Added nbshield, rtanbeam, rtanmax outputs
    !+ad_hist  27/11/13 PJK Added ohmic power to bigq denominator
    !+ad_hist  06/03/14 PJK Changed gamma units in output to 10^20 A/W-m2
    !+ad_hist  01/05/14 PJK Changed bigq description
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  19/06/14 PJK Imported code from obsolete routines
    !+ad_hisc               nbeam, ech, lwhymod
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  30/06/14 PJK Added error handling
    !+ad_hist  06/10/14 PJK Use global nbshinef instead of local fshine
    !+ad_hist  06/10/14 PJK Added use of forbitloss
    !+ad_hist  06/10/14 PJK Made feffcd usage consistent for all CD methods
    !+ad_hist  22/10/14 PJK Corrected forbitloss usage
    !+ad_hist  01/12/14 PJK Modified pinjmw output description
    !+ad_hist  01/04/15 JM  Implemented MDK's changes to NB power requirements
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    !!!!!!!!!!!!!

    integer, intent(in) :: iprint, outfile

    ! Local variables !
    !!!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: dene20, effnbss, effrfss, gamnb, gamrf, power1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    echpwr = 0.0D0
    pnbeam = 0.0D0
    plhybd = 0.0D0
    cnbeam = 0.0D0
    porbitlossmw = 0.0D0

    ! irfcd |  switch for current drive calculation
    ! = 0   |  turned off
    ! = 1   |  turned on
    if (irfcd /= 0) then

       ! put electron density in desired units (10^-20 m-3)
       dene20 = dene * 1.0D-20

       ! Calculate current drive efficiencies in units of Amps/Watt.
       ! iefrf |  switch for current drive efficiency model
       select case (iefrf)

       case (1)  ! Fenstermacher Lower Hybrid model

          effrfss = (0.36D0 * (1.0D0 + (te/25.0D0)**1.16D0)) / &
               (rmajor*dene20) * feffcd
          effcd = effrfss

       case (2)  ! Ion-Cyclotron current drive

          effrfss = 0.63D0 * 0.1D0*ten / (2.0D0 + zeff) / &
               (rmajor*dene20) * feffcd
          effcd = effrfss

       case (3)  ! Fenstermacher Electron Cyclotron Resonance model

          effrfss = 0.21D0 * ten/ (rmajor * dene20 * dlamee) * feffcd
          effcd = effrfss

       case (4)  ! Ehst Lower Hybrid / Fast Wave current drive

          effrfss = te**0.77D0 * (0.034D0 + 0.196D0 * beta) / &
               (rmajor*dene20) * ( 32.0D0/(5.0D0+zeff) + 2.0D0 + &
               (12.0D0*(6.0D0+zeff))/(5.0D0+zeff)/(3.0D0+zeff) + &
               3.76D0/zeff) / 12.507D0 * feffcd
          effcd = effrfss

       case (5)  ! ITER Neutral Beam current drive

          call iternb(effnbss,fpion,nbshinef)
          effnbss = effnbss * feffcd
          effcd = effnbss

       case (6)  ! Culham Lower Hybrid current drive model

          call cullhy(effrfss)
          effrfss = effrfss * feffcd
          effcd = effrfss

       case (7)  ! Culham ECCD model

          call culecd(effrfss)
          effrfss = effrfss * feffcd
          effcd = effrfss

       case (8)  ! Culham Neutral Beam model

          call culnbi(effnbss,fpion,nbshinef)
          effnbss = effnbss * feffcd
          effcd = effnbss

       case (9)  ! Issue #508 Remove RFP option  Oscillating Field CD model

       case (10)  ! ECRH user input gamma

          gamcd = gamma_ecrh
          effrfss = gamcd / (dene20 * rmajor)
          etacd = etaech
          effcd = effrfss

      case (11)  ! ECRH Poli model "HARE"
            ! call hare_calc(dens,bfield,R0,amin,rho,te,zeff,nout)
            call hare_calc(10.5d19,5.66d0, 9.072d2,2.920d2,0.1d0,32.d0, 2.d0, nout)
            ! Temporarily use the reference values for input to hare.
            ! Ignore the output: use the fixed values from the input file.
            gamcd = gamma_ecrh
            effrfss = gamcd / (dene20 * rmajor)
            etacd = etaech
            effcd = effrfss

       case default
          idiags(1) = iefrf
          call report_error(126)

       end select

       ! Compute current drive wall plug and injected powers (MW) and efficiencies
       select case (iefrf)

       case (1,2,4,6)  ! LHCD or ICCD

          !  Injected power
          plhybd = 1.0D-6 * faccd * plascur / effrfss + pheat
          pinjimw = 0.0D0
          pinjemw = plhybd

          !  Wall plug power
          pwplh = plhybd / etalh
          pinjwp = pwplh

          !  Wall plug to injector efficiency
          etacd = etalh

          !  Normalised current drive efficiency gamma
          gamrf = effrfss * (dene20 * rmajor)
          gamcd = gamrf

       case (3,7,10)  ! ECCD

          echpwr = 1.0D-6 * faccd * plascur / effrfss + pheat
          pinjimw = 0.0D0
          pinjemw = echpwr
          echwpow = echpwr / etaech
          pinjwp = echwpow
          etacd = etaech


       case (5,8)  ! NBCD



          ! MDK. See Gitlab issue #248, and scanned note.
          power1 = 1.0D-6 * faccd * plascur / effnbss + pheat

          ! Account for first orbit losses
          ! (power due to particles that are ionised but not thermalised) [MW]:
          ! This includes a second order term in shinethrough*(first orbit loss)
          forbitloss = min(0.999,forbitloss) ! Should never be needed

          if(ipedestal.ne.3)then  ! When not using PLASMOD
             pnbitot = power1 / (1.0D0-forbitloss+forbitloss*nbshinef)
          else
             ! Netural beam power calculated by PLASMOD
             pnbitot = pinjmw / (1.0D0-forbitloss+forbitloss*nbshinef)
          endif

          ! Shinethrough power (atoms that are not ionised) [MW]:
          nbshinemw = pnbitot * nbshinef

          ! First orbit loss
          porbitlossmw = forbitloss * (pnbitot - nbshinemw)

          ! Power deposited
          pinjmw = pnbitot - nbshinemw - porbitlossmw
          pinjimw = pinjmw * fpion
          pinjemw = pinjmw * (1.0D0-fpion)



          pwpnb = pnbitot/etanbi ! neutral beam wall plug power
          pinjwp = pwpnb
          etacd = etanbi
          gamnb = effnbss * (dene20 * rmajor)
          gamcd = gamnb
          cnbeam = 1.0D-3 * (pnbitot*1.0D6) / enbeam !  Neutral beam current (A)

!	  write(*,*) power1,fpion,pwpnb,pinjwp,etacd,gamnb,gamcd,cnbeam

       case (9)  ! OFCD
          ! RFP option removed in PROCESS (issue #508)

       end select

       ! Reset injected power to zero for ignited plasma (fudge)
       if (ignite == 1) then
           pinjwp = 0.0D0
       end if

       ! Total injected power

       pinjmw = pinjemw + pinjimw

       ! Ratio of fusion to input (injection+ohmic) power
       if (abs(pinjmw + porbitlossmw + pohmmw) < 1.0D-6) then
          bigq = 1.0D18
       else
          bigq = powfmw / (pinjmw + porbitlossmw + pohmmw)
       end if

    end if

    ! Output !
    !!!!!!!!!!

    if (iprint == 0) return

    call oheadr(outfile,'Current Drive System')

    if (irfcd == 0) then
       call ocmmnt(outfile,'No current drive used')
       call oblnkl(outfile)
       return
    end if

    select case (iefrf)

    case (1,4,6)
       call ocmmnt(outfile,'Lower Hybrid Current Drive')

    case (2)
       call ocmmnt(outfile,'Ion Cyclotron Current Drive')

    case (3,7)
       call ocmmnt(outfile,'Electron Cyclotron Current Drive')

    case (5,8)
       call ocmmnt(outfile,'Neutral Beam Current Drive')

    case (9)
       ! RFP option removed in PROCESS (issue #508)

    case (10)
       call ocmmnt(outfile,'Electron Cyclotron Current Drive (user input gamma_CD)')

    end select

    call ovarin(outfile,'Current drive efficiency model','(iefrf)',iefrf)

    if (ignite == 1) then
       call ocmmnt(outfile, &
            'Ignited plasma; injected power only used for start-up phase')
    end if

    call oblnkl(outfile)

    if (abs(facoh) > 1.0D-8) then
       call ocmmnt(outfile,'Current is driven by both inductive')
       call ocmmnt(outfile,'and non-inductive means.')
    end if

    call ovarre(outfile,'Auxiliary power used for plasma heating only (MW)', '(pheat)', pheat)
    call ovarre(outfile,'Fusion gain factor Q','(bigq)',bigq, 'OP ')
    call ovarre(outfile,'Current drive efficiency (A/W)','(effcd)',effcd, 'OP ')
    call ovarre(outfile,'Normalised current drive efficiency, gamma (10^20 A/W-m2)', &
         '(gamcd)',gamcd, 'OP ')
    call ovarre(outfile,'Wall plug to injector efficiency','(etacd)',etacd)

    call osubhd(outfile,'Fractions of current drive :')
    call ovarrf(outfile,'Bootstrap fraction','(bootipf)',bootipf, 'OP ')
    call ovarrf(outfile,'Auxiliary current drive fraction','(faccd)',faccd, 'OP ')
    call ovarrf(outfile,'Inductive fraction','(facoh)',facoh, 'OP ')
    ! Add total error check.
    call ovarrf(outfile,'Total','(bootipf+faccd+facoh)',bootipf+faccd+facoh)
    if (abs(bootipf+faccd+facoh-1.0d0) > 1.0d-8) then
        call ocmmnt(outfile,'ERROR: current drive fractions do not add to 1')
    end if
    ! MDK Add fvsbrnni as it can be an iteration variable
    call ovarrf(outfile,'Fraction of the plasma current produced by non-inductive means','(fvsbrnni)',fvsbrnni)

    if (abs(bootipf-bscfmax) < 1.0D-8) then
       call ocmmnt(outfile,'Warning : bootstrap current fraction is at')
       call ocmmnt(outfile,'          its prescribed maximum.')
    end if

    call oblnkl(outfile)

    if (abs(plhybd) > 1.0D-8) then
       call ovarre(outfile,'RF efficiency (A/W)','(effrfss)',effrfss, 'OP ')
       call ovarre(outfile,'RF gamma (10^20 A/W-m2)','(gamrf)',gamrf, 'OP ')
       call ovarre(outfile,'Lower hybrid injected power (MW)','(plhybd)',plhybd, 'OP ')
       call ovarre(outfile,'Lower hybrid wall plug efficiency','(etalh)',etalh)
       call ovarre(outfile,'Lower hybrid wall plug power (MW)','(pwplh)',pwplh, 'OP ')
    end if

    ! MDK rearranged and added nbshinemw
    !if (abs(pnbeam) > 1.0D-8) then
    if ((iefrf == 5).or.(iefrf== 8)) then
       call ovarre(outfile,'Neutral beam energy (keV)','(enbeam)',enbeam)
       call ovarre(outfile,'Neutral beam current (A)','(cnbeam)',cnbeam, 'OP ')
       call ovarre(outfile,'Beam efficiency (A/W)','(effnbss)',effnbss, 'OP ')
       call ovarre(outfile,'Beam gamma (10^20 A/W-m2)','(gamnb)',gamnb, 'OP ')
       call ovarre(outfile,'Neutral beam wall plug efficiency','(etanbi)',etanbi)
       call ovarre(outfile,'Beam decay lengths to centre','(taubeam)',taubeam, 'OP ')
       call ovarre(outfile,'Beam shine-through fraction','(nbshinef)',nbshinef, 'OP ')
       call ovarre(outfile,'Neutral beam wall plug power (MW)','(pwpnb)',pwpnb, 'OP ')

       call oblnkl(outfile)
       call ocmmnt(outfile,'Neutral beam power balance :')
       call ocmmnt(outfile,'----------------------------')
       call ovarrf(outfile,'Beam first orbit loss power (MW)','(porbitlossmw)', porbitlossmw, 'OP ')
       call ovarrf(outfile,'Beam shine-through power [MW]','(nbshinemw)',nbshinemw, 'OP ')
       call ovarrf(outfile,'Beam power deposited in plasma (MW)','(pinjmw)',pinjmw, 'OP ')
       call ovarrf(outfile,'Maximum allowable beam power (MW)','(pinjalw)',pinjalw)
       call ovarrf(outfile,'Total (MW)', &
                           '(porbitlossmw+nbshinemw+pinjmw)',porbitlossmw+nbshinemw+pinjmw)
       call oblnkl(outfile)
       call ovarrf(outfile,'Beam power entering vacuum vessel (MW)','(pnbitot)',pnbitot, 'OP ')
       call oblnkl(outfile)

       call ovarre(outfile,'Fraction of beam energy to ions','(fpion)',fpion, 'OP ')
       call ovarre(outfile,'Beam duct shielding thickness (m)','(nbshield)',nbshield)
       call ovarre(outfile,'Beam tangency radius / Plasma major radius','(frbeam)',frbeam)
       call ovarre(outfile,'Beam centreline tangency radius (m)','(rtanbeam)', rtanbeam, 'OP ')
       call ovarre(outfile,'Maximum possible tangency radius (m)','(rtanmax)', rtanmax, 'OP ')
    end if

    if (abs(echpwr) > 1.0D-8) then
       call ovarre(outfile,'Electron cyclotron injected power (MW)','(echpwr)',echpwr, 'OP ')
       call ovarrf(outfile,'Maximum allowable ECRH power (MW)','(pinjalw)',pinjalw)
       call ovarre(outfile,'ECH wall plug efficiency','(etaech)',etaech)
       call ovarre(outfile,'ECH wall plug power (MW)','(echwpow)',echwpow, 'OP ')
    end if

  end subroutine cudriv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine iternb(effnbss,fpion,fshine) bind(C, name="current_drive_iternb")

    !+ad_name  iternb
    !+ad_summ  Routine to calculate ITER Neutral Beam current drive parameters
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  etanb
    !+ad_args  effnbss : output real : neutral beam current drive efficiency (A/W)
    !+ad_args  fpion   : output real : fraction of NB power given to ions
    !+ad_args  fshine  : output real : shine-through fraction of beam
    !+ad_desc  This routine calculates the current drive parameters for a
    !+ad_desc  neutral beam system, based on the 1990 ITER model.
    !+ad_prob  None
    !+ad_call  cfnbi
    !+ad_call  etanb
    !+ad_call  report_error
    !+ad_call  sigbeam
    !+ad_hist  15/06/92 PJK Initial upgraded version
    !+ad_hist  22/08/12 PJK Initial F90 version
    !+ad_hist  19/06/13 PJK Corrected dpath calculation
    !+ad_hist  03/07/13 PJK Changed zeffai description
    !+ad_hist  24/02/14 PJK Rationalised arguments
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  01/09/14 PJK Set fshine to zero if it is negligible
    !+ad_hist  06/10/14 PJK Set fshine to 1.0e-20 if it is negligible
    !+ad_hist  06/10/14 PJK Moved feffcd usage to outside of this routine
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
    !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    !!!!!!!!!!!!!

    real(kind(1.0D0)), intent(out) :: effnbss,fpion,fshine

    ! Local variables !
    !!!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) :: dend,dent,dpath,sigstop

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Check argument sanity
    if ((1.0D0+eps) < frbeam) then
       fdiags(1) = eps ; fdiags(2) = frbeam
       call report_error(15)
    end if

    ! Calculate beam path length to centre
    dpath = rmajor * sqrt( (1.0D0 + eps)**2 - frbeam**2)

    ! Calculate beam stopping cross-section
    sigstop = sigbeam(enbeam/abeam,te,dene,ralpne,rncne,rnone,rnfene)

    ! Calculate number of decay lengths to centre
    taubeam = dpath * dene * sigstop

    ! Shine-through fraction of beam
    fshine = exp(-2.0D0 * dpath*dene*sigstop)
    fshine = max(fshine, 1.0D-20)

    ! Deuterium and tritium beam densities
    dend = deni * (1.0D0-ftritbm)
    dent = deni * ftritbm

    ! Power split to ions / electrons
    call cfnbi(abeam,enbeam,ten,dene,dend,dent,zeffai,dlamie,fpion)

    ! Current drive efficiency
    effnbss = frbeam * &
         etanb(abeam,alphan,alphat,aspect,dene,enbeam,rmajor,ten,zeff)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function etanb(abeam,alphan,alphat,aspect,dene,ebeam,rmajor,ten,zeff)

      !+ad_name  etanb
      !+ad_summ  Routine to find neutral beam current drive efficiency
      !+ad_summ  using the ITER 1990 formulation
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  abeam   : input real : beam ion mass (amu)
      !+ad_args  alphan  : input real : density profile factor
      !+ad_args  alphat  : input real : temperature profile factor
      !+ad_args  aspect  : input real : aspect ratio
      !+ad_args  dene    : input real : volume averaged electron density (m**-3)
      !+ad_args  enbeam  : input real : neutral beam energy (keV)
      !+ad_args  rmajor  : input real : plasma major radius (m)
      !+ad_args  ten     : input real : density weighted average electron temp. (keV)
      !+ad_args  zeff    : input real : plasma effective charge
      !+ad_desc  This routine calculates the current drive efficiency of
      !+ad_desc  a neutral beam system, based on the 1990 ITER model.
      !+ad_prob  No account is taken of pedestal profiles, and the shine-through
      !+ad_prob  power fraction is ignored. The improved version of this function,
      !+ad_prob  <CODE>etanb2</CODE>, is more appropriate for non-ITER plasma sizes
      !+ad_prob  and densities.
      !+ad_call  None
      !+ad_hist  15/06/92 PJK Initial upgraded version
      !+ad_hist  22/08/12 PJK Initial F90 version
      !+ad_hist  07/10/14 PJK Added comments about etanb2
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
      !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: etanb

      ! Arguments !
      !!!!!!!!!!!!!

      real(kind(1.0D0)), intent(in) :: abeam,alphan,alphat,aspect,dene, &
           ebeam,rmajor,ten,zeff

      ! Local variables !
      !!!!!!!!!!!!!!!!!!!

      real(kind(1.0D0)) :: abd,bbd,dene20,dum,epseff,ffac,gfac,rjfunc, &
           xj,xjs,yj,zbeam

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ! TODO comment this subroutine.

      zbeam = 1.0D0
      bbd = 1.0D0

      dene20 = 1.0D-20*dene

      ! Ratio of E_beam/E_crit
      xjs = ebeam / (bbd*10.0D0*abeam*ten)
      xj = sqrt(xjs)

      yj = 0.8D0 * zeff/abeam

      rjfunc = xjs / (4.0D0 + 3.0D0*yj + xjs * &
           (xj + 1.39D0 + 0.61D0*yj**0.7D0))

      epseff = 0.5D0/aspect
      gfac = (1.55D0 + 0.85D0/zeff)*sqrt(epseff) - &
           (0.2D0 + 1.55D0/zeff)*epseff
      ffac = 1.0D0/zbeam - (1.0D0 - gfac)/zeff

      abd = 0.107D0 * (1.0D0 - 0.35D0*alphan + 0.14D0*alphan**2) * &
           (1.0D0 - 0.21D0*alphat) * (1.0D0 - 0.2D-3*ebeam  + &
           0.09D-6 * ebeam**2)
      dum = abd *(5.0D0/rmajor) * (0.1D0*ten/dene20) * rjfunc/0.2D0 * ffac

      etanb = dum

    end function etanb

  end subroutine iternb

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cfnbi(afast,efast,te,ne,nd,nt,zeffai,xlmbda,fpion) bind(C, name="current_drive_cfnbi")

    !+ad_name  cfnbi
    !+ad_summ  Routine to calculate the fraction of the fast particle energy
    !+ad_summ  coupled to the ions
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  xlmbdabi
    !+ad_args  afast   : input real : mass of fast particle (units of proton mass)
    !+ad_args  efast   : input real : energy of fast particle (keV)
    !+ad_args  te      : input real : density weighted average electron temp. (keV)
    !+ad_args  ne      : input real : volume averaged electron density (m**-3)
    !+ad_args  nd      : input real : deuterium beam density (m**-3)
    !+ad_args  nt      : input real : tritium beam density (m**-3)
    !+ad_args  zeffai  : input real : mass weighted plasma effective charge
    !+ad_args  xlmbda  : input real : ion-electron coulomb logarithm
    !+ad_args  fpion   : output real : fraction of fast particle energy coupled to ions
    !+ad_desc  This routine calculates the fast particle energy coupled to
    !+ad_desc  the ions in the neutral beam system.
    !+ad_prob  None
    !+ad_call  xlmbdabi
    !+ad_hist  15/06/92 PJK Initial upgraded version
    !+ad_hist  22/08/12 PJK Initial F90 version
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  03/07/13 PJK Changed zeffai description
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: afast,efast,te,ne,nd,nt,zeffai,xlmbda
    real(kind(1.0D0)), intent(out) :: fpion

    !  Local variables

    real(kind(1.0D0)) :: ans,ecritfi,ecritfix,sum,sumln,thx,t1,t2,ve,x, &
         xlbd,xlbt,xlmbdai,xlnrat

    real(kind(1.0D0)), parameter :: atmd = 2.0D0
    real(kind(1.0D0)), parameter :: atmdt = 2.5D0
    real(kind(1.0D0)), parameter :: atmt = 3.0D0
    real(kind(1.0D0)), parameter :: c = 3.0D8
    real(kind(1.0D0)), parameter :: me = 9.1D-31
    real(kind(1.0D0)), parameter :: zd = 1.0D0
    real(kind(1.0D0)), parameter :: zt = 1.0D0

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    xlbd = xlmbdabi(afast,atmd,efast,te,ne)
    xlbt = xlmbdabi(afast,atmt,efast,te,ne)

    sum = nd*zd*zd * xlbd/atmd + nt*zt*zt * xlbt/atmt
    ecritfix = 16.0D0 * te * afast * (sum/(ne*xlmbda))**(2.0D0/3.0D0)

    xlmbdai = xlmbdabi(afast,atmdt,efast,te,ne)
    sumln = zeffai * xlmbdai/xlmbda
    xlnrat = (3.0D0*sqrt(pi)/4.0D0 * me/mproton * sumln)**(2.0D0/3.0D0)
    ve = c * sqrt(2.0D0*te/511.0D0)

    ecritfi = afast * mproton * ve*ve * xlnrat/(2.0D0 * echarge * 1.0D3)

    x = sqrt(efast/ecritfi)
    t1 = log( (x*x - x + 1.0D0) / ((x + 1.0D0)**2) )
    thx = (2.0D0*x - 1.0D0)/sqrt(3.0D0)
    t2 = 2.0D0*sqrt(3.0D0) *(atan(thx) + pi/6.0D0)

    ans = (t1 + t2) / (3.0D0 * x*x)
    fpion = ans

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function xlmbdabi(mb,mth,eb,t,nelec)

      !+ad_name  xlmbdabi
      !+ad_summ  Calculates the Coulomb logarithm for ion-ion collisions
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  mb     : input real : mass of fast particle (units of proton mass)
      !+ad_args  mth    : input real : mass of background ions (units of proton mass)
      !+ad_args  eb     : input real : energy of fast particle (keV)
      !+ad_args  t      : input real : density weighted average electron temp. (keV)
      !+ad_args  nelec  : input real : volume averaged electron density (m**-3)
      !+ad_desc  This function calculates the Coulomb logarithm for ion-ion
      !+ad_desc  collisions where the relative velocity may be large compared
      !+ad_desc  with the background ('mt') thermal velocity.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  15/06/92 PJK Initial upgraded version
      !+ad_hist  22/08/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Mikkelson and Singer, Nuc Tech/Fus, 4, 237 (1983)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: xlmbdabi

      !  Arguments

      real(kind(1.0D0)), intent(in) :: mb,mth,eb,t,nelec

      !  Local variables

      real(kind(1.0D0)) :: ans,x1,x2

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      x1 = (t/10.0D0) * (eb/1000.0D0) * mb/(nelec/1.0D20)
      x2 = mth/(mth + mb)

      ans = 23.7D0 + log(x2 * sqrt(x1))
      xlmbdabi = ans

    end function xlmbdabi

  end subroutine cfnbi

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function sigbeam(eb,te,ne,rnhe,rnc,rno,rnfe)

    !+ad_name  sigbeam
    !+ad_summ  Calculates the stopping cross-section for a hydrogen
    !+ad_summ  beam in a fusion plasma
    !+ad_type  Function returning real
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  eb     : input real : beam energy (kev/amu)
    !+ad_args  te     : input real : electron temperature (keV)
    !+ad_args  ne     : input real : electron density (10^20m-3)
    !+ad_args  rnhe   : input real : alpha density / ne
    !+ad_args  rnc    : input real : carbon density /ne
    !+ad_args  rno    : input real : oxygen density /ne
    !+ad_args  rnfe   : input real : iron density /ne
    !+ad_desc  This function calculates the stopping cross-section (m^-2)
    !+ad_desc  for a hydrogen beam in a fusion plasma.
    !+ad_prob  The model does not take into account impurities other than
    !+ad_prob  carbon, oxygen and iron, so the results may be inaccurate
    !+ad_prob  if <CODE>imprad_model = 1</CODE> and additional impurities
    !+ad_prob  are present.
    !+ad_call  None
    !+ad_hist  15/06/92 PJK Initial upgraded version
    !+ad_hist  22/08/12 PJK Initial F90 version
    !+ad_hist  24/06/14 PJK Added comment about imprad_model
    !+ad_stat  Okay
    !+ad_docs  Janev, Boley and Post, Nuclear Fusion 29 (1989) 2125
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: sigbeam

    !  Arguments

    real(kind(1.0D0)), intent(in) :: eb,te,ne,rnhe,rnc,rno,rnfe

    !  Local variables

    real(kind(1.0D0)) :: ans,nen,sz,s1
    real(kind(1.0D0)), dimension(2,3,2) :: a
    real(kind(1.0D0)), dimension(3,2,2,4) :: b
    real(kind(1.0D0)), dimension(4) :: nn,z

    integer :: i,is,j,k

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    data a/ 4.40D0, 2.30D-1, 7.46D-2,-2.55D-3, 3.16D-3, 1.32D-3, &
         -2.49D-2,-1.15D-2, 2.27D-3,-6.20D-4,-2.78D-5, 3.38D-5/

    data b/ &
         -2.36D0, 8.49D-1,-5.88D-2,-2.50D-1, 6.77D-2,-4.48D-3, &
         1.85D-1,-4.78D-2, 4.34D-3,-3.81D-2, 1.05D-2,-6.76D-4, &
         -1.49D0, 5.18D-1,-3.36D-2,-1.19D-1, 2.92D-2,-1.79D-3, &
         -1.54D-2, 7.18D-3, 3.41D-4,-1.50D-2, 3.66D-3,-2.04D-4, &
         -1.41D0, 4.77D-1,-3.05D-2,-1.08D-1, 2.59D-2,-1.57D-3, &
         -4.08D-4, 1.57D-3, 7.35D-4,-1.38D-2, 3.33D-3,-1.86D-4, &
         -1.03D0, 3.22D-1,-1.87D-2,-5.58D-2, 1.24D-2,-7.43D-4, &
         1.06D-1,-3.75D-2, 3.53D-3,-3.72D-3, 8.61D-4,-5.12D-5/

    z(1) = 2.0D0 ; z(2) = 6.0D0 ; z(3) = 8.0D0 ; z(4) = 26.0D0
    nn(1) = rnhe ; nn(2) = rnc ; nn(3) = rno ; nn(4) = rnfe

    nen = ne/1.0D19

    s1 = 0.0D0
    do k = 1,2
       do j = 1,3
          do i = 1,2
             s1 = s1 + a(i,j,k) * (log(eb))**(i-1) * &
                  (log(nen))**(j-1) * (log(te))**(k-1)
          end do
       end do
    end do

    !  Impurity term

    sz = 0.0D0
    do is = 1,4
       do k = 1,2
          do j = 1,2
             do i = 1,3
                sz = sz + b(i,j,k,is)* (log(eb))**(i-1) * &
                     (log(nen))**(j-1) * (log(te))**(k-1) * &
                     nn(is) * z(is) * (z(is)-1.0D0)
             end do
          end do
       end do
    end do

    ans = 1.0D-20 * ( exp(s1)/eb * (1.0D0 + sz) )

    sigbeam = max(ans,1.0D-23)

  end function sigbeam

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine cullhy(effrfss)

    !+ad_name  cullhy
    !+ad_summ  Routine to calculate Lower Hybrid current drive efficiency
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  lhrad
    !+ad_cont  lheval
    !+ad_args  effrfss : output real : lower hybrid current drive efficiency (A/W)
    !+ad_desc  This routine calculates the current drive parameters for a
    !+ad_desc  lower hybrid system, based on the AEA FUS 172 model.
    !+ad_prob  None
    !+ad_call  lhrad
    !+ad_call  nprofile
    !+ad_call  report_error
    !+ad_call  tprofile
    !+ad_hist  15/06/92 PJK Initial upgraded version
    !+ad_hist  22/08/12 PJK Initial F90 version
    !+ad_hist  24/02/14 PJK Local density and temperature calculated using
    !+ad_hisc               relevant profile model; rationalised arguments
    !+ad_hist  30/06/14 PJK Added error_handling
    !+ad_hist  06/10/14 PJK Moved feffcd usage to outside of this routine
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: effrfss

    !  Local variables

    real(kind(1.0D0)) :: blocal,dlocal,epslh,frac,gamlh,nplacc,rpenet, &
         rratio,term01,term02,term03,term04,tlocal,x

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate the penetration radius of the LH waves

    call lhrad(rratio)
    rpenet = rratio*rminor

    !  Local density, temperature, toroidal field at this minor radius

    dlocal = 1.0D-19 * nprofile(rratio,rhopedn,ne0,neped,nesep,alphan)
    tlocal = tprofile(rratio,rhopedt,te0,teped,tesep,alphat,tbeta)
    blocal = bt*rmajor/(rmajor-rpenet)  !  Calculated on inboard side

    !  Parallel refractive index needed for plasma access

    frac = sqrt(dlocal)/blocal
    nplacc = frac + sqrt(1.0D0 + frac*frac)

    !  Local inverse aspect ratio

    epslh = rpenet/rmajor

    !  LH normalised efficiency (A/W m**-2)

    x = 24.0D0 / (nplacc*sqrt(tlocal))

    term01 = 6.1D0 / (nplacc*nplacc * (zeff+5.0D0))
    term02 = 1.0D0 + (tlocal/25.0D0)**1.16D0
    term03 = epslh**0.77D0 * sqrt(12.25D0 + x*x)
    term04 = 3.5D0*epslh**0.77D0 + x

    if (term03 > term04) then
       fdiags(1) = term03 ; fdiags(2) = term04
       call report_error(129)
    end if

    gamlh = term01 * term02 * (1.0D0 - term03/term04)

    !  Current drive efficiency (A/W)

    effrfss = gamlh / ((0.1D0*dlocal)*rmajor)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine lhrad(rratio)

      !+ad_name  lhrad
      !+ad_summ  Routine to calculate Lower Hybrid wave absorption radius
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  rratio  : output real : minor radius of penetration / rminor
      !+ad_desc  This routine determines numerically the minor radius at which the
      !+ad_desc  damping of Lower Hybrid waves occurs, using a Newton-Raphson method.
      !+ad_prob  None
      !+ad_call  lheval
      !+ad_call  report_error
      !+ad_hist  15/06/92 PJK Initial upgraded version
      !+ad_hist  18/09/12 PJK Initial F90 version
      !+ad_hist  24/02/14 PJK Rationalised argument list
      !+ad_hist  26/06/14 PJK Added error handling
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(out) :: rratio

      !  Local variables

      real(kind(1.0D0)) :: dgdr,drfind,g0,g1,g2,rat0,rat1,r1,r2
      integer :: lapno
      integer, parameter :: maxlap = 100

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Correction to refractive index (kept within valid bounds)

      drfind = min(0.7D0, max(0.1D0,12.5D0/te0))

      !  Use Newton-Raphson method to establish the correct minor radius
      !  ratio. g is calculated as a function of r / r_minor, where g is
      !  the difference between the results of the two formulae for the
      !  energy E given in AEA FUS 172, p.58. The required minor radius
      !  ratio has been found when g is sufficiently close to zero.

      !  Initial guess for the minor radius ratio

      rat0 = 0.8D0

      lapno = 0
      do ; lapno = lapno+1

         !  Minor radius ratios either side of the latest guess

         r1 = rat0 - 1.0D-3*rat0
         r2 = rat0 + 1.0D-3*rat0

         !  Evaluate g at rat0, r1, r2

         call lheval(drfind,rat0,g0)
         call lheval(drfind,r1,g1)
         call lheval(drfind,r2,g2)

         !  Calculate gradient of g with respect to minor radius ratio

         dgdr = (g2-g1)/(r2-r1)

         !  New approximation

         rat1 = rat0 - g0/dgdr

         !  Force this approximation to lie within bounds

         rat1 = max(0.0001D0,rat1)
         rat1 = min(0.9999D0,rat1)

         !  Check the number of laps for convergence

         if (lapno >= maxlap) then
            idiags(1) = lapno ; call report_error(16)
            rat0 = 0.8D0
            exit
         end if

         !  Is g sufficiently close to zero?

         if (abs(g0) > 0.01D0) then
            !  No, so go around loop again
            rat0 = rat1
         else
            exit
         end if

      end do

      rratio = rat0

    end subroutine lhrad

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine lheval(drfind,rratio,ediff)

      !+ad_name  lheval
      !+ad_summ  Routine to evaluate the difference between electron energy
      !+ad_summ  expressions required to find the Lower Hybrid absorption radius
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  drfind  : input real : correction to parallel refractive index
      !+ad_args  rratio  : input real : guess for radius of penetration / rminor
      !+ad_args  ediff   : output real : difference between the E values (keV)
      !+ad_desc  This routine evaluates the difference between the values calculated
      !+ad_desc  from the two equations for the electron energy E, given in
      !+ad_desc  AEA FUS 172, p.58. This difference is used to locate the Lower Hybrid
      !+ad_desc  wave absorption radius via a Newton-Raphson method, in calling
      !+ad_desc  routine <A HREF="lhrad.html">lhrad</A>.
      !+ad_prob  None
      !+ad_call  nprofile
      !+ad_call  tprofile
      !+ad_hist  15/06/92 PJK Initial upgraded version
      !+ad_hist  18/09/12 PJK Initial F90 version
      !+ad_hist  24/02/14 PJK Rationalised argument list, and called profile
      !+ad_hisc               routines to calculate local quantities
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: drfind,rratio
      real(kind(1.0D0)), intent(out) :: ediff

      !  Local variables

      real(kind(1.0D0)) :: blocal,dlocal,e1,e2,frac,nplacc,refind,tlocal

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Local electron density

      dlocal = 1.0D-19 * nprofile(rratio,rhopedn,ne0,neped,nesep,alphan)

      !  Local electron temperature

      tlocal = tprofile(rratio,rhopedt,te0,teped,tesep,alphat,tbeta)

      !  Local toroidal field (evaluated at the inboard region of the flux surface)

      blocal = bt * rmajor/(rmajor - rratio*rminor)

      !  Parallel refractive index needed for plasma access

      frac = sqrt(dlocal)/blocal
      nplacc = frac + sqrt(1.0D0 + frac*frac)

      !  Total parallel refractive index

      refind = nplacc + drfind

      !  First equation for electron energy E

      e1 = 511.0D0 * (sqrt(1.0D0 + 1.0D0/(refind*refind)) - 1.0D0)

      !  Second equation for E

      e2 = 7.0D0 * tlocal

      !  Difference

      ediff = e1-e2

    end subroutine lheval

  end subroutine cullhy

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine culecd(effrfss)

    !+ad_name  culecd
    !+ad_summ  Routine to calculate Electron Cyclotron current drive efficiency
    !+ad_type  Subroutine
    !+ad_auth  M R O'Brien, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  eccdef
    !+ad_cont  legend
    !+ad_args  effrfss : output real : electron cyclotron current drive efficiency (A/W)
    !+ad_desc  This routine calculates the current drive parameters for a
    !+ad_desc  electron cyclotron system, based on the AEA FUS 172 model.
    !+ad_prob  None
    !+ad_call  eccdef
    !+ad_call  nprofile
    !+ad_call  tprofile
    !+ad_hist  16/06/92 PJK Initial upgraded version
    !+ad_hist  18/09/12 PJK Initial F90 version
    !+ad_hist  24/02/14 PJK Rationalised argument list, and called profile
    !+ad_hisc               routines to calculate local quantities
    !+ad_hist  06/10/14 PJK Moved feffcd usage to outside of this routine
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: effrfss

    !  Local variables

    real(kind(1.0D0)) :: cosang,coulog,dlocal,ecgam,ecgam1,ecgam2,ecgam3,ecgam4, &
         epsloc,rrr,tlocal,zlocal

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Local plasma parameters : take r = a/3

    rrr = 1.0D0/3.0D0

    !  Temperature

    tlocal = tprofile(rrr,rhopedt,te0,teped,tesep,alphat,tbeta)

    !  Density (10**20 m**-3)

    dlocal = 1.0D-20 * nprofile(rrr,rhopedn,ne0,neped,nesep,alphan)

    !  Inverse aspect ratio

    epsloc = rrr * rminor/rmajor

    !  Effective charge (use average value)

    zlocal = zeff

    !  Coulomb logarithm for ion-electron collisions
    !  (From J. A. Wesson, 'Tokamaks', Clarendon Press, Oxford, p.293)

    coulog = 15.2D0 - 0.5D0*log(dlocal) + log(tlocal)

    !  Calculate normalised current drive efficiency at four different
    !  poloidal angles, and average.
    !  cosang = cosine of the poloidal angle at which ECCD takes place
    !         = +1 outside, -1 inside.

    cosang =  1.0D0 ; call eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam1)
    cosang =  0.5D0 ; call eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam2)
    cosang = -0.5D0 ; call eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam3)
    cosang = -1.0D0 ; call eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam4)

    !  Normalised current drive efficiency (A/W m**-2)

    ecgam = 0.25D0 * (ecgam1+ecgam2+ecgam3+ecgam4)

    !  Current drive efficiency (A/W)

    effrfss = ecgam/(dlocal*rmajor)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine eccdef(tlocal,epsloc,zlocal,cosang,coulog,ecgam)

      !+ad_name  eccdef
      !+ad_summ  Routine to calculate Electron Cyclotron current drive efficiency
      !+ad_type  Subroutine
      !+ad_auth  M R O'Brien, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  tlocal : input real : local electron temperature (keV)
      !+ad_args  epsloc : input real : local inverse aspect ratio
      !+ad_args  zlocal : input real : local plasma effective charge
      !+ad_args  cosang : input real : cosine of the poloidal angle at which ECCD takes
      !+ad_argc                        place (+1 outside, -1 inside)
      !+ad_args  coulog : input real : local coulomb logarithm for ion-electron collisions
      !+ad_args  ecgam  : output real : normalised current drive efficiency (A/W m**-2)
      !+ad_desc  This routine calculates the current drive parameters for a
      !+ad_desc  electron cyclotron system, based on the AEA FUS 172 model.
      !+ad_desc  It works out the ECCD efficiency using the formula due to Cohen
      !+ad_desc  quoted in the ITER Physics Design Guidelines : 1989
      !+ad_desc  (but including division by the Coulomb Logarithm omitted from
      !+ad_desc  IPDG89). We have assumed gamma**2-1 << 1, where gamma is the
      !+ad_desc  relativistic factor. The notation follows that in IPDG89.
      !+ad_desc  <P>The answer ECGAM is the normalised efficiency nIR/P with n the
      !+ad_desc  local density in 10**20 /m**3, I the driven current in MAmps,
      !+ad_desc  R the major radius in metres, and P the absorbed power in MWatts.
      !+ad_prob  None
      !+ad_call  legend
      !+ad_call  report_error
      !+ad_hist  16/08/91 MOB Initial version
      !+ad_hist  16/06/92 PJK Initial upgraded version
      !+ad_hist  18/09/12 PJK Initial F90 version
      !+ad_hist  26/06/14 PJK Added error handling
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
      !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
      !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: tlocal,epsloc,zlocal,cosang,coulog
      real(kind(1.0D0)), intent(out) :: ecgam

      !  Local variables

      real(kind(1.0D0)) :: f,facm,fp,h,hp,lam,lams,mcsq,palpha,palphap,palphaps, &
           palphas,y

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      mcsq = 9.1095D-31 * 2.9979D8**2 /(1.0D3*1.6022D-19) !  keV
      f = 16.0D0 * (tlocal/mcsq)**2

      !  fp is the derivative of f with respect to gamma, the relativistic
      !  factor, taken equal to 1 + 2T/(m c**2)

      fp = 16.0D0 * tlocal/mcsq

      !  lam is IPDG89's lambda. LEGEND calculates the Legendre function of
      !  order alpha and argument lam, palpha, and its derivative, palphap.
      !  Here alpha satisfies alpha(alpha+1) = -8/(1+zlocal). alpha is of the
      !  form  (-1/2 + ix), with x a real number and i = sqrt(-1).

      lam = 1.0D0
      call legend(zlocal,lam,palpha,palphap)

      lams = sqrt(2.0D0*epsloc/(1.0D0+epsloc))
      call legend(zlocal,lams,palphas,palphaps)

      !  hp is the derivative of IPDG89's h function with respect to lam

      h = -4.0D0 * lam/(zlocal+5.0D0) * (1.0D0-lams*palpha/(lam*palphas))
      hp = -4.0D0 / (zlocal+5.0D0) * (1.0D0-lams*palphap/palphas)

      !  facm is IPDG89's momentum conserving factor

      facm = 1.5D0
      y = mcsq/(2.0D0*tlocal) * (1.0D0 + epsloc*cosang)

      !  We take the negative of the IPDG89 expression to get a positive
      !  number

      ecgam = -7.8D0 * facm * sqrt((1.0D0+epsloc)/(1.0D0-epsloc)) / coulog &
           * (h*fp - 0.5D0*y*f*hp)

      if (ecgam < 0.0D0) call report_error(17)

    end subroutine eccdef

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine legend(zlocal,arg,palpha,palphap)

      !+ad_name  legend
      !+ad_summ  Routine to calculate Legendre function and its derivative
      !+ad_type  Subroutine
      !+ad_auth  M R O'Brien, CCFE, Culham Science Centre
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  zlocal  : input real : local plasma effective charge
      !+ad_args  arg     : input real : argument of Legendre function
      !+ad_args  palpha  : output real : value of Legendre function
      !+ad_args  palphap : output real : derivative of Legendre function
      !+ad_desc  This routine calculates the Legendre function <CODE>palpha</CODE>
      !+ad_desc  of argument <CODE>arg</CODE> and order
      !+ad_desc  <CODE>alpha = -0.5 + i sqrt(xisq)</CODE>,
      !+ad_desc  and its derivative <CODE>palphap</CODE>.
      !+ad_desc  <P>This Legendre function is a conical function and we use the series
      !+ad_desc  in <CODE>xisq</CODE> given in Abramowitz and Stegun. The
      !+ad_desc  derivative is calculated from the derivative of this series.
      !+ad_desc  <P>The derivatives were checked by calculating <CODE>palpha</CODE> for
      !+ad_desc  neighbouring arguments. The calculation of <CODE>palpha</CODE> for zero
      !+ad_desc  argument was checked by comparison with the expression
      !+ad_desc  <CODE>palpha(0) = 1/sqrt(pi) * cos(pi*alpha/2) * gam1 / gam2</CODE>
      !+ad_desc  (Abramowitz and Stegun, eqn 8.6.1). Here <CODE>gam1</CODE> and
      !+ad_desc  <CODE>gam2</CODE> are the Gamma functions of arguments
      !+ad_desc  <CODE>0.5*(1+alpha)</CODE> and <CODE>0.5*(2+alpha)</CODE> respectively.
      !+ad_prob  None
      !+ad_call  report_error
      !+ad_hist  16/08/91 MOB Initial version
      !+ad_hist  16/06/92 PJK Initial upgraded version
      !+ad_hist  18/09/12 PJK Initial F90 version
      !+ad_hist  26/06/14 PJK Added error handling
      !+ad_stat  Okay
      !+ad_docs  Abramowitz and Stegun, equation 8.12.1
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: zlocal,arg
      real(kind(1.0D0)), intent(out) ::  palpha,palphap

      !  Local variables

      real(kind(1.0D0)) :: arg2,pold,poldp,pterm,sinsq,term1,term2,xisq
      integer :: n

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Check for invalid argument

      if (abs(arg) > (1.0D0+1.0D-10)) then
         fdiags(1) = arg ; call report_error(18)
      end if

      arg2 = min(arg, (1.0D0-1.0D-10))
      sinsq = 0.5D0*(1.0D0-arg2)
      xisq = 0.25D0*(32.0D0*zlocal/(zlocal+1.0D0) - 1.0D0)
      palpha = 1.0D0
      pold = 1.0D0
      pterm = 1.0D0
      palphap = 0.0D0
      poldp = 0.0D0

      do n = 1,10001

         !  Check for convergence every 20 iterations

         if ((n > 1).and.(mod(n,20) == 1)) then
            term1 = 1.0D-10 * max(abs(pold),abs(palpha))
            term2 = 1.0D-10 * max(abs(poldp),abs(palphap))

            if ( (abs(pold-palpha) < term1) .and. &
                 (abs(poldp-palphap) < term2) ) return

            pold = palpha
            poldp = palphap
         end if

         pterm = pterm * (4.0D0*xisq+(2.0D0*n - 1.0D0)**2) / &
              (2.0D0*n)**2 * sinsq
         palpha = palpha + pterm
         palphap = palphap - n*pterm/(1.0D0-arg2)

      end do

      !  Code will only get this far if convergence has failed

      call report_error(19)

    end subroutine legend

  end subroutine culecd

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine culnbi(effnbss,fpion,fshine)

    !+ad_name  culnbi
    !+ad_summ  Routine to calculate Neutral Beam current drive parameters
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  etanb2
    !+ad_args  effnbss : output real : neutral beam current drive efficiency (A/W)
    !+ad_args  fpion   : output real : fraction of NB power given to ions
    !+ad_args  fshine  : output real : shine-through fraction of beam
    !+ad_desc  This routine calculates Neutral Beam current drive parameters
    !+ad_desc  using the corrections outlined in AEA FUS 172 to the ITER method.
    !+ad_desc  <P>The result cannot be guaranteed for devices with aspect ratios far
    !+ad_desc  from that of ITER (approx. 2.8).
    !+ad_prob  None
    !+ad_call  cfnbi
    !+ad_call  etanb2
    !+ad_call  report_error
    !+ad_call  sigbeam
    !+ad_hist  17/06/92 PJK Initial upgraded version
    !+ad_hist  18/09/12 PJK Initial F90 version
    !+ad_hist  19/06/13 PJK Corrected dpath calculation
    !+ad_hist  03/07/13 PJK Changed zeffai description
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  01/09/14 PJK Set fshine to 1.0e-20 if it is negligible
    !+ad_hist  06/10/14 PJK Moved feffcd usage to outside of this routine
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: effnbss,fpion,fshine

    !  Local variables

    real(kind(1.0D0)) :: dend,dent,dpath,sigstop

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check argument sanity

    if ((1.0D0+eps) < frbeam) then
       fdiags(1) = eps ; fdiags(2) = frbeam
       call report_error(20)
    end if

    !  Calculate beam path length to centre

    dpath = rmajor * sqrt( (1.0D0 + eps)**2 - frbeam**2)

    !  Calculate beam stopping cross-section

    sigstop = sigbeam(enbeam/abeam,te,dene,ralpne,rncne,rnone,rnfene)

    !  Calculate number of decay lengths to centre

    taubeam = dpath * dnla * sigstop

    !  Shine-through fraction of beam

    fshine = exp(-2.0D0 * dpath*dnla*sigstop)
    fshine = max(fshine, 1.0D-20)

    !  Deuterium and tritium beam densities

    dend = deni * (1.0D0-ftritbm)
    dent = deni * ftritbm

    !  Power split to ions / electrons

    call cfnbi(abeam,enbeam,ten,dene,dend,dent,zeffai,dlamie,fpion)

    !  Current drive efficiency

    effnbss = etanb2(abeam,alphan,alphat,aspect,dene,dnla,enbeam, &
         frbeam,fshine,rmajor,rminor,ten,zeff)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function etanb2(abeam,alphan,alphat,aspect,dene,dnla,enbeam,frbeam, &
         fshine,rmajor,rminor,ten,zeff)

      !+ad_name  etanb2
      !+ad_summ  Routine to find neutral beam current drive efficiency
      !+ad_summ  using the ITER 1990 formulation, plus correction terms
      !+ad_summ  outlined in Culham Report AEA FUS 172
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  abeam   : input real : beam ion mass (amu)
      !+ad_args  alphan  : input real : density profile factor
      !+ad_args  alphat  : input real : temperature profile factor
      !+ad_args  aspect  : input real : aspect ratio
      !+ad_args  dene    : input real : volume averaged electron density (m**-3)
      !+ad_args  dnla    : input real : line averaged electron density (m**-3)
      !+ad_args  enbeam  : input real : neutral beam energy (keV)
      !+ad_args  frbeam  : input real : R_tangent / R_major for neutral beam injection
      !+ad_args  fshine  : input real : shine-through fraction of beam
      !+ad_args  rmajor  : input real : plasma major radius (m)
      !+ad_args  rminor  : input real : plasma minor radius (m)
      !+ad_args  ten     : input real : density weighted average electron temperature (keV)
      !+ad_args  zeff    : input real : plasma effective charge
      !+ad_desc  This routine calculates the current drive efficiency in A/W of
      !+ad_desc  a neutral beam system, based on the 1990 ITER model,
      !+ad_desc  plus correction terms outlined in Culham Report AEA FUS 172.
      !+ad_desc  <P>The formulae are from AEA FUS 172, unless denoted by IPDG89.
      !+ad_prob  No account is taken of pedestal profiles.
      !+ad_prob  Although it is taken account of here, fshine should be negligible
      !+ad_prob  otherwise the transmitted NBI power is not accounted for in the
      !+ad_prob  overall power balance.
      !+ad_call  report_error
      !+ad_hist  17/06/92 PJK Initial upgraded version
      !+ad_hist  18/09/12 PJK Initial F90 version
      !+ad_hist  10/10/12 PJK Changed enorm to ebnorm
      !+ad_hist  26/06/14 PJK Added error handling
      !+ad_hist  07/10/14 PJK Moved feffcd usage to outside of this routine
      !+ad_stat  Okay
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
      !+ad_docs  ITER Physics Design Guidelines: 1989 [IPDG89], N. A. Uckan et al,
      !+ad_docc  ITER Documentation Series No.10, IAEA/ITER/DS/10, IAEA, Vienna, 1990
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: etanb2

      !  Arguments

      real(kind(1.0D0)), intent(in) :: abeam,alphan,alphat,aspect,dene,dnla, &
           enbeam,frbeam,fshine,rmajor,rminor,ten,zeff

      !  Local variables

      real(kind(1.0D0)) :: abd,bbd,d,dene20,dnla20,dnorm,ebmev,ebnorm, &
           ecrit,epseff,epsitr,eps1,ffac,gamnb,gfac,j0,nnorm,r,xj, &
           xjs,yj,zbeam

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Charge of beam ions

      zbeam = 1.0D0

      !  Fitting factor (IPDG89)

      bbd = 1.0D0

      !  Volume averaged electron density (10**20 m**-3)

      dene20 = dene/1.0D20

      !  Line averaged electron density (10**20 m**-3)

      dnla20 = dnla/1.0D20

      !  Critical energy (MeV) (power to electrons = power to ions) (IPDG89)
      !  N.B. ten is in keV

      ecrit = 0.01D0 * abeam * ten

      !  Beam energy in MeV

      ebmev = enbeam/1.0D3

      !  x and y coefficients of function J0(x,y) (IPDG89)

      xjs = ebmev/(bbd*ecrit)
      xj = sqrt(xjs)

      yj = 0.8D0 * zeff/abeam

      !  Fitting function J0(x,y)

      j0 = xjs / (4.0D0 + 3.0D0*yj + xjs *(xj + 1.39D0 + &
           0.61D0 * yj**0.7D0))

      !  Effective inverse aspect ratio, with a limit on its maximum value

      epseff = min(0.2D0, (0.5D0/aspect))

      !  Reduction in the reverse electron current
      !  due to neoclassical effects

      gfac = (1.55D0 + 0.85D0/zeff)*sqrt(epseff) - &
           (0.2D0 + 1.55D0/zeff)*epseff

      !  Reduction in the net beam driven current
      !  due to the reverse electron current

      ffac = 1.0D0 - (zbeam/zeff) * (1.0D0 - gfac)

      !  Normalisation to allow results to be valid for
      !  non-ITER plasma size and density:

      !  Line averaged electron density (10**20 m**-3) normalised to ITER

      nnorm = 1.0D0

      !  Distance along beam to plasma centre

      r = max(rmajor,rmajor*frbeam)
      eps1 = rminor/r

      if ((1.0D0+eps1) < frbeam) then
         fdiags(1) = eps1 ; fdiags(2) = frbeam
         call report_error(21)
      end if

      d = rmajor * sqrt( (1.0D0+eps1)**2 - frbeam**2)

      !  Distance along beam to plasma centre for ITER
      !  assuming a tangency radius equal to the major radius

      epsitr = 2.15D0/6.0D0
      dnorm = 6.0D0 * sqrt(2.0D0*epsitr + epsitr**2)

      !  Normalisation to beam energy (assumes a simplified formula for
      !  the beam stopping cross-section)

      ebnorm = ebmev * ( (nnorm*dnorm)/(dnla20*d) )**(1.0D0/0.78D0)

      !  A_bd fitting coefficient, after normalisation with ebnorm

      abd = 0.107D0 * (1.0D0 - 0.35D0*alphan + 0.14D0*alphan**2) * &
           (1.0D0 - 0.21D0*alphat) * (1.0D0 - 0.2D0*ebnorm + 0.09D0*ebnorm**2)

      !  Normalised current drive efficiency (A/W m**-2) (IPDG89)

      gamnb = 5.0D0 * abd * 0.1D0*ten * (1.0D0-fshine) * frbeam * &
           j0/0.2D0 * ffac

      !  Current drive efficiency (A/W)

      etanb2 = gamnb / (dene20*rmajor)

    end function etanb2

  end subroutine culnbi

end module current_drive_module
