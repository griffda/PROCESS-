! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module scan_module

  !+ad_name  scan_module
  !+ad_summ  Module containing routines to perform a parameter scan
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  scan
  !+ad_args  None
  !+ad_desc  This module contains routines to perform a parameter scan
  !+ad_desc  over a range of values of a particular scanning variable.
  !+ad_prob  None
  !+ad_call  constraint_variables
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  global_variables
  !+ad_call  heat_transport_variables
  !+ad_call  impurity_radiation_module
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  process_output
  !+ad_call  tfcoil_variables
  !+ad_hist  09/10/12 PJK Initial version of module
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added global_variables module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  17/10/12 PJK Added variable descriptions
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  18/10/12 PJK Added pfcoil_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  29/10/12 PJK Added pf_power_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  31/10/12 PJK Added cost_variables
  !+ad_hist  31/10/12 PJK Added constraint_variables
  !+ad_hist  28/11/13 PJK Added scan variable 27: tbrmin
  !+ad_hist  12/02/14 PJK Added scan variable 28: bt
  !+ad_hist  04/06/14 PJK Added scan variable 29: coreradius
  !+ad_hist  16/06/14 PJK Added scan variable 30: fimpvar
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  22/07/14 PJK Raised ipnscns from 50 to 200
  !+ad_hist  06/08/15 MDK Added taulimit (31)
  !+ad_hist  14/11/16 JM  Added epsvmc (32)
  !+ad_hist  10/03/17 MDK Added ttarget (33)
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraint_variables
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use error_handling
  use global_variables
  use heat_transport_variables
  use impurity_radiation_module
  use numerics
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use process_output
  use tfcoil_variables
  use fwbs_variables
  use divertor_kallenbach_variables

  implicit none

  public

  !+ad_vars  ipnscns /200/ FIX : maximum number of scan points
  integer, parameter :: ipnscns = 200
  !+ad_vars  ipnscnv /34/ FIX : number of available scan variables
  integer, parameter :: ipnscnv = 40
  !+ad_vars  isweep /0/ : number of scan points to calculate
  integer :: isweep = 0
  !+ad_vars  nsweep /1/ : switch denoting quantity to scan:<UL>
  !+ad_varc          <LI> 1  aspect
  !+ad_varc          <LI> 2  hldivlim
  !+ad_varc          <LI> 3  pnetelin
  !+ad_varc          <LI> 4  hfact
  !+ad_varc          <LI> 5  oacdcp
  !+ad_varc          <LI> 6  walalw
  !+ad_varc          <LI> 7  beamfus0
  !+ad_varc          <LI> 8  fqval
  !+ad_varc          <LI> 9  te
  !+ad_varc          <LI> 10 boundu(15: fvs)
  !+ad_varc          <LI> 11 dnbeta
  !+ad_varc          <LI> 12 bscfmax (use negative values only)
  !+ad_varc          <LI> 13 boundu(10: hfact)
  !+ad_varc          <LI> 14 fiooic
  !+ad_varc          <LI> 15 fjprot
  !+ad_varc          <LI> 16 rmajor
  !+ad_varc          <LI> 17 bmxlim
  !+ad_varc          <LI> 18 gammax
  !+ad_varc          <LI> 19 boundl(16: ohcth)
  !+ad_varc          <LI> 20 tbrnmn
  !+ad_varc          <LI> 21 sigpfalw
  !+ad_varc          <LI> 22 cfactr (N.B. requires iavail=0)
  !+ad_varc          <LI> 23 boundu(72: fipir)
  !+ad_varc          <LI> 24 powfmax
  !+ad_varc          <LI> 25 kappa
  !+ad_varc          <LI> 26 triang
  !+ad_varc          <LI> 27 tbrmin (for blktmodel > 0 only)
  !+ad_varc          <LI> 28 bt
  !+ad_varc          <LI> 29 coreradius
  !+ad_varc          <LI> 30 fimpvar
  !+ad_varc          <LI> 31 taulimit
  !+ad_varc          <LI> 32 epsvmc
  !+ad_varc          <LI> 33 ttarget
  !+ad_varc          <LI> 34 qtargettotal
  !+ad_varc          <LI> 35 lambda_q
  !+ad_varc          <LI> 36 lambda_target
  !+ad_varc          <LI> 37 lcon_factor
  !+ad_varc          <LI> 38 Neon upper limit
  !+ad_varc          <LI> 39 Argon upper limit
  !+ad_varc          <LI> 40 Xenon upper limit</UL>

  integer :: nsweep = 1

  !+ad_vars  sweep(ipnscns) : actual values to use in scan
  real(kind(1.0D0)), dimension(ipnscns) :: sweep = 0.0D0

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine scan

    !+ad_name  scan
    !+ad_summ  Routine to call the optimisation routine VMCON over
    !+ad_summ  a range of values of one of the variables
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calls the optimisation routine VMCON
    !+ad_desc  a number of times, by performing a sweep over a range of
    !+ad_desc  values of a particular variable.
    !+ad_desc  <P>A number of output variable values are written to the
    !+ad_desc  <CODE>PLOT.DAT</CODE> file at each scan point, for
    !+ad_desc  plotting or other post-processing purposes.
    !+ad_prob  None
    !+ad_call  doopt
    !+ad_call  final
    !+ad_call  oblnkl
    !+ad_call  ovarin
    !+ad_call  ostars
    !+ad_call  report_error
    !+ad_hist  03/10/96 PJK Initial upgraded version
    !+ad_hist  01/04/98 PJK Added POWFMAX to list of scanning variables
    !+ad_hist  23/06/98 PJK Added KAPPA and TRIANG to list of scanning vars
    !+ad_hist  19/05/99 PJK Added warning about trying to scan CFACTR with
    !+ad_hisc               new availability model
    !+ad_hist  25/05/06 PJK Added implied-DO loops for sweep outputs
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/01/13 PJK Clarified some output labels
    !+ad_hist  27/06/13 PJK Modified beta coefficient label
    !+ad_hist  26/11/13 PJK Rationalised code structure; added scanning
    !+ad_hisc               variable information to output banner
    !+ad_hist  27/11/13 PJK Added Psep/R to list of output variables
    !+ad_hist  28/11/13 PJK Added scan variable 27: tbrmin
    !+ad_hist  12/02/14 PJK Added scan variable 28: bt
    !+ad_hist  13/02/14 PJK Replaced spaces with underscores in xlabel, plabel
    !+ad_hist  20/02/14 PJK Replaced te*pcoef with ten; changed plabel(20)
    !+ad_hist  30/04/14 PJK Fixed plabel(20)
    !+ad_hist  15/05/14 PJK Increased output width to 110 characters
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  04/06/14 PJK Added scan variable 29: coreradius
    !+ad_hist  16/06/14 PJK Added scan variable 30: fimpvar
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  09/07/14 PJK Turned error reporting off after each output step
    !+ad_hist  20/10/14 PJK OHC to CS
    !+ad_hist  06/08/15 MDK Add taulimit to PLOT.DAT and to scan variables (31)
    !+ad_hist  06/08/15 MDK Use 1p format: 3.0000E+01 instead of 0.3000E+01
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! MDK Put these in global_variables module
    ! character(len=25) :: xlabel,vlabel
    character(len=48) :: tlabel

    integer, parameter :: noutvars = 82
    integer, parameter :: width = 110

    character(len=25), dimension(noutvars), save :: plabel
    real(kind(1.0D0)), dimension(noutvars,ipnscns) :: outvar

    integer :: ifail,iscan,ivar
    logical :: first_call = .TRUE.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Turn off error reporting (until next output)

    errors_on = .false.

    tlabel = icase

    if (isweep == 0) then
       call doopt(ifail)
       call final(ifail)
       return
    end if

    if (isweep > ipnscns) then
       idiags(1) = isweep ; idiags(2) = ipnscns
       call report_error(94)
    end if

    !  Set up labels for plotting output
    !  Use underscores instead of spaces

    if (first_call) then
       plabel( 1) = 'Ifail____________________'
       plabel( 2) = 'Sqsumsq__________________'
       plabel( 3) = 'Electric_cost_(mil/kwh)__'
       plabel( 4) = 'Capital_cost_(mil/kwh)___'
       plabel( 5) = 'Fuel_cost_(mil/kwh)______'
       plabel( 6) = 'Operations_cost_(mil/kwh)'
       plabel( 7) = 'Capital_cost_(millions)__'
       plabel( 8) = 'Core_costs_(millions)____'
       plabel( 9) = 'Direct_cost_(billions)___'
       plabel(10) = 'Major_Radius_(m)_________'
       plabel(11) = 'Aspect_Ratio_____________'
       plabel(12) = 'Plasma_Current_(MA)______'
       plabel(13) = 'B_Toroidal_Axis_(T)______'
       plabel(14) = 'B_total_on_axis_(T)______'
       plabel(15) = 'Safety_Factor____________'
       plabel(16) = 'qlim_(zero_if_ishape=0)__'
       plabel(17) = 'Beta_____________________'
       plabel(18) = 'Beta_Limit_______________'
       plabel(19) = 'Epsilon_Beta_Poloidal____'
       plabel(20) = 'Dens.weight_Te_(10keV)___'
       plabel(21) = 'Average_Dens_(10^20/m^3)_'
       plabel(22) = 'H-fact_Iter_Power________'
       plabel(23) = 'H-fact_Iter_Offset_______'
       plabel(24) = 'Fusion_Power_(MW)________'
       plabel(25) = 'nb_Fusion_Power_(MW)_____'
       plabel(26) = 'Wall_Load_(MW/m^2)_______'
       plabel(27) = 'Injection_Power_(MW)_____'
       plabel(28) = 'Inject_Pwr_Wall_Plug_(MW)'
       plabel(29) = 'Heating_Power_(MW)_______'
       plabel(30) = 'Current_Drive_(MW)_______'
       plabel(31) = 'Big_Q____________________'
       plabel(32) = 'Bootstrap_Fraction_______'
       plabel(33) = 'Neutral_Beam_Energy_(MeV)'
       plabel(34) = 'Divertor_Heat_(MW/m^2)___'
       plabel(35) = 'TF_coil_Power_(MW)_______'
       plabel(36) = 'TF_coil_weight_(kg)______'
       plabel(37) = 'vM_stress_in_TF_case_(Pa)'
       plabel(38) = 'J_TF_inboard_leg_(MA/m^2)'
       plabel(39) = 'Centrepost_max_T_(TART)__'
       plabel(40) = 'Res_TF_inbrd_leg_Pwr_(MW)'
       plabel(41) = 'Coolant_Fraction_Ctr.____'
       plabel(42) = 'C/P_coolant_radius_(m)___'
       plabel(43) = 'C/P_coolant_velocity(m/s)'
       plabel(44) = 'C/P_pump_power_(MW)______'
       plabel(45) = 'PF_coil_Power_(MW)_______'
       plabel(46) = 'PF_coil_weight_(kg)______'
       plabel(47) = 'Gross_Elect_Pwr_(MW)_____'
       plabel(48) = 'Net_electric_Pwr_(MW)____'
       plabel(49) = 'Recirculating_Fraction___'
       plabel(50) = 'Psep/R___________________'
       plabel(51) = 'fimpvar__________________'
       plabel(52) = 'Tot._radiation_power_(MW)'
       plabel(53) = 'First_wall_peak_temp_(K)_'
       plabel(54) = 'Cu_frac_TFC_conductor____'
       plabel(55) = 'Winding_pack_area_TFC(m2)'
       plabel(56) = 'Conductor_area_TFC_(m2)__'
       plabel(57) = 'Area_TF_inboard_leg_(m2)_'
       plabel(58) = 'Taup/taueff_lower_limit__'
       plabel(59) = 'Plasma_temp_at_sep_[keV]_'
       plabel(60) = 'SOL_density_at_OMP_______'
       plabel(61) = 'Power_through__separatrix'
       plabel(62) = 'neomp/nesep_____________ '
       plabel(63) = 'qtargettotal____________ '
       plabel(64) = 'Total_pressure_at_target_'
       plabel(65) = 'Temperature_at_target____'
       plabel(66) = 'Helium_fraction__________'
       plabel(67) = 'Momentum_loss_factor_____'
       plabel(68) = 'TotalPowerLost_[W]_______'
       plabel(69) = 'H__concentration_________'
       plabel(70) = 'He_concentration_________'
       plabel(71) = 'Be_concentration_________'
       plabel(72) = 'C__concentration_________'
       plabel(73) = 'N__concentration_________'
       plabel(74) = 'O__concentration_________'
       plabel(75) = 'Ne_concentration_________'
       plabel(76) = 'Si_concentration_________'
       plabel(77) = 'Ar_concentration_________'
       plabel(78) = 'Fe_concentration_________'
       plabel(79) = 'Ni_concentration_________'
       plabel(80) = 'Kr_concentration_________'
       plabel(81) = 'Xe_concentration_________'
       plabel(82) = 'W__concentration_________'


       call ovarin(mfile,'Number of scan points','(isweep)',isweep)
       call ovarin(mfile,'Scanning variable number','(nsweep)',nsweep)

       first_call = .false.
    end if

    do iscan = 1,isweep

       select case (nsweep)

          ! Use underscores instead of spaces in xlabel
          ! MDK Remove the "=" from vlabel, to make it easier to compare with
          ! list of iteration variables

       case (1)
          aspect = sweep(iscan)
          vlabel = 'aspect' ; xlabel = 'Aspect_ratio'
       case (2)
          hldivlim = sweep(iscan)
          vlabel = 'hldivlim' ; xlabel = 'Div_heat_limit_(MW/m2)'
       case (3)
          pnetelin = sweep(iscan)
          vlabel = 'pnetelin' ; xlabel = 'Net_electric_power_(MW)'
       case (4)
          hfact = sweep(iscan)
          vlabel = 'hfact' ; xlabel = 'Confinement_H_factor'
       case (5)
          oacdcp = sweep(iscan)
          vlabel = 'oacdcp' ; xlabel = 'TF_inboard_leg_J_(MA/m2)'
       case (6)
          walalw = sweep(iscan)
          vlabel = 'walalw' ; xlabel = 'Allow._wall_load_(MW/m2)'
       case (7)
          beamfus0 = sweep(iscan)
          vlabel = 'beamfus0' ; xlabel = 'Beam_bkgrd_multiplier'
       case (8)
          fqval = sweep(iscan)
          vlabel = 'fqval' ; xlabel = 'Big_Q_f-value'
       case (9)
          te = sweep(iscan)
          vlabel = 'te' ; xlabel = 'Electron_temperature_keV'
       case (10)
          boundu(15) = sweep(iscan)
          vlabel = 'boundu(15)' ; xlabel = 'Volt-second_upper_bound'
       case (11)
          dnbeta = sweep(iscan)
          vlabel = 'dnbeta' ; xlabel = 'Beta_coefficient'
       case (12)
          bscfmax = sweep(iscan)
          vlabel = 'bscfmax' ; xlabel = 'Bootstrap_fraction'
       case (13)
          boundu(10) = sweep(iscan)
          vlabel = 'boundu(10)' ; xlabel = 'H_factor_upper_bound'
       case (14)
          fiooic = sweep(iscan)
          vlabel = 'fiooic' ; xlabel = 'TFC_Iop_/_Icrit_f-value'
       case (15)
          fjprot = sweep(iscan)
          vlabel = 'fjprot' ; xlabel = 'TFC_Jprot_limit_f-value'
       case (16)
          rmajor = sweep(iscan)
          vlabel = 'rmajor' ; xlabel = 'Plasma_major_radius_(m)'
       case (17)
          bmxlim = sweep(iscan)
          vlabel = 'bmxlim' ; xlabel = 'Max_toroidal_field_(T)'
       case (18)
          gammax = sweep(iscan)
          vlabel = 'gammax' ; xlabel = 'Maximum_CD_gamma'
       case (19)
          boundl(16) = sweep(iscan)
          vlabel = 'boundl(16)' ; xlabel = 'CS_thickness_lower_bound'
       case (20)
          tbrnmn = sweep(iscan)
          vlabel = 'tbrnmn' ; xlabel = 'Minimum_burn_time_(s)'
       case (21)
          sigpfalw = sweep(iscan)
          vlabel = 'sigpfalw' ; xlabel = 'Allowable_PF_coil_stress'
       case (22)
          if (iavail == 1) call report_error(95)
          cfactr = sweep(iscan)
          vlabel = 'cfactr' ; xlabel = 'Plant_availability_factor'
       case (23)
          boundu(72) = sweep(iscan)
          vlabel = 'boundu(72)' ; xlabel = 'Ip/Irod_upper_bound'
       case (24)
          powfmax = sweep(iscan)
          vlabel = 'powfmax' ; xlabel = 'Fusion_power_limit_(MW)'
       case (25)
          kappa = sweep(iscan)
          vlabel = 'kappa' ; xlabel = 'Plasma_elongation'
       case (26)
          triang = sweep(iscan)
          vlabel = 'triang' ; xlabel = 'Plasma_triangularity'
       case (27)
          tbrmin = sweep(iscan)
          vlabel = 'tbrmin' ; xlabel = 'Min_tritium_breed._ratio'
       case (28)
          bt = sweep(iscan)
          vlabel = 'bt' ; xlabel = 'Tor._field_on_axis_(T)'
       case (29)
          coreradius = sweep(iscan)
          vlabel = 'coreradius' ; xlabel = 'Core_radius'
       case (30)
          fimpvar = sweep(iscan)
          ! impurity_arr(impvar)%frac = fimpvar
          vlabel = 'fimpvar' ; xlabel = 'Impurity_fraction'
       case (31)
          taulimit = sweep(iscan)
          vlabel = 'taulimit' ; xlabel = 'Taup/taueff_lower_limit'
       case (32)
          epsvmc = sweep(iscan)
          vlabel = 'epsvmc' ; xlabel = 'VMCON error tolerance'
       case (33)
          ttarget = sweep(iscan)
          vlabel = 'ttarget' ; xlabel = 'Plasma temp at divertor'
      case (34)
          qtargettotal = sweep(iscan)
          vlabel = 'qtargettotal' ; xlabel = 'Total Q on target [W/m2] '
      case (35)
          lambda_q = sweep(iscan)
          vlabel = 'lambda_q' ; xlabel = 'lambda_q at OMP (m)'
      case (36)
          lambda_target = sweep(iscan)
          vlabel = 'lambda_target' ; xlabel = 'lambda_q at target (m)'
      case (37)
          lcon_factor = sweep(iscan)
          vlabel = 'lcon_factor' ; xlabel = 'Correction for lcon'
      case (38)
          boundu(129) = sweep(iscan)
          vlabel = 'boundu(129)' ; xlabel = ' Neon upper limit'
      case (39)
          boundu(131) = sweep(iscan)
          vlabel = 'boundu(131)' ; xlabel = ' Argon upper limit'
      case (40)
          boundu(135) = sweep(iscan)
          vlabel = 'boundu(135)' ; xlabel = ' Xenon upper limit'


       case default
          idiags(1) = nsweep ; call report_error(96)

       end select

       !  Write banner to output file

       call oblnkl(nout)
       call ostars(nout,width)
       ! MDK Added the "=" back in to the output statement.
       write(nout,10) ' ***** Scan point ', iscan,' of ',isweep,': ',trim(xlabel),', ',trim(vlabel),' = ',sweep(iscan),' *****'
10     format(a,i2,a,i2,5a,1pe10.3,a)
       call ostars(nout,width)

       !  Write additional information to mfile

       call oblnkl(mfile)
       call ovarin(mfile,'Scan point number','(iscan)',iscan)

       !  Call the optimization routine VMCON at this scan point
       write(*,*)'Starting scan point ',iscan
       call doopt(ifail)
       call final(ifail)

       !  Turn off error reporting (until next output)

       errors_on = .false.

       !  Store values for PLOT.DAT output

       outvar( 1,iscan) = dble(ifail)
       outvar( 2,iscan) = sqsumsq
       outvar( 3,iscan) = coe
       outvar( 4,iscan) = coecap
       outvar( 5,iscan) = coefuelt
       outvar( 6,iscan) = coeoam
       outvar( 7,iscan) = capcost
       outvar( 8,iscan) = c221 + c222
       outvar( 9,iscan) = cdirt / 1.0D3
       outvar(10,iscan) = rmajor
       outvar(11,iscan) = aspect
       outvar(12,iscan) = 1.0D-6 * plascur
       outvar(13,iscan) = bt
       outvar(14,iscan) = btot
       outvar(15,iscan) = q
       outvar(16,iscan) = qlim
       outvar(17,iscan) = beta
       outvar(18,iscan) = betalim
       outvar(19,iscan) = betap / aspect
       outvar(20,iscan) = ten/10.0D0
       outvar(21,iscan) = dene/1.0D20
       outvar(22,iscan) = hfac(6)
       outvar(23,iscan) = hfac(7)
       outvar(24,iscan) = powfmw
       outvar(25,iscan) = palpnb * 5.0D0
       outvar(26,iscan) = wallmw
       outvar(27,iscan) = pinjmw
       outvar(28,iscan) = pinjwp
       outvar(29,iscan) = pheat
       outvar(30,iscan) = pinjmw - pheat
       outvar(31,iscan) = bigq
       outvar(32,iscan) = bootipf
       outvar(33,iscan) = enbeam/1.0D3
       outvar(34,iscan) = hldiv
       outvar(35,iscan) = tfcmw
       outvar(36,iscan) = whttf
       outvar(37,iscan) = strtf2
       outvar(38,iscan) = oacdcp/1.0D6
       outvar(39,iscan) = tcpmax
       outvar(40,iscan) = tfcpmw
       outvar(41,iscan) = fcoolcp
       outvar(42,iscan) = rcool
       outvar(43,iscan) = vcool
       outvar(44,iscan) = ppump/1.0D6
       outvar(45,iscan) = 1.0D-3 * srcktpm
       outvar(46,iscan) = whtpf
       outvar(47,iscan) = pgrossmw
       outvar(48,iscan) = pnetelmw
       if (ireactor == 1) then
          outvar(49,iscan) = (pgrossmw-pnetelmw) / pgrossmw
       else
          outvar(49,iscan) = 0.0D0
       end if
       outvar(50,iscan) = pdivt/rmajor
       !outvar(51,iscan) = fimpvar
       outvar(51,iscan) = 0.0d0
       outvar(52,iscan) = pradmw
       outvar(53,iscan) = tpeak
       outvar(54,iscan) = fcutfsu
       outvar(55,iscan) = (wwp1+wwp2)*thkwp
       outvar(56,iscan) = acond
       outvar(57,iscan) = tfareain/tfno
       outvar(58,iscan) = taulimit
       outvar(59,iscan) = tesep
       outvar(60,iscan) = neomp
       outvar(61,iscan) = psep_kallenbach
       outvar(62,iscan) = neratio
       outvar(63,iscan) = qtargettotal
       outvar(64,iscan) = pressure0
       outvar(65,iscan) = ttarget
       outvar(66,iscan) = ralpne
       outvar(67,iscan) = fmom
       outvar(68,iscan) = TotalPowerLost
       outvar(69,iscan) = fimp(1)
       outvar(70,iscan) = fimp(2)
       outvar(71,iscan) = fimp(3)
       outvar(72,iscan) = fimp(4)
       outvar(73,iscan) = fimp(5)
       outvar(74,iscan) = fimp(6)
       outvar(75,iscan) = fimp(7)
       outvar(76,iscan) = fimp(8)
       outvar(77,iscan) = fimp(9)
       outvar(78,iscan) = fimp(10)
       outvar(79,iscan) = fimp(11)
       outvar(80,iscan) = fimp(12)
       outvar(81,iscan) = fimp(13)
       outvar(82,iscan) = fimp(14)


    end do  !  End of scanning loop

    !  Finally, write data to PLOT.DAT

    write(nplot,'(i8)') isweep
    write(nplot,'(a48)') tlabel
    write(nplot,'(a25, 1p, 20e11.4)') xlabel,(sweep(iscan),iscan=1,isweep)

    do ivar = 1,noutvars
       !write(nplot,'(a25,20e11.4)') plabel(ivar), (outvar(ivar,iscan), iscan=1,isweep)
       write(nplot,'(a25, 1p, 20e11.4)') plabel(ivar), (outvar(ivar,iscan), iscan=1,isweep)
    end do

  end subroutine scan

end module scan_module
