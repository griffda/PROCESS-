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

  use build_variables
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
  use final_module
  
  implicit none

  public

  !+ad_vars  ipnscns /200/ FIX : maximum number of scan points
  integer, parameter :: ipnscns = 200

  !+ad_vars  ipnscnv /45/ FIX : number of available scan variables
  integer, parameter :: ipnscnv = 47

  !+ad_vars  scan_dim /1/ : 1-D or 2-D scan switch (1=1D, 2=2D)
  integer :: scan_dim = 1

  !+ad_vars  isweep /0/ : number of scan points to calculate
  integer :: isweep = 0

  !+ad_vars  isweep_2 /0/ : number of 2D scan points to calculate
  integer :: isweep_2 = 0

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
  !+ad_varc          <LI> 21 not used
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
  !+ad_varc          <LI> 35 lambda_q_omp
  !+ad_varc          <LI> 36 lambda_target
  !+ad_varc          <LI> 37 lcon_factor
  !+ad_varc          <LI> 38 Neon upper limit
  !+ad_varc          <LI> 39 Argon upper limit
  !+ad_varc          <LI> 40 Xenon upper limit
  !+ad_varc          <LI> 41 blnkoth
  !+ad_varc          <LI> 42 Argon fraction fimp(9)
  !+ad_varc          <LI> 43 normalised minor radius at which electron cyclotron current drive is maximum
  !+ad_varc          <LI> 44 Allowable tresca stress in tf coil structural material
  !+ad_varc          <LI> 45 Minimum allowable temperature margin ; tf coils
  !+ad_varc          <LI> 46 boundu(150) fgwsep
  !+ad_varc          <LI> 47 impurity_enrichment(9) Argon impurity enrichment </UL>
  integer :: nsweep = 1

  !+ad_vars  nsweep_2 /3/ : switch denoting quantity to scan for 2D scan:
  integer :: nsweep_2 = 3

  !+ad_vars  sweep(ipnscns) /../: actual values to use in scan
  real(kind(1.0D0)), dimension(ipnscns) :: sweep = 0.0D0

  !+ad_vars  sweep_2(ipnscns) /../: actual values to use in 2D scan
  real(kind(1.0D0)), dimension(ipnscns) :: sweep_2 = 0.0D0

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
    !+ad_hist  06/08/15 MDK Use 1p format: 3.0000E+01 instead of 0.3000E+01
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Local variables
    integer :: ifail

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Turn off error reporting (until next output)
    errors_on = .false.

    if (isweep == 0) then
       call doopt(ifail)
       call final(ifail)
       return
    end if

    if (isweep > ipnscns) then
       idiags(1) = isweep ; idiags(2) = ipnscns
       call report_error(94)
    end if

    if (scan_dim == 2) then
        call scan_2d
    else
        call scan_1d
    end if

  end subroutine scan

  subroutine scan_1d
    !+ad_name  scan_1d
    !+ad_summ  Routine to call 1-D scan
    !+ad_type  Subroutine
    !+ad_auth  J Morris, UKAEA, Culham Science Centre
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Local variables
    character(len=48) :: tlabel
    integer, parameter :: noutvars = 83
    integer, parameter :: width = 110
    character(len=25), dimension(noutvars), save :: plabel
    real(kind(1.0D0)), dimension(noutvars,ipnscns) :: outvar
    integer :: ifail, iscan, ivar
    logical :: first_call = .TRUE.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    tlabel = icase

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
       plabel(68) = 'totalpowerlost_[W]_______'
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
       plabel(83) = 'teped____________________'

       call ovarin(mfile,'Number of scan points','(isweep)',isweep)
       call ovarin(mfile,'Scanning variable number','(nsweep)',nsweep)

       first_call = .false.
    end if

    do iscan = 1,isweep

        ! Makes iscan available globally (read-only)
        iscan_global = iscan

        call scan_select(nsweep, sweep, iscan, vlabel, xlabel)

        ! Write banner to output file
        call oblnkl(nout)
        call ostars(nout,width)
        write(nout,10) ' ***** Scan point ', iscan,' of ',isweep,': &
            ',trim(xlabel),', ',trim(vlabel),' = ',sweep(iscan),' *****'
    10     format(a,i2,a,i2,5a,1pe10.3,a)
        call ostars(nout,width)

        ! Write additional information to mfile
        call oblnkl(mfile)
        call ovarin(mfile,'Scan point number','(iscan)',iscan)

        ! Call the optimization routine VMCON at this scan point
        write(*,20)'Starting scan point ',iscan, ': ', trim(xlabel),', &
            ',trim(vlabel),' = ',sweep(iscan)
    20     format(a,i2,a,4a,1pe10.3)
        call doopt(ifail)
        call final(ifail)

        ! Turn off error reporting (until next output)
        errors_on = .false.

        ! Store values for PLOT.DAT output
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
        outvar(68,iscan) = totalpowerlost
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
        outvar(83,iscan) = teped

    end do  !  End of scanning loop

    ! Finally, write data to PLOT.DAT
    write(nplot,'(i8)') isweep
    write(nplot,'(a48)') tlabel
    write(nplot,'(a25, 1p, 200e11.4)') xlabel,(sweep(iscan),iscan=1,isweep)

    do ivar = 1,noutvars
       !write(nplot,'(a25,20e11.4)') plabel(ivar), (outvar(ivar,iscan), iscan=1,isweep)
       write(nplot,'(a25, 1p, 200e11.4)') plabel(ivar), (outvar(ivar,iscan), iscan=1,isweep)
    end do

  end subroutine scan_1d

  subroutine scan_2d
    !+ad_name  scan_2d
    !+ad_summ  Routine to call 2-D scan
    !+ad_type  Subroutine
    !+ad_auth  J Morris, UKAEA, Culham Science Centre
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables
    character(len=48) :: tlabel
    integer, parameter :: noutvars = 83
    integer, parameter :: width = 110
    character(len=25), dimension(noutvars), save :: plabel
    real(kind(1.0D0)), dimension(noutvars,ipnscns) :: outvar
    integer :: ifail, iscan, ivar, iscan_1, iscan_2
    logical :: first_call = .TRUE.
    real(kind(1.0D0)), dimension(ipnscns) :: sweep_1_vals, sweep_2_vals

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    tlabel = icase

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
        plabel(68) = 'totalpowerlost_[W]_______'
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
        plabel(83) = 'teped____________________'

        call ovarin(mfile,'Number of scan points','(isweep)',isweep)
        call ovarin(mfile,'Scanning variable number','(nsweep)',nsweep)

        first_call = .false.
    end if

    iscan = 1

    do iscan_1 = 1, isweep

        do iscan_2 = 1, isweep_2

            ! Makes iscan available globally (read-only)
            iscan_global = iscan

            call scan_select(nsweep, sweep, iscan_1, vlabel, xlabel)
            call scan_select(nsweep_2, sweep_2, iscan_2, vlabel_2, xlabel_2)

            ! Write banner to output file
            call oblnkl(nout)
            call ostars(nout,width)
            write(nout,10) iscan, isweep*isweep_2, trim(vlabel), &
                sweep(iscan_1), trim(vlabel_2), sweep_2(iscan_2)
        ! 10    format(a, i2, a, i2, 5a, 1pe10.3, a)
        10  format(' ***** 2D scan point ', i3, ' of ', i3, ' : ', a, ' = ', &
                   1pe10.3, ' and ', a, ' = ', 1pe10.3, ' *****')
            call ostars(nout,width)

            ! Write additional information to mfile
            call oblnkl(mfile)
            call ovarin(mfile,'Scan point number','(iscan)',iscan)

            ! Call the optimization routine VMCON at this scan point
            write(*,20) iscan, trim(xlabel), trim(vlabel), sweep(iscan_1), &
                trim(xlabel_2), trim(vlabel_2), sweep_2(iscan_2)
            ! 20     format(a,i2,a,4a,1pe10.3)
        20  format('Starting scan point ', i3, ': ', a, ', ', a, ' = ', &
                   1pe10.3, ' and ', a, ', ', a, ' = ', 1pe10.3)
            call doopt(ifail)
            call final(ifail)

            ! Turn off error reporting (until next output)
            errors_on = .false.

            ! Store values for PLOT.DAT output
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
            outvar(68,iscan) = totalpowerlost
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
            outvar(83,iscan) = teped

            sweep_1_vals(iscan) = sweep(iscan_1)
            sweep_2_vals(iscan) = sweep_2(iscan_2)
            iscan = iscan + 1
        end do  !  End of scanning loop
    end do  !  End of scanning loop

    ! Finally, write data to PLOT.DAT
    write(nplot,'(i8)') isweep*isweep_2
    write(nplot,'(a48)') tlabel
    write(nplot,'(a25, 1p, 200e11.4)') xlabel, (sweep_1_vals(iscan), iscan=1, &
          isweep*isweep_2)
    write(nplot,'(a25, 1p, 200e11.4)') xlabel_2, (sweep_2_vals(iscan), &
          iscan=1, isweep*isweep_2)

    do ivar = 1, noutvars
        write(nplot,'(a25, 1p, 200e11.4)') plabel(ivar), (outvar(ivar,iscan), &
            iscan=1,isweep*isweep_2)
    end do

  end subroutine scan_2d

  subroutine scan_select(nwp, swp, iscn, vlab, xlab)
    !+ad_name  scan_select
    !+ad_summ  Routine to select first scan case
    !+ad_type  Subroutine
    !+ad_auth  J Morris, UKAEA, Culham Science Centre
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments
    integer, intent(in) :: nwp, iscn
    real(kind(1.0D0)), intent(in), dimension(ipnscns) :: swp
    character(len=25), intent(out) :: vlab, xlab

    select case (nwp)
        ! Use underscores instead of spaces in xlabel
        ! MDK Remove the "=" from vlabel, to make it easier to compare with
        ! list of iteration variables

        case (1)
            aspect = swp(iscn)
            vlab = 'aspect' ; xlab = 'Aspect_ratio'
        case (2)
            hldivlim = swp(iscn)
            vlab = 'hldivlim' ; xlab = 'Div_heat_limit_(MW/m2)'
        case (3)
            pnetelin = swp(iscn)
            vlab = 'pnetelin' ; xlab = 'Net_electric_power_(MW)'
        case (4)
            hfact = swp(iscn)
            vlab = 'hfact' ; xlab = 'Confinement_H_factor'
        case (5)
            oacdcp = swp(iscn)
            vlab = 'oacdcp' ; xlab = 'TF_inboard_leg_J_(MA/m2)'
        case (6)
            walalw = swp(iscn)
            vlab = 'walalw' ; xlab = 'Allow._wall_load_(MW/m2)'
        case (7)
            beamfus0 = swp(iscn)
            vlab = 'beamfus0' ; xlab = 'Beam_bkgrd_multiplier'
        case (8)
            fqval = swp(iscn)
            vlab = 'fqval' ; xlab = 'Big_Q_f-value'
        case (9)
            te = swp(iscn)
            vlab = 'te' ; xlab = 'Electron_temperature_keV'
        case (10)
            boundu(15) = swp(iscn)
            vlab = 'boundu(15)' ; xlab = 'Volt-second_upper_bound'
        case (11)
            dnbeta = swp(iscn)
            vlab = 'dnbeta' ; xlab = 'Beta_coefficient'
        case (12)
            bscfmax = swp(iscn)
            vlab = 'bscfmax' ; xlab = 'Bootstrap_fraction'
        case (13)
            boundu(10) = swp(iscn)
            vlab = 'boundu(10)' ; xlab = 'H_factor_upper_bound'
        case (14)
            fiooic = swp(iscn)
            vlab = 'fiooic' ; xlab = 'TFC_Iop_/_Icrit_f-value'
        case (15)
            fjprot = swp(iscn)
            vlab = 'fjprot' ; xlab = 'TFC_Jprot_limit_f-value'
        case (16)
            rmajor = swp(iscn)
            vlab = 'rmajor' ; xlab = 'Plasma_major_radius_(m)'
        case (17)
            bmxlim = swp(iscn)
            vlab = 'bmxlim' ; xlab = 'Max_toroidal_field_(T)'
        case (18)
            gammax = swp(iscn)
            vlab = 'gammax' ; xlab = 'Maximum_CD_gamma'
        case (19)
            boundl(16) = swp(iscn)
            vlab = 'boundl(16)' ; xlab = 'CS_thickness_lower_bound'
        case (20)
            tbrnmn = swp(iscn)
            vlab = 'tbrnmn' ; xlab = 'Minimum_burn_time_(s)'
        case (21)
            ! sigpfalw = swp(iscn)
            vlab = 'obsolete' ; xlab = 'obsolete'
        case (22)
            if (iavail == 1) call report_error(95)
            cfactr = swp(iscn)
            vlab = 'cfactr' ; xlab = 'Plant_availability_factor'
        case (23)
            boundu(72) = swp(iscn)
            vlab = 'boundu(72)' ; xlab = 'Ip/Irod_upper_bound'
        case (24)
            powfmax = swp(iscn)
            vlab = 'powfmax' ; xlab = 'Fusion_power_limit_(MW)'
        case (25)
            kappa = swp(iscn)
            vlab = 'kappa' ; xlab = 'Plasma_elongation'
        case (26)
            triang = swp(iscn)
            vlab = 'triang' ; xlab = 'Plasma_triangularity'
        case (27)
            tbrmin = swp(iscn)
            vlab = 'tbrmin' ; xlab = 'Min_tritium_breed._ratio'
        case (28)
            bt = swp(iscn)
            vlab = 'bt' ; xlab = 'Tor._field_on_axis_(T)'
        case (29)
            coreradius = swp(iscn)
            vlab = 'coreradius' ; xlab = 'Core_radius'
        case (30)
            fimpvar = swp(iscn)
            ! impurity_arr(impvar)%frac = fimpvar
            vlab = 'fimpvar' ; xlab = 'Impurity_fraction'
        case (31)
            taulimit = swp(iscn)
            vlab = 'taulimit' ; xlab = 'Taup/taueff_lower_limit'
        case (32)
            epsvmc = swp(iscn)
            vlab = 'epsvmc' ; xlab = 'VMCON error tolerance'
        case (33)
            ttarget = swp(iscn)
            vlab = 'ttarget' ; xlab = 'Plasma temp at divertor'
        case (34)
            qtargettotal = swp(iscn)
            vlab = 'qtargettotal' ; xlab = 'Total Q on target [W/m2] '
        case (35)
            lambda_q_omp = swp(iscn)
            vlab = 'lambda_q_omp' ; xlab = 'lambda_q at OMP (m)'
        case (36)
            target_spread = swp(iscn)
            vlab = 'target_spread' ; xlab = 'lambda_q increase (m)'
        case (37)
            lcon_factor = swp(iscn)
            vlab = 'lcon_factor' ; xlab = 'Correction for lcon'
        case (38)
            boundu(129) = swp(iscn)
            vlab = 'boundu(129)' ; xlab = ' Neon upper limit'
        case (39)
            boundu(131) = swp(iscn)
            vlab = 'boundu(131)' ; xlab = ' Argon upper limit'
        case (40)
            boundu(135) = swp(iscn)
            vlab = 'boundu(135)' ; xlab = ' Xenon upper limit'
        case (41)
            blnkoth = swp(iscn)
            vlab = 'blnkoth' ; xlab = 'Outboard blanket thick.'
        case (42)
            fimp(9) = swp(iscn)
            impurity_arr(9)%frac = fimp(9)
            vlab = 'fimp(9)' ; xlab = 'Argon fraction'
        case (43)
            rho_ecrh = swp(iscn)
            vlab = 'rho_ecrh' ; xlab = 'rho at which ECCD is max'
        case (44)
            alstrtf = swp(iscn)
            vlab = 'alstrtf' ; xlab = 'Allowable_tresca_stress_in_tf_coil_(pa)'
        case (45)
            tmargmin_tf = swp(iscn)
            vlab = 'tmargmin_tf' ; xlab = 'Minimum_allowable_temperature_margin'
        case (46)
            boundu(152) = swp(iscn)
            vlab = 'boundu(152)' ; xlab = 'Max allowable fgwsep'
        case (47)
            impurity_enrichment(9) = swp(iscn)
            vlab = 'impurity_enrichment(9)' ; xlab = 'Argon SOL impurity enrichment'
        case default
            idiags(1) = nwp ; call report_error(96)

    end select

  end subroutine scan_select

  subroutine doopt(ifail)

  !+ad_name  doopt
  !+ad_summ  Routine to call the optimising equation solver
  !+ad_type  Subroutine
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  ifail   : output integer : error flag
  !+ad_desc  This routine calls the optimising equation solver.
  !+ad_prob  None
  !+ad_call  constraints
  !+ad_call  error_handling
  !+ad_call  function_evaluator
  !+ad_call  numerics
  !+ad_call  process_output
  !+ad_call  boundxc
  !+ad_call  constraint_eqns
  !+ad_call  int_to_string3
  !+ad_call  loadxc
  !+ad_call  oblnkl
  !+ad_call  ocmmnt
  !+ad_call  oheadr
  !+ad_call  optimiz
  !+ad_call  osubhd
  !+ad_call  ovarin
  !+ad_call  ovarre
  !+ad_call  report_error
  !+ad_call  verror
  !+ad_hist  03/10/96 PJK Initial upgraded version
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  10/10/12 PJK Modified to use new numerics and function_evaluator
  !+ad_hisc               modules
  !+ad_hist  31/01/13 PJK Added warning about high residuals if the convergence
  !+ad_hisc               is suspicious
  !+ad_hist  04/07/13 PJK Modified wording for variables at/beyond their bounds
  !+ad_hist  28/11/13 PJK Modified format lines for longer lablxc length
  !+ad_hist  13/02/14 PJK Output ifail even if a feasible solution found
  !+ad_hist  27/02/14 PJK Added nineqns usage; minor output modifications
  !+ad_hist  13/03/14 PJK Added numerical state information to mfile
  !+ad_hist  09/07/14 PJK Added error reporting
  !+ad_hist  09/07/14 PJK Added range-normalised iteration variable values to mfile
  !+ad_hist  28/07/14 PJK Added constraint_eqns call to evaluate residues
  !+ad_hisc               in physical units
  !+ad_hist  19/08/14 PJK Added nvar, neqns to output, constraint residues to mfile
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraints
  use error_handling
  use function_evaluator
  use numerics
  use process_output
  use utilities, only:upper_case
  use main_module, only:verror
  ! for ipedestal = 2 option
  !use plasmod_variables
  use plasmod_module

  implicit none

  !  Arguments
  integer, intent(out) :: ifail

  !  Local variables
  integer :: ii,inn,iflag
  real(kind(1.0D0)) :: summ,xcval,xmaxx,xminn,f,xnorm
  real(kind(1.0D0)), dimension(ipeqns) :: con1, con2, err
  character(len=1), dimension(ipeqns) :: sym
  character(len=10), dimension(ipeqns) :: lab
  character(len=30) :: strfom
  character(len=60) :: string1, string2

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  If no optimisation is required, leave the routine
  if (ioptimz < 0) return

  !  Set up variables to be iterated
  ! MDK Allocating here doesn't work if there is a scan
  ! allocate(name_xc(nvar))
  call loadxc
  call boundxc
  call optimiz(fcnvmc1,fcnvmc2,ifail,f)

  ! Calculate PLASMOD after everything else has finished for comparison
  if (ipedestal == 2) then

     call setupPlasmod(num,geom,comp,ped,inp0,i_flag)

     call plasmod_EF(num,geom,comp,ped,inp0,radp,mhd,loss,i_flag)

     if (i_flag==1)then
        write(*,*) 'PLASMOD has converged!!!'
     elseif (i_flag==0)then
        write(*,*) 'The PLASMOD transport model has crashed'
     elseif (i_flag==-1)then
        write(*,*) 'The PLASMOD transport model has not converged after itermax'
     elseif (i_flag==-2)then
        write(*,*) 'The PLASMOD equilibrium has crashed'
      endif

  endif

  !  Check on accuracy of solution by summing the
  !  squares of the residuals of the equality constraints
  summ = 0.0D0
  do ii = 1,neqns
     summ = summ + rcm(ii)*rcm(ii)
  end do
  sqsumsq = sqrt(summ)

  !  Turn on error reporting
  errors_on = .true.

  !  Print out information on solution
  call oheadr(nout,'Numerics')
  call ocmmnt(nout,'PROCESS has performed a VMCON (optimisation) run.')
  if (ifail /= 1) then
     !call ocmmnt(nout,'but could not find a feasible set of parameters.')
     call oheadr(nout,'PROCESS COULD NOT FIND A FEASIBLE SOLUTION')
     call oheadr(iotty,'PROCESS COULD NOT FIND A FEASIBLE SOLUTION')
     call ovarin(iotty,'VMCON error flag (ifail)','',ifail)
     call oblnkl(iotty)

     idiags(1) = ifail ; call report_error(132)

  else
     call ocmmnt(nout,'and found a feasible set of parameters.')
     call oblnkl(nout)
     call ovarin(nout,'VMCON error flag','(ifail)',ifail)
     call oheadr(iotty,'PROCESS found a feasible solution')
  end if

  !call oblnkl(nout)

  !  If necessary, write out a relevant error message
  if (ifail /= 1) then
     call verror(ifail)
     call oblnkl(nout)
     call oblnkl(iotty)
  else
     !  Show a warning if the constraints appear high even if allegedly converged
     if (sqsumsq >= 1.0D-2) then
        call oblnkl(nout)
        call ocmmnt(nout,'WARNING: Constraint residues are HIGH; consider re-running')
        call ocmmnt(nout,'   with lower values of EPSVMC to confirm convergence...')
        call ocmmnt(nout,'   (should be able to get down to about 1.0E-8 okay)')
        call oblnkl(nout)
        call ocmmnt(iotty,'WARNING: Constraint residues are HIGH; consider re-running')
        call ocmmnt(iotty,'   with lower values of EPSVMC to confirm convergence...')
        call ocmmnt(iotty,'   (should be able to get down to about 1.0E-8 okay)')
        call oblnkl(iotty)

        fdiags(1) = sqsumsq ; call report_error(134)

     end if
  end if

  call ovarin(nout,'Number of iteration variables','(nvar)',nvar)
  call ovarin(nout,'Number of constraints (total)','(neqns+nineqns)',neqns+nineqns)
  call ovarin(nout,'Optimisation switch','(ioptimz)',ioptimz)
  call ovarin(nout,'Figure of merit switch','(minmax)',minmax)
  if (ifail /= 1) then
     call ovarin(nout,'VMCON error flag','(ifail)',ifail)
  end if

  call ovarre(nout,'Square root of the sum of squares of the constraint residuals','(sqsumsq)',sqsumsq, 'OP ')
  call ovarre(nout,'VMCON convergence parameter','(convergence_parameter)',convergence_parameter, 'OP ')
  call ovarin(nout,'Number of VMCON iterations','(nviter)',nviter, 'OP ')
  call oblnkl(nout)

  if (ifail == 1) then
     string1 = 'PROCESS has successfully optimised the iteration variables'
  else
     string1 = 'PROCESS has tried to optimise the iteration variables'
  end if

  if (minmax > 0) then
     string2 = ' to minimise the figure of merit: '
  else
     string2 = ' to maximise the figure of merit: '
  end if

  strfom = lablmm(abs(minmax))
  call upper_case(strfom)
  write(nout,10) trim(string1) // trim(string2),  trim(strfom)
10 format(a90, t92, a22)

  call oblnkl(nout)

  !  Check which variables are at bounds
  iflag = 0
  do ii = 1,nvar
     xminn = 1.01D0*bondl(ii)
     xmaxx = 0.99D0*bondu(ii)

     if (xcm(ii) < xminn) then
        if (iflag == 0) then
           call ocmmnt(nout, &
                'Certain operating limits have been reached,')
           call ocmmnt(nout, &
                'as shown by the following iteration variables that are')
           call ocmmnt(nout, &
                'at or near to the edge of their prescribed range :')
           call oblnkl(nout)
           iflag = 1
        end if
        xcval = xcm(ii)*scafc(ii)
        !write(nout,30) ii,lablxc(ixc(ii)),xcval,bondl(ii)*scafc(ii)
        write(nout,30) lablxc(ixc(ii)),xcval,bondl(ii)*scafc(ii)
     end if

     if (xcm(ii) > xmaxx) then
        if (iflag == 0) then
           call ocmmnt(nout, &
                'Certain operating limits have been reached,')
           call ocmmnt(nout, &
                'as shown by the following iteration variables that are')
           call ocmmnt(nout, &
                'at or near to the edge of their prescribed range :')
           call oblnkl(nout)
           iflag = 1
        end if
        xcval = xcm(ii)*scafc(ii)
        write(nout,40) lablxc(ixc(ii)),xcval,bondu(ii)*scafc(ii)
     end if
  end do

!30 format(t4,'Variable ',i3,' (',a9, &
!        ',',1pe12.4,') is at or below its lower bound:',1pe12.4)
30 format(t4, a30, '=',1pe12.4,' is at or below its lower bound:',1pe12.4)
40 format(t4, a30, '=',1pe12.4,' is at or above its upper bound:',1pe12.4)
!40 format(t4,'Variable ',i3,' (',a9, &
!        ',',1pe12.4,') is at or above its upper bound:',1pe12.4)

  !  Print out information on numerics
  call osubhd(nout,'The solution vector is comprised as follows :')
!  write(nout,50)
! Remove Lagrange multipliers as no-one understands them.
! MFILE not changed
!50 format(t47,'lower',t59,'upper')

  write(nout,60)
!60 format(t23,'final',t33,'fractional',t46,'Lagrange',t58,'Lagrange')
60 format(t43,'final',t55,'final /')


  write(nout,70)
!70 format(t5,'i',t23,'value',t35,'change',t45,'multiplier', &
!        t57,'multiplier')
70 format(t5,'i',t43,'value',t55,'initial')

  call oblnkl(nout)

  do inn = 1,nvar
     xcs(inn) = xcm(inn)*scafc(inn)
!     write(nout,80) inn,lablxc(ixc(inn)),xcs(inn),xcm(inn), &
!          vlam(neqns+nineqns+inn), vlam(neqns+nineqns+1+inn+nvar)
     write(nout,80) inn,lablxc(ixc(inn)),xcs(inn),xcm(inn)
!80 format(t2,i4,t8,a9,t19,4(1pe12.4))
!80 format(t2,i4,t8,a30,t39,2(1pe12.4))
80 format(t2,i4,t8,a30,t39,1pe12.4, t52, 0pf10.4)
! MDK The 0p is needed because of a bizarre "feature"/bug in fortran:
! the 1p in the previous format continues until changed.
     call ovarre(mfile,lablxc(ixc(inn)),'(itvar'//int_to_string3(inn)//')',xcs(inn))

     !  'Range-normalised' iteration variable values for MFILE:
     !  0.0 (at lower bound) to 1.0 (at upper bound)
     if (bondl(inn) == bondu(inn)) then
        xnorm = 1.0D0
     else
        xnorm = (xcm(inn) - bondl(inn)) / (bondu(inn) - bondl(inn))
        xnorm = max(xnorm, 0.0D0)
        xnorm = min(xnorm, 1.0D0)
     end if
     ! Added ratio final/initial to MFILE
     call ovarre(mfile,trim(lablxc(ixc(inn)))//' (final value/initial value)', &
          '(xcm'//int_to_string3(inn)//')',xcm(inn))
     call ovarre(mfile,trim(lablxc(ixc(inn)))//' (range normalised)', &
          '(nitvar'//int_to_string3(inn)//')',xnorm)
  end do


  call osubhd(nout, &
       'The following equality constraint residues should be close to zero :')

  call constraint_eqns(neqns+nineqns,con1,-1,con2,err,sym,lab)
  write(nout,90)
90 format(t48,'physical',t73,'constraint',t100,'normalised')
  write(nout,100)
100 format(t47,'constraint',t74,'residue',t101,'residue')
  call oblnkl(nout)
  do inn = 1,neqns
     write(nout,110) inn,lablcc(icc(inn)),sym(inn),con2(inn), &
          lab(inn),err(inn),lab(inn),con1(inn)
     call ovarre(mfile,lablcc(icc(inn))//' normalised residue', &
          '(normres'//int_to_string3(inn)//')',con1(inn))
  end do
110 format(t2,i4,t8,a33,t46,a1,t47,1pe12.4,t60,a10,t71,1pe12.4,t84,a10,t98,1pe12.4)

  if (nineqns > 0) then
     call osubhd(nout, &
          'The following inequality constraint residues should be greater than or approximately equal to zero :')

     do inn = neqns+1,neqns+nineqns
        !write(nout,120) inn,lablcc(icc(inn)),rcm(inn),vlam(inn)
        write(nout,110) inn,lablcc(icc(inn)),sym(inn),con2(inn), &
                        lab(inn), err(inn), lab(inn)
        call ovarre(mfile,lablcc(icc(inn)),'(constr'//int_to_string3(inn)//')',rcm(inn))
     end do
  end if

! 120 format(t2,i4,t8,a33,t45,1pe12.4,1pe12.4)

end subroutine doopt

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module scan_module
