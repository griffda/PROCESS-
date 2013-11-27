!  $Id::                                                                $
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
  !+ad_call  global_variables
  !+ad_call  heat_transport_variables
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
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraint_variables
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use global_variables
  use heat_transport_variables
  use numerics
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use process_output
  use tfcoil_variables

  implicit none

  public

  !+ad_vars  ipnscns /50/ FIX : maximum number of scan points
  integer, parameter :: ipnscns = 50
  !+ad_vars  ipnscnv /26/ FIX : number of available scan variables
  integer, parameter :: ipnscnv = 26

  !+ad_vars  isweep /0/ : number of loops to perform
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
  !+ad_varc          <LI> 22 cfactr
  !+ad_varc          <LI> 23 boundu(72: fipir)
  !+ad_varc          <LI> 24 powfmax
  !+ad_varc          <LI> 25 kappa
  !+ad_varc          <LI> 26 triang </UL>
  integer :: nsweep = 1

  !+ad_vars  sweep(ipnscns) : Actual values to use in scan
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
    !+ad_call  ostars
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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    character(len=25) :: xlabel,vlabel
    character(len=48) :: tlabel

    integer, parameter :: noutvars = 50

    character(len=25), dimension(noutvars), save :: plabel
    real(kind(1.0D0)), dimension(noutvars,ipnscns) :: outvar

    integer :: ifail,i,ivar
    logical :: first_call = .TRUE.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    tlabel = icase

    if (isweep == 0) then
       call doopt(ifail)
       call final(ifail)
       return
    end if

    if (isweep > ipnscns) then
       write(*,*) 'Error in routine SCAN:'
       write(*,*) 'Illegal value of isweep, = ',isweep
       write(*,*) 'Maximum = ',ipnscns
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    !  Set up labels for plotting output

    if (first_call) then
       plabel( 1) = 'Ifail                    '
       plabel( 2) = 'Sqsumsq                  '
       plabel( 3) = 'Electric cost (mil/kwh)  '
       plabel( 4) = 'Capital cost (mil/kwh)   '
       plabel( 5) = 'Fuel cost (mil/kwh)      '
       plabel( 6) = 'Operations cost (mil/kwh)'
       plabel( 7) = 'Capital cost (mil)       '
       plabel( 8) = 'Core costs (millions)    '
       plabel( 9) = 'Direct cost (billions)   '
       plabel(10) = 'Major Radius (m)         '
       plabel(11) = 'Aspect Ratio             '
       plabel(12) = 'Plasma Current (MA)      '
       plabel(13) = 'B Toroidal Axis (T)      '
       plabel(14) = 'B total on axis (T)      '
       plabel(15) = 'Safety Factor            '
       plabel(16) = 'qlim (zero if ishape=0)  '
       plabel(17) = 'Beta                     '
       plabel(18) = 'Beta Limit               '
       plabel(19) = 'Epsilon Beta Poloidal    '
       plabel(20) = 'Average Temp x10 (KeV)   '
       plabel(21) = 'Average Dens (10^20/m^3) '
       plabel(22) = 'H-fact Iter Power        '
       plabel(23) = 'H-fact Iter Offset       '
       plabel(24) = 'Fusion Power (MW)        '
       plabel(25) = 'nb Fusion Power (MW)     '
       plabel(26) = 'Wall Load (MW/m^2)       '
       plabel(27) = 'Injection Power (MW)     '
       plabel(28) = 'Inject Pwr Wall Plug (MW)'
       plabel(29) = 'Heating Power (MW)       '
       plabel(30) = 'Current Drive (MW)       '
       plabel(31) = 'Big Q                    '
       plabel(32) = 'Bootstrap Fraction       '
       plabel(33) = 'Neutral Beam Energy (MeV)'
       plabel(34) = 'Divertor Heat (MW/m^2)   '
       plabel(35) = 'TF coil Power (MW)       '
       plabel(36) = 'TF coil weight (kg)      '
       plabel(37) = 'TF stress (MPa)          '
       plabel(38) = 'J TF inboard leg (MA/m^2)'
       plabel(39) = 'Centrepost max T (TART)  '
       plabel(40) = 'Res TF inbrd leg Pwr (MW)'
       plabel(41) = 'Coolant Fraction Ctr.    '
       plabel(42) = 'C/P coolant radius (m)   '
       plabel(43) = 'C/P coolant velocity(m/s)'
       plabel(44) = 'C/P pump power (MW)      '
       plabel(45) = 'PF coil Power (MW)       '
       plabel(46) = 'PF coil weight (kg)      '
       plabel(47) = 'Gross Elect Pwr (MW)     '
       plabel(48) = 'Net electric Pwr (MW)    '
       plabel(49) = 'Recirculating Fraction   '
       plabel(50) = 'Psep / R                 '

       first_call = .false.
    end if

    do i = 1,isweep

       select case (nsweep)

       case (1)
          aspect = sweep(i)
          vlabel = 'aspect = ' ; xlabel = 'Aspect ratio'
       case (2)
          hldivlim = sweep(i)
          vlabel = 'hldivlim = ' ; xlabel = 'Div heat limit (MW/m2)'
       case (3)
          pnetelin = sweep(i)
          vlabel = 'pnetelin = ' ; xlabel = 'Net electric power (MW)'
       case (4)
          hfact = sweep(i)
          vlabel = 'hfact = ' ; xlabel = 'Confinement H factor'
       case (5)
          oacdcp = sweep(i)
          vlabel = 'oacdcp = ' ; xlabel = 'TF inboard leg J (MA/m2)'
       case (6)
          walalw = sweep(i)
          vlabel = 'walalw = ' ; xlabel = 'Allow. wall load (MW/m2)'
       case (7)
          beamfus0 = sweep(i)
          vlabel = 'beamfus0 = ' ; xlabel = 'Beam bkgrd multiplier'
       case (8)
          fqval = sweep(i)
          vlabel = 'fqval = ' ; xlabel = 'Big Q f-value'
       case (9)
          te = sweep(i)
          vlabel = 'te = ' ; xlabel = 'Electron temperature (keV)'
       case (10)
          boundu(15) = sweep(i)
          vlabel = 'boundu(15) = ' ; xlabel = 'Volt-second upper bound'
       case (11)
          dnbeta = sweep(i)
          vlabel = 'dnbeta = ' ; xlabel = 'Beta coefficient'
       case (12)
          bscfmax = sweep(i)
          vlabel = 'bscfmax = ' ; xlabel = 'Bootstrap fraction'
       case (13)
          boundu(10) = sweep(i)
          vlabel = 'boundu(10) = ' ; xlabel = 'H factor upper bound'
       case (14)
          fiooic = sweep(i)
          vlabel = 'fiooic = ' ; xlabel = 'TFC Iop / Icrit f-value'
       case (15)
          fjprot = sweep(i)
          vlabel = 'fjprot = ' ; xlabel = 'TFC Jprot limit f-value'
       case (16)
          rmajor = sweep(i)
          vlabel = 'rmajor = ' ; xlabel = 'Plasma major radius (m)'
       case (17)
          bmxlim = sweep(i)
          vlabel = 'bmxlim = ' ; xlabel = 'Max toroidal field (T)'
       case (18)
          gammax = sweep(i)
          vlabel = 'gammax = ' ; xlabel = 'Maximum CD gamma'
       case (19)
          boundl(16) = sweep(i)
          vlabel = 'boundl(16) = ' ; xlabel = 'OHC thickness lower bound'
       case (20)
          tbrnmn = sweep(i)
          vlabel = 'tbrnmn = ' ; xlabel = 'Minimum burn time (s)'
       case (21)
          sigpfalw = sweep(i)
          vlabel = 'sigpfalw = ' ; xlabel = 'Allowable PF coil stress'
       case (22)
          if (iavail == 1) then
             write(*,*) 'Error in routine SCAN:'
             write(*,*) 'Do not scan CFACTR if IAVAIL=1'
             write(*,*) 'PROCESS stopping.'
             stop
          end if
          cfactr = sweep(i)
          vlabel = 'cfactr = ' ; xlabel = 'Plant availability factor'
       case (23)
          boundu(72) = sweep(i)
          vlabel = 'boundu(72) = ' ; xlabel = 'Ip/Irod upper bound'
       case (24)
          powfmax = sweep(i)
          vlabel = 'powfmax = ' ; xlabel = 'Fusion power limit (MW)'
       case (25)
          kappa = sweep(i)
          vlabel = 'kappa = ' ; xlabel = 'Plasma elongation'
       case (26)
          triang = sweep(i)
          vlabel = 'triang = ' ; xlabel = 'Plasma triangularity'

       case default
          write(*,*) 'Error in routine SCAN:'
          write(*,*) 'Illegal scan variable number, nsweep = ',nsweep
          write(*,*) 'PROCESS stopping.'
          stop

       end select

       !  Write banner to output file

       call oblnkl(nout)
       call ostars(nout,72)
       write(nout,10) i,isweep,trim(xlabel),trim(vlabel),sweep(i)
10     format(' ***** Scan point ',i2,' of ',i2, &
            ': ',a,', ',a,e12.4e2,' *****')
       call ostars(nout,72)

       !  Call the optimization routine VMCON at this scan point

       call doopt(ifail)
       call final(ifail)

       !  Store values for PLOT.DAT output

       outvar( 1,i) = dble(ifail)
       outvar( 2,i) = sqsumsq
       outvar( 3,i) = coe
       outvar( 4,i) = coecap
       outvar( 5,i) = coefuelt
       outvar( 6,i) = coeoam
       outvar( 7,i) = capcost
       outvar( 8,i) = c221 + c222
       outvar( 9,i) = cdirt / 1.0D3
       outvar(10,i) = rmajor
       outvar(11,i) = aspect
       outvar(12,i) = 1.0D-6 * plascur
       outvar(13,i) = bt
       outvar(14,i) = btot
       outvar(15,i) = q
       outvar(16,i) = qlim
       outvar(17,i) = beta
       outvar(18,i) = betalim
       outvar(19,i) = betap / aspect
       outvar(20,i) = te/10.0D0 * pcoef
       outvar(21,i) = dene/1.0D20
       outvar(22,i) = hfac(6)
       outvar(23,i) = hfac(7)
       outvar(24,i) = powfmw
       outvar(25,i) = palpnb * 5.0D0
       outvar(26,i) = wallmw
       outvar(27,i) = 1.0D-6 * (pinji + pinje)
       outvar(28,i) = pinjwp
       outvar(29,i) = pheat * 1.0D-6
       outvar(30,i) = 1.0D-6 * (pinji + pinje - pheat)
       outvar(31,i) = bigq
       outvar(32,i) = bootipf
       outvar(33,i) = enbeam/1.0D3
       outvar(34,i) = hldiv
       outvar(35,i) = tfcmw
       outvar(36,i) = whttf
       outvar(37,i) = sigrad + sigtan
       outvar(38,i) = oacdcp/1.0D6
       outvar(39,i) = tcpmax
       outvar(40,i) = tfcpmw
       outvar(41,i) = fcoolcp
       outvar(42,i) = rcool
       outvar(43,i) = vcool
       outvar(44,i) = ppump/1.0D6
       outvar(45,i) = 1.0D-3 * srcktpm
       outvar(46,i) = whtpf
       outvar(47,i) = pgrossmw
       outvar(48,i) = pnetelmw
       if (ireactor == 1) then
          outvar(49,i) = (pgrossmw-pnetelmw) / pgrossmw
       else
          outvar(49,i) = 0.0D0
       end if
       outvar(50,i) = pdivt/rmajor

    end do  !  End of scanning loop

    !  Finally, write data to PLOT.DAT

    write(nplot,'(i8)') isweep
    write(nplot,'(a48)') tlabel
    write(nplot,'(a25,20e11.4)') xlabel,(sweep(i),i=1,isweep)

    do ivar = 1,noutvars
       write(nplot,'(a25,20e11.4)') plabel(ivar),(outvar(ivar,i),i=1,isweep)
    end do

  end subroutine scan

end module scan_module
