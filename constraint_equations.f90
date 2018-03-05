! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module constraints

  !+ad_name  constraints
  !+ad_summ  Module defining the constraint equations
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  constraint_eqns
  !+ad_args  N/A
  !+ad_desc  This module contains the routine that evaluates the constraint
  !+ad_desc  equations.
  !+ad_prob  None
  !+ad_call  build_variables
  !+ad_call  constants
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables

  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  pulse_variables
  !+ad_call  report_error
  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  28/07/14 PJK Initial conversion from a subroutine
  !+ad_hist  23/05/16 JM  Extra comments
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Import modules !
  !!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use constraint_variables
  use cost_variables
  use current_drive_variables
  use divertor_kallenbach_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables

  use numerics
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use pulse_variables
  use rebco_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables
  use vacuum_variables

  implicit none

  public :: constraint_eqns

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine constraint_eqns(m,cc,ieqn,con,err,symbol,units)

    !+ad_name  constraint_eqns
    !+ad_summ  Routine that formulates the constraint equations
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  J Morris, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  m : input integer : Number of constraint equations to solve
    !+ad_args  cc(m) : output real array : Residual error in equation i
    !+ad_args  ieqn : input integer : Switch for constraint equations to evaluate;
    !+ad_argc                         if <CODE>ieqn</CODE> is zero or negative, evaluate
    !+ad_argc                         all the constraint equations, otherwise
    !+ad_argc                         evaluate only the <CODE>ieqn</CODE>th equation
    !+ad_args  con(m) : optional output real array : constraint value for equation i
    !+ad_argc                         in physical units
    !+ad_args  err(m) : optional output real array : residual error in equation i
    !+ad_argc                         in physical units
    !+ad_args  symbol(m) : optional output character array : <CODE>=</CODE>, <CODE>&gt;</CODE>
    !+ad_argc         or <CODE>&lt;</CODE> symbol for equation i, denoting its type
    !+ad_args  units(m) : optional output character array : constraint units in equation i
    !+ad_desc  This routine formulates the constraint equations.
    !+ad_desc  The code attempts to make cc(i) = 0 for all i=1 to m equations.
    !+ad_desc  All relevant consistency equations should be active in order
    !+ad_desc  to make a self-consistent machine.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  01/07/94 PJK Improved layout and added stellarator constraints
    !+ad_hist  08/12/94 PJK Added stellarator radial build consistency
    !+ad_hist  07/10/96 PJK Added ICULBL=2 option to constraint no.24
    !+ad_hist  01/04/98 PJK Modified equations 2,3,4,7 and 28 to take into
    !+ad_hisc               account IGNITE (ignition switch) setting
    !+ad_hist  25/07/11 PJK Applied Greenwald density limit to line-averaged
    !+ad_hisc               rather than volume-averaged density
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  14/11/11 PJK Changed NaN error check
    !+ad_hist  06/11/12 PJK Renamed routine from con1 to constraints,
    !+ad_hisc               and the source file itself from eqns.f90 to
    !+ad_hisc               constraint_equations.f90
    !+ad_hist  17/12/12 PJK Eqn 30 inverted to prevent problems if pinj=0;
    !+ad_hisc               Added new eqn 51, plus debug lines
    !+ad_hist  23/01/13 PJK Allowed eqn.47 to be used for stellarators
    !+ad_hist  04/06/13 PJK Added fwbs_variables, eqns 52-55
    !+ad_hist  11/06/13 PJK Changed wording for eqn 41; added note about
    !+ad_hisc               dign range
    !+ad_hist  18/06/13 PJK Removed dign from power balance equations
    !+ad_hist  27/06/13 PJK Removed Troyon as a beta limit descriptor
    !+ad_hist  25/09/13 PJK Modified eqn.20 from port size limit to
    !+ad_hisc               neutral beam tangency radius limit
    !+ad_hist  30/09/13 PJK Added new eqn 56
    !+ad_hist  10/10/13 PJK Made multiplier in beta equation explicit
    !+ad_hist  17/12/13 PJK Added ieqn argument to optionally only evaluate
    !+ad_hisc               one of the constraint equations
    !+ad_hist  13/02/14 PJK Made limit equations uniform in style
    !+ad_hist  26/02/14 PJK Added new eqns 57, 58
    !+ad_hist  05/03/14 PJK Removed redundant eqn 17
    !+ad_hist  01/05/14 PJK Changed eqn 28 description
    !+ad_hist  08/05/14 PJK Modified eqn 28
    !+ad_hist  19/05/14 PJK Removed redundant eqn 15
    !+ad_hist  19/05/14 PJK Added new eqn 17; modified eqns 2,4 to use
    !+ad_hisc               pcorerad instead of prad; added iradloss
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  28/07/14 PJK Subsumed routine into a module;
    !+ad_hisc               Added evaluation of residues etc. in physical
    !+ad_hisc               units
    !+ad_hist  01/10/14 PJK Added new eqn 15
    !+ad_hist  02/10/14 PJK Added new eqn 23
    !+ad_hist  06/10/14 PJK Added new eqn 59
    !+ad_hist  11/11/14 PJK Added new eqn 60
    !+ad_hist  12/11/14 PJK tcycle now a global variable
    !+ad_hist  13/11/14 PJK Changed iradloss usage in eqns 2 and 4
    !+ad_hist  17/11/14 PJK Added 'not recommended' comments to constraints 3 and 4
    !+ad_hist  25/11/14 JM  Added new eqn 61
    !+ad_hist  06/08/15 MDK Eqn 62: Issue #213 - lower limit on taup/taueff the ratio of particle to energy confinement times
    !+ad_hist  26/08/15 MDK Eqn 63: Issue #304 - upper limit on niterpump (vacuum_model = simple)
    !+ad_hist  18/11/15 RK  Eqn 64: Added constrain equation to limit Zeff
    !+ad_hist  26/11/15 RK  Eqn 65: Added constrain equation to set dump time
    !+ad_hist  24/05/16 JM  Added more information in the comments
    !+ad_hist  23/06/16 JM  Removed equation 38. No longer used anywhere in the code
    !+ad_hist  09/11/16 HL  Added new eqn 67
    !+ad_hist  25/01/17 JM  Added new eqn 68 for psep*b/q*A*r limit
    !+ad_hist  08/02/17 JM  Added constraint equations 69,70 and 71 for Kallenbach model
    !+ad_hist  27/02/17 JM  Added constraint equation 72 for Central Solenoid stress model
    !+ad_hist  20/04/17 JM  Added string tags to constraints
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Arguments !
    !!!!!!!!!!!!!

    integer, intent(in) :: m
    real(kind(1.0D0)), dimension(m), intent(out) :: cc
    real(kind(1.0D0)), optional, dimension(m), intent(out) :: con
    real(kind(1.0D0)), optional, dimension(m), intent(out) :: err
    character(len=1), optional, dimension(m), intent(out) :: symbol
    character(len=10), optional, dimension(m), intent(out) :: units
    integer, intent(in) :: ieqn

    ! Local variables !
    !!!!!!!!!!!!!!!!!!!

    integer :: i,i1,i2
    real(kind(1.0D0)) :: cratmx,pdenom,pnumerator,pradmaxpv
    real(kind(1.0D0)) :: pscaling,rcw,totmva

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! If ieqn is positive, only evaluate the 'ieqn'th constraint residue,
    ! otherwise evaluate all m constraint residues
    if (ieqn > 0) then
       i1 = ieqn ; i2 = ieqn
    else
       i1 = 1 ; i2 = m
    end if

    ! Consistency (equality) constraints should converge to zero.
    do i = i1,i2

       ! The constraint value in physical units is
       ! a) for consistency equations, the quantity to be equated, or
       ! b) for limit equations, the limiting value.
       ! The symbol is = for a consistency equation, < for an upper limit
       ! or > for a lower limit.
       select case (icc(i))

       case (1)  ! Relationship between beta, temperature (keV) and density
          ! This is a consistency equation
          !#=# physics
          !#=#=# consistency

          ! betaft |  fast alpha beta component
          ! betanb |  neutral beam beta component
          ! dene   |  electron density (m-3)
          ! ten    |  density weighted average electron temperature (keV)
          ! dnitot |  total ion density (m-3)
          ! tin    |  density weighted average ion temperature (keV)
          ! btot   |  total toroidal + poloidal field (T)
          ! beta   |  total plasma beta
          cc(i) = 1.0D0 - (betaft + betanb + 2.0D3*rmu0*echarge &
               * (dene*ten + dnitot*tin)/btot**2 )/beta

          if (present(con)) then
             con(i) = beta * (1.0D0 - cc(i))
             err(i) = beta * cc(i)
             symbol(i) = '='
             units(i) = ' '
          end if

       case (2)  ! Global plasma power balance equation
          ! This is a consistency equation
          !#=# physics
          !#=#=# consistency

          ! pscaling |  total transport power per volume (MW/m3)
          ! ptrepv   |  electron transport power per volume (MW/m3)
          ! ptripv   |  ion tranposrt power per volume (MW/m3)
          pscaling = ptrepv + ptripv

          ! iradloss | switch for radiation loss term usage in power balance
          ! = 0      | total power lost is scaling power plus radiation
          ! = 1      | total power lost is scaling power plus core radiation only
          ! = 2      | total power lost is scaling power only, with no additional
          !            allowance for radiation. This is not recommended for power plant models.
          if (iradloss == 0) then

             ! pradpv |  total radiation power per volume (MW/m3)
             pnumerator = pscaling + pradpv

          else if (iradloss == 1) then

             ! pcoreradpv |  total core radiation power per volume (MW/m3)
             pnumerator = pscaling + pcoreradpv

          else

             ! value just equals the total trasnport power per volume (MW/m3)
             pnumerator = pscaling

          end if

          ! ignite    |  switch for ignition assumption
          ! = 0       |  do not assume plasma ignition
          ! = 1       |  assume ignited
          ! falpha    |  fraction of alpha power deposited in plasma
          ! palppv    |  alpha power per volume (MW/m3)
          ! pchargepv |  non-alpha charged particle fusion power per volume (MW/m3)
          ! pohmpv    |  ohmic heating power per volume (MW/m3)
          ! pinjmw    |  total auxiliary injected power (MW)
          ! vol       |  plasma volume (m3)
          if (ignite == 0) then

             ! if plasma is not ignited then include incjected power
             pdenom = falpha*palppv + pchargepv + pohmpv + pinjmw/vol

          else

             ! if plasma is ignited
             pdenom = falpha*palppv + pchargepv + pohmpv

          end if

          cc(i) = 1.0D0 - pnumerator / pdenom

          if (present(con)) then
             con(i) = pdenom * (1.0D0 - cc(i))
             err(i) = pdenom * cc(i)
             symbol(i) = '='
             units(i) = 'MW/m3'
          end if

       case (3)  ! Global power balance equation for ions
          ! This is a consistency equation
          !#=# physics
          !#=#=# consistency

          ! N.B. This constraint is currently NOT RECOMMENDED for use.

          ! ignite    |  switch for ignition assumption
          ! = 0       |  do not assume plasma ignition
          ! = 1       |  assume ignited
          if (ignite == 0) then

             ! if plasma not ignited
             ! ptripv  |  ion transport power per volume (MW/m3)
             ! piepv   |  ion/electron equilibration power per volume (MW/m3)
             ! falpha  |  fraction of alpha power deposited in plasma
             ! palpipv |  alpha power per volume to ions (MW/m3)
             ! pinjimw |  auxiliary injected power to ions (MW)
             ! vol     |  plasma volume (m3)
             cc(i) = 1.0D0 - (ptripv + piepv) / &
                  (falpha*palpipv + pinjimw/vol)

             if (present(con)) then
                con(i) = (falpha*palpipv + pinjimw/vol) * (1.0D0 - cc(i))
                err(i) = (falpha*palpipv + pinjimw/vol) * cc(i)
                symbol(i) = '='
                units(i) = 'MW/m3'
             end if

          else

             ! if plasma ignited
             ! ptripv  |  ion transport power per volume (MW/m3)
             ! piepv   |  ion/electron equilibration power per volume (MW/m3)
             ! falpha  |  fraction of alpha power deposited in plasma
             ! palpipv |  alpha power per volume to ions (MW/m3)
             cc(i) = 1.0D0 - (ptripv+piepv) / (falpha*palpipv)

             if (present(con)) then
                con(i) = (falpha*palpipv) * (1.0D0 - cc(i))
                err(i) = (falpha*palpipv) * cc(i)
                symbol(i) = '='
                units(i) = 'MW/m3'
             end if

          end if

       case (4)  ! Global power balance equation for electrons
          ! This is a consistency equation
          !#=# physics
          !#=#=# consistency
          ! N.B. This constraint is currently NOT RECOMMENDED for use.

          ! pscaling |  total transport power per volume (MW/m3)
          ! ptrepv   |  electron transport power per volume (MW/m3)
          pscaling = ptrepv

          ! iradloss | switch for radiation loss term usage in power balance
          ! = 0      | total power lost is scaling power plus radiation
          ! = 1      | total power lost is scaling power plus core radiation only
          ! = 2      | total power lost is scaling power only, with no additional
          !            allowance for radiation. This is not recommended for power plant models.
          if (iradloss == 0) then

             ! pradpv |  total radiation power per volume (MW/m3)
             pnumerator = pscaling + pradpv

          else if (iradloss == 1) then

             ! pcoreradpv |  total core radiation power per volume (MW/m3)
             pnumerator = pscaling + pcoreradpv

          else

             ! value just equals the total trasnport power per volume (MW/m3)
             pnumerator = pscaling

          end if

          ! ignite  |  switch for ignition assumption
          ! = 0     |  do not assume plasma ignition
          ! = 1     |  assume ignited
          ! falpha  |  fraction of alpha power deposited in plasma
          ! palpepv |  alpha power per volume to electrons (MW/m3)
          ! piepv   |  ion/electron equilibration power per volume (MW/m3)
          ! pinjemw |  auxiliary injected power to electrons (MW)
          ! vol     |  plasma volume (m3)
          if (ignite == 0) then

             ! if plasma not ignited include injected power
             pdenom = falpha*palpepv + piepv + pinjemw/vol

          else

             ! if plasma ignited
             pdenom = falpha*palpepv + piepv

          end if

          cc(i) = 1.0D0 - pnumerator / pdenom
          if (present(con)) then
             con(i) = pdenom * (1.0D0 - cc(i))
             err(i) = pdenom * cc(i)
             symbol(i) = '='
             units(i) = 'MW/m3'
          end if

       case (5)  ! Equation for density upper limit
          !#=# physics
          !#=#=# fdene, dnelimt

          ! idensl |  switch for density limit to enforce
          ! = 1    |  old ASDEX;
          ! = 2    |  Borrass model for ITER (I);
          ! = 3    |  Borrass model for ITER (II);
          ! = 4    |  JET edge radiation;
          ! = 5    |  JET simplified;
          ! = 6    |  Hugill-Murakami Mq limit;
          ! = 7    |  Greenwald limit
          if (idensl == 7) then

             ! Apply Greenwald limit to line-averaged density
             ! fdene   |  f-value for density limit
             ! dnelimt |  density limit (m-3)
             ! dnla    |  line averaged electron density (m-3)
             cc(i) = 1.0D0 - fdene * dnelimt/dnla
             if (present(con)) then
                con(i) = fdene * dnelimt
                err(i) = fdene * dnelimt - dnla
                symbol(i) = '<'
                units(i) = '/m3'
             end if

          else

             ! fdene   |  f-value for density limit
             ! dnelimt |  density limit (m-3)
             ! dene    |  electron density (m-3)
             cc(i) = 1.0D0 - fdene * dnelimt/dene
             if (present(con)) then
                con(i) = dnelimt * (1.0D0 - cc(i))
                err(i) = dene * cc(i)
                symbol(i) = '<'
                units(i) = '/m3'
             end if

          end if

       case (6)  ! Equation for epsilon beta-poloidal upper limit
          !#=# physics
          !#=#=# fbeta, epbetmax

          ! fbeta    |  f-value for epsilon beta-poloidal
          ! epbetmax |  maximum (eps*beta_poloidal)
          ! eps      |  inverse aspect ratio
          ! betap    |  poloidal beta
          cc(i) = 1.0D0 - fbeta * epbetmax/(eps*betap)

          if (present(con)) then
             con(i) = epbetmax * (1.0D0 - cc(i))
             err(i) = (eps*betap) * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (7)  ! Equation for hot beam ion density
          ! This is a consistency equation (NBI)
          !#=# physics
          !#=#=# consistency

          ! ignite  |  switch for ignition assumption
          ! = 0     |  do not assume plasma ignition
          ! = 1     |  assume ignited
          if (ignite == 0) then

             ! if plasma not ignited
             ! dnbeam2 |  hot beam ion density from calculation (m-3)
             ! dnbeam  |  hot ion beam density, variable (m-3)
             cc(i) = 1.0D0 - dnbeam2/dnbeam

             if (present(con)) then
                con(i) = dnbeam * (1.0D0 - cc(i))
                err(i) = dnbeam * cc(i)
                symbol(i) = '='
                units(i) = '/m3'
             end if

          else

             call report_error(1)

          end if

       case (8)  ! Equation for neutron wall load upper limit
          !#=# physics
          !#=#=# fwalld, walalw

          ! fwalld |  f-value for maximum wall load
          ! walalw |  allowable wall load (MW/m2)
          ! wallmw |  average neutron wall load (MW/m2)
          cc(i) = 1.0D0 - fwalld * walalw/wallmw

          if (present(con)) then
             con(i) = fwalld * walalw
             err(i) = fwalld * walalw - wallmw
             symbol(i) = '<'
             units(i) = 'MW/m2'
          end if

       case (9)  ! Equation for fusion power upper limit
          !#=# physics
          !#=#=# ffuspow, powfmax

          ! ffuspow |  f-value for maximum fusion power
          ! powfmax |  maximum fusion power (MW)
          ! powfmw  |  fusion power (MW)
          cc(i) = 1.0D0 - ffuspow * powfmax/powfmw

          if (present(con)) then
             con(i) = powfmax * (1.0D0 - cc(i))
             err(i) = powfmw * cc(i)
             symbol(i) = '<'
             units(i) = 'MW'
          end if

       case (10)  ! Equation for field at TF coil
          ! This is a consistency equation
          ! (do not use for stellarators)
          !#=# tfcoil
          !#=#=# consistency

          ! rmajor |  plasma major radius (m)
          ! bt     |  toroidal field on axis (T)
          ! rbmax  |  radius of maximum toroidal field (m)
          ! bmaxtf |  peak field at toroidal field coil (T)
          cc(i) = 1.0D0 - (rmajor*bt)/(rbmax*bmaxtf)

          if (present(con)) then
             con(i) = (rbmax*bmaxtf) * (1.0D0 - cc(i))
             err(i) = (rbmax*bmaxtf) * cc(i)
             symbol(i) = '='
             units(i) = 'T.m'
          end if

       case (11)  ! Equation for radial build
          ! This is a consistency equation
          !#=# build
          !#=#=# consistency

          ! rbld   |  sum of thicknesses to plasma major radius (m)
          ! rmajor |  plasma major radius (m)
          cc(i) = 1.0D0 - rbld/rmajor

          if (present(con)) then
             con(i) = rmajor * (1.0D0 - cc(i))
             err(i) = rmajor * cc(i)
             symbol(i) = '='
             units(i) = 'm'
          end if

       case (12)  ! Equation for volt-second capability lower limit
          !#=# pfcoil
          !#=#=# fvs, vsstt

          ! vsstt (lower limit) is positive; vstot (available) is negative
          ! fvs   |  f-value for flux swing (V-s) requirement
          ! vstot |  total flux swing for pulse (Wb)
          ! vsstt |  total V-s needed (Wb)
          cc(i) = 1.0D0 + fvs * vstot/vsstt

          if (present(con)) then
             con(i) = vsstt * (1.0D0 - cc(i))
             err(i) = vsstt * cc(i)
             symbol(i) = '>'
             units(i) = 'V.sec'
          end if

       case (13)  ! Equation for burn time lower limit
          !#=# times
          !#=#=# ftburn, tbrnmn

          ! ftburn |  f-value for minimum burn time
          ! tburn  |  burn time (s)
          ! tbrnmn |  minimum burn time (s)
          cc(i) = 1.0D0 - ftburn * tburn/tbrnmn

          if (present(con)) then
             con(i) = tbrnmn / ftburn
             err(i) = tbrnmn / ftburn  - tburn
             symbol(i) = '>'
             units(i) = 'sec'
          end if

       case (14)  ! Equation to fix number of NBI decay lengths to plasma centre
          ! This is a consistency equation
          !#=# current_drive
          !#=#=# consistency

          ! taubeam |  neutral beam e-decay lengths to plasma centre
          ! tbeamin |  permitted neutral beam e-decay lengths to plasma centre
          cc(i) = 1.0D0 - taubeam/tbeamin

          if (present(con)) then
             con(i) = tbeamin * (1.0D0 - cc(i))
             err(i) = tbeamin * cc(i)
             symbol(i) = '='
             units(i) = ''
          end if

       case (15)  ! Equation for L-H power threshold limit
          !#=# physics
          !#=#=# flhthresh, plhthresh

          ! flhthresh |  f-value for L-H power threshold
          ! plhthresh |  L-H mode power threshold (MW)
          ! pdivt     |  power conducted to the divertor region (MW)
          cc(i) = -(1.0D0 - flhthresh * plhthresh / pdivt)

          if (present(con)) then
             con(i) = plhthresh
             err(i) = plhthresh - pdivt / flhthresh
             if (flhthresh > 1.0D0) then
                symbol(i) = '>'
             else
                symbol(i) = '<'
             end if
             units(i) = 'MW'
          end if

       case (16)  ! Equation for net electric power lower limit
          !#=# heat_transport
          !#=#=# fpnetel, pnetelin

          ! fpnetel  |  f-value for net electric power
          ! pnetelmw |  net electric power (MW)
          ! pnetelin |  required net electric power (MW)
          cc(i) = 1.0D0 - fpnetel * pnetelmw / pnetelin

          if (present(con)) then
             con(i) = pnetelin
             err(i) = pnetelmw - pnetelin / fpnetel
             symbol(i) = '>'
             units(i) = 'MW'
          end if

       case (17)  ! Equation for radiation power upper limit
          !#=# physics
          !#=#=# fradpwr, pradmaxpv

          ! pradmaxpv is the maximum possible power/vol that can be radiated
          ! falpha included to take into account alpha power not deposited in plasma

          ! pinjmw    |  total auxiliary injected power (MW)
          ! vol       |  plasma volume (m3)
          ! palppv    |  alpha power per volume (MW/m3)
          ! falpha    |  fraction of alpha power deposited in plasma
          ! pchargepv |  non-alpha chraged particle fusion power per volume (MW/m3)
          ! pohmpv    |  ohmic heating power per volume (MW/m3)
          pradmaxpv = pinjmw/vol + palppv*falpha + pchargepv + pohmpv

          ! fradpwr |  f-value for core radiation power limit
          ! pradpv  |  total radiation power per volume (MW/m3)
          cc(i) = 1.0D0 - fradpwr * pradmaxpv / pradpv

          if (present(con)) then
             con(i) = pradmaxpv * (1.0D0 - cc(i))
             err(i) = pradpv * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m3'
          end if

       case (18)  ! Equation for divertor heat load upper limit
          !#=# divertor
          !#=#=# fhldiv, hldivlim

          ! fhldiv   |  f-value for divertor heat load
          ! hldivlim |  divertor heat load limit (MW/m2)
          ! hldiv    |  divetor heat load (MW/m2)
          cc(i) = 1.0D0 - fhldiv * hldivlim/hldiv

          if (present(con)) then
             con(i) = hldivlim * (1.0D0 - cc(i))
             err(i) = hldiv * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m2'
          end if

       case (19)  ! Equation for MVA upper limit
          !#=# tfcoil
          !#=#=# fmva, mvalim

          ! totmva  |  total MVA in TF coil (MW)
          ! tfcpmw  |  peak resistive TF coil inboard leg power (MW)
          ! tflegmw |  TF coil outboard leg resistive power (MW)
          totmva = tfcpmw + tflegmw

          ! fmva   |  f-value for maximum MVA
          ! mvalim |  maximum MVA limit
          cc(i) = 1.0D0 - fmva * mvalim/totmva

          if (present(con)) then
             con(i) = mvalim * (1.0D0 - cc(i))
             err(i) = totmva * cc(i)
             symbol(i) = '<'
             units(i) = 'MVA'
          end if

       case (20)  ! Equation for neutral beam tangency radius upper limit
          !#=# current_drive
          !#=#=# fportsz, rtanmax

          ! fportsz  |  f-value for neutral beam tangency radius limit
          ! rtanmax  |  maximum tangency radius for centreline of beam (m)
          ! rtanbeam |  neutral beam centreline tangency radius (m)
          cc(i) = 1.0D0 - fportsz * rtanmax/rtanbeam

          if (present(con)) then
             con(i) = rtanmax * (1.0D0 - cc(i))
             err(i) = rtanbeam * cc(i)
             symbol(i) = '<'
             units(i) = 'm'
          end if

       case (21)  ! Equation for minor radius lower limit
          !#=# physics
          !#=#=# frminor, aplasmin

          ! frminor  |  f-value for minor radius limit
          ! rminor   |  plasma minor radius (m)
          ! aplasmin |  minimum minor radius (m)
          cc(i) = 1.0D0 - frminor * rminor/aplasmin

          if (present(con)) then
             con(i) = aplasmin * (1.0D0 - cc(i))
             err(i) = aplasmin * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (22)  ! Equation for divertor collision/connection length ratio upper limit
          !#=# divertor
          !#=#=# fdivcol, rlenmax

          ! fdivcol  |  f-value for divertor collisionality
          ! rlenmax  |  maximum value for length ratio
          ! rlclolcn |  ratio of collision length / connection length
          cc(i) = 1.0D0 - fdivcol * rlenmax / rlclolcn

          if (present(con)) then
             con(i) = rlenmax * (1.0D0 - cc(i))
             err(i) = rlclolcn * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (23)  ! Equation for conducting shell radius / rminor upper limit
          !#=# physics
          !#=#=# fcwr, cwrmax

          ! rcw     |  conducting shell radius (m)
          ! rminor  |  plasma minor radius (m)
          ! scraplo |  outboard scrape-off-layer thickness (m)
          ! fwoth   |  outboard first wall thickness (m)
          ! blnkoth |  outboard blanket thickness (m)
          rcw = rminor + scraplo + fwoth + blnkoth

          ! fcwr   |  f-value for conducting wall radius / rminor limit
          ! cwrmax |  maximum ratio of conducting wall distance to plasma minor
          !           radius for vertical stability
          cc(i) = 1.0D0 - fcwr * cwrmax*rminor / rcw

          if (present(con)) then
             con(i) = cwrmax*rminor * (1.0D0 - cc(i))
             err(i) = rcw * cc(i)
             symbol(i) = '<'
             units(i) = 'm'
          end if

       case (24)  ! Equation for beta upper limit
          !#=# physics
          !#=#=# fbetatry, betalim

          ! iculbl |  switch for beta limit scaling
          ! = 0    |  apply limit to total beta
          ! = 1    |  apply limit to thermal beta
          ! = 2    |  apply limit to thermal + neutral beam beta
          ! istell |  switch for stellarator option
          ! = 0    |  use tokamak model
          ! = 1    |  use stellarator model
          if ((iculbl == 0).or.(istell /= 0)) then

             ! Include all beta components
             ! Relevant for both tokamaks and stellarators
             ! fbetatry |  f-value for beta limit
             ! betalim  |  allowable beta
             ! beta     |  total plasma beta
             cc(i) = 1.0D0 - fbetatry * betalim/beta

             if (present(con)) then
                con(i) = betalim
                err(i) = betalim - beta / fbetatry
                symbol(i) = '<'
                units(i) = ''
             end if

          else if (iculbl == 1) then

             ! Here, the beta limit applies to only the thermal
             ! component, not the fast alpha or neutral beam parts
             ! fbetatry |  f-value for beta limit
             ! betalim  |  allowable beta
             ! beta     |  total plasma beta
             ! betaft   |  fast alpha beta component
             ! betanb   |  neutral beam beta component
             cc(i) = 1.0D0 - fbetatry * betalim/(beta-betaft-betanb)

             if (present(con)) then
                con(i) = betalim
                err(i) = betalim - (beta-betaft-betanb) / fbetatry
                symbol(i) = '<'
                units(i) = ''
             end if

          else  !  iculbl == 2

             ! Beta limit applies to thermal + neutral beam
             ! components of the total beta, i.e. excludes alphas
             ! fbetatry |  f-value for beta limit
             ! betalim  |  allowable beta
             ! beta     |  total plasma beta
             ! betaft   |  fast alpha beta component
             cc(i) = 1.0D0 - fbetatry * betalim/(beta-betaft)

             if (present(con)) then
                con(i) = betalim * (1.0D0 - cc(i))
                err(i) = (beta-betaft) * cc(i)
                symbol(i) = '<'
                units(i) = ''
             end if

          end if

       case (25)  ! Equation for peak toroidal field upper limit
          !#=# tfcoil
          !#=#=# fpeakb, bmxlim

          ! fpeakb |  f-value for maximum toroidal field
          ! bmxlim |  maximum peak toroidal field (T)
          ! bmaxtf |  peak field at TF coil (T)
          cc(i) = 1.0D0 - fpeakb * bmxlim/bmaxtf

          if (present(con)) then
             con(i) = bmxlim * (1.0D0 - cc(i))
             err(i) = bmaxtf * cc(i)
             symbol(i) = '<'
             units(i) = 'T'
          end if

       case (26)  ! Equation for Central Solenoid current density upper limit at EOF
          !#=# pfcoil
          !#=#=# fjohc, rjohc

          ! fjohc  |  f-value for central solenoid current at end-of-flattop
          ! rjohc  |  allowable central solenoid current density at end of flat-top (A/m2)
          ! coheof |  central solenoid overall current density at end of flat-top (A/m2)
          cc(i) = 1.0D0 - fjohc * rjohc/coheof

          if (present(con)) then
             con(i) = rjohc
             err(i) = rjohc - coheof / fjohc
             symbol(i) = '<'
             units(i) = 'A/m2'
          end if

       case (27)  ! Equation for Central Solenoid current density upper limit at BOP
          !#=# pfcoil
          !#=#=# fjohc0, rjohc0

          ! fjohc0 |  f-value for central solenoid current at beginning of pulse
          ! rjohc0 |  allowable central solenoid current density at beginning of pulse (A/m2)
          ! cohbop |  central solenoid overall current density at beginning of pulse (A/m2)
          cc(i) = 1.0D0 - fjohc0 * rjohc0/cohbop

          if (present(con)) then
             con(i) = rjohc0
             err(i) = rjohc0 - cohbop / fjohc0
             symbol(i) = '<'
             units(i) = 'A/m2'
          end if

       case (28)  ! Equation for fusion gain (big Q) lower limit
          !#=# physics
          !#=#=# fqval, bigqmin

          ! ignite    |  switch for ignition assumption
          ! = 0       |  do not assume plasma ignition
          ! = 1       |  assume ignited
          if (ignite == 0) then

             ! if plasma is not ignited
             ! fqval   |  f-value for Q
             ! bigq    |  fusion gain; P_fusion / (P_injection + P_ohmic)
             ! bigqmin |  minimum fusion gain Q
             cc(i) = 1.0D0 - fqval * bigq/bigqmin

             if (present(con)) then
                con(i) = bigqmin * (1.0D0 - cc(i))
                err(i) = bigqmin * cc(i)
                symbol(i) = '>'
                units(i) = ''
             end if

          else

             ! if plasma is ignited report error
             call report_error(4)

          end if

       case (29)  ! Equation for inboard major radius
          ! This is a consistency equation
          !#=# build
          !#=#=# consistency

          ! rmajor   |  plasma major radius (m)
          ! rminor   |  plasma minor radius (m)
          ! rinboard |  plasma inboard radius (m)
          cc(i) = 1.0D0 - (rmajor - rminor) / rinboard

          if (present(con)) then
             con(i) = rinboard * (1.0D0 - cc(i))
             err(i) = rinboard * cc(i)
             symbol(i) = '='
             units(i) = 'm'
          end if

       case (30)  ! Equation for injection power upper limit
          !#=# current_drive
          !#=#=# fpinj, pinjalw

          ! Usual form is inverted to prevent problems when injection power is zero
          ! pinjmw  |  total auxiliary injected power (MW)
          ! fpinj   |  f-value for injection power
          ! pinjalw |  maximum allowable value for injection power (MW)
          cc(i) = 1.0D0 - pinjmw / (fpinj * pinjalw)

          if (present(con)) then
             con(i) = pinjalw
             err(i) = pinjalw  - pinjmw * fpinj
             symbol(i) = '<'
             units(i) = 'MW'
          end if

       case (31)  ! Equation for TF coil case stress upper limit (SCTF)
          !#=# tfcoil
          !#=#=# fstrcase, alstrtf

          ! fstrcase |  f value for TF coil case stress
          ! alstrtf  |  allowable Tresca stress in TF coil structural material (MPa)
          ! strtf2   |  Tresca stress in TF coil case (MPa)
          cc(i) = 1.0D0 - fstrcase * alstrtf/strtf2

          if (present(con)) then
             con(i) = alstrtf
             err(i) = alstrtf - strtf2 / fstrcase
             symbol(i) = '<'
             units(i) = 'Pa'
          end if

       case (32)  ! Equation for TF coil conduit stress upper limit (SCTF)
          !#=# tfcoil
          !#=#=# fstrcond, alstrtf

          ! fstrcond |  f-value for TF coil conduit stress
          ! alstrtf  |  allowable Tresca stress in TF coil structural material (MPa)
          ! strtf1   |  Tresca stress in TF coil conduit (MPa)
          cc(i) = 1.0D0 - fstrcond * alstrtf/strtf1

          if (present(con)) then
             con(i) = alstrtf
             err(i) = alstrtf - strtf1 / fstrcond
             symbol(i) = '<'
             units(i) = 'Pa'
          end if

       case (33)  ! Equation for TF coil operating/critical J upper limit (SCTF)
          !#=# tfcoil
          !#=#=# fiooic, jwdgcrt

          ! fiooic  |  f-value for TF coil operating current / critical current ratio
          ! jwdgcrt |  critical current density for winding pack (A/m2)
          ! jwptf   |  winding pack current density (A/m2)
          cc(i) = 1.0D0 - fiooic * jwdgcrt/jwptf

          if (present(con)) then
             con(i) = jwdgcrt * (1.0D0 - cc(i))
             err(i) = jwptf * cc(i)
             symbol(i) = '<'
             units(i) = 'A/m2'
          end if

       case (34)  ! Equation for TF coil dump voltage upper limit (SCTF)
          !#=# tfcoil
          !#=#=# fvdump, vdalw

          ! fvdump |  f-value for dump voltage
          ! vdalw  |  max voltage across TF coil during quench (kV)
          ! vtfskv |  voltage across TF coil during quench (kV)
          cc(i) = 1.0D0 - fvdump * vdalw/vtfskv

          if (present(con)) then
             con(i) = vdalw
             err(i) = vdalw - vtfskv
             symbol(i) = '<'
             units(i) = 'V'
          end if

       case (35)  ! Equation for TF coil J_wp/J_prot upper limit (SCTF)
          !#=# tfcoil
          !#=#=# fjprot, jwdgpro

          ! fjprot  |  f-value for TF coil winding pack current density
          ! jwdgpro |  allowable TF coil winding pack current density, for dump
          !            temperature rise protection (A/m2)
          ! jwptf   |  winding pack current density (A/m2)
          cc(i) = 1.0D0 - fjprot * jwdgpro/jwptf

          if (present(con)) then
             con(i) = jwdgpro
             err(i) = jwptf - jwptf
             symbol(i) = '<'
             units(i) = 'A/m2'
          end if

       case (36)  ! Equation for TF coil s/c temperature margin lower limit (SCTF)
          !#=# tfcoil
          !#=#=# ftmargtf, tmargmin_tf

          ! ftmargtf |  f-value for TF coil temperature margin
          ! tmargtf  |  TF coil temperature margin (K)
          ! tmargmin_tf |  minimum allowable temperature margin (K)
          cc(i) = 1.0D0 - ftmargtf * tmargtf/tmargmin_tf

          if (present(con)) then
             con(i) = tmargmin_tf
             err(i) = tmargmin_tf - tmargtf
             symbol(i) = '>'
             units(i) = 'K'
          end if

       case (37)  ! Equation for current drive gamma upper limit
          !#=# current_drive
          !#=#=# fgamcd, gammax

          ! fgamcd |  f-value for current drive gamma
          ! gammax |  maximum current drive gamma
          ! gamcd  |  normalised current drive efficiency (1.0e20 A/W-m2)
          cc(i) = 1.0D0 - fgamcd * gammax/gamcd

          if (present(con)) then
             con(i) = gammax * (1.0D0 - cc(i))
             err(i) = gamcd * cc(i)
             symbol(i) = '<'
             units(i) = '1E20 A/Wm2'
          end if

          !#=# empty
          !#=#=# empty

       case (39)  ! Equation for first wall temperature upper limit
          ! Issue #348 (15/12/02)
          !#=# fwbs
          !#=#=# ftpeak, tfwmatmax

          ! If the temperature peak == 0 then report an error
          if (tpeak < 1.0D0) call report_error(5)

          ! ftpeak    |  f-value for first wall peak temperature
          ! tfwmatmax |  maximum temperature of first wall material (K)
          ! tpeak     |  peak first wall temperature (K)
          cc(i) = 1.0D0 - ftpeak * tfwmatmax/tpeak

          if (present(con)) then
             con(i) = tfwmatmax * (1.0D0 - cc(i))
             err(i) = tpeak * cc(i)
             symbol(i) = '<'
             units(i) = 'K'
          end if

       case (40)  ! Equation for auxiliary power lower limit
          !#=# current_drive
          !#=#=# fauxmn, auxmin

          ! fauxmn |  f-value for minimum auxiliary power
          ! pinjmw |  total auxiliary injected power (MW)
          ! auxmin |  minimum auxiliary power (MW)
          cc(i) = 1.0D0 - fauxmn * pinjmw/auxmin

          if (present(con)) then
             con(i) = auxmin * (1.0D0 - cc(i))
             err(i) = auxmin * cc(i)
             symbol(i) = '>'
             units(i) = 'MW'
          end if

       case (41)  ! Equation for plasma current ramp-up time lower limit
          !#=# times
          !#=#=# ftohs, tohsmn

          ! ftohs  |  f-value for plasma current ramp up time
          ! tohs   |  plasma current ramp up time for current initiation (s)
          ! tohsmn |  minimum plasma current ramp up time (s)
          cc(i) = 1.0D0 - ftohs * tohs/tohsmn

          if (present(con)) then
             con(i) = tohsmn * (1.0D0 - cc(i))
             err(i) = tohsmn * cc(i)
             symbol(i) = '>'
             units(i) = 'sec'
          end if

       case (42)  ! Equation for cycle time lower limit
          !#=# times
          !#=#=# ftcycl, tcycmn

          ! if the minimum cycle time == 0 report an error
          if (tcycmn < 1.0D0) call report_error(6)

          ! ftcycl |  f-value for cycle time
          ! tcycle |  full cycle time (s)
          ! tcycmn |  minimum cycle time (s)
          cc(i) = 1.0D0 - ftcycl * tcycle/tcycmn

          if (present(con)) then
             con(i) = tcycmn * (1.0D0 - cc(i))
             err(i) = tcycmn * cc(i)
             symbol(i) = '>'
             units(i) = 'sec'
          end if

       case (43)  ! Equation for average centrepost temperature
          ! This is a consistency equation (TART)
          !#=# tfcoil
          !#=#=# consistency

          ! if the machine isn't a ST then report error
          if (itart == 0) call report_error(7)

          ! tcpav  | average temp of TF coil inboard leg conductor (C)
          ! tcpav2 | centrepost average temperature (C)
          cc(i) = 1.0D0 - tcpav/tcpav2

          if (present(con)) then
             con(i) = tcpav2 * (1.0D0 - cc(i))
             err(i) = tcpav2 * cc(i)
             symbol(i) = '='
             units(i) = 'deg C'
          end if

       case (44)  ! Equation for centrepost temperature upper limit (TART)
          !#=# tfcoil
          !#=#=# fptemp, ptempalw

          ! if the machine isn't a ST then report error
          if (itart == 0) call report_error(8)

          ! fptemp   |  f-value for peak centrepost temperature
          ! ptempalw |  maximum peak centrepost temperature (C)
          ! tcpmax   |  peak centrepost temperature (C)
          cc(i) = 1.0D0 - fptemp * ptempalw / tcpmax

          if (present(con)) then
             con(i) = ptempalw * (1.0D0 - cc(i))
             err(i) = tcpmax * cc(i)
             symbol(i) = '<'
             units(i) = 'deg C'
          end if

       case (45)  ! Equation for edge safety factor lower limit (TART)
          !#=# tfcoil
          !#=#=# fq, qlim

          ! if the machine isn't a ST then report error
          if (itart == 0) call report_error(9)

          ! fq   |  f-value for edge safety factor
          ! q    |  safety factor 'near' plasma edge equal to q95 (unless
          !         icurr = 2 (ST current scaling),
          ! qlim |  lower limit for edge safety factor
          cc(i) = 1.0D0 - fq * q/qlim

          if (present(con)) then
             con(i) = qlim * (1.0D0 - cc(i))
             err(i) = qlim * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (46)  ! Equation for Ip/Irod upper limit (TART)
          !  This is a q-edge type limit for certain aspect ratios
          !#=# tfcoil
          !#=#=# fipir, cratmx

          ! if the machine isn't a ST then report error
          if (itart == 0) call report_error(10)

          ! cratmx |  maximum ratio of plasma current to centrepost current
          ! eps    |  inverse aspect ratio
          cratmx = 1.0D0 + 4.91D0*(eps-0.62D0)

          ! fipir   |  f-value for Ip / Irod limit
          ! ritfc   |  total summed current in TF coils (A)
          ! plascur |  plasma current (A)
          cc(i) = 1.0D0 - fipir * cratmx * ritfc/plascur

          if (present(con)) then
             con(i) = cratmx * (1.0D0 - cc(i))
             err(i) = plascur/ritfc * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (47)  ! Equation for TF coil toroidal thickness upper limit
          ! Issue #508 Remove RFP option
          ! Relevant only to reversed field pinch devices
          !#=# empty
          !#=#=# empty

       case (48)  ! Equation for poloidal beta upper limit
          !#=# physics
          !#=#=# fbetap, betpmx

          ! fbetap |  f-value for poloidal beta
          ! betpmx |  maximum poloidal beta
          ! betap  |  poloidal beta
          cc(i) = 1.0D0 - fbetap * betpmx/betap

          if (present(con)) then
             con(i) = betpmx * (1.0D0 - cc(i))
             err(i) = betap * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (49)  ! Issue #508 Remove RFP option Equation to ensure reversal parameter F is negative
        !#=# empty
        !#=#=# empty

       case (50)  ! Issue #508 Remove IFE option: Equation for repetition rate upper limit
        !#=# empty
        !#=#=# empty

       case (51)  ! Equation to enforce startup flux = available startup flux
          ! This is a consistency equation
          !#=# pfcoil
          !#=#=# consistency

          ! vsres |  resistive losses in startup V-s (Wb)
          ! vsind |  internal and external plasma inductance V-s (Wb)
          ! vssu  |  total flux swing for startup (Wb)
          cc(i) = 1.0D0 - (vsres+vsind) / vssu

          if (present(con)) then
             con(i) = vssu * (1.0D0 - cc(i))
             err(i) = vssu * cc(i)
             symbol(i) = '='
             units(i) = 'V.s'
          end if

       case (52)  ! Equation for tritium breeding ratio lower limit
          !#=# fwbs
          !#=#=# ftbr, tbrmin

          ! TODO should this only be for certain blanket models?

          ! ftbr   |  f-value for minimum tritium breeding ratio
          ! tbr |  tritium breeding ratio
          ! tbrmin |  minimum tritium breeding ratio
          cc(i) = 1.0D0 - ftbr * tbr/tbrmin

          if (present(con)) then
             con(i) = tbrmin * (1.0D0 - cc(i))
             err(i) = tbrmin * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (53)  ! Equation for fast neutron fluence on TF coil upper limit
          !#=# fwbs
          !#=#=# fflutf, nflutfmax

          ! fflutf    |  f-value for neutron fluence on TF coil
          ! nflutfmax |  max fast neutron fluence on TF coil (n/m2)
          ! nflutf    |  peak fast neutron fluence on TF coil superconductor (n/m2)
          cc(i) = 1.0D0 - fflutf * nflutfmax/nflutf

          if (present(con)) then
             con(i) = nflutfmax * (1.0D0 - cc(i))
             err(i) = nflutf * cc(i)
             symbol(i) = '<'
             units(i) = 'neutron/m2'
          end if

       case (54)  ! Equation for peak TF coil nuclear heating upper limit
          !#=# fwbs
          !#=#=# fptfnuc, ptfnucmax

          ! fptfnuc   |  f-value for maximum TF coil nuclear heating
          ! ptfnucmax |  maximum nuclear heating in TF coil (MW/m3)
          ! ptfnucpm3 |  nuclear heating in the TF coil (MW/m3)
          cc(i) = 1.0D0 - fptfnuc * ptfnucmax/ptfnucpm3

          if (present(con)) then
             con(i) = ptfnucmax * (1.0D0 - cc(i))
             err(i) = ptfnucpm3 * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m3'
          end if

       case (55)  ! Equation for helium concentration in vacuum vessel upper limit
          !#=# fwbs
          !#=#=# fvvhe, vvhemax

          ! fvvhe   |  f-value for vacuum vessel He concentration limit
          ! vvhealw |  allowed maximum helium concentration in vacuum vessel
          !            at end of plant life (appm)
          ! vvhemax |  maximum helium concentration in vacuum vessel at end of
          !            plant life (appm) (iblanket=2 (KIT HCPB))
          if (iblanket == 2) then
             cc(i) = 1.0D0 - fvvhe * vvhealw/vvhemax

             if (present(con)) then
                con(i) = vvhealw * (1.0D0 - cc(i))
                err(i) = vvhemax * cc(i)
                symbol(i) = '<'
                units(i) = 'appm'
             end if
          else
             !if iblanket != 2
             call report_error(173)
          end if

       case (56)  ! Equation for power through separatrix / major radius upper limit
          !#=# physics
          !#=#=# fpsepr, pseprmax

          ! fpsepr   |  f-value for maximum Psep/R limit
          ! pseprmax |  maximum ratio of power crossing the separatrix to plasma
          !             major radius (Psep/R) (MW/m)
          ! pdivt    |  power conducted to the divertor region (MW)
          ! rmajor   |  plasma major radius (m)
          cc(i) = 1.0D0 - fpsepr * pseprmax / (pdivt/rmajor)

          if (present(con)) then
             con(i) = pseprmax * (1.0D0 - cc(i))
             err(i) = (pdivt/rmajor) * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m'
          end if

       case (57)  ! Obsolete
        !#=# empty
        !#=#=# empty

       case (58)  ! Obsolete
        !#=# empty
        !#=#=# empty

       case (59)  ! Equation for neutral beam shine-through fraction upper limit
          !#=# current_drive
          !#=#=# fnbshinef, nbshinefmax

          ! fnbshinef   |  f-value for maximum neutral beam shine-through fraction
          ! nbshinefmax |  maximum neutral beam shine-through fraction
          ! nbshinef    |  neutral beam shine-through fraction
          cc(i) = 1.0D0 - fnbshinef * nbshinefmax / nbshinef

          if (present(con)) then
             con(i) = nbshinefmax * (1.0D0 - cc(i))
             err(i) = nbshinef * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

      case (60)  ! Equation for Central Solenoid s/c temperature margin lower limit
          !#=# tfcoil
          !#=#=# ftmargoh, tmargmin_cs

          ! ftmargoh |  f-value for central solenoid temparature margin
          ! tmargoh  |  central solenoid temprature margin (K)
          ! tmargmin_cs |  minimum allowable temperature margin (K)
          cc(i) = 1.0D0 - ftmargoh * tmargoh/tmargmin_cs

          if (present(con)) then
             con(i) = tmargmin_cs
             err(i) = tmargmin_cs - tmargoh
             symbol(i) = '>'
             units(i) = 'K'
          end if

       case (61)  ! Equation for availability limit
          !#=# cost
          !#=#=# favail, avail_min

          ! favail    |  f-value for minimum availability
          ! cfactr    |  total plant availability fraction
          ! avail_min |  minimum availability
          cc(i) = 1.0D0 - favail * cfactr / avail_min

          if (present(con)) then
             con(i) = avail_min * (1.0D0 - cc(i))
             err(i) = cfactr * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (62)  ! Lower limit on taup/taueff the ratio of alpha particle to energy confinement times
          !#=# physics
          !#=#=# ftaulimit, taulimit

          ! ftaulimit |  f-value for lower limit on taup/taueff the ratio of alpha particle to energy confinement
          ! taup      |  alpha particle confinement time (s)
          ! taueff    |  global energy confinement time (s)
          ! taulimit  |  Lower limit on taup/taueff the ratio of alpha particle to energy confinement times
          cc(i) = 1.0D0 - ftaulimit * (taup / taueff) / taulimit

          if (present(con)) then
             con(i) = taulimit
             err(i) = (taup / taueff) * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (63)  ! Upper limit on niterpump (vacuum_model = simple)
          !#=# vacuum
          !#=#=# fniterpump, tfno

          ! fniterpump |  f-value for constraint that number of pumps < tfno
          ! tfno       |  number of TF coils
          ! niterpump  |  number of high vacuum pumps (real number), each with the throughput
          !               of one ITER cryopump (50 Pa m3 s-1), all operating at the same time
          !               (vacuum_model = 'simple')
          cc(i) = 1.0D0 - fniterpump * tfno / niterpump

          if (present(con)) then
            con(i) = tfno
            err(i) = tfno * cc(i)
            symbol(i) = '<'
            units(i) = ''
          end if

       case (64)  ! Upper limit on Zeff
          !#=# physics
          !#=#=# fzeffmax, zeffmax

          ! fzeffmax |  f-value for maximum zeff
          ! zeffmax  |  maximum value for zeff
          ! zeff     |  plasma effective charge
          cc(i) = 1.0D0 - fzeffmax * (zeffmax/zeff)

          if (present(con)) then
            con(i) = zeffmax
            err(i) = zeffmax * cc(i)
            symbol(i) = '<'
            units(i) = ''
          end if

       case (65)  ! Limit TF dump time to calculated quench time
           ! (IDM: 2MBSE3)
           !#=# tfcoil
           !#=#=# ftaucq, taucq

           ! ftaucq |  f-value for calculated minimum TF quench time
           ! tdmptf |  dump time for TF coil (s)
           ! taucq  |  allowable TF quench time (s)
           cc(i) =  1.0d0 - ftaucq * tdmptf / taucq

           if (present(con)) then
             con(i) = taucq
             err(i) = taucq - tdmptf
             symbol(i) = '>'
             units(i) = 's'
           end if

       case (66)  ! Limit on rate of change of energy in poloidal field
           !#=# pfcoil
           !#=#=# fpoloidalpower, maxpoloidalpower

           ! fpoloidalpower    |  f-value for constraint on rate of change of energy in poloidal field
           ! maxpoloidalpower  |  Maximum permitted absolute rate of change of stored energy in poloidal field (MW)
           ! peakpoloidalpower |  Peak absolute rate of change of stored energy in poloidal field (MW)
           cc(i) = 1.0d0 - fpoloidalpower * maxpoloidalpower / peakpoloidalpower

           if (present(con)) then
             con(i) = maxpoloidalpower
             err(i) = maxpoloidalpower * cc(i)
             symbol(i) = '<'
             units(i) = 'MW'
           end if

       case (67) ! Simple upper limit on radiation wall load
           !#=# physics
           !#=#=# fradwall, maxradwallload

           ! fradwall    |  f-value for upper limit on radiation wall load
           ! maxradwallload  |  Maximum permitted radiation wall load (MW/m^2)
           ! peakradwallload |  Peak radiation wall load (MW/m^2)
           cc(i) = 1.0d0 - fradwall * maxradwallload / peakradwallload

           if (present(con)) then
             con(i) = maxradwallload
             err(i) =  maxradwallload * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m^2'
           end if

       case (68) ! New Psep scaling (PsepB/qAR)
           !#=# physics
           !#=#=# fpsepbqar, psepbqarmax

           ! Issue #464
           ! fpsepbqar       |  f-value for upper limit on psepbqar
           ! psepbqarmax     |  Maximum permitted PsepB/qAR (MWT/m)
           ! pdivt           |  Power going to divertor (Psep) (MW)
           ! bt              |  Toroidal field on-axis (T)
           ! q95             |  Safety factor q on 95% flux surface
           ! aspect          |  aspect ratio
           ! rmajor          |  plasma major radius (m)
           cc(i) = 1.0d0 - fpsepbqar * psepbqarmax / ((pdivt*bt)/(q95*aspect*rmajor))

           if (present(con)) then
             con(i) = psepbqarmax
             err(i) = (pdivt*bt)/(q95*aspect*rmajor) - psepbqarmax
             symbol(i) = '<'
             units(i) = 'MWT/m'
           end if


       case (69)  ! ensure separatrix power is less than value from Kallenbach divertor
           !#=# divertor_kallenbach
           !#=#=# fpsep, psep_kallenbach

           ! fpsep             | f-value for consistency of two values of separatrix power
           ! psep_kallenbach   | Power conducted through the separatrix, as calculated by the divertor model [W].
           ! pdivt             |  power to conducted to the divertor region (MW)
           !cc(i) = 1.0d0 - fpsep * (psep_kallenbach/1.0d6) / pdivt
           cc(i) = 1.0d0 - (psep_kallenbach/1.0d6) / pdivt

           if (present(con)) then
             con(i) = psep_kallenbach/1.0d6
             err(i) = psep_kallenbach/1.0d6 * cc(i)
             !symbol(i) = '<'
             symbol(i) = '='
             units(i) = 'MW'
           end if

       case (70)  ! Separatrix temperature consistency
           !#=# divertor_kallenbach
           !#=#=# consistency

           ! teomp   | separatrix temperature calculated by the Kallenbach divertor model [eV]
           ! tesep  | electron temperature at separatrix [keV]
           cc(i) = 1.0D0 - teomp/(1000.0D0*tesep)

           if (present(con)) then
             con(i) = teomp
             err(i) = teomp* cc(i)
             symbol(i) = '='
             units(i) = 'eV'
           end if

       case (71)  ! Separatrix density consistency
           !#=# divertor_kallenbach
           !#=#=# consistency

           ! neomp    | Mean SOL density at OMP calculated by the Kallenbach divertor model [m-3]
           ! nesep    | electron density at separatrix [m-3]
           ! neratio  | ratio of mean SOL density at OMP to separatrix density at OMP

           cc(i) = 1.0D0 - neomp/(nesep*neratio)

           if (present(con)) then
             con(i) = neomp
             err(i) = neomp* cc(i)
             symbol(i) = '='
             units(i) = 'm-3'
           end if

       case (72) ! Central Solenoid Tresca stress limit
           !#=# tfcoil
           !#=#=# foh_stress, alstroh

           ! foh_stress      |  f-value for stress limit
           ! alstroh         |  Maximum permitted stress [MPa]
           ! s_tresca_oh     |  Calculated Tresca stress for Central Solenoid [MPa]
           ! Reverse the sign so it works as an inequality constraint (cc(i) > 0)
           ! This will have no effect if it is used as an equality constraint because it will be squared.
           cc(i) = 1.0d0 - foh_stress * alstroh / s_tresca_oh

           if (present(con)) then
             con(i) = alstroh
             err(i) = alstroh - s_tresca_oh
             symbol(i) = '<'
             units(i) = 'MPa'
           end if

       case (73)  ! ensure separatrix power is greater than the L-H power + auxiliary power
           !#=# physics
           !#=#=# fplhsep, pdivt

           ! fplhsep    | f-value for consistency of two values of separatrix power
           ! plhthresh  | L-H mode power threshold (MW)
           ! pdivt      | power conducted to the divertor region (MW)
           ! pinjmw     | total auxiliary injected power (MW)
           cc(i) = 1.0d0 - fplhsep * pdivt / (plhthresh+pinjmw)

           if (present(con)) then
             con(i) = pdivt
             err(i) = pdivt * cc(i)
             symbol(i) = '>'
             units(i) = 'MW'
           end if

       case (74)  ! ensure TF coil quench temperature < tmax_croco
           ! ONLY used for croco HTS coil
           !#=# physics
           !#=#=# fcqt, tmax_croco

           ! fcqt | f-value for constraint 74: TF coil quench temperature < tmax_croco
           ! croco_quench_temperature | Actual TF coil quench temperature
           ! tmax_croco  | Maximum TF coil quench temperature
           cc(i) = 1.0d0 - fcqt * tmax_croco / croco_quench_temperature

           if (present(con)) then
             con(i) = croco_quench_temperature
             err(i) = croco_quench_temperature * cc(i)
             symbol(i) = '<'
             units(i) = 'K'
          end if

       case (75)  ! ensure that TF coil current / copper area < Maximum value
           ! ONLY used for croco HTS coil
           !#=# physics
           !#=#=# f_copperA_m2, copperA_m2_max

           ! f_copperA_m2 | f-value for constraint 75: TF coil current / copper area < copperA_m2_max
           ! copperA_m2 | TF coil current / copper area
           ! copperA_m2_max  | Maximum TF coil current / copper area
           cc(i) = 1.0d0 - f_copperA_m2 * copperA_m2_max / copperA_m2

           if (present(con)) then
             con(i) = copperA_m2
             err(i) = copperA_m2 * cc(i)
             symbol(i) = '<'
             units(i) = 'A/m2'
           end if

       case (76)  ! Eich critical separatrix density model
          ! Added for issue 558 with ref to http://iopscience.iop.org/article/10.1088/1741-4326/aaa340/pdf
           alpha_crit = (kappa ** 1.2D0) * (1.0D0 + 1.5D0 * triang)
           nesep_crit = 5.9D0 * alpha_crit * (aspect ** (-2.0D0/7.0D0)) * (((1.0D0 + (kappa ** 2.0D0)) / 2.0D0) ** (-6.0D0/7.0D0)) &
                * ((pdivt* 1.0D6) ** (-11.0D0/70.0D0)) * dlimit(7)
           cc(i) = 1.0D0 - fnesep * nesep_crit/nesep
           if (present(con)) then
             con(i) = nesep
             err(i) = nesep * cc(i)
             symbol(i) = '<'
             units(i) = 'm-3'
           end if

       case default

          idiags(1) = icc(i)
          call report_error(13)

       end select

       ! Crude method of catching NaN errors
       !if ((abs(cc(i)) > 9.99D99).or.(cc(i) /= cc(i))) then
       if (variable_error(cc(i))) then

          ! Add debugging lines as appropriate...
          select case (icc(i))

          ! Relationship between beta, temperature (keV) and density
          ! (consistency equation)
          case (1)
             write(*,*) 'betaft = ', betaft
             write(*,*) 'betanb = ', betanb
             write(*,*) 'dene = ', dene
             write(*,*) 'ten = ', ten
             write(*,*) 'dnitot = ', dnitot
             write(*,*) 'tin = ', tin
             write(*,*) 'btot = ',btot
             write(*,*) 'beta = ', beta

          ! Equation for net electric power lower limit
          case (16)
             write(*,*) 'fpnetel = ', fpnetel
             write(*,*) 'pnetelmw = ', pnetelmw
             write(*,*) 'pnetelin = ', pnetelin

          ! Equation for injection power upper limit
          case (30)
             write(*,*) 'fpinj = ', fpinj
             write(*,*) 'pinjalw = ', pinjalw
             write(*,*) 'pinjimw = ', pinjimw
             write(*,*) 'pinjemw = ', pinjemw

          ! Limit on rate of change of energy in poloidal field
          case (66)
             write(*,*) 'fpoloidalpower = ', fpoloidalpower
             write(*,*) 'maxpoloidalpower = ', maxpoloidalpower
             write(*,*) 'peakpoloidalpower = ', peakpoloidalpower


          end select

          idiags(1) = icc(i) ; fdiags(1) = cc(i)
          call report_error(14)

       end if

    end do
    ! Issue 505 Reverse the sign so it works as an inequality constraint (cc(i) > 0)
    ! This will have no effect if it is used as an equality constraint because it will be squared.
    cc = -cc


  end subroutine constraint_eqns

end module constraints
