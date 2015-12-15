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
  !+ad_call  ife_variables
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  pulse_variables
  !+ad_call  report_error
  !+ad_call  rfp_variables
  !+ad_call  stellarator_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_hist  28/07/14 PJK Initial conversion from a subroutine
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use constants
  use constraint_variables
  use current_drive_variables
  use divertor_variables
  use error_handling
  use fwbs_variables
  use heat_transport_variables
  use ife_variables
  use numerics
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use pulse_variables
  use rfp_variables
  use stellarator_variables
  use tfcoil_variables
  use times_variables
  use cost_variables
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
    !+ad_hist  27/02/96 PJK Added rfp equation 47.
    !+ad_hist  07/10/96 PJK Added ICULBL=2 option to constraint no.24
    !+ad_hist  21/03/97 PJK Added IFE equation 50.
    !+ad_hist  01/04/98 PJK Modified equations 2,3,4,7 and 28 to take into
    !+ad_hisc               account IGNITE (ignition switch) setting
    !+ad_hist  25/07/11 PJK Applied Greenwald density limit to line-averaged
    !+ad_hisc               rather than volume-averaged density
    !+ad_hist  20/09/11 PJK Initial F90 version
    !+ad_hist  14/11/11 PJK Changed NaN error check
    !+ad_hist  10/10/12 PJK Modified to use new numerics module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  29/10/12 PJK Added pf_power_variables
    !+ad_hist  30/10/12 PJK Added heat_transport_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  31/10/12 PJK Added constraint_variables
    !+ad_hist  31/10/12 PJK Added stellarator_variables
    !+ad_hist  05/11/12 PJK Added rfp_variables
    !+ad_hist  05/11/12 PJK Added ife_variables
    !+ad_hist  05/11/12 PJK Added pulse_variables
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
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: m
    real(kind(1.0D0)), dimension(m), intent(out) :: cc
    real(kind(1.0D0)), optional, dimension(m), intent(out) :: con
    real(kind(1.0D0)), optional, dimension(m), intent(out) :: err
    character(len=1), optional, dimension(m), intent(out) :: symbol
    character(len=10), optional, dimension(m), intent(out) :: units
    integer, intent(in) :: ieqn

    !  Local variables

    integer :: i,i1,i2
    real(kind(1.0D0)) :: acoil,cratmx,pdenom,pnumerator,pradmaxpv, &
         pscaling,rcw,totmva

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  If ieqn is positive, only evaluate the 'ieqn'th constraint residue,
    !  otherwise evaluate all m constraint residues

    if (ieqn > 0) then
       i1 = ieqn ; i2 = ieqn
    else
       i1 = 1 ; i2 = m
    end if

    !  Consistency (equality) constraints should converge to zero.

    do i = i1,i2

       !  The constraint value in physical units is
       !  a) for consistency equations, the quantity to be equated, or
       !  b) for limit equations, the limiting value.
       !  The symbol is = for a consistency equation, < for an upper limit
       !  or > for a lower limit.

       select case (icc(i))

       case (1)  !  Relationship between beta, temperature (keV) and density
          !  This is a consistency equation

          cc(i) = 1.0D0 - (betaft + betanb + 2.0D3*rmu0*echarge &
               * (dene*ten + dnitot*tin)/btot**2 )/beta
          if (present(con)) then
             con(i) = beta * (1.0D0 - cc(i))
             err(i) = beta * cc(i)
             symbol(i) = '='
             units(i) = ' '
          end if

       case (2)  !  Global plasma power balance equation
          !  This is a consistency equation

          pscaling = ptrepv + ptripv

          if (iradloss == 0) then  !  total power lost is scaling power plus radiation
             pnumerator = pscaling + pradpv
          else if (iradloss == 1) then  !  total power lost is scaling power plus core radiation only
             pnumerator = pscaling + pcoreradpv
          else  !  total power lost is scaling power only, with no additional allowance for radiation
             pnumerator = pscaling
          end if

          if (ignite == 0) then
             pdenom = falpha*palppv + pchargepv + pohmpv + pinjmw/vol
          else
             pdenom = falpha*palppv + pchargepv + pohmpv
          end if

          cc(i) = 1.0D0 - pnumerator / pdenom
          if (present(con)) then
             con(i) = pdenom * (1.0D0 - cc(i))
             err(i) = pdenom * cc(i)
             symbol(i) = '='
             units(i) = 'MW/m3'
          end if

       case (3)  !  Global power balance equation for ions
          !  This is a consistency equation

          !  N.B. This constraint is currently NOT RECOMMENDED for use.

          if (ignite == 0) then
             cc(i) = 1.0D0 - (ptripv + piepv) / &
                  (falpha*palpipv + pinjimw/vol)
             if (present(con)) then
                con(i) = (falpha*palpipv + pinjimw/vol) * (1.0D0 - cc(i))
                err(i) = (falpha*palpipv + pinjimw/vol) * cc(i)
                symbol(i) = '='
                units(i) = 'MW/m3'
             end if
          else
             cc(i) = 1.0D0 - (ptripv+piepv) / (falpha*palpipv)
             if (present(con)) then
                con(i) = (falpha*palpipv) * (1.0D0 - cc(i))
                err(i) = (falpha*palpipv) * cc(i)
                symbol(i) = '='
                units(i) = 'MW/m3'
             end if
          end if

       case (4)  !  Global power balance equation for electrons
          !  This is a consistency equation

          !  N.B. This constraint is currently NOT RECOMMENDED for use.

          pscaling = ptrepv

          if (iradloss == 0) then  !  total power lost is scaling power plus radiation
             pnumerator = pscaling + pradpv
          else if (iradloss == 1) then  !  total power lost is scaling power plus core radiation only
             pnumerator = pscaling + pcoreradpv
          else  !  total power lost is scaling power only, with no additional allowance for radiation
             pnumerator = pscaling
          end if

          if (ignite == 0) then
             pdenom = falpha*palpepv + piepv + pinjemw/vol
          else
             pdenom = falpha*palpepv + piepv
          end if

          cc(i) = 1.0D0 - pnumerator / pdenom
          if (present(con)) then
             con(i) = pdenom * (1.0D0 - cc(i))
             err(i) = pdenom * cc(i)
             symbol(i) = '='
             units(i) = 'MW/m3'
          end if

       case (5)  !  Equation for density upper limit

          if (idensl == 7) then  !  Apply Greenwald limit to line-averaged density
             cc(i) = 1.0D0 - fdene * dnelimt/dnla
             if (present(con)) then
                con(i) = dnelimt * (1.0D0 - cc(i))
                err(i) = dnla * cc(i)
                symbol(i) = '<'
                units(i) = '/m3'
             end if
          else
             cc(i) = 1.0D0 - fdene * dnelimt/dene
             if (present(con)) then
                con(i) = dnelimt * (1.0D0 - cc(i))
                err(i) = dene * cc(i)
                symbol(i) = '<'
                units(i) = '/m3'
             end if
          end if

       case (6)  !  Equation for epsilon beta-poloidal upper limit

          cc(i) = 1.0D0 - fbeta * epbetmax/(eps*betap)
          if (present(con)) then
             con(i) = epbetmax * (1.0D0 - cc(i))
             err(i) = (eps*betap) * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (7)  !  Equation for hot beam ion density
          !  This is a consistency equation (NBI)

          if (ignite == 0) then
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

       case (8)  !  Equation for neutron wall load upper limit

          cc(i) = 1.0D0 - fwalld * walalw/wallmw
          if (present(con)) then
             con(i) = walalw * (1.0D0 - cc(i))
             err(i) = wallmw * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m2'
          end if

       case (9)  !  Equation for fusion power upper limit

          cc(i) = 1.0D0 - ffuspow * powfmax/powfmw
          if (present(con)) then
             con(i) = powfmax * (1.0D0 - cc(i))
             err(i) = powfmw * cc(i)
             symbol(i) = '<'
             units(i) = 'MW'
          end if

       case (10)  !  Equation for field at TF coil
          !  This is a consistency equation
          !  (do not use for stellarators)

          cc(i) = 1.0D0 - (rmajor*bt)/(rbmax*bmaxtf)
          if (present(con)) then
             con(i) = (rbmax*bmaxtf) * (1.0D0 - cc(i))
             err(i) = (rbmax*bmaxtf) * cc(i)
             symbol(i) = '='
             units(i) = 'T.m'
          end if

       case (11)  !  Equation for radial build
          !  This is a consistency equation

          cc(i) = 1.0D0 - rbld/rmajor
          if (present(con)) then
             con(i) = rmajor * (1.0D0 - cc(i))
             err(i) = rmajor * cc(i)
             symbol(i) = '='
             units(i) = 'm'
          end if

       case (12)  !  Equation for volt-second capability lower limit

          !  vsstt (lower limit) is positive; vstot (available) is negative
          cc(i) = 1.0D0 + fvs * vstot/vsstt
          if (present(con)) then
             con(i) = vsstt * (1.0D0 - cc(i))
             err(i) = vsstt * cc(i)
             symbol(i) = '>'
             units(i) = 'V.sec'
          end if

       case (13)  !  Equation for burn time lower limit

          cc(i) = 1.0D0 - ftburn * tburn/tbrnmn
          if (present(con)) then
             con(i) = tbrnmn * (1.0D0 - cc(i))
             err(i) = tbrnmn * cc(i)
             symbol(i) = '>'
             units(i) = 'sec'
          end if

       case (14)  !  Equation to fix number of NBI decay lengths to plasma centre
          !  This is a consistency equation

          cc(i) = 1.0D0 - taubeam/tbeamin
          if (present(con)) then
             con(i) = tbeamin * (1.0D0 - cc(i))
             err(i) = tbeamin * cc(i)
             symbol(i) = '='
             units(i) = ''
          end if

       case (15)  !  Equation for L-H power threshold limit

          cc(i) = 1.0D0 - flhthresh * plhthresh / pdivt
          if (present(con)) then
             con(i) = plhthresh * (1.0D0 - cc(i))
             err(i) = pdivt * cc(i)
             if (flhthresh > 1.0D0) then
                symbol(i) = '>'
             else
                symbol(i) = '<'
             end if
             units(i) = 'MW'
          end if

       case (16)  !  Equation for net electric power lower limit

          cc(i) = 1.0D0 - fpnetel * pnetelmw / pnetelin
          if (present(con)) then
             con(i) = pnetelin * (1.0D0 - cc(i))
             err(i) = pnetelin * cc(i)
             symbol(i) = '>'
             units(i) = 'MW'
          end if

       case (17)  !  Equation for radiation power upper limit
          !  pradmaxpv is the maximum possible power/vol that can be radiated

          !pradmaxpv = pinjmw/vol + palppv + pchargepv + pohmpv
          ! Take account of alpha losses.
          pradmaxpv = pinjmw/vol + palppv*falpha + pchargepv + pohmpv
          cc(i) = 1.0D0 - fradpwr * pradmaxpv / pradpv
          if (present(con)) then
             con(i) = pradmaxpv * (1.0D0 - cc(i))
             err(i) = pradpv * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m3'
          end if

       case (18)  !  Equation for divertor heat load upper limit

          cc(i) = 1.0D0 - fhldiv * hldivlim/hldiv
          if (present(con)) then
             con(i) = hldivlim * (1.0D0 - cc(i))
             err(i) = hldiv * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m2'
          end if

       case (19)  !  Equation for MVA upper limit

          totmva = tfcpmw + tflegmw
          cc(i) = 1.0D0 - fmva * mvalim/totmva
          if (present(con)) then
             con(i) = mvalim * (1.0D0 - cc(i))
             err(i) = totmva * cc(i)
             symbol(i) = '<'
             units(i) = 'MVA'
          end if

       case (20)  !  Equation for neutral beam tangency radius upper limit

          cc(i) = 1.0D0 - fportsz * rtanmax/rtanbeam
          if (present(con)) then
             con(i) = rtanmax * (1.0D0 - cc(i))
             err(i) = rtanbeam * cc(i)
             symbol(i) = '<'
             units(i) = 'm'
          end if

       case (21)  !  Equation for minor radius lower limit

          cc(i) = 1.0D0 - frminor * rminor/aplasmin
          if (present(con)) then
             con(i) = aplasmin * (1.0D0 - cc(i))
             err(i) = aplasmin * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (22)  !  Equation for divertor collision/connection length ratio upper limit

          cc(i) = 1.0D0 - fdivcol * rlenmax / rlclolcn
          if (present(con)) then
             con(i) = rlenmax * (1.0D0 - cc(i))
             err(i) = rlclolcn * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (23)  !  Equation for conducting shell radius / rminor upper limit

          rcw = rminor + scraplo + fwoth + blnkoth
          cc(i) = 1.0D0 - fcwr * cwrmax*rminor / rcw
          if (present(con)) then
             con(i) = cwrmax*rminor * (1.0D0 - cc(i))
             err(i) = rcw * cc(i)
             symbol(i) = '<'
             units(i) = 'm'
          end if

       case (24)  !  Equation for beta upper limit

          if ((iculbl == 0).or.(istell /= 0)) then

             !  Include all beta components
             !  Relevant for both tokamaks and stellarators

             cc(i) = 1.0D0 - fbetatry * betalim/beta
             if (present(con)) then
                con(i) = betalim * (1.0D0 - cc(i))
                err(i) = beta * cc(i)
                symbol(i) = '<'
                units(i) = ''
             end if

          else if (iculbl == 1) then

             !  Here, the beta limit applies to only the thermal
             !  component, not the fast alpha or neutral beam parts

             cc(i) = 1.0D0 - fbetatry * betalim/(beta-betaft-betanb)
             if (present(con)) then
                con(i) = betalim * (1.0D0 - cc(i))
                err(i) = (beta-betaft-betanb) * cc(i)
                symbol(i) = '<'
                units(i) = ''
             end if

          else  !  iculbl == 2

             !  Beta limit applies to thermal + neutral beam
             !  components of the total beta, i.e. excludes alphas

             cc(i) = 1.0D0 - fbetatry * betalim/(beta-betaft)
             if (present(con)) then
                con(i) = betalim * (1.0D0 - cc(i))
                err(i) = (beta-betaft) * cc(i)
                symbol(i) = '<'
                units(i) = ''
             end if

          end if


       case (25)  !  Equation for peak toroidal field upper limit

          cc(i) = 1.0D0 - fpeakb * bmxlim/bmaxtf
          if (present(con)) then
             con(i) = bmxlim * (1.0D0 - cc(i))
             err(i) = bmaxtf * cc(i)
             symbol(i) = '<'
             units(i) = 'T'
          end if

       case (26)  !  Equation for OH coil current density upper limit at EOF

          cc(i) = 1.0D0 - fjohc * rjohc/coheof
          if (present(con)) then
             con(i) = rjohc * (1.0D0 - cc(i))
             err(i) = coheof * cc(i)
             symbol(i) = '<'
             units(i) = 'A/m2'
          end if

       case (27)  !  Equation for OH coil current density upper limit at BOP

          cc(i) = 1.0D0 - fjohc0 * rjohc0/cohbop
          if (present(con)) then
             con(i) = rjohc0 * (1.0D0 - cc(i))
             err(i) = cohbop * cc(i)
             symbol(i) = '<'
             units(i) = 'A/m2'
          end if

       case (28)  !  Equation for fusion gain (big Q) lower limit

          !  Q = fusion power / (injected+ohmic) power, >= bigqmin

          if (ignite == 0) then
             cc(i) = 1.0D0 - fqval * bigq/bigqmin
             if (present(con)) then
                con(i) = bigqmin * (1.0D0 - cc(i))
                err(i) = bigqmin * cc(i)
                symbol(i) = '>'
                units(i) = ''
             end if
          else
             call report_error(4)
          end if

       case (29)  !  Equation for inboard major radius
          !  This is a consistency equation

          cc(i) = 1.0D0 - (rmajor - rminor) / rinboard
          if (present(con)) then
             con(i) = rinboard * (1.0D0 - cc(i))
             err(i) = rinboard * cc(i)
             symbol(i) = '='
             units(i) = 'm'
          end if

       case (30)  !  Equation for injection power upper limit

          !  Usual form is inverted to prevent problems when injection power is zero

          cc(i) = 1.0D0 - pinjmw / (fpinj * pinjalw)
          if (present(con)) then
             con(i) = pinjalw * (1.0D0 - cc(i))
             err(i) = pinjmw * cc(i)
             symbol(i) = '<'
             units(i) = 'MW'
          end if

       case (31)  !  Equation for TF coil case stress upper limit (SCTF)

          cc(i) = 1.0D0 - fstrcase * alstrtf/strtf2
          if (present(con)) then
             con(i) = alstrtf * (1.0D0 - cc(i))
             err(i) = strtf2 * cc(i)
             symbol(i) = '<'
             units(i) = 'Pa'
          end if

       case (32)  !  Equation for TF coil conduit stress upper limit (SCTF)

          cc(i) = 1.0D0 - fstrcond * alstrtf/strtf1
          if (present(con)) then
             con(i) = alstrtf * (1.0D0 - cc(i))
             err(i) = strtf1 * cc(i)
             symbol(i) = '<'
             units(i) = 'Pa'
          end if

       case (33)  !  Equation for TF coil operating/critical J upper limit (SCTF)

          cc(i) = 1.0D0 - fiooic * jwdgcrt/jwptf
          if (present(con)) then
             con(i) = jwdgcrt * (1.0D0 - cc(i))
             err(i) = jwptf * cc(i)
             symbol(i) = '<'
             units(i) = 'A/m2'
          end if

       case (34)  !  Equation for TF coil dump voltage upper limit (SCTF)

          cc(i) = 1.0D0 - fvdump * vdalw/vtfskv
          if (present(con)) then
             con(i) = vdalw * (1.0D0 - cc(i))
             err(i) = vtfskv * cc(i)
             symbol(i) = '<'
             units(i) = 'V'
          end if

       case (35)  !  Equation for TF coil J_wp/J_prot upper limit (SCTF)

          cc(i) = 1.0D0 - fjprot * jwdgpro/jwptf
          if (present(con)) then
             con(i) = jwdgpro * (1.0D0 - cc(i))
             err(i) = jwptf * cc(i)
             symbol(i) = '<'
             units(i) = 'A/m2'
          end if

       case (36)  !  Equation for TF coil s/c temperature margin lower limit (SCTF)

          cc(i) = 1.0D0 - ftmargtf * tmargtf/tmargmin
          if (present(con)) then
             con(i) = tmargmin * (1.0D0 - cc(i))
             err(i) = tmargmin * cc(i)
             symbol(i) = '>'
             units(i) = 'K'
          end if

       case (37)  !  Equation for current drive gamma upper limit

          cc(i) = 1.0D0 - fgamcd * gammax/gamcd
          if (present(con)) then
             con(i) = gammax * (1.0D0 - cc(i))
             err(i) = gamcd * cc(i)
             symbol(i) = '<'
             units(i) = '1E20 A/Wm2'
          end if

       case (38)  !  Equation for first wall coolant temperature rise upper limit

          cc(i) = 1.0D0 - fdtmp * dtmpmx/tmprse
          if (present(con)) then
             con(i) = dtmpmx * (1.0D0 - cc(i))
             err(i) = tmprse * cc(i)
             symbol(i) = '<'
             units(i) = 'K'
          end if

       case (39)  !  Equation for first wall temperature upper limit
          ! Issue #348 (15/12/02) 
          if (tpeak == 0.0D0) call report_error(5)

          cc(i) = 1.0D0 - ftpeak * tfwmatmax/tpeak
          if (present(con)) then
             con(i) = tfwmatmax * (1.0D0 - cc(i))
             err(i) = tpeak * cc(i)
             symbol(i) = '<'
             units(i) = 'K'
          end if

       case (40)  !  Equation for auxiliary power lower limit

          cc(i) = 1.0D0 - fauxmn * pinjmw/auxmin
          if (present(con)) then
             con(i) = auxmin * (1.0D0 - cc(i))
             err(i) = auxmin * cc(i)
             symbol(i) = '>'
             units(i) = 'MW'
          end if

       case (41)  !  Equation for plasma current ramp-up time lower limit

          cc(i) = 1.0D0 - ftohs * tohs/tohsmn
          if (present(con)) then
             con(i) = tohsmn * (1.0D0 - cc(i))
             err(i) = tohsmn * cc(i)
             symbol(i) = '>'
             units(i) = 'sec'
          end if

       case (42)  !  Equation for cycle time lower limit

          if (tcycmn == 0.0D0) call report_error(6)

          cc(i) = 1.0D0 - ftcycl * tcycle/tcycmn
          if (present(con)) then
             con(i) = tcycmn * (1.0D0 - cc(i))
             err(i) = tcycmn * cc(i)
             symbol(i) = '>'
             units(i) = 'sec'
          end if

       case (43)  !  Equation for average centrepost temperature
          !  This is a consistency equation (TART)

          if (itart == 0) call report_error(7)

          cc(i) = 1.0D0 - tcpav/tcpav2
          if (present(con)) then
             con(i) = tcpav2 * (1.0D0 - cc(i))
             err(i) = tcpav2 * cc(i)
             symbol(i) = '='
             units(i) = 'deg C'
          end if

       case (44)  !  Equation for centrepost temperature upper limit (TART)

          if (itart == 0) call report_error(8)

          cc(i) = 1.0D0 - fptemp * ptempalw / tcpmax
          if (present(con)) then
             con(i) = ptempalw * (1.0D0 - cc(i))
             err(i) = tcpmax * cc(i)
             symbol(i) = '<'
             units(i) = 'deg C'
          end if

       case (45)  !  Equation for edge safety factor lower limit (TART)

          if (itart == 0) call report_error(9)

          cc(i) = 1.0D0 - fq * q/qlim
          if (present(con)) then
             con(i) = qlim * (1.0D0 - cc(i))
             err(i) = qlim * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (46)  !  Equation for Ip/Irod upper limit (TART)
          !  This is a q-edge type limit for certain aspect ratios

          if (itart == 0) call report_error(10)

          !  Maximum ratio of plasma current to centrepost current
          cratmx = 1.0D0 + 4.91D0*(eps-0.62D0)
          cc(i) = 1.0D0 - fipir * cratmx * ritfc/plascur
          if (present(con)) then
             con(i) = cratmx * (1.0D0 - cc(i))
             err(i) = plascur/ritfc * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (47)  !  Equation for TF coil toroidal thickness upper limit
          !  Relevant only to reversed field pinch devices

          if (irfp == 0) call report_error(11)

          cc(i) = 1.0D0 - frfptf * (2.0D0*(rbmax-tfcth)*tan(pi/tfno))/tftort
          if (present(con)) then
             con(i) = (2.0D0*(rbmax-tfcth)*tan(pi/tfno)) * (1.0D0 - cc(i))
             err(i) = tftort * cc(i)
             symbol(i) = '<'
             units(i) = 'm'
          end if

       case (48)  !  Equation for poloidal beta upper limit

          cc(i) = 1.0D0 - fbetap * betpmx/betap
          if (present(con)) then
             con(i) = betpmx * (1.0D0 - cc(i))
             err(i) = betap * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (49)  !  Equation to ensure RFP reversal parameter F is negative

          cc(i) = 1.0D0 + frfpf * rfpf/0.001D0
          if (present(con)) then
             con(i) = 0.001D0 * (1.0D0 - cc(i))
             err(i) = 0.001D0 * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (50)  !  Equation for IFE repetition rate upper limit
          !  Relevant only to inertial fusion energy devices

          if (ife == 0) call report_error(12)

          cc(i) = 1.0D0 - frrmax * rrmax/reprat
          if (present(con)) then
             con(i) = rrmax * (1.0D0 - cc(i))
             err(i) = reprat * cc(i)
             symbol(i) = '<'
             units(i) = '/sec'
          end if

       case (51)  !  Equation to enforce startup flux = available startup flux
          !  This is a consistency equation

          cc(i) = 1.0D0 - (vsres+vsind) / vssu
          if (present(con)) then
             con(i) = vssu * (1.0D0 - cc(i))
             err(i) = vssu * cc(i)
             symbol(i) = '='
             units(i) = 'V.s'
          end if

       case (52)  !  Equation for tritium breeding ratio lower limit

          cc(i) = 1.0D0 - ftbr * tbr/tbrmin
          if (present(con)) then
             con(i) = tbrmin * (1.0D0 - cc(i))
             err(i) = tbrmin * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (53)  !  Equation for fast neutron fluence on TF coil upper limit

          cc(i) = 1.0D0 - fflutf * nflutfmax/nflutf
          if (present(con)) then
             con(i) = nflutfmax * (1.0D0 - cc(i))
             err(i) = nflutf * cc(i)
             symbol(i) = '<'
             units(i) = 'neutron/m2'
          end if

       case (54)  !  Equation for peak TF coil nuclear heating upper limit

          cc(i) = 1.0D0 - fptfnuc * ptfnucmax/ptfnucpm3
          if (present(con)) then
             con(i) = ptfnucmax * (1.0D0 - cc(i))
             err(i) = ptfnucpm3 * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m3'
          end if

       case (55)  !  Equation for helium concentration in vacuum vessel upper limit

          cc(i) = 1.0D0 - fvvhe * vvhealw/vvhemax
          if (present(con)) then
             con(i) = vvhealw * (1.0D0 - cc(i))
             err(i) = vvhemax * cc(i)
             symbol(i) = '<'
             units(i) = 'appm'
          end if

       case (56)  !  Equation for power through separatrix / major radius upper limit

          cc(i) = 1.0D0 - fpsepr * pseprmax / (pdivt/rmajor)
          if (present(con)) then
             con(i) = pseprmax * (1.0D0 - cc(i))
             err(i) = (pdivt/rmajor) * cc(i)
             symbol(i) = '<'
             units(i) = 'MW/m'
          end if

       case (57)  !  Equation for TF coil outer leg toroidal thickness lower limit

          cc(i) = 1.0D0 - ftftort * tftort/(wwp1 + 2.0D0*tinstf + 2.0D0*casths)
          if (present(con)) then
             con(i) = (wwp1 + 2.0D0*tinstf + 2.0D0*casths) * (1.0D0 - cc(i))
             err(i) = (wwp1 + 2.0D0*tinstf + 2.0D0*casths) * cc(i)
             symbol(i) = '>'
             units(i) = 'm'
          end if

       case (58)  !  Equation for TF coil outer leg radial thickness lower limit

          cc(i) = 1.0D0 - ftfthko * tfthko/(thkwp + 2.0D0*tinstf)
          if (present(con)) then
             con(i) = (thkwp + 2.0D0*tinstf) * (1.0D0 - cc(i))
             err(i) = (thkwp + 2.0D0*tinstf) * cc(i)
             symbol(i) = '>'
             units(i) = 'm'
          end if

       case (59)  !  Equation for neutral beam shine-through fraction upper limit

          cc(i) = 1.0D0 - fnbshinef * nbshinefmax / nbshinef
          if (present(con)) then
             con(i) = nbshinefmax * (1.0D0 - cc(i))
             err(i) = nbshinef * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if

       case (60)  !  Equation for OH coil s/c temperature margin lower limit (SCTF)

          cc(i) = 1.0D0 - ftmargoh * tmargoh/tmargmin
          if (present(con)) then
             con(i) = tmargmin * (1.0D0 - cc(i))
             err(i) = tmargmin * cc(i)
             symbol(i) = '>'
             units(i) = 'K'
          end if

       case (61)  !  Equation for availability limit

          cc(i) = 1.0D0 - favail * cfactr / avail_min
          if (present(con)) then
             con(i) = avail_min * (1.0D0 - cc(i))
             err(i) = cfactr * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (62)  !  Lower limit on taup/taueff the ratio of alpha particle to energy confinement times
          cc(i) = 1.0D0 - ftaulimit * (taup / taueff) / taulimit
          if (present(con)) then
             con(i) = taulimit
             err(i) = (taup / taueff) * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

       case (63)  !  Upper limit on niterpump (vacuum_model = simple)
          cc(i) = 1.0D0 - fniterpump * tfno / niterpump
          if (present(con)) then
             con(i) = tfno
             err(i) = tfno * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if
	  
       case (64)  !  Upper limit on Zeff
          cc(i) = 1.0D0 - fzeffmax * (zeffmax/zeff)
	  if (present(con)) then
             con(i) = zeffmax
             err(i) = zeffmax * cc(i)
             symbol(i) = '<'
             units(i) = ''
          end if
	  
       case (65)  !  Limit TF dump time to calculated quench time (IDM: 2MBSE3)
          cc(i) = 1.0d0 - ftaucq * tdmptf / taucq
	  if (present(con)) then
             con(i) = taucq
             err(i) = taucq * cc(i)
             symbol(i) = '>'
             units(i) = ''
          end if

      case default

          idiags(1) = icc(i)
          call report_error(13)

       end select

       !  Crude method of catching NaN errors

       if ((abs(cc(i)) > 9.99D99).or.(cc(i) /= cc(i))) then

          !  Add debugging lines as appropriate...

          select case (icc(i))

          case (1)
             write(*,*) 'betaft = ', betaft
             write(*,*) 'betanb = ', betanb
             write(*,*) 'dene = ', dene
             write(*,*) 'ten = ', ten
             write(*,*) 'dnitot = ', dnitot
             write(*,*) 'tin = ', tin
             write(*,*) 'btot = ',btot
             write(*,*) 'beta = ', beta

          case (16)
             write(*,*) 'fpnetel = ', fpnetel
             write(*,*) 'pnetelmw = ', pnetelmw
             write(*,*) 'pnetelin = ', pnetelin

          case (30)
             write(*,*) 'fpinj = ', fpinj
             write(*,*) 'pinjalw = ', pinjalw
             write(*,*) 'pinjimw = ', pinjimw
             write(*,*) 'pinjemw = ', pinjemw

          end select

          idiags(1) = icc(i) ; fdiags(1) = cc(i)
          call report_error(14)

       end if

    end do

  end subroutine constraint_eqns

end module constraints
