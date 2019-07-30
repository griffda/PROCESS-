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

  use error_handling, only: report_error, idiags, fdiags
  
  implicit none

  public :: constraint_eqns

  type constraint_args_type
    real(kind(1.0D0)) :: cc
    real(kind(1.0D0)) :: con
    real(kind(1.0D0)) :: err
    character(len=1)  :: symbol
    character(len=10) :: units
  end type

contains

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
     !+ad_hist  25/04/19 SK  Added new constraint equation (81) ensuring ne(0) > ne(ped)
     !+ad_stat  Okay
     !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
     !
     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
     use numerics, only: icc
     use maths_library, only: variable_error

     implicit none

     ! Arguments !
     !!!!!!!!!!!!!

     integer, intent(in) :: m, ieqn
     real(kind(1.0D0)),           dimension(m), intent(out) :: cc
     real(kind(1.0D0)), optional, dimension(m), intent(out) :: con
     real(kind(1.0D0)), optional, dimension(m), intent(out) :: err
     character(len=1),  optional, dimension(m), intent(out) :: symbol
     character(len=10), optional, dimension(m), intent(out) :: units
	
     ! Local variables !
     !!!!!!!!!!!!!!!!!!!
	
     integer :: i,i1,i2
     real(kind(1.0D0)) :: cratmx,pdenom,pnumerator,pradmaxpv
     real(kind(1.0D0)) :: pscaling,rcw,totmva

     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     type (constraint_args_type) args
	
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

	      ! Relationship between beta, temperature (keV) and density
         case (1); call constraint_eqn_001(args)
	      ! Global plasma power balance equation
         case (2); call constraint_eqn_002(args)
	      ! Global power balance equation for ions
         case (3); call constraint_eqn_003(args)
	      ! Global power balance equation for electrons
         case (4); call constraint_eqn_004(args)
	      ! Equation for density upper limit
         case (5); call constraint_eqn_005(args)
	      ! Equation for epsilon beta-poloidal upper limit
         case (6); call constraint_eqn_006(args)
	      ! Equation for hot beam ion density
         case (7); call constraint_eqn_007(args)
	      ! Equation for neutron wall load upper limit
         case (8); call constraint_eqn_008(args)
	      ! Equation for fusion power upper limit
         case (9); call constraint_eqn_009(args)
	      ! Equation for field at TF coil
         case (10); call constraint_eqn_010(args)
	      ! Equation for radial build
         case (11); call constraint_eqn_011(args)
	      ! Equation for volt-second capability lower limit
         case (12); call constraint_eqn_012(args)
	      ! Equation for burn time lower limit
         case (13); call constraint_eqn_013(args)
	      ! Equation to fix number of NBI decay lengths to plasma centre
         case (14); call constraint_eqn_014(args)
	      ! Equation for L-H power threshold limit
         case (15); call constraint_eqn_015(args)
	      ! Equation for net electric power lower limit
         case (16); call constraint_eqn_016(args)
	      ! Equation for radiation power upper limit
         case (17); call constraint_eqn_017(args)
	      ! Equation for divertor heat load upper limit
         case (18); call constraint_eqn_018(args)
	      ! Equation for MVA upper limit
         case (19); call constraint_eqn_019(args)
	      ! Equation for neutral beam tangency radius upper limit
         case (20); call constraint_eqn_020(args)
	      ! Equation for minor radius lower limit
         case (21); call constraint_eqn_021(args)
	      ! Equation for divertor collision/connection length ratio upper limit
         case (22); call constraint_eqn_022(args)
	      ! Equation for conducting shell radius / rminor upper limit
         case (23); call constraint_eqn_023(args)
	      ! Equation for beta upper limit
         case (24); call constraint_eqn_024(args)
	      ! Equation for peak toroidal field upper limit
         case (25); call constraint_eqn_025(args)
	      ! Equation for Central Solenoid current density upper limit at EOF
         case (26); call constraint_eqn_026(args)
	      ! Equation for Central Solenoid current density upper limit at BOP
         case (27); call constraint_eqn_027(args)
	      ! Equation for fusion gain (big Q) lower limit
         case (28); call constraint_eqn_028(args)
	      ! Equation for inboard major radius
         case (29); call constraint_eqn_029(args)
	      ! Equation for injection power upper limit
         case (30); call constraint_eqn_030(args)
	      ! Equation for TF coil case stress upper limit (SCTF)
         case (31); call constraint_eqn_031(args)
	      ! Equation for TF coil conduit stress upper limit (SCTF)
         case (32); call constraint_eqn_032(args)
	      ! Equation for TF coil operating/critical J upper limit (SCTF)
         case (33); call constraint_eqn_033(args)
	      ! Equation for TF coil dump voltage upper limit (SCTF)
         case (34); call constraint_eqn_034(args)
	      ! Equation for TF coil J_wp/J_prot upper limit (SCTF)
         case (35); call constraint_eqn_035(args)
	      ! Equation for TF coil s/c temperature margin lower limit (SCTF)
         case (36); call constraint_eqn_036(args)
	      ! Equation for current drive gamma upper limit
         case (37); call constraint_eqn_037(args)
	      ! Equation for first wall temperature upper limit
         case (39); call constraint_eqn_039(args)
	      ! Equation for auxiliary power lower limit
         case (40); call constraint_eqn_040(args)
	      ! Equation for plasma current ramp-up time lower limit
         case (41); call constraint_eqn_041(args)
	      ! Equation for cycle time lower limit
         case (42); call constraint_eqn_042(args)
	      ! Equation for average centrepost temperature
         case (43); call constraint_eqn_043(args)
	      ! Equation for centrepost temperature upper limit (TART)
         case (44); call constraint_eqn_044(args)
	      ! Equation for edge safety factor lower limit (TART)
         case (45); call constraint_eqn_045(args)
	      ! Equation for Ip/Irod upper limit (TART)
         case (46); call constraint_eqn_046(args)  
	      ! Equation for TF coil toroidal thickness upper limit
         case (47); call constraint_eqn_047(args)  
	      ! Equation for poloidal beta upper limit
         case (48); call constraint_eqn_048(args)  
	      ! Issue #508 Remove RFP option Equation to ensure reversal parameter F is negative
         case (49); call constraint_eqn_049(args)
         ! Issue #508 Remove IFE option: Equation for repetition rate upper limit
         case (50); call constraint_eqn_050(args)
	      ! Equation to enforce startup flux = available startup flux
         case (51); call constraint_eqn_051(args)  
	      ! Equation for tritium breeding ratio lower limit
         case (52); call constraint_eqn_052(args)  
	      ! Equation for fast neutron fluence on TF coil upper limit
         case (53); call constraint_eqn_053(args)  
	      ! Equation for peak TF coil nuclear heating upper limit
         case (54); call constraint_eqn_054(args)  
	      ! Equation for helium concentration in vacuum vessel upper limit
         case (55); call constraint_eqn_055(args)  
	      ! Equation for power through separatrix / major radius upper limit
         case (56); call constraint_eqn_056(args)  
         ! Obsolete
         case (57); call constraint_eqn_057(args)
         ! Obsolete
         case (58); call constraint_eqn_058(args)
	      ! Equation for neutral beam shine-through fraction upper limit
         case (59); call constraint_eqn_059(args)  
	      ! Equation for Central Solenoid s/c temperature margin lower limit
         case (60); call constraint_eqn_060(args)  
	      ! Equation for availability limit
         case (61); call constraint_eqn_061(args)  
	      ! Lower limit on taup/taueff the ratio of alpha particle to energy confinement times
         case (62); call constraint_eqn_062(args)  
	      ! Upper limit on niterpump (vacuum_model = simple)
         case (63); call constraint_eqn_063(args)  
         ! Upper limit on Zeff
         case (64); call constraint_eqn_064(args)  
         ! Limit TF dump time to calculated quench time
         case (65); call constraint_eqn_065(args)
         ! Limit on rate of change of energy in poloidal field
         case (66); call constraint_eqn_066(args)
         ! Simple upper limit on radiation wall load
         case (67); call constraint_eqn_067(args)
         ! New Psep scaling (PsepB/qAR)
         case (68); call constraint_eqn_068(args)
         ! Ensure separatrix power is less than value from Kallenbach divertor 
         case (69); call constraint_eqn_069(args)
         ! Separatrix temperature consistency
         case (70); call constraint_eqn_070(args)
         ! Separatrix density consistency
         case (71); call constraint_eqn_071(args)
	      ! Central Solenoid Tresca stress limit
         case (72); call constraint_eqn_072(args)
         ! ensure separatrix power is greater than the L-H power + auxiliary power
         case (73); call constraint_eqn_073(args)
         ! ensure TF coil quench temperature < tmax_croco   
         case (74); call constraint_eqn_074(args)
	      ! ensure that TF coil current / copper area < Maximum value ONLY used for croco HTS coil
         case (75); call constraint_eqn_075(args)
         ! Eich critical separatrix density model
         case (76); call constraint_eqn_076(args)
         ! Equation for maximum TF current per turn upper limit
         case (77); call constraint_eqn_077(args)
	  	   ! Equation for Reinke criterion, divertor impurity fraction lower limit
         case (78); call constraint_eqn_078(args)
         ! Equation for maximum CS field
         case (79); call constraint_eqn_079(args)
         ! Lower limit pdivt
         case (80); call constraint_eqn_080(args)
         ! Constraint equation making sure that ne(0) > ne(ped)
         case (81); call constraint_eqn_081(args)
         
       case default

         idiags(1) = icc(i)
         call report_error(13)
         args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')	  
      
       end select

       cc(i) = args%cc
       if (present(con)) then
         con(i) = args%con
         if (present(err))    err(i)    = args%err
         if (present(symbol)) symbol(i) = args%symbol
         if (present(units))  units(i)  = args%units
       end if

       ! Crude method of catching NaN errors
       !if ((abs(cc(i)) > 9.99D99).or.(cc(i) /= cc(i))) then
       if (variable_error(cc(i))) then

         ! Add debugging lines as appropriate...
         select case (icc(i))

            ! Relationship between beta, temperature (keV) and density (consistency equation)
            case (1); call constraint_err_001()
            ! Equation for net electric power lower limit
            case (16); call constraint_err_016()
            ! Equation for injection power upper limit
            case (30); call constraint_err_030()
            ! Limit on rate of change of energy in poloidal field
            case (66); call constraint_err_066()

         end select

         idiags(1) = icc(i) ; fdiags(1) = cc(i)
         call report_error(14)

       end if

     end do
     ! Issue 505 Reverse the sign so it works as an inequality constraint (cc(i) > 0)
     ! This will have no effect if it is used as an equality constraint because it will be squared.
     cc = -cc

   end subroutine constraint_eqns

   !--- Error-handling routines
  
   subroutine constraint_err_001()
     !+ad_name  constraint_err_001
     !+ad_summ  Error in: Relationship between beta, temperature (keV) and density (consistency equation)
     !+ad_type  Subroutine
     !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
     use physics_variables, only: betaft, betanb, dene, ten, dnitot, tin, btot, beta
     write(*,*) 'betaft = ', betaft
     write(*,*) 'betanb = ', betanb
     write(*,*) 'dene = ', dene
     write(*,*) 'ten = ', ten
     write(*,*) 'dnitot = ', dnitot
     write(*,*) 'tin = ', tin
     write(*,*) 'btot = ',btot
     write(*,*) 'beta = ', beta
   end subroutine
   
   subroutine constraint_err_016()
     !+ad_name  constraint_err_016
     !+ad_summ  Error in: Equation for net electric power lower limit
     !+ad_type  Subroutine
     !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
     use constraint_variables, only: fpnetel, pnetelin
     use heat_transport_variables, only: pnetelmw
     implicit none
     write(*,*) 'fpnetel = ', fpnetel
     write(*,*) 'pnetelmw = ', pnetelmw
     write(*,*) 'pnetelin = ', pnetelin
   end subroutine
 
   subroutine constraint_err_030()
     !+ad_name  constraint_err_030
     !+ad_summ  Error in: Equation for injection power upper limit
     !+ad_type  Subroutine
     !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
     use current_drive_variables, only: pinjmw, pinjalw
     use constraint_variables, only: fpinj
     implicit none
     write(*,*) 'fpinj = ', fpinj
     write(*,*) 'pinjalw = ', pinjalw
     write(*,*) 'pinjmw = ', pinjmw
   end subroutine
    
   subroutine constraint_err_066()
     !+ad_name  constraint_err_066
     !+ad_summ  Error in: Limit on rate of change of energy in poloidal field
     !+ad_type  Subroutine
     !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
     use constraint_variables, only: fpoloidalpower 
     use pf_power_variables, only: maxpoloidalpower, peakpoloidalpower
     implicit none
     write(*,*) 'fpoloidalpower = ', fpoloidalpower
     write(*,*) 'maxpoloidalpower = ', maxpoloidalpower
     write(*,*) 'peakpoloidalpower = ', peakpoloidalpower
   end subroutine constraint_err_066

   !---

   subroutine constraint_eqn_001(args)
      !+ad_name  constraint_eqn_001
      !+ad_summ  Relationship between beta, temperature (keV) and density
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Relationship between beta, temperature (keV) and density
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# consistency
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  betaft : input real : fast alpha beta component
      !+ad_glos  betanb : input real : neutral beam beta component
      !+ad_glos  dene : input real : electron density (/m3) (iteration variable 6)
      !+ad_glos  ten : input real : density weighted average electron temperature (keV)
      !+ad_glos  dnitot : input real : total ion density (/m3)
      !+ad_glos  tin : input real : density weighted average ion temperature (keV)
      !+ad_glos  btot : input real : total toroidal + poloidal field (T)
      !+ad_glos  beta : input real : total plasma beta (iteration variable 5) (calculated if ipedestal =3)
      !+ad_glos  echarge : input real : FIX : electron charge (C)
      !+ad_glos  rmu0 : input real : FIX : permeability of free space, 4.pi x 10^(-7) H/m
      use physics_variables, only: betaft, betanb, dene, ten, dnitot, tin, btot, beta
      use constants, only: echarge,rmu0
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - (betaft + betanb + 2.0D3*rmu0*echarge * (dene*ten + dnitot*tin)/btot**2 )/beta
      args%con    = beta * (1.0D0 - args%cc)
      args%err    = beta * args%cc
      args%symbol = '='
      args%units  = ''

   end subroutine constraint_eqn_001

   subroutine constraint_eqn_002(args)
      !+ad_name  constraint_eqn_002
      !+ad_summ  Global plasma power balance equation
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Global plasma power balance equation
      !+ad_desc  This is a consistency equation
      !+ad_desc  N.B. This constraint is currently NOT RECOMMENDED for use.
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  iradloss : input integer : switch for radiation loss term usage in power balance (see User Guide):<UL>
      !+ad_gloc             <LI> = 0 total power lost is scaling power plus radiation (needed for ipedestal=2,3)
      !+ad_gloc             <LI> = 1 total power lost is scaling power plus core radiation only
      !+ad_gloc             <LI> = 2 total power lost is scaling power only, with no additional
      !+ad_gloc                      allowance for radiation. This is not recommended for power plant models.</UL>
      !+ad_glos  ignite : input integer : switch for ignition assumption:<UL>
      !+ad_gloc          <LI> = 0 do not assume plasma ignition;
      !+ad_gloc          <LI> = 1 assume ignited (but include auxiliary power in costs)</UL>
      !+ad_glos  ptrepv : input real : electron transport power per volume (MW/m3)
      !+ad_glos  ptripv : input real :  ion transport power per volume (MW/m3)
      !+ad_glos  pradpv : input real : total radiation power per volume (MW/m3)
      !+ad_glos  pcoreradpv : input real : total core radiation power per volume (MW/m3)
      !+ad_glos  falpha : input real : fraction of alpha power deposited in plasma
      !+ad_glos  palppv : input real : alpha power per volume (MW/m3)
      !+ad_glos  pchargepv : input real : non-alpha charged particle fusion power per volume (MW/m3)
      !+ad_glos  pohmpv : input real : ohmic heating power per volume (MW/m3)
      !+ad_glos  pinjmw : input real : total auxiliary injected power (MW)
      !+ad_glos  vol : input real : plasma volume (m3)
      use physics_variables, only: iradloss, ignite, ptrepv, ptripv, pradpv, & 
                                   pcoreradpv, falpha, palppv, pchargepv, &
                                   pohmpv, vol
      use current_drive_variables, only: pinjmw
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! pscaling : Local real : total transport power per volume (MW/m3)
      real(kind(1.0D0)) :: pscaling
      real(kind(1.0D0)) :: pnumerator, pdenom
      pscaling = ptrepv + ptripv
	   ! Total power lost is scaling power plus radiation:
      if (iradloss == 0) then
         pnumerator = pscaling + pradpv
      else if (iradloss == 1) then 		 
         pnumerator = pscaling + pcoreradpv
      else
         pnumerator = pscaling	  
      end if

      ! if plasma not ignited include injected power
      if (ignite == 0) then
         pdenom = falpha*palppv + pchargepv + pohmpv + pinjmw/vol
      else
      ! if plasma ignited
         pdenom = falpha*palppv + pchargepv + pohmpv
      end if

      args%cc = 1.0D0 - pnumerator / pdenom
      args%con = pdenom * (1.0D0 - args%cc)
      args%err = pdenom * args%cc
      args%symbol = '='
      args%units = 'MW/m3'

   end subroutine constraint_eqn_002

   subroutine constraint_eqn_003(args)
      !+ad_name  constraint_eqn_003
      !+ad_summ  Global power balance equation for ions
      !+ad_type  Subroutine
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Global power balance equation for ions
      !+ad_desc  This is a consistency equation (NBI)
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ignite : input integer : switch for ignition assumption:<UL>
      !+ad_gloc          <LI> = 0 do not assume plasma ignition;
      !+ad_gloc          <LI> = 1 assume ignited (but include auxiliary power in costs)</UL>
      !+ad_glos  ptripv : input real :  ion transport power per volume (MW/m3)
      !+ad_glos  piepv : input real : ion/electron equilibration power per volume (MW/m3)
      !+ad_glos  falpha : input real : fraction of alpha power deposited in plasma
      !+ad_glos  palpipv : input real : alpha power per volume to ions (MW/m3)
      !+ad_glos  pinjimw : input real : auxiliary injected power to ions (MW)
      !+ad_glos  vol : input real : plasma volume (m3)
      use physics_variables, only: ignite, ptripv, piepv, falpha, palpipv, vol
      use current_drive_variables, only: pinjimw
      implicit none
      type (constraint_args_type), intent(out) :: args

	   ! No assume plasma ignition:
      if (ignite == 0) then
         args%cc     = 1.0D0 - (ptripv + piepv) / (falpha*palpipv + pinjimw/vol)
         args%con    = (falpha*palpipv + pinjimw/vol) * (1.0D0 - args%cc)
         args%err    = (falpha*palpipv + pinjimw/vol) * args%cc
         args%symbol = '='
         args%units  = 'MW/m3'
	   ! Plasma ignited:
      else
         args%cc     = 1.0D0 - (ptripv+piepv) / (falpha*palpipv)
         args%con    = (falpha*palpipv) * (1.0D0 - args%cc)
         args%err    = (falpha*palpipv) * args%cc
         args%symbol = '='
         args%units  = 'MW/m3'
      end if

   end subroutine constraint_eqn_003

   subroutine constraint_eqn_004(args)
      !+ad_name  constraint_eqn_004
      !+ad_summ  Global power balance equation for electrons
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Global power balance equation for electrons
      !+ad_desc  This is a consistency equation
      !+ad_desc  N.B. This constraint is currently NOT RECOMMENDED for use.
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  iradloss : input integer : switch for radiation loss term usage in power balance (see User Guide):<UL>
      !+ad_gloc             <LI> = 0 total power lost is scaling power plus radiation (needed for ipedestal=2,3)
      !+ad_gloc             <LI> = 1 total power lost is scaling power plus core radiation only
      !+ad_gloc             <LI> = 2 total power lost is scaling power only, with no additional
      !+ad_gloc                      allowance for radiation. This is not recommended for power plant models.</UL>
      !+ad_glos  ignite : input integer : switch for ignition assumption:<UL>
      !+ad_gloc          <LI> = 0 do not assume plasma ignition;
      !+ad_gloc          <LI> = 1 assume ignited (but include auxiliary power in costs)</UL>
      !+ad_glos  ptrepv : input real : electron transport power per volume (MW/m3)
      !+ad_glos  pradpv : input real : total radiation power per volume (MW/m3)
      !+ad_glos  pcoreradpv : input real : total core radiation power per volume (MW/m3)
      !+ad_glos  falpha : input real : fraction of alpha power deposited in plasma
      !+ad_glos  palpepv : input real : alpha power per volume to electrons (MW/m3)
      !+ad_glos  piepv : input real : ion/electron equilibration power per volume (MW/m3)
      !+ad_glos  pinjemw : input real : auxiliary injected power to electrons (MW)
      !+ad_glos  vol : input real : plasma volume (m3)
      use physics_variables, only: iradloss, ignite, ptrepv, pcoreradpv, falpha, & 
                                 palpepv, piepv, vol, pradpv
      use current_drive_variables, only: pinjemw
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! pscaling : Local real : total transport power per volume (MW/m3)
      real(kind(1.0D0)) :: pscaling
      real(kind(1.0D0)) :: pnumerator, pdenom
      pscaling = ptrepv
	   ! Total power lost is scaling power plus radiation:
      if (iradloss == 0) then
         pnumerator = pscaling + pradpv
      else if (iradloss == 1) then 		 
         pnumerator = pscaling + pcoreradpv
      else
         pnumerator = pscaling	  
      end if
      
      ! if plasma not ignited include injected power
      if (ignite == 0) then
         pdenom = falpha*palpepv + piepv + pinjemw/vol
      else
      ! if plasma ignited
         pdenom = falpha*palpepv + piepv
      end if

      args%cc     = 1.0D0 - pnumerator / pdenom
      args%con    = pdenom * (1.0D0 - args%cc)
      args%err    = pdenom * args%cc
      args%symbol = '='
      args%units  = 'MW/m3'

   end subroutine constraint_eqn_004

   subroutine constraint_eqn_005(args)
      !+ad_name  constraint_eqn_005
      !+ad_summ  Equation for density upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for density upper limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fdene, dnelimt
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  idensl : input integer : switch for density limit to enforce (constraint equation 5):<UL>
      !+ad_gloc          <LI> = 1 old ASDEX;
      !+ad_gloc          <LI> = 2 Borrass model for ITER (I);
      !+ad_gloc          <LI> = 3 Borrass model for ITER (II);
      !+ad_gloc          <LI> = 4 JET edge radiation;
      !+ad_gloc          <LI> = 5 JET simplified;
      !+ad_gloc          <LI> = 6 Hugill-Murakami Mq limit;
      !+ad_gloc          <LI> = 7 Greenwald limit</UL>
      !+ad_glos  fdene : input real : f-value for density limit
      !+ad_glos  dene : input real : electron density (/m3)
      !+ad_glos  dnelimt : input real : density limit (/m3)
      !+ad_glos  dnla : input real : line averaged electron density (m-3)
      use physics_variables, only: idensl, dnelimt, dnla, dene
      use constraint_variables, only: fdene
      implicit none
      type (constraint_args_type), intent(out) :: args

	   ! Apply Greenwald limit to line-averaged density
      if (idensl == 7) then
         args%cc     = 1.0D0 - fdene * dnelimt/dnla
         args%con    = fdene * dnelimt
         args%err    = fdene * dnelimt - dnla
         args%symbol = '<'
         args%units  = '/m3'
      else
         args%cc = 1.0D0 - fdene * dnelimt/dene
         args%con    = dnelimt * (1.0D0 - args%cc)
         args%err    = dene * args%cc
         args%symbol = '<'
         args%units  = '/m3'
      end if

   end subroutine constraint_eqn_005

   subroutine constraint_eqn_006(args)
      !+ad_name  constraint_eqn_006
      !+ad_summ  Equation for epsilon beta-poloidal upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for epsilon beta-poloidal upper limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fbeta, epbetmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fbeta : input real : f-value for epsilon beta-poloidal
      !+ad_glos  epbetmax : input real : maximum (eps*beta_poloidal)
      !+ad_glos  eps : input real : inverse aspect ratio
      !+ad_glos  betap : input real : poloidal beta
      use physics_variables, only: epbetmax, eps, betap
      use constraint_variables, only: fbeta, fbeta
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fbeta * epbetmax/(eps*betap)
      args%con = epbetmax * (1.0D0 - args%cc)
      args%err = (eps*betap) * args%cc
      args%symbol = '<'
      args%units = ''

   end subroutine constraint_eqn_006

   subroutine constraint_eqn_007(args)
      !+ad_name  constraint_eqn_007
      !+ad_summ  Equation for hot beam ion density
      !+ad_type  Subroutine
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for hot beam ion density
      !+ad_desc  This is a consistency equation (NBI)
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ignite : input integer : switch for ignition assumption:<UL>
      !+ad_gloc          <LI> = 0 do not assume plasma ignition;
      !+ad_gloc          <LI> = 1 assume ignited (but include auxiliary power in costs)</UL>
      !+ad_gloc  Obviously, ignite must be zero if current drive is required.
      !+ad_gloc  If ignite=1, any auxiliary power is assumed to be used only
      !+ad_gloc  during plasma start-up, and is excluded from all steady-state
      !+ad_gloc  power balance calculations.
      !+ad_glos  dnbeam2 : input real :  hot beam ion density from calculation (/m3)
      !+ad_glos  dnbeam : input real : hot beam ion density, variable (/m3)
      use physics_variables, only: ignite, dnbeam2, dnbeam
      implicit none
      type (constraint_args_type), intent(out) :: args

	   ! Do not assume plasma ignition:
      if (ignite == 0) then
         args%cc     = 1.0D0 - dnbeam2/dnbeam
         args%con    = dnbeam * (1.0D0 - args%cc)
         args%err    = dnbeam * args%cc
         args%symbol = '='
         args%units  = '/m3'
      else
         args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')
         call report_error(1)
      end if

   end subroutine constraint_eqn_007

   subroutine constraint_eqn_008(args)
      !+ad_name  constraint_eqn_008
      !+ad_summ  Equation for neutron wall load upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for neutron wall load upper limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fwalld, walalw
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fwalld : input real : f-value for maximum wall load
      !+ad_glos  walalw : input real : allowable wall-load (MW/m2)
      !+ad_glos  wallmw : input real : average neutron wall load (MW/m2)
      use constraint_variables, only: fwalld, walalw
      use physics_variables, only: wallmw
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fwalld * walalw/wallmw
      args%con = fwalld * walalw
      args%err = fwalld * walalw - wallmw
      args%symbol = '<'
      args%units = 'MW/m2'

   end subroutine constraint_eqn_008

   subroutine constraint_eqn_009(args)
      !+ad_name  constraint_eqn_009
      !+ad_summ  Equation for fusion power upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for fusion power upper limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# ffuspow, powfmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ffuspow : input real : f-value for maximum fusion power
      !+ad_glos  powfmax : input real : maximum fusion power (MW)
      !+ad_glos  powfmw : input real : fusion power (MW)
      use constraint_variables, only: ffuspow, powfmax
      use physics_variables, only: powfmw
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - ffuspow * powfmax/powfmw
      args%con = powfmax * (1.0D0 - args%cc)
      args%err = powfmw * args%cc
      args%symbol = '<'
      args%units = 'MW'

   end subroutine constraint_eqn_009

   subroutine constraint_eqn_010(args)
      !+ad_name  constraint_eqn_010
      !+ad_summ  Equation for field at TF coil
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for field at TF coil
      !+ad_desc  (This is a consistency equation.)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  rmajor : input real : plasma major radius (m) 
      !+ad_glos  bt : input real : toroidal field on axis (T)
      !+ad_glos  rbmax : input real : radius of maximum TF B-field (m)
      !+ad_glos  bmaxtf : input real : mean peak field at TF coil (T)
      use physics_variables, only: rmajor, bt
      use tfcoil_variables, only: rbmax, bmaxtf
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - (rmajor*bt)/(rbmax*bmaxtf)
      args%con = (rbmax*bmaxtf) * (1.0D0 - args%cc)
      args%err = (rbmax*bmaxtf) * args%cc
      args%symbol = '='
      args%units = 'T.m'

   end subroutine constraint_eqn_010

   subroutine constraint_eqn_011(args)
      !+ad_name  constraint_eqn_011
      !+ad_summ  Equation for radial build
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for radial build
      !+ad_desc  (This is a consistency equation.)
      !+ad_desc    #=# build
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  rbld : input real : sum of thicknesses to the major radius (m)
      !+ad_glos  rmajor : input real : plasma major radius (m) 
      use build_variables, only: rbld
      use physics_variables, only: rmajor
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - rbld/rmajor
      args%con = rmajor * (1.0D0 - args%cc)
      args%err = rmajor * args%cc
      args%symbol = '='
      args%units = 'm'

   end subroutine constraint_eqn_011

   subroutine constraint_eqn_012(args)
      !+ad_name  constraint_eqn_012
      !+ad_summ  Equation for volt-second capability lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for volt-second capability lower limit
      !+ad_desc    #=# pfcoil
      !+ad_desc    #=#=# fvs, vsstt
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  vsstt : input real : total V-s needed (Wb)
      !+ad_gloc     vsstt (lower limit) is positive; vstot (available) is negative
      !+ad_glos  fvs : input real : f-value for flux-swing (V-s) requirement (STEADY STATE)
      !+ad_glos  vstot : input real :   total flux swing for pulse (Wb)
      use physics_variables, only: vsstt
      use constraint_variables, only: fvs
      use pfcoil_variables, only: vstot
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 + fvs * vstot/vsstt
      args%con = vsstt * (1.0D0 - args%cc)
      args%err = vsstt * args%cc
      args%symbol = '>'
      args%units = 'V.sec'

   end subroutine constraint_eqn_012

   subroutine constraint_eqn_013(args)
      !+ad_name  constraint_eqn_013
      !+ad_summ  Equation for burn time lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for burn time lower limit
      !+ad_desc    #=# times
      !+ad_desc    #=#=# ftburn, tbrnmn
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftburn : input real : f-value for minimum burn time
      !+ad_glos  tburn : input real : burn time (s) (calculated if lpulse=1)
      !+ad_glos  tbrnmn : input real :  minimum burn time (s)
      use constraint_variables, only: ftburn,tbrnmn
      use times_variables, only: tburn
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - ftburn * tburn/tbrnmn
      args%con = tbrnmn / ftburn
      args%err = tbrnmn / ftburn  - tburn
      args%symbol = '>'
      args%units = 'sec'

   end subroutine constraint_eqn_013

   subroutine constraint_eqn_014(args)
      !+ad_name  constraint_eqn_014
      !+ad_summ  Equation to fix number of NBI decay lengths to plasma centre
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation to fix number of NBI decay lengths to plasma centre
      !+ad_desc  This is a consistency equation
      !+ad_desc    #=# current_drive
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  taubeam : input real : neutral beam e-decay lengths to plasma centre
      !+ad_glos  tbeamin : input real : permitted neutral beam e-decay lengths to plasma centre
      use current_drive_variables, only: taubeam, tbeamin
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - taubeam/tbeamin
      args%con = tbeamin * (1.0D0 - args%cc)
      args%err = tbeamin * args%cc
      args%symbol = '='
      args%units = ''

   end subroutine constraint_eqn_014

   subroutine constraint_eqn_015(args)
      !+ad_name  constraint_eqn_015
      !+ad_summ  Equation for L-H power threshold limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for L-H power threshold limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# flhthresh, plhthresh
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  flhthresh : input real : f-value for L-H power threshold
      !+ad_glos  plhthresh : input real : L-H mode power threshold (MW)
      !+ad_glos  pdivt : input real : power to conducted to the divertor region (MW)
      use constraint_variables, only: flhthresh
      use physics_variables, only: plhthresh, pdivt
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  -(1.0D0 - flhthresh * plhthresh / pdivt)
      args%con = plhthresh
      args%err = plhthresh - pdivt / flhthresh
      if (flhthresh > 1.0D0) then
         args%symbol = '>'
      else
         args%symbol = '<'
      end if
      args%units = 'MW'

   end subroutine constraint_eqn_015

   subroutine constraint_eqn_016(args)
      !+ad_name  constraint_eqn_016
      !+ad_summ  Equation for net electric power lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for net electric power lower limit
      !+ad_desc    #=# heat_transport
      !+ad_desc    #=#=# fpnetel, pnetelin
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fpnetel : input real : f-value for net electric power
      !+ad_glos  pnetelmw : input real : net electric power (MW)
      !+ad_glos  pnetelin : input real : required net electric power (MW)
      use constraint_variables, only: fpnetel, pnetelin
      use heat_transport_variables, only: pnetelmw
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fpnetel * pnetelmw / pnetelin
      args%con = pnetelin
      args%err = pnetelmw - pnetelin / fpnetel
      args%symbol = '>'
      args%units = 'MW'

   end subroutine constraint_eqn_016

   subroutine constraint_eqn_017(args)
      !+ad_name  constraint_eqn_017
      !+ad_summ  Equation for radiation power upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_Argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for radiation power upper limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fradpwr, pradmaxpv
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  falpha : input real : fraction of alpha power deposited in plasma
      !+ad_glos  pinjmw : input real : total auxiliary injected power (MW)
      !+ad_glos  vol : input real : plasma volume (m3)
      !+ad_glos  palppv : input real : alpha power per volume (MW/m3)
      !+ad_glos  pchargepv :  input real : non-alpha charged particle fusion power per volume (MW/m3)
      !+ad_glos  pohmpv : input real : ohmic heating power per volume (MW/m3)
      !+ad_glos  fradpwr : input real : f-value for core radiation power limit
      !+ad_glos  pradpv : input real : total radiation power per volume (MW/m3)
      use physics_variables, only: falpha, vol, palppv, pchargepv, pohmpv, pradpv
      use current_drive_variables, only: pinjmw
      use constraint_variables, only: fradpwr
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! pradmaxpv : local real :  the maximum possible power/vol that can be radiated
      real(kind(1.0D0)) pradmaxpv
      pradmaxpv = pinjmw/vol + palppv*falpha + pchargepv + pohmpv
      args%cc =  1.0D0 - fradpwr * pradmaxpv / pradpv
      args%con = pradmaxpv * (1.0D0 - args%cc)
      args%err = pradpv * args%cc
      args%symbol = '<'
      args%units = 'MW/m3'

   end subroutine constraint_eqn_017

   subroutine constraint_eqn_018(args)
      !+ad_name  constraint_eqn_018
      !+ad_summ  Equation for divertor heat load upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for divertor heat load upper limit
      !+ad_desc    #=# divertor
      !+ad_desc    #=#=# fhldiv, hldivlim
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fhldiv : input real : peak resistive TF coil inboard leg power (MW)
      !+ad_glos  hldivlim : input real : heat load limit (MW/m2)
      !+ad_glos  hldiv : input real : divertor heat load (MW/m2)
      use constraint_variables, only: fhldiv
      use divertor_variables, only: hldivlim, hldiv
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fhldiv * hldivlim/hldiv
      args%con = hldivlim * (1.0D0 - args%cc)
      args%err = hldiv * args%cc
      args%symbol = '<'
      args%units = 'MW/m2'

   end subroutine constraint_eqn_018

   subroutine constraint_eqn_019(args)
      !+ad_name  constraint_eqn_019
      !+ad_summ  Equation for MVA upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for MVA upper limit
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fmva, mvalim
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  tfcpmw : input real : peak resistive TF coil inboard leg power (MW)
      !+ad_glos  tflegmw : input real : TF coil outboard leg resistive power (MW)
      !+ad_glos  fmva : input real : f-value for maximum MVA
      !+ad_glos  mvalim : input real : TF coil outboard leg resistive power (MW)
      use tfcoil_variables, only: tfcpmw, tflegmw
      use constraint_variables, only: fmva, mvalim
      implicit none
      type (constraint_args_type), intent(out) :: args
      ! totmva : local real : total MVA in TF coil (MW)
      real(kind(1.0D0)) totmva

      totmva = tfcpmw + tflegmw
      args%cc =  1.0D0 - fmva * mvalim/totmva
      args%con = mvalim * (1.0D0 - args%cc)
      args%err = totmva * args%cc
      args%symbol = '<'
      args%units = 'MVA'

   end subroutine constraint_eqn_019

   subroutine constraint_eqn_020(args)
      !+ad_name  constraint_eqn_020
      !+ad_summ  Equation for neutral beam tangency radius upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for neutral beam tangency radius upper limit
      !+ad_desc    #=# current_drive
      !+ad_desc    #=#=# fportsz, rtanmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fportsz : input real : f-value for neutral beam tangency radius limit
      !+ad_glos  rtanmax : input real : maximum tangency radius for centreline of beam (m)
      !+ad_glos  rtanbeam : input real : ratio of collision length / connection length
      use constraint_variables, only: fportsz
      use current_drive_variables, only: rtanmax, rtanbeam
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fportsz * rtanmax/rtanbeam
      args%con = rtanmax * (1.0D0 - args%cc)
      args%err = rtanbeam * args%cc
      args%symbol = '<'
      args%units = 'm'

   end subroutine constraint_eqn_020

   subroutine constraint_eqn_021(args)
      !+ad_name  constraint_eqn_021
      !+ad_summ  Equation for minor radius lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for minor radius lower limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# frminor, aplasmin
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  frminor : input real : f-value for minor radius limit
      !+ad_glos  rminor : input real : plasma minor radius (m)
      !+ad_glos  aplasmin : input real : minimum minor radius (m)
      use constraint_variables, only: frminor
      use physics_variables, only: rminor
      use build_variables, only: aplasmin 
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - frminor * rminor/aplasmin
      args%con = aplasmin * (1.0D0 - args%cc)
      args%err = aplasmin * args%cc
      args%symbol = '>'
      args%units = ''

   end subroutine constraint_eqn_021

   subroutine constraint_eqn_022(args)
      !+ad_name  constraint_eqn_022
      !+ad_summ  Equation for divertor collision/connection length ratio upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for divertor collision/connection length ratio upper limit
      !+ad_desc    #=# divertor
      !+ad_desc    #=#=# fdivcol, rlenmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fdivcol : input real : f-value for divertor collisionality
      !+ad_glos  rlenmax : input real : maximum value for length ratio (rlclolcn)
      !+ad_glos  rlclolcn : input real : ratio of collision length / connection length
      use constraint_variables, only: fdivcol
      use divertor_variables, only: rlenmax, rlclolcn
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fdivcol * rlenmax / rlclolcn
      args%con = rlenmax * (1.0D0 - args%cc)
      args%err = rlclolcn * args%cc
      args%symbol = '<'
      args%units = ''

   end subroutine constraint_eqn_022

   subroutine constraint_eqn_023(args)
      !+ad_name  constraint_eqn_023
      !+ad_summ  Equation for conducting shell radius / rminor upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for conducting shell radius / rminor upper limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fcwr, cwrmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  rminor : input real : plasma minor radius (m)
      !+ad_glos  scraplo : input real : gap between plasma and first wall, outboard side (m)
      !+ad_glos  fwoth : input real : outboard first wall thickness, initial estimate (m)
      !+ad_glos  blnkoth : input real : outboard blanket thickness (m)
      !+ad_glos  fcwr : input real : f-value for conducting wall radius / rminor limit
      !+ad_glos  cwrmax : input real : maximum ratio of conducting wall distance to plasma minor radius for vertical stability
      use physics_variables, only: rminor, cwrmax
      use build_variables, only: scraplo, fwoth, blnkoth
      use constraint_variables, only: fcwr
      implicit none
      type (constraint_args_type), intent(out) :: args
      ! rcw : local real : conducting shell radius (m)
      real(kind(1.0D0)) rcw

      rcw = rminor + scraplo + fwoth + blnkoth
      args%cc =  1.0D0 - fcwr * cwrmax*rminor / rcw
      args%con = cwrmax*rminor * (1.0D0 - args%cc)
      args%err = rcw * args%cc
      args%symbol = '<'
      args%units = 'm'

   end subroutine constraint_eqn_023

   subroutine constraint_eqn_024(args)
      !+ad_name  constraint_eqn_024
      !+ad_summ  Equation for beta upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for beta upper limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fbetatry, betalim
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  iculbl : input integer : switch for beta limit scaling (constraint equation  24):<UL>
      !+ad_gloc          <LI> = 0 apply limit to total beta;
      !+ad_gloc          <LI> = 1 apply limit to thermal beta;
      !+ad_gloc          <LI> = 2 apply limit to thermal + neutral beam beta</UL>
      !+ad_glos  istell : input integer : switch for stellarator option (set via <CODE>device.dat</CODE>):<UL>
      !+ad_gloc          <LI> = 0 use tokamak model;
      !+ad_gloc          <LI> = 1 use stellarator model</UL>
      !+ad_glos  fbetatry : input real : f-value for beta limit
      !+ad_glos  betalim : input real : allowable beta
      !+ad_glos  beta : input real : total plasma beta (calculated if ipedestal =3)
      !+ad_glos  betaft : input real : fast alpha beta component
      !+ad_glos  betanb : input real : neutral beam beta component
      use physics_variables, only: iculbl, betalim, beta, betanb, betaft
      use stellarator_variables, only: istell
      use constraint_variables, only: fbetatry
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! Include all beta components: relevant for both tokamaks and stellarators
      if ((iculbl == 0).or.(istell /= 0)) then
         args%cc =  1.0D0 - fbetatry * betalim/beta
         args%con = betalim
         args%err = betalim - beta / fbetatry
         args%symbol = '<'
         args%units = ''
      ! Here, the beta limit applies to only the thermal component, not the fast alpha or neutral beam parts
      else if (iculbl == 1) then
         args%cc = 1.0D0 - fbetatry * betalim/(beta-betaft-betanb)
         args%con = betalim
         args%err = betalim - (beta-betaft-betanb) / fbetatry
         args%symbol = '<'
         args%units = ''
      ! Beta limit applies to thermal + neutral beam: components of the total beta, i.e. excludes alphas
      else ! iculbl == 2
         args%cc = 1.0D0 - fbetatry * betalim/(beta-betaft)
         args%con = betalim * (1.0D0 - args%cc)
         args%err = (beta-betaft) * args%cc
         args%symbol = '<'
         args%units = ''
      end if 

   end subroutine constraint_eqn_024

   subroutine constraint_eqn_025(args)
      !+ad_name  constraint_eqn_025
      !+ad_summ  Equation for peak toroidal field upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for peak toroidal field upper limit
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fpeakb, bmxlim
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fpeakb : input real : f-value for maximum toroidal field
      !+ad_glos  bmxlim : input real : maximum peak toroidal field (T)
      !+ad_glos  bmaxtf : input real : mean peak field at TF coil (T)
      use constraint_variables, only: fpeakb, bmxlim
      use tfcoil_variables, only: bmaxtf
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fpeakb * bmxlim/bmaxtf
      args%con = bmxlim * (1.0D0 - args%cc)
      args%err = bmaxtf * args%cc
      args%symbol = '<'
      args%units = 'T'

   end subroutine constraint_eqn_025

   subroutine constraint_eqn_026(args)
      !+ad_name  constraint_eqn_026
      !+ad_summ  Equation for Central Solenoid current density upper limit at EOF
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for Central Solenoid current density upper limit at EOF
      !+ad_desc    #=# pfcoil
      !+ad_desc    #=#=# fjohc, rjohc
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fjohc : input real : f-value for central solenoid current at end-of-flattop
      !+ad_glos  rjohc : input real : allowable central solenoid current density at end of flat-top (A/m2)
      !+ad_glos  coheof : input real : central solenoid overall current density at end of flat-top (A/m2)
      use constraint_variables, only: fjohc
      use pfcoil_variables, only: rjohc, coheof
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fjohc * rjohc/coheof
      args%con = rjohc
      args%err = rjohc - coheof / fjohc
      args%symbol = '<'
      args%units = 'A/m2'

   end subroutine constraint_eqn_026

   subroutine constraint_eqn_027(args)
      !+ad_name  constraint_eqn_027
      !+ad_summ  Equation for Central Solenoid current density upper limit at BOP
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for Central Solenoid current density upper limit at BOP
      !+ad_desc    #=# pfcoil
      !+ad_desc    #=#=# fjohc0, rjohc0
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fjohc0 : input real : f-value for central solenoid current at beginning of pulse
      !+ad_glos  rjohc0 : input real : allowable central solenoid current density at beginning of pulse (A/m2)
      !+ad_glos  cohbop : input real : central solenoid overall current density at beginning of pulse (A/m2)
      use constraint_variables, only: fjohc0
      use pfcoil_variables, only: rjohc0, cohbop
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fjohc0 * rjohc0/cohbop
      args%con = rjohc0
      args%err = rjohc0 - cohbop / fjohc0
      args%symbol = '<'
      args%units = 'A/m2'

   end subroutine constraint_eqn_027

   subroutine constraint_eqn_028(args)
      !+ad_name  constraint_eqn_028
      !+ad_summ  Equation for fusion gain (big Q) lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for fusion gain (big Q) lower limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fqval, bigqmin
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fqval : input real : pf-value for Q
      !+ad_glos  bigq : input real : Fusion gain; P_fusion / (P_injection + P_ohmic)
      !+ad_glos  bigqmin : input real : minimum fusion gain Q
      !+ad_glos  ignite : input integer : switch for ignition assumption:<UL>
      !+ad_gloc          <LI> = 0 do not assume plasma ignition;
      !+ad_gloc          <LI> = 1 assume ignited (but include auxiliary power in costs)</UL>
      !+ad_gloc       Obviously, ignite must be zero if current drive is required.
      !+ad_gloc       If ignite=1, any auxiliary power is assumed to be used only
      !+ad_gloc       during plasma start-up, and is excluded from all steady-state
      !+ad_gloc       power balance calculations.
      use constraint_variables, only: fqval, bigqmin
      use current_drive_variables, only: bigq
      use physics_variables, only: ignite
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! if plasma is not ignited ...
      if (ignite == 0) then
         args%cc =  1.0D0 - fqval * bigq/bigqmin
         args%con = bigqmin * (1.0D0 - args%cc)
         args%err = bigqmin * args%cc
         args%symbol = '>'
         args%units = ''
      ! if plasma is ignited report error
      else
         args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')
         call report_error(4)
      end if 

   end subroutine constraint_eqn_028

   subroutine constraint_eqn_029(args)
      !+ad_name  constraint_eqn_029
      !+ad_summ  Equation for inboard major radius: This is a consistency equation
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for inboard major radius: This is a consistency equation
      !+ad_desc    #=# build
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  rmajor : input real : plasma major radius (m) (iteration variable 3)
      !+ad_glos  rminor : input real : plasma minor radius (m)
      !+ad_glos  rinboard : input real : plasma inboard radius (m)
      use physics_variables, only: rmajor, rminor
      use build_variables, only: rinboard
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - (rmajor - rminor) / rinboard
      args%con = rinboard * (1.0D0 - args%cc)
      args%err = rinboard * args%cc
      args%symbol = '='
      args%units = 'm'

   end subroutine constraint_eqn_029

   subroutine constraint_eqn_030(args)
      !+ad_name  constraint_eqn_030
      !+ad_summ  Equation for injection power upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for injection power upper limit
      !+ad_desc    #=# current_drive
      !+ad_desc    #=#=# fpinj, pinjalw
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  pinjmw : input real : total auxiliary injected power (MW)
      !+ad_glos  fpinj : input real : f-value for injection power
      !+ad_glos  pinjalw : input real : Maximum allowable value for injected power (MW)
      use current_drive_variables, only: pinjmw, pinjalw
      use constraint_variables, only: fpinj
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - pinjmw / (fpinj * pinjalw)
      args%con = pinjalw
      args%err = pinjalw  - pinjmw * fpinj
      args%symbol = '<'
      args%units = 'MW'

   end subroutine constraint_eqn_030

   subroutine constraint_eqn_031(args)
      !+ad_name  constraint_eqn_031
      !+ad_summ  Equation for TF coil case stress upper limit (SCTF)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for TF coil case stress upper limit (SCTF)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fstrcase, alstrtf
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fstrcase : input real : f-value for TF coil case stress
      !+ad_glos  alstrtf : input real : allowable Tresca stress in TF coil structural material (Pa)
      !+ad_glos  strtf2 : input real : Constrained stress in TF coil case (Pa) 
      use constraint_variables, only: fstrcase
      use tfcoil_variables, only: alstrtf, strtf2
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fstrcase * alstrtf/strtf2
      args%con = alstrtf
      args%err = alstrtf - strtf2 / fstrcase
      args%symbol = '<'
      args%units = 'Pa'

   end subroutine constraint_eqn_031

   subroutine constraint_eqn_032(args)
      !+ad_name  constraint_eqn_032
      !+ad_summ  Equation for TF coil conduit stress upper limit (SCTF)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for TF coil conduit stress upper limit (SCTF)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fstrcond, alstrtf
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fstrcond : input real : f-value for TF coil conduit stress
      !+ad_glos  alstrtf : input real : allowable Tresca stress in TF coil structural material (Pa)
      !+ad_glos  strtf1 : input real : Constrained Tresca stress in TF conductor conduit (Pa) 
      use constraint_variables, only: fstrcond
      use tfcoil_variables, only: alstrtf, strtf1
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fstrcond * alstrtf/strtf1
      args%con = alstrtf
      args%err = alstrtf - strtf1 / fstrcond
      args%symbol = '<'
      args%units = 'Pa'

   end subroutine constraint_eqn_032
   
   subroutine constraint_eqn_033(args)
      !+ad_name  constraint_eqn_033
      !+ad_summ  Equation for TF coil operating/critical J upper limit (SCTF)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for TF coil operating/critical J upper limit (SCTF)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fiooic, jwdgcrt
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fiooic : input real : f-value for TF coil operating current / critical
      !+ad_glos  jwdgcrt : input real : critical current density for winding pack (A/m2)
      !+ad_glos  jwptf : input real : winding pack current density (A/m2) 
      use constraint_variables, only: fiooic
      use tfcoil_variables, only: jwdgcrt, jwptf
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fiooic * jwdgcrt/jwptf
      args%con = jwdgcrt * (1.0D0 - args%cc)
      args%err = jwptf * args%cc
      args%symbol = '<'
      args%units = 'A/m2'

   end subroutine constraint_eqn_033
   
   subroutine constraint_eqn_034(args)
      !+ad_name  constraint_eqn_034
      !+ad_summ  Equation for TF coil dump voltage upper limit (SCTF)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for TF coil dump voltage upper limit (SCTF)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fvdump, vdalw
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fvdump : input real : f-value for dump voltage
      !+ad_glos  vdalw : input real : max voltage across TF coil during quench (kV)
      !+ad_glos  vtfskv : input real : voltage across a TF coil during quench (kV) 
      use constraint_variables, only: fvdump
      use tfcoil_variables, only: vdalw, vtfskv
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fvdump * vdalw/vtfskv
      args%con = vdalw
      args%err = vdalw - vtfskv
      args%symbol = '<'
      args%units = 'V'

   end subroutine constraint_eqn_034

   subroutine constraint_eqn_035(args)
      !+ad_name  constraint_eqn_035
      !+ad_summ  Equation for TF coil J_wp/J_prot upper limit (SCTF)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for TF coil J_wp/J_prot upper limit (SCTF)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fjprot, jwdgpro
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fjprot : input real : f-value for TF coil winding pack current density
      !+ad_glos  jwdgpro : input real : allowable TF coil winding pack current density, for dump temperature rise protection (A/m2)
      !+ad_glos  jwptf : input real : winding pack current density (A/m2) 
      use constraint_variables, only: fjprot
      use tfcoil_variables, only: jwdgpro, jwptf
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fjprot * jwdgpro/jwptf
      args%con = jwdgpro
      args%err = jwptf - jwptf
      args%symbol = '<'
      args%units = 'A/m2'

   end subroutine constraint_eqn_035

   subroutine constraint_eqn_036(args)
      !+ad_name  constraint_eqn_036
      !+ad_summ  Equation for TF coil s/c temperature margin lower limit (SCTF)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for TF coil s/c temperature margin lower limit (SCTF)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# ftmargtf, tmargmin_tf
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftmargtf : input real : f-value for TF coil temperature margin
      !+ad_glos  tmargtf : input real : TF coil temperature margin (K)
      !+ad_glos  tmargmin_tf : input real : minimum allowable temperature margin : TF coils (K) 
      use constraint_variables, only: ftmargtf
      use tfcoil_variables, only: tmargtf, tmargmin_tf
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - ftmargtf * tmargtf/tmargmin_tf
      args%con = tmargmin_tf
      args%err = tmargmin_tf - tmargtf
      args%symbol = '>'
      args%units = 'K'

   end subroutine constraint_eqn_036

   subroutine constraint_eqn_037(args)
      !+ad_name  constraint_eqn_037
      !+ad_summ  Equation for current drive gamma upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for current drive gamma upper limit
      !+ad_desc    #=# current_drive
      !+ad_desc    #=#=# fgamcd, gammax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fgamcd : input real : f-value for current drive gamma
      !+ad_glos  gammax : input real : maximum current drive gamma
      !+ad_glos  gamcd : input real : normalised current drive efficiency (1.0e20 A/W-m2) 
      use constraint_variables, only: fgamcd, gammax
      use current_drive_variables, only: gamcd
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fgamcd * gammax/gamcd
      args%con = gammax * (1.0D0 - args%cc)
      args%err = gamcd * args%cc
      args%symbol = '<'
      args%units = '1E20 A/Wm2'

   end subroutine constraint_eqn_037

   subroutine constraint_eqn_038(args)
      !+ad_name  constraint_eqn_038
      !+ad_summ  Obsolete
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_desc  Obsolete
      !+ad_desc    #=# empty
      !+ad_desc    #=#=# empty
      implicit none
	   ! Dummy formal arguments, for compliance with interface
      type (constraint_args_type), intent(out) :: args

      args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')

   end subroutine constraint_eqn_038

   subroutine constraint_eqn_039(args)
      !+ad_name  constraint_eqn_039
      !+ad_summ  Equation for first wall temperature upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for first wall temperature upper limit
      !+ad_desc    #=# fwbs
      !+ad_desc    #=#=# ftpeak, tfwmatmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftpeak : input real : f-value for first wall peak temperature
      !+ad_glos  tfwmatmax : input real : maximum temperature of first wall material (K) (secondary_cycle>1)
      !+ad_glos  tpeak : input real : peak first wall temperature (K) 
      use constraint_variables, only: ftpeak
      use fwbs_variables, only: tfwmatmax, tpeak
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! If the temperature peak == 0 then report an error
      if (tpeak < 1.0D0) call report_error(5)
      args%cc =  1.0D0 - ftpeak * tfwmatmax/tpeak
      args%con = tfwmatmax * (1.0D0 - args%cc)
      args%err = tpeak * args%cc
      args%symbol = '<'
      args%units = 'K'

   end subroutine constraint_eqn_039

   subroutine constraint_eqn_040(args)
      !+ad_name  constraint_eqn_040
      !+ad_summ  Equation for auxiliary power lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for auxiliary power lower limit
      !+ad_desc    #=# current_drive
      !+ad_desc    #=#=# fauxmn, auxmin
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fauxmn : input real : f-value for minimum auxiliary power
      !+ad_glos  pinjmw : input real : total auxiliary injected power (MW) 
      !+ad_glos  auxmin : input real : minimum auxiliary power (MW)
      use constraint_variables, only: fauxmn, auxmin
      use current_drive_variables, only: pinjmw
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fauxmn * pinjmw/auxmin
      args%con = auxmin * (1.0D0 - args%cc)
      args%err = auxmin * args%cc
      args%symbol = '>'
      args%units = 'MW'

   end subroutine constraint_eqn_040

   subroutine constraint_eqn_041(args)
      !+ad_name  constraint_eqn_041
      !+ad_summ  Equation for plasma current ramp-up time lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for plasma current ramp-up time lower limit
      !+ad_desc    #=# times
      !+ad_desc    #=#=# ftohs, tohsmn
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftohs : input real : f-value for plasma current ramp-up time
      !+ad_glos  tohs : input real : plasma current ramp-up time for current initiation (s) 
      !+ad_glos  tohsmn : input real : minimum plasma current ramp-up time (s)
      use constraint_variables, only: ftohs, tohsmn
      use times_variables, only: tohs
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - ftohs * tohs/tohsmn
      args%con = tohsmn * (1.0D0 - args%cc)
      args%err = tohsmn * args%cc
      args%symbol = '>'
      args%units = 'sec'

   end subroutine constraint_eqn_041

   subroutine constraint_eqn_042(args)
      !+ad_name  constraint_eqn_042
      !+ad_summ  Equation for cycle time lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for cycle time lower limit
      !+ad_desc    #=# times
      !+ad_desc    #=#=# ftcycl, tcycmn
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftcycl : input real : f-value for cycle time
      !+ad_glos  tcycle : input real : full cycle time (s) 
      !+ad_glos  tcycmn : input real : minimum cycle time (s) 
      use constraint_variables, only: ftcycl, tcycmn
      use times_variables, only: tcycle
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! if the minimum cycle time == 0 report an error
      if (tcycmn < 1.0D0) call report_error(6)
      args%cc =  1.0D0 - ftcycl * tcycle/tcycmn
      args%con = tcycmn * (1.0D0 - args%cc)
      args%err = tcycmn * args%cc
      args%symbol = '>'
      args%units = 'sec'

   end subroutine constraint_eqn_042

   subroutine constraint_eqn_043(args)
      !+ad_name  constraint_eqn_043
      !+ad_summ  Equation for average centrepost temperature: This is a consistency equation (TART)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for average centrepost temperature: This is a consistency equation (TART)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  tcpav : input real : average temp of TF coil inboard leg conductor (C)e
      !+ad_glos  tcpav2 : input real : centrepost average temperature (C) (for consistency) 
      !+ad_glos  itart : input integer : switch for spherical tokamak (ST) models:<UL>
      !+ad_gloc         <LI> = 0 use conventional aspect ratio models;
      !+ad_gloc         <LI> = 1 use spherical tokamak models</UL>
      use tfcoil_variables, only: tcpav, tcpav2
      use physics_variables, only: itart
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! if the machine isn't a ST then report error
      if (itart == 0) call report_error(7)
      args%cc =   1.0D0 - tcpav/tcpav2
      args%con = tcpav2 * (1.0D0 - args%cc)
      args%err = tcpav2 * args%cc
      args%symbol = '='
      args%units = 'deg C'

   end subroutine constraint_eqn_043

   subroutine constraint_eqn_044(args)
      !+ad_name  constraint_eqn_044
      !+ad_summ  Equation for centrepost temperature upper limit (TART)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for centrepost temperature upper limit (TART)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fptemp, ptempalw
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fptemp : input real : f-value for peak centrepost temperature
      !+ad_glos  ptempalw : input real : maximum peak centrepost temperature (C) 
      !+ad_glos  tcpmax : input real :  peak centrepost temperature (C)
      !+ad_glos  itart : input integer : switch for spherical tokamak (ST) models:<UL>
      !+ad_gloc         <LI> = 0 use conventional aspect ratio models;
      !+ad_gloc         <LI> = 1 use spherical tokamak models</UL>
      use constraint_variables, only: fptemp
      use tfcoil_variables, only: ptempalw, tcpmax
      use physics_variables, only: itart
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! if the machine isn't a ST then report error
      if (itart == 0) call report_error(8)
      args%cc =   1.0D0 - fptemp * ptempalw / tcpmax
      args%con = ptempalw * (1.0D0 - args%cc)
      args%err = tcpmax * args%cc
      args%symbol = '<'
      args%units = 'deg C'

   end subroutine constraint_eqn_044

   subroutine constraint_eqn_045(args)
      !+ad_name  constraint_eqn_045
      !+ad_summ  Equation for edge safety factor lower limit (TART)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for edge safety factor lower limit (TART)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fq, qlim
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fq : input real : f-value for edge safety factor
      !+ad_glos  q : safety factor 'near' plasma edge: equal to q95 
      !+ad_gloc  (unless icurr = 2 (ST current scaling), in which case q = mean edge safety factor qbar)
      !+ad_glos  qlim : input real :  lower limit for edge safety factor
      !+ad_glos  itart : input integer : switch for spherical tokamak (ST) models:<UL>
      !+ad_gloc         <LI> = 0 use conventional aspect ratio models;
      !+ad_gloc         <LI> = 1 use spherical tokamak models</UL>
      use constraint_variables, only: fq
      use physics_variables, only: q, qlim, itart
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! if the machine isn't a ST then report error
      if (itart == 0) call report_error(9)
      args%cc =   1.0D0 - fq * q/qlim
      args%con = qlim * (1.0D0 - args%cc)
      args%err = qlim * args%cc
      args%symbol = '<'
      args%units = ''

   end subroutine constraint_eqn_045

   subroutine constraint_eqn_046(args)
      !+ad_name  constraint_eqn_046
      !+ad_summ  Equation for Ip/Irod upper limit (TART)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for Ip/Irod upper limit (TART)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fipir, cratmx
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  eps : input real :  inverse aspect ratio
      !+ad_glos  fipir : input real :  f-value for Ip/Irod limit
      !+ad_glos  ritfc : input real :  total (summed) current in TF coils (A)
      !+ad_glos  plascur : input real :  plasma current (A)
      !+ad_glos  itart : input integer : switch for spherical tokamak (ST) models:<UL>
      !+ad_gloc         <LI> = 0 use conventional aspect ratio models;
      !+ad_gloc         <LI> = 1 use spherical tokamak models</UL>
      use physics_variables, only: eps, plascur, itart
      use constraint_variables, only: fipir
      use tfcoil_variables, only: ritfc
      implicit none
      ! cratmx : local real : maximum ratio of plasma current to centrepost current
      real(kind(1.0D0)) :: cratmx
      type (constraint_args_type), intent(out) :: args

      ! if the machine isn't a ST then report error
      if (itart == 0) call report_error(10)
      cratmx = 1.0D0 + 4.91D0*(eps-0.62D0)
      args%cc =  1.0D0 - fipir * cratmx * ritfc/plascur
      args%con = cratmx * (1.0D0 - args%cc)
      args%err = plascur/ritfc * args%cc
      args%symbol = '<'
      args%units = ''

   end subroutine constraint_eqn_046

   subroutine constraint_eqn_047(args)
      !+ad_name  constraint_eqn_047
      !+ad_summ  Issue #508 Remove RFP option: Relevant only to reversed field pinch devices
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_desc  Issue #508 Remove RFP option: Relevant only to reversed field pinch devices
      !+ad_desc  Equation for TF coil toroidal thickness upper limit
      !+ad_desc    #=# empty
      !+ad_desc    #=#=# empty
      implicit none
      ! Dummy formal arguments, just to comply with the subroutine interface
      type (constraint_args_type), intent(out) :: args

      args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')

   end subroutine constraint_eqn_047
   
   subroutine constraint_eqn_048(args)
      !+ad_name  constraint_eqn_048
      !+ad_summ  Equation for poloidal beta upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for poloidal beta upper limit
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fbetap, betpmx
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fbetap : input real : rf-value for poloidal beta
      !+ad_glos  betpmx : input real :  maximum poloidal beta
      !+ad_glos  betap : input real :  poloidal beta
      use constraint_variables, only: fbetap, betpmx
      use physics_variables, only: betap
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fbetap * betpmx/betap
      args%con = betpmx * (1.0D0 - args%cc)
      args%err = betap * args%cc
      args%symbol = '<'
      args%units = ''

   end subroutine constraint_eqn_048

   subroutine constraint_eqn_049(args)
      !+ad_name  constraint_eqn_049
      !+ad_summ  Issue #508 Remove IFE option: Equation for repetition rate upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_desc  Issue #508 Remove IFE option: Equation for repetition rate upper limit
      !+ad_desc    #=# empty
      !+ad_desc    #=#=# empty
      ! Dummy formal arguments, just to comply with the subroutine interface
      type (constraint_args_type), intent(out) :: args

      args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')

   end subroutine constraint_eqn_049

   subroutine constraint_eqn_050(args)
      !+ad_name  constraint_eqn_050
      !+ad_summ  Issue #508 Remove IFE option: Equation for repetition rate upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_desc  Issue #508 Remove IFE option: Equation for repetition rate upper limit
      !+ad_desc    #=# empty
      !+ad_desc    #=#=# empty
      ! Dummy formal arguments, just to comply with the subroutine interface
      type (constraint_args_type), intent(out) :: args

      args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')

   end subroutine constraint_eqn_050

   subroutine constraint_eqn_051(args)
      !+ad_name  constraint_eqn_051
      !+ad_summ  Equation to enforce startup flux = available startup flux
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation to enforce startup flux = available startup flux
      !+ad_desc    #=# pfcoil
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  vsres : input real : resistive losses in startup V-s (Wb)
      !+ad_glos  vsind : input real :  internal and external plasma inductance V-s (Wb))
      !+ad_glos  vssu : input real :  total flux swing for startup (Wb)
      use physics_variables, only: vsres, vsind
      use pfcoil_variables, only: vssu
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - (vsres+vsind) / vssu
      args%con = vssu * (1.0D0 - args%cc)
      args%err = vssu * args%cc
      args%symbol = '='
      args%units = 'V.s'

   end subroutine constraint_eqn_051

   subroutine constraint_eqn_052(args)
      !+ad_name  constraint_eqn_052
      !+ad_summ  Equation for tritium breeding ratio lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for tritium breeding ratio lower limit
      !+ad_desc    #=# fwbs
      !+ad_desc    #=#=# ftbr, tbrmin
      !+ad_desc  ? TODO should this only be for certain blanket models ?
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftbr : input real : f-value for minimum tritium breeding ratio
      !+ad_glos  tbr : input real :  tritium breeding ratio (iblanket=2,3 (KIT HCPB/HCLL))
      !+ad_glos  tbrmin : input real :  minimum tritium breeding ratio (If iblanket=1, tbrmin=minimum 5-year time-averaged tritium breeding ratio)
      use constraint_variables, only: ftbr, tbrmin
      use fwbs_variables, only: tbr
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - ftbr * tbr/tbrmin
      args%con = tbrmin * (1.0D0 - args%cc)
      args%err = tbrmin * args%cc
      args%symbol = '>'
      args%units = ''

   end subroutine constraint_eqn_052

   subroutine constraint_eqn_053(args)
      !+ad_name  constraint_eqn_053
      !+ad_summ  Equation for fast neutron fluence on TF coil upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for fast neutron fluence on TF coil upper limit
      !+ad_desc    #=# fwbs
      !+ad_desc    #=#=# fflutf, nflutfmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fflutf : input real : f-value for maximum TF coil nuclear heating
      !+ad_glos  nflutfmax : input real :  max fast neutron fluence on TF coil (n/m2)
      !+ad_glos  nflutf : input real :  peak fast neutron fluence on TF coil superconductor (n/m2)
      use constraint_variables, only: fflutf, nflutfmax
      use fwbs_variables, only: nflutf
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0D0 - fflutf * nflutfmax/nflutf
      args%con = nflutfmax * (1.0D0 - args%cc)
      args%err = nflutf * args%cc
      args%symbol = '<'
      args%units = 'neutron/m2'

   end subroutine constraint_eqn_053

   subroutine constraint_eqn_054(args)
      !+ad_name  constraint_eqn_054
      !+ad_summ  Equation for peak TF coil nuclear heating upper limi
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for peak TF coil nuclear heating upper limi
      !+ad_desc    #=# fwbs
      !+ad_desc    #=#=# fptfnuc, ptfnucmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fptfnuc : input real : f-value for maximum TF coil nuclear heating
      !+ad_glos  ptfnucmax : input real :  maximum nuclear heating in TF coil (MW/m3)
      !+ad_glos  ptfnucpm3 : input real :  nuclear heating in the TF coil (MW/m3) (blktmodel>0)
      use constraint_variables, only: fptfnuc, ptfnucmax
      use fwbs_variables, only: ptfnucpm3
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - fptfnuc * ptfnucmax/ptfnucpm3
      args%con = ptfnucmax * (1.0D0 - args%cc)
      args%err = ptfnucpm3 * args%cc
      args%symbol = '<'
      args%units = 'MW/m3'

   end subroutine constraint_eqn_054

   subroutine constraint_eqn_055(args)
      !+ad_name  constraint_eqn_055
      !+ad_summ  Equation for helium concentration in vacuum vessel upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for helium concentration in vacuum vessel upper limit
      !+ad_desc    #=# fwbs
      !+ad_desc    #=#=# fvvhe, vvhemax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fvvhe : input real : f-value for vacuum vessel He concentration limit
      !+ad_glos  vvhealw : input real :  allowed maximum helium concentration in vacuum vessel at end of plant life (appm) (iblanket =2)
      !+ad_glos  vvhemax : ivvhemaxnput real :  maximum helium concentration in vacuum vessel at end of plant life (appm) (iblanket=2 (KIT HCPB))
      !+ad_glos  iblanket : input integer : switch for blanket model: <UL>
      !+ad_gloc             <LI> = 1 CCFE HCPB model;
      !+ad_gloc             <LI> = 2 KIT HCPB model;
      !+ad_gloc             <LI> = 3 CCFE HCPB model with Tritium Breeding Ratio calculation;
      !+ad_gloc             <LI> = 4 KIT HCLL model</UL>
      use constraint_variables, only: fvvhe, vvhealw
      use fwbs_variables, only: vvhemax, iblanket
      implicit none
      type (constraint_args_type), intent(out) :: args

      if (iblanket == 2) then
         args%cc = 1.0D0 - fvvhe * vvhealw/vvhemax
         args%con = vvhealw * (1.0D0 - args%cc)
         args%err = vvhemax * args%cc
         args%symbol = '<'
         args%units = 'appm'
      else ! iblanket /= 2
         args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')
         call report_error(173)
      end if

   end subroutine constraint_eqn_055

   subroutine constraint_eqn_056(args)
      !+ad_name  constraint_eqn_056
      !+ad_summ  Equation for power through separatrix / major radius upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for power through separatrix / major radius upper limit
      !+ad_desc    #=# current_drive
      !+ad_desc    #=#=# fnbshinef, nbshinefmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fpsepr : input real : f-value for maximum Psep/R limit
      !+ad_glos  pseprmax : input real :  maximum ratio of power crossing the separatrix to plasma major radius (Psep/R) (MW/m)
      !+ad_glos  pdivt : input real :  power to be conducted to the divertor region (MW)
      !+ad_glos  rmajor : input real :  plasma major radius (m) 
      use constraint_variables, only: fpsepr, pseprmax
      use physics_variables, only: pdivt, rmajor
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - fpsepr * pseprmax / (pdivt/rmajor)
      args%con = pseprmax * (1.0D0 - args%cc)
      args%err = (pdivt/rmajor) * args%cc
      args%symbol = '<'
      args%units = 'MW/m'

   end subroutine constraint_eqn_056

   subroutine constraint_eqn_057(args)
      !+ad_name  constraint_eqn_057
      !+ad_summ  Obsolete
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_desc  Obsolete
      !+ad_desc    #=# empty
      !+ad_desc    #=#=# empty
      ! Dummy formal arguments, just to comply with the subroutine interface
      type (constraint_args_type), intent(out) :: args

      args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')

   end subroutine constraint_eqn_057

   subroutine constraint_eqn_058(args)
      !+ad_name  constraint_eqn_058
      !+ad_summ  Obsolete
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_desc  Obsolete
      !+ad_desc    #=# empty
      !+ad_desc    #=#=# empty
      ! Dummy formal arguments, just to comply with the subroutine interface
      type (constraint_args_type), intent(out) :: args

      args = constraint_args_type(0.0D0, 0.0D0, 0.0D0, '', '')

   end subroutine constraint_eqn_058

   subroutine constraint_eqn_059(args)
      !+ad_name  constraint_eqn_059
      !+ad_summ  Equation for neutral beam shine-through fraction upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for neutral beam shine-through fraction upper limit
      !+ad_desc    #=# current_drive
      !+ad_desc    #=#=# fnbshinef, nbshinefmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fnbshinef : input real : f-value for maximum neutral beam shine-through fraction
      !+ad_glos  nbshinefmax : input real :  maximum neutral beam shine-through fraction
      !+ad_glos  nbshinef : input real :  neutral beam shine-through fraction
      use constraint_variables, only: fnbshinef, nbshinefmax
      use current_drive_variables, only: nbshinef
      implicit none
      type (constraint_args_type), intent(out) :: args
      args%cc = 1.0D0 - fnbshinef * nbshinefmax / nbshinef
      args%con = nbshinefmax * (1.0D0 - args%cc)
      args%err = nbshinef * args%cc
      args%symbol = '<'
      args%units = ''
   end subroutine constraint_eqn_059
   
   subroutine constraint_eqn_060(args)
      !+ad_name  constraint_eqn_060
      !+ad_summ  Equation for Central Solenoid s/c temperature margin lower limi
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for Central Solenoid s/c temperature margin lower limi
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# ftmargoh, tmargmin_cs
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftmargoh : input real :  f-value for central solenoid temperature margin
      !+ad_glos  tmargoh : input real :  Central solenoid temperature margin (K)
      !+ad_glos  tmargmin_cs : input real :  Minimum allowable temperature margin : CS (K)
      use constraint_variables, only: ftmargoh
      use pfcoil_variables, only: tmargoh
      use tfcoil_variables, only: tmargmin_cs
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - ftmargoh * tmargoh/tmargmin_cs
      args%con = tmargmin_cs
      args%err = tmargmin_cs - tmargoh
      args%symbol = '>'
      args%units = 'K'

   end subroutine constraint_eqn_060
   
   subroutine constraint_eqn_061(args)
      !+ad_name  constraint_eqn_061
      !+ad_summ  Equation for availability limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Equation for availability limit
      !+ad_desc    #=# cost
      !+ad_desc    #=#=# favail, avail_min
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  favail : input real : F-value for minimum availability 
      !+ad_glos  cfactr : input real : Total plant availability fraction
      !+ad_glos  avail_min : input real : Minimum availability
      use cost_variables, only: favail, cfactr, avail_min
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - favail * cfactr / avail_min
      args%con = avail_min * (1.0D0 - args%cc)
      args%err = cfactr * args%cc
      args%symbol = '>'
      args%units = ''

   end subroutine constraint_eqn_061

   subroutine constraint_eqn_062(args)
      !+ad_name  constraint_eqn_062
      !+ad_summ  Lower limit on taup/taueff the ratio of alpha particle to energy confinement times
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Lower limit on taup/taueff the ratio of alpha particle to energy confinement times
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# ftaulimit, taulimit
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftaulimit : input real : f-value for lower limit on taup/taueff the ratio of alpha particle to energy confinement
      !+ad_glos  taup : input real : alpha particle confinement time (s)
      !+ad_glos  taueff : input real : global thermal energy confinement time (sec)
      !+ad_glos  taulimit : input real : Lower limit on taup/taueff the ratio of alpha particle to energy confinement times
      use constraint_variables, only: ftaulimit, taulimit
      use physics_variables, only: taup, taueff
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - ftaulimit * (taup / taueff) / taulimit
      args%con = taulimit
      args%err = (taup / taueff) * args%cc
      args%symbol = '>'
      args%units = ''

   end subroutine constraint_eqn_062

   subroutine constraint_eqn_063(args)
      !+ad_name  constraint_eqn_063
      !+ad_summ  Upper limit on niterpump (vacuum_model = simple)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Upper limit on niterpump (vacuum_model = simple)
      !+ad_desc    #=# vacuum
      !+ad_desc    #=#=# fniterpump, tfno
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fniterpump : input real : f-value for constraint that number of pumps < tfno
      !+ad_glos  tfno : input real : number of TF coils (default = 50 for stellarators)
      !+ad_glos  niterpump : input real : number of high vacuum pumps (real number), each with the throughput
      use constraint_variables, only: fniterpump
      use tfcoil_variables, only: tfno
      use vacuum_variables, only: niterpump
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - fniterpump * tfno / niterpump
      args%con = tfno
      args%err = tfno * args%cc
      args%symbol = '<'
      args%units = ''

   end subroutine constraint_eqn_063

   subroutine constraint_eqn_064(args)
      !+ad_name  constraint_eqn_064
      !+ad_summ  Upper limit on Zeff
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Upper limit on Zeff
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fzeffmax, zeffmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fzeffmax : input real : f-value for maximum zeff
      !+ad_glos  zeffmax : input real : maximum value for Zeff
      !+ad_glos  zeff : input real : plasma effective charge
      use constraint_variables, only: fzeffmax, zeffmax
      use physics_variables, only: zeff
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - fzeffmax * (zeffmax/zeff)
      args%con = zeffmax
      args%err = zeffmax * args%cc
      args%symbol = '<'
      args%units = ''

   end subroutine constraint_eqn_064

   subroutine constraint_eqn_065(args)
      !+ad_name  constraint_eqn_065
      !+ad_summ  Limit TF dump time to calculated quench time (IDM: 2MBSE3)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Limit TF dump time to calculated quench time (IDM: 2MBSE3)
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# ftaucq, taucq
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  ftaucq : input real : f-value for calculated minimum TF quench time
      !+ad_glos  tdmptf : input real :  fast discharge time for TF coil in event of quench (s)
      !+ad_glos  taucq : input real :  allowable TF quench time (s)
      use constraint_variables, only: ftaucq
      use tfcoil_variables, only: tdmptf, taucq
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc =  1.0d0 - ftaucq * tdmptf / taucq
      args%con = taucq
      args%err = taucq - tdmptf
      args%symbol = '>'
      args%units = 's'

   end subroutine constraint_eqn_065

   subroutine constraint_eqn_066(args)
      !+ad_name  constraint_eqn_066
      !+ad_summ  Limit on rate of change of energy in poloidal field
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Limit on rate of change of energy in poloidal field
      !+ad_desc    #=# pfcoil
      !+ad_desc    #=#=# fpoloidalpower, maxpoloidalpower
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fpoloidalpower : input real : f-value for constraint on rate of change of energy in poloidal field
      !+ad_glos  maxpoloidalpower : input real : Maximum permitted absolute rate of change of stored energy in poloidal field (MW)
      !+ad_glos  peakpoloidalpower : input real : Peak absolute rate of change of stored energy in poloidal field (MW) (11/01/16)
      use constraint_variables, only: fpoloidalpower 
      use pf_power_variables, only: maxpoloidalpower, peakpoloidalpower
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0d0 - fpoloidalpower * maxpoloidalpower / peakpoloidalpower
      args%con = maxpoloidalpower
      args%err = maxpoloidalpower * args%cc
      args%symbol = '<'
      args%units = 'MW'

   end subroutine constraint_eqn_066

   subroutine constraint_eqn_067(args)
      !+ad_name  constraint_eqn_067
      !+ad_summ  Simple upper limit on radiation wall load
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Simple upper limit on radiation wall load 
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fradwall, maxradwallload
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fradwall : input real : f-value for upper limit on radiation wall load
      !+ad_glos  maxradwallload : input real : Maximum permitted radiation wall load (MW/m^2)
      !+ad_glos  peakradwallload : input real : Peak radiation wall load (MW/m^2)
      use constraint_variables, only: fradwall, maxradwallload, peakradwallload
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0d0 - fradwall * maxradwallload / peakradwallload
      args%con = maxradwallload
      args%err =  maxradwallload * args%cc
      args%symbol = '<'
      args%units = 'MW/m^2'

   end subroutine constraint_eqn_067

   subroutine constraint_eqn_068(args)
      !+ad_name  constraint_eqn_068
      !+ad_summ  New Psep scaling (PsepB/qAR)
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  New Psep scaling (PsepB/qAR)
      !+ad_desc  Issue #464
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fpsepbqar, psepbqarmax
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fpsepbqar : input real : f-value for upper limit on psepbqar, maximum Psep*Bt/qAR limit 
      !+ad_glos  psepbqarmax : input real : maximum permitted value of ratio of Psep*Bt/qAR (MWT/m)
      !+ad_glos  pdivt : input real : Power to conducted to the divertor region (MW)
      !+ad_glos  bt : input real : toroidal field on axis (T) (iteration variable 2)
      !+ad_glos  q95 : input real : safety factor q at 95% flux surface
      !+ad_glos  aspect : input real : aspect ratio (iteration variable 1)
      !+ad_glos  rmajor : input real : plasma major radius (m) (iteration variable 3)
      use constraint_variables, only: fpsepbqar, psepbqarmax
      use physics_variables, only: pdivt, bt, q95, aspect, rmajor
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0d0 - fpsepbqar * psepbqarmax / ((pdivt*bt)/(q95*aspect*rmajor))
      args%con = psepbqarmax
      args%err = (pdivt*bt)/(q95*aspect*rmajor) - psepbqarmax
      args%symbol = '<'
      args%units = 'MWT/m'

   end subroutine constraint_eqn_068

   subroutine constraint_eqn_069(args)
      !+ad_name  constraint_eqn_069
      !+ad_summ  Ensure separatrix power is less than value from Kallenbach divertor
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Ensure separatrix power is less than value from Kallenbach divertor
      !+ad_desc    #=# divertor_kallenbach
      !+ad_desc    #=#=# psep_kallenbach
      !+ad_desc  fpsep has been removed from the equation.
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  psep_kallenbach : input real : Power conducted through the separatrix, as calculated by the divertor model [W]
      !+ad_glos  pdivt : input real :  power to conducted to the divertor region (MW)
      use divertor_kallenbach_variables, only: psep_kallenbach
      use physics_variables, only: pdivt
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0d0 - (psep_kallenbach/1.0d6) / pdivt
      args%con = psep_kallenbach/1.0d6
      args%err = psep_kallenbach/1.0d6 * args%cc
      args%symbol = '='
      args%units = 'MW'

   end subroutine constraint_eqn_069
   
   subroutine constraint_eqn_070(args)
      !+ad_name  constraint_eqn_070
      !+ad_summ  Separatrix density consistency
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Separatrix density consistency
      !+ad_desc    #=# divertor_kallenbach
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  teomp : input real : Separatrix temperature calculated by the Kallenbach divertor model [eV]
      !+ad_glos  tesep : input real : Electron temperature at separatrix [keV]
      use divertor_kallenbach_variables, only: teomp
      use  physics_variables, only: tesep
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - teomp/(1000.0D0*tesep)
      args%con = teomp
      args%err = teomp* args%cc
      args%symbol = '='
      args%units = 'eV'

   end subroutine constraint_eqn_070

   subroutine constraint_eqn_071(args)
      !+ad_name  constraint_eqn_071
      !+ad_summ  Separatrix density consistency
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Separatrix density consistency
      !+ad_desc    #=# divertor_kallenbach
      !+ad_desc    #=#=# consistency
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  neomp : input real : Mean SOL density at OMP calculated by the Kallenbach divertor model [m-3]
      !+ad_glos  nesep : input real :  electron density at separatrix [m-3] (ipedestal=1,2, calculated if 3)
      !+ad_glos  neratio : input real : Ratio of mean SOL density at OMP to separatrix density at OMP (iteration variable 121)
      use divertor_kallenbach_variables, only: neomp, neratio
      use physics_variables, only: nesep
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - neomp/(nesep*neratio)
      args%con = neomp
      args%err = neomp* args%cc
      args%symbol = '='
      args%units = 'm-3'

   end subroutine constraint_eqn_071
		   
   subroutine constraint_eqn_072(args)
      !+ad_name  constraint_eqn_072
      !+ad_summ  Central Solenoid Tresca stress limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Central Solenoid Tresca stress limit
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# foh_stress, alstroh
      !+ad_desc  Reverse the sign so it works as an inequality constraint (args%cc > 0)
      !+ad_desc  This will have no effect if it is used as an equality constraint because it will be squared.
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  foh_stress : input real : f-value for Tresca stress limit in Central Solenoid
      !+ad_glos  alstroh : input real :  allowable hoop stress in Central Solenoid structural material (Pa)
      !+ad_glos  s_tresca_oh : input real : Tresca stress coils/central solenoid (Pa)
      use constraint_variables, only: foh_stress
      use pfcoil_variables, only: alstroh, s_tresca_oh
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0d0 - foh_stress * alstroh / s_tresca_oh
      args%con = alstroh
      args%err = alstroh - s_tresca_oh
      args%symbol = '<'
      args%units = 'Pa'

   end subroutine constraint_eqn_072
   
   subroutine constraint_eqn_073(args)
      !+ad_name  constraint_eqn_073
      !+ad_summ  Ensure separatrix power is greater than the L-H power + auxiliary power
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Ensure separatrix power is greater than the L-H power + auxiliary power
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fplhsep, pdivt
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fplhsep : input real : F-value for Psep >= Plh + Paux : for consistency of two values of separatrix power
      !+ad_glos  plhthresh : input real : L-H mode power threshold (MW) 
      !+ad_glos  pdivt : input real : power to be conducted to the divertor region (MW)
      !+ad_glos  pinjmw : inout real : total auxiliary injected power (MW)
      use physics_variables, only: fplhsep, plhthresh, pdivt
      use current_drive_variables, only: pinjmw
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0d0 - fplhsep * pdivt / (plhthresh+pinjmw)
      args%con = pdivt
      args%err = pdivt * args%cc
      args%symbol = '>'
      args%units = 'MW'

   end subroutine constraint_eqn_073

   subroutine constraint_eqn_074(args)
      !+ad_name  constraint_eqn_074
      !+ad_summ  Ensure TF coil quench temperature < tmax_croco ONLY used for croco HTS coil
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Ensure TF coil quench temperature < tmax_croco ONLY used for croco HTS coil
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# fcqt, tmax_croco
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fcqt : input real : f-value: TF coil quench temparature remains below tmax_croco
      !+ad_glos  croco_quench_temperature : input real : CroCo strand: Actual temp reached during a quench (K)
      !+ad_glos  tmax_croco : input real : CroCo strand: maximum permitted temp during a quench (K)
      use constraint_variables, only: fcqt
      use tfcoil_variables, only: croco_quench_temperature, tmax_croco
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0d0 - fcqt * tmax_croco / croco_quench_temperature 
      args%con = croco_quench_temperature
      args%err = croco_quench_temperature * args%cc
      args%symbol = '<'
      args%units = 'K'

   end subroutine constraint_eqn_074

   subroutine constraint_eqn_075(args)
      !+ad_name  constraint_eqn_075
      !+ad_summ  Ensure that TF coil current / copper area < Maximum value
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_args  Ensure that TF coil current / copper area < Maximum value
      !+ad_desc  ONLY used for croco HTS coil
      !+ad_desc    #=# physics
      !+ad_desc    #=#=# f_copperA_m2, copperA_m2_max
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  copperA_m2 : input real : 
      !+ad_glos  copperA_m2_max : input real : 
      !+ad_glos  f_copperA_m2 : input real : 
      use rebco_variables, only: copperA_m2, copperA_m2_max, f_copperA_m2
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0d0 - f_copperA_m2 * copperA_m2_max / copperA_m2
      args%con = copperA_m2
      args%err = copperA_m2 * args%cc
      args%symbol = '<'
      args%units = 'A/m2'

   end subroutine constraint_eqn_075

   subroutine constraint_eqn_076(args)
      !+ad_name  constraint_eqn_076
      !+ad_summ  Eich critical separatrix density model: Added for issue 558
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argsc residual error in physical units; output string; units string
      !+ad_desc  Eich critical separatrix density model
      !+ad_desc  Added for issue 558 with ref to http://iopscience.iop.org/article/10.1088/1741-4326/aaa340/pdf
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  alpha_crit : output real : critical ballooning parameter value
      !+ad_glos  nesep_crit : output real : critical electron density at separatrix [m-3]
      !+ad_glos  kappa : input real : plasma separatrix elongation (calculated if ishape > 0)
      !+ad_glos  triang : input real : plasma separatrix triangularity (calculated if ishape=1, 3 or 4)
      !+ad_glos  aspect : input real : aspect ratio (iteration variable 1)
      !+ad_glos  pdivt : input real : power to conducted to the divertor region (MW)
      !+ad_glos  dlimit(7) : input real array : density limit (/m3) as calculated using various models
      !+ad_glos  fnesep : input real : f-value for Eich critical separatrix density
      use physics_variables, only: alpha_crit, nesep_crit, kappa, triang, & 
                                   aspect, pdivt, dlimit, nesep
      use constraint_variables, only: fnesep
      implicit none
      type (constraint_args_type), intent(out) :: args

      alpha_crit = (kappa ** 1.2D0) * (1.0D0 + 1.5D0 * triang)
      nesep_crit = 5.9D0 * alpha_crit * (aspect ** (-2.0D0/7.0D0)) * &
                (((1.0D0 + (kappa ** 2.0D0)) / 2.0D0) ** (-6.0D0/7.0D0)) &
                * ((pdivt* 1.0D6) ** (-11.0D0/70.0D0)) * dlimit(7)
      args%cc = 1.0D0 - fnesep * nesep_crit/nesep
      args%con = nesep
      args%err = nesep * args%cc
      args%symbol = '<'
      args%units = 'm-3'

   end subroutine constraint_eqn_076
		   
   subroutine constraint_eqn_077(args)
      !+ad_name  constraint_eqn_077
      !+ad_summ  Equation for maximum TF current per turn upper limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; residual error in physical units; output string; units string
      !+ad_desc  Equation for maximum TF current per turn upper limit
      !+ad_desc    #=# tfcoil
      !+ad_desc    #=#=# fcpttf, cpttf, cpttf_max
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fcpttf : input : f-value for TF coil current per turn
      !+ad_glos  cpttf_max  : input : allowable TF coil current per turn [A/turn]
      !+ad_glos  cpttf  : input : TF coil current per turn [A/turn]
      use constraint_variables, only: fcpttf 
      use tfcoil_variables, only: cpttf_max, cpttf
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc = 1.0D0 - fcpttf * cpttf_max/cpttf
      args%con = cpttf_max
      args%err = cpttf_max * args%cc
      args%symbol = '<'
      args%units = 'A/turn'

   end subroutine constraint_eqn_077

   subroutine constraint_eqn_078(args)
      !+ad_name  constraint_eqn_078
      !+ad_summ  Equation for Reinke criterion, divertor impurity fraction lower limit
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; residual error in physical units; output string; units string
      !+ad_desc  Equation for Reinke criterion, divertor impurity fraction lower limit
      !+ad_desc    #=# divertor
      !+ad_desc    #=#=# freinke, fzactual, fzmin
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present;
      !+ad_desc  and con will be printed out only if present. Thesw conditions were missing.
      !+ad_glos  freinke : input : f-value for Reinke criterion (itv 147)
      !+ad_glos  fzmin : input : minimum impurity fraction from Reinke model
      !+ad_glos  fzactual : input : actual impurity fraction
      use constraint_variables, only: freinke
      use reinke_variables, only: fzactual, fzmin
      implicit none
      type (constraint_args_type), intent(out) :: args

      ! write(*,*) 'freinke, fzact, fzmin = ', freinke, ', ', fzactual, ', ', fzmin
      !            1.0,    0.0,   value
      args%cc = 1.0D0 - freinke *  fzactual/fzmin
      !The following two pre-existing lines are not understood:
      !KE note - cc is always 1, code never enters IF statement...
      args%con = fzmin * (1.0D0 - args%cc)
      args%err = fzmin * args%cc
      args%symbol = '>'
      args%units  = ''
      ! write(*,*) 'cc, con = ', args%cc, ', ', args%con
      ! write(*,*) 'freinke, fzactual, fzmin = ', freinke, ', ', fzactual, ', ', fzmin

   end subroutine constraint_eqn_078

   subroutine constraint_eqn_079(args)
      !+ad_name  constraint_eqn_079
      !+ad_summ  Equation for maximum CS field
      !+ad_type  Subroutine
      !+ad_auth  P B Lloyd, CCFE, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; residual error in physical units; output string; units string
      !+ad_desc  Equation for maximum CS field
      !+ad_desc    #=# pfcoil
      !+ad_desc    #=#=# fbmaxcs, bmaxoh, bmaxoh0, bmaxcs_lim
      !+ad_desc  and hence also optional here.
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fbmaxcs : input : F-value for CS mmax field (cons. 79, itvar 149)
      !+ad_glos  bmaxcs_lim : input : Central solenoid max field limit [T]
      !+ad_glos  bmaxoh0 : input : maximum field in central solenoid at beginning of pulse (T)
      !+ad_glos  bmaxoh : input real : maximum field in central solenoid at end of flat-top (EoF) (T)
      !+ad_gloc    (Note: original code has "bmaxoh/bmaxoh0 |  peak CS field [T]".)
      use pfcoil_variables, only: fbmaxcs, bmaxcs_lim, bmaxoh0, bmaxoh
      implicit none
      type (constraint_args_type), intent(out) :: args

      args%cc     = 1.0D0 - fbmaxcs * bmaxcs_lim/max(bmaxoh, bmaxoh0)
      args%con    = bmaxcs_lim
      args%err    = max(bmaxoh, bmaxoh0) * args%cc
      args%symbol = '<'
      args%units  = 'A/turn'

   end subroutine constraint_eqn_079

   subroutine constraint_eqn_080(args)
      !+ad_name  constraint_eqn_080
      !+ad_summ  Equation for pdivt lower limit
      !+ad_type  Subroutine
      !+ad_auth  J Morris, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; residual error in physical units; 
      !+ad_argc  output string; units string
      !+ad_desc  Lower limit pdivt
      !+ad_desc  #=# physics
      !+ad_desc  #=#=# fpdivlim, pdivt
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be assigned only if present.
      !+ad_glos  fpdivlim : input : F-value for lower limit on pdivt (cons. 80, itvar 153)
      !+ad_glos  pdivtlim : input : Minimum power crossing separatrix pdivt [MW]
      !+ad_glos  pdivt : input : Power crossing separatrix [MW]
      use physics_variables, only: fpdivlim, pdivt
      use constraint_variables, only : pdivtlim
      implicit none

      type (constraint_args_type), intent(out) :: args
      args%cc     = 1.0D0 - fpdivlim * pdivt / pdivtlim
      args%con    = pdivtlim
      args%err    = pdivt * args%cc
      args%symbol = '>'
      args%units  = 'MW'

   end subroutine constraint_eqn_080

   subroutine constraint_eqn_081(args)
      !+ad_name  constraint_eqn_081
      !+ad_summ  Make sure that the central density is larger that the pedestal one
      !+ad_type  Subroutine
      !+ad_auth  J Morris, Culham Science Centre
      !+ad_args  args : output structure : residual error; constraint value; 
      !+ad_argc  residual error in physical units; output string; units string
      !+ad_desc  Lower limit ne0 > neped
      !+ad_desc  !#=# physics
      !+ad_argc  !#=#=# ne0, neped
      !+ad_desc  Logic change during pre-factoring: err, symbol, units will be 
      !+ad_desc  assigned only if present.
      !+ad_glos  fne0  : input : F-value for constraint on ne0 > neped 
      !+ad_glos  ne0   : input : Central electron density [m-3]
      !+ad_glos  neped : input : Electron density at pedestal [m-3]
      use physics_variables, only: ne0, fne0, neped
      implicit none

      type (constraint_args_type), intent(out) :: args
      args%cc     = 1.0D0 - fne0 * ne0/neped 
      args%con    = fne0
      args%err    = fne0 * args%cc
      args%symbol = '>'
      args%units  = '/m3'

   end subroutine constraint_eqn_081
   
end module constraints

