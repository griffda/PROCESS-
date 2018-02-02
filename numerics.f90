! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module numerics

  !+ad_name  numerics
  !+ad_summ  Module containing callers to the main equation solvers
  !+ad_summ  HYBRD and VMCON
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  eqsolv
  !+ad_cont  optimiz
  !+ad_args  N/A
  !+ad_desc  This module contains the primary numerics variables and the
  !+ad_desc  calling routines for the two equation solvers in the code.
  !+ad_prob  None
  !+ad_call  global_variables
  !+ad_call  maths_library
  !+ad_hist  10/10/12 PJK Initial version of module
  !+ad_hist  15/10/12 PJK Modified comment lines, and added default array values
  !+ad_hist  06/11/12 PJK Modified comment lines
  !+ad_hist  11/12/12 PJK Comment typo fixed
  !+ad_hist  17/12/12 PJK New constraint equation 51 added
  !+ad_hist  17/12/12 PJK New figure of merit 14 added
  !+ad_hist  13/01/13 PJK Modified lablcc comment for eqn.47
  !+ad_hist  31/01/13 PJK Changed FACTOR comment
  !+ad_hist  11/04/13 PJK Listed explicitly the icc, ixc elements turned on
  !+ad_hisc               by default; lowered boundu(rnbeam) to 1.0 from 1.0D20
  !+ad_hist  04/06/13 PJK New constraint eqns 52-55 added; new iteration
  !+ad_hisc               variables 89-96 added
  !+ad_hist  11/06/13 PJK Changed eqn 41 description
  !+ad_hist  19/06/13 PJK Changed eqn.23, var.28 descriptions
  !+ad_hist  27/06/13 PJK Changed eqn.24 description
  !+ad_hist  25/09/13 PJK Changed eqn.20 description
  !+ad_hist  30/09/13 PJK New constraint eqn.56 added; new iteration variable
  !+ad_hisc               97 added
  !+ad_hist  18/11/13 PJK Changed boundl(25: fpnetel) to 0.001
  !+ad_hist  18/11/13 PJK Changed various boundl, boundu values
  !+ad_hist  28/11/13 PJK New iteration variable 98: li6enrich
  !+ad_hist  17/12/13 PJK Added 'not recommended' comment for ioptimz=0
  !+ad_hist  19/12/13 PJK Changed epsfcn description
  !+ad_hist  12/02/14 PJK New figure of merit 15 added
  !+ad_hist  13/02/14 PJK Expanded lablcc(56) to all 33 characters to
  !+ad_hisc               prevent gfortran error message
  !+ad_hist  26/02/14 PJK New constraint eqns 57,58 added; new iteration
  !+ad_hisc               variables and 100 (ftfthko) added
  !+ad_hist  27/02/14 PJK Modified nineqns comment
  !+ad_hist  05/03/14 PJK Clarified lablcc descriptions
  !+ad_hist  06/03/14 PJK Comment changes
  !+ad_hist  30/04/14 PJK New iteration variable 101 (prp)
  !+ad_hist  01/05/14 PJK Relabelled lablcc(28), lablmm(5)
  !+ad_hist  08/05/14 PJK Changed boundu(101) (prp upper limit)
  !+ad_hist  19/05/14 PJK Relabelled lablcc(15)
  !+ad_hist  19/05/14 PJK Reassigned lablcc(17), lablxc(28: fradpwr)
  !+ad_hist  02/06/14 PJK New iteration variable 102 (fimpvar)
  !+ad_hist  30/06/14 PJK Changed boundl(11), boundu(11)
  !+ad_hist  08/07/14 PJK Added verbose from global_variables
  !+ad_hist  31/07/14 PJK Labelled constraints 57 and 58 as obsolete,
  !+ad_hisc               also iteration variables 99, 100
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_hist  01/10/14 PJK Reassigned lablcc(15); new iteration variable 103
  !+ad_hist  02/10/14 PJK Reassigned lablcc(23); new iteration variable 104
  !+ad_hist  06/10/14 PJK New iteration variable 105; new constraint 59
  !+ad_hist  13/10/14 PJK Changed boundu(50: fiooic) from 0.5 to 1.0
  !+ad_hist  20/10/14 PJK OH to CS
  !+ad_hist  11/11/14 PJK New iteration variable 106; new constraint 60
  !+ad_hist  13/11/14 PJK lablxc(106) corrected
  !+ad_hist  25/11/14 PJK New iteration variable 107; new constraint 61
  !+ad_hist  11/12/14 PJK Corrected lablcc(61) - all strings must be defined with
  !+ad_hisc               the correct length as declared in the corresponding
  !+ad_hisc               character(len=...) statement, otherwise
  !+ad_hisc               compilation using gfortran fails
  !+ad_hist  27/02/15 JM  Changed default values for boundu(4) and boundu & l (103)
  !+ad_hist  27/05/15 MDK Added breeder_f as iteration variable 108
  !+ad_hist  29/05/15 MDK Figure of merit 2 (P_fus P_in-total) has been replaced by "not used"
  !+ad_hist  11/06/15 MDK Add active_constraints(ipeqns) : Boolean array showing which constraints are active.
  !+ad_hist  05/08/15 MDK Add ralpne as an iteration variable. Constraint 62 on taup/taueff the ratio of particle to energy confinement times
  !+ad_hist  26/08/15 MDK fniterpump as iteration variable 11, constraint 63 niterpump < tfno
  !+ad_hist  18/11/15  RK Added new FoM for minimising RMAJOR and maximising TBURN and
  !+ad_hisc               added constraint equation to limit Z_eff, including new iteration
  !+ad_hisc		  variable 112 (fzeffmax)
  !+ad_hist  26/11/15  RK New constraint equation for taucq
  !+ad_hist  10/12/15  RK Net electrical output added as FoM
  !+ad_hist  09/11/16 HL  Added new constraint 67, it. var. 116
  !+ad_hist  19/01/17 JM  Added new constraint 68, it. var. 117
  !+ad_hist  08/02/17 JM  Added new constraints 69,70, 71, it. var. 118, 119, 120 (Kallenbach)
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use global_variables
  use constants
  use maths_library

  implicit none

  public

  !+ad_vars  ipnvars FIX : total number of variables available for iteration
  integer, parameter :: ipnvars = 143
  !+ad_vars  ipeqns  FIX : number of constraint equations available
  integer, parameter :: ipeqns = 75
  !+ad_vars  ipnfoms FIX : number of available figures of merit
  integer, parameter :: ipnfoms = 17

  integer, parameter :: ipvlam  = ipeqns+2*ipnvars+1
  integer, parameter :: iptnt   = (ipeqns*(3*ipeqns+13))/2
  integer, parameter :: ipvp1   = ipnvars+1

  !+ad_vars  ioptimz /1/ : code operation switch:<UL>
  !+ad_varc           <LI> = -1 for no optimisation, HYBRD only;
  !+ad_varc           <LI> = 0  for HYBRD and VMCON (not recommended);
  !+ad_varc           <LI> = 1  for optimisation, VMCON only</UL>
  integer :: ioptimz = 1

  !+ad_vars  minmax /7/ : switch for figure-of-merit (see lablmm for descriptions)
  !+ad_varc               negative => maximise, positive => minimise
  integer :: minmax = 7
  !+ad_vars  lablmm(ipnfoms) : labels describing figures of merit:<UL>
  character(len=22), dimension(ipnfoms) :: lablmm = (/ &
       !+ad_varc  <LI> ( 1) major radius
       'major radius.         ', &
       !+ad_varc  <LI> ( 2) not used
       'not used.             ', &
       !+ad_varc  <LI> ( 3) neutron wall load
       'neutron wall load.    ', &
       !+ad_varc  <LI> ( 4) P_tf + P_pf
       'P_tf + P_pf.          ', &
       !+ad_varc  <LI> ( 5) fusion gain Q
       'fusion gain.          ', &
       !+ad_varc  <LI> ( 6) cost of electricity
       'cost of electricity.  ', &
       !+ad_varc  <LI> ( 7) capital cost (direct cost if ireactor=0,
       !+ad_varc                          constructed cost otherwise)
       'capital cost.         ', &
       !+ad_varc  <LI> ( 8) aspect ratio
       'aspect ratio.         ', &
       !+ad_varc  <LI> ( 9) divertor heat load
       'divertor heat load.   ', &
       !+ad_varc  <LI> (10) toroidal field
       'toroidal field.       ', &
       !+ad_varc  <LI> (11) total injected power
       'total injected power. ', &
       !+ad_varc  <LI> (12) hydrogen plant capital cost OBSOLETE
       'H plant capital cost. ', &
       !+ad_varc  <LI> (13) hydrogen production rate OBSOLETE
       'H production rate.    ', &
       !+ad_varc  <LI> (14) pulse length
       'pulse length.         ', &
       !+ad_varc  <LI> (15) plant availability factor (N.B. requires
       !+ad_varc            iavail=1 to be set)
       'plant availability.   ', &
       !+ad_varc  <LI> (16) linear combination of major radius (minimised) and pulse length (maximised)
       !+ad_varc              note: FoM should be minimised only!
       'min R0, max tau_burn. ', &
       !+ad_varc  <LI> (17) net electrical output </UL>
       'net electrical output.' /)

  !+ad_vars  ncalls : number of function calls during solution
  integer :: ncalls = 0
  !+ad_vars  neqns /14/ : number of equality constraints to be satisfied
  integer :: neqns = 14
  !+ad_vars  nfev1 : number of calls to FCNHYB (HYBRD function caller) made
  integer :: nfev1 = 0
  !+ad_vars  nfev2 : number of calls to FCNVMC1 (VMCON function caller) made
  integer :: nfev2 = 0
  !+ad_vars  nineqns /0/ : number of inequality constraints VMCON must satisfy
  !+ad_varc                (leave at zero for now)
  integer :: nineqns = 0
  !+ad_vars  nvar /16/ : number of iteration variables to use
  integer :: nvar = 16
  !+ad_vars  nviter : number of VMCON iterations performed
  integer :: nviter = 0

  !+ad_vars  icc(ipeqns) /0/ :
  !+ad_varc           array defining which constraint equations to activate
  !+ad_varc           (see lablcc for descriptions)

  ! TODO Check the dictionaries are created correctly.
  ! Issue #491 Default constraints removed.
  integer, dimension(ipeqns) :: icc = 0

  !+ad_vars  active_constraints(ipeqns) : Logical array showing which constraints are active
  logical, dimension(ipeqns) :: active_constraints = .false.

  !+ad_vars  lablcc(ipeqns) : labels describing constraint equations (corresponding itvs)<UL>
  character(len=33), dimension(ipeqns) :: lablcc = (/ &
       !+ad_varc  <LI> ( 1) Beta (consistency equation) (itv 5)
       'Beta consistency                 ', &
       !+ad_varc  <LI> ( 2) Global power balance (consistency equation) (itv 10,1,2,3,4,6,11)
       'Global power balance consistency ', &
       !+ad_varc  <LI> ( 3) Ion power balance DEPRECATED (itv 10,1,2,3,4,6,11)
       'Ion power balance                ', &
       !+ad_varc  <LI> ( 4) Electron power balance DEPRECATED (itv 10,1,2,3,4,6,11)
       'Electron power balance           ', &
       !+ad_varc  <LI> ( 5) Density upper limit (itv 9,1,2,3,4,5,6)
       'Density upper limit              ', &
       !+ad_varc  <LI> ( 6) (Epsilon x beta poloidal) upper limit (itv 8,1,2,3,4,6)
       '(Epsilon x beta-pol) upper limit ', &
       !+ad_varc  <LI> ( 7) Beam ion density (NBI) (consistency equation) (itv 7)
       'Beam ion density consistency     ', &
       !+ad_varc  <LI> ( 8) Neutron wall load upper limit (itv 14,1,2,3,4,6)
       'Neutron wall load upper limit    ', &
       !+ad_varc  <LI> ( 9) Fusion power upper limit (itv 26,1,2,3,4,6)
       'Fusion power upper limit         ', &
       !+ad_varc  <LI> (10) Toroidal field 1/R (consistency equation) (itv 12,1,2,3,13 )
       'Toroidal field 1/R consistency   ', &
       !+ad_varc  <LI> (11) Radial build (consistency equation) (itv 3,1,13,16,29,42,61)
       'Radial build consistency         ', &
       !+ad_varc  <LI> (12) Volt second lower limit (STEADY STATE) (itv 15,1,2,3)
       'Volt second lower limit          ', &
       !+ad_varc  <LI> (13) Burn time lower limit (PULSE) (itv 21,1,16,17,22,29,42,44,61)
       'Burn time lower limit            ', &
       !+ad_varc  <LI> (14) Neutral beam decay lengths to plasma centre (NBI) (consistency equation)
       !+ac_varc            (itv 19,1,2,3,6)
       'NBI decay lengths consistency    ', &
       !+ad_varc  <LI> (15) LH power threshold limit (itv 103)
       'L-H power threshold limit        ', &
       !+ad_varc  <LI> (16) Net electric power lower limit (itv 25,1,2,3)
       'Net electric power lower limit   ', &
       !+ad_varc  <LI> (17) Radiation fraction upper limit (itv 28)
       'Radiation fraction upper limit   ', &
       !+ad_varc  <LI> (18) Divertor heat load upper limit (itv 27)
       'Divertor heat load upper limit   ', &
       !+ad_varc  <LI> (19) MVA upper limit (itv 30)
       'MVA upper limit                  ', &
       !+ad_varc  <LI> (20) Neutral beam tangency radius upper limit (NBI) (itv 33,31,3,13)
       'Beam tangency radius upper limit ', &
       !+ad_varc  <LI> (21) Plasma minor radius lower limit (itv 32)
       'Plasma minor radius lower limit  ', &
       !+ad_varc  <LI> (22) Divertor collisionality upper limit (itv 34,43)
       'Divertor collisionality upper lim', &
       !+ad_varc  <LI> (23) Conducting shell to plasma minor radius ratio upper limit
       !+ad_varc            (itv 104,1,74)
       'Conducting shell radius upper lim', &
       !+ad_varc  <LI> (24) Beta upper limit (itv 36,1,2,3,4,6,18)
       'Beta upper limit                 ', &
       !+ad_varc  <LI> (25) Peak toroidal field upper limit (itv 35,3,13,29)
       'Peak toroidal field upper limit  ', &
       !+ad_varc  <LI> (26) Central solenoid EOF current density upper limit
       !+ad_varc            (itv 38,37,41,12)
       'CS coil EOF current density limit', &
       !+ad_varc  <LI> (27) Central solenoid BOP current density upper limit
       !+ad_varc            (itv 39,37,41,12)
       'CS coil BOP current density limit', &
       !+ad_varc  <LI> (28) Fusion gain Q lower limit (itv 45,47,40)
       'Fusion gain Q lower limit        ', &
       !+ad_varc  <LI> (29) Inboard radial build consistency (itv 3,1,13,16,29,42,61)
       'Inboard radial build consistency ', &
       !+ad_varc  <LI> (30) Injection power upper limit (itv 46,47,11)
       'Injection power upper limit      ', &
       !+ad_varc  <LI> (31) TF coil case stress upper limit (SCTF) (itv 48,56,57,58,59,60,24)
       'TF coil case stress upper limit  ', &
       !+ad_varc  <LI> (32) TF coil conduit stress upper limit (SCTF) (itv 49,56,57,58,59,60,24)
       'TF coil conduit stress upper lim ', &
       !+ad_varc  <LI> (33) I_op / I_critical (TF coil) (SCTF) (itv 50,56,57,58,59,60,24)
       'I_op / I_critical (TF coil)      ', &
       !+ad_varc  <LI> (34) Dump voltage upper limit (SCTF) (itv 51,52,56,57,58,59,60,24)
       'Dump voltage upper limit         ', &
       !+ad_varc  <LI> (35) J_winding pack/J_protection upper limit (SCTF) (itv 53,56,57,58,59,60,24)
       'J_winding pack/J_protection limit', &
       !+ad_varc  <LI> (36) TF coil temperature margin lower limit (SCTF) (itv 54,55,56,57,58,59,60,24)
       'TF coil temp. margin lower limit ', &
       !+ad_varc  <LI> (37) Current drive gamma upper limit (itv 40,47)
       'Current drive gamma limit        ', &
       !+ad_varc  <LI> (38) First wall coolant temperature rise upper limit (itv 62)
       '1st wall coolant temp rise limit ', &
       !+ad_varc  <LI> (39) First wall peak temperature upper limit (itv 63)
       'First wall peak temperature limit', &
       !+ad_varc  <LI> (40) Start-up injection power lower limit (PULSE) (itv 64)
       'Start-up inj. power lower limit  ', &
       !+ad_varc  <LI> (41) Plasma current ramp-up time lower limit (PULSE) (itv  66,65)
       'Plasma curr. ramp time lower lim ', &
       !+ad_varc  <LI> (42) Cycle time lower limit (PULSE) (itv 67,65,17)
       'Cycle time lower limit           ', &
       !+ad_varc  <LI> (43) Average centrepost temperature
       !+ad_varc            (TART) (consistency equation) (itv 69,70,13)
       'Average centrepost temperature   ', &
       !+ad_varc  <LI> (44) Peak centrepost temperature upper limit (TART) (itv 68,69,70)
       'Peak centrepost temp. upper limit', &
       !+ad_varc  <LI> (45) Edge safety factor lower limit (TART) (itv 71,1,2,3)
       'Edge safety factor lower limit   ', &
       !+ad_varc  <LI> (46) Ip/Irod upper limit (TART) (itv 72,2,60)
       'Ip/Irod upper limit              ', &
       !+ad_varc  <LI> (47) NOT USED
       'TF coil tor. thickness upper lim ', &
       !+ad_varc  <LI> (48) Poloidal beta upper limit (itv 79,2,3,18)
       'Poloidal beta upper limit        ', &
       !+ad_varc  <LI> (49) NOT USED
       'RFP reversal parameter < 0       ', &
       !+ad_varc  <LI> (50) NOT USED
       'IFE repetition rate upper limit  ', &
       !+ad_varc  <LI> (51) Startup volt-seconds consistency (PULSE) (itv 16,29,3,1)
       'Startup volt-seconds consistency ', &
       !+ad_varc  <LI> (52) Tritium breeding ratio lower limit (itv 89,90,91)
       'Tritium breeding ratio lower lim ', &
       !+ad_varc  <LI> (53) Neutron fluence on TF coil upper limit (itv 92,93,94)
       'Neutron fluence on TF coil limit ', &
       !+ad_varc  <LI> (54) Peak TF coil nuclear heating upper limit (itv 95,93,94)
       'Peak TF coil nucl. heating limit ', &
       !+ad_varc  <LI> (55) Vacuum vessel helium concentration upper limit iblanket =2 (itv 96,93,94)
       'Vessel helium concentration limit', &
       !+ad_varc  <LI> (56) Pseparatrix/Rmajor upper limit (itv 97,1,3,102)
       'Psep / R upper limit             ', &
       !+ad_varc  <LI> (57) NOT USED
       'TF coil leg tor width lower limit', &
       !+ad_varc  <LI> (58) NOT USED
       'TF coil leg rad width lower limit', &
       !+ad_varc  <LI> (59) Neutral beam shine-through fraction upper limit (NBI) (itv 105,6,19,4 )
       'NB shine-through frac upper limit', &
       !+ad_varc  <LI> (60) Central solenoid temperature margin lower limit (SCTF) (itv 106)
       'CS temperature margin lower limit', &
       !+ad_varc  <LI> (61) Minimum availability value (itv 107)
       'Minimum availability value       ',  &
       !+ad_varc  <LI> (62) taup/taueff the ratio of particle to energy confinement times (itv 110)
       'taup/taueff                      ', &
       !+ad_varc  <LI> (63) The number of ITER-like vacuum pumps niterpump < tfno (itv 111)
       'number of ITER-like vacuum pumps ',  &
       !+ad_varc  <LI> (64) Zeff less than or equal to zeffmax (itv 112)
       'Zeff limit                       ',  &
       !+ad_varc  <LI> (65) Dump time set by VV loads (itv 56, 113)
       'Dump time set by VV stress       ',   &
       !+ad_varc  <LI> (66) Limit on rate of change of energy in poloidal field
       !+ad_varc            (Use iteration variable 65(tohs), 115)
       'Rate of change of energy in field',   &
       !+ad_varc  <LI> (67) Simple Radiation Wall load limit (itv 116, 102, 4,6)
       'Upper Lim. on Radiation Wall load',   &
       !+ad_varc  <LI> (68) Psep * Bt / qAR upper limit (itv 117)
       'Upper Lim. on Psep * Bt / q A R  ',   &
       !+ad_varc  <LI> (69) ensure separatrix power = the value from Kallenbach divertor (itv 118)
       'pdivt < psep_kallenbach divertor ',   &
       !+ad_varc  <LI> (70) ensure that teomp = separatrix temperature in the pedestal profile,
       !+ad_varc            (itv 119 (tesep))
       'Separatrix temp consistency      ',   &
       !+ad_varc  <LI> (71) ensure that neomp = separatrix density (nesep) x neratio
       'Separatrix density consistency   ',    &
       !+ad_varc  <LI> (72) central solenoid Tresca stress limit (itv 123 foh_stress)
       'CS Tresca stress limit           ',    &
       !+ad_varc  <LI> (73) Psep >= Plh + Paux (itv 137 (fplhsep)
       'Psep >= Plh + Paux               ',   &
       !+ad_varc  <LI> (74) TFC quench < tmax_croco (itv 141 (fcqt))</UL>
       'TFC quench < tmax_croco          ',    &
       !+ad_varc  <LI> (75) TFC quench < tmax_croco (itv 143 f_copperA_m2) </UL>
       'TFC current/copper area < Max    '    &
       /)
       ! Please note: All strings between '...' above must be exactly 33 chars long
       ! Each line of code has a comma before the ampersand, except the last one.
       ! The last ad_varc line ends with the html tag "</UL>".

  ! Issue #495.  Remove default iteration variables
  !+ad_vars  ixc(ipnvars) /0/ :
  !+ad_varc               array defining which iteration variables to activate
  !+ad_varc               (see lablxc for descriptions)
  integer, dimension(ipnvars) :: ixc = 0

  !+ad_vars  lablxc(ipnvars) : labels describing iteration variables
  !+ad_varc                   (NEW:THERE ARE NO DEFAULTS):<UL>
  ! WARNING These labels are used as variable names by write_new_in_dat.py, and possibly
  ! other python utilities, so they cannot easily be changed.
  character(len=14), dimension(ipnvars) :: lablxc = (/ &
       !+ad_varc  <LI> ( 1) aspect
       'aspect        ', &
       !+ad_varc  <LI> ( 2) bt
       'bt            ', &
       !+ad_varc  <LI> ( 3) rmajor
       'rmajor        ', &
       !+ad_varc  <LI> ( 4) te
       'te            ', &
       !+ad_varc  <LI> ( 5) beta
       'beta          ', &
       !+ad_varc  <LI> ( 6) dene
       'dene          ', &
       !+ad_varc  <LI> ( 7) rnbeam
       'rnbeam        ', &
       !+ad_varc  <LI> ( 8) fbeta (f-value for equation 6)
       'fbeta         ', &
       !+ad_varc  <LI> ( 9) fdene (f-value for equation 5)
       'fdene         ', &
       !+ad_varc  <LI> (10) hfact
       'hfact         ', &
       !+ad_varc  <LI> (11) pheat
       'pheat         ', &
       !+ad_varc  <LI> (12) oacdcp
       'oacdcp        ', &
       !+ad_varc  <LI> (13) tfcth (NOT RECOMMENDED)
       'tfcth         ', &
       !+ad_varc  <LI> (14) fwalld (f-value for equation 8)
       'fwalld        ', &
       !+ad_varc  <LI> (15) fvs (f-value for equation 12)
       'fvs           ', &
       !+ad_varc  <LI> (16) ohcth
       'ohcth         ', &
       !+ad_varc  <LI> (17) tdwell
       'tdwell        ', &
       !+ad_varc  <LI> (18) q
       'q             ', &
       !+ad_varc  <LI> (19) enbeam
       'enbeam        ', &
       !+ad_varc  <LI> (20) tcpav
       'tcpav         ', &
       !+ad_varc  <LI> (21) ftburn (f-value for equation 13)
       'ftburn        ', &
       !+ad_varc  <LI> (22) tbrnmn
       'tbrnmn        ', &
       !+ad_varc  <LI> (23) fcoolcp
       'fcoolcp       ', &
       !+ad_varc  <LI> (24) cdtfleg
       'cdtfleg       ', &
       !+ad_varc  <LI> (25) fpnetel (f-value for equation 16)
       'fpnetel       ', &
       !+ad_varc  <LI> (26) ffuspow (f-value for equation 9)
       'ffuspow       ', &
       !+ad_varc  <LI> (27) fhldiv (f-value for equation 18)
       'fhldiv        ', &
       !+ad_varc  <LI> (28) fradpwr (f-value for equation 17), total radiation fraction
       'fradpwr       ', &
       !+ad_varc  <LI> (29) bore
       'bore          ', &
       !+ad_varc  <LI> (30) fmva (f-value for equation 19)
       'fmva          ', &
       !+ad_varc  <LI> (31) gapomin
       'gapomin       ', &
       !+ad_varc  <LI> (32) frminor (f-value for equation 21)
       'frminor       ', &
       !+ad_varc  <LI> (33) fportsz (f-value for equation 20)
       'fportsz       ', &
       !+ad_varc  <LI> (34) fdivcol (f-value for equation 22)
       'fdivcol       ', &
       !+ad_varc  <LI> (35) fpeakb (f-value for equation 25)
       'fpeakb        ', &
       !+ad_varc  <LI> (36) fbetatry (f-value for equation 24)
       'fbetatry      ', &
       !+ad_varc  <LI> (37) coheof
       'coheof        ', &
       !+ad_varc  <LI> (38) fjohc (f-value for equation 26)
       'fjohc         ', &
       !+ad_varc  <LI> (39) fjohc0 (f-value for equation 27)
       'fjohc0        ', &
       !+ad_varc  <LI> (40) fgamcd (f-value for equation 37)
       'fgamcd        ', &
       !+ad_varc  <LI> (41) fcohbop
       'fcohbop       ', &
       !+ad_varc  <LI> (42) gapoh
       'gapoh         ', &
       !+ad_varc  <LI> (43) cfe0
       'cfe0          ', &
       !+ad_varc  <LI> (44) fvsbrnni
       'fvsbrnni      ', &
       !+ad_varc  <LI> (45) fqval (f-value for equation 28)
       'fqval         ', &
       !+ad_varc  <LI> (46) fpinj (f-value for equation 30)
       'fpinj         ', &
       !+ad_varc  <LI> (47) feffcd
       'feffcd        ', &
       !+ad_varc  <LI> (48) fstrcase (f-value for equation 31)
       'fstrcase      ', &
       !+ad_varc  <LI> (49) fstrcond (f-value for equation 32)
       'fstrcond      ', &
       !+ad_varc  <LI> (50) fiooic (f-value for equation 33)
       'fiooic        ', &
       !+ad_varc  <LI> (51) fvdump (f-value for equation 34)
       'fvdump        ', &
       !+ad_varc  <LI> (52) vdalw
       'vdalw         ', &
       !+ad_varc  <LI> (53) fjprot (f-value for equation 35)
       'fjprot        ', &
       !+ad_varc  <LI> (54) ftmargtf (f-value for equation 36)
       'ftmargtf      ', &
       !+ad_varc  <LI> (55) obsolete
       'obsolete      ', &
       !+ad_varc  <LI> (56) tdmptf
       'tdmptf        ', &
       !+ad_varc  <LI> (57) thkcas
       'thkcas        ', &
       !+ad_varc  <LI> (58) thwcndut
       'thwcndut      ', &
       !+ad_varc  <LI> (59) fcutfsu
       'fcutfsu       ', &
       !+ad_varc  <LI> (60) cpttf
       'cpttf         ', &
       !+ad_varc  <LI> (61) gapds
       'gapds         ', &
       !+ad_varc  <LI> (62) fdtmp (f-value for equation 38)
       'fdtmp         ', &
       !+ad_varc  <LI> (63) ftpeak (f-value for equation 39)
       'ftpeak        ', &
       !+ad_varc  <LI> (64) fauxmn (f-value for equation 40)
       'fauxmn        ', &
       !+ad_varc  <LI> (65) tohs
       'tohs          ', &
       !+ad_varc  <LI> (66) ftohs (f-value for equation 41)
       'ftohs         ', &
       !+ad_varc  <LI> (67) ftcycl (f-value for equation 42)
       'ftcycl        ', &
       !+ad_varc  <LI> (68) fptemp (f-value for equation 44)
       'fptemp        ', &
       !+ad_varc  <LI> (69) rcool
       'rcool         ', &
       !+ad_varc  <LI> (70) vcool
       'vcool         ', &
       !+ad_varc  <LI> (71) fq (f-value for equation 45)
       'fq            ', &
       !+ad_varc  <LI> (72) fipir (f-value for equation 46)
       'fipir         ', &
       !+ad_varc  <LI> (73) scrapli
       'scrapli       ', &
       !+ad_varc  <LI> (74) scraplo
       'scraplo       ', &
       !+ad_varc  <LI> (75) tfootfi
       'tfootfi       ', &
       !+ad_varc  <LI> (76) NOT USED
       'frfptf        ', &
       !+ad_varc  <LI> (77) NOT USED
       'tftort        ', &
       !+ad_varc  <LI> (78) NOT USED
       'rfpth         ', &
       !+ad_varc  <LI> (79) fbetap (f-value for equation 48)
       'fbetap        ', &
       !+ad_varc  <LI> (80) NOT USED
       'frfpf         ', &
       !+ad_varc  <LI> (81) NOT USED
       'edrive        ', &
       !+ad_varc  <LI> (82) NOT USED
       'drveff        ', &
       !+ad_varc  <LI> (83) NOT USED
       'tgain         ', &
       !+ad_varc  <LI> (84) NOT USED
       'chrad         ', &
       !+ad_varc  <LI> (85) NOT USED
       'pdrive        ', &
       !+ad_varc  <LI> (86) NOT USED
       'frrmax        ', &
       !+ad_varc  <LI> (87) NOT USED
       'helecmw       ', &
       !+ad_varc  <LI> (88) NOT USED
       'hthermmw      ', &
       !+ad_varc  <LI> (89) ftbr (f-value for equation 52)
       'ftbr          ', &
       !+ad_varc  <LI> (90) blbuith
       'blbuith       ', &
       !+ad_varc  <LI> (91) blbuoth
       'blbuoth       ', &
       !+ad_varc  <LI> (92) fflutf (f-value for equation 53)
       'fflutf        ', &
       !+ad_varc  <LI> (93) shldith
       'shldith       ', &
       !+ad_varc  <LI> (94) shldoth
       'shldoth       ', &
       !+ad_varc  <LI> (95) fptfnuc (f-value for equation 54)
       'fptfnuc       ', &
       !+ad_varc  <LI> (96) fvvhe (f-value for equation 55)
       'fvvhe         ', &
       !+ad_varc  <LI> (97) fpsepr (f-value for equation 56)
       'fpsepr        ', &
       !+ad_varc  <LI> (98) li6enrich
       'li6enrich     ', &
       !+ad_varc  <LI> (99) NOT USED
       'ftftort       ', &
       !+ad_varc  <LI> (100) NOT USED
       'ftfthko       ', &
       !+ad_varc  <LI> (101) NOT USED
       'prp           ', &
       !+ad_varc  <LI> (102) fimpvar
       'fimpvar       ', &
       !+ad_varc  <LI> (103) flhthresh (f-value for equation 15)
       'flhthresh     ', &
       !+ad_varc  <LI> (104) fcwr (f-value for equation 23)
       'fcwr          ', &
       !+ad_varc  <LI> (105) fnbshinef (f-value for equation 59)
       'fnbshinef     ', &
       !+ad_varc  <LI> (106) ftmargoh (f-value for equation 60)
       'ftmargoh      ', &
       !+ad_varc  <LI> (107) favail (f-value for equation 61)
       'favail        ', &
       !+ad_varc  <LI> (108) breeder_f: Volume of Li4SiO4 / (Volume of Be12Ti + Li4SiO4)
       'breeder_f     ', &
       !+ad_varc  <LI> (109) ralpne: thermal alpha density / electron density
       'ralpne        ', &
       !+ad_varc  <LI> (110) ftaulimit: Lower limit on taup/taueff the ratio of alpha particle
       !+ad_varc       to energy confinement times (f-value for equation 62)
       'ftaulimit     ', &
       !+ad_varc  <LI> (111) fniterpump: f-value for constraint that
       !+ad_varc       number of vacuum pumps <  TF coils (f-value for equation 63)
       'fniterpump    ',  &
       !+ad_varc  <LI> (112) fzeffmax: f-value for max Zeff (f-value for equation 64)
       'fzeffmax      ',  &
       !+ad_varc  <LI> (113) ftaucq: f-value for minimum quench time (f-value for equation 65)
       'ftaucq        ',  &
       !+ad_varc  <LI> (114) fw_channel_length: Length of a single first wall channel
       'fw_channel_l  ',  &
       !+ad_varc  <LI> (115) fpoloidalpower: f-value for max rate of change of energy in poloidal field
       !+ad_varc             (f-value for equation 66)
       'fpoloidalpower',  &
       !+ad_varc  <LI> (116) fradwall: f-value for radiation wall load limit (eq. 67)
       'fradwall      ',  &
       !+ad_varc  <LI> (117) fpsepbqar: f-value for  Psep*Bt/qar upper limit (eq. 68)
       'fpsepbqar     ',  &
       !+ad_varc  <LI> (118) fpsep: f-value to ensure separatrix power is less than value from Kallenbach divertor
       !+ad_varc            (f-value for equation 69)
       'fpsep         ',  &
       !+ad_varc  <LI> (119) tesep:  separatrix temperature calculated by the Kallenbach divertor model
       'tesep         ',  &
       !+ad_varc  <LI> (120) ttarget: Plasma temperature adjacent to divertor sheath [eV]
       'ttarget       ',  &
       !+ad_varc  <LI> (121) neratio: ratio of mean SOL density at OMP to separatrix density at OMP
       'neratio       ',  &
       !+ad_varc  <LI> (122) oh_steel_frac : streel fraction of OH coil
       'oh_steel_frac ',  &
       !+ad_varc  <LI> (123) foh_stress : f-value for CS coil Tresca stress limit (f-value for eq. 72)
       'foh_stress    ',  &
       !+ad_varc  <LI> (124) qtargettotal : Power density on target including surface recombination [W/m2]
       'qtargettotal  ',  &
       !+ad_varc  <LI> (125) fimp(3) :  Beryllium density fraction relative to electron density
       'fimp(03)      ', &
       !+ad_varc  <LI> (126) fimp(4) :  Carbon density fraction relative to electron density
       'fimp(04)      ', &
       !+ad_varc  <LI> (127) fimp(5) :  Nitrogen fraction relative to electron density
       'fimp(05)      ', &
       !+ad_varc  <LI> (128) fimp(6) :  Oxygen density fraction relative to electron density
       'fimp(06)      ', &
       !+ad_varc  <LI> (129) fimp(7) :  Neon density fraction relative to electron density
       'fimp(07)      ', &
       !+ad_varc  <LI> (130) fimp(8) :  Silicon density fraction relative to electron density
       'fimp(08)      ', &
       !+ad_varc  <LI> (131) fimp(9) :  Argon density fraction relative to electron density
       'fimp(09)      ', &
       !+ad_varc  <LI> (132) fimp(10) :  Iron density fraction relative to electron density
       'fimp(10)      ', &
       !+ad_varc  <LI> (133) fimp(11) :  Nickel density fraction relative to electron density
       'fimp(11)      ', &
       !+ad_varc  <LI> (134) fimp(12) :  Krypton density fraction relative to electron density
       'fimp(12)      ', &
       !+ad_varc  <LI> (135) fimp(13) :  Xenon density fraction relative to electron density
       'fimp(13)      ', &
       !+ad_varc  <LI> (136) fimp(14) :  Tungsten density fraction relative to electron density
       'fimp(14)      ', &
       !+ad_varc  <LI> (137) fplhsep (f-value for equation 73)
       'fplhsep       ', &
       !+ad_varc  <LI> (138) rebco_thickness : thickness of REBCO layer in tape (m)
       'rebco_thicknes', &
       !+ad_varc  <LI> (139) copper_thick : thickness of copper layer in tape (m)
       'copper_thick  ', &
       !+ad_varc  <LI> (140) thkwp : radial thickness of TFC winding pack (m)
       'thkwp         ', &
       !+ad_varc  <LI> (141) fcqt : TF coil quench temperature < tmax_croco (f-value for equation 74)
       'fcqt          ', &
       !+ad_varc  <LI> (142) nesep : electron density at separatrix [m-3]
       'nesep         ', &
       !+ad_varc  <LI> (143) f_copperA_m2 : TF coil current / copper area < Maximum value (f-value for equation 75)</UL>
       'f_copperA_m2  ' &
       /)

  character(len=14), dimension(:), allocatable :: name_xc

  !+ad_vars  sqsumsq : sqrt of the sum of the square of the constraint residuals
  real(kind(1.0D0)) :: sqsumsq = 0.0D0
  !+ad_vars  epsfcn /1.0e-3/ : finite difference step length for HYBRD/VMCON derivatives
  real(kind(1.0D0)) :: epsfcn = 1.0D-3
  !+ad_vars  epsvmc /1.0e-6/ : error tolerance for VMCON
  real(kind(1.0D0)) :: epsvmc = 1.0D-6
  !+ad_vars  factor /0.1/ : used in HYBRD for first step size
  real(kind(1.0D0)) :: factor = 0.1D0
  !+ad_vars  ftol /1.0e-4/ : error tolerance for HYBRD
  real(kind(1.0D0)) :: ftol = 1.0D-4

  !+ad_vars  boundl(ipnvars) /../ : lower bounds used on ixc variables during
  !+ad_varc                         VMCON optimisation runs
  real(kind(1.0D0)), dimension(ipnvars) :: boundl = (/ &
       1.100D0, &  !  1
       0.010D0, &  !  2
       0.100D0, &  !  3
       5.000D0, &  !  4
       0.001D0, &  !  5
       1.00D19, &  !  6
       1.00D-6, &  !  7
       0.001D0, &  !  8
       0.001D0, &  !  9
       0.100D0, &  !  10
       1.00D-3, &  !  11
       1.000D5, &  !  12
       0.100D0, &  !  13   MDK change from 1 to 0.1 25/05/2017
       0.001D0, &  !  14
       0.001D0, &  !  15
       0.010D0, &  !  16
       0.100D0, &  !  17
       2.000D0, &  !  18
       1.000D0, &  !  19
       40.00D0, &  !  20
       0.001D0, &  !  21
       0.001D0, &  !  22
       0.100D0, &  !  23
       1.000D4, &  !  24
       0.001D0, &  !  25
       0.001D0, &  !  26
       0.001D0, &  !  27
       0.001D0, &  !  28
       0.100D0, &  !  29
       0.010D0, &  !  30
       0.001D0, &  !  31
       0.001D0, &  !  32
       0.001D0, &  !  33
       0.001D0, &  !  34
       0.001D0, &  !  35
       0.001D0, &  !  36
       1.000D5, &  !  37
       0.010D0, &  !  38
       0.001D0, &  !  39
       0.001D0, &  !  40
       0.001D0, &  !  41
       0.001D0, &  !  42
       1.00D-6, &  !  43
       0.001D0, &  !  44
       0.001D0, &  !  45
       0.001D0, &  !  46
       0.001D0, &  !  47
       0.001D0, &  !  48
       0.001D0, &  !  49
       0.001D0, &  !  50
       0.001D0, &  !  51
       0.001D0, &  !  52
       0.001D0, &  !  53
       0.001D0, &  !  54
       0.001D0, &  !  55
       0.100D0, &  !  56
       0.050D0, &  !  57
       0.001D0, &  !  58
       0.001D0, &  !  59
       0.001D0, &  !  60
       0.001D0, &  !  61
       0.001D0, &  !  62
       0.001D0, &  !  63
       0.001D0, &  !  64
       0.100D0, &  !  65
       0.001D0, &  !  66
       0.001D0, &  !  67
       0.001D0, &  !  68
       0.001D0, &  !  69
       1.000D0, &  !  70
       0.001D0, &  !  71
       0.001D0, &  !  72
       0.001D0, &  !  73
       0.001D0, &  !  74
       0.200D0, &  !  75
       0.001D0, &  !  76
       0.050D0, &  !  77
       0.010D0, &  !  78
       0.001D0, &  !  79
       0.001D0, &  !  80
       1.000D5, &  !  81
       0.010D0, &  !  82
       1.000D0, &  !  83
       0.100D0, &  !  84
       1.000D6, &  !  85
       0.001D0, &  !  86
       1.000D0, &  !  87
       1.000D0, &  !  88
       0.001D0, &  !  89
       0.001D0, &  !  90
       0.001D0, &  !  91
       0.001D0, &  !  92
       0.001D0, &  !  93
       0.001D0, &  !  94
       0.001D0, &  !  95
       0.001D0, &  !  96
       0.001D0, &  !  97
       10.00D0, &  !  98
       0.001D0, &  !  99
       0.001D0, &  !  100
       1.00D-6, &  !  101
       1.00D-6, &  !  102
       1.000D0, &  !  103
       0.001D0, &  !  104
       0.001D0, &  !  105
       0.001D0, &  !  106
       0.001D0, &  !  107
       0.060D0, &  !  108
       0.050D0, &  !  109
       0.001D0, &  !  110
       0.001D0, &  !  111
       0.001D0, &  !  112
       0.001D0, &  !  113
       0.001D0, &  !  114
       0.001D0, &  !  115
       0.001D0, &  !  116
       0.001D0, &  !  117
       0.001D0, &  !  118
       0.000D0, &  !  119
       1.000D0, &  !  120
       0.001D0, &  !  121
       0.001D0, &  !  122
       0.001D0, &  !  123
       0.001D0, &  !  124
       1.00D-8, &  !  125
       1.00D-8, &  !  126
       1.00D-8, &  !  127
       1.00D-8, &  !  128
       1.00D-8, &  !  129
       1.00D-8, &  !  130
       1.00D-8, &  !  131
       1.00D-8, &  !  132
       1.00D-8, &  !  133
       1.00D-8, &  !  134
       1.00D-8, &  !  135
       1.00D-8, &  !  136
       0.001D0, &  !  137
       0.01D-6, &  !  138
       1.00D-6, &  !  139
       0.001D0, &  !  140
       0.001D0, &  !  141
       1.00D17, &  !  142
       0.001D0  &  !  143
       /)

  !+ad_vars  boundu(ipnvars) /../ : upper bounds used on ixc variables during
  !+ad_varc                         VMCON optimisation runs
  real(kind(1.0D0)), dimension(ipnvars) :: boundu = (/ &
       10.00D0, &  !  1
       100.0D0, &  !  2
       50.00D0, &  !  3
       150.0D0, &  !  4
       1.000D0, &  !  5
       1.00D21, &  !  6
       1.000D0, &  !  7
       1.000D0, &  !  8
       1.000D0, &  !  9
       3.000D0, &  !  10
       1.000D3, &  !  11
       1.500D8, &  !  12
       5.000D0, &  !  13
       1.000D0, &  !  14
       1.000D0, &  !  15
       10.00D0, &  !  16
       1.000D8, &  !  17
       50.00D0, &  !  18
       1.000D6, &  !  19
       1.000D3, &  !  20
       1.000D0, &  !  21
       1.000D6, &  !  22
       0.500D0, &  !  23
       1.000D8, &  !  24
       1.000D0, &  !  25
       1.000D0, &  !  26
       1.000D0, &  !  27
       0.990D0, &  !  28 Issue #219
       10.00D0, &  !  29
       1.000D0, &  !  30
       1.000D1, &  !  31
       1.000D0, &  !  32
       1.000D0, &  !  33
       1.000D0, &  !  34
       1.000D0, &  !  35
       1.000D0, &  !  36
       1.000D8, &  !  37
       1.000D0, &  !  38
       1.000D0, &  !  39
       1.000D0, &  !  40
       1.000D0, &  !  41
       10.00D0, &  !  42
       3.00D-3, &  !  43
       1.000D0, &  !  44
       1.000D0, &  !  45
       1.000D0, &  !  46
       1.000D0, &  !  47
       1.000D0, &  !  48
       1.000D0, &  !  49
       1.000D0, &  !  50
       1.000D0, &  !  51
       1.000D6, &  !  52
       1.000D0, &  !  53
       1.000D0, &  !  54
       100.0D0, &  !  55
       100.0D0, &  !  56
       1.000D0, &  !  57
       0.100D0, &  !  58
       1.000D0, &  !  59
       4.000D4, &  !  60
       10.00D0, &  !  61
       1.000D0, &  !  62
       1.000D0, &  !  63
       1.000D0, &  !  64
       1.000D3, &  !  65
       1.000D0, &  !  66
       1.000D0, &  !  67
       1.000D0, &  !  68
       0.010D0, &  !  69
       1.000D2, &  !  70
       1.000D0, &  !  71
       1.000D0, &  !  72
       10.00D0, &  !  73
       10.00D0, &  !  74
       5.000D0, &  !  75
       1.000D0, &  !  76
       4.000D0, &  !  77
       1.800D0, &  !  78
       1.000D0, &  !  79
       1.000D0, &  !  80
       5.000D7, &  !  81
       1.000D0, &  !  82
       500.0D0, &  !  83
       20.00D0, &  !  84
       200.0D6, &  !  85
       1.000D0, &  !  86
       4.000D3, &  !  87
       4.000D3, &  !  88
       1.000D0, &  !  89
       2.000D0, &  !  90
       2.000D0, &  !  91
       1.000D0, &  !  92
       10.00D0, &  !  93
       10.00D0, &  !  94
       1.000D0, &  !  95
       1.000D0, &  !  96
       1.000D0, &  !  97
       100.0D0, &  !  98
       1.000D0, &  !  99
       1.000D0, &  !  100
       0.010D0, &  !  101
       0.010D0, &  !  102
       1.000D6, &  !  103
       1.000D0, &  !  104
       1.000D0, &  !  105
       1.000D0, &  !  106
       1.000D0, &  !  107
       1.000D0, &  !  108
       0.150D0, &  !  109
       1.000D0, &  !  110
       1.000D0, &  !  111
       1.000D0, &  !  112
       1.000D0, &  !  113
       1.000D3, &  !  114
       1.000D0, &  !  115
       1.000D0, &  !  116
       1.000D0, &  !  117
       1.000D0, &  !  118
       1.000D1, &  !  119
       1.000D4, &  !  120
       1.000D0, &  !  121
       0.950D0, &  !  122
       1.000D0, &  !  123
       1.000D2, &  !  124
       0.010D0, &  !  125
       0.010D0, &  !  126
       0.010D0, &  !  127
       0.010D0, &  !  128
       0.010D0, &  !  129
       0.010D0, &  !  130
       0.010D0, &  !  131
       0.010D0, &  !  132
       0.010D0, &  !  133
       0.010D0, &  !  134
       0.010D0, &  !  135
       0.010D0, &  !  136
       1.000D0, &  !  137
       100.0D-6,&  !  138
       1.00D-3, &  !  139
       2.000D0, &  !  140
       1.000D0, &  !  141
       1.00D20, &  !  142
       1.000D0  &  !  143
       /)

  real(kind(1.0D0)), dimension(ipnvars) :: bondl = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: bondu = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: rcm = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: resdl = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: scafc = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: scale = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: xcm = 0.0D0
  real(kind(1.0D0)), dimension(ipnvars) :: xcs = 0.0D0
  real(kind(1.0D0)), dimension(ipvlam)  :: vlam = 0.0D0


contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine eqsolv(fcnhyb,n,x,fvec,tol,epsfcn,factor,nprint,info, &
       wa,lwa,resdl,nfev)

    !+ad_name  eqsolv
    !+ad_summ  Find the non-optimising HYBRD solution to the problem
    !+ad_type  Subroutine
    !+ad_auth  Argonne National Laboratory. Minpack Project. March 1980.
    !+ad_auth  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  fcnhyb : external routine name : see below
    !+ad_args  n : input integer : number of functions and variables
    !+ad_args  x(n) : input/output real array : On input X must contain
    !+ad_argc    an initial estimate of the solution vector. On output X
    !+ad_argc    contains the final estimate of the solution vector.
    !+ad_args  fvec(n) : output real array : Functions evaluated at output X
    !+ad_args  tol : input real : Termination occurs when the algorithm
    !+ad_argc    estimates that the relative error between X and the solution
    !+ad_argc    is at most TOL.
    !+ad_args  epsfcn : input real : Used in determining a suitable
    !+ad_argc    step length for the forward-difference approximation
    !+ad_argc    (see <A HREF="hybrd.html">hybrd</A>)
    !+ad_args  factor : input real : Used in determining the initial step bound
    !+ad_argc    (see <A HREF="hybrd.html">hybrd</A>)
    !+ad_args  nprint : input integer : Number of iterations between print-outs
    !+ad_args  info : output integer : If the user has terminated execution,
    !+ad_argc    INFO is set to the (negative) value of IFLAG, see description below.
    !+ad_argc    Otherwise, INFO is set as follows:
    !+ad_argc    <PRE>
    !+ad_argc    INFO = 0   Improper input parameters.
    !+ad_argc    INFO = 1   Algorithm estimates that the relative error
    !+ad_argc               between X and the solution is at most TOL.
    !+ad_argc    INFO = 2   Number of calls to FCNHYB has reached or exceeded
    !+ad_argc               200*(N+1).
    !+ad_argc    INFO = 3   TOL is too small. No further improvement in
    !+ad_argc               the approximate solution X is possible.
    !+ad_argc    INFO = 4   Iteration is not making good progress.
    !+ad_argc    </PRE>
    !+ad_args  wa(lwa) : input/output real array : work array
    !+ad_args  lwa : input integer : work array size, not less than (N*(3*N+13))/2
    !+ad_args  resdl(n) : output real array : residuals
    !+ad_args  nfev : output integer : number of iterations performed
    !+ad_desc  Routine EQSOLV is the Argonne Minpack subroutine HYBRD1
    !+ad_desc  which has been modified by D.T. Blackfield FEDC/TRW.
    !+ad_desc  The routine is the same except some of the arguments are
    !+ad_desc  user supplied rather than 'hardwired'.
    !+ad_desc  <P>The purpose of EQSOLV is to find a zero of a system of
    !+ad_desc  N nonlinear functions in N variables by a modification
    !+ad_desc  of the Powell hybrid method. This is done by using the
    !+ad_desc  more general nonlinear equation solver <A HREF="hybrd.html">HYBRD</A>.
    !+ad_desc  The user must provide a subroutine which calculates the functions.
    !+ad_desc  The Jacobian is then calculated by a forward-difference
    !+ad_desc  approximation.
    !+ad_desc  <P>FCNHYB is the name of a user-supplied subroutine which
    !+ad_desc  calculates the functions. FCNHYB must be declared
    !+ad_desc  in an external statement in the user calling
    !+ad_desc  program, and should be written as follows:
    !+ad_desc  <PRE>
    !+ad_desc  subroutine fcnhyb(n,x,fvec,iflag)
    !+ad_desc  integer n,iflag
    !+ad_desc  double precision x(n),fvec(n)
    !+ad_desc  ----------
    !+ad_desc  calculate the functions at x and
    !+ad_desc  return this vector in fvec.
    !+ad_desc  ---------
    !+ad_desc  return
    !+ad_desc  end
    !+ad_desc  </PRE>
    !+ad_desc  The value of iflag should not be changed by FCNHYB unless
    !+ad_desc  the user wants to terminate execution of EQSOLV.
    !+ad_desc  In this case set IFLAG to a negative integer.
    !+ad_prob  None
    !+ad_call  fcnhyb
    !+ad_call  hybrd
    !+ad_hist  27/07/11 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    external :: fcnhyb
    integer, intent(in) :: n, nprint, lwa
    real(kind(1.0D0)), dimension(n), intent(inout) :: x
    real(kind(1.0D0)), dimension(n), intent(out) :: fvec, resdl
    real(kind(1.0D0)), dimension(lwa), intent(out) :: wa
    real(kind(1.0D0)), intent(in) :: tol, epsfcn, factor
    integer, intent(out) :: info, nfev

    !  Local variables

    integer :: n1,indx,lr,maxfev,ml,mode,mu
    real(kind(1.0D0)), parameter :: one = 1.0D0
    real(kind(1.0D0)), parameter :: zero = 0.0D0
    real(kind(1.0D0)) :: xtol

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    info = 0

    !  Check the input parameters for errors

    if ( (n == 0).or.(tol < zero).or.(lwa < ((n*(3*n + 13))/2) ) ) return

    !  Call HYBRD

    maxfev = 200*(n + 1)
    xtol = tol
    ml = n - 1
    mu = n - 1
    mode = 2

    wa(:) = one

    lr = (n*(n + 1))/2
    indx = 6*n + lr
    n1 = n

    !+**PJK 23/10/92 Warning produced by QA Fortran :
    !+**PJK 23/10/92 Arg 16 in call to HYBRD has wrong dimensions.
    !+**PJK 23/10/92 Code works at present, but beware of future
    !+**PJK 23/10/92 modifications.

    call hybrd(fcnhyb,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,wa(1),mode, &
         factor,nprint,info,nfev,wa(indx+1),n1,wa(6*n+1),lr, &
         wa(n+1),wa(2*n+1),wa(3*n+1),wa(4*n+1),wa(5*n+1), &
         resdl)

    if (info == 5) info = 4

  end subroutine eqsolv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine optimiz(fcnvmc1,fcnvmc2,ifail,f)

    !+ad_name  optimiz
    !+ad_summ  Calls the minimisation/maximisation routine VMCON
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  fcnvmc1 : external routine : objective function evaluator
    !+ad_args  fcnvmc2 : external routine : gradient objective function evaluator
    !+ad_args  ifail   : output integer : error flag
    !+ad_args  f       : output real    : value of objective function at the output point
    !+ad_desc  This routine calls the minimisation/maximisation routine VMCON,
    !+ad_desc  developed by Argonne National Laboratory.
    !+ad_desc  On exit, the (normalised) value of the variable being maximised
    !+ad_desc  or minimised (i.e. the figure of merit) is returned in argument
    !+ad_desc  <CODE>f</CODE>.
    !+ad_prob  None
    !+ad_call  fcnvmc1
    !+ad_call  fcnvmc2
    !+ad_call  vmcon
    !+ad_hist  02/10/96 PJK Initial upgraded version
    !+ad_hist  08/10/12 PJK Initial F90 version
    !+ad_hist  10/10/12 PJK Added arguments fcnvmc1, fcnvmc2
    !+ad_hist  26/02/14 PJK Added nviter argument to vmcon call
    !+ad_hist  27/02/14 PJK Corrected usage of m, meq in case of inequalities
    !+ad_hist  08/07/14 PJK Added attempt to fix problems if VMCON exits with ifail=5
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    external :: fcnvmc1, fcnvmc2
    integer, intent(out) :: ifail
    real(kind(1.0D0)), intent(out) :: f

    !  Local variables

    integer :: ii,lb,lcnorm,ldel,lh,liwa,lwa,m,meq,mode,n
    integer, parameter :: ippn1  = ipnvars+1
    integer, parameter :: ipldel = 7*ippn1
    integer, parameter :: iplh   = 2*ippn1
    integer, parameter :: ipvmu  = ipeqns+2*ipnvars+1
    integer, parameter :: ipliwa = 6*ippn1+ipeqns
    integer, dimension(ipliwa) :: iwa
    integer, dimension(ipnvars) :: ilower,iupper

    real(kind(1.0D0)), parameter :: zero = 0.0D0
    real(kind(1.0D0)), parameter :: bfactor = 2.0D0
    real(kind(1.0D0)) :: xtol
    real(kind(1.0D0)), dimension(ipnvars) :: bdelta,bndl,bndu,etav,fgrd, &
         gammv,glag,glaga,xa,xv
    real(kind(1.0D0)), dimension(ipeqns) :: cm,conf
    real(kind(1.0D0)), dimension(ippn1) :: bdl,bdu,gm
    real(kind(1.0D0)), dimension(ipvmu) :: vmu
    real(kind(1.0D0)), dimension(ipldel) :: delta
    real(kind(1.0D0)), dimension(iplh) :: wa
    real(kind(1.0D0)), dimension(ippn1,ipeqns) :: cnorm
    real(kind(1.0D0)), dimension(ippn1,ippn1) :: b
    real(kind(1.0D0)), dimension(iplh,iplh) :: h

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    n = nvar
    m = neqns + nineqns
    meq = neqns
    xtol = epsvmc
    mode = 0
    lb = ippn1
    lcnorm = ippn1
    ldel = ipldel
    lh = iplh
    lwa = iplh
    liwa = ipliwa

    do ii = 1,n
       ilower(ii) = 1
       iupper(ii) = 1
       bndl(ii) = bondl(ii)
       bndu(ii) = bondu(ii)
       xv(ii) = xcm(ii)
    end do

    call vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
         lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
         gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
         liwa,ilower,iupper,bndl,bndu)

    write(*,*) ""

    !  If VMCON has exited with error code 5 try another run using a multiple of
    !  the identity matrix as input for the Hessian b(n,n).
    !  Only do this if VMCON has not iterated (nviter=1).

    if ((ifail == 5).and.(nviter < 2)) then
       mode = 1
       b(:,:) = zero
       do ii = 1, n
          b(ii,ii) = bfactor
          xv(ii) = xcm(ii)      !  Re-initialise iteration values
       end do
       if (verbose == 1) then
          write(*,*) 'VMCON error code = 5.  Rerunning VMCON using a new'
          write(*,*) 'initial estimate of the second derivative matrix.'
       end if

       call vmcon(fcnvmc1,fcnvmc2,mode,n,m,meq,xv,f,fgrd,conf,cnorm, &
            lcnorm,b,lb,xtol,maxcal,ifail,nfev2,nviter,vlam,glag,vmu,cm,glaga, &
            gammv,etav,xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa, &
            liwa,ilower,iupper,bndl,bndu)
    end if

    do ii = 1,n
       xcm(ii) = xv(ii)
    end do

    do ii = 1,m
       rcm(ii) = conf(ii)
    end do

  end subroutine optimiz

end module numerics
