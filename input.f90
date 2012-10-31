!  $Id::                                                                $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!  Uncomment #define line below to perform unit testing
!  Compile using pre-processor, e.g. ifort -cpp input.f90
!#define unit_test

module process_input

  !+ad_name  process_input
  !+ad_summ  Module containing the routines that perform the actual reading
  !+ad_summ  and parsing of the input file
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  check_range_int
  !+ad_cont  check_range_real
  !+ad_cont  run_summary
  !+ad_cont  get_subscript
  !+ad_cont  get_substring
  !+ad_cont  get_substring_trim
  !+ad_cont  get_value_int
  !+ad_cont  get_value_real
  !+ad_cont  get_variable_name
  !+ad_cont  input
  !+ad_cont  parse_input_file
  !+ad_cont  parse_int_array
  !+ad_cont  parse_int_variable
  !+ad_cont  parse_real_array
  !+ad_cont  parse_real_variable
  !+ad_cont  parse_string_variable
  !+ad_cont  report_error
  !+ad_cont  string_to_int
  !+ad_cont  string_to_real
  !+ad_cont  upper_case
  !+ad_args  N/A
  !+ad_desc  This module provides a set of routines to read in data from the
  !+ad_desc  main PROCESS input file (IN.DAT). The format of the file is
  !+ad_desc  similar to the F90 NAMELIST structure, but with a few
  !+ad_desc  additional features:
  !+ad_desc  <OL>
  !+ad_desc  <P><LI>Comments can be read in that are copied to the standard
  !+ad_desc  output channel - these are lines with five (or more)
  !+ad_desc  consecutive '*' characters at the start.
  !+ad_desc  <P><LI>Other lines within the file can contain simple comments
  !+ad_desc  for the user - these are not copied to the standard output
  !+ad_desc  channel. They start with one to four '*' characters.
  !+ad_desc  </OL>
  !+ad_desc  <P>Character strings, integers and double precision values can
  !+ad_desc  be read in.
  !+ad_desc  <P>The following rules must be obeyed when writing an input
  !+ad_desc  file:
  !+ad_desc  <UL>
  !+ad_desc  <P><LI>Each variable must be on a separate line.
  !+ad_desc  <P><LI>Leading spaces are ignored.
  !+ad_desc  <P><LI>Variable names can be upper case, lower case, or a
  !+ad_desc  mixture of both.
  !+ad_desc  <P><LI>Spaces may not appear within a variable name or data
  !+ad_desc  value.
  !+ad_desc  <P><LI>Other spaces within a line, and trailing spaces, are
  !+ad_desc  ignored.
  !+ad_desc  <P><LI>Commas are not necessary between variables.
  !+ad_desc  <P><LI>Data can extend over more than one line.
  !+ad_desc  <P><LI>One-dimensional arrays can be explicitly subscripted, or
  !+ad_desc  unscripted, in which case the following element order is
  !+ad_desc  assumed: A(1), A(2), A(3), ...
  !+ad_desc  <P><LI>At present, multiple dimension arrays can only be
  !+ad_desc  handled without reference to explicit subscripts, in which case
  !+ad_desc  the following element order is assumed: B(1,1), B(2,1), B(3,1),
  !+ad_desc  etc. The use of the input file to specify multiple dimension
  !+ad_desc  array elements is prone to error.
  !+ad_desc  <P><LI>Unscripted array elements must be separated by commas.
  !+ad_desc  <P><LI>Blank lines are allowed anywhere in the input file.
  !+ad_desc  <P><LI>Lines starting with a * are assumed to be comments.
  !+ad_desc  <P><LI>Comment lines starting with five or more asterisks
  !+ad_desc  (i.e. *****) are reproduced verbatim in the output file. These
  !+ad_desc  should be used copiously to give a great deal of information
  !+ad_desc  about the run being performed, and should be updated before
  !+ad_desc  every single run of the code, as it is very easy to lose track
  !+ad_desc  of what is being attempted.
  !+ad_desc  </UL>
  !+ad_prob  Some routines still contain GOTOs...
  !+ad_call  build_variables
  !+ad_call  buildings_variables
  !+ad_call  cost_variables
  !+ad_call  current_drive_variables
  !+ad_call  divertor_variables
  !+ad_call  fwbs_variables
  !+ad_call  heat_transport_variables
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_variables
  !+ad_call  pf_power_variables
  !+ad_call  process_output
  !+ad_call  scan_module
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  vacuum_variables
  !+ad_call  ife.h90
  !+ad_call  ineq.h90
  !+ad_call  pulse.h90
  !+ad_call  rfp.h90
  !+ad_call  stella.h90
  !+ad_hist  20/01/95 PJK Initial version (PROCESS)
  !+ad_hist  05/01/04 PJK Initial F90 version (CENTORI)
  !+ad_hist  02/10/12 PJK Initial F90 version (PROCESS)
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  09/10/12 PJK Modified to use scan_module
  !+ad_hist  10/10/12 PJK Modified to use new numerics module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  17/10/12 PJK Added divertor_variables
  !+ad_hist  18/10/12 PJK Added fwbs_variables
  !+ad_hist  18/10/12 PJK Added pfcoil_variables
  !+ad_hist  18/10/12 PJK Added tfcoil_variables
  !+ad_hist  29/10/12 PJK Added vacuum_variables
  !+ad_hist  29/10/12 PJK Added pf_power_variables
  !+ad_hist  30/10/12 PJK Added heat_transport_variables
  !+ad_hist  30/10/12 PJK Added times_variables
  !+ad_hist  30/10/12 PJK Added buildings_variables
  !+ad_hist  30/10/12 PJK Added build_variables
  !+ad_hist  31/10/12 PJK Added cost_variables
  !+ad_stat  Okay
  !+ad_docs  A User's Guide to the PROCESS Systems Code, P. J. Knight,
  !+ad_docc    AEA Fusion Report AEA FUS 251, 1993
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use build_variables
  use buildings_variables
  use cost_variables
  use current_drive_variables
  use divertor_variables
  use fwbs_variables
  use heat_transport_variables
  use numerics
  use pfcoil_variables
  use physics_variables
  use pf_power_variables
  use process_output
  use scan_module
  use tfcoil_variables
  use times_variables
  use vacuum_variables

  implicit none

  include 'ife.h90'
  include 'ineq.h90'
  include 'pulse.h90'
  include 'rfp.h90'
  include 'stella.h90'

  private
  public :: input, run_summary, check_range_int, check_range_real
  integer, public, parameter :: nin = 10

#ifdef unit_test
  public :: parse_input_file
#endif

  character(len=200) :: line  !  current line of text from input file
  integer :: linelen, lineno  !  current line length, line number
  integer :: iptr             !  current position on line
  integer :: infile, outfile, report_changes, icode
  logical :: subscript_present

  integer           :: error_code
  character(len=78) :: error_routine, error_message

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine input

    !+ad_name  input
    !+ad_summ  Routine that calls the main input file parsing routines
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine provides the interface between the input file
    !+ad_desc  reading routines and the rest of PROCESS.
    !+ad_prob  None
    !+ad_call  parse_input_file
    !+ad_hist  03/10/12 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  A User's Guide to the PROCESS Systems Code, P. J. Knight,
    !+ad_docc    AEA Fusion Report AEA FUS 251, 1993
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: show_changes = 1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call parse_input_file(nin,nout,show_changes)

  end subroutine input

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine run_summary

    !+ad_name  run_summary
    !+ad_summ  Routine to print out the active iteration variables and
    !+ad_summ  constraint equations for the run
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine prints out the active iteration variables and
    !+ad_desc  constraint equations for the run.
    !+ad_desc  <P>This used to be an ENTRY point into routine INPUT.
    !+ad_prob  This routine ought to be elsewhere in the code.
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_hist  28/06/94 PJK Improved layout
    !+ad_hist  03/10/12 PJK Initial F90 version
    !+ad_hist  08/10/12 PJK Changed routine name from edit1 to run_summary
    !+ad_stat  Okay
    !+ad_docs  A User's Guide to the PROCESS Systems Code, P. J. Knight,
    !+ad_docc    AEA Fusion Report AEA FUS 251, 1993
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer :: ii

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifndef unit_test
     call oblnkl(nout)
     call ocmmnt(nout,'The following variables will be adjusted by')
     call ocmmnt(nout,'the code during the iteration process :')
     call oblnkl(nout)

     write(nout,10)
 10  format(t10,'ixc',t18,'label')

     call oblnkl(nout)

     write(nout,20) (ii,ixc(ii),lablxc(ixc(ii)),ii=1,nvar)
 20  format(t1,i3,t10,i3,t18,a8)

     call oblnkl(nout)
     call ocmmnt(nout, & 
          'The following constraint equations have been imposed,')
     if (ioptimz == -1) then
        call ocmmnt(nout, & 
             'but limits will not be enforced by the code :')
     else
        call ocmmnt(nout,'and will be enforced by the code :')
     end if
     call oblnkl(nout)

     write(nout,30)
 30  format(t10,'icc',t25,'label')

     call oblnkl(nout)

     write(nout,40) (ii,icc(ii),lablcc(icc(ii)), ii=1,neqns)
 40  format(t1,i3,t10,i3,t18,a34)
#endif

  end subroutine run_summary

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_input_file(in_file,out_file,show_changes)

    !+ad_name  parse_input_file
    !+ad_summ  Routine that parses the contents of the input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  in_file  : input integer : Fortran input unit identifier
    !+ad_args  out_file : input integer : Fortran output unit identifier
    !+ad_args  show_changes : input integer : switch to turn on (1) or off (0)
    !+ad_argc    reporting of changed values
    !+ad_desc  This routine reads the data from the PROCESS input file (IN.DAT),
    !+ad_desc  dealing with comments or blank lines correctly, and sets the
    !+ad_desc  value of any variables found in the file. Any changes
    !+ad_desc  from the default values may be reported if required.
    !+ad_desc  <P>Each possible variable in this block is dealt with
    !+ad_desc  individually. (To add additional input variables, simply copy
    !+ad_desc  and edit one of the similar existing examples.)
    !+ad_desc  The routine also does the extremely useful function of checking
    !+ad_desc  that the given value for a variable lies within a sensible
    !+ad_desc  predefined range, and stops the program if apparently
    !+ad_desc  nonsensical values are attempted.
    !+ad_prob  None
    !+ad_call  get_variable_name
    !+ad_call  parse_int_array
    !+ad_call  parse_int_variable
    !+ad_call  parse_real_array
    !+ad_call  parse_real_variable
    !+ad_call  parse_string_variable
    !+ad_call  report_error
    !+ad_hist  05/01/04 PJK Initial F90 version (CENTORI)
    !+ad_hist  03/10/12 PJK CENTORI version converted for PROCESS
    !+ad_hist  10/10/12 PJK Removed IVMS
    !+ad_stat  Okay
    !+ad_docs  A User's Guide to the PROCESS Systems Code, P. J. Knight,
    !+ad_docc    AEA Fusion Report AEA FUS 251, 1993
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: in_file, out_file, show_changes

    !  Local variables

    integer :: iost
    integer :: ioldvl,isub1,isub2,ival,varlen

    character(len=40) :: clabel, clbl,clbl2
    character(len=32) :: varnam
    real(kind(1.0D0)) :: oldval,rval

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Initialise module-wide variables

    infile = in_file
    outfile = out_file
    report_changes = show_changes

    icode = 0
    lineno = 0

    !  Main loop

    loop_over_lines: do

       subscript_present = .FALSE.

       read(infile,'(A)',iostat=iost) line

       !  On error or end, return
       if (iost /= 0) exit loop_over_lines

       lineno = lineno + 1

       line = adjustl(line)  !  rotate any leading blanks to the end
       linelen = len_trim(line)

20     continue

       !  Ignore blank lines

       if (line == ' ') cycle

       !  Ignore comments, unless they start with '*****',
       !  in which case print them.

       if (line(1:5) == '*****') write(outfile,*) line(1:76)
       if (line(1:1) == '*') cycle
       if (line(1:1) == '$') cycle  !  in case block delimiters are still present

       iptr = 1

       !  This must be an assignment line, so get the variable name

       call get_variable_name(varnam,varlen,isub1,isub2)
       if (isub1 /= 0) subscript_present = .TRUE.
       if (varlen == 0) then
          error_code = lineno
          error_routine = 'PARSE_INPUT_FILE'
          error_message = 'Error whilst reading variable name'
          call report_error
       end if

       !  Read the associated data

       variable: select case (varnam(1:varlen))

          !  Numerical solver settings

       case ('BOUNDL')
          call parse_real_array('BOUNDL', boundl, isub1, ipnvars, &
               'Iteration variable lower bound', icode)
       case ('BOUNDU')
          call parse_real_array('BOUNDU', boundu, isub1, ipnvars, &
               'Iteration variable upper bound', icode)
       case ('EPSFCN')
          call parse_real_variable('EPSFCN', epsfcn, 0.0D0, 1.0D0, &
               'HYBRID derivative step length')
       case ('EPSVMC')
          call parse_real_variable('EPSVMC', epsvmc, 0.0D0, 1.0D0, &
               'VMCON error tolerance')
       case ('FACTOR')
          call parse_real_variable('FACTOR', factor, 0.0D0, 10.0D0, &
               'VMCON initial step size')
       case ('FTOL')
          call parse_real_variable('FTOL', ftol, 0.0D0, 1.0D0, &
               'HYBRID tolerance')
       case ('ICC')
          call parse_int_array('ICC', icc, isub1, ipeqns, &
               'Constraint equation', icode)
       case ('IOPTIMZ')
          call parse_int_variable('IOPTIMZ', ioptimz, -1, 1, &
               'Switch for HYBRID/VMCON')
       case ('IXC')
          call parse_int_array('IXC', ixc, isub1, ipnvars, &
               'Iteration variable', icode)
       case ('MAXCAL')
          call parse_int_variable('MAXCAL', maxcal, 0, 10000, &
               'Max no of VMCON iterations')
       case ('MINMAX')
          call parse_int_variable('MINMAX', minmax, -ipnfoms, ipnfoms, &
               'Switch for figure of merit')
      case ('NEQNS')
          call parse_int_variable('NEQNS', neqns, 1, ipeqns, &
               'No of constraint equations')
       case ('NVAR')
          call parse_int_variable('NVAR', nvar, 1, ipnvars, &
               'No of independent variables')

          !  Physics settings

       case ('ALPHAJ')
          call parse_real_variable('ALPHAJ', alphaj, 0.0D0, 10.0D0, &
               'Current density profile factor')
       case ('ALPHAN')
          call parse_real_variable('ALPHAN', alphan, 0.0D0, 10.0D0, &
               'Density profile factor')
       case ('ALPHAT')
          call parse_real_variable('ALPHAT', alphat, 0.0D0, 10.0D0, &
               'Temperature profile factor')
       case ('ASPECT')
          call parse_real_variable('ASPECT', aspect, 1.001D0, 20.0D0, &
               'Aspect ratio')
       case ('BEAMFUS0')
          call parse_real_variable('BEAMFUS0', beamfus0, 0.01D0, 10.0D0, &
               'Beam-background fusion multiplier')
       case ('BETA')
          call parse_real_variable('BETA', beta, 0.0D0, 1.0D0, &
               'Plasma beta')
       case ('BETBM0')
          call parse_real_variable('BETBM0', betbm0, 0.0D0, 10.0D0, &
               'Leading coeff. for NB beta fraction')
       case ('BT')
          call parse_real_variable('BT', bt, 0.0D0, 20.0D0, &
               'Toroidal field on axis (T)')
       case ('CAREA')
          call parse_real_variable('CAREA', carea, 0.01D0, 10.0D0, &
               'Plasma surface area multiplier')
       case ('CFE0')
          call parse_real_variable('CFE0', cfe0, 0.0D0, 10.0D0, &
               'Additional Fe impurity fraction')
       case ('CSAWTH')
          call parse_real_variable('CSAWTH', csawth, 0.0D0, 10.0D0, &
               'Coefficient for sawteeth effects')
       case ('CVOL')
          call parse_real_variable('CVOL', cvol, 0.01D0, 10.0D0, &
               'Plasma volume multiplier')
       case ('DENE')
          call parse_real_variable('DENE', dene, 1.0D18, 1.0D22, &
               'Electron density (/m3)')
       case ('DIGN')
          call parse_real_variable('DIGN', dign, 0.01D0, 100.0D0, &
               'Guess for ignition margin')
       case ('DNBETA')
          call parse_real_variable('DNBETA', dnbeta, 0.3D0, 20.0D0, &
               'Troyon beta coefficient')
       case ('EPBETMAX')
          call parse_real_variable('EPBETMAX', epbetmax, 0.01D0, 10.0D0, &
               'Max epsilon*beta value')
       case ('FALPHA')
          call parse_real_variable('FALPHA', falpha, 0.0D0, 1.0D0, &
               'Fraction of alpha power deposited to plasma')
       case ('FBFE')
          call parse_real_variable('FBFE', fbfe, 0.0D0, 1.0D0, &
               'Fraction of Fe radn to Bremsstrahlung')
       case ('FDEUT')
          call parse_real_variable('FDEUT', fdeut, 0.0D0, 1.0D0, &
               'Deuterium fuel fraction in D-He3 reaction')
       case ('FFWAL')
          call parse_real_variable('FFWAL', ffwal, 0.0D0, 10.0D0, &
               'Wall load fiddle factor')
       case ('FHE3')
          call parse_real_variable('FHE3', fhe3, 0.0D0, 1.0D0, &
               'Helium-3 fuel fraction in D-He3 reaction')
       case ('FRADMIN')
          call parse_real_variable('FRADMIN', fradmin, 0.0D0, 1.0D0, &
               'F-value for minimum radiation power')
       case ('FTR')
          call parse_real_variable('FTR', ftr, 0.0D0, 1.0D0, &
               'Tritium fraction of DT ions')
       case ('FTRIT')
          call parse_real_variable('FTRIT', ftrit, 0.0D0, 1.0D0, &
               'Tritium fuel fraction in D-He3 reaction')
       case ('FVSBRNNI')
          call parse_real_variable('FVSBRNNI', fvsbrnni, 0.0D0, 1.0D0, &
               'Non-inductive volt-sec burn fraction')
       case ('GAMMA')
          call parse_real_variable('GAMMA', gamma, 0.1D0, 1.0D0, &
               'Coefficient for resistive V-s formula')
       case ('GTSCALE')
          call parse_int_variable('GTSCALE', gtscale, 0, 1, &
               'Flag to scale Troyon coefficient with R/a')
       case ('HFACT')
          call parse_real_variable('HFACT', hfact, 0.01D0, 10.0D0, &
               'Energy confinement time H factor')
       case ('IBSS')
          call parse_int_variable('IBSS', ibss, 1, 3, &
               'Switch for bootstrap scaling')
       case ('ICULBL')
          call parse_int_variable('ICULBL', iculbl, 0, 2, &
               'Switch for Troyon beta limit scaling')
       case ('ICULDL')
          call parse_int_variable('ICULDL', iculdl, 0, 1, &
               'Switch for density limit formulae')
       case ('ICURR')
          call parse_int_variable('ICURR', icurr, 1, 7, &
               'Switch for plasma current scaling')
       case ('IDENSL')
          call parse_int_variable('IDENSL', idensl, 1, 7, &
               'Switch for enforced density limit')
       case ('IDHE3')
          call parse_int_variable('IDHE3', idhe3, 0, 1, &
               'Switch for main fusion reaction (1 = DHe3)')
       case ('IDIVRT')
          call parse_int_variable('IDIVRT', idivrt, 2, 2, &
               'Switch for divertor configuration')
       case ('IFALPHAP')
          call parse_int_variable('IFALPHAP', ifalphap, 0, 1, &
               'Switch for fast alpha pressure fit')
       case ('IFISPACT')
          call parse_int_variable('IFISPACT', ifispact, 0, 0, &
               'Switch for neutronics calculations')
       case ('IGEOM')
          call parse_int_variable('IGEOM', igeom, 0, 1, &
               'Switch for plasma geometry calculation')
       case ('IGNITE')
          call parse_int_variable('IGNITE', ignite, 0, 1, &
               'Switch for ignited plasma assumption')
       case ('IINVQD')
          call parse_int_variable('IINVQD', iinvqd, 0, 1, &
               'Switch for inverse quadrature')
       case ('IITER')
          call parse_int_variable('IITER', iiter, 0, 1, &
               'Switch for ITER fusion power calcs')
       case ('IMPC')
          call parse_real_variable('IMPC', impc, 0.0D0, 10.0D0, &
               'Carbon impurity multiplier')
       case ('IMPFE')
          call parse_real_variable('IMPFE', impfe, 0.0D0, 2.0D0, &
               'Iron impurity multiplier')
       case ('IMPO')
          call parse_real_variable('IMPO', impo, 0.0D0, 10.0D0, &
               'Oxygen impurity multiplier')
       case ('IRES')
          call parse_int_variable('IRES', ires, 0, 1, &
               'Switch for neo-cl. plasma resistivity')
       case ('ISC')
          call parse_int_variable('ISC', isc, 1, ipnlaws, &
               'Switch for confinement scaling law')
       case ('ISCRP')
          call parse_int_variable('ISCRP', iscrp, 0, 1, &
               'Switch for scrapeoff width')
       case ('ISHAPE')
          call parse_int_variable('ISHAPE', ishape, 0, 1, &
               'Switch for plasma shape vs. aspect')
       case ('ITART')
          call parse_int_variable('ITART', itart, 0, 1, &
               'Switch for tight aspect ratio physics')
       case ('IWALLD')
          call parse_int_variable('IWALLD', iwalld, 1, 2, &
               'Switch for wall load calculation')
       case ('KAPPA')
          call parse_real_variable('KAPPA', kappa, 0.99D0, 5.0D0, &
               'Plasma separatrix elongation')
       case ('Q')
          call parse_real_variable('Q', q, 0.01D0, 50.0D0, &
               'Edge safety factor')
       case ('Q0')
          call parse_real_variable('Q0', q0, 0.01D0, 20.0D0, &
               'Safety factor on axis')
       case ('RALPNE')
          call parse_real_variable('RALPNE', ralpne, 0.001D0, 1.0D0, &
               'Thermal alpha density / electron density')
       case ('RECYLE')
          call parse_real_variable('RECYLE', recyle, 0.0D0, 1.0D0, &
               'Alpha recycle to main plasma')
       case ('RFPTH')
          call parse_real_variable('RFPTH', rfpth, 0.01D0, 1.8D0, &
               'RFP pinch parameter, theta')
       case ('RLI')
          call parse_real_variable('RLI', rli, 0.0D0, 10.0D0, &
               'Normalised inductivity')
       case ('RMAJOR')
          call parse_real_variable('RMAJOR', rmajor, 0.1D0, 30.0D0, &
               'Plasma major radius (m)')
       case ('RNBEAM')
          call parse_real_variable('RNBEAM', rnbeam, 0.0D0, 1.0D0, &
               'Hot beam density / electron density')
       case ('RTPTE')
          call parse_real_variable('RTPTE', rtpte, 0.1D0, 20.0D0, &
               'He particle / plas. energy confin. time')
       case ('SSYNC')
          call parse_real_variable('SSYNC', ssync, 0.0D0, 1.0D0, &
               'Synchrotron wall reflectivity factor')
       case ('TE')
          call parse_real_variable('TE', te, 2.0D0, 150.0D0, &
               'Electron temperature (keV)')
       case ('TI')
          call parse_real_variable('TI', ti, 5.0D0, 50.0D0, &
               'Ion temperature (keV)')
       case ('TRATIO')
          call parse_real_variable('TRATIO', tratio, 0.01D0, 2.0D0, &
               'Ion / electron temperature ratio')
       case ('TRIANG')
          call parse_real_variable('TRIANG', triang, 0.0D0, 1.0D0, &
               'Plasma separatrix triangularity')

          !  Inequality settings

       case ('AUXMIN')
          call parse_real_variable('AUXMIN', auxmin, 0.01D0, 100.0D0, &
               'Minimum auxiliary power (MW)')
       case ('BETPMX')
          call parse_real_variable('BETPMX', betpmx, 0.01D0, 2.0D0, &
               'Maximum poloidal beta')
       case ('BMXLIM')
          call parse_real_variable('BMXLIM', bmxlim, 0.1D0, 50.0D0, &
               'Maximum toroidal field (T)')
       case ('DTMPMX')
          call parse_real_variable('DTMPMX', dtmpmx, 1.0D0, 1000.0D0, &
               'Maximum temp rise in f.w. coolant (K)')
       case ('FAUXMN')
          call parse_real_variable('FAUXMN', fauxmn, 0.001D0, 10.0D0, &
               'F-value for minimum auxiliary power')
       case ('FBETA')
          call parse_real_variable('FBETA', fbeta, 0.001D0, 10.0D0, &
               'F-value for eps.betap beta limit')
       case ('FBETAP')
          call parse_real_variable('FBETAP', fbetap, 0.001D0, 10.0D0, &
               'F-value for poloidal beta limit')
       case ('FBETATRY')
          call parse_real_variable('FBETATRY', fbetatry, 0.001D0, 10.0D0, &
               'F-value for Troyon beta limit')
       case ('FDENE')
          call parse_real_variable('FDENE', fdene, 0.001D0, 10.0D0, &
               'F-value for density limit')
       case ('FDIVCOL')
          call parse_real_variable('FDIVCOL', fdivcol, 0.001D0, 10.0D0, &
               'F-value for divertor collisionality')
       case ('FDTMP')
          call parse_real_variable('FDTMP', fdtmp, 0.001D0, 10.0D0, &
               'F-value for first wall coolant temp rise')
       case ('FGAMCD')
          call parse_real_variable('FGAMCD', fgamcd, 0.001D0, 10.0D0, &
               'F-value for current drive gamma')
       case ('FIPIR')
          call parse_real_variable('FIPIR', fipir, 0.001D0, 10.0D0, &
               'F-value for Ip/Irod')
       case ('FJOHC0')
          call parse_real_variable('FJOHC0', fjohc0, 0.001D0, 10.0D0, &
               'F-value for OH coil current at BOP')
       case ('FJTFC')
          call parse_real_variable('FJTFC', fjtfc, 0.001D0, 10.0D0, &
               'F-value for TF coil current density')
       case ('FHLDIV')
          call parse_real_variable('FHLDIV', fhldiv, 0.001D0, 10.0D0, &
               'F-value for divertor heat load')
       case ('FFUSPOW')
          call parse_real_variable('FFUSPOW', ffuspow, 0.001D0, 10.0D0, &
               'F-value for maximum fusion power')
       case ('FIOOIC')
          call parse_real_variable('FIOOIC', fiooic, 0.001D0, 10.0D0, &
               'F-value for SCTF iop/icrit')
       case ('FJPROT')
          call parse_real_variable('FJPROT', fjprot, 0.001D0, 10.0D0, &
               'F-value for SCTF winding pack J')
       case ('FMVA')
          call parse_real_variable('FMVA', fmva, 0.001D0, 10.0D0, &
               'F-value for maximum MVA')
       case ('FPEAKB')
          call parse_real_variable('FPEAKB', fpeakb, 0.001D0, 10.0D0, &
               'F-value for max toroidal field')
       case ('FPINJ')
          call parse_real_variable('FPINJ', fpinj, 0.001D0, 10.0D0, &
               'F-value for injection power')
       case ('FPNETEL')
          call parse_real_variable('FPNETEL', fpnetel, 0.001D0, 10.0D0, &
               'F-value for net electric power')
       case ('FPORTSZ')
          call parse_real_variable('FPORTSZ', fportsz, 0.001D0, 10.0D0, &
               'F-value for port size')
       case ('FPTEMP')
          call parse_real_variable('FPTEMP', fptemp, 0.001D0, 10.0D0, &
               'F-value for peak centrepost temperature')
       case ('FQ')
          call parse_real_variable('FQ', fq, 0.001D0, 10.0D0, &
               'F-value for edge safety factor')
       case ('FQVAL')
          call parse_real_variable('FQVAL', fqval, 0.001D0, 10.0D0, &
               'F-value for energy multiplication Q')
       case ('FRFPF')
          call parse_real_variable('FRFPF', frfpf, 0.001D0, 10.0D0, &
               'F-value for RFP reversal parameter')
       case ('FRFPTF')
          call parse_real_variable('FRFPTF', frfptf, 0.001D0, 1.0D0, &
               'F-value for TF coil toroidal thickness')
       case ('FRMINOR')
          call parse_real_variable('FRMINOR', frminor, 0.001D0, 10.0D0, &
               'F-value for minor radius limit')
       case ('FSTRCASE')
          call parse_real_variable('FSTRCASE', fstrcase, 0.001D0, 10.0D0, &
               'F-value for TF coil case stress')
       case ('FSTRCOND')
          call parse_real_variable('FSTRCOND', fstrcond, 0.001D0, 10.0D0, &
               'F-value for TF coil conduit stress')
       case ('FTBURN')
          call parse_real_variable('FTBURN', ftburn, 0.001D0, 10.0D0, &
               'F-value for burn time limit')
       case ('FTCYCL')
          call parse_real_variable('FTCYCL', ftcycl, 0.001D0, 10.0D0, &
               'F-value for cycle time')
       case ('FTMARGTF')
          call parse_real_variable('FTMARGTF', ftmargtf, 0.001D0, 10.0D0, &
               'F-value for TF coil temp. margin')
       case ('FTOHS')
          call parse_real_variable('FTOHS', ftohs, 0.001D0, 10.0D0, &
               'F-value for OH coil swing time')
       case ('FTPEAK')
          call parse_real_variable('FTPEAK', ftpeak, 0.001D0, 10.0D0, &
               'F-value for peak first wall temperature')
       case ('FVDUMP')
          call parse_real_variable('FVDUMP', fvdump, 0.001D0, 10.0D0, &
               'F-value for dump voltage')
       case ('FVS')
          call parse_real_variable('FVS', fvs, 0.001D0, 10.0D0, &
               'F-value for startup V-s requirement')
       case ('FWALLD')
          call parse_real_variable('FWALLD', fwalld, 0.001D0, 10.0D0, &
               'F-value for wall load limit')
       case ('GAMMAX')
          call parse_real_variable('GAMMAX', gammax, 0.01D0, 10.0D0, &
               'Maximum current drive gamma (A/W-m2)')
       case ('MVALIM')
          call parse_real_variable('MVALIM', mvalim, 0.0D0, 1000.0D0, &
               'Maximum MVA limit')
       case ('PNETELIN')
          call parse_real_variable('PNETELIN', pnetelin, 1.0D0, 1.0D4, &
               'Required net electric power (MW)')
       case ('POWFMAX')
          call parse_real_variable('POWFMAX', powfmax, 1.0D0, 1.0D4, &
               'Maximum fusion power (MW)')
       case ('TBRNMN')
          call parse_real_variable('TBRNMN', tbrnmn, 1.0D-3, 1.0D6, &
               'Minimum burn time (s)')
       case ('TPKMAX')
          call parse_real_variable('TPKMAX', tpkmax, 100.0D0, 1.0D3, &
               'Maximum first wall peak temperature (C)')
       case ('WALALW')
          call parse_real_variable('WALALW', walalw, 0.001D0, 50.0D0, &
               'Allowable wall load (MW/m2)')

          !  Current drive settings

       case ('BEAMWD')
          call parse_real_variable('BEAMWD', beamwd, 0.001D0, 5.0D0, &
               'Beam width (m)')
       case ('BSCFMAX')
          call parse_real_variable('BSCFMAX', bscfmax, -0.999D0, 0.999D0, &
               '(-fixed)/maximum Bootstrap fraction')
       case ('CBOOT')
          call parse_real_variable('CBOOT', cboot, 0.0D0, 10.0D0, &
               'Bootstrap current fraction multiplier')
       case ('ECHPWR0')
          call parse_real_variable('ECHPWR0', echpwr0, 0.0D0, 1.0D8, &
               'Startup ECH power (W)')
       case ('ENBEAM')
          call parse_real_variable('ENBEAM', enbeam, 1.0D0, 20.0D3, &
               'Neutral beam energy (keV)')
       case ('ETALH')
          call parse_real_variable('ETALH', etalh, 0.0D0, 1.0D0, &
               'LH wall plug to plasma efficiency')
       case ('ETAECH')
          call parse_real_variable('ETAECH', etaech, 0.0D0, 1.0D0, &
               'ECH wall plug to injector efficiency')
       case ('ETANBI')
          call parse_real_variable('ETANBI', etanbi, 0.0D0, 1.0D0, &
               'NBI wall plug to injector efficiency')
       case ('ETAOF')
          call parse_real_variable('ETAOF', etaof, 0.0D0, 1.0D0, &
               'OFCD wall plug to injector efficiency')
       case ('FEFFCD')
          call parse_real_variable('FEFFCD', feffcd, 0.0D0, 20.0D0, &
               'Current drive efficiency fiddle factor')
       case ('FRBEAM')
          call parse_real_variable('FRBEAM', frbeam, 0.5D0, 2.0D0, &
               'R_tan / R_major for NBI')
       case ('FTRITBM')
          call parse_real_variable('FTRITBM', ftritbm, 0.0D0, 1.0D0, &
               'Tritium fraction of beam')
       case ('IEFRF')
          call parse_int_variable('IEFRF', iefrf, 1, 9, &
               'Switch for curr drive efficiency model')
       case ('IRFCD')
          call parse_int_variable('IRFCD', irfcd, 0, 1, &
               'Switch for current drive calculation')
       case ('PHEAT')
          call parse_real_variable('PHEAT', pheat, 0.0D0, 1.0D9, &
               'Heating power not used for C.D. (W)')
       case ('PINJALW')
          call parse_real_variable('PINJALW', pinjalw, 0.0D0, 1.0D3, &
               'Maximum allowed injection power (MW)')
       case ('TBEAMIN')
          call parse_real_variable('TBEAMIN', tbeamin, 0.0D0, 10.0D0, &
               'No of NB decay lengths to plas centre')

          !  Time settings

       case ('TBURN')
          call parse_real_variable('TBURN', tburn, 0.0D0, 1.0D7, &
               'Burn time (s)')
       case ('TDWELL')
          call parse_real_variable('TDWELL', tdwell, 0.0D0, 1.0D4, &
               'Time between burns (s)')
       case ('THEAT')
          call parse_real_variable('THEAT', theat, 0.0D0, 1.0D4, &
               'Heating time after current ramp (s)')
       case ('TOHS')
          call parse_real_variable('TOHS', tohs, 0.0D0, 1.0D4, &
               'OH coil swing time for current init (s)')
       case ('TOHSIN')
          call parse_real_variable('TOHSIN', tohsin, 0.0D0, 1.0D4, &
               'Switch for TOHS calculation')
       case ('TQNCH')
          call parse_real_variable('TQNCH', tqnch, 0.0D0, 1.0D4, &
               'PF coil shutdown time (s)')
       case ('TRAMP')
          call parse_real_variable('TRAMP', tramp, 0.0D0, 1.0D4, &
               'Initial charge time for PF coils (s)')

          !  Divertor settings

       case ('ANGINC')
          call parse_real_variable('ANGINC', anginc, 0.0D0, 1.5707D0, &
               'Field line ang of incid on dvrtr (rad)')
       case ('BPSOUT')
          call parse_real_variable('BPSOUT', bpsout, 0.0D0, 10.0D0, &
               'Ref B_p at outer divertor strike point')
       case ('C1DIV')
          call parse_real_variable('C1DIV', c1div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('C2DIV')
          call parse_real_variable('C2DIV', c2div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('C3DIV')
          call parse_real_variable('C3DIV', c3div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('C4DIV')
          call parse_real_variable('C4DIV', c4div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('C5DIV')
          call parse_real_variable('C5DIV', c5div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('C6DIV')
          call parse_real_variable('C6DIV', c6div, -100.0D0, 100.0D0, &
               'Divertor model fitting coefficient')
       case ('DELLD')
          call parse_real_variable('DELLD', delld, 0.1D0, 2.0D0, &
               'Coefficient for power distribution')
       case ('DIVCLFR')
          call parse_real_variable('DIVCLFR', divclfr, 0.0D0, 1.0D0, &
               'Divertor coolant fraction')
       case ('DIVDENS')
          call parse_real_variable('DIVDENS', divdens, 0.1D0, 1.0D5, &
               'Divertor structure density (kg/m3)')
       case ('DIVDUM')
          call parse_int_variable('DIVDUM', divdum, 0, 1, &
               'Switch for divertor Zeff value')
       case ('DIVPLT')
          call parse_real_variable('DIVPLT', divplt, 0.1D0, 1.0D0, &
               'Divertor plate thickness (m)')
       case ('FDFS')
          call parse_real_variable('FDFS', fdfs, 0.0D0, 20.0D0, &
               'Radial gradient ratio')
       case ('FDIVA')
          call parse_real_variable('FDIVA', fdiva, 0.1D0, 2.0D0, &
               'Divertor area fiddle factor')
       case ('FGAMP')
          call parse_real_variable('FGAMP', fgamp, -100.0D0, 100.0D0, &
               'Sheath potential factor')
       case ('FIFIFI')
          call parse_real_variable('FIFIFI', fififi, 1.0D-6, 1.0D0, &
               'Coefficient for gamdiv')
       case ('FRRP')
          call parse_real_variable('FRRP', frrp, 0.0D0, 1.0D0, &
               'Fraction of radiated power to plate')
       case ('HLDIVLIM')
          call parse_real_variable('HLDIVLIM', hldivlim, 0.1D0, 20.0D0, &
               'Divertor heat load limit (MW/m2)')
       case ('KSIC')
          call parse_real_variable('KSIC', ksic, 0.0D0, 2.0D0, &
               'Divertor power fraction thingy')
       case ('OMEGAN')
          call parse_real_variable('OMEGAN', omegan, 0.1D0, 10.0D0, &
               'Pressure ratio (nT)_p / (nT)_s')
       case ('PLSEPO')
          call parse_real_variable('PLSEPO', plsepo, 0.1D0, 10.0D0, &
               'Poloidal length, x to outer strike point')
       case ('PRN1')
          call parse_real_variable('PRN1', prn1, 0.0D0, 1.0D0, &
               'n_scrapeoff / n_average plasma')
       case ('RLENMAX')
          call parse_real_variable('RLENMAX', rlenmax, 0.0D0, 1.0D0, &
               'Maximum value for length ratio')
       case ('XPARAIN')
          call parse_real_variable('XPARAIN', xparain, 0.01D0, 1.0D4, &
               'Parallel heat transport coeff (m2/s)')
       case ('XPERTIN')
          call parse_real_variable('XPERTIN', xpertin, 0.0D0, 10.0D0, &
               'Perpendicular heat trans coeff (m2/s)')
       case ('ZEFFDIV')
          call parse_real_variable('ZEFFDIV', zeffdiv, 0.01D0, 100.0D0, &
               'Zeff in the divertor region (if divdum.ne.0)')

          !  Radial / vertical build settings

       case ('APLASMIN')
          call parse_real_variable('APLASMIN', aplasmin, 0.01D0, 10.0D0, &
               'Minimum minor radius (m)')
       case ('BLNKITH')
          call parse_real_variable('BLNKITH', blnkith, 0.0D0, 10.0D0, &
               'Inboard blanket thickness (m)')
       case ('BLNKOTH')
          call parse_real_variable('BLNKOTH', blnkoth, 0.0D0, 10.0D0, &
               'Outboard blanket thickness (m)')
       case ('BCYLTH')
          call parse_real_variable('BCYLTH', bcylth, 0.0D0, 10.0D0, &
               'Bucking cylinder thickness (m)')
       case ('BORE')
          call parse_real_variable('BORE', bore, 0.0D0, 20.0D0, &
               'Machine bore (m)')
       case ('DDWEX')
          call parse_real_variable('DDWEX', ddwex, 0.0D0, 10.0D0, &
               'External dewar thickness (m)')
       case ('DDWI')
          call parse_real_variable('DDWI', ddwi, 0.0D0, 10.0D0, &
               'Dewar thickness (m)')
       case ('FMSBC')
          call parse_real_variable('FMSBC', fmsbc, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in buck cyl')
       case ('FMSBL')
          call parse_real_variable('FMSBL', fmsbl, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in blanket')
       case ('FMSDWE')
          call parse_real_variable('FMSDWE', fmsdwe, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in external dewar')
       case ('FMSDWI')
          call parse_real_variable('FMSDWI', fmsdwi, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in dewar')
       case ('FMSFW')
          call parse_real_variable('FMSFW', fmsfw, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in first wall')
       case ('FMSOH')
          call parse_real_variable('FMSOH', fmsoh, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in OH coil')
       case ('FMSSH')
          call parse_real_variable('FMSSH', fmssh, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in shield')
       case ('FMSTF')
          call parse_real_variable('FMSTF', fmstf, 0.0D0, 1.0D0, &
               'Martensitic frac of steel in TF coil')
       case ('FWITH')
          call parse_real_variable('FWITH', fwith, 0.0D0, 10.0D0, &
               'Inboard first wall thickness (m)')
       case ('FWOTH')
          call parse_real_variable('FWOTH', fwoth, 0.0D0, 10.0D0, &
               'Outboard first wall thickness (m)')
       case ('GAPOH')
          call parse_real_variable('GAPOH', gapoh, 0.0D0, 10.0D0, &
               'Gap between OHC and bucking cylinder (m)')
       case ('GAPDS')
          call parse_real_variable('GAPDS', gapds, 0.0D0, 10.0D0, &
               'Gap between dewar and shield (m)')
       case ('GAPOMIN')
          call parse_real_variable('GAPOMIN', gapomin, 0.0D0, 10.0D0, &
               'Min gap between outer shield & TFC (m)')
       case ('IOHCL')
          call parse_int_variable('IOHCL', iohcl, 0, 1, &
               'Switch for existence of OH coil')
       case ('OHCTH')
          call parse_real_variable('OHCTH', ohcth, 0.0D0, 10.0D0, &
               'OH coil thickness (m)')
       case ('RINBOARD')
          call parse_real_variable('RINBOARD', rinboard, 0.1D0, 10.0D0, &
               'Plasma inboard radius (m)')
       case ('SCRAPLI')
          call parse_real_variable('SCRAPLI', scrapli, 0.0D0, 10.0D0, &
               'Inboard scrapeoff length (m)')
       case ('SCRAPLO')
          call parse_real_variable('SCRAPLO', scraplo, 0.0D0, 10.0D0, &
               'Outboard scrapeoff length (m)')
       case ('SHLDITH')
          call parse_real_variable('SHLDITH', shldith, 0.0D0, 10.0D0, &
               'Inboard shield thickness (m)')
       case ('SHLDOTH')
          call parse_real_variable('SHLDOTH', shldoth, 0.0D0, 10.0D0, &
               'Outboard shield thickness (m)')
       case ('SHLDTTH')
          call parse_real_variable('SHLDTTH', shldtth, 0.0D0, 10.0D0, &
               'Top shield thickness (m)')
       case ('TFCTH')
          call parse_real_variable('TFCTH', tfcth, 0.0D0, 10.0D0, &
               'TF coil thickness (m)')
       case ('TFOOTFI')
          call parse_real_variable('TFOOTFI', tfootfi, 0.2D0, 5.0D0, &
               'TFC outboard/inboard leg thickness')
       case ('VGAPTF')
          call parse_real_variable('VGAPTF', vgaptf, 0.0D0, 10.0D0, &
               'Vert gap between x-pnt and divertor (m)')
       case ('VGAP2')
          call parse_real_variable('VGAP2', vgap2, 0.0D0, 10.0D0, &
               'Vert gap between TF coil and shield (m)')

          !  TF coil settings

       case ('ASPCSTF')
          call parse_real_variable('ASPCSTF', aspcstf, 0.1D0, 10.0D0, &
               'TF conductor cable aspect ratio')
       case ('BCRITSC')
          call parse_real_variable('BCRITSC', bcritsc, 10.0D0, 50.0D0, &
               'Critical field for superconductor')
       case ('CASFACT')
          call parse_real_variable('CASFACT', casfact, 0.1D0, 10.0D0, &
               'TF coil case thickness factor')
       case ('CASTHI')
          call parse_real_variable('CASTHI', casthi, 0.0D0, 1.0D0, &
               'TF coil case inner thickness (m)')
       case ('CASTHS')
          call parse_real_variable('CASTHS', casths, 0.0D0, 1.0D0, &
               'TF coil case sidewall thickness (m)')
       case ('CDTFLEG')
          call parse_real_variable('CDTFLEG', cdtfleg, 0.1D0, 1.0D8, &
               'TF leg overall current density (A/m2)')
       case ('CPTTF')
          call parse_real_variable('CPTTF', cpttf, 1.0D0, 1.0D6, &
               'TF coil leg current per turn (A)')
       case ('CSUTF')
          call parse_real_variable('CSUTF', csutf, 1.0D6, 1.0D11, &
               'Ultimate strength of TF coil case (Pa)')
       case ('CSYTF')
          call parse_real_variable('CSYTF', csytf, 1.0D6, 1.0D11, &
               'Yield strength of TF coil case (Pa)')
       case ('DCASE')
          call parse_real_variable('DCASE', dcase, 1.0D3, 1.0D5, &
               'Density of TF coil case (kg/m3)')
       case ('DCOND')
          call parse_real_array('DCOND', dcond, isub1, 5, &
               'TF coil superconductor density (kg/m3)', icode)
       case ('DCOPPER')
          call parse_real_variable('DCOPPER', dcopper, 8.0D3, 1.0D4, &
               'Density of copper (kg/m3)')
       case ('DRTOP')
          call parse_real_variable('DRTOP', drtop, -1.5D0, 1.5D0, &
               'ST CP top radius adjust (m)')
       case ('DZTOP')
          call parse_real_variable('DZTOP', dztop, -0.5D0, 0.5D0, &
               'ST CP taper height adjust (m)')
       case ('ETAPUMP')
          call parse_real_variable('ETAPUMP', etapump, 0.0D0, 1.0D0, &
               'Efficiency of c/p coolant pump')
       case ('EYSTL')
          call parse_real_variable('EYSTL', eystl, 1.0D8, 1.0D13, &
               'Steel case Youngs Modulus (Pa)')
       case ('EYINS')
          call parse_real_variable('EYINS', eyins, 1.0D8, 1.0D13, &
               'Insulator Youngs Modulus (Pa)')
       case ('EYWP')
          call parse_real_variable('EYWP', eywp, 1.0D8, 1.0D13, &
               'Winding pack Youngs Modulus (Pa)')
       case ('FARC4TF')
          call parse_real_variable('FARC4TF', farc4tf, 0.0D0, 1.0D0, &
               'TF coil shape parameter')
       case ('FCOOLCP')
          call parse_real_variable('FCOOLCP', fcoolcp, 0.0D0, 1.0D0, &
               'Coolant fraction of TF inner leg')
       case ('FCUTFSU')
          call parse_real_variable('FCUTFSU', fcutfsu, 0.0D0, 1.0D0, &
               'Cu fraction of SCTF cable conductor')
       case ('FRHOCP')
          call parse_real_variable('FRHOCP', frhocp, 0.01D0, 5.0D0, &
               'TART c/p resistivity enhancement factor')
       case ('ISUMATTF')
          call parse_int_variable('ISUMATTF', isumattf, 1, 5, &
               'TF coil superconductor material')
       case ('ITFMOD')
          call parse_int_variable('ITFMOD', itfmod, 0, 1, &
               'Switch for TF magnet model')
       case ('ITFSUP')
          call parse_int_variable('ITFSUP', itfsup, 0, 1, &
               'Switch for TF coil type')
       case ('JBUS')
          call parse_real_variable('JBUS', jbus, 1.0D4, 1.0D8, &
               'TF coil bus current density (A/m2)')
       case ('JCRIT_MODEL')
          call parse_int_variable('JCRIT_MODEL', jcrit_model, 0, 1, &
               'Critical J model for binary Nb3Sn')
       case ('JCRITSC')
          call parse_real_variable('JCRITSC', jcritsc, 1.0D9, 5.0D11, &
               'Critical J for superconductor')
       case ('MAGNT')
          call parse_int_variable('MAGNT', magnt, 1, 3, &
               'SCTF coil stress model')
       case ('OACDCP')
          call parse_real_variable('OACDCP', oacdcp, 1.0D4, 1.0D9, &
               'Overall J in inboard TF coil midplane')
       case ('POISSON')
          call parse_real_variable('POISSON', poisson, 0.0D0, 1.0D0, &
               'Poissons ratio for TF stress calc.')
       case ('PTEMPALW')
          call parse_real_variable('PTEMPALW', ptempalw, 50.0D0, 300.0D0, &
               'Maximum peak centrepost temp. (C)')
       case ('RCOOL')
          call parse_real_variable('RCOOL', rcool, 1.0D-6, 1.0D0, &
               'Centrepost coolant channel radius')
       case ('RIPMAX')
          call parse_real_variable('RIPMAX', ripmax, 0.1D0, 100.0D0, &
               'Max peak/ave ripple at plasma edge (%)')
       case ('STRNCON')
          call parse_real_variable('STRNCON', strncon, -1.0D0, 1.0D0, &
               'Strain in superconductor material')
       case ('TCOOLIN')
          call parse_real_variable('TCOOLIN', tcoolin, -273.1D0, 100.0D0, &
               'Centrepost coolant inlet temperature')
       case ('TCPAV')
          call parse_real_variable('TCPAV', tcpav, -200.0D0, 300.0D0, &
               'Average centrepost coolant temperature')
       case ('TCRITSC')
          call parse_real_variable('TCRITSC', tcritsc, 1.0D0, 300.0D0, &
               'Critical temperature for superconductor')
       case ('TDMPTF')
          call parse_real_variable('TDMPTF', tdmptf, 0.1D0, 100.0D0, &
               'Dump time for TF coil (s)')
       case ('TFLEGRES')
          call parse_real_variable('TFLEGRES', tflegres, 1.0D-10, 1.0D-5, &
               'TF coil leg resistivity (ohm-m)')
       case ('TFNO')
          call parse_real_variable('TFNO', tfno, 0.0D0, 100.0D0, &
               'Number of TF coils')
       case ('TFTMP')
          call parse_real_variable('TFTMP', tftmp, 0.01D0, 10.0D0, &
               'Peak TF coil He coolant temp. (K)')
       case ('TFTORT')
          call parse_real_variable('TFTORT', tftort, 0.1D0, 2.0D0, &
               'TF coil toroidal thickness (m)')
       case ('THICNDUT')
          call parse_real_variable('THICNDUT', thicndut, 0.0D0, 0.1D0, &
               'Conduit insulation thickness (m)')
       case ('THKCAS')
          call parse_real_variable('THKCAS', thkcas, 0.0D0, 1.0D0, &
               'External supercond. case thickness (m)')
       case ('THWCNDUT')
          call parse_real_variable('THWCNDUT', thwcndut, 0.0D0, 0.1D0, &
               'TF coil conduit case thickness (m)')
       case ('TINSTF')
          call parse_real_variable('TINSTF', tinstf, 0.0D0, 0.1D0, &
               'Ground wall insulation thickness (m)')
       case ('TMARGMIN')
          call parse_real_variable('TMARGMIN', tmargmin, 0.0D0, 10.0D0, &
               'Minimum allowable temp margin (K)')
       case ('TMAXPRO')
          call parse_real_variable('TMAXPRO', tmaxpro, 0.0D0, 1.0D3, &
               'Maximum temp rise during quench (K)')
       case ('TMPCRY')
          call parse_real_variable('TMPCRY', tmpcry, 0.01D0, 10.0D0, &
               'Cryogenic temperature (K)')
       case ('VCOOL')
          call parse_real_variable('VCOOL', vcool, 0.001D0, 100.0D0, &
               'Max centrepost coolant speed (m/s)')
       case ('VDALW')
          call parse_real_variable('VDALW', vdalw, 0.0D0, 100.0D0, &
               'Max V across TFC during quench (kV)')
       case ('VFTF')
          call parse_real_variable('VFTF', vftf, 0.0D0, 1.0D0, &
               'Coolant fraction of TF coil leg')
       case ('WPVF')
          call parse_real_variable('WPVF', wpvf, 0.0D0, 1.0D0, &
               'Void fraction of TF coil winding pack')
 
          !  PF coil settings

       case ('AC1OH')
          call parse_real_variable('AC1OH', ac1oh, 0.0D0, 1.0D0, &
               'OH coil cable conduit area (m2)')
       case ('ACSOH')
          call parse_real_variable('ACSOH', acsoh, 1.0D-6, 1.0D0, &
               'Conduit conductor X-section (m2)')
       case ('ALFAPF')
          call parse_real_variable('ALFAPF', alfapf, 1.0D-12, 1.0D0, &
               'PF coil current smoothing parameter')
       case ('COHEOF')
          call parse_real_variable('COHEOF', coheof, 1.0D4, 1.0D8, &
               'OH coil current density at EOF')
       case ('CPTDIN')
          call parse_real_array('CPTDIN', cptdin, isub1, ngc2, &
               'Current per turn for PF coil', icode)
       case ('FCOHBOF')
          call parse_real_variable('FCOHBOF', fcohbof, 0.0D0, 1.0D0, &
               'OH coil J ratio : BOF/EOF')
       case ('FCOHBOP')
          call parse_real_variable('FCOHBOP', fcohbop, 0.0D0, 1.0D0, &
               'OH coil J ratio : BOP/EOF')
       case ('FCUOH')
          call parse_real_variable('FCUOH', fcuoh, 0.0D0, 1.0D0, &
               'Cu frac of conductor in OH coil cable')
       case ('IPFLOC')
          call parse_int_array('IPFLOC', ipfloc, isub1, ngc, &
               'PF coil location', icode)
       case ('IPFRES')
          call parse_int_variable('IPFRES', ipfres, 0, 1, &
               'Switch for supercond / resist PF coils')
       case ('ISUMATPF')
          call parse_int_variable('ISUMATPF', isumatpf, 1, 3, &
               'PF coil superconductor material')
       case ('NCLS')
          call parse_int_array('NCLS', ncls, isub1, ngrpmx, &
               'No of coils in PF group', icode)
       case ('NFXFH')
          call parse_int_variable('NFXFH', nfxfh, 1, nfixmx/2, &
               'OH coil splitting parameter')
       case ('NGRP')
          call parse_int_variable('NGRP', ngrp, 0, ngrpmx, &
               'No of groups of PF coils')
       case ('OHHGHF')
          call parse_real_variable('OHHGHF', ohhghf, 0.0D0, 2.0D0, &
               'OH coil height / TF coil height')
       case ('PFCLRES')
          call parse_real_variable('PFCLRES', pfclres, 0.0D0, 1.0D-4, &
               'PF coil resistivity (ohm-m)')
       case ('RJCONPF')
          call parse_real_array('RJCONPF', rjconpf, isub1, ngc2, &
               'Average J of PF coil (A/m2)', icode)
       case ('ROUTR')
          call parse_real_variable('ROUTR', routr, -3.0D0, 3.0D0, &
               'Gap from outer TFC leg for PFC')
       case ('RPF1')
          call parse_real_variable('RPF1', rpf1, 0.0D0, 3.0D0, &
               'Radial offset for group 1 PF coils')
       case ('RPF2')
          call parse_real_variable('RPF2', rpf2, -3.0D0, 3.0D0, &
               'Radial offset for group 2 PF coils')
       case ('SCCUFAC')
          call parse_real_variable('SCCUFAC', sccufac, 0.001D0, 0.1D0, &
               'sc/cu ratio in PF coils per tesla')
       case ('SIGPFALW')
          call parse_real_variable('SIGPFALW', sigpfalw, 1.0D0, 1.0D3, &
               'Allowable stress in the PF coil (MPa)')
       case ('VF')
          call parse_real_array('VF', vf, isub1, ngc2, &
               'Void fraction of PF coil', icode)
       case ('VFOHC')
          call parse_real_variable('VFOHC', vfohc, 0.0D0, 1.0D0, &
               'OH coil void fraction for coolant')
       case ('ZREF')
          call parse_real_array('ZREF', zref, isub1, ngrpmx, &
               'height of coil group / minor radius', icode)

          !  Pulsed reactor settings

       case ('AFW')
          call parse_real_variable('AFW', afw, 1.0D-3, 0.5D0, &
               'Inner radius of first wall coolant channel (m)')
       case ('BCTMP')
          call parse_real_variable('BCTMP', bctmp, 1.0D0, 800.0D0, &
               'First wall bulk coolant temperature (C)')
       case ('COOLP')
          call parse_real_variable('COOLP', coolp, 1.0D5, 1.0D9, &
               'First wall coolant pressure (Pa)')
       case ('DTSTOR')
          call parse_real_variable('DTSTOR', dtstor, 50.0D0, 500.0D0, &
               'Max temp change in thermal storage medium (K)')
       case ('ISTORE')
          call parse_int_variable('ISTORE', istore, 1, 3, &
               'Switch for thermal storage option')
       case ('ITCYCL')
          call parse_int_variable('ITCYCL', itcycl, 1, 3, &
               'Switch for 1st wall axial stress model')
       case ('LPULSE')
          call parse_int_variable('LPULSE', lpulse, 0, 1, &
               'Switch for pulsed reactor model')
       case ('TMPRSE')
          call parse_real_variable('TMPRSE', tmprse, 1.0D0, 1.0D3, &
               'Temperature rise in first wall coolant (C)')

          !  First wall, blanket, shield settings

       case ('ASTR')
          call parse_int_variable('ASTR', astr, 1, 2, &
               'Switch for cooling channel geometry')
       case ('BSTR')
          call parse_int_variable('BSTR', bstr, 1, 2, &
               'Switch for blanket boundary condition')
       case ('COSTR')
          call parse_int_variable('COSTR', costr, 1, 2, &
               'Switch for blanket coolant material')
       case ('DENSTL')
          call parse_real_variable('DENSTL', denstl, 5.0D3, 1.0D4, &
               'Density of steel (kg/m3)')
       case ('EMULT')
          call parse_real_variable('EMULT', emult, 1.0D0, 2.0D0, &
               'Energy multip. in blanket and shield')
       case ('ESTR')
          call parse_int_variable('ESTR', estr, 1, 2, &
               'Switch for cooling channel orientation')
       case ('ETACP')
          call parse_real_variable('ETACP', etacp, 0.1D0, 1.0D0, &
               'Condenser isentropic efficiency')
       case ('ETAFP')
          call parse_real_variable('ETAFP', etafp, 0.1D0, 1.0D0, &
               'Feed pump isentropic efficiency')
       case ('ETAHP')
          call parse_real_variable('ETAHP', etahp, 0.1D0, 1.0D0, &
               'HP turbine isentropic efficiency')
       case ('ETAINP')
          call parse_real_variable('ETAINP', etainp, 0.1D0, 1.0D0, &
               'IP turbine isentropic efficiency')
       case ('ETALP')
          call parse_real_variable('ETALP', etalp, 0.1D0, 1.0D0, &
               'LP turbine isentropic efficiency')
       case ('FBLBE')
          call parse_real_variable('FBLBE', fblbe, 0.0D0, 1.0D0, &
               'Beryllium fraction of blanket')
       case ('FBLLI')
          call parse_real_variable('FBLLI', fblli, 0.0D0, 1.0D0, &
               'Lithium fraction of blanket')
       case ('FBLLI2O')
          call parse_real_variable('FBLLI2O', fblli2o, 0.0D0, 1.0D0, &
               'Li2O fraction of blanket')
       case ('FBLLIPB')
          call parse_real_variable('FBLLIPB', fbllipb, 0.0D0, 1.0D0, &
               'Li-Pb fraction of blanket')
       case ('FBLSS')
          call parse_real_variable('FBLSS', fblss, 0.0D0, 1.0D0, &
               'Stainless steel fraction of blanket')
       case ('FBLVD')
          call parse_real_variable('FBLVD', fblvd, 0.0D0, 1.0D0, &
               'Vanadium fraction of blanket')
       case ('FHOLE')
          call parse_real_variable('FHOLE', fhole, 0.0D0, 1.0D0, &
               'Hole frac of 1st wall (to neutrons)')
       case ('FKBLKT')
          call parse_real_variable('FKBLKT', fkblkt, 0.2D0, 5.0D0, &
               'Blanket elongation / plasma elongation')
       case ('FVOLBI')
          call parse_real_variable('FVOLBI', fvolbi, 0.0D0, 10.0D0, &
               'Fudge factor for inner blanket volume')
       case ('FVOLBO')
          call parse_real_variable('FVOLBO', fvolbo, 0.0D0, 10.0D0, &
               'Fudge factor for outer blanket volume')
       case ('FVOLCRY')
          call parse_real_variable('FVOLCRY', fvolcry, 0.0D0, 10.0D0, &
               'Fudge factor for cryostat volume')
       case ('FVOLDW')
          call parse_real_variable('FVOLDW', fvoldw, 0.0D0, 10.0D0, &
               'Fudge factor for dewar')
       case ('FVOLSI')
          call parse_real_variable('FVOLSI', fvolsi, 0.0D0, 10.0D0, &
               'Fudge factor for inner shield volume')
       case ('FVOLSO')
          call parse_real_variable('FVOLSO', fvolso, 0.0D0, 10.0D0, &
               'Fudge factor for outer shield volume')
       case ('FWCLFR')
          call parse_real_variable('FWCLFR', fwclfr, 0.0D0, 1.0D0, &
               'First wall coolant fraction')
       case ('LBLNKT')
          call parse_int_variable('LBLNKT', lblnkt, 0, 1, &
               'Switch for blanket model invoked')
       case ('NIPFWH')
          call parse_int_variable('NIPFWH', nipfwh, 1, 4, &
               'Number of IP feed water heater pumps')
       case ('NLPFWH')
          call parse_int_variable('NLPFWH', nlpfwh, 1, 4, &
               'Number of LP feed water heater pumps')
       case ('PC')
          call parse_real_variable('PC', pc, 0.004D0, 0.01D0, &
               'Condenser pressure (MPa)')
       case ('PH')
          call parse_real_variable('PH', ph, 5.0D0, 20.0D0, &
               'High pressure inlet pressure (MPa)')
       case ('PIN')
          call parse_real_variable('PIN', pin, 0.2D0, 1.0D0, &
               'Low pressure inlet pressure (MPa)')
       case ('PR')
          call parse_real_variable('PR', pr, 1.0D0, 5.0D0, &
               'Reheat intermediate pressure (MPa)')
       case ('SGEFF')
          call parse_real_variable('SGEFF', sgeff, 0.1D0, 1.0D0, &
               'Steam generator effectiveness')
       case ('SMSTR')
          call parse_int_variable('SMSTR', smstr, 1, 2, &
               'Switch for blanket material')
       case ('VFBLKT')
          call parse_real_variable('VFBLKT', vfblkt, 0.0D0, 1.0D0, &
               'Coolant void fraction in blanket')
       case ('VFSHLD')
          call parse_real_variable('VFSHLD', vfshld, 0.0D0, 1.0D0, &
               'Coolant void fraction in shield')
       case ('XDI')
          call parse_real_variable('XDI', xdi, 1.0D0, 10.0D0, &
               'Inner cooling channel diameter (cm)')
       case ('XDO')
          call parse_real_variable('XDO', xdo, 1.0D0, 10.0D0, &
               'Outer cooling channel diameter (cm)')
       case ('XPF')
          !  correct range: 1-5 for costr=1, 8-15 for costr=2
          call parse_real_variable('XPF', xpf, 1.0D0, 15.0D0, &
               'Blanket coolant inlet pressure (MPa)')
       case ('XTB')
          call parse_real_variable('XTB', xtb, 400.0D0, 800.0D0, &
               'Maximum blanket temperature (C)')
       case ('XTFI')
          !  correct range: 250-350 for costr=1, 200-300 for costr=2
          call parse_real_variable('XTFI', xtfi, 200.0D0, 350.0D0, &
               'Blanket coolant inlet temperature (C)')
       case ('XTFO')
          !  correct range: 500-800 for costr=1, 295-342 for costr=2
          call parse_real_variable('XTFO', xtfo, 295.0D0, 800.0D0, &
               'Blanket coolant outlet temperature (C)')

          !  Heat transport / power settings

       case ('BASEEL')
          call parse_real_variable('BASEEL', baseel, 1.0D6, 1.0D10, &
               'Base plant electric load (W)')
       case ('ETATH')
          call parse_real_variable('ETATH', etath, 0.0D0, 1.0D0, &
               'Thermal-electric conversion efficiency')
       case ('FAUXBOP')
          call parse_real_variable('FAUXBOP', fauxbop, 0.0D0, 1.0D0, &
               'Frac. of gross electric power to BOP')
       case ('FFWLG')
          call parse_real_variable('FFWLG', ffwlg, 0.0D0, 1.0D0, &
               '1st wall/dvrtr power frac to lg heat')
       case ('FMGDMW')
          call parse_real_variable('FMGDMW', fmgdmw, 0.0D0, 100.0D0, &
               'Power to MGF units (MW)')
       case ('HTPMW')
          call parse_real_variable('HTPMW', htpmw, 0.0D0, 500.0D0, &
               'Heat transport system pump power')
       case ('IPRIMHTP')
          call parse_int_variable('IPRIMHTP', iprimhtp, 0, 1, &
               'Switch for heat transport pump power')
       case ('PWPM2')
          call parse_real_variable('PWPM2', pwpm2, 0.0D0, 1.0D3, &
               'Base AC power requirement (W/m2)')
       case ('TFACPD')
          call parse_real_variable('TFACPD', tfacpd, 0.0D0, 100.0D0, &
               'Total ss TF coil AC power demand (MW)')
       case ('TRITHTMW')
          call parse_real_variable('TRITHTMW', trithtmw, 0.0D0, 100.0D0, &
               'Tritium process power (MW)')
       case ('VACHTMW')
          call parse_real_variable('VACHTMW', vachtmw, 0.0D0, 100.0D0, &
               'Vacuum pump power (MW)')

          !  Cost information settings

       case ('ABKTFLNC')
          call parse_real_variable('ABKTFLNC', abktflnc, 0.1D0, 100.0D0, &
               'Allowable blanket fluence (MW-yr/m2)')
       case ('ADIVFLNC')
          call parse_real_variable('ADIVFLNC', adivflnc, 0.1D0, 100.0D0, &
               'Allowable divertor fluence (MW-yr/m2)')
       case ('CFACTR')
          call parse_real_variable('CFACTR', cfactr, 0.0D0, 1.0D0, &
               'Plant capacity factor or availability')
       case ('CFIND')
          call parse_real_array('CFIND', cfind, isub1, 4, &
               'Indirect cost factor vs LSA', icode)
       case ('CPSTFLNC')
          call parse_real_variable('CPSTFLNC', cpstflnc, 0.01D0, 30.0D0, &
               'Allowable centrepost neutron fluence (MW-yr/m2)')
       case ('DECOMF')
          call parse_real_variable('DECOMF', decomf, 0.0D0, 1.0D0, &
               'Decommissioning fund fraction')
       case ('DINTRT')
          call parse_real_variable('DINTRT', dintrt, 0.0D0, 0.1D0, &
               'Borrowing - saving interest rate difference')
       case ('DTLIFE')
          call parse_real_variable('DTLIFE', dtlife, 0.0D0, 15.0D0, &
               'Decommissioning time prior to end of plant')
       case ('FCAP0')
          call parse_real_variable('FCAP0', fcap0, 1.0D0, 1.5D0, &
               'Ave cost of money for plant construction')
       case ('FCAP0CP')
          call parse_real_variable('FCAP0CP', fcap0cp, 1.0D0, 1.5D0, &
               'Ave cost of money for replaceable components')
       case ('FCDFUEL')
          call parse_real_variable('FCDFUEL', fcdfuel, 0.0D0, 1.0D0, &
               'Fraction of CD cost assumed fuel cost')
       case ('FCR0')
          call parse_real_variable('FCR0', fcr0, 0.0D0, 1.0D0, &
               'Fixed charge rate during construction')
       case ('FKIND')
          call parse_real_variable('FKIND', fkind, 0.5D0, 1.0D0, &
               'Multiplier for Nth of a kind costs')
       case ('IAVAIL')
          call parse_int_variable('IAVAIL', iavail, 0, 1, &
               'Switch for plant availability model')
       case ('IFUELTYP')
          call parse_int_variable('IFUELTYP', ifueltyp, 0, 1, &
               'Switch for costing of 1st wall etc.')
       case ('IPNET')
          call parse_int_variable('IPNET', ipnet, 0, 1, &
               'Switch for net electric power calc.')
       case ('IREACTOR')
          call parse_int_variable('IREACTOR', ireactor, 0, 1, &
               'Switch for MWe / C-o-E calculation')
       case ('LSA')
          call parse_int_variable('LSA', lsa, 1, 4, &
               'Level of safety assurance')
       case ('RATECDOL')
          call parse_real_variable('RATECDOL', ratecdol, 0.0D0, 0.5D0, &
               'Effective cost of money')
       case ('TBKTREPL')
          call parse_real_variable('TBKTREPL', tbktrepl, 0.01D0, 2.0D0, &
               'Time needed to replace blanket (yr)')
       case ('TCOMREPL')
          call parse_real_variable('TCOMREPL', tcomrepl, 0.01D0, 2.0D0, &
               'Time needed to replace blanket+divertor (yr)')
       case ('TDIVREPL')
          call parse_real_variable('TDIVREPL', tdivrepl, 0.01D0, 2.0D0, &
               'Time needed to replace divertor (yr)')
       case ('TLIFE')
          call parse_real_variable('TLIFE', tlife, 1.0D0, 100.0D0, &
               'Plant life (yr)')
       case ('UUBOP')
          call parse_real_variable('UUBOP', uubop, 0.005D0, 0.1D0, &
               'Unplanned unavailability for BOP')
       case ('UUCD')
          call parse_real_variable('UUCD', uucd, 0.005D0, 0.1D0, &
               'Unplanned unavailability for CD system')
       case ('UUDIV')
          call parse_real_variable('UUDIV', uudiv, 0.005D0, 0.1D0, &
               'Unplanned unavailability for divertor')
       case ('UUFUEL')
          call parse_real_variable('UUFUEL', uufuel, 0.005D0, 0.1D0, &
               'Unplanned unavailability for fuel system')
       case ('UUFW')
          call parse_real_variable('UUFW', uufw, 0.005D0, 0.1D0, &
               'Unplanned unavailability for first wall')
       case ('UUMAG')
          call parse_real_variable('UUMAG', uumag, 0.005D0, 0.1D0, &
               'Unplanned unavailability for magnets')
       case ('UUVES')
          call parse_real_variable('UUVES', uuves, 0.005D0, 0.1D0, &
               'Unplanned unavailability for vessel')

          !  Unit cost settings

       case ('CCONFIX')
          call parse_real_variable('CCONFIX', cconfix, 50.0D0, 200.0D0, &
               'Fixed cost of superconducting cable ($/m)')
       case ('CCONSHPF')
          call parse_real_variable('CCONSHPF', cconshpf, 50.0D0, 200.0D0, &
               'PF coil steel conduit/sheath cost ($/m)')
       case ('CCONSHTF')
          call parse_real_variable('CCONSHTF', cconshtf, 50.0D0, 200.0D0, &
               'TF coil steel conduit/sheath cost ($/m)')
       case ('CLAND')
          call parse_real_variable('CLAND', cland, 10.0D0, 100.0D0, &
               'Cost of land (M$)')
       case ('COWNER')
          call parse_real_variable('COWNER', cowner, 0.0D0, 1.0D0, &
               'Owner cost factor')
       case ('CSI')
          call parse_real_variable('CSI', csi, 1.0D0, 100.0D0, &
               'Allowance for site costs (M$)')
       case ('CTURBB')
          call parse_real_variable('CTURBB', cturbb, 100.0D0, 1000.0D0, &
               'Cost of turbine building (M$)')
       case ('FCONTNG')
          call parse_real_variable('FCONTNG', fcontng, 0.0D0, 1.0D0, &
               'Project contingency factor')
       case ('UCBLBE')
          call parse_real_variable('UCBLBE', ucblbe, 1.0D0, 1.0D3, &
               'Unit cost for blanket Be ($/kg)')
       case ('UCBLLI')
          call parse_real_variable('UCBLLI', ucblli, 1.0D2, 1.0D4, &
               'Unit cost for blanket Li ($/kg)')
       case ('UCBLLI2O')
          call parse_real_variable('UCBLLI2O', ucblli2o, 1.0D2, 1.0D4, &
               'Unit cost for blanket Li2O ($/kg)')
       case ('UCBLLIPB')
          call parse_real_variable('UCBLLIPB', ucbllipb, 1.0D2, 1.0D4, &
               'Unit cost for blanket Li-Pb ($/kg)')
       case ('UCBLSS')
          call parse_real_variable('UCBLSS', ucblss, 10.0D0, 1.0D3, &
               'Unit cost for blanket st.steel ($/kg)')
       case ('UCBLVD')
          call parse_real_variable('UCBLVD', ucblvd, 100.0D0, 1.0D3, &
               'Unit cost for blanket Vd ($/kg)')
       case ('UCBUS')
          call parse_real_variable('UCBUS', ucbus, 0.01D0, 10.0D0, &
               'Cost of Al bus for TF coil ($/A-m)')
       case ('UCCASE')
          call parse_real_variable('UCCASE', uccase, 1.0D0, 1.0D3, &
               'Cost of superconductor case ($/kg)')
       case ('UCCPCL1')
          call parse_real_variable('UCCPCL1', uccpcl1, 1.0D0, 1.0D3, &
               'Cost of tapered copper ($/kg)')
       case ('UCCPCLB')
          call parse_real_variable('UCCPCLB', uccpclb, 1.0D0, 1.0D3, &
               'Cost TF outer leg plate coils ($/kg)')
       case ('UCCRY')
          call parse_real_variable('UCCRY', uccry, 1.0D4, 1.0D6, &
               'Heat transport cryoplant costs ($/W)')
       case ('UCCRYO')
          call parse_real_variable('UCCRYO', uccryo, 1.0D0, 1.0D3, &
               'Unit cost for cryostat ($/kg)')
       case ('UCCU')
          call parse_real_variable('UCCU', uccu, 10.0D0, 1.0D2, &
               'Copper in SC cable cost ($/kg)')
       case ('UCDIV')
          call parse_real_variable('UCDIV', ucdiv, 1.0D3, 1.0D7, &
               'Cost of divertor blade ($)')
       case ('UCECH')
          call parse_real_variable('UCECH', ucech, 1.0D0, 10.0D0, &
               'ECH system cost ($/W)')
       case ('UCF1')
          call parse_real_variable('UCF1', ucf1, 1.0D6, 50.0D6, &
               'Fuelling system cost ($)')
       case ('UCFNC')
          call parse_real_variable('UCFNC', ucfnc, 10.0D0, 100.0D0, &
               'Outer PF fence support cost ($/kg)')
       case ('UCFUEL')
          call parse_real_variable('UCFUEL', ucfuel, 1.0D0, 10.0D0, &
               'Cost of fuel (M$/yr)')
       case ('UCHE3')
          call parse_real_variable('UCHE3', uche3, 1.0D5, 1.0D7, &
               'Cost of He3 fuel ($/kg)')
       case ('UCHRS')
          call parse_real_variable('UCHRS', uchrs, 1.0D7, 5.0D8, &
               'Cost of heat rejection system ($)')
       case ('UCHTS')
          call parse_real_array('UCHTS', uchts, isub1, 2, &
               'Cost of heat transp system equip per loop ($/W)', icode)
       case ('UCIAC')
          call parse_real_variable('UCIAC', uciac, 1.0D7, 1.0D9, &
               'Cost of instrum, control & diag.($/W)')
       case ('UCICH')
          call parse_real_variable('UCICH', ucich, 1.0D0, 10.0D0, &
               'Cost of ICH system ($/W)')
       case ('UCIHX')
          call parse_real_variable('UCIHX', ucihx, 0.0D0, 100.0D0, &
               'Cost of intermed. ht exchangers ($/W)')
       case ('UCLH')
          call parse_real_variable('UCLH', uclh, 1.0D0, 10.0D0, &
               'LH system cost ($/W)')
       case ('UCME')
          call parse_real_variable('UCME', ucme, 1.0D7, 1.0D9, &
               'Unit cost of maintenance equip. ($/W)')
       case ('UCMISC')
          call parse_real_variable('UCMISC', ucmisc, 1.0D7, 5.0D7, &
               'Miscellaneous plant allowance ($)')
       case ('UCNBI')
          call parse_real_variable('UCNBI', ucnbi, 1.0D0, 10.0D0, &
               'NBI system cost ($/W)')
       case ('UCOAM')
          call parse_real_array('UCOAM', ucoam, isub1, 4, &
               'Annual cost of operation and maintenance', icode)
       case ('UCOF')
          call parse_real_variable('UCOF', ucof, 0.1D0, 10.0D0, &
               'Oscillating field current drive cost ($/W)')
       case ('UCPENS')
          call parse_real_variable('UCPENS', ucpens, 1.0D0, 100.0D0, &
               'Penetration shield cost ($/kg)')
       case ('UCPFB')
          call parse_real_variable('UCPFB', ucpfb, 1.0D0, 1.0D3, &
               'Cost of PF coil buses ($/kA-m)')
       case ('UCPFBK')
          call parse_real_variable('UCPFBK', ucpfbk, 1.0D3, 1.0D5, &
               'Cost of PF coil DC breakers ($/MVA)')
       case ('UCPFBS')
          call parse_real_variable('UCPFBS', ucpfbs, 1.0D3, 1.0D4, &
               'Cost of PF burn power supplies ($/kW)')
       case ('UCPFCB')
          call parse_real_variable('UCPFCB', ucpfcb, 1.0D3, 1.0D5, &
               'Cost of PF coil AC breakers ($/circ)')
       case ('UCPFDR1')
          call parse_real_variable('UCPFDR1', ucpfdr1, 1.0D0, 1.0D3, &
               'Cost factor for dump resistors ($/MJ)')
       case ('UCPFIC')
          call parse_real_variable('UCPFIC', ucpfic, 1.0D3, 1.0D5, &
               'Cost of PF instrum & cont ($/channel)')
       case ('UCPFPS')
          call parse_real_variable('UCPFPS', ucpfps, 1.0D3, 1.0D5, &
               'Cost of PF coil pulsed P.S. ($/MVA)')
       case ('UCRB')
          call parse_real_variable('UCRB', ucrb, 1.0D2, 1.0D3, &
               'Cost of reactor building ($/m3)')
       case ('UCSC')
          call parse_real_array('UCSC', ucsc, isub1, 5, &
               'Cost of superconductor ($/kg)', icode)
       case ('UCSHLD')
          call parse_real_variable('UCSHLD', ucshld, 1.0D0, 100.0D0, &
               'Cost of shield structural steel ($/kg)')
       case ('UCTFBR')
          call parse_real_variable('UCTFBR', uctfbr, 1.0D0, 10.0D0, &
               'Cost of TF coil breakers ($/W**0.7)')
       case ('UCTFBUS')
          call parse_real_variable('UCTFBUS', uctfbus, 1.0D0, 1.0D3, &
               'Cost of TF coil bus ($/kg)')
       case ('UCTFPS')
          call parse_real_variable('UCTFPS', uctfps, 1.0D0, 1.0D3, &
               'Cost of TF power supplies ($/W**0.7)')
       case ('UCTFSW')
          call parse_real_variable('UCTFSW', uctfsw, 0.1D0, 10.0D0, &
               'Cost of TF slow dump switches ($/A)')
       case ('UCTURB')
          call parse_real_array('UCTURB', ucturb, isub1, 2, &
               'Cost of turbine plant equipment ($)', icode)
       case ('UCWINDPF')
          call parse_real_variable('UCWINDPF', ucwindpf, 100.0D0, 1.0D3, &
               'Cost of SCPF windings ($/m)')
       case ('UCWINDTF')
          call parse_real_variable('UCWINDTF', ucwindtf, 100.0D0, 1.0D3, &
               'Cost of SCTF windings ($/m)')
       case ('UCWST')
          call parse_real_array('UCWST', ucwst, isub1, 4, &
               'cost of waste disposal (M$/yr)', icode)

          !  Sweep settings

       case ('ISWEEP')
          call parse_int_variable('ISWEEP', isweep, 0, ipnscns, &
               'Number of scans to perform')
       case ('NSWEEP')
          call parse_int_variable('NSWEEP', nsweep, 1, ipnscnv, &
               'Variable used in scan')
       case ('SWEEP')
          call parse_real_array('SWEEP', sweep, isub1, ipnscns, &
               'Actual values to use in scan', icode)

          !  Buildings settings

       case ('ADMV')
          call parse_real_variable('ADMV', admv, 1.0D4, 1.0D6, &
               'Administration building volume (m3)')
       case ('CLH1')
          call parse_real_variable('CLH1', clh1, 0.0D0, 20.0D0, &
               'Clearance TF coil to cryostat top (m)')
       case ('CLH2')
          call parse_real_variable('CLH2', clh2, 0.0D0, 30.0D0, &
               'Clearance TF coil to foundation (m)')
       case ('CONV')
          call parse_real_variable('CONV', conv, 1.0D4, 1.0D6, &
               'Control building volume (m3)')
       case ('ESBLDGM3')
          call parse_real_variable('ESBLDGM3', esbldgm3, 1.0D3, 1.0D6, &
               'Energy storage building volume (m3)')
       case ('FNDT')
          call parse_real_variable('FNDT', fndt, 0.0D0, 10.0D0, &
               'Foundation thickness (m)')
       case ('HCCL')
          call parse_real_variable('HCCL', hccl, 0.0D0, 10.0D0, &
               'Clearance around components in hot cell (m)')
       case ('HCWT')
          call parse_real_variable('HCWT', hcwt, 0.0D0, 10.0D0, &
               'Hot cell wall thickness (m)')
       case ('PFBLDGM3')
          call parse_real_variable('PFBLDGM3', pfbldgm3, 1.0D4, 1.0D6, &
               'PF coil power conv. bldg volume (m3)')
       case ('PIBV')
          call parse_real_variable('PIBV', pibv, 1.0D3, 1.0D5, &
               'Power injection building volume (m3)')
       case ('RBRT')
          call parse_real_variable('RBRT', rbrt, 0.0D0, 10.0D0, &
               'Reactor building roof thickness (m)')
       case ('RBWT')
          call parse_real_variable('RBWT', rbwt, 0.0D0, 10.0D0, &
               'Reactor building wall thickness (m)')
       case ('ROW')
          call parse_real_variable('ROW', row, 0.0D0, 10.0D0, &
               'Wall clearance for cranes (m)')
       case ('RXCL')
          call parse_real_variable('RXCL', rxcl, 0.0D0, 10.0D0, &
               'Clearance around reactor (m)')
       case ('SHMF')
          call parse_real_variable('SHMF', shmf, 0.0D0, 1.0D0, &
               'Fraction of TF shield mass per lift')
       case ('SHOV')
          call parse_real_variable('SHOV', shov, 1.0D3, 1.0D6, &
               'Shops and warehouse volume (m3)')
       case ('STCL')
          call parse_real_variable('STCL', stcl, 0.0D0, 10.0D0, &
               'Clearance above crane to roof (m)')
       case ('TFCBV')
          call parse_real_variable('TFCBV', tfcbv, 1.0D4, 1.0D6, &
               'TF coil power conv. bldg volume (m3)')
       case ('TRCL')
          call parse_real_variable('TRCL', trcl, 0.0D0, 10.0D0, &
               'Transport clearance between comps (m)')
       case ('TRIV')
          call parse_real_variable('TRIV', triv, 1.0D4, 1.0D6, &
               'Tritium building volume (m3)')
       case ('WGT')
          call parse_real_variable('WGT', wgt, 1.0D4, 1.0D6, &
               'Reactor building crane capacity (kg)')
       case ('WGT2')
          call parse_real_variable('WGT2', wgt2, 1.0D4, 1.0D6, &
               'Hot cell crane capacity (kg)')

          !  Energy storage settings

       case ('ISCENR')
          call parse_int_variable('ISCENR', iscenr, 1, 3, &
               'Switch for energy storage option')

          !  Output file options settings

       case ('SECT01')
          call parse_int_variable('SECT01', sect01, 0, 1, &
               'Switch for power reactor output section')
       case ('SECT02')
          call parse_int_variable('SECT02', sect02, 0, 1, &
               'Switch for detailed costings output section')
       case ('SECT03')
          call parse_int_variable('SECT03', sect03, 0, 1, &
               'Switch for plasma output section')
       case ('SECT04')
          call parse_int_variable('SECT04', sect04, 0, 1, &
               'Switch for current drive output section')
       case ('SECT05')
          call parse_int_variable('SECT05', sect05, 0, 1, &
               'Switch for divertor output section')
       case ('SECT06')
          call parse_int_variable('SECT06', sect06, 0, 1, &
               'Switch for machine build output section')
       case ('SECT07')
          call parse_int_variable('SECT07', sect07, 0, 1, &
               'Switch for TF coils output section')
       case ('SECT08')
          call parse_int_variable('SECT08', sect08, 0, 1, &
               'Switch for PF coils output section')
       case ('SECT09')
          call parse_int_variable('SECT09', sect09, 0, 1, &
               'Switch for volt-seconds output section')
       case ('SECT10')
          call parse_int_variable('SECT10', sect10, 0, 1, &
               'Switch for support structure output section')
       case ('SECT11')
          call parse_int_variable('SECT11', sect11, 0, 1, &
               'Switch for PF coil inductances output section')
       case ('SECT12')
          call parse_int_variable('SECT12', sect12, 0, 1, &
               'Switch for shield/blanket output section')
       case ('SECT13')
          call parse_int_variable('SECT13', sect13, 0, 1, &
               'Switch for power conversion output section')
       case ('SECT14')
          call parse_int_variable('SECT14', sect14, 0, 1, &
               'Switch for heat transport output section')
       case ('SECT15')
          call parse_int_variable('SECT15', sect15, 0, 1, &
               'Switch for vacuum system output section')
       case ('SECT16')
          call parse_int_variable('SECT16', sect16, 0, 1, &
               'Switch for plant buildings output section')
       case ('SECT17')
          call parse_int_variable('SECT17', sect17, 0, 1, &
               'Switch for AC power output section')
       case ('SECT18')
          call parse_int_variable('SECT18', sect18, 0, 1, &
               'Switch for neutral beams output section')
       case ('SECT19')
          call parse_int_variable('SECT19', sect19, 0, 1, &
               'Switch for ECH output section')
       case ('SECT20')
          call parse_int_variable('SECT20', sect20, 0, 1, &
               'Switch for lower hybrid output section')
       case ('SECT21')
          call parse_int_variable('SECT21', sect21, 0, 1, &
               'Switch for time output section')

          !  Vacuum system settings

       case ('NTYPE')
          call parse_int_variable('NTYPE', ntype, 0, 1, &
               'Pump type')
       case ('PBASE')
          call parse_real_variable('PBASE', pbase, 1.0D-8, 1.0D-3, &
               'Base pressure (Pa)')
       case ('PRDIV')
          call parse_real_variable('PRDIV', prdiv, 0.0D0, 10.0D0, &
               'Divertor chamber pressure in burn (Pa)')
       case ('RAT')
          call parse_real_variable('RAT', rat, 1.0D-10, 1.0D-6, &
               'Plas chamber wall outgas rate (Pa-m/s)')
       case ('TN')
          call parse_real_variable('TN', tn, 1.0D0, 1.0D3, &
               'Neutral gas temp in chamber (K)')

          !  Stellarator settings

       case ('ISTHTR')
          call parse_int_variable('ISTHTR', isthtr, 1, 3, &
               'Stellarator method of auxiliary heating')

          !  Inertial Fusion Energy plant settings

       case ('BLDR')
          call parse_real_variable('BLDR', bldr, 0.0D0, 10.0D0, &
               'IFE blanket radial thickness (m)')
       case ('BLDZL')
          call parse_real_variable('BLDZL', bldzl, 0.0D0, 10.0D0, &
               'IFE blanket bottom part thickness (m)')
       case ('BLDZU')
          call parse_real_variable('BLDZU', bldzu, 0.0D0, 10.0D0, &
               'IFE blanket top part thickness (m)')
       case ('BLMATF')  !  N.B. actually a 2-D array
          call parse_real_array('BLMATF', blmatf, isub1, 3*(maxmat+1), &
               'IFE blanket material fraction', icode)
       case ('CDRIV0')
          call parse_real_variable('CDRIV0', cdriv0, 50.0D0, 500.0D0, &
               'IFE driver cost offset (M$)')
       case ('CDRIV1')
          call parse_real_variable('CDRIV1', cdriv1, 50.0D0, 500.0D0, &
               'IFE driver cost offset (M$)')
       case ('CDRIV2')
          call parse_real_variable('CDRIV2', cdriv2, 50.0D0, 500.0D0, &
               'IFE driver cost offset (M$)')
       case ('CHDZL')
          call parse_real_variable('CHDZL', chdzl, 0.0D0, 10.0D0, &
               'IFE chamber bottom part thickness (m)')
       case ('CHDZU')
          call parse_real_variable('CHDZU', chdzu, 0.0D0, 10.0D0, &
               'IFE chamber top part thickness (m)')
       case ('CHMATF')
          call parse_real_array('CHMATF', chmatf, isub1, maxmat+1, &
               'IFE chamber material fraction', icode)
       case ('CHRAD')
          call parse_real_variable('CHRAD', chrad, 0.1D0, 20.0D0, &
               'IFE chamber radial thickness (m)')
       case ('DCDRV0')
          call parse_real_variable('DCDRV0', dcdrv0, 0.0D0, 200.0D0, &
               'IFE driver cost gradient (M$/MJ)')
       case ('DCDRV1')
          call parse_real_variable('DCDRV1', dcdrv1, 0.0D0, 200.0D0, &
               'IFE driver cost gradient (M$/MJ)')
       case ('DCDRV2')
          call parse_real_variable('DCDRV2', dcdrv2, 0.0D0, 200.0D0, &
               'IFE driver cost gradient (M$/MJ)')
       case ('DRVEFF')
          call parse_real_variable('DRVEFF', drveff, 0.01D0, 1.0D0, &
               'IFE driver efficiency')
       case ('EDRIVE')
          call parse_real_variable('EDRIVE', edrive, 1.0D5, 50.0D6, &
               'IFE driver energy (J)')
       case ('ETAVE')
          call parse_real_array('ETAVE', etave, isub1, 10, &
               'IFE driver efficiency vs driver energy', icode)
       case ('FBREED')
          call parse_real_variable('FBREED', fbreed, 0.0D0, 0.999D0, &
               'Fraction of breeder outside core')
       case ('FBURN')
          call parse_real_variable('FBURN', fburn, 0.01D0, 1.0D0, &
               'IFE burn fraction')
       case ('FLIRAD')
          call parse_real_variable('FLIRAD', flirad, 0.0D0, 10.0D0, &
               'Radius of FLiBe inlet (HYLIFE) (m)')
       case ('FRRMAX')
          call parse_real_variable('FRRMAX', frrmax, 1.0D-6, 1.0D0, &
               'F-value for IFE repetition rate')
       case ('FWDR')
          call parse_real_variable('FWDR', fwdr, 0.0D0, 10.0D0, &
               'IFE first wall radial thickness (m)')
       case ('FWDZL')
          call parse_real_variable('FWDZL', fwdzl, 0.0D0, 10.0D0, &
               'IFE first wall bottom part thickness (m)')
       case ('FWDZU')
          call parse_real_variable('FWDZU', fwdzu, 0.0D0, 10.0D0, &
               'IFE first wall top part thickness (m)')
       case ('FWMATF')  !  N.B. actually a 2-D array
          call parse_real_array('FWMATF', fwmatf, isub1, 3*(maxmat+1), &
               'IFE first wall material fraction', icode)
       case ('GAINVE')
          call parse_real_array('GAINVE', gainve, isub1, 10, &
               'IFE target gain vs driver energy', icode)
       case ('IFEDRV')
          call parse_int_variable('IFEDRV', ifedrv, -1, 2, &
               'IFE driver type')
       case ('IFETYP')
          call parse_int_variable('IFETYP', ifetyp, 0, 3, &
               'IFE device build type')
       case ('MCDRIV')
          call parse_real_variable('MCDRIV', mcdriv, 0.1D0, 10.0D0, &
               'IFE driver cost multiplier')
       case ('PDRIVE')
          call parse_real_variable('PDRIVE', pdrive, 1.0D6, 200.0D6, &
               'IFE driver power to target (W)')
       case ('PIFECR')
          call parse_real_variable('PIFECR', pifecr, 0.0D0, 100.0D0, &
               'IFE cryogenic power (MW)')
       case ('PTARGF')
          call parse_real_variable('PTARGF', ptargf, 0.1D0, 100.0D0, &
               'IFE target factory power at 6Hz (MW)')
       case ('RRMAX')
          call parse_real_variable('RRMAX', rrmax, 1.0D0, 50.0D0, &
               'Maximum IFE repetition rate (Hz)')
       case ('SHDR')
          call parse_real_variable('SHDR', shdr, 0.0D0, 10.0D0, &
               'IFE shield radial thickness (m)')
       case ('SHDZL')
          call parse_real_variable('SHDZL', shdzl, 0.0D0, 10.0D0, &
               'IFE shield bottom part thickness (m)')
       case ('SHDZU')
          call parse_real_variable('SHDZU', shdzu, 0.0D0, 10.0D0, &
               'IFE shield top part thickness (m)')
       case ('SHMATF')  !  N.B. actually a 2-D array
          call parse_real_array('SHMATF', shmatf, isub1, 3*(maxmat+1), &
               'IFE shield material fraction', icode)
       case ('SOMBDR')
          call parse_real_variable('SOMBDR', sombdr, 0.0D0, 10.0D0, &
               'Radius of SOMBRERO blanket bottom (m)')
       case ('SOMTDR')
          call parse_real_variable('SOMTDR', somtdr, 0.0D0, 10.0D0, &
               'Radius of SOMBRERO blanket top (m)')
       case ('TGAIN')
          call parse_real_variable('TGAIN', tgain, 1.0D0, 500.0D0, &
               'IFE target gain')
       case ('UCCARB')
          call parse_real_variable('UCCARB', uccarb, 10.0D0, 1.0D3, &
               'Cost of carbon cloth ($/kg)')
       case ('UCCONC')
          call parse_real_variable('UCCONC', ucconc, 0.1D0, 1.0D3, &
               'Cost of concrete ($/kg)')
       case ('UCFLIB')
          call parse_real_variable('UCFLIB', ucflib, 10.0D0, 1.0D3, &
               'Cost of FLiBe ($/kg)')
       case ('UCTARG')
          call parse_real_variable('UCTARG', uctarg, 0.1D0, 1.0D3, &
               'Cost per IFE target ($/target)')
       case ('V1DR')
          call parse_real_variable('V1DR', v1dr, 0.0D0, 10.0D0, &
               'IFE void 1 radial thickness (m)')
       case ('V1DZL')
          call parse_real_variable('V1DZL', v1dzl, 0.0D0, 10.0D0, &
               'IFE void 1 bottom part thickness (m)')
       case ('V1DZU')
          call parse_real_variable('V1DZU', v1dzu, 0.0D0, 10.0D0, &
               'IFE void 1 top part thickness (m)')
       case ('V1MATF')  !  N.B. actually a 2-D array
          call parse_real_array('V1MATF', v1matf, isub1, 3*(maxmat+1), &
               'IFE void 1 material fraction', icode)
       case ('V2DR')
          call parse_real_variable('V2DR', v2dr, 0.0D0, 10.0D0, &
               'IFE void 2 radial thickness (m)')
       case ('V2DZL')
          call parse_real_variable('V2DZL', v2dzl, 0.0D0, 10.0D0, &
               'IFE void 2 bottom part thickness (m)')
       case ('V2DZU')
          call parse_real_variable('V2DZU', v2dzu, 0.0D0, 10.0D0, &
               'IFE void 2 top part thickness (m)')
       case ('V2MATF')  !  N.B. actually a 2-D array
          call parse_real_array('V2MATF', v2matf, isub1, 3*(maxmat+1), &
               'IFE void 2 material fraction', icode)
       case ('V3DR')
          call parse_real_variable('V3DR', v3dr, 0.0D0, 10.0D0, &
               'IFE void 3 radial thickness (m)')
       case ('V3DZL')
          call parse_real_variable('V3DZL', v3dzl, 0.0D0, 10.0D0, &
               'IFE void 3 bottom part thickness (m)')
       case ('V3DZU')
          call parse_real_variable('V3DZU', v3dzu, 0.0D0, 10.0D0, &
               'IFE void 3 top part thickness (m)')
       case ('V3MATF')  !  N.B. actually a 2-D array
          call parse_real_array('V3MATF', v3matf, isub1, 3*(maxmat+1), &
               'IFE void 3 material fraction', icode)

          !  Hydrogen plant settings

       case ('ETAHHTEN')
          call parse_real_variable('ETAHHTEN', etahhten, 0.0D0, 1.48D0, &
               'H production efficiency for HTEN')
       case ('ETAHHTEX')
          call parse_real_variable('ETAHHTEX', etahhtex, 0.0D0, 1.19D0, &
               'H production efficiency for HTEX')
       case ('ETAHLTE')
          call parse_real_variable('ETAHLTE', etahlte, 0.0D0, 1.0D0, &
               'H production efficiency for LTE')
       case ('ETAHTH')
          call parse_real_variable('ETAHTH', etahth, 0.0D0, 1.0D0, &
               'H production efficiency for TH')
       case ('HELECMW')
          call parse_real_variable('HELECMW', helecmw, 0.0D0, 8000.0D0, &
               'Electrical power for H production (MW)')
       case ('HTHERMMW')
          call parse_real_variable('HTHERMMW', hthermmw, 0.0D0, 8000.0D0, &
               'Thermal power for H production (MW)')
       case ('IHPLANT')
          call parse_int_variable('IHPLANT', ihplant, 0, 4, &
               'Hydrogen Production Plant')
       case ('UCHHTEN')
          call parse_real_variable('UCHHTEN', uchhten, 0.0D0, 2000.0D0, &
               'Unit cost of HTEN H production ($/kW)')
       case ('UCHHTEX')
          call parse_real_variable('UCHHTEX', uchhtex, 0.0D0, 2000.0D0, &
               'Unit cost of HTEX H production ($/kW)')
       case ('UCHLTE')
          call parse_real_variable('UCHLTE', uchlte, 0.0D0, 2000.0D0, &
               'Unit cost of LTE H production ($/kW)')
       case ('UCHTH')
          call parse_real_variable('UCHTH', uchth, 0.0D0, 2000.0D0, &
               'Unit cost of TH H production ($/kW)')

       case default
          error_code = lineno
          error_routine = 'PARSE_INPUT_FILE'
          error_message = &
               'Unknown variable in input file: '//varnam(1:varlen)
          call report_error

       end select variable

       !  If we have just read in an array, a different loop-back is needed

       if (icode == -1) goto 20

       cycle

    end do loop_over_lines

  end subroutine parse_input_file

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_real_variable(varnam,varval,vmin,vmax,description)

    !+ad_name  parse_real_variable
    !+ad_summ  Routine that obtains the value of a real variable from the input
    !+ad_summ  file and checks that it lies within the expected range
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  varnam : input string : name of the variable
    !+ad_args  varval : input/output real : value of the variable
    !+ad_args  vmin : input real : minimum allowed value for the variable
    !+ad_args  vmax : input real : maximum allowed value for the variable
    !+ad_args  description : input string : brief description of the variable
    !+ad_desc  This routine parses a line containing a 'name = value' pair
    !+ad_desc  for a real variable, extracting the value from the line
    !+ad_desc  and checking whether it lies between user-defined lower and
    !+ad_desc  upper limits.
    !+ad_prob  None
    !+ad_call  get_value_real
    !+ad_call  check_range_real
    !+ad_call  report_error
    !+ad_hist  13/04/11 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    real(kind(1.0D0)), intent(inout) :: varval
    real(kind(1.0D0)), intent(in) :: vmin, vmax

    !  Local variables

    real(kind(1.0D0)) :: oldval

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME
    !  and stop if this is the case

    if (subscript_present) then
       error_code = lineno
       error_routine = 'PARSE_REAL_VARIABLE'
       error_message = 'Unexpected subscript found'
       call report_error
    end if

    !  Obtain the new value for the variable

    oldval = varval
    call get_value_real(varval,icode)
    if (icode /= 0) then
       error_code = icode
       error_routine = 'PARSE_REAL_VARIABLE'
       error_message = 'Error whilst reading input file'
       call report_error
    end if

    !  Check variable lies within range

    call check_range_real(varnam,varval,vmin,vmax)

    if ((report_changes == 1).and.(varval /= oldval)) then
       write(outfile,*) trim(description),', ',trim(varnam),' = ',varval
    end if

  end subroutine parse_real_variable

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_int_variable(varnam,varval,vmin,vmax,description)

    !+ad_name  parse_int_variable
    !+ad_summ  Routine that obtains the value of an integer variable from the
    !+ad_summ  input file and checks that it lies within the expected range
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  varnam : input string : name of the variable
    !+ad_args  varval : input/output integer : value of the variable
    !+ad_args  vmin : input integer : minimum allowed value for the variable
    !+ad_args  vmax : input integer : maximum allowed value for the variable
    !+ad_args  description : input string : brief description of the variable
    !+ad_desc  This routine parses a line containing a 'name = value' pair
    !+ad_desc  for an integer variable, extracting the value from the line
    !+ad_desc  and checking whether it lies between user-defined lower and
    !+ad_desc  upper limits.
    !+ad_prob  None
    !+ad_call  get_value_int
    !+ad_call  check_range_int
    !+ad_call  report_error
    !+ad_hist  13/04/11 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    integer, intent(inout) :: varval
    integer, intent(in) :: vmin, vmax

    !  Local variables

    integer :: oldval

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME
    !  and stop if this is the case

    if (subscript_present) then
       error_code = lineno
       error_routine = 'PARSE_INT_VARIABLE'
       error_message = 'Unexpected subscript found'
       call report_error
    end if

    !  Obtain the new value for the variable

    oldval = varval
    call get_value_int(varval,icode)
    if (icode /= 0) then
       error_code = icode
       error_routine = 'PARSE_INT_VARIABLE'
       error_message = 'Error whilst reading input file'
       call report_error
    end if

    !  Check variable lies within range

    call check_range_int(varnam,varval,vmin,vmax)

    if ((report_changes == 1).and.(varval /= oldval)) then
       write(outfile,*) trim(description),', ',trim(varnam),' = ',varval
    end if

  end subroutine parse_int_variable

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_string_variable(varnam,varval,description)

    !+ad_name  parse_string_variable
    !+ad_summ  Routine that obtains the value of a string variable from the
    !+ad_summ  input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  varnam : input string : name of the variable
    !+ad_args  varval : input/output string : value of the variable
    !+ad_args  description : input string : brief description of the variable
    !+ad_desc  This routine parses a line containing a 'name = value' pair
    !+ad_desc  for a string variable, extracting the value from the line.
    !+ad_prob  None
    !+ad_call  get_substring
    !+ad_call  report_error
    !+ad_hist  13/04/11 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    character(len=*), intent(inout) :: varval

    !  Local variables

    character(len=200) :: oldval

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME
    !  and stop if this is the case

    if (subscript_present) then
       error_code = lineno
       error_routine = 'PARSE_STRING_VARIABLE'
       error_message = 'Unexpected subscript found'
       call report_error
    end if

    !  Obtain the new value for the variable

    oldval = varval
    call get_substring(varval,icode)
    if (icode /= 0) then
       error_code = icode
       error_routine = 'PARSE_STRING_VARIABLE'
       error_message = 'Error whilst reading input file'
       call report_error
    end if

    if ((report_changes == 1).and.(trim(varval) /= trim(oldval))) then
       write(outfile,*) trim(description),', ',trim(varnam),' = ',varval
    end if

  end subroutine parse_string_variable

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_real_array(varnam,varval,isub1,n,description,icode)

    !+ad_name  parse_real_array
    !+ad_summ  Routine that obtains the values of a real array from the input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  varnam : input string : name of the variable
    !+ad_args  varval(n) : input/output real array : value of the variable
    !+ad_args  isub1 : input integer : array element pointer
    !+ad_args  n : input integer : size of varval array
    !+ad_args  icode : output integer : diagnostic flag
    !+ad_args  description : input string : brief description of the variable
    !+ad_desc  This routine parses a line in one of the two following forms:
    !+ad_desc  <PRE>
    !+ad_desc  name = v1[, v2, ...]
    !+ad_desc  name(element) = v
    !+ad_desc  </PRE>
    !+ad_desc  to read in and extract one or more values for a real 1-D array.
    !+ad_desc  <P>N.B. No array bounds or value range checking is performed.
    !+ad_prob  None
    !+ad_call  get_value_real
    !+ad_call  report_error
    !+ad_hist  03/10/12 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    integer, intent(inout) :: isub1
    integer, intent(in) :: n
    integer, intent(out) :: icode
    real(kind(1.0D0)), dimension(n), intent(inout) :: varval

    !  Local variables

    real(kind(1.0D0)) :: oldval, val

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME

    if (subscript_present) then

       oldval = varval(isub1)
       call get_value_real(val,icode)

       if (icode /= 0) then
          error_code = icode
          error_routine = 'PARSE_REAL_ARRAY'
          error_message = 'GET_VALUE_REAL returns with icode = '
          call report_error
       end if

       varval(isub1) = val
       if ((report_changes == 1).and.(varval(isub1) /= oldval)) then
          write(outfile,10) trim(description),', &
               ',trim(varnam),'(',isub1,') = ',varval(isub1)
       end if

    else

       isub1 = 1
       do
          call get_value_real(val,icode)
          !  icode == 1 denotes an error
          !  icode == -1 denotes end of line, so the next line needs to be read in
          !  (hence the 'goto 20' in the calling routine)
          if (icode /= 0) return

          oldval = varval(isub1)
          varval(isub1) = val
          if ((report_changes == 1).and.(varval(isub1) /= oldval)) then
             write(outfile,10) trim(description),', &
                  ',trim(varnam),'(',isub1,') = ',varval(isub1)
          end if
          isub1 = isub1 + 1
       end do
    end if

10  format(a,a,a,a1,i3,a,e14.6e2)

  end subroutine parse_real_array

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine parse_int_array(varnam,varval,isub1,n,description,icode)

    !+ad_name  parse_int_array
    !+ad_summ  Routine that obtains the values of an integer array
    !+ad_summ  from the input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  varnam : input string : name of the variable
    !+ad_args  varval(n) : input/output integer array : value of the variable
    !+ad_args  isub1 : input integer : array element pointer
    !+ad_args  n : input integer : size of varval array
    !+ad_args  icode : output integer : diagnostic flag
    !+ad_args  description : input string : brief description of the variable
    !+ad_desc  This routine parses a line in one of the two following forms:
    !+ad_desc  <PRE>
    !+ad_desc  name = v1[, v2, ...]
    !+ad_desc  name(element) = v
    !+ad_desc  </PRE>
    !+ad_desc  to read in and extract one or more values for an integer 1-D array.
    !+ad_desc  <P>N.B. No array bounds or value range checking is performed.
    !+ad_prob  None
    !+ad_call  get_value_int
    !+ad_call  report_error
    !+ad_hist  03/10/12 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: varnam, description
    integer, intent(inout) :: isub1
    integer, intent(in) :: n
    integer, intent(out) :: icode
    integer, dimension(n), intent(inout) :: varval

    !  Local variables

    integer :: oldval, val

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Check whether a subscript was found by the preceding call to GET_VARIABLE_NAME

    if (subscript_present) then

       oldval = varval(isub1)
       call get_value_int(val,icode)

       if (icode /= 0) then
          error_code = icode
          error_routine = 'PARSE_INT_ARRAY'
          error_message = 'GET_VALUE_INT returns with icode = '
          call report_error
       end if

       varval(isub1) = val
       if ((report_changes == 1).and.(varval(isub1) /= oldval)) then
          write(outfile,10) trim(description),', &
               ',trim(varnam),'(',isub1,') = ',varval(isub1)
       end if

    else

       isub1 = 1
       do
          call get_value_int(val,icode)
          !  icode == 1 denotes an error
          !  icode == -1 denotes end of line, so the next line needs to be read in
          !  (hence the 'goto 20' in the calling routine)
          if (icode /= 0) return

          oldval = varval(isub1)
          varval(isub1) = val
          if ((report_changes == 1).and.(varval(isub1) /= oldval)) then
             write(outfile,10) trim(description),', &
                  ',trim(varnam),'(',isub1,') = ',varval(isub1)
          end if
          isub1 = isub1 + 1
       end do
    end if

10  format(a,a,a,a1,i3,a,i)

  end subroutine parse_int_array

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine string_to_int(string,length,ivar,icode)

    !+ad_name  string_to_int
    !+ad_summ  Routine that converts the ASCII digits in a string to
    !+ad_summ  an integer
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  string : input string : contains digits of the number
    !+ad_args  length : input integer : useful length of character string
    !+ad_args  ivar : output integer : value stored in the string
    !+ad_args  icode : output integer : diagnostic flag
    !+ad_desc  This routine converts the ASCII digits in string(1:length)
    !+ad_desc  to the integer ivar. It is equivalent to doing
    !+ad_desc  'READ(STRING(1:LENGTH),I) IVAR' but this routine conforms
    !+ad_desc  to the ANSI standard.
    !+ad_desc  Each digit is parsed in turn, the current total is multiplied
    !+ad_desc  by ten and the new digit is added.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_hist  01/04/08 PJK (v2.3.0.0) Replaced the use of an overflowing
    !+ad_hisc    integer as a flag, with a non-overflowing but large value
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: string
    integer, intent(in) :: length
    integer, intent(out) :: ivar, icode

    !  Local variables

    character(len=200) :: xstr
    integer :: iptr,izero,xlen
    logical :: negate

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ivar = 0
    icode = 0

    if (length <= 0) goto 1000

    negate = .false.
    izero = ichar('0')
    iptr = 1
    xstr = string(1:length)

    ! *** Ignore trailing spaces

    xlen = len_trim(xstr)
    if (xlen <= 0) goto 1000

    ! *** Ignore leading spaces

10  continue
    if (xstr(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr > xlen) goto 1000
       goto 10
    end if

    ! *** Check for leading + or -

    if (xstr(iptr:iptr) == '+') then
       iptr = iptr + 1
       if (iptr > xlen) goto 1000
    else if (xstr(iptr:iptr) == '-') then
       negate = .true.
       iptr = iptr + 1
       if (iptr > xlen) goto 1000
    else
       continue
    end if

    ! *** Ignore leading zeros

20  continue
    if (xstr(iptr:iptr) == '0') then
       iptr = iptr + 1
       if (iptr > xlen) goto 1000
       goto 20
    end if

    ! *** Check for number too large

    if ((xlen-iptr+1) > 10) then
       if (negate) then
          ivar = -1234567890
       else
          ivar = 1234567890
       end if
       icode = 1
       goto 1000
    else if ((xlen-iptr+1) == 10) then
       if (xstr(iptr:xlen) > '2147483647') then
          if (negate) then
             ivar = -1234567890
          else
             ivar = 1234567890
          end if
          icode = 1
          goto 1000
       end if
    else
       continue
    end if

    ! *** Parse the digits

30  continue
    if ((xstr(iptr:iptr) >= '0').and.(xstr(iptr:iptr) <= '9')) then
       ivar = (ivar * 10) + (ichar(xstr(iptr:iptr))-izero)
       iptr = iptr + 1
       if (iptr <= xlen) goto 30

       ! *** This is the normal exit path...

       if (negate) ivar = -ivar

    else
       icode = 1
    end if

1000 continue

  end subroutine string_to_int

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine string_to_real(string,length,rval,icode)

    !+ad_name  string_to_real
    !+ad_summ  Routine that converts the ASCII digits in a string to
    !+ad_summ  a real value
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  string : input string : contains digits of the number
    !+ad_args  length : input integer : useful length of character string
    !+ad_args  rvar : output real : value stored in the string
    !+ad_args  icode : output integer : diagnostic flag
    !+ad_desc  This routine converts the ASCII digits in string(1:length)
    !+ad_desc  to the real variable rvar.
    !+ad_desc  The string is parsed one character at a time, from the left,
    !+ad_desc  handling the mantissa, and all other components of the real
    !+ad_desc  number separately, combining them at the end.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: string
    integer, intent(in) :: length
    real(kind(1.0D0)), intent(out) :: rval
    integer, intent(out) :: icode

    !  Local variables

    real(kind(1.0D0)) :: valbdp,valadp,xfact
    integer :: iptr,izero,iexpon
    logical :: negatm,negate

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    iptr = 1

    ! *** Ignore leading spaces

10  continue
    if (string(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= length) goto 10
    end if

    ! *** Initialise real value

    rval = 0.0D0

    ! *** ASCII '0'

    izero = ichar('0')

    ! *** If negative mantissa

    negatm = .false.

    ! *** If negative exponent

    negate = .false.

    ! *** Value before decimal point

    valbdp = 0.0D0

    ! *** Value after decimal point

    valadp = 0.0D0

    ! *** Exponent

    iexpon = 0

    ! *** First character can be +, -, ., or <digit>

    if (string(iptr:iptr) == '+') then
       iptr = iptr + 1
       if (iptr > length) goto 50
    else if (string(iptr:iptr) == '-') then
       iptr = iptr + 1
       if (iptr > length) goto 50
       negatm = .true.
    else
       continue
    end if

    ! *** Parse the mantissa - before the decimal point

    valbdp = 0.0D0
    xfact = 0.1D0
20  continue
    if ((string(iptr:iptr) >= '0').and.(string(iptr:iptr) <= '9')) then
       valbdp = (valbdp * 10.0D0) + dble(ichar(string(iptr:iptr))-izero)
       iptr = iptr + 1
       if (iptr > length) goto 50
       goto 20
    end if

    ! *** After the mantissa, we expect '.' or 'd' or 'e'

    if (string(iptr:iptr) == '.') then
       iptr = iptr + 1
       if (iptr > length) goto 50
    end if

    ! *** Parse the mantissa - after the decimal point

    valadp = 0.0D0
30  continue
    if ((string(iptr:iptr) >= '0').and.(string(iptr:iptr) <= '9')) then
       valadp = valadp + (dble(ichar(string(iptr:iptr))-izero)*xfact)
       xfact = xfact * 0.1D0
       iptr = iptr + 1
       if (iptr > length) goto 50
       goto 30
    end if

    ! *** Now we expect the exponent

    if ( (string(iptr:iptr) == 'D').or. &
         (string(iptr:iptr) == 'E').or. &
         (string(iptr:iptr) == 'd').or. &
         (string(iptr:iptr) == 'e')) then
       iptr = iptr + 1
       if (iptr > length) goto 50

       ! *** First character can be +, -, ., or <digit>

       if (string(iptr:iptr) == '+') then
          iptr = iptr + 1
          if (iptr > length) goto 50
       else if (string(iptr:iptr) == '-') then
          iptr = iptr + 1
          if (iptr > length) goto 50
          negate = .true.
       else
          continue
       end if

       ! *** Parse the exponent

40     continue
       if ((string(iptr:iptr) >= '0').and.(string(iptr:iptr) <= '9')) then
          iexpon = (iexpon * 10) + (ichar(string(iptr:iptr))-izero)
          iptr = iptr + 1
          if (iptr <= length) goto 40
       end if
    else
       goto 60
    end if

50  continue

    ! *** Negative exponent?

    if (negate) iexpon = -iexpon

    ! *** Build the number at last

    if (iexpon == 0) then
       rval = (valbdp + valadp)
    else
       rval = (valbdp + valadp) * (10.0D0 ** iexpon)
    end if

    ! *** Negative mantissa?

    if (negatm) rval = -rval

    ! *** All OK

    icode = 0
    goto 1000

    ! *** Errors

60  continue
    icode = 1

1000 continue

  end subroutine string_to_real

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_value_int(ival,icode)

    !+ad_name  get_value_int
    !+ad_summ  Routine that extracts an integer value from a line of the
    !+ad_summ  input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  ival   : output integer : extracted integer value
    !+ad_args  icode  : output integer : diagnostic flag
    !+ad_desc  This routine extracts an integer value from the current line of
    !+ad_desc  the input file, i.e. the value of an integer variable as
    !+ad_desc  specified by the user.
    !+ad_prob  None
    !+ad_call  string_to_int
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: ival, icode

    !  Local variables

    character(len=200) :: varval
    integer :: varlen,iost

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Ignore leading spaces

10  continue
    if (iptr <= linelen) then
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          goto 10
       end if
    end if

    if (iptr > linelen) then

       ! *** Read next line of namelist data

20     CONTINUE
       read(infile,'(A)',iostat=iost) line

       ! *** On error or end, leave routine with error code set

       if (iost /= 0) goto 60

       lineno = lineno + 1

       ! *** Ignore blank lines

       if (line == ' ') goto 10

       ! *** Ignore comments, unless they start with '*****',
       ! *** in which case print them.

       if (line(1:5) == '*****') then
          write(outfile,*) line(1:76)
       end if

       if (line(1:1) == '*') goto 10

       ! *** Linelen of line excluding trailing spaces

       linelen = len_trim(line)

       ! *** If $END, return

       if (line(1:1) == '$') then
          icode = -1
          goto 1000
       end if
       iptr = 1
30     continue
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          if (iptr <= linelen) goto 30
          goto 20
       end if

       ! *** A continuation line starts with 0-9, - or + (more numbers)

       if ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) goto 40
       if ((line(iptr:iptr) == '+').or.(line(iptr:iptr) == '-')) goto 40
       icode = -1
       goto 1000
40     continue
    end if

    ! *** Put rest of line into varval (makes it easier to parse)

    varval = line(iptr:)
    varlen = index(varval,',') - 1
    if (varlen <= 0) varlen = index(varval,' ') - 1
    if (varlen <= 0) varlen = iptr

    ! *** Update pointer

    iptr = iptr + varlen

    ! *** Ignore trailing spaces

50  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 50
    end if

    ! *** Ignore comma, if present

    if (iptr <= linelen) then
       if (line(iptr:iptr) == ',') iptr = iptr + 1
    end if

    ! *** Convert the ASCII text into an integer value

    call string_to_int(varval,varlen,ival,icode)

    goto 1000

60  continue
    icode = 1

1000 continue

  end subroutine get_value_int

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_value_real(rval,icode)

    !+ad_name  get_value_real
    !+ad_summ  Routine that extracts a real value from a line of the input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rval   : output real : extracted real value
    !+ad_args  icode  : output integer : diagnostic flag
    !+ad_desc  This routine extracts a real value from the current line of
    !+ad_desc  the input file, i.e. the value of a real variable as specified
    !+ad_desc  by the user.
    !+ad_prob  None
    !+ad_call  string_to_real
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: icode
    real(kind(1.0D0)), intent(out) :: rval

    !  Local variables

    character(len=200) :: varval
    integer :: varlen,iost

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Ignore leading spaces

10  continue
    if (iptr <= linelen) then
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          goto 10
       end if
    end if
    if (iptr > linelen) then

       ! *** Read next line of namelist data

20     continue
       read(infile,'(A)',iostat=iost) line

       ! *** On error or end, leave routine with error set

       if (iost /= 0) goto 60

       lineno = lineno + 1

       ! *** Ignore blank lines

       if (line == ' ') goto 10

       ! *** Ignore comments, unless they start with '*****',
       ! *** in which case print them.

       if (line(1:5) == '*****') then
          write(outfile,*) line(1:76)
       end if

       if (line(1:1) == '*') goto 10

       ! *** Linelen of line excluding trailing spaces

       linelen = len_trim(line)

       ! *** If $END, return

       if (line(1:1) == '$') then
          icode = -1
          goto 1000
       end if
       iptr = 1
30     continue
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          if (iptr <= linelen) goto 30
          goto 20
       end if

       ! *** A continuation line starts with 0-9, - or + (more numbers)

       if ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) goto 40
       if ((line(iptr:iptr) == '+').or.(line(iptr:iptr) == '-')) goto 40
       icode = -1
       goto 1000
40     continue

    end if

    ! *** Put rest of line into varval (makes it easier to parse)

    varval = line(iptr:)
    varlen = index(varval,',') - 1
    if (varlen <= 0) varlen = index(varval,' ') - 1
    if (varlen <= 0) varlen = iptr

    ! *** Update pointer

    iptr = iptr + varlen

    ! *** Ignore trailing spaces

50  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 50
    end if

    ! *** Ignore comma, if present

    if (iptr <= linelen) then
       if (line(iptr:iptr) == ',') iptr = iptr + 1
    end if

    ! *** Convert the ASCII text into a real value

    call string_to_real(varval,varlen,rval,icode)

    goto 1000

60  continue
    icode = 1

1000 continue

  end subroutine get_value_real

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_substring_trim(string,icode)

    !+ad_name  get_substring_trim
    !+ad_summ  Routine that extracts a substring from a line of the input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  string : output string : extracted string
    !+ad_args  icode  : output integer : diagnostic flag
    !+ad_desc  This routine extracts a string from the current line of
    !+ad_desc  the input file, i.e. the value of a string variable as specified
    !+ad_desc  by the user.
    !+ad_prob  This routine truncates the string found at its first
    !+ad_prob  non-leading blank, so routine <A HREF="get_substring.html">get_substring</A>
    !+ad_prob  is used in practice.
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_stat  Okay, but not used at present
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: icode
    character(len=*), intent(out) :: string

    !  Local variables

    character(len=200) :: varval
    integer :: varlen,iost

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Ignore leading spaces

10  continue
    if (iptr <= linelen) then
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          goto 10
       end if
    end if

    if (iptr > linelen) then

       ! *** Read next line of namelist data

20     continue
       read(infile,'(A)',iostat=iost) line

       ! *** On error or end, leave routine with error set

       if (iost /= 0) goto 60

       lineno = lineno + 1

       ! *** Ignore blank lines

       if (line == ' ') goto 10

       ! *** Ignore comments, unless they start with '*****',
       ! *** in which case print them.

       if (line(1:5) == '*****') then
          write(outfile,*) line(1:76)
       end if

       if (line(1:1) == '*') goto 10

       ! *** Length of line excluding trailing spaces

       linelen = len_trim(line)

       ! *** If $END, return

       if (line(1:1) == '$') then
          icode = -1
          goto 1000
       end if
       iptr = 1
30     continue
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          if (iptr <= linelen) goto 30
          goto 20
       end if

       ! *** A continuation line starts with 0-9, - or + (more numbers)

       if ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) goto 40
       if ((line(iptr:iptr) == '+').or.(line(iptr:iptr) == '-')) goto 40
       icode = -1
       goto 1000
40     continue

    end if

    ! *** Put rest of line into varval (makes it easier to parse)

    varval = line(iptr:)
    varlen = index(varval,',') - 1
    if (varlen <= 0) varlen = index(varval,' ') - 1
    if (varlen <= 0) varlen = iptr

    ! *** Update pointer

    iptr = iptr + varlen

    ! *** Ignore trailing spaces

50  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 50
    end if

    ! *** Ignore comma, if present

    if (iptr <= linelen) then
       if (line(iptr:iptr) == ',') iptr = iptr + 1
    end if

    ! *** Write the text into the variable

    string = varval(1:varlen)

    goto 1000

60  continue
    icode = 1

1000 continue

  end subroutine get_substring_trim

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_substring(string,icode)

    !+ad_name  get_substring
    !+ad_summ  Routine that extracts a substring from a line of the input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  string : output string : extracted string
    !+ad_args  icode  : output integer : diagnostic flag
    !+ad_desc  This routine extracts a string from the current line of
    !+ad_desc  the input file, i.e. the value of a string variable as specified
    !+ad_desc  by the user. Unlike routine
    !+ad_desc  <A HREF="get_substring_trim.html">get_substring_trim</A>,
    !+ad_desc  this routine does not truncate the string found at its first
    !+ad_desc  non-leading blank.
    !+ad_prob  None
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: icode
    character(len=*), intent(out) :: string

    !  Local variables

    character(len=200) :: varval
    integer :: varlen,iost

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Ignore leading spaces

10  continue
    if (iptr <= linelen) then
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          goto 10
       end if
    end if
    if (iptr > linelen) then

       ! *** Read next line of namelist data

20     continue
       read(infile,'(A)',iostat=iost) line

       ! *** On error or end, leave routine with error set

       if (iost /= 0) goto 60

       lineno = lineno + 1

       ! *** Ignore blank lines

       if (line == ' ') goto 10

       ! *** Ignore comments, unless they start with '*****',
       ! *** in which case print them.

       if (line(1:5) == '*****') then
          write(outfile,*) line(1:76)
       end if

       if (line(1:1) == '*') goto 10

       ! *** Length of line excluding trailing spaces

       linelen = len_trim(line)

       ! *** If $END, return

       if (line(1:1) == '$') then
          icode = -1
          goto 1000
       end if
       iptr = 1
30     continue
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          if (iptr <= linelen) goto 30
          goto 20
       end if

       ! *** A continuation line starts with 0-9, - or + (more numbers)

       if ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) goto 40
       if ((line(iptr:iptr) == '+').or.(line(iptr:iptr) == '-')) goto 40
       icode = -1
       goto 1000
40     continue

    end if

    ! *** Put rest of line into varval (makes it easier to parse)

    varval = line(iptr:)
    varlen = index(varval,',') - 1
    if (varlen <= 0) varlen = index(varval,' ') - 1
    if (varlen <= 0) varlen = iptr

    ! *** Update pointer

    iptr = iptr + varlen

    ! *** Ignore trailing spaces

50  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 50
    end if

    ! *** Ignore comma, if present

    if (iptr <= linelen) then
       if (line(iptr:iptr) == ',') iptr = iptr + 1
    end if

    ! *** Write the text into the variable

    string = varval

    goto 1000

60  continue
    icode = 1

1000 continue

  end subroutine get_substring

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_subscript(isub1,isub2,icode)

    !+ad_name  get_subscript
    !+ad_summ  Routine that extracts any subscripts present in a line of
    !+ad_summ  the input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  isub1  : output integer : first subscript found
    !+ad_args  isub2  : output integer : second subscript found
    !+ad_args  icode  : output integer : diagnostic flag
    !+ad_desc  This routine extracts any subscripts from the current line of
    !+ad_desc  the input file, i.e. if any array elements are specified
    !+ad_desc  by the user. It looks at the next non-space character in the
    !+ad_desc  line, and if it is a left bracket, it assumes that at
    !+ad_desc  least one subscript is to follow and extracts it/them.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: isub1, isub2, icode

    !  Local variables

    integer :: izero
    logical :: negate

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Initial values

    isub1 = 0
    isub2 = 0

    ! *** First character should be '('

    if (line(iptr:iptr) /= '(') goto 70
    iptr = iptr + 1
    if (iptr > linelen) goto 80

    ! *** Parse the first subscript
    ! *** Ignore leading spaces

10  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr > linelen) goto 80
       goto 10
    end if

    izero = ichar('0')
    negate = .false.

    ! *** Extract and evaluate the first subscript
    ! *** Subscript may be prefaced by '+' or '-'

    if (line(iptr:iptr) == '+') then
       iptr = iptr + 1
       if (iptr > linelen) goto 80
    else if (line(iptr:iptr) == '-') then
       negate = .true.
       iptr = iptr + 1
       if (iptr > linelen) goto 80
    else
       continue
    end if

20  continue

    if ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) then
       isub1 = isub1 * 10 + ichar(line(iptr:iptr)) - izero
       iptr = iptr + 1
       if (iptr > linelen) goto 80
       goto 20
    end if
    if (negate) isub1 = -isub1

    ! *** Ignore trailing spaces of first subscript

30  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr > linelen) goto 70
       goto 30
    end if

    ! *** Is there a second subscript?

    if (line(iptr:iptr) == ',') then
       iptr = iptr + 1
       if (iptr > linelen) goto 80

       ! *** Ignore leading spaces of second subscript

40     continue
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          if (iptr > linelen) goto 80
          goto 40
       end if

       ! *** Extract and evaluate the second subscript

       negate = .false.

       ! *** Subscript may be prefaced by '+' or '-'

       if (line(iptr:iptr) == '+') then
          iptr = iptr + 1
          if (iptr > linelen) goto 80
       else if (line(iptr:iptr) == '-') then
          negate = .true.
          iptr = iptr + 1
          if (iptr > linelen) goto 80
       else
          continue
       end if
50     continue
       if ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) then
          isub2 = isub2 * 10 + ichar(line(iptr:iptr)) - izero
          iptr = iptr + 1
          if (iptr > linelen) goto 80
          goto 50
       end if

       ! *** Is it a negative subscript?

       if (negate) isub2 = -isub2

       ! *** Ignore trailing spaces of second subscript

60     continue
       if (line(iptr:iptr) == ' ') then
          iptr = iptr + 1
          if (iptr <= linelen) goto 60
       end if

    end if

    ! *** Must end with ')'

    if (line(iptr:iptr) /= ')') goto 80
    iptr = iptr + 1

70  continue
    icode = 0
    goto 1000

80  continue
    icode = 1

1000 continue

  end subroutine get_subscript

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine get_variable_name(varnam,varlen,isub1,isub2)

    !+ad_name  get_variable_name
    !+ad_summ  Routine that extracts a variable name from a line of
    !+ad_summ  the input file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  varnam : output string  : extracted variable name
    !+ad_args  varlen : output integer : length of variable name
    !+ad_args  isub1  : output integer : first subscript found
    !+ad_args  isub2  : output integer : second subscript found
    !+ad_desc  This routine extracts a variable name from the current line of
    !+ad_desc  the input file. It also extracts any subscripts present.
    !+ad_desc  On exit, the counter <CODE>iptr</CODE> points to the first
    !+ad_desc  character of the value to be assigned to the variable.
    !+ad_desc  If the routine finds an error a value of 0 is returned in
    !+ad_desc  variable <CODE>varlen</CODE>.
    !+ad_prob  None
    !+ad_call  get_subscript
    !+ad_call  upper_case
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(out) :: varlen, isub1, isub2
    character(len=*), intent(out) :: varnam

    !  Local variables

    character(len=80) :: line1
    integer :: ifrom,ito,icode

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! *** Store LINE in local variable

    line1 = line

    ! *** Convert string to upper case

    call upper_case(line)

    varlen = 0
    ifrom = iptr

    ! *** First character must be alphabetic

    if ((line(iptr:iptr) < 'A').or.(line(iptr:iptr) > 'Z')) goto 1000
    iptr = iptr + 1
    if (iptr > linelen) goto 1000

    ! *** Now parse the rest of the letters (must be alphanumeric or _ )

10  continue
    if ( ((line(iptr:iptr) >= 'A').and.(line(iptr:iptr) <= 'Z')).or. &
         ((line(iptr:iptr) == '_')).or. &
         ((line(iptr:iptr) >= '0').and.(line(iptr:iptr) <= '9')) ) then
       iptr = iptr + 1
       if (iptr <= linelen) goto 10
    end if

    ! *** Extract variable name

    ito = iptr - 1
    varlen = ito - ifrom + 1
    if (varlen > 0) varnam = line(ifrom:ito)

    ! *** Ignore intervening spaces

20  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 20
    end if

    ! *** Now extract any subscript

    call get_subscript(isub1,isub2,icode)
    if (icode /= 0) then
       varlen = 0
       goto 1000
    end if

    ! *** Ignore intervening spaces

30  continue
    if (line(iptr:iptr) == ' ') then
       iptr = iptr + 1
       if (iptr <= linelen) goto 30
    end if

    ! *** We now expect '='

    if (line(iptr:iptr) == '=') then
       iptr = iptr + 1

       ! *** Restore original string's upper/lower case after '=' sign

       line(iptr:linelen) = line1(iptr:linelen)

    else
       varlen = 0
    end if

1000 continue

  end subroutine get_variable_name

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine check_range_int(cvar,varval,min_value,max_value)

    !+ad_name  check_range_int
    !+ad_summ  Routine that checks whether an integer variable lies within
    !+ad_summ  the desired range
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer  : Fortran output unit identifier
    !+ad_args  cvar    : input string   : name of variable
    !+ad_args  varval  : input integer  : value of variable
    !+ad_args  min_value : input integer : minimum allowed value of variable
    !+ad_args  max_value : input integer : maximum allowed value of variable
    !+ad_desc  This routine checks whether an integer variable lies within
    !+ad_desc  the range predetermined by the user, and reports an error
    !+ad_desc  and stops if it doesn't.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_hist  13/04/11 PJK Improved error handling
    !+ad_hist  04/10/12 PJK Allowed min_value = max_value
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: cvar
    integer, intent(in) :: varval,min_value,max_value

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (min_value > max_value) then
       write(outfile,*) &
            'Illegal relative values of min_value and max_value'
       write(outfile,*) 'for variable ',cvar
       error_code = lineno
       error_routine = 'CHECK_RANGE_INT'
       error_message = 'Illegal min_value vs max_value'
       call report_error
    end if

    if ((varval < min_value).or.(varval > max_value)) then
       write(outfile,*) cvar,' lies outside its allowed range :'
       write(outfile,*) 'Minimum value = ',min_value
       write(outfile,*) 'Maximum value = ',max_value
       write(outfile,*) ' Actual value = ',varval
       error_code = lineno
       error_routine = 'CHECK_RANGE_INT'
       error_message = 'Variable range error'
       call report_error
    end if

  end subroutine check_range_int

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine check_range_real(cvar,varval,min_value,max_value)

    !+ad_name  check_range_real
    !+ad_summ  Routine that checks whether a real variable lies within
    !+ad_summ  the desired range
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  cvar    : input string   : name of variable
    !+ad_args  varval  : input real     : value of variable
    !+ad_args  min_value : input real   : minimum allowed value of variable
    !+ad_args  max_value : input real   : maximum allowed value of variable
    !+ad_desc  This routine checks whether a real variable lies within
    !+ad_desc  the range predetermined by the user, and reports an error
    !+ad_desc  and stops if it doesn't.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_hist  13/04/11 PJK Improved error handling
    !+ad_hist  04/10/12 PJK Allowed min_value = max_value
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(in) :: cvar
    real(kind(1.0D0)), intent(in) :: varval,min_value,max_value

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (min_value > max_value) then
       write(outfile,*) &
            'Illegal relative values of min_value and max_value'
       write(outfile,*) 'for variable ',cvar
       error_code = lineno
       error_routine = 'CHECK_RANGE_REAL'
       error_message = 'Illegal min_value vs max_value'
       call report_error
    end if

    if ((varval < min_value).or.(varval > max_value)) then
       write(outfile,*) cvar,' lies outside its allowed range :'
       write(outfile,*) 'Minimum value = ',min_value
       write(outfile,*) 'Maximum value = ',max_value
       write(outfile,*) ' Actual value = ',varval
       error_code = lineno
       error_routine = 'CHECK_RANGE_REAL'
       error_message = 'Variable range error'
       call report_error
    end if

  end subroutine check_range_real

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine upper_case(string,start,finish)

    !+ad_name  upper_case
    !+ad_summ  Routine that converts a (sub-)string to uppercase
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  string : input string   : character string of interest
    !+ad_args  start  : optional input integer  : starting character for conversion
    !+ad_args  finish : optional input integer  : final character for conversion
    !+ad_desc  This routine converts the specified section of a string
    !+ad_desc  to uppercase. By default, the whole string will be converted.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  05/01/04 PJK Initial F90 version
    !+ad_hist  12/04/11 PJK Made start,finish arguments optional
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=*), intent(inout) :: string
    integer, optional, intent(in) :: start,finish

    !  Local variables

    character(len=1) :: letter
    character(len=27) :: upptab = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_'
    integer :: loop, i

    integer :: first, last

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (present(start)) then
       first = start
    else
       first = 1
    end if

    if (present(finish)) then
       last = finish
    else
       last = len(string)
    end if

    if (first <= last) then
       do loop = first,last
          letter = string(loop:loop)
          i = index('abcdefghijklmnopqrstuvwxyz_',letter)
          if (i > 0) string(loop:loop) = upptab(i:i)
       end do
    end if

  end subroutine upper_case

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine report_error

    !+ad_name  report_error
    !+ad_summ  Reports an error and stops the program
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine is called if an error has been detected, and
    !+ad_desc  it reports the value of <CODE>error_code</CODE> and the
    !+ad_desc  user-supplied error message, and stops the program.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  03/10/12 PJK Initial version
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    write(*,*) 'Error trapped...'
    write(*,*) 'Routine ',trim(error_routine),': ',trim(error_message)
    write(*,*) 'Error Code: ',error_code

    stop 1

  end subroutine report_error

end module process_input

#ifdef unit_test
program test
  use process_input
  implicit none

  open(unit=1,file='IN.DAT',status='old')
  call parse_input_file(1,6,1)
  close(unit=1)
end program test
#endif
