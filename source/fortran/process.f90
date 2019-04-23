 ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

program process

  !+ad_name  process
  !+ad_summ  Power Reactor Optimisation Code for Environmental and Safety Studies
  !+ad_type  Main program
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  M Kumar, CCFE, Culham Science Centre
  !+ad_cont  N/A
  !+ad_args  None
  !+ad_desc  Power Reactor Optimisation Code for Environmental and Safety Studies
  !+ad_desc  <P>This is a systems code that evaluates various physics and
  !+ad_desc  engineering aspects of a fusion power plant subject to given
  !+ad_desc  constraints, and can optimise these parameters by minimising
  !+ad_desc  or maximising a function of them, such as the fusion power or
  !+ad_desc  cost of electricity.
  !+ad_desc  <P>This program is derived from the TETRA and STORAC codes produced by
  !+ad_desc  Oak Ridge National Laboratory, Tennessee, USA. The main authors in
  !+ad_desc  the USA were J.D.Galambos and P.C.Shipe.
  !+ad_desc  <P>The code was transferred to Culham Laboratory, Oxfordshire, UK, in
  !+ad_desc  April 1992, and the physics models were updated by P.J.Knight to
  !+ad_desc  include the findings of the Culham reactor studies documented in
  !+ad_desc  Culham Report AEA FUS 172 (1992). The standard of the Fortran has
  !+ad_desc  been thoroughly upgraded since that time, and a number of additional
  !+ad_desc  models have been added.
  !+ad_desc  <P>During 2012, PROCESS was upgraded from FORTRAN 77 to Fortran 95,
  !+ad_desc  to facilitate the restructuring of the code into proper modules
  !+ad_desc  (with all the benefits that modern software practices bring), and to
  !+ad_desc  aid the inclusion of more advanced physics and engineering models under
  !+ad_desc  development as part of a number of EFDA-sponsored collaborations.
  !+ad_prob  None
  !+ad_call  numerics
  !+ad_call  error_handling
  !+ad_call  global_variables
  !+ad_call  process_input
  !+ad_call  process_output
  !+ad_call  scan_module
  !+ad_call  eqslv
  !+ad_call  final
  !+ad_call  init
  !+ad_call  oheadr
  !+ad_call  scan
  !+ad_call  show_errors
  !+ad_hist  03/10/96 PJK Upgrade of main program unit
  !+ad_hist  08/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  09/10/12 PJK Modified to use scan_module
  !+ad_hist  10/10/12 PJK Modified to use numerics module
  !+ad_hist  06/11/12 PJK Renamed this source file from aamain.f90 to process.f90.
  !+ad_hisc               Transferred routine inform from aachange.f90
  !+ad_hist  13/02/14 PJK Added mfile close statement
  !+ad_hist  10/09/14 PJK Added vfile close statement
  !+ad_hist  28/10/16 MK  Removed systems commands and added a subroutine
  !+ad_hist               get_DDMonYYTimeZone to get date and time
  !+ad_hist  04/11/16 MK  Added check for existence of input file
  !+ad_hist  03/02/17 JM  Fixed input file existence check, now fileprefix defined before init
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !+ad_docs  Box file F/RS/CIRE5523/PWF (up to 15/01/96)
  !+ad_docs  Box file F/MI/PJK/PROCESS and F/PL/PJK/PROCESS (15/01/96 to 24/01/12)
  !+ad_docs  Box file T&amp;M/PKNIGHT/PROCESS (from 24/01/12)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use error_handling
  use global_variables
  use process_input
  use process_output
  use scan_module
  use numerics
  use divertor_Kallenbach_variables, only: kallenbach_tests, kallenbach_scan_switch
  use main_module
!  use output_module
  use init_module
  use final_module
  use kallenbach_module
  use hare, only: hare_calc
  use mode
  implicit none

  !  Arguments

  !  Local variables
  integer :: ifail
  character(len = 130) :: line
  character(len = 10)  :: fmtAppend
  character(len = 50) :: inFile
  character(len = 50) :: outFile
  integer :: iost
  logical :: inExist
  integer :: nargs
  integer :: file_name_length
  integer, parameter :: width = 110

  !  Obtain a file prefix from a command line argument
  !  (uses Fortran 2003 routines)
  nargs = command_argument_count()

  if (nargs == 0) then
     fileprefix = ''
  else
     call get_command_argument(1, fileprefix)
  end if

  if (trim(fileprefix) == "help") then
    call oblnkl(iotty)
    call ostars(iotty, width)
    call ocentr(iotty,'PROCESS', width)
    call ocentr(iotty,'Power Reactor Optimisation Code', width)
    call ostars(iotty, width)

    ! Usage help
    call osubhd(iotty,'# Usage')
    call ocmmnt(iotty, "Running code with IN.DAT        : ./<path_to_executable/process.exe")
    call ocmmnt(iotty, "Running code with named IN.DAT  : ./<path_to_executable/process.exe <path_to_input>/<file_prefix>IN.DAT")
    call oblnkl(iotty)
    call ocmmnt(iotty, "Help info                       : ./<path_to_executable/process.exe help")
    call oblnkl(iotty)
    call ocmmnt(iotty, "## Example Usage")
    call oblnkl(iotty)
    call ocmmnt(iotty, "Executable in current dir and input called IN.DAT in current dir  : ./process.exe")
    call ocmmnt(iotty, "Executable in current dir and named input in current dir          : ./process.exe tokamak_IN.DAT")
    call ocmmnt(iotty, "Executable in other dir and named input in other dir              : ./bin/process.exe ../../ITER_IN.DAT")
    call ocmmnt(iotty, "Executable in other dir and input called IN.DAT in current dir    : ./bin/process.exe")

    ! Input help
    call osubhd(iotty,'# Input')
    call ocmmnt(iotty, "Input file naming convention : <file_prefix>IN.DAT")
    call oblnkl(iotty)
    call ocmmnt(iotty, "## Input file syntax")
    call oblnkl(iotty)
    call ocmmnt(iotty, "Constraint equation             : icc = <constraint_number>")
    call ocmmnt(iotty, "Iteration variable              : ixc = <iteration_variable_number>")
    call ocmmnt(iotty, "Iteration variable lower bound  : boundl(<iteration_variable_number>) = <bound_value>")
    call ocmmnt(iotty, "Iteration variable upper bound  : boundu(<iteration_variable_number>) = <bound_value>")
    call ocmmnt(iotty, "Parameter                       : <parameter_name> = <parameter_value>")
    call ocmmnt(iotty, "Array                           : <array_name>(<array_index>) = <index_value>")

    ! Output help
    call osubhd(iotty,'# Output')
    call ocmmnt(iotty, "Output files naming convention : <file_prefix>OUT.DAT")
    call ocmmnt(iotty, "                               : <file_prefix>MFILE.DAT")
    call ocmmnt(iotty, "                               : <file_prefix>PLOT.DAT")

    ! Contact info
    call osubhd(iotty,'# Contact')
    call ocmmnt(iotty, "James Morris  : james.morris2@ukaea.uk")
    call ocmmnt(iotty, "Hanni Lux     : hanni.lux@ukaea.uk")
    call ocmmnt(iotty, "GitLab        : git.ccfe.ac.uk")
    call oblnkl(iotty)
    stop
  else if (trim(fileprefix) == "") then
    inFile = "IN.DAT"
  else
    file_name_length = LEN_TRIM(fileprefix)
    output_prefix = fileprefix(1:file_name_length-6)
    inFile = trim(fileprefix)
  end if
  outFile = trim(output_prefix)//"OUT.DAT"
  inquire(file = inFile, exist = inExist)

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  mainRun : if (inExist) then

    !  Initialise
    call init    

    ! Run built-in tests.
    ! These are distinct from the tests that are dependent on 'unit_test'.
    if (run_tests == 1) call runtests

    if(kallenbach_tests == 1) then
      call kallenbach_testing()
      call exit(0)
    endif

    if(kallenbach_scan_switch == 1) then
      call kallenbach_scan()
      call exit(0)
    endif

     ! Call equation solver (HYBRD)
    call eqslv(ifail)

     ! Call routine to do scans
    if (ioptimz >= 0) then
       call scan
    else
       call final(ifail)
    end if

    call show_errors

    call oheadr(nout,'End of PROCESS Output')
    call oheadr(iotty,'End of PROCESS Output')
    call oheadr(nout,'Copy of PROCESS Input Follows')

    close(unit = nin)
    close(unit = nout)
    close(unit = nplot)
    close(unit = mfile)
    if (verbose == 1) close(unit = vfile)

    open(unit = 100, FILE = inFile)
    open(unit = 101, FILE = outFile, ACCESS = "append")
    open(unit = 102, FILE=trim(output_prefix)//'MFILE.DAT', ACCESS = "append")
    fmtAppend = '(A)'
    write(102, fmtAppend) "***********************************************"

    DO
      read(100, fmtAppend, IOSTAT = iost) line
      if(iost < 0) exit                   ! exit if End of line is reached in IN.DAT
      write(101, fmtAppend) trim(line)
      write(102, fmtAppend) trim(line)
    END DO
    close(unit = 100)
    close(unit = 101)
    close(unit = 102)

  else mainRun

    write(*, *) "There is no input file named"//inFile//" in the analysis folder"

  end if mainRun

end program process

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! SVN 145: New CICC plots for User Guide
! SVN 149: MGF power usage correction
! SVN 150: Machine parameters use Fortran intrinsics
! SVN 151: Minor comment changes relating to coils, powers, buildings
! SVN 152: Changes to vacuum vessel and cryostat dimensions
! SVN 153: Corrected maximum crane lift requirement
! SVN 154: Building volume multipliers now input parameters;
!          corrected maintenance building height calculation
! SVN 155: Corrected blanket and shield height calculations;
!          clarified 'inner' to 'inboard', and 'outer' to 'outboard' throughout
! SVN 156: Modified case and winding pack thicknesses for TF nuclear heating calcs.
! SVN 157: Removed some obsolete variables, changed some default switch settings;
!          changed beryllium density
! SVN 158: Removed energy storage building if lpulse=0
! SVN 159: Corrected in-code comments about tfckw
! SVN 160: Corrected use of fhole in pulse.f90 (improved first wall nuclear heating
!          calculation)
! SVN 161: PF coil case area modified for superconducting coils; new sigpfcf input
! SVN 162: sigpfcalw added, replacing sigpfalw in PF coil case area calculation
! SVN 163: Modified isumattf usage; implemented Richard Kemp's corrections to itersc
! SVN 164: Removed cohbof; fcohbof no longer an input parameter
! SVN 165: Corrected various power conversion values
! SVN 166: Added new switch iprimnloss to control destiny of pnucloss
! SVN 167: New switch fwbsshape to control first wall, blanket, shield and
!          vacuum vessel cross-sectional shape; volume and area calculations for
!          these fully updated
! SVN 168: Swapped build order of vacuum vessel and adjacent gaps
! SVN 169: Added new KIT blanket neutronics model (User Guide still to do)
! SVN 170: Updated User Guide to describe new blanket model
! SVN 171: Top/bottom shield thickness now calculated if new blanket model is in use
! SVN 172: Produced in-source comments for new blanket model; added a number of
!          requested outputs. New ISHAPE=2 option for elongation scaling with aspect ratio
! SVN 173: Corrected rlp, ipdot, tohsmn calculations; changed units for qfuel, rndfuel;
!          added several more requested outputs
! SVN 174: Corrected reactor building height and vacuum vessel mass calculations;
!          removed dign; added section to User Guide about requirements for new models;
!          modified/clarified various comments
! SVN 175: Removed obsolete TF coil current density equation 23
! SVN 176: Corrected NBI path length calculation
! SVN 177: Minor changes to allow compilation with gfortran (4.6.3 - won't compile with
!          gfortran 4.4.5...)
! SVN 178: Correction to cryostat radius used in buildings call; modified swing time
!          comments; removed 'Troyon' descriptor for tokamak beta limits
! SVN 179: Updated plotting utilities (requires python 2.7.3 or higher)
! SVN 180: Corrected long-standing niggle with zeffai formula; now matches description
! SVN 181: Modified numerics output hints for optimising runs
! SVN 182: New stellarator plasma geometry and divertor models incorporated
! SVN 183: Comment changes
! SVN 184: Fixed a number of discrepancies in the D-He3 model, although the fusion
!          power calculations use different fits for D-T fusion to Bosch-Hale
! SVN 185: Rationalised fusion power calculations to use Bosch-Hale parametrization
!          in all cases; no iiter or idhe3 switches; D-He3 reaction is now controlled
!          via fhe3 only
! SVN 186: Removed obsolete fusion power routines; fixed problem with betaft if fdeut=1
! SVN 187: New port size (beam tangency radius) calculation
! SVN 188: Modified output formatting for reporting of arrays set in input file
! SVN 189: Incorporated Fabrizio Franza's suggested revisions to the KIT blanket model
! SVN 190: Added Psep/R limit equation (no.56)
! SVN 191: Re-assigned isumattf=2 to new Bi-2212 high-temperature superconductor model
! SVN 192: Minor adjustment of multiplier in beta calculations for consistency
! SVN 193: Correction to fast neutron flux profiles in VV in KIT blanket model
! SVN 194: Added fwareaib, fwareaob to calculations in stfwbs
! SVN 195: Modified stellarator blanket thicknesses consistently with KIT blanket model
! SVN 196/7 : Modified TF coil case mass calculation
! SVN 198: Corrected thermal energy outputs by 3/2 factor
! SVN 199: Unified kappa95 definition to be kappa/1.12
! SVN 200: Removed obscure upper limit on divertor null-to-strike distance;
!          Changed 'breeding unit' to 'breeding zone' in KIT blanket model;
!          Raised tftort upper limit
! SVN 201: Changed boundl(25: fpnetel) to 0.001 from 1.0
! SVN 202: Changed some other unusual boundl, boundu values
! SVN 203: Fix in vacuum.f90 to remove runtime error problem
! SVN 204: Fix in induct routine to prevent problems if ncls(1)=1
! SVN 205: Improved Central Solenoid self inductance calculation, and Central Solenoid to
!          plasma mutual inductance
! SVN 206: New output taup/taueff;
!          Erroneous decimal points present within input lines for integer variables
!          are now discarded with a warning message;
!          Added information about scanning variable to output file
! SVN 207: Lowered minimum input value for ralpne to 1.0D-12;
!          Added theat effects to flux consumption calculations
! SVN 208: Clarified usage of in-line comments in input file
! SVN 209: Added ohmic power to bigq denominator;
!          Modified poloidal field calculation for conventional tokamaks;
!          Moved pfrmax, pfmmax calculations for tokamaks into PF coil module;
!          Added Psep/R to output variables in PLOT.DAT
! SVN 210: Modified TF outboard leg calculation for resistive coils
! SVN 211: New scanning variable 27: tbrmin
! SVN 212: New iteration variable 98: li6enrich
! SVN 213: Fusion power for each fuel ion pair now output separately
! SVN 214: Current profile consistency option (iprofile=1) introduced
! SVN 215: Modified LSA usage in first wall costs;
!          Added argument to constraints to give the option of evaluating only a single
!          chosen constraint equation rather than all of them
! SVN 216: Fixed error with previous version; nvrbl --> nvar
! SVN 217: Typo fix in manual
! SVN 218: Changed epsfcn description
! SVN 219: Improved initialisation by calling 'caller' twice at the start of a run
!          and modifying some initial estimates for quantities. Added unit testing
!          code for VMCON
! SVN 220: New figure of merit cfactr; new scanning variable bt
! SVN 221: Constraint (limit) equations made uniform in style
! SVN 222: Minor mods to prevent gfortran compilation errors. Also added tratio usage
!          to calculate ti from te for stellarators
! SVN 223: Added new output channel mfile (MFILE.DAT) to write out machine-readable
!          data. Also changed space characters in PLOT.DAT to underscores.
! SVN 224: VMCON ifail flag now written to output under all circumstances
! SVN 225: Updated rkemp's python utilities
! SVN 226: Added hlux's PROCESS_dicts.py utility;
!          Modified code output for costs and machine build, particularly to MFILE.DAT
! SVN 227: Draft implementation of pedestal profiles (use ipedestal=1)
! SVN 228: Fixed alphap problem in culbst
! SVN 229: Corrected misunderstanding about pressure profiles introduced at SVN 227
! SVN 230: Removed echoing of long lines in the input file to standard output
! SVN 231: Added use of general plasma profiles into current drive module.
!          Rationalised (simplified) argument lists for current drive routines.
!          Moved plasma profile routines into new source file and module to prevent
!          circular compilation issues.
! SVN 232: Fixed Id problem in plasma_profiles.f90
! SVN 233: Fixed error in tcore calculation
! SVN 234: Added verbose switch, plus diagnostic output in maths_library.f90;
!          VMCON line search now exits and restarts if conditions appear unfavourable
! SVN 235: Typo fix in maths_library.f90;
!          Added mkovari's write_constraints.py utility
! SVN 236: Minor additions to User Guide;
!          Trapped problem with port size calculation if coil spacing is too narrow;
!          Trapped probable negative square root argument if Central Solenoid is very wide
! SVN 237: Used HYBRD throughout instead of HYBRID;
!          Added new argument niter to VMCON;
!          Added lists of figures and tables to User Guide
! SVN 238: Added constraints 57, 58, and iteration variables 99, 100
! SVN 239: Updated PROCESS_dicts.py
! SVN 240: Modified code to allow usage of inequality constraints in the future
! SVN 241: Minor output modifications
! SVN 242: Incorporated the new stellarator coil model;
!          Updated the stellarator description in the User Guide;
!          Moved a few maths utility routines into maths_library.f90;
!          Fixed problem with fusion reaction rate if temperature = zero;
!          Modified a few comments and added central profile values to output;
!          Corrected D-D reaction rates
! SVN 243: Minor comment changes; User Guide stellarator wording changes (F Warmer)
! SVN 244: Output of floats to mfile now always in scientific 'E' format;
!          Warning added if isumattf=2 range of validity is not upheld;
!          Warning about pdivt = 0.001 added;
!          Clarified ishape effects on kappa, triang
! SVN 245: Added references to STAR Code formulae
! SVN 246: Corrected tcore formula for pedestal profiles
! SVN 247: Added numerical state information to mfile
! SVN 248: Added Sauter et al bootstrap current fraction model
! SVN 249: Tidied up comments in Sauter et al model; added ibss=4 to User Guide;
!          Added run-time info, PF coil and TF coil geometry to mfile
! SVN 250: Tidied up string output to MFILE.DAT;
!          Added new description of optimisation algorithm to User Guide
!          (N.B. LaTeX not working properly due to problem with flow diagram)
! SVN 251: Corrected problem with User Guide
! SVN 252: Added isumattf to mfile
! SVN 253: Added full list of python utilities to repository, plus a new write-up in the
!          User Guide
! SVN 254: Minor changes to write_constraints.py
! SVN 255: Modified tfleng calculation to use tfthko on outboard side
! SVN 256: Added vertical field calculation
! SVN 257: Correction to ensure final solution vector is consistent with results
!          in the rest of the output
! SVN 258: Ensured that all quantities are re-calculated, regardless of the iprint
!          value at each call. This exposed a problem in availability.f90 in which
!          cfactr was not being taken into account in the lifetime values written
!          to the output file.
!          Also uploaded latest write_new_in_dat.py, process_funcs.py
! SVN 259: Updated write_new_in_dat.py, and added plot_mfile_sweep.py;
!          Corrected one line in scan.f90;
!          Added new two-layer TF coil stress model (stress_model=1) (draft only);
!          Small change in definition of rbmax for superconducting tokamak TF coils
! SVN 260: Brought process_dicts.py up to date with current code
! SVN 261: Changed ripmax default value to 1.0 percent
! SVN 262: Clarified logic for gtscale, iprofile interaction
! SVN 263: Clarified energy multiplication vs fusion gain wording
! GIT 263b: Changed SVN keywords to be updated manually
! GIT 264: Fixed progver format problem
! GIT 265: Removed wpvf usage
! GIT 266: New draft of stress model; replaced itfmod and stress_model with tfc_model
! GIT 267: Updated TF coil picture and description in User Guide
! GIT 268: Modified constraint 28 by adding new input parameter bigqmin
! GIT 269: Changed ripmax description; changed taup calculation to use alpharate
!          instead of fusionrate
! GIT 270: Tidied up comments in tfcpwr
! GIT 271: Added radial strain in insulator
! GIT 272: Initial draft of new impurity radiation model
! GIT 273: Modified python utility headers
! GIT 274: Made corrections to Sauter bootstrap fraction formulae as suggested by Fable
! GIT 275: Minor corrections to python utilities; added hyperlinks to User Guide
! GIT 276: Increased length of output lines
! GIT 277: Added vstot to output; removed ffwal from iwalld=2 calculation
! GIT 278: Removed tburn consistency equation, and replaced it with an internal loop;
!          ensured tburn is not negative (warning given if insufficient volt-seconds)
! GIT 279: Clarified core radiation usage; new radiation power constraint eqn;
!          introduced iradloss switch; corrected falpha usage (at least partially)
! GIT 280: Added warning if impurity temperature is below tabulated values
! GIT 281: Added several clauses for ignite switch to ensure injected power is
!          treated as zero for steady state power balance calculations. The usage
!          of ignite is now thought to be fully consistent throughout the code.
! GIT 282: Changed names (and in some cases, units) of several power-related variables
! GIT 283: Added iteration variable 102, fimpvar
! GIT 284: Changed a few more power-related variable names
! GIT 285: Added new scan variable coreradius
! GIT 286: Corrections to tfc_model=2
! GIT 287: New power flow model
! GIT 288: Correction to process_dicts.py
! GIT 289: Raised input upper limit on fimpvar; updated python library files
! GIT 290: Minor fix to mfile.py
! GIT 291: Removed duplicate outputs from mfile; 'make clean' now deletes all html files
!          and the User Guide pdf file
! GIT 292: Added fimpvar as scan variable 30
! GIT 293: Fixed small errors/inconsistencies in new power flow model
! GIT 294: Added impdir to allow impurity radiation datafile directory to be specified
! GIT 295: New confinement time scaling law DS03 (no.39)
! GIT 296: New ripple amplitude calculation
! GIT 297: Simplified current drive calculations
! GIT 298: Removed output section controlling flags sect?? (they were never used anyway,
!          and now it is important that all sections are output otherwise the mfile
!          will be incomplete)
! GIT 299: Corrected wallmw units in output files
! GIT 300: Removed references to bucking cylinder; updated in_dat.py library utility;
!          Blanket top/bottom thickness now always calculated rather than input
! GIT 301: Update mfile.py, plot_proc_func.py
! GIT 302: Corrected wallmw calculation to account for gaps in first wall
! GIT 303: Preliminary modifications to fispact.f90 for its possible resurrection;
!          Draft implementation of error handling module
! GIT 304: Added a tolerance level for the constraint residuals to the VMCON
!          convergence criteria;
!          Added a possible remedy to help with VMCON ifail=5 results
! GIT 305: Error handling now reports only during output steps, not during intermediate
!          iterations
! GIT 306: Range-normalised iteration variable values added to mfile
! GIT 307: Raised maximum number of scan points to 200
! GIT 308: Updated process_funcs.py
! GIT 309: Modified output banner and run description handling
! GIT 310: Constraint residues summary now output in physical units
! GIT 311: Updated in_dat.py
! GIT 312: Added fix for negative ion density occurrences at low electron density
! GIT 313: Corrected neutron power deposition in first wall for pulsed plants using
!          ipowerflow=1. Uncommented error trap in routine cycles.
! GIT 314: TF coil toroidal thickness tftort now calculated instead of input for
!          tokamaks
! GIT 315: Changed TF coil outboard radial thickness calculation
! GIT 316: tfthko now equal to tfcth for tokamaks; improved TF coil conductor mass calculations
! GIT 317: Updated run_process.py, process_config.py, process_funcs.py, process_dicts.py,
!          write_new_in_dat.py, in_dat.py
! GIT 318: Removed obsolete variables, other minor tidy-ups
! GIT 319: Removed casfact; added some variables to output files; trapped nvar < neqns
! GIT 320: Set fshine to zero if it is negligible; updated process_funcs.py;
!          trapped insufficient numbers of specified ixc, icc elements
! GIT 321: Added diagnose_process.py utility + funcs; minor wording changes elsewhere.
! GIT 322: Error list now read in from a JSON file
! GIT 323: New peak TF with ripple calculation; modified ripple calculation applicability range
! GIT 324: New scaling for PF coil to cryostat lid clearance
! GIT 325: Updated impurity radiation datafiles
! GIT 326: Added additional power balance outputs
! GIT 327: Added verbose output to VFILE.DAT
! GIT 328: Added error traps to pedestal profile routines
! GIT 329: Added tfcryoarea in advance of a change to the steady state cryogenic load calculation
! GIT 330: Removed Myall TF coil stress model; tfc_model switch usage changed
! GIT (new_defaults branch): Changed default values to approximate ITER-FDR (ITER98) design
! GIT (new_defaults branch): Updated/re-ordered variable descriptions
! GIT 331: Updated plot_proc_func.py
! GIT 332: Attempted to clarify zref usage
! GIT 333: Minor comment changes
! GIT 334: Merged new_defaults branch into develop branch
! GIT 335: Introduced a_to_b python utility
! GIT 336: Root directory is set via Makefile and new shell script setrootdir
! GIT 337: Corrections to create_dicts.py
! GIT 338: Minor corrections arising from gfortran warnings/error reports; added
!          impuritydata files to repository
! GIT 339: Merged process_gui branch into develop branch
! GIT 340: Rearranged GUI directory contents; reworded parts of User Guide
! GIT 341: September 2014 Master Release
! GIT 342: Added more ishape options
! GIT 343: Added L-H power threshold constraint
! GIT 344: Fixed problem with create_dicts.py (need to remove *.f90*~ files before running);
!          added conducting shell radius to rminor ratio constraint
! GIT 345: New NBI shine-through fraction constraint
! GIT 346: Added NBI orbit loss fraction
! GIT 347: Clarified NBI comments
! GIT 348: Improved TF coil superconductor temperature margin calculation;
!          modified reporting of errors pertaining to the input file;
!          clarified comments and variable names in routine itersc
! GIT 349: Split User Guide over several .tex files
! GIT 350: PF/CS superconductor calculations now use same routines as for TF coils;
!          PF/CS coil case calculations clarified
! GIT 351: Central Solenoid changed to Central solenoid in output files
! GIT 352: Confinement times for H=1 instead of H=2 now output
! GIT 353: Corrections to NBI orbit loss power handling; User Guide figure updates
! GIT 354: Updated build diagrams in User Guide
! GIT 355: Minor modifications and extra outputs for superconductor current density calculations
! GIT 356: Clarified some comments in PF coil field and current density calculations
! GIT 357: Added aion as an output
! GIT 358: Changed default values for fcuohsu, vfohc
! GIT 359: Added CS temperature margin calculation plus constraint
! GIT 360: Updated plot_proc_func.py
! GIT 361: Added warnings if itersc variables are out of range
! GIT 362: Changed tdwell default value;
!          incorporated tburn/tcycle into cost of electricity calculation
! GIT 363: Added fkzohm elongation adjustment multiplier
! GIT 364: Modified iradloss usage
! GIT 365: Ensured that CS conductor area remains positive
! GIT 366: Removed confusing acs/aturn comments in superconpf; disabled separate ion and electron
!          power balance constraints
! GIT 367: Added output_costs switch to turn on/off costing information to file
! GIT 368: Minor changes to User Guide
! GIT 369: Updated in_dat.py
! GIT 370: Corrected dcond usage for resistive PF coils
! GIT (dev_charrington_bop): Ensured that blanket material fractions sum to 1.0;
!          Coolant type coolwh now set via blkttype (assumed same coolant in all regions)
! GIT 371: Minor changes to User Guide
! GIT 372: Updated plot_proc_func.py; added 'make all' option
! GIT 373: Modified pinjmw description in output; changed abktflnc, adivflnc default values
! GIT 374: Added new availability model
! GIT 375: Updated instructions in User Guide regarding code changes and documentation
! GIT 376: Merged latest changes from dev_availability; addition of git commands to User Guide
! GIT 377: Minor correction to numerics.f90 for gfortran compilation
! GIT 378 (dev_charrington_bop): Incorporated REFPROP calls into code for coolant fluid properties
! GIT 379: Modified FISPACT I/O to use formatted data files
! GIT 380: Post-merger with dev_uncertainties branch
! GIT 381: Changed pinjht description;
!          Changed Django library location for GUI
! GIT 382: Various GIT branches merged
! GIT 383: Various minor changes and availability model updated. See release notes t383
! GIT 384: Changes to TF magnets calculations and a few minor fixes. See release notes t384
! GIT 385: Fixed tagging error and see release notes t385
! GIT 386: Fixes for new powerflow. Notes to be released with r388
! GIT 387: More minor fixes for powerflow. Notes to be released with r388
! GIT 388: Minor fix to version number
! GIT 389: New release. See release notes.
! GIT 390: Rewrite of calc_u_unplanned_fwbs and calc_u_unplanned_divertor
! GIT 393: Issue #290 Improvements to thermohydraulic model of first wall.
! GIT 395: Rewrite to vacuum pump availability. New Binomial routine.
! GIT 396: New cost model complete.  J Shimwell parametric TBR model #195. #292, #293
! GIT 397: Issues dealt with now or previously: #301 #219 #244 #252 #255 #262 #264 #268 #269 #278 #294 #295 #284
! GIT 398: Tidy first wall and blanket thermohydraulics (#302), Append input file to output file (#305)
! GIT 399: Minimum total electrical power for primary coolant pumps (htpmw_min) (#303). The user now specifies the allowable von Mises stress for TFC and hoop stress for CS.
! GIT 400: Blanket fractions now defined using breeder_multiplier: combined breeder/multipler fraction. Steel is remainder. Cryogenics output added.
!          Corrected surface heat flux on first wall #309. Cost of electricity and maintenance cost now included in 2015 cost model.
! GIT 401: Add active_constraints(ipeqns) : logical array showing which constraints are active.
!          #308 L-H threshold power (enforced) is boundl(103)*plhthresh.
!          #306 Added central tube for helium coolant in TF cable, but these variables don't yet do anything.
!          #311 Added Murari energy confinement non-power law scaling (isc=40)
! GIT 402  #318 Update to ICC list in user guide
!          #316 plot_proc missing values from MFILE
!          #315 Add comment to user Guide that release notes should be included on the checklist for adding changes.
!          #314 Inconsistent input data for blanket model: change default vfpblkt = 0.1 to have a working default input blanket model.
!          #263 'tmargmin' should not be an iteration variable.  Set the label and vardes text for iteration variable 55 to "obsolete".
! GIT 403  #242 As we never use the divertor output, I will just switch it off.
!          #270 Add "ITV" to all iteration variable outputs, and
!          ensure that all iteration variables are output using ovarre or ovarin, except for f-values.
! GIT 404  #256 There is now a warning in the output file and to the terminal if the sweep variable is also an iteration variable.
!          #270 Quantities listed in standard format are labelled as follows in columns 112-114:
!               ITV : Active iteration variable (in any output blocks)
!               OP  : Calculated output quantity
!          Tweaked OUT.DAT in a few places.
!          #213 Make helium content an iteration variable and constrain tauP/tauE
! 405      #304 Add a very simple vacuum pump model (Section 1).
! 406      #325 New rule for Power supply cost.  May not be complete.
!          #327 Tweaks to make old cost model work
! 407      #304 Section 2 : pump-down model
! 408      #328 PF coil and CS cross-section and cost
!          New error reporting in input.f90.
! 409      #348 New first wall model
!          #329 Improved error handling in input.f90.
! 410      #341 New TF coil shape.
!          #347 More options for primary pumping power - primary_pumping
!          #326 Minor changes to descriptions and a page number in pfcoil
!          Replaced CS coil self-inductance formula - see benchmark.
!          (PS This isn't a very important quantity!)
!          #338 Output PF energy and current vs. time
! 411      Added maximum rate of change of PF energy as a constraint.
! 412      Master release: Checked recent changes using the test suite. Made a
!          few minor changes. Updated test function in plot_proc.
! 413      HCLL model now implemented. See milestone march 2016 for details.
! 1.0.0    Master release and update of versioning format. See release_notes_1_0_0.md
! 1.0.6    Version used for start of 2017 baseline work
! 1.0.7    Kallenbach model implemented but not fully tested
! 1.0.8    Changes included that were used for jan/feb 2017 baseline runs.
! 1.0.9    Time-dependent power reqs and simplified input file
