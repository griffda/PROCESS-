# 1.0.17

> next release
> 
## Bug Fixes
 

## Features
- Restored IFE (Issue #901)
- GitLab pages now hosts the autodoc html output (only develop) (issue #418)
- Git branch now in output file (#912)
- CI jobs now run for cmake custom targets
- Added diamagnetic and Pfirsch-Schlüter current scalings #992

## Minor Changes
- Renamed 'test_files' to 'unit_tests' #972

## Documentation update


# 1.0.16

## Bug Fixes
 
- Fixed units issue with Lang et al. (2012) confinement scaling (#821)
- Fixed issue with error numbering (#826)
- Fixed issue with costing of TF coil dump resistors (#847)

## Features

- new command line argument `./process.exe help` provides help info
- new CMake option `-Ddll=ON/OFF`. Default is `ON`. Useful for profiling with gprof 
- Updated version of Kallenbach testing (now can run test case that matches Kallenbach 
  paper or user defined inputs). See Userguide for more info.
- Updated version of Kallenbach scanning (now can specify the variable to scan 
  from a selection, number of scan points etc.). See Userguide for more info.
- Added NSTX and NSTX-Petty08 confinement time scalings (#820)
- Added option to input the confinement time
- Added a new spherical tokamak plasma current relation based on FIESTA fitting
- CI now runs on all branches named "issue-*"
- Unit tests incorporated into main branch

## Minor Changes

- Increased the number of scan points to 1000 (issue #809)
- For issue #379 constraint 52 now gives warning for iblanket=1
- Updated FNSF test case (#822)
- Removed obsolete variable estotf (#199 #847)

## Documentation update

- Developper documentation update (code description/compilation/git instructions) 

# 1.0.15

## Bug Fixes
- Added emultmw calculation to stellarator and fixed power balance errors (Issue #783)
- Amended fpump* output to match with primary_pumping options.
- Corrected power crossing the separatrix for stellarators (Issue #787)
- Changed Connor-Hastie plasma current model to kappa95 and triang95% (Issue #791)

## Features
- HTS REBCO model final version implemented
- Can now limit the CS peak field to be below set maximum
- Added Hubbard 2012 and 2017 I-mode threshold scaling
- Added Hubbard I mode confinement time scaling
- Added I-mode version of Reinke criterion (fzmin)
- New figure of linear combination figure of merit. Linear combination (50/50 
  weighted) of $`Q`$ and $`t_{burn}`$.
- I mode scalings for confinement time and L-I power threshold from Hubbard 2017.
- New utility called `plot_profiles.py`. Plots T and n profiles for a list of given MFILES.
- Can now setup the repo in `debug` mode for compilation. See `README.md` for instructions.
- New scan variables - `impurity_ratio(9)` and `fgwsep`.
- New constraint on CS peak field.

## Minor Changes
- Explicitly state 1990 $ for old cost model
- Made photon_wall and rad_fraction global variables, and added calculations to stellarator. 
- TF coil documentation now in repository and makefile target `tfdoc`.

# 1.0.14

## Bug Fixes

- Wrong pedestal position used in plot_proc temperature plot (Issue #653) ([957f94a7](https://git.ccfe.ac.uk/process/process/commit/957f94a723b026f67544fa46548bc8a1be062d35))
- Removed hardwired Martin scaling for L-H threshold in plot_proc.py (Issue #679 and #680)
- Fixed error in spherical tokamak divertor geometry calculation (Issue #697)
- Fixed error in spherical tokamak radial build calculation (Issue #704)
- Fixed error in current drive fractions adding to > 1 (Issue #705).
- Fixed issues with uncertainty python utility (Issue #716 #746)
- Fixed issues when there is no inboard blanket (Issue #722 #732)
- Fixed incorrect cross sectional area calculation in resistive TF coils (Issue #727)
- Fixed constraint equations plot in diagnose_process.py (Issue #738)
- Corrected units in resistive TF coil stress output
- Corrected units on ucme and uciac in global variables.
- Fixed issue with plot_proc.py scan counting (Issue #748)
- Fixed issue with run_process.py not working (Issue #766)
- Switched obsolete estotf for estotftgj in stellarator 
- Corrected ztot calculation in tfpwr subroutine for resistive TF coils (#773)
- Corrected deltf in sctfcoil.f90 (#779)

## Features

PLASMOD
 - PLASMOD is a 1D transport model which replaces many of the plasma physics calculations in PROCESS. The previous set up remains available.
 - See reference: E. Fable et al., Fusion Engineering and Design, Volume 130, May 2018, Pages 131-136
 - PLASMOD can be run during every PROCESS iteration by setting ipedestal to 3. It can be run just once, at the end of a PROCESS run by setting ipedestal to 2.

 - Created a new file 'physics_functions.f90' to store code moved from physics.f90 which may be used by PLASMOD and other semi-independent models.
 - This is to prevent circular dependencies.
 - Subroutines include: beamcalc, beamfus, imprad, palph, palph2, prad_ipdg89, psync_albajar_fidone, pthresh, radpwr
 - Functions include: bosch_hale, fsv, p_eped_scaling, t_eped_scaling,

 - New user-defined inputs have been added, which all have the prefix 'plasmod_'. These are specific controls and inputs to PLASMOD.
 - For a complete list, see the vardes file. Where appropriate, previously-existing PROCESS input parameters still apply.
 - Certain constraints and iterations variables cannot be used with PLASMOD - see the User Guide for more information.

2D Scan
 - implemented a basic 2-D scan feature in PROCESS.
 - new inputs `scan_dim`, `isweep_2`, `nsweep_2` and `sweep_2`.
 - Does the scan in a basic grid like manner (i.e. jumps to start of next row from end of previous). Would be nice to upgrade to 'zig-zag'-like approach.

Utilities
 - New script compare_radials.py to plot two radial profiles on the same chart for comparison. Takes input columns of data representing the profiles, with the first column being the x-axis, e.g. radial position.
 - evaluate_uncertainties.py now outputs and additional file to allow analysis of failed PROCESS Runs.
 - New script plot_sankey.py to plot a Sankey diagram of the PROCESS power flow
 - New scripts cost_pie.py and cost_bar.py to analyse cost data.
 - New script popcon.py to plot POPCON plot from MFILE.

Miscellaneous
- TF stress in conduit Tresca criterion can now have regular and CEA adjusted options 
  (adjustment from [Torre et al. 2016](https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=7390035) 
  paper). (Issue #678)
- [Snipes et al.](2000; http://iopscience.iop.org/article/10.1088/0741-3335/42/5A/336) H-mode threshold scaling options added (Issue #680)
- Initial version of `.gitlab-ci.yml` created for GitLab CI.
- Added Spherical Tokamak and Stellarator examples to the test suite (Issues #715 and #718)
- Output to MFILE variable names for cost models
- Added [Reinke detachment criterion](http://iopscience.iop.org/article/10.1088/1741-4326/aa5145/meta) as constraint equation and formula for tesep (Issue #707)

## Minor changes

- Changed upper bound on `coheof` from 1.0e8 to 5.0e8 (Issue #668).
- A number of changes to `plot_proc.py` and outputs in the fortran associated 
  with vertical build. (Merge request !18)
- Update utilities guide for a number of Python utilities
    - `cad_output.py` (Issue #671)
    - `convert_in_dat.py` (Issue #672)
    - `mcnp_output.py` (Issue #674)
    - `output_summary.py` and `output_detailed.py` (Issue #675)
    - `plot_comparison.py` (Merge request !21)
- New Python utility
    - `plot_comparison.py` (Merge request !21)
- File prefixes for input files now works as intended. For example input file called `my_input_IN.DAT`
  will be outputted as `my_input_OUT.DAT` etc.
- `tbrnmn` no longer iteration variable as there is constraint equation 13 and f-value `ftburn` already. `tbrnmn` will act as the constraint limit input value.
- `cdtfleg` no longer an iteration variable.  The outboard leg current density is now calculated for resistive TF coils. (Issue #727)
- `tfacpd` is now calculted for resistive TF coils so is no longer an input.
- Reset test_suite files (Issue #719)
- Added error reporting to function ncore (Issue #735)
- Added input `plasma_res_factor` for adjustment factor for plasma resistivity. Default is 1.0   to preserve old behaviour.
- Added additional scaling factor 'eped_sf' for the EPED pedestal model (pressure and temperature versions).
- Slight change to functionality of utilities/write_new_in_dat.py: This script will no longer create a new IN.DAT from a non-feasible solution. If a scan is run, it will take by default the last feasible solution. If required there is also an option to use the first feasible solution from a scan (Issue #752).
- Made more robust the reading of input files - comments are now denoted only via an asterisk (*), and if a comment is present without an asterisk the reading of the input file will stop (previously it simply ignored constraint equations and iteration variables that could not be read). It is no longer permissible to write an input over multiple lines. Users can now use punctuation in comments as they wish, including full stops and commas.
- Stellarator radial build is output to MFILE (Issue #770)
