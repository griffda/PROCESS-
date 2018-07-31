# 1.0.14

> Next release

## Bug Fixes

- Wrong pedestal position used in plot_proc temperature plot (Issue #653) ([957f94a7](https://git.ccfe.ac.uk/process/process/commit/957f94a723b026f67544fa46548bc8a1be062d35))
- Removed hardwired Martin scaling for L-H threshold in plot_proc.py (Issue #679 and #680)
- Fixed error in spherical tokamak divertor geometry calculation (Issue #697)
- Fixed error in spherical tokamak radial build calculation (Issue #704)
- Fixed error in current drive fractions adding to > 1 (Issue #705).
- Fixed issues with uncertainty python utility (Issue #716)
- Fixed issues when there is no inboard blanket (Issue #722 #732)
- Fixed incorrect cross sectional area calculation in resistive TF coils (Issue #727)
- Fixed constraint equations plot in diagnose_process.py (Issue #738)
- Corrected units in resistive TF coil stress output
- Corrected units on ucme and uciac in global variables.

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

Utilities
	- New script compare_radials.py to plot two radial profiles on the same chart for comparison. Takes input columns of data representing the profiles, with the first column being the x-axis, e.g. radial position.
	- evaluate_uncertainties.py now outputs and additional file to allow analysis of failed PROCESS Runs.

- TF stress in conduit Tresca criterion can now have regular and CEA adjusted options 
  (adjustment from [Torre et al. 2016](https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=7390035) 
  paper). (Issue #678)
- [Snipes et al.](2000; http://iopscience.iop.org/article/10.1088/0741-3335/42/5A/336) H-mode threshold scaling options added (Issue #680)
- Initial version of `.gitlab-ci.yml` created for GitLab CI.
- Added Spherical Tokamak and Stellarator examples to the test suite (Issues #715 and #718)


## Minor changes

- Changed upper bound on `coheof` from 1e8 to 5e8 (Issue #668).
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