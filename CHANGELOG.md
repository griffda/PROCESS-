# 1.0.14

> Next release

## Bug Fixes

- Wrong pedestal position used in plot_proc temperature plot (Issue #653) ([957f94a7](https://git.ccfe.ac.uk/process/process/commit/957f94a723b026f67544fa46548bc8a1be062d35))

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

## Minor changes

- Changed upper bound on `coheof` from 1e8 to 5e8 (Issue #668).
- A number of changes to `plot_proc.py` and outputs in the fortran associated 
  with vertical build. (Merge requst !18)
- Update utilities guide for a number of Python utilities
    - `cad_output.py` (Issue #671)
    - `convert_in_dat.py` (Issue #672)
    - `mcnp_output.py` (Issue #674)
    - `output_summary.py` and `output_detailed.py` (Issue #675)
