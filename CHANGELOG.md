# 1.0.14

> Next release

## Bug Fixes

- Wrong pedestal position used in plot_proc temperature plot (Issue #653) ([957f94a7](https://git.ccfe.ac.uk/process/process/commit/957f94a723b026f67544fa46548bc8a1be062d35))

## Features

PLASMOD
 - add all changes to PROCESS from PLASMOD here
 - Created a new file 'physics_functions.f90' to store code moved from physics.f90 which may be used by PLASMOD and other semi-independent models. 
 - This is to prevent circular dependencies. 
 - Subroutines include: beamcalc, beamfus, imprad, palph, palph2, prad_ipdg89, psync_albajar_fidone, pthresh, radpwr
 - Functions include: bosch_hale, fsv, p_eped_scaling, t_eped_scaling,

## Minor changes

- Changed upper bound on `coheof` from 1e8 to 5e8 (Issue #668).
- A number of changes to `plot_proc.py` and outputs in the fortran associated 
  with vertical build. (Merge requst !18)
- Update utilities guide for a number of Python utilities
    - `cad_output.py` (Issue #671)
    - `convert_in_dat.py` (Issue #672)
    - `mcnp_output.py` (Issue #674)
    - `output_summary.py` and `output_detailed.py` (Issue #675)