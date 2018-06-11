# 1.0.14

> Next release

## Bug Fixes

- Wrong pedestal position used in plot_proc temperature plot (Issue #653) ([957f94a7](https://git.ccfe.ac.uk/process/process/commit/957f94a723b026f67544fa46548bc8a1be062d35))
- Removed hardwired Martin scaling for L-H threshold in plot_proc.py (Issue #679 and #680)
- Fixed error in spherical tokamak divertor geometry calculation (Issue #697)
- Fixed error in spherical tokamak radial build calculation (Issue #704)
- Fixed error in current drive fractions adding to > 1 (Issue #705).
- Fixed issues with uncertainty python utility (Issue #716)

## Features

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