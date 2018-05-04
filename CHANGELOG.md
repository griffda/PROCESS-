# 1.0.14

> Next release

## Bug Fixes

- Wrong pedestal position used in plot_proc temperature plot (Issue #653) ([957f94a7](https://git.ccfe.ac.uk/process/process/commit/957f94a723b026f67544fa46548bc8a1be062d35))
- Removed hardwired Martin scaling for L-H threshold in plot_proc.py (Issue #679 and #680)

## Features

- TF stress in conduit Tresca criterion can now have regular and CEA adjusted options 
  (adjustment from [Torre et al. 2016](https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=7390035) 
  paper). (Issue #678)
- [Snipes et al.](2000; http://iopscience.iop.org/article/10.1088/0741-3335/42/5A/336) H-mode threshold scaling options added (Issue #680)

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