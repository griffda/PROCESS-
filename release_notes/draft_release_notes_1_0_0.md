# PROCESS Release Notes v1.0.0

## Version numbering

Moved to a different version numbering in PROCESS to give more granularity to the 
version number. From now on the version will vake the form `1.0.0`.

This will be similar to the .NET convention of version numbering

```
[major version].[minor version].[revision number]

[major version] - release containing numerous major changes

[minor version] - medium change, i.e. new model, major bug fix

[revision number] - weekly or on demand build/change
```

## Updates

| Update | Description | Issues |
| -------- | -------- | --------- |
| HCLL model | Include CEA HCLL model in process code | `#355` |
| Compiler change | Changed the default compiler from `ifort` to `gfortran`.| `#351` |
| Stellarator test | Added a Stellarator test file to the test suite | `#349` |
| A to B tool | Updated to pep8 standard and fixed bugs | `#191` |
| README | Update to README.md file in PROCESS and contains information about the code, how to build, run and other things. | |
| User guide | Fix for solver diagram | `#359` |
|  | Update to bashrc instructions | `#417` |
| Radiation model | Bremstrahlung power density now depends on `Te`  |  |
|  | Changed Z in plasma_composition to be dependent on `Te` |  |
|  | Changed log-log interpolation in `Zav_of_Te` to log-lin |  |
| Constraints | Removed constraint equation `38` - Equation for first wall coolant temperature rise upper limit | `#377` |
| Test Suite | OUT.DAT now saved to test area by default | `#357` |
| Plot_proc | Updated documentation | `#192` |
| Uncertainties | Fixed incorrect normalisation of half Gaussian distributions |  |
| PF Coils | Switch `iprecomp` added to allow user to switch off CS coil pre-compression structure calculation | `#434` |
| Confinement | Added Petty 2008 and Lang 2012 confinement scaling | `#43` |
|  |  |  |