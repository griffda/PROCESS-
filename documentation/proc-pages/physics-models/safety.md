# Safety and Environment Models

At present, the neutronics, activation and inventory calculations compromise the safety and environment models in the code.

The models compromising the safety and environmental calculations[^1] within the code are all called from routine `FISPAC`. They are only performed once, at the end of each run, as they take a relatively lone time to evaluate, and the results are only used for diagnostic purposes - no constraints are imposed at present to minimise doses, for instance.

N.B. These models are currently not available in the present version of the code.

## Neutronics

The neutronics module predicts the neutron flux spectra in the inboard and outboard first wall and blanket components. The spectra are based in a simplified tokamak device that has a fixed ratio (=1.5825) between the outboard blanket thickness and the inboard blanket thickness, and are scaled according to the actual thickness of the outboard blanket. This relatively limited, single-parameter approach is expected to be replaced by a more general method, which should allow a more accurate portrayal of the device being modelled by `PROCESS`.

## Activation and inventory information

The code evaluates the consequences of exposing the power plant's materials to the calculated neutron fluxes, subject to the limitations imposed by the neutronics model. A library of neutron cross-sections and decay data is used to calculate the total activity, gamma-ray dose rate and decay heat output due to the materials' exposure to neutrons, both at the end of the plant's life and at a time 100 years later. These values are relevant to decommissioning and disposal studies, and additional parameter that can be obtained from the nuclide inventory will also by included as the need arises.

[^1]: N. P. Taylor, R. A. Forrest, P. J. Knight and L. J. Baker, *"Safety and Environmental Modelling in the PROCESS Code"*, Strategic Studies Note 94/14 (1994)