## PLASMOD

The PLASMOD[^1] code is a one-dimensional, time-independent particle and 
energy transport model, which has some basis in the ASTRA[^2] simulator. Even 
this simplified transport code leads to potentially much more accurate 
modelling of the fusion power generated when compared to the 0-D approximations 
found elsewhere in PROCESS. PLASMOD solves for a user-defined number of 
discrete points in the radial direction between the core of the plasma and the 
pedestal in order to create profiles which are not based on simple 
parameterizations.

Switch `ipedestal = 2` requests that PLASMOD be run *only after* PROCESS has 
found a feasible solution, whereas if `ipedestal = 3` then PLASMOD is called 
during every PROCESS iteration. It is recommended that `ipedestal = 3` be used, 
other than for testing purposes.

PLASMOD is run in conjunction with many PROCESS physics models, although certain 
constraints and iteration variables are not applicable when `ipedestal = 3`, as 
detailed in this section. Engineering models are unaffected, although 
naturally, results in all model-types are liable to change.

### PLASMOD inputs

User-defined inputs specific to PLASMOD have the prefix `plasmod\_` and should 
be added to the `IN.DAT`. These control various aspects of the transport 
simulation, from maximum number of iterations to be attempted to the current 
drive efficiency. For a complete list refer to the 
[variable description](/vardes/index.html) page.

### PLASMOD switches
Here is a summary of sub-models which may be accessed by the user:

- `plasmod_i_equiltype` -- Equilibrium model: 
    - `== 1` -- EMEQ, solve with sawteeth and inputted q95.
    - `== 2` -- EMEQ, solve with sawteeth and inputted Ip (not recommended).
- `plasmod_i_modeltype` -- Transport model: 
    - `== 1` -- Simple gyrobohm scaling with imposed H factor $>$ 1. 
    - `== 111` -- roughly calibrated to give H=1 for DEMO, but not fixed H.
    - `!= 1` or `!=111` -- Other values give H factor as output. 
- `plasmod_iprocess` -- Sub-functions:
    - `== 0` -- use PLASMOD functions
    - `== 1` -- use PROCESS functions.
- `plasmod_i_impmodel` -- Impurity model:
    - `== 0` -- fixed concentration
    - `== 1` -- fixed concentration at pedestal top, then fixed density.
- `plasmod_isawt` Sawtooth modelling:
    - `== 0` -- none
    - `== 1` -- solve with sawteeth.  

For more details refer to the vardes.html file.

### PROCESS physics models substituted by PLASMOD

This is a summary of key outputs from PLASMOD:

- Plasma geometry -- $\kappa$ and $\delta$
- Fusion reactions -- power from D + T, D + D,... interactions are calculated by PLASMOD
- Plasma profiles -- all radial profiles are created by PLASMOD, including many 
  not availabe in PROCESS 0-D: 
    - electron density, 
    - electron temperature, 
    - ion density, 
    - ion temperature, 
    - current density, 
    - bootstrap current, 
    - current from current drive, 
    - poloidal current, 
    - poloidal fluix, 
    - safety factor, 
    - plasma conductivity, 
    - alpha pressure

### PROCESS physics models used by PLASMOD

- Pedestal pressure scaling (subroutine `p_eped_scaling`)
- Radiation model for Bremsstrahlung (subroutine `impradprofile`)
- L-mode to H-mode power threshold calculation (subroutine `pthresh`)
- Fusion reaction rate calculation (function `bosch_hale`)

### PROCESS physics models constrained when using PLASMOD

The following constraint equations may not be used, as they are constrained within PLASMOD:

| `icc` | Description |
| - | - |
| 1  | Beta |
| 2  | Global power balance |
| 5  | Density upper limit |
| 15 | LH power threshold limit |
| 24 | Beta upper limit |
| 62 | Ratio of particle to energy confinement time |
| 68 | Pseparatrix * Bt / q*A*R |
  
The following iteration variables may not be used, as they are outputs of PLASMOD:

| `ixc` | Variable Name |
| - | - |
| 4   | `te` |
| 5   | `beta` |
| 6   | `dene` |
| 9   | `fdene` |
| 36  | `fbetatry` |
| 44  | `fvsbrnni` |
| 102 | `fimpvar` |
| 103 | `flhthresh` |
| 109 | `ralpne` |
| 110 | `ftaulimit` |
| 117 | `fpsepbqar` |

- If `plasmod_i_modeltype > 1` no constraint equations or iteration variables 
  relating to beta may be applied (`icc = 6, 24, 48` and `ixc = 8, 36, 79`.
- If `plasmod_i_equiltype == 2` then `q` cannot be used as an iteration 
  variable `ixc = 18`. N.B. This option is not recommended.
- Only `ishape = 4` may be used.
- NBI heating must be applied with the flag `iefrf` set to model 5 (ITER 
  neutral beam model) or model 8 (Culham neutral beam model); Electron/Ion 
  Cyclotron heating is not currently available in PLASMOD.
- Switch `iradloss` must be set to 0.
- L-H mode power threshold scaling mode must be 2008 Martin scaling: 
  nominal (`ilhthresh} = 6`).
- Both `fgwsep` and `fgwped` must be given a value greater than 0, and `fgwped` 
  must have the larger value.
- Control power may be defined by the user by setting either `contrpovs` or 
  `contrpovr` (per unit area or unit distance respectively) to be non-zero, but not both.
  
[^1]: E. Fable, C. Angioni, M. Siccinio, H. Zohm, 'Plasma physics for fusion 
reactor system codes: Framework and model code', FED **130**, 131-136 (2018)
[^2]: G.V. Pereverzev, P.N. Yushmanov, 'ASTRA. Automated System for Transport 
Analysis in a Tokamak', Garching: Max-Planck-Institut für Plasmaphysik (2002)