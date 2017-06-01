**Username**: morrisj

**Date**: 26/05/2017

**Time**: 16:08

**PROCESS version**: 1.0.10

**Run description**: ""PROCESS runs for new baseline design EU DEMO1 2017: 02.05.2017" " 

**IN.DAT Comment**

> Test file for the new `PROCESS` output tool. Based on the 2017
 baseline file. As much information as you like can enter this
 statement. It can include markdown and other characters.


# Contents

[Diagrams](#diagrams)

[General](#general)

&nbsp;&nbsp;&nbsp;&nbsp;[Radial Build](#radial-build)

&nbsp;&nbsp;&nbsp;&nbsp;[Vertical Build](#vertical-build)

[Physics](#physics)

[Magnets](#magnets)

[Balance of plant](#balance-of-plant)

[Breeder blanket](#breeder-blanket)

[Heating and current drive](#heating-and-current-drive)

[Tritium fuelling and vacuum](#tritium-fuelling-and-vacuum)

# Diagrams

![alt text](process_diagram.png 'PROCESS output diagrams')

# General

[:arrow_up:](#contents)

> 


## Radial Build

| Name | Thickness [m] | Radial Position [m] | Description |
| --- | --- | --- | --- |
| `bore` |2.459 |2.459 | Machine bore |
| `ohcth` |0.824 |3.283 | Central solenoid |
| `precomp` |0.054 |3.337 | CS precompression |
| `gapoh` |0.050 |3.387 | Gap between precomp and TF inboard |
| `tfcth` |1.053 |4.440 | TF coil inboard leg |
| `deltf` |0.137 |4.577 | Gap between TF inboard and thermal shield |
| `thshield` |0.050 |4.627 | Thermal shield |
| `gapds` |0.020 |4.647 | Gap between thermal shield and vacuum vessel |
| `ddwi + shldith` |0.600 |5.247 | Inboard vacuum vessel (and shielding) |
| `vvblgap` |0.020 |5.267 | Gap between vacuum vessel and inboard blanket |
| `blnkith` |0.755 |6.022 | Inboard blanket |
| `fwith` |0.018 |6.040 | Inboard first wall |
| `scrapli` |0.225 |6.265 | Inboard scrape-off layer |
| `rminor` |2.983 |9.248 | Plasma inboard minor radius |
| `rminor` |2.983 |12.231 | Plasma outboard minor radius |
| `scraplo` |0.225 |12.456 | Outboard scrape-off layer |
| `fwoth` |0.018 |12.474 | Outboard first wall |
| `blnkoth` |0.982 |13.456 | Outboard blanket |
| `vvblgap` |0.020 |13.476 | Gap between vacuum vessel and outboard blanket |
| `ddwi + shldoth` |1.100 |14.576 | Outboard vacuum vessel and shielding |
| `gapsto` |1.965 |16.541 | Gap between outboard vacuum vessel and thermal shield |
| `thshield` |0.050 |16.591 | Outboard thermal shield |
| `tftsgap` |0.050 |16.641 | Gap between outboard thermal shield and TF outboard |
| `tfthko` |1.053 |17.694 | TF coil outboard leg |
## Vertical Build

| Name | Thickness [m] | Height [m] | Description |
| --- | --- | --- | --- |
| `tfcth` |1.053 |8.622 | Top TF Coil |
| `tftsgap` |0.050 |7.569 | Gap between thermal shield and top TF coil |
| `thshield` |0.050 |7.519 | Thermal shield |
| `vgap2` |0.050 |7.469 | Gap between thermal shield and top vacuum vessel |
| `ddwi + shldtth` |0.600 |7.419 | Top vacuum vessel (and shielding) |
| `vvblgap` |0.020 |6.819 | Gap between top vacuum vessel and top blanket |
| `blnktth` |0.869 |6.800 | Top blanket thickness |
| `fwtth` |0.018 |5.931 | Top first wall |
| `vgaptop` |0.600 |5.913 | Top scrape-off layer |
| `rminor * kappa` |5.313 |5.313 | Plasma half-height (top) |
| `Midplane` |0.000 | 0 | Device midplane |
| `rminor * kappa` |5.313 |-5.313 | Plasma half-height (bottom) |
| `vgap` |1.600 |-6.913 | Lower scrape-off layer |
| `divfix` |0.621 |-7.534 | Divertor structure |
| `ddwi + shldlth` |1.000 |-8.534 | Lower vacuum vessel (and shielding) |
| `vgap2` |0.050 |-8.584 | Gap between lower vacuum vessel and thermal shield |
| `thshield` |0.050 |-8.634 | Lower thermal shield |
| `tftsgap` |0.050 |-8.684 | Gap between lower thermal shield and lower TF coil |
| `tfcth` |1.053 |-9.737 | Lower TF coil |
### Constraints

| Constraint | Description | F-Value Name | F-Value Value | Limit Name | Limit | Value |
| --- | --- | --- | --- | --- | --- | --- |
| 11 | Radial build | - | - | consistency | - | - |
| 13 | Burn time lower limit | ftburn | 1.00e+00 | tbrnmn | 1.00e+00 | 1.00e+00 |
> **11** : **Radial build** : ensure radial build is consistent with plasma major radius


> **13** : **Burn time lower limit** : set burn time equal to `tbrnmn` as `f-value` not iteration variable


### Iteration Variables

* Values in **bold** are **not default** but user inputs.

| No. | Name | Final Value | Description | Starting Value | Lower Bound | Upper Bound |
| --- | --- | --- | --- | --- | --- | --- |
| 13 | `tfcth` |1.053 | Inboard tf coil thickness, (centrepost for st) (m) | **1.05** |0.1 |5 |
| 16 | `ohcth` |0.8244 | Central solenoid thickness (m) | **0.8181** |0.01 |10 |
| 29 | `bore` |2.459 | Central solenoid inboard radius (m) | **2.483** | **0.1** |10 |
| 42 | `gapoh` |0.05 | Gap between central solenoid and tf coil (m) | **0.05** | **0.05** | **0.1** |
| 61 | `gapds` |0.02 | Gap between inboard vacuum vessel and thermal shield (m) | **0.12** | **0.02** |10 |
> **61** : **gapds** : lower bound set to 2 cm after communication from **WPPMI**


### Inputs

| Input | Value | Description | Comment |
| --- | --- | --- | --- |
| `vgap` | 1.60 | Vertical gap between x-point and divertor (m) | set to 1.6 m after discussion with **WPPMI** |
| `ddwex` | 0.15 | Cryostat thickness (m) |  |
| `tramp` | 500.0 | Initial pf coil charge time (s); if pulsed, = tohs | pre-magnetisation time set to 500 s in discussion with **WPPMI** |
| `shldoth` | 0.80 | Outboard shield thickness (m) | set to 0.8 m after discussion with **WPPMI** |
| `iavail` | 0 | Switch for plant availability model: | use input availability |
| `pulsetimings` | 0 | Switch for pulse timings (if lpulse=1): | ramp up and down set to `Ip/0.1` (communication with **WPPMI**) |
| `lpulse` | 1 | Switch for reactor model: |  |
| `gapomin` | 0.20 | Minimum gap between outboard vacuum vessel and tf coil (m) | set to 0.2 m after discussion with **WPPMI** |
| `iohcl` | 1 | Switch for existence of central solenoid: |  |
| `epsvmc` | 1.0e-8 | Error tolerance for vmcon |  |
| `cfactr` | 0.75 | Total plant availability fraction; |  |
| `ioptimz` | 1 | Code operation switch: |  |
| `shldith` | 0.30 | Inboard shield thickness (m) | set to 0.3 m after discussion with **WPPMI** |
| `tlife` | 40 | Plant life (years) |  |
| `tftsgap` | 0.05 | Minimum metal-to-metal gap between tf coil and thermal shield (m) | set to 0.05 m after discussion with **WPPMI** |
| `scraplo` | 0.225 | Gap between plasma and first wall, outboard side (m) |  |
| `cost_model` | 0 | Switch for cost model: | 1990 model |
| `scrapli` | 0.225 | Gap between plasma and first wall, inboard side (m) |  |
| `vvblgap` | 0.02 | Gap between vacuum vessel and blanket (m) | set to 0.02 m after discussion with **WPPMI** |
| `shldtth` | 0.30 | Upper/lower shield thickness (m); | set to 0.3 m after discussion with **WPPMI** |
| `minmax` | 1 | Switch for figure-of-merit (see lablmm for descriptions) |  |
| `blnkith` | 0.755 | Inboard blanket thickness (m); | set to 0.755 m after discussion with **WPPMI** |
| `vgap2` | 0.05 | Vertical gap between vacuum vessel and tf coil (m) | set to 0.05 m after discussion with **WPPMI** |
| `tdwell` | 0 | Time between pulses in a pulsed reactor (s) | set to 0 after discussion with **WPPMI**. Assumed pump-down can |
| `output_costs` | 0 | Switch for costs output: | costs model turned off |
| `ddwi` | 0.30 | Vacuum vessel thickness (tf coil / shield) (m) | set to 0.3 m after discussion with **WPPMI** |
### Outputs

| Output | Value | Description |
| --- | --- | --- |
| `ifail` |1 | VMCON error flag |
| `abktflnc` |5 | Allowable blanket neutron fluence (MW-yr/m2) |
| `adivflnc` |7 | Allowable divertor heat fluence (MW-yr/m2) |
| `bktlife` |6.692 | First wall / blanket lifetime (years) |
| `divlife` |7.544 | Divertor lifetime (years) |
| `cdrlife` |6.692 | Heating/CD system lifetime (years) |
| `tohs` |194.4 | Plasma current ramp-up time (s) |
| `theat` |10 | Heating time (s) |
| `tburn` |7200 | Burn time (s) |
| `tqnch` |194.4 | Reset time to zero current for CS (s) |
| `tcycle` |8099 | Total plant cycle time (s) |
|  |0 | kallenbach switch |
| `precomp` |0.05388 | CS precompression (m) |
| `deltf` |0.137 | TF coil inboard leg gap (m) |
| `thshield` |0.05 | Thermal shield (m) |
| `fwith` |0.018 | Inboard first wall radial thickness (m) |
| `fwoth` |0.018 | Outboard first wall radial thickness (m) |
| `blnkoth` |0.982 | Outboard blanket radial thickness (m) |
| `gapsto` |1.965 | Vessel to TF radial gap (m) |
| `tfthko` |1.053 | TF coil outboard leg radial thickness (m) |
| `blnktth` |0.8685 | Top blanket vertical thickness (m) |
| `fwtth` |0.018 | Top first wall vertical thickness (m) |
| `vgaptop` |0.6 | Top scrape-off vertical thickness (m) |
| `rminor*kappa` |5.313 | Plasma half-height (m) |
| `shldlth` |0.7 | Bottom radiation shield thickness (m) |
| `beamwd` |0.58 | Width of neutral beam duct where it passes between the TF coils (m) |
| `fncmass` |3.899e+05 | Outer PF coil fence mass (kg) |
| `aintmass` |8.027e+06 | Intercoil support structure mass (kg) |
| `coldmass` |5.115e+07 | Mass of cooled components (kg) |
| `clgsmass` |2.195e+06 | Gravity support structure mass (kg) |
| `gsm1` |9.492e+04 | Torus leg support mass (kg) |
| `gsm2` |5.728e+05 | Ring beam mass (kg) |
| `gsm3` |1.062e+06 | Ring legs mass (kg) |
| `vrci` |1.232e+06 | Internal volume of reactor building (m3) |
| `wrbi` |43.42 | Dist from centre of torus to bldg wall (m) |
| `efloor` |3.849e+05 | Effective floor area (m2) |
| `rbv` |1.386e+06 | Reactor building volume (m3) |
| `rmbv` |4.259e+05 | Reactor maintenance building volume (m3) |
| `wsv` |1.292e+05 | Warmshop volume (m3) |
| `triv` |4e+04 | Tritium building volume (m3) |
| `elev` |5.211e+04 | Electrical building volume (m3) |
| `conv` |6e+04 | Control building volume (m3) |
| `cryv` |1.608e+04 | Cryogenics building volume (m3) |
| `admv` |1e+05 | Administration building volume (m3) |
| `shov` |1e+05 | Shops volume (m3) |
| `volnucb` |1.843e+06 | Total volume of nuclear buildings (m3) |
# Physics

[:arrow_up:](#contents)

> 


### Constraints

| Constraint | Description | F-Value Name | F-Value Value | Limit Name | Limit | Value |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | Beta | - | - | consistency | - | - |
| 2 | Global power balance | - | - | consistency | - | - |
| 5 | Density upper limit | fdene | 1.20e+00 | dnelimt | 6.95e+19 | 8.34e+19 |
| 8 | Neutron wall load upper limit | fwalld | 1.24e-01 | walalw | 1.00e+00 | 1.24e-01 |
| 15 | LH power threshold limit | flhthresh |1.39 | plhthresh | **calculated** | - |
| 24 | Beta upper limit | fbetatry | 5.51e-01 | betalim | 5.61e-02 | 3.09e-02 |
| 68 | Psep | fpsep | **1.0** | psep_kallenbach | **calculated** | - |
| 72 | central solenoid Tresca stress limit | fplhsep | 1.00e+00 | pdivt | 1.55e+02 | 1.55e+02 |
> **1** : **Beta** : apply the beta consistency check


> **2** : **Global power balance** : enforce consistency of the power balance of the machine


> **5** : **Density upper limit** : allow the density to exceed the limit (the limit is the Greenwald density)


> **8** : **Neutron wall load upper limit** : surface flux of neutrons in MW/m2. Not limiting in baseline case


> **15** : **LH power threshold limit** : L-H power threshold limit


> **24** : **Beta upper limit** : 


> **72** : **central solenoid Tresca stress limit** : 


> **68** : **Psep** : implemented constraint instead of `psep/r` on request of **PMI**


### Iteration Variables

* Values in **bold** are **not default** but user inputs.

| No. | Name | Final Value | Description | Starting Value | Lower Bound | Upper Bound |
| --- | --- | --- | --- | --- | --- | --- |
| 2 | `bt` |5.094 | Toroidal field on axis (t) (iteration variable 2) |5.68 |0.01 | **20.0** |
| 3 | `rmajor` |9.249 | Plasma major radius (m) (iteration variable 3) | **9.072** |0.1 | **13** |
| 4 | `te` |13.11 | Volume averaged electron temperature (kev) |12.9 |5 | **150.0** |
| 5 | `beta` |0.03523 | Total plasma beta (iteration variable 5) |0.042 |0.001 |1 |
| 6 | `dene` |7.533e+19 | Electron density (/m3) (iteration variable 6) | **7.98e+19** | **6.0e+19** |1e+21 |
| 10 | `hfact` |1.1 | H factor on energy confinement times (iteration variable 10) |1 |0.1 | **1.1** |
| 18 | `q` |3 | Safety factor 'near' plasma edge (iteration variable 18): |3 | **3.0** |50 |
| 44 | `fvsbrnni` |0.4778 | Fraction of the plasma current produced by | **0.4434** |0.001 |1 |
| 135 | `fimp` | 0 | Impurity number density fractions relative to electron density | **[1.0, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.00044, 5e-05]** |1e-08 |0.01 |
> **135** : **fimp** : Xenon is seeded impurity


### Inputs

| Input | Value | Description | Comment |
| --- | --- | --- | --- |
| `iscdens` | 1 | Switch for pedestal profiles: |  |
| `ishape` | 2 | Switch for plasma cross-sectional shape calculation: |  |
| `idensl` | 7 | Switch for density limit to enforce (constraint equation 5): |  |
| `isc` | 34 | Switch for energy confinement time scaling law |  |
| `nesep` | 0.2e20 | Electron density at separatrix [m-3] (ipedestal=1) |  |
| `divfix` | 0.621 | Divertor structure vertical thickness (m) |  |
| `q0` | 1.0 | Safety factor on axis |  |
| `iprofile` | 1 | Switch for current profile consistency: |  |
| `ralpne` | 0.1 | Thermal alpha density / electron density (iteration variable 109) |  |
| `rhopedn` | 0.94 | R/a of density pedestal (ipedestal=1) |  |
| `ibss` | 4 | Switch for bootstrap current scaling: |  |
| `tesep` | 0.1 | Electron temperature at separatrix (kev) (ipedestal=1) |  |
| `rhopedt` | 0.94 | R/a of temperature pedestal (ipedestal=1) |  |
| `fkzohm` | 1.0245 | Zohm elongation scaling adjustment factor (ishape=2, 3) |  |
| `ssync` | 0.6 | Synchrotron wall reflectivity factor |  |
| `icurr` | 4 | Switch for plasma current scaling to use: |  |
| `ipedestal` | 1 | Switch for pedestal profiles: |  |
| `iculbl` | 1 | Switch for beta limit scaling (constraint equation 24): |  |
| `alphan` | 1.00 | Density profile index |  |
| `coreradius` | 0.75 | Normalised radius defining the 'core' region | set to 0.75 after discussion with **WPPMI** |
| `aspect` | 3.1 | Aspect ratio (iteration variable 1) |  |
| `tbeta` | 2.0 | Temperature profile index beta  (ipedestal=1) |  |
| `fgwped` | 0.85 | Fraction of greenwald density to set as pedestal-top density | set to 85 % after discussion with **WPPMI** |
| `kappa` | 1.848 | Plasma separatrix elongation (calculated if ishape > 0) |  |
| `neped` | 0.678e20 | Electron density of pedestal [m-3] (ipedestal=1) |  |
| `teped` | 5.5 | Electron temperature of pedestal (kev) (ipedestal=1, ieped=0) |  |
| `alphat` | 1.45 | Temperature profile index |  |
| `triang` | 0.5 | Plasma separatrix triangularity (calculated if ishape=1, 3 or 4) |  |
| `coreradiationfraction` | 0.6 | Fraction of radiation from 'core' region that is subtracted from the loss power | set to 0.6 after discussion with **WPPMI** |
| `gamma` | 0.3 | Ejima coefficient for resistive startup v-s formula |  |
| `ifalphap` | 1 | Switch for fast alpha pressure calculation: |  |
| `imprad_model` | 1 | Switch for impurity radiation model: |  |
| `snull` | 1 | Switch for single null / double null plasma: |  |
### Outputs

| Output | Value | Description |
| --- | --- | --- |
| `rminor` |2.983 | Minor radius (m) |
| `kappa95` |1.59 | Elongation, 95% surface (kappa/1.12) |
| `kappaa` |1.665 | Elongation, area ratio calc. |
| `triang95` |0.3333 | Triangularity, 95% surface (triang/1.5) |
| `pperim` |26.27 | Plasma poloidal perimeter (m) |
| `xarea` |46.56 | Plasma cross-sectional area (m2) |
| `sarea` |1484 | Plasma surface area (m2) |
| `vol` |2651 | Plasma volume (m3) |
| `plascur/1d6` |19.44 | Plasma current (MA) |
| `alphaj` |1.508 | Current density profile factor |
| `rli` |1.096 | Plasma internal inductance, li |
| `bvert` |-0.7044 | Vertical field at plasma (T) |
| `bt` |5.094 | Vacuum toroidal field at R (T) |
| `bp` |0.9299 | Average poloidal field (T) |
| `btot` |5.179 | Total field (sqrt(bp^2 + bt^2)) (T) |
| `q95` |3 | Safety factor at 95% flux surface |
| `qstar` |2.508 | Cylindrical safety factor (qcyl) |
| `beta` |0.03523 | Total plasma beta |
| `betap` |1.092 | Total poloidal beta |
|  |0.0364 | Total toroidal beta |
| `betaft` |0.004356 | Fast alpha beta |
| `betanb` |0 | Beam ion beta |
| `gammaft` |0.1411 | (Fast alpha + beam beta)/(thermal beta) |
|  |0.03087 | Thermal beta |
|  |0.9574 | Thermal poloidal beta |
|  |0.0319 | Thermal toroidal beta (= beta-exp) |
| `eps*betap` |0.3524 | 2nd stability beta : beta p / (R/a) |
| `epbetmax` |1.38 | 2nd stability beta upper limit |
| `dnbeta` |4.383 | Beta g coefficient |
|  |2.414 | Normalised thermal beta |
|  |2.754 | Normalised total beta |
| `betalim` |0.05607 | Limit on thermal beta |
|  |1.31e+09 | Plasma thermal energy (J) |
|  |1.494e+09 | Total plasma internal energy (J) |
| `te` |13.11 | Electron temperature (keV) |
| `te0` |27.47 | Electron temperature on axis (keV) |
| `ti` |13.11 | Ion temperature (keV) |
| `ti0` |27.47 | Ion temperature on axis (keV) |
| `ten` |14.52 | Electron temp., density weighted (keV) |
| `ne0` |9.908e+19 | Electron density on axis (/m3) |
| `dnla` |8.342e+19 | Line-averaged electron density (/m3) |
| `dnla_gw` |1.2 | Line-averaged electron density / Greenwald density |
| `dnitot` |6.625e+19 | Ion density (/m3) |
| `deni` |5.864e+19 | Fuel density (/m3) |
| `dnz` |3.148e+16 | High Z impurity density (/m3) |
| `dnalp` |7.533e+18 | Helium ion density (thermalised ions only) (/m3) |
| `dnprot` |4.508e+16 | Proton ash density (/m3) |
| `dnbeam` |0 | Hot beam density (/m3) |
| `dnelimt` |6.952e+19 | Density limit from scaling (/m3) |
| `boundu(9)` |8.342e+19 | Density limit (enforced) (/m3) |
| `imprad_model` |1 | Plasma impurity model |
| `fimp(01` |0.779 | H  concentration |
| `fimp(02` |0.1 | He concentration |
| `fimp(03` |0 | Be concentration |
| `fimp(04` |0 | C  concentration |
| `fimp(05` |0 | N  concentration |
| `fimp(06` |0 | O  concentration |
| `fimp(07` |0 | Ne concentration |
| `fimp(08` |0 | Si concentration |
| `fimp(09` |0 | Ar concentration |
| `fimp(10` |0 | Fe concentration |
| `fimp(11` |0 | Ni concentration |
| `fimp(12` |0 | Kr concentration |
| `fimp(13` |0.0003679 | Xe concentration |
| `fimp(14` |5e-05 | W  concentration |
| `aion` |2.734 | Average mass of all ions (amu) |
| `zeff` |2.234 | Effective charge |
| `ieped` |0 | Pedestal scaling switch |
| `fgwsep` |0.5 | Electron density at separatrix / nGW |
| `dlimit(1)` |4.696e+19 | Old ASDEX model |
| `dlimit(2)` |9.405e+19 | Borrass ITER model I |
| `dlimit(3)` |3.677e+19 | Borrass ITER model II |
| `dlimit(4)` |3.554e+21 | JET edge radiation model |
| `dlimit(5)` |3.526e+20 | JET simplified model |
| `dlimit(6)` |6.59e+19 | Hugill-Murakami Mq model |
| `dlimit(7)` |6.952e+19 | Greenwald model |
| `fdeut` |0.5 | Deuterium fuel fraction |
| `ftrit` |0.5 | Tritium fuel fraction |
| `powfmw` |2010 | Total fusion power (MW) |
| `pdt` |2007 | =    D-T fusion power (MW) |
| `pdd` |2.482 | +   D-D fusion power (MW) |
| `pdhe3` |0 | + D-He3 fusion power (MW) |
| `palpmw` |401.5 | Alpha power: total (MW) |
| `palpnb` |0 | Alpha power: beam-plasma (MW) |
| `pneutmw` |1607 | Neutron power (MW) |
| `pchargemw` |1.61 | Charged particle power (excluding alphas) (MW) |
|  |433.9 | Total power deposited in plasma (MW) |
| `pbrempv*vol` |72.63 | Bremsstrahlung radiation power (MW) |
| `plinepv*vol` |185.2 | Line radiation power (MW) |
| `psyncpv*vol` |20.69 | Synchrotron radiation power (MW) |
| `coreradiationfracti` |0.6 | Fraction of core radiation subtracted from P L |
| `pcoreradmw` |112.5 | Total core radiation power (MW) |
| `pedgeradmw` |166.1 | Edge radiation power (MW) |
| `pradmw` |278.6 | Total radiation power (MW) |
| `rad_fraction` |0.642 | Radiation fraction = total radiation / total power deposited in plasma |
| `photon_wall` |0.1727 | Nominal mean radiation load on inside surface of reactor (MW/m2) |
| `peakfactrad` |3.33 | Peaking factor for radiation wall load |
| `maxradwallload` |1 | Maximum permitted radiation wall load (MW/m^2) |
| `peakradwallload` |0.5751 | Peak radiation wall load (MW/m^2) |
| `wallmw` |0.9962 | Nominal mean neutron load on inside surface of reactor (MW/m2) |
| `pohmmw` |0.8637 | Ohmic heating power (MW) |
| `falpha` |0.95 | Fraction of alpha power deposited in plasma |
| `falpe` |0.7107 | Fraction of alpha power to electrons |
| `falpi` |0.2893 | Fraction of alpha power to ions |
| `ptrimw` |150.4 | Ion transport (MW) |
| `ptremw` |171 | Electron transport (MW) |
| `pinjimw` |0 | Injection power to ions (MW) |
| `pinjemw` |50 | Injection power to electrons (MW) |
| `ignite` |0 | Ignited plasma switch (0=not ignited, 1=ignited) |
| `pdivt` |155.3 | Power into divertor zone via charged particles (MW) |
| `pdivt/rmajor` |16.8 | Psep / R ratio (MW/m) |
| `pdivtbt/qar` |9.2 | Psep Bt / qAR ratio (MWT/m) |
| `pthrmw(1)` |158.6 | 1996 ITER scaling: nominal (MW) |
| `pthrmw(2)` |369.4 | 1996 ITER scaling: upper bound (MW) |
| `pthrmw(3)` |67.16 | 1996 ITER scaling: lower bound (MW) |
| `pthrmw(4)` |266 | 1997 ITER scaling (1) (MW) |
| `pthrmw(5)` |204 | 1997 ITER scaling (2) (MW) |
| `pthrmw(6)` |111.8 | 2008 Martin scaling: nominal (MW) |
| `pthrmw(7)` |147.3 | 2008 Martin scaling: 95% upper bound (MW) |
| `pthrmw(8)` |76.25 | 2008 Martin scaling: 95% lower bound (MW) |
| `boundl(103` |111.8 | L-H threshold power (enforced) (MW) |
| `tauelaw` | IPB98(y,2) | Confinement scaling law |
| `hfact` |1.1 | Confinement H factor |
| `taueff` |4.074 | Global energy confinement time (s) |
| `tauei` |4.074 | Ion energy confinement time (s) |
| `tauee` |4.074 | Electron energy confinement time (s) |
| `dntau` |3.069e+20 | n.tau = Volume-average electron density x Energy confinement time (s/m3) |
| `dntau*te` |4.022e+21 | Triple product  (keV s/m3) |
| `powerht` |321.4 | Transport loss power assumed in scaling law (MW) |
| `iradloss` |1 | Switch for radiation loss term usage in power balance |
|  |112.5 | Radiation power subtracted from plasma power balance (MW) |
| `taup` |28.03 | Alpha particle confinement time (s) |
| `taup/taueff` |6.88 | Alpha particle/energy confinement time ratio |
| `taulimit` |5 | Lower limit on taup/taueff |
| `vsstt` |702.9 | Total volt-second requirement (Wb) |
| `vsind` |314.8 | Inductive volt-seconds (Wb) |
| `vsres` |67.78 | Start-up resistive (Wb) |
| `vsbrn` |320.3 | Flat-top resistive (Wb) |
| `cboot` |1 | bootstrap current fraction multiplier |
| `bscf_iter89` |0.3301 | Bootstrap fraction (ITER 1989) |
| `bscf_nevins` |0.3222 | Bootstrap fraction (Nevins et al) |
| `bscf_wilson` |0.39 | Bootstrap fraction (Wilson et al) |
| `bscf_sauter` |0.367 | Bootstrap fraction (Sauter et al) |
| `bootipf.` |0.367 | Bootstrap fraction (enforced) |
| `vburn` |0.04443 | Loop voltage during burn (V) |
| `rplas` |4.376e-09 | Plasma resistance (ohm) |
| `res_time` |3341 | Resistive diffusion time (s) |
| `rlp` |1.62e-05 | Plasma inductance (H) |
| `csawth` |1 | Coefficient for sawtooth effects on burn V-s requirement |
| `tauratio` |1 | Ratio of He and pellet particle confinement times |
| `qfuel` |3.506e+21 | Fuelling rate (nucleus-pairs/s) |
| `rndfuel` |7.166e+20 | Fuel burn-up rate (reactions/s) |
| `burnup` |0.2044 | Burn-up fraction |
| `ptop_radial` |7.757 | Plasma top position, radial (m) |
| `ptop_vertical` |5.313 | Plasma top position, vertical (m) |
| `0.0` |0 | Plasma geometric centre, vertical (m) |
| `tril` |0.5 | Plasma lower triangularity |
| `tfoffset` |-0.5572 | TF coil vertical offset (m) |
| `rco` |5.391 | Plasma outer arc radius of curvature (m) |
| `rci` |10.21 | Plasma inner arc radius of curvature (m) |
| `rxpt` |7.757 | Plasma lower X-pt, radial (m) |
| `zxpt` |-5.313 | Plasma lower X-pt, vertical (m) |
| `thetai` |0.1707 | Poloidal plane angle between vertical and inner leg (rad) |
| `thetao` |1.023 | Poloidal plane angle between vertical and outer leg (rad) |
| `betai` |1 | Poloidal plane angle between inner leg and plate (rad) |
| `betao` |1 | Poloidal plane angle between outer leg and plate (rad) |
| `plsepi` |1 | Inner divertor leg poloidal length (m) |
| `plsepo` |1.5 | Outer divertor leg poloidal length (m) |
| `plleni` |1 | Inner divertor plate length (m) |
| `plleno` |1 | Outer divertor plate length (m) |
| `rspi` |6.772 | Inner strike point, radial (m) |
| `zspi` |-5.482 | Inner strike point, vertical (m) |
| `rplti` |6.966 | Inner plate top, radial (m) |
| `zplti` |-5.022 | Inner plate top, vertical (m) |
| `rplbi` |6.577 | Inner plate bottom, radial (m) |
| `zplbi` |-5.943 | Inner plate bottom, vertical (m) |
| `rspo` |8.538 | Outer strike point, radial (m) |
| `zspo` |-6.593 | Outer strike point, vertical (m) |
| `rplto` |8.756 | Outer plate top, radial (m) |
| `zplto` |-6.144 | Outer plate top, vertical (m) |
| `rplbo` |8.319 | Outer plate bottom, radial (m) |
| `zplbo` |-7.043 | Outer plate bottom, vertical (m) |
| `divht` |2.021 | Calculated maximum divertor height (m) |
# Magnets

[:arrow_up:](#contents)

> 


### Constraints

| Constraint | Description | F-Value Name | F-Value Value | Limit Name | Limit | Value |
| --- | --- | --- | --- | --- | --- | --- |
| 10 | Toroidal field 1 | - | - | consistency | - | - |
| 26 | Central solenoid EOF current density upper limit | fjohc | 5.95e-01 | rjohc | 2.01e+07 | 1.19e+07 |
| 27 | Central solenoid BOP current density upper limit | fjohc0 | 5.41e-01 | rjohc0 | 2.00e+07 | 1.09e+07 |
| 31 | TF coil case stress upper limit | fstrcase | 1.00e+00 | alstrtf | 6.00e+08 | 6.00e+08 |
| 32 | TF coil conduit stress upper limit | fstrcond | 8.27e-01 | alstrtf | 6.00e+08 | 4.96e+08 |
| 33 | I_op | fiooic | 5.92e-01 | jwdgcrt | 3.72e+07 | 2.20e+07 |
| 34 | Dump voltage upper limit | fvdump | 9.69e-01 | vdalw | 2.00e+01 | 1.94e+01 |
| 35 | J_winding pack | fjprot | 1.00e+00 | jwdgpro | 2.20e+07 | 2.20e+07 |
| 36 | TF coil temperature margin lower limit | ftmargtf | 1.00e+00 | tmargmin | 2.50e+00 | 2.50e+00 |
| 65 | Dump time set by VV loads | fpoloidalpower | 1.00e+00 | maxpoloidalpower | 1.00e+03 | 1.00e+03 |
> **10** : **Toroidal field 1** : ensure toroidal field is consistent with 1/R scaling


> **31** : **TF coil case stress upper limit** : stress limit from ITER class I TF steels (660 MPa) but as
 out-of-plane forces ignored limit lowered to 600 MPa


> **32** : **TF coil conduit stress upper limit** : stress limit from ITER class I TF steels (660 MPa) but as
 out-of-plane forces ignored limit lowered to 600 MPa


> **33** : **I_op** : 


> **34** : **Dump voltage upper limit** : dump voltage maximum at 10 kV in agreement with **WPMAG**


> **35** : **J_winding pack** : 


> **36** : **TF coil temperature margin lower limit** : set to 1.5 K in agreement with **WPMAG**


> **26** : **Central solenoid EOF current density upper limit** : 


> **27** : **Central solenoid BOP current density upper limit** : 


> **65** : **Dump time set by VV loads** : 


### Iteration Variables

* Values in **bold** are **not default** but user inputs.

| No. | Name | Final Value | Description | Starting Value | Lower Bound | Upper Bound |
| --- | --- | --- | --- | --- | --- | --- |
| 12 | `oacdcp` |9.228e+06 | Overall current density in tf coil inboard legs (a/m2) | **10050000.0** |1e+05 |1.5e+08 |
| 37 | `coheof` |1.194e+07 | Central solenoid overall current density at end of flat-top (a/m2) | **13540000.0** |1e+05 |1e+08 |
| 41 | `fcohbop` |0.9086 | Ratio of central solenoid overall current density at |0.9 |0.001 |1 |
| 52 | `vdalw` |8.947 | Max voltage across tf coil during quench (kv) | **10.00** |0.001 | **10.0** |
| 56 | `tdmptf` |23.22 | Quench time for tf coil (s) | **30.0** |0.1 |100 |
| 57 | `thkcas` |0.4924 | Inboard tf coil case outer (non-plasma side) thickness (m) | **0.495** |0.05 |1 |
| 58 | `thwcndut` |0.008 | Tf coil conduit case thickness (m) (iteration variable 58) | **0.008** | **8.0e-3** |0.1 |
| 59 | `fcutfsu` |0.7645 | Copper fraction of cable conductor (tf coils) |0.69 | **0.50** | **0.94** |
| 60 | `cpttf` |9e+04 | Tf coil current per turn (a). | **6.5e+04** | **6.0e4** | **9.0e4** |
| 122 | `oh_steel_frac` |0.8103 | Central solenoid steel fraction (iteration variable 122) | **0.8** |0.001 |0.95 |
> **58** : **thwcndut** : lower bound set to 8 mm after discussion with **WPMAG**


### Inputs

| Input | Value | Description | Comment |
| --- | --- | --- | --- |
| `isumatpf` | 3 | Switch for superconductor material in pf coils: | using NbTi in PF coils after discussion with **WPMAG** |
| `tfno` | 16 | Number of tf coils (default = 50 for stellarators) | set to 16 on advice of **WPPMI** |
| `casths` | 0.05 | Either: inboard tf coil sidewall case thickness (m) |  |
| `ipfloc` | **array** | Switch for locating scheme of pf coil group i: |
| `ipfloc`[0] | 2 | - |  |
| `ipfloc`[1] | 2 | - |  |
| `ipfloc`[2] | 3 | - |  |
| `ipfloc`[3] | 3 | - |  |
| `dhecoil` | 0.010 | Diameter of he coil in tf winding (m) | set to 10 mm on advice of **WPPMI** |
| `alstroh` | 6.0d8 | Allowable hoop stress in central solenoid structural material (pa) |  |
| `rpf2` | -1.825 | Offset (m) of radial position of ipfloc=2 pf coils |  |
| `zref` | **array** | Pf coil vertical positioning adjuster: |
| `zref`[0] |3.6 |  |  |
| `zref`[1] |1.2 |  |  |
| `zref`[2] |1 |  |  |
| `zref`[3] |2.8 |  |  |
| `zref`[4] |1 |  |  |
| `zref`[5] |1 |  |  |
| `zref`[6] |1 |  |  |
| `zref`[7] |1 |  |  |
| `alstrtf` | 6.0e8 | Allowable von mises stress in tf coil structural material (pa) |  |
| `ohhghf` | 0.9 | Central solenoid height / tf coil internal height |  |
| `sigvvall` | 9.3e7 | Allowable stress from tf quench in vacuum vessel (pa) |  |
| `ngrp` | 4 | Number of groups of pf coils. |  |
| `isumattf` | 5 | Switch for superconductor material in tf coils: | use WST scaling on advice of **WPMAG** |
| `tftmp` | 4.750 | Peak helium coolant temperature in tf coils and pf coils (k) |  |
| `ripmax` | 0.6 | Maximum allowable toroidal field ripple amplitude |  |
| `vftf` | 0.300 | Coolant fraction of tfc 'cable' (itfsup=1), or of tfc leg (itfsup=0) |  |
| `tmargmin` | 1.500 | Minimum allowable temperature margin (cs and tf coils) (k) |  |
| `ncls` | **array** | Number of pf coils in group j |
| `ncls`[0] | 1 | - |  |
| `ncls`[1] | 1 | - |  |
| `ncls`[2] | 2 | - |  |
| `ncls`[3] | 2 | - |  |
| `casthi` | 0.06 | Either: inboard tf coil case plasma side thickness (m) |  |
| `isumatoh` | 5 | Switch for superconductor material in central solenoid: |  |
| `tinstf` | 0.008 | Ground insulation thickness surrounding winding pack (m) | set to 8 mm on advice of **WPPMI** |
| `cptdin` | **array** | Peak current per turn input for pf coil i (a) |
| `cptdin`[0] | 4.22d4 | - |  |
| `cptdin`[1] |  4.22d4 | - |  |
| `cptdin`[2] |  4.22d4 | - |  |
| `cptdin`[3] |  4.22d4 | - |  |
| `cptdin`[4] |  4.3d4 | - |  |
| `cptdin`[5] |  4.3d4 | - |  |
| `cptdin`[6] |   4.3d4 | - |  |
| `cptdin`[7] |  4.3d4 | - |  |
| `fcuohsu` | 0.70 | Copper fraction of strand in central solenoid cable |  |
| `thicndut` | 1.5d-3 | Conduit insulation thickness (m) | set to 1.5 mm on advice of **WPPMI** |
| `strncon` | -0.0066 | Strain in superconductor material (tf, pf and cs) | set to 0.66 % on advice of **WPPMI** |
| `rjconpf` | **array** | Average winding pack current density of pf coil i (a/m2) |
| `rjconpf`[0] | 1.1d7 | - |  |
| `rjconpf`[1] |  1.1d7 | - |  |
| `rjconpf`[2] |  6.d6 | - |  |
| `rjconpf`[3] |  6.d6 | - |  |
| `rjconpf`[4] |  8.d6 | - |  |
| `rjconpf`[5] |  8.0d6 | - |  |
| `rjconpf`[6] |  8.0d6 | - |  |
| `rjconpf`[7] |  8.0d6 | - |  |
### Outputs

| Output | Value | Description |
| --- | --- | --- |
| `jwptf` |2.204e+07 | Winding pack current density (A/m2) |
| `tfarea/tfno` |1.596 | Cross-sectional area per coil (m2) |
| `tficrn` |0.8557 | Inboard leg outboard half-width (m) |
| `tfocrn` |0.6631 | Inboard leg inboard half-width (m) |
| `tftort` |1.711 | Outboard leg toroidal thickness (m) |
| `tfleng` |50.24 | Mean coil circumference (m) |
| `ritfc/1.d6` |235.6 | Total current (MA) |
| `bmaxtf` |10.89 | Peak field (Amperes Law,T) |
| `bmaxtfrp` |11.32 | Peak field (with ripple,T) |
| `ripple` |0.6 | Ripple amplitude at plasma (%) |
| `estotft` |144.9 | Total stored energy in TF coils (GJ) |
| `whttf` |1.843e+07 | Total mass of TF coils (kg) |
| `whttf/tfno` |1.152e+06 | Mass of each TF coil (kg) |
| `vforce` |2.588e+08 | Vertical separating force per leg (N) |
| `cforce` |8.017e+07 | Centering force per coil (N/m) |
| `rtfcin` |3.86 | Inboard leg centre radius (m) |
| `rtot` |17.17 | Outboard leg centre radius (m) |
| `hmax` |8.684 | Maximum inboard edge height (m) |
| `xarc(1)` |4.386 | TF coil arc point 1 R (m) |
| `yarc(1)` |4.541 | TF coil arc point 1 Z (m) |
| `xarc(2)` |8.652 | TF coil arc point 2 R (m) |
| `yarc(2)` |7.569 | TF coil arc point 2 Z (m) |
| `xarc(3)` |16.64 | TF coil arc point 3 R (m) |
| `yarc(3)` |0 | TF coil arc point 3 Z (m) |
| `xarc(4)` |8.652 | TF coil arc point 4 R (m) |
| `yarc(4)` |-8.684 | TF coil arc point 4 Z (m) |
| `xarc(5)` |4.386 | TF coil arc point 5 R (m) |
| `yarc(5)` |-5.21 | TF coil arc point 5 Z (m) |
| `taucq` |23.22 | Allowable quench time (s) |
| `leno` |0.0639 | Width of cable (square) (m) |
| `leni` |0.0449 | Width of space inside cable (m) |
| `acs` |0 | Area of space inside cable (m2) |
| `whtconsc` |1.84e+04 | Superconductor mass per coil (kg) |
| `whtconcu` |7.198e+04 | Copper mass per coil (kg) |
| `whtconsh` |1.105e+05 | Steel conduit mass per coil (kg) |
| `whtconin` |5539 | Conduit insulation mass per coil (kg) |
| `whtcon` |2.064e+05 | Total conductor cable mass per coil (kg) |
| `acstf` |0.001985 | Cable conductor + void area (m2) |
| `fcutfsu` |0.7645 | Copper fraction of conductor |
| `1-fcutfsu` |0.2355 | Superconductor fraction of conductor |
| `acond/ap` |0.3211 | Conductor fraction of winding pack |
| `turnstf*acndttf/ap` |0.4221 | Conduit fraction of winding pack |
| `aiwp/ap` |0.09169 | Insulator fraction of winding pack |
| `avwp/ap` |0.1459 | Helium area fraction of winding pack excluding central channel |
| `awphec/ap` |0.01923 | Central helium channel area as fraction of winding pack |
|  |1 | Check total area fractions in winding pack = 1 |
| `thkwp` |0.4641 | Winding radial thickness (m) |
| `wwp1` |1.486 | Winding width 1 (m) |
| `wwp2` |1.393 | Winding width 2 (m) |
| `tfinsgap` |0.01 | Winding pack insertion gap (m) |
| `whtgw` |6465 | Ground wall mass per coil (kg) |
| `turnstf` |163.6 | Number of turns per TF coil |
| `acasetf` |0.8559 | Inboard leg case area per coil (m2) |
| `acasetfo` |1.062 | Outboard leg case area per coil (m2) |
| `whtcas` |9.389e+05 | External case mass per coil (kg) |
| `tfc_model` |1 | TF coil model |
| `sigvert` |2.275e+08 | Vertical stress (Pa) |
| `sigrtf(1)` |2.442e-08 | Case radial stress (Pa) |
| `sigttf(1)` |-3.725e+08 | Case tangential stress (Pa) |
| `sigrcon` |-1.894e+08 | Conduit radial stress (Pa) |
| `sigtcon` |-2.688e+08 | Conduit tangential stress (Pa) |
| `s_tresca_case` |6e+08 | Tresca stress in case (MPa) |
| `s_tresca_cond` |4.963e+08 | Tresca stress in conduit (MPa) |
| `s_vmises_case` |5.247e+08 | von Mises stress in case (MPa) |
| `s_vmises_cond` |4.303e+08 | von Mises stress in conduit (MPa) |
| `deflect` |-0.006059 | Deflection at midplane (m) |
| `eyzwp` |8.705e+10 | Winding pack vertical Young's Modulus (Pa) |
| `windstrain` |0.002613 | Vertical strain on winding pack |
| `insstrain` |-0.009464 | Radial strain on insulator |
| `bc20m` |32.97 | Critical field at zero temperature and strain (T) |
| `tc0m` |16.06 | Critical temperature at zero field and strain (K) |
| `bmax` |11.32 | Peak field at conductor (T) |
| `thelium` |4.75 | Helium temperature at peak field (K) |
| `fhetot` |0.3396 | Total helium fraction inside cable space |
| `jcritsc` |4.924e+08 | Critical current density in superconductor (A/m2) |
| `jcritstr` |1.16e+08 | Critical current density in strand (A/m2) |
| `jwdgop` |2.204e+07 | Operating winding pack J (A/m2) |
| `jwdgcrt` |3.723e+07 | Critical winding pack current density (A/m2) |
| `icrit` |1.521e+05 | Critical current (A) |
| `iooic` |0.5919 | Operating current / critical current |
| `tmarg` |1.5 | Temperature margin (K) |
| `tmaxpro` |150 | Maximum temperature in quench (K) |
| `jwdgpro` |2.204e+07 | Maximum current density in winding pack given by quench protection (A/m2 |
| `vd` |8668 | Quench voltage (V) |
| `bmaxoh0` |11.24 | Maximum field at Beginning Of Pulse (T) |
| `jscoh_bop` |5.033e+08 | Critical superconductor current density at BOP (A/m2) |
| `jstrandoh_bop` |1.51e+08 | Critical strand current density at BOP (A/m2) |
| `rjohc0` |2.005e+07 | Allowable overall current density at BOP (A/m2) |
| `cohbop` |1.085e+07 | Actual overall current density at BOP (A/m2) |
| `bmaxoh` |11.23 | Maximum field at End Of Flattop (T) |
| `jscoh_eof` |5.039e+08 | Critical superconductor current density at EOF (A/m2) |
| `jstrandoh_eof` |1.512e+08 | Critical strand current density at EOF (A/m2) |
| `rjohc` |2.007e+07 | Allowable overall current density at EOF (A/m2) |
| `areaoh` |12.89 | CS overall cross-sectional area (m2) |
| `awpoh` |2.444 | CS conductor+void cross-sectional area (m2) |
| `awpoh*(1-vfohc)` |1.711 | CS conductor cross-sectional area (m2) |
| `awpoh*vfohc` |0.7331 | CS void cross-sectional area (m2) |
| `areaoh-awpoh` |10.44 | CS steel cross-sectional area (m2) |
| `oh_steel_frac` |0.8103 | CS steel area fraction |
| `sig_hoop` |2.526e+08 | Hoop stress in CS steel (Pa) |
| `sig_axial` |-3.474e+08 | Axial stress in CS steel (Pa) |
| `s_tresca_oh` |6e+08 | Tresca stress in CS steel (Pa) |
| `axial_force` |-1.814e+09 | Axial force in CS (N) |
| `vfohc` |0.3 | Void (coolant) fraction in conductor |
| `tmargoh` |1.5 | CS temperature margin (K) |
| `fcupfsu` |0.69 | Copper fraction in conductor |
| `sigpfcalw` |500 | Maximum permissible tensile stress (MPa) |
| `sigpfcf` |0.666 | JxB hoop force fraction supported by case |
| `rpf(01)` |6.526 | PF coil 01 radius (m) |
| `zpf(01)` |9.482 | PF coil 01 vertical position (m) |
| `pfdr01` |1.254 | PF coil 01 radial thickness (m) |
| `pfdz01` |1.254 | PF coil 01 vertical thickness (m) |
| `turns(01)` |410 | PF coil 01 turns |
| `ric(01)` |17.3 | PF coil 01 current (MA) |
| `bpf(01)` |5.314 | PF coil 01 field (T) |
| `rpf(02)` |6.526 | PF coil 02 radius (m) |
| `zpf(02)` |-10.6 | PF coil 02 vertical position (m) |
| `pfdr02` |1.336 | PF coil 02 radial thickness (m) |
| `pfdz02` |-1.336 | PF coil 02 vertical thickness (m) |
| `turns(02)` |465.5 | PF coil 02 turns |
| `ric(02)` |19.64 | PF coil 02 current (MA) |
| `bpf(02)` |5.717 | PF coil 02 field (T) |
| `rpf(03)` |18.96 | PF coil 03 radius (m) |
| `zpf(03)` |2.983 | PF coil 03 vertical position (m) |
| `pfdr03` |1.202 | PF coil 03 radial thickness (m) |
| `pfdz03` |1.202 | PF coil 03 vertical thickness (m) |
| `turns(03)` |205.3 | PF coil 03 turns |
| `ric(03)` |-8.662 | PF coil 03 current (MA) |
| `bpf(03)` |2.767 | PF coil 03 field (T) |
| `rpf(04)` |18.96 | PF coil 04 radius (m) |
| `zpf(04)` |-2.983 | PF coil 04 vertical position (m) |
| `pfdr04` |1.202 | PF coil 04 radial thickness (m) |
| `pfdz04` |-1.202 | PF coil 04 vertical thickness (m) |
| `turns(04)` |205.3 | PF coil 04 turns |
| `ric(04)` |-8.662 | PF coil 04 current (MA) |
| `bpf(04)` |2.767 | PF coil 04 field (T) |
| `rpf(05)` |17.28 | PF coil 05 radius (m) |
| `zpf(05)` |8.354 | PF coil 05 vertical position (m) |
| `pfdr05` |0.8288 | PF coil 05 radial thickness (m) |
| `pfdz05` |0.8288 | PF coil 05 vertical thickness (m) |
| `turns(05)` |127.8 | PF coil 05 turns |
| `ric(05)` |-5.496 | PF coil 05 current (MA) |
| `bpf(05)` |2.62 | PF coil 05 field (T) |
| `rpf(06)` |17.28 | PF coil 06 radius (m) |
| `zpf(06)` |-8.354 | PF coil 06 vertical position (m) |
| `pfdr06` |0.8288 | PF coil 06 radial thickness (m) |
| `pfdz06` |-0.8288 | PF coil 06 vertical thickness (m) |
| `turns(06)` |127.8 | PF coil 06 turns |
| `ric(06)` |-5.496 | PF coil 06 current (MA) |
| `bpf(06)` |2.62 | PF coil 06 field (T) |
| `rpf(nohc)` |2.872 | Central solenoid radius (m) |
| `zpf(nohc)` |0 | Central solenoid vertical position (m) |
| `ohdr` |0.8244 | Central solenoid radial thickness (m) |
| `ohdz` |15.63 | Central solenoid vertical thickness (m) |
| `turns(nohc)` |3579 | Central solenoid turns |
| `ric(nohc)` |-153.9 | Central solenoid current (MA) |
| `bpf(nohc)` |11.24 | Central solenoid field (T) |
| `ssq0` |0.0004496 | Sum of squares of residuals |
| `alfapf` |5e-10 | Smoothing parameter |
| `vstot` |-736.4 | Total volt-second consumption by coils (Wb) |
| `itfka` |90 | TF coil current (kA) |
| `ntfc` |16 | Number of TF coils |
| `vtfskv` |8.668 | Voltage across a TF coil during quench (kV) |
| `tchghr` |4 | TF coil charge time (hours) |
| `ltfth` |35.78 | Total inductance of TF coils (H) |
| `rcoils` |0 | Total resistance of TF coils (ohm) |
| `tfcv` |337.6 | TF coil charging voltage (V) |
| `ntfbkr` |16 | Number of DC circuit breakers |
| `ndumpr` |64 | Number of dump resistors |
| `r1dump` |0.09631 | Resistance per dump resistor (ohm) |
| `r1ppmw` |195 | Dump resistor peak power (MW) |
| `r1emj` |2264 | Energy supplied per dump resistor (MJ) |
| `ttfsec` |23.22 | TF coil L/R time constant (s) |
| `tfpsv` |354.4 | Power supply voltage (V) |
| `tfpska` |94.5 | Power supply current (kA) |
| `tfckw` |3.349e+04 | DC power supply rating (kW) |
| `tfackw` |3.722e+04 | AC power for charging (kW) |
| `rpower` |10.25 | TF coil resistive power (MW) |
| `xpower` |20.12 | TF coil inductive power (MVA) |
| `djmka` |0.125 | Aluminium bus current density (kA/cm2) |
| `albusa` |720 | Aluminium bus cross-sectional area (cm2) |
| `tfbusl` |3479 | Total length of TF coil bussing (m) |
| `albuswt` |676.4 | Aluminium bus weight (tonnes) |
| `rtfbus` |0.001266 | Total TF coil bus resistance (ohm) |
| `vtfbus` |113.9 | TF coil bus voltage drop (V) |
| `drarea` |5533 | Dump resistor floor area (m2) |
| `tfcfsp` |1852 | TF coil power conversion floor space (m2) |
| `tfcbv` |1.111e+04 | TF coil power conv. building volume (m3) |
| `xpwrmw` |22.36 | TF coil AC inductive power demand (MW) |
| `tfacpd` |11.39 | Total steady state AC power demand (MW) |
| `pfckts` |12 | Number of PF coil circuits |
| `spsmva` |336.3 | Sum of PF power supply ratings (MVA) |
| `spfbusl` |2568 | Total PF coil circuit bus length (m) |
| `pfbuspwr` |1080 | Total PF coil bus resistive power (kW) |
| `srcktpm` |1080 | Total PF coil resistive power (kW) |
| `vpfskv` |20 | Maximum PF coil voltage (kV) |
| `ensxpfm` |3.373e+04 | Maximum stored energy in poloidal field (MJ) |
| `peakpoloidalpower` |173.5 | Peak absolute rate of change of stored energy in poloidal field (MW) |
# Balance-of-plant

[:arrow_up:](#contents)

> 


### Constraints

| Constraint | Description | F-Value Name | F-Value Value | Limit Name | Limit | Value |
| --- | --- | --- | --- | --- | --- | --- |
| 16 | Net electric power lower limit | fpnetel | 1.00e+00 | pnetelin | 5.00e+02 | 5.00e+02 |
> **16** : **Net electric power lower limit** : set net electric power equal to `pnetelin` as `fpnetel` not iteration variable


### Inputs

| Input | Value | Description | Comment |
| --- | --- | --- | --- |
| `htpmw_div` | 0 | Divertor coolant mechanical pumping power (mw) | set to zero, so total (blkt, shld, div, fw) is 155 |
| `htpmw_fw` | 155 | First wall coolant mechanical pumping power (mw) | set to 155 MW, so total (blkt, shld, div, fw) is 155 |
| `ipowerflow` | 0 | Switch for power flow model: |  |
| `iprimshld` | 1 | Switch for shield thermal power destiny: |  |
| `etath` | 0.375 | Thermal to electric conversion efficiency |  |
| `htpmw_blkt` | 0 | Blanket coolant mechanical pumping power (mw) | set to zero, so total (blkt, shld, div, fw) is 155 |
| `htpmw_shld` | 0 | Shield and vacuum vessel coolant mechanical pumping power (mw) | set to zero, so total (blkt, shld, div, fw) is 155 |
### Outputs

| Output | Value | Description |
| --- | --- | --- |
| `basemw` |5 | Facility base load (MW) |
| `bdvmw` |0 | Divertor coil power supplies (MW) |
| `crymw` |42.14 | Cryoplant electric power (MW) |
| `htpmw..` |236 | Primary coolant pumps (MW) |
| `ppfmw` |123.4 | PF coil power supplies (MW) |
| `ptfmw` |11.39 | TF coil power supplies (MW) |
| `pheatingmw` |125 | Plasma heating supplies (MW) |
| `trithtmw..` |15 | Tritium processing (MW) |
| `vachtmw..` |0.5 | Vacuum pumps  (MW) |
| `pacpmw` |616.2 | Total pulsed power (MW) |
| `fcsht` |62.73 | Total base power required at all times (MW) |
| `emult` |1.269 | Neutron power multiplication in blanket |
| `fdiv` |0.115 | Divertor area fraction of whole toroid surface |
| `fhcd` |0 | H/CD apparatus + diagnostics area fraction |
| `1-fdiv-fhcd` |0.885 | First wall area fraction |
| `primary_pumping` |3 | Switch for pumping of primary coolant |
| `htpmwe_fw_blkt` |233.9 | Electrical pumping power for FW and blanket (MW) |
| `htpmwe_shld` |0.008356 | Electrical pumping power for shield (MW) |
| `htpmwe_div` |2.14 | Electrical pumping power for divertor (MW) |
| `htpmw` |236 | Total electrical pumping power for primary coolant (MW) |
| `fpumpshld` |0.005 | Coolant pump power / non-pumping thermal power in shield |
| `fpumpdiv` |0.005 | Coolant pump power / non-pumping thermal power in divertor |
| `pdivfraction` |0.1412 | Fraction of total high-grade thermal power to divertor |
|  |2602 | Total power leaving reactor (across vacuum vessel boundary) (MW) |
| `fachtmw` |62.73 | Heat removal from facilities (MW) |
| `htpsecmw` |30.68 | Coolant pumping efficiency losses (MW) |
| `pinjht` |75 | Heat removal from injection power (MW) |
| `trithtmw` |15 | Heat removal from tritium plant (MW) |
| `vachtmw` |0.5 | Heat removal from vacuum pumps (MW) |
| `tfcmw` |0 | TF coil resistive power (MW) |
| `psechtmw` |238.5 | Total low-grade thermal power (MW) |
| `pthermmw` |2650 | Total High-grade thermal power (MW) |
| `nphx` |3 | Number of primary heat exchangers |
| `pscalingmw` |321.4 | Transport power from scaling law (MW) |
|  |2395 | Total (MW) |
| `falpha*palpmw` |381.4 | Alpha power deposited in plasma (MW) |
| `pinjmw` |50 | Injected power deposited in plasma (MW) |
| `emultmw` |382.8 | Power from energy multiplication in blanket and shield (MW) |
| `htpmw_mech` |205.3 | Power deposited in primary coolant by pump (MW) |
| `pthermfw_blkt` |2274 | Heat extracted from first wall and blanket (MW) |
| `pthermshld` |1.461 | Heat extracted from shield  (MW) |
| `pthermdiv` |374.1 | Heat extracted from divertor (MW) |
| `psechcd` |0 | Nuclear and photon power lost to H/CD system (MW) |
| `pnetelmw.` |500 | Net electric power output(MW) |
| `pinjwp` |125 | Electric power for heating and current drive (MW) |
| `pfwp` |0.9596 | Electric power for PF coils (MW) |
| `pgrossmw` |993.8 | Gross electrical output* (MW) |
| `pnetelmw` |500 | Net electrical output (MW)	 |
| `rejected_main` |1656 | Heat rejected by main power conversion circuit (MW) |
| `pnetelmw/(powfmw+em` |20.9 | Net electric power / total nuclear power (%) |
| `pnetelmw/powfmw` |24.88 | Net electric power / total fusion power (%) |
| `cirpowfr` |0.4969 | Recirculating power fraction |
# Breeder-blanket

[:arrow_up:](#contents)

> 


### Inputs

| Input | Value | Description | Comment |
| --- | --- | --- | --- |
| `vfshld` | 0.60 | Coolant void fraction in shield |  |
| `secondary_cycle` | 2 | Switch for power conversion cycle: |  |
| `inuclear` | 1 | Switch for nuclear heating in the coils: |  |
| `etaiso` | 0.9 | Isentropic efficiency of fw and blanket coolant pumps |  |
| `qnuc` | 1.292e4 | Nuclear heating in the coils (w) (inuclear=1) | problem with nuclear heating after blanket size reduction. Input instead. |
| `etahtp` | 0.87 | Electrical efficiency of primary coolant pumps |  |
| `primary_pumping` | 3 | Switch for pumping power for primary coolant (06/01/2016): | Matti's BoP model |
### Outputs

| Output | Value | Description |
| --- | --- | --- |
| `fbltibe12` |0.375 | Titanium beryllide fraction |
| `fblli2sio4` |0.375 | Lithium orthosilicate fraction |
| `fblss_ccfe` |0.09705 | Steel fraction |
| `vfcblkt` |0.05295 | Coolant fraction |
| `vfpblkt` |0.1 | Purge gas fraction |
| `fw_armour_vol` |7.42 | First Wall Armour Volume (m3) |
| `volfw` |23.58 | First Wall Volume (m3) |
| `volblkt` |1445 | Blanket Volume (m3) |
| `volshld` |754 | Shield Volume (m3) |
| `vdewin` |1199 | Vacuum vessel volume (m3) |
| `fw_armour_mass` |1.428e+05 | First Wall Armour Mass (kg) |
| `fwmass` |1.839e+05 | First Wall Mass, excluding armour (kg) |
| `whtblkt` |3.619e+06 | Blanket Mass - Total(kg) |
| `whtbltibe12` |1.225e+06 | Blanket Mass - TiBe12 (kg) |
| `whtblli4sio4` |1.301e+06 | Blanket Mass - Li2SiO4 (kg) |
| `whtblss` |1.094e+06 | Blanket Mass - Steel (kg) |
| `armour_fw_bl_mass` |3.946e+06 | Total mass of armour, first wall and blanket (kg) |
| `whtshld` |2.352e+06 | Shield Mass (kg) |
| `vvmass` |9.355e+06 | Vacuum vessel mass (kg) |
| `ptfnuc` |0.04318 | Total nuclear heating in TF+PF coils (CS is negligible) (MW) |
| `pnucfw` |236 | Total nuclear heating in FW (MW) |
| `pnucblkt` |1568 | Total nuclear heating in the blanket (including emult) (MW) |
| `pnucshld` |1.454 | Total nuclear heating in the shield (MW) |
| `pnucdiv` |184.9 | Total nuclear heating in the divertor (MW) |
| `exp_blanket` |0.9999 | Blanket exponential factor |
| `exp_shield1` |0.001721 | Shield: first exponential |
| `exp_shield2` |0.2543 | Shield: second exponential |
| `secondary_cycle` |2 | Switch for plant secondary cycle |
| `fwpressure` |1.55e+07 | First wall coolant pressure (Pa) |
| `blpressure` |1.55e+07 | Blanket coolant pressure (Pa) |
| `nblktmodpi` |7 | No of inboard blanket modules poloidally |
| `nblktmodti` |32 | No of inboard blanket modules toroidally |
| `nblktmodpo` |8 | No of outboard blanket modules poloidally |
| `nblktmodto` |48 | No of outboard blanket modules toroidally |
| `fwarea` |1910 | First wall area (m2) |
| `rdewex` |20.06 | Cryostat internal radius (m) |
| `zdewex` |16.13 | Cryostat internal half-height (m) |
| `clh1` |6.394 | Vertical clearance from TF coil to cryostat (m) |
| `divsur` |192.4 | Divertor area (m2) |
| `divmas` |4.715e+04 | Divertor mass (kg) |
# Heating-and-current-drive

[:arrow_up:](#contents)

> The current DEMO baseline in PROCESS assumes only ECRH heating
 and current drive as PROCESS can only handle one type of HCD option.


### Constraints

| Constraint | Description | F-Value Name | F-Value Value | Limit Name | Limit | Value |
| --- | --- | --- | --- | --- | --- | --- |
| 30 | Injection power upper limit | fpinj | 1.00e+00 | pinjalw | 5.00e+01 | 5.00e+01 |
> **30** : **Injection power upper limit** : `fpinj` not iteration variable so power fixed at maximum `pinjalw`


### Inputs

| Input | Value | Description | Comment |
| --- | --- | --- | --- |
| `bscfmax` | 0.99 | Maximum fraction of plasma current from bootstrap; | keep bootstrap fraction to physical values |
| `gamma_ecrh` | 0.30 | User input ecrh gamma |  |
| `etaech` | 0.4 | Ech wall plug to injector efficiency |  |
| `pinjalw` | 50.0 | Maximum allowable value for injected power (mw) |  |
| `iefrf` | 10 | Switch for current drive efficiency model: |  |
### Outputs

| Output | Value | Description |
| --- | --- | --- |
| `pheat` |0 | Auxiliary power used for plasma heating only (MW) |
| `bigq` |39.52 | Fusion gain factor Q |
| `effcd` |0 | Current drive efficiency (A/W) |
| `gamcd` |0.3 | Normalised current drive efficiency, gamma (10^20 A/W-m2) |
| `etacd` |0.4 | Wall plug to injector efficiency |
| `bootipf` |0.367 | Bootstrap fraction |
| `faccd` |0.1108 | Auxiliary current drive fraction |
| `facoh` |0.5222 | Inductive fraction |
| `bootipf+faccd+facoh` |1 | Total |
| `echpwr` |50 | Electron cyclotron injected power (MW) |
| `echwpow` |125 | ECH wall plug power (MW) |
| `abs(vstot)` |736.4 | Total V-s capability of OH/PF coils (Wb) |
| `vssoft` |382.6 | Required volt-seconds during start-up (Wb) |
| `vsmax` |320.3 | Available volt-seconds during burn (Wb) |
# Tritium-fuelling-and-vacuum

[:arrow_up:](#contents)

### Outputs

| Output | Value | Description |
| --- | --- | --- |
| `rat` |1.3e-08 | First wall outgassing rate (Pa m/s) |
| `ogas` |0.0002075 | Total outgassing load (Pa m3/s) |
| `pbase` |0.0005 | Base pressure required (Pa) |
| `s(1)` |0.4149 | Required N2 pump speed (m3/s) |
| `snet(1)` |inf | N2 pump speed provided (m3/s) |
| `volume` |3065 | Plasma chamber volume (m3) |
| `pend` |0.1559 | Chamber pressure after burn (Pa) |
| `pstart` |0.001559 | Chamber pressure before burn (Pa) |
| `tdwell.` |0 | Dwell time between burns (s) |
| `s(2)` |inf | Required D-T pump speed (m3/s) |
| `snet(2)` |inf | D-T pump speed provided (m3/s) |
| `prdiv` |0.36 | Divertor chamber gas pressure (Pa) |
| `fhe` |0.2036 | Helium gas fraction in divertor chamber |
| `s(3)` |40.3 | Required helium pump speed (m3/s) |
| `snet(3)` |inf | Helium pump speed provided (m3/s) |
| `frate` |2.911e-05 | D-T fuelling rate (kg/s) |
| `s(4)` |40.3 | Required D-T pump speed (m3/s) |
| `snet(4)` |inf | D-T pump speed provided (m3/s) |
| `nduct` |16 | Number of large pump ducts |
| `d(imax)` |nan | Passage diameter, divertor to ducts (m) |
| `l1` |1.853 | Passage length (m) |
| `dout` |nan | Diameter of ducts (m) |
| `l2` |4.8 | Duct length, divertor to elbow (m) |
| `l3` |2 | Duct length, elbow to pumps (m) |
| `pumpn` |inf | Number of pumps |
| `qss/1.0d6` |0.02199 | Conduction and radiation heat loads on cryogenic components (MW) |
| `qnuc/1.0d6` |0.01292 | Nuclear heating of cryogenic components (MW) |
| `qac/1.0d6` |0.004439 | AC losses in cryogenic components (MW) |
| `qcl/1.0d6` |0.01958 | Resistive losses in current leads (MW) |
| `qmisc/1.0d6` |0.02652 | 45% allowance for heat loads in transfer lines, storage tanks etc (MW) |
| `helpow/1.0d6` |0.08546 | Sum = Total heat removal at cryogenic temperatures (W) |
| `tmpcry` |4.5 | Temperature of cryogenic components (K) |
|  |0.002028 | Efficiency (figure of merit) of cryogenic plant is 13% of ideal Carnot v |
| `crypmw` |42.14 | Electric power for cryogenic plant (MW) |
