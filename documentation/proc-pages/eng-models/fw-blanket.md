
# Armour, First Wall and Breeding Blanket

The surface facing the plasma is a thin layer of a material highly resistant to 
melting and erosion, such as tungsten, referred to "armour". It is cooled by 
conduction to the first wall underneath.

The first wall sits behind the armour, and is dedicated to removing the heat 
landing on the armour. It does not breed tritium. Due to the hostile environment 
the first wall and armour have only a short lifetime and therefore need to be 
replaced regularly. It is cooled either by gaseous helium or by pressurised 
liquid water, depending on the selection of blanket type using the switch 
`blkttype`.

## Wall Load Calculation

Switch `iwalld` determines whether the neutron wall load (power per unit area) 
should be calculated using the plasma surface area (`iwalld = 1`) or the first 
wall area (`iwalld = 2`) as the denominator. In the former case, input 
parameter `ffwal` (default value 0.92) can be used to scale the neutron power 
reaching the first wall.

The breeding blanket performs a number of tasks. An incoming neutron from a
deuterium-tritium (D-T) fusion reaction in the plasma loses energy in the
blanket. This energy is removed by the blanket coolant and used to produce
electricity. The neutron may also react with a lithium nucleus present in the
blanket to produce ("breed") a tritium nucleus which can be re-used as
fuel. The competing requirements of heating and tritium synthesis mean that a
neutron multiplier must be present, to ensure balance between tritium
destruction and creation. The blanket therefore contains beryllium to fulfil
this purpose. As with the first wall, the blanket has a relatively short
lifetime because of the high neutron fluence.

### Blanket Model Options

The models used for the thermoydraulics of the first wall, the profile of 
deposition of the neutron energy, tritium breeding, and conversion of heat to 
electricity have been revised extensively.

`iblanket` -- This switch selects between different types of blanket.

- `== 1` -- CCFE HCPB (helium-cooled pebble bed) model. The energy 
    deposition in the armour and first wall, blanket and shield are calculated 
    using parametric fits to an MCNP neutron and photon transport model of a 
    sector of a tokamak. The blanket contains lithium orthosilicate 
    Li$_4$SiO$_4$, titanium beryllide TiBe$_{12}$, helium and Eurofer steel. 
- `== 2` -- KIT HCPB model. It allows the energy multiplication factor `emult`, 
    the shielding requirements and tritium breeding ratio to be calculated 
    self-consistently with the blanket and shielding materials and sub-assembly 
    thicknesses, and for constraints to be applied to satisfy the engineering 
    requirements. For further details of this model.
- `== 3` -- CCFE HCPB model with tritium breeding ratio. It has the features of 
    the CCFE HCPB model above, with a set of fitting functions for calculating 
    tritium breeding ratio (TBR).  It requires a choice of `iblanket_thickness`, 
    specifiying a `THIN`, `MEDIUM` or `THICK` blanket. This fixes the values 
    of inboard and outboard blanket thickness, and the initial values of first 
    wall thickness (3 cm) and first wall armour (3 mm). Note that these last 
    two can be modified by the first wall thermohydraulic module, in which case 
    the output will not be fully self-consistent. The lithium-6 enrichment and 
    the breeder fraction (Li4SiO4/(Be12Ti+Li4SiO4) by volume) are available as 
    iteration variables, and the minimum TBR can be set as a constraint. The 
    maximum values of TBR achievable are as follows:

    - `THIN` -- 1.247
    - `MEDIUM` -- 1.261
    - `THICK` -- 1.264.

`secondary_cycle` -- This switch controls how the coolant pumping power in the 
first wall and blanket is determined, and also how the calculation of the plant's 
thermal to electric conversion efficiency (the secondary cycle thermal 
efficiency) proceeds.

### KIT Blanket Neutronics Model

The model used if `blktmodel = 1` is based on the Helium-Cooled Pebble
Bed (HCPB) blanket concept developed by KIT (a second advanced model --
Helium-Cooled Lithium Lead, HCLL -- will be implemented in due course). The
blanket, shield and vacuum vessel are segmented radially into a number of
sub-assemblies. Moving in the direction away from the plasma/first wall, these
are:

Breeding Zone (BZ) (which includes the first wall), with radial
thicknesses (inboard and outboard, respectively) `fwith + blbuith`,
`fwoth+blbuoth`. This consists of beryllium (with fraction by volume `fblbe`), 
breeder material (`fblbreed`), steel (`fblss`) and helium coolant. Three 
forms of breeder material are available: 
  
| `breedmat` | Description |
| :-: | - |
| 1 | lithium orthosilicate (Li$_4$SiO$_4$) |
| 2 | lithium metatitanate (Li$_2$TiO$_3$) |
| 3 | lithium zirconate (Li$_2$ZrO$_3$) |

The $^6$Li enrichment percentage may be modified from the default 30% using 
input parameter `li6enrich`.

- Box Manifold (BM), with radial thicknesses (inboard and outboard,
  respectively) `blbmith`, `blbmoth` and helium fractions `fblhebmi`, `fblhebmo` 
  (the rest being steel).
- Back Plate (BP), with radial thicknesses (inboard and outboard,
  respectively) `blbpith`, `blbpoth` and helium fractions `fblhebpi`, `fblhebpo` 
  (the rest being steel).

Together, the BZ, BM and BP make up the `blanket`, with total radial
thicknesses `blnkith` (inboard) and `blnkoth` (outboard), and void (coolant) 
fraction `vfblkt`; Note that these quantities are `calculated` from the 
sub-assembly values if `blktmodel > 0`, rather than being input parameters.

Low Temperature Shield and Vacuum Vessel (lumped together for these 
calculations), with radial thicknesses (inboard and outboard, respectively) 
`shldith + d_vv_in`, `shldoth + d_vv_out` and **water** coolant fraction 
`vfshld` (the rest being assumed to be steel for its mass calculation; the 
neutronics model assumes that the shield contains 2% boron  as a neutron absorber, 
but this material is not explicitly mentioned elsewhere in the code -- so 
its cost is not calculated, for example).

!!! Note "Note" 
    The fact that water is assumed to be the coolant in the shield, whereas 
    helium is the coolant in the blanket, leads to an inconsistency when 
    specifying the coolant type via switch `coolwh`. At present we mitigate this 
    by forcing `coolwh=2` (making water the coolant), as in this case the
    coolant mass and pumping costs are higher, giving the more pessimistic
    solution with regards to costs.

A few other input parameters are useful for tuning purposes, as follows:

| Parameter | Description |
|:-: | -|
| `fvolsi/fvolso` | area (and volume) coverage factors for the inboard and outboard shields, respectively. |
| `fvoldw` | multiplier for volume of vacuum vessel, used in mass calculations to account for ports, etc. |
| `npdiv` | number of divertor ports, used in the calculation of the tritium breeding ratio. |
| `nphcdin/nphcdout` | number of heating/current drive ports on the inboard and outboard sides, respectively, used in the calculation of the tritium breeding ratio. These may be either 'small'  (`hcdportsize = 1`) or 'large' (`hcdportsize = 2`). |
| `wallpf` | neutron wall load peaking factor (maximum/mean), used in the calculation of the blanket lifetime. |
| `ucblbreed` | unit cost (\$/kg) of the breeder material |

#### KIT model outputs and available constraints

The KIT blanket model has the following available constraints

| Constraint No. | F-value   | F-value No. | Limit       | Description |
| :------------: | :-------: | :---------: | :---------: | ----------- | 
| 52             | `ftbr`    | 89          | `tbrmin`    | Min required `tbr` |
| 53             | `fflutf`  | 92          | `nflutfmax` | Max allowed TF fluence |
| 54             | `fptfnuc` | 95          | `ptfnucmax` | Max allowed heating of TF coils |
| 55             | `fvvhe`   | 96          | `vvhealw`   | Max allowed He concentration in VV |

The KIT blanket neutronics model provides the following outputs:

| Output      | Units    | Itvar. | Description |
| :---------: | :------: | ------ | ----------- |
| `pnucblkt`  | MW       | -      | Total nuclear power deposited in blanket |
| `pnucshld`  | MW       | -      | Total nuclear power deposited in shield | 
| `emult`     | -        | -      | The energy multiplication factor in the blanket |
| `tbr`       | -        | -      | Tritium breeding ratio |
| `blbuith`   | m        | 90     | Inboard blanket thickness |
| `blbuoth`   | m        | 91     | Outboard blanket thickness |
| `tritprate` | -        | -      | The tritium production rate in grammes/day is calculated. |
| `nflutfi`   | n/m$^2$  | -      | The fast neutron fluence on the inboard TF coils |
| `nflutfo`   | n/m$^2$  | -      | The fast neutron fluence on the inboard TF coils |
| `shldith`   | m        | 93     | Inboard shield thickness |
| `shldoth`   | m        | 94     | Outboard shield thickness |
| `pnuctfi`   | MW/m$^3$ | -      | Nuclear heating power on inboard TF coil |
| `pnuctfo`   | MW/m$^3$ | -      | Nuclear heating power on outboard TF coil |
| `vvhemini`  | appm     | -      | Min He concentration in the inboard VV at the end of the plant lifetime |
| `vvhemaxi`  | appm     | -      | Max He concentration in the inboard VV at the end of the plant lifetime |
| `vvhemino`  | appm     | -      | Min He concentration in the outboard VV at the end of the plant lifetime |
| `vvhemaxo`  | appm     | -      | Max He concentration in the outboard VV at the end of the plant lifetime |
| `bktlife`   | fp-yrs   | -      | Blanket lifetime in full power years assuming max damage ~60 dpa|
