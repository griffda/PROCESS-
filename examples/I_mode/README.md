# Plasma Scenarios: I Mode

The improved energy confinement mode (I-mode) is a regime of plasma operation characterised by an increased temperature gradient and high energy confinement times as compared to L-mode but with a lack of ELMs. The energy confinement factor $`H_{98}(y,2)`$ in I-mode is typically 0.6-1.0, but in some shots with high toroidal magnetic field values up to 1.2 have been observed [1].

The I-mode regime is accessed in the low density and low edge collisionality $`\nu^\ast`$ regime. Stability analysis shows that I-mode is accessed well below the peeling-ballooning limit, consistent with the lack of ELMs [2].

Measurements on several machines has shown that the transition from L-mode to I-mode characterised by distinct changes in turbulence in the pedestal region. Low frequency broadband fluctuations decrease while a weakly coherent mode (WCM) in high frequency density and magnetic fluctuations emerges [3], additionally a geodesic acoustic mode (GAM) localised at the edge of the plasma at low frequency [4]. These modes act to drive a decrease in the thermal conductivity in the plasma edge. These modes appear together and while are observed well separated in frequency space show strong evidence of mode coupling driven by non-linearities. The lack of a density pedestal means we expect high particle transport which will lower impurity concentration and remove Helium ash.  

A robust demonstration of I-mode needs a plasma operated in the unfavourable ion $`\nabla B`$ drift direction to mitigate the transition to H-mode and allow for stable I-mode access. Therefore, the L-I threshold can be twice the LH threshold power, an empirical L-I power threshold scaling has been created and shows

```math
P_{LI}=0.162\ \bar{n}_{20}\ S\ B_T^{0.26}
```

where $`\bar{n}_{20}`$ is the line averaged electron density measured in units $`{10}^{20}m^{-3}`$, while $`S`$ is the plasma surface area and $`B_T`$ the toroidal magnetic field [5]. We see form this is that it is favourable to enter the I-mode regime to have a low density plasma and therefore we expect a lower fusions power than might be considered in a H-mode plasma. Due to the high LI threshold power a reactor relevant device operating in I-mode requires a large source of plasma heating. This mode has been observed in a wide range of machines with plasmas which are heated by different mechanism such as ICRH, ECRH and NBI and in machines with differing wall materials.

1. E. Viezzer, Nucl. Fusion 58 (2018) 115002
2. A.E. Hubbard et al, Nucl. Fusion 56 (2016) 086003
3. P. Manz et al, Nucl. Fusion 55 (2015) 083004
4. I. Cziegler et al, Phys. Plasma 20 (2013) 055904
5. A.E. Hubbard et al, Nucl. Fusion 57 (2017) 126039

Below is a list of constraints, iteration variables and input that are relevant for a power plant design with plasma operation in I-mode.

Constraint Equations:

* `icc = 5` – Density upper limit
* `icc = 15` – The L-H threshold
* `icc = 80` – Divertor power lower limit pdivt
* `icc = 30` – The injection power upper limit

Iterations Variables:

* `ixc = 6` – `dene` * electron density – I-mode is a low density regime so a lower initial value for the electron density is suggested, for example `dene = 7.432e19`.
* `ixc = 9` – `fdene` * f-value for density limit constraint equation – I-mode is a low density regime so an upper bound of 0.9 is recommended.
* `ixc = 46` – `fpinj` * f-value for injection power upper limit constraint equation – The high Psep needed to satisfy the LI threshold means that large injected powers are required for I-mode.  
* `ixc = 103` – `flhthresh` * f-value for L-H threshold constraint equation
* `ixc = 153` – `fdivlim` * f-value for the lower limit to pdivt constraint equation
* `ixc = 145` – `fgwped` * fraction of the Greenwald density to set as pedestal-top density – suggested initial value is `fgwped = 0.65` with a `boundl(145) = 0.65` and `boundu(145) = 0.72`. These lower values are chosen to reduce the density pedestal in I-mode.

Scaling Relations and Switches:

* `ilhthresh = 18` * Hubbard et al 2017 L-I threshold scaling – This produces the high Psep threshold needed to enter I-mode
* `isc = 43,44,45` * Hubbard 2017 I-mode – These confinement time scaling’s where made using Alcator C-Mod data, with high B-field but Ip = 1.1MA and PCD = 3MW. If running an I-mode scenario in non-Alcator C-Mod like machine it is recommend to use the IPB98(y,2) H-mode scaling (isc =34), but with radiation corrected h-factor (hfact) of around 0.8.
* `pdivtlim = 200.0` * This needs to be value near but below the expected LI threshold power, this is introduced to force the solver to explore the correct areas of the function parameter space.
* `fgwsep = 0.5` * fraction of Greenwald Density to set as separatrix density
* `ipedestal = 1,2,3` * switch for pedestal profiles
* `pinjalw = 150.0` * Maximum allowed injected power
* `eped_sf = 0.9` * Adjustment factor EPED scaling to reduce pedestal temperature or pressure to mitigate or prevent ELMs
