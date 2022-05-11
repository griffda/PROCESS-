# Hybrid Scenario

The hybrid plasma is designed as a near steady state H-mode scenario which efficiently uses the flux needed for long pulsed operation to maximise the flat top operation time and create high neutron fluency [1].

This is done by operating with a substantial bootstrap current to allow for a low fraction of current which is inductively driven. The efficient operation which maximises fusion power $P_{fus}$ and minimises the current drive power $P_{CD}$ is achieved by maximising normalised plasma beta $\beta_N$.

The creation of the high $\beta_N$ and low plasma current regime $I_p$ requires a high safety factor $q_{95}$ while maintaining a flat hollow q-profile in the plasma core with $q_0$ slightly above one to avoid sawtooth oscillations. These demands require good control of plasma shape to keep the q-profile hollow and to suppress neoclassical tearing modes [2].

Below is a list of constraints, iteration variables and input that are relevant for a power plant design with plasma operation in a hybrid scenario.

Note: the PROCESS 0D models have trouble capturing the physics of the hybrid scenario, for instance q-profile is the always assumed to be parabolic, and the way the plasma current is calculated for a given does not always given an optimum result.

### Constraint Equations

* `icc = 15` - LH power threshold limit.
* `icc = 30` - Injection power upper limit.

### Iteration Variables

* ixc = 18 - `q95` is the safety factor near plasma edge. We use a lower a higher lower bound of `boundl(18) = 4.5`.
* ixc = 44 - `fvsbrnni` is the fraction of the plasma current produced by non-inductive means. We seek a large bootstrap current so a lower limit of `boundl(44) = 0.616` is enforced.
* ixc = 46 - `fpinj` is the f-value for injection power. Allow the `pinj` to vary.
* ixc = 103 - `flhthresh` is the f-value for the L-H threshold. A Hybrid plasma scenario is a H-mode plasma.

### Scaling Relationships and Switches

* `pheat = 50.0` - Heating power used from current drive. This is lowered from the values uses in H-mode EU-DEMO baseline runs as we want injected power used as current drive.
* `iefrf = 5,8` - Switch for current drive efficiency model – we use an NBI current drive.
* `pinjalw = 175.0` - Maximum allowable value for injected power – Choose a high injected power.
* `isc = 34` - IPB98(y,2) H-mode. The confinement time scaling used for Hybrid scenarios.
* `hfact = 1.2` - H factor on energy confinement times, radiation corrected. Hybrid plasmas are expected to have strong confinement justifying an increased h factor.
* `pnetelin = 200.0` - Required net electric power. Low plasma current solution cannot be found unless lower the constraint on net electric power.


[^1]: E. Joffrin, Plasma Phys. Control. Fusion 49 (2007) B629-B649

[^2]: A.C.C Sips et al, Plasma Phys. Control Fusion 47 (2005) A19- A40