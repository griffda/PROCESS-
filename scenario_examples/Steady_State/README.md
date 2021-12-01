# Plasma Scenarios: L-mode

Below is a list of constraints, iteration variables and input parameters that are relevant for a power plant design and finding feasible solutions with PROCESS when operating in a steady-state configuration.  

Constraint Equations:

* `icc = 12` * Volt second lower limit – Instread of the burn time limit we now only require the volt seconds for start up
* `icc = 15` – the L-H threshold 
* `icc = 30` – The injection power upper limit - We will need to increase this to allow for large current drive

Iteration Variables:

* `ixc = 103` * `flhthresh`
* `ixc = 1` * `aspect` - Needed for achieving large bootstrap currents  

Scaling Relationships and Switches:

* `ilhthresh = 6` * Martin 2008 scaling
* `isc = 2,4,13,19,20,24,26,27,29,31,32,33,34,35,36,39,40,41,42,47` * switch for energy confinement time scaling law – It is recommend to use option `isc=34` for IPB98(y,2)
* `fvsbrnni = 1.0` *  fraction of the plasma current produced by non-inductive means
* `hfact = 1.4` * H factor on energy confinement times, radiation corrected - Need high plasma proformance.
* `ipedestal = 1,2,3` * Switch for pedestal profiles – we need to operate with pedestals in temperature and density
* `pinjalw = 225.0` * maximum allowable value for injected power – This should be chosen to be a significant value to give the high injected NBI torque needed to generate shear flows in the plasma edge  
