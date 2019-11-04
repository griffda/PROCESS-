# Plasma Scenarios: QH Mode

The quiescent H-mode, or QH-mode, is a low collisionality limit of H-mode physics. It possesses temperature and density pedestals and confinement times comparable with H-mode plasmas but does not exhibit ELMs [1].

A QH-mode plasma is operated in a regime near the peeling limit of the stability diagram, this regime is accessed best with strong plasma shaping, counter current NBI current drive and high cryopumping to produce low edge densities [2]. The low pedestal densities lead to a low collisionality system which maximises the pedestal bootstrap current for a given pressure gradient. The high edge current drives the plasma towards the low-n peeling mode limit of the stability diagram.

The QH-mode is observed to be accompanied by the appearance of an edge harmonic oscillation (EHO) which are low-n modes localised in the plasma edge, which theory and experiments have shown is driven by large edge $`\mathbf{E}\times\mathbf{B}`$ rotational shear. In typical studies of the role of toroidal rotation shear on the growth rates of peeling-ballooning instabilities it is found that the rotational shear stabiles the high-n modes while destabilising the low-n modes [3]. Whereas in the QH-mode scenario the low-n (typically n<5) EHO mode will damp the sheared flow in the edge and experiments have shown the low-n modes also strongly couple to resistive wall modes (RWM) [4], these effects aiding the modes saturation.

Putting these pieces together we create a complicated picture in which destabilising current and rotation in the plasma edge drives a high-n linear kink-ballooning instability, as it reaches a large amplitude it can generate non-linear instabilities which damp the system and saturate the mode at a finite amplitude. These non-linear modes create magnetic perturbations that allow for particle and current transport across the field, suppressing the typical ELMing behaviour of H-mode.

Most devices operating in QH-mode have used NBI torque by injecting in the counter current direction to create the necessary shear flows in the plasma edge. Experiments have shown that there is a critical $`\mathbf{E}\times\mathbf{B}`$ threshold for operation in QH-mode, below which normal ELMing H-modes are observed.

1. E. Viezzer Nucl. Fusion 58 (2018) 115002
2. K.H. Burrell et al, Physics of Plasmas 8 (2001) 2153
3. P.B. Snyder et al, Nucl. Fusion 47 (2007) 961-968
4. Xi Chen et al, Nucl. Fusion 56 (2016) 076011

Below is a list of constraints, iteration variables and input that are relevant for a power plant design with plasma operation in QH-mode.

Constraint Equations:

* `icc = 15` – the L-H threshold
* `icc = 30` – The injection power upper limit

Iteration Variables:

* `ixc = 103` * `flhthresh` * f-value for L-H threshold constraint equation

Scaling Relationships and Switches:

* `ilhthresh = 6` * Martin 2008 scaling
* `isc = 2,4,13,19,20,24,26,27,29,31,32,33,34,35,36,39,40,41,42,47` * switch for energy confinement time scaling law – It is recommend to use option `isc=34` for IPB98(y,2)
* `hfact = 1.1` * H factor on energy confinement times, radiation corrected
* `ipedestal = 1,2,3` * Switch for pedestal profiles – we need to operate with pedestals in temperature and density
* `fgwsep = 0.5` * fraction of Greenwald Density to set as separatrix density – we use a reduced value of the separatrix density
* `fgwped = 0.9` * fraction of Greenwald Density to set as pedestal-top density
* `iefrf = 5,8` * switch for current drive efficiency model – use a model for NBI current drive
* `pinjalw = 75.0` * maximum allowable value for injected power – This should be chosen to be a significant value to give the high injected NBI torque needed to generate shear flows in the plasma edge  
