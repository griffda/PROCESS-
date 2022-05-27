# L-mode

Below is a list of constraints, iteration variables and input parameters that are relevant for a power plant design and finding feasible solutions with PROCESS when operating in L-mode.  

### Constraint Equations

* `icc = 15` - Equation for the L-H threshold limit. Must operate below this limit

### Iteration Variables

* `ixc = 103` - `flhthresh` - This will need to be bounded to stop it ever going to 1.0 or greater

### Scaling Relations and switches

* `ipedestal = 0` - Force density and temperature to have parabolic profiles.
* `isc = 3,5,6,7,8,9,10,11,12,14,15,16,17,18,28,30` - Switch for energy confinement time scaling laws valid for L-mode plasmas.

A guide for obtaining a new feasible solution with PROCESS for an EU-DEMO like L-mode scenario:

* Starting with a well understood H-mode DEMO the energy confinement time scaling relation is switched to an L-mode scaling (e.g. `isc=28`).
* When run this input.dat will fail to find a solution, but the output will calculate the H-factor needed to satisfy the 0D energy conservation equations. This value can be around $>4$. We then scan downwards in `hfact` in small increments to find solutions in `hfact` approaching 1.1.
* We then switch to parabolic density and temperature profiles and rerun to find new solution.
* To aid in finding a feasible solution `pheat` (`ixc = 11`) was added as an iteration variable.
* Scan downward in `flhthresh` to reach L-H threshold limit.
