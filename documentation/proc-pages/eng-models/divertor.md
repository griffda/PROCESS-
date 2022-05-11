
# Divertor

The divertor provides a means of removing plasma reaching the scrape-off layer. 
The principal outputs from the code are the divertor heat load, used to 
determine its lifetime, and its peak temperature. The divertor is cooled either 
by gaseous helium or by pressurised water.

Switch `snull` controls the overall plasma configuration. Setting `snull = 0` 
corresponds to an up-down symmetric, double null configuration, while 
`snull = 1` (the default) assumes a single null plasma with the divertor at the 
bottom of the machine. The vertical build and PF coil current scaling 
algorithms take the value of this switch into account, although not the plasma 
geometry at present.

The Harrison-Kukushkin-Hotston divertor model[^1] developed for ITER is 
available, but is unlikely to be relevant for a reactor.

[^1]: N.A. Uckan and ITER Physics Group, 'ITER Physics Design Guidelines: 1989',
ITER Documentation Series, No. 10, IAEA/ITER/DS/10 (1990)