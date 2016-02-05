# PROCESS

PROCESS is the reactor systems code at [CCFE](www.ccfe.ac.uk). More information on PROCESS
can be found on the PROCESS [webpage](http://www.ccfe.ac.uk/powerplants.aspx).

## Documentation

### User guide
To read about how the code works and the modules in it see the
[user guide](http://www.ccfe.ac.uk/assets/Documents/Other/process.pdf)

### Physics paper

A [paper](http://www.sciencedirect.com/science/article/pii/S0920379614005961)
outlining the physics models in PROCESS published in fusion engineering and design.

### Engineering paper

A [paper](http://www.euro-fusionscipub.org/wp-content/uploads/2015/08/WPPMIPR1505.pdf)
outlining the engineering models in PROCESS published in fusion engineering and design.

### Other papers

A list of other papers using PROCESS:

- "Impurity radiation in DEMO systems modelling", H. Lux et al., 2015, fusion engineering and
design, ([paper](http://www.sciencedirect.com/science/article/pii/S0920379615302891))
- "Implications of toroidal field coil stress limits on power plant design using PROCESS", J. Morris et al.,
SOFT 2014, fusion engineering and design ([paper](http://www.sciencedirect.com/science/article/pii/S0920379615301290)).


## Build

- for compiling on CCFE fusion machines add to your bashrc:
    - `module unload ifort/10.0.023`
    - `module load gcc/4.8.2`
- get repository
    - `git clone git@git.ccfe.ac.uk:process/process.git folder_name`. Where folder name
    is the name of the folder you want to save PROCESS to.
- inside PROCESS directory run `make`

Additionally

- to make python dictionaries run `make dicts`
- to make documentation run `make doc`
- to make everything run `make all`
- to clean directory run `make clean`

## Run

- create input file IN.DAT
- run `./process.exe`
- results output in OUT.DAT, MFILE.DAT

## Contacts

[Richard Kembleton](richard.kembleton@ccfe.ac.uk)

[Micheal Kovari](michael.kovari@ccfe.ac.uk)

[Hanni Lux](Hanni.lux@ccfe.ac.uk)

[James Morris](james.morris2@ccfe.ac.uk)
