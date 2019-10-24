
# Introduction

The PROCESS Python utilities are located in the repository folder

```
/utilities/
```

## PROCESS 2-Page Summary

> `/utilities/plot_proc.py`

A utility to produce a two-page summary of the output, including the major
parameters, poloidal and toroidal cross-sections, and temperature and density 
profiles.

### Usage

```bash
python plot_proc.py [-h] [-f FILENAME] [-s]
```

If no `-f` argument is provided it assumes a file named `MFILE.DAT` is in the 
current directory.

### Output

Produces a two-page PDF file called `SUMMARY.pdf`

### Options

| Argument | Description | 
| - | - |
| `-h --help`   | show help message and exit
| `-f FILENAME` | specify input/output file prefix
| `-s, --show`  | show plot as well as saving figure

## Randomised Starting Point

> `./utilities/run_process.py`

This program runs PROCESS many times, randomly varying the initial values of 
the iteration variables until a feasible solution is found.  Stops when a 
feasible solution is found.  If in the course of a scan some scan points result 
in feasible solutions and some do not, `run_process.py` will stop if and only 
if the number of unfeasible scan points is less than or equal to the 
parameter `NO_ALLOWED_UNFEASIBLE`.

### Usage

```bash
python run_process.py [-h] [-f CONFIGFILE]
```

If no `-f CONFIGFILE` is provided the utility assumes a configuration file in 
the local directory called `run_process.conf`.

### Configuration File

The configuration file `run_process.conf` has the following style:

```
* This is a comment in the config file!

* Path to working directory in which PROCESS is run.
WDIR = Run1
* original IN.DAT name
ORIGINAL_IN_DAT = demo1a_10_sep_13.IN.DAT
* PATH to PROCESS binary
PROCESS = ~PROCESS/master/process
* ONE line comment to be put into README.txt
COMMENT = This is a test DEMO run! ;-)
* Maximum number of PROCESS runs
NITER = 5
* integer seed for random number generator; use None for random seed
SEED = 2
* factor within which the iteration variables are changed
FACTOR = 1.5
* Number of allowed unfeasible points that do not trigger rerunning.
NO_ALLOWED_UNFEASIBLE = 0
* include a summary file with the iteration variables at each stage.
INCLUDE_ITERVAR_DIFF = True
* add iteration variables - comma separated
ADD_IXC = 99, 77
* remove iteration variables - comma separated
DEL_IXC =
* add constraint equations  - comma separated
ADD_ICC = 57,
* remove constraint equations - comma separated
DEL_ICC =
* set any variable to a new value
* the variable does not have to exist in IN.DAT
VAR_TFTORT = 1.9
VAR_EPSVMC = 1e-4
* remove variables
DEL_VAR_IITER
```

### Output

A directory is created for each run. containing all the standard PROCESS code 
output, a `process.log` log file, and `README.txt` containing comments from 
config file

### Options

| Argument | Description |
| - | - |
| `-h --help` | show help message and exit |
| `-f CONFIGFILE --configfile CONFIGFILE` | configuration file, default = run_process.conf |

## Sankey Diagram

> `./utilities/plot_sankey.py`

The power flows of the power plant will be extracted from MFILE.DAT and used to populate a
Sankey diagram. The diagram will start from the initial fusion power and show all of the inputs 
and outputs for the power flows. The Recirculated will finish to connect with the initial
fusion power.

### Usage

```
python plot_sankey.py [-h] [-e END] [-f MFILE] [-s]
```

### Output

A .pdf file is created called 'SankeyPowerFlow.pdf', and 'SankeyPowerFlow_simplified.pdf' if
the -s option is used, in the directory the utility was run. 

### Options

| Argument | Description |
| - | - |
| `-h --help`       | show help message and exit      |
| `-e --end`        | file format, default = pdf      |
| `-f --mfile`      | mfile name, default = MFILE.DAT |
| `-s, --simplified`| Plot a simplified version       |
