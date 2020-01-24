
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

## Morris Method

> `./utilities/morris_method.py`

Program to evaluate model sensistivity by elementary effects method at a given PROCESS design point. The method of Morris is a technique for screening a large number of model parameters to identify the dominatant parameter for a global sensitivity analysis[1], more details can be found in textbook[2]. Note that this utility has a significanity longer run time that a typical evalution of PROCESS design points

dependanceas - SALib
the two types of config files (how is uses run_process.py)
what files it outputs  - .txt files I beleive  

[1] M. Morris, (1991) "Factorial Sampling Plans for Preliminary Computational Experiments." Technometrics, 33(2):161-174

[2] A. Saltelli, S. Tarantola, F. Campolongo, M. Ratto, T. Andres, J. Cariboni, D. Gatelli and M. Saisana, (2008) "Global Sensitivity Analysis: The Primer" (New York: Wiley)

### Usage

```bash
usage: morris_method.py [-h] [-f CONFIGFILE] [-i INPUTFILE] [-o OUTPUTVARNAME]
                        [-s SOLLIST] [-e ERRORLIST] [-m OUTPUTMEAN]
                        [-t TRAJNUM] [-n NUMLVLS]
```

### Configuration File

The configuration file `morris_method_conf.json` used the JSON format and has the following style

```
{
    "bounds": [
        [
            1.1,
            1.3
        ],
        [
            1.0,
            1.2
        ],
        [
            0.45,
            0.75
        ],
        [
            0.085,
            0.115
        ],
        [
            1e-05,
            0.0001
        ],
    ],
    "names": [
        "boundu(9)",
        "hfact",
        "coreradius",
        "fimp(2)",
        "fimp(14)",
    ],
    "num_vars": 5
}
```
In addition the utility also uses `run_process.py` and therefore can optionally use the configuation file `run_process.conf`.


### Output

As this utility uses the `run_process.py` tool it produces the same output files and in addition the utility produces several output files all in a .txt format. The file `param_values.txt` is created in the same directory as the python tool which lists all the parameter inputs generated by the trajectories which are used in PROCESS model runs. Three files are produced in the folder containing all the output files. Firstly, `capcost_sol.txt` which lists all final figures of merits found in PROCESS runs, the filename can be changed with SOLLIST option. Then secondly, `error_log.txt` wihch list all the run numbers that failed to find a converged solution, the filename can be changed with ERRORLIST option. Finally, the file `morris_method_output.txt` contains a table of the output of the method, with mean and variance of the elementary effects.

### Options

| Argument | Description |
| - | - |
| `-h, --help`      | show this help message and exit                                   |
| `-f CONFIGFILE`   | configuration file, default = run_process.conf                    |
| `-i INPUTFILE`    | input parameters file, default = morris_method_conf.json          |
| `-o OUTPUTVARNAME`| PROCESS output analysed, default = capcost                        |
| `-s SOLLIST`      | filename of PROCESS outputs, default = capcost_sol.txt            |
| `-e ERRORLIST`    | filename of failed PROCESS output, default = error_log.txt        |
| `-m OUTPUTMEAN`   | PROCESS mean model output value, default = 8056.98 (DEMO capcost) |
| `-t TRAJNUM`      | number of trajectories sampled, default = 25                      |
| `-n NUMLVLS`      | Number of grid levels used in hypercube sampling, default = 4     |

## Morris Plotting

> `./utilities/morris_plotting.py`

creates a scatter plot from 

### Usage

```bash
usage: morris_plotting.py [-h] [-f DATAFILE] [-o OUTPUTFILE]
```

Program to plot the output of the the sensistivity analysis by elementary
element method at a given PROCESS design point.

### Configuration File

well its gonna read morris_method_output.txt

### Output

make a .pdf file 

### Options

| Argument | Description |
| - | - |
| `-h, --help`    | show this help message and exit                            |
| `-f DATAFILE`   | datafile for plotting, default = morris_method_output.txt  |
| `-o OUTPUTFILE` | filename of outputed pdf file, default = morris_output.pdf |

## Sobol Method

> `./utilities/sobol_method.py`

### Usage

```bash
usage: sobol_method.py [-h] [-f CONFIGFILE] [-i INPUTFILE] [-o OUTPUTVARNAME]
                       [-s SOLLIST] [-e ERRORLIST] [-c CONVLIST]
                       [-m OUTPUTMEAN] [-t ITER]
```

Program to evaluate model sensistivity by Sobols method at a given PROCESS
design point.

### Configuration File

### Output

### Options

| Argument | Description |
| - | - |
| `-h, --help`       | show this help message and exit                                          |
| `-f CONFIGFILE`    | configuration file, default = run_process.conf                           |
| `-i INPUTFILE`     | input parameters file, default = sobol_method_conf.json                  |
| `-o OUTPUTVARNAME` | PROCESS output analysed, default = capcost                               |
| `-s SOLLIST`       | filename of PROCESS outputs, default = output_solutions.txt              |
| `-e ERRORLIST`     | filename of failed PROCESS output, default = output_failed_solutions.txt |
| `-c CONVLIST`      | filename of converged PROCESS output, default = output_conv_solution.txt |
| `-m OUTPUTMEAN`    | PROCESS mean model output value, default = 8056.98 (DEMO capcost)        |
| `-t ITER`          | number of model iteration sampled, default = 100                         |

## Sobol Plotting

> `./utilities/sobol_plotting.py`

### Usage

```bash
usage: sobol_plotting.py [-h] [-f DATAFILE] [-o OUTPUTFILE]
```

Program to plot the output of the the Sobols sensistivity analysis at a given
PROCESS design point.

### Configuration File

### Output

### Options

| Argument | Description |
| - | - |
| `-h, --help`    | show this help message and exit                           |
| `-f DATAFILE`   | datafile for plotting, default = sobol.txt                |
| `-o OUTPUTFILE` | filename of outputed pdf file, default = sobol_output.pdf |