
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

Program to evaluate model sensistivity by elementary effects method at a given PROCESS design point. The method of Morris is a technique for screening a large number of model parameters to identify the dominatant parameter for a global sensitivity analysis[1], and a guide to more details can be found, for example, in the textbook[2]. Note that this utility has a significanity longer run time that a typical evalution of PROCESS design points. This utilities requires the use of the Python library [SALib](https://salib.readthedocs.io/en/latest/index.html).

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
The file specifies a dictionary that gives all the information for running the Morris method tool. The number of variables considered in the Morris method with `num_vars`, the name of the variable as it appears the PROCESS MFILE is listed under `names` and the upper and lower bounds of the flat distribution is given in bounds. In addition the utility also uses `run_process.py` and therefore can optionally use the configuation file `run_process.conf`. Additionally, an `IN.DAT` file describing the relevant design point needs to be present.

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

Program to plot the output of the the sensistivity analysis by elementary effects method at a given PROCESS design point. It creates a scatter plot showing the mean agaisnt the variance of the elementary effects.

### Usage

```bash
usage: morris_plotting.py [-h] [-f DATAFILE] [-o OUTPUTFILE]
```

### Configuration File

The tool reads the data contained `morris_method_output.txt` produced from the program `morris_method.py`.

### Output

A .pdf file is created called `morris_output.pdf`. The name of the produced pdf file can be specified using te option OUTPUTFILE.

### Options

| Argument | Description |
| - | - |
| `-h, --help`    | show this help message and exit                            |
| `-f DATAFILE`   | datafile for plotting, default = morris_method_output.txt  |
| `-o OUTPUTFILE` | filename of outputed pdf file, default = morris_output.pdf |

## Sobol Method

> `./utilities/sobol_method.py`

Program to evaluate model sensistivity by Sobol's method at a given PROCESS design point. It uses the variance based global sensistivity analaysis to calculate the first order and total Sobol indices. More information on Sobol's method can be found, for example, in the testbook[1]. Note that this utility has a significanity longer run time that a typical evalution of PROCESS design points. This utilities requires the use of the Python library [SALib](https://salib.readthedocs.io/en/latest/index.html).

[1] A. Saltelli, S. Tarantola, F. Campolongo, M. Ratto, T. Andres, J. Cariboni, D. Gatelli and M. Saisana, (2008) "Global Sensitivity Analysis: The Primer" (New York: Wiley)

### Usage

```bash
usage: sobol_method.py [-h] [-f CONFIGFILE] [-i INPUTFILE] [-o OUTPUTVARNAME]
                       [-s SOLLIST] [-e ERRORLIST] [-c CONVLIST]
                       [-m OUTPUTMEAN] [-t ITER]
```

### Configuration File

The configuration file `sobol_method_conf.json` used the JSON format and has the following style
```
{
    "bounds": [
        [
            1.1,
            1.3
        ],
        [
            3.4,
            3.6
        ],
        [
            520000000.0,
            640000000.0
        ],
        [
            0.475,
            0.525
        ]
    ],
    "names": [
        "hfact",
        "boundl(18)",
        "alstrtf",
        "triang"
    ],
    "num_vars": 4
}
```
The file specifies a dictionary that gives all the information for running the Morris method tool. The number of variables considered in the Morris method with `num_vars`, the name of the variable as it appears the PROCESS MFILE is listed under `names` and the upper and lower bounds of the flat distribution is given in bounds. In addition the utility also uses `run_process.py` and therefore can optionally use the configuation file `run_process.conf`. Additionally, an `IN.DAT` file describing the relevant design point needs to be present.

### Output

This utility uses the `run_process.py` tool and therefore produces the same output files and in addition the tool creates several file in a .txt format. The parameter sampling points generated in the Sobol method sampling are saved the same folder as the program is run from as `param_values.txt`. The output of Sobol's method is shpwn over four files created in the same folder as the `run_process.py` utility working directory. Firstly, `output_solutions.txt` which contains a list of the final value of the figure of merit of every PROCESS run done over the calucation of the Sobol indices. Then two files `output_failed_solutions.txt` and `output_conv_solutions.txt` which list all PROCESS run solutions that failed and succeeded to converge respectively. Finally, the file `sobol.txt` which gives all the first order and total Sobol indices and their 95% confidence intervals.

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

Program to plot the output of the the Sobols sensistivity analysis at a given PROCESS design point. It creates a bar chart showing both the first order and total Sobol indices for each variable and give the 95% confidence intervals.

### Usage

```bash
usage: sobol_plotting.py [-h] [-f DATAFILE] [-o OUTPUTFILE]
```

### Configuration File

The tool reads the data contained `sobol.txt` produced from the program `sobol_method.py`. The name of the data file can be modified using the option DATAFILE.

### Output

A .pdf file is created called `sobol_output.pdf`. The name of the produced pdf file can be specified using the option OUTPUTFILE.

### Options

| Argument | Description |
| - | - |
| `-h, --help`    | show this help message and exit                           |
| `-f DATAFILE`   | datafile for plotting, default = sobol.txt                |
| `-o OUTPUTFILE` | filename of outputed pdf file, default = sobol_output.pdf |



## TF Stress distribution plots

> `./utilities/plot_stress_tf.py`

Program to plot stress and displacement radial distributions at the inboard mid-plane section of the TF coil.
This program uses the `SIG_TF.DAT` file, that store stress distributions of the VMCON point and stores the outputs
plots in the `SIG_TF_plots/` folder, created if not existing.

### Discussion of the stress modelling assumptions

In case of a resisitive coil, the stress is calculated from a generalized plane strain model, hence provinding vertical
stress radial distribution, alongside the radial and the toroidal ones. This is not the case for superconducting magnets
as a plane stress modelling is used for now. The reason is that a transverse orthotropic formulation of the generalized 
plane strain, is needed to correctly take the difference of the casing in the vertical direction properly. This will be
done in the near future. 

### Usage

```bash
usage: plot_stress_tf.py [-h] [-p [PLOT_SELEC]] [-sf [SAVE_FORMAT]] [-as [AXIS_FONT_SIZE]]
```

### Option

| Argument | Description |
| - | - |
| `-h, --help`    | show this help message and exit                           |
| `-p, --plot_selec [PLOT_SELEC]`   | Plot selection string :                 |
| - |   - if the string contains `sig`, plot the stress distributions |
| - |   - if the string contains `disp`, plot the radial displacement distribution |
| - |   - if the string contains `all`, plot stress and displecement distributions |
| `-sf, --save_format [SAVE_FORMAT]` | output format (default='pdf')  |
| `-as, --axis_font_size [AXIS_FONT_SIZE]` | Axis label font size selection (default=18) |