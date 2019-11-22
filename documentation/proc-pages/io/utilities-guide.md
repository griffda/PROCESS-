
# Introduction

The PROCESS Python utilities are located in the repository folder

```
/utilities/
```

A number of utilities for `PRoCESS` are available, for instance to modify the input file `IN.DAT`, run `PROCESS` until a feasible solution is found, or to extract and plot data from the `PROCESS` output.

All executables use Python library functions either from the publicly available `numpy`, `scipy` and `matplotlib` libraries of the `PROCESS` Python libraries. To used the `PROCESS` Python libraries made their directory is in your Python path.

All Python code has been written for Python 3.

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

### Parameters Displayed

`runtitle` - Variable describing the purpose of the run.

`PROCESS version` - Tagged version of the `PROCESS` used for the run.

`Date` - Date of the `PROCESS` run.

`Time` - Time of the `PROCESS` run.

`User` - Name of the user who ran `PROCESS`.

`Optimisation` - Figure of merit (`minmax`) for constrained optimisation.

`Plasma Composition` - Number densities of several ion species relative to the electron density.

`Coil Currents etc` - Peak coil currents of the PF coils in $MA$, flux swing of the central solenoid used for startup and total available in $Wb$. Total burn time `tburn` in hrs.

`Cost of electricity` - This is the cost of electricity in $/MWh$. Check the respective cost model for the reference year of the inflation used.

`Geometry` - This is the major radius $R_0$, the minor radius $a$, the aspect ratio $A$, the plasma elongation at the 95% flux surface $\kappa_{95}$, the plasma triangularity at the 95% flux surface $\delta_{95}$, the surface area of the plasma, the plasma volume, the number of TF coils, the inboard and outboard thicknesses of the blanket and shield as well as the total fusion power.

`Power flows` - Average neutron wall load $W_{all}=\frac{P_{neutrons}}{S_{plasma,surface}f_{user}}$[^2], the normalised radius of the 'core' region $\rho_{core}$ used in the radiation correction of the confinement scaling[^3] [^4], the electron density at the pedestal top $n_{e,ped}[m^{-3}]$, the normalised radius $\rho=r/a$ at the pedestal top, the helium fraction relative to the electron density, the core radiation $P_{rad} (\rho<\rho_{core})$ subtracted from $P_{heat}$ in confinement scaling and $W_{th}$, the total radiation inside the separatrix, the nuclear heating power to blanket $P_{nuc,blkt}= P_{neutr} (1-e^{-\frac{\Delta x_{blkt}}{\lambda_{decay}}})$, the nuclear heating power to the shield $P_{nuc,shld}=P_{neutr}-P_{nuc,blkt}$, the power crossing the separatrix into the SoL/Divertor $P_{sep}$, the L-H threshold power $P_{LH}$, the divertor lifetime in years, the high grade heat for electricity production $P_{therm}$, gross cycle efficiency $P_{e,gross}/P_{therm}$, Net cycle efficiency $\frac{P_{e,gross}-P_{heat,pump}}{P_{therm}-P_{heat,pump}}$, Net electric power $P_{e,net}=P_{e,gross}-P_{recirc}$, Plant efficiency $P_{e,net}/P_{fus}$.

**Physics** - Plasma current $I_P[MA]$, vaccuum magnetic field at in the plasma centre $B_T(R_0)$, the safety factor at the 95\% flux surface $q_{95}$, definitions of $\beta$ as given in \cite{kovari_physics}, the volume averaged electron temperature $\langle T_e\rangle$ and density $\langle n_e\rangle$, the fraction of the line averaged electron density over the Greenwald density $\langle n_{e,line}\rangle / n_{GW}$, the peaking of the electron temperature $T_{e,0}/\langle T_e\rangle$ and density  $n_{e,0}/\langle n_{e,vol}\rangle$, core and SoL effective charge $Z_{eff}=\sum_i f_iZ_i^2$, impurity fraction $f_Z=n_Z/\langle n_e\rangle$, H-factor and confinement time are calculated using a radiation corrected confinement scaling[^3] [^4].

**Neutral Beam Current Drive** - The steady state auxiliary power used for heating and current drive during the flat top phase (NOT to be confused with the start up or ramp down power requirements), part of the auxiliary power that is used for heating only, but not current drive, current drive fractions for the inductive, auxiliary and bootstrap current, the neutral beam current drive efficiency $\gamma_{NB}$, the neutral beam energy, the plasma heating used in the calculation of the confinement scaling/H-factor $P_{aux} + P_\alpha - P_{rad,core}$, the divertor figure of merit $P_{sep}/R$, $P_{sep}/(\langle n_e\rangle R)$, the fraction of the power crossing the separatrix with respect to the LH-threshold power $P_{sep}/P_{LH}$, the non-radiation corrected H-factor (calculated for info only).

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

## List PROCESS runs with comments

`./utilities/build_index.py`

Creates an index of all PROCESS run comments, after they have been created by `run_process.py` in a series of subfolders.

**Input**: `README.txt` files in subfolders

**Output**: `Index.txt`

**Configuration Options**: Optional arguments are:

```
# change the name of the file containing the folder description
build_index.py -r README.txt
# append the results to Index.txt instead of creating a new file
build_index.py -m a
# change the name of the subfolder Base - default=Run
build_index.py -b Base
# give a list of subfolder suffixes - default=all
build_index.py -b 1-4,6,8,9-12
# increase verbosity
buld_index.py -v
```

An example `Index.txt` file might look like this

```
Run1:
    Original run

Run2:
    Changed the no. TF coils
...
```

## Solver accuracy and globality

`test_process.py`

This program has a similar structure to the 'run_process.py' program, but varies the iteration variable start parameters for as many iterations as requested by the configuration file. It outputs a summary of the final iteration parameter values and figure of merit values for testing the accuracy of the optimisation solver in PROCESS. This also allows the user to verify the final solution id not only a local solution. For this type of study a large value of the factor within which the iteration variables are changed should be chosen¬

**Input**: `test_process.conf`, an `IN.DAT` file as specified in the config file

**Output**: All the standard PROCESS output, `process.log`, `README.txt`, `time.info` (stores the run time of actual PROCESS iterations), `SolverTest.out`

The `SolverTest.out` file contains a table of the `ifail` values, the figure of merit, the square root of the sum of the squares of the individual constraints as well as the initial and final values for the iteration variables and the individual constraint residuals. The value of Q, i.e., the ratio of fusion power to injected power, is also included in the `SolverTest.out` file. All values can be used to diagnose the performance of the optimisation solver.

**Configuration Options**: The configuration file `test_process.conf` has the following style:

```
* no iterations
NITER = 1000

* Working directory: Subdirectory in which to copy IN.DAT and test_process.conf
WDIR=Run1/

* original IN.DAT name (should not be called IN.DAT!)
ORIGINAL_IN_DAT = minimal_demo2_nosweep.IN.DAT

* sets flag of same name in IN.DAT:
*   None - keeps original value
*   -1   - Non-optimisation only
*    0   - one non-optimisation step followed by optimised iteration
*    1   - optimisation only
IOPTMIZ = 1

* sets the error tolerance for VMCON
*   None - keeps the original value (IN.DAT or default)
EPSVMC = None

* sets the step length for the finite difference derivatives
*   None - keeps the original values (IN.DAT or default)
EPSFCN = None

* sets the figure of merit
* positive value - minimise, negative value - maximise
*   None - keeps the original value
*   1    - plasma major radius
*   6    - cost of electricity
MINMAX = None

* integer seed for random number generator; use None for random seed
SEED = 2

* factor within which the iteration variables are changed
FACTOR = 1.1

* PATH to PROCESS binary
PROCESS=process.exe
```

A configuration file with an alterative name can be specified using the optional argument

`test_process.py -f CONFIGFILE`

## Step from one model to another

`a_to_b.py`

When attempting to model a reactor very different from those in previous studies, it can be difficult to find a feasible solution. This utility takes an initial `IN.DAT` (`A.DAT`) that is known to run successfully, and a target `IN.DAT` (`B.DAT`) that is substantially different. The difference is broken into small steps, and PROCESS runs repeatedly, stepping from the initial input file to the target input file. After each step, the output values of the iteration variables are used as the input starting values for the next step/

**Input**: `a_to_b.conf`, `A.DAT` and its corresponding `MFILE.DAT`, `B.DAT`. (These names can be changed by the user.)

**Output**: When the program finishes, the `.DAT` files from the last run of PROCESS can be found in the specified working directory. If option `keep_output = True`, the `IN.DAT`, `OUT.DAT`, `MFILE.DAT` and logged PROCESS output of each step are stored. Files are prefaced with their step number, e.g. `003.IN.DAT` can copied to the specified output directory.

**Troubleshooting**: Ensure that each relevant variable is only listed once in the vardes.html file.

**Configuration Options**: The configuration file `a_to_b.conf` has the following style:

```
*Comment line

*Working directory to store temporary files, default = wdir
wdir = wdir

*Switch to keep .DAT files for every step, default = True
keep_output = True

*Directory for output if keep_output = True, default = steps
outdir = steps

*IN.DAT file for A, default = A.DAT
a_filename = A.DAT

*IN.DAT file for B, default = B.DAT
b_filename = B.DAT

*Path to process binary
path_to_process = /home/PROCESS/master/process

*Number of iterations of vary_iteration_variables to run, default = 20
vary_niter = 20

*Number of steps to go from A to B, default = 10
nsteps = 10

*Factor to vary iteration variables within, default = 1.2
factor = 1.2

*Gap between upper and lower bounds to narrow to, default = 1.001
bound_gap = 1.001
```

## N-Dimensional Scanner Utility

This suite of Python utilities allows the user to conduct systematic, multi-dimensional parameter studies with PROCESS. It systematically varies a set of N user defined parameters within predefined bounds. This final results can be evaluated using the corresponding visualisation tool and/or saved in standard NetCDF format for further analysis.

The suite contains the following executables:

* `ndscan.py` - executes the Nd-scan as specified in the configuration file.
* `ndscan_package_only.py` - creates a NetCDF output file from a previous Nd-scan run.
* `ndscan_and_package.py` - both executes the Nd-scan and creates the NetCDF output file.
* `ndscan_visualisation.py` - visualises the NetCDF output.

**Input** `ndscan.json`, `IN.DAT`

**Output**: All MFILES in subdirectory `MFILES`, packaged NetCDF output file as named in configuration file (default `NdscanOutput.nc`).

When running any of the ndscan tools, optional arguments are:

```
# to specify another location/name for the configfile
ndscan.py -f CONFIGFILE

Use -h or --help for help
```

For the visualisation tool the corresponding NetCDF input file can also be specified with `-f`. Per default `NdscanOutput.nc` is used.

**Configuration Options**: The configuration file `ndscan.json` uses the JSON format (www.json.org) and has the following style

```
{
    "axes": [
	{
            "lowerbound": 7.5,
            "steps": 16,
            "upperbound": 9.0,
            "varname": "rmajor"
        },
        {
            "lowerbound": 5.5,
            "steps": 16,
            "upperbound": 7.0,
            "varname": "bt"
        }
    ],
    "_comment": [
        "This field helps to describe the config file for users reading",
        "Anything you write in here will be ignored by the code",
        "Each axis has these configuration parameters available:",
        "varname",
        "lowerbound",
        "upperbound",
        "'steps': number of evaluations"
    ],
    "optionals": {
        "remove_scanvars_from_ixc": true,
        "smooth_itervars": true
    },
    "description": "Description of the goals of this specific run",
    "title": "NdscanOutput",
    "author": "Me",
    "output_vars": [
        "beta",
        "pheat",
        "powfmw",
        "pradmw",
        "powerht",
        "cirpowfr",
        "te",
        "hfact",
        "dnelimt",
        "dene",
        "rmajor",
        "bt",
        "pnetelmw",
        "coe",
        "fwbllife",
        "capcost",
        "palpmw",
        "wallmw",
        "taueff"
    ]
}
```
The only required input parameters in the configuration file are the scan axes, out of which at least one has to be specified. For each axis a `varname` a `lowerbound`, an `upperbound` and the number of `steps` > ` have to be specified. All other parameters in the configuration file are optional. The parameters relevant for running the N-dimensional scan are:

`_comment` Anything in the comment section (like all other undefined sections) is for the user only and will be ignored by the program.

`optional:remove_scanvars_from_ixc` Removes all scanning variables from the iteration variables of the `IN.DAT` file (default = True).

`optionals:smooth_itervars` Ensures that each next point starts from the last successful run. This increases the run time, but improves the convergence and reduces errors.

The parameters only relevant to the creation of the summary NetCDF file are:

`author` The author will be copied into the NetCDF file.

`description` The description will be copied into the NetCDF file.

`title` Name of the output NetCDF file (default `NdscanOutput`) that is also copied into the title of the NetCDF file.

`output_vars` The variables that will be extracted from the MFILEs and stored in the NetCDF file. (Only need when creating the NetCDF output file.)

Additional parameters can be specified as in the `config` section for the `evaluate_uncertainties.py` tool.

The resulting NetCDF file can be visualised using the `ndscan_visualisation.py` tool. It has an interactive menu and is fairly self-explanatory.

## Turn output into input

`write_new_in_dat.py`

This program creates a new `IN.DAT` file with the initial values of all the iteration variables replaced by their results in `OUT.DAT`, if that output is a feasible solution.

When a scan has been run, by default this program uses the last feasible point in that scan to write the new starting values. There is also an option to select the first feasible solution from a scan.

**Input**: `IN.DAT`, `MFILE.DAT`

**Output**: `new_IN.DAT`

```
usage: write_new_in_dat.py [-h] [-f MFILE.DAT] [-i IN.DAT]

optional arguments:
  -h, --help   show this help message and exit
  -lfp         use the last feasible point from a scan (default)
  -ffp         use the first feasible point from a scan
```

## Create csv file summarising data in `plot_proc.py` output

`output_data.py`

A utility to output a set of data very similar to `plot_proc.py`, but to a comma-delimited format for inclusion in spreadsheets. This is used by `archive.sh` to import data into the PROCESS runs database. For other uses, it's best to use PLOT.DAT instead, as this is always generated by PROCESS, and can be easily loaded into a spreadsheet.

**Input**: `MFILE.DAT` (or as specified by user)

**Output**: `process_summary.txt` (or as specified by user)

**Configuration Options**: Optional arguments are:

```
usage: output_data.py [-h] [-f FILENAME] [-o OUTPUT]

Produce a single-column comma-separated (.txt) summary for a given scan. For
info contact rich.kemp@ccfe.ac.uk or james.morris2@ccfe.ac.uk

optional arguments:
  -h, --help   show this help message and exit
  -f FILENAME  specify input filename
  -o OUTPUT    specify output filename
```

## Create csv file summarising data for database

`create_csv4database.py`

This is essentially the same tool as `output_data.py`, but the format is frozen to assure consistency for the PROCESS Runs database excel spreadsheet.

## Create archivable files for PROCESS Runs database

`archive.sh`

This is a very simple shell script to create all the relevant files for the PROCESS Runs database on the EUROfusion IDM (https://idm.euro-fusion.org/?uid=2MUP64). It uses the `create\_csv4database.py` and the `plot\_proc.py` utilities.

**Input**: `IN.DAT`, `OUT.DAT`, `MFILE.DAT`

**Output**: `PREFIX.IN.DAT`, `PREFIX.OUT.DAT`, `PREFIX.pdf`, `PREFIX.csv`

The convention for the PREFIX is to use the following format e.g. DEMO1_detailed_description_Year_Month_Day, where DEMO1 is the description for the design investigated and the detailed description summarises the key changes made to the design.

## Compare a reference design with a PROCESS run

`ref_check.py`

Tool for comparing a PROCESS MFILE output file to a JSON reference file for a certain type of machine.

The JSON file format contains two dictionaries: 'PARAMS' and 'LIMITS'.

Arguments:
* `-r` - Reference JSON file
* `-f` - MFILE file to compare to JSON reference
* `-s` - Save output to file called `input_comp.txt`

Below is an example of the JSON reference file. The PARAMS dictionary takes just a variable name and the value it has for the reference. The LIMITS dictionary takes the limit value as the key. The content of the limit is then the limit value, whether it is a minimum or maximum and then lastly the parameter it is limiting. For more examples look at the `cfetr_small_ref.json` in the test suite CFETR_small case folder.

```
{
	"PARAMS":
        {
            "rmajor" : 5.7,
            "rminor": 1.6,
            "aspect": 3.563,
            "bt": 5.0,
            "kappa": 1.8,
            "triang": 0.4,
            "hfact": 1.3,
            "powfmw": 250
        },

    "LIMITS":
        {
            "alstrtf" :
                {
                    "limit": 600.0e6,
                    "type": "-",
                    "parameter": "strtf2"
                },
            "pseprmax" :
                {
                    "limit": 25.0,
                    "type": "-",
                    "parameter": "pdivt/rmajor"
                },
            "ripmax" :
               {
                   "limit": 0.5,
                    "type": "-",
                    "parameter": "ripple"
                }
        }
}
```

The code outputs the parameters which don't match, the parameters that match, the lower limits that aren't satisfied, the lower limits that are satisfied, the upper limits that aren't satisfied and the upper limits that are satisfied.

## Compare two MFILEs

`mfile_comparison.py`

Tool for comparing two MFILEs and outputting significant differences in numerical values.

Arguments:
* `-f` - Files to compare
* `-s` - Save output to file called comp.txt
* `--acc` - Percentage difference threshold for reporting
* `--verbose` - Additional output

## Comparing two input files

`in_dat_comparison.py`

Tool for comparing two IN.DATs and outputting inputs in one file and not the other, inputs in both with different values and inputs in both with the same value.

Arguments:
* `-f` - Files to compare
* `-s` - Save output to file called `input_comp.txt`

## Convert PROCESS MFILE to Catia CAD readable output

`cad_output.py`

The PROCESS utility `cad_output.py` takes the `mfile.py` and produces an output file suitable for using in CAD programs (for testing *Catia* was used). The output file is named `PROCESS.CAD` by default. The output file provides a list of named parameters for input into *Catia*. Modification for other CAD programs may be required. The options for the script are:

```
cad_output.py [-h] [-f FILENAME] [-o OUTPUT] [-s]

Produce a CAD output file of the PROCESS MFILE file for a given scan. For info
contact james.morris2@ccfe.ac.uk

optional arguments:
-h, --help   show this help message and exit
-f FILENAME  specify input filename
-o OUTPUT    specify output filename
-s, --show   show plot as well as saving figure
```

## Convert `IN.DAT` to new format

`convert_in_dat.py`

The utility `convert_in_dat.py` takes an old format `IN.DAT` (~pre-2014) ad converts it into the newer format. As fewer old `IN.DAT` files exist this utility will eventually be removed. The options for the script are:

```
convert_in_dat.py [-h] [-f f] [-o o]

PROCESS IN.DAT converter. Convert IN.DAT into new format. For info contact
james.morris2@ccfe.ac.uk

optional arguments:
-h, --help  show this help message and exit
-f f        File to read as IN.DAT (default="IN.DAT")
-o o        File to read as IN.DAT (default="new_IN.DAT")
```

## Creat MCNP input file from `MFILE.DAT`

`mcnp_output.py`

The utility `mcnp_output.py` makes a `MFILE.DAT` and converts it to a suitable format for MCNP runs. The options for the script are:

```
mcnp_output.py [-h] [-f f] [-o o] [--ctf]

Process MFILE.DAT into PROCESS.MCNP file.

optional arguments:
-h, --help  show this help message and exit
-f f        File to read as MFILE.DAT
-o o        File to write as PROCESS.MCNP
--ctf       True/False flag for CTF
```

## Cretae a html output summary from MFILE.DAT

`output_summary.py`

This utility has two versions in the repository:
* `output_summary.py` - general summary generate automatically. Not customisable.
* `output_detailed.py` - takes a configuration file called `output_detailed.json` to configure the output html.

The output HTML arranges the variables into sections and allows the user to add comments and other informations to the summary by adding comments to the PROCESS IN.DAT. The comments in the IN.DAT take the following form:

```
#header-physics : Comment under header called physics

#constraint-1 : Comment for constraint number 1
icc = 1

#iteration-variable-10 : Comment for iteration variable 10
ixc        = 10 * hfact

#in-neped : Comment for input neped
neped = 0.678e20 * Electron density of pedestal (/m3) (ipedestal=1)
```

The `IN.DAT` appendeed to the end of the `MFILE.DAT` and the Python gets this information from there. The comments have to follow the format above. It is not ideally on every PROCESS run, but is useful for summarising important PROCESS results. The options for the `output_summary.py` are:

```
output_summary.py [-h] [-f MFILENAME] [-o OUTFILENAME]

Create PROCESS output document.For info contact james.morris2@ukaea.uk

optional arguments:
-h, --help      show this help message and exit
-f MFILENAME    specify PROCESS MFILE
-o OUTFILENAME  specify output file
```

The options for the output `output_detailed.py` are:

```
output_detailed.py [-h] [-f MFILENAME] [-j JSONFILE] [-o OUTFILENAME]

Create PROCESS output document.For info contact james.morris2@ukaea.uk

optional arguments:
-h, --help      show this help message and exit
-f MFILENAME    specify PROCESS MFILE
-j JSONFILE     specify JSON file location and name
-o OUTFILENAME  specify output file
```

`Output_detailed.py` also requires a configuration file named `output_detailed.json`. This JSON file contains the grouping of the output and allows the user to adjust what is output and where.

## Output plotting: create data file

`make_plot_dat.py`

Creates a `PLOT.DAT`-type file from `MFILE.DAT`. This is required by `plot_sweep.py`.

**Input**: `make_plot_dat.conf`, `MFILE.DAT`

**Output**: `make_plot_dat.out`

**Configuration Options**: Optional arguments are:

```
# new variables for output
make_plot_dat.py -p rmajor
# writes make_plot_dat.out in columns
make_plot_dat.py --columns
# resets make_plot_dat.conf to PLOT.DAT layout
make_plot_dat.py --reset-config
# file to read as input
make_plot_dat.py -f MFILE.DAT
# run with default parameters
make_plot_dat.py --defaults
```

An example version of `make_plo_dat.conf` might look like this:

```
# make_plot_dat.out config file.
rmajor
aspect
rminor
bt
powfmw
pnetelmw
te
pdivt
strtf1
strtf2
```

## Plot scan results

`plot_mfile_sweep.py`

This utility plots normalised values of the iteration variables output by a parameter scan. Zero indicates an iteration variable at its lower bound and 1 an iteration variable at its upper bound.

**Input**: `MFILE.DAT`

**Output** `sweep_fig.pdf` (default of as specified by the user)

Optional arguments are:

```
# creates sweep_fig.pdf with R0, te, aspect (same variable names as in MFILE.DAT)
python plot_mfile_sweep.py -p rmajor te aspect
# creates demo1.png with Te, n
python plot_mfile_sweep.py -o demo1.png -p te dene
# creates a sweep_fig.pdf with R0, aspect with a different MFILE.DAT
python plot_mfile_sweep.py -f diff_mfile.dat -p rmajor aspect
# Show plot to screen instead of saving with R0 and aspect
python plot_mfile_sweep.py -p rmajor aspect --show

Use -h or --help for help
```

## Plot iteration variables and constraint residuals

`diagnose_process.py`

This utility aids the user to interpret PROCESS runs that do not find a feasible solution (unless PROCESS has terminated prematurely). It reads the `MFILE.DAT` and plots the normalised iteration variables, i.e. the iteration variable values normalised to their bounds such that 0 indicates an iteration vraible at its lower bound and ` an iteration variable at its upper bound. Furthermore, it shows the normalised constraint residuals.

**Input**: `MFILE.DAT`

**Output**: Displays plots on screen, still need to be saved by the user! (Remember to use `-Y` or `-X`, if `ssh`ing into a remote machine!)

Optional arguments are:

```
# allows to specify another location/name for the MFILE
python diagnose_process.py -f MFILE.DAT

Use -h or --help for help
```

## Plot two parameters from many MFILES

`plot_comparison.py`

```
usage: plot_comparison.py [-h] [-x XAXIS] [-y YAXIS] [-e END] [f [f ...]]

Program to display the evolution of two variables in a selection of MFILEs.

positional arguments:
  f                     list of MFiles to be plotted; default = MFILE.DAT

optional arguments:
  -h, --help            show this help message and exit
  -x XAXIS, --xaxis XAXIS
                        x-axis, default=rmajor
  -y YAXIS, --yaxis YAXIS
                        y-axis, default=powfmw
  -e END, --end END     file format default = pdf
```

## Plot a pie chart of the cost breakdown

`cost_pie.py`

This utility plots the cost breakdown as a pie chart giving each component as a percentage. This allows for the most expensive areas to be easily identified. For the 1990 cost mdoel, an additional plot showing how direct , indirect and contingency costs contribute to the overall budget is shown.

**Input**: `MFILE.DAT`

**Output**: Displays plot of the cost breakdown to screen. For the 1990 cost model, the breakdown for direct, indirect and contingency are also shown. These can be saved with `-s` argument (`cost_pie.pdf` and `direct_cost_pie.pdf`).

Help information:

```
usage: cost_pie.py [-h] [-f MFILE] [-s]

Displays the cost breakdown as a pie chart. For more information contact
Stuart.Muldrew@ukaea.uk

optional arguments:
-h, --help  show this help message and exit
-f MFILE    specify the MFILE (default=MFILE.DAT)
-s, --save  save as well as displaying figure
```

## Plot a bar chart of the cost breakdown

`cost_bar.py`

This utility plots the cost breakdown as a bar chart giving the cost of each component. This allows for the most expensive areas to be easily identified. For the 1900 cost model, an additional plt showing how the direct, indirect and contingency costs contribute to the overall bidget is shown. Multiple MFILEs can be specified allowing for different PROCESS runs to be compared on the same plot. An inflation factor can be specified using the `-inf` argument, which multipled all the costs by that value.

**Input**: `MFILE.DAT`

**Output**: Displays plot of the cost breakdown to screen. For the 1990 cost model, the breakdown for direct, indirect and contingency is also shown. These can be saved with `-s` argument (`cost_bar.pdf` and `direct_cost_bar.pdf`).

Help information:

```
usage: cost_bar.py [-h] [-f f [f ...]] [-s] [-inf INF]

Displays the cost breakdown as a bar chart. Multiple MFILEs can be given and
will be plotted on the same chart. For more information contact
Stuart.Muldrew@ukaea.uk

optional arguments:
-h, --help    show this help message and exit
-f f [f ...]  specify the MFILE(s) to plot
-s, --save    save as well as displaying figure
-inf INF      Inflation Factor (multiplies costs)
```

## POPCON plot

`popcon.py`

This utility generates a POPCON plot from an MFILE th can be saved with the `-s` argument. `-x`, `-y` and `-z` aloow the user to set the definition of temperature, density and power respectively that is plotted. Currently the impurity is set with an effective charge, and this can be parsed using `-zimp` (default is Argon). The routine also features a test case that can be run by supplying the `-t` argument only.

**Input**: `MFILE.DAT`

**Output**: Displays POPCON plot to screen. This can be saved with `-s` argument (`popcon.pdf`).

Help information:

```
usage: popcon.py [-h] [-f MFILE] [-s] [-t] [-x X] [-y Y] [-z Z] [-zimp ZIMP]

Displays a POPCON plot for input MFILE. For more information contact
Stuart.Muldrew@ukaea.uk or Peter.Knight@ukaea.uk

optional arguments:
-h, --help  show this help message and exit
-f MFILE    specify the MFILE (default=MFILE.DAT)
-s, --save  save as well as displaying figure
-t, --test  Test mode: ignores MFILE and runs with R Kembleton's test values
-x X        Temperature (x-axis): (0) Central (1) Volume (default=1)
-y Y        Density (y-axis): (0) Central (1) Volume (2) Line (default=1)
-z Z        Power (z-axis): (0) Aux for balance (1) Net (2) Fusion
(default=0)
-zimp ZIMP  Impurity charge (default=18 Ar)
```

## Profile plots

`plot_profiles.py`

This utility allows for plotting of the temperature and density profiles of a number of MFILEs. The options are described below:

```
    arguments:
    -h, --help              show this help message and exit
    -f MFILE [MFILE ...]    specify the llist of MFILEs to use
    -s, --save              save as well as displaying the figure
    -o,                     name of the output pdf file
    -n N,                   scan number in MFILE to use
```

## VMCON optimisation plots

`plot_opti_process.py`

Macro plotting a set of information about the `VMCON` optimisation from an output file called `OPT.DAT`. The file contains:
* The PROCESS indexes of the constraints and the variables used for the considered run.
* The variables described in [table 1](#table-1) stored for each `VMCON` iteration. Please notte that only one set of number is associated in per `VMCON` iteration.

<a name="table-1"></a>

|  Variable description  |  PROCESS code name  |  `VMCON` doc def  |
| ------------- | ------------- | ------------- |
| Normalized figure of merit | `abs(obj)` | $f(x)$ |			
| VMCON convergence criteria | `sum` | $\left| \nabla_x f(\vec{x}^{j-1})^T \cdot \vec{\delta}^{j} \right| + \sum^m_{i=1}\left| \lambda^j_i c_i(\vec{x}^{j-1}) \right|$ |
| Constraints residual quadratic sums | `sqsumsq` | $\sqrt{\sum^{m}_{i=1} c^{2}_i(\vec{x}^{j-1})}$ |
| Individual residual values | `conf(i)` | $c_i(\vec{x}^{j-1})$ |
| Normalized optimization variables values | `x(i)` | $\vec{x}^{j-1}$ |

Table 1: *Variables stored in `OPT.DAT`*

The python plot routines proposes the following plots:

1. Figure of merit plot<br>Evolution of the figure of merit with the `VMCON` index
2. Convergence plot<br>`VMCON` index evolution of:
    * The `VMCON` convergence parameter
    * The quadratic sum of the constraints residuals
    * The maximum between the `VMCON` convergence parameter and the constraints residual quadratic sum, actually used to test the PROCESS convergence.
3. Dominant constraints plots<br>The last `VMCON` iteration is used order to rank the constrints with their residual values. This allows to plot the `VMCON` index evolution of:
    * The $N_{const}^{dom}$ dominant constraints values ($N_{const}^{dom}$ can be used defined)
    * The quadratic sum of the dominant constraints
    * The total quadratic sum of the constraints<br>
The difference of the two quadratic sums allow to check of any other variables contribute to the constraints for any step of the optimisation
4. Selected constraint plot<br>Any constrints residual evolution can be plotted given its PROCESS ID number. The associated plot will contain:
    * The selected constraint evolution
    * The total quadratic sum of the constraints<br>
This allows a clearer visualisation of a given constraint evolution.
5. Major variable evolution<br>The variation amplitude of the optimisation variables $\max(\vec{x}^{j-1}) - \min(\vec{x}^{j-1})$ is used to rank the variables. This allows to plot the `VMCON` index evolution of the $N_{const}^{var}$ dominant variables ($N_{const}^{dom}$ can be used defined).
6. Selected variable pair trajectory plot<br>Any pair of variables can be selected using their PROCESS ID defined in vardes, to plot the evolution of their trajectory. The color of each points corresponds to the value of the PROCESS convergence criteris (on a base 10 lograithmic scale).

The use of the macro is described on the help option of the macro , shown for indicative purpose

```
usage: plot_opti_process.py [-h] [-p [PLOT_SELEC]] [-ndc [N_DOM_CONST]]
[-ndv [N_DOM_VAR]] [-ic [I_CONST]]
[-ixv [I_X_VAR]] [-iyv [I_Y_VAR]]
[-sf [SAVE_FORMAT]] [-as [AXIS_FONT_SIZE]]

Plot optimization information

optional arguments:
-h, --help            show this help message and exit
-p [PLOT_SELEC], --plot_selec [PLOT_SELEC]
Plot selection string :
    * If it containts 'FoM'      -> Figure of Merit plot 
    * If it containts 'conv'     -> convergence criteria plot 
    * If it containts 'domconst' -> dominant constraint plot
    * If it containts 'allconst' -> a plot for each constraint stored in All_Const/
    * If it containts 'domvar'   -> dominant variables plot 
    * If it containts 'allvar'   -> a plot for each variable in All_Var/
    * If it containts 'all'      -> all the mentionned plots (default value)
-ndc [N_DOM_CONST], --n_dom_const [N_DOM_CONST]
number of plotted dominant constaints (default=3)
-ndv [N_DOM_VAR], --n_dom_var [N_DOM_VAR]
number of plotted dominant variables  (default=4)
-ic [I_CONST], --i_const [I_CONST]
Selection of the constraint to be plotted (PROCESS number defined in vardes, default=-1)
-ixv [I_X_VAR], --i_X_var [I_X_VAR]
X variable on pair plot selection (PROCESS number defined in vardes, default=-1)
-iyv [I_Y_VAR], --i_Y_var [I_Y_VAR]
Y variable on pair plot selection (PROCESS number defined in vardes, default=-1)
-sf [SAVE_FORMAT], --save_format [SAVE_FORMAT]
output format (default='eps') 
-as [AXIS_FONT_SIZE], --axis_font_size [AXIS_FONT_SIZE]
Axis label font size selection (default=14)
```

The output can be defined in any visual data format supported by pyplot, the label font size can set. Please not that to select individual constraints or variable pair plots, it is enough to precisse the `-ic` and the `-ixv/-iyv` pair, respectively.

# Uncertainty Tools

In this section, we explain the usage of the PROCESS tools to both evaluate the uncertainties of a design point as well as display tham using a simple plotting facility.

Note that the uncertainty evaluation tool has a significantly longer run time than typical evaluations of PROCESS design points and therefore should only be used once a suitable design point has been found. As only user selected output data is kept, the user is recommended to put careful thought into the list of needed output variables.

## `evaluate_uncertainties.py`

This program evaluates the uncertainties of a single PROCESS design point by use of Monte Carlo method as described in[^5]. It takes a set of uncertain parameters as input, each of which can have either Guassian or a flat ('top-hat') probability distribution. These are specified together with optional details about the PRoCESS run in a configuration file. Additionally, and `IN.DAT` file desribing the relevant design point needs to be present. (If necessary this can be created from an `MFILE.DAT` by using the `write_new_indat.py` tool.)

When running the `evaluate_uncetainties.py` tool, optional arguments are:

```
#to specify another location/name for the configfile
evaluate_uncertainties.py -f CONFIGFILE

Use -h or --help for help
```

**Input**: `evaluate_uncertainties.json`, `IN.DAT` (or an alterantive input file as specified in the config file)

**Output**: `uncertainties.nc` (This file (NetCDF format) can be visualised using the `display_uncertainties.py` tool), `Readme.txt`, `process.log`, `UQ_error_summary.txt`, PROCESS otput from th elast run.

The configuration file `evaluate_uncertainties.json` uses the JSON format (www.json.org), and has the following style

```
{
	"_description": "Config file for uncertainties evaluation",
	"_author": "Hanni Lux",

	"config": {
		  "runtitle": "testrun for uncertainty tool on DEMO2",
		  "IN.DAT_path": "IN.DAT_demo2",
		  "process_bin": "~PROCESS/master/process.exe",
		  "working_directory": "Run1",
		  "pseudorandom_seed": 2
		  },
	"uncertainties": [
          	     {
               	     "Varname":"flhthresh",
               	     "Errortype":"Gaussian",
               	     "Mean":1.0,
               	     "Std":0.05
          	     },
          	     {
               	     "Varname":"coreradius",
               	     "Errortype":"Uniform",
               	     "Lowerbound":0.6,
               	     "Upperbound":0.9
          	     },
          	     {
               	     "Varname":"etanbi",
               	     "Errortype":"Relative",
               	     "Mean":0.3,
               	     "Percentage":10.0
          	     },
          	     {
               	     "Varname":"boundu(9)",
               	     "Errortype":"LowerHalfGaussian",
               	     "Mean":1.2,
               	     "Std":0.1
          	     },
          	     {
               	     "Varname":"boundl(103)",
               	     "Errortype":"UpperHalfGaussian",
               	     "Mean":1.0,
               	     "Std":0.25
          	     }
	     	],
     "output_vars": [ "rmajor", "dene", "te", "bt"],
     "no_samples": 1000
}
```

By convention, we have designated metadata about the PROCESS runs as having a preceding underscore to distinguish these values from the other configuration data used directly by the tools or PROCESS itself. Furthermore, all the optional attributes that can be changed when running PROCESS from most Python utilities, like e.g. `run_process.py`, can be specified in the "config" section. All these values have default values and do not need to be set.

`runtitle` is a one line description of the purpose of the run to be saved in `Readme.txt` in the working directory as well as the `runtitle` parameter in the `OUT.DAT` and `MFILE.DAT` files. Per default it is empty.

`IN.DAT_path` is the name/path of the `IN.DAT` file describing the design point. If not specified it is assumed to be `IN.DAT`.

`process_bin` is the process binary that should be used. The default assummes that the user works on the CCFE Fusion Linux machines and has executed the module commends for PROCESS as described in the Userguide. Then either the master or development branch of process is being used depending on which module has been loaded.

`working_directory` represents the working directory, in which PROCESS will be executed, in case this is supposed to be different to the current directory which can be helpfule when executing several runs with slightly different setups.

`pseudorandom_seed` is the value of the seed for the random number generator. It can be any integer value. If it is not specified, its default value is taken from the system clock.

Other parameters that can be specified in the config section are `Niter` and `factor`. Both do not typically need t be changed by the user. `Niter` is the maximum number of retires that the tool will attpempt, if PROCESS fails o find a feasible solution. This means that the tool varies the start values of the iteration variables within a factor given by `factor` of the original values as this does not change the physical meaning of the input file, but can help the solver to find a better starting point for its iteration. Their default values are `Niter=10` and `factor=1.5`.

Any uncertain parameters should be specified in the "uncertainties" section. Each parameter is specified in its own sub-directory as shown in the example. For each, the `Varname` and `Errortype` need to be specified as well as the `Mean` and standard deviation `Std` for Gaussian type errors as well as `Lowerbound` and `Upperbound` for Uniform or `Mean` and a `Percentage` for Relative errors. Apart from a standard Gaussian distribution, also a lower and an upper half Gaussian distribution are available that have a sharp but off at the mean. Please note, that *all distributions are being cut off at the boundaries for the input alues for PROCESS!* At least one uncertain parameter has to be specified for the program to run and technically there is no uppoer limit as to how many uncertain parameters can be used. However, for large numbers of uncertain parameters it is recommended to increase the number of sampling points.

There are a number of other parameters in the configuration file that can be specified:

`output_vars` is a list of strings of output variable names in the `MFILE.DAT`. These are the variables saved. This list is empty by default and it is therefore crucial for the user to specify the variables of interest because otherwise the tool will not run.

`no_samples` sets the number of sample points in the Monte Carlo method. It is by default set to its recommended minimum value of 1000, but the user should contemplate higher values especially if a large number of uncertain parameters is involved.

Two parameters that ca be further set in the config file (but are not recommended to be changed) are the number of scan point `no_scans` which is by default 5 and the number of allowed unfeasible points in a scan `no_allowed_unfeasible` which is 2 as recommended in [^5].

As the distributions of the uncertainties do not have to be represented by a simple Gaussian, reducing the output of two simple numbers like a mean and a standard deviation is not typically possible. Therefore, we have decided to keep all sampled points in the output files. However, to reduce the amount of data we have decided to only store the user specified parameters in the form of a NetCDF binary file. Given that a scan is performed at each sample points, only the last of these scan points is ever kept for evaluation. (there is an option for developers to keep all the data for debugging purposes. However, this should typically not be sued in production runs.) These files can be visualised using the `display_uncertainties.py` tool.

The `UQ_error_summary.txt` file is an ascii text file summarising all values of the uncertain parameter inputs, the normalised values of iteration variables (labelled "n_"variable name) and whether their runs have found a feasible solution (`ifail=1`), have encountered any process erros (`ifail=-1`, `error_status=3`) or whether they have not found a feasible solution. This file can be used to analyse parameter spaces prone to errors.

## `display_uncertainties.py`

This is a utility to display the output file `uncertainties.nc` created by the `evaluate_uncertainties.py` tool described above.

By default, if run in the working directory of an uncertainty evaluation, it creates a scatter lot of each user defined output parameter against the next parameter in the list. It also hows the 1D histograms of each parameter distribution. If two specific variables are given as arguments, the tool plots only these two against each other.

**Input**: `uncertainties.nc`

**Output**: `Uncertainties_varname1_varname2.pdf`

Usage:

```
display_uncertainties.py [-h] [-f FILENAME] [-e END] [v [v ...]]

Program to display uncertainties of a given PROCESS design point.

positional arguments:
  v              list of variables to be plotted; default = all

optional arguments:
  -h, --help     show this help message and exit
  -f FILENAME, --filename FILENAME
                 uncertainties data file, default = uncertainties.nc
  -e END, --end END     file format default = .pdf
```

## `diagnose_uncertainties.py`

This is a python facility to display the input parameter distributions in the final runs vs. the ones specified in the input file. This can be used to determine whether the input distributions are sufficiently sampled or whether the resulting distributions are skewed due to unfeasible designs being excluded.

**Input** `uncertainties.nc`, `evaluate_uncertainties.json`

**Output** `Uncertainties_Diagnostic_varname.pdf`

Usage:

```
diagnose_uncertainties.py [-h] [-e END] [-u UNCERTAINTIES] [-f FILENAME]

Program to check the final uncertainty distributions in the input parameters.

optional arguments:
  -h, --help            show this help message and exit
  -e END, --end END     file format default =pdf
  -u UNCERTAINTIES, --uncertainties UNCERTAINTIES
                        uncertainties config file default =
                        evaluate_uncertainties.json
  -f FILENAME, --filename FILENAME
                        uncertainties data file, default =uncertainties.nc
```

## Batch Jobs

As PROCESS typically runs very fast and does not produce much data output, it is not typically necessary to submit PROCESS runs or any python executables as a batch job to the fusion linux machines. However, the `evaluate_uncertainties.py` tool is one example of a python tool for PROCESS that does run for a long time and does create a lot of output. As the CPU time limit for any non-batch jobs on the fuslw machines is 30 minutes, any decent sampled `evaluate_uncertainties.py` run will need to be submitted as a batch job. (Please note, that if you have forgotten to ubmit you job as a batch job, your job will be terminated after 30 CPU minutes, but as the output is written to file continuously, you should not loose any of the output that has been produced until then.)

To submit a batch job, first create a file called e.g. `myjob.cmd`. It should contain the following content

```
# @ executable = evaluate_uncertainties.py
# @ arguments
# @ input = /dev/null
# @ output = /ABSOLUTE_PATH_TO_WORK_DIR/ll.out
# @ error = /ABSOLUTE_PATH_TO_WORK_DIR/ll.err
# @ initialdir = /ABSOLUTE_PATH_TO_WORK_DIR/
# @ notify_user = USERNAME
# @ notification = complete
# @ queue
module use /home/PROCESS/modules
module unload python
module load python/3.3
module load process/master
```

Once you have created the file you can submit a patch job by typing

`llsubmit myjob.cmd`

While your job is running you can keep track of its progress by typing `llq` or `xloadl`. More information and help with troubleshooting can be found under http://fusweb1.fusion.ccfe.ac.uk/computing/funfaq/ll/.

As the `uncertainties.nc` output file can get quite large, it might by indicated to write the output to the `/sratch` or `/tmp` directories as they have faster I/O. Please remember to copy your results into your home directory afterwards, as these directories are not backed up and will be frequently cleaned.

# Miscellaneous

## `fit_profile.py`

This is a python tool to fit a general temperature or density profile as given by the pedestalised profile parameterisation (`ipedestal=1`) to an ascii table. It is using a least squares method and it fitting the position of the pedestal as well as the peaking factors. Optional arguments are

```
  -h, --help            show this help message and exit
  -f FILENAME, --filename FILENAME
                        ascii file containing data in columns, default =
                        profile.txt
  -r RHO, --rho RHO     column of the normalised radius rho=r/a, default = 0
  -n DENSITY, --density DENSITY
                        column of the density profile, default = 1
  -t TEMPERATURE, --temperature TEMPERATURE
                        column of the temperature profile, default = 2
  -rn RHOPEDN, --rhopedn RHOPEDN
                        user defined initial guess of the density pedestal
                        position, if outside [0,1] starts at 0.9, default =
                        0.9
  -rt RHOPEDT, --rhopedt RHOPEDT
                        user defined initial guess of the temperature pedestal
                        position, if outside [0,1], starts at 0.9, default =
                        0.9
```

If the column of the density or temperature data does not exist, it is ignored. A warning is issued.

## `create_dicts.py`

This automatically generates the `process_dicts.py` file used by PROCESS utility programs. It does this by scanning the Fortran source code. The standard output should be rejected, using

`create_dicts.py > process_dicts.py`

## Line Length Checker

`line_length_standard.py`

Script to check line length of repository files

## `Compare_radials.py`

`compare_radials.py`

Radial plot comparison tool using PLASMOD-type input

## References

[^1]: M. Kovari, R. Kemp, H. Lux, P. Knight, J. Morris, D. J. Ward *"PROCESS: a systems code for fusion power plants - Part 1: Physics"*, Fusion Engineering and Design 89, 30543069 (2014), http://dx.doi.org/10.1016/j.fusengdes.2014.09.018
[^2]: M. Kovari, F. Fox, C. Harrington, R. Kembleton, P. Knight, H. Lux, J. Morris *"PROCESS: a systems code for fusion power plants - Part 2: Engineering"*, Fus. Eng. & Des. 104, 9-20 (2016)
[^3]: H. Lux, R. Kemp, D.J. Ward, M. Sertoli *"Impurity radiation in DEMO systems modelling"*, Fus. Eng. & Des. 101, 42-51 (2015)
[^4]: H. Lux, R. Kemp, E. Fable, R. Wenninger, *"Radiation and connement in 0D fusion systems codes"*, PPCF, 58, 7, 075001 (2016)
[^5]: *"Report on the Systems Code Activities by CCFE in 2014"*, R. Kemp, H. Lux, J. Morris, M. Kovari, P. Knight et al., EuroFusion Report EFDA D 2M94N2 v1.0 - PMI-7.1-2, December 2014 https://idm.euro-fusion.org/?uid=2M94N2&version=v1.0&action=get_document