# Input File

The input file `IN.DAT` is used to change the values of the physics, engineering 
and other code parameters from their default values, and to set up the numerics 
(constraint equations, iteration variables etc.) required to define the problem 
to be solved. 

If the code encounters a problem reading the input file, it will stop immediately 
with an error message. The last line of the output file `OUT.DAT` may give an 
indication of where in the input file the problem lies.

## File Naming Convention

The default PROCESS input file name is `IN.DAT`. The user can provide a named 
input file, that will produce named output files, provided the last 6 characters 
of the input file name are `IN.DAT`.

```bash
./process.exe my_file_name_IN.DAT
```

Will produce output files named

- `my_file_name_OUT.DAT`
- `my_file_name_MFILE.DAT`
- `my_file_name_OPT.DAT`

If no input file name is given as the first argument to the code, it assumes an 
`IN.DAT` file is present in the current directory.

## Constraints

The built-in equation solvers within PROCESS act on a special class, known as constraint 
equations, all of which are formulated in the source file `constraint equations.f90`. 

In the input file constraint equations are specified in the following way:

```
icc = 2 
```

Where `icc` is the constraints array in PROCESS and the user is requesting constraint 
equation 2. The user can add a comment on the same line

```
icc = 2 * Global power balance (consistency equation)
```

Some constraints have `f-value` variables. These are set as iteration variables, 
which are discussed below.

!!! Note "Constraints"
    See [solver](../../solver-guide/index.html) page for more info

## Iteration Variables

Variables that are adjusted by PROCESS in order to satisfy the constraints and 
optimise the figure of merit are referred to as iteration variables. Successive calls 
are made to the physics and engineering routines, with slightly different values for 
the iteration variables on each call, and the equation solver determines the effect on the 
output due to these small changes to the input.

It is important to remember that iteration variables must never be initialised to zero. 
The code will not be able to adjust the variable’s value if this is done, and it will 
stop with an error message.

An iteration variable can be specified in the input file using the following

```
ixc = 3
```

Where `ixc` is the iteration variable array in PROCESS. Like constraints, an in-line comment
can be included

```
ixc = 3 * Plasma major radius [m]
```

For example, the major radius is available as an iteration variable, and appears in the variable
description file as `rmajor /8.14/ : plasma major radius (m) (iteration variable 3)`. If it
is selected as an iteration variable, it will be adjusted by the code. The value input by the user (or
the default, if no value is specified), will be used as the starting value.

!!! Note "Constraints"
    See [solver](../../solver-guide/index.html) page for more info

## Bounds

The upper/lower bound of an iteration variable can be set by the user in the input file by

```
boundl(3) = 8 

boundu(3) = 12
```

Where `3` is the iteration variable number (in this case the major radius). Often the iteration
variable and its bounds are input together

```
ixc = 3 * Plasma major radius [m]
boundl(3) = 8
boundu(3) = 12
```

## Numerics

The numerics section of the input file contains the information about what the solver 
should accomplish. Firstly, the user can input what solver to use

```
ioptimz  = 1 * for optimisation VMCON only
```

In this case, the `VMCON` optimisation solver. The user can also input what value 
to use as the figure of merit for the run.

```
minmax   = 1 * Switch for figure-of-merit (see lablmm for descriptions)
```

In this case the user is choosing option `1`, which is major radius. For `minmax`

* a **positive** value means **minimise** the figure of merit
* a **negative** value means **maximise** the figure of merit

The user can also input the allowed error tolerance on the solver solution.

```
epsvmc   = 1.0e-8 * Error tolerance for vmcon
```

## Fixed Inputs

One can enter an input into the `IN.DAT`  by

```
rmajor = 8.90 * Plasma major radius [m]
```

The `*` is for adding comments to the input file. To comment out an entire line 
one can add a `*` to the beginning of the line, as below:

```
*rmajor = 8.90 * Plasma major radius [m]
```

!!! Note "Variable Descriptions"
    A full list of possible inputs is given in the PROCESS `html` documentation 
    file `vardes.html` and on the variable description page [here](../../vardes/index.html).

## Scan

One of a number of variables can be scanned during the course of a PROCESS
run. This option provides a method of determining the sensitivity of the
results to different input assumptions. The user specifies which variable is
to be scanned with

```
nsweep = 1 
isweep = 4
sweep = 2.8, 2.9, 3.0, 3.1
```

where `nsweep` is the scan variable chosen (see [variable descriptions](../../vardes/index.html)),
`isweep` is the number of scan points and `sweep` is the array of scan values. There 
is the option to have a 2-D scan in PROCESS using the switch `scan_dim = 2` as below

```
scan_dim = 2

nweep = 1
isweep = 4
sweep = 2.8, 2.9, 3.0, 3.1

nweep_2 = 4
isweep_2 = 4
sweep_2 = 1.0, 1.1, 1.2, 1.3
```

Where the scan parameters have duplicate names with `_2` for the second scan 
dimension.

The results from the previous scan point are used as the input to the next
scan point. The output files contain all of the scan points for a given run.

!!! Note "Note"
    For obvious reasons, the active scanning variable must not also be an active
    iteration variable.

## Example

Example `IN.DAT` files are available in the repository in the 
folder `/test_suite/test_files/`.
