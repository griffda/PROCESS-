
# Software Requirements

| Software | Version |
| - | - |
| cmake      | 3+    |
| python     | 3+    |
| gcc        | 4.8+  |
| texlive    | 2017+ |
| googletest | 1.8+  |

# UKAEA Freia Cluster

If working on the UKAEA Freia clusters add to your `.bashrc`

```bash
module unload ifort
module load gfortran
module unload python/2.7.5
module load python/3.5.1
module load texlive/2018
export GTEST=/home/PROCESS/testing_frameworks/googletest/googletest
export PYTHONPATH=$PYTHONPATH:[path to process folder]/utilities
```

# Directory Structure

The folder structure for the PROCESS system prior to compilation is described below:

```
.
+-- CMakeLists.txt                      : For building and compiling files
+-- GNUmakefile                         : Build and compile pFUnit files
+-- lib                                 : External libraries used in PROCESS
|   +-- PLASMOD                         : PLASMOD lib files
|   +-- REFPROP                         : REFPROP lib files
+-- source                              
|   +-- Fortran                         : Fortran source files
|   +-- cpp                             : C++ source files
+-- test_suite                          : Contains integration tests
+-- unit_tests
|   +-- pfunit_files                    : pFUnit test files
|   +-- gtest_files                     : GTest unit test files
+-- utilities/                          : Python utilities files
+-- fispact/                            : fispact Data file
+-- data                                : data files
|   +-- fluids
|   +-- h_data
|   +-- impuritydata
|   +-- lz_non_corona
|   +-- lz_non_corona_14_elements
+-- documentation                       : Contain documentation files
```

# Clone Repository

To clone the default branch (develop) from the remote enter:

```bash
git clone git@git.ccfe.ac.uk:process/process.git <folder_name>
```

Where `<folder_name>`is the name of the folder which will be created when 
cloning the repository.

To get a different branch:

```bash
git clone git@git.ccfe.ac.uk:process/process.git -b <branch_name> <folder_name>
```

Where `<branch_name>`is the name of the branch to checkout from the remote 
repository.

# Compiling the Code

!!! Note "On Freia cluster"
    Replace the `cmake` commands below with `cmake3`

Inside the PROCESS directory, run **cmake** to build, compile and generate the 
executable and shared object:

```bash
cmake -H. -Bbuild
```

or to use all compiler warnings (`-Wall` and `-Wextra`) run:

```bash
cmake -H. -Bbuild -Ddebug=ON
``` 


or to compile into single executable without dll:

```bash
cmake -H. -Bbuild -Ddll=OFF
``` 

After compile the code giving the build directory

```bash
cmake --build build
```

This step will create a folder called `bin`, which contains three files

- `process.exe`
- `process_GTest.exe`
- `libPROCESS_calc_engine.so`

## Custom Build Targets

```bash
cmake --build build --target <target_name>
```

| Target Name | Description |
| - | - | 
| dicts          | Makes Python dictionaries used for Python tools |
| doc            | Makes all documentation: PDF and HTML |
| vardes         | Make compact vardes.html file |
| autodoc        | Make autodoc tool |
| html           | Make all HTML documentation files |
| userguide      | Make PDF user guide |
| developerguide | Make PDF developer guide |
| utilitiesdoc   | Make PDF utilities guide |
| optsolverdoc   | Make PDF optimisation solver document |
| tfdoc          | Make PDF TF coil model documentation |

!!! Note "Python dictionaries"
    Once the dictionaries have been created add the utilities folder to the `PYTHONPATH` 

    `export PYTHONPATH=$PYTHONPATH:/home/<user_name>/<path_to_process>/utilities/`

# Running PROCESS

Create input file IN.DAT ([see input file section](input-guide.md))

To run the code (by default will try to use an input file called `IN.DAT` in 
the current directory):

```bash
 ./process.exe
```

To see the code help page

 ```bash
 ./process.exe help
 ```

The results are output to the following output files:

- `OUT.DAT`
- `MFILE.DAT`
- `OPT.DAT`
- `PLOT.DAT`


Optionally, one can run the `utilities/run_process.py` script in conjunction 
with a config file to randomly vary the starting point of the input parameter 
set until a feasilble solution is found. To look a the utility documentation 
compile the utilities document as described [here](getting-started.md).
