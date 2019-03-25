# PROCESS

PROCESS is the reactor systems code at [CCFE](www.ccfe.ac.uk). More information on PROCESS
can be found on the PROCESS [webpage](http://www.ccfe.ac.uk/powerplants.aspx).

## Documentation

### User guide
To read about how the code works and the modules in it see the
[user guide](http://www.ccfe.ac.uk/assets/Documents/Other/process.pdf)

### Physics paper

A [paper](http://www.sciencedirect.com/science/article/pii/S0920379614005961)
outlining the physics models in PROCESS published in Fusion Engineering and Design.

### Engineering paper

A [paper](http://www.euro-fusionscipub.org/wp-content/uploads/2015/08/WPPMIPR1505.pdf)
outlining the engineering models in PROCESS published in Fusion Engineering and Design.

### Other papers

A list of other papers using PROCESS:
- "Impurity radiation in DEMO systems modelling", H. Lux et al., 2015, Fusion Engineering and
Design, ([paper](http://www.sciencedirect.com/science/article/pii/S0920379615302891))
- "Implications of toroidal field coil stress limits on power plant design using PROCESS", J. Morris et al.,
SOFT 2014, Fusion Engineering and Design ([paper](http://www.sciencedirect.com/science/article/pii/S0920379615301290)).

## Build System

A number of software technologies are employed in PROCESS. [CMake](https://cmake.org/) is the compiler, for 
linking the modules and creating the executable and shared object. `process.exe` is the executable, and it links to the
shared object, libPROCESS_Calculation_Engine.

PROCESS calculation modules are primarly written in Fortran. Python is used for running integration tests.

[pFUnit](http://pfunit.sourceforge.net/), and [Googletest](https://github.com/google/googletest) are used for unit testing 
functions and subroutines within PROCESS. Googletest is also referred to as GTest in this document.

On Freia, paths to PFUnit and GTEST can be set in your user profile (.bashrc) as 
- `export PFUNIT=/home/PROCESS/testing_frameworks/pfunit_install/V_3-2_8`
- `export GTEST=/home/PROCESS/testing_frameworks/googletest/googletest`

## Directory Structure

The folder structure for the PROCESS system prior to compilation is descibed below:

```
*-- CMakeLists.txt                      : Build and compile files
*-- GNUmakefile                         : Build and compile pFUnit files
+-- lib                             : Libraries used in PROCESS
|   +-- PLASMOD                         : PLASMOD lib files
+-- source                          : source files
|   +-- Fortran                         : Fortran source files
|   +-- cpp                             : C++ source files
+-- test_suite
|   *-- ci_test_suite.py                : Python file for running test suite in Continuous integration system
|   *-- ci_test_suite_functions.py      : Python functions  for running test suite in Continuous integration system
|   *-- test_suite.py                   : Python file for running test suite by user on command line
|   *-- test_suite_functions.py         : Python functions  for running test suite by user on command line
|   +-- test_files                      : Input files for test suite
|   +-- test_area                       : Output files for test suite
+-- test_files
|   +-- pfunit_files                    : pFUnit test files
|   +-- gtest_files                     : GTest test files
+-- utilities/                          : Python utilities files
+-- fispact/                                : fispact Data file
+-- data                                : Data files
|   +-- fluids
|   +-- h_data
|   +-- lz_non_corona
|   +-- lz_non_corona_14_elements
+-- documentation                       : Contain documentation files
*-- IN.DAT                              : Sample PROCESS input file
```

## Build Steps
- When using Freia, it is recommended to load `gfortran` and unload `ifort` or other Fortran compilers explicitly before build:
    - `module unload ifort`
    - `module unload pgi`
    - `module load gfortran`  

1. get repository
    - `git clone git@git.ccfe.ac.uk:process/process.git folder_name`. Where `folder_name`is the name of the folder which will be created when cloning the repository.  
2. Inside the PROCESS directory, run CMAKE to build, compile and generate the executable and shared object
    - `cmake3 -H. -Bbuild`
      - or `cmake3 -H. -Bbuild -Ddebug=ON` to use all compiler warnings (`-Wall` and `-Wextra`).
    - `cmake3 --build build`
    - Step 2 will create a folder called `bin`, which contains three files: process.exe, process_GTest.exe and libPROCESS_calc_engine.so
3. pFUnit unit test files are located in the folder _test_files/pfunit_files/_ with extension _.pf_. Use `make tests` from your home directory to run the pFUnit test suite   
4. GTest unit test files are located in the folder _test_files/gtest_files/_ with extension _.h_. Use `./bin/process_test` from your home directory to run the GTest test suite 

During the compile and build steps, a number of files and folders are created. Additional files in the folder structure are listed below:

```
+-- build                       : This folder is generated after cmake3 -H. -Bbuild
+-- bin                         : This folder is generated after cmake3 --build build
|   *-- process.exe             : Executable for running PROCESS
|   *-- libPROCESS_calc_engine.so   : Shared object containing all PROCESS functions, subroutines and modules
|   *-- process_GTest.exe           : executable for running PROCESS unit test cases in GTest
+-- documentation                       : Contains documentation files
|   +-- html             : Documentation generated after cmake3 --build build --target vardes
*-- process.exe                     : PROCESS executable copied from /bin folder after finishing build process
*-- tests.x                         : PROCESS executable copied from /bin folder after finishing build process
*-- pfunit_results.xml              : pFUnit unit testing results file after running make tests
*-- OUT.DAT                         : PROCESS output file generated after running ./process.exe
*-- MFILE.DAT                       : PROCESS output file generated after running ./process.exe
*-- VFILE.DAT                       : PROCESS output file generated after running ./process.exe
```


Additionally

- to make python dictionaries run `cmake3 --build build --target dicts`
- to make documentation run `cmake3 --build build --target doc`. (pdf compilation on UKAEA Freia machines 
  requires the following line in your `.bashrc` file: `module load texlive/2017`)
- to make html files run `cmake3 --build build --target html`
- to clean the directory run `rm -rf build`

## Run

- create input file IN.DAT
- run `./process.exe`
- run `./process.exe help` provides help page
- results are output in OUT.DAT, MFILE.DAT
- optionally, run the `utilities/run_process.py` script in conjunction with a config file to randomly vary the starting point of the input parameter set until a feasilble solution is found.
    - `run_process.py -f CONFIGFILE`
    - An example of the config file can be found in `documentation/pdf/utilitiesdoc.pdf`

### Batch Mode on Freia

If you want to run PROCESS as a batch job on Freia do the following:
- create a folder in /common/scratch/ (e.g. `mkdir /common/scratch/process-batch`)
- copy across `process.exe` and `libPROCESS_calc_engine.so` into the folder
- copy your input file into the folder
- create a job file. e.g. `touch process.cmd`.

`process.cmd`  should contain the following:
```
# @ executable = ./process.exe
# @ arguments = <input filename>_IN.DAT
# @ input = /dev/null
# @ output = /home/<username>/baseline_2019.out
# @ error = /home/<username>/baseline_2019.err
# @ initialdir = /common/scratch/<folder name>/
# @ notify_user = <username>
# @ notification = complete
# @ queue

```

**NOTE: you need the empty line at the end of the file.**

To run the job enter:
```
llsubmit process.cmd
```

To see the status of your job enter:
```
qstat
```

## Development

### Commit Logs

To see the commit messages you can use the `git log` command. There are various options
described below.

| Command | Description | Example |
| -------- | -------- | --------- |
| `-(n)`   | show the last `n` commits  | `git log -5` |
| `--since or --after` |  limits the logs to be from date given | `git log --since "21-01-15"` |
| | can use `number.scale` where scale=year, month, week, day and minute | `git log --since 2.weeks` |
| `--until or --before` |  limits the logs to be up to date given | `git log --until "22-01-15"` |
| `--author` | only shows commits from given author | `git log --author "morrisj"` |
| `--grep` | only show commits with a commit message containing the string given | `git log --grep "magnet"`  |
| `--stat` | if you want to see some abbreviated stats for each commit | `git log --stat` |
| `--oneline` | Outputs commit number, date and message to a single line | `git log --oneline` |
| `--graph` | display commits in a ASCI graph/timeline | `git log --graph` |
| `-S` | only show commits adding or removing code matching the string | `git log -S "find_me"` |

- to output the log to a file add `>> file_name.log` to the end of the command

### Changing the code

Major changes

- Create a new branch (e.g. "model_a_development") on the GitLab webpage.
    - or run `git branch new_branch_name` after cloning the repo (next bullet)
- Clone the repository `git clone git@git.ccfe.ac.uk:process/process.git`
- Swap to your branch `git checkout new_branch_name`

Minor changes (e.g. single line changes)

- Clone the repository `git clone git.git.ccfe.ac.uk:process/process.git`

### Committing changes

- Make your changes to the code and at suitable stages commit locally:
    - `git add file_changed_1 file_changed_2`
    - `git commit -m "COMMIT MESSAGE"`
- The commit message should be informative and give useful information for future development.
  - Such as:
  ```
  Made changes to the TF coil magnet model. Updated the allowable stress in the coils
  to be 600MPa. Remove side-wall case. Ran test suite and everything OK.
  ```
  - not
  ```
  Update to magnet model
  ```

- Before pushing back to the repository make sure that your branch is up to date
with any changes that might have been made by other developers, `git pull`
- When you wish to push your branch back to the repository enter `git push`

### Merging

#### Develop into your branch

When you have finished making a major change on a new branch, you will need to merge your branch with the develop branch to keep up with the latest changes.

- Make sure you have committed all of your changes to your local branch.
- Update your local repo with `git pull`
- Checkout the development branch `git checkout develop`
- Check remote repo again `git pull`
- Checkout your new branch `git checkout my_branch_name`
- Merge develop into your branch `git merge develop`
- If there are conflicts check the files listed for the following:
```
<<<<<<< HEAD
This line was edited in dev_mynewmodel branch
=======
This line was edited in develop branch
>>>>>>> develop
```

- Resolve any conflicts then `git add file_1 file_2` where file_1 and file_2 are
files that had conflicts.
- Commit the changes `git commit`
- Push the branch back to the remote repo `git push`

#### Your branch into develop

After having developed your branch, and merged develop into it as detailed in the previous comment, you will need to merge your branch with develop.

- Check your repo is up to date `git pull`
- `git checkout my_branch_name`
- `git checkout develop`
- `git merge my_branch_name`
- Resolve conflicts in similar manner to section above
- `git push`

### Tagging

Version takes the form `x.y.z` for internal development versions and takes the form `x.y` 
for external master releases. 

This is similar to the .NET convention of version numbering


`[major version]`.`[minor version]`.`[revision number]`

`[major version]` - release containing numerous major changes

`[minor version]` - medium change, i.e. new model, major bug fix

`[revision number]` - weekly or on demand build/change


To add a tag to a commit do the following:

- In subroutine `inform` in the file `process.f90`, change the value of `progver` by
incrementing the revision appropriately (given guidance above) to `x.y.z` and the release date. It
is important to keep exactly the same format.
- Add a brief comment to the bottom of the source file `process.f90` describing the changes made
since the last commit in the same branch.  Start the line with `! x.y.z:`, following the
existing examples
- If any of the User Guide `.tex` files have been modified, edit the value of `\version`
in `process.tex` by changing the Revision to `x.y.z` and the date
- If you have changed any ”use” statements in the code, or any compilation dependencies in the Makefile, run
`rm -rf build`
- To ensure that all the code and documentation compiles successfully re-run the compiler, including the making of dicts, docs, etc.
- Check `test_suite.py` in the `test_suite` folder runs successfully
- Add files changed `git add file_1 file_2 ...`
- Commit changes `git commit -m "COMMIT MESSAGE"`
- Add a tag number `git tag -a x.y.z -m "Version x.y.z"`
- `git push`
- `git push origin x.y.z`

When releasing a ***tagged version of the code the user should compile a release note for the repo***.
This note should outline the major changes for the release, including issues from GitLab that
were resolved, bugs fixed, etc.

Between user tags Git will create tags in the following format:

```
1.0.12-11-g3f1b433
```

- `1.0.12` is the last manually entered tag by the user
- `11` is the number of commits since that tag
- `g3f1b433` is a unique identifier for this specific commit

This allows the user to checkout a specific commit between tagged versions. PROCESS now outputs this information into the `OUT.DAT` and `MFILE.DAT` and is 
updated upon compilation. This way each output file is trackable to a specific commit.


**Tagging Commands**

| Command | Description |
| -------- | -------- | 
| `git describe --tags`   | show the current tag  | 
| `git tag -l "1.0.*"` | list tags contained in `1.0.z` |
| `git checkout tags/<tag name>` | checkout a specific tag |

## Troubleshooting

If you encounter issues with file line endings when working between Windows and Linux. Run the command on Freia to convert the line endings to unix based line endings for a given file (create_dicts.py in this case).

```dos2unix create_dicts.py```

## Contacts

[Hanni Lux](Hanni.lux@ukaea.uk)

[Michael Kovari](michael.kovari@ukaea.uk)

[James Morris](james.morris2@ukaea.uk)
