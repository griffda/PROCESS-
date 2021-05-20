# Usage
## Running Process
Process can be run in two main modes, SingleRun (where Process runs once) and VaryRun (where Process runs multiple times, varying iteration parameters until a solution is found (what used to be run_process.py)).

For a SingleRun:
```bash
# Use an IN.DAT file in the current directory
process

# Use an IN.DAT file outside of the current directory
process -i path/to/IN.DAT 
```

For a VaryRun:
```bash
# Use a configuration file called run_process.conf in the current directory
process -v

# Use a conf file outside of the current directory
process -v -c path/to/my_conf_file.conf
```

The available arguments are:
```bash
process [--input, -i input_file_path] [--varyiterparams, -v] [--varyiterparamsconfig, -c config_file_path] [--help, -h]
```

Help is available with:
```bash
process --help
```

## Running Basic Utilities
### plot_proc
`plot_proc` is used for plotting an MFILE. It can be run using its own CLI:
```bash
python process/io/plot_proc.py -f path/to/MFILE.DAT
```

or through Process's main CLI (working, but still in development):
```bash
process -i path/to/IN.DAT --plot --mfile path/to/MFILE.DAT
```

## Coverage
### Coverage
Code coverage is tracked using `gcov` and the data files are generated when PROCESS is run. 
The coverage report is generated using `lcov` and is stored as webpages in the `html` folder inside
the `lcov_results` folder, to access it simply open `index.html`. The coverage data inside the `.gcda` files
is accumulative, that is to say that if PROCESS is run multiple times the `.gcda` files will have
coverage data for all of the PROCESS runs combined. There is no way to separate these, so if coverage data for 
individual runs needs to be recovered then the best method is to clear existing `.gcda` files, run PROCESS, 
generate the coverage report and copy the contents of the `lcov_results` folder out of the `process` directory 
and store it separately.
### Generate coverage report
To generate a coverage report, use the command:
```bash
cmake --build build --target coverage
```
This must be done after running PROCESS at least once.
### Clear existing coverage data
To clear existing coverage data (`.gcda` files) before a new run of PROCESS, use:
```bash
cmake --build build --target coverage_cleanup
```
Note that all existing coverage data is cleared on every re-build of PROCESS.

## Running Regression Tests
### regression
After PROCESS is installed, use:
```bash
cmake --build build --target regression
``` 
to run regression tests only. 
If the coverage data files (`.gcda` files) need to be cleared before running the regression tests use:
```bash
cmake --build build --target coverage_cleanup regression
```
If a coverage report for the regression tests needs to be generated use:
```bash
cmake --build build --target coverage_cleanup regression coverage
```