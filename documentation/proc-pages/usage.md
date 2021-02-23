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