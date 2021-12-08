# Debugging PROCESS

For the two tools below, running `f2py` with the `--debug` flag will include debug information into the compilation, and may provide some additional information to GDB and Valgrind outputs.

## GDB
Debuggers provide an alternative to `print` statements littering your code. These statements are often left in (or commented out) and can clutter up source code. When debugging within Fortran, they also mean recompilation is required - which takes a few minutes.

A couple of small "hacks" can be used to run PROCESS with GDB. 

Inside of the `dev` folder, the `process_wrapper.py` script can be found. This script is exactly the same as is run by the `process` command. However, as a Python script, we can now run PROCESS through GDB.

```bash
gdb python
>>> run dev/process_wrapper.py -i tracking/baseline_2018/baseline_2018_IN.dat
```

You can now perform any GDB action on PROCESS Fortran source.

### Breakpoints

Adding breakpoints is the most universally useful part of 

```bash
gdb python
>>> break vmcon.f90:200
```

If, at this point, GDB says it cannot find the source file, select the **y**es option as it will be able to find the source file PROCESS is run.

```bash
>>> run dev/process_wrapper.py -i tracking/baseline_2018/baseline_2018_IN.dat

Thread 1 "python" hit Breakpoint 1, vmcon_module::vmcon4 (niter=1, n=41, m=24, mpnppn=<optimized out>, mpnpp1=67, meq=24, lcnorm=176, lb=176, ldel=1232, lh=352, lwa=352, liwa=1143, npp=84, nsix=252,
    np1=42, mpn=65, mp1=25, tol=1e-08, objf=1.7780200000000002, best_sum_so_far_in=999, iwa_in=..., ilower=..., iupper=..., fgrd=..., conf=..., bdl_in=..., bdu_in=..., x=..., delta_in=..., gm_in=...,
    cm_in=..., wa=..., bndl=..., bndu=..., best_solution_vector_in=..., cnorm_in=..., b_in=<error reading variable: value requires 247808 bytes, which is more than max-value-size>,
    h=<error reading variable: value requires 991232 bytes, which is more than max-value-size>, info_in=0, mact_in=0, exit_code=0, info_out=0, mact_out=0, iwa_out=..., spgdel=0, sum=0,
    lowest_valid_fom=0, aux=0, best_sum_so_far_out=0, gm_out=..., vlam=..., glag=..., glaga=..., xa=..., vmu=..., delta_var=..., best_solution_vector_out=..., cnorm_out=..., b_out=..., bdl_out=...,
    bdu_out=..., delta_out=..., cm_out=...) at /root/process/source/fortran/vmcon.f90:60
60	    bdl_out = bdl_in(:size(bdl_out))
```

From this point you can then print out variables of interest. This includes module variables, as well as local variables, and routine/function parameters.

```bash
>>> print meq

$1 = 24
```

## Valgrind
Valgrind provides, among other things, a memory error checker, `memcheck`. We provide the Python suppression file, that stops errors in the Python binary being reported, in the `dev` folder as `valgrind-python.supp`.

We can then run the valgrind commands to chech for memory errors.

Perform standard analysis that outputs to `valgrind.log` in your current directory:
```
PYTHONMALLOC=malloc valgrind --suppressions=dev/valgrind-python.supp --log-file=valgrind.log process -i tests/regression/scenarios/2D_scan/IN.DAT
```

Perform some more detailed analysis that checks for the origin of unitialised values:
```
PYTHONMALLOC=malloc valgrind --suppressions=valgrind-python.supp --track-origins=yes --log-file=valgrind.log process -i tests/regression/scenarios/2D_scan/IN.DAT
```

Or, we can perform the most rigerous checks that report all memory leaks:
```
PYTHONMALLOC=malloc valgrind --suppressions=valgrind-python.supp --track-origins=yes --leak-check=full --log-file=valgrind.log process -i tests/regression/scenarios/2D_scan/IN.DAT
```