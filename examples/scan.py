# %% [markdown]
# # Example scan
# 
# Perform a parameter scan for a given input file and plot the results.
# 
# ## Scan details
# 
# The input file is a scan-enabled version of the baseline 2018 `IN.DAT`, as found in the `tracking` directory. The scan-relevant values are:
# ```
# nsweep = 17 * bmxlim, maximum peak toroidal field (T) (`constraint equation 25`)
# isweep = 11
# sweep = 11. , 11.2, 11.4, 11.6, 11.8, 12. , 12.2, 12.4, 12.6, 12.8, 13.
# ```

# %%
from process.main import SingleRun
from pathlib import Path

prefix = "baseline_2018_scan_"
input_name = Path(prefix + "IN.DAT")

# Perform a SingleRun on a scan-enabled input file
single_run = SingleRun(str(input_name))


# %% [markdown]
# ## Plot scan results
# Use `plot_scans.py` to plot the resulting `MFILE.DAT`.

# %%
from process.io import plot_scans

# Define working directory relative to project dir and input file name
mfile_name = Path(prefix + "MFILE.DAT")
output_dir = Path.cwd()

plot_scans.main(
    args=[
        "-f",
        str(mfile_name),
        "-yv",
        "bt rmajor pnetelmw powfmw capcost",
        "--outputdir",
        str(output_dir),
    ]
)



