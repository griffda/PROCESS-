# %% [markdown]
# # Process examples
# A Jupyter notebook to demonstrate usage of the `process` package. This notebook has also been exported as a Python script to `examples.py`.
# 
# ## Motivation
# Process is moving away from being a runnable package with a single command-line entry-point to an importable package which can be scripted. This notebook is a good way of demonstrating the functionality of the package, and could provide a better way of working for modellers, who may wish to create their own scripts or notebooks for different tasks.
# 
# ## Basic run of Process
# Run Process on an input file using the `SingleRun` class. This outputs an `MFILE.DAT` and an `OUT.DAT`.

# %%
import process
from process.main import SingleRun
from pathlib import Path

# Define working directory relative to project dir and input file name
wdir_rel = Path("tracking/baseline_2018")
input_name = "baseline_2018_IN.DAT"

proj_dir = Path(process.__file__).parent.parent
wdir_abs = proj_dir / wdir_rel
input_path = wdir_abs / input_name

# Run process on an input file
single_run = SingleRun(str(input_path))


# %% [markdown]
# ## Plot summary
# Create a summary PDF of the generated `MFILE.DAT` using `plot_proc`.

# %%
from process.io import plot_proc
from pdf2image import convert_from_path

# Create a summary PDF
plot_proc.main(args=["-f", str(single_run.mfile_path)])

# Convert PDF to PNG in order to display in notebook
summary_pdf = str(single_run.mfile_path) + "SUMMARY.pdf"
pages = convert_from_path(summary_pdf)
for page_no, page_image in enumerate(pages):
    png_path = wdir_abs / f"plot_proc_{page_no + 1}.png"
    page_image.save(png_path, "PNG")


# %% [markdown]
# `plot_proc`'s PDF output.
# 
# <img src="tracking/baseline_2018/plot_proc_1.png" width="1000">
# <img src="tracking/baseline_2018/plot_proc_2.png" width="1000">

# %% [markdown]
# ## View key output variables
# Run the Starfire scenario using `SingleRun` to set some values on the `CostsStep` instance and then print them.

# %%
# Define working directory relative to project dir and input file name
wdir_rel = Path("tests/regression/scenarios/starfire")
input_name = "IN.DAT"

wdir_abs_starfire = proj_dir / wdir_rel
input_path = wdir_abs_starfire / input_name

# Run process on an input file
single_run = SingleRun(str(input_path))


# %%
print(
    f"Building and Site Service Infrastructure: {single_run.models.costs_step.step21:.3e} M$"
)
print(f"Reactor plant equipment: {single_run.models.costs_step.step22:.3e} M$")


# %% [markdown]
# ## VaryRun
# Vary iteration parameters until a feasible solution is found, using the `VaryRun` class.

# %%
from process.main import VaryRun

wdir_rel = Path("tests/integration/data")
conf_name = "run_process.conf"

wdir_abs_vary = proj_dir / wdir_rel
conf_path = wdir_abs_vary / conf_name
vary_run = VaryRun(str(conf_path))

# %% [markdown]
# ## Plot scan
# Plot a scanned MFILE.

# %%
from process.io import plot_scans

# Define working directory relative to project dir and input file name
wdir_rel = Path("tests/integration/data")
mfile_name = "scan_MFILE.DAT"

wdir_abs_plot_scan = proj_dir / wdir_rel
mfile_path = wdir_abs_plot_scan / mfile_name

plot_scans.main(
    args=["-f", str(mfile_name), "-yv", "thwcndut", "--outputdir", str(wdir_abs_plot_scan)]
)

# %% [markdown]
# ## Clean up
# Currently files are created in locations that can't be easily controlled; this will be changed to a temporary directory with ongoing development. For now, clean these manually here.

# %%
from os import remove

# TODO This obviously needs sorting out
# Delete SingleRun baseline2019 files
delete = (
    "baseline_2018_MFILE.DAT",
    "baseline_2018_OPT.DAT",
    "baseline_2018_OUT.DAT",
    "baseline_2018_PLOT.DAT",
    "baseline_2018_SIG_TF.DAT",
    "baseline_2018_MFILE.DATSUMMARY.png",
    "plot_proc_1.png",
    "plot_proc_2.png",
)

for file in wdir_abs.glob("*"):
    if file.name in delete:
        remove(file)

# Delete VaryRun files
delete = (
    "IN.DAT",
    "OPT.DAT",
    "SIG_TF.DAT",
    "README.txt",
)

for file in wdir_abs_vary.glob("*"):
    if file.name in delete:
        remove(file)



