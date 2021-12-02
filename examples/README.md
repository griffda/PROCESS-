# Process Jupyter notebook examples

Examples of how to use Process are provided in the form of Jupyter notebooks. These provide a convenient way of explaining Process usage alongside runnable code cells, as well as displaying some types of output.

## Running notebooks

### Installation

`jupyter` (which includes the `notebook` package) should already be installed in the `process` virtual environment when the `cmake` installation is performed (it is included in the `requirements.txt`). If not, install with `pip install jupyter` (for all Jupyter packages) or `pip install notebook` (for just the `notebook` package).

### Notebooks in VS Code

The easiest way to run notebooks is in VS Code. Simply open the `.ipynb` file in VS Code to view and run it.

### Notebooks via the Jupyter server

The original way of running Jupyter notebooks has been via a web browser and the Jupyter server. To launch the server, navigate to the `examples` directory and run:

```bash
python -m notebook
```

A web browser will open and the notebook can be run from there.

## Maintaining notebooks and scripts
Notebooks (`.ipynb`) and scripts (`.py`) with the same filenames are maintained in the `examples` directory. From within VS Code, a notebook can be exported as a Python script. The reason for this is so that notebooks are always available to run directly, whilst the corresponding Python scripts are much easier to track diffs in when reviewing changes (notebooks are ultimately JSON and so create difficult diffs). The scripts can also be tested by `pytest` to ensure the example notebooks keep working.