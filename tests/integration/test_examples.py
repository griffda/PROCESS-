"""Integration test for examples.py.

examples.py is created by exporting examples.ipynb as a Python script.
"""
import runpy
import os
from pathlib import Path

# TODO How to reliably run examples/examples.py script? Relies on relative path
# from project root dir and pytest having project root dir as cwd. Could this be
# improved?


def test_examples():
    """Run the examples.py script and check no exceptions are raised.

    When running a Jupyter notebook, the cwd is set to the notebook's dir. The
    examples.ipynb notebook relies on relative paths to files in the repository,
    due to it being difficult to consistently get the location of a notebook
    from within the notebook itself, so absolute paths can't be used. 
    
    When pytest is used to run the examples.py script (created directly from the 
    notebook), the script uses the actual cwd instead, which is (usually) the 
    project root dir. Therefore the examples.ipynb notebook and pytest-run 
    examples.py script both rely on the cwd being the same, but without 
    intervention it is different in each case.
    
    Hence the test needs to set the cwd to the notebook's dir before running so
    that the examples.py script uses the same cwd as the notebook.
    """
    # Store cwd, which is usually the project root dir, then change to examples/
    cwd = Path.cwd()
    os.chdir("examples")
    # runpy used to run entire examples.py script
    runpy.run_path("examples.py")
    # Revert cwd change
    os.chdir(cwd)
