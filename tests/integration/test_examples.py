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
    examples.py notebook relies on relative paths to files in the repository,
    due to it being difficult to consistently get the location of a notebook
    from within the notebook itself. When pytest is used to run the examples.py
    script, the cwd is (usually) the project root dir. Hence the test needs to
    set the cwd to the notebook's dir in order to run.
    """
    # Store cwd, which is usually the project root dir, then change to examples/
    cwd = Path.cwd()
    os.chdir("examples")
    # runpy used to run entire examples.py script
    runpy.run_path("examples.py")
    # Revert cwd change
    os.chdir(cwd)
