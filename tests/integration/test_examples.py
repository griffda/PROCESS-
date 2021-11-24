"""Integration test for examples.py.

examples.py is created by exporting examples.ipynb as a Python script.
"""
import runpy

# TODO How to reliably import/test examples.py script? Relies on relative path
# and pytest having project root dir as cwd. Could this be improved?


def test_examples():
    """Run the examples.py script and check no exceptions are raised."""
    # runpy used to run entire script; contains no functions/classes
    runpy.run_path("examples.py")
