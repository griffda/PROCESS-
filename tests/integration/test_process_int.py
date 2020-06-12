"""Integration tests for the process.py module."""
from process import process
from process import fortran
from pathlib import Path

# Real input file for testing
# TODO Move to own fixture?
INPUT_FILE = "tracking/baseline_2018/baseline_2018_IN.DAT"
# Convert input file path to absolute and string
input_file_path = str(Path(INPUT_FILE).resolve())

def mock_true():
    """Mock function that returns True.

    :return: True
    :rtype: bool
    """
    return True

def test_process_main(monkeypatch):
    """Test the process.main() function, which performs a Process run."""
    # TODO Actually check that the run succeeds; will only be able to do this
    # when the interface moves a little deeper into the Fortran. Access to ifail
    # is what is wanted. Can't assert anything yet, so for now mock the call 
    # into the Fortran and check that an exception isn't thrown on the way.
    monkeypatch.setattr(fortran.process_module, "process_subroutine", mock_true)
    process.main(args=["-i", input_file_path])