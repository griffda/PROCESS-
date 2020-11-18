"""Integration tests for the process.py module."""
from process import main
from process import fortran
from pathlib import Path

# Real input file for testing
# TODO Move to own fixture?
INPUT_FILE = "tracking/baseline_2018/baseline_2018_IN.DAT"
# Convert input file path to absolute and string
input_file_path = str(Path(INPUT_FILE).resolve())

def test_process_main():
    """Test the main.main() function, which performs a Process run."""
    # TODO Actually check that the run succeeds; will only be able to do this
    # when the interface moves a little deeper into the Fortran. Access to ifail
    # is what is wanted. Can't assert anything yet, so for now call into the 
    # Fortran and check that an exception isn't thrown on the way.
    try:
        main.main(args=["-i", input_file_path])
        result = True
    except:
        result = False

    assert result == True