"""Unit tests for the process.py module."""
from process import process
import pytest

def test_main():
    """Check that the main function can run.
    
    Call the main function without any arguments. No input file is specified,
    so this should raise a FileNotFoundError, in which case the test passes.
    """
    with pytest.raises(FileNotFoundError):
        process.main(args=[])
        # If args is None, then the argparse parser uses sys.argv (i.e. the 
        # command-line args) instead. When running from pytest, these are some
        # pytest-specific arguments that we don't want going into the Process
        # argparser. Hence explicitly setting args=[] ensures that the Process
        # argparser gets an empty list (i.e. no arguments). This way it is
        # possible to test command-line arguments from the test suite, as if the
        # arguments are supplied on the command-line.