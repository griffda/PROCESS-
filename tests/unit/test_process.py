"""Unit tests for the process.py module."""
from process import process

def test_main():
    """Check that the main function can run without throwing an error.
    
    This checks that the Fortran can be called from the process.py Python 
    module.
    """
    assert process.main() == None