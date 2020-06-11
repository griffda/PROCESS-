"""Unit tests for the process.py module."""
from process import process
from process import fortran
import pytest
from pathlib import Path
import argparse

# Real input file for testing
# TODO Move to own fixture?
INPUT_FILE = "tracking/baseline_2018/baseline_2018_IN.DAT"
# Convert input file path to absolute and string
input_file_path = str(Path(INPUT_FILE).resolve())

# TODO Perhaps this should be an integration test?
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

def mock_init(*args, **kwargs):
    """Used to mock out __init__ methods on classes.

    :return: Nothing
    :rtype: Nonetype
    """
    return None

@pytest.fixture
def process_obj(monkeypatch):
    """Fixture to create a Process object.
    
    Returns a Process object with a mocked empty __init__ method; create the 
    object, but don't run the real __init__.

    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    :return: Process object
    :rtype: object
    """
    monkeypatch.setattr(process.Process, "__init__", mock_init)
    # Mock the __init__ method of the Process class with mock_init
    process_obj = process.Process()
    # Return the mocked Process object
    return process_obj

def test_Process(process_obj):
    """Test that Process objects can be created.
    
    Check the process_obj fixture can make an object of type Process.
    :param process_obj: Process object
    :type process_obj: object
    """
    assert type(process_obj) is process.Process

def test_parse_args(process_obj):
    """Test parse_args() method.

    Check the input file path argument is being stored on the Process object.
    :param process_obj: Process object
    :type process_obj: object
    """
    # Run parse args method and check file path is stored
    process_obj.parse_args(args=["-i", input_file_path])
    assert process_obj.args.input == input_file_path

def test_set_input(process_obj, monkeypatch):
    """Check the input file validation and setting of path.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # Mock the input file path to isolate this test from the other Process 
    # methods (don't have to run Process.parse_args() first to set up this way)
    # Set self.args to a Namespace object, then self.args.input to the input 
    # file path, like parse_args has already run
    monkeypatch.setattr(process_obj, "args", argparse.Namespace(), raising=False)
    monkeypatch.setattr(process_obj.args, "input", input_file_path, raising=False)
    
    # Mocks set up, can now run set_input()
    process_obj.set_input()

    # Check path has been set in the Fortran
    fileprefix = fortran.global_variables.fileprefix.decode().strip()
    # Convert string from byte-string for comparison
    assert fileprefix == input_file_path

def test_set_output(process_obj, monkeypatch):
    """Check output filename setting in the Fortran.

    :param process_obj: Process object fixture
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # Expected output prefix stored in Fortran
    expected = "output_prefix"
    # Mock self.filename_prefix on the process_obj with the value of expected
    monkeypatch.setattr(process_obj, "filename_prefix", expected, raising=False)
    # Run the method, and extract the value from the Fortran
    process_obj.set_output()
    # Convert string from byte-string for comparison
    result = fortran.global_variables.output_prefix.decode().strip()
    assert result == expected

# TODO Do the fortran modifications stick around? They're not a fixture...