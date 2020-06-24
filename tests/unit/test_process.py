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
    expected = input_file_path
    # Mock the input file path to isolate this test from the other Process 
    # methods (don't have to run Process.parse_args() first to set up this way)
    # Set self.args to a Namespace object, then self.args.input to the input 
    # file path, like parse_args has already run
    monkeypatch.setattr(process_obj, "args", argparse.Namespace(), raising=False)
    monkeypatch.setattr(process_obj.args, "input", input_file_path, raising=False)
    
    # Mock the Fortran set
    monkeypatch.setattr(fortran.global_variables, "fileprefix", None)

    # Mocks set up, can now run set_input()
    process_obj.set_input()

    # Check path has been set in the Fortran (mocked above)
    result = fortran.global_variables.fileprefix.decode().strip()
    # Convert string from byte-string for comparison
    assert result == expected

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
    # Mock the Fortran set
    monkeypatch.setattr(fortran.global_variables, "output_prefix", None)
    # Run the method, and extract the value from the Fortran
    process_obj.set_output()
    # Convert string from byte-string for comparison
    result = fortran.global_variables.output_prefix.decode().strip()
    assert result == expected

def test_initialise(process_obj, monkeypatch):
    """Test that the init_module can be called in the Fortran.

    :param process_obj: Process object
    :type process_obj: object
    """
    # Mock the init subroutine with a lambda function
    monkeypatch.setattr(fortran.init_module, "init", lambda: None)
    # Run initialise method; this will fail on a raised exception
    process_obj.initialise()

def test_run_hare_tests(process_obj, monkeypatch):
    """Check main_module.runtests() is run if run_tests == 1.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # TODO Can't actually check that this is being run yet; need a result that
    # can be checked in the Python (isolated fixtures), to avoid side-effects 
    # persisting in the Fortran
    # For now, just check that no exceptions are thrown before calling into 
    # the Fortran
    monkeypatch.setattr(fortran.global_variables, "run_tests", 1)
    monkeypatch.setattr(fortran.main_module, "runtests", lambda: None)
    process_obj.run_hare_tests()

def test_kallenbach_tests(process_obj, monkeypatch):
    """Check that the Kallenbach tests can be run if required.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # TODO Currently only checking for no exceptions before Fortran mock called
    monkeypatch.setattr(fortran.div_kal_vars, "kallenbach_tests", 1)
    monkeypatch.setattr(fortran.kallenbach_module, "kallenbach_testing", 
        lambda: None)
    # Expect a SystemExit, as the code is exited if the Kallenbach tests are run
    with pytest.raises(SystemExit):
        process_obj.kallenbach_tests()

def test_kallenbach_scan(process_obj, monkeypatch):
    """Check the Kallenbach scan can be run.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(fortran.div_kal_vars, "kallenbach_scan_switch", 1)
    monkeypatch.setattr(fortran.kallenbach_module, "kallenbach_scan", lambda: 
        None)
    # Catch a SystemExit after running the scan
    with pytest.raises(SystemExit):
        process_obj.kallenbach_scan()

def test_call_solver(process_obj, monkeypatch):
    """Check that the solver is called with the ifail integer.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # Mock the ifail value returned by the solver as 1
    expected = 1
    monkeypatch.setattr(fortran.main_module, "eqslv", lambda: expected)
    process_obj.call_solver()
    assert process_obj.ifail == expected

def test_scan(process_obj, monkeypatch):
    """Test if scan routine runs based on ioptimz value.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # Mock ioptimz value and check scan can run
    monkeypatch.setattr(fortran.numerics, "ioptimz", 0)
    monkeypatch.setattr(fortran.scan_module, "scan", lambda: None)
    process_obj.scan()

    # If ioptimz < 0, mock call to final
    monkeypatch.setattr(fortran.numerics, "ioptimz", -1)
    monkeypatch.setattr(process_obj, "ifail", 0, raising=False)
    monkeypatch.setattr(fortran.final_module, "final", lambda x: None)
    process_obj.scan()

def test_set_mfile(process_obj, monkeypatch):
    """Check the mfile filename is being stored correctly.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    prefix = "test"
    expected = Path(prefix + "MFILE.DAT")
    # Mock filename_prefix and run
    monkeypatch.setattr(process_obj, "filename_prefix", prefix, raising=False)
    process_obj.set_mfile()
    assert process_obj.mfile_path == expected

def test_show_errors(process_obj, monkeypatch):
    """Check that the show errors subroutine is called.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(fortran.error_handling, "show_errors", lambda: None)
    process_obj.show_errors()

def test_finish(process_obj, monkeypatch):
    """Check that the finish subroutine is called.

    :param process_obj: Process object
    :type process_obj: object
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(fortran.init_module, "finish", lambda: None)
    process_obj.finish()