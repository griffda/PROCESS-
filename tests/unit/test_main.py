"""Unit tests for the main.py module."""
import numpy as np
from process import main
from process.main import Process
from process.main import SingleRun
from process.main import VaryRun
from process import fortran
from process import scan
from process import final
from process.utilities.f2py_string_patch import (
    string_to_f2py_compatible,
    f2py_compatible_to_string,
)
import pytest
from pathlib import Path
import argparse

# import numpy as np

# Real input file for testing
# TODO Move to own fixture?
INPUT_FILE = "tracking/baseline_2018/baseline_2018_IN.DAT"
# Convert input file path to absolute and string
input_file_path = str(Path(INPUT_FILE).resolve())


def test_main(monkeypatch):
    """Check that main() can run.

    Call the main function without any arguments.
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # Mock initialisation of the Process object
    monkeypatch.setattr(Process, "__init__", mock_init)
    main.main(args=[])
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
    monkeypatch.setattr(main.Process, "__init__", mock_init)
    # Mock the __init__ method of the Process class with mock_init
    process_obj = Process()
    # Return the mocked Process object
    return process_obj


def test_Process(process_obj):
    """Test that Process objects can be created.

    Check the process_obj fixture can make an object of type Process.
    :param process_obj: Process object
    :type process_obj: object
    """
    assert type(process_obj) is Process


def test_parse_args(process_obj):
    """Test Process.parse_args() method.

    Check the input file path argument is being stored on the Process object.
    :param process_obj: Process object
    :type process_obj: object
    """
    # Run parse args method and check file path is stored
    process_obj.parse_args(args=["-i", input_file_path])
    assert process_obj.args.input == input_file_path


def test_run_mode(process_obj, monkeypatch):
    """Test the Process.run_mode() method.

    Check that VaryRun and SingleRun can be created based on CLI args.
    :param process_obj: Process fixture
    :type process_obj: Process
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # Mock the args attributes for --varyiterparams and --varyiterparamsconfig
    monkeypatch.setattr(process_obj, "args", argparse.Namespace(), raising=False)
    monkeypatch.setattr(process_obj.args, "varyiterparams", True, raising=False)
    monkeypatch.setattr(
        process_obj.args, "varyiterparamsconfig", "file.conf", raising=False
    )

    # Mock VaryRun() (don't want it to actually run), then assert run type is
    # VaryRun
    monkeypatch.setattr(VaryRun, "__init__", mock_init)
    process_obj.run_mode()
    assert type(process_obj.run) == VaryRun

    # Similarly, assert SingleRun when an input file arg is provided
    monkeypatch.setattr(process_obj.args, "varyiterparams", False)
    monkeypatch.setattr(process_obj.args, "input", "aFile", raising=False)
    monkeypatch.setattr(SingleRun, "__init__", mock_init)
    process_obj.run_mode()
    assert type(process_obj.run) == SingleRun


@pytest.fixture
def single_run(monkeypatch):
    """Fixture for a SingleRun object.

    :param monkeypatch: monkeypath fixture
    :type monkeypatch: object
    :return: SingleRun object
    :rtype: SingleRun
    """
    monkeypatch.setattr(SingleRun, "__init__", mock_init)
    single_run = SingleRun()
    single_run.models = None
    return single_run


def test_SingleRun(single_run):
    """Assert SingleRun objects can be created.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    """
    assert type(single_run) is SingleRun


def test_set_input(single_run, monkeypatch):
    """Check the input file validation and setting of path.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    expected = input_file_path
    # Mock the input file path to isolate this test from the other Process
    # methods (don't have to run Process.parse_args() first to set up this way)
    monkeypatch.setattr(single_run, "input_file", input_file_path, raising=False)

    # Mocking undo trys to set the value as none
    # TODO Create our own monkeypatch for strings (if needed)
    # Mock the Fortran set
    # monkeypatch.setattr(fortran.global_variables, "fileprefix", string_to_f2py_compatible(fortran.global_variables.fileprefix,None))
    # fortran.global_variables.test_setting_string()

    # Mocks set up, can now run set_input()
    single_run.set_input()
    # Check path has been set in the Fortran (mocked above)
    result = f2py_compatible_to_string(fortran.global_variables.fileprefix)
    assert result == expected


def test_set_output(single_run, monkeypatch):
    """Check output filename setting in the Fortran.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # Expected output prefix stored in Fortran
    expected = "output_prefix"
    # Mock self.filename_prefix on single_run with the value of expected
    monkeypatch.setattr(single_run, "filename_prefix", expected, raising=False)

    # Mocking undo trys to set the value as none
    # TODO Create our own monkeypatch for strings (if needed)
    # Mock the Fortran set
    # monkeypatch.setattr(fortran.global_variables, "output_prefix", None)
    # Run the method, and extract the value from the Fortran
    single_run.set_output()
    # Convert string from byte-string for comparison
    result = f2py_compatible_to_string(fortran.global_variables.output_prefix)
    assert result == expected


def test_initialise(single_run, monkeypatch):
    """Test that the init_module can be called in the Fortran.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    """
    # Mock the init subroutine with a lambda function
    # Run initialise method; this will fail on a raised exception
    single_run.initialise()


def test_run_hare_tests(single_run, monkeypatch):
    """Check main_module.runtests() is run if run_tests == 1.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # TODO Can't actually check that this is being run yet; need a result that
    # can be checked in the Python (isolated fixtures), to avoid side-effects
    # persisting in the Fortran
    # For now, just check that no exceptions are thrown before calling into
    # the Fortran
    monkeypatch.setattr(fortran.global_variables, "run_tests", 1)
    single_run.run_hare_tests()


def test_kallenbach_tests(single_run, monkeypatch):
    """Check that the Kallenbach tests can be run if required.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # TODO Currently only checking for no exceptions before Fortran mock called
    monkeypatch.setattr(fortran.div_kal_vars, "kallenbach_tests", 1)
    # Expect a SystemExit, as the code is exited if the Kallenbach tests are run
    with pytest.raises(SystemExit):
        single_run.kallenbach_tests()


def test_kallenbach_scan(single_run, monkeypatch):
    """Check the Kallenbach scan can be run.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    monkeypatch.setattr(fortran.div_kal_vars, "kallenbach_scan_switch", 1)
    # Catch a SystemExit after running the scan
    with pytest.raises(SystemExit):
        single_run.kallenbach_scan()


def test_call_solver(single_run, monkeypatch):
    """Attempt to call the hybrd() non-optimising solver.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    # No hybrd() call required
    monkeypatch.setattr(fortran.numerics, "ioptimz", 1)
    single_run.call_solver()

    # Attempt to use hybrd()
    monkeypatch.setattr(fortran.numerics, "ioptimz", -1)
    with pytest.raises(NotImplementedError):
        single_run.call_solver()


def test_set_mfile(single_run, monkeypatch):
    """Check the mfile filename is being stored correctly.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    prefix = "test"
    expected = Path(prefix + "MFILE.DAT")
    # Mock filename_prefix and run
    monkeypatch.setattr(single_run, "filename_prefix", prefix, raising=False)
    single_run.set_mfile()
    assert single_run.mfile_path == expected


def test_show_errors(single_run, monkeypatch):
    """Check that the show errors subroutine is called.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    single_run.show_errors()


def test_finish(single_run, monkeypatch):
    """Check that the finish subroutine is called.

    :param single_run: single_run fixture
    :type single_run: SingleRun
    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    """
    single_run.finish()


@pytest.fixture
def vary_run(monkeypatch):
    """Fixture to return a VaryRun object.

    :param monkeypatch: monkeypatch fixture
    :type monkeypatch: object
    :return: VaryRun object
    :rtype: VaryRun
    """
    monkeypatch.setattr(VaryRun, "__init__", mock_init)
    return VaryRun()


def test_vary_run(vary_run):
    """Assert VaryRun object can be created.

    :param vary_run: vary_run fixture
    :type vary_run: VaryRun
    """
    assert type(vary_run) is VaryRun
