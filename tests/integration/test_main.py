"""Integration tests for the main.py module."""
from process import main
from process import fortran
from pathlib import Path
from shutil import copy
import pytest
import os

@pytest.fixture
def temp_data(tmp_path):
    """Copy data dir contents into temp dir for testing.

    Any changes are discarded on fixture teardown.
    :param tmp_path: temporary path fixture
    :type tmp_path: Path
    :return: temporary path containing data files
    :rtype: Path
    """
    data_path = Path(__file__).parent / "data"

    for data_file in data_path.glob("*"):
        dst = tmp_path / data_file.name
        copy(data_file, dst)

    # Return tmp_path, now containing files copied from data dir
    return tmp_path

@pytest.fixture
def temp_data_cwd(temp_data):
    """Change cwd to temp_data dir, then yield it.

    Used when testing command-line args that look for files in the cwd.
    :param temp_data: temporary path containing data files
    :type temp_data: Path
    :yield: temporary path containing data files
    :rtype: Path
    """
    # Setup by changing cwd to temp_data and yielding it
    old_wd = os.getcwd()
    os.chdir(temp_data)
    yield temp_data

    # Teardown by changing back to previous dir
    os.chdir(old_wd)

def test_single_run(temp_data):
    """Test a SingleRun Process run with CLI args.

    This will just check that an exception isn't thrown.
    :param temp_data: temporary dir containing data files
    :type temp_data: Path
    """
    # Set input file path in temp_data dir
    # Baseline 2018 chosen because it's a baseline and runs quickly
    input_path = temp_data / "baseline_2018_IN.DAT"
    input_file = str(input_path.resolve())

    # Run a SingleRun with an explicitly defined IN.DAT
    main.main(args=["-i", input_file])

def test_single_run_cwd(temp_data_cwd):
    """SingleRun without defining an input file.

    Try running without a defined input file (no args). This will look for
    an IN.DAT in the cwd.
    :param temp_data_cwd: temporary data dir, which is also the cwd
    :type temp_data_cwd: Path
    """
    # Copy input file to make a file named "IN.DAT"
    copy(temp_data_cwd / "baseline_2018_IN.DAT", temp_data_cwd / "IN.DAT")
    # Run: args must be emptylist; if None, argparse tries to use CLI args
    main.main(args=[])

def test_vary_run(temp_data):
    """Test a VaryRun with CLI args.

    :param temp_data: temporary dir containing data files
    :type temp_data: Path
    """
    # Set run_process.conf path in temp dir
    # Chosen because it's the only VaryRun in the test suite, and is fast
    conf_path = temp_data / "run_process.conf"
    conf_file = str(conf_path.resolve())

    # Run a VaryRun with an explicit conf file name
    main.main(args=["--varyiterparams", "--varyiterparamsconfig", conf_file])

def test_vary_run_cwd(temp_data_cwd):
    """Test VaryRun without explicitly defining the conf file name.

    This will look for a run_process.conf in the cwd.
    :param temp_data_cwd: temporary data dir, which is also the cwd
    :type temp_data_cwd: Path
    """
    main.main(args=["--varyiterparams"])