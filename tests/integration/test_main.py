"""Integration tests for the main.py module."""
from process import main
from shutil import copy

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