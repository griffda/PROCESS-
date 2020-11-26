"""Integration tests for the main.py module."""
from process import main
from process import fortran
from pathlib import Path
from shutil import copy
import pytest
import os

def test_single_run(tmp_path):
    """Test a SingleRun Process run with CLI args."""
    # TODO Actually check that the run succeeds; will only be able to do this
    # when the interface moves a little deeper into the Fortran. Access to ifail
    # is what is wanted. Can't assert anything yet, so for now call into the 
    # Fortran and check that an exception isn't thrown on the way.
    
    # Single run dir path
    # Chosen because it's a baseline and runs quickly
    # TODO This path is silly: find a better way of doing this
    single_dir = Path(__file__).parent.parent.parent / "tracking/baseline_2018"
    
    # Copy files to temp dir so output files are discarded
    for test_file in single_dir.glob("*"):
        dst = tmp_path / test_file.name
        copy(test_file, dst)

    # IN.DAT path in temp dir
    input_path = tmp_path / "baseline_2018_IN.DAT"
    input_file = str(input_path.resolve())

    # Run a SingleRun with an explicitly defined IN.DAT
    main.main(args=["-i", input_file])

    # Try running without a defined input file (no args). This will look for
    # an IN.DAT in the cwd. Copy to make a file named "IN.DAT", then cd to the
    # test dir beforehand
    copy(input_path, tmp_path / "IN.DAT")
    old_wd = os.getcwd()
    os.chdir(tmp_path)
    # args must be emptylist; if None, argparse tries to use CLI args
    main.main(args=[])
    os.chdir(old_wd)

def test_vary_run(tmp_path):
    """Test a VaryRun with CLI args.

    :param tmp_path: temporary dir path fixture
    :type tmp_path: Path
    """
    # Vary run dir path
    # Chosen because it's the only VaryRun in the test suite, and is fast
    vary_dir = Path(__file__).parent.parent / "regression/scenarios/starfire"

    # Copy all files to temp dir so output files are discarded
    for test_file in vary_dir.glob("*"):
        dst = tmp_path / test_file.name
        copy(test_file, dst)

    # run_process.conf path in temp dir
    conf_path = tmp_path / "run_process.conf"
    conf_file = str(conf_path.resolve())

    # Run a VaryRun with a custom conf file name
    main.main(args=["--varyiterparams", "--varyiterparamsconfig", conf_file])

    # Test VaryRun without explicitly defining the conf file name
    # This will look for a run_process.conf in the cwd
    old_wd = os.getcwd()
    os.chdir(tmp_path)
    main.main(args=["--varyiterparams"])
    os.chdir(old_wd)