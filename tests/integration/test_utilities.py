"""Integration tests for utilities.

These tests check the utilities that PROCESS uses, mainly for file IO.
"""
import pytest
import logging
from pathlib import Path
import process.io.mfile as mf
import process.io.in_dat as indat
import process.io.plot_proc as pp

# Set up logging
# Set level to debug so all logs are passed to handlers
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)

# Create handlers for console and file output
s_handler = logging.StreamHandler()
f_handler = logging.FileHandler('tests/integration/utilities.log', mode='w')
s_handler.setLevel(logging.INFO)
f_handler.setLevel(logging.DEBUG)
logger.addHandler(s_handler)
logger.addHandler(f_handler)

logger.info("Running utilities integration tests")

# TODO More utilities tests to be implemented
# test_make_plot_dat
# test_convert_in_dat

def get_scenario_paths():
    """Get the paths to the regression scenario directories.

    The regression scenario IN.DATs and MFILE.DATs are used for these 
    integration tests.
    :return: Path objects for the scenario dirs
    :rtype: list
    """
    scenarios_path = Path(__file__).parent.parent / "regression" / "scenarios"
    scenarios_paths = [path for path in scenarios_path.iterdir()]
    return scenarios_paths

@pytest.fixture(params=get_scenario_paths())
def scenario_path(request):
    """Get the path to a regression scenario directory.

    Parameterised with all scenario directories, this will return paths for
    all scenario directories.
    :return: Path to a regression scenario dir
    :rtype: Path
    """
    scenario_path = request.param
    return scenario_path

@pytest.fixture
def mfile_path(scenario_path):
    """Create a path to a scenario's MFile.

    :param scenario_path: Path to a scenario dir
    :type scenario_path: Path
    :return: Path to that scenario's MFile
    :rtype: Path
    """
    mfile_path = scenario_path / "ref.MFILE.DAT"
    return mfile_path

@pytest.fixture
def input_file_path(scenario_path):
    """Create a path to a scenario's input file.

    :param scenario_path: Path to a scenario dir
    :type scenario_path: Path
    :return: Path to that scenario's IN.DAT
    :rtype: Path
    """
    input_file_path = scenario_path / "IN.DAT"
    return input_file_path

def test_mfile_lib(mfile_path):
    """Test the PROCESS mfile library.
    
    :param mfile_path: Path to the scenario's MFile
    :type mfile_path: Path
    """
    logger.info("Testing mfile.py")

    # Test MFile for this scenario
    # This try/except is not necessary, but allows additional logging to be 
    # added for clarity in addition to pytest's own logging
    try:
        assert mf.test(str(mfile_path)) == True
        # mf.test returns True on success
    except AssertionError:
        logger.exception(f"mfile test for {mfile_path.name} has failed")
        raise

def test_in_dat_lib(input_file_path):
    """Test the PROCESS in_dat library.

    :param input_file_path: Path to a scenario's input file
    :type input_file_path: Path
    """
    logger.info("Testing in_dat")

    # Test MFile for this scenario
    try:
        assert indat.test(str(input_file_path)) == True
    except AssertionError:
        logger.error(f"in_dat test for {input_file_path.name} has failed")
        raise

def test_plot_proc(mfile_path):
    """Test the PROCESS plot_proc script.

    :param mfile_path: Path to the scenario's MFile
    :type mfile_path: Path
    """
    logger.info("Testing plot_proc.py")

    # Test plot_proc on an MFile
    try:
        assert pp.test(str(mfile_path)) == True
    except AssertionError:
        logger.exception(f"plot_proc test for {mfile_path.name} has failed")
        raise