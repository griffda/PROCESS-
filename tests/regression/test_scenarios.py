"""PROCESS regression tests for various scenarios.

Process is run on different scenarios and the observed output is compared to an
expected result. If the observed output is outside a given tolerance, then the
test will fail for that scenario.

James Morris 14/11/2015
CCFE
"""
import pytest
from pytest import approx
import logging
import shutil
from pathlib import Path

from scenario import Scenario

# TODO Split logging into console and file outputs. Further logging 
# customisation?
logger = logging.getLogger(__name__)

def get_scenarios():
    """Generator to yield the scenarios that need to be tested.

    :yield: a single scenario
    :rtype: Scenario
    """
    # TODO Check this is a good way of getting the path
    p = Path.cwd() / 'tests' / 'regression' / 'scenarios'
    # Path object for the scenarios directory (all scenarios dir)

    scenario_dirs = [x for x in p.iterdir() if x.is_dir()]
    # List of scenario directory Path objects (individual scenario dirs)

    # Create a Scenario object for each scenario dir and yield
    for scenario_dir in scenario_dirs:
        scenario = Scenario(scenario_dir)
        yield scenario

@pytest.fixture
def scenarios_run():
    # Setup
    logger.info("Starting scenarios regression run")
    yield

    # Teardown
    logger.info("End of scenarios regression run")
    # TODO Need to log summary result of all tests

@pytest.fixture(params=get_scenarios())
def scenario(scenarios_run, request):
    """Scenario fixture, parameterised with different scenarios.

    :param scenarios_run: fixture for entire scenarios regression run
    :type scenarios_run: Fixture
    :param request: request fixture for access to different scenario parameters
    :type request: object
    :return: single Scenario object
    :rtype: object
    """
    # TODO Check type of scenarios_run
    scenario = request.param
    return scenario

def test_scenario(scenario, tmp_path):
    """Test a scenario in a temporary directory.

    A scenario is an input file and its expected outputs from Process. This
    test function checks that the observed outputs are within a tolerance of the
    expected outputs. This is done by comparing the observed and expected 
    MFILES.

    :param scenario: scenario fixture
    :type scenario: object
    :param tmp_path: temporary path fixture
    :type tmp_path: object
    """
    tolerance = 5e-2
    # The percentage tolerance used when comparing observed and expected values
    # TODO Remove hardcoding: should be a command-line argument

    logger.info(f"Starting test for {scenario.name}")

    # TODO Should only be logged once, not for every test
    logger.info(f"Tolerance set to {tolerance}%")

    # Copy the scenario's reference dir files into the tmp_dir to prevent 
    # modifications
    test_files = scenario.ref_dir.glob("*")
    for test_file in test_files:
        dst = tmp_path / test_file.name
        shutil.copyfile(test_file, dst)

    # Run the scenario: use the scenario method to run Process on the input file
    # in the temporary test directory
    # Assert the run doesn't throw any errors
    assert scenario.run(tmp_path) == True

    # Assert mfile contains something
    assert scenario.check_mfile_length() == True

    # Read in the reference (expected) and new (observed) MFiles
    scenario.read_mfiles()

    # Set the version number based on the MFile output
    # TODO Is this doing anything useful? Logged or output anywhere?
    scenario.set_version()

    # Assert that the ifail value indicates solver success
    assert scenario.check_ifail() == True

    # Compare expected and observed MFiles and yield diff items
    for diff_item in scenario.get_mfile_diffs():
        # TODO Perhaps put this in another func?
        var_name, exp, obs, chg = diff_item
        
        # Compare the expected and observed values for a variable
        # Try/except used to collect all diffs outside tolerance, rather than
        # failing entire test on first AssertionError
        try:
            # Assert with a relative tolerance
            assert exp == approx(obs, rel=tolerance)
            # Within tolerance
            # If different but within tolerance, log
            # If the same, ignore
            if exp != obs:
                logger.info(f"Diff within tolerance: {var_name} was {exp}, now "
                    f"{obs}, ({chg}%)")
        except AssertionError:
            # Outside tolerance: record diff item
            logger.exception(f"Diff outside tolerance: {var_name} was {exp}, "
                f"now {obs}, ({chg}%)")
            scenario.add_diff_item(diff_item)

    # Log summary result of test
    scenario.log_summary()

    # Check no diffs outside the tolerance have been found
    assert len(scenario.get_diff_items()) == 0
            
    # TODO Assert no unique vars found

# TODO Old CLI arguments: how to convert this functionality to pytest?
# Can pass CLI args to pytest...
# parser.add_argument("-d", "--diff", help="Set allowed tolerance between "
#                                          "two files in percentage terms.",
#                                          type=float, default=5.0)

# parser.add_argument("-s", "--save", help="Save outputs to new folders for"
#                     "reference case for this version of PROCESS.",
#                     action="store_true")

# parser.add_argument("--debug", help="Use debugging reference cases (cases "
#                     "beginning with 'error_').", action="store_true")

# parser.add_argument("--overwrite", help="Overwrite reference cases with"
#                     "new output. USE WITH CAUTION.", action="store_true")

# parser.add_argument("-r", "--ref", help="Set reference folder. Default ="
#                     "test_files", type=str, default="test_files")

# parser.add_argument("-u", "--utilities", help="Test utilities only", 
#                     action="store_true")