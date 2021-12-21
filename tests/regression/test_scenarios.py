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
import math

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
    scenario_dirs.sort(key=lambda scenario_dir: scenario_dir.name.lower())
    # Sorted list of scenario directory Path objects (individual scenario dirs)
    # Maintains order scenarios are run in

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

def get_scenario_id(scenario):
    """Return the name of the scenario.

    Used for getting the IDs for a fixture parameterised with scenarios.
    :param scenario: Scenario object parameterising a fixture
    :type scenario: scenario.Scenario
    :return: scenario name
    :rtype: str
    """
    return scenario.name

@pytest.fixture(params=get_scenarios(), ids=get_scenario_id)
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

def test_scenario(scenario, tmp_path, reg_tolerance, overwrite_refs_opt):
    """Test a scenario in a temporary directory.

    A scenario is an input file and its expected outputs from Process. This
    test function checks that the observed outputs are within a tolerance of the
    expected outputs. This is done by comparing the observed and expected 
    MFILES.

    :param scenario: scenario fixture
    :type scenario: object
    :param tmp_path: temporary path fixture
    :type tmp_path: object
    :param reg_tolerance: percentage tolerance when comparing observed and 
    expected values
    :type reg_tolerance: float
    :param overwrite_refs_opt: option to overwrite reference MFILE and OUT.DAT
    :type tmp_path: bool
    """
    # hybrd() has been temporarily commented out. Please see the comment in
    # function_evaluator.fcnhyb() for an explanation.
    # TODO Re-implement the IFE test using vmcon
    if scenario.name == "IFE":
        pytest.skip("IFE currently uses the hybrd non-optimising solver, which "
            "is currently not implemented"
        )

    logger.info(f"Starting test for {scenario.name}")

    # TODO Should only be logged once, not for every test
    logger.info(f"Tolerance set to {reg_tolerance}%")

    # Copy the scenario's reference dir files into the tmp_dir to prevent 
    # modifications
    test_files = scenario.ref_dir.glob("*")
    for test_file in test_files:
        dst = tmp_path / test_file.name
        shutil.copyfile(test_file, dst)

    # Run the scenario: use the scenario method to run Process on the input file
    # in the temporary test directory
    scenario.run(tmp_path)
    
    # Overwrite reference MFILE and OUT files (ref.MFILE.DAT and ref.OUT.DAT)
    # If overwriting refs, don't bother asserting anything else and return
    if overwrite_refs_opt:
        logger.info(
            f"Overwriting reference MFILE.DAT and OUT.DAT for {scenario.name}"
        )
        scenario.overwrite_ref_files()
        return

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
            if math.isnan(exp) and math.isnan(obs):
                # expected and observed value is NaN: warn, but don't fail
                logger.warning(f"{var_name} is NaN")
            else:
                # Assert with a relative tolerance
                assert exp == approx(obs, rel=reg_tolerance)
                
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
# parser.add_argument("-s", "--save", help="Save outputs to new folders for"
#                     "reference case for this version of PROCESS.",
#                     action="store_true")

# parser.add_argument("--debug", help="Use debugging reference cases (cases "
#                     "beginning with 'error_').", action="store_true")

# parser.add_argument("-r", "--ref", help="Set reference folder. Default ="
#                     "test_files", type=str, default="test_files")

# parser.add_argument("-u", "--utilities", help="Test utilities only", 
#                     action="store_true")