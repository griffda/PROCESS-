"""pytest config file for all tests.

Defines fixtures that will be shared across all test modules.
"""
import pytest

# Exclude utf_tests from collection until side-effect bugs fixed
collect_ignore_glob = ["uft_tests*"]

def pytest_addoption(parser):
    """Add custom CLI options to pytest.
    
    Add regression tolerance and overwrite options to pytest.
    :param parser: pytest's CLI arg parser
    :type parser: _pytest.config.argparsing.Parser
    """
    parser.addoption(
        "--reg-tolerance",
        default=0.0,
        type=float,
        help="Percentage tolerance for regression tests"
    )
    parser.addoption(
        "--overwrite",
        action="store_true",
        default=False,
        help="Overwrite test references"
    )

@pytest.fixture
def reg_tolerance(request):
    """Return the value of the --reg-tolerance CLI option.

    Gets the percentage tolerance to use in regression tests.
    e.g. "pytest --reg-tolerance=5" returns "5" here.
    :param request: request fixture to access CLI args
    :type request: SubRequest
    :return: value of --reg-tolerance option
    :rtype: float
    """
    return request.config.getoption("--reg-tolerance")

@pytest.fixture
def overwrite_refs_opt(request):
    """Return the value of the --overwrite CLI option.

    e.g. "pytest --overwrite" returns True here.
    :param request: request fixture to access CLI args
    :type request: SubRequest
    :return: True if --overwrite option present
    :rtype: bool
    """
    return request.config.getoption("--overwrite")
