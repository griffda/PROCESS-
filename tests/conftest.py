"""pytest config file for all tests.

Defines fixtures that will be shared across all test modules.
"""
import pytest

def pytest_addoption(parser):
    """Add a regression tolerance CLI option to pytest.

    :param parser: pytest's CLI arg parser
    :type parser: _pytest.config.argparsing.Parser
    """
    parser.addoption(
        "--reg-tolerance",
        default=0.0,
        type=float,
        help="Percentage tolerance for regression tests"
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