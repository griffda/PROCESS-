"""pytest config file for all tests.

Defines fixtures that will be shared across all test modules.
"""
import pytest
from system_check import system_compatible
import warnings
import numpy as np

from process.fortran import error_handling as eh

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
        help="Percentage tolerance for regression tests",
    )
    parser.addoption(
        "--overwrite",
        action="store_true",
        default=False,
        help="Overwrite test references",
    )
    parser.addoption(
        "--keep",
        default="",
        type=str,
        help="Define regression tests to copy the MFile back from the temp directory to the test directory as out.MFILE.DAT",
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


@pytest.fixture(scope="session", autouse=True)
def precondition(request):
    """Check a user for an outdated system
    Warn a user if their system is outdated and could have
    regression test floating point issues.

    Exits the test suite if trying to overwrite tests
    to stop inaccurate test assets being written.

    e.g. "pytest --overwrite" returns True here.
    :param request: request fixture to access CLI args
    :type request: SubRequest
    """
    compatible = system_compatible()
    basic_error_message = """
        \u001b[33m\033[1mYou are running the PROCESS test suite on an outdated system.\033[0m
        This can cause floating point rounding errors in regression tests.

        Please see documentation for information on running PROCESS (and tests)
        using a Docker/Singularity container.
        """
    if request.config.getoption("--overwrite") and not compatible:
        pytest.exit(
            basic_error_message
            + "\n \u001b[31m\033[1mTest overwriting is NOT allowed on outdated systems.\033[0m"
        )
    elif not compatible:
        warnings.warn(basic_error_message, UserWarning)


@pytest.fixture
def initialise_error_module(monkeypatch):
    """pytest fixture to initialise error module

    Any routine which can raise an error should initialise
    the error module otherwise segmentation faults can occur.

    This fixture also resets the `fdiags` array to 0's.

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    """
    eh.init_error_handling()
    eh.initialise_error_list()
    # monkeypatch.setattr(eh, 'fdiags', np.zeros(8))
    # monkeypatch.setattr(eh, 'errors_on', False)
