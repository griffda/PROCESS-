"""pytest config file for all tests.

Defines fixtures that will be shared across all test modules.
"""
import pytest

# Exclude utf_tests from collection until side-effect bugs fixed
collect_ignore_glob = ["uft_tests*"]

def pytest_addoption(parser):
    """Add an overwrite CLI option to pytest.

    :param parser: pytest's CLI arg parser
    :type parser: _pytest.config.argparsing.Parser
    """
    parser.addoption(
        "--overwrite",
        action="store_true",
        default=False,
        help="Overwrite test references"
    )

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