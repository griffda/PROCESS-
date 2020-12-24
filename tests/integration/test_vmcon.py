"""Integration tests for VMCON."""
from process.fortran import vmcon_test
from process.fortran import init_module
import pytest

@pytest.fixture(autouse=True)
def reinit():
    """Re-initialise Fortran module variables before each test is run."""
    init_module.init_all_module_vars()

@pytest.fixture(params=range(1,6))
def vmcon_test_case(request):
    """Fixture to return the VMCON test cases 1-5.

    :param request: request object for parameter access
    :type request: pytest.fixture.SubRequest
    :return: number of test case
    :rtype: int
    """
    return request.param

def test_vmcon(vmcon_test_case):
    """Run the 5 VMCON test scenarios.

    run_vmcon_test returns true or false.
    :param vmcon_test_case: number of the test case to run
    :type vmcon_test_case: int
    """
    assert vmcon_test.run_vmcon_test(vmcon_test_case)