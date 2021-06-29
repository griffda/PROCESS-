"""Integration tests for VMCON.

Cases 1 to 3 are recommended, others are included to probe the code's
behaviour with different initial guesses for the solution vector x
Expected answers for tests 1 to 3 are given in
VMCON documentation ANL-80-64
"""
from process.fortran import init_module
from process.fortran import error_handling
from process.vmcon import Vmcon
import pytest
import numpy as np
import logging
from abc import ABC, abstractmethod

# Debug-level terminal output logging
logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.DEBUG)
logger.addHandler(s_handler)

# Provide helpful errors in the event of a failed Vmcon test
error_handling.initialise_error_list()

@pytest.fixture(autouse=True)
def reinit():
    """Re-initialise Fortran module variables before each test is run."""
    init_module.init_all_module_vars()

class Case():
    """A Vmcon test case."""
    def __init__(self, name, vmcon):
        """Initialise name, Vmcon and expected result objects.

        :param name: name of test case
        :type name: str
        :param vmcon: custom Vmcon object for this test
        :type vmcon: Vmcon
        """
        self.name = name
        self.vmcon = vmcon
        self.exp = ExpectedResult()

    def check_result(self):
        """Assert observed results from Vmcon run equal expected results."""
        # Assert ifail is expected value (converged: ifail == 1)
        assert self.vmcon.ifail == self.exp.ifail

        # Assert final objective function value
        assert self.vmcon.objf == pytest.approx(self.exp.objf)

        # Final solution estimate
        for i in range(self.vmcon.n):
            assert self.vmcon.x[i] == pytest.approx(self.exp.x[i])

class ExpectedResult():
    """Expected result class for comparing an observed Vmcon result against."""
    def __init__(self):
        """Initialise expected attributes."""
        self.x = []
        self.c = []
        self.vlam = []
        self.objf = None
        self.errlg = 0.0
        self.errlm = 0.0
        self.errcom = 0.0
        self.errcon = 0.0
        self.ifail = 1

class VmconTest(ABC, Vmcon):
    """Testing class for Vmcon.

    :param ABC: abstract base class
    :type ABC: abc.ABC
    :param Vmcon: Vmcon class
    :type Vmcon: Vmcon
    """
    def __init__(self):
        """Initialise attributes common to Vmcon test sub-classes."""
        # Instantiate Vmcon
        super().__init__()
        
        # No bounds on x values set
        self.ilower[0:2] = 0.0
        self.iupper[0:2] = 0.0
        self.bndl[:] = 0.0
        self.bndu[:] = 0.0

        self.xtol = 1.0e-8

    @abstractmethod
    def fcnvmc1(self):
        """Function evaluator."""
        pass

    @abstractmethod
    def fcnvmc2(self):
        """Gradient function evaluator."""
        pass

class Vmcon1(VmconTest):
    """Override fcnvmc1 and 2 methods for test case 1.

    This allows a test to be run using custom function and gradient function
    evaluators specific to this test.
    
    Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
    subject to the following constraints:
    c1(x1,x2) = x1 - 2*x2 + 1 = 0 
    c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
    :param VmconTest: testing class for Vmcon
    :type VmconTest: VmconTest
    """
    def fcnvmc1(self):
        """Function evaluator."""
        self.objf = (self.x[0] - 2.0)**2 + (self.x[1] - 1.0)**2
        self.conf[0] = self.x[0] - 2.0 * self.x[1] + 1.0
        self.conf[1] = -0.25 * self.x[0]**2 - self.x[1] * self.x[1] + 1.0

    def fcnvmc2(self):
        """Gradient function evaluator."""
        self.fgrd[0] = 2.0 * (self.x[0] - 2.0)
        self.fgrd[1] = 2.0 * (self.x[1] - 1.0)

        self.cnorm[0, 0] = 1.0
        self.cnorm[1, 0] = -2.0
        self.cnorm[0, 1] = -0.5 * self.x[0]
        self.cnorm[1, 1] = -2.0 * self.x[1]

class Vmcon2(VmconTest):
    """Override fcnvmc1 and 2 methods for test case 2.
        
    Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
    subject to the following constraints:
    c1(x1,x2) = x1 - 2*x2 + 1 >= 0
    c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
    :param VmconTest: testing class for Vmcon
    :type VmconTest: VmconTest
    """
    def fcnvmc1(self):
        """Function evaluator."""
        self.objf = (self.x[0] - 2.0)**2 + (self.x[1] - 1.0)**2
        self.conf[0] = self.x[0] - 2.0 * self.x[1] + 1.0
        self.conf[1] = -0.25 * self.x[0]**2 - self.x[1] * self.x[1] + 1.0
    
    def fcnvmc2(self):
        """Gradient function evaluator."""
        self.fgrd[0] = 2.0 * (self.x[0] - 2.0)
        self.fgrd[1] = 2.0 * (self.x[1] - 1.0)

        self.cnorm[0, 0] = 1.0
        self.cnorm[1, 0] = -2.0
        self.cnorm[0, 1] = -0.5 * self.x[0]
        self.cnorm[1, 1] = -2.0 * self.x[1]

class Vmcon3(VmconTest):
    """Override fcnvmc1 and 2 methods for test case 3.
        
    Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
    subject to the following constraints:
    c1(x1,x2) = x1 + x2 - 3 = 0
    c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

    :param VmconTest: testing class for Vmcon
    :type VmconTest: VmconTest
    """
    def fcnvmc1(self):
        """Function evaluator."""
        self.objf = (self.x[0] - 2.0)**2 + (self.x[1] - 1.0)**2
        self.conf[0] = self.x[0] + self.x[1] - 3.0
        self.conf[1] = -0.25 * self.x[0]**2 - self.x[1] * self.x[1] + 1.0

    def fcnvmc2(self):
        """Gradient function evaluator."""
        self.fgrd[0] = 2.0
        self.fgrd[1] = 2.0 * (self.x[1] - 1.0)

        self.cnorm[0, 0] = 1.0
        self.cnorm[1, 0] = 1.0
        self.cnorm[0, 1] = -0.5 * self.x[0]
        self.cnorm[1, 1] = -2.0 * self.x[1]

class Vmcon4(VmconTest):
    """Override fcnvmc1 and 2 methods for test case 4.

    From Wikipedia: Lagrange Multiplier article
    Maximise f(x1,x2) = x1 + x2
    subject to the following constraint:
    c1(x1,x2) = x1**2 + x2**2 - 1 = 0

    :param VmconTest: testing class for Vmcon
    :type VmconTest: VmconTest
    """
    def fcnvmc1(self):
        """Function evaluator."""
        self.objf = self.x[0] + self.x[1]
        self.conf[0] = self.x[0] * self.x[0] + self.x[1] * self.x[1] - 1.0

    def fcnvmc2(self):
        """Gradient function evaluator."""
        self.fgrd[0] = 1.0
        self.fgrd[1] = 1.0
        self.cnorm[0, 0] = 2.0 * self.x[0]
        self.cnorm[1, 0] = 2.0 * self.x[1]

class Vmcon5(VmconTest):
    """Override fcnvmc1 and 2 methods for test case 5.

    :param VmconTest: testing class for Vmcon
    :type VmconTest: VmconTest
    """
    def fcnvmc1(self):
        """Function evaluator."""
        self.objf = self.x[0]**2
        self.conf[0] = self.x[0]**2 - 2.0 * self.x[0] - 3.0

    def fcnvmc2(self):
        """Gradient function evaluator."""
        self.fgrd[0] = 2.0 * self.x[0]
        self.cnorm[0, 0] = 2.0 * self.x[0] - 2.0

def get_case1():
    """Create test case 1 for Vmcon.
    
    Set up vmcon for the run and define the expected result.

    Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
    subject to the following constraints:
    c1(x1,x2) = x1 - 2*x2 + 1 = 0
    c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
    
    VMCON documentation ANL-80-64
    """
    # Create a case-specific Vmcon object with overridden fcnvmc1 and 2
    case = Case("1", Vmcon1())
    
    # Set up vmcon values for this case
    neqns = 1
    nineqns = 1
    case.vmcon.n = 2
    case.vmcon.m = neqns + nineqns
    case.vmcon.meq = neqns
    case.vmcon.x[0:2] = 2.0e0

    # Expected values
    case.exp.x = np.array([8.228756e-1, 9.114378e-1])
    case.exp.objf = 1.393464
    case.exp.c = np.array([1.387778e-17, -7.671641e-13])
    case.exp.vlam = np.array([-1.594491, 1.846591])
    case.exp.errlg = 3.345088e-12
    case.exp.errcom = 1.416660e-12
    case.exp.errcon = 7.671779e-13

    return case

def get_case2():
    """Create test case 2 for Vmcon.

    Set up vmcon for the run and define the expected result.
    
    Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
    subject to the following constraints:
    c1(x1,x2) = x1 - 2*x2 + 1 >= 0
    c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
    
    VMCON documentation ANL-80-64
    """
    case = Case("2", Vmcon2())

    # Vmcon values for this case
    neqns = 0
    nineqns = 2
    case.vmcon.n = 2
    case.vmcon.m = neqns + nineqns
    case.vmcon.meq = neqns
    case.vmcon.x[0:2] = 2.0e0

    # Expected values
    case.exp.x = np.array([1.664968, 5.540486e-1])
    case.exp.objf = 3.111186e-1
    case.exp.c = np.array([1.556871, -1.021405e-14])
    case.exp.vlam = np.array([0.0, 8.048955e-1])
    case.exp.errlg = 2.343333e-11
    case.exp.errcom = 8.221245e-15
    case.exp.errcon = 1.021405e-14

    return case

def get_case3():
    """Create test case 3 for Vmcon.
    
    Set up vmcon for the run and define the expected result.

    Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
    subject to the following constraints:
    c1(x1,x2) = x1 + x2 - 3 = 0
    c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

    Note that this test is supposed to fail with ifail=5
    as there is no feasible solution
    VMCON documentation ANL-80-64
    """
    # Create a case-specific Vmcon object with overridden fcnvmc1 and 2
    case = Case("3", Vmcon3())
    
    # Set up vmcon values for this case
    neqns = 1
    nineqns = 1
    case.vmcon.n = 2
    case.vmcon.m = neqns + nineqns
    case.vmcon.meq = neqns
    case.vmcon.x[0:2] = 2.0e0
    
    # Expected values
    case.exp.x = np.array([2.0, 2.0])
    case.exp.objf = 6.203295e-1
    case.exp.c = np.array([-6.6613381477509392e-16, -8.000000000004035e-01])
    case.exp.vlam = np.array([0.0, 0.0])
    case.exp.errlg = 1.599997724349894
    case.exp.errcon = 8.0000000000040417e-01
    case.exp.ifail = 5

    return case

def get_case4():
        """Create test case 4 for Vmcon.

        Set up vmcon for the run and define the expected result.
        
        Maximise f(x1,x2) = x1 + x2
        subject to the following constraint:
        c1(x1,x2) = x1**2 + x2**2 - 1 = 0
        
        http://en.wikipedia.org/wiki/Lagrange_multiplier
        """
        # Create a case-specific Vmcon object with overridden fcnvmc1 and 2
        case = Case("4", Vmcon4())

        # Set up vmcon values for this case
        neqns = 1
        nineqns = 0
        case.vmcon.n = 2
        case.vmcon.m = neqns + nineqns
        case.vmcon.meq = neqns
        case.vmcon.xtol = 2.0e-8
        case.vmcon.x[0:2] = 1.0e0
        # N.B. results can flip to minimum instead of maximum
        # if x(1), x(2) are initialised at different points...

        # Expected values
        case.exp.x = np.array([0.5 * 2.0**(1/2), 0.5 * 2.0**(1/2)])
        case.exp.objf = 2.0**(1/2)
        case.exp.c = np.array([0.0])
        case.exp.vlam = np.array([1.0 * 2.0**(1/2)])

        return case

def get_case5():
        """Create test case 5 for Vmcon.

        Set up vmcon for the run and define the expected result.

        Intersection of parabola x^2 with straight line 2x+3
        Unorthodox (and not recommended) method to find the root
        of an equation.
        
        Maximise f(x1) = x1**2
        subject to the following constraint:
        c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0

        Solutions to c1(x1) are x1 = -1 and x1 = 3, and depending on
        the initial guess for x1 either (or neither...) solution might
        be found. Since there is one constraint equation with one unknown
        the code cannot optimise properly.
        """
        # Create a case-specific Vmcon object with overridden fcnvmc1 and 2
        case = Case("5", Vmcon5())

        # Set up vmcon values for this case
        neqns = 1
        nineqns = 0
        case.vmcon.n = 1
        case.vmcon.m = neqns + nineqns
        case.vmcon.meq = neqns
        case.vmcon.x[0] = 5.0e0 # Try different values, e.g. 5.0, 2.0, 1.0, 0.0...

        # Expected values
        case.exp.x = np.array([3.0])
        case.exp.objf = 9.0
        case.exp.c = np.array([0.0])
        case.exp.vlam = np.array([1.5])

        return case

def get_case_fns():
    """Create a list of test case getter functions to run.

    :return: list of functions to return individual test cases
    :rtype: list
    """
    case_fns = [
        get_case1,
        get_case2,
        get_case3,
        get_case4,
        get_case5
    ]

    return case_fns

@pytest.fixture(params=get_case_fns())
def case(request):
    """Parameterised fixture for providing Vmcon test cases to run.

    :param request: provides access to various test case functions
    :type request: SubRequest
    :return: Vmcon scenario to run and its expected result
    :rtype: test_vmcon.Case
    """
    case_fn = request.param
    return case_fn()

def test_vmcon(case):
    """Integration test for Vmcon.

    :param case: a Vmcon scenario and its expected result
    :type case: test_vmcon.Case
    """
    logger.debug("Initial solution estimate:")
    for i in range(case.vmcon.n):
       logger.debug(f"x[{i}] = {case.vmcon.x[i]}")

    # Run Vmcon for this case
    case.vmcon.run()

    # Assert result
    try:
        case.check_result()
        logger.info(f"Vmcon test {case.name} passed")
    except AssertionError:
        logger.exception(f"Vmcon test {case.name} failed")
        log_failure(case)
        raise

def log_failure(case):
    """Write extra logs in the case of a test case failure.

    :param case: a failing test case
    :type case: Case
    """
    logger.debug(f"ifail = {case.vmcon.ifail} (expected value = {case.exp.ifail}")
    logger.debug(f"Number of function evaluations = {case.vmcon.fcnvmc1_calls}")
    
    logger.debug("Final solution estimate: calculated vs expected")
    for i in range (case.vmcon.n):
        logger.debug(f"x[{i}] = {case.vmcon.x[i]}, {case.exp.x[i]}")
    
    logger.debug(f"Final objective function value: calculated vs expected: "
        f"{case.vmcon.objf}, {case.exp.objf}")
    
    logger.debug("Constraints evaluated at x: calculated vs expected':")
    for i in range(case.vmcon.m):
        logger.debug(f"{case.vmcon.conf[i]}, {case.exp.c[i]}")

    logger.debug("Lagrange multiplier estimates: calculated vs expected")
    for i in range(case.vmcon.m):
        logger.debug(f"{case.vmcon.vlam[i]}, {case.exp.vlam[i]}")

    logger.debug("Lagrangian gradient error: calculated vs expected")
    errlg = 0.0
    for i in range(case.vmcon.n):
        summ = case.vmcon.fgrd[i]
        for j in range(case.vmcon.m):
            summ = summ - case.vmcon.vlam[j]*case.vmcon.cnorm[i, j]
        errlg = errlg + abs(summ)
    logger.debug(f"{errlg}, {case.exp.errlg}")

    logger.debug("Lagrange multiplier error: calculated vs expected")
    errlm = 0.0
    for i in range(case.vmcon.m):
        if (i <= case.vmcon.meq) or (case.vmcon.vlam[i] >= 0.0):
            continue
        errlm = errlm + abs(case.vmcon.vlam[i])
    logger.debug(f"{errlm}, {case.exp.errlm}")

    logger.debug("Complementarity error: calculated vs expected")
    errcom = 0.0
    for i in range(case.vmcon.m):
        errcom = errcom + abs(case.vmcon.vlam[i] * case.vmcon.conf[i])
    logger.debug(f"{errcom}, {case.exp.errcom}")

    logger.debug("Constraint error: calculated vs expected")
    errcon = 0.0
    for i in range(case.vmcon.m):
        if (i > case.vmcon.meq) and (case.vmcon.conf[i] >= 0.0):
            continue
        errcon = errcon + abs(case.vmcon.conf[i])
    logger.debug(f"{errcon}, {case.exp.errcon}")