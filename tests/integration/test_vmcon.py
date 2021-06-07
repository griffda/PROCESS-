"""Integration tests for VMCON."""
from process.fortran import init_module
from process.fortran import error_handling
from process.vmcon import Vmcon
import pytest
import numpy as np
import logging

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

# TODO vmcon_test commented out pending conversion to Python. vmcon_test
# depends on passing subroutine arguments to vmcon, which cannot be done now
# that vmcon is called from Python in optimiser.py (without subroutine args).
# Once a Python Vmcon class is created, these custom test subroutines (objfn
# and dobjfn) will be able to be called again, and this test reinstated in a
# partially Python form.

#   integer :: nfun ! function call counter
#   integer :: itest

#   !  Choose test to run by changing itest in main program below.
#   !  1 to 3 are recommended, others are included to probe the code's
#   !  behaviour with different initial guesses for the solution vector x
#   !  Expected answers for tests 1 to 3 are given in
#   !  VMCON documentation ANL-80-64

#   real(dp), dimension(2) :: x_exp
#   real(dp), dimension(2) :: c_exp, vlam_exp
#   real(dp) :: objf_exp, errlg_exp, errlm_exp
#   real(dp) :: errcom_exp, errcon_exp
#   integer :: ifail_exp
#   integer, parameter :: maxcal = 100
#   integer :: neqns
#   integer :: nfev2     ! number of calls to FCNVMC1 (VMCON function caller) made
#   integer :: nineqns   !  number of inequality constraints VMCON must satisfy
#   integer :: nvar      !  number of iteration variables to use
#   integer :: nviter    !  number of VMCON iterations performed 
#   integer :: ipvlam
#   integer, parameter :: ipnvars = 175   ! Local variable
#   integer, parameter :: ipeqns = 87     ! Local variable

# contains

#   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#   subroutine init_vmcon_test
#     !! Initialise module variables
#     implicit none

#     nfun = 0
#     itest = 1
#     neqns = 0
#     nfev2 = 0
#     nineqns = 0
#     nvar = 0
#     nviter = 0
#     ipvlam = 0
#   end subroutine init_vmcon_test

#   subroutine inittest(nvar,neqns,nineqns,x,ilower,iupper,bndl,bndu)
#     case (3)

#         !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
#         !  subject to the following constraints:
#         !  c1(x1,x2) = x1 + x2 - 3 = 0
#         !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
#         !
#         !  Note that this test is supposed to fail with ifail=5
#         !  as there is no feasible solution
#         !
#         !  VMCON documentation ANL-80-64

#         nvar = 2
#         neqns = 1
#         nineqns = 1
#         x(1) = 2.0D0 ; x(2) = 2.0D0

#         !  No bounds on x values set
#         ilower(1) = 0 ; ilower(2) = 0
#         iupper(1) = 0 ; iupper(2) = 0
#         bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

#         x_exp(1) = 2.3999994310874733D0
#         x_exp(2) = 6.0000056891252611D-01
#         objf_exp = 3.1999908974060504D-01
#         c_exp(1) = -6.6613381477509392D-16
#         c_exp(2) = -8.000000000004035D-01
#         vlam_exp(1) = 0.0D0
#         vlam_exp(2) = 0.0D0
#         errlg_exp = 1.599997724349894D0
#         errlm_exp = 0.0D0
#         errcom_exp = 0.0D0
#         errcon_exp = 8.0000000000040417D-01
#         ifail_exp = 5

#     case (4)

#         !  Maximise f(x1,x2) = x1 + x2
#         !  subject to the following constraint:
#         !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0
#         !
#         !  http://en.wikipedia.org/wiki/Lagrange_multiplier

#         nvar = 2
#         neqns = 1
#         nineqns = 0

#         !  N.B. results can flip to minimum instead of maximum
#         !  if x(1), x(2) are initialised at different points...
#         x(1) = 1.0D0 ; x(2) = 1.0D0

#         !  No bounds on x values set
#         ilower(1) = 0 ; ilower(2) = 0
#         iupper(1) = 0 ; iupper(2) = 0
#         bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

#         x_exp(1) = 0.5D0*sqrt(2.0D0)
#         x_exp(2) = 0.5D0*sqrt(2.0D0)
#         objf_exp = sqrt(2.0D0)
#         c_exp(1) = 0.0D0
#         vlam_exp(1) = 1.0D0/sqrt(2.0D0)
#         errlg_exp = 0.0D0
#         errlm_exp = 0.0D0
#         errcom_exp = 0.0D0
#         errcon_exp = 0.0D0
#         ifail_exp = 1

#     case (5)

#         !  Intersection of parabola x^2 with straight line 2x+3
#         !  Unorthodox (and not recommended) method to find the root
#         !  of an equation.
#         !
#         !  Maximise f(x1) = x1**2
#         !  subject to the following constraint:
#         !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0
#         !
#         !  Solutions to c1(x1) are x1 = -1 and x1 = 3, and depending on
#         !  the initial guess for x1 either (or neither...) solution might
#         !  be found. Since there is one constraint equation with one unknown
#         !  the code cannot optimise properly.

#         nvar = 1
#         neqns = 1
#         nineqns = 0

#         x(1) = 5.0D0  !  Try different values, e.g. 5.0, 2.0, 1.0, 0.0...

#         !  No bounds on x values set
#         ilower(1) = 0
#         iupper(1) = 0
#         bndl(:) = 0.0D0 ; bndu(:) = 0.0D0

#         x_exp(1) = 3.0
#         objf_exp = 9.0D0
#         c_exp(1) = 0.0D0
#         vlam_exp(1) = 1.5D0
#         errlg_exp = 0.0D0
#         errlm_exp = 0.0D0
#         errcom_exp = 0.0D0
#         errcon_exp = 0.0D0
#         ifail_exp = 1

#     end select

#   end subroutine inittest

#   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#   subroutine objfn(n,m,x,objf,conf, ifail)
#       ! variable itest cannot be passed here as the argument format is fixed by VMCON.
#       ! itest is a module-level variable.

#     implicit none
#     integer, intent(in) :: n,m
#     real(dp), dimension(n), intent(in) :: x
#     real(dp), intent(out) :: objf
#     real(dp), dimension(m), intent(out) :: conf
#     integer, intent(inout) :: ifail
#     ! ifail is not used here, but is required to avoid an interface mismatch
#     ! when calling vmcon with objfn as an argument
#     select case (itest)

#     case (3)

#         !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
#         !  subject to the following constraints:
#         !  c1(x1,x2) = x1 + x2 - 3 = 0
#         !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

#         objf = (x(1) - 2.0D0)**2 + (x(2) - 1.0D0)**2

#         conf(1) = x(1) + x(2) - 3.0D0
#         conf(2) = -0.25D0*x(1)**2 - x(2)*x(2) + 1.0D0

#     case (4)

#         !  From Wikipedia: Lagrange Multiplier article
#         !  Maximise f(x1,x2) = x1 + x2
#         !  subject to the following constraint:
#         !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0

#         objf = x(1) + x(2)

#         conf(1) = x(1)*x(1) + x(2)*x(2) - 1.0D0

#     case (5)

#         !  Intersection of parabola x^2 with straight line 2x+3
#         !  Maximise f(x1) = x1**2
#         !  subject to the following constraint:
#         !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0

#         objf = x(1)*x(1)

#         conf(1) = x(1)*x(1) - 2.0D0*x(1) - 3.0D0

#     end select

#     nfun = nfun + 1

#   end subroutine objfn

#   ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#   subroutine dobjfn(n,m,x,fgrd,cnorm,lcnorm,ifail)

#     implicit none

#     !  Arguments

#     integer, intent(in) :: n,m,lcnorm
#     real(dp), dimension(n), intent(in) :: x
#     real(dp), dimension(n), intent(out) :: fgrd
#     real(dp), dimension(lcnorm,m), intent(out) :: cnorm
#     integer, intent(inout) :: ifail
#     ifail = 0

#     ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#     !  fgrd(1...n) are the gradients of the objective function f
#     !  with respect to x1...xn

#     !  cnorm(1...n, 1...m) are the gradients of the constraints 1...m
#     !  with respect to x1...xn

#     select case (itest)

#     case (3)
#         !  Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
#         !  subject to the following constraints:
#         !  c1(x1,x2) = x1 + x2 - 3 = 0
#         !  c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0

#         fgrd(1) = 2.0D0
#         fgrd(2) = 2.0D0*(x(2) - 1.0D0)

#         cnorm(1,1) = 1.0D0
#         cnorm(2,1) = 1.0D0
#         cnorm(1,2) = -0.5D0*x(1)
#         cnorm(2,2) = -2.0D0*x(2)

#     case (4)
#         !  From Wikipedia: Lagrange Multiplier article
#         !  Maximise f(x1,x2) = x1 + x2
#         !  subject to the following constraint:
#         !  c1(x1,x2) = x1**2 + x2**2 - 1 = 0

#         fgrd(1) = 1.0D0
#         fgrd(2) = 1.0D0

#         cnorm(1,1) = 2.0D0*x(1)
#         cnorm(2,1) = 2.0D0*x(2)

#     case (5)

#         !  Intersection of parabola x^2 with straight line 2x+3
#         !  Maximise f(x1) = x1**2
#         !  subject to the following constraint:
#         !  c1(x1) = x1**2 - 2.0D0*x1 - 3 = 0

#         fgrd(1) = 2.0D0*x(1)

#         cnorm(1,1) = 2.0D0*x(1) - 2.0D0

#     end select

#   end subroutine dobjfn

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
        # Test 3 is not supposed to converge
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
        self.errlg = None
        self.errlm = None
        self.errcom = None
        self.errcon = None
        self.ifail = None

class Vmcon1(Vmcon):
    """Override fcnvmc1 and 2 methods for test case 1.

    This allows a test to be run using custom function and gradient function
    evaluators specific to this test.
    
    Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
    subject to the following constraints:
    c1(x1,x2) = x1 - 2*x2 + 1 = 0 
    c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
    :param Vmcon: base Vmcon class
    :type Vmcon: Vmcon
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
    case.vmcon.xtol = 1.0e-8
    case.vmcon.x[0:2] = 2.0e0

    # No bounds on x values set
    case.vmcon.ilower[0:2] = 0.0
    case.vmcon.iupper[0:2] = 0.0
    case.vmcon.bndl[:] = 0.0
    case.vmcon.bndu[:] = 0.0

    # Expected values
    case.exp.x = np.array([8.228756e-1, 9.114378e-1])
    case.exp.objf = 1.393464
    case.exp.c = np.array([1.387778e-17, -7.671641e-13])
    case.exp.vlam = np.array([-1.594491, 1.846591])
    case.exp.errlg = 3.345088e-12
    case.exp.errlm = 0.0
    case.exp.errcom = 1.416660e-12
    case.exp.errcon = 7.671779e-13
    case.exp.ifail = 1

    return case

class Vmcon2(Vmcon):
    """Override fcnvmc1 and 2 methods for test case 2.
        
    Minimise f(x1,x2) = (x1 - 2)**2 + (x2 - 1)**2
    subject to the following constraints:
    c1(x1,x2) = x1 - 2*x2 + 1 >= 0
    c2(x1,x2) = -x1**2/4 - x2**2 + 1 >= 0
    :param Vmcon: base Vmcon class
    :type Vmcon: Vmcon
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
    case.vmcon.xtol = 1.0e-8

    # No bounds on x values set
    case.vmcon.ilower[0:2] = 0.0
    case.vmcon.iupper[0:2] = 0.0
    case.vmcon.bndl[:] = 0.0
    case.vmcon.bndu[:] = 0.0

    # Expected values
    case.exp.x = np.array([1.664968, 5.540486e-1])
    case.exp.objf = 3.111186e-1
    case.exp.c = np.array([1.556871, -1.021405e-14])
    case.exp.vlam = np.array([0.0, 8.048955e-1])
    case.exp.errlg = 2.343333e-11
    case.exp.errlm = 0.0
    case.exp.errcom = 8.221245e-15
    case.exp.errcon = 1.021405e-14
    case.exp.ifail = 1

    return case

def get_cases():
    """Create list of test cases to run.

    :return: list of Case-derived objects
    :rtype: list
    """
    cases = [
        get_case1(),
        get_case2()
    ]
    return cases

@pytest.fixture(params=get_cases())
def case(request):
    """Parameterised fixture for providing Vmcon test cases to run.

    :param request: provides access to different parameterised test cases
    :type request: SubRequest
    :return: Vmcon scenario to run and its expected result
    :rtype: Case
    """
    case = request.param
    return case

def test_vmcon(case):
    """Integration test for Vmcon.

    :param case: a Vmcon scenario and its expected result
    :type case: Case
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