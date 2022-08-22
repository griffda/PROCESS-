"""An adapter for different solvers."""

from process.vmcon import Vmcon
from process.fortran import optimiz_module
from process.fortran import numerics
import numpy as np

# TODO Hard-coded solver selection for now
method = "legacy-vmcon"


def solve(
    evaluators,
    x,
    bndl,
    bndu,
    m,
    meq,
    ifail=0,
    b=None,
    ilower=None,
    iupper=None,
    tolerance=None,
):
    """Run a solver.

    :param evaluators: objective and constraint function and gradient evaluator
    :type evaluators: process.evaluators.Evaluators
    :param x: iteration variables
    :type x: np.ndarray
    :param bndl: lower bounds for the iteration variables
    :type bndl: np.ndarray
    :param bndu: upper bounds for the iteration variables
    :type bndu: np.ndarray
    :param m: number of constraint equations
    :type m: int
    :param meq: of the constraint equations, how many are equalities
    :type meq: int
    :param ifail: previous exit code for the solver, defaults to 0
    :type ifail: int, optional
    :param b: multiplier for an identity matrix as input for the Hessian b(n,n),
    defaults to None
    :type b: float, optional
    :param ilower: array of 0s and 1s to activate lower bounds on iteration vars
    in x
    :type ilower: np.ndarray, optional
    :param iupper: array of 0s and 1s to activate upper bounds on iteration vars
    in x
    :type iupper: np.ndarray, optional
    :param tolerance: tolerance for solver termination, defaults to None
    :type tolerance: float, optional
    :return: info: solver return code, x: solution vector, objf: objective
    function value, conf: constraint values
    :rtype: tuple(int, np.ndarray, float, np.ndarray)
    """
    if method == "legacy-vmcon":
        info, x, objf, conf = run_legacy_vmcon(
            evaluators,
            x,
            ilower,
            iupper,
            bndl,
            bndu,
            m,
            meq,
            ifail,
            b,
            tolerance,
        )
    elif method == "new-vmcon":
        info, x, objf, conf = run_new_vmcon(evaluators, x, bndl, bndu)

    # TODO Maybe turn into a SolverResult object?
    return info, x, objf, conf


def run_legacy_vmcon(
    evaluators, x, ilower, iupper, bndl, bndu, m, meq, ifail, b, tolerance
):
    """Run an instance of legacy Vmcon.

    :param evaluators: instance to evaluate objective and constraint functions
    and gradients
    :type evaluators: process.evaluators.Evaluators
    :param x: iteration variables
    :type x: np.ndarray
    :param bndl: lower bounds for the iteration variables
    :type bndl: np.ndarray
    :param bndu: upper bounds for the iteration variables
    :type bndu: np.ndarray
    :param ifail: previous exit code for the solver
    :type ifail: int
    :param b: multiplier for an identity matrix as input for the Hessian b(n,n)
    :type b: float
    :param tolerance: tolerance for termination of solver
    :type tolerance: float
    :return: info: solver return code, x: solution vector, objf: objective
    function value, conf: constraint values
    :rtype: tuple(int, np.ndarray, float, np.ndarray)
    """
    if tolerance is None:
        tolerance = numerics.epsvmc

    vmcon = Vmcon(evaluators, x, ilower, iupper, bndl, bndu, m, meq, ifail, tolerance)

    # Write basic info to OPT.DAT, then run vmcon solver
    n = numerics.nvar
    m = numerics.neqns + numerics.nineqns
    optimiz_module.write_out(n, m)

    if b is not None:
        # Rerun vmcon with different second derivative matrix
        vmcon.mode = 1
        vmcon.b.fill(0.0)
        vmcon.b[:n, :n] = np.identity(n) * b

    vmcon.run()

    info = vmcon.ifail
    x = vmcon.x
    objf = vmcon.objf
    # Trim maximum-sized arrays down to actually used lengths
    conf = vmcon.conf[:m]

    return info, x, objf, conf


def run_new_vmcon():
    """Run the new VMCON implementation.

    :raises NotImplementedError: To be added.
    """
    raise NotImplementedError
