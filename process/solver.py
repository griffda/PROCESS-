"""An adapter for different solvers."""

from process.evaluators import Evaluators
from process.vmcon import Vmcon
from process.fortran import optimiz_module
from process.fortran import numerics
import numpy as np

# TODO Hard-coded solver selection for now
method = "legacy-vmcon"


def solve(models, x, bndl, bndu, ifail=0, first_call=True, b=None):
    """Run a solver.

    :param models: physics and engineering model objects
    :type models: process.main.Models
    :param x: iteration variables
    :type x: np.ndarray
    :param bndl: lower bounds for the iteration variables
    :type bndl: np.ndarray
    :param bndu: upper bounds for the iteration variables
    :type bndu: np.ndarray
    :param ifail: previous exit code for the solver, defaults to 0
    :type ifail: int, optional
    :param first_call: boolean for running Evaluators.fcnvmc1() the first time,
    defaults to True
    :type first_call: bool, optional
    :param b: multiplier for an identity matrix as input for the Hessian b(n,n),
    defaults to None
    :type b: float, optional
    :return: info: solver return code, x: solution vector, objf: objective
    function value, conf: constraint values
    :rtype: tuple(int, np.ndarray, float, np.ndarray)
    """
    evaluators = Evaluators(models)

    if method == "legacy-vmcon":
        info, x, objf, conf = run_legacy_vmcon(
            evaluators, x, bndl, bndu, ifail, first_call, b
        )
    elif method == "new-vmcon":
        info, x, objf, conf = run_new_vmcon(evaluators, x, bndl, bndu)

    # TODO Maybe turn into a SolverResult object?
    return info, x, objf, conf


def run_legacy_vmcon(evaluators, x, bndl, bndu, ifail, first_call, b):
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
    :param first_call: boolean for running Evaluators.fcnvmc1() the first time,
    :type first_call: bool
    :param b: multiplier for an identity matrix as input for the Hessian b(n,n)
    :type b: float
    :return: info: solver return code, x: solution vector, objf: objective
    function value, conf: constraint values
    :rtype: tuple(int, np.ndarray, float, np.ndarray)
    """
    vmcon = Vmcon(evaluators, x, bndl, bndu, ifail, first_call)

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
