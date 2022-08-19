"""An adapter for different solvers."""

from process.evaluators import Evaluators
from process.vmcon import Vmcon
from process.fortran import optimiz_module
from process.fortran import numerics
import numpy as np

# TODO Hard-coded solver selection for now
method = "legacy-vmcon"


def solve(models, b=None):
    """Run a solver.

    :param models: physics and engineering model objects
    :type models: process.main.Models
    :param b: multiplier for an identity matrix as input for the Hessian b(n,n),
    defaults to None
    :type b: float, optional
    :return: info: solver return code, x: solution vector, objf: objective
    function value, conf: constraint values
    :rtype: tuple(int, np.ndarray, float, np.ndarray)
    """
    evaluators = Evaluators(models)

    if method == "legacy-vmcon":
        info, x, objf, conf = run_legacy_vmcon(evaluators, b)
    elif method == "new-vmcon":
        info, x, objf, conf = run_new_vmcon(evaluators)

    # TODO Maybe turn into a SolverResult object?
    return info, x, objf, conf


def run_legacy_vmcon(evaluators, b):
    """Run an instance of legacy Vmcon.

    :param evaluators: instance to evaluate objective and constraint functions
    and gradients
    :type evaluators: process.evaluators.Evaluators
    :param b: multiplier for an identity matrix as input for the Hessian b(n,n)
    :type b: float
    :return: info: solver return code, x: solution vector, objf: objective
    function value, conf: constraint values
    :rtype: tuple(int, np.ndarray, float, np.ndarray)
    """
    vmcon = Vmcon(evaluators)
    vmcon.load_iter_vars()

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
    # Trim maximum-sized arrays down to actually used lengths
    x = vmcon.x[:n]
    objf = vmcon.objf
    conf = vmcon.conf[:m]

    return info, x, objf, conf


def run_new_vmcon():
    """Run the new VMCON implementation.

    :raises NotImplementedError: To be added.
    """
    raise NotImplementedError
