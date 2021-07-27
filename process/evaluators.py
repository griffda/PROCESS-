from process.fortran import caller_module
from process.fortran import global_variables as gv
from process.fortran import constraints
from process.fortran import cost_variables as cv
from process.fortran import numerics
from process.fortran import physics_variables as pv
from process.fortran import stellarator_variables as sv
from process.fortran import times_variables as tv
from process.fortran import function_evaluator
import numpy as np
import math
import logging

# TODO Need to set up consistent logger hierarchy
logger = logging.getLogger(__name__)
f_handler = logging.FileHandler("process.log", mode="w")
logger.addHandler(f_handler)


def fcnvmc1(n, m, xv, ifail_in, first_call):
    """Function evaluator for VMCON.

    This routine is the function evaluator for the VMCON
    maximisation/minimisation routine.

    It calculates the objective and constraint functions at the
    n-dimensional point of interest xv.
    Note that the equality constraints must precede the inequality
    constraints in conf.
    AEA FUS 251: A User's Guide to the PROCESS Systems Code
    :param n: number of variables
    :type n: int
    :param m: number of constraints
    :type m: int
    :param xv: scaled variable values, length n
    :type xv: numpy.array
    :param ifail_in: ifail_in error flag (<0 stops calculation)
    :type ifail_in: int
    :param first_call: first call of fcnvmc1() for this Vmcon run
    :type first_call: bool
    :return: tuple containing: objfn objective function, conf(m) constraint
    functions, ifail_out error flag (<0 stops calculation)
    :rtype: tuple
    """
    # Output array for constraint functions
    conf = np.zeros(m, dtype=np.float64, order="F")

    # Evaluate machine parameters at xv
    caller_module.caller(xv, n)

    # To ensure that, at the start of a run, all physics/engineering
    # variables are fully initialised with consistent values, we perform
    # a second evaluation call here
    if first_call:
        caller_module.caller(xv, n)

    # Convergence loop to ensure burn time consistency
    if sv.istell == 0:
        loop = 0
        while (loop < 10) and (
            abs((tv.tburn - tv.tburn0) / max(tv.tburn, 0.01)) > 0.001
        ):
            loop += 1
            caller_module.caller(xv, n)
            if gv.verbose == 1:
                print("Internal tburn consistency check: ", tv.tburn, tv.tburn0)

        if loop >= 10:
            print("Burn time values are not consistent in iteration: ", numerics.nviter)
            print("tburn, tburn0: ", tv.tburn, tv.tburn0)

    # Evaluate figure of merit (objective function)
    objf = function_evaluator.funfom()

    # Evaluate constraint equations
    constraints.constraint_eqns(m, conf, -1)

    # To stop the program, set ifail < 0 here.
    # TODO Not sure this serves any purpose
    ifail_out = 1 * ifail_in

    # Verbose diagnostics
    if gv.verbose == 1:
        summ = 0.0
        for i in range(m):
            summ = summ + conf(i) ** 2

        sqsumconfsq = math.sqrt(summ)
        logger.debug(
            numerics.nviter,
            (1 - math.mod(ifail_out, 7)) - 1,
            math.mod(numerics.nviter, 2) - 1,
            pv.te,
            cv.coe,
            pv.rmajor,
            pv.powfmw,
            pv.bt,
            tv.tburn,
            sqsumconfsq,
            xv,
        )

    return objf, conf, ifail_out
