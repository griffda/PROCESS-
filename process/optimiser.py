from process.fortran import numerics
from process.solver import solve
from process.fortran import define_iteration_variables
from process.evaluators import Evaluators


class Optimiser:
    def __init__(self, models):
        """Creates and runs a Vmcon instance.

        This routine calls the minimisation/maximisation routine VMCON,
        developed by Argonne National Laboratory.
        On exit, the (normalised) value of the variable being maximised
        or minimised (i.e. the figure of merit) is returned in argument f.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code.

        This represents the old optimiz subroutine in the numerics module.

        :param models: physics and engineering model objects
        :type models: process.main.Models
        """
        self.models = models

    def run(self):
        """Run vmcon solver and retry if it fails in certain ways."""
        # Initialise iteration variables and bounds in Fortran
        define_iteration_variables.loadxc()
        define_iteration_variables.boundxc()

        # Initialise iteration variables and bounds in Python: relies on Fortran
        # iteration variables being defined above
        # Trim maximum size arrays down to actually used size
        n = numerics.nvar
        x = numerics.xcm[:n]
        bndl = numerics.bondl[:n]
        bndu = numerics.bondu[:n]

        # Define total number of constraints and equality constraints
        m = numerics.neqns + numerics.nineqns
        meq = numerics.neqns

        # Evaluators() calculates the objective and constraint functions and
        # their gradients for a given vector x
        evaluators = Evaluators(self.models)

        ifail, x, objf, conf = solve(evaluators, x, bndl, bndu, m, meq)

        # If fail then alter value of epsfcn - this can be improved
        if ifail != 1:
            print("Trying again with new epsfcn")
            # epsfcn is only used in evaluators.Evaluators()
            numerics.epsfcn = numerics.epsfcn * 10  # try new larger value
            print("new epsfcn = ", numerics.epsfcn)

            ifail, x, objf, conf = solve(
                evaluators, x, bndl, bndu, m, meq, ifail=ifail, first_call=False
            )
            # First solution attempt failed (ifail != 1): supply ifail value
            # to next attempt
            # first_call determines how Evaluators.fcnvmc1() runs the first time
            # TODO Check if fcnvmc1() could be called before the solver to
            # remove this dependency
            numerics.epsfcn = numerics.epsfcn / 10  # reset value

        if ifail != 1:
            print("Trying again with new epsfcn")
            numerics.epsfcn = numerics.epsfcn / 10  # try new smaller value
            print("new epsfcn = ", numerics.epsfcn)
            ifail, x, objf, conf = solve(
                evaluators, x, bndl, bndu, m, meq, ifail=ifail, first_call=False
            )
            numerics.epsfcn = numerics.epsfcn * 10  # reset value

        # If VMCON has exited with error code 5 try another run using a multiple
        # of the identity matrix as input for the Hessian b(n,n)
        # Only do this if VMCON has not iterated (nviter=1)
        if ifail == 5 and numerics.nviter < 2:
            print(
                "VMCON error code = 5.  Rerunning VMCON with a new initial "
                "estimate of the second derivative matrix."
            )
            ifail, x, objf, conf = solve(
                evaluators, x, bndl, bndu, m, meq, ifail=ifail, b=2.0, first_call=False
            )

        self.output(x, conf)

        return ifail

    def output(self, x, conf):
        """Store results back in Fortran numerics module."""
        numerics.xcm[: x.shape[0]] = x
        numerics.rcm[: conf.shape[0]] = conf
