from process.fortran import numerics
from process import solver


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
        ifail, x, objf, conf = solver.solve(self.models)

        # If fail then alter value of epsfcn - this can be improved
        if ifail != 1:
            print("Trying again with new epsfcn")
            # epsfcn is only used in evaluators.Evaluators()
            numerics.epsfcn = numerics.epsfcn * 10  # try new larger value
            print("new epsfcn = ", numerics.epsfcn)
            ifail, x, objf, conf = solver.solve(self.models)
            numerics.epsfcn = numerics.epsfcn / 10  # reset value

        if ifail != 1:
            print("Trying again with new epsfcn")
            numerics.epsfcn = numerics.epsfcn / 10  # try new smaller value
            print("new epsfcn = ", numerics.epsfcn)
            ifail, x, objf, conf = solver.solve(self.models)
            numerics.epsfcn = numerics.epsfcn * 10  # reset value

        # If VMCON has exited with error code 5 try another run using a multiple
        # of the identity matrix as input for the Hessian b(n,n)
        # Only do this if VMCON has not iterated (nviter=1)
        if ifail == 5 and numerics.nviter < 2:
            print(
                "VMCON error code = 5.  Rerunning VMCON with a new initial "
                "estimate of the second derivative matrix."
            )
            ifail, x, objf, conf = solver.solve(self.models, b=2.0)

        self.output(x, conf)

        return ifail

    def output(self, x, conf):
        """Store results back in Fortran numerics module."""
        numerics.xcm[: x.shape[0]] = x
        numerics.rcm[: conf.shape[0]] = conf
