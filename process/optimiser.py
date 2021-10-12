from process.fortran import optimiz_module
from process.fortran import numerics
from process.vmcon import Vmcon

class Optimiser():
    def __init__(self):
        """Creates and runs a Vmcon instance.

        This routine calls the minimisation/maximisation routine VMCON,
        developed by Argonne National Laboratory.
        On exit, the (normalised) value of the variable being maximised
        or minimised (i.e. the figure of merit) is returned in argument f.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code.

        This represents the old optimiz subroutine in the numerics module.
        """
        self.vmcon = Vmcon()

    def run(self):
        """Run vmcon solver and retry if it fails in certain ways."""
        self.vmcon.load_iter_vars()
        # Write basic info to OPT.DAT, then run vmcon solver
        optimiz_module.write_out(self.vmcon.n, self.vmcon.m)
        self.vmcon.run()
        
        # If fail then alter value of epsfcn - this can be improved
        if self.vmcon.ifail != 1:
            print('Trying again with new epsfcn')
            numerics.epsfcn = numerics.epsfcn * 10 # try new larger value
            print('new epsfcn = ', numerics.epsfcn)
            self.vmcon.run()
            numerics.epsfcn = numerics.epsfcn / 10 # reset value
        
        if self.vmcon.ifail != 1:
            print('Trying again with new epsfcn')
            numerics.epsfcn = numerics.epsfcn / 10 # try new smaller value
            print('new epsfcn = ', numerics.epsfcn)
            self.vmcon.run()
            numerics.epsfcn = numerics.epsfcn * 10 # reset value

        # If VMCON has exited with error code 5 try another run using a multiple
        # of the identity matrix as input for the Hessian b(n,n)
        # Only do this if VMCON has not iterated (nviter=1)
        if (self.vmcon.ifail == 5 and numerics.nviter < 2):
            self.vmcon.mode = 1
            self.mod_2nd_derivative()
            self.vmcon.run()

        self.output()

    def mod_2nd_derivative(self):
        """Rerun vmcon with different second derivative matrix."""
        self.vmcon.b.fill(0)
        bfactor = 2.0
        for i in range(self.vmcon.n):
            self.vmcon.b[i,i] = bfactor
            # Re-initialise iteration values
            self.vmcon.x[i] = numerics.xcm[i]

        print("VMCON error code = 5.  Rerunning VMCON with a new initial "
            "estimate of the second derivative matrix."
        )

    def output(self):
        """Store results back in Fortran numerics module."""
        for i in range(self.vmcon.n):
            numerics.xcm[i] = self.vmcon.x[i]

        for i in range(self.vmcon.m):
            numerics.rcm[i] = self.vmcon.conf[i]