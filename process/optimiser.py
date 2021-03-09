from process.fortran import optimiz_module
from process.fortran import vmcon_module
from process.fortran import numerics
from process.fortran import global_variables
import numpy as np

class Optimiser():
    def __init__(self):
        """Calls the minimisation/maximisation routine VMCON.

        This routine calls the minimisation/maximisation routine VMCON,
        developed by Argonne National Laboratory.
        On exit, the (normalised) value of the variable being maximised
        or minimised (i.e. the figure of merit) is returned in argument f.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code.

        This represents the old optimiz subroutine in the numerics module.
        """
        # Vars for array dimensions
        ipnvars = numerics.ipnvars
        ippn1 = ipnvars + 1
        ipeqns = numerics.ipeqns
        ipldel = 7 * ippn1
        iplh = 2 * ippn1
        ipliwa = (6 * ippn1) + ipeqns
        ipvmu = (ipeqns + 2) * (ipnvars + 1)

        # Attributes used by vmcon
        self.mode = 0
        self.n = numerics.nvar
        self.m = numerics.neqns + numerics.nineqns
        self.xtol = numerics.epsvmc
        self.lb = ippn1
        self.lcnorm = ippn1
        self.ldel = ipldel
        self.lh = iplh
        self.lwa = iplh
        self.liwa = ipliwa
        self.meq = numerics.neqns
        self.ifail = 1
        # ifail is the returned error code: 1 is OK
        self.f = 0.0
        # f is the value of objective function at the output point

        # Arrays used by vmcon
        self.b = np.ndarray((ippn1, ippn1), dtype=np.float64, order="F")
        
        self.cnorm = np.ndarray((ippn1, ipeqns), dtype=np.float64, order="F")

        self.xv = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.fgrd = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.glag = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.glaga = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.gammv = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.bdelta = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.bndl = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.bndu = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.etav = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.xa = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.iupper = np.ndarray(ipnvars, dtype=np.float64, order="F")
        self.ilower = np.ndarray(ipnvars, dtype=np.float64, order="F")

        self.bdl = np.ndarray(ippn1, dtype=np.float64, order="F")
        self.bdu = np.ndarray(ippn1, dtype=np.float64, order="F")
        self.gm = np.ndarray(ippn1, dtype=np.float64, order="F")

        self.wa = np.ndarray(iplh, dtype=np.float64, order="F")
        
        self.h = np.ndarray((iplh, iplh), dtype=np.float64, order="F")

        self.delta = np.ndarray(ipldel, dtype=np.float64, order="F")

        self.vmu = np.ndarray(ipvmu, dtype=np.float64, order="F")

        self.conf = np.ndarray(ipeqns, dtype=np.float64, order="F")
        self.cm = np.ndarray(ipeqns, dtype=np.float64, order="F")
        
        self.iwa = np.ndarray(ipliwa, dtype=np.int32, order="F")
        
        for i in range(self.n):
            self.ilower[i] = 1
            self.iupper[i] = 1

    def run(self):
        """Run vmcon solver and retry if it fails in certain ways."""
        # Initialise arrays: relies on arrays in numerics being loaded first.
        # This done in Scan.doopt()
        for i in range(self.n):
            self.bndl[i] = numerics.bondl[i]
            self.bndu[i] = numerics.bondu[i]
            self.xv[i] = numerics.xcm[i]

        # Write basic info to OPT.DAT, then run vmcon solver
        optimiz_module.write_out(self.n, self.m)
        self.vmcon()
        
        # If fail then alter value of epsfcn - this can be improved
        if self.ifail != 1:
            print('Trying again with new epsfcn')
            numerics.epsfcn = numerics.epsfcn * 10 # try new larger value
            print('new epsfcn = ', numerics.epsfcn)
            self.vmcon()
            numerics.epsfcn = numerics.epsfcn / 10 # reset value
        
        if self.ifail != 1:
            print('Trying again with new epsfcn')
            numerics.epsfcn = numerics.epsfcn / 10 # try new smaller value
            print('new epsfcn = ', numerics.epsfcn)
            self.vmcon()
            numerics.epsfcn = numerics.epsfcn * 10 # reset value

        # If VMCON has exited with error code 5 try another run using a multiple
        # of the identity matrix as input for the Hessian b(n,n)
        # Only do this if VMCON has not iterated (nviter=1)
        if (self.ifail == 5 and numerics.nviter < 2):
            self.mode = 1
            self.mod_2nd_derivative()
            self.vmcon()

        self.output()

    def vmcon(self):
        """Call the vmcon subroutine."""
        self.objf, self.ifail, numerics.nfev, numerics.nviter = vmcon_module.vmcon(
            self.mode, self.n, self.m, self.meq, self.xv, self.fgrd,
            self.conf, self.cnorm, self.lcnorm, self.b, self.lb, self.xtol,
            global_variables.maxcal, numerics.vlam, self.glag, self.vmu, 
            self.cm, self.glaga, self.gammv, self.etav, self.xa, self.bdelta,
            self.delta, self.ldel, self.gm, self.bdl, self.bdu, self.h,
            self.lh, self.wa, self.lwa, self.iwa, self.liwa, self.ilower,
            self.iupper, self.bndl, self.bndu, 
            global_variables.convergence_parameter
        )

    def mod_2nd_derivative(self):
        """Rerun vmcon with different second derivative matrix."""
        self.b.fill(0)
        bfactor = 2.0
        for i in range(self.n):
            self.b[i,i] = bfactor
            # Re-initialise iteration values
            self.xv[i] = numerics.xcm[i]

        print("VMCON error code = 5.  Rerunning VMCON with a new initial "
            "estimate of the second derivative matrix."
        )

    def output(self):
        """Store results back in Fortran numerics module."""
        for i in range(self.n):
            numerics.xcm[i] = self.xv[i]

        for i in range(self.m):
            numerics.rcm[i] = self.conf[i]