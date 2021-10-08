from logging import getLogger
from process.fortran import vmcon_module
from process.fortran import numerics
from process.fortran import global_variables
from process.fortran import define_iteration_variables
from process.evaluators import Evaluators
import numpy as np

logger = getLogger(__name__)

class Vmcon():
    """Driver for Fortran vmcon module."""
    def __init__(self, models):
        """Initialise vars for input/output with Fortran vmcon module.
        
        :param models: physics and engineering model objects
        :type models: process.main.Models
        """
        self.evaluators = Evaluators(models)

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
        self.ifail = 0
        # ifail is the returned error code: 1 is OK
        self.f = 0.0
        # f is the value of objective function at the output point

        # Arrays used by vmcon, grouped by dimension
        # np.zeros used as occasional seg faults occur when using uninitialised
        # np.ndarray
        self.b = np.zeros((ippn1, ippn1), dtype=np.float64, order="F")
        
        self.cnorm = np.zeros((ippn1, ipeqns), dtype=np.float64, order="F")

        self.x = np.zeros(ipnvars, dtype=np.float64, order="F")
        self.fgrd = np.zeros(ipnvars, dtype=np.float64, order="F")
        self.glag = np.zeros(ipnvars, dtype=np.float64, order="F")
        self.glaga = np.zeros(ipnvars, dtype=np.float64, order="F")
        self.gamma = np.zeros(ipnvars, dtype=np.float64, order="F")
        self.bdelta = np.zeros(ipnvars, dtype=np.float64, order="F")
        self.bndl = np.zeros(self.n, dtype=np.float64, order="F")
        self.bndu = np.zeros(self.n, dtype=np.float64, order="F")
        self.eta = np.zeros(ipnvars, dtype=np.float64, order="F")
        self.xa = np.zeros(ipnvars, dtype=np.float64, order="F")
        self.iupper = np.zeros(self.n, dtype=np.float64, order="F")
        self.ilower = np.zeros(self.n, dtype=np.float64, order="F")

        self.bdl = np.zeros(ippn1, dtype=np.float64, order="F")
        self.bdu = np.zeros(ippn1, dtype=np.float64, order="F")
        self.gm = np.zeros(ippn1, dtype=np.float64, order="F")

        self.wa = np.zeros(iplh, dtype=np.float64, order="F")
        
        self.h = np.zeros((iplh, iplh), dtype=np.float64, order="F")

        self.delta = np.zeros(ipldel, dtype=np.float64, order="F")

        self.vmu = np.zeros(ipvmu, dtype=np.float64, order="F")

        self.conf = np.zeros(ipeqns, dtype=np.float64, order="F")
        self.cm = np.zeros(ipeqns, dtype=np.float64, order="F")
        
        self.iwa = np.zeros(ipliwa, dtype=np.int32, order="F")

        for i in range(self.n):
            self.ilower[i] = 1
            self.iupper[i] = 1

        # Counter for fcnvmc1 calls
        self.fcnvmc1_calls = 0
        self.fcnvmc1_first_call = True

    def load_iter_vars(self):
        """Load Fortran iteration variables, then initialise Python arrays."""
        # Set up variables to be iterated
        define_iteration_variables.loadxc()
        define_iteration_variables.boundxc()
        
        # Initialise arrays: relies on arrays in numerics being loaded above
        for i in range(self.n):
            self.bndl[i] = numerics.bondl[i]
            self.bndu[i] = numerics.bondu[i]
            self.x[i] = numerics.xcm[i]

    def run(self):
        """Load vars into vmcon, run it and extract results."""
        vmcon_module.load(
            self.mode, self.n, self.m, self.meq, self.x, self.lcnorm, self.b,
            self.lb, self.xtol, global_variables.maxcal, self.ldel, self.lh,
            self.lwa, self.liwa, self.ilower, self.iupper, self.bndl, self.bndu
        )

        self.run_vmcon()

        self.ifail, numerics.nfev2, numerics.nviter, self.objf, self.x[:self.n], self.b, \
            self.iwa, self.fgrd[:self.n], self.conf[:self.m], self.glag[:self.n], \
            self.glaga[:self.n], self.gamma[:self.n], self.eta[:self.n], \
            self.xa[:self.n], self.bdelta[:self.n], self.cm[:self.m],  \
            self.delta, self.wa, self.cnorm[:self.lcnorm, :self.m], self.h, \
            numerics.vlam[:self.m + (2 * self.n) + 1], \
            self.vmu[:self.m + (2 * self.n) + 1], self.gm[:self.n + 1], \
            self.bdl[:self.n + 1], self.bdu[:self.n + 1], global_variables.convergence_parameter \
            = vmcon_module.unload(self.n, self.m, self.lcnorm, self.lb, self.ldel, self.lh, self.lwa, self.liwa)
        
        # TODO Rather than passing slices of these arrays, it may be possible to
        # simplify this by only creating the arrays at the required, rather
        # than maximum possible, length now they are created in Python

    def run_vmcon(self):
        """Call Fortran subroutines to actually run vmcon.

        This drives the Fortran vmcon module. The purpose of this is to
        separate the calls of fcnvmc1 and fcnvmc2 from the solver (vmcon) and
        call them from Python, so the physics and engineering modules can be
        accessed from Python.
        
        Calculates the least value of a function of several variables
        subject to linear and/or nonlinear equality and inequality
        constraints
        author: R L Crane, K E Hillstrom, M Minkoff, Argonne National Lab
        author: J Galambos, FEDC/ORNL
        author: P J Knight, CCFE, Culham Science Centre
        author: M D Kovari, CCFE, Culham Science Centre

        This subroutine calculates the least value of a function of
        several variables subject to linear and/or nonlinear equality
        and inequality constraints.
        More particularly, it solves the problem
        Minimize f(x)
        subject to c (x) =  0.0 ,  i = 1,...,meq
        and c (x) >= 0.0 ,  i = meq+1,...,m
        and l <= x <= u  ,  i = 1,...,n
        The subroutine implements a variable metric method for
        constrained optimization developed by M.J.D. Powell.
        ANL-80-64: Solution of the General Nonlinear Programming Problem
        with Subroutine VMCON, Roger L Crane, Kenneth E Hillstrom and
        Michael Minkoff, Argonne National Laboratory, 1980
        """
        # TODO Accommodate for vmcon_test.f90 testing subroutines
        
        # exit_code is used to handle exits and returns in previous vmcon() 
        # subroutine, which is now broken up into smaller subroutines.
        # TODO This could be improved with further refactoring once smaller 
        # vmcon subroutines are in place.

        # Checked after each subroutine call:
        # 0: OK, continue
        # 1: return from run_vmcon()
        # 2: break from the current loop
        vmcon_module.exit_code = 0
        
        vmcon_module.vmcon1()
        if vmcon_module.exit_code == 1:
            return
            
        # fcnvmc1: routine to calculate the objective and constraint functions
        self.fcnvmc1_wrapper()
        if vmcon_module.exit_code == 1:
            return
        
        vmcon_module.vmcon2()
        if vmcon_module.exit_code == 1:
            return

        # fcnvmc2: routine to calculate the gradients of the objective and 
        # constraint functions
        self.fcnvmc2_wrapper()
        if vmcon_module.exit_code == 1:
            return

        vmcon_module.vmcon3()
        if vmcon_module.exit_code == 1:
            return

        # Iteration
        while True:
            vmcon_module.vmcon4()
            if vmcon_module.exit_code == 1:
                return
            
            # Set sum to the weighted sum of infeasibilities
            # Set fls to the line search objective function
            # Line search
            while True:
                vmcon_module.vmcon5()

                if (vmcon_module.nfev == vmcon_module.nfinit):
                    vmcon_module.vmcon6()
                    if vmcon_module.exit_code == 1:
                        return
                else:
                    vmcon_module.vmcon7()
                    if vmcon_module.exit_code == 2:
                        break
                    
                    # Exit if the line search requires ten or more function evaluations
                    if vmcon_module.nfev >= (vmcon_module.nfinit + 10):
                        vmcon_module.vmcon8()
                        self.fcnvmc1_wrapper()
                        vmcon_module.vmcon9()
                        if vmcon_module.exit_code == 1:
                            return
                
                    vmcon_module.vmcon10()

                vmcon_module.vmcon11()
                if vmcon_module.exit_code == 1:
                    return

                self.fcnvmc1_wrapper()
                vmcon_module.vmcon12()
                if vmcon_module.exit_code == 1:
                    return
                # End of line search loop
        
            # Reset vmcon_module.exit_code; must be 2 to exit line_searh
            vmcon_module.exit_code = 0

            # Line search is complete. Calculate gradient of Lagrangian
            # function for use in updating hessian of Lagrangian
            self.fcnvmc1_wrapper()
            if vmcon_module.exit_code == 1:
                return
            
            self.fcnvmc2_wrapper()
            vmcon_module.vmcon13()
            if vmcon_module.exit_code == 1:
                return
            # End of iteration loop

    def fcnvmc1_wrapper(self):
        """Call fcnvmc1, synchronising variables before and after.
        
        Update Python Vmcon object from the vmcon Fortran module, run fcnvmc1()
        function evaluator, then update the Fortran module with the results.
        Input/output variables used by fcnvmc1() are kept in sync in both Python
        and Fortran.

        Strangely, some arrays of a given size are passed into subroutines which
        declare the argument as a different size. For example, self.x.size == 
        numerics.ipnvars (total number of variables available for iteration),
        yet vmcon_module.x.size == numerics.nvar (number of iteration variables
        to use). This was previously implicitly handled in the Fortran: the
        argument array was a smaller slice of the original.

        Now numpy arrays are being used, these slices need to be made explicit
        to handle the confusing different sizes of the same-named array in 
        different modules.
        """
        # Update self from Fortran vmcon module
        self.x[0:vmcon_module.x.size] = vmcon_module.x
        self.ifail = vmcon_module.info

        self.fcnvmc1()
        self.fcnvmc1_calls += 1

        # Update Fortran vmcon module from self
        vmcon_module.objf = self.objf

        try:
            vmcon_module.conf = self.conf[0:self.m]
        except ValueError:
            # Sometimes there's a size mismatch here: was hidden previously
            # by implicit Fortran array argument size coercion
            # vmcon_module.conf can be 1 element larger than self.m; this is
            # likely to be due to an intermittent memory error
            # TODO Investigate
            logger.warning(f"conf array size mismatch: attempted to broadcast "
                f"size {self.m} into {vmcon_module.conf.size}"
            )
            # Only assign range m
            vmcon_module.conf[0:self.m] = self.conf[0:self.m]

        vmcon_module.info = self.ifail

    def fcnvmc1(self):
        """Call the function evaluator for Vmcon.

        Calculates the objective and constraint functions.

        See comments on differing array sizes in fcnvmc1_wrapper().
        """
        self.objf, self.conf[0:self.m], self.ifail = self.evaluators.fcnvmc1(self.n, self.m, 
            self.x, self.ifail, self.fcnvmc1_first_call
        )

        if self.fcnvmc1_first_call == True:
            self.fcnvmc1_first_call = False
    
    def fcnvmc2_wrapper(self):
        """Call fcnvmc2, synchronising variables before and after.
        
        Update Python Vmcon object from the vmcon Fortran module, run fcnvmc2()
        function evaluator, then update the Fortran module with the results.
        Input/output variables used by fcnvmc2() are kept in sync in both Python
        and Fortran.

        See comments on differing array sizes in fcnvmc1_wrapper().
        """
        self.x[0:vmcon_module.x.size] = vmcon_module.x
        self.ifail = vmcon_module.info
        
        self.fcnvmc2()

        try:
            vmcon_module.fgrd = self.fgrd[0:self.n]
        except ValueError:
            # Sometimes there's a size mismatch here: same as fcnvmc1_wrapper()
            # comment. This appears to be due to serious underlying memory
            # problems concerning assumed-size (*) arrays which change size
            # from what they were allocated originally without error.
            # The symptoms are treated below; this replicates the array size
            # coercion in Fortran, which bizarrely passes the regression tests.
            # The root cause of these memory errors needs to be found urgently.
            # TODO Investigate
            logger.warning(f"fgrd array size mismatch: attempted to broadcast "
                f"size {self.n} into {vmcon_module.fgrd.size}"
            )
            # vmcon_module.fgrd should be of length n
            # If larger, then only assign a slice of length n
            # If smaller, assign as big a slice as possible
            # This is required, and does work
            if vmcon_module.fgrd.size > self.fgrd.size:
                vmcon_module.fgrd[0:self.n] = self.fgrd[0:self.n]
            else:
                vmcon_module.fgrd = self.fgrd[0:vmcon_module.fgrd.size]

        vmcon_module.cnorm = self.cnorm[0:self.lcnorm, 0:self.m]
        vmcon_module.info = self.ifail
        
    def fcnvmc2(self):
        """Call the gradient function evaluator for Vmcon.

        Calculates the gradients of the objective and constraint functions.

        See comments on differing array sizes in fcnvmc1_wrapper().
        """
        (
            self.fgrd[0:self.n],
            self.cnorm[0:self.lcnorm, 0:self.m],
            self.ifail,
        ) = self.evaluators.fcnvmc2(self.n, self.m, self.x, self.lcnorm, self.ifail)