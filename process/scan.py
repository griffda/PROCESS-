from process.fortran import error_handling
from process.fortran import final_module
from process.fortran import scan_module

class Scan():
    """Perform a parameter scan using the Fortran scan module."""
    def __init__(self):
        """Immediately run the run_scan() method."""
        self.run_scan()

    def run_scan(self):
        """Call VMCON over a range of values of one of the variables.
        
        This routine calls the optimisation routine VMCON a number of times, by 
        performing a sweep over a range of values of a particular variable. A 
        number of output variable values are written to the PLOT.DAT file at 
        each scan point, for plotting or other post-processing purposes.
        """
        # Turn off error reporting (until next output)
        error_handling.errors_on = False

        if scan_module.isweep == 0:
            ifail = scan_module.doopt()
            final_module.final(ifail)
            return

        if scan_module.isweep > scan_module.ipnscns:
            error_handling.idiags[1] = scan_module.isweep
            error_handling.idiags[2] = scan_module.ipnscns
            error_handling.report_error(94)

        if scan_module.scan_dim == 2:
            scan_module.scan_2d()
        else:
            scan_module.scan_1d()