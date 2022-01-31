from process.fortran import pfcoil_module as pf
from process import fortran as ft


class PFCoil:
    """Calculate poloidal field coil system parameters."""

    def __init__(self):
        """Initialise Fortran module variables."""
        self.outfile = ft.constants.nout  # output file unit
        pf.init_pfcoil_module()

    def run(self):
        """Run the PF coil model."""
        pf.pfcoil()

        # Poloidal field coil inductance calculation
        pf.induct(self.outfile, 0)

        # Volt-second capability of PF coil set
        pf.vsec()

    def output(self):
        """Output results to output file."""
        ft.pfcoil_module.outpf(self.outfile)
        ft.pfcoil_module.outvolt(self.outfile)

    def output_induct(self):
        """Output poloidal field coil inductance calculation."""
        ft.pfcoil_module.induct(self.outfile, 1)
