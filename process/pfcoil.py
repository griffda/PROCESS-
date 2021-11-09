from process.fortran import pfcoil_module as pf


class PFCoil:
    """Calculate poloidal field coil system parameters."""

    def __init__(self):
        """Initialise Fortran module variables."""
        pf.init_pfcoil_module()
