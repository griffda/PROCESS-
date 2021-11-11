from process import fortran as ft
from process.fortran import tfcoil_module as tf
from process.fortran import tfcoil_variables as tfv
from process.fortran import sctfcoil_module as sctf
from process.fortran import build_module as bm

class TFcoil:
    """Calculates the parameters of a resistive TF coil system for a fusion power plant"""

    def __init__(self):
        """Initialise Fortran module variables."""
        self.outfile = ft.constants.nout  # output file unit
        self.iprint = 0  # switch for writing to output file (1=yes)

    def run(self):
        """Run main tfcoil subroutine without outputting."""
        self.iprint = 0
        self.tfcoil()

    def output(self):
        """Run main tfcoil subroutine and write output."""
        self.iprint = 1
        self.tfcoil()
    
    def tfcoil(self):
        """TF coil module
        author: P J Knight, CCFE, Culham Science Centre
        outfile : input integer : output file unit
        iprint : input integer : switch for writing to output file (1=yes)
        This subroutine calculates various parameters for the TF coil set.
        If the TF coils are superconducting the calculations are performed
        in routine <A HREF="sctfcoil.html">sctfcoil</A> instead.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """

        sctf.sctfcoil(self.outfile, self.iprint)

        bm.portsz()