"""Final output at the end of a scan."""
from process import fortran as ft
from process.fortran import final_module as fm
from process.caller import caller


def finalise(ifail):
    """Routine to print out the final point in the scan.

    AEA FUS 251: A User's Guide to the PROCESS Systems Code
    :param ifail: error flag
    :type ifail: int
    """
    fm.final_header(ifail)

    # If no optimisation will be done, compute the OP variables now
    if ft.numerics.ioptimz == -2:
        ft.define_iteration_variables.loadxc()
        caller(ft.numerics.xcm, ft.numerics.nvar)
        fm.no_optimisation()

    fm.final_output()
