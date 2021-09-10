"""Final output at the end of a scan."""
from process import fortran as ft
from process.fortran import final_module as fm


def finalise(ifail):
    """Routine to print out the final point in the scan.

    AEA FUS 251: A User's Guide to the PROCESS Systems Code
    :param ifail: error flag
    :type ifail: int
    """
    fm.final_header(ifail)

    # Output relevant to no optimisation
    if ft.numerics.ioptimz == -2:
        fm.no_optimisation()

    fm.final_output()
