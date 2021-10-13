from process import fortran as ft
from process.fortran import costs_step_module as cs


class CostsStep:
    """STEP fusion power plant costings."""

    def __init__(self):
        """Initialise Fortran module variables."""
        cs.init_costs_step()

    def run(self):
        """Run main costs_step subroutine."""
        cs.costs_step(ft.constants.nout, 0)
