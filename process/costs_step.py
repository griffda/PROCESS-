from process import fortran


class CostsStep():
    def __init__(self):
        fortran.costs_step_module.init_costs_step()