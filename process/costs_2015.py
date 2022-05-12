import logging
from process.fortran import constants
from process.fortran import costs_2015_module
from process.fortran import cost_variables
from process.fortran import heat_transport_variables
from process.fortran import process_output as po


logger = logging.getLogger(__name__)
# Logging handler for console output
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.INFO)
logger.addHandler(s_handler)


class Costs2015:
    def __init__(self):
        self.outfile = constants.nout

    def run(self, output: bool):
        """
        Cost accounting for a fusion power plant
        author: J Morris, CCFE, Culham Science Centre
        outfile : input integer : output file unit
        iprint : input integer : switch for writing to output file (1=yes)
        This routine performs the cost accounting for a fusion power plant.
        PROCESS Costs Paper (M. Kovari, J. Morris)
        """
        costs_2015_module.ip = int(output)
        costs_2015_module.ofile = self.outfile

        # ###############################################

        # Calculate building costs
        costs_2015_module.calc_building_costs()

        # Calculate land costs
        costs_2015_module.calc_land_costs()

        # Calculate tf coil costs
        costs_2015_module.calc_tf_coil_costs()

        # Calculate fwbs costs
        costs_2015_module.calc_fwbs_costs()

        # Calculate remote handling costs
        costs_2015_module.calc_remote_handling_costs()

        # Calculate N plant and vacuum vessel costs
        costs_2015_module.calc_n_plant_and_vv_costs()

        # Calculate energy conversion system costs
        costs_2015_module.calc_energy_conversion_system()

        # Calculate remaining subsystems costs
        costs_2015_module.calc_remaining_subsystems()

        # Calculate total capital cost
        costs_2015_module.total_costs = (
            costs_2015_module.s_cost[8]
            + costs_2015_module.s_cost[12]
            + costs_2015_module.s_cost[20]
            + costs_2015_module.s_cost[26]
            + costs_2015_module.s_cost[30]
            + costs_2015_module.s_cost[33]
            + costs_2015_module.s_cost[34]
            + costs_2015_module.s_cost[60]
        )

        # Save as concost, the variable used as a Figure of Merit (M$)
        cost_variables.concost = costs_2015_module.total_costs / 1.0e6

        # Electrical output (given availability) for a whole year
        costs_2015_module.mean_electric_output = (
            heat_transport_variables.pnetelmw * cost_variables.cpfact
        )
        costs_2015_module.annual_electric_output = (
            costs_2015_module.mean_electric_output * 24.0e0 * 265.25e0
        )

        # Annual maintenance cost.
        costs_2015_module.maintenance = (
            costs_2015_module.s_cost[26] + costs_2015_module.s_cost[37]
        ) * cost_variables.maintenance_fwbs + (
            costs_2015_module.s_cost[8]
            + costs_2015_module.s_cost[30]
            + costs_2015_module.s_cost[33]
            + costs_2015_module.s_cost[34]
            + costs_2015_module.s_cost[40]
            + costs_2015_module.s_cost[42]
            + costs_2015_module.s_cost[44]
            + costs_2015_module.s_cost[46]
            + costs_2015_module.s_cost[47]
            + costs_2015_module.s_cost[48]
            + costs_2015_module.s_cost[49]
            + costs_2015_module.s_cost[50]
            + costs_2015_module.s_cost[51]
            + costs_2015_module.s_cost[52]
            + costs_2015_module.s_cost[53]
            + costs_2015_module.s_cost[57]
        ) * cost_variables.maintenance_gen

        # Levelized cost of electricity (LCOE) ($/MWh)
        if costs_2015_module.annual_electric_output > 0.00001:
            cost_variables.coe = (1.0e0 / costs_2015_module.annual_electric_output) * (
                costs_2015_module.total_costs / cost_variables.amortization
                + costs_2015_module.maintenance
            )

        # Switch on output if there is a NaN error
        if (abs(cost_variables.concost) > 9.99e99) or (
            cost_variables.concost != cost_variables.concost
        ):
            costs_2015_module.write_costs_to_output
            for i in 100:
                logger.log(
                    costs_2015_module.s_label[i],
                    costs_2015_module.s_kref[i],
                    costs_2015_module.s_k[i],
                    costs_2015_module.s_cref[i],
                    costs_2015_module.s_cost[i],
                    costs_2015_module.s_cost_factor[i],
                )
                po.ocmmnt(
                    self.outfile,
                    costs_2015_module.s_label[i],
                    costs_2015_module.s_kref[i],
                    costs_2015_module.s_k[i],
                    costs_2015_module.s_cref[i],
                    costs_2015_module.s_cost[i],
                    costs_2015_module.s_cost_factor[i],
                )

            return

        # Output costs #
        # ###############

        if (costs_2015_module.ip == 0) or (cost_variables.output_costs == 0):
            return

        costs_2015_module.write_costs_to_output()
