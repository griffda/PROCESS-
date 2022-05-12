import logging
from process.fortran import constants
from process.fortran import costs_2015_module
from process.fortran import cost_variables
from process.fortran import heat_transport_variables
from process.fortran import process_output as po
from process.fortran import global_variables
from process.fortran import fwbs_variables
from process.fortran import build_variables
from process.utilities.f2py_string_patch import string_to_f2py_compatible


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
        costs_2015_module.outfile = self.outfile

        # ###############################################

        # Calculate building costs
        costs_2015_module.calc_building_costs()

        # Calculate land costs
        costs_2015_module.calc_land_costs()

        # Calculate tf coil costs
        costs_2015_module.calc_tf_coil_costs()

        # Calculate fwbs costs
        self.calc_fwbs_costs()

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
            self.write_costs_to_output()
            for i in 100:  # noqa: E741
                logger.log(
                    costs_2015_module.s_label[i],  # noqa: E741
                    costs_2015_module.s_kref[i],  # noqa: E741
                    costs_2015_module.s_k[i],  # noqa: E741
                    costs_2015_module.s_cref[i],  # noqa: E741
                    costs_2015_module.s_cost[i],  # noqa: E741
                    costs_2015_module.s_cost_factor[i],  # noqa: E741
                )
                po.ocmmnt(
                    self.outfile,
                    costs_2015_module.s_label[i],  # noqa: E741
                    costs_2015_module.s_kref[i],  # noqa: E741
                    costs_2015_module.s_k[i],  # noqa: E741
                    costs_2015_module.s_cref[i],  # noqa: E741
                    costs_2015_module.s_cost[i],  # noqa: E741
                    costs_2015_module.s_cost_factor[i],  # noqa: E741
                )

            return

        # Output costs #
        # ###############

        if (costs_2015_module.ip == 0) or (cost_variables.output_costs == 0):
            return

        self.write_costs_to_output()

    def calc_fwbs_costs(self):
        """
        Function to calculate the cost of the first wall, blanket and shield
        author: J Morris, CCFE, Culham Science Centre
        None
        This routine calculates the cost of the first wall, blanket and shield
        coils for a fusion power plant based on the costings in the PROCESS costs paper.
        PROCESS Costs Paper (M. Kovari, J. Morris)
        """

        for i in range(21, 27):  # noqa: E741
            costs_2015_module.s_cost_factor[
                i
            ] = cost_variables.cost_factor_fwbs  # noqa: E741

        # Enrichment
        # Costs based on the number of separative work units (SWU) required
        #
        # SWU = P V(x_p) + T V(x_t) - F V(x_f)
        #
        # where V(x) is the value function
        #
        # V(x) = (1 - 2x)ln((1-x)/x)

        # Percentage of lithium 6 in the feed (natural abundance)
        feed_li6 = 0.0742e0
        # Percentage of lithium 6 in the tail (waste) (75% natural abundance)
        tail_li6 = feed_li6 * 0.75e0

        # Built-in test
        if (costs_2015_module.ip == 1) and (global_variables.run_tests == 1):
            product_li6 = 0.3
            feed_to_product_mass_ratio = (product_li6 - tail_li6) / (
                feed_li6 - tail_li6
            )
            tail_to_product_mass_ratio = (product_li6 - feed_li6) / (
                feed_li6 - tail_li6
            )
            p_v = costs_2015_module.value_function(product_li6)
            t_v = costs_2015_module.value_function(tail_li6)
            f_v = costs_2015_module.value_function(feed_li6)
            swu = (
                p_v
                + tail_to_product_mass_ratio * t_v
                - feed_to_product_mass_ratio * f_v
            )
            if abs(swu - 2.66e0) < 2.0e-2:
                po.ocmmnt(
                    self.outfile,
                    "SWU for default 30% enrichment.  Should = 2.66. CORRECT",
                )
            else:
                po.ocmmnt(
                    self.outfile,
                    "SWU for default 30% enrichment.  Should = 2.66. ERROR",
                )

            # Reference cost
            costs_2015_module.s_label[21] = "Lithium enrichment"
            costs_2015_module.s_cref[21] = 0.1e6
            costs_2015_module.s_k[21] = 64.7e0
            costs_2015_module.s_kref[21] = 64.7e0
            costs_2015_module.s_cost[21] = (
                costs_2015_module.s_cost_factor[21]
                * costs_2015_module.s_cref[21]
                * (costs_2015_module.s_k[21] / costs_2015_module.s_kref[21])
                ** cost_variables.costexp
            )
            if abs(costs_2015_module.s_cost[21] - 0.1e6) / 0.1e6 < 1.0e-3:
                po.ocmmnt(self.outfile, "Reference cost for enrichment CORRECT")
            else:
                po.ocmmnt(self.outfile, "Reference cost for enrichment ERROR")

        # Lithium 6 enrichment cost ($)
        costs_2015_module.s_label[21] = string_to_f2py_compatible(
            costs_2015_module.s_label[21], "Lithium enrichment"
        )

        # Zero cost for natural enrichment
        if fwbs_variables.li6enrich <= 7.42e0:
            costs_2015_module.s_cost[21] = 0.0e0
        else:
            # Percentage of lithium 6 in the product
            product_li6 = min(fwbs_variables.li6enrich, 99.99e0) / 100.0e0
            # SWU will be calculated for a unit mass of product (P=1)

            # Feed to product mass ratio
            feed_to_product_mass_ratio = (product_li6 - tail_li6) / (
                feed_li6 - tail_li6
            )

            # Tail to product mass ratio
            tail_to_product_mass_ratio = (product_li6 - feed_li6) / (
                feed_li6 - tail_li6
            )

            # Calculate value functions
            p_v = costs_2015_module.value_function(product_li6)
            t_v = costs_2015_module.value_function(tail_li6)
            f_v = costs_2015_module.value_function(feed_li6)

            # Calculate separative work units per kg
            swu = (
                p_v
                + tail_to_product_mass_ratio * t_v
                - feed_to_product_mass_ratio * f_v
            )

            # Mass of lithium (kg).  Lithium orthosilicate is 22% lithium by mass.
            mass_li = fwbs_variables.whtblli4sio4 * 0.22

            # Total swu for lithium in blanket
            total_swu = swu * mass_li

            # Reference cost for lithium enrichment (2014 $)
            costs_2015_module.s_cref[21] = 0.1e6
            # Reference case of lithium SWU
            costs_2015_module.s_k[21] = total_swu
            costs_2015_module.s_kref[21] = 64.7e0
            costs_2015_module.s_cost[21] = (
                costs_2015_module.s_cost_factor[21]
                * costs_2015_module.s_cref[21]
                * (costs_2015_module.s_k[21] / costs_2015_module.s_kref[21])
                ** cost_variables.costexp
            )

        costs_2015_module.s_label[22] = "Lithium orthosilicate pebble manufacturing"
        # Reference cost of lithium pebble manufacture (2014 $)
        costs_2015_module.s_cref[22] = 6.5e4
        # Scale with mass of pebbles (kg)
        costs_2015_module.s_k[22] = fwbs_variables.whtblli4sio4
        costs_2015_module.s_kref[22] = 10.0e0
        costs_2015_module.s_cost[22] = (
            costs_2015_module.s_cost_factor[22]
            * costs_2015_module.s_cref[22]
            * (costs_2015_module.s_k[22] / costs_2015_module.s_kref[22])
            ** cost_variables.costexp_pebbles
        )

        costs_2015_module.s_label[23] = "Titanium beryllide pebble manufacturing"
        #  Reference cost of titanium beryllide pebble manufacture (2014 $)
        costs_2015_module.s_cref[23] = 450.0e6
        #  Scale with mass of titanium beryllide pebbles (kg)
        costs_2015_module.s_k[23] = fwbs_variables.whtbltibe12
        costs_2015_module.s_kref[23] = 1.0e5
        costs_2015_module.s_cost[23] = (
            costs_2015_module.s_cost_factor[23]
            * costs_2015_module.s_cref[23]
            * (costs_2015_module.s_k[23] / costs_2015_module.s_kref[23])
            ** cost_variables.costexp_pebbles
        )

        costs_2015_module.s_label[24] = "First wall W coating manufacturing"
        #  Reference (PPCS A) first wall W coating cost (2014 $)
        costs_2015_module.s_cref[24] = 25.0e6
        #  W density (kg/m^3)
        W_density = 19250.0e0
        #  First wall W coating mass (kg)
        costs_2015_module.s_k[24] = (
            build_variables.fwarea * fwbs_variables.fw_armour_thickness * W_density
        )
        costs_2015_module.s_kref[24] = 29000.0e0
        costs_2015_module.s_cost[24] = (
            costs_2015_module.s_cost_factor[24]
            * costs_2015_module.s_cref[24]
            * (costs_2015_module.s_k[24] / costs_2015_module.s_kref[24])
            ** cost_variables.costexp
        )

        costs_2015_module.s_label[25] = "Blanket and shield materials and manufacturing"
        # The cost of making the blanket was estimated for PPCS A.
        # This cost includes only manufacturing – not R&D, transport, or assembly in the reactor.
        # It includes the first wall, blanket and shield, but excludes the breeder and multiplier materials.
        costs_2015_module.s_cref[25] = 317.0e6
        #  Scale with steel mass in blanket + shield mass
        costs_2015_module.s_k[25] = fwbs_variables.whtblss + fwbs_variables.whtshld
        costs_2015_module.s_kref[25] = 4.07e6
        costs_2015_module.s_cost[25] = (
            costs_2015_module.s_cost_factor[25]
            * costs_2015_module.s_cref[25]
            * (costs_2015_module.s_k[25] / costs_2015_module.s_kref[25])
            ** cost_variables.costexp
        )

        costs_2015_module.s_label[26] = "Total first wall and blanket cost"
        costs_2015_module.s_cost[26] = 0.0e0
        for j in range(21, 26):
            costs_2015_module.s_cost[26] = (
                costs_2015_module.s_cost[26] + costs_2015_module.s_cost[j]
            )

    def write_costs_to_output(self):
        """
        Function to output the costs calculations
        author: J Morris, CCFE, Culham Science Centre
        None
        This routine outputs the costs to output file
        PROCESS Costs Paper (M. Kovari, J. Morris)
        """
        po.oheadr(
            self.outfile,
            'Estimate of "overnight" capital cost for a first of kind power plant (2014 M$)',
        )

        po.oshead(self.outfile, "Buildings (M$)")
        for i in range(9):  # noqa: E741
            costs_2015_module.ocost(
                self.outfile,
                costs_2015_module.s_label[i],  # noqa: E741
                i + 1,  # noqa: E741
                costs_2015_module.s_cost[i] / 1.0e6,  # noqa: E741
            )

        po.oshead(self.outfile, "Land (M$)")

        for j in range(9, 13):
            costs_2015_module.ocost(
                self.outfile,
                costs_2015_module.s_label[j],
                j + 1,
                costs_2015_module.s_cost[j] / 1.0e6,
            )

        po.oshead(self.outfile, "TF Coils (M$)")

        for k in range(13, 21):
            costs_2015_module.ocost(
                self.outfile,
                costs_2015_module.s_label[k],
                k + 1,
                costs_2015_module.s_cost[k] / 1.0e6,
            )

        po.oshead(self.outfile, "First wall and blanket (M$)")
        for l in range(21, 27):  # noqa: E741
            costs_2015_module.ocost(
                self.outfile,
                costs_2015_module.s_label[l],
                l + 1,
                costs_2015_module.s_cost[l] / 1.0e6,
            )

        po.oshead(self.outfile, "Active maintenance and remote handling (M$)")
        costs_2015_module.ocost(
            self.outfile,
            costs_2015_module.s_label[27],
            28,
            costs_2015_module.s_cost[27] / 1.0e6,
        )
        costs_2015_module.ocost(
            self.outfile,
            costs_2015_module.s_label[28],
            29,
            costs_2015_module.s_cost[28] / 1.0e6,
        )
        costs_2015_module.ocost(
            self.outfile,
            costs_2015_module.s_label[30],
            31,
            costs_2015_module.s_cost[30] / 1.0e6,
        )

        po.oshead(self.outfile, "Vacuum vessel and liquid nitrogen plant (M$)")
        for n in range(31, 34):
            costs_2015_module.ocost(
                self.outfile,
                costs_2015_module.s_label[n],
                n + 1,
                costs_2015_module.s_cost[n] / 1.0e6,
            )

        po.oshead(self.outfile, "System for converting heat to electricity (M$)")
        costs_2015_module.ocost(
            self.outfile,
            costs_2015_module.s_label[34],
            35,
            costs_2015_module.s_cost[34] / 1.0e6,
        )

        po.oshead(self.outfile, "Remaining subsystems (M$)")
        for q in range(35, 61):
            costs_2015_module.ocost(
                self.outfile,
                costs_2015_module.s_label[q],
                q + 1,
                costs_2015_module.s_cost[q] / 1.0e6,
            )

        po.oblnkl(self.outfile)
        costs_2015_module.ocost_vname(
            self.outfile,
            "TOTAL OVERNIGHT CAPITAL COST (M$)",
            "(total_costs)",
            costs_2015_module.total_costs / 1.0e6,
        )
        costs_2015_module.ocost_vname(
            self.outfile,
            "Annual maintenance cost (M$)",
            "(maintenance)",
            costs_2015_module.maintenance / 1.0e6,
        )
        po.oblnkl(self.outfile)
        po.ovarrf(
            self.outfile,
            "Net electric output (MW)",
            "(pnetelmw)",
            heat_transport_variables.pnetelmw,
            "OP ",
        )
        po.ovarrf(
            self.outfile, "Capacity factor", "(cpfact)", cost_variables.cpfact, "OP "
        )
        po.ovarrf(
            self.outfile,
            "Mean electric output (MW)",
            "(mean_electric_output)",
            costs_2015_module.mean_electric_output,
            "OP ",
        )
        po.ovarrf(
            self.outfile,
            "Capital cost / mean electric output ($/W)",
            "",
            costs_2015_module.total_costs
            / costs_2015_module.mean_electric_output
            / 1.0e6,
            "OP ",
        )
        po.ovarrf(
            self.outfile,
            "Levelized cost of electricity ($/MWh)",
            "(coe)",
            cost_variables.coe,
            "OP ",
        )
