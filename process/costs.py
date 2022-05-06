from process.fortran import constants, costs_module, cost_variables
from process.fortran import process_output as po
from process.fortran import ife_variables, fwbs_variables
from process.fortran import tfcoil_variables
from process.fortran import physics_variables
from process.fortran import buildings_variables
from process.fortran import build_variables


class Costs:
    def __init__(self):
        self.outfile = constants.nout

    def run(self, output: bool = False):
        costs_module.costs(self.outfile, int(output))

    def costs(self, output: bool):
        """
                Cost accounting for a fusion power plant
        author: P J Knight, CCFE, Culham Science Centre
        outfile : input integer : output file unit
        iprint : input integer : switch for writing to output file (1=yes)
        This routine performs the cost accounting for a fusion power plant.
        The direct costs are calculated based on parameters input
        from other sections of the code.
        <P>Costs are in 1990 $, and assume first-of-a-kind components
        unless otherwise stated. Account 22 costs include a multiplier
        to account for Nth-of-a-kind cost reductions.
        <P>The code is arranged in the order of the standard accounts.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        self.acc21()

        #  Account 22 : Fusion power island
        self.acc22()

        #  Account 23 : Turbine plant equipment
        costs_module.acc23()

        #  Account 24 : Electric plant equipment
        costs_module.acc241()  # Account 241 : Switchyard
        costs_module.acc242()  # Account 242 : Transformers
        costs_module.acc243()  # Account 243 : Low voltage
        costs_module.acc244()  # Account 244 : Diesel generators
        costs_module.acc245()  # Account 245 : Auxiliary facility power equipment
        costs_module.acc24()  # Account 24  : Total

        #  Account 25 : Miscellaneous plant equipment
        costs_module.acc25()

        #  Account 26 : Heat rejection system
        costs_module.acc26()

        #  Total plant direct cost
        # cdirt = c21 + c22 + costs_module.c23 + costs_module.c24 + costs_module.c25 + costs_module.c26 + chplant
        cost_variables.cdirt = (
            costs_module.c21
            + costs_module.c22
            + costs_module.c23
            + costs_module.c24
            + costs_module.c25
            + costs_module.c26
        )

        #  Account 9 : Indirect cost and project contingency
        costs_module.acc9()

        #  Constructed cost
        cost_variables.concost = (
            cost_variables.cdirt + costs_module.cindrt + costs_module.ccont
        )

        #  Cost of electricity
        if (cost_variables.ireactor == 1) and (cost_variables.ipnet == 0):
            costs_module.coelc(self.outfile, int(output))

        if output and cost_variables.output_costs == 1:

            po.oheadr(self.self.outfile, "Detailed Costings (1990 US$)")
            po.ovarre(
                self.outfile,
                "Acc.22 multiplier for Nth of a kind",
                "(fkind)",
                cost_variables.fkind,
            )
            po.ovarin(
                self.outfile, "Level of Safety Assurance", "(lsa)", costs_module.lsa
            )
            po.oblnkl(self.outfile)
            po.oshead(self.outfile, "Structures and Site Facilities")
            po.ocosts(
                self.outfile,
                "(c211)",
                "Site improvements, facilities, land (M$)",
                costs_module.c211,
            )
            po.ocosts(
                self.outfile, "(c212)", "Reactor building cost (M$)", costs_module.c212
            )
            po.ocosts(
                self.outfile, "(c213)", "Turbine building cost (M$)", costs_module.c213
            )
            po.ocosts(
                self.outfile,
                "(c2141)",
                "Reactor maintenance building cost (M$)",
                costs_module.c2141,
            )
            po.ocosts(
                self.outfile, "(c2142)", "Warm shop cost (M$)", costs_module.c2142
            )
            po.ocosts(
                self.outfile, "(c215)", "Tritium building cost (M$)", costs_module.c215
            )
            po.ocosts(
                self.outfile,
                "(c216)",
                "Electrical equipment building cost (M$)",
                costs_module.c216,
            )
            po.ocosts(
                self.outfile,
                "(c2171)",
                "Additional buildings cost (M$)",
                costs_module.c2171,
            )
            po.ocosts(
                self.outfile,
                "(c2172)",
                "Control room buildings cost (M$)",
                costs_module.c2172,
            )
            po.ocosts(
                self.outfile,
                "(c2173)",
                "Shop and warehouses cost (M$)",
                costs_module.c2173,
            )
            po.ocosts(
                self.outfile,
                "(c2174)",
                "Cryogenic building cost (M$)",
                costs_module.c2174,
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(c21)", "Total account 21 cost (M$)", costs_module.c21
            )

            po.oshead(self.outfile, "Reactor Systems")
            po.ocosts(
                self.outfile, "(c2211)", "First wall cost (M$)", costs_module.c2211
            )
            if ife_variables.ife != 1:
                if fwbs_variables.iblanket == 4:
                    po.ocosts(
                        self.outfile,
                        "(c22121)",
                        "Blanket lithium-lead cost (M$)",
                        costs_module.c22121,
                    )
                    po.ocosts(
                        self.outfile,
                        "(c22122)",
                        "Blanket lithium cost (M$)",
                        costs_module.c22122,
                    )
                else:
                    po.ocosts(
                        self.outfile,
                        "(c22121)",
                        "Blanket beryllium cost (M$)",
                        costs_module.c22121,
                    )
                    po.ocosts(
                        self.outfile,
                        "(c22122)",
                        "Blanket breeder material cost (M$)",
                        costs_module.c22122,
                    )

                po.ocosts(
                    self.outfile,
                    "(c22123)",
                    "Blanket stainless steel cost (M$)",
                    costs_module.c22123,
                )
                po.ocosts(
                    self.outfile,
                    "(c22124)",
                    "Blanket vanadium cost (M$)",
                    costs_module.c22124,
                )
            else:  # IFE
                po.ocosts(
                    self.outfile,
                    "(c22121)",
                    "Blanket beryllium cost (M$)",
                    costs_module.c22121,
                )
                po.ocosts(
                    self.outfile,
                    "(c22122)",
                    "Blanket lithium oxide cost (M$)",
                    costs_module.c22122,
                )
                po.ocosts(
                    self.outfile,
                    "(c22123)",
                    "Blanket stainless steel cost (M$)",
                    costs_module.c22123,
                )
                po.ocosts(
                    self.outfile,
                    "(c22124)",
                    "Blanket vanadium cost (M$)",
                    costs_module.c22124,
                )
                po.ocosts(
                    self.outfile,
                    "(c22125)",
                    "Blanket carbon cloth cost (M$)",
                    costs_module.c22125,
                )
                po.ocosts(
                    self.outfile,
                    "(c22126)",
                    "Blanket concrete cost (M$)",
                    costs_module.c22126,
                )
                po.ocosts(
                    self.outfile,
                    "(c22127)",
                    "Blanket FLiBe cost (M$)",
                    costs_module.c22127,
                )
                po.ocosts(
                    self.outfile,
                    "(c22128)",
                    "Blanket lithium cost (M$)",
                    costs_module.c22128,
                )

            po.ocosts(
                self.outfile, "(c2212)", "Blanket total cost (M$)", costs_module.c2212
            )
            po.ocosts(
                self.outfile, "(c22131)", "Bulk shield cost (M$)", costs_module.c22131
            )
            po.ocosts(
                self.outfile,
                "(c22132)",
                "Penetration shielding cost (M$)",
                costs_module.c22132,
            )
            po.ocosts(
                self.outfile, "(c2213)", "Total shield cost (M$)", costs_module.c2213
            )
            po.ocosts(
                self.outfile,
                "(c2214)",
                "Total support structure cost (M$)",
                costs_module.c2214,
            )
            po.ocosts(self.outfile, "(c2215)", "Divertor cost (M$)", costs_module.c2215)
            #     if (cost_variables.ifueltyp == 1) :
            #         po.oblnkl(self.outfile)
            #         write(self.outfile,20)
            #     20     format(t2,             'First wall, total blanket and divertor direct costs',/,             t2,'are zero as they are assumed to be fuel costs.')
            #     elif  (cost_variables.ifueltyp == 2) :
            #         po.oblnkl(self.outfile)
            #         write(self.outfile,31)
            # 21     format(t2,             'Initial First wall, total blanket and divertor direct costs',/,             t2,'are in capital and replacemnet are in cost of electricity')

            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile,
                "(c221)",
                "Total account 221 cost (M$)",
                cost_variables.c221,
            )

            if ife_variables.ife != 1:

                po.oshead(self.outfile, "Magnets")

                if tfcoil_variables.i_tf_sup != 1:  # Resistive TF coils
                    if physics_variables.itart == 1:
                        po.ocosts(
                            self.outfile,
                            "(c22211)",
                            "Centrepost costs (M$)",
                            costs_module.c22211,
                        )
                    else:
                        po.ocosts(
                            self.outfile,
                            "(c22211)",
                            "Inboard leg cost (M$)",
                            costs_module.c22211,
                        )

                    po.ocosts(
                        self.outfile,
                        "(c22212)",
                        "Outboard leg cost (M$)",
                        costs_module.c22212,
                    )
                    po.ocosts(
                        self.outfile,
                        "(c2221)",
                        "TF magnet assemblies cost (M$)",
                        costs_module.c2221,
                    )
                else:  # Superconducting TF coils
                    po.ocosts(
                        self.outfile,
                        "(c22211)",
                        "TF coil conductor cost (M$)",
                        costs_module.c22211,
                    )
                    po.ocosts(
                        self.outfile,
                        "(c22212)",
                        "TF coil winding cost (M$)",
                        costs_module.c22212,
                    )
                    po.ocosts(
                        self.outfile,
                        "(c22213)",
                        "TF coil case cost (M$)",
                        costs_module.c22213,
                    )
                    po.ocosts(
                        self.outfile,
                        "(c22214)",
                        "TF intercoil structure cost (M$)",
                        costs_module.c22214,
                    )
                    po.ocosts(
                        self.outfile,
                        "(c22215)",
                        "TF coil gravity support structure (M$)",
                        costs_module.c22215,
                    )
                    po.ocosts(
                        self.outfile,
                        "(c2221)",
                        "TF magnet assemblies cost (M$)",
                        costs_module.c2221,
                    )

                po.ocosts(
                    self.outfile,
                    "(c22221)",
                    "PF coil conductor cost (M$)",
                    costs_module.c22221,
                )
                po.ocosts(
                    self.outfile,
                    "(c22222)",
                    "PF coil winding cost (M$)",
                    costs_module.c22222,
                )
                po.ocosts(
                    self.outfile,
                    "(c22223)",
                    "PF coil case cost (M$)",
                    costs_module.c22223,
                )
                po.ocosts(
                    self.outfile,
                    "(c22224)",
                    "PF coil support structure cost (M$)",
                    costs_module.c22224,
                )
                po.ocosts(
                    self.outfile,
                    "(c2222)",
                    "PF magnet assemblies cost (M$)",
                    costs_module.c2222,
                )
                po.ocosts(
                    self.outfile,
                    "(c2223)",
                    "Vacuum vessel assembly cost (M$)",
                    costs_module.c2223,
                )

                #     if ((physics_variables.itart == 1)and(cost_variables.ifueltyp == 1)) :
                #         po.oblnkl(self.outfile)
                #         write(self.outfile,30)
                # 30        format(t2,                'Centrepost direct cost is zero, as it ',                'is assumed to be a fuel cost.')
                #     elif  ((physics_variables.itart == 1)and(cost_variables.ifueltyp == 2)) :
                #         po.oblnkl(self.outfile)
                #         write(self.outfile,31)
                # 31        format(t2,                'Initial centrepost direct cost in included in capital ',                'cost and replacements are assumed to be a fuel cost.')

                po.oblnkl(self.outfile)
                po.ocosts(
                    self.outfile,
                    "(c222)",
                    "Total account 222 cost (M$)",
                    cost_variables.c222,
                )

            po.oshead(self.outfile, "Power Injection")

            if ife_variables.ife == 1:
                po.ocosts(
                    self.outfile,
                    "(c2231)",
                    "IFE driver system cost (M$)",
                    costs_module.c2231,
                )
            else:
                po.ocosts(
                    self.outfile, "(c2231)", "ECH system cost (M$)", costs_module.c2231
                )
                po.ocosts(
                    self.outfile,
                    "(c2232)",
                    "Lower hybrid system cost (M$)",
                    costs_module.c2232,
                )
                po.ocosts(
                    self.outfile,
                    "(c2233)",
                    "Neutral beam system cost (M$)",
                    costs_module.c2233,
                )

            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(c223)", "Total account 223 cost (M$)", costs_module.c223
            )

            po.oshead(self.outfile, "Vacuum Systems")
            po.ocosts(
                self.outfile,
                "(c2241)",
                "High vacuum pumps cost (M$)",
                costs_module.c2241,
            )
            po.ocosts(
                self.outfile, "(c2242)", "Backing pumps cost (M$)", costs_module.c2242
            )
            po.ocosts(
                self.outfile, "(c2243)", "Vacuum duct cost (M$)", costs_module.c2243
            )
            po.ocosts(self.outfile, "(c2244)", "Valves cost (M$)", costs_module.c2244)
            po.ocosts(
                self.outfile, "(c2245)", "Duct shielding cost (M$)", costs_module.c2245
            )
            po.ocosts(
                self.outfile, "(c2246)", "Instrumentation cost (M$)", costs_module.c2246
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(c224)", "Total account 224 cost (M$)", costs_module.c224
            )

            if ife_variables.ife != 1:
                po.oshead(self.outfile, "Power Conditioning")
                po.ocosts(
                    self.outfile,
                    "(c22511)",
                    "TF coil power supplies cost (M$)",
                    costs_module.c22511,
                )
                po.ocosts(
                    self.outfile,
                    "(c22512)",
                    "TF coil breakers cost (M$)",
                    costs_module.c22512,
                )
                po.ocosts(
                    self.outfile,
                    "(c22513)",
                    "TF coil dump resistors cost (M$)",
                    costs_module.c22513,
                )
                po.ocosts(
                    self.outfile,
                    "(c22514)",
                    "TF coil instrumentation and control (M$)",
                    costs_module.c22514,
                )
                po.ocosts(
                    self.outfile,
                    "(c22515)",
                    "TF coil bussing cost (M$)",
                    costs_module.c22515,
                )
                po.ocosts(
                    self.outfile,
                    "(c2251)",
                    "Total, TF coil power costs (M$)",
                    costs_module.c2251,
                )
                po.ocosts(
                    self.outfile,
                    "(c22521)",
                    "PF coil power supplies cost (M$)",
                    costs_module.c22521,
                )
                po.ocosts(
                    self.outfile,
                    "(c22522)",
                    "PF coil instrumentation and control (M$)",
                    costs_module.c22522,
                )
                po.ocosts(
                    self.outfile,
                    "(c22523)",
                    "PF coil bussing cost (M$)",
                    costs_module.c22523,
                )
                po.ocosts(
                    self.outfile,
                    "(c22524)",
                    "PF coil burn power supplies cost (M$)",
                    costs_module.c22524,
                )
                po.ocosts(
                    self.outfile,
                    "(c22525)",
                    "PF coil breakers cost (M$)",
                    costs_module.c22525,
                )
                po.ocosts(
                    self.outfile,
                    "(c22526)",
                    "PF coil dump resistors cost (M$)",
                    costs_module.c22526,
                )
                po.ocosts(
                    self.outfile,
                    "(c22527)",
                    "PF coil ac breakers cost (M$)",
                    costs_module.c22527,
                )
                po.ocosts(
                    self.outfile,
                    "(c2252)",
                    "Total, PF coil power costs (M$)",
                    costs_module.c2252,
                )
                po.ocosts(
                    self.outfile,
                    "(c2253)",
                    "Total, energy storage cost (M$)",
                    costs_module.c2253,
                )
                po.oblnkl(self.outfile)
                po.ocosts(
                    self.outfile,
                    "(c225)",
                    "Total account 225 cost (M$)",
                    costs_module.c225,
                )

            po.oshead(self.outfile, "Heat Transport System")
            po.ocosts(
                self.outfile,
                "(cpp)",
                "Pumps and piping system cost (M$)",
                costs_module.cpp,
            )
            po.ocosts(
                self.outfile,
                "(chx)",
                "Primary heat exchanger cost (M$)",
                costs_module.chx,
            )
            po.ocosts(
                self.outfile,
                "(c2261)",
                "Total, reactor cooling system cost (M$)",
                costs_module.c2261,
            )
            po.ocosts(
                self.outfile, "(cppa)", "Pumps, piping cost (M$)", costs_module.cppa
            )
            po.ocosts(
                self.outfile,
                "(c2262)",
                "Total, auxiliary cooling system cost (M$)",
                costs_module.c2262,
            )
            po.ocosts(
                self.outfile,
                "(c2263)",
                "Total, cryogenic system cost (M$)",
                costs_module.c2263,
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(c226)", "Total account 226 cost (M$)", costs_module.c226
            )

            po.oshead(self.outfile, "Fuel Handling System")
            po.ocosts(
                self.outfile, "(c2271)", "Fuelling system cost (M$)", costs_module.c2271
            )
            po.ocosts(
                self.outfile,
                "(c2272)",
                "Fuel processing and purification cost (M$)",
                costs_module.c2272,
            )
            po.ocosts(
                self.outfile,
                "(c2273)",
                "Atmospheric recovery systems cost (M$)",
                costs_module.c2273,
            )
            po.ocosts(
                self.outfile,
                "(c2274)",
                "Nuclear building ventilation cost (M$)",
                costs_module.c2274,
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(c227)", "Total account 227 cost (M$)", costs_module.c227
            )

            po.oshead(self.outfile, "Instrumentation and Control")
            po.ocosts(
                self.outfile,
                "(c228)",
                "Instrumentation and control cost (M$)",
                costs_module.c228,
            )

            po.oshead(self.outfile, "Maintenance Equipment")
            po.ocosts(
                self.outfile,
                "(c229)",
                "Maintenance equipment cost (M$)",
                costs_module.c229,
            )

            po.oshead(self.outfile, "Total Account 22 Cost")
            po.ocosts(
                self.outfile, "(c22)", "Total account 22 cost (M$)", costs_module.c22
            )

            po.oshead(self.outfile, "Turbine Plant Equipment")
            po.ocosts(
                self.outfile,
                "(c23)",
                "Turbine plant equipment cost (M$)",
                costs_module.c23,
            )

            po.oshead(self.outfile, "Electric Plant Equipment")
            po.ocosts(
                self.outfile,
                "(c241)",
                "Switchyard equipment cost (M$)",
                costs_module.c241,
            )
            po.ocosts(
                self.outfile, "(c242)", "Transformers cost (M$)", costs_module.c242
            )
            po.ocosts(
                self.outfile,
                "(c243)",
                "Low voltage equipment cost (M$)",
                costs_module.c243,
            )
            po.ocosts(
                self.outfile,
                "(c244)",
                "Diesel backup equipment cost (M$)",
                costs_module.c244,
            )
            po.ocosts(
                self.outfile,
                "(c245)",
                "Auxiliary facilities cost (M$)",
                costs_module.c245,
            )
            po.oblnkl(self.outfile)
            po.ocosts(
                self.outfile, "(c24)", "Total account 24 cost (M$)", costs_module.c24
            )

            po.oshead(self.outfile, "Miscellaneous Plant Equipment")
            po.ocosts(
                self.outfile,
                "(c25)",
                "Miscellaneous plant equipment cost (M$)",
                costs_module.c25,
            )

            po.oshead(self.outfile, "Heat Rejection System")
            po.ocosts(
                self.outfile,
                "(c26)",
                "Heat rejection system cost (M$)",
                costs_module.c26,
            )

            po.oshead(self.outfile, "Plant Direct Cost")
            po.ocosts(
                self.outfile, "(cdirt)", "Plant direct cost (M$)", cost_variables.cdirt
            )

            po.oshead(self.outfile, "Reactor Core Cost")
            po.ocosts(
                self.outfile,
                "(crctcore)",
                "Reactor core cost (M$)",
                cost_variables.crctcore,
            )

            po.oshead(self.outfile, "Indirect Cost")
            po.ocosts(self.outfile, "(c9)", "Indirect cost (M$)", costs_module.cindrt)

            po.oshead(self.outfile, "Total Contingency")
            po.ocosts(
                self.outfile, "(ccont)", "Total contingency (M$)", costs_module.ccont
            )

            po.oshead(self.outfile, "Constructed Cost")
            po.ocosts(
                self.outfile,
                "(concost)",
                "Constructed cost (M$)",
                cost_variables.concost,
            )

            if cost_variables.ireactor == 1:
                po.oshead(self.outfile, "Interest during Construction")
                po.ocosts(
                    self.outfile,
                    "(moneyint)",
                    "Interest during construction (M$)",
                    costs_module.moneyint,
                )

                po.oshead(self.outfile, "Total Capital Investment")
                po.ocosts(
                    self.outfile,
                    "(capcost)",
                    "Total capital investment (M$)",
                    cost_variables.capcost,
                )

    def acc22(self):
        """
        Account 22 : Fusion power island
        author: P J Knight, CCFE, Culham Science Centre
        author: J Morris, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 22 (fusion power island
        - the tokamak itself plus auxiliary power systems, etc.) costs.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        self.acc221()

        #  Account 222 : Magnets
        costs_module.acc222

        #  Account 223 : Power injection
        costs_module.acc223

        #  Account 224 : Vacuum system
        costs_module.acc224

        #  Account 225 : Power conditioning
        costs_module.acc225

        #  Account 226 : Heat transport system
        costs_module.acc2261  # Account 2261 : Reactor cooling system
        costs_module.acc2262  # Account 2262 : Auxiliary component coolin
        costs_module.acc2263  # Account 2263 : Cryogenic system
        costs_module.acc226  # ccount 226  : Total

        #  Account 227 : Fuel handling
        costs_module.acc2271  # Account 2271 : Fuelling system
        costs_module.acc2272  # Account 2272 : Fuel processing and purification
        costs_module.acc2273  # Account 2273 : Atmospheric recovery systems
        costs_module.acc2274  # Account 2274 : Nuclear building ventilation
        costs_module.acc227  # Account 227  : Total

        #  Account 228 : Instrumentation and control
        costs_module.acc228

        #  Account 229 : Maintenance equipment
        costs_module.acc229

        #  Reactor core costs
        cost_variables.crctcore = (
            cost_variables.c221 + cost_variables.c222 + costs_module.c223
        )

        #  Total account 22
        costs_module.c22 = (
            cost_variables.c221
            + cost_variables.c222
            + costs_module.c223
            + costs_module.c224
            + costs_module.c225
            + costs_module.c226
            + costs_module.c227
            + costs_module.c228
            + costs_module.c229
        )

    def acc221(self):
        """
        Account 221 : Reactor
        author: P J Knight, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 221 (reactor) costs.
        These include the first wall, blanket, shield, support structure
        and divertor plates.
        <P>If ifueltyp = 1, the first wall, blanket and divertor costs are
        treated as fuel costs, rather than as capital costs.
        <P>If ifueltyp = 2, the initial first wall, blanket and divertor costs are
        treated as capital costs, and replacemnts are included as fuel costs.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        self.acc2211()

        #  Account 221.2 : Blanket

        self.acc2212()

        #  Account 221.3 : Shield

        costs_module.acc2213

        #  Account 221.4 : Reactor structure

        costs_module.acc2214

        #  Account 221.5 : Divertor

        costs_module.acc2215

        #  Total account 221

        cost_variables.c221 = (
            costs_module.c2211
            + costs_module.c2212
            + costs_module.c2213
            + costs_module.c2214
            + costs_module.c2215
        )

    def acc222(self):
        """
        Account 222 : Magnets, including cryostat
        author: P J Knight, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 222 (magnet) costs,
        including the costs of associated cryostats.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        if ife_variables.ife == 1:
            cost_variables.c222 = 0.0e0
            return

        #  Account 222.1 : TF magnet assemblies

        costs_module.acc2221

        #  Account 222.2 : PF magnet assemblies
        costs_module.acc2222

        #  Account 222.3 : Cryostat

        costs_module.acc2223

        #  Total account 222

        cost_variables.c222 = (
            costs_module.c2221 + costs_module.c2222 + costs_module.c2223
        )

    def acc225(self):
        """
        Account 225 : Power conditioning
        author: P J Knight, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 225 (power conditioning) costs.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        if ife_variables.ife == 1:
            costs_module.c225 = 0.0e0
        else:

            #  Account 225.1 : TF coil power conditioning

            costs_module.acc2251

            #  Account 225.2 : PF coil power conditioning

            costs_module.acc2252

            #  Account 225.3 : Energy storage

            costs_module.acc2253

            #  Total account 225

            costs_module.c225 = (
                costs_module.c2251 + costs_module.c2252 + costs_module.c2253
            )

    def acc21(self):
        """
        Account 21 : Structures and site facilities
        author: P J Knight, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 21 (structures and site
        facilities) costs.
        Building costs are scaled with volume according to algorithms
        developed from TFCX, TFTR, and commercial power plant buildings.
        Costs include equipment, materials and installation labour, but
        no engineering or construction management.
        <P>The general form of the cost algorithm is cost=ucxx*volume**expxx.
        Allowances are used for site improvements and for miscellaneous
        buildings and land costs.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        cmlsa = [0.6800e0, 0.8400e0, 0.9200e0, 1.0000e0]
        exprb = 1.0e0
        #  Account 211 : Site improvements, facilities and land
        #  N.B. Land unaffected by LSA

        costs_module.c211 = (
            cost_variables.csi * cmlsa[cost_variables.lsa - 1] + cost_variables.cland
        )

        #  Account 212 : Reactor building

        costs_module.c212 = (
            1.0e-6
            * cost_variables.ucrb
            * buildings_variables.rbvol**exprb
            * cmlsa[cost_variables.lsa - 1]
        )

        #  Account 213 : Turbine building

        if cost_variables.ireactor == 1:
            costs_module.c213 = cost_variables.cturbb * cmlsa[cost_variables.lsa - 1]
        else:
            costs_module.c213 = 0.0e0

        #  Account 214 : Reactor maintenance and warm shops buildings

        costs_module.c2141 = (
            1.0e-6
            * cost_variables.ucmb
            * buildings_variables.rmbvol**exprb
            * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c2142 = (
            1.0e-6
            * cost_variables.ucws
            * buildings_variables.wsvol**exprb
            * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c214 = costs_module.c2141 + costs_module.c2142

        #  Account 215 : Tritium building

        costs_module.c215 = (
            1.0e-6
            * cost_variables.uctr
            * buildings_variables.triv**exprb
            * cmlsa[cost_variables.lsa - 1]
        )

        #  Account 216 : Electrical equipment building

        costs_module.c216 = (
            1.0e-6
            * cost_variables.ucel
            * buildings_variables.elevol**exprb
            * cmlsa[cost_variables.lsa - 1]
        )

        #  Account 217 : Other buildings
        #  Includes administration, control, shops, cryogenic
        #  plant and an allowance for miscellaneous structures

        costs_module.c2171 = (
            1.0e-6
            * cost_variables.ucad
            * buildings_variables.admvol**exprb
            * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c2172 = (
            1.0e-6
            * cost_variables.ucco
            * buildings_variables.convol**exprb
            * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c2173 = (
            1.0e-6
            * cost_variables.ucsh
            * buildings_variables.shovol**exprb
            * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c2174 = (
            1.0e-6
            * cost_variables.uccr
            * buildings_variables.cryvol**exprb
            * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c217 = (
            costs_module.c2171
            + costs_module.c2172
            + costs_module.c2173
            + costs_module.c2174
        )

        #  Total for Account 21

        costs_module.c21 = (
            costs_module.c211
            + costs_module.c212
            + costs_module.c213
            + costs_module.c214
            + costs_module.c215
            + costs_module.c216
            + costs_module.c217
        )

    def acc2211(self):
        """
        Account 221.1 : First wall
        author: P J Knight, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 221.1 (first wall) costs.
        The first wall cost is scaled linearly with surface area from TFCX.
        If ifueltyp = 1, the first wall cost is treated as a fuel cost,
        rather than as a capital cost.
        If ifueltyp = 2, inital first wall is included as a capital cost,
        and the replacement first wall cost is treated as a fuel costs.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        cmlsa = 0.5000e0, 0.7500e0, 0.8750e0, 1.0000e0

        if ife_variables.ife != 1:
            costs_module.c2211 = (
                1.0e-6
                * cmlsa[cost_variables.lsa - 1]
                * (
                    (cost_variables.ucfwa + cost_variables.ucfws)
                    * build_variables.fwarea
                    + cost_variables.ucfwps
                )
            )
        else:
            costs_module.c2211 = (
                1.0e-6
                * cmlsa[cost_variables.lsa - 1]
                * (
                    cost_variables.ucblss
                    * (
                        ife_variables.fwmatm(1, 1)
                        + ife_variables.fwmatm(2, 1)
                        + ife_variables.fwmatm(3, 1)
                    )
                    + ife_variables.uccarb
                    * (
                        ife_variables.fwmatm(1, 2)
                        + ife_variables.fwmatm(2, 2)
                        + ife_variables.fwmatm(3, 2)
                    )
                    + cost_variables.ucblli2o
                    * (
                        ife_variables.fwmatm(1, 4)
                        + ife_variables.fwmatm(2, 4)
                        + ife_variables.fwmatm(3, 4)
                    )
                    + ife_variables.ucconc
                    * (
                        ife_variables.fwmatm(1, 5)
                        + ife_variables.fwmatm(2, 5)
                        + ife_variables.fwmatm(3, 5)
                    )
                )
            )

        costs_module.c2211 = cost_variables.fkind * costs_module.c2211

        if cost_variables.ifueltyp == 1:
            cost_variables.fwallcst = costs_module.c2211
            costs_module.c2211 = 0.0e0
        elif cost_variables.ifueltyp == 2:
            cost_variables.fwallcst = costs_module.c2211
        else:
            cost_variables.fwallcst = 0.0e0

    def acc2212(self):
        """
        Account 221.2 : Blanket
        author: P J Knight, CCFE, Culham Science Centre
        None
        This routine evaluates the Account 221.2 (blanket) costs.
        If ifueltyp = 1, the blanket cost is treated as a fuel cost,
        rather than as a capital cost.
        If ifueltyp = 2, the initial blanket is included as a capital cost
        and the replacement blanket costs are treated as a fuel cost.
        AEA FUS 251: A User's Guide to the PROCESS Systems Code
        """
        cmlsa = [0.5000e0, 0.7500e0, 0.8750e0, 1.0000e0]

        if ife_variables.ife != 1:

            # iblanket=4 is used for KIT HCLL model. iblanket<4 are all
            # HCPB (CCFE, KIT and CCFE + Shimwell TBR calculation).

            if fwbs_variables.iblanket == 4:
                #  Liquid blanket (LiPb + Li)
                costs_module.c22121 = (
                    1.0e-6 * fwbs_variables.wtbllipb * cost_variables.ucbllipb
                )
                costs_module.c22122 = (
                    1.0e-6 * fwbs_variables.whtblli * cost_variables.ucblli
                )
            else:
                #  Solid blanket (Li2O + Be)
                costs_module.c22121 = (
                    1.0e-6 * fwbs_variables.whtblbe * cost_variables.ucblbe
                )
                if fwbs_variables.iblanket == 2:
                    # KIT model
                    costs_module.c22122 = (
                        1.0e-6 * fwbs_variables.whtblbreed * cost_variables.ucblbreed
                    )
                else:
                    # CCFE model
                    costs_module.c22122 = (
                        1.0e-6 * fwbs_variables.wtblli2o * cost_variables.ucblli2o
                    )

            costs_module.c22123 = (
                1.0e-6 * fwbs_variables.whtblss * cost_variables.ucblss
            )
            costs_module.c22124 = (
                1.0e-6 * fwbs_variables.whtblvd * cost_variables.ucblvd
            )
            costs_module.c22125 = 0.0e0
            costs_module.c22126 = 0.0e0
            costs_module.c22127 = 0.0e0

        else:

            #  IFE blanket; materials present are Li2O, steel, carbon, concrete,
            #  FLiBe and lithium

            costs_module.c22121 = 0.0e0
            costs_module.c22122 = (
                1.0e-6 * fwbs_variables.wtblli2o * cost_variables.ucblli2o
            )
            costs_module.c22123 = (
                1.0e-6 * fwbs_variables.whtblss * cost_variables.ucblss
            )
            costs_module.c22124 = 0.0e0
            costs_module.c22125 = (
                1.0e-6
                * ife_variables.uccarb
                * (
                    ife_variables.blmatm(1, 2)
                    + ife_variables.blmatm(2, 2)
                    + ife_variables.blmatm(3, 2)
                )
            )
            costs_module.c22126 = (
                1.0e-6
                * ife_variables.ucconc
                * (
                    ife_variables.blmatm(1, 5)
                    + ife_variables.blmatm(2, 5)
                    + ife_variables.blmatm(3, 5)
                )
            )
            costs_module.c22127 = 1.0e-6 * ife_variables.ucflib * ife_variables.mflibe
            costs_module.c22128 = (
                1.0e-6 * cost_variables.ucblli * fwbs_variables.whtblli
            )

        costs_module.c22121 = (
            cost_variables.fkind * costs_module.c22121 * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c22122 = (
            cost_variables.fkind * costs_module.c22122 * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c22123 = (
            cost_variables.fkind * costs_module.c22123 * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c22124 = (
            cost_variables.fkind * costs_module.c22124 * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c22125 = (
            cost_variables.fkind * costs_module.c22125 * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c22126 = (
            cost_variables.fkind * costs_module.c22126 * cmlsa[cost_variables.lsa - 1]
        )
        costs_module.c22127 = (
            cost_variables.fkind * costs_module.c22127 * cmlsa[cost_variables.lsa - 1]
        )

        costs_module.c2212 = (
            costs_module.c22121
            + costs_module.c22122
            + costs_module.c22123
            + costs_module.c22124
            + costs_module.c22125
            + costs_module.c22126
            + costs_module.c22127
        )

        if cost_variables.ifueltyp == 1:
            cost_variables.blkcst = costs_module.c2212
            costs_module.c2212 = 0.0e0
        elif cost_variables.ifueltyp == 2:
            cost_variables.blkcst = costs_module.c2212
        else:
            cost_variables.blkcst = 0.0e0
