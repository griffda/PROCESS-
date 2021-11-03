from process import fortran as ft


class Caller:
    """Calls physics and engineering models."""

    def __init__(self, models):
        """Initialise with Python model objects.

        :param models: physics and engineering model objects
        :type models: process.main.Models
        """
        self.models = models

    def call_models(self, xc, nvars):
        """Call the physics and engineering models.

        This method is the principal caller of all the physics and
        engineering models. Some are Fortran subroutines within modules, others
        will be methods on Python model objects.
        :param xc: Array of iteration variables, shape (ipnvars)
        :type xc: numpy.array
        :param nvars: Number of active iteration variables
        :type nvars: int
        """
        # Increment the call counter
        ft.numerics.ncalls = ft.numerics.ncalls + 1

        # Convert variables
        ft.define_iteration_variables.convxc(xc, nvars)

        # Perform the various function calls
        # Stellarator caller
        if ft.stellarator_variables.istell != 0:
            ft.stellarator_module.stcall()
            # TODO Is this return safe?
            return

        # Inertial Fusion Energy calls
        if ft.ife_variables.ife != 0:
            ft.ife_module.ifecll()
            return

        # Tokamak calls
        # Plasma geometry model
        ft.plasma_geometry_module.geomty()

        # Machine Build Model
        # Radial build
        ft.build_module.radialb(ft.constants.nout, 0)

        # Vertical build
        ft.build_module.vbuild(ft.constants.nout, 0)

        ft.physics_module.physics()

        # call build subroutines again if PLASMOD used, issue #650
        if ft.physics_variables.ipedestal == 3:
            # Radial build
            ft.build_module.radialb(ft.constants.nout, 0)

            # Vertical build
            ft.build_module.vbuild(ft.constants.nout, 0)

        # startup model (not used)
        # call startup(ft.constants.nout,0)  !  commented-out for speed reasons

        # Toroidal field coil model
        ft.tfcoil_module.tfcoil(ft.constants.nout, 0)

        # Toroidal field coil superconductor model
        if ft.tfcoil_variables.i_tf_sup == 1:
            ft.sctfcoil_module.tfspcall(ft.constants.nout, 0)

        # Poloidal field and Central Solenoid model
        ft.pfcoil_module.pfcoil()

        # Poloidal field coil inductance calculation
        ft.pfcoil_module.induct(ft.constants.nout, 0)

        # Volt-second capability of PF coil set
        ft.pfcoil_module.vsec()

        # Pulsed reactor model
        ft.pulse_module.pulse(ft.constants.nout, 0)

        # Blanket model
        """Blanket switch values
        No.  |  model
        ---- | ------
        1    |  CCFE HCPB model
        2    |  KIT HCPB model
        3    |  CCFE HCPB model with Tritium Breeding Ratio calculation
        4    |  KIT HCLL model
        """
        if ft.fwbs_variables.iblanket == 1:
            # CCFE HCPB model
            ft.ccfe_hcpb_module.ccfe_hcpb(ft.constants.nout, 0)
        elif ft.fwbs_variables.iblanket == 2:
            # KIT HCPB model
            ft.kit_hcpb_module.kit_hcpb(ft.constants.nout, 0)
        elif ft.fwbs_variables.iblanket == 3:
            # CCFE HCPB model with Tritium Breeding Ratio calculation
            ft.ccfe_hcpb_module.ccfe_hcpb(ft.constants.nout, 0)
            ft.fwbs_variables.tbr = ft.ccfe_hcpb_module.tbr_shimwell(
                ft.constants.nout,
                0,
                ft.fwbs_variables.breeder_f,
                ft.fwbs_variables.li6enrich,
                ft.fwbs_variables.iblanket_thickness,
            )
        elif ft.fwbs_variables.iblanket == 4:
            # KIT HCLL model
            ft.kit_hcll_module.kit_hcll(ft.constants.nout, 0)

        # Divertor Model
        if ft.global_variables.verbose == 1:
            verbose_logical = True
        else:
            verbose_logical = False
            # TODO These aren't used

        # New divertor model
        if ft.div_kal_vars.kallenbach_switch == 1:
            (
                ft.div_kal_vars.psep_kallenbach,
                ft.div_kal_vars.teomp,
                ft.div_kal_vars.neomp,
            ) = ft.divertor_ode.divertor_kallenbach(
                rmajor=ft.physics_variables.rmajor,
                rminor=ft.physics_variables.rminor,
                bt=ft.physics_variables.bt,
                plascur=ft.physics_variables.plascur,
                q=ft.physics_variables.q,
                verboseset=verbose_logical,
                ttarget=ft.div_kal_vars.ttarget,
                qtargettotal=ft.div_kal_vars.qtargettotal,
                targetangle=ft.div_kal_vars.targetangle,
                unit_test=False,
                bp=ft.physics_variables.bp,
                outfile=ft.constants.nout,
                iprint=0,
            )
        elif ft.div_kal_vars.kallenbach_switch == 0:
            # Old Divertor Model ! Comment this out MDK 30/11/16
            ft.divertor_module.divcall(ft.constants.nout, 0)

        # Structure Model
        ft.structure_module.strucall(ft.constants.nout, 0)

        # Tight aspect ratio machine model
        if ft.physics_variables.itart == 1 and ft.tfcoil_variables.i_tf_sup != 1:
            ft.tfcoil_module.cntrpst(ft.constants.nout, 0)

        # Toroidal field coil power model
        ft.power_module.tfpwr(ft.constants.nout, 0)

        # Poloidal field coil power model
        ft.power_module.pfpwr(ft.constants.nout, 0)

        # Plant heat transport part 1
        ft.power_module.power1()

        # Vacuum model
        ft.vacuum_module.vaccall(ft.constants.nout, 0)

        # Buildings model
        ft.buildings_module.bldgcall(ft.constants.nout, 0)

        # Plant AC power requirements
        ft.power_module.acpow(ft.constants.nout, 0)

        # Plant heat transport pt 2 & 3
        ft.power_module.power2(ft.constants.nout, 0)

        ft.power_module.power3(ft.constants.nout, 0)

        # Availability model
        """Availability switch values
        No.  |  model
        ---- | ------
        0    |  Input value for cfactr
        1    |  Ward and Taylor model (1999)
        2    |  Morris model (2015)
        """
        if ft.cost_variables.iavail > 1:
            ft.availability_module.avail_2(ft.constants.nout, 0)  # Morris model (2015)
        else:
            ft.availability_module.avail(
                ft.constants.nout, 0
            )  # Taylor and Ward model (1999)

        # Water usage in secondary cooling system
        ft.water_use_module.waterusecall(ft.constants.nout, 0)

        # Costs model
        """Cost switch values
        No.  |  model
        ---- | ------
        0    |  1990 costs model
        1    |  2015 Kovari model
        2    |  2019 STEP model
        """
        if ft.cost_variables.cost_model == 0:
            ft.costs_module.costs(ft.constants.nout, 0)
        elif ft.cost_variables.cost_model == 1:
            ft.costs_2015_module.costs_2015(0, 0)
        elif ft.cost_variables.cost_model == 2:
            self.models.costs_step.run()

        # FISPACT and LOCA model (not used)
        # if ft.physics_variables.ifispact == 1:
        #     ft.fispac(0)
        #     ft.loca(ft.constants.nout,0)
