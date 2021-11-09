from process import fortran as ft


def write(models, outfile):
    """Write the results to the main output file (OUT.DAT).

    Write the program results to a file, in a tidy format.

    AEA FUS 251: A User's Guide to the PROCESS Systems Code
    :param models: physics and engineering model objects
    :type models: process.main.Models
    :param outfile: Fortran output unit identifier
    :type outfile: int
    """
    # Turn on error reporting
    # (warnings etc. encountered in previous iterations may have cleared themselves
    # during the solution process)
    ft.error_handling.errors_on = True

    # Call stellarator output routine instead if relevant
    if ft.stellarator_variables.istell != 0:
        ft.stellarator_module.stout(outfile)
        return

    #  Call IFE output routine instead if relevant
    if ft.ife_variables.ife != 0:
        ft.ife_module.ifeout(outfile)
        return

    # Costs model
    # Cost switch values
    # No.  |  model
    # ---- | ------
    # 0    |  1990 costs model
    # 1    |  2015 Kovari model
    # 2    |  2019 STEP model

    if ft.cost_variables.cost_model == 0:
        ft.costs_module.costs(outfile, 1)
    elif ft.cost_variables.cost_model == 1:
        ft.costs_2015_module.costs_2015(outfile, 1)
    elif ft.cost_variables.cost_model == 2:
        models.costs_step.output()

    # Availability model
    # Availability switch values
    # No.  |  model
    # ---- | ------
    # 0    |  Input value for cfactr
    # 1    |  Ward and Taylor model (1999)
    # 2    |  Morris model (2015)

    if ft.cost_variables.iavail > 1:
        ft.availability_module.avail_2(outfile, 1)  # Morris model (2015)
    else:
        ft.availability_module.avail(outfile, 1)  # Taylor and Ward model (1999)

    # Writing the output from physics.f90 into OUT.DAT + MFILE.DAT
    ft.physics_module.outplas(outfile)

    # Writing
    if ft.physics_variables.ipedestal == 2 or ft.physics_variables.ipedestal == 3:
        ft.plasmod_module.outputplasmod(outfile)

    # TODO what is this? not in caller.f90
    ft.physics_module.igmarcal(outfile)

    # TODO what is this? Not in caller.f90?
    ft.current_drive_module.cudriv(outfile, 1)

    # Pulsed reactor model
    ft.pulse_module.pulse(outfile, 1)
    ft.physics_module.outtim(outfile)

    # Divertor Model
    if ft.global_variables.verbose == 1:
        verbose_logical = True
    else:
        verbose_logical = False

    ft.process_output.ovarin(
        ft.constants.mfile,
        "kallenbach_switch",
        "(kallenbach_switch)",
        ft.div_kal_vars.kallenbach_switch,
    )
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
            iprint=1,
        )

    else:
        # Old Divertor Model: Comment this out MDK 30/11/16
        ft.divertor_module.divcall(outfile, 1)

    # Machine Build Model
    # Radial build
    ft.build_module.radialb(outfile, 1)

    # Vertical build
    ft.build_module.vbuild(outfile, 1)

    # Toroidal field coil model
    ft.tfcoil_module.tfcoil(outfile, 1)

    # Toroidal field coil superconductor model
    if ft.tfcoil_variables.i_tf_sup == 1:
        ft.sctfcoil_module.tfspcall(outfile, 1)

    # Tight aspect ratio machine model
    if ft.physics_variables.itart == 1 and ft.tfcoil_variables.i_tf_sup != 1:
        ft.tfcoil_module.cntrpst(outfile, 1)

    # Poloidal field coil model !
    ft.pfcoil_module.outpf(outfile)

    # TODO what is outvolt?
    ft.pfcoil_module.outvolt(outfile)

    # Structure Model
    ft.structure_module.strucall(outfile, 1)

    # Poloidal field coil inductance calculation
    ft.pfcoil_module.induct(outfile, 1)

    # Blanket model
    # Blanket switch values
    # No.  |  model
    # ---- | ------
    # 1    |  CCFE HCPB model
    # 2    |  KIT HCPB model
    # 3    |  CCFE HCPB model with Tritium Breeding Ratio calculation
    # 4    |  KIT HCLL model
    if ft.fwbs_variables.iblanket == 1:
        # CCFE HCPB model
        ft.ccfe_hcpb_module.ccfe_hcpb(ft.constants.nout, 1)
    elif ft.fwbs_variables.iblanket == 2:
        # KIT HCPB model
        ft.kit_hcpb_module.kit_hcpb(ft.constants.nout, 1)
    elif ft.fwbs_variables.iblanket == 3:
        # CCFE HCPB model with Tritium Breeding Ratio calculation
        ft.ccfe_hcpb_module.ccfe_hcpb(ft.constants.nout, 1)
        ft.fwbs_variables.tbr = ft.ccfe_hcpb_module.tbr_shimwell(
            ft.constants.nout,
            1,
            ft.fwbs_variables.breeder_f,
            ft.fwbs_variables.li6enrich,
            ft.fwbs_variables.iblanket_thickness,
        )
    elif ft.fwbs_variables.iblanket == 4:
        # KIT HCLL model
        ft.kit_hcll_module.kit_hcll(ft.constants.nout, 1)

    # FISPACT and LOCA model (not used)
    # if (ifispact == 1) then
    #   call fispac(0)
    #   call fispac(1)
    #   call loca(outfile,0)
    #   call loca(outfile,1)
    # end if

    # Toroidal field coil power model
    ft.power_module.tfpwr(outfile, 1)

    # Poloidal field coil power model !
    ft.power_module.pfpwr(outfile, 1)

    # Vacuum model
    ft.vacuum_module.vaccall(outfile, 1)

    # Buildings model
    ft.buildings_module.bldgcall(outfile, 1)

    # Plant AC power requirements
    ft.power_module.acpow(outfile, 1)

    # Plant heat transport pt 2 & 3
    ft.power_module.power2(outfile, 1)
    ft.power_module.power3(ft.constants.nout, 1)

    # Water usage in secondary cooling system
    ft.water_use_module.waterusecall(outfile, 1)
