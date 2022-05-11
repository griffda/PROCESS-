"""Contains the kallenbach scan routine.

This routine runs the kallenbach scan over Process
before exiting, this is instead of running a full
Process run (ie VMCON won't run).
"""

import logging
import numpy
from process.fortran import (
    physics_variables,
    plasma_geometry_module,
    constants,
    physics_module,
    process_output as po,
    div_kal_vars,
    divertor_ode,
)

logger = logging.getLogger(__name__)
# Logging handler for console output
s_handler = logging.StreamHandler()
s_handler.setLevel(logging.INFO)
logger.addHandler(s_handler)


def kallenbach_scan():
    """Perform a scan over the divertor_kallenbach routine.

    Below is an example input file, using values from the kallenbach paper:

    kallenbach_scan_IN.DAT
    ```
    kallenbach_scan_switch = 1
    kallenbach_scan_var = 0
    kallenbach_scan_start = 900
    kallenbach_scan_end = 1000
    kallenbach_scan_num = 10

    rmajor = 8.0
    aspect = 2.91
    bt = 5.38806125
    q = 3.0
    kappa = 0.99
    triang = 0.6

    target_spread = 7.0D-3
    lambda_q_omp = 0.002D0
    netau_sol = 0.5
    targetangle = 2.3D0
    qtargettotal = 4.175D6
    ```
    """
    physics_variables.rminor = physics_variables.rmajor / physics_variables.aspect

    xi, thetai, xo, thetao = plasma_geometry_module.xparam(
        physics_variables.rminor, physics_variables.kappa, physics_variables.triang
    )

    physics_variables.pperim = 2 * (xo * thetao + xi * thetai)

    physics_variables.plascur = (
        1.33542 * (2 * numpy.pi * physics_variables.rminor) / constants.rmu0
    )

    b_pol = physics_module.bpol(
        physics_variables.itart,
        physics_variables.plascur,
        physics_variables.q,
        physics_variables.aspect,
        physics_variables.bt,
        physics_variables.kappa,
        physics_variables.triang,
        physics_variables.pperim,
    )

    po.oheadr(constants.nout, "Divertor: Kallenbach 1D Model - SCANS - ")
    po.ovarin(
        constants.nout,
        "Number of scan points",
        "(isweep)",
        div_kal_vars.kallenbach_scan_num,
    )
    po.ovarin(
        constants.nout,
        "Scanning variable number",
        "(kallenbach_scan_var)",
        div_kal_vars.kallenbach_scan_var,
    )
    po.ovarre(constants.nout, "Major radius [m]", "(rmajor)", physics_variables.rmajor)
    po.ovarre(constants.nout, "Aspect ratio", "(aspect)", physics_variables.aspect)
    po.ovarre(constants.nout, "Toroidal field [T]", "(bt)", physics_variables.bt)
    po.ovarre(
        constants.nout, "Plasma current [A]", "(plascur)", physics_variables.plascur
    )
    po.ovarre(constants.nout, "q95", "(q)", physics_variables.q)
    po.ovarre(constants.nout, "Plasma elongation", "(kappa)", physics_variables.kappa)
    po.ovarre(
        constants.nout, "Plasma triangularity", "(triang)", physics_variables.triang
    )
    po.ovarre(
        constants.nout,
        "Increase in SOL power fall-off length due to spreading [m]",
        "(target_spread)",
        div_kal_vars.target_spread,
    )
    po.ovarre(
        constants.nout,
        "SOL power fall-off length at the outer midplane [m]",
        "(lambda_q_omp)",
        div_kal_vars.lambda_q_omp,
    )
    po.ovarre(
        constants.nout,
        "Describing departure from local ionisation equil. in SOL [ms;1e20/m3]",
        "(netau_sol)",
        div_kal_vars.netau_sol,
    )
    po.ovarre(
        constants.nout,
        "Angle between flux surface and divertor target [deg]",
        "(targetangle)",
        div_kal_vars.targetangle,
    )
    po.ovarre(
        constants.nout,
        "Power density on target including surface recombination [w/m2]",
        "(qtargettotal)",
        div_kal_vars.qtargettotal,
    )

    variables = (
        "ttarget",
        "qtargettotal",
        "targetangle",
        "lambda_q_omp",
        "netau_sol",
    )

    try:
        target_variable = variables[div_kal_vars.kallenbach_scan_var]
    except IndexError:
        logger.error(
            f"Unrecognised Kallenbach scan variable, valid variables range from 0-{len(variables)-1} inclusive"
        )
        raise

    setattr(div_kal_vars, target_variable, div_kal_vars.kallenbach_scan_start)
    logger.info(f"Performing a Kallenbach scan on {target_variable}")

    step = (
        div_kal_vars.kallenbach_scan_end - div_kal_vars.kallenbach_scan_start
    ) / div_kal_vars.kallenbach_scan_num

    for i in range(div_kal_vars.kallenbach_scan_num):
        po.oblnkl(constants.nout)
        po.ovarin(constants.nout, "Scan point number", "(iscan)", i + 1)

        divertor_ode.divertor_kallenbach(
            rmajor=physics_variables.rmajor,
            rminor=physics_variables.rminor,
            bt=physics_variables.bt,
            plascur=physics_variables.plascur,
            q=physics_variables.q,
            verboseset=False,
            ttarget=div_kal_vars.ttarget,
            qtargettotal=div_kal_vars.qtargettotal,
            targetangle=div_kal_vars.targetangle,
            unit_test=False,
            bp=b_pol,
            outfile=constants.nout,
            iprint=1,
        )

        if div_kal_vars.kallenbach_scan_var == 0:
            div_kal_vars.ttarget += step
        else:
            setattr(div_kal_vars, target_variable, step)
