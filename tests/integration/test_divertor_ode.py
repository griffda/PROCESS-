import numpy
import pytest

from process.main import SingleRun
from process.fortran import (
    constants,
    physics_variables,
    div_kal_vars,
    impurity_radiation_module,
    divertor_ode,
    divertor_variables,
)


def test_divertor_kallenbach():
    """An integration test for the Kallenbach Divertor
    using inputs from the original 2016 paper.
    """

    # Sets the PROCESS default values
    SingleRun.init_module_vars()

    # Prepare data as per Kallenbach paper
    rmajor = 8.0e0
    rminor = 2.75e0
    bt = 4.00972e0 * (rmajor + rminor) / rmajor
    plascur = 1.33542e0 * (2.0e0 * numpy.pi * rminor) / constants.rmu0
    q = 3.0e0
    t_target = 2.3e0
    q_target_total = 4.175e6
    target_angle = 30.0e0
    b_pol = 0.956e0
    physics_variables.tesep = 0.298
    div_kal_vars.target_spread = 7.0e-3
    div_kal_vars.netau_sol = 0.5e0
    div_kal_vars.lambda_q_omp = 0.002e0

    div_kal_vars.lcon_factor = 100.0e0 / (
        0.395e0 * numpy.pi * 3.0e0 * 8.0e0 / 0.002e0**0.196
    )

    for i in range(1, impurity_radiation_module.nimp):
        impurity_radiation_module.impurity_arr_frac[i] = 0

    # Set the impurity array fraction of Nitrogen
    # gives 0.04 in SOL, as in Kallenbach paper
    impurity_radiation_module.impurity_arr_frac[4] = 8.0e-3

    # Define expected returns
    expected_psep_kallenbach = pytest.approx(150821564.29063162)
    expected_teomp = pytest.approx(260.5429755492756)
    expected_neomp = pytest.approx(4.5484725621268275e19)
    expected_fmom = pytest.approx(0.5044034672505998)
    expected_hldiv = pytest.approx(6.982882836407337)

    # Run model
    psep_kallenbach, teomp, neomp = divertor_ode.divertor_kallenbach(
        rmajor=rmajor,
        rminor=rminor,
        bt=bt,
        plascur=plascur,
        q=q,
        verboseset=False,
        ttarget=t_target,
        qtargettotal=q_target_total,
        targetangle=target_angle,
        unit_test=False,
        bp=b_pol,
        outfile=constants.nout,
        iprint=0,
    )

    # Compare expected and actual values

    assert psep_kallenbach == expected_psep_kallenbach
    assert teomp == expected_teomp
    assert neomp == expected_neomp
    assert div_kal_vars.fmom == expected_fmom
    assert divertor_variables.hldiv == expected_hldiv
