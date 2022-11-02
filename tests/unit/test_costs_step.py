"""Unit tests for costs_step.f90."""
from process.costs_step import CostsStep
from process.fortran import tfcoil_variables as tfv
from process.fortran import cost_variables as cv
from process.fortran import buildings_variables as bldgsv
from process.fortran import heat_transport_variables as htv
from process.fortran import physics_variables as pv
from process.fortran import build_variables as bv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import times_variables as tv

import numpy as np
import pytest

# step_ref cost array values taken from cost_variables_module
step_ref = np.array(
    [
        3.0,
        3.0e-1,
        1.115e1,
        1.5744e2,
        3.592e1,
        7.96,
        9.16,
        3.26,
        5.369e1,
        1.88,
        6.6e-1,
        8.63,
        3.1,
        2.05,
        8.7e-1,
        8.7e-1,
        9.1e-1,
        3.1e-1,
        1.81,
        8.236e1,
        1.8607e2,
        1.2572e2,
        3.46e1,
        7.25,
        4.0,
        3.349e1,
        5.274e1,
        4.86,
        5.29e1,
        2.45,
        2.82,
        1.676e1,
        6.984e1,
        7.7,
        3.6,
        2.8,
        8.0e-1,
        1.7,
        1.8,
        1.3,
        3.86e1,
        3.83e1,
        0.0,
        2.4e-1,
        8.0e-2,
        0.0,
        2.0,
        1.97,
        1.16,
        2.341e1,
        7.733e1,
        4.37,
        4.434e1,
        1.918e1,
        9.39,
        5.084e1,
        8.7,
        1.239e1,
        1.704e1,
        7.8,
        2.11,
        1.74e1,
        3.599e1,
        8.2,
        1.568e1,
        1.235e1,
        6.22,
        7.5e-1,
        19.21,
        12.85,
    ]
)


@pytest.fixture
def costs_step(monkeypatch):
    """Fixture to mock commonly used dependencies in cost subroutines.

    Create CostsStep instance and mock Fortran module variables to aid testing.
    The values are intended to be realistic.
    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :return: CostsStep model object
    :rtype: process.costs_step.CostsStep
    """
    costs_step = CostsStep()

    # Mock commonly-used Fortran module vars for testing
    monkeypatch.setattr(cv, "step_ref", step_ref)
    monkeypatch.setattr(cv, "step_con", 1.5e-1)
    monkeypatch.setattr(bldgsv, "efloor", 1e4)
    monkeypatch.setattr(htv, "pgrossmw", 5e2)
    monkeypatch.setattr(costs_step, "vfi", 6.737e3)
    monkeypatch.setattr(costs_step, "vfi_star", 6.737e3)
    monkeypatch.setattr(costs_step, "pth", 4.15e3)
    monkeypatch.setattr(costs_step, "ptherm_star", 4.15e3)
    monkeypatch.setattr(costs_step, "rmajor_star", 7.0)
    monkeypatch.setattr(costs_step, "rminor_star", 7 / 3.6)
    monkeypatch.setattr(tfv, "n_tf_turn", 104.6)
    monkeypatch.setattr(tfv, "tfleng", 34.63)
    # vfi values taken from Starfire reference in costs_step_module

    return costs_step


def test_init_costs_step():
    """Test initialisation of variables"""
    # Assert module vars are initialised correctly
    costs_step_object = CostsStep()
    assert costs_step_object.step20 == 0
    assert costs_step_object.step21 == 0
    assert costs_step_object.step22 == 0
    assert costs_step_object.step23 == 0
    assert costs_step_object.step24 == 0
    assert costs_step_object.step25 == 0
    assert costs_step_object.step27 == 0
    assert costs_step_object.step91 == 0
    assert costs_step_object.step92 == 0
    assert costs_step_object.step93 == 0
    assert costs_step_object.fwblkcost == 0
    assert costs_step_object.vfi == 0
    assert costs_step_object.vfi_star == 0
    assert costs_step_object.ptherm_star == 0
    assert costs_step_object.rmajor_star == 0
    assert costs_step_object.rminor_star == 0
    assert costs_step_object.pth == 0


def test_costs_step(monkeypatch, costs_step):
    """Test the costs_step subroutine

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    monkeypatch.setattr(cv, "cdirt", 0.0)
    monkeypatch.setattr(cv, "concost", 0.0)
    monkeypatch.setattr(bv, "r_tf_outboard_mid", 10.0)
    monkeypatch.setattr(bv, "tfthko", 10.0)
    monkeypatch.setattr(bv, "hpfu", 10.0)
    monkeypatch.setattr(bv, "hmax", 10.0)
    monkeypatch.setattr(bv, "tfcth", 10.0)
    monkeypatch.setattr(pv, "powfmw", 10.0)
    monkeypatch.setattr(fwbsv, "emultmw", 10.0)
    monkeypatch.setattr(htv, "pinjwp", 10.0)
    monkeypatch.setattr(costs_step, "vfi", 5e3)
    monkeypatch.setattr(costs_step, "vfi_star", 6.737e3)

    costs_step.run()

    # Test that module variables are calculated correctly
    obs_vfi = costs_step.vfi
    exp_vfi = 2.120575e4
    assert pytest.approx(obs_vfi) == exp_vfi
    obs_pth = costs_step.pth
    exp_pth = 30.0
    assert pytest.approx(obs_pth) == exp_pth

    # Test that module variables are assigned correctly
    assert costs_step.vfi_star == 6.737e3
    assert costs_step.ptherm_star == 4.15e3
    assert costs_step.rmajor_star == 7.0e0
    assert costs_step.rminor_star == 7.0 / 3.6

    # Total plant direct cost with remote handling
    exp = 7150.8739399
    obs = cv.cdirt
    assert pytest.approx(obs) == exp

    # Constructed cost
    exp_concost = 12692.8012433
    obs_concost = cv.concost
    assert pytest.approx(obs_concost) == exp_concost


@pytest.mark.parametrize(
    "isitetype, isiteaccomm, igridconn, irailaccess, exp",
    ((0, 0, 0, 0, 5.6742341e3), (1, 1, 1, 1, 5.5921241e3), (2, 0, 0, 0, 5.5803941e3)),
)
def test_bldg_and_site_service_infra_costs(
    monkeypatch, costs_step, isitetype, isiteaccomm, igridconn, irailaccess, exp
):
    """Validate sum of cost account 21.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    monkeypatch.setattr(costs_step, "step21", 0.0)
    monkeypatch.setattr(cv, "isitetype", isitetype)
    monkeypatch.setattr(cv, "isiteaccomm", isiteaccomm)
    monkeypatch.setattr(cv, "igridconn", igridconn)
    monkeypatch.setattr(cv, "irailaccess", irailaccess)

    # Run and assert result in M$
    costs_step.bldg_and_site_service_infra_costs()
    obs = costs_step.bldg_and_site_service_infra_cost
    assert pytest.approx(obs) == exp


def test_reactor_plant_equip_costs(monkeypatch, costs_step):
    """Validate sum of cost account 22.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cs, "step22", 0.0)
    # monkeypatch.setattr(pv, "rmajor", 1e2)
    # monkeypatch.setattr(pv, "rminor", 1e1)
    # monkeypatch.setattr(cs, "rmajor_star", 1e3)
    # monkeypatch.setattr(cs, "rminor_star", 1e3)

    # # Run and assert result in M$
    # cs.step_a22(0, 0)
    # exp = 676.5516457
    # obs = cs.step22
    monkeypatch.setattr(costs_step, "step22", 0.0)
    monkeypatch.setattr(bv, "fwarea", 9.42e2)
    monkeypatch.setattr(pv, "rmajor", 1e2)
    monkeypatch.setattr(pv, "rminor", 1e1)
    monkeypatch.setattr(costs_step, "rmajor_star", 1e3)
    monkeypatch.setattr(costs_step, "rminor_star", 1e3)
    monkeypatch.setattr(costs_step, "vfi", 6.737e3)
    monkeypatch.setattr(costs_step, "vfi_star", 6.737e3)

    # Run and assert result in M$
    costs_step.reactor_plant_equip_costs()
    exp = 1207.1446889
    obs = costs_step.reactor_plant_equip_cost
    assert pytest.approx(obs) == exp


@pytest.mark.parametrize("fkind, exp", ((1, 1573.1259947), (0.5, 1001.8129973)))
def test_reactor_equip_costs(monkeypatch, costs_step, fkind, exp):
    """Validate sum of cost account 22.01.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine: increase is value of step2201
    monkeypatch.setattr(costs_step, "step22", 0.0)
    monkeypatch.setattr(cv, "step_ref", np.zeros(70, order="F"))
    monkeypatch.setattr(cv, "fkind", fkind)

    # Only mock used array elements
    cv.step_ref[23] = 5.9e1
    cv.step_ref[24] = 3.254e1
    cv.step_ref[25] = 2.7254e2
    cv.step_ref[26] = 4.292e2
    cv.step_ref[27] = 3.955e1
    cv.step_ref[28] = 4.305e2
    cv.step_ref[29] = 1.994e1
    cv.step_ref[30] = 2.295e1
    cv.step_ref[31] = 1.364e2
    monkeypatch.setattr(cv, "ifueltyp", 0)
    monkeypatch.setattr(cv, "fcdfuel", 0.5)
    monkeypatch.setattr(pv, "rmajor", 6.0)
    monkeypatch.setattr(pv, "rminor", 3.0)
    monkeypatch.setattr(costs_step, "rmajor_star", 7.0)
    monkeypatch.setattr(costs_step, "rminor_star", 1.9)

    # exp2 = 1.0199574292e1
    step2201, spares = costs_step.reactor_equip_costs()
    assert pytest.approx(step2201) == exp
    # assert pytest.approx(spares) == exp2


def test_turbine_plant_equip_costs(monkeypatch, costs_step):
    """Validate sum of cost account 23.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(costs_step, "step23", 0.0)

    costs_step.turbine_plant_equip_costs()
    exp = 3.967150e2
    obs = costs_step.turbine_plant_equip_cost
    assert pytest.approx(obs) == exp


def test_coelc_step(monkeypatch, costs_step):
    """Test electricity cost calculations
    :param monkeypatch: fixture for mocking
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    monkeypatch.setattr(cv, "coecap", 0.0)
    monkeypatch.setattr(cv, "coefuelt", 0.0)
    monkeypatch.setattr(cv, "coeoam", 0.0)
    monkeypatch.setattr(htv, "pnetelmw", 1e5)
    monkeypatch.setattr(cv, "cfactr", 10.0)
    monkeypatch.setattr(tv, "tburn", 10.0)
    monkeypatch.setattr(tv, "tcycle", 5.0)
    monkeypatch.setattr(cv, "concost", 10.0)
    monkeypatch.setattr(cv, "fcap0", 10.0)
    monkeypatch.setattr(cv, "fcr0", 10.0)
    monkeypatch.setattr(cv, "discount_rate", 0.5)
    monkeypatch.setattr(fwbsv, "bktlife", 10.0)
    monkeypatch.setattr(costs_step, "fwblkcost", 10.0)
    monkeypatch.setattr(cv, "fcap0cp", 10.0)
    monkeypatch.setattr(cv, "divlife", 2.0)
    monkeypatch.setattr(cv, "divcst", 2.0)
    monkeypatch.setattr(pv, "itart", 1.0)
    monkeypatch.setattr(cv, "cplife", 3.0)
    monkeypatch.setattr(cv, "cpstcst", 10.0)
    monkeypatch.setattr(cv, "cdrlife", 5.0)
    monkeypatch.setattr(cv, "ifueltyp", 1.0)
    monkeypatch.setattr(cv, "cdcost", 5.0)
    monkeypatch.setattr(cv, "fcdfuel", 0.5)
    monkeypatch.setattr(cv, "step_ucoam", 10.0)
    monkeypatch.setattr(cv, "ucfuel", 5.0)
    monkeypatch.setattr(pv, "fhe3", 5.0)
    monkeypatch.setattr(pv, "wtgpd", 5.0)
    monkeypatch.setattr(cv, "uche3", 5.0)
    monkeypatch.setattr(cv, "step_ucwst", 10.0)
    monkeypatch.setattr(cv, "decomf", 0.5)
    monkeypatch.setattr(cv, "dintrt", 5.0)
    monkeypatch.setattr(cv, "tlife", 10.0)
    monkeypatch.setattr(cv, "dtlife", 10.0)

    # Test that coe is calculated correctly
    costs_step.coelc_step()
    expected_coe = 1.0369639053239339e-1
    observed_coe = cv.coe
    assert pytest.approx(observed_coe) == expected_coe

    expected_moneyinit = 90
    assert pytest.approx(cv.moneyint) == expected_moneyinit

    expected_capcost = 100
    assert pytest.approx(cv.capcost) == expected_capcost

    expected_coecap = 0.05703973
    assert pytest.approx(cv.coecap) == expected_coecap

    expected_coeoam = 0.00520699
    assert pytest.approx(cv.coeoam) == expected_coeoam

    expected_coefuelt = 0.03859768
    assert pytest.approx(cv.coefuelt) == expected_coefuelt
