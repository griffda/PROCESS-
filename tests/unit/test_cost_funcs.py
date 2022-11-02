from process.cost_funcs import CostModelBase
from process.fortran import tfcoil_variables as tfv
from process.fortran import cost_variables as cv
from process.fortran import buildings_variables as bldgsv
from process.fortran import heat_transport_variables as htv
from process.fortran import physics_variables as pv
from process.fortran import build_variables as bv
from process.fortran import current_drive_variables as cdv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import pfcoil_variables as pfv
from process.fortran import divertor_variables as dv
from process.fortran import structure_variables as sv

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
def cost_funcs(monkeypatch):
    """Fixture to mock commonly used dependencies in cost subroutines.

    Create CostsStep instance and mock Fortran module variables to aid testing.
    The values are intended to be realistic.
    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :return: CostsStep model object
    :rtype: process.costs_step.CostsStep
    """
    cost_funcs = CostModelBase()

    # Mock commonly-used Fortran module vars for testing
    monkeypatch.setattr(cv, "step_ref", step_ref)
    monkeypatch.setattr(cv, "step_con", 1.5e-1)
    monkeypatch.setattr(bldgsv, "efloor", 1e4)
    monkeypatch.setattr(htv, "pgrossmw", 5e2)
    monkeypatch.setattr(cost_funcs, "vfi", 6.737e3)
    monkeypatch.setattr(cost_funcs, "vfi_star", 6.737e3)
    monkeypatch.setattr(cost_funcs, "pth", 4.15e3)
    monkeypatch.setattr(cost_funcs, "ptherm_star", 4.15e3)
    monkeypatch.setattr(cost_funcs, "rmajor_star", 7.0)
    monkeypatch.setattr(cost_funcs, "rminor_star", 7 / 3.6)
    monkeypatch.setattr(tfv, "n_tf_turn", 104.6)
    monkeypatch.setattr(tfv, "tfleng", 34.63)
    # vfi values taken from Starfire reference in costs_step_module

    return cost_funcs


@pytest.fixture
def check_fkind(autouse=True):
    assert cv.fkind == 1.0
    yield
    assert cv.fkind == 1.0


def test_site_permits_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 12.

    :param: monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :para costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "site_permits_cost", 0.0)
    monkeypatch.setattr(cv, "site_permits", 1e8)

    # Run and assert result in M$
    cost_funcs.site_permits_costs()
    exp = 1.0e2
    obs = cost_funcs.site_permits_cost
    assert pytest.approx(obs) == exp


def test_plant_license_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 12.

    :param: monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :para costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(costs_step, "plant_license_cost", 0.0)
    monkeypatch.setattr(cv, "plant_licensing", 1e8)

    # Run and assert result in M$
    cost_funcs.plant_license_costs()
    exp = 1.0e2
    obs = cost_funcs.plant_license_cost
    assert pytest.approx(obs) == exp


def test_land_and_rights_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 20.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "step20", 0.0)
    monkeypatch.setattr(cv, "sitecost", 1e8)

    # Run and assert result in M$
    cost_funcs.land_and_rights_costs()
    exp = 1.003e2
    obs = cost_funcs.land_and_rights_cost
    assert pytest.approx(obs) == exp


@pytest.mark.parametrize(
    "isitetype, isiteaccomm, igridconn, irailaccess, exp",
    ((0, 0, 0, 0, 3.020832e3), (1, 1, 1, 1, 2.950832e3), (2, 0, 0, 0, 2.940832e3)),
)
def test_site_improvements_costs(
    monkeypatch, cost_funcs, isitetype, isiteaccomm, igridconn, irailaccess, exp
):
    """Validate sum of cost account 21.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(costs_step, "step21", 0.0)
    monkeypatch.setattr(cv, "isitetype", isitetype)
    monkeypatch.setattr(cv, "isiteaccomm", isiteaccomm)
    monkeypatch.setattr(cv, "igridconn", igridconn)
    monkeypatch.setattr(cv, "irailaccess", irailaccess)

    # Run and assert result in M$
    cost_funcs.site_improvements_costs()
    obs = cost_funcs.site_improvements_cost
    assert pytest.approx(obs) == exp


def test_reactor_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_reactor_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.reactor_bldg_costs()
    exp = 2.31296237
    obs = cost_funcs.reactor_bldg_cost
    assert pytest.approx(obs) == exp


def test_turbine_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    # monkeypatch.setattr(htv, "pgrossmw", 1e2)

    # Run and assert result in M$
    cost_funcs.turbine_bldg_costs()
    exp = 157.155
    obs = cost_funcs.turbine_bldg_cost
    assert pytest.approx(obs) == exp


def test_cooling_sys_struct_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    # monkeypatch.setattr(htv, "pgrossmw", 1e2)

    # Run and assert result in M$
    cost_funcs.cooling_sys_struct_costs()
    exp = 54.0775
    obs = cost_funcs.cooling_sys_struct_cost
    assert pytest.approx(obs) == exp


def test_electrical_and_power_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_ee_ps_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.electrical_and_power_bldg_costs()
    exp = 3.65476699
    obs = cost_funcs.electrical_and_power_bldg_cost
    assert pytest.approx(obs) == exp


def test_aux_services_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_aux_services_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.aux_services_bldg_costs()
    exp = 1.51692499
    obs = cost_funcs.aux_services_bldg_cost
    assert pytest.approx(obs) == exp


def test_hot_cell_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_hot_cell_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.hot_cell_costs()
    exp = 7.952725
    obs = cost_funcs.hot_cell_cost
    assert pytest.approx(obs) == exp


def test_reactor_serv_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_reactor_service_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.reactor_serv_bldg_costs()
    exp = 4.513624
    obs = cost_funcs.reactor_serv_bldg_cost
    assert pytest.approx(obs) == exp


def test_service_water_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_service_water_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.service_water_bldg_costs()
    exp = 0.64798899
    obs = cost_funcs.service_water_bldg_cost
    assert pytest.approx(obs) == exp


def test_fuel_handling_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_fuel_handling_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.fuel_handling_bldg_costs()
    exp = 12.334301
    obs = cost_funcs.fuel_handling_bldg_cost
    assert pytest.approx(obs) == exp


def test_control_room_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_control_room_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.control_room_costs()
    exp = 3.16419
    obs = cost_funcs.control_room_cost
    assert pytest.approx(obs) == exp


def test_ac_power_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_ac_ps_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.ac_power_bldg_costs()
    exp = 16.566771
    obs = cost_funcs.ac_power_bldg_cost
    assert pytest.approx(obs) == exp


def test_admin_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_admin_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.admin_bldg_costs()
    exp = 3.358777
    obs = cost_funcs.admin_bldg_cost
    assert pytest.approx(obs) == exp


def test_site_service_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_site_service_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.site_service_bldg_costs()
    exp = 1.57774899
    obs = cost_funcs.site_service_bldg_cost
    assert pytest.approx(obs) == exp


def test_cryo_and_inert_gas_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_cryo_inert_gas_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.cryo_and_inert_gas_bldg_costs()
    exp = 1.522104
    obs = cost_funcs.cryo_and_inert_gas_bldg_cost
    assert pytest.approx(obs) == exp


def test_security_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(bldgsv, "a_security_bldg", 1e2)

    # Run and assert result in M$
    cost_funcs.security_bldg_costs()
    exp = 0.529504
    obs = cost_funcs.security_bldg_cost
    assert pytest.approx(obs) == exp


def test_vent_stack_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "pth", 4.15e3)
    # monkeypatch.setattr(cost_funcs, "ptherm_star", 4.15e3)

    # Run and assert result in M$
    cost_funcs.vent_stack_costs()
    exp = 1.81
    obs = cost_funcs.vent_stack_cost
    assert pytest.approx(obs) == exp


def test_waste_fac_bldg_costs(monkeypatch, cost_funcs):
    """Validate sum of Reactor Building Costs

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_funcs: fixture to mock commonly-used cost vars
    :type cost_funcs: process.cost_funcs.CostModelBase
    """
    # Mock module vars
    monkeypatch.setattr(cv, "wfbuilding", 1e2)

    # Run and assert result in M$
    cost_funcs.waste_fac_bldg_costs()
    exp = 0.0001
    obs = cost_funcs.waste_fac_bldg_cost
    assert pytest.approx(obs) == exp


@pytest.mark.parametrize(
    "fkind, fwallcst_exp, blkcst_exp, exp",
    ((1, 5.0e-5, 0.095, 0.09505), (0.5, 2.5e-5, 0.0475, 0.047525)),
)
def test_blanket_first_wall_costs(
    monkeypatch, cost_funcs, fkind, fwallcst_exp, blkcst_exp, exp
):
    """Validate sum of cost account 22.01.01.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    monkeypatch.setattr(cv, "fwallcst", 0.0)
    monkeypatch.setattr(fwbsv, "fw_armour_mass", 5.0)
    monkeypatch.setattr(cv, "step_ucfwa", 5.0)
    monkeypatch.setattr(fwbsv, "fwmass", 5.0)
    monkeypatch.setattr(cv, "step_ucfws", 5.0)
    monkeypatch.setattr(cv, "ifueltyp", 2)
    monkeypatch.setattr(htv, "ipowerflow", 1)
    monkeypatch.setattr(fwbsv, "blkttype", 3)
    monkeypatch.setattr(fwbsv, "whtblbe", 10.0)
    monkeypatch.setattr(cv, "step_ucblbe", 8000)
    monkeypatch.setattr(fwbsv, "wtblli2o", 10.0)
    monkeypatch.setattr(cv, "step_ucblbreed", 800)
    monkeypatch.setattr(fwbsv, "whtblss", 10.0)
    monkeypatch.setattr(cv, "step_ucblss", 500)
    monkeypatch.setattr(fwbsv, "whtblvd", 10.0)
    monkeypatch.setattr(cv, "step_ucblvd", 200)
    monkeypatch.setattr(cv, "fkind", fkind)

    # Account 22.01.01.01 : First wall
    (
        fw_blk_total_cost,
        fw_cost,
        blk_total_cost,
        blk_multi_mat_cost,
        blk_breed_mat_cost,
        blk_steel_cost,
    ) = cost_funcs.blanket_first_wall_costs()
    fwallcst_obs = cv.fwallcst
    assert pytest.approx(fwallcst_obs) == fwallcst_exp

    # Test blkcst is correct
    blkcst_obs = cv.blkcst
    assert pytest.approx(blkcst_obs) == blkcst_exp

    # Test that the value of step220101 is calculated correctly
    # exp = 0.09505
    obs = fw_blk_total_cost
    assert pytest.approx(obs) == exp


@pytest.mark.parametrize("fkind, exp", ((1, 6.38925762e1), (0.5, 3.19462881e1)))
def test_ib_shield_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 22.01.02.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    monkeypatch.setattr(bv, "rsldi", 1.0)
    monkeypatch.setattr(bv, "shldith", 0.5)
    monkeypatch.setattr(bv, "shldtth", 0.5)
    monkeypatch.setattr(bv, "vgap", 0.5)
    monkeypatch.setattr(bv, "scrapli", 0.14)
    monkeypatch.setattr(bv, "scraplo", 0.15)
    monkeypatch.setattr(bv, "fwith", 0.5)
    monkeypatch.setattr(bv, "fwoth", 0.5)
    monkeypatch.setattr(bv, "blnktth", 0.5)
    monkeypatch.setattr(bv, "d_vv_in", 0.07)
    monkeypatch.setattr(fwbsv, "i_shield_mat", 0)
    monkeypatch.setattr(fwbsv, "denw", 19250.0)
    monkeypatch.setattr(fwbsv, "denwc", 15630.0)
    monkeypatch.setattr(dv, "divfix", 1.0)
    monkeypatch.setattr(cv, "step_ucshw", 269.638)
    monkeypatch.setattr(cv, "step_ucshwc", 930.251)
    monkeypatch.setattr(pv, "rminor", 1.0)
    monkeypatch.setattr(pv, "kappa", 1.792)
    monkeypatch.setattr(pv, "idivrt", 1)
    monkeypatch.setattr(cv, "fkind", fkind)

    cost_funcs.ib_shield_costs()
    obs = cost_funcs.ib_shield_cost
    assert pytest.approx(obs) == exp


@pytest.mark.parametrize(
    "fkind, cop_exp, sc_exp, cryoal_exp",
    ((1, 629.24550, 507.24287, 15.552), (0.5, 314.62275, 253.621435, 7.776)),
)
def test_tf_coils_costs(monkeypatch, cost_funcs, fkind, cop_exp, sc_exp, cryoal_exp):
    """Cost of TF coils for different materials (22.01.03.01).

    :param monkeypatch: fixture for mocking variables
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock dependencies with realistic values
    monkeypatch.setattr(cv, "step_ref", np.zeros(70, order="F"))
    # Only mock used array elements
    cv.step_ref[21] = 1.2572e2
    monkeypatch.setattr(cv, "cpstcst", 0.0)
    monkeypatch.setattr(cv, "ifueltyp", 1)
    monkeypatch.setattr(cv, "step_uc_cryo_al", 81.0)
    monkeypatch.setattr(cv, "step_mc_cryo_al_per", 0.2)
    monkeypatch.setattr(cv, "uccpcl1", 250.0)
    monkeypatch.setattr(cv, "uccpclb", 150.0)
    monkeypatch.setattr(cv, "step_uccu", 82.0)
    monkeypatch.setattr(cv, "step_uccase", 91.0)
    monkeypatch.setattr(cv, "step_cconfix", 233.0)
    monkeypatch.setattr(cv, "step_ucwindtf", 1520.0)
    monkeypatch.setattr(cv, "step_ucint", 91.0)
    monkeypatch.setattr(cv, "step_ucgss", 91.0)
    monkeypatch.setattr(cv, "step_cconshtf", 91.0)
    monkeypatch.setattr(tfv, "whtconal", 1.0e4)
    monkeypatch.setattr(tfv, "whtconsc", 2.868e3)
    monkeypatch.setattr(tfv, "whtcas", 9.198e4)
    monkeypatch.setattr(tfv, "whtconcu", 9.818e3)
    monkeypatch.setattr(tfv, "i_tf_sc_mat", 8)
    monkeypatch.setattr(tfv, "n_tf", 16.0)
    monkeypatch.setattr(tfv, "n_tf_turn", 104.6)
    monkeypatch.setattr(tfv, "tfleng", 34.63)
    monkeypatch.setattr(tfv, "whttflgs", 1.403e6)
    monkeypatch.setattr(tfv, "whtcp", 0.0)
    monkeypatch.setattr(pv, "itart", 1)
    monkeypatch.setattr(sv, "clgsmass", 1.570e5)
    monkeypatch.setattr(sv, "aintmass", 1.335e6)
    monkeypatch.setattr(cv, "fkind", fkind)

    # Copper coils
    monkeypatch.setattr(tfv, "i_tf_sup", 0)
    cost_funcs.tf_coils_costs()
    observed = cost_funcs.tf_coils_cost
    assert pytest.approx(observed) == cop_exp

    # Superconducting coils
    monkeypatch.setattr(tfv, "i_tf_sup", 1)
    cost_funcs.tf_coils_costs()
    # expected = 4129.54087
    # expected = 507.24287
    observed = cost_funcs.tf_coils_cost
    assert pytest.approx(observed) == sc_exp

    # Cryo-aluminium coils
    monkeypatch.setattr(tfv, "i_tf_sup", 2)
    cost_funcs.tf_coils_costs()
    # expected = 15.552
    obs = cost_funcs.tf_coils_cost
    assert pytest.approx(obs) == cryoal_exp


@pytest.mark.parametrize(
    "fkind, exp", [(1, 11.682954760169723), (0.5, 5.84147738008486)]
)
def test_pf_coils_costs(monkeypatch, cost_funcs, fkind, exp):
    """Test evaluation of account 22.01.03.02 (PF magnet) costs
    :param monkeypatch: fixture for mocking variables
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars used in subroutine
    monkeypatch.setattr(pfv, "nohc", 2.0)
    monkeypatch.setattr(pfv, "turns", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "rpf", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "ipfres", 1.0)
    monkeypatch.setattr(bv, "iohcl", 0.0)
    monkeypatch.setattr(cv, "step_uccu", 82.0)
    monkeypatch.setattr(pfv, "vf", np.full(22, 0.5, order="F"))
    monkeypatch.setattr(pfv, "ric", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "rjconpf", np.full(22, 1.0e7, order="F"))
    monkeypatch.setattr(cv, "fkind", fkind)

    cost_funcs.pf_coils_costs()
    obs = cost_funcs.pf_total_cost
    assert pytest.approx(obs) == exp


@pytest.mark.parametrize("fkind, exp", ((1, 59), (0.5, 29.5)))
def test_central_sol_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 20.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "step20", 0.0)
    monkeypatch.setattr(cv, "step_ref", np.zeros(70, order="F"))
    cv.step_ref[23] = 5.9e1
    monkeypatch.setattr(cv, "fkind", fkind)

    # Run and assert result in M$
    cost_funcs.central_sol_costs()
    # exp = 5.9e1
    obs = cost_funcs.central_sol_cost
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize("fkind, exp", ((1, 32.54), (0.5, 16.27)))
def test_control_coils_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 20.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "step20", 0.0)
    monkeypatch.setattr(cv, "step_ref", np.zeros(70, order="F"))
    cv.step_ref[24] = 3.254e1
    monkeypatch.setattr(cv, "fkind", fkind)

    # Run and assert result in M$
    cost_funcs.control_coils_costs()
    # exp = 4.0
    obs = cost_funcs.control_coils_cost
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize("fkind, exp", ((1, 2.04634910e3), (0.5, 1.02317455e3)))
def test_hcd_costs(monkeypatch, cost_funcs, fkind, exp):
    """Test evaluation of account costs: 22.01.04
    (Auxiliary Heating and Current Drive)

    :param monkeypatch: fixture for mocking variables
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars used in subroutine
    monkeypatch.setattr(cv, "fcdfuel", 0.1)
    monkeypatch.setattr(cv, "ucich", 3.0)
    monkeypatch.setattr(cv, "uclh", 3.3)
    monkeypatch.setattr(cv, "ifueltyp", 0.0)
    monkeypatch.setattr(cdv, "iefrf", 5.0)
    monkeypatch.setattr(cdv, "iefrffix", 5.0)
    monkeypatch.setattr(cdv, "echpwr", 90.0)
    monkeypatch.setattr(cv, "step_ref", np.zeros(70, order="F"))
    monkeypatch.setattr(cv, "fkind", fkind)
    # Only mock used array elements
    cv.step_ref[68] = 19.21
    cv.step_ref[69] = 12.85

    cost_funcs.hcd_costs()
    obs = cost_funcs.hcd_cost
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize("fkind, exp", ((1, 429.2), (0.5, 214.6)))
def test_primary_structure_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 20.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "step20", 0.0)
    monkeypatch.setattr(cv, "step_ref", np.zeros(70, order="F"))
    cv.step_ref[26] = 4.292e2
    monkeypatch.setattr(cv, "fkind", fkind)

    # Run and assert result in M$
    cost_funcs.primary_structure_costs()
    # exp = 52.74
    obs = cost_funcs.primary_structure_cost
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize("fkind, exp", ((1, 39.55), (0.5, 19.775)))
def test_reactor_vacuum_sys_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 20.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "step20", 0.0)
    monkeypatch.setattr(cv, "step_ref", np.zeros(70, order="F"))
    cv.step_ref[27] = 3.955e1
    monkeypatch.setattr(cv, "fkind", fkind)

    # Run and assert result in M$
    cost_funcs.reactor_vacuum_sys_costs()
    # exp = 4.86
    obs = cost_funcs.reactor_vacuum_sys_cost
    assert pytest.approx(obs) == exp


def test_power_supplies_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 20.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "step20", 0.0)

    # Run and assert result in M$
    cost_funcs.power_supplies_costs()
    exp = 52.9
    obs = cost_funcs.power_supplies_cost
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize("fkind, exp", ((1, 227.48664416), (0.5, 113.74332208)))
def test_divertor_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 20.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    # monkeypatch.setattr(cost_funcs, "step20", 0.0)
    monkeypatch.setattr(dv, "divleg_profile_inner", 1.0)
    monkeypatch.setattr(dv, "divleg_profile_outer", 2.0)
    monkeypatch.setattr(pv, "rmajor", 6.0)
    monkeypatch.setattr(pv, "idivrt", 1)
    monkeypatch.setattr(cv, "fkind", fkind)
    monkeypatch.setattr(cv, "ifueltyp", 2)
    cv.step_ref[31] = 1.364e2

    # Run and assert result in M$
    cost_funcs.divertor_costs()
    # exp = 227.48664416
    obs = cost_funcs.divertor_cost
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize("fkind, exp", ((1, 4.611899e1), (0.5, 2.3059495e1)))
def test_heat_transfer_sys_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 22.02.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars used in subroutine
    monkeypatch.setattr(cv, "fkind", fkind)

    cost_funcs.heat_transfer_sys_costs()
    obs = cost_funcs.heat_transfer_sys_cost
    assert pytest.approx(obs) == exp


def test_cryo_sys_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 22.03.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(tfv, "cryo_cool_req", 10.0)

    cost_funcs.cryo_sys_costs()
    obs = cost_funcs.cryo_sys_cost
    exp = 26.1919825
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize("fkind, exp", ((1, 4.8e0), (0.5, 2.4e0)))
def test_waste_disposal_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 22.04.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(cv, "fkind", fkind)

    cost_funcs.waste_disposal_costs()
    obs = cost_funcs.waste_disposal_cost
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize(
    "fkind, exp1, exp2", ((1, 3.86e1, 1.940036), (0.5, 1.93e1, 0.970018))
)
def test_fuel_handling_costs(monkeypatch, cost_funcs, fkind, exp1, exp2):
    """Validate sum of cost account 22.05.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(cv, "fkind", fkind)

    fuel_handling_cost, spares = cost_funcs.fuel_handling_costs()
    assert pytest.approx(fuel_handling_cost) == exp1
    assert pytest.approx(spares) == exp2


#
@pytest.mark.parametrize("fkind, exp1", ((1, 5.45e0), (0.5, 2.725)))
def test_other_reactor_equip_costs(monkeypatch, cost_funcs, fkind, exp1):
    """Validate sum of cost account 22.06.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(cv, "fkind", fkind)

    exp2 = 8.3e-1
    other_reactor_equip_cost, spares = cost_funcs.other_reactor_equip_costs()
    assert pytest.approx(other_reactor_equip_cost) == exp1
    assert pytest.approx(spares) == exp2


#
@pytest.mark.parametrize("fkind, exp", ((1, 2.341e1), (0.5, 1.1705e1)))
def test_instrument_and_control_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 22.07.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(cv, "fkind", fkind)

    cost_funcs.instrument_and_control_costs()
    obs = cost_funcs.instrument_and_control_cost
    assert pytest.approx(obs) == exp


def test_turbine_sys_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 22.02.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars used in subroutine

    cost_funcs.turbine_sys_costs()
    obs = cost_funcs.turbine_sys_cost
    exp = 277.71999999
    assert pytest.approx(obs) == exp


def test_heat_reject_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 22.02.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars used in subroutine

    cost_funcs.heat_reject_costs()
    obs = cost_funcs.heat_reject_cost
    exp = 62.48339499
    assert pytest.approx(obs) == exp


def test_electric_plant_equip_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 24.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine

    cost_funcs.electric_plant_equip_costs()
    obs = cost_funcs.electric_plant_equip_cost
    exp = 9.168104e1
    assert pytest.approx(obs) == exp


def test_misc_equip_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 25.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(bldgsv, "wgt", 5e2)

    cost_funcs.misc_equip_costs()
    obs = cost_funcs.misc_equip_cost
    exp = 82.89063225
    assert pytest.approx(obs) == exp


#
@pytest.mark.parametrize("fkind, exp", ((1, 50.0), (0.5, 25.0)))
def test_remote_handling_costs(monkeypatch, cost_funcs, fkind, exp):
    """Validate sum of cost account 27.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock dependencies with realistic values
    monkeypatch.setattr(cv, "step_rh_costfrac", 0.05)
    monkeypatch.setattr(cv, "cdirt", 1.0e3)
    monkeypatch.setattr(cv, "fkind", fkind)

    cost_funcs.remote_handling_costs()
    obs = cost_funcs.remote_handling_cost
    assert pytest.approx(obs) == exp


def test_indirect_costs(monkeypatch, cost_funcs):
    """Validate sum of cost account 25.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(cv, "cdirt", 1.0e3)

    cost_funcs.indirect_costs()

    obs_con = cost_funcs.construct_facs_cost
    exp_con = 300
    assert pytest.approx(obs_con) == exp_con

    obs_eng = cost_funcs.eng_construct_manage_cost
    exp_eng = 325
    assert pytest.approx(obs_eng) == exp_eng

    obs_other = cost_funcs.other_cost
    exp_other = 150
    assert pytest.approx(obs_other) == exp_other
