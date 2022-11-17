from process.cost_model_2 import CostModel2
from process.fortran import tfcoil_variables as tfv
from process.fortran import cost_variables as cv
from process.fortran import buildings_variables as bldgsv
from process.fortran import heat_transport_variables as htv
from process.fortran import physics_variables as pv
from process.fortran import build_variables as bv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import times_variables as tv
from process.fortran import divertor_variables as dv
from process.fortran import current_drive_variables as cdv
from process.fortran import structure_variables as sv
from process.fortran import pfcoil_variables as pfv

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
def cost_model_2(monkeypatch):
    """Fixture to mock commonly used dependencies in cost subroutines.

    Create CostModel2 instance and mock Fortran module variables to aid testing.
    The values are intended to be realistic.
    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :return: CostModel2 model object
    :rtype: process.cost_model_2.CostModel2
    """
    cost_model_2 = CostModel2()

    return cost_model_2


def test_cost_model_2(monkeypatch, cost_model_2):
    """Test the cost_model_2 subroutine

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param cost_model_2: fixture to mock commonly-used cost vars
    :type cost_model_2: process.cost_model_2.CostModel2
    """
    # Mock module vars
    monkeypatch.setattr(cv, "step_ref", step_ref)
    monkeypatch.setattr(cv, "step_con", 1.5e-1)
    monkeypatch.setattr(bldgsv, "efloor", 1e4)
    monkeypatch.setattr(htv, "pgrossmw", 5e2)
    monkeypatch.setattr(cost_model_2, "vfi", 6.737e3)
    monkeypatch.setattr(cost_model_2, "vfi_star", 6.737e3)
    monkeypatch.setattr(cost_model_2, "pth", 4.15e3)
    monkeypatch.setattr(cost_model_2, "ptherm_star", 4.15e3)
    monkeypatch.setattr(cost_model_2, "rmajor_star", 7.0)
    monkeypatch.setattr(cost_model_2, "rminor_star", 7 / 3.6)
    monkeypatch.setattr(tfv, "n_tf_turn", 104.6)
    monkeypatch.setattr(tfv, "tfleng", 34.63)
    monkeypatch.setattr(cv, "site_permits", 1e8)
    monkeypatch.setattr(cv, "plant_licensing", 1e8)
    monkeypatch.setattr(cv, "sitecost", 1e8)
    monkeypatch.setattr(cv, "isitetype", 0)
    monkeypatch.setattr(cv, "isiteaccomm", 0)
    monkeypatch.setattr(cv, "igridconn", 0)
    monkeypatch.setattr(cv, "irailaccess", 0)
    monkeypatch.setattr(bldgsv, "a_reactor_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_ee_ps_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_aux_services_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_hot_cell_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_reactor_service_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_service_water_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_fuel_handling_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_control_room_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_ac_ps_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_admin_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_site_service_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_cryo_inert_gas_bldg", 1e2)
    monkeypatch.setattr(bldgsv, "a_security_bldg", 1e2)
    monkeypatch.setattr(cv, "wfbuilding", 1e2)
    monkeypatch.setattr(cv, "fwallcst", 0.0)
    monkeypatch.setattr(fwbsv, "fw_armour_mass", 5.0)
    monkeypatch.setattr(cv, "step_ucfwa", 5.0)
    monkeypatch.setattr(fwbsv, "fwmass", 5.0)
    monkeypatch.setattr(bv, "fwarea", 9.42e2)
    monkeypatch.setattr(cv, "step_ucfws", 5.0)
    monkeypatch.setattr(cv, "ifueltyp", 2)
    monkeypatch.setattr(htv, "ipowerflow", 0)
    monkeypatch.setattr(fwbsv, "blktmodel", 0)
    monkeypatch.setattr(fwbsv, "blkttype", 3)
    monkeypatch.setattr(fwbsv, "whtblbe", 10.0)
    monkeypatch.setattr(cv, "step_ucblbe", 8000)
    monkeypatch.setattr(fwbsv, "wtblli2o", 10.0)
    monkeypatch.setattr(cv, "step_ucblbreed", 800)
    monkeypatch.setattr(fwbsv, "whtblss", 10.0)
    monkeypatch.setattr(cv, "step_ucblss", 500)
    monkeypatch.setattr(fwbsv, "whtblvd", 10.0)
    monkeypatch.setattr(cv, "step_ucblvd", 200)
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
    monkeypatch.setattr(cv, "cpstcst", 0.0)
    monkeypatch.setattr(cv, "step_uc_cryo_al", 81.0)
    monkeypatch.setattr(cv, "step_mc_cryo_al_per", 0.2)
    monkeypatch.setattr(cv, "uccpcl1", 250.0)
    monkeypatch.setattr(cv, "uccpclb", 150.0)
    monkeypatch.setattr(cv, "step_uccu", 82.0)
    monkeypatch.setattr(cv, "step_uccase", 91.0)
    monkeypatch.setattr(cv, "step_cconfix", 233.0)
    monkeypatch.setattr(cv, "step_ucwindpf", 1520.0)
    monkeypatch.setattr(cv, "step_ucwindtf", 1520.0)
    monkeypatch.setattr(cv, "step_ucint", 91.0)
    monkeypatch.setattr(cv, "step_ucgss", 91.0)
    monkeypatch.setattr(cv, "step_cconshtf", 91.0)
    monkeypatch.setattr(cv, "step_ucfnc", 91.0)
    monkeypatch.setattr(pfv, "whtpfs", 0.0)
    monkeypatch.setattr(sv, "fncmass", 100.0)
    monkeypatch.setattr(tfv, "whtconal", 1.0e4)
    monkeypatch.setattr(tfv, "whtconsc", 2.868e3)
    monkeypatch.setattr(tfv, "whtcas", 9.198e4)
    monkeypatch.setattr(tfv, "whtconcu", 9.818e3)
    monkeypatch.setattr(tfv, "i_tf_sc_mat", 8)
    monkeypatch.setattr(tfv, "n_tf", 16.0)
    monkeypatch.setattr(tfv, "whttflgs", 1.403e6)
    monkeypatch.setattr(tfv, "i_tf_sup", 0)
    monkeypatch.setattr(tfv, "whtcp", 1e6)
    monkeypatch.setattr(pv, "itart", 1)
    monkeypatch.setattr(sv, "clgsmass", 1.570e5)
    monkeypatch.setattr(sv, "aintmass", 1.335e6)
    monkeypatch.setattr(pfv, "nohc", 2.0)
    monkeypatch.setattr(pfv, "turns", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "rpf", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "ipfres", 1.0)
    monkeypatch.setattr(cv, "step_ucsc", np.full(9, 5.0, order="F"))
    monkeypatch.setattr(pfv, "isumatpf", 1)
    monkeypatch.setattr(pfv, "fcupfsu", 0.5)
    monkeypatch.setattr(bv, "iohcl", 0.0)
    monkeypatch.setattr(pfv, "vf", np.full(22, 0.5, order="F"))
    monkeypatch.setattr(pfv, "ric", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "rjconpf", np.full(22, 1.0e7, order="F"))
    monkeypatch.setattr(tfv, "dcond", np.full(9, 5.0e3, order="F"))
    monkeypatch.setattr(cv, "fcdfuel", 0.1)
    monkeypatch.setattr(cv, "ucich", 3.0)
    monkeypatch.setattr(cv, "uclh", 3.3)
    monkeypatch.setattr(cdv, "iefrf", 5.0)
    monkeypatch.setattr(cdv, "iefrffix", 5.0)
    monkeypatch.setattr(cdv, "echpwr", 90.0)
    monkeypatch.setattr(dv, "divleg_profile_inner", 1.0)
    monkeypatch.setattr(dv, "divleg_profile_outer", 2.0)
    monkeypatch.setattr(pv, "rmajor", 6.0)
    monkeypatch.setattr(tfv, "cryo_cool_req", 10.0)
    monkeypatch.setattr(bldgsv, "wgt", 5e2)
    monkeypatch.setattr(cv, "step_rh_costfrac", 0.075)
    monkeypatch.setattr(cv, "coecap", 0.0)
    monkeypatch.setattr(cv, "coefuelt", 0.0)
    monkeypatch.setattr(cv, "coeoam", 0.0)
    monkeypatch.setattr(htv, "pnetelmw", 1e5)
    monkeypatch.setattr(cv, "cfactr", 10.0)
    monkeypatch.setattr(tv, "tburn", 10.0)
    monkeypatch.setattr(tv, "tcycle", 5.0)
    monkeypatch.setattr(cv, "fcap0", 10.0)
    monkeypatch.setattr(cv, "fcr0", 10.0)
    monkeypatch.setattr(cv, "discount_rate", 0.5)
    monkeypatch.setattr(fwbsv, "bktlife", 10.0)
    monkeypatch.setattr(cost_model_2, "fwblkcost", 10.0)
    monkeypatch.setattr(cv, "fcap0cp", 10.0)
    monkeypatch.setattr(cv, "divlife", 2.0)
    monkeypatch.setattr(cv, "divcst", 2.0)
    monkeypatch.setattr(cv, "cplife", 3.0)
    monkeypatch.setattr(cv, "cdrlife", 5.0)
    monkeypatch.setattr(cv, "cdcost", 5.0)
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
    monkeypatch.setattr(cv, "step91_per", 3.0e-1)
    monkeypatch.setattr(cv, "step92_per", 3.25e-1)
    monkeypatch.setattr(cv, "step93_per", 1.5e-1)
    monkeypatch.setattr(cv, "fkind", 1.0)

    cost_model_2.run()

    # Test that module variables are calculated correctly
    obs_vfi = cost_model_2.vfi
    exp_vfi = 2.120575e4
    assert pytest.approx(obs_vfi) == exp_vfi
    obs_pth = cost_model_2.pth
    exp_pth = 30.0
    assert pytest.approx(obs_pth) == exp_pth

    # Test that module variables are assigned correctly
    assert cost_model_2.vfi_star == 6.737e3
    assert cost_model_2.ptherm_star == 4.15e3
    assert cost_model_2.rmajor_star == 7.0e0
    assert cost_model_2.rminor_star == 7.0 / 3.6

    # Total plant direct cost with remote handling
    exp = 5195.55897756
    obs = cv.cdirt
    assert pytest.approx(obs) == exp

    # Constructed cost
    exp_concost = 9222.11718518
    obs_concost = cv.concost
    assert pytest.approx(obs_concost) == exp_concost
