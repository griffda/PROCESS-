"""Unit tests for costs_step.f90."""
from process.costs_step import CostsStep
from process.fortran import tfcoil_variables as tfv
from process.fortran import cost_variables as cv
from process.fortran import buildings_variables as bldgsv
from process.fortran import heat_transport_variables as htv
from process.fortran import physics_variables as pv
from process.fortran import build_variables as bv
from process.fortran import current_drive_variables as cdv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import pfcoil_variables as pfv
from process.fortran import times_variables as tv
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
    monkeypatch.setattr(costs_step, "rminor_star", 7/3.6)
    monkeypatch.setattr(tfv, "n_tf_turn", 104.6)
    monkeypatch.setattr(tfv, "tfleng", 34.63)
    # vfi values taken from Starfire reference in costs_step_module

    return costs_step


def test_init_costs_step():
    """Test initialisation of variables"""
    #Assert module vars are initialised correctly
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
    #Mock module vars
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

    #Test that module variables are calculated correctly
    obs_vfi = costs_step.vfi
    exp_vfi = 2.120575e4
    assert pytest.approx(obs_vfi) == exp_vfi
    obs_pth = costs_step.pth
    exp_pth = 30.0
    assert pytest.approx(obs_pth) == exp_pth

    #Test that module variables are assigned correctly
    assert costs_step.vfi_star == 6.737e3 
    assert costs_step.ptherm_star == 4.15e3
    assert costs_step.rmajor_star == 7.0e0
    assert costs_step.rminor_star == 7.0/3.6

    #Total plant direct cost with remote handling
    exp = 3700.3758089
    obs = cv.cdirt
    assert pytest.approx(obs) == exp

    #Constructed cost
    exp_concost = 6568.16706084
    obs_concost = cv.concost
    assert pytest.approx(obs_concost) == exp_concost



def test_step_a20(monkeypatch, costs_step):
    """Validate sum of cost account 20.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    costs_step = CostsStep()
    
    # Mock module vars
    monkeypatch.setattr(costs_step, "step20", 0.0)
    monkeypatch.setattr(cv, "sitecost", 1e8)

    # Run and assert result in M$
    costs_step.step_a20()
    exp = 1.003e2
    obs = costs_step.step20
    assert pytest.approx(obs) == exp



def test_step_a21(monkeypatch, costs_step):
    """Validate sum of cost account 21.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars
    monkeypatch.setattr(costs_step, "step21", 0.0)

    # Run and assert result in M$
    costs_step.step_a21()
    exp = 2.115588e3
    obs = costs_step.step21
    assert pytest.approx(obs) == exp


def test_step_a22(monkeypatch, costs_step):
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
    costs_step.step_a22()
    exp = 670.4911458
    obs = costs_step.step22
    assert pytest.approx(obs) == exp


def test_step_a2201(monkeypatch, costs_step):
    """Validate sum of cost account 22.01.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine: increase is value of step2201
    monkeypatch.setattr(costs_step, "step22", 0.0)
    monkeypatch.setattr(cv, "step_ref", np.zeros(70, order="F"))
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
    
    exp1 = 1344.9437432
    #TODO update spares expected value
    # exp2 = 1.0199574292e1
    step2201, spares = costs_step.step_a2201()
    assert pytest.approx(step2201) == exp1
    # assert pytest.approx(spares) == exp2

def test_step_a220101(monkeypatch, costs_step):
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

    #Account 22.01.01.01 : First wall
    (
        step220101,
        step22010101,
        step22010102,
        step2201010201,
        step2201010202,
        step2201010203,
    ) = costs_step.step_a220101()
    fwallcst_exp = 5.0e-5
    fwallcst_obs = cv.fwallcst
    assert pytest.approx(fwallcst_obs) == fwallcst_exp

    #Test blkcst is correct
    blkcst_exp = 0.095
    blkcst_obs = cv.blkcst
    assert pytest.approx(blkcst_obs) == blkcst_exp

    #Test that the value of step220101 is calculated correctly
    assert pytest.approx(step220101) == 0.09505

def test_step_a220102(monkeypatch, costs_step):
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

    obs = costs_step.step_a220102()
    exp = 6.38925762e1
    assert pytest.approx(obs) == exp

def test_step_a22010301(monkeypatch, costs_step):
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
    monkeypatch.setattr(costs_step, "vfi", 5e3)
    monkeypatch.setattr(costs_step, "vfi_star", 6.737e3)
    
    # Copper coils
    monkeypatch.setattr(tfv, "i_tf_sup", 0)
    expected = 629.24550
    observed = costs_step.step_a22010301()
    assert pytest.approx(observed) == expected

    # Superconducting coils
    monkeypatch.setattr(tfv, "i_tf_sup", 1)
    #expected = 4129.54087
    expected = 507.24287
    observed = costs_step.step_a22010301()
    assert pytest.approx(observed) == expected

    # Cryo-aluminium coils
    monkeypatch.setattr(tfv, "i_tf_sup", 2)
    expected = 15.552
    obs = costs_step.step_a22010301()
    assert pytest.approx(obs) == expected

def test_step_a22010302(monkeypatch, costs_step):
    """Test evaluation of account 22.01.03.02 (PF magnet) costs
    :param monkeypatch: fixture for mocking variables
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    #Mock module vars used in subroutine
    monkeypatch.setattr(pfv, "nohc", 2.0)
    monkeypatch.setattr(pfv, "turns", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "rpf", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "ipfres", 1.0)
    monkeypatch.setattr(bv, "iohcl", 0.0)
    monkeypatch.setattr(cv, "step_uccu", 82.0)
    monkeypatch.setattr(pfv, "vf", np.full(22, 0.5, order="F"))
    monkeypatch.setattr(pfv, "ric", np.full(22, 5.0, order="F"))
    monkeypatch.setattr(pfv, "rjconpf", np.full(22, 1.0e7, order="F"))

    exp = 11.682954760169723
    obs = costs_step.step_a22010302()
    assert pytest.approx(obs) == exp


def test_step_a220104(monkeypatch, costs_step):
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
    # Only mock used array elements
    cv.step_ref[68] = 19.21
    cv.step_ref[69] = 12.85

    exp = 2.04634910e3
    obs = costs_step.step_a220104()
    assert pytest.approx(obs) == exp


def test_step_a2202(costs_step):
    """Validate sum of cost account 22.02.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    exp = 4.611899e1
    obs = costs_step.step_a2202()
    assert pytest.approx(obs) == exp


def test_step_a2203(costs_step):
    """Validate sum of cost account 22.03.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    exp = 1.490e1
    obs = costs_step.step_a2203()
    assert pytest.approx(obs) == exp


def test_step_a2204(costs_step):
    """Validate sum of cost account 22.04.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    exp = 4.8e0
    obs = costs_step.step_a2204()
    assert pytest.approx(obs) == exp


def test_step_a2205(costs_step):
    """Validate sum of cost account 22.05.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    exp1 = 3.86e1
    exp2 = 1.940036
    step2205, spares = costs_step.step_a2205()
    assert pytest.approx(step2205) == exp1
    assert pytest.approx(spares) == exp2


def test_step_a2206(costs_step):
    """Validate sum of cost account 22.06.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    exp1 = 5.45e0
    exp2 = 8.3e-1
    step2206, spares = costs_step.step_a2206()
    assert pytest.approx(step2206) == exp1
    assert pytest.approx(spares) == exp2


def test_step_a2207(costs_step):
    """Validate sum of cost account 22.07.

    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    exp = 2.341e1
    obs = costs_step.step_a2207()
    assert pytest.approx(obs) == exp


def test_step_a23(monkeypatch, costs_step):
    """Validate sum of cost account 23.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(costs_step, "step23", 0.0)

    costs_step.step_a23()
    exp = 3.967150e2
    obs = costs_step.step23
    assert pytest.approx(obs) == exp


def test_step_a24(monkeypatch, costs_step):
    """Validate sum of cost account 24.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(costs_step, "step24", 0.0)

    exp = 9.168104e1
    costs_step.step_a24()
    obs = costs_step.step24
    assert pytest.approx(obs) == exp


def test_step_a25(monkeypatch, costs_step):
    """Validate sum of cost account 25.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(costs_step, "step25", 0.0)

    exp = 1.050024e2
    costs_step.step_a25()
    obs = costs_step.step25
    assert pytest.approx(obs) == exp


def test_step_a27(monkeypatch, costs_step):
    """Validate sum of cost account 27.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock dependencies with realistic values
    monkeypatch.setattr(costs_step, "step27", 0.0)
    monkeypatch.setattr(cv, "step_rh_costfrac", 0.05)
    monkeypatch.setattr(cv, "cdirt", 200.0)

    exp = 10.0
    costs_step.step_a27()
    obs = costs_step.step27
    assert pytest.approx(obs) == exp  


def test_step_indirect_costs(monkeypatch, costs_step):
    """Test indirect cost calculations.

    :param monkeypatch: fixture for mocking
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    # Mock module vars passed and being set
    monkeypatch.setattr(cv, "cdirt", 1.0e3)
    monkeypatch.setattr(costs_step, "step91", 0.0)
    monkeypatch.setattr(costs_step, "step92", 0.0)
    monkeypatch.setattr(costs_step, "step93", 0.0)
    
    # Run and assert module vars for costs
    costs_step.step_indirect_costs()
    assert costs_step.step91 == 300
    assert costs_step.step92 == 325
    assert costs_step.step93 == 150


def test_coelc_step(monkeypatch, costs_step):
    """Test electricity cost calculations
    :param monkeypatch: fixture for mocking
    :type monkeypatch: MonkeyPatch
    :param costs_step: fixture to mock commonly-used cost vars
    :type costs_step: process.costs_step.CostsStep
    """
    #Mock module vars
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


