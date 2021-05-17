"""Unit tests for costs_step.f90."""
from process.fortran import costs_step_module as cs
from process.fortran import cost_variables as cv
from process.fortran import buildings_variables as bv
from process.fortran import heat_transport_variables as htv
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
    ]
)

@pytest.fixture
def shared_cost_vars(monkeypatch):
    """Fixture to mock commonly used dependencies in cost subroutines.

    The values are intended to be realistic.
    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    """
    monkeypatch.setattr(cv, "step_ref", step_ref)
    monkeypatch.setattr(cv, "step_con", 1.5e-1)
    monkeypatch.setattr(bv, "efloor", 1e4)
    monkeypatch.setattr(htv, "pgrossmw", 5e2)
    monkeypatch.setattr(cs, "vfi", 6.737e3)
    monkeypatch.setattr(cs, "vfi_star", 6.737e3)
    # vfi values taken from Starfire reference in costs_step_module

def test_step_a21(monkeypatch, shared_cost_vars):
    """Validate sum of cost account 21.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param shared_cost_vars: fixture to mock commonly-used cost vars
    :type shared_cost_vars: Fixture
    """
    # Mock module vars
    monkeypatch.setattr(cs, "pth", 4.15e3)
    monkeypatch.setattr(cs, "ptherm_star", 4.15e3)
    monkeypatch.setattr(cs, "step21", 0.0)

    # Run and assert result in M$
    cs.step_a21(0, 0)
    exp = 1.999836e3
    obs = cs.step21
    assert pytest.approx(obs) == exp

def test_step_a2202(monkeypatch, shared_cost_vars):
    """Validate sum of cost account 22.02.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param shared_cost_vars: fixture to mock commonly-used cost vars
    :type shared_cost_vars: Fixture
    """
    # Mock module var set in subroutine: increase is value of step2202
    monkeypatch.setattr(cs, "step22", 0.0)
    
    exp = 4.611899e1
    cs.step_a2202(0, 0)
    obs = cs.step22
    assert pytest.approx(obs) == exp

def test_step_a2203(monkeypatch, shared_cost_vars):
    """Validate sum of cost account 22.03.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param shared_cost_vars: fixture to mock commonly-used cost vars
    :type shared_cost_vars: Fixture
    """
    # Mock module var set in subroutine: increase is value of step2203
    monkeypatch.setattr(cs, "step22", 0.0)

    exp = 1.490e1
    cs.step_a2203(0, 0)
    obs = cs.step22
    assert pytest.approx(obs) == exp

def test_step_a23(monkeypatch, shared_cost_vars):
    """Validate sum of cost account 23.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param shared_cost_vars: fixture to mock commonly-used cost vars
    :type shared_cost_vars: Fixture
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(cs, "step23", 0.0)

    exp = 3.967150e2
    cs.step_a23(0, 0)
    obs = cs.step23
    assert pytest.approx(obs) == exp

def test_step_a24(monkeypatch, shared_cost_vars):
    """Validate sum of cost account 24.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param shared_cost_vars: fixture to mock commonly-used cost vars
    :type shared_cost_vars: Fixture
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(cs, "step24", 0.0)

    exp = 9.168104e1
    cs.step_a24(0, 0)
    obs = cs.step24
    assert pytest.approx(obs) == exp

def test_step_a25(monkeypatch, shared_cost_vars):
    """Validate sum of cost account 25.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    :param shared_cost_vars: fixture to mock commonly-used cost vars
    :type shared_cost_vars: Fixture
    """
    # Mock module var set in subroutine
    monkeypatch.setattr(cs, "step25", 0.0)

    exp = 1.050024e2
    cs.step_a25(0, 0)
    obs = cs.step25
    assert pytest.approx(obs) == exp

def test_step_indirect_costs(monkeypatch):
    """Test indirect cost calculations.

    :param monkeypatch: fixture for mocking
    :type monkeypatch: MonkeyPatch
    """
    # Mock cdirt and module vars being set
    monkeypatch.setattr(cv, "cdirt", 1.0e3)
    monkeypatch.setattr(cs, "step91", 0.0)
    monkeypatch.setattr(cs, "step92", 0.0)
    monkeypatch.setattr(cs, "step93", 0.0)
    
    # Run and assert module vars for costs
    cs.step_indirect_costs(0, 0)
    assert cs.step91 == 300
    assert cs.step92 == 325
    assert cs.step93 == 150
