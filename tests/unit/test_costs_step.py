"""Unit tests for costs_step.f90."""
from process.fortran import costs_step_module as cs
from process.fortran import tfcoil_variables as tfv
from process.fortran import cost_variables as cv
from process.fortran import physics_variables as pv
import numpy as np
import pytest

def test_step_a22010301(monkeypatch):
    """Cost of TF coils for different materials (22.01.03.01).

    :param monkeypatch: fixture for mocking variables
    :type monkeypatch: MonkeyPatch
    """
    # Mock dependencies with realistic values
    monkeypatch.setattr(cv, "step_ref", np.zeros(68, order="F"))
    # Only mock used array elements
    cv.step_ref[21] = 1.2572e2
    monkeypatch.setattr(cv, "cpstcst", 0.0)
    monkeypatch.setattr(cv, "fkind", 1.0)
    monkeypatch.setattr(cv, "lsa", 4)
    monkeypatch.setattr(cv, "cfind", np.zeros(4, order="F"))
    cv.cfind[3] = 0.29
    monkeypatch.setattr(cv, "ifueltyp", 0)
    monkeypatch.setattr(cv, "step_uc_cryo_al", 81.0)
    monkeypatch.setattr(cv, "step_mc_cryo_al_per", 20.0)
    monkeypatch.setattr(cv, "uccpcl1", 250.0)
    monkeypatch.setattr(cv, "uccpclb", 150.0)
    monkeypatch.setattr(tfv, "whtconal", 1.0e4)
    monkeypatch.setattr(tfv, "n_tf", 16.0)
    monkeypatch.setattr(tfv, "whttflgs", 0.0)
    monkeypatch.setattr(tfv, "whtcp", 1.0e4)
    monkeypatch.setattr(pv, "itart", 0)
    monkeypatch.setattr(cs, "vfi", 5e3)
    monkeypatch.setattr(cs, "vfi_star", 6.737e3)
    
    # Copper coils
    monkeypatch.setattr(tfv, "i_tf_sup", 0)
    expected = 2.16775
    observed = cs.step_a22010301()
    assert pytest.approx(observed) == expected

    # Superconducting coils
    monkeypatch.setattr(tfv, "i_tf_sup", 1)
    expected = 93.30563
    observed = cs.step_a22010301()
    assert pytest.approx(observed) == expected

    # Cryo-aluminium coils
    monkeypatch.setattr(tfv, "i_tf_sup", 2)
    expected = 15.552
    observed = cs.step_a22010301()
    assert pytest.approx(observed) == expected