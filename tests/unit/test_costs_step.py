"""Unit tests for costs_step.f90."""
from process.fortran import costs_step_module as cs
from process.fortran import cost_variables as cv

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
