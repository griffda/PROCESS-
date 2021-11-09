"""Unit tests for pfcoil.f90."""
import pytest
from process.pfcoil import PFCoil
from process.fortran import pfcoil_module as pf

@pytest.fixture
def pfcoil():
    """Fixture to create a PFCoil object.

    :return: an instance of PFCoil
    :rtype: process.pfcoil.PFCoil
    """
    pfcoil = PFCoil()

    return pfcoil

def test_init_pfcoil(pfcoil):
    """Test initialisation of Fortran module variables.

    :param pfcoil: PFCoil object
    :type pfcoil: process.pfcoil.PFCoil
    """
    # Test a selection of module variables
    assert pf.ssq0 == 0.0
    assert pf.cslimit == False
    assert pf.nef == 0