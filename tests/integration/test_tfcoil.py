"""Integration tests for tfcoil.f90."""

import collections
import pytest

from process.fortran import tfcoil_module as tf
from process.fortran import tfcoil_variables as tfv
from process.fortran import build_variables as bv
from process.fortran import fwbs_variables as fwbsv
from process.fortran import error_handling as eh
from process import fortran as ft


CntrpstTestAsset = collections.namedtuple('CntrpstTestAsset', ['i_tf_sup', 'tcoolin', 'expected_dtiocool', 'expected_tcpav2', 'expected_tcpmax', 'expected_ppump'])
"""Test asset for a test case of cntrpst

:i_tf_sup: value for tfcoil_variables.i_tf_sup to be mocked with (0=Copper, 2=Cryogenic aluminium)
:type i_tf_sup: integer
:tcoolin: value for tfcoil_variables.tcoolin to be mocked with (centrepost coolant inlet temperature)
:type tcoolin: float

:expected_dtiocool: expected value of tfcoil_variables.dtiocool after tfcoil.cntrpst routine has run
:type expected_dtiocool: float
:expected_tcpav2: expected value of tfcoil_variables.tcpav2 after tfcoil.cntrpst routine has run
:type expected_tcpav2: float
:expected_tcpmax: expected value of tfcoil_variables.tcpmax after tfcoil.cntrpst routine has run
:type expected_tcpmax: float
:expected_ppump: expected value of tfcoil_variables.ppump after tfcoil.cntrpst routine has run
:type expected_ppump: float
"""

@pytest.mark.parametrize('cntrpst_asset',[
    CntrpstTestAsset(0, 100.0, 0.00075899, 100.00109611, 100.00147829, 7.05905966e+08),
    # CntrpstTestAsset(1, 45.0),
    CntrpstTestAsset(2, 43.6, 0.00645998, 43.60678774, 43.61001841, 80926408.5501315)
])
def test_cntrpst(cntrpst_asset, monkeypatch, initialise_error_module):
    """Integration test for cntrpst

    Testing tfcoil module variables being set:
        - dtiocool
        - tcpav2
        - tcpmax
        - ppump
    
    :param cntrpst_asset: test asset containing values to mock and expected results for the represented test case
    :type cntrpst_asset: CntrpstTestAsset

    :param monkeypatch: Mock fixture
    :type monkeypatch: object
    
    :param initialise_error_module: Fixture to setup error handling module
    :type initialise_error_module: tests.integration.conftest.initialise_error_module
    """
    monkeypatch.setattr(tfv, 'a_cp_cool', 1)
    monkeypatch.setattr(tfv, 'n_tf', 16)
    monkeypatch.setattr(tfv, 'rcool', 0.005)
    monkeypatch.setattr(tfv, 'vcool', 20.0)
    monkeypatch.setattr(tfv, 'vol_cond_cp', 2)
    monkeypatch.setattr(tfv, 'prescp', 1)
    monkeypatch.setattr(tfv, 'i_tf_sup', cntrpst_asset.i_tf_sup)
    monkeypatch.setattr(tfv, 'tcoolin', cntrpst_asset.tcoolin)
    monkeypatch.setattr(fwbsv, 'pnuc_cp_tf', 1)
    monkeypatch.setattr(bv, 'hmax', 1)
    monkeypatch.setattr(bv, 'tfthko', 0.5)

    tf.cntrpst(ft.constants.nout,0)

    # appears to be the same for all cases?
    assert pytest.approx(tfv.ncool) == 203718.3271576
    
    assert pytest.approx(tfv.dtiocool, abs=1e-8) ==  cntrpst_asset.expected_dtiocool
    assert pytest.approx(tfv.tcpav2) ==  cntrpst_asset.expected_tcpav2
    assert pytest.approx(tfv.tcpmax) ==  cntrpst_asset.expected_tcpmax
    assert pytest.approx(tfv.ppump) ==  cntrpst_asset.expected_ppump
   