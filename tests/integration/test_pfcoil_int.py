"""Integration tests for the PFCoil module.

Vaguely realistic mocked values are taken from baseline2019 output, init values
in the pfcoil_variables module, or where necessary, guesses.

Many of these subroutines are long and perform multiple gets/sets on many "use" 
dependencies. As a result, many mocks are required to isolate the tests. There
are also many variables that could be asserted, so a few key variables central
to the testing of the subroutine have been chosen.
"""
import numpy as np
import pytest
from process.fortran import pfcoil_module as pf
from process.fortran import build_variables as bv
from process.fortran import pfcoil_variables as pfv
from process.fortran import physics_variables as pv
from process.fortran import error_handling as eh
from process.fortran import fwbs_variables as fwbsv
from process.fortran import tfcoil_variables as tfv
from process.fortran import times_variables as tv
from process.fortran import constants


def test_pfcoil(monkeypatch):
    """Test pfcoil subroutine.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    """
    monkeypatch.setattr(bv, "iohcl", 1)
    monkeypatch.setattr(bv, "hpfdif", 0.0)
    monkeypatch.setattr(bv, "hpfu", 4.0)  # guess
    monkeypatch.setattr(bv, "hmax", 8.8)
    monkeypatch.setattr(bv, "ohcth", 0.65)
    monkeypatch.setattr(bv, "tfthko", 1.4)
    monkeypatch.setattr(bv, "tfcth", 1.4)
    monkeypatch.setattr(bv, "r_tf_outboard_mid", 1.66e1)
    monkeypatch.setattr(bv, "bore", 2.15)
    monkeypatch.setattr(eh, "idiags", np.full(8, -999999))
    monkeypatch.setattr(fwbsv, "denstl", 7.8e3)
    monkeypatch.setattr(pfv, "rpf1", 0.0)
    monkeypatch.setattr(pfv, "whtpfs", 0.0)
    monkeypatch.setattr(pfv, "curpff", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "nohc", 0)
    monkeypatch.setattr(pfv, "pfrmax", 0.0)
    monkeypatch.setattr(pfv, "fcohbop", 1.0)
    monkeypatch.setattr(pfv, "rjconpf", np.full(22, 1.1e7))
    monkeypatch.setattr(pfv, "ngrp", 4)
    monkeypatch.setattr(pfv, "rohc", 0.0)
    monkeypatch.setattr(pfv, "ncls", np.array([1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0]))
    monkeypatch.setattr(pfv, "zpf", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "cptdin", np.full(22, 4.22e4))
    monkeypatch.setattr(pfv, "pfcaseth", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "itr_sum", 0.0)
    monkeypatch.setattr(pfv, "sigpfcf", 6.66e-1)
    monkeypatch.setattr(pfv, "ohhghf", 9.0e-1)
    monkeypatch.setattr(pfv, "ipfloc", np.array([2, 2, 3, 3, 0, 0, 0, 0, 0, 0]))
    monkeypatch.setattr(pfv, "wts", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "powpfres", 0.0)
    monkeypatch.setattr(pfv, "curpfb", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "routr", 1.5)
    monkeypatch.setattr(pfv, "ric", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "fcohbof", 2.654e-1)
    monkeypatch.setattr(pfv, "rpf2", -1.825)
    monkeypatch.setattr(pfv, "nfxfh", 7)
    monkeypatch.setattr(pfv, "bpf", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "zl", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "wtc", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "vf", np.full(22, 3.0e-1))
    monkeypatch.setattr(pfv, "turns", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "curpfs", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "rpf", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "zref", [3.6, 1.2, 2.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0])
    monkeypatch.setattr(pfv, "pfmmax", 0.0)
    monkeypatch.setattr(pfv, "ipfres", 0)
    monkeypatch.setattr(pfv, "alfapf", 5.0e-10)
    monkeypatch.setattr(pfv, "ncirt", 8)
    monkeypatch.setattr(pfv, "pfclres", 2.5e-8)
    monkeypatch.setattr(pfv, "cpt", np.full([22, 6], 0.0))
    monkeypatch.setattr(pfv, "waves", np.full([22, 6], 0.0))
    monkeypatch.setattr(pfv, "sxlg", np.full([22, 22], 0.0))
    monkeypatch.setattr(pfv, "sigpfcalw", 5.0e2)
    monkeypatch.setattr(pfv, "coheof", 1.6932e7)
    monkeypatch.setattr(pfv, "zh", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "fcohbof", 2.654e-1)
    monkeypatch.setattr(pfv, "ra", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "rb", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "isumatpf", 3)
    monkeypatch.setattr(pfv, "isumatoh", 1)
    monkeypatch.setattr(pfv, "whtpf", 0.0)
    monkeypatch.setattr(pfv, "fcupfsu", 6.900e-1)
    monkeypatch.setattr(pfv, "cohbop", 1.693e7)
    monkeypatch.setattr(pfv, "rjpfalw", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "i_sup_pf_shape", 0)
    monkeypatch.setattr(pfv, "rref", np.full(10, 7.0))
    monkeypatch.setattr(pfv, "i_pf_current", 1)
    monkeypatch.setattr(pfv, "ccl0_ma", np.full(10, 0.0))
    monkeypatch.setattr(pfv, "ccls_ma", np.full(10, 0.0))
    monkeypatch.setattr(pv, "bvert", -6.51e-1)
    monkeypatch.setattr(pv, "kappa", 1.727)
    monkeypatch.setattr(pv, "rli", 1.693)
    monkeypatch.setattr(pv, "itartpf", 0)
    monkeypatch.setattr(pv, "vsres", 6.151e1)
    monkeypatch.setattr(pv, "plascur", 1.8254e7)
    monkeypatch.setattr(pv, "triang", 0.413)
    monkeypatch.setattr(pv, "rminor", 2.883)
    monkeypatch.setattr(pv, "rmajor", 8.938)
    monkeypatch.setattr(pv, "vsind", 3.497e2)
    monkeypatch.setattr(pv, "aspect", 3.1)
    monkeypatch.setattr(pv, "itart", 0)
    monkeypatch.setattr(pv, "betap", 6.313e-1)
    monkeypatch.setattr(tfv, "tftmp", 4.750)
    monkeypatch.setattr(tfv, "dcond", np.full(9, 9.0e3))
    monkeypatch.setattr(tfv, "i_tf_sup", 1)
    monkeypatch.setattr(tfv, "fhts", 0.5)
    monkeypatch.setattr(tfv, "tcritsc", 1.6e1)
    monkeypatch.setattr(tfv, "strncon_pf", -5.0e-3)
    monkeypatch.setattr(tfv, "bcritsc", 2.4e1)
    monkeypatch.setattr(tfv, "b_crit_upper_nbti", 1.486e1)
    monkeypatch.setattr(tfv, "t_crit_nbti", 9.04)
    monkeypatch.setattr(tv, "tim", np.full(6, 0.0))
    monkeypatch.setattr(tv, "tramp", 5.0e2)
    monkeypatch.setattr(tv, "tburn", 7.1263e-1)
    monkeypatch.setattr(tv, "tohs", 1.82538e2)
    monkeypatch.setattr(tv, "tqnch", 1.82538e2)
    monkeypatch.setattr(tv, "theat", 1.0e1)
    monkeypatch.setattr(constants, "dcopper", 8.9e3)

    pf.pfcoil()

    assert pytest.approx(pv.bvert) == -0.65121393
    assert pytest.approx(pfv.zpf) == np.array(
        [
            4.86,
            -4.86,
            7.2075,
            -7.2075,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
            0.0,
        ]
    )


def test_ohcalc(monkeypatch):
    """Test ohcalc subroutine.

    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    """
    monkeypatch.setattr(bv, "hmax", 8.864)
    monkeypatch.setattr(bv, "ohcth", 6.510e-1)
    monkeypatch.setattr(fwbsv, "denstl", 7.8e3)
    monkeypatch.setattr(eh, "idiags", np.full(8, 0))
    monkeypatch.setattr(pfv, "nohc", 5)
    monkeypatch.setattr(pfv, "bmaxoh", 1.4e1)
    monkeypatch.setattr(pfv, "i_cs_stress", 0)
    monkeypatch.setattr(pfv, "coheof", 1.693e7)
    monkeypatch.setattr(pfv, "rohc", 0.0)
    monkeypatch.setattr(pfv, "vfohc", 3.0e-1)
    monkeypatch.setattr(pfv, "jstrandoh_bop", 1.069e8)
    monkeypatch.setattr(pfv, "fcuohsu", 7.000e-1)
    monkeypatch.setattr(pfv, "isumatoh", 5)
    monkeypatch.setattr(pfv, "ohhghf", 0.9)
    monkeypatch.setattr(pfv, "areaoh", 1.039e1)
    monkeypatch.setattr(pfv, "powpfres", 0.0)
    monkeypatch.setattr(pfv, "jstrandoh_eof", 1.427e8)
    monkeypatch.setattr(pfv, "powohres", 0.0)
    monkeypatch.setattr(pfv, "rjohc0", 3.048e7)
    monkeypatch.setattr(pfv, "s_tresca_oh", 5.718e8)
    monkeypatch.setattr(pfv, "awpoh", 4.232)
    monkeypatch.setattr(pfv, "oh_steel_frac", 5.926e-1)
    monkeypatch.setattr(pfv, "bmaxoh0", 1.4e1)
    monkeypatch.setattr(pfv, "rjohc", 4.070e7)
    monkeypatch.setattr(pfv, "tmargoh", 1.5)
    monkeypatch.setattr(pfv, "ipfres", 0)
    monkeypatch.setattr(pfv, "rjpfalw", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "pfclres", 2.8e-8)
    monkeypatch.setattr(pfv, "vf", np.full(22, 0.3))
    monkeypatch.setattr(pfv, "ric", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "bpf", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "jscoh_eof", 4.758e8)
    monkeypatch.setattr(pfv, "zpf", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "rb", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "ra", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "jscoh_bop", 3.562e8)
    monkeypatch.setattr(pfv, "cptdin", np.full(22, 4.22e4))
    monkeypatch.setattr(pfv, "pfcaseth", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "rpf", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "cohbop", 1.693e7)
    monkeypatch.setattr(pfv, "zh", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "wtc", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "zl", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "turns", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "wts", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "a_oh_turn", 0.0)
    monkeypatch.setattr(tfv, "dcond", np.full(9, 9.0e3))
    monkeypatch.setattr(tfv, "tftmp", 4.750)
    monkeypatch.setattr(tfv, "tcritsc", 1.6e1)
    monkeypatch.setattr(tfv, "strncon_cs", -5.000e-3)
    monkeypatch.setattr(tfv, "fhts", 0.5)
    monkeypatch.setattr(tfv, "bcritsc", 2.4e1)
    monkeypatch.setattr(tfv, "b_crit_upper_nbti", 1.486e1)
    monkeypatch.setattr(tfv, "t_crit_nbti", 9.04)
    monkeypatch.setattr(constants, "dcopper", 8.9e3)
    monkeypatch.setattr(pfv, "curpfs", np.full(22, -175.84911993600002))

    pf.ohcalc()

    assert pytest.approx(pfv.bpf[4]) == 9.286960e2
    assert pytest.approx(pfv.rjohc) == -7.717510e9
