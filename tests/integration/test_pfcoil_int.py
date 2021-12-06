"""Integration tests for the PFCoil module.

Vaguely realistic mocked values are taken from baseline2019 output, init values
in the pfcoil_variables module, or where necessary, guesses.

Many of these subroutines are long and perform multiple gets/sets on many "use" 
dependencies. As a result, many mocks are required to isolate the tests. There
are also many variables that could be asserted, so a few key variables central
to the testing of the subroutine have been chosen.
"""
import numpy as np
from numpy.testing import assert_array_almost_equal
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
    # Mocks for ohcalc()
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

    # Mocks for peakb()
    monkeypatch.setattr(bv, "iohcl", 1)
    monkeypatch.setattr(pfv, "waves", np.full([22, 6], 0.0))
    monkeypatch.setattr(pfv, "ngrp", 4)
    monkeypatch.setattr(pfv, "ncls", np.array([1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0]))
    monkeypatch.setattr(pfv, "curpfb", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "curpff", np.full(22, 0.0))
    monkeypatch.setattr(pfv, "curpfs", np.full(22, -175.84911993600002))
    monkeypatch.setattr(pv, "rmajor", 8.938)
    monkeypatch.setattr(pv, "plascur", 1.8254e7)

    # Mocks for hoop_stress()
    monkeypatch.setattr(tfv, "poisson_steel", 3.0e-1)

    # Mocks for superconpf()
    monkeypatch.setattr(eh, "fdiags", np.full(8, -9.99999e5))
    monkeypatch.setattr(tfv, "tmargmin_cs", 1.5)
    monkeypatch.setattr(tfv, "temp_margin", 0.0)
    monkeypatch.setattr(tfv, "b_crit_upper_nbti", 1.486e1)
    monkeypatch.setattr(tfv, "b_crit_upper_nbti", 9.04)

    pf.ohcalc()

    assert pytest.approx(pfv.bpf[4]) == 9.299805e2
    assert pytest.approx(pfv.rjohc) == -7.728453e9


def test_efc(monkeypatch):
    """Test efc subroutine.

    efc() requires specific arguments in order to work; these were discovered
    using gdb to break on the first call of efc() when running the baseline 2019
    IN.DAT.
    :param monkeypatch: mocking fixture
    :type monkeypatch: MonkeyPatch
    """
    ngrpmx = 10
    nclsmx = 2
    nptsmx = 32
    nfixmx = 64
    lrow1 = 2 * nptsmx + ngrpmx
    lcol1 = ngrpmx
    npts = 32
    rpts = np.array(
        [
            6.0547741935483881,
            6.2407887617065567,
            6.4268033298647254,
            6.612817898022894,
            6.7988324661810626,
            6.9848470343392313,
            7.1708616024973999,
            7.3568761706555676,
            7.5428907388137372,
            7.7289053069719049,
            7.9149198751300744,
            8.1009344432882422,
            8.2869490114464099,
            8.4729635796045795,
            8.658978147762749,
            8.8449927159209167,
            9.0310072840790845,
            9.217021852237254,
            9.4030364203954235,
            9.5890509885535913,
            9.775065556711759,
            9.9610801248699286,
            10.147094693028098,
            10.333109261186266,
            10.519123829344434,
            10.705138397502601,
            10.891152965660771,
            11.07716753381894,
            11.263182101977108,
            11.449196670135276,
            11.635211238293445,
            11.821225806451615,
        ]
    )
    zpts = np.full(nptsmx, 0.0)
    brin = np.full(nptsmx, 0.0)
    bzin = np.full(nptsmx, 0.0)
    nfix = 14
    rfix = np.full(nfixmx, 0.0)
    rfix[0:14] = 2.3936999999999999
    zfix = np.full(nfixmx, 0.0)
    zfix[0:14] = [
        0.56988544739721259,
        1.7096563421916378,
        2.8494272369860631,
        3.9891981317804879,
        5.1289690265749135,
        6.2687399213693382,
        7.4085108161637638,
        -0.56988544739721259,
        -1.7096563421916378,
        -2.8494272369860631,
        -3.9891981317804879,
        -5.1289690265749135,
        -6.2687399213693382,
        -7.4085108161637638,
    ]
    cfix = np.full(nfixmx, 0.0)
    cfix[0:14] = 12547065.315963898
    ngrp = 4
    ncls = np.array([1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0])

    # This 2D array argument discovered via gdb prints as a 1D array, therefore
    # needs to be reshaped into its original 2D. Fortran ordering is essential
    # when passing greater-than-1D arrays from Python to Fortran
    rcls = np.reshape(
        [
            6.7651653417201345,
            6.7651653417201345,
            18.597693381136555,
            17.000392357531304,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            18.597693381136555,
            17.000392357531304,
            0,
            0,
            0,
            0,
            0,
            0,
        ],
        (10, 2),
        order="F",
    )
    zcls = np.reshape(
        [
            9.8904697261474404,
            -11.124884737289973,
            2.883225806451613,
            8.0730322580645151,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            -2.883225806451613,
            -8.0730322580645151,
            0,
            0,
            0,
            0,
            0,
            0,
        ],
        (10, 2),
        order="F",
    )
    alfa = 5.0e-10
    bfix = np.full(lrow1, 0.0)
    gmat = np.full([lrow1, lcol1], 0.0, order="F")
    bvec = np.full(lrow1, 0.0)
    rc = np.full(nclsmx, 0.0)
    zc = np.full(nclsmx, 0.0)
    cc = np.full(nclsmx, 0.0)
    xc = np.full(nclsmx, 0.0)
    umat = np.full([lrow1, lcol1], 0.0, order="F")
    vmat = np.full([lrow1, lcol1], 0.0, order="F")
    sigma = np.full(ngrpmx, 0.0)
    work2 = np.full(ngrpmx, 0.0)

    ssq, ccls = pf.efc(
        npts,
        rpts,
        zpts,
        brin,
        bzin,
        nfix,
        rfix,
        zfix,
        cfix,
        ngrp,
        ncls,
        rcls,
        zcls,
        alfa,
        bfix,
        gmat,
        bvec,
        rc,
        zc,
        cc,
        xc,
        umat,
        vmat,
        sigma,
        work2,
    )

    assert pytest.approx(ssq) == 4.208729e-4
    assert pytest.approx(ccls[0:4]) == np.array(
        [
            12846165.42893886,
            16377261.02000236,
            579111.6216917,
            20660782.82356247,
        ]
    )


def test_mtrx():
    """Test mtrx subroutine.

    mtrx() requires specific arguments in order to work; these were discovered
    using gdb to break on the first call of mtrx() when running the baseline 2019
    IN.DAT.
    """
    nptsmx = 32
    lcol1 = 10
    npts = 32
    rpts = np.array(
        [
            6.0547741935483881,
            6.2407887617065567,
            6.4268033298647254,
            6.612817898022894,
            6.7988324661810626,
            6.9848470343392313,
            7.1708616024973999,
            7.3568761706555676,
            7.5428907388137372,
            7.7289053069719049,
            7.9149198751300744,
            8.1009344432882422,
            8.2869490114464099,
            8.4729635796045795,
            8.658978147762749,
            8.8449927159209167,
            9.0310072840790845,
            9.217021852237254,
            9.4030364203954235,
            9.5890509885535913,
            9.775065556711759,
            9.9610801248699286,
            10.147094693028098,
            10.333109261186266,
            10.519123829344434,
            10.705138397502601,
            10.891152965660771,
            11.07716753381894,
            11.263182101977108,
            11.449196670135276,
            11.635211238293445,
            11.821225806451615,
        ]
    )
    zpts = np.zeros(nptsmx)
    brin = np.zeros(nptsmx)
    bzin = np.zeros(nptsmx)
    ngrp = 4
    ncls = np.array([1, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0])
    rcls = np.reshape(
        [
            0,
            0,
            0,
            18.597693381136555,
            17.000392357531304,
            0,
            0,
            0,
            0,
            0,
            0,
            6.9169190417774516e-323,
            4.9406564584124654e-324,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
        ],
        (10, 2),
        order="F",
    )
    zcls = np.reshape(
        [
            0,
            0,
            0,
            -2.883225806451613,
            -8.0730322580645151,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
        ],
        (10, 2),
        order="F",
    )
    alfa = 5.0e-10
    bfix = np.array(
        [
            -4.163336342344337e-17,
            0,
            1.3877787807814457e-17,
            -3.4694469519536142e-17,
            -6.9388939039072284e-18,
            2.0816681711721685e-17,
            -1.3877787807814457e-17,
            3.1225022567582528e-17,
            1.3877787807814457e-17,
            2.7755575615628914e-17,
            3.1225022567582528e-17,
            1.0408340855860843e-17,
            -4.163336342344337e-17,
            -3.4694469519536142e-18,
            -1.7347234759768071e-17,
            1.3877787807814457e-17,
            -2.0816681711721685e-17,
            1.7347234759768071e-17,
            -2.4286128663675299e-17,
            0,
            -3.4694469519536142e-18,
            -1.0408340855860843e-17,
            0,
            0,
            1.7347234759768071e-18,
            -1.0408340855860843e-17,
            1.7347234759768071e-18,
            -6.9388939039072284e-18,
            6.9388939039072284e-18,
            -6.9388939039072284e-18,
            3.4694469519536142e-18,
            5.2041704279304213e-18,
            -0.3130693525427572,
            -0.30317412503141067,
            -0.29349361903088056,
            -0.28403698539500122,
            -0.27481156279537977,
            -0.26582301409269415,
            -0.25707546259584763,
            -0.24857162691582502,
            -0.24031295298976921,
            -0.23229974199262643,
            -0.22453127307719892,
            -0.21700592011907616,
            -0.20972126186523041,
            -0.20267418508484636,
            -0.19586098049525957,
            -0.18927743138451436,
            -0.18291889497602498,
            -0.17678037668189961,
            -0.17085659747167894,
            -0.16514205464473081,
            -0.15963107633981927,
            -0.15431787014642362,
            -0.14919656620141122,
            -0.1442612551639135,
            -0.13950602146198512,
            -0.13492497219911381,
            -0.13051226209776626,
            -0.12626211484245656,
            -0.12216884116737772,
            -0.11822685401402291,
            -0.11443068106371825,
            -0.11077497492862906,
            1.244050972187992e-316,
            9.655957481515668e-97,
            0,
            6.9533558071276999e-310,
            6.9533558069559627e-310,
            6.9533474562307984e-310,
            0,
            0,
            1.2440161899665248e-316,
            9.655957481515668e-97,
        ]
    )

    nrws, gmat, bvec, rc, zc, cc, xc = pf.mtrx(
        lcol1,
        npts,
        rpts,
        zpts,
        brin,
        bzin,
        ngrp,
        ncls,
        rcls,
        zcls,
        alfa,
        bfix,
    )

    gmat_exp = np.array(
        [
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                2.92158969e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                3.04960151e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                3.18186728e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                3.31865171e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                3.46023678e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                3.60692320e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                3.75903204e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                3.91690643e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                4.08091348e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                4.25144640e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                4.42892676e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                4.61380701e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                4.80657328e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                5.00774837e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                5.21789513e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                5.43762013e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                5.66757773e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                5.90847452e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                6.16107431e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                6.42620349e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                6.70475709e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                6.99770541e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                7.30610130e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                7.63108823e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                7.97390922e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                8.33591662e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                8.71858292e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                9.12351264e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                9.55245534e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                1.00073199e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                1.04901901e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                1.10033415e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -3.30317878e-19,
                -3.30317878e-19,
                -6.60635757e-19,
                3.50145552e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -3.20472338e-19,
                -3.20472338e-19,
                -6.40944676e-19,
                3.51776252e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -3.11196728e-19,
                -3.11196728e-19,
                -6.22393456e-19,
                3.53472730e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -3.02442952e-19,
                -3.02442952e-19,
                -6.04885904e-19,
                3.55236630e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.94168179e-19,
                -2.94168179e-19,
                -5.88336358e-19,
                3.57069672e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.86334140e-19,
                -2.86334140e-19,
                -5.72668279e-19,
                3.58973648e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.78906536e-19,
                -2.78906536e-19,
                -5.57813071e-19,
                3.60950425e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.71854537e-19,
                -2.71854537e-19,
                -5.43709074e-19,
                3.63001946e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.65150356e-19,
                -2.65150356e-19,
                -5.30300712e-19,
                3.65130230e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.58768880e-19,
                -2.58768880e-19,
                -5.17537759e-19,
                3.67337373e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.52687355e-19,
                -2.52687355e-19,
                -5.05374710e-19,
                3.69625546e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.46885119e-19,
                -2.46885119e-19,
                -4.93770239e-19,
                3.71996994e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.41343366e-19,
                -2.41343366e-19,
                -4.82686732e-19,
                3.74454037e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.36044938e-19,
                -2.36044938e-19,
                -4.72089877e-19,
                3.76999062e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.30974156e-19,
                -2.30974156e-19,
                -4.61948311e-19,
                3.79634522e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.26116655e-19,
                -2.26116655e-19,
                -4.52233310e-19,
                3.82362931e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.21459257e-19,
                -2.21459257e-19,
                -4.42918515e-19,
                3.85186853e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.16989848e-19,
                -2.16989848e-19,
                -4.33979695e-19,
                3.88108893e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.12697269e-19,
                -2.12697269e-19,
                -4.25394538e-19,
                3.91131688e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.08571231e-19,
                -2.08571231e-19,
                -4.17142461e-19,
                3.94257887e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.04602225e-19,
                -2.04602225e-19,
                -4.09204451e-19,
                3.97490133e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -2.00781456e-19,
                -2.00781456e-19,
                -4.01562911e-19,
                4.00831037e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.97100769e-19,
                -1.97100769e-19,
                -3.94201538e-19,
                4.04283150e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.93552600e-19,
                -1.93552600e-19,
                -3.87105201e-19,
                4.07848927e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.90129919e-19,
                -1.90129919e-19,
                -3.80259839e-19,
                4.11530678e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.86826185e-19,
                -1.86826185e-19,
                -3.73652370e-19,
                4.15330516e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.83635302e-19,
                -1.83635302e-19,
                -3.67270604e-19,
                4.19250290e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.80551586e-19,
                -1.80551586e-19,
                -3.61103172e-19,
                4.23291505e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.77569727e-19,
                -1.77569727e-19,
                -3.55139453e-19,
                4.27455221e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.74684759e-19,
                -1.74684759e-19,
                -3.49369519e-19,
                4.31741941e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.71892037e-19,
                -1.71892037e-19,
                -3.43784075e-19,
                4.36151462e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                -1.69187206e-19,
                -1.69187206e-19,
                -3.38374412e-19,
                4.40682706e-08,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                5.00000000e-10,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                5.00000000e-10,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                1.00000000e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                1.00000000e-09,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
            [
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
                0.00000000e00,
            ],
        ]
    )

    bvec_exp = np.array(
        [
            4.16333634e-17,
            0.00000000e00,
            -1.38777878e-17,
            3.46944695e-17,
            6.93889390e-18,
            -2.08166817e-17,
            1.38777878e-17,
            -3.12250226e-17,
            -1.38777878e-17,
            -2.77555756e-17,
            -3.12250226e-17,
            -1.04083409e-17,
            4.16333634e-17,
            3.46944695e-18,
            1.73472348e-17,
            -1.38777878e-17,
            2.08166817e-17,
            -1.73472348e-17,
            2.42861287e-17,
            0.00000000e00,
            3.46944695e-18,
            1.04083409e-17,
            0.00000000e00,
            0.00000000e00,
            -1.73472348e-18,
            1.04083409e-17,
            -1.73472348e-18,
            6.93889390e-18,
            -6.93889390e-18,
            6.93889390e-18,
            -3.46944695e-18,
            -5.20417043e-18,
            3.13069353e-01,
            3.03174125e-01,
            2.93493619e-01,
            2.84036985e-01,
            2.74811563e-01,
            2.65823014e-01,
            2.57075463e-01,
            2.48571627e-01,
            2.40312953e-01,
            2.32299742e-01,
            2.24531273e-01,
            2.17005920e-01,
            2.09721262e-01,
            2.02674185e-01,
            1.95860980e-01,
            1.89277431e-01,
            1.82918895e-01,
            1.76780377e-01,
            1.70856597e-01,
            1.65142055e-01,
            1.59631076e-01,
            1.54317870e-01,
            1.49196566e-01,
            1.44261255e-01,
            1.39506021e-01,
            1.34924972e-01,
            1.30512262e-01,
            1.26262115e-01,
            1.22168841e-01,
            1.18226854e-01,
            1.14430681e-01,
            1.10774975e-01,
            0.00000000e00,
            0.00000000e00,
            0.00000000e00,
            0.00000000e00,
            0.00000000e00,
            0.00000000e00,
            0.00000000e00,
            0.00000000e00,
            0.00000000e00,
            0.00000000e00,
        ]
    )
    rc_exp = np.array([18.59769338, 0.0])
    zc_exp = np.array([-2.88322581, 0.0])
    cc_exp = np.array([1.0, 1.0])
    xc_exp = np.array([1.65994082e-05, -1.48549917e-16])

    assert nrws == 68
    assert_array_almost_equal(gmat, gmat_exp)
    assert_array_almost_equal(bvec, bvec_exp)
    assert_array_almost_equal(rc, rc_exp)
    assert_array_almost_equal(zc, zc_exp)
    assert_array_almost_equal(cc, cc_exp)
    assert_array_almost_equal(xc, xc_exp)
