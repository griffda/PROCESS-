"""Unit tests for plasma_geometry.f90."""

import pytest
from typing import NamedTuple, Any
from process.fortran import plasma_geometry_module


class XparamParam(NamedTuple):

    a: Any = None

    kap: Any = None

    tri: Any = None

    expected_xi: Any = None

    expected_thetai: Any = None

    expected_xo: Any = None

    expected_thetao: Any = None


@pytest.mark.parametrize(
    "xparamparam",
    (
        XparamParam(
            a=2.8677741935483869,
            kap=1.8480000000000001,
            tri=0.5,
            expected_xi=10.510690667870968,
            expected_thetai=0.52847258461252744,
            expected_xo=5.4154130183225808,
            expected_thetao=1.3636548755403939,
        ),
        XparamParam(
            a=2.8677741935483869,
            kap=1.8480000000000001,
            tri=0.5,
            expected_xi=10.510690667870968,
            expected_thetai=0.52847258461252744,
            expected_xo=5.4154130183225808,
            expected_thetao=1.3636548755403939,
        ),
    ),
)
def test_xparam(xparamparam, monkeypatch):
    """
    Automatically generated Regression Unit Test for xparam.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param xparamparam: the data used to mock and assert in this test.
    :type xparamparam: xparamparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    xi, thetai, xo, thetao = plasma_geometry_module.xparam(
        a=xparamparam.a, kap=xparamparam.kap, tri=xparamparam.tri
    )

    assert xi == pytest.approx(xparamparam.expected_xi)

    assert thetai == pytest.approx(xparamparam.expected_thetai)

    assert xo == pytest.approx(xparamparam.expected_xo)

    assert thetao == pytest.approx(xparamparam.expected_thetao)
