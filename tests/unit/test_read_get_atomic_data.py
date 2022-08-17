from typing import NamedTuple
import pytest
from process.fortran import read_and_get_atomic_data


class HRateParameter(NamedTuple):
    density: float
    temperature: float
    mass: float
    expected_s: float
    expected_al: float
    expected_rcx: float
    expected_plt: float
    expected_prb: float


@pytest.mark.parametrize(
    "h_rate_param",
    (
        HRateParameter(
            1.2994667740136895e21,
            2.3,
            2.5,
            2.1617080648757677e-16,
            6.878887218709737e-19,
            7.8783848997902079e-15,
            2.4253133343946401e-34,
            1.3795929178688193e-36,
        ),
    ),
)
def test_get_h_rates(h_rate_param: HRateParameter):
    s, al, rcx, plt, prb = read_and_get_atomic_data.get_h_rates(
        h_rate_param.density, h_rate_param.temperature, h_rate_param.mass, False
    )

    assert s == pytest.approx(h_rate_param.expected_s)
    assert al == pytest.approx(h_rate_param.expected_al)
    assert rcx == pytest.approx(h_rate_param.expected_rcx)
    assert plt == pytest.approx(h_rate_param.expected_plt)
    assert prb == pytest.approx(h_rate_param.expected_prb)
