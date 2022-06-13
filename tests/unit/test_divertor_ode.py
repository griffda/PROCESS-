from typing import List, NamedTuple
import numpy
import pytest

from process.fortran import divertor_ode, div_kal_vars, divertor_ode_var


class DifferentialContext(NamedTuple):
    eightemi48: float
    netau_sol: float
    lengthofwidesol: float
    area_target: float
    area_omp: float
    mi: float
    abserr_sol: float
    aplas: float
    eleion: float
    v01: float
    v02: float
    zeff_div: float
    impurity_concs: List[float]
    impurities_present: List[bool]


differntial_context = DifferentialContext(
    5320.9556296177216,
    0.5,
    9.912078498284858,
    0.15737105872405716,
    0.044567059923323327,
    4.1513473024999996e-27,
    0.0001,
    2.5,
    2.4032659949999999e-18,
    5541.86350714133,
    55418.6350714133,
    1.8556660323935243,
    [0, 0, 0, 0, 0.04, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [
        False,
        True,
        False,
        False,
        True,
        False,
        False,
        False,
        False,
        False,
        False,
        False,
        False,
        False,
    ],
)


@pytest.mark.parametrize(
    "t, y, expected_yp",
    (
        (
            0,
            [
                15.605745694752727,
                1.5605745694752728,
                2.2999999999999998,
                -17.296982513495614,
                1913.5036741348672,
                6.770759755977859,
                1,
                1,
                1,
                1,
            ],
            [
                -788.93029229063518,
                -7.9102629926316865,
                369.84349835647322,
                481.05199746313747,
                971186.24631028692,
                1294.7475890966286,
                7.5864740618042106,
                85.624591473503202,
                1019.1611724231047,
                182.3753511382165,
            ],
        ),
        (
            9.0967518203041023e-05,
            [
                15.533978664027849,
                1.5598549924824998,
                2.3336437451690188,
                -17.253222407159775,
                2001.8500766746413,
                6.8885397308573504,
                1.0006901227173142,
                1.0077890565834939,
                1.0927105625042315,
                1.0165902330744516,
            ],
            [
                -1204.8465357738346,
                -12.136613117438337,
                586.27679416542298,
                734.96895816689084,
                971691.56709368643,
                1762.7033577638458,
                14.077469787618167,
                103.79260013463018,
                1366.070186619022,
                278.76310122257513,
            ],
        ),
    ),
)
def test_differentail(t, y, expected_yp, monkeypatch):
    mock_params = [
        "eightemi48",
        "lengthofwidesol",
        "area_target",
        "area_omp",
        "mi",
        "aplas",
        "eleion",
        "v01",
        "v02",
        "zeff_div",
        "impurities_present",
    ]

    for p in mock_params:
        monkeypatch.setattr(divertor_ode, p, getattr(differntial_context, p))
    monkeypatch.setattr(div_kal_vars, "netau_sol", differntial_context.netau_sol)
    monkeypatch.setattr(div_kal_vars, "abserr_sol", differntial_context.abserr_sol)
    monkeypatch.setattr(
        divertor_ode_var, "impurity_concs", differntial_context.impurity_concs
    )

    yp = divertor_ode.differential(t, y)

    numpy.testing.assert_array_almost_equal(yp, expected_yp)
