import pytest
from typing import NamedTuple, Any

from process.fortran import sctfcoil_module
from process.fortran import tfcoil_variables
from process.fortran import global_variables
from process.sctfcoil import Sctfcoil


@pytest.fixture
def sctfcoil():
    """Provides Sctfcoil object for testing.

    :returns: initialised Sctfcoil object
    :rtype: process.sctfcoil.Sctfcoil
    """
    return Sctfcoil()


class ProtectParam(NamedTuple):

    aio: Any = None

    tfes: Any = None

    acs: Any = None

    aturn: Any = None

    tdump: Any = None

    fcond: Any = None

    fcu: Any = None

    tba: Any = None

    tmax: Any = None

    expected_ajwpro: Any = None

    expected_vd: Any = None


@pytest.mark.parametrize(
    "protectparam",
    (
        ProtectParam(
            aio=74026.751437500003,
            tfes=9561415368.8360519,
            acs=0.001293323051622732,
            aturn=0.0032012300777680192,
            tdump=25.829000000000001,
            fcond=0.63927285511442711,
            fcu=0.80884,
            tba=4.75,
            tmax=150,
            expected_ajwpro=17475706.393616617,
            expected_vd=10001.287165953383,
        ),
    ),
)
def test_protect(protectparam, monkeypatch):
    """
    Automatically generated Regression Unit Test for protect.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param protectparam: the data used to mock and assert in this test.
    :type protectparam: protectparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    ajwpro, vd = sctfcoil_module.protect(
        aio=protectparam.aio,
        tfes=protectparam.tfes,
        acs=protectparam.acs,
        aturn=protectparam.aturn,
        tdump=protectparam.tdump,
        fcond=protectparam.fcond,
        fcu=protectparam.fcu,
        tba=protectparam.tba,
        tmax=protectparam.tmax,
    )

    assert ajwpro == pytest.approx(protectparam.expected_ajwpro)

    assert vd == pytest.approx(protectparam.expected_vd)


class SuperconParam(NamedTuple):

    tmargmin_tf: Any = None

    n_tf: Any = None

    temp_margin: Any = None

    jwdgpro: Any = None

    dhecoil: Any = None

    cpttf: Any = None

    bmaxtfrp: Any = None

    str_tf_con_res: Any = None

    b_crit_upper_nbti: Any = None

    i_str_wp: Any = None

    str_wp: Any = None

    t_crit_nbti: Any = None

    tf_fit_t: Any = None

    tf_fit_z: Any = None

    tf_fit_y: Any = None

    run_tests: Any = None

    isumat: Any = None

    iprint: Any = None

    outfile: Any = None

    acs: Any = None

    aturn: Any = None

    bmax: Any = None

    fcu: Any = None

    fhe: Any = None

    fhts: Any = None

    iop: Any = None

    jwp: Any = None

    tdmptf: Any = None

    tfes: Any = None

    thelium: Any = None

    tmax: Any = None

    bcritsc: Any = None

    tcritsc: Any = None

    expected_temp_margin: Any = None

    expected_jwdgpro: Any = None

    expected_jwdgcrt: Any = None

    expected_vd: Any = None

    expected_tmarg: Any = None


@pytest.mark.parametrize(
    "superconparam",
    (
        SuperconParam(
            tmargmin_tf=1.5,
            n_tf=16,
            temp_margin=0,
            jwdgpro=0,
            dhecoil=0.010000000000000002,
            cpttf=74026.751437500003,
            bmaxtfrp=12.48976756562082,
            str_tf_con_res=-0.0050000000000000001,
            b_crit_upper_nbti=14.859999999999999,
            i_str_wp=1,
            str_wp=0.0015619754370069119,
            t_crit_nbti=9.0399999999999991,
            tf_fit_t=0.80807838916035957,
            tf_fit_z=0.3149613642807837,
            tf_fit_y=1.0658869305062604,
            run_tests=0,
            isumat=5,
            iprint=0,
            outfile=11,
            acs=0.001293323051622732,
            aturn=0.0032012300777680192,
            bmax=12.48976756562082,
            fcu=0.80884,
            fhe=0.30000000000000004,
            fhts=0.5,
            iop=74026.751437500003,
            jwp=23124470.793774806,
            tdmptf=25.829000000000001,
            tfes=9548964780.4287167,
            thelium=4.75,
            tmax=150,
            bcritsc=24,
            tcritsc=16,
            expected_temp_margin=2.3431632224075836,
            expected_jwdgpro=17475706.393616617,
            expected_jwdgcrt=41107234.360397324,
            expected_vd=9988.2637896807955,
            expected_tmarg=2.3431632224075836,
        ),
        SuperconParam(
            tmargmin_tf=1.5,
            n_tf=16,
            temp_margin=2.3431632224075836,
            jwdgpro=17475706.393616617,
            dhecoil=0.010000000000000002,
            cpttf=74026.751437500003,
            bmaxtfrp=12.48976756562082,
            str_tf_con_res=-0.0050000000000000001,
            b_crit_upper_nbti=14.859999999999999,
            i_str_wp=1,
            str_wp=0.0015619754370069119,
            t_crit_nbti=9.0399999999999991,
            tf_fit_t=0.80807838916035957,
            tf_fit_z=0.3149613642807837,
            tf_fit_y=1.0658869305062604,
            run_tests=0,
            isumat=5,
            iprint=0,
            outfile=11,
            acs=0.001293323051622732,
            aturn=0.0032012300777680192,
            bmax=12.48976756562082,
            fcu=0.80884,
            fhe=0.30000000000000004,
            fhts=0.5,
            iop=74026.751437500003,
            jwp=23124470.793774806,
            tdmptf=25.829000000000001,
            tfes=9561415368.8360519,
            thelium=4.75,
            tmax=150,
            bcritsc=24,
            tcritsc=16,
            expected_temp_margin=2.3431632224075836,
            expected_jwdgpro=17475706.393616617,
            expected_jwdgcrt=41107234.360397324,
            expected_vd=10001.287165953383,
            expected_tmarg=2.3431632224075836,
        ),
        SuperconParam(
            tmargmin_tf=1.5,
            n_tf=16,
            temp_margin=2.3431632224075836,
            jwdgpro=17475706.393616617,
            dhecoil=0.010000000000000002,
            cpttf=74026.751437500003,
            bmaxtfrp=12.48976756562082,
            str_tf_con_res=-0.0050000000000000001,
            b_crit_upper_nbti=14.859999999999999,
            i_str_wp=1,
            str_wp=0.0015619754370069119,
            t_crit_nbti=9.0399999999999991,
            tf_fit_t=0.80807838916035957,
            tf_fit_z=0.3149613642807837,
            tf_fit_y=1.0658869305062604,
            run_tests=0,
            isumat=5,
            iprint=0,
            outfile=11,
            acs=0.001293323051622732,
            aturn=0.0032012300777680192,
            bmax=12.48976756562082,
            fcu=0.80884,
            fhe=0.30000000000000004,
            fhts=0.5,
            iop=74026.751437500003,
            jwp=23124470.793774806,
            tdmptf=25.829000000000001,
            tfes=9561415368.8360519,
            thelium=4.75,
            tmax=150,
            bcritsc=24,
            tcritsc=16,
            expected_temp_margin=2.3431632224075836,
            expected_jwdgpro=17475706.393616617,
            expected_jwdgcrt=41107234.360397324,
            expected_vd=10001.287165953383,
            expected_tmarg=2.3431632224075836,
        ),
    ),
)
def test_supercon(superconparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for supercon.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param superconparam: the data used to mock and assert in this test.
    :type superconparam: superconparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    monkeypatch.setattr(tfcoil_variables, "tmargmin_tf", superconparam.tmargmin_tf)

    monkeypatch.setattr(tfcoil_variables, "n_tf", superconparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "temp_margin", superconparam.temp_margin)

    monkeypatch.setattr(tfcoil_variables, "jwdgpro", superconparam.jwdgpro)

    monkeypatch.setattr(tfcoil_variables, "dhecoil", superconparam.dhecoil)

    monkeypatch.setattr(tfcoil_variables, "cpttf", superconparam.cpttf)

    monkeypatch.setattr(tfcoil_variables, "bmaxtfrp", superconparam.bmaxtfrp)

    monkeypatch.setattr(
        tfcoil_variables, "str_tf_con_res", superconparam.str_tf_con_res
    )

    monkeypatch.setattr(
        tfcoil_variables, "b_crit_upper_nbti", superconparam.b_crit_upper_nbti
    )

    monkeypatch.setattr(tfcoil_variables, "i_str_wp", superconparam.i_str_wp)

    monkeypatch.setattr(tfcoil_variables, "str_wp", superconparam.str_wp)

    monkeypatch.setattr(tfcoil_variables, "t_crit_nbti", superconparam.t_crit_nbti)

    monkeypatch.setattr(sctfcoil_module, "tf_fit_t", superconparam.tf_fit_t)

    monkeypatch.setattr(sctfcoil_module, "tf_fit_z", superconparam.tf_fit_z)

    monkeypatch.setattr(sctfcoil_module, "tf_fit_y", superconparam.tf_fit_y)

    monkeypatch.setattr(global_variables, "run_tests", superconparam.run_tests)

    jwdgcrt, vd, tmarg = sctfcoil.supercon(
        isumat=superconparam.isumat,
        acs=superconparam.acs,
        aturn=superconparam.aturn,
        bmax=superconparam.bmax,
        fcu=superconparam.fcu,
        fhe=superconparam.fhe,
        fhts=superconparam.fhts,
        iop=superconparam.iop,
        jwp=superconparam.jwp,
        tdmptf=superconparam.tdmptf,
        tfes=superconparam.tfes,
        thelium=superconparam.thelium,
        tmax=superconparam.tmax,
        bcritsc=superconparam.bcritsc,
        tcritsc=superconparam.tcritsc,
        output=False,
    )

    assert tfcoil_variables.temp_margin == pytest.approx(
        superconparam.expected_temp_margin
    )

    assert tfcoil_variables.jwdgpro == pytest.approx(superconparam.expected_jwdgpro)

    assert jwdgcrt == pytest.approx(superconparam.expected_jwdgcrt)

    assert vd == pytest.approx(superconparam.expected_vd)

    assert tmarg == pytest.approx(superconparam.expected_tmarg)
