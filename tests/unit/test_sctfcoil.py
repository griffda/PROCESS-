import pytest
import numpy
from typing import NamedTuple, Any

from process.fortran import sctfcoil_module
from process.fortran import tfcoil_variables, pfcoil_variables
from process.fortran import global_variables
from process.fortran import physics_variables
from process.fortran import build_variables
from process.fortran import fwbs_variables
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
def test_protect(protectparam, sctfcoil):
    """
    Automatically generated Regression Unit Test for protect.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param protectparam: the data used to mock and assert in this test.
    :type protectparam: protectparam

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    ajwpro, vd = sctfcoil.protect(
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


class TfCurrentParam(NamedTuple):

    casthi: Any = None

    ritfc: Any = None

    rbmax: Any = None

    i_tf_sup: Any = None

    casths_fraction: Any = None

    tinstf: Any = None

    tftort: Any = None

    bmaxtf: Any = None

    tfinsgap: Any = None

    tfc_sidewall_is_fraction: Any = None

    casths: Any = None

    casthi_is_fraction: Any = None

    casthi_fraction: Any = None

    n_tf: Any = None

    thicndut: Any = None

    thkcas: Any = None

    oacdcp: Any = None

    tfareain: Any = None

    r_tf_inboard_out: Any = None

    r_tf_inboard_in: Any = None

    tfcth: Any = None

    bt: Any = None

    rmajor: Any = None

    tfc_current: Any = None

    theta_coil: Any = None

    expected_ritfc: Any = None

    expected_rbmax: Any = None

    expected_bmaxtf: Any = None

    expected_oacdcp: Any = None

    expected_tfc_current: Any = None


@pytest.mark.parametrize(
    "tfcurrentparam",
    (
        TfCurrentParam(
            casthi=0.060000000000000012,
            ritfc=0,
            rbmax=0,
            i_tf_sup=1,
            casths_fraction=0.059999999999999998,
            tinstf=0.0080000000000000019,
            tftort=1.6395161177915356,
            bmaxtf=0,
            tfinsgap=0.01,
            tfc_sidewall_is_fraction=False,
            casths=0.05000000000000001,
            casthi_is_fraction=False,
            casthi_fraction=0.050000000000000003,
            n_tf=16,
            thicndut=0.002,
            thkcas=0.52465000000000006,
            oacdcp=8673900,
            tfareain=27.308689677971632,
            r_tf_inboard_out=4.20194118510911,
            r_tf_inboard_in=2.9939411851091102,
            tfcth=1.208,
            bt=5.3292000000000002,
            rmajor=8.8901000000000003,
            tfc_current=0,
            theta_coil=0.19634954084936207,
            expected_ritfc=236885604.60000002,
            expected_rbmax=4.0432020634751211,
            expected_bmaxtf=11.717722779177526,
            expected_oacdcp=8674367.2945641987,
            expected_tfc_current=14805350.287500001,
        ),
    ),
)
def test_tf_current(tfcurrentparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_current.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfcurrentparam: the data used to mock and assert in this test.
    :type tfcurrentparam: tfcurrentparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    monkeypatch.setattr(tfcoil_variables, "casthi", tfcurrentparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "ritfc", tfcurrentparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "rbmax", tfcurrentparam.rbmax)

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tfcurrentparam.i_tf_sup)

    monkeypatch.setattr(
        tfcoil_variables, "casths_fraction", tfcurrentparam.casths_fraction
    )

    monkeypatch.setattr(tfcoil_variables, "tinstf", tfcurrentparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "tftort", tfcurrentparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "bmaxtf", tfcurrentparam.bmaxtf)

    monkeypatch.setattr(tfcoil_variables, "tfinsgap", tfcurrentparam.tfinsgap)

    monkeypatch.setattr(
        tfcoil_variables,
        "tfc_sidewall_is_fraction",
        tfcurrentparam.tfc_sidewall_is_fraction,
    )

    monkeypatch.setattr(tfcoil_variables, "casths", tfcurrentparam.casths)

    monkeypatch.setattr(
        tfcoil_variables, "casthi_is_fraction", tfcurrentparam.casthi_is_fraction
    )

    monkeypatch.setattr(
        tfcoil_variables, "casthi_fraction", tfcurrentparam.casthi_fraction
    )

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfcurrentparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "thicndut", tfcurrentparam.thicndut)

    monkeypatch.setattr(tfcoil_variables, "thkcas", tfcurrentparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "oacdcp", tfcurrentparam.oacdcp)

    monkeypatch.setattr(tfcoil_variables, "tfareain", tfcurrentparam.tfareain)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfcurrentparam.r_tf_inboard_out
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfcurrentparam.r_tf_inboard_in
    )

    monkeypatch.setattr(build_variables, "tfcth", tfcurrentparam.tfcth)

    monkeypatch.setattr(physics_variables, "bt", tfcurrentparam.bt)

    monkeypatch.setattr(physics_variables, "rmajor", tfcurrentparam.rmajor)

    monkeypatch.setattr(sctfcoil_module, "tfc_current", tfcurrentparam.tfc_current)

    monkeypatch.setattr(sctfcoil_module, "theta_coil", tfcurrentparam.theta_coil)

    sctfcoil.tf_current()

    assert tfcoil_variables.ritfc == pytest.approx(tfcurrentparam.expected_ritfc)

    assert tfcoil_variables.rbmax == pytest.approx(tfcurrentparam.expected_rbmax)

    assert tfcoil_variables.bmaxtf == pytest.approx(tfcurrentparam.expected_bmaxtf)

    assert tfcoil_variables.oacdcp == pytest.approx(tfcurrentparam.expected_oacdcp)

    assert sctfcoil_module.tfc_current == pytest.approx(
        tfcurrentparam.expected_tfc_current
    )


class TfGlobalGeometryParam(NamedTuple):

    r_tf_outboard_mid: Any = None

    r_cp_top: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    tfthko: Any = None

    tfareain: Any = None

    ritfc: Any = None

    tftort: Any = None

    n_tf: Any = None

    arealeg: Any = None

    i_tf_sup: Any = None

    dztop: Any = None

    i_tf_case_geom: Any = None

    itart: Any = None

    kappa: Any = None

    rminor: Any = None

    h_cp_top: Any = None

    r_tf_outboard_in: Any = None

    r_tf_outboard_out: Any = None

    theta_coil: Any = None

    tan_theta_coil: Any = None

    expected_tfareain: Any = None

    expected_tftort: Any = None

    expected_arealeg: Any = None

    expected_r_tf_outboard_in: Any = None

    expected_r_tf_outboard_out: Any = None

    expected_theta_coil: Any = None

    expected_tan_theta_coil: Any = None


@pytest.mark.parametrize(
    "tfglobalgeometryparam",
    (
        TfGlobalGeometryParam(
            r_tf_outboard_mid=16.519405859443332,
            r_cp_top=4.20194118510911,
            r_tf_inboard_in=2.9939411851091102,
            r_tf_inboard_out=4.20194118510911,
            tfthko=1.208,
            tfareain=0,
            ritfc=0,
            tftort=1,
            n_tf=16,
            arealeg=0,
            i_tf_sup=1,
            dztop=0,
            i_tf_case_geom=0,
            itart=0,
            kappa=1.8480000000000001,
            rminor=2.8677741935483869,
            h_cp_top=0,
            r_tf_outboard_in=0,
            r_tf_outboard_out=0,
            theta_coil=0,
            tan_theta_coil=0,
            expected_tfareain=27.308689677971632,
            expected_tftort=1.6395161177915356,
            expected_arealeg=1.9805354702921749,
            expected_r_tf_outboard_in=15.915405859443332,
            expected_r_tf_outboard_out=17.123405859443331,
            expected_theta_coil=0.19634954084936207,
            expected_tan_theta_coil=0.19891236737965801,
        ),
        TfGlobalGeometryParam(
            r_tf_outboard_mid=17.063351291812893,
            r_cp_top=4.4822055399518357,
            r_tf_inboard_in=2.9538679176819831,
            r_tf_inboard_out=4.4822055399518357,
            tfthko=1.5283376222698528,
            tfareain=35.703669036223495,
            ritfc=241812532.66279837,
            tftort=1.7488698442633552,
            n_tf=16,
            arealeg=2.6728635794409041,
            i_tf_sup=1,
            dztop=0,
            i_tf_case_geom=0,
            itart=0,
            kappa=1.8480000000000001,
            rminor=2.9620024998595755,
            h_cp_top=0,
            r_tf_outboard_in=16.299182480677967,
            r_tf_outboard_out=17.827520102947819,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            expected_tfareain=35.703669036223495,
            expected_tftort=1.7488698442633552,
            expected_arealeg=2.6728635794409041,
            expected_r_tf_outboard_in=16.299182480677967,
            expected_r_tf_outboard_out=17.827520102947819,
            expected_theta_coil=0.19634954084936207,
            expected_tan_theta_coil=0.19891236737965801,
        ),
    ),
)
def test_tf_global_geometry(tfglobalgeometryparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_global_geometry.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfglobalgeometryparam: the data used to mock and assert in this test.
    :type tfglobalgeometryparam: tfglobalgeometryparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    monkeypatch.setattr(
        build_variables, "r_tf_outboard_mid", tfglobalgeometryparam.r_tf_outboard_mid
    )

    monkeypatch.setattr(build_variables, "r_cp_top", tfglobalgeometryparam.r_cp_top)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfglobalgeometryparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfglobalgeometryparam.r_tf_inboard_out
    )

    monkeypatch.setattr(build_variables, "tfthko", tfglobalgeometryparam.tfthko)

    monkeypatch.setattr(tfcoil_variables, "tfareain", tfglobalgeometryparam.tfareain)

    monkeypatch.setattr(tfcoil_variables, "ritfc", tfglobalgeometryparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "tftort", tfglobalgeometryparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfglobalgeometryparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "arealeg", tfglobalgeometryparam.arealeg)

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tfglobalgeometryparam.i_tf_sup)

    monkeypatch.setattr(tfcoil_variables, "dztop", tfglobalgeometryparam.dztop)

    monkeypatch.setattr(
        tfcoil_variables, "i_tf_case_geom", tfglobalgeometryparam.i_tf_case_geom
    )

    monkeypatch.setattr(physics_variables, "itart", tfglobalgeometryparam.itart)

    monkeypatch.setattr(physics_variables, "kappa", tfglobalgeometryparam.kappa)

    monkeypatch.setattr(physics_variables, "rminor", tfglobalgeometryparam.rminor)

    monkeypatch.setattr(sctfcoil_module, "h_cp_top", tfglobalgeometryparam.h_cp_top)

    monkeypatch.setattr(
        sctfcoil_module, "r_tf_outboard_in", tfglobalgeometryparam.r_tf_outboard_in
    )

    monkeypatch.setattr(
        sctfcoil_module, "r_tf_outboard_out", tfglobalgeometryparam.r_tf_outboard_out
    )

    monkeypatch.setattr(sctfcoil_module, "theta_coil", tfglobalgeometryparam.theta_coil)

    monkeypatch.setattr(
        sctfcoil_module, "tan_theta_coil", tfglobalgeometryparam.tan_theta_coil
    )

    sctfcoil.tf_global_geometry()

    assert tfcoil_variables.tfareain == pytest.approx(
        tfglobalgeometryparam.expected_tfareain
    )

    assert tfcoil_variables.tftort == pytest.approx(
        tfglobalgeometryparam.expected_tftort
    )

    assert tfcoil_variables.arealeg == pytest.approx(
        tfglobalgeometryparam.expected_arealeg
    )

    assert sctfcoil_module.r_tf_outboard_in == pytest.approx(
        tfglobalgeometryparam.expected_r_tf_outboard_in
    )

    assert sctfcoil_module.r_tf_outboard_out == pytest.approx(
        tfglobalgeometryparam.expected_r_tf_outboard_out
    )

    assert sctfcoil_module.theta_coil == pytest.approx(
        tfglobalgeometryparam.expected_theta_coil
    )

    assert sctfcoil_module.tan_theta_coil == pytest.approx(
        tfglobalgeometryparam.expected_tan_theta_coil
    )


@pytest.mark.parametrize(
    "a, b, expected_circumference",
    (
        (2.667950e9, 6.782819e8, 11464316399.111176),
        (4.7186039761812131, 3.6192586838709673, 26.308134540723429),
    ),
    ids=["johndcook", "baseline_2018"],
)
def test_circumference(a, b, expected_circumference, sctfcoil):
    """Unit test for the sctfcoil circumference routine.

    This unit test uses values from an external blog referenced in the
    routine header (https://www.johndcook.com/blog/2013/05/05/ramanujan-circumference-ellipse/)
    as well as results obtained from baseline 2018.

    :param a: the value of a (x/a)^2...  in the formula of an ellipse
    :type a: float

    :param b: the value of b ...(y/b)^2...  in the formula of an ellipse
    :type b: float

    :param expected_circumference: the expected result of the routine given inputs a and b
    :type expected_circumference: float

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """
    assert sctfcoil.circumference(a, b) == pytest.approx(expected_circumference)


class ResTfInternalGeomParam(NamedTuple):

    n_tf_turn: Any = None

    thicndut: Any = None

    thkcas: Any = None

    dr_tf_wp: Any = None

    tftort: Any = None

    tfareain: Any = None

    ritfc: Any = None

    fcoolcp: Any = None

    cpttf: Any = None

    cdtfleg: Any = None

    casthi: Any = None

    aiwp: Any = None

    acasetf: Any = None

    tinstf: Any = None

    n_tf: Any = None

    tfthko: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    r_cp_top: Any = None

    itart: Any = None

    expected_n_tf_turn: Any = None

    expected_cpttf: Any = None

    expected_cdtfleg: Any = None

    expected_aiwp: Any = None

    expected_acasetf: Any = None


class TfResHeatingParam(NamedTuple):
    rhocp: Any = None
    tlegav: Any = None
    thicndut: Any = None
    th_joint_contact: Any = None
    rhotfleg: Any = None
    vol_cond_cp: Any = None
    n_tf_turn: Any = None
    thkcas: Any = None
    tftort: Any = None
    tfleng: Any = None
    tflegres: Any = None
    tcpav: Any = None
    arealeg: Any = None
    ritfc: Any = None
    rho_tf_joints: Any = None
    presleg: Any = None
    prescp: Any = None
    pres_joints: Any = None
    n_tf_joints_contact: Any = None
    n_tf_joints: Any = None
    n_tf: Any = None
    i_tf_sup: Any = None
    frholeg: Any = None
    frhocp: Any = None
    fcoolcp: Any = None
    casthi: Any = None
    a_cp_cool: Any = None
    fcoolleg: Any = None
    i_cp_joints: Any = None
    tinstf: Any = None
    tfthko: Any = None
    tfcth: Any = None
    r_cp_top: Any = None
    hmax: Any = None
    r_tf_inboard_in: Any = None
    r_tf_inboard_out: Any = None
    itart: Any = None
    h_cp_top: Any = None
    is_leg_cp_temp_same: Any = None
    expected_rhocp: Any = None
    expected_rhotfleg: Any = None
    expected_vol_cond_cp: Any = None
    expected_tflegres: Any = None
    expected_presleg: Any = None
    expected_prescp: Any = None
    expected_pres_joints: Any = None
    expected_a_cp_cool: Any = None
    expected_is_leg_cp_temp_same: Any = None


@pytest.mark.parametrize(
    "restfinternalgeomparam",
    (
        ResTfInternalGeomParam(
            n_tf_turn=0,
            thicndut=0.00080000000000000004,
            thkcas=0,
            dr_tf_wp=0.15483000000000002,
            tftort=0.45367650933034859,
            tfareain=0.0753112923616783,
            ritfc=25500000,
            fcoolcp=0.12725,
            cpttf=70000,
            cdtfleg=0,
            casthi=0.0077415000000000019,
            aiwp=0,
            acasetf=0,
            tinstf=0,
            n_tf=12,
            tfthko=0.15483000000000002,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.87643571428571443,
            itart=1,
            expected_n_tf_turn=1,
            expected_cpttf=2125000,
            expected_cdtfleg=421788350.27812088,
            expected_aiwp=0.00030678028680367151,
            expected_acasetf=0.00061190425043863676,
        ),
        ResTfInternalGeomParam(
            n_tf_turn=1,
            thicndut=0.00080000000000000004,
            thkcas=0,
            dr_tf_wp=0.14708850000000001,
            tftort=0.44435902370665786,
            tfareain=0.0753112923616783,
            ritfc=25500000,
            fcoolcp=0.12725,
            cpttf=2125000,
            cdtfleg=421788350.27812088,
            casthi=0.0077415000000000019,
            aiwp=0.00030678028680367151,
            acasetf=0.00061190425043863676,
            tinstf=0,
            n_tf=12,
            tfthko=0.15483000000000002,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.85843571428571441,
            itart=1,
            expected_n_tf_turn=1,
            expected_cpttf=2125000,
            expected_cdtfleg=430664525.98439038,
            expected_aiwp=0.00029439388680367086,
            expected_acasetf=0.00061190425043863676,
        ),
    ),
)
def test_res_tf_internal_geom(restfinternalgeomparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for res_tf_internal_geom.

    This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

    :param restfinternalgeomparam: the data used to mock and assert in this test.
    :type restfinternalgeomparam: restfinternalgeomparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch

    :param sctfcoil: initialised Sctfcoil object
    :type sctfcoil: process.sctfcoil.Sctfcoil
    """

    monkeypatch.setattr(tfcoil_variables, "n_tf_turn", restfinternalgeomparam.n_tf_turn)

    monkeypatch.setattr(tfcoil_variables, "thicndut", restfinternalgeomparam.thicndut)

    monkeypatch.setattr(tfcoil_variables, "thkcas", restfinternalgeomparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", restfinternalgeomparam.dr_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "tftort", restfinternalgeomparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "tfareain", restfinternalgeomparam.tfareain)

    monkeypatch.setattr(tfcoil_variables, "ritfc", restfinternalgeomparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "fcoolcp", restfinternalgeomparam.fcoolcp)

    monkeypatch.setattr(tfcoil_variables, "cpttf", restfinternalgeomparam.cpttf)

    monkeypatch.setattr(tfcoil_variables, "cdtfleg", restfinternalgeomparam.cdtfleg)

    monkeypatch.setattr(tfcoil_variables, "casthi", restfinternalgeomparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "aiwp", restfinternalgeomparam.aiwp)

    monkeypatch.setattr(tfcoil_variables, "acasetf", restfinternalgeomparam.acasetf)

    monkeypatch.setattr(tfcoil_variables, "tinstf", restfinternalgeomparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "n_tf", restfinternalgeomparam.n_tf)

    monkeypatch.setattr(build_variables, "tfthko", restfinternalgeomparam.tfthko)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", restfinternalgeomparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", restfinternalgeomparam.r_tf_inboard_out
    )

    monkeypatch.setattr(build_variables, "r_cp_top", restfinternalgeomparam.r_cp_top)

    monkeypatch.setattr(physics_variables, "itart", restfinternalgeomparam.itart)

    sctfcoil.res_tf_internal_geom()

    assert tfcoil_variables.n_tf_turn == pytest.approx(
        restfinternalgeomparam.expected_n_tf_turn
    )

    assert tfcoil_variables.cpttf == pytest.approx(
        restfinternalgeomparam.expected_cpttf
    )

    assert tfcoil_variables.cdtfleg == pytest.approx(
        restfinternalgeomparam.expected_cdtfleg
    )

    assert tfcoil_variables.aiwp == pytest.approx(restfinternalgeomparam.expected_aiwp)

    assert tfcoil_variables.acasetf == pytest.approx(
        restfinternalgeomparam.expected_acasetf
    )

    assert tfcoil_variables.n_tf_turn == pytest.approx(
        restfinternalgeomparam.expected_n_tf_turn
    )

    assert tfcoil_variables.cpttf == pytest.approx(
        restfinternalgeomparam.expected_cpttf
    )

    assert tfcoil_variables.cdtfleg == pytest.approx(
        restfinternalgeomparam.expected_cdtfleg
    )

    assert tfcoil_variables.aiwp == pytest.approx(restfinternalgeomparam.expected_aiwp)

    assert tfcoil_variables.acasetf == pytest.approx(
        restfinternalgeomparam.expected_acasetf
    )


@pytest.mark.parametrize(
    "tfresheatingparam",
    (
        TfResHeatingParam(
            rhocp=0,
            tlegav=-1,
            thicndut=0.00080000000000000004,
            th_joint_contact=0.029999999999999999,
            rhotfleg=0,
            vol_cond_cp=0,
            n_tf_turn=1,
            thkcas=0,
            tftort=0.45367650933034859,
            tfleng=15.582502857142856,
            tflegres=0,
            tcpav=347.13,
            arealeg=0.070242733939617885,
            ritfc=25500000,
            rho_tf_joints=2.5000000000000002e-10,
            presleg=0,
            prescp=0,
            pres_joints=0,
            n_tf_joints_contact=6,
            n_tf_joints=4,
            n_tf=12,
            i_tf_sup=0,
            frholeg=1,
            frhocp=1,
            fcoolcp=0.12725,
            casthi=0.0077415000000000019,
            a_cp_cool=0,
            fcoolleg=0.20000000000000001,
            i_cp_joints=1,
            tinstf=0,
            tfthko=0.15483000000000002,
            tfcth=0.15483000000000002,
            r_cp_top=0.87643571428571443,
            hmax=4.4214285714285717,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            itart=1,
            h_cp_top=2.6714285714285717,
            is_leg_cp_temp_same=0,
            expected_rhocp=2.1831760869565221e-08,
            expected_rhotfleg=2.1831760869565221e-08,
            expected_vol_cond_cp=12.020160732580297,
            expected_tflegres=6.1387543007600344e-06,
            expected_presleg=332643748.67243439,
            expected_prescp=470083798.99090022,
            expected_pres_joints=1944336.7995005273,
            expected_a_cp_cool=0.00068328705812121333,
            expected_is_leg_cp_temp_same=1,
        ),
        TfResHeatingParam(
            rhocp=2.1831760869565221e-08,
            tlegav=-1,
            thicndut=0.00080000000000000004,
            th_joint_contact=0.029999999999999999,
            rhotfleg=2.1831760869565221e-08,
            vol_cond_cp=12.020160732580297,
            n_tf_turn=1,
            thkcas=0,
            tftort=0.44435902370665786,
            tfleng=15.654502857142857,
            tflegres=6.1387543007600344e-06,
            tcpav=347.13,
            arealeg=0.068800107640501845,
            ritfc=25500000,
            rho_tf_joints=2.5000000000000002e-10,
            presleg=332643748.67243439,
            prescp=470083798.99090022,
            pres_joints=1944336.7995005273,
            n_tf_joints_contact=6,
            n_tf_joints=4,
            n_tf=12,
            i_tf_sup=0,
            frholeg=1,
            frhocp=1,
            fcoolcp=0.12725,
            casthi=0.0077415000000000019,
            a_cp_cool=0.00068328705812121333,
            fcoolleg=0.20000000000000001,
            i_cp_joints=1,
            tinstf=0,
            tfthko=0.15483000000000002,
            tfcth=0.15483000000000002,
            r_cp_top=0.85843571428571441,
            hmax=4.4214285714285717,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            itart=1,
            h_cp_top=2.6714285714285717,
            is_leg_cp_temp_same=1,
            expected_rhocp=2.1831760869565221e-08,
            expected_rhotfleg=2.1831760869565221e-08,
            expected_vol_cond_cp=11.545770024935592,
            expected_tflegres=6.2969005770928158e-06,
            expected_presleg=341213300.02121693,
            expected_prescp=475710489.56122422,
            expected_pres_joints=1944336.7995005273,
            expected_a_cp_cool=0.00068328705812121333,
            expected_is_leg_cp_temp_same=1,
        ),
    ),
)
def test_tf_res_heating(tfresheatingparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_res_heating.

    This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

    :param tfresheatingparam: the data used to mock and assert in this test.
    :type tfresheatingparam: tfresheatingparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "rhocp", tfresheatingparam.rhocp)

    monkeypatch.setattr(tfcoil_variables, "tlegav", tfresheatingparam.tlegav)

    monkeypatch.setattr(tfcoil_variables, "thicndut", tfresheatingparam.thicndut)

    monkeypatch.setattr(
        tfcoil_variables, "th_joint_contact", tfresheatingparam.th_joint_contact
    )

    monkeypatch.setattr(tfcoil_variables, "rhotfleg", tfresheatingparam.rhotfleg)

    monkeypatch.setattr(tfcoil_variables, "vol_cond_cp", tfresheatingparam.vol_cond_cp)

    monkeypatch.setattr(tfcoil_variables, "n_tf_turn", tfresheatingparam.n_tf_turn)

    monkeypatch.setattr(tfcoil_variables, "thkcas", tfresheatingparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "tftort", tfresheatingparam.tftort)

    monkeypatch.setattr(tfcoil_variables, "tfleng", tfresheatingparam.tfleng)

    monkeypatch.setattr(tfcoil_variables, "tflegres", tfresheatingparam.tflegres)

    monkeypatch.setattr(tfcoil_variables, "tcpav", tfresheatingparam.tcpav)

    monkeypatch.setattr(tfcoil_variables, "arealeg", tfresheatingparam.arealeg)

    monkeypatch.setattr(tfcoil_variables, "ritfc", tfresheatingparam.ritfc)

    monkeypatch.setattr(
        tfcoil_variables, "rho_tf_joints", tfresheatingparam.rho_tf_joints
    )

    monkeypatch.setattr(tfcoil_variables, "presleg", tfresheatingparam.presleg)

    monkeypatch.setattr(tfcoil_variables, "prescp", tfresheatingparam.prescp)

    monkeypatch.setattr(tfcoil_variables, "pres_joints", tfresheatingparam.pres_joints)

    monkeypatch.setattr(
        tfcoil_variables, "n_tf_joints_contact", tfresheatingparam.n_tf_joints_contact
    )

    monkeypatch.setattr(tfcoil_variables, "n_tf_joints", tfresheatingparam.n_tf_joints)

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfresheatingparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tfresheatingparam.i_tf_sup)

    monkeypatch.setattr(tfcoil_variables, "frholeg", tfresheatingparam.frholeg)

    monkeypatch.setattr(tfcoil_variables, "frhocp", tfresheatingparam.frhocp)

    monkeypatch.setattr(tfcoil_variables, "fcoolcp", tfresheatingparam.fcoolcp)

    monkeypatch.setattr(tfcoil_variables, "casthi", tfresheatingparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "a_cp_cool", tfresheatingparam.a_cp_cool)

    monkeypatch.setattr(tfcoil_variables, "fcoolleg", tfresheatingparam.fcoolleg)

    monkeypatch.setattr(tfcoil_variables, "i_cp_joints", tfresheatingparam.i_cp_joints)

    monkeypatch.setattr(tfcoil_variables, "tinstf", tfresheatingparam.tinstf)

    monkeypatch.setattr(build_variables, "tfthko", tfresheatingparam.tfthko)

    monkeypatch.setattr(build_variables, "tfcth", tfresheatingparam.tfcth)

    monkeypatch.setattr(build_variables, "r_cp_top", tfresheatingparam.r_cp_top)

    monkeypatch.setattr(build_variables, "hmax", tfresheatingparam.hmax)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfresheatingparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfresheatingparam.r_tf_inboard_out
    )

    monkeypatch.setattr(physics_variables, "itart", tfresheatingparam.itart)

    monkeypatch.setattr(sctfcoil_module, "h_cp_top", tfresheatingparam.h_cp_top)

    monkeypatch.setattr(
        sctfcoil_module, "is_leg_cp_temp_same", tfresheatingparam.is_leg_cp_temp_same
    )

    sctfcoil.tf_res_heating()

    assert tfcoil_variables.rhocp == pytest.approx(tfresheatingparam.expected_rhocp)

    assert tfcoil_variables.rhotfleg == pytest.approx(
        tfresheatingparam.expected_rhotfleg
    )

    assert tfcoil_variables.vol_cond_cp == pytest.approx(
        tfresheatingparam.expected_vol_cond_cp
    )

    assert tfcoil_variables.tflegres == pytest.approx(
        tfresheatingparam.expected_tflegres
    )

    assert tfcoil_variables.presleg == pytest.approx(tfresheatingparam.expected_presleg)

    assert tfcoil_variables.prescp == pytest.approx(tfresheatingparam.expected_prescp)

    assert tfcoil_variables.pres_joints == pytest.approx(
        tfresheatingparam.expected_pres_joints
    )

    assert tfcoil_variables.a_cp_cool == pytest.approx(
        tfresheatingparam.expected_a_cp_cool
    )

    assert sctfcoil_module.is_leg_cp_temp_same == pytest.approx(
        tfresheatingparam.expected_is_leg_cp_temp_same
    )


class CpostParam(NamedTuple):

    n_tf: Any = None

    hmax: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    r_cp_top: Any = None

    ztop: Any = None

    hmaxi: Any = None

    cas_in_th: Any = None

    cas_out_th: Any = None

    gr_ins_th: Any = None

    ins_th: Any = None

    n_tf_turn: Any = None

    curr: Any = None

    rho: Any = None

    fcool: Any = None

    expected_vol_ins_cp: Any = None

    expected_vol_gr_ins_cp: Any = None

    expected_vol_case_cp: Any = None

    expected_respow: Any = None

    expected_vol_cond_cp: Any = None

    expected_a_cp_cool: Any = None


@pytest.mark.parametrize(
    "cpostparam",
    (
        CpostParam(
            n_tf=12,
            hmax=4.4214285714285717,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.87643571428571443,
            ztop=2.6714285714285717,
            hmaxi=4.5762585714285713,
            cas_in_th=0,
            cas_out_th=0.0077415000000000019,
            gr_ins_th=0,
            ins_th=0.00080000000000000004,
            n_tf_turn=1,
            curr=25500000,
            rho=2.1831760869565221e-08,
            fcool=0.12725,
            expected_vol_ins_cp=0.12917075053120922,
            expected_vol_gr_ins_cp=0,
            expected_vol_case_cp=0.12791418544773489,
            expected_respow=470083798.99090022,
            expected_vol_cond_cp=12.020160732580297,
            expected_a_cp_cool=0.00068328705812121333,
        ),
        CpostParam(
            n_tf=12,
            hmax=4.4214285714285717,
            r_tf_inboard_in=0,
            r_tf_inboard_out=0.15483000000000002,
            r_cp_top=0.85843571428571441,
            ztop=2.6714285714285717,
            hmaxi=4.5762585714285713,
            cas_in_th=0,
            cas_out_th=0.0077415000000000019,
            gr_ins_th=0,
            ins_th=0.00080000000000000004,
            n_tf_turn=1,
            curr=25500000,
            rho=2.1831760869565221e-08,
            fcool=0.12725,
            expected_vol_ins_cp=0.12679799009998483,
            expected_vol_gr_ins_cp=0,
            expected_vol_case_cp=0.12648575512245444,
            expected_respow=475710489.56122422,
            expected_vol_cond_cp=11.545770024935592,
            expected_a_cp_cool=0.00068328705812121333,
        ),
    ),
)
def test_cpost(cpostparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for cpost.

    This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

    :param cpostparam: the data used to mock and assert in this test.
    :type cpostparam: cpostparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "n_tf", cpostparam.n_tf)

    monkeypatch.setattr(build_variables, "hmax", cpostparam.hmax)

    (
        a_cp_cool,
        vol_cond_cp,
        respow,
        vol_ins_cp,
        vol_case_cp,
        vol_gr_ins_cp,
    ) = sctfcoil.cpost(
        r_tf_inboard_in=cpostparam.r_tf_inboard_in,
        r_tf_inboard_out=cpostparam.r_tf_inboard_out,
        r_cp_top=cpostparam.r_cp_top,
        ztop=cpostparam.ztop,
        hmaxi=cpostparam.hmaxi,
        cas_in_th=cpostparam.cas_in_th,
        cas_out_th=cpostparam.cas_out_th,
        gr_ins_th=cpostparam.gr_ins_th,
        ins_th=cpostparam.ins_th,
        n_tf_turn=cpostparam.n_tf_turn,
        curr=cpostparam.curr,
        rho=cpostparam.rho,
        fcool=cpostparam.fcool,
    )

    assert vol_ins_cp == pytest.approx(cpostparam.expected_vol_ins_cp)

    assert vol_gr_ins_cp == pytest.approx(cpostparam.expected_vol_gr_ins_cp)

    assert vol_case_cp == pytest.approx(cpostparam.expected_vol_case_cp)

    assert respow == pytest.approx(cpostparam.expected_respow)

    assert vol_cond_cp == pytest.approx(cpostparam.expected_vol_cond_cp)

    assert a_cp_cool == pytest.approx(cpostparam.expected_a_cp_cool)


class TfFieldAndForceParam(NamedTuple):

    rminor: Any = None

    rmajor: Any = None

    bt: Any = None

    itart: Any = None

    r_tf_outboard_mid: Any = None

    r_vv_inboard_out: Any = None

    r_tf_inboard_mid: Any = None

    r_cp_top: Any = None

    vforce: Any = None

    n_tf: Any = None

    taucq: Any = None

    sigvvall: Any = None

    cforce: Any = None

    ritfc: Any = None

    bmaxtf: Any = None

    i_tf_sup: Any = None

    f_vforce_inboard: Any = None

    vforce_outboard: Any = None

    tinstf: Any = None

    thicndut: Any = None

    dr_tf_wp: Any = None

    tfinsgap: Any = None

    i_cp_joints: Any = None

    casthi: Any = None

    r_tf_outboard_in: Any = None

    r_wp_inner: Any = None

    r_wp_outer: Any = None

    vforce_inboard_tot: Any = None

    expected_vforce: Any = None

    expected_cforce: Any = None

    expected_f_vforce_inboard: Any = None

    expected_vforce_outboard: Any = None

    expected_vforce_inboard_tot: Any = None


@pytest.mark.parametrize(
    "tffieldandforceparam",
    (
        TfFieldAndForceParam(
            rminor=0.97142857142857153,
            rmajor=1.7000000000000002,
            bt=3,
            itart=1,
            r_tf_outboard_mid=4.1688435714285719,
            r_vv_inboard_out=0.20483000000000001,
            r_tf_inboard_mid=0.077415000000000012,
            r_cp_top=0.87643571428571443,
            vforce=0,
            n_tf=12,
            taucq=30,
            sigvvall=93000000,
            cforce=0,
            ritfc=25500000,
            bmaxtf=34.862617362267024,
            i_tf_sup=0,
            f_vforce_inboard=0.5,
            vforce_outboard=0,
            tinstf=0,
            thicndut=0.00080000000000000004,
            dr_tf_wp=0.15483000000000002,
            tfinsgap=0.01,
            i_cp_joints=1,
            casthi=0.0077415000000000019,
            r_tf_outboard_in=4.0914285714285716,
            r_wp_inner=0,
            r_wp_outer=0.14708850000000001,
            vforce_inboard_tot=0,
            expected_vforce=12380916.66459452,
            expected_cforce=37041530.947408713,
            expected_f_vforce_inboard=0.59539634897566385,
            expected_vforce_outboard=8413494.7991220243,
            expected_vforce_inboard_tot=148570999.97513425,
        ),
        TfFieldAndForceParam(
            rminor=0.97142857142857153,
            rmajor=1.7000000000000002,
            bt=3,
            itart=1,
            r_tf_outboard_mid=4.1868435714285717,
            r_vv_inboard_out=0.20483000000000001,
            r_tf_inboard_mid=0.077415000000000012,
            r_cp_top=0.85843571428571441,
            vforce=12380916.66459452,
            n_tf=12,
            taucq=30,
            sigvvall=93000000,
            cforce=37041530.947408713,
            ritfc=25500000,
            bmaxtf=34.862617362267024,
            i_tf_sup=0,
            f_vforce_inboard=0.59539634897566385,
            vforce_outboard=8413494.7991220243,
            tinstf=0,
            thicndut=0.00080000000000000004,
            dr_tf_wp=0.14708850000000001,
            tfinsgap=0.01,
            i_cp_joints=1,
            casthi=0.0077415000000000019,
            r_tf_outboard_in=4.1094285714285714,
            r_wp_inner=0,
            r_wp_outer=0.14708850000000001,
            vforce_inboard_tot=148570999.97513425,
            expected_vforce=12268469.138442248,
            expected_cforce=37041530.947408713,
            expected_f_vforce_inboard=0.58932254522566518,
            expected_vforce_outboard=8549450.0771621168,
            expected_vforce_inboard_tot=147221629.66130698,
        ),
    ),
)
def test_tf_field_and_force(tffieldandforceparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_field_and_force.

    This test was generated using data from tests/regression/scenarios/FNSF/IN.DAT.

    :param tffieldandforceparam: the data used to mock and assert in this test.
    :type tffieldandforceparam: tffieldandforceparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(physics_variables, "rminor", tffieldandforceparam.rminor)

    monkeypatch.setattr(physics_variables, "rmajor", tffieldandforceparam.rmajor)

    monkeypatch.setattr(physics_variables, "bt", tffieldandforceparam.bt)

    monkeypatch.setattr(physics_variables, "itart", tffieldandforceparam.itart)

    monkeypatch.setattr(
        build_variables, "r_tf_outboard_mid", tffieldandforceparam.r_tf_outboard_mid
    )

    monkeypatch.setattr(
        build_variables, "r_vv_inboard_out", tffieldandforceparam.r_vv_inboard_out
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_mid", tffieldandforceparam.r_tf_inboard_mid
    )

    monkeypatch.setattr(build_variables, "r_cp_top", tffieldandforceparam.r_cp_top)

    monkeypatch.setattr(tfcoil_variables, "vforce", tffieldandforceparam.vforce)

    monkeypatch.setattr(tfcoil_variables, "n_tf", tffieldandforceparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "taucq", tffieldandforceparam.taucq)

    monkeypatch.setattr(tfcoil_variables, "sigvvall", tffieldandforceparam.sigvvall)

    monkeypatch.setattr(tfcoil_variables, "cforce", tffieldandforceparam.cforce)

    monkeypatch.setattr(tfcoil_variables, "ritfc", tffieldandforceparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "bmaxtf", tffieldandforceparam.bmaxtf)

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tffieldandforceparam.i_tf_sup)

    monkeypatch.setattr(
        tfcoil_variables, "f_vforce_inboard", tffieldandforceparam.f_vforce_inboard
    )

    monkeypatch.setattr(
        tfcoil_variables, "vforce_outboard", tffieldandforceparam.vforce_outboard
    )

    monkeypatch.setattr(tfcoil_variables, "tinstf", tffieldandforceparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "thicndut", tffieldandforceparam.thicndut)

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", tffieldandforceparam.dr_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "tfinsgap", tffieldandforceparam.tfinsgap)

    monkeypatch.setattr(
        tfcoil_variables, "i_cp_joints", tffieldandforceparam.i_cp_joints
    )

    monkeypatch.setattr(tfcoil_variables, "casthi", tffieldandforceparam.casthi)

    monkeypatch.setattr(
        sctfcoil_module, "r_tf_outboard_in", tffieldandforceparam.r_tf_outboard_in
    )

    monkeypatch.setattr(sctfcoil_module, "r_wp_inner", tffieldandforceparam.r_wp_inner)

    monkeypatch.setattr(sctfcoil_module, "r_wp_outer", tffieldandforceparam.r_wp_outer)

    monkeypatch.setattr(
        sctfcoil_module, "vforce_inboard_tot", tffieldandforceparam.vforce_inboard_tot
    )

    sctfcoil.tf_field_and_force()

    assert tfcoil_variables.vforce == pytest.approx(
        tffieldandforceparam.expected_vforce
    )

    assert tfcoil_variables.cforce == pytest.approx(
        tffieldandforceparam.expected_cforce
    )

    assert tfcoil_variables.f_vforce_inboard == pytest.approx(
        tffieldandforceparam.expected_f_vforce_inboard
    )

    assert tfcoil_variables.vforce_outboard == pytest.approx(
        tffieldandforceparam.expected_vforce_outboard
    )

    assert sctfcoil_module.vforce_inboard_tot == pytest.approx(
        tffieldandforceparam.expected_vforce_inboard_tot
    )


class TfcindParam(NamedTuple):

    yarc: Any = None

    xarc: Any = None

    tfind: Any = None

    tfthk: Any = None

    expected_yarc: Any = None

    expected_tfind: Any = None


@pytest.mark.parametrize(
    "tfcindparam",
    (
        TfcindParam(
            yarc=numpy.array(
                (
                    4.5228880258064512,
                    7.5381467096774184,
                    0,
                    -9.0730900215620327,
                    -5.4438540129372193,
                ),
                order="F",
            ),
            xarc=numpy.array(
                (
                    4.20194118510911,
                    8.316545161290323,
                    15.915405859443332,
                    8.316545161290323,
                    4.20194118510911,
                ),
                order="F",
            ),
            tfind=0,
            tfthk=1.208,
            expected_tfind=5.4453892599192845e-06,
        ),
        TfcindParam(
            yarc=numpy.array(
                (
                    4.5336880258064509,
                    7.5561467096774191,
                    0,
                    -9.0730900215620327,
                    -5.4438540129372193,
                ),
                order="F",
            ),
            xarc=numpy.array(
                (
                    4.20194118510911,
                    8.316545161290323,
                    15.915405859443332,
                    8.316545161290323,
                    4.20194118510911,
                ),
                order="F",
            ),
            tfind=5.4524893280368181e-06,
            tfthk=1.208,
            expected_tfind=5.4524893280368181e-06,
        ),
    ),
)
def test_tfcind(tfcindparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tfcind.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfcindparam: the data used to mock and assert in this test.
    :type tfcindparam: tfcindparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "yarc", tfcindparam.yarc)

    monkeypatch.setattr(tfcoil_variables, "xarc", tfcindparam.xarc)

    monkeypatch.setattr(tfcoil_variables, "tfind", tfcindparam.tfind)

    sctfcoil.tfcind(tfthk=tfcindparam.tfthk)

    assert tfcoil_variables.tfind == pytest.approx(tfcindparam.expected_tfind)


class TfCoilAreaAndMassesParam(NamedTuple):

    hr1: Any = None

    r_tf_outboard_mid: Any = None

    tfcth: Any = None

    r_tf_inboard_mid: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    hmax: Any = None

    denstl: Any = None

    whtconsh: Any = None

    whttf: Any = None

    whtcas: Any = None

    tficrn: Any = None

    tfcryoarea: Any = None

    tfsao: Any = None

    whtgw: Any = None

    tfocrn: Any = None

    whtconsc: Any = None

    whtconcu: Any = None

    whtcon: Any = None

    whtconin: Any = None

    tfsai: Any = None

    vftf: Any = None

    dcond: Any = None

    dcondins: Any = None

    tfleng: Any = None

    dcase: Any = None

    acndttf: Any = None

    n_tf_turn: Any = None

    n_tf: Any = None

    aiwp: Any = None

    acasetfo: Any = None

    acasetf: Any = None

    fcutfsu: Any = None

    awphec: Any = None

    acstf: Any = None

    whttflgs: Any = None

    whtcp: Any = None

    whtconal: Any = None

    vol_cond_cp: Any = None

    i_tf_sup: Any = None

    i_tf_sc_mat: Any = None

    arealeg: Any = None

    thkcas: Any = None

    voltfleg: Any = None

    cplen: Any = None

    itart: Any = None

    awpc: Any = None

    awptf: Any = None

    vol_ins_cp: Any = None

    vol_gr_ins_cp: Any = None

    vol_case_cp: Any = None

    a_leg_ins: Any = None

    a_leg_gr_ins: Any = None

    a_leg_cond: Any = None

    theta_coil: Any = None

    tan_theta_coil: Any = None

    expected_whtconsh: Any = None

    expected_whtcas: Any = None

    expected_tficrn: Any = None

    expected_tfcryoarea: Any = None

    expected_tfsao: Any = None

    expected_whtgw: Any = None

    expected_tfocrn: Any = None

    expected_whtconsc: Any = None

    expected_whtconcu: Any = None

    expected_whtcon: Any = None

    expected_whtconin: Any = None

    expected_cplen: Any = None


@pytest.mark.parametrize(
    "tfcoilareaandmassesparam",
    (
        TfCoilAreaAndMassesParam(
            hr1=0,
            r_tf_outboard_mid=16.519405859443332,
            tfcth=1.208,
            r_tf_inboard_mid=3.5979411851091103,
            r_tf_inboard_in=2.9939411851091102,
            r_tf_inboard_out=4.20194118510911,
            hmax=9.0730900215620327,
            denstl=7800,
            whtconsh=0,
            whttf=0,
            whtcas=0,
            tficrn=0,
            tfcryoarea=0,
            tfsao=0,
            whtgw=0,
            tfocrn=0,
            whtconsc=0,
            whtconcu=0,
            whtcon=0,
            whtconin=0,
            tfsai=0,
            vftf=0.30000000000000004,
            dcond=numpy.array(
                numpy.array(
                    (6080, 6080, 6070, 6080, 6080, 8500, 6070, 8500, 8500), order="F"
                ),
                order="F",
            ).transpose(),
            dcondins=1800,
            tfleng=50.483843027201402,
            dcase=8000,
            acndttf=0.0014685061538103825,
            n_tf_turn=200,
            n_tf=16,
            aiwp=0.087880174466980876,
            acasetfo=1.2752592893394648,
            acasetf=1.0015169239205168,
            fcutfsu=0.80884,
            awphec=0.015707963267948974,
            acstf=0.001293323051622732,
            whttflgs=0,
            whtcp=0,
            whtconal=0,
            vol_cond_cp=0,
            i_tf_sup=1,
            i_tf_sc_mat=5,
            arealeg=1.9805354702921749,
            thkcas=0.52465000000000006,
            voltfleg=0,
            cplen=0,
            itart=0,
            awpc=0.70527618095271016,
            awptf=0.64024601555360383,
            vol_ins_cp=0,
            vol_gr_ins_cp=0,
            vol_case_cp=0,
            a_leg_ins=0,
            a_leg_gr_ins=0,
            a_leg_cond=0,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            expected_whtconsh=115651.90127937049,
            expected_whtcas=1034021.9996272125,
            expected_tficrn=0.8197580588957678,
            expected_tfcryoarea=6381.2092203414386,
            expected_tfsao=1324.3051892984724,
            expected_whtgw=5909.3507916745702,
            expected_tfocrn=0.59553192892551199,
            expected_whtconsc=5802.5700395134345,
            expected_whtconcu=58744.465423173802,
            expected_whtcon=188184.68882144717,
            expected_whtconin=7985.7520793894437,
            expected_cplen=20.562180043124066,
        ),
        TfCoilAreaAndMassesParam(
            hr1=0,
            r_tf_outboard_mid=16.519405859443332,
            tfcth=1.208,
            r_tf_inboard_mid=3.5979411851091103,
            r_tf_inboard_in=2.9939411851091102,
            r_tf_inboard_out=4.20194118510911,
            hmax=9.0730900215620327,
            denstl=7800,
            whtconsh=115651.90127937049,
            whttf=19649856.627845347,
            whtcas=1034021.9996272125,
            tficrn=0.8197580588957678,
            tfcryoarea=6381.2092203414386,
            tfsao=1324.3051892984724,
            whtgw=5909.3507916745702,
            tfocrn=0.59553192892551199,
            whtconsc=5802.5700395134345,
            whtconcu=58744.465423173802,
            whtcon=0,
            whtconin=0,
            tfsai=0,
            vftf=0.30000000000000004,
            dcond=numpy.array(
                numpy.array(
                    (6080, 6080, 6070, 6080, 6080, 8500, 6070, 8500, 8500), order="F"
                ),
                order="F",
            ).transpose(),
            dcondins=1800,
            tfleng=50.514015976170839,
            dcase=8000,
            acndttf=0.0014685061538103825,
            n_tf_turn=200,
            n_tf=16,
            aiwp=0.087880174466980876,
            acasetfo=1.2752592893394648,
            acasetf=1.0015169239205168,
            fcutfsu=0.80884,
            awphec=0.015707963267948974,
            acstf=0.001293323051622732,
            whttflgs=0,
            whtcp=0,
            whtconal=0,
            vol_cond_cp=0,
            i_tf_sup=1,
            i_tf_sc_mat=5,
            arealeg=1.9805354702921749,
            thkcas=0.52465000000000006,
            voltfleg=0,
            cplen=20.562180043124066,
            itart=0,
            awpc=0.70527618095271016,
            awptf=0.64024601555360383,
            vol_ins_cp=0,
            vol_gr_ins_cp=0,
            vol_case_cp=0,
            a_leg_ins=0,
            a_leg_gr_ins=0,
            a_leg_cond=0,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            expected_whtconsh=115721.02357090525,
            expected_whtcas=1034699.2182961091,
            expected_tficrn=0.8197580588957678,
            expected_tfcryoarea=6385.0231118485681,
            expected_tfsao=1325.0966938769795,
            expected_whtgw=5912.8826650262808,
            expected_tfocrn=0.59553192892551199,
            expected_whtconsc=5806.038092640837,
            expected_whtconcu=58779.575542593491,
            expected_whtcon=188297.16217276,
            expected_whtconin=7990.5249666247555,
            expected_cplen=20.562180043124066,
        ),
    ),
)
def test_tf_coil_area_and_masses(tfcoilareaandmassesparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_coil_area_and_masses.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfcoilareaandmassesparam: the data used to mock and assert in this test.
    :type tfcoilareaandmassesparam: tfcoilareaandmassesparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(build_variables, "hr1", tfcoilareaandmassesparam.hr1)

    monkeypatch.setattr(
        build_variables, "r_tf_outboard_mid", tfcoilareaandmassesparam.r_tf_outboard_mid
    )

    monkeypatch.setattr(build_variables, "tfcth", tfcoilareaandmassesparam.tfcth)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_mid", tfcoilareaandmassesparam.r_tf_inboard_mid
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfcoilareaandmassesparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfcoilareaandmassesparam.r_tf_inboard_out
    )

    monkeypatch.setattr(build_variables, "hmax", tfcoilareaandmassesparam.hmax)

    monkeypatch.setattr(fwbs_variables, "denstl", tfcoilareaandmassesparam.denstl)

    monkeypatch.setattr(tfcoil_variables, "whtconsh", tfcoilareaandmassesparam.whtconsh)

    monkeypatch.setattr(tfcoil_variables, "whttf", tfcoilareaandmassesparam.whttf)

    monkeypatch.setattr(tfcoil_variables, "whtcas", tfcoilareaandmassesparam.whtcas)

    monkeypatch.setattr(tfcoil_variables, "tficrn", tfcoilareaandmassesparam.tficrn)

    monkeypatch.setattr(
        tfcoil_variables, "tfcryoarea", tfcoilareaandmassesparam.tfcryoarea
    )

    monkeypatch.setattr(tfcoil_variables, "tfsao", tfcoilareaandmassesparam.tfsao)

    monkeypatch.setattr(tfcoil_variables, "whtgw", tfcoilareaandmassesparam.whtgw)

    monkeypatch.setattr(tfcoil_variables, "tfocrn", tfcoilareaandmassesparam.tfocrn)

    monkeypatch.setattr(tfcoil_variables, "whtconsc", tfcoilareaandmassesparam.whtconsc)

    monkeypatch.setattr(tfcoil_variables, "whtconcu", tfcoilareaandmassesparam.whtconcu)

    monkeypatch.setattr(tfcoil_variables, "whtcon", tfcoilareaandmassesparam.whtcon)

    monkeypatch.setattr(tfcoil_variables, "whtconin", tfcoilareaandmassesparam.whtconin)

    monkeypatch.setattr(tfcoil_variables, "tfsai", tfcoilareaandmassesparam.tfsai)

    monkeypatch.setattr(tfcoil_variables, "vftf", tfcoilareaandmassesparam.vftf)

    monkeypatch.setattr(tfcoil_variables, "dcond", tfcoilareaandmassesparam.dcond)

    monkeypatch.setattr(tfcoil_variables, "dcondins", tfcoilareaandmassesparam.dcondins)

    monkeypatch.setattr(tfcoil_variables, "tfleng", tfcoilareaandmassesparam.tfleng)

    monkeypatch.setattr(tfcoil_variables, "dcase", tfcoilareaandmassesparam.dcase)

    monkeypatch.setattr(tfcoil_variables, "acndttf", tfcoilareaandmassesparam.acndttf)

    monkeypatch.setattr(
        tfcoil_variables, "n_tf_turn", tfcoilareaandmassesparam.n_tf_turn
    )

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfcoilareaandmassesparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "aiwp", tfcoilareaandmassesparam.aiwp)

    monkeypatch.setattr(tfcoil_variables, "acasetfo", tfcoilareaandmassesparam.acasetfo)

    monkeypatch.setattr(tfcoil_variables, "acasetf", tfcoilareaandmassesparam.acasetf)

    monkeypatch.setattr(tfcoil_variables, "fcutfsu", tfcoilareaandmassesparam.fcutfsu)

    monkeypatch.setattr(tfcoil_variables, "awphec", tfcoilareaandmassesparam.awphec)

    monkeypatch.setattr(tfcoil_variables, "acstf", tfcoilareaandmassesparam.acstf)

    monkeypatch.setattr(tfcoil_variables, "whttflgs", tfcoilareaandmassesparam.whttflgs)

    monkeypatch.setattr(tfcoil_variables, "whtcp", tfcoilareaandmassesparam.whtcp)

    monkeypatch.setattr(tfcoil_variables, "whtconal", tfcoilareaandmassesparam.whtconal)

    monkeypatch.setattr(
        tfcoil_variables, "vol_cond_cp", tfcoilareaandmassesparam.vol_cond_cp
    )

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", tfcoilareaandmassesparam.i_tf_sup)

    monkeypatch.setattr(
        tfcoil_variables, "i_tf_sc_mat", tfcoilareaandmassesparam.i_tf_sc_mat
    )

    monkeypatch.setattr(tfcoil_variables, "arealeg", tfcoilareaandmassesparam.arealeg)

    monkeypatch.setattr(tfcoil_variables, "thkcas", tfcoilareaandmassesparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "voltfleg", tfcoilareaandmassesparam.voltfleg)

    monkeypatch.setattr(tfcoil_variables, "cplen", tfcoilareaandmassesparam.cplen)

    monkeypatch.setattr(physics_variables, "itart", tfcoilareaandmassesparam.itart)

    monkeypatch.setattr(sctfcoil_module, "awpc", tfcoilareaandmassesparam.awpc)

    monkeypatch.setattr(sctfcoil_module, "awptf", tfcoilareaandmassesparam.awptf)

    monkeypatch.setattr(
        sctfcoil_module, "vol_ins_cp", tfcoilareaandmassesparam.vol_ins_cp
    )

    monkeypatch.setattr(
        sctfcoil_module, "vol_gr_ins_cp", tfcoilareaandmassesparam.vol_gr_ins_cp
    )

    monkeypatch.setattr(
        sctfcoil_module, "vol_case_cp", tfcoilareaandmassesparam.vol_case_cp
    )

    monkeypatch.setattr(
        sctfcoil_module, "a_leg_ins", tfcoilareaandmassesparam.a_leg_ins
    )

    monkeypatch.setattr(
        sctfcoil_module, "a_leg_gr_ins", tfcoilareaandmassesparam.a_leg_gr_ins
    )

    monkeypatch.setattr(
        sctfcoil_module, "a_leg_cond", tfcoilareaandmassesparam.a_leg_cond
    )

    monkeypatch.setattr(
        sctfcoil_module, "theta_coil", tfcoilareaandmassesparam.theta_coil
    )

    monkeypatch.setattr(
        sctfcoil_module, "tan_theta_coil", tfcoilareaandmassesparam.tan_theta_coil
    )

    sctfcoil.tf_coil_area_and_masses()

    assert tfcoil_variables.whtconsh == pytest.approx(
        tfcoilareaandmassesparam.expected_whtconsh
    )

    assert tfcoil_variables.whtcas == pytest.approx(
        tfcoilareaandmassesparam.expected_whtcas
    )

    assert tfcoil_variables.tficrn == pytest.approx(
        tfcoilareaandmassesparam.expected_tficrn
    )

    assert tfcoil_variables.tfcryoarea == pytest.approx(
        tfcoilareaandmassesparam.expected_tfcryoarea
    )

    assert tfcoil_variables.tfsao == pytest.approx(
        tfcoilareaandmassesparam.expected_tfsao
    )

    assert tfcoil_variables.whtgw == pytest.approx(
        tfcoilareaandmassesparam.expected_whtgw
    )

    assert tfcoil_variables.tfocrn == pytest.approx(
        tfcoilareaandmassesparam.expected_tfocrn
    )

    assert tfcoil_variables.whtconsc == pytest.approx(
        tfcoilareaandmassesparam.expected_whtconsc
    )

    assert tfcoil_variables.whtconcu == pytest.approx(
        tfcoilareaandmassesparam.expected_whtconcu
    )

    assert tfcoil_variables.whtcon == pytest.approx(
        tfcoilareaandmassesparam.expected_whtcon
    )

    assert tfcoil_variables.whtconin == pytest.approx(
        tfcoilareaandmassesparam.expected_whtconin
    )

    assert tfcoil_variables.cplen == pytest.approx(
        tfcoilareaandmassesparam.expected_cplen
    )


class PeakTfWithRippleParam(NamedTuple):

    tf_fit_t: Any = None

    tf_fit_z: Any = None

    tf_fit_y: Any = None

    n_tf: Any = None

    wwp1: Any = None

    dr_tf_wp: Any = None

    tfin: Any = None

    bmaxtf: Any = None

    expected_tf_fit_t: Any = None

    expected_tf_fit_z: Any = None

    expected_tf_fit_y: Any = None

    expected_bmaxtfrp: Any = None

    expected_flag: Any = None


@pytest.mark.parametrize(
    "peaktfwithrippleparam",
    (
        PeakTfWithRippleParam(
            tf_fit_t=0,
            tf_fit_z=0,
            tf_fit_y=0,
            n_tf=16,
            wwp1=1.299782604942499,
            dr_tf_wp=0.50661087836601015,
            tfin=3.789896624292115,
            bmaxtf=11.717722779177526,
            expected_tf_fit_t=0.80807838916035957,
            expected_tf_fit_z=0.3149613642807837,
            expected_tf_fit_y=1.0658869305062604,
            expected_bmaxtfrp=12.48976756562082,
            expected_flag=0,
        ),
        PeakTfWithRippleParam(
            tf_fit_t=0.80807838916035957,
            tf_fit_z=0.3149613642807837,
            tf_fit_y=1.0658869305062604,
            n_tf=16,
            wwp1=1.299782604942499,
            dr_tf_wp=0.50661087836601015,
            tfin=3.789896624292115,
            bmaxtf=11.717722779177526,
            expected_tf_fit_t=0.80807838916035957,
            expected_tf_fit_z=0.3149613642807837,
            expected_tf_fit_y=1.0658869305062604,
            expected_bmaxtfrp=12.48976756562082,
            expected_flag=0,
        ),
    ),
)
def test_peak_tf_with_ripple(peaktfwithrippleparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for peak_tf_with_ripple.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param peaktfwithrippleparam: the data used to mock and assert in this test.
    :type peaktfwithrippleparam: peaktfwithrippleparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(sctfcoil_module, "tf_fit_t", peaktfwithrippleparam.tf_fit_t)

    monkeypatch.setattr(sctfcoil_module, "tf_fit_z", peaktfwithrippleparam.tf_fit_z)

    monkeypatch.setattr(sctfcoil_module, "tf_fit_y", peaktfwithrippleparam.tf_fit_y)

    bmaxtfrp, flag = sctfcoil.peak_tf_with_ripple(
        n_tf=peaktfwithrippleparam.n_tf,
        wwp1=peaktfwithrippleparam.wwp1,
        dr_tf_wp=peaktfwithrippleparam.dr_tf_wp,
        tfin=peaktfwithrippleparam.tfin,
        bmaxtf=peaktfwithrippleparam.bmaxtf,
    )

    assert sctfcoil_module.tf_fit_t == pytest.approx(
        peaktfwithrippleparam.expected_tf_fit_t
    )

    assert sctfcoil_module.tf_fit_z == pytest.approx(
        peaktfwithrippleparam.expected_tf_fit_z
    )

    assert sctfcoil_module.tf_fit_y == pytest.approx(
        peaktfwithrippleparam.expected_tf_fit_y
    )

    assert bmaxtfrp == pytest.approx(peaktfwithrippleparam.expected_bmaxtfrp)

    assert flag == pytest.approx(peaktfwithrippleparam.expected_flag)


class TfWpGeomParam(NamedTuple):

    tfcth: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    dr_tf_wp: Any = None

    casthi: Any = None

    thkcas: Any = None

    casths: Any = None

    wwp1: Any = None

    wwp2: Any = None

    tinstf: Any = None

    tfinsgap: Any = None

    awpc: Any = None

    awptf: Any = None

    r_wp_inner: Any = None

    r_wp_outer: Any = None

    r_wp_centre: Any = None

    t_wp_toroidal: Any = None

    t_wp_toroidal_av: Any = None

    a_ground_ins: Any = None

    theta_coil: Any = None

    tan_theta_coil: Any = None

    i_tf_wp_geom: Any = None

    expected_wwp1: Any = None

    expected_awpc: Any = None

    expected_awptf: Any = None

    expected_r_wp_inner: Any = None

    expected_r_wp_outer: Any = None

    expected_r_wp_centre: Any = None

    expected_t_wp_toroidal: Any = None

    expected_t_wp_toroidal_av: Any = None

    expected_a_ground_ins: Any = None


@pytest.mark.parametrize(
    "tfwpgeomparam",
    (
        TfWpGeomParam(
            tfcth=1.208,
            r_tf_inboard_in=2.9939411851091102,
            r_tf_inboard_out=4.20194118510911,
            dr_tf_wp=0.54261087836601019,
            casthi=0.060000000000000012,
            thkcas=0.52465000000000006,
            casths=0.05000000000000001,
            wwp1=0,
            wwp2=0,
            tinstf=0.0080000000000000019,
            tfinsgap=0.01,
            awpc=0,
            awptf=0,
            r_wp_inner=0,
            r_wp_outer=0,
            r_wp_centre=0,
            t_wp_toroidal=0,
            t_wp_toroidal_av=0,
            a_ground_ins=0,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            i_tf_wp_geom=0,
            expected_wwp1=1.299782604942499,
            expected_awpc=0.70527618095271016,
            expected_awptf=0.64024601555360383,
            expected_r_wp_inner=3.5185911851091101,
            expected_r_wp_outer=4.06120206347512,
            expected_r_wp_centre=3.789896624292115,
            expected_t_wp_toroidal=1.299782604942499,
            expected_t_wp_toroidal_av=1.299782604942499,
            expected_a_ground_ins=0.028582295732936136,
        ),
        TfWpGeomParam(
            tfcth=1.208,
            r_tf_inboard_in=2.9939411851091102,
            r_tf_inboard_out=4.20194118510911,
            dr_tf_wp=0.54261087836601019,
            casthi=0.060000000000000012,
            thkcas=0.52465000000000006,
            casths=0.05000000000000001,
            wwp1=1.299782604942499,
            wwp2=0,
            tinstf=0.0080000000000000019,
            tfinsgap=0.01,
            awpc=0.70527618095271016,
            awptf=0.64024601555360383,
            r_wp_inner=3.5185911851091101,
            r_wp_outer=4.06120206347512,
            r_wp_centre=3.789896624292115,
            t_wp_toroidal=1.299782604942499,
            t_wp_toroidal_av=1.299782604942499,
            a_ground_ins=0.028582295732936136,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            i_tf_wp_geom=0,
            expected_wwp1=1.299782604942499,
            expected_awpc=0.70527618095271016,
            expected_awptf=0.64024601555360383,
            expected_r_wp_inner=3.5185911851091101,
            expected_r_wp_outer=4.06120206347512,
            expected_r_wp_centre=3.789896624292115,
            expected_t_wp_toroidal=1.299782604942499,
            expected_t_wp_toroidal_av=1.299782604942499,
            expected_a_ground_ins=0.028582295732936136,
        ),
    ),
)
def test_tf_wp_geom(tfwpgeomparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_wp_geom.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfwpgeomparam: the data used to mock and assert in this test.
    :type tfwpgeomparam: tfwpgeomparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(build_variables, "tfcth", tfwpgeomparam.tfcth)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfwpgeomparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfwpgeomparam.r_tf_inboard_out
    )

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", tfwpgeomparam.dr_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "casthi", tfwpgeomparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "thkcas", tfwpgeomparam.thkcas)

    monkeypatch.setattr(tfcoil_variables, "casths", tfwpgeomparam.casths)

    monkeypatch.setattr(tfcoil_variables, "wwp1", tfwpgeomparam.wwp1)

    monkeypatch.setattr(tfcoil_variables, "wwp2", tfwpgeomparam.wwp2)

    monkeypatch.setattr(tfcoil_variables, "tinstf", tfwpgeomparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "tfinsgap", tfwpgeomparam.tfinsgap)

    monkeypatch.setattr(sctfcoil_module, "awpc", tfwpgeomparam.awpc)

    monkeypatch.setattr(sctfcoil_module, "awptf", tfwpgeomparam.awptf)

    monkeypatch.setattr(sctfcoil_module, "r_wp_inner", tfwpgeomparam.r_wp_inner)

    monkeypatch.setattr(sctfcoil_module, "r_wp_outer", tfwpgeomparam.r_wp_outer)

    monkeypatch.setattr(sctfcoil_module, "r_wp_centre", tfwpgeomparam.r_wp_centre)

    monkeypatch.setattr(sctfcoil_module, "t_wp_toroidal", tfwpgeomparam.t_wp_toroidal)

    monkeypatch.setattr(
        sctfcoil_module, "t_wp_toroidal_av", tfwpgeomparam.t_wp_toroidal_av
    )

    monkeypatch.setattr(sctfcoil_module, "a_ground_ins", tfwpgeomparam.a_ground_ins)

    monkeypatch.setattr(sctfcoil_module, "theta_coil", tfwpgeomparam.theta_coil)

    monkeypatch.setattr(sctfcoil_module, "tan_theta_coil", tfwpgeomparam.tan_theta_coil)

    sctfcoil.tf_wp_geom(i_tf_wp_geom=tfwpgeomparam.i_tf_wp_geom)

    assert tfcoil_variables.wwp1 == pytest.approx(tfwpgeomparam.expected_wwp1)

    assert sctfcoil_module.awpc == pytest.approx(tfwpgeomparam.expected_awpc)

    assert sctfcoil_module.awptf == pytest.approx(tfwpgeomparam.expected_awptf)

    assert sctfcoil_module.r_wp_inner == pytest.approx(
        tfwpgeomparam.expected_r_wp_inner
    )

    assert sctfcoil_module.r_wp_outer == pytest.approx(
        tfwpgeomparam.expected_r_wp_outer
    )

    assert sctfcoil_module.r_wp_centre == pytest.approx(
        tfwpgeomparam.expected_r_wp_centre
    )

    assert sctfcoil_module.t_wp_toroidal == pytest.approx(
        tfwpgeomparam.expected_t_wp_toroidal
    )

    assert sctfcoil_module.t_wp_toroidal_av == pytest.approx(
        tfwpgeomparam.expected_t_wp_toroidal_av
    )

    assert sctfcoil_module.a_ground_ins == pytest.approx(
        tfwpgeomparam.expected_a_ground_ins
    )


class TfCaseGeomParam(NamedTuple):

    acasetf: Any = None

    acasetfo: Any = None

    arealeg: Any = None

    tfareain: Any = None

    n_tf: Any = None

    casths: Any = None

    casthi: Any = None

    dr_tf_wp: Any = None

    r_tf_inboard_in: Any = None

    r_tf_inboard_out: Any = None

    awpc: Any = None

    r_wp_inner: Any = None

    r_wp_outer: Any = None

    t_lat_case_av: Any = None

    a_case_front: Any = None

    a_case_nose: Any = None

    theta_coil: Any = None

    tan_theta_coil: Any = None

    i_tf_wp_geom: Any = None

    i_tf_case_geom: Any = None

    expected_acasetf: Any = None

    expected_acasetfo: Any = None

    expected_t_lat_case_av: Any = None

    expected_a_case_front: Any = None

    expected_a_case_nose: Any = None


@pytest.mark.parametrize(
    "tfcasegeomparam",
    (
        TfCaseGeomParam(
            acasetf=0,
            acasetfo=0,
            arealeg=1.9805354702921749,
            tfareain=27.308689677971632,
            n_tf=16,
            casths=0.05000000000000001,
            casthi=0.060000000000000012,
            dr_tf_wp=0.54261087836601019,
            r_tf_inboard_in=2.9939411851091102,
            r_tf_inboard_out=4.20194118510911,
            awpc=0.70527618095271016,
            r_wp_inner=3.5185911851091101,
            r_wp_outer=4.06120206347512,
            t_lat_case_av=0,
            a_case_front=0,
            a_case_nose=0,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            i_tf_wp_geom=0,
            i_tf_case_geom=0,
            expected_acasetf=1.0015169239205168,
            expected_acasetfo=1.2752592893394648,
            expected_t_lat_case_av=0.10396600719086938,
            expected_a_case_front=0.18607458590131154,
            expected_a_case_nose=0.70261616505511615,
        ),
        TfCaseGeomParam(
            acasetf=1.0015169239205168,
            acasetfo=1.2752592893394648,
            arealeg=1.9805354702921749,
            tfareain=27.308689677971632,
            n_tf=16,
            casths=0.05000000000000001,
            casthi=0.060000000000000012,
            dr_tf_wp=0.54261087836601019,
            r_tf_inboard_in=2.9939411851091102,
            r_tf_inboard_out=4.20194118510911,
            awpc=0.70527618095271016,
            r_wp_inner=3.5185911851091101,
            r_wp_outer=4.06120206347512,
            t_lat_case_av=0.10396600719086938,
            a_case_front=0.18607458590131154,
            a_case_nose=0.70261616505511615,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            i_tf_wp_geom=0,
            i_tf_case_geom=0,
            expected_acasetf=1.0015169239205168,
            expected_acasetfo=1.2752592893394648,
            expected_t_lat_case_av=0.10396600719086938,
            expected_a_case_front=0.18607458590131154,
            expected_a_case_nose=0.70261616505511615,
        ),
    ),
)
def test_tf_case_geom(tfcasegeomparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_case_geom.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfcasegeomparam: the data used to mock and assert in this test.
    :type tfcasegeomparam: tfcasegeomparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "acasetf", tfcasegeomparam.acasetf)

    monkeypatch.setattr(tfcoil_variables, "acasetfo", tfcasegeomparam.acasetfo)

    monkeypatch.setattr(tfcoil_variables, "arealeg", tfcasegeomparam.arealeg)

    monkeypatch.setattr(tfcoil_variables, "tfareain", tfcasegeomparam.tfareain)

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfcasegeomparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "casths", tfcasegeomparam.casths)

    monkeypatch.setattr(tfcoil_variables, "casthi", tfcasegeomparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", tfcasegeomparam.dr_tf_wp)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", tfcasegeomparam.r_tf_inboard_in
    )

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_out", tfcasegeomparam.r_tf_inboard_out
    )

    monkeypatch.setattr(sctfcoil_module, "awpc", tfcasegeomparam.awpc)

    monkeypatch.setattr(sctfcoil_module, "r_wp_inner", tfcasegeomparam.r_wp_inner)

    monkeypatch.setattr(sctfcoil_module, "r_wp_outer", tfcasegeomparam.r_wp_outer)

    monkeypatch.setattr(sctfcoil_module, "t_lat_case_av", tfcasegeomparam.t_lat_case_av)

    monkeypatch.setattr(sctfcoil_module, "a_case_front", tfcasegeomparam.a_case_front)

    monkeypatch.setattr(sctfcoil_module, "a_case_nose", tfcasegeomparam.a_case_nose)

    monkeypatch.setattr(sctfcoil_module, "theta_coil", tfcasegeomparam.theta_coil)

    monkeypatch.setattr(
        sctfcoil_module, "tan_theta_coil", tfcasegeomparam.tan_theta_coil
    )

    sctfcoil.tf_case_geom(
        i_tf_wp_geom=tfcasegeomparam.i_tf_wp_geom,
        i_tf_case_geom=tfcasegeomparam.i_tf_case_geom,
    )

    assert tfcoil_variables.acasetf == pytest.approx(tfcasegeomparam.expected_acasetf)

    assert tfcoil_variables.acasetfo == pytest.approx(tfcasegeomparam.expected_acasetfo)

    assert sctfcoil_module.t_lat_case_av == pytest.approx(
        tfcasegeomparam.expected_t_lat_case_av
    )

    assert sctfcoil_module.a_case_front == pytest.approx(
        tfcasegeomparam.expected_a_case_front
    )

    assert sctfcoil_module.a_case_nose == pytest.approx(
        tfcasegeomparam.expected_a_case_nose
    )


class TfIntegerTurnGeomParam(NamedTuple):

    dr_tf_wp: Any = None

    tinstf: Any = None

    tfinsgap: Any = None

    t_conductor: Any = None

    t_turn_tf: Any = None

    tfc_current: Any = None

    t_wp_toroidal: Any = None

    t_conductor_radial: Any = None

    t_conductor_toroidal: Any = None

    t_cable_radial: Any = None

    t_cable_toroidal: Any = None

    t_turn_radial: Any = None

    t_turn_toroidal: Any = None

    t_cable: Any = None

    n_layer: Any = None

    n_pancake: Any = None

    thwcndut: Any = None

    thicndut: Any = None

    expected_t_conductor: Any = None

    expected_t_turn_tf: Any = None

    expected_t_conductor_radial: Any = None

    expected_t_conductor_toroidal: Any = None

    expected_t_cable_radial: Any = None

    expected_t_cable_toroidal: Any = None

    expected_t_turn_radial: Any = None

    expected_t_turn_toroidal: Any = None

    expected_t_cable: Any = None

    expected_acstf: Any = None

    expected_acndttf: Any = None

    expected_insulation_area: Any = None

    expected_cpttf: Any = None

    expected_n_tf_turn: Any = None


@pytest.mark.parametrize(
    "tfintegerturngeomparam",
    (
        TfIntegerTurnGeomParam(
            dr_tf_wp=0.54261087836601019,
            tinstf=0.0080000000000000019,
            tfinsgap=0.01,
            t_conductor=0,
            t_turn_tf=0,
            tfc_current=14805350.287500001,
            t_wp_toroidal=1.299782604942499,
            t_conductor_radial=0,
            t_conductor_toroidal=0,
            t_cable_radial=0,
            t_cable_toroidal=0,
            t_turn_radial=0,
            t_turn_toroidal=0,
            t_cable=0,
            n_layer=10,
            n_pancake=20,
            thwcndut=0.0080000000000000002,
            thicndut=0.002,
            expected_t_conductor=0.052553108427885735,
            expected_t_turn_tf=0.056579413904423038,
            expected_t_conductor_radial=0.046661087836601015,
            expected_t_conductor_toroidal=0.059189130247124938,
            expected_t_cable_radial=0.030661087836601014,
            expected_t_cable_toroidal=0.043189130247124938,
            expected_t_turn_radial=0.050661087836601018,
            expected_t_turn_toroidal=0.063189130247124942,
            expected_t_cable=0.036389912284773368,
            expected_acstf=0.001293323051622732,
            expected_acndttf=0.0014685061538103825,
            expected_insulation_area=0.00043940087233490435,
            expected_cpttf=74026.751437500003,
            expected_n_tf_turn=200,
        ),
        TfIntegerTurnGeomParam(
            dr_tf_wp=0.54261087836601019,
            tinstf=0.0080000000000000019,
            tfinsgap=0.01,
            t_conductor=0.052553108427885735,
            t_turn_tf=0.056579413904423038,
            tfc_current=14805350.287500001,
            t_wp_toroidal=1.299782604942499,
            t_conductor_radial=0.046661087836601015,
            t_conductor_toroidal=0.059189130247124938,
            t_cable_radial=0.030661087836601014,
            t_cable_toroidal=0.043189130247124938,
            t_turn_radial=0.050661087836601018,
            t_turn_toroidal=0.063189130247124942,
            t_cable=0.036389912284773368,
            n_layer=10,
            n_pancake=20,
            thwcndut=0.0080000000000000002,
            thicndut=0.002,
            expected_t_conductor=0.052553108427885735,
            expected_t_turn_tf=0.056579413904423038,
            expected_t_conductor_radial=0.046661087836601015,
            expected_t_conductor_toroidal=0.059189130247124938,
            expected_t_cable_radial=0.030661087836601014,
            expected_t_cable_toroidal=0.043189130247124938,
            expected_t_turn_radial=0.050661087836601018,
            expected_t_turn_toroidal=0.063189130247124942,
            expected_t_cable=0.036389912284773368,
            expected_acstf=0.001293323051622732,
            expected_acndttf=0.0014685061538103825,
            expected_insulation_area=0.00043940087233490435,
            expected_cpttf=74026.751437500003,
            expected_n_tf_turn=200,
        ),
    ),
)
def test_tf_integer_turn_geom(tfintegerturngeomparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_integer_turn_geom.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tfintegerturngeomparam: the data used to mock and assert in this test.
    :type tfintegerturngeomparam: tfintegerturngeomparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", tfintegerturngeomparam.dr_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "tinstf", tfintegerturngeomparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "tfinsgap", tfintegerturngeomparam.tfinsgap)

    monkeypatch.setattr(
        tfcoil_variables, "t_conductor", tfintegerturngeomparam.t_conductor
    )

    monkeypatch.setattr(tfcoil_variables, "t_turn_tf", tfintegerturngeomparam.t_turn_tf)

    monkeypatch.setattr(
        sctfcoil_module, "tfc_current", tfintegerturngeomparam.tfc_current
    )

    monkeypatch.setattr(
        sctfcoil_module, "t_wp_toroidal", tfintegerturngeomparam.t_wp_toroidal
    )

    monkeypatch.setattr(
        sctfcoil_module, "t_conductor_radial", tfintegerturngeomparam.t_conductor_radial
    )

    monkeypatch.setattr(
        sctfcoil_module,
        "t_conductor_toroidal",
        tfintegerturngeomparam.t_conductor_toroidal,
    )

    monkeypatch.setattr(
        sctfcoil_module, "t_cable_radial", tfintegerturngeomparam.t_cable_radial
    )

    monkeypatch.setattr(
        sctfcoil_module, "t_cable_toroidal", tfintegerturngeomparam.t_cable_toroidal
    )

    monkeypatch.setattr(
        sctfcoil_module, "t_turn_radial", tfintegerturngeomparam.t_turn_radial
    )

    monkeypatch.setattr(
        sctfcoil_module, "t_turn_toroidal", tfintegerturngeomparam.t_turn_toroidal
    )

    monkeypatch.setattr(sctfcoil_module, "t_cable", tfintegerturngeomparam.t_cable)

    (
        acstf,
        acndttf,
        insulation_area,
        cpttf,
        n_tf_turn,
    ) = sctfcoil.tf_integer_turn_geom(
        n_layer=tfintegerturngeomparam.n_layer,
        n_pancake=tfintegerturngeomparam.n_pancake,
        thwcndut=tfintegerturngeomparam.thwcndut,
        thicndut=tfintegerturngeomparam.thicndut,
    )

    assert tfcoil_variables.t_conductor == pytest.approx(
        tfintegerturngeomparam.expected_t_conductor
    )

    assert tfcoil_variables.t_turn_tf == pytest.approx(
        tfintegerturngeomparam.expected_t_turn_tf
    )

    assert sctfcoil_module.t_conductor_radial == pytest.approx(
        tfintegerturngeomparam.expected_t_conductor_radial
    )

    assert sctfcoil_module.t_conductor_toroidal == pytest.approx(
        tfintegerturngeomparam.expected_t_conductor_toroidal
    )

    assert sctfcoil_module.t_cable_radial == pytest.approx(
        tfintegerturngeomparam.expected_t_cable_radial
    )

    assert sctfcoil_module.t_cable_toroidal == pytest.approx(
        tfintegerturngeomparam.expected_t_cable_toroidal
    )

    assert sctfcoil_module.t_turn_radial == pytest.approx(
        tfintegerturngeomparam.expected_t_turn_radial
    )

    assert sctfcoil_module.t_turn_toroidal == pytest.approx(
        tfintegerturngeomparam.expected_t_turn_toroidal
    )

    assert sctfcoil_module.t_cable == pytest.approx(
        tfintegerturngeomparam.expected_t_cable
    )

    assert acstf == pytest.approx(tfintegerturngeomparam.expected_acstf)

    assert acndttf == pytest.approx(tfintegerturngeomparam.expected_acndttf)

    assert insulation_area == pytest.approx(
        tfintegerturngeomparam.expected_insulation_area
    )

    assert cpttf == pytest.approx(tfintegerturngeomparam.expected_cpttf)

    assert n_tf_turn == pytest.approx(tfintegerturngeomparam.expected_n_tf_turn)


# class TfAveragedTurnGeomParam(NamedTuple):

#     layer_ins: Any = None

#     t_conductor: Any = None

#     t_turn_tf: Any = None

#     t_turn_tf_is_input: Any = None

#     cpttf: Any = None

#     t_cable_tf: Any = None

#     t_cable_tf_is_input: Any = None

#     awptf: Any = None

#     t_turn_radial: Any = None

#     t_turn_toroidal: Any = None

#     t_cable: Any = None

#     i_tf_sc_mat: Any = None

#     jwptf: Any = None

#     thwcndut: Any = None

#     thicndut: Any = None

#     expected_t_conductor: Any = None

#     expected_t_turn_tf: Any = None

#     expected_t_turn_radial: Any = None

#     expected_t_turn_toroidal: Any = None

#     expected_t_cable: Any = None

#     expected_acstf: Any = None

#     expected_acndttf: Any = None

#     expected_insulation_area: Any = None

#     expected_n_tf_turn: Any = None


# @pytest.mark.parametrize(
#     "tfaveragedturngeomparam",
#     (
#         TfAveragedTurnGeomParam(
#             layer_ins=0,
#             t_conductor=0,
#             t_turn_tf=0,
#             t_turn_tf_is_input=False,
#             cpttf=65000,
#             t_cable_tf=0,
#             t_cable_tf_is_input=False,
#             awptf=0.60510952642236249,
#             t_turn_radial=0,
#             t_turn_toroidal=0,
#             t_cable=0,
#             i_tf_sc_mat=5,
#             jwptf=26493137.688284047,
#             thwcndut=0.0080000000000000019,
#             thicndut=0.00080000000000000004,
#             expected_t_conductor=0.047932469413859431,
#             expected_t_turn_tf=0.049532469413859428,
#             expected_t_turn_radial=0.049532469413859428,
#             expected_t_turn_toroidal=0.049532469413859428,
#             expected_t_cable=0.031932469413859424,
#             expected_acstf=0.00098877993839630008,
#             expected_acndttf=0.0013087416857142699,
#             expected_insulation_area=0.00015594390212434958,
#             expected_n_tf_turn=246.63461538461544,
#         ),
#         TfAveragedTurnGeomParam(
#             layer_ins=0,
#             t_conductor=0.047932469413859431,
#             t_turn_tf=0.049532469413859428,
#             t_turn_tf_is_input=False,
#             cpttf=65000,
#             t_cable_tf=0,
#             t_cable_tf_is_input=False,
#             awptf=0.60510952642236249,
#             t_turn_radial=0.049532469413859428,
#             t_turn_toroidal=0.049532469413859428,
#             t_cable=0.031932469413859424,
#             i_tf_sc_mat=5,
#             jwptf=26493137.688284047,
#             thwcndut=0.0080000000000000019,
#             thicndut=0.00080000000000000004,
#             expected_t_conductor=0.047932469413859431,
#             expected_t_turn_tf=0.049532469413859428,
#             expected_t_turn_radial=0.049532469413859428,
#             expected_t_turn_toroidal=0.049532469413859428,
#             expected_t_cable=0.031932469413859424,
#             expected_acstf=0.00098877993839630008,
#             expected_acndttf=0.0013087416857142699,
#             expected_insulation_area=0.00015594390212434958,
#             expected_n_tf_turn=246.63461538461544,
#         ),
#     ),
# )
# def test_tf_averaged_turn_geom(tfaveragedturngeomparam, monkeypatch, sctfcoil):
#     """
#     Automatically generated Regression Unit Test for tf_averaged_turn_geom.

#     This test was generated using data from tests/regression/scenarios/i_mode/IN.DAT.

#     :param tfaveragedturngeomparam: the data used to mock and assert in this test.
#     :type tfaveragedturngeomparam: tfaveragedturngeomparam

#     :param monkeypatch: pytest fixture used to mock module/class variables
#     :type monkeypatch: _pytest.monkeypatch.monkeypatch
#     """

#     monkeypatch.setattr(
#         tfcoil_variables, "layer_ins", tfaveragedturngeomparam.layer_ins
#     )

#     monkeypatch.setattr(
#         tfcoil_variables, "t_conductor", tfaveragedturngeomparam.t_conductor
#     )

#     monkeypatch.setattr(
#         tfcoil_variables, "t_turn_tf", tfaveragedturngeomparam.t_turn_tf
#     )

#     monkeypatch.setattr(
#         tfcoil_variables,
#         "t_turn_tf_is_input",
#         tfaveragedturngeomparam.t_turn_tf_is_input,
#     )

#     monkeypatch.setattr(tfcoil_variables, "cpttf", tfaveragedturngeomparam.cpttf)

#     monkeypatch.setattr(
#         tfcoil_variables, "t_cable_tf", tfaveragedturngeomparam.t_cable_tf
#     )

#     monkeypatch.setattr(
#         tfcoil_variables,
#         "t_cable_tf_is_input",
#         tfaveragedturngeomparam.t_cable_tf_is_input,
#     )

#     monkeypatch.setattr(sctfcoil_module, "awptf", tfaveragedturngeomparam.awptf)

#     monkeypatch.setattr(
#         sctfcoil_module, "t_turn_radial", tfaveragedturngeomparam.t_turn_radial
#     )

#     monkeypatch.setattr(
#         sctfcoil_module, "t_turn_toroidal", tfaveragedturngeomparam.t_turn_toroidal
#     )

#     monkeypatch.setattr(sctfcoil_module, "t_cable", tfaveragedturngeomparam.t_cable)

#     acstf, acndttf, insulation_area, n_tf_turn = sctfcoil.tf_averaged_turn_geom(
#         i_tf_sc_mat=tfaveragedturngeomparam.i_tf_sc_mat,
#         jwptf=tfaveragedturngeomparam.jwptf,
#         thwcndut=tfaveragedturngeomparam.thwcndut,
#         thicndut=tfaveragedturngeomparam.thicndut,
#     )

#     assert tfcoil_variables.t_conductor == pytest.approx(
#         tfaveragedturngeomparam.expected_t_conductor
#     )

#     assert tfcoil_variables.t_turn_tf == pytest.approx(
#         tfaveragedturngeomparam.expected_t_turn_tf
#     )

#     assert sctfcoil_module.t_turn_radial == pytest.approx(
#         tfaveragedturngeomparam.expected_t_turn_radial
#     )

#     assert sctfcoil_module.t_turn_toroidal == pytest.approx(
#         tfaveragedturngeomparam.expected_t_turn_toroidal
#     )

#     assert sctfcoil_module.t_cable == pytest.approx(
#         tfaveragedturngeomparam.expected_t_cable
#     )

#     assert acstf == pytest.approx(tfaveragedturngeomparam.expected_acstf)

#     assert acndttf == pytest.approx(tfaveragedturngeomparam.expected_acndttf)

#     assert insulation_area == pytest.approx(
#         tfaveragedturngeomparam.expected_insulation_area
#     )

#     assert n_tf_turn == pytest.approx(tfaveragedturngeomparam.expected_n_tf_turn)


class TfWpCurrentsParam(NamedTuple):

    ritfc: Any = None

    n_tf: Any = None

    jwptf: Any = None

    awptf: Any = None

    expected_jwptf: Any = None


@pytest.mark.parametrize(
    "tfwpcurrentsparam",
    (
        TfWpCurrentsParam(
            ritfc=256500000.00000003,
            n_tf=16,
            jwptf=0,
            awptf=0.60510952642236249,
            expected_jwptf=26493137.688284047,
        ),
        TfWpCurrentsParam(
            ritfc=256500000.00000003,
            n_tf=16,
            jwptf=26493137.688284047,
            awptf=0.60510952642236249,
            expected_jwptf=26493137.688284047,
        ),
    ),
)
def test_tf_wp_currents(tfwpcurrentsparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for tf_wp_currents.

    This test was generated using data from tests/regression/scenarios/i_mode/IN.DAT.

    :param tfwpcurrentsparam: the data used to mock and assert in this test.
    :type tfwpcurrentsparam: tfwpcurrentsparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(tfcoil_variables, "ritfc", tfwpcurrentsparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "n_tf", tfwpcurrentsparam.n_tf)

    monkeypatch.setattr(tfcoil_variables, "jwptf", tfwpcurrentsparam.jwptf)

    monkeypatch.setattr(sctfcoil_module, "awptf", tfwpcurrentsparam.awptf)

    sctfcoil.tf_wp_currents()

    assert tfcoil_variables.jwptf == pytest.approx(tfwpcurrentsparam.expected_jwptf)


class StressclParam(NamedTuple):

    tfcth: Any = None

    r_tf_inboard_mid: Any = None

    bore: Any = None

    ohcth: Any = None

    hmax: Any = None

    r_tf_inboard_in: Any = None

    casestr: Any = None

    n_tf_turn: Any = None

    dr_tf_wp: Any = None

    i_tf_tresca: Any = None

    acstf: Any = None

    vforce: Any = None

    ritfc: Any = None

    jwptf: Any = None

    sig_tf_cs_bucked: Any = None

    sig_tf_case: Any = None

    sig_tf_wp: Any = None

    thwcndut: Any = None

    insstrain: Any = None

    tinstf: Any = None

    thicndut: Any = None

    acndttf: Any = None

    tfinsgap: Any = None

    acasetf: Any = None

    sig_tf_case_max: Any = None

    poisson_steel: Any = None

    poisson_copper: Any = None

    poisson_al: Any = None

    n_tf_graded_layers: Any = None

    i_tf_sup: Any = None

    i_tf_bucking: Any = None

    fcoolcp: Any = None

    eyoung_cond_axial: Any = None

    eyoung_steel: Any = None

    eyoung_res_tf_buck: Any = None

    eyoung_ins: Any = None

    eyoung_al: Any = None

    eyoung_copper: Any = None

    aiwp: Any = None

    aswp: Any = None

    cpttf: Any = None

    n_tf: Any = None

    i_tf_stress_model: Any = None

    sig_tf_wp_max: Any = None

    i_tf_turns_integer: Any = None

    casthi: Any = None

    acond: Any = None

    avwp: Any = None

    awphec: Any = None

    poisson_ins: Any = None

    eyoung_cond_trans: Any = None

    poisson_cond_axial: Any = None

    poisson_cond_trans: Any = None

    dhecoil: Any = None

    fcutfsu: Any = None

    str_wp: Any = None

    n_tf_wp_layers: Any = None

    ipfres: Any = None

    oh_steel_frac: Any = None

    ohhghf: Any = None

    coheof: Any = None

    cohbop: Any = None

    ncls: Any = None

    cptdin: Any = None

    awpc: Any = None

    a_tf_steel: Any = None

    a_tf_ins: Any = None

    r_wp_inner: Any = None

    r_wp_outer: Any = None

    t_wp_toroidal: Any = None

    t_wp_toroidal_av: Any = None

    t_lat_case_av: Any = None

    a_case_front: Any = None

    a_case_nose: Any = None

    theta_coil: Any = None

    tan_theta_coil: Any = None

    t_cable_radial: Any = None

    t_cable: Any = None

    vforce_inboard_tot: Any = None

    iprint: Any = None

    outfile: Any = None

    n_radial_array: Any = None

    n_tf_layer: Any = None

    expected_casestr: Any = None

    expected_sig_tf_case: Any = None

    expected_sig_tf_wp: Any = None

    expected_insstrain: Any = None

    expected_str_wp: Any = None


@pytest.mark.parametrize(
    "stressclparam",
    (
        StressclParam(
            tfcth=1.208,
            r_tf_inboard_mid=3.5979411851091103,
            bore=2.3322000000000003,
            ohcth=0.55242000000000002,
            hmax=9.0730900215620327,
            r_tf_inboard_in=2.9939411851091102,
            casestr=0,
            n_tf_turn=200,
            dr_tf_wp=0.54261087836601019,
            i_tf_tresca=0,
            acstf=0.001293323051622732,
            vforce=250545611.13801825,
            ritfc=236885604.60000002,
            jwptf=23124470.793774806,
            sig_tf_cs_bucked=0,
            sig_tf_case=0,
            sig_tf_wp=0,
            thwcndut=0.0080000000000000002,
            insstrain=0,
            tinstf=0.0080000000000000019,
            thicndut=0.002,
            acndttf=0.0014685061538103825,
            tfinsgap=0.01,
            acasetf=1.0015169239205168,
            sig_tf_case_max=580000000,
            poisson_steel=0.29999999999999999,
            poisson_copper=0.34999999999999998,
            poisson_al=0.34999999999999998,
            n_tf_graded_layers=1,
            i_tf_sup=1,
            i_tf_bucking=1,
            fcoolcp=0.29999999999999999,
            eyoung_cond_axial=0,
            eyoung_steel=205000000000,
            eyoung_res_tf_buck=150000000000,
            eyoung_ins=20000000000,
            eyoung_al=69000000000,
            eyoung_copper=117000000000,
            aiwp=0.087880174466980876,
            aswp=0.29370123076207649,
            cpttf=74026.751437500003,
            n_tf=16,
            i_tf_stress_model=1,
            sig_tf_wp_max=580000000,
            i_tf_turns_integer=1,
            casthi=0.060000000000000012,
            acond=0.1653572639592335,
            avwp=0.07759938309736393,
            awphec=0.015707963267948974,
            poisson_ins=0.34000000000000002,
            eyoung_cond_trans=0,
            poisson_cond_axial=0.30000001192092896,
            poisson_cond_trans=0.30000001192092896,
            dhecoil=0.010000000000000002,
            fcutfsu=0.80884,
            str_wp=0,
            n_tf_wp_layers=5,
            ipfres=0,
            oh_steel_frac=0.57874999999999999,
            ohhghf=0.90000000000000002,
            coheof=20726000,
            cohbop=0,
            ncls=numpy.array(
                numpy.array((1, 1, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0), order="F"), order="F"
            ).transpose(),
            cptdin=numpy.array(
                numpy.array(
                    (
                        42200,
                        42200,
                        42200,
                        42200,
                        43000,
                        43000,
                        43000,
                        43000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                    ),
                    order="F",
                ),
                order="F",
            ).transpose(),
            awpc=0.70527618095271016,
            a_tf_steel=1.2952181546825934,
            a_tf_ins=0.11646247019991701,
            r_wp_inner=3.5185911851091101,
            r_wp_outer=4.06120206347512,
            t_wp_toroidal=1.299782604942499,
            t_wp_toroidal_av=1.299782604942499,
            t_lat_case_av=0.10396600719086938,
            a_case_front=0.18607458590131154,
            a_case_nose=0.70261616505511615,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            t_cable_radial=0.030661087836601014,
            t_cable=0.036389912284773368,
            vforce_inboard_tot=4008729778.208292,
            iprint=0,
            outfile=11,
            n_radial_array=100,
            n_tf_layer=3,
            expected_casestr=0.00094360452596334093,
            expected_sig_tf_case=543381805.25001633,
            expected_sig_tf_wp=397005702.35272157,
            expected_insstrain=-0.006687152422925652,
            expected_str_wp=0.0015619754370069119,
        ),
        StressclParam(
            tfcth=1.208,
            r_tf_inboard_mid=3.5979411851091103,
            bore=2.3322000000000003,
            ohcth=0.55242000000000002,
            hmax=9.0730900215620327,
            r_tf_inboard_in=2.9939411851091102,
            casestr=0.00094360452596334093,
            n_tf_turn=200,
            dr_tf_wp=0.54261087836601019,
            i_tf_tresca=0,
            acstf=0.001293323051622732,
            vforce=250545611.13801825,
            ritfc=236885604.60000002,
            jwptf=23124470.793774806,
            sig_tf_cs_bucked=0,
            sig_tf_case=543381805.25001633,
            sig_tf_wp=397005702.35272157,
            thwcndut=0.0080000000000000002,
            insstrain=0,
            tinstf=0.0080000000000000019,
            thicndut=0.002,
            acndttf=0.0014685061538103825,
            tfinsgap=0.01,
            acasetf=1.0015169239205168,
            sig_tf_case_max=580000000,
            poisson_steel=0.29999999999999999,
            poisson_copper=0.34999999999999998,
            poisson_al=0.34999999999999998,
            n_tf_graded_layers=1,
            i_tf_sup=1,
            i_tf_bucking=1,
            fcoolcp=0.29999999999999999,
            eyoung_cond_axial=0,
            eyoung_steel=205000000000,
            eyoung_res_tf_buck=150000000000,
            eyoung_ins=20000000000,
            eyoung_al=69000000000,
            eyoung_copper=117000000000,
            aiwp=0.087880174466980876,
            aswp=0.29370123076207649,
            cpttf=74026.751437500003,
            n_tf=16,
            i_tf_stress_model=1,
            sig_tf_wp_max=580000000,
            i_tf_turns_integer=1,
            casthi=0.060000000000000012,
            acond=0.1653572639592335,
            avwp=0.07759938309736393,
            awphec=0.015707963267948974,
            poisson_ins=0.34000000000000002,
            eyoung_cond_trans=0,
            poisson_cond_axial=0.30000001192092896,
            poisson_cond_trans=0.30000001192092896,
            dhecoil=0.010000000000000002,
            fcutfsu=0.80884,
            str_wp=0.0015619754370069119,
            n_tf_wp_layers=5,
            ipfres=0,
            oh_steel_frac=0.57874999999999999,
            ohhghf=0.90000000000000002,
            coheof=20726000,
            cohbop=19311657.760000002,
            ncls=numpy.array(
                numpy.array((1, 1, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0), order="F"), order="F"
            ).transpose(),
            cptdin=numpy.array(
                numpy.array(
                    (
                        42200,
                        42200,
                        42200,
                        42200,
                        43000,
                        43000,
                        43000,
                        43000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                        40000,
                    ),
                    order="F",
                ),
                order="F",
            ).transpose(),
            awpc=0.70527618095271016,
            a_tf_steel=1.2952181546825934,
            a_tf_ins=0.11646247019991701,
            r_wp_inner=3.5185911851091101,
            r_wp_outer=4.06120206347512,
            t_wp_toroidal=1.299782604942499,
            t_wp_toroidal_av=1.299782604942499,
            t_lat_case_av=0.10396600719086938,
            a_case_front=0.18607458590131154,
            a_case_nose=0.70261616505511615,
            theta_coil=0.19634954084936207,
            tan_theta_coil=0.19891236737965801,
            t_cable_radial=0.030661087836601014,
            t_cable=0.036389912284773368,
            vforce_inboard_tot=4008729778.208292,
            iprint=0,
            outfile=11,
            n_radial_array=100,
            n_tf_layer=3,
            expected_casestr=0.00094360452596334093,
            expected_sig_tf_case=543381805.25001633,
            expected_sig_tf_wp=397005702.35272157,
            expected_insstrain=-0.006687152422925652,
            expected_str_wp=0.0015619754370069119,
        ),
    ),
)
def test_stresscl(stressclparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for stresscl.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param stressclparam: the data used to mock and assert in this test.
    :type stressclparam: stressclparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(build_variables, "tfcth", stressclparam.tfcth)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_mid", stressclparam.r_tf_inboard_mid
    )

    monkeypatch.setattr(build_variables, "bore", stressclparam.bore)

    monkeypatch.setattr(build_variables, "ohcth", stressclparam.ohcth)

    monkeypatch.setattr(build_variables, "hmax", stressclparam.hmax)

    monkeypatch.setattr(
        build_variables, "r_tf_inboard_in", stressclparam.r_tf_inboard_in
    )

    monkeypatch.setattr(tfcoil_variables, "casestr", stressclparam.casestr)

    monkeypatch.setattr(tfcoil_variables, "n_tf_turn", stressclparam.n_tf_turn)

    monkeypatch.setattr(tfcoil_variables, "dr_tf_wp", stressclparam.dr_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "i_tf_tresca", stressclparam.i_tf_tresca)

    monkeypatch.setattr(tfcoil_variables, "acstf", stressclparam.acstf)

    monkeypatch.setattr(tfcoil_variables, "vforce", stressclparam.vforce)

    monkeypatch.setattr(tfcoil_variables, "ritfc", stressclparam.ritfc)

    monkeypatch.setattr(tfcoil_variables, "jwptf", stressclparam.jwptf)

    monkeypatch.setattr(
        tfcoil_variables, "sig_tf_cs_bucked", stressclparam.sig_tf_cs_bucked
    )

    monkeypatch.setattr(tfcoil_variables, "sig_tf_case", stressclparam.sig_tf_case)

    monkeypatch.setattr(tfcoil_variables, "sig_tf_wp", stressclparam.sig_tf_wp)

    monkeypatch.setattr(tfcoil_variables, "thwcndut", stressclparam.thwcndut)

    monkeypatch.setattr(tfcoil_variables, "insstrain", stressclparam.insstrain)

    monkeypatch.setattr(tfcoil_variables, "tinstf", stressclparam.tinstf)

    monkeypatch.setattr(tfcoil_variables, "thicndut", stressclparam.thicndut)

    monkeypatch.setattr(tfcoil_variables, "acndttf", stressclparam.acndttf)

    monkeypatch.setattr(tfcoil_variables, "tfinsgap", stressclparam.tfinsgap)

    monkeypatch.setattr(tfcoil_variables, "acasetf", stressclparam.acasetf)

    monkeypatch.setattr(
        tfcoil_variables, "sig_tf_case_max", stressclparam.sig_tf_case_max
    )

    monkeypatch.setattr(tfcoil_variables, "poisson_steel", stressclparam.poisson_steel)

    monkeypatch.setattr(
        tfcoil_variables, "poisson_copper", stressclparam.poisson_copper
    )

    monkeypatch.setattr(tfcoil_variables, "poisson_al", stressclparam.poisson_al)

    monkeypatch.setattr(
        tfcoil_variables, "n_tf_graded_layers", stressclparam.n_tf_graded_layers
    )

    monkeypatch.setattr(tfcoil_variables, "i_tf_sup", stressclparam.i_tf_sup)

    monkeypatch.setattr(tfcoil_variables, "i_tf_bucking", stressclparam.i_tf_bucking)

    monkeypatch.setattr(tfcoil_variables, "fcoolcp", stressclparam.fcoolcp)

    monkeypatch.setattr(
        tfcoil_variables, "eyoung_cond_axial", stressclparam.eyoung_cond_axial
    )

    monkeypatch.setattr(tfcoil_variables, "eyoung_steel", stressclparam.eyoung_steel)

    monkeypatch.setattr(
        tfcoil_variables, "eyoung_res_tf_buck", stressclparam.eyoung_res_tf_buck
    )

    monkeypatch.setattr(tfcoil_variables, "eyoung_ins", stressclparam.eyoung_ins)

    monkeypatch.setattr(tfcoil_variables, "eyoung_al", stressclparam.eyoung_al)

    monkeypatch.setattr(tfcoil_variables, "eyoung_copper", stressclparam.eyoung_copper)

    monkeypatch.setattr(tfcoil_variables, "aiwp", stressclparam.aiwp)

    monkeypatch.setattr(tfcoil_variables, "aswp", stressclparam.aswp)

    monkeypatch.setattr(tfcoil_variables, "cpttf", stressclparam.cpttf)

    monkeypatch.setattr(tfcoil_variables, "n_tf", stressclparam.n_tf)

    monkeypatch.setattr(
        tfcoil_variables, "i_tf_stress_model", stressclparam.i_tf_stress_model
    )

    monkeypatch.setattr(tfcoil_variables, "sig_tf_wp_max", stressclparam.sig_tf_wp_max)

    monkeypatch.setattr(
        tfcoil_variables, "i_tf_turns_integer", stressclparam.i_tf_turns_integer
    )

    monkeypatch.setattr(tfcoil_variables, "casthi", stressclparam.casthi)

    monkeypatch.setattr(tfcoil_variables, "acond", stressclparam.acond)

    monkeypatch.setattr(tfcoil_variables, "avwp", stressclparam.avwp)

    monkeypatch.setattr(tfcoil_variables, "awphec", stressclparam.awphec)

    monkeypatch.setattr(tfcoil_variables, "poisson_ins", stressclparam.poisson_ins)

    monkeypatch.setattr(
        tfcoil_variables, "eyoung_cond_trans", stressclparam.eyoung_cond_trans
    )

    monkeypatch.setattr(
        tfcoil_variables, "poisson_cond_axial", stressclparam.poisson_cond_axial
    )

    monkeypatch.setattr(
        tfcoil_variables, "poisson_cond_trans", stressclparam.poisson_cond_trans
    )

    monkeypatch.setattr(tfcoil_variables, "dhecoil", stressclparam.dhecoil)

    monkeypatch.setattr(tfcoil_variables, "fcutfsu", stressclparam.fcutfsu)

    monkeypatch.setattr(tfcoil_variables, "str_wp", stressclparam.str_wp)

    monkeypatch.setattr(
        tfcoil_variables, "n_tf_wp_layers", stressclparam.n_tf_wp_layers
    )

    monkeypatch.setattr(pfcoil_variables, "ipfres", stressclparam.ipfres)

    monkeypatch.setattr(pfcoil_variables, "oh_steel_frac", stressclparam.oh_steel_frac)

    monkeypatch.setattr(pfcoil_variables, "ohhghf", stressclparam.ohhghf)

    monkeypatch.setattr(pfcoil_variables, "coheof", stressclparam.coheof)

    monkeypatch.setattr(pfcoil_variables, "cohbop", stressclparam.cohbop)

    monkeypatch.setattr(pfcoil_variables, "ncls", stressclparam.ncls)

    monkeypatch.setattr(pfcoil_variables, "cptdin", stressclparam.cptdin)

    monkeypatch.setattr(sctfcoil_module, "awpc", stressclparam.awpc)

    monkeypatch.setattr(sctfcoil_module, "a_tf_steel", stressclparam.a_tf_steel)

    monkeypatch.setattr(sctfcoil_module, "a_tf_ins", stressclparam.a_tf_ins)

    monkeypatch.setattr(sctfcoil_module, "r_wp_inner", stressclparam.r_wp_inner)

    monkeypatch.setattr(sctfcoil_module, "r_wp_outer", stressclparam.r_wp_outer)

    monkeypatch.setattr(sctfcoil_module, "t_wp_toroidal", stressclparam.t_wp_toroidal)

    monkeypatch.setattr(
        sctfcoil_module, "t_wp_toroidal_av", stressclparam.t_wp_toroidal_av
    )

    monkeypatch.setattr(sctfcoil_module, "t_lat_case_av", stressclparam.t_lat_case_av)

    monkeypatch.setattr(sctfcoil_module, "a_case_front", stressclparam.a_case_front)

    monkeypatch.setattr(sctfcoil_module, "a_case_nose", stressclparam.a_case_nose)

    monkeypatch.setattr(sctfcoil_module, "theta_coil", stressclparam.theta_coil)

    monkeypatch.setattr(sctfcoil_module, "tan_theta_coil", stressclparam.tan_theta_coil)

    monkeypatch.setattr(sctfcoil_module, "t_cable_radial", stressclparam.t_cable_radial)

    monkeypatch.setattr(sctfcoil_module, "t_cable", stressclparam.t_cable)

    monkeypatch.setattr(
        sctfcoil_module, "vforce_inboard_tot", stressclparam.vforce_inboard_tot
    )

    sctfcoil.stresscl(
        # iprint=stressclparam.iprint,
        # outfile=stressclparam.outfile,
        n_radial_array=stressclparam.n_radial_array,
        n_tf_layer=stressclparam.n_tf_layer,
        output=False,
    )

    assert tfcoil_variables.casestr == pytest.approx(
        stressclparam.expected_casestr, rel=0.01
    )

    assert tfcoil_variables.sig_tf_case == pytest.approx(
        stressclparam.expected_sig_tf_case, rel=0.01
    )

    assert tfcoil_variables.sig_tf_wp == pytest.approx(
        stressclparam.expected_sig_tf_wp, rel=0.01
    )

    assert tfcoil_variables.insstrain == pytest.approx(
        stressclparam.expected_insstrain, rel=0.01
    )

    assert tfcoil_variables.str_wp == pytest.approx(
        stressclparam.expected_str_wp, rel=0.01
    )


class PlaneStressParam(NamedTuple):

    linesolv: Any = None

    n_radial_array: Any = None

    nlayers: Any = None

    nu: Any = None

    rad: Any = None

    ey: Any = None

    j: Any = None

    expected_sigr: Any = None

    expected_sigt: Any = None

    expected_r_deflect: Any = None

    expected_rradius: Any = None


@pytest.mark.parametrize(
    "planestressparam",
    (
        PlaneStressParam(
            n_radial_array=100,
            nlayers=3,
            nu=numpy.array(
                numpy.array(
                    (0.29999999999999999, 0.30904421667064924, 0.29999999999999999),
                    order="F",
                ),
                order="F",
            ).transpose(),
            rad=numpy.array(
                numpy.array(
                    (
                        2.9939411851091102,
                        3.5414797139565706,
                        4.0876202904571599,
                        4.1476202904571595,
                    ),
                    order="F",
                ),
                order="F",
            ).transpose(),
            ey=numpy.array(
                numpy.array(
                    (205000000000, 43126670035.025253, 205000000000), order="F"
                ),
                order="F",
            ).transpose(),
            j=numpy.array(
                numpy.array((0, 18097185.781970859, 0), order="F"), order="F"
            ).transpose(),
            expected_sigr=[
                -2.4424334654893232e-08,
                -638231.8557665064,
                -1272978.0149700916,
                -1904263.8142719308,
                -2532114.3605434895,
                -3156554.5333628617,
                -3777608.9874794306,
                -4395302.155247825,
                -5009658.249030471,
                -5620701.263570728,
                -6228454.978335433,
                -6832942.959828663,
                -7434188.563876485,
                -8032214.937882668,
                -8627045.023056788,
                -9218701.556614354,
                -9807207.073949814,
                -10392583.91078215,
                -10974854.205274446,
                -11554039.900126902,
                -12130162.744644081,
                -12703244.296776902,
                -13273305.925139176,
                -13840368.810999645,
                -14404453.950249646,
                -14965582.155346386,
                -15523774.057233013,
                -16079050.107234603,
                -16631430.578931753,
                -17180935.570011087,
                -17727585.004093204,
                -18271398.632538795,
                -18812396.036232647,
                -19350596.62734648,
                -19886019.651080146,
                -20418684.18738218,
                -20948609.15264967,
                -21475813.301407665,
                -22000315.22796852,
                -22522133.368071437,
                -23041286.000502665,
                -23557791.24869603,
                -24071667.082314655,
                -24582931.318814173,
                -25091601.624986973,
                -25597695.518488474,
                -26101230.36934536,
                -26602223.401446022,
                -27100691.69401342,
                -27596652.183060795,
                -28090121.662830118,
                -28581116.78721403,
                -29069654.071160764,
                -29555749.892062955,
                -30039420.491130333,
                -30520681.974746134,
                -30999550.31580822,
                -31476041.355054256,
                -31950170.80237179,
                -32421954.238093317,
                -32891407.11427586,
                -33358544.755966645,
                -33823382.362453565,
                -34285935.00850168,
                -34746217.64557528,
                -35204245.10304624,
                -35660032.08938842,
                -36113593.19335851,
                -36564942.885163374,
                -37014095.51761435,
                -37461065.327268094,
                -37905866.43555489,
                -38348512.84989392,
                -38789018.46479614,
                -39227397.06295462,
                -39663662.31632255,
                -40097827.78717951,
                -40529906.92918517,
                -40959913.08842186,
                -41387859.50442491,
                -41813759.31120195,
                -42237625.538240544,
                -42659471.11150465,
                -43079308.85442032,
                -43497151.48884999,
                -43913011.636056446,
                -44326901.81765573,
                -44738834.45655985,
                -45148821.87790898,
                -45556876.30999329,
                -45963009.88516481,
                -46367234.64073932,
                -46769562.51988827,
                -47170005.3725208,
                -47568574.95615649,
                -47965282.93678839,
                -48360140.88973655,
                -48753160.30049253,
                -49144352.56555468,
                -49533728.99325403,
                -49921300.80457225,
                -49954359.75079646,
                -49974994.42195266,
                -49983237.62599281,
                -49979121.9572441,
                -49962679.79819685,
                -49933943.32127723,
                -49892944.490593456,
                -49839715.06367475,
                -49774286.59318317,
                -49696690.42861684,
                -49606957.71798863,
                -49505119.40949438,
                -49391206.25315848,
                -49265248.80247163,
                -49127277.41599939,
                -48977322.25898817,
                -48815413.30494866,
                -48641580.33722007,
                -48455852.95053048,
                -48258260.55252396,
                -48048832.36529516,
                -47827597.42688859,
                -47594584.59279354,
                -47349822.53742289,
                -47093339.7555755,
                -46825164.56388797,
                -46545325.10226658,
                -46253849.335311905,
                -45950765.053722985,
                -45636099.87569279,
                -45309881.248289905,
                -44972136.4488203,
                -44622892.586189084,
                -44262176.60223451,
                -43890015.273056686,
                -43506435.21033499,
                -43111462.862629004,
                -42705124.51666748,
                -42287446.29862941,
                -41858454.17540391,
                -41418173.955848075,
                -40966631.29202441,
                -40503851.68043686,
                -40029860.46324171,
                -39544682.82945523,
                -39048343.816151306,
                -38540868.309644595,
                -38022281.04666105,
                -37492606.61550121,
                -36951869.45718825,
                -36400093.8666107,
                -35837303.993648715,
                -35263523.844296955,
                -34678777.281764984,
                -34083088.02758166,
                -33476479.662682313,
                -32858975.628480688,
                -32230599.227943555,
                -31591373.626640167,
                -30941321.853796344,
                -30280466.80333005,
                -29608831.2348803,
                -28926437.77482557,
                -28233308.91729284,
                -27529467.02516012,
                -26814934.331043653,
                -26089732.93828588,
                -25353884.821918853,
                -24607411.829640914,
                -23850335.682764247,
                -23082677.977159116,
                -22304460.184202105,
                -21515703.651693877,
                -20716429.604788203,
                -19906659.1469038,
                -19086413.26062489,
                -18255712.80860509,
                -17414578.534444828,
                -16563031.063583972,
                -15701090.904164698,
                -14828778.447903601,
                -13946113.970939336,
                -13053117.634693496,
                -12149809.48670104,
                -11236209.461455286,
                -10312337.381225778,
                -9378212.956880776,
                -8433855.788702467,
                -7479285.367186973,
                -6514521.073845657,
                -5539582.181996526,
                -4554487.857543052,
                -3559257.15975762,
                -2553909.042047122,
                -1538462.3527192033,
                -512935.8357366017,
                522651.86853182205,
                1568282.2225660088,
                2623936.790960876,
                3689597.239691184,
                3651895.2312386343,
                3614209.818926577,
                3576540.9930156856,
                3538888.7437740113,
                3501253.0614764676,
                3463633.9364053444,
                3426031.358849844,
                3388445.3191064713,
                3350875.807478741,
                3313322.8142772256,
                3275786.3298197784,
                3238266.3444310892,
                3200762.8484432246,
                3163275.8321950175,
                3125805.2860326516,
                3088351.2003092715,
                3050913.5653850324,
                3013492.3716272213,
                2976087.6094100852,
                2938699.269115101,
                2901327.341130488,
                2863971.8158517913,
                2826632.683681444,
                2789309.9350288156,
                2752003.5603105063,
                2714713.549949905,
                2677439.8943775576,
                2640182.584030824,
                2602941.609354269,
                2565716.960799371,
                2528508.6288244454,
                2491316.6038949895,
                2454140.8764832164,
                2416981.4370685695,
                2379838.2761372086,
                2342711.3841823773,
                2305600.751704303,
                2268506.369209956,
                2231428.2272134367,
                2194366.316235515,
                2157320.6268042135,
                2120291.149454151,
                2083277.8747270512,
                2046280.7931714552,
                2009299.8953427651,
                1972335.1718033694,
                1935386.6131223973,
                1898454.2098760128,
                1861537.9526470467,
                1824637.8320254136,
                1787753.838607672,
                1750885.9629973397,
                1714034.1958048972,
                1677198.52764737,
                1640378.9491488906,
                1603575.4509401869,
                1566788.0236590202,
                1530016.6579497717,
                1493261.344463783,
                1456522.0738591612,
                1419798.8368006812,
                1383091.623960128,
                1346400.426015808,
                1309725.23365311,
                1273066.0375638222,
                1236422.82844689,
                1199795.5970077787,
                1163184.3339586959,
                1126589.0300187604,
                1090009.6759136617,
                1053446.2623759524,
                1016898.7801447797,
                980367.2199661784,
                943851.5725928022,
                907351.8287839729,
                870867.9793058997,
                834400.0149312643,
                797947.9264396364,
                761511.704617082,
                725091.3402565063,
                688686.8241575313,
                652298.1471262509,
                615925.2999756229,
                579568.273525077,
                543227.0586009311,
                506901.6460359016,
                470592.0266694955,
                434298.19134793617,
                398020.1309237974,
                361757.83625661384,
                325511.29821222165,
                289280.50766334473,
                253065.45548915488,
                216866.13257541857,
                180682.52981466774,
                144514.63810578472,
                108362.44835449063,
                72225.95147278359,
                36105.13837954948,
            ],
            expected_sigt=[
                -349942877.4275314,
                -349304645.57176495,
                -348669899.41256136,
                -348038613.61325955,
                -347410763.066988,
                -346786322.8941686,
                -346165268.44005203,
                -345547575.2722837,
                -344933219.178501,
                -344322176.1639607,
                -343714422.44919604,
                -343109934.4677028,
                -342508688.863655,
                -341910662.4896488,
                -341315832.4044747,
                -340724175.87091714,
                -340135670.35358167,
                -339550293.5167493,
                -338968023.222257,
                -338388837.5274046,
                -337812714.68288743,
                -337239633.1307546,
                -336669571.5023923,
                -336102508.61653185,
                -335538423.4772818,
                -334977295.2721851,
                -334419103.37029845,
                -333863827.3202969,
                -333311446.84859973,
                -332761941.85752034,
                -332215292.42343825,
                -331671478.7949927,
                -331130481.39129883,
                -330592280.800185,
                -330056857.7764513,
                -329524193.2401493,
                -328994268.2748818,
                -328467064.1261238,
                -327942562.1995629,
                -327420744.05946004,
                -326901591.42702883,
                -326385086.17883545,
                -325871210.3452168,
                -325359946.10871726,
                -324851275.8025445,
                -324345181.90904295,
                -323841647.0581861,
                -323340654.02608544,
                -322842185.73351806,
                -322346225.24447066,
                -321852755.7647013,
                -321361760.64031744,
                -320873223.3563707,
                -320387127.5354685,
                -319903456.9364011,
                -319422195.4527854,
                -318943327.11172324,
                -318466836.0724772,
                -317992706.6251597,
                -317520923.18943816,
                -317051470.3132556,
                -316584332.6715648,
                -316119495.0650779,
                -315656942.4190298,
                -315196659.7819562,
                -314738632.32448524,
                -314282845.33814305,
                -313829284.23417294,
                -313377934.54236805,
                -312928781.9099172,
                -312481812.10026336,
                -312037010.99197656,
                -311594364.57763755,
                -311153858.9627353,
                -310715480.3645769,
                -310279215.1112089,
                -309845049.640352,
                -309412970.4983463,
                -308982964.3391096,
                -308555017.92310655,
                -308129118.11632955,
                -307705251.8892909,
                -307283406.3160268,
                -306863568.5731112,
                -306445725.9386815,
                -306029865.791475,
                -305615975.60987574,
                -305204042.97097164,
                -304794055.5496225,
                -304386001.1175382,
                -303979867.5423667,
                -303575642.78679216,
                -303173314.9076432,
                -302772872.0550107,
                -302374302.471375,
                -301977594.4907431,
                -301582736.53779495,
                -301189717.12703896,
                -300798524.8619768,
                -300409148.4342775,
                -75393985.39087032,
                -75352895.95330013,
                -75308186.27146304,
                -75259848.14709243,
                -75207873.48230305,
                -75152254.27849749,
                -75092982.635284,
                -75030050.74940914,
                -74963450.91370073,
                -74893175.51602733,
                -74819217.03826267,
                -74741568.05526796,
                -74660221.23388477,
                -74575169.33193818,
                -74486405.1972532,
                -74393921.76667804,
                -74297712.06512664,
                -74197769.20462456,
                -74094086.38337062,
                -73986656.88480622,
                -73875474.07669795,
                -73760531.41022801,
                -73641822.41909933,
                -73519340.71864453,
                -73393080.00495018,
                -73263034.05399033,
                -73129196.72076792,
                -72991561.93846779,
                -72850123.71761769,
                -72704876.14526016,
                -72555813.3841336,
                -72402929.67186344,
                -72246219.32015613,
                -72085676.71401668,
                -71921296.31095727,
                -71753072.64022708,
                -71581000.30204895,
                -71405073.9668568,
                -71225288.37455449,
                -71041638.33377115,
                -70854118.72113262,
                -70662724.48053451,
                -70467450.62243325,
                -70268292.22313243,
                -70065244.4240877,
                -69858302.43121327,
                -69647461.51419875,
                -69432717.00583516,
                -69214064.30134167,
                -68991498.85770856,
                -68765016.19304176,
                -68534611.8859144,
                -68300281.57473324,
                -68062020.95709684,
                -67819825.78918111,
                -67573691.88511074,
                -67323615.11635438,
                -67069591.41111759,
                -66811616.75374359,
                -66549687.18412141,
                -66283798.797103256,
                -66013947.74192255,
                -65740130.22162387,
                -65462342.49249586,
                -65180580.86350979,
                -64894841.695768714,
                -64605121.401957214,
                -64311416.44580017,
                -64013723.341527745,
                -63712038.653342426,
                -63406358.99489821,
                -63096681.02878188,
                -62783001.46599489,
                -62465317.06545305,
                -62143624.63347747,
                -61817921.02330379,
                -61488203.13458894,
                -61154467.91292381,
                -60816712.34935414,
                -60474933.47990544,
                -60129128.38510988,
                -59779294.189546056,
                -59425428.061372176,
                -59067527.21187808,
                -58705588.895027705,
                -58339610.40701998,
                -57969589.08584027,
                -57595522.31083187,
                -57217407.502260946,
                -56835242.12088977,
                -56449023.667553514,
                -56058749.68274543,
                -55664417.74619955,
                -55266025.4764855,
                -54863570.530602135,
                -54457050.60357594,
                -54046463.4280663,
                -53631806.773974754,
                -53213078.44805462,
                -52790276.293530844,
                -253219448.39249134,
                -253181746.38403878,
                -253144060.97172672,
                -253106392.14581582,
                -253068739.89657417,
                -253031104.21427664,
                -252993485.0892055,
                -252955882.51165,
                -252918296.4719066,
                -252880726.9602789,
                -252843173.96707734,
                -252805637.48261994,
                -252768117.49723122,
                -252730614.00124338,
                -252693126.98499516,
                -252655656.43883282,
                -252618202.35310942,
                -252580764.71818522,
                -252543343.52442738,
                -252505938.76221025,
                -252468550.42191526,
                -252431178.4939306,
                -252393822.96865195,
                -252356483.8364816,
                -252319161.08782896,
                -252281854.7131107,
                -252244564.70275006,
                -252207291.04717773,
                -252170033.736831,
                -252132792.76215443,
                -252095568.1135995,
                -252058359.7816246,
                -252021167.75669512,
                -251983992.0292834,
                -251946832.5898687,
                -251909689.42893735,
                -251872562.53698257,
                -251835451.90450448,
                -251798357.5220101,
                -251761279.38001359,
                -251724217.46903566,
                -251687171.77960438,
                -251650142.3022543,
                -251613129.02752718,
                -251576131.94597158,
                -251539151.04814288,
                -251502186.32460356,
                -251465237.76592252,
                -251428305.36267614,
                -251391389.1054472,
                -251354488.98482555,
                -251317604.99140784,
                -251280737.1157975,
                -251243885.34860504,
                -251207049.68044755,
                -251170230.10194904,
                -251133426.60374033,
                -251096639.1764592,
                -251059867.81074992,
                -251023112.4972639,
                -250986373.22665933,
                -250949649.98960084,
                -250912942.77676025,
                -250876251.57881597,
                -250839576.38645324,
                -250802917.19036397,
                -250766273.98124704,
                -250729646.7498079,
                -250693035.48675886,
                -250656440.18281895,
                -250619860.82871377,
                -250583297.4151761,
                -250546749.9329449,
                -250510218.37276635,
                -250473702.72539294,
                -250437202.9815841,
                -250400719.13210604,
                -250364251.1677314,
                -250327799.0792398,
                -250291362.8574172,
                -250254942.49305668,
                -250218537.97695765,
                -250182149.29992643,
                -250145776.45277578,
                -250109419.42632523,
                -250073078.21140105,
                -250036752.79883608,
                -250000443.17946965,
                -249964149.34414807,
                -249927871.28372398,
                -249891608.98905677,
                -249855362.45101237,
                -249819131.66046348,
                -249782916.60828927,
                -249746717.28537557,
                -249710533.6826148,
                -249674365.79090595,
                -249638213.60115463,
                -249602077.10427296,
                -249565956.29117972,
            ],
            expected_r_deflect=[
                -0.005110772649589637,
                -0.005107979732115243,
                -0.005105208914710189,
                -0.005102460076784812,
                -0.00509973309862519,
                -0.0050970278613852085,
                -0.005094344247078711,
                -0.0050916821385717315,
                -0.005089041419574819,
                -0.005086421974635437,
                -0.005083823689130451,
                -0.00508124644925869,
                -0.005078690142033596,
                -0.005076154655275944,
                -0.005073639877606648,
                -0.005071145698439637,
                -0.005068672007974808,
                -0.0050662186971910635,
                -0.005063785657839406,
                -0.005061372782436119,
                -0.005058979964256019,
                -0.005056607097325768,
                -0.005054254076417275,
                -0.005051920797041146,
                -0.005049607155440222,
                -0.005047313048583168,
                -0.005045038374158145,
                -0.0050427830305665375,
                -0.005040546916916744,
                -0.005038329933018046,
                -0.005036131979374529,
                -0.005033952957179067,
                -0.00503179276830738,
                -0.005029651315312134,
                -0.005027528501417125,
                -0.0050254242305115045,
                -0.005023338407144072,
                -0.005021270936517628,
                -0.005019221724483378,
                -0.005017190677535402,
                -0.005015177702805176,
                -0.00501318270805615,
                -0.0050112056016783785,
                -0.005009246292683213,
                -0.00500730469069804,
                -0.0050053807059610815,
                -0.005003474249316234,
                -0.00500158523220798,
                -0.004999713566676328,
                -0.004997859165351823,
                -0.004996021941450596,
                -0.004994201808769465,
                -0.004992398681681088,
                -0.004990612475129163,
                -0.004988843104623673,
                -0.004987090486236186,
                -0.004985354536595191,
                -0.004983635172881489,
                -0.0049819323128236295,
                -0.004980245874693384,
                -0.004978575777301276,
                -0.004976921939992143,
                -0.004975284282640753,
                -0.004973662725647458,
                -0.00497205718993389,
                -0.004970467596938702,
                -0.00496889386861335,
                -0.004967335927417915,
                -0.004965793696316967,
                -0.0049642670987754675,
                -0.004962756058754715,
                -0.004961260500708327,
                -0.004959780349578261,
                -0.004958315530790879,
                -0.004956865970253039,
                -0.004955431594348239,
                -0.004954012329932786,
                -0.004952608104332005,
                -0.00495121884533649,
                -0.004949844481198384,
                -0.004948484940627697,
                -0.00494714015278866,
                -0.004945810047296113,
                -0.0049444945542119305,
                -0.00494319360404147,
                -0.00494190712773007,
                -0.004940635056659575,
                -0.004939377322644884,
                -0.004938133857930543,
                -0.0049369045951873705,
                -0.004935689467509104,
                -0.004934488408409089,
                -0.004933301351816993,
                -0.0049321282320755515,
                -0.004930968983937346,
                -0.004929823542561608,
                -0.0049286918435110655,
                -0.004927573822748794,
                -0.004926469416635126,
                -0.004925378561924566,
                -0.0049243011957627175,
                -0.00492767542842093,
                -0.00493105473941545,
                -0.0049344376993040034,
                -0.004937822883104445,
                -0.004941208870263711,
                -0.0049445942446273705,
                -0.004947977594409306,
                -0.004951357512161553,
                -0.004954732594744646,
                -0.00495810144329803,
                -0.004961462663210653,
                -0.004964814864092157,
                -0.004968156659744025,
                -0.004971486668130923,
                -0.004974803511352557,
                -0.004978105815615613,
                -0.00498139221120579,
                -0.004984661332460477,
                -0.004987911817741164,
                -0.004991142309406588,
                -0.004994351453785614,
                -0.004997537901150734,
                -0.005000700305691663,
                -0.0050038373254889384,
                -0.005006947622488253,
                -0.005010029862474369,
                -0.005013082715045802,
                -0.005016104853589359,
                -0.0050190949552551545,
                -0.005022051700931451,
                -0.005024973775220265,
                -0.005027859866412632,
                -0.0050307086664643785,
                -0.005033518870972115,
                -0.005036289179149139,
                -0.0050390182938020445,
                -0.005041704921306817,
                -0.005044347771585994,
                -0.0050469455580851585,
                -0.005049496997750119,
                -0.005052000811004348,
                -0.0050544557217261366,
                -0.005056860457226531,
                -0.005059213748226904,
                -0.005061514328837002,
                -0.00506376093653324,
                -0.00506595231213694,
                -0.005068087199792903,
                -0.0050701643469480695,
                -0.005072182504330391,
                -0.005074140425927964,
                -0.005076036868968126,
                -0.0050778705938968605,
                -0.005079640364358401,
                -0.0050813449471749095,
                -0.0050829831123263,
                -0.005084553632930311,
                -0.005086055285222824,
                -0.005087486848537798,
                -0.005088847105288369,
                -0.005090134840946975,
                -0.005091348844026539,
                -0.005092487906061177,
                -0.00509355082158755,
                -0.005094536388125848,
                -0.005095443406161526,
                -0.005096270679126519,
                -0.0050970170133812776,
                -0.005097681218196398,
                -0.005098262105734608,
                -0.005098758491033112,
                -0.005099169191985525,
                -0.005099493029324692,
                -0.005099728826604727,
                -0.00509987541018414,
                -0.005099931609208519,
                -0.005099896255593428,
                -0.005099768184007369,
                -0.005099546231855212,
                -0.005099229239261538,
                -0.005098816049053878,
                -0.0050983055067464755,
                -0.005097696460524048,
                -0.005096987761225497,
                -0.005096178262328027,
                -0.005095266819931027,
                -0.005094252292740603,
                -0.005093133542053485,
                -0.0050919094317419256,
                -0.005090578828238018,
                -0.005089140600518294,
                -0.005087593620088843,
                -0.005085936760970022,
                -0.005084168899681357,
                -0.0050822889152271344,
                -0.005080295689081138,
                -0.005078188105172354,
                -0.005075965049870346,
                -0.005073625411970845,
                -0.0050711680826814156,
                -0.0050709350165634055,
                -0.005070702093865363,
                -0.005070469314524161,
                -0.005070236678476709,
                -0.005070004185659953,
                -0.005069771836010877,
                -0.005069539629466499,
                -0.0050693075659638785,
                -0.005069075645440109,
                -0.00506884386783232,
                -0.0050686122330776805,
                -0.005068380741113396,
                -0.005068149391876707,
                -0.005067918185304892,
                -0.005067687121335266,
                -0.005067456199905181,
                -0.005067225420952024,
                -0.005066994784413221,
                -0.005066764290226234,
                -0.0050665339383285605,
                -0.005066303728657736,
                -0.005066073661151332,
                -0.005065843735746956,
                -0.005065613952382253,
                -0.0050653843109949035,
                -0.005065154811522624,
                -0.00506492545390317,
                -0.0050646962380743316,
                -0.005064467163973935,
                -0.0050642382315398415,
                -0.005064009440709953,
                -0.005063780791422204,
                -0.005063552283614566,
                -0.0050633239172250466,
                -0.005063095692191691,
                -0.0050628676084525795,
                -0.005062639665945829,
                -0.0050624118646095916,
                -0.0050621842043820555,
                -0.005061956685201447,
                -0.0050617293070060266,
                -0.005061502069734092,
                -0.005061274973323974,
                -0.005061048017714043,
                -0.0050608212028427045,
                -0.005060594528648399,
                -0.005060367995069603,
                -0.0050601416020448296,
                -0.005059915349512625,
                -0.005059689237411577,
                -0.005059463265680303,
                -0.005059237434257461,
                -0.005059011743081741,
                -0.005058786192091869,
                -0.005058560781226611,
                -0.005058335510424764,
                -0.005058110379625163,
                -0.005057885388766677,
                -0.005057660537788212,
                -0.005057435826628708,
                -0.005057211255227144,
                -0.005056986823522529,
                -0.005056762531453913,
                -0.005056538378960378,
                -0.0050563143659810425,
                -0.005056090492455059,
                -0.00505586675832162,
                -0.005055643163519947,
                -0.005055419707989301,
                -0.005055196391668977,
                -0.005054973214498306,
                -0.005054750176416653,
                -0.005054527277363419,
                -0.005054304517278041,
                -0.0050540818960999895,
                -0.005053859413768771,
                -0.005053637070223927,
                -0.005053414865405034,
                -0.0050531927992517045,
                -0.005052970871703585,
                -0.005052749082700356,
                -0.005052527432181736,
                -0.005052305920087477,
                -0.005052084546357364,
                -0.005051863310931219,
                -0.005051642213748901,
                -0.005051421254750298,
                -0.005051200433875337,
                -0.005050979751063981,
                -0.005050759206256223,
                -0.005050538799392096,
                -0.005050318530411664,
                -0.005050098399255028,
                -0.005049878405862322,
                -0.005049658550173715,
                -0.005049438832129412,
                -0.005049219251669652,
                -0.005048999808734708,
                -0.0050487805032648865,
            ],
            expected_rradius=[
                2.9939411851091102,
                2.999416570397585,
                3.0048919556860594,
                3.010367340974534,
                3.0158427262630085,
                3.021318111551483,
                3.0267934968399577,
                3.0322688821284323,
                3.037744267416907,
                3.043219652705382,
                3.0486950379938564,
                3.054170423282331,
                3.0596458085708056,
                3.06512119385928,
                3.0705965791477547,
                3.0760719644362293,
                3.081547349724704,
                3.0870227350131785,
                3.092498120301653,
                3.0979735055901276,
                3.103448890878602,
                3.108924276167077,
                3.1143996614555514,
                3.119875046744026,
                3.1253504320325005,
                3.130825817320975,
                3.13630120260945,
                3.1417765878979247,
                3.1472519731863993,
                3.152727358474874,
                3.1582027437633484,
                3.163678129051823,
                3.1691535143402976,
                3.174628899628772,
                3.1801042849172467,
                3.1855796702057213,
                3.191055055494196,
                3.1965304407826705,
                3.202005826071145,
                3.2074812113596196,
                3.212956596648094,
                3.218431981936569,
                3.223907367225044,
                3.2293827525135184,
                3.234858137801993,
                3.2403335230904675,
                3.245808908378942,
                3.2512842936674167,
                3.2567596789558912,
                3.262235064244366,
                3.2677104495328404,
                3.273185834821315,
                3.2786612201097896,
                3.284136605398264,
                3.2896119906867387,
                3.2950873759752133,
                3.300562761263688,
                3.3060381465521624,
                3.311513531840637,
                3.316988917129112,
                3.3224643024175866,
                3.327939687706061,
                3.3334150729945358,
                3.3388904582830103,
                3.344365843571485,
                3.3498412288599595,
                3.355316614148434,
                3.3607919994369087,
                3.3662673847253832,
                3.371742770013858,
                3.3772181553023324,
                3.382693540590807,
                3.3881689258792815,
                3.393644311167756,
                3.399119696456231,
                3.4045950817447057,
                3.4100704670331803,
                3.415545852321655,
                3.4210212376101294,
                3.426496622898604,
                3.4319720081870786,
                3.437447393475553,
                3.4429227787640277,
                3.4483981640525023,
                3.453873549340977,
                3.4593489346294515,
                3.464824319917926,
                3.4702997052064006,
                3.475775090494875,
                3.48125047578335,
                3.4867258610718244,
                3.492201246360299,
                3.4976766316487735,
                3.5031520169372485,
                3.508627402225723,
                3.5141027875141977,
                3.5195781728026723,
                3.525053558091147,
                3.5305289433796214,
                3.536004328668096,
                3.5414797139565706,
                3.5469411197215766,
                3.5524025254865825,
                3.557863931251588,
                3.563325337016594,
                3.5687867427816,
                3.574248148546606,
                3.579709554311612,
                3.5851709600766175,
                3.5906323658416235,
                3.5960937716066295,
                3.6015551773716354,
                3.6070165831366414,
                3.6124779889016474,
                3.617939394666653,
                3.623400800431659,
                3.628862206196665,
                3.634323611961671,
                3.639785017726677,
                3.6452464234916824,
                3.6507078292566884,
                3.6561692350216943,
                3.6616306407867003,
                3.6670920465517063,
                3.672553452316712,
                3.678014858081718,
                3.6834762638467238,
                3.6889376696117298,
                3.6943990753767357,
                3.6998604811417417,
                3.7053218869067472,
                3.710783292671753,
                3.716244698436759,
                3.721706104201765,
                3.727167509966771,
                3.7326289157317767,
                3.7380903214967827,
                3.7435517272617886,
                3.7490131330267946,
                3.7544745387918006,
                3.759935944556806,
                3.765397350321812,
                3.770858756086818,
                3.776320161851824,
                3.78178156761683,
                3.7872429733818356,
                3.7927043791468416,
                3.7981657849118475,
                3.8036271906768535,
                3.8090885964418595,
                3.8145500022068655,
                3.820011407971871,
                3.825472813736877,
                3.830934219501883,
                3.836395625266889,
                3.841857031031895,
                3.8473184367969004,
                3.8527798425619064,
                3.8582412483269124,
                3.8637026540919184,
                3.8691640598569244,
                3.8746254656219303,
                3.880086871386936,
                3.885548277151942,
                3.891009682916948,
                3.896471088681954,
                3.9019324944469593,
                3.9073939002119653,
                3.9128553059769713,
                3.9183167117419773,
                3.9237781175069832,
                3.929239523271989,
                3.9347009290369948,
                3.9401623348020007,
                3.9456237405670067,
                3.9510851463320127,
                3.9565465520970187,
                3.962007957862024,
                3.96746936362703,
                3.972930769392036,
                3.978392175157042,
                3.983853580922048,
                3.989314986687054,
                3.9947763924520596,
                4.000237798217066,
                4.005699203982071,
                4.011160609747077,
                4.016622015512083,
                4.022083421277089,
                4.027544827042095,
                4.033006232807101,
                4.038467638572107,
                4.043929044337113,
                4.049390450102119,
                4.054851855867125,
                4.06031326163213,
                4.065774667397136,
                4.071236073162142,
                4.076697478927148,
                4.082158884692154,
                4.08762029045716,
                4.08822029045716,
                4.08882029045716,
                4.08942029045716,
                4.09002029045716,
                4.09062029045716,
                4.0912202904571595,
                4.09182029045716,
                4.09242029045716,
                4.09302029045716,
                4.09362029045716,
                4.09422029045716,
                4.09482029045716,
                4.0954202904571595,
                4.09602029045716,
                4.09662029045716,
                4.09722029045716,
                4.09782029045716,
                4.09842029045716,
                4.09902029045716,
                4.0996202904571595,
                4.10022029045716,
                4.10082029045716,
                4.10142029045716,
                4.10202029045716,
                4.10262029045716,
                4.10322029045716,
                4.1038202904571595,
                4.10442029045716,
                4.10502029045716,
                4.10562029045716,
                4.10622029045716,
                4.10682029045716,
                4.10742029045716,
                4.108020290457159,
                4.10862029045716,
                4.10922029045716,
                4.10982029045716,
                4.11042029045716,
                4.1110202904571596,
                4.11162029045716,
                4.112220290457159,
                4.11282029045716,
                4.11342029045716,
                4.11402029045716,
                4.11462029045716,
                4.1152202904571595,
                4.11582029045716,
                4.116420290457159,
                4.11702029045716,
                4.117620290457159,
                4.11822029045716,
                4.11882029045716,
                4.1194202904571595,
                4.12002029045716,
                4.120620290457159,
                4.12122029045716,
                4.121820290457159,
                4.12242029045716,
                4.12302029045716,
                4.1236202904571595,
                4.12422029045716,
                4.124820290457159,
                4.12542029045716,
                4.126020290457159,
                4.12662029045716,
                4.12722029045716,
                4.1278202904571595,
                4.12842029045716,
                4.129020290457159,
                4.12962029045716,
                4.130220290457159,
                4.13082029045716,
                4.13142029045716,
                4.1320202904571595,
                4.13262029045716,
                4.133220290457159,
                4.13382029045716,
                4.134420290457159,
                4.13502029045716,
                4.13562029045716,
                4.136220290457159,
                4.13682029045716,
                4.137420290457159,
                4.13802029045716,
                4.138620290457159,
                4.13922029045716,
                4.13982029045716,
                4.140420290457159,
                4.14102029045716,
                4.141620290457159,
                4.14222029045716,
                4.142820290457159,
                4.1434202904571595,
                4.14402029045716,
                4.144620290457159,
                4.14522029045716,
                4.145820290457159,
                4.14642029045716,
                4.147020290457159,
            ],
        ),
        PlaneStressParam(
            n_radial_array=100,
            nlayers=3,
            nu=numpy.array(
                numpy.array(
                    (0.29999999999999999, 0.30904421667064924, 0.29999999999999999),
                    order="F",
                ),
                order="F",
            ).transpose(),
            rad=numpy.array(
                numpy.array(
                    (
                        2.9939411851091102,
                        3.5414797139565706,
                        4.0876202904571599,
                        4.1476202904571595,
                    ),
                    order="F",
                ),
                order="F",
            ).transpose(),
            ey=numpy.array(
                numpy.array(
                    (205000000000, 43126670035.025253, 205000000000), order="F"
                ),
                order="F",
            ).transpose(),
            j=numpy.array(
                numpy.array((0, 18097185.781970859, 0), order="F"), order="F"
            ).transpose(),
            expected_sigr=[
                -2.4424334654893232e-08,
                -638231.8557665064,
                -1272978.0149700916,
                -1904263.8142719308,
                -2532114.3605434895,
                -3156554.5333628617,
                -3777608.9874794306,
                -4395302.155247825,
                -5009658.249030471,
                -5620701.263570728,
                -6228454.978335433,
                -6832942.959828663,
                -7434188.563876485,
                -8032214.937882668,
                -8627045.023056788,
                -9218701.556614354,
                -9807207.073949814,
                -10392583.91078215,
                -10974854.205274446,
                -11554039.900126902,
                -12130162.744644081,
                -12703244.296776902,
                -13273305.925139176,
                -13840368.810999645,
                -14404453.950249646,
                -14965582.155346386,
                -15523774.057233013,
                -16079050.107234603,
                -16631430.578931753,
                -17180935.570011087,
                -17727585.004093204,
                -18271398.632538795,
                -18812396.036232647,
                -19350596.62734648,
                -19886019.651080146,
                -20418684.18738218,
                -20948609.15264967,
                -21475813.301407665,
                -22000315.22796852,
                -22522133.368071437,
                -23041286.000502665,
                -23557791.24869603,
                -24071667.082314655,
                -24582931.318814173,
                -25091601.624986973,
                -25597695.518488474,
                -26101230.36934536,
                -26602223.401446022,
                -27100691.69401342,
                -27596652.183060795,
                -28090121.662830118,
                -28581116.78721403,
                -29069654.071160764,
                -29555749.892062955,
                -30039420.491130333,
                -30520681.974746134,
                -30999550.31580822,
                -31476041.355054256,
                -31950170.80237179,
                -32421954.238093317,
                -32891407.11427586,
                -33358544.755966645,
                -33823382.362453565,
                -34285935.00850168,
                -34746217.64557528,
                -35204245.10304624,
                -35660032.08938842,
                -36113593.19335851,
                -36564942.885163374,
                -37014095.51761435,
                -37461065.327268094,
                -37905866.43555489,
                -38348512.84989392,
                -38789018.46479614,
                -39227397.06295462,
                -39663662.31632255,
                -40097827.78717951,
                -40529906.92918517,
                -40959913.08842186,
                -41387859.50442491,
                -41813759.31120195,
                -42237625.538240544,
                -42659471.11150465,
                -43079308.85442032,
                -43497151.48884999,
                -43913011.636056446,
                -44326901.81765573,
                -44738834.45655985,
                -45148821.87790898,
                -45556876.30999329,
                -45963009.88516481,
                -46367234.64073932,
                -46769562.51988827,
                -47170005.3725208,
                -47568574.95615649,
                -47965282.93678839,
                -48360140.88973655,
                -48753160.30049253,
                -49144352.56555468,
                -49533728.99325403,
                -49921300.80457225,
                -49954359.75079646,
                -49974994.42195266,
                -49983237.62599281,
                -49979121.9572441,
                -49962679.79819685,
                -49933943.32127723,
                -49892944.490593456,
                -49839715.06367475,
                -49774286.59318317,
                -49696690.42861684,
                -49606957.71798863,
                -49505119.40949438,
                -49391206.25315848,
                -49265248.80247163,
                -49127277.41599939,
                -48977322.25898817,
                -48815413.30494866,
                -48641580.33722007,
                -48455852.95053048,
                -48258260.55252396,
                -48048832.36529516,
                -47827597.42688859,
                -47594584.59279354,
                -47349822.53742289,
                -47093339.7555755,
                -46825164.56388797,
                -46545325.10226658,
                -46253849.335311905,
                -45950765.053722985,
                -45636099.87569279,
                -45309881.248289905,
                -44972136.4488203,
                -44622892.586189084,
                -44262176.60223451,
                -43890015.273056686,
                -43506435.21033499,
                -43111462.862629004,
                -42705124.51666748,
                -42287446.29862941,
                -41858454.17540391,
                -41418173.955848075,
                -40966631.29202441,
                -40503851.68043686,
                -40029860.46324171,
                -39544682.82945523,
                -39048343.816151306,
                -38540868.309644595,
                -38022281.04666105,
                -37492606.61550121,
                -36951869.45718825,
                -36400093.8666107,
                -35837303.993648715,
                -35263523.844296955,
                -34678777.281764984,
                -34083088.02758166,
                -33476479.662682313,
                -32858975.628480688,
                -32230599.227943555,
                -31591373.626640167,
                -30941321.853796344,
                -30280466.80333005,
                -29608831.2348803,
                -28926437.77482557,
                -28233308.91729284,
                -27529467.02516012,
                -26814934.331043653,
                -26089732.93828588,
                -25353884.821918853,
                -24607411.829640914,
                -23850335.682764247,
                -23082677.977159116,
                -22304460.184202105,
                -21515703.651693877,
                -20716429.604788203,
                -19906659.1469038,
                -19086413.26062489,
                -18255712.80860509,
                -17414578.534444828,
                -16563031.063583972,
                -15701090.904164698,
                -14828778.447903601,
                -13946113.970939336,
                -13053117.634693496,
                -12149809.48670104,
                -11236209.461455286,
                -10312337.381225778,
                -9378212.956880776,
                -8433855.788702467,
                -7479285.367186973,
                -6514521.073845657,
                -5539582.181996526,
                -4554487.857543052,
                -3559257.15975762,
                -2553909.042047122,
                -1538462.3527192033,
                -512935.8357366017,
                522651.86853182205,
                1568282.2225660088,
                2623936.790960876,
                3689597.239691184,
                3651895.2312386343,
                3614209.818926577,
                3576540.9930156856,
                3538888.7437740113,
                3501253.0614764676,
                3463633.9364053444,
                3426031.358849844,
                3388445.3191064713,
                3350875.807478741,
                3313322.8142772256,
                3275786.3298197784,
                3238266.3444310892,
                3200762.8484432246,
                3163275.8321950175,
                3125805.2860326516,
                3088351.2003092715,
                3050913.5653850324,
                3013492.3716272213,
                2976087.6094100852,
                2938699.269115101,
                2901327.341130488,
                2863971.8158517913,
                2826632.683681444,
                2789309.9350288156,
                2752003.5603105063,
                2714713.549949905,
                2677439.8943775576,
                2640182.584030824,
                2602941.609354269,
                2565716.960799371,
                2528508.6288244454,
                2491316.6038949895,
                2454140.8764832164,
                2416981.4370685695,
                2379838.2761372086,
                2342711.3841823773,
                2305600.751704303,
                2268506.369209956,
                2231428.2272134367,
                2194366.316235515,
                2157320.6268042135,
                2120291.149454151,
                2083277.8747270512,
                2046280.7931714552,
                2009299.8953427651,
                1972335.1718033694,
                1935386.6131223973,
                1898454.2098760128,
                1861537.9526470467,
                1824637.8320254136,
                1787753.838607672,
                1750885.9629973397,
                1714034.1958048972,
                1677198.52764737,
                1640378.9491488906,
                1603575.4509401869,
                1566788.0236590202,
                1530016.6579497717,
                1493261.344463783,
                1456522.0738591612,
                1419798.8368006812,
                1383091.623960128,
                1346400.426015808,
                1309725.23365311,
                1273066.0375638222,
                1236422.82844689,
                1199795.5970077787,
                1163184.3339586959,
                1126589.0300187604,
                1090009.6759136617,
                1053446.2623759524,
                1016898.7801447797,
                980367.2199661784,
                943851.5725928022,
                907351.8287839729,
                870867.9793058997,
                834400.0149312643,
                797947.9264396364,
                761511.704617082,
                725091.3402565063,
                688686.8241575313,
                652298.1471262509,
                615925.2999756229,
                579568.273525077,
                543227.0586009311,
                506901.6460359016,
                470592.0266694955,
                434298.19134793617,
                398020.1309237974,
                361757.83625661384,
                325511.29821222165,
                289280.50766334473,
                253065.45548915488,
                216866.13257541857,
                180682.52981466774,
                144514.63810578472,
                108362.44835449063,
                72225.95147278359,
                36105.13837954948,
            ],
            expected_sigt=[
                -349942877.4275314,
                -349304645.57176495,
                -348669899.41256136,
                -348038613.61325955,
                -347410763.066988,
                -346786322.8941686,
                -346165268.44005203,
                -345547575.2722837,
                -344933219.178501,
                -344322176.1639607,
                -343714422.44919604,
                -343109934.4677028,
                -342508688.863655,
                -341910662.4896488,
                -341315832.4044747,
                -340724175.87091714,
                -340135670.35358167,
                -339550293.5167493,
                -338968023.222257,
                -338388837.5274046,
                -337812714.68288743,
                -337239633.1307546,
                -336669571.5023923,
                -336102508.61653185,
                -335538423.4772818,
                -334977295.2721851,
                -334419103.37029845,
                -333863827.3202969,
                -333311446.84859973,
                -332761941.85752034,
                -332215292.42343825,
                -331671478.7949927,
                -331130481.39129883,
                -330592280.800185,
                -330056857.7764513,
                -329524193.2401493,
                -328994268.2748818,
                -328467064.1261238,
                -327942562.1995629,
                -327420744.05946004,
                -326901591.42702883,
                -326385086.17883545,
                -325871210.3452168,
                -325359946.10871726,
                -324851275.8025445,
                -324345181.90904295,
                -323841647.0581861,
                -323340654.02608544,
                -322842185.73351806,
                -322346225.24447066,
                -321852755.7647013,
                -321361760.64031744,
                -320873223.3563707,
                -320387127.5354685,
                -319903456.9364011,
                -319422195.4527854,
                -318943327.11172324,
                -318466836.0724772,
                -317992706.6251597,
                -317520923.18943816,
                -317051470.3132556,
                -316584332.6715648,
                -316119495.0650779,
                -315656942.4190298,
                -315196659.7819562,
                -314738632.32448524,
                -314282845.33814305,
                -313829284.23417294,
                -313377934.54236805,
                -312928781.9099172,
                -312481812.10026336,
                -312037010.99197656,
                -311594364.57763755,
                -311153858.9627353,
                -310715480.3645769,
                -310279215.1112089,
                -309845049.640352,
                -309412970.4983463,
                -308982964.3391096,
                -308555017.92310655,
                -308129118.11632955,
                -307705251.8892909,
                -307283406.3160268,
                -306863568.5731112,
                -306445725.9386815,
                -306029865.791475,
                -305615975.60987574,
                -305204042.97097164,
                -304794055.5496225,
                -304386001.1175382,
                -303979867.5423667,
                -303575642.78679216,
                -303173314.9076432,
                -302772872.0550107,
                -302374302.471375,
                -301977594.4907431,
                -301582736.53779495,
                -301189717.12703896,
                -300798524.8619768,
                -300409148.4342775,
                -75393985.39087032,
                -75352895.95330013,
                -75308186.27146304,
                -75259848.14709243,
                -75207873.48230305,
                -75152254.27849749,
                -75092982.635284,
                -75030050.74940914,
                -74963450.91370073,
                -74893175.51602733,
                -74819217.03826267,
                -74741568.05526796,
                -74660221.23388477,
                -74575169.33193818,
                -74486405.1972532,
                -74393921.76667804,
                -74297712.06512664,
                -74197769.20462456,
                -74094086.38337062,
                -73986656.88480622,
                -73875474.07669795,
                -73760531.41022801,
                -73641822.41909933,
                -73519340.71864453,
                -73393080.00495018,
                -73263034.05399033,
                -73129196.72076792,
                -72991561.93846779,
                -72850123.71761769,
                -72704876.14526016,
                -72555813.3841336,
                -72402929.67186344,
                -72246219.32015613,
                -72085676.71401668,
                -71921296.31095727,
                -71753072.64022708,
                -71581000.30204895,
                -71405073.9668568,
                -71225288.37455449,
                -71041638.33377115,
                -70854118.72113262,
                -70662724.48053451,
                -70467450.62243325,
                -70268292.22313243,
                -70065244.4240877,
                -69858302.43121327,
                -69647461.51419875,
                -69432717.00583516,
                -69214064.30134167,
                -68991498.85770856,
                -68765016.19304176,
                -68534611.8859144,
                -68300281.57473324,
                -68062020.95709684,
                -67819825.78918111,
                -67573691.88511074,
                -67323615.11635438,
                -67069591.41111759,
                -66811616.75374359,
                -66549687.18412141,
                -66283798.797103256,
                -66013947.74192255,
                -65740130.22162387,
                -65462342.49249586,
                -65180580.86350979,
                -64894841.695768714,
                -64605121.401957214,
                -64311416.44580017,
                -64013723.341527745,
                -63712038.653342426,
                -63406358.99489821,
                -63096681.02878188,
                -62783001.46599489,
                -62465317.06545305,
                -62143624.63347747,
                -61817921.02330379,
                -61488203.13458894,
                -61154467.91292381,
                -60816712.34935414,
                -60474933.47990544,
                -60129128.38510988,
                -59779294.189546056,
                -59425428.061372176,
                -59067527.21187808,
                -58705588.895027705,
                -58339610.40701998,
                -57969589.08584027,
                -57595522.31083187,
                -57217407.502260946,
                -56835242.12088977,
                -56449023.667553514,
                -56058749.68274543,
                -55664417.74619955,
                -55266025.4764855,
                -54863570.530602135,
                -54457050.60357594,
                -54046463.4280663,
                -53631806.773974754,
                -53213078.44805462,
                -52790276.293530844,
                -253219448.39249134,
                -253181746.38403878,
                -253144060.97172672,
                -253106392.14581582,
                -253068739.89657417,
                -253031104.21427664,
                -252993485.0892055,
                -252955882.51165,
                -252918296.4719066,
                -252880726.9602789,
                -252843173.96707734,
                -252805637.48261994,
                -252768117.49723122,
                -252730614.00124338,
                -252693126.98499516,
                -252655656.43883282,
                -252618202.35310942,
                -252580764.71818522,
                -252543343.52442738,
                -252505938.76221025,
                -252468550.42191526,
                -252431178.4939306,
                -252393822.96865195,
                -252356483.8364816,
                -252319161.08782896,
                -252281854.7131107,
                -252244564.70275006,
                -252207291.04717773,
                -252170033.736831,
                -252132792.76215443,
                -252095568.1135995,
                -252058359.7816246,
                -252021167.75669512,
                -251983992.0292834,
                -251946832.5898687,
                -251909689.42893735,
                -251872562.53698257,
                -251835451.90450448,
                -251798357.5220101,
                -251761279.38001359,
                -251724217.46903566,
                -251687171.77960438,
                -251650142.3022543,
                -251613129.02752718,
                -251576131.94597158,
                -251539151.04814288,
                -251502186.32460356,
                -251465237.76592252,
                -251428305.36267614,
                -251391389.1054472,
                -251354488.98482555,
                -251317604.99140784,
                -251280737.1157975,
                -251243885.34860504,
                -251207049.68044755,
                -251170230.10194904,
                -251133426.60374033,
                -251096639.1764592,
                -251059867.81074992,
                -251023112.4972639,
                -250986373.22665933,
                -250949649.98960084,
                -250912942.77676025,
                -250876251.57881597,
                -250839576.38645324,
                -250802917.19036397,
                -250766273.98124704,
                -250729646.7498079,
                -250693035.48675886,
                -250656440.18281895,
                -250619860.82871377,
                -250583297.4151761,
                -250546749.9329449,
                -250510218.37276635,
                -250473702.72539294,
                -250437202.9815841,
                -250400719.13210604,
                -250364251.1677314,
                -250327799.0792398,
                -250291362.8574172,
                -250254942.49305668,
                -250218537.97695765,
                -250182149.29992643,
                -250145776.45277578,
                -250109419.42632523,
                -250073078.21140105,
                -250036752.79883608,
                -250000443.17946965,
                -249964149.34414807,
                -249927871.28372398,
                -249891608.98905677,
                -249855362.45101237,
                -249819131.66046348,
                -249782916.60828927,
                -249746717.28537557,
                -249710533.6826148,
                -249674365.79090595,
                -249638213.60115463,
                -249602077.10427296,
                -249565956.29117972,
            ],
            expected_r_deflect=[
                -0.005110772649589637,
                -0.005107979732115243,
                -0.005105208914710189,
                -0.005102460076784812,
                -0.00509973309862519,
                -0.0050970278613852085,
                -0.005094344247078711,
                -0.0050916821385717315,
                -0.005089041419574819,
                -0.005086421974635437,
                -0.005083823689130451,
                -0.00508124644925869,
                -0.005078690142033596,
                -0.005076154655275944,
                -0.005073639877606648,
                -0.005071145698439637,
                -0.005068672007974808,
                -0.0050662186971910635,
                -0.005063785657839406,
                -0.005061372782436119,
                -0.005058979964256019,
                -0.005056607097325768,
                -0.005054254076417275,
                -0.005051920797041146,
                -0.005049607155440222,
                -0.005047313048583168,
                -0.005045038374158145,
                -0.0050427830305665375,
                -0.005040546916916744,
                -0.005038329933018046,
                -0.005036131979374529,
                -0.005033952957179067,
                -0.00503179276830738,
                -0.005029651315312134,
                -0.005027528501417125,
                -0.0050254242305115045,
                -0.005023338407144072,
                -0.005021270936517628,
                -0.005019221724483378,
                -0.005017190677535402,
                -0.005015177702805176,
                -0.00501318270805615,
                -0.0050112056016783785,
                -0.005009246292683213,
                -0.00500730469069804,
                -0.0050053807059610815,
                -0.005003474249316234,
                -0.00500158523220798,
                -0.004999713566676328,
                -0.004997859165351823,
                -0.004996021941450596,
                -0.004994201808769465,
                -0.004992398681681088,
                -0.004990612475129163,
                -0.004988843104623673,
                -0.004987090486236186,
                -0.004985354536595191,
                -0.004983635172881489,
                -0.0049819323128236295,
                -0.004980245874693384,
                -0.004978575777301276,
                -0.004976921939992143,
                -0.004975284282640753,
                -0.004973662725647458,
                -0.00497205718993389,
                -0.004970467596938702,
                -0.00496889386861335,
                -0.004967335927417915,
                -0.004965793696316967,
                -0.0049642670987754675,
                -0.004962756058754715,
                -0.004961260500708327,
                -0.004959780349578261,
                -0.004958315530790879,
                -0.004956865970253039,
                -0.004955431594348239,
                -0.004954012329932786,
                -0.004952608104332005,
                -0.00495121884533649,
                -0.004949844481198384,
                -0.004948484940627697,
                -0.00494714015278866,
                -0.004945810047296113,
                -0.0049444945542119305,
                -0.00494319360404147,
                -0.00494190712773007,
                -0.004940635056659575,
                -0.004939377322644884,
                -0.004938133857930543,
                -0.0049369045951873705,
                -0.004935689467509104,
                -0.004934488408409089,
                -0.004933301351816993,
                -0.0049321282320755515,
                -0.004930968983937346,
                -0.004929823542561608,
                -0.0049286918435110655,
                -0.004927573822748794,
                -0.004926469416635126,
                -0.004925378561924566,
                -0.0049243011957627175,
                -0.00492767542842093,
                -0.00493105473941545,
                -0.0049344376993040034,
                -0.004937822883104445,
                -0.004941208870263711,
                -0.0049445942446273705,
                -0.004947977594409306,
                -0.004951357512161553,
                -0.004954732594744646,
                -0.00495810144329803,
                -0.004961462663210653,
                -0.004964814864092157,
                -0.004968156659744025,
                -0.004971486668130923,
                -0.004974803511352557,
                -0.004978105815615613,
                -0.00498139221120579,
                -0.004984661332460477,
                -0.004987911817741164,
                -0.004991142309406588,
                -0.004994351453785614,
                -0.004997537901150734,
                -0.005000700305691663,
                -0.0050038373254889384,
                -0.005006947622488253,
                -0.005010029862474369,
                -0.005013082715045802,
                -0.005016104853589359,
                -0.0050190949552551545,
                -0.005022051700931451,
                -0.005024973775220265,
                -0.005027859866412632,
                -0.0050307086664643785,
                -0.005033518870972115,
                -0.005036289179149139,
                -0.0050390182938020445,
                -0.005041704921306817,
                -0.005044347771585994,
                -0.0050469455580851585,
                -0.005049496997750119,
                -0.005052000811004348,
                -0.0050544557217261366,
                -0.005056860457226531,
                -0.005059213748226904,
                -0.005061514328837002,
                -0.00506376093653324,
                -0.00506595231213694,
                -0.005068087199792903,
                -0.0050701643469480695,
                -0.005072182504330391,
                -0.005074140425927964,
                -0.005076036868968126,
                -0.0050778705938968605,
                -0.005079640364358401,
                -0.0050813449471749095,
                -0.0050829831123263,
                -0.005084553632930311,
                -0.005086055285222824,
                -0.005087486848537798,
                -0.005088847105288369,
                -0.005090134840946975,
                -0.005091348844026539,
                -0.005092487906061177,
                -0.00509355082158755,
                -0.005094536388125848,
                -0.005095443406161526,
                -0.005096270679126519,
                -0.0050970170133812776,
                -0.005097681218196398,
                -0.005098262105734608,
                -0.005098758491033112,
                -0.005099169191985525,
                -0.005099493029324692,
                -0.005099728826604727,
                -0.00509987541018414,
                -0.005099931609208519,
                -0.005099896255593428,
                -0.005099768184007369,
                -0.005099546231855212,
                -0.005099229239261538,
                -0.005098816049053878,
                -0.0050983055067464755,
                -0.005097696460524048,
                -0.005096987761225497,
                -0.005096178262328027,
                -0.005095266819931027,
                -0.005094252292740603,
                -0.005093133542053485,
                -0.0050919094317419256,
                -0.005090578828238018,
                -0.005089140600518294,
                -0.005087593620088843,
                -0.005085936760970022,
                -0.005084168899681357,
                -0.0050822889152271344,
                -0.005080295689081138,
                -0.005078188105172354,
                -0.005075965049870346,
                -0.005073625411970845,
                -0.0050711680826814156,
                -0.0050709350165634055,
                -0.005070702093865363,
                -0.005070469314524161,
                -0.005070236678476709,
                -0.005070004185659953,
                -0.005069771836010877,
                -0.005069539629466499,
                -0.0050693075659638785,
                -0.005069075645440109,
                -0.00506884386783232,
                -0.0050686122330776805,
                -0.005068380741113396,
                -0.005068149391876707,
                -0.005067918185304892,
                -0.005067687121335266,
                -0.005067456199905181,
                -0.005067225420952024,
                -0.005066994784413221,
                -0.005066764290226234,
                -0.0050665339383285605,
                -0.005066303728657736,
                -0.005066073661151332,
                -0.005065843735746956,
                -0.005065613952382253,
                -0.0050653843109949035,
                -0.005065154811522624,
                -0.00506492545390317,
                -0.0050646962380743316,
                -0.005064467163973935,
                -0.0050642382315398415,
                -0.005064009440709953,
                -0.005063780791422204,
                -0.005063552283614566,
                -0.0050633239172250466,
                -0.005063095692191691,
                -0.0050628676084525795,
                -0.005062639665945829,
                -0.0050624118646095916,
                -0.0050621842043820555,
                -0.005061956685201447,
                -0.0050617293070060266,
                -0.005061502069734092,
                -0.005061274973323974,
                -0.005061048017714043,
                -0.0050608212028427045,
                -0.005060594528648399,
                -0.005060367995069603,
                -0.0050601416020448296,
                -0.005059915349512625,
                -0.005059689237411577,
                -0.005059463265680303,
                -0.005059237434257461,
                -0.005059011743081741,
                -0.005058786192091869,
                -0.005058560781226611,
                -0.005058335510424764,
                -0.005058110379625163,
                -0.005057885388766677,
                -0.005057660537788212,
                -0.005057435826628708,
                -0.005057211255227144,
                -0.005056986823522529,
                -0.005056762531453913,
                -0.005056538378960378,
                -0.0050563143659810425,
                -0.005056090492455059,
                -0.00505586675832162,
                -0.005055643163519947,
                -0.005055419707989301,
                -0.005055196391668977,
                -0.005054973214498306,
                -0.005054750176416653,
                -0.005054527277363419,
                -0.005054304517278041,
                -0.0050540818960999895,
                -0.005053859413768771,
                -0.005053637070223927,
                -0.005053414865405034,
                -0.0050531927992517045,
                -0.005052970871703585,
                -0.005052749082700356,
                -0.005052527432181736,
                -0.005052305920087477,
                -0.005052084546357364,
                -0.005051863310931219,
                -0.005051642213748901,
                -0.005051421254750298,
                -0.005051200433875337,
                -0.005050979751063981,
                -0.005050759206256223,
                -0.005050538799392096,
                -0.005050318530411664,
                -0.005050098399255028,
                -0.005049878405862322,
                -0.005049658550173715,
                -0.005049438832129412,
                -0.005049219251669652,
                -0.005048999808734708,
                -0.0050487805032648865,
            ],
            expected_rradius=[
                2.9939411851091102,
                2.999416570397585,
                3.0048919556860594,
                3.010367340974534,
                3.0158427262630085,
                3.021318111551483,
                3.0267934968399577,
                3.0322688821284323,
                3.037744267416907,
                3.043219652705382,
                3.0486950379938564,
                3.054170423282331,
                3.0596458085708056,
                3.06512119385928,
                3.0705965791477547,
                3.0760719644362293,
                3.081547349724704,
                3.0870227350131785,
                3.092498120301653,
                3.0979735055901276,
                3.103448890878602,
                3.108924276167077,
                3.1143996614555514,
                3.119875046744026,
                3.1253504320325005,
                3.130825817320975,
                3.13630120260945,
                3.1417765878979247,
                3.1472519731863993,
                3.152727358474874,
                3.1582027437633484,
                3.163678129051823,
                3.1691535143402976,
                3.174628899628772,
                3.1801042849172467,
                3.1855796702057213,
                3.191055055494196,
                3.1965304407826705,
                3.202005826071145,
                3.2074812113596196,
                3.212956596648094,
                3.218431981936569,
                3.223907367225044,
                3.2293827525135184,
                3.234858137801993,
                3.2403335230904675,
                3.245808908378942,
                3.2512842936674167,
                3.2567596789558912,
                3.262235064244366,
                3.2677104495328404,
                3.273185834821315,
                3.2786612201097896,
                3.284136605398264,
                3.2896119906867387,
                3.2950873759752133,
                3.300562761263688,
                3.3060381465521624,
                3.311513531840637,
                3.316988917129112,
                3.3224643024175866,
                3.327939687706061,
                3.3334150729945358,
                3.3388904582830103,
                3.344365843571485,
                3.3498412288599595,
                3.355316614148434,
                3.3607919994369087,
                3.3662673847253832,
                3.371742770013858,
                3.3772181553023324,
                3.382693540590807,
                3.3881689258792815,
                3.393644311167756,
                3.399119696456231,
                3.4045950817447057,
                3.4100704670331803,
                3.415545852321655,
                3.4210212376101294,
                3.426496622898604,
                3.4319720081870786,
                3.437447393475553,
                3.4429227787640277,
                3.4483981640525023,
                3.453873549340977,
                3.4593489346294515,
                3.464824319917926,
                3.4702997052064006,
                3.475775090494875,
                3.48125047578335,
                3.4867258610718244,
                3.492201246360299,
                3.4976766316487735,
                3.5031520169372485,
                3.508627402225723,
                3.5141027875141977,
                3.5195781728026723,
                3.525053558091147,
                3.5305289433796214,
                3.536004328668096,
                3.5414797139565706,
                3.5469411197215766,
                3.5524025254865825,
                3.557863931251588,
                3.563325337016594,
                3.5687867427816,
                3.574248148546606,
                3.579709554311612,
                3.5851709600766175,
                3.5906323658416235,
                3.5960937716066295,
                3.6015551773716354,
                3.6070165831366414,
                3.6124779889016474,
                3.617939394666653,
                3.623400800431659,
                3.628862206196665,
                3.634323611961671,
                3.639785017726677,
                3.6452464234916824,
                3.6507078292566884,
                3.6561692350216943,
                3.6616306407867003,
                3.6670920465517063,
                3.672553452316712,
                3.678014858081718,
                3.6834762638467238,
                3.6889376696117298,
                3.6943990753767357,
                3.6998604811417417,
                3.7053218869067472,
                3.710783292671753,
                3.716244698436759,
                3.721706104201765,
                3.727167509966771,
                3.7326289157317767,
                3.7380903214967827,
                3.7435517272617886,
                3.7490131330267946,
                3.7544745387918006,
                3.759935944556806,
                3.765397350321812,
                3.770858756086818,
                3.776320161851824,
                3.78178156761683,
                3.7872429733818356,
                3.7927043791468416,
                3.7981657849118475,
                3.8036271906768535,
                3.8090885964418595,
                3.8145500022068655,
                3.820011407971871,
                3.825472813736877,
                3.830934219501883,
                3.836395625266889,
                3.841857031031895,
                3.8473184367969004,
                3.8527798425619064,
                3.8582412483269124,
                3.8637026540919184,
                3.8691640598569244,
                3.8746254656219303,
                3.880086871386936,
                3.885548277151942,
                3.891009682916948,
                3.896471088681954,
                3.9019324944469593,
                3.9073939002119653,
                3.9128553059769713,
                3.9183167117419773,
                3.9237781175069832,
                3.929239523271989,
                3.9347009290369948,
                3.9401623348020007,
                3.9456237405670067,
                3.9510851463320127,
                3.9565465520970187,
                3.962007957862024,
                3.96746936362703,
                3.972930769392036,
                3.978392175157042,
                3.983853580922048,
                3.989314986687054,
                3.9947763924520596,
                4.000237798217066,
                4.005699203982071,
                4.011160609747077,
                4.016622015512083,
                4.022083421277089,
                4.027544827042095,
                4.033006232807101,
                4.038467638572107,
                4.043929044337113,
                4.049390450102119,
                4.054851855867125,
                4.06031326163213,
                4.065774667397136,
                4.071236073162142,
                4.076697478927148,
                4.082158884692154,
                4.08762029045716,
                4.08822029045716,
                4.08882029045716,
                4.08942029045716,
                4.09002029045716,
                4.09062029045716,
                4.0912202904571595,
                4.09182029045716,
                4.09242029045716,
                4.09302029045716,
                4.09362029045716,
                4.09422029045716,
                4.09482029045716,
                4.0954202904571595,
                4.09602029045716,
                4.09662029045716,
                4.09722029045716,
                4.09782029045716,
                4.09842029045716,
                4.09902029045716,
                4.0996202904571595,
                4.10022029045716,
                4.10082029045716,
                4.10142029045716,
                4.10202029045716,
                4.10262029045716,
                4.10322029045716,
                4.1038202904571595,
                4.10442029045716,
                4.10502029045716,
                4.10562029045716,
                4.10622029045716,
                4.10682029045716,
                4.10742029045716,
                4.108020290457159,
                4.10862029045716,
                4.10922029045716,
                4.10982029045716,
                4.11042029045716,
                4.1110202904571596,
                4.11162029045716,
                4.112220290457159,
                4.11282029045716,
                4.11342029045716,
                4.11402029045716,
                4.11462029045716,
                4.1152202904571595,
                4.11582029045716,
                4.116420290457159,
                4.11702029045716,
                4.117620290457159,
                4.11822029045716,
                4.11882029045716,
                4.1194202904571595,
                4.12002029045716,
                4.120620290457159,
                4.12122029045716,
                4.121820290457159,
                4.12242029045716,
                4.12302029045716,
                4.1236202904571595,
                4.12422029045716,
                4.124820290457159,
                4.12542029045716,
                4.126020290457159,
                4.12662029045716,
                4.12722029045716,
                4.1278202904571595,
                4.12842029045716,
                4.129020290457159,
                4.12962029045716,
                4.130220290457159,
                4.13082029045716,
                4.13142029045716,
                4.1320202904571595,
                4.13262029045716,
                4.133220290457159,
                4.13382029045716,
                4.134420290457159,
                4.13502029045716,
                4.13562029045716,
                4.136220290457159,
                4.13682029045716,
                4.137420290457159,
                4.13802029045716,
                4.138620290457159,
                4.13922029045716,
                4.13982029045716,
                4.140420290457159,
                4.14102029045716,
                4.141620290457159,
                4.14222029045716,
                4.142820290457159,
                4.1434202904571595,
                4.14402029045716,
                4.144620290457159,
                4.14522029045716,
                4.145820290457159,
                4.14642029045716,
                4.147020290457159,
            ],
        ),
    ),
)
def test_plane_stress(planestressparam, monkeypatch, sctfcoil):
    """
    Automatically generated Regression Unit Test for plane_stress.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param planestressparam: the data used to mock and assert in this test.
    :type planestressparam: planestressparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    sigr, sigt, r_deflect, rradius = sctfcoil.plane_stress(
        n_radial_array=planestressparam.n_radial_array,
        nlayers=planestressparam.nlayers,
        nu=planestressparam.nu,
        rad=planestressparam.rad,
        ey=planestressparam.ey,
        j=planestressparam.j,
    )

    assert sigr == pytest.approx(planestressparam.expected_sigr)

    assert sigt == pytest.approx(planestressparam.expected_sigt)

    assert r_deflect == pytest.approx(planestressparam.expected_r_deflect)

    assert rradius == pytest.approx(planestressparam.expected_rradius)
