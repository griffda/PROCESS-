import pytest
import numpy
from typing import NamedTuple, Any


from process.fortran import numerics

from process.fortran import physics_variables

from process.fortran import pulse_variables

from process.fortran import pf_power_variables

from process.fortran import times_variables

from process.fortran import constraint_variables

from process.fortran import pfcoil_variables

from process.pulse import Pulse


@pytest.fixture
def pulse():
    """Provides Pulse object for testing.

    :returns: initialised Pulse object
    :rtype: process.pulse.Pulse
    """
    return Pulse()


class TohswgParam(NamedTuple):

    tohsmn: Any = None

    vpfskv: Any = None

    ncirt: Any = None

    ipfres: Any = None

    nohc: Any = None

    powohres: Any = None

    sxlg: Any = None

    cpt: Any = None

    ric: Any = None

    turns: Any = None

    cptdin: Any = None

    plascur: Any = None

    rmajor: Any = None

    active_constraints: Any = None

    lpulse: Any = None

    outfile: Any = None

    iprint: Any = None

    expected_tohsmn: Any = None


class BurnParam(NamedTuple):

    rplas: Any = None

    vsres: Any = None

    vsind: Any = None

    vsbn: Any = None

    vstot: Any = None

    plascur: Any = None

    facoh: Any = None

    csawth: Any = None

    lpulse: Any = None

    tburn: Any = None

    theat: Any = None

    outfile: Any = None

    iprint: Any = None

    expected_tburn: Any = None


@pytest.mark.parametrize(
    "tohswgparam",
    (
        TohswgParam(
            tohsmn=0,
            vpfskv=0,
            ncirt=8,
            ipfres=0,
            nohc=7,
            powohres=0,
            sxlg=numpy.array(
                (
                    (
                        2.4933245328128733,
                        0.044628616646609699,
                        0.23809409972275222,
                        0.15765363220324183,
                        0.21869592803714535,
                        0.066200200497514364,
                        0.88106839153571093,
                        0.00081513222584746908,
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
                    ),
                    (
                        0.044628616646609699,
                        4.3316688790171067,
                        0.18920709024491103,
                        0.2933332275969967,
                        0.078421246973138292,
                        0.28375289838876006,
                        0.85440319548277999,
                        0.00086087843592316273,
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
                    ),
                    (
                        0.23809409972275222,
                        0.189207090244911,
                        3.1287213343340152,
                        1.1084361059086956,
                        0.72476925375751766,
                        0.39082336057406741,
                        0.54626354354859408,
                        0.0017044090640383977,
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
                    ),
                    (
                        0.15765363220324183,
                        0.2933332275969967,
                        1.1084361059086956,
                        3.1287213343340152,
                        0.39082336057406741,
                        0.72476925375751766,
                        0.54626354354859408,
                        0.0017044090640383977,
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
                    ),
                    (
                        0.21869592803714535,
                        0.078421246973138292,
                        0.72476925375751755,
                        0.39082336057406741,
                        1.3966126540800101,
                        0.15016488330981106,
                        0.32769603485124527,
                        0.00088156051922039398,
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
                    ),
                    (
                        0.06620020049751435,
                        0.28375289838876006,
                        0.39082336057406741,
                        0.72476925375751755,
                        0.15016488330981106,
                        1.3966126540800101,
                        0.32769603485124527,
                        0.00088156051922039398,
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
                    ),
                    (
                        0.88106839153571093,
                        0.85440319548277999,
                        0.54626354354859408,
                        0.54626354354859408,
                        0.32769603485124527,
                        0.32769603485124527,
                        25.013930780082362,
                        0.0049030712741391248,
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
                    ),
                    (
                        0.00081513222584746908,
                        0.00086087843592316273,
                        0.0017044090640383977,
                        0.0017044090640383977,
                        0.00088156051922039398,
                        0.00088156051922039398,
                        0.0049030712741391248,
                        1.6039223939491056e-05,
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
                    ),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                ),
                order="F",
            ).transpose(),
            cpt=numpy.array(
                (
                    (
                        0,
                        0,
                        -0,
                        -0,
                        -0,
                        -0,
                        -0,
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
                    ),
                    (
                        42200,
                        42200,
                        3020.1587941719699,
                        3020.1587941719699,
                        3300.7614790395205,
                        3300.7614790395205,
                        40065.680000000008,
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
                    ),
                    (
                        192.99998911749938,
                        -6144.2544496187575,
                        -42200,
                        -42200,
                        -43000,
                        -43000,
                        -43000,
                        17721306.969367817,
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
                    ),
                    (
                        192.99998911749938,
                        -6144.2544496187575,
                        -42200,
                        -42200,
                        -43000,
                        -43000,
                        -43000,
                        17721306.969367817,
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
                    ),
                    (
                        192.99998911749938,
                        -6144.2544496187575,
                        -42200,
                        -42200,
                        -43000,
                        -43000,
                        -43000,
                        17721306.969367817,
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
                    ),
                    (
                        0,
                        0,
                        -0,
                        -0,
                        -0,
                        -0,
                        -0,
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
                    ),
                ),
                order="F",
            ).transpose(),
            ric=numpy.array(
                numpy.array(
                    (
                        14.742063826112572,
                        20.032681634901596,
                        -8.1098913365453207,
                        -8.1098913365453207,
                        -5.5984385047179748,
                        -5.5984385047179748,
                        -186.98751599968145,
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
                    ),
                    order="F",
                ),
                order="F",
            ).transpose(),
            turns=numpy.array(
                numpy.array(
                    (
                        349.33800535811781,
                        474.70809561378195,
                        192.17751982334883,
                        192.17751982334883,
                        130.19624429576686,
                        130.19624429576686,
                        4348.5468837135222,
                        1,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                    ),
                    order="F",
                ),
                order="F",
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
            plascur=17721306.969367817,
            rmajor=8.8901000000000003,
            active_constraints=(
                True,
                True,
                False,
                False,
                True,
                False,
                False,
                True,
                False,
                False,
                True,
                False,
                True,
                False,
                True,
                True,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                True,
                True,
                True,
                True,
                False,
                False,
                True,
                True,
                True,
                True,
                True,
                True,
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
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                True,
                False,
                True,
                False,
                False,
                True,
                False,
                False,
                True,
                False,
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
                False,
                False,
                False,
                False,
                False,
                False,
                False,
            ),
            lpulse=1,
            outfile=11,
            iprint=0,
            expected_tohsmn=-526.67247746645455,
        ),
        TohswgParam(
            tohsmn=-526.67247746645455,
            vpfskv=20,
            ncirt=8,
            ipfres=0,
            nohc=7,
            powohres=0,
            sxlg=numpy.array(
                (
                    (
                        3.7857701742128254,
                        0.062167683674113128,
                        0.30025750984452332,
                        0.19874280828032037,
                        0.2744601225583434,
                        0.082976826625761502,
                        1.106555259132262,
                        0.0010245405670381989,
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
                    ),
                    (
                        0.062167683674113128,
                        5.2009458910657758,
                        0.20981508662554843,
                        0.32528240077446535,
                        0.086481270467700405,
                        0.3129166150591528,
                        0.94616523358477389,
                        0.00095333590829220006,
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
                    ),
                    (
                        0.30025750984452332,
                        0.20981508662554843,
                        3.1367218790421991,
                        1.1114784104069353,
                        0.72273508891439919,
                        0.38972646092521013,
                        0.54701268968450767,
                        0.0017067464916032066,
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
                    ),
                    (
                        0.19874280828032037,
                        0.32528240077446535,
                        1.1114784104069353,
                        3.1367218790421991,
                        0.38972646092521013,
                        0.72273508891439919,
                        0.54701268968450767,
                        0.0017067464916032066,
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
                    ),
                    (
                        0.2744601225583434,
                        0.086481270467700391,
                        0.7227350889143993,
                        0.38972646092521013,
                        1.3857247860088595,
                        0.14891442656236473,
                        0.32632878326620607,
                        0.00087788236968843749,
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
                    ),
                    (
                        0.082976826625761516,
                        0.3129166150591528,
                        0.38972646092521013,
                        0.7227350889143993,
                        0.14891442656236473,
                        1.3857247860088595,
                        0.32632878326620607,
                        0.00087788236968843749,
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
                    ),
                    (
                        1.106555259132262,
                        0.94616523358477389,
                        0.54701268968450767,
                        0.54701268968450767,
                        0.32632878326620607,
                        0.32632878326620607,
                        25.013930780082362,
                        0.0049030712741391248,
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
                    ),
                    (
                        0.0010245405670381989,
                        0.00095333590829220006,
                        0.0017067464916032066,
                        0.0017067464916032066,
                        0.00087788236968843749,
                        0.00087788236968843749,
                        0.0049030712741391248,
                        1.6039223939491056e-05,
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
                    ),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                    (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                ),
                order="F",
            ).transpose(),
            cpt=numpy.array(
                (
                    (
                        0,
                        0,
                        -0,
                        -0,
                        -0,
                        -0,
                        -0,
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
                    ),
                    (
                        33652.264421185464,
                        38170.398920011168,
                        3066.1011211106288,
                        3066.1011211106288,
                        3142.8828598960731,
                        3142.8828598960731,
                        40065.680000000008,
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
                    ),
                    (
                        42200,
                        42200,
                        -38359.094089176579,
                        -38359.094089176579,
                        -39062.909579046187,
                        -39062.909579046187,
                        7190.2908790616439,
                        17721306.969367817,
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
                    ),
                    (
                        42200,
                        42200,
                        -38359.094089176579,
                        -38359.094089176579,
                        -39062.909579046187,
                        -39062.909579046187,
                        7190.2908790616439,
                        17721306.969367817,
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
                    ),
                    (
                        43.796984391747266,
                        -5616.07162951385,
                        -42200,
                        -42200,
                        -43000,
                        -43000,
                        -43000,
                        17721306.969367817,
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
                    ),
                    (
                        0,
                        0,
                        -0,
                        -0,
                        -0,
                        -0,
                        -0,
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
                    ),
                ),
                order="F",
            ).transpose(),
            ric=numpy.array(
                numpy.array(
                    (
                        18.585545191033798,
                        22.184171359174297,
                        -8.1210132461605689,
                        -8.1210132461605689,
                        -5.5750800471681448,
                        -5.5750800471681448,
                        -186.98751599968145,
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
                    ),
                    order="F",
                ),
                order="F",
            ).transpose(),
            turns=numpy.array(
                numpy.array(
                    (
                        440.41576282070616,
                        525.69126443540983,
                        192.44107218389973,
                        192.44107218389973,
                        129.65302435274756,
                        129.65302435274756,
                        4348.5468837135222,
                        1,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                        100,
                    ),
                    order="F",
                ),
                order="F",
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
            plascur=17721306.969367817,
            rmajor=8.8901000000000003,
            active_constraints=(
                True,
                True,
                False,
                False,
                True,
                False,
                False,
                True,
                False,
                False,
                True,
                False,
                True,
                False,
                True,
                True,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                True,
                True,
                True,
                True,
                False,
                False,
                True,
                True,
                True,
                True,
                True,
                True,
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
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                False,
                True,
                False,
                True,
                False,
                False,
                True,
                False,
                False,
                True,
                False,
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
                False,
                False,
                False,
                False,
                False,
                False,
                False,
            ),
            lpulse=1,
            outfile=11,
            iprint=0,
            expected_tohsmn=51.251726699574235,
        ),
    ),
)
def test_tohswg(tohswgparam, monkeypatch, pulse):
    """
    Automatically generated Regression Unit Test for tohswg.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param tohswgparam: the data used to mock and assert in this test.
    :type tohswgparam: tohswgparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(constraint_variables, "tohsmn", tohswgparam.tohsmn)

    monkeypatch.setattr(pf_power_variables, "vpfskv", tohswgparam.vpfskv)

    monkeypatch.setattr(pfcoil_variables, "ncirt", tohswgparam.ncirt)

    monkeypatch.setattr(pfcoil_variables, "ipfres", tohswgparam.ipfres)

    monkeypatch.setattr(pfcoil_variables, "nohc", tohswgparam.nohc)

    monkeypatch.setattr(pfcoil_variables, "powohres", tohswgparam.powohres)

    monkeypatch.setattr(pfcoil_variables, "sxlg", tohswgparam.sxlg)

    monkeypatch.setattr(pfcoil_variables, "cpt", tohswgparam.cpt)

    monkeypatch.setattr(pfcoil_variables, "ric", tohswgparam.ric)

    monkeypatch.setattr(pfcoil_variables, "turns", tohswgparam.turns)

    monkeypatch.setattr(pfcoil_variables, "cptdin", tohswgparam.cptdin)

    monkeypatch.setattr(physics_variables, "plascur", tohswgparam.plascur)

    monkeypatch.setattr(physics_variables, "rmajor", tohswgparam.rmajor)

    monkeypatch.setattr(numerics, "active_constraints", tohswgparam.active_constraints)

    monkeypatch.setattr(pulse_variables, "lpulse", tohswgparam.lpulse)

    pulse.tohswg(output=False)

    assert constraint_variables.tohsmn == pytest.approx(tohswgparam.expected_tohsmn)


@pytest.mark.parametrize(
    "burnparam",
    (
        BurnParam(
            rplas=3.2347283861249307e-09,
            vsres=59.392760827339345,
            vsind=284.23601098215397,
            vsbn=0,
            vstot=-718.91787876294552,
            plascur=17721306.969367817,
            facoh=0.60433999999999999,
            csawth=1,
            lpulse=1,
            tburn=0,
            theat=10,
            outfile=11,
            iprint=0,
            expected_tburn=0,
        ),
        BurnParam(
            rplas=3.2347283861249307e-09,
            vsres=59.392760827339345,
            vsind=284.23601098215397,
            vstot=-718.9849676846776,
            vsbn=-354.76231817639609,
            plascur=17721306.969367817,
            facoh=0.60433999999999999,
            csawth=1,
            lpulse=1,
            tburn=10234.092022756307,
            theat=10,
            outfile=11,
            iprint=0,
            expected_tburn=10230.533336387545,
        ),
    ),
)
def test_burn(burnparam, monkeypatch, initialise_error_module, pulse):
    """
    Automatically generated Regression Unit Test for burn.

    This test was generated using data from tracking/baseline_2018/baseline_2018_IN.DAT.

    :param burnparam: the data used to mock and assert in this test.
    :type burnparam: burnparam

    :param monkeypatch: pytest fixture used to mock module/class variables
    :type monkeypatch: _pytest.monkeypatch.monkeypatch
    """

    monkeypatch.setattr(physics_variables, "rplas", burnparam.rplas)

    monkeypatch.setattr(physics_variables, "vsres", burnparam.vsres)

    monkeypatch.setattr(physics_variables, "vsind", burnparam.vsind)

    monkeypatch.setattr(pfcoil_variables, "vstot", burnparam.vstot)

    monkeypatch.setattr(pfcoil_variables, "vsbn", burnparam.vsbn)

    monkeypatch.setattr(physics_variables, "plascur", burnparam.plascur)

    monkeypatch.setattr(physics_variables, "facoh", burnparam.facoh)

    monkeypatch.setattr(physics_variables, "csawth", burnparam.csawth)

    monkeypatch.setattr(pulse_variables, "lpulse", burnparam.lpulse)

    monkeypatch.setattr(times_variables, "tburn", burnparam.tburn)

    monkeypatch.setattr(times_variables, "theat", burnparam.theat)

    pulse.burn(output=True)

    assert times_variables.tburn == pytest.approx(burnparam.expected_tburn)
