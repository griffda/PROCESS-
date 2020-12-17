###############################################################################
#                                                                             #
#                     UnitFort Plasma Geometry Module Test                    #
#                                                                             #
#   Tests on methods within the Plasma Geometry module contained              #
#   in the file:                                                              #
#                                                                             #
#   'source/fortran/plasma_geometry_module.f90'                               #
#                                                                             #
#   within the PROCESS application libraries.                                 #
#                                                                             #
#   @author :   K. Zarebski <kristian.zarebski@ukaea.uk>                      #
#   @date   :   last modified 2020-12-01                                      #
#                                                                             #
#   WARNING: Due to many PROCESS global variables being updated by these      #
#            methods some variables have to be reinitialised. However this    #
#            is only performed on those which were manually altered. As such  #
#            it is highly recommended that the UnitFort tests be run on their #
#            own as there is no guarantee they will not affect others using   #
#            the PROCESS python library.                                      #
#                                                                             #
#            Run using the dedicated marker: pytest -m unitfort               # 
#                                                                             #                                                                            #
#   DISCLAIMER                                                                #
#   ----------                                                                #
#                                                                             #
#   This test was created using the outputs of the UnitFort test parser       #
#   running on a FORTRAN 90 script.The method is 'naive' having no knowledge  #
#   of what the purpose of a method is, and knowing only the variable type    #
#   but not the constraints behind choice of the value of the variable.       #
#   As such the results should NOT be taken as indication of the correct      #
#   values a method returns, but rather used for the purposes of              #
#   regression testing.                                                       #
#                                                                             #
#   UnitFort repository: https://git.ccfe.ac.uk/kzarebsk/unitfort             #
#                                                                             #
###############################################################################

import pytest
from process.fortran import plasma_geometry_module


@pytest.mark.unitfort
def test_xparam(uft_data):
    _data = uft_data['xparam']

    _arg_list = [
        'a',    'kap',  'tri'
    ]

    _res_list = [
        'xi',   'thetai',   'xo',   'thetao'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = plasma_geometry_module.xparam(**_args)

    for i, res in enumerate(_res_list):
        assert _pyproc[i] == _data['final'][res], \
            f"{res}: {_pyproc[i]} != {_data['final'][res]}"


@pytest.mark.unitfort
def test_perim(uft_data):
    _data = uft_data['perim']

    _arg_list = [
        'a',    'kap',  'tri'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = plasma_geometry_module.perim(**_args)

    assert _pyproc == _data['final']['perim'], \
        f'perim: {_pyproc} != {_data["final"]["perim"]}'
