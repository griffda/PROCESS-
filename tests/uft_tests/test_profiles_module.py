###############################################################################
#                                                                             #
#                        UnitFort Profiles Module Test                        #
#                                                                             #
#   Tests on methods within the Profiles module contained in the file:        #
#                                                                             #
#   'source/fortran/plasma_profiles.f90'                                      #
#                                                                             #
#   within the PROCESS application libraries.                                 #
#                                                                             #
#   @author :   K. Zarebski <kristian.zarebski@ukaea.uk>                      #
#   @date   :   last modified 2020-12-03                                      #
#                                                                             #
#   WARNING: Due to many PROCESS global variables being updated by these      #
#            methods some variables have to be reinitialised. However this    #
#            is only performed on those which were manually altered. As such  #
#            it is highly recommended that the UnitFort tests be run on their #
#            own as there is no guarantee they will not affect others using   #
#            the PROCESS python library.                                      #
#                                                                             #
#            Run using the dedicated marker: pytest -m unitfort               # 
#                                                                             #                                                                             #
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
from process.fortran import profiles_module


@pytest.mark.unitfort
def test_tprofile(uft_data):
    _data = uft_data['tprofile']

    _arg_list = [
        'rho',    'rhopedt',  't0', 'tped', 'tsep', 'alphat',   'tbeta'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = profiles_module.tprofile(**_args)

    assert _pyproc == _data['final']['tprofile'], \
        f'tprofile: {_pyproc} != {_data["final"]["tprofile"]}'


@pytest.mark.unitfort
def test_nprofile(uft_data):
    _data = uft_data['nprofile']

    _arg_list = [
        'rho',    'rhopedn',  'n0', 'nped', 'nsep', 'alphan'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = profiles_module.nprofile(**_args)

    assert _pyproc == _data['final']['nprofile'], \
        f'tprofile: {_pyproc} != {_data["final"]["nprofile"]}'


@pytest.mark.unitfort
def test_ncore(uft_data):
    _data = uft_data['ncore']

    _arg_list = [
        'rhopedn',  'nped', 'nsep', 'nav', 'alphan'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = profiles_module.ncore(**_args)

    assert _pyproc == _data['final']['ncore'], \
        f'tprofile: {_pyproc} != {_data["final"]["ncore"]}'


@pytest.mark.unitfort
def test_tcore(uft_data):
    _data = uft_data['tcore']

    _arg_list = [
        'rhopedt',  'tped', 'tsep', 'tav', 'alphat',    'tbeta'
    ]

    _args = {t: _data['initial'][t] for t in _arg_list}

    _pyproc = profiles_module.tcore(**_args)

    assert _pyproc == _data['final']['tcore'], \
        f'tprofile: {_pyproc} != {_data["final"]["tcore"]}'
