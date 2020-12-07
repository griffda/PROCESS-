###############################################################################
#                                                                             #
#                        UnitFort Costs 2015 Module Test                      #
#                                                                             #
#   Tests on methods within the Costs 2015 module contained in the file:      #                                                          #
#                                                                             #
#   'source/fortran/costs_2015.f90'                                           #
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
#                                                                             #                                                                          #
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
from process.fortran import costs_2015_module


@pytest.mark.unitfort
def test_value_function(uft_data):
    _data = uft_data['value_function']

    _pyproc = costs_2015_module.value_function(_data['initial']['x'])

    assert _pyproc == _data['final']['v'], \
        f'value_function: {_pyproc} != {_data["final"]["v"]}'
