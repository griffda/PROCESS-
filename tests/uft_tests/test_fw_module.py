###############################################################################
#                                                                             #
#                           UnitFort FW Module Test                           #
#                                                                             #
#   Tests on methods within the FW module contained in the file:              #
#                                                                             #
#   'source/fortran/fw.f90'                                                   #
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
#                                                                             #                                                                           #
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
from process.fortran import fw_module


@pytest.mark.unitfort
def test_fw_thermal_conductivity(uft_data):
    _data = uft_data['fw_thermal_conductivity']

    _pyproc = fw_module.fw_thermal_conductivity(_data['initial']['t'])

    assert _pyproc == _data['final']['fw_thermal_conductivity'], \
        f'fw_thermal_conductivity: {_pyproc} != {_data["final"]["fw_thermal_conductivity"]}'
