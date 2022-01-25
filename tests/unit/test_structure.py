"""Unit tests for structure.f90"""

import pytest

from process.fortran import structure_module

class TestStructure:
    def test_structure(self):
        """Tests the structure subroutine"""
        ai: float = 17721306.969367817
        r0: float = 8.8901
        a: float = 2.8677741935483869
        akappa: float = 1.848
        b0: float = 5.3292
        tf_h_width: float = 15.337464674334223
        tfhmax: float = 9.0730900215620327
        shldmass: float = 2294873.8131476026
        dvrtmass: float = 43563.275828777645
        pfmass: float = 5446188.2481440185
        tfmass: float = 21234909.756419446
        fwmass: float = 224802.80270851994
        blmass: float = 3501027.3252278985
        coolmass: float = 1199.6389920083477
        dewmass: float = 16426726.727684354
        i_tf_sup: int = 1
        ipfres: int = 0

        expected_fncmass: float = 310716.52923547616
        expected_aintmass: float = 5829865.436088617
        expected_clgsmass: float = 2018975.3864451263
        expected_coldmass: float = 48937690.168336436
        expected_gsm: float = 1685092.6111564008

        fncmass,aintmass,clgsmass,coldmass,gsm = structure_module.structure(ai,r0,a,akappa,b0,i_tf_sup,ipfres,tf_h_width,tfhmax,shldmass,dvrtmass,pfmass,tfmass,fwmass,blmass,coolmass,dewmass, 0, 0)

        assert fncmass == pytest.approx(expected_fncmass)
        assert aintmass == pytest.approx(expected_aintmass)
        assert clgsmass == pytest.approx(expected_clgsmass)
        assert coldmass == pytest.approx(expected_coldmass)
        assert gsm == pytest.approx(expected_gsm)