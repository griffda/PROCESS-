###############################################################################
#                                                                             #
#                      MFILE to Python Dictionary Conversion                  #
#                                                                             #
#   Converts data from a PROCESS MFILE to a Python Dictionary with the        #
#   option to then write the result to a file. The format of the output is    #
#   determined by the specified file type which can be JSON, TOML, YAML or    #
#   a Pickle file. If TOMLKit is available the output contains docstrings.    #
#                                                                             #
#   @author :   K. Zarebski <kristian.zarebski@ukaea.uk>                      #
#   @date   :   last modified 2021-02-19                                      #
#                                                                             #
###############################################################################
import os
import re
from typing import Dict, Any, List, Union
from collections import OrderedDict

PROCESS_START = 'Power Reactor Optimisation Code'   # Start of output statement
PROCESS_END = 'End of PROCESS Output'               # End of output statement


class MFILEParser:
    def __init__(self, mfile_input: str = None) -> None:
        self._input_file = mfile_input
        self._mfile_data: OrderedDict = OrderedDict()
        if self._input_file:
            self.parse(self._input_file)

    def _find_var_val_from_str(self, value_str: str) -> Any:
        """Convert a string variable to float, int etc.

        This function parsers values given within the MFILE removing other
        unneeded information such as the specific typing in PROCESS

        Parameters
        ----------
        value_str : str
            value as a string

        Returns
        -------
        Any
            the value in the appropriate form
        """
        for type_str in ['OP', 'IP', 'ITV']:
            value_str = value_str.replace(type_str, '')
        try:
            return int(value_str)
        except ValueError:
            pass
        try:
            return float(value_str)
        except ValueError:
            return value_str

    def _fuzzy_line_search(self, lines: List[str], search_str: str,
                           result_n: int = None) -> Union[List[int], int]:
        """Search lines for a string.

        Parameters
        ----------
        lines : List[str]
            list of lines to search through
        search_str : str
            substring to look for in lines
        result_n : int, optional
            specify which result to return, 1st, 2nd etc.

        Returns
        -------
        int, List[int]
            index/indices of line match
        """
        _results = [
            i for i, line in enumerate(lines) if search_str in line
        ]

        if result_n is not None:
            return _results[result_n]
        else:
            return _results

    def _get_values(self, lines: List[str]) -> Dict[str, Any]:
        """Extracts value, description and variable name from MFILE lines.

        Parameters
        ----------
        lines : List[str]
            list of file lines to be parsed

        Returns
        -------
        Dict[str, Any]
            dictionary containing variable name, value and description
        """
        # Compile regex for converting underscores which are spaces into
        # a space character
        _space_re = r'(\_{5,})'
        _var_re = r'(\([a-z0-9\-\+\*\/\_\%\]\[]+\))'

        # Extract lines from the given line set that follow the variable
        # statement convention of 'desc_______(varname)________ value'
        _lines = [
            line for line in lines if re.findall(_var_re, line)
        ]

        # Remove extra symbols such as quotation marks and split line into
        # the three required components using regex
        _lines = [
            [
                i.replace('"', '').replace('`', '').strip()
                for i in re.split(_space_re, line)
                if not(re.findall(_space_re, i)) and i.strip()
            ]
            for line in _lines
        ]

        # If there are not three components in a given line, try splitting
        # the components present by ' ' instead and append 
        for i, line in enumerate(_lines):
            if len(line) != 3:
                _new_line = []
                for element in line:
                    if ' ' in element:
                        _new_line += element.split(' ')
                    else:
                        _new_line += element
                _lines[i] = _new_line[:3]

        # Use an ordered dictionary to match ordering in MFILE
        _vars_dict = OrderedDict()

        # Iterate through the resultant line sets and tidy them a little
        # finally creating a dictionary entry for each with the required
        # information
        for line in _lines:
            _var_key = line[1][1:-1]
            _var_key = _var_key.replace('%', '.')
            if not _var_key:
                continue
            _value = line[2]
            _desc = line[0].replace('_-_', '-').replace('_', ' ')
            _desc = _desc.title().strip()
            _desc = _desc.replace('"', '')
            _desc = re.sub(r'\s{2,}', ' ', _desc)
            if any(i in _var_key for i in ['(', '[']):
                _index = re.findall(r'[\(\[]{1}(\d+)', _var_key)
                if not _index:
                    continue
                _var_key = _var_key.split('(')[0].split('[')[0]
                if _var_key not in _vars_dict:
                    _vars_dict[_var_key] = {
                        'description': _desc,
                        'value': []
                    }
                _vars_dict[_var_key]['value'].append(
                    self._find_var_val_from_str(_value)
                )
            else:
                _vars_dict[_var_key] = {
                    'description': _desc,
                    'value': self._find_var_val_from_str(_value)
                }

        return _vars_dict

    def parse(self, input_mfile: str) -> Dict[str, Any]:
        """Parse an MFILE into a Python Dictionary and dump if output specified.

        Parameters
        ----------
        input_mfile : str
            MFILE to be parsed

        Returns
        -------
        Dict[str, Any]
            dictionary containing the extracted MFILE PROCESS output

        Raises
        ------
        FileNotFoundError
            if the specified MFILE does not exist
        """
        if not os.path.exists(input_mfile):
            raise FileNotFoundError(
                f"Could not load MFILE '{input_mfile}'"
                ", file not found."
            )

        with open(input_mfile) as f:
            _lines = f.readlines()

        # Search for various lines within the MFILE as indexes
        _start_statement_i = self._fuzzy_line_search(_lines, PROCESS_START, -1)
        _end_statement_i = self._fuzzy_line_search(_lines, PROCESS_END, -1)
        _err_warn_i = self._fuzzy_line_search(_lines, 'Errors and Warnings', -1)
        _numerics_i = self._fuzzy_line_search(_lines, '# Numerics #', -1)
        _ffp_i = self._fuzzy_line_search(_lines, '# Final Feasible Point #', -1)
        _pa_i = self._fuzzy_line_search(_lines, '# Plant Availability #', -1)
        _plasma_i = self._fuzzy_line_search(_lines, '# Plasma #', -1)
        _plasmod_i = self._fuzzy_line_search(_lines, '# PLASMOD #', -1)
        _e_confine_i = self._fuzzy_line_search(
            _lines, 'Energy confinement times, and required H-factors :', -1
        )
        _cds_i = self._fuzzy_line_search(_lines, '# Current Drive System #', -1)
        _times_i = self._fuzzy_line_search(_lines, '# Times #', -1)
        _divkal_i = self._fuzzy_line_search(
            _lines, '# Divertor: Kallenbach 1D Model #', -1
        )
        _rad_i = self._fuzzy_line_search(_lines, '# Radial Build #', -1)
        _vert_i = self._fuzzy_line_search(_lines, '# Vertical Build #', -1)
        _divbuild_i = self._fuzzy_line_search(
            _lines, "# Divertor build and plasma position #", -1
        )
        _tfcoil_i = self._fuzzy_line_search(_lines, '# TF coils  #', -1)
        _sctfcoil_i = self._fuzzy_line_search(
            _lines, '# Superconducting TF Coils #', -1
        )
        _cs_i = self._fuzzy_line_search(
            _lines, '# Central Solenoid and PF Coils #', -1
        )
        _vsc_i = self._fuzzy_line_search(_lines, '# Volt Second Consumption #', -1)
        _wf_i = self._fuzzy_line_search(_lines, '# Waveforms #', -1)
        _pfwav_i = self._fuzzy_line_search(
            _lines, '# PF Circuit Waveform Data #', -1
        )
        _sup_i = self._fuzzy_line_search(_lines, '# Support Structure #', -1)
        _pfcind_i = self._fuzzy_line_search(_lines, '# PF Coil Inductances #', -1)
        _prpump_i = self._fuzzy_line_search(
            _lines, '# Pumping for primary coolant (helium) #', -1
        )
        _fwb_i = self._fuzzy_line_search(
            _lines, '# First wall and blanket : CCFE HCPB model #', -1
        )
        _sctfcpc_i = self._fuzzy_line_search(
            _lines, '# Superconducting TF Coil Power Conversion #', -1
        )
        _pfcs_pe_i = self._fuzzy_line_search(
            _lines, '# PF Coils and Central Solenoid: Power and Energy #', -1
        )
        _vacsys_i = self._fuzzy_line_search(
            _lines, '# Vacuum System #', -1
        )
        _pbs_i = self._fuzzy_line_search(
            _lines, '# Plant Buildings System #', -1
        )
        _elec_pr_i = self._fuzzy_line_search(
            _lines, '# Electric Power Requirements #', -1
        )
        _cryo_i = self._fuzzy_line_search(
            _lines, '# Cryogenics #', -1
        )
        _pphb_i = self._fuzzy_line_search(
            _lines, '# Plant Power / Heat Transport Balance #', -1
        )

        # Use the indexes to define subblocks in the MFILE variable output
        _meta_data_block = _lines[_start_statement_i+1:_numerics_i]
        _numerics_block = _lines[_numerics_i+1:_ffp_i]
        _p_availability_block = _lines[_pa_i+1:_plasma_i]
        _plasma_block = _lines[_plasma_i+1:_plasmod_i]
        _plasmod_block = _lines[_plasmod_i+1:_e_confine_i]
        _cds_block = _lines[_cds_i+1:_times_i]
        _times_block = _lines[_times_i+1:_divkal_i]
        _divkal_block = _lines[_divkal_i+1:_rad_i]
        _radbuild_block = _lines[_rad_i+1:_vert_i]
        _vertbuild_block = _lines[_vert_i+1:_divbuild_i]
        _divbuild_block = _lines[_divbuild_i+1:_tfcoil_i]
        _tfcoil_block = _lines[_tfcoil_i+1:_sctfcoil_i]
        _sctf_block = _lines[_sctfcoil_i+1:_cs_i]
        _cs_block = _lines[_cs_i+1:_vsc_i]
        _voltsec_block = _lines[_vsc_i+1:_wf_i]
        _waveform_block = _lines[_wf_i+1:_pfwav_i]
        _pfwave_block = _lines[_pfwav_i+1:_sup_i]
        _support_block = _lines[_sup_i+1:_pfcind_i]
        _pump_block = _lines[_prpump_i+1:_fwb_i]
        _fwb_block = _lines[_fwb_i+1:_sctfcpc_i]
        _sctfcpc_block = _lines[_sctfcpc_i+1:_pfcs_pe_i]
        _pccspe_block = _lines[_pfcs_pe_i+1:_vacsys_i]
        _vacsys_block = _lines[_vacsys_i+1:_pbs_i]
        _pbs_block = _lines[_pbs_i+1:_elec_pr_i]
        _elec_pr_block = _lines[_elec_pr_i+1:_cryo_i]
        _cryo_block = _lines[_cryo_i+1:_pphb_i]
        _pphb_block = _lines[_pphb_i+1:_err_warn_i]
        _err_warn_block = _lines[_err_warn_i+1:_end_statement_i]

        # Process each subblock and add the result to a dictionary
        self._mfile_data['metadata'] = self._get_values(_meta_data_block)
        self._mfile_data['numerics'] = self._get_values(_numerics_block)
        self._mfile_data['plant_availability'] = self._get_values(_p_availability_block)
        self._mfile_data['plasma'] = self._get_values(_plasma_block)
        self._mfile_data['plasmod'] = self._get_values(_plasmod_block)
        self._mfile_data['current_drive_system'] = self._get_values(_cds_block)
        self._mfile_data['times'] = self._get_values(_times_block)
        self._mfile_data['divertor_kallenbach'] = self._get_values(_divkal_block)
        self._mfile_data['radial_build'] = self._get_values(_radbuild_block)
        self._mfile_data['vertical_build'] = self._get_values(_vertbuild_block)
        self._mfile_data['divertor_build'] = self._get_values(_divbuild_block)
        self._mfile_data['tfcoils'] = self._get_values(_tfcoil_block)
        self._mfile_data['sctfcoils'] = self._get_values(_sctf_block)
        self._mfile_data['central_solenoid_pf_coils'] = self._get_values(_cs_block)
        self._mfile_data['volt_second_consumption'] = self._get_values(_voltsec_block)
        self._mfile_data['waveforms'] = self._get_values(_waveform_block)
        self._mfile_data['pf_circuit_waveform'] = self._get_values(_pfwave_block)
        self._mfile_data['support_structure'] = self._get_values(_support_block)
        self._mfile_data['coolant_pumping'] = self._get_values(_pump_block)
        self._mfile_data['first_wall_blanket_hcpb'] = self._get_values(_fwb_block)
        self._mfile_data['sctf_power_conversion'] = self._get_values(_sctfcpc_block)
        self._mfile_data['pfcoil_cs_power_energy'] = self._get_values(_pccspe_block)
        self._mfile_data['vacuum_system'] = self._get_values(_vacsys_block)
        self._mfile_data['plant_buildings_system'] = self._get_values(_pbs_block)
        self._mfile_data[
            'electric_power_requirements'
        ] = self._get_values(_elec_pr_block)
        self._mfile_data['cryogenics'] = self._get_values(_cryo_block)
        self._mfile_data['plant_power_heat_transfer_balance'] = self._get_values(
            _pphb_block
        )
        self._mfile_data['errors_warnings'] = self._get_values(_err_warn_block)

        return self._mfile_data

    def write(self, output_filename: str) -> None:
        """Write output to file.

        Parameters
        ----------
        output_filename : str
            path of output file, file type is determined from the type and can
            be '.toml', '.yml', '.pckl', '.json'
        """
        _suffix = os.path.splitext(output_filename)[1].lower()

        if _suffix == '.toml':
            try:
                import tomlkit
            except ImportError:
                import toml
                print(
                    "WARNING: Python module 'tomlkit' not found, "
                    "file comments will not be written to created TOML file"
                )
                toml.dump(self._mfile_data, open(_args.output_file, 'w'))
                exit(0)
            _doc = tomlkit.document()
            _doc.add(tomlkit.comment('PROCESS Run Output'))
            for group_name, data in self._mfile_data.items():
                _new_dict = {}
                for var_name, var_data in data.items():
                    _new_dict[var_name] = var_data['value']
                _header = group_name.replace('_', ' ').title()
                _ls = int((75-len(_header))/2)
                _rs = 75-len(_header)-_ls
                _header = _ls*'-'+' '+_header+' '+_rs*'-'
                _doc.add(tomlkit.comment(_header))
                _doc.add(group_name, _new_dict)
                _doc.add(tomlkit.nl())
                _doc.add(tomlkit.nl())

            for group_name, data in self._mfile_data.items():
                for var_name, var_data in data.items():
                    _doc[group_name][var_name].comment(
                        self._mfile_data[group_name][var_name]['description']
                    )

            with open(_args.output_file, 'w') as f:
                f.write(tomlkit.dumps(_doc))
        elif _suffix == '.json':
            import json
            json.dump(self._mfile_data, open(_args.output_file, 'w'))
        elif _suffix in ['.yml', '.yaml']:
            import yaml
            yaml.dump(self._mfile_data, open(_args.output_file, 'w'))
        elif _suffix == '.pckl':
            import pickle
            pickle.dump(self._mfile_data, open(_args.output_file, 'wb'))
        else:
            print(
                f"Unrecognised file format '{_suffix}'"
            )


if __name__ in "__main__":
    import argparse

    _parser = argparse.ArgumentParser('MFILE2Dict')
    _parser.add_argument('input_mfile', help='Input MFILE to parse')
    _parser.add_argument(
        'output_file',
        help='Output filename, can be JSON, YAML, Pickle or TOML'
    )

    _args = _parser.parse_args()

    _mfile_parser = MFILEParser(_args.input_mfile)
    _mfile_parser.write(_args.output_file)
