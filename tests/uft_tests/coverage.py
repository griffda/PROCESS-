###############################################################################
#                                                                             #
#                      PROCESS UnitFort Test Coverage                         #
#                                                                             #
#   The following module contains a class to determine the fraction of        #
#   methods within PROCESS which had a UnitFort test associated with them.    #
#   Currently this method searches for the term 'END SUBROUTINE/FUNCTION' to  #
#   count how many subroutines/functions exist. The disadvantage of this      #
#   approach is that it counts nested functions/subroutines (those defined    #
#   within other methods) which are not covered by UFT. This combined with    #
#   the fact that many subroutines do not return values, and that f90wrap     #
#   does not yet convert all methods means 100% coverage is impossible.       #
#                                                                             #
#   @author :   K. Zarebski <kristian.zarebski@ukaea.uk>                      #
#   @date   :   last modified 2020-12-04                                      #
#                                                                             #
# ############################################################################# 

from typing import Dict, List, Any
import glob
import logging
from tabulate import tabulate
import os
from jinja2 import Template
import re
import pathlib

logging.basicConfig()

TEST_ROOT = os.path.dirname(__file__)
SRC_ROOT = os.path.join(
    pathlib.Path(TEST_ROOT).parent.parent,
    'source',
    'fortran'
)


class PROCESSUFTCoverage(object):
    '''
    PROCESS UnitFort Coverage Reporter
    ----------------------------------

    Calculates the fraction of subroutines/functions within PROCESS which
    have an associated UnitFort test.
    '''
    def __init__(self, test_folder: str = TEST_ROOT,
                 src_folder: str = SRC_ROOT) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)
        self._logger.setLevel(logging.INFO)
        self._tests = self.get_current_coverage(test_folder)
        self._all_methods = self.get_all_methods(src_folder)
        self._report = self.get_coverage_report()

    def get_current_coverage(self, test_loc: str) -> Dict[str, List[str]]:
        '''
        Use glob search to find a complete list of all the test files made
        containing PyTests constructed from UFT data. Iterate through these
        files reading in the created test names which should match the
        methods within PROCESS.

        Arguments
        ---------

        test_loc        location of PyTest test scripts


        Returns
        -------

        A dictionary of the form:

            - dict
               |_ module_1
               |     |_ tested_method_name
               |     |
               ..    ..

        '''
        _test_files = glob.glob(os.path.join(test_loc, 'test_*.py'))

        _test_dict: Dict[str, List[Any]] = {}

        # Iterate through list of test files obtaining list of tested methods
        # categorised by their parent module
        for f in _test_files:
            _file_str = open(f).readlines()
            _mod_name = f.split('/')[-1].split('.')[0]
            _mod_name = _mod_name.replace('test_', '').replace('_module', '')
            _test_dict[_mod_name] = []

            for line in _file_str:
                if 'def ' in line:
                    _name = re.findall(r'def (.+)\(', line, re.IGNORECASE)
                    if not _name:
                        continue
                    _name = _name[0].strip().replace('test_', '')
                    _test_dict[_mod_name].append(_name)
        return _test_dict

    def get_all_methods(self, source_loc: str) -> Dict[str, List[str]]:
        '''
        Retrieve the complete list of functions and subroutines from PROCESS
        by iterating through all FORTRAN sources and finding the term:
        'END SUBROUTINE ...' or 'END FUNCTION ...' and extracting the names of
        the methods. Categorise by parent module.

        Arguments
        ---------

        source_loc      address of FORTRAN source files


        Returns
        -------

        A dictionary of the form:

            - dict
               |_ module_1
               |     |_ method_name
               |     |
               ..    ..

        '''
        _all_srcs = glob.glob(os.path.join(source_loc, '*.f90'))
        _module = 'None'

        # Module, Subroutine and Function search Regex
        _mod_find = re.compile(r'module ([a-z0-9\_]+)', re.IGNORECASE)
        _sub_find = re.compile(r'end subroutine ([a-z0-9\_]+)', re.IGNORECASE)
        _func_find = re.compile(r'end function ([a-z0-9\_]+)', re.IGNORECASE)

        _methods_dict: Dict[str, List[str]] = {'None': []}

        # Iterate through all source files obtaining a list of the methods
        # categorised by the parent module
        for f in _all_srcs:
            for line in open(f).readlines():
                if not line.strip() or line.strip()[0] == '!':
                    continue
                if _mod_find.findall(line):
                    _module = _mod_find.findall(line)[0].strip()
                    if _module not in _methods_dict:
                        _methods_dict[_module] = []
                elif _sub_find.findall(line):
                    _sub = _sub_find.findall(line)[0].strip().lower()
                    _methods_dict[_module].append(_sub)
                    _module, _methods_dict[_module]
                elif _func_find.findall(line):
                    _func = _func_find.findall(line)[0].strip().lower()
                    _methods_dict[_module].append(_func)

        # If there are no methods within a given module remove the module
        # listing from the methods dictionary
        _to_del = [k for k in _methods_dict if not _methods_dict[k]]

        for d in _to_del:
            del _methods_dict[d]

        return _methods_dict

    def get_coverage_report(self) -> Dict[str, List]:
        '''
        Assemble the coverage data into a single dictionary containing both
        the value as a percentage and as a fraction string

        Returns
        -------

        A dictionary with the percentage and fractional coverage per module

        '''
        _denom_dict = {t: len(self._all_methods[t]) for t in self._all_methods}
        _num_dict = {t: 0 for t in self._all_methods}

        for test in self._tests:
            for method in self._tests[test]:
                for mod in self._all_methods:
                    if method in self._all_methods[mod]:
                        _num_dict[mod] += 1

        _denom = sum(_denom_dict[i] for i in _denom_dict)
        _num = sum(_num_dict[i] for i in _num_dict)

        _perc_dict = {}

        for t in _denom_dict:
            _perc_dict[t] = [
                100.*_num_dict[t]/_denom_dict[t],
                f'{_num_dict[t]}/{_denom_dict[t]}'
            ]

        _perc_dict['total'] = [
            100.*_num/_denom,
            f'{_num}/{_denom}'
        ]

        return _perc_dict

    def _gen_html_table(self):
        '''
        Generate a HTML table from the coverage data
        '''
        _table_struct = Template('''
    <table class="table table-striped">
        <tr>
            <th>Module</th>
            <th colspan="2"><center>Method Coverage</center></th>
        </tr>
{{entries}}
    </table>
        ''')

        _rows = ''

        _table = []

        for module in self._report:
            _row_template = Template('''
        <tr>
            <td>{{name}}</td>
            <td>{{perc}}</td>
            <td>{{value}}</td>
        </tr>
            ''')

            _val = self._report[module][1]
            _perc = self._report[module][0]
            _table.append([module, f'{_perc:.2f}'])

            if _perc < 50:
                _perc = f'<div class="alert alert-danger">{_perc:.1f}</div>'
                _value = f'<div class="alert alert-danger">{_val}</div>'
            elif _perc < 80:
                _perc = f'<div class="alert alert-warning">{_perc:.1f}</div>'
                _value = f'<div class="alert alert-warning">{_val}</div>'
            else:
                _perc = f'<div class="alert alert-success">{_perc:.1f}</div>'
                _value = f'<div class="alert alert-success">{_val}</div>'

            _name = module

            if _name == 'total':
                _tot_perc = self._report[module][0]

            _row = _row_template.render(
                value=_value,
                perc=_perc,
                name=_name
            )

            _rows += _row

        return _table_struct.render(entries=_rows), _tot_perc, _table

    def create_html_page(self):
        '''
        Generate a HTML page to display the PROCESS UnitFort coverage
        '''
        _page_template = Template('''
<!doctype html>

<html lang="en">
<head>
  <meta charset="utf-8">

  <!-- Required meta tags -->
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

  <!-- Bootstrap CSS -->
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css" integrity="sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2" crossorigin="anonymous">

  <title>UFT PROCESS Coverage</title>
  <meta name="description" content="{{desc}}">

  <link rel="stylesheet" href="css/styles.css?v=1.0">

</head>

<body>
<div class="page-header">
    <center><h1>PROCESS UnitFort Test Coverage</h1></center> 
</div>
<div class="container">
<table class="table table-bordered">
    <tr>
        <th><b>Total coverage</b></th>
        <th>{{summary}}</th>
    </tr>
</table>
</div>
<div class="container">
{{table}}
</div>
  <script src="js/scripts.js"></script>
</body>
</html>
        ''')

        _table, _tot_perc, _stdout_table = self._gen_html_table()

        _page = _page_template.render(
            desc='Percentage coverage of FORTRAN functions and '
            'subroutines within the PROCESS library from PyTest UFT tests',
            table=_table,
            summary=f'<div class="alert alert-danger">{_tot_perc:.1f}&#37;</div>'
        )

        with open('uft_coverage.html', 'w') as f:
            f.write(_page)

        print(
            f'''
============================================================

              PROCESS UnitFort Test Coverage

    Total UnitFort Test Coverage for PROCESS: {_tot_perc:.1f}%

{tabulate(_stdout_table, headers=['Module', 'Coverage/%'], tablefmt='pretty')}

============================================================
        ''')


if __name__ in "__main__":
    PROCESSUFTCoverage().create_html_page()
