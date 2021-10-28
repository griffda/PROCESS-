"""
Documentation tools cannot document the fortan shared object file
due. However, f2py auto-documents the entire interface. 

This file processes that interface into a dummy-Python format,
_fortran.py with all the same names and docstrings, however
this is a format that can be documented.

fortran (python module)
├─ fortran module (python class)
│  ├─ subroutine (python class method)
│  ├─ function (python class method)
│  ├─ module variable (python class attribute)
"""


import inspect
import os

from process import fortran


def get_classes(ftrn):
    classes = []

    for name, module in inspect.getmembers(ftrn):
        if type(module) == type(fortran.main_module):
            classes.append({
                'name': name,
                'doc': module.__doc__,
                'functions': get_functions(module)
            })
    
    return classes


def get_functions(fortran_module):
    functions = []

    for name, function in inspect.getmembers(fortran_module):
        if type(fortran_module) == type(fortran.main_module.inform):
            functions.append({
                'name': name,
                'doc': function.__doc__
            })
    
    return functions


def create_class_signature(klass):

    name = klass.get('name')
    docstring = f'F2Py generated wrapper around the {name} module' # this doc string is full of rubbish

    functions = []
    variables = []

    for i in klass.get('functions'):
        defn, isfunc = create_function_signature(i)

        if isfunc:
            functions.append(defn)
        else:
            variables.append(defn)

    body = '\n'.join(variables) + '\n'.join(functions)


    string = f'class {name}:\n\t"""{docstring}"""\n\t{body}'

    return string


def create_function_signature(function):
    name = function.get('name')
    if name[0:2] == '__': return '', True
    docstring = function.get('doc')

    if docstring:
        assert all(ord(c) != 0 for c in docstring)
        docstring = docstring.replace('\n','\n\t\t\t').strip('\n\t')
        string = f'\t@classmethod\n\tdef {name}(cls):\n\t\t"""{docstring}"""\n\t\tpass'

        return string, True

    string = f'\n\t{name} = None\n'

    return string, False


if __name__ == '__main__':
    fortran_module = get_classes(fortran)
    fortran_module_definitions = [create_class_signature(i) for i in fortran_module]

    string = '\n\n'.join(fortran_module_definitions)

    current_dir = os.path.dirname(os.path.abspath(__file__))
    target_dir = os.path.join(current_dir, '../process')
    with open (f'{target_dir}/_fortran.py', 'w') as file:
        file.write(string)