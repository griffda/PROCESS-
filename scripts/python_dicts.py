"""
As the Python conversion continues, we want to maintain backwards compatibility with our utility tools.
These tools rely on the so-called 'dictionaries' to provide some of the meta data used in their function.

This extension to the existing `create_dicts.py` provides a means to allow data structures in Python to
still be included in the 'dictionaries'.
"""

import inspect
from typing import Any, List, NamedTuple

from process.main import Models
from process.variables import AnnotatedVariable


class AnnotatedVariableData(NamedTuple):
    """Holds data about a variable being processed before insertion into the dictionaries.

    :param parent: the name of the class this variable was declared within
    :type parent: str

    :param name: the name of the variable
    :type name: str

    :param obj: a pointer to the actual variable
    :type obj: _Variable (hidden within AnnotatedVariable)

    :param docstring: provide a docstring, description, to the variable.
    :type docstring: str

    :param units: the units of this variable
    :type units: str
    """

    parent: str
    name: str
    obj: Any
    docstring: str
    units: str


# _Variable is a hidden class so cannot be used
# to check type. This hackery is needed
# to provde the type checking.
_Variable_module_name = AnnotatedVariable(object).__class__.__module__
_Variable_name = AnnotatedVariable(object).__class__.__name__


def get_non_dunder_class_members(_object: object) -> dict:
    """Given a class, this function returns all members that are not dunder methods/variables.
    It should be noted that member is class variables and class methods.

    :param _object: the object (instantiated class) to be interogated
    :type _object: object

    :return non_dunder_members: a dictionary mapping a members name to the underlying member
    :type non_dunder_members: dict
    """
    non_dunder_members = {}
    for name, value in inspect.getmembers(_object):
        if name[0:2] != "__":
            non_dunder_members[name] = value

    return non_dunder_members


def get_annotated_variables(parent_name: str, _object) -> List[AnnotatedVariableData]:
    """Given a physics and engineering module, this function extracts and returns all of the
    annotated variables.

    :param parent_name: the name of the parent (physics and engineering) module
    :type parent_name: str

    :param _object: the underlying (annotated) class variable
    :type _object: _Variable (hidden within DocableVariable)

    :return annotated_variables: array of AnnotatedVariableData
    :type annotated_variables: List[AnnotatedVariableData]
    """
    annotated_variables = []
    for name, obj in inspect.getmembers(_object):
        # hackery beget hackery
        if (
            obj.__class__.__module__ == _Variable_module_name
            and obj.__class__.__name__ == _Variable_name
        ):
            annotated_variables.append(
                AnnotatedVariableData(
                    parent=parent_name,
                    name=name,
                    obj=obj,
                    docstring=obj.__doc__,
                    units=obj.__units__,
                )
            )

    return annotated_variables


def get_python_variables() -> List[AnnotatedVariableData]:
    """Drives the discovery of AnnotatedVariables, returning them back to be
    parsed into the dicts by `create_dicts.py`.
    """
    models_obj = Models()
    # non-dunder methods of Models are all the models
    # that we want to get the variables from
    models_dict = get_non_dunder_class_members(models_obj)

    variables = []
    for name, model in models_dict.items():
        for annotated_variable in get_annotated_variables(name, model):
            variables.append(annotated_variable)

    return variables
