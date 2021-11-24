import inspect
from typing import Any, List, NamedTuple

from process.main import Models
from process.variables import DocableVariable

class AnnotatedVariable(NamedTuple):
    parent: str
    name: str
    obj: Any
    docstring: str
    units: str

_Variable_module_name = DocableVariable(object).__class__.__module__
_Variable_name = DocableVariable(object).__class__.__name__


def get_non_dunder_class_members(obj):
    non_dunder_members = {}
    for name, value in inspect.getmembers(obj):
        if name[0:2] == "__":
            continue
            
        non_dunder_members[name] = value
    
    return non_dunder_members


def get_annotated_variables(parent_name, obj) -> List[AnnotatedVariable]:
    annotated_variables = []
    for name, obj in inspect.getmembers(obj):
        # hackery beget hackery
        if obj.__class__.__module__ == _Variable_module_name and obj.__class__.__name__ == _Variable_name:
            annotated_variables.append(AnnotatedVariable(parent=parent_name, name=name, obj=obj, docstring=obj.__doc__, units=obj.__units__))
    
    return annotated_variables

def get_python_variables() -> List[AnnotatedVariable]:
    models_obj = Models()
    models_dict = get_non_dunder_class_members(models_obj)

    variables = []
    for name, model in models_dict.items():
        for annotated_variable in get_annotated_variables(name, model):
            variables.append(annotated_variable)

    return variables