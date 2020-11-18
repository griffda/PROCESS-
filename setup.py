from setuptools import setup, find_packages
from setuptools.command.test import test as TestCommand

import sys

setup_kwargs = {
    "name": "process",
    "version": "1.0.17",
    "description": (
        "Power Reactor Optimisation Code for Environmental and Safety Studies"
    ),
    "url": "https://ccfe.ukaea.uk/resources/process/",
    "author": "UKAEA",
    "packages": find_packages(),
    "package_dir": {
        "process": "process"
    },
    "package_data": {
        "process": [
            "_fortran*.so",
            ".lib/libgfortran.so*",
            ".lib/libprocess_lib.so",
            "data/fluids/*",
            "data/h_data/*",
            "data/impuritydata/*",
            "data/lz_non_corona/*",
            "data/lz_non_corona_14_elements/*",
            "utilities/*"
        ],
        "process.io": [
            "python_fortran_dicts.json"
        ]
    },
    "test_suite" : "pytest",
    "install_requires" : ["numpy", "f90wrap"],
    "extras_require" : {'test' : ['pytest', 'scipy']},
    "entry_points": {
        "console_scripts": [
            "process_script=process.process_script_advanced:main",
            "process=process.main:main"
            ]
    }
}

if __name__ == "__main__":
    setup(**setup_kwargs)
