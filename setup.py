from setuptools import setup, find_packages
import site
import os
import platform

MODULE_NAME = "process"
_install_loc = os.path.join(site.getsitepackages()[0], MODULE_NAME)
EXTRA_ARGS = []
if platform.system() == "Darwin":
    EXTRA_ARGS = ["-Wl,-rpath," + os.path.join(_install_loc, "lib")]

setup_kwargs = {
    "name": MODULE_NAME,
    "version": "2.4.0",
    "description": (
        "Power Reactor Optimisation Code for Environmental and Safety Studies"
    ),
    "url": "https://ccfe.ukaea.uk/resources/process/",
    "author": "UKAEA",
    "packages": find_packages(),
    "package_dir": {"process": "process"},
    "package_data": {
        "process": [
            "lib/lib*",
            "fortran*.so",
            "data/fluids/*",
            "data/h_data/*",
            "data/lz_non_corona/*",
            "data/lz_non_corona_14_elements/*",
            "utilities/*",
        ],
        "process.io": ["python_fortran_dicts.json"],
        "process.data.impuritydata": ["*"],
    },
    "test_suite": "pytest",
    "install_requires": [
        "numpy>=1.19.0,<1.22.1",
        "importlib-resources ; python_version<'3.7'",
    ],
    "extras_require": {"test": ["pytest", "scipy"]},
    "entry_points": {
        "console_scripts": [
            "process_script=process.process_script_advanced:main",
            "process=process.main:main",
        ]
    },
    "extra_link_args": EXTRA_ARGS,
}

if __name__ == "__main__":
    setup(**setup_kwargs)
