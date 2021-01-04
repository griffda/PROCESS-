"""Script to modify an import path in the f90wrap-generated interface module.

In the f90wrap-generated interface module (fortran.py), it performs an import
"import _fortran" which depends on the input script being in the same 
directory as the _fortran module in order for it to be found on the module
search path. It is preferable to have the _fortran module as part of a package
which is called by a script outside of that package. Is is also preferable to
use absolute imports throughout that package.

As there's no control over this f90wrap-generated file's import statements, this
script replaces the "import _fortran" statement with "from process import 
_fortran", which allows the _fortran module to be found when running the process
package from an external script.

Using absolute imports consistently in a package in this way is preferable to 
relying on more brittle PYTHONPATH modifications, which is the alternative.

This script is called from CMakeLists.txt, and therefore forms part of the 
build system.
"""
import argparse
from pathlib import Path
import re

# Parse the fortran interface path positional argument from cmake
# This is the path to the f90wrap-generated file (e.g. fortran.py)
parser = argparse.ArgumentParser()
parser.add_argument('fortran_interface_path_str', type=str)
args = parser.parse_args()

# Create Path object for the file and check it exists
fortran_interface_path = Path(args.fortran_interface_path_str)
if fortran_interface_path.exists() is False:
    raise FileNotFoundError("Can't locate fortran interface python module.")

# Find and replace the import statement
# Read in the fortran.py source file
with open(fortran_interface_path, 'r') as fortran_interface:
    source = fortran_interface.read()

# Regex substitution: prepend "from process " to the "import _fortran" statement
# Only perform one sub, and multiline allows regex to match at start (^) of each
# line, rather than the entire string
source = re.sub(r"^import _fortran", "from process import _fortran", source, 1,
    re.MULTILINE)

# Write out the modified source file
with open(fortran_interface_path, 'w') as fortran_interface:
    fortran_interface.write(source)
