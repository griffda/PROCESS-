"""Run Process by calling into the Fortran.

This uses a Python module called fortran.py, which uses an extension module 
called "_fortran.cpython... .so", which are both generated from 
process_module.f90. The process_module module contains the code to actually run 
Process.

This file, process.py, is now analogous to process.f90, which contains the 
Fortran "program" statement. This Python module effectively acts as the Fortran
"program".
"""
import fortran

# Run Process using the highest-level module
fortran.process_module.process_subroutine()