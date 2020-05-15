# Rough script to compile and wrap what's required to run Process Fortran
# from Python

# Clear out old .o, .mod and f90wrap_* files
cd ~/process
rm source/fortran/*.mod source/fortran/*.o source/fortran/f90wrap_*

# Run cmake
rm build
cmake -H. -Bbuild
cmake --build build

# Copy the .o files
cp ./build/CMakeFiles/PROCESS_calc_engine.dir/source/fortran/*.o source/fortran
cp ./build/CMakeFiles/PROCESS_calc_engine.dir/lib/REFPROP/*.o source/fortran
cp ./build/CMakeFiles/PROCESS_calc_engine.dir/lib/PLASMOD/*.o source/fortran

# Copy the .mod files
cp build/*.mod source/fortran
cp lib/PLASMOD/*.o source/fortran

# Now run f90wrap and f2py
cd source/fortran
f90wrap -k kind_map process_module.f90 global_variables.f90 -m fortran
f2py -c f90wrap_* -m _fortran *.o