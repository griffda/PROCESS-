# Build and run PROCESS simply

# Build
cmake -H. -Bbuild
cmake --build build

# Run Process
./bin/process.exe

# Create Python dictionaries
cmake --build build --target dicts

# Add utilities folder to Python path for importing modules
export PYTHONPATH=$PYTHONPATH:~/process/utilities/

# Display summary output in matplotlib and create SUMMARY.pdf
python utilities/plot_proc.py --show
