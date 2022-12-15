from process.uncertainties import hdf_to_csv
##from griff_work import hdf_to_csv

plot = hdf_to_csv.main([
"-i", 
"/home/griff/process/griff_work/HPC/monte_carlo/monte_carlo_output/uncertainties_data.h5",
"-v",
"/home/griff/process/griff_work/HDF5_to_csv_vars.json",
])

#python mfile_to_csv.py -f /home/griff/process/griff_work/monte_carlo/ref_MFILE.DAT -v /home/griff/process/process/io/mfile_to_csv_vars.json 