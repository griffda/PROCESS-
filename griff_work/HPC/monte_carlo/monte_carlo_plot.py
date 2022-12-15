from process.uncertainties import hdf_to_scatter_plot

plot = hdf_to_scatter_plot.main(
["-i", 
"/rds/general/user/tgg120/home/process/griff_work/HPC/monte_carlo/monte_carlo_output/uncertainties_data.h5",
 "-v", 
 "powfmw", "fwoutlet", "coe","precircmw", 
 "-sf", 
 "pdf", 
 "-p"]
 )