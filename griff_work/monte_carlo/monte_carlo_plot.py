from process.uncertainties import hdf_to_scatter_plot

plot = hdf_to_scatter_plot.main(["-i", "/home/griff/process/griff_work/monte_carlo/monte_carlo_output/uncertainties_data.h5", "-v", "beta", "-sf", "pdf", "-p"])