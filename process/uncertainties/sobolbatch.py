from process.uncertainties import evaluate_uncertainties

#args = ["-f", "home/griff/process/process/uncertainties/config_evaluate_uncertainties.json", "-m", "run_sobol_method"]

sob = evaluate_uncertainties.main(["-f", "config_evaluate_uncertainties.json", "-m", "sobol_method"])
sob()