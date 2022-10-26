from process.uncertainties import evaluate_uncertainties

sob = evaluate_uncertainties.main(["-f", "/home/griff/process/process/uncertainties/config_evaluate_uncertainties.json", "-m", "morris_method"])
sob()

