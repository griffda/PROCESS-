from process.main import main
from pathlib import Path
file = Path("/home/griff/process/process/uncertainties/config_evaluate_uncertainties.json").resolve()
main(["-v", "-c", str(file)])