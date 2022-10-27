from process.main import main
from pathlib import Path
file = Path("/home/griff/process/tests/regression/scenarios/Menard_HTS-PP/IN.DAT").resolve()
main(["-i", str(file)])