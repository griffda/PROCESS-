from process.main import main
from pathlib import Path
file = Path("tests/regression/scenarios/Menard_HTS-PP/IN.DAT").resolve()
main(["-i", str(file)])