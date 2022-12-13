from process.main import main
from pathlib import Path
file = Path("/rds/general/user/tgg120/home/process/tests/regression/scenarios/Menard_HTS-PP/IN.DAT").resolve()
main(["-i", str(file)])