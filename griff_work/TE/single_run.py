from process.main import main
from pathlib import Path
file = Path("/home/griff/process/griff_work/TE/TE2_IN.DAT").resolve()
main(["-i", str(file)])