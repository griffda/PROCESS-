"""Running this script will run baseline 2018 and print its runtime [s] to stdout."""

import subprocess
import time
from pathlib import Path


def baseline2018_runtime():
    """Runs baseline 2018 and returns the runtime in seconds"""
    script_dir = Path(__file__).resolve().parent
    baseline_2018_indat = script_dir / "../tracking/baseline_2018/baseline_2018_IN.DAT"

    start = time.time()

    subprocess.run(f"process -i {str(baseline_2018_indat.resolve())}", shell=True)

    return time.time() - start


if __name__ == "__main__":
    print(f"Runtime of baseline 2018: {baseline2018_runtime()}")
