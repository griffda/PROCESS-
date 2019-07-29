"""

    Script to check line length of repository files

    Files include:
        - *.f90
        - *.py
        - *.tex

    James Morris
    UKAEA
    26.09.19
        
"""

import os
import sys

# Get repository root directory
import  pathlib
import time
timeout = time.time() + 10   # 10 seconds
found_root = False
back = ""
while not found_root:
    if time.time() > timeout:
        print("Can't find repository root. Make sure utility is being run "
              "inside a PROCESS repository clone")
        break
    else:
        my_file = pathlib.Path(back + ".gitignore")
        if my_file.is_file():
            found_root = True
            if back == "":
                REPO_ROOT = "."
            else:
                REPO_ROOT = back
        back += "../"


if __name__ == "__main__":

    # Intro message

    intro = """

    Checking line length standard (<100) lines.
    
    For the following locations

        - /source/fortran/*.f90
        - /source/utilities/*.py
        - /source/documentation/*.tex

    J. Morris
    UKAEA
    26.07.19

    """

    print(intro)

    # Set status
    STATUS = True
    LINE_COUNT = 0
    FILE_COUNT = 0

    # Check main FORTRAN
    fort_path = REPO_ROOT+"/source/fortran/"
    for filename in os.listdir(fort_path):
        if ".f90" in filename:
            FORT_STATUS = True
            print("")
            print(filename)
            file_lines = open(fort_path + filename, "r").readlines()
            counter = 0
            for line in file_lines:
                counter += 1
                if len(line) > 100:
                    print("|-- {0} :: line :: {1} :: length={2}".
                          format(filename, counter, len(line)))
                    STATUS = False
                    LINE_COUNT += 1
                    if FORT_STATUS:
                        FILE_COUNT += 1
                    FORT_STATUS = False

    # Check Python
    py_path = REPO_ROOT+"/utilities/"
    for filename in os.listdir(py_path):
        if ".py" in filename:
            PY_STATUS = True
            print("")
            print(filename)
            file_lines = open(py_path + filename, "r").readlines()
            counter = 0
            for line in file_lines:
                counter += 1
                if len(line) > 100:
                    print("|-- {0} :: line :: {1} :: length={2}".
                          format(filename, counter, len(line)))
                    STATUS = False
                    LINE_COUNT += 1
                    if PY_STATUS:
                        FILE_COUNT += 1
                    PY_STATUS = False

    # # Check Latex
    tex_path = REPO_ROOT+"/documentation/"
    for filename in os.listdir(tex_path):
        if ".tex" in filename:
            TEX_STATUS = True
            print("")
            print(filename)
            file_lines = open(tex_path + filename, "r").readlines()
            counter = 0
            for line in file_lines:
                counter += 1
                if len(line) > 100:
                    print("|-- {0} :: line :: {1} :: length={2}".
                          format(filename, counter, len(line)))
                    STATUS = False
                    LINE_COUNT += 1
                    if TEX_STATUS:
                        FILE_COUNT += 1
                    TEX_STATUS = False

    # if STATUS:
    #     sys.exit(0)
    # else:
    #     sys.exit("\nERROR: Line length exceeded {0} times in {1} files".
    #              format(LINE_COUNT, FILE_COUNT))
