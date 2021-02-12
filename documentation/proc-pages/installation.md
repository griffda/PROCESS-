# Installation
## Supported environments
PROCESS is supported on Ubuntu 20, Mac and Windows 10 (via Windows Subsystem for Linux). It is not supported natively in Windows (through MinGW for example). It is not currently supported on the Freia or Heimdal clusters.

## Ubuntu and Windows (using Windows Subsystem for Linux)
*It is highly recommended users create a Python virtual environment in order to use the PROCESS Python package, as this ensures that installations of required package versions don't affect other packages that require different versions in your environment. It isn't necessary, however.*
*Please note due to bugs in f90wrap, Python3.9 is not yet supported. Running with Python3.9 will cause syntax errors to be raised when running f2py on the f90wrap outputs.*

This installation is known to work on Ubuntu 20 (under Windows Subsystem for Linux or not). For Mac, see below.

If you have previously modified your `$PYTHONPATH` environment variable to include `process/utilities`, perhaps in your `~/.bashrc` file, then please remove this modification. Re-start your terminal for the changes to take effect, and check this is not on your `$PYTHONPATH` with:
```bash
echo $PYTHONPATH
```

This modification is not required to run Process now, and it may result in Ford failing during the build process otherwise.

Firstly, install cmake, gfortran and pip, lcov:
```bash
sudo apt update
sudo apt install cmake
sudo apt install gfortran
sudo apt install python3-pip
sudo apt install lcov
```

Clone the PROCESS repository from Gitlab, and navigate into the resulting directory:
```bash
git clone git@git.ccfe.ac.uk:process/process.git
cd process
```

Create and activate a virtual environment if you'd like to use one:
```bash
python3 -m venv env
source env/bin/activate
```

Then install numpy; this is explicitly required due to it being a pre-requisite in f90wrap's setup.py file.
```bash
pip3 install numpy
```

Now we need to compile the Fortran and create the Python interface. This is done using cmake to configure the build and then make to build it. Finally start the build process:
```bash
cmake -S . -B build
cmake --build build
```

The build step may take some time when run for the first time (~3 mins) as the Fortran code is compiled and then wrapped using `f90wrap` and `f2py` to create the Python libraries. Once this is completed the Process Python package is then automatically installed using `pip` and should be ready to use on Linux. If the installation was successful the command `process` should be available on the command line.

To rebuild, for example after making a change to the Fortran source, run `cmake --build build` again.

## macOS Installation
For macOS users it is highly recommended you install GCC using Homebrew. This version of PROCESS searches for the `libgfortran` library by using GCC and the build has been proven to work using this compiler. By default, mac will build with Apple Clang which is the default binary when running the `gcc` command. You will need to either specify the compiler when running CMake:

```
cmake -H. -Bbuild -DCMAKE_C_COMPILER=/path/to/gcc/binary
```

or set your GCC installation to be at the front of the `PATH` variable, e.g. if your installation is in `/usr/local/bin`:

```
export PATH=/usr/local/bin:$PATH
```

Furthermore an additional step is required post-build in which the shared object produced by `f2py` needs to be editted to change the library links using `image_name_tool`, a script has been provided to automate this process. Upon completion of the PROCESS installation Mac users should run:

```bash
bash scripts/macos_update_shared_objects.sh
```

## Testing
As a first basic test that the setup has been successful try importing the package from outside of the repository folder in a Python interactive interpreter:
```bash
cd
python3
```

... to move to your home directory and start the Python interpreter. Then:
```python
import process
process
```

... should output:
```bash
<module 'process' from '/home/jmaddock/process/process/__init__.py'>
```

This indicates that the Process Python package has been installed.

For thorough testing, the test suite needs to be run. The included tests are run using PyTest, and requirements for the tests are installed by running:
```BASH
pip install .[test]
```

PyTest can then be run on the tests folder:
```BASH
pytest tests
```

If everything passes, this indicates a successful installation.

### Prepare Release (not required)
It is possible to build a standalone module which can be distributed without the need for the source code. This exists as a pippable "wheels" module which is build by running:

```BASH
mkdir process_dist
python setup.py bdist_wheel -d process_dist
```

this will produce a `.whl` file within the folder `process_dist` which can be installed from by running:

```BASH
pip install --find-links=process_dist/ process
```

Alternatively the module can be packaged into an archive in the same location by running:

```BASH
python setup.py sdist
```