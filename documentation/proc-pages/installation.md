# Installation
## Supported environments
PROCESS is supported on Ubuntu 20 and Windows 10 (via Windows Subsystem for Linux). It is not supported natively in Windows (through MinGW for example). It can be run on Mac or in other environments via a Docker container. It is not currently supported on the Freia or Heimdal clusters.

## Ubuntu and Windows (using Windows Subsystem for Linux)
*It is highly recommended users create a Python virtual environment in order to use the PROCESS Python package, as this ensures that installations of required package versions don't affect other packages that require different versions in your environment. It isn't necessary, however.*
*Please note due to bugs in f90wrap, Python3.9 is not yet supported. Running with Python3.9 will cause syntax errors to be raised when running f2py on the f90wrap outputs.*

To install Windows Subsystem for Linux (WSL) follow the 'Manual Installation Steps' [here](https://docs.microsoft.com/en-us/windows/wsl/install-win10). Choose WSL 2 and Ubuntu 20 (if installing from the Microsoft store then Ubuntu 20 is installed by default).

Install Visual Studio Code [here](https://code.visualstudio.com/).

This installation is known to work on Ubuntu 20 (under Windows Subsystem for Linux or not). For Mac, see below.

GFortran version 9 or above is needed for successful installation and execution of PROCESS. Versions below GFortran-9 will be rejected by CMake by default since, while PROCESS might compile successfully with lower GFortran versions, other aspects of PROCESS (tests, coverage, etc.) will fail.

If you have previously modified your `$PYTHONPATH` environment variable to include `process/utilities`, perhaps in your `~/.bashrc` file, then please remove this modification. Re-start your terminal for the changes to take effect, and check this is not on your `$PYTHONPATH` with:
```bash
echo $PYTHONPATH
```

This modification is not required to run Process now, and it may result in Ford failing during the build process otherwise.

Firstly, open the terminal and install cmake, gfortran and pip, lcov:
```bash
sudo apt update
sudo apt install cmake
sudo apt install gfortran
sudo apt install python3-pip
sudo apt install lcov
```

To clone the PROCESS repository from Gitlab you will need to use an SSH key. A guide on how to find if you have an existing SSH key pair or to generate a new SSH key pair can be found on Gitlab [here](https://docs.gitlab.com/ee/ssh/).

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


An error may be encountered here because `ford` is installed in `.local/bin`, which is not on the `PATH` in some environments, so you will need to add `.local/bin` to the `PATH` if this error occurs. You can do this using `nano`:

First use:
```bash
nano ~/.profile
```
 Then use the arrow keys to navigate to the bottom of the `nano` editor and type:
```bash
export PATH=$PATH:/home/yourusername/.local/bin
``` 
where you use your own username in place of `yourusername` above. Then use `Ctrl-X`, then type `Y`, then press enter. Then either close and reopen the terminal, or type:
```bash
source ~/.profile
```
Now when you try the build process again it should be successful.


CMake needs to be at least version `3.13.0`. This is so that the command `cmake -S . -B build` executes correctly. Running this command on an earlier CMake version results in:
```bash
CMake Error: The source directory "/home/process/build" does not exist.
Specify --help for usage, or press the help button on the CMake GUI.
``` 
subsequently making the `build` directory and running the command again results in:
```bash
CMake Error: The source directory "/home/process/build" does not appear to contain CMakeLists.txt.
Specify --help for usage, or press the help button on the CMake GUI.
```

The build step may take some time when run for the first time (~3 mins) as the Fortran code is compiled and then wrapped using `f90wrap` and `f2py` to create the Python libraries. Once this is completed the Process Python package is then automatically installed using `pip` and should be ready to use on Linux. If the installation was successful the command `process` should be available on the command line.

To rebuild, for example after making a change to the Fortran source, run `cmake --build build` again.

## Docker container
Process can be run on Mac or in other environments inside a Docker container. The Process repository, including source and build directories, remain in the host filesystem, but the building and running of Process is performed inside the container. This ensures that Process produces the same results as in other fully-supported environments, such as the CI system. The Ubuntu-based development image used is similar to the one used on the CI system, but it is designed to work immediately with no further installations.

Firstly, [install Docker](https://docs.docker.com/get-docker/). On Mac, this can be accomplished using `homebrew`:
```
brew cask install docker
```

Then login to the Gitlab container registry:
```
docker login git.ccfe.ac.uk:4567
```

Then download the Docker image from the Process Gitlab container registry:
```
docker pull git.ccfe.ac.uk:4567/process/process/dev
```
Running `docker image ls` should show the image in your local Docker image repository. Optionally, you can change the image name to something more manageable:
```
docker tag git.ccfe.ac.uk:4567/process/process/dev process-dev
```
to rename the image to "process-dev" with the "latest" tag: "process:latest".

Now run the container:
```
docker run -it -v ~/process:/root/process process-dev
```
This runs a container which is an instance of the process-dev image. `-it` runs the container in interactive mode (`-i`, allows `stdin`) with a terminal (`-t`, allows bash-like interaction). `-v` specifies the bind mount to use; mount the host `~/process` directory to the `/root/process` directory in the container. This means that the container has read and write access to the `process` project directory on the host filesystem and will stay in sync with it.

Now the container is running, configure, clean and build from the project root directory inside the container:
```
cd ~/process
cmake -S . -B build
cmake --build build --target clean
cmake --build build
```
The clean step is required to remove any build targets or caches from previous host builds to ensure a completely fresh build from inside the container. This is only required when using the container for the first time.

Once Process has built inside the container, it can be tested (as in the following section) by running `pytest`. Once the test suite passes, this confirms that your Docker container runs Process with the same results as the CI system. Process can now be developed and run as before, with the build and running taking place inside the container.

There is also a VS Code extension for Docker containers that may be helpful.

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
To exit the Python interpreter you can use 
```BASH
exit() 
```
and press enter, or use `Ctrl-Z`. 

For thorough testing, the test suite needs to be run. If you have just exited the Python interpreter, you will need to navigate back to the process project directory for this by using:
```BASH
cd process
```
 The included tests are run using PyTest, and requirements for the tests are installed by running the following:
```BASH
pip install .[test]
```

PyTest can then be run on the tests folder:
```BASH
pytest tests
```

If everything passes, this indicates a successful installation. If anything fails, this indicates that your environment produces different results to what is expected. You might consider creating an issue in Gitlab, or trying out the Docker container instead.

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