# Installation
Below are instructions of how to install the applications, code and dependencies such that you can begin working on Process. 

## Supported environments
**Please note, only Python3.8 is supported, Python3.9 and Python3.6 are not.**
PROCESS is natively supported on Ubuntu 20. Other Linux distributions will be able to successfully build and execute PROCESS however may give inconsistent results due to version disparities of dynamically linked libraries.

There are three supported ways to run Process on a non-native Ubuntu 20.04 machine:
1. WSL (Windows Subsytem for Linux- if you are a Widnows user)- go [here](#ubuntu-and-windows-using-windows-subsystem-for-linux-and-installing-visual-studio-code).
1. Docker (for users on privileged machines that are of the wrong OS)- go [here](#docker-container).
1. Singularity (mainly for use on shared resources e.g. Freia)- go [here](#singularity-container).

Using the Windows Subsystem for Linux (on Windows) or a containerised environment is the recommended way to build, test, and run PROCESS on any OS other than Ubuntu 20. Documentation on Ubuntu for WSL can be found [here](https://ubuntu.com/wsl) for a deeper description of how they function together and the advantages of use.

## Ubuntu and Windows (using Windows Subsystem for Linux) and Installing Visual Studio Code
*This section is for users on a Windows system, if you are a MacOS or priviledged machine user please go [here](#docker-container), or if you are a using a shared resource, please go [here](#singularity-container).*

To install Windows Subsystem for Linux (WSL) follow the 'Manual Installation Steps' [here](https://docs.microsoft.com/en-us/windows/wsl/install-win10). Choose WSL 2 and Ubuntu 20 (if installing from the Microsoft store then Ubuntu 20 is installed by default). 


The following command is used to install WSL:
```bash
wsl --install
```

However, you need admin privilages to perfom this command. Please ask support to grant you these rights temporarily so you can successsfully install WSL. This can be done via a Marval ticket or email to support if you are a new starter and do not have Nucleus access yet.

Next, you will need to install a source code editor so you are able to work with the Process code. Visual Studio Code, or VSCode for short, is a great choice. It is a lightweight but powerful source code editor which runs on your deskotp and is available for Windows, MacOS and Linux. It has a vast extension package allowing ease of use with a range of languages. More information on VSCode can be found [here](https://code.visualstudio.com/docs).

Install Visual Studio Code [here](https://code.visualstudio.com/).

To connect WSL to VS Code, the following extension should be installed in VS Code upon opening: `Remote - WSL`. This allows for opening of any folder in the Windows Subsystem for Linux. This is performed by using `Ctrl+Shift+X` on VS Code, searching for `Remote - WSL` and then installing.


GFortran version 9 or above is needed for successful installation and execution of PROCESS. Versions below GFortran-9 will be rejected by CMake by default since, while PROCESS might compile successfully with lower GFortran versions, other aspects of PROCESS (tests, coverage, etc.) will fail.

If you have previously modified your `$PYTHONPATH` environment variable to include `process/utilities`, perhaps in your `~/.bashrc` file, then please remove this modification. Re-start your terminal for the changes to take effect, and check this is not on your `$PYTHONPATH` with:
```bash
echo $PYTHONPATH
```

This modification is not required to run PROCESS now, and it may result in Ford failing during the build process otherwise.

Firstly, open the terminal and install cmake, gfortran, pip etc.:
```bash
sudo apt update
sudo apt install cmake
sudo apt install gfortran
sudo apt install python3-pip
sudo apt install lcov
sudo apt install poppler-utils
```
Next, the code will need to be downloaded so you can work with it. The Process code is stored in a GitLab repository and as such needs to be 'cloned' - i.e bought to your VSCode window from GitLab. 

To clone the PROCESS repository from Gitlab you will need to use an SSH key. A guide on how to find if you have an existing SSH key pair or to generate a new SSH key pair can be found on Gitlab [here](https://docs.gitlab.com/ee/ssh/).

Clone the PROCESS repository from Gitlab, and navigate into the resulting directory:
```bash
git clone git@git.ccfe.ac.uk:process/process.git
cd process
```

Create and activate a virtual environment if you'd like to use one:

*It is highly recommended users create a Python virtual environment in order to use the PROCESS Python package, as this ensures that installations of required package versions don't affect other packages that require different versions in your environment. It isn't necessary, however.*
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

If you plan on developing code for Process, please see the `pre-commit` documentation for installing this tool required by developers: [development/pre-commit](http://process.gitpages.ccfe.ac.uk/process/development/pre-commit/)



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

Now, skip to [testing](#testing).

## Docker container 
*This section is for users of a priviledged machine of the wrong OS e.g MacOS. If you are a using a shared resource e.g. Freia, please go [here](#singularity-container).*

### **If you are using a Windows system with WSL and have followed the above steps then this next section is not necessary and you may skip to testing your installation [here](#testing).**

Process can be run on Mac or in other environments inside a Docker container. The Process repository, including source and build directories, remain in the host filesystem, but the building and running of Process is performed inside the container. This ensures that Process produces the same results as in other fully-supported environments, such as the CI system. The Ubuntu-based development image used is similar to the one used on the CI system, but it is designed to work immediately with no further installations.

*Please note due to recent changes in the Docker Desktop ToS, you will require either a Docker Desktop license to run on Mac, or you will require a Linux environment by other means, such as a virtual machine.

Firstly, [install Docker](https://docs.docker.com/get-docker/). On Mac, this can be accomplished using `homebrew`:
```
brew --cask install docker
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
This runs a container which is an instance of the process-dev image. `-it` runs the container in interactive mode (`-i`, allows `stdin`) with a terminal (`-t`, allows bash-like interaction). `-v` specifies the bind mount to use; mount the host `~/process` directory to the `/root/process` directory in the container. This means that the container has read and write access to the `process` project directory on the host filesystem and will stay in sync with it. Please be aware that changes made in a Docker container outside of mounted folders will not be saved on exiting the container.

Now the container is running, configure, clean and build from the project root directory inside the container:
```
cd ~/process
cmake -S . -B build
cmake --build build --clean-first
```
The clean step is required to remove any build targets or caches from previous host builds to ensure a completely fresh build from inside the container. This is only required when using the container for the first time.

Once Process has built inside the container, it can be tested (as in the following section) by running `pytest`. Once the test suite passes, this confirms that your Docker container runs Process with the same results as the CI system. Process can now be developed and run as before, with the build and running taking place inside the container.

There is also a VS Code extension for Docker containers that may be helpful.

Now, skip to [testing](#testing).

## Singularity container
Singularity is a container environment similar to Docker. This means a user can run PROCESS with all required dependencies installed. Singularity, however, is designed to work with user-level permissions and, as such, is supported by many shared resource administrators (unlike Docker, which poses a security risk).

Singularity can convert OCI compliant containers into the Singularity Image Format (SIF) to run the Docker container above. Download, and convert the Docker container by running: `singularity pull --docker-login process.sif docker://git.ccfe.ac.uk:4567/process/process/dev:latest`. Singularity will then ask for a username and password, your CCFE GitLab short username and password.

Singularity will write the container into your current directory, it can then be moved or copied like any file.

Running `singularity shell process.sif` will load a Singularity shell with the dependencies for PROCESS installed. Singularity will automatically mount your home (`$HOME`) directory into the container. Therefore, if PROCESS lives in `~/process` on your system, it will also live inside of `~/process` in the shell environment.

It should also be noted that while the Singularity container has a Python 3.8 by default, it will be impossible to pip install any packages without getting a `Read-only file system` error. This is because you are treated as a non-admin user within the container, and, as such, you cannot update the system Python. For this reason, it is recommended that you still use a virtual environment within the Singularity container (as described above). `pip install <package> --user` will work; however, it will cause conflicts with existing Python packages you have installed outside of your container.

## Testing
### Check for a successful installation
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

... should output something similar to:
```bash
<module 'process' from 'path/to/process/root/__init__.py'>
```

This indicates that the PROCESS Python package has been installed.
To exit the Python interpreter you can use
```BASH
exit()
```
and press enter, or use `Ctrl-Z`.

### PROCESS test suite
The PROCESS test suite provides through tests that can be used to confirm a successful installation; the tests can then be used to verify changes you make have not affected the wider codebase.

Firstly, ensure you are in the PROCESS root directory.
```BASH
cd process
```

The test suite uses PyTest and can be fully run using:
```BASH
pytest
```
which runs unit, integration, and regression tests.

A more in-depth discussion of testing can be found [here](http://process.gitpages.ccfe.ac.uk/process/development/testing/).

If everything passes, this indicates a successful installation. If anything fails, this indicates that your environment produces different results to what is expected. You might consider creating an issue in Gitlab, or trying out the Docker container instead.

## Automatically activating virtual environment on VS Code Open
When VS Code is first opened, you are able to set it such that the command:
```bash
source env/bin/activate
```
is executed automatically. This saves manually activating the virtual environment everytime you open the application. This is done by first using `Ctrl+Shift+P` and searching for `Python:Select Interpreter`. The select: `Python *version* ('env':venv) ./env/bin/python`. This should be starred as the recommended version. Now, close your terminal and close VS Code. Reopen and open a new terminal which should now automatically point to the virtual environmnet signalled by an `(env)` in front of your user. 

## Prepare Release (not required)
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
