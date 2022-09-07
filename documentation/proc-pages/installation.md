# Installation
Below are instructions of how to install the applications, code and dependencies such that you can begin working on Process. This includes differing instructions for those working on different operating systems.

# 1. Getting Setup
## Supported environments
**Please note, only Python3.8 is supported, Python3.9, Python3.10 and Python3.6 are not.**
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

If the above procedure fails to work, there is a page on Nucleus giving a further detailed instructions on installing WSL on Windows which have proven to be helpful for some. They can be found [here](https://intranet.ukaea.uk/software/guides/wsl2.html). 

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
sudo apt install -y cmake gfortran python3-pip lcov poppler-utils python3.8-venv
```
Next, the code will need to be downloaded so you can work with it. The Process code is stored in a GitLab repository and as such needs to be 'cloned' - i.e bought to your VSCode window from GitLab. 

To clone the PROCESS repository from Gitlab you will need to use an SSH key. A guide on how to find if you have an existing SSH key pair or to generate a new SSH key pair can be found on Gitlab [here](https://docs.gitlab.com/ee/ssh/).

Clone the PROCESS repository from Gitlab, and navigate into the resulting directory:
```bash
git clone git@git.ccfe.ac.uk:process/process.git
cd process
```

reate and activate a virtual environment if you'd like to use one:

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

### Steps to use Singularity to install PROCESS on Freia: 

Log onto Freia and make sure the Singularity module is loaded 
```
module load singularity/3.7.1
```
Pull the Singularity image and enter your git username and password when prompted:

``` 
singularity pull --docker-login process.sif docker://git.ccfe.ac.uk:4567/process/process/dev:latest 
``` 
Make sure you have created [ssh keys](https://docs.gitlab.com/ee/ssh/) and open the Singularity shell:

```
singularity shell process.sif
```
Singularity is an environment which allows you to use dependencies not available on Freia, like the correct versions of Python and cmake. With the shell open, the installation of PROCESS can proceed as usual:

```
git clone git@git.ccfe.ac.uk:process/process.git
cd process
python3 -m venv env --without-pip --system-site-packages
source env/bin/activate
export PATH=$PATH:~USERNAME/.local/bin/
cmake -S . -B build
cmake --build build
```
Now you can run commands within the shell like `process -i tracking/baseline_2018/baseline_2018_IN.DAT` to verify installation and create [batch jobs](http://process.gitpages.ccfe.ac.uk/process/io/utilities-guide/).
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
is executed automatically. This saves manually activating the virtual environment everytime you open the application. This is done by first using `Ctrl+Shift+P` and searching for `Python:Select Interpreter`. The select: `Python *version* ('env':venv) ./env/bin/python`. This should be starred as the recommended version.
You will see that in your project the .vscode directroy will contain a `settings.json` file. Open this and inside of it add:

```json
"python.terminal.activateEnvironment": true
```
Dont forget to add a comma before to separate it from already present key value pairs.

Now, close your terminal and close VS Code. Reopen and open a new terminal which should now automatically point to the virtual environmnet signalled by an `(env)` in front of your user.

# 2. Virtual Environments
In the installation instructions above, a virtual environment is mentioned for using to work on Process. It might not be so clear as to why a virtual environment is used however.

Imagine a scenario where you are working on multipal projects and each project uses different versions of different dependencies. This can be confusing and cause issues down the line. A virtual environment is an aid that will allow maintenance of dependencies within each project. Essentially, having a virtual enviroment for each project you will work on will store the dependencies of those specific projects within itself, preventing crossover. This is the reason we recommend installing one so your other projects don't influence the dependencies of Process and visa versa.

*note*: Only Python3.8 versions are supported in Process. If you have installed a recent version of Ubuntu such as v22, Python 3.10 may have been installed. If this is the case it is recommended that you install Ubuntu20.04 which will have Python3.8 as its default. If not, this may lead to build issues down the line.

# 3. Troubleshooting

Experience has shown that the first few attempts at running `PROCESS` with a new file tends to produce infeasible results; that is, the code will not find a consistent set of machine parameters. The highly non-linear nature of the numerics of `PROCESS` is the reason for this difficulty, and it often requires a great deal of painstaking adjustment of the input file to overcome. The utility `a_to_b` is useful for this situation.
<br><br>

### Error handling

In general, errors detected during a run are handled in a consistent manner, with the code producing useful diagnostic messages to help the user understand what has happened.

There are three levels of errors and warnings that may occur:

Level 1: An *informational* message is produced under certain conditions, for example if the code modified the user's input choice for some reason.

Level 2: A *warning* message is produced if a non-fatal situation has occurred that may result in an output case that is inaccurate or unreliable in some way.

Level 3: An *error* message will occur is a severe of fatal error has occurred and the program cannot continue.

These messages are printed on the screen during the course of a run, and those still active at the final (feasible or unfeasible) solution point are also written to the end of the output file (messages encountered during the iteration process are not copied to the output file, as the convergence to a valid solution might resolve some of the warnings produced earlier in the solution process).

The `error_status` variable returns the highest security level that has been encountered (or zero if no abnormal conditions have been found); of a severe error (level 3) is flagged at any point the program is terminated immediately. The final message number encountered during a run is returned via output variable `error_id` . In addition, with certain messages, a number of diagnostic values may also be given; these can be used to provide extra diagnostic information if the source code is available.
<br><br>

### General problems

A code of the size and complexity of `PROCESS` contains myriads of equations and variables. Virtually everything depends indirectly on everything else because of the nature of the code structure, so perhaps it is not surprising that it is often difficult to achieve a successful outcome.

Naturally, problems will occur if some of the parameters become un-physical. For example, if the aspect ratio becomes less than or equal to one, then we must expect problems to appear. For this reason, the bounds on the iteration variables should be selected with care.

Occasionally arithmetic ("NaN") errors are reported. They usually occur when the code is exploring un-physical values of the parameters, and often suggest that no feasible solution exists for the input file used.

The error messages produced by the code attempt to provide diagnostic information, telling the user where the problems occurs, and also suggest a possible solution. These messages are out of necessity brief, and so cannot promise to lead to a more successful outcome.

The is the option to turn on extra debugging output; to do this, set `verbose = 1` in the input file.
<br><br>

### Optimisation problems

On reflection it is perhaps surprising that `PROCESS` ever does manage to find the global minimum figure of merit value, if there are `nvar` iteration variables active the search is over `nvar`-dimensional parameter space, in which there may be many shallow minima of approximately equal depth. Remember that `nvar` is usually of the order of twenty.

The machine found by `PROCESS` may not, therefore, be the absolute optimal device. It is quite easy to have two or more solutions, with results only a few percent different, but a long way apart in parameter space. The technique of "stationary" scans is sometimes used in this situation: a scan is requested, but the same value of the scan variable is listed repeatedly.

Scans should be started in the middle of a range of values, to try to keep the scan within the same family of machines. The optimum machine found may otherwise suddenly jump to a new region of parameter space, causing the output variables to seem to vary unpredictably with the scanning variable.

It should be noted that in general the machine produced by `PROCESS` will always sit against one or more operation limits. If, during a scan, the limit being leant upon changes (i.e. if the machine jumps from leaning on the beta limit to leaning on the density limit) the output parameters may well become discontinuous in gradient, and trends may suddenly change direction.
<br><br>

### Unfeasible results

In the numerics section of the output file, the code indicates whether the run produced a feasible ot unfeasible result.

The former implies a successful outcome, although it is always worth checking that the sum of the squares of the constraint residuals (`sqsumsq`) is small ($~10^{-3}$ or less); the code will issue a warning if the solver reports convergence but the value of `sqsumsq` exceeds $10^{-2}$. If this occurs, reducing the value of the HYBRD tolerance `ftol` or `VMCON` tolerance `epsvmc` as appropriate should indicate whether the result is valid ot not; the output can usually be trusted of (1) the constraint residuals[^1] fall as the tolerance is reduced to about $10^{-8}$, and (2) the code indicates that a feasible solution is still found.

An unfeasible result occurs if `PROCESS` cannot find a set of values for the iteration variables which satisfies all the given constraints. In this case, the values of the constraint residues shown in the output give some indication of which constraint equations are not being satisfied - those with the highest residues should be examined further. In optimisation mode, the code also indicates which iteration variables lie at the edge of their allowed range.

Unfeasible runs can be caused by specifying physical incompatible input parameters, using insufficient iteration variables, or by starting the problem with unsuitable values of the iteration variables.

The utility `run_process` carries out many runs, changing the starting values of the iteration variables randomly. It stops once a feasible solution is found.

Another approach is to start with an input file that gives a feasible solution, and modify it step by step towards the parameters desired. This process is automated by the utility `a_to_b`.

It is important to choose the right number of *useful* iteration variables for the problem to be solved - it is possible to activate too many iteration variables as well as too few, some of which may be redundant.

Both optimisation and non-optimisation runs can fail with an error message suggesting that the iteration process is not making good progress. This is likely to be due to the code itself unable to escape a region of the parameters space where the minimum in the residuals is significantly above zero. In this situation, there is either no solution possible (the residuals can therefore never approach zero), or the topology of the local minimum makes it difficult for the code to escape to the global minimum. Again, a helpful technique os to wither change the list of iteration variables in use, or to simply modify their initial values to try to help the code avoid such regions.

A technique that occasionally removes problems due to unfeasible results, particularly if an error code `ifail = 3` is encountered during an optimisation run, is to adjust slightly one of the limits imposed on the iteration variables, even if the limit in question has not been reached. This subtly alters the gradients computed by the code during the iteration process and may tip the balance so that the code decides that the device produced is feasible after all. For instance, a certain component's temperature might be 400 K, and its maximum allowable temperature is 1000 K. Adjusting this limit to 900 K (which will make no different to the *actual* temperature) may be enough to persuade the code that it has found a feasible solution.

Similarly, the order in which the constraint equations and iteration variables are stored in the `icc` and `ixc` arrays can make the difference between a feasible and unfeasible result. This seemingly illogical behaviour is  typical of the way in which the code works.

Another technique in such situations may be to change the finite difference step length `epsfcn`, as this might subtly change the path taken in the approach towards a solution.

It may be the case that the act of satisfying all the required constraints is impossible. No machine can exist if the allowed operating regime is too restrictive, or if two constraints require conflicting (non-overlapping) parameters spaces. In this case some relaxation of the requirements is needed for the code to produce a successful machine design.
<br><br>

### Hints

The above sections should indicate that it is the complex inter-play between the constraint equations and the iteration variables that determines whether the code will eb successful at producing a useful result. It can be somewhat laborious process to arrive at a working vase, and experience is often of great value in this situation.

It should be remembered that sufficient iteration variables should be used to solve each constraint equation. For instance, a specific limit equation may be $A \leq B$, i.e. $A = fB$, where the f-value $f$ must lie between zero and one for the relation to be satisfied. However, if none of the iteration variables have any effect on the values of $A$ and $B$, and $A$ happens to be *greater* than $B$, the `PROCESS` will clearly not be able to solve the constraint.

The lower and upper bounds of the iteration variables are all available to be changed in the input file. Constraints can be relaxed in a controlled manner by moving these bounds, although in some cases care should be taken to ensure that un-physical values cannot occur. The code indicates which iteration variables lie at the edge of their range.

It is suggested that constraint equations should be added one at a time, with required new iteration variables activated at each step. If the situation becomes unfeasible it can be helpful to reset the initial iteration variable values to those shown in the output from a previous feasible case and rerun the code.
<br><br>

[^1]: The constraint residuals are the final values of $c_i$ in the constraint equations. The value `sqsumsq` is the square root of the sum of the squares of these residuals.
