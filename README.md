# PROCESS

PROCESS is the reactor systems code at [CCFE](www.ccfe.ac.uk). More information on PROCESS
can be found on the PROCESS [webpage](http://www.ccfe.ac.uk/powerplants.aspx).

## Documentation

### User guide
To read about how the code works and the modules in it see the
[user guide](http://www.ccfe.ac.uk/assets/Documents/Other/process.pdf)

### Physics paper

A [paper](http://www.sciencedirect.com/science/article/pii/S0920379614005961)
outlining the physics models in PROCESS published in fusion engineering and design.

### Engineering paper

A [paper](http://www.euro-fusionscipub.org/wp-content/uploads/2015/08/WPPMIPR1505.pdf)
outlining the engineering models in PROCESS published in fusion engineering and design.

### Other papers

A list of other papers using PROCESS:

- "Impurity radiation in DEMO systems modelling", H. Lux et al., 2015, fusion engineering and
design, ([paper](http://www.sciencedirect.com/science/article/pii/S0920379615302891))
- "Implications of toroidal field coil stress limits on power plant design using PROCESS", J. Morris et al.,
SOFT 2014, fusion engineering and design ([paper](http://www.sciencedirect.com/science/article/pii/S0920379615301290)).

## Build

- get repository
    - `git clone git@git.ccfe.ac.uk:process/process.git folder_name`. Where `folder_name`
    is the name of the folder that will be created when cloning the repository.
- inside PROCESS directory run `make` to compile the executable `process.exe`.

Additionally

- to make python dictionaries run `make dicts`
- to make documentation run `make doc`
- to make html files run `make html`
- to make everything run `make all`
- to clean the directory run `make clean`

## Run

- create input file IN.DAT
- run `./process.exe`
- results output in OUT.DAT, MFILE.DAT

## Development

### Commit Logs

To see the commit messages you can use the `git log` command. There are various options
described below.

| Command | Description | Example |
| -------- | -------- | --------- |
| `-(n)`   | show the last `n` commits  | `git log -5` |
| `--since or --after` |  limits the logs to be from date given | `git log --since "21-01-15"` |
| | can use `number.scale` where scale=year, month, week, day and minute | `git log --since 2.weeks` |
| `--until or --before` |  limits the logs to be up to date given | `git log --until "22-01-15"` |
| `--author` | only shows commits from given author | `git log --author "morrisj"` |
| `--grep` | only show commits with a commit message containing the string given | `git log --grep "magnet"`  |
| `--stat` | if you want to see some abbreviated stats for each commit | `git log --stat` |
| `--oneline` | Outputs commit number, date and message to a single line | `git log --oneline` |
| `--graph` | display commits in a ASCI graph/timeline | `git log --graph` |
| `-S` | only show commits adding or removing code matching the string | `git log -S "find_me"` |

- to output the log to a file add `>> file_name.log` to the end of the command

### Changing the code

Major changes

- Create a new branch (e.g. "model_a_development") on the GitLab webpage.
    - or run `git branch new_branch_name` after clone the repo (next bullet)
- Clone the repo `git clone git@git.ccfe.ac.uk:process/process.git`
- Swap to your branch `git checkout new_branch_name`

Minor changes

- Clone the repo `git clone git.git.ccfe.ac.uk:process/process.git`

### Committing changes

- Make your changes to the code and at suitable stages commit locally:
    - `git add file_changed_1 file_changed_2`
    - `git commit -m "COMMIT MESSAGE"`
- The commit message should be informative and give useful information for future development.
  - Such as:
  ```
  Made changes to the TF coil magnet model. Updated the allowable stress in the coils
  to be 600MPa. Remove side-wall case. Ran test suite and everything OK.
  ```
  - not
  ```
  Update to magnet model
  ```

- Before pushing back to the repository make sure that your branch is up to date
with any changes that might have been made by other developers, `git pull`
- When you wish to push your branch back to the repository enter `git push`

### Merging

#### Develop into your branch

If you have finished making a major change, after creating a new branch, you will need
to merge your branch with the develop branch.

- Make sure you have committed all of your changes to your local branch.
- Update your local repo with `git pull`
- Checkout the development branch `git checkout develop`
- Check remote repo again `git pull`
- Checkout your new branch `git checkout my_branch_name`
- Merge develop into your branch `git merge develop`
- If there are conflicts check the files listed for the following:
```
<<<<<<< HEAD
This line was edited in dev_mynewmodel branch
=======
This line was edited in develop branch
>>>>>>> develop
```

- Resolve any conflicts then `git add file_1 file_2` where file_1 and file_2 are
files that had conflicts.
- Commit the changes `git commit`
- Push the branch back to the remote repo `git push`

#### Your branch into develop

After developing your branch and during the development merging develop into it to make
sure the two branches don't diverge you will need to merge your branch with develop.

- Check your repo is up to date `git pull`
- `git checkout my_branch_name`
- `git checkout develop`
- `git merge my_branch_name`
- Resolve conflicts in similar manner to section above
- `git push`

### Tagging

Version takes the form `x.y.z` for internal development versions and takes the form `x.y` 
for external master releases. 

This will be similar to the .NET convention of version numbering


`[major version]`.`[minor version]`.`[revision number]`

`[major version]` - release containing numerous major changes

`[minor version]` - medium change, i.e. new model, major bug fix

`[revision number]` - weekly or on demand build/change


To add a tag to a commit do the following:

- In routine inform of file `process.f90`, change the definition of `progver` by
incrementing the revision appropriately (given guidance above) to `x.y.z` and the release date. It
is important to keep exactly the same format.
- Add a brief comment to the bottom of source file `process.f90` describing the changes made
since the last commit in the same branch.  Start the line with `! x.y.z:`, following the
existing examples
- If any of the User Guide `.tex` files have been modified, edit the definition of `\version`
in `process.tex` by changing the Revision (to `x.y.z`) and the date
- If you have changed any ”use” statements in the code, or any compilation dependencies in the Makefile, run
`make clean`
- To ensure that all the code and documentation compiles successfully run `make all`
- Run `test_suite.py` in the `test_suite` folder to ensure PROCESS runs correctly
- Add files changed `git add file_1 file_2 ...`
- Commit changes `git commit -m "COMMIT MESSAGE"`
- Add a tag number `git tag -a x.y.z -m "Version x.y.z"`
- `git push`
- `git push origin x.y.z`

When releasing a ***tagged version of the code the user should compile a release note for the repo***.
This note should outline the major changes for the release, including issues from GitLab that
were resolved, bugs fixed, etc.

Between user tags Git will create tags in the following format:

```
1.0.12-11-g3f1b433
```

- `1.0.12` is the last manually entered tag by the user
- `11` is the number of commits since that tag
- `g3f1b433` is a unique identifier for this specific commit

This allows the user to checkout a specific commit between tagged versions. PROCESS now outputs this information into the `OUT.DAT` and `MFILE.DAT` and is updated upon compilation. This way each output file is trackable to a specific commit.


**Tagging Commands**

| Command | Description |
| -------- | -------- | 
| `git describe --tags`   | show the current tag  | 
| `git tag -l "1.0.*"` | list tags contained in `1.0.z` |
| `git checkout tags/<tag name>` | checkout a specific tag |

## Windows Users

PROCESS does compile and run on Windows. The following instructions will get you to the point of compiling 
and running the fortran code (and the Python utilities) on Windows.

1. Install `mingw` using the following [installer](http://www.fortran.com/blog/wp-content/uploads/2013/05/g95-Mingw_201210.exe).
    - click on basic setup and install the following options
        - `mingw32-base`
        - `mingw32-gcc-gfortran`
        - `mingw32-gcc-g++`
        - `msys-base`
    - Setup the windows environment variable `PATH` = `c:\MinGW\bin\`
        - in `c:\MinGW\bin\` make a copy of `mingw32-make.exe` called `make.exe`

2. Install a suitable editor or IDE. [Microsoft Visual Studio Code](https://code.visualstudio.com/) is 
recommended.

3. Install a Python distribution for windows and make sure it is available on the Windows command prompt.
    - Recommend the [Anaconda Python distribution](https://www.continuum.io/downloads) for Windows.

4. Install [git](https://git-for-windows.github.io/) for Windows
    - ideally choose the option of having the git commands available to the Windows command prompt.
    - set your git `user.name` and `user.email` to the desired values
        ```
        git config --global user.name "User Name"
        git config --global user.email "user.name@domain.com"
        ```
    - turn of git converting line endings to `CRLF`
        ```
        git config --global core.autocrlf false
        ```

5. Clone the code from the PROCESS gitlab repository.
    ```
    git clone http://git.ccfe.ac.uk/process/process.git -b branch folder_name
    ```

6. **Check that your editor is using `LF` line endings and not `CRLF` line endings.**

7. Go into the PROCESS code folder and build the code by running the following in the command prompt
    ```
    make
    ```

8. Run the code (provided there is an  `IN.DAT`)
    ```
    process.exe
    ```

9. Running the utilities
    - Edit environment variables and creating a new one called `PYTHONPATH` and set it 
    to `c:\some path\utilities\`. Where `some path` is the location of the process folder.
    - Make the PROCESS Python dictionaries
        ```
        make win_dicts
        ```
    - Run a utility, e.g.
        ```
        python utilities\plot_proc.py -f MFILE.DAT
        ```

10. To create the HTML documentation run,
    ```
    make win_doc
    ```

11. Finally, to clean the make run,
    ```
    make win_clean
    ```

## Contacts

[Hanni Lux](Hanni.lux@ukaea.uk)

[Michael Kovari](michael.kovari@ukaea.uk)

[James Morris](james.morris2@ukaea.uk)