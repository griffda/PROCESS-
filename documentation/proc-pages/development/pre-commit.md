# Pre-commit

[pre-commit website](https://pre-commit.com/#top_level-files)

Pre-commit is Process' way of ensuring all code pushed to our repository is of a certain quality and style. One common style ensures Process can be easily navigated and understood by all users. It also reduces the "diffs" when reviewing code changes as often small style changes (such as the addition of a new-line) will litter the "diffs" and make reviewing harder.

Pre-commit works on staged files (ie those that have been `git add`ed) after you issue the `git commit` command but before the commit is made. If any pre-commit job **failed** then the commit will not be made. On a failure, one of two things can happen:

1. Pre-commit plugins will rectify the mistakes made. This will happen with code formatters (whose job it is to edit your files to the correct style). The files the plugins have changed can then be `git add`ed again and the `git commit` command re-issued.
2. A pre-commit plugin will identify the mistake but will NOT fix it. This will happen with `flake8` which is a linter and warns of code mistakes but does not correct them. You will need to fix these mistakes manually. Now, the changed files can be `git add`ed and the `git commit` command re-issued.

I would advise you become familiar with the *What does pre-commit check for?* section of this document. This will allow you to understand whether a mistake has been automatically fixed or not.


## Installation
When running `cmake --build build/` pre-commit should have been installed automatically. This can be tested by typing `pre-commit -h`; if pre-commit is not installed, then it can be installed by `python3.8 -m pip install pre-commit`. For developers of Process, `pre-commit install` will install pre-commit as a **Git pre-commit hook**. When pre-commit is a hook, it will run on all of the files you have changed before allowing a commit -- if any changes are identified, they will be fixed and you will need to re-add the files that pre-commit has changed.

!! warning "Pre-commit Python version"
    It is important then pre-commit is running on Python 3.8. Any other Python version may produce code formatted in such a way that our CI system rejects your code.


!!! example "Adding two files"
    Consider that two files are being `git add`ed.
    One of the files, `foo.py` has stylistic changes which **Black** objects to.

    ```
    > git add foo.py bar.py
    > git commit -m "Adding foo and bar"
        Trim Trailing Whitespace.................................................Passed
        Check for merge conflicts................................................Passed
        Debug Statements (Python)................................................Passed
        black....................................................................Failed
            - hook id: black
            - exit code: 1
            - files were modified by this hook

            Fixing foo.py
        Format YAML files....................................(no files to check)Skipped

    > git add foo.py # since black has modified foo.py
    > git commit -m "Adding foo and bar"
    ```

    To avoid the need to re-add files a second time you could run `black .` which will do the formatting (of Python code) that pre-commit would do.


## Pre-commit and the `quality` CI stage
The Process continuous integration system (used to check Process builds and passes tests) also has a `quality` stage. This is where several jobs will run to ensure the quality of your code. If all your commits pass through pre-commit, then these jobs should not fail as your code will be of a high quality. 

## Using black with VSCode
Although not required, the `black` VSCode extension will ensure that all the Python files you save will be black-compliant and, as such, won't need to modified by pre-commit.

In VSCode, use `Ctrl+,` (`Command+,` for Mac users) to open the settings page. From here, use the search bar to find **"Editor: Format On Save"** and tick the box. Next, search for **"Python > Formatting: Provider"** and set it to `black`. Now, upon saving a Python file, the black formatter will run over it.

## What does pre-commit check for?
### General style on all files
Pre-commit performs a few checks on each and every file you add, regardless of type.

* `end-of-file-fixer` will check that each file ends with exactly one new line. **This plugin will automatically fix any mistakes it finds**.
* `trailing-whitespace` will check that there is no excess whitespace (spaces or tabs) at the end of any line. **This plugin will automatically fix any mistakes it finds**.
* `check-merge-conflict` checks that all merge conflict's have been resolved. **This plugin will NOT automatically fix any mistakes it finds**.

### Python checks
Because Process is becoming increasingly Pythonised, pre-commit performs many Python style checks.

* [`black`](https://black.readthedocs.io/en/stable/) has already been discussed on this page. It is an industry-standard Python code formatter that enforces a "one-way is right" style. This ensures all of our Python is of the same, black-correct, style. **This plugin will automatically fix any mistakes it finds**.
* [`flake8`](https://flake8.pycqa.org/en/latest/) is the linter of choice on Process. A linter checks common errors in code. Flake8, for instance, can check for `import` statements that are unused or variables that are declared but never used. Black, a formatter, will not remove these "mistakes" as it will never change the semantic meaning of code. **This plugin will NOT automatically fix any mistakes it finds**.
* `check-docstring-first` will check that a function/class docstring comes before any of the body (ie the docstring must be at the top). **This plugin will NOT automatically fix any mistakes it finds**.
* `debug-statements` will check that debug statements (those using the built-in `pdb` module) have been removed. **This plugin will NOT automatically fix any mistakes it finds**.

### Other checks
* [`yamlfmt`](https://github.com/jumanjihouse/pre-commit-hook-yamlfmt) formats YAML code (similar to what `black` does for Python code). **This plugin will automatically fix any mistakes it finds**.
