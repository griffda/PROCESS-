# Pre-commit

[pre-commit website](https://pre-commit.com/#top_level-files)

Pre-commit is Process' way of ensuring all code pushed to our repository is of a certain quality and style. One common style ensures Process can be easily navigated and understood by all users.

Pre-commit performs some miscellaneous checks such as check merge conflicts have been resolved (some Git almost always does), ensuring Docstrings are in the right place, and checking that no Python debug statements have been left in. It will also automatically reformat all files to have exactly one trailing newline at the end, and that there are no trailing whitespace at the end of lines.

If pre-commit modifies any files, that job will **fail** and the modified files will need to be added added (using `git add`) to be commited.


## Installation
When running `cmake --build build/` pre-commit should have been installed automatically. This can be tested by typing `pre-commit -h`; if pre-commit is not installed, then it can be installed by `python3.8 -m pip install pre-commit`. For developers of Process, `pre-commit install` will install pre-commit as a **Git pre-commit hook**. When pre-commit is a hook, it will run on all of the files you have changed before allowing a commit -- if any changes are identified, they will be fixed and you will need to re-add the files that pre-commit has changed.

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

## Flake8
[flake8 Docs](https://flake8.pycqa.org/en/latest/index.html)
Flake8 is the linter that Process uses to ensure all Python code is of a certain quality. It, among other things, enforces a maximum line length, enforces strict rules around useless imports and unused variables.

Flake8 will print a list of problems which should be addressed before a commit is made - these issues will be flagged by the CI system, and the job will fail.

Flake8 has a list of excluded issues that it will not check for.

## Black
[Black Docs](https://black.readthedocs.io/en/stable/)
Black is an automatic code formatter for Python. It enforces a "one right-way" system. All code will be formatted to comply with their code style. This allows developers to write code in a style they are familiar with, and format their code before commiting it to the repository.

## yamlfmt
[yamlfmt on GitHub](https://github.com/jumanjihouse/pre-commit-hook-yamlfmt)
Formats all YAML files including CI and pre-commit configuration files.
