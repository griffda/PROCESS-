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

## Using black with VSCode
In VSCode, use `Ctrl+,` (`Command+,` for Mac users) to open the settings page. From here, use the search bar to find **"Editor: Format On Save"** and tick the box. Next, search for **"Python > Formatting: Provider"** and set it to `black`. Now, upon saving a Python file, the black formatter will run over it.

## Further reading
The following is a list of references for the tools pre-commit runs:
- [flake8 Docs](https://flake8.pycqa.org/en/latest/index.html)
- [Black Docs](https://black.readthedocs.io/en/stable/)
- [yamlfmt on GitHub](https://github.com/jumanjihouse/pre-commit-hook-yamlfmt)
