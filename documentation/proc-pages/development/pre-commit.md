# Pre-commit

[pre-commit website](https://pre-commit.com/#top_level-files)

Pre-commit is Process' way of ensuring all code pushed to our repository is of a certain quality and style. One common style ensures Process can be easily navigated and understood by all users.

Pre-commit performs some miscellaneous checks such as check merge conflicts have been resolved (some Git almost always does), ensuring Docstrings are in the right place, and checking that no Python debug statements have been left in. It will also automatically reformat all files to have exactly one trailing newline at the end, and that there are no trailing whitespace at the end of lines.

If pre-commit modifies any files, that job will **fail** and the modified files will need to be added added (using `git add`) to be commited.


!!! note
    Installation of pre-commit is covered in the [Installation Guide](http://process.gitpages.ccfe.ac.uk/process/installation/).


You can run pre-commit before you `git commit` to avoid the need to re-add files: `pre-commit run -a` to run all hooks on all files. Without the `-a` flag, only staged files will be checked (same as when the automatic Git hook is called).

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
