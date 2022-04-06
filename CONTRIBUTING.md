# Contributing
To contribute to Process, please follow this procedure.

1. Raise an issue and describe the problem.
1. On the page for that issue, create a branch and merge request.
1. Pull the branch and implement the solution.
1. Build the code, run it and check your solution works.
1. Merge develop into the issue branch (being sure to pull develop first). This ensures that the code and test references are up to date with `develop`, which may have changed whilst the issue branch was being worked on.
1. Re-build using `cmake --build build` to update the installed `process` version with those changes from `develop`.
1. Run the test suite by running `pytest` in the project root directory. It is possible that the Python-Fortran dictionaries tests (`tests/integration/test_dicts.py`) or the regression tests (`tests/regression/test_scenario.py`) will fail at this point: the code changes may have changed the observed outputs which now differ to the expected outputs. If any other tests have failed, the code changes (or failing tests) need to be ammended so that they pass. You can also run `pytest tests/regression --reg-tolerance=5` to run the regression tests with a 5% tolerance to see what (if anything) has changed in the regression tests by more than 5%.
1. Once only the dictionaries and/or the regression tests fail (or everything passes), push the local commits to the remote if it hasn't been done already. This triggers the CI system, which will run the test suite again, including 0% and 5% regression tolerance jobs. The purpose of this is so that the reviewer of the merge request can see from the pipeline's 5% regression job what changes >5% (if any) your code changes create.
1. If you are happy with the >5% regression changes and that they are an intended consequence of your changes, run `pytest --overwrite`. This will overwrite the reference Python-Fortran dictionaries and the regression test references with the new outputs; it will overwrite the expected results with the observed ones. Once complete, running `pytest` again should result in everything passing. The test suite changes can then be committed and pushed.
1. The subsequent pipeline should then trigger and pass. The reviewer can see the regression changes >5% by looking at the 5% tolerance job before the regression references were overwritten. The complete reference changes are visible in the reference overwrite commit.
1. A YAML file should be created using the `create_changelog.py` script in the `changelog_update` directory and changes documented inside this YAML file - **if the changes are notable and it would benefit other users to be aware**. Please see the [Instructions on the new CHANGELOG procedure](documentation/proc-pages/development/versioning.md) section in the docs on how to create and edit this YAML file. (Upon Process upversioning, an RSE should go [here](changelog_update/README.md) for instructions on how to implement these changes.)
1. Once the changes are approved by the reviewer, the branch can then be merged.

For an explanation of the reasoning behind this approach, please read the [testing documentation](http://process.gitpages.ccfe.ac.uk/process/development/testing).
