# Testing
## Understanding testing
The purpose of testing is to check that the existing code behaves as it is expected to, and that any code changes don't produce unexpected results, such as breaking pre-existing functionality or creating an unwanted side-effect. It allows changes to be made with confidence and it increases confidence in the results produced by the code. 

Tests run part of the code with a given input and assert that the observed output is equal to an expected output. If the assertion is true the test will pass, if it is false, it will fail. A collection of tests in known as a test suite, and they can be classified into three different testing scopes:

### Unit tests
Unit tests test the smallest possible piece of code, like a function or method. It might call a function with some arguments and assert that it returns what is expected.

### Integration tests
Integration tests test larger pieces of code that make up parts of the program, such as classes, an input file reader or a plotting utility. It might create an instance of a class with an input filename, then call a method to create a plot file, then check that the plot was created without throwing any exceptions.

### Regression tests
Regression tests detect changes in the entire program's behaviour by checking that for a given input, it produces exactly the same output as before those changes. It detects changes in the program's output. Therefore if your code changes the program's output, it will fail the regression test. In this case that output difference will need to be reviewed and if accepted, the expected (or reference) result will need to be updated in the test suite.

## pytest
Process uses the `pytest` testing framework in its test suite. `pytest` tests are modular, quick to write with little code and produce helpful information when they fail. It is used widely in the Python world.

### Running pytest
`pytest` can be run locally by running `pytest` in the project root directory. This will run all tests. `pytest` can also be configured to run in the sidebar of VS Code. The Continuous Integration (CI) system also runs the `pytest` test suite in the `testing` stage of the pipeline. Unit, integration and regression tests are run as separate jobs to make it easier to see where failures lie.

### Updating test references
If code changes affect the result of a test so that the observed output no longer equals the expected output stored in the test suite, the test will fail and the test reference will need to be updated. In Process, this is typical in the regression scenario tests (`tests/regression/test_scenario.py`) and the Python-Fortran dictionary tests (`tests/integration/test_dicts.py`).

To run the regression tests with a 5% tolerance run `pytest tests/regression --reg-tolerance=5`; this will show you only the values in each regression scenario that have changed by >5% and hence still fail the test. This is useful for seeing the values that have changed by a significant amount.

To overwrite the references, run `pytest --overwrite` which will re-run the test suite, overwriting both the Python-Fortran dictionaries and the regression scenario reference files. Running the test suite again with `pytest` should then pass.

For the correct way to contribute code to Process (including how to update the test references), see CONTRIBUTING.md.

## Reasoning behind the CONTRIBUTING.md method
When reviewing code in a merge request, it is important to understand the effect those changes will have on the output for various regression scenarios. This particularly applies to detecting large unintended changes to the output in certain scenarios.

Typically the solver will arrive at a very slightly different solution, and so there are large numbers of very small changes with a few more significant changes amongst them. It is therefore useful for the reviewer to be able to filter out those more significant changes, say >5%, in order to understand the effect of the code changes. This is why a %5 tolerance regression test job is used in the CI system; it will fail and report if any values in any scenario differ from the reference values by >5%.

In a branch with code changes that affect the output, it is helpful to commit the corresponding regression test reference changes too. This means that the code changes are accountable for the regression reference changes in the same branch; it is possible to see the effect of the branch on the output.

Specific aspects of the CONTRIBUTING.md procedure are reasoned below.

### Step 5: Merging develop into the issue branch
This ensures that the code and test references are up to date with develop, which may have changed whilst the issue branch was being worked on. This ensures that when the test references are overwritten, the reference changes are only as a result of the branch code changes and the reference changes are relative to the latest references on develop. 

If the issue branch was merged to develop without merging develop into the branch first, there would be problems. The tests would pass on the branch after overwriting, but on merging to develop there could be merge conflicts in the references (they would have been changed on both branches). After resolving the conflicts, or even if there weren't any, it is highly likely that the resulting references on develop would result in a failure of the test suite. This is because the references would then be a product of two branches overwriting different parts of them, and hence they wouldn't correspond to a single output of a regression scenario any more.

The regression references will always need to be overwritten completely, and different versions of them shoudn't be merged together. Merging develop into the issue branch first avoids this.

### Step 6: Running the test suite locally
It is useful to see which tests fail before pushing to the remote, not least because it is faster to run them locally. If tests other than the Python-Fortran dictionary tests or the regression tests fail, then further work is needed to get those tests to pass, as updating the test references will not fix them.

### Step 7: Pushing despite failing Python-Fortran dictionary and/or regression tests
The purpose of this is to run the regression test job with 0 and 5% tolerances. The 0% job will fail, but the 5% job will only fail if there are any changes above 5%. This allows the reviewer to see only the significant changes in the 5% job trace. If there are any failures in the Python-Fortran dictionaries, these will also be clear.

### Step 8: Overwriting the test references and committing them
This allows the code changes and corresponding complete reference changes to be on the same branch and hence easily accountable. After the overwrite, the test suite should pass.

### Step 9: Passing pipeline and merge request review
The pipeline should now pass, and the significant reference changes should be clear to the reviewer by looking at the (possibly failed) 5% tolerance regression job.