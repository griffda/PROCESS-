# `test_suite.py`

The PROCESS test suite allows PROCESS developers to quickly test new modifications to PROCESS using a defined library of reference cases. The test suite is bundled with PROCESS and is a single python script (`test_suite.py`) with a number of options.

The test suite will provide the following output:
* Summary to terminal and file (`summary.log`)
* PROCESS terminal output log to file for each test case (`run.log`)
* PROCESS error log to terminal and to file if errors for each test case (`error.log`)
* New `MFILE.DAT`s and `OUT.DAT`s for each test case (`new.MFILE.DAT` and `new.OUT.DAT`)

### Folder structure

The PROCESS folder structure is shown below. The test suite folder (`test_suite`) is in the main directory with the FORTRAN files. Inside the test suite folder there is the main code (`test_suite.py`) as well as the folders containing the reference cases (`test_files`). The figure below shows the folder structure for the `test_suite`.

![alt text](../../img/Test_suite_folder_structure.svg "Test suite folder structure")

Figure 1: PROCESS Test suite folder structure after a test run. he `test_area` folder is where the output of the test run is stored. Each test case has a README file which outlines the reasoning for the test.

### Adding a reference case

1. Have an `IN.DAT` that works with the current version of PROCESS
2. Run PROCESS to create an `OUT.DAT` and `MFILE.DAT`
3. Create a folder in the location `/my_develop/test_suite/test_files/[test_name]` with a suitable name for the test case (if your local PROCESS folder is `my_develop`).
4. Copy your `IN.DAT`, `OUT.DAT` and `MFILE.DAT` into this folder
5. Copy `IN.DAT` as `ref.IN.DAT`
6. Rename `OUT.DAT` as `ref.OUT.DAT`
7. Rename `MFILE.DAT` as `ref.MFILE.DATE`
8. cd `../..` to return to `test_suite` folder
9. `test_suite.py` to run the test suite to check it works
10. Commit to Git locally and push to the central repository

### Running the Test Suite

To run the test suite the user should go to the folder `/process/test_suite/` and the run the Python file `test_suite.py`. The test suite Python script has the following optional arguments.

```
python test_suite.py [options]

 -h - -help         show usage

 -s --save          save test output to folder with PROCESS revision
					number in it(e.g. test_r383)

 -d --diff [val]    set allowed difference between reference case and
					new output in percentage terms.

 -r --ref           choose reference folder to use for the test cases
                    default is "test_files"

 --debug            run reference cases prefixed with "error_" for
                    testing of the error handling in the test suite

 Examples

    python test_suite.py -d 10 --save

    python test_suite.py -d 1
```

### Test Suite Output Formatting

`summary.log`

The `summary.log` file mirrors what was displayed to the terminal during a test suite run. It will look somthing like the following.

```
PROCESS Test Suite

Date: 2015-12-03
Diff set to: 5.0%

PROCESS Test Cases

Test ==>  DEMO1_a31_06_2015             DIFF(6)
Test ==>  costs_paper                   OK
Test ==>  test 3                        ERROR
Test ==>  test 4                        OK

PROCESS version r[version] test run complete
```

`diff.log`

This file will contain the differences between the reference case and the new run for a given test. The file will look like the example below.

```
PROCESS Test Suite

Test Case: [test_name]
Difference value: [diff]

Differences above allowed value:

    var | ref | new | %
    a     10    15    50
    b     10    9     10
    ...   ...   ...   ...

Variables in ref NOT IN new:

    var 1
    var 2
    ...

Variables in new NOT IN ref:

    var 1
    var 2
    ...
```

`run.log`

`Run.log` will take the output to the terminal for each PROCESS test case and dump it to a file in the test folder in `/process/test_suite/test_area/[test_name]/`. If there is an error while running PROCESS this will be highlighted in `summary.log` and then the user can inspect the output in `run.log`.

`new.MFILE.DAT`

This is the `MFILE.DAT` the test suite created when running a given test case. After running the test suite will move this file to the folder `/process/test_suite/test_area/[test_name]/`.

`new.OUT.DAT`

This is the `OUT.DAT` the test suite created when running a given test case. After running the test suite will move this file to the folder `/process/test_suite/test_area/[test_name]/.`