# Unit Tests
[PDF of webpage](../pdf/unit-tests.pdf)

The PROCESS code uses Googletest framework for unit testing. The following 
instructions show how to implement a test.

## Test without module variables

This will outline the procedure for adding a new basic test. The FORTRAN function to be added is one that calculates the binomial coefficient for a given n, k.

```math
\binom{n}{k} = \frac{n!}{k!(n-k)!}
```

This function is defined in `maths_library.f90` as:

```fortran
real(kind(1.0D0))  function binomial(n,k) result(coefficient) &
  bind (C, name="c_binomial")
  
  ! This outputs a real approximation to the coefficient
  implicit none
  integer, intent(in) :: n, k
  integer :: numerator, i
  
  if (k == 0) then
      coefficient = 1
  else
      coefficient = 1.0D0
      do i = 1, k
          numerator = n + 1 -i
          coefficient = coefficient * real(numerator)/real(i)
      end do
  end if
  
end function binomial
```

The function takes two arguments `n` and `k` and returns a result, `coefficient`, which is a `real(kind(1.0D0))` which means double. The `bind` keyword provides a link between the C++ Googletest framework and the FORTRAN. The `name` is the name that the function will be referred to from the C++. The 
changes required to the files for this example are as follows.

### main_test.cpp

Below is an outline of the `main_test.cpp` file in the PROCESS repository.

```c++
#include <iostream>
#include <gtest/gtest.h>

#include "test_functions.h"
#include "test_util.h"
#include "test_evaluators.h"
#include "test_availability.h"
#include "test_current_drive.h"
#include "test_build_fixture.h"
#include "test_maths_lib.h"

int main(int argc, char *argv[])
{
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

To add the new test in `test_maths_lib.h` a new `include` statement is required in this file, if is not already present. To add a test to an existing 
set of tests there is no need to add anything here.

### test_maths_lib.h

Below is the contents of the `test_maths_lib.h` file which will contain the tests for the source file `maths_library.f90`.

```c++
TEST(Maths_library, binomial_1) { 
   int n = 1;
   int k = 1;
   double c;
   c = c_binomial(&n, &k);
   EXPECT_EQ(1.0, c);
}
```

The first statement defines which group of tests the test `binomial_1` belongs to (`Maths_library`). We then declare in C++ the two arguments required for the function, `n` and `k`.

```c++
int n = 1;
int k = 1;
```

A local declaration for the function result is also required.

```c++
double c;
```

The test is then called using the `name` given in the C `bind` statement from the function declaration in `maths_library.f90`.

```c++
c = c_binomial(&n, &k);
```

The two local variables `n` and `k` are passed by reference so hence the `&` before the name.

The last part of the test performs the check against the expected value:

```c++
EXPECT_EQ(1.0, c);
```

This asserts that we expect the outcome:

```math
\binom{1}{1} = 1
```

It will report a failure otherwise. A failure in any test case will count as a 
failed CI pipeline.

### test_functions.h

Below is the part of `test_functions.h` that needs to be modified to add the new unit test `binomial_1`.

```c++
extern "C"
{
   // Function prototype for accessing Fortran functions 
   double c_process_value_cpp(double *, double *);
   void c_funfom(double *);
   void c_calc_u_unplanned_hcd(double *);
   void c_iternb(double *, double *, double *);
   void c_cfnbi(double *, double *, double *, double *, double *, double *,double *, double *, double *);

   // new entry
   double c_binomial(int *, int *);
}
```

The final line:

```c++
double c_binomial(int *, int *);
```

Tells the Googletest C++ framework about the binomial function, referring to the `bind` name that was given. It also tells the framework what arguments to expect (to integer pointers) and what value the function returns (`double`). If the subroutine didn't return a value one would declare is as `void <subroutine_name>`.

### Running the test

To run the test, run the PROCESS compilation again `cmake --build build`.
This will create an executable file in `/bin/` called `process_GTest.exe`

Run the unit test executable with:

```bash
./bin/process_GTest.exe
```

## Test output

The output of the binomial test will be part of the overall test output and should look like this.

```bash
[----------] 1 test from Maths_library
[ RUN      ] Maths_library.binomial_1
[       OK ] Maths_library.binomial_1 (0 ms)
[----------] 1 test from Maths_library (0 ms total)
```

# Test Case with Global Variables

In FORTRAN a subroutine can use variables outside of the scope of the subroutine by importing variables from other modules using the `use` statement. Such as,

```fortran
use global_variables, only: verbose, maxcal
```

This statement says from the module `global_variables` use only `verbose` and `maxcal`.

To make the Googletest testing framework work some modifications need to be made.

The example test being added is for the subroutine `calc_u_unplanned_bop` which calculates the estimated unplanned downtime for the balance-of-plant system for a fusion power plant. It is in the file `availability.f90`.

## calc_u_unplanned_bop

The subroutine is declared as:

```fortran
subroutine calc_u_unplanned_bop(outfile, iprint, u_unplanned_bop) &
    bind(C, name="c_calc_u_unplanned_bop")
```

Like the `maths_library.f90` example it requires a C `bind` statement to be added. The subroutine takes three arguments

```fortran
integer, intent(in) :: outfile, iprint
real(kind(1.0D0)), intent(out) :: u_unplanned_bop
```

The two inputs are to configure the PROCESS code output and where it is written (they are both integers). The output is a real.

The subroutine also uses some local variables, declared as:

```fortran
real(kind(1.0D0)) :: bop_fail_rate, bop_mttr
integer :: bop_num_failures
```

The calculation for the subroutine is given below:

```fortran
! Balance of plant failure rate (failures per hour)
! ENEA study WP13-DTM02-T01
bop_fail_rate = 9.39D-5

! Number of balance of plant failures in plant operational lifetime
bop_num_failures = nint(bop_fail_rate * 365.25D0 * 24.0D0 * t_operation)

! Balance of plant mean time to repair (years)
! ENEA study WP13-DTM02-T01
bop_mttr = 96.0D0 / (24.0D0 * 365.25D0)

! Unplanned downtime balance of plant
u_unplanned_bop = (bop_mttr * bop_num_failures)/(t_operation)
```

You will notice that there is a variable used in the calculation that is not defined in the subroutine or passed as an argument. To make the variable visible to the Googletest framework one needs to add C bindings to the variable where it is declared.

## t_operation - global variable

Searching inside the code, one should find the declaration of `t_operation` in `global_variables.f90`:

```fortran
!+ad_vars  t_operation : Operational time (yrs)
real(kind(1.0D0)) :: t_operation = 0.0D0
```

To add a C binding this should become:

```fortran
!+ad_vars  t_operation : Operational time (yrs)
real(kind(1.0D0)), bind(C) :: t_operation = 0.0D0
```

The definition of `t_operation` is in `availability.f90`.

```fortran
! Operational time (years)
t_operation = tlife * (1.0D0-u_planned)
```

## test_availability.h

Once complete add the test case to the `test_availability.h` file, such as:

```c++
TEST(Availability, calc_u_unplanned_bop) { 
   int a = 0;
   int b = 0;
   double result;

   // module and global variables
   extern double t_operation;
   t_operation = 25.0;

   c_calc_u_unplanned_bop(&a, &b, &result);
   EXPECT_NEAR(result, 0.009, 0.0005);
}
```

## test_functions.h

And add an entry in `test_functions.h` as before:

```c++
void c_calc_u_unplanned_bop(int *, int *, double *);
```

## Result

After running `process_GTest.exe` in `/bin/` the result should look the 
following.

```bash
[----------] 2 tests from Availability
[ RUN      ] Availability.calc_u_unplanned_hcd
[       OK ] Availability.calc_u_unplanned_hcd (0 ms)
[ RUN      ] Availability.calc_u_unplanned_bop
[       OK ] Availability.calc_u_unplanned_bop (0 ms)
[----------] 2 tests from Availability (0 ms total)
```