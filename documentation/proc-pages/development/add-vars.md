# Style Guide
[PDF of webpage](../pdf/add_vars.pdf)

<p style='text-align: justify;'>
    Specific instruction must be followed to add an input, iteration variable
    optimisation figure of merit and constraints to the <em>PROCESS</em> code.
</p>

## Add a input

To add a *PROCESS* parameter, please follow the following steps:

1. Choose the most relevant variable module defined in `global variables.f90`.
2. Specify a sensible default value
3. <p>Add a description of the input variable below the declaration, specifying 
the units. Here is an example : </p>
4. Add the parameter to the `parse_input_file` subroutine in `input.f90`. Please us the `parse_real_variable` variable for reals, `parse_int_array` for integers and `parse_real_array` for array inputs. Here is an example of the code to add:

Variable definition example

```fortran  
  real(dp) :: rho_tf_joints = 2.5D-10
  !! TF joints surfacic resistivity [ohm.m]
  !! Feldmetal joints assumed.

```

Code example in the `input.f90` file.

```fortran
  case ('rho_tf_joints')
     call parse_real_variable('rho_tf_joints', rho_tf_joints, 0.0D0, 1.0D-2, &
          'TF joints surfacic resistivity')
```

## Add a iteration variable

<p style='text-align: justify;'>
  To add a <em>PROCESS</em> iteration variable, please follow the following steps in 
  addition to the instruction to add an input variable:
</p>

1. <p style='text-align: justify;'>
    The parameter `ipnvars` in module `numerics` in `numerics.f90` will normally
    be greater than the actual number of iteration variables, and does not need
    to be changed.
  </p>
2. <p style='text-align: justify;'>
    Utilise the next available block of code in module
    `define_iteration_variables` in `numerics.f90`.
</p>
3. <p style='text-align: justify;'>
    Assign values for the variable's lower and upper bounds to the relevant
    elements in arrays `boundl` (lower) and `boundu` (upper).
  </p>
4. <p style='text-align: justify;'>
    Paste the variable name in the relevant places in the code block in place
    of the word `DUMMY`.
  </p>
5. <p style='text-align: justify;'>
    Ensure that the relevant element of character array `lablxc` is exactly 14
    characters long.
  </p>
6. <p style='text-align: justify;'>
    Add the variable `use` `only` statement in the relevant functions (`itv_XX`
    and subroutine `set_itv_XX`).
  </p>


It should be noted that iteration variables must not be reset elsewhere in the
code. That is, they may only be assigned new values when originally
initialised (in the relevant module, or in the input file if required), and in
the `subroutine set_itv_XX` where the iteration process itself is performed.
Otherwise, the numerical procedure cannot adjust the value as it requires, and
the program will fail.

## Add a figure of merit

New figures of merit are added to *PROCESS* in the following way:


1. <p style='text-align: justify;'>
    Increment the parameter `ipnfoms` in module `numerics` in source file 
    `numerics.f90` to accommodate the new figure of merit.
  </p>
2. <p style='text-align: justify;'>
    Assign a description of the new figure of merit to the relevant element
    of array `lablmm` in module `numerics` in the source file `numerics.f90`.
  </p>
3. <p style='text-align: justify;'>
    Add the new figure of merit equation to routine `FUNFOM` in the source file
    `evaluators.f90`, following the method used in the existing examples. The
    value of `fc` should be of order unity, so select a reasonable scaling
    factor if necessary. 
  </p>
4. <p style='text-align: justify;'>
    Add the `use` `only` statements for all the added variables in all modified
    functions
  </p>

## Add a scan variable

After following the instruction to add an input variable, you can make the variable a scan variable by following these steps:

1. <p style='text-align: justify;'>
    Increment the parameter `ipnscnv` defined in the `scan_module` module in the `scan.f90` source file, to accommodate the new scanning variable. The incremented value will identify your scan variable.
  </p>
2. <p style='text-align: justify;'>
    Add a short description in the new scanning variable in the `nsweep` entry in source in the same source code, alongside its identification number:
  </p>
3. <p style='text-align: justify;'>
    Update the `scan_select` subroutine in the `scan.f90` source file by adding a new case statement connecting the vaiable to the scan integer switch, a short variable desciption `vlab` (the variable name) and a more explicit variable description `xlab`. Don't forget to add the use only statment at the beginning of `scan_select`.
  </p>
4. <p style='text-align: justify;'>
    Add a section on the input variable description indicating the scan switch.
  </p>

`nsweep` comment example:
```fortran

  integer :: nsweep = 1
  !! nsweep /1/ : switch denoting quantity to scan:<UL>
  !!         <LI> 1  aspect
  !!         <LI> 2  hldivlim
  ...
  !!         <LI> 54 GL_nbti upper critical field at 0 Kelvin
  !!         <LI> 55 `shldith` : Inboard neutron shield thickness </UL>
```

`scan_select` case example:

```fortran
  case (54)
      b_crit_upper_nbti = swp(iscn)
      vlab = 'Bc2(0K)' ; xlab = 'GL_NbTi Bc2(0K)'
  case(55)
      shldith = swp(iscn)
      vlab = 'shldith' ; xlab = 'Inboard neutronic shield'
```


## Add a constraint equation

Constraint equations are added to *PROCESS* in the following way:


1. <p style='text-align: justify;'>
    Increment the parameter `ipeqns` in module `numerics` in the source file
    `numerics.f90` in order to accommodate the new constraint.
  </p>
2. <p style='text-align: justify;'>
    Add an additional line to the initialisation of the array `icc` in module
    `numerics` in source file `numerics.f90`.
  </p>
3. <p style='text-align: justify;'>
    Assign a description of the new constraint to the relevant element of
    array `lablcc`, in module `numerics` in source file `numerics.f90`.
  </p>
4. <p style='text-align: justify;'>
    Add a new Fortran `case` statement containing the new constraint
    equation to routine `CONSTRAINT_EQNS` in source file
    `constraint_equations.f90`, ensuring that all the variables used in the
    formula are contained in the modules specified via `use` statements present
    at the start of this file.  Use a similar formulation to that used for the
    existing constraint equations, remembering that the code will try to force
    `cc(i)` to be zero.
  </p>

<p style='text-align: justify;'>
  Remember that if a limit equation is being added, a new f-value iteration
  variable may also need to be added to the code.
</p>
