
# Style Guide
[PDF of webpage](../pdf/standards.pdf)

## Line Length

For optimal readability, a limit of 100 characters for maximum line length has been set. This is below the maximum line length of 132 characters for Fortran (to prevent compilation errors) and prevents long lines that run on past the edge of the screen wasting programmers time with scrolling.

# Coding Standards

## Double declarations

PROCESS uses the Fortran 2008+ intrinsic precision module as shown in the example below. The
use statement will need to be at the module level. See the 
[fortran wiki](http://fortranwiki.org/fortran/show/Real+precision) for more information.

```fortran
use, intrinsic :: iso_fortran_env, only: dp=>real64

real(dp) :: b
!! Variable description

```

## Naming conventions

Please see <a href="https://git.ccfe.ac.uk/process/process/-/issues/939">issue 939</a>.

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

This is a copy 

## Add a figure of merit

To be udated!

## Add a scan variable

After following the instruction to add an input variable, you can make the variable a scan variable by following these steps:

1. Increment the parameter `ipnscnv` defined in the `scan_module` module in the `scan.f90` source file, to accommodate the new scanning variable. The incremented value will identify your scan variable.
2. Add a short description in the new scanning variable in the `nsweep` entry in source in the same source code, alongside its identification number:
3. Update the `scan_select` subroutine in the `scan.f90` source file by adding a new case statement connecting the vaiable to the scan integer switch, a short variable desciption `vlab` (the variable name) and a more explicit variable description `xlab`. Don't forget to add the use only statment at the beginning of `scan_select`.
4. Add a section on the input variable description indicating the scan switch.

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

To be udated!

# Code Documentation Using FORD
PROCESS uses FORD (FORtran Documentation) to automatically generate documentation from comments 
in the FORTRAN code. FORD parses FORTRAN source to understand the structure of the project as well 
as picking up "docmarked" comments in the source to create the documentation.

Regular Fortran comments are prefixed with a "!"; these are ignored by FORD and don't go into 
the documentation. FORD comments are prefixed by a "!!", called a docmark; these are picked up 
by FORD and go into the documentation.

The "!!" docmark goes after the statement it documents. For example, to document variables:

```fortran
real(kind(1.0D0)) :: alphan = 0.25D0
!! Density profile index
real(kind(1.0D0)) :: alphap = 0.0D0
!! Pressure profile index
real(kind(1.0D0)) :: alpharate = 0.0D0
!! Alpha particle production rate (particles/m3/sec)
```

...and to document modules:
```fortran
module global_variables
  !! Module containing miscellaneous global variables
  !! This module contains miscellaneous global variables not
  !! well-suited to any of the other 'variables' modules.
```

This documentation will appear in the 
[FORD docs](http://process.gitpages.ccfe.ac.uk/process/ford_site/index.html) section in the 
left-hand navigation bar. Within this site, the "Variables" section in the top navigation bar 
provides variable descriptions in the same manner as the original "vardes" page. 

To document a statement before it occurs in the source, use "!>". However, it is encouraged to 
use "!!" for consistency. The rationale behind this and further information is included on the 
[FORD wiki](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Documentation).

The FORD project on github can be found [here](https://github.com/Fortran-FOSS-Programmers/ford).

## Example of FORD documentation for a subroutine (constraint equation)

```fortran

subroutine constraint_eqn_001(args)
  !! author: J Morris
  !! category: equality constraint
  !!
  !! Relationship between beta, temperature (keV) and density
  !!
  !! \begin{equation} 
  !! c_i = 1 - \frac{1}{\beta}\left( \beta_{ft} + \beta_{NBI} + 2 \times 10^3 \mu_0 e
  !! \left( \frac{n_e T_e + n_i T_i}{B_{tot}^2} \right) \right)
  !! \end{equation}
  !!
  !! - \( \beta \) -- total plasma beta
  !! - \( \beta_{ft} \) -- fast alpha beta component
  !! - \( \beta_{NBI} \) -- neutral beam beta component
  !! - \( n_e \) -- electron density [m\(^3\)]
  !! - \( n_i \) -- total ion density [m\(^3\)]
  !! - \( T_e \) -- density weighted average electron temperature [keV]
  !! - \( T_i \) -- density weighted average ion temperature [keV]
  !! - \( B_{tot} \) -- total toroidal + poloidal field [T]

  use physics_variables, only: betaft, betanb, dene, ten, dnitot, tin, btot, beta
  use constants, only: echarge,rmu0

  implicit none

  type(constraint_args_type), intent(out) :: args
  !! constraint derived type

    args%cc = 1.0D0 - (betaft + betanb + &
      2.0D3*rmu0*echarge * (dene*ten + dnitot*tin)/btot**2 )/beta
    args%con = beta * (1.0D0 - args%cc)
    args%err = beta * args%cc
    args%symbol = '='
    args%units  = ''

end subroutine constraint_eqn_001

```

Creates:

<img
    src="../../images/ford_example_1.png"
    alt="alt text"
    width="700px"
    >
