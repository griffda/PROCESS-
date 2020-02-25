
# Style Guide

## Line Length



# Coding Standards


## Naming conventions

# Code Documentation Using FORD
PROCESS uses FORD (FORtran Documentation) to automatically generate documentation from comments in the FORTRAN code. FORD parses FORTRAN source to understand the structure of the project as well as picking up "docmarked" comments in the source to create the documentation.

Regular Fortran comments are prefixed with a "!"; these are ignored by FORD and don't go into the documentation. FORD comments are prefixed by a "!!", called a docmark; these are picked up by FORD and go into the documentation.

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

This documentation will appear in the [FORD docs](http://process.gitpages.ccfe.ac.uk/process/ford_site/index.html) section in the left-hand navigation bar. Within this site, the "Variables" section in the top navigation bar provides variable descriptions in the same manner as the original "vardes" page. 

To document a statement before it occurs in the source, use "!>". However, it is encouraged to use "!!" for consistency. The rationale behind this and further information is included on the [FORD wiki](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Documentation).

The FORD project on github can be found [here](https://github.com/Fortran-FOSS-Programmers/ford).