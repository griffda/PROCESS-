# PROCESS
[PDF of webpage](pdf/index.pdf)

<img
    title="UKAEA logo"
    src="../../images/ukaea-logo.png"
    alt="ukaea-logo.png"
    class="logo"
    >

PROCESS is a systems code at [CCFE](http://www.ccfe.ac.uk/) that calculates in a 
self-consistent manner the parameters of a fusion power plant with a specified 
performance, ensuring that its operating limits are not violated, and with the option 
to optimise to a given function of these parameters.
<br><br>

## Rationale

During the course of studies into a proposed fusion power plant there may be times 
when questions of the following type arise:

* Are the machine’s physics and engineering parameters consistent with one another?
* Which machine of a given size and shape produces the cheapest electricity?
* What is the effect of increasing the limit of the maximum plasma density on the 
  amount of auxiliary power required?

Questions such as these are extremely difficult to answer, since the large number of 
parameters involved in designing a fusion power plant are highly dependent on one 
another. Fortunately, computer programs have been written to address these issues, 
and PROCESS is one of them.

Suppose that a power plant design calls for a machine with a given size and 
shape, which will produce a certain net electric power. There may be a vast number of 
different conceptual machines that satisfy the problem as stated so far, and PROCESS 
can be used in “non-optimisation” mode to find one of the concepts whose physics and engineering 
parameters are self-consistent. However, the machine found by PROCESS in this manner may 
not be possible to build in practice — the coils may be overstressed, for instance, or 
the plasma pressure may exceed the maximum possible value. PROCESS contains a large number 
of constraints to prevent the code from finding a machine with such problems and running 
the code in so-called “optimisation” mode forces these constraints to be met. The number 
of possible conceptual machines is thus considerably reduced, and optimisation of the 
parameters with respect to (for example) the cost of electricity will reduce this number to a 
minimum, and hopefully one.

Formally then, PROCESS is a systems code that calculates in a self-consistent manner 
the parameters of a fusion power plant with a specified performance, ensuring that its 
operating limits are not violated, and with the option to optimise a given function of 
these parameters.
<br><br>

## History

PROCESS is derived from several earlier systems codes, but is largely based on the 
TETRA (Tokamak Engineering Test Reactor Analysis) code[^1] and its descendant STORAC 
(Spherical TOrus Reactor Analysis Code)[^2], which include routines relevant to the 
spherical tokamak class of machines.

These codes, and much of the original version of PROCESS itself, were written by 
personnel at Oak Ridge National Laboratory in Tennessee, USA, with contributions from 
several other laboratories in the USA. In addition, many of the mathematical 
routines have been taken from a number of different well-established source libraries.

A great deal of effort was expended upon 
[UKAEA](https://www.gov.uk/government/organisations/uk-atomic-energy-authority) on 
the code’s arrival from ORNL in the early 1990s to upgrade and extend the code, 
including the addition of machines based on the stellerator, reversed field pinch 
and inertial confinement concepts.

PROCESS is actively being developed. This wiki is updated in parallel with the code 
itself to ensure that the documentation remains consistent with the latest version of PROCESS. 
It is to be hoped that it will be of assistance not only to users of PROCESS, but 
to anyone using PROCESS outputs or models based on them.
<br><br>

## Developer Group

- [James Morris](mailto:james.morris2@ukaea.uk)
- [Michael Kovari](mailto:michael.kovari@ukaea.uk)
- [Stuart Muldrew](mailto:stuart.muldrew@ukaea.uk)
- [Alex Pearce](mailto:alex.pearce@ukaea.uk)
- [Sebastien Kahn](mailto:sebastien.kahn@ukaea.uk)
- [Adam Brown](mailto:adam.brown@ukaea.uk)
- [Jonathan Maddock](mailto:jonathan.maddock@ukaea.uk)
<br><br>

## References

Below is a list of PROCESS publications and reference material links:

- [M. Kovari et al., PROCESS: a systems code for fusion power plants - Part 1: 
  Physics](http://www.sciencedirect.com/science/article/pii/S0920379614005961)
- [M. Kovari et al., PROCESS: a systems code for fusion power plants - Part 2: 
Engineering](https://www.sciencedirect.com/science/article/pii/S0920379616300072)
- [H. Lux et al., Implications of uncertainties on European DEMO design, Nuc. Fus. 59 
  (2019)](https://iopscience.iop.org/article/10.1088/1741-4326/ab13e2/meta)
- [J. Morris, at al., Validation and sensitivity of CFETR design using EU systems 
  codes, FED 2019 in press corrected proof](https://www.sciencedirect.com/science/article/pii/S0920379619300341)
- [S.I. Muldrew et al., Uncertainty analysis of an SST-2 fusion reactor design. 
  FED, 2019 in press corrected proof](https://www.sciencedirect.com/science/article/pii/S0920379618308317)
- [S.B.L Chislett-McDonald et al., Could high H98-factor commercial tokamak power plants 
  use Nb–Ti toroidal field coils?, IEEE Transactions on Applied Superconductivity 29 (2019) 
  ](https://ieeexplore.ieee.org/abstract/document/8630091)

A list of publications using PROCESS can be found [here](./publications.md).
<br><br>
  
[^1]: R. L. Reid et al., “ETR/ITER Systems Code”, Oak Ridge Report ORNL/FEDC-87/7 (1988)
[^2]: J. D. Galambos, “STAR Code : Spherical Tokamak Analysis and Reactor Code”, 
Unpublished internal Oak Ridge document.