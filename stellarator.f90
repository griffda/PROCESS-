! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module helias5b_coil_parameters

  !+ad_name  helias5b_coil_parameters
  !+ad_summ  Module containing Helias 5-B power plant parameter values
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_auth  F Warmer, IPP Greifswald
  !+ad_cont  None
  !+ad_args  N/A
  !+ad_desc  This module contains a set of constants that define the
  !+ad_desc  coil set parameters for the Helias 5-B stellarator power plant design.
  !+ad_prob  None
  !+ad_call  N/A
  !+ad_hist  19/11/13 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  HELIAS 5-B magnet system structure and maintenance concept,
  !+ad_docc  Felix Schauer, Konstantin Egorov, Victor Bykov, Fus. Eng. Design (2013),
  !+ad_docc  in press
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Number of coils
  integer, parameter :: tfno5B = 50
  !  Major radius (m)
  real(kind(1.0D0)), parameter :: Rg5B = 22.0D0
  !  Coil minor radius (m), = U/(2*pi), U=34 m
  real(kind(1.0D0)), parameter :: Rk5B = 5.41127D0
  !  Distance (m) from inner torus superconductor to outer torus superconductor
  !  in bean-shaped cross-section
  real(kind(1.0D0)), parameter :: D_coil_5B = 5.2625D0
  !  Coil current (A)
  real(kind(1.0D0)), parameter :: I5B = 13.65D6
  !  Maximum field at superconductor surface (T)
  real(kind(1.0D0)), parameter :: B1 = 12.5D0
  !  Field at magnetic axis (T)
  real(kind(1.0D0)), parameter :: B10 = 5.9D0
  !  Nb3Sn critical field (T)
  real(kind(1.0D0)), parameter :: Bco = 33.0D0
  !  NbTi critical field (T)
  real(kind(1.0D0)), parameter :: Bc2 = 14.2D0
  !  Toroidal width of superconductor coil cross-section (winding pack) (m)
  real(kind(1.0D0)), parameter :: b5B = 0.710D0
  !  Radial width of superconductor coil cross-section (winding pack) (m)
  real(kind(1.0D0)), parameter :: h5B = 0.750D0
  !  Total toroidal width of the coil cross-section (m)
  real(kind(1.0D0)), parameter :: bb5b = 0.920D0
  !  Total radial width of the coil cross-section (m)
  real(kind(1.0D0)), parameter :: hh5b = 1.000D0
  !  Coil winding-pack radius (circular approximation) (m), = sqrt(h5B*b5B/pi)
  real(kind(1.0D0)), parameter :: RQ5B = 0.41170379D0
  !  Stored coil energy (GJ)
  real(kind(1.0D0)), parameter :: W5B = 160.0D0

  ! ~ 1.6626 m / 1.8 m  == r_theta0_5B / rminor_5B
  real(kind(1.0D0)), parameter :: k0 = 0.4618D0
  ! ~ 13.65 MA / 5.9 T  == I_5B / B0_5B
  real(kind(1.0D0)), parameter :: k1 = 2.31356D0
  ! ~ 160 GJ / (5.9 T)^2  == W_mag_5B / B0_5B^2
  real(kind(1.0D0)), parameter :: k2 = 4.59638D0
  ! ~ 11500 tonnes / 160 GJ  == M_struc_5B / W_mag_5B
  real(kind(1.0D0)), parameter :: k3 = 71.875D0
  ! ~ 7.6 m / 6.2 m  == width_max_5B / w_coil_5B
  real(kind(1.0D0)), parameter :: k4 = 1.2233D0
  ! ~ 12 m / 6.2 m  == height_max_5B / w_coil_5B
  real(kind(1.0D0)), parameter :: k5 = 1.9316D0
  ! ~ 34.14 m / 6.2 m  == U_avg_5B / w_coil_5B
  real(kind(1.0D0)), parameter :: k6 = 5.4954D0

end module helias5b_coil_parameters

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module cartesian_vectors

  !+ad_name  cartesian_vectors
  !+ad_summ  Module providing Cartesian vectors and associated operations
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  None
  !+ad_args  N/A
  !+ad_desc  This module defines a vector datatype within Cartesian coordinates,
  !+ad_desc  and provides a set of basic vector operators.
  !+ad_prob  None
  !+ad_call  N/A
  !+ad_hist  19/11/13 PJK Initial version
  !+ad_stat  Okay
  !+ad_docs  None
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  private
  public :: vector
  public :: operator(+), operator(-)
  public :: operator(.dot.), operator(.cross.)
  public :: modulus, unit_vector

  !  Declare vector type

  type :: vector
     real(kind(1.0D0)) :: x  !  Magnitude in x direction
     real(kind(1.0D0)) :: y  !  Magnitude in y direction
     real(kind(1.0D0)) :: z  !  Magnitude in z direction
  end type vector

  !  Functions

  interface operator (+)
     module procedure add_vectors
  end interface

  interface operator (-)
     module procedure subtract_vectors
  end interface

  interface operator (*)
     module procedure scale_vector
  end interface

  interface operator (/)
     module procedure divide_vector
  end interface

  interface operator (.dot.)
     module procedure dot_product
  end interface

  interface operator (.cross.)
     module procedure cross_product
  end interface

contains

  elemental function add_vectors(a,b)
    type (vector) :: add_vectors
    type (vector), intent(in) :: a,b
    add_vectors%x = a%x + b%x
    add_vectors%y = a%y + b%y
    add_vectors%z = a%z + b%z
  end function add_vectors

  elemental function subtract_vectors(a,b)
    type (vector) :: subtract_vectors
    type (vector), intent(in) :: a,b
    subtract_vectors%x = a%x - b%x
    subtract_vectors%y = a%y - b%y
    subtract_vectors%z = a%z - b%z
  end function subtract_vectors

  elemental function scale_vector(a,b)
    type (vector) :: scale_vector
    type (vector), intent(in) :: a
    real(kind(1.0D0)), intent(in) :: b
    scale_vector%x = a%x * b
    scale_vector%y = a%y * b
    scale_vector%z = a%z * b
  end function scale_vector

  elemental function divide_vector(a,b)
    type (vector) :: divide_vector
    type (vector), intent(in) :: a
    real(kind(1.0D0)), intent(in) :: b
    divide_vector%x = a%x / b
    divide_vector%y = a%y / b
    divide_vector%z = a%z / b
  end function divide_vector

  elemental function dot_product(a,b)
    real(kind(1.0D0)) :: dot_product
    type (vector), intent(in) :: a,b
    dot_product = (a%x * b%x) + (a%y * b%y) + (a%z * b%z)
  end function dot_product

  elemental function cross_product(a,b)
    type (vector) :: cross_product
    type (vector), intent(in) :: a,b
    cross_product%x = (a%y * b%z) - (a%z * b%y)
    cross_product%y = (a%z * b%x) - (a%x * b%z)
    cross_product%z = (a%x * b%y) - (a%y * b%x)
  end function cross_product

  elemental function modulus(a)
    real(kind(1.0D0)) :: modulus
    type (vector), intent(in) :: a
    modulus = sqrt(a%x*a%x + a%y*a%y + a%z*a%z)
  end function modulus

  elemental function unit_vector(a)
    type (vector) :: unit_vector
    type (vector), intent(in) :: a
    unit_vector = a / modulus(a)
  end function unit_vector

end module cartesian_vectors

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module stellarator_module

  !+ad_name  stellarator_module
  !+ad_summ  Module containing stellarator routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  stcall
  !+ad_cont  stinit
  !+ad_cont  stgeom
  !+ad_cont  stbild
  !+ad_cont  stphys
  !+ad_cont  stheat
  !+ad_cont  stcoil
  !+ad_cont  stfwbs
  !+ad_cont  stdlim
  !+ad_cont  stblim
  !+ad_cont  stigma
  !+ad_cont  stout
  !+ad_cont  ststrc
  !+ad_cont  stdiv
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of the first wall, blanket and shield components
  !+ad_desc  of a fusion power plant.
  !+ad_prob  None
  !+ad_call  availability_module
  !+ad_call  build_variables
  !+ad_call  buildings_module
  !+ad_call  constants
  !+ad_call  costs_module
  !+ad_call  cost_variables
  !+ad_call  current_drive_module
  !+ad_call  current_drive_variables
  !+ad_call  divertor_module
  !+ad_call  divertor_variables
  !+ad_call  error_handling
  !+ad_call  fwbs_module
  !+ad_call  fwbs_variables
  !+ad_call  global_variables
  !+ad_call  heat_transport_variables
  !+ad_call  impurity_radiation_module
  !+ad_call  kit_blanket_model
  !+ad_call  maths_library
  !+ad_call  numerics
  !+ad_call  pfcoil_variables
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  plasma_geometry_module
  !+ad_call  power_module
  !+ad_call  process_output
  !+ad_call  rfp_variables
  !+ad_call  sctfcoil_module
  !+ad_call  stellarator_variables
  !+ad_call  structure_module
  !+ad_call  structure_variables
  !+ad_call  tfcoil_variables
  !+ad_call  times_variables
  !+ad_call  vacuum_module
  !+ad_hist  31/10/12 PJK Initial version of module
  !+ad_hist  06/11/12 PJK Added availability_module
  !+ad_hist  06/11/12 PJK Added plasma_geometry_module
  !+ad_hist  14/08/13 PJK Added cost_variables, kit_blanket_model
  !+ad_hist  24/02/14 PJK Added profiles_module
  !+ad_hist  14/05/14 PJK Added impurity_radiation_module
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use availability_module
  use build_variables
  use buildings_module
  use constants
  use costs_module
  use cost_variables
  use current_drive_module
  use current_drive_variables
  use divertor_module
  use divertor_variables
  use error_handling
  use fwbs_module
  use fwbs_variables
  use global_variables
  use heat_transport_variables
  use impurity_radiation_module
  use kit_blanket_model
  use maths_library
  use numerics
  use pfcoil_variables
  use physics_module
  use physics_variables
  use plasma_geometry_module
  use power_module
  use process_output
  use profiles_module
  use rfp_variables
  use sctfcoil_module
  use stellarator_variables
  use structure_module
  use structure_variables
  use tfcoil_variables
  use times_variables
  use vacuum_module

  implicit none

  private
  public :: stcall, stinit, stout

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcall

    !+ad_name  stcall
    !+ad_summ  Routine to call the physics and engineering modules
    !+ad_summ  relevant to stellarators
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine is the caller for the stellarator models.
    !+ad_prob  None
    !+ad_call  acpow
    !+ad_call  avail
    !+ad_call  bldgcall
    !+ad_call  costs
    !+ad_call  power1
    !+ad_call  power2
    !+ad_call  stbild
    !+ad_call  stcoil
    !+ad_call  stdiv
    !+ad_call  stfwbs
    !+ad_call  stgeom
    !+ad_call  stphys
    !+ad_call  ststrc
    !+ad_call  tfpwr
    !+ad_call  vaccall
    !+ad_hist  28/06/94 PJK Initial version
    !+ad_hist  23/01/97 PJK Split routine POWER into POWER1 and POWER2
    !+ad_hist  19/11/97 PJK Corrected call to STCOIL (missing arguments)
    !+ad_hist  19/05/99 PJK Added call to routine AVAIL
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added costs_module
    !+ad_hist  17/10/12 PJK Added divertor_module
    !+ad_hist  18/10/12 PJK Added fwbs_module
    !+ad_hist  18/10/12 PJK Added vacuum_module
    !+ad_hist  30/10/12 PJK Added power_module
    !+ad_hist  30/10/12 PJK Added buildings_module
    !+ad_hist  12/08/13 PJK Removed call to (tokamak) geomty
    !+ad_hist  13/08/13 PJK/FW Added call to new stellarator divertor model
    !+ad_hist  14/08/13 PJK/FW Removed call to (tokamak) fwbs
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call stgeom
    call stbild(nout,0)
    call stphys
    call stcoil(nout,0)
    call ststrc(nout,0)
    call stfwbs(nout,0)
    call stdiv(nout,0)
    call tfpwr(nout,0)
    call power1
    call vaccall(nout,0)
    call bldgcall(nout,0)
    call acpow(nout,0)
    call power2(nout,0)
    call avail(nout,0)
    call costs(nout,0)

  end subroutine stcall

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stinit

    !+ad_name  stinit
    !+ad_summ  Routine to initialise the variables relevant to stellarators
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine initialises the variables relevant to stellarators.
    !+ad_desc  Many of these may override the values set in routine
    !+ad_desc  <A HREF="initial.html">initial</A>.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  28/06/94 PJK Initial version
    !+ad_hist  09/09/94 PJK Changed ICASE
    !+ad_hist  07/12/94 PJK Changed default q and kappa values
    !+ad_hist  04/12/95 PJK Ensured stellarators do not use D-He3 reaction
    !+ad_hist  26/02/96 PJK Modified initial setting of ISTELL (moved to
    !+ad_hisc               routine DEVTYP)
    !+ad_hist  22/01/97 PJK Subsumed heattr.h, heatrinp.h and pfelect.h into
    !+ad_hisc               htpwr.h
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  10/10/12 PJK Modified to use new numerics module
    !+ad_hist  15/10/12 PJK Added global_variables module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  31/10/12 PJK Added stellarator_variables
    !+ad_hist  23/01/13 PJK Turned off some output sections
    !+ad_hist  12/08/13 PJK/FW Changed kappa values to 1.0
    !+ad_hist  11/09/13 PJK Removed idhe3 setting
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  24/06/14 PJK Removed refs to bucking cylinder
    !+ad_hist  23/07/14 PJK Changed icase description
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (istell == 0) return

    !  Numerics quantities

    boundl(1) = 5.0D0

    boundu(1) = 20.0D0
    boundu(3) = 30.0D0
    boundu(29) = 20.0D0

    icase = 'Stellarator model'

    !  Build quantities

    ohcth = 0.0D0
    iohcl = 0
    ohhghf = 0.0D0
    gapoh = 0.0D0
    tfootfi = 1.0D0

    !  Physics quantities

    aspect = 12.5D0
    dnbeta = 0.0D0
    rmajor = 20.0D0
    kappa = 1.0D0
    kappa95 = 1.0D0
    triang = 0.0D0
    q = 1.03D0

    !  Turn off current drive

    irfcd = 0

    !  Times for different phases

    tburn = 3.15576D7  !  one year
    tohs = 0.0D0
    tpulse = 3.15576D7  !  one year
    tqnch = 0.0D0
    tramp = 0.0D0

    !  Coil quantities

    tfno = 50.0D0

  end subroutine stinit

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stgeom

    !+ad_name  stgeom
    !+ad_summ  Routine to calculate the plasma volume and surface area for
    !+ad_summ  a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the plasma volume and surface area for
    !+ad_desc  a stellarator configuration.
    !+ad_desc  <P>The method is based on that described in Geiger, and
    !+ad_desc  has been converted from MATLAB code written by Felix Warmer.
    !+ad_prob  The surface area integration is very slow; a different method
    !+ad_prob  may be implemented in the future.
    !+ad_call  None
    !+ad_hist  28/06/94 PJK Initial version
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  12/08/13 PJK/FW Implementation of full model
    !+ad_stat  Okay
    !+ad_docs  Stellarator Plasma Geometry Model for the Systems
    !+ad_docc  Code PROCESS, F. Warmer, 19/06/2013
    !+ad_docs  J. Geiger, IPP Greifswald internal document:  'Darstellung von
    !+ad_docc  ineinandergeschachtelten toroidal geschlossenen Flächen mit
    !+ad_docc  Fourierkoeffizienten' ('Representation of nested, closed
    !+ad_docc  surfaces with Fourier coefficients')
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    integer, parameter :: n_i = 12, n_j = 25
    integer, parameter :: m_max = n_i-1, n_max = (n_j-1)/2
    integer, parameter :: nu = 200, nv = 200
    integer, save :: nf_vmec
    integer :: i,j,m,n,iu,iv,nf,m1,n1,m2,n2,a1,b1,a2,b2,a3,b3,a4,b4
    real(kind(1.0D0)), dimension(0:m_max,-n_max:n_max), save :: Rmn0, Zmn0
    real(kind(1.0D0)), dimension(0:m_max,-n_max:n_max) :: Rmn, Zmn
    real(kind(1.0D0)), dimension(4*m_max+1,4*n_max+1) :: Rv, Zv
    real(kind(1.0D0)), save :: r_vmec,a_vmec,aspect_vmec,v_vmec,s_vmec
    real(kind(1.0D0)) :: a,a_square,sr,sa,vvv,r_maj,du,dv,rr,drdv,drdu
    real(kind(1.0D0)) :: dzdv,dzdu,rtemp,sum1,sum2,rn,u,v,suv,cuv
    character(len=80) :: header
    logical :: first_call = .true.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (first_call) then

       !  VMEC information file

       open(unit=1,file=vmec_info_file,status='old')
       read(1,'(A)') header
       read(1,*) R_vmec, a_vmec, aspect_vmec, V_vmec, S_vmec, rn
       close(unit=1)

       nf_vmec = int(rn)

       !  Import VMEC boundary Fourier coefficients from files.
       !  These contain the plasma boundary represented by Fourier components
       !  R(m,n) and Z(m,n)
       !
       !  Fortran complication: array sizes n_i,n_j,m_max,n_max are needed in advance...

       !  Rmn Fourier components

       open(unit=1,file=vmec_rmn_file,status='old')
       do m = 0,m_max
          read(1,*) (Rmn0(m,n),n=-n_max,n_max)
       end do
       close(unit=1)

       !  Zmn Fourier components

       open(unit=1,file=vmec_zmn_file,status='old')
       do m = 0,m_max
          read(1,*) (Zmn0(m,n),n=-n_max,n_max)
       end do
       close(unit=1)

       first_call = .false.
    end if

    Rmn = Rmn0
    Zmn = Zmn0

    rminor = rmajor/aspect
    eps = 1.0D0/aspect

    !  Number of field periods; this is always 5 for the W7-X line,
    !  but a 4 periodic machine could be possible in the future

    nf = nf_vmec

    !  Scale major and minor radius
    !  with scaling factors SR = R / R_vmec for the major radius
    !                   and Sa = a / a_vmec for the minor radius
    !
    !  If a certain 'R' shall be used, calculate R_vmec in advance
    !  and define SR not as factor, but as given above

    SR = rmajor/R_vmec
    Sa = rminor/a_vmec

    do n = -n_max,n_max
       do m = 0,m_max

          if (m == 0) then
             Rmn(m,n) = SR*Rmn(m,n)
             Zmn(m,n) = SR*Zmn(m,n)
          else
             Rmn(m,n) = Sa*Rmn(m,n)
             Zmn(m,n) = Sa*Zmn(m,n)           
          end if

       end do
    end do

    !  Calculate effective minor radius
    !  This is an average value integrated over toroidal angle

    a_square = 0.0D0
    do n = -n_max,n_max
       do m = 0,m_max
          a_square = a_square + m*Zmn(m,n)*Rmn(m,n)
       end do
    end do

    a = sqrt(a_square)

    !  Calculation of the plasma volume
    !
    !  Expand array and set not given fourier components to zero.
    !  This is required because the volume is calculated with
    !  a double sum over m1,n1 and m2,n2 resulting in mixed
    !  mode numbers m*=m1+m2, etc. 
    !  Therefore m* > m_max meaning, that the sum goes over mode numbers,
    !  which are not given by the input file - but these components can simply
    !  be set to zero

    Rv = 0.0D0 ; Zv = 0.0D0
    do j = n_max+1,3*n_max+1
       do i = 2*m_max+1,3*m_max+1
          Rv(i,j) = Rmn(i-2*m_max-1,j-2*n_max-1)
          Zv(i,j) = Zmn(i-2*m_max-1,j-2*n_max-1)
       end do
    end do

    !  Double summation over m1,n1 and m2,n2 to calculate the volume

    vol = 0.0D0

    do n1 = -n_max,n_max
       do m1 = 0,m_max

          rtemp = Rmn(m1,n1)

          do n2 = -n_max,n_max
             do m2 = 0,m_max

                a1 = m1+m2+1+2*m_max
                b1 = n1+n2+1+2*n_max

                a2 = m1-m2+1+2*m_max
                b2 = n1-n2+1+2*n_max

                a3 = m2-m1+1+2*m_max
                b3 = n2-n1+1+2*n_max

                a4 = -m1-m2+1+2*m_max
                b4 = -n1-n2+1+2*n_max

                sum1 = Zv(a1,b1) - Zv(a2,b2) + Zv(a3,b3) - Zv(a4,b4)
                sum2 = Rv(a1,b1) + Rv(a2,b2) + Rv(a3,b3) + Rv(a4,b4)

                vol = vol + rtemp*( (m2*Rmn(m2,n2)*sum1) + (m2*Zmn(m2,n2)*sum2) )

             end do
          end do
       end do
    end do

    vol = vol * pi*pi/3.0D0

    !  This is an average value using 'a' as calculated above

    R_maj = vol / (2.0D0*a*a*pi*pi)

    !  Calculation of the surface area of the LCFS
    !
    !  The integral cannot be simplified with analytical methods
    !  as was done for the volume and poloidal surface
    !  therefore it must be numerically 'integrated' over the poloidal and
    !  toroidal angles; calculation time depends strongly on the fineness of the
    !  grid 'nu' and 'nv'
    !
    !  I just dissolved the integral in the most simple Riemann sum, which is
    !  very inaccurate, even at 400 points, which takes a few seconds only <1%

    du = 2.0D0*pi/(nu-1)
    dv = 2.0D0*pi/(nv-1)

    sarea = 0.0D0
    do iu = 1,nu
       u = (iu-1)*du  !  poloidal angle
       do iv = 1,nv
          v = (iv-1)*dv  !  toroidal angle

          RR = 0.0D0
          dRdv = 0.0D0 ; dRdu = 0.0D0 ; dZdv = 0.0D0 ; dZdu = 0.0D0

          do n = -n_max,n_max
             do m = 0,m_max
                suv = sin(m*u - nf*n*v)
                cuv = cos(m*u - nf*n*v)
                RR = RR + Rmn(m,n)*cos(m*u - n*v)
                dRdv = dRdv + nf*n*Rmn(m,n)*suv
                dRdu = dRdu -    m*Rmn(m,n)*suv
                dZdv = dZdv - nf*n*Zmn(m,n)*cuv
                dZdu = dZdu +    m*Zmn(m,n)*cuv
             end do
          end do

          sarea = sarea + sqrt( (RR**2)*(dZdu**2) + &
               ((dRdv*dZdu)-(dZdv*dRdu))**2 + &
               (RR**2)*(dRdu**2) ) * du*dv

       end do
    end do

    !  Cross-sectional area, averaged over toroidal angle

    xarea = pi*a*a  ! average, could be calculated for every toroidal angle if desired

    !  sareao is retained only for obsolescent fispact calculation...

    sareao = 0.5D0*sarea  !  Used only in the divertor model; approximate as for tokamaks

  end subroutine stgeom

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stbild(outfile,iprint)

    !+ad_name  stbild
    !+ad_summ  Routine to determine the build of a stellarator machine
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine determines the build of the stellarator machine.
    !+ad_desc  The values calculated are based on the mean minor radius, etc.,
    !+ad_desc  as the actual radial and vertical build thicknesses vary with
    !+ad_desc  toroidal angle.
    !+ad_prob  None
    !+ad_call  obuild
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_hist  29/06/94 PJK Initial version
    !+ad_hist  10/06/96 PJK Added first wall area calculation
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  15/05/13 PJK Swapped build order of vacuum vessel and gap
    !+ad_hist  12/08/13 PJK/FW Better approximation for fwarea
    !+ad_hist  25/09/13 PJK Removed port size output
    !+ad_hist  07/11/13 PJK Corrected blanket/shield thicknesses if blktmodel > 0
    !+ad_hist  03/03/14 PJK tfootfi no longer used to calculate tfthko
    !+ad_hist  24/04/14 PJK Calculation proceeds irrespective of iprint
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  24/06/14 PJK Removed refs to bcylth;
    !+ad_hisc               blnktth now always calculated
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: drbild,radius,awall

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate total blanket thicknesses if blktmodel > 0

    if (blktmodel > 0) then
       blnkith = blbuith + blbmith + blbpith
       blnkoth = blbuoth + blbmoth + blbpoth
       shldtth = 0.5D0*(shldith+shldoth)
    end if

    !  Top/bottom blanket thickness

    blnktth = 0.5D0*(blnkith+blnkoth)

    !  Radial build to centre of plasma (should be equal to rmajor)

    rbld = bore + ohcth + gapoh + tfcth + gapds + &
         ddwi + shldith + blnkith + fwith + scrapli + rminor

    !  Radius to inner edge of inboard shield

    rsldi = rmajor - rminor - scrapli - fwith - blnkith - shldith

    !  Radius to outer edge of outboard shield

    rsldo = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth

    !  Thickness of outboard TF coil legs

    tfthko = tfcth 

    !  Radius to centre of outboard TF coil legs

    gapsto = gapomin
    rtot = rsldo + ddwi + gapsto + 0.5D0*tfthko

    !  Height to inside edge of TF coil
    !  Roughly equal to average of (inboard build from TF coil to plasma
    !  centre) and (outboard build from plasma centre to TF coil)

    hmax = 0.5D0 * ( &
         (gapds+ddwi+shldith+blnkith+fwith+scrapli+rminor) + &
         (rminor+scraplo+fwoth+blnkoth+shldoth+ddwi+gapsto) )

    !  Outer divertor strike point radius, set equal to major radius

    rstrko = rmajor

    !  First wall area: scales with minor radius
    !  (c.f. area ~ 2.pi.R * 2.pi.a)

    !  Old method
    !fwarea = 4.0D0*pi**2*sf*rmajor*(rminor+(scrapli+scraplo)/2.0D0) &
    !     * 0.875D0

    awall = rminor + 0.5D0*(scrapli + scraplo)
    fwarea = sarea * awall/rminor

    if (iprint == 0) return

    !  Print out device build

    call oheadr(outfile,'Radial Build')

    write(outfile,10)
10  format(t43,'Thickness (m)',t60,'Radius (m)')

    radius = 0.0D0
    call obuild(outfile,'Device centreline',0.0D0,radius)

    drbild = bore + ohcth + gapoh
    radius = radius + drbild
    call obuild(outfile,'Machine bore',drbild,radius)

    radius = radius + tfcth
    call obuild(outfile,'TF coil inboard leg',tfcth,radius)

    radius = radius + gapds
    call obuild(outfile,'Gap',gapds,radius)

    radius = radius + ddwi
    call obuild(outfile,'Vacuum vessel',ddwi,radius)

    radius = radius + shldith
    call obuild(outfile,'Inboard shield',shldith,radius)

    radius = radius + blnkith
    call obuild(outfile,'Inboard blanket',blnkith,radius)

    radius = radius + fwith
    call obuild(outfile,'Inboard first wall',fwith,radius)

    radius = radius + scrapli
    call obuild(outfile,'Inboard scrape-off',scrapli,radius)

    radius = radius + rminor
    call obuild(outfile,'Plasma geometric centre',rminor,radius)

    radius = radius + rminor
    call obuild(outfile,'Plasma outboard edge',rminor,radius)

    radius = radius + scraplo
    call obuild(outfile,'Outboard scrape-off',scraplo,radius)

    radius = radius + fwoth
    call obuild(outfile,'Outboard first wall',fwoth,radius)

    radius = radius + blnkoth
    call obuild(outfile,'Outboard blanket',blnkoth,radius)

    radius = radius + shldoth
    call obuild(outfile,'Outboard shield',shldoth,radius)

    radius = radius + ddwi
    call obuild(outfile,'Vacuum vessel',ddwi,radius)

    radius = radius + gapsto
    call obuild(outfile,'Gap',gapsto,radius)

    radius = radius + tfthko
    call obuild(outfile,'TF coil outboard leg',tfthko,radius)

  end subroutine stbild

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stphys

    !+ad_name  stphys
    !+ad_summ  Routine to calculate stellarator plasma physics information
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine calculates the physics quantities relevant to
    !+ad_desc  a stellarator device.
    !+ad_prob  None
    !+ad_call  beamfus
    !+ad_call  betcom
    !+ad_call  palph
    !+ad_call  palph2
    !+ad_call  pcond
    !+ad_call  phyaux
    !+ad_call  radpwr
    !+ad_call  rether
    !+ad_call  stblim
    !+ad_call  stdlim
    !+ad_call  stheat
    !+ad_hist  29/06/94 PJK Initial version
    !+ad_hist  16/01/96 PJK Modifications in the light of D-He3 changes
    !+ad_hisc               (idhe3 is always set to zero for stellarators)
    !+ad_hist  10/06/96 PJK Added use of IWALLD in wall load calculation
    !+ad_hist  01/04/98 PJK Modified BETCOM and PCOND calls
    !+ad_hist  01/04/98 PJK Modified BETCOM call
    !+ad_hist  24/04/98 PJK Modified BETCOM call
    !+ad_hist  30/06/98 PJK Modified PCOND call
    !+ad_hist  19/01/99 PJK Modified PCOND call
    !+ad_hist  16/07/01 PJK Modified PCOND call
    !+ad_hist  22/05/06 PJK Modified PALPH2 call
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added physics_module
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  30/10/12 PJK Added times_variables
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  17/12/12 PJK Added zfear to betcom, radpwr argument lists
    !+ad_hist  23/01/13 PJK Modified poloidal field calculation to use iotabar;
    !+ad_hisc               Changed PCOND q95 argument to iotabar
    !+ad_hist  12/06/13 PJK taup now global
    !+ad_hist  14/08/13 PJK/FW New definition for plrad, using f_rad
    !+ad_hist  10/09/13 PJK Modified calls to PALPH, PHYAUX
    !+ad_hist  11/09/13 PJK Removed idhe3, ftr, iiter usage
    !+ad_hist  28/11/13 PJK Added pdtpv, pdhe3pv, pddpv to palph arguments
    !+ad_hist  13/02/14 PJK Added tratio usage to calculate ti from te
    !+ad_hist  19/02/14 PJK Added plasma_profiles call and made other
    !+ad_hisc               necessary changes
    !+ad_hist  08/05/14 PJK Modified call to PHYAUX
    !+ad_hist  14/05/14 PJK Added call to plasma_composition and new
    !+ad_hisc               impurity radiation calculations
    !+ad_hist  15/05/14 PJK Removed ffwal from iwalld=2 calculation
    !+ad_hist  20/05/14 PJK Cleaned up radiation power usage
    !+ad_hist  21/05/14 PJK Added ignite clause to powht calculation
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  11/06/14 PJK Introduced pchargemw, ptremw, ptrimw
    !+ad_hist  24/06/14 PJK Corrected neutron wall load to account for gaps
    !+ad_hisc               in first wall
    !+ad_hist  19/08/14 PJK Removed impfe usage
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: fusrat,pddpv,pdtpv,pdhe3pv,powht,sbar,sigvdt,zion

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate plasma composition

    if (imprad_model == 0) then
       call betcom(cfe0,dene,fdeut,ftrit,fhe3,ftritbm,ignite,impc,impo, &
            ralpne,rnbeam,te,zeff,abeam,afuel,aion,deni,dlamee,dlamie,dnalp, &
            dnbeam,dnitot,dnprot,dnz,falpe,falpi,rncne,rnone,rnfene,zeffai, &
            zion,zfear)
    else
       call plasma_composition
    end if

    !  Calculate density and temperature profile quantities

    call plasma_profiles

    q95 = q

    !  Calculate poloidal field using rotation transform

    bp = rminor * bt / rmajor * iotabar

    !  Total field

    btot = sqrt(bt**2 + bp**2)

    !  Poloidal beta

    betap = beta * ( btot/bp )**2

    !  Perform auxiliary power calculations

    call stheat(nout,0)

    !  Calculate fusion power

    call palph(alphan,alphat,deni,fdeut,fhe3,ftrit,ti,palppv,pchargepv,pneutpv, &
         sigvdt,fusionrate,alpharate,protonrate,pdtpv,pdhe3pv,pddpv)

    pdt = pdtpv * vol
    pdhe3 = pdhe3pv * vol
    pdd = pddpv * vol

    !  Calculate neutral beam slowing down effects
    !  If ignited, then ignore beam fusion effects

    if ((pnbeam /= 0.0D0).and.(ignite == 0)) then
       call beamfus(beamfus0,betbm0,bp,bt,cnbeam,dene,deni,dlamie, &
            ealphadt,enbeam,fdeut,ftrit,ftritbm,sigvdt,ten,tin,vol, &
            zeffai,betanb,dnbeam2,palpnb)
       fusionrate = fusionrate + 1.0D6*palpnb / (1.0D3*ealphadt*echarge) / vol
       alpharate = alpharate + 1.0D6*palpnb / (1.0D3*ealphadt*echarge) / vol
    end if

    pdt = pdt + 5.0D0*palpnb

    call palph2(bt,bp,dene,deni,dnitot,falpe,falpi,palpnb, &
         ifalphap,pchargepv,pneutpv,ten,tin,vol,palpmw,pneutmw,pchargemw, &
         betaft,palppv,palpipv,palpepv,pfuscmw,powfmw)

    !  Neutron wall load

    if (iwalld == 1) then
       wallmw = ffwal * pneutmw / sarea
    else
       if (ipowerflow == 0) then
          wallmw = (1.0D0-fhole)*pneutmw / fwarea
       else
          wallmw = (1.0D0-fhole-fhcd-fdiv)*pneutmw / fwarea
       end if
    end if

    !  Calculate ion/electron equilibration power

    call rether(alphan,alphat,dene,dlamie,te,ti,zeffai,piepv)

    !  Calculate radiation power
    !  N.B. pedgeradpv is recalculated below

    call radpwr(imprad_model,pbrempv,plinepv,psyncpv, &
         pcoreradpv,pedgeradpv,pradpv)

    pcoreradmw = pcoreradpv*vol
    pradmw = pradpv*vol

    !  Heating power to plasma (= Psol in divertor model)
    !  Ohmic power is zero in a stellarator

    powht = palpmw + pchargemw + pohmmw - pcoreradmw
    if (ignite == 0) powht = powht + pinjmw

    !  Line radiation power/volume is obtained via input parameter f_rad
    !  (in contrast to tokamak calculation)

    pedgeradpv = f_rad*powht/vol
    pedgeradmw = pedgeradpv*vol

    !  Power to divertor, = (1-f_rad)*Psol

    pdivt = powht - pedgeradmw

    !  The following line is unphysical, but prevents -ve sqrt argument
    !  Should be obsolete if constraint eqn 17 is turned on (but beware -
    !  this may not be quite correct for stellarators)

    pdivt = max(0.001D0, pdivt)

    !  Calculate density limit

    call stdlim(alphan,bt,powht,rmajor,rminor,dnelimt)

    !  Calculate transport losses and energy confinement time using the
    !  chosen scaling law
    !  N.B. iotabar replaces tokamak q95 in argument list

    call pcond(afuel,palpmw,aspect,bt,dnitot,dene,dnla,eps,hfact, &
         iinvqd,isc,ignite,kappa,kappa95,kappaa,pchargemw,pinjmw, &
         plascur,pohmpv,pcoreradpv,rmajor,rminor,te,ten,tin,iotabar,qstar,vol, &
         xarea,zeff,ptrepv,ptripv,tauee,tauei,taueff,powerht)

    ptremw = ptrepv*vol
    ptrimw = ptripv*vol

    !  Calculate auxiliary physics related information
    !  for the rest of the code

    sbar = 1.0D0
    call phyaux(aspect,dene,deni,fusionrate,alpharate,plascur,sbar,dnalp, &
         dnprot,taueff,vol,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

    !  Calculate beta limit

    call stblim(betalim)

  end subroutine stphys

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stheat(outfile,iprint)

    !+ad_name  stheat
    !+ad_summ  Routine to calculate the auxiliary heating power
    !+ad_summ  in a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the auxiliary heating power for
    !+ad_desc  a stellarator device.
    !+ad_prob  None
    !+ad_call  culnbi
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_call  report_error
    !+ad_hist  29/06/94 PJK Initial version
    !+ad_hist  01/04/98 PJK Modified call to CULNBI
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  17/10/12 PJK Added current_drive_module
    !+ad_hist  31/10/12 PJK Added stellarator_variables
    !+ad_hist  23/01/13 PJK Added comment about ignited plasma
    !+ad_hist  11/09/13 PJK Changed ftr to ftritbm
    !+ad_hist  25/09/13 PJK Added nbshield, rtanbeam, rtanmax outputs
    !+ad_hist  27/11/13 PJK Added ohmic power to bigq denominator
    !+ad_hist  24/02/14 PJK Modified arguments to CULNBI
    !+ad_hist  24/04/14 PJK Calculation proceeds irrespective of iprint
    !+ad_hist  01/05/14 PJK Changed bigq description
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  06/10/14 PJK Use global nbshinef instead of local fshine
    !+ad_hisc               introduced porbitlossmw
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !+ad_docs  AEA FUS 172: Physics Assessment for the European Reactor Study
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)), save :: effnbss,fpion

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Assign heating power to the desired mechanism

    select case (isthtr)

    case (1)  !  Electron cyclotron resonance heating

       echpwr = pheat
       pinjimw = 0.0D0
       pinjemw = echpwr

    case (2)  !  Lower Hybrid heating

       plhybd = pheat
       pinjimw = 0.0D0
       pinjemw = plhybd

    case (3)  !  Neutral beam injection heating

       !  Use routine described in AEA FUS 172, but discard the current
       !  drive efficiency as this is irrelevant for stellarators. We are
       !  only really interested in fpion, nbshinef and taubeam.

       call culnbi(effnbss,fpion,nbshinef)

       pnbeam = pheat * (1.0D0-forbitloss)
       porbitlossmw = pheat * forbitloss
       pinjimw = pnbeam * fpion
       pinjemw = pnbeam * (1.0D0-fpion)

    case default

       idiags(1) = isthtr ; call report_error(107)

    end select

    !  Total injected power

    pinjmw = pinjemw + pinjimw

    !  Calculate neutral beam current

    if (abs(pnbeam) > 1.0D-8) then
       cnbeam = 1.0D-3 * (pnbeam*1.0D6) / enbeam
    else
       cnbeam = 0.0D0
    end if

    !  Ratio of fusion to input (injection+ohmic) power

    if (abs(pinjmw + porbitlossmw + pohmmw) < 1.0D-6) then
       bigq = 1.0D18
    else
       bigq = powfmw / (pinjmw + porbitlossmw + pohmmw)
    end if

    if (iprint == 0) return

    !  Output section

    call oheadr(outfile,'Auxiliary Heating System')

    select case (isthtr)
    case (1)
       call ocmmnt(outfile,'Electron Cyclotron Resonance Heating')
    case (2)
       call ocmmnt(outfile,'Lower Hybrid Heating')
    case (3)
       call ocmmnt(outfile,'Neutral Beam Injection Heating')
    end select
    if (ignite == 1) then
       call ocmmnt(outfile, &
            'Ignited plasma; injected power only used for start-up phase')
    end if
    call oblnkl(outfile)

    call ovarre(outfile,'Auxiliary power supplied to plasma (MW)', &
         '(pheat)',pheat)
    call ovarre(outfile,'Fusion gain factor Q','(bigq)',bigq)

    if (abs(pnbeam) > 1.0D-8) then
       call ovarre(outfile,'Neutral beam energy (keV)','(enbeam)',enbeam)
       call ovarre(outfile,'Neutral beam current (A)','(cnbeam)',cnbeam)
       call ovarre(outfile,'Fraction of beam energy to ions','(fpion)', &
            fpion)
       call ovarre(outfile,'Neutral beam shine-through fraction','(nbshinef)', &
            nbshinef)
       call ovarre(outfile,'Neutral beam orbit loss power (MW)','(porbitlossmw)', &
            porbitlossmw)
       call ovarre(outfile,'Beam duct shielding thickness (m)','(nbshield)',nbshield)
       call ovarre(outfile,'R injection tangent / R-major','(frbeam)', &
            frbeam)
       call ovarre(outfile,'Beam centreline tangency radius (m)','(rtanbeam)', &
            rtanbeam)
       call ovarre(outfile,'Maximum possible tangency radius (m)','(rtanmax)', &
            rtanmax)
       call ovarre(outfile,'Beam decay lengths to centre','(taubeam)', &
            taubeam)
    end if

  end subroutine stheat

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stfwbs(outfile,iprint)

    !+ad_name  stfwbs
    !+ad_summ  Routine to calculate first wall, blanket and shield properties
    !+ad_summ  for a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  None
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_args  iprint : input integer : Switch to write output to file (1=yes)
    !+ad_desc  This routine calculates a stellarator's first wall, blanket and
    !+ad_desc  shield properties.
    !+ad_desc  It calculates the nuclear heating in the blanket / shield, and
    !+ad_desc  estimates the volume and masses of the first wall,
    !+ad_desc  blanket, shield and vacuum vessel.
    !+ad_desc  <P>The arrays <CODE>coef(i,j)</CODE> and <CODE>decay(i,j)</CODE>
    !+ad_desc  are used for exponential decay approximations of the
    !+ad_desc  (superconducting) TF coil nuclear parameters.
    !+ad_desc  <UL><P><LI><CODE>j = 1</CODE> : stainless steel shield (assumed)
    !+ad_desc      <P><LI><CODE>j = 2</CODE> : tungsten shield (not used)</UL>
    !+ad_desc  Note: Costing and mass calculations elsewhere assume
    !+ad_desc  stainless steel only.
    !+ad_desc  <P>The method is the same as for tokamaks (as performed via
    !+ad_desc  <A HREF="fwbs.html">fwbs</A>), except for the volume calculations,
    !+ad_desc  which scale the surface area of the components from that
    !+ad_desc  of the plasma.
    !+ad_prob  None
    !+ad_call  blanket_panos
    !+ad_call  blanket_neutronics
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarin
    !+ad_call  ovarre
    !+ad_hist  01/07/94 PJK Initial version
    !+ad_hist  10/06/96 PJK Moved first wall area calculation into STBILD
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_hist  14/08/13 PJK/FW First full replacement for FWBS
    !+ad_hist  07/11/13 PJK Added fwareaib/ob calculations
    !+ad_hist  03/03/14 PJK Moved divertor surface area calculation into STDIV
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  23/06/14 PJK Corrected wallmw units
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(kind(1.0D0)), dimension(5) :: fact
    real(kind(1.0D0)), dimension(5,2) :: coef
    real(kind(1.0D0)), dimension(7,2) :: decay

    integer, parameter :: ishmat = 1  !  stainless steel coil casing is assumed

    real(kind(1.0D0)) :: adewex,coilhtmx,decaybl,dpacop,dshieq,dshoeq, &
         fpsdt,fpydt,hecan,htheci,pheci,pheco,pneut2,ptfi,ptfiwp, &
         ptfo,ptfowp,r1,r2,raddose,volshldi,volshldo,wpthk

    logical :: first_call = .true.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  First wall inboard, outboard areas (assume 50% of total each)

    fwareaib = 0.5D0*fwarea
    fwareaob = 0.5D0*fwarea

    !  Blanket volume; assume that its surface area is scaled directly from the
    !  plasma surface area.
    !  Uses fhole (only, in contrast to tokamaks using ipowerflow=1) to take
    !  account of gaps due to ports etc.

    r1 = rminor + 0.5D0*(scrapli+fwith + scraplo+fwoth)
    blarea = sarea * r1/rminor * (1.0D0-fhole)
    blareaib = 0.5D0*blarea
    blareaob = 0.5D0*blarea

    volblkti = blareaib * blnkith
    volblkto = blareaob * blnkoth
    volblkt = volblkti + volblkto

    !  Shield volume
    !  Uses fvolsi, fvolso as area coverage factors

    r1 = r1 + 0.5D0*(blnkith+blnkoth)
    sharea = sarea * r1/rminor
    shareaib = 0.5D0*sharea * fvolsi
    shareaob = 0.5D0*sharea * fvolso

    volshldi = shareaib * shldith
    volshldo = shareaob * shldoth
    volshld = volshldi + volshldo

    !  Neutron power lost through holes in first wall

    pnucloss = pneutmw * fhole

    !  Blanket neutronics calculations

    if (blktmodel == 1) then

       call blanket_neutronics

       fpydt = cfactr * tlife

    else

       !  TF coil nuclear heating parameters

       fact(1) = 8.0D0
       fact(2) = 8.0D0
       fact(3) = 6.0D0
       fact(4) = 4.0D0
       fact(5) = 4.0D0

       coef(1,1) = 10.3D0
       coef(2,1) = 11.6D0
       coef(3,1) = 7.08D5
       coef(4,1) = 2.19D18
       coef(5,1) = 3.33D-7
       coef(1,2) = 8.32D0
       coef(2,2) = 10.6D0
       coef(3,2) = 7.16D5
       coef(4,2) = 2.39D18
       coef(5,2) = 3.84D-7

       decay(1,1) = 10.05D0
       decay(2,1) = 17.61D0
       decay(3,1) = 13.82D0
       decay(4,1) = 13.24D0
       decay(5,1) = 14.31D0
       decay(6,1) = 13.26D0
       decay(7,1) = 13.25D0
       decay(1,2) = 10.02D0
       decay(2,2) = 3.33D0
       decay(3,2) = 15.45D0
       decay(4,2) = 14.47D0
       decay(5,2) = 15.87D0
       decay(6,2) = 15.25D0
       decay(7,2) = 17.25D0

       pnuccp = 0.0D0

       !  Energy-multiplied neutron power

       pneut2 = (pneutmw - pnucloss - pnuccp) * emult

       !  Nuclear heating in the blanket

       !if (lblnkt == 1) then
       !   if (smstr == 1) then  !  solid blanket
       !      decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
       !   else  !  liquid blanket
       !      decaybl = 0.075D0 / (1.0D0 - vfblkt - fbllipb - fblli)
       !   end if
       !else  !  original blanket model - solid blanket
       decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)
       !end if

       pnucblkt = pneut2 * (1.0D0 - exp(-blnkoth/decaybl) )

       !  Nuclear heating in the shield

       pnucshld = pneut2 - pnucblkt

       !  Full power DT operation years for replacement of TF Coil
       !  (or Plant Life)

       fpydt = cfactr * tlife
       fpsdt = fpydt * 3.154D7

       !  Superconducting TF coil shielding calculations
       !  The 'He can' previously referred to is actually the steel case on the
       !  plasma-facing side of the TF coil.

       if (itfsup == 1) then

          !  N.B. The vacuum vessel appears to be ignored

          dshieq = shldith + fwith + blnkith
          dshoeq = shldoth + fwoth + blnkoth

          !  Case thickness on plasma-facing side of TF coil

          hecan = casthi

          !  Winding pack radial thickness, including groundwall insulation

          wpthk = thkwp + 2.0D0*tinstf

          !  Nuclear heating rate in inboard TF coil (MW/m**3)

          coilhtmx = fact(1) * wallmw * coef(1,ishmat) * &
               exp(-decay(6,ishmat) * (dshieq + hecan))

          !  Total nuclear heating (MW)

          ptfiwp = coilhtmx * tfsai * &
               (1.0D0-exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)
          ptfowp = fact(1) * wallmw * coef(1,ishmat) * &
               exp(-decay(6,ishmat) * (dshoeq + hecan)) * tfsao * &
               (1.0D0 - exp(-decay(1,ishmat)*wpthk)) / decay(1,ishmat)

          !  Nuclear heating in plasma-side TF coil case (MW)

          htheci = fact(2) * wallmw * coef(2,ishmat) * &
               exp(-decay(7,ishmat) * dshieq)
          pheci = htheci * tfsai * (1.0D0-exp(-decay(2,ishmat)*hecan))/ &
               decay(2,ishmat)
          pheco = fact(2) * wallmw * coef(2,ishmat) * &
               exp(-decay(7,ishmat) * dshoeq) * tfsao * &
               (1.0D0-exp(-decay(2,ishmat)*hecan))/decay(2,ishmat)
          ptfi = ptfiwp + pheci
          ptfo = ptfowp + pheco
          ptfnuc = ptfi + ptfo

          !  Insulator dose (rad)

          raddose = coef(3,ishmat) * fpsdt * fact(3) * wallmw * &
               exp(-decay(3,ishmat) * (dshieq+hecan))

          !  Maximum neutron fluence in superconductor (n/m**2)

          nflutf = fpsdt * fact(4) * wallmw * coef(4,ishmat) * &
               exp(-decay(4,ishmat) * (dshieq+hecan))

          !  Atomic displacement in copper stabilizer

          dpacop = fpsdt * fact(5) * wallmw * coef(5,ishmat) * &
               exp(-decay(5,ishmat) * (dshieq + hecan) )

       else  !  Resistive TF coils
          dshieq = 0.0D0
          dshoeq = 0.0D0
          hecan = 0.0D0
          wpthk = 0.0D0
          coilhtmx = 0.0D0
          ptfiwp = 0.0D0
          ptfowp = 0.0D0
          htheci = 0.0D0
          pheci = 0.0D0
          pheco = 0.0D0
          ptfi = 0.0D0
          ptfo = 0.0D0
          ptfnuc = 0.0D0
          raddose = 0.0D0
          nflutf = 0.0D0
          dpacop = 0.0D0
       end if

    end if ! blktmodel = 0

    !  Divertor mass
    !  N.B. divsur is calculated in stdiv after this point, so will
    !  be zero on first lap, hence the initial approximation

    if (first_call) then
       divsur = 50.0D0
       first_call = .false.
    end if

    divmas = divsur * divdens * (1.0D0 - divclfr) * divplt

    !  Start adding components of the coolant mass:
    !  Divertor coolant volume (m3)

    coolmass = divsur * divclfr * divplt  !  volume

    !  Blanket mass, excluding coolant

    if (blktmodel == 0) then
       whtblss = volblkt * denstl * fblss
       whtblbe = volblkt * 1850.0D0  * fblbe  !  density modified from 1900 kg/m3
       whtblvd = volblkt * 5870.0D0  * fblvd
       wtblli2o = volblkt * 2010.0D0  * fblli2o
       whtblkt = whtblss + whtblvd + wtblli2o + whtblbe
    else  !  volume fractions proportional to sub-assembly thicknesses
       whtblss = denstl * ( &
            volblkti/blnkith * ( &
            blbuith * fblss + &
            blbmith * (1.0D0-fblhebmi) + &
            blbpith * (1.0D0-fblhebpi) ) &
            + volblkto/blnkoth * ( &
            blbuoth * fblss + &
            blbmoth * (1.0D0-fblhebmo) + &
            blbpoth * (1.0D0-fblhebpo) ) )
       whtblbe = 1850.0D0 * fblbe * ( &
            (volblkti * blbuith/blnkith) + (volblkto * blbuoth/blnkoth) )
       whtblbreed = densbreed * fblbreed * ( &
            (volblkti * blbuith/blnkith) + (volblkto * blbuoth/blnkoth) )
       whtblkt = whtblss + whtblbe + whtblbreed

       vfblkt = volblkti/volblkt * ( &  !  inboard portion
            (blbuith/blnkith) * (1.0D0 - fblbe - fblbreed - fblss) &
            + (blbmith/blnkith) * fblhebmi &
            + (blbpith/blnkith) * fblhebpi )
       vfblkt = vfblkt + volblkto/volblkt * ( &  !  outboard portion
            (blbuoth/blnkoth) * (1.0D0 - fblbe - fblbreed - fblss) &
            + (blbmoth/blnkoth) * fblhebmo &
            + (blbpoth/blnkoth) * fblhebpo )
    end if

    whtshld = volshld * denstl * (1.0D0 - vfshld)

    !  Thermodynamic blanket model
    !  (supersedes above calculations of blanket mass and volume)

    !if (lblnkt == 1) then
    !   call blanket_panos(1,outfile,iprint)
    !
    !   !  Different (!) approximation for inboard/outboard
    !   !  blanket volumes: assume cylinders of equal heights
    !
    !   r1 = rsldi + shldith + 0.5D0*blnkith
    !   r2 = rsldo - shldoth - 0.5D0*blnkoth
    !   volblkti = volblkt * (r1*blnkith)/((r1*blnkith)+(r2*blnkoth))
    !   volblkto = volblkt * (r2*blnkoth)/((r1*blnkith)+(r2*blnkoth))
    !
    !end if

    !  Blanket coolant is assumed to be helium for the models used
    !  when blktmodel > 0

    if (blktmodel == 0) then
       coolmass = coolmass + volblkt*vfblkt  !  volume
    end if

    !  Penetration shield (set = internal shield)

    wpenshld = whtshld
    coolmass = coolmass + volshld*vfshld  !  volume

    !  First wall mass
    !  (first wall area is calculated elsewhere)

    fwmass = fwarea * (fwith+fwoth)/2.0D0 * denstl * (1.0D0-fwclfr)

    !  Surface areas adjacent to plasma

    coolmass = coolmass + fwarea * (fwith+fwoth)/2.0D0 * fwclfr  !  volume

    !  Mass of coolant = volume * density at typical coolant
    !  temperatures and pressures
    !  N.B. for blktmodel > 0, mass of helium coolant in blanket is ignored...

    if ((blktmodel > 0).or.(costr == 2)) then  !  pressurised water coolant
       coolmass = coolmass*806.719D0
    else  !  gaseous helium coolant
       coolmass = coolmass*1.517D0
    end if

    !  Assume external cryostat is a torus with circular cross-section,
    !  centred on plasma major radius.
    !  N.B. No check made to see if coils etc. lie wholly within cryostat...

    !  External cryostat outboard major radius (m)

    rdewex = rtot + 0.5D0*tfthko + rpf2dewar
    adewex = rdewex-rmajor

    !  External cryostat volume

    vdewex = 4.0D0*pi*pi*rmajor*adewex * ddwex

    !  Internal vacuum vessel volume
    !  fvoldw accounts for ports, support, etc. additions

    r1 = rminor + 0.5D0*(scrapli+fwith+blnkith+shldith &
         + scraplo+fwoth+blnkoth+shldoth)
    vdewin = ddwi * sarea * r1/rminor * fvoldw

    !  Vacuum vessel mass

    cryomass = vdewin * denstl

    !  Sum of internal vacuum vessel and external cryostat masses

    dewmkg = (vdewin + vdewex) * denstl

    if (iprint == 0) return

    !  Output section

    call oheadr(outfile,'Shield / Blanket')
    call ovarre(outfile,'Average neutron wall load (MW/m2)','(wallmw)', wallmw)
    if (blktmodel > 0) then
       call ovarre(outfile,'Neutron wall load peaking factor','(wallpf)', wallpf)
    end if
    call ovarre(outfile,'DT full power TF coil operation (yrs)', &
         '(fpydt)',fpydt)
    call ovarre(outfile,'Inboard shield thickness (m)','(shldith)',shldith)
    call ovarre(outfile,'Outboard shield thickness (m)','(shldoth)',shldoth)
    call ovarre(outfile,'Top shield thickness (m)','(shldtth)',shldtth)
    if (blktmodel > 0) then
       call ovarre(outfile,'Inboard breeding unit thickness (m)','(blbuith)', blbuith)
       call ovarre(outfile,'Inboard box manifold thickness (m)','(blbmith)', blbmith)
       call ovarre(outfile,'Inboard back plate thickness (m)','(blbpith)', blbpith)
    end if
    call ovarre(outfile,'Inboard blanket thickness (m)','(blnkith)', blnkith)
    if (blktmodel > 0) then
       call ovarre(outfile,'Outboard breeding unit thickness (m)','(blbuoth)', blbuoth)
       call ovarre(outfile,'Outboard box manifold thickness (m)','(blbmoth)', blbmoth)
       call ovarre(outfile,'Outboard back plate thickness (m)','(blbpoth)', blbpoth)
    end if
    call ovarre(outfile,'Outboard blanket thickness (m)','(blnkoth)', blnkoth)
    call ovarre(outfile,'Top blanket thickness (m)','(blnktth)',blnktth)
    if (blktmodel == 0) then
       call ovarre(outfile,'Inboard side TF coil case thickness (m)', &
            '(hecan)',hecan)
       call osubhd(outfile,'TF coil nuclear parameters :')
       call ovarre(outfile,'Peak magnet heating (MW/m3)','(coilhtmx)', &
            coilhtmx)
       call ovarre(outfile,'Inboard TF coil winding pack heating (MW)', &
            '(ptfiwp)',ptfiwp)
       call ovarre(outfile,'Outboard TF coil winding pack heating (MW)', &
            '(ptfowp)',ptfowp)
       call ovarre(outfile,'Peak TF coil case heating (MW/m3)','(htheci)', &
            htheci)
       call ovarre(outfile,'Inboard coil case heating (MW)','(pheci)',pheci)
       call ovarre(outfile,'Outboard coil case heating (MW)','(pheco)',pheco)
       call ovarre(outfile,'Insulator dose (rad)','(raddose)',raddose)
       call ovarre(outfile,'Maximum neutron fluence (n/m2)','(nflutf)', &
            nflutf)
       call ovarre(outfile,'Copper stabiliser displacements/atom', &
            '(dpacop)',dpacop)

       call osubhd(outfile,'Nuclear heating :')
       call ovarre(outfile,'Blanket heating (MW)','(pnucblkt)',pnucblkt)
       call ovarre(outfile,'Shield heating (MW)','(pnucshld)',pnucshld)
    else
       call osubhd(outfile,'Blanket neutronics :')
       call ovarre(outfile,'Blanket heating (MW)','(pnucblkt)',pnucblkt)
       call ovarre(outfile,'Shield heating (MW)','(pnucshld)',pnucshld)
       call ovarre(outfile,'Energy multiplication in blanket','(emult)',emult)
       call ovarin(outfile,'Number of divertor ports assumed','(npdiv)',npdiv)
       call ovarin(outfile,'Number of inboard H/CD ports assumed', &
            '(nphcdin)',nphcdin)
       call ovarin(outfile,'Number of outboard H/CD ports assumed', &
            '(nphcdout)',nphcdout)
       select case (hcdportsize)
       case (1)
          call ocmmnt(outfile,'     (small heating/current drive ports assumed)')
       case default
          call ocmmnt(outfile,'     (large heating/current drive ports assumed)')
       end select
       select case (breedmat)
       case (1)
          call ocmmnt(outfile,'Breeder material: Lithium orthosilicate (Li4Si04)')
       case (2)
          call ocmmnt(outfile,'Breeder material: Lithium methatitanate (Li2TiO3)')
       case (3)
          call ocmmnt(outfile,'Breeder material: Lithium zirconate (Li2ZrO3)')
       case default  !  shouldn't get here...
          call ocmmnt(outfile,'Unknown breeder material...')
       end select
       call ovarre(outfile,'Lithium-6 enrichment (%)','(li6enrich)',li6enrich)
       call ovarre(outfile,'Tritium breeding ratio','(tbr)',tbr)
       call ovarre(outfile,'Tritium production rate (g/day)','(tritprate)',tritprate)
       call ovarre(outfile,'Nuclear heating on i/b TF coil (MW/m3)','(pnuctfi)',pnuctfi)
       call ovarre(outfile,'Nuclear heating on o/b TF coil (MW/m3)','(pnuctfo)',pnuctfo)
       call ovarre(outfile,'Total nuclear heating on TF coil (MW)','(ptfnuc)',ptfnuc)
       call ovarre(outfile,'Fast neut. fluence on i/b TF coil (n/m2)', &
            '(nflutfi)',nflutfi*1.0D4)
       call ovarre(outfile,'Fast neut. fluence on o/b TF coil (n/m2)', &
            '(nflutfo)',nflutfo*1.0D4)
       call ovarre(outfile,'Minimum final He conc. in IB VV (appm)','(vvhemini)',vvhemini)
       call ovarre(outfile,'Minimum final He conc. in OB VV (appm)','(vvhemino)',vvhemino)
       call ovarre(outfile,'Maximum final He conc. in IB VV (appm)','(vvhemaxi)',vvhemaxi)
       call ovarre(outfile,'Maximum final He conc. in OB VV (appm)','(vvhemaxo)',vvhemaxo)
       call ovarre(outfile,'Blanket lifetime (full power years)','(bktlife)',bktlife)
       call ovarre(outfile,'Blanket lifetime (calendar years)','(t_bl_y)',t_bl_y)
    end if

    call osubhd(outfile,'Blanket / shield volumes and weights :')

    !if (lblnkt == 1) then
    !   if (smstr == 1) then
    !      write(outfile,600) volblkti, volblkto, volblkt,  &
    !           whtblkt, vfblkt, fblbe, whtblbe, fblli2o, wtblli2o,  &
    !           fblss, whtblss, fblvd, whtblvd, volshldi, volshldo,  &
    !           volshld, whtshld, vfshld, wpenshld
    !   else
    !      write(outfile,601) volblkti, volblkto, volblkt,  &
    !           whtblkt, vfblkt, fbllipb, wtbllipb, fblli, whtblli,  &
    !           fblss, whtblss, fblvd, whtblvd, volshldi, volshldo,  &
    !           volshld, whtshld, vfshld, wpenshld
    !   end if
    !else if (blktmodel == 0) then
    if (blktmodel == 0) then
       write(outfile,600) volblkti, volblkto, volblkt, whtblkt, vfblkt, &
            fblbe, whtblbe, fblli2o, wtblli2o, fblss, whtblss, fblvd, &
            whtblvd, volshldi, volshldo, volshld, whtshld, vfshld, &
            wpenshld
    else
       write(outfile,602) volblkti, volblkto, volblkt, whtblkt, vfblkt, &
            (volblkti/volblkt * blbuith/blnkith + &
            volblkto/volblkt * blbuoth/blnkoth) * fblbe, whtblbe, &
            (volblkti/volblkt * blbuith/blnkith + &
            volblkto/volblkt * blbuoth/blnkoth) * fblbreed, whtblbreed, &
            volblkti/volblkt/blnkith * (blbuith * fblss &
            + blbmith * (1.0D0-fblhebmi) + blbpith * (1.0D0-fblhebpi)) + &
            volblkto/volblkt/blnkoth * (blbuoth * fblss &
            + blbmoth * (1.0D0-fblhebmo) + blbpoth * (1.0D0-fblhebpo)), &
            whtblss, &
            volshldi, volshldo, volshld, whtshld, vfshld, wpenshld
    end if

600 format( &
         t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
         t32,'-----------',t45,'------------',t62,'-----------'/ &
         '    Inboard blanket' ,t32,1pe10.3,/ &
         '    Outboard blanket' ,t32,1pe10.3,/ &
         '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '       Blanket Be   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket Li2O ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/ &
         '    Inboard shield'  ,t32,1pe10.3,/ &
         '    Outboard shield'  ,t32,1pe10.3,/ &
         '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '    Penetration shield'        ,t62,1pe10.3)

601 format( &
         t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
         t32,'-----------',t45,'------------',t62,'-----------'/ &
         '    Inboard blanket' ,t32,1pe10.3,/ &
         '    Outboard blanket' ,t32,1pe10.3,/ &
         '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '       Blanket LiPb ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket Li   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket ss   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket Vd   ',t45,1pe10.3,t62,1pe10.3/ &
         '    Inboard shield'  ,t32,1pe10.3,/ &
         '    Outboard shield'  ,t32,1pe10.3,/ &
         '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '    Penetration shield'        ,t62,1pe10.3)

602 format( &
         t32,'volume (m3)',t45,'vol fraction',t62,'weight (kg)'/ &
         t32,'-----------',t45,'------------',t62,'-----------'/ &
         '    Inboard blanket' ,t32,1pe10.3,/ &
         '    Outboard blanket' ,t32,1pe10.3,/ &
         '    Total blanket' ,t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '       Blanket Be   ',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket breeder',t45,1pe10.3,t62,1pe10.3/ &
         '       Blanket steel',t45,1pe10.3,t62,1pe10.3/ &
         '    Inboard shield'  ,t32,1pe10.3,/ &
         '    Outboard shield'  ,t32,1pe10.3,/ &
         '    Primary shield',t32,1pe10.3,t62,1pe10.3/ &
         '       Void fraction' ,t45,1pe10.3,/ &
         '    Penetration shield'        ,t62,1pe10.3)

    call osubhd(outfile,'Other volumes, masses and areas :')
    call ovarre(outfile,'First wall area (m2)','(fwarea)',fwarea)
    call ovarre(outfile,'First wall mass (kg)','(fwmass)',fwmass)
    call ovarre(outfile,'External cryostat inner radius (m)','',rdewex-2.0D0*adewex)
    call ovarre(outfile,'External cryostat outer radius (m)','(rdewex)',rdewex)
    call ovarre(outfile,'External cryostat minor radius (m)','(adewex)',adewex)
    call ovarre(outfile,'External cryostat shell volume (m3)','(vdewex)',vdewex)
    call ovarre(outfile,'External cryostat mass (kg)','',dewmkg-cryomass)
    call ovarre(outfile,'Internal vacuum vessel shell volume (m3)','(vdewin)',vdewin)
    call ovarre(outfile,'Vacuum vessel mass (kg)','(cryomass)',cryomass)
    call ovarre(outfile,'Total cryostat + vacuum vessel mass (kg)','(dewmkg)',dewmkg)
    call ovarre(outfile,'Divertor area (m2)','(divsur)',divsur)
    call ovarre(outfile,'Divertor mass (kg)','(divmas)',divmas)

  end subroutine stfwbs

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stdlim(alphan,bt,powht,rmajor,rminor,dlimit)

    !+ad_name  stdlim
    !+ad_summ  Routine to calculate the density limit in a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  alphan : input real : Density profile index
    !+ad_args  bt     : input real : Toroidal field on axis (T)
    !+ad_args  powht  : input real : Absorbed heating power (MW)
    !+ad_args  rmajor : input real : Plasma major radius (m)
    !+ad_args  rminor : input real : Plasma minor radius (m)
    !+ad_args  dlimit : output real : Maximum volume-averaged plasma density (/m3)
    !+ad_desc  This routine calculates the density limit for a stellarator.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  30/06/94 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_hist  03/03/14 PJK Generalised for any density profile type
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  S.Sudo, Y.Takeiri, H.Zushi et al., Scalings of Energy Confinement
    !+ad_docc  and Density Limit in Stellarator/Heliotron Devices, Nuclear Fusion
    !+ad_docc  vol.30, 11 (1990).
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: alphan,bt,powht,rmajor,rminor
    real(kind(1.0D0)), intent(out) :: dlimit

    !  Local variables

    real(kind(1.0D0)) :: arg,dnlamx

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    arg = powht*bt / (rmajor*rminor*rminor)

    if (arg <= 0.0D0) then
       fdiags(1) = arg ; fdiags(2) = powht
       fdiags(3) = bt ; fdiags(4) = rmajor
       fdiags(5) = rminor
       call report_error(108)
    end if

    !  Maximum line-averaged electron density

    dnlamx = 0.25D20 * sqrt(arg)

    !  Scale the result so that it applies to the volume-averaged
    !  electron density

    dlimit = dnlamx * dene/dnla

  end subroutine stdlim

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stblim(betamx)

    !+ad_name  stblim
    !+ad_summ  Routine to calculate the beta limit in a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  betamx : output real : Maximum volume-averaged plasma beta
    !+ad_desc  This routine calculates the beta limit for a stellarator.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  30/06/94 PJK Initial version
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_stat  Okay
    !+ad_docs  J.F.Lyon, K.Gulec, R.L.Miller and L.El-Guebaly, Status of the U.S.
    !+ad_docc  Stellarator Reactor Study
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: betamx

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    betamx = 0.05D0

  end subroutine stblim

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stigma(outfile)

    !+ad_name  stigma
    !+ad_summ  Routine to calculate ignition margin at the final point
    !+ad_summ  with different stellarator confinement time scaling laws
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This routine calculates the ignition margin at the final
    !+ad_desc  point with different stellarator confinement time scaling laws
    !+ad_prob  None
    !+ad_call  fhfac
    !+ad_call  oblnkl
    !+ad_call  osubhd
    !+ad_call  pcond
    !+ad_hist  30/06/94 PJK Initial version
    !+ad_hist  01/04/98 PJK Modified call to PCOND
    !+ad_hist  30/06/98 PJK Modified call to PCOND
    !+ad_hist  19/01/99 PJK Modified call to PCOND
    !+ad_hist  16/07/01 PJK Modified call to PCOND
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added physics_module
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  23/01/13 PJK Added two more scaling laws; changed PCOND argument
    !+ad_hisc               q95 to iotabar
    !+ad_hist  20/05/14 PJK Changed prad to pcorerad
    !+ad_hist  22/05/14 PJK Name changes to power quantities
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    real(kind(1.0D0)) :: d2,powerhtz,ptrez,ptriz,taueez,taueezz, &
         taueffz,taueiz
    integer :: i,iisc
    integer, parameter :: nstlaw = 5
    integer, dimension(nstlaw) :: istlaw

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call osubhd(outfile,'Confinement times, and required H-factors :')

    write(outfile,10)
10  format( &
         t5,'scaling law', &
         t30,'confinement time (s)', &
         t55,'H-factor for')

    write(outfile,20)
20  format( &
         t34,'for H = 2', &
         t54,'power balance')

    call oblnkl(outfile)

    !  Label stellarator scaling laws (update if more are added)

    istlaw(1) = 21
    istlaw(2) = 22
    istlaw(3) = 23
    istlaw(4) = 37
    istlaw(5) = 38

    !  Calculate power balances for all stellarator scaling laws
    !  assuming H = 2

    d2 = 2.0D0
    do iisc = 1,nstlaw
       i = istlaw(iisc)

       call pcond(afuel,palpmw,aspect,bt,dnitot,dene,dnla,eps,d2, &
            iinvqd,i,ignite,kappa,kappa95,kappaa,pchargemw,pinjmw, &
            plascur,pohmpv,pcoreradpv,rmajor,rminor,te,ten,tin, &
            iotabar,qstar,vol,xarea,zeff,ptrez,ptriz,taueez,taueiz, &
            taueffz,powerhtz)

       hfac(iisc) = fhfac(i)
       write(outfile,30) tauscl(istlaw(iisc)),taueez,hfac(iisc)
    end do
30  format(t2,a24,t34,f7.3,t58,f7.3)

  end subroutine stigma

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stout(outfile)

    !+ad_name  stout
    !+ad_summ  Routine to print out the final stellarator machine parameters
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This routine prints out the stellarator's parameters at the
    !+ad_desc  end of a run.
    !+ad_prob  None
    !+ad_call  acpow
    !+ad_call  avail
    !+ad_call  bldgcall
    !+ad_call  costs
    !+ad_call  fispac
    !+ad_call  loca
    !+ad_call  outplas
    !+ad_call  power2
    !+ad_call  stbild
    !+ad_call  stcoil
    !+ad_call  stdiv
    !+ad_call  stfwbs
    !+ad_call  stheat
    !+ad_call  stigma
    !+ad_call  ststrc
    !+ad_call  tfpwr
    !+ad_call  tfspcall
    !+ad_call  vaccall
    !+ad_hist  30/06/94 PJK Initial version
    !+ad_hist  27/02/96 PJK Added use of IFISPACT
    !+ad_hist  23/01/97 PJK Split routine POWER into POWER1 and POWER2
    !+ad_hist  26/02/97 PJK Added routine LOCA
    !+ad_hist  18/11/97 PJK Removed NOUT argument from FISPAC call
    !+ad_hist  19/05/99 PJK Added call to routine AVAIL
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added costs_module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  17/10/12 PJK Added divertor_module
    !+ad_hist  18/10/12 PJK Added fwbs_module
    !+ad_hist  29/10/12 PJK Added sctfcoil_module
    !+ad_hist  29/10/12 PJK Added vacuum_module
    !+ad_hist  30/10/12 PJK Added power_module
    !+ad_hist  30/10/12 PJK Added buildings_module
    !+ad_hist  23/01/13 PJK Commented out fispac, loca calls
    !+ad_hist  13/08/13 PJK/FW Added new stellarator divertor module
    !+ad_hist  14/08/13 PJK/FW Replaced fwbs with stfwbs
    !+ad_hist  03/03/14 PJK Removed call to tfspcall
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call costs(outfile,1)
    call avail(outfile,1)
    call outplas(outfile)
    call stigma(outfile)
    call stheat(outfile,1)
    call stdiv(outfile,1)
    call stbild(outfile,1)
    call stcoil(outfile,1)
    call ststrc(outfile,1)
    call stfwbs(outfile,1)

    !if (ifispact == 1) then
    !   call fispac(0)
    !   call fispac(1)
    !   call loca(outfile,0)
    !   call loca(outfile,1)
    !end if

    call tfpwr(outfile,1)
    call vaccall(outfile,1)
    call bldgcall(outfile,1)
    call acpow(outfile,1)
    call power2(outfile,1)

  end subroutine stout

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine ststrc(outfile,iprint)

    !+ad_name  ststrc
    !+ad_summ  Routine to calculate the structural masses for a stellarator
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the structural masses for a stellarator.
    !+ad_desc  This is the stellarator version of routine
    !+ad_desc  <A HREF="struct.html">STRUCT</A>. In practice, many of the masses
    !+ad_desc  are simply set to zero to avoid double-counting of structural
    !+ad_desc  components that are specified differently for tokamaks.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_hist  01/07/94 PJK Initial version
    !+ad_hist  01/02/96 PJK Added itfsup, ipfres to argument list of STRUCT
    !+ad_hist  24/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  17/10/12 PJK Added divertor_variables
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added pfcoil_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  29/10/12 PJK Added structure_variables
    !+ad_hist  29/10/12 PJK Added structure_module
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  03/03/14 PJK Calculations simplified and brought
    !+ad_hisc               inside this routine
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_stat  Okay
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Tokamak/RFP-specific PF coil fence mass set to zero

    fncmass = 0.0D0

    !  Set the following to zero to avoid double-counting.
    !  The intercoil mass (aintmass) is calculated in stcoil and
    !  is assumed to comprise all of the structural mass

    clgsmass = 0.0D0
    gsmass = 0.0D0

    !  Total mass of cooled components

    coldmass = whttf + aintmass + dewmkg

    !  Output section

    if (iprint == 0) return

    call oheadr(outfile,'Support Structure')
    call ovarre(outfile,'Intercoil support structure mass (kg)', &
         '(aintmass)',aintmass)
    call ovarre(outfile,'Mass of cooled components (kg)', &
         '(coldmass)',coldmass)

  end subroutine ststrc

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stdiv(outfile,iprint)

    !+ad_name  stdiv
    !+ad_summ  Routine to call the stellarator divertor model
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  None
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calls the divertor model for a stellarator,
    !+ad_desc  developed by Felix Warmer.
    !+ad_prob  At present the divertor surface area calculation is
    !+ad_prob  missing here (the tokamak version is used elsewhere), and
    !+ad_prob  this affects the mass/size of the divertor, i.e. its cost.
    !+ad_call  None
    !+ad_hist  14/08/13 PJK/FW Initial version
    !+ad_hist  03/03/14 PJK Divertor plate width calculated by assuming
    !+ad_hisc               wetted area is a fraction fdivwet of total plate area
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_stat  Okay
    !+ad_docs  Stellarator Divertor Model for the Systems
    !+ad_docc  Code PROCESS, F. Warmer, 21/06/2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: R,alpha,xi_p,T_scrape,Theta,darea
    real(kind(1.0D0)) :: E,c_s,w_r,Delta,L_P,L_X_T,l_q,l_b
    real(kind(1.0D0)) :: F_x,L_D,L_T,L_W,P_div,A_eff,q_div

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! PROCESS variables to local variables

    Theta = flpitch !  ~bmn [rad] field line pitch
    R = rmajor
    P_div = pdivt
    alpha = anginc
    xi_p = xpertin
    T_scrape = tdiv

    !  Scrape-off temperature in Joules

    E = T_scrape*echarge

    !  Sound speed of particles (m/s)

    c_s = sqrt(E/(afuel*umass))

    !  Island size (m)

    w_r = 4.0D0*sqrt(bmn * R/(shear*n_res))

    !  Perpendicular (to plate) distance from X-point to divertor plate (m)

    Delta = f_w*w_r

    !  Length 'along' plasma (m)

    L_P = twopi*R*(dble(m_res)/n_res)

    !  Connection length from X-point to divertor plate (m)

    L_X_T = Delta/Theta

    !  Power decay length (m)

    l_q = sqrt(xi_p*(L_X_T/c_s))

    !  Channel broadening length (m)

    l_b = sqrt(xi_p*L_P/(c_s))

    !  Channel broadening factor

    F_x = 1.0D0 + (l_b / (L_P*Theta))

    !  Length of a single divertor plate (m)

    L_D = F_x*L_P*(Theta/alpha)

    !  Total length of divertor plates (m)

    L_T = 2.0D0*n_res*L_D

    !  Wetted area (m2)

    A_eff = L_T*l_q

    !  Divertor plate width (m): assume total area is wetted area/fdivwet

    darea = A_eff / fdivwet
    L_W = darea / L_T

    !  Divertor heat load (MW/m2)

    q_div = f_asym*(P_div/A_eff)

    !  Transfer to global variables

    hldiv = q_div
    divsur = darea

    if (iprint == 0) return

    call oheadr(outfile,'Divertor')

    call ovarre(outfile,'Power to divertor (MW)','(pdivt)',pdivt)
    call ovarre(outfile,'Angle of incidence (deg)','(anginc)',anginc*180.0D0/pi)
    call ovarre(outfile,'Perp. heat transport coefficient (m2/s)', &
         '(xpertin)',xpertin)
    call ovarre(outfile,'Divertor plasma temperature (eV)','(tdiv)',tdiv)
    call ovarre(outfile,'Radiated power fraction in SOL','(f_rad)',f_rad)
    call ovarre(outfile,'Heat load peaking factor','(f_asym)',f_asym)
    call ovarin(outfile,'Poloidal resonance number','(m_res)',m_res)
    call ovarin(outfile,'Toroidal resonance number','(n_res)',n_res)
    call ovarre(outfile,'Relative radial field perturbation','(bmn)',bmn)
    call ovarre(outfile,'Field line pitch (rad)','(flpitch)',flpitch)
    call ovarre(outfile,'Island size fraction factor','(f_w)',f_w)
    call ovarre(outfile,'Magnetic shear (/m)','(shear)',shear)
    call ovarre(outfile,'Divertor wetted area (m2)','(A_eff)',A_eff)
    call ovarre(outfile,'Wetted area fraction of total plate area','(fdivwet)',fdivwet)
    call ovarre(outfile,'Divertor plate length (m)','(L_d)',L_d)
    call ovarre(outfile,'Divertor plate width (m)','(L_w)',L_w)
    call ovarre(outfile,'Flux channel broadening factor','(F_x)',F_x)
    call ovarre(outfile,'Power decay width (cm)','(100*l_q)',100.0D0*l_q)
    call ovarre(outfile,'Island width (m)','(w_r)',w_r)
    call ovarre(outfile,'Perp. distance from X-point to plate (m)', &
         '(Delta)',Delta)
    call ovarre(outfile,'Peak heat load (MW/m2)','(hldiv)',hldiv)

  end subroutine stdiv

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcoil(outfile,iprint)

    !+ad_name  stcoil
    !+ad_summ  Routine that performs the calculations for stellarator coils
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_auth  F Warmer, IPP Greifswald
    !+ad_cont  coil_energy
    !+ad_cont  intersect
    !+ad_cont  scaling_calculations
    !+ad_cont  solenoid
    !+ad_args  outfile : input integer : output file unit
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine calculates the properties of the coils for
    !+ad_desc  a stellarator device.
    !+ad_desc  <P>The coil set parameters for the Helias 5-B stellarator power
    !+ad_desc  plant design are used as the basis for the calculations. The coils
    !+ad_desc  are assumed to be a fixed shape, but are scaled in size
    !+ad_desc  appropriately for the machine being modelled. Analytic inductance
    !+ad_desc  and field calculations based on the assumption of circular coils
    !+ad_desc  are used to evaluate the critical field at the coil superconductor.
    !+ad_prob  None
    !+ad_call  helias5b_coil_parameters
    !+ad_call  coil_energy
    !+ad_call  find_y_nonuniform_x
    !+ad_call  intersect
    !+ad_call  report_error
    !+ad_call  scaling_calculations
    !+ad_call  stcoil_output
    !+ad_hist  20/07/94 PJK Initial version
    !+ad_hist  20/09/12 PJK Initial F90 version
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added constants
    !+ad_hist  18/10/12 PJK Added fwbs_variables
    !+ad_hist  18/10/12 PJK Added tfcoil_variables
    !+ad_hist  18/10/12 PJK Modified argument list of tfcind
    !+ad_hist  29/10/12 PJK Added sctfcoil_module
    !+ad_hist  30/10/12 PJK Added build_variables
    !+ad_hist  23/01/13 PJK TFTORT now assumed to be the input value unless
    !+ad_hisc               limited by space available
    !+ad_hist  04/03/14 PJK/FW Routine completely rewritten; now based on
    !+ad_hisc               Felix Warmer's MATLAB code
    !+ad_hist  01/05/14 PJK Added rbmax comment
    !+ad_hist  06/05/14 PJK Removed wpvf completely
    !+ad_hist  24/06/14 PJK Removed refs to bucking cylinder
    !+ad_hist  26/06/14 PJK Added error_handling
    !+ad_hist  30/07/14 PJK Renamed borev to tfborev
    !+ad_hist  16/09/14 PJK Added tfcryoarea
    !+ad_stat  Okay
    !+ad_docs  The Stellarator Coil model for the Systems code PROCESS,
    !+ad_docc  F. Warmer, F. Schauer, IPP Greifswald, October 2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use helias5b_coil_parameters

    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    integer :: nbticool,n_it,k
    real(kind(1.0D0)) :: a_hor_max,a_vert_max,alph,b,b0_final,b_abs_max, &
         b_abs_mittel,b_hor_avg,b_hor_max,b_i,b_max_final,b_max_max,b_maxtf, &
         b_vert_avg,b_vert_max,bc,cpttf2,cr_area,d_coil,d_ic,du,f1,f2,f_b,f_i, &
         f_max,f_n,f_q,f_q_final,f_r,f_s,h,h_hor_max,h_insu_in,h_insu_out,h_max, &
         h_vert_max,i,j,kk,m_struc,msupstr,off,r_avg,r_occ,r_theta0,res,s_case, &
         t_c,t_no,t_no2,t_u,t_w,t_w_i,tfarea_sc,u,w_coil,w_mag,w_max,y,z,z1,z2
    real(kind(1.0D0)), allocatable, dimension(:) :: b_k,b_max_k,b_max_k2,b_lin, &
         f_k,f_q_crit_k

    real(kind(1.0D0)) :: awpc,awptf,leni,leno,rbcndut,rcoil

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Insulation thicknesses

    d_ic = 0.3D0    !  Insulation etc. thickness on top of coil casing (m)
    t_w = 0.053D0   !  Turn width (m)
    t_w_i = 0.0585  !  Average turn width including insulation (m)

    !  Densities for mass calculation

    nbticool = 2  !  Switch for NbTi superconductor cooling
    !  1 == normal helium cooling at 4.2 K
    !  2 == superfluid helium cooling at 1.8 K

    if (nbticool == 1) then
       T_u = 4.2
    else if (nbticool == 2) then
       T_u = 1.8
    else
       idiags(1) = nbticool ; call report_error(109)
    end if

    ! As the Stellarator has a different toroidal symmetry compared to the
    ! Tokamak, we identify some poloidal planes with names according to their
    ! shape:
    ! toroidal angle theta:
    ! theta = 0 degrees   -  bean-shaped plane
    ! theta = 36 degrees  -  triangular plane
    ! The coil module here is based on the bean-shaped plane as this plane can
    ! be most easily scaled and is similar to the Tokamak poloidal shape. As
    ! all coil shapes are assumed to be fixed, all other coils scale
    ! accordingly. 

    !  r_theta0   !  This is a new global input variable which will be calculated
    !       in the updated geometry module

    r_theta0 = k0*rminor

    !  Distance (m) from inner torus superconductor to outer torus superconductor
    !  in bean-shaped cross-section

    D_coil = gapds + ddwi + shldith + blnkith + fwith + scrapli + r_theta0 &
         + r_theta0 + scraplo + fwoth + blnkoth + shldoth + ddwi + gapsto

    !  Factors used to scale the Helias 5-B parameters

    f_R = rmajor/Rg5B       !  Size scaling factor with respect to Helias 5-B
    f_s = D_coil/D_coil_5B  !  Coil scaling factor 
    f_N = tfno/tfno5B       !  Coil number factor
    f_B = bt/B10            !  B-field scaling factor
    f_I = f_R*f_B/f_N       !  Current scaling factor

    !  Total coil current (MA)

    I = k1*bt * f_R/f_N

    !  Calculate B-fields for different coil cross-section scales

    res = 0.05D0               ! resolution 
    f_max = 1.5D0              ! maximal f_q for iteration
    N_it = nint(f_max/res)     ! number of iterations
    off = 0.05D0               ! offset

    allocate(f_k(n_it),b_k(n_it),b_max_k(n_it),b_max_k2(n_it))
    f_k = 0.0D0 ; B_k = 0.0D0 ; B_max_k = 0.0D0 ; B_max_k2 = 0.0D0

    do k = 1,N_it

       f_Q = (res*k) + off  !  Coil cross-section scaling factor
       f_k(k) = f_Q

       !  B-field calculation

       call scaling_calculations(f_R, f_s, f_Q, f_I, tfno, &
            b_abs_max, b_abs_mittel)

       B_k(k) = B_abs_mittel
       B_max_k(k) = B_abs_max

    end do

    !  Ensure that the maximal field is smaller than the critical field of
    !  Nb3SN / NbTi

    if (isumattf == 1) then
       Bc = Bco
    else if (isumattf == 3) then
       Bc = Bc2        
    else
       idiags(1) = isumattf ; call report_error(110)
    end if

    if (maxval(B_max_k) > Bc) then
       B_max_max = Bc - 1.0D0
    else
       B_max_max = maxval(B_max_k)
    end if

    if ( (isumattf == 3).and.(nbticool == 1) ) then
       B_max_max = 10.0D0
    end if

    !  Calculate critical field for different coil cross-section scales

    allocate(b_lin(n_it),f_q_crit_k(n_it))
    do k = 1,n_it
       b_lin(k) = minval(b_max_k) + dble(k-1)/(n_it-1) * (b_max_max - minval(b_max_k))
    end do

    if (isumattf == 1) then
       !  Nb3Sn critical field scaling:
       !  Y. Ilyin et al., Supercond. Sci. Technol. 20 (2007) 186
       !  Normal helium cooling with T=4.2K intrinsically assumed
       f_Q_crit_k(:) = 10.9D0 * sqrt(f_I) * (B_lin(:)**0.25D0) / (Bc - B_lin(:))

    else if (isumattf == 3) then
       ! NbTi critical field scaling:
       ! F. Schauer et al., Mechanical Quench Test, see Documentation

       alph = 0.59D0
       T_c = 9.48D0
       f_Q_crit_k(:) = 1.54D0 * sqrt(f_I) * &
            (((Bc-B_lin(:))*((T_c/(Bc**alph))-(T_u/((Bc-B_lin(:))**alph))))**(-0.5D0))
    end if

    !  Find maximal field and final f_Q
    !  This is the intersection between the critical Nb3Sn/NbTi scaling
    !  and the scaling calculated by solenoid loop calculations

    z1 = 0.5D0 * (maxval(B_lin) - minval(B_lin))
    call intersect(B_max_k,f_k,n_it,B_lin,f_Q_crit_k,n_it,z1)
    f_Q_final = find_y_nonuniform_x(z1,B_max_k,f_k,n_it)
    Y = f_Q_final

    !  Recalculate B-fields at intersection point of curves

    call scaling_calculations(f_R, f_s, f_Q_final, f_I, tfno, &
         B_max_final, B0_final)

    !  Maximum field at superconductor surface (T)

    b_maxtf = B_max_final

    !  Total stored magnetic energy calculation (GJ)
    !  W_mag2 = k2.*(bt.^2).*(f_s.^2).*f_R;  ! alternative good approximation

    W_mag = coil_energy(tfno, f_R, f_s, f_I, f_Q_final)

    !  Mass of support structure (includes casing) (tonnes)
    !  Scaling for required structure mass (Steel) from:
    !  F.C. Moon, J. Appl. Phys. 53(12) (1982) 9112
    !
    !  Values based on regression analysis by Greifswald, March 2014

    M_struc = 1.3483D0 * (1000.0D0 * W_mag)**0.7821D0
    msupstr = 1000.0D0*M_struc  !  kg

    !  Coil cross sections, etc.

    !  Winding pack toroidal, radial cross-sections (m)

    b = Y * b5B
    h = Y * h5B

    !  Winding pack cross-sectional area (m2)

    cr_area = b*h

    !  Case scaling factor

    S_case = ((W_mag/W5B)**0.75D0) / f_s

    z2 = S_case + (b5B*h5B)/(bb5b*hh5b)*(Y**2 - S_case)
    z = sqrt(z2)

    kk = 100.0D0 / 150.0D0        !  Ratio of h_in and h_out in H5B
    b_i = 0.5D0*(z*bb5b - b)      !  Toroidal casing thickness (m) (includes insulation)
    h_insu_out = (z*hh5b - h) / (kk+1)  !  Radial casing thickness facing outer side (m)
    h_insu_in = kk*h_insu_out     !  Radial casing thickness facing plasma side (m)

    !  Bean-shaped cross-section: distance from superconducting centre inner side
    !  of torus to superconducting centre outer side of torus
    !  == minimal distance in a coil

    w_coil = D_coil + 2.0D0*h_insu_in + h

    !  Estimated maximal width of coil (m) based on Helias 5-B (coil type 1)

    w_max = k4*w_coil

    !  Estimated maximal height of coil (m) based on Helias 5-B (coil type 1)

    h_max = k5*w_coil

    !  Estimated average circumference of coil (m)

    U = k6*w_coil

    !  Estimated average coil radius (m)

    r_avg = U / (2.0D0*pi)

    !  Current density in superconductor (winding pack) (MA/m2)

    j = I/cr_area

    !  Vertical ports

    !  Outer coil centre major radius, average (m)
    !  (excludes half of coil thickness, otherwise equal to rtot)

    R_occ = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth + ddwi + gapsto

    !  Average space between two coil centres (m)

    dU = 2.0D0*pi * R_occ/tfno

    !  Average toroidal port size (m)

    b_vert_avg = dU - (2.0D0*b_i + b) - 2.0D0*d_ic

    !  Maximal toroidal port size (vertical ports) (m)

    b_vert_max = 1.125D0 * b_vert_avg

    !  Maximal poloidal port size (vertical ports) (m)

    h_vert_max = f_s*4.3D0

    !  Maximal vertical port clearance area (m2)

    a_vert_max = b_vert_max*h_vert_max

    !  Horizontal ports

    b_hor_avg = b_vert_avg

    !  Maximal toroidal port size (horizontal ports) (m)

    b_hor_max = 1.125D0 * b_hor_avg

    !  Maximal poloidal port size (horizontal ports) (m)

    h_hor_max = f_s*5.0D0

    !  Maximal horizontal port clearance area (m2)

    a_hor_max = b_hor_max*h_hor_max

    !  Pass values back to PROCESS variables

    bmaxtf = b_maxtf     ! [T] maximal magnetic field on SC surface
    thkwp = h            ! [m] radial thickness of winding pack
    wwp1 = b             ! [m] toroidal thickness of winding pack
    awptf = thkwp*wwp1   ! [m^2] winding-pack cross sectional area
    casthi = h_insu_in   ! [m] coil case thickness (plasma side)
    thkcas = h_insu_out  ! [m] coil case thickness (external sides)
    casths = b_i         ! [m] coil case thickness (toroidal side)
    tinstf = 0.0D0       ! insulation, already in casing:  casthi+tinstf == h_insu_in
    awpc = (thkwp + 2.0D0*tinstf)*(wwp1 + 2.0D0*tinstf)
    ! [m^2] winding-pack cross sectional area including insulation
    tftort = wwp1 + 2.0D0*casths  ! [m] Thickness of inboard leg in toroidal direction  
    tfcth = thkcas + thkwp + casthi  ! [m] Thickness of inboard leg in radial direction  
    tfthko = tfcth                   ! [m] Thickness of outboard leg in radial direction  
    acasetf = (tfcth*tftort)-awpc    ! [m^2] Cross-sectional area of surrounding case

    if (isumattf == 3) tftmp = T_u  ! [K] Helium coolant temperature for NbTi

    !  Single turn outputs according to PROCESS variables

    t_no = awptf / (t_w_i**2)  !  estimated number of turns for a given turn size
    t_no2 = 156.0D0 * Y**2     !  estimated number of turns for Helias 5-B turns
    cpttf = I*1.0D6 / t_no     ! [A] current per turn - estimation
    cpttf2 = 86.0D3            ! [A] current per turn - H5B

    !  Radius of rounded corners of cable space inside conduit
    !  0.75 taken from the former PROCESS model

    rbcndut = thwcndut*0.75D0

    arealeg = tfcth*tftort  ! [m^2] overall coil cross-sectional area
    rtfcin = rmajor - (0.5D0*tfcth+gapds+ddwi+shldith+blnkith+fwith+scrapli+rminor)
    ! [m] radius of centre of inboard leg, average
    rcoil = rtfcin + 0.5D0*tfcth  ! [m] radius of outer edge of inboard leg, average
    tfareain = tfno*tfcth*tftort  ! [m^2] Total area of all coil legs
    tfarea_sc = tfno*awptf        ! [m^2] Total area of all coil winding packs
    ritfc = tfno * I * 1.0D6      ! [A] Total current in ALL coils
    oacdcp = ritfc/tfareain       ! [A / m^2] overall current density
    rbmax = rcoil                 ! [m] radius of peak field occurrence, average
                                  !     N.B. different to tokamak SCTF calculation
    hmax = 0.5D0*h_max - tfcth    ! [m] maximum half-height of coil
    tfboreh = D_coil              ! [m] estimated horizontal coil bore
    tfborev = 2.0D0*hmax          ! [m] estimated vertical coil bore
    tfleng = U                    ! [m] estimated average length of a coil

    estotf = W_mag/tfno           ! [GJ] magnetic energy per coil

    !jwptf = ritfc/(tfno*awptf)
    jwptf = j*1.0D6               ! [A/m^2] winding pack current density

    !leno = sqrt(cpttf/jwptf)          
    leno = t_w_i                  ! [m] Dimension of square cross-section of each turn

    ! [m] Dimension of square cable space inside insulation
    !     and case of the conduit of each turn

    leni = leno - 2.0D0*(thwcndut + thicndut)  !  leni = t_w

    ! [m^2] Cross-sectional area of cable space per turn

    acstf = leni**2 - (4.0D0-pi)*rbcndut**2

    ! [m^2] Cross-sectional area of conduit case per turn

    acndttf = (leni + 2.0D0*thwcndut)**2 - acstf

    !  Total number of turns per coil

    turnstf = t_no    

    ! [m^2] Total conductor cross-sectional area, taking account of void area

    acond = acstf*turnstf * (1.0D0-vftf)

    ! [m^2] Void area in cable, for He

    avwp = acstf*turnstf*vftf

    ! [m^2] Insulation area (not including ground-wall)

    aiwp = turnstf * (leno**2 - acndttf - acstf)

    ! [m^2] Structure area for cable

    aswp = turnstf*acndttf

    ! [m] Half-width of side of coil nearest torus centreline

    tfocrn = 0.5D0*tftort

    ! [m] Half-width of side of coil nearest plasma

    tficrn = 0.5D0*tftort

    ! [m^2] Total surface area of coil side facing plasma: inboard region

    tfsai = tfno*tftort * 0.5D0*tfleng

    ! [m^2] Total surface area of coil side facing plasma: outboard region

    tfsao = tfsai  !  depends, how 'inboard' and 'outboard' are defined

    ! [m^2] Total surface area of toroidal shells covering coils

    tfcryoarea = 2.0D0 * tfleng * twopi*0.5D0*(rtfcin+rtot)

    !  Masses of conductor constituents

    ! [kg] Mass of case
    !  (no need for correction factors as is the case for tokamaks)

    whtcas = tfleng*acasetf * dcase

    ! [kg] mass of Superconductor

    whtconsc = tfleng*turnstf*acstf*(1.0D0-vftf)*(1.0D0-fcutfsu) * dcond(isumattf)

    ! [kg] mass of Copper in conductor

    whtconcu = tfleng*turnstf*acstf*(1.0D0-vftf)*fcutfsu * dcopper

    ! [kg] mass of Steel conduit (sheath)

    whtconsh = tfleng*turnstf*acndttf * denstl

    ! [kg] Total conductor mass

    whtcon = whtconsc + whtconcu + whtconsh

    ! [kg] Total coil mass

    whttf = (whtcas + whtcon)*tfno

    ! [kg] Total support structure mass
    ! msupstr includes the casing mass, so this needs to be subtracted
    ! Currently, this is assumed to comprise all the machine's support
    ! structural mass

    aintmass = msupstr - (whtcas*tfno)

    ! [m] Maximum available poloidal extent for horizontal ports

    hportpmax = h_hor_max

    ! [m] Maximum available toroidal extent for horizontal ports

    hporttmax = b_hor_max

    ! [m2] Maximum available area for horizontal ports

    hportamax = a_hor_max

    ! [m] Maximum available poloidal extent for vertical ports

    vportpmax = h_vert_max

    ! [m] Maximum available toroidal extent for vertical ports

    vporttmax = b_vert_max

    ! [m2] Maximum available area for vertical ports

    vportamax = a_vert_max

    if (iprint == 1) call stcoil_output(outfile)

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine scaling_calculations(f_R, f_s, f_Q, f_I, Nsp, B_abs_max, B_abs_mittel)

      !+ad_name  scaling_calculations
      !+ad_summ  Routine that calculates the important magnetic field values
      !+ad_summ  for stellarator coils
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  F Warmer, IPP Greifswald
      !+ad_auth  F Schauer, IPP Greifswald
      !+ad_cont  None
      !+ad_args  f_R          : input real : major radius scaling factor
      !+ad_args  f_s          : input real : coil width scaling factor
      !+ad_args  f_Q          : input real : coil cross-section scaling factor
      !+ad_args  f_I          : input real : current scaling factor
      !+ad_args  Nsp          : input real : number of coils
      !+ad_args  B_abs_max    : output real : maximum field at superconductor surface (T)
      !+ad_args  B_abs_mittel : output real : magnetic field at plasma centre (T)
      !+ad_desc  This routine calculates the magnetic field at the plasma centre
      !+ad_desc  and the maximum field value on the superconductor using scaling factors
      !+ad_desc  to adjust the values calculated for the Helias 5-B design.
      !+ad_desc  <P>The coils are approximated by circular filaments, shifted and
      !+ad_desc  tilted with respect to one another. The field calculations use elliptic
      !+ad_desc  integrals.
      !+ad_prob  None
      !+ad_call  cartesian_vectors
      !+ad_call  solenoid
      !+ad_hist  03/03/14 PJK/FW Initial version
      !+ad_stat  Okay
      !+ad_docs  The Stellarator Coil model for the Systems code PROCESS,
      !+ad_docc  F. Warmer, F. Schauer, IPP Greifswald, October 2013
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use cartesian_vectors

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: f_r, f_s, f_q, f_i, nsp
      real(kind(1.0D0)), intent(out) :: b_abs_max, b_abs_mittel

      !  Local variables

      integer :: i,n_h,n_b,nc,nsp_int
      real(kind(1.0D0)) :: b,b_abs_zentr,b_abs_zwischen,b_max_equiv,b_x_zentr, &
           b_x_zwischen,b_y_zentr,b_y_zwischen,bi_x,bi_y,bir,bir_1,biz,biz_1, &
           di_mean,dphi,h,il,isp,phi,r_q,rg,ri,rk,rri,vz_r,zi
      real(kind(1.0D0)), dimension(3,5) :: b_ptot
      real(kind(1.0D0)), allocatable, dimension(:,:) :: b_p
      type(vector) :: ax_coili,c_ci,n_ax_ci,n_z,n_rcoili,p
      type(vector), dimension(3) :: p_i

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      nsp_int = int(nsp)

      !  Each coil is modelled using n_h*n_b filamentary circular conductors

      n_h = 13
      n_b = 12

      Rg = Rg5B*f_R  !  major radius
      Rk = Rk5B*f_s  !  coil 'minor' radius
      h = h5B*f_Q    !  coil radial cross-section
      b = b5B*f_Q    !  coil toroidal cross-section
      R_Q = RQ5B*f_Q !  equivalent coil cross-section radius

      dphi = 2.0D0*pi/Nsp  !  (average) toroidal angle between two adjacent coils

      Isp = I5B*f_I       !  coil current
      IL = Isp/(n_h*n_b)  !  filament current

      Di_mean = 2.0D0*Rk  !  mean coil diameter
      rri = Rk - 0.5D0*h  !  radius of the inner edge of the coil

      !  Set up vectors describing locations of points of interest

      !  Point at centre of a coil located in the x-z (y=0) plane

      P_i(1)%x = Rg
      P_i(1)%y = 0.0D0
      P_i(1)%z = 0.0D0

      !  Point on the toroidal axis between the above coil and the one adjacent to it

      P_i(2)%x = Rg*cos(0.5D0*dphi)
      P_i(2)%y = Rg*sin(0.5D0*dphi)
      P_i(2)%z = 0.0D0

      !  Location of maximum B-field, at the plasma-facing edge of the
      !  inboard part of coil

      P_i(3)%x = Rg-rri
      P_i(3)%y = 0.0D0
      P_i(3)%z = 0.0D0

      !  Normal vector to the x-y plane

      n_z%x = 0.0D0
      n_z%y = 0.0D0
      n_z%z = 1.0D0

      !  Set up arrays to contain data for each coil, and totals

      allocate(B_P(Nsp_int,5)) ; B_P = 0.0D0
      B_Ptot(:,:) = 0.0D0

      !  Loop to calculate the field on the toroidal axis

      do i = 1,3  !  loop over reference points of interest P_i
         P = P_i(i)

         do NC = 1,Nsp_int  !  loop over coils

            phi = dphi*(NC-1) !  toroidal angle of coil nc

            !  Point at centre of coil nc

            C_Ci%x = Rg*cos(phi)
            C_Ci%y = Rg*sin(phi)
            C_Ci%z = 0.0D0

            !  Axis direction of coil nc

            Ax_coilI%x = -sin(phi)
            Ax_coilI%y = cos(phi)
            Ax_coilI%z = 0.0D0
            n_Ax_cI = unit_vector(Ax_coilI)  !  unit vector

            ! Unit vector from the origin in the direction of the centre of coil nc

            n_rCoilI = n_Ax_cI.cross.n_z

            !  Radial distance from the reference point P

            ri = modulus( n_Ax_cI.cross.(P-C_Ci) )

            !  Sign of Br (to be calculated in solenoid below);
            !  negative if P is radially inside the coil axis

            vz_r = sign(1.0D0,(n_rCoilI.dot.P)-Rg)

            !  Axial (toroidal) distance from the coil centre to point P

            zi = n_Ax_cI.dot.(P-C_Ci)

            !  Calculate magnetic field components in local coordinates
            !  relative to point P, assuming unit current

            call solenoid(rri,h,b,n_h,n_b,ri,zi,BiR_1,BiZ_1)

            !  Radial component of B in local coordinate system,
            !  scaled with current and with sign correction

            BiR = vz_r * BiR_1 * IL

            !  Toroidal component of B in local coordinate system

            BiZ = BiZ_1 * IL

            !  Convert to torus coordinate system

            Bi_X = BiR*cos(phi) - BiZ*sin(phi)  !  radial
            Bi_Y = BiR*sin(phi) + BiZ*cos(phi)  !  toroidal

            !  Populate data array for this coil

            B_P(NC,1) = NC       !  Coil number
            B_P(NC,2) = C_Ci%x   !  Central point, x
            B_P(NC,3) = C_Ci%y   !  Central point, y
            B_P(NC,4) = Bi_X     !  Radial field component
            B_P(NC,5) = Bi_Y     !  Toroidal field component

         end do

         !  Sum the data values over the coils at each reference point

         !B_Ptot(i,1) = sum(B_P(:,1))  !  not necessary
         !B_Ptot(i,2) = sum(B_P(:,2))  !  not necessary
         !B_Ptot(i,3) = sum(B_P(:,3))  !  not necessary
         B_Ptot(i,4) = sum(B_P(:,4))  !  total radial field
         B_Ptot(i,5) = sum(B_P(:,5))  !  total toroidal field

      end do

      !  Total field at coil centre, i.e. on plasma axis

      B_X_Zentr = B_Ptot(1,4)
      B_Y_Zentr = B_Ptot(1,5)
      B_abs_Zentr = sqrt(B_X_Zentr**2 + B_Y_Zentr**2)

      !  Total field halfway between coil centres, also on plasma axis

      B_X_zwischen = B_Ptot(2,4)
      B_Y_zwischen = B_Ptot(2,5)
      B_abs_Zwischen = sqrt(B_X_zwischen**2 + B_Y_zwischen**2)

      !  Average of the above

      B_abs_mittel = 0.5D0*(B_abs_Zentr + B_abs_Zwischen)
      !B_abs_mittel = B_abs_mittel * 0.950923D0  !  (commented out by FW)

      !  Total field at inside (plasma-facing) edge of inner part of coil,
      !  normalised to Helias 5-B due to non-circular modular coils

      B_abs_max = 1.11062D0 * sqrt(B_Ptot(3,4)**2 + B_Ptot(3,5)**2)

      !  Equivalent field calculated for planar (tokamak-like TF) coils,
      !  which typically gives much lower values!
      !  (not used)
      !B_max_equiv = 2.0D-7 * Isp / R_Q  !  = mu0.I / (2.pi.R)

    end subroutine scaling_calculations

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine solenoid(Ri,H,L,nR,nZ,r,z, Br, Bz)

      !+ad_name  solenoid
      !+ad_summ  Routine that calculates the magnetic field due to a circular coil
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  F Warmer, IPP Greifswald
      !+ad_auth  F Schauer, IPP Greifswald
      !+ad_cont  None
      !+ad_args  Ri : input real : radius of coil inner surface (m)
      !+ad_args  H  : input real : coil cross-sectional radial width (m)
      !+ad_args  L  : input real : coil cross-sectional axial width (m)
      !+ad_args  nR : input integer : number of radial filaments to use
      !+ad_args  nZ : input integer : number of axial filaments to use
      !+ad_args  r  : input real : radial distance of point of interest from
      !+ad_argc                    coil centre (m)
      !+ad_args  z  : input real : axial distance of point of interest from coil (m)
      !+ad_args  Br : output real : magnetic field radial component at (r,z) (T)
      !+ad_args  Bz : output real : magnetic field axial component at (r,z) (T)
      !+ad_desc  This routine calculates the magnetic field at a point (R,Z)
      !+ad_desc  due to unit current flowing in a circular planar coil centred
      !+ad_desc  at the origin, lying in the x-y plane.
      !+ad_desc  <P>The coil is approximated by nR*nZ circular filaments, which
      !+ad_desc  take account of the coil conductor cross-section.
      !+ad_desc  The field calculations use elliptic integrals.
      !+ad_prob  None
      !+ad_call  ellipke
      !+ad_hist  03/03/14 PJK/FW Initial version
      !+ad_stat  Okay
      !+ad_docs  The Stellarator Coil model for the Systems code PROCESS,
      !+ad_docc  F. Warmer, F. Schauer, IPP Greifswald, October 2013
      !+ad_docs  D. Bruce Montgomery, Solenoid Magnet Design, Section 8.4.1, p.237,
      !+ad_docc  Wiley-Interscience (1968)
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: ri,h,l,r,z
      integer, intent(in) :: nR, nZ
      real(kind(1.0D0)), intent(out) :: Br, Bz

      !  Local variables

      integer :: ii,jj
      real(kind(1.0D0)) :: ra,b,da,dz,sumhr,sumhz,a,zi,ksq,kk,ek,hz,hr,roa,zoa

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      Ra = Ri + H
      b = 0.5D0*L
      Da = (Ra - Ri) / nR
      Dz = 0.5D0 * L / nZ

      SumHr = 0.0D0
      SumHz = 0.0D0

      do jj = 1,nR
         a = Ri + (jj - 0.5D0)*Da
         roa = r/a

         do ii = 1,nZ
            zi =  (2.0D0*ii - 1.0D0) * Dz - b
            zoa = (z-zi)/a

            ksq = 4.0D0*roa/((1 + roa)**2 + zoa**2)
            call ellipke(ksq,kk,ek)

            Hz = 2.0D0/(a*4.0D0*pi) * (1.0D0/sqrt((1.0D0 + roa)**2 + zoa**2)) &
                 * ( kk + (1.0D0 - roa**2 - zoa**2) / ((1.0D0-roa)**2 + zoa**2)*ek )

            Hr = 2.0D0/(a*4.0D0*pi) * (z-zi)/max(1.0D-6,r) * &
                 (1.0D0/sqrt((1 + roa)**2 + zoa**2)) &
                 * (-kk + (1.0D0 + roa**2 + zoa**2) / ((1.0D0-roa)**2 + zoa**2)*ek )

            SumHr = SumHr + Hr
            SumHz = SumHz + Hz
         end do
      end do

      Br = rmu0 * SumHr
      Bz = rmu0 * SumHz

    end subroutine solenoid

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    function coil_energy(Nsp,f_R,f_s,f_I,f_Q)

      !+ad_name  coil_energy
      !+ad_summ  Routine that calculates the stored energy in the stellarator coils
      !+ad_type  Function returning real
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_auth  F Warmer, IPP Greifswald
      !+ad_auth  F Schauer, IPP Greifswald
      !+ad_cont  None
      !+ad_args  Nsp : input real : number of coils
      !+ad_args  f_R : input real : major radius scaling factor
      !+ad_args  f_s : input real : coil width scaling factor
      !+ad_args  f_I : input real : current scaling factor
      !+ad_args  f_Q : input real : coil cross-section scaling factor
      !+ad_desc  This routine calculates the total stored energy in the modular
      !+ad_desc  stellarator coils.
      !+ad_desc  <P>The coils are approximated by a number of circular planar
      !+ad_desc  filaments, which take account of the coil conductor cross-section.
      !+ad_desc  The field calculations use elliptic integrals.
      !+ad_prob  None
      !+ad_call  ellipke
      !+ad_call  sumup3
      !+ad_call  tril
      !+ad_hist  03/03/14 PJK Initial version
      !+ad_hist  25/09/14 PJK Corrected do-loop argument
      !+ad_stat  Okay
      !+ad_docs  The Stellarator Coil model for the Systems code PROCESS,
      !+ad_docc  F. Warmer, F. Schauer, IPP Greifswald, October 2013
      !+ad_docs  S. Babic et al, IEEE Transactions on Magnetics, 46, no.9 (2010) 3591
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      real(kind(1.0D0)) :: coil_energy

      !  Arguments

      real(kind(1.0D0)), intent(in) :: Nsp,f_R,f_s,f_I,f_Q

      !  Local variables

      integer :: nsp_int,n_h,n_b,zaehler,n,ii,j,indn
      real(kind(1.0D0)) :: a,alpha,ao,b,beta,c,cf,d_phi,delta,dtheta,ek, &
           energie_gj,gamma,il,indukt,integral,isp,kk,l,ll,m11,p1,p2,p3,p4, &
           p5,phi,r_q,rg,rk,rk5bcorr,rp,rs,theta,vz,xc,yc,zc,phi_ind,vo,k,ksq,psi
      real(kind(1.0D0)), allocatable, dimension(:) :: m,integrand
      real(kind(1.0D0)), allocatable, dimension(:,:) :: indmat,indmatu,indmato,indmattot

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      nsp_int = int(nsp)

      !  Correction factor, as the area inside a modular coil is smaller
      !  than the area in a ring with the same circumference

      cf = 0.8824D0
      Rk5Bcorr = cf*rk5b  !  Corrected coil minor radius for Helias 5-B

      !  Each coil is modelled using n_h*n_b filamentary circular conductors

      n_h = 13
      n_b = 12

      Rg = Rg5B*f_R      !  major radius
      Rk = Rk5Bcorr*f_s  !  coil 'minor' radius
      R_Q = RQ5B*f_Q     !  equivalent coil cross-section radius

      Isp = I5B*f_I      !  coil current
      IL = Isp/(n_h*n_b) !  filament current

      !  Coil self-inductance

      M11 = rmu0 * Rk * (log(Rk/R_Q) + 0.25D0)

      allocate(M(nsp_int-1)) ; M = 0.0D0

      !  Number of points for integration

      n = 100

      allocate(integrand(n))

      dtheta = 2.0D0*pi/nsp
      d_phi = 2.0D0*pi/(n-1)
      zaehler = 0  !  counter

      do j = 1,nsp_int-1  !  loop over other (secondary) coils

         !  Toroidal angle of secondary coil relative to primary

         theta = j*dtheta
         if (theta == 0.5D0*pi) theta = theta + 1.0D-5
         if (theta == 1.5D0*pi) theta = theta + 1.0D-5

         !  Radii of primary and secondary coils

         Rp = Rk ; Rs = Rk

         !  Sign for p1 to p5

         vz = 1.0D0

         !  Location of centre of secondary coil

         xc = 0.0D0
         yc = -Rg * (1.0D0-cos(theta))
         zc =  Rg * sin(theta)

         !  Equation for plane containing secondary coil;
         !  ax + by + cz + d = 0

         a = 0.0D0
         b = -sin(theta)
         c =  cos(theta)

         alpha = Rs/Rp
         beta = xc/Rp
         gamma = yc/Rp
         delta = zc/Rp

         l = sqrt(a**2 + c**2)
         ll = sqrt(a**2 + b**2 + c**2)

         p1 = vz*gamma*c/l
         p2 = -vz*(beta*l**2 + gamma*a*b)/(l*ll)
         p3 = alpha*c/ll
         p4 = -vz*(beta*a*b-gamma*l**2 + delta*b*c)/(l*ll)
         p5 = -vz*(beta*c-delta*a)/l

         !  Find integrand along secondary coil circumference

         do ii = 1,n
            phi = (ii-1)*d_phi
            Ao = 1.0D0 + alpha**2 + beta**2 + gamma**2 + delta**2 &
                 + 2.0D0*alpha*(p4*cos(phi) + p5*sin(phi))
            Vo = sqrt(alpha**2*((1.0D0-(b*c/(l*ll))**2)*cos(phi)**2 &
                 + (c/l)**2*sin(phi)**2 + a*b*c/(l**2*ll)*sin(phi*2)) &
                 + beta**2 + gamma**2 - vz*2.0D0*alpha*(beta*a*b - gamma*l**2) &
                 / (l*ll) * cos(phi) - vz*2.0D0*alpha*beta*c/l*sin(phi))
            ksq = 4.0D0*Vo / (Ao + 2.0D0*Vo)
            call ellipke(ksq,kk,ek)
            psi = (1.0D0 - 0.5D0*ksq)*kk - ek

            integrand(ii) = (p1*cos(phi) + p2*sin(phi) + p3)*psi &
                 / (sqrt(ksq)*Vo**1.5D0)
         end do

         zaehler = zaehler+1

         !  Perform integral

         call sumup3(d_phi, integrand, integral, n)

         M(zaehler) = 4.0D-7 * Rs * integral

      end do

      !  Build full mutual inductance matrix

      allocate(indmat(nsp_int,nsp_int),indmatu(nsp_int,nsp_int), &
           indmato(nsp_int,nsp_int),indmattot(nsp_int,nsp_int))
      IndMat = 0.0D0

      !  Self-inductance

      IndMat(1,1) = M11

      !  Fill remainder of first column

      IndMat(2:Nsp_int,1) = M(:)

      !  Fill remainder of lower triangle including diagonal

      do indn = 2,nsp_int
         IndMat(indN:Nsp_int,indN) = IndMat(indN-1:Nsp_int-1,indN-1)
      end do

      !  Extract only the lower triangular matrix elements...

      call tril(IndMat,nsp_int,IndMatU)

      !  Transpose this to get the upper triangular matrix elements...

      IndMatO = transpose(IndMatU)

      !  Add the upper triangular matrix to the lower triangle + diagonal
      !  to get the full inductance matrix in Henries

      IndMatTot = IndMat + IndMatO

      !  Sum of the self and mutual inductances (H)

      Indukt = sum(IndMatTot)

      !  Total stored energy (GJ) = 0.5 L.I^2

      Energie_GJ = 0.5D-9 * Indukt * Isp**2

      coil_energy = Energie_GJ

    end function coil_energy

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine intersect(x1,y1,n1,x2,y2,n2,x)

      !+ad_name  intersect
      !+ad_summ  Routine to find the x (abscissa) intersection point of two curves
      !+ad_summ  each defined by tabulated (x,y) values
      !+ad_type  Subroutine
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  N/A
      !+ad_args  x1(1:n1) : input real array : x values for first curve
      !+ad_args  y1(1:n1) : input real array : y values for first curve
      !+ad_args  n1       : input integer : length of arrays x1, y1
      !+ad_args  x2(1:n2) : input real array : x values for first curve
      !+ad_args  y2(1:n2) : input real array : y values for first curve
      !+ad_args  n2       : input integer : length of arrays x2, y2
      !+ad_args  x        : input/output real : initial x value guess on entry;
      !+ad_argc             x value at point of intersection on exit
      !+ad_desc  This routine estimates the x point (abscissa) at which two curves
      !+ad_desc  defined by tabulated (x,y) values intersect, using simple
      !+ad_desc  linear interpolation and the Newton-Raphson method.
      !+ad_desc  The routine will stop with an error message if no crossing point
      !+ad_desc  is found within the x ranges of the two curves.
      !+ad_prob  None
      !+ad_call  find_y_nonuniform_x
      !+ad_call  report_error
      !+ad_hist  03/03/14 PJK Initial version; code will warn the user if there
      !+ad_hisc               is a problem, but not stop
      !+ad_hist  26/06/14 PJK Added error handling
      !+ad_stat  Okay
      !+ad_docs  None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      integer, intent(in) :: n1, n2
      real(kind(1.0D0)), dimension(n1), intent(in) :: x1, y1
      real(kind(1.0D0)), dimension(n2), intent(in) :: x2, y2

      real(kind(1.0D0)), intent(inout) :: x

      real(kind(1.0D0)) :: dx,xmin,xmax,ymin,ymax
      real(kind(1.0D0)) :: y01,y02,y,yleft,yright,epsy
      integer :: i, nmax = 100

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Find overlapping x range

      xmin = max(minval(x1),minval(x2))
      xmax = min(maxval(x1),maxval(x2))

      if (xmin >= xmax) then
         fdiags(1) = minval(x1) ; fdiags(2) = minval(x2)
         fdiags(3) = maxval(x1) ; fdiags(4) = maxval(x2)
         call report_error(111)
      end if

      !  Ensure input guess for x is within this range

      if (x < xmin) then
         x = xmin
      else if (x > xmax) then
         x = xmax
      else
         continue  !  x already in range
      end if

      !  Find overall y range, and set tolerance
      !  in final difference in y values

      ymin = min(minval(y1),minval(y2))
      ymax = max(maxval(y1),maxval(y2))

      epsy = 1.0D-6 * (ymax-ymin)

      !  Finite difference dx

      dx = 0.01D0/max(n1,n2) * (xmax-xmin)

      i = 0
      converge: do
         i = i+1

         !  Find difference in y values at x

         y01 = find_y_nonuniform_x(x,x1,y1,n1)
         y02 = find_y_nonuniform_x(x,x2,y2,n2)
         y = y01 - y02

         if (abs(y) < epsy) exit converge

         !  Find difference in y values at x+dx

         y01 = find_y_nonuniform_x(x+dx,x1,y1,n1)
         y02 = find_y_nonuniform_x(x+dx,x2,y2,n2)
         yright = y01 - y02

         !  Find difference in y values at x-dx

         y01 = find_y_nonuniform_x(x-dx,x1,y1,n1)
         y02 = find_y_nonuniform_x(x-dx,x2,y2,n2)
         yleft = y01 - y02

         !  Adjust x using Newton-Raphson method

         x = x - 2.0D0*dx*y/(yright-yleft)

         if (x < xmin) then
            fdiags(1) = x ; fdiags(2) = xmin
            call report_error(112)
            x = xmin
            exit converge
         end if
         if (x > xmax) then
            fdiags(1) = x ; fdiags(2) = xmax
            call report_error(113)
            x = xmax
            exit converge
         end if
         if (i > nmax) then
            idiags(1) = i ; idiags(2) = nmax
            call report_error(114)
            exit converge
         end if

      end do converge

    end subroutine intersect

  end subroutine stcoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcoil_output(outfile)

    !+ad_name  stcoil_output
    !+ad_summ  Writes stellarator modular coil output to file
    !+ad_type  Subroutine
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  outfile : input integer : output file unit
    !+ad_desc  This routine writes the stellarator modular coil results
    !+ad_desc  to the output file.
    !+ad_prob  None
    !+ad_call  oblnkl
    !+ad_call  ocmmnt
    !+ad_call  oheadr
    !+ad_call  osubhd
    !+ad_call  ovarre
    !+ad_hist  03/03/14 PJK Initial version, based on outtf
    !+ad_hist  19/06/14 PJK Removed sect?? flags
    !+ad_hist  30/07/14 PJK Renamed borev to tfborev
    !+ad_hist  31/07/14 PJK Removed aspcstf
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    integer :: i
    real(kind(1.0D0)) :: ap

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call oheadr(outfile,'Modular Coils')

    call osubhd(outfile,'General Coil Parameters :')

    call ovarre(outfile,'Number of modular coils','(tfno)',tfno)
    call ovarre(outfile,'Cross-sectional area per coil (m2)','(tfarea/tfno)', &
         tfareain/tfno)
    call ovarre(outfile,'Total inboard leg radial thickness (m)','(tfcth)',tfcth)
    call ovarre(outfile,'Total outboard leg radial thickness (m)','(tfthko)',tfthko)
    call ovarre(outfile,'Inboard leg outboard half-width (m)','(tficrn)',tficrn)
    call ovarre(outfile,'Inboard leg inboard half-width (m)','(tfocrn)',tfocrn)
    call ovarre(outfile,'Outboard leg toroidal thickness (m)','(tftort)',tftort)
    call ovarre(outfile,'Mean coil circumference (m)','(tfleng)',tfleng)
    call ovarre(outfile,'Total current (MA)','(ritfc/1.D6)',1.0D-6*ritfc)
    call ovarre(outfile,'Winding pack current density (A/m2)','(jwptf)',jwptf)
    call ovarre(outfile,'Overall current density (A/m2)','(oacdcp)',oacdcp)
    call ovarre(outfile,'Maximum field on superconductor (T)','(bmaxtf)',bmaxtf)
    call ovarre(outfile,'Stored energy per coil (GJ)','(estotf)',estotf)
    call ovarre(outfile,'Total mass of coils (kg)','(whttf)',whttf)

    call osubhd(outfile,'Coil Geometry :')
    call ovarre(outfile,'Inboard leg centre radius (m)','(rtfcin)',rtfcin)
    call ovarre(outfile,'Outboard leg centre radius (m)','(rtot)',rtot)
    call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax)
    call ovarre(outfile,'Clear horizontal bore (m)','(tfboreh)',tfboreh)
    call ovarre(outfile,'Clear vertical bore (m)','(tfborev)',tfborev)

    call osubhd(outfile,'Conductor Information :')
    call ovarre(outfile,'Superconductor mass per coil (kg)','(whtconsc)',whtconsc)
    call ovarre(outfile,'Copper mass per coil (kg)','(whtconcu)',whtconcu)
    call ovarre(outfile,'Steel conduit mass per coil (kg)','(whtconsh)',whtconsh)
    call ovarre(outfile,'Total conductor cable mass per coil (kg)','(whtcon)',whtcon)
    call ovarre(outfile,'Cable conductor + void area (m2)','(acstf)',acstf)
    call ovarre(outfile,'Cable space coolant fraction','(vftf)',vftf)
    call ovarre(outfile,'Conduit case thickness (m)','(thwcndut)',thwcndut)
    call ovarre(outfile,'Cable insulation thickness (m)','(thicndut)',thicndut)

    ap = acond + aswp + aiwp + avwp

    call osubhd(outfile,'Winding Pack Information :')
    call ovarre(outfile,'Conductor fraction of winding pack','(acond/ap)',acond/ap)
    call ovarre(outfile,'Copper fraction of conductor','(fcutfsu)',fcutfsu)
    call ovarre(outfile,'Structure fraction of winding pack','(aswp/ap)',aswp/ap)
    call ovarre(outfile,'Insulator fraction of winding pack','(aiwp/ap)',aiwp/ap)
    call ovarre(outfile,'Helium fraction of winding pack','(avwp/ap)',avwp/ap)
    call ovarre(outfile,'Winding radial thickness (m)','(thkwp)',thkwp)
    call ovarre(outfile,'Winding toroidal thickness (m)','(wwp1)',wwp1)
    call ovarre(outfile,'Ground wall insulation thickness (m)','(tinstf)',tinstf)
    call ovarre(outfile,'Number of turns per coil','(turnstf)',turnstf)
    call ovarre(outfile,'Current per turn (A)','(cpttf)',cpttf)

    call osubhd(outfile,'External Case Information :')

    call ovarre(outfile,'Case thickness, plasma side (m)','(casthi)',casthi)
    call ovarre(outfile,'Case thickness, outer side (m)','(thkcas)',thkcas)
    call ovarre(outfile,'Case toroidal thickness (m)','(casths)',casths)
    call ovarre(outfile,'Case area per coil (m2)','(acasetf)',acasetf)
    call ovarre(outfile,'External case mass per coil (kg)','(whtcas)',whtcas)

    call osubhd(outfile,'Available Space for Ports :')

    call ovarre(outfile,'Max toroidal size of vertical ports (m)', &
         '(vporttmax)',vporttmax)
    call ovarre(outfile,'Max poloidal size of vertical ports (m)', &
         '(vportpmax)',vportpmax)
    call ovarre(outfile,'Max area of vertical ports (m2)', &
         '(vportamax)',vportamax)
    call ovarre(outfile,'Max toroidal size of horizontal ports (m)', &
         '(hporttmax)',hporttmax)
    call ovarre(outfile,'Max poloidal size of horizontal ports (m)', &
         '(hportpmax)',hportpmax)
    call ovarre(outfile,'Max area of horizontal ports (m2)', &
         '(hportamax)',hportamax)

  end subroutine stcoil_output

end module stellarator_module
