! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module impurity_radiation_module

  !+ad_name  impurity_radiation_module
  !+ad_summ  Module for new impurity radiation calculations
  !+ad_type  Module
  !+ad_auth  H Lux, CCFE, Culham Science Centre
  !+ad_auth  R Kemp, CCFE, Culham Science Centre
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  initialise_imprad
  !+ad_cont  init_imp_element
  !+ad_cont  import_impdata
  !+ad_cont  z2index
  !+ad_cont  element2index
  !+ad_cont  impradprofile
  !+ad_cont  pbremden
  !+ad_cont  pimpden
  !+ad_cont  fradcore
  !+ad_cont  Zav_of_te
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  bremsstrahlung and line radiation of impurities
  !+ad_desc  including H  and He, assuming a coronal equilibrium.
  !+ad_desc  <P>The model is only valid for T &gt; 30 eV. For some impurity
  !+ad_desc  species there is also an upper temperature limit of T &lt; 40 keV.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  error_handling
  !+ad_call  profiles_module
  !+ad_hist  13/12/13 HL  Initial version of module
  !+ad_hist  13/05/14 PJK Initial PROCESS implementation
  !+ad_hist  02/06/14 PJK Added impvar, fimpvar
  !+ad_hist  17/06/14 PJK Added impdir
  !+ad_hist  26/06/14 PJK Added error_handling
  !+ad_hist  03/09/14 HL  Added average atomic charge values to data;
  !+ad_hisc               changed directory containing datafiles
  !+ad_hist  17/09/14 PJK Changed default values
  !+ad_hist  18/09/14 PJK Updated/re-ordered comments
  !+ad_hist  24/09/14 PJK Path name now set automatically (via make)
  !+ad_hist  25/09/14 PJK Corrected root.dir include syntax
  !+ad_hist  06/10/14 PJK Changed impvar default from 10 to 9
  !+ad_hist  08/12/14 PJK Changed impdir label
  !+ad_hist  29/03/16 HL Added coreradiationfraction
  !+ad_stat  Okay
  !+ad_docs  Johner, Fusion Science and Technology 59 (2011), pp 308-349
  !+ad_docs  Sertoli, private communication
  !+ad_docs  Kallenbach et al., Plasma Phys. Control. Fus. 55(2013) 124041
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use error_handling
  use profiles_module

  implicit none

  private
  public :: initialise_imprad, impradprofile, z2index, element2index, fradcore
  public :: imp_dat, Zav_of_te

  !+ad_vars  imprad_model /1/ : switch for impurity radiation model:<UL>
  !+ad_varc               <LI>  = 0 original ITER 1989 model
  !+ad_varc               <LI>  = 1 2014 multi-impurity, pedestal profile model </UL>
  !+ad_varc  (Whichever model is used, it is recommended to turn on
  !+ad_varc  constraint eqn.17 with iteration variable 28: fradpwr.)
  integer, public :: imprad_model = 1

  !+ad_vars  nimp /14/ FIX : number of ion species in impurity radiation model
  integer, public, parameter :: nimp = 14

  !+ad_vars  coreradius /0.6/ : normalised radius defining the 'core' region
  real(kind(1.0D0)), public :: coreradius = 0.6D0

  !+ad_vars  coreradiationfraction /1.0/ : fraction of radiation from 'core' region that is subtracted from the loss power
  real(kind(1.0D0)), public :: coreradiationfraction = 1.0D0

  !+ad_vars  fimp(nimp) /1.0,0.1,0.02,0.0,0.0,0.0,0.0,0.0,0.0016,0.0,0.0,0.0,0.0,0.0/ :
  !+ad_varc         impurity number density fractions relative to electron density
  !+ad_varc         (iteration variable 102 is fimp(impvar))
  real(kind(1.0D0)), public, dimension(nimp) :: fimp = &
       (/ 1.0D0, 0.1D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.00D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
  !+ad_vars  imp_label(nimp) : impurity ion species names:<UL>
  character(len=2), public, dimension(nimp) :: imp_label = (/ &
  !+ad_varc  <LI> ( 1)  Hydrogen  (fraction calculated by code)
       'H_', &
  !+ad_varc  <LI> ( 2)  Helium
       'He', &
  !+ad_varc  <LI> ( 3)  Beryllium
       'Be', &
  !+ad_varc  <LI> ( 4)  Carbon
       'C_', &
  !+ad_varc  <LI> ( 5)  Nitrogen
       'N_', &
  !+ad_varc  <LI> ( 6)  Oxygen
       'O_', &
  !+ad_varc  <LI> ( 7)  Neon
       'Ne', &
  !+ad_varc  <LI> ( 8)  Silicon
       'Si', &
  !+ad_varc  <LI> ( 9)  Argon
       'Ar', &
  !+ad_varc  <LI> (10)  Iron
       'Fe', &
  !+ad_varc  <LI> (11)  Nickel
       'Ni', &
  !+ad_varc  <LI> (12)  Krypton
       'Kr', &
  !+ad_varc  <LI> (13)  Xenon
       'Xe', &
  !+ad_varc  <LI> (14)  Tungsten</UL>
       'W_'/)

  !+ad_vars  fimpvar /1.0e-3/ : impurity fraction to be used as fimp(impvar)
  !+ad_varc                     (iteration variable 102)
  ! Deprecated
  real(kind(1.0D0)), public :: fimpvar = 1.0D-3

    !  Obtain the root directory

  include "root.dir"

  !+ad_vars  impdir /'/home/PROCESS/[branch]/impuritydata'/ :
  !+ad_varc           Directory containing impurity radiation data files
  character(len=120), public :: impdir = ROOTDIR//'/data/impuritydata/'

  !+ad_vars  impvar : impurity to be iterated (deprecated)
  !+ad_varc                       variable number 102 is turned on
  integer, public :: impvar = 9

  !  Declare impurity data type

  type :: imp_dat

     character(len=2)  :: Label    !  Element name
     integer           :: Z        !  Charge number
     real(kind(1.0D0)) :: amass    !  Atomic mass
     real(kind(1.0D0)) :: frac     !  Number density fraction (relative to ne)
     integer           :: len_tab  !  Length of temperature vs. Lz table
     !  Table of temperature values
     real(kind(1.0D0)), allocatable, dimension(:) :: Temp_keV
     !  Table of corresponding Lz values
     real(kind(1.0D0)), allocatable, dimension(:) :: Lz_Wm3
     !  Table of corresponding average atomic charge values
     real(kind(1.0D0)), allocatable, dimension(:) :: Zav

  end type imp_dat

  type(imp_dat),  dimension(nimp), save, public :: impurity_arr

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine initialise_imprad

    !+ad_name  initialise_imprad
    !+ad_summ  Initialises the impurity radiation data structure
    !+ad_type  Subroutine
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  None
    !+ad_desc  This routine initialises the impurity radiation data.
    !+ad_prob  None
    !+ad_call  init_imp_element
    !+ad_hist  08/05/14 HL  First draft of the routine
    !+ad_hist  13/05/14 PJK First draft within PROCESS
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: tmult, lzmult, frac
    integer :: table_length, errorflag

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Error flag will be set to 1 by routine init_imp_element if any
    !  impurity datafiles are missing; this will also force the code
    !  to use the original impurity radiation model

    errorflag = 0

    table_length = 100  !  Number of temperature and Lz values in data file
    tmult = 1.0D0   !  Conversion from temperatures in data file to keV
    lzmult = 1.0D0  !  Conversion from Lz values in data file to W/m3

    frac = 1.0D0

    !  Hydrogen

    call init_imp_element(no=1, label=imp_label(1), Z=1, amass=1.01D0, &
         frac=frac, len_tab=table_length, TinkeV=tmult, LzinWm3=lzmult, &
         error=errorflag)

    frac = 0.0D0

    !  Helium
    call init_imp_element(2, imp_label(2), 2, 4.003D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Beryllium
    call init_imp_element(3, imp_label(3), 4, 9.01D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Carbon
    call init_imp_element(4, imp_label(4), 6, 12.01D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Nitrogen
    call init_imp_element(5, imp_label(5), 7, 14.01D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Oxygen
    call init_imp_element(6, imp_label(6), 8, 15.999D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Neon
    call init_imp_element(7, imp_label(7), 10, 20.18D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Silicon
    call init_imp_element(8, imp_label(8), 14, 28.09D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Argon
    call init_imp_element(9, imp_label(9), 18, 39.95D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Iron
    call init_imp_element(10, imp_label(10), 26, 55.85D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Nickel
    call init_imp_element(11, imp_label(11), 28, 58.70D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Krypton
    call init_imp_element(12, imp_label(12), 36, 83.80D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Xenon
    call init_imp_element(13, imp_label(13), 54, 131.30D0, &
         frac, table_length, tmult, lzmult, errorflag)

    !  Tungsten
    call init_imp_element(14, imp_label(14), 74, 183.85D0, &
         frac, table_length, tmult, lzmult, errorflag)

  end subroutine initialise_imprad

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init_imp_element(no, label, Z, amass, frac, len_tab, TinkeV, &
       LzinWm3, error)

    !+ad_name  init_imp_element
    !+ad_summ  Initialises the impurity radiation data for a species
    !+ad_type  Subroutine
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  no      : input integer  : position of species in impurity array
    !+ad_args  label   : input string   : species name
    !+ad_args  Z       : input integer  : species charge number
    !+ad_args  amass   : input real     : species atomic mass (amu)
    !+ad_args  frac    : input real     : number density / electron density
    !+ad_args  len_tab : input integer  : length of temperature and Lz tables
    !+ad_args  TinkeV  : input real     : temperature conversion factor from file to keV
    !+ad_args  LzinWm3 : input real     : Lz conversion factor from file to W/m3
    !+ad_args  error   : input/output integer : Error flag; 0 = okay, 1 = missing
    !+ad_argc                             impurity data
    !+ad_desc  This routine initialises the impurity radiation data structure
    !+ad_desc  for a given impurity species.
    !+ad_desc  <P>The Lz versus temperature data are read in from file.
    !+ad_prob  None
    !+ad_call  import_impdata
    !+ad_call  report_error
    !+ad_hist  09/05/14 HL  First draft of the routine
    !+ad_hist  14/05/14 PJK Initial PROCESS version
    !+ad_hist  17/06/14 PJK Added impdir usage
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_hist  26/05/17 JM  Lack of impurity data now exits instead of using old model
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in)           :: no
    character(len=2), intent(in)  :: label
    integer, intent(in)           :: Z
    real(kind(1.0D0)), intent(in) :: amass
    real(kind(1.0D0)), intent(in) :: frac
    integer, intent(in)           :: len_tab
    real(kind(1.0D0)), intent(in) :: TinkeV, LzinWm3

    integer, intent(inout) :: error

    !  Local variables

    integer :: status, i
    character(len=12) :: filename
    character(len=128) :: fullpath
    logical :: iexist

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (error == 1) return

    if (no > size(impurity_arr)) then
       idiags(1) = no ; idiags(2) = size(impurity_arr)
       call report_error(27)
    end if

    impurity_arr(no)%label   = label
    impurity_arr(no)%Z       = Z
    impurity_arr(no)%amass   = amass
    impurity_arr(no)%frac    = frac
    impurity_arr(no)%len_tab = len_tab

    allocate( &
         impurity_arr(no)%Temp_keV(len_tab), &
         impurity_arr(no)%Lz_Wm3(len_tab), &
         impurity_arr(no)%Zav(len_tab), &
         stat=status)
    if (status /= 0) then
       idiags(1) = status ; call report_error(28)
    end if

    !  Read tabulated data in from file, assuming it exists
    !  Add trailing / to impdir if necessary

    filename = label // 'Lzdata.dat'

    if (index(impdir,'/',.true.) == len(trim(impdir))) then
       fullpath = trim(impdir)//trim(filename)
    else
       fullpath = trim(impdir)//'/'//trim(filename)
    end if

    inquire(file=trim(fullpath), exist=iexist)
    if (iexist) then
       call import_impdata(fullpath, len_tab, &
            impurity_arr(no)%Temp_keV, impurity_arr(no)%Lz_Wm3, impurity_arr(no)%Zav)
    else
      !  call report_error(29)
      !  imprad_model = 0
      !  write(*,*) "#####   Imprad_model = 0   #####", label // 'Lzdata.dat', iexist, impdir
      !  error = 1
       write(*,*) "# Warning :  Cannot find impurity data please check path."
       write(*,*) "# Error   :  Current path is: ", impdir
       stop
    end if

    !  Convert tabulated units if necessary

    do i = 1, len_tab
       impurity_arr(no)%Temp_keV(i) = impurity_arr(no)%Temp_keV(i) * TinkeV ! keV
       impurity_arr(no)%Lz_Wm3(i)   = impurity_arr(no)%Lz_Wm3(i)   * LzinWm3 ! W/m3
    end do

  end subroutine init_imp_element

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine import_impdata(filename, nlines, col1, col2, col3, skiprows, fmt)

    !+ad_name  import_impdata
    !+ad_summ  Reads two columns of data from file
    !+ad_type  Subroutine
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  filename : input char(len=128)     : input filename
    !+ad_args  nlines   : input integer           : no. of lines to be read
    !+ad_args  col1(nlines) : output real array   : data in column1
    !+ad_args  col2(nlines) : output real array   : data in column2
    !+ad_args  col3(nlines) : output real array   : data in column3
    !+ad_args  skiprows : optional input integer  : no. of initial rows to skip
    !+ad_args  fmt      : optional input char(len=128) : data format
    !+ad_desc  This routine reads in the data of a two column file and
    !+ad_desc  returns it. The first two rows are skipped by default.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  01/05/14 HL  First draft of routine
    !+ad_hist  07/05/14 HL  Added skiprows
    !+ad_hist  19/05/14 PJK Modified error handling
    !+ad_hist  26/06/14 PJK Added (proper) error handling
    !+ad_hist  03/09/14 HL  Added third column of data
    !+ad_stat  Okay
    !+ad_docs  N/A
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=128), intent(in) :: filename
    integer, intent(in) :: nlines
    real(kind(1.0D0)), dimension(nlines), intent(out) :: col1, col2, col3
    integer, optional, intent(in) :: skiprows
    character(len=128), optional, intent(in) :: fmt

    !  Local variables

    integer :: iostat, i, local_skip
    integer, parameter :: unit = 18
    character(len=25) :: buffer
    real(kind(1.0D0)) :: in1, in2, in3
    character(len=128) :: local_fmt

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Optional variables

    if (present(skiprows)) then
       local_skip = skiprows
    else
       local_skip = 2
    end if

    if (present(fmt)) then
       local_fmt = fmt
    else
       local_fmt = '(3F10.2)'
    end if

    open(unit=unit, file=trim(filename), status='old', action='read', iostat=iostat)

    if (iostat /= 0) then
       idiags(1) = iostat ; call report_error(30)
    end if

    !  Skip first lines (comments)

    do i = 1, local_skip
       read(unit,*,iostat=iostat) buffer
       if (iostat > 0) then
          idiags(1) = iostat ; call report_error(31)
       end if
    end do

    do i = 1, nlines
       read(unit=unit, fmt=local_fmt, iostat=iostat) in1, in2, in3
       if (iostat > 0) then
          idiags(1) = iostat ; idiags(2) = i
          call report_error(32)
       else if (iostat < 0) then
          exit  !  EOF
       else
          col1(i) = in1
          col2(i) = in2
          col3(i) = in3
       end if
    end do

    close(unit=unit)

  end subroutine import_impdata

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function z2index(zimp)

    !+ad_name  z2index
    !+ad_summ  Returns the index of element in the impurity array with charge Z
    !+ad_type  Function returning integer
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  zimp : input integer : impurity atomic number Z
    !+ad_desc  This function returns the index of the element
    !+ad_desc  in the impurity array with the corresponding nuclear charge Z.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  17/12/13 HL  First draft of routine
    !+ad_hist  09/05/14 HL  Using new data structure
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer :: z2index

    !  Arguments

    integer, intent(in) :: zimp

    !  Local variables

    integer :: i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    do i = 1, size(impurity_arr)
       if (zimp == impurity_arr(i)%Z) then
          z2index = i
          return
       end if
    end do

    !  Should only get here if there is a problem

    idiags(1) = zimp ; call report_error(33)

  end function z2index

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function element2index(element_label)

    !+ad_name  element2index
    !+ad_summ  Returns the index of the element in the impurity array with
    !+ad_summ  a given name
    !+ad_type  Function returning integer
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  element_label : input string : impurity name
    !+ad_desc  This function returns the index of the element
    !+ad_desc  in the impurity array with the corresponding name.
    !+ad_prob  None
    !+ad_call  report_error
    !+ad_hist  14/05/14 PJK Initial version
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    integer :: element2index

    !  Arguments

    character(len=*), intent(in) :: element_label

    !  Local variables

    integer :: i

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    do i = 1, size(impurity_arr)
       if (element_label == impurity_arr(i)%label) then
          element2index = i
          return
       end if
    end do

    !  Should only get here if there is a problem

    call report_error(34)

  end function element2index

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine impradprofile(imp_element, ne, te, pimp, pbrem, pline)

    !+ad_name  impradprofile
    !+ad_summ  Implementation of Bremsstrahlung and loss-function curves
    !+ad_type  Subroutine
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  imp_element : input imp_dat : impurity element
    !+ad_args  ne    : input real  : electron density (/m3)
    !+ad_args  te    : input real  : electron temperature (keV)
    !+ad_args  pimp  : output real : total impurity radiation density (W/m3)
    !+ad_args  pbrem : output real : Bremsstrahlung radiation density (W/m3)
    !+ad_args  pline : output real : other radiation density (W/m3)
    !+ad_desc  This routine calculates the impurity radiation losses
    !+ad_desc  for a given temperature and density. Bremsstrahlung equation
    !+ad_desc  from Johner, L(z) data (coronal equilibrium) from Marco
    !+ad_desc  Sertoli, Asdex-U, ref. Kallenbach et al.
    !+ad_prob  None
    !+ad_call  pbremden
    !+ad_call  pimpden
    !+ad_hist  08/10/13 RK  First draft of routine
    !+ad_hist  13/01/14 HL  Implemented fixed lower + variable higher temp limit
    !+ad_hist  09/05/14 HL  Using new data structure
    !+ad_hist  14/05/14 PJK First PROCESS version
    !+ad_stat  Okay
    !+ad_docs  Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !+ad_docs  Sertoli, private communication
    !+ad_docs  Kallenbach et al., Plasma Phys. Control. Fus. 55(2013) 124041
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    type(imp_dat), intent(in) :: imp_element
    real(kind(1.0D0)), intent(in) :: ne, te
    real(kind(1.0D0)), intent(out) :: pimp, pbrem, pline

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Bremsstrahlung

    pbrem = pbremden(imp_element, ne, te)

    !  Total impurity radiation

    pimp = pimpden(imp_element, ne, te)

    if (pimp >= pbrem) then
       pline = pimp - pbrem
    else  !  shouldn't do this... model inconsistency has occurred; okay at high T!
       pline = 0.0D0
       pimp = pbrem
    end if

  end subroutine impradprofile

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function pbremden(imp_element, ne, te)

    !+ad_name  pbremden
    !+ad_summ  Bremsstrahlung radiation density (W/m3)
    !+ad_type  Function returning real
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  imp_element : input imp_dat : impurity element
    !+ad_args  ne : input real : electron density (/m3)
    !+ad_args  te : input real : electron temperature (keV)
    !+ad_desc  This routine calculates the bremsstrahlung radiation losses
    !+ad_desc  for a given temperature and density using the Born approximation
    !+ad_desc  documented in Johner (2011).
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  08/10/13 RK  First draft of routine
    !+ad_hist  13/01/14 HL  Separated bremsstrahlung from impurity radiation
    !+ad_hist  09/05/14 HL  Using new data structure
    !+ad_stat  Okay
    !+ad_docs  Johner, Fusion Science and Technology 59 (2011), pp 308-349
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: pbremden

    !  Arguments

    type(imp_dat),  intent(in) :: imp_element
    real(kind(1.0D0)), intent(in) :: ne, te

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    pbremden = imp_element%frac * ne * ne * Zav_of_te(imp_element,te)**2 &
         * 5.355D-37 * sqrt(te)

  end function pbremden

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function pimpden(imp_element, ne, te)

    !+ad_name  pimpden
    !+ad_summ  Total impurity radiation density (W/m3)
    !+ad_type  Function returning real
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  imp_element : input imp_dat : impurity element
    !+ad_args  ne : input real : electron density (m^-3)
    !+ad_args  te : input real : electron temperature (keV)
    !+ad_desc  This routine calculates the total impurity
    !+ad_desc  radiation losses (line radiation + bremsstrahlung)
    !+ad_desc  for a given temperature and density.
    !+ad_desc  <P>The L(Z) versus temperature data is interpolated from
    !+ad_desc  lookup tables.
    !+ad_prob  If the requested temperature for the calculation is outside
    !+ad_prob  of the tabulated range of the fit, the nearest temperature
    !+ad_prob  point's data is used.
    !+ad_call  report_error
    !+ad_hist  09/05/14 HL  First draft of routine
    !+ad_hist  14/05/14 PJK Initial PROCESS version; added treatment of out-of-range
    !+ad_hisc               temperature values
    !+ad_hist  19/05/14 PJK Added hydrogen isotopes' line radiation contribution
    !+ad_hist  21/05/14 PJK Added warning message if te is below tabulated values
    !+ad_hist  26/06/14 PJK Added error handling
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: pimpden

    !  Arguments

    type(imp_dat),  intent(in) :: imp_element
    real(kind(1.0D0)), intent(in) :: ne, te

    !  Local variables

    integer :: i
    real(kind(1.0D0)) :: xi, yi, c, lz
    logical :: toolow = .false.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Interpolate tabulated data

    !  Temperatures lower than the minimum, or higher than the maximum,
    !  are dealt with by taking the lz value at the nearest tabulated
    !  temperature point

    if (te <= imp_element%Temp_keV(1)) then

       lz = imp_element%Lz_Wm3(1)

       if (.not.toolow) then  !  Only print warning once during a run
          toolow = .true.
          fdiags(1) = te ; call report_error(35)
       end if

    else if (te >= imp_element%Temp_keV(imp_element%len_tab)) then
       !  This is okay because Bremsstrahlung will dominate at higher temp.
       lz = imp_element%Lz_Wm3(imp_element%len_tab)

    else

       do i = 1, imp_element%len_tab-1

          !  Linear interpolation in log-log space

          if ( (te > imp_element%Temp_keV(i)) .and. &
               (te <= imp_element%Temp_keV(i+1)) ) then

             yi = log(imp_element%Lz_Wm3(i))
             xi = log(imp_element%Temp_keV(i))
             c  = (log(imp_element%Lz_Wm3(i+1)) - yi) / &
                  (log(imp_element%Temp_keV(i+1)) - xi)
             lz = exp( yi + c * (log(te) - xi) )
             exit
          end if

       end do

    end if

    pimpden = imp_element%frac * ne * ne * lz

  end function pimpden

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function fradcore(rho,coreradius,coreradiationfraction)

    !+ad_name  fradcore
    !+ad_summ  Function to calculate core radiation fraction
    !+ad_type  Function returning real
    !+ad_auth  R Kemp, CCFE, Culham Science Centre
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  rho        : input real : normalised minor radius
    !+ad_args  coreradius : input real : normalised core radius
    !+ad_args coreradiationfraction : input real : fraction of core radiation
    !+ad_desc  This function calculates the core radiation fraction
    !+ad_desc  at normalised minor radius <CODE>rho</CODE> given a fixed
    !+ad_desc  core radius using only a specified fraction of that radiation.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  08/10/13 RK  Initial draft
    !+ad_hist  16/12/13 HL  Added coreradius as optional input
    !+ad_hist  19/05/14 PJK First PROCESS implementation
    !+ad_hist 29/03/16 HL Added coreradiationfraction
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    real(kind(1.0D0)) :: fradcore

    !  Arguments

    real(kind(1.0D0)), intent(in) :: rho
    real(kind(1.0D0)), intent(in) :: coreradius
    real(kind(1.0D0)), intent(in) :: coreradiationfraction

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (rho < coreradius) then
       fradcore = coreradiationfraction
    else
       fradcore = 0.0D0
    end if

  end function fradcore


  function Zav_of_te(imp_element,te)

    !+ad_name  Zav_of_te
    !+ad_summ  Electron temperature dependent average atomic number
    !+ad_type  Function returning real
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  imp_element : input imp_dat : impurity element
    !+ad_args  te : input real : electron temperature (keV)
    !+ad_desc  This routine returns the interpolated average atomic
    !+ad_desc  charge for a given electron temperature.
    !+ad_desc  <P>The Zav versus temperature data is interpolated from
    !+ad_desc  lookup tables from the ADAS data base provided by Martin O'Mullane.
    !+ad_prob  If the requested temperature for the calculation is outside
    !+ad_prob  of the tabulated range of the fit, the nearest temperature
    !+ad_prob  point's data is used.
    !+ad_call  report_error
    !+ad_hist  22/02/16 HL  First draft of routine
    !+ad_stat  Okay
    !+ad_docs  None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       implicit none

    real(kind(1.0D0)) :: Zav_of_te

    !  Arguments

    type(imp_dat),  intent(in) :: imp_element
    real(kind(1.0D0)), intent(in) :: te

    !  Local variables

    integer :: i
    real(kind(1.0D0)) :: xi, yi, c

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Interpolate tabulated data

    !  Temperatures lower than the minimum, or higher than the maximum,
    !  are dealt with by taking the Zav value at the nearest tabulated
    !  temperature point

    if (te <= imp_element%Temp_keV(1)) then
       ! This should not be too unreasonable.
       Zav_of_te = imp_element%Zav(1)

    else if (te >= imp_element%Temp_keV(imp_element%len_tab)) then
       !  This should be okay, as most elements are fully ionised by now.
       Zav_of_te = imp_element%Zav(imp_element%len_tab)

    else

       do i = 1, imp_element%len_tab-1

          !  Linear interpolation in log-lin space

          if ( (te > imp_element%Temp_keV(i)) .and. &
               (te <= imp_element%Temp_keV(i+1)) ) then

             yi = imp_element%Zav(i)
             xi = log(imp_element%Temp_keV(i))
             c  = (imp_element%Zav(i+1) - yi) / &
                  (log(imp_element%Temp_keV(i+1)) - xi)
             Zav_of_te = yi + c * (log(te) - xi)
             exit
          end if

       end do

    end if

  end function Zav_of_te


end module impurity_radiation_module
