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
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  bremsstrahlung and line radiation of impurities
  !+ad_desc  including H  and He, assuming a coronal equilibrium.
  !+ad_desc  <P>The model is only valid for T &gt; 30 eV. For some impurity
  !+ad_desc  species there is also an upper temperature limit of T &lt; 40 keV.
  !+ad_prob  None
  !+ad_call  constants
  !+ad_call  profiles_module
  !+ad_hist  13/12/13 HL Initial version of module
  !+ad_hist  13/05/14 PJK Initial PROCESS implementation
  !+ad_stat  Okay
  !+ad_docs  Johner, Fusion Science and Technology 59 (2011), pp 308-349
  !+ad_docs  Sertoli, private communication
  !+ad_docs  Kallenbach et al., Plasma Phys. Control. Fus. 55(2013) 124041
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constants
  use profiles_module

  implicit none

  private
  public :: initialise_imprad, impradprofile, z2index, element2index
  public :: imp_dat

  !+ad_vars  nimp /14/ FIX : number of ion species in impurity radiation model
  integer, public, parameter :: nimp = 14

  !+ad_vars  coreradius /0.9/ : normalised radius defining the 'core' region
  real(kind(1.0D0)), public :: coreradius = 0.9D0

  !+ad_vars  fimp(nimp) /1.0,0.0,.../ : impurity number density fractions relative
  !+ad_varc                             to electron density
  real(kind(1.0D0)), public, dimension(nimp) :: fimp = &
       (/ 1.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, &
       0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0 /)
  !+ad_vars  imp_label(nimp) /.../ FIX : impurity ion species:<UL>
  character(len=2), dimension(nimp) :: imp_label = (/ &
  !+ad_varc  <LI> ( 1)  Hydrogen  (fraction calculated by code)
       'H_', &
  !+ad_varc  <LI> ( 2)  Helium    (fraction calculated by code)
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

  !+ad_vars  imprad_model /0/ : switch for impurity radiation model:<UL>
  !+ad_varc               <LI>  = 0 original ITER 1989 model
  !+ad_varc               <LI>  = 1 new model</UL>
  integer, public :: imprad_model = 0

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

    table_length = 200  !  Number of temperature and Lz values in data file
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
    !+ad_hist  09/05/14 HL  First draft of the routine
    !+ad_hist  14/05/14 PJK Initial PROCESS version
    !+ad_hist  Okay  
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
    character(len=128) :: filename
    logical :: iexist

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    if (error == 1) return

    if (no > size(impurity_arr)) then
       write(*,*) 'Error in routine INIT_IMP_ELEMENT:'
       write(*,*) 'You are trying to access an out of range array element'
       write(*,*) 'PROCESS stopping.'
       stop
    end if
    
    impurity_arr(no)%label   = label
    impurity_arr(no)%Z       = Z
    impurity_arr(no)%amass   = amass
    impurity_arr(no)%frac    = frac
    impurity_arr(no)%len_tab = len_tab

    allocate( &
         impurity_arr(no)%Temp_keV(len_tab), &
         impurity_arr(no)%Lz_Wm3(len_tab), &
         stat=status)
    if (status /= 0) then
       write(*,*) 'Error in routine INIT_IMP_ELEMENT:'
       write(*,*) 'Allocation problem...'
       write(*,*) 'PROCESS stopping.'
       stop
    end if

    !  Read tabulated data in from file, assuming it exists

    filename = 'impuritydata/' // Label // 'Lzdata.dat'

    inquire(file=trim(filename),exist=iexist)
    if (iexist) then
       call import_impdata(filename, len_tab, &
            impurity_arr(no)%Temp_keV, impurity_arr(no)%Lz_Wm3)
    else
       write(*,*) 'Warning in routine INIT_IMP_ELEMENT:'
       write(*,*) 'Impurity datafiles are missing...'
       write(*,*) 'Switching to original impurity model.'
       imprad_model = 0
       error = 1
       return
    end if

    !  Convert tabulated units if necessary

    do i = 1, len_tab
       impurity_arr(no)%Temp_keV(i) = impurity_arr(no)%Temp_keV(i) * TinkeV ! keV
       impurity_arr(no)%Lz_Wm3(i)   = impurity_arr(no)%Lz_Wm3(i)   * LzinWm3 ! W/m3
    end do

  end subroutine init_imp_element

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine import_impdata(filename, nlines, col1, col2, skiprows, fmt)
    
    !+ad_name  import_impdata
    !+ad_summ  Reads two columns of data from file
    !+ad_type  Subroutine
    !+ad_auth  H Lux, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  filename : input char(len=128)     : input filename
    !+ad_args  nlines   : input integer           : no. of lines to be read
    !+ad_args  col1(nlines) : output real array   : data in column1 
    !+ad_args  col2(nlines) : output real array   : data in column2
    !+ad_args  skiprows : optional input integer  : no. of initial rows to skip
    !+ad_args  fmt      : optional input char(len=128) : data format
    !+ad_desc  This routine reads in the data of a two column file and
    !+ad_desc  returns it. The first two rows are skipped by default.
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  01/05/14 HL  First draft of routine
    !+ad_hist  07/05/14 HL  Added skiprows
    !+ad_stat  Okay
    !+ad_docs  N/A
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    character(len=128), intent(in) :: filename
    integer, intent(in) :: nlines
    real(kind(1.0D0)), dimension(nlines), intent(out) :: col1, col2
    integer, optional, intent(in) :: skiprows
    character(len=128), optional, intent(in) :: fmt

    !  Local variables

    integer :: iostat, i, local_skip
    integer, parameter :: unit = 18
    character(len=25) :: buffer
    real(kind(1.0D0)) :: in1, in2
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
       local_fmt = '(2F10.2)'
    end if

    open(unit=unit, file=filename, status='old', action='read', iostat=iostat)

    if (iostat /= 0 ) then
       write(*,*) 'Error: There was a problem opening the file ', filename
       call exit()
    end if

    !  Skip first lines (comments)

    do i = 1, local_skip
       read(unit,*,iostat=iostat) buffer
       if (iostat > 0) then 
          write(*,*) 'There is a problem with reading the file', filename
          call exit()
       end if
    end do

    do i = 1, nlines
       read(unit=unit, fmt=local_fmt, iostat=iostat) in1, in2
       if (iostat > 0) then 
          write(*,*) 'There is a problem with reading the file', filename
          call exit()
       else if (iostat < 0) then
          exit ! EOF
       else
          col1(i) = in1 
          col2(i) = in2 
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
    !+ad_call  None
    !+ad_hist  17/12/13 HL  First draft of routine
    !+ad_hist  09/05/14 HL  Using new data structure
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

    write(*,*) 'Error in routine Z2INDEX:'
    write(*,*) 'Element with charge ', zimp , 'is not in impurity_arr!'
    write(*,*) 'PROCESS stopping.'
    stop

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
    !+ad_call  None
    !+ad_hist  14/05/14 PJK Initial version
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

    write(*,*) 'Error in routine ELEMENT2INDEX:'
    write(*,*) 'Element with label '//element_label//' is not in impurity_arr!'
    write(*,*) 'PROCESS stopping.'
    stop

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
    else  !  shouldn't do this... model inconsistency has occurred
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

    pbremden = imp_element%frac * ne * ne * imp_element%Z * imp_element%Z &
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
    !+ad_call  None
    !+ad_hist  09/05/14 HL  First draft of routine
    !+ad_hist  14/05/14 PJK Initial PROCESS version; added treatment of out-of-range
    !+ad_hisc               temperature values
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

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Line radiation for hydrogen isotopes is excluded

    if (imp_element%Z == 1) then
       pimpden = 0.0D0
       return
    end if

    !  Interpolate tabulated data

    !  Temperatures lower than the minimum, or higher than the maximum,
    !  are dealt with by taking the lz value at the nearest tabulated
    !  temperature point

    if (te <= imp_element%Temp_keV(1)) then

       lz = imp_element%Lz_Wm3(1)

    else if (te >= imp_element%Temp_keV(imp_element%len_tab)) then

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

end module impurity_radiation_module
