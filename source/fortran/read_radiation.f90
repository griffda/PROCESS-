module read_radiation
  !+ad_name  read_radiation
  !+ad_summ  Module for reading radiation data
  !+ad_type  Module
  !+ad_auth  M Kovari, CCFE, Culham Science Centre
  !+ad_args  N/A
  !+ad_desc  This module contains the PROCESS Kallenbach divertor model
  !+ad_prob  None
  !+ad_hist  25/01/17 MDK Initial version of module
  !+ad_hist  08/02/17 JM  Tidy up
  !+ad_stat  Okay
  !+ad_docs
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Modules to import !
  !!!!!!!!!!!!!!!!!!!!!

  use maths_library
  use impurity_radiation_module, only: nimp, fimp, imp_label

  implicit none

  ! List of impurities in the SOL/divertor model IS now same as the main plasma impurities

contains

  FUNCTION read_lz(element, te, netau, mean_z, mean_qz, verbose)
    !+ad_name  read_lz
    !+ad_summ  Read Lz, mean_z or mean_qz data from database
    !+ad_type  Function returning real
    !+ad_auth  M Koviar, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  element : input char : element name
    !+ad_args  te : input real : electron temperature [eV]
    !+ad_args  netau : input real : "non-coronal parameter" for radiative loss function [ms.1e20/m3]
    !+ad_args  mean_z : input logical : get mean charge instead of Lz
    !+ad_args  mean_qz : input logical : get mean quadratic charge instead of Lz
    !+ad_args  verbose : input logical : verbose boolean
    !+ad_desc  read radiative loss data Lz calculated by M. O'Mullane from ADAS in 2016,
    !+ad_desc  OR mean charge
    !+ad_desc  A. Kallenbach, 11.1.2013, R. Dux (add readout of Z, Z^2, add a few elements)
    !+ad_desc  Fortran: Michael Kovari 2/6/16
    !+ad_prob  None
    !+ad_call  None
    !+ad_hist  01/02/17 MDK Initial version
    !+ad_stat  Okay
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Modules to import !
    !!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Variable declarations !
    !!!!!!!!!!!!!!!!!!!!!!!!!

    integer :: i, j
    integer :: location=0

    ! "non-coronal parameter" for radiative loss function [ms.1e20/m3]
    real(kind(1.0D0)), intent(in) :: netau

    ! Electron temperature [eV]
    real(kind(1.0D0)), intent(in) :: te

    ! Verbose switch
    logical, intent(in) :: verbose

    ! switches for geting mean charge or mean quadratic charge instead of Lz
    logical, intent(in) :: mean_z, mean_qz

    ! Element name
    character(len=*), intent(in) :: element

    ! Function output
    real(kind(1.0D0)) :: read_lz

    ! Natural logs of netau and te
    real(kind(1.0D0)) :: lnetau, lte

    ! Length of temperature and netau data
    integer,save :: nt, nnetau

    ! Impurity data array
    real(kind(1.0D0)), save, dimension(200,5) :: impurity_data

    ! The values of the Lz, mean Z, and mean Z^2 stored in the data files
    real(kind(1.0D0)), save, dimension(nimp,200,5) :: data_lz, data_z, data_qz

    ! The values of ln(n.tau) at which the data is stored.
    real(kind(1.0D0)), save, dimension(nimp,5) :: lnetau_lz, lnetau_z, lnetau_qz

    ! The values of ln(Te) at which the data is stored.
    real(kind(1.0D0)), save, dimension(nimp, 200) :: logT_lz, logT_z, logT_qz

    ! Lz data filename
    character(len=100) :: filename

    ! First call boolean switch
    logical, save :: FirstCall(nimp) = .true.

    ! Obtain the root directory from the file 'root.dir'
    ! The # character must be at the start of the line.
#include "root.dir"


 !   character(len=120), save :: lzdir = trim(ROOTDIR//'/data/lz_non_corona_14_elements/')
    character(len=180), save :: lzdir = trim(INSTALLDIR//'/data/lz_non_corona_14_elements/')
    ! Find the index of the element.  Exclude hydrogen by starting at 2 (Helium)
    do i = 2, nimp
        if (imp_label(i) .eq. element) then
            location = i
            exit
        endif
    enddo

    ! Check element was found
    if (location.eq.0) then
      write(*,*)'element '//element//' not supported'
    end if

    ! Natural log netau and electron temperature
    lnetau = log(netau)
    lte = log(te)

    ! Read the data for that element

    ! FirstCall
    if(FirstCall(location)) then

        FirstCall(location)=.false.

        ! Each data file may have its own values of T and n, stored in arrays logT_lz, lnetau_lz etc
        ! Store data in logarithm form for interpolation

        ! Assign loss data filename
        filename = trim(lzdir)//trim(element)//'_lz_tau.dat'

        ! Read the impuriy data
        call read_impurity_data(filename, nt, nnetau, impurity_data, logT_lz(location,:), lnetau_lz(location,:))

        ! Store log impurity data
        data_lz(location,:,:) = log(impurity_data)

        if (verbose) then
            write(*,*)'Lz'
            write(*,*)'log(T) values'
            write(*,'(8(f10.3))')(logT_lz(location,i), i= 1, nt)
            do j=1,nnetau
                write(*,*)'log(ne.tau)= ',lnetau_lz(location,j)
                write(*,'(8(e10.3))')(impurity_data(i,j), i= 1, nt)
            enddo
        endif

        ! Assign z data filename
        filename = trim(lzdir)//trim(element)//'_z_tau.dat'

        ! Read z data
        call read_impurity_data(filename, nt, nnetau, impurity_data, logT_z(location,:), lnetau_z(location,:))

        if (verbose) then
            write(*,*)'Element = '// element//'./data/LZ_NON_CORONA/'//element//'_z_tau.dat'
            write(*,*)'Mean Z'
            write(*,*)'log(T) values'
            write(*,'(8(f10.3))')(logT_z(location,i), i= 1, nt)
            do j=1,nnetau
                write(*,*)'log(ne.tau)= ',lnetau_z(location,j)
                write(*,'(8(e10.3))')(impurity_data(i,j), i= 1, nt)
            enddo
        endif

        ! Store log z data
        data_z(location,:,:) = log(impurity_data)

        ! Assign z^2 data filename
        filename = trim(lzdir)//trim(element)//'_z2_tau.dat'

        ! Read root-mean square z data
        call read_impurity_data(filename, nt, nnetau, impurity_data, logT_qz(location,:), lnetau_qz(location,:))

        ! Square RMS z data
        impurity_data = impurity_data**2

        ! Store z^2 data
        data_qz(location,:,:) = log(impurity_data)

        if (verbose) then
            write(*,*)'Mean Z^2'
            write(*,*)'log(T) values'
            write(*,'(8(f10.3))')(logT_qz(location,i), i= 1, nt)
            do j=1,nnetau
                write(*,*)'log(ne.tau)= ',lnetau_qz(location,j)
                write(*,'(8(e10.3))')(impurity_data(i,j), i= 1, nt)
            enddo
        endif

     endif

    ! Interpolate for the specified values of log(temperature) and log(n.tau)
    ! Using function interpolate(x_len, x_array, y_len, y_array, f, x, y, delta)
    if(mean_z) then

        read_lz = interpolate(nt, logT_z(location,:), nnetau, lnetau_z(location,:), data_z(location,:,:), lte, lnetau)

    elseif(mean_qz) then

        read_lz = interpolate(nt, logT_qz(location,:), nnetau, lnetau_qz(location,:), data_qz(location,:,:), lte, lnetau)

    else

        if(  (minval(lnetau_lz(location,:)).gt.lnetau) .or.  (maxval(lnetau_lz(location,:)).lt.lnetau)  )  then
            write(*,*)'lnetau is out of range, =', lnetau
        endif

        read_lz = interpolate(nt, logT_lz(location,:), nnetau, lnetau_lz(location,:), data_lz(location,:,:), lte, lnetau)

    endif

    read_lz = exp(read_lz)

  end function read_lz

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine read_impurity_data(filename,nt,nnetau,impurity_data,logT_array,lnetau_array)
    !+ad_name  read_impurity_data
    !+ad_summ  Read impurity data from ADAS database
    !+ad_type  subroutine
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_args  filename      : input character   : filename of data
    !+ad_args  nt            : output integer    : length of temperature data
    !+ad_args  nnetau        : output integer    : length of netau data
    !+ad_args  impurity_data : output real array : impurity data array
    !+ad_args  logT_array    : output real array : log temperature array
    !+ad_args  lnetau_array  : output real array : log netau array
    !+ad_desc  Read the impurity data from database
    !+ad_prob  None
    !+ad_hist  08/02/17 MDK  Initial version
    !+ad_hist  08/02/17 JM  Tidy
    !+ad_stat  Okay
    !+ad_docs
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Subroutine declarations !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Filename for data
    character(len=*), intent(in) :: filename

    ! Length of temperature and netau arrays
    integer, intent(out) :: nt, nnetau

    ! Impurity data array
    real(kind(1.0D0)), intent(out), dimension(200,5) :: impurity_data

    ! Log netau and temperature arrays
    real(kind(1.0D0)), intent(out), dimension(5) :: lnetau_array
    real(kind(1.0D0)), intent(out), dimension(200) :: logT_array

    ! netau data
    real(kind(1.0D0)) :: data_netau

    !
    integer :: pos, i, j, iostatus

    ! Electron temperature array [eV]
    real(kind(1.0D0)), dimension(200) :: T_array

    character(len=100) :: string, substring

    ! Open data file
    open(unit=8,file=filename,status='old')
    read(8,*)
    read(8,*)
    read(8,*)
    read(8,*)nt, nnetau

    if ((nt.ne.200).or.(nnetau.ne.5))then
      write(*,*)'read_impurity_data: There must be exactly 200 temperature values and 5 values of n.tau'
      write(*,*)'nt = ',nt,'  nnetau = ',nnetau
    endif

    do
      read(8,*,IOSTAT=iostatus)string

      ! The string 'Te[eV]' is present
      if(index(string,'Te[eV]').ne.0) exit

      if(iostatus.ne.0)then
          write(*,*)'Problem in reading impurity data from file ',filename
          stop
      endif

    enddo

    read(8,'(8(f10.3))')(T_array(i), i= 1, nt)

    ! Create log temperature array [eV]
    logT_array = log(T_array)

    ! Loop over netau
    do j = 1, nnetau

      ! Read the non-coronal parameter ne.tau
      read(8,'(a100)')string

      pos = index(string,'=')

      substring = string(pos+1:)

      read(substring,*)data_netau

      lnetau_array(j) = log(data_netau)

      ! Read the data
      ! A fixed format read doesn't work for some reason
      read(8,*)(impurity_data(i,j), i= 1, nt)

    enddo

    close(unit=8)

  end subroutine read_impurity_data

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine plot_Lz()
    !+ad_name  plot_lz
    !+ad_summ  Write loss data to file for plotting
    !+ad_type  subroutine
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Write loss data to file for plotting
    !+ad_desc  Compare to Figure 3 in Kallenbach 2016.
    !+ad_prob  None
    !+ad_hist  08/02/17 MDK Initial version
    !+ad_hist  13/02/17 JM  Tidy
    !+ad_stat  Okay
    !+ad_docs
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Subroutine declarations !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!

    character(len=2) :: element

    real(kind(1.0D0)) :: dummy

    integer :: i, j

    integer, parameter :: points = 27

    ! Temperature plot points
    real(kind(1.0D0)) :: te(points) = (/1., 1.2, 1.5, 2., 2.5, 3., 4., 5., 6., 7., 8., 9., &
        10., 12., 14., 16., 20., 30., 40., 50., 60., 70., 80., 90., 100., 150., 200./)

    real(kind(1.0D0)) :: Lz_plot(nimp)

    real(kind(1.0D0)) :: netau = 0.5

    open(unit=12,file='radiative_loss_functions.txt',status='replace')
    write(12,'(30a11)')'Te (eV)', (imp_label(i), i=2,nimp)

    ! Just read data.  Exclude hydrogen by starting at 2
    do i=2,nimp
        element=imp_label(i)
        dummy=read_lz(element,30.0d0,netau, mean_z=.false., mean_qz=.false., verbose=.false.)
    enddo

    do i=1,points
        do j=2,nimp
            Lz_plot(j)=read_lz(imp_label(j),te(i),netau, mean_z=.false., mean_qz=.false., verbose=.false.)
        enddo
        write(12,'(30es11.3)')te(i), (Lz_plot(j), j=2,nimp)
    enddo
    close(unit=12)

  end subroutine plot_Lz

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine plot_z()
    !+ad_name  plot_z
    !+ad_summ  Write z and z^2 data to file for plotting
    !+ad_type  subroutine
    !+ad_auth  M Kovari, CCFE, Culham Science Centre
    !+ad_cont  N/A
    !+ad_desc  Write z and z^2 data to file for plotting
    !+ad_prob  None
    !+ad_hist  08/02/17 MDK Initial version
    !+ad_hist  13/02/17 JM  Tidy
    !+ad_stat  Okay
    !+ad_docs
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    ! Subroutine declarations !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!

    character(len=2) :: element

    real(kind(1.0D0)) :: dummy

    integer :: i, j, k

    integer, parameter :: points = 27

    real(kind(1.0D0)) :: te(points) = (/1., 1.2, 1.5, 2., 2.5, 3.,4., 5., 6., 7., &
        8., 9.,10.,12., 14., 16., 20., 30., 40., 50., 60., 70., 80., 90., 100., 150., 200./)

    real(kind(1.0D0)) :: Z_plot(3,nimp)

    real(kind(1.0D0)) :: netau(3) = (/0.1, 1.0, 10.0/)

    open(unit=12,file='mean_Z.txt',status='replace')

    write(12,*)'Mean Z'

    write(12,'(a11,  42(3(a4, es7.1)))')'Te (ev)',((imp_label(i),netau(j), j=1,3),i=2,nimp)

    ! Just read data.  Exclude hydrogen by starting at 2
    do i = 2, nimp
        element = imp_label(i)
        dummy = read_lz(element,30.0d0,1.0d0, mean_z=.true., mean_qz=.false., verbose=.true.)
        dummy = read_lz(element,30.0d0,1.0d0, mean_z=.false., mean_qz=.true., verbose=.true.)
    enddo

    do i = 1, points
        do j = 1, 3
            do k = 2, nimp
                element = imp_label(k)
                Z_plot(j,k) = read_lz(element,te(i),netau(j), mean_z=.true., mean_qz=.false., verbose=.false.)
            enddo
        enddo
        write(12,'(42es11.3)')te(i), ((Z_plot(j,k), j=1,3), k=2,nimp)
    enddo

    write(12,*)
    write(12,*)'Mean Z^2'
    write(12,'(a11,  42(3(a4, es7.1)))')'Te (ev)', ((imp_label(i),netau(j), j=1,3),i=2,nimp)
    do i = 1, points
        do j = 1, 3
            do k = 2, nimp
                element = imp_label(k)
                Z_plot(j,k) = read_lz(element,te(i),netau(j), mean_z=.false., mean_qz=.true., verbose=.false.)
            enddo
        enddo
        write(12,'(42es11.3)')te(i), ((Z_plot(j,k), j=1,3), k=2,nimp)
    enddo

    close(unit=12)

  end subroutine plot_z

end module read_radiation
