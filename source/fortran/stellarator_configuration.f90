module stellarator_configuration
    !! author: J Lion, IPP Greifswald
    !! Module containing defining parameters for a stellarator
    !!
    !! This module contains a set of constants that defines a
    !! stellarator configuration. These parameters are based on external
    !! calculations and are hardcoded right now into this module. There will be
    !! the possibiltiy to set them via an input file in the future.
    !! The list below will be modified in further commits.
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use, intrinsic :: iso_fortran_env, only: dp=>real64    
    implicit none



    type :: stella_config
       character (len = 20) :: name
       ! Name of the configuration
 
       integer symmetry
       !  Number of coils
 
       integer coilspermodule
       !  Coils per module
 
       real(dp)  rmajor_ref
       !  Reference Point for major radius where all the other variables are determined
 
       real(dp)  rminor_ref
       !  Reference Point for minor radius where all the other variables are determined
 
       real(dp)  coil_rmajor
       !  Reference Point for coil major radius
 
       real(dp)  coil_rminor
       !  Reference Point for coil minor radius
 
       real(dp)  aspect_ref
       !  Reference Point for aspect ratio where all the other variables are determined
 
       real(dp)  bt_ref
       !  Reference Point for toroidal b where all the other variables are determined
       
       real(dp) WP_area
       !  Winding pack area at the reference point [m^2]
 
       real(dp)  WP_bmax
       !  The maximal magnetic field in the winding pack at the reference size of the winding pack [T]
 
       real(dp) i0
       !  Coil current needed for b0 at the reference point
       
       real(dp) a1
       !  Magnetic field fit parameter a1 (for the maximal field on the coils)
       
       real(dp) a2
       !  Magnetic field fit parameter a2
 
       real(dp) dmin
       !  Minimal intercoil distance at the reference point
       
       real(dp) inductance
       !  inductance at the reference point
 
       real(dp) coilsurface
       !  Coil surface at the reference point
 
       real(dp) coillength
       !  Total coil length at the reference point
       
       real(dp) max_portsize_width
       !  Port size in toroidal direction at the reference point
 
       real(dp) maximal_coil_height
       !  The maximal coil height at reference point.
 
       real(dp) min_plasma_coil_distance
       !  The minimal distance between coil and plasma at the reference point
 
       real(dp) plasma_volume
       !  The plasma volume at the reference point. Scales as a*R^2. [m^3]
 
       real(dp) plasma_surface
       !  The plasma surface a the reference point. [m^2]
 
       real(dp) WP_ratio
       !  Ratio radial to toroidal length of the winding pack. (a1 and a2 should be calculated using this value) [1]
      
       real(dp) max_force_density
       !  Maximal toroidal and radially averaged force density at reference point in a WP cross section [MN/m^3]
       
       real(dp) max_force_density_MNm
       !  Maximal integrated force density at reference point in a WP cross section [MN/m]

       real(dp) min_bend_radius
       !  Minimal bending radius at reference point [m]
 
       real(dp) epseff
       !  Maximal epsilon effective in the core region [1]

       real(dp) max_lateral_force_density
       !  Maximal lateral force density of the coil set [MN/m]

       real(dp) max_radial_force_density
       !  Maximal radial force density of the coil set [MN/m]

       real(dp) centering_force_max_MN
       !  Maximal centering force of a coil in the coil set [MN]

       real(dp) centering_force_min_MN
       !  Minimal centering force of a coil in the coil set (negative means pointing outwards) [MN]

       real(dp) centering_force_avg_MN
       !  Average centering force the coils in the coil set [MN/coil]

       real(dp), dimension(:), allocatable :: D11_star_mono_input
       !  The monoenergetic radial transport coefficients normalized by the plateau value.

       real(dp), dimension(:), allocatable :: nu_star_mono_input
       !  The monoenergetic radial transport coefficients normalized by the plateau value.
 
       real(dp) :: neutron_peakfactor
       !  The neutron peaking factor determined through inhomogeneities on the stellarator wall (qmax/qavg)




    end type stella_config
 
    ! Overload fortran constructor
    interface stella_config
       procedure :: new_stella_config
    end interface stella_config
 
 contains
 
    function new_stella_config(index) result(output_config)
       integer, intent(in) :: index
       type(stella_config) :: output_config
 
       select case (index)
          
          ! This is the istell case switch:
          ! istell = 1: Helias5 machine
          ! istell = 2: Helias4 machine
          ! istell = 3: Helias3 machine
          ! istell = 4: w7x30 machine
          ! istell = 5: w7x50 machine
          ! istell = 6: Init from json
 
          ! All parameters set here are prelimnirary versions and might be changed in further commits
 
          case(1)
             ! Helias5 Machine
             ! The values are given at the reference point
 
             output_config%name = "Helias 5b"
 
             output_config%rmajor_ref = 22.2D0
             output_config%rminor_ref = 1.80D0
             output_config%aspect_ref = 12.33D0
 
             ! Coil radii
             output_config%coil_rmajor = 22.44D0
             output_config%coil_rminor = 4.76D0
 
             output_config%bt_ref = 5.6D0
             output_config%WP_area = 0.8d0*0.6d0
             output_config%WP_bmax = 11.44d0
 
             output_config%symmetry = 5
             output_config%coilspermodule = 10
 
             output_config%a1 = 0.688D0
             output_config%a2 = 0.025D0
 
             output_config%plasma_volume = 1422.63D0  ! This value is for Helias 5
             output_config%dmin = 0.84D0
             output_config%max_portsize_width = 2.12D0 
 
             output_config%plasma_surface = 1960.0D0 ! Plasma Surface
 
             output_config%maximal_coil_height = 12.7 ! [m] Full height max point to min point
 
             output_config%coilsurface = 4817.7D0 ! Coil surface, dimensionfull. At reference point
 
             output_config%coillength = 1680.0D0 ! Central filament length of machine with outer radius 1m.
       
             output_config%I0 = 13.06D0 ! Coil Current needed to produce 1T on axis in [MA] at outer radius 1m
             output_config%inductance = 1655.76D-6 ! inductance in muH
 
             output_config%WP_ratio = 1.2D0 ! The fit values in stellarator config class should be calculated using this value.
 
             output_config%max_force_density = 120.0d0 ! [MN/m^3]
             output_config%max_force_density_MNm = 98.0d0 ! [MN/m]

             output_config%max_lateral_force_density = 92.4d0 ! [MN/m^3]
             output_config%max_radial_force_density = 113.5d0   ! [MN/m^3]

             output_config%centering_force_max_MN = 189.5d0
             output_config%centering_force_min_MN = -55.7d0
             output_config%centering_force_avg_MN = 93.0d0
 
             output_config%min_plasma_coil_distance = 1.9d0
 
             output_config%min_bend_radius = 1.0 ! [m]

             output_config%neutron_peakfactor = 1.6

             output_config%epseff = 0.015
 
             allocate(output_config%D11_star_mono_input(10))
             allocate(output_config%nu_star_mono_input(10))

             output_config%D11_star_mono_input = (/1,1,1,1,1,1,1,1,1,1/)
             output_config%nu_star_mono_input = (/1d-8,1d-7,1d-6,1d-5,1d-4,1d-3,1d-2,1d-1,1d0,1d1/)

   


          case(2)
             ! Helias4 Machine
             output_config%name = "Helias 4"
             ! Reference point where all the other variables are determined from
             ! Plasma outer radius
             output_config%rmajor_ref = 17.6D0
             output_config%rminor_ref = 2.0D0
             output_config%aspect_ref =  8.8D0
 
             ! Coil radii
             output_config%coil_rmajor = 18.39D0
             output_config%coil_rminor = 4.94D0
 
             output_config%bt_ref = 5.6D0
             output_config%WP_area = 0.8d0*0.6d0
             output_config%WP_bmax = 11.51d0
 
             output_config%symmetry = 4
             output_config%coilspermodule = 10
             output_config%a1 = 0.676D0
             output_config%a2 = 0.029D0
             output_config%plasma_volume =   1380.0D0
             output_config%dmin = 1.08D0
             output_config%max_portsize_width = 3.24D0
 
             output_config%plasma_surface = 1900.0D0
             output_config%maximal_coil_height = 13.34D0  ! [m] Full height max point to min point
 
             output_config%coilsurface =  4100.0D0! Coil surface, dimensionfull. At reference point
 
             output_config%coillength = 1435.07D0 ! Central filament length of machine with outer radius 1m.
       
             output_config%I0 = 13.146D0 ! Coil Current needed to produce b0 on axis in [MA] at reference point
             output_config%inductance = 1290.4D-6 ! inductance/R*A^2 in muH
 
             output_config%WP_ratio = 1.3D0
 
             output_config%max_force_density = 120.0d0 ! [MN/m^3]
             output_config%max_force_density_MNm = 98.0d0 ! [MN/m]


             output_config%max_lateral_force_density = 87.9d0 ! [MN/m^3]
             output_config%max_radial_force_density = 109.9d0   ! [MN/m^3]

             output_config%centering_force_max_MN = 226.0d0
             output_config%centering_force_min_MN = -35.3d0
             output_config%centering_force_avg_MN = 125.8d0
 
             output_config%min_plasma_coil_distance = 1.7d0
 
             output_config%min_bend_radius = 0.86 ! [m]
 
             output_config%neutron_peakfactor = 1.6

             output_config%epseff = 0.015

 
          case(3)
             ! Helias 3 Machine
             output_config%name = "Helias 3"
             ! Reference point where all the other variables are determined from
             ! Plasma outer radius
             output_config%rmajor_ref = 13.86d0
             output_config%rminor_ref = 2.18d0
             output_config%aspect_ref =  6.36d0
 
             ! Coil radii
             output_config%coil_rmajor = 14.53D0
             output_config%coil_rminor = 6.12D0
             
             output_config%bt_ref = 5.6D0
             output_config%WP_bmax = 12.346d0
             output_config%WP_area = 0.8d0*0.6d0
 
             output_config%symmetry = 3
             output_config%coilspermodule = 10
 
             ! Bmax fit parameters
             output_config%a1 = 0.56D0
             output_config%a2 = 0.030D0
 
             output_config%plasma_volume =   1300.8D0
             output_config%dmin = 1.145D0
             output_config%max_portsize_width = 3.24D0 !??? guess. not ready yet
 
             output_config%plasma_surface = 1600.00D0
 
             output_config%maximal_coil_height = 17.74D0! [m] Full height max point to min point
 
             output_config%coilsurface = 4240.0D0 ! Coil surface, dimensionfull. At reference point
 
             output_config%coillength = 1287.3D0 ! Central filament length of machine with outer radius 1m.
       
             output_config%I0 = 14.23D0 ! Coil Current needed to produce 1T on axis in [MA] at outer radius 1m
             output_config%inductance = 1250.7D-6 ! inductance in muH
 
             output_config%WP_ratio = 1.3D0
 
             output_config%max_force_density = 120.0d0
             output_config%max_force_density_MNm = 98.0d0 ! [MN/m]


             output_config%max_lateral_force_density = 96.6d0 ! [MN/m^3]
             output_config%max_radial_force_density = 130.5d0   ! [MN/m^3]

             output_config%centering_force_max_MN = 428.1d0
             output_config%centering_force_min_MN = -70.3d0
             output_config%centering_force_avg_MN = 240.9d0
 
             output_config%min_plasma_coil_distance = 1.78d0
 
             output_config%min_bend_radius = 1.145 ! [m]
 
             output_config%neutron_peakfactor = 1.6

             output_config%epseff = 0.015

 
          case(4)
             ! w7x30 Machine
             output_config%name = "W7X-30"
             ! Reference point where all the other variables are determined from
             ! Plasma outer radius
             output_config%rmajor_ref = 5.50D0
             output_config%rminor_ref = 0.49D0
             output_config%aspect_ref =  11.2D0
 
             ! Coil radii
             output_config%coil_rmajor = 5.62D0
             output_config%coil_rminor = 1.36D0
 
 
             output_config%bt_ref = 3.0D0
             output_config%WP_area = 0.18d0*0.15d0
             output_config%WP_bmax = 10.6d0
 
             output_config%symmetry = 5
             output_config%coilspermodule = 6
             output_config%a1 = 0.98D0
             output_config%a2 = 0.041D0
             output_config%plasma_volume =   26.4D0
             output_config%dmin = 0.21D0
             output_config%max_portsize_width = 0.5D0
 
             output_config%plasma_surface = 128.3D0
             output_config%maximal_coil_height = 3.6D0  ! [m] Full height max point to min point
 
             output_config%coilsurface =  370.0D0! Coil surface, dimensionfull. At reference point
 
             output_config%coillength = 303.4D0 ! Central filament length of machine with outer radius 1m.
       
             output_config%I0 = 2.9D0 ! Coil Current needed to produce b0 on axis in [MA] at reference point
             output_config%inductance = 252.7D-6 ! inductance/R*A^2 in muH
 
             output_config%WP_ratio = 1.2D0
 
             output_config%max_force_density = 350.0d0 ! [MN/m^3]
             output_config%max_force_density_MNm = 98.0d0 ! [MN/m]


             output_config%max_lateral_force_density = 271.1d0 ! [MN/m^3]
             output_config%max_radial_force_density = 305.2d0   ! [MN/m^3]

             output_config%centering_force_max_MN = 7.95d0
             output_config%centering_force_min_MN = -2.15d0
             output_config%centering_force_avg_MN = 3.46d0
 
             output_config%min_plasma_coil_distance = 0.45D0 
 
             output_config%min_bend_radius = 0.186 ! [m]
 
             output_config%neutron_peakfactor = 1.6

             output_config%epseff = 0.015

          case(5)
             ! w7x50 Machine
             output_config%name = "W7X-50"
             ! Reference point where all the other variables are determined from
             ! Plasma outer radius
             output_config%rmajor_ref = 5.5D0
             output_config%rminor_ref = 0.49D0
             output_config%aspect_ref =   11.2D0
 
             ! Coil radii
             output_config%coil_rmajor = 5.62d0
             output_config%coil_rminor = 1.18D0
 
 
             output_config%bt_ref = 3.0D0
             output_config%WP_area = 0.18d0*0.15d0
             output_config%WP_bmax = 6.3d0
 
             output_config%symmetry = 5
             output_config%coilspermodule = 10
             output_config%a1 = 0.66D0
             output_config%a2 = 0.025D0
             output_config%plasma_volume =   26.4D0
             output_config%dmin = 0.28D0
             output_config%max_portsize_width = 0.3D0
 
             output_config%plasma_surface = 128.3D0
             output_config%maximal_coil_height = 3.1D0  ! [m] Full height max point to min point
 
             output_config%coilsurface =  299.85D0! Coil surface, dimensionfull. At reference point
 
             output_config%coillength = 420.67D0 ! Central filament length of machine with outer radius 1m.
       
             output_config%I0 = 1.745D0 ! Coil Current needed to produce b0 on axis in [MA] at reference point
             output_config%inductance = 412.4D-6 ! inductance/R*A^2 in muH
 
             output_config%WP_ratio = 1.2D0
 
             output_config%max_force_density = 250.0d0 ! [MN/m^3]
             output_config%max_force_density_MNm = 98.0d0 ! [MN/m]


             output_config%max_lateral_force_density = 116.4d0 ! [MN/m^3]
             output_config%max_radial_force_density = 148.d0   ! [MN/m^3]

             output_config%centering_force_max_MN = 2.99d0
             output_config%centering_force_min_MN = -1.29d0
             output_config%centering_force_avg_MN = 1.61d0
 
             output_config%min_plasma_coil_distance = 0.39D0
 
             output_config%min_bend_radius = 0.39 ! [m]
 
             output_config%neutron_peakfactor = 1.6

             output_config%epseff = 0.015
         
          
          
          case(6)
             ! Init from json
             output_config = stella_config_json()
          case default
             ! Return some error here. The index is not implemented yet.
             write(*,*)'ERROR in initialization of stellarator config. No such istell: ',index
       end select
 
    end function new_stella_config
 


    function stella_config_json() result(output_config)

        !! Initialises the effective stellarator values using a json input file
        !! author: J Lion, IPP Greifswald
        !! None
        !! This routine reads in all effective variables that
        !! are given needed by the 'constructor' stella_config
        !! <P>The effective values are read in from a JSON-format file.
        !! None
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        use fson_library, only: fson_parse, fson_value, fson_get, fson_destroy 


    
        !  Arguments
    
        !  Local variables
    
        integer :: n_values
        character(len=180) :: filename
        type(fson_value), pointer :: stellafile
        type(stella_config) :: output_config

        real(dp), dimension(:), allocatable :: nustar,d11,d13
        !type(stella_config), allocatable, dimension(:) :: stella_json

        ! Obtain the root directory
        ! I don't know why but this #include needs to tabbed to the very left of the line.
#include "root.dir"
    
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        !  Parse the json file
    
        filename = 'stella_conf.json'
        stellafile => fson_parse(trim(filename))
    
        !  Extract information arrays from the file
    
        call fson_get(stellafile, "name", output_config%name)
        call fson_get(stellafile, "symmetry", output_config%symmetry)

        call fson_get(stellafile, "coilspermodule", output_config%coilspermodule)
        call fson_get(stellafile, "rmajor_ref", output_config%rmajor_ref)
        call fson_get(stellafile, "rminor_ref", output_config%rminor_ref)
        call fson_get(stellafile, "coil_rmajor", output_config%coil_rmajor)
        call fson_get(stellafile, "coil_rminor", output_config%coil_rminor)
        call fson_get(stellafile, "aspect_ref", output_config%aspect_ref)
        call fson_get(stellafile, "bt_ref", output_config%bt_ref)
        call fson_get(stellafile, "WP_area", output_config%WP_area)
        call fson_get(stellafile, "WP_bmax", output_config%WP_bmax)
        call fson_get(stellafile, "i0", output_config%i0)
        call fson_get(stellafile, "a1", output_config%a1)
        call fson_get(stellafile, "a2", output_config%a2)
        call fson_get(stellafile, "dmin", output_config%dmin)
        call fson_get(stellafile, "inductance", output_config%inductance)
        call fson_get(stellafile, "coilsurface", output_config%coilsurface)
        call fson_get(stellafile, "coillength", output_config%coillength)
        call fson_get(stellafile, "max_portsize_width", output_config%max_portsize_width)
        call fson_get(stellafile, "maximal_coil_height", output_config%maximal_coil_height)
        call fson_get(stellafile, "min_plasma_coil_distance", output_config%min_plasma_coil_distance)
        call fson_get(stellafile, "plasma_volume", output_config%plasma_volume)
        call fson_get(stellafile, "plasma_surface", output_config%plasma_surface)
        call fson_get(stellafile, "WP_ratio", output_config%WP_ratio)
        call fson_get(stellafile, "max_force_density", output_config%max_force_density)
        call fson_get(stellafile, "max_force_density_MNm", output_config%max_force_density_MNm)
        call fson_get(stellafile, "min_bend_radius", output_config%min_bend_radius)
        call fson_get(stellafile, "epseff", output_config%epseff)
        
        call fson_get(stellafile, "max_lateral_force_density", output_config%max_lateral_force_density)
        call fson_get(stellafile, "max_radial_force_density", output_config%max_radial_force_density)

        call fson_get(stellafile, "centering_force_max_MN", output_config%centering_force_max_MN)
        call fson_get(stellafile, "centering_force_min_MN", output_config%centering_force_min_MN)
        call fson_get(stellafile, "centering_force_avg_MN", output_config%centering_force_avg_MN)

        call fson_get(stellafile, "neutron_peakfactor", output_config%neutron_peakfactor)


        call fson_get(stellafile, "number_nu_star", n_values)


        allocate(output_config%D11_star_mono_input(n_values))
        allocate(output_config%nu_star_mono_input(n_values))


        call fson_get(stellafile, "D11_star_mono_input", output_config%D11_star_mono_input)
        call fson_get(stellafile, "nu_star_mono_input", output_config%nu_star_mono_input)




        !  Clean up
        call fson_destroy(stellafile)
    
    end function stella_config_json


    subroutine stella_error(index,keyname)
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Gives the errors of a the stellarator module

    integer, intent(in) :: index
    character, intent(in) :: keyname


    select case (index)
    case(1)
        ! Error reading in a json attribute
        ! Not used yet because I don't know how.
        write(*,*)'ERROR in initialization of stellarator config. Missing json key: ',keyname


    case default
        ! Return some error here. The index is not implemented yet.
        write(*,*)'ERROR in stella_error! No such error index: ',index

    end select


    end subroutine stella_error


 end module stellarator_configuration