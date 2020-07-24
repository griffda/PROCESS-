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


    character (len=35), parameter :: keys(26) = [Character(len=35):: "name",&
                                            "symmetry",&
                                            "coilspermodule",&
                                            "rmajor_ref",&
                                            "rminor_ref",&
                                            "coil_rmajor",&
                                            "coil_rminor",&
                                            "aspect_ref",&
                                            "bt_ref",&
                                            "WP_area",&
                                            "WP_bmax",&
                                            "i0",&
                                            "a1",&
                                            "a2",&
                                            "dmin",&
                                            "inductance",&
                                            "coilsurface",&
                                            "coillength",&
                                            "max_portsize_width",&
                                            "maximal_coil_height",&
                                            "min_plasma_coil_distance",&
                                            "plasma_volume",&
                                            "plasma_surface",&
                                            "WP_ratio",&
                                            "max_force_density",&
                                            "min_bend_radius"]



 
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
       !  Maximal integrated force density at reference point in a WP cross section [MN/m^3]
       
       real(dp) min_bend_radius
       !  Minimal bending radius at reference point [m]
 
       real(dp) epseff
       !  Maximal epsilon effective in the core region [1]

       real(dp) max_lateral_force_density
       !  Maximal lateral force density of the coil set [MN/m]

       real(dp) max_radial_force_density
       !  Maximal radial force density of the coil set [MN/m]

       real(dp), dimension(:), allocatable :: D11_star_mono_input
       !  The monoenergetic radial transport coefficients normalized by the plateau value.
       real(dp), dimension(:), allocatable :: nu_star_mono_input
       !  The monoenergetic radial transport coefficients normalized by the plateau value.
 
    end type stella_config
 
    ! Overload fortran constructor
    interface stella_config
       procedure :: new_stella_config
    end interface stella_config
 
 contains
 
    type(stella_config) function new_stella_config(index)
       integer, intent(in) :: index
 
 
       select case (index)
 
          ! This is the istell case switch:
          ! istell = 1: Helias5 machine
          ! istell = 2: Helias4 machine
          ! istell = 3: Helias3 machine
 
          ! All parameters set here are prelimnirary versions and might be changed in further commits
 
          case(1)
             ! Helias5 Machine
             ! The values are given at the reference point
 
             new_stella_config%name = "Helias 5b"
 
             new_stella_config%rmajor_ref = 22.2D0
             new_stella_config%rminor_ref = 1.80D0
             new_stella_config%aspect_ref = 12.33D0
 
             ! Coil radii
             new_stella_config%coil_rmajor = 22.44D0
             new_stella_config%coil_rminor = 4.76D0
 
             new_stella_config%bt_ref = 5.6D0
             new_stella_config%WP_area = 0.8d0*0.6d0
             new_stella_config%WP_bmax = 11.44d0
 
             new_stella_config%symmetry = 5
             new_stella_config%coilspermodule = 10
 
             new_stella_config%a1 = 0.688D0
             new_stella_config%a2 = 0.025D0
 
             new_stella_config%plasma_volume = 1422.63D0  ! This value is for Helias 5
             new_stella_config%dmin = 0.84D0
             new_stella_config%max_portsize_width = 2.12D0 
 
             new_stella_config%plasma_surface = 1960.0D0 ! Plasma Surface
 
             new_stella_config%maximal_coil_height = 12.7 ! [m] Full height max point to min point
 
             new_stella_config%coilsurface = 4817.7D0 ! Coil surface, dimensionfull. At reference point
 
             new_stella_config%coillength = 1680.0D0 ! Central filament length of machine with outer radius 1m.
       
             new_stella_config%I0 = 13.06D0 ! Coil Current needed to produce 1T on axis in [MA] at outer radius 1m
             new_stella_config%inductance = 1655.76D-6 ! inductance in muH
 
             new_stella_config%WP_ratio = 1.2D0 ! The fit values in stellarator config class should be calculated using this value.
 
             new_stella_config%max_force_density = 120.0d0 ! [MN/m^3]
 
             new_stella_config%min_plasma_coil_distance = 1.9d0
 
             new_stella_config%min_bend_radius = 1.0 ! [m]
 
          case(2)
             ! Helias4 Machine
             new_stella_config%name = "Helias 4"
             ! Reference point where all the other variables are determined from
             ! Plasma outer radius
             new_stella_config%rmajor_ref = 17.6D0
             new_stella_config%rminor_ref = 2.0D0
             new_stella_config%aspect_ref =  8.8D0
 
             ! Coil radii
             new_stella_config%coil_rmajor = 18.39D0
             new_stella_config%coil_rminor = 4.94D0
 
             new_stella_config%bt_ref = 5.6D0
             new_stella_config%WP_area = 0.8d0*0.6d0
             new_stella_config%WP_bmax = 11.51d0
 
             new_stella_config%symmetry = 4
             new_stella_config%coilspermodule = 10
             new_stella_config%a1 = 0.676D0
             new_stella_config%a2 = 0.029D0
             new_stella_config%plasma_volume =   1380.0D0
             new_stella_config%dmin = 1.08D0
             new_stella_config%max_portsize_width = 3.24D0
 
             new_stella_config%plasma_surface = 1900.0D0
             new_stella_config%maximal_coil_height = 13.34D0  ! [m] Full height max point to min point
 
             new_stella_config%coilsurface =  4100.0D0! Coil surface, dimensionfull. At reference point
 
             new_stella_config%coillength = 1435.07D0 ! Central filament length of machine with outer radius 1m.
       
             new_stella_config%I0 = 13.146D0 ! Coil Current needed to produce b0 on axis in [MA] at reference point
             new_stella_config%inductance = 1290.4D-6 ! inductance/R*A^2 in muH
 
             new_stella_config%WP_ratio = 1.3D0
 
             new_stella_config%max_force_density = 120.0d0 ! [MN/m^3]
 
             new_stella_config%min_plasma_coil_distance = 1.7d0
 
             new_stella_config%min_bend_radius = 0.86 ! [m]
 
 
          case(3)
             ! Helias 3 Machine
             new_stella_config%name = "Helias 3"
             ! Reference point where all the other variables are determined from
             ! Plasma outer radius
             new_stella_config%rmajor_ref = 13.86d0
             new_stella_config%rminor_ref = 2.18d0
             new_stella_config%aspect_ref =  6.36d0
 
             ! Coil radii
             new_stella_config%coil_rmajor = 14.53D0
             new_stella_config%coil_rminor = 6.12D0
             
             new_stella_config%bt_ref = 5.6D0
             new_stella_config%WP_bmax = 12.346d0
             new_stella_config%WP_area = 0.8d0*0.6d0
 
             new_stella_config%symmetry = 3
             new_stella_config%coilspermodule = 10
 
             ! Bmax fit parameters
             new_stella_config%a1 = 0.56D0
             new_stella_config%a2 = 0.030D0
 
             new_stella_config%plasma_volume =   1300.8D0
             new_stella_config%dmin = 1.145D0
             new_stella_config%max_portsize_width = 3.24D0 !??? guess. not ready yet
 
             new_stella_config%plasma_surface = 1600.00D0
 
             new_stella_config%maximal_coil_height = 17.74D0! [m] Full height max point to min point
 
             new_stella_config%coilsurface = 4240.0D0 ! Coil surface, dimensionfull. At reference point
 
             new_stella_config%coillength = 1287.3D0 ! Central filament length of machine with outer radius 1m.
       
             new_stella_config%I0 = 14.23D0 ! Coil Current needed to produce 1T on axis in [MA] at outer radius 1m
             new_stella_config%inductance = 1250.7D-6 ! inductance in muH
 
             new_stella_config%WP_ratio = 1.3D0
 
             new_stella_config%max_force_density = 120.0d0 ! Multiply with I^2 [MA] A_wp^(-1) [m^2] to obtain [MN/m^3]
 
             new_stella_config%min_plasma_coil_distance = 1.78d0
 
             new_stella_config%min_bend_radius = 1.145 ! [m]
 
 
          case(4)
             ! w7x30 Machine
             new_stella_config%name = "W7X-30"
             ! Reference point where all the other variables are determined from
             ! Plasma outer radius
             new_stella_config%rmajor_ref = 5.50D0
             new_stella_config%rminor_ref = 0.49D0
             new_stella_config%aspect_ref =  11.2D0
 
             ! Coil radii
             new_stella_config%coil_rmajor = 5.62D0
             new_stella_config%coil_rminor = 1.36D0
 
 
             new_stella_config%bt_ref = 3.0D0
             new_stella_config%WP_area = 0.18d0*0.15d0
             new_stella_config%WP_bmax = 10.6d0
 
             new_stella_config%symmetry = 5
             new_stella_config%coilspermodule = 6
             new_stella_config%a1 = 0.98D0
             new_stella_config%a2 = 0.041D0
             new_stella_config%plasma_volume =   26.4D0
             new_stella_config%dmin = 0.21D0
             new_stella_config%max_portsize_width = 0.5D0
 
             new_stella_config%plasma_surface = 128.3D0
             new_stella_config%maximal_coil_height = 3.6D0  ! [m] Full height max point to min point
 
             new_stella_config%coilsurface =  370.0D0! Coil surface, dimensionfull. At reference point
 
             new_stella_config%coillength = 303.4D0 ! Central filament length of machine with outer radius 1m.
       
             new_stella_config%I0 = 2.9D0 ! Coil Current needed to produce b0 on axis in [MA] at reference point
             new_stella_config%inductance = 252.7D-6 ! inductance/R*A^2 in muH
 
             new_stella_config%WP_ratio = 1.2D0
 
             new_stella_config%max_force_density = 350.0d0 ! [MN/m^3]
 
             new_stella_config%min_plasma_coil_distance = 0.45D0 
 
             new_stella_config%min_bend_radius = 0.186 ! [m]
 
          case(5)
             ! w7x50 Machine
             new_stella_config%name = "W7X-50"
             ! Reference point where all the other variables are determined from
             ! Plasma outer radius
             new_stella_config%rmajor_ref = 5.5D0
             new_stella_config%rminor_ref = 0.49D0
             new_stella_config%aspect_ref =   11.2D0
 
             ! Coil radii
             new_stella_config%coil_rmajor = 5.62d0
             new_stella_config%coil_rminor = 1.18D0
 
 
             new_stella_config%bt_ref = 3.0D0
             new_stella_config%WP_area = 0.18d0*0.15d0
             new_stella_config%WP_bmax = 6.3d0
 
             new_stella_config%symmetry = 5
             new_stella_config%coilspermodule = 10
             new_stella_config%a1 = 0.66D0
             new_stella_config%a2 = 0.025D0
             new_stella_config%plasma_volume =   26.4D0
             new_stella_config%dmin = 0.28D0
             new_stella_config%max_portsize_width = 0.3D0
 
             new_stella_config%plasma_surface = 128.3D0
             new_stella_config%maximal_coil_height = 3.1D0  ! [m] Full height max point to min point
 
             new_stella_config%coilsurface =  299.85D0! Coil surface, dimensionfull. At reference point
 
             new_stella_config%coillength = 420.67D0 ! Central filament length of machine with outer radius 1m.
       
             new_stella_config%I0 = 1.745D0 ! Coil Current needed to produce b0 on axis in [MA] at reference point
             new_stella_config%inductance = 412.4D-6 ! inductance/R*A^2 in muH
 
             new_stella_config%WP_ratio = 1.2D0
 
             new_stella_config%max_force_density = 250.0d0 ! [MN/m^3]
 
             new_stella_config%min_plasma_coil_distance = 0.39D0
 
             new_stella_config%min_bend_radius = 0.39 ! [m]
 
 
         
          
          
          case(6)
             ! Init from json
             new_stella_config = stella_config_json()

          case default
             ! Return some error here. The index is not implemented yet.
             write(*,*)'ERROR in initialization of stellarator config. No such istell: ',index
       end select
 
    end function new_stella_config
 


    type(stella_config) function stella_config_json()

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
        !type(stella_config), allocatable, dimension(:) :: stella_json

        ! Obtain the root directory
        ! I don't know why but this #include needs to tabbed to the very left of the line.
#include "root.dir"
    
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
        !  Parse the json file
    
        filename = 'stella_conf.json'
        stellafile => fson_parse(trim(filename))
    
        !  Extract information arrays from the file
    
        call fson_get(stellafile, "name", stella_config_json%name)
        call fson_get(stellafile, "symmetry", stella_config_json%symmetry)

        call fson_get(stellafile, "coilspermodule", stella_config_json%coilspermodule)
        call fson_get(stellafile, "rmajor_ref", stella_config_json%rmajor_ref)
        call fson_get(stellafile, "rminor_ref", stella_config_json%rminor_ref)
        call fson_get(stellafile, "coil_rmajor", stella_config_json%coil_rmajor)
        call fson_get(stellafile, "coil_rminor", stella_config_json%coil_rminor)
        call fson_get(stellafile, "aspect_ref", stella_config_json%aspect_ref)
        call fson_get(stellafile, "bt_ref", stella_config_json%bt_ref)
        call fson_get(stellafile, "WP_area", stella_config_json%WP_area)
        call fson_get(stellafile, "WP_bmax", stella_config_json%WP_bmax)
        call fson_get(stellafile, "i0", stella_config_json%i0)
        call fson_get(stellafile, "a1", stella_config_json%a1)
        call fson_get(stellafile, "a2", stella_config_json%a2)
        call fson_get(stellafile, "dmin", stella_config_json%dmin)
        call fson_get(stellafile, "inductance", stella_config_json%inductance)
        call fson_get(stellafile, "coilsurface", stella_config_json%coilsurface)
        call fson_get(stellafile, "coillength", stella_config_json%coillength)
        call fson_get(stellafile, "max_portsize_width", stella_config_json%max_portsize_width)
        call fson_get(stellafile, "maximal_coil_height", stella_config_json%maximal_coil_height)
        call fson_get(stellafile, "min_plasma_coil_distance", stella_config_json%min_plasma_coil_distance)
        call fson_get(stellafile, "plasma_volume", stella_config_json%plasma_volume)
        call fson_get(stellafile, "plasma_surface", stella_config_json%plasma_surface)
        call fson_get(stellafile, "WP_ratio", stella_config_json%WP_ratio)
        call fson_get(stellafile, "max_force_density", stella_config_json%max_force_density)
        call fson_get(stellafile, "min_bend_radius", stella_config_json%min_bend_radius)
        call fson_get(stellafile, "epseff", stella_config_json%epseff)
        call fson_get(stellafile, "max_lateral_force_density", stella_config_json%max_lateral_force_density)
        call fson_get(stellafile, "max_radial_force_density", stella_config_json%max_radial_force_density)

        call fson_get(stellafile, "D11_star_mono_input", stella_config_json%D11_star_mono_input)
        call fson_get(stellafile, "nu_star_mono_input", stella_config_json%nu_star_mono_input)



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