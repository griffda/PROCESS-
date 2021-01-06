module stellarator_configuration
  !! author: J Lion, IPP Greifswald
  !! Module containing defining parameters for a stellarator
  !!
  !! This module contains a set of constants that defines a
  !! stellarator configuration. These parameters are based on external
  !! calculations.
  !! The list will be modified in further commits.
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none


  type :: stella_config

      ! Name
      character (len = 20) :: name

      !  Number of coils
      integer symmetry
      !  Coils per module
      integer coilspermodule


      !  Reference Point where all the other variables are determined
      real(kind(1.0D0)) ::  rmajor_ref
      real(kind(1.0D0)) ::  rminor_ref
      real(kind(1.0D0)) ::  aspect_ref
      real(kind(1.0D0)) ::  bt_ref
      
      !  Coil current needed for 1T on axis at outer radius 1m
      real(kind(1.0D0)) :: i0
      !  Magnetic field fit parameter a1
      real(kind(1.0D0)) :: a1
      !  Magnetic field fit parameter a2
      real(kind(1.0D0)) :: a2
      !  Minimal intercoil distance dmin/Rmax
      real(kind(1.0D0)) :: dmin
      !  Inductivity
      real(kind(1.0D0)) :: inductivity
      !  Coil surface
      real(kind(1.0D0)) :: coilsurface
      !  Total coil length
      real(kind(1.0D0)) :: coillength
      !  Port size in toroidal direction
      real(kind(1.0D0)) :: max_portsize_width
      !  Average minor Coil Radius (as r/R) to get it dimensionless.
      real(kind(1.0D0)) :: coil_epsilon 

      !  The ratio of the coil to plasma radius.
      !  (Average midpoint of the coils to average maj. radius. Will be approx. 1)
      real(kind(1.0D0)) :: coil_to_plasma_ratio

      !  The maximal coil height at reference point.
      real(kind(1.0D0)) :: maximal_coil_height

      !  The minimal distance between coil and plasma
      real(kind(1.0D0)) :: min_plasma_coil_distance

      !  2 volume parameters v1 and v2. They scale according to r*R^2 and r^3
      real(kind(1.0D0)) :: vr2r

      !  2 surface parameters s0 and s1. They are the leading taylor orders.
      real(kind(1.0D0)) :: s0 

      !  Ratio radial to toroidal length of the winding pack. (a1 and a2 should be calculated using this value)
      real(kind(1.0D0)) :: WP_ratio

      !  Maximal force density in MN/m^3 at one point:
      real(kind(1.0D0)) :: max_force_density


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
            new_stella_config%name = "Helias 5b"
            ! Reference point where all the other variables are determined from
            ! Plasma outer radius
            new_stella_config%rmajor_ref = 22.0D0
            new_stella_config%aspect_ref = 12.5D0
            new_stella_config%rminor_ref = 22.0D0/12.5D0
            new_stella_config%bt_ref = 5.6D0

            new_stella_config%symmetry = 5
            new_stella_config%coilspermodule = 10
            new_stella_config%a1 = 0.712D0
            new_stella_config%a2 = 0.027D0
            new_stella_config%vr2r = 19.816D0  ! This value is for Helias 5
            new_stella_config%dmin = 0.84D0
            new_stella_config%max_portsize_width = 2.12D0 

            new_stella_config%s0 = 49.228D0 ! Plasma Surface

            new_stella_config%maximal_coil_height = 12.7 ! [m] Full height max point to min point

            new_stella_config%coilsurface = 4817.7D0 ! Coil surface, dimensionfull. At reference point
      
            new_stella_config%coil_epsilon = 4.7D0 / 22.46D0

            new_stella_config%coillength = 1681.0D0 ! Central filament length of machine with outer radius 1m.

            new_stella_config%coil_to_plasma_ratio = 22.46D0/22.0D0 ! Approximately
      
            new_stella_config%I0 = 13.06D0/5.6D0/22.46D0 ! Coil Current needed to produce 1T on axis in [MA] at outer radius 1m
            new_stella_config%inductivity = 1655.76D-6/22.0D0* 12.5D0**2 ! Inductivity/R*A^2 in muH/m

            new_stella_config%WP_ratio = 1.3D0 ! The fit values in stellarator config class should be calculated using this value.

            new_stella_config%max_force_density = 0.37 ! Multiply with I^2 [MA] A_wp^(-1) [m^2] to obtain [MN/m^3]

            new_stella_config%min_plasma_coil_distance = (4.7D0-1.76D0)/22.0D0 ! This is coil minor radius - plasma radius divided by major radius for scaling.

        case(2)
            ! Helias4 Machine
            new_stella_config%name = "Helias 4"
            ! Reference point where all the other variables are determined from
            ! Plasma outer radius
            new_stella_config%rmajor_ref = 18.0D0
            new_stella_config%rminor_ref = 2.1D0
            new_stella_config%aspect_ref =  18.0D0/2.1D0
            new_stella_config%bt_ref = 5.6D0

            new_stella_config%symmetry = 4
            new_stella_config%coilspermodule = 10
            new_stella_config%a1 = 0.691D0
            new_stella_config%a2 = 0.031D0
            new_stella_config%vr2r =   1560.0D0/18.0D0/2.1D0**2
            new_stella_config%dmin = 1.08D0
            new_stella_config%max_portsize_width = 3.24D0

            new_stella_config%s0 = 1492.0D0/18.0D0/2.1D0
            new_stella_config%maximal_coil_height = 13.34D0  ! [m] Full height max point to min point

            new_stella_config%coilsurface =  4019.0D0! Coil surface, dimensionfull. At reference point
      
            new_stella_config%coil_epsilon = 4.95D0 / 18.41D0

            new_stella_config%coillength = 1435.07D0 ! Central filament length of machine with outer radius 1m.

            new_stella_config%coil_to_plasma_ratio = 1.0D0 ! Approximately
      
            new_stella_config%I0 = 13.146D0/5.6D0/18.0D0 ! Coil Current needed to produce 1T on axis in [MA] at outer radius 1m
            new_stella_config%inductivity = 1290.4D-6/18.0D0*(18.0D0/2.1D0)**2 ! Inductivity/R*A^2 in muH/m

            new_stella_config%WP_ratio = 1.3D0

            new_stella_config%max_force_density = 0.37 ! Multiply with I^2 [MA] A_wp^(-1) [m^2] to obtain [MN/m^3]

            new_stella_config%min_plasma_coil_distance = (4.95D0-2.1D0)/18.0D0 


        case(3)
            ! Helias 3 Machine
            new_stella_config%name = "Helias 3"
            ! Reference point where all the other variables are determined from
            ! Plasma outer radius
            new_stella_config%rmajor_ref = 15.0D0
            new_stella_config%rminor_ref = 2.5D0
            new_stella_config%aspect_ref =  15.0D0/2.5D0  
            new_stella_config%bt_ref = 5.6D0

            new_stella_config%symmetry = 3
            new_stella_config%coilspermodule = 10
            new_stella_config%a1 = 0.571D0
            new_stella_config%a2 = 0.033D0
            new_stella_config%vr2r =   1600.0D0/15.0D0/2.5D0**2
            new_stella_config%dmin = 1.15D0
            new_stella_config%max_portsize_width = 3.24D0

            new_stella_config%s0 = 1480.44D0/15.0D0/2.5D0

            new_stella_config%maximal_coil_height = 17.74D0! [m] Full height max point to min point

            new_stella_config%coilsurface = 4114.0D0 ! Coil surface, dimensionfull. At reference point
      
            new_stella_config%coil_epsilon = 6.12D0/14.58D0

            new_stella_config%coillength = 1287.3D0 ! Central filament length of machine with outer radius 1m.

            new_stella_config%coil_to_plasma_ratio = 1.0D0 ! Approximately
      
            new_stella_config%I0 = 14.23D0/5.6D0/15.0D0 ! Coil Current needed to produce 1T on axis in [MA] at outer radius 1m
            new_stella_config%inductivity = 1250.7D-6/15.0D0*(15.0D0/2.5D0)**2 ! Inductivity/R*A^2 in muH/m

            new_stella_config%WP_ratio = 1.3D0

            new_stella_config%max_force_density = 0.37 ! Multiply with I^2 [MA] A_wp^(-1) [m^2] to obtain [MN/m^3]

            new_stella_config%min_plasma_coil_distance = (6.12D0-2.5D0)/15.0D0 

        case default
            ! Return some error here. The index is not implemented yet.
            write(*,*)'ERROR in initialization of stellarator config. No such istell: ',index
      end select

  end function new_stella_config
end module stellarator_configuration