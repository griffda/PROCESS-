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
      real(kind(1.0D0))  rmajor_ref
      real(kind(1.0D0))  rminor_ref
      real(kind(1.0D0))  aspect_ref
      real(kind(1.0D0))  bt_ref
      
      !  Coil current needed for 1T on axis at outer radius 1m
      real(kind(1.0D0)) i0
      !  Magnetic field fit parameter a1
      real(kind(1.0D0)) a1
      !  Magnetic field fit parameter a2
      real(kind(1.0D0)) a2
      !  Minimal intercoil distance dmin/Rmax
      real(kind(1.0D0)) dmin
      !  Inductivity
      real(kind(1.0D0)) inductivity
      !  Coil surface
      real(kind(1.0D0)) coilsurface
      !  Total coil length
      real(kind(1.0D0)) coillength
      !  Port size in toroidal direction
      real(kind(1.0D0)) max_portsize_width
      !  Average minor Coil Radius (as r/R) to get it dimensionless.
      real(kind(1.0D0)) coil_epsilon 

      !  The ratio of the coil to plasma radius.
      !  (Average midpoint of the coils to average maj. radius. Will be approx. 1)
      real(kind(1.0D0)) coil_to_plasma_ratio

      !  The maximal coil height at reference point.
      real(kind(1.0D0)) maximal_coil_height

      !  The minimal distance between coil and plasma
      real(kind(1.0D0)) min_plasma_coil_distance

      !  2 volume parameters v1 and v2. They scale according to r*R^2 and r^3
      real(kind(1.0D0)) vr2r

      !  2 surface parameters s0 and s1. They are the leading taylor orders.
      real(kind(1.0D0)) s0 

      !  Ratio radial to toroidal length of the winding pack. (a1 and a2 should be calculated using this value)
      real(kind(1.0D0)) WP_ratio

      !  Maximal force density in MN/m^3 at one point:
      real(kind(1.0D0)) max_force_density


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


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module stellarator_module

  !! Module containing stellarator routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! parameters of the first wall, blanket and shield components
  !! of a fusion power plant.

  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use availability_module
  use build_variables
  use buildings_module
  use constants
  use constraint_variables
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
  use physics_functions_module
  use physics_module
  use physics_variables
  use plasma_geometry_module
  use power_module
  use process_output
  use profiles_module

  use sctfcoil_module
  use stellarator_variables
  use structure_module
  use structure_variables
  use tfcoil_variables
  use times_variables
  use vacuum_module

  ! Include the stellarator configuration
  use stellarator_configuration, only : stella_config, new_stella_config

  implicit none


  type(stella_config) :: config


  real(kind(1.0D0)), private :: f_n,f_r,f_a,f_b,f_i ! scaling parameters to reference point.

  logical :: first_call = .true.
  private :: config
  public :: stcall, stinit, stout

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcall

    !! Routine to call the physics and engineering modules
    !! relevant to stellarators
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! None
    !! This routine is the caller for the stellarator models.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call stnewconfig          
    call stgeom
    call stphys
    call stcoil(nout,0)
    call stbild(nout,0)
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


    ! set first call variable to false:
    first_call  = .false.
  end subroutine stcall

  subroutine stinit

    !! Routine to initialise the variables relevant to stellarators
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! None
    !! This routine initialises the variables relevant to stellarators.
    !! Many of these may override the values set in routine
    !! <A HREF="initial.html">initial</A>.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    !real(kind(1.0D0)) :: fsum

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! This routine is called before (!!!) the input file. put everything that depends on the input file in stcaller
    if (istell == 0) return

    !  Numerics quantities

    boundl(1) = 5.0D0

    boundu(1) = 20.0D0
    boundu(3) = 30.0D0
    boundu(29) = 20.0D0

    icase = 'Stellarator model'


    !  Are the following lines still up to date?
    !  Build quantities

    ohcth = 0.0D0
    iohcl = 0
    ohhghf = 0.0D0
    gapoh = 0.0D0
    tfootfi = 1.0D0

    !  Physics quantities

    dnbeta = 0.0D0
    rmajor = 20.0D0
    kappa = 1.0D0
    kappa95 = 1.0D0
    triang = 0.0D0
    q = 1.03D0

    !  Turn off current drive

    irfcd = 0

    !  Times for different phases

    tramp = 0.0D0
    tohs = 0.0D0
    tburn = 3.15576D7  !  one year
    tqnch = 0.0D0
    tpulse = tohs + theat + tburn + tqnch
    tdown  = tramp + tohs + tqnch + tdwell
    tcycle = tramp + tohs + theat + tburn + tqnch + tdwell


    !  Blanket properties
    !secondary_cycle = 0  !  simple thermal hydraulic model assumed

    !  CCFE HCPB blanket model
	!if (iblanket == 1) then
    !  fsum = fbltibe12 + fblli2sio4 + fblss + vfcblkt + vfpblkt
    !  if (abs(fsum-1.0D0) > 1.0D-4) then
    !    idiags(1) = iblanket
    !    fdiags(2) = fbltibe12
    !    fdiags(3) = fblli2sio4
    !    fdiags(4) = fblss
    !    fdiags(5) = vfcblkt
    !    fdiags(6) = vfpblkt
    !    fdiags(7) = fsum
    !    call report_error(165)
    !  end if
    !end if

    !if (ipowerflow == 0) blkttype = 3

    !  Coolant fluid type

    !if ((blkttype == 1).or.(blkttype == 2)) then
    !   coolwh = 2  !  water
    !else
    !   coolwh = 1  !  helium
    !end if

    !  But... set coolant to water if blktmodel > 0
    !  Although the *blanket* is by definition helium-cooled in this case,
    !  the shield etc. are assumed to be water-cooled, and since water is
    !  heavier (and the unit cost of pumping it is higher), the calculation
    !  for coolmass is better done with coolwh=2 if blktmodel > 0 to give
    !  slightly pessimistic results.

    !if (blktmodel > 0) then
    !   secondary_cycle = 0
    !   blkttype = 3  !  HCPB
    !   coolwh = 2
    !end if

    !  Ensure that blanket material fractions add up to 1.0

    !if (blkttype < 3) then
    !   fsum = fblli2o + fblbe + vfblkt + fblss + fblvd
    !   if (abs(fsum-1.0D0) > 1.0D-4) then
    !      idiags(1) = blkttype
    !      fdiags(1) = fblli2o
    !      fdiags(2) = fblbe
    !      fdiags(3) = vfblkt
    !      fdiags(4) = fblss
    !      fdiags(5) = fblvd
    !      fdiags(6) = fsum
    !      call report_error(165)
    !   end if
    !else
    !   fsum = fbllipb + fblli + vfblkt + fblss + fblvd
    !   if (abs(fsum-1.0D0) > 1.0D-4) then
    !      idiags(1) = blkttype
    !      fdiags(1) = fbllipb
    !      fdiags(2) = fblli
    !      fdiags(3) = vfblkt
    !      fdiags(4) = fblss
    !      fdiags(5) = fblvd
    !      fdiags(6) = fsum
    !      call report_error(165)
    !   end if
    !end if

  end subroutine stinit

  subroutine stnewconfig
    !! author: J Lion, IPP Greifswald
    !! Routine to initialise the stellarator configuration
    !!
    !! Routine to initialise the stellarator configuration.
    !! This routine is called right before the calculation and could
    !! in principle overwrite variables from the input file.
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    config = new_stella_config(istell)

    n_tf = config%coilspermodule*config%symmetry !! This overwrites n_tf in input file.
   
    !  Factors used to scale the reference point.
    f_R = rmajor/config%rmajor_ref       !  Size scaling factor with respect to Helias 5-B
    f_a = aspect / config%aspect_ref
    f_N = n_tf/(config%coilspermodule * config%symmetry)       !  Coil number factor
    f_B = bt/config%bt_ref            !  B-field scaling factor
 


  end subroutine stnewconfig

  subroutine stgeom
    !! author: J Lion, IPP Greifswald
    !! Routine to calculate the plasma volume and surface area for
    !! a stellarator using precalculated effective values
    !!
    !! This routine calculates the plasma volume and surface area for
    !! a stellarator configuration.
    !! It is simple scaling based on a Fourier representation based on 
    !! that described in Geiger documentation.
    !!
    !! J. Geiger, IPP Greifswald internal document:  'Darstellung von
    !! ineinandergeschachtelten toroidal geschlossenen Flaechen mit
    !! Fourierkoeffizienten' ('Representation of nested, closed
    !! surfaces with Fourier coefficients')
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    !  Cross-sectional area, averaged over toroidal angle
    rminor = rmajor/aspect
    eps = 1.0D0/aspect

    vol = rmajor*rminor**2 * config%vr2r

    sarea = rmajor*rminor * config%s0 

    xarea = pi*rminor*rminor  ! average, could be calculated for every toroidal angle if desired

    !  sareao is retained only for obsolescent fispact calculation...

    sareao = 0.5D0*sarea  !  Used only in the divertor model; approximate as for tokamaks

  end subroutine stgeom

  subroutine stbild(outfile,iprint)

    !! Routine to determine the build of a stellarator machine
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine determines the build of the stellarator machine.
    !! The values calculated are based on the mean minor radius, etc.,
    !! as the actual radial and vertical build thicknesses vary with
    !! toroidal angle.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
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

    ! First Wall

    fwith = 2.0D0*afw + 2.0D0*fw_wall
    fwoth = fwith

    !  Radial build to centre of plasma (should be equal to rmajor)

    rbld = bore + ohcth + gapoh + tfcth + gapds + &
         ddwi + shldith + blnkith + fwith + scrapli + rminor


    ! Bc stellarators cannot scale rmninor reasonably well an additional constraint equation is required,
    ! that ensures that there is enough space between coils and plasma.
    required_radial_space = (tfcth/2.0D0 + gapds + ddwi + shldith + blnkith + fwith + scrapli)

    available_radial_space = config%min_plasma_coil_distance * rmajor



    !  Radius to inner edge of inboard shield

    rsldi = rmajor - rminor - scrapli - fwith - blnkith - shldith

    !  Radius to outer edge of outboard shield

    rsldo = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth

    !  Thickness of outboard TF coil legs

    tfthko = tfcth

    !  Radius to centre of outboard TF coil legs

    gapsto = gapomin
    r_tf_outboard_mid = rsldo + ddwi + gapsto + 0.5D0*tfthko

    !  Height to inside edge of TF coil
    !  Roughly equal to average of (inboard build from TF coil to plasma
    !  centre) and (outboard build from plasma centre to TF coil)

    hmax = 0.5D0 * ( &
         (gapds+ddwi+shldith+blnkith+fwith+scrapli+rminor) + &
         (rminor+scraplo+fwoth+blnkoth+shldoth+ddwi+gapsto) )

    !  Outer divertor strike point radius, set equal to major radius

    rspo = rmajor

    !  First wall area: scales with minor radius

    awall = rminor + 0.5D0*(scrapli + scraplo)
    fwarea = sarea * awall/rminor
    if (ipowerflow == 0) then
       fwarea = (1.0D0-fhole) * fwarea
    else
       fwarea = (1.0D0-fhole-fdiv-fhcd) * fwarea
    end if

    if (iprint == 0) return

    !  Print out device build


    call oheadr(outfile,'Radial Build')

    call ovarre(outfile,'Avail. Space (m)','(available_radial_space)',available_radial_space)
    call ovarre(outfile,'Req. Space (m)','(required_radial_space)',required_radial_space)
    call ovarre(outfile,'f value: ','(f_avspace)',f_avspace)
   


    write(outfile,10)
   10  format(t43,'Thickness (m)',t60,'Radius (m)')

    radius = 0.0D0
    call obuild(outfile,'Device centreline',0.0D0,radius)

    drbild = bore + ohcth + gapoh
    radius = radius + drbild
    call obuild(outfile,'Machine bore',drbild,radius,'(bore)')
    call ovarre(mfile,'Machine bore (m)','(bore)',drbild)

    radius = radius + tfcth
    call obuild(outfile,'Coil inboard leg',tfcth,radius,'(tfcth)')
    call ovarre(mfile,'Coil inboard leg (m)','(deltf)',tfcth)

    radius = radius + gapds
    call obuild(outfile,'Gap',gapds,radius,'(gapds)')
    call ovarre(mfile,'Gap (m)','(gapds)',gapds)

    radius = radius + ddwi
    call obuild(outfile,'Vacuum vessel',ddwi,radius,'(ddwi)')
    call ovarre(mfile,'Vacuum vessel radial thickness (m)','(ddwi)',ddwi)

    radius = radius + shldith
    call obuild(outfile,'Inboard shield',shldith,radius,'(shldith)')
    call ovarre(mfile,'Inner radiation shield radial thickness (m)','(shldith)',shldith)

    radius = radius + blnkith
    call obuild(outfile,'Inboard blanket',blnkith,radius,'(blnkith)')
    call ovarre(mfile,'Inboard blanket radial thickness (m)','(blnkith)',blnkith)

    radius = radius + fwith
    call obuild(outfile,'Inboard first wall',fwith,radius,'(fwith)')
    call ovarre(mfile,'Inboard first wall radial thickness (m)','(fwith)',fwith)

    radius = radius + scrapli
    call obuild(outfile,'Inboard scrape-off',scrapli,radius,'(scrapli)')
    call ovarre(mfile,'Inboard scrape-off radial thickness (m)','(scrapli)',scrapli)

    radius = radius + rminor
    call obuild(outfile,'Plasma geometric centre',rminor,radius,'(rminor)')

    radius = radius + rminor
    call obuild(outfile,'Plasma outboard edge',rminor,radius,'(rminor)')

    radius = radius + scraplo
    call obuild(outfile,'Outboard scrape-off',scraplo,radius,'(scraplo)')
    call ovarre(mfile,'Outboard scrape-off radial thickness (m)','(scraplo)',scraplo)

    radius = radius + fwoth
    call obuild(outfile,'Outboard first wall',fwoth,radius,'(fwoth)')
    call ovarre(mfile,'Outboard first wall radial thickness (m)','(fwoth)',fwoth)

    radius = radius + blnkoth
    call obuild(outfile,'Outboard blanket',blnkoth,radius,'(blnkoth)')
    call ovarre(mfile,'Outboard blanket radial thickness (m)','(blnkoth)',blnkoth)

    radius = radius + shldoth
    call obuild(outfile,'Outboard shield',shldoth,radius,'(shldoth)')
    call ovarre(mfile,'Outer radiation shield radial thickness (m)','(shldoth)',shldoth)

    radius = radius + ddwi
    call obuild(outfile,'Vacuum vessel',ddwi,radius,'(ddwi)')

    radius = radius + gapsto
    call obuild(outfile,'Gap',gapsto,radius,'(gapsto)')
    call ovarre(mfile,'Gap (m)','(gapsto)',gapsto)

    radius = radius + tfthko
    call obuild(outfile,'Coil outboard leg',tfthko,radius,'(tfthko)')
    call ovarre(mfile,'Coil outboard leg radial thickness (m)','(tfthko)',tfthko)

  end subroutine stbild

  subroutine stphys

    !! Routine to calculate stellarator plasma physics information
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! None
    !! This routine calculates the physics quantities relevant to
    !! a stellarator device.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !! AEA FUS 172: Physics Assessment for the European Reactor Study
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    !  Local variables

    real(kind(1.0D0)) :: fusrat,pddpv,pdtpv,pdhe3pv,powht,sbar,sigvdt,zion

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Calculate plasma composition
    ! Issue #261 Remove old radiation model
    call plasma_composition

    !  Calculate density and temperature profile quantities

    call plasma_profiles


    !  Total field
    btot = sqrt(bt**2 + bp**2)

    !  Set beta as a consequence:
    !  This replaces constraint equation 1 as it is just an equaliity.
    beta = (betaft + betanb + 2.0D3*rmu0*echarge * (dene*ten + dnitot*tin)/btot**2 )


    q95 = q

    !  Calculate poloidal field using rotation transform
    bp = rminor * bt / rmajor * iotabar

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
    call radpwr(pbrempv,plinepv,psyncpv, pcoreradpv,pedgeradpv,pradpv)

    pcoreradmw = pcoreradpv*vol
    pedgeradmw = pedgeradpv*vol
    pradmw = pradpv*vol

    !  Heating power to plasma (= Psol in divertor model)
    !  Ohmic power is zero in a stellarator
    !  pradmw here is core + edge (no SOL)

    powht = falpha*palpmw + pchargemw + pohmmw - pradmw
    powht = max(0.001D0, powht) ! To avoid negative heating power.


    if (ignite == 0) powht = powht + pinjmw

    !  Power to divertor, = (1-f_rad)*Psol

    psolradmw = f_rad * powht
    pdivt = powht - psolradmw

    ! Add SOL Radiation to total

    pradmw = pradmw + psolradmw
    pradpv = pradmw / vol

    !  The following line is unphysical, but prevents -ve sqrt argument
    !  Should be obsolete if constraint eqn 17 is turned on (but beware -
    !  this may not be quite correct for stellarators)

    pdivt = max(0.001D0, pdivt)

    !  Power transported to the first wall by escaped alpha particles

    palpfwmw = palpmw * (1.0D0-falpha)

    !  Nominal mean photon wall load

    if (iwalld == 1) then
        photon_wall = ffwal * pradmw / sarea
    else
        if (ipowerflow == 0) then
            photon_wall = (1.0D0-fhole)*pradmw / fwarea
        else
            photon_wall = (1.0D0-fhole-fhcd-fdiv)*pradmw / fwarea
        end if
    end if

    peakradwallload = photon_wall * peakfactrad

    rad_fraction = pradmw / (falpha*palpmw+pchargemw+pohmmw+pinjmw)

    !  Calculate density limit

    call stdlim(bt,powht,rmajor,rminor,dnelimt)

    !  Calculate transport losses and energy confinement time using the
    !  chosen scaling law
    !  N.B. iotabar replaces tokamak q95 in argument list

    call pcond(afuel,palpmw,aspect,bt,dnitot,dene,dnla,eps,hfact, &
         iinvqd,isc,ignite,kappa,kappa95,kappaa,pchargemw,pinjmw, &
         plascur,pcoreradpv,rmajor,rminor,te,ten,tin,iotabar,qstar,vol, &
         xarea,zeff,ptrepv,ptripv,tauee,tauei,taueff,powerht)

    ptremw = ptrepv*vol
    ptrimw = ptripv*vol

    pscalingmw = ptremw + ptrimw

    !  Calculate auxiliary physics related information
    !  for the rest of the code

    sbar = 1.0D0
    call phyaux(aspect,dene,deni,fusionrate,alpharate,plascur,sbar,dnalp, &
         taueff,vol,burnup,dntau,figmer,fusrat,qfuel,rndfuel,taup)

    !  Calculate beta limit. Does nothing atm so commented out
    
    call stblim(betalim)


  end subroutine stphys

  subroutine stheat(outfile,iprint)

    !! Routine to calculate the auxiliary heating power
    !! in a stellarator
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates the auxiliary heating power for
    !! a stellarator device.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !! AEA FUS 172: Physics Assessment for the European Reactor Study
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
       etacd = etaech
       pinjwp = (pinjimw + pinjemw)/etacd

    case (2)  !  Lower Hybrid heating

       plhybd = pheat
       pinjimw = 0.0D0
       pinjemw = plhybd
       etacd = etalh
       pinjwp = (pinjimw + pinjemw)/etacd

    case (3)  !  Neutral beam injection heating

       !  Use routine described in AEA FUS 172, but discard the current
       !  drive efficiency as this is irrelevant for stellarators. We are
       !  only really interested in fpion, nbshinef and taubeam.

       call culnbi(effnbss,fpion,nbshinef)

       pnbeam = pheat * (1.0D0-forbitloss)
       porbitlossmw = pheat * forbitloss
       pinjimw = pnbeam * fpion
       pinjemw = pnbeam * (1.0D0-fpion)
       etacd = etanbi
       pinjwp = (pinjimw + pinjemw)/etacd

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

  subroutine stfwbs(outfile,iprint)
    !! Routine to calculate first wall, blanket and shield properties
    !! for a stellarator
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! outfile : input integer : Fortran output unit identifier
    !! iprint : input integer : Switch to write output to file (1=yes)
    !! This routine calculates a stellarator's first wall, blanket and
    !! shield properties.
    !! It calculates the nuclear heating in the blanket / shield, and
    !! estimates the volume and masses of the first wall,
    !! blanket, shield and vacuum vessel.
    !! <P>The arrays <CODE>coef(i,j)</CODE> and <CODE>decay(i,j)</CODE>
    !! are used for exponential decay approximations of the
    !! (superconducting) TF coil nuclear parameters.
    !! <UL><P><LI><CODE>j = 1</CODE> : stainless steel shield (assumed)
    !! <P><LI><CODE>j = 2</CODE> : tungsten shield (not used)</UL>
    !! Note: Costing and mass calculations elsewhere assume
    !! stainless steel only.
    !! <P>The method is the same as for tokamaks (as performed via
    !! <A HREF="fwbs.html">fwbs</A>), except for the volume calculations,
    !! which scale the surface area of the components from that
    !! of the plasma.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile, iprint

    !  Local variables

    real(kind(1.0D0)) :: adewex,bfwi,bfwo,coilhtmx,coolvol,decaybl,decaybzi, &
         decaybzo,decayfwi,decayfwo,decayshldi,decayshldo,dpacop,htheci, &
         pheci,pheco,pneut2,pnucbsi,pnucbso,pnucbzi,pnucbzo,pnucfwbs, &
         pnucfwbsi,pnucfwbso,pnucfwi,pnucfwo,pnucshldi,pnucshldo,pnucsi, &
         pnucso,psurffwi,psurffwo,ptfiwp,ptfowp,r1,raddose,vffwi,vffwo, &
         volshldi,volshldo

    logical :: first_call = .true.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  First wall full-power lifetime (years)
    !  May be recalculated below if ipowerflow=1 and secondary_cycle>0,
    !  and also by the availability model

    fwlife = min(abktflnc/wallmw, tlife)

    !  First wall inboard, outboard areas (assume 50% of total each)

    fwareaib = 0.5D0*fwarea
    fwareaob = 0.5D0*fwarea

    !  Blanket volume; assume that its surface area is scaled directly from the
    !  plasma surface area.
    !  Uses fhole etc. to take account of gaps due to ports etc.

    r1 = rminor + 0.5D0*(scrapli+fwith + scraplo+fwoth)
    if (ipowerflow == 0) then
       blarea = sarea * r1/rminor * (1.0D0-fhole)
    else
       blarea = sarea * r1/rminor * (1.0D0-fhole-fdiv-fhcd)
    end if
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

    !  Neutron power lost through holes in first wall (eventually absorbed by
    !  shield)

    pnucloss = pneutmw * fhole

    !  Blanket neutronics calculations

    if (blktmodel == 1) then

       call blanket_neutronics

       if (ipowerflow == 1) then
          pnucdiv = pneutmw * fdiv
          pnuchcd = pneutmw * fhcd
          pnucfw = pneutmw - pnucdiv - pnucloss - pnuchcd

          pradloss = pradmw * fhole
          praddiv = pradmw * fdiv
          pradhcd = pradmw * fhcd
          pradfw = pradmw - praddiv - pradloss - pradhcd

          htpmw_fw = fpumpfw * (pnucfw + pradfw + porbitlossmw)
          htpmw_blkt = fpumpblkt * pnucblkt
          htpmw_shld = fpumpshld * pnucshld
          htpmw_div = fpumpdiv * (pdivt + pnucdiv + praddiv)

          !  Void fraction in first wall / breeding zone,
          !  for use in fwmass and coolvol calculation below

          vffwi = 1.0D0 - fblbe - fblbreed - fblss
          vffwo = vffwi
       end if

    else

       pnuccp = 0.0D0

       if (ipowerflow == 0) then

          !  Energy-multiplied neutron power

          pneut2 = (pneutmw - pnucloss - pnuccp) * emult

          emultmw = pneut2 - (pneutmw - pnucloss - pnuccp)

          !  Nuclear heating in the blanket

          decaybl = 0.075D0 / (1.0D0 - vfblkt - fblli2o - fblbe)

          pnucblkt = pneut2 * (1.0D0 - exp(-blnkoth/decaybl))

          !  Nuclear heating in the shield
          pnucshld = pneut2 - pnucblkt

          !  Superconducting coil shielding calculations
          call sctfcoil_nuclear_heating_iter90(coilhtmx,dpacop,htheci,nflutf, &
               pheci,pheco,ptfiwp,ptfowp,raddose,ptfnuc)

       else  !  ipowerflow == 1

          !  Neutron power incident on divertor (MW)

          pnucdiv = pneutmw * fdiv

          !  Neutron power incident on HCD apparatus (MW)

          pnuchcd = pneutmw * fhcd

          !  Neutron power deposited in first wall, blanket and shield (MW)

          pnucfwbs = pneutmw - pnucdiv - pnucloss - pnuccp - pnuchcd

          !  Split between inboard and outboard by first wall area fractions

          pnucfwbsi = pnucfwbs * fwareaib/fwarea
          pnucfwbso = pnucfwbs * fwareaob/fwarea

          !  Radiation power incident on divertor (MW)

          praddiv = pradmw * fdiv

          !  Radiation power incident on HCD apparatus (MW)

          pradhcd = pradmw * fhcd

          !  Radiation power lost through holes (eventually hits shield) (MW)

          pradloss = pradmw * fhole

          !  Radiation power incident on first wall (MW)

          pradfw = pradmw - praddiv - pradloss - pradhcd

          !  Calculate the power deposited in the first wall, blanket and shield,
          !  and the required coolant pumping power

          !  If we have chosen pressurised water as the coolant, set the
          !  coolant outlet temperature as 20 deg C below the boiling point

          if (coolwh == 2) then
             outlet_temp = tsat(1.0D-6*coolp) - 20.0D0  !  in K
          end if

          bfwi = 0.5D0*fwith
          bfwo = 0.5D0*fwoth

          vffwi = afwi*afwi/(bfwi*bfwi)  !  inboard FW coolant void fraction
          vffwo = afwo*afwo/(bfwo*bfwo)  !  outboard FW coolant void fraction

          !  First wall decay length (m) - improved calculation required

          decayfwi = declfw
          decayfwo = declfw

          !  Surface heat flux on first wall (MW) (sum = pradfw)

          psurffwi = pradfw * fwareaib/fwarea
          psurffwo = pradfw * fwareaob/fwarea

          !  Simple blanket model (primary_pumping = 0 or 1) is assumed for stellarators

          !  The power deposited in the first wall, breeder zone and shield is
          !  calculated according to their dimensions and materials assuming
          !  an exponential attenuation of nuclear heating with increasing
          !  radial distance.  The pumping power for the coolant is calculated
          !  as a fraction of the total thermal power deposited in the
          !  coolant.

          pnucfwi = pnucfwbsi * (1.0D0 - exp(-2.0D0*bfwi/decayfwi))
          pnucfwo = pnucfwbso * (1.0D0 - exp(-2.0D0*bfwo/decayfwo))

          !  Neutron power reaching blanket and shield (MW)

          pnucbsi = pnucfwbsi - pnucfwi
          pnucbso = pnucfwbso - pnucfwo

          !  Blanket decay length (m) - improved calculation required

          decaybzi = declblkt
          decaybzo = declblkt

          !  Neutron power deposited in breeder zone (MW)

          pnucbzi = pnucbsi * (1.0D0 - exp(-blnkith/decaybzi))
          pnucbzo = pnucbso * (1.0D0 - exp(-blnkoth/decaybzo))

          !  Calculate coolant pumping powers from input fraction.
          !  The pumping power is assumed to be a fraction, fpump, of the
          !  incident thermal power to each component so that
          !  htpmw_i = fpump_i*C, where C is the non-pumping thermal power
          !  deposited in the coolant

          !  First wall and Blanket pumping power (MW)

          if (primary_pumping==0) then
          !    Use input
          else if (primary_pumping==1) then
              htpmw_fw = fpumpfw * (pnucfwi + pnucfwo + psurffwi + psurffwo + porbitlossmw)
              htpmw_blkt = fpumpblkt * (pnucbzi*emult + pnucbzo*emult)
          else
              call report_error(215)
          endif

          emultmw = fpumpblkt * (pnucbzi*emult + pnucbzo) * (emult - 1.0D0)

          !  Total nuclear heating of first wall (MW)

          pnucfw = pnucfwi + pnucfwo

          !  Total nuclear heating of blanket (MW)

          pnucblkt = (pnucbzi + pnucbzo)*emult

          emultmw = emultmw + (pnucbzi + pnucbzo) * (emult - 1.0D0)

          !  Calculation of shield and divertor powers
          !  Shield and divertor powers and pumping powers are calculated using the same
          !  simplified method as the first wall and breeder zone when primary_pumping = 1.
          !  i.e. the pumping power is a fraction of the total thermal power deposited in the
          !  coolant.

          !  Neutron power reaching the shield (MW)
          !  The power lost from the fhole area fraction is assumed to be incident upon the shield

          pnucsi = pnucbsi - pnucbzi + (pnucloss + pradloss) * fwareaib/fwarea
          pnucso = pnucbso - pnucbzo + (pnucloss + pradloss) * fwareaob/fwarea

          !  Improved calculation of shield power decay lengths required

          decayshldi = declshld
          decayshldo = declshld

          !  Neutron power deposited in the shield (MW)

          pnucshldi = pnucsi * (1.0D0 - exp(-shldith/decayshldi))
          pnucshldo = pnucso * (1.0D0 - exp(-shldoth/decayshldo))

          pnucshld = pnucshldi + pnucshldo

          !  Calculate coolant pumping powers from input fraction.
          !  The pumping power is assumed to be a fraction, fpump, of the incident
          !  thermal power to each component so that,
          !     htpmw_i = fpump_i*C
          !  where C is the non-pumping thermal power deposited in the coolant

          if (primary_pumping==0) then
              !    Use input
          else if (primary_pumping==1) then

              !  Shield pumping power (MW)
              htpmw_shld = fpumpshld*(pnucshldi + pnucshldo)

              !  Divertor pumping power (MW)
              htpmw_div = fpumpdiv*(pdivt + pnucdiv + praddiv)

          end if

          !  Remaining neutron power to coils and elsewhere. This is assumed
          !  (for superconducting coils at least) to be absorbed by the
          !  coils, and so contributes to the cryogenic load

          if (i_tf_sup == 1) then
             ptfnuc = pnucsi + pnucso - pnucshldi - pnucshldo
          else  !  resistive coils
             ptfnuc = 0.0D0
          end if

       end if  !  ipowerflow

    end if  !  blktmodel

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

    coolvol = divsur * divclfr * divplt

    !  Blanket mass, excluding coolant

    if (blktmodel == 0) then
       if ((blkttype == 1).or.(blkttype == 2)) then  !  liquid breeder (WCLL or HCLL)
          wtbllipb = volblkt * fbllipb * 9400.0D0
          whtblli = volblkt * fblli * 534.0D0
          whtblkt = wtbllipb + whtblli
       else  !  solid breeder (HCPB); always for ipowerflow=0
          wtblli2o = volblkt * fblli2o * 2010.0D0
          whtblbe = volblkt * fblbe * 1850.0D0
          whtblkt = wtblli2o + whtblbe
       end if
       whtblss = volblkt * denstl * fblss
       whtblvd = volblkt * 5870.0D0  * fblvd

       whtblkt = whtblkt + whtblss + whtblvd

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

    !  When blktmodel > 0, although the blanket is by definition helium-cooled
    !  in this case, the shield etc. are assumed to be water-cooled, and since
    !  water is heavier the calculation for coolmass is better done with
    !  coolwh=2 if blktmodel > 0; thus we can ignore the helium coolant mass
    !  in the blanket.

    if (blktmodel == 0) then
       coolvol = coolvol + volblkt*vfblkt
    end if

    !  Shield mass

    whtshld = volshld * denstl * (1.0D0 - vfshld)
    coolvol = coolvol + volshld*vfshld

    !  Penetration shield (set = internal shield)

    wpenshld = whtshld

    if (ipowerflow == 0) then

       !  First wall mass
       !  (first wall area is calculated elsewhere)

       fwmass = fwarea * (fwith+fwoth)/2.0D0 * denstl * (1.0D0-fwclfr)

       !  Surface areas adjacent to plasma

       coolvol = coolvol + fwarea * (fwith+fwoth)/2.0D0 * fwclfr

    else

       fwmass = denstl * &
            (fwareaib*fwith*(1.0D0-vffwi) + fwareaob*fwoth*(1.0D0-vffwo))
       coolvol = coolvol + fwareaib*fwith*vffwi + fwareaob*fwoth*vffwo

       !  Average first wall coolant fraction, only used by old routines
       !  in fispact.f90, safety.f90

       fwclfr = (fwareaib*fwith*vffwi + fwareaob*fwoth*vffwo) / &
            (fwarea*0.5D0*(fwith+fwoth))

    end if

    !  Mass of coolant = volume * density at typical coolant
    !  temperatures and pressures
    !  N.B. for blktmodel > 0, mass of *water* coolant in the non-blanket
    !  structures is used (see comment above)

    if ((blktmodel > 0).or.(coolwh == 2)) then  !  pressurised water coolant
       coolmass = coolvol*806.719D0
    else  !  gaseous helium coolant
       coolmass = coolvol*1.517D0
    end if

    !  Assume external cryostat is a torus with circular cross-section,
    !  centred on plasma major radius.
    !  N.B. No check made to see if coils etc. lie wholly within cryostat...

    !  External cryostat outboard major radius (m)

    rdewex = r_tf_outboard_mid + 0.5D0*tfthko + rpf2dewar
    adewex = rdewex-rmajor

    !  External cryostat volume

    vdewex = 4.0D0*pi*pi*rmajor*adewex * ddwex

    !  Internal vacuum vessel volume
    !  fvoldw accounts for ports, support, etc. additions

    r1 = rminor + 0.5D0*(scrapli+fwith+blnkith+shldith &
         + scraplo+fwoth+blnkoth+shldoth)
    vdewin = ddwi * sarea * r1/rminor * fvoldw

    !  Vacuum vessel mass

    vvmass = vdewin * denstl

    !  Sum of internal vacuum vessel and external cryostat masses

    dewmkg = (vdewin + vdewex) * denstl

    if (iprint == 0) return

    !  Output section

    call oheadr(outfile,'First Wall / Blanket / Shield')
    call ovarre(outfile,'Average neutron wall load (MW/m2)','(wallmw)', wallmw)
    if (blktmodel > 0) then
       call ovarre(outfile,'Neutron wall load peaking factor','(wallpf)', wallpf)
    end if
    call ovarre(outfile,'First wall full-power lifetime (years)', &
         '(fwlife)',fwlife)

    call ovarre(outfile,'Inboard shield thickness (m)','(shldith)',shldith)
    call ovarre(outfile,'Outboard shield thickness (m)','(shldoth)',shldoth)
    call ovarre(outfile,'Top shield thickness (m)','(shldtth)',shldtth)

    if (blktmodel > 0) then
       call ovarre(outfile,'Inboard breeding zone thickness (m)', &
            '(blbuith)', blbuith)
       call ovarre(outfile,'Inboard box manifold thickness (m)', &
            '(blbmith)', blbmith)
       call ovarre(outfile,'Inboard back plate thickness (m)', &
            '(blbpith)', blbpith)
    end if
    call ovarre(outfile,'Inboard blanket thickness (m)','(blnkith)', blnkith)
    if (blktmodel > 0) then
       call ovarre(outfile,'Outboard breeding zone thickness (m)', &
            '(blbuoth)', blbuoth)
       call ovarre(outfile,'Outboard box manifold thickness (m)', &
            '(blbmoth)', blbmoth)
       call ovarre(outfile,'Outboard back plate thickness (m)', &
            '(blbpoth)', blbpoth)
    end if
    call ovarre(outfile,'Outboard blanket thickness (m)','(blnkoth)', blnkoth)
    call ovarre(outfile,'Top blanket thickness (m)','(blnktth)',blnktth)

    if ((ipowerflow == 0).and.(blktmodel == 0)) then
       call osubhd(outfile,'Coil nuclear parameters :')
       call ovarre(outfile,'Peak magnet heating (MW/m3)','(coilhtmx)', &
            coilhtmx)
       call ovarre(outfile,'Inboard coil winding pack heating (MW)', &
            '(ptfiwp)',ptfiwp)
       call ovarre(outfile,'Outboard coil winding pack heating (MW)', &
            '(ptfowp)',ptfowp)
       call ovarre(outfile,'Peak coil case heating (MW/m3)','(htheci)', &
            htheci)
       call ovarre(outfile,'Inboard coil case heating (MW)','(pheci)',pheci)
       call ovarre(outfile,'Outboard coil case heating (MW)','(pheco)',pheco)
       call ovarre(outfile,'Insulator dose (rad)','(raddose)',raddose)
       call ovarre(outfile,'Maximum neutron fluence (n/m2)','(nflutf)', &
            nflutf)
       call ovarre(outfile,'Copper stabiliser displacements/atom', &
            '(dpacop)',dpacop)
    end if

    if (blktmodel == 0) then
       call osubhd(outfile,'Nuclear heating :')
       call ovarre(outfile, &
            'Blanket heating (including energy multiplication) (MW)', &
            '(pnucblkt)',pnucblkt)
       call ovarre(outfile,'Shield nuclear heating (MW)', &
            '(pnucshld)',pnucshld)
       call ovarre(outfile,'Coil nuclear heating (MW)', &
            '(ptfnuc)',ptfnuc)
    else
       call osubhd(outfile,'Blanket neutronics :')
       call ovarre(outfile, &
            'Blanket heating (including energy multiplication) (MW)', &
            '(pnucblkt)',pnucblkt)
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
       call ovarre(outfile,'Nuclear heating on i/b coil (MW/m3)','(pnuctfi)',pnuctfi)
       call ovarre(outfile,'Nuclear heating on o/b coil (MW/m3)','(pnuctfo)',pnuctfo)
       call ovarre(outfile,'Total nuclear heating on coil (MW)','(ptfnuc)',ptfnuc)
       call ovarre(outfile,'Fast neut. fluence on i/b coil (n/m2)', &
            '(nflutfi)',nflutfi*1.0D4)
       call ovarre(outfile,'Fast neut. fluence on o/b coil (n/m2)', &
            '(nflutfo)',nflutfo*1.0D4)
       call ovarre(outfile,'Minimum final He conc. in IB VV (appm)','(vvhemini)',vvhemini)
       call ovarre(outfile,'Minimum final He conc. in OB VV (appm)','(vvhemino)',vvhemino)
       call ovarre(outfile,'Maximum final He conc. in IB VV (appm)','(vvhemaxi)',vvhemaxi)
       call ovarre(outfile,'Maximum final He conc. in OB VV (appm)','(vvhemaxo)',vvhemaxo)
       call ovarre(outfile,'Blanket lifetime (full power years)','(bktlife)',bktlife)
       call ovarre(outfile,'Blanket lifetime (calendar years)','(t_bl_y)',t_bl_y)
    end if

    if ((ipowerflow == 1).and.(blktmodel == 0)) then
       call oblnkl(outfile)
       call ovarin(outfile, &
            'First wall / blanket thermodynamic model','(secondary_cycle)',secondary_cycle)
       if (secondary_cycle == 0) then
          call ocmmnt(outfile,'   (Simple calculation)')
       end if
    end if

    call osubhd(outfile,'Blanket / shield volumes and weights :')

    if (blktmodel == 0) then
       if ((blkttype == 1).or.(blkttype == 2)) then
          write(outfile,601) volblkti, volblkto, volblkt,  &
               whtblkt, vfblkt, fbllipb, wtbllipb, fblli, whtblli,  &
               fblss, whtblss, fblvd, whtblvd, volshldi, volshldo,  &
               volshld, whtshld, vfshld, wpenshld
       else  !  (also if ipowerflow=0)
          write(outfile,600) volblkti, volblkto, volblkt,  &
               whtblkt, vfblkt, fblbe, whtblbe, fblli2o, wtblli2o,  &
               fblss, whtblss, fblvd, whtblvd, volshldi, volshldo,  &
               volshld, whtshld, vfshld, wpenshld
       end if
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
    call ovarre(outfile,'External cryostat mass (kg)','',dewmkg-vvmass)
    call ovarre(outfile,'Internal vacuum vessel shell volume (m3)','(vdewin)',vdewin)
    call ovarre(outfile,'Vacuum vessel mass (kg)','(vvmass)',vvmass)
    call ovarre(outfile,'Total cryostat + vacuum vessel mass (kg)','(dewmkg)',dewmkg)
    call ovarre(outfile,'Divertor area (m2)','(divsur)',divsur)
    call ovarre(outfile,'Divertor mass (kg)','(divmas)',divmas)

  end subroutine stfwbs

  subroutine stdlim(bt,powht,rmajor,rminor,dlimit)

    !! Routine to calculate the density limit in a stellarator
    !! author: P J Knight, CCFE, Culham Science Centre
    !! bt     : input real : Toroidal field on axis (T)
    !! powht  : input real : Absorbed heating power (MW)
    !! rmajor : input real : Plasma major radius (m)
    !! rminor : input real : Plasma minor radius (m)
    !! dlimit : output real : Maximum volume-averaged plasma density (/m3)
    !! This routine calculates the density limit for a stellarator.
    !! S.Sudo, Y.Takeiri, H.Zushi et al., Scalings of Energy Confinement
    !! and Density Limit in Stellarator/Heliotron Devices, Nuclear Fusion
    !! vol.30, 11 (1990).
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(in) :: bt,powht,rmajor,rminor
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

  subroutine stblim(betamx)

    !! Routine to calculate the beta limit in a stellarator
    !! author: P J Knight, CCFE, Culham Science Centre
    !! betamx : output real : Maximum volume-averaged plasma beta
    !! This routine calculates the beta limit for a stellarator.
    !! J.F.Lyon, K.Gulec, R.L.Miller and L.El-Guebaly, Status of the U.S.
    !! Stellarator Reactor Study
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    real(kind(1.0D0)), intent(out) :: betamx

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    betamx = 0.05D0

  end subroutine stblim

  subroutine stigma(outfile)

    !! Routine to calculate ignition margin at the final point
    !! with different stellarator confinement time scaling laws
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! This routine calculates the ignition margin at the final
    !! point with different stellarator confinement time scaling laws
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    real(kind(1.0D0)) :: d2,powerhtz,ptrez,ptriz,taueez, &
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
            plascur,pcoreradpv,rmajor,rminor,te,ten,tin, &
            iotabar,qstar,vol,xarea,zeff,ptrez,ptriz,taueez,taueiz, &
            taueffz,powerhtz)

       hfac(iisc) = fhfac(i)
       write(outfile,30) tauscl(istlaw(iisc)),taueez,hfac(iisc)
    end do
   30  format(t2,a24,t34,f7.3,t58,f7.3)

  end subroutine stigma

  subroutine stout(outfile)

    !! Routine to print out the final stellarator machine parameters
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! outfile : input integer : output file unit
    !! This routine prints out the stellarator's parameters at the
    !! end of a run.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Some on screen printouts for consistency checks:
    print *,"Used stellarator configuration: ", config%name
    print *,"Deviation from reference point"
    print *,"aspect ratio",aspect/config%aspect_ref
    print *,"major radius",rmajor/config%rmajor_ref
    print *,"n_tf (should be 1)", n_tf/(config%coilspermodule*config%symmetry)

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

  subroutine ststrc(outfile,iprint)

    !! Routine to calculate the structural masses for a stellarator
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates the structural masses for a stellarator.
    !! This is the stellarator version of routine
    !! <A HREF="struct.html">STRUCT</A>. In practice, many of the masses
    !! are simply set to zero to avoid double-counting of structural
    !! components that are specified differently for tokamaks.
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables
    real(kind(1.0D0)) :: intercoil_surface, M_intercoil, M_struc, msupstr
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Tokamak-specific PF coil fence mass set to zero
    fncmass = 0.0D0

    !  Reactor core gravity support mass
    gsmass = 0.0D0 !? Not sure about this.


    !!!!!!!!! This is the previous scaling law for intercoil structure
    !!!!!!!!! We keep is here as a reference to the new model, which
    !!!!!!!!! we do not really trust yet.
    !  Mass of support structure (includes casing) (tonnes)
    !  Scaling for required structure mass (Steel) from:
    !  F.C. Moon, J. Appl. Phys. 53(12) (1982) 9112
    !
    !  Values based on regression analysis by Greifswald, March 2014
    M_struc = 1.3483D0 * (1000.0D0 * estotftgj)**0.7821D0
    msupstr = 1000.0D0*M_struc  !  kg


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Intercoil suppeort structure calculation:
    ! Calculate the intercoil bolted plates structure from the coil surface
    ! which needs to be precalculated (or calculated in PROCESS but this not done here)
    ! The coil width is substracted from that:
    !total_coil_width = b + 2* d_ic + 2* case_thickness_constant
    !total_coil_thickness = h + 2* d_ic + 2* case_thickness_constant
    !
    intercoil_surface = config%coilsurface * f_a*f_r &
                         - tftort * config%coillength* f_r/f_a * f_N 


    ! This 0.18 m is an effective thickness which is scaled with empirial 1.5 law. 5.6 T is reference point of Helias
    ! The thickness 0.18 was obtained as a measured value from Schauer, F. and Bykov, V. design of Helias 5-B. (nucl Fus. 2013)
    aintmass = 0.18D0 *f_B**1.5 * intercoil_surface * denstl 
    
    clgsmass = 0.2D0*aintmass    ! Very simple approximation for the gravity support.
                                 ! This fits for the Helias 5b reactor design point ( F. and Bykov, V. design of Helias 5-B. (nucl Fus. 2013)).


    !  Total mass of cooled components
    coldmass = whttf + aintmass + dewmkg

    !  Output section

    if (iprint == 0) return

    call oheadr(outfile,'Support Structure')
    call ovarre(outfile,'Intercoil support structure mass (from intercoil calculation) (kg)', &
         '(aintmass)',aintmass)
   call ovarre(outfile,'Intercoil support structure mass (scaling, for comparison) (kg)', &
         '(aintmass)',msupstr)
    call ovarre(outfile,'Gravity support structure mass (kg)', &
         '(clgsmass)',clgsmass)
    call ovarre(outfile,'Mass of cooled components (kg)', &
         '(coldmass)',coldmass)

  end subroutine ststrc

  subroutine stdiv(outfile,iprint)

    !! Routine to call the stellarator divertor model
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calls the divertor model for a stellarator,
    !! developed by Felix Warmer.
    !! Stellarator Divertor Model for the Systems
    !! Code PROCESS, F. Warmer, 21/06/2013
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

    call ovarre(outfile,'Power to divertor (MW)','(pdivt.)',pdivt)
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

  subroutine stcoil(outfile,iprint)

    !! Routine that performs the calculations for stellarator coils
    !! author: J Lion, IPP Greifswald
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates the properties of the coils for
    !! a stellarator device.
    !! <P>Some precalculated effective parameters for a stellarator power
    !! plant design are used as the basis for the calculations. The coils
    !! are assumed to be a fixed shape, but are scaled in size
    !! appropriately for the machine being modelled. 
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use superconductors, only : jcrit_rebco, jcrit_nbti, bi2212, itersc, wstsc
    use rebco_variables, only: copperA_m2, copperA_m2_max
    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint


    real(kind(1.0D0)) :: r_coil_major, r_coil_minor, case_thickness_constant, coilcurrent

    real(kind(1.0D0)), allocatable, dimension(:) ::   jcrit_vector,RHS,LHS,awp, B_max_k

    real(kind(1.0D0)) :: ap,vd,f_scu, awp_min, awpc, awp_tor, awp_rad, &
                           b_vert_max, awptf, r_tf_inleg_mid, radvv, tf_total_h_width, &
                           tfborev

    integer :: N_it,k

    !  Local variables

    ! Sets major and minor coil radius (important for machine scalings) 
   
    r_coil_major = config%coil_to_plasma_ratio * rmajor
    r_coil_minor = config%coil_epsilon * r_coil_major * config%aspect_ref/aspect ! This aspect scaling is only valid close to the intended aspect ratio.

    ! Coil case thickness (m). Here assumed to be constant 
    ! until something better comes up.
    case_thickness_constant = 0.12D0 ! !? Leave this constant for now... Check this!!


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Winding Pack Geometry: for one conductor
   !
     ! This one conductor will just be multiplied later to fit the winding pack size.
     !       
     ! [m] Dimension of square cable space inside insulation
     !     and case of the conduit of each turn
     leni = leno - 2.0D0 * (thwcndut + thicndut)  !  leni = t_w
     if(leni<0) print *, "leni is negative. Check leno, thwcndut and thicndut."
     ! [m^2] Cross-sectional area of cable space per turn
     acstf = 0.9D0 * leni**2 ! 0.9 to include some rounded corners. (acstf = pi (leni/2)**2 = pi/4 *leni**2 for perfect round conductor). This factor depends on how round the corners are.
     ! [m^2] Cross-sectional area of conduit case per turn
     acndttf = (leni + 2.0D0*thwcndut)**2 - acstf
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Winding Pack total size:
   !
     ! Total coil current (MA)
     coilcurrent = bt * config%I0 * r_coil_major /f_N
 
     N_it = 200     ! number of iterations
 
     allocate(RHS(n_it),LHS(n_it))
     allocate(jcrit_vector(n_it),Awp(n_it),b_max_k(n_it))
 
     do k = 1,N_it
 
       ! Sample coil winding pack
       Awp(k) = (r_coil_minor/100.0D0 + (dble(k)-1) / (dble(N_it)-1) * (r_coil_minor/1.0D0-r_coil_minor/100.0D0))**2
       if (isumattf==6) Awp(k) =(r_coil_minor/150.0D0 + (dble(k)-1) / (dble(N_it)-1) * (r_coil_minor/1.0D0-r_coil_minor/150.0D0))**2
 
       !  B-field calculation
       B_max_k(k) = bmax_from_awp(Awp(k)/r_coil_major**2,coilcurrent)
 
       ! jcrit for this bmax:
       jcrit_vector(k) = jcrit_frommaterial(B_max_k(k),tftmp+1.5) ! Get here a temperature margin of 1.5K.
 
     end do
 
     ! The operation current density weighted with the global iop/icrit fraction
     LHS = fiooic * jcrit_vector 
     
     ! Conduct fraction of conduit * Superconductor fraction in conductor
     f_scu =   (acstf*(1.0D0-vftf))/(leno**2)*(1.0D0-fcutfsu) !fraction that is SC of wp.
 
     RHS = coilcurrent/(Awp(:)*f_scu) ! f_scu should be the fraction of the sc that is in the winding pack.
 
     Awp_min = (r_coil_minor/20.0D0)**2 ! Initial guess for intersection routine
     if (isumattf==6) Awp_min = (r_coil_minor/100.0D0)**2 ! If REBCO, then start at smaller winding pack ratios
 
     ! Find the intersection between LHS and RHS (or: how much awp do I need to get to the desired coil current)
     call intersect(Awp,LHS,N_it,Awp,RHS,N_it,Awp_min)
 
     ! Maximum field at superconductor surface (T)
     Awp_min = Max(leno**2,Awp_min)
 
     ! Recalculate bmaxtf at the found awp_min:
     bmaxtf = bmax_from_awp(Awp_min/r_coil_major**2,coilcurrent)
 
     ! Winding pack toroidal, radial cross-sections (m)
     awp_tor = sqrt(awp_min) / sqrt(config%WP_ratio) ! Toroidal dimension
     awp_rad = sqrt(awp_min) * sqrt(config%WP_ratio) ! Radial dimension
 
     wwp1 = awp_tor                ! [m] toroidal thickness of winding pack
     wwp2 = awp_tor                ! [m] toroidal thickness of winding pack (region in front)
     thkwp = awp_rad               ! [m] radial thickness of winding pack
 
     !  [m^2] winding-pack cross sectional area including insulation (not global)
     awpc = (thkwp + 2.0D0*tinstf)*(wwp1 + 2.0D0*tinstf)
 

     awptf = awp_tor*awp_rad                 ! [m^2] winding-pack cross sectional area
     jwptf = coilcurrent*1.0D6/awptf         ! [A/m^2] winding pack current density
     turnstf = awptf / (leno**2)             !  estimated number of turns for a given turn size (not global). Take at least 1.
     cpttf = coilcurrent*1.0D6 / turnstf     ! [A] current per turn - estimation
     ! [m^2] Total conductor cross-sectional area, taking account of void area
     acond = acstf*turnstf * (1.0D0-vftf)
     ! [m^2] Void area in cable, for He
     avwp = acstf*turnstf*vftf
     ! [m^2] Insulation area (not including ground-wall)
     aiwp = turnstf * (leno**2 - acndttf - acstf)
     ! [m^2] Structure area for cable
     aswp = turnstf*acndttf
   ! End of winding pack calculations
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !  Casing calculations
   !  
    !  For now assumed to be constant in a bolted plate model.
    ! 
    casthi = case_thickness_constant/2.0D0 ! [m] coil case thickness outboard distance (radial)
    thkcas = case_thickness_constant/2.0D0 ! [m] coil case thickness inboard distance  (radial).
    casths = case_thickness_constant/2.0D0 ! [m] coil case thickness toroidal distance (toroidal)
 
   ! End of casing calculations
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !  Port calculations
   !
     !  Maximal toroidal port size (vertical ports) (m)
     !  The maximal distance is correct but the vertical extension of this port is not clear!
     !  This is simplified for now and can be made more accurate in the future!
     vporttmax = 0.4D0 * config%max_portsize_width /f_n  ! This is not accurate yet. Needs more insight!
  
     !  Maximal poloidal port size (vertical ports) (m)
     vportpmax = 2.0* vporttmax ! Simple approximation
  
     !  Maximal vertical port clearance area (m2)
     vportamax = vporttmax*vportpmax
  
     !  Horizontal ports
     !  Maximal toroidal port size (horizontal ports) (m)
     hporttmax =  0.8D0 * config%max_portsize_width /f_n ! Factor 0.8 to take the variation with height into account
  
     !  Maximal poloidal port size (horizontal ports) (m)
     hportpmax = 2.0D0 * hporttmax ! Simple approximation
  
     !  Maximal horizontal port clearance area (m2)
     hportamax = hporttmax*hportpmax
   ! End of port calculations
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !  General Coil Geometry values
   !  
     tftort = wwp1 + 2.0D0*casths+ 2.0D0*tinstf     ! [m] Thickness of inboard leg in toroidal direction
     tfcth = thkcas + thkwp + casthi+ 2.0D0*tinstf  ! [m] Thickness of inboard leg in radial direction
     tfthko = thkcas + thkwp + casthi+ 2.0D0*tinstf ! [m] Thickness of outboard leg in radial direction (same as inboard)
     arealeg = tfcth*tftort                         ! [m^2] overall coil cross-sectional area (assuming inboard and 
                                                    !       outboard leg are the same)
     acasetf = (tfcth*tftort)-awpc                  ! [m^2] Cross-sectional area of surrounding case
     
     tfocrn = 0.5D0*tftort                          ! [m] Half-width of side of coil nearest torus centreline
     tficrn = 0.5D0*tftort                          ! [m] Half-width of side of coil nearest plasma
  
     ! [m^2] Total surface area of coil side facing plasma: inboard region
     tfsai = n_tf*tftort * 0.5D0*tfleng
     ! [m^2] Total surface area of coil side facing plasma: outboard region
     tfsao = tfsai  !  depends, how 'inboard' and 'outboard' are defined
  
     ! [m] Minimal distance in toroidal direction between two stellarator coils (from mid to mid)
     ! Consistency with coil width is checked in constraint equation 82
     toroidalgap = config%dmin * rmajor / config%rmajor_ref 
  
     !  Variables for ALL coils.
     tfareain = n_tf*arealeg                              ! [m^2] Total area of all coil legs (midplane)
     ritfc = n_tf * coilcurrent * 1.0D6                   ! [A] Total current in ALL coils
     oacdcp = ritfc/tfareain                              ! [A / m^2] overall current density
     rbmax = r_coil_major-r_coil_minor+awp_rad            ! [m] radius of peak field occurrence, average
                                                          ! jlion: not sure what this will be used for. Not very
                                                          ! useful for stellarators
 
 
     estotftgj = 0.5D0 * (config%inductivity*rmajor/aspect**2)&
                   * (ritfc/n_tf)**2 * 1.0D-9             ! [GJ] Total magnetic energy
  
     !  Coil dimensions
     hmax = 0.5D0 * config%maximal_coil_height *f_r/f_a   ! [m] maximum half-height of coil
     r_tf_inleg_mid =  r_coil_major-r_coil_minor          ! This is not very well defined for a stellarator.
                                                          ! Though, this is taken as an average value.
     tf_total_h_width = r_coil_minor                      !? not really sure what this is supposed to be. Estimated as
                                                          ! the average minor coil radius
     
     
     tfborev = 2.0D0*hmax                   ! [m] estimated vertical coil bore
     
     
     tfleng = config%coillength/n_tf                      ! [m] estimated average length of a coil
 
     ! [m^2] Total surface area of toroidal shells covering coils
     tfcryoarea = config%coilsurface * f_r**2 / f_a *1.1D0 !1.1 to scale it out a bit. Should be coupled to winding pack maybe.
 
 
   ! End of general coil geometry values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !  Masses of conductor constituents
   !
    ! [kg] Mass of case
    !  (no need for correction factors as is the case for tokamaks)
    ! This is only correct if the winding pack is 'thin' (tfleng>>sqrt(acasetf)).
    whtcas = tfleng * acasetf * dcase
     ! [kg] mass of Superconductor
    whtconsc = (tfleng * turnstf * acstf*(1.0D0-vftf) * (1.0D0-fcutfsu) - tfleng*awphec) &
               *dcond(isumattf)
      ! [kg] mass of Copper in conductor
    whtconcu =  (tfleng * turnstf * acstf*(1.0D0-vftf) * fcutfsu - tfleng*awphec) * dcopper
      ! [kg] mass of Steel conduit (sheath)
    whtconsh = tfleng*turnstf*acndttf * denstl
    !if (isumattf==6)   whtconsh = fcondsteel * awptf *tfleng* denstl
      ! Conduit insulation mass [kg]
    ! (aiwp already contains turnstf)
    whtconin = tfleng * aiwp * dcondins
      ! [kg] Total conductor mass
    whtcon = whtconsc + whtconcu + whtconsh + whtconin
      ! [kg] Total coil mass
    whttf = (whtcas + whtcon)*n_tf
    ! End of general coil geometry values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Quench protection:
   !
     ! This copied from the tokamak module:
     ! Radial position of vacuum vessel [m]
     radvv = rmajor - rminor - scrapli - fwith - blnkith - vvblgap - shldith
 
     ! Quench time [s]
     taucq = (bt * ritfc * rminor * rminor) / (radvv * sigvvall)
 
     ! the conductor fraction is meant of the cable space!
     call protect(cpttf,estotftgj/n_tf*1.0D9,acstf,   leno**2   ,tdmptf,1-vftf,fcutfsu,tftmp,tmaxpro,jwdgpro,vd)
  
     ! Also give the copper area for REBCO quench calculations:
     copperA_m2 = cpttf/(acond * fcutfsu)
     vtfskv = vd/1.0D3 ! Dump voltage
  
     ! the conductor fraction is meant of the cable space!
     call protect(cpttf,estotftgj/n_tf*1.0D9,acstf,   leno**2   ,tdmptf,1-vftf,fcutfsu,tftmp,tmaxpro,jwdgpro,vd)
 
  
  
     ! Also give the copper area for REBCO quench calculations:
     copperA_m2 = cpttf/(acond * fcutfsu)
     vtfskv = vd/1.0D3 ! Dump voltage
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!!!!! Forces scaling !!!!!!!!!!!!!!
    max_force_density = config%max_force_density *(ritfc*1.0D-6/n_tf) * bmaxtf / awptf

    ! Approximate, very simple maxiumum stress: (needed for limitation of icc 32)
    strtf2 = max_force_density * thkwp *1.0D6 ! in Pa


    if (iprint == 1) call stcoil_output(outfile)

   contains
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    real(kind(1.0D0)) function bmax_from_awp(awp_dimensionless,current)

       !! Returns a fitted function for bmax for stellarators
       !! author: J Lion, IPP Greifswald
       !! Returns a fitted function for bmax in dependece
       !! of the winding pack. The stellarator type config
       !! is taken from the parent scope.
       !
       ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

       implicit none
 
       ! t is the winding pack width (sqrt(awp) r8 now) divided by coil_rmajor
       real(kind(1.0D0)), intent(in) ::awp_dimensionless,current
 

       ! This funtion is exact in scaling of the winding pack but does not take scaling in n_tf into account, neither does
       ! it work for varying aspect ratio. (In 0th order its insenstive to different aspect ratios.)
       ! r_coil_major and r_coil_minor are taken from parent scope
       bmax_from_awp = 2.0D-1 * current*n_tf/(r_coil_major-r_coil_minor) &
                      * (config%a1+config%a2/(sqrt(awp_dimensionless)))
    end function 

    real(kind(1.0D0)) function jcrit_frommaterial(bmax,thelium)

       ! gives jcrit from material
  
        real(kind(1.0D0)), intent(in) ::Bmax, thelium
  
        real(kind(1.0D0)) :: strain, bc20m, tc0m, jcritsc, bcrit, tcrit
  
        real(kind(1.0D0)) :: jstrand, jwp, fhe, tmarg
  
        real(kind(1.0D0)) :: c0, jcritstr, fcu
  
        logical :: validity
  
        strain = 0.0D0  ! for now
        fhe = vftf     ! this is helium fraction in the superconductor (set it to the fixed global variable here)
  
        fcu = fcutfsu ! fcutfsu is a global variable. Is the copper fraction
                       ! of a cable conductor.
  
  
  
        ! This fraction is copied from sctfcoil.f90 10/2019
        select case (isumattf)
  
        case (1)  !  ITER Nb3Sn critical surface parameterization
           bc20m = 32.97D0 ! these are values taken from sctfcoil.f90
           tc0m = 16.06D0
  
           !  jcritsc returned by itersc is the critical current density in the
           !  superconductor - not the whole strand, which contains copper
           if(bmax>bc20m) then 
              !print *,"bcrit too large!"
              jcritsc = 1.0D-9 ! Set to a small nonzero value
           else
              call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
           end if
  
           jcritstr = jcritsc * (1.0D0-fcu)
  
           ! This is needed right now. Can we change it later?
           if(jcritsc .lt. 0.0D0) then 
              jcritsc = 1.0D-9
           end if
  
        case (2)  !  Bi-2212 high temperature superconductor parameterization
  
           !  Current density in a strand of Bi-2212 conductor
           !  N.B. jcrit returned by bi2212 is the critical current density
           !  in the strand, not just the superconducting portion.
           !  The parameterization for jcritstr assumes a particular strand
           !  composition that does not require a user-defined copper fraction,
           !  so this is irrelevant in this model
  
           jstrand = jwp / (1.0D0-fhe)
  
           call bi2212(bmax,jstrand,thelium,fhts,jcritstr,tmarg) ! bi2212 outputs jcritstr
           jcritsc = jcritstr / (1.0D0-fcu)
           tcrit = thelium + tmarg
  
        case (3)  !  NbTi data
           bc20m = 15.0D0
           tc0m = 9.3D0
           c0 = 1.0D10
           
           if(bmax>bc20m) then 
              !print *,"bcrit too large!"
              jcritsc = 1.0D-9 ! Set to a small nonzero value
           else
              call jcrit_nbti(thelium,bmax,c0,bc20m,tc0m,jcritsc,tcrit)
              ! I dont need tcrit here so dont use it.
           end if
           jcritstr = jcritsc * (1.0D0-fcu)
           
           ! This is needed right now. Can we change it later?
           if(jcritsc .lt. 0.0D0) then 
  
              jcritstr = 1.0D-9* (1.0D0-fcu)
           end if
  
        case (4)  !  As (1), but user-defined parameters
           bc20m = bcritsc
           tc0m = tcritsc
           call itersc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
           jcritstr = jcritsc * (1.0D0-fcu)
  
        case (5) ! WST Nb3Sn parameterisation
              bc20m = 32.97D0
              tc0m = 16.06D0
  
              !  jcritsc returned by itersc is the critical current density in the
              !  superconductor - not the whole strand, which contains copper
  
              call wstsc(thelium,bmax,strain,bc20m,tc0m,jcritsc,bcrit,tcrit)
              jcritstr = jcritsc * (1.0D0-fcu)
  
        case (6) ! "REBCO" 2nd generation HTS superconductor in CrCo strand
           call jcrit_rebco(thelium,bmax,jcritsc,validity,0)
           !call supercon_croco(t_w_i**2,bmax,cpttf,tftmp, &
           !iprint, outfile, jcritsc,tcrit)
           ! this call might not be consistent with fcu and fhe.
           jcritsc = Max(1.0D-9,jcritsc)
        case default  !  Error condition
           idiags(1) = isumattf ; call report_error(156)
  
        end select
  
        jcrit_frommaterial = jcritsc *1.0D-6 ! To get it in MA/m^2
        return
    end function 

    subroutine protect(aio,tfes,acs,aturn,tdump,fcond,fcu,tba,tmax,ajwpro,vd)

        !! Finds the current density limited by the protection limit
        !! author: P J Knight, CCFE, Culham Science Centre
        !! author: J Miller, ORNL
        !! aio : input real : Operating current (A)
        !! tfes : input real : Energy stored in one TF coil (J)
        !! acs : input real : Cable space - inside area (m2)
        !! aturn : input real : Area per turn (i.e.  entire cable) (m2)
        !! tdump : input real : Dump time (sec)
        !! fcond : input real : Fraction of cable space containing conductor
        !! fcu : input real : Fraction of conductor that is copper
        !! tba : input real : He temperature at peak field point (K)
        !! tmax : input real : Max conductor temperature during quench (K)
        !! ajwpro : output real :  Winding pack current density from temperature
        !! rise protection (A/m2)
        !! vd : output real :  Discharge voltage imposed on a TF coil (V)
        !! This routine calculates maximum conductor current density which
        !! limits the peak temperature in the winding to a given limit (tmax).
        !! It also finds the dump voltage.
        !! <P>These calculations are based on Miller's formulations.
        !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
        !
        ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
         implicit none
   
         !  Arguments
   
         real(kind(1.0D0)), intent(in) :: aio, tfes, acs, aturn, tdump, fcond, &
         fcu,tba,tmax
         real(kind(1.0D0)), intent(out) :: ajwpro, vd
   
         !  Local variables
   
         integer :: no,np
         real(kind(1.0D0)) :: aa,ai1,ai2,ai3,ajcp,bb,cc,dd,tav
         real(kind(1.0D0)), dimension(11) :: p1, p2, p3
   
         ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   
         !  Integration coefficients p1,p2,p3
   
         p1(1) = 0.0D0
         p1(2) = 0.8D0
         p1(3) = 1.75D0
         p1(4) = 2.4D0
         p1(5) = 2.7D0
         p1(6) = 2.95D0
         p1(7) = 3.1D0
         p1(8) = 3.2D0
         p1(9) = 3.3D0
         p1(10) = 3.4D0
         p1(11) = 3.5D0
   
         p2(1) = 0.0D0
         p2(2) = 0.05D0
         p2(3) = 0.5D0
         p2(4) = 1.4D0
         p2(5) = 2.6D0
         p2(6) = 3.7D0
         p2(7) = 4.6D0
         p2(8) = 5.3D0
         p2(9) = 5.95D0
         p2(10) = 6.55D0
         p2(11) = 7.1D0
   
         p3(1) = 0.0D0
         p3(2) = 0.05D0
         p3(3) = 0.5D0
         p3(4) = 1.4D0
         p3(5) = 2.6D0
         p3(6) = 3.7D0
         p3(7) = 4.6D0
         p3(8) = 5.4D0
         p3(9) = 6.05D0
         p3(10) = 6.8D0
         p3(11) = 7.2D0
   
         !  Dump voltage
         vd = 2.0D0 * tfes/(tdump*aio)
   
         !  Current density limited by temperature rise during quench
   
         tav = 1.0D0 + (tmax-tba)/20.0D0
         no = int(tav)
         np = no+1
         np = min(np,11)
   
         ai1 = 1.0D16 * ( p1(no)+(p1(np)-p1(no)) * (tav - no) )
         ai2 = 1.0D16 * ( p2(no)+(p2(np)-p2(no)) * (tav - no) )
         ai3 = 1.0D16 * ( p3(no)+(p3(np)-p3(no)) * (tav - no) )
   
         aa = vd * aio/tfes
         bb = (1.0D0-fcond)*fcond*fcu*ai1
         cc = (fcu*fcond)**2 * ai2
         dd = (1.0D0-fcu)*fcu * fcond**2 * ai3
         ajcp = sqrt( aa* (bb+cc+dd) )
         ajwpro = ajcp*(acs/aturn)
   
    end subroutine protect

    subroutine intersect(x1,y1,n1,x2,y2,n2,x)

      !! Routine to find the x (abscissa) intersection point of two curves
      !! each defined by tabulated (x,y) values
      !! author: P J Knight, CCFE, Culham Science Centre
      !! x1(1:n1) : input real array : x values for first curve
      !! y1(1:n1) : input real array : y values for first curve
      !! n1       : input integer : length of arrays x1, y1
      !! x2(1:n2) : input real array : x values for first curve
      !! y2(1:n2) : input real array : y values for first curve
      !! n2       : input integer : length of arrays x2, y2
      !! x        : input/output real : initial x value guess on entry;
      !! x value at point of intersection on exit
      !! This routine estimates the x point (abscissa) at which two curves
      !! defined by tabulated (x,y) values intersect, using simple
      !! linear interpolation and the Newton-Raphson method.
      !! The routine will stop with an error message if no crossing point
      !! is found within the x ranges of the two curves.
      !! None
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

    subroutine stcoil_output(outfile)

      !! Writes stellarator modular coil output to file
      !! author: P J Knight, CCFE, Culham Science Centre
      !! outfile : input integer : output file unit
      !! This routine writes the stellarator modular coil results
      !! to the output file.
      !! None
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
      implicit none
  
      !  Arguments
  
      integer, intent(in) :: outfile
    
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
      call oheadr(outfile,'Modular Coils')
  
      call osubhd(outfile,'General Coil Parameters :')
  
      call ovarre(outfile,'Number of modular coils','(n_tf)',n_tf)
      call ovarre(outfile,'Cross-sectional area per coil (m2)','(tfarea/n_tf)', &
                  tfareain/n_tf)
      call ovarre(outfile,'Total inboard leg radial thickness (m)','(tfcth)',tfcth)
      call ovarre(outfile,'Total outboard leg radial thickness (m)','(tfthko)',tfthko)
      call ovarre(outfile,'Inboard leg outboard half-width (m)','(tficrn)',tficrn)
      call ovarre(outfile,'Inboard leg inboard half-width (m)','(tfocrn)',tfocrn)
      call ovarre(outfile,'Outboard leg toroidal thickness (m)','(tftort)',tftort)
      call ovarre(outfile,'Minimum Coil distance (m)','(toroidalgap)',toroidalgap)
      call ovarre(outfile,'Mean coil circumference (m)','(tfleng)',tfleng)
      call ovarre(outfile,'Total current (MA)','(ritfc)',1.0D-6*ritfc)
      call ovarre(outfile,'Current per coil(MA)','(ritfc/n_tf)',1.0D-6*ritfc/n_tf)
      call ovarre(outfile,'Winding pack current density (A/m2)','(jwptf)',jwptf)
      call ovarre(outfile,'Overall current density (A/m2)','(oacdcp)',oacdcp)
      call ovarre(outfile,'Maximum field on superconductor (T)','(bmaxtf)',bmaxtf)
      call ovarre(outfile,'Total Stored energy (GJ)','(estotftgj)',estotftgj)
      call ovarre(outfile,'Total mass of coils (kg)','(whttf)',whttf)
  
      call osubhd(outfile,'Coil Geometry :')
      call ovarre(outfile,'Inboard leg centre radius (m)','(r_tf_inleg_mid)',r_tf_inleg_mid)
      call ovarre(outfile,'Outboard leg centre radius (m)','(r_tf_outboard_mid)',r_tf_outboard_mid)
      call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax)
      call ovarre(outfile,'Clear horizontal bore (m)','(tf_total_h_width)',tf_total_h_width)
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
      call ovarre(outfile,'jop/jcrit','(fiooic)',fiooic)
  
      call osubhd(outfile,'Forces and Stress :')
      call ovarre(outfile,'Maximal force density (MN/m3)','(max_force_density)',max_force_density)
      call ovarre(outfile,'Maximal stress (approx.) (MPa)','(strtf2)',strtf2*1.0D-6)
  
      call osubhd(outfile,'Quench Restrictions :')
      call ovarre(outfile,'Allowable stress in vacuum vessel (VV) due to quench (Pa)','(sigvvall)',sigvvall)
      call ovarre(outfile,'Minimum allowed quench time due to stress in VV (s)','(taucq)',taucq, 'OP ')
      call ovarre(outfile,'Actual quench time (or time constant) (s)','(tdmptf)',tdmptf)
      call ovarre(outfile,'Maximum allowed voltage during quench due to insulation (kV)', '(vdalw)', vdalw)
      call ovarre(outfile,'Actual quench voltage (kV)','(vtfskv)',vtfskv, 'OP ')
      call ovarre(outfile,'Current (A) per m^2 copper:','(coppera_m2/coppera_m2_max)',coppera_m2/coppera_m2_max)



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

  end subroutine stcoil

  

end module stellarator_module
