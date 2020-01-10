! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module helias5b_coil_parameters

  !! Module containing Helias 5-B power plant parameter values
  !! author: P J Knight, CCFE, Culham Science Centre
  !! author: F Warmer, IPP Greifswald
  !! N/A
  !! This module contains a set of constants that define the
  !! coil set parameters for the Helias 5-B stellarator power plant design.
  !! HELIAS 5-B magnet system structure and maintenance concept,
  !! Felix Schauer, Konstantin Egorov, Victor Bykov, Fus. Eng. Design (2013),
  !! in press
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !  Number of coils
  integer, parameter :: n_tf5B = 50
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

  !! Module providing Cartesian vectors and associated operations
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module defines a vector datatype within Cartesian coordinates,
  !! and provides a set of basic vector operators.
  !! None
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
     module procedure dot_product_stell
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

  elemental function dot_product_stell(a,b)
    real(kind(1.0D0)) :: dot_product_stell
    type (vector), intent(in) :: a,b
    dot_product_stell = (a%x * b%x) + (a%y * b%y) + (a%z * b%z)
  end function dot_product_stell

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

  !! Module containing stellarator routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! parameters of the first wall, blanket and shield components
  !! of a fusion power plant.

  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  private
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

    use availability_module, only: avail
    use buildings_module, only: bldgcall
    use costs_module, only: costs
    use power_module, only: tfpwr, power1, acpow, power2
    use process_output, only: nout
    use vacuum_module, only: vaccall
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

    use build_variables, only: gapoh, iohcl, ohcth, tfootfi
    use current_drive_variables, only: irfcd
    use pfcoil_variables, only: ohhghf
    use physics_variables, only: aspect, dnbeta, kappa, kappa95, q, rmajor, &
      triang, hfac, tauscl
    use process_output, only: nout, icase, boundl, boundu
    use stellarator_variables, only: istell
    use tfcoil_variables, only: n_tf
    use times_variables, only: tburn, tcycle, tdown, tdwell, theat, tohs, &
      tpulse, tqnch, tramp
    implicit none

    !  Arguments

    !  Local variables

    !real(kind(1.0D0)) :: fsum

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

    tramp = 0.0D0
    tohs = 0.0D0
    tburn = 3.15576D7  !  one year
    tqnch = 0.0D0
    tpulse = tohs + theat + tburn + tqnch
    tdown  = tramp + tohs + tqnch + tdwell
    tcycle = tramp + tohs + theat + tburn + tqnch + tdwell

    !  Coil quantities
    n_tf = 50.0D0

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    use physics_variables, only: aspect, eps, rmajor, rminor, sarea, sareao, &
      vol, xarea
    use process_output, only: pi
    implicit none

    !  Arguments

    !  Local variables

    !  Cross-sectional area, averaged over toroidal angle
    rminor = rmajor/aspect
    eps = 1.0D0/aspect

    vol = rmajor*rminor**2 * 19.75D0 !This value is for Helias5 solely

    sarea = rmajor*rminor * 48.56D0 !This value is for Helias5 solely

    xarea = pi*rminor*rminor  ! average, could be calculated for every toroidal angle if desired

    !  sareao is retained only for obsolescent fispact calculation...

    sareao = 0.5D0*sarea  !  Used only in the divertor model; approximate as for tokamaks

  end subroutine stgeom

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    use build_variables, only: blbmith, blbmoth, blbpith, blbpoth, blbuith, &
      blbuoth, blnkith, blnkoth, blnktth, bore, ddwi, fwarea, fwith, fwoth, &
      gapds, gapoh, gapomin, gapsto, hmax, ohcth, r_tf_outboard_mid, rbld, &
      rsldi, rsldo, rspo, scrapli, scraplo, shldith, shldoth, shldtth, tfcth, &
      tfthko
    use fwbs_variables, only: afw, blktmodel, fdiv, fhcd, fhole, fw_wall
    use heat_transport_variables, only: ipowerflow
    use physics_variables, only: rmajor, rminor, sarea
    use process_output, only: mfile, oheadr, obuild, ovarre
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    use build_variables, only: fwarea
    use constraint_variables, only: peakfactrad, peakradwallload
    use current_drive_variables, only: cnbeam, enbeam, ftritbm, pinjmw, pnbeam
    use fwbs_variables, only: fdiv, fhcd, fhole
    use heat_transport_variables, only: ipowerflow
    use physics_functions_module, only: palph, beamfus, palph2
    use physics_module, only: plasma_composition, rether, radpwr, pcond, phyaux
    use physics_variables, only: afuel, alphan, alpharate, alphat, aspect, &
      beamfus0, beta, betaft, betalim, betanb, betap, betbm0, bp, bt, btot, &
      burnup, dene, deni, dlamie, dnalp, dnbeam2, dnelimt, dnitot, dnla, &
      dntau, ealphadt, eps, falpe, falpha, falpi, fdeut, fhe3, figmer, ftrit, &
      fusionrate, hfact, ifalphap, ignite, iinvqd, isc, iwalld, kappa, &
      kappa95, kappaa, palpepv, palpepv, palpfwmw, palpipv, palpmw, pbrempv, &
      pchargemw, pcoreradmw, pcoreradpv, pdd, pdhe3, pdivt, pdt, pedgeradmw, &
      pfuscmw, pedgeradpv, photon_wall, piepv, plascur, plinepv, pneutmw, &
      pneutpv, pohmmw, powerht, powfmw, pradmw, pradpv, protonrate, &
      pscalingmw, psolradmw, psyncpv, ptremw, ptrepv, ptrimw, ptripv, q, q95, &
      qfuel, qstar, rad_fraction, rmajor, rminor, rndfuel, sarea, tauee, &
      taueff, tauei, taup, te, ten, ti, tin, vol, wallmw, xarea, zeff, zeffai, &
      ffwal, palpnb, palppv, pchargepv
    use process_output, only: echarge, nout
    use profiles_module, only: plasma_profiles
    use stellarator_variables, only: f_rad, iotabar
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
    call radpwr(pbrempv,plinepv,psyncpv, pcoreradpv,pedgeradpv,pradpv)

    pcoreradmw = pcoreradpv*vol
    pedgeradmw = pedgeradpv*vol
    pradmw = pradpv*vol

    !  Heating power to plasma (= Psol in divertor model)
    !  Ohmic power is zero in a stellarator
    !  pradmw here is core + edge (no SOL)

    powht = falpha*palpmw + pchargemw + pohmmw - pradmw
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

    !  Calculate beta limit

    call stblim(betalim)

  end subroutine stphys

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    use current_drive_variables, only: bigq, cnbeam, echpwr, enbeam, &
      etacd, etaech, etalh, etanbi, forbitloss, frbeam, nbshield, nbshinef, &
      pheat, pinjemw, pinjimw, pinjmw, plhybd, pnbeam, porbitlossmw, &
      rtanbeam, rtanmax, taubeam
    use current_drive_module, only: culnbi
    use heat_transport_variables, only: pinjwp
    use error_handling, only: idiags, report_error
    use physics_variables, only: ignite, pohmmw, powfmw
    use process_output, only: oheadr, ocmmnt, oblnkl, ovarre
    use stellarator_variables, only: isthtr
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    use build_variables, only: blarea, blareaib, blbmith, blbmoth, blbpith, &
      blbpoth, blbuith, blbuoth, blnkith, blnkoth, blnktth, ddwex, ddwi, &
      fwarea, fwareaib, fwareaob, fwith, fwoth, r_tf_outboard_mid, scrapli, &
      scraplo, sharea, shareaib, shareaob, shldith, shldoth, shldtth, tfthko, &
      blareaob
    use cost_variables, only: abktflnc, tlife
    use current_drive_variables, only: porbitlossmw
    use divertor_variables, only: divclfr, divdens, divmas, divplt, divsur
    use fwbs_module, only: tsat, blanket_neutronics, &
      sctfcoil_nuclear_heating_iter90
    use fwbs_variables, only: afwi, afwo, bktlife, blktmodel, blkttype, &
      breedmat, coolmass, coolp, coolwh, declblkt, declfw, declshld, &
      densbreed, denstl, dewmkg, emult, emultmw, fblbe, fblbreed, fblhebmi, &
      fblhebmo, fblli, fbllipb, fblss, fblvd, fdiv, fhcd, fhole, fvoldw, &
      fvolso, fwclfr, fwlife, fwmass, hcdportsize, li6enrich, nflutf, &
      npdiv, nphcdin, nphcdout, outlet_temp, pnucblkt, pnuccp, pnucdiv, &
      pnucdiv, pnucfw, pnuchcd, pnucloss, pnucshld, praddiv, pradfw, pradhcd, &
      pradloss, primary_pumping, ptfnuc, rdewex, rpf2dewar, secondary_cycle, &
      tbr, tritprate, vdewex, vdewin, vfblkt, vfshld, volblkt, volblkti, &
      volblkto, volshld, vvmass, wallpf, whtblbe, whtblbreed, whtblkt, &
      whtblli, whtblss, whtblvd, whtshld, wpenshld, wtblli2o, wtbllipb, &
      fblhebpi, fblhebpo, fblli2o, fvolsi
    use heat_transport_variables, only: fpumpblkt, fpumpdiv, fpumpfw, &
      fpumpshld, htpmw_blkt, htpmw_div, htpmw_fw, htpmw_shld, ipowerflow
    use kit_blanket_model, only: nflutfi, nflutfo, pnuctfi, pnuctfo, &
      t_bl_y, vvhemaxi, vvhemaxo, vvhemini, vvhemino
    use error_handling, only: report_error
    use physics_variables, only: pdivt, pneutmw, pradmw, rminor, rmajor, &
      sarea, wallmw
    use process_output, only: pi, oheadr, ovarre, osubhd, ovarin, ocmmnt, oblnkl
    use tfcoil_variables, only: i_tf_sup
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    use error_handling, only: fdiags, report_error
    use physics_variables, only: dene, dnla
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

    use current_drive_variables, only: pinjmw
    use physics_module, only: fhfac, pcond
    use physics_variables, only: afuel, aspect, bt, dene, dnitot, dnla, &
      eps, ignite, iinvqd, kappa, kappa95, kappaa, palpmw, pchargemw, &
      pchargepv, plascur, qstar, rmajor, rminor, te, ten, tin, vol, xarea, &
      zeff, pcoreradpv, hfac, tauscl
    use process_output, only: osubhd, oblnkl
    use stellarator_variables, only: iotabar
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
    
    use availability_module, only: avail
    use buildings_module, only: bldgcall
    use costs_module, only: costs
    use physics_module, only: outplas
    use power_module, only: tfpwr, acpow, power2
    use vacuum_module, only: vaccall
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

    use fwbs_variables, only: dewmkg
    use process_output, only: oheadr, ovarre
    use structure_variables, only: aintmass, clgsmass, coldmass, fncmass, gsmass
    use tfcoil_variables, only: whttf
    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Tokamak-specific PF coil fence mass set to zero
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

    use divertor_variables, only: anginc, divsur, hldiv, tdiv, xpertin
    use physics_variables, only: afuel, pdivt, rmajor
    use process_output, only: echarge, pi, twopi, umass, oheadr, ovarre, ovarin
    use stellarator_variables, only: bmn, f_asym, f_rad, f_w, fdivwet, &
      flpitch, m_res, n_res, shear
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

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcoil(outfile,iprint)

    !! Routine that performs the calculations for stellarator coils
    !! author: P J Knight, CCFE, Culham Science Centre
    !! author: F Warmer, IPP Greifswald
    !! outfile : input integer : output file unit
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine calculates the properties of the coils for
    !! a stellarator device.
    !! <P>The coil set parameters for the Helias 5-B stellarator power
    !! plant design are used as the basis for the calculations. The coils
    !! are assumed to be a fixed shape, but are scaled in size
    !! appropriately for the machine being modelled. Analytic inductance
    !! and field calculations based on the assumption of circular coils
    !! are used to evaluate the critical field at the coil superconductor.
    !! The Stellarator Coil model for the Systems code PROCESS,
    !! F. Warmer, F. Schauer, IPP Greifswald, October 2013
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use helias5b_coil_parameters
    use build_variables, only: blnkith, blnkoth, ddwi, dh_tf_inner_bore, &
      dr_tf_inner_bore, fwith, fwoth, gapds, gapsto, hmax, r_tf_outboard_mid, &
      r_tf_outboard_mid, scrapli, scraplo, shldith, shldoth, tfcth, tfthko, &
      r_tf_inboard_mid
    use fwbs_variables, only: denstl
    use error_handling, only: report_error, fdiags, idiags
    use physics_variables, only: bt, rmajor, rminor
    use process_output, only: pi, twopi, rmu0, find_y_nonuniform_x, ellipke, &
      sumup3, tril
    use stellarator_variables, only: hportamax, hporttmax, hportpmax, &
      vportamax, vportpmax, vporttmax
    use structure_variables, only: aintmass
    use tfcoil_variables, only: acasetf, acndttf, acond, acstf, aiwp, arealeg, &
      aswp, avwp, bmaxtf, casthi, casths, cpttf, dcase, dcopper, estotftgj, &
      fcutfsu, isumattf, jwptf, n_tf, oacdcp, rbmax, ritfc, tfareain, &
      tfcryoarea, tficrn, tfleng, tfocrn, tfsai, tfsao, tftmp, tftort, &
      thicndut, thkcas, thkwp, thwcndut, tinstf, turnstf, vftf, whtcas, &
      whtcon, whtconcu, whtconsc, whtconsh, whttf, wwp1, dcond
    implicit none

    !  Arguments

    integer, intent(in) :: outfile,iprint

    !  Local variables

    integer :: nbticool,n_it,k
    real(kind(1.0D0)) :: a_hor_max,a_vert_max,alph,b,b0_final,b_abs_max, &
         b_abs_mittel,b_hor_avg,b_hor_max,b_i,b_max_final,b_max_max,b_maxtf, &
         b_vert_avg,b_vert_max,bc,cpttf2,cr_area,d_coil,d_ic,du,f_b,f_i, &
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
    f_N = n_tf/n_tf5B       !  Coil number factor
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

       call scaling_calculations(f_R, f_s, f_Q, f_I, n_tf, &
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

    call scaling_calculations(f_R, f_s, f_Q_final, f_I, n_tf, &
         B_max_final, B0_final)

    !  Maximum field at superconductor surface (T)

    b_maxtf = B_max_final

    !  Total stored magnetic energy calculation (GJ)
    !  W_mag2 = k2.*(bt.^2).*(f_s.^2).*f_R;  ! alternative good approximation

    W_mag = coil_energy(n_tf, f_R, f_s, f_I, f_Q_final)

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
    !  (excludes half of coil thickness, otherwise equal to r_tf_outboard_mid)

    R_occ = rmajor + rminor + scraplo + fwoth + blnkoth + shldoth + ddwi + gapsto

    !  Average space between two coil centres (m)

    dU = 2.0D0*pi * R_occ/n_tf

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
    r_tf_inboard_mid = rmajor - (0.5D0*tfcth+gapds+ddwi+shldith+blnkith+fwith+scrapli+rminor)
    ! [m] radius of centre of inboard leg, average
    rcoil = r_tf_inboard_mid + 0.5D0*tfcth  ! [m] radius of outer edge of inboard leg, average
    tfareain = n_tf*tfcth*tftort  ! [m^2] Total area of all coil legs
    tfarea_sc = n_tf*awptf        ! [m^2] Total area of all coil winding packs
    ritfc = n_tf * I * 1.0D6      ! [A] Total current in ALL coils
    oacdcp = ritfc/tfareain       ! [A / m^2] overall current density
    rbmax = rcoil                 ! [m] radius of peak field occurrence, average
                                  !     N.B. different to tokamak SCTF calculation
    hmax = 0.5D0*h_max - tfcth    ! [m] maximum half-height of coil
    dr_tf_inner_bore = D_coil     ! [m] estimated horizontal coil bore
    dh_tf_inner_bore = 2.0D0*hmax          ! [m] estimated vertical coil bore
    tfleng = U                    ! [m] estimated average length of a coil

    estotftgj = W_mag             ! [GJ] Total magnetic energy

    !jwptf = ritfc/(n_tf*awptf)
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

    tfsai = n_tf*tftort * 0.5D0*tfleng

    ! [m^2] Total surface area of coil side facing plasma: outboard region

    tfsao = tfsai  !  depends, how 'inboard' and 'outboard' are defined

    ! [m^2] Total surface area of toroidal shells covering coils

    tfcryoarea = 2.0D0 * tfleng * twopi*0.5D0*(r_tf_inboard_mid+r_tf_outboard_mid)

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

    whttf = (whtcas + whtcon)*n_tf

    ! [kg] Total support structure mass
    ! msupstr includes the casing mass, so this needs to be subtracted
    ! Currently, this is assumed to comprise all the machine's support
    ! structural mass

    aintmass = msupstr - (whtcas*n_tf)

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

      !! Routine that calculates the important magnetic field values
      !! for stellarator coils
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: F Warmer, IPP Greifswald
      !! author: F Schauer, IPP Greifswald
      !! f_R          : input real : major radius scaling factor
      !! f_s          : input real : coil width scaling factor
      !! f_Q          : input real : coil cross-section scaling factor
      !! f_I          : input real : current scaling factor
      !! Nsp          : input real : number of coils
      !! B_abs_max    : output real : maximum field at superconductor surface (T)
      !! B_abs_mittel : output real : magnetic field at plasma centre (T)
      !! This routine calculates the magnetic field at the plasma centre
      !! and the maximum field value on the superconductor using scaling factors
      !! to adjust the values calculated for the Helias 5-B design.
      !! <P>The coils are approximated by circular filaments, shifted and
      !! tilted with respect to one another. The field calculations use elliptic
      !! integrals.
      !! The Stellarator Coil model for the Systems code PROCESS,
      !! F. Warmer, F. Schauer, IPP Greifswald, October 2013
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      use cartesian_vectors

      implicit none

      !  Arguments

      real(kind(1.0D0)), intent(in) :: f_r, f_s, f_q, f_i, nsp
      real(kind(1.0D0)), intent(out) :: b_abs_max, b_abs_mittel

      !  Local variables

      integer :: i,n_h,n_b,nc,nsp_int
      real(kind(1.0D0)) :: b,b_abs_zentr,b_abs_zwischen,b_x_zentr, &
           b_x_zwischen,b_y_zentr,b_y_zwischen,bi_x,bi_y,bir,bir_1,biz,biz_1, &
           di_mean,dphi,h,il,isp,phi,r_q,rg,ri,rk,rri,vz_r,zi
          !  b_max_equiv
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

      !! Routine that calculates the magnetic field due to a circular coil
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: F Warmer, IPP Greifswald
      !! author: F Schauer, IPP Greifswald
      !! Ri : input real : radius of coil inner surface (m)
      !! H  : input real : coil cross-sectional radial width (m)
      !! L  : input real : coil cross-sectional axial width (m)
      !! nR : input integer : number of radial filaments to use
      !! nZ : input integer : number of axial filaments to use
      !! r  : input real : radial distance of point of interest from
      !! coil centre (m)
      !! z  : input real : axial distance of point of interest from coil (m)
      !! Br : output real : magnetic field radial component at (r,z) (T)
      !! Bz : output real : magnetic field axial component at (r,z) (T)
      !! This routine calculates the magnetic field at a point (R,Z)
      !! due to unit current flowing in a circular planar coil centred
      !! at the origin, lying in the x-y plane.
      !! <P>The coil is approximated by nR*nZ circular filaments, which
      !! take account of the coil conductor cross-section.
      !! The field calculations use elliptic integrals.
      !! The Stellarator Coil model for the Systems code PROCESS,
      !! F. Warmer, F. Schauer, IPP Greifswald, October 2013
      !! D. Bruce Montgomery, Solenoid Magnet Design, Section 8.4.1, p.237,
      !! Wiley-Interscience (1968)
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

      !! Routine that calculates the stored energy in the stellarator coils
      !! author: P J Knight, CCFE, Culham Science Centre
      !! author: F Warmer, IPP Greifswald
      !! author: F Schauer, IPP Greifswald
      !! Nsp : input real : number of coils
      !! f_R : input real : major radius scaling factor
      !! f_s : input real : coil width scaling factor
      !! f_I : input real : current scaling factor
      !! f_Q : input real : coil cross-section scaling factor
      !! This routine calculates the total stored energy in the modular
      !! stellarator coils.
      !! <P>The coils are approximated by a number of circular planar
      !! filaments, which take account of the coil conductor cross-section.
      !! The field calculations use elliptic integrals.
      !! The Stellarator Coil model for the Systems code PROCESS,
      !! F. Warmer, F. Schauer, IPP Greifswald, October 2013
      !! S. Babic et al, IEEE Transactions on Magnetics, 46, no.9 (2010) 3591
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
           p5,phi,r_q,rg,rk,rk5bcorr,rp,rs,theta,vz,xc,yc,zc,vo,ksq,psi
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

  end subroutine stcoil

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine stcoil_output(outfile)

    !! Writes stellarator modular coil output to file
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : output file unit
    !! This routine writes the stellarator modular coil results
    !! to the output file.
    !! None
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use build_variables, only: dh_tf_inner_bore, dr_tf_inner_bore, hmax, &
      r_tf_inboard_mid, r_tf_outboard_mid, tfcth, tfthko
    use process_output, only: oheadr, osubhd, ovarre
    use stellarator_variables, only: hportamax, hportpmax, hporttmax, &
      vportamax, vportpmax, vporttmax
    use tfcoil_variables, only: acasetf, acond, acasetf, aiwp, aswp, bmaxtf, &
      casthi, casths, cpttf, estotftgj, fcutfsu, jwptf, n_tf, oacdcp, ritfc, &
      tfareain, tficrn, tfleng, tfocrn, tftort, thicndut, thkcas, thkwp, &
      thwcndut, turnstf, turnstf, vftf, whtcas, whtcon, whtconcu, whtconsc, &
      whtconsh, whttf, wwp1, acstf, avwp, tinstf
    implicit none

    !  Arguments

    integer, intent(in) :: outfile

    !  Local variables
    real(kind(1.0D0)) :: ap

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
    call ovarre(outfile,'Mean coil circumference (m)','(tfleng)',tfleng)
    call ovarre(outfile,'Total current (MA)','(ritfc/1.D6)',1.0D-6*ritfc)
    call ovarre(outfile,'Winding pack current density (A/m2)','(jwptf)',jwptf)
    call ovarre(outfile,'Overall current density (A/m2)','(oacdcp)',oacdcp)
    call ovarre(outfile,'Maximum field on superconductor (T)','(bmaxtf)',bmaxtf)
    call ovarre(outfile,'Total Stored energy (GJ)','(estotftgj)',estotftgj)
    call ovarre(outfile,'Total mass of coils (kg)','(whttf)',whttf)

    call osubhd(outfile,'Coil Geometry :')
    call ovarre(outfile,'Inboard leg centre radius (m)','(r_tf_inboard_mid)',r_tf_inboard_mid)
    call ovarre(outfile,'Outboard leg centre radius (m)','(r_tf_outboard_mid)',r_tf_outboard_mid)
    call ovarre(outfile,'Maximum inboard edge height (m)','(hmax)',hmax)
    call ovarre(outfile,'Clear horizontal bore (m)','(dr_tf_inner_bore)',dr_tf_inner_bore)
    call ovarre(outfile,'Clear vertical bore (m)','(dh_tf_inner_bore)',dh_tf_inner_bore)

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
