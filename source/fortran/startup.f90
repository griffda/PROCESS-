!  $Id:: startup.f90 258 2014-04-24 12:28:55Z pknight                   $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module startup_module

  !! Module containing plasma start-up routines
  !! author: P J Knight, CCFE, Culham Science Centre
  !! N/A
  !! This module contains routines for calculating the
  !! parameters of the plasma start-up phase.
  !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  implicit none

  private
  public :: startup

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine startup(outfile,iprint)

    !! NEVER CALLED
    !! Routine to find the minimum auxiliary power required for start-up
    !! author: C A Gardner, AEA Fusion, Culham Laboratory
    !! author: P J Knight, CCFE, Culham Science Centre
    !! outfile : input integer : Fortran output unit identifier
    !! iprint : input integer : switch for writing to output file (1=yes)
    !! This routine finds the minimum auxiliary power required in start-up.
    !! This is accomplished by calling the non-linear optimisation routine
    !! VMCON, so this subroutine simply sets up the equations to be solved.
    !! <P>PROCESS assumes that all the ion temperatures and profile
    !! parameters are identical and utilises charge neutrality in
    !! order to calculate DENI.
    !! <P>ZEFF is assumed to remain constant, in other words the
    !! ion/electron ratios are invariant.
    !! <P>The most general form for the energy confinement time is :
    !! <PRE>
    !! ptaue   qtaue                   rtaue
    !! ____      __
    !! taue =  gtaue + ftaue   dene      te     ( <Paux>  +  palpmw )
    !! ____     __
    !! where DENE and TE are the electron density (10**20 m**-3) and
    !! electron temperature (10 keV) respectively.
    !! </PRE>
    !! The equation defining ignition has the form
    !! <PRE>
    !! 2   s                2   1/2      -3/2
    !! A n   T      =   B n   T    +  P    - C n   T    + D T
    !! e20 e10          e20 e10     aux      e20 e10      e10
    !! ----------
    !! tau
    !! E
    !! =    P         +  P    -  P         +  P
    !! alpha        aux     rad          ohm
    !! 
    !! p   q                                  r
    !! tau    =  gg  + ff n   T    ( P      + P    + P    + P    )
    !! E                e20 e10    alpha    ohm    rad    ohm
    !! </PRE>
    !! We solve this equation for (ne20, Te10) subject to the constraints,
    !! <PRE>
    !! d P
    !! aux
    !! -----  =  0.
    !! d T
    !! e10
    !! </PRE>
    !! to find the minimum auxiliary power required in start-up.
    !! Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
    !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    use constraint_variables, only: auxmin
    use physics_variables, only: alphan, alphat, dene, deni, ftrit, kappa, &
      pcoef, plascur, ralpne, rmajor, rminor, rncne, rnfene, rnone, rpfac, &
      te, ti, vol, zeff, facoh
    use process_output, only: oheadr, ovarre
    use startup_variables, only: nign, tign, ftaue, ftaue, ptaue, qtaue, &
      rtaue, gtaue

    
		use maths_library, only: vmcon
    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: aa,bb,cc,dd,ne20,te10,ti10,fd,fdt,rrplas,objf,tol
    integer :: meq,maxfev,info,nfev,niter,mode,s

    integer, parameter :: n = 2
    integer, parameter :: m = 2
    integer, parameter :: lcnorm = n+1
    integer, parameter :: lb = n+1
    integer, parameter :: ldel = 7*(n+1)
    integer, parameter :: lh = 2*(n+1)
    integer, parameter :: lwa = 2*(n+1)
    integer, parameter :: liwa = 6*(n+1)+m

    real(kind(1.0D0)), dimension(n) :: x,fgrd,glag,glaga,gamma1, &
         eta,xa,bdelta,bndl,bndu
    real(kind(1.0D0)), dimension(m) :: conf,cm
    real(kind(1.0D0)), dimension(lcnorm,m) :: cnorm
    real(kind(1.0D0)), dimension(lb,lb) :: b
    real(kind(1.0D0)), dimension(m+2*n+1) :: vlam1,vmu
    real(kind(1.0D0)), dimension(ldel) :: delta
    real(kind(1.0D0)), dimension(n+1) :: gm,bdl,bdu
    real(kind(1.0D0)), dimension(lh,lh) :: h
    real(kind(1.0D0)), dimension(lwa) :: wa
    integer, dimension(liwa) :: iwa
    integer, dimension(n) :: ilower,iupper

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !  Define normalised temperature and densities

    ne20 = dene/1.0D20
    te10 = te/10.0D0
    ti10 = ti/10.0D0

    aa = 0.24D0*(1.0D0 + (deni/dene + ralpne + rncne + rnone + rnfene)*ti/te)

    if ((ti10 > 0.4D0).and.(ti10 <= 1.0D0)) then
       s = 3
    else if ((ti10 > 1.0D0).and.(ti10 <= 2.0D0)) then
       s = 2

       !  Outside the above ranges of ti10, the value of s is not known
       !  (by PJK), so s is set according to whether ti10 is above
       !  or below unity...

    else if (ti10 <= 1.0D0) then
       s = 3
    else
       s = 2
    end if

    fd = 1.0D0 - ftrit
    fdt = deni/dene

    !  Alpha power multiplier

    bb = 0.155D0 * (4.0D0*fd*(1.0D0 - fd)*fdt**2) * &
         (1.0D0 + alphan + alphat)**s / &
         ((1.0D0 + alphan)**(s-2) * (1.0D0 + 2.0D0*alphan + dble(s)*alphat))

    !  Radiation power multiplier

    cc = 1.68D-2 * ( sqrt((1.0D0 + alphan)**3) &
         * sqrt(1.0D0 + alphan + alphat) / &
         (1.0D0 + 2.0D0*alphan + 0.5D0*alphat) )*zeff

    !  Ohmic power multiplier
    !  If the ohmic power density calculated in subroutine POHM is
    !  changed in the future then the constant DD must be changed
    !  accordingly.

    !  The following lines come directly from the formulae within
    !  routine POHM, but with t10 replaced by pcoef

    rrplas = 2.15D-9*zeff*rmajor/(kappa*rminor**2*pcoef**1.5D0)
    rrplas = rrplas*rpfac

    dd = (facoh*plascur)**2 * rrplas * 1.0D-6/vol

    !  Multiply coefficients by plasma volume

    aa = aa*vol
    bb = bb*vol
    cc = cc*vol
    dd = dd*vol

    !  Initial values for the density and temperature

    x(1) = ne20
    x(2) = te10

    !  Initialise variables for VMCON

    meq = 1
    mode = 0
    tol = 1.0D-3
    maxfev = 100

    ilower(1) = 1
    ilower(2) = 1
    iupper(1) = 1
    iupper(2) = 1
    bndl(1) = 0.1D0
    bndl(2) = 0.5D0
    bndu(1) = 100.0D0
    bndu(2) = 50.0D0

    call vmcon(start1,start2,mode,n,m,meq,x,objf,fgrd,conf, &
         cnorm,lcnorm,b,lb,tol,maxfev,info,nfev,niter,vlam1,glag,vmu, &
         cm,glaga,gamma1,eta,xa,bdelta,delta,ldel,gm,bdl,bdu, &
         h,lh,wa,lwa,iwa,liwa,ilower,iupper,bndl,bndu)
    if(info /= 1)write(*,*)'Subroutine startup failed: VMCON call failed.'

    auxmin = objf
    nign = x(1)*1.0D20
    tign = x(2)*10.0D0

    !  Output Section

    if (iprint == 1) then

       call oheadr(outfile,'Start-up')

       call ovarre(outfile,'Minimum auxiliary power requirement (MW)', &
            '(auxmin)',auxmin)
       call ovarre(outfile,'Start-up electron density (/m3)', &
            '(nign))',nign)
       call ovarre(outfile,'Start-up electron temperature (keV)', &
            '(tign)',tign)

    end if

  contains

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine start1(n,m,x,objf,conf,info)

      !! Calculates the auxiliary power and the constraint equations
      !! relevant to the minimisation of the auxiliary power
      !! author: C A Gardner, AEA Fusion, Culham Laboratory
      !! author: P J Knight, CCFE, Culham Science Centre
      !! n       : input integer : number of equations
      !! m       : input integer : number of constraints
      !! x(n)    : input real array : current values of the
      !! density and temperature
      !! objf    : output real : auxiliary power (MW)
      !! conf(m) : output real array : contraints
      !! info    : in/out integer : error status flag
      !! This routine calculates the auxiliary power and the
      !! constraint equations relevant to its minimisation.
      !! Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: n,m
      integer, intent(inout) :: info
      real(kind(1.0D0)), dimension(n), intent(in) :: x
      real(kind(1.0D0)), dimension(m), intent(out) :: conf
      real(kind(1.0D0)), intent(out) :: objf

      !  Local variables

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call cudrv1(n,x,objf)
      call constr(n,m,x,objf,conf)

      info = info*1

    end subroutine start1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine start2(n,m,x,fgrd,cnorm,lcnorm,info)

      !! Calculates the first derivative of the auxiliary power
      !! and the constraint equations relevant to the minimisation
      !! of the auxiliary power
      !! author: C A Gardner, AEA Fusion, Culham Laboratory
      !! author: P J Knight, CCFE, Culham Science Centre
      !! n       : input integer : number of equations
      !! m       : input integer : number of constraints
      !! x(n)    : input real array : current values of the density and temperature
      !! fgrd(n) : output real array : first derivative of auxiliary power
      !! cnorm(m,lcnorm) : output real array : constraints
      !! lcnorm  : input integer : array index
      !! info    : in/out integer : error status flag
      !! This routine calculates the first derivative of the
      !! auxiliary power and the constraint equations relevant
      !! to the minimisation of the auxiliary power.
      !! Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: n,m,lcnorm
      integer, intent(inout) :: info
      real(kind(1.0D0)), dimension(n), intent(in) :: x
      real(kind(1.0D0)), dimension(n), intent(out) :: fgrd
      real(kind(1.0D0)), dimension(lcnorm,m), intent(out) :: cnorm

      !  Local variables

      real(kind(1.0D0)), dimension(2) :: xfor,xbac,cfor,cbac
      real(kind(1.0D0)) :: ffor,fbac
      real(kind(1.0D0)) :: epsfcn = 1.0D-3
      integer :: i,j

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do i = 1,n
         do j = 1,n
            xfor(j) = x(j)
            xbac(j) = x(j)
            if (i == j) then
               xfor(i) = x(j) * (1.0D0 + epsfcn)
               xbac(i) = x(j) * (1.0D0 - epsfcn)
            end if
         end do

         call cudrv1(n,xfor,ffor)
         call constr(n,m,xfor,ffor,cfor)

         call cudrv1(n,xbac,fbac)
         call constr(n,m,xbac,fbac,cbac)

         fgrd(i) = (ffor-fbac)/(xfor(i)-xbac(i))

         do j = 1,m
            cnorm(i,j) = (cfor(j)-cbac(j)) / (xfor(i)-xbac(i))
         end do
      end do

      info = info*1

    end subroutine start2

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine cudrv1(n,x,paux)

      !! Routine acting as interface between the start-up routines
      !! and CUDRIV
      !! author: C A Gardner, AEA Fusion, Culham Laboratory
      !! author: P J Knight, CCFE, Culham Science Centre
      !! n       : input integer : number of equations
      !! x(n)    : input real array : current values of the density and temperature
      !! paux    : output real : auxiliary power (MW)
      !! This routine acts as the interface between the start-up routines
      !! and <A HREF="cudriv.html">CUDRIV</A>.
      !! Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      use current_drive_variables, only: pinjmw
      use physics_module, only: physics

      implicit none

      !  Arguments

      integer, intent(in) :: n
      real(kind(1.0D0)), dimension(n), intent(in) :: x
      real(kind(1.0D0)), intent(out) :: paux

      !  Local variables

      real(kind(1.0D0)) :: storen,storet

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !  Set electron density, temperature for this iteration, but
      !  store original values to allow them to be reset later

      storen = dene
      storet = te

      dene = x(1)*1.0D20
      te   = x(2)*10.0D0

      !  Call the physics routines with these values - PHYSICS calls
      !  routine CUDRIV itself.

      call physics

      !  Total injection power (MW)

      paux = pinjmw

      !  Reset density and temperature to pre-call values

      dene = storen
      te = storet

      !  Call physics routines again to reset all values

      call physics

    end subroutine cudrv1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine constr(n,m,x,paux,conf)

      !! Calculates the constraint equations relevant to the
      !! minimisation of the auxiliary power
      !! author: C A Gardner, AEA Fusion, Culham Laboratory
      !! author: P J Knight, CCFE, Culham Science Centre
      !! n       : input integer : number of equations
      !! m       : input integer : number of constraints
      !! x(n)    : input real array : current values of the density and temperature
      !! paux    : input real : auxiliary power (MW)
      !! conf(m) : output real array : constraints
      !! This routine calculates the constraint equations relevant
      !! to the minimisation of the auxiliary power.
      !! Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
      !! AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      implicit none

      !  Arguments

      integer, intent(in) :: n,m
      real(kind(1.0D0)), intent(in) :: paux
      real(kind(1.0D0)), dimension(n), intent(in) :: x
      real(kind(1.0D0)), dimension(m), intent(out) :: conf

      !  Local variables

      real(kind(1.0D0)) :: eta,detadt

      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      eta = bb*x(1)**2*x(2)**s + paux - cc*x(1)**2*sqrt(x(2)) &
           + dd / sqrt(x(2)**3)
      detadt = dble(s)*bb*x(1)**2*x(2)**(s-1) - 0.5D0*cc*x(1)**2 &
           / sqrt(x(2)) - 1.5D0*dd/sqrt(x(2)**5)

      conf(1) = aa*x(1) - (gtaue + ftaue*(1.0D0+rtaue)*x(1)**ptaue &
           * x(2)**qtaue*eta**rtaue)*detadt &
           - qtaue*ftaue*x(1)**ptaue*x(2)**(qtaue-1.0D0)*eta**(rtaue+1.0D0)
      conf(2) = paux

    end subroutine constr

  end subroutine startup

end module startup_module
