!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module startup_module

  !+ad_name  startup_module
  !+ad_summ  Module containing plasma start-up routines
  !+ad_type  Module
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  startup
  !+ad_args  N/A
  !+ad_desc  This module contains routines for calculating the
  !+ad_desc  parameters of the plasma start-up phase.
  !+ad_prob  None
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  maths_library
  !+ad_call  physics_module
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  startup_variables
  !+ad_hist  06/11/12 PJK Initial version of module; vmcon1 subroutine
  !+ad_hisc               deleted as F90 allows recursive calls
  !+ad_stat  Okay
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraint_variables
  use current_drive_variables
  use maths_library
  use physics_module
  use physics_variables
  use process_output
  use startup_variables

  implicit none

  private
  public :: startup

contains

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine startup(outfile,iprint)

    !+ad_name  startup
    !+ad_summ  Routine to find the minimum auxiliary power required for start-up
    !+ad_type  Subroutine
    !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
    !+ad_auth  P J Knight, CCFE, Culham Science Centre
    !+ad_cont  constr
    !+ad_cont  cudrv1
    !+ad_cont  start1
    !+ad_cont  start2
    !+ad_args  outfile : input integer : Fortran output unit identifier
    !+ad_args  iprint : input integer : switch for writing to output file (1=yes)
    !+ad_desc  This routine finds the minimum auxiliary power required in start-up.
    !+ad_desc  This is accomplished by calling the non-linear optimisation routine
    !+ad_desc  VMCON, so this subroutine simply sets up the equations to be solved.
    !+ad_desc  <P>PROCESS assumes that all the ion temperatures and profile
    !+ad_desc  parameters are identical and utilises charge neutrality in 
    !+ad_desc  order to calculate DENI.
    !+ad_desc  <P>ZEFF is assumed to remain constant, in other words the
    !+ad_desc  ion/electron ratios are invariant.
    !+ad_desc  <P>The most general form for the energy confinement time is :
    !+ad_desc  <PRE>
    !+ad_desc                              ptaue   qtaue                   rtaue
    !+ad_desc                          ____      __
    !+ad_desc  taue =  gtaue + ftaue   dene      te     ( <Paux>  +  alpmw )
    !+ad_desc        ____     __
    !+ad_desc  where DENE and TE are the electron density (10**20 m**-3) and
    !+ad_desc  electron temperature (10 keV) respectively.
    !+ad_desc  </PRE>
    !+ad_desc  The equation defining ignition has the form
    !+ad_desc  <PRE>
    !+ad_desc                       2   s                2   1/2      -3/2
    !+ad_desc   A n   T      =   B n   T    +  P    - C n   T    + D T
    !+ad_desc      e20 e10          e20 e10     aux      e20 e10      e10
    !+ad_desc  ----------
    !+ad_desc     tau
    !+ad_desc        E
    !+ad_desc                =    P         +  P    -  P         +  P
    !+ad_desc                      alpha        aux     rad          ohm
    !+ad_desc  
    !+ad_desc                      p   q                                  r
    !+ad_desc  tau    =  gg  + ff n   T    ( P      + P    + P    + P    )
    !+ad_desc     E                e20 e10    alpha    ohm    rad    ohm
    !+ad_desc  </PRE>
    !+ad_desc  We solve this equation for (ne20, Te10) subject to the constraints,
    !+ad_desc  <PRE>
    !+ad_desc            d P
    !+ad_desc               aux
    !+ad_desc            -----  =  0.
    !+ad_desc            d T
    !+ad_desc               e10
    !+ad_desc  </PRE>
    !+ad_desc  to find the minimum auxiliary power required in start-up.
    !+ad_prob  None
    !+ad_call  oheadr
    !+ad_call  ovarre
    !+ad_call  start1
    !+ad_call  start2
    !+ad_call  vmcon
    !+ad_hist  25/11/93 PJK Incorporation into PROCESS
    !+ad_hist  02/10/12 PJK Initial F90 version
    !+ad_hist  09/10/12 PJK Modified to use new process_output module
    !+ad_hist  15/10/12 PJK Added physics_variables
    !+ad_hist  16/10/12 PJK Added current_drive_variables
    !+ad_hist  31/10/12 PJK Added constraint_variables
    !+ad_hist  06/11/12 PJK Added outfile argument.
    !+ad_hisc               Routine now calls VMCON instead of local version VMCON1.
    !+ad_hist  11/04/13 PJK Removed ires if-statement
    !+ad_hist  11/09/13 PJK Changed ftr to ftrit; N.B. D-T reaction is assumed.
    !+ad_stat  Not currently used
    !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
    !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
    !
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    implicit none

    !  Arguments

    integer, intent(in) :: iprint,outfile

    !  Local variables

    real(kind(1.0D0)) :: aa,bb,cc,dd,ne20,te10,ti10,fd,fdt,rrplas,objf,tol
    integer :: meq,maxfev,info,nfev,mode,s

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

    if (iprint /= 1) then

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
            cnorm,lcnorm,b,lb,tol,maxfev,info,nfev,vlam1,glag,vmu, &
            cm,glaga,gamma1,eta,xa,bdelta,delta,ldel,gm,bdl,bdu, &
            h,lh,wa,lwa,iwa,liwa,ilower,iupper,bndl,bndu)

       auxmin = objf
       nign = x(1)*1.0D20
       tign = x(2)*10.0D0

    else

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

      !+ad_name  start1
      !+ad_summ  Calculates the auxiliary power and the constraint equations
      !+ad_summ  relevant to the minimisation of the auxiliary power
      !+ad_type  Subroutine
      !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  n       : input integer : number of equations
      !+ad_args  m       : input integer : number of constraints
      !+ad_args  x(n)    : input real array : current values of the
      !+ad_argc            density and temperature
      !+ad_args  objf    : output real : auxiliary power (MW)
      !+ad_args  conf(m) : output real array : contraints
      !+ad_args  info    : in/out integer : error status flag
      !+ad_desc  This routine calculates the auxiliary power and the
      !+ad_desc  constraint equations relevant to its minimisation.
      !+ad_prob  None
      !+ad_call  constr
      !+ad_call  cudrv1
      !+ad_hist  25/11/93 PJK Incorporation into PROCESS
      !+ad_hist  02/10/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
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

      !+ad_name  start2
      !+ad_summ  Calculates the first derivative of the auxiliary power
      !+ad_summ  and the constraint equations relevant to the minimisation
      !+ad_summ  of the auxiliary power
      !+ad_type  Subroutine
      !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  n       : input integer : number of equations
      !+ad_args  m       : input integer : number of constraints
      !+ad_args  x(n)    : input real array : current values of the density and temperature
      !+ad_args  fgrd(n) : output real array : first derivative of auxiliary power
      !+ad_args  cnorm(m,lcnorm) : output real array : constraints
      !+ad_args  lcnorm  : input integer : array index
      !+ad_args  info    : in/out integer : error status flag
      !+ad_desc  This routine calculates the first derivative of the
      !+ad_desc  auxiliary power and the constraint equations relevant
      !+ad_desc  to the minimisation of the auxiliary power.
      !+ad_prob  None
      !+ad_call  constr
      !+ad_call  cudrv1
      !+ad_hist  25/11/93 PJK Incorporation into PROCESS
      !+ad_hist  02/10/12 PJK Initial F90 version
      !+ad_hist  10/10/12 PJK Hardwired local value of epsfcn
      !+ad_stat  Okay
      !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
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

      !+ad_name  cudrv1
      !+ad_summ  Routine acting as interface between the start-up routines
      !+ad_summ  and CUDRIV
      !+ad_type  Subroutine
      !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  n       : input integer : number of equations
      !+ad_args  x(n)    : input real array : current values of the density and temperature
      !+ad_args  paux    : output real : auxiliary power (MW)
      !+ad_desc  This routine acts as the interface between the start-up routines
      !+ad_desc  and <A HREF="cudriv.html">CUDRIV</A>.
      !+ad_prob  None
      !+ad_call  physics
      !+ad_call  cudrv1
      !+ad_hist  25/11/93 PJK Incorporation into PROCESS
      !+ad_hist  02/10/12 PJK Initial F90 version
      !+ad_hist  15/10/12 PJK Added physics_variables
      !+ad_hist  16/10/12 PJK Added physics_module
      !+ad_hist  16/10/12 PJK Added current_drive_variables
      !+ad_stat  Okay
      !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
      !
      ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

      paux = (pinje + pinji)*1.0D-6

      !  Reset density and temperature to pre-call values

      dene = storen
      te = storet

      !  Call physics routines again to reset all values

      call physics

    end subroutine cudrv1

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine constr(n,m,x,paux,conf)

      !+ad_name  constr
      !+ad_summ  Calculates the constraint equations relevant to the
      !+ad_summ  minimisation of the auxiliary power
      !+ad_type  Subroutine
      !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
      !+ad_auth  P J Knight, CCFE, Culham Science Centre
      !+ad_cont  None
      !+ad_args  n       : input integer : number of equations
      !+ad_args  m       : input integer : number of constraints
      !+ad_args  x(n)    : input real array : current values of the density and temperature
      !+ad_args  paux    : input real : auxiliary power (MW)
      !+ad_args  conf(m) : output real array : constraints
      !+ad_desc  This routine calculates the constraint equations relevant
      !+ad_desc  to the minimisation of the auxiliary power.
      !+ad_prob  None
      !+ad_call  None
      !+ad_hist  25/11/93 PJK Incorporation into PROCESS
      !+ad_hist  02/10/12 PJK Initial F90 version
      !+ad_stat  Okay
      !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
      !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
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
