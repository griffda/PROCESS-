!  $Id::                                                                $
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine startup(iprint)

  !+ad_name  startup
  !+ad_summ  Routine to find the minimum auxiliary power required for start-up
  !+ad_type  Subroutine
  !+ad_auth  C A Gardner, AEA Fusion, Culham Laboratory
  !+ad_auth  P J Knight, CCFE, Culham Science Centre
  !+ad_cont  constr
  !+ad_cont  cudrv1
  !+ad_cont  start1
  !+ad_cont  start2
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
  !+ad_call  constraint_variables
  !+ad_call  current_drive_variables
  !+ad_call  physics_variables
  !+ad_call  process_output
  !+ad_call  start.h90
  !+ad_call  oheadr
  !+ad_call  ovarre
  !+ad_call  start1
  !+ad_call  start2
  !+ad_call  vmcon1
  !+ad_hist  25/11/93 PJK Incorporation into PROCESS
  !+ad_hist  02/10/12 PJK Initial F90 version
  !+ad_hist  09/10/12 PJK Modified to use new process_output module
  !+ad_hist  15/10/12 PJK Added physics_variables
  !+ad_hist  16/10/12 PJK Added current_drive_variables
  !+ad_hist  31/10/12 PJK Added constraint_variables
  !+ad_stat  Not currently used
  !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  use constraint_variables
  use current_drive_variables
  use physics_variables
  use process_output

  implicit none

  include 'start.h90'

  !  Arguments

  integer, intent(in) :: iprint

  !  Local variables

  real(kind(1.0D0)) :: ne20,te10,ti10,fd,fdt,rrplas,objf,tol
  integer :: meq,maxfev,info,nfev,mode

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

     fd = 1.0D0 - ftr
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
     if (ires == 1) then
        rrplas = rrplas*rpfac
     end if

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

     !  N.B. If the VMCON routine stored in minpac.f is used, then
     !  we have problems with recursive calls. Therefore we must call
     !  an almost identical version, VMCON1, instead.

     call vmcon1(start1,start2,mode,n,m,meq,x,objf,fgrd,conf, &
          cnorm,lcnorm,b,lb,tol,maxfev,info,nfev,vlam1,glag,vmu, &
          cm,glaga,gamma1,eta,xa,bdelta,delta,ldel,gm,bdl,bdu, &
          h,lh,wa,lwa,iwa,liwa,ilower,iupper,bndl,bndu)

     auxmin = objf
     nign = x(1)*1.0D20
     tign = x(2)*10.0D0

  else

     call oheadr(nout,'Start-up')

     call ovarre(nout,'Minimum auxiliary power requirement (MW)', &
          '(auxmin)',auxmin)
     call ovarre(nout,'Start-up electron density (/m3)', &
          '(x(1))',x(1)*1.0D20)
     call ovarre(nout,'Start-up electron temperature (keV)', &
          '(x(2))',x(2)*10.0D0)

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
  !+ad_args  x(n)    : input real array : current values of the density and temperature
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
  !+ad_call  current_drive_variables
  !+ad_call  physics_module
  !+ad_call  physics_variables
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

  use current_drive_variables
  use physics_module
  use physics_variables

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
  !+ad_call  start.h90
  !+ad_hist  25/11/93 PJK Incorporation into PROCESS
  !+ad_hist  02/10/12 PJK Initial F90 version
  !+ad_stat  Okay
  !+ad_docs  Work File Notes F/MPE/MOD/CAG/PROCESS/PULSE
  !+ad_docs  AEA FUS 251: A User's Guide to the PROCESS Systems Code
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  include 'start.h90'

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

!______________________________________________________________________
!+**PJK 25/11/93 The following has been added to help STARTUP run.
!+**PJK 02/10/12 Not yet converted to full F90
!______________________________________________________________________
SUBROUTINE VMCON1( &
     start1,start2,mode,n,m,meq,x,objf,fgrd,conf,cnorm,lcnorm, &
     b,lb,tol,maxfev,info,nfev,vlam,glag,vmu,cm,glaga,gamma,eta, &
     xa,bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa,liwa,ilower, &
     iupper,bndl,bndu)

  !+**PJK 25/11/93 This routine is identical to subroutine VMCON
  !+**PJK 25/11/93 stored in minpac.f, but calls different routines
  !+**PJK 25/11/93 start1,  start2  and qpsub1, instead of
  !+**PJK 25/11/93 fcnvmc1, fcnvmc2 and qpsub, respectively.

  !  This subroutine calculates the least value of a function of
  !  several variables subject to linear and/or nonlinear equality
  !  and inequality constraints.  More particularly, it solves the
  !  problem
  !
  !            minimize f(x)
  !
  !     subject to c (x) =  0.0 ,  i = 1,...,meq
  !                 i
  !
  !            and c (x) >= 0.0 ,  i = meq+1,...,m
  !                 i
  !
  !            and l <= x <= u  ,  i = 1,...n
  !                 i    i    i
  !
  !
  !  The subroutine implements a variable metric method for
  !  constrained optimization developed by M.J.D. Powell.
  !
  !  The subroutine statement is
  !
  !    subroutine vmcon1(start1,start2,mode,n,m,meq,x,objf,fgrd,conf,
  !                     cnorm,lcnorm,b,lb,tol,maxfev,info,
  !                     nfev,vlam,glag,vmu,cm,glaga,gamma,eta,xa,
  !                     bdelta,delta,ldel,gm,bdl,bdu,h,lh,wa,lwa,iwa,
  !                     liwa)
  !
  !  where
  !
  !  START1 is the name of the user supplied subroutine which
  !  calculates the objective and constraint functions. START1
  !  should be declared in an external statement in the user
  !  calling program, and should be written as follows:
  !
  !   subroutine start1(n,m,x,objf,conf,info)
  !   integer n,m,info
  !   real objf
  !   real x(n),fgrd(n),conf(m)
  !   ---------------
  !   statements to calculate the objective and constraint at x.
  !   the objective and constraint functions and must be returned in
  !   objf, conf. note that the equality
  !   constraints must precede the inequality constraints in conf.
  !   ---------------
  !   return
  !   end
  !
  !  START2 is the name of the user supplied subroutine which
  !  calculates the gradients (first derivative vectors)
  !  of the objective and constraint functions. START2 should be
  !  declared in an external statement in the user calling
  !  program, and should be written as follows :
  !
  !   subroutine start2(n,m,x,fgrd,cnorm,lcnorm,info)
  !   integer n,m,lcnorm,info
  !   real objf
  !   real x(n),cnorm(lcnorm,m)
  !   ---------------
  !   statements to calculate the gradients of the objective and
  !   constraint functions at x. the gradient of the objective
  !   function must be returned in fgrd. note that the equality
  !   constraints must precede the inequality constraints in conf.
  !   the constraint gradients or normals must be returned as the
  !   columns of cnorm.
  !   ---------------
  !   return
  !   end
  !
  !  The value of INFO should not be changed by START2 unless the
  !  user wants to terminate execution of VMCON1. In this case
  !  set INFO to a negative integer.
  !
  !  MODE is a non-negative integer input variable set to 1 if the
  !  second derivative matrix in b (see below) is provided by the
  !  user, and to 0 if it is to be initialized to the identity
  !  matrix.
  !
  !  N is a positive integer input variable set to the number of
  !  variables.
  !
  !  M is a positive integer input variable set to the number of
  !  constraints.
  !
  !  MEQ is a non-negative integer input variable set to the number
  !  of equality constraints. MEQ must be less than or equal to N.
  !
  !  X is a real array of length N. On input it must contain an
  !  initial estimate of the solution vector. On output X
  !  contains the final estimate of the solution vector.
  !
  !  OBJF is a real output variable that contains the value of the
  !  objective function at the output x.
  !
  !  FGRD is a real output array of length N which contains the
  !  components of the gradient of the objective function at
  !  the output X.
  !
  !  CONF is a real output array of length M which contains the
  !  values of the constraint functions at the output X. The
  !  equality constraints must precede the inequality constraints.
  !
  !  CNORM is a real LCNORM by M array whose columns contain the
  !  constraint normals at the output X in the first N positions.
  !
  !  LCNORM is a positive integer input variable set to the row
  !  dimension of CNORM which is at least N+1.  The (N+1)st row
  !  of CNORM is used for work space.
  !
  !  B is a real LB by LB array whose first N rows and columns
  !  contain the approximation to the second derivative matrix
  !  of the Lagrangian function. Often, an adequate initial
  !  B matrix can be obtained by approximating the hessian
  !  of the objective function.  On input, the approximation is
  !  provided by the user if MODE = 1 and is set to the identity
  !  matrix if MODE = 0. the (N+1)st row and column are used for
  !  work space.
  !
  !  LB is a positive integer input variable set to the row
  !  dimension of B which is at least N+1.
  !
  !  TOL is a non-negative input variable. A normal return occurs
  !  when the objective function plus suitably weighted multiples
  !  of the constraint functions are predicted to differ from
  !  their optimal values by at most TOL.
  !
  !  MAXFEV is a positive integer input variable set to the limit
  !  on the number of calls to START1.
  !
  !  INFO is an integer output variable set as follows
  !
  !   if INFO is negative then user termination. otherwise
  !
  !   INFO = 0  improper input parameters. Tests are made to ensure
  !             that N and M are positive, TOL is non-negative,
  !             MEQ is less than or equal to N, and that LCNORM,
  !             LB, LDEL, LH, LWA, and LIWA are sufficiently large.
  !
  !   INFO = 1  a normal return. see description of TOL.
  !
  !   INFO = 2  number of calls to START1 is at least MAXFEV.
  !
  !   INFO = 3  line search required ten calls of START1.
  !
  !   INFO = 4  uphill search direction was calculated.
  !
  !   INFO = 5  quadratic programming technique was unable to find
  !             a feasible point.
  !
  !   INFO = 6  quadratic programming technique was restricted by
  !             an artificial bound or failed due to a singular
  !             matrix.
  !
  !  NFEV is an integer output variable set to the number of calls
  !  to START1.
  !
  !  VLAM is a real output array of length M+2N+1 which contains
  !  the Lagrange multipliers at the output X.  The Lagrange
  !  multipliers provide the sensitivity of the objective
  !  function to changes in the constraint functions.
  !  note that VLAM(M+I), I=1,...,N gives the multipliers for
  !  the lower bound constraints.  VLAM(M+N+1+I), I=1,...,N
  !  gives the multipliers for the upper bound constraints.
  !
  !  GLAG is a real output array of length N which contains the
  !  components of the gradient of the Lagrangian function at
  !  the output x.
  !
  !  CM is a real work array of length M.
  !
  !  VMU is a real work array of length M+2N+1.
  !
  !  GLAGA, GAMMA, ETA, XA, BDELTA are real work arrays of
  !  length N.
  !
  !  DELTA is a real work array of length LDEL.
  !
  !  LDEL is a positive integer input variable set equal to the
  !  length of DELTA which is at least MAX(7*(N+1),4*(N+1)+M).
  !
  !  GM, BDL, BDU are real work arrays of length N+1.
  !
  !  H is a real LH by LH work array.
  !
  !  LH is a positive integer input variable set to the dimension
  !  of the square array H which is at least 2*(N+1).
  !
  !  WA is a real work array of length LWA.
  !
  !  LWA is a positive integer input variable set equal to the
  !  dimension of WA which is at least 2*(N+1).
  !
  !  IWA is an integer work array of length LIWA.
  !
  !  LIWA is a positive integer input variable set equal to the
  !  dimension of IWA which is at least 6*(N+1) + M.
  !
  !  ILOWER is an integer array of length N.
  !  If X(I) has a lower bound, ILOWER(I) is set to 1
  !  on input.  If no bound is provided, ILOWER(i) should
  !  be 0 (the default value).
  !
  !  BNDL is a real array of length N.
  !  If X(I) has a lower bound, it should be given in BNDL(I).
  !
  !  IUPPER is an integer array of length N.
  !  If X(I) has an upper bound, IUPPER(I) is set to 1
  !  on input.  If no bound is provided, IUPPER(I) should
  !  be 0 (the default value).
  !
  !  BNDU is a real array of length N.
  !  If X(I) has a upper bound, it should be given in BNDU(I).
  !
  !  Algorithm version of June 1979.
  !
  !  Roger L. Crane, Kenneth E. Hillstrom, Michael Minkoff
  !  Modified for simple bounds, M. Minkoff (10/26/82)
  !  Modified for separate function and gradient evaluation,
  !  M. Minkoff (11/18/83)
  !  Modified to pass ILOWER, IUPPER,BNDL,BDNU through argument
  !  list instead of through common block, and remove write
  !  capability J. Galambos (5/21/91)
  !  Modified for separate START1 and START2 functions to avoid passing
  !  info via a common block. J. Galambos (5/21/91)

  use maths_library

  IMPLICIT NONE

  INTEGER mode,n,m,meq,lcnorm,lb,maxfev,info,nfev,ldel,lh,lwa,liwa
  INTEGER iwa(liwa),ilower(n),iupper(n)
  INTEGER i,j,k,mact,nfinit,nls,np1,np1j,npp,nqp,nsix,nsixi
  INTEGER inx,ki,ml,mlp1,mcon,mp1,mpn,mpnpp1,mpnppn

  real(kind(1.0D0)) objf,tol
  real(kind(1.0D0)) x(n),fgrd(n),conf(m),cnorm(lcnorm,m),b(lb,lb), &
       vlam(*),glag(n),vmu(*),cm(m),glaga(n),gamma(n), &
       eta(n),xa(n),bdelta(n),delta(ldel),gm(*), &
       bdl(*),bdu(*),h(lh,lh),wa(lwa),bndl(n),bndu(n)
  real(kind(1.0D0)) alpha,aux,auxa,calpha,cp1,cp2,cp5,dbd,dflsa,dg, &
       fls,flsa,one,spgdel,sum,temp,thcomp,theta,zero

  EXTERNAL start1,start2

  zero = 0.0D0
  cp1 = 0.1D0
  cp2 = 0.2D0
  cp5 = 0.5D0
  one = 1.0D0

  np1 = n + 1
  npp = 2*np1
  info = 0

  !  Check input parameters for errors

  if ( &
       (n.le.0)           .or. &
       (m.le.0)           .or. &
       (meq.gt.n)         .or. &
       (lcnorm.lt.(n+1))  .or. &
       (lb.lt.(n+1))      .or. &
       (tol.lt.zero)      .or. &
       (ldel.lt.max(7*(n+1),4*(n+1)+m)).or. &
       (lh.lt.(2*(n+1)))  .or. &
       (lwa.lt.(2*(n+1))) .or. &
       (liwa.lt.(6*(n+1)+m)) &
       ) goto 450

  !  Set the initial elements of b and vmu. vmu is the weighting
  !  vector to be used in the line search.
  !  Use hessian estimate provided by user if mode = 1 on input

  if (mode .eq. 1) goto 25

  !  Use identity matrix for hessian estimate

  do j = 1, n
     do i = 1, n
        b(i,j) = zero
     end do
     b(j,j) = one
  end do

25 continue

  !  Set m + 1 to mp1
  !  Set m + n to mpn (these are limits for lower bound indices)
  !  Set m + np1 + 1 to mpnpp1
  !  Set m + np1 + n to mpnppn (these are limits for upper bound
  !  indices)

  mp1 = m + 1
  mpn = m + n
  mpnpp1 = m + np1 + 1
  mpnppn = m + np1 + n

  !  Set mcon to total number of actual constraints

  mcon = m
  do i = 1, n
     if (ilower(i) .eq. 1) mcon = mcon + 1
  end do

  !  Set ml to m + number of lower bounds

  ml = mcon

  !  Set mlp1 to ml + 1

  mlp1 = ml + 1
  do i = 1, n
     if (iupper(i) .eq. 1) mcon = mcon + 1
  end do
  do k = 1, mpnppn
     vmu(k) = zero
  end do

  !  Set initial values of some variables
  !  nfev is the number of calls of start1
  !  nsix is the length of an array
  !  nqp is the number of quadratic subproblems

  nfev = 1
  nsix = 6*np1
  nqp = 0

  !  Calculate the initial functions and gradients

  call start1(n,m,x,objf,conf,info)

  if (info .lt. 0) goto 450

  call start2(n,m,x,fgrd,cnorm,lcnorm,info)

  if (info .lt. 0) goto 450

  !  Start the iteration by calling the quadratic programming
  !  subroutine

40 continue

  !  Increment the quadratic subproblem counter

  nqp = nqp + 1

  !  Set the linear term of the quadratic problem objective function
  !  to the negative gradient of objf

  do i = 1, n
     gm(i) = -fgrd(i)
  end do
  do i = 1, mpnppn
     vlam(i) = zero
  end do

  !+**PJK 15/11/11      call qpsub1(
  call qpsub( &
       n,m,meq,conf,cnorm,lcnorm,b,lb,gm,bdl,bdu,info,x,delta, &
       ldel,cm,h,lh,mact,wa,lwa,iwa,liwa,ilower,iupper, &
       bndl,bndu)

  !  The following return is made if the quadratic problem solver
  !  failed to find a feasible point, if an artificial bound was
  !  active, or if a singular matrix was detected

  if ((info .eq. 5) .or. (info .eq. 6)) goto 450

  !  Initialize the line search iteration counter

  nls = 0

  !  Calculate the Lagrange multipliers

  do j = 1, mact
     k = iwa(j) - npp
     if (k .gt. 0) goto 59
     ki = iwa(j)
     k = ki + m
     if (ki .gt. np1) goto 58
     if (ki .eq. np1) goto 70
     if (ilower(ki) .eq. 1) goto 59
     goto 70
58   continue
     ki = iwa(j) - np1
     k = ki + m + np1
     if (ki .eq. np1) goto 70
     if (iupper(ki) .eq. 1) goto 59
     goto 70
59   continue
     do i = 1, n
        np1j = np1 + j
        nsixi = nsix + i
        vlam(k) = vlam(k) + h(np1j,i)*delta(nsixi)
     end do
70   continue
  end do

  !  Calculate the gradient of the Lagrangian function
  !  nfinit is the value of nfev at the start of an iteration

  nfinit = nfev
  do i = 1, n
     glag(i) = fgrd(i)
  end do
  do k = 1, m
     if (vlam(k) .eq. zero) goto 100
     do i = 1, n
        glag(i) = glag(i) - cnorm(i,k)*vlam(k)
     end do
100  continue
  end do
  do k = mp1, mpn
     if (vlam(k) .eq. zero) goto 105
     inx = k - m
     if (ilower(inx) .eq. 0) goto 105
     glag(inx) = glag(inx) - vlam(k)
105  continue
  end do
  do k = mpnpp1, mpnppn
     if (vlam(k) .eq. zero) goto 106
     inx = k - m - np1
     if (iupper(inx) .eq. 0) goto 106
     glag(inx) = glag(inx) + vlam(k)
106  continue
  end do

  !  Set spgdel to the scalar product of fgrd and delta
  !  Store the elements of glag and x

  spgdel = zero
  do i = 1, n
     spgdel = spgdel + fgrd(i)*delta(i)
     glaga(i) = glag(i)
     xa(i) = x(i)
  end do

  !  Revise the vector vmu and test for convergence

  sum = abs(spgdel)
  do k = 1, mpnppn
     aux = abs(vlam(k))
     vmu(k) = max(aux,cp5*(aux + vmu(k)))
     temp = 0.00D0
     if (k .gt. m) goto 111
     temp = conf(k)
     goto 119
111  continue
     if (k .gt. mpn) goto 112
     inx = k - m
     if (ilower(inx) .eq. 0) goto 120
     temp = x(inx) - bndl(inx)
     goto 119
112  continue
     inx = k - m - np1
     if ((inx .eq. 0) .or. (inx .gt. n)) goto 120
     if (iupper(inx) .eq. 0) goto 120
     temp = bndu(inx) - x(inx)
119  continue
     sum = sum + abs(vlam(k)*temp)
120  continue
  end do

  !  Exit if convergence criterion is satisfied

  if (sum .le. tol) goto 450

  !  Set sum to the weighted sum of infeasibilities
  !  Set fls to the line search objective function

130 continue

  !  Increment the line search iteration counter

  nls = nls + 1
  sum = zero
  do k = 1, mpnppn
     aux = 0.0D0
     if (k .le. meq) aux = conf(k)
     temp = 0.0D0
     if (k .gt. m) goto 131
     temp = conf(k)
     goto 139
131  continue
     if (k .gt. mpn) goto 132
     inx = k - m
     if (ilower(inx) .eq. 0) goto 140
     temp = x(inx) - bndl(inx)
     goto 139
132  continue
     inx = k - m - np1
     if ((inx .eq. 0) .or. (inx .gt. n)) goto 140
     if (iupper(inx) .eq. 0) goto 140
     temp = bndu(inx) - x(inx)
139  continue
     sum = sum + vmu(k)*max(aux,-temp)
140  continue
  end do
  fls = objf + sum

  if (nfev .ne. nfinit) goto 150

  !  Set the initial conditions for the line search
  !  flsa is the initial value of the line search function
  !  dflsa is its first derivative (if delta(np1) = 1)
  !  alpha is the next reduction in the step-length

  flsa = fls
  dflsa = spgdel - delta(np1)*sum
  if (dflsa .ge. zero) goto 420

  !  Set initial multiplying factor for stepsize
  !  Set initial value of stepsize for output

  alpha = one
  calpha = one
  goto 210

  !  Test whether line search is complete

150 continue
  aux = fls - flsa

  !  Exit line search if function difference is small

  if (aux .le. (cp1*dflsa)) goto 260

  !  Exit if the line search requires ten or more function
  !  evaluations

  if (nfev .ge. (nfinit + 10)) goto 380

  !  Calculate next reduction in the line step assuming a quadratic
  !  fit.

  alpha = max(cp1,cp5*dflsa/(dflsa - aux))

  !  Multiply delta by alpha and calculate the new x

210 continue
  calpha = alpha*calpha

  do i = 1, n
     delta(i) = alpha*delta(i)
     x(i) = xa(i) + delta(i)
  end do

  dflsa = alpha*dflsa

  !  Test nfev against maxfev, call start1 and resume line search

  if (nfev .ge. maxfev) goto 380
  nfev = nfev + 1

  call start1(n,m,x,objf,conf,info)

  if (info .lt. 0) goto 450
  goto 130

  !  Line search is complete. Calculate gradient of Lagrangian
  !  function for use in updating hessian of Lagrangian

260 continue

  call start1(n,m,x,objf,conf,info)
  call start2(n,m,x,fgrd,cnorm,lcnorm,info)

  if (info .lt. 0) goto 450
  do i = 1, n
     glag(i) = fgrd(i)
  end do
  do k = 1, m
     if (vlam(k) .eq. zero) goto 290
     do i = 1, n
        glag(i) = glag(i) - cnorm(i,k)*vlam(k)
     end do
290  continue
  end do
  do k = mp1, mpn
     if (vlam(k) .eq. zero) goto 291
     inx = k - m
     if (ilower(inx) .eq. 0) goto 291
     glag(inx) = glag(inx) - vlam(k)
291  continue
  end do
  do k = mpnpp1, mpnppn
     if (vlam(k) .eq. zero) goto 292
     inx = k - m - np1
     if (iupper(inx) .eq. 0) goto 292
     glag(inx) = glag(inx) + vlam(k)
292  continue
  end do

  !  Calculate gamma and bdelta in order to revise b
  !  Set dg to the scalar product of delta and gamma
  !  Set dbd to the scalar product of delta and bdelta

  dg = zero
  dbd = zero
  do i = 1, n
     gamma(i) = glag(i) - glaga(i)
     bdelta(i) = zero
     do j = 1, n
        bdelta(i) = bdelta(i) + b(i,j)*delta(j)
     end do
     dg = dg + delta(i)*gamma(i)
     dbd = dbd + delta(i)*bdelta(i)
  end do

  !  Calculate the vector eta for the b-f-g-s formula
  !  replace dg by the scalar product of delta and eta

  aux = cp2*dbd
  theta = one
  if (dg .lt. aux) theta = (dbd - aux)/(dbd - dg)
  thcomp = one - theta
  do i = 1, n
     eta(i) = theta*gamma(i) + thcomp*bdelta(i)
  end do
  if (dg .lt. aux) dg = aux

  !  Revise the matrix b and begin new iteration

  do i = 1, n
     aux = bdelta(i)/dbd
     auxa = eta(i)/dg
     do j = i, n
        b(i,j) = b(i,j) - aux*bdelta(j) + auxa*eta(j)
        b(j,i) = b(i,j)
     end do
  end do
  goto 40

  !  Error returns. restore previous solution

380 continue
  do i = 1, n
     x(i) = xa(i)
  end do
  if (nfev .ge. maxfev) goto 400
  nfev = nfev + 1

  call start1(n,m,x,objf,conf,info)

  if (info .lt. 0) goto 450
  goto 410

  !  Error return because there have been maxfev calls of start1

400 continue
  info = 2
  goto 450

  !  Error return because line search required 10 calls of start1

410 continue
  info = 3
  goto 450

  !  Error return because uphill search direction was calculated

420 continue
  info = 4

450 continue

  return
end SUBROUTINE VMCON1
