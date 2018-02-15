  !       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  !       module e3m
  
  !+ad_name  e3m
  !+ad_summ  MHD Equilibrium Solver
  !+ad_type  Module
  !+ad_auth  ""
  !+ad_cont  EMEQ
  !+ad_cont  EQAB3
  !+ad_cont  EQGB3
  !+ad_cont  EQK3
  !+ad_cont  EQLVU3 
  !+ad_cont  EQC1
  !+ad_cont  EQPPAB
  !+ad_args  N/A
  !+ad_desc  This module contains the routines that solve the MHD equilibrium equation for...
  !+ad_prob  None
  !+ad_call  ""
  !+ad_call  ""
  !+ad_hist  31/10/16  Documenting the initial version
  !+ad_stat  Okay
  !+ad_docs L.E. Zakharov,"Method of Electrodynamic Moments for Equilibrium Calculations in a Toroidal Plasma", Preprint of the Kurchatov Institute of Atomic Energy IAE-4114/6, Moscow (1985)
  !
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  
  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !+ad_name EMEQ
  !+ad_summ ""
  ! INPUT
  !+ad_args BA(1:NA1)[MA/m^2]     : input real array : flux function in front of R/r on the rhs of G-Sh. eq.
  !+ad_args BB(1:NA1)[MA/m^2]     : input real array : flux function in front of (r/R-R/r) - " - " - " - " -
  !+ad_args BR00 [m]              : input real : R geom. center of the edge magnetic surface
  !+ad_args SA0  [m]              : input real : a_{edge} - "a" for the edge magnetic surface
  !+ad_args GL0  [d/l]            : input real : elongation of the edge magnetic surface
  !+ad_args GD30 [m]              : input real : triangularity of the edge magnetic surface (\delta(a_{edge}))
  !+ad_args NA1 [d/l]             : input integer :: : # of grid points
  !+ad_args ACC	                : input real : relative accuracy      ! ACEQLB
  !+ad_args B0T  [T]              : input real : vacuum toroidal magnetic field at the point r=BR00
  !+ad_args PLCUR[MA]             : input real : plasma current
  ! OUTPUT
  !+ad_args GR(1:NA1) [m]         : output real array : \rho(a)
  !+ad_args GBD(1:NA1) [m]        : output real array : \Delta(a)
  !+ad_args GL(1:NA1) [d/l]       : output real array : \lambda(a)
  !+ad_args GSD(1:NA1) [d/l]      : output real array : \delta(a)/[a_{edge}(a/a_{edge})^2]
  !+ad_args GRA(1:NA1) [d/l]      : output real array : <(\nabla a)^2>
  !+ad_args SQGRA                 : output real :  <\sqrt{|g^{11}|}>	= <|nabla(a)|>
  !+ad_args GRAR(1:NA1) [1/m^2]   : output real array : <[(\nabla a)/r]^2>
  !+ad_args AVR2(1:NA1) [1/m^2]   : output real array : <[1/r]^2>
  !+ad_args avsqg                 : output real : \prti{V}{a}/(4\pi^2)
  !+ad_args Vol                   : output real :  V(a)
  !+ad_args B2B0EQ                : output real :  <B**2/B0**2>
  !+ad_args B0B2EQ                : output real :  <B0**2/B**2>
  !+ad_args BMAXEQ                : output real :  BMAXT
  !+ad_args BMINEQ                : output real :  BMINT
  !+ad_args BMODEQ                : output real :  <B/BTOR>
  !+ad_args FOFBEQ                : output real :  <(BTOR/B)**2*(1.-SQRT(1-B/Bmax)*(1+.5B/Bmax))>
  !+ad_args GRDAEQ                : output real :  <grad a>
  !+ad_args TIME                  : output real : 
  !+ad_desc Everywhere above 
  !+ad_desc (i)  the same equidistant grid with respect to the "radial" 
  !+ad_desc      variable "a" is assumed to be used both for input and output.
  !+ad_desc (ii) average <f> is an [0.5/\pi] integral over the poloidal angle \tau 
  !+ad_desc      of the quantity [f\sqrt{g}], where g_{ij} is the metric tensor 
  !+ad_desc      of {a,\tau,\zeta}.
  !+ad_desc (iii) r is the current polar radius (the distance to the major axis)
  !+ad_desc
  !+ad_prob None
  !+ad_call None
  !+ad_hist 31/10/16  Documenting the initial version
  !+ad_stat Okay
  !+ad_docs None
  
  subroutine EMEQ &
    ! Input:
    (BA,BB, &  ! j_zeta = BA*(R00/r) + BB*(r/R00-R00/r)
         BR00, &	! R00 = R_0+\Delta_edge		! RTOR+SHIFT
         SA0, &	! a_edge			! ABC
         GL0, &	! \lambda_edge			! ELONG
         GD30, &	! \delta_edge			! TRIAN*ABC
         NA1, &	! radial grid point No.		! N3EQL
         ACC, &	! relative accuracy		! ACEQLB
         B0T, &	! B_tor_vac at BR00		! BTOR*RTOR/(RTOR+SHIFT)
         PLCUR, &	! Total plasma current		! IPL
                                ! Output:
         GR, &	! \Rho(a)
         GBD, &	! \Delta(a)
         GL, &	! \lambda(a)
         GSD, &	! \delta(a)
         GRA, &	! <g^{11}>		= <[nabla(a)]^2>
         SQGRA, &	! <\sqrt{|g^{11}|}>	= <|nabla(a)|>
         GRAR, &! <g^{11}g^{33}>	= <[nabla(a)/r]^2>
         AVR2, &	! <g^{33}>		= <1/r^2>=G33/RTOR**2
         AI0, &	! I				-> IPOL*RTOR*BTOR
         dgrda, &	! \prti{\rho}{a}
         avsqg, &	! \prti{V}{a}/(4\pi^2)
         Vol, &	! V(a)
         B2B0EQ, & ! <B**2/B0**2>
         B0B2EQ, & ! <B0**2/B**2>
         BMAXEQ, & ! BMAXT
         BMINEQ, & ! BMINT
         BMODEQ, & ! <B/BTOR>
         FOFBEQ, & ! <(BTOR/B)**2*(1.-SQRT(1-B/Bmax)*(1+.5B/Bmax))>
         GRDAEQ,TIME)! <grad a>
    ! - BR00,SA0 - MAJOR & MINOR RADII /METER/
    ! - GLO,GD3O - ELONGATION AND TRIANGULARITY /BOUNDARY VALUE/
    ! - GSD = TRIANGULARITY
    ! 	\Delta' =WDSD1(I)*WSA(I)
    ! 	\lambda'=WDGL(I)*WGL(I)*WSAA(I)
    ! 	\delta' =WDSD3(I)*WSA(I)
    
    implicit none
    include 'emeq.inc'
    integer ::	NA1,NAOLD,NA,NT,NT1,I,I1,J,K,NITER
    real(kind(1.0d0)) ::	BR00,SA0,GL0,GD30,ACC,B0T,PLCUR,TIME
    real(kind(1.0d0)) ::	BA(NA1),BB(NA1),GR(NA1),GBD(NA1),GL(NA1) &
    ,GSD(NA1),GRA(NA1),SQGRA(NA1),GRAR(NA1),AVR2(NA1) &
    ,AI0(NA1),dgrda(NA1),avsqg(NA1),Vol(NA1),GR2AUX(NA1)
    real(kind(1.0d0)) ::	B2B0EQ(NA1),B0B2EQ(NA1),BMAXEQ(NA1), &
    BMINEQ(NA1),BMODEQ(NA1),FOFBEQ(NA1),GRDAEQ(NA1), &
    A,AA,C,CC,S,SS,SR,SX,SX1,T,Y,Y1,SDT,SDT0, &
    DRDA,DZDA,DRDT,DZDT,DMETR,DA2,DGR2,FI,FJ,D0,CGP, &
    GP,GP2,GR2,GLOLD,G3DOLD,AOLD,G22A2,SKGGG, &
    SQG,YLIN,YVOL,YMIN,YMAX &
    ,SKDR,SKGA,SQG22R
!    save AOLD,GLOLD,G3DOLD,NAOLD,NITER!,cgp
    common /EMEQMR/SKDR(NP),SKGA(NP),SQG22R(NP)
    data AOLD/0.d0/GLOLD/0.d0/G3DOLD/0.d0/NAOLD/1/
    data NITER/60/ !cgp/4.0d0*datan(1.0d0)/ !/3.14159265359d0/	
    
    !*************************************************
    ! 	if (TIME .gt. .1)	then
    ! 	write(*,*)BR00	! R_0+\Delta_edge		! RTOR+SHIFT
    ! 	write(*,*)SA0	! a_edge			! ABC	   
    ! 	write(*,*)GL0	! \lambda_edge			! ELONG	   
    ! 	write(*,*)GD30	! \delta_edge			! TRIAN*ABC0
    ! 	write(*,*)NA1
    ! 	write(*,*)B0T	! B_tor_vac @ BR00	 ! B_0*R_0/(R_0+SHIFT)
    ! 	write(*,*)PLCUR	! Total plasma current		! IPL
    ! 	write(*,'(2(E14.6))')(BA(j),BB(j),j=1,NA1)
    ! 	stop
    ! 	endif
    !**************************************************

    GP=4.0d0*datan(1.0d0) !3.1415926

    WBBS0=B0T
    WBJ0=0.2*PLCUR
    NA=NA1-1
    NT=12*MAX(1,NA1/8)
    ! 		Set initial conditions / zero iteration
    if (               NAOLD .eq. NA &
    .and. abs(AOLD-SA0)    .lt. 1.E-6 &
    .and. abs(GLOLD-GL0)   .lt. 1.E-6 &
    .and. abs(G3DOLD-GD30) .lt. 1.E-6 &
    )	goto	4
    !      call add2loc("Calling EQGB3"//char(0))
    !      call EQGB3(BR00,SA0,GL0,GD30,NA)
    call EQGB3(BR00,SA0,GL0,GD30,NA,WBR0,WBR00, &
    WSA,WSAA,WGL,WSD3,WDSD3, &
    WSCI1,WSCI3,WSCI5,WSCI7,WSCJ1,WSCJ3,WSCJ5,WSCJ7)
    AOLD=SA0
    GLOLD=GL0
    G3DOLD=GD30
    NAOLD=NA
    WBR00=BR00
4   continue
    do  I=1,NA1
       WSJP(I)=BA(I)
       WSP(I)=BB(I)
    enddo
    NITER=30                    ! Use 60 for the 1st entry only
    !      call add2loc("Calling EQAB3"//char(0))
    call EQAB3(NA,NT,NITER,ACC) ! Call MEM equil solver
    if (NA .le. 1)	then
       NA1 = NA
       goto 31
       return
    endif
    !      call add2loc("Calling EQPPAB"//char(0))
    call EQPPAB(NA)
    D0=WSD1(NA1)*WSAA(NA1)
    GR(1)=0.
    Vol(1)=0.
    GR2=0.
    fi=0.
    !s=4.*cgp*cgp
    s=4.*GP*GP 
    do I=1,NA1
       Vol(i)=s*WSAA(i)*(WBR0*wsl0(i)+WSAA(i)*wsl1(i))
       j=i-1
       fj=fi
       AI0(I)=WBF(I)*WBR0
       sqg=wbg0(i)*wgl(i)*wbr0
       avsqg(i)=WSA(i)*sqg
       GRA(I)	=SKGA(I)/sqg
       SQGRA(I)=SQG22R(I)/sqg
       GRAR(I)=wbg22(i)/(wgl(i)*wbr0*sqg)
       AVR2(I)=wbg33(i)*wgl(i)/(wbr0*sqg)
       GL(I)=WGL(I)
       GBD(I)=D0-WSD1(I)*WSAA(I)
       GSD(I)=WSD3(I)*WSAA(i)
       fi=wbg33(i)*wgl(i)*AI0(I)/(wbr0*WBBS0)
       if (I.gt.1) then
          DGR2=fj*WSCJ1(j)+fi*WSCI1(j)
          GR2=GR2+DGR2
          if (DGR2.le.0.)	then
             write(*,*)"NA1 = 0",GR2,DGR2
             NA1 = 0
             goto 31
             return
          endif
          GR(I)=SQRT(GR2*2.)
          dgrda(i)=fi*WSA(i)/gr(i)
       else
          dgrda(i)=sqrt(fi)
       endif
    enddo
    !MR extra quantities
    GR2AUX(1)=0.
    !moved above 17 GP=4.0d0*datan(1.0d0) !3.1415926
    GP2=2.*GP
    NT1=NT+1
    SDT0=1./NT
    YLIN=0.
    YVOL=0.
    DO I=1,NA1
       A=WSA(I)
       AA=A*A
       B2B0EQ(I)	=0.
       B0B2EQ(I) 	=0.
       BMODEQ(I) 	=0.
       FOFBEQ(I) 	=0.
       GRDAEQ(I)	=0.
       SKGGG		=0.
       YMIN		=99999.
       YMAX		=0.
       AI0(I)=WBF(I)*WBR0
       IF(I.GT.1) THEN
          K=I-1
          GR2AUX(I)=GR2AUX(K)+(SKDR(K)*AI0(K)*WSCJ1(K)+ &
          SKDR(I)*AI0(I)*WSCI1(K))*2./WBBS0
       ENDIF !CORRECTED  20161115
       ! Need to calculate Ymax and Ymin, before doing flux surface averages
       !                                        ( Y=B^2 )
       DO K=1,NT1
          T=SDT0*GP*(K-1)
          SDT=SDT0
          IF(K.EQ.1.OR.K.EQ.NT1) SDT=SDT0*0.5
          C=COS(T)
          S=SIN(T)
          SS=S*S
          CC=1.-SS
          SX1=-WSD1(I)-WSD3(I)*SS
          SX=C+A*SX1
          SR=WBR0+A*SX
          ! R,Z derivatives
          DRDA=-WDSD1(I)*A+C-WDSD3(I)*A*SS
          DZDA=S*WGL(I)*(AA*WDGL(I)+1)
          DRDT=-A*S-2.*AA*WSD3(I)*C*S	
          DZDT=WGL(I)*A*C
          ! metric tensor components
          DMETR=DRDA*DZDT-DRDT*DZDA
          SKGGG=SKGGG+DMETR*SR*SDT
          if(I.NE.1)then
             I1=I-1
             Y=(AI0(I)/SR/WBBS0)**2 + (DRDT**2+DZDT**2)* &
             ((GR2AUX(I)-GR2AUX(I1))/(WSA(I)-WSA(I1))/ &
             (WSQ(I)+WSQ(I1))/SR/DMETR)**2
          else
             Y=(AI0(I)/SR/WBBS0)**2
          endif
          YMAX=MAX(Y,YMAX)
          YMIN=MIN(Y,YMIN)
       ENDDO!CORRECTED  20161115
       DO K=1,NT1
          T=SDT0*GP*(K-1)
          SDT=SDT0
          IF(K.EQ.1.OR.K.EQ.NT1) SDT=SDT0*0.5
          C=COS(T)
          S=SIN(T)
          SS=S*S
          CC=1.-SS
          SX1=-WSD1(I)-WSD3(I)*SS
          SX=C+A*SX1
          SR=WBR0+A*SX
          ! R,Z derivatives
          DRDA=-WDSD1(I)*A+C-WDSD3(I)*A*SS
          DZDA=S*WGL(I)*(AA*WDGL(I)+1)
          DRDT=-A*S-2.*AA*WSD3(I)*C*S
          DZDT=WGL(I)*A*C
          !  G22/A**2
          G22A2=SS+4.*A*WSD3(I)*SS*C+(2.*A*WSD3(I)*S*C)**2+ &
          (WGL(I)*C)**2
          ! D**2/A**2
          DA2=(C-A*(WDSD1(I)+WDSD3(I)*SS))*WGL(I)*C+ &
          WGL(I)*SS*(WDGL(I)*AA+1.)*(1.+2.*A*C*WSD3(I))
          DA2=DA2*DA2
          ! metric tensor components
          DMETR =DRDA*DZDT-DRDT*DZDA
          if(I.NE.1)then
             I1=I-1
             Y=(AI0(I)/SR/WBBS0)**2 + (DRDT**2+DZDT**2)* &
             ((GR2AUX(I)-GR2AUX(I1))/(WSA(I)-WSA(I1))/ &
             (WSQ(I)+WSQ(I1))/SR/DMETR)**2
             B2B0EQ(I)=B2B0EQ(I)+ Y*DMETR*SR*SDT
             B0B2EQ(I)=B0B2EQ(I)+ DMETR*SR*SDT/Y
             BMODEQ(I)=BMODEQ(I)+ SQRT(Y)*DMETR*SR*SDT
             IF (Y.GT.YMAX) YMAX=Y
             Y1 = SQRT(Y/YMAX)
             FOFBEQ(I)=FOFBEQ(I)+1./Y*(1.-SQRT(abs(1.-Y1)) &
             *(1.+.5*Y1))*DMETR*SR*SDT
             GRDAEQ(I)=GRDAEQ(I)+ SQRT(G22A2/DA2)*DMETR*SR*SDT
             YLIN	=YLIN+(DRDT**2+DZDT**2)* &
             ((GR2AUX(I)-GR2AUX(I1))/(WSA(I)-WSA(I1))/ &
             (WSQ(I)+WSQ(I1))/SR/DMETR)**2*DMETR*SR*SDT
             yvol = yvol + DMETR*SR*SDT
          else
             Y=(AI0(I)/SR/WBBS0)**2
             B2B0EQ(I)=B2B0EQ(I)+Y*SDT
             B0B2EQ(I)=B0B2EQ(I)+ SDT/Y
             BMODEQ(I)=BMODEQ(I)+ SQRT(Y)*SDT
             IF (Y.GT.YMAX) YMAX=Y
             Y1 = SQRT(Y/YMAX)
             Y1 = 1.d0/Y*(1.d0-SQRT(abs(1.d0-Y1))*(1.d0+.5d0*Y1))*SDT
             FOFBEQ(I)=FOFBEQ(I)+Y1
             GRDAEQ(I)=GRDAEQ(I)+SQRT(G22A2/DA2)*SDT
          endif
       ENDDO !CORRECTED  20161115
       BMAXEQ(I)=SQRT(YMAX)*WBBS0
       BMINEQ(I)=SQRT(YMIN)*WBBS0
       if(I.NE.1)then
          B0B2EQ(I)=B0B2EQ(I)/SKGGG
          B2B0EQ(I)=B2B0EQ(I)/SKGGG
          BMODEQ(I)=BMODEQ(I)/SKGGG
          FOFBEQ(I)=FOFBEQ(I)/SKGGG
          GRDAEQ(I)=GRDAEQ(I)/SKGGG
       endif
    ENDDO !CORRECTED  20161115
    !MR
    !YLIN=YLIN*WBBS0*WBBS0*A*2./(.4*cgp*PLCUR)**2/BR00
    YLIN=YLIN*WBBS0*WBBS0*A*2./(.4*GP*PLCUR)**2/BR00
    ! 	open(33,file='dat/lin3')
    ! 	write(33,*) YLIN
    !,yvol*A
    ! 	close(33)
30  format(5(f10.4))
31  continue
    goto 10
    open(99,file='./CHARTST/EMEQ.chartst',STATUS='UNKNOWN',Access = 'append')
    write(99,*) NA1,NAOLD,NA,NT,NT1,I,I1,J,K,NITER
    write(99,*)	BR00,SA0,GL0,GD30,ACC,B0T,PLCUR,TIME
    write(99,*)	BA,BB,GR,GBD,GL &
    ,GSD,GRA,SQGRA,GRAR,AVR2 &
    ,AI0,dgrda,avsqg,Vol,GR2AUX
    write(99,*)	B2B0EQ,B0B2EQ,BMAXEQ, &
    BMINEQ,BMODEQ,FOFBEQ,GRDAEQ, &
    A,AA,C,CC,S,SS,SR,SX,SX1,T,Y,Y1,SDT,SDT0, &
    DRDA,DZDA,DRDT,DZDT,DMETR,DA2,DGR2,FI,FJ,D0,CGP, &
    GP,GP2,GR2,GLOLD,G3DOLD,AOLD,G22A2,SKGGG, &
    SQG,YLIN,YVOL,YMIN,YMAX &
    ,SKDR,SKGA,SQG22R
    !write(99,*) AOLD,GLOLD,G3DOLD,NAOLD,cgp,NITER
    write(99,*) AOLD,GLOLD,G3DOLD,NAOLD,GP,NITER
    write(99,*) SKDR,SKGA,SQG22R
    close(99)
    open(99,file='./CHARTST/EMEQINC.chartst',STATUS='UNKNOWN',Access = 'append')
10  return
  END subroutine EMEQ



  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !+ad_name EQAB3
  !+ad_summ 3 MOMENT EQUILIBRIUM SOLVER			AUGUST 17,1988
  !
  !+ad_args NA    : input real :
  !+ad_args NT    : input real :
  !+ad_args NITER : input real : max number of iterations
  !+ad_args ACC   : input real : relative tolerance parameter
  !+ad_prob None
  !+ad_call None
  !+ad_hist 31/10/16  Documenting the initial version
  !+ad_stat Okay
  !+ad_docs None


  
  ! - 3 MOMENT EQUILIBRIUM SOLVER			AUGUST 17,1988 
  ! NITER - max number of iterations
  ! ACC   - relative tolerance parameter
  subroutine EQAB3(NA,NT,NITER,ACC &
    !     >,ITER,WSAC1,WSAC2,WSAC3,WBR0,WGL,WSD1,WSD3,WDGL,WSL0,WSL1,WSL2
    !     >,WSL22
    )
    implicit none
    include	'emeq.inc'
    integer ::	NA,NT,NITER,NA1,I,J,I1,JJ
    !      real(kind(1.0d0)) ::
    !     1	   WSAC1,WSAC2,WSAC3,WBR0,WBR00,
    !     2	   WGL(*),WSD1(*),WSD3(*),WDGL(*),WSL0(*),WSL1(*),WSL2(*),
    !     3	   WSL22(*),
    real(kind(1.0d0)) :: &
    ACC,AA,AAI,A4AI,A6AI,S,SC2,W0,W1,W2,W3,BI0,BP1,BP3, &
    FU0,FU0I,FU0J,FU1I,FU1J,FU2I,FU2J,FU3I,FU3J, &
    FV0I,FV0J,FV1I,FV1J,FV2I,FV2J,FV3I,FV3J
    NA1=	NA+1
    ITER=	0
200 ITER=	ITER+1
    WSAC1=	WDSD1(NA1)
    WSAC2=	WDGL(NA1)
    WSAC3=	WDSD3(NA1)
    WBR0=	WBR00+WSD1(NA1)*WSAA(NA1)
    !      call add2loc("Calling EQK3"//char(0))
    CALL EQK3(NA,NT)
    !      call add2loc("Calling EQALVU3"//char(0))
    !      CALL EQLVU3(NA)
    call EQLVU3(NA,WBR0,WSAA,WGL,WSD1,WSD3, &
    WSL0,WSL1,WSL2,WSL3,WSL22, &
    WSV0,WSV1,WSV2,WSV3,WSU0,WSU1,WSU2,WSU3)
    !      call add2loc("Calling EQC1"//char(0))
    CALL EQC1(NA,WBR0,WBR00,WBA,WBB,WSA,WSP,WSJP,WDBA,WDBB)
    !  GMC,SW0,SDD1,GDL
    !eq0
    WGMC(1)=WBA(1)*WSL0(1)/WBK0(1)
    BI0=	0.
    W0=	0.
    FU0I=	WDBA(1)*WSL0(1)
    FV0I=	WDBB(1)*WSV0(1)
    FU0=	WGMC(1)*WSU0(1)
    WSW0(1)=0.25*(FU0-FU0I)
    !eq2
    W2=	0.
    FU2I=	WGMC(1)*WSU2(1)-WDBA(1)*WSL2(1)
    FV2I=	WDBB(1)*WSV2(1)
    SC2=	0.25*(WGL(1)**2-1.)
    WBS2(1)=((WBA(1)*WSL22(1)+WBB(1)*(WSV2(1)+SC2*WSV0(1))+ &
    FU2I/6.+SC2*WSW0(1))/WGMC(1)-WBK20(1))/WBK22(1)
    !eq1
    WDGL(1)=0.
    W1=	0.
    FU1I=	WGMC(1)*WSU1(1)
    FV1I=	WDBA(1)*WSL1(1)+WDBB(1)*WSV1(1)
    BP1=	(WBA(1)*WSL1(1)+WBB(1)*WSV1(1)+ 0.25&
    *FU1I)/WGMC(1)-WBK10(1)
    !eq3
    W3=	0.
    FU3I=	WGMC(1)*WSU3(1)
    FV3I=	WDBA(1)*WSL3(1)+WDBB(1)*WSV3(1)
    BP3=	(WBA(1)*WSL3(1)+WBB(1)*WSV3(1)+ &
    FU3I/6.)/WGMC(1)-WBK30(1)
    S=	1./(WBK11(1)*WBK33(1)-WBK13(1)*WBK31(1))
    WBS1(1)=(BP1*WBK33(1)-BP3*WBK13(1))*S
    WBS3(1)=(BP3*WBK11(1)-BP1*WBK31(1))*S
    WDSD1(1)=WBS1(1)
    DO I=2,NA1
       J=	I-1
       AA=	WSAA(I)
       AAI=	1./AA
       A4AI=	AAI*AAI
       A6AI=	A4AI*AAI
       !eq0
       FU0J=	FU0I
       FU0I=	WDBA(I)*WSL0(I)
       FV0J=	FV0I
       FV0I=	WDBB(I)*WSV0(I)
       BI0=	BI0+WSCJ3(J)*FU0J+WSCI3(J)*FU0I+ &
       WSCJ5(J)*FV0J+WSCI5(J)*FV0I
       W0=	W0+WSCJ3(J)*FU0
       WGMC(I)=(WBA(I)*WSL0(I)+WBB(I)*AA*WSV0(I)+(W0-BI0)*AAI) &
       /(WBK0(I)-WSCI3(J)*WSU0(I)*AAI)
       FU0=	WGMC(I)*WSU0(I)
       W0=	W0+WSCI3(J)*FU0
       WSW0(I)=(W0-BI0)*A4AI
       !eq2
       FU2J=	FU2I
       FU2I=	WGMC(I)*WSU2(I)-WDBA(I)*WSL2(I)
       FV2J=	FV2I
       FV2I=	WDBB(I)*WSV2(I)
       W2=	W2+FU2J*WSCJ5(J)+FU2I*WSCI5(J)- &
       FV2J*WSCJ7(J)-FV2I*WSCI7(J)
       SC2=	0.25*(WGL(I)**2-1.)
       WBS2(I)=((WBA(I)*WSL22(I)+WBB(I)*(WSV2(I)+SC2*WSV0(I))+ &
       W2*A6AI+SC2*WSW0(I))/WGMC(I)-WBK20(I))/WBK22(I)
       WDGL(I)=WDGL(J)+(WBS2(J)+WBS2(I))*WSCJ1(J)
       !eq1
       FV1J=	FV1I
       FV1I=	WDBA(I)*WSL1(I)+WDBB(I)*WSV1(I)
       FU1J=	FU1I
       FU1I=	WGMC(I)*WSU1(I)
       W1=	W1+FU1J*WSCJ3(J)+FU1I*WSCI3(J)- &
       FV1J*WSCJ5(J)-FV1I*WSCI5(J)
       BP1=	(WBA(I)*WSL1(I)+WBB(I)*WSV1(I)+ &
       W1*A4AI)/WGMC(I)-WBK10(I)
       !eq3
       FU3J=	FU3I
       FU3I=	WGMC(I)*WSU3(I)
       FV3J=	FV3I
       FV3I=	WDBA(I)*WSL3(I)+WDBB(I)*WSV3(I)
       W3=	W3+FU3J*WSCJ5(J)+FU3I*WSCI5(J)- &
       FV3J*WSCJ7(J)-FV3I*WSCI7(J)
       BP3=	(WBA(I)*WSL3(I)+WBB(I)*WSV3(I)+ &
       W3*A6AI)/WGMC(I)-WBK30(I)
       S=	1./(WBK11(I)*WBK33(I)-WBK13(I)*WBK31(I))
       WBS1(I)=(BP1*WBK33(I)-BP3*WBK13(I))*S
       WBS3(I)=(BP3*WBK11(I)-BP1*WBK31(I))*S
       WDSD1(I)=WBS1(I)
    ENDDO !CORRECTED  20161115
    !  SD1,GL
    W1=	0.
    WSD1(1)=0.5*WDSD1(1)
    S=	WGL(NA1)*EXP(-WDGL(NA1))
    ! 	write(*,*)S,WGL(1),WGL(NA1),WDGL(NA1),EXP(-WDGL(NA1)),WDSD1(1)
    WGL(1)=	S
    WDGL(1)=WBS2(1)
    DO I=2,NA1
       J=	I-1
       WGL(I)=	S*EXP(WDGL(I))
       WDGL(I)=WBS2(I)
       W1=	W1+(WDSD1(J)+WDSD1(I))*WSCJ1(J)
       WSD1(I)=W1/WSAA(I)
    ENDDO !CORRECTED  20161115
    !  SD3,SDD3
    FU3J=	1.+WDGL(NA1)*WSAA(NA1)
    WDSD3(NA1)=WBS3(NA1)+2.*WSD3(NA1)*FU3J
    DO I1=1,NA
       J=	NA1-I1
       I=	J+1
       AA=	WSAA(J)
       FU3I=	FU3J
       FU3J=	1.+WDGL(J)*AA
       WSD3(J)=(WSD3(I)*(WSAA(I)-2.*WSCI1(J)*FU3I)-(WBS3(I)+ &
       WBS3(J))*WSCJ1(J))/(AA+2.*WSCJ1(J)*FU3J)
       WDSD3(J)=WBS3(J)+2.*WSD3(J)*FU3J
    ENDDO !CORRECTED  20161115
    !error
    WSAC1=	WSA(NA1)*ABS(WDSD1(NA1)-WSAC1)
    WSAC2=	WSAA(NA1)*ABS(WDGL(NA1)-WSAC2)
    WSAC3=	WSA(NA1)*ABS(WDSD3(NA1)-WSAC3)
    WSACC=	WSAC1+WSAC2+WSAC3
    IF(ITER.LT.NITER.AND.WSACC.GT.ACC) GO TO 200
    
    !      write(*,'(2I5,5F10.7)')ITER,NITER,WSACC
    if (ITER.EQ.NITER)	then
       write(*,*)'>>> EMEQ >>> Maximum iteration number achieved'
       write(*,*)'             No convergence'
       NA = 0			! Suppress optional output
       goto 31
       return
    endif
    
    !  SD2D1,GD2L,SD2D3
    WD2SD1(1)=WDSD1(1)
    FU1I=	0.
    WD2GL(1)=WDGL(1)*WGL(1)
    FU2I=	-WGL(1)
    WD2SD3(1)=WDSD3(1)
    FU3I=	0.
    DO I=2,NA1
       J=	I-1
       AA=	WSAA(I)
       FU1J=	FU1I
       FU1I=	AA*(WDSD1(I)-WSD1(I))
       WD2SD1(I)=(FU1I-FU1J)/WSCI1(J)-WD2SD1(J)
       FU2J=	FU2I
       FU2I=	WGL(I)*(AA*WDGL(I)-1.)
       WD2GL(I)=(FU2I-FU2J)/WSCI1(J)-WD2GL(J)
       FU3J=	FU3I
       FU3I=	AA*(WDSD3(I)-WSD3(I))
       WD2SD3(I)=(FU3I-FU3J)/WSCI1(J)-WD2SD3(J)
    ENDDO !CORRECTED  20161115

31  continue
    goto 10
    open(99,file='./CHARTST/EQAB3.chartst',STATUS='UNKNOWN',Access = 'append')
    write(99,*) NA,NT,NITER,NA1,I,J,I1,JJ
    write(99,*)  &
    ACC,AA,AAI,A4AI,A6AI,S,SC2,W0,W1,W2,W3,BI0,BP1,BP3, &
    FU0,FU0I,FU0J,FU1I,FU1J,FU2I,FU2J,FU3I,FU3J,&
    FV0I,FV0J,FV1I,FV1J,FV2I,FV2J,FV3I,FV3J
    close(99)
    open(99,file='./CHARTST/EMEQINCEQAB3.chartst',STATUS='UNKNOWN',Access = 'append')

10  RETURN
  END subroutine EQAB3





  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !+ad_name EQGB3
  !+ad_summ ""
  ! geometria
  !+ad_args NA    : input real :
  !+ad_args BR00  : input real :
  !+ad_args SA0   : input real : 
  !+ad_args GL0   : input real : 
  !+ad_args GD30  : input real :
  ! Arrays defined here and used elsewhere
  !+ad_args WSA   : output real array : j*SA0/NA	current minor radius, a
  !+ad_args WSAA  : output real array : WSA**2	a^2
  !+ad_args WSCI1 : output real array : 
  !+ad_args WSCI3 : output real array :
  !+ad_args WSCI5 : output real array :
  !+ad_args WSCI7 : output real array : 
  !+ad_args WSCJ1 : output real array :
  !+ad_args WSCJ3 : output real array : 
  !+ad_args WSCJ5 : output real array : 
  !+ad_args WSCJ7 : output real array :
  ! Arrays first defined here and modified in EQAB3
  !+ad_args WGL   : output real array : \lambda
  !+ad_args WSD3  : output real array : \delta/a^2
  !+ad_args WDSD3 : output real array : 
  !+ad_prob None
  !+ad_call None
  !+ad_hist 31/10/16  Documenting the initial version
  !+ad_stat Okay
  !+ad_docs None

  !======================================================================|
  !       EQGB3.FOR - INITIAL CONDITIONS FOR THREE MOMENT EQUILIBRIA 17-AUG-1988
  BLOCK DATA GB3
     implicit none
     include	'emeq.inc'
     data	WSD1/NP*0.d0/,WDSD1/NP*0.d0/,WDGL/NP*0.d0/
     data	WBS1/NP*0.d0/,WBS2/NP*0.d0/,WBS3/NP*0.d0/
  end BLOCK DATA
  !======================================================================|

  
  ! ======================================================================|
  subroutine EQGB3(BR00,SA0,GL0,GD30,NA,WBR0,WBR00, &
    WSA,WSAA,WGL,WSD3,WDSD3, &
    WSCI1,WSCI3,WSCI5,WSCI7,WSCJ1,WSCJ3,WSCJ5,WSCJ7)
    ! ----------------------------------------------------------------------|
    ! Parameters used:
    ! Input:
    ! 	Variables:	NA,BR00,SA0,GL0,GD30
    ! 	Arrays:		
    ! Output
    ! 	Arrays defined here and used elsewhere
    !   WSA  = j*SA0/NA	current minor radius, a
    !   WSAA = WSA**2	a^2
    !   WSCI1 = 
    !   WSCI3 = 
    !   WSCI5 = 
    !   WSCI7 = 
    !   WSCJ1 = 
    !   WSCJ3 = 
    !   WSCJ5 = 
    !   WSCJ7 = 
    ! 	Arrays first defined here and modified in EQAB3
    !   WGL  = \lambda
    !   WSD3 = \delta/a^2
    !   WDSD3
    ! ----------------------------------------------------------------------|
    implicit none
    !      include	'emeq.inc'
    integer ::	NA,NA1,I,J
    real(kind(1.0d0)) :: &
    BR00,SA0,GL0,GD30,DA,D3,D32,AAI,AAJ,H,C0,CI,CJ,CII,CJJ
    real(kind(1.0d0)) ::	WBR0,WBR00
    real(kind(1.0d0)) ::	WSA(*),WSAA(*),WGL(*),WSD3(*),WDSD3(*)
    real(kind(1.0d0)) ::	WSCI1(*),WSCI3(*),WSCI5(*),WSCI7(*)
    real(kind(1.0d0)) ::	WSCJ1(*),WSCJ3(*),WSCJ5(*),WSCJ7(*)
    WBR00=BR00
    WBR0=BR00
    DA=SA0/NA!diff 
    D3=GD30/SA0**2!gd30 triang, 
    D32=2.*D3
    NA1=NA+1
    DO I=1,NA1
       J=I-1
       !geometria
       WSA(I)=DA*J
       WSAA(I)=WSA(I)**2
       WGL(I)=GL0 !elon
       WSD3(I)=D3
       WDSD3(I)=D32
       ! ---C(J,N),C(I,N)
       !parecen diferenciales
       IF(J.NE.0) THEN
       AAI=WSAA(I)
       AAJ=WSAA(J)
       H=(AAI-AAJ)*0.5
       WSCJ1(J)=H*0.5
       WSCI1(J)=WSCJ1(J)
       CJ=AAI+2.*AAJ
       CI=AAJ+2.*AAI
       C0=H/6.
       WSCJ3(J)=C0*CJ
       WSCI3(J)=C0*CI
       ! ---CJJ=AJ**2N
       CJJ=AAJ*AAJ
       ! ---CII=AI**2N
       CII=AAI*AAI
       CJ=AAI*CJ+3.*CJJ
       CI=AAJ*CI+3.*CII
       C0=C0*0.5
       WSCJ5(J)=C0*CJ
       WSCI5(J)=C0*CI
       C0=H*0.05
       WSCJ7(J)=C0*(AAI*CJ+4.*CJJ*AAJ)
       WSCI7(J)=C0*(AAJ*CI+4.*CII*AAI)
       ENDIF
    ENDDO !CORRECTED  20161115

31  continue
    goto 10
    open(99,file='./CHARTST/EQGB3.chartst',STATUS='UNKNOWN',Access = 'append')
    write(99,*)  NA,NA1,I,J
    write(99,*)   &
    BR00,SA0,GL0,GD30,DA,D3,D32,AAI,AAJ,H,C0,CI,CJ,CII,CJJ
    write(99,*)  	WBR0,WBR00
    write(99,*)  	WSA(NA1),WSAA(NA1),WGL(NA1),WSD3(NA1),WDSD3(NA1)
    write(99,*)  	WSCI1(NA1),WSCI3(NA1),WSCI5(NA1),WSCI7(NA1)
    write(99,*)  	WSCJ1(NA1),WSCJ3(NA1),WSCJ5(NA1),WSCJ7(NA1)
    close(99)
    !open(99,file='./CHARTST/EMEQINCEQGB3.chartst',STATUS='UNKNOWN',Access = 'append')
    !INCLUDE 'EMEQINCPRINT.INC'
10  RETURN
  END subroutine EQGB3
  
  
  ! - EQK3.FOR - LEFT HAND SIDE AVERAGER		AUGUST 17,1988
  
  subroutine EQK3(NA,NT)
    ! ----------------------------------------------------------------------|
    ! Parameters used:
    ! Input:
    ! 	Variables:	NA,WBR0
    ! 	Arrays:		
    ! Output (arrays):
    ! 			
    ! ----------------------------------------------------------------------|
    implicit none
!    include	'emeq.inc'
    integer ::	NA,NT,NA1,NT1,I,K
    real(kind(1.0d0)) :: &
    A,AA,ASPA,EE,SC1,SC2,T,DEN, &
    DA1,DA2,RA,RI,XR,G22A2,R0RD,SF2,SF20,SF21,SF3,SF30,SF31, &
    XRXR,SK10,SK11,SK13,SK0,SK01,SK2,SR,SX,SXX,SX1,SXX1,SZZ, &
    S,SS,SY1,C,CC,BM1,BM2,BMI,BN,BN0,BN1,BN2,BN12,SDT,SDT0 &
    ,SKDR,SKGA,SQG22R
    common /EMEQMR/SKDR(NP),SKGA(NP),SQG22R(NP)
    !save	CGP
    !data CGP/4.0d0*datan(1.0d0)/ !/3.14159265359d0/
    real(kind(1.0d0)), parameter :: CGP = 4.0d0*datan(1.0d0)
    NA1=	NA+1
    NT1=	NT+1
    SDT0=	1./NT
    RI=	1./WBR0
    DO I=1,NA1
       !geometric parameters
       A=	WSA(I)
       AA=	A*A
       ASPA=	AA*RI
       EE=	WGL(I)**2
       SC1=	0.25*(EE-1.)
       SC2=	0.5*(EE+1.)
       WBK02(I)=0.
       WBG332(I)=0.
       WBG222(I)=0.
       WSU1(I)=0.
       WSU2(I)=0.
       WBK20(I)=0.
       WBK22(I)=0.
       WBK10(I)=0.
       WBK11(I)=0.
       WBK13(I)=0.
       WBK30(I)=0.
       WBK31(I)=0.
       WBK33(I)=0.
       ! ------------------------------------------
       ! KONOVALOV LIKES TO DO SOMETHING BY HIMSELF
       ! ------------------------------------------
       SKGA(I)=0.
       SKDR(I)=0.
       SQG22R(I)=0.
       ! ------------------------------------------
       DO K=1,NT1
          T=	SDT0*CGP*(K-1)
          SDT=	SDT0
          IF(K.EQ.1.OR.K.EQ.NT1) SDT=SDT0*0.5
          ! --------------------------------------
          C=	COS(T)
          S=      SIN(T)
          SS=	S*S
          CC=	C*C
          SX1=	-WSD1(I)-WSD3(I)*SS
          SXX1=	SX1*SX1
          SZZ=	EE*SS
          SX=	C+A*SX1
          SXX=	SX*SX
          SR=	WBR0+A*SX
          SY1=	2.*C*WSD3(I)
          ! ---BN0=N0*DT
          BN0=	(EE*CC+SS)*SDT
          ! ---BN1=N1*DT
          BN1=	2.*SY1*SS*SDT
          ! ---BN2=N2*DT
          BN2=	SS*SY1*SY1*SDT
          BN12=	BN1+A*BN2
          BN=	BN0+A*BN12
          BM1=	-(WBS1(I)+WBS3(I)*SS)*C
          BM2=	WBS2(I)*SS
          BMI=	1./(1.+A*BM1+AA*BM2)
          !  EVEN EQUATIONS
          DEN=	1./(1.+A*BM1)
          SK01=	BN1-BN0*BM1
          SK0=	(BN2-BN1*(BM1+A*BM2))*BMI+BN0*BM1*BM1*DEN
          SK2=	-BN0*SS*DEN*BMI
          SF20=	CC-SZZ+SC1
          SF21=	2.*C*SX1
          SF2=	SF20+A*SF21+AA*SXX1
          WBK02(I)=WBK02(I)+SK0+WBS2(I)*SK2
          WBK20(I)=WBK20(I)+BN0*SXX1+SK01*SF21+SK0*SF2
          WBK22(I)=WBK22(I)+SK2*SF2
          !  ODD EQUATIONS
          DEN=	1./(1.+AA*BM2)
          SK0=	BN0*DEN
          SK10=	BN12*BMI
          SK11=	SK0*C*BMI
          SK13=	SK11*SS
          SF3=	CC-3.*SZZ
          SF30=	C*SF3
          SF31=	SX1*(SXX+C*SX+SF3)
          SF3=	SF30+A*SF31
          WBK10(I)=WBK10(I)+SK0*SX1+SK10*SX
          WBK30(I)=WBK30(I)+SK0*SF31+SK10*SF3
          WBK11(I)=WBK11(I)+SK11*SX
          WBK31(I)=WBK31(I)+SK11*SF3
          WBK13(I)=WBK13(I)+SK13*SX
          WBK33(I)=WBK33(I)+SK13*SF3
          XR=	SX*RI
          XRXR=	XR*XR
          R0RD=	WBR0/SR
          WBG222(I)=WBG222(I)+BN*BMI*XRXR*R0RD
          R0RD=R0RD*SDT
          WBG332(I)=WBG332(I)+(XRXR+BM2-BM1*XR)*R0RD
          R0RD=	R0RD*SX*C
          WSU1(I)=WSU1(I)+R0RD*SZZ
          WSU2(I)=WSU2(I)+R0RD*SXX
          ! HERE ZAKHAROV'S AUTHOR RIGHTS ARE CANCELED WITHOUT ANY CEREMONY
          ! 	DRDA=-WDSD1(I)*A+C-WDSD3(I)*A*SS
          ! 	DZDA=S*WGL(I)*(AA*WDGL(I)+1.)
          ! 	DRDT=-A*S-2.*AA*WSD3(I)*C*S	
          ! 	DZDT=WGL(I)*A*C
          ! METRIC (SUBSRIPT) TENSOR COMPONENTS
          ! 	G11=DRDA**2+DZDA**2
          ! 	G22=DRDT**2+DZDT**2
          ! 	G12=DRDA*DRDT+DZDA*DZDT
          !  G22/A**2
          G22A2=SS+4.*A*WSD3(I)*SS*C+(2.*A*WSD3(I)*S*C)**2+(WGL(I)*C)**2
          ! D/A
          DA1=WGL(I)*(CC-A*(WDSD1(I)+WDSD3(I)*SS)*C+ &
          SS*(WDGL(I)*AA+1.)*(1.+2.*A*C*WSD3(I)))
          ! ---- ASTRA METRIC COMBINATIONS -------
          SKGA(I)=SKGA(I)+G22A2*SR*SDT/DA1
          SQG22R(I)=SQG22R(I)+sqrt(G22A2)*SR*SDT
          ! D**2/A**2
          DA2=(C-A*(WDSD1(I)+WDSD3(I)*SS))*WGL(I)*C+ &
          WGL(I)*SS*(WDGL(I)*AA+1.)*(1.+2.*A*C*WSD3(I))
          SKDR(I)=SKDR(I)+SDT*DA2/SR      
       ENDDO !CORRECTED  20161115
       WBG332(I)=WBG332(I)+(WSD1(I)+0.5*WSD3(I))*RI
       WBG222(I)=WBG222(I)+WBK02(I)-(WBK10(I)+ &
       WBS1(I)*WBK11(I)+WBS3(I)*WBK13(I))*RI
       WBD02(I)= 0.5*WBS2(I)
       RA=	  1.-WSD1(I)*AA*RI
       WBG02(I)=0.5*WBS2(I)*(RA-0.75*WSD3(I)*ASPA)- &
       (WSD1(I)+0.5*(WSD3(I)+WBS1(I)+0.25*WBS3(I)))*RI
       WBD12(I)= WBG02(I)-WBG332(I)
       WBK0(I)=  SC2+AA*WBK02(I)
       WDBK00(I)=EE*WDGL(I)
       WBD0(I)=  1.+AA*WBD02(I)
       WBG0(I)=  1.+AA*WBG02(I)
       WBG33(I)= 1.+AA*WBG332(I)
       WBG22(I)= SC2+AA*WBG222(I)
    ENDDO !CORRECTED  20161115

31  continue
    goto 10
    open(99,file='./CHARTST/EQK3.chartst',STATUS='UNKNOWN',Access = 'append')
    write(99,*)      NA,NT,NA1,NT1,I,K
    write(99,*) &
    CGP,A,AA,ASPA,EE,SC1,SC2,T,DEN, &
    DA1,DA2,RA,RI,XR,G22A2,R0RD,SF2,SF20,SF21,SF3,SF30,SF31, &
    XRXR,SK10,SK11,SK13,SK0,SK01,SK2,SR,SX,SXX,SX1,SXX1,SZZ, &
    S,SS,SY1,C,CC,BM1,BM2,BMI,BN,BN0,BN1,BN2,BN12,SDT,SDT0 &
    ,SKDR,SKGA,SQG22R
    write(99,*) SKDR,SKGA,SQG22R
    close(99)
    open(99,file='./CHARTST/EMEQINCEQK3.chartst',STATUS='UNKNOWN',Access = 'append')
10  return
  END subroutine EQK3

  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !+ad_name EQLVU3
  !+ad_summ RIGHT HAND SIDE AVERAGER	AUGUST 17,1988
  !
  !+ad_args NA      : input real :
  !+ad_args WBR0    : input real : WBR0
  ! Arrays 
  !+ad_args WSAA(*) : input real array : 
  !+ad_args WGL(*)  : input real array :
  !+ad_args WSD1(*) : input real array : 
  !+ad_args WSD3(*) : input real array :
  ! Arrays defined here and used elsewhere
  !+ad_args WSL0    : output real array :
  !+ad_args WSL1    : output real array : 
  !+ad_args WSL2    : output real array :
  !+ad_args WSL3    : output real array : 
  !+ad_args WSL22   : output real array : 
  !+ad_args WSV0    : output real array :
  !+ad_args WSV1    : output real array : 
  !+ad_args WSV2    : output real array :
  !+ad_args WSV3    : output real array :
  ! Arrays modified here, first defined in EQK3, used in EQAB3
  !+ad_args WSU0    : output real array : 
  !+ad_args WSU1    : output real array : 
  !+ad_args WSU2    : output real array : 
  !+ad_args WSU3    : output real array :
  !+ad_prob None
  !+ad_call None
  !+ad_hist 31/10/16  Documenting the initial version
  !+ad_stat Okay
  !+ad_docs None

  
  ! - EQLVU3.FOR - RIGHT HAND SIDE AVERAGER	AUGUST 17,1988
  ! calcula     L0,L1,L2,L3,L22,V0,V1,V2,V3,U0,U1,U2,U3
  subroutine EQLVU3(NA,WBR0,WSAA,WGL,WSD1,WSD3, &
    WSL0,WSL1,WSL2,WSL3,WSL22,  &
    WSV0,WSV1,WSV2,WSV3,WSU0,WSU1,WSU2,WSU3)
    ! ----------------------------------------------------------------------|
    ! Parameters used:
    ! Input:
    ! 	Variables:	NA,WBR0
    ! 	Arrays:	
    !   WSAA(*)
    !   WGL(*)
    !   WSD1(*)
    !   WSD3(*)
    ! Output
    ! 	Arrays defined here and used elsewhere
    !   WSL0
    !   WSL1
    !   WSL2
    !   WSL3
    !   WSL22
    !   WSV0
    !   WSV1
    !   WSV2
    !   WSV3
    ! 	Arrays modified here, first defined in EQK3, used in EQAB3
    !   WSU0
    !   WSU1
    !   WSU2
    !   WSU3
    ! ----------------------------------------------------------------------|
    implicit none
    	include	'emeq.inc'
    real(kind(1.0d0)) ::	WBR0,WSAA(*),WGL(*),WSD1(*),WSD3(*)
    real(kind(1.0d0)) ::	WSL0(*),WSL1(*),WSL2(*),WSL3(*),WSL22(*)
    real(kind(1.0d0)) ::	WSV0(*),WSV1(*),WSV2(*),WSV3(*)
    real(kind(1.0d0)) ::	WSU0(*),WSU1(*),WSU2(*),WSU3(*)
    integer ::	NA,NA1,I
    real(kind(1.0d0)) :: &
    CR2,CR3,CR4,CR6,CR8,AA,E,EE,T1,T3,T5,RBR0, &
    S,S1,S2,X20,X22,X30,X32,X40,X42,X50,X60,X5X1,X6X1, &
    Y0,Y2,Y4,UX0,UX2,UX12,UX30,UT0,UT1,UT2,UT4,UT6, &
    UY0,UY1,UY2,UY3,UY4,UZ1,UZ3
    data CR2 /5.d-1/,CR3/3.33333333d-1/,CR6/1.66666666d-1/, &
    CR4/2.5d-1/,CR8/1.25d-1/
    NA1=	NA+1
    RBR0=1./WBR0
    DO I=1,NA1
       AA=	WSAA(I)
       E=	WGL(I)
       EE=	E*E
       UX2=	CR2*WSD3(I)
       UX0=	-WSD1(I)-UX2
       ! -------------------------------
       UY4=	CR2*UX2*UX2
       UY0=	UX0*UX0+UY4
       Y0=	CR2+UY0*AA
       UY1=	2.*UX0+UX2
       UY2=	2.*UX0*UX2
       Y2=	CR2+UY2*AA
       UY3=	UX2
       Y4=	UY4*AA
       UT0=	UX0*Y0+CR2*(UY1+UX2*Y2)
       UT1=	UX0*UY1+UY0+CR2*(UY2+UX2*(UY3+UY1))
       T1=	0.75+UT1*AA
       UT2=	UX0*Y2+Y0*UX2+CR2*(UY3+UY1+UX2*Y4)
       T3=	AA*UX0*UY3+CR2*(Y2+Y4+AA*UX2*UY1)
       UT4=	UX0*Y4+CR2*(UY3+UX2*Y2)
       T5=	CR2*(Y4+AA*UX2*UY3)
       UT6=	CR2*UX2*Y4
       UZ1=	UY1*(2.*Y0+Y2)+UY3*(Y2+Y4)
       UZ3=	UY3*2.*Y0+UY1*(Y4+Y2)
       X5X1=	T1*Y0+AA*UT0*UY1+CR2*(Y2*(T1+T3)+ &
       Y4*(T3+T5)+AA*(UT2*UY1+UY3*(UT2+UT4)))
       X6X1=	T1*(2.*UT0+UT2)+T3*(UT2+UT4)+T5*(UT4+UT6)
       S=	EE*CR8
       X20=	CR4*UY1
       X22=	S*(UY1-UY3)*CR2
       X30=	CR6*T1
       X32=	S*(T1-T3)*CR3
       UX30=	CR6*UT1
       X40=	CR8*UZ1
       X42=	S*(UZ1-UZ3)*0.25
       X50=	X5X1*0.1
       X60=	CR2*X6X1*CR6
       ! -------------------------------
       S1=(1.-EE)*CR4
       WSL0(I)=	E*CR2
       WSL22(I)=	E*UX30
       WSL2(I)=	WSL0(I)*S1+AA*WSL22(I)
       WSL1(I)=	E*X20
       WSL3(I)=	E*(X40-3.*X22)
       S=	2.*E*RBR0
       S1=	CR2*RBR0
       S2=	AA*S1
       WSV0(I)=	S*(X20+S1*X30)
       WSV1(I)=	S*(X30+S2*X40)
       WSV2(I)=	S*(X40-X22+S1*(X50-X32))
       WSV3(I)=	S*(X50-3.*X32+S2*(X60-3.*X42))
       
       UX30=	WSU2(I)
       UX12=	WSU1(I)
       S=	EE*RBR0
       S2=	AA*RBR0
       WSU1(I)=	S*(CR2+S2*(UX30*RBR0-2.*X20))
       WSU0(I)=	-RBR0*WSU1(I)
       WSU2(I)=	S*(2.*X20+RBR0*(UX12-UX30))
       WSU3(I)=	S*(UX30-3.*UX12)
    ENDDO !CORRECTED  20161115

31  continue
    goto 10
    open(99,file='./CHARTST/EQLVU3.chartst',STATUS='UNKNOWN',Access = 'append')
    write(99,*)  WBR0,WSAA(NA1),WGL(NA1),WSD1(NA1),WSD3(NA1)
    write(99,*)  WSL0(NA1),WSL1(NA1),WSL2(NA1),WSL3(NA1),WSL22(NA1)
    write(99,*)  WSV0(NA1),WSV1(NA1),WSV2(NA1),WSV3(NA1)
    write(99,*)  WSU0(NA1),WSU1(NA1),WSU2(NA1),WSU3(NA1)
    write(99,*)  NA,NA1,I
    write(99,*)   &
    CR2,CR3,CR4,CR6,CR8,AA,E,EE,T1,T3,T5,RBR0, &
    S,S1,S2,X20,X22,X30,X32,X40,X42,X50,X60,X5X1,X6X1, &
    Y0,Y2,Y4,UX0,UX2,UX12,UX30,UT0,UT1,UT2,UT4,UT6, &
    UY0,UY1,UY2,UY3,UY4,UZ1,UZ3
    close(99)


10  RETURN
  END subroutine EQLVU3


  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !+ad_name EQC1
  !+ad_summ "Calculates Zakharov's functions A and B"
  !
  !+ad_args NA      : input real :
  !+ad_args WBR0    : input real : 
  !+ad_args WBR00   : input real : 
  ! Arrays 
  !+ad_args WSA     : input real array : 
  !+ad_args WSP     : input real array :
  !+ad_args WSJP    : input real array : 
  ! Arrays 
  !+ad_args WBA     : output real array : Zakharov's function A
  !+ad_args WBB     : output real array : Zakharov's function B
  !+ad_args WDBA    : output real array : h/a*dA/da
  !+ad_args WDBB    : output real array : 
  !+ad_call None
  !+ad_hist 31/10/16  Documenting the initial version
  !+ad_stat Okay
  !+ad_docs None
  
  
  subroutine EQC1(NA,WBR0,WBR00,WBA,WBB,WSA,WSP,WSJP,WDBA,WDBB)
    ! ----------------------------------------------------------------------|
    ! Parameters used:
    ! Input:
    ! 	Variables:	NA,WBR0,WBR00
    ! 	Arrays:		WSA,WSP,WSJP
    ! Output:
    !      Arrays:
    !   WBA   Zakharov's function A
    !   WBB   Zakharov's function B
    !   WDBA  h/a*dA/da
    !   WDBB  
    ! ----------------------------------------------------------------------|
    implicit none
     	include 'emeq.inc'
    ! 	real(kind(1.0d0)) :: WBR0,WBR00
    ! 	real(kind(1.0d0)) :: WSA(1),WSP(1),WSJP(1),WBA(1),WBB(1),WDBA(1),WDBB(1)
    real(kind(1.0d0)) :: WBR0,WBR00,WSA(*)
    real(kind(1.0d0)) :: WBA(*),WBB(*),WSP(*),WSJP(*),WDBA(*),WDBB(*)
    
    integer ::	NA,NA1,I
    real(kind(1.0d0)) ::	H, S, RS, SS
    NA1=NA+1
    WDBA(1)=2.*(WSJP(2)-WSJP(1))/WSA(2)**2
    WBA(1)=WSJP(1)
    WBB(1)=WSP(1)
    WDBB(1)=2.*(WSP(2)-WSP(1))/WSA(2)**2
    H=.5/WSA(2)
    DO I=2,NA
       WBA(I)=WSJP(I)
       WBB(I)=WSP(I)
       WDBB(I)=(WSP(I+1)-WSP(I-1))*H/WSA(I)
       WDBA(I)=(WSJP(I+1)-WSJP(I-1))*H/WSA(I)
    ENDDO !CORRECTED  20161115
    WBA(NA1)=WSJP(NA1)
    WDBA(NA1)=(WSJP(NA1)-WSJP(NA))/WSA(2)/WSA(NA1)
    WBB(NA1)=WSP(NA1)
    WDBB(NA1)=(WSP(NA1)-WSP(NA))/WSA(2)/WSA(NA1)
    S=WBR0/WBR00
    RS=1./S
    SS=S-RS
    DO I=1,NA1
       WBA(I)=WBA(I)*RS+WBB(I)*SS
       WBB(I)=WBB(I)*S
       WDBA(I)=WDBA(I)*RS+WDBB(I)*SS
       WDBB(I)=WDBB(I)*S
    ENDDO !CORRECTED  20161115

31  continue
    goto 10
    open(99,file='./CHARTST/EQC1.chartst',STATUS='UNKNOWN',Access = 'append')
    write(99,*)   WBR0,WBR00,WSA(NA1)
    write(99,*)   WBA(NA1),WBB(NA1),WSP(NA1),WSJP(NA1),WDBA(NA1),WDBB(NA1)
    write(99,*)   NA,NA1,I
    write(99,*)   H, S, RS, SS
    close(99)

10  return

  end subroutine EQC1





  ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !+ad_name EQPPAB
  !+ad_summ ""
  ! Input Variables
  !+ad_args NA      : input real :
  !+ad_args WBR0    : input real : WBR0 = R0[m]
  !+ad_args WBR00   : input real : WBR00 = (RTOR+SHIFT) = R[m]
  !+ad_args WBJ0    : input real : WBJ0 = 0.2*I[MA]
  !+ad_args WBBS0   : input real : WBBS0 = Bs[T],
  ! Input Arrays 
  !+ad_args WSA(*)  : input real array : WSA(I) = a[m], 
  !+ad_args WSAA(*) : input real array : WSAA(I) = a**2
  !+ad_args WGL(*)  : input real array :
  !+ad_args WSD1(*) : input real array : 
  !+ad_args WSD3(*) : input real array : 
  ! Output Variables 
  !+ad_args WGB     : output real : betas
  !+ad_args WGB0    : output real : betas
  !+ad_args WGBD    : output real : betas
  !+ad_args WGBJ    : output real : betas
  !+ad_args WGBST   : output real : betas
  !+ad_args WGMJ    : output real : DERIVATIVES
  !+ad_args WGMJEX  : output real : DERIVATIVES ...
  !+ad_args WSQC    : output real : 
  !+ad_args WGPINT  : output real : magnetic flux --- printed as output
  !+ad_args WGPRES  : output real : internal inductance --- not used
  !+ad_args WSLI    : output real : internal inductance --- not used
  ! Output Arrays 
  !+ad_args WSL0    : output real array : 
  !+ad_args WSL1    : output real array : 
  !+ad_args WSL2    : output real array :
  !+ad_args WSL3    : output real array : 
  !+ad_args WSL22   : output real array : 
  !+ad_args WSV0    : output real array : 
  !+ad_args WSV1    : output real array :
  !+ad_args WSV2    : output real array : 
  !+ad_args WSV3    : output real array : 
  !+ad_call None
  !+ad_hist 31/10/16  Documenting the initial version
  !+ad_stat Okay
  !+ad_docs None


  
  subroutine EQPPAB(NA)
    ! ----------------------------------------------------------------------|
    ! 	gm0 = 0.4*cgp
    ! 	WSA(I) = a[m], WSAA(I) = a**2
    ! 	WBR00 = (RTOR+SHIFT) = R[m]
    ! 	WBR0 = R0[m]
    ! 	WBJ0 = 0.2*I[MA]
    ! 	WBBS0 = Bs[T],
    ! 	WBF(I)=0.2*F/R0[MA/m], WBFF(I) = WBF(I)**2
    ! 	WSJ(I)=gm0*<j>[MA/m**2], WSJP(I) = <jB>/B
    ! 	WSJSL(I) = j(r,gt=cgp), WSJSR(I) = j(r,gt=0)
    ! 	WSP(I) = gm0*p[MJ/m**3]
    ! 	WGP(I) = gP[VS]/(2*cgp*R0)
    ! 	WDSQRQ(I) = aq'/q
    !   WGPINT, WGPRES, WSLI are defined but not used (not present in commons
    ! ----------------------------------------------------------------------|
    ! Parameters used:
    ! Input:
    ! 	Variables:	NA,WBR0,WBR00,WBJ0,WBBS0
    ! 	Arrays:	
    !   WSA(*)
    !   WSAA(*)
    !   WGL(*)
    !   WSD1(*)
    !   WSD3(*)
    ! Output
    !     Variables:
    !   WGB
    !   WGB0
    !   WGBD
    !   WGBJ
    !   WGBST
    !   WGMJ
    !   WGMJEX
    !   WSQC
    !   WGPINT
    !   WGPRES
    !   WSLI
    ! 	Arrays:
    !   WSL0
    !   WSL1
    !   WSL2
    !   WSL3
    !   WSL22
    !   WSV0
    !   WSV1
    !   WSV2
    !   WSV3
    ! ----------------------------------------------------------------------|
    implicit none
    include	'emeq.inc'
    ! 	real(kind(1.0d0)) :: WBR0,WBR00	!,WBJ0,WBBS0
    ! 	real(kind(1.0d0)) :: WBA(*),WBB(*),WGL(*),WGP(*),WDBA(*),WDBB(*)
    ! 	real(kind(1.0d0)) :: WSJ(*),WSJSL(*),WSJSR(*),WDSJ(*)
    ! 	real(kind(1.0d0)) :: WGB,WGB0,WGBD,WGBJ,WGBST,WGMJ,WGMJEX,WSQC
    ! 	real(kind(1.0d0)) :: WSP(1),WSA(1),WSAA(1),WGL(1),WGP(1),WSJ(1),WSJP(1)
    ! 	real(kind(1.0d0)) :: WBA(1),WBB(1),WBF(1),WBFF(1),WSD1(1)
    ! 	real(kind(1.0d0)) :: WDBA(1),WDBB(1),WDGL(1),WBK0(1),WBK02(1),WDBK00(1)
    ! 	real(kind(1.0d0)) :: WDSP(1),WDSJ(1),WSQ(1),WDSQ(1),WDSQRQ(1),WDSJP(1)
    ! 	real(kind(1.0d0)) :: WBD0(1),WBD12(1),WBD02(1)
    ! 	real(kind(1.0d0)) :: WBG22(1),WBG33(1),WBG332(1)
    ! 	real(kind(1.0d0)) :: WSCI3(1),WSCJ3(1),WSCJ1(1)
    ! 	real(kind(1.0d0)) :: WSL0(1),WSU0(1),WSW0(1),WSV0(1),WGMC(1),WDGMC(1)
    ! 	real(kind(1.0d0)) :: WSJSL(1),WSJSR(*)
    integer ::	NA,NA1,I,J
    real(kind(1.0d0)) :: &
    S,SLI,SLJ,G33I,G33J,DG33,BJ,BJI,BJJ,RR,RL, &
    DL0,DL0I,DL0J,D12I,D12J,DD12,DV0,FF0,FFI,FFJ,V0I,V0J, &
    GB,GBI,GBJ,GBS,GBSI,GBSJ,GBD,GBDI,GBDJ,GPI,GPJ,GMJI,GMJJ, &
    WGPINT,WGPRES,WSLI
    !save CGP
    !data CGP/4.0d0*datan(1.0d0)/ !/3.14159265359d0/
    real(kind(1.0d0)), parameter :: CGP = 4.0d0*datan(1.0d0)
    NA1=NA+1
    ! --- GP,SJ,DSJ,BJ,SJL,SJR
    
    G33I=0.
    DG33=2.*(WBA(1)*(WBG332(1)-WBD02(1))+WBB(1)*WBD12(1))
    WSJ(1)=WBA(1)
    WDSJ(1)=WDBA(1)+DG33
    BJI=WSJ(1)*WGL(1)
    BJ=0.
    
    DO I=2,NA1
       J=I-1
       G33J=G33I
       G33I=(WBA(I)*(WBG332(I)-WBD02(I))+WBB(I)*WBD12(I))* &
       WSAA(I)/WBD0(I)
       DG33=(G33I-G33J)/WSCJ1(J)-DG33
       WSJ(I)=WBA(I)+G33I
       WDSJ(I)=WDBA(I)+DG33
       BJJ=BJI
       BJI=WSJ(I)*WBD0(I)*WGL(I)
       BJ=BJ+(BJJ+BJI)*WSCJ1(J)
    ENDDO !CORRECTED  20161115
    
    S=WBJ0/BJ
    
    DO I=1,NA1
       WBA(I)	=WBA(I)*S
       WDBA(I)	=WDBA(I)*S
       WBB(I)	=WBB(I)*S
       WDBB(I)	=WDBB(I)*S
       WSJ(I)	=WSJ(I)*S
       WDSJ(I)	=WDSJ(I)*S
       WGMC(I)	=WGMC(I)*S
       WSW0(I)	=WSW0(I)*S
       
       RR=WBR0-WSD1(I)*WSAA(I)
       RL=WBR0/(RR-WSA(I))
       RR=WBR0/(RR+WSA(I))
       WSJSL(I)=WBA(I)*RL+WBB(I)*(1./RL-RL)!zakharov (18)
       WSJSR(I)=WBA(I)*RR+WBB(I)*(1./RR-RR)!zakharov (18)
       
       WDSP(I)=-WBB(I)*WGMC(I)*WGL(I)
    ENDDO !CORRECTED  20161115
    
    GPI=WGMC(1)*WGL(1)
    WGP(1)=0.
    SLI=WGP(1)*WSJ(1)*WGL(1)*WBD0(1)
    WSLI=0.
    
    FFI=(WBA(1)-WBB(1))*GPI
    WBFF(1)=0.
    WSP(1)=0.
    
    GBI=WBB(1)*GPI*WSL0(1)
    GB=0.
    GBSI=0.
    GBS=0.
    GBDI=(WBA(1)-WBB(1))*WGMC(1)*WSU0(1)
    GBD=0.
    
    DO I=2,NA1
       J=I-1
       
       GPJ=GPI
       GPI=WGMC(I)*WGL(I)
       WGP(I)=WGP(J)-(GPJ+GPI)*WSCJ1(J)
       SLJ=SLI
       SLI=WGP(I)*WSJ(I)*WGL(I)*WBD0(I)
       WSLI=WSLI+(SLJ+SLI)*WSCJ1(J)
       
       FFJ=FFI
       FFI=(WBA(I)-WBB(I))*GPI
       WBFF(I)=WBFF(J)+(FFJ+FFI)*WSCJ1(J)
       WSP(I)=WSP(J)+(WDSP(J)+WDSP(I))*WSCJ1(J)
       
       GBJ=GBI
       GBI=WBB(I)*GPI*WSL0(I)
       GB=GB+GBJ*WSCJ3(J)+GBI*WSCI3(J)
       
       GBSJ=GBSI
       GBSI=WSP(I)*GBI
       GBS=GBS+GBSJ*WSCJ3(J)+GBSI*WSCI3(J)
       
       GBDJ=GBDI
       GBDI=(WBA(I)-WBB(I))*WGMC(I)*WSU0(I)
       GBD=GBD+GBDJ*WSCJ3(J)+GBDI*WSCI3(J)
    ENDDO !CORRECTED  20161115
    WGPINT=-2.*CGP*WBR0*WGP(NA1)
    ! 
    !  internal iductance
    ! 
    WSLI=2.*WBR0/(WBR00*WBJ0**2)*(WSLI-WGP(NA1)*WBJ0)
    WGPRES=CGP*WBR00*WSLI*WBJ0
    ! 
    !  betta j
    ! 
    WGBJ=4.*GB/(WBJ0**2)
    S=WSL0(NA1)*WSAA(NA1)
    ! 
    !  betta
    ! 
    WGB=2.*GB/(WBBS0**2*S)
    ! 
    !  betta*
    ! 
    WGBST=2.*SQRT(2.*(GBS-WSP(NA1)*GB)/S)/WBBS0**2
    !DPC ff0 here is not yet defined, moved ff0 calculation up
    FF0=(WBBS0*WBR00/WBR0)**2
    WGBD=2.*GB/(WBR0**2*(2.*GBD-FF0*WSU0(NA1)*WSAA(NA1)/WGL(NA1)))
    WGMJ=4.*GBD*(WBR0/WBJ0)**2
    WSP(1)=-WSP(NA1) ! WSP(I) = gm0*p[MJ/m**3] - weighted plasma pressure
    !moved up	FF0=(WBBS0*WBR00/WBR0)**2
    WBFF(1)=FF0+2.*WBFF(NA1)
    WBF(1)=SQRT(WBFF(1))
    WGB0=2.*WSP(1)/WBBS0**2
    WSQC=WBBS0*WSAA(NA1)*(WGL(NA1)**2+1.)*.5/(WBJ0*WBR00)
    
    ! --- SJP,SDJP,SP,BFF,DGMC,SQ,SDQ,GMJ,GMJEX
    DV0=2.*(WBB(1)*WSV0(1)+WSW0(1)-WGMC(1)*WBK02(1))
    V0I=0.
    DL0I=(WDGL(1)-2.*WBD02(1))*WGL(1)
    DL0=0.
    WDGMC(1)=(WDBA(1)*WSL0(1)+WBA(1)*(WGL(1)*WBD02(1)+.25*DL0I)- &
    WGMC(1)*WDBK00(1)+DV0)/WBK0(1)
    
    FFI=(WBA(1)-WBB(1))*WGMC(1)*WGL(1)
    WSQ(1)=WBF(1)/(WBR0*WGMC(1))
    DG33=2.*WBG332(1)*WSQ(1)
    G33I=0.
    WDSQ(1)=WSQ(1)*(-FFI/WBFF(1)-WDGMC(1)/WGMC(1))+DG33
    WDSQRQ(1)=0.
    
    DD12=2.*((WBA(1)-WBB(1))*WBG22(1)/(WBR0*WSQ(1))**2+ &
    WBB(1)*WBD12(1))
    D12I=0.
    WSJP(1)=WBA(1)
    WDSJP(1)=WDBA(1)+DD12
    
    BJI=WGL(1)
    BJ=0.
    GMJI=FFI/WBF(1)*BJI*.5
    WGMJEX=0.
    
    DO I=2,NA1
       J=I-1
       WSP(I)=WSP(I)-WSP(NA1)
       ! --- DERIVATIVES
       S=1./WSCJ1(J)
       GPI=WGMC(I)*WGL(I)
       
       WBFF(I)=WBFF(1)-2.*WBFF(I)
       WBF(I)=SQRT(WBFF(I))
       
       DL0J=DL0I
       DL0I=(WDGL(I)-2.*WBD02(I))*WGL(I)
       DL0=DL0+DL0J*WSCJ3(J)+DL0I*WSCI3(J)
       V0J=V0I
       V0I=(WBB(I)*WSV0(I)+WSW0(I)-WGMC(I)*WBK02(I))*WSAA(I)
       DV0=(V0I-V0J)*S-DV0
       WDGMC(I)=(WBA(I)*(WGL(I)*WBD02(I)+DL0/WSAA(I)**2)+ &
       WDBA(I)*WSL0(I)-WGMC(I)*WDBK00(I)+DV0)/(WGL(I)**2+1.)*2.
       
       FFI=(WBA(I)-WBB(I))*WGL(I)*WGMC(I)
       G33J=G33I
       WSQ(I)=WBF(I)/(WBR0*WGMC(I))
       G33I=WSQ(I)*WBG332(I)*WSAA(I)
       DG33=(G33I-G33J)*S-DG33
       WDSQ(I)=WSQ(I)*(-FFI/WBFF(I)-WDGMC(I)/WGMC(I))+DG33
       WSQ(I)=WSQ(I)+G33I
       WDSQRQ(I)=WSAA(I)*WDSQ(I)/WSQ(I) !aq'/q Zakharov (63)?
       
       D12J=D12I
       D12I=((WBA(I)-WBB(I))*WBG22(I)*WBG33(I)/(WBR0*WSQ(I))**2+ &
       WBB(I)*WBD12(I)/WBG33(I))*WSAA(I)
       DD12=(D12I-D12J)*S-DD12
       WSJP(I)=WBA(I)+D12I
       WDSJP(I)=WDBA(I)+DD12
       
       BJJ=BJI
       BJI=WBG33(I)*WGL(I)
       BJ=BJ+(BJJ+BJI)*WSCJ1(J)
       GMJJ=GMJI
       GMJI=FFI*BJ/(WBF(I)*WSAA(I))
       WGMJEX=WGMJEX+GMJJ*WSCJ3(J)+GMJI*WSCI3(J)
       
    ENDDO !CORRECTED  20161115
    
    WGMJEX=4.*WGMJEX*WBBS0/WBJ0**2
    
31  continue
    goto 10
    open(99,file='./CHARTST/EQPPAB.chartst',STATUS='UNKNOWN',Access = 'append')
    write(99,*)   NA,NA1,I,J
    write(99,*)    &
    S,SLI,SLJ,G33I,G33J,CGP,DG33,BJ,BJI,BJJ,RR,RL, &
    DL0,DL0I,DL0J,D12I,D12J,DD12,DV0,FF0,FFI,FFJ,V0I,V0J, &
    GB,GBI,GBJ,GBS,GBSI,GBSJ,GBD,GBDI,GBDJ,GPI,GPJ,GMJI,GMJJ, &
    WGPINT,WGPRES,WSLI
    close(99)
    open(99,file='./CHARTST/EMEQINCEQPPAB.chartst',STATUS='UNKNOWN',Access = 'append')

10  return
  END subroutine EQPPAB
  ! ======================================================================|
  ! EQGB3.FOR - INITIAL CONDITIONS FOR THREE MOMENT EQUILIBRIA 17-AUG-1988
  !	BLOCK DATA GB3
  !	implicit none
  !	include	'emeq.inc'
  !	data	WSD1/NP*0.d0/,WDSD1/NP*0.d0/,WDGL/NP*0.d0/
  !	data	WBS1/NP*0.d0/,WBS2/NP*0.d0/,WBS3/NP*0.d0/
  !	end
  ! ======================================================================|
