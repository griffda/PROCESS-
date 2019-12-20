C----------------------------------------------------------------------
      PROGRAM FISPRO
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Calls FISPACT subroutines as requested by value of WTYPE
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C
C *** Common block ALINK:
C  IX3(70)     INT    Mat numbers of first isomers
C  IX4(10)     INT    Mat numbers of second isomers
C
C *** Common block BLOCK:
C  A(18000)    REAL   The main array of data
C
C *** Common block CALIST:
C  GMS         REAL   Mass of input material (gms)
C  NSTEPS      INT    Number of subintervals (see WLVL2)
C  YSTEPS      REAL   REAL version of NSTEPS
C  ZLEVEL      REAL   Equilibrium criterion (see WLVL1)
C
C *** Common block CONVRG:
C  MAXXT       INT    Max number of integration steps (see WMAXT)
C  CONVV       REAL   Convergence criterion (see WCONV)
C
C *** Common block DATA0:
C  FLUX(3)     REAL   Proportion of flux in each group
C
C *** Common block DATA1:
C  MIND        DBLE   Minimum number of atoms, not = 0
C
C *** Common block DBLOCK:
C  B(364)      REAL   Number of atoms of each nuclide
C
C *** Common block DOSOUT:
C  XA(19)      REAL   Energy absorption coeff of air (m2/kg) in groups
C  XMU(19)     REAL   Mass attenuation coeff (m2/kg) in group format
C
C *** Common block ENDLS1:
C  ICOU        INT    Counter of number of decays in ENDF file
C  IN(364)     INT    Atomic number of each nuclide
C  IW(364)     INT    Atomic mass of each nuclide
C  KDIC        INT    Counter of number of decays in ENDF file
C
C *** Common block EXT:
C  I22         INT    No of decays & reacts with daughter not in equil
C  I31         INT    No of decays with daughter not in equilibrium
C  I32         INT    I22 + No of decays with parent & daugh in equil
C  BR1(1000)   REAL   Branching ratio of parent -> daugh in decays
C
C *** Common block IDNT:
C  VSIDNT      CHAR*4  Version number 1.0/03 = 1003
C  SPIDNT      CHAR*22 Identifier of spectrum (region)
C  XSIDNT      CHAR*15 Identifier of cross section library
C  DDIDNT      CHAR*15 Identifier of decay data library
C
C *** Common block INFO1:
C  IATMWT(364) INT    Atomic mass of each nuclide
C  FUWT(364)   REAL   Number of atoms of each nuclide
C  ISEX2       INT    Number of types of nuclide in material
C
C *** Common block INFO2:
C  IDS(364)    CHAR*4 Chemical symbol of each nuclide
C
C *** Common block LISTAG:
C  NSTART      INT    Start pointer in A()
C  NCH         INT    Number of decay libraries (=1)
C  NSPEC       INT    Number of gamma groups and data (=29)
C  NISOT       INT    Number of indep fission yield distribs (=0)
C  MTOTAL      INT    Start pointer of summed distribution data
C  MFRAC       INT    Start pointer of yield distribution data
C  N           INT    Number of nuclides in library
C  MASS        INT    Start pointer of atomic masses
C  MIDEN       INT    Start pointer of atomic numbers
C  MLAMDA      INT    Start pointer of decay constants
C  MSPEC       INT    Start pointer of disintgration & spectral data
C  MYILDS      INT    Start pointer of fission yields
C  MYIELD      INT    Start pointer of summed fission yields
C  NTRANS      INT    Number of decays in library
C  MTRANS      INT    Start pointer of decay data
C  NCROSS      INT    Number of neutron reactions in library
C  MCROSS      INT    Start pointer of reaction data
C  NYIELD      INT    Number of fission yields
C
C *** Common block OUT1:
C  FLUX2       REAL   Flux (n/cm2/s) for irradiation (see WFLUX)
C  T           REAL   Time interval (s) (see WTIME)
C
C *** Common block OUT2:
C  NAMREP(83)  CHAR*4 Chemical symbols e.g. 'AG' of elements
C
C *** Common block PROINP:
C  WELEMP(83)  REAL   % value of each element input
C  WMASS       REAL   Mass (kg) of material to be irradiated
C  WTIME       REAL   Time (s) of irradiation or cooling
C  WFLUX       REAL   Flux (n/cm2/s) during interval
C  WSPECT(100) REAL   100 group spectrum
C  WTYPE       INT    Specifies what the run of FISP_PRO does
C                     1 - prepares collapsed cross-section file
C                     2 - prepares condensed decay data
C                     3 - prepares array file
C                     4 - does an inventory run
C  WREGN       INT    Label (1 - 999) to identify the region
C
C *** Common block PROOUT:
C  WINVT(359)  REAL   The inventory output
C  WTOTA       REAL   The total activity (Bq)
C  WTOTD       REAL   The total gamma dose (Sv)
C  WTOTH       REAL   The total heat output (W)
C  WERRNM      INT    Error number
C                     0 - an error free run
C                     1 - WTYPE value not 1, 2, 3 or 4
C                     2 - Parent nuclide of reaction not in lib
C                     3 - Daughter nuclide of reaction not in lib
C                     4 - WREGN value < 1 or > 999
C                     5 - WTYPE value not 2 or 3
C                     6 - Decay lib and index file not consistent
C                     7 - Not decay data library
C                     8 - Decay mode not in library
C
C *** Common block PROSTR
C  WSPID       CHAR*22 Identifier of region and spectrum
C
C *** Common block PRPRM0
C  WLVL1       REAL   The first LEVEL parameter - C
C  WLVL2       INT    The second LEVEL parameter - N
C  WJSPEK      INT    Value of JSPEK, 1 if approx spectra calc
C  WCONV       REAL   Convergence criterion (=2.E-3)
C  WMAXT       INT    Maximum number of integrating steps (=10)
C
C *** Common block PRPRM1
C  WMIND       DBLE   The value of MIND, minimum no of atoms
C
C *** Common block SPKGN1:
C  JSPEK       INT    Set (=1) if approx spectra calc'ed (see WJSPEC)
C
C *** Common block TRANS0:
C  NP1(1000)   INT    Material numbers of parents for decays & reactions
C  ND1(1000)   INT    Material numbers of daughs for decays & reactions
C  TR1(1000)   REAL   Decay constant*branching ratio or cross section
C  I2          INT    No of decays & reactions with daugh not in equilib
C  I1          INT    Total number of decays and reactions
C  N           INT    Number of nuclides in library
C  YLAM(364)   REAL   (lambda+sigma*phi) value for each nuclide
C
C *** Common block TRANS1:
C  Y(364)      DBLE   Number of atoms of each nuclide after reordering
C
C *** Common block WALLD2:
C  LIBDAT      CHAR*72 Description of cross section library data
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      INTEGER WLVL2,WJSPEK,WMAXT
      REAL WLVL1,WCONV
      COMMON /PRPRM0/ WLVL1,WLVL2,WJSPEK,WCONV,WMAXT

      DOUBLE PRECISION WMIND
      COMMON /PRPRM1/ WMIND

      CHARACTER*22 WSPID
      COMMON /PROSTR/ WSPID

C  Local variables
      INTEGER I
      REAL TEMPSP(100)

C  External routines
      EXTERNAL AINP,CALC,COLLXS,DOSES,ENDFPR,INITVR,MASSIN,OUTPUT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Initialisations

C  Parameters set up for stand alone testing only~~~~~~~~~~~~~~~~~~~~~~
      DATA TEMPSP/2.93720E+14,1.63650E+13,4.90960E+12,5.35010E+12,
     +     6.35640E+12,6.24510E+12,5.57180E+12,5.99800E+12,5.91540E+12,
     +     5.61590E+12,5.83120E+12,6.41360E+12,7.30300E+12,8.41330E+12,
     +     9.56190E+12,1.13930E+13,1.32260E+13,1.54110E+13,1.87760E+13,
     +     1.90890E+13,2.11920E+13,2.39780E+13,2.42650E+13,2.76970E+13,
     +     2.68050E+13,3.25770E+13,2.69520E+13,3.01630E+13,3.06780E+13,
     +     3.00130E+13,3.57260E+13,3.64780E+13,3.11760E+13,2.71250E+13,
     +     2.43790E+13,1.84060E+13,2.36850E+13,2.43690E+13,2.39190E+13,
     +     1.66480E+13,1.89300E+13,1.66680E+13,1.59090E+13,1.32700E+13,
     +     1.55790E+13,1.40340E+13,1.26960E+13,1.54620E+13,1.29110E+13,
     +     2.61470E+13,2.95860E+13,2.19010E+13,2.27100E+13,1.79590E+13,
     +     2.56710E+13,2.02030E+13,1.80390E+13,1.77100E+13,1.61570E+13,
     +     1.40470E+13,1.67850E+13,1.66050E+13,1.66870E+13,1.60870E+13,
     +     1.54670E+13,1.49170E+13,1.47000E+13,1.28560E+13,1.34870E+13,
     +     1.34660E+13,1.32360E+13,1.29810E+13,1.28110E+13,1.25470E+13,
     +     1.22580E+13,1.16940E+13,1.16710E+13,1.13210E+13,1.09600E+13,
     +     1.06100E+13,9.78200E+12,9.68160E+12,9.36860E+12,8.99930E+12,
     +     8.61160E+12,8.19850E+12,7.81280E+12,7.42030E+12,7.02750E+12,
     +     6.63800E+12,6.25370E+12,5.87630E+12,5.50670E+12,5.14580E+12,
     +4.7890E+12,4.4448E+12,4.1129E+12,3.7914E+12,3.4763E+12,2.9894E+13/
C+**PJK
      WRITE(*,*) 'Enter WTYPE :'
      READ(*,*) WTYPE
      WREGN = 1
      WJSPEK = 1
      WMIND = 1.D5
      WMASS = 1.0
      WTIME = 3.15576E7
      WFLUX = 1.E15
      WLVL1 = 50.0
      WLVL2 = 1
      WCONV = 2.0E-3
      WMAXT = 10
      DO 10 I = 1,100
         WSPECT(I) = TEMPSP(I)
 10   CONTINUE
      DO 20 I = 1,83
         WELEMP(I) = 0.0
 20   CONTINUE
      WELEMP(73) = 100.0
      WSPID = 'Regn 12 - EEF reactor '
C  End of stand alone testing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      WERRNM = 0
      CALL INITVR

      IF (WTYPE.EQ.1) THEN
         CALL COLLXS
      ELSE IF (WTYPE.EQ.2) THEN
         WREGN = 0
         CALL ENDFPR
      ELSE IF (WTYPE.EQ.3) THEN
         CALL ENDFPR
      ELSE IF (WTYPE.EQ.4) THEN
         CALL AINP
         CALL MASSIN
         CALL CALC
         CALL DOSES
         CALL OUTPUT

C  Output to file for stand alone testing only~~~~~~~~~~~~~~~~~~~~~~~~~
         OPEN (4 ,FILE='PROINV.DAT',STATUS='UNKNOWN')
         WRITE (4,99000) (I,WINVT(I),I=1,359)
99000    FORMAT (1X,I4,3X,1PE11.5)
         WRITE (4,99010) WTOTA
99010    FORMAT (1X,'Total activity    = ',1PE11.5,' Bq')
         WRITE (4,99020) WTOTD
99020    FORMAT (1X,'Total g dose rate = ',1PE11.5,' Sv/h')
         WRITE (4,99030) WTOTH
99030    FORMAT (1X,'Total heat output = ',1PE11.5,' kW')
         CLOSE(4)
C  End of stand alone testing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ELSE
         WERRNM = 1
         STOP
      END IF
      END

C----------------------------------------------------------------------
      BLOCK DATA
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Initialisation of arrays and variables used in COMMON block OUT2
C  (element symbols)
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      CHARACTER*4 NAMREP(83)
      COMMON /OUT2  / NAMREP

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DATA NAMREP/'H   ','HE  ','LI  ','BE  ','B   ','C   ','N   ',
     +     'O   ','F   ','NE  ','NA  ','MG  ','AL  ','SI  ','P   ',
     +     'S   ','CL  ','AR  ','K   ','CA  ','SC  ','TI  ','V   ',
     +     'CR  ','MN  ','FE  ','CO  ','NI  ','CU  ','ZN  ','GA  ',
     +     'GE  ','AS  ','SE  ','BR  ','KR  ','RB  ','SR  ','Y   ',
     +     'ZR  ','NB  ','MO  ','TC  ','RU  ','RH  ','PD  ','AG  ',
     +     'CD  ','IN  ','SN  ','SB  ','TE  ','I   ','XE  ','CS  ',
     +     'BA  ','LA  ','CE  ','PR  ','ND  ','PM  ','SM  ','EU  ',
     +     'GD  ','TB  ','DY  ','HO  ','ER  ','TM  ','YB  ','LU  ',
     +     'HF  ','TA  ','W   ','RE  ','OS  ','IR  ','PT  ','AU  ',
     +     'HG  ','TL  ','PB  ','BI  '/

      END

C----------------------------------------------------------------------
      SUBROUTINE AINP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Reads an array file and puts all data in A() and commons.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables

      REAL A(18000)
      COMMON /BLOCK / A

      INTEGER ICOU,IN(364),IW(364),KDIC
      COMMON /ENDLS1/ ICOU,IN,IW,KDIC

      INTEGER IX3(70),IX4(10)
      COMMON /ALINK / IX3,IX4

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA,
     +     MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      COMMON /LISTAG/ NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,
     +     MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      INTEGER JSPEK
      COMMON /SPKGN1/ JSPEK

      CHARACTER*72 LIBDAT
      COMMON /WALLD2/ LIBDAT

C  Local variables
      CHARACTER*3 NUMRGN
      CHARACTER*12 TEMP
      INTEGER K,KS,KE,KAR

C  External routines
      EXTERNAL CNVNT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (WREGN.LT.1.OR.WREGN.GT.999) THEN
         WERRNM = 4
         STOP
      END IF
      CALL CNVNT(WREGN,NUMRGN)
      TEMP = 'WARRY'//NUMRGN//'.DAT'
      OPEN (13,FILE=TEMP,STATUS='OLD',ACCESS='SEQUENTIAL',
     +     FORM='UNFORMATTED')
      READ(13) LIBDAT
      READ (13) NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,
     +     MIDEN,MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS
      READ(13) (IX3(K),K=1,70)
      READ(13) (IX4(K),K=1,10)
      READ(13) (FLUX(K),K=1,3),MIND
      READ(13) (B(K),K=1,N)
      READ(13) ICOU,(IN(K),K=1,N)
      READ(13) (IW(K),K=1,N),KDIC
      KS = 1
      DO 710 KAR=1,9
         KE = KS + 1999
         READ (13) (A(K),K=KS,KE)
         KS = KE + 1
 710  CONTINUE
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE CALC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Call the integrating routine the correct number of times.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      REAL A(18000)
      COMMON /BLOCK / A

      INTEGER NSTEPS
      REAL GMS,YSTEPS,ZLEVEL
      COMMON /CALIST/ GMS,NSTEPS,YSTEPS,ZLEVEL

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      REAL FLUX2,T
      COMMON /OUT1  / FLUX2,T

      CHARACTER*4 NAMREP(83)
      COMMON /OUT2  / NAMREP

C  Local variables
      REAL TIMT,ELEVEL
      INTEGER ISTEPS,INDXG

C  External routines
      EXTERNAL CHAIN

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Loop over time steps
      DO 10 ISTEPS = 1,NSTEPS
         INDXG = 0
C  The time in the subinterval calculated
         TIMT = T/YSTEPS
C  Call CHAIN to do integration
         ELEVEL = ZLEVEL/(TIMT*YSTEPS)
         CALL CHAIN(FLUX2,INDXG,TIMT,ELEVEL)
         INDXG = INDXG + 1
 10   CONTINUE
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE CHAIN(FLUXT, INDXG, TIME, ELEVEL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  To re-order nuclides, decay types, and reaction types depending on
C  whether or not the nuclide is considered to be in an equilibrium
C  state.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  FLUXT   Irradiation flux (n/cm2/s)
C  INDXG   Start point in FLUX()
C  TIME    Time step (s)
C  ELEVEL  Equilibrium criterion
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER INDXG
      REAL ELEVEL,FLUXT,TIME

C  Global variables
      REAL A(18000)
      COMMON /BLOCK / A

      INTEGER MAXXT
      REAL CONVV
      COMMON /CONVRG/ MAXXT,CONVV

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      INTEGER I22,I31,I32
      REAL BR1(1000)
      COMMON /EXT   / I22,I31,I32,BR1

      CHARACTER*4 VSIDNT
      CHARACTER*22 SPIDNT
      CHARACTER*15 XSIDNT,DDIDNT
      COMMON /IDNT  / VSIDNT,SPIDNT,XSIDNT,DDIDNT

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      INTEGER NP1(1000),ND1(1000),I2,I1,N
      REAL TR1(1000),YLAM(364)
      COMMON /TRANS0/ NP1,ND1,TR1,I2,I1,N,YLAM

      DOUBLE PRECISION Y(364)
      COMMON /TRANS1/ Y

C  Local variables
      DOUBLE PRECISION BOLD(364)
      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,MASS,MIDEN,MLAMDA,
     +     MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      INTEGER MATOM1,I,NNN,INDX1,INDX2,INDEX1(364),INDEX2(364),
     +     M,MM,ND,NN,NP
      REAL LAMBDA(364),TCROSS(364),XLAM,YTOTAL

C  External routines
      EXTERNAL IDNTFY,INTEGS,INTEGT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      NSTART = 0
      CALL IDNTFY(NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,
     +            MASS,MIDEN,MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,
     +            NCROSS,MCROSS,NYIELD)
      MATOM1 = 0

      DO 10 I=1,N
         Y(I) = 0.0D0
         YLAM(I) = 0.0
         INDEX1(I) = 0
         INDEX2(I) = 0
         LAMBDA(I) = 0.0
 10   CONTINUE
      NNN = NTRANS + NCROSS
      DO 20 I=1,NNN
         NP1(I) = 0
         ND1(I) = 0
         TR1(I) = 0.0
 20   CONTINUE
      IF (FLUXT.LE.0.0) NCROSS = 0
C
C COPIES  DECAY  CONSTANTS INTO LAMBDA(-)
C
      INDX1 = MLAMDA
      DO 30 I=1,N
         INDX1 = INDX1 + 1
         TCROSS(I) = 0.0
         LAMBDA(I) = A(INDX1)
 30   CONTINUE
C
C CALCULATES  TOTAL CROSS  SECTIONS  TCROSS(-)
C
      INDX1 = MCROSS
      IF (NCROSS.EQ.0) GOTO 60
      DO 50 I=1,NCROSS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         INDX2 = INDXG
         INDX1 = INDX1 + 1
         INDX2 = INDX2 + 1
C ## IF statement added so that only +ve cross sections are added
C    to total. Rewritten (-ve) cross sections ignored. (July 1989)
         IF (A(INDX1).GT.0.) THEN
            TCROSS(NP) = TCROSS(NP) + A(INDX1)*FLUX(INDX2)
         END IF
 50   CONTINUE
 60   CONTINUE
      M = 0
      DO 80 I=1,N
         XLAM = LAMBDA(I)
         IF (XLAM.LT.ELEVEL) GOTO 70
         GOTO 80
 70      CONTINUE
         M = M + 1
         INDEX1(I) = M
         INDEX2(M) = I
 80   CONTINUE
      MM = M
      DO 90 I=1,N
         XLAM = LAMBDA(I)
         IF (XLAM.LT.ELEVEL) GOTO 90
         MM = MM + 1
         INDEX1(I) = MM
         INDEX2(MM) = I
 90   CONTINUE
C
C LISTS  NP1(I1)  ND1(I1)  NR1(I1)  TR1(I1)
C NP1=PARENT  ISOTOPE, ND1=DAUGHTER  ISOTOPE
C NR1=0   TR1=BRANCHING  RATIO * DECAY  CONSTANT
C NR1=1   TR1= CROSS  SECTION
C
      I1 = 0
      INDX1 = MTRANS
      IF (NTRANS.EQ.0) GOTO 130
      DO 120 I=1,NTRANS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX2 = MLAMDA + NP
         NP = INDEX1(NP)
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         IF (ND.EQ.0) GOTO 110
         ND = INDEX1(ND)
         INDX1 = INDX1 + 1
         IF (ND.GT.M) GOTO 120
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
C ## ABS added so that -ve branching ratios treated correctly (Nov89)
         TR1(I1) = ABS(A(INDX1))*A(INDX2)
         BR1(I1) = ABS(A(INDX1))
         GOTO 120
 110     CONTINUE
         INDX1 = INDX1 + 1
 120  CONTINUE
 130  CONTINUE
      I31 = I1
      INDX1 = MCROSS
      IF (NCROSS.EQ.0) GOTO 210
      DO 200 I=1,NCROSS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         NP = INDEX1(NP)
         IF (ND.EQ.0) GOTO 180
         ND = INDEX1(ND)
         IF (ND.GT.M) GOTO 180
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
         TR1(I1) = 0.0
         BR1(I1) = 0.0
         INDX2 = INDXG
         INDX1 = INDX1 + 1
         INDX2 = INDX2 + 1
C ## ABS function added so that both +ve and -ve cross sections are
C    treated correctly. (July 1989)
         TR1(I1) = TR1(I1) + ABS(A(INDX1))*FLUXT*FLUX(INDX2)
         GOTO 190
 180     CONTINUE
         INDX1 = INDX1 + 1

 190     CONTINUE
 200  CONTINUE
 210  CONTINUE
C
C LISTS  NP1(I1)  ND1(I1)  NR1(I1)  TR1(I1)
C NP1=PARENT  ISOTOPE, ND1=DAUGHTER  ISOTOPE
C NR1=0  TR1=BRANCHING  RATIO * DECAY  CONSTANT
C NR1=1  TR1=CROSS SECTION
C
      I2 = I1
      INDX1 = MTRANS
      IF (NTRANS.EQ.0) GOTO 240
      DO 230 I=1,NTRANS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX2 = MLAMDA + NP
         NP = INDEX1(NP)
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         IF (ND.EQ.0) GOTO 220
         ND = INDEX1(ND)
         INDX1 = INDX1 + 1
         IF (ND.LE.M) GOTO 230
         IF (NP.GT.M) GOTO 230
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
C ## ABS added so that -ve branching ratios treated correctly (Nov89)
         TR1(I1) = ABS(A(INDX1))*A(INDX2)
         BR1(I1) = ABS(A(INDX1))
         GOTO 230
 220     CONTINUE
         INDX1 = INDX1 + 1
 230  CONTINUE
 240  CONTINUE
      INDX1 = MCROSS
      IF (NCROSS.EQ.0) GOTO 320
      DO 310 I=1,NCROSS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         NP = INDEX1(NP)
         IF (ND.EQ.0) GOTO 290
         ND = INDEX1(ND)
         IF (ND.LE.M) GOTO 290
         IF (NP.GT.M) GOTO 290
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
         TR1(I1) = 0.0
         BR1(I1) = 0.0
         INDX2 = INDXG
         INDX1 = INDX1 + 1
         INDX2 = INDX2 + 1
C ## ABS function added so that both +ve and -ve cross sections are
C    treated correctly. (July 1989)
         TR1(I1) = TR1(I1) + ABS(A(INDX1))*FLUXT*FLUX(INDX2)
         GOTO 300
 290     CONTINUE
         INDX1 = INDX1 + 1

 300     CONTINUE
 310  CONTINUE
 320  CONTINUE
C
C LISTS  NP1(I1)  ND1(I1)  NR1(I1)  TR1(I1)
C NP1=PARENT  ISOTOPE,ND1=DAUGHTER  ISOTOPE
C NR1=0  TR1=BRANCHING  RATIO * DECAY  CONSTANT
C NR1=1  TR1=CROSS  SECTION
C
      I22 = I1
      INDX1 = MTRANS
      IF (NTRANS.EQ.0) GOTO 350
      DO 340 I=1,NTRANS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX2 = MLAMDA + NP
         NP = INDEX1(NP)
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         IF (ND.EQ.0) GOTO 330
         ND = INDEX1(ND)
         INDX1 = INDX1 + 1
         IF (ND.LE.M) GOTO 340
         IF (NP.LE.M) GOTO 340
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
C ## ABS added so that -ve branching ratios treated correctly (Nov89)
         TR1(I1) = ABS(A(INDX1))*A(INDX2)
         BR1(I1) = ABS(A(INDX1))
         GOTO 340
 330     CONTINUE
         INDX1 = INDX1 + 1
 340  CONTINUE
 350  CONTINUE
      I32 = I1
      INDX1 = MCROSS
      IF (NCROSS.EQ.0) GOTO 430
      DO 420 I=1,NCROSS
         INDX1 = INDX1 + 1
         NP = INT(A(INDX1))
         INDX1 = INDX1 + 1
         ND = INT(A(INDX1))
         NP = INDEX1(NP)
         IF (ND.EQ.0) GOTO 400
         ND = INDEX1(ND)
         IF (ND.LE.M) GOTO 400
         IF (NP.LE.M) GOTO 400
         I1 = I1 + 1
         NP1(I1) = NP
         ND1(I1) = ND
         TR1(I1) = 0.0
         BR1(I1) = 0.0
         INDX2 = INDXG
         INDX1 = INDX1 + 1
         INDX2 = INDX2 + 1
C ## ABS function added so that both +ve and -ve cross sections are
C    treated correctly. (July 1989)
         TR1(I1) = TR1(I1) + ABS(A(INDX1))*FLUXT*FLUX(INDX2)
         GOTO 410
 400     CONTINUE
         INDX1 = INDX1 + 1

 410     CONTINUE
 420  CONTINUE
 430  CONTINUE
C
C YLAM=DECAY CONSTANT  Y=NUMBER DENSITY   YTOTAL=TOTAL ATOMS
C
      YTOTAL = 0.0
      DO 440 I=1,N
         NN = INDEX2(I)
         INDX1 = MATOM1 + NN
         YLAM(I) = LAMBDA(NN) + TCROSS(NN)*FLUXT
         Y(I) = B(INDX1)
         IF (Y(I).LT.1.0D0) Y(I) = 0.0D0
 440  CONTINUE
      YTOTAL = 1.0
      IF (MAXXT.LT.10) THEN
         CALL INTEGS(M, FLUXT, YTOTAL, TIME)
         GOTO 473
      END IF
      CALL INTEGT(M, FLUXT, YTOTAL, TIME)
C
C PUTS  ATOMIC  NUMBER  DENSITIES  INTO  ORIGINAL  SCHEME
C
 473  CONTINUE
      DO 480 I=1,N
         NN = INDEX2(I)
         INDX1 = MATOM1 + NN
         BOLD(INDX1) = B(INDX1)
         IF (Y(I).LT.1.0D-60) Y(I) = 0.0D0
         B(INDX1) = Y(I)*DBLE(YTOTAL)
         BOLD(INDX1) = (BOLD(INDX1)+B(INDX1))/2.0D0
 480  CONTINUE
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE COLLXS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Prepares collapsed cross-section file.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      CHARACTER*4 VSIDNT
      CHARACTER*22 SPIDNT
      CHARACTER*15 XSIDNT,DDIDNT
      COMMON /IDNT  / VSIDNT,SPIDNT,XSIDNT,DDIDNT

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

C  Local variables
      CHARACTER*3 NUMRGN
      CHARACTER*8 DUMTIM
      CHARACTER*12 TEMP
      CHARACTER*47 CLIDNT
      INTEGER MREAC(114),NP(1000),ND(1000),NP2(1000),ND2(1000),I,N,
     +     NUCL(364),IN,IEXTRA,NUCL1,MT,NCROSS,MT1,MT2,NT1,M,N0,J0,
     +     MFICTN,JPAR(500),IFLAG,J,JJ,K,KERROR,NUCL2
      REAL FLUX(101),ZSECT(1000),ZSEKT(1000),XSECT(101),DUMMY

C  External routines
      EXTERNAL CNVNT,CLOKK

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (WREGN.LT.1.OR.WREGN.GT.999) THEN
         WERRNM = 4
         STOP
      END IF
      CALL CNVNT(WREGN,NUMRGN)
      TEMP = 'WCOLL'//NUMRGN//'.DAT'

      OPEN (6 ,FILE='FISPACT_ERROR',STATUS='UNKNOWN')
      OPEN (17,FILE= TEMP,          STATUS='UNKNOWN')
      OPEN (18,FILE='INDEXSIM.DAT', STATUS='OLD')
      OPEN (19,FILE='PROCXSEC.DAT', STATUS='OLD')

      M = 0
      N0 = 1
      J0 = 1
C Define MFICTN as the ZA&Isom number of Fe51 15/3/93
      MFICTN = 260510

      DO 10 I = 1,101
         FLUX(I) = 0.0
 10   CONTINUE
      DO 20 I = 1,114
         MREAC(I) = 0
 20   CONTINUE
      DO 30 I = 1,1000
         NP(I) = 0
         ND(I) = 0
         NP2(I) = 0
         ND2(I) = 0
         ZSECT(I) = 0.
         ZSEKT(I) = 0.
 30   CONTINUE
C
C  **READ IN LIST OF NUCLIDE IDENTIFIERS AND POSITIONS**
C
      I = 1
 50   CONTINUE
      READ (18,99001,END=60) N,NUCL(I)
      I = I + 1
      GOTO 50
C
C  **GET FLUXES (HIGH ENERGY FIRST) FROM COMMON**
C
 60   CONTINUE
      CLOSE (UNIT=18)

      DO 65 I =1,100
         FLUX(I) = WSPECT(I)
         FLUX(101) = FLUX(101) + FLUX(I)
 65   CONTINUE
C
C  **READ CROSS SECTION LIBRARY**
C
      IN = 0
C  ## Need to read start of file a 2nd time for gas nuclides (Nov89)
      IEXTRA = 0
      MREAC(16) = 10
      MREAC(17) = 20
      MREAC(22) = 20040
      MREAC(23) = 60120
      MREAC(24) = 20050
      MREAC(25) = 20060
      MREAC(28) = 10010
      MREAC(29) = 40080
      MREAC(30) = 40090
      MREAC(32) = 10020
      MREAC(33) = 10030
      MREAC(34) = 20030
      MREAC(37) = 30
      MREAC(42) = 10020
      MREAC(4) = 0
      MREAC(102) = -10
      MREAC(103) = 10000
      MREAC(104) = 10010
      MREAC(105) = 10020
      MREAC(106) = 20020
      MREAC(107) = 20030
      MREAC(108) = 40070
      MREAC(109) = 60110
      MREAC(111) = 20010
      MREAC(112) = 30040
      MREAC(113) = 50100
      MREAC(114) = 60090
 95   CONTINUE
      READ (19,99004)
      READ (19,99008) XSIDNT
      DO 100 J = 1,14
         READ (19,99004)
 100  CONTINUE
 110  CONTINUE
      IN = IN + 1
      DO 120 I = 1,101
         XSECT(I) = 0.0
 120  CONTINUE
 130  CONTINUE
      READ (19,99002,END=160) NUCL1,MT,NCROSS
      READ (19,99004)
      READ (19,99004)
      READ (19,99003) (XSECT(J),J=1,NCROSS)
      IF (NCROSS.EQ.1) GOTO 130
C  ## Need to read start of file a 2nd time for gas nuclides (Nov89)
      IF (IEXTRA.EQ.1) THEN
         IF (NUCL1.GT.20040) GOTO 160
         NUCL1 = NUCL1 + 3
      END IF
C
C  **WORK OUT DAUGHTERS AND CONVERT IDENTIFIERS**
C
      MT1 = INT(MT/10)
C
C  **CHECK FOR ISOMERIC STATE AS TARGET**
C
      NT1 = INT(NUCL1/10)
      NT1 = NT1*10
      MT2 = MT - 10*MT1
      IF (MT1.EQ.0) MT1 = 1
C  ## MODIFICATION TO ALLOW FOR FISSION. HARWELL 1991
C  @@ And to allow input of data for reactions forming
C     Fe-51. Use the ENDF MT value of 43 for these data
C     Required for testing PROCESS work 12/3/93.
      IF (MT1.EQ.18) THEN
         NUCL2 = 0
      ELSE IF (MT1.EQ.43) THEN
         NUCL2 = MFICTN
      ELSE
         NUCL2 = NT1 - MREAC(MT1) + MT2
      END IF
C
C  **CHANGE IDENTIFIERS TO POSITIONS**
C
C  ## THIS PART MADE MORE EFFICIENT HARWELL 1990
      J = J0
      KERROR = 0
 132  CONTINUE
      IF (NUCL1.NE.NUCL(J)) THEN
         J = J+1
         IF (J.LT.N) GOTO 132
         J = 1
         IF (KERROR.EQ.1) THEN
            WRITE(6,99007) NUCL1,MT
            WERRNM = 2
            STOP
         END IF
         KERROR = KERROR + 1
         GOTO 132
      END IF
      NP(IN) = J
      J0 = J
      J = N0
      KERROR = 0
 134  CONTINUE
      IF (NUCL2.EQ.0) THEN
         J = 0
      ELSE IF (NUCL2.NE.NUCL(J)) THEN
         J = J+1
         IF (J.LT.N) GOTO 134
         J = 1
         IF (KERROR.EQ.1) THEN
            WRITE(6,99007) NUCL1,MT
            WERRNM = 3
            STOP
         END IF
         KERROR = KERROR + 1
         GOTO 134
      ELSE
         CONTINUE
      END IF
      ND(IN) = J
      N0 = J0-80
      IF (N0.LT.1) N0 = 1
C  ## END OF MODS
      IF (MT.EQ.41.AND.ND(IN).EQ.0) GOTO 130
C
C  **COLLAPSE ENERGY GROUPS**
C
      DO 150 J = 1,NCROSS
         XSECT(101) = XSECT(101) + (XSECT(J)*FLUX(J))
 150  CONTINUE
      XSECT(101) = XSECT(101)/FLUX(101)
C  ## Do not use data if effective cross section < 1.E-12 barns
C  ## Harwell 21/11/90
      IF (XSECT(101).LT.1.E-12) GOTO 130
      ZSECT(IN) = XSECT(101)
C  ## By repeating the 5 gas nuclides, (in all libraries), try
C     to get all gases (Nov 89).
C  ## Correction made to include reactions to isomers, 
C     change MT to MT1 and divide reac number by 10. Mar 90
C  ## Additional reactions for He4 production (Nov 1989)##
      IF (MT1.EQ.107.OR.MT1.EQ.22.OR.MT1.EQ.24) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
C  ## Additional reactions for He3 production (Nov 1989)##
      IF (MT1.EQ.106.OR.MT1.EQ.34) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-1
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
C  ## Additional reactions for H3 production (Nov 1989)##
      IF (MT1.EQ.105.OR.MT1.EQ.33) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-2
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
C  ## Additional reactions for H2 production (Nov 1989)##
      IF (MT1.EQ.104.OR.MT1.EQ.32) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-3
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
C  ## Additional reactions for H1 production (Nov 1989)##
      IF (MT1.EQ.103.OR.MT1.EQ.28.OR.MT1.EQ.42) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-4
         ZSECT(IN) = -ZSECT(IN-1)
         GOTO 110
      END IF
C  ## Additional reactions for H1 production (Nov 1989)##
      IF (MT1.EQ.111) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N-4
         ZSECT(IN) = -2.*ZSECT(IN-1)
         GOTO 110
      END IF
C  ## Additional reactions for He4 production (Nov 1989)##
      IF (MT1.EQ.29) THEN
         IN = IN+1
         NP(IN) = NP(IN-1)
         ND(IN) = N
         ZSECT(IN) = -2.*ZSECT(IN-1)
      END IF
      GOTO 110
C  ## Need to read start of file a 2nd time for gas nuclides (Nov89)
 160  CONTINUE
      IEXTRA = IEXTRA + 1
      IF (IEXTRA.EQ.1) THEN
         REWIND 19
         IN = IN - 1
         GOTO 95
      END IF
C
C  **IF SAME NP AND ND OCCUR FOR GIVEN NUCLIDE, THEN SUM X-SECTIONS
C  **SO THAT REACTION OCCURS BUT ONCE.
C
C  ##Sum cross sections only if both +ve or -ve. This means that some
C    -ve cross sections (for light elements) are ignored. (July 1989)
C  ## By including the 5 gas nuclides should remove problem (Nov 89).
C  ## HARWELL MODIFICATION NOV 1990.
C  ## MAKES SUMMING MORE EFFICIENT
      N0 = NP(1)
      JPAR(1) = 1
      J0 = 1
      DO 172 J = 1,IN
         IF (NP(J).NE.N0) THEN
            J0 = J0 +1
            N0 = NP(J)
            JPAR(J0) = J
         END IF
 172  CONTINUE
      DO 180 JJ = 1,J0-1
         DO 178 J = JPAR(JJ),JPAR(JJ+1)-1
            M = M + 1
            IFLAG = 0
            DO 176 K = J+1,JPAR(JJ+1)-1
               IF (ND(J).EQ.ND(K)) THEN
                  IF (ZSECT(J).GT.0.) THEN
                     IF (ZSECT(K).GT.0.) THEN
                        ZSECT(K) = ZSECT(J) + ZSECT(K)
                     ELSE
                        ZSECT(K) = ZSECT(J)
                     END IF
                  ELSE
                     IF (ZSECT(K).LT.0.) THEN
                        ZSECT(K) = ZSECT(J) + ZSECT(K)
                     END IF
                  END IF
                  IFLAG = IFLAG + 1
                  IF (IFLAG.EQ.1) M = M - 1
C  ## NEXT LINE ADDED 26/6/91. PROBLEM IS THAT IF THREE REACTIONS HAVE
C     THE SAME DAUGHTER THEN THE FIRST CROSS SECTION WAS ADDED TWICE.
                  GOTO 177
               END IF
 176        CONTINUE
 177        CONTINUE
            IF (IFLAG.LE.0) THEN
               NP2(M) = NP(J)
               ND2(M) = ND(J)
               ZSEKT(M) = ZSECT(J)
            END IF
 178     CONTINUE
 180  CONTINUE
C  ## END OF HARWELL MODIFICATION
C
C  **NOW WRITE OUT IN FISPIN FORM**
C
C  #  GET CURRENT DATE
      CALL CLOKK(DUMMY,DUMTIM,TEMP)
      CLIDNT = TEMP(1:2)//TEMP(4:5)//TEMP(7:8)//
     +     VSIDNT//XSIDNT//SPIDNT
      WRITE (17,99006) M,CLIDNT
      WRITE (17,99005) (NP2(I),ND2(I),ZSEKT(I),I=1,M)
      WRITE (17,99010)
      WRITE (17,99011) (FLUX(I),I=1,100)
C
      RETURN
C
C **FORMAT STATEMENTS**
99001 FORMAT (I6,10X,I6)
99002 FORMAT (1X,I6,1X,I4,1X,I4)
99003 FORMAT (6E12.5)
99004 FORMAT (A80)
99005 FORMAT (1X,I4,2X,I4,4X,1PE12.5)
99006 FORMAT (1X,I5,2X,A47)
99007 FORMAT (1X,'NUCL1 = ',I6,'  MT = ',I6)
99008 FORMAT (6X,A15)
99010 FORMAT (1X,'100')
99011 FORMAT (1X,E11.5)
      END

C----------------------------------------------------------------------
      SUBROUTINE CLOKK(T,H,D)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Routine to return the data and time
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  T      : (OUTPUT) Time in seconds
C  H      : (OUTPUT) Time in HH:MM:SS format
C  D      : (OUTPUT) Date in DD/MM/YY format
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*8 H,D
      REAL T

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C+**PJK 26/11/93 CHARACTER*8 EDATE,TIME

C+**PJK 26/11/93 Commented out CLOCK call : CALL CLOCK(T)
      T = 0.0

C+**PJK 26/11/93 Commented out TIME call : H=TIME()
      H = 'HH:MM:SS'

C+**PJK 25/11/93 Commented out EDATE call : D=EDATE()
      D = 'DD/MM/YY'

      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE CNVNT(NUMB,CNUMB)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Converts between region number and the character representation
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  NUMB   : (INPUT)  An integer in the range 0 - 999
C  CNUMB  : (OUTPUT) The character*3 representation e.g. '006'
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*3 CNUMB
      INTEGER NUMB

C  Local variables
      INTEGER IH,IT,IU,NZERO

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      NZERO = ICHAR('0')
      IH = INT(NUMB/100)
      IF (IH.EQ.0) THEN
         CNUMB(1:1) = '0'
      ELSE
         CNUMB(1:1) = CHAR(IH+NZERO)
      END IF

      IT = INT((NUMB - 100 * IH)/10)
      IF (IT.EQ.0) THEN
         CNUMB(2:2) = '0'
      ELSE
         CNUMB(2:2) = CHAR(IT+NZERO)
      END IF

      IU = NUMB - 100 * IH - 10 * IT
      IF (IU.EQ.0) THEN
         CNUMB(3:3) = '0'
      ELSE
         CNUMB(3:3) = CHAR(IU+NZERO)
      END IF

      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE CONV(ZB,MAT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Converts between material numbers and ENDF identifiers
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  ZB    : (INPUT)  Standard ENDF/B 'ZA' identifier
C  MAT   : (OUTPUT) Material number (0 - N)
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER MAT
      REAL ZB

C  Global variables
      INTEGER IX3(70),IX4(10)
      COMMON /ALINK / IX3,IX4

      REAL A(18000)
      COMMON /BLOCK / A

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

C  Local variables
      INTEGER I,IA,IZ,M,MASS,MATA,MATB,MC,MEND,MIDEN,MIDF,N,NISOT,NSP,
     +     NST
      REAL AW,ZA,ZN

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      IF (ZB.GT.0.0) THEN
         NST = 3
         NSP = INT(A(NST))
         NST = NST + 18*NSP + 1
         NISOT = INT(A(NST))
         NST = NST + NSP + NISOT + 1
         N = INT(A(NST))
         MASS = NST
         NST = NST + N
         MIDEN = NST
         NST = NST + N
         MEND = NST
         MIDF = MIDEN + 1
         ZA = ZB/1000.0
         IZ = INT(ZA)
         ZN = REAL(IZ)
         AW = ZB - 1000.0*ZN
         DO 10 I = MIDF,MEND
            IF (INT(A(I)).EQ.IZ) THEN
               IA = I - N
               IF (INT(A(IA)).EQ.INT(AW)) GOTO 20
            END IF
 10      CONTINUE
      END IF
      MAT = 0
      GOTO 30
 20   CONTINUE
      MAT = I - MIDEN
 30   CONTINUE
      GOTO 1000
C-----------------------------------------------------------------------
C-    TESTS IF ISOMER IS IN LIBRARY
C-----------------------------------------------------------------------
      ENTRY CONXA(M,MATA,MC)
      IF (M.EQ.2) THEN
         MATB = MATA + 2
         DO 40 I = 1,10
            IF (MATB.EQ.IX4(I)) GOTO 60
 40      CONTINUE
      ELSE
         MATB = MATA + 1
         DO 50 I = 1,70
            IF (MATB.EQ.IX3(I)) GOTO 60
 50      CONTINUE
      END IF
      MC = 0
      GOTO 1000

 60   CONTINUE
      MC = MATB
      GOTO 1000
C-----------------------------------------------------------------------
C-    RETURNS AN INTEGER DEPENDING ON THE ISOMERIC STATE
C-----------------------------------------------------------------------
      ENTRY CONYA(MAT,MC)
      DO 70 I = 1,70
         IF (MAT.EQ.IX3(I)) GOTO 90
 70   CONTINUE
      DO 80 I = 1,10
         IF (MAT.EQ.IX4(I)) GOTO 100
 80   CONTINUE
      MC = 1
      GOTO 1000

 90   CONTINUE
      MC = 2
      GOTO 1000

 100  CONTINUE
      MC = 3

 1000 CONTINUE
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE DOSES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Calculates absorption and attenuation coefficients. Data in the
C  common DOSOUT are used in routine OUTPUT to calculate dose rates.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      REAL XA(19),XMU(19)
      COMMON /DOSOUT/ XA,XMU

      INTEGER IATMWT(364),ISEX2
      REAL FUWT(364)
      COMMON /INFO1 / IATMWT,FUWT,ISEX2

      CHARACTER*4 IDS(364)
      COMMON /INFO2 / IDS

      REAL FLUX2,T
      COMMON /OUT1  / FLUX2,T

      CHARACTER*4 NAMREP(83)
      COMMON /OUT2  / NAMREP

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

C  Local variables
      INTEGER IZZ(40),IZ(83),JZZ(50),DFLAG,I,IJ,IK,IL,J,K,KH,KL,M1
      REAL XAA(17),E(19),EE(17),XM(19,50),XMM(17,40),FWT(83),
     +     YM(19,50),AX,AY,DE,DZ,FAWT,TOTT

C  External functions

C  External routines

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C+**PJK 26/11/93
      SAVE DFLAG

      DATA DFLAG/0/

      DATA E/.15,.25,.35,.5,.7,.9,1.11,1.33,1.55,1.83,2.25,2.75,3.5,
     +     4.5,5.75,7.25,9.0,11.0,13.0/

      DATA EE/.15,.2,.3,.4,.5,.6,.8,1.,1.5,2.,3.,4.,5.,6.,8.0,10.0,15.0/

      DATA IZZ/1,3,4,5,6,7,8,9,11,12,13,14,15,16,17,18,19,20,22,25,26,
     +     29,30,31,32,33,35,47,48,50,52,53,55,56,58,74,79,80,82,92/

      DATA XAA/2.494,2.672,2.872,2.949,2.966,2.953,2.882,2.787,2.545,
     +     2.342,2.054,1.866,1.737,1.644,1.521,1.446,1.349/

      DATA ((XMM(I,J),I=1,14),J=1,10)/26.51,24.29,21.12,18.93,17.29,
     +     15.99,14.05,12.63,10.27,8.77,6.923,5.807,5.049,4.498,11.57,
     +     10.6,9.208,8.249,7.532,6.968,6.121,5.503,4.475,3.83,3.042,
     +     2.572,2.257,2.03,11.9,10.89,9.46,8.473,7.737,7.156,6.286,
     +     5.652,4.597,3.937,3.138,2.664,2.347,2.121,12.43,11.36,9.865,
     +     8.834,8.065,7.46,6.552,5.89,4.792,4.108,3.284,2.799,2.477,
     +     2.248,13.47,12.29,10.66,9.545,8.712,8.058,7.077,6.362,5.177,
     +     4.443,3.562,3.047,2.708,2.469,13.53,12.33,10.68,9.555,8.72,
     +     8.064,7.082,6.366,5.181,4.45,3.579,3.073,2.742,2.511,13.6,
     +     12.37,10.7,9.567,8.729,8.071,7.087,6.37,5.186,4.458,3.597,
     +     3.1,2.777,2.553,12.98,11.76,10.15,9.072,8.275,7.65,6.716,
     +     6.036,4.915,4.229,3.422,2.96,2.663,2.457,13.35,11.99,10.29,
     +     9.185,8.371,7.736,6.788,6.1,4.968,4.283,3.486,3.038,2.753,
     +     2.559,13.93,12.45,10.65,9.491,8.646,7.988,7.008,6.296,5.129,
     +     4.425,3.613,3.159,2.873,2.681/

      DATA ((XMM(I,J),I=1,14),J=11,20)/13.78,12.23,10.42,9.276,8.446,
     +     7.801,6.842,6.146,5.007,4.324,3.541,3.107,2.836,2.655,14.47,
     +     12.75,10.82,9.614,8.748,8.077,7.082,6.361,5.183,4.48,3.679,
     +     3.24,2.967,2.788,14.31,12.5,10.55,9.359,8.51,7.855,6.884,
     +     6.182,5.038,4.359,3.59,3.172,2.915,2.748,15.05,13.01,10.91,
     +     9.666,8.782,8.103,7.099,6.374,5.195,4.498,3.716,3.294,3.037,
     +     2.872,14.79,12.65,10.53,9.311,8.452,7.795,6.826,6.127,4.994,
     +     4.328,3.585,3.189,2.95,2.798,14.25,12.04,9.95,8.775,7.957,
     +     7.334,6.419,5.761,4.696,4.073,3.384,3.019,2.803,2.666,15.79,
     +     13.18,10.79,9.493,8.599,7.921,6.929,6.217,5.067,4.4,3.665,
     +     3.281,3.055,2.915,16.71,13.74,11.15,9.781,8.849,8.146,7.122,
     +     6.388,5.207,4.525,3.78,3.395,3.17,3.034,16.46,13.13,10.42,
     +     9.08,8.191,7.529,6.572,5.891,4.802,4.18,3.511,3.173,2.981,
     +     2.868,18.34,13.89,10.62,9.131,8.191,7.508,6.536,5.852,4.768,
     +     4.161,3.524,3.213,3.044,2.951/

      DATA ((XMM(I,J),I=1,14),J=21,30)/19.6,14.58,10.98,9.398,8.413,
     +     7.703,6.698,5.994,4.883,4.265,3.622,3.311,3.146,3.057,22.1,
     +     15.57,11.18,9.409,8.36,7.624,6.605,5.9,4.803,4.204,3.599,
     +     3.318,3.176,3.108,23.35,16.15,11.41,9.537,8.45,7.694,6.656,
     +     5.942,4.835,4.236,3.635,3.36,3.225,3.161,23.8,16.17,11.22,
     +     9.321,8.234,7.486,6.465,5.767,4.692,4.113,3.539,3.28,3.156,
     +     3.099,24.84,16.58,11.3,9.326,8.212,7.453,6.427,5.728,4.658,
     +     4.087,3.525,3.276,3.159,3.108,26.14,17.15,11.49,9.408,8.257,
     +     7.481,6.439,5.735,4.661,4.093,3.539,3.297,3.187,3.141,28.89,
     +     18.34,11.84,9.554,8.325,7.513,6.442,5.727,4.65,4.089,3.553,
     +     3.326,3.229,3.194,54.04,29.63,15.57,11.3,9.314,8.145,6.762,
     +     5.919,4.754,4.209,3.754,3.606,3.577,3.601,55.7,30.29,15.68,
     +     11.27,9.244,8.058,6.666,5.826,4.673,4.139,3.698,3.559,3.536,
     +     3.564,60.67,32.49,16.36,11.55,9.364,8.109,6.661,5.8,4.639,
     +     4.113,3.687,3.562,3.549,3.584/

      DATA ((XMM(I,J),I=1,14),J=31,40)/64.65,34.16,16.75,11.61,9.317,
     +     8.013,6.533,5.667,4.518,4.009,3.606,3.495,3.492,3.534,69.48,
     +     36.5,17.68,12.15,9.69,8.304,6.744,5.838,4.647,4.124,3.716,
     +     3.607,3.608,3.655,75.89,39.41,18.62,12.57,9.911,8.431,6.789,
     +     5.852,4.641,4.121,3.725,3.625,3.635,3.689,78.27,40.45,18.91,
     +     12.65,9.922,8.41,6.744,5.801,4.592,4.078,3.692,3.598,3.611,
     +     3.669,86.86,44.52,20.39,13.42,10.41,8.757,6.963,5.962,4.701,
     +     4.176,3.791,3.704,3.726,3.792,158.1,78.44,32.38,19.25,13.78,
     +     10.93,8.065,6.616,5.,4.432,4.075,4.037,4.103,4.211,186.,
     +     92.14,37.45,21.8,15.3,11.94,8.604,6.953,5.167,4.568,4.201,
     +     4.166,4.239,4.355,190.9,94.56,38.34,22.24,15.55,12.1,8.679,
     +     6.993,5.179,4.575,4.208,4.172,4.245,4.362,201.4,99.85,40.26,
     +     23.23,16.13,12.48,8.869,7.103,5.222,4.607,4.234,4.197,4.272,
     +     4.391,259.1,129.8,51.91,29.22,19.76,14.9,10.16,7.894,5.586,
     +     4.876,4.446,4.391,4.463,4.583/

      DATA ((XMM(I,J),I=15,17),J=1,40)/3.746,3.254,2.539,1.725,1.529,
     +     1.252,1.819,1.827,1.360,1.945,1.755,1.495,2.154,1.960,1.698,
     +     2.209,2.024,1.783,2.263,2.089,1.866,2.195,1.039,1.846,2.319,
     +     2.181,2.022,2.445,2.313,2.168,2.437,2.318,2.195,2.574,2.462,
     +     2.352,2.552,2.452,2.364,2.683,2.590,2.517,2.628,2.549,2.496,
     +     2.517,2.451,2.419,2.766,2.704,2.687,2.893,2.839,2.878,2.760,
     +     2.728,2.761,2.876,2.871,2.951,2.991,2.994,3.092,3.074,3.103,
     +     3.247,3.138,3.175,3.335,3.086,3.130,3.300,3.104,3.156,3.341,
     +     3.146,3.207,3.405,3.218,3.293,3.521,3.723,3.882,4.277,3.691,
     +     3.853,4.253,3.724,3.896,4.316,3.683,3.860,4.290,3.815,4.002,
     +     4.455,3.860,4.057,4.529,3.843,4.042,4.518,3.981,4.194,4.699,
     +     4.472,4.747,5.384,4.633,4.926,5.598,4.643,4.937,5.613,4.675,
     +     4.972,5.658,4.879,5.194,5.926/

C**** All values need a factor of .001
      IF (DFLAG.LE.0) THEN
         DO 20 I = 1,17
            DO 10 J = 1,40
               XMM(I,J) = XMM(I,J)*.001
 10         CONTINUE
            XAA(I) = XAA(I)*.001
 20      CONTINUE
C**** Calculates the two arrays by interpolation
         DO 60 J = 1,19
            DO 25 I = 41,50
               XM(J,I) = 0.0
 25         CONTINUE
            K = 1
 30         CONTINUE
            IF (EE(K).GE.E(J)) THEN
               KH = K
               IF (ABS(EE(KH)-E(J)).LT. 0.001) THEN
                  XA(J) = XAA(KH)
                  DO 40 I = 1,40
                     XM(J,I) = XMM(KH,I)
                     YM(J,I) = 0.0
 40               CONTINUE
               ELSE
                  KL = K - 1
                  DE = (E(J)-EE(KL))/(EE(KH)-EE(KL))
                  XA(J) = XAA(KL) + DE*(XAA(KH)-XAA(KL))
                  DO 50 I = 1,40
                     XM(J,I) = XMM(KL,I) + DE*(XMM(KH,I)-XMM(KL,I))
                     YM(J,I) = 0.0
 50               CONTINUE
               END IF
            ELSE
               K = K + 1
               GOTO 30
            END IF
 60      CONTINUE
         DFLAG = 1
C**** Calculates the weight fraction of initial materials
         TOTT = 0.0
         FAWT = 0.0
         IJ = 0
         DO 80 J = 1,83
            FWT(J) = 0.0
            IZ(J) = 0
            IK = 0
            DO 70 K = 1,ISEX2
               IF (IDS(K).EQ.NAMREP(J)) THEN
                  FAWT = FUWT(K)*REAL(IATMWT(K))
                  FWT(J) = FWT(J) + FAWT
                  IK = 1
               END IF
 70         CONTINUE
            IF (IK.NE.0) THEN
               IJ = IJ + 1
               IZ(IJ) = J
               TOTT = TOTT + FWT(J)
            END IF
 80      CONTINUE
         DO 90 J = 1,83
            FWT(J) = FWT(J)/TOTT
 90      CONTINUE
         TOTT = TOTT*1.66056559E-27
         TOTT = WMASS
C**** Identifies the materials for which mu will need calculating
         IL = 0
         DO 120 J = 1,IJ
            K = 1
 100        CONTINUE
            IF (IZZ(K).GE.IZ(J)) THEN
               KH = K
               IF (IZZ(KH).NE.IZ(J)) THEN
                  KL = K - 1
                  DZ = REAL(IZ(J)-IZZ(KL))/REAL(IZZ(KH)-IZZ(KL))
                  IL = IL + 1
                  JZZ(IL) = IZ(J)
                  DO 110 I = 1,19
                     YM(I,IL) = XM(I,KL) + DZ*(XM(I,KH)-XM(I,KL))
 110              CONTINUE
               END IF
            ELSE IF (K.LT.40) THEN
               K = K + 1
               GOTO 100
            ELSE
               KH = 40
               KL = 39
               DZ = REAL(IZ(J)-IZZ(KL))/REAL(IZZ(KH)-IZZ(KL))
               IL = IL + 1
               JZZ(IL) = IZ(J)
               DO 111 I = 1,19
                  YM(I,IL) = XM(I,KL) + DZ*(XM(I,KH)-XM(I,KL))
 111           CONTINUE
            END IF
 120     CONTINUE
C**** Calculates the weighted values of MU over original materials
         DO 130 I = 1,19
            XMU(I) = 0.0
 130     CONTINUE
         DO 170 J = 1,IJ
            K = 1
            M1 = 0
 140        CONTINUE
            IF (IZ(J).NE.IZZ(K)) THEN
               K = K + 1
               IF (K.LE.40) GOTO 140
               M1 = 1
               K = 1
 150           CONTINUE
               IF (IZ(J).NE.JZZ(K)) THEN
                  K = K + 1
                  GOTO 150
               END IF
            END IF
            AX = 1.
            AY = 0.
            IF (M1.NE.0) THEN
               AX = 0.
               AY = 1.
            END IF
            DO 160 I = 1,19
               XMU(I) = XMU(I) + FWT(IZ(J))*(AX*XM(I,K)+AY*YM(I,K))
 160        CONTINUE
 170     CONTINUE
      END IF

      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE ENDFP(INA)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Reads an ENDF/BV decay data file. If no gamma spectral data are
C  given then an approximate spectrum is calculated if 'SPEK' is set.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  INA    : (INPUT)  Stream to input decay library
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER INA

C  Global variables
      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      REAL A(18000)
      COMMON /BLOCK / A

      INTEGER ICOU,IN(364),IW(364),KDIC
      COMMON /ENDLS1/ ICOU,IN,IW,KDIC

      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA,
     +     MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      COMMON /LISTAG/ NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,
     +     MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD

      INTEGER JSPEK
      COMMON /SPKGN1/ JSPEK

C  Local variables
      CHARACTER*1 NUMCHR
      CHARACTER*4 ANOT(20)
      CHARACTER*50 FILENM,TEMPFN

      REAL AE(6),XEXP(25),ENAC(25),XX(200),YY(200)
      REAL A1,A2,A9,AW,AWI,AWR,B1,B2,BR,D7,DCK,DELB,DELQ,DELT,DER,DERB,
     +     DFC,DFD,DIC,DIL,DIS,DRI,EFACT,ELIS,EMAX,EMAY,ENAS,ENI,ER,ERB,
     +     FC,FD,FLODIV,FNUBAR,PAR,QUE,RFS,RI,RICC,RICK,RICL,RIS,RTYP,
     +     SPI,STA,STYP,SUMAIN,SUMBR,THAL,TYP,XINT,Y1,ZA,ZAA

      INTEGER IDMF(50,3),ILA(6),NBT(200),INT1(200)
      INTEGER I,I1,I2,IC,ICC,ID,IL,ILINES,IMAX,IREAD,IRS,IRT,IST,IZ,J,
     +     JA,JCOU,K,K1,K2,K3,K4,KCOU,L3,LCON,LENGTH,LFI,LIS,LISO,LRP,
     +     M1,M2,M3,M4,M5,M6,MAT,MATA,MATD,MC,MCO,MCP,MD,MF,MT,N1,NB,
     +     NDK,NER,NEUTC,NFOR,NLIB,NLIN,NMOD,NN,NP,NR,NSP,NSPG,NSUB,
     +     NT,NUM,NUMBR,NUME,NVER,NWD,NXC

C  External functions
      INTEGER LENGT
      EXTERNAL LENGT

C  External routines
      EXTERNAL CONV,CONXA,CONYA

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      DATA ENAC/0.0,0.01,0.02,0.05,0.1,0.2,0.3,0.4,0.6,0.8,1.0,1.22,
     +     1.44,1.66,2.0,2.5,3.0,4.0,5.0,6.5,8.0,10.0,12.0,14.0,20.0/

C ENERGY BOUNDARIES

      NB = NSPEC - 5
C
C **READING ENDF/BV GENERAL DATA(MT=451)
C
      JA = 0
      MATD = 0
      MAT = 0
C  ## Changes so that decay data is read in from several files
C     It is assumed that the files have the extension .00n
C
      IREAD = 0
 65   CONTINUE
      CLOSE(UNIT=INA)

      OPEN (INA,FILE='PROCDEC1.001',STATUS='UNKNOWN')
      READ (INA,99002) ILINES
      DO 70 NLIN = 1,ILINES
         READ (INA,99001) (ANOT(J),J=1,17)
 70   CONTINUE
      MAT = 0
      IF (IREAD.EQ.1) MATD = MATD - 1

 80   CONTINUE
      IF (MAT.EQ.-1) GOTO 210
      IF ( MATD.EQ.N) GOTO 210

 81   CONTINUE
      READ (INA,99003,END=1000)
     +     ZA,AWR,LRP,LFI,NLIB,NMOD,MAT,MF,MT,NUM
      JA = JA + 1
C
C **NLIB IS LIBRARY IDENTIFIER(=0 FOR ENDF)
C **NMOD IS MODIFICATION NO.
C
      MATD = MATD + 1
      IF (MAT.EQ.-1) GOTO 210
      MD = MATD
C
C **MAT MATERIAL NO. ON DATA TAPE.MATD PROGRAM COUNT OF MATERIALS
C
      READ (INA,99003) ELIS,STA,LIS,LISO,N1,NFOR,MAT,MF,MT,NUM
      JA = JA + 1
C
C **STA=0=STABLE NUCLIDE,STA=1=UNSTABLE NUCLIDE
C
C **TEST FOR ENDFB6 FORMAT DATA
C
      IF (NFOR.EQ.6) THEN
         JA = JA + 1
         READ (INA,99003) AWI,AWI,N1,N1,NSUB,NVER,MAT,MF,MT,NUM
         IF (NSUB.NE.4) THEN
            WRITE (6,99004) ZA
            WERRNM = 7
            STOP
         END IF
      END IF

      READ (INA,99003) A1,A2,M3,M4,NWD,NXC,MAT,MF,MT,NUM
      JA = JA + 1
C
C **NWD NO. OF COMMENT CARDS
C **NXC NO. OF DICTIONARY CARDS
C
      DO 90 I = 1,NWD
         READ (INA,99005) (ANOT(NN),NN=1,17),MAT,MF,MT,NUM
         JA = JA + 1
 90   CONTINUE
      DO 100 I = 1,NXC
         READ (INA,99003) B1,B2,IDMF(I,1),IDMF(I,2),IDMF(I,3),IL,MAT,MF,
     +        MT,NUM
         JA = JA + 1
 100  CONTINUE
      JA = 0
      MCO = 1
      MCP = MCO + 1
C
C ** JA=COUNTER ON INPUT ITEMS
C ** MCO=CURRENT NO. OF DECAY REACTIONS
C
C ****** SEND ******
C
 110  CONTINUE
      READ (INA,99006) (ILA(I),I=1,6),MAT,MF,MT,NUM
      KDIC = KDIC + 1
C
C ** KDIC=NO. OF DECAY REACTIONS
C
      IF (MT.GT.0) WRITE (6,99007) NUM
      IF (MCO.LE.364) THEN
C
C ****** FEND ******
C
         READ (INA,99008) MAT,MF,MT,NUM
         IF (MF.NE.0) WRITE (6,99009) NUM
         READ (INA,99003) ZA,AWR,LIS,LISO,M1,NSP,MAT,MF,MT,NUM
         IF (MF.LE.0) GOTO 80
         IF (INT(STA).EQ.0) GOTO 80
         MCO = MCO + 1
         MCP = MCO + 1
C
C ** NSP TOTAL NO. OF RADIATION SPECTRA
C
         READ (INA,99010) THAL,DELT,M2,M3,NUME,M4,MAT,MF,MT,NUM,
     +        (AE(NN),NN=1,NUME)
C
C **THE ARRAY AE(N) CONTAINS 6 NOS:EB,DEB,EG,DEG,EA,DEA
C **THEY ARE THE MEAN ENERGIES PER DISINTEGRATION & ARE REQUIRED
C **FOR DECAY HEAT CALCULATIONS
C
         JA = JA + 1
         MATA = MATD + MLAMDA
         A(MATA) = 0.693147/THAL
         READ (INA,99003) SPI,PAR,M5,M6,L3,NDK,MAT,MF,MT,NUM
C
C **NDK IS THE TOTAL NO. OF DECAY MODES GIVEN,L3=6*NDK
C
         JA = JA + 1
         SUMBR = 0.0
         EMAX = 0.0
         A9 = 14.
         DO 160 I = 1,NDK
            ICOU = ICOU + 1
            READ (INA,99011) RTYP,RFS,QUE,DELQ,BR,DELB,MAT,MF,MT,NUM
            JA = JA + 1
            SUMBR = SUMBR + BR
            IRT = INT(RTYP)
            IF (ABS(RTYP-1.5).LT. 0.001) IRT = 11
            IF (ABS(RTYP-1.4).LT. 0.001) IRT = 12
C
C **RTYP IS THE CODE WHICH DEFINES THE DECAY MODE
C
            A(ICOU) = REAL(MATD)
            MC = MATD
            IF (I.LE.1) THEN
C
C **NEXT SECTION DONE ONCE FOR EACH NUCLIDE
C
               JCOU = MSPEC + NSPEC*(MC-1) + 1
               JCOU = JCOU + 1
               DO 120 IC = 1,3
                  JCOU = JCOU + 1
                  IF (IC.EQ.1) ICC = 5
C A(JCOU)=QUE*1.0E-6
                  IF (IC.EQ.2) ICC = 1
                  IF (IC.EQ.3) ICC = 3
                  A(JCOU) = AE(ICC)*1.0E-6
 120           CONTINUE
            END IF
            IRS = INT(RFS)
C
C **RFS IS DAUGHTER STATE(0=GROUND,1=ISOMERIC)
C
            ICOU = ICOU + 1
C
C **IRT=1 BETA - DECAY
C **IRT=2 BETA + DECAY
C **IRT=3 ISOMERIC TRANSITION
C **IRT=4 ALPHA DECAY
C **IRT=5 NEUTRON EMISSION
C **IRT=6 SP. FISSION DECAY
C **IRT=7 PROTON EMISSION
C **IRT=10 UNKNOWN ORIGIN
C **IRT=11 BETA- DECAY & NEUTRON EMISSION
C **IRT=12 BETA- DECAY & ALPHA EMISSION
C
            IF (IRT.EQ.1) THEN
            ELSE IF (IRT.EQ.2) THEN
            ELSE IF (IRT.EQ.4) THEN
            ELSE IF (IRT.EQ.5) THEN
            ELSE IF (IRT.EQ.6) THEN
C
C **DECAY IS BY SP. FISSION
C
               ZAA = 0.0
C
C **DAUGHTER NOT IN LIBRARY
C
               A(ICOU) = 0.0
               GOTO 130
            ELSE IF (IRT.EQ.7) THEN
            ELSE IF (IRT.EQ.8) THEN
               GOTO 140
            ELSE IF (IRT.EQ.9) THEN
               GOTO 140
            ELSE IF (IRT.EQ.10) THEN
               WRITE (6,99012) IRT
               GOTO 150
            ELSE IF (IRT.EQ.11) THEN
            ELSE IF (IRT.EQ.12) THEN
            ELSE IF (IRT.EQ.13) THEN
               GOTO 140
            ELSE
               CALL CONYA(MATD,MC)
               IF (MC.EQ.2) THEN
                  A(ICOU) = REAL(MATD - 1)
               ELSE IF (MC.EQ.3) THEN
                  IF (IRS.EQ.0) A(ICOU) = REAL(MATD - 2)
                  IF (IRS.EQ.1) A(ICOU) = REAL(MATD - 1)
                  IF (IRS.EQ.2) A(ICOU) = REAL(MATD)
               ELSE
                  A(ICOU) = REAL(MATD)
               END IF
               GOTO 130
            END IF
            FLODIV = ZA/1000.
            IZ = INT(FLODIV)
            AW = ZA - REAL(IZ)*1000.0
C
C **SETTING IZ & AW FOR DIFFERENT DECAYS
C
            IF (IRT.EQ.1) IZ = IZ + 1
            IF (IRT.EQ.2) IZ = IZ - 1
            IF (IRT.EQ.4) IZ = IZ - 2
            IF (IRT.EQ.4) AW = AW - 4.0
            IF (IRT.EQ.5) AW = AW - 1.0
            IF (IRT.EQ.7) IZ = IZ - 1
            IF (IRT.EQ.7) AW = AW - 1.0
            IF (IRT.EQ.11) THEN
               IZ = IZ + 1
               AW = AW - 1.0
            END IF
            IF (IRT.EQ.12) THEN
               IZ = IZ - 1
               AW = AW - 4.0
            END IF
C
C **SETTING IZ & AW FOR DAUGHTER
C
            ZAA = AW + 1000.0*REAL(IZ)
            CALL CONV(ZAA,MATA)
            IF (IRS.GT.0) THEN
               CALL CONXA(IRS,MATA,MC)
C
C **THIS SECTION IDENTIFIES ISOMERIC DAUGHTER
C
               A(ICOU) = REAL(MC)
            ELSE
C
C **NEXT SECTION ONLY FOR GROUND STATE OF DAUGHTER
C
               A(ICOU) = REAL(MATA)
            END IF
 130        CONTINUE
            ICOU = ICOU + 1
            A(ICOU) = BR
            GOTO 150
 140        CONTINUE
            WRITE (6,99013) IRT
            WERRNM = 8
            STOP
C
C **THIS SECTION CALCULATES MAX ENERGY OF GENERATED GAMMA SPECTRUM
C
 150        CONTINUE
            IF (JSPEK.NE.0) THEN
               IF (IRT.EQ.3) THEN
                  EMAY = AE(3)*1.0E-6
                  IF (EMAY.GT.EMAX) EMAX = EMAY
                  GOTO 160
               END IF
               IF (IRT.EQ.2) THEN
                  EMAY = 5.
                  IF (EMAY.GT.EMAX) EMAX = EMAY
                  GOTO 160
               END IF
               IF (IRT.EQ.1) THEN
                  EMAY = AE(1)*2.0E-6
                  IF (EMAY.GT.EMAX) EMAX = EMAY
               END IF
            END IF
C ## adding in -ve branching ratios to A(). (Nov89)
            IF (IRT.EQ.4.OR.IRT.EQ.12) THEN
               ICOU = ICOU + 1
               A(ICOU) = A(ICOU-3)
               ICOU = ICOU + 1
               A(ICOU) = 5.0
               ICOU = ICOU + 1
               A(ICOU) = -A(ICOU-3)
            END IF
 160     CONTINUE
C
C *** END OF LOOP ON DECAYS ***
C
C
C **OMIT NEXT SECTION IF NO. OF RADIATION SPECTRA(NSP)=0
C **AND CARRY OUT SPECTAL GENERATION
C
         IF (NSP.EQ.0) GOTO 220
         NSPG = 0
         DO 200 K = 1,NSP
            READ (INA,99003) D7,STYP,LCON,K1,K2,NER,MAT,MF,MT,NUM
            JA = JA + 1
C
C **NER NO. OF DISCRETE ENERGIES,LCON=0:DISCRETE SPECTRUM
C **LCON=1:CONTINUOUS SPECTRUM,LCON=2:DISCRETE&CONTINUOUS
C **D9 IS AN INTEGER(=6) INDICATING NO. OF ITEMS IN FOLLOWING LIST
C
            READ (INA,99014) FD,DFD,ERB,DERB,FC,DFC,MAT,MF,MT,NUM
            JA = JA + 1
C
C *** IF STYP=5 (SP. FISSION) FC=NUBAR=NO. OF ***
C *** PROMPT NEUTRONS EMMITTED PER  FISSION   ***
C
            IST = INT(STYP)
            IF (IST.EQ.5 .AND. LCON.EQ.1) FNUBAR = FC
            IF (IST.EQ.5) THEN
               NEUTC = MSPEC + NSPEC*(MD-1) + 1
               A(NEUTC) = FNUBAR + A(NEUTC)
            END IF
C **OMIT NEXT SECTION IF ONLY CONTINUOUS SPECTRUM IS GIVEN
C
            IF (LCON.NE.1) THEN
               SUMAIN = 0.0
C
C **SUMAIN=SUM OF INTENSITIES OF CURRENT DECAY SPECTRUM
C
               IF (NER.NE.0) THEN
                  DO 190 NN = 1,NER
                     READ (INA,99003) ER,DER,K2,K3,NT,K4,MAT,MF,MT,NUM
C
C **NT NO. OF PARAMETERS FOR ENERGY ER (NT=6 EXCEPT FOR STYP=0.0)
C
                     JA = JA + 1
C
C **STYP=0 GAMMA RADIATION
C **STYP=1 BETA - RADIATION
C **STYP=2 BETA + RADIATION
C **STYP=4 ALPHA RADIATION
C **STYP=5 NEUTRONS
C **STYP=6 SP. FISSION FRAGMENTS
C **STYP=7 PROTONS
C **STYP=8 DISCRETE ELECTRONS
C **STYP=9 X-RAYS
C
                     READ (INA,99011) RTYP,TYP,RI,DRI,RIS,DIS,MAT,MF,MT,
     +                    NUM
                     JA = JA + 1
                     IF (IST.EQ.0 .OR. IST.EQ.9) THEN
C
C **RI=INTENSITY OF DISGRETE ENERGY ER
C
                        SUMAIN = SUMAIN + RI
                        ENI = ER*RI
C
C **IC ENERGY BAND BOUNDARY
C
                        DO 170 IC = 1,NB
                           ID = IC
                           ENAS = ENAC(IC)*1.0E6
                           IF (ER.LE.ENAS) GOTO 180
 170                    CONTINUE
C
C **DECREASE KCOU BY 1 ELSE 1ST GROUP ALWAYS EMPTY. (HARWELL)
C
 180                    CONTINUE
                        KCOU = JCOU + ID - 1
C
C *** MULTIPLICATION FACTORS CHANGED BY 100 IN FOLLOWING TWO LINES.
C *** HARWELL JULY 1987
C
                        A(KCOU) = A(KCOU) + ENI*1.0E-6*FD
                     END IF
                     IF (NT.GT.6) THEN
                        READ (INA,99011) RICC,DIC,RICK,DCK,RICL,DIL,MAT,
     +                       MF,MT,NUM
                        JA = JA + 1
                     END IF
 190              CONTINUE
               END IF
C
C **OMIT IF LCON=0
C
               IF (LCON.EQ.0) THEN
                  IF (IST.EQ.0 .OR. IST.EQ.9) NSPG = NSPG + 1
                  GOTO 200
               END IF
            END IF
C  ## Y2 and Y3 replaced by K1 and K2 in next line as format demands
C  ## integer values. HARWELL 21/11/90.
            READ (INA,99015) RTYP,Y1,K1,K2,NR,NP,MAT,MF,MT,NUM,
     +           (NBT(NN),INT1(NN),NN=1,NR)
            JA = JA + 1
            READ (INA,99016) (XX(NN),YY(NN),NN=1,NP)
            JA = JA + 1
C
C **NR NO. OF INTERPOLATION RANGES FOR CONTINUOUS SPECTRUM
C **NP NO. OF PTS AT WHICH THE CONTINUOUS SPECTRUM  IS GIVEN
C
            IF (IST.EQ.0 .OR. IST.EQ.9) NSPG = NSPG + 1
 200     CONTINUE
         IF (NSPG.NE.0) GOTO 110
         GOTO 220
      END IF
C  ## Changed so that decay file read a second time only
C  for the first 5 gas nuclides. (Nov 89)
 210  CONTINUE
      IREAD = IREAD + 1
      IF (IREAD.EQ.1) GOTO 65
      RETURN

 220  CONTINUE
      DO 230 I1 = 1,NB
         IMAX = I1
         IF (EMAX.LE.ENAC(I1+1)) GOTO 240
 230  CONTINUE
      IF (I1.EQ.NB+1) THEN
         EMAX = 0.0
         GOTO 110
      END IF
 240  CONTINUE
      IF (AE(3).GT.1.E3) THEN
         EFACT = (A9*AE(3)*1.0E-6/EMAX)/(1.-(1.+A9)*EXP(-A9))
         XEXP(1) = 1.0
         DO 250 I2 = 1,IMAX
            XEXP(I2+1) = EXP(-A9*ENAC(I2+1)/EMAX)
            XINT = EFACT*(XEXP(I2)-XEXP(I2+1))
            A(JCOU+I2) = XINT*(ENAC(I2+1)+ENAC(I2))/2.
 250     CONTINUE
      END IF
      EMAX = 0.0
      GOTO 110
C  ## Changes so that files closed and opened in sequence
C  ## 6/9/91
 1000 CONTINUE
      INQUIRE(UNIT=INA,NAME=FILENM)
      CLOSE(UNIT=INA)
      LENGTH=LENGT(FILENM)
      NUMCHR=FILENM(LENGTH:LENGTH)
C ## To increase portability 48 -> ICHAR('0') 13/1/93
      NUMBR=ICHAR(NUMCHR)-ICHAR('0')
      NUMBR=NUMBR+1
      TEMPFN=FILENM(1:LENGTH)
C  ## Change made because there are 10 decay data files for EAF-3
C  ## 18/9/92
      IF (NUMBR.EQ.10) THEN
         FILENM=TEMPFN(1:LENGTH-2)//'10'
      ELSE
C ## To increase portability 48 -> ICHAR('0') 13/1/93
         FILENM=TEMPFN(1:LENGTH-1)//CHAR(ICHAR('0')+NUMBR)
      END IF
      OPEN(INA,FILE=FILENM,STATUS='UNKNOWN')
      GOTO 81
C  ## End of modification
99001 FORMAT (20A4)
99002 FORMAT (I6)
99003 FORMAT (2E11.5,4I11,I4,I2,I3,I5)
99004 FORMAT (' ',' ZA is ',1PE11.5)
99005 FORMAT (16A4,A2,I4,I2,I3,I5)
99006 FORMAT (6I11,I4,I2,I3,I5)
99007 FORMAT (' ',' SEND FAULT AT CARD NUMBER ',I6)
99008 FORMAT (66X,I4,I2,I3,I5)
99009 FORMAT (' ','FEND FAULT AT CARD NUMBER',I5)
99010 FORMAT (2E11.5,4I11,I4,I2,I3,I5/(6E11.5))
99011 FORMAT (6E11.5,I4,I2,I3,I5)
99012 FORMAT (' ',' RTYPE ',I4,' UNKNOWN ORIGIN ')
99013 FORMAT (' ',' RTYPE =',I4)
99014 FORMAT (6E11.5,I4,I2,I3,I5)
99015 FORMAT (2E11.4,4I11,I4,I2,I3,I5/(6I11))
99016 FORMAT (6E11.4)
      END

C----------------------------------------------------------------------
      SUBROUTINE ENDFPR
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Initialises and produces lists of isomers prior to reading decay
C  and cross section libraries. Writes a condensed form of these
C  for subsequent runs.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      INTEGER IX3(70),IX4(10)
      COMMON /ALINK / IX3,IX4

      REAL A(18000)
      COMMON /BLOCK / A

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      DOUBLE PRECISION BB(364)
      COMMON /DBLOCK/ BB

      INTEGER ICOU,IN(364),IW(364),KDIC
      COMMON /ENDLS1/ ICOU,IN,IW,KDIC

      CHARACTER*4 VSIDNT
      CHARACTER*22 SPIDNT
      CHARACTER*15 XSIDNT,DDIDNT
      COMMON /IDNT  / VSIDNT,SPIDNT,XSIDNT,DDIDNT

      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA,
     +     MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      COMMON /LISTAG/ NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,
     +     MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      INTEGER JSPEK
      COMMON /SPKGN1/ JSPEK

      CHARACTER*72 LIBDAT
      COMMON /WALLD2/ LIBDAT

C  Local variables
      CHARACTER*1  NUMCHR
      CHARACTER*3  NUMRGN
      CHARACTER*4  ANOT(20)
      CHARACTER*8  DUMTIM
      CHARACTER*12 TEMP
      CHARACTER*47 CLIDNT
      CHARACTER*50 FILENM,TEMPFN
      CHARACTER*72 OLDLIB

      INTEGER I,IA,ILINES,INA,ISCT3,ISCT4,IX,IY,IZA,IZZ,J,J3,JCS,JCT,
     +     K,KAR,KE,KMAT,KNISOT,KS,KTRANS,LCON,LENGTH,LFI,LISO,LRP,
     +     LSTART,MAST,MAT,MATC,MF,MIDEP,MT,N1,N2,NDK,NER,NFOR,NG,NLIB,
     +     NLIN,NMAT,NMOD,NN,NP,NP2,NR,NR2,NSP,NSUB,NT,NUM,NUMBEG,NUMBR,
     +     NVER,NWD,NXC
      INTEGER NUCL(364),LX(200)

      REAL Y(200)
      REAL AWI,AWR,BARNS,DUMMY,RTYP,ZA

C  External functions
      INTEGER LENGT
      EXTERNAL LENGT

C  External routines
      EXTERNAL CLOKK,CNVNT,ENDFP

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      NSTART = 0
      LSTART = 0
      NCH = 1
      NSPEC = 29
      NISOT = 0
      N = 364
      NTRANS = 268
      NCROSS = 700
      NMAT = 0
      KMAT = 0
      INA = 16

      OPEN (16,FILE='PROCDEC1.001',STATUS='UNKNOWN')
      OPEN (18,FILE='INDEXSIM.DAT',STATUS='OLD')
      IF (WTYPE.EQ.3) THEN
         CALL CNVNT(WREGN,NUMRGN)
         TEMP = 'WCOLL'//NUMRGN//'.DAT'
         OPEN(12,FILE=TEMP,STATUS='OLD')
         OPEN (13,FILE='WARRY000.DAT',STATUS='OLD',ACCESS='SEQUENTIAL',
     +        FORM='UNFORMATTED')
         KNISOT = 0
         READ(13) OLDLIB
         READ (13)
     +        NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA,
     +        MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS
         READ(13) (IX3(K),K=1,70)
         READ(13) (IX4(K),K=1,10)
         READ(13) (FLUX(K),K=1,3),MIND
         READ(13) (BB(K),K=1,N)
         READ(13) ICOU,(IN(K),K=1,N)
         READ(13) (IW(K),K=1,N),KDIC
         KS = 1
         DO 2 KAR=1,9
            KE = KS + 1999
            READ (13) (A(K),K=KS,KE)
            KS = KE + 1
 2       CONTINUE
         CLOSE (UNIT=13)
C  ## HARWELL MODIFICATION DECEMBER 1990.
         READ (12,99020) NCROSS,CLIDNT
         BACKSPACE(12)
         XSIDNT = CLIDNT(11:25)
         SPIDNT = CLIDNT(26:47)
         DDIDNT = OLDLIB(58:72)
 
C
C  RESET NCROSS AND ERASE OLD CROSS SECTION DATA
C
         NSTART = MCROSS
         A(NSTART) = REAL(NCROSS)
         NSTART = NSTART + 1
         DO 3 I = NSTART,18000
            A(I) = 0.0
 3       CONTINUE
         GOTO 132
C
C  ## END OF MODIFICATION FOR OPTION 'ARRA'
C
      ELSE IF (WTYPE.EQ.1.OR.WTYPE.GE.4) THEN
         WERRNM = 5
         STOP
      ELSE
         CONTINUE
      END IF
C  ## HARWELL MODIFICATION NOV 1990.
C  ## READ IN LIST OF NUCLIDE ZA IDENTIFIERS
C
      I = 1
 5    CONTINUE
      READ (18,99018,END=7) NUCL(I)
      I = I + 1
      GOTO 5

 7    CONTINUE
      ISCT3 = 0
      ISCT4 = 0
      KTRANS = 0
      MATC = 0
C  ## Changes so that decay data is read in from several files
C     It is assumed that the files have the extension .00n
C     6/9/91
C
      READ (INA,99002) ILINES,DDIDNT
      DO 10 NLIN = 1,ILINES
         READ (INA,99001) (ANOT(J),J=1,17)
 10   CONTINUE
 20   CONTINUE
      READ (INA,99003,END=1000)
     +     ZA,AWR,LRP,LFI,NLIB,NMOD,MAT,MF,MT,NUM
      IF (MAT.EQ.-1) THEN
         N = MATC
C  ## Changes so that 5 gas nuclides repeated. (Nov 89)
         NTRANS = KTRANS + 1
         DO 25 I = 1,5
            IN(N+I) = IN(I)
            IW(N+I) = IW(I)
 25      CONTINUE
         N = N + 5
      ELSE
         MATC = MATC + 1
         IZA = INT(ZA + 0.1)
         IZZ = IZA/1000
         IN(MATC) = IZZ
         IW(MATC) = IZA - 1000*IZZ
         READ (INA,99006) LISO,NFOR
C  ## HARWELL MODIFICATION NOV 1990.
C  ## TEST THAT INDEX IS CONSISTENT WITH DECAY DATA
         IF (NUCL(MATC).NE.(10*INT(ZA)+LISO)) THEN
            WRITE(6,99019) MATC,NUCL(MATC),INT(ZA),LISO
            WERRNM = 6
            STOP
         END IF
C
C **TEST FOR ENDFB6 FORMAT DATA
C
         IF (NFOR.EQ.6) THEN
            READ (INA,99003) AWI,AWI,N1,N1,NSUB,NVER,MAT,MF,MT,NUM
            IF (NSUB.NE.4) THEN
               WRITE (6,99007) ZA
               STOP
            END IF
         END IF

         READ (INA,99008) NWD,NXC
         DO 30 JCT = 1,NWD
            READ (INA,99009)
 30      CONTINUE
         DO 40 JCS = 1,NXC
            READ (INA,99009)
 40      CONTINUE
C
C *** FOR NXC=1 AS IN STABLE NUCLIDES, SKIP SPECTRAL DATA
C
         IF (NXC.NE.1) THEN
            LISO = LISO + 1
            IF (LISO.EQ.1) THEN
            ELSE IF (LISO.EQ.3) THEN
               ISCT4 = ISCT4 + 1
               IX4(ISCT4) = MATC
            ELSE
               ISCT3 = ISCT3 + 1
               IX3(ISCT3) = MATC
            END IF
            READ (INA,99009)
            READ (INA,99009)
            READ (INA,99010) NSP
            READ (INA,99009)
            READ (INA,99009)
            READ (INA,99010) NDK
            KTRANS = KTRANS + NDK
            DO 50 J3 = 1,NDK
C ## Read RTYP so that KTRANS is increased for alpha decays.
C NB FORMAT 99011 also changed. (Nov 89)
               READ (INA,99011) RTYP
               IF (ABS(RTYP-4.0).LT.0.001) KTRANS=KTRANS+1
               IF (ABS(RTYP-1.4).LT.0.001) KTRANS=KTRANS+1
 50         CONTINUE
            IF (NSP.NE.0) THEN
               DO 70 K = 1,NSP
                  READ (INA,99012) LCON,NER
                  READ (INA,99009)
                  IF (LCON.NE.1) THEN
                     IF (NER.NE.0) THEN
                        DO 60 N2 = 1,NER
                           READ (INA,99013) NT
                           READ (INA,99009)
                           IF (NT.GT.6) READ (INA,99009)
 60                     CONTINUE
                     END IF
                     IF (LCON.EQ.0) GOTO 70
                  END IF
                  READ (INA,99014) NR,NP
                  NR2 = NR*2
                  NP2 = NP*2
                  READ (INA,99015) (LX(NN),NN=1,NR2)
                  READ (INA,99016) (Y(NN),NN=1,NP2)
 70            CONTINUE
            END IF
         END IF
         READ (INA,99009)
         READ (INA,99009)
         READ (INA,99017) MAT
         GOTO 20
      END IF
C  ## HARWELL MODIFICATION DECEMBER 1990.
      CLIDNT = '          PROCESS XSECT1 Dummy region 000      '
      XSIDNT = CLIDNT(11:25)
      SPIDNT = CLIDNT(26:47)
      NISOT = KMAT
C
C *** ARRAY A IDENTIFIERS ***
C
      NSTART = NSTART + 1
      A(NSTART) = REAL(NCH)
      NSTART = NSTART + 1
      A(NSTART) = 1.0
      NSTART = NSTART + 1
      A(NSTART) = REAL(NSPEC)
      NSTART = NSTART + NSPEC*18
      NSTART = NSTART + 1
      A(NSTART) = REAL(NISOT)
      MTOTAL = NSTART
      NSTART = NSTART + NSPEC
      MFRAC = NSTART
      NSTART = NSTART + NISOT
      NSTART = NSTART + 1
      A(NSTART) = REAL(N)
      MASS = NSTART
      NSTART = NSTART + N
      MIDEN = NSTART
      NSTART = NSTART + N
      MLAMDA = NSTART
      NSTART = NSTART + N
      MSPEC = NSTART
      NSTART = NSTART + N*NSPEC
      MYILDS = NSTART
      NSTART = NSTART + NYIELD*NISOT
      MYIELD = NSTART
      A(MYIELD+1) = 1.0
      NSTART = NSTART + N + 1
      A(NSTART) = REAL(NTRANS)
      MTRANS = NSTART
      NSTART = NSTART + NTRANS*3
      NSTART = NSTART + 1
      MCROSS = NSTART
      A(NSTART) = REAL(NCROSS)
      NSTART = NSTART + 1
      DO 90 I = 1,N
         LSTART = LSTART + 1
         BB(LSTART) = 0.0D0
 90   CONTINUE
      IA = 0
      MAST = MASS + 1
      DO 120 I = MAST,MIDEN
         IA = IA + 1
         A(I) = REAL(IW(IA))
         DO 100 IX = 1,ISCT3
            IY = IX3(IX)
            IF (IA.EQ.IY) A(I) = A(I) + 0.5
 100     CONTINUE
         DO 110 IX = 1,ISCT4
            IY = IX4(IX)
            IF (IA.EQ.IY) A(I) = A(I) + 0.5
 110     CONTINUE
 120  CONTINUE
      IA = 0
      MIDEP = MIDEN + 1
      DO 130 I = MIDEP,MLAMDA
         IA = IA + 1
         A(I) = REAL(IN(IA))
 130  CONTINUE
      KDIC = 1
      ICOU = MTRANS
C
C CALL OTHER ROUTINES
C
      CALL ENDFP(INA)
C
C READ IN COLLAPSED DATA. (NCROSS REACTIONS AND NGROUP X-SECTIONS).
C
 132  CONTINUE
      IF (WTYPE.EQ.2) GOTO 154
C  ## THIS METHOD OF READING COLLAPX WORKS ONLY IF NGROUP = 1.
C  ## HARWELL NOV 1990.
      READ (12,99020) NCROSS,CLIDNT
      DO 150 K = 1,NCROSS
         READ (12,99021) NG,N2,BARNS
         A(NSTART) = REAL(NG)
         NSTART = NSTART + 1
         A(NSTART) = REAL(N2)
         NSTART = NSTART + 1
         A(NSTART) = BARNS*1.E-24
         NSTART = NSTART + 1
 150  CONTINUE
      READ (12,99022,END=152) NUMBEG
 152  CONTINUE
 154  CONTINUE
C     
C WRITE "LISTAG", OTHER COMMONS AND "A" ARRAY TO DISK. (UNFORMATTED).
C
C  ## HARWELL MODIFICATION DECEMBER 1990.
C  #  GET CURRENT DATE
      CALL CLOKK(DUMMY,DUMTIM,TEMP)
      LIBDAT = CLIDNT//TEMP(1:2)//TEMP(4:5)//TEMP(7:8)//VSIDNT//DDIDNT
C  ## HARWELL MODIFICATION JANUARY 1991.
      FLUX(2) = 1.0
      KNISOT = 0
      CALL CNVNT(WREGN,NUMRGN)
      TEMP = 'WARRY'//NUMRGN//'.DAT'
      OPEN (13,FILE=TEMP,STATUS='UNKNOWN',ACCESS='SEQUENTIAL',
     +     FORM='UNFORMATTED')
      WRITE (13) LIBDAT
      WRITE (13) NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,
     +     MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS
      WRITE (13) (IX3(K),K=1,70)
      WRITE (13) (IX4(K),K=1,10)
      WRITE (13) (FLUX(K),K=1,3),MIND
      WRITE (13) (BB(K),K=1,N)
      WRITE (13) ICOU,(IN(K),K=1,N)
      WRITE (13) (IW(K),K=1,N),KDIC
      KS = 1
      DO 160 KAR = 1,9
         KE = KS + 1999
         WRITE (13) (A(K),K=KS,KE)
         KS = KE + 1
 160  CONTINUE
C  ##
C     CALL PONDEC
      CLOSE(UNIT=13)
      CLOSE(UNIT=12)
      RETURN
C  ## Changes so that files closed and opened in sequence
C  ## 6/9/91
 1000 CONTINUE
      INQUIRE(UNIT=INA,NAME=FILENM)
      CLOSE(UNIT=INA)
      LENGTH = LENGT(FILENM)
      NUMCHR = FILENM(LENGTH:LENGTH)
C ## To increase portability 48 -> ICHAR('0') 13/1/93
      NUMBR = ICHAR(NUMCHR)-ICHAR('0')
      NUMBR = NUMBR+1
      TEMPFN = FILENM(1:LENGTH)
C  ## Changes made because there are 10 decay files for EAF-3.
C  ## 18/9/92
      IF (NUMBR.EQ.10) THEN
         FILENM = TEMPFN(1:LENGTH-2)//'10'
      ELSE
C ## To increase portability 48 -> ICHAR('0') 13/1/93
         FILENM = TEMPFN(1:LENGTH-1)//CHAR(ICHAR('0')+NUMBR)
      END IF
      OPEN(INA,FILE=FILENM,STATUS='UNKNOWN')
      GOTO 20
C  ## End of modification
99001 FORMAT (20A4)
99002 FORMAT (I6,1X,A15)
99003 FORMAT (2E11.5,4I11,I4,I2,I3,I5)
99006 FORMAT (33X,I11,11X,I11)
99007 FORMAT (//41X,'SUB-LIBRARY FLAGGED AS OTHER THAN DECAY DATA',
     +     ' - PROGRAM HALTED - ZA is ',1PE11.5/37X,'<<',85('='),
     +     '>>')
99008 FORMAT (44X,2I11)
99009 FORMAT (1X)
99010 FORMAT (55X,I11)
99011 FORMAT (E11.5)
99012 FORMAT (22X,I11,22X,I11)
99013 FORMAT (44X,I11)
99014 FORMAT (44X,2I11)
99015 FORMAT (6I11)
99016 FORMAT (6E11.5)
99017 FORMAT (66X,I4)
99018 FORMAT (16X,I6)
99019 FORMAT (' NUCL(',I4,') = ',I6,' ZA = ',I5,' ISOMER = ',I2)
99020 FORMAT (1X,I5,2X,A47)
99021 FORMAT (1X,I4,2X,I4,4X,1PE12.5)
99022 FORMAT (1X,I3)
      END

C----------------------------------------------------------------------
      SUBROUTINE IDNTFY(NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,
     +     MIDEN,MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,
     +     MCROSS,NYIELD)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Sets up the pointers for the A() array.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  See comments for LISTAG common in routine FISPRO
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER MASS,MCROSS,MFRAC,MIDEN,MLAMDA,MSPEC,MTOTAL,MTRANS,
     +     MYIELD,MYILDS,N,NCH,NCROSS,NISOT,NSPEC,NSTART,NTRANS,NYIELD

C  Global variables
      REAL A(18000)
      COMMON/BLOCK /A

C  Local variables
      INTEGER NGROUP

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      NYIELD = 1190
      NSTART = NSTART+1
      NCH    = INT(A(NSTART))
      NSTART = NSTART+1
      NGROUP = INT(A(NSTART))
      NSTART = NSTART+1
      NSPEC  = INT(A(NSTART))
      NSTART = NSTART+18*NSPEC
      NSTART = NSTART+1
      NISOT  = INT(A(NSTART))
      MTOTAL = NSTART
      NSTART = NSTART+NSPEC
      MFRAC  = NSTART
      NSTART = NSTART+NISOT
      NSTART = NSTART+1
      N      = INT(A(NSTART))
      MASS   = NSTART
      NSTART = NSTART+N
      MIDEN  = NSTART
      NSTART = NSTART+N
      MLAMDA = NSTART
      NSTART = NSTART+N
      MSPEC  = NSTART
      NSTART = NSTART+N*NSPEC
      MYILDS = NSTART
      NSTART = NSTART+NYIELD*NISOT
      MYIELD = NSTART
      NSTART = NSTART+N
      NSTART = NSTART+1
      NTRANS = INT(A(NSTART))
      MTRANS = NSTART
      NSTART = NSTART+3*NTRANS
      NSTART = NSTART+1
      NCROSS = INT(A(NSTART))
      MCROSS = NSTART
      NSTART = NSTART+(NGROUP+2)*NCROSS

      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE INITVR
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Initialisation routine.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      INTEGER IX3(70),IX4(10)
      COMMON /ALINK / IX3,IX4

      REAL A(18000)
      COMMON/BLOCK /A

      INTEGER NSTEPS
      REAL GMS,YSTEPS,ZLEVEL
      COMMON /CALIST/ GMS,NSTEPS,YSTEPS,ZLEVEL

      INTEGER MAXXT
      REAL CONVV
      COMMON /CONVRG/ MAXXT,CONVV

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      REAL XA(19),XMU(19)
      COMMON /DOSOUT/ XA,XMU

      CHARACTER*4 VSIDNT
      CHARACTER*22 SPIDNT
      CHARACTER*15 XSIDNT,DDIDNT
      COMMON /IDNT  / VSIDNT,SPIDNT,XSIDNT,DDIDNT

      INTEGER IATMWT(364),ISEX2
      REAL FUWT(364)
      COMMON /INFO1 / IATMWT,FUWT,ISEX2

      CHARACTER*4 IDS(364)
      COMMON /INFO2 / IDS

      INTEGER NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,MLAMDA,
     +     MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD
      COMMON /LISTAG/ NSTART,NCH,NSPEC,NISOT,MTOTAL,MFRAC,N,MASS,MIDEN,
     +     MLAMDA,MSPEC,MYILDS,MYIELD,NTRANS,MTRANS,NCROSS,MCROSS,NYIELD

      REAL FLUX2,T
      COMMON /OUT1  / FLUX2,T

      CHARACTER*4 NAMREP(83)
      COMMON /OUT2  / NAMREP

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WLVL2,WJSPEK,WMAXT
      REAL WLVL1,WCONV
      COMMON /PRPRM0/ WLVL1,WLVL2,WJSPEK,WCONV,WMAXT

      DOUBLE PRECISION WMIND
      COMMON /PRPRM1/ WMIND

      CHARACTER*22 WSPID
      COMMON /PROSTR/ WSPID

      INTEGER JSPEK
      COMMON /SPKGN1/ JSPEK


C  Local variables

C  External functions

C  External routines

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      INTEGER I,J
C
C INITIALISATION
C
      DO 1 I = 1,10
         IX4(I) = 0
 1    CONTINUE
      DO 2 I = 1,70
         IX3(I) = 0
 2    CONTINUE
C ## VERSION NUMBER OF FISPRO 2.0/10 = 2010
      VSIDNT = '1001'
      NCH = 0
      N = 0
      DO 10 J = 1,3
         FLUX(J) = 0.0
 10   CONTINUE
      DO 20 J = 1,364
         B(J) = 0.0D0
 20   CONTINUE
      DO 30 J = 1,18000
         A(J) = 0.0
 30   CONTINUE
      FLUX(1) = 1.0
      MCROSS = 0
      NCROSS = 0
      NSTART = 0
      NSPEC  = 0
      NISOT  = 0
      MTOTAL = 0
      MFRAC  = 0
      MASS   = 0
      MIDEN  = 0
      MLAMDA = 0
      MSPEC  = 0
      MYIELD = 0
      MYILDS = 0
      NTRANS = 0
      MTRANS = 0
      NYIELD = 1190
      ZLEVEL = 0.0
C Set parameters from PRPRM0 and PRPRM1 commons
      MIND   = WMIND
      ZLEVEL = WLVL1
      NSTEPS = WLVL2
      YSTEPS = REAL(NSTEPS)
      JSPEK  = WJSPEK
      T      = WTIME
      FLUX2  = WFLUX
      MAXXT  = WMAXT
      CONVV  = WCONV
      SPIDNT = WSPID

      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE INTEGS(M, FLUXT, YTOTAL, TIME)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Integration routine, used if INTEGT unsuccessful
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  M       Number of nuclides not in equilibrium
C  FLUXT   Total flux (n/cm2/s)
C  YTOTAL
C  TIME
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER M
      REAL FLUXT,YTOTAL,TIME

C  Global variables
      INTEGER MAXXT
      REAL CONVV
      COMMON /CONVRG/ MAXXT,CONVV

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      INTEGER I22,I31,I32
      REAL BR1(1000)
      COMMON /EXT   / I22,I31,I32,BR1

      INTEGER NP1(1000),ND1(1000),I2,I1,N
      REAL TR1(1000),YLAM(364)
      COMMON /TRANS0/ NP1,ND1,TR1,I2,I1,N,YLAM

      DOUBLE PRECISION Y(364)
      COMMON /TRANS1/ Y

C  Local variables
      DOUBLE PRECISION A1,B1,D1,F1,R,O,Q,S(364),F,ERRCOL
      DOUBLE PRECISION C1(364),E1(364),EXTRAP(364,10),G1(364),SX(364),
     +     XINTER(364),YINTER(364),ZINTER(364)

      INTEGER I,I23,I4,IA,IB,IC,ICRI,ID,IE,IND,INUM,IQ,IQP,IQQ,IR,J,J1,
     +     JUMP,JYY,K,K1,L,M1,MAXEXT,ND,NDE,NDQ,NP,NPQ,NPX,NQX,NSTEPS

      REAL STEPLE,EPSB,TMAX,H,BSY,BRX,BRY,BSX,D2,B,FLODIV,ERS,CRIT

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C+**PJK 26/11/93
      IA = 0
      STEPLE = 0.0
      NSTEPS = 0
      DO 2 J = 1,10
         DO 1 I = 1,364
            EXTRAP(I,J) = 0.0D0
 1       CONTINUE
 2    CONTINUE

      DO 5 I = 1,N
         S(I) = 0.0D0
 5    CONTINUE
      EPSB = CONVV
      MAXEXT = MAXXT
      TMAX = TIME
      H = TIME
      NDE = M
      M1 = M + 1
      I4 = I2 + 1
      JYY = 0
      INUM = 0
      O = 0.0D0
      ERRCOL = 0.0D0
C
C CORRECTION FOR EQUILIBRIUM ISOTOPES LOSS
C
      IF (M.EQ.N) GOTO 280
      I23 = I22 + 1
      NPX = 0
      BRX = 0.0
      IF (I23.GE.I32) GOTO 30
      DO 20 IQ=I23,I32
         NP = NP1(IQ)
         IF (NP.EQ.NPX) BRX = BRX + BR1(IQ)
         ND = ND1(IQ)
         IF (ND.EQ.0 .OR. NP.EQ.0) GOTO 20
         Y(ND) = Y(ND) + Y(NP)*DBLE(BR1(IQ))
         BRY = 1.0 - BRX
         IF (BRY.LT.1.0E-6) Y(NP) = 0.0D0
         IF (IQ.EQ.I23) GOTO 20
         IQQ = IQ - 1
         NQX = 0
         BSX = 0.0
         DO 10 IQP=I23,IQQ
            NPQ = NP1(IQP)
            IF (NPQ.EQ.NQX) BSX = BSX + BR1(IQP)
            NDQ = ND1(IQP)
            IF (NPQ.NE.ND) GOTO 10
            BSY = 1.0 - BSX
            IF (BSY.LT.1.0E-6) Y(ND) = 0.0D0
            Y(NDQ) = Y(NDQ) + Y(NP)*DBLE(BR1(IQP))
 10      CONTINUE
 20   CONTINUE
 30   CONTINUE
      NPX = 0
      BRX = 0.0
      IF (I31.EQ.0) GOTO 60
      DO 50 IQ=1,I31
         IF (FLUXT.GT.0.0 .AND. TIME.GT.0.0) GOTO 50
         NP = NP1(IQ)
         IF (NP.EQ.NPX) BRX = BRX + BR1(IQ)
         DO 40 IR=1,M
            IF (NP.EQ.IR) GOTO 50
 40      CONTINUE
         ND = ND1(IQ)
         Y(ND) = Y(ND) + Y(NP)*DBLE(BR1(IQ))
         BRY = 1.0 - BRX
         IF (BRY.LT.1.0E-6) Y(NP) = 0.0D0
 50   CONTINUE
 60   CONTINUE
C
C SET INITIAL VALUES INTO XINTER.
C
      GOTO 280
C
C MODULE ERRCOL
C COMPUTES FRACTIONAL ERROR IN IATH COLUMN OF EXTRAPOLATION ARRAY.
C A1 IS SET TO MAXIMUM ERROR OBTAINED FROM DIFFERENCE OF ADJACENT
C COLUMNS OF EXTRAPOLATION ARRAY.
C
 70   CONTINUE
      A1 = 0.0D0
      J1 = IA - 1
      DO 90 K1=1,NDE
         B1 = EXTRAP(K1,IA)
         IF (ABS(B1).GT.1.0D-12) GOTO 80
         A1 = MAX(A1,ABS(B1-EXTRAP(K1,J1))*1.0D12)
         GOTO 90
 80      CONTINUE
         A1 = MAX(A1,ABS(1.0D0-EXTRAP(K1,J1)/B1))
 90   CONTINUE
C
C FRACTIONAL ERROR = ERROR FOUND / MAXIMUM ERROR.
C
      ERRCOL = A1/DBLE(EPSB)
      GOTO JUMP, (350, 500)
C
C MODULE PREDL
C
C A1 = CONVERGENCE CRITERION FOR SERIES FOR EXPONENTIAL FACTOR.
C
 100  CONTINUE
      A1 = 1.0D-06*DBLE(STEPLE)
      IF (M.EQ.N) GOTO 120
      DO 110 I=M1,N
         B1 = DBLE(YLAM(I))
         C1(I) = 1.0D0/B1
 110  CONTINUE
 120  CONTINUE
      DO 150 I=1,NDE
C
C SET INTERVAL VALUES INTO XINTER.
C
         XINTER(I) = Y(I)
C
C C(I) IS DIAGONAL ELEMENT.
C
         B1 = DBLE(-YLAM(I))
C
C D=LAM*H FOR ONE-STEP METHOD.
C
         D1 = B1*DBLE(STEPLE)
         D2 = REAL(ABS(D1))
         IF (D2.LT.150.0) G1(I) = EXP(D1)
         IF (D2.GE.150.0) G1(I) = 0.0D0
C
C IF LAM*H IS SMALL, THEN COMPUTE EXPONENTIAL FACTORS BY SERIES
C SERIES E1=SUM(LAM(I-1)*H(I)).
C AT 322, OTHERWISE E1=EXP(LAM*H).
C
         IF (ABS(D1).LT.0.1D0) GOTO 130
         IF (D2.LT.150.0) E1(I) = (EXP(D1)-1.0D0)/B1
         IF (D2.GE.150.0) E1(I) = (-1.0D0)/B1
         GOTO 150
 130     CONTINUE
         B1 = DBLE(STEPLE)
         F1 = B1
C
C J1 COUNTS TERMS, F1 IS LATEST TERM, B1 IS SUM OF TERMS.
C
         J1 = 1
 140     CONTINUE
         J1 = J1 + 1
         F1 = D1*F1/DBLE(J1)
         B1 = B1 + F1
         IF (A1.LT.ABS(F1)) GOTO 140
C
C SET RESULT INTO E1, FINISHING WHEN MOD.F1 IS LESS THAN A1.
C
         E1(I) = B1
 150  CONTINUE
C
C I COUNTS STEPS OF ONE-STEP METHOD.
C
      I = 0
 160  CONTINUE
      I = I + 1
C
C EVALUATE R.H.S. OF DN/DT=AN=DX.
C NDE IS NUMBER OF BURNABLE NUCLIDES IN PROBLEM (LESS THAN 101)
C NJ1,NJ2 ARE TABLES OF NUCLIDES PRODUCED BY CAPTURE AND DECAY.
C X IS THE NUMBER DENSITY TABLE.
C NNF IS THE NUMBER OF FISSILE NUCLIDES.
C NNFP IS THE NUMBER OF FISSION PRODUCTS IN PROBLEM.
C
      IF (M.EQ.N) GOTO 220
      DO 170 IQ=M1,N
         XINTER(IQ) = S(IQ)*C1(IQ)
 170  CONTINUE
      IF (I.GT.1) GOTO 190
      DO 180 IQ=1,N
         YINTER(IQ) = XINTER(IQ)
 180  CONTINUE
 190  CONTINUE
      IF (I2.EQ.I1) GOTO 220
      DO 200 IQ=I4,I1
         NP = NP1(IQ)
         ND = ND1(IQ)
         IF (YINTER(NP).LT.1.D-50) YINTER(NP)=0.0D0
         XINTER(ND) = XINTER(ND) + DBLE(TR1(IQ))*YINTER(NP)*C1(ND)
 200  CONTINUE
      DO 210 IQ=M1,N
         YINTER(IQ) = XINTER(IQ)
 210  CONTINUE
 220  CONTINUE
      DO 230 IQ=1,NDE
         SX(IQ) = S(IQ)
 230  CONTINUE
      DO 240 IQ=1,I2
         NP = NP1(IQ)
         ND = ND1(IQ)
         IF (XINTER(NP).LT.1.D-50) XINTER(NP)=0.0D0
         SX(ND) = SX(ND) + DBLE(TR1(IQ))*XINTER(NP)
 240  CONTINUE
      JYY = JYY + 1
      DO 250 K1=1,NDE
         IF (XINTER(K1).LT.1.D-50) XINTER(K1)=0.0D0
         XINTER(K1) = XINTER(K1)*G1(K1) + SX(K1)*E1(K1)
 250  CONTINUE
      DO 260 IQ=1,N
         YINTER(IQ) = XINTER(IQ)
 260  CONTINUE
C
C ONE-STEP METHOD:  X=X+(EXP(LAM*H)-1.0)/LAM*DX/DT.
C TAKEN OVER 'NSTEPS' STEPS OF LENGTH 'STEPLE'.
C
      IF (I.NE.NSTEPS) GOTO 160
      GOTO JUMP, (310, 380)
 270  FORMAT (' --STEP LENGTH HAS BECOME NEGLIGIBLY SMALL--')
C
C IND - STEP REDUCTION FACTOR.
C
 280  CONTINUE
      IND = 1
C
C B - CONFIDENCE FACTOR FOR STEP CHANGING,
C IF B=1.0 FULL CONFIDENCE,  IF B=0 NO CONFIDENCE.
C
      B = 1.0
C
C K IS NUMBER OF STEPS TO COVER INTERVAL TMAX.
C
      K = INT(MAX(1.1,TMAX/H+0.1))
C
C H - STEP LENGTH OF EXTRAPOLATION PROCEDURE.
C
      H = TMAX/REAL(K)
C
C L - COLUMN ON WHICH TO TEST CONVERGENCE, IF L=0 NO TEST.
C
      L = 0
C
C DECREASE H BY STEP-LENGTH REDUCTION FACTOR (IND).
C
 290  CONTINUE
      H = H/REAL(IND)
C
C INCREASE NUMBER OF STEPS (K) CORRESPONDINGLY.
C
      K = IND*K
C
C RESET REDUCTION FACTOR TO UNITY.
C
      IND = 1
C
C TEST IF H IS TOO SMALL, I.E. SOMETHING GONE WRONG.
C
      IF (H.GT.TMAX*0.1E-4) GOTO 300
      WRITE (6,270)
      STOP 1
C
C STEPLE - ONE-STEP METHOD STEP LENGTH
C STEPLE SET FIRSTLY TO EXTRAPOLATION STEP (H)
C
 300  CONTINUE
      STEPLE = H
C
C NSTEPS - NUMBER OF STEPS OF LENGTH STEPLE TO COVER EXTRAPOLATION
C STEP (H).
C
      NSTEPS = 1
C
C INTEGRATE EQUATIONS BY 1 STEP OF ONE-STEP METHOD.
C
      ASSIGN 310 TO JUMP
      GOTO 100
 310  CONTINUE
      DO 320 IQ=1,NDE
C
C STORE INITIAL  RESULTS  INTO  EXTRAPOLATION ARRAY
C
         EXTRAP(IQ,1) = XINTER(IQ)
 320  CONTINUE
C
C IA  COUNTS  COLUMNS  OF  EXTRAPOLATION  ARRAY
C
      IA = 1
C
C IF IA=MAXEXT WE HAVE USED STORAGE FOR EXTRAPOLATION    U
C WITHOUT CONVERGENCE.
C
 330  CONTINUE
      IF (MAXEXT.GT.IA) GOTO 370
C
C IB - NUMBER OF DERIVATIVE EVALUATIONS TO OBTAIN CONVERGENCE  IN
C 1 EXTRAPOLATION.
C
      IB = 2
C
C F - WILL CONTAIN BEST ESTIMATE OF FUNCTION EVALUATIONS PER UNIT
C STEP USING VARIOUS ORDERS OF EXTRAPOLATION.
C
      F = 1.0D+37
C
C IA COUNTS THROUGH COLUMNS OF EXTRAPOLATION ARRAY.
C NSTEPS IS NUMBER OF STEPS REQUIRED TO COVER INTERVAL USING IATH
C ORDER METHOD.
C
      IA = 1
 340  CONTINUE
      IA = IA + 1
      NSTEPS = NSTEPS/2
      ASSIGN 350 TO JUMP
      GOTO 70
C
C COMPUTE FRACTIONAL ERROR IN IATH COLUMN.
C
 350  CONTINUE
      O = ERRCOL
C
C IF THIS COLUMN HAS CONVERGED ALREADY GOTO 45.
C
      IF (O.LE.1.0D+00) GOTO 510
C
C INCREASE NUMBER OF DERIVATIVE EVALUATIONS AS ORDER INCREASES.
C
      IB = IB + IB
C
C STEPLE IS ESTIMATED STEP LENGTH TO PRODUCE CONVERGENCE IN THIS
C COLUMN.
C
      STEPLE = H*REAL(O)**(1.0/REAL(1-IA))/REAL(NSTEPS)
C
C Q IS FUNCTION EVALUATIONS PER UNIT STEP.
C
      Q = DBLE(IB)/DBLE(STEPLE)
      IF (Q.GT.F) GOTO 360
C
C STORE BEST FUNCTION EVALUATIONS PER UNIT STEP.
C
      F = Q
C
C SET L TO COLUMN FOR LATER TEST.
C
      L = IA
C
C R IS STEP LENGTH REQUIRED FOR ORDER.
C
      R = DBLE(STEPLE)
 360  CONTINUE
      IF (IA.NE.MAXEXT) GOTO 340
C
C SET MAXEXT TO BEST ORDER EXTRAPOLATION.
C
      MAXEXT = L
C
C SET STEP REDUCTION FACTOR TO REDUCE STEP LENGTH,
C MODIFIED BY CONFIDENCE FACTOR B.
C RESUME AT 16.
C
      FLODIV = H/(B*REAL(R)) + 1.0
      IND = INT(FLODIV)
      GOTO 290
C
C DOUBLE NUMBER OF STEPS OF ONE-STEP METHOD.
C
 370  CONTINUE
      NSTEPS = NSTEPS + NSTEPS
C
C DECREASE STEP LENGTH.
C
      STEPLE = 0.5*STEPLE
      ASSIGN 380 TO JUMP
C
C INTEGRATE EQUATIONS WITH 'NSTEPS' STEPS OF SIZE 'STEPLE'.
C
      GOTO 100
C
C IB - CONVERGENCE TEST INDICATOR.
C
 380  CONTINUE
      IB = 1
C
C IC - NEXT COLUMN OF EXTRAPOLATION.
C
      IC = IA + 1
      IF (MAXXT.LT.6) GOTO 385
      IF (IC.LE.5) GOTO 430
C
C O - MAXIMUM ERROR.
C
 385  CONTINUE
      O = 0.0D+00
      DO 420 ID=1,NDE
C
C F - EXTRAPOLATION FACTOR.
C
         F = 1.0D0
C
C Q - LATEST ONE-STEP ESTIMATE.
C
         Q = XINTER(ID)
         DO 390 IE=1,IA
C
C F DOUBLES TO BECOME 2.0 TO THE POWER (R+1.0).
C
            F = F + F
C
C R - OLD VALUE IN THIS COLUMN OF EXTRAPOLATION OVERWRITTEN BY Q.
C
            R = EXTRAP(ID,IE)
            EXTRAP(ID,IE) = Q
C
C Q UPDATED TO NEXT COLUMN ESTIMATE,
C Q = (2.0**(R+1.0)*(NEW-OLD))/(2.0**(R+1)-1.0).
C
            Q = (F*Q-R)/(F-1.0D0)
 390     CONTINUE
         EXTRAP(ID,IC) = Q
C
C IF CONVERGENCE TESTS ALREADY FAILED TEST NO FURTHER,
C ELSE O IS SET TO MAXIMUM ERROR FOUND AND TESTED AGAINST MAX. ERROR
C IF (IB.LT.0)GOTO 310
C
         B1 = EXTRAP(ID,IA)
         IF (ABS(B1).GT.1.0D-12) GOTO 400
         O = MAX(O,ABS(B1-R)*1.0D+4)

C+**PJK 29/11/93 Arithmetic-IF replaced
C         IF (DBLE(EPSB)-O) 410, 420, 420
         IF (DBLE(EPSB).LT.O) THEN
            GOTO 410
         ELSE
            GOTO 420
         END IF

 400     CONTINUE
         ERS = ABS(1.0E+00-REAL(R/B1))
         IF (DBLE(ERS).LT.O) GOTO 420
         O = DBLE(ERS)

C+**PJK 29/11/93 Arithmetic-IF replaced
C         IF (DBLE(EPSB)-O) 410, 410, 420
         IF (DBLE(EPSB).LE.O) THEN
            GOTO 410
         ELSE
            GOTO 420
         END IF

C
C IB=-1: CONVERGENCE TESTS FAILED.
C
 410     CONTINUE
         IB = -1
         IF (IC.EQ.MAXEXT) EXTRAP(ID,IC) = XINTER(ID)
 420  CONTINUE
C
C IA IS NEW COLUMN, I.E. LAST STORED.
C
      GOTO 460
 430  CONTINUE
      DO 450 ID=1,NDE
         DO 440 IE=1,IA
            EXTRAP(ID,IE) = XINTER(ID)
 440     CONTINUE
         EXTRAP(ID,IC) = XINTER(ID)
 450  CONTINUE
      IB = -1
      IF (IC.EQ.MAXEXT) EXTRAP(ID,IC) = XINTER(ID)
 460  CONTINUE
      IA = IC
C
C O BECOMES FRACTIONAL ERROR.
C
      O = O/DBLE(EPSB)
C
C IF CONVERGENCE PASSED GOTO 340.
C
      IF (IB.GE.0 .AND. K.EQ.1) WRITE (6,470) IA
 470  FORMAT('0','NUMBER OF ITERATIONS',I6)
      IF (IB.GE.0) GOTO 520
      IF (IB.LT.0 .AND. IA.EQ.MAXEXT) WRITE (6,480)
 480  FORMAT('   CASE NOT PROPERLY CONVERGED, but if no "?" flags set',
     +     ' then convergence achieved for ALL printed isotopes.')
      IF (IB.LT.0 .AND. IA.EQ.MAXEXT) GOTO 520
C
C IF L IS SET TO COMPARE THIS COLUMN AND NO CONVERGENCE HAS BEEN
C OBTAINED, COMPUTE FRACTIONAL ERROR.
C
      IF (IA.NE.L) GOTO 330
      IF (INUM.EQ.0) WRITE (6,490)
 490  FORMAT('0','CONVERGENCE NOT ACHIEVED BY FIRST ROUTE-TRY SECOND')
      INUM = 1
      ASSIGN 500 TO JUMP
      GOTO 70
C
C REDUCE CONFIDENCE FACTOR.
C
 500  CONTINUE
      B = B*REAL(ERRCOL)**(1.0/REAL(1-IA))
C
C RETURN TO 20 FOR NEXT ITERATION.
C
      GOTO 330
C
C TOTAL CONVERGENCE FAILURE, BUT CONVERGENCE IN EARLY COLUMN,
C SET MAXEXT TO THIS COLUMN.
C
 510  CONTINUE
      MAXEXT = IA
C
C REDUCE STEP LENGTH.
C
      H = H/REAL(NSTEPS)
C
C INCREASE NUMBER OF STEPS TO GO.
C
      K = 1 + NSTEPS*(K-1)
 520  CONTINUE
      DO 530 L=1,NDE
C
C *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
C
         IF (EXTRAP(L,IA).LT.MIND) EXTRAP(L,IA) = 0.0D0
         XINTER(L) = EXTRAP(L,IA)
         YINTER(L) = XINTER(L)
         Y(L) = EXTRAP(L,IA)
 530  CONTINUE
      IF (M.EQ.N) GOTO 600
 540  CONTINUE
      DO 550 L=M1,N
         ZINTER(L) = YINTER(L)
         XINTER(L) = S(L)*C1(L)
 550  CONTINUE
      IF (I2.EQ.I1) GOTO 570
      DO 560 L=I4,I1
         NP = NP1(L)
         ND = ND1(L)
         IF (YINTER(NP).LT.1.D-50) YINTER(NP)=0.0D0
         XINTER(ND) = XINTER(ND) + DBLE(TR1(L))*YINTER(NP)*C1(ND)
 560  CONTINUE
 570  CONTINUE
      DO 580 L=M1,N
C
C *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
C
         IF (XINTER(L).LT.MIND) XINTER(L) = 0.0D0
         YINTER(L) = XINTER(L)
         Y(L) = XINTER(L)
 580  CONTINUE
      ICRI = 0
      DO 590 L=M1,N
         CRIT = REAL(ZINTER(L) - YINTER(L))
         IF (YINTER(L).LT.1.0D-5) GOTO 590
         CRIT = CRIT/REAL(YINTER(L))
         IF (ABS(CRIT).GT.EPSB) ICRI = 1
 590  CONTINUE
      IF (ICRI.GT.0) GOTO 540
 600  CONTINUE
C
C DECREASE NUMBER OF STEPS.
C
      K = K - 1
C
C SET COLUMN INDICATOR TO ZERO FOR NO TEST.
C
      L = 0
C
C IF ONLY ONE STEP LEFT GOTO 18 TO COMPLETE INTERVAL.
C IF MORE THAN ONE STEP LEFT, F IS DISTANCE TO GO.
C
C+**PJK 29/11/93 Arithmetic-IF replaced
C      IF (1-K) 610, 300, 620
      IF (K.GT.1) THEN
         GOTO 610
      ELSE IF (K.EQ.1) THEN
         GOTO 300
      ELSE
         GOTO 620
      END IF

 610  CONTINUE
      F = DBLE(H*REAL(K))
C
C L IS SET TO ORDER OF EXTRAPOLATION.
C
      L = IA
      FLODIV = REAL(F)/(H*(1.0+B*(REAL(O)**(1.0/REAL(1-IA))-1.0))) + 0.9
      K = INT(FLODIV)
C
C STEP LENGTH IS INCREASED BY CONFIDENCE FACTOR TIMES ESTIMATED
C FULL INCREASE TO PRODUCE CONVERGENCE IN IATH COLUMN,
C
      H = REAL(F)/REAL(K)
C
C RETURN TO 18 FOR NEXT STEP.
C
      GOTO 300
C
C END OF INTERVAL SO RETURN.
C
 620  CONTINUE
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE INTEGT(M, FLUXT, YTOTAL, TIME)
C***********************************************************************
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Integration routine, used for the inventory calculations.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  M
C  FLUXT
C  YTOTAL
C  TIME
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      INTEGER M
      REAL FLUXT,YTOTAL,TIME

C  Global variables
      INTEGER MAXXT
      REAL CONVV
      COMMON /CONVRG/ MAXXT,CONVV

      REAL FLUX(3)
      COMMON /DATA0 / FLUX

      DOUBLE PRECISION MIND
      COMMON /DATA1 / MIND

      INTEGER I22,I31,I32
      REAL BR1(1000)
      COMMON /EXT   / I22,I31,I32,BR1

      INTEGER NP1(1000),ND1(1000),I2,I1,N
      REAL TR1(1000),YLAM(364)
      COMMON /TRANS0/ NP1,ND1,TR1,I2,I1,N,YLAM

      DOUBLE PRECISION Y(364)
      COMMON /TRANS1/ Y

C  Local variables
      DOUBLE PRECISION A1,B1,D1,F1,R,O,Q,S(364),YY(364),F,ERRCOL
      DOUBLE PRECISION C1(364),E1(364),EXTRAP(364,10),G1(364),SX(364),
     +     XINTER(364),YINTER(364),ZINTER(364)

      INTEGER I,I23,I4,IA,IB,IC,ICRI,ID,IE,IND,INUM,IQ,IQP,IQQ,IR,IXY,
     +     J,J1,JUMP,JYY,K,K1,L,M1,MAXEXT,ND,NDE,NDQ,NP,NPQ,NPX,NQX,
     +     NSTEPS

      REAL B,BRX,BRY,BSX,BSY,CRIT,D2,EPSB,FLODIV,H,STEPLE,TMAX

C  External routines
      EXTERNAL INTEGS

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C+**PJK 29/11/93
      IA = 0
      NSTEPS = 0
      STEPLE = 0.0
      DO 2 J = 1,10
         DO 1 I = 1,364
            EXTRAP(I,J) = 0.0D0
 1       CONTINUE
 2    CONTINUE

      DO 5 I = 1,N
         S(I) = 0.0D0
 5    CONTINUE
      EPSB = CONVV
      MAXEXT = MAXXT
      TMAX = TIME
      H = TIME
      NDE = M
      M1 = M + 1
      I4 = I2 + 1
      JYY = 0
      INUM = 0
      DO 10 I=1,N
         YY(I) = Y(I)
 10   CONTINUE
C
C CORRECTION FOR EQUILIBRIUM ISOTOPES LOSS
C
      IF (M.EQ.N) GOTO 290
      I23 = I22 + 1
      NPX = 0
      IF (I23.GE.I32) GOTO 40
      BRX = 0.0
      DO 30 IQ=I23,I32
         NP = NP1(IQ)
         IF (NP.EQ.NPX) BRX = BRX + BR1(IQ)
         ND = ND1(IQ)
         IF (ND.EQ.0 .OR. NP.EQ.0) GOTO 30
         Y(ND) = Y(ND) + Y(NP)*DBLE(BR1(IQ))
         BRY = 1.0 - BRX
         IF (BRY.LT.1.0E-6) Y(NP) = 0.0D0
         IF (IQ.EQ.I23) GOTO 30
         IQQ = IQ - 1
         NQX = 0
         BSX = 0.0
         DO 20 IQP=I23,IQQ
            NPQ = NP1(IQP)
            IF (NPQ.EQ.NQX) BSX = BSX + BR1(IQP)
            NDQ = ND1(IQP)
            IF (NPQ.NE.ND) GOTO 20
            BSY = 1.0 - BSX
            IF (BSY.LT.1.0E-6) Y(ND) = 0.0D0
            Y(NDQ) = Y(NDQ) + Y(NP)*DBLE(BR1(IQP))
 20      CONTINUE
 30   CONTINUE
 40   CONTINUE
      NPX = 0
      BRX = 0.0
      IF (I31.EQ.0) GOTO 70
      DO 60 IQ=1,I31
         IF (FLUXT.GT.0.0 .AND. TIME.GT.0.0) GOTO 60
         NP = NP1(IQ)
         IF (NP.EQ.NPX) BRX = BRX + BR1(IQ)
         DO 50 IR=1,M
            IF (NP.EQ.IR) GOTO 60
 50      CONTINUE
         ND = ND1(IQ)
         Y(ND) = Y(ND) + Y(NP)*DBLE(BR1(IQ))
         BRY = 1.0 - BRX
         IF (BRY.LT.1.0E-6) Y(NP) = 0.0D0
 60   CONTINUE
 70   CONTINUE
C
C SET INITIAL VALUES INTO XINTER.
C
      GOTO 290
C
C MODULE ERRCOL
C COMPUTES FRACTIONAL ERROR IN IATH COLUMN OF EXTRAPOLATION ARRAY.
C A1 IS SET TO MAXIMUM ERROR OBTAINED FROM DIFFERENCE OF ADJACENT
C COLUMNS OF EXTRAPOLATION ARRAY.
C
 80   CONTINUE
      A1 = 0.0D0
      J1 = IA - 1
      DO 100 K1=1,NDE
         B1 = EXTRAP(K1,IA)
         IF (ABS(B1).GT.1.0D-12) GOTO 90
         A1 = MAX(A1,ABS(B1-EXTRAP(K1,J1))*1.0D+12)
         GOTO 100
 90      CONTINUE
         A1 = MAX(A1,ABS(1.0D0-EXTRAP(K1,J1)/B1))
 100  CONTINUE
C
C FRACTIONAL ERROR = ERROR FOUND / MAXIMUM ERROR.
C
      ERRCOL = A1/DBLE(EPSB)
      GOTO JUMP, (370, 470)
C
C
C MODULE PREDL
C
C A1 = CONVERGENCE CRITERION FOR SERIES FOR EXPONENTIAL FACTOR.
C
 110  CONTINUE
      A1 = 1.0D-06*DBLE(STEPLE)
      IF (M.EQ.N) GOTO 130
      DO 120 I=M1,N
         B1 = DBLE(YLAM(I))
         C1(I) = 1.0D0/B1
 120  CONTINUE
 130  CONTINUE
      DO 160 I=1,NDE
C
C SET INTERVAL VALUES INTO XINTER.
C
         XINTER(I) = Y(I)
C
C C(I) IS DIAGONAL ELEMENT.
C
         B1 = DBLE(-YLAM(I))
C
C D=LAM*H FOR ONE-STEP METHOD.
C
         D1 = B1*DBLE(STEPLE)
         D2 = REAL(ABS(D1))
         IF (D2.LT.150.0) G1(I) = EXP(D1)
         IF (D2.GE.150.0) G1(I) = 0.0D0
C
C IF LAM*H IS SMALL, THEN COMPUTE EXPONENTIAL FACTORS BY SERIES
C SERIES E1=SUM(LAM(I-1)*H(I)).
C AT 322, OTHERWISE E1=EXP(LAM*H).
C
         IF (ABS(D1).LT.0.1D00) GOTO 140
         IF (D2.LT.150.0) E1(I) = (EXP(D1)-1.0D0)/B1
         IF (D2.GE.150.0) E1(I) = (-1.0D0)/B1
         GOTO 160
 140     CONTINUE
         B1 = DBLE(STEPLE)
         F1 = B1
C
C J1 COUNTS TERMS, F1 IS LATEST TERM, B1 IS SUM OF TERMS.
C
         J1 = 1
 150     CONTINUE
         J1 = J1 + 1
         F1 = D1*F1/DBLE(J1)
         B1 = B1 + F1
         IF (A1.LT.ABS(F1)) GOTO 150
C
C SET RESULT INTO E1, FINISHING WHEN MOD.F1 IS LESS THAN A1.
C
         E1(I) = B1
 160  CONTINUE
C
C I COUNTS STEPS OF ONE-STEP METHOD.
C
      I = 0
 170  CONTINUE
      I = I + 1
C
C EVALUATE R.H.S. OF DN/DT=AN=DX.
C NDE IS NUMBER OF BURNABLE NUCLIDES IN PROBLEM (LESS THAN 101)
C NJ1,NJ2 ARE TABLES OF NUCLIDES PRODUCED BY CAPTURE AND DECAY.
C X IS THE NUMBER DENSITY TABLE.
C NNF IS THE NUMBER OF FISSILE NUCLIDES.
C NNFP IS THE NUMBER OF FISSION PRODUCTS IN PROBLEM.
C
      IF (M.EQ.N) GOTO 230
      DO 180 IQ=M1,N
         XINTER(IQ) = S(IQ)*C1(IQ)
 180  CONTINUE
      IF (I.GT.1) GOTO 200
      DO 190 IQ=1,N
         YINTER(IQ) = XINTER(IQ)
 190  CONTINUE
 200  CONTINUE
      IF (I2.EQ.I1) GOTO 230
      DO 210 IQ=I4,I1
         NP = NP1(IQ)
         ND = ND1(IQ)
         IF (YINTER(NP).LT.1.D-50) YINTER(NP)=0.0D0
         XINTER(ND) = XINTER(ND) + DBLE(TR1(IQ))*YINTER(NP)*C1(ND)
 210  CONTINUE
      DO 220 IQ=M1,N
         YINTER(IQ) = XINTER(IQ)
 220  CONTINUE
 230  CONTINUE
      DO 240 IQ=1,NDE
         SX(IQ) = S(IQ)
 240  CONTINUE
      DO 250 IQ=1,I2
         NP = NP1(IQ)
         ND = ND1(IQ)
         IF (XINTER(NP).LT.1.D-50) XINTER(NP)=0.0D0
         SX(ND) = SX(ND) + DBLE(TR1(IQ))*XINTER(NP)
 250  CONTINUE
      JYY = JYY + 1
      DO 260 K1=1,NDE
         IF (XINTER(K1).LT.1.D-50) XINTER(K1)=0.0D0
         XINTER(K1) = XINTER(K1)*G1(K1) + SX(K1)*E1(K1)
 260  CONTINUE
      DO 270 IQ=1,N
         YINTER(IQ) = XINTER(IQ)
 270  CONTINUE
C
C ONE-STEP METHOD:  X=X+(EXP(LAM*H)-1.0)/LAM*DX/DT.
C TAKEN OVER 'NSTEPS' STEPS OF LENGTH 'STEPLE'.
C
      IF (I.NE.NSTEPS) GOTO 170
      GOTO JUMP, (320, 400)
 280  FORMAT (' --STEP LENGTH HAS BECOME NEGLIGIBLY SMALL--')
C
C IND - STEP REDUCTION FACTOR.
C
 290  CONTINUE
      IND = 1
C
C B - CONFIDENCE FACTOR FOR STEP CHANGING,
C IF B=1.0 FULL CONFIDENCE,  IF B=0 NO CONFIDENCE.
C
      B = 1.0
C
C K IS NUMBER OF STEPS TO COVER INTERVAL TMAX.
C
      K = INT(MAX(1.1,TMAX/H+0.1))
C
C H - STEP LENGTH OF EXTRAPOLATION PROCEDURE.
C
      H = TMAX/REAL(K)
C
C L - COLUMN ON WHICH TO TEST CONVERGENCE, IF L=0 NO TEST.
C
      L = 0
C
C DECREASE H BY STEP-LENGTH REDUCTION FACTOR (IND).
C
 300  CONTINUE
      H = H/REAL(IND)
C
C INCREASE NUMBER OF STEPS (K) CORRESPONDINGLY.
C
      K = IND*K
C
C RESET REDUCTION FACTOR TO UNITY.
C
      IND = 1
C
C TEST IF H IS TOO SMALL, I.E. SOMETHING GONE WRONG.
C
      IF (H.GT.TMAX*0.1E-4) GOTO 310
      WRITE (6,280)
      STOP 1
C
C STEPLE - ONE-STEP METHOD STEP LENGTH
C STEPLE SET FIRSTLY TO EXTRAPOLATION STEP (H)
C
 310  CONTINUE
      STEPLE = H
C
C NSTEPS - NUMBER OF STEPS OF LENGTH STEPLE TO COVER EXTRAPOLATION
C STEP (H).
C
      NSTEPS = 1
C
C INTEGRATE EQUATIONS BY 1 STEP OF ONE-STEP METHOD.
C
      ASSIGN 320 TO JUMP
      GOTO 110
 320  CONTINUE
      DO 330 IA=1,NDE
C
C STORE INITIAL  RESULTS  INTO  EXTRAPOLATION ARRAY
C
         EXTRAP(IA,1) = XINTER(IA)
 330  CONTINUE
C
C IA  COUNTS  COLUMNS  OF  EXTRAPOLATION  ARRAY
C
      IA = 1
C
C IF IA=MAXEXT WE HAVE USED STORAGE FOR EXTRAPOLATION    U
C WITHOUT CONVERGENCE.
C
 340  CONTINUE
      IF (MAXEXT.GT.IA) GOTO 390
C
C *****  ADDITION APRIL 1983  *****
C
      DO 350 IXY=1,N
C
C *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
C
         IF (YY(IXY).LT.MIND) YY(IXY) = 0.0D0
         Y(IXY) = YY(IXY)
 350  CONTINUE
      CALL INTEGS(M, FLUXT, YTOTAL, TIME)
      IF (N.GT.1) GOTO 590
C
C *********************************
C IB - NUMBER OF DERIVATIVE EVALUATIONS TO OBTAIN CONVERGENCE  IN
C 1 EXTRAPOLATION.
C
      IB = 2
C
C F - WILL CONTAIN BEST ESTIMATE OF FUNCTION EVALUATIONS PER UNIT
C STEP USING VARIOUS ORDERS OF EXTRAPOLATION.
C
      F = 1.0D+37
C
C IA COUNTS THROUGH COLUMNS OF EXRAPOLATION ARRAY.
C NSTEPS IS NUMBER OF STEPS REQUIRED TO COVER INTERVAL USING IATH
C ORDER METHOD.
C
      IA = 1
 360  CONTINUE
      IA = IA + 1
      NSTEPS = NSTEPS/2
      ASSIGN 370 TO JUMP
      GOTO 80
C
C COMPUTE FRACTIONAL ERROR IN IATH COLUMN.
C
 370  CONTINUE
      O = ERRCOL
C
C IF THIS COLUMN HAS CONVERGED ALREADY GOTO 45.
C
      IF (O.LE.1.0D+00) GOTO 480
C
C INCREASE NUMBER OF DERIVATIVE EVALUATIONS AS ORDER INCREASES.
C
      IB = IB + IB
C
C STEPLE IS ESTIMATED STEP LENGTH TO PRODUCE CONVERGENCE IN THIS
C COLUMN.
C
      STEPLE = H*REAL(O)**(1.0/REAL(1-IA))/REAL(NSTEPS)
C
C Q IS FUNCTION EVALUATIONS PER UNIT STEP.
C
      Q = DBLE(IB)/DBLE(STEPLE)
      IF (Q.GT.F) GOTO 380
C
C STORE BEST FUNCTION EVALUATIONS PER UNIT STEP.
C
      F = Q
C
C SET L TO COLUMN FOR LATER TEST.
C
      L = IA
C
C R IS STEP LENGTH REQUIRED FOR ORDER.
C
      R = DBLE(STEPLE)
 380  CONTINUE
      IF (IA.NE.MAXEXT) GOTO 360
C
C SET MAXEXT TO BEST ORDER EXTRAPOLATION.
C
      MAXEXT = L
C
C SET STEP REDUCTION FACTOR TO REDUCE STEP LENGTH,
C MODIFIED BY CONFIDENCE FACTOR B.
C RESUME AT 16.
C
      FLODIV = H/(B*REAL(R)) + 1.0
      IND = INT(FLODIV)
      GOTO 300
C
C DOUBLE NUMBER OF STEPS OF ONE-STEP METHOD.
C
 390  CONTINUE
      NSTEPS = NSTEPS + NSTEPS
 
C
C DECREASE STEP LENGTH.
C
      STEPLE = 0.5*STEPLE
      ASSIGN 400 TO JUMP
C
C INTEGRATE EQUATIONS WITH 'NSTEPS' STEPS OF SIZE 'STEPLE'.
C
      GOTO 110
C
C IB - CONVERGENCE TEST INDICATOR.
C
 400  CONTINUE
      IB = 1
C
C IC - NEXT COLUMN OF EXTRAPOLATION.
C
      IC = IA + 1
C
C O - MAXIMUM ERROR.
C
      O = 0.0D+00
      DO 440 ID=1,NDE
C
C F - EXTRAPOLATION FACTOR.
C
         F = 1.0D0
C
C Q - LATEST ONE-STEP ESTIMATE.
C
         Q = XINTER(ID)
         DO 410 IE=1,IA
C
C F DOUBLES TO BECOME 2.0 TO THE POWER (R+1.0).
C
            F = F + F
C
C R - OLD VALUE IN THIS COLUMN OF EXTRAPOLATION OVERWRITTEN BY Q.
C
            R = EXTRAP(ID,IE)
            EXTRAP(ID,IE) = Q
C
C Q UPDATED TO NEXT COLUMN ESTIMATE,
C Q = (2.0**(R+1.0)*(NEW-OLD))/(2.0**(R+1)-1.0).
C
            Q = (F*Q-R)/(F-1.0D0)
 410     CONTINUE
         EXTRAP(ID,IC) = Q
C
C IF CONVERGENCE TESTS ALREADY FAILED TEST NO FURTHER,
C ELSE O IS SET TO MAXIMUM ERROR FOUND AND TESTED AGAINST MAX. ERROR
C
         IF (IB.LT.0) GOTO 440
         B1 = EXTRAP(ID,IA)
         IF (ABS(B1).GT.1.0D-12) GOTO 420
         O = MAX(O,ABS(B1-R)*1.0D+4)
C+**PJK 29/11/93 Arithmetic-IF replaced
C         IF (DBLE(EPSB)-O) 430, 440, 440
         IF (DBLE(EPSB).LT.O) THEN
            GOTO 430
         ELSE
            GOTO 440
         END IF

 420     CONTINUE
         O = MAX(O,ABS(1.0D+00-R/B1))
C+**PJK 29/11/93 Arithmetic-IF replaced
C         IF (DBLE(EPSB)-O) 430, 430, 440
         IF (DBLE(EPSB).LE.O) THEN
            GOTO 430
         ELSE
            GOTO 440
         END IF
C
C IB=-1: CONVERGENCE TESTS FAILED.
C
 430     CONTINUE
         IB = -1
 440  CONTINUE
C
C IA IS NEW COLUMN, I.E. LAST STORED.
C
      IA = IC
C
C O BECOMES FRACTIONAL ERROR.
C
      O = O/DBLE(EPSB)
C
C IF CONVERGENCE PASSED GOTO 46.
C
      IF (IB.GE.0 .AND. K.EQ.1) WRITE (6,450) IA
 450  FORMAT('0','NUMBER OF ITERATIONS',I6)
      IF (IB.GE.0) GOTO 490
C
C IF L IS SET TO COMPARE THIS COLUMN AND NO CONVERGENCE HAS BEEN
C OBTAINED, COMPUTE FRACTIONAL ERROR.
C
      IF (IA.NE.L) GOTO 340
      IF (INUM.EQ.0) WRITE (6,460)
 460  FORMAT('0','CONVERGENCE NOT ACHIEVED BY FIRST ROUTE-NOW SECOND')
      INUM = 1
      ASSIGN 470 TO JUMP
      GOTO 80
C
C REDUCE CONFIDENCE FACTOR.
C
 470  CONTINUE
      B = B*REAL(ERRCOL)**(1.0/REAL(1-IA))
C
C RETURN TO 20 FOR NEXT ITERATION.
C
      GOTO 340
C
C TOTAL CONVERGENCE FAILURE, BUT CONVERGENCE IN EARLY COLUMN,
C SET MAXEXT TO THIS COLUMN.
C
 480  CONTINUE
      MAXEXT = IA
C
C REDUCE STEP LENGTH.
C
      H = H/REAL(NSTEPS)
C
C INCREASE NUMBER OF STEPS TO GO.
C
      K = 1 + NSTEPS*(K-1)
 490  CONTINUE
      DO 500 L=1,NDE
C
C *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
C
         IF (EXTRAP(L,IA).LT.MIND) EXTRAP(L,IA) = 0.0D0
         XINTER(L) = EXTRAP(L,IA)
         YINTER(L) = XINTER(L)
         Y(L) = EXTRAP(L,IA)
 500  CONTINUE
      IF (M.EQ.N) GOTO 570
 510  CONTINUE
      DO 520 L=M1,N
         ZINTER(L) = YINTER(L)
         XINTER(L) = S(L)*C1(L)
 520  CONTINUE
      IF (I2.EQ.I1) GOTO 540
      DO 530 L=I4,I1
         NP = NP1(L)
         ND = ND1(L)
         IF (YINTER(NP).LT.1.D-50) YINTER(NP)=0.0D0
         XINTER(ND) = XINTER(ND) + DBLE(TR1(L))*YINTER(NP)*C1(ND)
 530  CONTINUE
 540  CONTINUE
      DO 550 L=M1,N
C
C *****  CHECK FOR NUMBER DENSITIES LESS THAN "MIND" (DEFAULT 1.0)
C
         IF (XINTER(L).LT.MIND) XINTER(L) = 0.0D0
         YINTER(L) = XINTER(L)
         Y(L) = XINTER(L)
 550  CONTINUE
      ICRI = 0
      DO 560 L=M1,N
         CRIT = REAL(ZINTER(L) - YINTER(L))
         IF (YINTER(L).LT.1.0D-5) GOTO 560
         CRIT = CRIT/REAL(YINTER(L))
         IF (ABS(CRIT).GT.EPSB) ICRI = 1
 560  CONTINUE
      IF (ICRI.GT.0) GOTO 510
 570  CONTINUE
C
C DECREASE NUMBER OF STEPS.
C
      K = K - 1
C
C SET COLUMN INDICATOR TO ZERO FOR NO TEST.
C
      L = 0
C
C IF ONLY ONE STEP LEFT GOTO 18 TO COMPLETE INTERVAL.
C IF MORE THAN ONE STEP LEFT, F IS DISTANCE TO GO.
C
C+**PJK 29/11/93 Arithmetic-IF replaced
C      IF (1-K) 580, 310, 590
      IF (K.GT.1) THEN
         GOTO 580
      ELSE IF (K.EQ.1) THEN
         GOTO 310
      ELSE
         GOTO 590
      END IF

 580  CONTINUE
      F = DBLE(H)*DBLE(K)
C
C L IS SET TO ORDER OF EXTRAPOLATION.
C
      L = IA
      FLODIV = REAL(F)/(H*(1.0+B*(REAL(O)**(1.0/REAL(1-IA))-1.0))) + 0.9
      K = INT(FLODIV)
C
C STEP LENGTH IS INCREASED BY CONFIDENCE FACTOR TIMES ESTIMATED
C FULL INCREASE TO PRODUCE CONVERGENCE IN IATH COLUMN,
C
      H = REAL(F)/REAL(K)
C
C RETURN TO 18 FOR NEXT STEP.
C
      GOTO 310
C
C END OF INTERVAL SO RETURN.
C
 590  CONTINUE
      RETURN
      END

C----------------------------------------------------------------------
      INTEGER FUNCTION LENGT(CHRSTR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Enables the length of a character string to be calculated
C  excluding any trailing blanks.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  CHRSTR : (INPUT)  Character string of interest
C
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Arguments
      CHARACTER*(*) CHRSTR

C  Local variables
      INTEGER I,I1

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      I1 = LEN(CHRSTR)
      LENGT = I1
      DO 10 I = I1,1,-1
        IF (CHRSTR(I:I).EQ.' ') THEN
          LENGT = LENGT - 1
        ELSE
          GOTO 20
        END IF
 10   CONTINUE

 20   CONTINUE
      END

C----------------------------------------------------------------------
      SUBROUTINE MASSIN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Subroutine to convert from wt% elements to atoms of each nuclide.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      INTEGER NSTEPS
      REAL GMS,YSTEPS,ZLEVEL
      COMMON /CALIST/ GMS,NSTEPS,YSTEPS,ZLEVEL

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      REAL XA(19),XMU(19)
      COMMON /DOSOUT/ XA,XMU

      INTEGER IATMWT(364),ISEX2
      REAL FUWT(364)
      COMMON /INFO1 / IATMWT,FUWT,ISEX2

      CHARACTER*4 IDS(364)
      COMMON /INFO2 / IDS

      REAL FLUX2,T
      COMMON /OUT1  / FLUX2,T

      CHARACTER*4 NAMREP(83)
      COMMON /OUT2  / NAMREP

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

C  Local variables
      REAL XNUM(92),AWT(92),ABUN(13,92),ABN1(13,20),ABN2(13,20),
     +     ABN3(13,20),ABN4(13,20),ABN5(13,12)
      REAL ZA
      INTEGER NUMB(92,2)
      INTEGER IA,J1,J2,KNUM,MAT

C  External routines
      EXTERNAL CONV

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SAVE ABN1,ABN2,ABN3,ABN4,ABN5

      EQUIVALENCE (ABN1(1,1),ABUN(1,1))
      EQUIVALENCE (ABN2(1,1),ABUN(1,21))
      EQUIVALENCE (ABN3(1,1),ABUN(1,41))
      EQUIVALENCE (ABN4(1,1),ABUN(1,61))
      EQUIVALENCE (ABN5(1,1),ABUN(1,81))

      DATA AWT/1.00794,4.002602,6.941,9.01218,10.811,12.011,14.0067,
     +     15.9994,18.998403,20.179,22.98977,24.305,26.98154,28.0855,
     +     30.97376,32.066,35.453,39.948,39.0983,40.078,44.95591,47.88,
     +     50.9415,51.9961,54.9380,55.847,58.9332,58.69,63.546,65.39,
     +     69.723,72.59,74.9216,78.96,79.904,83.80,85.4678,87.62,
     +     88.9059,91.224,92.9064,95.94,0.,101.07,102.9055,106.42,
     +     107.8682,112.41,114.82,118.710,121.75,127.60,126.9045,131.29,
     +     132.9054,137.33,138.9055,140.12,140.9077,144.24,0.,150.36,
     +     151.96,157.25,158.9254,162.50,164.9304,167.26,168.9342,
     +     173.04,174.967,178.49,180.9479,183.85,186.207,190.2,192.22,
     +     195.08,196.9665,200.59,204.383,207.2,208.9804,0.,0.,0.,0.,0.,
     +     0.,232.0381,0.,238.0289/
      DATA NUMB(1,1),NUMB(1,2)/2,1/
      DATA NUMB(2,1),NUMB(2,2)/2,3/
      DATA NUMB(3,1),NUMB(3,2)/2,6/
      DATA NUMB(4,1),NUMB(4,2)/1,9/
      DATA NUMB(5,1),NUMB(5,2)/2,10/
      DATA NUMB(6,1),NUMB(6,2)/2,12/
      DATA NUMB(7,1),NUMB(7,2)/2,14/
      DATA NUMB(8,1),NUMB(8,2)/3,16/
      DATA NUMB(9,1),NUMB(9,2)/1,19/
      DATA NUMB(10,1),NUMB(10,2)/3,20/
      DATA NUMB(11,1),NUMB(11,2)/1,23/
      DATA NUMB(12,1),NUMB(12,2)/3,24/
      DATA NUMB(13,1),NUMB(13,2)/1,27/
      DATA NUMB(14,1),NUMB(14,2)/3,28/
      DATA NUMB(15,1),NUMB(15,2)/1,31/
      DATA NUMB(16,1),NUMB(16,2)/5,32/
      DATA NUMB(17,1),NUMB(17,2)/3,35/
      DATA NUMB(18,1),NUMB(18,2)/5,36/
      DATA NUMB(19,1),NUMB(19,2)/3,39/
      DATA NUMB(20,1),NUMB(20,2)/9,40/
      DATA NUMB(21,1),NUMB(21,2)/1,45/
      DATA NUMB(22,1),NUMB(22,2)/5,46/
      DATA NUMB(23,1),NUMB(23,2)/2,50/
      DATA NUMB(24,1),NUMB(24,2)/5,50/
      DATA NUMB(25,1),NUMB(25,2)/1,55/
      DATA NUMB(26,1),NUMB(26,2)/5,54/
      DATA NUMB(27,1),NUMB(27,2)/1,59/
      DATA NUMB(28,1),NUMB(28,2)/7,58/
      DATA NUMB(29,1),NUMB(29,2)/3,63/
      DATA NUMB(30,1),NUMB(30,2)/7,64/
      DATA NUMB(31,1),NUMB(31,2)/3,69/
      DATA NUMB(32,1),NUMB(32,2)/7,70/
      DATA NUMB(33,1),NUMB(33,2)/1,75/
      DATA NUMB(34,1),NUMB(34,2)/9,74/
      DATA NUMB(35,1),NUMB(35,2)/3,79/
      DATA NUMB(36,1),NUMB(36,2)/9,78/
      DATA NUMB(37,1),NUMB(37,2)/3,85/
      DATA NUMB(38,1),NUMB(38,2)/5,84/
      DATA NUMB(39,1),NUMB(39,2)/1,89/
      DATA NUMB(40,1),NUMB(40,2)/7,90/
      DATA NUMB(41,1),NUMB(41,2)/1,93/
      DATA NUMB(42,1),NUMB(42,2)/9,92/
      DATA NUMB(43,1),NUMB(43,2)/0,0/
      DATA NUMB(44,1),NUMB(44,2)/9,96/
      DATA NUMB(45,1),NUMB(45,2)/1,103/
      DATA NUMB(46,1),NUMB(46,2)/9,102/
      DATA NUMB(47,1),NUMB(47,2)/3,107/
      DATA NUMB(48,1),NUMB(48,2)/11,106/
      DATA NUMB(49,1),NUMB(49,2)/3,113/
      DATA NUMB(50,1),NUMB(50,2)/13,112/
      DATA NUMB(51,1),NUMB(51,2)/3,121/
      DATA NUMB(52,1),NUMB(52,2)/11,120/
      DATA NUMB(53,1),NUMB(53,2)/1,127/
      DATA NUMB(54,1),NUMB(54,2)/13,124/
      DATA NUMB(55,1),NUMB(55,2)/1,133/
      DATA NUMB(56,1),NUMB(56,2)/9,130/
      DATA NUMB(57,1),NUMB(57,2)/2,138/
      DATA NUMB(58,1),NUMB(58,2)/7,136/
      DATA NUMB(59,1),NUMB(59,2)/1,141/
      DATA NUMB(60,1),NUMB(60,2)/9,142/
      DATA NUMB(61,1),NUMB(61,2)/0,0/
      DATA NUMB(62,1),NUMB(62,2)/11,144/
      DATA NUMB(63,1),NUMB(63,2)/3,151/
      DATA NUMB(64,1),NUMB(64,2)/9,152/
      DATA NUMB(65,1),NUMB(65,2)/1,159/
      DATA NUMB(66,1),NUMB(66,2)/9,156/
      DATA NUMB(67,1),NUMB(67,2)/1,165/
      DATA NUMB(68,1),NUMB(68,2)/9,162/
      DATA NUMB(69,1),NUMB(69,2)/1,169/
      DATA NUMB(70,1),NUMB(70,2)/9,168/
      DATA NUMB(71,1),NUMB(71,2)/2,175/
      DATA NUMB(72,1),NUMB(72,2)/7,174/
      DATA NUMB(73,1),NUMB(73,2)/2,180/
      DATA NUMB(74,1),NUMB(74,2)/7,180/
      DATA NUMB(75,1),NUMB(75,2)/3,185/
      DATA NUMB(76,1),NUMB(76,2)/9,184/
      DATA NUMB(77,1),NUMB(77,2)/3,191/
      DATA NUMB(78,1),NUMB(78,2)/9,190/
      DATA NUMB(79,1),NUMB(79,2)/1,197/
      DATA NUMB(80,1),NUMB(80,2)/9,196/
      DATA NUMB(81,1),NUMB(81,2)/3,203/
      DATA NUMB(82,1),NUMB(82,2)/5,204/
      DATA NUMB(83,1),NUMB(83,2)/1,209/
      DATA NUMB(84,1),NUMB(84,2)/0,0/
      DATA NUMB(85,1),NUMB(85,2)/0,0/
      DATA NUMB(86,1),NUMB(86,2)/0,0/
      DATA NUMB(87,1),NUMB(87,2)/0,0/
      DATA NUMB(88,1),NUMB(88,2)/0,0/
      DATA NUMB(89,1),NUMB(89,2)/0,0/
      DATA NUMB(90,1),NUMB(90,2)/1,232/
      DATA NUMB(91,1),NUMB(91,2)/0,0/
      DATA NUMB(92,1),NUMB(92,2)/5,234/
      DATA ABN1/99.985,0.015,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.000138,
     +     99.999862,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,7.5,92.5,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,19.9,80.1,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,98.9,1.1,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,99.634,0.366,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,99.762,0.038,0.2,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,90.51,0.27,9.22,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,78.99,10.0,11.01,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,92.23,4.67,3.10,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,95.02,0.75,4.21,0.,0.02,0.,0.,0.,0.,0.,0.,0.,0.,
     +     75.77,0.,24.23,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.337,0.,0.063,
     +     0.,99.6,0.,0.,0.,0.,0.,0.,0.,0.,93.2581,0.0117,6.7302,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,96.941,0.,0.647,0.135,2.086,0.,0.004,
     +     0.,0.187,0.,0.,0.,0./
      DATA ABN2/100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,8.0,7.3,73.8,
     +     5.5,5.4,0.,0.,0.,0.,0.,0.,0.,0.,0.250,99.750,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,4.345,0.,83.789,9.501,2.365,0.,0.,0.,0.,0.,
     +     0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,5.8,0.,
     +     91.72,2.2,0.28,0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,68.27,0.,26.10,1.13,3.59,0.,0.91,0.,0.,
     +     0.,0.,0.,0.,69.17,0.,30.83,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     48.6,0.,27.9,4.1,18.8,0.,0.6,0.,0.,0.,0.,0.,0.,60.1,0.,39.9,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,20.5,0.,27.4,7.8,36.5,0.,7.8,
     +     0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.9,0.,9.0,7.6,23.6,0.,49.7,0.,9.2,0.,0.,0.,0.,50.69,0.,
     +     49.31,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.35,0.,2.25,0.,11.6,
     +     11.5,57.0,0.,17.3,0.,0.,0.,0.,72.165,0.,27.835,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.56,0.,9.86,7.0,82.58,0.,0.,0.,0.,0.,0.,
     +     0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,51.45,11.22,
     +     17.15,0.,17.38,0.,2.8,0.,0.,0.,0.,0.,0./
      DATA ABN3/100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,14.84,0.,9.25,
     +     15.92,16.68,9.55,24.13,0.,9.63,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,5.52,0.,1.88,12.7,12.6,17.0,31.6,0.,
     +     18.7,0.,0.,0.,0.,100.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     1.02,0.,11.14,22.33,27.33,0.,26.46,0.,11.72,0.,0.,0.,0.,
     +     51.839,0.,48.161,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,1.25,0.,0.89,
     +     0.,12.49,12.80,24.13,12.22,28.73,0.,7.49,0.,0.,4.3,0.,95.7,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.97,0.,0.65,0.36,14.53,7.68,
     +     24.22,8.58,32.59,0.,4.63,0.,5.79,57.3,0.,42.7,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.096,0.,2.6,0.908,4.816,7.14,18.95,0.,31.69,
     +     0.,33.80,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.10,0.,0.09,0.,1.91,26.4,4.1,21.2,26.9,0.,10.4,0.,8.9,100.0,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.106,0.,0.101,0.,2.417,
     +     6.592,7.854,11.23,71.7,0.,0.,0.,0.,0.09,99.91,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.19,0.,0.25,0.,88.48,0.,11.08,0.,0.,0.,0.,
     +     0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,27.13,12.18,
     +     23.80,8.3,17.19,0.,5.76,0.,5.64,0.,0.,0.,0./
      DATA ABN4/0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,3.1,0.,0.,15.0,
     +     11.3,13.8,7.4,0.,26.7,0.,22.7,0.,0.,47.8,0.,52.2,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.2,0.,2.18,14.8,20.47,15.65,24.84,0.,
     +     21.86,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.06,0.,0.1,0.,2.34,18.9,25.5,24.9,28.2,0.,0.,0.,0.,100.0,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.14,0.,1.61,0.,33.6,22.95,
     +     26.8,0.,14.9,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.13,0.,3.05,14.3,21.9,16.12,31.8,0.,12.7,0.,0.,0.,0.,
     +     97.41,2.59,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.162,0.,5.206,
     +     18.606,27.297,13.629,35.1,0.,0.,0.,0.,0.,0.,0.012,99.988,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.13,0.,26.3,14.3,30.67,0.,
     +     28.6,0.,0.,0.,0.,0.,0.,37.4,0.,62.6,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.02,0.,1.58,1.6,13.3,16.1,26.4,0.,41.0,0.,0.,0.,0.,
     +     37.3,0.,62.7,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.01,0.,0.79,0.,
     +     32.9,33.8,25.3,0.,7.2,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.14,0.,10.02,16.84,23.13,13.22,29.80,0.,6.85,
     +     0.,0.,0.,0./
      DATA ABN5/29.524,0.,70.476,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,1.4,0.,
     +     24.1,22.1,52.4,0.,0.,0.,0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,100.0,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     +     0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.0055,0.720,0.,0.,
     +     99.2745,0.,0.,0.,0.,0.,0.,0.,0./

      GMS = WMASS * 1000.
      KNUM = 0
      DO 40 J1 = 1,83
         IF (WELEMP(J1).LE.1E-10) GOTO 40
         IF (AWT(J1).GT.0.) THEN
            XNUM(J1) = GMS*WELEMP(J1)*6.02204459E21/AWT(J1)
            DO 30 J2 = 1,NUMB(J1,1)
               KNUM = KNUM + 1
               IA = NUMB(J1,2) + J2 - 1
               ZA = 1000.*REAL(J1) + REAL(IA)
               CALL CONV(ZA,MAT)
C 1 line added so that Ta180m is long lived isotope 17/11/93
               IF ((1000*J1 + IA).EQ.73180) MAT = MAT + 1
               B(MAT) = DBLE(XNUM(J1)*ABUN(J2,J1)/100.)
               FUWT(KNUM) = REAL(B(MAT))
               IDS(KNUM) = NAMREP(J1)
               IATMWT(KNUM) = IA
 30         CONTINUE
         END IF
 40   CONTINUE
      ISEX2 = KNUM

      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE OUTPUT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.000
C
C--SCCS information
C  Module         : %W%
C  Module name    : %M%
C  Version no.    : %I%
C  Creation date  : %E%
C  Creation time  : %U%
C  SCCS file      :
C  %P%
C
C--Description
C  Prepares the data for the output common for PROCESS.
C  Note that only a single time interval is allowed.
C
C--Author
C  Robin Forrest D3/176 Culham Laboratory, ext.3586
C  Peter Knight  D3/G12 Culham Laboratory, ext.3330
C
C--Date
C  01 December 1993
C
C--Reference
C  None
C  
C--History
C  01/12/93 PJK 1.000 Initial version
C
C--Arguments
C  None
C
C--Global variables passed via COMMON
C  See FISPRO routine for details
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Global variables
      REAL A(18000)
      COMMON/BLOCK /A

      DOUBLE PRECISION B(364)
      COMMON /DBLOCK/ B

      REAL XA(19),XMU(19)
      COMMON /DOSOUT/ XA,XMU

      INTEGER WTYPE,WREGN
      REAL WELEMP(83),WMASS,WTIME,WFLUX,WSPECT(100)
      COMMON /PROINP/ WELEMP,WMASS,WTIME,WFLUX,WSPECT,WTYPE,WREGN

      INTEGER WERRNM
      REAL WINVT(359),WTOTA,WTOTD,WTOTH
      COMMON /PROOUT/ WINVT,WTOTA,WTOTD,WTOTH,WERRNM

      INTEGER WLVL2,WJSPEK,WMAXT
      REAL WLVL1,WCONV
      COMMON /PRPRM0/ WLVL1,WLVL2,WJSPEK,WCONV,WMAXT

      DOUBLE PRECISION WMIND
      COMMON /PRPRM1/ WMIND

C  Local variables
      INTEGER I,ID,INDX20,INDX9,INDX99,MASS,MCROSS,MFRAC,MIDEN,MLAMDA,
     +     MSPEC,MTOTAL,MTRANS,MYIELD,MYILDS,N,NCH,NCROSS,NISOT,NSPEC,
     +     NSTART,NTRANS,NYIELD
      REAL ACT,XDOSE

C  External routines
      EXTERNAL IDNTFY

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  Get value of N by calling IDNTFY.
      NSTART = 0
      CALL IDNTFY(NSTART, NCH, NSPEC, NISOT, MTOTAL, MFRAC, N,
     +     MASS, MIDEN, MLAMDA, MSPEC, MYILDS, MYIELD, NTRANS, MTRANS,
     +     NCROSS, MCROSS, NYIELD)
C Set up the pointer to the A() array.
      INDX9 = MLAMDA
C Initialise the total quantities
      WTOTA = 0.0
      WTOTD = 0.0
      DO 10 I=1,N-5
         WINVT(I) = REAL(B(I))
 10   CONTINUE
C Note that it is necessary to add the gas nuclides at the end of the
C inventory on at the start.
      DO 20 I=1,5
         WINVT(I) = WINVT(I) + REAL(B(N-5+I))
 20   CONTINUE
C If WINVT() is < WMIND then set to 0.0
      DO 30 I=1,N-5
         IF (WINVT(I).LT.WMIND) WINVT(I) = 0.0
         INDX9 = INDX9 + 1
         ACT = WINVT(I)*A(INDX9)
C Add contribution to total activity
         WTOTA = WTOTA + ACT
         IF (ACT.GT.0.0) THEN
            INDX99 = MSPEC + (I-1)*NSPEC + 9
            INDX20 = INDX99 - 5
C Add contribution to total heat output
            WTOTH = WTOTH + ACT * (A(INDX20-1)+A(INDX20)+A(INDX20+1))
            XDOSE = 0.0
            DO 25 ID = 1,19
               XDOSE = XDOSE + A(INDX99+ID)*XA(ID)/XMU(ID)
 25         CONTINUE
C Add contribution to total gamma dose rate
            WTOTD = WTOTD + XDOSE*ACT*5.76E-10/WMASS
         END IF
 30   CONTINUE
C Convert from Bq-MeV to kW
      WTOTH = WTOTH * 1.6021773E-16
      RETURN
      END
