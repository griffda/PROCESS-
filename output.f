C----------------------------------------------------------------------
C--SCCS information
C  Module         : $Id$
C  Module name    : $RCSfile: output.f,v $
C  Version no.    : $Revision: 3.15 $
C  Creation date  : $Date: 1999/05/19 09:29:43 $
C  Creation time  : 
C  SCCS file      :
C  %P%
C
C----------------------------------------------------------------------
      SUBROUTINE OUTPUT(NOUT)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--Version number 1.300
C
C--Description
C  Routine to write the results to the main output file.
C
C--Author
C  Peter Knight D3/162a Culham Laboratory, ext.4181
C
C--Date
C  19 May 1999
C
C--Reference
C  None
C  
C--History
C  23/01/97 PJK 1.000 Initial upgraded version. Split routine POWER
C                     into POWER1 and POWER2
C  06/02/97 PJK 1.100 Added routine LOCA
C  21/03/97 PJK 1.200 Added routine IFEOUT
C  18/11/97 PJK 1.210 Removed NOUT argument from FISPAC call
C  19/05/99 PJK 1.300 Added routine AVAIL
C
C--Arguments
C  NOUT   : (INPUT)  Fortran output unit specifier
C
C--Global variables passed via COMMON
C  None
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C  INCLUDE files
      INCLUDE 'param.h'
      INCLUDE 'phydat.h'
      INCLUDE 'stella.h'
      INCLUDE 'rfp.h'
      INCLUDE 'ife.h'

C  Arguments
      INTEGER NOUT

C  External routines
      EXTERNAL 
     +     ACPOW,AVAIL,BLDGCALL,CNTRPST,COSTS,CUDRIV,DIVCALL,ECH,
     +     FISPAC,FWBS,IFEOUT,IGMARCAL,INDUCT,LOCA,LWHYMOD,NBEAM,OUTPF,
     +     OUTPLAS,OUTTIM,OUTVOLT,PFPWR,POWER2,PULSE,RADIALB,RFPPFC,
     +     RFPPFP,RFPTFC,STOUT,STRUCALL,TFCOIL,TFPWR,TFSPCALL,VACCALL

C--End of preamble--CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C *** Call stellarator output routine if relevant

      IF (ISTELL.NE.0) THEN
         CALL STOUT(NOUT)
         GOTO 1000
      END IF

C *** Call inertial fusion energy output routine if relevant

      IF (IFE.NE.0) THEN
         CALL IFEOUT(NOUT)
         GOTO 1000
      END IF

      CALL COSTS(NOUT,1)

C+**PJK 10/05/99
      CALL AVAIL(NOUT,1)

      CALL OUTPLAS(NOUT)

      CALL IGMARCAL(NOUT)

      CALL CUDRIV(NOUT,1)

C+**PJK 12/11/93
      CALL PULSE(NOUT,1)

C+**PJK 23/05/94
      CALL OUTTIM(NOUT)

      CALL DIVCALL(1,NOUT)

      CALL RADIALB(1,NOUT)

C+**PJK 06/03/96
      IF (IRFP.EQ.0) THEN
         CALL TFCOIL(NOUT,1)
      ELSE
         CALL RFPTFC(NOUT,1)
      END IF

      CALL TFSPCALL(NOUT,1)

C+**PJK 29/01/96
      IF (ITART.EQ.1) CALL CNTRPST(NOUT,1)

C+**PJK 06/03/96
      IF (IRFP.EQ.0) THEN
         CALL OUTPF(NOUT)
      ELSE
         CALL RFPPFC(NOUT,1)
      END IF

C+**PJK 06/03/96
      IF (IRFP.EQ.0) CALL OUTVOLT(NOUT)

      CALL STRUCALL(NOUT,1)

C+**PJK 06/03/96
      IF (IRFP.EQ.0) CALL INDUCT(1,NOUT)

      CALL FWBS(NOUT,1)

      IF (IFISPACT.EQ.1) THEN
         CALL FISPAC(0)
         CALL FISPAC(1)
C+**PJK 06/02/97
         CALL LOCA(NOUT,0)
         CALL LOCA(NOUT,1)
      END IF

      CALL TFPWR(NOUT,1)

C+**PJK 06/03/96
      IF (IRFP.EQ.0) THEN
         CALL PFPWR(NOUT,1)
      ELSE
         CALL RFPPFP(NOUT,1)
      END IF

C+**PJK 23/01/97      CALL POWER(NOUT,1)

      CALL VACCALL(NOUT,1)

      CALL BLDGCALL(NOUT,1)

      CALL ACPOW(NOUT,1)

C+**PJK 23/01/97
      CALL POWER2(NOUT,1)

      CALL NBEAM(NOUT,1)

      CALL ECH(NOUT,1)

      CALL LWHYMOD(NOUT,1)

 1000 CONTINUE

      RETURN
      END
c______________________________________________________________________
      SUBROUTINE OCENTR(nout,string,width)

C  Routine to print a centred header within a line of asterisks.
C  It cannot cope with a zero-length string (use OSTARS instead).
C
C  nout    : (INPUT)  Fortran output unit identifier
C  string  : (INPUT)  Character string to be used

      INTEGER nout,lh,width,maxwd,nstars,nstars2
      CHARACTER*(*) string
      CHARACTER stars*50

C  Maximum width of the line to be printed
      PARAMETER(maxwd = 100)

C  Note that the number of stars must be greater than or equal to
C  half of MAXWD.
      stars = '**************************************************'

C  Length of string
      lh = len(string)

      if (width.gt.maxwd) then
         write(*,*) 'Error in routine OCENTR :'
         write(*,*) 'Maximum width = ',maxwd
         write(*,*) 'Requested width = ',width
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

      if (lh.eq.0) then
         write(*,*) 'Error in routine OCENTR :'
         write(*,*) 'A zero-length string is not permitted.'
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

      if (lh.ge.width) then
         write(*,*) 'Error in routine OCENTR :'
         write(*,*) string
         write(*,*) 'This is too long to fit into ',width,' columns.'
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C  Number of stars to be printed on the left
      nstars = int( (width-lh)/2 ) - 1

C  Number of stars to be printed on the right
      nstars2 = width - (nstars+lh+2)

C  Write the whole line
      write(nout,*) stars(1:nstars),' ',string,' ',stars(1:nstars2)

      return
      end
c______________________________________________________________________
      SUBROUTINE OSTARS(nout,width)

C  Routine to print a line of asterisks.
C
C  nout    : (INPUT)  Fortran output unit identifier
C  width   : (INPUT)  Number of asterisks in the line

      INTEGER nout,width,maxwd
      CHARACTER stars*100

C  Maximum number of stars on the line
      PARAMETER(maxwd = 100)

C  Note that the number of stars must be greater than or equal to
C  half of MAXWD.
      stars = '**************************************************'//
     +        '**************************************************'

      if (width.gt.maxwd) then
         write(*,*) 'Error in routine OSTARS :'
         write(*,*) 'Maximum width = ',maxwd
         write(*,*) 'Requested width = ',width
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C  Write the line of stars
      write(nout,10) stars(1:width)
 10   format(t2,a)

      return
      end
c______________________________________________________________________
      SUBROUTINE OHEADR(nout,string)

C  Routine to print a centred header within a line of asterisks,
C  and between two blank lines.
C
C  nout    : (INPUT)  Fortran output unit identifier
C  string  : (INPUT)  Character string to be used

      INTEGER nout,width
      CHARACTER*(*) string

      EXTERNAL oblnkl,ocentr

C  Total width of the line to be printed
      PARAMETER(width = 72)

C  Write the whole line, with blank lines on either side
      call oblnkl(nout)
      call ocentr(nout,string,width)
      call oblnkl(nout)

      return
      end
c______________________________________________________________________
      SUBROUTINE OSHEAD(nout,string)

C  Routine to print a short, centred header within a line of asterisks,
C  and between two blank lines.
C
C  nout    : (INPUT)  Fortran output unit identifier
C  string  : (INPUT)  Character string to be used

      INTEGER nout,width
      CHARACTER*(*) string

      EXTERNAL oblnkl,ocentr

C  Total width of the line to be printed
      PARAMETER(width = 50)

      call oblnkl(nout)
      call ocentr(nout,string,width)
      call oblnkl(nout)

      return
      end
c______________________________________________________________________
      SUBROUTINE OBLNKL(nout)

C  Routine to print a blank line.
C
C  nout    : (INPUT)  Fortran output unit identifier

      INTEGER nout

      write(nout,10)
 10   format(' ')

      return
      end
c______________________________________________________________________
      SUBROUTINE OSUBHD(nout,string)

C  Routine to print a subheading between two blank lines.
C
C  nout    : (INPUT)  Fortran output unit identifier
C  string  : (INPUT)  Character string to be used

      INTEGER nout
      CHARACTER*(*) string

      EXTERNAL oblnkl,ocmmnt

C  Write the whole line, including blank lines on either side
      call oblnkl(nout)
      call ocmmnt(nout,string)
      call oblnkl(nout)

      return
      end
c______________________________________________________________________
      SUBROUTINE OCMMNT(nout,string)

C  Routine to print a comment.
C
C  nout    : (INPUT)  Fortran output unit identifier
C  string  : (INPUT)  Character string to be used

      INTEGER nout,lh,width
      CHARACTER*(*) string

C  Maximum width of the line to be printed
      PARAMETER(width = 72)

C  Length of string
      lh = len(string)

      if (lh.eq.0) then
         write(*,*) 'Error in routine OCMMNT :'
         write(*,*) 'A zero-length string is not permitted.'
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

      if (lh.ge.width) then
         write(*,*) 'Error in routine OCMMNT :'
         write(*,*) string
         write(*,*) 'This is too long to fit into ',width,' columns.'
         write(*,*) 'PROCESS stopping.'
         STOP
      end if

C  Write the whole line
      write(nout,10) string
 10   format(t2,a)

      return
      end
c______________________________________________________________________
      SUBROUTINE OVARRF(nout,descr,varnam,value)

C  Routine to print out a description, the name and value of a
C  double precision variable in F format (e.g. -12345.000)
C
C  nout    : (INPUT)  Fortran output unit identifier
C  descr   : (INPUT)  Description of the variable
C  varnam  : (INPUT)  Name of the variable
C  value   : (INPUT)  Value of the variable

      CHARACTER dum42*42,dum13*13
      CHARACTER*(*) descr,varnam
      INTEGER nout
      DOUBLE PRECISION value

C  Replace descr and varnam with dummy strings of the correct length.
C  This counters problems that would occur if the two original strings
C  were the wrong length.
      dum42 = descr
      dum13 = varnam

      write(nout,10) dum42,dum13,value
 10   format(1x,a,t45,a,t60,f10.3)

      return
      end
c______________________________________________________________________
      SUBROUTINE OVARRE(nout,descr,varnam,value)

C  Routine to print out a description, the name and value of a
C  double precision variable in E format (e.g. -1.234E+04)
C
C  nout    : (INPUT)  Fortran output unit identifier
C  descr   : (INPUT)  Description of the variable
C  varnam  : (INPUT)  Name of the variable
C  value   : (INPUT)  Value of the variable

      CHARACTER dum42*42,dum13*13
      CHARACTER*(*) descr,varnam
      INTEGER nout
      DOUBLE PRECISION value

C  Replace descr and varnam with dummy strings of the correct length.
C  This counters problems that would occur if the two original strings
C  were the wrong length.
      dum42 = descr
      dum13 = varnam

      write(nout,10) dum42,dum13,value
 10   format(1x,a,t45,a,t60,1pe10.3)

      return
      end
c______________________________________________________________________
      SUBROUTINE OVARIN(nout,descr,varnam,value)

C  Routine to print out a description, the name and value of an
C  integer variable.
C
C  nout    : (INPUT)  Fortran output unit identifier
C  descr   : (INPUT)  Description of the variable
C  varnam  : (INPUT)  Name of the variable
C  value   : (INPUT)  Value of the variable

      CHARACTER dum42*42,dum13*13
      CHARACTER*(*) descr,varnam
      INTEGER nout
      INTEGER value

C  Replace descr and varnam with dummy strings of the correct length.
C  This counters problems that would occur if the two original strings
C  were the wrong length.
      dum42 = descr
      dum13 = varnam

      write(nout,10) dum42,dum13,value
 10   format(1x,a,t45,a,t60,i10)

      return
      end
c______________________________________________________________________
      SUBROUTINE OCOSTS(nout,ccode,descr,value)

C  Routine to print out the code, description and value of a cost item
C
C  nout    : (INPUT)  Fortran output unit identifier
C  ccode   : (INPUT)  Code number/name of the cost item
C  descr   : (INPUT)  Description of the cost item
C  value   : (INPUT)  Value of the cost item

      CHARACTER dum8*8,dum42*42
      CHARACTER*(*) ccode,descr
      INTEGER nout
      DOUBLE PRECISION value

C  Replace ccode and descr with dummy strings of the correct length.
C  This counters problems that would occur if the two original strings
C  were the wrong length.
      dum8 = ccode
      dum42 = descr

      write(nout,10) dum8,dum42,value
 10   format(1x,a,t10,a,t60,f10.2)

      return
      end
c______________________________________________________________________
      SUBROUTINE OBUILD(nout,descr,thick,total)

C  Routine to print out a description, the thickness and summed build
C  of a component of the radial or vertical build.
C
C  nout    : (INPUT)  Fortran output unit identifier
C  descr   : (INPUT)  Description of the component
C  thick   : (INPUT)  Thickness of the component (m)
C  total   : (INPUT)  Total build, including this component (m)

      CHARACTER dum30*30
      CHARACTER*(*) descr
      INTEGER nout
      DOUBLE PRECISION thick,total

C  Replace descr with dummy string of the correct length.
C  This counters problems that would occur if the original string
C  was the wrong length.
      dum30 = descr

      write(nout,10) dum30,thick,total
 10   format(1x,a,t42,f10.3,t58,f10.3)

      return
      end
c______________________________________________________________________
      SUBROUTINE OUTTIM(nout)

C  Routine to print out the times of the various stages
C  during a single plant cycle
C
C  nout    : (INPUT)  Fortran output unit identifier

      INCLUDE 'times.h'
      INCLUDE 'osections.h'

      DOUBLE PRECISION tcycle
      INTEGER nout

      EXTERNAL oblnkl,oheadr,ovarrf

      if (sect21.eq.0) goto 1000

      tcycle = tramp + tohs + theat + tburn + tqnch + tdwell

      call oheadr(nout,'Times')

      call ovarrf(nout,'Initial charge time for PF coils (s)','(tramp)',
     +     tramp)
      call ovarrf(nout,'OH coil swing time (s)','(tohs)',tohs)
      call ovarrf(nout,'Heating time (s)','(theat)',theat)
      call ovarre(nout,'Burn time (s)','(tburn)',tburn)
      call ovarrf(nout,'Shutdown time for PF coils (s)','(tqnch)',tqnch)
      call ovarrf(nout,'Time between pulses (s)','(tdwell)',tdwell)
      call oblnkl(nout)
      call ovarre(nout,'Pulse time (s)','(tpulse)',tpulse)
      call ovarrf(nout,'Down time (s)','(tdown)',tdown)
      call ovarre(nout,'Total plant cycle time (s)','(tcycle)',tcycle)

 1000 continue

      return
      end

